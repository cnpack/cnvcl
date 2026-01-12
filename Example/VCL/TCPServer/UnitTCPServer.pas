unit UnitTCPServer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WinSock, CnThreadingTCPServer, CnNetwork, CnNative, CnTLS, CnSocket,
  CnNetConsts, CnECC, CnChaCha20, CnPoly1305, CnAEAD, CnRandom, CnPemUtils, CnRSA,
  CnSHA2, CnBigNumber;

type
  TFormTCPServer = class(TForm)
    lblIP: TLabel;
    edtIP: TEdit;
    lblPort: TLabel;
    edtPort: TEdit;
    btnOpen: TButton;
    mmoResult: TMemo;
    btnOpenTLS: TButton;
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOpenTLSClick(Sender: TObject);
  private
    FTCP: TCnThreadingTCPServer;
    FTLS: TCnThreadingTCPServer;
    procedure Log(const Msg: string);
    procedure DoSSLLog(Sender: TObject; const Msg: string);
    function DoServerTLSHandshake(ClientSocket: TCnClientSocket): Boolean;
  public
    procedure TCPAccept(Sender: TObject; ClientSocket: TCnClientSocket);
    procedure TLSAccept(Sender: TObject; ClientSocket: TCnClientSocket);
    procedure TCPError(Sender: TObject; SocketError: Integer);
  end;

var
  FormTCPServer: TFormTCPServer;

implementation

{$R *.DFM}

procedure TFormTCPServer.btnOpenClick(Sender: TObject);
begin
  if FTCP.Active then
  begin
    FTCP.Active := False;
    btnOpen.Caption := 'Open';
    btnOpenTLS.Enabled := True;
  end
  else
  begin
    FTCP.LocalIP := edtIP.Text;
    FTCP.LocalPort := StrToInt(edtPort.Text);

    FTCP.Active := True;
    if FTCP.Listening then
    begin
      btnOpen.Caption := 'Close';
      btnOpenTLS.Enabled := False;
      Log('Listening at Port: ' + IntToStr(FTCP.ActualLocalPort));
    end;
  end;
end;

procedure TFormTCPServer.btnOpenTLSClick(Sender: TObject);
begin
  if (FTLS <> nil) and FTLS.Active then
  begin
    FTLS.Active := False;
    btnOpenTLS.Caption := 'Start TLS Server';
    btnOpen.Enabled := True;
  end
  else
  begin
    if FTLS = nil then
      FTLS := TCnThreadingTCPServer.Create(Self);
    FTLS.OnAccept := TLSAccept;
    FTLS.OnError := TCPError;

    FTLS.LocalIP := edtIP.Text;
    FTLS.LocalPort := StrToInt(edtPort.Text);

    FTLS.Active := True;
    if FTLS.Listening then
    begin
      btnOpenTLS.Caption := 'Stop TLS Server';
      btnOpen.Enabled := False;
      Log('TLS Server Listening at Port: ' + IntToStr(FTLS.ActualLocalPort));
    end;
  end;
end;

procedure TFormTCPServer.FormCreate(Sender: TObject);
begin
  FTCP := TCnThreadingTCPServer.Create(Self);
  FTCP.OnAccept := TCPAccept;
  FTCP.OnError := TCPError;
end;

procedure TFormTCPServer.DoSSLLog(Sender: TObject; const Msg: string);
begin
  Log('SSL: ' + Msg);
end;

procedure TFormTCPServer.Log(const Msg: string);
begin
  mmoResult.Lines.Add(Msg);
end;

function TFormTCPServer.DoServerTLSHandshake(ClientSocket: TCnClientSocket): Boolean;
var
  R: TBytes;
  H: TCnTLSRecordLayer;
  Len, BodyLen, C: Integer;
  HS: Byte;
  ClientRandom, ServerRandom: TBytes;
  Msg: TBytes;
  TidStr: string;
  HH: TCnTLSHandShakeHeader;
  Total: TBytes;
  CertDER: TBytes;
  CertFile: string;
  Mem: TMemoryStream;
  VLen, CLen: Integer;
  // SKE
  Ecc: TCnEcc;
  EpPriv: TCnEccPrivateKey;
  EpPub: TCnEccPublicKey;
  CurveBytes: array[0..1] of Byte;
  PtLen: Byte;
  Pt: TBytes;
  KeyFile: string;
  Pri: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
  Sig: TBytes;
  ToSign: TBytes;
  Alg: array[0..1] of Byte;
  SL: Word;
  // handshake log
  HandshakeLog: TBytes;
  // key schedule
  PreMaster: TBytes;
  Master: TBytes;
  KeyBlock: TBytes;
  ClientWriteKey, ServerWriteKey: TBytes;
  ClientFixedIV, ServerFixedIV: TBytes;
  ClientSeq, ServerSeq: TUInt64;
  // tls buffers
  AAD, Plain, En, HSHash, EV, SV, RespB, Tmp, Exts: TBytes;
  Buf: TBytes;
  Tag: TCnGCM128Tag;
  Resp: AnsiString;
  CliPub: TCnEccPublicKey;
  P: TCnEccPoint;
  SelCipherHi, SelCipherLo: Byte;
  Found: Boolean;
  HaveC02F: Boolean;
  Off, SidLen, CSLen, I: Integer;
  CHi, CLo: Byte;
  SuiteStr, sHave, sHave2: string;
  CompLen, ExtsLen, ExtType, ExtLen, PairsLen, J: Integer;
  KeyLen, IvLen: Integer;
  UseChaCha: Boolean;
  function SendExact(const Buf; Len: Integer): Boolean;
  var Sent, Cnt: Integer; P: PAnsiChar;
  begin
    Result := False;
    P := @Buf;
    Sent := 0;
    while Sent < Len do
    begin
      Cnt := ClientSocket.Send(P[Sent], Len - Sent);
      if Cnt = SOCKET_ERROR then
        Exit;
      if Cnt = 0 then
        Exit;
      Inc(Sent, Cnt);
    end;
    Result := True;
  end;
  function RecvExact(Need: Integer; out OutBuf: TBytes): Boolean;
  var Got, Cnt: Integer;
  begin
    Result := False;
    SetLength(OutBuf, Need);
    Got := 0;
    while Got < Need do
    begin
      Cnt := ClientSocket.Recv(OutBuf[Got], Need - Got);
      if Cnt = SOCKET_ERROR then
        Exit;
      if Cnt = 0 then
        Exit;
      Inc(Got, Cnt);
    end;
    Result := True;
  end;
  function AlertName(Desc: Byte): string;
  begin
    case Desc of
      0: Result := 'close_notify';
      10: Result := 'unexpected_message';
      20: Result := 'bad_record_mac';
      21: Result := 'decryption_failed';
      22: Result := 'record_overflow';
      40: Result := 'handshake_failure';
      41: Result := 'no_certificate';
      42: Result := 'bad_certificate';
      43: Result := 'unsupported_certificate';
      44: Result := 'certificate_revoked';
      45: Result := 'certificate_expired';
      46: Result := 'certificate_unknown';
      47: Result := 'illegal_parameter';
      48: Result := 'unknown_ca';
      49: Result := 'access_denied';
      50: Result := 'decode_error';
      51: Result := 'decrypt_error';
      70: Result := 'protocol_version';
      71: Result := 'insufficient_security';
      80: Result := 'internal_error';
      90: Result := 'user_canceled';
      100: Result := 'no_renegotiation';
    else
      Result := 'unknown';
    end;
  end;
  // helpers
  function MakeLabel(const S: AnsiString): TBytes;
  var B: TBytes;
  begin
    SetLength(B, Length(S));
    if Length(S) > 0 then
      Move(PAnsiChar(S)^, B[0], Length(S));
    Result := B;
  end;
  function BuildAAD(CT: Byte; Seq: TUInt64; PlainLen: Integer): TBytes;
  var A: TBytes; V: TBytes; L: Word;
  begin
    V := TLSUInt64ToBE8(Seq);
    SetLength(A, 13);
    Move(V[0], A[0], 8);
    A[8] := CT;
    A[9] := 3;
    A[10] := 3;
    L := htons(PlainLen);
    Move(L, A[11], 2);
    Result := A;
  end;
  procedure AppendHS(HSType: Byte; const Body: TBytes);
  var Hdr: array[0..3] of Byte; L3: Integer;
  begin
    Hdr[0] := HSType;
    L3 := Length(Body);
    Hdr[1] := (L3 shr 16) and $FF;
    PWord(@Hdr[2])^ := htons(L3 and $FFFF);
    HandshakeLog := ConcatBytes(HandshakeLog, NewBytesFromMemory(@Hdr[0], 4));
    if L3 > 0 then
      HandshakeLog := ConcatBytes(HandshakeLog, Body);
  end;
begin
  Result := False;
  TidStr := 'TID=' + IntToStr(GetCurrentThreadId);
  DoSSLLog(Self, TidStr + ' Start TLS Handshake');
  Len := ClientSocket.Recv(H, 5);
  if Len <> 5 then
  begin
    DoSSLLog(Self, TidStr + ' Invalid Record Header Len=' + IntToStr(Len));
    Exit;
  end;
  DoSSLLog(Self, TidStr + ' Recv Record CT=' + IntToStr(H.ContentType) +
    ' Ver=' + IntToStr(H.MajorVersion) + '.' + IntToStr(H.MinorVersion));
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    SetLength(Buf, BodyLen);
    Len := ClientSocket.Recv(Buf[0], BodyLen);
    if (Len = SOCKET_ERROR) or (Len <> BodyLen) or (BodyLen < 2) then
      Exit;
    DoSSLLog(Self, TidStr + ' Alert Level=' + IntToStr(Buf[0]) + ' Desc=' + IntToStr(Buf[1]) + ' (' + AlertName(Buf[1]) + ')');
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_HANDSHAKE then
    Exit;
  BodyLen := ntohs(H.BodyLength);
  if (BodyLen <= 0) or (BodyLen > 16384) then
  begin
    DoSSLLog(Self, TidStr + ' Invalid BodyLen=' + IntToStr(BodyLen));
    Exit;
  end;
  SetLength(Buf, BodyLen);
  Len := ClientSocket.Recv(Buf[0], BodyLen);
  if Len = SOCKET_ERROR then
  begin
    DoSSLLog(Self, TidStr + ' Recv Body Socket Error');
    Exit;
  end;
  if Len <> BodyLen then
  begin
    while (Len <> SOCKET_ERROR) and (Len < BodyLen) do
    begin
      C := ClientSocket.Recv(Buf[Len], BodyLen - Len);
      if C = SOCKET_ERROR then
        Break;
      Inc(Len, C);
    end;
    if Len <> BodyLen then
      Exit;
  end;
  HS := Buf[0];
  if HS <> CN_TLS_HANDSHAKE_TYPE_CLIENT_HELLO then
  begin
    DoSSLLog(Self, TidStr + ' First HS Not ClientHello Type=' + IntToStr(HS));
    Exit;
  end;
  DoSSLLog(Self, TidStr + ' ClientHello Received BodyLen=' + IntToStr(BodyLen));
  SetLength(ClientRandom, 32);
  Move(Buf[6], ClientRandom[0], 32);
  SelCipherHi := $CC; SelCipherLo := $A8; Found := False; HaveC02F := False;
  Off := 6 + 32;
  if BodyLen > Off then
  begin
    SidLen := Buf[Off]; Inc(Off);
    Inc(Off, SidLen);
    if BodyLen >= Off + 2 then
    begin
      CSLen := (Integer(Buf[Off]) shl 8) or Integer(Buf[Off + 1]);
      Inc(Off, 2);
      I := 0;
      SuiteStr := '';
      while (I + 1 < CSLen) and (Off + I + 1 < BodyLen) do
      begin
        CHi := Buf[Off + I];
        CLo := Buf[Off + I + 1];
        if SuiteStr <> '' then
          SuiteStr := SuiteStr + ',';
        SuiteStr := SuiteStr + '0x' + IntToHex((Integer(CHi) shl 8) or Integer(CLo), 4);
        if (CHi = $CC) and (CLo = $A8) then
        begin
          SelCipherHi := CHi; SelCipherLo := CLo; Found := True; Break;
        end;
        if (CHi = $C0) and (CLo = $2F) then
          HaveC02F := True;
        Inc(I, 2);
      end;
      sHave := 'No';
      if Found then sHave := 'Yes';
      sHave2 := 'No';
      if HaveC02F then sHave2 := 'Yes';
      DoSSLLog(Self, TidStr + ' Client Suites: ' + SuiteStr);
      DoSSLLog(Self, TidStr + ' Have 0xCCA8: ' + sHave);
      DoSSLLog(Self, TidStr + ' Have 0xC02F: ' + sHave2);
      if Off + CSLen < BodyLen then
      begin
        Inc(Off, CSLen);
        if Off < BodyLen then
        begin
          CompLen := Buf[Off];
          Inc(Off);
          Inc(Off, CompLen);
          if BodyLen >= Off + 2 then
          begin
            ExtsLen := (Integer(Buf[Off]) shl 8) or Integer(Buf[Off + 1]);
            Inc(Off, 2);
            I := 0;
            while (I + 4 <= ExtsLen) and (Off + I + 4 <= BodyLen) do
            begin
              ExtType := (Integer(Buf[Off + I]) shl 8) or Integer(Buf[Off + I + 1]);
              ExtLen := (Integer(Buf[Off + I + 2]) shl 8) or Integer(Buf[Off + I + 3]);
              Inc(I, 4);
              if (ExtType = $000D) and (ExtLen >= 2) and (I + ExtLen <= ExtsLen) then
              begin
                PairsLen := (Integer(Buf[Off + I]) shl 8) or Integer(Buf[Off + I + 1]);
                Inc(J, 0);
                SuiteStr := '';
                if PairsLen > 0 then
                begin
                  J := 2;
                  while (J + 1 < ExtLen) and ((J - 2) < PairsLen) do
                  begin
                    CHi := Buf[Off + I + J];
                    CLo := Buf[Off + I + J + 1];
                    if SuiteStr <> '' then
                      SuiteStr := SuiteStr + ',';
                    SuiteStr := SuiteStr + '0x' + IntToHex((Integer(CHi) shl 8) or Integer(CLo), 4);
                    Inc(J, 2);
                  end;
                end;
                sHave := 'No';
                J := 2;
                while (J + 1 < ExtLen) and ((J - 2) < PairsLen) do
                begin
                  if (Buf[Off + I + J] = 4) and (Buf[Off + I + J + 1] = 1) then
                  begin
                    sHave := 'Yes';
                    Break;
                  end;
                  Inc(J, 2);
                end;
                DoSSLLog(Self, TidStr + ' Client SigAlgs: ' + SuiteStr);
                DoSSLLog(Self, TidStr + ' Have 0x0401 (rsa_pkcs1_sha256): ' + sHave);
              end;
              Inc(I, ExtLen);
            end;
          end;
        end;
      end;
    end;
  end;
  if not Found then
  begin
    if HaveC02F then
    begin
      SelCipherHi := $C0; SelCipherLo := $2F;
    end
    else
    begin
      DoSSLLog(Self, TidStr + ' No Supported CipherSuite Found, send alert handshake_failure');
      H.ContentType := CN_TLS_CONTENT_TYPE_ALERT;
      H.MajorVersion := 3;
      H.MinorVersion := 3;
      H.BodyLength := htons(2);
      if not SendExact(H, 5) then Exit;
      Msg := nil; SetLength(Msg, 2); Msg[0] := CN_TLS_ALERT_LEVEL_FATAL; Msg[1] := CN_TLS_ALERT_DESC_HANDSHAKE_FAILURE;
      if not SendExact(Msg[0], 2) then Exit;
      Exit;
    end;
  end;
  UseChaCha := (SelCipherHi = $CC) and (SelCipherLo = $A8);
  if UseChaCha then
  begin
    KeyLen := 32;
    IvLen := 12;
  end
  else
  begin
    KeyLen := 16;
    IvLen := 4;
  end;
  SetLength(Tmp, BodyLen - 4);
  if BodyLen > 4 then
    Move(Buf[4], Tmp[0], BodyLen - 4);
  AppendHS(HS, Tmp);
  DoSSLLog(Self, TidStr + ' ClientRandom Parsed Len=' + IntToStr(Length(ClientRandom)));
  SetLength(ServerRandom, 32);
  CnRandomFillBytes(@ServerRandom[0], 32);
  DoSSLLog(Self, TidStr + ' ServerRandom Generated Len=' + IntToStr(Length(ServerRandom)));
  Exts := nil;
  SetLength(Tmp, 4);
  PWord(@Tmp[0])^ := htons($000B);
  PWord(@Tmp[2])^ := htons(2);
  Exts := ConcatBytes(Exts, Tmp);
  SetLength(Tmp, 2);
  Tmp[0] := 1;
  Tmp[1] := 0;
  Exts := ConcatBytes(Exts, Tmp);
  SetLength(Tmp, 4);
  PWord(@Tmp[0])^ := htons($FF01);
  PWord(@Tmp[2])^ := htons(1);
  Exts := ConcatBytes(Exts, Tmp);
  SetLength(Tmp, 1);
  Tmp[0] := 0;
  Exts := ConcatBytes(Exts, Tmp);
  SetLength(Msg, 2 + 32 + 1 + 2 + 1 + 2 + Length(Exts));
  Msg[0] := 3;
  Msg[1] := 3;
  Move(ServerRandom[0], Msg[2], 32);
  Msg[34] := 0;
  Msg[35] := SelCipherHi;
  Msg[36] := SelCipherLo;
  Msg[37] := 0;
  PWord(@Msg[38])^ := htons(Length(Exts));
  if Length(Exts) > 0 then
    Move(Exts[0], Msg[40], Length(Exts));
  HH.HandShakeType := CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO;
  HH.LengthHi := (Length(Msg) shr 16) and $FF;
  HH.LengthLo := htons(Length(Msg) and $FFFF);
  SetLength(Total, 4 + Length(Msg));
  Move(HH, Total[0], 4);
  Move(Msg[0], Total[4], Length(Msg));
  H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
  H.MajorVersion := 3;
  H.MinorVersion := 3;
  H.BodyLength := htons(Length(Total));
  DoSSLLog(Self, TidStr + ' Send ServerHello Len=' + IntToStr(Length(Total)));
  if not SendExact(H, 5) then Exit;
  if (Length(Total) > 0) and (not SendExact(Total[0], Length(Total))) then Exit;
  DoSSLLog(Self, TidStr + ' ServerHello Sent');
  AppendHS(CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO, Copy(Msg, 0, Length(Msg)));
  
  CertFile := ExtractFilePath(ParamStr(0)) + 'server_cert.crt';
  Mem := TMemoryStream.Create;
  try
    if LoadPemFileToMemory(CertFile, '-----BEGIN CERTIFICATE-----', '-----END CERTIFICATE-----', Mem) then
    begin
      SetLength(CertDER, Mem.Size);
      if Mem.Size > 0 then
      begin
        Mem.Position := 0;
        Mem.Read(CertDER[0], Mem.Size);
      end;
      CLen := Length(CertDER);
      VLen := 3 + CLen;
      SetLength(Msg, 3 + 3 + CLen);
      Msg[0] := (VLen shr 16) and $FF;
      PWord(@Msg[1])^ := htons(VLen and $FFFF);
      Msg[3] := (CLen shr 16) and $FF;
      PWord(@Msg[4])^ := htons(CLen and $FFFF);
      if CLen > 0 then
        Move(CertDER[0], Msg[6], CLen);
      HH.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CERTIFICATE;
      HH.LengthHi := (Length(Msg) shr 16) and $FF;
      HH.LengthLo := htons(Length(Msg) and $FFFF);
      SetLength(Total, 4 + Length(Msg));
      Move(HH, Total[0], 4);
      Move(Msg[0], Total[4], Length(Msg));
      H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
      H.MajorVersion := 3;
      H.MinorVersion := 3;
      H.BodyLength := htons(Length(Total));
      DoSSLLog(Self, TidStr + ' Send Certificate Len=' + IntToStr(Length(Total)));
      if not SendExact(H, 5) then Exit;
      if not SendExact(Total[0], Length(Total)) then Exit;
      DoSSLLog(Self, TidStr + ' Certificate Sent');
      AppendHS(CN_TLS_HANDSHAKE_TYPE_CERTIFICATE, Copy(Msg, 0, Length(Msg)));
    end
    else
    begin
      DoSSLLog(Self, TidStr + ' Load Certificate Failed: ' + CertFile);
      Exit;
    end;
  finally
    Mem.Free;
  end;
  
  // Send ServerKeyExchange (ECDHE_RSA with secp256r1)
  Ecc := TCnEcc.Create(ctSecp256r1);
  EpPriv := TCnEccPrivateKey.Create;
  EpPub := TCnEccPublicKey.Create;
  try
    Ecc.GenerateKeys(EpPriv, EpPub);
    CurveBytes[0] := 0;
    CurveBytes[1] := $17; // secp256r1
    PtLen := Byte(1 + 2 * Ecc.BytesCount);
    SetLength(Pt, PtLen);
    Pt[0] := $04;
    EpPub.X.ToBinary(@Pt[1], Ecc.BytesCount);
    EpPub.Y.ToBinary(@Pt[1 + Ecc.BytesCount], Ecc.BytesCount);
    SetLength(Msg, 1 + 2 + 1 + PtLen);
    Msg[0] := 3;
    Msg[1] := CurveBytes[0];
    Msg[2] := CurveBytes[1];
    Msg[3] := PtLen;
    if PtLen > 0 then
      Move(Pt[0], Msg[4], PtLen);
    ToSign := nil;
    ToSign := ConcatBytes(ToSign, ClientRandom);
    ToSign := ConcatBytes(ToSign, ServerRandom);
    ToSign := ConcatBytes(ToSign, Msg);
    KeyFile := ExtractFilePath(ParamStr(0)) + 'server_key.pem';
    Pri := TCnRSAPrivateKey.Create(True);
    Pub := TCnRSAPublicKey.Create;
    try
      if CnRSALoadKeysFromPem(KeyFile, Pri, Pub, ckhMd5, '') then
      begin
        Sig := CnRSASignBytes(ToSign, Pri, rsdtSHA256);
        Alg[0] := 4; // SHA256
        Alg[1] := 1; // RSA
        SL := Length(Sig);
        SetLength(Msg, Length(Msg) + 2 + 2 + SL);
        Msg[4 + PtLen] := Alg[0];
        Msg[5 + PtLen] := Alg[1];
        PWord(@Msg[6 + PtLen])^ := htons(SL);
        if SL > 0 then
          Move(Sig[0], Msg[8 + PtLen], SL);
        DoSSLLog(Self, TidStr + ' SKE CurveType=' + IntToStr(Msg[0]));
        DoSSLLog(Self, TidStr + ' SKE NamedCurve=' + IntToHex((Integer(Msg[1]) shl 8) or Integer(Msg[2]), 4));
        SetLength(Tmp, PtLen);
        if PtLen > 0 then
          Move(Msg[4], Tmp[0], PtLen);
        DoSSLLog(Self, TidStr + ' SKE ECPoint Len=' + IntToStr(PtLen) + ' ECPoint=' + BytesToHex(Tmp));
        DoSSLLog(Self, TidStr + ' SKE SigAlg=' + IntToHex((Integer(Msg[4 + PtLen]) shl 8) or Integer(Msg[5 + PtLen]), 4));
        SL := (Integer(Msg[6 + PtLen]) shl 8) or Integer(Msg[7 + PtLen]);
        SetLength(Tmp, SL);
        if SL > 0 then
          Move(Msg[8 + PtLen], Tmp[0], SL);
        DoSSLLog(Self, TidStr + ' SKE SigLen=' + IntToStr(SL) + ' Sig=' + BytesToHex(Tmp));
        HH.HandShakeType := CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED;
        HH.LengthHi := (Length(Msg) shr 16) and $FF;
        HH.LengthLo := htons(Length(Msg) and $FFFF);
        SetLength(Total, 4 + Length(Msg));
        Move(HH, Total[0], 4);
        Move(Msg[0], Total[4], Length(Msg));
        H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
        H.MajorVersion := 3;
        H.MinorVersion := 3;
        H.BodyLength := htons(Length(Total));
        DoSSLLog(Self, TidStr + ' Send ServerKeyExchange Len=' + IntToStr(Length(Total)));
        if not SendExact(H, 5) then Exit;
        if not SendExact(Total[0], Length(Total)) then Exit;
        DoSSLLog(Self, TidStr + ' ServerKeyExchange Sent');
        AppendHS(CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED, Copy(Msg, 0, Length(Msg)));
      end
      else
      begin
        DoSSLLog(Self, TidStr + ' Load PrivateKey Failed: ' + KeyFile);
        Exit;
      end;
    finally
      Pri.Free;
      Pub.Free;
    end;
  finally
  end;
  
  SetLength(Msg, 0);
  HH.HandShakeType := CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED;
  HH.LengthHi := 0;
  HH.LengthLo := 0;
  SetLength(Total, 4);
  Move(HH, Total[0], 4);
  H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
  H.MajorVersion := 3;
  H.MinorVersion := 3;
  H.BodyLength := htons(Length(Total));
  DoSSLLog(Self, TidStr + ' Send ServerHelloDone');
  if not SendExact(H, 5) then Exit;
  if not SendExact(Total[0], Length(Total)) then Exit;
  DoSSLLog(Self, TidStr + ' ServerHelloDone Sent');
  AppendHS(CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED, nil);
  
  // Receive ClientKeyExchange
  DoSSLLog(Self, TidStr + ' Waiting ClientKeyExchange...');
  if not RecvExact(5, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' ClientKeyExchange header not received (client closed or error)');
    Exit;
  end;
  Move(Buf[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if not RecvExact(BodyLen, Buf) or (BodyLen < 2) then
      Exit;
    DoSSLLog(Self, TidStr + ' Alert Level=' + IntToStr(Buf[0]) + ' Desc=' + IntToStr(Buf[1]) + ' (' + AlertName(Buf[1]) + ')');
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_HANDSHAKE then
    Exit;
  BodyLen := ntohs(H.BodyLength);
  if not RecvExact(BodyLen, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' ClientKeyExchange body not received (client closed or error)');
    Exit;
  end;
  HS := Buf[0];
  if HS <> CN_TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE_RESERVED then
    Exit;
  SetLength(Tmp, BodyLen - 4);
  if BodyLen > 4 then
    Move(Buf[4], Tmp[0], BodyLen - 4);
  AppendHS(HS, Tmp);
  PtLen := Buf[4];
  if PtLen <> Byte(1 + 2 * Ecc.BytesCount) then
    Exit;
  if Buf[5] <> $04 then
    Exit;
  CliPub := TCnEccPublicKey.Create;
  try
    CliPub.X := TCnBigNumber.FromBinary(@Buf[6], Ecc.BytesCount);
    CliPub.Y := TCnBigNumber.FromBinary(@Buf[6 + Ecc.BytesCount], Ecc.BytesCount);
    P := TCnEccPoint.Create;
    try
      P.Assign(CliPub);
      Ecc.MultiplePoint(EpPriv, P);
      SetLength(PreMaster, Ecc.BytesCount);
      P.X.ToBinary(@PreMaster[0], Ecc.BytesCount);
    finally
      P.Free;
    end;
  finally
    CliPub.Free;
  end;
  
  // Derive keys
  Master := TLSPseudoRandomFunc(PreMaster, 'master secret',
    ConcatBytes(ClientRandom, ServerRandom), esdtSHA256, 48);
  KeyBlock := TLSPseudoRandomFunc(Master, 'key expansion',
    ConcatBytes(ServerRandom, ClientRandom), esdtSHA256, 2 * KeyLen + 2 * IvLen);
  ClientWriteKey := Copy(KeyBlock, 0, KeyLen);
  ServerWriteKey := Copy(KeyBlock, KeyLen, KeyLen);
  ClientFixedIV := Copy(KeyBlock, 2 * KeyLen, IvLen);
  ServerFixedIV := Copy(KeyBlock, 2 * KeyLen + IvLen, IvLen);
  ClientSeq := 0;
  ServerSeq := 0;
  
  // Receive ChangeCipherSpec
  DoSSLLog(Self, TidStr + ' Waiting ChangeCipherSpec...');
  if not RecvExact(5, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' ChangeCipherSpec header not received (client closed or error)');
    Exit;
  end;
  Move(Buf[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if not RecvExact(BodyLen, Buf) or (BodyLen < 2) then
    begin
      DoSSLLog(Self, TidStr + ' ChangeCipherSpec alert body not received');
      Exit;
    end;
    DoSSLLog(Self, TidStr + ' Alert Level=' + IntToStr(Buf[0]) + ' Desc=' + IntToStr(Buf[1]) + ' (' + AlertName(Buf[1]) + ')');
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC then
    Exit;
  BodyLen := ntohs(H.BodyLength);
  if not RecvExact(BodyLen, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' ChangeCipherSpec body not received (client closed or error)');
    Exit;
  end;
  
  // Receive Finished
  DoSSLLog(Self, TidStr + ' Waiting Finished...');
  if not RecvExact(5, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' Finished header not received (client closed or error)');
    Exit;
  end;
  Move(Buf[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if not RecvExact(BodyLen, Buf) or (BodyLen < 2) then
    begin
      DoSSLLog(Self, TidStr + ' Finished alert body not received');
      Exit;
    end;
    DoSSLLog(Self, TidStr + ' Alert Level=' + IntToStr(Buf[0]) + ' Desc=' + IntToStr(Buf[1]) + ' (' + AlertName(Buf[1]) + ')');
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_HANDSHAKE then
    Exit;
  BodyLen := ntohs(H.BodyLength);
  if not RecvExact(BodyLen, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' Finished body not received (client closed or error)');
    Exit;
  end;
  if UseChaCha then
  begin
    if BodyLen < SizeOf(TCnGCM128Tag) then
      Exit;
    SetLength(En, BodyLen - SizeOf(TCnGCM128Tag));
    if Length(En) > 0 then
      Move(Buf[0], En[0], Length(En));
    Move(Buf[BodyLen - SizeOf(TCnGCM128Tag)], Tag[0], SizeOf(TCnGCM128Tag));
    AAD := BuildAAD(CN_TLS_CONTENT_TYPE_HANDSHAKE, ClientSeq, 16);
    Plain := TLSChaCha20Poly1305Decrypt(ClientWriteKey, ClientFixedIV, En, AAD, ClientSeq, Tag);
  end
  else
  begin
    if BodyLen < 8 + SizeOf(TCnGCM128Tag) then
      Exit;
    SetLength(Tmp, 8);
    Move(Buf[0], Tmp[0], 8);
    DoSSLLog(Self, TidStr + ' Finished ExplicitNonce: ' + BytesToHex(Tmp));
    SetLength(En, BodyLen - 8 - SizeOf(TCnGCM128Tag));
    if Length(En) > 0 then
      Move(Buf[8], En[0], Length(En));
    Move(Buf[BodyLen - SizeOf(TCnGCM128Tag)], Tag[0], SizeOf(TCnGCM128Tag));
    AAD := BuildAAD(CN_TLS_CONTENT_TYPE_HANDSHAKE, ClientSeq, 16);
    Plain := AES128GCMDecryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, Tmp), En, AAD, Tag);
  end;
  if Length(Plain) < 16 then
  begin
    DoSSLLog(Self, TidStr + ' Finished decrypt failed or length mismatch, PlainLen=' + IntToStr(Length(Plain)));
    Exit;
  end;
  HSHash := TLSEccDigestBytes(HandshakeLog, esdtSHA256);
  EV := TLSPseudoRandomFunc(Master, 'client finished', HSHash, esdtSHA256, 12);
  DoSSLLog(Self, TidStr + ' Finished HSHash: ' + BytesToHex(HSHash));
  DoSSLLog(Self, TidStr + ' Finished ExpectedVerify: ' + BytesToHex(EV));
  SetLength(Tmp, 12);
  Move(Plain[4], Tmp[0], 12);
  DoSSLLog(Self, TidStr + ' Finished ReceivedVerify: ' + BytesToHex(Tmp));
  if not CompareMem(@Tmp[0], @EV[0], 12) then
  begin
    DoSSLLog(Self, TidStr + ' Finished verify_data mismatch');
    Exit;
  end;
  AppendHS(CN_TLS_HANDSHAKE_TYPE_FINISHED, Tmp);
  ClientSeq := ClientSeq + 1;
  
  // Send ChangeCipherSpec
  SetLength(Msg, 1);
  Msg[0] := 1;
  H.ContentType := CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC;
  H.MajorVersion := 3;
  H.MinorVersion := 3;
  H.BodyLength := htons(1);
  if not SendExact(H, 5) then Exit;
  if not SendExact(Msg[0], 1) then Exit;
  
  // Send Finished
  HSHash := TLSEccDigestBytes(HandshakeLog, esdtSHA256);
  SV := TLSPseudoRandomFunc(Master, 'server finished', HSHash, esdtSHA256, 12);
  SetLength(Tmp, 1 + 3 + 12);
  Tmp[0] := 20;
  Tmp[1] := 0;
  PWord(@Tmp[2])^ := htons(12);
  Move(SV[0], Tmp[4], 12);
  AAD := BuildAAD(CN_TLS_CONTENT_TYPE_HANDSHAKE, ServerSeq, 16);
  if UseChaCha then
  begin
    En := TLSChaCha20Poly1305Encrypt(ServerWriteKey, ServerFixedIV, Tmp, AAD, ServerSeq, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    H.BodyLength := htons(Length(En) + SizeOf(TCnGCM128Tag));
    if not SendExact(H, 5) then Exit;
    if (Length(En) > 0) and (not SendExact(En[0], Length(En))) then Exit;
    if not SendExact(Tag[0], SizeOf(TCnGCM128Tag)) then Exit;
  end
  else
  begin
    SetLength(Tmp, 8);
    Tmp[0] := Byte((ServerSeq shr 56) and $FF);
    Tmp[1] := Byte((ServerSeq shr 48) and $FF);
    Tmp[2] := Byte((ServerSeq shr 40) and $FF);
    Tmp[3] := Byte((ServerSeq shr 32) and $FF);
    Tmp[4] := Byte((ServerSeq shr 24) and $FF);
    Tmp[5] := Byte((ServerSeq shr 16) and $FF);
    Tmp[6] := Byte((ServerSeq shr 8) and $FF);
    Tmp[7] := Byte(ServerSeq and $FF);
    SetLength(Plain, 1 + 3 + 12);
    Plain[0] := 20;
    Plain[1] := 0;
    PWord(@Plain[2])^ := htons(12);
    Move(SV[0], Plain[4], 12);
    En := AES128GCMEncryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, Tmp), Plain, AAD, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    H.BodyLength := htons(8 + Length(En) + SizeOf(TCnGCM128Tag));
    if not SendExact(H, 5) then Exit;
    if not SendExact(Tmp[0], 8) then Exit;
    if (Length(En) > 0) and (not SendExact(En[0], Length(En))) then Exit;
    if not SendExact(Tag[0], SizeOf(TCnGCM128Tag)) then Exit;
  end;
  ServerSeq := ServerSeq + 1;
  
  // Read one HTTP request
  DoSSLLog(Self, TidStr + ' Waiting ApplicationData (HTTP)...');
  if not RecvExact(5, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' ApplicationData header not received (client closed or error)');
    Exit;
  end;
  Move(Buf[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if not RecvExact(BodyLen, Buf) or (BodyLen < 2) then
    begin
      DoSSLLog(Self, TidStr + ' ApplicationData alert body not received');
      Exit;
    end;
    DoSSLLog(Self, TidStr + ' Alert Level=' + IntToStr(Buf[0]) + ' Desc=' + IntToStr(Buf[1]) + ' (' + AlertName(Buf[1]) + ')');
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_APPLICATION_DATA then
    Exit;
  BodyLen := ntohs(H.BodyLength);
  if not RecvExact(BodyLen, Buf) then
  begin
    DoSSLLog(Self, TidStr + ' ApplicationData body not received (client closed or error)');
    Exit;
  end;
  if UseChaCha then
  begin
    if BodyLen < SizeOf(TCnGCM128Tag) then
      Exit;
    SetLength(En, BodyLen - SizeOf(TCnGCM128Tag));
    if Length(En) > 0 then
      Move(Buf[0], En[0], Length(En));
    Move(Buf[BodyLen - SizeOf(TCnGCM128Tag)], Tag[0], SizeOf(TCnGCM128Tag));
    AAD := BuildAAD(CN_TLS_CONTENT_TYPE_APPLICATION_DATA, ClientSeq, Length(En));
    Plain := TLSChaCha20Poly1305Decrypt(ClientWriteKey, ClientFixedIV, En, AAD, ClientSeq, Tag);
  end
  else
  begin
    if BodyLen < 8 + SizeOf(TCnGCM128Tag) then
      Exit;
    SetLength(Tmp, 8);
    Move(Buf[0], Tmp[0], 8);
    SetLength(En, BodyLen - 8 - SizeOf(TCnGCM128Tag));
    if Length(En) > 0 then
      Move(Buf[8], En[0], Length(En));
    Move(Buf[BodyLen - SizeOf(TCnGCM128Tag)], Tag[0], SizeOf(TCnGCM128Tag));
    AAD := BuildAAD(CN_TLS_CONTENT_TYPE_APPLICATION_DATA, ClientSeq, Length(En));
    Plain := AES128GCMDecryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, Tmp), En, AAD, Tag);
  end;
  if Plain = nil then
    Exit;
  ClientSeq := ClientSeq + 1;
  
  // Send 404 response
  Resp := 'HTTP/1.1 404 Not Found'#13#10'Content-Type: text/plain'#13#10'Content-Length: 9'#13#10'Connection: close'#13#10#13#10'Not Found';
  RespB := nil;
  SetLength(RespB, Length(Resp));
  if Length(RespB) > 0 then
    Move(PAnsiChar(AnsiString(Resp))^, RespB[0], Length(Resp));
  AAD := BuildAAD(CN_TLS_CONTENT_TYPE_APPLICATION_DATA, ServerSeq, Length(RespB));
  if UseChaCha then
  begin
    En := TLSChaCha20Poly1305Encrypt(ServerWriteKey, ServerFixedIV, RespB, AAD, ServerSeq, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    H.BodyLength := htons(Length(En) + SizeOf(TCnGCM128Tag));
    if not SendExact(H, 5) then Exit;
    if (Length(En) > 0) and (not SendExact(En[0], Length(En))) then Exit;
    if not SendExact(Tag[0], SizeOf(TCnGCM128Tag)) then Exit;
  end
  else
  begin
    SetLength(Tmp, 8);
    Tmp[0] := Byte((ServerSeq shr 56) and $FF);
    Tmp[1] := Byte((ServerSeq shr 48) and $FF);
    Tmp[2] := Byte((ServerSeq shr 40) and $FF);
    Tmp[3] := Byte((ServerSeq shr 32) and $FF);
    Tmp[4] := Byte((ServerSeq shr 24) and $FF);
    Tmp[5] := Byte((ServerSeq shr 16) and $FF);
    Tmp[6] := Byte((ServerSeq shr 8) and $FF);
    Tmp[7] := Byte(ServerSeq and $FF);
    En := AES128GCMEncryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, Tmp), RespB, AAD, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    H.BodyLength := htons(8 + Length(En) + SizeOf(TCnGCM128Tag));
    if not SendExact(H, 5) then Exit;
    if not SendExact(Tmp[0], 8) then Exit;
    if (Length(En) > 0) and (not SendExact(En[0], Length(En))) then Exit;
    if not SendExact(Tag[0], SizeOf(TCnGCM128Tag)) then Exit;
  end;
  ServerSeq := ServerSeq + 1;
  
  Result := True;
  if EpPriv <> nil then EpPriv.Free;
  if EpPub <> nil then EpPub.Free;
  if Ecc <> nil then Ecc.Free;
end;

procedure TFormTCPServer.TCPAccept(Sender: TObject; ClientSocket: TCnClientSocket);
var
  C: Integer;
  RecvBuf: array[0..1023] of Byte;
  SendBuf: array[0..1023] of Byte;
begin
  Log('Connected: ' + ClientSocket.RemoteIP + ':' + IntToStr(ClientSocket.RemotePort));
  // 演示服务端收、发
  C := ClientSocket.Recv(RecvBuf, SizeOf(RecvBuf) - 1);
  if C = SOCKET_ERROR then
    Exit;

  Log('Get ' + IntToStr(C) + ' Bytes.');
  SendBuf[0] := Ord('A');
  SendBuf[1] := Ord('B');
  C := ClientSocket.Send(SendBuf, 2);
  if C = SOCKET_ERROR then
    Exit;

  Log('Send ' + IntToStr(C) + ' Bytes.');
end;

procedure TFormTCPServer.TLSAccept(Sender: TObject; ClientSocket: TCnClientSocket);
begin
  Log('Connected(TLS): ' + ClientSocket.RemoteIP + ':' + IntToStr(ClientSocket.RemotePort));
  if not DoServerTLSHandshake(ClientSocket) then
  begin
    Log('TLS Handshake Failed.');
    ClientSocket.Shutdown;
    Exit;
  end;
  Log('TLS Handshake Success.');
end;

procedure TFormTCPServer.TCPError(Sender: TObject; SocketError: Integer);
begin
  Log('*** Socket Error: ' + IntToStr(SocketError));
end;

end.

