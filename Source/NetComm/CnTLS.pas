{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTLS;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包 TLS Client 实现单元
* 单元作者：CnPack 开发组
* 备    注：基于阻塞式 TCP 客户端之上实现的 TLS 安全传输客户端
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.01.09 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, {$IFDEF MSWINDOWS} Windows, WinSock, {$ELSE}
  {$IFDEF FPC} Sockets, {$ELSE}
  System.Net.Socket, Posix.NetinetIn, Posix.SysSocket, Posix.Unistd, Posix.ArpaInet,
  {$ENDIF} {$ENDIF}CnConsts, CnNetConsts, CnSocket, CnClasses, CnNetwork,
  CnRandom, CnPoly1305, CnBigNumber, CnNative, CnECC, CnAEAD, CnChaCha20, CnSHA2,
  CnSM3, CnMD5, CnSHA1, CnRSA, CnPemUtils, CnTCPClient, CnThreadingTCPServer;

type
  ECnTLSException = class(Exception);

  TCnTLSType = (cttTLS1_2); // TLS 1.3 not now

  TCnTLSogEvent = procedure (Sender: TObject; const LogMsg: string) of object;

  TCnTLSHandshakeState = (
    hsInit,
    hsClientHelloSent,
    hsServerHelloReceived,
    hsCertificateReceived,
    hsServerKeyExchangeReceived,
    hsServerHelloDoneReceived,
    hsClientKeyExchangeSent,
    hsChangeCipherSpecSent,
    hsFinishedSent,
    hsServerChangeCipherSpecReceived,
    hsServerFinishedReceived,
    hsConnected
  );

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnTLSClient = class(TCnTCPClient)
  private
    FClientWriteKey: TBytes;
    FServerWriteKey: TBytes;
    FClientFixedIV: TBytes;
    FServerFixedIV: TBytes;
    FKeyLen: Integer;
    FIVLen: Integer;
    FDigestType: TCnEccSignDigestType;
    FCipherIsSM4GCM: Boolean;
    FCipherIsChaCha20Poly1305: Boolean;
    FClientSeq: TUInt64;
    FServerSeq: TUInt64;

    FHandshakeState: TCnTLSHandshakeState;
    FVerifyCertificate: Boolean;
    FEnableExtendedMasterSecret: Boolean;
    FOnTLSLog: TCnTLSogEvent;
    FRecvPlainBuf: TBytes;
    FRecvPlainPos: Integer;
    FMaxFragmentLen: Word;

    procedure DoTLSLog(const Msg: string);
    function BuildAppDataAAD(Seq: TUInt64; PlainLen: Integer): TBytes;
    function EncryptAppDataBody(const Plain, AAD, Nonce: TBytes; out Tag:
      TCnGCM128Tag): TBytes;
    function DecryptAppDataBody(const Body, AAD: TBytes; const Nonce: TBytes;
      var InTag: TCnGCM128Tag): TBytes;
    function EncryptAndSendAppData(const Plain: TBytes; Flags: Integer): Integer;
    function ReadAndDecryptAppData(var Buf; Len: Integer; Flags: Integer): Integer;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    procedure DoConnect; override;
    function DoHandShake(const Host: AnsiString; Port: Word): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Send(var Buf; Len: Integer; Flags: Integer = 0): Integer; override;
    function Recv(var Buf; Len: Integer; Flags: Integer = 0): Integer; override;

    property HandshakeState: TCnTLSHandshakeState read FHandshakeState;
  published
    property VerifyCertificate: Boolean read FVerifyCertificate write FVerifyCertificate default True;
    property EnableExtendedMasterSecret: Boolean read FEnableExtendedMasterSecret
      write FEnableExtendedMasterSecret default True;
    property MaxFragmentLen: Word read FMaxFragmentLen write FMaxFragmentLen default 16384;
    property OnTLSLog: TCnTLSogEvent read FOnTLSLog write FOnTLSLog;
  end;

type
  TCnTLSServer = class;

  TCnTLSServerClientSocket = class(TCnClientSocket)
  private
    FVersion: Word;
    FCipherSuite: Word;
    FHandshakeDone: Boolean;
    FServerWriteKey: TBytes;
    FClientWriteKey: TBytes;
    FServerFixedIV: TBytes;
    FClientFixedIV: TBytes;
    FServerSeq: TUInt64;
    FClientSeq: TUInt64;
    FHandshakeLog: TBytes;
    FRecvPlainBuf: TBytes;
    FRecvPlainPos: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Send(var Buf; Len: Integer; Flags: Integer = 0): Integer; override;
    function Recv(var Buf; Len: Integer; Flags: Integer = 0): Integer; override;

    property Version: Word read FVersion write FVersion;
    property CipherSuite: Word read FCipherSuite write FCipherSuite;
    property HandshakeDone: Boolean read FHandshakeDone write FHandshakeDone;
    property ServerWriteKey: TBytes read FServerWriteKey write FServerWriteKey;
    property ClientWriteKey: TBytes read FClientWriteKey write FClientWriteKey;
    property ServerFixedIV: TBytes read FServerFixedIV write FServerFixedIV;
    property ClientFixedIV: TBytes read FClientFixedIV write FClientFixedIV;
    property ServerSeq: TUInt64 read FServerSeq write FServerSeq;
    property ClientSeq: TUInt64 read FClientSeq write FClientSeq;
    property HandshakeLog: TBytes read FHandshakeLog write FHandshakeLog;
  end;

  TCnTLSServerClientThread = class(TCnTCPClientThread)
  private
    FOwnerServer: TCnTLSServer;
  protected
    procedure Execute; override;
    function DoGetClientSocket: TCnClientSocket; override;
  end;

  TCnTLSServerErrorEvent = procedure(Sender: TObject; ClientSocket:
    TCnClientSocket; Code: Integer; const Msg: string) of object;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnTLSServer = class(TCnThreadingTCPServer)
  private
    FServerCertFile: string;
    FServerKeyFile: string;
    FServerKeyPassword: string;
    FPreferChaCha: Boolean;
    FMaxFragmentLen: Word;
    FOnTLSAccept: TCnSocketAcceptEvent;
    FOnTLSError: TCnTLSServerErrorEvent;
    FOnTLSLog: TCnTLSogEvent;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    function DoGetClientThread: TCnTCPClientThread; override;
    function DoHandShake(ClientSocket: TCnTLSServerClientSocket): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SendPlainData(ClientSocket: TCnClientSocket; const Data: TBytes): Boolean;
    property ServerCertFile: string read FServerCertFile write FServerCertFile;
    property ServerKeyFile: string read FServerKeyFile write FServerKeyFile;
    property ServerKeyPassword: string read FServerKeyPassword write FServerKeyPassword;
    property PreferChaCha: Boolean read FPreferChaCha write FPreferChaCha;
  published
    property MaxFragmentLen: Word read FMaxFragmentLen write FMaxFragmentLen default 16384;
    property OnTLSAccept: TCnSocketAcceptEvent read FOnTLSAccept write FOnTLSAccept;
    property OnTLSError: TCnTLSServerErrorEvent read FOnTLSError write FOnTLSError;
    property OnTLSLog: TCnTLSogEvent read FOnTLSLog write FOnTLSLog;
  end;

function TLSUInt64ToBE8(Value: TUInt64): TBytes;
function TLSChaChaNonce(const FixedIV12: TBytes; Seq: TUInt64): TBytes;
function TLSPoly1305Tag(AAD, C: TBytes; PolyKey: TBytes): TCnGCM128Tag;
function TLSChaCha20Poly1305Encrypt(Key, FixedIV12, Plain, AAD: TBytes; Seq: TUInt64; out Tag: TCnGCM128Tag): TBytes;
function TLSChaCha20Poly1305Decrypt(Key, FixedIV12, En, AAD: TBytes; Seq: TUInt64; InTag: TCnGCM128Tag): TBytes;
function TLSEccDigestBytes(Data: TBytes; DigestType: TCnEccSignDigestType): TBytes;
function TLSEccHMacBytes(Key, Data: TBytes; DigestType: TCnEccSignDigestType): TBytes;
function TLSPseudoRandomFunc(Secret: TBytes; const PLabel: AnsiString; Seed: TBytes; DigestType: TCnEccSignDigestType; NeedLength: Integer): TBytes;

implementation

resourcestring
  SHandshakeFailed = 'TLS Handshake Failed: %s';
  SConnectionFailed = 'TLS Connection Failed';
  SEncryptionFailed = 'TLS Encryption Failed';
  SDecryptionFailed = 'TLS Decryption Failed';
  SInvalidCipherSuite = 'Invalid Cipher Suite: 0x%.4x';

function TLSUInt64ToBE8(Value: TUInt64): TBytes;
var
  R: TBytes;
  I: Integer;
begin
  SetLength(R, 8);
  if CurrentByteOrderIsBigEndian then
  begin
    for I := 0 to 7 do
      R[I] := Byte((Value shr (I * 8)) and $FF);
  end
  else
  begin
    for I := 0 to 7 do
      R[7 - I] := Byte((Value shr (I * 8)) and $FF);
  end;
  Result := R;
end;

function TCnTLSServer.DoHandShake(ClientSocket: TCnTLSServerClientSocket): Boolean;
var
  R: TBytes;
  H: TCnTLSRecordLayer;
  Len, BodyLen, C: Integer;
  HS: Byte;
  ClientRandom, ServerRandom: TBytes;
  Msg: TBytes;
  HH: TCnTLSHandShakeHeader;
  Total: TBytes;
  CertDER: TBytes;
  CertFile: string;
  Mem: TMemoryStream;
  VLen, CLen: Integer;
  Ecc: TCnEcc;
  EpPriv: TCnEccPrivateKey;
  EpPub: TCnEccPublicKey;
  EccSign: TCnEcc;
  CurveBytes: array[0..1] of Byte;
  PtLen: Byte;
  Pt: TBytes;
  KeyFile: string;
  Pri: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
  EccPrivKey: TCnEccPrivateKey;
  EccPubKey: TCnEccPublicKey;
  ServerCurve: TCnEccCurveType;
  Sig: TBytes;
  ToSign: TBytes;
  Alg: array[0..1] of Byte;
  SL: Word;
  HandshakeLog: TBytes;
  PreMaster: TBytes;
  Master: TBytes;
  KeyBlock: TBytes;
  ClientWriteKey, ServerWriteKey: TBytes;
  ClientFixedIV, ServerFixedIV: TBytes;
  ClientSeq, ServerSeq: TUInt64;
  AAD, Plain, En, HSHash, EV, SV, RespB, Tmp, Exts: TBytes;
  Buf: TBytes;
  Tag: TCnGCM128Tag;
  Resp: AnsiString;
  CliPub: TCnEccPublicKey;
  P: TCnEccPoint;
  SelCipherHi, SelCipherLo: Byte;
  Found: Boolean;
  HaveC02F: Boolean;
  HaveC02B: Boolean;
  HaveCCA8: Boolean;
  HaveCCA9: Boolean;
  KeyIsRSA: Boolean;
  KeyIsECC: Boolean;
  Off, SidLen, CSLen, I: Integer;
  CHi, CLo: Byte;
  SuiteStr, sHave, sHave2: string;
  CompLen, ExtsLen, ExtType, ExtLen, PairsLen, J: Integer;
  KeyLen, IvLen: Integer;
  UseChaCha: Boolean;
  OutS: TMemoryStream;

  procedure DoTLSLog(const S: string);
  begin
    if Assigned(Self.FOnTLSLog) then
      Self.FOnTLSLog(Self, S);
  end;

  function SendExact(const Buf; L: Integer): Boolean;
  var
    Sent, Cnt: Integer;
    P: PAnsiChar;
  begin
    Result := False;
    P := @Buf;
    Sent := 0;
    while Sent < L do
    begin
      Cnt := ClientSocket.Send(P[Sent], L - Sent);
      if Cnt = SOCKET_ERROR then
        Exit;
      if Cnt = 0 then
        Exit;
      Inc(Sent, Cnt);
    end;
    Result := True;
  end;

  function RecvExact(Need: Integer; out OutBuf: TBytes): Boolean;
  var
    Got, Cnt: Integer;
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
      0:
        Result := 'close_notify';
      10:
        Result := 'unexpected_message';
      20:
        Result := 'bad_record_mac';
      21:
        Result := 'decryption_failed';
      22:
        Result := 'record_overflow';
      40:
        Result := 'handshake_failure';
      41:
        Result := 'no_certificate';
      42:
        Result := 'bad_certificate';
      43:
        Result := 'unsupported_certificate';
      44:
        Result := 'certificate_revoked';
      45:
        Result := 'certificate_expired';
      46:
        Result := 'certificate_unknown';
      47:
        Result := 'illegal_parameter';
      48:
        Result := 'unknown_ca';
      49:
        Result := 'access_denied';
      50:
        Result := 'decode_error';
      51:
        Result := 'decrypt_error';
      70:
        Result := 'protocol_version';
      71:
        Result := 'insufficient_security';
      80:
        Result := 'internal_error';
      90:
        Result := 'user_canceled';
      100:
        Result := 'no_renegotiation';
    else
      Result := 'unknown';
    end;
  end;

  function BuildAAD2(CT: Byte; Seq: TUInt64; PlainLen: Integer): TBytes;
  var
    A: TBytes;
    V: TBytes;
    L2: Word;
  begin
    V := TLSUInt64ToBE8(Seq);
    SetLength(A, 13);
    Move(V[0], A[0], 8);
    A[8] := CT;
    A[9] := 3;
    A[10] := 3;
    L2 := htons(PlainLen);
    Move(L2, A[11], 2);
    Result := A;
  end;

  procedure AppendHS(HSType: Byte; const Body: TBytes);
  var
    Hdr: array[0..3] of Byte;
    L3: Integer;
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
  DoTLSLog('Start TLS Handshake');
  Len := ClientSocket.Recv(H, 5);
  if Len <> 5 then
  begin
    DoTLSLog('Failed to read record header Len=' + IntToStr(Len));
    Exit;
  end;

  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    SetLength(Buf, BodyLen);
    Len := ClientSocket.Recv(Buf[0], BodyLen);
    if (Len = SOCKET_ERROR) or (Len <> BodyLen) or (BodyLen < 2) then
    begin
      DoTLSLog('Alert received with invalid length BodyLen=' + IntToStr(BodyLen));
      Exit;
    end;
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_HANDSHAKE then
  begin
    DoTLSLog('Record type is not Handshake Type=' + IntToStr(H.ContentType));
    Exit;
  end;

  BodyLen := ntohs(H.BodyLength);
  if (BodyLen <= 0) or (BodyLen > 16384) then
  begin
    DoTLSLog('Invalid handshake body length Len=' + IntToStr(BodyLen));
    Exit;
  end;

  SetLength(Buf, BodyLen);
  Len := ClientSocket.Recv(Buf[0], BodyLen);
  if Len = SOCKET_ERROR then
  begin
    DoTLSLog('Failed to receive handshake body');
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
    begin
      DoTLSLog('Handshake body incomplete');
      Exit;
    end;
  end;

  HS := Buf[0];
  if HS <> CN_TLS_HANDSHAKE_TYPE_CLIENT_HELLO then
  begin
    DoTLSLog('Expected ClientHello, got=' + IntToStr(HS));
    Exit;
  end;

  SetLength(ClientRandom, 32);
  Move(Buf[6], ClientRandom[0], 32);
  SelCipherHi := $CC;
  SelCipherLo := $A8;
  Found := False;
  HaveC02F := False;
  HaveC02B := False;
  HaveCCA8 := False;
  HaveCCA9 := False;
  Off := 6 + 32;

  if BodyLen > Off then
  begin
    SidLen := Buf[Off];
    Inc(Off);
    Inc(Off, SidLen);

    if BodyLen >= Off + 2 then
    begin
      CSLen := (Integer(Buf[Off]) shl 8) or Integer(Buf[Off + 1]);
      Inc(Off, 2);
      I := 0;

      while (I + 1 < CSLen) and (Off + I + 1 < BodyLen) do
      begin
        CHi := Buf[Off + I];
        CLo := Buf[Off + I + 1];
        if (CHi = $CC) and (CLo = $A8) then
          HaveCCA8 := True
        else if (CHi = $CC) and (CLo = $A9) then
          HaveCCA9 := True
        else if (CHi = $C0) and (CLo = $2F) then
          HaveC02F := True
        else if (CHi = $C0) and (CLo = $2B) then
          HaveC02B := True;
        if (CHi = $C0) and (CLo = $2F) then
          HaveC02F := True;
        Inc(I, 2);
      end;

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
          end;
        end;
      end;
    end;
  end;

  KeyFile := FServerKeyFile;
  if KeyFile = '' then
    KeyFile := ExtractFilePath(ParamStr(0)) + 'server_key.pem';
  KeyIsRSA := False;
  KeyIsECC := False;
  Pri := TCnRSAPrivateKey.Create(True);
  Pub := TCnRSAPublicKey.Create;
  EccPrivKey := TCnEccPrivateKey.Create;
  EccPubKey := TCnEccPublicKey.Create;
  ServerCurve := ctSecp256r1;

  try
    if CnRSALoadKeysFromPem(KeyFile, Pri, Pub, ckhMd5, AnsiString(FServerKeyPassword)) then
      KeyIsRSA := True
    else
    if CnEccLoadKeysFromPem(KeyFile, EccPrivKey, EccPubKey, ServerCurve, ckhMd5,
      AnsiString(FServerKeyPassword)) then
      KeyIsECC := True;
  except
  end;

  if KeyIsECC then
  begin
    if HaveCCA9 then
    begin SelCipherHi := $CC; SelCipherLo := $A9; end
    else if HaveC02B then
    begin SelCipherHi := $C0; SelCipherLo := $2B; end
    else
    begin
      // 客户端不支持 ECDSA 套件，失败
      DoTLSLog('Client does not support ECDSA cipher suites');
      H.ContentType := CN_TLS_CONTENT_TYPE_ALERT;
      H.MajorVersion := 3;
      H.MinorVersion := 3;
      H.BodyLength := htons(2);
      if not SendExact(H, 5) then Exit;
      Msg := nil; SetLength(Msg, 2);
      Msg[0] := CN_TLS_ALERT_LEVEL_FATAL;
      Msg[1] := CN_TLS_ALERT_DESC_HANDSHAKE_FAILURE;
      if not SendExact(Msg[0], 2) then Exit;
      Exit;
    end;
  end
  else
  begin
    if HaveCCA8 then
    begin
      SelCipherHi := $CC;
      SelCipherLo := $A8;
    end
    else if HaveC02F then
    begin
      SelCipherHi := $C0;
      SelCipherLo := $2F;
    end
    else
    begin
      DoTLSLog('Client does not support RSA cipher suites');
      H.ContentType := CN_TLS_CONTENT_TYPE_ALERT;
      H.MajorVersion := 3;
      H.MinorVersion := 3;
      H.BodyLength := htons(2);
      if not SendExact(H, 5) then
        Exit;

      Msg := nil;
      SetLength(Msg, 2);
      Msg[0] := CN_TLS_ALERT_LEVEL_FATAL;
      Msg[1] := CN_TLS_ALERT_DESC_HANDSHAKE_FAILURE;
      if not SendExact(Msg[0], 2) then
        Exit;

      Exit;
    end;
  end;

  UseChaCha := (SelCipherHi = $CC) and ((SelCipherLo = $A8) or (SelCipherLo = $A9));
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
  SetLength(ServerRandom, 32);
  CnRandomFillBytes(@ServerRandom[0], 32);
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

  if not SendExact(H, 5) then
  begin
    DoTLSLog('Failed to send ServerHello');
    Exit;
  end;
  if (Length(Total) > 0) and (not SendExact(Total[0], Length(Total))) then
  begin
    DoTLSLog('Failed to send ServerHello payload');
    Exit;
  end;
  AppendHS(CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO, Copy(Msg, 0, Length(Msg)));
  CertFile := Self.FServerCertFile;
  if CertFile = '' then
    CertFile := ExtractFilePath(ParamStr(0)) + 'server_cert.crt';

  Mem := TMemoryStream.Create;
  try
    if LoadPemFileToMemory(CertFile, '-----BEGIN CERTIFICATE-----',
      '-----END CERTIFICATE-----', Mem) then
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
      if not SendExact(H, 5) then
        Exit;
      if not SendExact(Total[0], Length(Total)) then
        Exit;
      AppendHS(CN_TLS_HANDSHAKE_TYPE_CERTIFICATE, Copy(Msg, 0, Length(Msg)));
    end
    else
    begin
      DoTLSLog('Failed to load server certificate ' + CertFile);
      Exit;
    end;
  finally
    Mem.Free;
  end;

  Ecc := TCnEcc.Create(ctSecp256r1);
  EpPriv := TCnEccPrivateKey.Create;
  EpPub := TCnEccPublicKey.Create;

  Ecc.GenerateKeys(EpPriv, EpPub);
  CurveBytes[0] := 0;
  CurveBytes[1] := $17;
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

  // 根据私钥类型进行签名与算法标识
  if KeyIsRSA then
  begin
    Sig := CnRSASignBytes(ToSign, Pri, rsdtSHA256);
    Alg[0] := 4; // sha256
    Alg[1] := 1; // rsa
    SL := Length(Sig);
    SetLength(Msg, Length(Msg) + 2 + 2 + SL);
    Msg[4 + PtLen] := Alg[0];
    Msg[5 + PtLen] := Alg[1];
    PWord(@Msg[6 + PtLen])^ := htons(SL);
    if SL > 0 then
      Move(Sig[0], Msg[8 + PtLen], SL);
  end
  else
  begin
    // ECDSA 签名（DER 序列 r,s）
    Mem := TMemoryStream.Create;
    try
      if Length(ToSign) > 0 then
        Mem.Write(ToSign[0], Length(ToSign));
      Mem.Position := 0;
      // 输出到临时流，获取 DER 编码的 (r,s)
      OutS := TMemoryStream.Create;
      try
        EccSign := TCnEcc.Create(ServerCurve);
        try
          if not CnEccSignStream(Mem, OutS, EccSign, EccPrivKey, esdtSHA256) then
            Exit;
          SetLength(Sig, OutS.Size);
          if OutS.Size > 0 then
          begin
            OutS.Position := 0;
            OutS.Read(Sig[0], OutS.Size);
          end;
        finally
          EccSign.Free;
        end;
      finally
        OutS.Free;
      end;
    finally
      Mem.Free;
    end;
    // 写入签名与算法
    Alg[0] := 4; // sha256
    Alg[1] := 3; // ecdsa
    SL := Length(Sig);
    SetLength(Msg, Length(Msg) + 2 + 2 + SL);
    Msg[4 + PtLen] := Alg[0];
    Msg[5 + PtLen] := Alg[1];
    PWord(@Msg[6 + PtLen])^ := htons(SL);
    if SL > 0 then
      Move(Sig[0], Msg[8 + PtLen], SL);
  end;

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

  if not SendExact(H, 5) then
  begin
    DoTLSLog('Failed to send ServerKeyExchange');
    Exit;
  end;
  if not SendExact(Total[0], Length(Total)) then
  begin
    DoTLSLog('Failed to send ServerKeyExchange payload');
    Exit;
  end;
  AppendHS(CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED, Copy(Msg, 0, Length(Msg)));

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

  if not SendExact(H, 5) then
  begin
    DoTLSLog('Failed to send ServerHelloDone');
    Exit;
  end;
  if not SendExact(Total[0], Length(Total)) then
  begin
    DoTLSLog('Failed to send ServerHelloDone payload');
    Exit;
  end;
  AppendHS(CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED, nil);
  if not RecvExact(5, Buf) then
  begin
    DoTLSLog('Failed waiting for ChangeCipherSpec');
    Exit;
  end;
  Move(Buf[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if not RecvExact(BodyLen, Buf) or (BodyLen < 2) then
      Exit;
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_HANDSHAKE then
    Exit;

  BodyLen := ntohs(H.BodyLength);
  if not RecvExact(BodyLen, Buf) then
    Exit;
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
  Ecc.Free;

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
  if not RecvExact(5, Buf) then
    Exit;

  Move(Buf[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if not RecvExact(BodyLen, Buf) or (BodyLen < 2) then
    begin
      DoTLSLog('Alert received during CCS stage');
      Exit;
    end;
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC then
  begin
    DoTLSLog('Expected ChangeCipherSpec, got type=' + IntToStr(H.ContentType));
    Exit;
  end;

  BodyLen := ntohs(H.BodyLength);
  if not RecvExact(BodyLen, Buf) then
  begin
    DoTLSLog('Failed to receive Finished record');
    Exit;
  end;
  if not RecvExact(5, Buf) then
    Exit;

  Move(Buf[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if not RecvExact(BodyLen, Buf) or (BodyLen < 2) then
      Exit;
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_HANDSHAKE then
    Exit;

  BodyLen := ntohs(H.BodyLength);
  if not RecvExact(BodyLen, Buf) then
    Exit;

  if UseChaCha then
  begin
    if BodyLen < SizeOf(TCnGCM128Tag) then
      Exit;
    SetLength(En, BodyLen - SizeOf(TCnGCM128Tag));
    if Length(En) > 0 then
      Move(Buf[0], En[0], Length(En));
    Move(Buf[BodyLen - SizeOf(TCnGCM128Tag)], Tag[0], SizeOf(TCnGCM128Tag));
    AAD := BuildAAD2(CN_TLS_CONTENT_TYPE_HANDSHAKE, ClientSeq, 16);
    Plain := TLSChaCha20Poly1305Decrypt(ClientWriteKey, ClientFixedIV, En, AAD,
      ClientSeq, Tag);
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
    AAD := BuildAAD2(CN_TLS_CONTENT_TYPE_HANDSHAKE, ClientSeq, 16);
    Plain := AES128GCMDecryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV,
      Tmp), En, AAD, Tag);
  end;
  if Length(Plain) < 16 then
  begin
    DoTLSLog('Failed to decrypt client Finished');
    Exit;
  end;

  HSHash := TLSEccDigestBytes(HandshakeLog, esdtSHA256);
  EV := TLSPseudoRandomFunc(Master, 'client finished', HSHash, esdtSHA256, 12);
  SetLength(Tmp, 12);
  Move(Plain[4], Tmp[0], 12);
  if not CompareMem(@Tmp[0], @EV[0], 12) then
  begin
    DoTLSLog('Client Finished verify mismatch');
    Exit;
  end;

  AppendHS(CN_TLS_HANDSHAKE_TYPE_FINISHED, Tmp);
  ClientSeq := ClientSeq + 1;
  SetLength(Msg, 1);
  Msg[0] := 1;
  H.ContentType := CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC;
  H.MajorVersion := 3;
  H.MinorVersion := 3;
  H.BodyLength := htons(1);
  if not SendExact(H, 5) then
  begin
    DoTLSLog('Failed to send ChangeCipherSpec');
    Exit;
  end;
  if not SendExact(Msg[0], 1) then
  begin
    DoTLSLog('Failed to send ChangeCipherSpec payload');
    Exit;
  end;

  HSHash := TLSEccDigestBytes(HandshakeLog, esdtSHA256);
  SV := TLSPseudoRandomFunc(Master, 'server finished', HSHash, esdtSHA256, 12);
  SetLength(Tmp, 1 + 3 + 12);
  Tmp[0] := 20;
  Tmp[1] := 0;
  PWord(@Tmp[2])^ := htons(12);
  Move(SV[0], Tmp[4], 12);
  AAD := BuildAAD2(CN_TLS_CONTENT_TYPE_HANDSHAKE, ServerSeq, 16);

  if UseChaCha then
  begin
    En := TLSChaCha20Poly1305Encrypt(ServerWriteKey, ServerFixedIV, Tmp, AAD,
      ServerSeq, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    H.BodyLength := htons(Length(En) + SizeOf(TCnGCM128Tag));
    if not SendExact(H, 5) then
    begin
      DoTLSLog('Failed to send ServerFinished');
      Exit;
    end;
    if (Length(En) > 0) and (not SendExact(En[0], Length(En))) then
    begin
      DoTLSLog('Failed to send ServerFinished payload');
      Exit;
    end;
    if not SendExact(Tag[0], SizeOf(TCnGCM128Tag)) then
    begin
      DoTLSLog('Failed to send ServerFinished tag');
      Exit;
    end;
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
    En := AES128GCMEncryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, Tmp),
      Plain, AAD, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    H.BodyLength := htons(8 + Length(En) + SizeOf(TCnGCM128Tag));
    if not SendExact(H, 5) then
    begin
      DoTLSLog('Failed to send ServerFinished');
      Exit;
    end;
    if not SendExact(Tmp[0], 8) then
    begin
      DoTLSLog('Failed to send ServerFinished nonce');
      Exit;
    end;
    if (Length(En) > 0) and (not SendExact(En[0], Length(En))) then
    begin
      DoTLSLog('Failed to send ServerFinished payload');
      Exit;
    end;
    if not SendExact(Tag[0], SizeOf(TCnGCM128Tag)) then
    begin
      DoTLSLog('Failed to send ServerFinished tag');
      Exit;
    end;
  end;
  ServerSeq := ServerSeq + 1;
  Result := True;
  DoTLSLog('TLS Handshake Success');

  ClientSocket.FHandshakeDone := True;
  ClientSocket.FServerWriteKey := ServerWriteKey;
  ClientSocket.FClientWriteKey := ClientWriteKey;
  ClientSocket.FServerFixedIV := ServerFixedIV;
  ClientSocket.FClientFixedIV := ClientFixedIV;
  ClientSocket.FServerSeq := ServerSeq;
  ClientSocket.FClientSeq := ClientSeq;
  ClientSocket.FCipherSuite := (Integer(SelCipherHi) shl 8) or Integer(SelCipherLo);
end;

function BuildAAD(ContentType: Byte; Version: Word; Length: Word; const Seq:
  TUInt64): TBytes;
var
  A, V: TBytes;
  L: Word;
begin
  V := TLSUInt64ToBE8(Seq);
  SetLength(A, 13);
  Move(V[0], A[0], 8);
  A[8] := ContentType;
  A[9] := Byte(Version shr 8);
  A[10] := Byte(Version and $FF);
  L := htons(Length);
  Move(L, A[11], 2);
  Result := A;
end;

function MakeLabel(const S: string): TBytes;
begin
  Result := AnsiToBytes(AnsiString(S));
end;

constructor TCnTLSServerClientSocket.Create;
begin
  inherited;
  FVersion := 0;
  FCipherSuite := 0;
  FHandshakeDone := False;
  FServerSeq := 0;
  FClientSeq := 0;
  FRecvPlainBuf := nil;
  FRecvPlainPos := 0;
end;

destructor TCnTLSServerClientSocket.Destroy;
begin

  inherited;
end;

function TCnTLSServerClientThread.DoGetClientSocket: TCnClientSocket;
begin
  Result := TCnTLSServerClientSocket.Create;
end;

procedure TCnTLSServerClientThread.Execute;
var
  Skt: TCnTLSServerClientSocket;
begin
  Skt := TCnTLSServerClientSocket(ClientSocket);
  if Assigned(Skt.Server) then
    FOwnerServer := TCnTLSServer(Skt.Server);
  if not FOwnerServer.DoHandShake(Skt) then
  begin
    if Assigned(FOwnerServer) and Assigned(FOwnerServer.FOnTLSError) then
      FOwnerServer.FOnTLSError(FOwnerServer, Skt, -1, 'Handshake failed');
    Skt.Shutdown;
    Exit;
  end;
  if Assigned(FOwnerServer) and Assigned(FOwnerServer.FOnTLSAccept) then
    FOwnerServer.FOnTLSAccept(FOwnerServer, Skt);
  Skt.Shutdown;
end;

procedure TCnTLSServer.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnTLSServerName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnTLSServerComment;
end;

constructor TCnTLSServer.Create(AOwner: TComponent);
begin
  inherited;
  FMaxFragmentLen := 16384;
  FPreferChaCha := True;
end;

destructor TCnTLSServer.Destroy;
begin

  inherited;
end;

function TCnTLSServer.DoGetClientThread: TCnTCPClientThread;
var
  T: TCnTLSServerClientThread;
begin
  T := TCnTLSServerClientThread.Create(True);
  Result := T;
end;

function TCnTLSServer.SendPlainData(ClientSocket: TCnClientSocket; const Data: TBytes): Boolean;
var
  Skt: TCnTLSServerClientSocket;
  UseChaCha: Boolean;
  En, AAD, Tmp: TBytes;
  H: TCnTLSRecordLayer;
  Tag: TCnGCM128Tag;
  BodyLen: Integer;
  Seq: TUInt64;
begin
  Result := False;
  if not (ClientSocket is TCnTLSServerClientSocket) then
    Exit;
  Skt := TCnTLSServerClientSocket(ClientSocket);
  if not Skt.HandshakeDone then
    Exit;
  UseChaCha := (Skt.CipherSuite = CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305) or
    (Skt.CipherSuite = CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305);
  Seq := Skt.ServerSeq;
  AAD := BuildAAD(CN_TLS_CONTENT_TYPE_APPLICATION_DATA, $0303, Length(Data), Seq);
  if UseChaCha then
  begin
    En := TLSChaCha20Poly1305Encrypt(Skt.ServerWriteKey, Skt.ServerFixedIV, Data, AAD, Seq, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    BodyLen := Length(En) + SizeOf(TCnGCM128Tag);
    H.BodyLength := htons(BodyLen);
    if Skt.Send(H, 5) = SOCKET_ERROR then Exit;
    if (Length(En) > 0) and (Skt.Send(En[0], Length(En)) = SOCKET_ERROR) then Exit;
    if Skt.Send(Tag[0], SizeOf(TCnGCM128Tag)) = SOCKET_ERROR then Exit;
  end
  else
  begin
    SetLength(Tmp, 8);
    Tmp[0] := Byte((Seq shr 56) and $FF);
    Tmp[1] := Byte((Seq shr 48) and $FF);
    Tmp[2] := Byte((Seq shr 40) and $FF);
    Tmp[3] := Byte((Seq shr 32) and $FF);
    Tmp[4] := Byte((Seq shr 24) and $FF);
    Tmp[5] := Byte((Seq shr 16) and $FF);
    Tmp[6] := Byte((Seq shr 8) and $FF);
    Tmp[7] := Byte(Seq and $FF);
    En := AES128GCMEncryptBytes(Skt.ServerWriteKey, ConcatBytes(Skt.ServerFixedIV, Tmp), Data, AAD, Tag);
    H.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
    H.MajorVersion := 3;
    H.MinorVersion := 3;
    BodyLen := 8 + Length(En) + SizeOf(TCnGCM128Tag);
    H.BodyLength := htons(BodyLen);
    if Skt.Send(H, 5) = SOCKET_ERROR then Exit;
    if Skt.Send(Tmp[0], 8) = SOCKET_ERROR then Exit;
    if (Length(En) > 0) and (Skt.Send(En[0], Length(En)) = SOCKET_ERROR) then Exit;
    if Skt.Send(Tag[0], SizeOf(TCnGCM128Tag)) = SOCKET_ERROR then Exit;
  end;
  Skt.ServerSeq := Seq + 1;
  Result := True;
end;

function TCnTLSServerClientSocket.Send(var Buf; Len: Integer; Flags: Integer): Integer;
var
  UseChaCha: Boolean;
  H: TCnTLSRecordLayer;
  En, AAD, Tmp, Data: TBytes;
  Tag: TCnGCM128Tag;
  BodyLen, Off, SegLen: Integer;
  Seq: TUInt64;
  OwnerSrv: TCnTLSServer;
begin
  if not FHandshakeDone then
  begin
    Result := inherited Send(Buf, Len, Flags);
    Exit;
  end;
  SetLength(Data, Len);
  if Len > 0 then
    Move(Buf, Data[0], Len);
  UseChaCha := (FCipherSuite = CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305) or
    (FCipherSuite = CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305);
  Off := 0;
  OwnerSrv := TCnTLSServer(Server);
  while Off < Len do
  begin
    SegLen := Len - Off;
    if (OwnerSrv <> nil) and (OwnerSrv.MaxFragmentLen > 0) and (SegLen > OwnerSrv.MaxFragmentLen) then
      SegLen := OwnerSrv.MaxFragmentLen;
    Seq := FServerSeq;
    AAD := BuildAAD(CN_TLS_CONTENT_TYPE_APPLICATION_DATA, $0303, SegLen, Seq);
    if UseChaCha then
    begin
      En := TLSChaCha20Poly1305Encrypt(FServerWriteKey, FServerFixedIV, Copy(Data, Off, SegLen), AAD, Seq, Tag);
      H.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
      H.MajorVersion := 3;
      H.MinorVersion := 3;
      BodyLen := Length(En) + SizeOf(TCnGCM128Tag);
      H.BodyLength := htons(BodyLen);
      if inherited Send(H, 5) = SOCKET_ERROR then
      begin Result := SOCKET_ERROR; Exit; end;
      if (Length(En) > 0) and (inherited Send(En[0], Length(En)) = SOCKET_ERROR) then
      begin Result := SOCKET_ERROR; Exit; end;
      if inherited Send(Tag[0], SizeOf(TCnGCM128Tag)) = SOCKET_ERROR then
      begin Result := SOCKET_ERROR; Exit; end;
    end
    else
    begin
      SetLength(Tmp, 8);
      Tmp[0] := Byte((Seq shr 56) and $FF);
      Tmp[1] := Byte((Seq shr 48) and $FF);
      Tmp[2] := Byte((Seq shr 40) and $FF);
      Tmp[3] := Byte((Seq shr 32) and $FF);
      Tmp[4] := Byte((Seq shr 24) and $FF);
      Tmp[5] := Byte((Seq shr 16) and $FF);
      Tmp[6] := Byte((Seq shr 8) and $FF);
      Tmp[7] := Byte(Seq and $FF);
      En := AES128GCMEncryptBytes(FServerWriteKey, ConcatBytes(FServerFixedIV, Tmp), Copy(Data, Off, SegLen), AAD, Tag);
      H.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
      H.MajorVersion := 3;
      H.MinorVersion := 3;
      BodyLen := 8 + Length(En) + SizeOf(TCnGCM128Tag);
      H.BodyLength := htons(BodyLen);
      if inherited Send(H, 5) = SOCKET_ERROR then
      begin Result := SOCKET_ERROR; Exit; end;
      if inherited Send(Tmp[0], 8) = SOCKET_ERROR then
      begin Result := SOCKET_ERROR; Exit; end;
      if (Length(En) > 0) and (inherited Send(En[0], Length(En)) = SOCKET_ERROR) then
      begin Result := SOCKET_ERROR; Exit; end;
      if inherited Send(Tag[0], SizeOf(TCnGCM128Tag)) = SOCKET_ERROR then
      begin Result := SOCKET_ERROR; Exit; end;
    end;
    FServerSeq := Seq + 1;
    Off := Off + SegLen;
  end;
  Result := Len;
end;

function TCnTLSServerClientSocket.Recv(var Buf; Len: Integer; Flags: Integer): Integer;
var
  UseChaCha: Boolean;
  H: TCnTLSRecordLayer;
  BodyLen, Cnt: Integer;
  En, AAD, Tmp, Plain, Raw: TBytes;
  Tag: TCnGCM128Tag;
  Seq: TUInt64;
begin
  if not FHandshakeDone then
  begin
    Result := inherited Recv(Buf, Len, Flags);
    Exit;
  end;
  if (FRecvPlainBuf <> nil) and (FRecvPlainPos < Length(FRecvPlainBuf)) then
  begin
    Cnt := Length(FRecvPlainBuf) - FRecvPlainPos;
    if Cnt > Len then Cnt := Len;
    Move(FRecvPlainBuf[FRecvPlainPos], Buf, Cnt);
    Inc(FRecvPlainPos, Cnt);
    if FRecvPlainPos >= Length(FRecvPlainBuf) then
    begin
      FRecvPlainBuf := nil;
      FRecvPlainPos := 0;
    end;
    Result := Cnt;
    Exit;
  end;
  SetLength(Raw, 5);
  if inherited Recv(Raw[0], 5, Flags) <> 5 then
  begin
    Result := SOCKET_ERROR;
    Exit;
  end;
  Move(Raw[0], H, 5);
  if H.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
  begin
    BodyLen := ntohs(H.BodyLength);
    if BodyLen <= 0 then
    begin Result := 0; Exit; end;
    SetLength(Raw, BodyLen);
    if inherited Recv(Raw[0], BodyLen, Flags) <> BodyLen then
    begin Result := SOCKET_ERROR; Exit; end;
    Result := 0;
    Exit;
  end
  else if H.ContentType <> CN_TLS_CONTENT_TYPE_APPLICATION_DATA then
  begin
    Result := 0;
    Exit;
  end;
  BodyLen := ntohs(H.BodyLength);
  if BodyLen <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  SetLength(Raw, BodyLen);
  if inherited Recv(Raw[0], BodyLen, Flags) <> BodyLen then
  begin
    Result := SOCKET_ERROR;
    Exit;
  end;
  UseChaCha := (FCipherSuite = CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305) or
    (FCipherSuite = CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305);
  Seq := FClientSeq;
  if UseChaCha then
  begin
    if BodyLen < SizeOf(TCnGCM128Tag) then
    begin Result := SOCKET_ERROR; Exit; end;
    SetLength(En, BodyLen - SizeOf(TCnGCM128Tag));
    if Length(En) > 0 then
      Move(Raw[0], En[0], Length(En));
    Move(Raw[BodyLen - SizeOf(TCnGCM128Tag)], Tag[0], SizeOf(TCnGCM128Tag));
    AAD := BuildAAD(CN_TLS_CONTENT_TYPE_APPLICATION_DATA, $0303, Length(En), Seq);
    Plain := TLSChaCha20Poly1305Decrypt(FClientWriteKey, FClientFixedIV, En, AAD, Seq, Tag);
  end
  else
  begin
    if BodyLen < 8 + SizeOf(TCnGCM128Tag) then
    begin Result := SOCKET_ERROR; Exit; end;
    SetLength(Tmp, 8);
    Move(Raw[0], Tmp[0], 8);
    SetLength(En, BodyLen - 8 - SizeOf(TCnGCM128Tag));
    if Length(En) > 0 then
      Move(Raw[8], En[0], Length(En));
    Move(Raw[BodyLen - SizeOf(TCnGCM128Tag)], Tag[0], SizeOf(TCnGCM128Tag));
    AAD := BuildAAD(CN_TLS_CONTENT_TYPE_APPLICATION_DATA, $0303, Length(En), Seq);
    Plain := AES128GCMDecryptBytes(FClientWriteKey, ConcatBytes(FClientFixedIV, Tmp), En, AAD, Tag);
  end;
  if Plain = nil then
  begin
    Result := SOCKET_ERROR;
    Exit;
  end;
  FClientSeq := Seq + 1;
  if Length(Plain) <= Len then
  begin
    if Length(Plain) > 0 then
      Move(Plain[0], Buf, Length(Plain));
    Result := Length(Plain);
  end
  else
  begin
    Move(Plain[0], Buf, Len);
    FRecvPlainBuf := Plain;
    FRecvPlainPos := Len;
    Result := Len;
  end;
end;

function TLSChaChaNonce(const FixedIV12: TBytes; Seq: TUInt64): TBytes;
var
  N: TBytes;
  S: array[0..7] of Byte;
  I: Integer;
begin
  SetLength(N, 12);
  for I := 0 to 7 do
    S[7 - I] := Byte((Seq shr (I * 8)) and $FF);
  for I := 0 to 3 do
    N[I] := FixedIV12[I];
  for I := 0 to 7 do
    N[4 + I] := FixedIV12[4 + I] xor S[I];
  Result := N;
end;

function TLSPoly1305Tag(AAD, C: TBytes; PolyKey: TBytes): TCnGCM128Tag;
var
  M: TBytes;
  PadLen: Integer;
  D: TCnPoly1305Digest;
  Ls: array[0..15] of Byte;
  AL, CL: TUInt64;
begin
  M := nil;
  if AAD <> nil then
  begin
    M := ConcatBytes(M, AAD);
    PadLen := (16 - (Length(AAD) mod 16)) mod 16;
    if PadLen > 0 then
      M := ConcatBytes(M, NewZeroBytes(PadLen));
  end;
  if C <> nil then
  begin
    M := ConcatBytes(M, C);
    PadLen := (16 - (Length(C) mod 16)) mod 16;
    if PadLen > 0 then
      M := ConcatBytes(M, NewZeroBytes(PadLen));
  end;
  FillChar(Ls[0], SizeOf(Ls), 0);
  AL := Length(AAD);
  CL := Length(C);
  Move(AL, Ls[0], SizeOf(TUInt64));
  Move(CL, Ls[8], SizeOf(TUInt64));
  M := ConcatBytes(M, NewBytesFromMemory(@Ls[0], SizeOf(Ls)));
  D := Poly1305Bytes(M, PolyKey);
  Move(D[0], Result[0], SizeOf(Result));
end;

function TLSChaCha20Poly1305Encrypt(Key, FixedIV12, Plain, AAD: TBytes; Seq: TUInt64; out Tag: TCnGCM128Tag): TBytes;
var
  K: TCnChaChaKey;
  N: TCnChaChaNonce;
  Nonce12, OTK, KS: TBytes;
  Stream: TCnChaChaState;
  I, J: Integer;
begin
  Result := nil;
  Nonce12 := TLSChaChaNonce(FixedIV12, Seq);
  Move(Key[0], K[0], SizeOf(TCnChaChaKey));
  Move(Nonce12[0], N[0], SizeOf(TCnChaChaNonce));
  ChaCha20Block(K, N, 0, Stream);
  SetLength(KS, 64);
  for I := 0 to 15 do
    for J := 0 to 3 do
      KS[I * 4 + J] := Byte((Stream[I] shr (J * 8)) and $FF);
  OTK := Copy(KS, 0, 32);
  SetLength(Result, Length(Plain));
  if Length(Plain) > 0 then
    Result := ChaCha20EncryptBytes(K, N, Plain);
  Tag := TLSPoly1305Tag(AAD, Result, OTK);
end;

function TLSChaCha20Poly1305Decrypt(Key, FixedIV12, En, AAD: TBytes; Seq: TUInt64; InTag: TCnGCM128Tag): TBytes;
var
  K: TCnChaChaKey;
  N: TCnChaChaNonce;
  Nonce12, OTK, KS: TBytes;
  Stream: TCnChaChaState;
  I, J: Integer;
  CTag: TCnGCM128Tag;
begin
  Result := nil;
  Nonce12 := TLSChaChaNonce(FixedIV12, Seq);
  Move(Key[0], K[0], SizeOf(TCnChaChaKey));
  Move(Nonce12[0], N[0], SizeOf(TCnChaChaNonce));
  ChaCha20Block(K, N, 0, Stream);
  SetLength(KS, 64);
  for I := 0 to 15 do
    for J := 0 to 3 do
      KS[I * 4 + J] := Byte((Stream[I] shr (J * 8)) and $FF);
  OTK := Copy(KS, 0, 32);
  CTag := TLSPoly1305Tag(AAD, En, OTK);
  if not CompareMem(@CTag[0], @InTag[0], SizeOf(TCnGCM128Tag)) then
    Exit;
  SetLength(Result, Length(En));
  if Length(En) > 0 then
    Result := ChaCha20DecryptBytes(K, N, En);
end;

function TLSEccDigestBytes(Data: TBytes; DigestType: TCnEccSignDigestType): TBytes;
var
  MD5Dig: TCnMD5Digest;
  SHA1Dig: TCnSHA1Digest;
  SHA256Dig: TCnSHA256Digest;
  SM3Dig: TCnSM3Digest;
  SHA384Dig: TCnSHA384Digest;
  SHA512Dig: TCnSHA512Digest;
begin
  Result := nil;
  case DigestType of
    esdtMD5:
      begin
        MD5Dig := MD5Bytes(Data);
        Result := NewBytesFromMemory(@MD5Dig[0], SizeOf(TCnMD5Digest));
      end;
    esdtSHA1:
      begin
        SHA1Dig := SHA1Bytes(Data);
        Result := NewBytesFromMemory(@SHA1Dig[0], SizeOf(TCnSHA1Digest));
      end;
    esdtSHA256:
      begin
        SHA256Dig := SHA256Bytes(Data);
        Result := NewBytesFromMemory(@SHA256Dig[0], SizeOf(TCnSHA256Digest));
      end;
    esdtSM3:
      begin
        SM3Dig := SM3Bytes(Data);
        Result := NewBytesFromMemory(@SM3Dig[0], SizeOf(TCnSM3Digest));
      end;
    esdtSHA384:
      begin
        SHA384Dig := SHA384Bytes(Data);
        Result := NewBytesFromMemory(@SHA384Dig[0], SizeOf(TCnSHA384Digest));
      end;
    esdtSHA512:
      begin
        SHA512Dig := SHA512Bytes(Data);
        Result := NewBytesFromMemory(@SHA512Dig[0], SizeOf(TCnSHA512Digest));
      end;
  end;
end;

function TLSEccHMacBytes(Key: TBytes; Data: TBytes; DigestType:
  TCnEccSignDigestType): TBytes;
var
  MD5Dig: TCnMD5Digest;
  SHA1Dig: TCnSHA1Digest;
  SHA256Dig: TCnSHA256Digest;
  SM3Dig: TCnSM3Digest;
  SHA384Dig: TCnSHA384Digest;
  SHA512Dig: TCnSHA512Digest;
begin
  Result := nil;
  case DigestType of
    esdtMD5:
      begin
        MD5Dig := MD5HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@MD5Dig[0], SizeOf(TCnMD5Digest));
      end;
    esdtSHA1:
      begin
        SHA1Dig := SHA1HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA1Dig[0], SizeOf(TCnSHA1Digest));
      end;
    esdtSHA256:
      begin
        SHA256Dig := SHA256HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA256Dig[0], SizeOf(TCnSHA256Digest));
      end;
    esdtSM3:
      begin
        SM3Dig := SM3HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SM3Dig[0], SizeOf(TCnSM3Digest));
      end;
    esdtSHA384:
      begin
        SHA384Dig := SHA384HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA384Dig[0], SizeOf(TCnSHA384Digest));
      end;
    esdtSHA512:
      begin
        SHA512Dig := SHA512HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA512Dig[0], SizeOf(TCnSHA512Digest));
      end;
  end;
end;

function TLSPseudoRandomFunc(Secret: TBytes; const PLabel: AnsiString; Seed: TBytes;
  DigestType: TCnEccSignDigestType; NeedLength: Integer): TBytes;
var
  Data, Res, A: TBytes;
begin
  Data := ConcatBytes(AnsiToBytes(PLabel), Seed);
  A := TLSEccHMacBytes(Secret, Data, DigestType);
  Res := nil;
  repeat
    Res := ConcatBytes(Res, TLSEccHMacBytes(Secret, ConcatBytes(A, Data), DigestType));
    A := TLSEccHMacBytes(Secret, A, DigestType);
  until Length(Res) >= NeedLength;
  Result := Copy(Res, 0, NeedLength);
end;

function TLSExtractEccCurveDigest(SigAlg: Word; out CurveType: TCnEccCurveType; out
  DigestType: TCnEccSignDigestType): Boolean;
begin
  Result := True;
  case SigAlg of
    CN_TLS_SIGN_ALG_ECDSA_SECP256R1_SHA256:
      begin
        CurveType := ctSecp256r1;
        DigestType := esdtSHA256;
      end;
    CN_TLS_SIGN_ALG_ECDSA_SECP384R1_SHA384:
      begin
        CurveType := ctSecp384r1;
        DigestType := esdtSHA384;
      end;
    CN_TLS_SIGN_ALG_ECDSA_SECP521R1_SHA512:
      begin
        CurveType := ctSecp521r1;
        DigestType := esdtSHA512;
      end;
  else
    Result := False;
  end;
end;

{ TCnTLSClient }

constructor TCnTLSClient.Create(AOwner: TComponent);
begin
  inherited;
  FVerifyCertificate := True;
  FEnableExtendedMasterSecret := True;
  FHandshakeState := hsInit;
  FClientSeq := 0;
  FServerSeq := 0;
  FRecvPlainBuf := nil;
  FRecvPlainPos := 0;
  FMaxFragmentLen := 16384;
end;

destructor TCnTLSClient.Destroy;
begin

  inherited;
end;

procedure TCnTLSClient.DoTLSLog(const Msg: string);
begin
  if Assigned(FOnTLSLog) then
    FOnTLSLog(Self, Msg);
end;

procedure TCnTLSClient.DoConnect;
begin
  inherited DoConnect;
  
  DoTLSLog('Starting TLS 1.2 Handshake...');
  FHandshakeState := hsInit;
  if not DoHandShake(RemoteHost, RemotePort) then
    Close;
end;

function TCnTLSClient.BuildAppDataAAD(Seq: TUInt64; PlainLen: Integer): TBytes;
var
  AADFix: array[0..12] of Byte;
  SeqBytes: array[0..7] of Byte;
  I: Integer;
begin
  FillChar(AADFix[0], SizeOf(AADFix), 0);
  for I := 0 to 7 do
    SeqBytes[7 - I] := Byte((Seq shr (I * 8)) and $FF);
  Move(SeqBytes[0], AADFix[0], 8);
  AADFix[8] := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
  AADFix[9] := 3;
  AADFix[10] := 3;
  AADFix[11] := Byte(PlainLen shr 8);
  AADFix[12] := Byte(PlainLen and $FF);
  Result := NewBytesFromMemory(@AADFix[0], SizeOf(AADFix));
end;

function TCnTLSClient.EncryptAppDataBody(const Plain, AAD, Nonce: TBytes;
  out Tag: TCnGCM128Tag): TBytes;
var
  Stream: TCnChaChaState;
  KS, OTK: TBytes;
  I, J: Integer;
  Body: TBytes;
  ExplicitNonce: TBytes;
  PadLen: Integer;
  M: TBytes;
  PD: TCnPoly1305Digest;
  AL, CL: TUInt64;
  Ls: array[0..15] of Byte;
begin
  Result := nil;
  if FCipherIsChaCha20Poly1305 then
  begin
    ChaCha20Block(TCnChaChaKey(Pointer(@FClientWriteKey[0])^), TCnChaChaNonce(Pointer(@Nonce[0])^), 0, Stream);
    SetLength(KS, 64);
    for I := 0 to 15 do
      for J := 0 to 3 do
        KS[I * 4 + J] := Byte((Stream[I] shr (J * 8)) and $FF);
    OTK := Copy(KS, 0, 32);
    Body := ChaCha20EncryptBytes(TCnChaChaKey(Pointer(@FClientWriteKey[0])^), TCnChaChaNonce(Pointer(@Nonce[0])^), Plain);
    M := nil;
    if AAD <> nil then
    begin
      M := ConcatBytes(M, AAD);
      PadLen := (16 - (Length(AAD) mod 16)) mod 16;
      if PadLen > 0 then
        M := ConcatBytes(M, NewZeroBytes(PadLen));
    end;
    if Body <> nil then
    begin
      M := ConcatBytes(M, Body);
      PadLen := (16 - (Length(Body) mod 16)) mod 16;
      if PadLen > 0 then
        M := ConcatBytes(M, NewZeroBytes(PadLen));
    end;
    FillChar(Ls[0], SizeOf(Ls), 0);
    AL := Length(AAD);
    CL := Length(Body);
    Move(AL, Ls[0], SizeOf(TUInt64));
    Move(CL, Ls[8], SizeOf(TUInt64));
    M := ConcatBytes(M, NewBytesFromMemory(@Ls[0], SizeOf(Ls)));
    PD := Poly1305Bytes(M, OTK);
    Move(PD[0], Tag[0], SizeOf(Tag));
    Result := ConcatBytes(Body, NewBytesFromMemory(@Tag, SizeOf(Tag)));
  end
  else
  begin
    if FCipherIsSM4GCM then
      Body := SM4GCMEncryptBytes(FClientWriteKey, Nonce, Plain, AAD, Tag)
    else if FKeyLen = 16 then
      Body := AES128GCMEncryptBytes(FClientWriteKey, Nonce, Plain, AAD, Tag)
    else
      Body := AES256GCMEncryptBytes(FClientWriteKey, Nonce, Plain, AAD, Tag);
    ExplicitNonce := Copy(Nonce, Length(Nonce) - 8, 8);
    Result := ConcatBytes(ExplicitNonce, Body, NewBytesFromMemory(@Tag, SizeOf(Tag)));
  end;
end;

function TCnTLSClient.DecryptAppDataBody(const Body, AAD: TBytes; const Nonce: TBytes; var InTag: TCnGCM128Tag): TBytes;
var
  Stream: TCnChaChaState;
  KS, OTK: TBytes;
  I, J: Integer;
  Cipher: TBytes;
  CTag: TCnGCM128Tag;
  M: TBytes;
  PD: TCnPoly1305Digest;
  PadLen: Integer;
  AL, CL: TUInt64;
  Ls: array[0..15] of Byte;
begin
  Result := nil;
  if FCipherIsChaCha20Poly1305 then
  begin
    ChaCha20Block(TCnChaChaKey(Pointer(@FServerWriteKey[0])^), TCnChaChaNonce(Pointer(@Nonce[0])^), 0, Stream);
    SetLength(KS, 64);
    for I := 0 to 15 do
      for J := 0 to 3 do
        KS[I * 4 + J] := Byte((Stream[I] shr (J * 8)) and $FF);
    OTK := Copy(KS, 0, 32);
    if Length(Body) < SizeOf(TCnGCM128Tag) then Exit;
    Cipher := Copy(Body, 0, Length(Body) - SizeOf(TCnGCM128Tag));
    Move(Body[Length(Cipher)], CTag[0], SizeOf(CTag));
    M := nil;
    if AAD <> nil then
    begin
      M := ConcatBytes(M, AAD);
      PadLen := (16 - (Length(AAD) mod 16)) mod 16;
      if PadLen > 0 then
        M := ConcatBytes(M, NewZeroBytes(PadLen));
    end;
    if Cipher <> nil then
    begin
      M := ConcatBytes(M, Cipher);
      PadLen := (16 - (Length(Cipher) mod 16)) mod 16;
      if PadLen > 0 then
        M := ConcatBytes(M, NewZeroBytes(PadLen));
    end;
    FillChar(Ls[0], SizeOf(Ls), 0);
    AL := Length(AAD);
    CL := Length(Cipher);
    Move(AL, Ls[0], SizeOf(TUInt64));
    Move(CL, Ls[8], SizeOf(TUInt64));
    M := ConcatBytes(M, NewBytesFromMemory(@Ls[0], SizeOf(Ls)));
    PD := Poly1305Bytes(M, OTK);
    if not CompareMem(@PD[0], @CTag[0], SizeOf(CTag)) then
      Exit;
    Result := ChaCha20DecryptBytes(TCnChaChaKey(Pointer(@FServerWriteKey[0])^), TCnChaChaNonce(Pointer(@Nonce[0])^), Cipher);
  end
  else
  begin
    DoTLSLog('DecryptAppDataBody FKeyLen: ' + IntToStr(FKeyLen));
    DoTLSLog('DecryptAppDataBody FCipherIsSM4GCM: ' + BoolToStr(FCipherIsSM4GCM, True));
    DoTLSLog('DecryptAppDataBody Nonce Len: ' + IntToStr(Length(Nonce)) + ' Nonce: ' + BytesToHex(Nonce));
    DoTLSLog('DecryptAppDataBody AAD Len: ' + IntToStr(Length(AAD)) + ' AAD: ' + BytesToHex(AAD));
    DoTLSLog('DecryptAppDataBody Body Len: ' + IntToStr(Length(Body)));
    DoTLSLog('DecryptAppDataBody FServerWriteKey Len: ' + IntToStr(Length(FServerWriteKey)));
    if FCipherIsSM4GCM then
      Result := SM4GCMDecryptBytes(FServerWriteKey, Nonce, Body, AAD, InTag)
    else if FKeyLen = 16 then
      Result := AES128GCMDecryptBytes(FServerWriteKey, Nonce, Body, AAD, InTag)
    else
      Result := AES256GCMDecryptBytes(FServerWriteKey, Nonce, Body, AAD, InTag);
    DoTLSLog('DecryptAppDataBody Result Len: ' + IntToStr(Length(Result)));
  end;
end;

function TCnTLSClient.EncryptAndSendAppData(const Plain: TBytes; Flags: Integer): Integer;
var
  AAD, Nonce, Body: TBytes;
  Tag: TCnGCM128Tag;
  H: PCnTLSRecordLayer;
  SendBuf: array[0..8191] of Byte;
  SeqBE: array[0..7] of Byte;
  I: Integer;
begin
  AAD := BuildAppDataAAD(FClientSeq, Length(Plain));
  if FCipherIsChaCha20Poly1305 then
    Nonce := TLSChaChaNonce(FClientFixedIV, FClientSeq)
  else
  begin
    for I := 0 to 7 do
      SeqBE[7 - I] := Byte((FClientSeq shr (I * 8)) and $FF);
    Nonce := ConcatBytes(FClientFixedIV, NewBytesFromMemory(@SeqBE[0], 8));
  end;
  Body := EncryptAppDataBody(Plain, AAD, Nonce, Tag);
  FillChar(SendBuf, SizeOf(SendBuf), 0);
  H := PCnTLSRecordLayer(@SendBuf[0]);
  H^.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
  H^.MajorVersion := 3;
  H^.MinorVersion := 3;
  CnSetTLSRecordLayerBodyLength(H, Length(Body));
  Move(Body[0], H^.Body[0], Length(Body));

  Result := inherited Send(H^, 5 + CnGetTLSRecordLayerBodyLength(H), Flags);
  if Result <> SOCKET_ERROR then
    Inc(FClientSeq);
end;

function TCnTLSClient.ReadAndDecryptAppData(var Buf; Len: Integer; Flags: Integer): Integer;
var
  Buffer: PByteArray;
  BytesReceived, TotalConsumed, RecBodyLen: Integer;
  H: PCnTLSRecordLayer;
  AAD, Nonce, Body: TBytes;
  Tag: TCnGCM128Tag;
  Explicit, Plain: TBytes;
begin
  Result := SOCKET_ERROR;
  BytesReceived := inherited Recv(Buf, Len, Flags);

  DoTLSLog('ReadAndDecryptAppData BytesReceived: ' + IntToStr(BytesReceived));
  if BytesReceived <= SizeOf(TCnTLSRecordLayer) then Exit;
  TotalConsumed := 0;
  Buffer := PByteArray(@Buf);
  while TotalConsumed < BytesReceived do
  begin
    H := PCnTLSRecordLayer(@(Buffer^[TotalConsumed]));
    RecBodyLen := CnGetTLSRecordLayerBodyLength(H);
    DoTLSLog('ReadAndDecryptAppData ContentType: ' + IntToStr(H^.ContentType) + ' RecBodyLen: ' + IntToStr(RecBodyLen));
    if H^.ContentType = CN_TLS_CONTENT_TYPE_APPLICATION_DATA then
    begin
      DoTLSLog('ServerSeq used for AAD: ' + IntToStr(FServerSeq));
      if FCipherIsChaCha20Poly1305 then
      begin
        SetLength(Body, RecBodyLen);
        Move(H^.Body[0], Body[0], RecBodyLen);
        AAD := BuildAppDataAAD(FServerSeq, RecBodyLen - SizeOf(TCnGCM128Tag));
        Nonce := TLSChaChaNonce(FServerFixedIV, FServerSeq);
        Move(Body[RecBodyLen - SizeOf(TCnGCM128Tag)], Tag, SizeOf(Tag));
        Plain := DecryptAppDataBody(Body, AAD, Nonce, Tag);
      end
      else
      begin
        Explicit := NewBytesFromMemory(@H^.Body[0], 8);
        SetLength(Body, RecBodyLen - 8 - SizeOf(TCnGCM128Tag));
        Move((PAnsiChar(@H^.Body[0]) + 8)^, Body[0], Length(Body));
        Move((PAnsiChar(@H^.Body[0]) + 8 + Length(Body))^, Tag, SizeOf(Tag));
        AAD := BuildAppDataAAD(FServerSeq, Length(Body));
        Nonce := ConcatBytes(FServerFixedIV, Explicit);
        DoTLSLog('Server AppData ExplicitNonce: ' + BytesToHex(Explicit));
        DoTLSLog('Server AppData AAD: ' + BytesToHex(AAD));
        DoTLSLog('Server AppData EnCipher Len: ' + IntToStr(Length(Body)));
        DoTLSLog('Server AppData Tag: ' + BytesToHex(NewBytesFromMemory(@Tag, SizeOf(Tag))));
        Plain := DecryptAppDataBody(Body, AAD, Nonce, Tag);
      end;

      DoTLSLog('ReadAndDecryptAppData Plain Len: ' + IntToStr(Length(Plain)));
      Result := Length(Plain);
      if Result > 0 then
      begin
        Move(Plain[0], Buf, Result);
        Inc(FServerSeq);
      end;

      Exit;
    end;
    Inc(TotalConsumed, 5 + RecBodyLen);
  end;
end;

function TCnTLSClient.Send(var Buf; Len: Integer; Flags: Integer): Integer;
var
  Plain: TBytes;
begin
  if FHandshakeState <> hsConnected then
  begin
    Result := inherited Send(Buf, Len, Flags);
    Exit;
  end;

  SetLength(Plain, Len);
  Move(Buf, Plain[0], Len);

  Result := EncryptAndSendAppData(Plain, Flags);
  if Result > 0 then
  begin
    Result := Len;
    Inc(FClientSeq);
  end;
end;

function TCnTLSClient.Recv(var Buf; Len: Integer; Flags: Integer): Integer;
var
  Buffer: TBytes;
  Body: TBytes;
  AAD: TBytes;
  Nonce: TBytes;
  Tag: TCnGCM128Tag;
  Explicit: TBytes;
  SeqBytes: array[0..7] of Byte;
begin
  if FHandshakeState <> hsConnected then
  begin
    Result := inherited Recv(Buf, Len, Flags);
    Exit;
  end;

  Result := ReadAndDecryptAppData(Buf, Len, Flags);
end;

procedure TCnTLSClient.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnTLSClientName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnTLSClientComment;
end;

function TCnTLSClient.DoHandShake(const Host: AnsiString;
  Port: Word): Boolean;
var
  Buffer: array[0..8191] of Byte;
  H: PCnTLSRecordLayer;
  B: PCnTLSHandShakeHeader;
  C: PCnTLSHandShakeClientHello;
  SNI: PCnTLSHandShakeServerNameIndication;
  E: PCnTLSHandShakeExtensions;
  EI: PCnTLSHandShakeExtensionItem;
  S: PCnTLSHandShakeServerHello;
  Cer: PCnTLSHandShakeCertificate;
  CI: PCnTLSHandShakeCertificateItem;
  SK: PCnTLSHandShakeServerKeyExchange;
  SP: PCnTLSHandShakeSignedParams;
  CK: PCnTLSHandShakeClientKeyExchange;
  CC: PCnTLSChangeCipherSpecPacket;
  F: PCnTLSHandShakeFinished;
  ServerHelloExt: PCnTLSHandShakeExtensions;
  ExtItem2: PCnTLSHandShakeExtensionItem;
  RandClient, RandServer: TBytes;
  SessionId: TBytes;
  CompressionMethod: TBytes;
  Ciphers: TWords;
  Rs: TCnFDSet;
  TotalHandShake: TBytes;
  BytesReceived: Integer;
  TotalConsumed: Integer;
  CurrentPtr: PByte;
  CurveType: TCnEccCurveType;
  DigestType: TCnEccSignDigestType;
  ServerCertBytes: TBytes;
  ServerKeyBytes: TBytes;
  Ecc: TCnEcc;
  EccPrivKey: TCnEccPrivateKey;
  EccPubKey: TCnEccPublicKey;
  PreMasterKey: TCnEccPublicKey;
  PreMasterX: TCnBigNumber;
  PreMasterBytes: TBytes;
  MasterKey: TBytes;
  SelectedCipher: Word;
  ClientWriteKey, ServerWriteKey: TBytes;
  ClientFixedIV, ServerFixedIV: TBytes;
  KeyBlock: TBytes;
  KeyLen, IvLen: Integer;
  ClientSeq, ServerSeq: TUInt64;
  CipherIsSM4GCM: Boolean;
  CipherIsChaCha20Poly1305: Boolean;
  VerifyData: TBytes;
  PlainFinished: TBytes;
  AAD: TBytes;
  ExplicitNonce: TBytes;
  EnCipher: TBytes;
  Tag: TCnGCM128Tag;
  ServerFinTag: TCnGCM128Tag;
  ServerEn: TBytes;
  ServerPlain: TBytes;
  RecBodyLen: Integer;
  Ok: Boolean;
  IpStr: string;
  ExtConsumed: Integer;
  ExtTotalLen: Integer;
  PBody: PByte;
{$IFNDEF MSWINDOWS}
  HE: THostEntry;
{$ENDIF}
  AADFix: array[0..12] of Byte;
  AADFix2: array[0..12] of Byte;
  SeqBytes: array[0..7] of Byte;
  I: Integer;
  Off: Integer;
  L3: Integer;
  EmsNegotiated: Boolean;
  Lvl, Desc: Integer;
  ClientRecordCount, ServerRecordCount: TUInt64;
  Req: TBytes;
  IVNonce: TBytes;
  TmpStr: AnsiString;
  GotSH, GotCert, GotSKE, GotSHD: Boolean;
  function SendAll(const Buf; Len: Integer): Boolean;
  var Sent, Cnt: Integer; P: PAnsiChar;
  begin
    Result := False;
    P := @Buf;
    Sent := 0;
    while Sent < Len do
    begin
      Cnt := inherited Send(P[Sent], Len - Sent, 0);
      if Cnt = SOCKET_ERROR then
        Exit;
      if Cnt = 0 then
        Exit;
      Inc(Sent, Cnt);
    end;
    Result := True;
  end;
  function RecvExact(var OutBuf: TBytes; Need: Integer): Boolean;
  var Got, Cnt: Integer;
    Tv: record
      tv_sec: Longint;
      tv_usec: Longint;
    end;
  begin
    Result := False;
    SetLength(OutBuf, Need);
    Got := 0;
    while Got < Need do
    begin
      CnFDZero(Rs);
      CnFDSet(Socket, Rs);
      Tv.tv_sec := 10;
      Tv.tv_usec := 0;
      if CnSelect(Socket + 1, @Rs, nil, nil, @Tv) <= 0 then
        Exit;
      Cnt := inherited Recv(OutBuf[Got], Need - Got, 0);
      if Cnt = SOCKET_ERROR then
        Exit;
      if Cnt = 0 then
        Exit;
      Inc(Got, Cnt);
    end;
    Result := True;
  end;
  function RecvOneRecord: TBytes;
  var Hdr: TBytes; Body: TBytes; BL: Integer;
  begin
    Result := nil;
    Hdr := nil; Body := nil;
    if not RecvExact(Hdr, 5) then Exit;
    BL := (Integer(Hdr[3]) shl 8) or Integer(Hdr[4]);
    if BL < 0 then Exit;
    if not RecvExact(Body, BL) then Exit;
    SetLength(Result, 5 + BL);
    Move(Hdr[0], Result[0], 5);
    if BL > 0 then
      Move(Body[0], Result[5], BL);
  end;
begin
  Result := False;
  EmsNegotiated := False;

  ClientSeq := 0;
  ServerSeq := 0;
  ClientRecordCount := 0;
  ServerRecordCount := 0;
  FillChar(Buffer, SizeOf(Buffer), 0);

  // 发包①：ClientHello（握手记录，内容含：版本、Random、Session、CipherSuites、扩展SNI/ALPN/曲线/签名/EMS）
  H := PCnTLSRecordLayer(@Buffer[0]);
  H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
  H^.MajorVersion := 3;
  H^.MinorVersion := 3;
  B := PCnTLSHandShakeHeader(@H^.Body[0]);
  B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CLIENT_HELLO;
  C := PCnTLSHandShakeClientHello(@B^.Content[0]);
  C^.ProtocolVersion := CN_TLS_SSL_VERSION_TLS_12;
  SetLength(RandClient, SizeOf(C^.Random));
  CnRandomFillBytes2(@RandClient[0], Length(RandClient));
  Move(RandClient[0], C^.Random[0], SizeOf(C^.Random));
  SetLength(SessionId, 32);
  CnRandomFillBytes2(@SessionId[0], 32);
  CnSetTLSHandShakeClientHelloSessionId(C, SessionId);
  SetLength(Ciphers, 8);
  Ciphers[0] := CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305;
  Ciphers[1] := CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305;
  Ciphers[2] := CN_CIPHER_ECDHE_RSA_AES128_GCM_SHA256;
  Ciphers[3] := CN_CIPHER_ECDHE_ECDSA_AES128_GCM_SHA256;
  Ciphers[4] := CN_CIPHER_ECDHE_RSA_AES256_GCM_SHA384;
  Ciphers[5] := CN_CIPHER_ECDHE_ECDSA_AES256_GCM_SHA384;
  Ciphers[6] := CN_CIPHER_AES128_GCM_SHA256;
  Ciphers[7] := CN_CIPHER_AES256_GCM_SHA384;
  CnSetTLSHandShakeClientHelloCipherSuites(C, Ciphers);
  SetLength(CompressionMethod, 1);
  CompressionMethod[0] := 0;
  CnSetTLSHandShakeClientHelloCompressionMethod(C, CompressionMethod);
  E := PCnTLSHandShakeExtensions(PAnsiChar(C) + SizeOf(Word) + 32 + 1 + C^.SessionLength
    + SizeOf(Word) + CnGetTLSHandShakeClientHelloCipherSuitesLength(C) + 1 +
    CnGetTLSHandShakeClientHelloCompressionMethodLength(C));
  EI := CnGetTLSHandShakeExtensionsExtensionItem(E);
  CnSetTLSHandShakeExtensionsExtensionType(EI, CN_TLS_EXTENSIONTYPE_SERVER_NAME);
  SNI := CnGetTLSHandShakeExtensionsExtensionData(EI);
  CnSetTLSHandShakeExtensionsExtensionDataLength(EI,
    CnTLSHandShakeServerNameIndicationAddHost(SNI, Host));
  EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
  SetLength(Ciphers, 4);
  Ciphers[0] := CN_TLS_NAMED_GROUP_X25519;
  Ciphers[1] := CN_TLS_NAMED_GROUP_SECP256R1;
  Ciphers[2] := CN_TLS_NAMED_GROUP_SECP384R1;
  Ciphers[3] := CN_TLS_NAMED_GROUP_SECP521R1;
  CnSetTLSHandShakeSupportedGroups(EI, Ciphers);
  EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
  CnSetTLSHandShakeECPointFormats(EI, CN_TLS_EC_POINT_FORMATS_UNCOMPRESSED);
  EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
  SetLength(Ciphers, 6);
  Ciphers[0] := CN_TLS_SIGN_ALG_ECDSA_SECP256R1_SHA256;
  Ciphers[1] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA256;
  Ciphers[2] := CN_TLS_SIGN_ALG_ECDSA_SECP384R1_SHA384;
  Ciphers[3] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA384;
  Ciphers[4] := CN_TLS_SIGN_ALG_ECDSA_SECP521R1_SHA512;
  Ciphers[5] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA512;
  CnSetTLSHandShakeSignatureAlgorithms(EI, Ciphers);
  EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
  CnSetTLSHandShakeExtensionsExtensionType(EI,
    CN_TLS_EXTENSIONTYPE_EXTENDED_MASTER_SECRET);
  CnSetTLSHandShakeExtensionsExtensionDataLength(EI, 0);
  EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
  CnSetTLSHandShakeExtensionsExtensionType(EI,
    CN_TLS_EXTENSIONTYPE_APPLICATION_LAYER_PROTOCOL_NEGOTIATION);
  SetLength(Ciphers, 0);

  // 构造 ALPN: ProtocolNameList = 2-byte length + entries
  // entries: 'h2' and 'http/1.1'
  // 总 entries length = (1+2) + (1+8) = 12
  SetLength(CompressionMethod, 2 + (1 + 2) + (1 + 8));
  CompressionMethod[0] := 0;
  CompressionMethod[1] := 12;
  CompressionMethod[2] := 2;
  CompressionMethod[3] := Ord('h');
  CompressionMethod[4] := Ord('2');
  CompressionMethod[5] := 8;
  CompressionMethod[6] := Ord('h');
  CompressionMethod[7] := Ord('t');
  CompressionMethod[8] := Ord('t');
  CompressionMethod[9] := Ord('p');
  CompressionMethod[10] := Ord('/');
  CompressionMethod[11] := Ord('1');
  CompressionMethod[12] := Ord('.');
  CompressionMethod[13] := Ord('1');
  CnSetTLSHandShakeExtensionsExtensionData(EI, CompressionMethod);
  CnSetTLSHandShakeExtensionsExtensionLengthByItemCount(E, 6);
  CnSetTLSHandShakeHeaderContentLength(B,
    SizeOf(Word) + 32 + 1 + C^.SessionLength + SizeOf(Word) + CnGetTLSHandShakeClientHelloCipherSuitesLength(C) + 1 + CnGetTLSHandShakeClientHelloCompressionMethodLength(C) + SizeOf(Word) + CnGetTLSHandShakeExtensionsExtensionLength(E));
  CnSetTLSRecordLayerBodyLength(H, 1 + 3 + CnGetTLSHandShakeHeaderContentLength(B));
  if not SendAll(H^, 5 + CnGetTLSRecordLayerBodyLength(H)) then
  begin
    DoTLSLog('Send ClientHello failed');
    Exit;
  end;
  DoTLSLog('Sent ClientHello, Size: ' + IntToStr(5 + CnGetTLSRecordLayerBodyLength(H)));
  Inc(ClientSeq);
  Inc(ClientRecordCount);
  Inc(ClientSeq);
  TotalHandShake := NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H));
  DoTLSLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
    CnGetTLSRecordLayerBodyLength(H)]));
  DoTLSLog('Waiting server initial response...');
  GotSH := False;
  GotCert := False;
  GotSKE := False;
  GotSHD := False;

  // 按记录边界逐条读取，直到收齐 SH/Certificate/SKE/SHD
  while not (GotSH and GotCert and GotSKE and GotSHD) do
  begin
    Req := RecvOneRecord;
    if (Req = nil) or (Length(Req) < 5) then
    begin
      DoTLSLog('RecvOneRecord failed or empty, break');
      Break;
    end;
    H := PCnTLSRecordLayer(@Req[0]);
    RecBodyLen := CnGetTLSRecordLayerBodyLength(H);
    DoTLSLog(Format('TLSRecordLayer.ContentType %d', [H^.ContentType]));
    DoTLSLog(Format('TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
    DoTLSLog(Format('TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
    DoTLSLog(Format('TLSRecordLayer.BodyLength %d', [RecBodyLen]));
    if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
    begin
      Off := 0;
      while Off + 4 <= RecBodyLen do
      begin
        B := PCnTLSHandShakeHeader(@H^.Body[Off]);
        L3 := (Integer(B^.LengthHi) shl 16) or UInt16NetworkToHost(B^.LengthLo);
        if (L3 < 0) or (Off + 4 + L3 > RecBodyLen) then
          Break;
        TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, 4 + L3));
        DoTLSLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType, L3]));
        if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO then
        begin
          GotSH := True;
          S := PCnTLSHandShakeServerHello(@B^.Content[0]);
          RandServer := NewBytesFromMemory(@S^.Random[0], SizeOf(S^.Random));
          SelectedCipher := CnGetTLSHandShakeServerHelloCipherSuite(S);
          ServerHelloExt := CnGetTLSHandShakeServerHelloExtensions(B);
          DoTLSLog(Format('ServerHello CipherSuite 0x%.4x', [SelectedCipher]));
          if ServerHelloExt <> nil then
          begin
            ExtItem2 := CnGetTLSHandShakeExtensionsExtensionItem(ServerHelloExt);
            ExtConsumed := 0;
            ExtTotalLen := CnGetTLSHandShakeExtensionsExtensionLength(ServerHelloExt);
            while (ExtItem2 <> nil) and (ExtConsumed < ExtTotalLen) do
            begin
              if CnGetTLSHandShakeExtensionsExtensionType(ExtItem2) =
                CN_TLS_EXTENSIONTYPE_EXTENDED_MASTER_SECRET then
                EmsNegotiated := True;
              Inc(ExtConsumed, SizeOf(Word) + SizeOf(Word) +
                CnGetTLSHandShakeExtensionsExtensionDataLength(ExtItem2));
              ExtItem2 := CnGetTLSHandShakeExtensionsExtensionItem(ServerHelloExt, ExtItem2);
            end;
          end;
        end
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED then
        begin
          GotSKE := True;
          SK := PCnTLSHandShakeServerKeyExchange(@B^.Content[0]);
          ServerKeyBytes := CnGetTLSHandShakeServerKeyExchangeECPoint(SK);
          case CnGetTLSHandShakeServerKeyExchangeNamedCurve(SK) of
            CN_TLS_NAMED_GROUP_SECP256R1: CurveType := ctSecp256r1;
            CN_TLS_NAMED_GROUP_SECP384R1: CurveType := ctSecp384r1;
            CN_TLS_NAMED_GROUP_SECP521R1: CurveType := ctSecp521r1;
          else
            CurveType := ctSecp256r1;
          end;
          DoTLSLog(Format('ServerKeyExchange ECPoint Length %d', [Length(ServerKeyBytes)]));
        end
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED then
          GotSHD := True
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE then
          GotCert := True
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_STATUS_RESERVED then
          DoTLSLog('CertificateStatus received')
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_REQUEST then
          DoTLSLog('CertificateRequest received')
      else
        DoTLSLog(Format('Unhandled HandshakeType %d', [B^.HandShakeType]));
      Off := 4 + (Integer(B^.LengthHi) shl 16) or UInt16NetworkToHost(B^.LengthLo);
      while Off + 4 <= RecBodyLen do
      begin
        B := PCnTLSHandShakeHeader(@H^.Body[Off]);
        L3 := (Integer(B^.LengthHi) shl 16) or UInt16NetworkToHost(B^.LengthLo);
        if (L3 < 0) or (Off + 4 + L3 > RecBodyLen) then
          Break;
        TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, 4 + L3));
        DoTLSLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType, L3]));
        if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED then
        begin
          GotSKE := True;
          SK := PCnTLSHandShakeServerKeyExchange(@B^.Content[0]);
          ServerKeyBytes := CnGetTLSHandShakeServerKeyExchangeECPoint(SK);
          case CnGetTLSHandShakeServerKeyExchangeNamedCurve(SK) of
            CN_TLS_NAMED_GROUP_SECP256R1: CurveType := ctSecp256r1;
            CN_TLS_NAMED_GROUP_SECP384R1: CurveType := ctSecp384r1;
            CN_TLS_NAMED_GROUP_SECP521R1: CurveType := ctSecp521r1;
          else
            CurveType := ctSecp256r1;
          end;
          DoTLSLog(Format('ServerKeyExchange ECPoint Length %d', [Length(ServerKeyBytes)]));
        end
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED then
        begin
          GotSHD := True;
          DoTLSLog('ServerHelloDone');
        end
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_STATUS_RESERVED then
          DoTLSLog('CertificateStatus received')
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_REQUEST then
          DoTLSLog('CertificateRequest received');
        Off := Off + 4 + L3;
      end;
        Off := Off + 4 + L3;
      end;
    end
    else if H^.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
    begin
      if RecBodyLen >= 2 then
      begin
        PBody := @H^.Body[0];
        Lvl := PBody^;
        Inc(PBody);
        Desc := PBody^;
        DoTLSLog(Format('Alert received: level=%d desc=%d', [Lvl, Desc]));
      end;
      Exit;
    end;
    Inc(ServerSeq);
  end;
  Ecc := nil;
  EccPrivKey := nil;
  EccPubKey := nil;
  PreMasterKey := nil;
  if not (GotSH and GotCert and GotSKE and GotSHD) then
  begin
    DoTLSLog('Incomplete server handshake messages, abort');
    Exit;
  end;

  // 计算 ECDHE 共享密钥，派生 pre_master，并发包②：ClientKeyExchange（握手记录，ECDHE 点，opaque8 长度前缀）
  try
    DoTLSLog(Format('SelectedCipher final: 0x%.4x', [SelectedCipher]));
    Ecc := TCnEcc.Create(CurveType);
    EccPrivKey := TCnEccPrivateKey.Create;
    EccPubKey := TCnEccPublicKey.Create;
    Ecc.GenerateKey(EccPrivKey);
    PreMasterKey := TCnEccPublicKey.Create;
    PreMasterX := TCnBigNumber.Create;
    EccPubKey.SetBytes(ServerKeyBytes);
    CnEccDiffieHellmanComputeKey(Ecc, EccPrivKey, EccPubKey, PreMasterKey);
    CipherIsSM4GCM := False;
    CipherIsChaCha20Poly1305 := False;
    if (SelectedCipher = CN_CIPHER_ECDHE_RSA_AES128_GCM_SHA256) or
       (SelectedCipher = CN_CIPHER_ECDHE_ECDSA_AES128_GCM_SHA256) or
       (SelectedCipher = CN_CIPHER_AES128_GCM_SHA256) then
    begin
      KeyLen := 16;
      IvLen := 4;
      DigestType := esdtSHA256;
    end
    else if (SelectedCipher = CN_CIPHER_ECDHE_RSA_AES256_GCM_SHA384) or
            (SelectedCipher = CN_CIPHER_ECDHE_ECDSA_AES256_GCM_SHA384) or
            (SelectedCipher = CN_CIPHER_AES256_GCM_SHA384) then
    begin
      KeyLen := 32;
      IvLen := 4;
      DigestType := esdtSHA384;
    end
    else if (SelectedCipher = CN_CIPHER_TLS_SM4_GCM_SM3) then
    begin
      KeyLen := 16;
      IvLen := 4;
      DigestType := esdtSM3;
      CipherIsSM4GCM := True;
    end
    else if (SelectedCipher = CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305) or
             (SelectedCipher = CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305) then
    begin
      KeyLen := 32;
      IvLen := 12;
      DigestType := esdtSHA256;
      CipherIsChaCha20Poly1305 := True;
    end
    else
    begin
      DoTLSLog(Format('Unsupported CipherSuite 0x%.4x', [SelectedCipher]));
      Exit;
    end;
    CnEccDiffieHellmanGenerateOutKey(Ecc, EccPrivKey, EccPubKey);
    H := PCnTLSRecordLayer(@Buffer[0]);
    H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H^.MajorVersion := 3;
    H^.MinorVersion := 3;
    B := PCnTLSHandShakeHeader(@H^.Body[0]);
    B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE_RESERVED;
    PByte(@B^.Content[0])^ := Byte(Length(EccPubKey.ToBytes(Ecc.BytesCount)));
    CK := PCnTLSHandShakeClientKeyExchange(PAnsiChar(@B^.Content[0]) + 1);
    CnSetTLSHandShakeClientKeyExchangeECPoint(CK, EccPubKey.ToBytes(Ecc.BytesCount));
    CnSetTLSHandShakeHeaderContentLength(B, 1 + Length(EccPubKey.ToBytes(Ecc.BytesCount)));
    CnSetTLSRecordLayerBodyLength(H, 1 + 3 + CnGetTLSHandShakeHeaderContentLength(B));
    if not SendAll(H^, 5 + CnGetTLSRecordLayerBodyLength(H)) then
    begin
      DoTLSLog('Send ClientKeyExchange failed');
      Exit;
    end;
    DoTLSLog('Sent ClientKeyExchange, Size: ' + IntToStr(5 +
      CnGetTLSRecordLayerBodyLength(H)));
    TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B,
      CnGetTLSRecordLayerBodyLength(H)));
    DoTLSLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
      CnGetTLSRecordLayerBodyLength(H)]));
    Inc(ClientSeq);
    Inc(ClientRecordCount);
    if Ecc.PointToPlain(PreMasterKey, PreMasterX) then
    begin
      PreMasterBytes := BigNumberToBytes(PreMasterX, Ecc.BytesCount);
      DoTLSLog('PreMaster (X coord): ' + BytesToHex(PreMasterBytes));
      if EmsNegotiated then
        MasterKey := TLSPseudoRandomFunc(PreMasterBytes, 'extended master secret',
          TLSEccDigestBytes(TotalHandShake, DigestType), DigestType, 48)
      else
        MasterKey := TLSPseudoRandomFunc(PreMasterBytes, 'master secret',
          ConcatBytes(RandClient, RandServer), DigestType, 48);
    end
    else
    begin
      DoTLSLog('Failed to extract ECDH X coordinate');
      Exit;
    end;
    DoTLSLog('MasterSecret: ' + BytesToHex(MasterKey));

    // 发包③：ChangeCipherSpec（CCS，进入加密态；读/写记录序列在 CCS 后重置为0）
    H := PCnTLSRecordLayer(@Buffer[0]);
    H^.ContentType := CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC;
    H^.MajorVersion := 3;
    H^.MinorVersion := 3;
    CC := PCnTLSChangeCipherSpecPacket(@H^.Body[0]);
    CC^.Content := CN_TLS_CHANGE_CIPHER_SPEC;
    CnSetTLSRecordLayerBodyLength(H, 1);
    if not SendAll(H^, 5 + CnGetTLSRecordLayerBodyLength(H)) then
    begin
      DoTLSLog('Send ChangeCipherSpec failed');
      Exit;
    end;
    DoTLSLog('Sent ChangeCipherSpec, Size: ' + IntToStr(5 +
      CnGetTLSRecordLayerBodyLength(H)));
    ClientSeq := 0;
    DoTLSLog('HandshakeHash: ' + BytesToHex(TLSEccDigestBytes(TotalHandShake, DigestType)));
    VerifyData := TLSPseudoRandomFunc(MasterKey, 'client finished',
      TLSEccDigestBytes(TotalHandShake, DigestType), DigestType, 12);
    DoTLSLog('Client VerifyData: ' + BytesToHex(VerifyData));

    // 发包④：Finished（握手记录，加密态；AAD 使用序列 0，长度=明文finished=16）
    H := PCnTLSRecordLayer(@Buffer[0]);
    H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H^.MajorVersion := 3;
    H^.MinorVersion := 3;
    B := PCnTLSHandShakeHeader(@H^.Body[0]);
    B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_FINISHED;
    F := PCnTLSHandShakeFinished(@B^.Content[0]);
    CnSetTLSTLSHandShakeFinishedVerifyData(F, VerifyData);
    CnSetTLSHandShakeHeaderContentLength(B, 12);
    PlainFinished := NewBytesFromMemory(B, 1 + 3 + 12);
    DoTLSLog('PlainFinished: ' + BytesToHex(PlainFinished));
    DoTLSLog('PlainFinished: ' + BytesToHex(PlainFinished));
    TotalHandShake := ConcatBytes(TotalHandShake, PlainFinished);
    DoTLSLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
      Length(PlainFinished)]));
    FillChar(AADFix[0], SizeOf(AADFix), 0);
    for I := 0 to 7 do
      SeqBytes[7 - I] := Byte((ClientSeq shr (I * 8)) and $FF);
    Move(SeqBytes[0], AADFix[0], 8);
    AADFix[8] := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    AADFix[9] := 3;
    AADFix[10] := 3;
    AADFix[11] := Byte(Length(PlainFinished) shr 8);
    AADFix[12] := Byte(Length(PlainFinished) and $FF);
    AAD := NewBytesFromMemory(@AADFix[0], SizeOf(AADFix));
    SetLength(ExplicitNonce, 8);
    for I := 0 to 7 do
      ExplicitNonce[7 - I] := Byte((ClientSeq shr (I * 8)) and $FF);
    DoTLSLog('ClientSeq used for AAD: ' + IntToStr(ClientSeq));
    DoTLSLog('Client AAD: ' + BytesToHex(AAD));
    // move printing of full nonce after IV derived
    KeyBlock := TLSPseudoRandomFunc(MasterKey, 'key expansion', ConcatBytes(RandServer,
      RandClient), DigestType, 2 * KeyLen + 2 * IvLen);
    ClientWriteKey := Copy(KeyBlock, 0, KeyLen);
    ServerWriteKey := Copy(KeyBlock, KeyLen, KeyLen);
    ClientFixedIV := Copy(KeyBlock, 2 * KeyLen, IvLen);
    ServerFixedIV := Copy(KeyBlock, 2 * KeyLen + IvLen, IvLen);
    DoTLSLog('KeyBlock: ' + BytesToHex(KeyBlock));
    DoTLSLog('ClientWriteKey: ' + BytesToHex(ClientWriteKey));
    DoTLSLog('ServerWriteKey: ' + BytesToHex(ServerWriteKey));
    DoTLSLog('Client FixedIV: ' + BytesToHex(ClientFixedIV));
    if CipherIsChaCha20Poly1305 then
      DoTLSLog('Client Nonce (full 12B): ' + BytesToHex(TLSChaChaNonce(ClientFixedIV, ClientSeq)))
    else
      DoTLSLog('Client Nonce (full 12B): ' + BytesToHex(ConcatBytes(ClientFixedIV, ExplicitNonce)));
    DoTLSLog('Server FixedIV: ' + BytesToHex(ServerFixedIV));
    // 保存会话状态到成员，便于后续应用数据加解密
    FClientWriteKey := ClientWriteKey;
    FServerWriteKey := ServerWriteKey;
    FClientFixedIV := ClientFixedIV;
    FServerFixedIV := ServerFixedIV;
    FKeyLen := KeyLen;
    FIVLen := IvLen;
    FDigestType := DigestType;
    FCipherIsSM4GCM := CipherIsSM4GCM;
    FCipherIsChaCha20Poly1305 := CipherIsChaCha20Poly1305;
    EnCipher := nil;
    if CipherIsChaCha20Poly1305 then
      EnCipher := TLSChaCha20Poly1305Encrypt(ClientWriteKey, ClientFixedIV, PlainFinished, AAD, ClientSeq, Tag)
    else if CipherIsSM4GCM then
      EnCipher := SM4GCMEncryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, ExplicitNonce), PlainFinished, AAD, Tag)
    else if KeyLen = 16 then
      EnCipher := AES128GCMEncryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, ExplicitNonce), PlainFinished, AAD, Tag)
    else
      EnCipher := AES256GCMEncryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, ExplicitNonce), PlainFinished, AAD, Tag);
    DoTLSLog('Client EnCipher Len: ' + IntToStr(Length(EnCipher)));
    DoTLSLog('Client Tag: ' + BytesToHex(NewBytesFromMemory(@Tag, SizeOf(Tag))));
    if CipherIsChaCha20Poly1305 then
    begin
      DoTLSLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
      DoTLSLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
      CnSetTLSRecordLayerBodyLength(H, Length(EnCipher) + SizeOf(Tag));
      Move(EnCipher[0], H^.Body[0], Length(EnCipher));
      Move(Tag, (PAnsiChar(@H^.Body[0]) + Length(EnCipher))^, SizeOf(Tag));
    end
    else
    begin
      DoTLSLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(ExplicitNonce, EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
      DoTLSLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(ExplicitNonce, EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
      CnSetTLSRecordLayerBodyLength(H, 8 + Length(EnCipher) + SizeOf(Tag));
      Move(ExplicitNonce[0], H^.Body[0], 8);
      Move(EnCipher[0], (PAnsiChar(@H^.Body[0]) + 8)^, Length(EnCipher));
      Move(Tag, (PAnsiChar(@H^.Body[0]) + 8 + Length(EnCipher))^, SizeOf(Tag));
    end;
    if not SendAll(H^, 5 + CnGetTLSRecordLayerBodyLength(H)) then
    begin
      DoTLSLog('Send Finished failed');
      Exit;
    end;
    DoTLSLog('Sent Finished Packet, Size: ' + IntToStr(5 +
      CnGetTLSRecordLayerBodyLength(H)));
    Inc(ClientSeq);
    FillChar(Buffer, SizeOf(Buffer), 0);

    // 收包②：等待并读取服务器的 CCS 与加密的 Finished（序列0）
    CnFDZero(Rs);
    CnFDSet(Socket, Rs);
    DoTLSLog('Waiting server Finished...');
    if CnSelect(Socket + 1, @Rs, nil, nil, nil) > 0 then
    begin
      DoTLSLog('Readable after Finished');
      BytesReceived := inherited Recv(Buffer[0], Length(Buffer), 0);
      DoTLSLog('Recv len after Finished: ' + IntToStr(BytesReceived));
    end
    else
    begin
      DoTLSLog('Select timeout after Finished');
      BytesReceived := 0;
    end;
    if BytesReceived <= SizeOf(TCnTLSRecordLayer) then
    begin
      DoTLSLog('Receive after Finished failed or no data, len=' + IntToStr(BytesReceived)
        + ', errno=' + IntToStr(CnGetNetErrorNo));
      Exit;
    end;
    DoTLSLog(Format('SSL/TLS Get Response %d', [BytesReceived]));
    TotalConsumed := 0;
    ServerSeq := 0;
    while TotalConsumed < BytesReceived do
    begin
      H := PCnTLSRecordLayer(@Buffer[TotalConsumed]);
      RecBodyLen := CnGetTLSRecordLayerBodyLength(H);
      if H^.ContentType = CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC then
      begin
        DoTLSLog('Recv ChangeCipherSpec');
      end
      else if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
      begin
        // 收包②-1：服务器加密 Finished（握手记录），校验 verify_data，成功则握手完成
        DoTLSLog('Recv Encrypted Handshake');
        DoTLSLog('ServerSeq used for AAD: ' + IntToStr(ServerSeq));
        FillChar(AADFix2[0], SizeOf(AADFix2), 0);
        for I := 0 to 7 do
          SeqBytes[7 - I] := Byte((ServerSeq shr (I * 8)) and $FF);
        Move(SeqBytes[0], AADFix2[0], 8);
        AADFix2[8] := CN_TLS_CONTENT_TYPE_HANDSHAKE;
        AADFix2[9] := 3;
        AADFix2[10] := 3;
        if CipherIsChaCha20Poly1305 then
        begin
          SetLength(ServerEn, RecBodyLen - SizeOf(ServerFinTag));
          Move((PAnsiChar(@H^.Body[0]))^, ServerEn[0], Length(ServerEn));
          Move((PAnsiChar(@H^.Body[0]) + Length(ServerEn))^, ServerFinTag, SizeOf(ServerFinTag));
          AADFix2[11] := Byte((Length(ServerEn)) shr 8);
          AADFix2[12] := Byte((Length(ServerEn)) and $FF);
          AAD := NewBytesFromMemory(@AADFix2[0], SizeOf(AADFix2));
          DoTLSLog('Server FixedIV: ' + BytesToHex(ServerFixedIV));
          DoTLSLog('Server AAD: ' + BytesToHex(AAD));
          DoTLSLog('Server EnCipher Len: ' + IntToStr(Length(ServerEn)));
          DoTLSLog('Server Tag: ' + BytesToHex(NewBytesFromMemory(@ServerFinTag, SizeOf(ServerFinTag))));
          ServerPlain := TLSChaCha20Poly1305Decrypt(ServerWriteKey, ServerFixedIV, ServerEn, AAD, ServerSeq, ServerFinTag);
        end
        else
        begin
          ExplicitNonce := NewBytesFromMemory(@H^.Body[0], 8);
          SetLength(ServerEn, RecBodyLen - 8 - SizeOf(ServerFinTag));
          Move((PAnsiChar(@H^.Body[0]) + 8)^, ServerEn[0], Length(ServerEn));
          Move((PAnsiChar(@H^.Body[0]) + 8 + Length(ServerEn))^, ServerFinTag, SizeOf(ServerFinTag));
          AADFix2[11] := Byte(((RecBodyLen - 8 - SizeOf(ServerFinTag))) shr 8);
          AADFix2[12] := Byte(((RecBodyLen - 8 - SizeOf(ServerFinTag))) and $FF);
          AAD := NewBytesFromMemory(@AADFix2[0], SizeOf(AADFix2));
          DoTLSLog('Server ExplicitNonce: ' + BytesToHex(ExplicitNonce));
          DoTLSLog('Server FixedIV: ' + BytesToHex(ServerFixedIV));
          DoTLSLog('Server AAD: ' + BytesToHex(AAD));
          DoTLSLog('Server EnCipher Len: ' + IntToStr(Length(ServerEn)));
          DoTLSLog('Server Tag: ' + BytesToHex(NewBytesFromMemory(@ServerFinTag, SizeOf(ServerFinTag))));
          if CipherIsSM4GCM then
            ServerPlain := SM4GCMDecryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, ExplicitNonce), ServerEn, AAD, ServerFinTag)
          else if KeyLen = 16 then
            ServerPlain := AES128GCMDecryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, ExplicitNonce), ServerEn, AAD, ServerFinTag)
          else
            ServerPlain := AES256GCMDecryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, ExplicitNonce), ServerEn, AAD, ServerFinTag);
        end;
        if (Length(ServerPlain) = 0) then
        begin
          DoTLSLog('Decrypt Server Finished Failed');
          Exit;
        end;
        DoTLSLog('Server Finished Decrypted Length: ' + IntToStr(Length(ServerPlain)));
        B := PCnTLSHandShakeHeader(@ServerPlain[0]);
        F := PCnTLSHandShakeFinished(@B^.Content[0]);
        VerifyData := TLSPseudoRandomFunc(MasterKey, 'server finished',
          TLSEccDigestBytes(TotalHandShake, DigestType), DigestType, 12);
        Ok := CompareMem(@VerifyData[0], @F^.VerifyData[0], 12);
        DoTLSLog('Server VerifyData: ' + BytesToHex(VerifyData));
        DoTLSLog('Verify Match: ' + BoolToStr(Ok, True));
        if not Ok then
        begin
          DoTLSLog('Server Finished verify mismatch');
          Exit;
        end;
        Result := True;
        DoTLSLog('Handshake Completed');
        Inc(ServerSeq);
        // 握手完成后，发送应用数据并接收解密响应
        FClientSeq := ClientSeq;
        FServerSeq := ServerSeq;

        FHandshakeState := hsConnected;

{
        SetLength(Req, 0);
        Req := ConcatBytes(AnsiToBytes('GET / HTTP/1.1'#13#10),
                           AnsiToBytes('Host: ' + string(Host) + #13#10),
                           AnsiToBytes('Connection: close'#13#10),
                           AnsiToBytes('Accept: */*'#13#10#13#10));
        if not EncryptAndSendAppData(Sock, Req) then
        begin
          DoTLSLog('Send ApplicationData failed');
          CnCloseSocket(Sock);
          Exit;
        end;
        DoTLSLog('Sent ApplicationData');
        if ReadAndDecryptAppData(Sock, ServerPlain) then
        begin
          SetLength(TmpStr, Length(ServerPlain));
          Move(ServerPlain[0], PAnsiChar(TmpStr)^, Length(ServerPlain));
          DoTLSLog('HTTP Response Chunk: ' + string(TmpStr));
        end
        else
        begin
          DoTLSLog('Decrypt Server AppData Failed');
          CnCloseSocket(Sock);
          Exit;
        end;
}

        Break;
      end
      else if H^.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
      begin
        DoTLSLog('Recv Alert');
        if RecBodyLen >= 2 then
        begin
          Lvl := Ord(PAnsiChar(@H^.Body[0])^);
          Desc := Ord((PAnsiChar(@H^.Body[0]) + 1)^);
          DoTLSLog(Format('Alert level=%d, desc=%d', [Lvl, Desc]));
        end;
      end;
      Inc(TotalConsumed, 5 + RecBodyLen);
      if H^.ContentType <> CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC then
        Inc(ServerSeq);
    end;
  finally
    PreMasterKey.Free;
    PreMasterX.Free;
    EccPubKey.Free;
    EccPrivKey.Free;
    Ecc.Free;
  end;
end;

end.

