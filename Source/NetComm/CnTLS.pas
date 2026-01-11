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
  {$ENDIF} {$ENDIF}
  CnConsts, CnNetConsts, CnSocket, CnClasses, CnNetwork, CnRandom, CnPoly1305, CnBigNumber,
  CnNative, CnECC, CnAEAD, CnChaCha20, CnTCPClient, CnSHA2, CnSM3, CnMD5, CnSHA1;

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

    procedure DoTLSLog(const Msg: string);
    function BuildAppDataAAD(Seq: TUInt64; PlainLen: Integer): TBytes;
    function EncryptAppDataBody(const Plain, AAD, Nonce: TBytes; out Tag: TCnGCM128Tag): TBytes;
    function DecryptAppDataBody(const Body, AAD: TBytes; const Nonce: TBytes; var InTag: TCnGCM128Tag): TBytes;
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
      
    property OnTLSLog: TCnTLSogEvent read FOnTLSLog write FOnTLSLog;
  end;

implementation

resourcestring
  SCnTLSClientName = 'CnTLSClient';
  SCnTLSClientComment = 'TLS 1.2 Client Component';
  SHandshakeFailed = 'TLS Handshake failed: %s';
  SConnectionFailed = 'TLS Connection failed';
  SEncryptionFailed = 'TLS Encryption failed';
  SDecryptionFailed = 'TLS Decryption failed';
  SInvalidCipherSuite = 'Invalid cipher suite: 0x%.4x';

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

function TLSChaChaNonce(const FixedIV12: TBytes;
  Seq: TUInt64): TBytes;
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
{$IFNDEF MSWINDOWS}
  HE: THostEntry;
{$ENDIF}
  AADFix: array[0..12] of Byte;
  AADFix2: array[0..12] of Byte;
  SeqBytes: array[0..7] of Byte;
  I: Integer;
  EmsNegotiated: Boolean;
  Lvl, Desc: Integer;
  ClientRecordCount, ServerRecordCount: TUInt64;
  Req: TBytes;
  IVNonce: TBytes;
  TmpStr: AnsiString;
  GotSH, GotCert, GotSKE, GotSHD: Boolean;
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
  if inherited Send(H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
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
  FillChar(Buffer, SizeOf(Buffer), 0);

  // 收包①：读取服务器握手记录（可能分多包）
  // 期望顺序：ServerHello →Certificate →ServerKeyExchange →ServerHelloDone
  BytesReceived := inherited Recv(Buffer[0], Length(Buffer), 0);
  if BytesReceived <= SizeOf(TCnTLSRecordLayer) then
  begin
    DoTLSLog('Receive response failed or no data');
    Exit;
  end;
  DoTLSLog(Format('SSL/TLS Get Response %d', [BytesReceived]));
  TotalConsumed := 0;
  SelectedCipher := 0;
  GotSH := False;
  GotCert := False;
  GotSKE := False;
  GotSHD := False;
  while TotalConsumed < BytesReceived do
  begin
    CurrentPtr := @Buffer[TotalConsumed];
    H := PCnTLSRecordLayer(CurrentPtr);
    DoTLSLog(Format('TLSRecordLayer.ContentType %d', [H^.ContentType]));
    DoTLSLog(Format('TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
    DoTLSLog(Format('TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
    DoTLSLog(Format('TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(H)]));
    Inc(TotalConsumed, 5 + CnGetTLSRecordLayerBodyLength(H));
    Inc(ServerSeq);
    if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
    begin
      B := PCnTLSHandShakeHeader(@H^.Body[0]);
      TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B,
        CnGetTLSRecordLayerBodyLength(H)));
      DoTLSLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
        CnGetTLSRecordLayerBodyLength(H)]));
      if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO then
      begin
        GotSH := True;
        S := PCnTLSHandShakeServerHello(@B^.Content[0]);
        RandServer := NewBytesFromMemory(@S^.Random[0], SizeOf(S^.Random));
        SelectedCipher := CnGetTLSHandShakeServerHelloCipherSuite(S);
        DoTLSLog(Format('ServerHello CipherSuite 0x%.4x', [SelectedCipher]));
        ServerHelloExt := CnGetTLSHandShakeServerHelloExtensions(B);
        if ServerHelloExt <> nil then
        begin
          try
            ExtItem2 := CnGetTLSHandShakeExtensionsExtensionItem(ServerHelloExt);
            while ExtItem2 <> nil do
            begin
              if CnGetTLSHandShakeExtensionsExtensionType(ExtItem2) =
                CN_TLS_EXTENSIONTYPE_EXTENDED_MASTER_SECRET then
                EmsNegotiated := True;
              ExtItem2 := CnGetTLSHandShakeExtensionsExtensionItem(ServerHelloExt,
                ExtItem2);
            end;
          except
            EmsNegotiated := False;
          end;
        end;
        DoTLSLog('EMS Negotiated: ' + BoolToStr(EmsNegotiated, True));
      end
      else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE then
      begin
        GotCert := True;
        Cer := PCnTLSHandShakeCertificate(@B^.Content[0]);
        CI := CnGetTLSHandShakeCertificateItem(Cer);
        ServerCertBytes := CnGetTLSHandShakeCertificateItemCertificate(CI);
        DoTLSLog(Format('Certificate Length %d', [Length(ServerCertBytes)]));
      end
      else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED then
      begin
        GotSKE := True;
        SK := PCnTLSHandShakeServerKeyExchange(@B^.Content[0]);
        ServerKeyBytes := CnGetTLSHandShakeServerKeyExchangeECPoint(SK);
      // set curve by named group from ServerKeyExchange
        case CnGetTLSHandShakeServerKeyExchangeNamedCurve(SK) of
          CN_TLS_NAMED_GROUP_SECP256R1:
            CurveType := ctSecp256r1;
          CN_TLS_NAMED_GROUP_SECP384R1:
            CurveType := ctSecp384r1;
          CN_TLS_NAMED_GROUP_SECP521R1:
            CurveType := ctSecp521r1;
        else
          CurveType := ctSecp256r1;
        end;
        DoTLSLog(Format('ServerKeyExchange ECPoint Length %d', [Length(ServerKeyBytes)]));
      end
      else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED then
      begin
        GotSHD := True;
        DoTLSLog('ServerHelloDone');
      end;
    end
  end;

  // 收包①补充：若未收全 SH/Certificate/SKE/SHD，则继续 select/recv，直到四者齐备
  while not (GotSH and GotCert and GotSKE and GotSHD) do
  begin
    CnFDZero(Rs);
    CnFDSet(Socket, Rs);
    if CnSelect(Socket + 1, @Rs, nil, nil, nil) <= 0 then
      Break;
    FillChar(Buffer, SizeOf(Buffer), 0);
    BytesReceived := inherited Recv(Buffer[0], Length(Buffer), 0);
    if BytesReceived <= SizeOf(TCnTLSRecordLayer) then
      Break;
    DoTLSLog(Format('SSL/TLS Get Response %d', [BytesReceived]));
    TotalConsumed := 0;
    while TotalConsumed < BytesReceived do
    begin
      CurrentPtr := @Buffer[TotalConsumed];
      H := PCnTLSRecordLayer(CurrentPtr);
      RecBodyLen := CnGetTLSRecordLayerBodyLength(H);
      if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
      begin
        B := PCnTLSHandShakeHeader(@H^.Body[0]);
        TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H)));
        DoTLSLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType, CnGetTLSRecordLayerBodyLength(H)]));
        if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO then
          GotSH := True
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
          GotCert := True;
      end;
      Inc(TotalConsumed, 5 + RecBodyLen);
      Inc(ServerSeq);
    end;
  end;
  Ecc := nil;
  EccPrivKey := nil;
  EccPubKey := nil;
  PreMasterKey := nil;

  // 计算 ECDHE 共享密钥，派生 pre_master，并发包②：ClientKeyExchange（握手记录，ECDHE 点，opaque8 长度前缀）
  try
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
    if inherited Send(H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
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
    if inherited Send(H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
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
    if inherited Send(H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
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
