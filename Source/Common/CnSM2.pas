{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnSM2;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SM2 椭圆曲线算法单元
* 单元作者：刘啸
* 备    注：要实现基于 SM2 的数据加解密、签名验签、密钥交换
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.04.02 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnECC, CnBigNumber, CnSM3, CnKDF;

type
  TCnSM2 = class(TCnEcc)
  {* SM2 椭圆曲线运算类，具体实现在指定曲线类型的基类 TCnEcc 中}
  public
    constructor Create; override;
  end;

  TCnSM2Signature = class(TCnEccPoint);
  {* 签名是两个大数，X Y 分别代表 R S}

// ========================= SM2 椭圆曲线加解密算法 ============================

function CnSM2EncryptData(PlainData: Pointer; DataLen: Integer; OutStream:
  TStream; PublicKey: TCnEccPublicKey; Sm2: TCnSm2 = nil): Boolean;
{* 用公钥对数据块进行加密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则}

function CnSM2DecryptData(EnData: Pointer; DataLen: Integer; OutStream: TStream;
  PrivateKey: TCnEccPrivateKey; Sm2: TCnSm2 = nil): Boolean;
{* 用公钥对数据块进行解密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则}

// ====================== SM2 椭圆曲线数字签名验证算法 =========================

function CnSM2SignData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  OutSignature: TCnSM2Signature; PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey;
  Sm2: TCnSM2 = nil): Boolean;
{* 私钥对数据块签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法
   第2部分:数字签名算法》中的运算规则，要附上签名者与曲线信息以及公钥的数字摘要}

function CnSM2VerifyData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  InSignature: TCnSM2Signature; PublicKey: TCnEccPublicKey; Sm2: TCnSM2 = nil): Boolean;
{* 公钥验证数据块的签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法
   第2部分:数字签名算法》中的运算规则来}

// ======================== SM2 椭圆曲线密钥交换算法 ===========================

{
  SM2 密钥交换前提：A B 双方都有自身 ID 与公私钥，并都知道对方的 ID 与对方的公钥
}
function CnSM2KeyExchangeAStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey;
  OutRA: TCnEccPoint; Sm2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第一步 A 用户生成随机密钥 RA，供发给 B}

function CnSM2KeyExchangeBStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey; InRA: TCnEccPoint;
  out OutKeyB: AnsiString; OutRB: TCnEccPoint; out OutOptionalSB: TSM3Digest; Sm2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第二步 B 用户收到 A 的数据，计算 Kb，并把可选的验证结果返回 A}

function CnSM2KeyExchangeAStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey; InRB: TCnEccPoint;
  out OutKeyA: AnsiString; out OutOptionalSA: TSM3Digest; Sm2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第三步 A 用户收到 B 的数据计算 Ka，并把可选的验证结果返回 B，初步协商好 Ka = Kb}

function CnSM2KeyExchangeBStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey;
  const InOptionalSA: TSM3Digest; Sm2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第四步 B 用户收到 A 的数据计算结果校验，协商完毕，此步可选}

implementation

{ TCnSM2 }

constructor TCnSM2.Create;
begin
  inherited;
  Load(ctSM2);
end;

{
  传入明文 M，长 MLen 字节，随机生成 k，计算

  Cl = k * G => (xl, yl)         // 非压缩存储，长度为两个数字位长加 1

  k * PublicKey => (x2, y2)
  t <= KDF(x2‖y2, Mlen)
  C2 <= M xor t                  // 长度 MLen

  C3 <= SM3(x2‖M‖y2)           // 长度 32 字节

  密文为：C1‖C3‖C2
}
function CnSM2EncryptData(PlainData: Pointer; DataLen: Integer; OutStream:
  TStream; PublicKey: TCnEccPublicKey; Sm2: TCnSm2 = nil): Boolean;
var
  Py, P1, P2: TCnEccPoint;
  K: TCnBigNumber;
  B: Byte;
  M: PAnsiChar;
  I: Integer;
  Buf: array of Byte;
  KDFStr, T, C3H: AnsiString;
  Sm3Dig: TSM3Digest;
  Sm2IsNil: Boolean;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (OutStream = nil) or (PublicKey = nil) then
    Exit;

  Py := nil;
  P1 := nil;
  P2 := nil;
  K := nil;
  Sm2IsNil := Sm2 = nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    K := TCnBigNumber.Create;

    // 确保公钥 X Y 均存在
    if PublicKey.Y.IsZero then
    begin
      Py := TCnEccPoint.Create;
      if not Sm2.PlainToPoint(PublicKey.X, Py) then
        Exit;
      BigNumberCopy(PublicKey.Y, Py.Y);
    end;

    // 生成一个随机 K
    if not BigNumberRandRange(K, Sm2.Order) then
      Exit;
    // K.SetHex('384F30353073AEECE7A1654330A96204D37982A3E15B2CB5');

    P1 := TCnEccPoint.Create;
    P1.Assign(Sm2.Generator);
    Sm2.MultiplePoint(K, P1);  // 计算出 K * G 得到 X1 Y1

    B := 4;
    OutStream.Position := 0;

    OutStream.Write(B, 1);
    SetLength(Buf, P1.X.GetBytesCount);
    P1.X.ToBinary(@Buf[0]);
    OutStream.Write(Buf[0], P1.X.GetBytesCount);
    SetLength(Buf, P1.Y.GetBytesCount);
    P1.Y.ToBinary(@Buf[0]);
    OutStream.Write(Buf[0], P1.Y.GetBytesCount); // 拼成 C1

    P2 := TCnEccPoint.Create;
    P2.Assign(PublicKey);
    Sm2.MultiplePoint(K, P2); // 计算出 K * PublicKey 得到 X2 Y2

    SetLength(KDFStr, P2.X.GetBytesCount + P2.Y.GetBytesCount);
    P2.X.ToBinary(@KDFStr[1]);
    P2.Y.ToBinary(@KDFStr[P2.X.GetBytesCount + 1]);
    T := CnSM2KDF(KDFStr, DataLen);

    M := PAnsiChar(PlainData);
    for I := 1 to DataLen do
      T[I] := AnsiChar(Byte(T[I]) xor Byte(M[I - 1])); // T 里是 C2，但先不能写

    SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + DataLen);
    P2.X.ToBinary(@C3H[1]);
    Move(M[0], C3H[P2.X.GetBytesCount + 1], DataLen);
    P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + DataLen + 1]); // 拼成算 C3 的
    Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

    OutStream.Write(Sm3Dig[0], SizeOf(TSM3Digest));        // 写入 C3
    OutStream.Write(T[1], DataLen);                        // 写入 C2
    Result := True;
  finally
    P2.Free;
    P1.Free;
    Py.Free;
    K.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

{
  MLen <= DataLen - SM3DigLength - 2 * Sm2 Byte Length - 1，劈开拿到 C1 C2 C3

  PrivateKey * C1 => (x2, y2)

  t <= KDF(x2‖y2, Mlen)

  M' <= C2 xor t

  还可对比 SM3(x2‖M‖y2) Hash 是否与 C3 相等
}
function CnSM2DecryptData(EnData: Pointer; DataLen: Integer; OutStream: TStream;
  PrivateKey: TCnEccPrivateKey; Sm2: TCnSM2): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  MP: AnsiString;
  KDFStr, T, C3H: AnsiString;
  Sm2IsNil: Boolean;
  P2: TCnEccPoint;
  I: Integer;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  if (EnData = nil) or (DataLen <= 0) or (OutStream = nil) or (PrivateKey = nil) then
    Exit;

  P2 := nil;
  Sm2IsNil := Sm2 = nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    MLen := DataLen - SizeOf(TSM3Digest) - (Sm2.BitsCount div 4) - 1;
    if MLen <= 0 then
      Exit;

    P2 := TCnEccPoint.Create;
    M := PAnsiChar(EnData);
    Inc(M);
    P2.X.SetBinary(M, Sm2.BitsCount div 8);
    Inc(M, Sm2.BitsCount div 8);
    P2.Y.SetBinary(M, Sm2.BitsCount div 8);
    Sm2.MultiplePoint(PrivateKey, P2);

    SetLength(KDFStr, P2.X.GetBytesCount + P2.Y.GetBytesCount);
    P2.X.ToBinary(@KDFStr[1]);
    P2.Y.ToBinary(@KDFStr[P2.X.GetBytesCount + 1]);
    T := CnSM2KDF(KDFStr, MLen);

    SetLength(MP, MLen);
    M := PAnsiChar(EnData);
    Inc(M, SizeOf(TSM3Digest) + (Sm2.BitsCount div 4) + 1);
    for I := 1 to MLen do
      MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I])); // MP 得到明文

    SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + MLen);
    P2.X.ToBinary(@C3H[1]);
    Move(MP[1], C3H[P2.X.GetBytesCount + 1], MLen);
    P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + MLen + 1]);    // 拼成算 C3 的
    Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

    M := PAnsiChar(EnData);
    Inc(M, (Sm2.BitsCount div 4) + 1);
    if CompareMem(@Sm3Dig[0], M, SizeOf(TSM3Digest)) then  // 比对 Hash 是否相等
    begin
      OutStream.Write(MP[1], Length(MP));
      Result := True;
    end;
  finally
    P2.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

// 计算 Za 值也就是 Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
function CalcSM2UserHash(const UserID: AnsiString; PublicKey: TCnEccPublicKey;
  Sm2: TCnSM2): TSM3Digest;
var
  Stream: TMemoryStream;
  Len: Integer;
  ULen: Word;
begin
  Stream := TMemoryStream.Create;
  try
    Len := Length(UserID) * 8;
    ULen := ((Len and $FF) shl 8) or ((Len and $FF00) shr 8);

    Stream.Write(ULen, SizeOf(ULen));
    if ULen > 0 then
      Stream.Write(UserID[1], Length(UserID));

    BigNumberWriteBinaryToStream(Sm2.CoefficientA, Stream);
    BigNumberWriteBinaryToStream(Sm2.CoefficientB, Stream);
    BigNumberWriteBinaryToStream(Sm2.Generator.X, Stream);
    BigNumberWriteBinaryToStream(Sm2.Generator.Y, Stream);
    BigNumberWriteBinaryToStream(PublicKey.X, Stream);
    BigNumberWriteBinaryToStream(PublicKey.Y, Stream);

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);  // 算出 ZA
  finally
    Stream.Free;
  end;
end;

// 根据 Za 与数据再次计算杂凑值 e
function CalcSM2SignatureHash(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  PublicKey: TCnEccPublicKey; Sm2: TCnSM2): TSM3Digest;
var
  Stream: TMemoryStream;
  Sm3Dig: TSM3Digest;
begin
  Stream := TMemoryStream.Create;
  try
    Sm3Dig := CalcSM2UserHash(UserID, PublicKey, Sm2);
    Stream.Write(Sm3Dig[0], SizeOf(TSM3Digest));
    Stream.Write(PlainData^, DataLen);

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);  // 再次算出杂凑值 e
  finally
    Stream.Free;
  end;
end;

{
  ZA <= Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
  e <= Hash(ZA‖M)

  k * G => (x1, y1)

  r <= (e + x1) mod n

  s <= ((1 + PrivateKey)^-1 * (k - r * PrivateKey)) mod n
}
function CnSM2SignData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  OutSignature: TCnSM2Signature; PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey;
  Sm2: TCnSM2): Boolean;
var
  K, R, E: TCnBigNumber;
  P: TCnEccPoint;
  Sm2IsNil: Boolean;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (OutSignature = nil) or
    (PrivateKey = nil) or (PublicKey = nil) then
    Exit;

  K := nil;
  P := nil;
  E := nil;
  R := nil;
  Sm2IsNil := Sm2 = nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataLen, PublicKey, Sm2); // 杂凑值 e

    P := TCnEccPoint.Create;
    E := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    K := TCnBigNumber.Create;

    while True do
    begin
      // 生成一个随机 K
      if not BigNumberRandRange(K, Sm2.Order) then
        Exit;
      // K.SetHex('6CB28D99385C175C94F94E934817663FC176D925DD72B727260DBAAE1FB2F96F');

      P.Assign(Sm2.Generator);
      Sm2.MultiplePoint(K, P);

      // 计算 R = (e + x) mod N
      E.SetBinary(@Sm3Dig[0], SizeOf(TSM3Digest));
      if not BigNumberAdd(E, E, P.X) then
        Exit;
      if not BigNumberMod(R, E, Sm2.Order) then // 算出 R 后 E 不用了
        Exit;

      if R.IsZero then  // R 不能为 0
        Continue;

      if not BigNumberAdd(E, R, K) then
        Exit;
      if BigNumberCompare(E, Sm2.Order) = 0 then // R + K = N 也不行
        Continue;

      BigNumberCopy(OutSignature.X, R);  // 得到一个签名值 R

      BigNumberCopy(E, PrivateKey);
      BigNumberAddWord(E, 1);
      BigNumberModularInverse(R, E, Sm2.Order);      // 求逆元得到 (1 + PrivateKey)^-1，放在 R 里

      // 求 K - R * PrivateKey，又用起 E 来
      if not BigNumberMul(E, OutSignature.X, PrivateKey) then
        Exit;
      if not BigNumberSub(E, K, E) then
        Exit;

      if not BigNumberMul(R, E, R) then // (1 + PrivateKey)^-1 * (K - R * PrivateKey) 放在 R 里
        Exit;

      if not BigNumberNonNegativeMod(OutSignature.Y, R, Sm2.Order) then // 注意余数不能为负
        Exit;

      Result := True;
      Break;
    end;
  finally
    K.Free;
    P.Free;
    R.Free;
    E.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

{
  ZA = Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
  e <= Hash(ZA‖M)

  t <= (r + s) mod n
  P <= s * G + t * PublicKey
  r' <= (e + P.x) mod n
  比对 r' 和 r
}
function CnSM2VerifyData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  InSignature: TCnSM2Signature; PublicKey: TCnEccPublicKey; Sm2: TCnSM2 = nil): Boolean;
var
  K, R, E: TCnBigNumber;
  P, Q: TCnEccPoint;
  Sm2IsNil: Boolean;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (InSignature = nil) or (PublicKey = nil) then
    Exit;

  K := nil;
  P := nil;
  Q := nil;
  E := nil;
  R := nil;
  Sm2IsNil := Sm2 = nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    if BigNumberCompare(InSignature.X, Sm2.Order) >= 0 then
      Exit;
    if BigNumberCompare(InSignature.Y, Sm2.Order) >= 0 then
      Exit;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataLen, PublicKey, Sm2); // 杂凑值 e

    P := TCnEccPoint.Create;
    Q := TCnEccPoint.Create;
    E := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    K := TCnBigNumber.Create;

    if not BigNumberAdd(K, InSignature.X, InSignature.Y) then
      Exit;
    if not BigNumberNonNegativeMod(R, K, Sm2.Order) then
      Exit;
    if R.IsZero then  // (r + s) mod n = 0 则失败，这里 R 是文中的 T
      Exit;

    P.Assign(Sm2.Generator);
    Sm2.MultiplePoint(InSignature.Y, P);
    Q.Assign(PublicKey);
    Sm2.MultiplePoint(R, Q);
    Sm2.PointAddPoint(P, Q, P);   // s * G + t * PublicKey => P

    E.SetBinary(@Sm3Dig[0], SizeOf(TSM3Digest));
    if not BigNumberAdd(E, E, P.X) then
      Exit;

    if not BigNumberNonNegativeMod(R, E, Sm2.Order) then
      Exit;

    Result := BigNumberCompare(R, InSignature.X) = 0;
  finally
    K.Free;
    P.Free;
    Q.Free;
    R.Free;
    E.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

{
  计算交换出的密钥：KDF(Xuv‖Yuv‖Za‖Zb, kLen)
}
function CalcSM2ExchangeKey(UV: TCnEccPoint; Za, Zb: TSM3Digest; KeyByteLength: Integer): AnsiString;
var
  Stream: TMemoryStream;
  S: AnsiString;
begin
  Stream := TMemoryStream.Create;
  try
    BigNumberWriteBinaryToStream(UV.X, Stream);
    BigNumberWriteBinaryToStream(UV.Y, Stream);
    Stream.Write(Za[0], SizeOf(TSM3Digest));
    Stream.Write(Zb[0], SizeOf(TSM3Digest));

    SetLength(S, Stream.Size);
    Stream.Position := 0;
    Stream.Read(S[1], Stream.Size);

    Result := CnSM2KDF(S, KeyByteLength);
  finally
    SetLength(S, 0);
    Stream.Free;
  end;
end;

{
  Hash(0x02‖Yuv‖Hash(Xuv‖Za‖Zb‖X1‖Y1‖X2‖Y2))
       0x03
}
function CalcSM2OptionalSig(UV, P1, P2: TCnEccPoint; Za, Zb: TSM3Digest; Step2or3: Boolean): TSM3Digest;
var
  Stream: TMemoryStream;
  Sm3Dig: TSM3Digest;
  B: Byte;
begin
  if Step2or3 then
    B := 2
  else
    B := 3;

  Stream := TMemoryStream.Create;
  try
    BigNumberWriteBinaryToStream(UV.X, Stream);
    Stream.Write(Za[0], SizeOf(TSM3Digest));
    Stream.Write(Zb[0], SizeOf(TSM3Digest));
    BigNumberWriteBinaryToStream(P1.X, Stream);
    BigNumberWriteBinaryToStream(P1.Y, Stream);
    BigNumberWriteBinaryToStream(P2.X, Stream);
    BigNumberWriteBinaryToStream(P2.Y, Stream);
    Sm3Dig := SM3(PAnsiChar(Stream.Memory), Stream.Size);

    Stream.Clear;
    Stream.Write(B, 1);
    BigNumberWriteBinaryToStream(UV.Y, Stream);
    Stream.Write(Sm3Dig[0], SizeOf(TSM3Digest));

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);
  finally
    Stream.Free;
  end;
end;

{
  随机值 rA * G => RA 传给 B
}
function CnSM2KeyExchangeAStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey;
  OutRA: TCnEccPoint; Sm2: TCnSM2): Boolean;
var
  R: TCnBigNumber;
  Sm2IsNil: Boolean;
begin
  Result := False;
  if (KeyByteLength <= 0) or (APrivateKey = nil) or (APublicKey = nil) or (OutRA = nil) then
    Exit;

  Sm2IsNil := Sm2 = nil;
  R := nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    R := TCnBigNumber.Create;
    if not BigNumberRandRange(R, Sm2.Order) then
      Exit;
    R.SetHex('83A2C9C8B96E5AF70BD480B472409A9A327257F1EBB73F5B073354B248668563');

    OutRA.Assign(Sm2.Generator);
    Sm2.MultiplePoint(R, OutRA);
    Result := True;
  finally
    R.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

{
  随机值 * G => RB
  x2 <= RB.X
  X2 <= 2^W + (x2 and (2^W - 1) 表示把 x2 的第 W 位置 1，W + 1 以上全塞 0
  T <= (BPrivateKey + 随机值 * X2) mod N

  x1 <= RA.X
  X1 <= 2^W + (x1 and (2^W - 1)
  KB <= (h * T) * (APublicKey + X1 * RA)

  注意 BigNumber 的 BitCount 为 2 为底的对数向上取整
}
function CnSM2KeyExchangeBStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey; InRA: TCnEccPoint;
  out OutKeyB: AnsiString; OutRB: TCnEccPoint; out OutOptionalSB: TSM3Digest; Sm2: TCnSM2): Boolean;
var
  Sm2IsNil: Boolean;
  I, W: Integer;
  R, X, T: TCnBigNumber;
  V: TCnEccPoint;
  Za, Zb: TSM3Digest;
begin
  Result := False;
  if (KeyByteLength <= 0) or (BPrivateKey = nil) or (APublicKey = nil) or
    (BPublicKey = nil) or (InRA = nil) then
    Exit;

  Sm2IsNil := Sm2 = nil;
  R := nil;
  X := nil;
  T := nil;
  V := nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    if not Sm2.IsPointOnCurve(InRA) then // 验证传过来的 RA 是否满足方程
      Exit;

    R := TCnBigNumber.Create;
    if not BigNumberRandRange(R, Sm2.Order) then
      Exit;

    // R.SetHex('33FE21940342161C55619C4A0C060293D543C80AF19748CE176D83477DE71C80');
    OutRB.Assign(Sm2.Generator);
    Sm2.MultiplePoint(R, OutRB);

    W := (Sm2.Order.GetBitsCount + 1) div 2 - 1;
    X := TCnBigNumber.Create;

    BigNumberCopy(X, OutRB.X);

    // 2^W 次方表示第 W 位 1（位从 0 开始算） ，2^W - 1 则表示 0 位到 W - 1 位全置 1
    // X2 = 2^W + (x2 and (2^W - 1) 表示把 x2 的第 W 位置 1，W + 1 以上全塞 0，x2 是 RB.X
    BigNumberSetBit(X, W);
    for I := W + 1 to X.GetBitsCount - 1 do
      BigNumberClearBit(X, I);

    if not BigNumberMul(X, R, X) then
      Exit;
    if not BigNumberAdd(X, X, BPrivateKey) then
      Exit;
    T := TCnBigNumber.Create;
    if not BigNumberNonNegativeMod(T, X, Sm2.Order) then // T = (BPrivateKey + 随机值 * X2) mod N
      Exit;

    BigNumberCopy(X, InRA.X);
    BigNumberSetBit(X, W);
    for I := W + 1 to X.GetBitsCount - 1 do
      BigNumberClearBit(X, I);

    // 计算 XV YV。 (h * t) * (APublicKey + X * RA)
    V := TCnEccPoint.Create;
    V.Assign(InRA);
    Sm2.MultiplePoint(X, V);
    Sm2.PointAddPoint(V, APublicKey, V);
    Sm2.MultiplePoint(T, V);

    if V.X.IsZero or V.Y.IsZero then // 如果是无穷远点则协商失败
      Exit;

    // 协商初步成功，计算 KB
    Za := CalcSM2UserHash(AUserID, APublicKey, Sm2);
    Zb := CalcSM2UserHash(BUserID, BPublicKey, Sm2);
    OutKeyB := CalcSM2ExchangeKey(V, Za, Zb, KeyByteLength); // 共享密钥协商成功！

    // 然后计算 SB 供 A 核对
    OutOptionalSB := CalcSM2OptionalSig(V, InRA, OutRB, Za, Zb, True);
    Result := True;
  finally
    V.Free;
    T.Free;
    X.Free;
    R.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

function CnSM2KeyExchangeAStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey; InRB: TCnEccPoint;
  out OutKeyA: AnsiString; out OutOptionalSA: TSM3Digest; Sm2: TCnSM2): Boolean;
var
  Sm2IsNil: Boolean;
begin
  Sm2IsNil := Sm2 = nil;

  try

    if Sm2IsNil then
      Sm2 := TCnSM2.Create;


  finally
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

function CnSM2KeyExchangeBStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnEccPrivateKey; APublicKey, BPublicKey: TCnEccPublicKey;
  const InOptionalSA: TSM3Digest; Sm2: TCnSM2): Boolean;
var
  Sm2IsNil: Boolean;
begin
  Sm2IsNil := Sm2 = nil;

  try

    if Sm2IsNil then
      Sm2 := TCnSM2.Create;


  finally
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

end.

