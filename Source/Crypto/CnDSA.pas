{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnDSA;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：DSA 算法单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了基于普通离散对数（非椭圆曲线离散对数）的 DSA 签名验签机制。
*           部分规范基于 NIST.FIPS.186-4
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.10.11 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnBigNumber, CnNative, CnMD5, CnSHA1, CnSHA2, CnSM3;

type
  TCnDSAPrimeType = (dptBit1024160, dptBit2048224, dptBit2048256, dptBit3072256);
  {* DSA 的素数位数分类，分别是 P 和 Q 的位数}

  TCnDSAHashType = (dhtAuto, dhtMD5, dhtSHA1, dhtSHA224, dhtSHA256, dhtSM3);
  {* DSA 的签名的杂凑类型，Auto 表示根据 Q 的位数自动选择}

  TCnDSADomainParameter = class(TPersistent)
  {* DSA 的域参数}
  private
    FQ: TCnBigNumber;
    FP: TCnBigNumber;
    FG: TCnBigNumber;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent                  - 欲从之赋值的源对象

       返回值：（无）
    }

    property P: TCnBigNumber read FP;
    property Q: TCnBigNumber read FQ;
    property G: TCnBigNumber read FG;
  end;

  TCnDSAPrivateKey = class(TCnBigNumber);
  {* DSA 的私钥，计算次数 X 次}

  TCnDSAPublicKey = class(TCnBigNumber);
  {* DSA 的公钥，Y = G 的 X 次方 mod P}

  TCnDSASignature = class(TPersistent)
  {* DSA 的签名，两个大数 R S}
  private
    FS: TCnBigNumber;
    FR: TCnBigNumber;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent                  - 欲从之赋值的源对象

       返回值：（无）
    }

    property R: TCnBigNumber read FR;
    {* 签名 R 值}
    property S: TCnBigNumber read FS;
    {* 签名 S 值}
  end;

function CnDSAGenerateParameter(OutParameter: TCnDSADomainParameter;
  PrimeType: TCnDSAPrimeType = dptBit1024160): Boolean;
{* 生成 DSA 域参数，包括有限域素数 P，子群的阶 Q，及生成元 G，返回生成是否成功。
   PrimeType 参数可指定特定的 P 和 Q 的位数。

   参数：
     OutParameter: TCnDSADomainParameter  - DSA 域参数
     PrimeType: TCnDSAPrimeType           - 指定的素数位长度

   返回值：Boolean                        - 返回生成是否成功
}

function CnDSAVerifyParameter(DSAParameter: TCnDSADomainParameter): Boolean;
{* 校验 DSA 域参数是否合法，包括判断有限域素数 P，子群的阶 Q，及生成元 G 等，返回是否合法。

   参数：
     DSAParameter: TCnDSADomainParameter  - 待校验的 DSA 域参数

   返回值：Boolean                        - 返回是否合法
}

function CnDSAGenerateKeys(DSAParameter: TCnDSADomainParameter;
  OutPrivateKey: TCnDSAPrivateKey; OutPublicKey: TCnDSAPublicKey): Boolean;
{* 在指定的 DSA 域参数下，生成一对 DSA 公私钥，返回生成是否成功。

   参数：
     DSAParameter: TCnDSADomainParameter  - DSA 域参数
     OutPrivateKey: TCnDSAPrivateKey      - 生成的 DSA 私钥
     OutPublicKey: TCnDSAPublicKey        - 生成的 DSA 公钥

   返回值：Boolean                        - 返回生成是否成功
}

function CnDSAVerifyKeys(DSAParameter: TCnDSADomainParameter;
  PrivateKey: TCnDSAPrivateKey; PublicKey: TCnDSAPublicKey): Boolean;
{* 在指定的 DSA 域参数下校验一对 DSA 公私钥，返回校验是否成功。

   参数：
     DSAParameter: TCnDSADomainParameter  - DSA 域参数
     PrivateKey: TCnDSAPrivateKey         - 待校验的 DSA 私钥
     PublicKey: TCnDSAPublicKey           - 待校验的 DSA 公钥

   返回值：Boolean                        - 返回校验是否成功
}

function CnDSASignData(Data: Pointer; DataByteLen: Integer;
  DSAParameter: TCnDSADomainParameter; PrivateKey: TCnDSAPrivateKey;
  OutSignature: TCnDSASignature; HashType: TCnDSAHashType = dhtAuto): Boolean;
{* 在指定的 DSA 域参数下，用指定 DSA 私钥及杂凑算法，对内存块数据进行签名。
   返回签名是否成功。杂凑算法类型如不传，默认 dhtAuto，表示根据 DSA 域参数中的 Q 的位数自动匹配。

   参数：
     Data: Pointer                        - 待签名的数据块地址
     DataByteLen: Integer                 - 待签名的数据块字节长度
     DSAParameter: TCnDSADomainParameter  - DSA 域参数
     PrivateKey: TCnDSAPrivateKey         - DSA 私钥
     OutSignature: TCnDSASignature        - 输出的 DSA 签名值
     HashType: TCnDSAHashType             - 杂凑算法

   返回值：Boolean                        - 返回签名是否成功
}

function CnDSAVerifyData(Data: Pointer; DataByteLen: Integer;
  DSAParameter: TCnDSADomainParameter; PublicKey: TCnDSAPublicKey;
  Signature: TCnDSASignature; HashType: TCnDSAHashType = dhtAuto): Boolean;
{* 在指定的 DSA 域参数下，用指定 DSA 公钥及杂凑算法，对内存块数据进行签名验证。
   返回验证是否成功。杂凑算法类型如不传，默认 dhtAuto，表示根据 DSA 域参数中的 Q 的位数自动匹配。

   参数：
     Data: Pointer                        - 待验证的数据块地址
     DataByteLen: Integer                 - 待验证的数据块字节长度
     DSAParameter: TCnDSADomainParameter  - DSA 域参数
     PublicKey: TCnDSAPublicKey           - DSA 公钥
     Signature: TCnDSASignature           - 待验证的 DSA 签名值
     HashType: TCnDSAHashType             - 杂凑算法

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnDSASignBytes(Data: TBytes; DSAParameter: TCnDSADomainParameter;
  PrivateKey: TCnDSAPrivateKey; OutSignature: TCnDSASignature;
  HashType: TCnDSAHashType = dhtAuto): Boolean;
{* 在指定的 DSA 域参数下，用指定 DSA 私钥及杂凑算法，对字节数组进行签名。
   返回签名是否成功。杂凑算法类型如不传，默认 dhtAuto，表示根据 DSA 域参数中的 Q 的位数自动匹配。

   参数：
     Data: TBytes                         - 待签名的字节数组
     DSAParameter: TCnDSADomainParameter  - DSA 域参数
     PrivateKey: TCnDSAPrivateKey         - DSA 私钥
     OutSignature: TCnDSASignature        - 输出的 DSA 签名值
     HashType: TCnDSAHashType             - 杂凑算法

   返回值：Boolean                        - 返回签名是否成功
}

function CnDSAVerifyBytes(Data: TBytes; DSAParameter: TCnDSADomainParameter;
  PublicKey: TCnDSAPublicKey; Signature: TCnDSASignature;
  HashType: TCnDSAHashType = dhtAuto): Boolean;
{* 在指定的 DSA 域参数下，用指定 DSA 公钥及杂凑算法，对字节数组进行签名验证。
   返回验证是否成功。杂凑算法类型如不传，默认 dhtAuto，表示根据 DSA 域参数中的 Q 的位数自动匹配。

   参数：
     Data: TBytes                         - 待验证的字节数组
     DSAParameter: TCnDSADomainParameter  - DSA 域参数
     PublicKey: TCnDSAPublicKey           - DSA 公钥
     Signature: TCnDSASignature           - 待验证的 DSA 签名值
     HashType: TCnDSAHashType             - 杂凑算法

   返回值：Boolean                        - 返回验证签名是否成功
}

implementation

{ TCnDSADomainParameters }

procedure TCnDSADomainParameter.Assign(Source: TPersistent);
begin
  if Source is TCnDSADomainParameter then
  begin
    BigNumberCopy(FP, TCnDSADomainParameter(Source).P);
    BigNumberCopy(FQ, TCnDSADomainParameter(Source).Q);
    BigNumberCopy(FG, TCnDSADomainParameter(Source).G);
  end
  else
    inherited;
end;

constructor TCnDSADomainParameter.Create;
begin
  inherited;
  FP := TCnBigNumber.Create;
  FQ := TCnBigNumber.Create;
  FG := TCnBigNumber.Create;
end;

destructor TCnDSADomainParameter.Destroy;
begin
  FG.Free;
  FQ.Free;
  FP.Free;
  inherited;
end;

{ TCnDSASignature }

procedure TCnDSASignature.Assign(Source: TPersistent);
begin
  if Source is TCnDSASignature then
  begin
    BigNumberCopy(FS, TCnDSASignature(Source).S);
    BigNumberCopy(FR, TCnDSASignature(Source).R);
  end
  else
    inherited;
end;

constructor TCnDSASignature.Create;
begin
  inherited;
  FR := TCnBigNumber.Create;
  FS := TCnBigNumber.Create;
end;

destructor TCnDSASignature.Destroy;
begin
  FS.Free;
  FR.Free;
  inherited;
end;

// 根据 DSA 参数类型或杂凑类型计算杂凑值，值放 OutDigest 中
function DSAHashData(Data: Pointer; DataByteLen: Integer; OutDigest: TCnBigNumber;
  Parameter: TCnDSADomainParameter; HashType: TCnDSAHashType = dhtAuto): Boolean;
var
  MD5Dig: TCnMD5Digest;
  SHA1Dig: TCnSHA1Digest;
  SHA224Dig: TCnSHA224Digest;
  SHA256Dig: TCnSHA256Digest;
  SM3Dig: TCnSM3Digest;
begin
  Result := False;
  case HashType of
    dhtAuto:
      begin
        if Parameter <> nil then
        begin
          case Parameter.Q.GetBitsCount of
            160:
              begin
                SHA1Dig := SHA1(PAnsiChar(Data), DataByteLen);
                OutDigest.SetBinary(@SHA1Dig[0], SizeOf(TCnSHA1Digest));
              end;
            224:
              begin
                SHA224Dig := SHA224(PAnsiChar(Data), DataByteLen);
                OutDigest.SetBinary(@SHA224Dig[0], SizeOf(TCnSHA224Digest));
              end;
            256:
              begin
                SHA256Dig := SHA256(PAnsiChar(Data), DataByteLen);
                OutDigest.SetBinary(@SHA256Dig[0], SizeOf(TCnSHA256Digest));
              end;
          else
            Exit;
          end;
        end;
      end;
    dhtMD5:
      begin
        MD5Dig := MD5(PAnsiChar(Data), DataByteLen);
        OutDigest.SetBinary(@MD5Dig[0], SizeOf(TCnMD5Digest));
      end;
    dhtSHA1:
      begin
        SHA1Dig := SHA1(PAnsiChar(Data), DataByteLen);
        OutDigest.SetBinary(@SHA1Dig[0], SizeOf(TCnSHA1Digest));
      end;
    dhtSHA224:
      begin
        SHA224Dig := SHA224(PAnsiChar(Data), DataByteLen);
        OutDigest.SetBinary(@SHA224Dig[0], SizeOf(TCnSHA224Digest));
      end;
    dhtSHA256:
      begin
        SHA256Dig := SHA256(PAnsiChar(Data), DataByteLen);
        OutDigest.SetBinary(@SHA256Dig[0], SizeOf(TCnSHA256Digest));
      end;
    dhtSM3:
      begin
        SM3Dig := SM3(PAnsiChar(Data), DataByteLen);
        OutDigest.SetBinary(@SM3Dig[0], SizeOf(TCnSM3Digest));
      end;
  else
    Exit;
  end;

  Result := True;
end;

function CnDSAGenerateParameter(OutParameter: TCnDSADomainParameter;
  PrimeType: TCnDSAPrimeType = dptBit1024160): Boolean;
var
  PB, QB, KV: Integer;
  K, H: TCnBigNumber;
begin
  Result := False;

  // 先生成位数较少的 Q，然后随机取 K 乘 Q 再减 1 来判断位数及素性，符合位数及是素数则为 P
  case PrimeType of
    dptBit1024160:
      begin
        PB := 1024;
        QB := 160;
      end;
    dptBit2048224:
      begin
        PB := 2048;
        QB := 224;
      end;
    dptBit2048256:
      begin
        PB := 2048;
        QB := 256;
      end;
    dptBit3072256:
      begin
        PB := 3072;
        QB := 256;
      end;
  else
    Exit;
  end;

  KV := PB - QB; // K 大概需要这么多位做乘积
  K := nil;
  H := nil;

  try
    K := TCnBigNumber.Create;
    repeat
      // 生成指定较短位数的素数 Q
      if not BigNumberGeneratePrimeByBitsCount(OutParameter.Q, QB) then Exit;

      // 随机取 K
      if not BigNumberRandBits(K, KV) then Exit;

      // 乘积并加一算 P
      if not BigNumberMul(OutParameter.P, K, OutParameter.Q) then Exit;

      // TODO: 检查 P - 1 必须有大素数因子以抗攻击

      if not BigNumberAddWord(OutParameter.P, 1) then Exit;

      // 最后检查位数与素性
    until (OutParameter.P.GetBitsCount = PB) and BigNumberIsProbablyPrime(OutParameter.P);

    // 拿到合法的 P 了，计算生成元 G，此时 K 的值是 (P - 1)/Q
    H := TCnBigNumber.Create;
    repeat
      // 随机取 H > 1 且 < P - 1
      if not BigNumberRandBits(H, PB) then Exit;
      if BigNumberCompare(H, OutParameter.P) >= 0 then
        if not BigNumberSub(H, H, OutParameter.P) then Exit;

      if H.IsZero or H.IsOne then
        Continue;

      // 计算 H^K mod P，如果非 1，则就是生成元
      if not BigNumberPowerMod(OutParameter.G, H, K, OutParameter.P) then Exit;
    until not OutParameter.G.IsOne;

    Result := True;
  finally
    H.Free;
    K.Free;
  end;
end;

function CnDSAVerifyParameter(DSAParameter: TCnDSADomainParameter): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  if DSAParameter.P.IsNegative or DSAParameter.Q.IsNegative
    or DSAParameter.G.IsNegative then
    Exit;

  // 俩都得是素数
  if not BigNumberIsProbablyPrime(DSAParameter.P) then Exit;
  if not BigNumberIsProbablyPrime(DSAParameter.Q) then Exit;

  // G 不能比 P - 1 大
  if BigNumberCompare(DSAParameter.G, DSAParameter.P) >= 0 then Exit;

  T := TCnBigNumber.Create;
  try
    // G 得比 2 大
    T.SetWord(2);
    if BigNumberCompare(DSAParameter.G, T) <= 0 then Exit;

    // P - 1 要能整除 Q
    BigNumberCopy(T, DSAParameter.P);
    T.SubWord(1);
    if not BigNumberMod(T, T, DSAParameter.Q) then Exit;

    if not T.IsZero then
      Exit;

    // G 得是生成元，也就是 G^Q mod P 得 = 1
    if not BigNumberPowerMod(T, DSAParameter.G, DSAParameter.Q, DSAParameter.P) then Exit;
    Result := T.IsOne;
  finally
    T.Free;
  end;
end;

function CnDSAGenerateKeys(DSAParameter: TCnDSADomainParameter;
  OutPrivateKey: TCnDSAPrivateKey; OutPublicKey: TCnDSAPublicKey): Boolean;
begin
  Result := False;
  repeat
    if not BigNumberRandRange(OutPrivateKey, DSAParameter.Q) then
      Exit;
  until not OutPrivateKey.IsZero and not OutPrivateKey.IsOne;

  Result := BigNumberPowerMod(OutPublicKey, DSAParameter.G, OutPrivateKey, DSAParameter.P);
end;

function CnDSAVerifyKeys(DSAParameter: TCnDSADomainParameter;
  PrivateKey: TCnDSAPrivateKey; PublicKey: TCnDSAPublicKey): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  if PrivateKey.IsNegative or PrivateKey.IsZero then
    Exit;

  T := TCnBigNumber.Create;
  try
    if BigNumberPowerMod(T, DSAParameter.G, PrivateKey, DSAParameter.P) then
      Result := BigNumberEqual(T, PublicKey);
  finally
    T.Free;
  end;
end;

function DSASignHashData(Hash: TCnBigNumber; DSAParameter: TCnDSADomainParameter;
  PrivateKey: TCnDSAPrivateKey; OutSignature: TCnDSASignature): Boolean;
var
  K, KInv, T: TCnBigNumber;
begin
  Result := False;

  K := nil;
  KInv := nil;
  T := nil;

  try
    K := TCnBigNumber.Create;
    repeat
      if not BigNumberRandRange(K, DSAParameter.Q) then Exit;

      if K.IsZero then
        K.SetOne;

      // r = (g^k mod p) mod q
      if not BigNumberPowerMod(OutSignature.R, DSAParameter.G, K, DSAParameter.P) then Exit;
      if not BigNumberMod(OutSignature.R, OutSignature.R, DSAParameter.Q) then Exit;

      if OutSignature.R.IsZero then
        Continue;

      // 准备好 K 的模逆元
      KInv := TCnBigNumber.Create;
      if not BigNumberModularInverse(KInv, K, DSAParameter.Q) then Exit;

      // T = Hash + 私钥 * R
      T := TCnBigNumber.Create;
      if not BigNumberMul(T, PrivateKey, OutSignature.R) then Exit;
      if not BigNumberAdd(T, Hash, T) then Exit;

      // 再乘以 K 模拟元再 mod Q
      if not BigNumberDirectMulMod(OutSignature.S, KInv, T, DSAParameter.Q) then Exit;
      if OutSignature.S.IsZero then
        Continue;

      Result := True;
      Exit;
    until False;
  finally
    T.Free;
    KInv.Free;
    K.Free;
  end;
end;

function DSAVerifyHashData(Hash: TCnBigNumber; DSAParameter: TCnDSADomainParameter;
  PublicKey: TCnDSAPublicKey; Signature: TCnDSASignature): Boolean;
var
  W, U1, U2, P1, P2: TCnBigNumber;
begin
  Result := False;

  W := nil;
  U1 := nil;
  U2 := nil;
  P1 := nil;
  P2 := nil;

  try
    W := TCnBigNumber.Create;
    // S 的模拟元搁入 W
    if not BigNumberModularInverse(W, Signature.S, DSAParameter.Q) then Exit;

    U1 := TCnBigNumber.Create;
    U2 := TCnBigNumber.Create;

    // 计算 Hash * W mod Q
    if not BigNumberDirectMulMod(U1, Hash, W, DSAParameter.Q) then Exit;
    // 计算 R * W mod Q
    if not BigNumberDirectMulMod(U2, Signature.R, W, DSAParameter.Q) then Exit;

    // 计算 G^U1 mod P
    P1 := TCnBigNumber.Create;
    if not BigNumberPowerMod(P1, DSAParameter.G, U1, DSAParameter.P) then Exit;
    // 计算 公钥^U2 mod P
    P2 := TCnBigNumber.Create;
    if not BigNumberPowerMod(P2, PublicKey, U2, DSAParameter.P) then Exit;

    // 两者相乘再 mod P 再 mod Q，放入 W
    if not BigNumberDirectMulMod(W, P1, P2, DSAParameter.P) then Exit;
    if not BigNumberMod(W, W, DSAParameter.Q) then Exit;

    // 结果比对 W 和 R
    Result := BigNumberEqual(W, Signature.R);
  finally
    P2.Free;
    P1.Free;
    U2.Free;
    U1.Free;
    W.Free;
  end;
end;

function CnDSASignData(Data: Pointer; DataByteLen: Integer;
  DSAParameter: TCnDSADomainParameter; PrivateKey: TCnDSAPrivateKey;
  OutSignature: TCnDSASignature; HashType: TCnDSAHashType = dhtAuto): Boolean;
var
  Dig: TCnBigNumber;
begin
  Result := False;
  Dig := TCnBigNumber.Create;
  try
    if DSAHashData(Data, DataByteLen, Dig, DSAParameter, HashType) then
      Result := DSASignHashData(Dig, DSAParameter, PrivateKey, OutSignature);
  finally
    Dig.Free;
  end;
end;

function CnDSAVerifyData(Data: Pointer; DataByteLen: Integer;
  DSAParameter: TCnDSADomainParameter; PublicKey: TCnDSAPublicKey;
  Signature: TCnDSASignature; HashType: TCnDSAHashType = dhtAuto): Boolean;
var
  Dig: TCnBigNumber;
begin
  Result := False;
  Dig := TCnBigNumber.Create;
  try
    if DSAHashData(Data, DataByteLen, Dig, DSAParameter, HashType) then
      Result := DSAVerifyHashData(Dig, DSAParameter, PublicKey, Signature);
  finally
    Dig.Free;
  end;
end;

function CnDSASignBytes(Data: TBytes; DSAParameter: TCnDSADomainParameter;
  PrivateKey: TCnDSAPrivateKey; OutSignature: TCnDSASignature;
  HashType: TCnDSAHashType = dhtAuto): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnDSASignData(nil, 0, DSAParameter, PrivateKey,
      OutSignature, HashType)
  else
    Result := CnDSASignData(@Data[0], Length(Data), DSAParameter, PrivateKey,
      OutSignature, HashType);
end;

function CnDSAVerifyBytes(Data: TBytes; DSAParameter: TCnDSADomainParameter;
  PublicKey: TCnDSAPublicKey; Signature: TCnDSASignature;
  HashType: TCnDSAHashType = dhtAuto): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnDSAVerifyData(nil, 0, DSAParameter, PublicKey,
      Signature, HashType)
  else
    Result := CnDSAVerifyData(@Data[0], Length(Data), DSAParameter, PublicKey,
      Signature, HashType);
end;

end.
