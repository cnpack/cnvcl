{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnRSA;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：RSA 算法单元
* 单元作者：刘啸
* 备    注：包括 Int64 范围内的 RSA 算法以及大数算法，公钥 Exponent 固定使用 65537。
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2018.06.15 V1.5
*               支持文件签名与验证，类似于 Openssl 中的用法，有原始签名与散列签名两类：
*               openssl rsautl -sign -in hello -inkey rsa.pem -out hello.default.sign.openssl
*               // 私钥原始签名，直接把文件内容补齐后用私钥加密并存储，等同于加密，对应 CnRSASignFile 指定 sdtNone
*               openssl dgst -md5 -sign rsa.pem -out hello.md5.sign.openssl hello
*               openssl dgst -sha1 -sign rsa.pem -out hello.sha1.sign.openssl hello
*               openssl dgst -sha256 -sign rsa.pem -out hello.sha256.sign.openssl hello
*               // 私钥散列签名，可指定散列算法，默认 md5。对应 CnRSASignFile 并指定散列算法。
*               // 原始文件散列值经过 BER 编码再 PKCS1 补齐后私钥加密并存储成签名文件
*               openssl dgst -verify rsa_pub.pem -signature hello.sign.openssl hello
*               // 公钥散列验证原始文件与签名文件，散列算法类型在签名文件中。
*               // 对应 CnRSAVerify，公钥解开签名文件后去除 PKCS1 对齐再解开 BER 编码并比对散列值
*           2018.06.14 V1.5
*               支持文件加解密，类似于 Openssl 中的用法，如：
*               openssl rsautl -encrypt -in hello -inkey rsa_pub.pem -pubin -out hello.en.pub.openssl
*               openssl rsautl -encrypt -in hello -inkey rsa.pem -out hello.en.pub.openssl
*               // 用公钥加密，等同于方法 CnRSAEncryptFile 并传入 PublicKey
*               openssl rsautl -decrypt -in hello.en.pub.openssl -inkey rsa.pem -out hello.de.priv.openssl
*               // 用私钥解密，等同于方法 CnRSADecryptFile 并传入 PrivateKey
*               注意 Openssl 提倡公钥加密私钥解密，但我们也实现了私钥加密公钥解密
*           2018.06.05 V1.4
*               将 Int64 支持扩展至 UInt64
*           2018.06.02 V1.4
*               能够将公私钥保存成兼容 Openssl 的未加密的公私钥 PEM 格式文件
*           2018.05.27 V1.3
*               能够从 Openssl 1.0.2 生成的未加密的公私钥 PEM 格式文件中读入公私钥，如
*               openssl genrsa -out private_pkcs1.pem 2048
*                  // PKCS#1 格式的公私钥
*               openssl pkcs8 -topk8 -inform PEM -in private_pkcs1.pem -outform PEM -nocrypt -out private_pkcs8.pem
*                  // PKCS#8 格式的公私钥
*               openssl rsa -in private_pkcs1.pem -outform PEM -RSAPublicKey_out -out public_pkcs1.pem
*                  // PKCS#1 格式的公钥
*               openssl rsa -in private_pkcs1.pem -outform PEM -pubout -out public_pkcs8.pem
*                  // PKCS#8 格式的公钥
*           2018.05.22 V1.2
*               将公私钥组合成对象以方便使用
*           2017.04.05 V1.1
*               实现大数的 RSA 密钥生成与加解密
*           2017.04.03 V1.0
*               创建单元，Int64 范围内的 RSA 从 CnPrimeNumber 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, CnPrimeNumber, CnBigNumber, CnBase64, CnBerUtils,
  CnNativeDecl, CnMD5, CnSHA1, CnSHA2;

const
  CN_PKCS1_BLOCK_TYPE_PRIVATE_00       = 00;
  CN_PKCS1_BLOCK_TYPE_PRIVATE_FF       = 01;
  CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM    = 02;
  {* RSA 加解密时的三类块类型字段}

  CN_RSA_PKCS1_PADDING_SIZE            = 11;

  // 以下 OID 都预先写死，不动态计算编码了
  OID_RSAENCRYPTION_PKCS1: array[0..8] of Byte = ( // 1.2.840.113549.1.1.1
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $01
  );  // $2A = 40 * 1 + 2

  OID_SIGN_MD5: array[0..7] of Byte = (            // 1.2.840.113549.2.5
    $2A, $86, $48, $86, $F7, $0D, $02, $05
  );

  OID_SIGN_SHA1: array[0..4] of Byte = (           // 1.3.14.3.2.26
    $2B, $0E, $03, $02, $1A
  );

  OID_SIGN_SHA256: array[0..8] of Byte = (         // 2.16.840.1.101.3.4.2.1
    $60, $86, $48, $01, $65, $03, $04, $02, $01
  );

type
  TCnRSASignDigestType = (sdtNone, sdtMD5, sdtSHA1, sdtSHA256);
  {* RSA 签名所支持的数字摘要算法，可无摘要}

  TCnRSAKeyType = (cktPKCS1, cktPKCS8);
  {* RSA 密钥文件格式}

  TCnRSAPrivateKey = class(TPersistent)
  {* RSA 私钥}
  private
    FPrimeKey1: TCnBigNumber;
    FPrimeKey2: TCnBigNumber;
    FPrivKeyProduct: TCnBigNumber;
    FPrivKeyExponent: TCnBigNumber;
    function GetBitsCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    property PrimeKey1: TCnBigNumber read FPrimeKey1 write FPrimeKey1;
    {* 大素数 1，p}
    property PrimeKey2: TCnBigNumber read FPrimeKey2 write FPrimeKey2;
    {* 大素数 2，q}
    property PrivKeyProduct: TCnBigNumber read FPrivKeyProduct write FPrivKeyProduct;
    {* 俩素数乘积 n，也叫 Modulus}
    property PrivKeyExponent: TCnBigNumber read FPrivKeyExponent write FPrivKeyProduct;
    {* 私钥指数 d}
    property BitsCount: Integer read GetBitsCount;
    {* 密钥的位数，也即素数乘积的有效位数}
  end;

  TCnRSAPublicKey = class(TPersistent)
  {* RSA 公钥}
  private
    FPubKeyProduct: TCnBigNumber;
    FPubKeyExponent: TCnBigNumber;
    function GetBitsCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    property PubKeyProduct: TCnBigNumber read FPubKeyProduct write FPubKeyProduct;
    {* 俩素数乘积 n，也叫 Modulus}
    property PubKeyExponent: TCnBigNumber read FPubKeyExponent write FPubKeyExponent;
    {* 公钥指数 e，65537}
    property BitsCount: Integer read GetBitsCount;
    {* 密钥的位数，也即素数乘积的有效位数}
  end;

// UInt64 范围内的 RSA 加解密实现

function Int64ExtendedEuclideanGcd(A, B: TUInt64; out X: TUInt64; out Y: TUInt64): TUInt64;
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解，
   如果得出 X 小于 0，可加上 B}

procedure Int64ExtendedEuclideanGcd2(A, B: TUInt64; out X: TUInt64; out Y: TUInt64);
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X - B * Y = 1 的整数解，
   如果得出 X 小于 0，可加上 B}

function CnInt64RSAGenerateKeys(out PrimeKey1: Cardinal; out PrimeKey2: Cardinal;
  out PrivKeyProduct: TUInt64; out PrivKeyExponent: TUInt64;
  out PubKeyProduct: TUInt64; out PubKeyExponent: TUInt64; HighBitSet: Boolean = True): Boolean;
{* 生成 RSA 算法所需的公私钥，素数均不大于 Cardinal，Keys 均不大于 UInt64
   HighBitSet 为 True 时要求素数最高位为 1，且乘积是 64 Bit}

function CnInt64RSAEncrypt(Data: TUInt64; PrivKeyProduct: TUInt64;
  PrivKeyExponent: TUInt64; out Res: TUInt64): Boolean;
{* 利用上面生成的私钥对数据进行加密，返回加密是否成功}

function CnInt64RSADecrypt(Res: TUInt64; PubKeyProduct: TUInt64;
  PubKeyExponent: TUInt64; out Data: TUInt64): Boolean;
{* 利用上面生成的公钥对数据进行解密，返回解密是否成功}

// 大数范围内的 RSA 加解密实现

function CnRSAGenerateKeysByPrimeBits(PrimeBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
{* 生成 RSA 算法所需的公私钥，PrimeBits 是素数的二进制位数，其余参数均为生成。
   PrimeBits 取值为 512/1024/2048等，注意目前不是乘积的范围}

function CnRSAGenerateKeys(ModulusBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
{* 生成 RSA 算法所需的公私钥，ModulusBits 是素数乘积的二进制位数，其余参数均为生成。
   ModulusBits 取值为 512/1024/2048等}

function CnRSALoadKeysFromPem(const PemFileName: string;
  PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
{* 从 PEM 格式文件中加载公私钥数据，如某钥参数为空则不载入}

function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS1): Boolean;
{* 将公私钥写入 PEM 格式文件中，返回是否成功}

function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
{* 从 PEM 格式文件中加载公钥数据，返回是否成功}

function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS8): BOolean;
{* 将公钥写入 PEM 格式文件中，返回是否成功}

function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
{* 利用上面生成的私钥对数据进行加密，返回加密是否成功}

function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
{* 利用上面生成的公钥对数据进行解密，返回解密是否成功}

// RSA 数据与文件加解密实现

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对数据块进行加密，加密前使用 PKCS1 填充，结果放 OutBuf 中，
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对数据块进行加密，加密前使用 PKCS1 填充，结果放 OutBuf 中，
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对数据块进行解密，并解开 PKCS1 填充，结果放 OutBuf 中，并返回数据长度
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对数据块进行解密，并解开其 PKCS1 填充，结果放 OutBuf 中，并返回数据长度
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对文件进行加密，加密前使用 PKCS1 填充，结果存输出文件中}

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对文件进行加密，加密前使用 PKCS1 填充，结果存输出文件中}

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对文件进行解密，并解开其 PKCS1 填充，结果存输出文件中}

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对文件进行解密，并解开其 PKCS1 填充，结果存输出文件中}

// RSA 文件签名与验证实现

function CnRSASignFile(const InFileName, OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean;
{* 用私钥签名指定文件。
   未指定数字摘要算法时等于将源文件用 PKCS1 Private_FF 补齐后加密
   当指定了数字摘要算法时，使用指定数字摘要算法对文件进行计算得到散列值，
   原始的二进制散列值进行 BER 编码再 PKCS1 补齐再用私钥加密}

function CnRSAVerifyFile(const InFileName, InSignFileName: string;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = sdtMD5): Boolean; overload;
{* 用公钥与签名值验证指定文件，也即用指定数字摘要算法对文件进行计算得到散列值，
   并用公钥解密签名内容并解开 PKCS1 补齐再解开 BER 编码得到散列算法与散列值，
   并比对两个二进制散列值是否相同，返回验证是否通过}

// 其他辅助函数

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream): Boolean;
{* 从 PEM 格式编码的文件中验证指定头尾后读入实际内容并进行 Base64 解码}

function GetDigestSignTypeFromBerOID(OID: Pointer; OidLen: Integer): TCnRSASignDigestType;
{* 从 BER 解析出的 OID 获取其对应的散列摘要类型}

implementation

const
  // PKCS#1
  PEM_RSA_PRIVATE_HEAD = '-----BEGIN RSA PRIVATE KEY-----';
  PEM_RSA_PRIVATE_TAIL = '-----END RSA PRIVATE KEY-----';

  PEM_RSA_PUBLIC_HEAD = '-----BEGIN RSA PUBLIC KEY-----';
  PEM_RSA_PUBLIC_TAIL = '-----END RSA PUBLIC KEY-----';

  // PKCS#8
  PEM_PRIVATE_HEAD = '-----BEGIN PRIVATE KEY-----';
  PEM_PRIVATE_TAIL = '-----END PRIVATE KEY-----';

  PEM_PUBLIC_HEAD = '-----BEGIN PUBLIC KEY-----';
  PEM_PUBLIC_TAIL = '-----END PUBLIC KEY-----';

// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分
function Int64RSACrypt(Data: TUInt64; Product: TUInt64; Exponent: TUInt64;
  out Res: TUInt64): Boolean;
begin
  Res := MontgomeryPowerMod(Data, Exponent, Product);
  Result := True;
end;

// 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解
function Int64ExtendedEuclideanGcd(A, B: TUInt64; out X: TUInt64; out Y: TUInt64): TUInt64;
var
  R, T: TUInt64;
begin
  if B = 0 then
  begin
    X := 1;
    Y := 0;
    Result := A;
  end
  else
  begin
    R := Int64ExtendedEuclideanGcd(B, UInt64Mod(A, B), X, Y);
    T := X;
    X := Y;
    Y := T - UInt64Div(A, B) * Y;
    Result := R;
  end;
end;

// 扩展欧几里得辗转相除法求二元一次不定方程 A * X - B * Y = 1 的整数解
procedure Int64ExtendedEuclideanGcd2(A, B: TUInt64; out X: TUInt64; out Y: TUInt64);
begin
  if B = 0 then
  begin
    X := 1;
    Y := 0;
  end
  else
  begin
    Int64ExtendedEuclideanGcd2(B, UInt64Mod(A, B), Y, X);
    Y := Y - X * UInt64Div(A, B);
  end;
end;

function GetInt64BitCount(A: TUInt64): Integer;
var
  I: Integer;
begin
  I := 0;
  while (A shr I) <> 0 do
    Inc(I);

  Result := I;
end;

// 生成 RSA 算法所需的公私钥，素数均不大于 Cardinal，Keys 均不大于 TUInt64
function CnInt64RSAGenerateKeys(out PrimeKey1: Cardinal; out PrimeKey2: Cardinal;
  out PrivKeyProduct: TUInt64; out PrivKeyExponent: TUInt64;
  out PubKeyProduct: TUInt64; out PubKeyExponent: TUInt64; HighBitSet: Boolean): Boolean;
var
  N: Cardinal;
  Succ: Boolean;
  Product, Y: TUInt64;
begin
  repeat
    PrimeKey1 := CnGenerateInt32Prime(HighBitSet);

    N := Trunc(Random * 1000);
    Sleep(N);

    PrimeKey2 := CnGenerateInt32Prime(HighBitSet);
    if HighBitSet then
    begin
      Product := TUInt64(PrimeKey1) * TUInt64(PrimeKey2);
      Succ := GetInt64BitCount(Product) = 64;
    end
    else
      Succ := True;
  until Succ;

  if PrimeKey2 > PrimeKey1 then  // 一般使 p > q
  begin
    N := PrimeKey1;
    PrimeKey1 := PrimeKey2;
    PrimeKey2 := N;
  end;

  PrivKeyProduct := TUInt64(PrimeKey1) * TUInt64(PrimeKey2);
  PubKeyProduct := TUInt64(PrimeKey2) * TUInt64(PrimeKey1);   // 积 n 在公私钥中是相同的
  PubKeyExponent := 65537;                                    // 固定

  Product := TUInt64(PrimeKey1 - 1) * TUInt64(PrimeKey2 - 1);

  //                      e                d             (p-1)(q-1)
  // 用辗转相除法求 PubKeyExponent * PrivKeyExponent mod Product = 1 中的 PrivKeyExponent
  // r = (p-1)(q-1) 也就是解方程 e * d + r * y = 1，其中 e、r 已知，求 d 与 y。
  Int64ExtendedEuclideanGcd(PubKeyExponent, Product, PrivKeyExponent, Y);
  while PrivKeyExponent < 0 do
  begin
     // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上 r，加到大于零为止
     PrivKeyExponent := PrivKeyExponent + Product;
  end;
  Result := True;
end;

// 利用上面生成的私钥对数据进行加密，返回加密是否成功
function CnInt64RSAEncrypt(Data: TUInt64; PrivKeyProduct: TUInt64;
  PrivKeyExponent: TUInt64; out Res: TUInt64): Boolean;
begin
  Result := Int64RSACrypt(Data, PrivKeyProduct, PrivKeyExponent, Res);
end;

// 利用上面生成的公钥对数据进行解密，返回解密是否成功
function CnInt64RSADecrypt(Res: TUInt64; PubKeyProduct: TUInt64;
  PubKeyExponent: TUInt64; out Data: TUInt64): Boolean;
begin
  Result := Int64RSACrypt(Res, PubKeyProduct, PubKeyExponent, Data);
end;

function CnRSAGenerateKeysByPrimeBits(PrimeBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  N: Integer;
  Suc: Boolean;
  R, Y, Rem, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  if PrimeBits <= 16 then
    Exit;

  PrivateKey.Clear;
  PublicKey.Clear;

  Suc := False;
  while not Suc do
  begin
    if not BigNumberGeneratePrime(PrivateKey.PrimeKey1, PrimeBits div 8) then
      Exit;

    N := Trunc(Random * 1000);
    Sleep(N);

    if not BigNumberGeneratePrime(PrivateKey.PrimeKey2, PrimeBits div 8) then
      Exit;

    // TODO: p 和 q 的差不能过小，不满足时得 Continue

    // 一般要求 Prime1 > Prime2 以便计算 CRT 等参数
    if BigNumberCompare(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) < 0 then
      BigNumberSwap(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2);

    if not BigNumberMul(PrivateKey.PrivKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    // p、q 的积是否满足 Bit 数，不满足时得 Continue
    if PrivateKey.PrivKeyProduct.GetBitsCount <> PrimeBits * 2 then
      Continue;

    // TODO: pq 的积的 NAF 系数是否满足条件，不满足时得 Continue

    if not BigNumberMul(PublicKey.PubKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    PublicKey.PubKeyExponent.SetDec('65537');

    Rem := nil;
    Y := nil;
    R := nil;
    S1 := nil;
    S2 := nil;
    One := nil;

    try
      Rem := TCnBigNumber.Create;
      Y := TCnBigNumber.Create;
      R := TCnBigNumber.Create;
      S1 := TCnBigNumber.Create;
      S2 := TCnBigNumber.Create;
      One := TCnBigNumber.Create;

      BigNumberSetOne(One);
      BigNumberSub(S1, PrivateKey.PrimeKey1, One);
      BigNumberSub(S2, PrivateKey.PrimeKey2, One);
      BigNumberMul(R, S1, S2);     // 计算积二，R = (p - 1) * (q - 1)

      // 求 e 也就是 PubKeyExponent（65537）针对积二 R 的模反元素 d 也就是 PrivKeyExponent
      BigNumberExtendedEuclideanGcd(PublicKey.PubKeyExponent, R, PrivateKey.PrivKeyExponent, Y);

      // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上积二 R
      if BigNumberIsNegative(PrivateKey.PrivKeyExponent) then
         BigNumberAdd(PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyExponent, R);

      // TODO: d 不能太小，不满足时得 Continue
    finally
      One.Free;
      S2.Free;
      S1.Free;
      R.Free;
      Y.Free;
      Rem.Free;
    end;

    Suc := True;
  end;
  Result := True;
end;

function CnRSAGenerateKeys(ModulusBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  N, PB1, PB2, MinDB, MinW: Integer;
  Suc: Boolean;
  R, Y, Rem, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  if ModulusBits < 128 then
    Exit;

  PrivateKey.Clear;
  PublicKey.Clear;
  Suc := False;

  PB1 := (ModulusBits + 1) div 2;
  PB2 := ModulusBits - PB1;
  MinDB := ModulusBits div 2 - 100;
  if MinDB < ModulusBits div 3 then
    MinDB := ModulusBits div 3;
  MinW := ModulusBits shr 2;

  while not Suc do
  begin
    if not BigNumberGeneratePrimeByBitsCount(PrivateKey.PrimeKey1, PB1) then
      Exit;

    N := Trunc(Random * 1000);
    Sleep(N);

    if not BigNumberGeneratePrimeByBitsCount(PrivateKey.PrimeKey2, PB2) then
      Exit;

    // TODO: p 和 q 的差不能过小，不满足时得 Continue

    // 一般要求 Prime1 > Prime2 以便计算 CRT 等参数
    if BigNumberCompare(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) < 0 then
      BigNumberSwap(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2);

    if not BigNumberMul(PrivateKey.PrivKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    // p、q 的积是否满足 Bit 数，不满足时得 Continue
    if PrivateKey.PrivKeyProduct.GetBitsCount <> ModulusBits then
      Continue;

    // TODO: pq 的积的 NAF 系数是否满足条件，不满足时得 Continue

    if not BigNumberMul(PublicKey.PubKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Exit;

    PublicKey.PubKeyExponent.SetDec('65537');

    Rem := nil;
    Y := nil;
    R := nil;
    S1 := nil;
    S2 := nil;
    One := nil;

    try
      Rem := TCnBigNumber.Create;
      Y := TCnBigNumber.Create;
      R := TCnBigNumber.Create;
      S1 := TCnBigNumber.Create;
      S2 := TCnBigNumber.Create;
      One := TCnBigNumber.Create;

      BigNumberSetOne(One);
      BigNumberSub(S1, PrivateKey.PrimeKey1, One);
      BigNumberSub(S2, PrivateKey.PrimeKey2, One);
      BigNumberMul(R, S1, S2);     // 计算积二，R = (p - 1) * (q - 1)

      // 求 e 也就是 PubKeyExponent（65537）针对积二 R 的模反元素 d 也就是 PrivKeyExponent
      BigNumberExtendedEuclideanGcd(PublicKey.PubKeyExponent, R, PrivateKey.PrivKeyExponent, Y);

      // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上积二 R
      if BigNumberIsNegative(PrivateKey.PrivKeyExponent) then
         BigNumberAdd(PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyExponent, R);

      // TODO: d 不能太小，不满足时得 Continue
    finally
      One.Free;
      S2.Free;
      S1.Free;
      R.Free;
      Y.Free;
      Rem.Free;
    end;

    Suc := True;
  end;
  Result := True;
end;

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream): Boolean;
var
  I: Integer;
  S: string;
  Sl: TStringList;
begin
  Result := False;

  if (ExpectHead <> '') and (ExpectTail <> '') then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromFile(FileName);
      if Sl.Count > 2 then
      begin
        if Trim(Sl[0]) <> ExpectHead then
          Exit;

        if Trim(Sl[Sl.Count - 1]) = '' then
          Sl.Delete(Sl.Count - 1);

        if Trim(Sl[Sl.Count - 1]) <> ExpectTail then
          Exit;

        Sl.Delete(Sl.Count - 1);
        Sl.Delete(0);
        S := '';
        for I := 0 to Sl.Count - 1 do
          S := S + Sl[I];

        // To De Base64 S
        MemoryStream.Clear;
        Result := (BASE64_OK = Base64Decode(S, MemoryStream));
      end;
    finally
      Sl.Free;
    end;
  end;
end;

procedure PutIndexedBigIntegerToBigInt(Node: TCnBerReadNode; BigNumber: TCnBigNumber);
var
  P: Pointer;
begin
  if (Node = nil) or (Node.BerDataLength <= 0) then
    Exit;

  P := GetMemory(Node.BerDataLength);
  Node.CopyDataTo(P);
  BigNumber.SetBinary(P, Node.BerDataLength);
  FreeMemory(P);
end;

// 从 PEM 格式文件中加载公私钥数据
(*
PKCS#1:
  RSAPrivateKey ::= SEQUENCE {                        0
    version Version,                                  1 0
    modulus INTEGER, – n                             2 公私钥
    publicExponent INTEGER, – e                      3 公钥
    privateExponent INTEGER, – d                     4 私钥
    prime1 INTEGER, – p                              5 私钥
    prime2 INTEGER, – q                              6 私钥
    exponent1 INTEGER, – d mod (p-1)                 7 CRT 系数 1
    exponent2 INTEGER, – d mod (q-1)                 8 CRT 系数 2
    coefficient INTEGER, – (1/q) mod p               9 CRT 系数 3：q 针对 p 的模逆元
    otherPrimeInfos OtherPrimeInfos OPTIONAL          10

    模逆元 x = (1/q) mod p 可得 xq = 1 mod p 也即 xq = 1 + yp 也就是 qx + (-p)y = 1
    可以用扩展欧几里得辗转相除法直接求解
  }

PKCS#8:
  PrivateKeyInfo ::= SEQUENCE {
    version         Version,
    algorithm       AlgorithmIdentifier,
    PrivateKey      OCTET STRING
  }

  AlgorithmIdentifier ::= SEQUENCE {
    algorithm       OBJECT IDENTIFIER,
    parameters      ANY DEFINED BY algorithm OPTIONAL
  }
  PrivateKey 是上面 PKCS#1 的 RSAPrivateKey 结构
  也即：
  SEQUENCE (3 elem)
    INTEGER 0
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    OCTET STRING (1 elem)
      SEQUENCE (9 elem)
        INTEGER 0
        INTEGER                                       8 公私钥 Modulus
        INTEGER                                       9 公钥   e
        INTEGER                                       10 私钥  d
        INTEGER                                       11 私钥  p
        INTEGER                                       12 私钥  q
        INTEGER
        INTEGER
        INTEGER
*)
function CnRSALoadKeysFromPem(const PemFileName: string;
  PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
var
  MemStream: TMemoryStream;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  MemStream := nil;
  Reader := nil;

  try
    MemStream := TMemoryStream.Create;
    if LoadPemFileToMemory(PemFileName, PEM_RSA_PRIVATE_HEAD, PEM_RSA_PRIVATE_TAIL, MemStream) then
    begin
      // 读 PKCS#1 的 PEM 公私钥格式
      Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size);
      if Reader.TotalCount >= 8 then
      begin
        Node := Reader.Items[1]; // 0 是整个 Sequence，1 是 Version
        if Node.AsByte = 0 then // 只支持版本 0
        begin
          // 2 和 3 整成公钥
          if PublicKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[2], PublicKey.PubKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[3], PublicKey.PubKeyExponent);
          end;

          // 2 4 5 6 整成私钥
          if PrivateKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[2], PrivateKey.PrivKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[4], PrivateKey.PrivKeyExponent);
            PutIndexedBigIntegerToBigInt(Reader.Items[5], PrivateKey.PrimeKey1);
            PutIndexedBigIntegerToBigInt(Reader.Items[6], PrivateKey.PrimeKey2);
          end;

          Result := True;
        end;
      end;
    end
    else if LoadPemFileToMemory(PemFileName, PEM_PRIVATE_HEAD, PEM_PRIVATE_TAIL, MemStream) then
    begin
      // 读 PKCS#8 的 PEM 公私钥格式
      Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
      if Reader.TotalCount >= 12 then
      begin
        Node := Reader.Items[1]; // 0 是整个 Sequence，1 是 Version
        if Node.AsByte = 0 then // 只支持版本 0
        begin
          // 8 和 9 整成公钥
          if PublicKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[8], PublicKey.PubKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[9], PublicKey.PubKeyExponent);
          end;
      
          // 8 10 11 12 整成私钥
          if PrivateKey <> nil then
          begin
            PutIndexedBigIntegerToBigInt(Reader.Items[8], PrivateKey.PrivKeyProduct);
            PutIndexedBigIntegerToBigInt(Reader.Items[10], PrivateKey.PrivKeyExponent);
            PutIndexedBigIntegerToBigInt(Reader.Items[11], PrivateKey.PrimeKey1);
            PutIndexedBigIntegerToBigInt(Reader.Items[12], PrivateKey.PrimeKey2);
          end;
      
          Result := True;
        end;
      end;
    end;
  finally
    MemStream.Free;
    Reader.Free;
  end;
end;

// 从 PEM 格式文件中加载公钥数据
// 注意 PKCS#8 的 PublicKey 的 PEM 在标准 ASN.1 上做了一层封装，
// 把 Modulus 与 Exponent 封在了 BitString 中，需要 Paser 解析出来
(*
PKCS#1:
  RSAPublicKey ::= SEQUENCE {
      modulus           INTEGER,  -- n
      publicExponent    INTEGER   -- e
  }

PKCS#8:
  PublicKeyInfo ::= SEQUENCE {
    algorithm       AlgorithmIdentifier,
    PublicKey       BIT STRING
  }

  AlgorithmIdentifier ::= SEQUENCE {
    algorithm       OBJECT IDENTIFIER,
    parameters      ANY DEFINED BY algorithm OPTIONAL
  }
  也即：
  SEQUENCE (2 elem)
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.113549.1.1.1 rsaEncryption(PKCS #1)
      NULL
    BIT STRING (1 elem)
      SEQUENCE (2 elem)
        INTEGER     - Modulus
        INTEGER     - Exponent
*)
function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  Mem: TMemoryStream;
  Reader: TCnBerReader;
begin
  Result := False;
  Mem := nil;
  Reader := nil;

  try
    Mem := TMemoryStream.Create;
    if LoadPemFileToMemory(PemFileName, PEM_PUBLIC_HEAD, PEM_PUBLIC_TAIL, Mem) then
    begin
      // 读 PKCS#8 格式的公钥
      Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size, True);
      if Reader.TotalCount >= 7 then
      begin
        // 6 和 7 整成公钥
        if PublicKey <> nil then
        begin
          PutIndexedBigIntegerToBigInt(Reader.Items[6], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigInt(Reader.Items[7], PublicKey.PubKeyExponent);
        end;

        Result := True;
      end;
    end
    else if LoadPemFileToMemory(PemFileName, PEM_RSA_PUBLIC_HEAD, PEM_RSA_PUBLIC_TAIL, Mem) then
    begin
      // 读 PKCS#1 格式的公钥
      Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size);
      if Reader.TotalCount >= 3 then
      begin
        // 1 和 2 整成公钥
        if PublicKey <> nil then
        begin
          PutIndexedBigIntegerToBigInt(Reader.Items[1], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigInt(Reader.Items[2], PublicKey.PubKeyExponent);
        end;
      
        Result := True;
      end;
    end;
  finally
    Mem.Free;
    Reader.Free;
  end;
end;

// 如果整数最高位是 1，则需要前面补 0 避免与负数的表述混淆
function CalcIntegerTLV(BigNumber: TCnBigNumber): Cardinal;
begin
  Result := BigNumber.GetBytesCount;
  if BigNumber.IsBitSet((Result * 8) - 1) then
    Inc(Result);
end;

procedure SplitStringToList(const S: string; List: TStrings);
const
  LINE_WIDTH = 64;
var
  C, R: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  if S <> '' then
  begin
    R := S;
    while R <> '' do
    begin
      C := Copy(R, 1, LINE_WIDTH);
      Delete(R, 1, LINE_WIDTH);
      List.Add(C);
    end;
  end;
end;

function AddBigNumberToWriter(Writer: TCnBerWriter; Num: TCnBigNumber;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  P: Pointer;
  C, D: Integer;
begin
  Result := nil;
  if (Writer = nil) or (Num = nil) then
    Exit;

  // Integer 编码需要处理最高位
  C := CalcIntegerTLV(Num);
  if C <= 0 then
    Exit;

  P := GetMemory(C);
  D := C - Num.GetBytesCount;
  ZeroMemory(P, D);
  Num.ToBinary(PAnsiChar(Integer(P) + D));

  Result := Writer.AddBasicNode(CN_BER_TAG_INTEGER, P, C, Parent);
  FreeMemory(P);
end;

// 将公私钥写入 PEM 格式文件中
function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  List: TStrings;
  N, T, R1, R2, X, Y : TCnBigNumber;
  S: string;
  B: Byte;
begin
  Result := False;
  if (PublicKey = nil) or (PublicKey.PubKeyProduct.GetBytesCount <= 0) or
    (PublicKey.PubKeyExponent.GetBytesCount <= 0) then
    Exit;

  if (PrivateKey = nil) or (PrivateKey.PrivKeyProduct.GetBytesCount <= 0) or
    (PrivateKey.PrivKeyExponent.GetBytesCount <= 0) then
    Exit;

  Mem := nil;
  List := nil;
  Writer := nil;
  T := nil;
  R1 := nil;
  R2 := nil;
  N := nil;
  X := nil;
  Y := nil;

  try
    T := BigNumberNew;
    R1 := BigNumberNew;
    R2 := BigNumberNew;
    N := BigNumberNew;
    X := BigNumberNew;
    Y := BigNumberNew;
    if not T.SetOne then
      Exit;

    BigNumberSub(N, PrivateKey.PrimeKey1, T);
    BigNumberMod(R1, PrivateKey.PrivKeyExponent, N); // R1 = d mod (p - 1)

    BigNumberSub(N, PrivateKey.PrimeKey2, T);
    BigNumberMod(R2, PrivateKey.PrivKeyExponent, N); // R2 = d mod (q - 1)

    // X = 是不定方程 qx + (-p)y = 1 的解
    BigNumberExtendedEuclideanGcd(PrivateKey.PrimeKey2, PrivateKey.PrimeKey1, X, Y);
    if BigNumberIsNegative(X) then
      BigNumberAdd(X, X, PrivateKey.PrimeKey1);

    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    B := 0;
    if KeyType = cktPKCS1 then
    begin
      // 拼 PKCS1 格式的内容
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyProduct, Root);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyExponent, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey1, Root);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey2, Root);
      AddBigNumberToWriter(Writer, R1, Root);
      AddBigNumberToWriter(Writer, R2, Root);
      AddBigNumberToWriter(Writer, X, Root);
    end
    else if KeyType = cktPKCS8 then
    begin
      // 拼 PKCS8 格式的内容
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

      // 给 Node1 加 ObjectIdentifier 与 Null
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
      Writer.AddNullNode(Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_OCTET_STRING, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);

      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyProduct, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrivKeyExponent, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey1, Node);
      AddBigNumberToWriter(Writer, PrivateKey.PrimeKey2, Node);
      AddBigNumberToWriter(Writer, R1, Node);
      AddBigNumberToWriter(Writer, R2, Node);
      AddBigNumberToWriter(Writer, X, Node);
    end;

    // 树搭好了，输出并 Base64 再分段再拼头尾最后写文件
    Mem := TMemoryStream.Create;
    Writer.SaveToStream(Mem);
    Mem.Position := 0;

    if Base64_OK = Base64Encode(Mem, S) then
    begin
      List := TStringList.Create;
      SplitStringToList(S, List);
      if KeyType = cktPKCS1 then
      begin
        List.Insert(0, PEM_RSA_PRIVATE_HEAD);
        List.Add(PEM_RSA_PRIVATE_TAIL);
      end
      else if KeyType = cktPKCS8 then
      begin
        List.Insert(0, PEM_PRIVATE_HEAD);
        List.Add(PEM_PRIVATE_TAIL);
      end;
      List.SaveToFile(PemFileName);
      Result := True;
    end;
  finally
    BigNumberFree(T);
    BigNumberFree(R1);
    BigNumberFree(R2);
    BigNumberFree(N);
    BigNumberFree(X);
    BigNumberFree(Y);

    Mem.Free;
    List.Free;
    Writer.Free;
  end;
end;

// 将公钥写入 PEM 格式文件中
function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  List: TStrings;
  S: string;
begin
  Result := False;
  if (PublicKey = nil) or (PublicKey.PubKeyProduct.GetBytesCount <= 0) or
    (PublicKey.PubKeyExponent.GetBytesCount <= 0) then
    Exit;

  Mem := nil;
  List := nil;
  Writer := nil;
  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    if KeyType = cktPKCS1 then
    begin
      // 拼 PKCS1 格式的内容，比较简单
      AddBigNumberToWriter(Writer, PublicKey.PubKeyProduct, Root);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Root);
    end
    else if KeyType = cktPKCS8 then
    begin
      // 拼 PKCS8 格式的内容
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

      // 给 Node 加 ObjectIdentifier 与 Null
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(OID_RSAENCRYPTION_PKCS1), Node);
      Writer.AddNullNode(Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_BIT_STRING, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyProduct, Node);
      AddBigNumberToWriter(Writer, PublicKey.PubKeyExponent, Node);
    end;

    // 树搭好了，输出并 Base64 再分段再拼头尾最后写文件
    Mem := TMemoryStream.Create;
    Writer.SaveToStream(Mem);
    Mem.Position := 0;

    if Base64_OK = Base64Encode(Mem, S) then
    begin
      List := TStringList.Create;
      SplitStringToList(S, List);
      if KeyType = cktPKCS1 then
      begin
        List.Insert(0, PEM_RSA_PUBLIC_HEAD);
        List.Add(PEM_RSA_PUBLIC_TAIL);
      end
      else if KeyType = cktPKCS8 then
      begin
        List.Insert(0, PEM_PUBLIC_HEAD);
        List.Add(PEM_PUBLIC_TAIL);
      end;
      List.SaveToFile(PemFileName);
      Result := True;
    end;
  finally
    Mem.Free;
    List.Free;
    Writer.Free;
  end;
end;

// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分
function RSACrypt(Data: TCnBigNumber; Product: TCnBigNumber; Exponent: TCnBigNumber;
  Res: TCnBigNumber): Boolean;
begin
  Result := BigNumberMontgomeryPowerMod(Res, Data, Exponent, Product);
end;

// 利用上面生成的私钥对数据进行加密，返回加密是否成功
function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res);
end;

// 利用上面生成的公钥对数据进行解密，返回解密是否成功
function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Res, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Data);
end;

{ TCnRSAPrivateKey }

procedure TCnRSAPrivateKey.Assign(Source: TPersistent);
begin
  if Source is TCnRSAPrivateKey then
  begin
    BigNumberCopy(FPrimeKey1, (Source as TCnRSAPrivateKey).PrimeKey1);
    BigNumberCopy(FPrimeKey2, (Source as TCnRSAPrivateKey).PrimeKey2);
    BigNumberCopy(FPrivKeyProduct, (Source as TCnRSAPrivateKey).PrivKeyProduct);
    BigNumberCopy(FPrivKeyExponent, (Source as TCnRSAPrivateKey).PrivKeyExponent);
  end
  else
    inherited;
end;

procedure TCnRSAPrivateKey.Clear;
begin
  FPrimeKey1.Clear;
  FPrimeKey2.Clear;
  FPrivKeyProduct.Clear;
  FPrivKeyExponent.Clear;
end;

constructor TCnRSAPrivateKey.Create;
begin
  FPrimeKey1 := TCnBigNumber.Create;
  FPrimeKey2 := TCnBigNumber.Create;
  FPrivKeyProduct := TCnBigNumber.Create;
  FPrivKeyExponent := TCnBigNumber.Create;
end;

destructor TCnRSAPrivateKey.Destroy;
begin
  FPrimeKey1.Free;
  FPrimeKey2.Free;
  FPrivKeyProduct.Free;
  FPrivKeyExponent.Free;
  inherited;
end;

function TCnRSAPrivateKey.GetBitsCount: Integer;
begin
  Result := FPrivKeyProduct.GetBitsCount;
end;

{ TCnRSAPublicKey }

procedure TCnRSAPublicKey.Assign(Source: TPersistent);
begin
  if Source is TCnRSAPublicKey then
  begin
    BigNumberCopy(FPubKeyProduct, (Source as TCnRSAPublicKey).PubKeyProduct);
    BigNumberCopy(FPubKeyExponent, (Source as TCnRSAPublicKey).PubKeyExponent);
  end
  else
    inherited;
end;

procedure TCnRSAPublicKey.Clear;
begin
  FPubKeyProduct.Clear;
  FPubKeyExponent.Clear;
end;

constructor TCnRSAPublicKey.Create;
begin
  FPubKeyProduct := TCnBigNumber.Create;
  FPubKeyExponent := TCnBigNumber.Create;
end;

destructor TCnRSAPublicKey.Destroy;
begin
  FPubKeyExponent.Free;
  FPubKeyProduct.Free;
  inherited;
end;

function TCnRSAPublicKey.GetBitsCount: Integer;
begin
  Result := FPubKeyProduct.GetBitsCount;
end;

{ RSA 加密解密运算}

// 将数据块补上填充内容写入 Stream 中，返回成功与否。
// PaddingType 取 0、1、2，BlockLen 字节数如 128 等
// EB = 00 || BT || PS || 00 || D
function PKCS1AddPadding(PaddingType, BlockSize: Integer; Data: Pointer; DataLen: Integer;
  outStream: TStream): Boolean;
var
  I: Integer;
  B, F: Byte;
begin
  Result := False;
  if (Data = nil) or (DataLen <= 0) then
    Exit;

  // 不足以填充
  if DataLen > BlockSize - CN_RSA_PKCS1_PADDING_SIZE then
    Exit;

  B := 0;
  outStream.Write(B, 1);       // 写前导字节 00
  B := PaddingType;
  F := BlockSize - DataLen - 3; // 3 表示一个前导 00、一个类型字节、一个填充后的 00 结尾

  case PaddingType of
    CN_PKCS1_BLOCK_TYPE_PRIVATE_00:
      begin
        outStream.Write(B, 1);
        B := 0;
        for I := 1 to F do
          outStream.Write(B, 1);
      end;
    CN_PKCS1_BLOCK_TYPE_PRIVATE_FF:
      begin
        outStream.Write(B, 1);
        B := $FF;
        for I := 1 to F do
          outStream.Write(B, 1);
      end;
    CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM:
      begin
        outStream.Write(B, 1);
        Randomize;
        for I := 1 to F do
        begin
          B := Trunc(Random(255));
          if B = 0 then
            Inc(B);
          outStream.Write(B, 1);
        end;
      end;
  else
    Exit;
  end;

  B := 0;
  outStream.Write(B, 1);
  outStream.Write(Data^, DataLen);
  Result := True;
end;

// 将一片内存区域按指定的 Padding 方式填充后进行 RSA 加解密计算
function RSAPaddingCrypt(PaddingType, BlockSize: Integer; PlainData: Pointer;
  DataLen: Integer; OutBuf: Pointer; Exponent, Product: TCnBigNumber): Boolean;
var
  Stream: TMemoryStream;
  Res, Data: TCnBigNumber;
begin
  Result := False;
  Res := nil;
  Data := nil;
  Stream := nil;
  try
    Stream := TMemoryStream.Create;
    if not PKCS1AddPadding(PaddingType, BlockSize, PlainData, DataLen, Stream) then
      Exit;

    Res := TCnBigNumber.Create;
    Data := TCnBigNumber.FromBinary(PAnsiChar(Stream.Memory), Stream.Size);
    if not RSACrypt(Data, Product, Exponent, Res) then
      Exit;

    Res.ToBinary(PAnsiChar(OutBuf));
    Result := True;
  finally
    Stream.Free;
    Data.Free;
    Res.Free;
  end;
end;

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM, PublicKey.BitsCount div 8,
    PlainData, DataLen, OutBuf, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.BitsCount div 8,
    PlainData, DataLen, OutBuf, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PublicKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);
    if not CnRSAEncryptData(Stream.Memory, Stream.Size, @Res[0], PublicKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], PublicKey.BitsCount div 8);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PrivateKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);
    if not CnRSAEncryptData(Stream.Memory, Stream.Size, @Res[0], PrivateKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], PrivateKey.BitsCount div 8);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function PKCS1RemovePadding(InData: Pointer; InDataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer): Boolean;
var
  P: PAnsiChar;
  I, J, Start: Integer;
begin
  Result := False;
  OutLen := 0;
  I := 0;

  P := PAnsiChar(InData);
  while P[I] = #0 do // 首字符不一定是 #0，可能已经被去掉了
    Inc(I);

  if I >= InDataLen then
    Exit;

  Start := 0;
  case Ord(P[I]) of
    CN_PKCS1_BLOCK_TYPE_PRIVATE_00:
      begin
        // 从 P[I + 1] 开始寻找非 00 便是
        J := I + 1;
        while J < InDataLen do
        begin
          if P[J] <> #0 then
          begin
            Start := J;
            Break;
          end;
          Inc(J);
        end;
      end;
    CN_PKCS1_BLOCK_TYPE_PRIVATE_FF,
    CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM:
      begin
        // 从 P[I + 1] 开始寻找到第一个 00 后的便是
        J := I + 1;
        while J < InDataLen do
        begin
          if P[J] = #0 then
          begin
            Start := J;
            Break;
          end;
          Inc(J);
        end;

        if Start <> 0 then
          Inc(Start);
      end;
  end;

  if Start > 0 then
  begin
    CopyMemory(OutBuf, @P[Start], InDataLen - Start);
    OutLen := InDataLen - Start;
    Result := True;
  end;
end;

// 将一片内存区域进行 RSA 加解密计算后按其展现的 Padding 方式解出原始数据
function RSADecryptPadding(BlockSize: Integer; EnData: Pointer; DataLen: Integer;
  OutBuf: Pointer; out OutLen: Integer; Exponent, Product: TCnBigNumber): Boolean;
var
  Stream: TMemoryStream;
  Res, Data: TCnBigNumber;
  ResBuf: array of Byte;
begin
  Result := False;
  Res := nil;
  Data := nil;
  Stream := nil;
  try
    Res := TCnBigNumber.Create;
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnData), DataLen);
    if not RSACrypt(Data, Product, Exponent, Res) then
      Exit;

    SetLength(ResBuf, Res.GetBytesCount);
    Res.ToBinary(PAnsiChar(@ResBuf[0]));

    Result := PKCS1RemovePadding(@ResBuf[0], Length(ResBuf), OutBuf, OutLen);
  finally
    Stream.Free;
    Res.Free;
    Data.Free;
  end;
end;

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSADecryptPadding(PublicKey.GetBitsCount div 8, EnData, DataLen,
    OutBuf, OutLen, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSADecryptPadding(PrivateKey.GetBitsCount div 8, EnData, DataLen,
    OutBuf, OutLen, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  Stream: TMemoryStream;
  Res: array of Byte;
  OutLen: Integer;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PublicKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);

    if Stream.Size <> PublicKey.GetBitsCount div 8 then
      Exit;

    if not CnRSADecryptData(Stream.Memory, Stream.Size, @Res[0], OutLen, PublicKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], OutLen);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
  OutLen: Integer;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PrivateKey.BitsCount div 8);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);

    if Stream.Size <> PrivateKey.GetBitsCount div 8 then
      Exit;

    if not CnRSADecryptData(Stream.Memory, Stream.Size, @Res[0], OutLen, PrivateKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], OutLen);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

// RSA 文件签名与验证实现

// 根据指定数字摘要算法计算其二进制散列值并写入 Stream
function CalcDigestFile(const FileName: string; SignType: TCnRSASignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TMD5Digest;
  Sha1: TSHA1Digest;
  Sha256: TSHA256Digest;
begin
  Result := False;
  case SignType of
    sdtMD5:
      begin
        Md5 := MD5File(FileName);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    sdtSHA1:
      begin
        Sha1 := SHA1File(FileName);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    sdtSHA256:
      begin
        Sha256 := SHA256File(FileName);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
  end;
end;

{
  通过数字摘要算法算出二进制摘要后，还要进行 BER 编码再 PKCS1 Padding
  BER 编码的格式如下：
  DigestInfo ::= SEQUENCE )
    digestAlgorithm DigestAlgorithmIdentifier,
    digest Digest )

  DigestAlgorithmIdentifier ::= AlgorithmIdentifier
  Digest ::= OCTET STRING

  也就是：
  SEQUENCE
    SEQUENCE
      OBJECT IDENTIFIER
      NULL
    OCTET STRING
}
function CnRSASignFile(const InFileName, OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType): Boolean;
var
  Stream, BerStream, EnStream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf: array of Byte;
  Writer: TCnBerWriter;
  Root, Node: TCnBerWriteNode;

  function WriterAddOIDNode(AWriter: TCnBerWriter; ASignType: TCnRSASignDigestType;
    AParent: TCnBerWriteNode): TCnBerWriteNode;
  begin
    Result := nil;
    case ASignType of
      sdtMD5:
        Result := Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_MD5[0],
          SizeOf(OID_SIGN_MD5), AParent);
      sdtSHA1:
        Result := Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_SHA1[0],
          SizeOf(OID_SIGN_SHA1), AParent);
      sdtSHA256:
        Result := Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_SHA256[0],
          SizeOf(OID_SIGN_SHA256), AParent);
    end;
  end;

begin
  Result := False;
  Stream := nil;
  EnStream := nil;
  BerStream := nil;
  Writer := nil;
  Data := nil;
  Res := nil;

  try
    Stream := TMemoryStream.Create;
    EnStream := TMemoryStream.Create;

    if SignType = sdtNone then
    begin
      // 无数字摘要，直接整内容对齐
      Stream.LoadFromFile(InFileName);
      if not PKCS1AddPadding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBitsCount div 8,
        Stream.Memory, Stream.Size, EnStream) then
        Exit;
    end
    else // 有数字摘要
    begin
      if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的散列值
        Exit;

      BerStream := TMemoryStream.Create;
      Writer := TCnBerWriter.Create;

      // 然后按格式进行 BER 编码
      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
      WriterAddOIDNode(Writer, SignType, Node);
      Writer.AddNullNode(Node);
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, Stream.Memory, Stream.Size, Root);
      Writer.SaveToStream(BerStream);

      // 再把 BER 编码后的内容 PKCS1 填充对齐
      if not PKCS1AddPadding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBitsCount div 8,
        BerStream.Memory, BerStream.Size, EnStream) then
        Exit;
    end;

    // 私钥加密运算
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnStream.Memory), EnStream.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res) then
    begin
      SetLength(ResBuf, Res.GetBytesCount);
      Res.ToBinary(@ResBuf[0]);

      // 保存用私钥加密后的内容至文件
      Stream.Clear;
      Stream.Write(ResBuf[0], Res.GetBytesCount);
      Stream.SaveToFile(OutSignFileName);
      Result := True;
    end;
  finally
    Stream.Free;
    EnStream.Free;
    BerStream.Free;
    Data.Free;
    Res.Free;
    Writer.Free;
    SetLength(ResBuf, 0);
  end;
end;

function CnRSAVerifyFile(const InFileName, InSignFileName: string;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType): Boolean;
var
  Stream, Sign: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf, BerBuf: array of Byte;
  BerLen: Integer;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  Sign := nil;
  Data := nil;
  Res := nil;

  try
    Stream := TMemoryStream.Create;
    Sign := TMemoryStream.Create;

    // 不管怎样签名文件先公钥解密
    Sign.LoadFromFile(InSignFileName);
    Data := TCnBigNumber.FromBinary(PAnsiChar(Sign.Memory), Sign.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Res) then
    begin
      SetLength(ResBuf, Res.GetBytesCount);
      Res.ToBinary(@ResBuf[0]);

      if SignType = sdtNone then
      begin
        Stream.LoadFromFile(InFileName); // 无摘要时，直接比对解密内容与原始文件
        Result := Stream.Size = Res.GetBytesCount;
        if Result then
          Result := CompareMem(Stream.Memory, @ResBuf[0], Stream.Size);
      end
      else
      begin
        // 从 Res 中解出 PKCS1 对齐的内容
        SetLength(BerBuf, Length(ResBuf));
        if not PKCS1RemovePadding(@ResBuf[0], Length(ResBuf), BerBuf, BerLen) then
          Exit;

        if (BerLen <= 0) or (BerLen >= Length(ResBuf)) then
          Exit;

        // 解开 Ber 内容里的编码与加密算法，不使用 SignType 原始值
        Reader := TCnBerReader.Create(@BerBuf[0], BerLen);
        if Reader.TotalCount < 5 then
          Exit;

        Node := Reader.Items[2];
        SignType := GetDigestSignTypeFromBerOID(Node.BerDataAddress, Node.BerDataLength);
        if SignType = sdtNone then
          Exit;

        if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的散列值
          Exit;

        // 与 Ber 解出的散列值比较
        Node := Reader.Items[4];
        Result := Stream.Size = Node.BerDataLength;
        if Result then
          Result := CompareMem(Stream.Memory, Node.BerDataAddress, Stream.Size);
      end;
    end;
  finally
    Stream.Free;
    Reader.Free;
    Sign.Free;
    Data.Free;
    Res.Free;
    SetLength(ResBuf, 0);
    SetLength(BerBuf, 0);
  end;
end;

function GetDigestSignTypeFromBerOID(OID: Pointer; OidLen: Integer): TCnRSASignDigestType;
begin
  Result := sdtNone;
  if (OidLen = SizeOf(OID_SIGN_MD5)) and CompareMem(OID, @OID_SIGN_MD5[0], OidLen) then
    Result := sdtMD5
  else if (OidLen = SizeOf(OID_SIGN_SHA1)) and CompareMem(OID, @OID_SIGN_SHA1[0], OidLen) then
    Result := sdtSHA1
  else if (OidLen = SizeOf(OID_SIGN_SHA256)) and CompareMem(OID, @OID_SIGN_SHA256[0], OidLen) then
    Result := sdtSHA256;
end;

end.
