{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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
* 备    注：包括 Int64 范围内的 RSA 算法以及大数算法，公钥 Exponent 默认固定使用 65537。
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.04.26 V2.4
*               修改 Integer 地址转换以支持 MacOS64
*           2021.06.12 V2.1
*               加入 OAEP Padding 的处理
*           2021.05.09 V2.0
*               私钥载入时自动判断 PEM 内是 PKCS1 还是 PKCS8 格式，不依赖于头尾行的横杠注释
*           2020.06.10 V1.9
*               公私钥允许从 Stream 中载入
*           2020.03.27 V1.8
*               公钥允许用 3。实现了加密的 PEM 文件的读写，不过只支持 des/3des/aes
*           2020.03.13 V1.7
*               加入详细的错误码。调用返回 False 时可通过 GetLastCnRSAError 获取 ECN_RSA_* 形式的错误码
*           2019.04.19 V1.6
*               支持 Win32/Win64/MacOS32
*           2018.06.15 V1.5
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
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnPrimeNumber, CnBigNumber,
  CnBase64, CnBerUtils, CnPemUtils, CnNativeDecl, CnMD5, CnSHA1, CnSHA2, CnSM3;

const
  // 以下 OID 都预先写死，不动态计算编码了
  OID_RSAENCRYPTION_PKCS1: array[0..8] of Byte = ( // 1.2.840.113549.1.1.1
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $01
  );  // $2A = 40 * 1 + 2

  // 错误码
  ECN_RSA_OK                           = 0; // 没错
  ECN_RSA_INVALID_INPUT                = 1; // 输入为空或长度不对
  ECN_RSA_INVALID_BITS                 = 2; // 密钥位数不对
  ECN_RSA_BIGNUMBER_ERROR              = 3; // 大数运算错误
  ECN_RSA_BER_ERROR                    = 4; // BER 格式编码错误
  ECN_RSA_PADDING_ERROR                = 5; // PADDING 对齐错误
  ECN_RSA_DIGEST_ERROR                 = 6; // 数字摘要错误
  ECN_RSA_PEM_FORMAT_ERROR             = 7; // PEM 格式错误
  ECN_RSA_PEM_CRYPT_ERROR              = 8; // PEM 加解密错误

type
  TCnRSASignDigestType = (rsdtNone, rsdtMD5, rsdtSHA1, rsdtSHA256, rsdtSM3);
  {* RSA 签名所支持的数字摘要算法，可无摘要}

  TCnRSAKeyType = (cktPKCS1, cktPKCS8);
  {* RSA 密钥文件格式}

  TCnRSAPaddingMode = (cpmPKCS1, cpmOAEP);
  {* RSA 加密的填充模式，PKCS1 适合加解密，包括三种填充类型，
    OAEP 只适用于公钥加密，默认使用 SHA1 作为掩码生成杂凑算法}

  TCnRSAPrivateKey = class(TPersistent)
  {* RSA 私钥}
  private
    FPrimeKey1: TCnBigNumber;
    FPrimeKey2: TCnBigNumber;
    FPrivKeyProduct: TCnBigNumber;
    FPrivKeyExponent: TCnBigNumber;
    function GetBitsCount: Integer;
    function GetBytesCount: Integer;
  public
    constructor Create; virtual;
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
    {* 密钥的位数，也即素数乘积 n 的有效位数}
    property BytesCount: Integer read GetBytesCount;
    {* 密钥的字节数，等于素数乘积 n 的有效位数除以 8}
  end;

  TCnRSAPublicKey = class(TPersistent)
  {* RSA 公钥}
  private
    FPubKeyProduct: TCnBigNumber;
    FPubKeyExponent: TCnBigNumber;
    function GetBitsCount: Integer;
    function GetBytesCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    property PubKeyProduct: TCnBigNumber read FPubKeyProduct write FPubKeyProduct;
    {* 俩素数乘积 n，也叫 Modulus}
    property PubKeyExponent: TCnBigNumber read FPubKeyExponent write FPubKeyExponent;
    {* 公钥指数 e，65537}
    property BitsCount: Integer read GetBitsCount;
    {* 密钥的位数，也即素数乘积 n 的有效位数}
    property BytesCount: Integer read GetBytesCount;
    {* 密钥的字节数，等于素数乘积 n 的有效位数除以 8}
  end;

// UInt64 范围内的 RSA 加解密实现

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
  PublicKey: TCnRSAPublicKey; PublicKeyUse3: Boolean = False): Boolean;
{* 生成 RSA 算法所需的公私钥，PrimeBits 是素数的二进制位数，其余参数均为生成。
   PrimeBits 取值为 512/1024/2048等，注意目前不是乘积的范围。内部缺乏安全判断。
   PublicKeyUse3 为 True 时公钥指数用 3，否则用 65537}

function CnRSAGenerateKeys(ModulusBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; PublicKeyUse3: Boolean = False): Boolean;
{* 生成 RSA 算法所需的公私钥，ModulusBits 是素数乘积的二进制位数，其余参数均为生成。
   ModulusBits 取值为 512/1024/2048等。内部有安全判断。
   PublicKeyUse3 为 True 时公钥指数用 3，否则用 65537}

function CnRSALoadKeysFromPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式文件中加载公私钥数据，如某钥参数为空则不载入
  自动判断 PKCS1 还是 PKCS8，不依赖于头尾行的 ----- 注释
  KeyHashMethod: 对应 PEM 文件的加密 Hash 算法，默认 MD5（无法根据 PEM 文件内容自动判断）
  Password: PEM 文件如加密，此处应传对应密码
}

function CnRSALoadKeysFromPem(PemStream: TStream; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的流中加载公私钥数据，如某钥参数为空则不载入
  自动判断 PKCS1 还是 PKCS8，不依赖于头尾行的 ----- 注释
  KeyHashMethod: 对应 PEM 文件的加密 Hash 算法，默认 MD5（无法根据 PEM 文件内容自动判断）
  Password: PEM 文件如加密，此处应传对应密码
}

function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS1;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean;
{* 将公私钥写入 PEM 格式文件中，返回是否成功
  KeyEncryptMethod: 如 PEM 文件需加密，可用此参数指定加密方式，ckeNone 表示不加密，忽略后续参数
  KeyHashMethod: 生成 Key 的 Hash 算法，默认 MD5
  Password: PEM 文件的加密密码
}

function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式文件中加载公钥数据，返回是否成功}

function CnRSALoadPublicKeyFromPem(const PemStream: TStream;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式流中加载公钥数据，返回是否成功}

function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS8;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  const Password: string = ''): Boolean;
{* 将公钥写入 PEM 格式文件中，返回是否成功}

function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
{* 利用上面生成的私钥对数据进行加密，返回加密是否成功}

function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
{* 利用上面生成的公钥对数据进行解密，返回解密是否成功}

// ======================== RSA 数据与文件加解密实现 ===========================

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对数据块进行加密，无填充，结果放 OutBuf 中，
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对数据块进行加密，无填充，结果放 OutBuf 中，
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对数据块进行无填充解密，结果放 OutBuf 中，并返回数据长度
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对数据块进行无填充解密结果放 OutBuf 中，并返回数据长度
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用公钥对数据块进行加密，加密前可指定使用 PKCS1 填充或 OAEP 填充，结果放 OutBuf 中，
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
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey; PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用私钥对数据块进行解密，并解开其 PKCS1 填充或 OAEP 填充，结果放 OutBuf 中，并返回数据长度
  OutBuf 长度不能短于密钥长度，1024 Bit 的 则 128 字节}

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用公钥对文件进行加密，加密前可指定使用 PKCS1 填充或 OAEP 填充，结果存输出文件中}

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对文件进行加密，加密前使用 PKCS1 填充，结果存输出文件中}

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对文件进行解密，并解开其 PKCS1 填充，结果存输出文件中，注意不支持 OAEP 填充}

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey; PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用私钥对文件进行解密，并解开其 PKCS1 填充或 OAEP 填充，结果存输出文件中}

// =========================== RSA 文件签名与验证实现 ==========================
// 流与文件分开实现是因为计算文件摘要时支持大文件，而 FileStream 低版本不支持
// 注意 RSA 签名是先 Hash 再拼一段数据用 RSA 私钥加密，验证时能解出 Hash 值
// 这点和 ECC 签名不同：ECC 签名并不解出 Hash 值，而是通过中间运算比对大数

function CnRSASignFile(const InFileName, OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用私钥签名指定文件。
   未指定数字摘要算法时等于将源文件用 PKCS1 Private_FF 补齐后加密
   当指定了数字摘要算法时，使用指定数字摘要算法对文件进行计算得到散列值，
   原始的二进制散列值进行 BER 编码再 PKCS1 补齐再用私钥加密}

function CnRSAVerifyFile(const InFileName, InSignFileName: string;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用公钥与签名值验证指定文件，也即用指定数字摘要算法对文件进行计算得到散列值，
   并用公钥解密签名内容并解开 PKCS1 补齐再解开 BER 编码得到散列算法与散列值，
   并比对两个二进制散列值是否相同，返回验证是否通过}

function CnRSASignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用私钥签名指定内存流}

function CnRSAVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用公钥与签名值验证指定内存流}

// OAEP Padding 的生成与验证算法

function AddOaepSha1MgfPadding(ToBuf: PByte; ToLen: Integer; PlainData: PByte;
  DataLen: Integer; DigestParam: PByte = nil; ParamLen: Integer = 0): Boolean;
{* 对 Data 里 DataLen 的数据进行 OAEP 填充，内容放到 ToBuf 的 ToLen 里，返回填充是否成功。
  默认使用 SHA1 对 DigestBuf 内容进行散列，ToLen 一般是 RSA 的密钥的积的字节数}

function RemoveOaepSha1MgfPadding(ToBuf: PByte; out OutLen: Integer; EnData: PByte;
  DataLen: Integer; DigestParam: PByte = nil; ParamLen: Integer = 0): Boolean;
{* 对 EnData 里 DataLen 的数据进行 OAEP 检验并去除填充，内容放到 ToBuf 的 OutLen 里，返回检查是否成功。
  ToBuf 能容纳的实际长度不能太短，如成功，OutLen 返回明文数据长度
  默认使用 SHA1 对 DigestBuf 内容进行散列，DataLen 要求是 RSA 的密钥的积的字节数}

// Diffie-Hellman 离散对数密钥交换算法

function CnDiffieHellmanGeneratePrimeRootByBitsCount(BitsCount: Integer;
  Prime, MinRoot: TCnBigNumber): Boolean;
{* 生成 Diffie-Hellman 密钥协商算法所需的素数与其最小原根，涉及到因素分解因此较慢}

function CnDiffieHellmanGenerateOutKey(Prime, Root, SelfPrivateKey: TCnBigNumber;
  const OutPublicKey: TCnBigNumber): Boolean;
{* 根据自身选择的随机数 PrivateKey 生成 Diffie-Hellman 密钥协商的输出公钥
   其中 OutPublicKey = (Root ^ SelfPrivateKey) mod Prime}

function CnDiffieHellmanComputeKey(Prime, SelfPrivateKey, OtherPublicKey: TCnBigNumber;
  const SecretKey: TCnBigNumber): Boolean;
{* 根据对方发送的 Diffie-Hellman 密钥协商的输出公钥计算生成公认的密钥
   其中 SecretKey = (OtherPublicKey ^ SelfPrivateKey) mod Prime}

// ================================= 其他辅助函数 ==============================

function GetDigestSignTypeFromBerOID(OID: Pointer; OidLen: Integer): TCnRSASignDigestType;
{* 从 BER 解析出的 OID 获取其对应的散列摘要类型}

function AddDigestTypeOIDNodeToWriter(AWriter: TCnBerWriter; ASignType: TCnRSASignDigestType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
{* 将一个散列算法的 OID 写入一个 Ber 节点}

function GetRSADigestNameFromSignDigestType(Digest: TCnRSASignDigestType): string;
{* 从签名散列算法枚举值获取其名称}

function GetLastCnRSAError: Integer;
{* 获取本线程内最近一次 ErrorCode，当以上函数返回 False 时可调用此函数获取错误详情}

implementation

uses
  CnRandom;

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

  OID_SIGN_MD5: array[0..7] of Byte = (            // 1.2.840.113549.2.5
    $2A, $86, $48, $86, $F7, $0D, $02, $05
  );

  OID_SIGN_SHA1: array[0..4] of Byte = (           // 1.3.14.3.2.26
    $2B, $0E, $03, $02, $1A
  );

  OID_SIGN_SHA256: array[0..8] of Byte = (         // 2.16.840.1.101.3.4.2.1
    $60, $86, $48, $01, $65, $03, $04, $02, $01
  );

threadvar
  RSAErrorCode: Integer;

// 获取本线程内最近一次 ErrorCode，当以上函数返回 False 时可调用此函数获取错误详情}
function GetLastCnRSAError: Integer;
begin
  Result := RSAErrorCode;
end;

// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分
function Int64RSACrypt(Data: TUInt64; Product: TUInt64; Exponent: TUInt64;
  out Res: TUInt64): Boolean;
begin
  Res := MontgomeryPowerMod(Data, Exponent, Product);
  Result := True;
end;

function GetInt64BitCount(A: TUInt64): Integer;
var
  I: Integer;
begin
  I := 0;
  while A <> 0 do
  begin
    A := A shr 1;
    Inc(I);
  end;
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
    PrimeKey1 := CnGenerateUInt32Prime(HighBitSet);

    N := Trunc(Random * 100); // 以调整 CnGenerateUInt32Prime 内部的随机数发生器
    Sleep(N);

    PrimeKey2 := CnGenerateUInt32Prime(HighBitSet);
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
  CnInt64ExtendedEuclideanGcd(PubKeyExponent, Product, PrivKeyExponent, Y);
  while UInt64IsNegative(PrivKeyExponent) do
  begin
     // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上倍数个 r，加到大于零为止
     Y := (UInt64Div(-PrivKeyExponent, Product) + 1) * Product;
     PrivKeyExponent := PrivKeyExponent + Y;
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
  PublicKey: TCnRSAPublicKey; PublicKeyUse3: Boolean): Boolean;
var
  N: Integer;
  Suc: Boolean;
  R, Y, Rem, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  if PrimeBits <= 16 then
  begin
    RSAErrorCode := ECN_RSA_INVALID_BITS;
    Exit;
  end;

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

    if PublicKeyUse3 then
      PublicKey.PubKeyExponent.SetDec('3')
    else
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
  PublicKey: TCnRSAPublicKey; PublicKeyUse3: Boolean): Boolean;
var
  PB1, PB2, MinDB, MinW: Integer;
  Suc: Boolean;
  Dif, MinD: TCnBigNumber;
  R, Y, Rem, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  RSAErrorCode := ECN_RSA_BIGNUMBER_ERROR;
  if ModulusBits < 128 then
  begin
    RSAErrorCode := ECN_RSA_INVALID_BITS;
    Exit;
  end;

  PrivateKey.Clear;
  PublicKey.Clear;
  Suc := False;

  PB1 := (ModulusBits + 1) div 2;
  PB2 := ModulusBits - PB1;
  MinDB := ModulusBits div 2 - 100;
  if MinDB < ModulusBits div 3 then
    MinDB := ModulusBits div 3;
  MinW := ModulusBits shr 2;

  Rem := nil;
  Y := nil;
  R := nil;
  S1 := nil;
  S2 := nil;
  One := nil;
  Dif := nil;
  MinD := nil;

  try
    Rem := TCnBigNumber.Create;
    Y := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    S1 := TCnBigNumber.Create;
    S2 := TCnBigNumber.Create;
    One := TCnBigNumber.Create;
    Dif := TCnBigNumber.Create;
    MinD := TCnBigNumber.Create;

    while not Suc do
    begin
      if not BigNumberGeneratePrimeByBitsCount(PrivateKey.PrimeKey1, PB1) then
        Exit;

      if not BigNumberGeneratePrimeByBitsCount(PrivateKey.PrimeKey2, PB2) then
        Exit;

      if not BigNumberMul(PrivateKey.PrivKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
        Exit;

      // p、q 的积是否满足 Bit 数，不满足时得 Continue
      if PrivateKey.PrivKeyProduct.GetBitsCount <> ModulusBits then
        Continue;

      // 如果乘积的位数为 n，则 |p-q| 的位数要求比 n/3 大，也比 n/2 - 100 大
      if not BigNumberSub(Dif, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
        Exit;

      if Dif.GetBitsCount <= MinDB then
        Continue;

      // 一般要求 Prime1 > Prime2 以便计算 CRT 等参数
      if BigNumberCompare(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) < 0 then
        BigNumberSwap(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2);

      // TODO: pq 的积的非相邻形式（Non-Adjacent Form）NAF 系数是否满足条件，不满足时得 Continue

      if not BigNumberMul(PublicKey.PubKeyProduct, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
        Exit;

      if PublicKeyUse3 then
        PublicKey.PubKeyExponent.SetDec('3')
      else
        PublicKey.PubKeyExponent.SetDec('65537');

      BigNumberSetOne(One);
      BigNumberSub(S1, PrivateKey.PrimeKey1, One);
      BigNumberSub(S2, PrivateKey.PrimeKey2, One);
      BigNumberMul(R, S1, S2);     // 计算积二，R = (p - 1) * (q - 1)

      // 求 e 也就是 PubKeyExponent（65537）针对积二 R 的模反元素 d 也就是 PrivKeyExponent
      BigNumberExtendedEuclideanGcd(PublicKey.PubKeyExponent, R, PrivateKey.PrivKeyExponent, Y);

      // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上积二 R
      if BigNumberIsNegative(PrivateKey.PrivKeyExponent) then
         BigNumberAdd(PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyExponent, R);

      // d 不能太小，必须大于 2 的 n/2 次方
      MinD.SetOne;
      MinD.ShiftLeft(MinW);
      if BigNumberCompare(PrivateKey.PrivKeyExponent, MinD) <= 0 then
        Continue;

      Suc := True;
    end;
  finally
    MinD.Free;
    Dif.Free;
    One.Free;
    S2.Free;
    S1.Free;
    R.Free;
    Y.Free;
    Rem.Free;
  end;
  RSAErrorCode := ECN_RSA_OK;
  Result := True;
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
function CnRSALoadKeysFromPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnRSALoadKeysFromPem(Stream, PrivateKey, PublicKey, KeyHashMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnRSALoadKeysFromPem(PemStream: TStream; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
var
  LoadOK: Boolean;
  MemStream: TMemoryStream;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
{$IFDEF TSTREAM_LONGINT}
  OldPos: LongInt;
{$ELSE}
  OldPos: Int64;
{$ENDIF}
begin
  Result := False;
  MemStream := nil;
  Reader := nil;

  try
    MemStream := TMemoryStream.Create;
    OldPos := PemStream.Position;

    LoadOK := LoadPemStreamToMemory(PemStream, PEM_RSA_PRIVATE_HEAD, PEM_RSA_PRIVATE_TAIL,
      MemStream, Password, KeyHashMethod);
    if not LoadOK then
    begin
      PemStream.Position := OldPos;
      LoadOK := LoadPemStreamToMemory(PemStream, PEM_PRIVATE_HEAD, PEM_PRIVATE_TAIL,
        MemStream, Password, KeyHashMethod);
    end;

    if not LoadOK then
    begin
      RSAErrorCode := ECN_RSA_PEM_FORMAT_ERROR;
      Exit;
    end;

    Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
    Reader.ParseToTree;

    if Reader.TotalCount >= 12 then // 子节点多，说明是 PKCS#8 的 PEM 公私钥格式
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
    end
    else // 子节点太少，重新不解析内部字符串地读
    begin
      Reader.Free;
      Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size);
      Reader.ParseToTree;

      if Reader.TotalCount >= 8 then // 这个数量的子节点，是 PKCS#1 的 PEM 公私钥格式
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
    end;

    if not Result then
      RSAErrorCode := ECN_RSA_PEM_FORMAT_ERROR;
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
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnRSALoadPublicKeyFromPem(Stream, PublicKey, KeyHashMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnRSALoadPublicKeyFromPem(const PemStream: TStream;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
var
  Mem: TMemoryStream;
  Reader: TCnBerReader;
{$IFDEF TSTREAM_LONGINT}
  OldPos: LongInt;
{$ELSE}
  OldPos: Int64;
{$ENDIF}
begin
  Result := False;
  Mem := nil;
  Reader := nil;

  try
    Mem := TMemoryStream.Create;
    OldPos := PemStream.Position;

    if LoadPemStreamToMemory(PemStream, PEM_PUBLIC_HEAD, PEM_PUBLIC_TAIL, Mem,
      Password, KeyHashMethod) then
    begin
      // 读 PKCS#8 格式的公钥
      Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size, True);
      Reader.ParseToTree;
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
    end;

    if Result then
      Exit;

    PemStream.Position := OldPos;
    if LoadPemStreamToMemory(PemStream, PEM_RSA_PUBLIC_HEAD, PEM_RSA_PUBLIC_TAIL,
      Mem, Password, KeyHashMethod) then
    begin
      // 读 PKCS#1 格式的公钥
      Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size);
      Reader.ParseToTree;
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

    if not Result then
      RSAErrorCode := ECN_RSA_PEM_FORMAT_ERROR;
  finally
    Mem.Free;
    Reader.Free;
  end;
end;

// 将公私钥写入 PEM 格式文件中
function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType; KeyEncryptMethod: TCnKeyEncryptMethod;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  N, T, R1, R2, X, Y : TCnBigNumber;
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

    if KeyType = cktPKCS1 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_RSA_PRIVATE_HEAD,
        PEM_RSA_PRIVATE_TAIL, Mem, KeyEncryptMethod, KeyHashMethod, Password)
    else if KeyType = cktPKCS8 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_PRIVATE_HEAD,
        PEM_PRIVATE_TAIL, Mem, KeyEncryptMethod, KeyHashMethod, Password);
  finally
    BigNumberFree(T);
    BigNumberFree(R1);
    BigNumberFree(R2);
    BigNumberFree(N);
    BigNumberFree(X);
    BigNumberFree(Y);

    Mem.Free;
    Writer.Free;
  end;
end;

// 将公钥写入 PEM 格式文件中
function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType;
  KeyEncryptMethod: TCnKeyEncryptMethod; const Password: string): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
begin
  Result := False;
  if (PublicKey = nil) or (PublicKey.PubKeyProduct.GetBytesCount <= 0) or
    (PublicKey.PubKeyExponent.GetBytesCount <= 0) then
    Exit;

  Mem := nil;
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

    if KeyType = cktPKCS1 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_RSA_PUBLIC_HEAD,
        PEM_RSA_PUBLIC_TAIL, Mem, KeyEncryptMethod, ckhMd5, Password)
    else if KeyType = cktPKCS8 then
      Result := SaveMemoryToPemFile(PemFileName, PEM_PUBLIC_HEAD,
        PEM_PUBLIC_TAIL, Mem, KeyEncryptMethod, ckhMd5, Password);
  finally
    Mem.Free;
    Writer.Free;
  end;
end;

// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分。内部会设置错误码
function RSACrypt(Data: TCnBigNumber; Product: TCnBigNumber; Exponent: TCnBigNumber;
  Res: TCnBigNumber): Boolean;
begin
  Result := BigNumberMontgomeryPowerMod(Res, Data, Exponent, Product);
  if not Result then
    RSAErrorCode := ECN_RSA_BIGNUMBER_ERROR;
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
  inherited;
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

function TCnRSAPrivateKey.GetBytesCount: Integer;
begin
  Result := FPrivKeyExponent.GetBytesCount;
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
  inherited;
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

function TCnRSAPublicKey.GetBytesCount: Integer;
begin
  Result := FPubKeyProduct.GetBytesCount;
end;

{ RSA 加密解密运算}

function RSACryptRawData(Data: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; Exponent, Product: TCnBigNumber): Boolean;
var
  D, R: TCnBigNumber;
begin
  Result := False;
  if (Data <> nil) and (DataLen > 0) then
  begin
    R := TCnBigNumber.Create;
    D := TCnBigNumber.FromBinary(PAnsiChar(Data), DataLen);

    if RSACrypt(D, Product, Exponent, R) then
    begin
      R.ToBinary(OutBuf);
      OutLen := R.GetBytesCount;
      Result := True;
    end;
  end
  else
    RSAErrorCode := ECN_RSA_INVALID_INPUT;
end;

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSACryptRawData(PlainData, DataLen, OutBuf, OutLen,
    PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSAEncryptRawData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSACryptRawData(PlainData, DataLen, OutBuf, OutLen,
    PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSACryptRawData(EnData, DataLen, OutBuf, OutLen,
    PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSADecryptRawData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSACryptRawData(EnData, DataLen, OutBuf, OutLen,
    PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

// 将一片内存区域按指定的 Padding 模式与类型填充后进行 RSA 加解密计算
function RSAPaddingCrypt(PaddingType, BlockSize: Integer; PlainData: Pointer;
  DataLen: Integer; OutBuf: Pointer; Exponent, Product: TCnBigNumber;
  PaddingMode: TCnRSAPaddingMode): Boolean;
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
    if PaddingMode = cpmPKCS1 then
    begin
      if not AddPKCS1Padding(PaddingType, BlockSize, PlainData, DataLen, Stream) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;
    end
    else if PaddingMode = cpmOAEP then
    begin
      // OAEP 公钥加密，仅公钥的控制在调用者
      Stream.Size := Product.GetBytesCount;
      if not AddOaepSha1MgfPadding(Stream.Memory, Stream.Size, PlainData, DataLen) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;
    end;

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
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM, PublicKey.BitsCount div 8,
    PlainData, DataLen, OutBuf, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct, PaddingMode);
end;

function CnRSAEncryptData(PlainData: Pointer; DataLen: Integer; OutBuf: Pointer;
  PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.BitsCount div 8,
    PlainData, DataLen, OutBuf, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct, cpmPKCS1);
  // 私钥加密只支持 PKCS1 对齐方式，不支持 OAEP 对齐方式
end;

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PublicKey.BytesCount);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);
    if not CnRSAEncryptData(Stream.Memory, Stream.Size, @Res[0], PublicKey, PaddingMode) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], PublicKey.BytesCount);
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
    SetLength(Res, PrivateKey.BytesCount);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);
    if not CnRSAEncryptData(Stream.Memory, Stream.Size, @Res[0], PrivateKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], PrivateKey.BytesCount);
    Stream.SaveToFile(OutFileName);
    Result := True;
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

// 将一片内存区域进行 RSA 加解密计算后按其展现的 Padding 方式解出原始数据
function RSADecryptPadding(BlockSize: Integer; EnData: Pointer; DataLen: Integer;
  OutBuf: Pointer; out OutLen: Integer; Exponent, Product: TCnBigNumber;
  PaddingMode: TCnRSAPaddingMode): Boolean;
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

    SetLength(ResBuf, BlockSize);
    if Res.GetBytesCount = BlockSize then
      Res.ToBinary(PAnsiChar(@ResBuf[0]))
    else if Res.GetBytesCount < BlockSize then
      Res.ToBinary(PAnsiChar(@ResBuf[BlockSize - Res.GetBytesCount]));
      // 解出来的 Res 可能前面有 0 导致 GetBytesCount 不够 BlockSize，需要右对齐

    if PaddingMode = cpmPKCS1 then
    begin
      Result := RemovePKCS1Padding(@ResBuf[0], Length(ResBuf), OutBuf, OutLen);
      if not Result then
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
    end
    else if PaddingMode = cpmOAEP then
    begin
      // OAEP 解密，仅私钥的控制在调用者
      Result := RemoveOaepSha1MgfPadding(OutBuf, OutLen, @ResBuf[0], Length(ResBuf));
      if not Result then
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
    end;
  finally
    Stream.Free;
    Res.Free;
    Data.Free;
  end;
end;

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSADecryptPadding(PublicKey.GetBytesCount, EnData, DataLen,
    OutBuf, OutLen, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct, cpmPKCS1);
  // 公钥解密只支持 PKCS1，不支持 OAEP
end;

function CnRSADecryptData(EnData: Pointer; DataLen: Integer; OutBuf: Pointer;
  out OutLen: Integer; PrivateKey: TCnRSAPrivateKey; PaddingMode: TCnRSAPaddingMode): Boolean;
begin
  Result := RSADecryptPadding(PrivateKey.GetBytesCount, EnData, DataLen,
    OutBuf, OutLen, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct, PaddingMode);
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
    SetLength(Res, PublicKey.GetBytesCount);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);

    if Stream.Size <> PublicKey.GetBytesCount then
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
  PrivateKey: TCnRSAPrivateKey; PaddingMode: TCnRSAPaddingMode): Boolean; overload;
var
  Stream: TMemoryStream;
  Res: array of Byte;
  OutLen: Integer;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PrivateKey.BytesCount);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);

    if Stream.Size <> PrivateKey.GetBytesCount then
    begin
      RSAErrorCode := ECN_RSA_INVALID_INPUT;
      Exit;
    end;

    if not CnRSADecryptData(Stream.Memory, Stream.Size, @Res[0], OutLen, PrivateKey, PaddingMode) then
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

// 根据指定数字摘要算法计算指定流的二进制散列值并写入 Stream
function CalcDigestStream(InStream: TStream; SignType: TCnRSASignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TMD5Digest;
  Sha1: TSHA1Digest;
  Sha256: TSHA256Digest;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  case SignType of
    rsdtMD5:
      begin
        Md5 := MD5Stream(InStream);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    rsdtSHA1:
      begin
        Sha1 := SHA1Stream(InStream);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    rsdtSHA256:
      begin
        Sha256 := SHA256Stream(InStream);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
    rsdtSM3:
      begin
        Sm3Dig := SM3Stream(InStream);
        outStream.Write(Sm3Dig, SizeOf(TSM3Digest));
        Result := True;
      end
  end;

  if not Result then
    RSAErrorCode := ECN_RSA_DIGEST_ERROR;
end;

// 根据指定数字摘要算法计算文件的二进制散列值并写入 Stream
function CalcDigestFile(const FileName: string; SignType: TCnRSASignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TMD5Digest;
  Sha1: TSHA1Digest;
  Sha256: TSHA256Digest;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  case SignType of
    rsdtMD5:
      begin
        Md5 := MD5File(FileName);
        outStream.Write(Md5, SizeOf(TMD5Digest));
        Result := True;
      end;
    rsdtSHA1:
      begin
        Sha1 := SHA1File(FileName);
        outStream.Write(Sha1, SizeOf(TSHA1Digest));
        Result := True;
      end;
    rsdtSHA256:
      begin
        Sha256 := SHA256File(FileName);
        outStream.Write(Sha256, SizeOf(TSHA256Digest));
        Result := True;
      end;
    rsdtSM3:
      begin
        Sm3Dig := SM3File(FileName);
        outStream.Write(Sm3Dig, SizeOf(TSM3Digest));
        Result := True;
      end;
  end;

  if not Result then
    RSAErrorCode := ECN_RSA_DIGEST_ERROR;
end;

function AddDigestTypeOIDNodeToWriter(AWriter: TCnBerWriter; ASignType: TCnRSASignDigestType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
begin
  Result := nil;
  case ASignType of
    rsdtMD5:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_MD5[0],
        SizeOf(OID_SIGN_MD5), AParent);
    rsdtSHA1:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_SHA1[0],
        SizeOf(OID_SIGN_SHA1), AParent);
    rsdtSHA256:
      Result := AWriter.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @OID_SIGN_SHA256[0],
        SizeOf(OID_SIGN_SHA256), AParent);
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

function CnRSASignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
var
  Stream, BerStream, EnStream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf: array of Byte;
  Writer: TCnBerWriter;
  Root, Node: TCnBerWriteNode;
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

    if SignType = rsdtNone then
    begin
      // 无数字摘要，直接整内容对齐
      if not AddPKCS1Padding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBytesCount,
        InStream.Memory, InStream.Size, EnStream) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;
    end
    else // 有数字摘要
    begin
      if not CalcDigestStream(InStream, SignType, Stream) then // 计算流的散列值
        Exit;

      BerStream := TMemoryStream.Create;
      Writer := TCnBerWriter.Create;

      // 然后按格式进行 BER 编码
      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
      AddDigestTypeOIDNodeToWriter(Writer, SignType, Node);
      Writer.AddNullNode(Node);
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, Stream.Memory, Stream.Size, Root);
      Writer.SaveToStream(BerStream);

      // 再把 BER 编码后的内容 PKCS1 填充对齐
      if not AddPKCS1Padding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBytesCount,
        BerStream.Memory, BerStream.Size, EnStream) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;
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
      Stream.SaveToStream(OutSignStream);
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

function CnRSAVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
var
  Stream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf, BerBuf: array of Byte;
  BerLen: Integer;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  Data := nil;
  Res := nil;

  try
    Stream := TMemoryStream.Create;

    // 不管怎样签名内容先公钥解密
    Data := TCnBigNumber.FromBinary(PAnsiChar(InSignStream.Memory), InSignStream.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Res) then
    begin
      SetLength(ResBuf, Res.GetBytesCount);
      Res.ToBinary(@ResBuf[0]);

      // 从 Res 中解出 PKCS1 对齐的内容放入 BerBuf 中
      SetLength(BerBuf, Length(ResBuf));
      if not RemovePKCS1Padding(@ResBuf[0], Length(ResBuf), @BerBuf[0], BerLen) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;

      if SignType = rsdtNone then
      begin
        // 无摘要时，从解密内容里去除了 PKCS1 的 Padding 的剩下内容直接与原始 InStream 内容比对
        Result := InStream.Size = BerLen;
        if Result then
          Result := CompareMem(InStream.Memory, @BerBuf[0], InStream.Size);

        RSAErrorCode := ECN_RSA_OK; // 正常进行校验，即使校验不通过也清空错误码
      end
      else
      begin
        if (BerLen <= 0) or (BerLen >= Length(ResBuf)) then
        begin
          RSAErrorCode := ECN_RSA_BER_ERROR;
          Exit;
        end;

        // 解开 Ber 内容里的编码与加密算法，不使用 SignType 原始值
        Reader := TCnBerReader.Create(@BerBuf[0], BerLen);
        Reader.ParseToTree;
        if Reader.TotalCount < 5 then
        begin
          RSAErrorCode := ECN_RSA_BER_ERROR;
          Exit;
        end;

        Node := Reader.Items[2];
        SignType := GetDigestSignTypeFromBerOID(Node.BerDataAddress, Node.BerDataLength);
        if SignType = rsdtNone then
        begin
          RSAErrorCode := ECN_RSA_BER_ERROR;
          Exit;
        end;

        if not CalcDigestStream(InStream, SignType, Stream) then // 计算流的散列值
          Exit;

        // 与 Ber 解出的散列值比较
        Node := Reader.Items[4];
        Result := Stream.Size = Node.BerDataLength;
        if Result then
          Result := CompareMem(Stream.Memory, Node.BerDataAddress, Stream.Size);

        RSAErrorCode := ECN_RSA_OK; // 正常进行校验，即使校验不通过也清空错误码
      end;
    end;
  finally
    Stream.Free;
    Reader.Free;
    Data.Free;
    Res.Free;
    SetLength(ResBuf, 0);
    SetLength(BerBuf, 0);
  end;
end;

function CnRSASignFile(const InFileName, OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType): Boolean;
var
  Stream, BerStream, EnStream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf: array of Byte;
  Writer: TCnBerWriter;
  Root, Node: TCnBerWriteNode;
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

    if SignType = rsdtNone then
    begin
      // 无数字摘要，直接整内容对齐
      Stream.LoadFromFile(InFileName);
      if not AddPKCS1Padding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBytesCount,
        Stream.Memory, Stream.Size, EnStream) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;
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
      AddDigestTypeOIDNodeToWriter(Writer, SignType, Node);
      Writer.AddNullNode(Node);
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, Stream.Memory, Stream.Size, Root);
      Writer.SaveToStream(BerStream);

      // 再把 BER 编码后的内容 PKCS1 填充对齐
      if not AddPKCS1Padding(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.GetBytesCount,
        BerStream.Memory, BerStream.Size, EnStream) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;
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

      // 从 Res 中解出 PKCS1 对齐的内容放入 BerBuf 中
      SetLength(BerBuf, Length(ResBuf));
      if not RemovePKCS1Padding(@ResBuf[0], Length(ResBuf), @BerBuf[0], BerLen) then
      begin
        RSAErrorCode := ECN_RSA_PADDING_ERROR;
        Exit;
      end;

      if SignType = rsdtNone then
      begin
        Stream.LoadFromFile(InFileName); // 无摘要时，直接比对解密内容与原始文件
        Result := Stream.Size = BerLen;
        if Result then
          Result := CompareMem(Stream.Memory, @BerBuf[0], Stream.Size);

        RSAErrorCode := ECN_RSA_OK; // 正常进行校验，即使校验不通过也清空错误码
      end
      else
      begin
        if (BerLen <= 0) or (BerLen >= Length(ResBuf)) then
        begin
          RSAErrorCode := ECN_RSA_BER_ERROR;
          Exit;
        end;

        // 解开 Ber 内容里的编码与加密算法，不使用 SignType 原始值
        Reader := TCnBerReader.Create(@BerBuf[0], BerLen);
        Reader.ParseToTree;
        if Reader.TotalCount < 5 then
        begin
          RSAErrorCode := ECN_RSA_BER_ERROR;
          Exit;
        end;

        Node := Reader.Items[2];
        SignType := GetDigestSignTypeFromBerOID(Node.BerDataAddress, Node.BerDataLength);
        if SignType = rsdtNone then
        begin
          RSAErrorCode := ECN_RSA_BER_ERROR;
          Exit;
        end;

        if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的散列值
          Exit;

        // 与 Ber 解出的散列值比较
        Node := Reader.Items[4];
        Result := Stream.Size = Node.BerDataLength;
        if Result then
          Result := CompareMem(Stream.Memory, Node.BerDataAddress, Stream.Size);

        RSAErrorCode := ECN_RSA_OK; // 正常进行校验，即使校验不通过也清空错误码
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

// 生成 Diffie-Hellman 密钥协商算法所需的素数与其最小原根，涉及到因素分解因此较慢
function CnDiffieHellmanGeneratePrimeRootByBitsCount(BitsCount: Integer;
  Prime, MinRoot: TCnBigNumber): Boolean;
var
  I: Integer;
  Num, PrimeSubOne: TCnBigNumber;
  Factors: TCnBigNumberList;
begin
  Result := False;
  if BitsCount <= 16 then
  begin
    RSAErrorCode := ECN_RSA_INVALID_BITS;
    Exit;
  end;

  if not BigNumberGeneratePrimeByBitsCount(Prime, BitsCount) then
  begin
    RSAErrorCode := ECN_RSA_BIGNUMBER_ERROR;
    Exit;
  end;

  Factors := TCnBigNumberList.Create;
  PrimeSubOne := BigNumberNew;
  Num := BigNumberNew;

  try
    BigNumberCopy(PrimeSubOne, Prime);
    BigNumberSubWord(PrimeSubOne, 1);
    BigNumberFindFactors(PrimeSubOne, Factors);
    Factors.RemoveDuplicated;

    MinRoot.SetZero;
    for I := 2 to MaxInt do // 不查太大的大数
    begin
      Num.SetWord(I);
      if BigNumberCheckPrimitiveRoot(Num, Prime, Factors) then
      begin
        MinRoot.SetWord(I);
        Result := True;
        Exit;
      end;
    end;
  finally
    Factors.Free;
    BigNumberFree(PrimeSubOne);
    BigNumberFree(Num);
  end;
end;

// 根据自身选择的随机数 PrivateKey 生成 Diffie-Hellman 密钥协商的输出公钥
function CnDiffieHellmanGenerateOutKey(Prime, Root, SelfPrivateKey: TCnBigNumber;
  const OutPublicKey: TCnBigNumber): Boolean;
begin
  // OutPublicKey = (Root ^ SelfPrivateKey) mod Prime
  Result := BigNumberMontgomeryPowerMod(OutPublicKey, Root, SelfPrivateKey, Prime);
end;

// 根据对方发送的 Diffie-Hellman 密钥协商的输出公钥计算生成公认的密钥
function CnDiffieHellmanComputeKey(Prime, SelfPrivateKey, OtherPublicKey: TCnBigNumber;
  const SecretKey: TCnBigNumber): Boolean;
begin
  // SecretKey = (OtherPublicKey ^ SelfPrivateKey) mod Prime
  Result := BigNumberMontgomeryPowerMod(SecretKey, OtherPublicKey, SelfPrivateKey, Prime);
end;

function GetDigestSignTypeFromBerOID(OID: Pointer; OidLen: Integer): TCnRSASignDigestType;
begin
  Result := rsdtNone;
  if (OidLen = SizeOf(OID_SIGN_MD5)) and CompareMem(OID, @OID_SIGN_MD5[0], OidLen) then
    Result := rsdtMD5
  else if (OidLen = SizeOf(OID_SIGN_SHA1)) and CompareMem(OID, @OID_SIGN_SHA1[0], OidLen) then
    Result := rsdtSHA1
  else if (OidLen = SizeOf(OID_SIGN_SHA256)) and CompareMem(OID, @OID_SIGN_SHA256[0], OidLen) then
    Result := rsdtSHA256;
end;

function GetRSADigestNameFromSignDigestType(Digest: TCnRSASignDigestType): string;
begin
  case Digest of
    rsdtNone: Result := '<None>';
    rsdtMD5: Result := 'MD5';
    rsdtSHA1: Result := 'SHA1';
    rsdtSHA256: Result := 'SHA256';
  else
    Result := '<Unknown>';
  end;
end;

// 默认的使用 SHA1 的掩码生成函数
function Pkcs1Sha1MGF(Seed: Pointer; SeedLen: Integer; OutMask: Pointer;
  MaskLen: Integer): Boolean;
var
  I, OutLen, MdLen: Integer;
  Cnt: array[0..3] of Byte;
  Ctx: TSHA1Context;
  Dig: TSHA1Digest;
begin
  Result := False;
  OutLen := 0;
  MdLen := SizeOf(TSHA1Digest);
  if (Seed = nil) or (SeedLen <= 0) then
    Exit;

  if (OutMask = nil) or (MaskLen <= 0) then
    Exit;

  I := 0;
  while OutLen < MaskLen do
  begin
    Cnt[0] := (I shr 24) and $FF;
    Cnt[1] := (I shr 16) and $FF;
    Cnt[2] := (I shr 8) and $FF;
    Cnt[3] := I and $FF;

    SHA1Init(Ctx);
    SHA1Update(Ctx, PAnsiChar(Seed), SeedLen);
    SHA1Update(Ctx, @Cnt[0], SizeOf(Cnt));

    if OutLen + MdLen <= MaskLen then
    begin
      SHA1Final(Ctx, PSHA1Digest(TCnNativeInt(OutMask) + OutLen)^);
      OutLen := OutLen + MdLen;
    end
    else
    begin
      SHA1Final(Ctx, Dig);
      Move(Dig[0], PSHA1Digest(TCnNativeInt(OutMask) + OutLen)^, MaskLen - OutLen);
      OutLen := MaskLen;
    end;

    Inc(I);
  end;
  Result := True;
end;

function AddOaepSha1MgfPadding(ToBuf: PByte; ToLen: Integer; PlainData: PByte;
  DataLen: Integer; DigestParam: PByte = nil; ParamLen: Integer = 0): Boolean;
var
  EmLen, MdLen, I: Integer;
  SeedMask: TSHA1Digest;
  DB, Seed: PByteArray;
  DBMask: array of Byte;
begin
  Result := False;
  EmLen:= ToLen - 1;

  MdLen := SizeOf(TSHA1Digest);

  if (DataLen > EmLen - 2 * MdLen - 1) or (EmLen < 2 * MdLen + 1) then
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  ToBuf^ := 0;
  Seed := PByteArray(TCnNativeInt(ToBuf) + 1);
  DB := PByteArray(TCnNativeInt(ToBuf) + MdLen + 1);

  // 00 | 20 位 Seed | DB
  // 其中 DB := ParamHash || PS || 0x01 || Data，长度是 EmLen - MdLen
  // 后面要 XOR 一次称为 MaskDB
  SeedMask := SHA1Buffer(DigestParam, ParamLen);
  Move(SeedMask[0], DB^, MdLen);

  // To 区 DB 的前 20 字节先留着，后面到尾巴先填满 0
  FillChar(PByte(TCnNativeInt(DB) + MdLen)^, EmLen - DataLen - 2 * MdLen - 1, 0);
  DB^[EmLen - DataLen - MdLen - 1] := 1;

  // 明文搁后面
  Move(PlainData^, PByte(TCnNativeInt(DB) + EmLen - DataLen - MdLen)^, DataLen);

  // To[1] 开始的 20 个字节 Rand 一下
  if not CnRandomFillBytes(PAnsiChar(Seed), MdLen) then
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  SetLength(DBMask, EmLen - MdLen);

  // 随机 Seed 算出 MGF 数据，准备和 DB 做 XOR
  if not Pkcs1Sha1MGF(Seed, MdLen, @DBMask[0], EmLen - MdLen) then
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  for I := 0 to EmLen - MdLen - 1 do
    DB^[I] := DB^[I] xor DBMask[I];        // 得到 Masked DB

  // XOR 过的 Masked DB 再算出 MGF 数据，准备和随机 Seed 做 XOR
  if not Pkcs1Sha1MGF(DB, EmLen - MdLen, @SeedMask[0], MdLen) then
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  for I := 0 to MdLen - 1 do
    Seed^[I] := Seed^[I] xor SeedMask[I];  // 得到 Masked Seed

  SetLength(DBMask, 0);
  Result := True;
  // 最后加密消息 = 00 || maskedSeed || maskedDB
end;

function RemoveOaepSha1MgfPadding(ToBuf: PByte; out OutLen: Integer; EnData: PByte;
  DataLen: Integer; DigestParam: PByte = nil; ParamLen: Integer = 0): Boolean;
var
  I, MdLen, DBLen, MStart: Integer;
  MaskedDB, MaskedSeed: PByteArray;
  Seed, ParamHash: TSHA1Digest;
  DB: array of Byte;
begin
  Result := False;
  if (EnData = nil) or (ToBuf = nil) then
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  if EnData^ <> 0 then  // 首字节必须是 0
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  MdLen := SizeOf(TSHA1Digest);
  DBLen := DataLen - MdLen - 1;
  if DBLen <= 0 then
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  // 找出密文中的 长 MdLen 的 MaskedSeed 以及后面长 DBLen 的 MaskedDB
  MaskedSeed := PByteArray(TCnNativeInt(EnData) + 1);
  MaskedDB := PByteArray(TCnNativeInt(EnData) + MdLen + 1);

  ParamHash := SHA1Buffer(DigestParam, ParamLen);

  // 把 MaskedDB 先算出来
  if not Pkcs1Sha1MGF(@MaskedDB[0], DBLen, @Seed[0], MdLen) then
  begin
    RSAErrorCode := ECN_RSA_PADDING_ERROR;
    Exit;
  end;

  for I := 0 to MdLen - 1 do
    Seed[I] := Seed[I] xor MaskedSeed^[I];  // 得到 Seed

  SetLength(DB, DBLen);
  try
    if not Pkcs1Sha1MGF(@Seed[0], MdLen, @DB[0], DBLen) then
    begin
      RSAErrorCode := ECN_RSA_PADDING_ERROR;
      Exit;
    end;

    for I := 0 to DBLen - 1 do
      DB[I] := DB[I] xor MaskedDB^[I];  // 得到 DB

    // 这里 DB 的前 MdLen 字节应该等于 ParamHash，比较判断之
    if not CompareMem(@DB[0], @ParamHash[0], MdLen) then
    begin
      RSAErrorCode := ECN_RSA_PADDING_ERROR;
      Exit;
    end;

    // 通过后从 DB[MdLen] 开始跳过纯 0 搜 1，搜到 1 后，1 后的到尾巴的就是消息原文
    MStart := -1;
    for I := MdLen to DBLen - 1 do
    begin
      if DB[I] <> 0 then
      begin
        if DB[I] <> 1 then
        begin
          RSAErrorCode := ECN_RSA_PADDING_ERROR;
          Exit;
        end
        else // 0 后的第一个 1
        begin
          // 记录此时的 I + 1
          MStart := I + 1;
          Break;
        end;
      end; // 0 则跳过
    end;

    // DB[MStart] 到 DB[DBLen - 1] 是数据明文
    if (MStart > 0) and (MStart < DBLen) then
    begin
//      没法判断目标区域是否够不够容纳，因为 OutLen 没传进 ToBuf 的实际长度来
//      if DBLen - MStart > OutLen then
//      begin
//        RSAErrorCode := ECN_RSA_PADDING_ERROR;
//        Exit;
//      end;

      Move(DB[MStart], ToBuf^, DBLen - MStart);
      OutLen := DBLen - MStart; // 返回明文数据长度
      Result := True;
    end;
  finally
    SetLength(DB, 0);
  end;
end;

end.
