{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnRSA;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：RSA 算法单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 Int64 及大整数范围内的 RSA 算法，公钥 Exponent 默认固定使用 65537。
*           并基于大整数 RSA 算法实现了公私钥生成、存储、载入与数据加解密、签名验签。 
*           另外官方提倡公钥加密、私钥解密，但 RSA 两者等同，也可私钥加密、公钥解密，
*           本单元两类方法都提供了，使用时须注意配对。
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.11.23 V2.9
*               增加一批针对字节数组的加解密函数
*           2024.09.27 V2.8
*               修正私钥返回位数可能错误的问题
*           2023.12.14 V2.7
*               增加验证公私钥的机制，并完善从文件和流中加载与保存
*           2023.02.16 V2.6
*               实现大数形式的基于离散对数的变色龙杂凑算法
*           2023.02.15 V2.5
*               大数 RSA 加密支持 CRT（中国剩余定理）加速，私钥运算耗时降至三分之一
*           2022.04.26 V2.4
*               修改 Integer 地址转换以支持 MacOS64
*           2021.06.12 V2.1
*               加入 OAEP Padding 的处理
*           2021.05.09 V2.0
*               私钥载入时自动判断 PEM 内是 PKCS1 还是 PKCS8 格式，不依赖于头尾行的横杠注释
*           2020.06.10 V1.9
*               公私钥允许从 Stream 中载入
*           2020.03.27 V1.8
*               公钥允许用 3。实现了加密的 PEM 文件的读写，不过只支持 DES/3DES/AES
*           2020.03.13 V1.7
*               加入详细的错误码。调用返回 False 时可通过 GetLastCnRSAError 获取 ECN_RSA_* 形式的错误码
*           2019.04.19 V1.6
*               支持 Win32/Win64/MacOS32
*           2018.06.15 V1.5
*               支持文件签名与验证，类似于 Openssl 中的用法，有原始签名与杂凑签名两类：
*               openssl rsautl -sign -in hello -inkey rsa.pem -out hello.default.sign.openssl
*               // 私钥原始签名，直接把文件内容补齐后用私钥加密并存储，等同于加密，对应 CnRSASignFile 指定 sdtNone
*               openssl dgst -md5 -sign rsa.pem -out hello.md5.sign.openssl hello
*               openssl dgst -sha1 -sign rsa.pem -out hello.sha1.sign.openssl hello
*               openssl dgst -sha256 -sign rsa.pem -out hello.sha256.sign.openssl hello
*               // 私钥杂凑签名，可指定杂凑算法，默认 md5。对应 CnRSASignFile 并指定杂凑算法。
*               // 原始文件杂凑值经过 BER 编码再 PKCS1 补齐后私钥加密并存储成签名文件
*               openssl dgst -verify rsa_pub.pem -signature hello.sign.openssl hello
*               // 公钥杂凑验证原始文件与签名文件，杂凑算法类型在签名文件中。
*               // 对应 CnRSAVerify，公钥解开签名文件后去除 PKCS1 对齐再解开 BER 编码并比对杂凑值
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
*               创建单元，Int64 范围内的 RSA 从 CnPrime 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// 私钥加解密如使用 CRT 进行计算加速，1024 位的私钥运算能够将耗时降低至三分之一
// 但仅在部分场合使用

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnConsts, CnPrime,
  CnBigNumber, CnBerUtils, CnPemUtils, CnNative, CnMD5, CnSHA1, CnSHA2, CnSM3;

const
  // 以下 OID 都预先写死，不动态计算编码了
  CN_OID_RSAENCRYPTION_PKCS1: array[0..8] of Byte = ( // $2A = 40 * 1 + 2
    $2A, $86, $48, $86, $F7, $0D, $01, $01, $01
  );
  {* RSA PKCS1 的 OID 编码，实际值为 1.2.840.113549.1.1.1}

  // 错误码
  ECN_RSA_OK                           = ECN_OK;
  {* RSA 系列错误码：无错误，值为 0}

  ECN_RSA_ERROR_BASE                   = ECN_CUSTOM_ERROR_BASE + $100;
  {* RSA 系列错误码的基准起始值，为 ECN_CUSTOM_ERROR_BASE 加上 $100}

  ECN_RSA_INVALID_INPUT                = ECN_RSA_ERROR_BASE + 1;
  {* RSA 错误码之输入为空或长度错误}
  ECN_RSA_INVALID_BITS                 = ECN_RSA_ERROR_BASE + 2;
  {* RSA 错误码之密钥位数错误}
  ECN_RSA_BIGNUMBER_ERROR              = ECN_RSA_ERROR_BASE + 3;
  {* RSA 错误码之大数运算错误}
  ECN_RSA_BER_ERROR                    = ECN_RSA_ERROR_BASE + 4;
  {* RSA 错误码之 BER 格式编码错误}
  ECN_RSA_PADDING_ERROR                = ECN_RSA_ERROR_BASE + 5;
  {* RSA 错误码之 PADDING 对齐错误}
  ECN_RSA_DIGEST_ERROR                 = ECN_RSA_ERROR_BASE + 6;
  {* RSA 错误码之数字摘要错误}
  ECN_RSA_PEM_FORMAT_ERROR             = ECN_RSA_ERROR_BASE + 7;
  {* RSA 错误码之 PEM 格式错误}
  ECN_RSA_PEM_CRYPT_ERROR              = ECN_RSA_ERROR_BASE + 8;
  {* RSA 错误码之 PEM 加解密错误}

type
  TCnRSASignDigestType = (rsdtNone, rsdtMD5, rsdtSHA1, rsdtSHA256, rsdtSM3);
  {* RSA 签名所支持的杂凑摘要类型，可无摘要}

  TCnRSAKeyType = (cktPKCS1, cktPKCS8);
  {* RSA 密钥文件格式。注意它和 CnECC 中的 TCnEccKeyType 名字重复，使用时要注意}

  TCnRSAPaddingMode = (cpmPKCS1, cpmOAEP);
  {* RSA 加密的填充模式，PKCS1 适合加解密，包括三种填充类型，
    OAEP 只适用于公钥加密，默认使用 SHA1 作为掩码生成杂凑算法}

  TCnRSAPrivateKey = class(TPersistent)
  {* RSA 私钥}
  private
    FUseCRT: Boolean;
    FPrimeKey1: TCnBigNumber;
    FPrimeKey2: TCnBigNumber;
    FPrivKeyProduct: TCnBigNumber;
    FPrivKeyExponent: TCnBigNumber;
    FDP1: TCnBigNumber;  // CRT 加速的三个中间变量
    FDQ1: TCnBigNumber;
    FQInv: TCnBigNumber;
    function GetBitsCount: Integer;
    function GetBytesCount: Integer;
  public
    constructor Create(CRT: Boolean = False); virtual;
    {* 构造函数。

       参数：
         CRT: Boolean                     - 是否使用 CRT 在特定场合进行私钥加密运算加速，默认不使用
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent              - 欲从之赋值的源对象

       返回值：（无）
    }

    procedure Clear;
    {* 清空值}
    procedure UpdateCRT;
    {* 使用 CRT 加速时供外部的调用以在私钥数据更新时同步更新内部 CRT 数据}

    property PrimeKey1: TCnBigNumber read FPrimeKey1 write FPrimeKey1;
    {* 大素数 1，p，要求比 q 大}
    property PrimeKey2: TCnBigNumber read FPrimeKey2 write FPrimeKey2;
    {* 大素数 2，q，要求比 p 小}
    property PrivKeyProduct: TCnBigNumber read FPrivKeyProduct write FPrivKeyProduct;
    {* 俩素数乘积 n，也叫 Modulus，生成时其位数需严格等于所需安全位数}
    property PrivKeyExponent: TCnBigNumber read FPrivKeyExponent write FPrivKeyProduct;
    {* 私钥指数 d}
    property BitsCount: Integer read GetBitsCount;
    {* 密钥的位数，也即素数乘积 n 的有效位数}
    property BytesCount: Integer read GetBytesCount;
    {* 密钥的字节数，等于素数乘积 n 的有效位数除以 8}
    property UseCRT: Boolean read FUseCRT;
    {* 是否使用 CRT 进行运算加速}
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
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent              - 欲从之赋值的源对象

       返回值：（无）
    }

    procedure Clear;
    {* 清空值}

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
  out PrivKeyProduct: TUInt64; out PrivKeyExponent: TUInt64; out PubKeyProduct: TUInt64;
  out PubKeyExponent: TUInt64; HighBitSet: Boolean = True): Boolean;
{* 生成 RSA 算法所需的一对公私钥，素数均不大于 Cardinal，Keys 均不大于 UInt64，
   HighBitSet 为 True 时要求素数最高位为 1，且乘积是 64 Bit。

   参数：
     out PrimeKey1: Cardinal              - 生成的素数一
     out PrimeKey2: Cardinal              - 生成的素数二
     out PrivKeyProduct: TUInt64          - 生成的私钥的素数积
     out PrivKeyExponent: TUInt64         - 生成的私钥指数
     out PubKeyProduct: TUInt64           - 生成的公钥的素数积
     out PubKeyExponent: TUInt64          - 生成的公钥指数，固定使用 65537
     HighBitSet: Boolean                  - 是否要求素数最高位为 1

   返回值：Boolean                        - 返回生成是否成功
}

function CnInt64RSAEncrypt(Data: TUInt64; PrivKeyProduct: TUInt64;
  PrivKeyExponent: TUInt64; out Res: TUInt64): Boolean;
{* 利用 RSA 私钥对数据 Data 进行加密，加密结果写入 Res。返回加密是否成功。

   参数：
     Data: TUInt64                        - 待加密的数据
     PrivKeyProduct: TUInt64              - RSA 私钥的素数积
     PrivKeyExponent: TUInt64             - RSA 私钥指数
     out Res: TUInt64                     - 输出的加密结果

   返回值：Boolean                        - 返回加密是否成功
}

function CnInt64RSADecrypt(Res: TUInt64; PubKeyProduct: TUInt64;
  PubKeyExponent: TUInt64; out Data: TUInt64): Boolean;
{* 利用 RSA 公钥对数据 Res 进行解密，解密结果写入 Data。返回解密是否成功

   参数：
     Res: TUInt64                         - 待解密的数据
     PubKeyProduct: TUInt64               - RSA 公钥素数积
     PubKeyExponent: TUInt64              - RSA 公钥指数
     out Data: TUInt64                    - 输出的解密结果

   返回值：Boolean                        - 返回解密是否成功
}

// 大数范围内的 RSA 加解密实现

function CnRSAGenerateKeysByPrimeBits(PrimeBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; PublicKeyUse3: Boolean = False): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 生成 RSA 算法所需的一对公私钥，PrimeBits 是素数的二进制位数，其余参数均为生成。
   PrimeBits 取值为 512/1024/2048等，注意目前不是乘积的范围。内部缺乏安全判断。不推荐使用。
   PublicKeyUse3 为 True 时公钥指数用 3，否则用 65537

   参数：
     PrimeBits: Integer                   - 需生成的素数的位数
     PrivateKey: TCnRSAPrivateKey         - 生成的 RSA 私钥
     PublicKey: TCnRSAPublicKey           - 生成的 RSA 公钥
     PublicKeyUse3: Boolean               - 公钥是否使用 3，否则 65537，默认后者

   返回值：Boolean                        - 返回生成是否成功
}

function CnRSAGenerateKeys(ModulusBits: Integer; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; PublicKeyUse3: Boolean = False): Boolean;
{* 生成 RSA 算法所需的一对公私钥，ModulusBits 是素数乘积的二进制位数，其余参数均为生成。
   ModulusBits 取值为 512/1024/2048等。内部有安全判断。
   PublicKeyUse3 为 True 时公钥指数用 3，否则用 65537

   参数：
     ModulusBits: Integer                 - 需生成的素数乘积位数
     PrivateKey: TCnRSAPrivateKey         - 生成的 RSA 私钥
     PublicKey: TCnRSAPublicKey           - 生成的 RSA 公钥
     PublicKeyUse3: Boolean               - 公钥是否使用 3，否则 65537，默认后者

   返回值：Boolean                        - 返回生成是否成功
}

function CnRSAVerifyKeys(PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
{* 验证一对 RSA 公私钥是否配套。

   参数：
     PrivateKey: TCnRSAPrivateKey         - 待验证的 RSA 私钥
     PublicKey: TCnRSAPublicKey           - 待验证的 RSA 公钥

   返回值：Boolean                        - 返回验证是否成功
}

function CnRSALoadKeysFromPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的文件中加载一对 RSA 公私钥数据，如某钥参数为空则不载入。
   自动判断 PKCS1 还是 PKCS8，不依赖于头尾行的 ----- 注释。

   参数：
     const PemFileName: string            - 待加载的 PEM 文件名
     PrivateKey: TCnRSAPrivateKey         - 加载后的内容存入该 RSA 私钥
     PublicKey: TCnRSAPublicKey           - 加载后的内容存入该 RSA 公钥
     KeyHashMethod: TCnKeyHashMethod      - PEM 文件如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 自动判断
     const Password: string               - PEM 文件如加密，此处应传对应的密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnRSALoadKeysFromPem(PemStream: TStream; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的流中加载一对 RSA 公私钥数据，如某钥参数为空则不载入。
   自动判断 PKCS1 还是 PKCS8，不依赖于头尾行的 ----- 注释。

   参数：
     PemStream: TStream                   - 待加载的 PEM 格式的流
     PrivateKey: TCnRSAPrivateKey         - 加载后的内容存入该 RSA 私钥
     PublicKey: TCnRSAPublicKey           - 加载后的内容存入该 RSA 公钥
     KeyHashMethod: TCnKeyHashMethod      - PEM 流如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 内容自动判断
     const Password: string               - PEM 流如加密，此处应传对应的密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnRSASaveKeysToPem(const PemFileName: string; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS1;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 将一对 RSA 公私钥写入 PEM 格式的文件中，返回是否成功。

   参数：
     const PemFileName: string                            - 待保存的 PEM 文件名
     PrivateKey: TCnRSAPrivateKey                         - 待保存的 RSA 私钥
     PublicKey: TCnRSAPublicKey                           - 待保存的 RSA 公钥
     KeyType: TCnRSAKeyType                               - 待保存的 RSA 公私钥的 PEM 格式，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 文件的加密模式，默认不加密，并忽略后面两个参数
     KeyHashMethod: TCnKeyHashMethod                      - 保存的 PEM 文件如需加密，此处设置其加密杂凑算法，默认 MD5
     const Password: string                               - 保存的 PEM 文件如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

function CnRSASaveKeysToPem(PemStream: TStream; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS1;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 将一对 RSA 公私钥写入 PEM 格式的流中，返回是否成功。

   参数：
     PemStream: TStream                                   - 待保存的 PEM 格式的流
     PrivateKey: TCnRSAPrivateKey                         - 待保存的 RSA 私钥
     PublicKey: TCnRSAPublicKey                           - 待保存的 RSA 公钥
     KeyType: TCnRSAKeyType                               - 待保存的 RSA 公私钥的 PEM 格式，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 流的加密模式，默认不加密，并忽略后面两个参数
     KeyHashMethod: TCnKeyHashMethod                      - 保存的 PEM 流如加密，此处设置其加密杂凑算法，默认 MD5
     const Password: string                               - 保存的 PEM 流如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

function CnRSALoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的文件中加载 RSA 公钥数据，返回是否成功。

   参数：
     const PemFileName: string            - 待加载的 PEM 文件名
     PublicKey: TCnRSAPublicKey           - 加载后的内容存入该 RSA 公钥
     KeyHashMethod: TCnKeyHashMethod      - PEM 文件如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 内容自动判断
     const Password: string               - PEM 文件如加密，此处应传对应密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnRSALoadPublicKeyFromPem(const PemStream: TStream;
  PublicKey: TCnRSAPublicKey; KeyHashMethod: TCnKeyHashMethod = ckhMd5;
  const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的流中加载 RSA 公钥数据，返回是否成功。

   参数：
     const PemStream: TStream             - 待加载的 PEM 格式的流
     PublicKey: TCnRSAPublicKey           - 加载后的内容存入该 RSA 公钥
     KeyHashMethod: TCnKeyHashMethod      - PEM 流如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 内容自动判断
     const Password: string               - PEM 流如加密，此处应传对应密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnRSASavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS8;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  const Password: string = ''): Boolean; overload;
{* 将 RSA 公钥写入 PEM 格式的文件中，返回是否成功。

   参数：
     const PemFileName: string                            - 待保存的 PEM 文件名
     PublicKey: TCnRSAPublicKey                           - 待保存的 RSA 公钥
     KeyType: TCnRSAKeyType                               - 待保存的 RSA 公钥的 PEM 格式，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 文件的加密模式，默认不加密，并忽略后面的参数
     const Password: string                               - 保存的 PEM 文件如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

function CnRSASavePublicKeyToPem(PemStream: TStream;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType = cktPKCS8;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  const Password: string = ''): Boolean; overload;
{* 将 RSA 公钥写入 PEM 格式的流中，返回是否成功。

   参数：
     PemStream: TStream                                   - 待保存的 PEM 格式的流
     PublicKey: TCnRSAPublicKey                           - 待保存的 RSA 公钥
     KeyType: TCnRSAKeyType                               - 待保存的 RSA 公钥的 PEM 格式，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 流的加密模式，默认不加密，并忽略后面的参数
     const Password: string                               - 保存的 PEM 流如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean; overload;
{* 使用 RSA 私钥对数据 Data 进行加密，加密结果写入 Res。返回加密是否成功。
   该方法内部会根据私钥的 CRT 设置决定是否启用 CRT 加速。

   参数：
     Data: TCnBigNumber                   - 待加密的大数数据
     PrivateKey: TCnRSAPrivateKey         - 用于加密的 RSA 私钥
     Res: TCnBigNumber                    - 输出的加密后的大数数据

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSAEncrypt(Data: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Res: TCnBigNumber): Boolean; overload;
{* 利用 RSA 公钥对数据 Data 进行加密，加密结果写入 Res。返回加密是否成功。

   参数：
     Data: TCnBigNumber                   - 待加密的大数数据
     PublicKey: TCnRSAPublicKey           - 用于加密的 RSA 公钥
     Res: TCnBigNumber                    - 输出的加密后的大数数据

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSADecrypt(Res: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Data: TCnBigNumber): Boolean; overload;
{* 利用 RSA 私钥对数据 Res 进行解密，解密结果写入 Data。返回解密是否成功。
   该方法内部会根据私钥的 CRT 设置决定是否启用 CRT 加速。

   参数：
     Res: TCnBigNumber                    - 输出的解密后的大数数据
     PrivateKey: TCnRSAPrivateKey         - 用于解密的 RSA 私钥
     Data: TCnBigNumber                   - 待解密的大数数据

   返回值：Boolean                        - 返回解密是否成功
}

function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean; overload;
{* 利用 RSA 公钥对数据 Res 进行解密，解密结果写入 Data。返回解密是否成功

   参数：
     Res: TCnBigNumber                    - 输出的解密后的大数数据
     PublicKey: TCnRSAPublicKey           - 用于解密的 RSA 公钥
     Data: TCnBigNumber                   - 待解密的大数数据

   返回值：Boolean                        - 返回解密是否成功
}

// ======================== RSA 数据与文件加解密实现 ===========================

function CnRSAEncryptRawData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对数据块进行加密，无填充，结果放 OutBuf 中，并返回密文数据字节长度。

   参数：
     PlainData: Pointer                   - 待加密的明文数据块地址
     DataByteLen: Integer                 - 待加密的明文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的密文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     out OutByteLen: Integer              - 返回的密文实际字节长度
     PublicKey: TCnRSAPublicKey           - 用于加密的 RSA 公钥

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSAEncryptRawData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对数据块进行加密，无填充，结果放 OutBuf 中，并返回密文数据字节长度。

   参数：
     PlainData: Pointer                   - 待加密的明文数据块地址
     DataByteLen: Integer                 - 待加密的明文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的密文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     out OutByteLen: Integer              - 返回的密文实际字节长度
     PrivateKey: TCnRSAPrivateKey         - 用于加密的 RSA 私钥

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSADecryptRawData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对数据块进行无填充解密，结果放 OutBuf 中，并返回明文数据字节长度。

   参数：
     EnData: Pointer                      - 待解密的密文数据块地址
     DataByteLen: Integer                 - 待解密的密文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的明文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     out OutByteLen: Integer              - 返回的明文实际字节长度
     PublicKey: TCnRSAPublicKey           - 用于解密的 RSA 公钥

   返回值：Boolean                        - 返回解密是否成功
}

function CnRSADecryptRawData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对数据块进行无填充解密结果放 OutBuf 中，并返回明文数据字节长度。

   参数：
     EnData: Pointer                      - 待解密的密文数据块地址
     DataByteLen: Integer                 - 待解密的密文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的明文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     out OutByteLen: Integer              - 返回的明文实际字节长度
     PrivateKey: TCnRSAPrivateKey         - 用于解密的 RSA 私钥

   返回值：Boolean                        - 返回解密是否成功
}

function CnRSAEncryptRawBytes(PlainData: TBytes; PublicKey: TCnRSAPublicKey): TBytes; overload;
{* 用公钥对字节数组进行无填充加密，返回加密的字节数组。

   参数：
     PlainData: TBytes                    - 待加密的明文字节数组
     PublicKey: TCnRSAPublicKey           - 用于加密的 RSA 公钥

   返回值：TBytes                         - 返回密文字节数组
}

function CnRSAEncryptRawBytes(PlainData: TBytes; PrivateKey: TCnRSAPrivateKey): TBytes; overload;
{* 用私钥对字节数组进行无填充加密，返回加密的字节数组。

   参数：
     PlainData: TBytes                    - 待加密的明文字节数组
     PrivateKey: TCnRSAPrivateKey         - 用于加密的 RSA 私钥

   返回值：TBytes                         - 返回密文字节数组
}

function CnRSADecryptRawBytes(EnData: TBytes; PublicKey: TCnRSAPublicKey): TBytes; overload;
{* 用公钥对字节数组进行无填充解密，返回解密的字节数组。

   参数：
     EnData: TBytes                       - 待解密的密文字节数组
     PublicKey: TCnRSAPublicKey           - 用于解密的 RSA 公钥

   返回值：TBytes                         - 返回明文字节数组
}

function CnRSADecryptRawBytes(EnData: TBytes; PrivateKey: TCnRSAPrivateKey): TBytes; overload;
{* 用私钥对字节数组进行无填充解密，返回解密的字节数组。

   参数：
     EnData: TBytes                       - 待解密的密文字节数组
     PrivateKey: TCnRSAPrivateKey         - 用于解密的 RSA 私钥

   返回值：TBytes                         - 返回明文字节数组
}

function CnRSAEncryptData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用公钥对数据块进行加密，加密前可指定使用 PKCS1 填充或 OAEP 填充，结果放 OutBuf 中。
   因为有填充，返回的密文字节长度固定为密钥素数积的字节长度。

   参数：
     PlainData: Pointer                   - 待加密的明文数据块地址
     DataByteLen: Integer                 - 待加密的明文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的密文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     PublicKey: TCnRSAPublicKey           - 用于加密的 RSA 公钥
     PaddingMode: TCnRSAPaddingMode       - 指定对齐模式

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSAEncryptData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对数据块进行加密，加密前使用 PKCS1 填充，结果放 OutBuf 中。
   因为有填充，返回的密文字节长度固定为密钥素数积的字节长度。

   参数：
     PlainData: Pointer                   - 待加密的明文数据块地址
     DataByteLen: Integer                 - 待加密的明文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的密文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     PrivateKey: TCnRSAPrivateKey         - 用于加密的 RSA 私钥

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSADecryptData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对数据块进行解密，并解开 PKCS1 填充，结果放 OutBuf 中，并返回明文数据字节长度。

   参数：
     EnData: Pointer                      - 待解密的密文数据块地址
     DataByteLen: Integer                 - 待解密的密文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的明文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     out OutByteLen: Integer              - 返回的明文实际字节长度
     PublicKey: TCnRSAPublicKey           - 用于解密的 RSA 公钥

   返回值：Boolean                        - 返回解密是否成功
}

function CnRSADecryptData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PrivateKey: TCnRSAPrivateKey;
  PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用私钥对数据块进行解密，并解开其 PKCS1 填充或 OAEP 填充，结果放 OutBuf 中，并返回明文数据字节长度。

   参数：
     EnData: Pointer                      - 待加密的明文数据块地址
     DataByteLen: Integer                 - 待加密的明文数据块字节长度
     OutBuf: Pointer                      - 容纳返回的明文数据的数据块的地址，其字节长度不能短于密钥素数积的字节长度
     out OutByteLen: Integer              - 返回的明文实际字节长度
     PrivateKey: TCnRSAPrivateKey         - 用于加密的 RSA 私钥
     PaddingMode: TCnRSAPaddingMode       - 指定对齐模式，需和密文的实际情况一致

   返回值：Boolean                        - 返回解密是否成功
}

function CnRSAEncryptBytes(PlainData: TBytes; PublicKey: TCnRSAPublicKey;
  PaddingMode: TCnRSAPaddingMode = cpmPKCS1): TBytes; overload;
{* 用公钥对字节数组进行加密，加密前可指定使用 PKCS1 填充或 OAEP 填充，返回加密的字节数组。

   参数：
     PlainData: TBytes                    - 待加密的明文字节数组
     PublicKey: TCnRSAPublicKey           - 用于加密的 RSA 公钥
     PaddingMode: TCnRSAPaddingMode       - 指定对齐模式

   返回值：TBytes                         - 返回密文字节数组
}

function CnRSAEncryptBytes(PlainData: TBytes; PrivateKey: TCnRSAPrivateKey): TBytes; overload;
{* 用私钥对字节数组进行加密，加密前使用 PKCS1 填充，返回加密的字节数组。

   参数：
     PlainData: TBytes                    - 待加密的明文字节数组
     PrivateKey: TCnRSAPrivateKey         - 用于加密的 RSA 私钥

   返回值：TBytes                         - 返回密文字节数组
}

function CnRSADecryptBytes(EnData: TBytes; PublicKey: TCnRSAPublicKey): TBytes; overload;
{* 用公钥对字节数组进行解密，并解开 PKCS1 填充，返回解密的字节数组。

   参数：
     EnData: TBytes                       - 待解密的密文字节数组
     PublicKey: TCnRSAPublicKey           - 用于解密的 RSA 公钥

   返回值：TBytes                         - 返回明文字节数组
}

function CnRSADecryptBytes(EnData: TBytes; PrivateKey: TCnRSAPrivateKey;
  PaddingMode: TCnRSAPaddingMode = cpmPKCS1): TBytes; overload;
{* 用私钥对字节数组进行解密，并解开其 PKCS1 填充或 OAEP 填充，返回解密的字节数组。

   参数：
     EnData: TBytes                       - 待解密的密文字节数组
     PrivateKey: TCnRSAPrivateKey         - 用于解密的 RSA 私钥
     PaddingMode: TCnRSAPaddingMode       - 指定对齐模式，需和密文文件的实际情况一致

   返回值：TBytes                         - 返回明文字节数组
}

function CnRSAEncryptFile(const InFileName: string; const OutFileName: string;
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用公钥对文件进行加密，加密前可指定使用 PKCS1 填充或 OAEP 填充，结果存输出文件中。

   参数：
     const InFileName: string             - 待加密的明文文件名
     const OutFileName: string            - 加密后输出的密文文件名
     PublicKey: TCnRSAPublicKey           - 用于加密的 RSA 公钥
     PaddingMode: TCnRSAPaddingMode       - 指定对齐模式

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSAEncryptFile(const InFileName: string; const OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean; overload;
{* 用私钥对文件进行加密，加密前使用 PKCS1 填充，结果存输出文件中。

   参数：
     const InFileName: string             - 待加密的明文文件名
     const OutFileName: string            - 加密后输出的密文文件名
     PrivateKey: TCnRSAPrivateKey         - 用于加密的 RSA 私钥

   返回值：Boolean                        - 返回加密是否成功
}

function CnRSADecryptFile(const InFileName: string; const OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean; overload;
{* 用公钥对文件进行解密，并解开其 PKCS1 填充，结果存输出文件中，注意不支持 OAEP 填充。

   参数：
     const InFileName: string             - 待解密的密文文件名
     const OutFileName: string            - 解密后输出的明文文件名
     PublicKey: TCnRSAPublicKey           - 用于解密的 RSA 公钥

   返回值：Boolean                        - 返回解密是否成功
}

function CnRSADecryptFile(const InFileName: string; const OutFileName: string;
  PrivateKey: TCnRSAPrivateKey; PaddingMode: TCnRSAPaddingMode = cpmPKCS1): Boolean; overload;
{* 用私钥对文件进行解密，并解开其 PKCS1 填充或 OAEP 填充，结果存输出文件中。

   参数：
     const InFileName: string             - 待解密的密文文件名
     const OutFileName: string            - 解密后输出的明文文件名
     PrivateKey: TCnRSAPrivateKey         - 用于解密的 RSA 私钥
     PaddingMode: TCnRSAPaddingMode       - 指定对齐模式，需和密文文件的实际情况一致

   返回值：Boolean                        - 返回解密是否成功
}

// =========================== RSA 文件签名与验证实现 ==========================
//
// 流与文件分开实现是因为计算文件摘要时支持大文件，而 FileStream 低版本不支持
//
// 注意 RSA 签名是先杂凑再拼一段数据用 RSA 私钥加密，验证时能解出杂凑值，
// 这点和 ECC 签名不同：ECC 签名并不解出 Hash 值，而是通过中间运算比对大数。

function CnRSASignFile(const InFileName: string; const OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用 RSA 私钥签名指定文件，签名结果直接存储至 OutSignFileName 文件中，返回签名是否成功。
   未指定签名杂凑摘要类型时，等于将源文件用 PKCS1 Private_FF 补齐后加密。
   当指定了签名杂凑摘要类型时，使用指定签名杂凑摘要算法对文件进行计算得到杂凑值，
   再将原始的二进制杂凑值进行 BER 编码，再 PKCS1 补齐再用私钥加密。

   参数：
     const InFileName: string             - 待签名的文件名
     const OutSignFileName: string        - 签名内容的输出文件名
     PrivateKey: TCnRSAPrivateKey         - 用于签名的 RSA 私钥
     SignType: TCnRSASignDigestType       - 指定签名杂凑摘要类型

   返回值：Boolean                        - 返回签名是否成功
}

function CnRSAVerifyFile(const InFileName: string; const InSignFileName: string;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用 RSA 公钥与签名值文件验证指定文件，也即用指定签名杂凑摘要算法对文件进行计算得到杂凑值，
   并用公钥解密签名内容并解开 PKCS1 补齐再解开 BER 编码得到杂凑算法与杂凑值，
   并比对两个二进制杂凑值是否相同，返回验证是否通过。

   参数：
     const InFileName: string             - 待验证签名的文件名
     const InSignFileName: string         - 签名内容的文件名
     PublicKey: TCnRSAPublicKey           - 用于验证签名的 RSA 公钥
     SignType: TCnRSASignDigestType       - 指定签名杂凑摘要类型

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnRSASignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用 RSA 私钥签名指定内存流，签名值写入 OutSignStream 中，返回签名是否成功

   参数：
     InStream: TMemoryStream              - 待签名的内存流
     OutSignStream: TMemoryStream         - 输出的签名内容内存流
     PrivateKey: TCnRSAPrivateKey         - 用于签名的 RSA 私钥
     SignType: TCnRSASignDigestType       - 指定签名杂凑摘要类型

   返回值：Boolean                        - 返回签名是否成功
}

function CnRSAVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用 RSA 公钥与签名值内存流验证指定内存流，返回验证是否通过

   参数：
     InStream: TMemoryStream              - 待验证签名的内存流
     InSignStream: TMemoryStream          - 签名内容内存流
     PublicKey: TCnRSAPublicKey           - 用于验证签名的 RSA 公钥
     SignType: TCnRSASignDigestType       - 指定签名杂凑摘要类型

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnRSASignBytes(InData: TBytes; PrivateKey: TCnRSAPrivateKey;
  SignType: TCnRSASignDigestType = rsdtMD5): TBytes;
{* 用 RSA 私钥签名字节数组，返回签名值的字节数组，如签名失败则返回空。

   参数：
     InData: TBytes                       - 待签名的字节数组
     PrivateKey: TCnRSAPrivateKey         - 用于签名的 RSA 私钥
     SignType: TCnRSASignDigestType       - 指定签名杂凑摘要类型

   返回值：TBytes                         - 返回签名内容的字节数组，失败则返回空
}

function CnRSAVerifyBytes(InData: TBytes; InSignBytes: TBytes;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType = rsdtMD5): Boolean;
{* 用 RSA 公钥与签名字节数组验证指定字节数组，返回验证是否通过。

   参数：
     InData: TBytes                       - 待验证签名的字节数组
     InSignBytes: TBytes                  - 签名内容字节数组
     PublicKey: TCnRSAPublicKey           - 用于验证签名的 RSA 公钥
     SignType: TCnRSASignDigestType       - 指定签名杂凑摘要类型

   返回值：Boolean                        - 返回验证签名是否成功
}

// OAEP Padding 的生成与验证算法

function AddOaepSha1MgfPadding(ToBuf: PByte; ToByteLen: Integer; PlainData: PByte;
  DataByteLen: Integer; DigestParam: PByte = nil; ParamByteLen: Integer = 0): Boolean;
{* 对 Data 里 DataLen 的数据进行 OAEP 填充，内容放到 ToBuf 的 ToByteLen 里，返回填充是否成功。
   默认使用 SHA1 对 DigestBuf 内容进行杂凑，ToByteLen 一般是 RSA 的素数积的字节数。

   参数：
     ToBuf: PByte                         - 待容纳填充后的数据的数据块地址
     ToByteLen: Integer                   - 待容纳填充后的数据的数据块字节长度，需足够大
     PlainData: PByte                     - 待进行 OAEP 填充的数据块地址
     DataByteLen: Integer                 - 待进行 OAEP 填充的数据块字节长度
     DigestParam: PByte                   - 额外进行杂凑拼接的数据块地址
     ParamByteLen: Integer                - 额外进行杂凑拼接的数据块字节长度

   返回值：Boolean                        - 返回填充是否成功
}

function RemoveOaepSha1MgfPadding(ToBuf: PByte; out OutByteLen: Integer; EnData: PByte;
  DataByteLen: Integer; DigestParam: PByte = nil; ParamByteLen: Integer = 0): Boolean;
{* 对 EnData 里 DataLen 的数据进行 OAEP 检验并去除填充，内容放到 ToBuf 的 OutByteLen 里，返回检查是否成功。
   ToBuf 能容纳的实际长度不能太短，如成功，OutByteLen 返回明文数据长度
   默认使用 SHA1 对 DigestBuf 内容进行杂凑，DataByteLen 要求是 RSA 的密钥的积的字节数。

   参数：
     ToBuf: PByte                         - 容纳返回的明文数据的数据块的地址
     out OutByteLen: Integer              - 如去除填充成功，返回明文数据长度
     EnData: PByte                        - 待进行 OAEP 验证的数据块地址
     DataByteLen: Integer                 - 待进行 OAEP 验证的数据块字节长度
     DigestParam: PByte                   - 额外进行杂凑拼接的数据块地址
     ParamByteLen: Integer                - 额外进行杂凑拼接的数据块字节长度

   返回值：Boolean                        - 返回去除填充是否成功
}

// ================ Diffie-Hellman 离散对数密钥交换算法 ========================

function CnDiffieHellmanGeneratePrimeRootByBitsCount(BitsCount: Integer;
  Prime: TCnBigNumber; MinRoot: TCnBigNumber): Boolean;
{* 生成 Diffie-Hellman 密钥协商算法所需的素数与其最小原根，实际等同于变色龙杂凑函数。
  涉及到因素分解因此较慢。原根也就是该素域的生成元，也就是各次幂求余能遍历素数以下所有值。

   参数：
     BitsCount: Integer                   - 所需生成的素数的位数
     Prime: TCnBigNumber                  - 生成的用于 Diffie-Hellman 密钥协商的素数
     MinRoot: TCnBigNumber                - 生成的用于 Diffie-Hellman 密钥协商的最小原根

   返回值：Boolean                        - 返回生成是否成功
}

function CnDiffieHellmanGenerateOutKey(Prime: TCnBigNumber; Root: TCnBigNumber;
  SelfPrivateKey: TCnBigNumber; OutPublicKey: TCnBigNumber): Boolean;
{* 根据自身选择的随机数 PrivateKey 生成 Diffie-Hellman 密钥协商的输出公钥，需要双方各自调用。
   其中 OutPublicKey = (Root ^ SelfPrivateKey) mod Prime。
   要保证安全，可以使用 CnSecretSharing 单元中定义的 CN_PRIME_FFDHE_* 素数，对应原根均为 2。

   参数：
     Prime: TCnBigNumber                  - Diffie-Hellman 密钥协商的素数
     Root: TCnBigNumber                   - Diffie-Hellman 密钥协商的原根
     SelfPrivateKey: TCnBigNumber         - 自身选择的随机私钥
     OutPublicKey: TCnBigNumber           - 生成的输出公钥，需传输给对方

   返回值：Boolean                        - 返回生成是否成功
}

function CnDiffieHellmanComputeKey(Prime: TCnBigNumber; SelfPrivateKey: TCnBigNumber;
  OtherPublicKey: TCnBigNumber; SecretKey: TCnBigNumber): Boolean;
{* 根据对方发送的 Diffie-Hellman 密钥协商的输出公钥计算生成协商的密钥，需要双方各自调用。
   其中 SecretKey = (OtherPublicKey ^ SelfPrivateKey) mod Prime。
   要保证安全，可以使用 CnSecretSharing 单元中定义的 CN_PRIME_FFDHE_* 素数，对应原根均为 2

   参数：
     Prime: TCnBigNumber                  - Diffie-Hellman 密钥协商的素数
     SelfPrivateKey: TCnBigNumber         - 自身选择的随机私钥
     OtherPublicKey: TCnBigNumber         - 从对方处传输而来的公钥
     SecretKey: TCnBigNumber              - 生成的协商密钥

   返回值：Boolean                        - 返回生成是否成功
}

// ====================== 基于离散对数的变色龙杂凑函数 =========================

function CnChameleonHashGeneratePrimeRootByBitsCount(BitsCount: Integer;
  Prime: TCnBigNumber; MinRoot: TCnBigNumber): Boolean;
{* 生成基于离散对数的变色龙杂凑函数所需的素数与其最小原根，实际等同于 Diffie-Hellman，
   涉及到因素分解因此较慢。原根也就是该素域的生成元，也就是各次幂求余能遍历素数以下所有值。

   参数：
     BitsCount: Integer                   - 所需生成的素数的位数
     Prime: TCnBigNumber                  - 生成的用于变色龙杂凑的素数
     MinRoot: TCnBigNumber                - 生成的用于变色龙杂凑的最小原根

   返回值：Boolean                        - 返回生成是否成功
}

function CnChameleonHashCalcDigest(InData: TCnBigNumber; InRandom: TCnBigNumber;
  InSecretKey: TCnBigNumber; OutHash: TCnBigNumber; Prime: TCnBigNumber; Root: TCnBigNumber): Boolean;
{* 基于普通离散对数的变色龙杂凑函数，根据一随机值与一 SecretKey，生成指定消息的杂凑。
   其中，Prime 和 Root 可由上面 CnDiffieHellmanGeneratePrimeRootByBitsCount 生成。

   参数：
     InData: TCnBigNumber                 - 待计算杂凑的数据
     InRandom: TCnBigNumber               - 参与计算的随机值
     InSecretKey: TCnBigNumber            - 参与计算的 SecretKey
     OutHash: TCnBigNumber                - 生成的变色龙杂凑值
     Prime: TCnBigNumber                  - 变色龙杂凑的素数
     Root: TCnBigNumber                   - 变色龙杂凑的原根

   返回值：Boolean                        - 返回生成是否成功
}

function CnChameleonHashFindRandom(InOldData: TCnBigNumber; InNewData: TCnBigNumber;
  InOldRandom: TCnBigNumber; InSecretKey: TCnBigNumber; OutNewRandom: TCnBigNumber;
  Prime: TCnBigNumber; Root: TCnBigNumber): Boolean;
{* 基于普通离散对数的变色龙杂凑函数，根据 SecretKey 与新旧消息，生成能够生成相同杂凑的新随机值
   其中，Prime 和 Root 须与原始消息杂凑生成时相同。
   可以利用 SecretKey 和 NewRandom 对 InNewData 调用 CnChameleonHashCalcDigest 生成相同的杂凑值

   参数：
     InOldData: TCnBigNumber              - 计算过杂凑的旧数据
     InNewData: TCnBigNumber              - 待计算杂凑的新数据
     InOldRandom: TCnBigNumber            - 计算旧数据时的随机值
     InSecretKey: TCnBigNumber            - 计算旧数据时的 SecretKey
     OutNewRandom: TCnBigNumber           - 计算出的新随机值
     Prime: TCnBigNumber                  - 变色龙杂凑的素数
     Root: TCnBigNumber                   - 变色龙杂凑的原根

   返回值：Boolean                        - 返回生成是否成功
}

// ================================= 其他辅助函数 ==============================

function GetDigestSignTypeFromBerOID(OID: Pointer; OidByteLen: Integer): TCnRSASignDigestType;
{* 从 BER 解析出的 OID 获取其对应的 RSA 签名杂凑摘要类型枚举值。

   参数：
     OID: Pointer                         - OID 所在的数据块地址
     OidLen: Integer                      - OID 所在的数据块的字节长度

   返回值：TCnRSASignDigestType           - 返回该 OID 对应的 RSA 签名杂凑摘要类型枚举值
}

function AddDigestTypeOIDNodeToWriter(AWriter: TCnBerWriter; ASignType: TCnRSASignDigestType;
  AParent: TCnBerWriteNode): TCnBerWriteNode;
{* 将一个 RSA 签名杂凑摘要类型枚举值的 OID 添加到一个 Ber 节点的子节点中。

   参数：
     AWriter: TCnBerWriter                - 写入时使用的 BerWriter 实例
     ASignType: TCnRSASignDigestType      - 待写入的 RSA 签名杂凑摘要类型枚举值
     AParent: TCnBerWriteNode             - 待添加的 Ber 父节点

   返回值：TCnBerWriteNode                - 返回添加的 Ber 子节点
}

function GetRSADigestNameFromSignDigestType(Digest: TCnRSASignDigestType): string;
{* 从签名杂凑算法枚举值获取其名称。

   参数：
     Digest: TCnRSASignDigestType         - 待获取的 RSA 签名杂凑摘要类型枚举值

   返回值：string                         - 返回 RSA 签名杂凑摘要类型名称
}

function GetLastCnRSAError: Integer;
{* 获取本线程内最近一次 ErrorCode，当以上函数返回 False 时可调用此函数获取错误详情。

   参数：
     （无）

   返回值：Integer                        - 返回 RSA 相关错误码
}

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

// 获取本线程内最近一次 ErrorCode，当以上函数返回 False 时可调用此函数获取错误详情}
function GetLastCnRSAError: Integer;
begin
  Result := CnGetLastError;
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
  Succ := False;
  repeat
    PrimeKey1 := CnGenerateUInt32Prime(HighBitSet);

    N := Trunc(Random * 100); // 以调整 CnGenerateUInt32Prime 内部的随机数发生器
    Sleep(N);

    PrimeKey2 := CnGenerateUInt32Prime(HighBitSet);

    if PrimeKey1 = PrimeKey2 then // 俩素数不能相等
      Continue;

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
    _CnSetLastError(ECN_RSA_INVALID_BITS);
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

    // 俩素数不能相等
    if BigNumberEqual(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
      Continue;

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
      PrivateKey.UpdateCRT;
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
  _CnSetLastError(ECN_RSA_BIGNUMBER_ERROR);
  if ModulusBits < 128 then
  begin
    _CnSetLastError(ECN_RSA_INVALID_BITS);
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

      // 俩素数不能相等
      if BigNumberEqual(PrivateKey.PrimeKey1, PrivateKey.PrimeKey2) then
        Continue;

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

      PrivateKey.UpdateCRT;
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

  Result := True;
  _CnSetLastError(ECN_RSA_OK);
end;

function CnRSAVerifyKeys(PrivateKey: TCnRSAPrivateKey; PublicKey: TCnRSAPublicKey): Boolean;
var
  T, M, P: TCnBigNumber;
begin
  // 私钥的俩素数乘积要等于私钥的 Product
  // 公私钥的 Product 得相等
  // 公钥指数是 3 或 65537
  // 验证 d
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) then
    Exit;

  if not BigNumberEqual(PrivateKey.PrivKeyProduct, PublicKey.PubKeyProduct) then
    Exit;

  // e 只允许 3 或 65537
  if not PublicKey.PubKeyExponent.IsWord(65537) and not PublicKey.PubKeyExponent.IsWord(3) then
    Exit;

  T := nil;
  P := nil;
  M := nil;

  try
    T := TCnBigNumber.Create;
    BigNumberMul(T, PrivateKey.PrimeKey1, PrivateKey.PrimeKey2);
    if not BigNumberEqual(T, PublicKey.PubKeyProduct) then
      Exit;

    // 验证 d 是否是 e * d mod (p-1)(q-1) = 1
    P := TCnBigNumber.Create;
    BigNumberCopy(P, PrivateKey.PrimeKey1);
    BigNumberCopy(T, PrivateKey.PrimeKey2);
    BigNumberSubWord(P, 1);
    BigNumberSubWord(T, 1);

    BigNumberMul(T, P, T); // T 得到 (p-1)(q-1)

    M := TCnBigNumber.Create;
    BigNumberMul(M, PrivateKey.FPrivKeyExponent, PublicKey.PubKeyExponent); // M 得到 e * d

    BigNumberMod(P, M, T);
    if not P.IsOne then
      Exit;

    Result := True;
    _CnSetLastError(ECN_RSA_OK);
  finally
    M.Free;
    P.Free;
    T.Free;
  end;
end;

// 从 PEM 格式文件中加载公私钥数据
(*
PKCS#1:
  RSAPrivateKey ::= SEQUENCE {                       0
    version Version,                                 1 0
    modulus INTEGER, – n                             2 公私钥
    publicExponent INTEGER, – e                      3 公钥
    privateExponent INTEGER, – d                     4 私钥
    prime1 INTEGER, – p                              5 私钥
    prime2 INTEGER, – q                              6 私钥
    exponent1 INTEGER, – d mod (p-1)                 7 CRT 系数 1
    exponent2 INTEGER, – d mod (q-1)                 8 CRT 系数 2
    coefficient INTEGER, – (1/q) mod p               9 CRT 系数 3：q 针对 p 的模逆元
    otherPrimeInfos OtherPrimeInfos OPTIONAL         10

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
  const Password: string = ''): Boolean;
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
      _CnSetLastError(ECN_RSA_PEM_FORMAT_ERROR);
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
          PutIndexedBigIntegerToBigNumber(Reader.Items[8], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigNumber(Reader.Items[9], PublicKey.PubKeyExponent);
        end;

        // 8 10 11 12 整成私钥
        if PrivateKey <> nil then
        begin
          PutIndexedBigIntegerToBigNumber(Reader.Items[8], PrivateKey.PrivKeyProduct);
          PutIndexedBigIntegerToBigNumber(Reader.Items[10], PrivateKey.PrivKeyExponent);
          PutIndexedBigIntegerToBigNumber(Reader.Items[11], PrivateKey.PrimeKey1);
          PutIndexedBigIntegerToBigNumber(Reader.Items[12], PrivateKey.PrimeKey2);

          PrivateKey.UpdateCRT;
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
            PutIndexedBigIntegerToBigNumber(Reader.Items[2], PublicKey.PubKeyProduct);
            PutIndexedBigIntegerToBigNumber(Reader.Items[3], PublicKey.PubKeyExponent);
          end;

          // 2 4 5 6 整成私钥
          if PrivateKey <> nil then
          begin
            PutIndexedBigIntegerToBigNumber(Reader.Items[2], PrivateKey.PrivKeyProduct);
            PutIndexedBigIntegerToBigNumber(Reader.Items[4], PrivateKey.PrivKeyExponent);
            PutIndexedBigIntegerToBigNumber(Reader.Items[5], PrivateKey.PrimeKey1);
            PutIndexedBigIntegerToBigNumber(Reader.Items[6], PrivateKey.PrimeKey2);

            PrivateKey.UpdateCRT;
          end;

          Result := True;
          _CnSetLastError(ECN_RSA_OK);
        end;
      end;
    end;

    if Result then
      _CnSetLastError(ECN_RSA_OK)
    else
      _CnSetLastError(ECN_RSA_PEM_FORMAT_ERROR);
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
  const Password: string = ''): Boolean;
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
          PutIndexedBigIntegerToBigNumber(Reader.Items[6], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigNumber(Reader.Items[7], PublicKey.PubKeyExponent);
        end;

        Result := True;
      end;
    end;

    if Result then
    begin
      _CnSetLastError(ECN_RSA_OK);
      Exit;
    end;

    PemStream.Position := OldPos;
    if LoadPemStreamToMemory(PemStream, PEM_RSA_PUBLIC_HEAD, PEM_RSA_PUBLIC_TAIL,
      Mem, Password, KeyHashMethod) then
    begin
      // 读 PKCS#1 格式的公钥
      Reader := TCnBerReader.Create(PByte(Mem.Memory), Mem.Size);
      Reader.ParseToTree;
      if Reader.TotalCount in [3, 4] then // 大于等于 5 的话不像 PKCS1 格式
      begin
        // 1 和 2 整成公钥
        if PublicKey <> nil then
        begin
          PutIndexedBigIntegerToBigNumber(Reader.Items[1], PublicKey.PubKeyProduct);
          PutIndexedBigIntegerToBigNumber(Reader.Items[2], PublicKey.PubKeyExponent);
        end;
      
        Result := True;
      end;
    end;

    if Result then
      _CnSetLastError(ECN_RSA_OK)
    else
      _CnSetLastError(ECN_RSA_PEM_FORMAT_ERROR);
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
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmCreate);
  try
    Result := CnRSASaveKeysToPem(Stream, PrivateKey, PublicKey, KeyType,
      KeyEncryptMethod, KeyHashMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnRSASaveKeysToPem(PemStream: TStream; PrivateKey: TCnRSAPrivateKey;
  PublicKey: TCnRSAPublicKey; KeyType: TCnRSAKeyType;
  KeyEncryptMethod: TCnKeyEncryptMethod;
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
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Exit;
  end;

  if (PrivateKey = nil) or (PrivateKey.PrivKeyProduct.GetBytesCount <= 0) or
    (PrivateKey.PrivKeyExponent.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Exit;
  end;

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
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @CN_OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(CN_OID_RSAENCRYPTION_PKCS1), Node);
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
      Result := SaveMemoryToPemStream(PemStream, PEM_RSA_PRIVATE_HEAD,
        PEM_RSA_PRIVATE_TAIL, Mem, KeyEncryptMethod, KeyHashMethod, Password)
    else if KeyType = cktPKCS8 then
      Result := SaveMemoryToPemStream(PemStream, PEM_PRIVATE_HEAD,
        PEM_PRIVATE_TAIL, Mem, KeyEncryptMethod, KeyHashMethod, Password);

    if Result then
      _CnSetLastError(ECN_RSA_OK)
    else
      _CnSetLastError(ECN_RSA_PEM_FORMAT_ERROR);
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
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmCreate);
  try
    Result := CnRSASavePublicKeyToPem(Stream, PublicKey, KeyType,
      KeyEncryptMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnRSASavePublicKeyToPem(PemStream: TStream;
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
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Exit;
  end;

  Writer := nil;
  Mem := nil;

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
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @CN_OID_RSAENCRYPTION_PKCS1[0],
        SizeOf(CN_OID_RSAENCRYPTION_PKCS1), Node);
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
      Result := SaveMemoryToPemStream(PemStream, PEM_RSA_PUBLIC_HEAD,
        PEM_RSA_PUBLIC_TAIL, Mem, KeyEncryptMethod, ckhMd5, Password)
    else if KeyType = cktPKCS8 then
      Result := SaveMemoryToPemStream(PemStream, PEM_PUBLIC_HEAD,
        PEM_PUBLIC_TAIL, Mem, KeyEncryptMethod, ckhMd5, Password);

    if Result then
      _CnSetLastError(ECN_RSA_OK)
    else
      _CnSetLastError(ECN_RSA_PEM_FORMAT_ERROR);
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
    _CnSetLastError(ECN_RSA_BIGNUMBER_ERROR);
end;

// 利用私钥对数据进行加密，返回加密是否成功
function CnRSAEncrypt(Data: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Res: TCnBigNumber): Boolean;
begin
  Result := CnRSADecrypt(Res, PrivateKey, Data); // 本质上是同一个私钥运算，可复用
end;

// 利用公钥对数据进行加密，返回加密是否成功
function CnRSAEncrypt(Data: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Res: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Data, PublicKey.PubKeyProduct, PublicKey.PubKeyExponent, Res);
end;

// 利用私钥对数据进行解密，返回解密是否成功
function CnRSADecrypt(Res: TCnBigNumber; PrivateKey: TCnRSAPrivateKey;
  Data: TCnBigNumber): Boolean;
var
  M1, M2: TCnBigNumber;
begin
  if PrivateKey.UseCRT then
  begin
    M1 := nil;
    M2 := nil;

    // m1 = c^dP mod p
    // m2 = c^dQ mod q
    // h = qInv.(m1 - m2) mod p
    // m = m2 + h.q

    try
      M1 := TCnBigNumber.Create;
      BigNumberMontgomeryPowerMod(M1, Data, PrivateKey.FDP1, PrivateKey.FPrimeKey1);
      // m1 = c^dP mod p

      M2 := TCnBigNumber.Create;
      BigNumberMontgomeryPowerMod(M2, Data, PrivateKey.FDQ1, PrivateKey.FPrimeKey2);
      // m2 = c^dQ mod q

      // 以下复用 m1
      BigNumberSubMod(M1, M1, M2, PrivateKey.FPrimeKey1);
      // m1 := m1 - m2 mod p

      BigNumberDirectMulMod(M1, PrivateKey.FQInv, M1, PrivateKey.FPrimeKey1);
      // m1 := qInv * m1 mod p

      BigNumberMul(M1, M1, PrivateKey.FPrimeKey2);
      // m1 := m1 * q

      BigNumberAdd(Res, M2, M1);
      // m = m2 + m1

      Result := True;
    finally
      M2.Free;
      M1.Free;
    end;
  end
  else
    Result := RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res);
end;

// 利用公钥对数据进行解密，返回解密是否成功
function CnRSADecrypt(Res: TCnBigNumber; PublicKey: TCnRSAPublicKey;
  Data: TCnBigNumber): Boolean;
begin
  Result := CnRSAEncrypt(Data, PublicKey, Res); // 本质上是同一个公钥运算，可复用
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

    // 都用 CRT 加速时复制这仨才有意义
    if FUseCRT and (Source as TCnRSAPrivateKey).UseCRT then
    begin
      BigNumberCopy(FDP1, (Source as TCnRSAPrivateKey).FDP1);
      BigNumberCopy(FDQ1, (Source as TCnRSAPrivateKey).FDQ1);
      BigNumberCopy(FQInv, (Source as TCnRSAPrivateKey).FQInv);
    end;
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

procedure TCnRSAPrivateKey.UpdateCRT;
var
  T: TCnBigNumber;
begin
  if not FUseCRT then
    Exit;

  T := TCnBigNumber.Create;
  try
    if BigNumberCompare(FPrimeKey1, FPrimeKey2) < 0 then // 确保 p > q
      BigNumberSwap(FPrimeKey1, FPrimeKey2);

    // 计算 DP1 = D mod (PrimeKey1 - 1);
    BigNumberCopy(T, FPrimeKey1);
    T.SubWord(1);
    BigNumberMod(FDP1, FPrivKeyExponent, T);

    // 计算 DQ1 = D mod (PrimeKey2 - 1);
    BigNumberCopy(T, FPrimeKey2);
    T.SubWord(1);
    BigNumberMod(FDQ1, FPrivKeyExponent, T);

    // 计算 QInv = Prime2 对 Prime1 的模逆元
    BigNumberModularInverse(FQInv, FPrimeKey2, FPrimeKey1);
  finally
    T.Free;
  end;
end;

constructor TCnRSAPrivateKey.Create(CRT: Boolean);
begin
  inherited Create;
  FUseCRT := CRT;

  FPrimeKey1 := TCnBigNumber.Create;
  FPrimeKey2 := TCnBigNumber.Create;
  FPrivKeyProduct := TCnBigNumber.Create;
  FPrivKeyExponent := TCnBigNumber.Create;

  if FUseCRT then
  begin
    FDP1 := TCnBigNumber.Create;
    FDQ1 := TCnBigNumber.Create;
    FQInv := TCnBigNumber.Create;
  end;
end;

destructor TCnRSAPrivateKey.Destroy;
begin
  if FUseCRT then
  begin
    FQInv.Free;
    FDQ1.Free;
    FDP1.Free;
  end;

  FPrivKeyExponent.Free;
  FPrivKeyProduct.Free;
  FPrimeKey2.Free;
  FPrimeKey1.Free;
  inherited;
end;

function TCnRSAPrivateKey.GetBitsCount: Integer;
begin
  Result := FPrivKeyProduct.GetBitsCount;
end;

function TCnRSAPrivateKey.GetBytesCount: Integer;
begin
  Result := FPrivKeyProduct.GetBytesCount;
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

function RSACryptRawData(Data: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; Exponent, Product: TCnBigNumber): Boolean;
var
  D, R: TCnBigNumber;
begin
  Result := False;
  if (Data <> nil) and (DataByteLen > 0) then
  begin
    R := TCnBigNumber.Create;
    D := TCnBigNumber.FromBinary(PAnsiChar(Data), DataByteLen);

    if RSACrypt(D, Product, Exponent, R) then
    begin
      R.ToBinary(OutBuf); // TODO: Fixed Len?
      OutByteLen := R.GetBytesCount;

      Result := True;
      _CnSetLastError(ECN_RSA_OK);
    end;
  end
  else
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
end;

function CnRSAEncryptRawData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSACryptRawData(PlainData, DataByteLen, OutBuf, OutByteLen,
    PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSAEncryptRawData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSACryptRawData(PlainData, DataByteLen, OutBuf, OutByteLen,
    PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSADecryptRawData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSACryptRawData(EnData, DataByteLen, OutBuf, OutByteLen,
    PublicKey.PubKeyExponent, PublicKey.PubKeyProduct);
end;

function CnRSADecryptRawData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSACryptRawData(EnData, DataByteLen, OutBuf, OutByteLen,
    PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct);
end;

function CnRSAEncryptRawBytes(PlainData: TBytes; PublicKey: TCnRSAPublicKey): TBytes;
var
  OutLen: Integer;
begin
  if (Length(PlainData) = 0) or (PublicKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PublicKey.GetBytesCount);
    if CnRSAEncryptRawData(@PlainData[0], Length(PlainData), @Result[0], OutLen, PublicKey) then
      SetLength(Result, OutLen)
    else
      SetLength(Result, 0);
  end;
end;

function CnRSAEncryptRawBytes(PlainData: TBytes; PrivateKey: TCnRSAPrivateKey): TBytes;
var
  OutLen: Integer;
begin
  if (Length(PlainData) = 0) or (PrivateKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PrivateKey.GetBytesCount);
    if CnRSAEncryptRawData(@PlainData[0], Length(PlainData), @Result[0], OutLen, PrivateKey) then
      SetLength(Result, OutLen)
    else
      SetLength(Result, 0);
  end;
end;

function CnRSADecryptRawBytes(EnData: TBytes; PublicKey: TCnRSAPublicKey): TBytes;
var
  OutLen: Integer;
begin
  if (Length(EnData) = 0) or (PublicKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PublicKey.GetBytesCount);
    if CnRSAEncryptRawData(@EnData[0], Length(EnData), @Result[0], OutLen, PublicKey) then
      SetLength(Result, OutLen)
    else
      SetLength(Result, 0);
  end;
end;

function CnRSADecryptRawBytes(EnData: TBytes; PrivateKey: TCnRSAPrivateKey): TBytes;
var
  OutLen: Integer;
begin
  if (Length(EnData) = 0) or (PrivateKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PrivateKey.GetBytesCount);
    if CnRSAEncryptRawData(@EnData[0], Length(EnData), @Result[0], OutLen, PrivateKey) then
      SetLength(Result, OutLen)
    else
      SetLength(Result, 0);
  end;
end;

// 将一片内存区域按指定的 Padding 模式与类型填充后进行 RSA 加解密计算
function RSAPaddingCrypt(PaddingType, BlockSize: Integer; PlainData: Pointer;
  DataByteLen: Integer; OutBuf: Pointer; Exponent, Product: TCnBigNumber;
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
      if not AddPKCS1Padding(PaddingType, BlockSize, PlainData, DataByteLen, Stream) then
      begin
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;
    end
    else if PaddingMode = cpmOAEP then
    begin
      // OAEP 公钥加密，仅公钥的控制在调用者
      Stream.Size := Product.GetBytesCount;
      if not AddOaepSha1MgfPadding(Stream.Memory, Stream.Size, PlainData, DataByteLen) then
      begin
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;
    end;

    Res := TCnBigNumber.Create;
    Data := TCnBigNumber.FromBinary(PAnsiChar(Stream.Memory), Stream.Size);
    if not RSACrypt(Data, Product, Exponent, Res) then
      Exit;

    Res.ToBinary(PAnsiChar(OutBuf), Stream.Size);

    Result := True;
    _CnSetLastError(ECN_RSA_OK);
  finally
    Stream.Free;
    Data.Free;
    Res.Free;
  end;
end;

function CnRSAEncryptData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM, PublicKey.BitsCount div 8,
    PlainData, DataByteLen, OutBuf, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct, PaddingMode);
end;

function CnRSAEncryptData(PlainData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  PrivateKey: TCnRSAPrivateKey): Boolean;
begin
  Result := RSAPaddingCrypt(CN_PKCS1_BLOCK_TYPE_PRIVATE_FF, PrivateKey.BitsCount div 8,
    PlainData, DataByteLen, OutBuf, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct, cpmPKCS1);
  // 私钥加密只支持 PKCS1 对齐方式，不支持 OAEP 对齐方式
end;

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey; PaddingMode: TCnRSAPaddingMode): Boolean;
var
  Stream: TMemoryStream;
  Res: TBytes;
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
    _CnSetLastError(ECN_RSA_OK);
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function CnRSAEncryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey): Boolean;
var
  Stream: TMemoryStream;
  Res: TBytes;
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
    _CnSetLastError(ECN_RSA_OK);
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

// 将一片内存区域进行 RSA 加解密计算后按其展现的 Padding 方式解出原始数据
function RSADecryptPadding(BlockSize: Integer; EnData: Pointer; DataByteLen: Integer;
  OutBuf: Pointer; out OutLen: Integer; Exponent, Product: TCnBigNumber;
  PaddingMode: TCnRSAPaddingMode): Boolean;
var
  Stream: TMemoryStream;
  Res, Data: TCnBigNumber;
  ResBuf: TBytes;
begin
  Result := False;
  Res := nil;
  Data := nil;
  Stream := nil;

  try
    Res := TCnBigNumber.Create;
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnData), DataByteLen);
    if not RSACrypt(Data, Product, Exponent, Res) then
      Exit;

    SetLength(ResBuf, BlockSize);
    Res.ToBinary(PAnsiChar(@ResBuf[0]), BlockSize);
    // 解出来的 Res 可能前面有 0 导致 GetBytesCount 不够 BlockSize，需要右对齐

    if PaddingMode = cpmPKCS1 then
    begin
      Result := RemovePKCS1Padding(@ResBuf[0], Length(ResBuf), OutBuf, OutLen);
      if not Result then
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
    end
    else if PaddingMode = cpmOAEP then
    begin
      // OAEP 解密，仅私钥的控制在调用者
      Result := RemoveOaepSha1MgfPadding(OutBuf, OutLen, @ResBuf[0], Length(ResBuf));

      if Result then
        _CnSetLastError(ECN_RSA_OK)
      else
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
    end;
  finally
    Stream.Free;
    Res.Free;
    Data.Free;
  end;
end;

function CnRSADecryptData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PublicKey: TCnRSAPublicKey): Boolean;
begin
  Result := RSADecryptPadding(PublicKey.GetBytesCount, EnData, DataByteLen,
    OutBuf, OutByteLen, PublicKey.PubKeyExponent, PublicKey.PubKeyProduct, cpmPKCS1);
  // 公钥解密只支持 PKCS1，不支持 OAEP
end;

function CnRSADecryptData(EnData: Pointer; DataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer; PrivateKey: TCnRSAPrivateKey; PaddingMode: TCnRSAPaddingMode): Boolean;
begin
  Result := RSADecryptPadding(PrivateKey.GetBytesCount, EnData, DataByteLen,
    OutBuf, OutByteLen, PrivateKey.PrivKeyExponent, PrivateKey.PrivKeyProduct, PaddingMode);
end;

function CnRSAEncryptBytes(PlainData: TBytes; PublicKey: TCnRSAPublicKey;
  PaddingMode: TCnRSAPaddingMode): TBytes;
begin
  if (Length(PlainData) = 0) or (PublicKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PublicKey.GetBytesCount);
    if not CnRSAEncryptData(@PlainData[0], Length(PlainData), @Result[0], PublicKey, PaddingMode) then
      SetLength(Result, 0);
  end;
end;

function CnRSAEncryptBytes(PlainData: TBytes; PrivateKey: TCnRSAPrivateKey): TBytes;
begin
  if (Length(PlainData) = 0) or (PrivateKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PrivateKey.GetBytesCount);
    if not CnRSAEncryptData(@PlainData[0], Length(PlainData), @Result[0], PrivateKey) then
      SetLength(Result, 0);
  end;
end;

function CnRSADecryptBytes(EnData: TBytes; PublicKey: TCnRSAPublicKey): TBytes;
var
  OutLen: Integer;
begin
  if (Length(EnData) = 0) or (PublicKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PublicKey.GetBytesCount);
    if CnRSADecryptData(@EnData[0], Length(EnData), @Result[0], OutLen, PublicKey) then
      SetLength(Result, OutLen)
    else
      SetLength(Result, 0);
  end;
end;

function CnRSADecryptBytes(EnData: TBytes; PrivateKey: TCnRSAPrivateKey;
  PaddingMode: TCnRSAPaddingMode): TBytes;
var
  OutLen: Integer;
begin
  if (Length(EnData) = 0) or (PrivateKey.GetBytesCount <= 0) then
  begin
    _CnSetLastError(ECN_RSA_INVALID_INPUT);
    Result := nil;
  end
  else
  begin
    SetLength(Result, PrivateKey.GetBytesCount);
    if CnRSADecryptData(@EnData[0], Length(EnData), @Result[0], OutLen, PrivateKey) then
      SetLength(Result, OutLen)
    else
      SetLength(Result, 0);
  end;
end;

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PublicKey: TCnRSAPublicKey): Boolean;
var
  Stream: TMemoryStream;
  Res: TBytes;
  OutLen: Integer;
begin
  Result := False;
  Stream := nil;
  try
    SetLength(Res, PublicKey.GetBytesCount);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFileName);

    if Stream.Size <> PublicKey.GetBytesCount then
    begin
      _CnSetLastError(ECN_RSA_INVALID_INPUT);
      Exit;
    end;

    if not CnRSADecryptData(Stream.Memory, Stream.Size, @Res[0], OutLen, PublicKey) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], OutLen);
    Stream.SaveToFile(OutFileName);

    Result := True;
    _CnSetLastError(ECN_RSA_OK);
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

function CnRSADecryptFile(const InFileName, OutFileName: string;
  PrivateKey: TCnRSAPrivateKey; PaddingMode: TCnRSAPaddingMode): Boolean;
var
  Stream: TMemoryStream;
  Res: TBytes;
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
      _CnSetLastError(ECN_RSA_INVALID_INPUT);
      Exit;
    end;

    if not CnRSADecryptData(Stream.Memory, Stream.Size, @Res[0], OutLen, PrivateKey, PaddingMode) then
      Exit;

    Stream.Clear;
    Stream.Write(Res[0], OutLen);
    Stream.SaveToFile(OutFileName);

    Result := True;
    _CnSetLastError(ECN_RSA_OK);
  finally
    Stream.Free;
    SetLength(Res, 0);
  end;
end;

// RSA 文件签名与验证实现

// 根据指定数字摘要算法计算指定流的二进制杂凑值并写入 Stream，如果出错内部会设置错误码
function CalcDigestStream(InStream: TStream; SignType: TCnRSASignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TCnMD5Digest;
  Sha1: TCnSHA1Digest;
  Sha256: TCnSHA256Digest;
  Sm3Dig: TCnSM3Digest;
begin
  Result := False;
  case SignType of
    rsdtMD5:
      begin
        Md5 := MD5Stream(InStream);
        outStream.Write(Md5, SizeOf(TCnMD5Digest));
        Result := True;
      end;
    rsdtSHA1:
      begin
        Sha1 := SHA1Stream(InStream);
        outStream.Write(Sha1, SizeOf(TCnSHA1Digest));
        Result := True;
      end;
    rsdtSHA256:
      begin
        Sha256 := SHA256Stream(InStream);
        outStream.Write(Sha256, SizeOf(TCnSHA256Digest));
        Result := True;
      end;
    rsdtSM3:
      begin
        Sm3Dig := SM3Stream(InStream);
        outStream.Write(Sm3Dig, SizeOf(TCnSM3Digest));
        Result := True;
      end
  end;

  if Result then
    _CnSetLastError(ECN_RSA_OK)
  else
    _CnSetLastError(ECN_RSA_DIGEST_ERROR);
end;

// 根据指定数字摘要算法计算文件的二进制杂凑值并写入 Stream
function CalcDigestFile(const FileName: string; SignType: TCnRSASignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TCnMD5Digest;
  Sha1: TCnSHA1Digest;
  Sha256: TCnSHA256Digest;
  Sm3Dig: TCnSM3Digest;
begin
  Result := False;
  case SignType of
    rsdtMD5:
      begin
        Md5 := MD5File(FileName);
        outStream.Write(Md5, SizeOf(TCnMD5Digest));
        Result := True;
      end;
    rsdtSHA1:
      begin
        Sha1 := SHA1File(FileName);
        outStream.Write(Sha1, SizeOf(TCnSHA1Digest));
        Result := True;
      end;
    rsdtSHA256:
      begin
        Sha256 := SHA256File(FileName);
        outStream.Write(Sha256, SizeOf(TCnSHA256Digest));
        Result := True;
      end;
    rsdtSM3:
      begin
        Sm3Dig := SM3File(FileName);
        outStream.Write(Sm3Dig, SizeOf(TCnSM3Digest));
        Result := True;
      end;
  end;

  if Result then
    _CnSetLastError(ECN_RSA_OK)
  else
    _CnSetLastError(ECN_RSA_DIGEST_ERROR);
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
  ResBuf: TBytes;
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
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;
    end
    else // 有数字摘要
    begin
      if not CalcDigestStream(InStream, SignType, Stream) then // 计算流的杂凑值
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
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;
    end;

    // 私钥加密运算
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnStream.Memory), EnStream.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res) then
    begin
      // 注意 Res 可能存在前导 0，所以此处必须以 PrivateKey.GetBytesCount 为准，才能确保不漏前导 0
      SetLength(ResBuf, PrivateKey.GetBytesCount);
      Res.ToBinary(@ResBuf[0], PrivateKey.GetBytesCount);

      // 保存用私钥加密后的内容至文件
      Stream.Clear;
      Stream.Write(ResBuf[0], PrivateKey.GetBytesCount);
      Stream.SaveToStream(OutSignStream);

      Result := True;
      _CnSetLastError(ECN_RSA_OK);
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
  ResBuf, BerBuf: TBytes;
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
      // 注意 Res 可能存在前导 0，所以此处必须以 PublicKey.GetBytesCount 为准，才能确保不漏前导 0
      SetLength(ResBuf, PublicKey.GetBytesCount);
      Res.ToBinary(@ResBuf[0], PublicKey.GetBytesCount);

      // 从 Res 中解出 PKCS1 对齐的内容放入 BerBuf 中
      SetLength(BerBuf, Length(ResBuf));
      if not RemovePKCS1Padding(@ResBuf[0], Length(ResBuf), @BerBuf[0], BerLen) then
      begin
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;

      if SignType = rsdtNone then
      begin
        // 无摘要时，从解密内容里去除了 PKCS1 的 Padding 的剩下内容直接与原始 InStream 内容比对
        Result := InStream.Size = BerLen;
        if Result then
          Result := CompareMem(InStream.Memory, @BerBuf[0], InStream.Size);

        _CnSetLastError(ECN_RSA_OK); // 正常进行校验，即使校验不通过也清空错误码
      end
      else
      begin
        if (BerLen <= 0) or (BerLen >= Length(ResBuf)) then
        begin
          _CnSetLastError(ECN_RSA_BER_ERROR);
          Exit;
        end;

        // 解开 Ber 内容里的编码与加密算法，不使用 SignType 原始值
        Reader := TCnBerReader.Create(@BerBuf[0], BerLen);
        Reader.ParseToTree;
        if Reader.TotalCount < 5 then
        begin
          _CnSetLastError(ECN_RSA_BER_ERROR);
          Exit;
        end;

        Node := Reader.Items[2];
        SignType := GetDigestSignTypeFromBerOID(Node.BerDataAddress, Node.BerDataLength);
        if SignType = rsdtNone then
        begin
          _CnSetLastError(ECN_RSA_BER_ERROR);
          Exit;
        end;

        if not CalcDigestStream(InStream, SignType, Stream) then // 计算流的杂凑值
          Exit;

        // 与 Ber 解出的杂凑值比较
        Node := Reader.Items[4];
        Result := Stream.Size = Node.BerDataLength;
        if Result then
          Result := CompareMem(Stream.Memory, Node.BerDataAddress, Stream.Size);

        _CnSetLastError(ECN_RSA_OK); // 正常进行校验，即使校验不通过也清空错误码
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

function CnRSASignBytes(InData: TBytes; PrivateKey: TCnRSAPrivateKey;
  SignType: TCnRSASignDigestType): TBytes;
var
  InStream, OutStream: TMemoryStream;
begin
  Result := nil;
  InStream := nil;
  OutStream := nil;

  try
    InStream := TMemoryStream.Create;
    BytesToStream(InData, InStream);

    OutStream := TMemoryStream.Create;
    if CnRSASignStream(InStream, OutStream, PrivateKey, SignType) then
      Result := StreamToBytes(OutStream);
  finally
    OutStream.Free;
    InStream.Free;
  end;
end;

function CnRSAVerifyBytes(InData: TBytes; InSignBytes: TBytes;
  PublicKey: TCnRSAPublicKey; SignType: TCnRSASignDigestType): Boolean;
var
  InStream, SignStream: TMemoryStream;
begin
  InStream := nil;
  SignStream := nil;

  try
    InStream := TMemoryStream.Create;
    BytesToStream(InData, InStream);

    SignStream := TMemoryStream.Create;
    BytesToStream(InSignBytes, SignStream);
    Result := CnRSAVerifyStream(InStream, SignStream, PublicKey, SignType);
  finally
    SignStream.Free;
    InStream.Free;
  end;
end;

function CnRSASignFile(const InFileName, OutSignFileName: string;
  PrivateKey: TCnRSAPrivateKey; SignType: TCnRSASignDigestType): Boolean;
var
  Stream, BerStream, EnStream: TMemoryStream;
  Data, Res: TCnBigNumber;
  ResBuf: TBytes;
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
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;
    end
    else // 有数字摘要
    begin
      if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的杂凑值
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
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;
    end;

    // 私钥加密运算
    Data := TCnBigNumber.FromBinary(PAnsiChar(EnStream.Memory), EnStream.Size);
    Res := TCnBigNumber.Create;

    if RSACrypt(Data, PrivateKey.PrivKeyProduct, PrivateKey.PrivKeyExponent, Res) then
    begin
      // 注意 Res 可能存在前导 0，所以此处必须以 PrivateKey.GetBytesCount 为准，才能确保不漏前导 0
      SetLength(ResBuf, PrivateKey.GetBytesCount);
      Res.ToBinary(@ResBuf[0], PrivateKey.GetBytesCount);

      // 保存用私钥加密后的内容至文件
      Stream.Clear;
      Stream.Write(ResBuf[0], PrivateKey.GetBytesCount);
      Stream.SaveToFile(OutSignFileName);

      Result := True;
      _CnSetLastError(ECN_RSA_OK);
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
  ResBuf, BerBuf: TBytes;
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
      SetLength(ResBuf, PublicKey.GetBytesCount);
      Res.ToBinary(@ResBuf[0], PublicKey.GetBytesCount);

      // 从 Res 中解出 PKCS1 对齐的内容放入 BerBuf 中
      SetLength(BerBuf, Length(ResBuf));
      if not RemovePKCS1Padding(@ResBuf[0], Length(ResBuf), @BerBuf[0], BerLen) then
      begin
        _CnSetLastError(ECN_RSA_PADDING_ERROR);
        Exit;
      end;

      if SignType = rsdtNone then
      begin
        Stream.LoadFromFile(InFileName); // 无摘要时，直接比对解密内容与原始文件
        Result := Stream.Size = BerLen;
        if Result then
          Result := CompareMem(Stream.Memory, @BerBuf[0], Stream.Size);

        _CnSetLastError(ECN_RSA_OK); // 正常进行校验，即使校验不通过也清空错误码
      end
      else
      begin
        if (BerLen <= 0) or (BerLen >= Length(ResBuf)) then
        begin
          _CnSetLastError(ECN_RSA_BER_ERROR);
          Exit;
        end;

        // 解开 Ber 内容里的编码与加密算法，不使用 SignType 原始值
        Reader := TCnBerReader.Create(@BerBuf[0], BerLen);
        Reader.ParseToTree;
        if Reader.TotalCount < 5 then
        begin
          _CnSetLastError(ECN_RSA_BER_ERROR);
          Exit;
        end;

        Node := Reader.Items[2];
        SignType := GetDigestSignTypeFromBerOID(Node.BerDataAddress, Node.BerDataLength);
        if SignType = rsdtNone then
        begin
          _CnSetLastError(ECN_RSA_BER_ERROR);
          Exit;
        end;

        if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的杂凑值
          Exit;

        // 与 Ber 解出的杂凑值比较
        Node := Reader.Items[4];
        Result := Stream.Size = Node.BerDataLength;
        if Result then
          Result := CompareMem(Stream.Memory, Node.BerDataAddress, Stream.Size);

        _CnSetLastError(ECN_RSA_OK); // 正常进行校验，即使校验不通过也清空错误码
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
  Q, R, T: TCnBigNumber;
begin
  Result := False;
  if BitsCount <= 16 then
  begin
    _CnSetLastError(ECN_RSA_INVALID_BITS);
    Exit;
  end;

  Q := nil;
  T := nil;
  R := nil;

  try
    Q := TCnBigNumber.Create;
    repeat
      if not BigNumberGeneratePrimeByBitsCount(Prime, BitsCount) then
      begin
        _CnSetLastError(ECN_RSA_BIGNUMBER_ERROR);
        Exit;
      end;

      if BigNumberCopy(Q, Prime) = nil then
        Exit;

      Q.SubWord(1);
      Q.ShiftRightOne;

      if BigNumberIsProbablyPrime(Q) then // P = 2Q + 1，P 和 Q 都是素数，再用以下条件判断
        Break;
    until False;

    T := TCnBigNumber.Create;
    R := TCnBigNumber.Create;

    for I := 2 to MaxInt do
    begin
      T.SetWord(I);
      if not BigNumberDirectMulMod(R, T, T, Prime) then // G^2 mod P <> 1
        Exit;

      if not R.IsOne then
      begin
        if not BigNumberPowerMod(R, T, Q, Prime) then   // G^Q mod P <> 1
          Exit;

        if not R.IsOne then
        begin
          Result := BigNumberCopy(MinRoot, T) <> nil;
          Exit;
        end;
      end;
    end;
  finally
    R.Free;
    T.Free;
    Q.Free;
  end;
end;

// 根据自身选择的随机数 PrivateKey 生成 Diffie-Hellman 密钥协商的输出公钥
function CnDiffieHellmanGenerateOutKey(Prime, Root, SelfPrivateKey: TCnBigNumber;
  OutPublicKey: TCnBigNumber): Boolean;
begin
  // OutPublicKey = (Root ^ SelfPrivateKey) mod Prime
  Result := BigNumberMontgomeryPowerMod(OutPublicKey, Root, SelfPrivateKey, Prime);
end;

// 根据对方发送的 Diffie-Hellman 密钥协商的输出公钥计算生成公认的密钥
function CnDiffieHellmanComputeKey(Prime, SelfPrivateKey, OtherPublicKey: TCnBigNumber;
  SecretKey: TCnBigNumber): Boolean;
begin
  // SecretKey = (OtherPublicKey ^ SelfPrivateKey) mod Prime
  Result := BigNumberMontgomeryPowerMod(SecretKey, OtherPublicKey, SelfPrivateKey, Prime);
end;

// 生成基于离散对数的变色龙杂凑函数所需的素数与其最小原根，实际等同于 Diffie-Hellman
function CnChameleonHashGeneratePrimeRootByBitsCount(BitsCount: Integer;
  Prime, MinRoot: TCnBigNumber): Boolean;
begin
  Result := CnDiffieHellmanGeneratePrimeRootByBitsCount(BitsCount, Prime, MinRoot);
end;

// 基于普通离散对数的变色龙杂凑函数，根据一随机值与一 SecretKey，生成指定消息的杂凑
function CnChameleonHashCalcDigest(InData: TCnBigNumber; InRandom: TCnBigNumber;
  InSecretKey: TCnBigNumber; OutHash: TCnBigNumber; Prime, Root: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  T := nil;

  // Hash(M, R) = g^(M + R * Sk) mod P
  try
    T := TCnBigNumber.Create;
    BigNumberCopy(T, InSecretKey);
    BigNumberDirectMulMod(T, InRandom, T, Prime);

    BigNumberAddMod(T, InData, T, Prime);
    Result := BigNumberMontgomeryPowerMod(OutHash, Root, T, Prime);
  finally
    T.Free;
  end;
end;

// 基于普通离散对数的变色龙杂凑函数，根据 SecretKey 与新旧消息，生成能够生成相同杂凑的新随机值
function CnChameleonHashFindRandom(InOldData, InNewData: TCnBigNumber;
  InOldRandom, InSecretKey: TCnBigNumber; OutNewRandom: TCnBigNumber;
  Prime, Root: TCnBigNumber): Boolean;
var
  M, SK: TCnBigNumber;
begin
  M := nil;
  SK := nil;

  // R2 := ((M1 - M2)/SK + R1) mod P
  try
    M := TCnBigNumber.Create;
    BigNumberSubMod(M, InOldData, InNewData, Prime);

    SK := TCnBigNumber.Create;
    BigNumberModularInverse(SK, InSecretKey, Prime);

    BigNumberDirectMulMod(M, M, SK, Prime);
    Result := BigNumberAddMod(OutNewRandom, M, InOldRandom, Prime);
  finally
    SK.Free;
    M.Free;
  end;
end;

function GetDigestSignTypeFromBerOID(OID: Pointer; OidByteLen: Integer): TCnRSASignDigestType;
begin
  Result := rsdtNone;
  if (OidByteLen = SizeOf(OID_SIGN_MD5)) and CompareMem(OID, @OID_SIGN_MD5[0], OidByteLen) then
    Result := rsdtMD5
  else if (OidByteLen = SizeOf(OID_SIGN_SHA1)) and CompareMem(OID, @OID_SIGN_SHA1[0], OidByteLen) then
    Result := rsdtSHA1
  else if (OidByteLen = SizeOf(OID_SIGN_SHA256)) and CompareMem(OID, @OID_SIGN_SHA256[0], OidByteLen) then
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
  Ctx: TCnSHA1Context;
  Dig: TCnSHA1Digest;
begin
  Result := False;
  OutLen := 0;
  MdLen := SizeOf(TCnSHA1Digest);
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
      SHA1Final(Ctx, PCnSHA1Digest(TCnNativeInt(OutMask) + OutLen)^);
      OutLen := OutLen + MdLen;
    end
    else
    begin
      SHA1Final(Ctx, Dig);
      Move(Dig[0], PCnSHA1Digest(TCnNativeInt(OutMask) + OutLen)^, MaskLen - OutLen);
      OutLen := MaskLen;
    end;

    Inc(I);
  end;
  Result := True;
end;

function AddOaepSha1MgfPadding(ToBuf: PByte; ToByteLen: Integer; PlainData: PByte;
  DataByteLen: Integer; DigestParam: PByte; ParamByteLen: Integer): Boolean;
var
  EmLen, MdLen, I: Integer;
  SeedMask: TCnSHA1Digest;
  DB, Seed: PByteArray;
  DBMask: TBytes;
begin
  Result := False;
  EmLen:= ToByteLen - 1;

  MdLen := SizeOf(TCnSHA1Digest);

  if (DataByteLen > EmLen - 2 * MdLen - 1) or (EmLen < 2 * MdLen + 1) then
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  ToBuf^ := 0;
  Seed := PByteArray(TCnNativeInt(ToBuf) + 1);
  DB := PByteArray(TCnNativeInt(ToBuf) + MdLen + 1);

  // 00 | 20 位 Seed | DB
  // 其中 DB := ParamHash || PS || 0x01 || Data，长度是 EmLen - MdLen
  // 后面要 XOR 一次称为 MaskDB
  SeedMask := SHA1Buffer(DigestParam, ParamByteLen);
  Move(SeedMask[0], DB^, MdLen);

  // To 区 DB 的前 20 字节先留着，后面到尾巴先填满 0
  FillChar(PByte(TCnNativeInt(DB) + MdLen)^, EmLen - DataByteLen - 2 * MdLen - 1, 0);
  DB^[EmLen - DataByteLen - MdLen - 1] := 1;

  // 明文搁后面
  Move(PlainData^, PByte(TCnNativeInt(DB) + EmLen - DataByteLen - MdLen)^, DataByteLen);

  // To[1] 开始的 20 个字节 Rand 一下
  if not CnRandomFillBytes(PAnsiChar(Seed), MdLen) then
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  SetLength(DBMask, EmLen - MdLen);

  // 随机 Seed 算出 MGF 数据，准备和 DB 做 XOR
  if not Pkcs1Sha1MGF(Seed, MdLen, @DBMask[0], EmLen - MdLen) then
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  for I := 0 to EmLen - MdLen - 1 do
    DB^[I] := DB^[I] xor DBMask[I];        // 得到 Masked DB

  // XOR 过的 Masked DB 再算出 MGF 数据，准备和随机 Seed 做 XOR
  if not Pkcs1Sha1MGF(DB, EmLen - MdLen, @SeedMask[0], MdLen) then
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  for I := 0 to MdLen - 1 do
    Seed^[I] := Seed^[I] xor SeedMask[I];  // 得到 Masked Seed

  SetLength(DBMask, 0);
  Result := True;
  // 最后加密消息 = 00 || maskedSeed || maskedDB
end;

function RemoveOaepSha1MgfPadding(ToBuf: PByte; out OutByteLen: Integer; EnData: PByte;
  DataByteLen: Integer; DigestParam: PByte; ParamByteLen: Integer): Boolean;
var
  I, MdLen, DBLen, MStart: Integer;
  MaskedDB, MaskedSeed: PByteArray;
  Seed, ParamHash: TCnSHA1Digest;
  DB: TBytes;
begin
  Result := False;
  if (EnData = nil) or (ToBuf = nil) then
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  if EnData^ <> 0 then  // 首字节必须是 0
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  MdLen := SizeOf(TCnSHA1Digest);
  DBLen := DataByteLen - MdLen - 1;
  if DBLen <= 0 then
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  // 找出密文中的 长 MdLen 的 MaskedSeed 以及后面长 DBLen 的 MaskedDB
  MaskedSeed := PByteArray(TCnNativeInt(EnData) + 1);
  MaskedDB := PByteArray(TCnNativeInt(EnData) + MdLen + 1);

  ParamHash := SHA1Buffer(DigestParam, ParamByteLen);

  // 把 MaskedDB 先算出来
  if not Pkcs1Sha1MGF(@MaskedDB[0], DBLen, @Seed[0], MdLen) then
  begin
    _CnSetLastError(ECN_RSA_PADDING_ERROR);
    Exit;
  end;

  for I := 0 to MdLen - 1 do
    Seed[I] := Seed[I] xor MaskedSeed^[I];  // 得到 Seed

  SetLength(DB, DBLen);
  try
    if not Pkcs1Sha1MGF(@Seed[0], MdLen, @DB[0], DBLen) then
    begin
      _CnSetLastError(ECN_RSA_PADDING_ERROR);
      Exit;
    end;

    for I := 0 to DBLen - 1 do
      DB[I] := DB[I] xor MaskedDB^[I];  // 得到 DB

    // 这里 DB 的前 MdLen 字节应该等于 ParamHash，比较判断之
    if not CompareMem(@DB[0], @ParamHash[0], MdLen) then
    begin
      _CnSetLastError(ECN_RSA_PADDING_ERROR);
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
          _CnSetLastError(ECN_RSA_PADDING_ERROR);
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
//        _CnSetLastError(ECN_RSA_PADDING_ERROR);
//        Exit;
//      end;

      Move(DB[MStart], ToBuf^, DBLen - MStart);
      OutByteLen := DBLen - MStart; // 返回明文数据长度
      Result := True;
    end;
  finally
    SetLength(DB, 0);
  end;
end;

end.
