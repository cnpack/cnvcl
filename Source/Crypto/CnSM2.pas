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

unit CnSM2;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：国家商用密码 SM2 椭圆曲线算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 GM/T 0003.x-2012《SM2椭圆曲线公钥密码算法》
*           规范中的基于 SM2 的数据加解密、签名验签、密钥交换，以及协同密钥生成、协同签名、协同解密等。
*
*           注意 SM2 签名规范完全不同于 OpenSSL 中的 ECC 签名，并且杂凑函数只能使用 SM3。
*           注意签名时当 UserId 传空时内部默认会使用字符串 1234567812345678 以符合
*           《GM/T 0009-2012 SM2密码算法使用规范》第 10 节的要求。
*
*           另外，签名时计算的 Za 值是 SM3(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
*           其中 EntLen 是 UserID 的位长度（也就是字节长度 * 8）的网络顺序字节表示。
*
*           另外，注意 SM2 椭圆曲线签名无法从签名与原始值中恢复公钥。
*           虽然有 PublicKey = (s + r)^-1 * (k*G - s*G)
*           且尽管 k 对外未知但 k*G 的坐标 x1 是可用 r 反推出来，因为 r <= (e + x1) mod n
*           所以 x1 <= (r - e) mod n，因而 y1 也能算出来，但 e 使用了公钥的杂凑值，
*           导致出现了先有蛋还是先有鸡的问题。
*
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：Win7 + XE
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.01.12 V2.3
*               SM2 公钥类支持在不传 SM2 实例时加载压缩格式的公钥
*           2023.04.29 V2.2
*               将密钥交换的输出密钥格式由 AnsiString 改为 TBytes 以避免乱码
*           2023.04.10 V2.1
*               修正部分坐标值较小的情况下的加解密对齐问题
*           2023.03.25 V2.0
*               加密与签名时允许外界指定随机数，传入随机数的十六进制字符串
*           2022.12.15 V1.9
*               修正签名时可能省略前导 0 导致签名计算错误的问题
*           2022.11.01 V1.8
*               签名时允许公钥传 nil，内部通过私钥计算出公钥进行签名
*           2022.08.31 V1.7
*               根据双方协同签名机制推理出三方协同签名机制并实现，并顺手实现三方协同解密
*           2022.06.18 V1.6
*               使用预计算 2 次幂点以及基于 16 的固定基来加速 SM2 的 G 点标量乘计算
*               使用 NAF 来加速 SM2 的非 G 点标量乘计算
*           2022.06.01 V1.5
*               增加简易的协同解密与签名的实现
*           2022.05.27 V1.4
*               增加文件加解密的实现
*           2022.05.26 V1.3
*               增加非交互式 Schnorr 零知识证明验证过程的实现
*           2022.03.30 V1.2
*               兼容加解密的 C1C3C2 与 C1C2C3 排列模式以及前导字节 04
*           2021.11.25 V1.1
*               增加封装的 SignFile 与 VerifyFile 函数
*           2020.04.04 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, CnNative, CnECC, CnBigNumber, CnConsts, CnSM3;

const
  CN_SM2_FINITEFIELD_BYTESIZE = 32;
  {* SM2 椭圆曲线的素域位数，256 Bits，也是单个坐标值位数}

  CN_SM2_MIN_ENCRYPT_BYTESIZE = SizeOf(TCnSM3Digest) + CN_SM2_FINITEFIELD_BYTESIZE * 2;
  {* 最小的 SM2 加密结果长度，两个坐标加一个 SM3 摘要长度，共 96 字节}

  // 错误码
  ECN_SM2_OK                           = ECN_OK;
  {* SM2 系列错误码：无错误，值为 0}

  ECN_SM2_ERROR_BASE                   = ECN_CUSTOM_ERROR_BASE + $200;
  {* SM2 系列错误码的基准起始值，为 ECN_CUSTOM_ERROR_BASE 加上 $200}

  ECN_SM2_INVALID_INPUT                = ECN_SM2_ERROR_BASE + 1;
  {* SM2 错误码之输入为空或长度错误}
  ECN_SM2_RANDOM_ERROR                 = ECN_SM2_ERROR_BASE + 2;
  {* SM2 错误码之随机数相关错误}
  ECN_SM2_BIGNUMBER_ERROR              = ECN_SM2_ERROR_BASE + 3;
  {* SM2 错误码之大数运算错误}
  ECN_SM2_DECRYPT_INFINITE_ERROR       = ECN_SM2_ERROR_BASE + 4;
  {* SM2 错误码之解密时碰上无穷远点}
  ECN_SM2_KEYEXCHANGE_INFINITE_ERROR   = ECN_SM2_ERROR_BASE + 5;
  {* SM2 错误码之密钥交换碰上无穷远点}

type
  TCnSM2PrivateKey = TCnEccPrivateKey;
  {* SM2 的私钥就是普通椭圆曲线的私钥，可以用 ECC 中的相应 Load/Save 函数处理}

  TCnSM2PublicKey = class(TCnEccPublicKey)
  {* SM2 的公钥就是普通椭圆曲线的公钥，可以用 ECC 中的相应 Load/Save 函数处理}
  public
    procedure SetHex(const Buf: AnsiString); reintroduce;
    {* 从十六进制字符串中加载坐标点，内部有 02 03 04 前缀的处理，
       如果无 02 03 04 前缀则对半劈开分别赋值给 X 和 Y
       如果前缀是 02 或 03，说明内容只有 X 坐标，此时内部用 SM2 参数创建实例并计算 Y 坐标。

       参数：
         const Buf: AnsiString            - 十六进制字符串

       返回值：（无）
    }
  end;

  TCnSM2 = class(TCnEcc)
  {* SM2 椭圆曲线运算类，具体大部分实现在指定曲线类型的基类 TCnEcc 中}
  public
    constructor Create; override;
    {* 构造函数}

    procedure AffineMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point); override;
    {* 使用预计算的仿射坐标点进行坐标点乘法加速。

       参数：
         K: TCnBigNumber                  - 乘数，形式为大数
         Point: TCnEcc3Point              - 待计算的坐标点

       返回值：（无）
    }
  end;

  TCnSM2Signature = class(TCnEccSignature);
  {* SM2 椭圆曲线签名的内容就是普通椭圆曲线的签名的内容（注意与 ECC 的算法与文件格式不同）}

  TCnSM2CryptSequenceType = (cstC1C3C2, cstC1C2C3);
  {* SM2 加密数据时的拼接方式，国标上是 C1C3C2，但经常有想当然的 C1C2C3 版本，故此本单元做兼容}

  TCnSM2CollaborativePrivateKey = TCnSM2PrivateKey;
  {* SM2 协同私钥就是普通椭圆曲线的私钥，但有至少两个}

  TCnSM2CollaborativePublicKey = TCnSM2PublicKey;
  {* SM2 协同私钥就是普通椭圆曲线的公钥，同样是一个}

// ========================== SM2 椭圆曲线密钥生成 =============================

function CnSM2GenerateKeys(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil): Boolean;
{* 生成一对 SM2 公私钥。

   参数：
     PrivateKey: TCnSM2PrivateKey         - 待生成的 SM2 私钥
     PublicKey: TCnSM2PublicKey           - 待生成的 SM2 公钥
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回生成是否成功
}

function CnSM2CheckKeys(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil): Boolean;
{* 检验一对 SM2 公私钥是否合法。

   参数：
     PrivateKey: TCnSM2PrivateKey         - 待检验的 SM2 私钥
     PublicKey: TCnSM2PublicKey           - 待检验的 SM2 公钥
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回检验是否合法
}

// ========================= SM2 椭圆曲线加解密算法 ============================

function CnSM2EncryptData(PlainData: Pointer; DataByteLen: Integer; OutStream:
  TStream; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2;
  IncludePrefixByte: Boolean = True; const RandHex: string = ''): Boolean; overload;
{* 用公钥对数据块进行加密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则。
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3，
   IncludePrefixByte 用来声明是否包括 C1 前导的 $04 一字节，默认包括。
   返回加密是否成功，加密结果写入 OutStream 中。

   参数：
     PlainData: Pointer                                   - 待加密的明文数据块地址
     DataByteLen: Integer                                 - 待加密的明文数据块字节长度
     OutStream: TStream                                   - 输出的密文流
     PublicKey: TCnSM2PublicKey                           - 加密用的 SM2 公钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 输出密文的内部拼接顺序，默认国标的 C1C3C2
     IncludePrefixByte: Boolean                           - 是否包括 C1 的前导字节 $04，默认包括
     const RandHex: string                                - 可外部指定随机数的十六进制字符串，默认为空，空则内部生成

   返回值：Boolean                                        - 返回加密是否成功
}

function CnSM2EncryptData(PlainData: TBytes; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2;
  IncludePrefixByte: Boolean = True; const RandHex: string = ''): TBytes; overload;
{* 用公钥对字节数组进行加密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则。
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3，
   IncludePrefixByte 用来声明是否包括 C1 前导的 $04 一字节，默认包括。
   返回密文字节数组，如果加密失败则返回空。

   参数：
     PlainData: TBytes                                    - 待加密的明文字节数组
     PublicKey: TCnSM2PublicKey                           - 加密用的 SM2 公钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 输出密文的内部拼接顺序，默认国标的 C1C3C2
     IncludePrefixByte: Boolean                           - 输出密文中是否要包括 C1 的前导字节 $04，默认包括
     const RandHex: string                                - 可外部指定随机数的十六进制字符串，默认为空，空则内部生成

   返回值：TBytes                                         - 如果成功则返回密文字节数组，失败则返回空
}

function CnSM2DecryptData(EnData: Pointer; DataByteLen: Integer; OutStream: TStream;
  PrivateKey: TCnSM2PrivateKey; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean; overload;
{* 用私钥对数据块进行解密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则。
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3。
   无需 IncludePrefixByte 参数，内部自动处理。
   返回解密是否成功，解出的明文写入 OutStream 中。

   参数：
     EnData: Pointer                                      - 待解密的密文数据块地址
     DataByteLen: Integer                                 - 待解密的密文数据块字节长度
     OutStream: TStream                                   - 输出的明文流
     PrivateKey: TCnSM2PrivateKey                         - 解密用的 SM2 私钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 内部拼接顺序，默认国标的 C1C3C2，需和密文的实际情况一致

   返回值：Boolean                                        - 返回解密是否成功
}

function CnSM2DecryptData(EnData: TBytes; PrivateKey: TCnSM2PrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): TBytes; overload;
{* 用私钥对数据块进行解密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则。
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3。
   无需 IncludePrefixByte 参数，内部自动处理。
   返回解密后的明文字节数组，如果解密失败则返回空。

   参数：
     EnData: TBytes                                       - 待解密的密文字节数组
     PrivateKey: TCnSM2PrivateKey                         - 解密用的 SM2 私钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 内部拼接顺序，默认国标的 C1C3C2，需和密文的实际情况一致

   返回值：TBytes                                         - 如果成功则返回明文字节数组，失败则返回空
}

function CnSM2EncryptFile(const InFile: string; const OutFile: string; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2;
  IncludePrefixByte: Boolean = True; const RandHex: string = ''): Boolean;
{* 用公钥加密 InFile 文件内容，加密结果存 OutFile 里，返回是否加密成功。
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3。
   IncludePrefixByte 用来声明是否包括 C1 前导的 $04 一字节，默认包括。

   参数：
     const InFile: string                                 - 待加密的明文原始文件名
     const OutFile: string                                - 密文输出的目标文件名
     PublicKey: TCnSM2PublicKey                           - 加密用的 SM2 公钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 输出密文的内部拼接顺序，默认国标的 C1C3C2
     IncludePrefixByte: Boolean                           - 输出密文中是否要包括 C1 的前导字节 $04，默认包括
     const RandHex: string                                - 可外部指定随机数的十六进制字符串，默认为空，空则内部生成

   返回值：Boolean                                        - 返回加密是否成功
}

function CnSM2DecryptFile(const InFile: string; const OutFile: string; PrivateKey: TCnSM2PrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
{* 用私钥解密 InFile 文件内容，解密结果存 OutFile 里，返回是否解密成功。
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3。
   无需 IncludePrefixByte 参数，内部自动处理。

   参数：
     const InFile: string                                 - 待解密的密文文件名
     const OutFile: string                                - 明文输出的目标文件名
     PrivateKey: TCnSM2PrivateKey                         - 解密用的 SM2 私钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 内部拼接顺序，默认国标的 C1C3C2，需和密文的实际情况一致

   返回值：Boolean                                        - 返回解密是否成功
}

function CnSM2CryptToAsn1(EnData: TBytes; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2; IncludePrefixByte: Boolean = True): TBytes; overload;
{* 将 EnData 中字节数组形式的原始加密内容转换为 ASN1/BER 格式的字节数组。

   参数：
     EnData: TBytes                                       - 待转换的密文字节数组
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 密文的内部拼接顺序，默认国标的 C1C3C2
     IncludePrefixByte: Boolean                           - 密文中是否包括 C1 的前导字节 $04，默认包括

   返回值：TBytes                                         - 返回转换后的字节数组
}

function CnSM2CryptToAsn1(EnStream: TStream; OutStream: TStream; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2; IncludePrefixByte: Boolean = True): Boolean; overload;
{* 将 EnStream 中流格式的原始加密内容转换为 ASN1/BER 格式并写入 OutStream 流中。

   参数：
     EnStream: TStream                                    - 待转换的密文流
     OutStream: TStream                                   - 转换后的目标流
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 密文的内部拼接顺序，默认国标的 C1C3C2
     IncludePrefixByte: Boolean                           - 密文中是否包括 C1 的前导字节 $04，默认包括

   返回值：Boolean                                        - 返回转换是否成功
}

function CnSM2CryptFromAsn1(Asn1Data: TBytes; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2; IncludePrefixByte: Boolean = True): TBytes; overload;
{* 将 Asn1Data 中 ASN1/BER 格式的字节数组形式的加密内容转换为原始字节数组

   参数：
     Asn1Data: TBytes                                     - 待转换的 ASN1 格式的密文字节数组
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 内部拼接顺序，默认国标的 C1C3C2，需和 ASN1 格式的密文字节数组的实际情况一致
     IncludePrefixByte: Boolean                           - 输出的字节数组中是否包括 C1 的前导字节 $04，默认包括

   返回值：TBytes                                         - 返回转换后的字节数组
}

function CnSM2CryptFromAsn1(Asn1Stream: TStream; OutStream: TStream; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2; IncludePrefixByte: Boolean = True): Boolean; overload;
{* 将 Asn1Stream 中 ASN1/BER 格式流的加密内容转换为原始加密内容并写入 OutStream 流中

   参数：
     Asn1Stream: TStream                                  - 待转换的 ASN1 格式的密文流
     OutStream: TStream                                   - 输出的原始密文流
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 内部拼接顺序，默认国标的 C1C3C2，需和 ASN1 格式的密文流的实际情况一致
     IncludePrefixByte: Boolean                           - 输出的流中是否包括 C1 的前导字节 $04，默认包括

   返回值：Boolean                                        - 返回转换是否成功
}

// ====================== SM2 椭圆曲线数字签名验证算法 =========================

function CnSM2SignData(const UserID: AnsiString; PlainData: Pointer; DataByteLen: Integer;
  OutSignature: TCnSM2Signature; PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey = nil;
  SM2: TCnSM2 = nil; const RandHex: string = ''): Boolean; overload;
{* 私钥对数据块签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法第2部分:数字签名算法》
   中的运算规则，要附上签名者与曲线信息以及公钥的数字摘要。返回签名是否成功。

   参数：
     const UserID: AnsiString             - 用来签名的用户标识
     PlainData: Pointer                   - 待签名的明文数据块地址
     DataByteLen: Integer                 - 待签名的明文数据块字节长度
     OutSignature: TCnSM2Signature        - 输出的签名值
     PrivateKey: TCnSM2PrivateKey         - 用来签名的 SM2 私钥
     PublicKey: TCnSM2PublicKey           - 用来签名的 SM2 公钥，可传 nil，内部将使用 PrivateKey 重新计算出 PublickKey 参与签名
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空
     const RandHex: string                - 可外部指定随机数的十六进制字符串，默认为空，空则内部生成

   返回值：Boolean                        - 返回签名是否成功
}

function CnSM2SignData(const UserID: AnsiString; PlainData: TBytes;
  OutSignature: TCnSM2Signature; PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey = nil;
  SM2: TCnSM2 = nil; const RandHex: string = ''): Boolean; overload;
{* 私钥对字节数组签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法第2部分:数字签名算法》
   中的运算规则，要附上签名者与曲线信息以及公钥的数字摘要。返回签名是否成功。

   参数：
     const UserID: AnsiString             - 用来签名的用户标识
     PlainData: TBytes                    - 待签名的明文字节数组
     OutSignature: TCnSM2Signature        - 输出的签名值
     PrivateKey: TCnSM2PrivateKey         - 用来签名的 SM2 私钥
     PublicKey: TCnSM2PublicKey           - 用来签名的 SM2 公钥，可传 nil，内部将使用 PrivateKey 重新计算出 PublickKey 参与签名
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空
     const RandHex: string                - 可外部指定随机数的十六进制字符串，默认为空，空则内部生成

   返回值：Boolean                        - 返回签名是否成功
}

function CnSM2VerifyData(const UserID: AnsiString; PlainData: Pointer; DataByteLen: Integer;
  InSignature: TCnSM2Signature; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean; overload;
{* 公钥验证数据块的签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法
   第2部分:数字签名算法》中的运算规则来

   参数：
     const UserID: AnsiString             - 用来验证签名的用户标识，需和签名的用户标识保持一致
     PlainData: Pointer                   - 待验证的明文数据块地址
     DataByteLen: Integer                 - 待验证的明文数据块字节长度
     InSignature: TCnSM2Signature         - 待验证的签名值
     PublicKey: TCnSM2PublicKey           - 用来验证的 SM2 公钥
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnSM2VerifyData(const UserID: AnsiString; PlainData: TBytes;
  InSignature: TCnSM2Signature; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean; overload;
{* 公钥验证字节数组的签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法
   第2部分:数字签名算法》中的运算规则来

   参数：
     const UserID: AnsiString             - 用来验证签名的用户标识，需和签名的用户标识保持一致
     PlainData: TBytes                    - 待验证的明文字节数组
     InSignature: TCnSM2Signature         - 待验证的签名值
     PublicKey: TCnSM2PublicKey           - 用来验证的 SM2 公钥
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnSM2SignFile(const UserID: AnsiString; const FileName: string;
  PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey = nil; SM2: TCnSM2 = nil): string;
{* 封装的私钥对文件签名操作，返回签名值的十六进制字符串，注意内部操作是将文件全部加载入内存，
  如签名出错则返回空字符串。

   参数：
     const UserID: AnsiString             - 用来签名的用户标识
     const FileName: string               - 待签名的文件名
     PrivateKey: TCnSM2PrivateKey         - 用来签名的 SM2 私钥
     PublicKey: TCnSM2PublicKey           - 用来签名的 SM2 公钥，可传 nil，内部将使用 PrivateKey 重新计算出 PublickKey 参与签名
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：string                         - 返回签名值的十六进制字符串
}

function CnSM2VerifyFile(const UserID: AnsiString; const FileName: string;
  const InHexSignature: string; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean;
{* 封装的公钥验证数据块的签名，参数是签名值的十六进制字符串，注意内部操作是将文件全部加载入内存。

   参数：
     const UserID: AnsiString             - 用来验证签名的用户标识，需和签名的用户标识保持一致
     const FileName: string               - 待验证的文件名
     const InHexSignature: string         - 待验证的十六进制的签名值
     PublicKey: TCnSM2PublicKey           - 用来验证的 SM2 公钥
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回验证签名是否成功
}

// ======================== SM2 椭圆曲线密钥交换算法 ===========================

{
  SM2 密钥交换前提：A B 双方都有自身 ID 与公私钥，并都知道对方的 ID 与对方的公钥
}
function CnSM2KeyExchangeAStep1(const AUserID: AnsiString; const BUserID: AnsiString;
  KeyByteLength: Integer; APrivateKey: TCnSM2PrivateKey; APublicKey: TCnSM2PublicKey;
  BPublicKey: TCnSM2PublicKey; OutARand: TCnBigNumber; OutRA: TCnEccPoint; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第一步 A 用户生成随机点 RA，供发给 B。
   输入：A B 的用户名，所需密码长度、自己的私钥、双方的公钥。
   输出：随机值 OutARand；生成的随机点 RA（发给 B）

   参数：
     const AUserID: AnsiString            - A 方的用户标识
     const BUserID: AnsiString            - B 方的用户标识
     KeyByteLength: Integer               - 需要交换的密钥字节长度
     APrivateKey: TCnSM2PrivateKey        - A 方的 SM2 私钥
     APublicKey: TCnSM2PublicKey          - A 方的 SM2 公钥
     BPublicKey: TCnSM2PublicKey          - B 方的 SM2 私钥
     OutARand: TCnBigNumber               - 生成的中间结果随机数，需在本次交换会话中保留，不能传输给 B 方
     OutRA: TCnEccPoint                   - 生成的中间结果坐标点 R，需在本次交换会话中保留，并需传输给 B 方
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回生成是否成功
}

function CnSM2KeyExchangeBStep1(const AUserID: AnsiString; const BUserID: AnsiString;
  KeyByteLength: Integer; BPrivateKey: TCnSM2PrivateKey; APublicKey: TCnSM2PublicKey;
  BPublicKey: TCnSM2PublicKey; InRA: TCnEccPoint; out OutKeyB: TBytes; OutRB: TCnEccPoint;
  out OutOptionalSB: TCnSM3Digest; out OutOptionalS2: TCnSM3Digest; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第二步 B 用户收到 A 的数据，计算 Kb，并把可选的验证结果返回 A。
   输入：A B 的用户名，所需密码长度、自己的私钥、双方的公钥、A 传来的 RA。
   输出：计算成功的共享密钥 Kb、生成的随机点 RB（发给 A）、可选的校验杂凑 SB（发给 A 验证），可选的校验杂凑 S2。

   参数：
     const AUserID: AnsiString            - A 方的用户标识
     const BUserID: AnsiString            - B 方的用户标识
     KeyByteLength: Integer               - 需要交换的密钥字节长度
     BPrivateKey: TCnSM2PrivateKey        - B 方的 SM2 私钥
     APublicKey: TCnSM2PublicKey          - A 方的 SM2 公钥
     BPublicKey: TCnSM2PublicKey          - B 方的 SM2 私钥
     InRA: TCnEccPoint                    - 由 A 方生成并传输而来的坐标点 R
     out OutKeyB: TBytes                  - B 方交换输出的密钥字节数组，值应等于下面的 OutKeyA
     OutRB: TCnEccPoint                   - 生成的中间结果坐标点 R，需传输回 A 方
     out OutOptionalSB: TCnSM3Digest      - 生成的校验杂凑值 S，可传输给 A 方供验证
     out OutOptionalS2: TCnSM3Digest      - 生成的校验杂凑值 S2，需在本次交换会话中保留，不能传输给 A 方
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回交换是否成功
}

function CnSM2KeyExchangeAStep2(const AUserID: AnsiString; const BUserID: AnsiString;
  KeyByteLength: Integer; APrivateKey: TCnSM2PrivateKey; APublicKey: TCnSM2PublicKey;
  BPublicKey: TCnSM2PublicKey; MyRA: TCnEccPoint; InRB: TCnEccPoint; MyARand: TCnBigNumber;
  out OutKeyA: TBytes; InOptionalSB: TCnSM3Digest; out OutOptionalSA: TCnSM3Digest; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第三步 A 用户收到 B 的数据计算 Ka，并把可选的验证结果返回 B，初步协商好 Ka = Kb
  输入：A B 的用户名，所需密码长度、自己的私钥、双方的公钥、B 传来的 RB 与可选的 SB，自己的点 RA、自己的随机值 MyARand
  输出：计算成功的共享密钥 Ka、可选的校验杂凑 SA（发给 B 验证）。

   参数：
     const AUserID: AnsiString            - A 方的用户标识
     const BUserID: AnsiString            - B 方的用户标识
     KeyByteLength: Integer               - 需要交换的密钥字节长度
     APrivateKey: TCnSM2PrivateKey        - A 方的 SM2 私钥
     APublicKey: TCnSM2PublicKey          - A 方的 SM2 公钥
     BPublicKey: TCnSM2PublicKey          - B 方的 SM2 公钥
     MyRA: TCnEccPoint                    - A 方第一次调用时生成的中间结果坐标点 R
     InRB: TCnEccPoint                    - 由 B 方生成并传输而来的中间结果坐标点 R
     MyARand: TCnBigNumber                - A 方第一次调用时生成的中间结果随机数
     out OutKeyA: TBytes                  - A 方交换输出的密钥字节数组，值应等于下面的 OutKeyB
     InOptionalSB: TCnSM3Digest           - 由 B 方生成并传输而来的校验杂凑值 S 供验证
     out OutOptionalSA: TCnSM3Digest      - 生成的校验杂凑值 S
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回交换是否成功
}

function CnSM2KeyExchangeBStep2(const AUserID: AnsiString; const BUserID: AnsiString;
  KeyByteLength: Integer; BPrivateKey: TCnSM2PrivateKey; APublicKey: TCnSM2PublicKey;
  BPublicKey: TCnSM2PublicKey; InOptionalSA: TCnSM3Digest; MyOptionalS2: TCnSM3Digest;
  SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第四步 B 用户收到 A 的数据计算结果校验，协商完毕，此步可选。
   实质上只对比 B 第二步生成的 S2 与 A 第三步发来的 SA，其余参数均不使用。

   参数：
     const AUserID: AnsiString            - A 方的用户标识
     const BUserID: AnsiString            - B 方的用户标识
     KeyByteLength: Integer               - 需要交换的密钥字节长度
     BPrivateKey: TCnSM2PrivateKey        - B 方的 SM2 私钥
     APublicKey: TCnSM2PublicKey          - A 方的 SM2 公钥
     BPublicKey: TCnSM2PublicKey          - B 方的 SM2 公钥
     InOptionalSA: TCnSM3Digest           - 由 A 方生成并传输而来的校验杂凑值 S 供验证
     MyOptionalS2: TCnSM3Digest           - B 方第二次调用时生成的校验杂凑值供与 S 对比
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回校验是否成功
}

// =============== 基于 SM2/SM3 的非交互式 Schnorr 零知识证明 ==================

function CnSM2SchnorrProve(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  OutR: TCnEccPoint; OutZ: TCnBigNumber; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2/SM3 的非交互式 Schnorr 零知识证明步骤一，由私钥拥有者调用。
  私钥拥有者生成 R 和 Z，返回生成是否成功。
  该函数用于 SM2 私钥拥有者证明自己拥有对应公钥的私钥而无需公开该私钥。

   参数：
     PrivateKey: TCnSM2PrivateKey         - 进行 Schnorr 零知识证明的 SM2 私钥
     PublicKey: TCnSM2PublicKey           - 进行 Schnorr 零知识证明的 SM2 公钥
     OutR: TCnEccPoint                    - SM2 私钥拥有者生成的 R 坐标点
     OutZ: TCnBigNumber                   - SM2 私钥拥有者生成的 Z 大数值
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回生成是否成功
}

function CnSM2SchnorrCheck(PublicKey: TCnSM2PublicKey; InR: TCnEccPoint;
  InZ: TCnBigNumber; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2/SM3 的非交互式 Schnorr 零知识证明步骤二，由拿到公钥者验证
   验证对方发来的 R 和 Z，如果成功，说明对方拥有该公钥对应的私钥
   该函数用于验证对方是否拥有某 SM2 公钥对应的私钥

   参数：
     PublicKey: TCnSM2PublicKey           - 用来验证 Schnorr 零知识证明的 SM2 公钥
     InR: TCnEccPoint                     - Schnorr 零知识证明中生成的 R 坐标点
     InZ: TCnBigNumber                    - Schnorr 零知识证明中生成的 Z 大数值
     SM2: TCnSM2                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                        - 返回验证是否成功
}

// ========== SM2 椭圆曲线双方互相信任的简易协同算法之协同密钥生成 =============

{
  本协同模式下，A B 双方互相信任，因而不对对方做认证，无条件相信对方发来的数据。
  其中：公钥 = （私钥分量A * 私钥分量B - 1）* G
}
function CnSM2CollaborativeGenerateKeyAStep1(PrivateKeyA: TCnSM2CollaborativePrivateKey;
  OutPointToB: TCnEccPoint; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同算法，A 第一步生成自己的私钥分量 PrivateKeyA，并产出中间结果 OutPointToB，
   该点值需要传输至 B，返回是否生成成功。

   参数：
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 双方协同模式下 A 方生成的 SM2 私钥分量，A 方需保密
     OutPointToB: TCnEccPoint                             - 双方协同模式下生成的中间结果坐标点，需传输给 B 方
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

function CnSM2CollaborativeGenerateKeyBStep1(PrivateKeyB: TCnSM2CollaborativePrivateKey;
  InPointFromA: TCnEccPoint; PublicKey: TCnSM2CollaborativePublicKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同算法，B 第二步生成自己的私钥分量 PrivateKeyB，并根据 A 传来的中间结果 InPointFromA，
   生成公用的公钥 PublicKey，返回是否生成成功。公钥 PublicKey 后面需要传给 A 并公布出去。

   参数：
     PrivateKeyB: TCnSM2CollaborativePrivateKey           - 双方协同模式下 B 方生成的 SM2 私钥分量，B 方需保密
     InPointFromA: TCnEccPoint                            - 双方协同模式下由 A 方生成并传输而来的中间结果坐标点
     PublicKey: TCnSM2CollaborativePublicKey              - 双方协同模式下生成的共同的 SM2 公钥，可公布并传输给 A 方
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

// =============== SM2 椭圆曲线双方互相信任的简易协同签名算法 ==================

function CnSM2CollaborativeSignAStep1(const UserID: AnsiString; PlainData: Pointer;
  DataByteLen: Integer; OutHashEToB: TCnBigNumber; OutQToB: TCnEccPoint; OutRandKA: TCnBigNumber;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同签名，A 第一步根据原始数据签出中间值 E 和 Q，发送给 B，返回该步签名是否成功
   注意 OutRandK 不要发给 B！另外，注意该步 PrivateKeyA 未使用。

   参数：
     const UserID: AnsiString                             - 双方协同模式下用来签名的共同的用户标识
     PlainData: Pointer                                   - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     OutHashEToB: TCnBigNumber                            - 双方协同模式下 A 方输出的中间值 E 大数值，需传输给 B 方
     OutQToB: TCnEccPoint                                 - 双方协同模式下 A 方输出的中间值 Q 坐标点，需传输给 B 方
     OutRandKA: TCnBigNumber                              - 双方协同模式下 A 方输出的中间随机值 K，A 方需保密
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 双方协同模式下 A 方的 SM2 私钥分量，该步内部暂未使用
     PublicKey: TCnSM2PublicKey                           - 双方协同模式下共同的 SM2 公钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回该 A 方的第一步签名是否成功
}

function CnSM2CollaborativeSignBStep1(InHashEFromA: TCnBigNumber; InQFromA: TCnEccPoint;
  OutRToA: TCnBigNumber; OutS1ToA: TCnBigNumber; OutS2ToA: TCnBigNumber;
  PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同签名，B 第二步根据 A 签出的中间值 E 和 Q，
   结合 PrivateKeyB 生成 R S1 S2 发送回 A，返回该步签名是否成功。

   参数：
     InHashEFromA: TCnBigNumber                           - 双方协同模式下由 A 方在第一步生成并传输而来的 E 大数值
     InQFromA: TCnEccPoint                                - 双方协同模式下由 A 方在第一步生成并传输而来的 Q 坐标点
     OutRToA: TCnBigNumber                                - 双方协同模式下 B 方生成的中间值 R 大数值，需传输回 A 方
     OutS1ToA: TCnBigNumber                               - 双方协同模式下 B 方生成的中间值 S1 大数值，需传输回 A 方
     OutS2ToA: TCnBigNumber                               - 双方协同模式下 B 方生成的中间值 S2 大数值，需传输回 A 方
     PrivateKeyB: TCnSM2CollaborativePrivateKey           - 双方协同模式下 B 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回该 B 方的第二步签名是否成功
}

function CnSM2CollaborativeSignAStep2(InRandKA: TCnBigNumber; InRFromB: TCnBigNumber;
  InS1FromB: TCnBigNumber; InS2FromB: TCnBigNumber; OutSignature: TCnSM2Signature;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同签名，A 第三步根据 A 第一步的 OutRandK 随机值与 B 签出的中间值 R S1 S2，
   结合 PrivateKeyA 生成最终签名，返回该步签名是否成功。

   参数：
     InRandKA: TCnBigNumber                               - A 方第一步签名中生成的中间随机值 K
     InRFromB: TCnBigNumber                               - 双方协同模式下由 B 方在第二步生成并传输回来的 R 大数值
     InS1FromB: TCnBigNumber                              - 双方协同模式下由 B 方在第二步生成并传输回来的 S1 大数值
     InS2FromB: TCnBigNumber                              - 双方协同模式下由 B 方在第二步生成并传输回来的 S2 大数值
     OutSignature: TCnSM2Signature                        - 输出的最终签名值
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 双方协同模式下 A 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回该 A 方的第三步也就是最终步签名是否成功
}

// =============== SM2 椭圆曲线双方互相信任的简易协同解密算法 ==================

function CnSM2CollaborativeDecryptAStep1(EnData: Pointer; DataByteLen: Integer;
  OutTToB: TCnEccPoint; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同解密，A 第一步根据密文解出中间值 T，发送给 B，返回该步解密是否成功。

   参数：
     EnData: Pointer                                      - 待解密的密文数据块地址
     DataByteLen: Integer                                 - 待解密的密文数据块字节长度
     OutTToB: TCnEccPoint                                 - 双方协同模式下 A 方生成的中间结果坐标点 T，需传输给 B 方
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 双方协同模式下 A 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回 A 方的第一步解密是否成功
}

function CnSM2CollaborativeDecryptBStep1(InTFromA: TCnEccPoint; OutTToA: TCnEccPoint;
  PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同解密，B 第二步根据 A 解出的中间值 T，生成另一个中间值 T 发送回 A，
   返回该步解密是否成功。

   参数：
     InTFromA: TCnEccPoint                                - 双方协同模式下由 A 方生成并传输而来的中间结果坐标点 T
     OutTToA: TCnEccPoint                                 - 双方协同模式下 B 方生成的中间结果坐标点 T，需传输回 A 方
     PrivateKeyB: TCnSM2CollaborativePrivateKey           - 双方协同模式下 B 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回 B 方的第二步解密是否成功
}

function CnSM2CollaborativeDecryptAStep2(EnData: Pointer; DataByteLen: Integer;
  InTFromB: TCnEccPoint; OutStream: TStream; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
{* 基于 SM2 椭圆曲线的双方协同解密，A 第三步根据 B 解出的中间值 T 算出最终解密结果写入 Stream，
  返回该步最终解密是否成功
  注意密文与 SequenceType 须保持和 AStep1 中的完全一致

   参数：
     EnData: Pointer                                      - 待解密的密文数据块地址
     DataByteLen: Integer                                 - 待解密的密文数据块字节长度
     InTFromB: TCnEccPoint                                - 双方协同模式下由 B 方生成并传输而来的中间结果坐标点 T
     OutStream: TStream                                   - 输出的明文流
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 双方协同模式下 A 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 内部拼接顺序，默认国标的 C1C3C2，需和密文的实际情况一致

   返回值：Boolean                                        - 返回 A 方的第三步也就是最终步的解密是否成功
}

// ======== SM2 椭圆曲线三方或更多方互相信任的简易协同算法之协同密钥生成 =======
{
  本协同模式下，A B C 三方或更多方互相信任，因而不对对方做认证，无条件相信对方发来的数据。
  多方模式下，允许中间的非头尾方沿用 CnSM2Collaborative3 * BStep1 这类调用进行多次
  内部本质上等同于双方协同，只是中间步骤存在 0 步 ～ 多步之分

  其中：公钥 = （私钥分量A * 私钥分量B * 私钥分量C - 1）* G
}
function CnSM2Collaborative3GenerateKeyAStep1(PrivateKeyA: TCnSM2CollaborativePrivateKey;
  OutPointToB: TCnEccPoint; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同算法，A 第一步生成自己的私钥分量 PrivateKeyA，并产出中间结果 OutPointToB，
   该点值需要传输至 B，返回是否生成成功。

   参数：
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 多方协同模式下 A 方生成的 SM2 私钥分量
     OutPointToB: TCnEccPoint                             - 多方协同模式下 A 方生成的中间结果坐标点，需传输给 B 方
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

function CnSM2Collaborative3GenerateKeyBStep1(PrivateKeyB: TCnSM2CollaborativePrivateKey;
  InPointFromA: TCnEccPoint; OutPointToC: TCnEccPoint; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同算法，B 第二步生成自己的私钥分量 PrivateKeyB，并根据 A 传来的中间结果 InPointFromA，
   生成中间结果 OutPointToC，该点值需要传输至 C。返回是否生成成功。
   如果更多方，C 也需调用本方法生成给 D 的，以此类推。

   参数：
     PrivateKeyB: TCnSM2CollaborativePrivateKey           - 多方协同模式下 B 方或其他中间方生成的 SM2 私钥分量
     InPointFromA: TCnEccPoint                            - 多方协同模式下由 A 方或上一方生成并传输而来的中间结果坐标点
     OutPointToC: TCnEccPoint                             - 多方协同模式下 B 方或其他中间方生成的中间结果坐标点，需要传输给下一方
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

function CnSM2Collaborative3GenerateKeyCStep1(PrivateKeyC: TCnSM2CollaborativePrivateKey;
  InPointFromB: TCnEccPoint; PublicKey: TCnSM2CollaborativePublicKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同算法，C 第三步生成自己的私钥分量 PrivateKeyC，并根据 B 传来的中间结果 InPointFromB，
   生成公用的公钥 PublicKey，返回是否生成成功。如果更多方，本方法是最后一位调用的。
   公钥 PublicKey 后面需要传给 A、B 并公布出去。

   参数：
     PrivateKeyC: TCnSM2CollaborativePrivateKey           - 多方协同模式下 C 方或最后一方生成的 SM2 私钥分量
     InPointFromB: TCnEccPoint                            - 多方协同模式下由 B 方或上一方生成并传输而来的中间结果坐标点
     PublicKey: TCnSM2CollaborativePublicKey              - 多方协同模式下 C 方或最后一方生成的 SM2 公钥，可公布并传输给 A B 及每一方
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

// =========== SM2 椭圆曲线三方或更多方互相信任的简易协同签名算法 ==============
{
  核心过程是 A -> B (-> B') -> C (-> B') -> B  -> A，箭头左边为上一方，右边为下一方
}

function CnSM2Collaborative3SignAStep1(const UserID: AnsiString; PlainData: Pointer;
  DataByteLen: Integer; OutHashEToBC: TCnBigNumber; OutQToB: TCnEccPoint; OutRandKA: TCnBigNumber;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同签名，A 第一步根据原始数据签出中间值 E 和 Qa，发送给 B，返回该步签名是否成功。
   OutHashEToBC 要发给下一步 B 以及下下步 C，对应 InHashEFromA，OutQToB 要发给下一步 B，对应 InQFromA。
   注意 OutRandKA 要保存待第五步调用，不要发出去！

   参数：
     const UserID: AnsiString                             - 多方协同模式下用来签名的共同的用户标识
     PlainData: Pointer                                   - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     OutHashEToBC: TCnBigNumber                           - 多方协同模式下 A 方生成的杂凑值，需要传输给后面的每一方
     OutQToB: TCnEccPoint                                 - 多方协同模式下 A 方生成的中间结果坐标点 Q，需要传输给 B 方
     OutRandKA: TCnBigNumber                              - 多方协同模式下 A 方生成的中间随机值 K，A 方需保密
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 多方协同模式下 A 方的 SM2 私钥分量
     PublicKey: TCnSM2PublicKey                           - 多方协同模式下的 SM2 公钥
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

function CnSM2Collaborative3SignBStep1(InHashEFromA: TCnBigNumber; InQFromA: TCnEccPoint;
  OutQToC: TCnEccPoint; OutRandKB: TCnBigNumber; PrivateKeyB: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同签名，B 第二步根据 A 第一步签出的中间值 E 和 Qa，生成 Qb 与 E 一起发给 C，返回该步签名是否成功。
   InHashEFromA 来源于上一步的 OutHashEToBC，InQFromA 来源于上一步的 OutQToB，OutQToC 要发给下一步 C，对应 InQFromB。
   注意 OutRandKB 要保存待第四步调用，不要发出去！

   参数：
     InHashEFromA: TCnBigNumber                           - 多方协同模式下由 A 方生成并传输而来的杂凑值
     InQFromA: TCnEccPoint                                - 多方协同模式下由 A 方或上一方生成并传输而来的中间结果坐标点 Q
     OutQToC: TCnEccPoint                                 - 多方协同模式下 B 方生成的中间结果坐标点 Q，需传输给 C 方或下一方
     OutRandKB: TCnBigNumber                              - 多方协同模式下 B 方生成的中间随机值 K，B 方需保密
     PrivateKeyB: TCnSM2CollaborativePrivateKey           - 多方协同模式下 B 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

function CnSM2Collaborative3SignCStep1(InHashEFromA: TCnBigNumber; InQFromB: TCnEccPoint;
  OutRToBA: TCnBigNumber; OutS1ToB: TCnBigNumber; OutS2ToB: TCnBigNumber;
  PrivateKeyC: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同签名，C 第三步根据 B 第二步签出的中间值 E 和 Qb，生成 R S1 S2 发送回 B，返回该步签名是否成功
   InHashEFromA 来源于上一步的 OutHashEToBC，InQFromB 来源于上一步的 OutQToC，OutRToBA 要发给下一步 B 以及下下步 A，对应 InRFromC。
   OutS1ToB 要发给下一步 B，对应 InS1FromC，OutS2ToB 要发给下一步 B，对应 InS2FromC。

   参数：
     InHashEFromA: TCnBigNumber                           - 多方协同模式下由 A 方生成并传输而来的杂凑值
     InQFromB: TCnEccPoint                                - 多方协同模式下由 B 方或上一方生成并传输而来的中间结果坐标点 Q
     OutRToBA: TCnBigNumber                               - 多方协同模式下 C 方或最后一方生成的中间值 R，需传输给前面各方
     OutS1ToB: TCnBigNumber                               - 多方协同模式下 C 方或最后一方生成的中间签名值 S1，需传输给上一方
     OutS2ToB: TCnBigNumber                               - 多方协同模式下 C 方或最后一方生成的中间签名值 S2，需传输给上一方
     PrivateKeyC: TCnSM2CollaborativePrivateKey           - 多方协同模式下 C 方或最后一方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

function CnSM2Collaborative3SignBStep2(InRandKB: TCnBigNumber; InRFromC: TCnBigNumber;
  InS1FromC: TCnBigNumber; InS2FromC: TCnBigNumber; OutS1ToA: TCnBigNumber;
  OutS2ToA: TCnBigNumber; PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同签名，B 第四步根据 C 第三步签出的中间值生成新的 S1 S2 与 R 发给 A，返回该步签名是否成功
   InRandKB 是第二步中的 OutRandKB，InRFromC 来源于上一步的 OutRToBA，InS1FromC 来源于上一步的 OutS1ToB，
   InS2FromC 来源于上一步的 OutS2ToB，OutS1ToA 要发给下一步 A，对应 InS1FromB，OutS2ToA 要发给下一步 A，对应 InS2FromB。

   参数：
     InRandKB: TCnBigNumber                               - 多方协同模式下由 B 方在前一步中生成的中间随机值 K
     InRFromC: TCnBigNumber                               - 多方协同模式下由 C 方或最后一方生成并传输而来的中间值 R
     InS1FromC: TCnBigNumber                              - 多方协同模式下由 C 方或最后一方生成并传输而来的中间签名值 S1
     InS2FromC: TCnBigNumber                              - 多方协同模式下由 C 方或最后一方生成并传输而来的中间签名值 S2
     OutS1ToA: TCnBigNumber                               - 多方协同模式下生成的中间签名值 S1，需传输给 A 方或下一方
     OutS2ToA: TCnBigNumber                               - 多方协同模式下生成的中间签名值 S2，需传输给 A 方或下一方
     PrivateKeyB: TCnSM2CollaborativePrivateKey           - 多方协同模式下 B 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回生成是否成功
}

function CnSM2Collaborative3SignAStep2(InRandKA: TCnBigNumber; InRFromC: TCnBigNumber;
  InS1FromB: TCnBigNumber; InS2FromB: TCnBigNumber; OutSignature: TCnSM2Signature;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同签名，A 第五步根据 OutRandKA 随机值与 B 第四步的签出的中间值 S1 S2 与原始 R，
   生成最终签名，返回该步签名是否成功。InRandKA 是第一步中的 OutRandKA，InRFromC 来源于上上步的 OutRToBA。
   InS1FromB 来源于上一步的 OutS1ToA，InS2FromB 来源于上一步的 OutS2ToA，最终签名值在 OutSignature 中。

   参数：
     InRandKA: TCnBigNumber                               - 多方协同模式下由 A 方在第一步中生成的中间随机值 K
     InRFromC: TCnBigNumber                               - 多方协同模式下由 C 方或最后一方生成并传输而来的中间随机值 R
     InS1FromB: TCnBigNumber                              - 多方协同模式下由 B 方或上一方生成并传输而来的中间签名值 S1
     InS2FromB: TCnBigNumber                              - 多方协同模式下由 B 方或上一方生成并传输而来的中间签名值 S2
     OutSignature: TCnSM2Signature                        - 输出的最终签名值
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 多方协同模式下 A 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回签名是否成功
}

// =========== SM2 椭圆曲线三方或更多方互相信任的简易协同解密算法 ==============
{
  原理较签名简单多了，A B C 各自用自身的私钥分量叠加乘一个点，C 乘完后返回给 A 解密即可，无需再次过 B
}
function CnSM2Collaborative3DecryptAStep1(EnData: Pointer; DataByteLen: Integer;
  OutTToB: TCnEccPoint; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同解密，A 第一步根据密文解出中间值 T，发送给 B，返回该步解密是否成功。

   参数：
     EnData: Pointer                                      - 待解密的密文数据块地址
     DataByteLen: Integer                                 - 待解密的密文数据块字节长度
     OutTToB: TCnEccPoint                                 - 多方协同模式下 A 方生成的中间结果坐标点 T，需要传输给 B 方
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 多方协同模式下 A 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回计算是否成功
}

function CnSM2Collaborative3DecryptBStep1(InTFromA: TCnEccPoint; OutTToC: TCnEccPoint;
  PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的三方协同解密，B 第二步根据 A 发来的中间值 T 算出自己的中间值 T，发送给 C，返回该步解密是否成功。

   参数：
     InTFromA: TCnEccPoint                                - 多方协同模式下由 A 方生成并传输而来的中间结果坐标点 T
     OutTToC: TCnEccPoint                                 - 多方协同模式下 B 方生成的中间结果坐标点 T，需要传输给 C 或下一方
     PrivateKeyB: TCnSM2CollaborativePrivateKey           - 多方协同模式下 B 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回计算是否成功
}

function CnSM2Collaborative3DecryptCStep1(InTFromB: TCnEccPoint; OutTToA: TCnEccPoint;
  PrivateKeyC: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的双方协同解密，C 第三步根据 B 解出的中间值 T，生成最终值 T 发送回 A，（注意不用过 B 了），
   返回该步解密是否成功。

   参数：
     InTFromB: TCnEccPoint                                - 多方协同模式下由 B 方或上一方生成并传输而来的中间结果坐标点 T
     OutTToA: TCnEccPoint                                 - 多方协同模式下 C 方生成的中间结果坐标点 T，需要传输给 A
     PrivateKeyC: TCnSM2CollaborativePrivateKey           - 多方协同模式下 C 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空

   返回值：Boolean                                        - 返回计算是否成功
}

function CnSM2Collaborative3DecryptAStep2(EnData: Pointer; DataByteLen: Integer;
  InTFromC: TCnEccPoint; OutStream: TStream; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
{* 基于 SM2 椭圆曲线的双方协同解密，A 第四步根据 C 解出的中间值 T 算出最终解密结果写入 Stream，
   返回该步最终解密是否成功。注意密文与 SequenceType 须保持和 AStep1 中的完全一致。

   参数：
     EnData: Pointer                                      - 待解密的密文数据块地址
     DataByteLen: Integer                                 - 待解密的密文数据块字节长度
     InTFromC: TCnEccPoint                                - 多方协同模式下由 C 方生成并传输而来的中间结果坐标点 T
     OutStream: TStream                                   - 输出的明文流
     PrivateKeyA: TCnSM2CollaborativePrivateKey           - 多方协同模式下 A 方的 SM2 私钥分量
     SM2: TCnSM2                                          - 可以传入 SM2 实例，默认为空
     SequenceType: TCnSM2CryptSequenceType                - 内部拼接顺序，默认国标的 C1C3C2，需和密文的实际情况一致

   返回值：Boolean                                        - 返回解密是否成功
}

implementation

uses
  CnKDF, CnBerUtils;

const
  CN_SM2_DEF_UID: array[0..15] of Byte =
    ($31, $32, $33, $34, $35, $36, $37, $38, $31, $32, $33, $34, $35, $36, $37, $38);

var
  FLocalSM2Generator: TCnEccPoint = nil;     // SM2 的 G 点供比较用
  FSM2AffineGPower2KList: TObjectList = nil; // SM2 的 G 点的预计算坐标，第 n 个表示 2^n 次方倍点
  FSM2AffinePreMatrix: TCnEcc3Matrix = nil;  // SM2 的 G 点的 2^4 固定基预计算坐标，第 Row 行第 Col 列的值是 Col * (2^4)^Row 倍点

{* X <= 2^W + (x and (2^W - 1) 表示把 x 的第 W 位置 1，第 W + 1 及以上全塞 0
   简而言之就是取 X 的低 W 位并保证再一位的第 W 位是 1，位从 0 开始数
  其中 W 是 N 的 BitsCount 的一半少点儿，该函数用于密钥交换
  注意：它和 CnECC 中的同名函数功能不同}
procedure BuildShortXValue(X: TCnBigNumber; Order: TCnBigNumber);
var
  I, W: Integer;
begin
  W := (Order.GetBitsCount + 1) div 2 - 1;
  BigNumberSetBit(X, W);
  for I := W + 1 to X.GetBitsCount - 1 do
    BigNumberClearBit(X, I);
end;

{ TCnSM2PublicKey }

procedure TCnSM2PublicKey.SetHex(const Buf: AnsiString);
var
  SM2: TCnSM2;
begin
  SM2 := TCnSM2.Create;
  try
    inherited SetHex(Buf, SM2);
  finally
    SM2.Free;
  end;
end;

{ TCnSM2 }

procedure TCnSM2.AffineMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point);
var
  I, C, Row, Col: Integer;
  E, R: TCnEcc3Point;
  IsG: Boolean;
  M: TCnBigNumber;
  Naf: TShortInts;
begin
  if BigNumberIsNegative(K) then
  begin
    // BigNumberSetNegative(K, False);
    AffinePointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    Point.X.SetZero;
    Point.Y.SetZero;
    Point.Z.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  // 须判断是否标准 SM2 曲线的 G 点，而不是本曲线的 G 点
  IsG := Point.Z.IsOne and BigNumberEqual(Point.X, FLocalSM2Generator.X) and
    BigNumberEqual(Point.Y, FLocalSM2Generator.Y);

  R := nil;
  E := nil;
  M := nil;
  Naf := nil;

  try
    R := TCnEcc3Point.Create;
    E := TCnEcc3Point.Create;

    E.X := Point.X;
    E.Y := Point.Y;
    E.Z := Point.Z;

    C := BigNumberGetBitsCount(K);
    if IsG then // 注意，以下查表乘法优化必须保证本曲线是 SM2 标准曲线
    begin
      // 判断是 G 点的话，可以查表减少乘法与加法次数
      if C <= BitsCount then
      begin
        // 小于 256 的乘数，直接固定基查表加，最多 64 次加法
        Row := 0;

        M := TCnBigNumber.Create;
        BigNumberCopy(M, K);

        while not M.IsZero do
        begin
          Col := BigNumberAndWordTo(M, $000F); // 留下最低四位
          AffinePointAddPoint(R, FSM2AffinePreMatrix[Row, Col], R);
          // 第几块，块内几，定位到矩阵元素，累加

          BigNumberShiftRight(M, M, 4);
          Inc(Row);
        end;
      end
      else // 大于 256 的，按每个 2 次幂查表点加
      begin
        for I := 0 to C - 1 do
        begin
          if BigNumberIsBitSet(K, I) then
            AffinePointAddPoint(R, E, R);

          // P 是 G 点，无需点加，直接取出
          if I < FSM2AffineGPower2KList.Count - 1 then
            E.Assign(TCnEcc3Point(FSM2AffineGPower2KList[I + 1]))
          else if I < C - 1 then // 如果此次没有预置点，则 E 自加，最后一轮不用自加
            AffinePointAddPoint(E, E, E);
        end;
      end;
    end
    else // 不是 G 点，常规加（验证签名时常用，改用 NAF 加速，大概少六分之一的点加法）
    begin
      // R 初始为 0，E 是原始点
      Naf := BigNumberNonAdjanceFormWidth(K);
      for I := High(Naf) downto Low(Naf) do
      begin
        AffinePointAddPoint(R, R, R);
        if Naf[I] = 1 then
          AffinePointAddPoint(R, E, R)
        else if Naf[I] = -1 then
          AffinePointSubPoint(R, E, R)
      end;

//      原始点乘平均一半，改用 NAF 缩小到大概 1/3
//      for I := 0 to C - 1 do
//      begin
//        if BigNumberIsBitSet(K, I) then
//          AffinePointAddPoint(R, E, R);
//
//        if I < C - 1 then // 最后一轮不用自加
//          AffinePointAddPoint(E, E, E);
//      end;
    end;

    Point.X := R.X;
    Point.Y := R.Y;
    Point.Z := R.Z;
  finally
    SetLength(Naf, 0);
    M.Free;
    E.Free;
    R.Free;
  end;
end;

constructor TCnSM2.Create;
begin
  inherited;
  Load(ctSM2);
end;

function CnSM2GenerateKeys(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    SM2.GenerateKeys(PrivateKey, PublicKey);
    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CheckKeys(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil): Boolean;
var
  SM2IsNil: Boolean;
  Pub: TCnSM2PublicKey;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;
  Pub := nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Pub := TCnSM2PublicKey.Create;
    Pub.Assign(SM2.Generator);
    SM2.MultiplePoint(PrivateKey, Pub);

    Result := CnEccPointsEqual(Pub, PublicKey);
    _CnSetLastError(ECN_SM2_OK);
  finally
    Pub.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  传入明文 M，长 MLen 字节，随机生成 k，计算

  C1 = k * G => (x1, y1)         // 非压缩存储，长度为两个数字位长加 1，在 SM2 中也就是 32 * 2 + 1 = 65 字节

  k * PublicKey => (x2, y2)
  t <= KDF(x2‖y2, MLen)
  C2 <= M xor t                  // 长度 MLen

  C3 <= SM3(x2‖M‖y2)           // 长度 32 字节

  密文为：C1‖C3‖C2             // 总长 MLen + 97 字节
}
function CnSM2EncryptData(PlainData: Pointer; DataByteLen: Integer; OutStream:
  TStream; PublicKey: TCnSM2PublicKey; SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType;
  IncludePrefixByte: Boolean; const RandHex: string): Boolean;
var
  Py, P1, P2: TCnEccPoint;
  K: TCnBigNumber;
  B: Byte;
  M: PAnsiChar;
  I: Integer;
  Buf, T, KDFB: TBytes;
  C3H: AnsiString;
  Sm3Dig: TCnSM3Digest;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PlainData = nil) or (DataByteLen <= 0) or (OutStream = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  Py := nil;
  P1 := nil;
  P2 := nil;
  K := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    K := TCnBigNumber.Create;

    // 确保公钥 X Y 均存在
    if PublicKey.Y.IsZero then
    begin
      Py := TCnEccPoint.Create;
      if not SM2.PlainToPoint(PublicKey.X, Py) then
        Exit;
      BigNumberCopy(PublicKey.Y, Py.Y);
    end;

    // 使用指定 K， 或生成一个随机 K
    if RandHex <> '' then
      K.SetHex(AnsiString(RandHex))
    else
    begin
      if not BigNumberRandRange(K, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;
    end;

    P1 := TCnEccPoint.Create;
    P1.Assign(SM2.Generator);
    SM2.MultiplePoint(K, P1);  // 计算出 K * G 得到 X1 Y1

    OutStream.Position := 0;
    if IncludePrefixByte then
    begin
      B := 4;
      OutStream.Write(B, 1);
    end;

    SetLength(Buf, CN_SM2_FINITEFIELD_BYTESIZE);
    P1.X.ToBinary(@Buf[0], CN_SM2_FINITEFIELD_BYTESIZE);
    OutStream.Write(Buf[0], CN_SM2_FINITEFIELD_BYTESIZE);
    SetLength(Buf, CN_SM2_FINITEFIELD_BYTESIZE);
    P1.Y.ToBinary(@Buf[0], CN_SM2_FINITEFIELD_BYTESIZE);
    OutStream.Write(Buf[0], CN_SM2_FINITEFIELD_BYTESIZE);    // 拼成 C1

    P2 := TCnEccPoint.Create;
    P2.Assign(PublicKey);
    SM2.MultiplePoint(K, P2); // 计算出 K * PublicKey 得到 X2 Y2

    SetLength(KDFB, CN_SM2_FINITEFIELD_BYTESIZE * 2);
    P2.X.ToBinary(@KDFB[0], CN_SM2_FINITEFIELD_BYTESIZE);
    P2.Y.ToBinary(@KDFB[CN_SM2_FINITEFIELD_BYTESIZE], CN_SM2_FINITEFIELD_BYTESIZE);
    T := CnSM2KDFBytes(KDFB, DataByteLen);

    M := PAnsiChar(PlainData);
    for I := 1 to DataByteLen do
      T[I - 1] := Byte(T[I - 1]) xor Byte(M[I - 1]);         // T 里是 C2，但先不能写

    SetLength(C3H, CN_SM2_FINITEFIELD_BYTESIZE * 2 + DataByteLen);
    P2.X.ToBinary(@C3H[1], CN_SM2_FINITEFIELD_BYTESIZE);
    Move(M[0], C3H[CN_SM2_FINITEFIELD_BYTESIZE + 1], DataByteLen);
    P2.Y.ToBinary(@C3H[CN_SM2_FINITEFIELD_BYTESIZE + DataByteLen + 1], CN_SM2_FINITEFIELD_BYTESIZE); // 拼成算 C3 的
    Sm3Dig := SM3(@C3H[1], Length(C3H));                     // 算出 C3

    if SequenceType = cstC1C3C2 then
    begin
      OutStream.Write(Sm3Dig[0], SizeOf(TCnSM3Digest));      // 写入 C3
      OutStream.Write(T[0], DataByteLen);                    // 写入 C2
    end
    else
    begin
      OutStream.Write(T[0], DataByteLen);                    // 写入 C2
      OutStream.Write(Sm3Dig[0], SizeOf(TCnSM3Digest));      // 写入 C3
    end;

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    P2.Free;
    P1.Free;
    Py.Free;
    K.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2EncryptData(PlainData: TBytes; PublicKey: TCnSM2PublicKey; SM2: TCnSM2;
  SequenceType: TCnSM2CryptSequenceType; IncludePrefixByte: Boolean; const RandHex: string): TBytes;
var
  Stream: TMemoryStream;
begin
  Result := nil;
  Stream := TMemoryStream.Create;
  try
    if CnSM2EncryptData(@PlainData[0], Length(PlainData), Stream, PublicKey, SM2,
      SequenceType, IncludePrefixByte, RandHex) then
    begin
      SetLength(Result, Stream.Size);
      Move(Stream.Memory^, Result[0], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

{
  MLen <= DataLen - SM3DigLength - 2 * Sm2ByteLength - 1，劈开拿到 C1 C2 C3

  PrivateKey * C1 => (x2, y2)

  t <= KDF(x2‖y2, Mlen)

  M' <= C2 xor t

  还可对比 SM3(x2‖M‖y2) Hash 是否与 C3 相等
}
function CnSM2DecryptData(EnData: Pointer; DataByteLen: Integer; OutStream: TStream;
  PrivateKey: TCnSM2PrivateKey; SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  MP: AnsiString;
  KDFB, T: TBytes;
  C3H: AnsiString;
  SM2IsNil: Boolean;
  P2: TCnEccPoint;
  I, PrefixLen: Integer;
  Sm3Dig: TCnSM3Digest;
begin
  Result := False;
  if (EnData = nil) or (DataByteLen <= 0) or (OutStream = nil) or (PrivateKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  P2 := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    MLen := DataByteLen - CN_SM2_MIN_ENCRYPT_BYTESIZE;
    if MLen <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    P2 := TCnEccPoint.Create;
    M := PAnsiChar(EnData);
    if M^ = #$04 then  // 跳过可能的前导字节 $04
    begin
      Dec(MLen);
      if MLen <= 0 then
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
        Exit;
      end;

      PrefixLen := 1;
      Inc(M);
    end
    else
      PrefixLen := 0;

    // 读出 C1
    P2.X.SetBinary(M, CN_SM2_FINITEFIELD_BYTESIZE);
    Inc(M, CN_SM2_FINITEFIELD_BYTESIZE);
    P2.Y.SetBinary(M, CN_SM2_FINITEFIELD_BYTESIZE);
    if P2.IsZero then
    begin
      _CnSetLastError(ECN_SM2_DECRYPT_INFINITE_ERROR);
      Exit;
    end;

    SM2.MultiplePoint(PrivateKey, P2);

    SetLength(KDFB, CN_SM2_FINITEFIELD_BYTESIZE * 2);
    P2.X.ToBinary(@KDFB[0], CN_SM2_FINITEFIELD_BYTESIZE);
    P2.Y.ToBinary(@KDFB[CN_SM2_FINITEFIELD_BYTESIZE], CN_SM2_FINITEFIELD_BYTESIZE);
    T := CnSM2KDFBytes(KDFB, MLen);

    if SequenceType = cstC1C3C2 then
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, SizeOf(TCnSM3Digest) + CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen); // 跳过 C3 指向 C2
      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I - 1]));  // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, CN_SM2_FINITEFIELD_BYTESIZE * 2 + MLen);
      P2.X.ToBinary(@C3H[1], CN_SM2_FINITEFIELD_BYTESIZE);
      Move(MP[1], C3H[CN_SM2_FINITEFIELD_BYTESIZE + 1], MLen);
      P2.Y.ToBinary(@C3H[CN_SM2_FINITEFIELD_BYTESIZE + MLen + 1], CN_SM2_FINITEFIELD_BYTESIZE);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                     // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen);     // M 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TCnSM3Digest)) then  // 比对杂凑值是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end
    else // C1C2C3 的排列
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen);     // 指向 C2

      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I - 1]));  // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, CN_SM2_FINITEFIELD_BYTESIZE * 2 + MLen);
      P2.X.ToBinary(@C3H[1], CN_SM2_FINITEFIELD_BYTESIZE);
      Move(MP[1], C3H[CN_SM2_FINITEFIELD_BYTESIZE + 1], MLen);
      P2.Y.ToBinary(@C3H[CN_SM2_FINITEFIELD_BYTESIZE + MLen + 1], CN_SM2_FINITEFIELD_BYTESIZE);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                     // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen + MLen);      // 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TCnSM3Digest)) then  // 比对杂凑值是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end;
  finally
    P2.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2DecryptData(EnData: TBytes; PrivateKey: TCnSM2PrivateKey;
  SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType): TBytes;
var
  Stream: TMemoryStream;
begin
  Result := nil;
  Stream := TMemoryStream.Create;
  try
    if CnSM2DecryptData(@EnData[0], Length(EnData), Stream, PrivateKey, SM2,
      SequenceType) then
    begin
      SetLength(Result, Stream.Size);
      Move(Stream.Memory^, Result[0], Stream.Size);
    end;
  finally
    Stream.Free;
  end;
end;

function CnSM2EncryptFile(const InFile, OutFile: string; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType; IncludePrefixByte: Boolean;
  const RandHex: string): Boolean;
var
  Stream: TMemoryStream;
  F: TFileStream;
begin
  Stream := nil;
  F := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFile);

    F := TFileStream.Create(OutFile, fmCreate);
    Result := CnSM2EncryptData(Stream.Memory, Stream.Size, F, PublicKey, SM2,
      SequenceType, IncludePrefixByte, RandHex);
  finally
    F.Free;
    Stream.Free;
  end;
end;

function CnSM2DecryptFile(const InFile, OutFile: string; PrivateKey: TCnSM2PrivateKey;
  SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType): Boolean;
var
  Stream: TMemoryStream;
  F: TFileStream;
begin
  Stream := nil;
  F := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFile);

    F := TFileStream.Create(OutFile, fmCreate);
    Result := CnSM2DecryptData(Stream.Memory, Stream.Size, F, PrivateKey, SM2, SequenceType);
  finally
    F.Free;
    Stream.Free;
  end;
end;

function CnSM2CryptToAsn1(EnData: TBytes; SM2: TCnSM2;
  SequenceType: TCnSM2CryptSequenceType; IncludePrefixByte: Boolean): TBytes;
var
  P: Pointer;
  MLen: Integer;
  Num: TCnBigNumber;
  Writer: TCnBerWriter;
  Root: TCnBerWriteNode;
begin
  Result := nil;
  MLen := Length(EnData) - CN_SM2_MIN_ENCRYPT_BYTESIZE;
  if MLen <= 0 then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  if IncludePrefixByte then
  begin
    if (MLen <= 1) or (EnData[0] <> 04) then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;
    P := @EnData[1]; // 跳过前导字节 04
    Dec(MLen);
  end
  else
    P := @EnData[0];

  Writer := nil;
  Num := nil;

  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);

    Num := TCnBigNumber.Create;

    // P 已指向 C1，写 C1 中的 X
    Num.SetBinary(P, CN_SM2_FINITEFIELD_BYTESIZE);
    AddBigNumberToWriter(Writer, Num, Root);
    P := Pointer(TCnNativeInt(P) + CN_SM2_FINITEFIELD_BYTESIZE);

    // 写 C1 中的 Y
    Num.SetBinary(P, CN_SM2_FINITEFIELD_BYTESIZE);
    AddBigNumberToWriter(Writer, Num, Root);
    P := Pointer(TCnNativeInt(P) + CN_SM2_FINITEFIELD_BYTESIZE);

    // C1 写完，根据类型处理 C3C2 或 C2C3
    if SequenceType = cstC1C3C2 then
    begin
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, P, SizeOf(TCnSM3Digest)); // 写 C3 校验
      P := Pointer(TCnNativeInt(P) + SizeOf(TCnSM3Digest));
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, P, MLen);                 // 写 C2 密文
    end
    else
    begin
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, P, MLen);                 // 写 C2 密文
      P := Pointer(TCnNativeInt(P) + MLen);
      Writer.AddBasicNode(CN_BER_TAG_OCTET_STRING, P, SizeOf(TCnSM3Digest)); // 写 C3 校验
    end;

    SetLength(Result, Writer.TotalSize);
    Writer.SaveTo(@Result[0]);
  finally
    Num.Free;
    Writer.Free;
  end;
end;

function CnSM2CryptToAsn1(EnStream: TStream; OutStream: TStream; SM2: TCnSM2;
  SequenceType: TCnSM2CryptSequenceType; IncludePrefixByte: Boolean): Boolean;
var
  R: TBytes;
begin
  Result := False;
  R := CnSM2CryptToAsn1(StreamToBytes(EnStream), SM2, SequenceType, IncludePrefixByte);
  if R <> nil then
    Result := BytesToStream(R, OutStream) > 0;
end;

function CnSM2CryptFromAsn1(Asn1Data: TBytes; SM2: TCnSM2;
  SequenceType: TCnSM2CryptSequenceType; IncludePrefixByte: Boolean): TBytes;
var
  Idx: Integer;
  Reader: TCnBerReader;
  X, Y: TCnBigNumber;
begin
  Result := nil;
  if Length(Asn1Data) < CN_SM2_MIN_ENCRYPT_BYTESIZE + 4 then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  Reader := nil;
  X := nil;
  Y := nil;

  try
    Reader := TCnBerReader.Create(@Asn1Data[0], Length(Asn1Data));
    Reader.ParseToTree;

    if Reader.TotalCount <> 5 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    if (Reader.Items[1].BerDataLength > CN_SM2_FINITEFIELD_BYTESIZE + 1)
      or ((Reader.Items[2].BerDataLength > CN_SM2_FINITEFIELD_BYTESIZE + 1)) then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    X := TCnBigNumber.Create;
    PutIndexedBigIntegerToBigNumber(Reader.Items[1], X);
    if X.GetBytesCount > CN_SM2_FINITEFIELD_BYTESIZE then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    Y := TCnBigNumber.Create;
    PutIndexedBigIntegerToBigNumber(Reader.Items[2], Y);
    if Y.GetBytesCount > CN_SM2_FINITEFIELD_BYTESIZE then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    if SequenceType = cstC1C3C2 then
    begin
      if Reader.Items[3].BerDataLength <> SizeOf(TCnSM3Digest) then // 尾巴上的 C3 长度必须是 32 字节
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
       Exit;
      end;
    end
    else
    begin
      if Reader.Items[4].BerDataLength <> SizeOf(TCnSM3Digest) then // 尾巴上的 C3 长度必须是 32 字节
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
       Exit;
      end;
    end;

    Idx := CN_SM2_FINITEFIELD_BYTESIZE * 2 + Reader.Items[3].BerDataLength
      + Reader.Items[4].BerDataLength;
    if IncludePrefixByte then
      Inc(Idx);

    SetLength(Result, Idx);
    Idx := 0;
    if IncludePrefixByte then
    begin
      Result[0] := 04;
      Inc(Idx);
    end;
    X.ToBinary(@Result[Idx], CN_SM2_FINITEFIELD_BYTESIZE);
    Inc(Idx, CN_SM2_FINITEFIELD_BYTESIZE);
    Y.ToBinary(@Result[Idx], CN_SM2_FINITEFIELD_BYTESIZE);

    Inc(Idx, CN_SM2_FINITEFIELD_BYTESIZE);

    // 无论 3 是 C3，4 是 C2，还是 3 是 C2，4 是 C3，都能这么写
    Reader.Items[3].CopyDataTo(@Result[Idx]);
    Inc(Idx, Reader.Items[3].BerDataLength);
    Reader.Items[4].CopyDataTo(@Result[Idx]);
  finally
    Y.Free;
    X.Free;
    Reader.Free;
  end;
end;

function CnSM2CryptFromAsn1(Asn1Stream: TStream; OutStream: TStream; SM2: TCnSM2;
  SequenceType: TCnSM2CryptSequenceType; IncludePrefixByte: Boolean): Boolean;
var
  R: TBytes;
begin
  Result := False;
  R := CnSM2CryptFromAsn1(StreamToBytes(Asn1Stream), SM2, SequenceType, IncludePrefixByte);
  if R <> nil then
    Result := BytesToStream(R, OutStream) > 0;
end;

// 计算 Za 值也就是 Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
// 其中 EntLen 是 UserID 的位长度（也就是字节长度 * 8）的网络顺序字节表示
function CalcSM2UserHash(const UserID: AnsiString; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2): TCnSM3Digest;
var
  Stream: TMemoryStream;
  Len: Integer;
  ULen: Word;
begin
  Stream := TMemoryStream.Create;
  try
    if UserID <> '' then
    begin
      Len := Length(UserID) * 8;
      ULen := UInt16HostToNetwork(Len); // 转成网络字节顺序

      Stream.Write(ULen, SizeOf(ULen));
      if ULen > 0 then
        Stream.Write(UserID[1], Length(UserID));
    end
    else // UserID 为空时按规范使用字符串 1234567812345678
    begin
      Len := SizeOf(CN_SM2_DEF_UID) * 8;
      ULen := UInt16HostToNetwork(Len); // 转成网络字节顺序

      Stream.Write(ULen, SizeOf(ULen));
      if ULen > 0 then
        Stream.Write(CN_SM2_DEF_UID[0], SizeOf(CN_SM2_DEF_UID));
    end;

    BigNumberWriteBinaryToStream(SM2.CoefficientA, Stream);
    BigNumberWriteBinaryToStream(SM2.CoefficientB, Stream);
    BigNumberWriteBinaryToStream(SM2.Generator.X, Stream);
    BigNumberWriteBinaryToStream(SM2.Generator.Y, Stream);
    BigNumberWriteBinaryToStream(PublicKey.X, Stream, SM2.BytesCount);
    BigNumberWriteBinaryToStream(PublicKey.Y, Stream, SM2.BytesCount);

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);  // 算出 ZA
  finally
    Stream.Free;
  end;
end;

// 根据 Za 与数据再次计算杂凑值 e
function CalcSM2SignatureHash(const UserID: AnsiString; PlainData: Pointer; DataByteLen: Integer;
  PublicKey: TCnSM2PublicKey; SM2: TCnSM2): TCnSM3Digest;
var
  Stream: TMemoryStream;
  Sm3Dig: TCnSM3Digest;
begin
  Stream := TMemoryStream.Create;
  try
    Sm3Dig := CalcSM2UserHash(UserID, PublicKey, SM2);
    Stream.Write(Sm3Dig[0], SizeOf(TCnSM3Digest));
    Stream.Write(PlainData^, DataByteLen);

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);  // 再次算出杂凑值 e
  finally
    Stream.Free;
  end;
end;

{
  ZA <= Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
  e <= Hash(ZA‖M)

  k * G => (x1, y1)

  输出签名 r <= (e + x1) mod n

  输出签名 s <= ((1 + PrivateKey)^-1 * (k - r * PrivateKey)) mod n

}
function CnSM2SignData(const UserID: AnsiString; PlainData: Pointer; DataByteLen: Integer;
  OutSignature: TCnSM2Signature; PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2; const RandHex: string): Boolean;
var
  K, R, E: TCnBigNumber;
  P: TCnEccPoint;
  SM2IsNil: Boolean;
  PubIsNil: Boolean;
  HexSet: Boolean;
  Sm3Dig: TCnSM3Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataByteLen <= 0) or (OutSignature = nil) or
    (PrivateKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K := nil;
  P := nil;
  E := nil;
  R := nil;
  SM2IsNil := SM2 = nil;
  PubIsNil := PublicKey = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if PubIsNil then
    begin
      PublicKey := TCnSM2PublicKey.Create;
      PublicKey.Assign(SM2.Generator);
      SM2.MultiplePoint(PrivateKey, PublicKey);
    end;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataByteLen, PublicKey, SM2); // 杂凑值 e

    P := TCnEccPoint.Create;
    E := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    K := TCnBigNumber.Create;
    HexSet := False;

    while True do
    begin
      // 使用指定 K，或生成一个随机 K
      if RandHex <> '' then
      begin
        K.SetHex(AnsiString(RandHex));
        HexSet := True;
      end
      else
      begin
        if not BigNumberRandRange(K, SM2.Order) then
        begin
          _CnSetLastError(ECN_SM2_RANDOM_ERROR);
          Exit;
        end;
      end;

      P.Assign(SM2.Generator);
      SM2.MultiplePoint(K, P);

      // 计算 R = (e + x) mod N
      E.SetBinary(@Sm3Dig[0], SizeOf(TCnSM3Digest));
      BigNumberAdd(E, E, P.X);
      BigNumberMod(R, E, SM2.Order); // 算出 R 后 E 不用了

      if R.IsZero then  // R 不能为 0
      begin
        if HexSet then // 外界使用的随机数不符合要求
        begin
          _CnSetLastError(ECN_SM2_RANDOM_ERROR);
          Exit;
        end;
        Continue;
      end;

      BigNumberAdd(E, R, K);
      if BigNumberCompare(E, SM2.Order) = 0 then // R + K = N 也不行
      begin
        if HexSet then // 外界使用的随机数不符合要求
        begin
          _CnSetLastError(ECN_SM2_RANDOM_ERROR);
          Exit;
        end;
        Continue;
      end;

      BigNumberCopy(OutSignature.R, R);  // 得到一个签名值 R

      BigNumberCopy(E, PrivateKey);
      BigNumberAddWord(E, 1);
      BigNumberModularInverse(R, E, SM2.Order);      // 求逆元得到 (1 + PrivateKey)^-1，放在 R 里

      // 求 K - R * PrivateKey，又用起 E 来
      BigNumberMul(E, OutSignature.R, PrivateKey);
      BigNumberSub(E, K, E);
      BigNumberMul(R, E, R); // (1 + PrivateKey)^-1 * (K - R * PrivateKey) 放在 R 里
      BigNumberNonNegativeMod(OutSignature.S, R, SM2.Order); // 注意余数不能为负

      Result := True;
      _CnSetLastError(ECN_SM2_OK);

      Break;
    end;
  finally
    K.Free;
    P.Free;
    R.Free;
    E.Free;
    if PubIsNil then
      PublicKey.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2SignData(const UserID: AnsiString; PlainData: TBytes;
  OutSignature: TCnSM2Signature; PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2; const RandHex: string): Boolean;
begin
  Result := CnSM2SignData(UserID, @PlainData[0], Length(PlainData), OutSignature,
    PrivateKey, PublicKey, SM2, RandHex);
end;

{
  s 和 r 是签名值
  ZA = Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
  e <= Hash(ZA‖M)

  t <= (r + s) mod n
  P <= s * G + t * PublicKey
  r' <= (e + P.x) mod n
  比对 r' 和 r

  其中，P 的运算得到 k*G
  P = s*G + t*d*G = [s + d(r + s)] *G = ((1+d)*s + dr)*G
  因为 s = (k-rd)/(1+d) 所以上式 = (k - rd + rd) * G = k*G

  该 P 点的 x 值和 e 运算得到 r
}
function CnSM2VerifyData(const UserID: AnsiString; PlainData: Pointer; DataByteLen: Integer;
  InSignature: TCnSM2Signature; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): Boolean;
var
  K, R, E: TCnBigNumber;
  P, Q: TCnEccPoint;
  SM2IsNil: Boolean;
  Sm3Dig: TCnSM3Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataByteLen <= 0) or (InSignature = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K := nil;
  P := nil;
  Q := nil;
  E := nil;
  R := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if BigNumberCompare(InSignature.R, SM2.Order) >= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    if BigNumberCompare(InSignature.S, SM2.Order) >= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataByteLen, PublicKey, SM2); // 杂凑值 e

    P := TCnEccPoint.Create;
    Q := TCnEccPoint.Create;
    E := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    K := TCnBigNumber.Create;

    BigNumberAdd(K, InSignature.R, InSignature.S);
    BigNumberNonNegativeMod(R, K, SM2.Order);
    if R.IsZero then  // (r + s) mod n = 0 则失败，这里 R 是文中的 T
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    P.Assign(SM2.Generator);
    SM2.MultiplePoint(InSignature.S, P);
    Q.Assign(PublicKey);
    SM2.MultiplePoint(R, Q);
    SM2.PointAddPoint(P, Q, P);   // s * G + t * PublicKey => P

    E.SetBinary(@Sm3Dig[0], SizeOf(TCnSM3Digest));
    BigNumberAdd(E, E, P.X);

    BigNumberNonNegativeMod(R, E, SM2.Order);

    Result := BigNumberCompare(R, InSignature.R) = 0;
    _CnSetLastError(ECN_SM2_OK); // 正常进行校验，即使校验不通过也清空错误码
  finally
    K.Free;
    P.Free;
    Q.Free;
    R.Free;
    E.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2VerifyData(const UserID: AnsiString; PlainData: TBytes;
  InSignature: TCnSM2Signature; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): Boolean;
begin
  Result := CnSM2VerifyData(UserID, @PlainData[0], Length(PlainData), InSignature, PublicKey, SM2);
end;

function CnSM2SignFile(const UserID: AnsiString; const FileName: string;
  PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): string;
var
  OutSign: TCnSM2Signature;
  Stream: TMemoryStream;
begin
  Result := '';
  if not FileExists(FileName) then
  begin
    _CnSetLastError(ECN_FILE_NOT_FOUND);
    Exit;
  end;

  OutSign := nil;
  Stream := nil;

  try
    OutSign := TCnSM2Signature.Create;
    Stream := TMemoryStream.Create;

    Stream.LoadFromFile(FileName);
    if CnSM2SignData(UserID, Stream.Memory, Stream.Size, OutSign, PrivateKey, PublicKey, SM2) then
      Result := OutSign.ToHex(SM2.BytesCount);
  finally
    Stream.Free;
    OutSign.Free;
  end;
end;

function CnSM2VerifyFile(const UserID: AnsiString; const FileName: string;
  const InHexSignature: string; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): Boolean;
var
  InSign: TCnSM2Signature;
  Stream: TMemoryStream;
begin
  Result := False;
  if not FileExists(FileName) then
  begin
    _CnSetLastError(ECN_FILE_NOT_FOUND);
    Exit;
  end;

  InSign := nil;
  Stream := nil;

  try
    InSign := TCnSM2Signature.Create;
    InSign.SetHex(AnsiString(InHexSignature));

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    Result := CnSM2VerifyData(UserID, Stream.Memory, Stream.Size, InSign, PublicKey, SM2);
  finally
    Stream.Free;
    InSign.Free;
  end;
end;

{
  计算交换出的密钥：KDF(Xuv‖Yuv‖Za‖Zb, kLen)
}
function CalcSM2ExchangeKey(UV: TCnEccPoint; Za, Zb: TCnSM3Digest; KeyByteLength: Integer): TBytes;
var
  Stream: TMemoryStream;
  S: TBytes;
begin
  Stream := TMemoryStream.Create;
  try
    BigNumberWriteBinaryToStream(UV.X, Stream);
    BigNumberWriteBinaryToStream(UV.Y, Stream);
    Stream.Write(Za[0], SizeOf(TCnSM3Digest));
    Stream.Write(Zb[0], SizeOf(TCnSM3Digest));

    SetLength(S, Stream.Size);
    Stream.Position := 0;
    Stream.Read(S[0], Stream.Size);

    Result := CnSM2KDFBytes(S, KeyByteLength);
  finally
    SetLength(S, 0);
    Stream.Free;
  end;
end;

{
  Hash(0x02‖Yuv‖Hash(Xuv‖Za‖Zb‖X1‖Y1‖X2‖Y2))
       0x03
}
function CalcSM2OptionalSig(UV, P1, P2: TCnEccPoint; Za, Zb: TCnSM3Digest; Step2or3: Boolean): TCnSM3Digest;
var
  Stream: TMemoryStream;
  Sm3Dig: TCnSM3Digest;
  B: Byte;
begin
  if Step2or3 then
    B := 2
  else
    B := 3;

  Stream := TMemoryStream.Create;
  try
    BigNumberWriteBinaryToStream(UV.X, Stream);
    Stream.Write(Za[0], SizeOf(TCnSM3Digest));
    Stream.Write(Zb[0], SizeOf(TCnSM3Digest));
    BigNumberWriteBinaryToStream(P1.X, Stream);
    BigNumberWriteBinaryToStream(P1.Y, Stream);
    BigNumberWriteBinaryToStream(P2.X, Stream);
    BigNumberWriteBinaryToStream(P2.Y, Stream);
    Sm3Dig := SM3(PAnsiChar(Stream.Memory), Stream.Size);

    Stream.Clear;
    Stream.Write(B, 1);
    BigNumberWriteBinaryToStream(UV.Y, Stream);
    Stream.Write(Sm3Dig[0], SizeOf(TCnSM3Digest));

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);
  finally
    Stream.Free;
  end;
end;

{
  随机值 rA * G => RA 传给 B
}
function CnSM2KeyExchangeAStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey;
  OutARand: TCnBigNumber; OutRA: TCnEccPoint; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (KeyByteLength <= 0) or (APrivateKey = nil) or (APublicKey = nil) or (OutRA = nil)
    or (OutARand = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;
  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(OutARand, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;

    OutRA.Assign(SM2.Generator);
    SM2.MultiplePoint(OutARand, OutRA);
    Result := True;
  finally
    if SM2IsNil then
      SM2.Free;
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
  BPrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey; InRA: TCnEccPoint;
  out OutKeyB: TBytes; OutRB: TCnEccPoint; out OutOptionalSB: TCnSM3Digest;
  out OutOptionalS2: TCnSM3Digest; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
  R, X, T: TCnBigNumber;
  V: TCnEccPoint;
  Za, Zb: TCnSM3Digest;
begin
  Result := False;
  if (KeyByteLength <= 0) or (BPrivateKey = nil) or (APublicKey = nil) or
    (BPublicKey = nil) or (InRA = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;
  R := nil;
  X := nil;
  T := nil;
  V := nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not SM2.IsPointOnCurve(InRA) then // 验证传过来的 RA 是否满足方程
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    R := TCnBigNumber.Create;
    if not BigNumberRandRange(R, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;

    OutRB.Assign(SM2.Generator);
    SM2.MultiplePoint(R, OutRB);

    X := TCnBigNumber.Create;
    BigNumberCopy(X, OutRB.X);

    // 2^W 次方表示第 W 位 1（位从 0 开始算） ，2^W - 1 则表示 0 位到 W - 1 位全置 1
    // X2 = 2^W + (x2 and (2^W - 1) 表示把 x2 的第 W 位置 1，W + 1 以上全塞 0，x2 是 RB.X
    BuildShortXValue(X, SM2.Order);

    BigNumberMul(X, R, X);
    BigNumberAdd(X, X, BPrivateKey);

    T := TCnBigNumber.Create;
    BigNumberNonNegativeMod(T, X, SM2.Order); // T = (BPrivateKey + 随机值 * X2) mod N

    BigNumberCopy(X, InRA.X);
    BuildShortXValue(X, SM2.Order);

    // 计算 XV YV。 (h * t) * (APublicKey + X * RA)
    V := TCnEccPoint.Create;
    V.Assign(InRA);
    SM2.MultiplePoint(X, V);
    SM2.PointAddPoint(V, APublicKey, V);
    SM2.MultiplePoint(T, V);

    if V.X.IsZero or V.Y.IsZero then // 如果是无穷远点则协商失败
    begin
      _CnSetLastError(ECN_SM2_KEYEXCHANGE_INFINITE_ERROR);
      Exit;
    end;

    // 协商初步成功，计算 KB
    Za := CalcSM2UserHash(AUserID, APublicKey, SM2);
    Zb := CalcSM2UserHash(BUserID, BPublicKey, SM2);
    OutKeyB := CalcSM2ExchangeKey(V, Za, Zb, KeyByteLength); // 共享密钥协商成功！

    // 然后计算 SB 供 A 核对
    OutOptionalSB := CalcSM2OptionalSig(V, InRA, OutRB, Za, Zb, True);

    // 顺便计算 S2 等 A 发来 SA 时核对
    OutOptionalS2 := CalcSM2OptionalSig(V, InRA, OutRB, Za, Zb, False);
    Result := True;
  finally
    V.Free;
    T.Free;
    X.Free;
    R.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2KeyExchangeAStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey; MyRA, InRB: TCnEccPoint;
  MyARand: TCnBigNumber; out OutKeyA: TBytes; InOptionalSB: TCnSM3Digest;
  out OutOptionalSA: TCnSM3Digest; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
  X, T: TCnBigNumber;
  U: TCnEccPoint;
  Za, Zb: TCnSM3Digest;
begin
  Result := False;
  if (KeyByteLength <= 0) or (APrivateKey = nil) or (APublicKey = nil) or
    (BPublicKey = nil) or (MyRA = nil) or (InRB = nil) or (MyARand = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;
  X := nil;
  T := nil;
  U := nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not SM2.IsPointOnCurve(InRB) then // 验证传过来的 RB 是否满足方程
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    X := TCnBigNumber.Create;
    BigNumberCopy(X, MyRA.X);
    BuildShortXValue(X, SM2.Order);     // 从 RA 里整出 X1

    BigNumberMul(X, MyARand, X);
    BigNumberAdd(X, X, APrivateKey);

    T := TCnBigNumber.Create;
    BigNumberNonNegativeMod(T, X, SM2.Order); // T = (APrivateKey + 随机值 * X1) mod N

    BigNumberCopy(X, InRB.X);
    BuildShortXValue(X, SM2.Order);

    // 计算 XU YU。 (h * t) * (BPublicKey + X * RB)
    U := TCnEccPoint.Create;
    U.Assign(InRB);
    SM2.MultiplePoint(X, U);
    SM2.PointAddPoint(U, BPublicKey, U);
    SM2.MultiplePoint(T, U);

    if U.X.IsZero or U.Y.IsZero then // 如果是无穷远点则协商失败
    begin
      _CnSetLastError(ECN_SM2_KEYEXCHANGE_INFINITE_ERROR);
      Exit;
    end;

    // 协商初步成功，计算 KA
    Za := CalcSM2UserHash(AUserID, APublicKey, SM2);
    Zb := CalcSM2UserHash(BUserID, BPublicKey, SM2);
    OutKeyA := CalcSM2ExchangeKey(U, Za, Zb, KeyByteLength); // 共享密钥协商成功！

    // 然后计算 SB 核对
    OutOptionalSA := CalcSM2OptionalSig(U, MyRA, InRB, Za, Zb, True);
    if not CompareMem(@OutOptionalSA[0], @InOptionalSB[0], SizeOf(TCnSM3Digest)) then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    // 然后计算 SA 供 B 核对
    OutOptionalSA := CalcSM2OptionalSig(U, MyRA, InRB, Za, Zb, False);
    Result := True;
  finally
    U.Free;
    T.Free;
    X.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2KeyExchangeBStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey;
  InOptionalSA: TCnSM3Digest; MyOptionalS2: TCnSM3Digest; SM2: TCnSM2): Boolean;
begin
  Result := CompareMem(@InOptionalSA[0], @MyOptionalS2[0], SizeOf(TCnSM3Digest));
end;

{
  随机取 r
  点 R <= r * G
  数 c <= Hash(PublicKey, R)
  数 z <= r + c * PrivateKey
}
function CnSM2SchnorrProve(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  OutR: TCnEccPoint; OutZ: TCnBigNumber; SM2: TCnSM2): Boolean;
var
  R: TCnBigNumber;
  Dig: TCnSM3Digest;
  SM2IsNil: Boolean;
  Stream: TMemoryStream;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) or (OutR = nil) or (OutZ = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  R := nil;
  Stream := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    R := TCnBigNumber.Create;
    if not BigNumberRandBytes(R, CN_SM2_FINITEFIELD_BYTESIZE) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;

    OutR.Assign(SM2.Generator);
    SM2.MultiplePoint(R, OutR);

    Stream := TMemoryStream.Create;
    if CnEccPointToStream(PublicKey, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    if CnEccPointToStream(OutR, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    Dig := SM3(Stream.Memory, Stream.Size);

    OutZ.SetBinary(@Dig[0], SizeOf(TCnSM3Digest));

    // 注意，此处无需也不能 mod P！
    BigNumberMul(OutZ, OutZ, PrivateKey);
    BigNumberAdd(OutZ, OutZ, R);

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    Stream.Free;
    R.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  数 c <= Hash(PublicKey, R)
  点 z * G ?= R + c * PublicKey
}
function CnSM2SchnorrCheck(PublicKey: TCnSM2PublicKey; InR: TCnEccPoint;
  InZ: TCnBigNumber; SM2: TCnSM2): Boolean;
var
  C: TCnBigNumber;
  Dig: TCnSM3Digest;
  SM2IsNil: Boolean;
  Stream: TMemoryStream;
  P1, P2: TCnEccPoint;
begin
  Result := False;
  if (PublicKey = nil) or (InR = nil) or (InZ = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  Stream := nil;
  C := nil;
  P1 := nil;
  P2 := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Stream := TMemoryStream.Create;
    if CnEccPointToStream(PublicKey, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    if CnEccPointToStream(InR, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    Dig := SM3(Stream.Memory, Stream.Size);

    C := TCnBigNumber.Create;
    C.SetBinary(@Dig[0], SizeOf(TCnSM3Digest));

    P1 := TCnEccPoint.Create;
    P1.Assign(SM2.Generator);
    SM2.MultiplePoint(InZ, P1);

    P2 := TCnEccPoint.Create;
    P2.Assign(PublicKey);
    SM2.MultiplePoint(C, P2);
    SM2.PointAddPoint(P2, InR, P2);

    Result := CnEccPointsEqual(P1, P2);
    _CnSetLastError(ECN_SM2_OK);
  finally
    P2.Free;
    P1.Free;
    C.Free;
    Stream.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

// ========== SM2 椭圆曲线双方互相信任的简易协同算法之协同密钥生成 =============

function CnSM2CollaborativeGenerateKeyAStep1(PrivateKeyA: TCnSM2CollaborativePrivateKey;
  OutPointToB: TCnEccPoint; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyA = nil) or (OutPointToB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(PrivateKeyA, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if PrivateKeyA.IsZero then // 万一真拿到 0，就加 1
      PrivateKeyA.SetOne;

    OutPointToB.Assign(SM2.Generator);
    SM2.MultiplePoint(PrivateKeyA, OutPointToB); // 基点乘 PrivateKeyA 次给 B

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeGenerateKeyBStep1(PrivateKeyB: TCnSM2CollaborativePrivateKey;
  InPointFromA: TCnEccPoint; PublicKey: TCnSM2CollaborativePublicKey; SM2: TCnSM2): Boolean;
var
  P: TCnEccPoint;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InPointFromA = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  P := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(PrivateKeyB, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if PrivateKeyB.IsZero then // 万一真拿到 0，就加 1
      PrivateKeyB.SetOne;

    PublicKey.Assign(InPointFromA);
    SM2.MultiplePoint(PrivateKeyB, PublicKey); // 得到的 PublicKey 还要减 G

    P := TCnEccPoint.Create;
    P.Assign(SM2.Generator);
    SM2.PointInverse(P);
    SM2.PointAddPoint(PublicKey, P, PublicKey);

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    P.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

// =============== SM2 椭圆曲线双方互相信任的简易协同签名算法 ==================
{
  A 生成随机数 ka，并计算点 ka*G 给 B，也把杂凑值 e 给 B
}
function CnSM2CollaborativeSignAStep1(const UserID: AnsiString; PlainData: Pointer;
  DataByteLen: Integer; OutHashEToB: TCnBigNumber; OutQToB: TCnEccPoint; OutRandKA: TCnBigNumber;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): Boolean;
var
  Sm3Dig: TCnSM3Digest;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyA = nil) or (OutHashEToB = nil) or (OutQToB = nil) or
    (OutRandKA = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataByteLen, PublicKey, SM2); // 杂凑值 e 要给 B
    OutHashEToB.SetBinary(@Sm3Dig[0], SizeOf(TCnSM3Digest));

    if not BigNumberRandRange(OutRandKA, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if OutRandKA.IsZero then               // 万一真拿到 0，就加 1
      OutRandKA.SetOne;

    OutQToB.Assign(SM2.Generator);
    SM2.MultiplePoint(OutRandKA, OutQToB); // K 要留着给 A 签名的下一步，注意这里没有使用 PrivateKeyA

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  B 生成随机数 k2 去乘以 ka*G，并再生成随机数 k1，算出重要点 (ka*k2+k1)*G，该点的横坐标 r
  再算出 S1 = k2/dB    S2 = (k1+r)/dB
}
function CnSM2CollaborativeSignBStep1(InHashEFromA: TCnBigNumber; InQFromA: TCnEccPoint;
  OutRToA, OutS1ToA, OutS2ToA: TCnBigNumber; PrivateKeyB: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2): Boolean;
var
  K1, K2, Inv: TCnBigNumber;
  P, Q: TCnEccPoint;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InHashEFromA = nil) or (InQFromA = nil)
    or (OutRToA = nil) or (OutS1ToA = nil) or (OutS2ToA = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K1 := nil;
  K2 := nil;
  Q := nil;
  P := nil;
  Inv := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    K1 := TCnBigNumber.Create;
    K2 := TCnBigNumber.Create;
    Q := TCnEccPoint.Create;
    P := TCnEccPoint.Create;
    Inv := TCnBigNumber.Create;

    while True do
    begin
      if not BigNumberRandRange(K1, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;
      if K1.IsZero then // 万一真拿到 0，就加 1
        K1.SetOne;

      Q.Assign(SM2.Generator);
      SM2.MultiplePoint(K1, Q); // 先计算出一个自己的 Q 点

      // 再生成一次随机 K
      if not BigNumberRandRange(K2, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;
      if K2.IsZero then // 万一真拿到 0，就加 1
        K2.SetOne;

      P.Assign(InQFromA);
      SM2.MultiplePoint(K2, P);   // 对方的 Q 点乘以自己的 K
      SM2.PointAddPoint(P, Q, Q); // 再加上自己的 Q

      // r = (Q.x + e) mod N
      BigNumberAddMod(OutRToA, Q.X, InHashEFromA, SM2.Order);

      if OutRToA.IsZero then                               // 注意到这为止 PrivateKeyB 未起作用
        Continue;

      BigNumberModularInverse(Inv, PrivateKeyB, SM2.Order);
      BigNumberDirectMulMod(OutS1ToA, Inv, K2, SM2.Order); // 算出 s1 = k2 / PrivateKeyB
      BigNumberAddMod(K1, K1, OutRToA, SM2.Order);         // K1 + r
      BigNumberDirectMulMod(OutS2ToA, K1, Inv, SM2.Order); // K1 + r / PrivateKeyB

      Result := True;
      _CnSetLastError(ECN_SM2_OK);

      Break;
    end;
  finally
    Inv.Free;
    P.Free;
    Q.Free;
    K2.Free;
    K1.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;
{
  A 拿到 B 传回来的 S1 S2 后算出 S = (ka*k2/dA*dB) + T -r
  其中临时变量 T = S2/dA

           (ka*k2 + k1) + (dA*dB - 1)*r
  得到 S = -----------------------------  为简化起见，令 k = (ka*k2 + k1)
                      dA*dB

  验证时同样计算 P = s*G + (r+s)*PublicKey = s+(r+s)(dA*dB-1)*G
  其中 r+s = (k-r)(dA*dB-1)/dAdB （展开消去了 r*dA*dB），代入化简
  最后展开得到 P = (k *dA*dB/dA*dB)*G = k*G，成功！
}
function CnSM2CollaborativeSignAStep2(InRandKA, InRFromB, InS1FromB, InS2FromB: TCnBigNumber;
  OutSignature: TCnSM2Signature; PrivateKeyA: TCnSM2CollaborativePrivateKey; SM2: TCnSM2): Boolean;
var
  Inv, T: TCnBigNumber;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyA = nil) or (OutSignature = nil) or
    (InRFromB = nil) or (InS1FromB = nil) or (InS2FromB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  Inv := nil;
  T := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Inv := TCnBigNumber.Create;
    BigNumberModularInverse(Inv, PrivateKeyA, SM2.Order);

    T := TCnBigNumber.Create;
    BigNumberDirectMulMod(T, Inv, InS2FromB, SM2.Order); // T := S2 / PrivateKeyA
    BigNumberDirectMulMod(OutSignature.S, InRandKA, Inv, SM2.Order); // Ka / PrivateKeyA
    BigNumberDirectMulMod(OutSignature.S, OutSignature.S, InS1FromB, SM2.Order); // K * S1 / PrivateKeyA

    BigNumberAddMod(OutSignature.S, OutSignature.S, T, SM2.Order);
    BigNumberSubMod(OutSignature.S, OutSignature.S, InRFromB, SM2.Order);

    if not OutSignature.S.IsZero then
    begin
      BigNumberAdd(T, OutSignature.S, InRFromB);

      if not BigNumberEqual(T, SM2.Order) then
      begin
        if BigNumberCopy(OutSignature.R, InRFromB) = nil then
        begin
          _CnSetLastError(ECN_SM2_BIGNUMBER_ERROR);
          Exit;
        end;
      end;

      Result := True;
      _CnSetLastError(ECN_SM2_OK);
    end;
  finally
    T.Free;
    Inv.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

// =============== SM2 椭圆曲线双方互相信任的简易协同解密算法 ==================

function CnSM2CollaborativeDecryptAStep1(EnData: Pointer; DataByteLen: Integer;
  OutTToB: TCnEccPoint; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (EnData = nil) or (DataByteLen <= 0) or (PrivateKeyA = nil)
    or (OutTToB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    MLen := DataByteLen - CN_SM2_MIN_ENCRYPT_BYTESIZE;
    if MLen <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    M := PAnsiChar(EnData);
    if M^ = #$04 then  // 跳过可能的前导字节 $04
    begin
      Dec(MLen);
      if MLen <= 0 then
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
        Exit;
      end;

      Inc(M);
    end;

    // 读出 C1
    OutTToB.X.SetBinary(M, SM2.BitsCount div 8);
    Inc(M, SM2.BitsCount div 8);
    OutTToB.Y.SetBinary(M, SM2.BitsCount div 8);
    if OutTToB.IsZero then
    begin
      _CnSetLastError(ECN_SM2_DECRYPT_INFINITE_ERROR);
      Exit;
    end;

    SM2.MultiplePoint(PrivateKeyA, OutTToB); // C1 点乘私钥发给 B

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeDecryptBStep1(InTFromA: TCnEccPoint; OutTToA: TCnEccPoint;
  PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InTFromA = nil) or (OutTToA = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    OutTToA.Assign(InTFromA);
    SM2.MultiplePoint(PrivateKeyB, OutTToA);

     Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeDecryptAStep2(EnData: Pointer; DataByteLen: Integer;
  InTFromB: TCnEccPoint; OutStream: TStream; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  MP: AnsiString;
  KDFB, T: TBytes;
  C3H: AnsiString;
  P2: TCnEccPoint;
  I, PrefixLen: Integer;
  Sm3Dig: TCnSM3Digest;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (EnData = nil) or (DataByteLen <= 0) or (PrivateKeyA = nil)
    or (InTFromB = nil) or (OutStream = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    MLen := DataByteLen - CN_SM2_MIN_ENCRYPT_BYTESIZE;
    if MLen <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    P2 := TCnEccPoint.Create;
    M := PAnsiChar(EnData);
    if M^ = #$04 then  // 跳过可能的前导字节 $04
    begin
      Dec(MLen);
      if MLen <= 0 then
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
        Exit;
      end;

      PrefixLen := 1;
      Inc(M);
    end
    else
      PrefixLen := 0;

    // 读出 C1
    P2.X.SetBinary(M, SM2.BitsCount div 8);
    Inc(M, SM2.BitsCount div 8);
    P2.Y.SetBinary(M, SM2.BitsCount div 8);
    if P2.IsZero then
    begin
      _CnSetLastError(ECN_SM2_DECRYPT_INFINITE_ERROR);
      Exit;
    end;

    // P2 <= InTFromB - C1
    SM2.PointSubPoint(InTFromB, P2, P2);

    // 以下同常规解密

    SetLength(KDFB, CN_SM2_FINITEFIELD_BYTESIZE * 2);
    P2.X.ToBinary(@KDFB[0], CN_SM2_FINITEFIELD_BYTESIZE);
    P2.Y.ToBinary(@KDFB[CN_SM2_FINITEFIELD_BYTESIZE], CN_SM2_FINITEFIELD_BYTESIZE);
    T := CnSM2KDFBytes(KDFB, MLen);

    if SequenceType = cstC1C3C2 then
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, SizeOf(TCnSM3Digest) + CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen); // 跳过 C3 指向 C2
      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I - 1]));    // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, CN_SM2_FINITEFIELD_BYTESIZE * 2 + MLen);
      P2.X.ToBinary(@C3H[1], CN_SM2_FINITEFIELD_BYTESIZE);
      Move(MP[1], C3H[CN_SM2_FINITEFIELD_BYTESIZE + 1], MLen);
      P2.Y.ToBinary(@C3H[CN_SM2_FINITEFIELD_BYTESIZE + MLen + 1], CN_SM2_FINITEFIELD_BYTESIZE);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen);             // M 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TCnSM3Digest)) then  // 比对杂凑值是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end
    else // C1C2C3 的排列
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen);             // 指向 C2

      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I - 1]));    // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, CN_SM2_FINITEFIELD_BYTESIZE * 2 + MLen);
      P2.X.ToBinary(@C3H[1], CN_SM2_FINITEFIELD_BYTESIZE);
      Move(MP[1], C3H[CN_SM2_FINITEFIELD_BYTESIZE + 1], MLen);
      P2.Y.ToBinary(@C3H[CN_SM2_FINITEFIELD_BYTESIZE + MLen + 1], CN_SM2_FINITEFIELD_BYTESIZE);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, CN_SM2_FINITEFIELD_BYTESIZE * 2 + PrefixLen + MLen);      // 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TCnSM3Digest)) then  // 比对杂凑值是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end;
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

procedure CheckPrePoints;
const
  M_WIDTH = 4;
var
  SM2: TCnSM2;
  P, Q: TCnEcc3Point;
  R, C, I: Integer;
  MRows, MCols: Integer;
begin
  if FSM2AffineGPower2KList.Count > 0 then
    Exit;

  FLocalSM2Generator := TCnEccPoint.Create;
  SM2 := TCnSM2.Create;
  try
    FLocalSM2Generator.Assign(SM2.Generator);

    // 创建预计算的 2^n 列表
    P := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(SM2.Generator, P);

    FSM2AffineGPower2KList.Add(P);      // 第 0 个是 2 的 0 次方也就是 1 倍就是自身
    for I := 1 to 255 do
    begin
      Q := TCnEcc3Point.Create;
      SM2.AffinePointAddPoint(P, P, Q); // Q 变成 2P
      FSM2AffineGPower2KList.Add(Q);    // 加入列表
      P.Assign(Q);                      // P 变成 2P 准备下次循环
    end;

    // 创建预计算的固定基矩阵
    if FSM2AffinePreMatrix <> nil then
      Exit;

    MRows := SM2.BitsCount div M_WIDTH;
    MCols := 1 shl M_WIDTH;

    FSM2AffinePreMatrix := TCnEcc3Matrix.Create(MRows, MCols);
    CnEccPointToEcc3Point(SM2.Generator, P); // P 拿到射影 G
    FSM2AffinePreMatrix.ValueObject[0, 0].SetZero;

    // 算第 0 行的倍点
    for C := 0 to MCols - 2 do
      SM2.AffinePointAddPoint(FSM2AffinePreMatrix.ValueObject[0, C], P,
        FSM2AffinePreMatrix.ValueObject[0, C + 1]);

    for R := 1 to MRows - 1 do
    begin
      for C := 0 to MCols - 1 do
      begin
        SM2.AffinePointAddPoint(FSM2AffinePreMatrix.ValueObject[R - 1, C],
          FSM2AffinePreMatrix.ValueObject[R - 1, C], FSM2AffinePreMatrix.ValueObject[R, C]);
        for I := 1 to M_WIDTH - 1 do
          SM2.AffinePointAddPoint(FSM2AffinePreMatrix.ValueObject[R, C],
            FSM2AffinePreMatrix.ValueObject[R, C], FSM2AffinePreMatrix.ValueObject[R, C]);
          // 自加二次 = 乘以 4，自加四次 = 乘以 16
      end;
    end;
  finally
    SM2.Free;
  end;
end;

// ======== SM2 椭圆曲线三方或更多方互相信任的简易协同算法之协同密钥生成 =======
{
  dA * G => B
}
function CnSM2Collaborative3GenerateKeyAStep1(PrivateKeyA: TCnSM2CollaborativePrivateKey;
  OutPointToB: TCnEccPoint; SM2: TCnSM2): Boolean;
begin
  Result := CnSM2CollaborativeGenerateKeyAStep1(PrivateKeyA, OutPointToB, SM2);
end;

{
  dA * dB * G => C
}
function CnSM2Collaborative3GenerateKeyBStep1(PrivateKeyB: TCnSM2CollaborativePrivateKey;
  InPointFromA: TCnEccPoint; OutPointToC: TCnEccPoint; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (OutPointToC = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(PrivateKeyB, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if PrivateKeyB.IsZero then // 万一真拿到 0，就加 1
      PrivateKeyB.SetOne;

    OutPointToC.Assign(InPointFromA);
    SM2.MultiplePoint(PrivateKeyB, OutPointToC); // A 的点乘 PrivateKeyB 次给 C

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  (dA * dB * dC - 1) * G
}
function CnSM2Collaborative3GenerateKeyCStep1(PrivateKeyC: TCnSM2CollaborativePrivateKey;
  InPointFromB: TCnEccPoint; PublicKey: TCnSM2CollaborativePublicKey; SM2: TCnSM2): Boolean;
begin
  Result := CnSM2CollaborativeGenerateKeyBStep1(PrivateKeyC, InPointFromB, PublicKey, SM2); // 有减一操作
end;

{
  ka * G => B
  e => B
}
function CnSM2Collaborative3SignAStep1(const UserID: AnsiString; PlainData: Pointer;
  DataByteLen: Integer; OutHashEToBC: TCnBigNumber; OutQToB: TCnEccPoint; OutRandKA: TCnBigNumber;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean;
begin
  Result := CnSM2CollaborativeSignAStep1(UserID, PlainData, DataByteLen, OutHashEToBC,
    OutQToB, OutRandKA, PrivateKeyA, PublicKey, SM2);
end;

{
  kb * ka * G => C
  e => C
}
function CnSM2Collaborative3SignBStep1(InHashEFromA: TCnBigNumber; InQFromA: TCnEccPoint;
  OutQToC: TCnEccPoint; OutRandKB: TCnBigNumber; PrivateKeyB: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InHashEFromA = nil) or (InQFromA = nil)
    or (OutQToC = nil) or (OutRandKB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(OutRandKB, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if OutRandKB.IsZero then                // 万一真拿到 0，就加 1
      OutRandKB.SetOne;

    // Kb * Qa => Qb
    OutQToC.Assign(InQFromA);
    SM2.MultiplePoint(OutRandKB, OutQToC);   // 又计算出一个自己的 Q 点

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  Q = kc * kb * ka * G + k1 * G，其 x 坐标 + e => r，r 相对比较固定地传递给 A 和 B
  S1 = kc / dC         => B
  S2 = (k1 + r) / dC   => B
}
function CnSM2Collaborative3SignCStep1(InHashEFromA: TCnBigNumber; InQFromB: TCnEccPoint;
  OutRToBA, OutS1ToB, OutS2ToB: TCnBigNumber; PrivateKeyC: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
var
  SM2IsNil: Boolean;
  Inv, K1, RandKC: TCnBigNumber;
  P, Q: TCnEccPoint;
begin
  Result := False;
  if (PrivateKeyC = nil) or (InHashEFromA = nil) or (InQFromB = nil)  then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K1 := nil;
  RandKC := nil;
  P := nil;
  Q := nil;
  Inv := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    K1 := TCnBigNumber.Create;
    RandKC := TCnBigNumber.Create;
    P := TCnEccPoint.Create;
    Q := TCnEccPoint.Create;
    Inv := TCnBigNumber.Create;

    while True do
    begin
      if not BigNumberRandRange(K1, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;
      if K1.IsZero then // 万一真拿到 0，就加 1
        K1.SetOne;

      Q.Assign(SM2.Generator);
      SM2.MultiplePoint(K1, Q); // 先计算出一个自己的 Q 点

      // 再生成一次随机 K
      if not BigNumberRandRange(RandKC, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;
      if RandKC.IsZero then // 万一真拿到 0，就加 1
        RandKC.SetOne;

      P.Assign(InQFromB);
      SM2.MultiplePoint(RandKC, P);   // 对方的 Q 点乘以自己的 RandKC，这里 KC 和 A B 中的 RandKA RandKB 地位等同
      SM2.PointAddPoint(P, Q, Q); // 再加上自己的 Q，得到重要点 Q

      // r = (Q.x + e) mod N
      BigNumberAddMod(OutRToBA, Q.X, InHashEFromA, SM2.Order);

      if OutRToBA.IsZero then                               // 注意到这为止 PrivateKeyC 未起作用
        Continue;

      BigNumberModularInverse(Inv, PrivateKeyC, SM2.Order);
      BigNumberDirectMulMod(OutS1ToB, Inv, RandKC, SM2.Order); // 算出 S1 = RandKC / PrivateKeyC
      BigNumberAddMod(K1, K1, OutRToBA, SM2.Order);            // K1 + r
      BigNumberDirectMulMod(OutS2ToB, K1, Inv, SM2.Order);     // 算出 S2 = K1 + r / PrivateKeyC

      Result := True;
      _CnSetLastError(ECN_SM2_OK);

      Break;
    end;
  finally
    Inv.Free;
    Q.Free;
    P.Free;
    RandKC.Free;
    K1.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  S1 = (kc * kb) / (dC * dB)  => A
  S2 = (k1 + r) / (dC * dB)   => A
}
function CnSM2Collaborative3SignBStep2(InRandKB, InRFromC, InS1FromC, InS2FromC: TCnBigNumber;
  OutS1ToA, OutS2ToA: TCnBigNumber; PrivateKeyB: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
var
  SM2IsNil: Boolean;
  Inv, K2: TCnBigNumber;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InRandKB = nil) or (InRFromC = nil) or (InS1FromC = nil)
     or (InS2FromC = nil) or (OutS1ToA = nil) or (OutS2ToA = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K2 := nil;
  Inv := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    K2 := TCnBigNumber.Create;
    Inv := TCnBigNumber.Create;

    // S1 = S1 * Kb / dB mod N
    BigNumberModularInverse(Inv, PrivateKeyB, SM2.Order);           // 得到 PrivateKeyB^-1
    BigNumberDirectMulMod(OutS1ToA, InS1FromC, Inv, SM2.Order);     // S1c / PrivateKeyB
    BigNumberDirectMulMod(OutS1ToA, OutS1ToA, InRandKB, SM2.Order); // (Kb * S1c) / PrivateKeyB

    // S2 := S2 / dB
    BigNumberDirectMulMod(OutS2ToA, InS2FromC, Inv, SM2.Order);     // S2 / PrivateKeyB

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    Inv.Free;
    K2.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  S1 = (kc * kb * ka) / (dC * dB * dA)
  S2 = (k1 + r) / (dC * dB * dA)

  S = S1 + S2 - r

  验证 S 的过程类似于 P 的计算化简，
  为简化，令 k = (k1 + ka*kb*kc)  令 d = dA*dB*dC
  因为 P = [s +(r+s)(d-1)]*G
  其中 s = (k+r-dr)/d   得到 s+r = (k+r)/d
  最后 P 点展开得到单纯的 k*G = (k1 + ka*kb*kc)*G
}
function CnSM2Collaborative3SignAStep2(InRandKA, InRFromC, InS1FromB, InS2FromB: TCnBigNumber;
  OutSignature: TCnSM2Signature; PrivateKeyA: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
var
  SM2IsNil: Boolean;
  Inv, S1, S2: TCnBigNumber;
begin
  Result := False;
  if (PrivateKeyA = nil) or (InRandKA = nil) or (InRFromC = nil) or (InS1FromB = nil)
    or (InS2FromB = nil) or (OutSignature = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  S1 := nil;
  S2 := nil;
  Inv := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    S1 := TCnBigNumber.Create;
    S2 := TCnBigNumber.Create;
    Inv := TCnBigNumber.Create;

    // S1 = S1 * Ka / dA mod N
    BigNumberModularInverse(Inv, PrivateKeyA, SM2.Order);     // 得到 PrivateKeyA^-1
    BigNumberDirectMulMod(S1, InS1FromB, Inv, SM2.Order);     // S1b / PrivateKeyA
    BigNumberDirectMulMod(S1, S1, InRandKA, SM2.Order);       // (Ka * S1b) / PrivateKeyA

    // S2 := S2 / dA
    BigNumberDirectMulMod(S2, InS2FromB, Inv, SM2.Order);     // S2b / PrivateKeyB

    // S := S1 + S2 - R
    BigNumberAddMod(OutSignature.S, S1, S2, SM2.Order);
    BigNumberSubMod(OutSignature.S, OutSignature.S, InRFromC, SM2.Order);

    BigNumberCopy(OutSignature.R, InRFromC);                  // R S 为最终签名
    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    Inv.Free;
    S2.Free;
    S1.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

// =========== SM2 椭圆曲线三方或更多方互相信任的简易协同解密算法 ==============

function CnSM2Collaborative3DecryptAStep1(EnData: Pointer; DataByteLen: Integer;
  OutTToB: TCnEccPoint; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
begin
  Result := CnSM2CollaborativeDecryptAStep1(EnData, DataByteLen, OutTToB, PrivateKeyA, SM2);
end;

function CnSM2Collaborative3DecryptBStep1(InTFromA: TCnEccPoint; OutTToC: TCnEccPoint;
  PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
begin
  Result := CnSM2CollaborativeDecryptBStep1(InTFromA, OutTToC, PrivateKeyB, SM2);
end;

function CnSM2Collaborative3DecryptCStep1(InTFromB: TCnEccPoint; OutTToA: TCnEccPoint;
  PrivateKeyC: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
begin
  Result := CnSM2CollaborativeDecryptBStep1(InTFromB, OutTToA, PrivateKeyC, SM2);
end;

function CnSM2Collaborative3DecryptAStep2(EnData: Pointer; DataByteLen: Integer;
  InTFromC: TCnEccPoint; OutStream: TStream; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
begin
  Result := CnSM2CollaborativeDecryptAStep2(EnData, DataByteLen, InTFromC, OutStream,
    PrivateKeyA, SM2, SequenceType);
end;

procedure InitSM2;
begin
  FSM2AffineGPower2KList := TObjectList.Create(True);
  CheckPrePoints;
end;

procedure FintSM2;
begin
  FLocalSM2Generator.Free;
  FSM2AffinePreMatrix.Free;
  FSM2AffineGPower2KList.Free;
end;

initialization
  InitSM2;

finalization
  FintSM2;

end.

