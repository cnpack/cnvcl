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

unit CnMLKEM;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：基于模块化格的密钥封装机制 MLKEM 实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 NIST 的 FIPS 203 规范中的 MLKEM
*           （Module-Lattice-based Key-Encapsulation Mechanism、基于模块化格的密钥封装机制）算法。
*
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2025.11.16 V1.0
*               创建单元，从 CnLattice 中独立而来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs,
  CnNative, CnVector, CnBigNumber, CnPolynomial, CnRandom, CnBits, CnSHA3;

const
  CN_MLKEM_KEY_SIZE    = 32;
  {* MLKEM 的共享密钥及种子等的长度}

  CN_MLKEM_POLY_SIZE   = 256;
  {* MLKEM 的多项式尺寸，最高次数是 255 次}

  CN_MLKEM_PRIME       = 3329;
  {* MLKEM 使用的素数}

  CN_MLKEM_PRIME_INV   = 3303;
  {* MLKEM 使用的 128 对该素数的模逆元}

type
  ECnMLKEMException = class(Exception);
  {* MLKEM 相关异常}

  TCnMLKEMType = (cmkt512, cmkt768, cmkt1024);
  {* MLKEM 的三种实现规范}

  TCnMLKEMSeed = array[0..CN_MLKEM_KEY_SIZE - 1] of Byte;
  {* MLKEM 种子，32 字节}

  TCnMLKEMBlock = array[0..CN_MLKEM_KEY_SIZE - 1] of Byte;
  {* MLKEM 块数据，32 字节}

  TCnMLKEMPolynomial = array[0..CN_MLKEM_POLY_SIZE - 1] of Word;
  {* MLKEM 多项式系数，256 个双字，用来表达一个多项式}

  TCnMLKEMPolyVector = array of TCnMLKEMPolynomial;
  {* MLKEM 多项式列表或叫向量，用来表达 S 或 E 等}

  TCnMLKEMPolyMatrix = array of TCnMLKEMPolyVector;
  {* MLKEM 多项式矩阵，用来表达 A}

  TCnMLKEMDecapsulationKey = class
  {* MLKEM 的非公开解封密钥，包括秘密多项式向量与隐式拒绝的随机种子}
  public
    SecretVector: TCnMLKEMPolyVector;
    {* 秘密多项式向量，相当于规范里的 S，系数已 NTT 化}
    InjectionSeed: TCnMLKEMSeed;
    {* 用于隐式拒绝的随机种子，不参与密钥生成，相当于规范里的 Z}
    EnKeyHash: TCnMLKEMSeed;
    {* 对应公开密钥的杂凑值} 
  end;

  TCnMLKEMEncapsulationKey = class
  {* MLKEM 的封装密钥，可公开，包括可用来生成矩阵的种子，以及公钥多项式向量}
  public
    GenerationSeed: TCnMLKEMSeed;
    {* 用于生成整套密钥的主随机种子，相当于规范里的 D}
    PubVector: TCnMLKEMPolyVector;
    {* 生成的公开多项式向量，相当于规范里的 T，系数已 NTT 化}
  end;

  TCnMLKEM = class
  {* 基于模块化格的密钥封装机制（Module-Lattice-based Key Encapsulation Mechanism）实现类}
  private
    FMatrixSize: Integer;
    FNoise1: Integer;
    FNoise2: Integer;
    FCompressDigits: Integer;
    FCompressU: Integer;
    FCompressV: Integer;
    FMLKEMType: TCnMLKEMType;
    function GetCipherUPolyByteLength: Integer;
    function GetCipherUByteLength: Integer;
    function GetCipherVByteLength: Integer;

    procedure GenerateMatrix(const Seed: TCnMLKEMSeed; out Matrix: TCnMLKEMPolyMatrix);
    {* 根据种子生成矩阵 A，系数是 NTT 形式}
    procedure TransposeMatrix(const InMatrix: TCnMLKEMPolyMatrix; out Matrix: TCnMLKEMPolyMatrix);
    {* 生成矩阵 A 的转置矩阵}

    procedure SamplePolynomial(const Seed: TCnMLKEMSeed; Noise: Integer;
      var Counter: Integer; out Polynomial: TCnMLKEMPolynomial; UseNTT: Boolean = True);
    {* 采样生成一个多项式，UseNTT 控制内容是否 NTT 化}
    procedure SampleVector(const Seed: TCnMLKEMSeed; Noise: Integer;
      var Counter: Integer; out PolyVector: TCnMLKEMPolyVector; UseNTT: Boolean = True);
    {* 采样生成一个多项式向量，UseNTT 控制内容是否 NTT 化}
  protected
    procedure KPKEKeyGen(const D: TCnMLKEMSeed; out GenerationSeed: TCnMLKEMSeed;
      out Secret, Pub: TCnMLKEMPolyVector);
    {* 核心生成方法，D 是外部传入的真随机数}

    procedure KPKEEncrypt(EncapKey: TCnMLKEMEncapsulationKey; const Msg: TCnMLKEMBlock;
      const Seed: TCnMLKEMSeed; out UVector: TCnMLKEMPolyVector; out VPolynomial: TCnMLKEMPolynomial);
    {* 核心加密方法，使用公开密钥与 32 位真随机种子，加密 32 位消息，返回密文对应的多项式向量与多项式，均是非 NTT 形式}

    procedure KPKEDecrypt(DecapKey: TCnMLKEMDecapsulationKey; const UVector: TCnMLKEMPolyVector;
      const VPolynomial: TCnMLKEMPolynomial; out Msg: TCnMLKEMBlock);
    {* 核心解密方法，使用非 NTT 形式的密文对应的多项式向量与多项式，还原 32 位消息}

    procedure ExtractUVFromCipherText(const CipherText: TBytes; out UVector: TCnMLKEMPolyVector;
      out VPolynomial: TCnMLKEMPolynomial);
    {* 从密文 CipherText 中解出密文对应的多项式向量与多项式，均是非 NTT 形式}
  public
    constructor Create(AType: TCnMLKEMType); virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure GenerateKeys(EncapKey: TCnMLKEMEncapsulationKey; DecapKey: TCnMLKEMDecapsulationKey;
      const RandDHex: string = ''; const RandZHex: string = ''); overload;
    {* 用两个真随机 32 字节种子，生成一对 Key，随机数允许外部传入 64 字符的十六进制字符串}

    procedure GenerateKeys(out EnKey: TBytes; out DeKey: TBytes;
      const RandDHex: string = ''; const RandZHex: string = ''); overload;
    {* 用两个真随机 32 字节种子，生成一对 Key 的字节数组，随机数允许外部传入 64 字符的十六进制字符串}

    procedure LoadKeyFromBytes(Key: TBytes; EncapKey: TCnMLKEMEncapsulationKey);
    {* 从字节数组中加载公开密钥，失败则抛异常}
    procedure LoadKeysFromBytes(Key: TBytes; DecapKey: TCnMLKEMDecapsulationKey;
      EncapKey: TCnMLKEMEncapsulationKey);
    {* 从字节数组中加载非公开密钥与公开密钥，失败则抛异常}

    function SaveEncapKeyToBytes(EncapKey: TCnMLKEMEncapsulationKey): TBytes;
    {* 将公开密钥保存成字节数组 EK}
    function SaveKeysToBytes(DecapKey: TCnMLKEMDecapsulationKey; EncapKey: TCnMLKEMEncapsulationKey): TBytes;
    {* 将非公开密钥与公开密钥都保存成字节数组 DK，注意 DecapKey 无法单独保存}

    function MLKEMEncrypt(EnKey: TBytes; Msg: TBytes; const RandHex: string = ''): TBytes;
    {* 用公开密钥流加密消息，返回加密密文。
       要求消息长 32 字节，少补 0 多则截断。随机数允许外部传入 64 字符的十六进制字符串}

    function MLKEMDecrypt(DeKey: TBytes; CipherText: TBytes): TBytes;
    {* 用非公开密钥流解密消息，返回解密后的明文}

    procedure MLKEMEncaps(EnKey: TBytes; Msg: TBytes;
      out ShareKey: TBytes; out CipherText: TBytes);
    {* 用公开密钥流封装共享密钥，返回共享密钥与密文。随机数允许外部传入 64 字符的十六进制字符串}

    function MLKEMDecaps(DeKey: TBytes; CipherText: TBytes): TBytes;
    {* 用非公开密钥解封密文，返回共享密钥。如失败，返回随机密钥}

    function GetEncapKeyByteLength: Integer;
    {* 返回公开密钥的字节长度}
    function GetDecapKeyByteLength: Integer;
    {* 返回非公开密钥的字节长度}
    function GetCipherByteLength: Integer;
    {* 返回密钥的字节长度}

    procedure CheckEncapKey(EnKey: TBytes);
    {* 检查公开密钥字节数组是否合法，不合法则抛异常}
    procedure CheckDecapKey(DeKey: TBytes);
    {* 检查非公开密钥字节数组是否合法，不合法则抛异常}
    procedure CheckKeyPair(EncapKey: TCnMLKEMEncapsulationKey; DecapKey: TCnMLKEMDecapsulationKey);
    {* 检查一对 Key 是否匹配}

    property MLKEMType: TCnMLKEMType read FMLKEMType;
    {* MLKEM 的算法类型，512、768、1024 三种}
    property MatrixSize: Integer read FMatrixSize write FMatrixSize;
    {* 矩阵的尺寸，在这里是方阵尺寸，取值 2 或 3 或 4，对应规范中的 k}
    property Noise1: Integer read FNoise1 write FNoise1;
    {* 噪声参数一，控制生成密钥时秘密向量和错误向量的采样范围，对应规范中的 Eta1}
    property Noise2: Integer read FNoise2 write FNoise2;
    {* 噪声参数二，控制封装时的随机向量与两个错误向量的采样范围，对应规范中的 Eta2}
    property CompressDigits: Integer read FCompressDigits write FCompressDigits;
    {* D 的压缩位数}
    property CompressU: Integer read FCompressU write FCompressU;
    {* U 的压缩位数}
    property CompressV: Integer read FCompressV write FCompressV;
    {* V 的压缩位数}
  end;

// ============================ MLKEM 工具函数 =================================

procedure MLKEMPolynomialToInt64Polynomial(const Src: TCnMLKEMPolynomial; Dst: TCnInt64Polynomial);
{* 将 MKLEM 格式的多项式转换为一元整系数多项式对象。

   参数：
     Src: TCnMLKEMPolynomial              - 待转换的 MKLEM 格式的多项式
     Dst: TCnInt64Polynomial              - 目标一元整系数多项式对象

   返回值：（无）
}

procedure Int64PolynomialToMLKEMPolynomial(const Src: TCnInt64Polynomial; var Dst: TCnMLKEMPolynomial);
{* 将一元整系数多项式对象转换为 MKLEM 格式的多项式。

   参数：
     Src: TCnInt64Polynomial              - 待转换的一元整系数多项式对象
     Dst: TCnMLKEMPolynomial              - 目标 MKLEM 格式的多项式

   返回值：（无）
}

procedure MLKEMPolynomialToINTT(var Res: TCnMLKEMPolynomial; const P: TCnMLKEMPolynomial);
{* 将 NTT 系数的 MKLEM 格式的多项式转换为非 NTT 系数的，俩参数可以相同。

   参数：
     Res: TCnMLKEMPolynomial              - 非 NTT 系数的 MKLEM 格式的多项式结果
     P: TCnMLKEMPolynomial                - 待转换的 NTT 系数的 MKLEM 格式的多项式

   返回值：（无）
}

procedure MLKEMPolynomialToNTT(var Res: TCnMLKEMPolynomial; const P: TCnMLKEMPolynomial);
{* 将非 NTT 系数的 MKLEM 格式的多项式转换为 NTT 系数的，俩参数可以相同。

   参数：
     Res: TCnMLKEMPolynomial              - NTT 系数的 MKLEM 格式的多项式结果
     P: TCnMLKEMPolynomial                - 待转换的非 NTT 系数的 MKLEM 格式的多项式

   返回值：（无）
}

procedure MLKEMPolynomialAdd(var Res: TCnMLKEMPolynomial;
  const P1: TCnMLKEMPolynomial; const P2: TCnMLKEMPolynomial);
{* 两个 MKLEM 格式的多项式在 mod 3329 有限域中相加，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLKEMPolynomial              - MKLEM 格式的多项式和
     P1: TCnMLKEMPolynomial               - MKLEM 格式的多项式加数一
     P2: TCnMLKEMPolynomial               - MKLEM 格式的多项式加数二

   返回值：（无）
}

procedure MLKEMPolynomialSub(var Res: TCnMLKEMPolynomial;
  const P1: TCnMLKEMPolynomial; const P2: TCnMLKEMPolynomial);
{* 两个 MKLEM 格式的多项式在 mod 3329 有限域中相减，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLKEMPolynomial              - MKLEM 格式的多项式差
     P1: TCnMLKEMPolynomial               - MKLEM 格式的多项式被减数
     P2: TCnMLKEMPolynomial               - MKLEM 格式的多项式减数

   返回值：（无）
}

procedure MLKEMPolynomialMul(var Res: TCnMLKEMPolynomial; const P1, P2: TCnMLKEMPolynomial;
  IsNTT: Boolean = True);
{* 两个 MKLEM 格式的多项式在 mod 3329 及 x^256 + 1 的多项式环上相乘。
   IsNTT 指示参数是否是 NTT 模式，是则执行 NTT 乘法，不是则执行普通乘法。

   参数：
     Res: TCnMLKEMPolynomial              - MKLEM 格式的多项式积
     P1: TCnMLKEMPolynomial               - MKLEM 格式的多项式乘数一
     P2: TCnMLKEMPolynomial               - MKLEM 格式的多项式乘数二
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

   返回值：（无）
}

procedure MLKEMVectorToNTT(var Res: TCnMLKEMPolyVector; const V: TCnMLKEMPolyVector);
{* 将非 NTT 系数的 MKLEM 格式的多项式向量就地转换为 NTT 系数的。

   参数：
     var Res: TCnMLKEMPolyVector          - MKLEM 格式的多项式向量结果
     V: TCnMLKEMPolyVector                - 待转换的 MKLEM 格式的多项式向量

   返回值：（无）
}

procedure MLKEMVectorToINTT(var Res: TCnMLKEMPolyVector; const V: TCnMLKEMPolyVector);
{* 将 NTT 系数的 MKLEM 格式的多项式向量就地转换为非 NTT 系数的。

   参数：
     var Res: TCnMLKEMPolyVector          - MKLEM 格式的多项式向量结果
     V: TCnMLKEMPolyVector                - 待转换的 MKLEM 格式的多项式向量

   返回值：（无）
}

procedure MLKEMVectorAdd(var Res: TCnMLKEMPolyVector;
  const P1: TCnMLKEMPolyVector; const P2: TCnMLKEMPolyVector);
{* 两个 MKLEM 格式的多项式向量在 mod 3329 有限域中相加，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLKEMPolynomial              - MKLEM 格式的多项式向量和
     P1: TCnMLKEMPolynomial               - MKLEM 格式的多项式向量加数一
     P2: TCnMLKEMPolynomial               - MKLEM 格式的多项式向量加数二

   返回值：（无）
}

procedure MLKEMMatrixVectorMul(var Res: TCnMLKEMPolyVector;
  const A: TCnMLKEMPolyMatrix; const S: TCnMLKEMPolyVector; IsNTT: Boolean = True);
{* 一个 MKLEM 格式的多项式方阵在 mod 3329 及 x^256 + 1 的多项式环上乘以一个多项式向量，
   得到一个多项式向量。用户需自行确保 A 是方阵，及其尺寸与 S 维度一致。

   参数：
     Res: TCnMLKEMPolyVector              - MKLEM 格式的多项式积
     A: TCnMLKEMPolyMatrix                - MKLEM 格式的多项式方阵
     S: TCnMLKEMPolyVector                - MKLEM 格式的多项式向量
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

   返回值：（无）
}

procedure MLKEMVectorDotProduct(var Res: TCnMLKEMPolynomial;
  const V1: TCnMLKEMPolyVector; const V2: TCnMLKEMPolyVector; IsNTT: Boolean = True);
{* 两个 MKLEM 格式的多项式向量在 mod 3329 及 x^256 + 1 的多项式环上点乘，
   得到一个多项式向量。

   参数：
     Res: TCnMLKEMPolynomial              - MKLEM 格式的多项式点乘积
     V1: TCnMLKEMPolynomial               - MKLEM 格式的多项式乘数一
     V2: TCnMLKEMPolynomial               - MKLEM 格式的多项式乘数二
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

   返回值：（无）
}

implementation

resourcestring
  SCnErrorMLKEMInvalidParam = 'Invalid MLKEM Value';
  SCnErrorMLKEMEtaMustBe2Or3 = 'Eta Must Be 2 or 3';
  SCnErrorMLKEMInvalidRandomLength = 'Invalid Random Length for SamplePolyCBD';
  SCnErrorMLKEMInvalidSampleNTT = 'Sample NTT Function Input Must Be 34 Bytes';
  SCnErrorMLKEMInvalidEncodeDigit = 'Digit Must Be Between 1 and 12';
  SCnErrorMLKEMEncapKeyLengthMismatch = 'Encapsulation Key Length Mismatch. Expected %d, Got %d';
  SCnErrorMLKEMEncapKeyModulusCheckFailed = 'Encapsulation Key Modulus Check Failed';
  SCnErrorMLKEMDecapKeyLengthMismatch = 'Decapsulation Key Length Mismatch. Expected %d, Got %d';
  SCnErrorMLKEMDecapKeyStructureInvalid = 'Invalid Decapsulation Key Structure: Can NOT Extract Encapsulation Key';
  SCnErrorMLKEMDecapKeyHashFailed = 'Decapsulation Key Hash Verification Failed';
  SCnErrorMLKEMInvalidMsgLength = 'Invalid Message Length';
  SCnErrorMLKEMInvalidHexLength = 'Invalid Random Hex Length';
  SCnErrorMLKEMCipherLengthMismatch = 'Cipher Length Mismatch. Expected %d, Got %d';
  SCnErrorMLKEMKeyPairCheckFail = 'Key Pair Check Failed';
  SCnErrorMLKEMInvalidPolynomialDegree = 'Invalid Polynomial Degree';

type
  // Barrett Reduction 所需的参数结构体
  TMLKEMBarrettReduce = packed record
    MU: Cardinal;    // Floror(2^k / q) 的近似值 (通常四舍五入)
    K: Integer;      // 幂次 k，用于计算 2^k
    HalfQ: Word;     // q/2 的上取整或下取整，用于中心化约减
    D: Integer;      // 此表项对应的压缩参数 d (如 du 或 dv)
  end;

const
  // ML-KEM Barrett Reduction 查找表，包含不同参数集和操作（压缩、解压）所需的约减参数
  MLKEM_BARRETT_TABLE: array[0..4] of TMLKEMBarrettReduce = (
    (MU: 80635;   K: 28; HalfQ: 1665; D: 1),     // round(2^28/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  1 is mlkem512 du
    (MU: 1290167; K: 32; HalfQ: 1665; D: 10),    // round(2^32/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  10 is mlkem768 du
    (MU: 80635;   K: 28; HalfQ: 1665; D: 4),     // round(2^28/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  4 is mlkem768 dv
    (MU: 40318;   K: 27; HalfQ: 1664; D: 5),     // round(2^27/MLKEM_Q), ?, Floor(MLKEM_Q/2), 5 is mlkem1024 dv
    (MU: 645084;  K: 31; HalfQ: 1664; D: 11)     // round(2^31/MLKEM_Q), ?, Floor(MLKEM_Q/2), 11 is mlkem1024 du
  );

  // FIPS 203 Appendix A 的 NTT 预计算值
  MLKEM_ZETA_NTT: array[0..127] of Word = (
    1, 1729, 2580, 3289, 2642, 630, 1897, 848,
    1062, 1919, 193, 797, 2786, 3260, 569, 1746,
    296, 2447, 1339, 1476, 3046, 56, 2240, 1333,
    1426, 2094, 535, 2882, 2393, 2879, 1974, 821,
    289, 331, 3253, 1756, 1197, 2304, 2277, 2055,
    650, 1977, 2513, 632, 2865, 33, 1320, 1915,
    2319, 1435, 807, 452, 1438, 2868, 1534, 2402,
    2647, 2617, 1481, 648, 2474, 3110, 1227, 910,
    17, 2761, 583, 2649, 1637, 723, 2288, 1100,
    1409, 2662, 3281, 233, 756, 2156, 3015, 3050,
    1703, 1651, 2789, 1789, 1847, 952, 1461, 2687,
    939, 2308, 2437, 2388, 733, 2337, 268, 641,
    1584, 2298, 2037, 3220, 375, 2549, 2090, 1645,
    1063, 319, 2773, 757, 2099, 561, 2466, 2594,
    2804, 1092, 403, 1026, 1143, 2150, 2775, 886,
    1722, 1212, 1874, 1029, 2110, 2935, 885, 2154
  );

  ZETA_BASE_CASE: array[0..127] of Word = (
    17, 3312, 2761, 568, 583, 2746, 2649, 680,
    1637, 1692, 723, 2606, 2288, 1041, 1100, 2229,
    1409, 1920, 2662, 667, 3281, 48, 233, 3096,
    756, 2573, 2156, 1173, 3015, 314, 3050, 279,
    1703, 1626, 1651, 1678, 2789, 540, 1789, 1540,
    1847, 1482, 952, 2377, 1461, 1868, 2687, 642,
    939, 2390, 2308, 1021, 2437, 892, 2388, 941,
    733, 2596, 2337, 992, 268, 3061, 641, 2688,
    1584, 1745, 2298, 1031, 2037, 1292, 3220, 109,
    375, 2954, 2549, 780, 2090, 1239, 1645, 1684,
    1063, 2266, 319, 3010, 2773, 556, 757, 2572,
    2099, 1230, 561, 2768, 2466, 863, 2594, 735,
    2804, 525, 1092, 2237, 403, 2926, 1026, 2303,
    1143, 2186, 2150, 1179, 2775, 554, 886, 2443,
    1722, 1607, 1212, 2117, 1874, 1455, 1029,2300,
    2110, 1219, 2935, 394, 885, 2444, 2154, 1175
  );

var
  FInt64PolynomialPool: TCnInt64PolynomialPool = nil;
  FMLKEMRing: TCnInt64Polynomial = nil;

// ================================ MLKEM ======================================

procedure MLKEMCheckEta(Eta: Integer);
begin
  if (Eta <> 2) and (Eta <> 3) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMEtaMustBe2Or3);
end;

procedure MLKEMCheckEncodeDigit(D: Integer);
begin
  if not D in [1.. 12] then
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidEncodeDigit);
end;

// 把每个数的低 D 位取出来紧拼到一起
function MLKEMByteEncode(W: TWords; D: Integer): TBytes; overload;
var
  I: Integer;
  B: TCnBitBuilder;
begin
  MLKEMCheckEncodeDigit(D);

  B := TCnBitBuilder.Create;
  try
    for I := 0 to Length(W) - 1 do
      B.AppendWordRange(W[I], D - 1);

    Result := B.ToBytes;
  finally
    B.Free;
  end;
end;

function MLKEMByteEncode(P: TCnMLKEMPolynomial; D: Integer): TBytes; overload;
var
  I: Integer;
  B: TCnBitBuilder;
begin
  MLKEMCheckEncodeDigit(D);

  B := TCnBitBuilder.Create;
  try
    for I := Low(P) to High(P) do
      B.AppendWordRange(P[I], D - 1);

    Result := B.ToBytes;
  finally
    B.Free;
  end;
end;

function MLKEMByteDecode(B: TBytes; D: Integer): TWords; overload;
var
  I, L: Integer;
  C: TCnBitBuilder;
  V: Cardinal;
begin
  if Length(B) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  MLKEMCheckEncodeDigit(D);

  C := TCnBitBuilder.Create;
  try
    C.SetBytes(B);

    L := (8 * (Length(B)) + D - 1) div D;
    SetLength(Result, L);

    for I := 0 to L - 1 do
    begin
      V := C.Copy(I * D, D);
      if D = 12 then
        Result[I] := V mod CN_MLKEM_PRIME
      else
        Result[I] := V;
    end;
  finally
    C.Free;
  end;
end;

// 调用者要确保 B 解开的是 256 个 Word
procedure MLKEMByteDecode(B: TBytes; D: Integer; out P: TCnMLKEMPolynomial); overload;
var
  W: TWords;
begin
  W := MLKEMByteDecode(B, D);
  Move(W[0], P[0], Length(W) * SizeOf(Word));
end;

// 将一个 X 的系数值压缩到 D 位并返回
function MLKEMCompress(X, D: Word): Word;
var
  V, T: Word;
  I: Integer;

  function DivMlKemQ(X: Word; B, HQ, BS: Integer; BM: TUInt64): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  var
    R: TUInt64;
  begin
    R := (TUInt64(X) shl B) + TUInt64(HQ);
    R := UInt64Mul(R, BM);
    R := R shr BS;
    Result := Word(R and ((1 shl B) - 1));
  end;

begin
  V := 0;
  T := (X + CN_MLKEM_PRIME) mod CN_MLKEM_PRIME;

  for I := Low(MLKEM_BARRETT_TABLE) to High(MLKEM_BARRETT_TABLE) do
  begin
    if D = MLKEM_BARRETT_TABLE[I].D then
    begin
      V := DivMlKemQ(T, MLKEM_BARRETT_TABLE[I].D, MLKEM_BARRETT_TABLE[I].HalfQ,
        MLKEM_BARRETT_TABLE[I].K, MLKEM_BARRETT_TABLE[I].MU);
      Break;
    end;
  end;

  Result := V;
end;

// 将一个压缩后的系数值解压并返回
function MLKEMDecompress(X: Word; D: Word): Word;
var
  P: Cardinal;
begin
  P := Cardinal(X) * CN_MLKEM_PRIME;

  Result := Word((P shr D) +                // 商（主干部分）
    ((P and ((1 shl D) - 1)) shr (D - 1))); // 四舍五入的进位项
end;

// 压缩一个多项式，Res 和 Poly 可以相同
procedure MLKEMCompressPolynomial(var Res: TCnMLKEMPolynomial; const Poly: TCnMLKEMPolynomial;
  D: Integer);
var
  I: Integer;
begin
  for I := 0 to CN_MLKEM_POLY_SIZE - 1 do
    Res[I] := MLKEMCompress(Poly[I], D);
end;

// 压缩一个多项式向量，Res 和 V 可以相同
procedure MLKEMCompressVector(var Res: TCnMLKEMPolyVector; const V: TCnMLKEMPolyVector;
  D: Integer);
var
  I, J: Integer;
begin
  if V <> Res then
    SetLength(Res, Length(V));

  for I := 0 to Length(V) - 1 do
  begin
    for J := 0 to CN_MLKEM_POLY_SIZE - 1 do
      Res[I][J] := MLKEMCompress(V[I][J], D);
  end;
end;

// 解压缩一个多项式，Res 和 Poly 可以相同
procedure MLKEMDecompressPolynomial(var Res: TCnMLKEMPolynomial; const Poly: TCnMLKEMPolynomial;
  D: Integer);
var
  I: Integer;
begin
  for I := 0 to CN_MLKEM_POLY_SIZE - 1 do
    Res[I] := MLKEMDecompress(Poly[I], D);
end;

// 解压缩一个多项式向量，Res 和 V 可以相同
procedure MLKEMDecompressVector(var Res: TCnMLKEMPolyVector; const V: TCnMLKEMPolyVector;
  D: Integer);
var
  I, J: Integer;
begin
  if V <> Res then
    SetLength(Res, Length(V));

  for I := 0 to Length(V) - 1 do
  begin
    for J := 0 to CN_MLKEM_POLY_SIZE - 1 do
      Res[I][J] := MLKEMDecompress(V[I][J], D);
  end;
end;

// 模素数加减乘法
function MLKEMModAdd(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A + B) mod CN_MLKEM_PRIME;
end;

function MLKEMModSub(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if A >= B then
    Result := A - B
  else
    Result := CN_MLKEM_PRIME + A - B;

  Result := Result mod CN_MLKEM_PRIME;
end;

function MLKEMModMul(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Word(Cardinal(A) * Cardinal(B) mod CN_MLKEM_PRIME);
end;

// MLKEM 使用的特定数论变换
function MLKEMNTT(const F: TWords): TWords;
var
  Len, Start, J, I: Integer;
  Zeta, T: Word;
begin
  SetLength(Result, CN_MLKEM_POLY_SIZE);
  Move(F[0], Result[0], CN_MLKEM_POLY_SIZE * SizeOf(Word));

  I := 1;
  Len := 128;

  while Len >= 2 do
  begin
    Start := 0;
    while Start < CN_MLKEM_POLY_SIZE do
    begin
      Zeta := MLKEM_ZETA_NTT[I];
      Inc(I);

      for J := Start to Start + Len - 1 do
      begin
        T := MLKEMModMul(Zeta, Result[J + Len]);
        Result[J + Len] := MLKEMModSub(Result[J], T);
        Result[J] := MLKEMModAdd(Result[J], T);
      end;

      Inc(Start, 2 * Len);
    end;

    Len := Len div 2;
  end;
end;

function MLKEMINTT(const F: TWords): TWords;
var
  Len, Start, J, I: Integer;
  Zeta, T: Word;
begin
  SetLength(Result, CN_MLKEM_POLY_SIZE);
  Move(F[0], Result[0], CN_MLKEM_POLY_SIZE * SizeOf(Word));

  I := 127;
  Len := 2;

  while Len <= 128 do
  begin
    Start := 0;
    while Start < CN_MLKEM_POLY_SIZE do
    begin
      Zeta := MLKEM_ZETA_NTT[I];
      Dec(I);

      for J := Start to Start + Len - 1 do
      begin
        T := Result[J];
        Result[J] := MLKEMModAdd(T, Result[J + Len]);
        Result[J + Len] := MLKEMModMul(Zeta, MLKEMModSub(Result[J + Len], T));
      end;

      Inc(Start, 2 * Len);
    end;

    Len := Len * 2;
  end;

  // 最终缩放：乘以 3303，是 128 对 3329 的模逆元
  for J := 0 to CN_MLKEM_POLY_SIZE - 1 do
    Result[J] := MLKEMModMul(Result[J], CN_MLKEM_PRIME_INV);
end;

// 根据真随机数组生成 256 个采样的 NTT 多项式系数，RandBytes 的长度至少要 34 字节
function MLKEMSampleNTT(const RandBytes: TBytes): TWords;
var
  Ctx: TCnSHA3Context;
  C: TBytes;
  D1, D2: Integer;
  J: Integer;
begin
  if Length(RandBytes) < CN_MLKEM_KEY_SIZE + 2 then
    raise Exception.Create(SCnErrorMLKEMInvalidSampleNTT);

  SetLength(Result, CN_MLKEM_POLY_SIZE);

  SHAKE128Init(Ctx, 0);
  SHAKE128Absorb(Ctx, PAnsiChar(@RandBytes[0]), Length(RandBytes));

  J := 0;
  while J < CN_MLKEM_POLY_SIZE do
  begin
    C := SHAKE128Squeeze(Ctx, 3);

    // 从 3 字节中提取两个 12 位数值
    D1 := C[0] + 256 * (C[1] and $0F);       // 使用 C[1] 的低 4 位
    D2 := (C[1] shr 4) + 16 * C[2];          // 使用 C[1] 的高 4 位

    // 检查第一个值是否有效
    if D1 < CN_MLKEM_PRIME then
    begin
      Result[J] := D1;
      Inc(J);

      // 如果已经收集够 256 个值，提前退出
      if J >= CN_MLKEM_POLY_SIZE then
        Break;
    end;

    // 检查第二个值是否有效
    if (D2 < CN_MLKEM_PRIME) and (J < CN_MLKEM_POLY_SIZE) then
    begin
      Result[J] := D2;
      Inc(J);
    end;
  end;
end;

// 根据真随机数组生成 256 个采样的多项式系数，RandBytes 的长度至少要 64 * Eta 字节
function MLKEMSamplePolyCBD(const RandBytes: TBytes; Eta: Integer): TWords;
var
  I, J, X, Y: Integer;
  Bits: TCnBitBuilder;

  function BitToInt(Bit: Boolean): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  begin
    if Bit then
      Result := 1
    else
      Result := 0;
  end;

begin
  MLKEMCheckEta(Eta);
  if Length(RandBytes) < 64 * Eta then
    raise Exception.Create(SCnErrorMLKEMInvalidRandomLength);

  SetLength(Result, CN_MLKEM_POLY_SIZE);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendBytes(RandBytes);

    for I := 0 to CN_MLKEM_POLY_SIZE - 1 do
    begin
      X := 0;
      Y := 0;

      for J := 0 to Eta - 1 do
        X := X + BitToInt(Bits[2 * I * Eta + J]);
      for J := 0 to Eta - 1 do
        Y := Y + BitToInt(Bits[2 * I * Eta + Eta + J]);

      if X >= Y then
        Result[I] := X - Y
      else
        Result[I] := CN_MLKEM_PRIME + X - Y;
    end;
  finally
    Bits.Free;
  end;
end;

// PRF 函数，根据 32 字节输入和一字节附加数据，用 SHAKE256 生成 64 * Eta 长度的字节
function MLKEMPseudoRandomFunc(Eta: Integer; const Input: TCnMLKEMSeed; B: Byte): TBytes;
var
  T: TBytes;
begin
  if (Eta <> 2) and (Eta <> 3) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMEtaMustBe2Or3);

  SetLength(T, SizeOf(TCnMLKEMSeed) + 1);
  Move(Input[0], T[0], SizeOf(TCnMLKEMSeed));
  T[SizeOf(TCnMLKEMSeed)] := B;

  Result := SHAKE256Bytes(T, Eta * 64);
end;

// MLKEM 使用的 G 函数，内部用 SHA3_512 生成两个 32 位杂凑值
procedure MLKEMGFunc(const Data: TBytes; out Block1, Block2: TCnMLKEMBlock);
var
  Dig: TCnSHA3_512Digest;
begin
  Dig := SHA3_512Bytes(Data);
  Move(Dig[0], Block1[0], SizeOf(TCnMLKEMBlock));
  Move(Dig[SizeOf(TCnMLKEMBlock)], Block2[0], SizeOf(TCnMLKEMBlock));
end;

// MLKEM 使用的 H 函数，内部用 SHA3_256 生成 32 位杂凑值
function MLKEMHFunc(const Data: TBytes): TCnMLKEMBlock;
var
  Dig: TCnSHA3_256Digest;
begin
  Dig := SHA3_256Bytes(Data);
  Move(Dig[0], Result[0], SizeOf(TCnMLKEMBlock));
end;

// MLKEM 使用的 J 函数，内部用 SHAKE256 生成 32 位杂凑值
function MLKEMJFunc(const Data: TBytes): TCnMLKEMBlock;
var
  Dig: TBytes;
begin
  Dig := SHAKE256Bytes(Data, SizeOf(TCnMLKEMBlock));
  Move(Dig[0], Result[0], SizeOf(TCnMLKEMBlock));
end;

{ TCnMLKEM }

procedure TCnMLKEM.CheckDecapKey(DeKey: TBytes);
var
  ExpLen: Integer;
  EkStart, EkLength: Integer;
  EkBytes: TBytes;
  HStart, HLength: Integer;
  HBytes, ComputedHash: TCnMLKEMBlock;
begin
  // 检查长度是否符合预期
  ExpLen := GetDecapKeyByteLength;
  if Length(DeKey) <> ExpLen then
    raise ECnMLKEMException.CreateFmt(SCnErrorMLKEMDecapKeyLengthMismatch,
      [ExpLen, Length(DeKey)]);

  // 提取 ek 部分: dk[384k : 768k+32]
  EkStart := 384 * FMatrixSize;
  EkLength := GetEncapKeyByteLength;

  if (EkStart + EkLength) > Length(DeKey) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMDecapKeyStructureInvalid);

  SetLength(EkBytes, EkLength);
  Move(DeKey[EkStart], EkBytes[0], EkLength);

  // 使用已有的封装密钥检查方法
  CheckEncapKey(EkBytes);

  // 杂凑检查，验证 H(ek) 是否正确
  // H(ek) 位于 dk[768k+32 : 768k+64]
  HStart := 768 * FMatrixSize + 32;
  HLength := SizeOf(TCnMLKEMBlock);

  if (HStart + HLength) > Length(DeKey) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMDecapKeyStructureInvalid);

  Move(DeKey[HStart], HBytes[0], HLength);

  // 计算 ek 的杂凑值
  ComputedHash := MLKEMHFunc(EkBytes);

  // 比较计算出的杂凑值与存储的杂凑值
  if not CompareMem(@HBytes[0], @ComputedHash[0], SizeOf(TCnMLKEMBlock)) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMDecapKeyHashFailed);
end;

procedure TCnMLKEM.CheckEncapKey(EnKey: TBytes);
var
  PolyBytesLength: Integer;
  PolyBytes, TestBytes: TBytes;
begin
  if Length(EnKey) <> GetEncapKeyByteLength then
    raise ECnMLKEMException.CreateFmt(SCnErrorMLKEMEncapKeyLengthMismatch,
      [GetEncapKeyByteLength, Length(EnKey)]);

  PolyBytesLength := 384 * FMatrixSize;

  // 提取多项式部分字节
  SetLength(PolyBytes, PolyBytesLength);
  Move(EnKey[0], PolyBytes[0], PolyBytesLength);

  // 进行 ByteDecode12 然后 ByteEncode12 往返
  TestBytes := MLKEMByteEncode(MLKEMByteDecode(PolyBytes, 12), 12);
  if not CompareBytes(PolyBytes, TestBytes) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMEncapKeyModulusCheckFailed);
end;

procedure TCnMLKEM.CheckKeyPair(EncapKey: TCnMLKEMEncapsulationKey;
  DecapKey: TCnMLKEMDecapsulationKey);
var
  En, De: TBytes;
  Matrix: TCnMLKEMPolyMatrix;
  M, ShareKey, CipherText: TBytes;
begin
  // 格式解析检验
  En := SaveEncapKeyToBytes(EncapKey);
  CheckEncapKey(En);
  De := SaveKeysToBytes(DecapKey, EncapKey);
  CheckDecapKey(De);

  // 矩阵生成检验
  GenerateMatrix(EncapKey.GenerationSeed, Matrix);

  // Encap/Decap 检验
  M := CnRandomBytes(SizeOf(TCnMLKEMBlock));
  MLKEMEncaps(En, M, ShareKey, CipherText); // 包装一个共享密钥，并拿到密文

  M := MLKEMDecaps(De, CipherText);         // 解密文，核对是否和共享密钥相等
  if not CompareBytes(M, ShareKey) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMKeyPairCheckFail);
end;

constructor TCnMLKEM.Create(AType: TCnMLKEMType);
begin
  inherited Create;
  FMLKEMType := AType;
  FNoise2 := 2;

  case AType of
    cmkt512:
      begin
        FMatrixSize := 2;
        FNoise1 := 3;
        FCompressU := 10;
        FCompressV := 4;
      end;
    cmkt768:
      begin
        FMatrixSize := 3;
        FNoise1 := 2;
        FCompressU := 10;
        FCompressV := 4;
      end;
    cmkt1024:
      begin
        FMatrixSize := 4;
        FNoise1 := 2;
        FCompressU := 11;
        FCompressV := 5;
      end;
  else
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidParam);
  end;
end;

destructor TCnMLKEM.Destroy;
begin

  inherited;
end;

procedure TCnMLKEM.GenerateMatrix(const Seed: TCnMLKEMSeed;
  out Matrix: TCnMLKEMPolyMatrix);
var
  I, J: Integer;
  PJI: TBytes;
  W: TWords;
begin
  // 设置矩阵大小
  SetLength(Matrix, FMatrixSize);
  for I := 0 to FMatrixSize - 1 do
    SetLength(Matrix[I], FMatrixSize);

  // 准备好 Sample 随机数据
  SetLength(PJI, SizeOf(TCnMLKEMSeed) + 2);
  Move(Seed[0], PJI[0], SizeOf(TCnMLKEMSeed));

  // 生成矩阵
  for I := 0 to FMatrixSize - 1 do
  begin
    for J := 0 to FMatrixSize - 1 do
    begin
      PJI[SizeOf(TCnMLKEMSeed)] := J;
      PJI[SizeOf(TCnMLKEMSeed) + 1] := I;
      W := MLKEMSampleNTT(PJI);
      Move(W[0], Matrix[I][J][0], Length(W) * SizeOf(Word));
    end;
  end;
end;

procedure TCnMLKEM.TransposeMatrix(const InMatrix: TCnMLKEMPolyMatrix; out Matrix: TCnMLKEMPolyMatrix);
var
  I, J: Integer;
begin
  SetLength(Matrix, FMatrixSize);
  for I := 0 to FMatrixSize - 1 do
    SetLength(Matrix[I], FMatrixSize);

  for I := 0 to FMatrixSize - 1 do
  begin
    for J := 0 to FMatrixSize - 1 do
      Matrix[J][I] := InMatrix[I][J];
  end;
end;

function TCnMLKEM.GetDecapKeyByteLength: Integer;
begin
  Result := 768 * FMatrixSize + 96;
end;

function TCnMLKEM.GetEncapKeyByteLength: Integer;
begin
  Result := 384 * FMatrixSize + 32;
end;

procedure TCnMLKEM.KPKEKeyGen(const D: TCnMLKEMSeed; out GenerationSeed: TCnMLKEMSeed;
  out Secret, Pub: TCnMLKEMPolyVector);
var
  N: Integer;
  O: TCnMLKEMSeed;
  DK: TBytes;
  Matrix: TCnMLKEMPolyMatrix;
  Noise: TCnMLKEMPolyVector;
begin
  SetLength(DK, 1);
  DK[0] := FMatrixSize;
  DK := ConcatBytes(NewBytesFromMemory(@D[0], SizeOf(TCnMLKEMBlock)), DK);

  // 生成公共种子
  MLKEMGFunc(DK, TCnMLKEMBlock(GenerationSeed), TCnMLKEMBlock(O));
  N := 0;

  // 生成矩阵
  GenerateMatrix(GenerationSeed, Matrix);

  // 生成 K 个 S，是 NTT 形式的
  SampleVector(O, FNoise1, N, Secret);

  // 生成 K 个 E，也是 NTT 形式的
  SampleVector(O, FNoise1, N, Noise);

  // 计算 T = A * S + E
  MLKEMMatrixVectorMul(Pub, Matrix, Secret);
  MLKEMVectorAdd(Pub, Pub, Noise);
end;

procedure TCnMLKEM.LoadKeyFromBytes(Key: TBytes; EncapKey: TCnMLKEMEncapsulationKey);
var
  I: Integer;
  PolyBytes: TBytes;
  PolyLength: Integer;
  SeedStart: Integer;
begin
  // 首先检查密钥字节数组的合法性
  CheckEncapKey(Key);

  // 计算多项式部分的长度（每个多项式 384 字节，共 k 个）
  PolyLength := 384 * FMatrixSize;

  // 确保字节数组长度足够
  if Length(Key) < PolyLength + SizeOf(TCnMLKEMSeed) then
    raise ECnMLKEMException.CreateFmt(SCnErrorMLKEMEncapKeyLengthMismatch,
      [PolyLength + SizeOf(TCnMLKEMSeed), Length(Key)]);

  // 设置公钥多项式向量的大小
  SetLength(EncapKey.PubVector, FMatrixSize);

  // 解析多项式部分
  for I := 0 to FMatrixSize - 1 do
  begin
    // 提取每个多项式的字节数据（每个384字节）
    SetLength(PolyBytes, 384);
    Move(Key[I * 384], PolyBytes[0], 384);

    // 将字节解码为多项式系数
    // 注意这里需要将解码后的 TWords 转换为 TCnMLKEMPolynomial
    // 假设 ByteDecode 返回的 TWords 长度是 256
    Move(MLKEMByteDecode(PolyBytes, 12)[0], EncapKey.PubVector[I][0],
      CN_MLKEM_POLY_SIZE * SizeOf(Word));
  end;

  // 解析生成种子部分（最后 32 字节）
  SeedStart := PolyLength;
  Move(Key[SeedStart], EncapKey.GenerationSeed[0], SizeOf(TCnMLKEMSeed));
end;

procedure TCnMLKEM.LoadKeysFromBytes(Key: TBytes;
  DecapKey: TCnMLKEMDecapsulationKey; EncapKey: TCnMLKEMEncapsulationKey);
var
  I, ZStart, PolyLength, EkStart, EkLength: Integer;
  PolyBytes, EkBytes: TBytes;
begin
  // 首先检查密钥字节数组的合法性
  CheckDecapKey(Key);

  // 计算各部分的位置和长度
  PolyLength := 384 * FMatrixSize; // 秘密向量部分长度

  // 解析秘密向量部分 (dk)
  SetLength(DecapKey.SecretVector, FMatrixSize);
  for I := 0 to FMatrixSize - 1 do
  begin
    // 提取每个秘密多项式的字节数据（每个 384 字节）
    SetLength(PolyBytes, 384);
    Move(Key[I * 384], PolyBytes[0], 384);

    // 将字节解码为多项式系数
    Move(MLKEMByteDecode(PolyBytes, 12)[0], DecapKey.SecretVector[I][0],
      CN_MLKEM_POLY_SIZE * SizeOf(Word));
  end;

  // Key 字节数组中的杂凑部分加载进非公开密钥的杂凑值中
  Move(Key[768 * FMatrixSize + 32], DecapKey.EnKeyHash[0], SizeOf(TCnMLKEMSeed));

  // 解析封装密钥部分 (ek)
  EkStart := PolyLength; // 384k
  EkLength := GetEncapKeyByteLength; // ek 部分的长度

  SetLength(EkBytes, EkLength);
  Move(Key[EkStart], EkBytes[0], EkLength);

  // 使用已有的 LoadKeyFromBytes 方法加载封装密钥
  LoadKeyFromBytes(EkBytes, EncapKey);

  // 解析注入种子部分 (z)
  ZStart := 768 * FMatrixSize + 64; // dk(384k) + ek(384k+32) + H(ek)(32) = 768k+64
  Move(Key[ZStart], DecapKey.InjectionSeed[0], SizeOf(TCnMLKEMSeed));
end;

procedure TCnMLKEM.GenerateKeys(EncapKey: TCnMLKEMEncapsulationKey;
  DecapKey: TCnMLKEMDecapsulationKey; const RandDHex: string; const RandZHex: string);
var
  D: TCnMLKEMSeed;
  B: TBytes;
begin
  if ((Length(RandDHex) > 0) and (Length(RandDHex) <> 64)) or
    ((Length(RandZHex) > 0) and (Length(RandZHex) <> 64)) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidHexLength);

  if Length(RandDHex) = 0 then
    CnRandomFillBytes(@D[0], SizeOf(TCnMLKEMSeed))
  else
    PutBytesToMemory(HexToBytes(RandDHex), @D[0], SizeOf(TCnMLKEMSeed));

  if Length(RandZHex) = 0 then
    CnRandomFillBytes(@DecapKey.InjectionSeed[0], SizeOf(TCnMLKEMSeed))
  else
    PutBytesToMemory(HexToBytes(RandZHex), @DecapKey.InjectionSeed[0], SizeOf(TCnMLKEMSeed));

  // 生成密钥
  KPKEKeyGen(D, EncapKey.GenerationSeed, DecapKey.SecretVector, EncapKey.PubVector);

  // 公开密钥的杂凑值，放非公开密钥里备用
  B := SaveEncapKeyToBytes(EncapKey);
  DecapKey.EnKeyHash := TCnMLKEMSeed(MLKEMHFunc(B));
end;

procedure TCnMLKEM.GenerateKeys(out EnKey: TBytes; out DeKey: TBytes;
  const RandDHex: string; const RandZHex: string);
var
  EK: TCnMLKEMEncapsulationKey;
  DK: TCnMLKEMDecapsulationKey;
begin
  EK := nil;
  DK := nil;

  try
    EK := TCnMLKEMEncapsulationKey.Create;
    DK := TCnMLKEMDecapsulationKey.Create;

    GenerateKeys(EK, DK, RandDHex, RandZHex);
    EnKey := SaveEncapKeyToBytes(EK);
    DeKey := SaveKeysToBytes(DK, EK);
  finally
    DK.Free;
    EK.Free;
  end;
end;

procedure TCnMLKEM.SamplePolynomial(const Seed: TCnMLKEMSeed;
  Noise: Integer; var Counter: Integer; out Polynomial: TCnMLKEMPolynomial;
  UseNTT: Boolean);
var
  R: TBytes;
  W: TWords;
begin
  R := MLKEMPseudoRandomFunc(Noise, Seed, Counter);
  W := MLKEMSamplePolyCBD(R, Noise);
  if UseNTT then
    W := MLKEMNTT(W);
  Move(W[0], Polynomial[0], Length(W) * SizeOf(Word));
  Inc(Counter);
end;

procedure TCnMLKEM.SampleVector(const Seed: TCnMLKEMSeed; Noise: Integer;
  var Counter: Integer; out PolyVector: TCnMLKEMPolyVector; UseNTT: Boolean);
var
  I: Integer;
  R: TBytes;
  W: TWords;
begin
  SetLength(PolyVector, FMatrixSize);
  for I := 0 to FMatrixSize - 1 do
  begin
    R := MLKEMPseudoRandomFunc(Noise, Seed, Counter);
    W := MLKEMSamplePolyCBD(R, Noise);
    if UseNTT then
      W := MLKEMNTT(W);
    Move(W[0], PolyVector[I][0], Length(W) * SizeOf(Word));
    Inc(Counter);
  end;
end;

function TCnMLKEM.SaveKeysToBytes(DecapKey: TCnMLKEMDecapsulationKey;
  EncapKey: TCnMLKEMEncapsulationKey): TBytes;
var
  I: Integer;
  EK, DK: TBytes;
begin
  EK := SaveEncapKeyToBytes(EncapKey);

  DK := nil;
  for I := 0 to FMatrixSize - 1 do
    DK := ConcatBytes(DK, MLKEMByteEncode(DecapKey.SecretVector[I], 12));

  // dk || ek || H(ek) || z
  Result := ConcatBytes(DK, EK);
  Result := ConcatBytes(Result, NewBytesFromMemory(@DecapKey.EnKeyHash[0], SizeOf(TCnMLKEMSeed)));
  Result := ConcatBytes(Result, NewBytesFromMemory(@DecapKey.InjectionSeed[0], SizeOf(TCnMLKEMBlock)));
end;

function TCnMLKEM.SaveEncapKeyToBytes(EncapKey: TCnMLKEMEncapsulationKey): TBytes;
var
  I: Integer;
begin
  // T 拼上 PubSeed 输出作为 EK
  Result := nil;
  for I := 0 to FMatrixSize - 1 do
    Result := ConcatBytes(Result, MLKEMByteEncode(EncapKey.PubVector[I], 12));

  Result := ConcatBytes(Result, NewBytesFromMemory(@EncapKey.GenerationSeed[0], SizeOf(TCnMLKEMSeed)));
end;

procedure TCnMLKEM.KPKEEncrypt(EncapKey: TCnMLKEMEncapsulationKey;
  const Msg: TCnMLKEMBlock; const Seed: TCnMLKEMSeed; out UVector: TCnMLKEMPolyVector;
  out VPolynomial: TCnMLKEMPolynomial);
var
  N: Integer;
  M, MT: TCnMLKEMPolyMatrix;
  Y, E1: TCnMLKEMPolyVector;
  E2, MP: TCnMLKEMPolynomial;
  B: TBytes;
  W: TWords;
begin
  // 生成矩阵
  GenerateMatrix(EncapKey.GenerationSeed, M);

  // 转置矩阵
  TransposeMatrix(M, MT);

  // 生成随机向量 Y 与误差向量 E1，注意前者 Y 是 NTT 的，便于运算
  N := 0;
  SampleVector(Seed, FNoise1, N, Y);

  // 后者 E1 非 NTT 的
  SampleVector(Seed, FNoise2, N, E1, False);

  // 生成误差多项式 E2，注意也非 NTT 模式
  SamplePolynomial(Seed, FNoise2, N, E2, False);

  // U = AT * Y + E1，先 NTT 矩阵乘，得到 U 中间结果的 NTT 模式
  MLKEMMatrixVectorMul(UVector, MT, Y);

  // U 解回非 NTT 模式，加 E1
  MLKEMVectorToINTT(UVector, UVector);
  MLKEMVectorAdd(UVector, UVector, E1);

  // V = T^ * Y + E2 + 消息多项式，注意 T^ 和 Y 都是 NTT 的，其乘积要做个非 NTT 转换
  MLKEMVectorDotProduct(VPolynomial, EncapKey.PubVector, Y);

  MLKEMPolynomialToINTT(VPolynomial, VPolynomial);      // 转换成非 NTT
  MLKEMPolynomialAdd(VPolynomial, VPolynomial, E2);     // 再加 E2

  // 将 32 字节消息每一位展开成一个 Word，共展开成 256 个 Word
  B := NewBytesFromMemory(@Msg[0], SizeOf(TCnMLKEMBlock));
  W := MLKEMByteDecode(B, 1);

  // 将 256 个 Word 放入多项式
  Move(W[0], MP[0], SizeOf(TCnMLKEMPolynomial));
  MLKEMDecompressPolynomial(MP, MP, 1);

  // V 再加上消息多项式
  MLKEMPolynomialAdd(VPolynomial, VPolynomial, MP);
end;

function TCnMLKEM.MLKEMEncrypt(EnKey: TBytes; Msg: TBytes; const RandHex: string): TBytes;
var
  En: TCnMLKEMEncapsulationKey;
  M: TCnMLKEMBlock;
  Seed: TCnMLKEMSeed;
  B: TBytes;
  L, I: Integer;
  U, UC: TCnMLKEMPolyVector;
  V, VC: TCnMLKEMPolynomial;
begin
  if Length(Msg) <> CN_MLKEM_KEY_SIZE then
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidMsgLength);

  if (Length(RandHex) > 0) and (Length(RandHex) <> 64) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidHexLength);

  En := TCnMLKEMEncapsulationKey.Create;

  try
    LoadKeyFromBytes(EnKey, En);               // 准备好 Key
    Move(Msg[0], M[0], SizeOf(TCnMLKEMBlock)); // 准备好 Msg

    if RandHex = '' then
      CnRandomFillBytes(@Seed[0], SizeOf(TCnMLKEMSeed))
    else
    begin
      B := HexToBytes(RandHex);
      if Length(B) > 0 then
      begin
        L := Length(B);
        if L > SizeOf(TCnMLKEMSeed) then
          L := SizeOf(TCnMLKEMSeed);

        FillChar(Seed[0], SizeOf(TCnMLKEMSeed), 0);
        Move(B[0], Seed[0], L);
      end
      else
        CnRandomFillBytes(@Seed[0], SizeOf(TCnMLKEMSeed));
    end;

    // 准备好了消息种子 M 和随机种子 R，调用内部加密方法，返回 U V
    KPKEEncrypt(En, M, Seed, U, V);

    // 压缩编码，将非 NTT 系数的 U V 返回作为 Chipher 字节数组
    MLKEMCompressVector(UC, U, FCompressU);
    MLKEMCompressPolynomial(VC, V, FCompressV);

    Result := nil;
    for I := Low(UC) to High(UC) do
      Result := ConcatBytes(Result, MLKEMByteEncode(UC[I], FCompressU));
    Result := ConcatBytes(Result, MLKEMByteEncode(VC, FCompressV));
  finally
    En.Free;
  end;
end;

procedure TCnMLKEM.KPKEDecrypt(DecapKey: TCnMLKEMDecapsulationKey;
  const UVector: TCnMLKEMPolyVector; const VPolynomial: TCnMLKEMPolynomial;
  out Msg: TCnMLKEMBlock);
var
  I: Integer;
  B: TBytes;
  U: TCnMLKEMPolyVector;
  W, T: TCnMLKEMPolynomial;
begin
  // 外部传入的 UVector 是非 NTT 的，转换为 NTT 方式放到 U 中
  SetLength(U, FMatrixSize);
  for I := Low(UVector) to High(UVector) do
    MLKEMPolynomialToNTT(U[I], UVector[I]);

  // 计算 S 点乘 U，得到一个 NTT 形式的多项式 T
  MLKEMVectorDotProduct(T, DecapKey.SecretVector, U);

  // 积转换成非 NTT
  MLKEMPolynomialToINTT(T, T);

  // 以非 NTT 的方式计算 V - 这个积，得到消息多项式 W，解密初步完毕
  MLKEMPolynomialSub(W, VPolynomial, T);

  // 压缩该消息多项式
  MLKEMCompressPolynomial(T, W, 1);

  // 解码消息多项式
  B := MLKEMByteEncode(T, 1);
  if Length(B) <> SizeOf(TCnMLKEMBlock) then
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidMsgLength);

  // 返回明文消息内容
  Move(B[0], Msg[0], SizeOf(TCnMLKEMBlock));
end;

function TCnMLKEM.MLKEMDecrypt(DeKey, CipherText: TBytes): TBytes;
var
  De: TCnMLKEMDecapsulationKey;
  En: TCnMLKEMEncapsulationKey;
  UVector: TCnMLKEMPolyVector;
  VPoly: TCnMLKEMPolynomial;
  Msg: TCnMLKEMBlock;
begin
  Result := nil;

  De := TCnMLKEMDecapsulationKey.Create;
  En := TCnMLKEMEncapsulationKey.Create;
  try
    LoadKeysFromBytes(DeKey, De, En);
    ExtractUVFromCipherText(CipherText, UVector, VPoly);
    KPKEDecrypt(De, UVector, VPoly, Msg);

    // 解密后返回，注意这里不判断是否成功
    SetLength(Result, SizeOf(TCnMLKEMBlock));
    Move(Msg[0], Result[0], SizeOf(TCnMLKEMBlock));
  finally
    En.Free;
    De.Free;
  end;
end;

function TCnMLKEM.MLKEMDecaps(DeKey, CipherText: TBytes): TBytes;
var
  I: Integer;
  En: TCnMLKEMEncapsulationKey;
  De: TCnMLKEMDecapsulationKey;
  Msg, S, R: TCnMLKEMBlock;
  B, C: TBytes;
  U, UC: TCnMLKEMPolyVector;
  V, VC: TCnMLKEMPolynomial;
begin
  De := TCnMLKEMDecapsulationKey.Create;
  En := TCnMLKEMEncapsulationKey.Create;
  try
    LoadKeysFromBytes(DeKey, De, En);

    FillChar(Msg[0], SizeOf(TCnMLKEMBlock), 0);
    ExtractUVFromCipherText(CipherText, U, V);
    KPKEDecrypt(De, U, V, Msg);

    B := ConcatBytes(NewBytesFromMemory(@Msg[0], SizeOf(TCnMLKEMBlock)),
      NewBytesFromMemory(@De.EnKeyHash[0], SizeOf(TCnMLKEMSeed)));

    MLKEMGFunc(B, S, R); // S 是解出来的共享密钥，R 是解出的一个随机数

    // 验证，这里重新加解密一遍
    KPKEEncrypt(En, Msg, TCnMLKEMSeed(R), U, V);

    // 压缩编码，将非 NTT 系数的 U V 返回作为 Chipher 字节数组
    MLKEMCompressVector(UC, U, FCompressU);
    MLKEMCompressPolynomial(VC, V, FCompressV);

    C := nil;
    for I := Low(UC) to High(UC) do
      C := ConcatBytes(C, MLKEMByteEncode(UC[I], FCompressU));
    C := ConcatBytes(C, MLKEMByteEncode(VC, FCompressV));

    if not CompareBytes(C, CipherText) then
    begin
      // 结果不匹配，说明出错，重新计算失败的胡乱杂凑放 S 里
      C := NewBytesFromMemory(@De.InjectionSeed[0], SizeOf(TCnMLKEMSeed));
      C := ConcatBytes(C, CipherText);
      S := MLKEMJFunc(C);
    end;

    // 无论结果是否匹配，都返回 S
    Result := NewBytesFromMemory(@S[0], SizeOf(TCnMLKEMBlock));
  finally
    En.Free;
    De.Free;
  end;
end;

procedure TCnMLKEM.MLKEMEncaps(EnKey: TBytes; Msg: TBytes;
  out ShareKey, CipherText: TBytes);
var
  D, R: TBytes;
  B1, B2: TCnMLKEMBlock;
begin
  if Length(Msg) <> CN_MLKEM_KEY_SIZE then
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidMsgLength);

  CheckEncapKey(EnKey);

  B1 := MLKEMHFunc(EnKey);
  D := NewBytesFromMemory(@B1[0], Length(B1));
  MLKEMGFunc(ConcatBytes(Msg, D), B1, B2);

  ShareKey := NewBytesFromMemory(@B1[0], Length(B1));
  R := NewBytesFromMemory(@B2[0], Length(B2));

  CipherText := MLKEMEncrypt(EnKey, Msg, BytesToHex(R));
end;

function TCnMLKEM.GetCipherByteLength: Integer;
begin
  Result := GetCipherUByteLength + GetCipherVByteLength;
end;

function TCnMLKEM.GetCipherUByteLength: Integer;
begin
  Result := FMatrixSize * GetCipherUPolyByteLength;
end;

function TCnMLKEM.GetCipherVByteLength: Integer;
begin
  Result := CN_MLKEM_POLY_SIZE * FCompressV div 8;
end;

function TCnMLKEM.GetCipherUPolyByteLength: Integer;
begin
  Result := CN_MLKEM_POLY_SIZE * FCompressU div 8
end;

procedure TCnMLKEM.ExtractUVFromCipherText(const CipherText: TBytes;
  out UVector: TCnMLKEMPolyVector; out VPolynomial: TCnMLKEMPolynomial);
var
  I, ExpLen: Integer;
  B: TBytes;
  UC: TCnMLKEMPolyVector;
  VC: TCnMLKEMPolynomial;
begin
  ExpLen := GetCipherByteLength;
  if Length(CipherText) <> ExpLen then
    raise ECnMLKEMException.CreateFmt(SCnErrorMLKEMCipherLengthMismatch,
      [ExpLen, Length(CipherText)]);

  // 还原回 U
  SetLength(UVector, FMatrixSize);
  SetLength(UC, FMatrixSize);

  // 抽取整个 U 向量的多项式系数
  SetLength(B, GetCipherUPolyByteLength);
  for I := 0 to FMatrixSize - 1 do
  begin
    // 提取 U 中的一个多项式并解码
    Move(CipherText[I * Length(B)], B[0], Length(B));
    MLKEMByteDecode(B, FCompressU, UC[I]);
  end;

  // 解压缩向量 U
  MLKEMDecompressVector(UVector, UC, FCompressU);

  // 还原回多项式 VC
  SetLength(B, GetCipherVByteLength);
  Move(CipherText[GetCipherUByteLength], B[0], Length(B));
  MLKEMByteDecode(B, FCompressV, VC);

  // 解压缩多项式 V
  MLKEMDecompressPolynomial(VPolynomial, VC, FCompressV);
end;

procedure MLKEMPolynomialToInt64Polynomial(const Src: TCnMLKEMPolynomial; Dst: TCnInt64Polynomial);
var
  I: Integer;
begin
  Dst.MaxDegree := CN_MLKEM_POLY_SIZE - 1;
  for I := 0 to CN_MLKEM_POLY_SIZE - 1 do
    Dst[I] := Src[I];
  Dst.CorrectTop;
end;

procedure Int64PolynomialToMLKEMPolynomial(const Src: TCnInt64Polynomial; var Dst: TCnMLKEMPolynomial);
var
  I: Integer;
begin
  if Src.MaxDegree > CN_MLKEM_POLY_SIZE - 1 then
    raise ECnMLKEMException.Create(SCnErrorMLKEMInvalidPolynomialDegree);

  FillChar(Dst[0], SizeOf(TCnMLKEMPolynomial), 0);
  for I := 0 to Src.MaxDegree do
    Dst[I] := Src[I];
end;

procedure MLKEMPolynomialToINTT(var Res: TCnMLKEMPolynomial; const P: TCnMLKEMPolynomial);
var
  W: TWords;
begin
  SetLength(W, CN_MLKEM_POLY_SIZE);
  Move(P[0], W[0], Length(W) * SizeOf(Word));
  W := MLKEMINTT(W);
  Move(W[0], Res[0], Length(W) * SizeOf(Word));
end;

procedure MLKEMPolynomialToNTT(var Res: TCnMLKEMPolynomial; const P: TCnMLKEMPolynomial);
var
  W: TWords;
begin
  SetLength(W, CN_MLKEM_POLY_SIZE);
  Move(P[0], W[0], Length(W) * SizeOf(Word));
  W := MLKEMNTT(W);
  Move(W[0], Res[0], Length(W) * SizeOf(Word));
end;

procedure MLKEMPolynomialAdd(var Res: TCnMLKEMPolynomial; const P1, P2: TCnMLKEMPolynomial);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    Res[I] := MLKEMModAdd(P1[I], P2[I]);
end;

procedure MLKEMPolynomialSub(var Res: TCnMLKEMPolynomial; const P1, P2: TCnMLKEMPolynomial);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    Res[I] := MLKEMModSub(P1[I], P2[I]);
end;

procedure MLKEMPolynomialMul(var Res: TCnMLKEMPolynomial; const P1, P2: TCnMLKEMPolynomial;
  IsNTT: Boolean);
var
  I: Integer;
  C0, C1: Word;
  MP1, MP2, MP: TCnInt64Polynomial;

  procedure BaseCaseMultiply(A0, A1, B0, B1, Gamma: Word; out OC0, OC1: Word);
  begin
    // C0 = A0 * B0 + A1 * B1 * Gamma
    OC0 := MLKEMModAdd(MLKEMModMul(A0, B0), MLKEMModMul(MLKEMModMul(A1, B1), Gamma));

    // C1 = A0 * B1 + A1 * B0
    OC1 := MLKEMModAdd(MLKEMModMul(A0, B1), MLKEMModMul(A1, B0));
  end;

begin
  if IsNTT then
  begin
    for I := 0 to 127 do
    begin
      // 对每个二次分量进行乘法
      BaseCaseMultiply(P1[2 * I], P1[2 * I + 1], P2[2 * I], P2[2 * I + 1],
        ZETA_BASE_CASE[I], C0, C1);

      Res[2 * I] := C0;
      Res[2 * I + 1] := C1;
    end;
  end
  else
  begin
    MP1 := FInt64PolynomialPool.Obtain;
    MP2 := FInt64PolynomialPool.Obtain;
    MP := FInt64PolynomialPool.Obtain;

    try
      MLKEMPolynomialToInt64Polynomial(P1, MP1);
      MLKEMPolynomialToInt64Polynomial(P2, MP2);

      Int64PolynomialMul(MP, MP1, MP2);
      Int64PolynomialMod(MP, MP, FMLKEMRing);
      Int64PolynomialNonNegativeModWord(MP, CN_MLKEM_PRIME);

      Int64PolynomialToMLKEMPolynomial(MP, Res);
    finally
      FInt64PolynomialPool.Recycle(MP);
      FInt64PolynomialPool.Recycle(MP2);
      FInt64PolynomialPool.Recycle(MP1);
    end;
  end;
end;

procedure MLKEMVectorToNTT(var Res: TCnMLKEMPolyVector; const V: TCnMLKEMPolyVector);
var
  I: Integer;
begin
  for I := Low(V) to High(V) do
    MLKEMPolynomialToNTT(Res[I], V[I]);
end;

procedure MLKEMVectorToINTT(var Res: TCnMLKEMPolyVector; const V: TCnMLKEMPolyVector);
var
  I: Integer;
begin
  for I := Low(V) to High(V) do
    MLKEMPolynomialToINTT(Res[I], V[I]);
end;

procedure MLKEMVectorAdd(var Res: TCnMLKEMPolyVector; const P1, P2: TCnMLKEMPolyVector);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    MLKEMPolynomialAdd(Res[I], P1[I], P2[I]);
end;

procedure MLKEMMatrixVectorMul(var Res: TCnMLKEMPolyVector;
  const A: TCnMLKEMPolyMatrix; const S: TCnMLKEMPolyVector; IsNTT: Boolean);
var
  I, J: Integer;
  T: TCnMLKEMPolynomial;
begin
  SetLength(Res, Length(S));

  for I := Low(Res) to High(Res) do
  begin
    FillChar(Res[I][0], SizeOf(TCnMLKEMPolynomial), 0);
    for J := Low(S) to High(S) do
    begin
      // 多项式相乘
      MLKEMPolynomialMul(T, A[I, J], S[J], IsNTT);

      // 累加到结果中
      MLKEMPolynomialAdd(Res[I], Res[I], T);
    end;
  end;
end;

procedure MLKEMVectorDotProduct(var Res: TCnMLKEMPolynomial;
  const V1: TCnMLKEMPolyVector; const V2: TCnMLKEMPolyVector; IsNTT: Boolean);
var
  I: Integer;
  T: TCnMLKEMPolynomial;
begin
  FillChar(Res[0], SizeOf(TCnMLKEMPolynomial), 0);
  for I := Low(V1) to High(V1) do
  begin
    MLKEMPolynomialMul(T, V1[I], V2[I], IsNTT);
    MLKEMPolynomialAdd(Res, Res, T);
  end;
end;

initialization
  FInt64PolynomialPool := TCnInt64PolynomialPool.Create;

  // 初始化 MLKEM 的多项式环里的模多项式
  FMLKEMRing := TCnInt64Polynomial.Create;
  FMLKEMRing.MaxDegree := 256;
  FMLKEMRing[256] := 1;
  FMLKEMRing[0] := 1;

finalization
  FMLKEMRing.Free;
  FInt64PolynomialPool.Free;

end.

