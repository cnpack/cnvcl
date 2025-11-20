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

unit CnMLDSA;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：基于模块化格的数字签名算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 NIST 的 FIPS 204 规范中的 MLDSA
*          （Module-Lattice-based Digital Signature Algorithm，基于模块化格的数字签名算法）。
*
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.11.16 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs,
  CnNative, CnVector, CnPolynomial, CnRandom, CnBits, CnSHA2, CnSHA3, CnSM3;

const
  CN_MLDSA_KEY_SIZE    = 32;
  {* MLDSA 的种子等的长度}

  CN_MLDSA_DIGEST_SIZE = 64;
  {* MLDSA 的部分杂凑的长度}

  CN_MLDSA_POLY_SIZE   = 256;
  {* MLDSA 的多项式尺寸，最高次数是 255 次}

  CN_MLDSA_PRIME       = 8380417;
  {* MLDSA 使用的素数}

  CN_MLDSA_PRIME_INV   = 8347681;
  {* MLDSA 使用的 256 对该素数的模逆元}

  CN_MLDSA_DROPBIT     = 13;
  {* MLDSA 统一分离低 13 位，也就是规范中的 D}

  CN_MLDSA_DROPVALUE   = 1 shl (CN_MLDSA_DROPBIT - 1);
  {* MLDSA 中使用的 2^(D-1)}

  CN_MLDSA_PUBKEY_BIT  = 10;
  {* CN_MLDSA_PRIME - 1 的位数减 13 = 23- 13 = 10，也即规范中的 (bitlen (q-1)-d) 用于储存公钥}

type
  ECnMLDSAException = class(Exception);
  {* MLDSA 相关异常}

  TCnMLDSAType = (cmdt44, cmdt65, cmdt87);
  {* MLDSA 的三种实现规范}

  TCnMLDSAHashType = (cmhtNone, cmhtSHA224, cmhtSHA256, cmhtSHA384, cmhtSHA512,
    cmhtSHA512_224, cmhtSHA512_256, cmhtSHA3_224, cmhtSHA3_256, cmhtSHA3_384,
    cmhtSHA3_512, cmhtSHAKE128, cmhtSHAKE256, cmhtSM3);
  {* MLDSA 支持的杂凑算法}

  TCnMLDSASeed = array[0..CN_MLDSA_KEY_SIZE - 1] of Byte;
  {* MLDSA 种子，32 字节}

  TCnMLDSABlock = array[0..CN_MLDSA_KEY_SIZE - 1] of Byte;
  {* MLDSA 块数据，32 字节}

  TCnMLDSAKeyDigest = array[0..CN_MLDSA_DIGEST_SIZE - 1] of Byte;
  {* MLDSA 密钥的摘要字段，64 字节}

  TCnMLDSAPolynomial = array[0..CN_MLDSA_POLY_SIZE - 1] of Integer;
  {* MLDSA 多项式系数，256 个整数，用来表达一个多项式}

  TCnMLDSAPolyVector = array of TCnMLDSAPolynomial;
  {* MLDSA 多项式列表或叫向量，用来表达 T 或 S 等}

  TCnMLDSAPolyMatrix = array of TCnMLDSAPolyVector;
  {* MLDSA 多项式矩阵，用来表达 A}

  TCnMLDSAPrivateKey = class
  {* MLDSA 的私钥}
  private
    FGenerationSeed: TCnMLDSASeed;
    FKey: TCnMLDSASeed;
    FTrace: TCnMLDSAKeyDigest;
    FS1: TCnMLDSAPolyVector;
    FS2: TCnMLDSAPolyVector;
    FT0: TCnMLDSAPolyVector;
  public
    property GenerationSeed: TCnMLDSASeed read FGenerationSeed;
    {* 用于生成矩阵的随机种子，相当于规范里的 rho 象形 p}
    property Key: TCnMLDSASeed read FKey write FKey;
    {* 规范里的从杂凑结果中抽取的 K}
    property Trace: TCnMLDSAKeyDigest read FTrace write FTrace;
    {* 公钥流的 64 字节 SHAKE256 摘要}
    property S1: TCnMLDSAPolyVector read FS1 write FS1;
    {* 秘密多项式向量 S1，维度为矩阵列数，系数为非 NTT 形式}
    property S2: TCnMLDSAPolyVector read FS2 write FS2;
    {* 秘密多项式向量 S2，维度为矩阵行数，系数为非 NTT 形式}
    property T0: TCnMLDSAPolyVector read FT0 write FT0;
    {* 矩阵运算得到的多项式向量 T 的分离私钥部分 T0，维度为矩阵行数，系数为非 NTT 形式}
  end;

  TCnMLDSAPublicKey = class
  {* MLDSA 的公钥}
  private
    FGenerationSeed: TCnMLDSASeed;
    FT1: TCnMLDSAPolyVector;
  public
    property GenerationSeed: TCnMLDSASeed read FGenerationSeed;
    {* 用于生成矩阵的随机种子，相当于规范里的 p}
    property T1: TCnMLDSAPolyVector read FT1 write FT1;
    {* 矩阵运算得到的多项式向量 T 的分离公钥部分 T1，维度为矩阵行数}
  end;

  TCnMLDSA = class
  {* 基于模块化格的数字签名算法（Module-Lattice-based Digital Signature Algorithm）实现类}
  private
    FMLDSAType: TCnMLDSAType;
    FMatrixRowCount: Integer;
    FMatrixColCount: Integer;
    FNoise: Integer;
    FGamma1: Integer;
    FGamma2: Integer;
    FTau: Integer;
    FLambda: Integer;
    FBeta: Integer;
    FOmega: Integer;

    procedure GenerateMatrix(const Seed: TCnMLDSASeed; out Matrix: TCnMLDSAPolyMatrix);
    {* 根据种子生成矩阵 A，系数是 NTT 形式，FMatrixRowCount 行，FMatrixColCount 列}
    procedure GenerateSecret(const Seed: TCnMLDSAKeyDigest; out S1, S2: TCnMLDSAPolyVector);
    {* 根据种子生成两个秘密多项式向量，系数是非 NTT 形式。S1 维度 FMatrixColCount，S2 维度 FMatrixRowCount}
    procedure GenerateMask(const Dig: TBytes; Mu: Integer; out Mask: TCnMLDSAPolyVector);
    {* 根据种子生成掩码多项式向量}

    function GetNoiseBitLength: Integer;
    function HintBitPackVector(const H: TCnMLDSAPolyVector): TBytes;
    procedure HintBitUnpackVector(const Data: TBytes; var V: TCnMLDSAPolyVector);
    procedure UseHintVector(var Res: TCnMLDSAPolyVector; const H: TCnMLDSAPolyVector;
      const R: TCnMLDSAPolyVector);
  protected
    function CalcSignHashBytes(const Msg: TBytes; HashType: TCnMLDSAHashType;
      const Ctx: AnsiString): TBytes;
    {* 预计算待签名数据的杂凑并组合}
    function InternalSign(PrivateKey: TCnMLDSAPrivateKey; const Msg: TBytes;
      const Seed: TCnMLDSASeed): TBytes;
    {* 使用随机数与私钥进行内部签名}
    function InternalVerify(PublicKey: TCnMLDSAPublicKey; const Msg: TBytes;
      const Signature: TBytes): Boolean;
    {* 验证内部签名是否正确}

    function SigEncode(const C: TBytes; const Z: TCnMLDSAPolyVector;
      const H: TCnMLDSAPolyVector): TBytes;
    {* 将计算出的签名内容编码成字节数组}
    procedure SigDecode(const Signature: TBytes; out C: TBytes;
      out Z: TCnMLDSAPolyVector; out H: TCnMLDSAPolyVector);
    {* 从字节数组中加载签名内容}
  public
    constructor Create(AType: TCnMLDSAType); virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure GenerateKeys(PrivateKey: TCnMLDSAPrivateKey; PublicKey: TCnMLDSAPublicKey;
      const RandHex: string = '');
    {* 用一个真随机 32 字节种子，生成一对 Key，如果不传则内部产生随机数生成}

    procedure LoadPrivateKeyFromBytes(PrivateKey: TCnMLDSAPrivateKey; const SK: TBytes);
    {* 从字节数组中加载私钥，要求字节数组长度随三种规范不同分别是 2560、4032、4896}
    procedure LoadPublicKeyFromBytes(PublicKey: TCnMLDSAPublicKey; const PK: TBytes);
    {* 从字节数组中加载公钥，要求字节数组长度随三种规范不同分别是 1312、1952、2592}

    function SavePrivateKeyToBytes(PrivateKey: TCnMLDSAPrivateKey): TBytes;
    {* 将私钥保存成字节数组 SK，长度随三种规范不同分别是 2560、4032、4896}
    function SavePublicKeyToBytes(PublicKey: TCnMLDSAPublicKey): TBytes;
    {* 将公钥保存成字节数组 PK，长度随三种规范不同分别是 1312、1952、2592}

    function SignBytes(PrivateKey: TCnMLDSAPrivateKey; const Msg: TBytes;
      const Ctx: AnsiString = ''; HashType: TCnMLDSAHashType = cmhtNone;
      const RandHex: string = ''): TBytes;
    {* 使用私钥针对字节数组签名，Ctx 是附加字符串，长度不能大于 255。
       RandHex 可指定随机数，注意不传时，内部使用 32 个 0。
       签名字节数组长度随三种规范不同分别是 2420、3309、4627}

    function VerifyBytes(PublicKey: TCnMLDSAPublicKey; const Msg: TBytes;
      const Signature: TBytes; const Ctx: AnsiString = ''; HashType: TCnMLDSAHashType = cmhtNone): Boolean;
    {* 使用公钥验证字节数组的签名，Ctx 是附加字符串，长度不能大于 255。
       签名字节数组长度随三种规范不同分别是 2420、3309、4627}

    property MLDSAType: TCnMLDSAType read FMLDSAType;
    {* MLDSA 的算法类型，44、65、87 三种}
    property MatrixRowCount: Integer read FMatrixRowCount write FMatrixRowCount;
    {* 矩阵行数，对应规范中的 k}
    property MatrixColCount: Integer read FMatrixColCount write FMatrixColCount;
    {* 矩阵列数，对应规范中的 l}
    property Noise: Integer read FNoise write FNoise;
    {* 噪声参数，也就是私钥系数正负范围，对应规范中的 Eta}
    property Gamma1: Integer read FGamma1;
    {* 掩码向量 y 的系数范围 [-γ+1, γ]}
    property Gamma2: Integer read FGamma2;
    {* 低阶舍入范围}
    property Tau: Integer read FTau;
    {* 挑战多项式 c 中 ±1 系数的数量}
    property Lambda: Integer read FLambda;
    {* 挑战哈希的碰撞强度}
    property Beta: Integer read FBeta;
    {* β = τ · η}
    property Omega: Integer read FOmega;
    {* 提示 h 中 1 的最大数量}
  end;

// ============================ MLDSA 工具函数 =================================

procedure MLDSAPolynomialToINTT(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial);
{* 将 NTT 系数的 MKDSA 格式的多项式转换为非 NTT 系数的，俩参数可以相同。

   参数：
     Res: TCnMLDSAPolynomial              - 非 NTT 系数的 MKDSA 格式的多项式结果
     P: TCnMLDSAPolynomial                - 待转换的 NTT 系数的 MKDSA 格式的多项式

   返回值：（无）
}

procedure MLDSAPolynomialToNTT(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial);
{* 将非 NTT 系数的 MKDSA 格式的多项式转换为 NTT 系数的，俩参数可以相同。

   参数：
     Res: TCnMLDSAPolynomial              - NTT 系数的 MKDSA 格式的多项式结果
     P: TCnMLDSAPolynomial                - 待转换的非 NTT 系数的 MKDSA 格式的多项式

   返回值：（无）
}

procedure MLDSAPolynomialAdd(var Res: TCnMLDSAPolynomial;
  const P1: TCnMLDSAPolynomial; const P2: TCnMLDSAPolynomial);
{* 两个 MKDSA 格式的多项式在 mod 8380417 有限域中相加，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式和
     P1: TCnMLDSAPolynomial               - MKDSA 格式的多项式加数一
     P2: TCnMLDSAPolynomial               - MKDSA 格式的多项式加数二

   返回值：（无）
}

procedure MLDSAPolynomialSub(var Res: TCnMLDSAPolynomial;
  const P1: TCnMLDSAPolynomial; const P2: TCnMLDSAPolynomial);
{* 两个 MKDSA 格式的多项式在 mod 8380417 有限域中相减，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式差
     P1: TCnMLDSAPolynomial               - MKDSA 格式的多项式被减数
     P2: TCnMLDSAPolynomial               - MKDSA 格式的多项式减数

   返回值：（无）
}

procedure MLDSAPolynomialNeg(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial);
{* 一个 MKDSA 格式的多项式在 mod 8380417 有限域中取反。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式取反的结果
     P: TCnMLDSAPolynomial                - 待取反的 MKDSA 格式的多项式

   返回值：（无）
}

procedure MLDSAPolynomialMul(var Res: TCnMLDSAPolynomial; const P1, P2: TCnMLDSAPolynomial;
  IsNTT: Boolean = True); overload;
{* 两个 MKDSA 格式的多项式在 mod 8380417 及 x^256 + 1 的多项式环上相乘。
   IsNTT 指示参数是否是 NTT 模式，是则执行 NTT 乘法，不是则执行普通乘法。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式积
     P1: TCnMLDSAPolynomial               - MKDSA 格式的多项式乘数一
     P2: TCnMLDSAPolynomial               - MKDSA 格式的多项式乘数二
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

   返回值：（无）
}

procedure MLDSAPolynomialMul(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial;
  N: Integer); overload;
{* 一个 MKDSA 格式的多项式在 mod 8380417 上每个系数乘以一个整数 N。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式积
     P: TCnMLDSAPolynomial                - MKDSA 格式的多项式乘数
     N: Integer                           - 整数乘数

   返回值：（无）
}

procedure MLDSAVectorToNTT(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
{* 将非 NTT 系数的 MKDSA 格式的多项式向量就地转换为 NTT 系数的。

   参数：
     var Res: TCnMLDSAPolyVector          - MKDSA 格式的多项式向量结果
     V: TCnMLDSAPolyVector                - 待转换的 MKDSA 格式的多项式向量

   返回值：（无）
}

procedure MLDSAVectorToINTT(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
{* 将 NTT 系数的 MKDSA 格式的多项式向量就地转换为非 NTT 系数的。

   参数：
     var Res: TCnMLDSAPolyVector          - MKDSA 格式的多项式向量结果
     V: TCnMLDSAPolyVector                - 待转换的 MKDSA 格式的多项式向量

   返回值：（无）
}

procedure MLDSAVectorAdd(var Res: TCnMLDSAPolyVector;
  const V1: TCnMLDSAPolyVector; const V2: TCnMLDSAPolyVector);
{* 两个 MKDSA 格式的多项式向量在 mod 8380417 有限域中相加，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式向量和
     V1: TCnMLDSAPolynomial               - MKDSA 格式的多项式向量加数一
     V2: TCnMLDSAPolynomial               - MKDSA 格式的多项式向量加数二

   返回值：（无）
}

procedure MLDSAVectorSub(var Res: TCnMLDSAPolyVector;
  const V1: TCnMLDSAPolyVector; const V2: TCnMLDSAPolyVector);
{* 两个 MKDSA 格式的多项式向量在 mod 8380417 有限域中相减，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式向量差
     V1: TCnMLDSAPolynomial               - MKDSA 格式的多项式向量被减数
     V2: TCnMLDSAPolynomial               - MKDSA 格式的多项式向量减数

   返回值：（无）
}

procedure MLDSAVectorNeg(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
{* 一个 MKDSA 格式的多项式向量在 mod 8380417 有限域中取反。

   参数：
     Res: TCnMLDSAPolyVector              - MKDSA 格式的多项式向量取反的结果
     V: TCnMLDSAPolyVector                - 待取反的 MKDSA 格式的多项式向量

   返回值：（无）
}

procedure MLDSAPolynomialVectorMul(var Res: TCnMLDSAPolyVector;
  const C: TCnMLDSAPolynomial; const V: TCnMLDSAPolyVector; IsNTT: Boolean = True);
{* 一个 MKDSA 格式的多项式向量在 mod 8380417 及 x^256 + 1 的多项式环上乘以一个多项式，
   得到一个多项式向量。结果维度为 V 的维度。

   参数：
     Res: TCnMLDSAPolyVector              - MKDSA 格式的多项式积
     C: TCnMLDSAPolynomial                - MKDSA 格式的多项式向
     V: TCnMLDSAPolyVector                - MKDSA 格式的多项式
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

   返回值：（无）
}

procedure MLDSAMatrixVectorMul(var Res: TCnMLDSAPolyVector;
  const A: TCnMLDSAPolyMatrix; const S: TCnMLDSAPolyVector; IsNTT: Boolean = True);
{* 一个 MKDSA 格式的多项式矩阵在 mod 8380417 及 x^256 + 1 的多项式环上乘以一个多项式向量，
   得到一个多项式向量。用户需自行确保 A 的列数与 S 的维度一致，结果维度为 A 的行数。Res 不能是 S。

   参数：
     Res: TCnMLDSAPolyVector              - MKDSA 格式的多项式积
     A: TCnMLDSAPolyMatrix                - MKDSA 格式的多项式矩阵
     S: TCnMLDSAPolyVector                - MKDSA 格式的多项式向量
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

   返回值：（无）
}

procedure MLDSAVectorDotProduct(var Res: TCnMLDSAPolynomial;
  const V1: TCnMLDSAPolyVector; const V2: TCnMLDSAPolyVector; IsNTT: Boolean = True);
{* 两个 MKDSA 格式的多项式向量在 mod 8380417 及 x^256 + 1 的多项式环上点乘，
   得到一个多项式向量。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式点乘积
     V1: TCnMLDSAPolynomial               - MKDSA 格式的多项式乘数一
     V2: TCnMLDSAPolynomial               - MKDSA 格式的多项式乘数二
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

   返回值：（无）
}

implementation

resourcestring
  SCnErrorMLDSAInvalidParam = 'Invalid MLDSA Value';
  SCnErrorMLDSAInvalidRejNTT = 'Rej NTT Function Input Must Be 34 Bytes';
  SCnErrorMLDSAKeyLengthMismatch = 'MLDSA Key Length Mismatch. Expected %d, Got %d';
  SCnErrorMLDSAPackLengthMismatch = 'MLDSA Pack Length Mismatch. Expected %d, Got %d';
  SCnErrorMLDSASigLengthMismatch = 'MLDSA Signature Length Mismatch. Expected %d, Got %d';
  SCnErrorMLDSAInvalidHexLength = 'Invalid Random Hex Length';
  SCnErrorMLDSAInvalidMsgLength = 'Invalid Message Length';
  SCnErrorMLDSAInvalidCtxLength = 'Invalid Byte String Ctx Length';
  SCnErrorMLDSATooMany1sHint = 'Too Many 1s in Hint Vector';
  SCnErrorMLDSAHintExceedsLimit = 'Vector Exceeds Omega Limit';

const
  // FIPS 204 Appendix B 的 NTT 预计算值
  MLDSA_ZETA_NTT: array[0..255] of Integer = (
    0, 4808194, 3765607, 3761513, 5178923, 5496691, 5234739, 5178987,
    7778734, 3542485, 2682288, 2129892, 3764867, 7375178, 557458, 7159240,
    5010068, 4317364, 2663378, 6705802, 4855975, 7946292, 676590, 7044481,
    5152541, 1714295, 2453983, 1460718, 7737789, 4795319, 2815639, 2283733,
    3602218, 3182878, 2740543, 4793971, 5269599, 2101410, 3704823, 1159875,
    394148, 928749, 1095468, 4874037, 2071829, 4361428, 3241972, 2156050,
    3415069, 1759347, 7562881, 4805951, 3756790, 6444618, 6663429, 4430364,
    5483103, 3192354, 556856, 3870317, 2917338, 1853806, 3345963, 1858416,
    3073009, 1277625, 5744944, 3852015, 4183372, 5157610, 5258977, 8106357,
    2508980, 2028118, 1937570, 4564692, 2811291, 5396636, 7270901, 4158088,
    1528066, 482649, 1148858, 5418153, 7814814, 169688, 2462444, 5046034,
    4213992, 4892034, 1987814, 5183169, 1736313, 235407, 5130263, 3258457,
    5801164, 1787943, 5989328, 6125690, 3482206, 4197502, 7080401, 6018354,
    7062739, 2461387, 3035980, 621164, 3901472, 7153756, 2925816, 3374250,
    1356448, 5604662, 2683270, 5601629, 4912752, 2312838, 7727142, 7921254,
    348812, 8052569, 1011223, 6026202, 4561790, 6458164, 6143691, 1744507,
    1753, 6444997, 5720892, 6924527, 2660408, 6600190, 8321269, 2772600,
    1182243, 87208, 636927, 4415111, 4423672, 6084020, 5095502, 4663471,
    8352605, 822541, 1009365, 5926272, 6400920, 1596822, 4423473, 4620952,
    6695264, 4969849, 2678278, 4611469, 4829411, 635956, 8129971, 5925040,
    4234153, 6607829, 2192938, 6653329, 2387513, 4768667, 8111961, 5199961,
    3747250, 2296099, 1239911, 4541938, 3195676, 2642980, 1254190, 8368000,
    2998219, 141835, 8291116, 2513018, 7025525, 613238, 7070156, 6161950,
    7921677, 6458423, 4040196, 4908348, 2039144, 6500539, 7561656, 6201452,
    6757063, 2105286, 6006015, 6346610, 586241, 7200804, 527981, 5637006,
    6903432, 1994046, 2491325, 6987258, 507927, 7192532, 7655613, 6545891,
    5346675, 8041997, 2647994, 3009748, 5767564, 4148469, 749577, 4357667,
    3980599, 2569011, 6764887, 1723229, 1665318, 2028038, 1163598, 5011144,
    3994671, 8368538, 7009900, 3020393, 3363542, 214880, 545376, 7609976,
    3105558, 7277073, 508145, 7826699, 860144, 3430436, 140244, 6866265,
    6195333, 3123762, 2358373, 6187330, 5365997, 6663603, 2926054, 7987710,
    8077412, 3531229, 4405932, 4606686, 1900052, 7598542, 1054478, 7648983
  );

  MLDSA_INVALID_COEFF_HALFBYTE = $66;

  MLDSA_HASH_SHAKE128_SIZE     = 32;

  MLDSA_HASH_SHAKE256_SIZE     = 64;

  OID_MLDSA_PREHASH_SHA224: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.4
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $04
  );

  OID_MLDSA_PREHASH_SHA256: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.1
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $01
  );

  OID_MLDSA_PREHASH_SHA384: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.2
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $02
  );

  OID_MLDSA_PREHASH_SHA512: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.3
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $03
  );

  OID_MLDSA_PREHASH_SHA512_224: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.5
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $05
  );

  OID_MLDSA_PREHASH_SHA512_256: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.6
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $06
  );

  OID_MLDSA_PREHASH_SHA3_224: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.7
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $07
  );

  OID_MLDSA_PREHASH_SHA3_256: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.8
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $08
  );

  OID_MLDSA_PREHASH_SHA3_384: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.9
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $09
  );

  OID_MLDSA_PREHASH_SHA3_512: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.10
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $0A
  );

  OID_MLDSA_PREHASH_SHAKE128: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.11
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $0B
  );

  OID_MLDSA_PREHASH_SHAKE256: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.12
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $0C
  );

  OID_MLDSA_PREHASH_SM3: array[0..7] of Byte = ( // 1.0.10118.3.0.65
    $06, $06, $28, $CF, $06, $03, $00, $41
  );

// ================================ MLDSA ======================================

// 模素数加减乘法
function MLDSAModAdd(A, B: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Int64NonNegativeAddMod(A, B, CN_MLDSA_PRIME);
end;

function MLDSAModSub(A, B: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Int64NonNegativeAddMod(A, -B, CN_MLDSA_PRIME);
end;

function MLDSAModMul(A, B: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Int64NonNegativeMulMod(A, B, CN_MLDSA_PRIME);
end;

// 将整数 A 模中心化，也就是映射到 [-(q-1)/2, (Q-1)/2] 范围内
function MLDSACenterMod(A: Integer): Integer;
begin
  Result := A mod CN_MLDSA_PRIME;
  if Result < 0 then
    Result := Result + CN_MLDSA_PRIME;
  if Result > (CN_MLDSA_PRIME - 1) div 2 then
    Result := Result - CN_MLDSA_PRIME;
end;

// MLDSA 使用的特定数论变换
function MLDSANTT(const F: TIntegers): TIntegers;
var
  Len, Start, J, I: Integer;
  Zeta, T: Integer;
begin
  SetLength(Result, CN_MLDSA_POLY_SIZE);
  Move(F[0], Result[0], CN_MLDSA_POLY_SIZE * SizeOf(Integer));

  I := 0;
  Len := 128;

  while Len >= 1 do
  begin
    Start := 0;
    while Start < CN_MLDSA_POLY_SIZE do
    begin
      Inc(I);
      Zeta := MLDSA_ZETA_NTT[I];

      for J := Start to Start + Len - 1 do
      begin
        T := MLDSAModMul(Zeta, Result[J + Len]);
        Result[J + Len] := MLDSAModSub(Result[J], T);
        Result[J] := MLDSAModAdd(Result[J], T);
      end;

      Inc(Start, 2 * Len);
    end;

    Len := Len div 2;
  end;
end;

function MLDSAINTT(const F: TIntegers): TIntegers;
var
  Len, Start, J, I: Integer;
  Zeta, T: Integer;
begin
  SetLength(Result, CN_MLDSA_POLY_SIZE);
  Move(F[0], Result[0], CN_MLDSA_POLY_SIZE * SizeOf(Integer));

  I := 256;
  Len := 1;

  while Len < 256 do
  begin
    Start := 0;
    while Start < CN_MLDSA_POLY_SIZE do
    begin
      Dec(I);
      Zeta := -MLDSA_ZETA_NTT[I];

      for J := Start to Start + Len - 1 do
      begin
        T := Result[J];
        Result[J] := MLDSAModAdd(T, Result[J + Len]);
        Result[J + Len] := MLDSAModMul(Zeta, MLDSAModSub(T, Result[J + Len]));
      end;

      Inc(Start, 2 * Len);
    end;

    Len := Len * 2;
  end;

  // 最终缩放：乘以 8347681，是 256 对 8380417 的模逆元
  for J := 0 to CN_MLDSA_POLY_SIZE - 1 do
    Result[J] := MLDSAModMul(Result[J], CN_MLDSA_PRIME_INV);
end;

procedure MLDSAPolynomialToINTT(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial);
var
  W: TIntegers;
begin
  SetLength(W, CN_MLDSA_POLY_SIZE);
  Move(P[0], W[0], Length(W) * SizeOf(Integer));
  W := MLDSAINTT(W);
  Move(W[0], Res[0], Length(W) * SizeOf(Integer));
end;

procedure MLDSAPolynomialToNTT(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial);
var
  W: TIntegers;
begin
  SetLength(W, CN_MLDSA_POLY_SIZE);
  Move(P[0], W[0], Length(W) * SizeOf(Integer));
  W := MLDSANTT(W);
  Move(W[0], Res[0], Length(W) * SizeOf(Integer));
end;

procedure MLDSAPolynomialAdd(var Res: TCnMLDSAPolynomial; const P1, P2: TCnMLDSAPolynomial);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    Res[I] := MLDSAModAdd(P1[I], P2[I]);
end;

procedure MLDSAPolynomialSub(var Res: TCnMLDSAPolynomial; const P1, P2: TCnMLDSAPolynomial);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    Res[I] := MLDSAModSub(P1[I], P2[I]);
end;

procedure MLDSAPolynomialNeg(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial);
var
  I: Integer;
begin
  for I := Low(P) to High(P) do
    Res[I] := MLDSAModSub(0, P[I]);  // 0 - P[I] mod q
end;

procedure MLDSAPolynomialMul(var Res: TCnMLDSAPolynomial; const P1, P2: TCnMLDSAPolynomial;
  IsNTT: Boolean);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    Res[I] := MLDSAModMul(P1[I], P2[I]);
end;

procedure MLDSAPolynomialMul(var Res: TCnMLDSAPolynomial; const P: TCnMLDSAPolynomial;
  N: Integer);
var
  I: Integer;
begin
  for I := Low(P) to High(P) do
    Res[I] := MLDSAModMul(P[I], N);
end;

procedure MLDSAVectorToNTT(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  if (Res <> V) and (Length(Res) <> Length(V)) then
    SetLength(Res, Length(V));

  for I := Low(V) to High(V) do
    MLDSAPolynomialToNTT(Res[I], V[I]);
end;

procedure MLDSAVectorToINTT(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  if (Res <> V) and (Length(Res) <> Length(V)) then
    SetLength(Res, Length(V));

  for I := Low(V) to High(V) do
    MLDSAPolynomialToINTT(Res[I], V[I]);
end;

procedure MLDSAVectorAdd(var Res: TCnMLDSAPolyVector; const V1, V2: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  for I := Low(V1) to High(V1) do
    MLDSAPolynomialAdd(Res[I], V1[I], V2[I]);
end;

procedure MLDSAVectorSub(var Res: TCnMLDSAPolyVector; const V1, V2: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  for I := Low(V1) to High(V1) do
    MLDSAPolynomialSub(Res[I], V1[I], V2[I]);
end;

procedure MLDSAVectorNeg(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  if (Res <> V) and (Length(Res) <> Length(V)) then
    SetLength(Res, Length(V));

  for I := Low(V) to High(V) do
    MLDSAPolynomialNeg(Res[I], V[I]);
end;

procedure MLDSAPolynomialVectorMul(var Res: TCnMLDSAPolyVector;
  const C: TCnMLDSAPolynomial; const V: TCnMLDSAPolyVector; IsNTT: Boolean = True);
var
  I: Integer;
begin
  if (Res <> V) and (Length(Res) <> Length(V)) then
    SetLength(Res, Length(V));

  for I := Low(V) to High(V) do
    MLDSAPolynomialMul(Res[I], C, V[I], IsNTT);
end;

procedure MLDSAMatrixVectorMul(var Res: TCnMLDSAPolyVector;
  const A: TCnMLDSAPolyMatrix; const S: TCnMLDSAPolyVector; IsNTT: Boolean);
var
  I, J: Integer;
  T: TCnMLDSAPolynomial;
begin
  SetLength(Res, Length(A)); // 结果向量维度是矩阵行数

  for I := Low(Res) to High(Res) do  // 针对矩阵每一行
  begin
    FillChar(Res[I][0], SizeOf(TCnMLDSAPolynomial), 0);
    for J := Low(S) to High(S) do    // 针对矩阵一行中的每个元素及向量中的每个维度
    begin
      // 多项式相乘
      MLDSAPolynomialMul(T, A[I, J], S[J], IsNTT);

      // 累加到结果中
      MLDSAPolynomialAdd(Res[I], Res[I], T);
    end;
  end;
end;

procedure MLDSAVectorDotProduct(var Res: TCnMLDSAPolynomial;
  const V1: TCnMLDSAPolyVector; const V2: TCnMLDSAPolyVector; IsNTT: Boolean);
var
  I: Integer;
  T: TCnMLDSAPolynomial;
begin
  FillChar(Res[0], SizeOf(TCnMLDSAPolynomial), 0);
  for I := Low(V1) to High(V1) do
  begin
    MLDSAPolynomialMul(T, V1[I], V2[I], IsNTT);
    MLDSAPolynomialAdd(Res, Res, T);
  end;
end;

// 计算 t1·2^d
procedure MLDSAPolynomialVectorScaleByPower2(var Res: TCnMLDSAPolyVector;
  const V: TCnMLDSAPolyVector; D: Integer);
var
  I: Integer;
  ScaleFactor: Integer;
begin
  // 计算缩放因子 2^d mod q
  ScaleFactor := 1;
  for I := 1 to D do
    ScaleFactor := (ScaleFactor * 2) mod CN_MLDSA_PRIME;

  SetLength(Res, Length(V));
  for I := Low(V) to High(V) do
    MLDSAPolynomialMul(Res[I], V[I], ScaleFactor);
end;

// 根据仨字节构造整数，返回 -1 表示失败
function MLDSACoeffFromThreeBytes(B0, B1, B2: Byte): Integer;
begin
  if B2 > 127 then
    B2 := B2 - 128;

  Result := (Integer(B2) shl 16) or (Integer(B1) shl 8) or B0;
  if Result >= CN_MLDSA_PRIME then
    Result := -1;
end;

function MLDSACoeffFromHalfByte(B: Byte; Eta: Integer): Integer;
begin
  if (Eta = 2) and (B < 15) then
    Result := 2 - (B mod 5)        // -2 ~ 2
  else
  begin
    if (Eta = 4) and (B < 9) then
      Result := 4 - B              // -4 ~ 4
    else
      Result := MLDSA_INVALID_COEFF_HALFBYTE;
  end;
end;

function MLDSARejNTTPoly(const RandBytes: TBytes): TIntegers;
var
  Ctx: TCnSHA3Context;
  C: TBytes;
  D, J: Integer;
begin
  if Length(RandBytes) < CN_MLDSA_KEY_SIZE + 2 then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidRejNTT);

  SetLength(Result, CN_MLDSA_POLY_SIZE);

  SHAKE128Init(Ctx, 0);
  SHAKE128Absorb(Ctx, PAnsiChar(@RandBytes[0]), Length(RandBytes));

  J := 0;
  while J < CN_MLDSA_POLY_SIZE do
  begin
    C := SHAKE128Squeeze(Ctx, 3);
    D := MLDSACoeffFromThreeBytes(C[0], C[1], C[2]);
    if D < 0 then
      Continue;

    Result[J] := D;
    Inc(J);
  end;
end;

function MLDSARejBoundedPoly(const RandBytes: TBytes; Eta: Integer): TIntegers;
var
  Ctx: TCnSHA3Context;
  J: Integer;
  Z: TBytes;
  Z0, Z1: ShortInt;
begin
  if Length(RandBytes) < CN_MLDSA_KEY_SIZE + 2 then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidRejNTT);

  SetLength(Result, CN_MLDSA_POLY_SIZE);

  SHAKE256Init(Ctx, 0);
  SHAKE256Absorb(Ctx, PAnsiChar(@RandBytes[0]), Length(RandBytes));

  J := 0;
  while J < CN_MLDSA_POLY_SIZE do
  begin
    Z := SHAKE256Squeeze(Ctx, 1);
    Z0 := MLDSACoeffFromHalfByte(Z[0] and $0F, Eta);
    Z1 := MLDSACoeffFromHalfByte(Z[0] shr 4, Eta);

    if Z0 <> MLDSA_INVALID_COEFF_HALFBYTE then
    begin
      Result[J] := Z0;
      Inc(J);
    end;
    if (Z1 <> MLDSA_INVALID_COEFF_HALFBYTE) and (J < CN_MLDSA_POLY_SIZE) then
    begin
      Result[J] := Z1;
      Inc(J);
    end;
  end;
end;

// 取 R 的低 D 位（13 位）放 R0，其余的放 R1
procedure MLDSAPower2Round(R: Integer; out R0, R1: Integer);
var
  T: Integer;
  HD: Integer;
begin
  T := R mod CN_MLDSA_PRIME;
  if T < 0 then
    T := T + CN_MLDSA_PRIME;

  HD := 1 shl (CN_MLDSA_DROPBIT - 1);  // 4096
  R0 := T and ((1 shl CN_MLDSA_DROPBIT) - 1);  // [0, 8191]

  if R0 >= HD then
    R0 := R0 - (1 shl CN_MLDSA_DROPBIT);  // [-4096, -1]

  R1 := (T - R0) shr CN_MLDSA_DROPBIT;    // -4096 可能有问题
end;

procedure MLDSAPower2RoundPolynomial(const R: TCnMLDSAPolynomial; var R0, R1: TCnMLDSAPolynomial);
var
  I: Integer;
begin
  for I := Low(R) to High(R) do
    MLDSAPower2Round(R[I], R0[I], R1[I]);
end;

procedure MLDSAPower2RoundVector(const V: TCnMLDSAPolyVector; var V0, V1: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  if Length(V) <= 0 then
    Exit;

  SetLength(V0, Length(V));
  SetLength(V1, Length(V));

  for I := Low(V) to High(V) do
    MLDSAPower2RoundPolynomial(V[I], V0[I], V1[I]);
end;

procedure MLDSADecompose(R, Gamma: Integer; out R1, R0: Integer);
var
  RP: Integer;
  Alpha: Integer;
begin
  Alpha := 2 * Gamma;

  // 标准化到 [0, q)
  RP := R mod CN_MLDSA_PRIME;
  if RP < 0 then
    RP := RP + CN_MLDSA_PRIME;

  // 计算 r0，映射到 (-γ, γ]
  R0 := RP mod Alpha;

  // 确保 r0 ∈ (-γ, γ]，当 r0 > γ 时，调整为 r0 - α
  if R0 > Gamma then
    R0 := R0 - Alpha;

  R1 := (RP - R0) div Alpha;

  // 边界情况处理，当 r - r0 = q - 1 时，设置 r1 = 0, r0 = r0 - 1
  if (RP - R0) = (CN_MLDSA_PRIME - 1) then
  begin
    R1 := 0;
    R0 := R0 - 1;
  end;
end;

function MLDSAHighBits(R, Gamma: Integer): Integer;
var
  R1, R0: Integer;
begin
  MLDSADecompose(R, Gamma, R1, R0);
  Result := R1;
end;

function MLDSALowBits(R, Gamma: Integer): Integer;
var
  R1, R0: Integer;
begin
  MLDSADecompose(R, Gamma, R1, R0);
  Result := R0;
end;

function MLDSAMakeHint(Z, R, Gamma: Integer): Boolean;
var
  R1, V1: Integer;
begin
  R1 := MLDSAHighBits(R, Gamma);
  V1 := MLDSAHighBits(R + Z, Gamma);
  Result := (R1 <> V1);
end;

procedure MLDSAMakeHintPolynomial(var Res: TCnMLDSAPolynomial;
  const Z, R: TCnMLDSAPolynomial; Gamma: Integer);
var
  I: Integer;
begin
  for I := Low(Z) to High(Z) do
  begin
    if MLDSAMakeHint(Z[I], R[I], Gamma) then
      Res[I] := 1
    else
      Res[I] := 0;
  end;
end;

procedure MLDSAMakeHintVector(var Res: TCnMLDSAPolyVector;
  const Z, R: TCnMLDSAPolyVector; Gamma: Integer);
var
  I, J: Integer;
begin
  SetLength(Res, Length(Z));

  for I := Low(Z) to High(Z) do
  begin
    for J := Low(Z[I]) to High(Z[I]) do
    begin
      if MLDSAMakeHint(Z[I][J], R[I][J], Gamma) then
        Res[I][J] := 1
      else
        Res[I][J] := 0;
    end;
  end;
end;

function MLDSAUseHint(H: Boolean; R, Gamma: Integer): Integer;
var
  M: Integer;
  R1, R0: Integer;
begin
  M := (CN_MLDSA_PRIME - 1) div (2 * Gamma);
  MLDSADecompose(R, Gamma, R1, R0);

  if H then
  begin
    if R0 > 0 then
      Result := (R1 + 1) mod M
    else
      Result := (R1 - 1) mod M;
  end
  else
  begin
    Result := R1;
  end;

  if Result < 0 then // 不能返回负值
    Result := Result + M;
end;

procedure MLDSADecomposePolynomial(const R: TCnMLDSAPolynomial; Gamma: Integer;
  out R1, R0: TCnMLDSAPolynomial);
var
  I: Integer;
begin
  for I := Low(R) to High(R) do
    MLDSADecompose(R[I], Gamma, R1[I], R0[I]);
end;

procedure MLDSADecomposeVector(const V: TCnMLDSAPolyVector;
  Gamma: Integer; out V1, V0: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  if (V0 <> V) and (Length(V0) <> Length(V)) then
    SetLength(V0, Length(V));
  if (V1 <> V) and (Length(V1) <> Length(V)) then
    SetLength(V1, Length(V));
  
  for I := Low(V) to High(V) do
    MLDSADecomposePolynomial(V[I], Gamma, V1[I], V0[I]);
end;

procedure MLDSAHighBitsPolynomial(var Res: TCnMLDSAPolynomial; const R: TCnMLDSAPolynomial;
  Gamma: Integer);
var
  I: Integer;
begin
  for I := Low(R) to High(R) do
    Res[I] := MLDSAHighBits(R[I], Gamma);
end;

procedure MLDSAHighBitsVector(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector;
  Gamma: Integer);
var
  I: Integer;
begin
  if (Res <> V) and (Length(Res) <> Length(V)) then
    SetLength(Res, Length(V));

  for I := Low(V) to High(V) do
    MLDSAHighBitsPolynomial(Res[I], V[I], Gamma);
end;

procedure MLDSALowBitsPolynomial(var Res: TCnMLDSAPolynomial; const R: TCnMLDSAPolynomial;
  Gamma: Integer);
var
  I: Integer;
begin
  for I := Low(R) to High(R) do
    Res[I] := MLDSALowBits(R[I], Gamma);
end;

procedure MLDSALowBitsVector(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector;
  Gamma: Integer);
var
  I: Integer;
begin
  if (Res <> V) and (Length(Res) <> Length(V)) then
    SetLength(Res, Length(V));

  for I := Low(V) to High(V) do
    MLDSALowBitsPolynomial(Res[I], V[I], Gamma);
end;

// 多项式系数低 D 位打包，注意 256 个数字，无论 D 多少，
// 输出都是 8 的整数因而也就是整数字节
function MLDSASimpleBitPackPolynomial(const P: TCnMLDSAPolynomial; D: Integer): TBytes;
var
  I: Integer;
  B: TCnBitBuilder;
begin
  B := TCnBitBuilder.Create;
  try
    for I := Low(P) to High(P) do
      B.AppendDWordRange(P[I], D - 1);
    Result := B.ToBytes;
  finally
    B.Free;
  end;
end;

// 多项式向量系数低 D 位打包
function MLDSASimpleBitPackVector(const V: TCnMLDSAPolyVector; D: Integer): TBytes;
var
  I, J: Integer;
  B: TCnBitBuilder;
begin
  B := TCnBitBuilder.Create;
  try
    for I := Low(V) to High(V) do
    begin
      for J := Low(V[I]) to High(V[I]) do
        B.AppendDWordRange(V[I][J], D - 1);
    end;
    Result := B.ToBytes;
  finally
    B.Free;
  end;
end;

procedure MLDSASimpleBitUnpackPolynomial(const Data: TBytes;
  P: TCnMLDSAPolynomial; D: Integer);
var
  B: TCnBitBuilder;
  I: Integer;
begin
  if Length(Data) <> 32 * D then
    raise ECnMLDSAException.CreateFmt(SCnErrorMLDSAPackLengthMismatch,
      [32 * D, Length(Data)]);

  B := TCnBitBuilder.Create;
  try
    B.SetBytes(Data);
    for I := Low(P) to High(P) do
      P[I] := B.Copy(D * I, D);
  finally
    B.Free;
  end;
end;

procedure MLDSASimpleBitUnpackVector(const Data: TBytes;
  V: TCnMLDSAPolyVector; D: Integer);
var
  I, J, T: Integer;
  B: TCnBitBuilder;
begin
  if Length(Data) <> 32 * D * Length(V) then
    raise ECnMLDSAException.CreateFmt(SCnErrorMLDSAPackLengthMismatch,
      [32 * D * Length(V), Length(Data)]);

  B := TCnBitBuilder.Create;
  try
    B.SetBytes(Data);
    T := 0;
    for I := Low(V) to High(V) do
    begin
      for J := Low(V[I]) to High(V[I]) do
      begin
        V[I][J] := B.Copy(T, D);
        Inc(T, D);
      end;
    end;
  finally
    B.Free;
  end;
end;

// 多项式系数打包到 [-A, B] 范围内
function MLDSABitPackPolynomial(const P: TCnMLDSAPolynomial; A, B: Integer): TBytes;
var
  I, W, L: Integer;
  T: TCnBitBuilder;
begin
  T := TCnBitBuilder.Create;
  try
    for I := Low(P) to High(P) do
    begin
      W := B - P[I];
      L := GetUInt32BitLength(A + B);
      T.AppendDWordRange(W, L - 1);
    end;
    Result := T.ToBytes;
  finally
    T.Free;
  end;
end;

function MLDSABitPackVector(const V: TCnMLDSAPolyVector; A, B: Integer): TBytes;
var
  I, J, W, L: Integer;
  T: TCnBitBuilder;
begin
  T := TCnBitBuilder.Create;
  try
    for I := Low(V) to High(V) do
    begin
      for J := Low(V[I]) to High(V[I]) do
      begin
        W := B - V[I][J];
        L := GetUInt32BitLength(A + B);
        T.AppendDWordRange(W, L - 1);
      end;
    end;
    Result := T.ToBytes;
  finally
    T.Free;
  end;
end;

procedure MLDSABitUnpackPolynomial(const Data: TBytes;
  out P: TCnMLDSAPolynomial; A, B: Integer);
var
  T: TCnBitBuilder;
  I, L: Integer;
begin
  L := 32 * GetUInt32BitLength(A + B);
  if Length(Data) <> L then
    raise ECnMLDSAException.CreateFmt(SCnErrorMLDSAPackLengthMismatch,
      [L, Length(Data)]);

  L := GetUInt32BitLength(A + B);
  T := TCnBitBuilder.Create;
  try
    T.SetBytes(Data);
    for I := Low(P) to High(P) do
      P[I] := B - T.Copy(L * I, L);
  finally
    T.Free;
  end;
end;

procedure MLDSABitUnpackVector(const Data: TBytes;
  var V: TCnMLDSAPolyVector; A, B: Integer);
var
  I, J, T, L: Integer;
  D: TCnBitBuilder;
begin
  L := 32 * (GetUInt32BitLength(A + B)) * Length(V);
  if Length(Data) <> L then
    raise ECnMLDSAException.CreateFmt(SCnErrorMLDSAPackLengthMismatch,
      [L, Length(Data)]);

  L := GetUInt32BitLength(A + B);
  D := TCnBitBuilder.Create;
  try
    D.SetBytes(Data);
    T := 0;
    for I := Low(V) to High(V) do
    begin
      for J := Low(V[I]) to High(V[I]) do
      begin
        V[I][J] := B - D.Copy(T, L);
        Inc(T, L);
      end;
    end;
  finally
    D.Free;
  end;
end;

function MLDSAW1Encode(const W1: TCnMLDSAPolyVector; Gamma: Integer): TBytes;
var
  I, U: Integer;
begin
  Result := nil;

  // 计算系数的上限: (q-1)/(2γ2)
  U := (CN_MLDSA_PRIME - 1) div (2 * Gamma) - 1;

  // 对向量中的每个多项式进行编码
  for I := Low(W1) to High(W1) do
    Result := ConcatBytes(Result, MLDSASimpleBitPackPolynomial(W1[I], GetUInt32BitLength(U)));
end;

procedure MLDSASampleInBall(const Seed: TBytes; Tau: Integer; out Res: TCnMLDSAPolynomial);
var
  Ctx: TCnSHA3Context;
  S: TBytes;
  H: array[0..63] of Byte;
  I, J: Integer;
  JByte: Byte;
begin
  FillChar(Res[0], SizeOf(TCnMLDSAPolynomial), 0);

  SHAKE256Init(Ctx, 0);
  SHAKE256Absorb(Ctx, PAnsiChar(@Seed[0]), Length(Seed));

  S := SHAKE256Squeeze(Ctx, 8);
  for I := 0 to 7 do
  begin
    for J := 0 to 7 do
    begin
      if I * 8 + J < 64 then
        H[I * 8 + J] := (S[I] shr J) and 1;
    end;
  end;

  for I := 256 - Tau to 255 do
  begin
    S := SHAKE256Squeeze(Ctx, 1);
    JByte := S[0];

    // 拒绝采样，确保 j ≤ i
    while JByte > I do
    begin
      S := SHAKE256Squeeze(Ctx, 1);
      JByte := S[0];
    end;

    J := JByte;
    Res[I] := Res[J];

    if H[I + Tau - 256] = 0 then
      Res[J] := 1      // (-1)^0 = 1
    else
      Res[J] := -1;    // (-1)^1 = -1
  end;
end;

function VectorInfinityNorm(const V: TCnMLDSAPolyVector): Integer;
var
  I, J: Integer;
  AV: Integer;
begin
  Result := 0;
  for I := Low(V) to High(V) do
  begin
    for J := Low(V[I]) to High(V[I]) do
    begin
      // 计算系数的绝对值
      AV := Abs(V[I][J]);
      if AV > CN_MLDSA_PRIME div 2 then
        AV := CN_MLDSA_PRIME - AV;

      if AV > Result then
        Result := AV;
    end;
  end;
end;

function MLDSAVectorCountOne(const V: TCnMLDSAPolyVector): Integer;
var
  I, J: Integer;
begin
  Result := 0;
  for I := Low(V) to High(V) do
  begin
    for J := Low(V[I]) to High(V[I]) do
    begin
      if V[I][J] <> 0 then
        Inc(Result);
    end;
  end;
end;

procedure MLDSAVectorCenterMod(var Res: TCnMLDSAPolyVector;
  const V: TCnMLDSAPolyVector);
var
  I, J: Integer;
begin
  if (Res <> V) and (Length(Res) <> Length(V)) then
    SetLength(Res, Length(V));
  
  for I := Low(V) to High(V) do
  begin
    for J := Low(V[I]) to High(V[I]) do
      Res[I][J] := MLDSACenterMod(V[I][J]);
  end;
end;

function MLDSAHFunc(const Data: TBytes; DigestLen: Integer = CN_MLDSA_DIGEST_SIZE): TBytes;
begin
  Result := SHAKE256Bytes(Data, DigestLen);
end;

function MLDSAGFunc(const Data: TBytes; DigestLen: Integer = CN_MLDSA_DIGEST_SIZE): TBytes;
begin
  Result := SHAKE128Bytes(Data, DigestLen);
end;

{ TCnMLDSA }

function TCnMLDSA.HintBitPackVector(const H: TCnMLDSAPolyVector): TBytes;
var
  Index, I, J: Integer;
begin
  // 结果长度为 ω + k
  SetLength(Result, FOmega + Length(H));

  FillChar(Result[0], Length(Result), 0);
  Index := 0;
  for I := Low(H) to High(H) do
  begin
    for J := Low(H[I]) to High(H[I]) do
    begin
      // 如果提示位为 1，记录位置
      if H[I][J] <> 0 then
      begin
        if Index >= FOmega then
          raise ECnMLDSAException.Create(SCnErrorMLDSATooMany1sHint);

        Result[Index] := Byte(J);
        Inc(Index);
      end;
    end;

    Result[FOmega + I] := Byte(Index);
  end;

  // 确保没有超出ω的限制
  if Index > FOmega then
    raise ECnMLDSAException.Create(SCnErrorMLDSAHintExceedsLimit);
end;

procedure TCnMLDSA.HintBitUnpackVector(const Data: TBytes; var V: TCnMLDSAPolyVector);
var
  Y: TBytes;
  Index, First, I: Integer;
begin
  // 验证数据长度
  if Length(Data) <> FOmega + FMatrixRowCount then
  begin
    V := nil;
    Exit;
  end;

  Y := Data;
  SetLength(V, FMatrixRowCount);
  for I := 0 to FMatrixRowCount - 1 do
    FillChar(V[I][0], SizeOf(TCnMLDSAPolynomial), 0);

  Index := 0;
  for I := 0 to FMatrixRowCount - 1 do
  begin
    // 检查索引范围是否有效
    if (Y[FOmega + I] < Index) or (Y[FOmega + I] > FOmega) then
    begin
      V := nil;
      Exit;
    end;

    First := Index;

    // 解码当前多项式的1位置
    while Index < Y[FOmega + I] do
    begin
      // 检查位置是否有序（递增）
      if (Index > First) and (Y[Index - 1] >= Y[Index]) then
      begin
        V := nil;
        Exit;
      end;

      // 设置对应系数为 1
      V[I][Y[Index]] := 1;
      Inc(Index);
    end;
  end;

  // 检查剩余字节是否为零
  for I := Index to FOmega - 1 do
  begin
    if Y[I] <> 0 then
    begin
      V := nil;
      Exit;
    end;
  end;
end;

procedure TCnMLDSA.UseHintVector(var Res: TCnMLDSAPolyVector;
  const H: TCnMLDSAPolyVector; const R: TCnMLDSAPolyVector);
var
  I, J: Integer;
begin
  SetLength(Res, Length(H));
  for I := Low(H) to High(H) do
  begin
    for J := Low(H[I]) to High(H[I]) do
      Res[I][J] := MLDSAUseHint(H[I][J] <> 0, R[I][J], FGamma2);
  end;
end;

function TCnMLDSA.CalcSignHashBytes(const Msg: TBytes;
  HashType: TCnMLDSAHashType; const Ctx: AnsiString): TBytes;
var
  DigShake: TBytes;
  DigSha224: TCnSHA224Digest;
  DigSha256: TCnSHA256Digest;
  DigSha384: TCnSHA384Digest;
  DigSha512: TCnSHA512Digest;
  DigSha512_224: TCnSHA512_224Digest;
  DigSha512_256: TCnSHA512_256Digest;
  DigSha3_224: TCnSHA3_224Digest;
  DigSha3_256: TCnSHA3_256Digest;
  DigSha3_384: TCnSHA3_384Digest;
  DigSha3_512: TCnSHA3_512Digest;
  DigSm3: TCnSM3Digest;
begin
  case HashType of
    cmhtNone:
      begin
        SetLength(Result, 2 + Length(Ctx) + Length(Msg));
        Result[0] := 0;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));
        Move(Msg[0], Result[2 + Length(Ctx)], Length(Msg));
      end;
    cmhtSHA224:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA224) + Length(Ctx) + SizeOf(TCnSHA224Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA224[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA224));

        DigSha224 := SHA224Bytes(Msg);
        Move(DigSha224[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA224)], SizeOf(TCnSHA224Digest));
      end;
    cmhtSHA256:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA256) + Length(Ctx) + SizeOf(TCnSHA256Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA256[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA256));

        DigSha256 := SHA256Bytes(Msg);
        Move(DigSha256[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA256)], SizeOf(TCnSHA256Digest));
      end;
    cmhtSHA384:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA384) + Length(Ctx) + SizeOf(TCnSHA384Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA384[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA384));

        DigSha384 := SHA384Bytes(Msg);
        Move(DigSha384[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA384)], SizeOf(TCnSHA384Digest));
      end;
    cmhtSHA512:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA512) + Length(Ctx) + SizeOf(TCnSHA512Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA512[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA512));

        DigSha512 := SHA512Bytes(Msg);
        Move(DigSha512[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA512)], SizeOf(TCnSHA512Digest));
      end;
    cmhtSHA512_224:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA512_224) + Length(Ctx) + SizeOf(TCnSHA512_224Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA512_224[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA512_224));

        DigSha512_224 := SHA512_224Bytes(Msg);
        Move(DigSha512_224[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA512_224)], SizeOf(TCnSHA512_224Digest));
      end;
    cmhtSHA512_256:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA512_256) + Length(Ctx) + SizeOf(TCnSHA512_256Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA512_256[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA512_256));

        DigSha512_256 := SHA512_256Bytes(Msg);
        Move(DigSha512_256[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA512_256)], SizeOf(TCnSHA512_256Digest));
      end;
    cmhtSHA3_224:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA3_224) + Length(Ctx) + SizeOf(TCnSHA3_224Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA3_224[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA3_224));

        DigSHA3_224 := SHA3_224Bytes(Msg);
        Move(DigSHA3_224[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA3_224)], SizeOf(TCnSHA3_224Digest));
      end;
    cmhtSHA3_256:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA3_256) + Length(Ctx) + SizeOf(TCnSHA3_256Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA3_256[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA3_256));

        DigSHA3_256 := SHA3_256Bytes(Msg);
        Move(DigSHA3_256[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA3_256)], SizeOf(TCnSHA3_256Digest));
      end;
    cmhtSHA3_384:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA3_384) + Length(Ctx) + SizeOf(TCnSHA3_384Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA3_384[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA3_384));

        DigSHA3_384 := SHA3_384Bytes(Msg);
        Move(DigSHA3_384[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA3_384)], SizeOf(TCnSHA3_384Digest));
      end;
    cmhtSHA3_512:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHA3_512) + Length(Ctx) + SizeOf(TCnSHA3_512Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHA3_512[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHA3_512));

        DigSHA3_512 := SHA3_512Bytes(Msg);
        Move(DigSHA3_512[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHA3_512)], SizeOf(TCnSHA3_512Digest));
      end;
    cmhtSHAKE128:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHAKE128) + Length(Ctx) + MLDSA_HASH_SHAKE128_SIZE);
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHAKE128[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHAKE128));

        DigShake := SHAKE128Bytes(Msg, MLDSA_HASH_SHAKE128_SIZE);
        Move(DigShake[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHAKE128)], MLDSA_HASH_SHAKE128_SIZE);
      end;
    cmhtSHAKE256:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SHAKE256) + Length(Ctx) + MLDSA_HASH_SHAKE256_SIZE);
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SHAKE256[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SHAKE256));

        DigShake := SHAKE256Bytes(Msg, MLDSA_HASH_SHAKE256_SIZE);
        Move(DigShake[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SHAKE256)], MLDSA_HASH_SHAKE256_SIZE);
      end;
    cmhtSM3:
      begin
        SetLength(Result, 2 + SizeOf(OID_MLDSA_PREHASH_SM3) + Length(Ctx) + SizeOf(TCnSM3Digest));
        Result[0] := 1;
        Result[1] := Length(Ctx);
        if Length(Ctx) > 0 then
          Move(Ctx[1], Result[2], Length(Ctx));

        Move(OID_MLDSA_PREHASH_SM3[0], Result[2 + Length(Ctx)], SizeOf(OID_MLDSA_PREHASH_SM3));

        DigSm3 := SM3Bytes(Msg);
        Move(DigSm3[0], Result[2 + Length(Ctx) + SizeOf(OID_MLDSA_PREHASH_SM3)], SizeOf(TCnSM3Digest));
      end;
  end;
end;

constructor TCnMLDSA.Create(AType: TCnMLDSAType);
begin
  inherited Create;
  FMLDSAType := AType;

  case AType of
    cmdt44:
      begin
        FMatrixRowCount := 4;
        FMatrixColCount := 4;
        FNoise := 2;
        FGamma1 := 1 shl 17; // 2^17 = 131072
        FGamma2 := (CN_MLDSA_PRIME - 1) div 88; // (8380416)/88 ≈95232
        FTau := 39;
        FLambda := 128;
        FBeta := FTau * FNoise; // 39 * 2 = 78
        FOmega := 80;
      end;
    cmdt65:
      begin
        FMatrixRowCount := 6;
        FMatrixColCount := 5;
        FNoise := 4;
        FGamma1 := 1 shl 19; // 2^19 = 524288
        FGamma2 := (CN_MLDSA_PRIME - 1) div 32; // (8380416)/32 = 261888
        FTau := 49;
        FLambda := 192;
        FBeta := FTau * FNoise; // 49 * 4 = 196
        FOmega := 55;
      end;
    cmdt87:
      begin
        FMatrixRowCount := 8;
        FMatrixColCount := 7;
        FNoise := 2;
        FGamma1 := 1 shl 19; // 2^19 = 524288
        FGamma2 := (CN_MLDSA_PRIME - 1) div 32; // (8380416)/32 = 261888
        FTau := 60;
        FLambda := 256;
        FBeta := FTau * FNoise; // 60 * 2 = 120
        FOmega := 75;
      end;
  else
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidParam);
  end;
end;

destructor TCnMLDSA.Destroy;
begin

  inherited;
end;

procedure TCnMLDSA.GenerateKeys(PrivateKey: TCnMLDSAPrivateKey;
  PublicKey: TCnMLDSAPublicKey; const RandHex: string);
var
  B, R, DB: TBytes;
  P1: TCnMLDSAKeyDigest;
  Matrix: TCnMLDSAPolyMatrix;
  S, T: TCnMLDSAPolyVector;
begin
  if (Length(RandHex) > 0) and (Length(RandHex) <> 64) then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidHexLength);

  if Length(RandHex) = 0 then
    R := CnRandomBytes(CN_MLDSA_KEY_SIZE)
  else
    R := HexToBytes(RandHex);

  SetLength(B, SizeOf(TCnMLDSASeed) + 2);
  Move(R[0], B[0], SizeOf(TCnMLDSASeed));

  B[SizeOf(TCnMLDSASeed)] := FMatrixRowCount;
  B[SizeOf(TCnMLDSASeed) + 1] := FMatrixColCount;

  DB := MLDSAHFunc(B, 128);

  // 128 字节摘要拆成 32 字节 p、64 字节 p1、32 字节 K
  Move(DB[0], PrivateKey.FGenerationSeed[0], SizeOf(TCnMLDSASeed));
  Move(DB[0], PublicKey.FGenerationSeed[0], SizeOf(TCnMLDSASeed));
  Move(DB[SizeOf(TCnMLDSASeed)], P1[0], CN_MLDSA_DIGEST_SIZE);
  Move(DB[SizeOf(TCnMLDSASeed) + CN_MLDSA_DIGEST_SIZE], PrivateKey.FKey[0], CN_MLDSA_DIGEST_SIZE);

  // 用 p 生成矩阵，NTT 形式，Row 行 Col 列
  GenerateMatrix(PrivateKey.GenerationSeed, Matrix);

  // 用 p1 生成两个秘密多项式向量，均非 NTT 形式，前者维度 Col，后者维度 Row
  GenerateSecret(P1, PrivateKey.FS1, PrivateKey.FS2);

  // S1 转 NTT 形式到 S，维度 Col
  MLDSAVectorToNTT(S, PrivateKey.FS1);

  // 计算 T = A * S1 + S2
  MLDSAMatrixVectorMul(T, Matrix, S);      // 两个 NTT 相乘，得到维度 Row
  MLDSAVectorToINTT(T, T);                 // 结果向量转回非 NTT
  MLDSAVectorAdd(T, T, PrivateKey.FS2);    // 和 S2 相加，两个维度都是 Row

  // 结果 T 向量拆分为 T0 和 T1
  MLDSAPower2RoundVector(T, PrivateKey.FT0, PublicKey.FT1);

  // 公钥字节数组求杂凑作为私钥 tr
  B := SavePublicKeyToBytes(PublicKey);
  B := MLDSAHFunc(B);
  Move(B[0], PrivateKey.FTrace[0], CN_MLDSA_DIGEST_SIZE);
end;

procedure TCnMLDSA.GenerateMask(const Dig: TBytes; Mu: Integer;
  out Mask: TCnMLDSAPolyVector);
var
  I: Integer;
  B, R: TBytes;
  V: TBytes;
  C: Integer;
  P: PCnWord;
begin
  if Length(Dig) <> CN_MLDSA_DIGEST_SIZE then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidParam);

  // 计算每个系数需要的位数
  C := 1 + GetUInt32BitLength(FGamma1 - 1);

  // 设置结果向量维度为矩阵列数
  SetLength(Mask, FMatrixColCount);

  SetLength(B, 2);
  P := @B[0];
  for I := 0 to FMatrixColCount - 1 do
  begin
    // 构造种子: ρ' = ρ || IntegerToBytes(μ + r, 2)
    P^ := UInt16ToLittleEndian(Word(Mu + I));
    R := ConcatBytes(Dig, B);

    // 计算杂凑: v = H(ρ', 32 * c)
    V := MLDSAHFunc(R, 32 * C);

    // 解包为多项式: y[r] = BitUnpack(v, γ1-1, γ1)
    MLDSABitUnpackPolynomial(V, Mask[I], FGamma1 - 1, FGamma1);
  end;
end;

procedure TCnMLDSA.GenerateMatrix(const Seed: TCnMLDSASeed;
  out Matrix: TCnMLDSAPolyMatrix);
var
  I, J: Integer;
  PJI: TBytes;
  W: TIntegers;
begin
  // 设置矩阵大小
  SetLength(Matrix, FMatrixRowCount);
  for I := 0 to FMatrixRowCount - 1 do
    SetLength(Matrix[I], FMatrixColCount);

  // 准备好 Sample 随机数据
  SetLength(PJI, SizeOf(TCnMLDSASeed) + 2);
  Move(Seed[0], PJI[0], SizeOf(TCnMLDSASeed));

  // 生成矩阵，内容是 NTT 形式
  for I := 0 to FMatrixRowCount - 1 do
  begin
    for J := 0 to FMatrixColCount - 1 do
    begin
      PJI[SizeOf(TCnMLDSASeed)] := J;
      PJI[SizeOf(TCnMLDSASeed) + 1] := I;
      W := MLDSARejNTTPoly(PJI);
      Move(W[0], Matrix[I][J][0], Length(W) * SizeOf(Integer));
    end;
  end;
end;

procedure TCnMLDSA.GenerateSecret(const Seed: TCnMLDSAKeyDigest; out S1,
  S2: TCnMLDSAPolyVector);
var
  I: Integer;
  PJI: TBytes;
  W: TIntegers;
  P: PCnWord;
begin
  SetLength(S1, FMatrixColCount);
  SetLength(S2, FMatrixRowCount);

  SetLength(PJI, SizeOf(TCnMLDSAKeyDigest) + 2);
  Move(Seed[0], PJI[0], SizeOf(TCnMLDSAKeyDigest));
  P := @PJI[SizeOf(TCnMLDSAKeyDigest)];

  for I := 0 to FMatrixColCount - 1 do
  begin
    P^ := UInt16ToLittleEndian(I);
    W := MLDSARejBoundedPoly(PJI, FNoise);
    Move(W[0], S1[I][0], Length(W) * SizeOf(Integer));
  end;

  for I := 0 to FMatrixRowCount - 1 do
  begin
    P^ := UInt16ToLittleEndian(I + FMatrixColCount);
    W := MLDSARejBoundedPoly(PJI, FNoise);
    Move(W[0], S2[I][0], Length(W) * SizeOf(Integer));
  end;
end;

function TCnMLDSA.GetNoiseBitLength: Integer;
begin
  case FNoise of
    2: Result := 2;
    4: Result := 3;
  else
    Result := 2;
  end;
end;

function TCnMLDSA.InternalSign(PrivateKey: TCnMLDSAPrivateKey;
  const Msg: TBytes; const Seed: TCnMLDSASeed): TBytes;
var
  S1: TCnMLDSAPolyVector;
  S2: TCnMLDSAPolyVector;
  T0: TCnMLDSAPolyVector;
  M: TCnMLDSAPolyMatrix;
  Miu, P2, CM: TBytes;
  Cnt: Integer;
  Y, YN, W, W1, CS1, CS2, Z, CT0, R, R0, T1, T2, H: TCnMLDSAPolyVector;
  C, CN: TCnMLDSAPolynomial;
begin
  // 仨向量的 NTT 形式
  MLDSAVectorToNTT(S1, PrivateKey.S1);
  MLDSAVectorToNTT(S2, PrivateKey.S2);
  MLDSAVectorToNTT(T0, PrivateKey.T0);

  // 生成矩阵
  GenerateMatrix(PrivateKey.GenerationSeed, M);

  // 生成 64 字节 Miu
  Miu := MLDSAHFunc(ConcatBytes(NewBytesFromMemory(@PrivateKey.Trace[0], SizeOf(TCnMLDSAKeyDigest)), Msg));

  // 生成 64 字节 P2
  P2 := MLDSAHFunc(ConcatBytes(NewBytesFromMemory(@PrivateKey.Key[0], SizeOf(TCnMLDSASeed)),
    NewBytesFromMemory(@Seed[0], SizeOf(TCnMLDSASeed)), Miu));

  Cnt := 0;
  while True do
  begin
    GenerateMask(P2, Cnt, Y);       // 生成 Y，非 NTT 形式
    MLDSAVectorToNTT(YN, Y);        // 转成 NTT 形式

    MLDSAMatrixVectorMul(W, M, YN); // W = A * Yn，NTT 形式
    MLDSAVectorToINTT(W, W);        // W 转回非 NTT 形式

    MLDSAHighBitsVector(W1, W, FGamma2);

    CM := MLDSAHFunc(ConcatBytes(Miu, MLDSAW1Encode(W1, FGamma2)), FLambda shr 2);

    MLDSASampleInBall(CM, FTau, C); // 采样生成 C 多项式
    MLDSAPolynomialToNTT(CN, C);    // 转换为 NTT 形式

    MLDSAPolynomialVectorMul(CS1, CN, S1);  // CN 与 NTT 的 S1 相乘并转为非 NTT 放 CS1
    MLDSAVectorToINTT(CS1, CS1);

    MLDSAPolynomialVectorMul(CS2, CN, S2);  // CN 与 NTT 的 S2 相乘并转为非 NTT 放 CS2
    MLDSAVectorToINTT(CS2, CS2);

    SetLength(Z, Length(Y));
    MLDSAVectorAdd(Z, Y, CS1);              // Z := Y + CS1
    SetLength(R, Length(W));
    MLDSAVectorSub(R, W, CS2);              // R := W - CS2
    MLDSALowBitsVector(R0, R, FGamma2);

    if (VectorInfinityNorm(Z) >= FGamma1 - FBeta) or // 拒绝当前签名尝试，继续下一回
      (VectorInfinityNorm(R0) >= FGamma2 - FBeta) then
    begin
      Inc(Cnt, FMatrixColCount);
      Continue;
    end;

    MLDSAPolynomialVectorMul(CT0, CN, T0);  // CN 与 NTT 的 T0 相乘并转为非 NTT 放 CT0
    MLDSAVectorToINTT(CT0, CT0);

    if VectorInfinityNorm(CT0) >= FGamma2 then
    begin
      Inc(Cnt, FMatrixColCount);
      Continue;
    end;

    // 计算 -CT0 和 W - CS2 + CT0
    MLDSAVectorNeg(T1, CT0);

    // 计算 W - CS2 + CT0
    SetLength(T2, Length(W));
    MLDSAVectorSub(T2, W, CS2);
    MLDSAVectorAdd(T2, T2, CT0);

    // 计算提示向量 H
    MLDSAMakeHintVector(H, T1, T2, FGamma2);
    if MLDSAVectorCountOne(H) > FOmega then
    begin
      Inc(Cnt, FMatrixColCount);
      Continue;
    end;

    // 都符合要求，签名生成完毕
    MLDSAVectorCenterMod(Z, Z);
    Result := SigEncode(CM, Z, H);
    Exit;
  end;
end;

function TCnMLDSA.InternalVerify(PublicKey: TCnMLDSAPublicKey; const Msg,
  Signature: TBytes): Boolean;
var
  C, CM, TR, Miu: TBytes;
  Z, H, ZN, ZT, T1, T1N, W1, WA: TCnMLDSAPolyVector;
  M: TCnMLDSAPolyMatrix;
  CP, CPN: TCnMLDSAPolynomial;
begin
  Result := False;
  SigDecode(Signature, C, Z, H);
  if H = nil then
    Exit;

  GenerateMatrix(PublicKey.GenerationSeed, M);
  TR := SavePublicKeyToBytes(PublicKey);
  TR := MLDSAHFunc(TR, CN_MLDSA_DIGEST_SIZE);
  Miu := MLDSAHFunc(ConcatBytes(TR, Msg), CN_MLDSA_DIGEST_SIZE);

  MLDSASampleInBall(C, FTau, CP);  // 从 C 生成 CP

  MLDSAVectorToNTT(ZN, Z);
  MLDSAMatrixVectorMul(ZT, M, ZN); // A * ntt(Z) -> ZT
  MLDSAPolynomialToNTT(CPN, CP);

  MLDSAPolynomialVectorScaleByPower2(T1, PublicKey.T1, CN_MLDSA_DROPBIT); // T1 * 2^d
  MLDSAVectorToNTT(T1N, T1);
  MLDSAPolynomialVectorMul(T1, CPN, T1N);  // ntt(C) * ntt(T1 * 2^d) -> T1

  // ZN - T1
  SetLength(W1, Length(ZT));
  MLDSAVectorSub(W1, ZT, T1);
  MLDSAVectorToINTT(WA, W1);               // 得到 Wapprox

  UseHintVector(W1, H, WA);                // 算出 W1

  Result := VectorInfinityNorm(Z) < FGamma1 - FBeta;
  if Result then
  begin
    CM := MLDSAHFunc(ConcatBytes(Miu, MLDSAW1Encode(W1, FGamma2)), FLambda shr 2);
    Result := CompareBytes(C, CM);
  end;
end;

procedure TCnMLDSA.LoadPrivateKeyFromBytes(PrivateKey: TCnMLDSAPrivateKey;
  const SK: TBytes);
var
  L: Integer;
  B: TBytes;
begin
  L := SizeOf(TCnMLDSASeed) * 2 + SizeOf(TCnMLDSAKeyDigest)
    + 32 * (FMatrixRowCount + FMatrixColCount) * (GetNoiseBitLength + 1)
    + 32 * FMatrixRowCount * CN_MLDSA_DROPBIT;
  if Length(SK) <> L then
    raise ECnMLDSAException.CreateFmt(SCnErrorMLDSAKeyLengthMismatch,
      [L, Length(SK)]);

  Move(SK[0], PrivateKey.FGenerationSeed[0], SizeOf(TCnMLDSASeed));
  Move(SK[SizeOf(TCnMLDSASeed)], PrivateKey.FKey[0], SizeOf(TCnMLDSASeed));
  Move(SK[2 * SizeOf(TCnMLDSASeed)], PrivateKey.FTrace[0], SizeOf(TCnMLDSAKeyDigest));

  // 复制出来给 S1
  L := 2 * SizeOf(TCnMLDSASeed) + SizeOf(TCnMLDSAKeyDigest);
  B := Copy(SK, L, 32 * FMatrixColCount * (GetNoiseBitLength + 1));
  SetLength(PrivateKey.FS1, FMatrixColCount);
  MLDSABitUnpackVector(B, PrivateKey.FS1, FNoise, FNoise);

  // 复制出来给 S2
  Inc(L, 32 * FMatrixColCount * (GetNoiseBitLength + 1));
  B := Copy(SK, L, 32 * FMatrixRowCount * (GetNoiseBitLength + 1));
  SetLength(PrivateKey.FS2, FMatrixRowCount);
  MLDSABitUnpackVector(B, PrivateKey.FS2, FNoise, FNoise);

  // 复制出来给 T0
  Inc(L, 32 * FMatrixRowCount * (GetNoiseBitLength + 1));
  B := Copy(SK, L, MaxInt);
  SetLength(PrivateKey.FT0, FMatrixRowCount);
  MLDSABitUnpackVector(B, PrivateKey.FT0, CN_MLDSA_DROPVALUE - 1, CN_MLDSA_DROPVALUE);
end;

procedure TCnMLDSA.LoadPublicKeyFromBytes(PublicKey: TCnMLDSAPublicKey;
  const PK: TBytes);
var
  B: TBytes;
begin
  if Length(PK) <> SizeOf(TCnMLDSASeed) + 32 * FMatrixRowCount * CN_MLDSA_PUBKEY_BIT then
    raise ECnMLDSAException.CreateFmt(SCnErrorMLDSAKeyLengthMismatch,
      [SizeOf(TCnMLDSASeed) + 32 * FMatrixRowCount * CN_MLDSA_PUBKEY_BIT, Length(PK)]);

  Move(PK[0], PublicKey.FGenerationSeed[0], SizeOf(TCnMLDSASeed));
  B := Copy(PK, SizeOf(TCnMLDSASeed), MaxInt);
  SetLength(PublicKey.FT1, FMatrixRowCount);
  MLDSASimpleBitUnpackVector(B, PublicKey.FT1, CN_MLDSA_PUBKEY_BIT);
end;

function TCnMLDSA.SavePrivateKeyToBytes(PrivateKey: TCnMLDSAPrivateKey): TBytes;
var
  S1, S2, T0: TBytes;
begin
  Result := ConcatBytes(NewBytesFromMemory(@PrivateKey.GenerationSeed[0], SizeOf(TCnMLDSASeed)),
    NewBytesFromMemory(@PrivateKey.Key[0], SizeOf(TCnMLDSASeed)),
    NewBytesFromMemory(@PrivateKey.Trace[0], SizeOf(TCnMLDSAKeyDigest)));

  S1 := MLDSABitPackVector(PrivateKey.S1, FNoise, FNoise);
  S2 := MLDSABitPackVector(PrivateKey.S2, FNoise, FNoise);
  T0 := MLDSABitPackVector(PrivateKey.T0, CN_MLDSA_DROPVALUE - 1, CN_MLDSA_DROPVALUE);
  Result := ConcatBytes(Result, S1, S2, T0);
end;

function TCnMLDSA.SavePublicKeyToBytes(PublicKey: TCnMLDSAPublicKey): TBytes;
begin
  Result := NewBytesFromMemory(@PublicKey.GenerationSeed[0], SizeOf(TCnMLDSASeed));
  Result := ConcatBytes(Result, MLDSASimpleBitPackVector(PublicKey.FT1, CN_MLDSA_PUBKEY_BIT));
end;

procedure TCnMLDSA.SigDecode(const Signature: TBytes; out C: TBytes; out Z,
  H: TCnMLDSAPolyVector);
var
  ExpLen, CLen, ZLength, HLength: Integer;
  Offset: Integer;
  I: Integer;
  PackedZ, PackedH: TBytes;
begin
  // 计算预期的签名长度，包括 c 长度、PackedZ 长度与 PackedH 长度，应该是 2420、3309、4627
  ExpLen := FLambda div 4 + FMatrixColCount * 32 * (1 + GetUInt32BitLength(FGamma1 - 1)) +
    FOmega + FMatrixRowCount;

  // 检查签名长度是否合法
  if Length(Signature) <> ExpLen then
    raise ECnMLDSAException.CreateFmt(SCnErrorMLDSASigLengthMismatch,
      [ExpLen, Length(Signature)]);

  CLen := FLambda div 4;
  SetLength(C, CLen);
  Move(Signature[0], C[0], CLen);

  Offset := CLen;

  // 解码响应向量 z
  ZLength := FMatrixColCount * 32 * (GetUInt32BitLength(FGamma1 - 1) + 1);
  SetLength(PackedZ, ZLength);
  Move(Signature[Offset], PackedZ[0], ZLength);

  SetLength(Z, FMatrixColCount);
  for I := 0 to FMatrixColCount - 1 do
  begin
    MLDSABitUnpackPolynomial(
      Copy(PackedZ, I * ZLength div FMatrixColCount, ZLength div FMatrixColCount),
      Z[I], FGamma1 - 1, FGamma1);
  end;

  Offset := Offset + ZLength;

  // 码提示向量 h
  HLength := FOmega + FMatrixRowCount;
  SetLength(PackedH, HLength);
  Move(Signature[Offset], PackedH[0], HLength);

  HintBitUnpackVector(PackedH, H);

  // 注意 H 可能是 nil
end;

function TCnMLDSA.SigEncode(const C: TBytes; const Z, H: TCnMLDSAPolyVector): TBytes;
var
  I: Integer;
  PackedZ: TBytes;
  PackedH: TBytes;
begin
  PackedZ := nil;
  for I := Low(Z) to High(Z) do
  begin
    // 使用 BitPack 编码每个多项式：BitPack(z[i], γ-1, γ)
    PackedZ := ConcatBytes(PackedZ,
      MLDSABitPackPolynomial(Z[I], FGamma1 - 1, FGamma1));
  end;

  PackedH := HintBitPackVector(H);
  Result := ConcatBytes(C, PackedZ, PackedH);
end;

function TCnMLDSA.SignBytes(PrivateKey: TCnMLDSAPrivateKey; const Msg: TBytes;
  const Ctx: AnsiString; HashType: TCnMLDSAHashType; const RandHex: string): TBytes;
var
  Seed: TCnMLDSASeed;
  R, Data: TBytes;
begin
  if Length(Msg) <= 0 then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidMsgLength);

  if (Length(RandHex) > 0) and (Length(RandHex) <> 64) then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidHexLength);

  if Length(Ctx) > 255 then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidCtxLength);

  if Length(RandHex) = 0 then
  begin
    SetLength(R, CN_MLDSA_KEY_SIZE);
    FillChar(R[0], Length(R), 0);
  end
  else
    R := HexToBytes(RandHex);

  Move(R[0], Seed[0], SizeOf(TCnMLDSASeed));
  Data := CalcSignHashBytes(Msg, HashType, Ctx);

  // Data 第一个字节 0 表示原始信息，1 表示杂凑过且有 OID
  Result := InternalSign(PrivateKey, Data, Seed);
end;

function TCnMLDSA.VerifyBytes(PublicKey: TCnMLDSAPublicKey; const Msg: TBytes;
  const Signature: TBytes; const Ctx: AnsiString; HashType: TCnMLDSAHashType): Boolean;
var
  Data: TBytes;
begin
  if Length(Msg) <= 0 then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidMsgLength);

  if Length(Ctx) > 255 then
    raise ECnMLDSAException.Create(SCnErrorMLDSAInvalidCtxLength);

  Data := CalcSignHashBytes(Msg, HashType, Ctx);
  Result := InternalVerify(PublicKey, Data, Signature);
end;

end.
