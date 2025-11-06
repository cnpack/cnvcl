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

unit CnLattice;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：格密码计算单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元简略实现了基于格（Lattice）的 NTRU 加解密算法。
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.09.10 V1.1
*               实现 NTRU 的加解密算法
*           2023.08.25 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs,
  CnNative, CnVector, CnBigNumber, CnPolynomial, CnRandom, CnBits, CnSHA3;

const
  CN_MLKEM_KEY_SIZE    = 32;
  {* MLKEM  的共享密钥及种子等的长度}

  CN_MLKEM_POLY_DEGREE = 256;
  {* MLKEM  的多项式次数}

  CN_MLKEM_PRIME       = 3329;
  {* MLKEM  使用的素数}

  CN_MLKEM_PRIME_INV   = 3303;
  {* MLKEM  使用的 128 对该素数的模逆元}

type
  ECnLatticeException = class(Exception);
  {* NTRU/MLKEM相关异常}

  TCnNTRUParamType = (cnptCustomized, cnptClassic, cnptHPS2048509, cnptHPS2048677,
    cnptHPS4096821);
  {* NTRU 几个推荐参数}

  TCnMLKEMType = (cmkt512, cmkt768, cmkt1024);
  {* MLKEM 的三种实现规范}

  TCnNTRUPrivateKey = class
  {* Number Theory Research Unit 的私钥，F G 两个多项式及其模逆}
  private
    FFQ: TCnInt64Polynomial;
    FF: TCnInt64Polynomial;
    FG: TCnInt64Polynomial;
    FFP: TCnInt64Polynomial;
    procedure SetFF(const Value: TCnInt64Polynomial);
    procedure SetFFP(const Value: TCnInt64Polynomial);
    procedure SetFFQ(const Value: TCnInt64Polynomial);
    procedure SetFG(const Value: TCnInt64Polynomial);
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 显示 F 和 G 的字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property F: TCnInt64Polynomial read FF write SetFF;
    {* 私钥多项式 F，随机生成时要求有 D+1 个 1，D 个 -1，其他是 0}
    property G: TCnInt64Polynomial read FG write SetFG;
    {* 私钥多项式 G，随机生成时要求有 D 个 1，D 个 -1，其他是 0}
    property FQ: TCnInt64Polynomial read FFQ write SetFFQ;
    {* 私钥多项式 F 对大模 Q 的模逆多项式，由外界计算而设，供运算加速用}
    property FP: TCnInt64Polynomial read FFP write SetFFP;
    {* 私钥多项式 F 对小素数模 P 的模逆多项式，由外界计算而设，供运算加速用}
  end;

  TCnNTRUPublicKey = class
  {* Number Theory Research Unit 的公钥，一个 H 多项式}
  private
    FH: TCnInt64Polynomial;
    procedure SetFH(const Value: TCnInt64Polynomial);
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 显示 H 的字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property H: TCnInt64Polynomial read FH write SetFH;
    {* 公钥多项式}
  end;

  TCnNTRU = class
  {* Number Theory Research Unit 实现类}
  private
    FQ: Int64;
    FQExponent: Integer;
    FD: Integer;
    FN: Integer;
    FPrime: Integer;
    FRing: TCnInt64Polynomial;
  protected
    procedure RandPolynomial(P: TCnInt64Polynomial; MaxDegree: Integer;
      OneCount: Integer; MinusOneCount: Integer); overload;
    {* 随机生成最高次数是 MaxDegree 的多项式，有 OneCount 个 1，MinusOneCount 个 -1，其余是 0。

       参数：
         P: TCnInt64Polynomial            - 生成的结果多项式
         MaxDegree: Integer               - 最高次数
         OneCount: Integer                - 1 的个数
         MinusOneCount: Integer           - -1 的个数

       返回值：（无）
    }

    procedure RandPolynomial(P: TCnInt64Polynomial; MaxDegree: Integer); overload;
    {* 随机生成最高次数是 MaxDegree 的多项式，内部系数 1 0 -1 随机，注意与 FPrime 无关。

       参数：
         P: TCnInt64Polynomial            - 生成的结果多项式
         MaxDegree: Integer               - 最高次数

       返回值：（无）
    }

  public
    constructor Create(NTRUType: TCnNTRUParamType = cnptClassic); virtual;
    {* 构造函数，指定 NTRU 参数类型。

       参数：
         NTRUType: TCnNTRUParamType       - NTRU 参数类型

       返回值：TCnNTRU                    - 对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Load(Predefined: TCnNTRUParamType);
    {* 加载预定类型的 NTRU 参数。

       参数：
         Predefined: TCnNTRUParamType     - NTRU 参数类型

       返回值：（无）
    }

    procedure GenerateKeys(PrivateKey: TCnNTRUPrivateKey; PublicKey: TCnNTRUPublicKey);
    {* 生成一对公私钥。

       参数：
         PrivateKey: TCnNTRUPrivateKey    - 生成的 NTRU 私钥
         PublicKey: TCnNTRUPublicKey      - 生成的 NTRU 公钥

       返回值：（无）
    }

    procedure Encrypt(PublicKey: TCnNTRUPublicKey; PlainData: TCnInt64Polynomial;
      OutEnData: TCnInt64Polynomial);
    {* 用公钥加密明文多项式得到密文多项式，两者次数最高 N - 1，因为环是 X^N - 1。

       参数：
         PublicKey: TCnNTRUPublicKey      - NTRU 公钥
         PlainData: TCnInt64Polynomial    - 待加密的明文多项式
         OutEnData: TCnInt64Polynomial    - 输出的密文多项式

       返回值：（无）
    }

    procedure Decrypt(PrivateKey: TCnNTRUPrivateKey; EnData: TCnInt64Polynomial;
      OutPlainData: TCnInt64Polynomial);
    {* 用私钥解密密文多项式得到明文多项式，两者次数最高 N - 1，因为环是 X^N - 1。

       参数：
         PrivateKey: TCnNTRUPrivateKey    - NTRU 私钥
         EnData: TCnInt64Polynomial       - 待解密的密文多项式
         OutPlainData: TCnInt64Polynomial - 输出的明文多项式

       返回值：（无）
    }

    function EncryptBytes(PublicKey: TCnNTRUPublicKey; Data: TBytes): TBytes;
    {* 用公钥加密明文字节数组，返回加密结果，注意明文会被补 #0 到规定长度。

       参数：
         PublicKey: TCnNTRUPublicKey      - NTRU 公钥
         Data: TBytes                     - 待加密的明文字节数组

       返回值：TBytes                     - 返回密文字节数组
    }

    function DecryptBytes(PrivateKey: TCnNTRUPrivateKey; EnData: TBytes): TBytes;
    {* 用私钥解密密文字节数组，返回解密结果，注意明文会被补 #0 到规定长度。

       参数：
         PrivateKey: TCnNTRUPrivateKey    - NTRU 私钥
         EnData: TBytes                   - 待解密的密文字节数组

       返回值：TBytes                     - 返回明文字节数组
    }

    property Ring: TCnInt64Polynomial read FRing;
    {* 多项式环}
    property N: Integer read FN write FN;
    {* 多项式位数}
    property D: Integer read FD write FD;
    {* 控制私钥多项式的参数范围}
    property Prime: Integer read FPrime write FPrime;
    {* 小素数模，默认 3}
    property QExponent: Integer read FQExponent write FQExponent;
    {* 大素数幂模的幂指数，底为 2，模为 2^QExponent}
  end;

  TCnMLKEMSeed = array[0..CN_MLKEM_KEY_SIZE - 1] of Byte;
  {* MLKEM 种子，32 字节}

  TCnMLKEMBlock = array[0..CN_MLKEM_KEY_SIZE - 1] of Byte;
  {* MLKEM 块数据，32 字节}

  TCnMLKEMPolynomial = array[0..CN_MLKEM_POLY_DEGREE - 1] of Word;
  {* MLKEM 多项式系数，256 个双字，用来表达一个多项式}

  TCnMLKEMPolyVector = array of TCnMLKEMPolynomial;
  {* 多项式列表或叫向量，用来表达 S 或 E 等}

  TCnMLKEMPolyMatrix = array of TCnMLKEMPolyVector;
  {* 多项式矩阵，用来表达 A}

  TCnMLKEMDecapsulationKey = class
  {* MLKEM 的非公开解封密钥，包括秘密多项式向量与隐式拒绝的随机种子}
  private
    FSecretVector: TCnMLKEMPolyVector;
    FInjectionSeed: TCnMLKEMSeed;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    property SecretVector: TCnMLKEMPolyVector read FSecretVector;
    {* 秘密多项式向量，相当于规范里的 S}
    property InjectionSeed: TCnMLKEMSeed read FInjectionSeed;
    {* 用于隐式拒绝的随机种子，不参与密钥生成，相当于规范里的 Z}
  end;

  TCnMLKEMEncapsulationKey = class
  {* MLKEM 的封装密钥，可公开，包括可用来生成矩阵的种子，以及公钥多项式向量}
  private
    FGenerationSeed: TCnMLKEMSeed;
    FPubVector: TCnMLKEMPolyVector;            // 私钥与矩阵计算出的公钥多项式向量
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    property GenerationSeed: TCnMLKEMSeed read FGenerationSeed;
    {* 用于生成整套密钥的主随机种子，相当于规范里的 D}
    property PubVector: TCnMLKEMPolyVector read FPubVector;
    {* 生成的公开多项式向量，相当于规范里的 T}
  end;

  TCnMLKEM = class
  {* 基于模块化格的密钥封装机制（Module-Lattice-based Key Encapsulation Mechanism）实现类}
  private
    FMatrixRank: Integer;
    FNoise1: Integer;
    FNoise2: Integer;
    FRing: TCnInt64Polynomial;
    FCompressDigits: Integer;
  protected
    procedure KPKEKeyGen(const D: TCnMLKEMSeed; out GenerationSeed: TCnMLKEMSeed;
      out Secret, Pub: TCnMLKEMPolyVector);
    {* 核心生成方法，D 是外部传入的真随机数}

  public
    constructor Create(AType: TCnMLKEMType); virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure MLKEMKeyGen(EncapKey: TCnMLKEMEncapsulationKey; DecapKey: TCnMLKEMDecapsulationKey;
       const RandDHex: string = ''; const RandZHex: string = '');
    {* 用两个真随机 32 字节种子，生成一对 Key，随机数允许外部传入十六进制}

    function SaveKeyToBytes(EncapKey: TCnMLKEMEncapsulationKey): TBytes;
    {* 将公开密钥保存成字节流}
    function SaveKeysToBytes(DecapKey: TCnMLKEMDecapsulationKey; EncapKey: TCnMLKEMEncapsulationKey): TBytes;
    {* 将非公开密钥与公开密钥都保存成字节流}

    class function Compress(X: Word; D: Word): Word;
    {* 将一个 X 的系数值压缩到 D 位并返回}
    class function Decompress(X: Word; D: Word): Word;
    {* 将一个压缩后的系数值解压并返回}
    class function SamplePolyCBD(const RandBytes: TBytes; Eta: Integer): TWords;
    {* 根据真随机数组生成 256 个采样的多项式系数，RandBytes 的长度至少要 64 * Eta 字节}
    class function SampleNTT(const RandBytes: TBytes): TWords;
    {* 根据真随机数组生成 256 个采样的多项式系数供 NTT 变换用，RandBytes 的长度至少要 34 字节}

    class function PseudoRandomFunc(Eta: Integer; const Input: TCnMLKEMSeed; B: Byte): TBytes;
    {* PRF 函数，根据 32 字节输入和一字节附加数据，用 SHAKE256 生成 64 * Eta 长度的字节}
    class function HFunc(const Data: TBytes): TCnMLKEMBlock;
    {* H 函数，内部用 SHA3_256 生成 32 位杂凑值}
    class function JFunc(const Data: TBytes): TCnMLKEMBlock;
    {* J 函数，内部用 SHAKE256 生成 32 位杂凑值}
    class procedure GFunc(const Data: TBytes; out Block1, Block2: TCnMLKEMBlock);
    {* G 函数，内部用 SHA3_512 生成两个 32 位杂凑值}

    property MatrixRank: Integer read FMatrixRank write FMatrixRank;
    {* 矩阵的秩，在这里是方阵尺寸，取值 2 或 3 或 4}
    property Ring: TCnInt64Polynomial read FRing;
    {* 多项式环的模多项式}
    property Noise1: Integer read FNoise1 write FNoise1;
    {* 噪声参数一，控制生成密钥时秘密向量和错误向量的采样范围}
    property Noise2: Integer read FNoise2 write FNoise2;
    {* 噪声参数二，控制封装时的随机向量与两个错误向量的采样范围}
    property CompressDigits: Integer read FCompressDigits write FCompressDigits;
    {* 压缩位数}
  end;

procedure NTRUDataToInt64Polynomial(Res: TCnInt64Polynomial; Data: Pointer;
  ByteLength: Integer; N: Int64; Modulus: Int64; CheckSum: Boolean = True);
{* 根据 NTRU 的规范将数据内容转换为模数的多项式供加解密，如数据超长会抛异常。
   以 Q 的二进制位数为单位劈分数据，如 CheckSum 为 True，则取前 N - 1 个系数，小端转换为
   多项式的 0 次到 N - 2 次项系数，N - 1 次系数则是各系数和 mod Q 再取负，适合于明文转换。
   如 CheckSum 为 False，则取前 N 个系数，小端转换为多项式的 0 次到 N - 1 次项系数，适合于密文转换。
   返回转换是否成功。

   参数：
     Res: TCnInt64Polynomial              - 输出的结果多项式
     Data: Pointer                        - 待转换的数据块地址
     ByteLength: Integer                  - 待转换的数据块字节长度
     N: Int64                             - 多项式位数
     Modulus: Int64                       - 模数
     CheckSum: Boolean                    - 取系数校验的方式

   返回值：（无）
}                                                                              

function NTRUInt64PolynomialToData(P: TCnInt64Polynomial; N: Int64; Modulus: Int64;
  Data: Pointer; CheckSum: Boolean = True): Integer;
{* 根据 NTRU 的规范将模数的多项式转换为数据内容并放于 Data 所指的内存中，返回放置的内存长度。
   如 CheckSum 为 True，只取 0 到 N - 1 次共 N - 2 个系数，适合于明文转换。
   如 CheckSum 为 False 则取 0 到 N 次共 N - 1 个系数，适合于密文转换。
   先将多项式系数 mod 到 0 到 Q - 1 的范围，每个值放入以 Q 的二进制位数为单位的数据块，
   再拼起来补 0 凑足整数字节。如果 Data 传 nil，则返回所需的内存长度。

   参数：
     P: TCnInt64Polynomial                - 待转换的多项式
     N: Int64                             - 多项式位数
     Modulus: Int64                       - 模数
     Data: Pointer                        - 待放置内容的区域地址
     CheckSum: Boolean                    - 取系数校验的方式

   返回值：Integer                        - 如果 Data 传 nil，则返回所需的内存长度。其他情况返回放置的内存长度。
}

function Int64GaussianLatticeReduction(V1: TCnInt64Vector; V2: TCnInt64Vector;
  X: TCnInt64Vector; Y: TCnInt64Vector): Boolean;
{* 对两个二维 Int64 向量做整数格上的近似高斯格基约减以求解二维 SVP 问题，返回是否成功。

   参数：
     V1: TCnInt64Vector                   - 待约减的二维向量一
     V2: TCnInt64Vector                   - 待约减的二维向量二
     X: TCnInt64Vector                    - 约减的二维向量结果一
     Y: TCnInt64Vector                    - 约减的二维向量结果二

   返回值：Boolean                        - 返回约减是否成功
}

function BigNumberGaussianLatticeReduction(V1: TCnBigNumberVector; V2: TCnBigNumberVector;
  X: TCnBigNumberVector; Y: TCnBigNumberVector): Boolean;
{* 对两个二维大整数向量做整数格上的近似高斯格基约减以求解二维 SVP 问题，返回是否成功。
   用的虽然是格拉姆-施密特的正交化思想，但结果并不是正交的。

   参数：
     V1: TCnBigNumberVector               - 待约减的二维大整数向量一
     V2: TCnBigNumberVector               - 待约减的二维大整数向量二
     X: TCnBigNumberVector                - 约减的二维大整数向量结果一
     Y: TCnBigNumberVector                - 约减的二维大整数向量结果二

   返回值：Boolean                        - 返回约减是否成功
}

implementation

resourcestring
  SCnErrorLatticeNTRUInvalidParam = 'Invalid NTRU Value.';
  SCnErrorLatticeModulusTooMuch = 'Modulus Too Much %d';
  SCnErrorLatticeDataTooLong = 'Data Too Long %d';
  SCnErrorLatticeMLKEMInvalidParam = 'Invalid MLKEM Value.';
  SCnErrorLatticeEtaMustBe2Or3 = 'Eta Must Be 2 or 3';
  SCnErrorLatticeInvalidRandomLength = 'Invalid Random Length for SamplePolyCBD';
  SCnErrorLatticeInvalidSampleNTT = 'SampleNTT Input Must Be 34 Bytes';
  SCnErrorLatticeInvalidEncodeDigit = 'Digit must be between 1 and 12';

type
  TCnNTRUPredefinedParams = packed record
    N: Int64;
    D: Int64;
    P: Int64;
    QExp: Int64;
  end;

  // Barrett Reduction 所需的参数结构体
  TMLKEMBarrettReduce = packed record
    MU: Cardinal;    // Floror(2^k / q) 的近似值 (通常四舍五入)
    K: Integer;      // 幂次 k，用于计算 2^k
    HalfQ: Word;     // q/2 的上取整或下取整，用于中心化约减
    D: Integer;      // 此表项对应的压缩参数 d (如 du 或 dv)
  end;

const
  NTRU_PRE_DEFINED_PARAMS: array[TCnNTRUParamType] of TCnNTRUPredefinedParams = (
    (N: 11; D: 3; P: 3; QExp: 2),
    (N: 251; D: 72; P: 3; QExp: 8),
    (N: 509; D: 127; P: 3; QExp: 11),  // D 内部是 2^QExp div 16 - 1
    (N: 677; D: 127; P: 3; QExp: 11),  // D 内部是 2^QExp div 16 - 1
    (N: 821; D: 255; P: 3; QExp: 12)   // D 内部是 2^QExp div 16 - 1
    // (N: 702; D: 0; P: 3; QExp: 13)
  );

const
  // ML-KEM Barrett Reduction 查找表，包含不同参数集和操作（压缩、解压）所需的约减参数
  MLKEM_BARRETT_TABLE: array[0..4] of TMLKEMBarrettReduce = (
    (MU: 80635;   K: 28; HalfQ: 1665; D: 1),     // round(2^28/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  1 is mlkem512 du
    (MU: 1290167; K: 32; HalfQ: 1665; D: 10),    // round(2^32/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  10 is mlkem768 du
    (MU: 80635;   K: 28; HalfQ: 1665; D: 4),     // round(2^28/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  4 is mlkem768 dv
    (MU: 40318;   K: 27; HalfQ: 1664; D: 5),     // round(2^27/MLKEM_Q), ?, Floor(MLKEM_Q/2), 5 is mlkem1024 dv
    (MU: 645084;  K: 31; HalfQ: 1664; D: 11)     // round(2^31/MLKEM_Q), ?, Floor(MLKEM_Q/2), 11 is mlkem1024 du
  );

const
  // FIPS 203 Appendix A 的 NTT 预计算值
  ZETA_NTT: array[0..127] of Word = (
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
    // 0-7
    17,   2761, 583,  2649, 1637, 723,  2288, 1100,
    // 8-15
    1409, 2662, 3281, 233,  756,  2156, 3015, 3050,
    // 16-23
    1703, 1651, 2789, 1789, 1847, 952,  1461, 2687,
    // 24-31
    939,  2308, 2437, 2388, 733,  2337, 268,  641,
    // 32-39
    1584, 2298, 2037, 3220, 375,  2549, 2090, 1645,
    // 40-47
    1063, 319,  2773, 757,  2099, 561,  2466, 2594,
    // 48-55
    2804, 1092, 403,  1026, 1143, 2150, 2775, 886,
    // 56-63
    1722, 1212, 1874, 1029, 2110, 2935, 885,  2154,
    // 64-71
    17,   2761, 583,  2649, 1637, 723,  2288, 1100,
    // 72-79
    1409, 2662, 3281, 233,  756,  2156, 3015, 3050,
    // 80-87
    1703, 1651, 2789, 1789, 1847, 952,  1461, 2687,
    // 88-95
    939,  2308, 2437, 2388, 733,  2337, 268,  641,
    // 96-103
    1584, 2298, 2037, 3220, 375,  2549, 2090, 1645,
    // 104-111
    1063, 319,  2773, 757,  2099, 561,  2466, 2594,
    // 112-119
    2804, 1092, 403,  1026, 1143, 2150, 2775, 886,
    // 120-127
    1722, 1212, 1874, 1029, 2110, 2935, 885,  2154
  );
var
  FBigNumberPool: TCnBigNumberPool = nil;
  FInt64PolynomialPool: TCnInt64PolynomialPool = nil;
  FBigNumberVectorPool: TCnBigNumberVectorPool = nil;

function Int64GaussianLatticeReduction(V1: TCnInt64Vector; V2: TCnInt64Vector;
  X: TCnInt64Vector; Y: TCnInt64Vector): Boolean;
var
  U1, U2, T: TCnInt64Vector;
  M: Int64;
  K: Extended;
begin
  U1 := nil;
  U2 := nil;
  T := nil;

  try
    U1 := TCnInt64Vector.Create;
    U2 := TCnInt64Vector.Create;
    T := TCnInt64Vector.Create;

    Int64VectorCopy(U1, X);
    Int64VectorCopy(U2, Y);

    if Int64VectorModule(U1) > Int64VectorModule(U2) then
      Int64VectorSwap(U1, U2);

    while True do
    begin
      K := Int64VectorDotProduct(U2, U1) / Int64VectorDotProduct(U1, U1);
      M := Round(K);  // K 可能比取整后的 M 大

      Int64VectorMul(T, U1, M);
      Int64VectorSub(U2, U2, T);
//      if M > K then   // 这里用负似乎意义不大且各版本不一
//        Int64VectorNegate(U2, U2);

      if Int64VectorModule(U1) <= Int64VectorModule(U2) then
      begin
        Int64VectorCopy(V1, U1);
        Int64VectorCopy(V2, U2);
        Result := True;
        Exit;
      end
      else
        Int64VectorSwap(U1, U2);
    end;
  finally
    T.Free;
    U2.Free;
    U1.Free;
  end;
end;

function BigNumberGaussianLatticeReduction(V1: TCnBigNumberVector; V2: TCnBigNumberVector;
  X: TCnBigNumberVector; Y: TCnBigNumberVector): Boolean;
var
  U1, U2, T: TCnBigNumberVector;
  M, M1, M2: TCnBigNumber;
  Ru: Boolean;
begin
  U1 := nil;
  U2 := nil;
  T := nil;
  M := nil;
  M1 := nil;
  M2 := nil;

  try
    U1 := FBigNumberVectorPool.Obtain;
    U2 := FBigNumberVectorPool.Obtain;
    T := FBigNumberVectorPool.Obtain;
    M := FBigNumberPool.Obtain;
    M1 := FBigNumberPool.Obtain;
    M2 := FBigNumberPool.Obtain;

    // 确保 |X| <= |Y|
    BigNumberVectorCopy(U1, X);
    BigNumberVectorCopy(U2, Y);

    BigNumberVectorModuleSquare(M1, U1);
    BigNumberVectorModuleSquare(M2, U2);
    if BigNumberCompare(M1, M2) > 0 then
      BigNumberVectorSwap(U1, U2);

    // U1 := X;  U2 := Y;
    while True do
    begin
      BigNumberVectorDotProduct(M2, U2, U1);
      BigNumberVectorDotProduct(M1, U1, U1);
      BigNumberRoundDiv(M, M2, M1, Ru); // Ru 如果为 True 表示整数 M 比真实结果大

      BigNumberVectorMul(T, U1, M);
      BigNumberVectorSub(U2, U2, T);
//      if Ru then   // 这里用负似乎意义不大且各版本不一
//        BigNumberVectorNegate(U2, U2);

      BigNumberVectorModuleSquare(M1, U1);
      BigNumberVectorModuleSquare(M2, U2);
      if BigNumberCompare(M1, M2) <= 0 then
      begin
        BigNumberVectorCopy(V1, U1);
        BigNumberVectorCopy(V2, U2);
        Result := True;
        Exit;
      end
      else
        BigNumberVectorSwap(U1, U2);
    end;
  finally
    FBigNumberPool.Recycle(M2);
    FBigNumberPool.Recycle(M1);
    FBigNumberPool.Recycle(M);
    FBigNumberVectorPool.Recycle(T);
    FBigNumberVectorPool.Recycle(U2);
    FBigNumberVectorPool.Recycle(U1);
  end;
end;

procedure NTRUDataToInt64Polynomial(Res: TCnInt64Polynomial; Data: Pointer;
  ByteLength: Integer; N, Modulus: Int64; CheckSum: Boolean);
var
  I, Blk, C: Integer;
  Bld: TCnBitBuilder;
  B: Cardinal;
  Sum: Int64;
begin
  Blk := GetUInt64HighBits(Modulus);
  if (Res = nil) or (Blk < 0) or (N <= 1) then
    Exit;

  if Blk > 31 then // 限制在 Cardinal 内的模数
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeModulusTooMuch, [Modulus]);

  if CheckSum then
    C := N - 1  // 读 N - 1 个，第 N 个留着做校验和
  else
    C := N;     // 读 N 个

  // 一共要读　Blk * C 个位，如果待读的内容字节数超出这么多位所占的字节数，则抛出异常
  if ByteLength > (Blk * C + 7) div 8 then
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeDataTooLong, [ByteLength]);

  Bld := TCnBitBuilder.Create;
  try
    Bld.ReadFrom(Data, ByteLength); // 读入了内容
    if Bld.BitLength < Blk * C then  // 如果内容太短不够 Blk * C 个位，则要补上
      Bld.BitLength := Blk * C;

    Res.MaxDegree := N - 1; // 读 N - 1 个时 N - 1 次是校验位，读 N 个时最高 N - 1 次
    Sum := 0;
    for I := 0 to C - 1 do
    begin
      B := Bld.Copy(Blk * I, Blk);  // TODO: 检查是否要小端？
      B := Int64NonNegativeMod(B, Modulus);
      Res[I] := B;
      Sum := Sum + B;
    end;

    if CheckSum then
      Res[N - 1] := -Int64NonNegativeMod(Sum, Modulus);
  finally
    Bld.Free;
  end;
end;

function NTRUInt64PolynomialToData(P: TCnInt64Polynomial; N, Modulus: Int64;
  Data: Pointer; CheckSum: Boolean): Integer;
var
  I, Blk, C: Integer;
  B: Cardinal;
  Bld: TCnBitBuilder;
begin
  Result := 0;
  Blk := GetUInt64HighBits(Modulus);
  if (P = nil) or (Blk < 0) or (N <= 1) then
    Exit;

  if Blk > 31 then // 限制在 Cardinal 内的模数
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeModulusTooMuch, [Modulus]);

  if CheckSum then
    C := N - 1
  else
    C := N;

  // 多项式最多 C 个项，从 0 到 C - 1 次，超过的忽略，不足的会在 Data 后部补 0
  Result := (C * Blk + 7) div 8;
  if Data = nil then
    Exit;

  FillChar(Data^, Result, 0);
  Bld := TCnBitBuilder.Create;
  try
    for I := 0 to C - 1 do
    begin
      B := Cardinal(Int64NonNegativeMod(P[I], Modulus));
      Bld.AppendDWordRange(B, Blk - 1); // 0 到 Blk - 1 共 Blk 位
    end;
    // CheckSum 为 True 时最高的 N - 1 次项是检验项，不参与输入

    Bld.WriteTo(Data);
  finally
    Bld.Free;
  end;
end;

{ TCnNTRUPublicKey }

constructor TCnNTRUPublicKey.Create;
begin
  inherited;
  FH := TCnInt64Polynomial.Create;
end;

destructor TCnNTRUPublicKey.Destroy;
begin
  FH.Free;
  inherited;
end;

procedure TCnNTRUPublicKey.SetFH(const Value: TCnInt64Polynomial);
begin
  Int64PolynomialCopy(FH, Value);
end;

function TCnNTRUPublicKey.ToString: string;
begin
  Result := H.ToString;
end;

{ TCnNTRUPrivateKey }

constructor TCnNTRUPrivateKey.Create;
begin
  inherited;
  FF := TCnInt64Polynomial.Create;
  FG := TCnInt64Polynomial.Create;
  FFP := TCnInt64Polynomial.Create;
  FFQ := TCnInt64Polynomial.Create;
end;

destructor TCnNTRUPrivateKey.Destroy;
begin
  FFQ.Free;
  FFP.Free;
  FG.Free;
  FF.Free;
  inherited;
end;

procedure TCnNTRUPrivateKey.SetFF(const Value: TCnInt64Polynomial);
begin
  Int64PolynomialCopy(FF, Value);
end;

procedure TCnNTRUPrivateKey.SetFFP(const Value: TCnInt64Polynomial);
begin
  Int64PolynomialCopy(FFP, Value);
end;

procedure TCnNTRUPrivateKey.SetFFQ(const Value: TCnInt64Polynomial);
begin
  Int64PolynomialCopy(FFQ, Value);
end;

procedure TCnNTRUPrivateKey.SetFG(const Value: TCnInt64Polynomial);
begin
  Int64PolynomialCopy(FG, Value);
end;

function TCnNTRUPrivateKey.ToString: string;
begin
  Result := FF.ToString + ',' + FG.ToString;
end;

{ TCnNTRU }

constructor TCnNTRU.Create(NTRUType: TCnNTRUParamType);
begin
  inherited Create;
  FRing := TCnInt64Polynomial.Create;
  Load(NTRUType);
end;

procedure TCnNTRU.Decrypt(PrivateKey: TCnNTRUPrivateKey; EnData,
  OutPlainData: TCnInt64Polynomial);
begin
  // 在 Ring 上计算 F * 密文 mod FQ 再 mod Prime 再乘以 Fp mod Prime
  Int64PolynomialGaloisMul(OutPlainData, PrivateKey.F, EnData, FQ, FRing);
  Int64PolynomialCentralize(OutPlainData, FQ);

  Int64PolynomialNonNegativeModWord(OutPlainData, FPrime);
  Int64PolynomialGaloisMul(OutPlainData, OutPlainData, PrivateKey.FP, FPrime, FRing);
  Int64PolynomialCentralize(OutPlainData, FPrime);
end;

function TCnNTRU.DecryptBytes(PrivateKey: TCnNTRUPrivateKey; EnData: TBytes): TBytes;
var
  En, De: TCnInt64Polynomial;
  L: Integer;
begin
  Result := nil;
  En := nil;
  De := nil;

  try
    En := FInt64PolynomialPool.Obtain;
    NTRUDataToInt64Polynomial(En, @EnData[0], Length(EnData), FN, FQ, False);
    // 密文数据转多项式，模数要用大模数，且不需要最高项做校验

    De := FInt64PolynomialPool.Obtain;
    Decrypt(PrivateKey, En, De);

    // 明文多项式转明文数据，模数得用小素数
    L := NTRUInt64PolynomialToData(De, FN, FPrime, nil);
    if L > 0 then
    begin
      SetLength(Result, L);
      NTRUInt64PolynomialToData(De, FN, FPrime, @Result[0]);
    end;
  finally
    FInt64PolynomialPool.Recycle(De);
    FInt64PolynomialPool.Recycle(En);
  end;
end;

destructor TCnNTRU.Destroy;
begin
  FRing.Free;
  inherited;
end;

procedure TCnNTRU.Encrypt(PublicKey: TCnNTRUPublicKey; PlainData,
  OutEnData: TCnInt64Polynomial);
var
  R: TCnInt64Polynomial;
begin
  // 在 Ring 上计算随机 R * H + PlainData mod FQ
  R := nil;

  try
    R := FInt64PolynomialPool.Obtain;
    RandPolynomial(R, FN - 1);

    Int64PolynomialGaloisMul(OutEnData, R, PublicKey.H, FQ, FRing);
    Int64PolynomialGaloisAdd(OutEnData, OutEnData, PlainData, FQ, FRing);
  finally
    FInt64PolynomialPool.Recycle(R);
  end;
end;

function TCnNTRU.EncryptBytes(PublicKey: TCnNTRUPublicKey; Data: TBytes): TBytes;
var
  Pl, En: TCnInt64Polynomial;
  L: Integer;
begin
  Result := nil;
  Pl := nil;
  En := nil;

  try
    Pl := FInt64PolynomialPool.Obtain;
    NTRUDataToInt64Polynomial(Pl, @Data[0], Length(Data), FN, FPrime);
    // 明文数据转明文多项式，模数要用小素数

    En := FInt64PolynomialPool.Obtain;
    Encrypt(PublicKey, Pl, En);

    // 密文多项式转密文数据，模数要用大模数，且最高位无需校验
    L := NTRUInt64PolynomialToData(En, FN, FQ, nil, False);
    if L > 0 then
    begin
      SetLength(Result, L);
      NTRUInt64PolynomialToData(En, FN, FQ, @Result[0], False);
    end;
  finally
    FInt64PolynomialPool.Recycle(En);
    FInt64PolynomialPool.Recycle(Pl);
  end;
end;

procedure TCnNTRU.GenerateKeys(PrivateKey: TCnNTRUPrivateKey;
  PublicKey: TCnNTRUPublicKey);
var
  HasInv: Boolean;
begin
  repeat
    // 随机按数量生成多项式 F，并求逆，确保都存在
    //（似乎 D 个 1、D 个 -1 始终无逆，得用 D + 1 个 1）
    RandPolynomial(PrivateKey.F, FN - 1, D + 1, D);
    HasInv := True;
    try
      Int64PolynomialGaloisModularInverse(PrivateKey.FP, PrivateKey.F,
        FRing, FPrime, True);
    except
      HasInv := False;
    end;

    if HasInv then
    begin
      HasInv := Int64PolynomialGaloisPrimePowerModularInverse(PrivateKey.FQ,
        PrivateKey.F, FRing, 2, FQExponent);
      if HasInv then
        Break;
    end;
  until False;

  // 再随机生成多项式 G，与 F 一起作为私钥，同时 FQ FP 是一大一小俩模逆多项式，存起来备运算
  RandPolynomial(PrivateKey.G, FN - 1, D, D);

  // 计算出 H 后中心化，作为公钥
  Int64PolynomialGaloisMul(PublicKey.H, PrivateKey.FQ, PrivateKey.G, FQ, FRing);
  Int64PolynomialGaloisMulWord(PublicKey.H, FPrime, FQ);
  Int64PolynomialCentralize(PublicKey.H, FQ);
end;

procedure TCnNTRU.Load(Predefined: TCnNTRUParamType);
begin
  FN := NTRU_PRE_DEFINED_PARAMS[Predefined].N;
  FD := NTRU_PRE_DEFINED_PARAMS[Predefined].D;
  FPrime := NTRU_PRE_DEFINED_PARAMS[Predefined].P;
  FQExponent := NTRU_PRE_DEFINED_PARAMS[Predefined].QExp;

  FQ := Int64NonNegativPower(2, FQExponent);

  FRing.SetZero;
  FRing.MaxDegree := N;
  FRing[N] := 1;
  FRing[0] := -1;
end;

procedure TCnNTRU.RandPolynomial(P: TCnInt64Polynomial; MaxDegree,
  OneCount, MinusOneCount: Integer);
var
  F: array of Integer;
  I: Integer;
begin
  if (MaxDegree < 0) or (OneCount < 0) or (MinusOneCount < 0) or
    (OneCount + MinusOneCount >= MaxDegree) then
    raise ECnLatticeException.Create(SCnErrorLatticeNTRUInvalidParam);

  SetLength(F, MaxDegree + 1);
  for I := 0 to OneCount - 1 do
    F[I] := 1;
  for I := OneCount to OneCount + MinusOneCount - 1 do
    F[I] := -1;
  for I := OneCount + MinusOneCount to MaxDegree do
    F[I] := 0;

  // 洗牌算法
  CnKnuthShuffle(@F[0], SizeOf(Integer), Length(F));

  P.MaxDegree := MaxDegree;
  for I := 0 to MaxDegree do
    P[I] := F[I];

  SetLength(F, 0);
end;

procedure TCnNTRU.RandPolynomial(P: TCnInt64Polynomial; MaxDegree: Integer);
var
  I: Integer;
begin
  if MaxDegree < 0 then
    raise ECnLatticeException.Create(SCnErrorLatticeNTRUInvalidParam);

  P.MaxDegree := MaxDegree;
  for I := 0 to MaxDegree do
    P[I] := RandomUInt32LessThan(3) - 1; // [0, 3) 也就是 0 1 2 都减一就是 -1 0 1
end;

// ================================ MLKEM ======================================

procedure CheckEta(Eta: Integer);
begin
  if (Eta <> 2) and (Eta <> 3) then
    raise ECnLatticeException.Create(SCnErrorLatticeEtaMustBe2Or3);
end;

procedure CheckEncodeDigit(D: Integer);
begin
  if not D in [1.. 12] then
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidEncodeDigit);
end;

// 把每个数的低 D 位取出来紧拼到一起
function ByteEncode(W: TWords; D: Integer): TBytes; overload;
var
  I: Integer;
  B: TCnBitBuilder;
begin
  CheckEncodeDigit(D);

  B := TCnBitBuilder.Create;
  try
    for I := 0 to Length(W) - 1 do
      B.AppendWordRange(W[I], D - 1);

    Result := B.ToBytes;
  finally
    B.Free;
  end;
end;

function ByteEncode(P: TCnMLKEMPolynomial; D: Integer): TBytes; overload;
var
  I: Integer;
  B: TCnBitBuilder;
begin
  CheckEncodeDigit(D);

  B := TCnBitBuilder.Create;
  try
    for I := Low(P) to High(P) do
      B.AppendWordRange(P[I], D - 1);

    Result := B.ToBytes;
  finally
    B.Free;
  end;
end;

function ByteDecode(B: TBytes; D: Integer): TWords;
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

  CheckEncodeDigit(D);

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

function DivMlKemQ(X: Word; B, HQ, BS: Integer; BM: TUInt64): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  R: TUInt64;
begin
  R := (TUInt64(X) shl B) + TUInt64(HQ);
  R := UInt64Mul(R, BM);
  R := R shr BS;
  Result := Word(R and ((1 shl B) - 1));
end;

function BitRev7(X: Byte): Byte; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 6 do
  begin
    Result := (Result shl 1) or (X and 1);
    X := X shr 1;
  end;
end;

// 模素数加减乘法
function ModAdd(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A + B) mod CN_MLKEM_PRIME;
end;

function ModSub(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if A >= B then
    Result := A - B
  else
    Result := CN_MLKEM_PRIME + A - B;
end;

function ModMul(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Word(Cardinal(A) * Cardinal(B) mod CN_MLKEM_PRIME);
end;

// 数论变换
function NTT(const F: TWords): TWords;
var
  Len, Start, J, I: Integer;
  Zeta, T: Word;
begin
  SetLength(Result, 256);
  Move(F[0], Result[0], 256 * SizeOf(Word));

  I := 1;
  Len := 128;

  while Len >= 2 do
  begin
    Start := 0;
    while Start < 256 do
    begin
      Zeta := ZETA_NTT[BitRev7(I) mod 128];
      Inc(I);

      for J := Start to Start + Len - 1 do
      begin
        T := ModMul(Zeta, Result[J + Len]);
        Result[J + Len] := ModSub(Result[J], T);
        Result[J] := ModAdd(Result[J], T);
      end;

      Inc(Start, 2 * Len);
    end;

    Len := Len div 2;
  end;
end;

function INTT(const F: TWords): TWords;
var
  Len, Start, J, I: Integer;
  Zeta, T: Word;
begin
  SetLength(Result, 256);
  Move(F[0], Result[0], 256 * SizeOf(Word));

  I := 127;
  Len := 2;

  while Len <= 128 do
  begin
    Start := 0;
    while Start < 256 do
    begin
      Zeta := ZETA_NTT[BitRev7(I) mod 128];
      Dec(I);

      for J := Start to Start + Len - 1 do
      begin
        T := Result[J];
        Result[J] := ModAdd(T, Result[J + Len]);
        Result[J + Len] := ModMul(Zeta, ModSub(Result[J + Len], T));
      end;

      Inc(Start, 2 * Len);
    end;

    Len := Len * 2;
  end;

  // 最终缩放：乘以 3303，是 128 对 3329 的模逆元
  for J := 0 to 255 do
    Result[J] := ModMul(Result[J], CN_MLKEM_PRIME_INV);
end;

// 俩多项式相加
procedure PolyAddInNTT(var Res: TCnMLKEMPolynomial; const V1, V2: TCnMLKEMPolynomial);
var
  I: Integer;
begin
  for I := Low(V1) to High(V1) do
    Res[I] := ModAdd(V1[I], V2[I]);
end;

// 俩排多项式相加，或者说是俩多项式向量相加
procedure VectorAddInNTT(var Res: TCnMLKEMPolyVector; const V1, V2: TCnMLKEMPolyVector);
var
  I: Integer;
begin
  for I := Low(V1) to High(V1) do
    PolyAddInNTT(Res[I], V1[I], V2[I]);
end;

procedure BaseCaseMultiply(A0, A1, B0, B1, Gamma: Word; out C0, C1: Word);
begin
  // C0 = A0 * B0 + A1 * B1 * Gamma
  C0 := ModAdd(ModMul(A0, B0), ModMul(ModMul(A1, B1), Gamma));

  // C1 = A0 * B1 + A1 * B0
  C1 := ModAdd(ModMul(A0, B1), ModMul(A1, B0));
end;

// NTT 上俩多项式相乘
procedure PolyMulInNTT(var Res: TCnMLKEMPolynomial; const F, G: TCnMLKEMPolynomial);
var
  I: Integer;
  C0, C1: Word;
begin
  for I := 0 to 127 do
  begin
    // 对每个二次分量进行乘法
    BaseCaseMultiply(F[2 * I], F[2 * I + 1], G[2 * I], G[2 * I + 1],
      ZETA_BASE_CASE[I], C0, C1);

    Res[2 * I] := C0;
    Res[2 * I + 1] := C1;
  end;
end;

// NTT 上方阵乘以一个多项式向量，得到一个多项式向量
procedure MatrixMulVectorInNTT(var Res: TCnMLKEMPolyVector;
  const A: TCnMLKEMPolyMatrix; const S: TCnMLKEMPolyVector);
var
  I, J: Integer;
  T: TCnMLKEMPolynomial;
begin
  SetLength(Res, Length(S));

  for I := Low(Res) to High(Res) do
  begin
    // 初始化结果多项式为零
    FillChar(Res[I], SizeOf(TCnMLKEMPolyVector), 0);

    for J := Low(S) to High(S) do
    begin
      PolyMulInNTT(T, A[I, J], S[J]);

      // 累加到结果中
      PolyAddInNTT(Res[I], Res[I], T);
    end;
  end;
end;

{ TCnMLKEM }

class function TCnMLKEM.Compress(X, D: Word): Word;
var
  V, T: Word;
  I: Integer;
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

constructor TCnMLKEM.Create(AType: TCnMLKEMType);
begin
  inherited Create;
  FNoise2 := 2;

  FRing := TCnInt64Polynomial.Create;
  FRing.SetCoefficent(CN_MLKEM_POLY_DEGREE, 1);
  FRing.SetCoefficent(0, 1);

  case AType of
    cmkt512:
      begin
        FMatrixRank := 2;
        FNoise1 := 3;
      end;
    cmkt768:
      begin
        FMatrixRank := 3;
        FNoise1 := 2;
      end;
    cmkt1024:
      begin
        FMatrixRank := 4;
        FNoise1 := 2;
      end;
  else
    raise ECnLatticeException.Create(SCnErrorLatticeMLKEMInvalidParam);
  end;
end;

class function TCnMLKEM.Decompress(X: Word; D: Word): Word;
var
  P: Cardinal;
begin
  P := Cardinal(X) * CN_MLKEM_PRIME;

  Result := Word((P shr D) +                // 商（主干部分）
    ((P and ((1 shl D) - 1)) shr (D - 1))); // 四舍五入的进位项
end;

destructor TCnMLKEM.Destroy;
begin
  FRing.Free;
  inherited;
end;

class procedure TCnMLKEM.GFunc(const Data: TBytes; out Block1,
  Block2: TCnMLKEMBlock);
var
  Dig: TCnSHA3_512Digest;
begin
  Dig := SHA3_512Bytes(Data);
  Move(Dig[0], Block1[0], SizeOf(TCnMLKEMBlock));
  Move(Dig[SizeOf(TCnMLKEMBlock)], Block2[0], SizeOf(TCnMLKEMBlock));
end;

class function TCnMLKEM.HFunc(const Data: TBytes): TCnMLKEMBlock;
var
  Dig: TCnSHA3_256Digest;
begin
  Dig := SHA3_256Bytes(Data);
  Move(Dig[0], Result[0], SizeOf(TCnMLKEMBlock));
end;

class function TCnMLKEM.JFunc(const Data: TBytes): TCnMLKEMBlock;
var
  Dig: TBytes;
begin
  Dig := SHAKE256Bytes(Data, SizeOf(TCnMLKEMBlock));
  Move(Dig[0], Result[0], SizeOf(TCnMLKEMBlock));
end;

procedure TCnMLKEM.KPKEKeyGen(const D: TCnMLKEMSeed; out GenerationSeed: TCnMLKEMSeed;
  out Secret, Pub: TCnMLKEMPolyVector);
var
  I, J, N: Integer;
  O: TCnMLKEMSeed;
  DK, PJI, R: TBytes;
  W: TWords;
  Matrix: TCnMLKEMPolyMatrix;
  Noise: TCnMLKEMPolyVector;
begin
  SetLength(DK, 1);
  DK[0] := FMatrixRank;
  DK := ConcatBytes(NewBytesFromMemory(@D[0], SizeOf(TCnMLKEMBlock)), DK);

  GFunc(DK, TCnMLKEMBlock(GenerationSeed), TCnMLKEMBlock(O));
  N := 0;

  // 设置矩阵大小
  SetLength(Matrix, FMatrixRank);
  for I := 0 to FMatrixRank - 1 do
    SetLength(Matrix[I], FMatrixRank);

  // 准备好 Sample 随机数据
  SetLength(PJI, SizeOf(TCnMLKEMSeed) + 2);
  Move(GenerationSeed[0], PJI[0], SizeOf(TCnMLKEMSeed));

  // 生成矩阵
  for I := 0 to FMatrixRank - 1 do
  begin
    for J := 0 to FMatrixRank - 1 do
    begin
      PJI[SizeOf(TCnMLKEMSeed)] := J;
      PJI[SizeOf(TCnMLKEMSeed) + 1] := I;
      W := SampleNTT(PJI);
      Move(W[0], Matrix[I][J][0], Length(W) * SizeOf(Word));
    end;
  end;

  // 生成 K 个 S
  SetLength(Secret, FMatrixRank);
  for I := 0 to FMatrixRank - 1 do
  begin
    R := PseudoRandomFunc(FNoise1, O, N);
    W := SamplePolyCBD(R, FNoise1);
    W := NTT(W);
    Move(W[0], Secret[I][0], Length(W) * SizeOf(Word));
    Inc(N);
  end;

  // 生成 K 个 E
  SetLength(Noise, FMatrixRank);
  for I := 0 to FMatrixRank - 1 do
  begin
    R := PseudoRandomFunc(FNoise1, O, N);
    W := SamplePolyCBD(R, FNoise1);
    W := NTT(W);
    Move(W[0], Noise[I][0], Length(W) * SizeOf(Word));
    Inc(N);
  end;

  // 计算 T = A * S + E
  MatrixMulVectorInNTT(Pub, Matrix, Secret);
  VectorAddInNTT(Pub, Pub, Noise);
end;

procedure TCnMLKEM.MLKEMKeyGen(EncapKey: TCnMLKEMEncapsulationKey;
  DecapKey: TCnMLKEMDecapsulationKey; const RandDHex: string; const RandZHex: string);
var
  D: TCnMLKEMSeed;
begin
  if Length(RandDHex) = 0 then
    CnRandomFillBytes(@D[0], SizeOf(TCnMLKEMSeed))
  else
    PutBytesToMemory(HexToBytes(RandDHex), @D[0], SizeOf(TCnMLKEMSeed));

  if Length(RandZHex) = 0 then
    CnRandomFillBytes(@DecapKey.FInjectionSeed[0], SizeOf(TCnMLKEMSeed))
  else
    PutBytesToMemory(HexToBytes(RandZHex), @DecapKey.FInjectionSeed[0], SizeOf(TCnMLKEMSeed));

  KPKEKeyGen(D, EncapKey.FGenerationSeed, EncapKey.FPubVector, DecapKey.FSecretVector);
end;

class function TCnMLKEM.PseudoRandomFunc(Eta: Integer;
  const Input: TCnMLKEMSeed; B: Byte): TBytes;
var
  T: TBytes;
begin
  if (Eta <> 2) and (Eta <> 3) then
    raise ECnLatticeException.Create(SCnErrorLatticeEtaMustBe2Or3);

  SetLength(T, SizeOf(TCnMLKEMSeed) + 1);
  Move(Input[0], T[0], SizeOf(TCnMLKEMSeed));
  T[SizeOf(TCnMLKEMSeed)] := B;

  Result := SHAKE256Bytes(T, Eta * 64);
end;

class function TCnMLKEM.SampleNTT(const RandBytes: TBytes): TWords;
var
  Ctx: TCnSHA3Context;
  C: TBytes;
  D1, D2: Integer;
  J: Integer;
begin
  if Length(RandBytes) < CN_MLKEM_KEY_SIZE + 2 then
    raise Exception.Create(SCnErrorLatticeInvalidSampleNTT);

  SetLength(Result, CN_MLKEM_POLY_DEGREE);

  SHAKE128Init(Ctx, 0);
  SHAKE128Absorb(Ctx, PAnsiChar(@RandBytes[0]), Length(RandBytes));

  J := 0;
  while J < CN_MLKEM_POLY_DEGREE do
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
      if J >= CN_MLKEM_POLY_DEGREE then
        Break;
    end;

    // 检查第二个值是否有效
    if (D2 < CN_MLKEM_PRIME) and (J < CN_MLKEM_POLY_DEGREE) then
    begin
      Result[J] := D2;
      Inc(J);
    end;
  end;
end;

class function TCnMLKEM.SamplePolyCBD(const RandBytes: TBytes; Eta: Integer): TWords;
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
  CheckEta(Eta);
  if Length(RandBytes) < 64 * Eta then
    raise Exception.Create(SCnErrorLatticeInvalidRandomLength);

  SetLength(Result, CN_MLKEM_POLY_DEGREE);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendBytes(RandBytes);

    for I := 0 to CN_MLKEM_POLY_DEGREE - 1 do
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

{ TCnMLKEMPublicKey }

constructor TCnMLKEMEncapsulationKey.Create;
begin
  inherited Create;

end;

destructor TCnMLKEMEncapsulationKey.Destroy;
begin

  inherited;
end;

{ TCnMLKEMPrivateKey }

constructor TCnMLKEMDecapsulationKey.Create;
begin
  inherited Create;
end;

destructor TCnMLKEMDecapsulationKey.Destroy;
begin

  inherited;
end;

function TCnMLKEM.SaveKeysToBytes(DecapKey: TCnMLKEMDecapsulationKey;
  EncapKey: TCnMLKEMEncapsulationKey): TBytes;
var
  I: Integer;
  EK, DK: TBytes;
  B: TCnMLKEMBlock;
begin
  EK := SaveKeyToBytes(EncapKey);

  DK := nil;
  for I := 0 to FMatrixRank - 1 do
    DK := ConcatBytes(DK, ByteEncode(DecapKey.SecretVector[I], 12));

  // dk || ek || H(ek) || z
  Result := ConcatBytes(DK, EK);
  B := HFunc(EK);
  Result := ConcatBytes(Result, NewBytesFromMemory(@B[0], SizeOf(TCnMLKEMBlock)));
  Result := ConcatBytes(Result, NewBytesFromMemory(@DecapKey.FInjectionSeed[0], SizeOf(TCnMLKEMBlock)));
end;

function TCnMLKEM.SaveKeyToBytes(EncapKey: TCnMLKEMEncapsulationKey): TBytes;
var
  I: Integer;
begin
  // T 拼上 PubSeed 输出作为 EK
  Result := nil;
  for I := 0 to FMatrixRank - 1 do
    Result := ConcatBytes(Result, ByteEncode(EncapKey.PubVector[I], 12));

  Result := ConcatBytes(Result, NewBytesFromMemory(@EncapKey.GenerationSeed[0], SizeOf(TCnMLKEMSeed)));
end;

initialization
  FBigNumberPool := TCnBigNumberPool.Create;
  FInt64PolynomialPool := TCnInt64PolynomialPool.Create;
  FBigNumberVectorPool := TCnBigNumberVectorPool.Create;

finalization
  FBigNumberVectorPool.Free;
  FInt64PolynomialPool.Free;
  FBigNumberPool.Free;

end.
