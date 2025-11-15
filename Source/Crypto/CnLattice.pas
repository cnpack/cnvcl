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
* 备    注：本单元简略实现了基于格（Lattice）的 NTRU （数论研究单元）公开密钥加解密算法
*           及 MLKEM （基于模块化格上的学习误差问题的密钥封装）算法。
*           MLKEM 基于 NIST 的 FIPS 203 规范。
*
*           NTRU 只支持较短的明文加解密。
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2025.11.10 V1.2
*               实现 MLKEM 的加解密与密钥封装解封算法
*           2023.09.10 V1.1
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
  {* MLKEM 的共享密钥及种子等的长度}

  CN_MLKEM_POLY_SIZE   = 256;
  {* MLKEM 的多项式尺寸，最高次数是 255 次}

  CN_MLKEM_PRIME       = 3329;
  {* MLKEM 使用的素数}

  CN_MLKEM_PRIME_INV   = 3303;
  {* MLKEM 使用的 128 对该素数的模逆元}

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
  ECnLatticeException = class(Exception);
  {* NTRU/MLKEM相关异常}

  TCnNTRUParamType = (cnptCustomized, cnptClassic, cnptHPS2048509, cnptHPS2048677,
    cnptHPS4096821);
  {* NTRU 几个推荐参数，其中 cnptCustomized 需要自行指定参数，
     如参数不合适则有安全问题，故不推荐使用。}

  TCnMLKEMType = (cmkt512, cmkt768, cmkt1024);
  {* MLKEM 的三种实现规范}

  TCnMLDSAType = (cmdt44, cmdt65, cmdt87);
  {* MLDSA 的三种实现规范}

  TCnMLDSAHashType = (cmhtNone, cmhtSHA256, cmhtSHA512, cmhtSHAKE128);
  {* MLDSA 支持的杂凑算法}

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
    FQ: Int64;   // 提前计算出的模数 2^FQExponent
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

  TCnMLKEMPolynomial = array[0..CN_MLKEM_POLY_SIZE - 1] of Word;
  {* MLKEM 多项式系数，256 个双字，用来表达一个多项式}

  TCnMLKEMPolyVector = array of TCnMLKEMPolynomial;
  {* MLKEM 多项式列表或叫向量，用来表达 S 或 E 等}

  TCnMLKEMPolyMatrix = array of TCnMLKEMPolyVector;
  {* MLKEM 多项式矩阵，用来表达 A}

  TCnMLKEMDecapsulationKey = class
  {* MLKEM 的非公开解封密钥，包括秘密多项式向量与隐式拒绝的随机种子}
  private
    FSecretVector: TCnMLKEMPolyVector;
    FInjectionSeed: TCnMLKEMSeed;
    FEnKeyHash: TCnMLKEMSeed;
  public
    property SecretVector: TCnMLKEMPolyVector read FSecretVector;
    {* 秘密多项式向量，相当于规范里的 S，系数已 NTT 化}
    property InjectionSeed: TCnMLKEMSeed read FInjectionSeed;
    {* 用于隐式拒绝的随机种子，不参与密钥生成，相当于规范里的 Z}
    property EnKeyHash: TCnMLKEMSeed read FEnKeyHash;
    {* 对应公开密钥的杂凑值} 
  end;

  TCnMLKEMEncapsulationKey = class
  {* MLKEM 的封装密钥，可公开，包括可用来生成矩阵的种子，以及公钥多项式向量}
  private
    FGenerationSeed: TCnMLKEMSeed;
    FPubVector: TCnMLKEMPolyVector;            // 私钥与矩阵计算出的公钥多项式向量
  public
    property GenerationSeed: TCnMLKEMSeed read FGenerationSeed;
    {* 用于生成整套密钥的主随机种子，相当于规范里的 D}
    property PubVector: TCnMLKEMPolyVector read FPubVector;
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
    function GetEncapKeyByteLength: Integer;
    function GetDecapKeyByteLength: Integer;
    function GetCipherUPolyByteLength: Integer;
    function GetCipherUByteLength: Integer;
    function GetCipherVByteLength: Integer;
    function GetCipherByteLength: Integer;

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

    procedure CheckEncapKey(EnKey: TBytes);
    {* 检查公开密钥字节数组是否合法，不合法则抛异常}
    procedure CheckDecapKey(DeKey: TBytes);
    {* 检查非公开密钥字节数组是否合法，不合法则抛异常}
    procedure CheckKeyPair(EncapKey: TCnMLKEMEncapsulationKey; DecapKey: TCnMLKEMDecapsulationKey);
    {* 检查一对 Key 是否匹配}
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
    {* 秘密多项式向量 S1，维度为矩阵列数}
    property S2: TCnMLDSAPolyVector read FS2 write FS2;
    {* 秘密多项式向量 S2，维度为矩阵行数}
    property T0: TCnMLDSAPolyVector read FT0 write FT0;
    {* 矩阵运算得到的多项式向量 T 的分离私钥部分 T0，维度为矩阵行数}
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
    function GetNoiseBitLength: Integer;

    procedure GenerateMatrix(const Seed: TCnMLDSASeed; out Matrix: TCnMLDSAPolyMatrix);
    {* 根据种子生成矩阵 A，系数是 NTT 形式，FMatrixRowCount 行，FMatrixColCount 列}
    procedure GenerateSecret(const Seed: TCnMLDSAKeyDigest; out S1, S2: TCnMLDSAPolyVector);
    {* 根据种子生成两个秘密多项式向量，系数是非 NTT 形式。S1 维度 FMatrixColCount，S2 维度 FMatrixRowCount}
  protected

  public
    constructor Create(AType: TCnMLDSAType); virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure GenerateKeys(PrivateKey: TCnMLDSAPrivateKey; PublicKey: TCnMLDSAPublicKey;
      const RandHex: string = '');
    {* 用一个真随机 32 字节种子，生成一对 Key，如果不传则内部产生随机数生成}

    procedure LoadPrivateKeyFromBytes(PrivateKey: TCnMLDSAPrivateKey; const SK: TBytes);
    {* 从字节数组中加载私钥}
    procedure LoadPublicKeyFromBytes(PublicKey: TCnMLDSAPublicKey; const PK: TBytes);
    {* 从字节数组中加载公钥}

    function SavePrivateKeyToBytes(PrivateKey: TCnMLDSAPrivateKey): TBytes;
    {* 将私钥保存成字节数组 SK}
    function SavePublicKeyToBytes(PublicKey: TCnMLDSAPublicKey): TBytes;
    {* 将公钥保存成字节数组 PK}

    property MLDSAType: TCnMLDSAType read FMLDSAType;
    {* MLDSA 的算法类型，44、65、87 三种}
    property MatrixRowCount: Integer read FMatrixRowCount write FMatrixRowCount;
    {* 矩阵行数，对应规范中的 k}
    property MatrixColCount: Integer read FMatrixColCount write FMatrixColCount;
    {* 矩阵列数，对应规范中的 l}
    property Noise: Integer read FNoise write FNoise;
    {* 噪声参数，也就是私钥系数正负范围，对应规范中的 Eta}
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
     N: Int64                             - 多项式位数，一般取 NTRU 的 N
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

procedure MLDSAPolynomialMul(var Res: TCnMLDSAPolynomial; const P1, P2: TCnMLDSAPolynomial;
  IsNTT: Boolean = True);
{* 两个 MKDSA 格式的多项式在 mod 8380417 及 x^256 + 1 的多项式环上相乘。
   IsNTT 指示参数是否是 NTT 模式，是则执行 NTT 乘法，不是则执行普通乘法。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式积
     P1: TCnMLDSAPolynomial               - MKDSA 格式的多项式乘数一
     P2: TCnMLDSAPolynomial               - MKDSA 格式的多项式乘数二
     IsNTT: Boolean                       - 多项式系数是否是 NTT 模式

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
  const P1: TCnMLDSAPolyVector; const P2: TCnMLDSAPolyVector);
{* 两个 MKDSA 格式的多项式向量在 mod 8380417 有限域中相加，NTT 系数或 非 NTT 系数均适用。

   参数：
     Res: TCnMLDSAPolynomial              - MKDSA 格式的多项式向量和
     P1: TCnMLDSAPolynomial               - MKDSA 格式的多项式向量加数一
     P2: TCnMLDSAPolynomial               - MKDSA 格式的多项式向量加数二

   返回值：（无）
}

procedure MLDSAMatrixVectorMul(var Res: TCnMLDSAPolyVector;
  const A: TCnMLDSAPolyMatrix; const S: TCnMLDSAPolyVector; IsNTT: Boolean = True);
{* 一个 MKDSA 格式的多项式矩阵在 mod 8380417 及 x^256 + 1 的多项式环上乘以一个多项式向量，
   得到一个多项式向量。用户需自行确保 A 的列数与 S 的维度一致，结果维度为 A 的行数。

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
  SCnErrorLatticeNTRUInvalidParam = 'Invalid NTRU Value.';
  SCnErrorLatticeModulusTooMuch = 'Modulus Too Much %d';
  SCnErrorLatticeDataTooLong = 'Data Too Long %d';
  SCnErrorLatticeMLKEMInvalidParam = 'Invalid MLKEM Value';
  SCnErrorLatticeEtaMustBe2Or3 = 'Eta Must Be 2 or 3';
  SCnErrorLatticeInvalidRandomLength = 'Invalid Random Length for SamplePolyCBD';
  SCnErrorLatticeInvalidSampleNTT = 'Sample NTT Function Input Must Be 34 Bytes';
  SCnErrorLatticeInvalidEncodeDigit = 'Digit Must Be Between 1 and 12';
  SCnErrorLatticeEncapKeyLengthMismatch = 'Encapsulation Key Length Mismatch. Expected %d, Got %d';
  SCnErrorLatticeEncapKeyModulusCheckFailed = 'Encapsulation Key Modulus Check Failed';
  SCnErrorLatticeDecapKeyLengthMismatch = 'Decapsulation Key Length Mismatch. Expected %d, Got %d';
  SCnErrorLatticeDecapKeyStructureInvalid = 'Invalid Decapsulation Key Structure: Can NOT Extract Encapsulation Key';
  SCnErrorLatticeDecapKeyHashFailed = 'Decapsulation Key Hash Verification Failed';
  SCnErrorLatticeInvalidMsgLength = 'Invalid Message Length';
  SCnErrorLatticeInvalidHexLength = 'Invalid Random Hex Length';
  SCnErrorLatticeCipherLengthMismatch = 'Cipher Length Mismatch. Expected %d, Got %d';
  SCnErrorLatticeKeyPairCheckFail = 'Key Pair Check Failed';
  SCnErrorLatticeInvalidPolynomialDegree = 'Invalid Polynomial Degree';
  SCnErrorLatticeMLDSAInvalidParam = 'Invalid MLDSA Value';
  SCnErrorLatticeMLDSAKeyLengthMismatch = 'MLDSA Key Length Mismatch. Expected %d, Got %d';
  SCnErrorLatticeMLDSAPackLengthMismatch = 'MLDSA Pack Length Mismatch. Expected %d, Got %d';

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

  OID_MLDSA_PREHASH_SHA256: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.1
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $01
  );

  OID_MLDSA_PREHASH_SHA512: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.3
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $03
  );

  OID_MLDSA_PREHASH_SHAKE128: array[0..10] of Byte = ( // 2.16.840.1.101.3.4.2.11
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $0B
  );

var
  FBigNumberPool: TCnBigNumberPool = nil;
  FInt64PolynomialPool: TCnInt64PolynomialPool = nil;
  FBigNumberVectorPool: TCnBigNumberVectorPool = nil;
  FMLKEMRing: TCnInt64Polynomial = nil;

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

procedure MLKEMCheckEta(Eta: Integer);
begin
  if (Eta <> 2) and (Eta <> 3) then
    raise ECnLatticeException.Create(SCnErrorLatticeEtaMustBe2Or3);
end;

procedure MLKEMCheckEncodeDigit(D: Integer);
begin
  if not D in [1.. 12] then
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidEncodeDigit);
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
    raise Exception.Create(SCnErrorLatticeInvalidSampleNTT);

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
    raise Exception.Create(SCnErrorLatticeInvalidRandomLength);

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
    raise ECnLatticeException.Create(SCnErrorLatticeEtaMustBe2Or3);

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
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeDecapKeyLengthMismatch,
      [ExpLen, Length(DeKey)]);

  // 提取 ek 部分: dk[384k : 768k+32]
  EkStart := 384 * FMatrixSize;
  EkLength := GetEncapKeyByteLength;

  if (EkStart + EkLength) > Length(DeKey) then
    raise ECnLatticeException.Create(SCnErrorLatticeDecapKeyStructureInvalid);

  SetLength(EkBytes, EkLength);
  Move(DeKey[EkStart], EkBytes[0], EkLength);

  // 使用已有的封装密钥检查方法
  CheckEncapKey(EkBytes);

  // 杂凑检查，验证 H(ek) 是否正确
  // H(ek) 位于 dk[768k+32 : 768k+64]
  HStart := 768 * FMatrixSize + 32;
  HLength := SizeOf(TCnMLKEMBlock);

  if (HStart + HLength) > Length(DeKey) then
    raise ECnLatticeException.Create(SCnErrorLatticeDecapKeyStructureInvalid);

  Move(DeKey[HStart], HBytes[0], HLength);

  // 计算 ek 的杂凑值
  ComputedHash := MLKEMHFunc(EkBytes);

  // 比较计算出的杂凑值与存储的杂凑值
  if not CompareMem(@HBytes[0], @ComputedHash[0], SizeOf(TCnMLKEMBlock)) then
    raise ECnLatticeException.Create(SCnErrorLatticeDecapKeyHashFailed);
end;

procedure TCnMLKEM.CheckEncapKey(EnKey: TBytes);
var
  PolyBytesLength: Integer;
  PolyBytes, TestBytes: TBytes;
begin
  if Length(EnKey) <> GetEncapKeyByteLength then
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeEncapKeyLengthMismatch,
      [GetEncapKeyByteLength, Length(EnKey)]);

  PolyBytesLength := 384 * FMatrixSize;

  // 提取多项式部分字节
  SetLength(PolyBytes, PolyBytesLength);
  Move(EnKey[0], PolyBytes[0], PolyBytesLength);

  // 进行 ByteDecode12 然后 ByteEncode12 往返
  TestBytes := MLKEMByteEncode(MLKEMByteDecode(PolyBytes, 12), 12);
  if not CompareBytes(PolyBytes, TestBytes) then
    raise ECnLatticeException.Create(SCnErrorLatticeEncapKeyModulusCheckFailed);
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
    raise ECnLatticeException.Create(SCnErrorLatticeKeyPairCheckFail);
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
    raise ECnLatticeException.Create(SCnErrorLatticeMLKEMInvalidParam);
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
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeEncapKeyLengthMismatch, 
      [PolyLength + SizeOf(TCnMLKEMSeed), Length(Key)]);

  // 设置公钥多项式向量的大小
  SetLength(EncapKey.FPubVector, FMatrixSize);

  // 解析多项式部分
  for I := 0 to FMatrixSize - 1 do
  begin
    // 提取每个多项式的字节数据（每个384字节）
    SetLength(PolyBytes, 384);
    Move(Key[I * 384], PolyBytes[0], 384);

    // 将字节解码为多项式系数
    // 注意这里需要将解码后的 TWords 转换为 TCnMLKEMPolynomial
    // 假设 ByteDecode 返回的 TWords 长度是 256
    Move(MLKEMByteDecode(PolyBytes, 12)[0], EncapKey.FPubVector[I][0],
      CN_MLKEM_POLY_SIZE * SizeOf(Word));
  end;

  // 解析生成种子部分（最后 32 字节）
  SeedStart := PolyLength;
  Move(Key[SeedStart], EncapKey.FGenerationSeed[0], SizeOf(TCnMLKEMSeed));
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
  SetLength(DecapKey.FSecretVector, FMatrixSize);
  for I := 0 to FMatrixSize - 1 do
  begin
    // 提取每个秘密多项式的字节数据（每个 384 字节）
    SetLength(PolyBytes, 384);
    Move(Key[I * 384], PolyBytes[0], 384);

    // 将字节解码为多项式系数
    Move(MLKEMByteDecode(PolyBytes, 12)[0], DecapKey.FSecretVector[I][0],
      CN_MLKEM_POLY_SIZE * SizeOf(Word));
  end;

  // Key 字节数组中的杂凑部分加载进非公开密钥的杂凑值中
  Move(Key[768 * FMatrixSize + 32], DecapKey.FEnKeyHash[0], SizeOf(TCnMLKEMSeed));

  // 解析封装密钥部分 (ek)
  EkStart := PolyLength; // 384k
  EkLength := GetEncapKeyByteLength; // ek 部分的长度

  SetLength(EkBytes, EkLength);
  Move(Key[EkStart], EkBytes[0], EkLength);

  // 使用已有的 LoadKeyFromBytes 方法加载封装密钥
  LoadKeyFromBytes(EkBytes, EncapKey);

  // 解析注入种子部分 (z)
  ZStart := 768 * FMatrixSize + 64; // dk(384k) + ek(384k+32) + H(ek)(32) = 768k+64
  Move(Key[ZStart], DecapKey.FInjectionSeed[0], SizeOf(TCnMLKEMSeed));
end;

procedure TCnMLKEM.GenerateKeys(EncapKey: TCnMLKEMEncapsulationKey;
  DecapKey: TCnMLKEMDecapsulationKey; const RandDHex: string; const RandZHex: string);
var
  D: TCnMLKEMSeed;
  B: TBytes;
begin
  if ((Length(RandDHex) > 0) and (Length(RandDHex) <> 64)) or
    ((Length(RandZHex) > 0) and (Length(RandZHex) <> 64)) then
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidHexLength);

  if Length(RandDHex) = 0 then
    CnRandomFillBytes(@D[0], SizeOf(TCnMLKEMSeed))
  else
    PutBytesToMemory(HexToBytes(RandDHex), @D[0], SizeOf(TCnMLKEMSeed));

  if Length(RandZHex) = 0 then
    CnRandomFillBytes(@DecapKey.FInjectionSeed[0], SizeOf(TCnMLKEMSeed))
  else
    PutBytesToMemory(HexToBytes(RandZHex), @DecapKey.FInjectionSeed[0], SizeOf(TCnMLKEMSeed));

  // 生成密钥
  KPKEKeyGen(D, EncapKey.FGenerationSeed, DecapKey.FSecretVector, EncapKey.FPubVector);

  // 公开密钥的杂凑值，放非公开密钥里备用
  B := SaveEncapKeyToBytes(EncapKey);
  DecapKey.FEnKeyHash := TCnMLKEMSeed(MLKEMHFunc(B));
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
  Result := ConcatBytes(Result, NewBytesFromMemory(@DecapKey.FEnKeyHash[0], SizeOf(TCnMLKEMSeed)));
  Result := ConcatBytes(Result, NewBytesFromMemory(@DecapKey.FInjectionSeed[0], SizeOf(TCnMLKEMBlock)));
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
  A, AT: TCnMLKEMPolyMatrix;
  Y, E1: TCnMLKEMPolyVector;
  E2, MP: TCnMLKEMPolynomial;
  B: TBytes;
  W: TWords;
begin
  // 生成矩阵
  GenerateMatrix(EncapKey.GenerationSeed, A);

  // 转置矩阵
  TransposeMatrix(A, AT);

  // 生成随机向量 Y 与误差向量 E1，注意前者 Y 是 NTT 的，便于运算
  N := 0;
  SampleVector(Seed, FNoise1, N, Y);

  // 后者 E1 非 NTT 的
  SampleVector(Seed, FNoise2, N, E1, False);

  // 生成误差多项式 E2，注意也非 NTT 模式
  SamplePolynomial(Seed, FNoise2, N, E2, False);

  // U = AT * Y + E1，先 NTT 矩阵乘，得到 U 中间结果的 NTT 模式
  MLKEMMatrixVectorMul(UVector, AT, Y);

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
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidMsgLength);

  if (Length(RandHex) > 0) and (Length(RandHex) <> 64) then
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidHexLength);

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
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidMsgLength);

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
      C := NewBytesFromMemory(@De.FInjectionSeed[0], SizeOf(TCnMLKEMSeed));
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
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidMsgLength);

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
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeCipherLengthMismatch,
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
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidPolynomialDegree);

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

procedure MLDSAPolynomialMul(var Res: TCnMLDSAPolynomial; const P1, P2: TCnMLDSAPolynomial;
  IsNTT: Boolean);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    Res[I] := MLDSAModMul(P1[I], P2[I]);
end;

procedure MLDSAVectorToNTT(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  for I := Low(V) to High(V) do
    MLDSAPolynomialToNTT(Res[I], V[I]);
end;

procedure MLDSAVectorToINTT(var Res: TCnMLDSAPolyVector; const V: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  for I := Low(V) to High(V) do
    MLDSAPolynomialToINTT(Res[I], V[I]);
end;

procedure MLDSAVectorAdd(var Res: TCnMLDSAPolyVector; const P1, P2: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  for I := Low(P1) to High(P1) do
    MLDSAPolynomialAdd(Res[I], P1[I], P2[I]);
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
    raise Exception.Create(SCnErrorLatticeInvalidSampleNTT);

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
  C: TBytes;
  J: Integer;
  Z: TBytes;
  Z0, Z1: ShortInt;
begin
  if Length(RandBytes) < CN_MLDSA_KEY_SIZE + 2 then
    raise Exception.Create(SCnErrorLatticeInvalidSampleNTT);

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

// 取 R 的低 D 位放 R0，其余的放 R1
procedure Power2Round(R: Integer; out R0, R1: Integer);
var
  T: Integer;
begin
  T := R mod CN_MLDSA_PRIME;
  if T < 0 then
    T := T + CN_MLDSA_PRIME;

  R0 := T and ((1 shl CN_MLDSA_DROPBIT) - 1);
  R1 := (T - R0) shr CN_MLDSA_DROPBIT;
end;

procedure Power2RoundPolynomial(const R: TCnMLDSAPolynomial; var R0, R1: TCnMLDSAPolynomial);
var
  I: Integer;
begin
  for I := Low(R) to High(R) do
    Power2Round(R[I], R0[I], R1[I]);
end;

procedure Power2RoundVector(const V: TCnMLDSAPolyVector; var V0, V1: TCnMLDSAPolyVector);
var
  I: Integer;
begin
  if Length(V) <= 0 then
    Exit;

  SetLength(V0, Length(V));
  SetLength(V1, Length(V));

  for I := Low(V) to High(V) do
    Power2RoundPolynomial(V[I], V0[I], V1[I]);
end;

// 多项式系数低 D 位打包，注意 256 个数字，无论 D 多少，
// 输出都是 8 的整数因而也就是整数字节
function SimpleBitPackPolynomial(const P: TCnMLDSAPolynomial; D: Integer): TBytes;
var
  I: Integer;
  B: TCnBitBuilder;
begin
  B := TCnBitBuilder.Create;
  try
    for I := Low(P) to High(P) do
      B.AppendDWordRange(P[I], D - 1);
  finally
    B.Free;
  end;
end;

// 多项式向量系数低 D 位打包
function SimpleBitPackVector(const V: TCnMLDSAPolyVector; D: Integer): TBytes;
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

procedure SimpleBitUnpackPolynomial(const Data: TBytes;
  P: TCnMLDSAPolynomial; D: Integer);
var
  B: TCnBitBuilder;
  I: Integer;
begin
  if Length(Data) <> 32 * D then
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeMLDSAPackLengthMismatch,
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

procedure SimpleBitUnpackVector(const Data: TBytes;
  V: TCnMLDSAPolyVector; D: Integer);
var
  I, J, T: Integer;
  B: TCnBitBuilder;
begin
  if Length(Data) <> 32 * D * Length(V) then
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeMLDSAPackLengthMismatch,
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
function BitPackPolynomial(const P: TCnMLDSAPolynomial; A, B: Integer): TBytes;
var
  I, W, L: Integer;
  T: TCnBitBuilder;
begin
  T := TCnBitBuilder.Create;
  try
    for I := Low(P) to High(P) do
    begin
      W := B - P[I];
      L := GetUInt32HighBits(A + B);
      T.AppendDWordRange(W, L);
    end;
    Result := T.ToBytes;
  finally
    T.Free;
  end;
end;

function BitPackVector(const V: TCnMLDSAPolyVector; A, B: Integer): TBytes;
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
        L := GetUInt32HighBits(A + B);
        T.AppendDWordRange(W, L);
      end;
    end;
    Result := T.ToBytes;
  finally
    T.Free;
  end;
end;

procedure BitUnpackPolynomial(const Data: TBytes;
  P: TCnMLDSAPolynomial; A, B: Integer);
var
  T: TCnBitBuilder;
  I, L: Integer;
begin
  I := 32 * (GetUInt32HighBits(A + B) + 1);
  if Length(Data) <> I then
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeMLDSAPackLengthMismatch,
      [I, Length(Data)]);

  L := GetUInt32HighBits(A + B) + 1;
  T := TCnBitBuilder.Create;
  try
    T.SetBytes(Data);
    for I := Low(P) to High(P) do
      P[I] := T.Copy(L * I, L);
  finally
    T.Free;
  end;
end;

procedure BitUnpackVector(const Data: TBytes;
  V: TCnMLDSAPolyVector; A, B: Integer);
var
  I, J, T, L: Integer;
  D: TCnBitBuilder;
begin
  I := 32 * (GetUInt32HighBits(A + B) + 1) * Length(V);
  if Length(Data) <> I then
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeMLDSAPackLengthMismatch,
      [I, Length(Data)]);

  L := GetUInt32HighBits(A + B) + 1;
  D := TCnBitBuilder.Create;
  try
    D.SetBytes(Data);
    T := 0;
    for I := Low(V) to High(V) do
    begin
      for J := Low(V[I]) to High(V[I]) do
      begin
        V[I][J] := D.Copy(T, L);
        Inc(T, L);
      end;
    end;
  finally
    D.Free;
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
      end;
    cmdt65:
      begin
        FMatrixRowCount := 6;
        FMatrixColCount := 5;
        FNoise := 4;
      end;
    cmdt87:
      begin
        FMatrixRowCount := 8;
        FMatrixColCount := 7;
        FNoise := 2;
      end;
  else
    raise ECnLatticeException.Create(SCnErrorLatticeMLDSAInvalidParam);
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
    raise ECnLatticeException.Create(SCnErrorLatticeInvalidHexLength);

  if Length(RandHex) = 0 then
    R := CnRandomBytes(CN_MLKEM_KEY_SIZE)
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
  SetLength(S, Length(PrivateKey.FS1));
  MLDSAVectorToNTT(S, PrivateKey.FS1);

  // 计算 T = A * S1 + S2
  MLDSAMatrixVectorMul(T, Matrix, S);      // 两个 NTT 相乘，得到维度 Row
  MLDSAVectorToINTT(T, T);                 // 结果向量转回非 NTT
  MLDSAVectorAdd(T, T, PrivateKey.FS2);    // 和 S2 相加，两个维度都是 Row

  // 结果 T 向量拆分为 T0 和 T1
  Power2RoundVector(T, PrivateKey.FT0, PublicKey.FT1);

  // 公钥字节数组求杂凑作为私钥 tr
  B := SavePublicKeyToBytes(PublicKey);
  B := MLDSAHFunc(B);
  Move(B[0], PrivateKey.FTrace[0], CN_MLDSA_DIGEST_SIZE);
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
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeMLDSAKeyLengthMismatch,
      [L, Length(SK)]);

  Move(SK[0], PrivateKey.FGenerationSeed[0], SizeOf(TCnMLDSASeed));
  Move(SK[SizeOf(TCnMLDSASeed)], PrivateKey.FKey[0], SizeOf(TCnMLDSASeed));
  Move(SK[2 * SizeOf(TCnMLDSASeed)], PrivateKey.FTrace[0], SizeOf(TCnMLDSAKeyDigest));

  // 复制出来给 S1
  L := 2 * SizeOf(TCnMLDSASeed) + SizeOf(TCnMLDSAKeyDigest);
  B := Copy(SK, L, 32 * FMatrixColCount * (GetNoiseBitLength + 1));
  SetLength(PrivateKey.FS1, FMatrixColCount);
  BitUnpackVector(B, PrivateKey.FS1, FNoise, FNoise);

  // 复制出来给 S2
  Inc(L, 32 * FMatrixColCount * (GetNoiseBitLength + 1));
  B := Copy(SK, L, 32 * FMatrixRowCount * (GetNoiseBitLength + 1));
  SetLength(PrivateKey.FS2, FMatrixRowCount);
  BitUnpackVector(B, PrivateKey.FS2, FNoise, FNoise);

  // 复制出来给 T0
  Inc(L, 32 * FMatrixRowCount * (GetNoiseBitLength + 1));
  B := Copy(SK, L, MaxInt);
  SetLength(PrivateKey.FT0, FMatrixRowCount);
  BitUnpackVector(B, PrivateKey.FT0, CN_MLDSA_DROPVALUE - 1, CN_MLDSA_DROPVALUE);
end;

procedure TCnMLDSA.LoadPublicKeyFromBytes(PublicKey: TCnMLDSAPublicKey;
  const PK: TBytes);
var
  B: TBytes;
begin
  if Length(PK) <> SizeOf(TCnMLDSASeed) + 32 * FMatrixRowCount * CN_MLDSA_PUBKEY_BIT then
    raise ECnLatticeException.CreateFmt(SCnErrorLatticeMLDSAKeyLengthMismatch,
      [SizeOf(TCnMLDSASeed) + 32 * FMatrixRowCount * CN_MLDSA_PUBKEY_BIT, Length(PK)]);

  Move(PK[0], PublicKey.FGenerationSeed[0], SizeOf(TCnMLDSASeed));
  B := Copy(PK, SizeOf(TCnMLDSASeed), MaxInt);
  SetLength(PublicKey.FT1, FMatrixRowCount);
  SimpleBitUnpackVector(B, PublicKey.FT1, CN_MLDSA_PUBKEY_BIT);
end;

function TCnMLDSA.SavePrivateKeyToBytes(PrivateKey: TCnMLDSAPrivateKey): TBytes;
var
  S1, S2, T0: TBytes;
begin
  Result := ConcatBytes(NewBytesFromMemory(@PrivateKey.GenerationSeed[0], SizeOf(TCnMLDSASeed)),
    NewBytesFromMemory(@PrivateKey.Key[0], SizeOf(TCnMLDSASeed)),
    NewBytesFromMemory(@PrivateKey.Trace[0], SizeOf(TCnMLDSAKeyDigest)));

  S1 := BitPackVector(PrivateKey.S1, FNoise, FNoise);
  S2 := BitPackVector(PrivateKey.S2, FNoise, FNoise);
  T0 := BitPackVector(PrivateKey.T0, CN_MLDSA_DROPVALUE - 1, CN_MLDSA_DROPVALUE);
  Result := ConcatBytes(Result, S1, S2, T0);
end;

function TCnMLDSA.SavePublicKeyToBytes(PublicKey: TCnMLDSAPublicKey): TBytes;
begin
  Result := NewBytesFromMemory(@PublicKey.GenerationSeed[0], SizeOf(TCnMLDSASeed));
  Result := ConcatBytes(Result, SimpleBitPackVector(PublicKey.FT1, CN_MLDSA_PUBKEY_BIT));
end;

initialization
  FBigNumberPool := TCnBigNumberPool.Create;
  FInt64PolynomialPool := TCnInt64PolynomialPool.Create;
  FBigNumberVectorPool := TCnBigNumberVectorPool.Create;

  // 初始化 MLKEM 的多项式环里的模多项式
  FMLKEMRing := TCnInt64Polynomial.Create;
  FMLKEMRing.MaxDegree := 256;
  FMLKEMRing[256] := 1;
  FMLKEMRing[0] := 1;

finalization
  FMLKEMRing.Free;
  FBigNumberVectorPool.Free;
  FInt64PolynomialPool.Free;
  FBigNumberPool.Free;

end.
