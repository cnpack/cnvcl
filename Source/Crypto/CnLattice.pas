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
  CnNative, CnVector, CnBigNumber, CnPolynomial, CnRandom, CnBits;

const
  CN_MLKEM_KEY_SIZE    = 32;
  {* MLKEM  的共享密钥及种子等的长度}

  CN_MLKEM_POLY_DEGREE = 256;
  {* MLKEM  的多项式次数}

  CN_MLKEM_PRIME       = 3329;
  {* MLKEM  使用的素数}

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

  TCnMLKEM = class
  private
    FMatrixRank: Integer;
    FNoise1: Integer;
    FNoise2: Integer;
    FRing: TCnInt64Polynomial;
    FCompressDigits: Integer;
  public
    constructor Create(AType: TCnMLKEMType); virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    class function Compress(X: Word; D: Word): Word;
    {* 将一个 X 的系数值压缩到 D 位并返回}
    class function Decompress(X: Word; D: Word): Word;
    {* 将一个压缩后的系数值解压并返回}

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

  TCnMLKEMPrivateKey = class
  {* MLKEM 的私钥，包括秘密多项式向量与噪音多项式向量}
  private
    FSecretVector: TCnInt64PolynomialList;
    FNoiseVector: TCnInt64PolynomialList;
    FPubVector: TCnInt64PolynomialList;
    FSeed: TCnMLKEMSeed;
    FSalt: TCnMLKEMSeed;
    // FMatrix: array of array of TCnInt64Polynomial; // 根据 Seed 生成的矩阵
  public
    constructor Create(AType: TCnMLKEMType); virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    property Seed: TCnMLKEMSeed read FSeed;
    {* 随机种子，用于生成矩阵}
    property Salt: TCnMLKEMSeed read FSalt;
    {* 随机盐}

    property SecretVector: TCnInt64PolynomialList read FSecretVector;
    {* 秘密多项式向量}
    property NoiseVector: TCnInt64PolynomialList read FNoiseVector;
    {* 噪音多项式向量}
    property PubVector: TCnInt64PolynomialList read FPubVector;
    {* 公钥多项式向量}
  end;

  TCnMLKEMPublicKey = class
  {* MLKEM 的公钥，包括可用来生成矩阵的种子，以及公钥多项式向量}
  private
    FSeed: TCnMLKEMSeed;
    FPubVector: TCnInt64PolynomialList;            // 私钥与矩阵计算出的公钥多项式向量
    // FMatrix: array of array of TCnInt64Polynomial; // 根据 Seed 生成的矩阵
  public
    constructor Create(AType: TCnMLKEMType); virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    property Seed: TCnMLKEMSeed read FSeed;
    {* 随机种子，用于生成矩阵}
    property PubVector: TCnInt64PolynomialList read FPubVector;
    {* 公钥多项式向量}
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

function DivMlKemQ(X: Word; B, HQ, BS: Integer; BM: TUInt64): Word;
var
  R: TUInt64;
begin
  R := (TUInt64(X) shl B) + TUInt64(HQ);
  R := UInt64Mul(R, BM);
  R := R shr BS;
  Result := Word(R and ((1 shl B) - 1));
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

{ TCnMLKEMPublicKey }

constructor TCnMLKEMPublicKey.Create(AType: TCnMLKEMType);
begin
  inherited Create;
  FPubVector := TCnInt64PolynomialList.Create;
end;

destructor TCnMLKEMPublicKey.Destroy;
begin
  FPubVector.Free;
  inherited;
end;

{ TCnMLKEMPrivateKey }

constructor TCnMLKEMPrivateKey.Create(AType: TCnMLKEMType);
begin
  FSecretVector := TCnInt64PolynomialList.Create;
  FNoiseVector := TCnInt64PolynomialList.Create;
  FPubVector := TCnInt64PolynomialList.Create;
end;

destructor TCnMLKEMPrivateKey.Destroy;
begin
  FPubVector.Free;
  FNoiseVector.Free;
  FSecretVector.Free;
  inherited;
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
