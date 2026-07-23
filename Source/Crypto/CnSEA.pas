{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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

unit CnSEA;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：Schoof-Elkies-Atkin 算法与模多项式相关计算
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：目前能在一两个小时里算出 secp128r1，五个半小时算出 secp160r1，
*           十六小时算出 secp192r1。
* 开发平台：PWin10 + Delphi 10.3
* 兼容测试：PWin9X/2000/XP/7/10/11 + Delphi/C++Builder 5 ~ 13/FPC
* 本 地 化：该单元中的字符串均符合标准
* 修改记录：2026.07.23 V1.4
*               不断优化，能算出 192 位素数域上的椭圆曲线的阶了
*           2026.07.15 V1.3
*               不断优化，能算出 160 位素数域上的椭圆曲线的阶了
*           2026.07.10 V1.2
*               实现 SEA 第二阶段：Aktin 素数检测与合并计算
*           2026.07.09 V1.1
*               实现 SEA 第一阶段：Elkies 素数检测与迹计算
*           2026.03.24 V1.0
*               创建单元，实现经典模多项式生成
================================================================================
|</PRE>}

{$I CnPack.inc}

interface

uses
  SysUtils, Classes, Contnrs, CnBigNumber, CnPolynomial, CnPrime, CnECC, CnContainers,
  CnNative;

type
  TCnSeaPrimeType = (sptElkies, sptAtkin, sptFailed);
  {* SEA 算法中对素数的分类：
     sptElkies - Elkies 素数，Φ_l(j, Y) 在 F_p 中有根
     sptAtkin  - Atkin 素数，Φ_l(j, Y) 在 F_p 中无根
     sptFailed - 判定失败（输入错误或计算异常）}

  TCnSeaAtkinInfo = class
  {* Atkin 素数信息类，包括可能的 traces t mod L.}
  public
    L: Int64;
    R: Integer;
    PossibleTraces: TCnInt64List;
    SplitType: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

function CnSeaAtkinPossibleTraces(Traces: TCnInt64List;
  L: Integer; A, B, P: TCnBigNumber;
  PhiL: TCnBigNumberBiPolynomial = nil): Boolean;
{* 计算 Atkin 素数 L 的所有可能 t mod L 值集合。}

function CnGenerateClassicalModularPolynomial(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
{* 计算并返回正整数 L 的经典模多项式 Phi_L(X, Y)，L 是素数时初步符合以下网址的结果。
   https://math.mit.edu/~drew/ClassicalModPolys.html
   合数返回 False，表示不支持。

   参数：
     Res: TCnBigNumberBiPolynomial        - 返回计算出的二元大整数多项式
     L: Integer                           - 模多项式序数

   返回值：Boolean                        - 返回计算是否成功
}

function CnGenerateClassicalModularPolynomialModP(Res: TCnInt64BiPolynomial;
  L: Integer; Prime: Int64): Boolean;
{* 在小素数模下计算经典模多项式 Phi_L(X, Y)。
   使用原始 Int64 数组以获得最大速度，Prime 必须小于 2^31。}

function CnGenerateClassicalModularPolynomialCRT(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
{* CRT（中国剩余定理）方法计算经典模多项式 Phi_L(X, Y)。
   使用多个小素数模和 Int64 运算，相比直接 BigNumber 方法能获得约 1000 倍提速。
   当 L >= 11 时，CnGenerateClassicalModularPolynomial 会自动使用此方法。

   参数：
     Res: TCnBigNumberBiPolynomial        - 返回二元多项式
     L: Integer                           - 模多项式序数

   返回值：Boolean                        - 返回计算是否成功
}

procedure SaveModularPolynomialCoefficientsToText(P: TCnBigNumberBiPolynomial; Res: TStrings);
{* 将二元多项式的非零系数以 [X度, Y度] 系数值逐行打印到 Res 字符串列表中，采用 MIT 格式。
   同时验证对称位置 [i,j] 和 [j,i] 的系数是否相等，不相等则抛出异常。
   注意：[i,i] 对角线项无需验证（自身和自己相等）。

   参数：
     P: TCnBigNumberBiPolynomial          - 待打印的二元多项式
     Res: TStrings                        - 输出的目标字符串列表

   返回值：（无）
}

function LoadModularPolynomialCoefficientsFromText(Res: TCnBigNumberBiPolynomial;
  Lines: TStrings): Boolean;
{* 从 MIT 格式的文本字符串中加载二元模多项式系数。
   每行的格式类似于 [XDeg, YDeg] DecimalValue，与 SaveModularPolynomialCoefficientsToText 一致。
   对称的位置 [i,j] 与 [j,i] 均会被加载，返回加载是否成功。

   参数：
     Res: TCnBigNumberBiPolynomial        - 待加载的二元多项式
     Lines: TStrings                      - 待加载的字符串列表

   返回值：Boolean                        - 加载是否成功
}

function CnSeaJInvariant(Res, A, B, P: TCnBigNumber): Boolean;
{* 计算椭圆曲线 E: y^2 = x^3 + Ax + B 在 F_p 上的 j 不变量。
   j = 1728 * 4A^3 / (4A^3 + 27B^2) mod p

   参数：
     Res: TCnBigNumber                    - 返回 j 不变量
     A: TCnBigNumber                      - Weierstrass 方程的 a 参数
     B: TCnBigNumber                      - Weierstrass 方程的 b 参数
     P: TCnBigNumber                      - 有限域素数 p

   返回值：Boolean                        - 返回计算是否成功（判别式为零时返回 False）
}

function CnSeaCheckPrimeType(L: Integer; A, B, P: TCnBigNumber;
  JPrime: TCnBigNumber = nil; PhiL: TCnBigNumberBiPolynomial = nil): TCnSeaPrimeType;
{* 判定素数 L 对曲线 E/F_p 是 Elkies 素数还是 Atkin 素数。
   通过计算 Φ_l(j(E), Y) mod p 并检查其在 F_p 中是否有根来实现。

   参数：
     L: Integer                           - 待判定的素数
     A, B: TCnBigNumber                   - Weierstrass 方程参数
     P: TCnBigNumber                      - 有限域素数 p
     JPrime: TCnBigNumber                 - 若为 Elkies 素数且非 nil，返回一个根 j'

   返回值：TCnSeaPrimeType                - 素数类型
}

function CnSeaElkiesKernelPolynomial(Res: TCnBigNumberPolynomial;
  L: Integer; A, B, P: TCnBigNumber; DPs: TObjectList = nil;
  ScalarLambda: PInteger = nil): Boolean;
{* 对 Elkies 素数 L，计算核多项式 h(x)（次数为 (l-1)/2）。
   通过对 L 阶除法多项式 ψ_l(x) 取平方-free 部分并因式分解来实现。

   参数：
     Res: TCnBigNumberPolynomial          - 返回核多项式 h(x)
     L: Integer                           - Elkies 素数
     A, B: TCnBigNumber                   - Weierstrass 方程参数
     P: TCnBigNumber                      - 有限域素数 p
	 ScalarLambda: PInteger               -

   返回值：Boolean                        - 返回计算是否成功
}

function CnSeaElkiesTrace(Res: TCnBigNumber; L: Integer;
  A, B, P: TCnBigNumber; DPs: TObjectList = nil): Boolean;
{* 对 Elkies 素数 L，利用核多项式计算 Frobenius 迹 t mod l。
   在商环 F_p[x]/(h(x)) 中搜索 Frobenius 的特征值 λ，
   然后 t ≡ λ + p·1/λ (mod p)。

   参数：
     Res: TCnBigNumber                    - 返回 t mod l
     L: Integer                           - Elkies 素数
     A, B: TCnBigNumber                   - Weierstrass 方程参数
     P: TCnBigNumber                      - 有限域素数 p

   返回值：Boolean                        - 返回计算是否成功
}

function CnSeaSafetyBits(PrimeBits: Integer): Integer;
{* 根据 p 的位数返回 SafetyBits（分段策略）。
   <=112 位返回 0（标准 SEA 已足够）；128 位返回 40；192 位返回 72；256 位返回 104。
   用于在素数收集阶段扩展阈值，使 Atkin 过滤足够强以启用 SkipVerify。}

function CnSeaMaxRequiredPrimeL(P: TCnBigNumber): Integer;
{* 快速计算在素域 E/F_p 上运行 SEA 算法所需的最大模多项式次数 L_max。
   累加小素数，直到它们的乘积超过 4sqrt(p)（Hasse 界）。
   所需的素数即为 2 到 L_max 之间的所有素数（若适用，则跳过 L = p）。
   利用这一方法可以提前准备 CnMP_.txt 预计算文件，而无需运行完整的 SEA 算法。

   参数：
     P: TCnBigNumber                      - 有限域素数 p

   返回值：Integer                        - 模多项式所需最高次数，如果返回 0，表示 P 不合法或实在太大了
}

function CnSeaPointCount(Res, A, B, P: TCnBigNumber;
  ModPolys: TObjectList = nil): Boolean;
{* SEA 点不完整计数算法：计算椭圆曲线 E: y^2 = x^3 + Ax + B 在 F_p 上的点数 #E(F_p)。
   对每个小素数 l，优先使用 Elkies 方法计算 t mod l，Atkin 素数回退到 Schoof 方法，
   最后用中国剩余定理合并结果，#E = p + 1 - t。
   ModPolys 应当是预先计算好的模多项式，次数依次为 3, 5, 7, 11……

   参数：
     Res: TCnBigNumber                    - 返回点数 #E(F_p)
     A, B: TCnBigNumber                   - Weierstrass 方程参数
     P: TCnBigNumber                      - 有限域素数 p
     ModPolys: TObjectList                - 预计算的模多项式列表，为 nil 时内部自行计算

   返回值：Boolean                        - 返回计算是否成功
}

implementation

{$IFDEF SEA_TRACE}

uses
  CnDebug;

// 通过 CnDebugger 进行简单跟踪。编译时加 -dSEA_TRACE。
var
  _SeaT0: TDateTime = 0;

function _SeaMs: Int64;
begin
  if _SeaT0 = 0 then _SeaT0 := Now;
  Result := Round((Now - _SeaT0) * 86400000);
end;

procedure _SeaT(const Fmt: string; const Args: array of const);
begin
  CnDebugger.TraceMsg(Format('[%d] ', [_SeaMs]) + Format(Fmt, Args));
end;

{$ENDIF}

const
  CN_SEA_CRT_THRESHOLD = 11;
  {* 模多项式计算时，当 L >= CN_SEA_CRT_THRESHOLD 时自动切换到 CRT 方法。
     CRT 方法使用多个小素数模运算和 Int64 快速运算，比直接 BigNumber 方法快约 10~20 倍。
     L < CN_SEA_CRT_THRESHOLD 时使用 BigNumber 方法（系数较小，直接计算更快且无需 CRT 重建）。}

  CN_SEA_BSGS_THRESHOLD = 100000;
  {* 当 Elkies-Atkin 组合搜索空间 N = floor(2*QMax/M_E) 超过此阈值时，
     从暴力搜索切换到 BSGS（Baby-Step Giant-Step）算法。}

  CN_SEA_MAX_BABY_STEPS = 5000;
  {* CRT baby-step 表的最大条目数。超过则停止添加 Atkin 素数。}

  CN_SEA_MAX_CRT_L_PRODUCT: Int64 = $1000000000000000;  // 2^60
  {* CRT 组合 L 乘积上限，保证 baby-step r 值不溢出 Int64。}

  CN_SEA_MAX_MODPOLY_L = 199;
  {* 最大可用模多项式次数。CnMP_*.txt 预计算文件覆盖到此 L 值。
     超过此值的模多项式需要实时生成，耗时极长，应避免。}

  CN_SEA_PRODUCT_SAFETY_BASE = 112;
  {* 素数乘积阈值的基础安全余量（位数）。
     标准 SEA 在乘积 > 4*sqrt(p) 时停止收集素数，但这仅保证 Elkies+Atkin
     的 L 乘积超过 Hasse 界。由于 Atkin 素数的过滤强度 |S_i|/L_i 通常约 0.5
     （而非 1/L_i），实际假阳性数可能高达 N * 0.5^(numAtkin)，在大素数时
     仍可达数百万，导致 SeaVerifyTrace 验证耗时数天。

     SafetyBits 采用分段策略（CnSeaSafetyBits 函数），对小的 p 不增加额外
     余量（标准 SEA 已足够），对 128 位及以上逐步增加余量以启用 SkipVerify：
       <=112 位: SafetyBits=0  （L≈47, Combine 秒级）
       128 位:   SafetyBits=40 （L≈83, Combine 秒级，Elkies 最大 deg≈3400）
       192 位:   SafetyBits=72 （L≈131, Elkies 最大 deg≈8580）
       256 位:   SafetyBits=104（L≈157, Elkies 最大 deg≈12246）
     每段增加的素数约 4~6 个，其中约一半为 Elkies，使 E_fp_log2 < -2
     从而 SkipVerify=1，Combine 阶段从小时级降为秒级。}

  CN_SEA_ELKIES_BSGS_THRESHOLD = 71;
  {* 素数大于该值时采用 BSGS 查找，否则线性。
     BSGS 算法虽然能将迭代次数从 (L+1)/2 降低到大约 2*sqrt((L+1)/2)，
     但它的每次迭代都有额外的开销（比如需要通过多项式模逆来构建 baby step 表）。
     因此，对于较小的 L（<71），线性搜索反而更快。具体来看：当 L=71 时，
     线性搜索需要 36 次，而 BSGS 需要 12 次；当 L=1009 时，线性搜索需要 505 次，而 BSGS 只需要 46 次。}

function CnSeaSafetyBits(PrimeBits: Integer): Integer;
begin
  { 分段 SafetyBits 策略：
    - <=112 位：SafetyBits=0，标准 SEA 的 L≈47 已使 Combine 足够快
    - 113~128 位：SafetyBits=40，收集到 L≈83，使 SkipVerify=1
    - 129~192 位：SafetyBits=72，收集到 L≈131
    - >=193 位：SafetyBits=104，收集到 L≈157 }
  if PrimeBits <= 112 then
    Result := 0
  else if PrimeBits <= 128 then
    Result := 40
  else if PrimeBits <= 192 then
    Result := 48
  else
    Result := 72;
end;

var
  FSeaBigNumberPool: TCnBigNumberPool = nil;
  FSeaPolynomialPool: TCnBigNumberPolynomialPool = nil;
  FSeaRationalPolynomialPool: TCnBigNumberRationalPolynomialPool = nil;

// 计算因子和函数 sigma_k(n) = sum_{d|n} d^k
procedure CalcSigma(Res: TCnBigNumber; N, K: Integer);
var
  D, I: Integer;
  T, Base: TCnBigNumber;
begin
  Res.SetZero;
  if N <= 0 then Exit;

  T := TCnBigNumber.Create;
  Base := TCnBigNumber.Create;
  try
    for D := 1 to N do
    begin
      if (N mod D) = 0 then
      begin
        Base.SetWord(D);
        BigNumberCopy(T, Base);
        for I := 2 to K do
          BigNumberMul(T, T, Base);
        BigNumberAdd(Res, Res, T);
      end;
    end;
  finally
    Base.Free;
    T.Free;
  end;
end;

// 计算艾森斯坦级数 E4 的 q-展开式，最高到 MaxDegree 次
procedure CalcE4(Res: TCnBigNumberPolynomial; MaxDegree: Integer);
var
  I: Integer;
  Sigma: TCnBigNumber;
  C240: TCnBigNumber;
begin
  Res.MaxDegree := MaxDegree;
  Res[0].SetWord(1);

  Sigma := TCnBigNumber.Create;
  C240 := TCnBigNumber.Create;
  try
    C240.SetWord(240);
    for I := 1 to MaxDegree do
    begin
      CalcSigma(Sigma, I, 3);
      BigNumberMul(Res[I], Sigma, C240);
    end;
    Res.CorrectTop;
  finally
    C240.Free;
    Sigma.Free;
  end;
end;

// 计算艾森斯坦级数 E6 的 q-展开式，最高到 MaxDegree 次
procedure CalcE6(Res: TCnBigNumberPolynomial; MaxDegree: Integer);
var
  I: Integer;
  Sigma: TCnBigNumber;
  C504: TCnBigNumber;
begin
  Res.MaxDegree := MaxDegree;
  Res[0].SetWord(1);

  Sigma := TCnBigNumber.Create;
  C504 := TCnBigNumber.Create;
  try
    C504.SetWord(504);
    for I := 1 to MaxDegree do
    begin
      CalcSigma(Sigma, I, 5);
      BigNumberMul(Res[I], Sigma, C504);
      Res[I].Negate;
    end;
    Res.CorrectTop;
  finally
    C504.Free;
    Sigma.Free;
  end;
end;

// 计算 j-不变量的 q-展开式 J(q) = q * j(q) = 1 + 744q + 196884q^2 + ...
// 最高到 MaxDegree 次。
procedure CalcJ(Res: TCnBigNumberPolynomial; MaxDegree: Integer);
var
  E4, E6, E4_3, E6_2, Delta, DeltaInv: TCnBigNumberPolynomial;
  C1728, E3: TCnBigNumber;
begin
  E4 := TCnBigNumberPolynomial.Create;
  E6 := TCnBigNumberPolynomial.Create;
  E4_3 := TCnBigNumberPolynomial.Create;
  E6_2 := TCnBigNumberPolynomial.Create;
  Delta := TCnBigNumberPolynomial.Create;
  C1728 := TCnBigNumber.Create;
  E3 := TCnBigNumber.Create;
  E3.SetWord(3);
  try
    CalcE4(E4, MaxDegree + 1);
    CalcE6(E6, MaxDegree + 1);

    // E4^3
    BigNumberPolynomialPowerTrunc(E4_3, E4, E3, MaxDegree + 1);

    // E6^2
    BigNumberPolynomialMulTrunc(E6_2, E6, E6, MaxDegree + 1);

    // Delta = (E4^3 - E6^2) / 1728
    BigNumberPolynomialSub(Delta, E4_3, E6_2);

    C1728.SetWord(1728);
    BigNumberPolynomialDivBigNumber(Delta, C1728);

    // J(q) = E4^3 / (Delta / q)
    // Delta / q
    BigNumberPolynomialShiftRight(Delta, 1);

    // 多项式除法（求逆）：计算 (Delta/q)^-1 mod q^(MaxDegree+1)
    DeltaInv := TCnBigNumberPolynomial.Create;
    try
      BigNumberPolynomialInverseTrunc(DeltaInv, Delta, MaxDegree);

      // J(q) = E4^3 * (Delta/q)^-1 mod q^(MaxDegree+1)
      BigNumberPolynomialMulTrunc(Res, E4_3, DeltaInv, MaxDegree);
    finally
      DeltaInv.Free;
    end;
  finally
    E3.Free;
    C1728.Free;
    Delta.Free;
    E6_2.Free;
    E4_3.Free;
    E6.Free;
    E4.Free;
  end;
end;

// ================== CRT Modular Polynomial Computation ==================
// 使用原始 Int64 数组和快速内联模运算的优化实现
// arithmetic.
//
// On 64-bit CPU: primes < 2^30, Int64 multiplication is native (1 instruction).
// On 32-bit CPU: primes < 2^15, Cardinal multiplication is native (1 instruction),
//   avoiding slow software-emulated Int64 multiply/divide.

{$IFDEF CPU64BITS}
// 64 位：素数 < 2^30，A*B < 2^60 可放入 Int64，使用原生乘法
function SeaFastMulMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A * B) mod P;
end;

// 小于 2^31 素数的快速模加
function SeaFastAddMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := A + B;
  if Result >= P then Result := Result - P;
end;

// 小于 2^31 素数的快速模减
function SeaFastSubMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := A - B;
  if Result < 0 then Result := Result + P;
end;

const
  // 小于 2^30 的最大素数，用作素数搜索起点
  SeaCRTPrimeStart: Int64 = 1073741789;
  SeaCRTBitsPerPrime = 30;

{$ELSE}
// 32 位：素数 < 2^15，A*B < 2^30 可放入 Cardinal，使用原生 32 位乘法
// 避免 32 位 CPU 上软件模拟的慢速 Int64 乘除法。
function SeaFastMulMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Cardinal(A) * Cardinal(B)) mod Cardinal(P);
end;

function SeaFastAddMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Int64(Cardinal(A) + Cardinal(B));
  if Result >= P then Result := Result - P;
end;

function SeaFastSubMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Int64(Cardinal(A)) - Int64(Cardinal(B));
  if Result < 0 then Result := Result + P;
end;

const
  // 小于 2^15 的最大素数，用作素数搜索起点
  SeaCRTPrimeStart: Int64 = 32749;
  SeaCRTBitsPerPrime = 15;

{$ENDIF}

// 计算 CRT 重构 Phi_L 所需的位数。
// 理论界限：H(Phi_L) ~ exp(6*L*ln(L))，位数：6*L*log2(L)。
// 经验拟合：实际位数 ≈ 6*L*log2(L) + 17*L（O(L) 项是显著的）。
// 公式 6*L*log2(L) + 20*L + 50 对所有实用 L 提供安全裕度。
//   L=5: 219b (实际157位），L=11：498位（实际421位）,
//   L=23: 1134b (实际1018位），L=29：1475位（实际1338位）,
//   L=53: 2931b (实际~2722位），L=97：5831位
function SeaCalcBitsNeeded(L: Integer): Integer;
var
  Log2L: Double;
begin
  if L <= 1 then
    Result := 100
  else
  begin
    Log2L := Ln(L) / Ln(2.0);
    Result := Trunc(6.0 * L * Log2L) + 20 * L + 50;
  end;
end;

// 使用类似筛法的批量方法计算 sigma_k(N) mod Prime
// 一次性预计算所有 1..MaxN 的 sigma：O(MaxN * log(MaxN))
procedure SeaBatchSigmaModP(var Sigmas: array of Int64; MaxN, K: Integer; Prime: Int64);
var
  D, M: Integer;
  PowD, Acc: Int64;
begin
  for D := 0 to MaxN do
    Sigmas[D] := 0;
  for D := 1 to MaxN do
  begin
    // PowD = D^K mod Prime（模幂运算）
    PowD := Int64NonNegativeMod(D, Prime);
    Acc := 1;
    M := K;
    while M > 0 do
    begin
      if (M and 1) = 1 then
        Acc := SeaFastMulMod(Acc, PowD, Prime);
      M := M shr 1;
      if M > 0 then
        PowD := SeaFastMulMod(PowD, PowD, Prime);
    end;
    // 将 PowD 加到 D 的所有倍数位置
    M := D;
    while M <= MaxN do
    begin
      Sigmas[M] := SeaFastAddMod(Sigmas[M], Acc, Prime);
      Inc(M, D);
    end;
  end;
end;

var
  SeaSigma3Cache: array of Int64;
  SeaSigma5Cache: array of Int64;
  SeaSigma3CachePrime: Int64 = 0;
  SeaSigma5CachePrime: Int64 = 0;
  SeaSigma3CacheMaxN: Integer = 0;
  SeaSigma5CacheMaxN: Integer = 0;

// 在原始数组 J[0..MaxDegree] 中计算 J(q) mod Prime
// J(q) = q * j(q) = 1 + 744q + 196884q^2 + ...
procedure SeaCalcJRaw(var J: array of Int64; MaxDegree: Integer; Prime: Int64);
var
  E4, E6, E4_3, E6_2, Delta, DeltaInv, T2: array of Int64;
  I, J2: Integer;
  Inv1728, Sum: Int64;
begin
  SetLength(E4, MaxDegree + 2);
  SetLength(E6, MaxDegree + 2);
  SetLength(E4_3, MaxDegree + 2);
  SetLength(E6_2, MaxDegree + 2);
  SetLength(Delta, MaxDegree + 2);
  SetLength(DeltaInv, MaxDegree + 2);

  // 批量计算 sigma_3 和 sigma_5
  if (SeaSigma3CachePrime <> Prime) or (SeaSigma3CacheMaxN < MaxDegree + 1) then
  begin
    SetLength(SeaSigma3Cache, MaxDegree + 2);
    SeaBatchSigmaModP(SeaSigma3Cache, MaxDegree + 1, 3, Prime);
    SeaSigma3CachePrime := Prime;
    SeaSigma3CacheMaxN := MaxDegree + 1;
  end;
  if (SeaSigma5CachePrime <> Prime) or (SeaSigma5CacheMaxN < MaxDegree + 1) then
  begin
    SetLength(SeaSigma5Cache, MaxDegree + 2);
    SeaBatchSigmaModP(SeaSigma5Cache, MaxDegree + 1, 5, Prime);
    SeaSigma5CachePrime := Prime;
    SeaSigma5CacheMaxN := MaxDegree + 1;
  end;

  // E4[n] = 240 * sigma_3(n) mod Prime
  E4[0] := 1;
  for I := 1 to MaxDegree + 1 do
    E4[I] := SeaFastMulMod(SeaSigma3Cache[I], 240, Prime);

  // E6[n] = -504 * sigma_5(n) mod Prime
  E6[0] := 1;
  for I := 1 to MaxDegree + 1 do
    E6[I] := SeaFastSubMod(0, SeaFastMulMod(SeaSigma5Cache[I], 504, Prime), Prime);

  // E4^3 截断到 MaxDegree+1（逐项乘法）
  for I := 0 to MaxDegree + 1 do
    E4_3[I] := 0;
  for I := 0 to MaxDegree + 1 do
  begin
    if E4[I] = 0 then Continue;
    for J2 := 0 to MaxDegree + 1 - I do
    begin
      if E4[J2] = 0 then Continue;
      E4_3[I + J2] := SeaFastAddMod(E4_3[I + J2], SeaFastMulMod(E4[I], E4[J2], Prime), Prime);
    end;
  end;
  // 再乘 E4 得到 E4^3
  // 目前 E4_3 = E4^2，需要再乘一次
  // 使用临时变量
  begin
    SetLength(T2, MaxDegree + 2);
    for I := 0 to MaxDegree + 1 do
      T2[I] := 0;
    for I := 0 to MaxDegree + 1 do
    begin
      if E4_3[I] = 0 then Continue;
      for J2 := 0 to MaxDegree + 1 - I do
      begin
        if E4[J2] = 0 then Continue;
        T2[I + J2] := SeaFastAddMod(T2[I + J2], SeaFastMulMod(E4_3[I], E4[J2], Prime), Prime);
      end;
    end;
    for I := 0 to MaxDegree + 1 do
      E4_3[I] := T2[I];
  end;

  // E6^2 截断计算
  for I := 0 to MaxDegree + 1 do
    E6_2[I] := 0;
  for I := 0 to MaxDegree + 1 do
  begin
    if E6[I] = 0 then Continue;
    for J2 := 0 to MaxDegree + 1 - I do
    begin
      if E6[J2] = 0 then Continue;
      E6_2[I + J2] := SeaFastAddMod(E6_2[I + J2], SeaFastMulMod(E6[I], E6[J2], Prime), Prime);
    end;
  end;

  // Delta = (E4^3 - E6^2) * 1728 的逆
  Inv1728 := CnInt64ModularInverse2(Int64NonNegativeMod(1728, Prime), Prime);
  for I := 0 to MaxDegree + 1 do
    Delta[I] := SeaFastMulMod(SeaFastSubMod(E4_3[I], E6_2[I], Prime), Inv1728, Prime);

  // Delta / q = 右移一位
  for I := 0 to MaxDegree do
    Delta[I] := Delta[I + 1];
  Delta[MaxDegree + 1] := 0;

  // 通过牛顿迭代计算 DeltaInv = (Delta/q)^(-1) mod x^(MaxDegree+1)
  DeltaInv[0] := CnInt64ModularInverse2(Delta[0], Prime);
  for I := 1 to MaxDegree do
  begin
    Sum := 0;
    for J2 := 1 to I do
    begin
      if (Delta[J2] = 0) or (DeltaInv[I - J2] = 0) then Continue;
      Sum := SeaFastAddMod(Sum, SeaFastMulMod(Delta[J2], DeltaInv[I - J2], Prime), Prime);
    end;
    DeltaInv[I] := SeaFastMulMod(SeaFastSubMod(0, Sum, Prime), DeltaInv[0], Prime);
  end;

  // J(q) = E4^3 * DeltaInv 截断到 MaxDegree
  for I := 0 to MaxDegree do
    J[I] := 0;
  for I := 0 to MaxDegree do
  begin
    if E4_3[I] = 0 then Continue;
    for J2 := 0 to MaxDegree - I do
    begin
      if DeltaInv[J2] = 0 then Continue;
      J[I + J2] := SeaFastAddMod(J[I + J2], SeaFastMulMod(E4_3[I], DeltaInv[J2], Prime), Prime);
    end;
  end;
end;

// 使用原始 Int64 数组计算经典模多项式 Phi_L mod Prime
// 所有多项式运算使用普通数组以获得最大速度.
function CnGenerateClassicalModularPolynomialModP(Res: TCnInt64BiPolynomial;
  L: Integer; Prime: Int64): Boolean;
var
  N, I, K, D, M, U, V, MaxY, J2: Integer;
  J_q: array of Int64;
  H: array of array of Int64;  // H[k][i] = J(q)^k 中 q^i 的系数
  PmArr: array of Int64;
  Pm_Poly: array of array of Int64;
  Sm_Poly: array of array of Int64;  // Sm_Poly[k][v]
  SmDeg: array of Integer;
  PmDeg: array of Integer;
  T64, Sum: Int64;
  HTemp: array of Int64;
  PrimeL: Int64;
begin
  Result := False;
  if Res = nil then Exit;
  if L < 1 then Exit;
  if (L > 1) and not CnUInt32IsPrime(L) then Exit;

  if L = 1 then
  begin
    Res.SetZero;
    Res.SafeValue[1, 0] := 1;
    Res.SafeValue[0, 1] := Int64NonNegativeMod(Prime - 1, Prime);
    Res.CorrectTop;
    Result := True;
    Exit;
  end;

  N := L * (L + 1);
  PrimeL := Int64NonNegativeMod(L, Prime);

  // 计算 J(q) mod Prime
  SetLength(J_q, N + 1);
  SeaCalcJRaw(J_q, N, Prime);

  // 使用截断乘法计算 H[k] = J(q)^k，其中 k = 0..N
  // H[k][i] 中 i = 0..N
  SetLength(H, N + 1);
  for K := 0 to N do
  begin
    SetLength(H[K], N + 1);
    for I := 0 to N do
      H[K][I] := 0;
  end;

  // H[0] = 1
  H[0][0] := 1;

  // H[k] = H[k-1] * J(q) 截断到度数 N
  SetLength(HTemp, N + 1);
  for K := 1 to N do
  begin
    // HTemp = H[K-1] * J_q mod x^(N+1)
    for I := 0 to N do
      HTemp[I] := 0;
    for I := 0 to N do
    begin
      if H[K - 1][I] = 0 then Continue;
      J2 := N - I;
      for D := 0 to J2 do
      begin
        if J_q[D] = 0 then Continue;
        HTemp[I + D] := SeaFastAddMod(HTemp[I + D], SeaFastMulMod(H[K - 1][I], J_q[D], Prime), Prime);
      end;
    end;
    // 复制到 H[K]
    for I := 0 to N do
      H[K][I] := HTemp[I];
  end;

  // 计算 Pm_Poly[M]，其中 M = 1..L+1
  SetLength(PmDeg, L + 2);
  SetLength(SmDeg, L + 2);
  SetLength(Sm_Poly, L + 2);
  SetLength(PmArr, 0); // 将根据 M 调整大小

  // 首先计算所有 Pm_Poly 并存储为 H 类数组的行
  // Pm_Poly[M] 度数为 M*L
  // 将 Pm_Poly 临时存储在 Sm_Poly 中... 不，使用单独数组
  // 实际上，在 Sm_Poly 计算过程中即时计算 Pm_Poly[M]
  // 以节省内存。但 Sm_Poly 递推需要 Pm_Poly[1..K]，所以需要
  // 全部存储。

  SetLength(Pm_Poly, L + 2);

  for M := 1 to L + 1 do
  begin
    SetLength(PmArr, M * L + 1);
    for I := 0 to M * L do
      PmArr[I] := 0;

    for I := -M * L to 0 do
    begin
      Sum := 0;
      if (I mod L = 0) and (I div L >= -M) then
        Sum := SeaFastAddMod(Sum, H[M][(I div L) + M], Prime);
      if (I * L >= -M) then
      begin
        T64 := SeaFastMulMod(H[M][I * L + M], PrimeL, Prime);
        Sum := SeaFastAddMod(Sum, T64, Prime);
      end;
      PmArr[I + M * L] := Sum;
    end;

    SetLength(Pm_Poly[M], M * L + 1);
    PmDeg[M] := M * L;

    for D := M * L downto 0 do
    begin
      T64 := PmArr[-D + M * L];
      Pm_Poly[M][D] := T64;

      if T64 <> 0 then
      begin
        for I := -D to 0 do
        begin
          if H[D][I + D] <> 0 then
          begin
            Sum := SeaFastMulMod(T64, H[D][I + D], Prime);
            PmArr[I + M * L] := SeaFastSubMod(PmArr[I + M * L], Sum, Prime);
          end;
        end;
      end;
    end;
  end;

  // 使用牛顿递推计算 Sm_Poly
  // Sm_Poly[K] = (1/K) * Σ_{I=1}^{K} (-1)^(I-1) * Sm_Poly[K-I] * Pm_Poly[I]
  SetLength(Sm_Poly, L + 2);
  SmDeg[0] := 0;
  SetLength(Sm_Poly[0], 1);
  Sm_Poly[0][0] := 1;

  for K := 1 to L + 1 do
  begin
    // 确定 Sm_Poly[K] 的最大度数
    SmDeg[K] := 0;
    for I := 1 to K do
    begin
      if SmDeg[K - I] + PmDeg[I] > SmDeg[K] then
        SmDeg[K] := SmDeg[K - I] + PmDeg[I];
    end;
    SetLength(Sm_Poly[K], SmDeg[K] + 1);
    for I := 0 to SmDeg[K] do
      Sm_Poly[K][I] := 0;

    for I := 1 to K do
    begin
      // Sm_Poly[K] += (-1)^(I-1) * Sm_Poly[K-I] * Pm_Poly[I]
      for D := 0 to SmDeg[K - I] do
      begin
        if Sm_Poly[K - I][D] = 0 then Continue;
        for J2 := 0 to PmDeg[I] do
        begin
          if Pm_Poly[I][J2] = 0 then Continue;
          T64 := SeaFastMulMod(Sm_Poly[K - I][D], Pm_Poly[I][J2], Prime);
          if (I - 1) mod 2 = 1 then
            Sm_Poly[K][D + J2] := SeaFastSubMod(Sm_Poly[K][D + J2], T64, Prime)
          else
            Sm_Poly[K][D + J2] := SeaFastAddMod(Sm_Poly[K][D + J2], T64, Prime);
        end;
      end;
    end;

    // 除以 K：乘 K^(-1) mod Prime
    T64 := CnInt64ModularInverse2(Int64NonNegativeMod(K, Prime), Prime);
    for I := 0 to SmDeg[K] do
      Sm_Poly[K][I] := SeaFastMulMod(Sm_Poly[K][I], T64, Prime);
  end;

  // 将结果装配到 TCnInt64BiPolynomial 中
  Res.SetZero;
  MaxY := 0;
  for K := 0 to L + 1 do
    if SmDeg[K] > MaxY then MaxY := SmDeg[K];
  Res.MaxXDegree := L + 1;
  Res.MaxYDegree := MaxY;

  for K := 0 to L + 1 do
  begin
    U := L + 1 - K;
    for V := 0 to SmDeg[K] do
    begin
      if Sm_Poly[K][V] <> 0 then
      begin
        if K mod 2 = 1 then
          Res.SafeValue[U, V] := SeaFastSubMod(0, Sm_Poly[K][V], Prime)
        else
          Res.SafeValue[U, V] := Sm_Poly[K][V];
      end;
    end;
  end;
  Res.CorrectTop;

  Result := True;
end;

// 找到小于给定数的下一个素数（用于 CRT 素数选取）
function SeaFindNextPrimeBelow(N: Int64): Int64;
var
  Candidate: Int64;
begin
  Result := 0;
  if N < 3 then Exit;
  Candidate := N;
  if Candidate mod 2 = 0 then Candidate := Candidate - 1;
  while Candidate >= 3 do
  begin
    if CnUInt32IsPrime(Cardinal(Candidate)) then
    begin
      Result := Candidate;
      Exit;
    end;
    Candidate := Candidate - 2;
  end;
end;

// 使用 CRT 方法计算经典模多项式
function CnGenerateClassicalModularPolynomialCRT(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
var
  // 小 L：委托给直接方法
  I, K, MaxX, MaxY: Integer;
  Prime: Int64;
  BitsNeeded, BitsCovered: Integer;
  Candidate: Int64;
  Primes: TCnInt64List;
  ModResults: TObjectList;  // 管理 TCnInt64BiPolynomial 对象
  ModRes: TCnInt64BiPolynomial;
  // 每个系数的 CRT 状态
  V, M, Tmp, Tmp2: TCnBigNumber;
  VModP, Diff, InvM, T, T1: Int64;
  U, VIdx: Integer;
begin
  Result := False;
  if (Res = nil) or (L < 1) then Exit;

  // 对于小 L，使用直接 BigNumber 方法
  if L < CN_SEA_CRT_THRESHOLD then
  begin
    Result := CnGenerateClassicalModularPolynomial(Res, L);
    Exit;
  end;

  if (L > 1) and not CnUInt32IsPrime(L) then Exit;

  // L=1 特殊处理
  if L = 1 then
  begin
    Result := CnGenerateClassicalModularPolynomial(Res, L);
    Exit;
  end;

  Primes := TCnInt64List.Create;
  ModResults := TObjectList.Create(True);
  V := TCnBigNumber.Create;
  M := TCnBigNumber.Create;
  Tmp := TCnBigNumber.Create;
  Tmp2 := TCnBigNumber.Create;
  try
    // 高度上界：理论公式 6*L*log2(L) + 20*L + 50
    // 详见 SeaCalcBitsNeeded 的推导与验证数据。
    BitsNeeded := SeaCalcBitsNeeded(L);
    Candidate := SeaCRTPrimeStart;
    BitsCovered := 0;

    while BitsCovered < BitsNeeded do
    begin
      Prime := SeaFindNextPrimeBelow(Candidate);
      if Prime = 0 then
        raise Exception.Create('Cannot find enough primes for CRT');

      // 跳过整除 1728 (= 2^6 * 3^3) 或 <= L+1 的素数
      if (Prime <= L + 1) or (Prime <= 1728) then
      begin
        Candidate := Prime - 2;
        Continue;
      end;

      Primes.Add(Prime);
      BitsCovered := BitsCovered + SeaCRTBitsPerPrime;
      Candidate := Prime - 2;

      if Primes.Count > 500 then
        raise Exception.Create('Too many primes needed, L may be too large');
    end;

    // 对每个素数 p_i 计算 Phi_L mod p_i
    for I := 0 to Primes.Count - 1 do
    begin
      ModRes := TCnInt64BiPolynomial.Create;
      if not CnGenerateClassicalModularPolynomialModP(ModRes, L, Primes[I]) then
      begin
        ModRes.Free;
        Exit;
      end;
      ModResults.Add(ModRes);
    end;

    // 确定结果的维度
    MaxX := L + 1;
    MaxY := 0;
    for I := 0 to ModResults.Count - 1 do
    begin
      ModRes := TCnInt64BiPolynomial(ModResults[I]);
      if ModRes.MaxYDegree > MaxY then
        MaxY := ModRes.MaxYDegree;
    end;

    // 初始化结果
    Res.SetZero;
    Res.MaxXDegree := MaxX;
    Res.MaxYDegree := MaxY;

    // 对每个系数 (U, V) 进行 CRT 重构
    for U := 0 to MaxX do
    begin
      for VIdx := 0 to MaxY do
      begin
        // 增量 CRT：
        // V = v_0 mod p_0，M = p_0
        // 对每个后续素数 p_k：
        //   t = (v_k - V mod p_k) * M^(-1) mod p_k
        //   V = V + M * t
        //   M = M * p_k

        V.SetZero;
        M.SetWord(Cardinal(Primes[0]));

        // 取 v_0 = ModResults[0].SafeValue[U, VIdx]
        V.SetWord(Cardinal(TCnInt64BiPolynomial(ModResults[0]).SafeValue[U, VIdx]));

        for K := 1 to Primes.Count - 1 do
        begin
          Prime := Primes[K];
          ModRes := TCnInt64BiPolynomial(ModResults[K]);

          // v_k = ModRes.SafeValue[U, VIdx]
          // VModP = V mod Prime
          VModP := BigNumberModWord(V, TCnBigNumberElement(Prime));

          // Diff = (v_k - VModP) mod Prime
          Diff := Int64NonNegativeMod(ModRes.SafeValue[U, VIdx] - VModP, Prime);

          if Diff = 0 then
          begin
            // V is already correct mod Prime, just extend M
            BigNumberMulWord(M, TCnBigNumberElement(Prime));
            Continue;
          end;

          // InvM = M^(-1) mod Prime
          // M mod Prime
          T1 := BigNumberModWord(M, TCnBigNumberElement(Prime));
          InvM := CnInt64ModularInverse2(T1, Prime);

          // T = Diff * InvM mod Prime
          T := Int64NonNegativeMulMod(Diff, InvM, Prime);

          // V = V + M * T
          BigNumberSetInt64(Tmp, T);
          BigNumberMul(Tmp2, M, Tmp);
          BigNumberAdd(V, V, Tmp2);

          // M = M * Prime
          BigNumberMulWord(M, TCnBigNumberElement(Prime));
        end;

        // Handle sign: if V > M/2, V = V - M
        BigNumberShiftRightOne(Tmp, M);  // Tmp = M / 2
        if BigNumberCompare(V, Tmp) > 0 then
          BigNumberSub(V, V, M);

        // 保存结果
        if not V.IsZero then
          BigNumberCopy(Res.SafeValue[U, VIdx], V);
      end;
    end;

    Res.CorrectTop;
    Result := True;
  finally
    Tmp2.Free;
    Tmp.Free;
    M.Free;
    V.Free;
    ModResults.Free;
    Primes.Free;
  end;
end;

function CnGenerateClassicalModularPolynomial(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
var
  N, I, K, D, M, U, V, MaxY: Integer;
  J_q, PolyT: TCnBigNumberPolynomial;
  H: array of TCnBigNumberPolynomial;
  Pm_Poly, Sm_Poly: array of TCnBigNumberPolynomial;
  PmArr: array of TCnBigNumber;
  T, Sum, Coeff: TCnBigNumber;
begin
  // 对于 L >= CN_SEA_CRT_THRESHOLD，使用 CRT 方法获得约 1000 倍提速
  if L >= CN_SEA_CRT_THRESHOLD then
  begin
    Result := CnGenerateClassicalModularPolynomialCRT(Res, L);
    Exit;
  end;

  Result := False;
  if Res = nil then Exit;
  if L < 1 then Exit;

  // SEA 算法只需素数 L 的模多项式，合数 L 的模多项式计算方法不同
  // （需考虑中间等环，多项式次数为 psi(N) = N*prod(1+1/p) 而非 L+1）
  // 此处仅支持 L=1 和素数 L，避免合数输入产生静默错误
  if (L > 1) and not CnUInt32IsPrime(L) then Exit;

  // L=1: Phi_1(X, Y) = X - Y
  if L = 1 then
  begin
    Res.SetZero;
    Res.SafeValue[1, 0].SetOne;
    Res.SafeValue[0, 1].SetWord(1);
    Res.SafeValue[0, 1].Negate;
    Res.CorrectTop;
    Result := True;
    Exit;
  end;

  N := L * (L + 1);

  J_q := TCnBigNumberPolynomial.Create;
  T := TCnBigNumber.Create;
  Sum := TCnBigNumber.Create;
  Coeff := TCnBigNumber.Create;
  PolyT := TCnBigNumberPolynomial.Create;
  try
    CalcJ(J_q, N);

    SetLength(H, N + 1);
    for K := 0 to N do
    begin
      H[K] := TCnBigNumberPolynomial.Create;
      H[K].SetZero;
      H[K].MaxDegree := N;
    end;

    // H[0](q) = 1
    H[0][0].SetWord(1);

    // H[k](q) = H[k-1](q) * j(q), truncated to degree N
    // H[k][j] = coeff of q^(j-k), J_q[j] = coeff of q^(j-1)
    // The polynomial product H[k-1] * J_q directly gives H[k][j] = product[j]
    // (shifts cancel: q^(j-(k-1)) * q^(n-1) = q^(j+n-k), so index j+n=k+i => H[k][i+k])
    // 使用 BigNumberPolynomialMulTrunc 代替手动三层循环以提高效率
    // (zero-skipping, pooled objects, better memory access)
    for K := 1 to N do
      BigNumberPolynomialMulTrunc(H[K], H[K - 1], J_q, N);

    SetLength(Pm_Poly, L + 2);
    for M := 1 to L + 1 do
    begin
      SetLength(PmArr, M * L + 1);
      for I := -M * L to 0 do
      begin
        Sum.SetZero;
        if (I mod L = 0) and (I div L >= -M) then
        begin
          BigNumberAdd(Sum, Sum, H[M][(I div L) + M]);
        end;
        if (I * L >= -M) then
        begin
          BigNumberCopy(T, H[M][I * L + M]);
          BigNumberMulWord(T, L);
          BigNumberAdd(Sum, Sum, T);
        end;
        PmArr[I + M * L] := TCnBigNumber.Create;
        BigNumberCopy(PmArr[I + M * L], Sum);
      end;

      Pm_Poly[M] := TCnBigNumberPolynomial.Create;
      Pm_Poly[M].MaxDegree := M * L;
      for D := M * L downto 0 do
      begin
        BigNumberCopy(Coeff, PmArr[-D + M * L]);
        BigNumberCopy(Pm_Poly[M][D], Coeff);

        for I := -D to 0 do
        begin
          BigNumberMul(T, Coeff, H[D][I + D]);
          BigNumberSub(PmArr[I + M * L], PmArr[I + M * L], T);
        end;
      end;

      for I := 0 to M * L do
        PmArr[I].Free;
    end;

    SetLength(Sm_Poly, L + 2);
    Sm_Poly[0] := TCnBigNumberPolynomial.Create;
    Sm_Poly[0].MaxDegree := 0;
    Sm_Poly[0][0].SetWord(1);

    for K := 1 to L + 1 do
    begin
      Sm_Poly[K] := TCnBigNumberPolynomial.Create;
      Sm_Poly[K].SetZero;

      for I := 1 to K do
      begin
        BigNumberPolynomialMul(PolyT, Sm_Poly[K - I], Pm_Poly[I]);
        if (I - 1) mod 2 = 1 then
          BigNumberPolynomialSub(Sm_Poly[K], Sm_Poly[K], PolyT)
        else
          BigNumberPolynomialAdd(Sm_Poly[K], Sm_Poly[K], PolyT);
      end;

      T.SetWord(K);
      BigNumberPolynomialDivBigNumber(Sm_Poly[K], T);
    end;

    Res.SetZero;
    MaxY := 0;
    for K := 0 to L + 1 do
      if Sm_Poly[K].MaxDegree > MaxY then MaxY := Sm_Poly[K].MaxDegree;
    Res.MaxXDegree := L + 1;
    Res.MaxYDegree := MaxY;

    for K := 0 to L + 1 do
    begin
      U := L + 1 - K;
      for V := 0 to Sm_Poly[K].MaxDegree do
      begin
        if not Sm_Poly[K][V].IsZero then
        begin
          BigNumberCopy(Res.SafeValue[U, V], Sm_Poly[K][V]);
          if K mod 2 = 1 then
            Res.SafeValue[U, V].Negate;
        end;
      end;
    end;
    Res.CorrectTop;
  finally
    if Length(Sm_Poly) > 0 then
      for K := 0 to L + 1 do
        if Sm_Poly[K] <> nil then Sm_Poly[K].Free;
    if Length(Pm_Poly) > 0 then
      for K := 1 to L + 1 do
        if Pm_Poly[K] <> nil then Pm_Poly[K].Free;
    if Length(H) > 0 then
      for K := 0 to N do
        if H[K] <> nil then H[K].Free;

    PolyT.Free;
    Coeff.Free;
    Sum.Free;
    T.Free;
    J_q.Free;
  end;

  Result := True;
end;

procedure SaveModularPolynomialCoefficientsToText(P: TCnBigNumberBiPolynomial; Res: TStrings);
var
  I, J: Integer;
  YList: TCnSparseBigNumberList;
  CoeffIJ, CoeffJI: TCnBigNumber;
begin
  if (P = nil) or (Res = nil) then
    Exit;

  // 第一遍：输出所有非零系数
  for I := 0 to P.MaxXDegree do
  begin
    YList := P.YFactorsList[I];
    for J := 0 to YList.Count - 1 do
    begin
      if I >= YList[J].Exponent then
        Res.Add(Format('[%d,%d] %s', [I, YList[J].Exponent, YList[J].Value.ToDec]));
    end;
  end;

  // 第二遍：验证对称位置 [i,j] 与 [j,i] 的系数是否相等
  for I := 0 to P.MaxXDegree do
  begin
    YList := P.YFactorsList[I];
    for J := 0 to YList.Count - 1 do
    begin
      // 跳过对角线 [i,i]，只检查 i <> j
      if YList[J].Exponent = I then Continue;

      // 用 ReadonlyValue 安全读取 [j,i]
      CoeffIJ := YList[J].Value;
      CoeffJI := P.ReadonlyValue[YList[J].Exponent, I];

      if BigNumberCompare(CoeffIJ, CoeffJI) <> 0 then
        raise Exception.CreateFmt(
          'PrintModularPolynomialCoefficients: NOT Equal! [%d,%d]=%s vs [%d,%d]=%s',
          [I, YList[J].Exponent, CoeffIJ.ToDec,
           YList[J].Exponent, I, CoeffJI.ToDec]);
    end;
  end;
end;

function LoadModularPolynomialCoefficientsFromText(Res: TCnBigNumberBiPolynomial;
  Lines: TStrings): Boolean;
var
  I, M, N, SpPos, Comma: Integer;
  Line, Key, ValStr: string;
  Coeff: TCnBigNumber;
begin
  Result := False;
  if (Res = nil) or (Lines = nil) then Exit;
  Res.SetZero;

  Coeff := TCnBigNumber.Create;
  try
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      if Line = '' then Continue;

      // Parse "[m,n] value"
      SpPos := Pos('] ', Line);
      if SpPos = 0 then Continue;
      Key := Copy(Line, 1, SpPos);  // [m,n]
      ValStr := Trim(Copy(Line, SpPos + 1, MaxInt));
      if ValStr = '' then Continue;

      // 从 [m,n] 提取 m 和 n
      Key := StringReplace(Key, '[', '', [rfReplaceAll]);
      Key := StringReplace(Key, ']', '', [rfReplaceAll]);
      Key := StringReplace(Key, ' ', '', [rfReplaceAll]);
      Comma := Pos(',', Key);
      if Comma = 0 then Continue;
      M := StrToInt(Copy(Key, 1, Comma - 1));
      N := StrToInt(Copy(Key, Comma + 1, MaxInt));

      Coeff.SetDec(ValStr);
      // Set [M, N] (SafeValue auto-expands dimensions)
      BigNumberCopy(Res.SafeValue[M, N], Coeff);
      // 同时填充对称位置 [N, M]，对角线除外
      if M <> N then
        BigNumberCopy(Res.SafeValue[N, M], Coeff);
      Result := True;
    end;
  finally
    Coeff.Free;
  end;
end;

// ================ SEA 第一阶段实现 ================

function CnSeaJInvariant(Res, A, B, P: TCnBigNumber): Boolean;
var
  T1, T2, Num, Den: TCnBigNumber;
begin
  // j = 1728 * 4A^3 / (4A^3 + 27B^2) mod p
  Result := False;
  if (Res = nil) or (A = nil) or (B = nil) or (P = nil) then Exit;
  if P.IsZero or P.IsNegative then Exit;

  T1 := FSeaBigNumberPool.Obtain;
  T2 := FSeaBigNumberPool.Obtain;
  Num := FSeaBigNumberPool.Obtain;
  Den := FSeaBigNumberPool.Obtain;
  try
    // Num = 4 * A^3 mod p
    BigNumberMul(T1, A, A);
    BigNumberMod(T1, T1, P);
    BigNumberMul(T1, T1, A);
    BigNumberMod(T1, T1, P);
    BigNumberCopy(Num, T1);
    BigNumberMulWord(Num, 4);
    BigNumberMod(Num, Num, P);

    // T2 = 27 * B^2 mod p
    BigNumberMul(T1, B, B);
    BigNumberMod(T1, T1, P);
    BigNumberCopy(T2, T1);
    BigNumberMulWord(T2, 27);
    BigNumberMod(T2, T2, P);

    // Den = 4A^3 + 27B^2 mod p （即判别式相关量）
    BigNumberAdd(Den, Num, T2);
    BigNumberMod(Den, Den, P);

    if Den.IsZero then Exit; // 奇异曲线，无 j 不变量

    // j = 1728 * Num / Den mod p
    BigNumberMulWord(Num, 1728);
    BigNumberMod(Num, Num, P);
    BigNumberModularInverse(T1, Den, P);
    BigNumberMul(Res, Num, T1);
    BigNumberMod(Res, Res, P);

    Result := True;
  finally
    FSeaBigNumberPool.Recycle(Den);
    FSeaBigNumberPool.Recycle(Num);
    FSeaBigNumberPool.Recycle(T2);
    FSeaBigNumberPool.Recycle(T1);
  end;
end;

function CnSeaCheckPrimeType(L: Integer; A, B, P: TCnBigNumber;
  JPrime: TCnBigNumber; PhiL: TCnBigNumberBiPolynomial): TCnSeaPrimeType;
var
  J: TCnBigNumber;
  OwnPhiL: Boolean;
  FY: TCnBigNumberPolynomial;
  Roots: TCnBigNumberList;
  G, XPowP, XPmX, YPoly: TCnBigNumberPolynomial;
  Factors: TCnBigNumberPolynomialList;
  Idx: Integer;
begin
  Result := sptFailed;
  if (L < 2) or ((L > 2) and not CnUInt32IsPrime(L)) then Exit;
  if (A = nil) or (B = nil) or (P = nil) then Exit;

  J := FSeaBigNumberPool.Obtain;
  OwnPhiL := (PhiL = nil);
  if OwnPhiL then
    PhiL := TCnBigNumberBiPolynomial.Create;
  FY := FSeaPolynomialPool.Obtain;
  Roots := TCnBigNumberList.Create;
  try
    // 计算 j 不变量
    if not CnSeaJInvariant(J, A, B, P) then Exit;

    // 生成模多项式 Phi_L(X, Y)
    if OwnPhiL then
    begin
      if not CnGenerateClassicalModularPolynomial(PhiL, L) then
	    Exit;
    end;

    // 在 X = j 处求值，得到关于 Y 的一元多项式 f(Y) = Phi_L(j, Y) mod p
    if not BigNumberBiPolynomialGaloisEvaluateByX(FY, PhiL, J, P) then Exit;

    // 常数多项式无法有根
    if FY.MaxDegree <= 0 then Exit;

    // 搜索 f(Y) 在 F_p 中的线性因子（根）
    BigNumberPolynomialGaloisFindLinearFactors(Roots, FY, P);

    if Roots.Count > 0 then
    begin
      Result := sptElkies;
      if JPrime <> nil then
        BigNumberCopy(JPrime, Roots[0]);
    end
    else
    begin
      // For large p, use GCD(g(Y), Y^p - Y) to detect linear factors
      G := FSeaPolynomialPool.Obtain;
      XPowP := FSeaPolynomialPool.Obtain;
      XPmX := FSeaPolynomialPool.Obtain;
      YPoly := FSeaPolynomialPool.Obtain;
      try
        YPoly.SetCoefficients([0, 1]);
        BigNumberPolynomialGaloisPower(XPowP, YPoly, P, P, FY);
        BigNumberPolynomialGaloisSub(XPmX, XPowP, YPoly, P);
        BigNumberPolynomialGaloisGreatestCommonDivisor(G, XPmX, FY, P);

        if G.MaxDegree > 0 then
        begin
          Result := sptElkies;
          if JPrime <> nil then
          begin
            Factors := TCnBigNumberPolynomialList.Create;
            try
              if BigNumberPolynomialGaloisFactorCantorZassenhaus(Factors, G, P) then
              begin
                for Idx := 0 to Factors.Count - 1 do
                begin
                  if TCnBigNumberPolynomial(Factors[Idx]).MaxDegree = 1 then
                  begin
                    BigNumberCopy(JPrime, TCnBigNumberPolynomial(Factors[Idx])[0]);
                    BigNumberNonNegativeMod(JPrime, JPrime, P);
                    Break;
                  end;
                end;
              end;
            finally
              Factors.Free;
            end;
          end;
        end
        else
          Result := sptAtkin;
      finally
        FSeaPolynomialPool.Recycle(YPoly);
        FSeaPolynomialPool.Recycle(XPmX);
        FSeaPolynomialPool.Recycle(XPowP);
        FSeaPolynomialPool.Recycle(G);
      end;
    end;
  finally
    Roots.Free;
    FSeaPolynomialPool.Recycle(FY);
    if OwnPhiL then
	  PhiL.Free;
    FSeaBigNumberPool.Recycle(J);
  end;
end;

function CnSeaElkiesKernelPolynomial(Res: TCnBigNumberPolynomial;
  L: Integer; A, B, P: TCnBigNumber; DPs: TObjectList;
  ScalarLambda: PInteger): Boolean;
var
  OwnDPs: Boolean;
  PsiL, Y2, XPowP, XPmX: TCnBigNumberPolynomial;
  T1, T2, G, H: TCnBigNumberPolynomial;
  Lambda, TargetDeg: Integer;
  Found: Boolean;
  CzFactors: TCnBigNumberPolynomialList;
  CzI: Integer;
  CzOk: Boolean;
  ScalarLam: Integer;
  BQ: TCnBigNumber;
begin
  Result := False;
  if (Res = nil) or (L < 3) then Exit;
  if (A = nil) or (B = nil) or (P = nil) then Exit;

  OwnDPs := False;
  Y2 := nil;
  XPowP := nil;
  XPmX := nil;
  T1 := nil;
  T2 := nil;
  G := nil;
  H := nil;

  try
    // 生成 0 到 L+1 阶的除法多项式
    if DPs = nil then
    begin
      DPs := TObjectList.Create(True);
      CnGenerateGaloisDivisionPolynomials(A, B, P, L + 1, DPs);
      OwnDPs := True;
    end;
    // 取 Psi_L(x) 作为模多项式，由 DPs 拥有
    PsiL := TCnBigNumberPolynomial(DPs[L]);

    // 设置 Y2 = x^3 + Ax + B
    Y2 := FSeaPolynomialPool.Obtain;
    Y2.SetCoefficients([B, A, 0, 1]);

    // 计算 x^p mod Psi_L（大 Psi_L 时使用 Barrett 约简）
    XPowP := FSeaPolynomialPool.Obtain;
    XPowP.SetCoefficients([0, 1]); // x
    {$IFDEF SEA_TRACE} _SeaT('[ElkKern] L=%d x^p mod Psi start deg=%d', [L, PsiL.MaxDegree]); {$ENDIF}
    BigNumberPolynomialGaloisPowerBarrett(XPowP, XPowP, P, P, PsiL);
    {$IFDEF SEA_TRACE} _SeaT('[ElkKern] L=%d x^p mod Psi done', [L]); {$ENDIF}

    // XPmX = x^p - x mod Psi_L
    XPmX := FSeaPolynomialPool.Obtain;
    T1 := FSeaPolynomialPool.Obtain;
    T1.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisSub(XPmX, XPowP, T1, P, PsiL);

    T2 := FSeaPolynomialPool.Obtain;
    G := FSeaPolynomialPool.Obtain;
    H := FSeaPolynomialPool.Obtain;

    TargetDeg := (L - 1) div 2;
    Found := False;
    ScalarLam := 0;

    // 对于奇素数 L，特征值 λ 和 L-λ 共享同一个核多项式 h(x)。
    // 搜索 λ = 1..(L-1)/2 覆盖全部特征值（2 倍加速）
    {$IFDEF SEA_TRACE} _SeaT('[ElkKern] L=%d deg=%d loop 1..%d', [L, TargetDeg, (L-1) div 2]); {$ENDIF}

    for Lambda := 1 to (L - 1) div 2 do
    begin
      {$IFDEF SEA_TRACE}
      if (Lambda mod ((L - 1) div 10 + 1) = 1) or (Lambda = L - 1) then
        _SeaT('[ElkKern] L=%d Lambda =%d/%d', [L, Lambda, L-1]);
      {$ENDIF}
      if (Lambda and 1) <> 0 then
      begin
        // lambda 为奇数: G = (x^p - x) * F[lambda]^2 + F[lambda-1] * F[lambda+1] * Y2
        BigNumberPolynomialGaloisMul(T1, TCnBigNumberPolynomial(DPs[Lambda]), TCnBigNumberPolynomial(DPs[Lambda]), P, PsiL);
        BigNumberPolynomialGaloisMul(T1, T1, XPmX, P, PsiL);

        BigNumberPolynomialGaloisMul(T2, TCnBigNumberPolynomial(DPs[Lambda - 1]), TCnBigNumberPolynomial(DPs[Lambda + 1]), P, PsiL);
        BigNumberPolynomialGaloisMul(T2, T2, Y2, P, PsiL);

        BigNumberPolynomialGaloisAdd(G, T1, T2, P, PsiL);
      end
      else
      begin
        // lambda 为偶数: G = (x^p - x) * F[lambda]^2 * Y2 + F[lambda-1] * F[lambda+1]
        BigNumberPolynomialGaloisMul(T1, TCnBigNumberPolynomial(DPs[Lambda]), TCnBigNumberPolynomial(DPs[Lambda]), P, PsiL);
        BigNumberPolynomialGaloisMul(T1, T1, XPmX, P, PsiL);
        BigNumberPolynomialGaloisMul(T1, T1, Y2, P, PsiL);

        BigNumberPolynomialGaloisMul(T2, TCnBigNumberPolynomial(DPs[Lambda - 1]), TCnBigNumberPolynomial(DPs[Lambda + 1]), P, PsiL);

        BigNumberPolynomialGaloisAdd(G, T1, T2, P, PsiL);
      end;

      // H = gcd(Psi_L, G)
      // 当 G 是零多项式时（如 lambda=1 的标量 Frobenius）
      // and x^p = x mod PsiL), gcd(PsiL, 0) = PsiL.
      if G.IsZero then
        BigNumberPolynomialCopy(H, PsiL)
      else
        BigNumberPolynomialGaloisGreatestCommonDivisor(H, PsiL, G, P);

      if H.MaxDegree = TargetDeg then
      begin
        BigNumberPolynomialCopy(Res, H);
        Found := True;
        Break;
      end
      else if (H.MaxDegree > TargetDeg) and (H.MaxDegree mod TargetDeg = 0) then
      begin
        // GCD degree is a multiple of TargetDeg - both eigenvalue kernels
        // are in F_p (e.g. when t = 0 mod L and -p is QR mod L).
        // Factor H and extract a factor of degree TargetDeg.
        CzFactors := TCnBigNumberPolynomialList.Create;
        try
          CzOk := BigNumberPolynomialGaloisFactorCantorZassenhaus(CzFactors, H, P);
          if CzOk then
          begin
            for CzI := 0 to CzFactors.Count - 1 do
            begin
              if TCnBigNumberPolynomial(CzFactors[CzI]).MaxDegree = TargetDeg then
              begin
                BigNumberPolynomialCopy(Res, TCnBigNumberPolynomial(CzFactors[CzI]));
                Found := True;
                Break;
              end;
            end;
          end;
        finally
          CzFactors.Free;
        end;
        if Found then Break;
        // CZ factorization failed. If GCD = full Psi_L, Frobenius is scalar
        // 具有特征值 Lambda。记录下来供直接迹计算。
        if (H.MaxDegree = PsiL.MaxDegree) and (ScalarLam = 0) then
          ScalarLam := Lambda;
      end;
    end;

    if not Found then
    begin
      // 如果检测到标量 Frobenius（对某些 lambda 有 GCD = 完全 Psi_L），
      // 通过检查 y 坐标作用来确定实际特征值。
      // When x^p = x mod PsiL, Frobenius is ±1 on E[L].
      // Y2^((p-1)/2) = 1 means pi = +1 (eigenvalue 1).
      // Y2^((p-1)/2) = -1 means pi = -1 (eigenvalue L-1).
      if ScalarLam > 0 then
      begin
        BQ := FSeaBigNumberPool.Obtain;
        try
          BigNumberCopy(BQ, P);
          BQ.SubWord(1);
          BigNumberShiftRightOne(BQ, BQ); // (p-1)/2
          BigNumberPolynomialGaloisPowerBarrett(T2, Y2, BQ, P, PsiL);
          if T2.IsOne then
            ScalarLam := 1
          else
            ScalarLam := L - 1;
        finally
          FSeaBigNumberPool.Recycle(BQ);
        end;
        if ScalarLambda <> nil then
          ScalarLambda^ := ScalarLam;
        // 返回 Psi_L 作为核多项式（完全度数）。
        BigNumberPolynomialCopy(Res, PsiL);
        Result := True;
      end;
      Exit;
    end;

    // Normal success: also pass scalar eigenvalue if available
    if (ScalarLambda <> nil) and (ScalarLam > 0) then
      ScalarLambda^ := ScalarLam;

    Result := True;
  finally
    FSeaPolynomialPool.Recycle(H);
    FSeaPolynomialPool.Recycle(G);
    FSeaPolynomialPool.Recycle(T2);
    FSeaPolynomialPool.Recycle(T1);
    FSeaPolynomialPool.Recycle(XPmX);
    FSeaPolynomialPool.Recycle(XPowP);
    FSeaPolynomialPool.Recycle(Y2);
    if OwnDPs then
      DPs.Free;
  end;
end;

// ==================== Elkies Trace: Polynomial-Only Point Operations ====================

// Polynomial-only elliptic curve point addition (handles doubling and identity).
// Points are (X, Y) where actual coords are (X, Y * sqrt(Y2(x))).
// 商环 F_p[x]/(h(x)) 中的所有运算通过多项式模逆实现。
procedure SeaPolyPointAdd(SX, SY: TCnBigNumberPolynomial;
  PX, PY, QX, QY, Y2, InvY2, H: TCnBigNumberPolynomial;
  A, P: TCnBigNumber);
var
  T1, T2, T3, R, Delta, InvDelta: TCnBigNumberPolynomial;
begin
  // Handle identity element
  if PX.IsZero and PY.IsZero then
  begin
    BigNumberPolynomialCopy(SX, QX);
    BigNumberPolynomialCopy(SY, QY);
    Exit;
  end;
  if QX.IsZero and QY.IsZero then
  begin
    BigNumberPolynomialCopy(SX, PX);
    BigNumberPolynomialCopy(SY, PY);
    Exit;
  end;

  T1 := nil;
  T2 := nil;
  T3 := nil;
  R := nil;
  Delta := nil;
  InvDelta := nil;
  try
    T1 := FSeaPolynomialPool.Obtain;
    T2 := FSeaPolynomialPool.Obtain;
    T3 := FSeaPolynomialPool.Obtain;
    R := FSeaPolynomialPool.Obtain;
    Delta := FSeaPolynomialPool.Obtain;
    InvDelta := FSeaPolynomialPool.Obtain;

    if BigNumberPolynomialGaloisEqual(PX, QX, P) then
    begin
      // X equal: doubling or opposite point
      if BigNumberPolynomialGaloisEqual(PY, QY, P) then
      begin
        // Doubling [2]P
        // Num = 3*PX^2 + A
        BigNumberPolynomialGaloisMul(T1, PX, PX, P, H);
        BigNumberPolynomialGaloisAdd(T2, T1, T1, P, H);   // 2*PX^2
        BigNumberPolynomialGaloisAdd(T2, T2, T1, P, H);   // 3*PX^2
        BigNumberPolynomialGaloisAddBigNumber(T2, A, P);   // 3*PX^2 + A

        // Den = 2*PY
        BigNumberPolynomialGaloisAdd(T3, PY, PY, P, H);

        // R = Num * Den^{-1}
        BigNumberPolynomialGaloisModularInverse(InvDelta, T3, H, P);
        BigNumberPolynomialGaloisMul(R, T2, InvDelta, P, H);

        // SX = R^2 * InvY2 - 2*PX
        BigNumberPolynomialGaloisMul(T1, R, R, P, H);
        BigNumberPolynomialGaloisMul(SX, T1, InvY2, P, H);
        BigNumberPolynomialGaloisSub(T1, SX, PX, P, H);
        BigNumberPolynomialGaloisSub(SX, T1, PX, P, H);

        // SY = R * (PX - SX) * InvY2 - PY
        BigNumberPolynomialGaloisSub(T1, PX, SX, P, H);
        BigNumberPolynomialGaloisMul(SY, R, T1, P, H);
        BigNumberPolynomialGaloisMul(SY, SY, InvY2, P, H);
        BigNumberPolynomialGaloisSub(SY, SY, PY, P, H);
      end
      else
      begin
        // Y opposite, result is identity
        SX.SetZero;
        SY.SetZero;
      end;
    end
    else
    begin
      // Generic addition P + Q (X1 != X2)
      // R = (QY - PY) / (QX - PX)
      BigNumberPolynomialGaloisSub(Delta, QX, PX, P, H);
      BigNumberPolynomialGaloisModularInverse(InvDelta, Delta, H, P);
      BigNumberPolynomialGaloisSub(T1, QY, PY, P, H);
      BigNumberPolynomialGaloisMul(R, T1, InvDelta, P, H);

      // SX = R^2 * Y2 - PX - QX
      BigNumberPolynomialGaloisMul(T1, R, R, P, H);
      BigNumberPolynomialGaloisMul(SX, T1, Y2, P, H);
      BigNumberPolynomialGaloisSub(SX, SX, PX, P, H);
      BigNumberPolynomialGaloisSub(SX, SX, QX, P, H);

      // SY = R * (PX - SX) - PY
      BigNumberPolynomialGaloisSub(T1, PX, SX, P, H);
      BigNumberPolynomialGaloisMul(SY, R, T1, P, H);
      BigNumberPolynomialGaloisSub(SY, SY, PY, P, H);
    end;
  finally
    FSeaPolynomialPool.Recycle(InvDelta);
    FSeaPolynomialPool.Recycle(Delta);
    FSeaPolynomialPool.Recycle(R);
    FSeaPolynomialPool.Recycle(T3);
    FSeaPolynomialPool.Recycle(T2);
    FSeaPolynomialPool.Recycle(T1);
  end;
end;

function CnSeaElkiesTrace(Res: TCnBigNumber; L: Integer;
  A, B, P: TCnBigNumber; DPs: TObjectList = nil): Boolean;
var
  H, Y2, InvY2: TCnBigNumberPolynomial;
  BQ: TCnBigNumber;
  // Frobenius 像与点坐标用纯多项式表示（非有理式）
  PiPX, PiPY: TCnBigNumberPolynomial;
  PX, PY: TCnBigNumberPolynomial;
  // 线性搜索迭代变量
  RSX, RSY, TSX, TSY: TCnBigNumberPolynomial;
  // BSGS 变量
  BabyX, BabyY: array of TCnBigNumberPolynomial;
  NegMX, NegMY: TCnBigNumberPolynomial;
  GX, GY: TCnBigNumberPolynomial;
  M, BabyIdx, GiantIdx, Lambda: Integer;
  UseBSGS: Boolean;
  I: Integer;
  T, LambdaInv, K: TCnBigNumber;
  Found: Boolean;
  BabyLen, GiantMax: Integer;
  ScalarLam: Integer;
begin
  Result := False;
  if (Res = nil) or (L < 3) then Exit;
  if (A = nil) or (B = nil) or (P = nil) then Exit;

  H := nil;
  Y2 := nil;
  InvY2 := nil;
  BQ := nil;
  PiPX := nil;
  PiPY := nil;
  PX := nil;
  PY := nil;
  RSX := nil;
  RSY := nil;
  TSX := nil;
  TSY := nil;
  NegMX := nil;
  NegMY := nil;
  GX := nil;
  GY := nil;
  T := nil;
  LambdaInv := nil;
  K := nil;
  SetLength(BabyX, 0);
  SetLength(BabyY, 0);
  try
    // 步骤1：计算核多项式 h(x)，度数 (L-1)/2
    H := FSeaPolynomialPool.Obtain;
    ScalarLam := 0;
    {$IFDEF SEA_TRACE} _SeaT('[ElkiesTrace] L=%d KernelPoly start', [L]); {$ENDIF}
    if not CnSeaElkiesKernelPolynomial(H, L, A, B, P, DPs, @ScalarLam) then Exit;
    {$IFDEF SEA_TRACE} _SeaT('[ElkiesTrace] L=%d KernelPoly done deg=%d sLam=%d', [L, H.MaxDegree, ScalarLam]); {$ENDIF}

    // 标量 Frobenius：特征值已知，直接计算迹
    // t = lambda + p * lambda^{-1} mod L
    if ScalarLam > 0 then
    begin
      T := FSeaBigNumberPool.Obtain;
      LambdaInv := FSeaBigNumberPool.Obtain;
      K := FSeaBigNumberPool.Obtain;
      K.SetWord(L);
      BigNumberSetWord(T, ScalarLam);
      BigNumberModularInverse(LambdaInv, T, K);
      // t = lambda + p * lambda^{-1} mod L
      BigNumberSetWord(T, ScalarLam);
      BigNumberMod(T, T, K);
      BigNumberMod(LambdaInv, LambdaInv, K);
      BigNumberMul(Res, P, LambdaInv);
      BigNumberMod(Res, Res, K);
      BigNumberAdd(Res, Res, T);
      BigNumberMod(Res, Res, K);
      Result := True;
      Exit;
    end;

    // 步骤2：曲线多项式 Y2 = x^3 + Ax + B
    Y2 := FSeaPolynomialPool.Obtain;
    Y2.SetCoefficients([B, A, 0, 1]);

    BQ := FSeaBigNumberPool.Obtain;

    // 步骤2b：预计算 InvY2 = Y2^{-1} mod h（用于倍点）
    InvY2 := FSeaPolynomialPool.Obtain;
    BigNumberPolynomialGaloisModularInverse(InvY2, Y2, H, P);

    // 步骤3：用纯多项式计算 Frobenius 像 pi(P)
    // pi(P) x-component = x^p mod h(x)
    // pi(P) y-coefficient = Y2^((p-1)/2) mod h(x)
    PiPX := FSeaPolynomialPool.Obtain;
    PiPX.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPowerBarrett(PiPX, PiPX, P, P, H); // x^p mod h

    PiPY := FSeaPolynomialPool.Obtain;
    BigNumberCopy(BQ, P);
    BQ.SubWord(1);
    BigNumberShiftRightOne(BQ, BQ); // (p-1)/2
    BigNumberPolynomialGaloisPowerBarrett(PiPY, Y2, BQ, P, H); // Y2^((p-1)/2) mod h

    // 步骤4：通用点 P = (x, 1) 用多项式表示
    PX := FSeaPolynomialPool.Obtain;
    PX.SetCoefficients([0, 1]); // x
    // Reduce PX mod h(x) for proper quotient ring representation.
    // When deg(H) = 1 (L=3), x must be reduced to a constant.
    if H.MaxDegree <= PX.MaxDegree then
    begin
      RSX := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialGaloisDiv(nil, RSX, PX, H, P);
      BigNumberPolynomialCopy(PX, RSX);
      FSeaPolynomialPool.Recycle(RSX);
      RSX := nil;
    end;
    PY := FSeaPolynomialPool.Obtain;
    PY.SetOne; // 1

    // 步骤5-6：搜索特征值 lambda 使 [lambda]P = pi(P)
    Found := False;
    Lambda := 0;

    UseBSGS := (L >= CN_SEA_ELKIES_BSGS_THRESHOLD);

    {$IFDEF SEA_TRACE} _SeaT('[ElkiesTrace] L=%d eigen search %s', [L, 'BSGS']); {$ENDIF}

    if UseBSGS then
    begin
      // ===== BSGS 搜索 =====
      // lambda = a + b*m, a in {1..m}, b in {0..m}
      // Baby: [1]P, [2]P, ..., [m]P
      // 大步：G = pi(P) - [b*m]P，查找 G = [a]P

      M := 1;
      while M * M < ((L + 1) div 2) do
        Inc(M);

      BabyLen := M;
      SetLength(BabyX, BabyLen);
      SetLength(BabyY, BabyLen);

      // 小步0：[1]P = (x, 1)
      BabyX[0] := FSeaPolynomialPool.Obtain;
      BabyY[0] := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialCopy(BabyX[0], PX);
      BigNumberPolynomialCopy(BabyY[0], PY);

      // 小步1：[2]P（[1]P 的倍点）
      if BabyLen >= 2 then
      begin
        BabyX[1] := FSeaPolynomialPool.Obtain;
        BabyY[1] := FSeaPolynomialPool.Obtain;
        SeaPolyPointAdd(BabyX[1], BabyY[1], BabyX[0], BabyY[0],
          BabyX[0], BabyY[0], Y2, InvY2, H, A, P);
      end;

      // 小步2..m-1：[a+1]P = [a]P + P
      for BabyIdx := 2 to BabyLen - 1 do
      begin
        BabyX[BabyIdx] := FSeaPolynomialPool.Obtain;
        BabyY[BabyIdx] := FSeaPolynomialPool.Obtain;
        SeaPolyPointAdd(BabyX[BabyIdx], BabyY[BabyIdx], BabyX[BabyIdx - 1], BabyY[BabyIdx - 1],
          PX, PY, Y2, InvY2, H, A, P);
      end;

      // [m]P is in BabyX[m-1], BabyY[m-1]
      // 计算 -[m]P = (MX, -MY)
      NegMX := FSeaPolynomialPool.Obtain;
      NegMY := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialCopy(NegMX, BabyX[BabyLen - 1]);
      BigNumberPolynomialCopy(NegMY, BabyY[BabyLen - 1]);
      BigNumberPolynomialGaloisNegate(NegMY, P);

      // 大步：G = pi(P)
      GX := FSeaPolynomialPool.Obtain;
      GY := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialCopy(GX, PiPX);
      BigNumberPolynomialCopy(GY, PiPY);

      GiantMax := M + 1;
      for GiantIdx := 0 to GiantMax do
      begin
        // 检查 G 是否为单位元（lambda = b*m）
        if GX.IsZero and GY.IsZero then
        begin
          if GiantIdx > 0 then
          begin
            Lambda := GiantIdx * M;
            Found := True;
            Break;
          end;
        end
        else
        begin
          // 在小步表中搜索 X 坐标匹配
          for BabyIdx := 0 to BabyLen - 1 do
          begin
            if BigNumberPolynomialGaloisEqual(GX, BabyX[BabyIdx], P) then
            begin
              // X 坐标匹配，检查 Y 符号
              if BigNumberPolynomialGaloisEqual(GY, BabyY[BabyIdx], P) then
                Lambda := (BabyIdx + 1) + GiantIdx * M
              else
                Lambda := L - ((BabyIdx + 1) + GiantIdx * M);
              Found := True;
              Break;
            end;
          end;
          if Found then Break;
        end;

        // G = G + (-[m]P) = G - [m]P
        if GiantIdx < GiantMax then
        begin
          TSX := FSeaPolynomialPool.Obtain;
          TSY := FSeaPolynomialPool.Obtain;
          SeaPolyPointAdd(TSX, TSY, GX, GY, NegMX, NegMY, Y2, InvY2, H, A, P);
          BigNumberPolynomialCopy(GX, TSX);
          BigNumberPolynomialCopy(GY, TSY);
          FSeaPolynomialPool.Recycle(TSX);
          FSeaPolynomialPool.Recycle(TSY);
          TSX := nil;
          TSY := nil;
        end;
      end;
    end
    else
    begin
      // ===== 线性搜索（纯多项式）=====
      RSX := FSeaPolynomialPool.Obtain;
      RSY := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialCopy(RSX, PX); // [1]P
      BigNumberPolynomialCopy(RSY, PY);

      TSX := FSeaPolynomialPool.Obtain;
      TSY := FSeaPolynomialPool.Obtain;

      for I := 1 to (L + 1) div 2 do
      begin
        if BigNumberPolynomialGaloisEqual(RSX, PiPX, P) then
        begin
          if BigNumberPolynomialGaloisEqual(RSY, PiPY, P) then
            Lambda := I
          else
            Lambda := L - I;
          Found := True;
          Break;
        end;

      // RS = RS + P = [I+1]P（增量加法）
        if I < (L + 1) div 2 then
        begin
          SeaPolyPointAdd(TSX, TSY, RSX, RSY, PX, PY, Y2, InvY2, H, A, P);
          BigNumberPolynomialCopy(RSX, TSX);
          BigNumberPolynomialCopy(RSY, TSY);
        end;
      end;
    end;

    if not Found then Exit;

    // 步骤 7：计算 t = lambda + p * lambda^{-1} mod L
    T := FSeaBigNumberPool.Obtain;
    LambdaInv := FSeaBigNumberPool.Obtain;
    K := FSeaBigNumberPool.Obtain;

    if Lambda = 0 then
    begin
      // t = 0 mod L
      Res.SetZero;
      Result := True;
      Exit;
    end;

    // lambda^{-1} mod L
    K.SetWord(L);
    BigNumberSetWord(T, Lambda);
    BigNumberModularInverse(LambdaInv, T, K);

    // t = lambda + p * lambda^{-1} mod L
    BigNumberSetWord(T, Lambda);
    BigNumberMod(T, T, K);
    BigNumberMod(LambdaInv, LambdaInv, K);
    BigNumberMul(Res, P, LambdaInv);
    BigNumberMod(Res, Res, K);
    BigNumberAdd(Res, Res, T);
    BigNumberMod(Res, Res, K);

    Result := True;
  finally
    FSeaBigNumberPool.Recycle(K);
    FSeaBigNumberPool.Recycle(LambdaInv);
    FSeaBigNumberPool.Recycle(T);
    FSeaPolynomialPool.Recycle(GY);
    FSeaPolynomialPool.Recycle(GX);
    FSeaPolynomialPool.Recycle(NegMY);
    FSeaPolynomialPool.Recycle(NegMX);
    // 回收小步表
    for BabyIdx := 0 to Length(BabyX) - 1 do
    begin
      if BabyX[BabyIdx] <> nil then
        FSeaPolynomialPool.Recycle(BabyX[BabyIdx]);
      if BabyY[BabyIdx] <> nil then
        FSeaPolynomialPool.Recycle(BabyY[BabyIdx]);
    end;
    SetLength(BabyX, 0);
    SetLength(BabyY, 0);
    FSeaPolynomialPool.Recycle(TSY);
    FSeaPolynomialPool.Recycle(TSX);
    FSeaPolynomialPool.Recycle(RSY);
    FSeaPolynomialPool.Recycle(RSX);
    FSeaPolynomialPool.Recycle(PY);
    FSeaPolynomialPool.Recycle(PX);
    FSeaPolynomialPool.Recycle(PiPY);
    FSeaPolynomialPool.Recycle(PiPX);
    FSeaPolynomialPool.Recycle(InvY2);
    FSeaBigNumberPool.Recycle(BQ);
    FSeaPolynomialPool.Recycle(Y2);
    FSeaPolynomialPool.Recycle(H);
  end;
end;

// 通过检查 E/F_p 上一点 P 满足 [p+1-t]P = O 来验证候选迹 t
function SeaVerifyTrace(A, B, P, T: TCnBigNumber): Boolean;
var
  N, X, Y2, Y, RX, RY, SX, SY, Lam, T1, T2, T3: TCnBigNumber;
  Inf: Boolean;
  I, Bits: Integer;
  StartX: Int64;
  PointCount: Integer;
begin
  Result := False;
  N := nil; X := nil; Y2 := nil; Y := nil;
  RX := nil; RY := nil; SX := nil; SY := nil;
  Lam := nil; T1 := nil; T2 := nil; T3 := nil;
  try
    N := FSeaBigNumberPool.Obtain;
    X := FSeaBigNumberPool.Obtain;
    Y2 := FSeaBigNumberPool.Obtain;
    Y := FSeaBigNumberPool.Obtain;
    RX := FSeaBigNumberPool.Obtain;
    RY := FSeaBigNumberPool.Obtain;
    SX := FSeaBigNumberPool.Obtain;
    SY := FSeaBigNumberPool.Obtain;
    Lam := FSeaBigNumberPool.Obtain;
    T1 := FSeaBigNumberPool.Obtain;
    T2 := FSeaBigNumberPool.Obtain;
    T3 := FSeaBigNumberPool.Obtain;

    // N = |p + 1 - t|
    BigNumberSub(N, P, T);
    BigNumberAddWord(N, 1);
    if N.IsNegative then
      N.Negate;

    // 尝试多个点，避免小阶点引起的误报。
    // For a 48-bit prime, trying 3 points gives false-positive probability
    // < 1/2^48, which is sufficient.
    PointCount := 0;
    StartX := 0;
    while (StartX < 10000) and (PointCount < 3) do
    begin
      X.SetWord(StartX);
      if BigNumberCompare(X, P) >= 0 then
        Exit; // Ran out of x values

      // y^2 = x^3 + Ax + B mod p
      BigNumberMul(Y2, X, X);
      BigNumberMod(Y2, Y2, P);
      BigNumberMul(Y2, Y2, X);
      BigNumberMod(Y2, Y2, P);
      BigNumberMul(T1, A, X);
      BigNumberMod(T1, T1, P);
      BigNumberAdd(Y2, Y2, T1);
      BigNumberMod(Y2, Y2, P);
      BigNumberAdd(Y2, Y2, B);
      BigNumberMod(Y2, Y2, P);

      Inc(StartX);
      if Y2.IsZero then
      begin
        // Point (X, 0) — order 2, skip (too small)
        Continue;
      end;

      // 通过欧拉准则检查是否为二次剩余
      BigNumberCopy(T1, P);
      T1.SubWord(1);
      BigNumberShiftRightOne(T1, T1);
      BigNumberPowerMod(T2, Y2, T1, P);
      if not T2.IsOne then
        Continue; // 不是二次剩余，此 x 坐标无对应点

      if not BigNumberTonelliShanks(Y, Y2, P) then
        Continue;

      BigNumberCopy(SX, X);
      BigNumberCopy(SY, Y);
      Inc(PointCount);

      // 使用倍加算法（MSB 到 LSB）计算 [N]P
      Inf := True;
      Bits := BigNumberGetBitsCount(N);

      for I := Bits - 1 downto 0 do
      begin
        if not Inf then
        begin
          // R = 2*R (point doubling)
          // lambda = (3*RX^2 + A) / (2*RY) mod p
          BigNumberMul(T1, RX, RX);
          BigNumberMod(T1, T1, P);
          BigNumberMulWord(T1, 3);
          BigNumberMod(T1, T1, P);
          BigNumberAdd(T1, T1, A);
          BigNumberMod(T1, T1, P);

          BigNumberSetWord(T2, 2);
          BigNumberMul(T2, T2, RY);
          BigNumberMod(T2, T2, P);
          if T2.IsZero then
          begin
            Inf := True;
          end
          else
          begin
            BigNumberModularInverse(T3, T2, P);
            BigNumberMul(Lam, T1, T3);
            BigNumberMod(Lam, Lam, P);

            BigNumberMul(T1, Lam, Lam);
            BigNumberMod(T1, T1, P);
            BigNumberSetWord(T2, 2);
            BigNumberMul(T2, T2, RX);
            BigNumberMod(T2, T2, P);
            BigNumberSub(T1, T1, T2);
            BigNumberMod(T1, T1, P);

            BigNumberSub(T2, RX, T1);
            BigNumberMod(T2, T2, P);
            BigNumberMul(T3, Lam, T2);
            BigNumberMod(T3, T3, P);
            BigNumberSub(T3, T3, RY);
            BigNumberMod(T3, T3, P);

            BigNumberCopy(RX, T1);
            BigNumberCopy(RY, T3);
          end;
        end;

        if BigNumberIsBitSet(N, I) then
        begin
          if Inf then
          begin
            BigNumberCopy(RX, SX);
            BigNumberCopy(RY, SY);
            Inf := False;
          end
          else
          begin
            // R = R + S (point addition)
            if BigNumberCompare(RX, SX) = 0 then
            begin
              if BigNumberCompare(RY, SY) = 0 then
              begin
                // R = S, so R + S = 2*R (point doubling)
                // The doubling above was of R_old; now R = 2*R_old = S,
                // so we need a NEW doubling to get R + S = 2S.
                // lambda = (3*RX^2 + A) / (2*RY) mod p
                BigNumberMul(T1, RX, RX);
                BigNumberMod(T1, T1, P);
                BigNumberMulWord(T1, 3);
                BigNumberMod(T1, T1, P);
                BigNumberAdd(T1, T1, A);
                BigNumberMod(T1, T1, P);

                BigNumberSetWord(T2, 2);
                BigNumberMul(T2, T2, RY);
                BigNumberMod(T2, T2, P);
                if T2.IsZero then
                  Inf := True
                else
                begin
                  BigNumberModularInverse(T3, T2, P);
                  BigNumberMul(Lam, T1, T3);
                  BigNumberMod(Lam, Lam, P);

                  BigNumberMul(T1, Lam, Lam);
                  BigNumberMod(T1, T1, P);
                  BigNumberSetWord(T2, 2);
                  BigNumberMul(T2, T2, RX);
                  BigNumberMod(T2, T2, P);
                  BigNumberSub(T1, T1, T2);
                  BigNumberMod(T1, T1, P);

                  BigNumberSub(T2, RX, T1);
                  BigNumberMod(T2, T2, P);
                  BigNumberMul(T3, Lam, T2);
                  BigNumberMod(T3, T3, P);
                  BigNumberSub(T3, T3, RY);
                  BigNumberMod(T3, T3, P);

                  BigNumberCopy(RX, T1);
                  BigNumberCopy(RY, T3);
                end;
                Continue;
              end
              else
              begin
                Inf := True; // R = -S
                Continue;
              end;
            end;

            // lambda = (SY - RY) / (SX - RX) mod p
            BigNumberSub(T1, SY, RY);
            BigNumberMod(T1, T1, P);
            BigNumberSub(T2, SX, RX);
            BigNumberMod(T2, T2, P);
            BigNumberModularInverse(T3, T2, P);
            BigNumberMul(Lam, T1, T3);
            BigNumberMod(Lam, Lam, P);

            BigNumberMul(T1, Lam, Lam);
            BigNumberMod(T1, T1, P);
            BigNumberSub(T1, T1, RX);
            BigNumberMod(T1, T1, P);
            BigNumberSub(T1, T1, SX);
            BigNumberMod(T1, T1, P);

            BigNumberSub(T2, RX, T1);
            BigNumberMod(T2, T2, P);
            BigNumberMul(T3, Lam, T2);
            BigNumberMod(T3, T3, P);
            BigNumberSub(T3, T3, RY);
            BigNumberMod(T3, T3, P);

            BigNumberCopy(RX, T1);
            BigNumberCopy(RY, T3);
          end;
        end;
      end;

      // 若任意点 [N]P != O，则拒绝此迹
      if not Inf then
        Exit;
    end;

    // 所有点验证通过：对所有测试点 [N]P = O
    Result := PointCount > 0;
  finally
    FSeaBigNumberPool.Recycle(T3);
	FSeaBigNumberPool.Recycle(T2);
	FSeaBigNumberPool.Recycle(T1);
	FSeaBigNumberPool.Recycle(Lam);
    FSeaBigNumberPool.Recycle(SY);
	FSeaBigNumberPool.Recycle(SX);
	FSeaBigNumberPool.Recycle(RY);
	FSeaBigNumberPool.Recycle(RX);
    FSeaBigNumberPool.Recycle(Y);
	FSeaBigNumberPool.Recycle(Y2);
	FSeaBigNumberPool.Recycle(X);
	FSeaBigNumberPool.Recycle(N);
  end;
end;

function CnSeaCombineElkiesAtkin(Res: TCnBigNumber;
  ElkiesTraces, ElkiesModuli: TCnInt64List;
  AtkinInfos: TObjectList;
  A, B, P: TCnBigNumber): Boolean; forward;

function CnSeaPointCount(Res, A, B, P: TCnBigNumber;
  ModPolys: TObjectList = nil): Boolean;
var
  Pa, Ta: TCnInt64List;
  QMax, QMul, BQ: TCnBigNumber;
  I, J, ModPolyIdx, RequiredModPolyCount, SafetyBits: Integer;
  L: Int64;
  DPs: TObjectList;
  Y2, P1, P2, G: TCnBigNumberPolynomial;
  TraceRes: TCnBigNumber;
  PrimeType: TCnSeaPrimeType;
  ElkiesOk: Boolean;
  AtkinInfos: TObjectList;
  ElkiesTraces, ElkiesModuli: TCnInt64List;
  AtkinTraces: TCnInt64List;
  Info: TCnSeaAtkinInfo;
begin
  Result := False;
  if (Res = nil) or (A = nil) or (B = nil) or (P = nil) then Exit;
  if P.IsZero or P.IsNegative then Exit;

  Pa := nil;
  Ta := nil;
  DPs := nil;
  QMax := nil;
  QMul := nil;
  BQ := nil;
  Y2 := nil;
  P1 := nil;
  P2 := nil;
  G := nil;
  TraceRes := nil;

  AtkinInfos := nil;
  ElkiesTraces := nil;
  ElkiesModuli := nil;
  AtkinTraces := nil;

  try
    Pa := TCnInt64List.Create;
    Ta := TCnInt64List.Create;

    QMax := FSeaBigNumberPool.Obtain;
    QMul := FSeaBigNumberPool.Obtain;
    BQ := FSeaBigNumberPool.Obtain;
    TraceRes := FSeaBigNumberPool.Obtain;
    AtkinInfos := TObjectList.Create(True);
    ElkiesTraces := TCnInt64List.Create;
    ElkiesModuli := TCnInt64List.Create;
    AtkinTraces := TCnInt64List.Create;

    // 计算所需素数列表：乘积 > 4 * sqrt(p) * 2^SafetyBits
    // SafetyBits 随 p 的位数动态增长，确保 Atkin 过滤足够强
    if not BigNumberSqrt(QMax, P) then Exit;
    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);
    SafetyBits := CnSeaSafetyBits(BigNumberGetBitsCount(P));
    for J := 1 to SafetyBits do
      BigNumberShiftLeftOne(QMax, QMax);
    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    while (BigNumberCompare(QMul, QMax) <= 0) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      L := CN_PRIME_NUMBERS_SQRT_UINT32[I];
      // 保护：在可用最大模多项式 L 处停止，避免耗时的即时生成
      if L > CN_SEA_MAX_MODPOLY_L then Break;
      // 跳过 L = P 的情况
      BigNumberSetWord(BQ, L);
      if BigNumberCompare(BQ, P) <> 0 then
      begin
        BigNumberMulWord(QMul, L);
        Pa.Add(L);
        Ta.Add(0);
      end;
      Inc(I);
    end;

    // Note: if L > CN_SEA_MAX_MODPOLY_L guard triggered before threshold was
    // 已使用。CnSeaCombineElkiesAtkin 中的 SkipVerify 逻辑将处理
    // 过滤较弱的情况。
    if Pa.Count = 0 then
      raise ECnEccException.Create('No primes available for SEA.');

    // 验证外部 ModPolys：必须对 Pa 中每个素数 L >= 3 各有一个条目
    if ModPolys <> nil then
    begin
      RequiredModPolyCount := 0;
      for I := 0 to Pa.Count - 1 do
        if Pa[I] >= 3 then
          Inc(RequiredModPolyCount);
      if ModPolys.Count < RequiredModPolyCount then
        raise ECnEccException.CreateFmt(
          'ModPolys has %d entries but SEA needs %d (primes 3..%d for this p). ' +
          'Use CnSeaMaxRequiredPrimeL to determine the required L in advance.',
          [ModPolys.Count, RequiredModPolyCount, Pa[Pa.Count - 1]]);
    end;
    // 准备 Y2 = x^3 + Ax + B
    Y2 := FSeaPolynomialPool.Obtain;
    P1 := FSeaPolynomialPool.Obtain;
    P2 := FSeaPolynomialPool.Obtain;
    G := FSeaPolynomialPool.Obtain;
    Y2.SetCoefficients([B, A, 0, 1]);

    // 处理 L = 2 的特殊情形：检查 gcd(x^p - x, x^3+Ax+B)
    if Pa.Count > 0 then
    begin
      L := Pa[0];
      if L = 2 then
      begin
        {$IFDEF SEA_TRACE} _SeaT('[SEA] L=2 x^p mod Y2 start (deg 3, %d-bit p)', [BigNumberGetBitsCount(P)]); {$ENDIF}
        P1.SetCoefficients([0, 1]); // x
        BigNumberPolynomialGaloisPower(P1, P1, P, P, Y2); // x^p mod Y2
        P2.SetCoefficients([0, 1]); // x
        BigNumberPolynomialGaloisSub(P1, P1, P2, P); // x^p - x
        BigNumberPolynomialGaloisGreatestCommonDivisor(G, P1, Y2, P);
        if G.IsOne then
          Ta[0] := 1
        else
          Ta[0] := 0;
        ElkiesTraces.Add(Ta[0]);
        ElkiesModuli.Add(2);
        {$IFDEF SEA_TRACE} _SeaT('[SEA] L=2 done t=%d', [Ta[0]]); {$ENDIF}
      end;
    end;

    // 预生成除法多项式（供 Schoof 回退使用）
    if Pa.Count > 0 then
    begin
      {$IFDEF SEA_TRACE} _SeaT('[SEA] GenerateDivisionPolynomials start (max deg=%d)', [Pa[Pa.Count - 1] + 2]); {$ENDIF}
      DPs := TObjectList.Create(True);
      CnGenerateGaloisDivisionPolynomials(A, B, P, Pa[Pa.Count - 1] + 2, DPs);
      {$IFDEF SEA_TRACE} _SeaT('[SEA] GenerateDivisionPolynomials done (%d polys)', [DPs.Count]); {$ENDIF}
    end;

    // 对每个素数 L >= 3 计算迹
    ModPolyIdx := 0;
    for I := 0 to Pa.Count - 1 do
    begin
      L := Pa[I];
      if L = 2 then Continue; // 已处理

      {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d start', [L]); {$ENDIF}

      // 检查是 Elkies 还是 Atkin
      if (ModPolys <> nil) and (ModPolyIdx < ModPolys.Count) then
        PrimeType := CnSeaCheckPrimeType(L, A, B, P, nil, TCnBigNumberBiPolynomial(ModPolys[ModPolyIdx]))
      else
        PrimeType := CnSeaCheckPrimeType(L, A, B, P);
      Inc(ModPolyIdx);

      {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d type=%d ElmAtk', [L, Ord(PrimeType)]); {$ENDIF}

      if PrimeType = sptElkies then
      begin
        {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d ElkiesTrace start', [L]); {$ENDIF}
        // 尝试 Elkies 方法
        ElkiesOk := CnSeaElkiesTrace(TraceRes, L, A, B, P, DPs);
        if ElkiesOk then
        begin
          Ta[I] := TraceRes.GetInt64;
          ElkiesTraces.Add(Ta[I]);
          ElkiesModuli.Add(L);
          {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d Elkies OK t=%d', [L, Ta[I]]); {$ENDIF}
        end
        else
        begin
          // Elkies failed (e.g. CM curve where Phi_L has roots in F_p but
          // Frobenius 在 E[L] 中无特征向量），回退到 Atkin。
          {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d Elkies FAIL -> Atkin', [L]); {$ENDIF}
          AtkinTraces.Clear;
          if (ModPolys <> nil) and (ModPolyIdx - 1 < ModPolys.Count) then
            ElkiesOk := CnSeaAtkinPossibleTraces(AtkinTraces, L, A, B, P,
              TCnBigNumberBiPolynomial(ModPolys[ModPolyIdx - 1]))
          else
            ElkiesOk := CnSeaAtkinPossibleTraces(AtkinTraces, L, A, B, P);
          if ElkiesOk then
          begin
            Info := TCnSeaAtkinInfo.Create;
            Info.L := L;
            Info.R := 0;
            Info.PossibleTraces.AddList(AtkinTraces);
            AtkinInfos.Add(Info);
            Ta[I] := -1;
            {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d fallback Atkin %d cand', [L, AtkinTraces.Count]); {$ENDIF}
          end
          else
          begin
            Ta[I] := -1;
          end;
        end;
      end
      else if PrimeType = sptAtkin then
      begin
        {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d AtkinPossibleTraces start', [L]); {$ENDIF}
        AtkinTraces.Clear;
        if (ModPolys <> nil) and (ModPolyIdx - 1 < ModPolys.Count) then
          ElkiesOk := CnSeaAtkinPossibleTraces(AtkinTraces, L, A, B, P,
            TCnBigNumberBiPolynomial(ModPolys[ModPolyIdx - 1]))
        else
          ElkiesOk := CnSeaAtkinPossibleTraces(AtkinTraces, L, A, B, P);
        if ElkiesOk then
        begin
          Info := TCnSeaAtkinInfo.Create;
          Info.L := L;
          Info.R := 0;
          Info.PossibleTraces.AddList(AtkinTraces);
          AtkinInfos.Add(Info);
          Ta[I] := -1;
          {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d Atkin OK %d cand', [L, AtkinTraces.Count]); {$ENDIF}
        end
        else
        begin
          Ta[I] := -1;
          {$IFDEF SEA_TRACE} _SeaT('[SEA] L=%d Atkin FAILED', [L]); {$ENDIF}
        end;
      end
      else
      begin
        Ta[I] := -1;
      end;
    end;

    // 中国剩余定理合并结果
    {$IFDEF SEA_TRACE} _SeaT('[SEA] Combine: %d Elk %d Atk', [ElkiesTraces.Count, AtkinInfos.Count]); {$ENDIF}
    if AtkinInfos.Count > 0 then
    begin
      {$IFDEF SEA_TRACE} _SeaT('[SEA] -> CnSeaCombineElkiesAtkin start', []); {$ENDIF}
      if not CnSeaCombineElkiesAtkin(Res, ElkiesTraces, ElkiesModuli, AtkinInfos, A, B, P) then
      begin
        {$IFDEF SEA_TRACE} _SeaT('[SEA] -> Combine FAIL, Elkies-only CRT', []); {$ENDIF}
        BigNumberChineseRemainderTheorem(Res, ElkiesTraces, ElkiesModuli);
      end;
    end
    else
      BigNumberChineseRemainderTheorem(Res, Ta, Pa);

    // 根据 Hasse 界限定迹的符号：|t| <= 2*sqrt(p)
    BigNumberSqrt(QMax, P);
    QMax.AddWord(1);
    QMax.ShiftLeftOne; // 2*sqrt(p) + 1（上界）

    if BigNumberUnsignedCompare(Res, QMax) >= 0 then
    begin
      // 迹可能为负，减去全部素数乘积
      QMul.SetOne;
      for J := 0 to Pa.Count - 1 do
      begin
        BQ.SetInt64(Pa[J]);
        BigNumberMul(QMul, QMul, BQ);
      end;

      if Res.IsNegative then
        BigNumberAdd(Res, Res, QMul)
      else
        BigNumberSub(Res, Res, QMul);
    end;

    // #E = p + 1 - t
    Res.Negate;
    BigNumberAdd(Res, Res, P);
    Res.AddWord(1);

    Result := True;
  finally
    FSeaPolynomialPool.Recycle(G);
    FSeaPolynomialPool.Recycle(P2);
    FSeaPolynomialPool.Recycle(P1);
    FSeaPolynomialPool.Recycle(Y2);
    FSeaBigNumberPool.Recycle(TraceRes);
    FSeaBigNumberPool.Recycle(BQ);
    FSeaBigNumberPool.Recycle(QMul);
    FSeaBigNumberPool.Recycle(QMax);
    DPs.Free;
    AtkinTraces.Free;
    ElkiesModuli.Free;
    ElkiesTraces.Free;
    AtkinInfos.Free;
    Ta.Free;
    Pa.Free;
  end;
end;

// ==================== Max Required L Calculation ====================

function CnSeaMaxRequiredPrimeL(P: TCnBigNumber): Integer;
var
  QMax, QMul, BQ: TCnBigNumber;
  I, J, SafetyBits: Integer;
  L: Int64;
begin
  Result := 0;
  if (P = nil) or P.IsZero or P.IsNegative then Exit;

  QMax := TCnBigNumber.Create;
  QMul := TCnBigNumber.Create;
  BQ := TCnBigNumber.Create;
  try
    // Extended threshold: product > 4*sqrt(p) * 2^SafetyBits
    // 通过 CnSeaSafetyBits 计算 SafetyBits（分段函数）
    if not BigNumberSqrt(QMax, P) then Exit;
    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);
    SafetyBits := CnSeaSafetyBits(BigNumberGetBitsCount(P));
    for J := 1 to SafetyBits do
      BigNumberShiftLeftOne(QMax, QMax);

    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    while (BigNumberCompare(QMul, QMax) <= 0) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      L := CN_PRIME_NUMBERS_SQRT_UINT32[I];
      // Guard: stop at max available mod poly L
      if L > CN_SEA_MAX_MODPOLY_L then Break;

      // 跳过 L = P（与域特征相同）
      BigNumberSetWord(BQ, L);
      if BigNumberCompare(BQ, P) <> 0 then
      begin
        BigNumberMulWord(QMul, L);
        Result := L;
      end;

      Inc(I);
    end;

    if I > High(CN_PRIME_NUMBERS_SQRT_UINT32) then
      Result := 0;  // P 太大，超出可用素数表范围
  finally
    BQ.Free;
    QMul.Free;
    QMax.Free;
  end;
end;

// ==================== Atkin Prime Handling ====================

function SeaInt64GCD(A, B: Integer): Integer;
begin
  while B <> 0 do
  begin
    Result := A mod B;
    A := B;
    B := Result;
  end;
  Result := A;
end;

function SeaInt64LCM(A, B: Integer): Integer;
var
  G: Integer;
begin
  if (A = 0) or (B = 0) then
  begin
    Result := 0;
    Exit;
  end;
  G := SeaInt64GCD(A, B);
  Result := (A div G) * B;
end;

function SeaInt64PowMod(Base, Exp, M: Int64): Int64;
var
  B, E: Int64;
begin
  Result := 1;
  B := Base mod M;
  E := Exp;
  while E > 0 do
  begin
    if E and 1 = 1 then
      Result := (Result * B) mod M;
    B := (B * B) mod M;
    E := E shr 1;
  end;
end;

function SeaFindPrimitiveRoot(L: Int64): Int64;
var
  N, T, G: Int64;
  Factors: array of Int64;
  I, Idx: Integer;
  IsPrim: Boolean;
begin
  Result := 0;
  if L < 2 then Exit;
  if L = 2 then begin Result := 1; Exit; end;
  N := L - 1;
  T := N;
  SetLength(Factors, 0);
  I := 2;
  while I * I <= T do
  begin
    if T mod I = 0 then
    begin
      Idx := Length(Factors);
      SetLength(Factors, Idx + 1);
      Factors[Idx] := I;
      while T mod I = 0 do T := T div I;
    end;
    Inc(I);
  end;
  if T > 1 then
  begin
    Idx := Length(Factors);
    SetLength(Factors, Idx + 1);
    Factors[Idx] := T;
  end;
  G := 2;
  while G <= L - 1 do
  begin
    IsPrim := True;
    for I := 0 to High(Factors) do
    begin
      if SeaInt64PowMod(G, (L - 1) div Factors[I], L) = 1 then
      begin
        IsPrim := False;
        Break;
      end;
    end;
    if IsPrim then
    begin
      Result := G;
      Exit;
    end;
    Inc(G);
  end;
end;

function SeaFindNonResidue(L: Int64): Int64;
var
  D: Int64;
begin
  Result := 0;
  D := 2;
  while D <= L - 1 do
  begin
    if SeaInt64PowMod(D, (L - 1) div 2, L) = L - 1 then
    begin
      Result := D;
      Exit;
    end;
    Inc(D);
  end;
end;

procedure SeaFp2Mul(a1, b1, a2, b2: Int64; Delta, L: Int64; var ra, rb: Int64);
var
  T1, T2, T3: Int64;
begin
  T1 := (a1 * a2) mod L;
  T2 := (b1 * b2) mod L;
  T2 := (T2 * Delta) mod L;
  ra := (T1 + T2) mod L;
  T3 := (a1 * b2 + b1 * a2) mod L;
  rb := T3;
end;

procedure SeaFp2Pow(a, b: Int64; Exp: Int64; Delta, L: Int64; var ra, rb: Int64);
var
  CA, CB, TA, TB: Int64;
begin
  ra := 1;
  rb := 0;
  CA := a mod L;
  CB := b mod L;
  while Exp > 0 do
  begin
    if Exp and 1 = 1 then
    begin
      SeaFp2Mul(ra, rb, CA, CB, Delta, L, TA, TB);
      ra := TA;
      rb := TB;
    end;
    SeaFp2Mul(CA, CB, CA, CB, Delta, L, TA, TB);
    CA := TA;
    CB := TB;
    Exp := Exp shr 1;
  end;
end;

procedure SeaFindNorm1Generator(var ga, gb: Int64; Delta, L: Int64);
var
  LP1, T: Int64;
  Factors: array of Int64;
  I, Idx: Integer;
  IsGen: Boolean;
  A, B, TA, TB: Int64;
  Q: Int64;
begin
  LP1 := L + 1;
  T := LP1;
  SetLength(Factors, 0);
  I := 2;
  while I * I <= T do
  begin
    if T mod I = 0 then
    begin
      Idx := Length(Factors);
      SetLength(Factors, Idx + 1);
      Factors[Idx] := I;
      while T mod I = 0 do T := T div I;
    end;
    Inc(I);
  end;
  if T > 1 then
  begin
    Idx := Length(Factors);
    SetLength(Factors, Idx + 1);
    Factors[Idx] := T;
  end;
  B := 1;
  while B <= L - 1 do
  begin
    Q := (1 + (Delta * ((B * B) mod L)) mod L) mod L;
    // Q = a^2，对范1元素 (a,b)。接受 Q=0（a=0）或二次剩余。
    if (Q <> 0) and (SeaInt64PowMod(Q, (L - 1) div 2, L) <> 1) then
    begin
      Inc(B);
      Continue;
    end;
    A := 0;
    while A <= L - 1 do
    begin
      if (A * A) mod L = Q then
      begin
        IsGen := True;
        for I := 0 to High(Factors) do
        begin
          SeaFp2Pow(A, B, LP1 div Factors[I], Delta, L, TA, TB);
          if (TA = 1) and (TB = 0) then
          begin
            IsGen := False;
            Break;
          end;
        end;
        if IsGen then
        begin
          ga := A;
          gb := B;
          Exit;
        end;
      end;
      Inc(A);
    end;
    Inc(B);
  end;
end;

procedure SeaAtkinTracesRDivLm1(Traces: TCnInt64List; L: Int64; R: Integer;
  P: TCnBigNumber);
var
  G, PModL, Lambda, LambdaInv, T2: Int64;
  K, T: Int64;
begin
  G := SeaFindPrimitiveRoot(L);
  PModL := BigNumberModWord(P, L);
  K := 0;
  while K <= R - 1 do
  begin
    Lambda := SeaInt64PowMod(G, ((L - 1) div R) * K, L);
    if Lambda = 0 then
    begin
      Inc(K);
      Continue;
    end;
    LambdaInv := SeaInt64PowMod(Lambda, R - 1, L);
    T2 := (Lambda + 2 + LambdaInv) mod L;
    T2 := (PModL * T2) mod L;
    if T2 = 0 then
    begin
      if Traces.IndexOf(0) < 0 then Traces.Add(0);
    end
    else
    begin
      if SeaInt64PowMod(T2, (L - 1) div 2, L) = 1 then
      begin
        T := 0;
        while T <= L - 1 do
        begin
          if (T * T) mod L = T2 then
          begin
            if Traces.IndexOf(T) < 0 then Traces.Add(T);
            if Traces.IndexOf(L - T) < 0 then Traces.Add(L - T);
            Break;
          end;
          Inc(T);
        end;
      end;
    end;
    Inc(K);
  end;
end;

procedure SeaAtkinTracesRDivLp1(Traces: TCnInt64List; L: Int64; R: Integer;
  P: TCnBigNumber);
var
  Delta, GA, GB: Int64;
  LP1DivR, K: Int64;
  LA, LB: Int64;
  T2, T: Int64;
  PModL: Int64;
begin
  Delta := SeaFindNonResidue(L);
  SeaFindNorm1Generator(GA, GB, Delta, L);
  LP1DivR := (L + 1) div R;
  PModL := BigNumberModWord(P, L);
  K := 0;
  while K <= R - 1 do
  begin
    SeaFp2Pow(GA, GB, LP1DivR * K, Delta, L, LA, LB);
    T2 := (2 * PModL * ((LA + 1) mod L)) mod L;
    if T2 = 0 then
    begin
      if Traces.IndexOf(0) < 0 then Traces.Add(0);
    end
    else
    begin
      if SeaInt64PowMod(T2, (L - 1) div 2, L) = 1 then
      begin
        T := 0;
        while T <= L - 1 do
        begin
          if (T * T) mod L = T2 then
          begin
            if Traces.IndexOf(T) < 0 then Traces.Add(T);
            if Traces.IndexOf(L - T) < 0 then Traces.Add(L - T);
            Break;
          end;
          Inc(T);
        end;
      end;
    end;
    Inc(K);
  end;
end;

// TCnSeaAtkinInfo

constructor TCnSeaAtkinInfo.Create;
begin
  inherited Create;
  PossibleTraces := TCnInt64List.Create;
end;

destructor TCnSeaAtkinInfo.Destroy;
begin
  PossibleTraces.Free;
  inherited Destroy;
end;

function CnSeaAtkinPossibleTraces(Traces: TCnInt64List;
  L: Integer; A, B, P: TCnBigNumber;
  PhiL: TCnBigNumberBiPolynomial): Boolean;
var
  J: TCnBigNumber;
  OwnPhiL: Boolean;
  FY: TCnBigNumberPolynomial;
  R: Integer;
  TotalDeg, Multiplicity: Integer;
  SplitType: Integer;
  L64: Int64;
  // DDF-only variables (replaces CZ Factors)
  SFFactors: TCnBigNumberPolynomialList;
  IdxSF, D_DDF, N_DDF, DF_DDF: Integer;
  SF_p, G_p, GH_p, GHX_p, GHSub_p: TCnBigNumberPolynomial;
begin
  Result := False;
  if (Traces = nil) or (L < 3) then Exit;
  if (A = nil) or (B = nil) or (P = nil) then Exit;
  if not CnUInt32IsPrime(L) then Exit;

  Traces.Clear;
  J := FSeaBigNumberPool.Obtain;
  OwnPhiL := (PhiL = nil);
  if OwnPhiL then
    PhiL := TCnBigNumberBiPolynomial.Create;
  FY := FSeaPolynomialPool.Obtain;
  try
    {$IFDEF SEA_TRACE} _SeaT('[Atkin] L=%d j-inv start', [L]); {$ENDIF}
    if not CnSeaJInvariant(J, A, B, P) then Exit;
    if OwnPhiL then
    begin
      if not CnGenerateClassicalModularPolynomial(PhiL, L) then Exit;
    end;
    {$IFDEF SEA_TRACE} _SeaT('[Atkin] L=%d eval Phi', [L]); {$ENDIF}
    if not BigNumberBiPolynomialGaloisEvaluateByX(FY, PhiL, J, P) then Exit;
    if FY.MaxDegree <= 0 then Exit;
    {$IFDEF SEA_TRACE} _SeaT('[Atkin] L=%d FYdeg=%d DDF start', [L, FY.MaxDegree]); {$ENDIF}

    // ---- DDF-only factorization (skip expensive EDF) ----
    // 相异度因式分解得出 G_d = 所有 d 次不可约因式之积。
    // R = 所有 G_d 非平凡的 d 的最小公倍数，无需等度因式分解。
    SFFactors := TCnBigNumberPolynomialList.Create;
    try
      if BigNumberPolynomialGaloisSquareFreeFactorization(SFFactors, FY, P) <= 0 then Exit;
      TotalDeg := 0; R := 1;

      for IdxSF := 0 to SFFactors.Count - 1 do
      begin
        SF_p := TCnBigNumberPolynomial(SFFactors[IdxSF]);
        N_DDF := SF_p.MaxDegree;
        if N_DDF <= 1 then
        begin
          TotalDeg := TotalDeg + N_DDF;
          if N_DDF > 0 then R := SeaInt64LCM(R, N_DDF);
          Continue;
        end;

        GH_p := FSeaPolynomialPool.Obtain;
        GHX_p := FSeaPolynomialPool.Obtain;
        GHSub_p := FSeaPolynomialPool.Obtain;
        G_p := FSeaPolynomialPool.Obtain;
        try
          GHX_p.SetZero; GHX_p.MaxDegree := 1; GHX_p[1].SetWord(1); // x
          BigNumberPolynomialCopy(GH_p, GHX_p);
          DF_DDF := 0; D_DDF := 1;

          while (D_DDF <= N_DDF div 2) and (SF_p.MaxDegree > 0) do
          begin
            BigNumberPolynomialGaloisPower(GH_p, GH_p, P, P, SF_p);
            BigNumberPolynomialGaloisSub(GHSub_p, GH_p, GHX_p, P);
            BigNumberPolynomialGaloisGreatestCommonDivisor(G_p, GHSub_p, SF_p, P);
            if G_p.MaxDegree > 0 then
            begin
              R := SeaInt64LCM(R, D_DDF);
              DF_DDF := DF_DDF + G_p.MaxDegree;
              BigNumberPolynomialGaloisDiv(GHSub_p, nil, SF_p, G_p, P);
              BigNumberPolynomialCopy(SF_p, GHSub_p);
            end;
            Inc(D_DDF);
          end;
          if SF_p.MaxDegree > 0 then
          begin
            R := SeaInt64LCM(R, SF_p.MaxDegree);
            DF_DDF := DF_DDF + SF_p.MaxDegree;
          end;
          TotalDeg := TotalDeg + DF_DDF;
        finally
          FSeaPolynomialPool.Recycle(G_p);
          FSeaPolynomialPool.Recycle(GHSub_p);
          FSeaPolynomialPool.Recycle(GHX_p);
          FSeaPolynomialPool.Recycle(GH_p);
        end;
      end;

      if TotalDeg = 0 then Exit;
      Multiplicity := 1;
      if FY.MaxDegree mod TotalDeg = 0 then
        Multiplicity := FY.MaxDegree div TotalDeg;
      R := R * Multiplicity;
    finally
      SFFactors.Free;
    end;
    if R < 1 then Exit;
    {$IFDEF SEA_TRACE} _SeaT('[Atkin] L=%d DDF done R=%d M=%d', [L, R, Multiplicity]); {$ENDIF}

    L64 := L;
    SplitType := 0;
    if (L64 - 1) mod R = 0 then
      SplitType := 1;
    if (L64 + 1) mod R = 0 then
      SplitType := SplitType or 2;
    if SplitType = 0 then Exit;

    // 当 R 同时整除 L-1 和 L+1（如 R=2, L 为奇数），尝试两种
    // 分裂类型并合并候选。正确的特征值比
    // gamma 可能位于 F_L* 或 F_{L^2}* 的范1子群中。
    if (SplitType and 1) <> 0 then
      SeaAtkinTracesRDivLm1(Traces, L64, R, P);
    if (SplitType and 2) <> 0 then
      SeaAtkinTracesRDivLp1(Traces, L64, R, P);

    Result := Traces.Count > 0;
  finally
    FSeaPolynomialPool.Recycle(FY);
    if OwnPhiL then PhiL.Free;
    FSeaBigNumberPool.Recycle(J);
  end;
end;

function CnSeaCombineElkiesAtkin(Res: TCnBigNumber;
  ElkiesTraces, ElkiesModuli: TCnInt64List;
  AtkinInfos: TObjectList;
  A, B, P: TCnBigNumber): Boolean;
var
  I, K: Integer;
  M_E, T_E: TCnBigNumber;
  QMax, QTmp, Tmp, T: TCnBigNumber;
  N: Integer;
  Found, Verified, UseBSGS, SkipVerify: Boolean;
  TMod: Int64;
  AtkinFilterRatio, E_fp_Log2: Double;
  LVal, L1Val, LCheck, InvME64: Int64;
  BestIdx, Dir: Integer;
  BabyR: Int64;
  // 多素数 CRT 选取
  SelIdx: array[0..5] of Integer;
  SelL: array[0..5] of Int64;
  SelC: array[0..5] of Integer;
  SelCount, SelBestIdx: Integer;
  SelBestR, SelR: Double;
  SelFound: Boolean;
  SelK, SelJ, SelPrev: Integer;
  CRT_LProd, CRT_Lk, CRT_InvMk, CRT_TModK, CRT_R2, CRT_R1, CRT_Diff: Int64;
  CRTOldCnt: Integer;
  S1, S2: TCnInt64List;  // CRT 临时列表

  // Int64 预计算
  RmL: array of Int64;
  RmTE: array of Int64;       // T_E mod L (read-only)
  RmGS: array of Int64;
  RmBaby0: array of Int64;
  RmBase: array of Int64;     // per-direction base_t mod L (scratch)
  RmLUT: array of Boolean;        // flattened 1D O(1) membership
  RmLUTOff: array of Integer;     // RmLUT 的行起始偏移
  BabyMod1D: array of Int64;  // flattened: [K * BabyCnt + I]
  BabyCnt: Integer;
  Survive: array of Boolean;
  RmCount, RmIdx: Integer;
  RmSkp: Boolean;
  RmTModL, RmBm, RmLk: Int64;

  // BSGS 变量
  BabyRs: TCnInt64List;
  GStep, BaseT, Threshold: TCnBigNumber;
  GiantSize, GiantMax, SurvCnt: Int64;
begin
  Result := False;
  M_E := nil;
  T_E := nil;
  QMax := nil;
  QTmp := nil;
  Tmp := nil;
  T := nil;
  GStep := nil;
  BaseT := nil;
  Threshold := nil;
  BabyRs := nil;

  try
    M_E := FSeaBigNumberPool.Obtain;
    T_E := FSeaBigNumberPool.Obtain;
    QMax := FSeaBigNumberPool.Obtain;
    QTmp := FSeaBigNumberPool.Obtain;
    Tmp := FSeaBigNumberPool.Obtain;
    T := FSeaBigNumberPool.Obtain;
    GStep := FSeaBigNumberPool.Obtain;
    BaseT := FSeaBigNumberPool.Obtain;
    Threshold := FSeaBigNumberPool.Obtain;

    // 对 Elkies 结果做 CRT
    if ElkiesTraces.Count > 0 then
    begin
      if not BigNumberChineseRemainderTheorem(T_E, ElkiesTraces, ElkiesModuli) then
        Exit;
      M_E.SetOne;
      for I := 0 to ElkiesModuli.Count - 1 do
      begin
        QTmp.SetInt64(ElkiesModuli[I]);
        BigNumberMul(M_E, M_E, QTmp);
      end;
    end
    else
    begin
      T_E.SetZero;
      M_E.SetOne;
    end;

    // Hasse 界：|t| <= 2*sqrt(p)。用 2*(sqrt(p)+1) 作为安全冗余。
    BigNumberSqrt(QMax, P);
    QMax.AddWord(1);
    BigNumberMulWord(QMax, 2);

    // 搜索范围：t = t_E + k*M_E 必须落在 [-QMax, QMax] 内。
    // 由于 t_E ∈ [0, M_E)，k 最多覆盖 ceil(2*QMax/M_E)+1 个值。
    // 候选总数 = floor(2*QMax / M_E) + 1
    BigNumberAdd(Tmp, QMax, QMax);         // Tmp = 2*QMax
    BigNumberDiv(Tmp, nil, Tmp, M_E);      // Tmp = floor(2*QMax / M_E)

    // ---- 计算预期误报以判断是否需要验证 ----
    // E_fp = (2*QMax / M_E) * 所有Atkin素数 (|S_i|/L_i) 之积
    // 若 E_fp < 1，则 Atkin 过滤足够强，最多只有一个候选迹
    // 能够存活，点验证不再必要。
    AtkinFilterRatio := 1.0;
    for I := 0 to AtkinInfos.Count - 1 do
      AtkinFilterRatio := AtkinFilterRatio *
        (TCnSeaAtkinInfo(AtkinInfos[I]).PossibleTraces.Count /
         TCnSeaAtkinInfo(AtkinInfos[I]).L);
    // log2(E_fp) ≈ bits(2*QMax) - bits(M_E) + log2(AtkinFilterRatio)
    E_fp_Log2 := (BigNumberGetBitsCount(QMax) + 1) - BigNumberGetBitsCount(M_E) +
                 Ln(AtkinFilterRatio) / Ln(2.0);
    SkipVerify := (E_fp_Log2 < -2.0);  // E_fp < 0.25，非常安全
    {$IFDEF SEA_TRACE}
    _SeaT('[Combine] E_fp_log2=%.1f SkipVerify=%d AtkinRatio=%.6e',
      [E_fp_Log2, Ord(SkipVerify), AtkinFilterRatio]);
    {$ENDIF}

    // 用 BigNumber 比较决定暴力 vs BSGS。
    // 避免 M_E 相对 QMax 较小时 Int64 溢出。
    BigNumberSetWord(QTmp, CN_SEA_BSGS_THRESHOLD);
    if BigNumberCompare(Tmp, QTmp) > 0 then
      UseBSGS := True
    else
    begin
      N := Integer(BigNumberGetInt64(Tmp)) + 1;
      UseBSGS := False;
    end;

    {$IFDEF SEA_TRACE}
    if UseBSGS then
      _SeaT('[Combine] BSGS mode, N~%s', ['TOO_LARGE'])
    else
      _SeaT('[Combine] BruteForce mode, N=%d', [N]);
    {$ENDIF}

    if not UseBSGS then
    begin
      // ---- Brute force search: t = t_E + k*M_E for k = -N..N ----
      Found := False;
      for K := -N to N do
      begin
        BigNumberSetInt64(Tmp, K);
        BigNumberMul(Tmp, Tmp, M_E);
        BigNumberAdd(T, T_E, Tmp);

        // 检查 Hasse 界
        BigNumberCopy(QTmp, T);
        if QTmp.IsNegative then QTmp.Negate;
        if BigNumberCompare(QTmp, QMax) > 0 then Continue;

        // 检查所有 Atkin 约束条件
        Found := True;
        for I := 0 to AtkinInfos.Count - 1 do
        begin
          LVal := TCnSeaAtkinInfo(AtkinInfos[I]).L;
          TMod := BigNumberModWord(T, LVal);
          if T.IsNegative then
            TMod := (LVal - TMod) mod LVal;
          if TCnSeaAtkinInfo(AtkinInfos[I]).PossibleTraces.IndexOf(TMod) < 0 then
          begin
            Found := False;
            Break;
          end;
        end;

        if Found then
        begin
          if SkipVerify then
          begin
            // Atkin 过滤足够强：这是唯一答案
            BigNumberCopy(Res, T);
            Result := True;
            Exit;
          end;
          Verified := SeaVerifyTrace(A, B, P, T);
          if Verified then
          begin
            BigNumberCopy(Res, T);
            Result := True;
            Exit;
          end;
        end;
      end;
    end
    else
    begin
      // ---- BSGS (Baby-Step Giant-Step) search ----
      //
      // Goal: find k such that t = t_E + k*M_E satisfies:
      //   (a) |t| <= QMax  (Hasse bound)
      //   (b) t mod L_i in PossibleTraces[i] for each Atkin prime L_i
      //   (c) [p+1-t]P = O (point verification)
      //
      // 选取选择率最高的 Atkin 素数 L_1（最小 |S_1|/L_1）。
      // 对 S_1 中的每个候选 s，约束 t ≡ s (mod L_1) 给出：
      //   k = (s - t_E) * M_E^{-1}  (mod L_1)
      // 令 r_s 为此值取在 [0, L_1) 中。则 k = j*L_1 + r_s，j 为整数。
      //
      // 小步：预计算 S_1 中所有 s 对应的 {r_s}（最多 |S_1| 个值，< L_1）。
      // 大步：GStep = L_1 * M_E。迭代 j = 0, ±1, ±2, ...
      //   base_t(j) = t_E + j * GStep
      //   t = base_t + r_s * M_E，对每个 r_s
      //   检查 Hasse 界 + 剩余 Atkin 约束 + 验证。
      //
      // 复杂度：O(|S_1| * 2*QMax / (L_1*M_E)) = O((|S_1|/L_1) * N)
      // 相对暴力的加速比：factor L_1 / |S_1|。

      // ---- 自适应多素数 CRT BSGS ----
      // Greedy: select primes with smallest |S|/L ratio.
      // Stop when baby steps > 5000 or L_prod > 2^60.
      SelCount := 0; CRT_LProd := 1;
      for SelBestIdx := 0 to 5 do
      begin
        BestIdx := -1; SelBestR := 1e99;
        for K := 0 to AtkinInfos.Count - 1 do
        begin
          SelFound := False;
          for SelJ := 0 to SelCount - 1 do
            if K = SelIdx[SelJ] then begin SelFound := True; Break; end;
          if SelFound then Continue;
          SelR := TCnSeaAtkinInfo(AtkinInfos[K]).PossibleTraces.Count /
                  TCnSeaAtkinInfo(AtkinInfos[K]).L;
          if SelR < SelBestR then begin SelBestR := SelR; BestIdx := K; end;
        end;
        if BestIdx < 0 then Break;

        // 在提交前先检查小步上限
        SelC[SelCount] := TCnSeaAtkinInfo(AtkinInfos[BestIdx]).PossibleTraces.Count;
        SelPrev := 1; for SelJ := 0 to SelCount do SelPrev := SelPrev * SelC[SelJ];
        if (SelCount > 0) and (SelPrev > CN_SEA_MAX_BABY_STEPS) then Break;
        // Int64 safety: L_prod < 2^60
        if (SelCount > 0) and (CRT_LProd > CN_SEA_MAX_CRT_L_PRODUCT div TCnSeaAtkinInfo(AtkinInfos[BestIdx]).L) then Break;
        // 确认提交
        SelIdx[SelCount] := BestIdx;
        SelL[SelCount] := TCnSeaAtkinInfo(AtkinInfos[BestIdx]).L;
        CRT_LProd := CRT_LProd * SelL[SelCount];
        Inc(SelCount);
      end;
      if SelCount = 0 then Exit;
      L1Val := SelL[0];
      BestIdx := SelIdx[0];

      // ---- Build CRT baby-step table ----
      BabyRs := TCnInt64List.Create;
      // Step 1: r mod L1
      QTmp.SetInt64(L1Val);
      BigNumberModularInverse(Tmp, M_E, QTmp);
      InvME64 := BigNumberModWord(Tmp, L1Val);
      TMod := BigNumberModWord(T_E, L1Val);
      if T_E.IsNegative then TMod := (L1Val - TMod) mod L1Val;
      S1 := TCnSeaAtkinInfo(AtkinInfos[BestIdx]).PossibleTraces;
      for I := 0 to S1.Count - 1 do
        BabyRs.Add( ((S1[I] - TMod + L1Val) mod L1Val * InvME64) mod L1Val );
      CRT_LProd := L1Val;

      // 步骤2..N：迭代 CRT 折叠
      for SelK := 1 to SelCount - 1 do
      begin
        CRT_Lk := SelL[SelK];
        QTmp.SetInt64(CRT_Lk);
        BigNumberModularInverse(Tmp, M_E, QTmp);
        CRT_InvMk := BigNumberModWord(Tmp, CRT_Lk);
        CRT_TModK := BigNumberModWord(T_E, CRT_Lk);
        if T_E.IsNegative then CRT_TModK := (CRT_Lk - CRT_TModK) mod CRT_Lk;

        S2 := TCnSeaAtkinInfo(AtkinInfos[SelIdx[SelK]]).PossibleTraces;
        CRTOldCnt := BabyRs.Count;
        for I := 0 to S2.Count - 1 do
        begin
          CRT_R2 := ((S2[I] - CRT_TModK + CRT_Lk) mod CRT_Lk * CRT_InvMk) mod CRT_Lk;
          for SelJ := 0 to CRTOldCnt - 1 do
          begin
            CRT_R1 := BabyRs[SelJ];
            CRT_Diff := (CRT_R2 - CRT_R1) mod CRT_Lk;
            if CRT_Diff < 0 then CRT_Diff := CRT_Diff + CRT_Lk;
            CRT_Diff := Int64NonNegativeMulMod(CRT_Diff,
              CnInt64ModularInverse2(CRT_LProd mod CRT_Lk, CRT_Lk), CRT_Lk);
            BabyRs.Add(CRT_R1 + CRT_Diff * CRT_LProd);
          end;
        end;
        for I := 0 to CRTOldCnt - 1 do BabyRs.Delete(0);
        CRT_LProd := CRT_LProd * CRT_Lk;
      end;

      // 大步 = CRT_LProd * M_E
      BigNumberSetInt64(Tmp, CRT_LProd);
      BigNumberMul(GStep, Tmp, M_E);

      // 阈值 = QMax + (CRT_LProd - 1) * M_E
      BigNumberSetInt64(Tmp, CRT_LProd - 1);
      BigNumberMul(Threshold, Tmp, M_E);
      BigNumberAdd(Threshold, Threshold, QMax);

      // ---- Int64 precompute: remaining Atkin constraints ----
      // Build O(1) lookup table per prime for fast membership check
      RmCount := 0;
      for I := 0 to AtkinInfos.Count - 1 do
      begin
        RmSkp := False;
        for SelJ := 0 to SelCount - 1 do
          if I = SelIdx[SelJ] then begin RmSkp := True; Break; end;
        if not RmSkp then Inc(RmCount);
      end;
      SetLength(RmL, RmCount); SetLength(RmTE, RmCount);
      SetLength(RmGS, RmCount); SetLength(RmBaby0, RmCount);
      SetLength(RmBase, RmCount);
      // 第一遍：填充 RmL 和候选列表
      RmIdx := 0;
      for I := 0 to AtkinInfos.Count - 1 do
      begin
        RmSkp := False;
        for SelJ := 0 to SelCount - 1 do
          if I = SelIdx[SelJ] then begin RmSkp := True; Break; end;
        if RmSkp then Continue;
        RmL[RmIdx] := TCnSeaAtkinInfo(AtkinInfos[I]).L;
        RmTModL := BigNumberModWord(T_E, RmL[RmIdx]);
        if T_E.IsNegative then RmTModL := (RmL[RmIdx] - RmTModL) mod RmL[RmIdx];
        RmTE[RmIdx] := RmTModL;
        RmGS[RmIdx] := BigNumberModWord(GStep, RmL[RmIdx]);
        RmBaby0[RmIdx] := BigNumberModWord(M_E, RmL[RmIdx]);
        Inc(RmIdx);
      end;
      // 从已知 L 值计算一维查找表偏移
      SetLength(RmLUTOff, RmCount + 1);
      RmLUTOff[0] := 0;
      for K := 0 to RmCount - 1 do
        RmLUTOff[K + 1] := RmLUTOff[K] + Integer(RmL[K]) + 1;
      SetLength(RmLUT, RmLUTOff[RmCount]);
      for K := 0 to High(RmLUT) do RmLUT[K] := False;
      // 用候选值填充查找表 LUT
      RmIdx := 0;
      for I := 0 to AtkinInfos.Count - 1 do
      begin
        RmSkp := False;
        for SelJ := 0 to SelCount - 1 do
          if I = SelIdx[SelJ] then begin RmSkp := True; Break; end;
        if RmSkp then Continue;
        S1 := TCnSeaAtkinInfo(AtkinInfos[I]).PossibleTraces;
        SelPrev := RmLUTOff[RmIdx]; // base offset for this row
        for SelJ := 0 to S1.Count - 1 do
          RmLUT[SelPrev + S1[SelJ]] := True;
        Inc(RmIdx);
      end;
      // 预计算 (BabyRs[i] * RmBaby0[k]) mod RmL[k] → 展开为一维数组
      BabyCnt := BabyRs.Count;
      SetLength(BabyMod1D, RmCount * BabyCnt);
      for K := 0 to RmCount - 1 do
      begin
        SelPrev := K * BabyCnt; // row offset
        for I := 0 to BabyCnt - 1 do
          BabyMod1D[SelPrev + I] := (BabyRs[I] * RmBaby0[K]) mod RmL[K];
      end;

      // ---- Int64-optimized BSGS loop ----
      // 预计算 MaxGiantSize = Threshold / GStep（Int64 安全的比较）
      BigNumberDiv(QTmp, nil, Threshold, GStep);
      GiantMax := BigNumberGetInt64(QTmp);
      {$IFDEF SEA_TRACE}
      _SeaT('[Combine] CRT %d primes, %d baby, %d remAtk, ~%d giant steps',
        [SelCount, BabyRs.Count, RmCount, GiantMax]);
      {$ENDIF}
      Found := False; GiantSize := 0;
      SetLength(Survive, BabyCnt);  // 循环外部一次性预分配
      {$IFDEF SEA_TRACE}
      _SeaT('[Combine] BSGS loop start', []);
      {$ENDIF}
      while True do
      begin
        {$IFDEF SEA_TRACE}
        if GiantSize = 0 then _SeaT('[Combine] step0 dir0 start', []);
        {$ENDIF}
        for Dir := 0 to 1 do
        begin
          if (Dir = 1) and (GiantSize = 0) then Continue;
          // base_t mod each L (Int64) into RmTE as scratch
          {$IFDEF SEA_TRACE}
          if GiantSize = 0 then _SeaT('[Combine] baset start', []);
          {$ENDIF}
          for I := 0 to RmCount - 1 do
          begin
            RmLk := RmL[I];
            RmBm := ((GiantSize mod RmLk) * RmGS[I]) mod RmLk;
            if Dir = 0 then RmBm := (RmTE[I] + RmBm) mod RmLk
            else begin
              RmBm := (RmTE[I] - RmBm) mod RmLk;
              if RmBm < 0 then RmBm := RmBm + RmLk;
            end;
            RmBase[I] := RmBm;
          end;
          {$IFDEF SEA_TRACE}
          if GiantSize = 0 then _SeaT('[Combine] baset done, baby loop start count=%d', [BabyRs.Count]);
          {$ENDIF}

          // 过滤小步：对每个剩余 Atkin 素数，淘汰不满足其约束的小步。
          // 只有通过所有素数的小步才是候选，进入完整验证。
          // Survive 数组在循环外预分配以获得最佳性能。
          for I := 0 to BabyCnt - 1 do Survive[I] := True;

          for K := 0 to RmCount - 1 do
          begin
            RmLk := RmL[K];
            SelPrev := K * BabyCnt; // row start in BabyMod1D
            CRT_TModK := RmBase[K];  // base mod this prime
            RmTModL := RmLUTOff[K];
            for I := 0 to BabyCnt - 1 do
            begin
              if not Survive[I] then Continue;
              RmBm := (CRT_TModK + BabyMod1D[SelPrev + I]) mod RmLk;
              if not RmLUT[RmTModL + RmBm] then
                Survive[I] := False;
            end;
          end;

          // Count survivors for tracing
          {$IFDEF SEA_TRACE}
          if (GiantSize > 0) and (GiantSize mod 10 = 0) then
          begin
            SurvCnt := 0;
            for I := 0 to BabyCnt - 1 do
              if Survive[I] then Inc(SurvCnt);
            _SeaT('[Combine] step %d/%d dir%d  survivors=%d', [GiantSize, GiantMax, Dir, SurvCnt]);
          end;
          {$ENDIF}

          // Check any surviving baby steps (extremely rare)
          for I := 0 to BabyCnt - 1 do
          begin
            if not Survive[I] then
              Continue;
            BigNumberCopy(BaseT, T_E);
            BigNumberSetInt64(Tmp, GiantSize);
            BigNumberMul(Tmp, Tmp, GStep);
            if Dir = 0 then
              BigNumberAdd(BaseT, BaseT, Tmp)
            else
              BigNumberSub(BaseT, BaseT, Tmp);

            BigNumberSetInt64(Tmp, BabyRs[I]);
            BigNumberMul(Tmp, Tmp, M_E);
            BigNumberAdd(T, BaseT, Tmp);
            BigNumberCopy(QTmp, T);
            if QTmp.IsNegative then
              QTmp.Negate;

            if BigNumberCompare(QTmp, QMax) <= 0 then
            begin
              if SkipVerify then
              begin
                // Atkin 过滤足够强：这是唯一答案
                BigNumberCopy(Res, T);
                Result := True;
                Exit;
              end;
              {$IFDEF SEA_TRACE}
              // _SeaT('[Combine] step %d verifying candidate...', [GiantSize]);
              {$ENDIF}
              if SeaVerifyTrace(A, B, P, T) then
              begin
                BigNumberCopy(Res, T);
              {$IFDEF SEA_TRACE}
              _SeaT('[Combine] step %d candidate OK.', [GiantSize]);
              {$ENDIF}
                Result := True;
                Exit;
              end;

            end;
          end;
          {$IFDEF SEA_TRACE}
          if GiantSize = 0 then
            _SeaT('[Combine] baby loop done', []);
          {$ENDIF}
        end;
        {$IFDEF SEA_TRACE}
        if GiantSize = 0 then
          _SeaT('[Combine] step0 done, now loop', []);
        {$ENDIF}
        if GiantSize >= GiantMax then
          Break;
        Inc(GiantSize);
      end;
    end;
  finally
    BabyRs.Free;
    FSeaBigNumberPool.Recycle(Threshold);
    FSeaBigNumberPool.Recycle(BaseT);
    FSeaBigNumberPool.Recycle(GStep);
    FSeaBigNumberPool.Recycle(T);
    FSeaBigNumberPool.Recycle(Tmp);
    FSeaBigNumberPool.Recycle(QTmp);
    FSeaBigNumberPool.Recycle(QMax);
    FSeaBigNumberPool.Recycle(T_E);
    FSeaBigNumberPool.Recycle(M_E);
  end;
end;

initialization
  FSeaBigNumberPool := TCnBigNumberPool.Create;
  FSeaPolynomialPool := TCnBigNumberPolynomialPool.Create;
  FSeaRationalPolynomialPool := TCnBigNumberRationalPolynomialPool.Create;

finalization
  FSeaRationalPolynomialPool.Free;
  FSeaPolynomialPool.Free;
  FSeaBigNumberPool.Free;

end.
