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
* 备    注：
* 开发平台：PWin10 + Delphi 10.3
* 兼容测试：PWin9X/2000/XP/7/10/11 + Delphi/C++Builder 5 ~ 13/FPC
* 本 地 化：该单元中的字符串均符合标准
* 修改记录：2026.07.10 V1.2
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
{* Compute the set of possible t mod L values for an Atkin prime L.}

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
{*  Compute classical modular polynomial Phi_L(X, Y) modulo a small prime.
   Uses raw Int64 arrays for maximum speed. Prime must be < 2^31.}

function CnGenerateClassicalModularPolynomialCRT(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
{*  CRT (Chinese Remainder Theorem) method to compute classical modular polynomial Phi_L(X, Y).
   Uses multiple small primes and Int64 arithmetic for ~1000x speedup over BigNumber method.
   Automatically used by CnGenerateClassicalModularPolynomial for L >= 11.

   Parameters:
     Res: TCnBigNumberBiPolynomial        - Result bivariate polynomial
     L: Integer                           - Modular polynomial order

   Returns: Boolean                        - Whether computation succeeded
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

const
  CN_SEA_CRT_THRESHOLD = 11;
  {* 模多项式计算时，当 L >= CN_SEA_CRT_THRESHOLD 时自动切换到 CRT 方法。
     CRT 方法使用多个小素数模运算和 Int64 快速运算，比直接 BigNumber 方法快约 10~20 倍。
     L < CN_SEA_CRT_THRESHOLD 时使用 BigNumber 方法（系数较小，直接计算更快且无需 CRT 重建）。}

  CN_SEA_BSGS_THRESHOLD = 100000;
  {* 当 Elkies-Atkin 组合搜索空间 N = floor(2*QMax/M_E) 超过此阈值时，
     从暴力搜索切换到 BSGS（Baby-Step Giant-Step）算法。
     取值依据：每次迭代约 25μs（256 位 BigNumber 运算），100000 次约 2.5 秒，
     在可接受范围内。超过此值则 BSGS 的 L_1/|S_1| 倍加速（通常 2~10x）更划算。
     - 调大：更多用暴力搜索（简单但慢），适合小素数场景
     - 调小：更早启用 BSGS（快但需 Atkin 素数），适合大素数场景}

  CN_SEA_ELKIES_BSGS_THRESHOLD = 71;
  {* 素数大于该值时采用 BSGS 查找，否则线性。
     BSGS 算法虽然能将迭代次数从 (L+1)/2 降低到大约 2*sqrt((L+1)/2)，
     但它的每次迭代都有额外的开销（比如需要通过多项式模逆来构建 baby step 表）。
     因此，对于较小的 L（<71），线性搜索反而更快。具体来看：当 L=71 时，
     线性搜索需要 36 次，而 BSGS 需要 12 次；当 L=1009 时，线性搜索需要 505 次，而 BSGS 只需要 46 次。}

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
        for i := 2 to K do
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
// Optimized implementation using raw Int64 arrays and fast inline modular
// arithmetic.
//
// On 64-bit CPU: primes < 2^30, Int64 multiplication is native (1 instruction).
// On 32-bit CPU: primes < 2^15, Cardinal multiplication is native (1 instruction),
//   avoiding slow software-emulated Int64 multiply/divide.

{$IFDEF CPU64BITS}
// 64-bit: primes < 2^30, A*B < 2^60 fits in Int64, native multiply
function SeaFastMulMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A * B) mod P;
end;

// Fast modular add for primes < 2^31
function SeaFastAddMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := A + B;
  if Result >= P then Result := Result - P;
end;

// Fast modular sub for primes < 2^31
function SeaFastSubMod(A, B, P: Int64): Int64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := A - B;
  if Result < 0 then Result := Result + P;
end;

const
  // Largest prime < 2^30, used as starting point for prime search
  SeaCRTPrimeStart: Int64 = 1073741789;
  SeaCRTBitsPerPrime = 30;

{$ELSE}
// 32-bit: primes < 2^15, A*B < 2^30 fits in Cardinal, native 32-bit multiply
// This avoids slow software-emulated Int64 multiply/divide on 32-bit CPUs.
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
  // Largest prime < 2^15, used as starting point for prime search
  SeaCRTPrimeStart: Int64 = 32749;
  SeaCRTBitsPerPrime = 15;

{$ENDIF}

// Compute the number of bits needed for CRT reconstruction of Phi_L.
// Theoretical bound: H(Phi_L) ~ exp(6*L*ln(L)), in bits: 6*L*log2(L).
// Empirical fit: actual bits ≈ 6*L*log2(L) + 17*L (the O(L) term is significant).
// Formula 6*L*log2(L) + 20*L + 50 provides safe margin for all practical L.
//   L=5: 219b (actual 157b), L=11: 498b (actual 421b),
//   L=23: 1134b (actual 1018b), L=29: 1475b (actual 1338b),
//   L=53: 2931b (actual ~2722b), L=97: 5831b
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

// Compute sigma_k(N) mod Prime using sieve-like batch approach
// Pre-computes sigma for all 1..MaxN at once: O(MaxN * log(MaxN))
procedure SeaBatchSigmaModP(var Sigmas: array of Int64; MaxN, K: Integer; Prime: Int64);
var
  D, M: Integer;
  PowD, Acc: Int64;
begin
  for D := 0 to MaxN do
    Sigmas[D] := 0;
  for D := 1 to MaxN do
  begin
    // PowD = D^K mod Prime
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
    // Add PowD to all multiples of D
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

// Compute J(q) mod Prime into a raw array J[0..MaxDegree]
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

  // Batch compute sigma_3 and sigma_5
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

  // E4^3 truncated to MaxDegree+1 (schoolbook)
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
  // Multiply by E4 again for E4^3
  // Actually E4_3 currently = E4^2, need one more multiply
  // Let's use a temp
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

  // E6^2 truncated
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

  // Delta = (E4^3 - E6^2) * 1728^(-1)
  Inv1728 := CnInt64ModularInverse2(Int64NonNegativeMod(1728, Prime), Prime);
  for I := 0 to MaxDegree + 1 do
    Delta[I] := SeaFastMulMod(SeaFastSubMod(E4_3[I], E6_2[I], Prime), Inv1728, Prime);

  // Delta / q = shift right by 1
  for I := 0 to MaxDegree do
    Delta[I] := Delta[I + 1];
  Delta[MaxDegree + 1] := 0;

  // DeltaInv = (Delta/q)^(-1) mod x^(MaxDegree+1) via Newton's iteration
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

  // J(q) = E4^3 * DeltaInv truncated to MaxDegree
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

// Compute classical modular polynomial Phi_L mod Prime using raw Int64 arrays
// All polynomial operations done with plain arrays for maximum speed.
function CnGenerateClassicalModularPolynomialModP(Res: TCnInt64BiPolynomial;
  L: Integer; Prime: Int64): Boolean;
var
  N, I, K, D, M, U, V, MaxY, J2: Integer;
  J_q: array of Int64;
  H: array of array of Int64;  // H[k][i] = coefficient of q^i in J(q)^k
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

  // Compute J(q) mod Prime
  SetLength(J_q, N + 1);
  SeaCalcJRaw(J_q, N, Prime);

  // Compute H[k] = J(q)^k for k = 0..N using truncated multiplication
  // H[k][i] for i = 0..N
  SetLength(H, N + 1);
  for K := 0 to N do
  begin
    SetLength(H[K], N + 1);
    for I := 0 to N do
      H[K][I] := 0;
  end;

  // H[0] = 1
  H[0][0] := 1;

  // H[k] = H[k-1] * J(q) truncated to degree N
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
    // Copy to H[K]
    for I := 0 to N do
      H[K][I] := HTemp[I];
  end;

  // Compute Pm_Poly[M] for M = 1..L+1
  SetLength(PmDeg, L + 2);
  SetLength(SmDeg, L + 2);
  SetLength(Sm_Poly, L + 2);
  SetLength(PmArr, 0); // will resize per M

  // First compute all Pm_Poly and store as rows of H-like arrays
  // Pm_Poly[M] has degree M*L
  // We'll store Pm_Poly in Sm_Poly temporarily... no, let's use a separate array
  // Actually, let's compute Pm_Poly[M] on the fly during Sm_Poly computation
  // to save memory. But the Sm_Poly recurrence needs Pm_Poly[1..K], so we need
  // to store them all.

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

  // Compute Sm_Poly using Newton's recurrence
  // Sm_Poly[K] = (1/K) * sum_{I=1}^{K} (-1)^(I-1) * Sm_Poly[K-I] * Pm_Poly[I]
  SetLength(Sm_Poly, L + 2);
  SmDeg[0] := 0;
  SetLength(Sm_Poly[0], 1);
  Sm_Poly[0][0] := 1;

  for K := 1 to L + 1 do
  begin
    // Determine max degree of Sm_Poly[K]
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

    // Divide by K: multiply by K^(-1) mod Prime
    T64 := CnInt64ModularInverse2(Int64NonNegativeMod(K, Prime), Prime);
    for I := 0 to SmDeg[K] do
      Sm_Poly[K][I] := SeaFastMulMod(Sm_Poly[K][I], T64, Prime);
  end;

  // Assemble result into TCnInt64BiPolynomial
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

// Find next prime below a given number (for CRT prime selection)
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

// Compute classical modular polynomial using CRT method
function CnGenerateClassicalModularPolynomialCRT(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
var
  // Small L: delegate to direct method
  I, K, MaxX, MaxY: Integer;
  Prime: Int64;
  BitsNeeded, BitsCovered: Integer;
  Candidate: Int64;
  Primes: TCnInt64List;
  ModResults: TObjectList;  // owns TCnInt64BiPolynomial objects
  ModRes: TCnInt64BiPolynomial;
  // CRT state per coefficient
  V, M, Tmp, Tmp2: TCnBigNumber;
  VModP, Diff, InvM, T, T1: Int64;
  U, VIdx: Integer;
begin
  Result := False;
  if (Res = nil) or (L < 1) then Exit;

  // For small L, use direct BigNumber method
  if L < CN_SEA_CRT_THRESHOLD then
  begin
    Result := CnGenerateClassicalModularPolynomial(Res, L);
    Exit;
  end;

  if (L > 1) and not CnUInt32IsPrime(L) then Exit;

  // L=1 special case
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
    // Height bound: theoretical formula 6*L*log2(L) + 20*L + 50
    // See SeaCalcBitsNeeded for details and verification data.
    BitsNeeded := SeaCalcBitsNeeded(L);
    Candidate := SeaCRTPrimeStart;
    BitsCovered := 0;

    while BitsCovered < BitsNeeded do
    begin
      Prime := SeaFindNextPrimeBelow(Candidate);
      if Prime = 0 then
        raise Exception.Create('Cannot find enough primes for CRT');

      // Skip primes that divide 1728 (= 2^6 * 3^3) or are <= L+1
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

    // Compute Phi_L mod p_i for each prime
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

    // Determine the dimensions of the result
    MaxX := L + 1;
    MaxY := 0;
    for I := 0 to ModResults.Count - 1 do
    begin
      ModRes := TCnInt64BiPolynomial(ModResults[I]);
      if ModRes.MaxYDegree > MaxY then
        MaxY := ModRes.MaxYDegree;
    end;

    // Initialize result
    Res.SetZero;
    Res.MaxXDegree := MaxX;
    Res.MaxYDegree := MaxY;

    // CRT reconstruction for each coefficient (U, V)
    for U := 0 to MaxX do
    begin
      for VIdx := 0 to MaxY do
      begin
        // Incremental CRT:
        // V = v_0 mod p_0, M = p_0
        // For each subsequent prime p_k:
        //   t = (v_k - V mod p_k) * M^(-1) mod p_k
        //   V = V + M * t
        //   M = M * p_k

        V.SetZero;
        M.SetWord(Cardinal(Primes[0]));

        // Get v_0 = ModResults[0].SafeValue[U, VIdx]
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

        // Store the result
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
  // For L >= CN_SEA_CRT_THRESHOLD, use CRT method for dramatic speedup (~1000x)
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
    // Use BigNumberPolynomialMulTrunc instead of manual triple loop for efficiency
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

      // Extract m and n from [m,n]
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
      // Also fill symmetric [N, M] unless it's the diagonal
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

    // 计算 x^p mod Psi_L
    XPowP := FSeaPolynomialPool.Obtain;
    XPowP.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(XPowP, XPowP, P, P, PsiL);

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

    // 对每个 lambda = 1, ..., L-1，检查 gcd(Psi_L, G_lambda) 的次数
    for Lambda := 1 to L - 1 do
    begin
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
      // When G is the zero polynomial (e.g. scalar Frobenius with lambda=1
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
        // with eigenvalue Lambda. Record it for direct trace computation.
        if (H.MaxDegree = PsiL.MaxDegree) and (ScalarLam = 0) then
          ScalarLam := Lambda;
      end;
    end;

    if not Found then
    begin
      // If scalar Frobenius was detected (GCD = full Psi_L for some lambda),
      // determine the actual eigenvalue by checking the y-coordinate action.
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
          BigNumberPolynomialGaloisPower(T2, Y2, BQ, P, PsiL);
          if T2.IsOne then
            ScalarLam := 1
          else
            ScalarLam := L - 1;
        finally
          FSeaBigNumberPool.Recycle(BQ);
        end;
        if ScalarLambda <> nil then
          ScalarLambda^ := ScalarLam;
        // Return Psi_L as the kernel polynomial (full degree).
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
// All operations in quotient ring F_p[x]/(h(x)) via polynomial modular inverse.
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
  // Frobenius image and point coords as pure polynomials (not rational)
  PiPX, PiPY: TCnBigNumberPolynomial;
  PX, PY: TCnBigNumberPolynomial;
  // Linear search iteration variables
  RSX, RSY, TSX, TSY: TCnBigNumberPolynomial;
  // BSGS variables
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
    // Step 1: compute kernel polynomial h(x), degree (L-1)/2
    H := FSeaPolynomialPool.Obtain;
    ScalarLam := 0;
    if not CnSeaElkiesKernelPolynomial(H, L, A, B, P, DPs, @ScalarLam) then Exit;

    // Scalar Frobenius: eigenvalue is known, compute trace directly
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

    // Step 2: curve polynomial Y2 = x^3 + Ax + B
    Y2 := FSeaPolynomialPool.Obtain;
    Y2.SetCoefficients([B, A, 0, 1]);

    BQ := FSeaBigNumberPool.Obtain;

    // Step 2b: precompute InvY2 = Y2^{-1} mod h (for doubling)
    InvY2 := FSeaPolynomialPool.Obtain;
    BigNumberPolynomialGaloisModularInverse(InvY2, Y2, H, P);

    // Step 3: compute Frobenius image pi(P) as pure polynomials
    // pi(P) x-component = x^p mod h(x)
    // pi(P) y-coefficient = Y2^((p-1)/2) mod h(x)
    PiPX := FSeaPolynomialPool.Obtain;
    PiPX.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(PiPX, PiPX, P, P, H); // x^p mod h

    PiPY := FSeaPolynomialPool.Obtain;
    BigNumberCopy(BQ, P);
    BQ.SubWord(1);
    BigNumberShiftRightOne(BQ, BQ); // (p-1)/2
    BigNumberPolynomialGaloisPower(PiPY, Y2, BQ, P, H); // Y2^((p-1)/2) mod h

    // Step 4: generic point P = (x, 1) in polynomial representation
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

    // Steps 5-6: search for eigenvalue lambda such that [lambda]P = pi(P)
    Found := False;
    Lambda := 0;

    UseBSGS := (L >= CN_SEA_ELKIES_BSGS_THRESHOLD);

    if UseBSGS then
    begin
      // ===== BSGS Search =====
      // lambda = a + b*m, a in {1..m}, b in {0..m}
      // Baby: [1]P, [2]P, ..., [m]P
      // Giant: G = pi(P) - [b*m]P, look for G = [a]P

      M := 1;
      while M * M < ((L + 1) div 2) do
        Inc(M);

      BabyLen := M;
      SetLength(BabyX, BabyLen);
      SetLength(BabyY, BabyLen);

      // Baby step 0: [1]P = (x, 1)
      BabyX[0] := FSeaPolynomialPool.Obtain;
      BabyY[0] := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialCopy(BabyX[0], PX);
      BigNumberPolynomialCopy(BabyY[0], PY);

      // Baby step 1: [2]P (doubling of [1]P)
      if BabyLen >= 2 then
      begin
        BabyX[1] := FSeaPolynomialPool.Obtain;
        BabyY[1] := FSeaPolynomialPool.Obtain;
        SeaPolyPointAdd(BabyX[1], BabyY[1], BabyX[0], BabyY[0],
          BabyX[0], BabyY[0], Y2, InvY2, H, A, P);
      end;

      // Baby steps 2..m-1: [a+1]P = [a]P + P
      for BabyIdx := 2 to BabyLen - 1 do
      begin
        BabyX[BabyIdx] := FSeaPolynomialPool.Obtain;
        BabyY[BabyIdx] := FSeaPolynomialPool.Obtain;
        SeaPolyPointAdd(BabyX[BabyIdx], BabyY[BabyIdx], BabyX[BabyIdx - 1], BabyY[BabyIdx - 1],
          PX, PY, Y2, InvY2, H, A, P);
      end;

      // [m]P is in BabyX[m-1], BabyY[m-1]
      // Compute -[m]P = (MX, -MY)
      NegMX := FSeaPolynomialPool.Obtain;
      NegMY := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialCopy(NegMX, BabyX[BabyLen - 1]);
      BigNumberPolynomialCopy(NegMY, BabyY[BabyLen - 1]);
      BigNumberPolynomialGaloisNegate(NegMY, P);

      // Giant steps: G = pi(P)
      GX := FSeaPolynomialPool.Obtain;
      GY := FSeaPolynomialPool.Obtain;
      BigNumberPolynomialCopy(GX, PiPX);
      BigNumberPolynomialCopy(GY, PiPY);

      GiantMax := M + 1;
      for GiantIdx := 0 to GiantMax do
      begin
        // Check if G is identity (lambda = b*m)
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
          // Search baby table for X match
          for BabyIdx := 0 to BabyLen - 1 do
          begin
            if BigNumberPolynomialGaloisEqual(GX, BabyX[BabyIdx], P) then
            begin
              // X matches, check Y for sign
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
      // ===== Linear Search (polynomial-only) =====
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
    // Recycle baby step table
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

// Verify candidate trace t by checking [p+1-t]P = O for a point P on E/F_p
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

    // Try multiple points to avoid false positives from small-order points.
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

      // Check QR via Euler's criterion
      BigNumberCopy(T1, P);
      T1.SubWord(1);
      BigNumberShiftRightOne(T1, T1);
      BigNumberPowerMod(T2, Y2, T1, P);
      if not T2.IsOne then
        Continue; // Not a QR, no point with this x

      if not BigNumberTonelliShanks(Y, Y2, P) then
        Continue;

      BigNumberCopy(SX, X);
      BigNumberCopy(SY, Y);
      Inc(PointCount);

      // Compute [N]P using double-and-add (MSB to LSB)
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

      // If [N]P != O for any point, reject this trace
      if not Inf then
        Exit;
    end;

    // All points verified: [N]P = O for all tested points
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
  I, J, ModPolyIdx, RequiredModPolyCount: Integer;
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

    // 计算所需素数列表：乘积 > 4 * sqrt(p)
    if not BigNumberSqrt(QMax, P) then Exit;
    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);
    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    while (BigNumberCompare(QMul, QMax) <= 0) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      L := CN_PRIME_NUMBERS_SQRT_UINT32[I];
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

    if I > High(CN_PRIME_NUMBERS_SQRT_UINT32) then
      raise ECnEccException.Create('Prime number is too large for SEA.');

    // Validate external ModPolys: must have one entry per prime L >= 3 in Pa
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
      end;
    end;

    // 预生成除法多项式（供 Schoof 回退使用）
    if Pa.Count > 0 then
    begin
      DPs := TObjectList.Create(True);
      CnGenerateGaloisDivisionPolynomials(A, B, P, Pa[Pa.Count - 1] + 2, DPs);
    end;

    // 对每个素数 L >= 3 计算迹
    ModPolyIdx := 0;
    for I := 0 to Pa.Count - 1 do
    begin
      L := Pa[I];
      if L = 2 then Continue; // 已处理

      // 检查是 Elkies 还是 Atkin
      if (ModPolys <> nil) and (ModPolyIdx < ModPolys.Count) then
        PrimeType := CnSeaCheckPrimeType(L, A, B, P, nil, TCnBigNumberBiPolynomial(ModPolys[ModPolyIdx]))
      else
        PrimeType := CnSeaCheckPrimeType(L, A, B, P);
      Inc(ModPolyIdx);

      if PrimeType = sptElkies then
      begin
        // 尝试 Elkies 方法
        ElkiesOk := CnSeaElkiesTrace(TraceRes, L, A, B, P, DPs);
        if ElkiesOk then
        begin
          Ta[I] := TraceRes.GetInt64;
          ElkiesTraces.Add(Ta[I]);
          ElkiesModuli.Add(L);
        end
        else
        begin
          // Elkies failed (e.g. CM curve where Phi_L has roots in F_p but
          // Frobenius has no eigenvector in E[L]). Fall back to Atkin.
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
          end
          else
          begin
            Ta[I] := -1;
          end;
        end;
      end
      else if PrimeType = sptAtkin then
      begin
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
        end
        else
        begin
          Ta[I] := -1;
        end;
      end
      else
      begin
        Ta[I] := -1;
      end;
    end;

    // 中国剩余定理合并结果
    if AtkinInfos.Count > 0 then
    begin
      if not CnSeaCombineElkiesAtkin(Res, ElkiesTraces, ElkiesModuli, AtkinInfos, A, B, P) then
      begin
        // Atkin matching failed (search space too large).
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
  I: Integer;
  L: Int64;
begin
  Result := 0;
  if (P = nil) or P.IsZero or P.IsNegative then Exit;

  QMax := TCnBigNumber.Create;
  QMul := TCnBigNumber.Create;
  BQ := TCnBigNumber.Create;
  try
    // Hasse bound: need product of all small primes L > 4*sqrt(p)
    if not BigNumberSqrt(QMax, P) then Exit;
    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);

    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    while (BigNumberCompare(QMul, QMax) <= 0) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      L := CN_PRIME_NUMBERS_SQRT_UINT32[I];

      // Skip L = P (same as the field characteristic)
      BigNumberSetWord(BQ, L);
      if BigNumberCompare(BQ, P) <> 0 then
      begin
        BigNumberMulWord(QMul, L);
        Result := L;
      end;

      Inc(I);
    end;

    if I > High(CN_PRIME_NUMBERS_SQRT_UINT32) then
      Result := 0;  // P too large for available prime table
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
    // Q = a^2 for norm-1 element (a, b). Accept Q=0 (a=0) or QR.
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
  Factors: TCnBigNumberPolynomialList;
  I, Deg, R: Integer;
  TotalDeg, Multiplicity: Integer;
  SplitType: Integer;
  L64: Int64;
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
  Factors := TCnBigNumberPolynomialList.Create;
  try
    if not CnSeaJInvariant(J, A, B, P) then Exit;
    if OwnPhiL then
    begin
      if not CnGenerateClassicalModularPolynomial(PhiL, L) then Exit;
    end;
    if not BigNumberBiPolynomialGaloisEvaluateByX(FY, PhiL, J, P) then Exit;
    if FY.MaxDegree <= 0 then Exit;
    if not BigNumberPolynomialGaloisFactorCantorZassenhaus(Factors, FY, P) then Exit;

    // Compute R from factor degrees, accounting for repeated factors.
    // For CM curves (e.g. j=1728), Phi_L(j,Y) can be a perfect square g^2.
    // The square-free factorization returns g, losing the multiplicity.
    // The true R (order of eigenvalue ratio gamma) must be multiplied by
    // the multiplicity to reflect the original polynomial's factor degrees.
    TotalDeg := 0;
    for I := 0 to Factors.Count - 1 do
    begin
      Deg := TCnBigNumberPolynomial(Factors[I]).MaxDegree;
      if Deg > 0 then
        TotalDeg := TotalDeg + Deg;
    end;
    if TotalDeg = 0 then Exit;

    // Multiplicity = input_degree / total_square_free_degree
    // (1 for non-CM curves, 2 for CM curves with j=1728 and L inert in Z[i])
    if (TotalDeg > 0) and (FY.MaxDegree mod TotalDeg = 0) then
      Multiplicity := FY.MaxDegree div TotalDeg
    else
      Multiplicity := 1;

    R := 1;
    for I := 0 to Factors.Count - 1 do
    begin
      Deg := TCnBigNumberPolynomial(Factors[I]).MaxDegree;
      if Deg > 0 then
        R := SeaInt64LCM(R, Deg * Multiplicity);
    end;
    if R < 1 then Exit;

    L64 := L;
    SplitType := 0;
    if (L64 - 1) mod R = 0 then
      SplitType := 1;
    if (L64 + 1) mod R = 0 then
      SplitType := SplitType or 2;
    if SplitType = 0 then Exit;

    // When R divides both L-1 and L+1 (e.g. R=2, L odd), try both
    // split types and merge candidates. The correct eigenvalue ratio
    // gamma may lie in either F_L* or the norm-1 subgroup of F_{L^2}*.
    if (SplitType and 1) <> 0 then
      SeaAtkinTracesRDivLm1(Traces, L64, R, P);
    if (SplitType and 2) <> 0 then
      SeaAtkinTracesRDivLp1(Traces, L64, R, P);

    Result := Traces.Count > 0;
  finally
    Factors.Free;
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
  Found, Verified, UseBSGS: Boolean;
  TMod: Int64;
  LVal, L1Val, LCheck, InvME64: Int64;
  BestIdx, Dir: Integer;
  BabyR: Int64;

  // BSGS variables
  BabyRs: TCnInt64List;       // baby step r values: k mod L_1
  GStep, BaseT, Threshold: TCnBigNumber;
  GiantSize: Int64;
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

    // CRT on Elkies results
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

    // Hasse bound: |t| <= 2*sqrt(p). Use 2*(sqrt(p)+1) for safety.
    BigNumberSqrt(QMax, P);
    QMax.AddWord(1);
    BigNumberMulWord(QMax, 2);

    // Search range: t = t_E + k*M_E must lie in [-QMax, QMax].
    // Since t_E in [0, M_E), k ranges over at most ceil(2*QMax/M_E)+1 values.
    // Total candidates = floor(2*QMax / M_E) + 1
    BigNumberAdd(Tmp, QMax, QMax);         // Tmp = 2*QMax
    BigNumberDiv(Tmp, nil, Tmp, M_E);      // Tmp = floor(2*QMax / M_E)

    // Use BigNumber comparison to decide brute force vs BSGS.
    // Avoids Int64 overflow when M_E is small relative to QMax.
    BigNumberSetWord(QTmp, CN_SEA_BSGS_THRESHOLD);
    if BigNumberCompare(Tmp, QTmp) > 0 then
      UseBSGS := True
    else
    begin
      N := Integer(BigNumberGetInt64(Tmp)) + 1;
      UseBSGS := False;
    end;

    if not UseBSGS then
    begin
      // ---- Brute force search: t = t_E + k*M_E for k = -N..N ----
      Found := False;
      for K := -N to N do
      begin
        BigNumberSetInt64(Tmp, K);
        BigNumberMul(Tmp, Tmp, M_E);
        BigNumberAdd(T, T_E, Tmp);

        // Check Hasse bound
        BigNumberCopy(QTmp, T);
        if QTmp.IsNegative then QTmp.Negate;
        if BigNumberCompare(QTmp, QMax) > 0 then Continue;

        // Check against all Atkin constraints
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
      // Method: pick the most selective Atkin prime L_1 (smallest |S_1|/L_1).
      // For each candidate s in S_1, the constraint t = s (mod L_1) gives:
      //   k = (s - t_E) * M_E^{-1}  (mod L_1)
      // Let r_s = this value in [0, L_1). Then k = j*L_1 + r_s for integer j.
      //
      // Baby step: precompute {r_s} for all s in S_1 (at most |S_1| values, < L_1).
      // Giant step: GStep = L_1 * M_E. Iterate j = 0, +-1, +-2, ...
      //   base_t(j) = t_E + j * GStep
      //   t = base_t + r_s * M_E  for each r_s
      //   Check Hasse bound + remaining Atkin constraints + verify.
      //
      // Complexity: O(|S_1| * 2*QMax / (L_1*M_E)) = O((|S_1|/L_1) * N)
      // Speedup over brute force: factor L_1 / |S_1|.

      if AtkinInfos.Count = 0 then
        Exit; // No Atkin primes to guide BSGS

      // Select most selective Atkin prime
      BestIdx := 0;
      for K := 1 to AtkinInfos.Count - 1 do
      begin
        if TCnSeaAtkinInfo(AtkinInfos[K]).PossibleTraces.Count *
           TCnSeaAtkinInfo(AtkinInfos[BestIdx]).L <
           TCnSeaAtkinInfo(AtkinInfos[BestIdx]).PossibleTraces.Count *
           TCnSeaAtkinInfo(AtkinInfos[K]).L then
          BestIdx := K;
      end;
      L1Val := TCnSeaAtkinInfo(AtkinInfos[BestIdx]).L;

      // Compute M_E^{-1} mod L_1 via BigNumber
      QTmp.SetInt64(L1Val);
      BigNumberModularInverse(Tmp, M_E, QTmp);
      InvME64 := BigNumberModWord(Tmp, L1Val);

      // t_E mod L_1
      TMod := BigNumberModWord(T_E, L1Val);
      if T_E.IsNegative then
        TMod := (L1Val - TMod) mod L1Val;

      // Build baby step table: r_s = (s - t_E_mod) * M_E_inv mod L_1
      BabyRs := TCnInt64List.Create;
      for K := 0 to TCnSeaAtkinInfo(AtkinInfos[BestIdx]).PossibleTraces.Count - 1 do
      begin
        BabyR := (TCnSeaAtkinInfo(AtkinInfos[BestIdx]).PossibleTraces[K] - TMod
          + L1Val) mod L1Val;
        BabyR := (BabyR * InvME64) mod L1Val;
        BabyRs.Add(BabyR);
      end;

      // Giant step = L_1 * M_E
      BigNumberSetInt64(Tmp, L1Val);
      BigNumberMul(GStep, Tmp, M_E);

      // Termination threshold = QMax + (L_1 - 1) * M_E
      // When |j * GStep| exceeds this, no baby step can bring t within Hasse bound.
      BigNumberSetInt64(Tmp, L1Val - 1);
      BigNumberMul(Threshold, Tmp, M_E);
      BigNumberAdd(Threshold, Threshold, QMax);

      // Iterate giant steps: j = 0, +-1, +-2, ...
      Found := False;
      GiantSize := 0;
      while True do
      begin
        for Dir := 0 to 1 do
        begin
          if (Dir = 1) and (GiantSize = 0) then Continue;

          // base_t = t_E + sign * GiantSize * GStep
          BigNumberCopy(BaseT, T_E);
          BigNumberSetInt64(Tmp, GiantSize);
          BigNumberMul(Tmp, Tmp, GStep);
          if Dir = 0 then
            BigNumberAdd(BaseT, BaseT, Tmp)
          else
            BigNumberSub(BaseT, BaseT, Tmp);

          // For each baby step r_s
          for K := 0 to BabyRs.Count - 1 do
          begin
            // t = base_t + r_s * M_E
            BigNumberSetInt64(Tmp, BabyRs[K]);
            BigNumberMul(Tmp, Tmp, M_E);
            BigNumberAdd(T, BaseT, Tmp);

            // Check Hasse bound
            BigNumberCopy(QTmp, T);
            if QTmp.IsNegative then QTmp.Negate;
            if BigNumberCompare(QTmp, QMax) > 0 then Continue;

            // Check remaining Atkin constraints (skip BestIdx)
            Found := True;
            for I := 0 to AtkinInfos.Count - 1 do
            begin
              if I = BestIdx then Continue;
              LCheck := TCnSeaAtkinInfo(AtkinInfos[I]).L;
              TMod := BigNumberModWord(T, LCheck);
              if T.IsNegative then
                TMod := (LCheck - TMod) mod LCheck;
              if TCnSeaAtkinInfo(AtkinInfos[I]).PossibleTraces.IndexOf(TMod) < 0 then
              begin
                Found := False;
                Break;
              end;
            end;

            if Found then
            begin
              Verified := SeaVerifyTrace(A, B, P, T);
              if Verified then
              begin
                BigNumberCopy(Res, T);
                Result := True;
                Exit;
              end;
            end;
          end;
        end;

        // Check termination: |GiantSize * GStep| > Threshold
        BigNumberSetInt64(Tmp, GiantSize);
        BigNumberMul(Tmp, Tmp, GStep);
        if Tmp.IsNegative then Tmp.Negate;
        if BigNumberCompare(Tmp, Threshold) > 0 then Break;

        Inc(GiantSize);
        if GiantSize > 200000000 then Break; // safety limit
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
