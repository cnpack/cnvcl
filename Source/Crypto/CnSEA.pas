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
  SysUtils, Classes, Contnrs, CnBigNumber, CnPolynomial, CnPrime, CnECC, CnContainers;

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

procedure PrintModularPolynomialCoefficients(P: TCnBigNumberBiPolynomial; Res: TStrings);
{* 将二元多项式的非零系数以 [X度, Y度] 系数值 的格式逐行打印到 Res 字符串列表中。
   同时验证对称位置 [i,j] 和 [j,i] 的系数是否相等，不相等则抛出异常。
   注意：[i,i] 对角线项无需验证（自身和自己相等）。

   参数：
     P: TCnBigNumberBiPolynomial          - 待打印的二元多项式
     Res: TStrings                        - 输出的目标字符串列表

   返回值：（无）
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
  L: Integer; A, B, P: TCnBigNumber; DPs: TObjectList = nil): Boolean;
{* 对 Elkies 素数 L，计算核多项式 h(x)（次数为 (l-1)/2）。
   通过对 L 阶除法多项式 ψ_l(x) 取平方-free 部分并因式分解来实现。

   参数：
     Res: TCnBigNumberPolynomial          - 返回核多项式 h(x)
     L: Integer                           - Elkies 素数
     A, B: TCnBigNumber                   - Weierstrass 方程参数
     P: TCnBigNumber                      - 有限域素数 p

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

function CnSeaPointCount(Res, A, B, P: TCnBigNumber;
  ModPolys: TObjectList = nil): Boolean;
{* SEA 点不完整计数算法：计算椭圆曲线 E: y^2 = x^3 + Ax + B 在 F_p 上的点数 #E(F_p)。
   对每个小素数 l，优先使用 Elkies 方法计算 t mod l，Atkin 素数回退到 Schoof 方法，
   最后用中国剩余定理合并结果，#E = p + 1 - t。

   参数：
     Res: TCnBigNumber                    - 返回点数 #E(F_p)
     A, B: TCnBigNumber                   - Weierstrass 方程参数
     P: TCnBigNumber                      - 有限域素数 p

   返回值：Boolean                        - 返回计算是否成功
}

implementation

const
  CN_SEA_BSGS_THRESHOLD = 100000;
  {* 当 Elkies-Atkin 组合搜索空间 N = floor(2*QMax/M_E) 超过此阈值时，
     从暴力搜索切换到 BSGS（Baby-Step Giant-Step）算法。

     取值依据：每次迭代约 25μs（256 位 BigNumber 运算），100000 次约 2.5 秒，
     在可接受范围内。超过此值则 BSGS 的 L_1/|S_1| 倍加速（通常 2~10x）更划算。
     - 调大：更多用暴力搜索（简单但慢），适合小素数场景
     - 调小：更早启用 BSGS（快但需 Atkin 素数），适合大素数场景}

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

function CnGenerateClassicalModularPolynomial(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
var
  N, I, K, D, M, U, V, MaxY: Integer;
  J_q, PolyT: TCnBigNumberPolynomial;
  H: array of TCnBigNumberPolynomial;
  Pm_Poly, Sm_Poly: array of TCnBigNumberPolynomial;
  PmArr: array of TCnBigNumber;
  T, Sum, Coeff: TCnBigNumber;
begin
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

      for I := 0 to M * L do PmArr[I].Free;
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

procedure PrintModularPolynomialCoefficients(P: TCnBigNumberBiPolynomial; Res: TStrings);
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
      if I >= J then
        Res.Add(Format('[%d,%d] %s', [I, YList[J].Exponent, YList[J].Value.ToDec]));
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
          'PrintModularPolynomialCoefficients: 对称系数不等 [%d,%d]=%s vs [%d,%d]=%s',
          [I, YList[J].Exponent, CoeffIJ.ToDec,
           YList[J].Exponent, I, CoeffJI.ToDec]);
    end;
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
  L: Integer; A, B, P: TCnBigNumber; DPs: TObjectList = nil): Boolean;
var
  OwnDPs: Boolean;
  PsiL, Y2, XPowP, XPmX: TCnBigNumberPolynomial;
  T1, T2, G, H: TCnBigNumberPolynomial;
  Lambda, TargetDeg: Integer;
  Found: Boolean;
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
    // 取 Psi_L(x) 作为模多项式 —— 由 DPs 拥有
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
      BigNumberPolynomialGaloisGreatestCommonDivisor(H, PsiL, G, P);

      if H.MaxDegree = TargetDeg then
      begin
        BigNumberPolynomialCopy(Res, H);
        Found := True;
        Break;
      end;
    end;

    if not Found then Exit;

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

function CnSeaElkiesTrace(Res: TCnBigNumber; L: Integer;
  A, B, P: TCnBigNumber; DPs: TObjectList = nil): Boolean;
var
  H: TCnBigNumberPolynomial;
  Y2: TCnBigNumberPolynomial;
  BQ: TCnBigNumber;
  PiPX, PiPY: TCnBigNumberRationalPolynomial;
  RSX, RSY, TSX, TSY: TCnBigNumberRationalPolynomial;
  PX, PY: TCnBigNumberRationalPolynomial;
  I, Lambda: Integer;
  T, LambdaInv, K: TCnBigNumber;
  Found: Boolean;
begin
  Result := False;
  if (Res = nil) or (L < 3) then Exit;
  if (A = nil) or (B = nil) or (P = nil) then Exit;

  H := nil;
  Y2 := nil;
  BQ := nil;
  PiPX := nil;
  PiPY := nil;
  RSX := nil;
  RSY := nil;
  TSX := nil;
  TSY := nil;
  PX := nil;
  PY := nil;
  T := nil;
  LambdaInv := nil;
  K := nil;
  try
    // 步骤 1：计算核多项式 h(x)，次数为 (L-1)/2
    H := FSeaPolynomialPool.Obtain;
    if not CnSeaElkiesKernelPolynomial(H, L, A, B, P, DPs) then Exit;

    // 步骤 2：设置辅助多项式 Y2 = x^3 + Ax + B
    Y2 := FSeaPolynomialPool.Obtain;
    Y2.SetCoefficients([B, A, 0, 1]);

    BQ := FSeaBigNumberPool.Obtain;

    // 步骤 3：计算 Frobenius 作用 pi(P) = (x^p mod h, y * Y2^((p-1)/2) mod h)
    // 在有理多项式表示中，点 (x, y) 对应 (MX, MY * y)
    // pi(P) 的 x 坐标 = x^p mod h(x)
    // pi(P) 的 y 系数 = Y2^((p-1)/2) mod h(x)（因为 y^p = y * (y^2)^((p-1)/2)）

    PiPX := FSeaRationalPolynomialPool.Obtain;
    PiPY := FSeaRationalPolynomialPool.Obtain;

    PiPX.SetOne;
    PiPX.Numerator.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(PiPX.Numerator, PiPX.Numerator, P, P, H); // x^p mod h

    PiPY.SetOne;
    BigNumberCopy(BQ, P);
    BQ.SubWord(1);
    BigNumberShiftRightOne(BQ, BQ); // (p-1)/2
    BigNumberPolynomialGaloisPower(PiPY.Numerator, Y2, BQ, P, H); // Y2^((p-1)/2) mod h

    // 步骤 4：设置通用点 P = (x, y) 用于增量计算
    PX := FSeaRationalPolynomialPool.Obtain;
    PY := FSeaRationalPolynomialPool.Obtain;
    PX.SetOne;
    PX.Numerator.SetCoefficients([0, 1]); // x
    PY.SetOne; // 1 * y

    // 步骤 5：初始化 RS = [1]P = (x, y)
    RSX := FSeaRationalPolynomialPool.Obtain;
    RSY := FSeaRationalPolynomialPool.Obtain;
    BigNumberRationalPolynomialCopy(RSX, PX);
    BigNumberRationalPolynomialCopy(RSY, PY);

    TSX := FSeaRationalPolynomialPool.Obtain;
    TSY := FSeaRationalPolynomialPool.Obtain;

    // 步骤 6：搜索特征值 lambda
    // 逐一比较 [J]P 与 pi(P)，J = 1, ..., (L+1)/2
    // 若 x 坐标匹配，再比较 y 坐标确定符号
    Found := False;
    Lambda := 0;

    for I := 1 to (L + 1) div 2 do
    begin
      if BigNumberRationalPolynomialGaloisEqual(RSX, PiPX, P, H) then
      begin
        // x 坐标匹配
        if BigNumberRationalPolynomialGaloisEqual(RSY, PiPY, P, H) then
          Lambda := I  // y 也匹配
        else
          Lambda := L - I; // y 相反
        Found := True;
        Break;
      end;

      // RS = RS + P = [I+1]P（增量加法）
      if I < (L + 1) div 2 then
      begin
        TCnPolynomialEcc.RationalPointAddPoint(RSX, RSY, PX, PY, TSX, TSY, A, B, P, H);
        BigNumberRationalPolynomialCopy(RSX, TSX);
        BigNumberRationalPolynomialCopy(RSY, TSY);
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
    FSeaRationalPolynomialPool.Recycle(TSY);
    FSeaRationalPolynomialPool.Recycle(TSX);
    FSeaRationalPolynomialPool.Recycle(RSY);
    FSeaRationalPolynomialPool.Recycle(RSX);
    FSeaRationalPolynomialPool.Recycle(PY);
    FSeaRationalPolynomialPool.Recycle(PX);
    FSeaRationalPolynomialPool.Recycle(PiPY);
    FSeaRationalPolynomialPool.Recycle(PiPX);
    FSeaBigNumberPool.Recycle(BQ);
    FSeaPolynomialPool.Recycle(Y2);
    FSeaPolynomialPool.Recycle(H);
  end;
end;

// 辅助函数：使用基本 Schoof 方法计算 t mod L（用于 Atkin 素数或回退）
function SeaSchoofTraceModL(Res: TCnBigNumber; L: Int64;
  A, B, P: TCnBigNumber; DPs: TObjectList): Boolean;
var
  K: Int64;
  Y2, LDP: TCnBigNumberPolynomial;
  BQ: TCnBigNumber;
  Pi2PX, Pi2PY, PiPX, PiPY, KPX, KPY: TCnBigNumberRationalPolynomial;
  LSX, LSY, RSX, RSY, TSX, TSY: TCnBigNumberRationalPolynomial;
  I: Integer;
  Found: Boolean;
begin
  Result := False;

  Y2 := nil;
  BQ := nil;
  Pi2PX := nil;
  Pi2PY := nil;
  PiPX := nil;
  PiPY := nil;
  KPX := nil;
  KPY := nil;
  LSX := nil;
  LSY := nil;
  RSX := nil;
  RSY := nil;
  TSX := nil;
  TSY := nil;
  try
    Y2 := FSeaPolynomialPool.Obtain;
    Y2.SetCoefficients([B, A, 0, 1]);

    BQ := FSeaBigNumberPool.Obtain;

    // 取 L 阶除法多项式作为模多项式
    LDP := TCnBigNumberPolynomial(DPs[L]);

    K := BigNumberModWord(P, L);

    // 计算 pi^2(P) 的 x 坐标: x^(p^2) mod LDP
    Pi2PX := FSeaRationalPolynomialPool.Obtain;
    Pi2PX.SetOne;
    Pi2PX.Numerator.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(Pi2PX.Numerator, Pi2PX.Numerator, P, P, LDP); // x^p
    BigNumberPolynomialGaloisPower(Pi2PX.Numerator, Pi2PX.Numerator, P, P, LDP); // x^(p^2)

    // 计算 pi^2(P) 的 y 系数: Y2^((p^2-1)/2) mod LDP
    Pi2PY := FSeaRationalPolynomialPool.Obtain;
    Pi2PY.SetOne;
    BigNumberMul(BQ, P, P); // p^2
    BQ.SubWord(1);          // p^2 - 1
    BigNumberShiftRightOne(BQ, BQ); // (p^2 - 1) / 2
    BigNumberPolynomialGaloisPower(Pi2PY.Numerator, Y2, BQ, P, LDP);

    // 计算 [K]P
    KPX := FSeaRationalPolynomialPool.Obtain;
    KPY := FSeaRationalPolynomialPool.Obtain;
    KPX.SetOne;
    KPX.Numerator.SetCoefficients([0, 1]); // x
    KPY.SetOne; // 1 * y
    TCnPolynomialEcc.RationalMultiplePoint(K, KPX, KPY, A, B, P, LDP);

    // LS = pi^2(P) + [K]P
    LSX := FSeaRationalPolynomialPool.Obtain;
    LSY := FSeaRationalPolynomialPool.Obtain;
    TCnPolynomialEcc.RationalPointAddPoint(Pi2PX, Pi2PY, KPX, KPY, LSX, LSY, A, B, P, LDP);

    // 若 LS 为零（点 at infinity），则 t = 0
    if LSX.IsZero and LSY.IsZero then
    begin
      Res.SetZero;
      Result := True;
      Exit;
    end;

    // 计算 pi(P) 的 x 坐标: x^p mod LDP
    PiPX := FSeaRationalPolynomialPool.Obtain;
    PiPX.SetOne;
    PiPX.Numerator.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(PiPX.Numerator, PiPX.Numerator, P, P, LDP);

    // 计算 pi(P) 的 y 系数: Y2^((p-1)/2) mod LDP
    PiPY := FSeaRationalPolynomialPool.Obtain;
    PiPY.SetOne;
    BigNumberCopy(BQ, P);
    BQ.SubWord(1);
    BigNumberShiftRightOne(BQ, BQ); // (p-1)/2
    BigNumberPolynomialGaloisPower(PiPY.Numerator, Y2, BQ, P, LDP);

    // 搜索 t: RS = [J] * pi(P)，J = 1, ..., (L+1)/2
    RSX := FSeaRationalPolynomialPool.Obtain;
    RSY := FSeaRationalPolynomialPool.Obtain;
    BigNumberRationalPolynomialCopy(RSX, PiPX);
    BigNumberRationalPolynomialCopy(RSY, PiPY);

    TSX := FSeaRationalPolynomialPool.Obtain;
    TSY := FSeaRationalPolynomialPool.Obtain;

    Found := False;
    for I := 1 to (L + 1) div 2 do
    begin
      if BigNumberRationalPolynomialGaloisEqual(LSX, RSX, P, LDP) then
      begin
        if BigNumberRationalPolynomialGaloisEqual(LSY, RSY, P, LDP) then
          BigNumberSetWord(Res, I)
        else
          BigNumberSetWord(Res, L - I);
        Found := True;
        Break;
      end;

      // RS = RS + pi(P) = [I+1] * pi(P)
      if I < (L + 1) div 2 then
      begin
        TCnPolynomialEcc.RationalPointAddPoint(RSX, RSY, PiPX, PiPY, TSX, TSY, A, B, P, LDP);
        BigNumberRationalPolynomialCopy(RSX, TSX);
        BigNumberRationalPolynomialCopy(RSY, TSY);
      end;
    end;

    if not Found then
    begin
      // t = 0
      Res.SetZero;
    end;

    Result := True;
  finally
    FSeaRationalPolynomialPool.Recycle(TSY);
    FSeaRationalPolynomialPool.Recycle(TSX);
    FSeaRationalPolynomialPool.Recycle(RSY);
    FSeaRationalPolynomialPool.Recycle(RSX);
    FSeaRationalPolynomialPool.Recycle(LSY);
    FSeaRationalPolynomialPool.Recycle(LSX);
    FSeaRationalPolynomialPool.Recycle(KPY);
    FSeaRationalPolynomialPool.Recycle(KPX);
    FSeaRationalPolynomialPool.Recycle(PiPY);
    FSeaRationalPolynomialPool.Recycle(PiPX);
    FSeaRationalPolynomialPool.Recycle(Pi2PY);
    FSeaRationalPolynomialPool.Recycle(Pi2PX);
    FSeaBigNumberPool.Recycle(BQ);
    // LDP 由 DPs 拥有，不释放
    Y2.Free;
  end;
end;

// Verify candidate trace t by checking [p+1-t]P = O for a point P on E/F_p
function SeaVerifyTrace(A, B, P, T: TCnBigNumber): Boolean;
var
  N, X, Y2, Y, RX, RY, SX, SY, Lam, T1, T2, T3: TCnBigNumber;
  Inf: Boolean;
  I, Bits: Integer;
  StartX: Int64;
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

    // Find a point on E(F_p): try x = 0, 1, 2, ...
    StartX := 0;
    while True do
    begin
      X.SetWord(StartX);
      if BigNumberCompare(X, P) >= 0 then Exit;

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

      if Y2.IsZero then
      begin
        BigNumberCopy(SX, X);
        SY.SetZero;
        Break;
      end;

      // Check QR via Euler's criterion
      BigNumberCopy(T1, P);
      T1.SubWord(1);
      BigNumberShiftRightOne(T1, T1);
      BigNumberPowerMod(T2, Y2, T1, P);
      if T2.IsOne then
      begin
        if BigNumberTonelliShanks(Y, Y2, P) then
        begin
          BigNumberCopy(SX, X);
          BigNumberCopy(SY, Y);
          Break;
        end;
      end;
      Inc(StartX);
    end;

    // Compute [N]P using double-and-add
    Inf := True;
    Bits := BigNumberGetBitsCount(N);

    for I := 0 to Bits - 1 do
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
              Continue // R = S, doubling already done
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

    Result := Inf;
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
  I, J, ModPolyIdx: Integer;
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
          // Elkies 失败，回退到 Schoof
          SeaSchoofTraceModL(TraceRes, L, A, B, P, DPs);
          Ta[I] := TraceRes.GetInt64;
          ElkiesTraces.Add(Ta[I]);
          ElkiesModuli.Add(L);
        end;
      end
      else if PrimeType = sptAtkin then
      begin
        AtkinTraces.Clear;
        if (ModPolys <> nil) and (ModPolyIdx - 1 < ModPolys.Count) and
           CnSeaAtkinPossibleTraces(AtkinTraces, L, A, B, P,
           TCnBigNumberBiPolynomial(ModPolys[ModPolyIdx - 1])) then
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
          SeaSchoofTraceModL(TraceRes, L, A, B, P, DPs);
          Ta[I] := TraceRes.GetInt64;
          ElkiesTraces.Add(Ta[I]);
          ElkiesModuli.Add(L);
        end;
      end
      else
      begin
        // 判定失败，回退到 Schoof
        SeaSchoofTraceModL(TraceRes, L, A, B, P, DPs);
        Ta[I] := TraceRes.GetInt64;
      end;
    end;

    // 中国剩余定理合并结果
    if AtkinInfos.Count > 0 then
    begin
      if not CnSeaCombineElkiesAtkin(Res, ElkiesTraces, ElkiesModuli, AtkinInfos, A, B, P) then
      begin
        // Atkin matching failed (search space too large).
        // Fall back to Schoof for Atkin primes, then CRT.
        for I := 0 to AtkinInfos.Count - 1 do
        begin
          L := TCnSeaAtkinInfo(AtkinInfos[I]).L;
          SeaSchoofTraceModL(TraceRes, L, A, B, P, DPs);
          ElkiesTraces.Add(TraceRes.GetInt64);
          ElkiesModuli.Add(L);
        end;
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

    R := 1;
    for I := 0 to Factors.Count - 1 do
    begin
      Deg := TCnBigNumberPolynomial(Factors[I]).MaxDegree;
      if Deg > 0 then
        R := SeaInt64LCM(R, Deg);
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
