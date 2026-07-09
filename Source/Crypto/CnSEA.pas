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
* 修改记录：2026.07.09 V1.1
*               实现 SEA 第一阶段：Elkies 素数检测与迹计算
*           2026.03.24 V1.0
*           创建单元，实现经典模多项式生成
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
  JPrime: TCnBigNumber = nil): TCnSeaPrimeType;
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
  L: Integer; A, B, P: TCnBigNumber): Boolean;
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
  A, B, P: TCnBigNumber): Boolean;
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

function CnSeaElkiesPointCount(Res, A, B, P: TCnBigNumber): Boolean;
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

    // H[k](q) = H[k-1](q) * j(q)
    // H[k][i+k] corresponds to q^i, i in [-k..N-k]
    // j(q) coeff of q^r is J_q[r+1], r in [-1..N-1]
    for K := 1 to N do
    begin
      for I := -K to N - K do
      begin
        Sum.SetZero;
        // i = d + r, where d is power in H[k-1] (d in [-(k-1)..N-(k-1)])
        // r is power in j(q) (r in [-1..N-1])
        // so d = i - r.
        // We iterate r from -1 to N-1.
        for D := -(K - 1) to N - (K - 1) do
        begin
          // r = i - d
          // r must be in [-1..N-1]
          if (I - D >= -1) and (I - D <= N - 1) then
          begin
            BigNumberMul(T, H[K - 1][D + (K - 1)], J_q[I - D + 1]);
            BigNumberAdd(Sum, Sum, T);
          end;
        end;
        BigNumberCopy(H[K][I + K], Sum);
      end;
    end;

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

  T1 := TCnBigNumber.Create;
  T2 := TCnBigNumber.Create;
  Num := TCnBigNumber.Create;
  Den := TCnBigNumber.Create;
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
    Den.Free;
    Num.Free;
    T2.Free;
    T1.Free;
  end;
end;

function CnSeaCheckPrimeType(L: Integer; A, B, P: TCnBigNumber;
  JPrime: TCnBigNumber): TCnSeaPrimeType;
var
  J: TCnBigNumber;
  PhiL: TCnBigNumberBiPolynomial;
  FY: TCnBigNumberPolynomial;
  Roots: TCnBigNumberList;
begin
  Result := sptFailed;
  if (L < 2) or ((L > 2) and not CnUInt32IsPrime(L)) then Exit;
  if (A = nil) or (B = nil) or (P = nil) then Exit;

  J := TCnBigNumber.Create;
  PhiL := TCnBigNumberBiPolynomial.Create;
  FY := TCnBigNumberPolynomial.Create;
  Roots := TCnBigNumberList.Create;
  try
    // 计算 j 不变量
    if not CnSeaJInvariant(J, A, B, P) then Exit;

    // 生成模多项式 Phi_L(X, Y)
    if not CnGenerateClassicalModularPolynomial(PhiL, L) then Exit;

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
      Result := sptAtkin;
  finally
    Roots.Free;
    FY.Free;
    PhiL.Free;
    J.Free;
  end;
end;

function CnSeaElkiesKernelPolynomial(Res: TCnBigNumberPolynomial;
  L: Integer; A, B, P: TCnBigNumber): Boolean;
var
  DPs: TObjectList;
  PsiL, Y2, XPowP, XPmX: TCnBigNumberPolynomial;
  T1, T2, T3, G, H: TCnBigNumberPolynomial;
  Lambda, TargetDeg: Integer;
  Found: Boolean;
begin
  Result := False;
  if (Res = nil) or (L < 3) then Exit;
  if (A = nil) or (B = nil) or (P = nil) then Exit;

  DPs := nil;
  Y2 := nil;
  XPowP := nil;
  XPmX := nil;
  T1 := nil;
  T2 := nil;
  T3 := nil;
  G := nil;
  H := nil;
  try
    // 生成 0 到 L+1 阶的除法多项式
    DPs := TObjectList.Create(True);
    CnGenerateGaloisDivisionPolynomials(A, B, P, L + 1, DPs);

    // 取 Psi_L(x) 作为模多项式 —— 由 DPs 拥有
    PsiL := TCnBigNumberPolynomial(DPs[L]);

    // 设置 Y2 = x^3 + Ax + B
    Y2 := TCnBigNumberPolynomial.Create;
    Y2.SetCoefficients([B, A, 0, 1]);

    // 计算 x^p mod Psi_L
    XPowP := TCnBigNumberPolynomial.Create;
    XPowP.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(XPowP, XPowP, P, P, PsiL);

    // XPmX = x^p - x mod Psi_L
    XPmX := TCnBigNumberPolynomial.Create;
    T1 := TCnBigNumberPolynomial.Create;
    T1.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisSub(XPmX, XPowP, T1, P, PsiL);

    T2 := TCnBigNumberPolynomial.Create;
    T3 := TCnBigNumberPolynomial.Create;
    G := TCnBigNumberPolynomial.Create;
    H := TCnBigNumberPolynomial.Create;

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
    H.Free;
    G.Free;
    T3.Free;
    T2.Free;
    T1.Free;
    XPmX.Free;
    XPowP.Free;
    Y2.Free;
    DPs.Free;
  end;
end;

function CnSeaElkiesTrace(Res: TCnBigNumber; L: Integer;
  A, B, P: TCnBigNumber): Boolean;
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
    H := TCnBigNumberPolynomial.Create;
    if not CnSeaElkiesKernelPolynomial(H, L, A, B, P) then Exit;

    // 步骤 2：设置辅助多项式 Y2 = x^3 + Ax + B
    Y2 := TCnBigNumberPolynomial.Create;
    Y2.SetCoefficients([B, A, 0, 1]);

    BQ := TCnBigNumber.Create;

    // 步骤 3：计算 Frobenius 作用 pi(P) = (x^p mod h, y * Y2^((p-1)/2) mod h)
    // 在有理多项式表示中，点 (x, y) 对应 (MX, MY * y)
    // pi(P) 的 x 坐标 = x^p mod h(x)
    // pi(P) 的 y 系数 = Y2^((p-1)/2) mod h(x)（因为 y^p = y * (y^2)^((p-1)/2)）

    PiPX := TCnBigNumberRationalPolynomial.Create;
    PiPY := TCnBigNumberRationalPolynomial.Create;

    PiPX.SetOne;
    PiPX.Numerator.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(PiPX.Numerator, PiPX.Numerator, P, P, H); // x^p mod h

    PiPY.SetOne;
    BigNumberCopy(BQ, P);
    BQ.SubWord(1);
    BigNumberShiftRightOne(BQ, BQ); // (p-1)/2
    BigNumberPolynomialGaloisPower(PiPY.Numerator, Y2, BQ, P, H); // Y2^((p-1)/2) mod h

    // 步骤 4：设置通用点 P = (x, y) 用于增量计算
    PX := TCnBigNumberRationalPolynomial.Create;
    PY := TCnBigNumberRationalPolynomial.Create;
    PX.SetOne;
    PX.Numerator.SetCoefficients([0, 1]); // x
    PY.SetOne; // 1 * y

    // 步骤 5：初始化 RS = [1]P = (x, y)
    RSX := TCnBigNumberRationalPolynomial.Create;
    RSY := TCnBigNumberRationalPolynomial.Create;
    BigNumberRationalPolynomialCopy(RSX, PX);
    BigNumberRationalPolynomialCopy(RSY, PY);

    TSX := TCnBigNumberRationalPolynomial.Create;
    TSY := TCnBigNumberRationalPolynomial.Create;

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
    T := TCnBigNumber.Create;
    LambdaInv := TCnBigNumber.Create;
    K := TCnBigNumber.Create;

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
    K.Free;
    LambdaInv.Free;
    T.Free;
    TSY.Free;
    TSX.Free;
    RSY.Free;
    RSX.Free;
    PY.Free;
    PX.Free;
    PiPY.Free;
    PiPX.Free;
    BQ.Free;
    Y2.Free;
    H.Free;
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
  LDP := nil;
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
    Y2 := TCnBigNumberPolynomial.Create;
    Y2.SetCoefficients([B, A, 0, 1]);

    BQ := TCnBigNumber.Create;

    // 取 L 阶除法多项式作为模多项式
    LDP := TCnBigNumberPolynomial(DPs[L]);

    K := BigNumberModWord(P, L);

    // 计算 pi^2(P) 的 x 坐标: x^(p^2) mod LDP
    Pi2PX := TCnBigNumberRationalPolynomial.Create;
    Pi2PX.SetOne;
    Pi2PX.Numerator.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(Pi2PX.Numerator, Pi2PX.Numerator, P, P, LDP); // x^p
    BigNumberPolynomialGaloisPower(Pi2PX.Numerator, Pi2PX.Numerator, P, P, LDP); // x^(p^2)

    // 计算 pi^2(P) 的 y 系数: Y2^((p^2-1)/2) mod LDP
    Pi2PY := TCnBigNumberRationalPolynomial.Create;
    Pi2PY.SetOne;
    BigNumberMul(BQ, P, P); // p^2
    BQ.SubWord(1);          // p^2 - 1
    BigNumberShiftRightOne(BQ, BQ); // (p^2 - 1) / 2
    BigNumberPolynomialGaloisPower(Pi2PY.Numerator, Y2, BQ, P, LDP);

    // 计算 [K]P
    KPX := TCnBigNumberRationalPolynomial.Create;
    KPY := TCnBigNumberRationalPolynomial.Create;
    KPX.SetOne;
    KPX.Numerator.SetCoefficients([0, 1]); // x
    KPY.SetOne; // 1 * y
    TCnPolynomialEcc.RationalMultiplePoint(K, KPX, KPY, A, B, P, LDP);

    // LS = pi^2(P) + [K]P
    LSX := TCnBigNumberRationalPolynomial.Create;
    LSY := TCnBigNumberRationalPolynomial.Create;
    TCnPolynomialEcc.RationalPointAddPoint(Pi2PX, Pi2PY, KPX, KPY, LSX, LSY, A, B, P, LDP);

    // 若 LS 为零（点 at infinity），则 t = 0
    if LSX.IsZero and LSY.IsZero then
    begin
      Res.SetZero;
      Result := True;
      Exit;
    end;

    // 计算 pi(P) 的 x 坐标: x^p mod LDP
    PiPX := TCnBigNumberRationalPolynomial.Create;
    PiPX.SetOne;
    PiPX.Numerator.SetCoefficients([0, 1]); // x
    BigNumberPolynomialGaloisPower(PiPX.Numerator, PiPX.Numerator, P, P, LDP);

    // 计算 pi(P) 的 y 系数: Y2^((p-1)/2) mod LDP
    PiPY := TCnBigNumberRationalPolynomial.Create;
    PiPY.SetOne;
    BigNumberCopy(BQ, P);
    BQ.SubWord(1);
    BigNumberShiftRightOne(BQ, BQ); // (p-1)/2
    BigNumberPolynomialGaloisPower(PiPY.Numerator, Y2, BQ, P, LDP);

    // 搜索 t: RS = [J] * pi(P)，J = 1, ..., (L+1)/2
    RSX := TCnBigNumberRationalPolynomial.Create;
    RSY := TCnBigNumberRationalPolynomial.Create;
    BigNumberRationalPolynomialCopy(RSX, PiPX);
    BigNumberRationalPolynomialCopy(RSY, PiPY);

    TSX := TCnBigNumberRationalPolynomial.Create;
    TSY := TCnBigNumberRationalPolynomial.Create;

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
    TSY.Free;
    TSX.Free;
    RSY.Free;
    RSX.Free;
    LSY.Free;
    LSX.Free;
    KPY.Free;
    KPX.Free;
    PiPY.Free;
    PiPX.Free;
    Pi2PY.Free;
    Pi2PX.Free;
    BQ.Free;
    // LDP 由 DPs 拥有，不释放
    Y2.Free;
  end;
end;

function CnSeaElkiesPointCount(Res, A, B, P: TCnBigNumber): Boolean;
var
  Pa, Ta: TCnInt64List;
  QMax, QMul, BQ: TCnBigNumber;
  I, J: Integer;
  L: Int64;
  DPs: TObjectList;
  Y2, P1, P2, G: TCnBigNumberPolynomial;
  TraceRes: TCnBigNumber;
  PrimeType: TCnSeaPrimeType;
  ElkiesOk: Boolean;
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

    QMax := TCnBigNumber.Create;
    QMul := TCnBigNumber.Create;
    BQ := TCnBigNumber.Create;
    TraceRes := TCnBigNumber.Create;

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
    Y2 := TCnBigNumberPolynomial.Create;
    P1 := TCnBigNumberPolynomial.Create;
    P2 := TCnBigNumberPolynomial.Create;
    G := TCnBigNumberPolynomial.Create;
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
      end;
    end;

    // 预生成除法多项式（供 Schoof 回退使用）
    if Pa.Count > 0 then
    begin
      DPs := TObjectList.Create(True);
      CnGenerateGaloisDivisionPolynomials(A, B, P, Pa[Pa.Count - 1] + 2, DPs);
    end;

    // 对每个素数 L >= 3 计算迹
    for I := 0 to Pa.Count - 1 do
    begin
      L := Pa[I];
      if L = 2 then Continue; // 已处理

      // 检查是 Elkies 还是 Atkin
      PrimeType := CnSeaCheckPrimeType(L, A, B, P);

      if PrimeType = sptElkies then
      begin
        // 尝试 Elkies 方法
        ElkiesOk := CnSeaElkiesTrace(TraceRes, L, A, B, P);
        if ElkiesOk then
          Ta[I] := TraceRes.GetInt64
        else
        begin
          // Elkies 失败，回退到 Schoof
          SeaSchoofTraceModL(TraceRes, L, A, B, P, DPs);
          Ta[I] := TraceRes.GetInt64;
        end;
      end
      else if PrimeType = sptAtkin then
      begin
        // Atkin 素数：使用基本 Schoof 方法
        SeaSchoofTraceModL(TraceRes, L, A, B, P, DPs);
        Ta[I] := TraceRes.GetInt64;
      end
      else
      begin
        // 判定失败，回退到 Schoof
        SeaSchoofTraceModL(TraceRes, L, A, B, P, DPs);
        Ta[I] := TraceRes.GetInt64;
      end;
    end;

    // 中国剩余定理合并结果
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
    G.Free;
    P2.Free;
    P1.Free;
    Y2.Free;
    TraceRes.Free;
    BQ.Free;
    QMul.Free;
    QMax.Free;
    DPs.Free;
    Ta.Free;
    Pa.Free;
  end;
end;

end.
