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
* 兼容测试：暂无
* 本 地 化：该单元中的字符串均以常量的形式在单元的接口部分定义
* 修改记录：2026.03.24 V1.0
*               创建单元
================================================================================
|</PRE>}

{$I CnPack.inc}

interface

uses
  SysUtils, Classes, CnBigNumber, CnPolynomial;

function CnGenerateClassicalModularPolynomial(Res: TCnBigNumberBiPolynomial; L: Integer): Boolean;
{* 计算并返回正整数 L 的经典模多项式 Phi_L(X, Y)，L 是素数时初步符合以下网址的结果。
   https://math.mit.edu/~drew/ClassicalModPolys.html

   参数：
     Res: TCnBigNumberBiPolynomial        - 返回计算出的二元大整数多项式
     L: Integer                           - 模多项式序数

  返回值：Boolean                         - 返回计算是否成功
}

procedure PrintModularPolynomialCoefficients(P: TCnBigNumberBiPolynomial; Res: TStrings);
{* 将二元多项式的非零系数以 [X度, Y度] 系数值 的格式逐行打印到 Res 字符串列表中。
   同时验证对称位置 [i,j] 和 [j,i] 的系数是否相等，不相等则抛出异常。
   注意：[i,i] 对角线项无需验证（自身和自己相等）。

   参数：
    P: TCnBigNumberBiPolynomial           - 待打印的二元多项式
    Res: TStrings                         - 输出的目标字符串列表

   返回值：（无）
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

end.
