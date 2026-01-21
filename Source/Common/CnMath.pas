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

unit CnMath;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：数学计算的算法单元
* 单元作者：CnPack 开发组
* 备    注：本单元实现了一些数学函数，少部分目的在于脱离 Math 库，运行效率较官方实现可能略低。
*           另一方面补充一些 Math 库中未实现的内容。
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.01.21 V1.1
*               加入高精度正弦余弦等的计算
*           2021.12.08 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnBigDecimal, CnComplex;

const
  CN_PI = 3.1415926535897932384626;
  {* 圆周率的浮点值}

  CN_FLOAT_DEFAULT_DIGIT = 10;
  {* 默认的浮点运算位数}

type
  ECnMathException = class(Exception);
  {* 数学计算相关异常}

  TInt64s = array of Int64;
  {* Int64 动态数组}

function CnAbs(F: Extended): Extended; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 计算浮点数的绝对值。

   参数：
     F: Extended                          - 待计算的浮点数

   返回值：Extended                       - 返回的绝对值
}

function CnFloor(F: Extended): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 浮点数向数轴负方向取整。

   参数：
     F: Extended                          - 待取整的浮点数

   返回值：Integer                        - 返回的取整值
}

function CnCeil(F: Extended): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 浮点数向数轴正方向取整。

   参数：
     F: Extended                          - 待取整的浮点数

   返回值：Integer                        - 返回的取整值
}

{
  计算连分数：
                  A1
  B0 +  ----------------------
                     A2
        B1 + -----------------
                       A3
             B2 + ------------
                            An
                  B3 + ... ---
                            Bn
}
function Int64ContinuedFraction(A, B: TInt64s): Extended;
{* 计算连分数的值。连分数表示形式如下，注意 A B 要求长度一致，且 A[0] 的值会被忽略。
   A 和 B 必须是相同大小的数组，实际使用的分子为 A[1] 到尾，分母为 B0 到尾巴。
   如果数组长度为 0，函数返回 0，如果数组长度为 1，函数返回 B[0]
   数组长度至少为 2 才会进行连分数计算。

                  A1
   B0 + ----------------------
                     A2
        B1 + -----------------
                       A3
             B2 + ------------
                            An
                  B3 + ... ---
                            Bn

   参数：
     A: TInt64s                           - 分子数组，A[0] 被忽略，A[1] 对应 A1，A[n] 对应 An
     B: TInt64s                           - 分母数组，B[0] 对应 B0，B[1] 对应 B1，...，B[n] 对应 Bn

   返回值：Extended                       - 返回连分数值
}

function Int64Sqrt(N: Int64): Extended;
{* 计算 Int64 的平方根，使用牛顿迭代 Xn+1 = (Xn + N/Xn)/2

   参数：
     N: Int64                             - 待计算平方根的整数

   返回值：Extended                       - 返回平方根
}

function FloatSqrt(F: Extended): Extended;
{* 计算扩展精度浮点数的平方根，使用牛顿迭代 Xn+1 = (Xn + N/Xn)/2

   参数：
     F: Extended                          - 待计算平方根的浮点数

   返回值：Extended                       - 返回平方根
}

function Int64LogN(N: Int64): Extended;
{* 计算 Int64 的自然对数，使用反双曲函数展开。

   参数：
     N: Int64                             - 待计算自然对数的整数

   返回值：Extended                       - 返回自然对数
}

function FloatLogN(F: Extended): Extended;
{* 计算扩展精度浮点数的自然对数，使用反双曲函数展开。

   参数：
     F: Extended                          - 待计算自然对数的浮点数

   返回值：Extended                       - 返回自然对数
}

function Int64Log10(N: Int64): Extended;
{* 计算 Int64 的常用对数，直接使用自然对数换算。

   参数：
     N: Int64                             - 待计算常用对数的整数

   返回值：Extended                       - 返回常用对数
}

function FloatLog10(F: Extended): Extended;
{* 计算扩展精度浮点数的常用对数，直接使用自然对数换算。

   参数：
     F: Extended                          - 待计算常用对数的浮点数

   返回值：Extended                       - 返回常用对数
}

function Int64Log2(N: Int64): Extended;
{* 计算 Int64 的 2 为底的对数，直接使用自然对数换算。

   参数：
     N: Int64                             - 待计算的 2 为底的对数的整数

   返回值：Extended                       - 返回 2 为底的对数
}

function FloatLog2(F: Extended): Extended;
{* 计算扩展精度浮点数的 2 为底的对数，直接使用自然对数换算。

   参数：
     F: Extended                          - 待计算的 2 为底的对数的浮点数

   返回值：Extended                       - 返回 2 为底的对数
}

function FloatGaussLegendrePi(RoundCount: Integer = 3): string;
{* 扩展精度范围内用高斯勒让德公式计算 Pi，3 轮便已抵达扩展精度极限。

   参数：
     RoundCount: Integer                  - 计算轮数

   返回值：string                         - 返回的 Pi 值字符串
}

function GaussLegendrePi(RoundCount: Integer = 8): string; overload;
{* 大浮点数用高斯勒让德公式计算 Pi，8 次迭代精度就到了 100 多位，12 轮耗时 5 秒。

   参数：
     RoundCount: Integer                  - 计算轮数

   返回值：string                         - 返回的 Pi 值字符串
}

function GaussLegendrePi(Res: TCnBigDecimal; RoundCount: Integer = 8): Boolean; overload;
{* 大浮点数用高斯勒让德公式计算 Pi，8 次迭代精度就到了 100 多位，12 轮耗时 5 秒。

   参数：
     Res: TCnBigDecimal                   - 返回的 Pi 值
     RoundCount: Integer                  - 计算轮数

   返回值：string                         - 返回计算是否成功
}

function XavierGourdonEuler(BlockSize: Integer = 1000): string;
{* 用 Xavier Gourdon 法计算欧拉常数 e 的值，参数为计算轮数。

   参数：
     BlockSize: Integer                   - 计算轮数

   返回值：string                         - 返回的 e 值字符串
}

function FloatAlmostZero(F: Extended; AbsGap: Extended = 0.0): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 判断一浮点数是否离 0 足够近。

   参数：
     F: Extended                          - 待判断的浮点数
     AbsGap: Extended                     - 近的距离，默认传 0.0，内部使用极小常量判断

   返回值：Boolean                        - 是否离 0 足够近
}

function FloatEqual(A: Extended; B: Extended; AbsGap: Extended = 0.0): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 封装的两个浮点数是否相等的判断。

   参数：
     A: Extended                          - 待判断的浮点数一
     B: Extended                          - 待判断的浮点数二
     AbsGap: Extended                     - 相等的距离，默认传 0.0，内部使用极小常量判断

   返回值：Boolean                        - 返回是否近似相等
}

function NormalizeAngle(Angle: Extended): Extended;
{* 将角度变至 [0, 2π) 范围内，也即归一化。

   参数：
     Angle: Extended                      - 待归一化的角度值

   返回值：Extended                       - 返回归一化后的角度
}

function FloatToHex(Value: Extended; MaxDigit: Integer = CN_FLOAT_DEFAULT_DIGIT): string;
{* 浮点数转换为十六进制字符串，包括整数部分与小数部分，MaxDigit 指明除不尽时最多保留小数点后多少位。

   参数：
     Value: Extended                      - 待转换的浮点数
     MaxDigit: Integer                    - 指明除不尽时最多保留小数点后多少位

   返回值：string                         - 返回表示该浮点数的十六进制字符串
}

function HexToFloat(const Hex: string): Extended;
{* 十六进制字符串转换成浮点数，支持带小数点的小数。

   参数：
     const Hex: string                    - 待转换的十六进制字符串，支持带小数点

   返回值：Extended                       - 返回的浮点数
}

function CnIntAbs(N: Integer): Integer;
{* 计算整数的绝对值。

   参数：
     N: Integer                           - 待计算绝对值的整数

   返回值：Integer                        - 返回绝对值
}

function CnInt64Abs(N: Int64): Int64;
{* 计算 Int64 的绝对值。

   参数：
     N: Int64                             - 待计算绝对值的整数

   返回值：Integer                        - 返回绝对值
}

function FastInverseSqrt(X: Single): Single;
{* 快速计算开根号的倒数。

   参数：
     X: Single                            - 待计算的单精度浮点数

   返回值：Single                         - 返回计算结果
}

function FastSqrt(N: Cardinal): Cardinal;
{* 逐位确定法快速计算整数的平方根的整数部分。

   参数：
     N: Cardinal                          - 待计算的整数

   返回值：Cardinal                       - 返回平方根的整数部分
}

function FastSqrt64(N: Int64): Int64;
{* 逐位确定法快速计算整数的平方根的整数部分。

   参数：
     N: Int64                             - 待计算的整数

   返回值：Int64                          - 返回平方根的整数部分
}

function BigDecimalEulerExp(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点数计算 e 的 Num 次方，精度由 Precision 控制。

   参数：
     Res: TCnBigDecimal                   - 容纳返回的计算结果
     Num: TCnBigDecimal                   - 指数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

function BigDecimalSin(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点数计算 Num 弧度的正弦值，精度由 Precision 控制。

   参数：
     Res: TCnBigDecimal                   - 容纳返回的计算结果
     Num: TCnBigDecimal                   - 弧度数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

function BigDecimalCos(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点数计算 Num 弧度的余弦值，精度由 Precision 控制。

   参数：
     Res: TCnBigDecimal                   - 容纳返回的计算结果
     Num: TCnBigDecimal                   - 弧度数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

function BigDecimalHyperbolicSin(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点数计算 Num 的双曲正弦值，精度由 Precision 控制。

   参数：
     Res: TCnBigDecimal                   - 容纳返回的计算结果
     Num: TCnBigDecimal                   - 弧度数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

function BigDecimalHyperbolicCos(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点数计算 Num 的双曲余弦值，精度由 Precision 控制。

   参数：
     Res: TCnBigDecimal                   - 容纳返回的计算结果
     Num: TCnBigDecimal                   - 弧度数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

function BigComplexDecimalEulerExp(Res, Num: TCnBigComplexDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点复数计算 e 的 Num 复数次方，实部和虚部的精度由 Precision 控制。

   参数：
     Res: TCnBigComplexDecimal            - 容纳返回的计算结果
     Num: TCnBigComplexDecimal            - 指数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

function BigComplexDecimalSin(Res, Num: TCnBigComplexDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点复数计算 Num 的复数正弦值，精度由 Precision 控制。

   参数：
     Res: TCnBigComplexDecimal            - 容纳返回的计算结果
     Num: TCnBigComplexDecimal            - 弧度数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

function BigComplexDecimalCos(Res, Num: TCnBigComplexDecimal;
  Precision: Integer = 0): Boolean;
{* 用大浮点复数计算 Num 的复数余弦值，精度由 Precision 控制。

   参数：
     Res: TCnBigComplexDecimal            - 容纳返回的计算结果
     Num: TCnBigComplexDecimal            - 弧度数
     Precision: Integer                   - 精度，也即小数点后的位数，如传 0 则使用默认设置

   返回值：Boolean                        - 返回计算是否成功
}

implementation

const
  SCN_FLOAT_GAP = 0.000001;         // 普通浮点判断
  SCN_EXTEND_GAP = 0.00000000001;   // 本单元中的迭代计算差值
  SCN_LOGN_TO_LOG2 = 1.4426950408889634073599246810019;
  SCN_LOGN_TO_LOG10 = 0.43429448190325182765112891891661;

  CN_TAYLOR_MAX_ITERATIONS = 10000;
  {* 泰勒级数最大迭代次数，防止死循环}

resourcestring
  SCnErrorMathSqrtRange = 'Sqrt Range Error.';
  SCnErrorMathLogRange = 'Log Range Error.';
  SCnErrorMathFractionError = 'Error Length for Continue Fraction';

var
  FLocalBigDecimalPool: TCnBigDecimalPool = nil;

function CnAbs(F: Extended): Extended;
begin
  if F < 0 then
    Result := -F
  else
    Result := F;
end;

function CnFloor(F: Extended): Integer;
begin
  Result := Trunc(F);
  if Frac(F) < 0 then
    Dec(Result);
end;

function CnCeil(F: Extended): Integer;
begin
  Result := Trunc(F);
  if Frac(F) > 0 then
    Inc(Result);
end;

function Int64ContinuedFraction(A, B: TInt64s): Extended;
var
  I, N: Integer;
  T: Extended;
begin
  if Length(A) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  if Length(A) <> Length(B) then
    raise ECnMathException.Create(SCnErrorMathFractionError);

  // 实际下标范围是 0..N
  N := Length(A) - 1;

  // 如果只有 B0，直接返回
  if N = 0 then
  begin
    Result := B[0];
    Exit;
  end;

  // 从最内层开始计算：T = A[N] / B[N]
  T := A[N] / B[N];

  // 从第 N-1 项向前迭代计算：T = A[I] / (B[I] + T)
  for I := N - 1 downto 1 do
    T := A[I] / (B[I] + T);

  Result := B[0] + T;
end;

{$HINTS OFF}

function Int64Sqrt(N: Int64): Extended;
var
  X0: Extended;
begin
  if N < 0 then
    raise ECnMathException.Create(SCnErrorMathSqrtRange);

  Result := 0;
  if (N = 0) or (N = 1) then
  begin
    Result := N;
    Exit;
  end;

  X0 := N;
  while True do
  begin
    Result := (X0 + N/X0) / 2;

    if CnAbs(Result - X0) < SCN_EXTEND_GAP then
      Break;
    X0 := Result;
  end;
end;

function FloatSqrt(F: Extended): Extended;
var
  X0: Extended;
begin
  if F < 0 then
    raise ECnMathException.Create(SCnErrorMathSqrtRange);

  Result := 0;
  if (F = 0) or (F = 1) then
  begin
    Result := F;
    Exit;
  end;

  X0 := F;
  while True do
  begin
    Result := (X0 + F/X0) / 2;

    if CnAbs(Result - X0) < SCN_EXTEND_GAP then
      Break;
    X0 := Result;
  end;
end;

{$HINTS ON}

function Int64LogN(N: Int64): Extended;
var
  I: Integer;
  F: Extended;
  Z, D: Extended;
begin
  if N <= 0 then
    raise ERangeError.Create(SCnErrorMathLogRange);

  Result := 0;
  if N = 1 then
    Exit;

  //           [ z-1   1 (z-1)^3   1 (z-1)^5        ]
  // lnz = 2 * | --- + - ------- + - ------- + .... |
  //           [ z+1   3 (z+1)^3   5 (z+1)^5        ]

  F := N;
  Z := (F - 1) / (F + 1);
  D := Z;
  Z := Z * Z;
  I := 1;

  while True do
  begin
    Result := Result + D / I;
    Inc(I, 2);
    D := D * Z;

    if CnAbs(D) < SCN_EXTEND_GAP then
      Break;
  end;
  Result := Result * 2;
end;

function FloatLogN(F: Extended): Extended;
var
  I: Integer;
  Z, D: Extended;
begin
  if F <= 0 then
    raise ERangeError.Create(SCnErrorMathLogRange);

  Result := 0;
  if F = 1 then
    Exit;

  //           [ z-1   1 (z-1)^3   1 (z-1)^5        ]
  // lnz = 2 * | --- + - ------- + - ------- + .... |
  //           [ z+1   3 (z+1)^3   5 (z+1)^5        ]

  Z := (F - 1) / (F + 1);
  D := Z;
  Z := Z * Z;
  I := 1;

  while True do
  begin
    Result := Result + D / I;
    Inc(I, 2);
    D := D * Z;

    if CnAbs(D) < SCN_EXTEND_GAP then
      Break;
  end;
  Result := Result * 2;
end;

function Int64Log10(N: Int64): Extended;
begin
  Result := Int64LogN(N) * SCN_LOGN_TO_LOG10;
end;

function FloatLog10(F: Extended): Extended;
begin
  Result := FloatLogN(F) * SCN_LOGN_TO_LOG10;
end;

function Int64Log2(N: Int64): Extended;
begin
  Result := Int64LogN(N) * SCN_LOGN_TO_LOG2;
end;

function FloatLog2(F: Extended): Extended;
begin
  Result := FloatLogN(F) * SCN_LOGN_TO_LOG2;
end;

function FloatGaussLegendrePi(RoundCount: Integer): string;
var
  I: Integer;
  A0, B0, T0, P0: Extended;
  A1, B1, T1, P1: Extended;
  Res: Extended;
begin
  A0 := 1;
  B0 := Sqrt(2) / 2;
  T0 := 0.25;
  P0 := 1;
  Res := 0;

  for I := 1 to RoundCount do
  begin
    A1 := (A0 + B0) / 2;
    B1 := Sqrt(A0 * B0);
    T1 := T0 - P0 * (A0 - A1) * (A0 - A1);
    P1 := P0 * 2;

    Res := (A1 + B1) * (A1 + B1) / (T1 * 4);

    A0 := A1;
    B0 := B1;
    T0 := T1;
    P0 := P1;
  end;

  Result := FloatToStr(Res);
end;

function GaussLegendrePi(RoundCount: Integer = 8): string;
var
  R: TCnBigDecimal;
begin
  R := FLocalBigDecimalPool.Obtain;
  try
    if GaussLegendrePi(R, RoundCount) then
      Result := R.ToString;
  finally
    FLocalBigDecimalPool.Recycle(R);
  end;
end;

function GaussLegendrePi(Res: TCnBigDecimal; RoundCount: Integer): Boolean;
var
  I, P: Integer;
  A0, B0, T0, P0: TCnBigDecimal;
  A1, B1, T1, P1: TCnBigDecimal;
  X1, X2: TCnBigDecimal;
begin
  Result := False;
  if (Res = nil) or (RoundCount < 0) then
    Exit;

  A0 := nil;
  B0 := nil;
  T0 := nil;
  P0 := nil;

  A1 := nil;
  B1 := nil;
  T1 := nil;
  P1 := nil;

  X1 := nil;
  X2 := nil;

  try
    A0 := FLocalBigDecimalPool.Obtain;
    B0 := FLocalBigDecimalPool.Obtain;
    T0 := FLocalBigDecimalPool.Obtain;
    P0 := FLocalBigDecimalPool.Obtain;

    A1 := FLocalBigDecimalPool.Obtain;
    B1 := FLocalBigDecimalPool.Obtain;
    T1 := FLocalBigDecimalPool.Obtain;
    P1 := FLocalBigDecimalPool.Obtain;

    // 临时变量
    X1 := FLocalBigDecimalPool.Obtain;
    X1.SetWord(2);
    X2 := FLocalBigDecimalPool.Obtain;

    P := 1 shl RoundCount;  // 根据 Round 数量提前确定精度
    if P < 16 then
      P := 16;

    A0.SetOne;
    B0.SetWord(2);
    BigDecimalSqrt(B0, B0, P);
    BigDecimalDiv(B0, B0, X1, P);
    T0.SetExtended(0.25);
    P0.SetOne;

    Res.SetZero;
    for I := 1 to RoundCount do
    begin
      // A1 := (A0 + B0) / 2;
      BigDecimalAdd(A1, A0, B0);
      BigDecimalDiv(A1, A1, X1, P);

      // B1 := Sqrt(A0 * B0);
      BigDecimalMul(B1, A0, B0);
      BigDecimalSqrt(B1, B1, P);

      // T1 := T0 - P0 * (A0 - A1) * (A0 - A1);
      BigDecimalSub(T1, A0, A1);
      BigDecimalMul(T1, T1, T1);
      BigDecimalMul(T1, T1, P0);
      BigDecimalSub(T1, T0, T1);

      // P1 := P0 * 2;
      BigDecimalAdd(P1, P0, P0);

      // Res := (A1 + B1) * (A1 + B1) / (T1 * 4);
      BigDecimalAdd(Res, A1, B1);
      BigDecimalMul(Res, Res, Res);
      BigDecimalAdd(X2, T1, T1);
      BigDecimalAdd(X2, X2, X2);

      BigDecimalDiv(Res, Res, X2, P);

      // 准备下一轮迭代
      BigDecimalCopy(A0, A1);
      BigDecimalCopy(B0, B1);
      BigDecimalCopy(T0, T1);
      BigDecimalCopy(P0, P1);
    end;

    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(X1);
    FLocalBigDecimalPool.Recycle(X2);

    FLocalBigDecimalPool.Recycle(A1);
    FLocalBigDecimalPool.Recycle(B1);
    FLocalBigDecimalPool.Recycle(T1);
    FLocalBigDecimalPool.Recycle(P1);

    FLocalBigDecimalPool.Recycle(A0);
    FLocalBigDecimalPool.Recycle(B0);
    FLocalBigDecimalPool.Recycle(T0);
    FLocalBigDecimalPool.Recycle(P0);
  end;
end;

function XavierGourdonEuler(BlockSize: Integer = 1000): string;
var
  N, M, X: Integer;
  A: array of Integer;
begin
  if BlockSize <= 0 then
    Exit;

  SetLength(A, BlockSize);
  N := BlockSize;
  M := BlockSize;
  Dec(N);
  A[0] := 0;
  while N <> 0 do
  begin
    A[N] := 1;
    Dec(N);
  end;
  A[1] := 2;
  X := 65536; // X 竟然随便是几甚至没初始化貌似都行？

  while M > 9 do
  begin
    N := M;
    Dec(M);
    Dec(N);
    while N <> 0 do
    begin
      A[N] := X mod N;
      X := 10 * A[N - 1] + X div N;
      Dec(N);
    end;

    Result := Result + IntToStr(X);
  end;

  if Length(Result) > 2 then
    Insert('.', Result, 2);
end;

function FloatAlmostZero(F: Extended; AbsGap: Extended): Boolean;
{$IFDEF SUPPORT_INLINE}
const
  SCN_FLOAT_GAP = 0.000001; // inline 不能使用外边的常量
{$ENDIF}
begin
  if AbsGap = 0.0 then
    AbsGap := SCN_FLOAT_GAP;
  Result := CnAbs(F) < AbsGap;
end;

function FloatEqual(A: Extended; B: Extended; AbsGap: Extended): Boolean;
begin
  Result := FloatAlmostZero(A - B, AbsGap);
end;

function NormalizeAngle(Angle: Extended): Extended;
begin
  Result := Angle;
  Result := Result - 2 * CN_PI * CnFloor(Result / (2 * CN_PI));
  if Result < 0 then
    Result := Result + 2 * CN_PI;
end;

function FloatToHex(Value: Extended; MaxDigit: Integer): string;
var
  A, B: Extended;
  S: string;
  Neg: Boolean;
  R, C: Integer;
begin
  A := Int(Value);
  B := Frac(Value);

  Neg := A < 0;
  if Neg then
  begin
    A := -A;
    B := -B;
  end;

  Result := '';
  while not FloatAlmostZero(A) do
  begin
    // 求 A 除以 16 的余数
    R := Trunc(A - Int(A / 16.0) * 16);

    // 将余数转换为十六进制字符并添加到字符串
    Result := IntToHex(R, 1) + Result;

    // 整数部分除以 16
    A := Int(A / 16);
  end;

  C := 0;
  S := '.';
  while (CnAbs(B) >= SCN_EXTEND_GAP) and (C <= MaxDigit) do
  begin
    B := B * 16;               // 乘以 16 取整数部分
    R := Trunc(B);
    S := S + IntToHex(R, 1);

    B := B - R;
    Inc(C);
  end;

  if Result = '' then
    Result := '0'
  else if Neg then
    Result := '-' + Result;

  if S <> '.' then
    Result := Result + S;
end;

function HexToFloat(const Hex: string): Extended;
var
  I: Integer;
  S: string;
  Neg: Boolean;

  function HexIntegerToFloat(Hex: PChar; CharLen: Integer): Extended;
  var
    I: Integer;
    C: Char;
  begin
    Result := 0;
    for I := 0 to CharLen - 1 do
    begin
      C := Hex[I];
      if (C >= '0') and (C <= '9') then
        Result := Result * 16 + Ord(C) - Ord('0')
      else if (C >= 'A') and (C <= 'F') then
        Result := Result * 16 + Ord(C) - Ord('A') + 10
      else if (C >= 'a') and (C <= 'f') then
        Result := Result * 16 + Ord(C) - Ord('a') + 10
      else
        raise Exception.CreateFmt('Error: not a Hex PChar: %c', [C]);
    end;
  end;

  function HexDecimalToFloat(Hex: PChar; CharLen: Integer): Extended;
  var
    I: Integer;
    C: Char;
    R: Extended;
  begin
    Result := 0;
    R := 1;
    for I := 0 to CharLen - 1 do
    begin
      C := Hex[I];
      R := R / 16;
      if (C >= '0') and (C <= '9') then
        Result := Result + (Ord(C) - Ord('0')) * R
      else if (C >= 'A') and (C <= 'F') then
        Result := Result + (Ord(C) - Ord('A') + 10) * R
      else if (C >= 'a') and (C <= 'f') then
        Result := Result + (Ord(C) - Ord('a') + 10) * R
      else
        raise Exception.CreateFmt('Error: not a Hex PChar: %c', [C]);
    end;
  end;

begin
  I := Pos('.', Hex);
  if I > 0 then
    S := Copy(Hex, 1, I - 1)
  else
    S := Hex;

  Neg := False;
  if (Length(S) > 0) and (S[1] = '-') then
  begin
    Delete(S, 1, 1);
    Neg := True;
  end;

  // 整数部分转换成值
  Result := HexIntegerToFloat(PChar(S), Length(S));

  if I > 0 then
  begin
    S := Copy(Hex, I + 1, MaxInt);

    // 把小数部分转换成值
    Result := Result + HexDecimalToFloat(PChar(S), Length(S));
  end;

  if Neg then
    Result := -Result;
end;

function CnIntAbs(N: Integer): Integer;
begin
  if N < 0 then
    Result := -N
  else
    Result := N;
end;

function CnInt64Abs(N: Int64): Int64;
begin
  if N < 0 then
    Result := -N
  else
    Result := N;
end;

// 快速计算开根号的倒数
function FastInverseSqrt(X: Single): Single;
type
  PCnInteger = ^Integer;
  PCnSingle = ^Single;
var
  xHalf: Single;
  I: Integer;
begin
  xHalf := 0.5 * X;
  I := (PCnInteger(@X))^;
  I := $5F375A86 - (I shr 1);
  X := (PCnSingle(@I))^;
  X := X *(1.5 - xHalf * X * X);
  X := X *(1.5 - xHalf * X * X);
  Result := X;
end;

// 逐位确定法快速计算整数的平方根的整数部分
function FastSqrt(N: Cardinal): Cardinal;
var
  T, B: Cardinal;
  Sft: Cardinal;
begin
  Result := 0;
  B := $8000;
  Sft := 15;
  repeat
    T := ((Result shl 1)+ B) shl Sft;
    Dec(Sft);
    if N >= T then
    begin
      Result := Result + B;
      N := N - T;
    end;
    B := B shr 1;
  until B = 0;
end;

// 逐位确定法快速计算整数的平方根的整数部分
function FastSqrt64(N: Int64): Int64;
var
  T, B: Int64;
  Sft: Int64;
begin
  Result := 0;
  B := $80000000;
  Sft := 31;
  repeat
    T := ((Result shl 1)+ B) shl Sft;
    Dec(Sft);
    if N >= T then
    begin
      Result := Result + B;
      N := N - T;
    end;
    B := B shr 1;
  until B = 0;
end;

function BigDecimalEulerExp(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer): Boolean;
{
  计算 e^x（实数）
  使用泰勒级数：e^x = 1 + x + x^2/2! + x^3/3! + ...

  算法优化：
  1. 范围归约：如果 |x| > 1，先计算 e^(x/2^k)，再平方 k 次
  2. 泰勒级数：对于 |x| < 1，级数收敛快
}
var
  I, K, TargetPrecision: Integer;
  X, Term, Sum, Factorial, Gap: TCnBigDecimal;
  Neg: Boolean;
begin
  Result := False;

  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  // 特殊情况
  if Num.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end;

  X := FLocalBigDecimalPool.Obtain;
  Term := FLocalBigDecimalPool.Obtain;
  Sum := FLocalBigDecimalPool.Obtain;
  Factorial := FLocalBigDecimalPool.Obtain;
  Gap := FLocalBigDecimalPool.Obtain;

  try
    BigDecimalCopy(X, Num);
    Neg := X.IsNegative;
    if Neg then
      X.Negate;

    // 范围归约：如果 x > 1，先除以 2^k 使其 < 1
    K := 0;
    while BigDecimalCompare(X, CnBigDecimalOne) > 0 do
    begin
      X.DivWord(2, Precision + 10);  // 除以 2，保留额外精度
      Inc(K);
    end;

    // 计算收敛阈值
    Gap.SetOne;
    Gap.Scale := Precision + 5;  // 10^-(Precision+5)

    // 泰勒级数：e^x = 1 + x + x^2/2! + x^3/3! + ...
    Sum.SetOne;           // S? = 1
    Term.SetOne;          // T? = 1
    Factorial.SetOne;     // 0! = 1

    I := 1;
    TargetPrecision := Precision + 10;  // 内部计算保留额外精度

    while I <= CN_TAYLOR_MAX_ITERATIONS do
    begin
      // Term := Term * X / I
      BigDecimalMul(Term, Term, X, TargetPrecision);
      Factorial.SetWord(I);
      BigDecimalDiv(Term, Term, Factorial, TargetPrecision);

      // Sum := Sum + Term
      BigDecimalAdd(Sum, Sum, Term);

      // 检查收敛：如果 |Term| < Gap，停止
      if BigDecimalCompare(Term, Gap) < 0 then
        Break;

      Inc(I);
    end;

    if I > CN_TAYLOR_MAX_ITERATIONS then
      raise ECnBigDecimalException.Create('Exp: Taylor series did not converge');

    // 如果做了范围归约，需要平方 k 次：(e^(x/2^k))^(2^k) = e^x
    for I := 1 to K do
      BigDecimalMul(Sum, Sum, Sum, TargetPrecision);

    // 如果原始输入是负数：e^(-x) = 1 / e^x
    if Neg then
    begin
      Res.SetOne;
      BigDecimalDiv(Res, Res, Sum, Precision);
    end
    else
    begin
      BigDecimalCopy(Res, Sum);
      Res.RoundTo(Precision);
    end;

    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(X);
    FLocalBigDecimalPool.Recycle(Term);
    FLocalBigDecimalPool.Recycle(Sum);
    FLocalBigDecimalPool.Recycle(Factorial);
    FLocalBigDecimalPool.Recycle(Gap);
  end;
end;

function GaussLegendrePrecistionToRoundCount(Precision: Integer): Integer;
begin
  if Precision <= 0 then
    Result := 1
  else
    Result := GetUInt32HighBits(Precision) + 1;
end;

function BigDecimalSin(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer): Boolean;
{
  计算 sin(x)
  使用泰勒级数：sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...

  优化：
  1. 范围归约到 [0, π/2]
  2. 使用恒等式 sin(x) = sin(π-x), sin(x+2π) = sin(x)
}
var
  I, TargetPrecision, Sign: Integer;
  X, X2, Term, Sum, Factorial, Gap, Pi, PiOver2, TwoPi: TCnBigDecimal;
  TN: Boolean;
begin
  Result := False;

  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  if Num.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  X := FLocalBigDecimalPool.Obtain;
  X2 := FLocalBigDecimalPool.Obtain;
  Term := FLocalBigDecimalPool.Obtain;
  Sum := FLocalBigDecimalPool.Obtain;
  Factorial := FLocalBigDecimalPool.Obtain;
  Gap := FLocalBigDecimalPool.Obtain;
  Pi := FLocalBigDecimalPool.Obtain;
  PiOver2 := FLocalBigDecimalPool.Obtain;
  TwoPi := FLocalBigDecimalPool.Obtain;

  try
    TargetPrecision := Precision + 10;

    // 计算 π
    GaussLegendrePi(Pi, GaussLegendrePrecistionToRoundCount(TargetPrecision));

    BigDecimalCopy(TwoPi, Pi);
    TwoPi.MulWord(2);
    BigDecimalCopy(PiOver2, Pi);
    PiOver2.DivWord(2, TargetPrecision);

    // 范围归约到 [0, 2π)
    BigDecimalCopy(X, Num);
    Sign := 1;

    if X.IsNegative then
    begin
      X.Negate;
      Sign := -1;
    end;

    // X := X mod 2π
    while BigDecimalCompare(X, TwoPi) >= 0 do
      BigDecimalSub(X, X, TwoPi);

    // 进一步归约到 [0, π/2]，利用对称性
    if BigDecimalCompare(X, Pi) >= 0 then
    begin
      BigDecimalSub(X, X, Pi);
      Sign := -Sign;
    end;

    if BigDecimalCompare(X, PiOver2) > 0 then
    begin
      BigDecimalSub(X, Pi, X);  // sin(π - x) = sin(x)
    end;

    // 泰勒级数：sin(x) = x - x^3/3! + x^5/5! - ...
    BigDecimalMul(X2, X, X, TargetPrecision);  // x^2

    Gap.SetOne;
    Gap.Scale := Precision + 5;

    BigDecimalCopy(Sum, X);    // S = x
    BigDecimalCopy(Term, X);   // T? = x

    I := 1;
    while I <= CN_TAYLOR_MAX_ITERATIONS do
    begin
      // Term := -Term * x^2 / ((2i)(2i+1))
      BigDecimalMul(Term, Term, X2, TargetPrecision);
      Term.Negate;
      Term.DivWord(2 * I * (2 * I + 1), TargetPrecision);

      BigDecimalAdd(Sum, Sum, Term);

      TN := Term.IsNegative;
      if TN then
        Term.Negate;

      if BigDecimalCompare(Term, Gap) < 0 then
        Break;

      if TN then
        Term.Negate;
      Inc(I);
    end;

    if I > CN_TAYLOR_MAX_ITERATIONS then
      raise ECnBigDecimalException.Create('Sin: Taylor series did not converge');

    BigDecimalCopy(Res, Sum);
    if Sign < 0 then
      Res.Negate;

    Res.RoundTo(Precision);
    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(X);
    FLocalBigDecimalPool.Recycle(X2);
    FLocalBigDecimalPool.Recycle(Term);
    FLocalBigDecimalPool.Recycle(Sum);
    FLocalBigDecimalPool.Recycle(Factorial);
    FLocalBigDecimalPool.Recycle(Gap);
    FLocalBigDecimalPool.Recycle(Pi);
    FLocalBigDecimalPool.Recycle(PiOver2);
    FLocalBigDecimalPool.Recycle(TwoPi);
  end;
end;

function BigDecimalCos(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer): Boolean;
{
  计算 cos(x)
  使用泰勒级数：cos(x) = 1 - x^2/2! + x^4/4! - x^6/6! + ...
  或者利用：cos(x) = sin(π/2 - x)
}
var
  I, TargetPrecision, Sign: Integer;
  X, X2, Term, Sum, Gap, Pi, TwoPi: TCnBigDecimal;
  TN: Boolean;
begin
  Result := False;

  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  if Num.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end;

  X := FLocalBigDecimalPool.Obtain;
  X2 := FLocalBigDecimalPool.Obtain;
  Term := FLocalBigDecimalPool.Obtain;
  Sum := FLocalBigDecimalPool.Obtain;
  Gap := FLocalBigDecimalPool.Obtain;
  Pi := FLocalBigDecimalPool.Obtain;
  TwoPi := FLocalBigDecimalPool.Obtain;

  try
    TargetPrecision := Precision + 10;

    GaussLegendrePi(Pi, GaussLegendrePrecistionToRoundCount(TargetPrecision));
    BigDecimalCopy(TwoPi, Pi);
    TwoPi.MulWord(2);

    // 范围归约
    BigDecimalCopy(X, Num);
    if X.IsNegative then
      X.Negate;  // cos(-x) = cos(x)

    while BigDecimalCompare(X, TwoPi) >= 0 do
      BigDecimalSub(X, X, TwoPi);

    Sign := 1;
    if BigDecimalCompare(X, Pi) >= 0 then
    begin
      BigDecimalSub(X, X, Pi);
      Sign := -1;  // cos(π + x) = -cos(x)
    end;

    // 泰勒级数：cos(x) = 1 - x^2/2! + x^4/4! - ...
    BigDecimalMul(X2, X, X, TargetPrecision);

    Gap.SetOne;
    Gap.Scale := Precision + 5;

    Sum.SetOne;     // S = 1
    Term.SetOne;    // T? = 1

    I := 1;
    while I <= CN_TAYLOR_MAX_ITERATIONS do
    begin
      // Term := -Term * x^2 / ((2i-1)(2i))
      BigDecimalMul(Term, Term, X2, TargetPrecision);
      Term.Negate;
      Term.DivWord((2 * I - 1) * (2 * I), TargetPrecision);

      BigDecimalAdd(Sum, Sum, Term);

      TN := Term.IsNegative;
      if TN then
        Term.Negate;

      if BigDecimalCompare(Term, Gap) < 0 then
        Break;

      if TN then
        Term.Negate;
      Inc(I);
    end;

    if I > CN_TAYLOR_MAX_ITERATIONS then
      raise ECnBigDecimalException.Create('Cos: Taylor series did not converge');

    BigDecimalCopy(Res, Sum);
    if Sign < 0 then
      Res.Negate;

    Res.RoundTo(Precision);
    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(X);
    FLocalBigDecimalPool.Recycle(X2);
    FLocalBigDecimalPool.Recycle(Term);
    FLocalBigDecimalPool.Recycle(Sum);
    FLocalBigDecimalPool.Recycle(Gap);
    FLocalBigDecimalPool.Recycle(Pi);
    FLocalBigDecimalPool.Recycle(TwoPi);
  end;
end;

function BigDecimalHyperbolicSin(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer = 0): Boolean;
{
  双曲正弦：sinh(x) = (e^x - e^(-x)) / 2
}
var
  ExpX, ExpNegX: TCnBigDecimal;
begin
  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  ExpX := FLocalBigDecimalPool.Obtain;
  ExpNegX := FLocalBigDecimalPool.Obtain;

  try
    // 计算 e^x
    BigDecimalEulerExp(ExpX, Num, Precision + 5);

    // 计算 e^(-x) = 1 / e^x
    ExpNegX.SetOne;
    BigDecimalDiv(ExpNegX, ExpNegX, ExpX, Precision + 5);

    // sinh(x) = (e^x - e^(-x)) / 2
    BigDecimalSub(Res, ExpX, ExpNegX);
    Res.DivWord(2, Precision);

    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(ExpX);
    FLocalBigDecimalPool.Recycle(ExpNegX);
  end;
end;

function BigDecimalHyperbolicCos(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Precision: Integer = 0): Boolean;
{
  双曲余弦：cosh(x) = (e^x + e^(-x)) / 2
}
var
  ExpX, ExpNegX: TCnBigDecimal;
begin
  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  ExpX := FLocalBigDecimalPool.Obtain;
  ExpNegX := FLocalBigDecimalPool.Obtain;

  try
    BigDecimalEulerExp(ExpX, Num, Precision + 5);

    ExpNegX.SetOne;
    BigDecimalDiv(ExpNegX, ExpNegX, ExpX, Precision + 5);

    // cosh(x) = (e^x + e^(-x)) / 2
    BigDecimalAdd(Res, ExpX, ExpNegX);
    Res.DivWord(2, Precision);

    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(ExpX);
    FLocalBigDecimalPool.Recycle(ExpNegX);
  end;
end;

function BigComplexDecimalEulerExp(Res, Num: TCnBigComplexDecimal;
  Precision: Integer = 0): Boolean;
{
  复数指数函数：e^(a+bi) = e^a * (cos(b) + i*sin(b))

  算法：
  1. 分离实部和虚部：z = a + bi
  2. 计算 e^a
  3. 计算 cos(b) 和 sin(b)
  4. 结果 = e^a * cos(b) + i * e^a * sin(b)
}
var
  ExpA, CosB, SinB: TCnBigDecimal;
  TargetPrecision: Integer;
begin
  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  TargetPrecision := Precision + 10;  // 内部计算使用更高精度

  // 特殊情况：如果是纯实数
  if Num.IsPureReal then
  begin
    BigDecimalEulerExp(Res.R, Num.R, Precision);
    Res.I.SetZero;
    Result := True;
    Exit;
  end;

  // 特殊情况：如果是纯虚数 e^(bi) = cos(b) + i*sin(b)
  if Num.IsPureImaginary then
  begin
    BigDecimalCos(Res.R, Num.I, Precision);
    BigDecimalSin(Res.I, Num.I, Precision);
    Result := True;
    Exit;
  end;

  ExpA := FLocalBigDecimalPool.Obtain;
  CosB := FLocalBigDecimalPool.Obtain;
  SinB := FLocalBigDecimalPool.Obtain;

  try
    // 计算 e^a（实部指数）
    BigDecimalEulerExp(ExpA, Num.R, TargetPrecision);

    // 计算 cos(b) 和 sin(b)（虚部的三角函数）
    BigDecimalCos(CosB, Num.I, TargetPrecision);
    BigDecimalSin(SinB, Num.I, TargetPrecision);

    // 结果 = e^a * cos(b) + i * e^a * sin(b)
    BigDecimalMul(Res.R, ExpA, CosB, Precision);
    BigDecimalMul(Res.I, ExpA, SinB, Precision);

    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(ExpA);
    FLocalBigDecimalPool.Recycle(CosB);
    FLocalBigDecimalPool.Recycle(SinB);
  end;
end;

function BigComplexDecimalSin(Res, Num: TCnBigComplexDecimal;
  Precision: Integer): Boolean;
{
  复数正弦：sin(z) = (e^(iz) - e^(-iz)) / (2i)

  使用公式：sin(a+bi) = sin(a)cosh(b) + i*cos(a)sinh(b)
}
var
  A, B: TCnBigDecimal;
  SinA, CosA, SinhB, CoshB: TCnBigDecimal;
  TargetPrecision: Integer;
begin
  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  TargetPrecision := Precision + 10;

  // 特殊情况：纯实数
  if Num.IsPureReal then
  begin
    BigDecimalSin(Res.R, Num.R, Precision);
    Res.I.SetZero;
    Result := True;
    Exit;
  end;

  A := FLocalBigDecimalPool.Obtain;
  B := FLocalBigDecimalPool.Obtain;
  SinA := FLocalBigDecimalPool.Obtain;
  CosA := FLocalBigDecimalPool.Obtain;
  SinhB := FLocalBigDecimalPool.Obtain;
  CoshB := FLocalBigDecimalPool.Obtain;

  try
    BigDecimalCopy(A, Num.R);
    BigDecimalCopy(B, Num.I);

    // 使用公式：sin(a+bi) = sin(a)cosh(b) + i*cos(a)sinh(b)

    // 计算 sin(a) 和 cos(a)
    BigDecimalSin(SinA, A, TargetPrecision);
    BigDecimalCos(CosA, A, TargetPrecision);

    // 计算 sinh(b) 和 cosh(b)
    BigDecimalHyperbolicSin(SinhB, B, TargetPrecision);
    BigDecimalHyperbolicCos(CoshB, B, TargetPrecision);

    // 实部 = sin(a) * cosh(b)
    BigDecimalMul(Res.R, SinA, CoshB, Precision);

    // 虚部 = cos(a) * sinh(b)
    BigDecimalMul(Res.I, CosA, SinhB, Precision);

    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(A);
    FLocalBigDecimalPool.Recycle(B);
    FLocalBigDecimalPool.Recycle(SinA);
    FLocalBigDecimalPool.Recycle(CosA);
    FLocalBigDecimalPool.Recycle(SinhB);
    FLocalBigDecimalPool.Recycle(CoshB);
  end;
end;

function BigComplexDecimalCos(Res, Num: TCnBigComplexDecimal;
  Precision: Integer = 0): Boolean;
{
  复数余弦：cos(z) = (e^(iz) + e^(-iz)) / 2

  使用公式：cos(a+bi) = cos(a)cosh(b) - i*sin(a)sinh(b)
}
var
  A, B: TCnBigDecimal;
  SinA, CosA, SinhB, CoshB: TCnBigDecimal;
  TargetPrecision: Integer;
begin
  if Precision <= 0 then
    Precision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  TargetPrecision := Precision + 10;

  if Num.IsPureReal then
  begin
    BigDecimalCos(Res.R, Num.R, Precision);
    Res.I.SetZero;
    Result := True;
    Exit;
  end;

  A := FLocalBigDecimalPool.Obtain;
  B := FLocalBigDecimalPool.Obtain;
  SinA := FLocalBigDecimalPool.Obtain;
  CosA := FLocalBigDecimalPool.Obtain;
  SinhB := FLocalBigDecimalPool.Obtain;
  CoshB := FLocalBigDecimalPool.Obtain;

  try
    BigDecimalCopy(A, Num.R);
    BigDecimalCopy(B, Num.I);

    BigDecimalSin(SinA, A, TargetPrecision);
    BigDecimalCos(CosA, A, TargetPrecision);
    BigDecimalHyperbolicSin(SinhB, B, TargetPrecision);
    BigDecimalHyperbolicCos(CoshB, B, TargetPrecision);

    // 实部 = cos(a) * cosh(b)
    BigDecimalMul(Res.R, CosA, CoshB, Precision);

    // 虚部 = -sin(a) * sinh(b)
    BigDecimalMul(Res.I, SinA, SinhB, Precision);
    Res.I.Negate;

    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(A);
    FLocalBigDecimalPool.Recycle(B);
    FLocalBigDecimalPool.Recycle(SinA);
    FLocalBigDecimalPool.Recycle(CosA);
    FLocalBigDecimalPool.Recycle(SinhB);
    FLocalBigDecimalPool.Recycle(CoshB);
  end;
end;

initialization
  FLocalBigDecimalPool := TCnBigDecimalPool.Create;

finalization
  FLocalBigDecimalPool.Free;

end.
