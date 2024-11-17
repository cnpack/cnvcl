{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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
* 备    注：旨在脱离 Math 库，先不太管运行效率
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2021.12.08 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

const
  CN_PI = 3.1415926535897932384626;
  CN_FLOAT_DEFAULT_DIGIT = 10;

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
                  B4 + ... ---
                            Bn
}
// function Int64ContinuedFraction

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

function GaussLegendrePi(RoundCount: Integer = 8): string;
{* 大浮点数用高斯勒让德公式计算 Pi，8 次迭代精度就到了 100 多位，12 轮耗时 5 秒。

   参数：
     RoundCount: Integer                  - 计算轮数

   返回值：string                         - 返回的 Pi 值字符串
}

function XavierGourdonEuler(BlockSize: Integer = 1000): string;
{* 用 Xavier Gourdon 法计算欧拉常数 e 的值，参数为计算轮数。

   参数：
     BlockSize: Integer                   - 计算轮数

   返回值：string                         - 返回的 e 值字符串
}

function FloatAlmostZero(F: Extended): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 判断一浮点数是否离 0 足够近。

   参数：
     F: Extended                          - 待判断的浮点数

   返回值：Boolean                        - 是否离 0 足够近
}

function FloatEqual(A: Extended; B: Extended): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 封装的两个浮点数是否相等的判断。

   参数：
     A: Extended                          - 待判断的浮点数一
     B: Extended                          - 待判断的浮点数二

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

implementation

uses
  CnBigDecimal, CnNative;

const
  SCN_FLOAT_GAP = 0.000001;         // 普通浮点判断
  SCN_EXTEND_GAP = 0.00000000001;   // 本单元中的迭代计算差值
  SCN_LOGN_TO_LOG2 = 1.4426950408889634073599246810019;
  SCN_LOGN_TO_LOG10 = 0.43429448190325182765112891891661;

resourcestring
  SCnErrorMathSqrtRange = 'Sqrt Range Error.';
  SCnErrorMathLogRange = 'Log Range Error.';

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

{$HINTS OFF}

function Int64Sqrt(N: Int64): Extended;
var
  X0: Extended;
begin
  if N < 0 then
    raise ERangeError.Create(SCnErrorMathSqrtRange);

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
    raise ERangeError.Create(SCnErrorMathSqrtRange);

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

function GaussLegendrePi(RoundCount: Integer): string;
var
  I, P: Integer;
  A0, B0, T0, P0: TCnBigDecimal;
  A1, B1, T1, P1: TCnBigDecimal;
  X1, X2: TCnBigDecimal;
  Res: TCnBigDecimal;
begin
  A0 := nil;
  B0 := nil;
  T0 := nil;
  P0 := nil;

  A1 := nil;
  B1 := nil;
  T1 := nil;
  P1 := nil;

  Res := nil;
  X1 := nil;
  X2 := nil;

  try
    A0 := TCnBigDecimal.Create;
    B0 := TCnBigDecimal.Create;
    T0 := TCnBigDecimal.Create;
    P0 := TCnBigDecimal.Create;

    A1 := TCnBigDecimal.Create;
    B1 := TCnBigDecimal.Create;
    T1 := TCnBigDecimal.Create;
    P1 := TCnBigDecimal.Create;

    Res := TCnBigDecimal.Create;

    // 临时变量
    X1 := TCnBigDecimal.Create;
    X1.SetWord(2);
    X2 := TCnBigDecimal.Create;

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

    Result := Res.ToString;
  finally
    X1.Free;
    X2.Free;

    Res.Free;

    A1.Free;
    B1.Free;
    T1.Free;
    P1.Free;

    A0.Free;
    B0.Free;
    T0.Free;
    P0.Free;
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

function FloatAlmostZero(F: Extended): Boolean;
{$IFDEF SUPPORT_INLINE}
const
  SCN_FLOAT_GAP = 0.000001; // inline 不能使用外边的常量
{$ENDIF}
begin
  Result := CnAbs(F) < SCN_FLOAT_GAP;
end;

function FloatEqual(A: Extended; B: Extended): Boolean;
begin
  Result := FloatAlmostZero(A - B);
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
  I := $5f375a86 - (I shr 1);
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

end.
