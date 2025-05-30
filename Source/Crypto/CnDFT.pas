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

unit CnDFT;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：基于浮点复数的离散傅立叶变换以及基于 Int 64 的快速数论变换实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了基于浮点复数的离散傅立叶变换以及基于 Int 64 的快速数论变换
*           使用快速傅立叶变换实现离散傅立叶变换可以以加速多项式乘法，但因浮点存在会损失精度
*           使用快速数论变换则没这个问题。但快速数论变换也有限制：
*           一是多项式系数必须为正数并小于模数（负的系数还不知道如何处理），
*           二是多项式项数必须小于 2^23（本单元模数限制）。
*
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.06.29 V1.2
*               增加一二维离散余弦变换及其逆变换
*           2021.08.29 V1.1
*               增加快速数论变换，使用特定素数
*           2020.11.23 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnComplex, CnMatrix;

procedure ButterflyChangeComplex(CA: PCnComplexArray; Len: Integer);
{* 蝴蝶变换，调整复数数组内部元素的顺序以便奇偶分治。

   参数：
     CA: PCnComplexArray                  - 指向待进行蝴蝶变换的复数数组
     Len: Integer                         - 数组里的复数个数

   返回值：（无）
}

procedure ButterflyChangeInt64(IA: PInt64Array; Len: Integer);
{* 蝴蝶变换，调整 Int64 数组内部元素的顺序以便奇偶分治。

   参数：
     IA: PInt64Array                      - 指向待进行蝴蝶变换的 Int64 数组
     Len: Integer                         - 数组里的 Int64 个数

   返回值：（无）
}

function CnFFT(Data: PCnComplexArray; Len: Integer): Boolean;
{* 快速傅立叶变换，将多项式的系数复数数组转换为点值向量复数数组，要确保 Len 为 2 的整数次幂。

   参数：
     Data: PCnComplexArray                - 指向待进行快速傅立叶变换的复数数组
     Len: Integer                         - 数组里的复数个数，必须是 2 的整数次幂

   返回值：Boolean                        - 返回变换是否成功
}

function CnIFFT(Data: PCnComplexArray; Len: Integer): Boolean;
{* 快速傅立叶逆变换，将点值向量复数数组转换为多项式的系数复数数组，要确保 Len 为 2 的整数次幂。

   参数：
     Data: PCnComplexArray                - 指向待进行快速傅立叶逆变换的复数数组
     Len: Integer                         - 数组里的复数个数，必须是 2 的整数次幂

   返回值：Boolean                        - 返回逆变换是否成功
}

function CnNTT(Data: PInt64Array; Len: Integer): Boolean;
{* 快速数论变换，将多项式的系数 int 64 数组转换为点值向量 int64 数组，
   注意要确保 Len 为 2 的整数次幂，并且 Data 各系数必须大于 0 且小于 CN_P。

   参数：
     Data: PInt64Array                    - 指向待进行快速数论变换的 Int64 数组
     Len: Integer                         - 数组里的 Int64 个数，，必须是 2 的整数次幂

   返回值：Boolean                        - 返回变换是否成功
}

function CnINTT(Data: PInt64Array; Len: Integer): Boolean;
{* 快速数论逆变换，将点值向量 int 64 数组转换为多项式的系数 int 64 数组，
   注意要确保 Len 为 2 的整数次幂，并且 Data 各系数必须大于 0 且小于 CN_P。

   参数：
     Data: PInt64Array                    - 指向待进行快速数论逆变换的 Int64 数组
     Len: Integer                         - 数组里的 Int64 个数，，必须是 2 的整数次幂

   返回值：Boolean                        - 返回逆变换是否成功
}

function CnDCT(Data: PExtendedArray; Res: PExtendedArray; Len: Integer): Boolean;
{* 一维 DCT 变换（离散余弦），将 Data 所指的浮点数组做一次一维离散余弦变换，
   结果放入 Res 所指的浮点数组中，要求数组长度均为 Len，返回变换是否成功。

   参数：
     Data: PExtendedArray                 - 指向待进行一维 DCT 变换的浮点数组
     Res: PExtendedArray                  - 指向变换结果的浮点数组
     Len: Integer                         - 数组里的浮点数个数

   返回值：Boolean                        - 返回变换是否成功
}

function CnIDCT(Data: PExtendedArray; Res: PExtendedArray; Len: Integer): Boolean;
{* 一维逆 DCT 变换（离散余弦），将 Data 所指的浮点数组做一次一维逆离散余弦变换，
   结果放入 Res 所指的浮点数组中，要求数组长度均为 Len，返回逆变换是否成功。

   参数：
     Data: PExtendedArray                 - 指向待进行一维逆 DCT 变换的浮点数组
     Res: PExtendedArray                  - 指向逆变换结果的浮点数组
     Len: Integer                         - 数组里的浮点数个数

   返回值：Boolean                        - 返回逆变换是否成功
}

function CnGenerateDCT2Matrix(M: TCnFloatMatrix; N: Integer): Boolean;
{* 生成 N 阶二维 DCT 变换矩阵，该矩阵为方阵。

   参数：
     M: TCnFloatMatrix                    - 待生成的二维 DCT 变换矩阵
     N: Integer                           - 矩阵的阶

   返回值：Boolean                        - 返回生成是否成功
}

function CnDCT2(Data: TCnFloatMatrix; Res: TCnFloatMatrix; DCTM: TCnFloatMatrix = nil;
  DCTMT: TCnFloatMatrix = nil; T: TCnFloatMatrix = nil): Boolean;
{* 二维 DCT 变换（离散余弦），将 Data 所代表的浮点矩阵做一次二维离散余弦变换，
   结果放入 Res 所代表的浮点矩阵中，要求各矩阵均为方阵且尺寸相等，
   DCTM/DCTMT 可以为预计算的变换矩阵与其转置矩阵，T 为临时计算矩阵，返回变换是否成功。

   参数：
     Data: TCnFloatMatrix                 - 待进行二维 DCT 变换的浮点矩阵
     Res: TCnFloatMatrix                  - 结果矩阵
     DCTM: TCnFloatMatrix                 - 预计算的变换矩阵
     DCTMT: TCnFloatMatrix                - 预计算的变换矩阵的转置矩阵
     T: TCnFloatMatrix                    - 临时计算矩阵

   返回值：Boolean                        - 返回变换是否成功
}

function CnIDCT2(Data: TCnFloatMatrix; Res: TCnFloatMatrix; DCTM: TCnFloatMatrix = nil;
  DCTMT: TCnFloatMatrix = nil; T: TCnFloatMatrix = nil): Boolean;
{* 二维逆 DCT 变换（离散余弦），将 Data 所代表的浮点矩阵做一次二维逆离散余弦变换，
   结果放入 Res 所代表的浮点矩阵中，要求各矩阵均为方阵且尺寸相等，
   DCTM/DCTMT 可以为预计算的变换矩阵与其转置矩阵，T 为临时计算矩阵，返回逆变换是否成功。

   参数：
     Data: TCnFloatMatrix                 - 待进行二维逆 DCT 变换的浮点矩阵
     Res: TCnFloatMatrix                  - 结果矩阵
     DCTM: TCnFloatMatrix                 - 预计算的变换矩阵
     DCTMT: TCnFloatMatrix                - 预计算的变换矩阵的转置矩阵
     T: TCnFloatMatrix                    - 临时计算矩阵

   返回值：Boolean                        - 返回逆变换是否成功
}

implementation

uses
  CnPrime;

const
  Pi = 3.1415926535897932384626;

  CN_NR = 1 shl 22;     // 2 的 23 次方的一半，最多只能处理次数为 CN_NR 的多项式
  CN_G = 3;             // 下面素数的原根是 3
  CN_G_INV = 332748118; // 该原根对该素数的逆元为 332748118
  CN_P = 998244353;     // 选取素数为 998244353 = 2^23*119 + 1，小于 Int32 的最大值 2147483647

// 蝴蝶变换，调整数组内部元素的顺序，要确保 Len 为 2 的整数次幂
procedure ButterflyChangeComplex(CA: PCnComplexArray; Len: Integer);
var
  I: Integer;
  R: array of Integer;
begin
  if Len <= 1 then
    Exit;

  SetLength(R, Len);
  for I := 0 to Len - 1 do
  begin
    R[I] := R[I shr 1] shr 1;
    if (I and 1) <> 0 then
      R[I] := R[I] or (Len shr 1);
  end;

  for I := 0 to Len - 1 do
  begin
    if I < R[I] then
      ComplexNumberSwap(CA^[I], CA^[R[I]]);
  end;
  SetLength(R, 0);
end;

// 蝴蝶变换，调整数组内部元素的顺序，要确保 Len 为 2 的整数次幂
procedure ButterflyChangeInt64(IA: PInt64Array; Len: Integer);
var
  I: Integer;
  R: array of Integer;
  T: Int64;
begin
  if Len <= 1 then
    Exit;

  SetLength(R, Len);
  for I := 0 to Len - 1 do
  begin
    R[I] := R[I shr 1] shr 1;
    if (I and 1) <> 0 then
      R[I] := R[I] or (Len shr 1);
  end;

  for I := 0 to Len - 1 do
  begin
    if I < R[I] then
    begin
      T := IA^[I];
      IA^[I] := IA^[R[I]];
      IA^[R[I]] := T;
    end;
  end;
  SetLength(R, 0);
end;

// 迭代非递归方式实现的快速傅立叶变换及其逆变换
function FFT(Data: PCnComplexArray; Len: Integer; IsReverse: Boolean): Boolean;
var
  J, T, M, R, K: Integer;
  WN, W, X, Y: TCnComplexNumber;
begin
  Result := False;
  if (Data = nil) or (Len <= 0) then
    Exit;

  // Len 必须 2 的整数次幂
  if not IsUInt32PowerOf2(Cardinal(Len)) then
    Exit;

  if IsReverse then
    T := -1
  else
    T := 1;

  ButterflyChangeComplex(Data, Len);

  M := 1;
  while M < Len do
  begin
    WN.R := Cos(Pi / M);
    WN.I := Sin(Pi / M) * T;

    J := 0;
    R := M shl 1;
    while J < Len do
    begin
      W.R := 1.0;
      W.I := 0;

      K := 0;
      while K < M do
      begin
        ComplexNumberCopy(X, Data^[J + K]);
        ComplexNumberMul(Y, Data^[J + K + M], W);

        ComplexNumberAdd(Data^[J + K], X, Y);
        ComplexNumberSub(Data^[J + K + M], X, Y);

        ComplexNumberMul(W, W, WN);
        Inc(K);
      end;

      J := J + R;
    end;

    M := M shl 1;
  end;

  if IsReverse then
    for J := 0 to Len - 1 do
      ComplexNumberDiv(Data^[J], Data^[J], Len);

  Result := True;
end;

function CnFFT(Data: PCnComplexArray; Len: Integer): Boolean;
begin
  Result := FFT(Data, Len, False);
end;

function CnIFFT(Data: PCnComplexArray; Len: Integer): Boolean;
begin
  Result := FFT(Data, Len, True);
end;

// 迭代非递归方式实现的快速数论变换及其逆变换
function NTT(Data: PInt64Array; Len: Integer; IsReverse: Boolean): Boolean;
var
  M, K, J, R: Integer;
  G0, GN, X, Y: Int64;
begin
  Result := False;
  if (Data = nil) or (Len <= 0) or (Len > CN_NR) then
    Exit;

  // Len 必须 2 的整数次幂
  if not IsUInt32PowerOf2(Cardinal(Len)) then
    Exit;

  ButterflyChangeInt64(Data, Len);

  M := 1;
  while M < Len do
  begin
    // MontgomeryPowerMod 会把负的 Int64 作为正的无符号 UInt64，但这里各系数都为正，可以使用
    if IsReverse then
      GN := MontgomeryPowerMod(CN_G_INV, (CN_P - 1) div (M shl 1), CN_P)
    else
      GN := MontgomeryPowerMod(CN_G, (CN_P - 1) div (M shl 1) , CN_P);

    J := 0;
    R := M shl 1;
    while J < Len do
    begin
      G0 := 1;
      K := 0;

      while K < M do
      begin
        X := Data^[J + K];
        Y := Int64MultipleMod(G0, Data^[J + K + M], CN_P);
        Data^[J + K] := Int64AddMod(X, Y, CN_P);

        X := X - Y;
        if X < 0 then
          X := X + CN_P; // X - Y 可能是负数，不能用 AddMod
        Data^[J + K + M] := X mod CN_P;

        G0 := Int64MultipleMod(G0, GN, CN_P);
        Inc(K);
      end;

      J := J + R;
    end;

    M := M shl 1;
  end;

  if IsReverse then
    for J := 0 to Len - 1 do
      Data^[J] := Data^[J] div Len;

  Result := True;
end;

function CnNTT(Data: PInt64Array; Len: Integer): Boolean;
begin
  Result := NTT(Data, Len, False);
end;

function CnINTT(Data: PInt64Array; Len: Integer): Boolean;
begin
  Result := NTT(Data, Len, True);
end;

function CnDCT(Data, Res: PExtendedArray; Len: Integer): Boolean;
var
  X, U: Integer;
  C: Extended;
begin
  Result := False;
  if (Len <= 0) or (Data = nil) or (Res = nil) then
    Exit;

  Res^[0] := 0;
  for X := 0 to Len - 1 do
    Res^[0] := Res^[0] + Data^[X];

  Res^[0] := Res^[0] / Sqrt(Len); // 额外求得 F0

  for U := 1 to Len - 1 do
  begin
    // 求 FU
    Res^[U] := 0;
    for X := 0 to Len - 1 do
    begin
      C := Cos(Pi * U * (2 * X + 1) / (2 * Len));
      Res^[U] := Res^[U] + Data^[X] * C;
    end;
    Res^[U] := Res^[U] * Sqrt(2.0 / Len);
  end;
  Result := True;
end;

function CnIDCT(Data, Res: PExtendedArray; Len: Integer): Boolean;
var
  X, U: Integer;
  A1, A2, C: Extended;
begin
  Result := False;
  if (Len <= 0) or (Data = nil) or (Res = nil) then
    Exit;

  A1 := 1.0 / Sqrt(Len);
  A2 := Sqrt(2.0 / Len);

  for X := 0 to Len - 1 do
  begin
    // 求 fx
    Res^[X] := 0;
    for U := 0 to Len - 1 do
    begin
      C := Cos(Pi * U * (2 * X + 1) / (2 * Len));
      if U = 0 then
        Res^[X] := Res^[X] + Data^[U] * C * A1
      else
        Res^[X] := Res^[X] + Data^[U] * C * A2;
    end;
  end;
  Result := True;
end;

function CnGenerateDCT2Matrix(M: TCnFloatMatrix; N: Integer): Boolean;
var
  I, J: Integer;
  A1, A2: Extended;
begin
  Result := False;
  if (M = nil) or (N < 2) then
    Exit;

  M.RowCount := N;
  M.ColCount := N;

  A1 := 1.0 / Sqrt(N);
  A2 := Sqrt(2.0 / N);

  for I := 0 to M.RowCount - 1 do
  begin
    for J := 0 to M.ColCount - 1 do
    begin
      M.Value[I, J] := Cos(I * (J + 0.5) * Pi / N);

      if I = 0 then
        M.Value[I, J] := M.Value[I, J] * A1
      else
        M.Value[I, J] := M.Value[I, J] * A2;
    end;
  end;
  Result := True;
end;

function CnDCT2(Data, Res: TCnFloatMatrix; DCTM: TCnFloatMatrix;
  DCTMT: TCnFloatMatrix; T: TCnFloatMatrix): Boolean;
var
  MIsNil, MTIsNil, TIsNil: Boolean;
begin
  // Res := M * Data * M'
  Result := False;
  if (Data = nil) or (Res = nil) then
    Exit;

  if Data.RowCount <> Data.ColCount then
    Exit;

  MIsNil := DCTM = nil;
  MTIsNil := DCTMT = nil;
  TIsNil := T = nil;

  try
    if MIsNil then
    begin
      DCTM := TCnFloatMatrix.Create;
      CnGenerateDCT2Matrix(DCTM, Data.RowCount);
    end;

    if MTIsNil then
    begin
      DCTMT := TCnFloatMatrix.Create;
      CnMatrixTranspose(DCTM, DCTMT);
    end;

    if TIsNil then
      T := TCnFloatMatrix.Create;

    CnMatrixMul(DCTM, Data, T);
    CnMatrixMul(T, DCTMT, Res);

    Result := True;
  finally
    if TIsNil then
      T.Free;
    if MIsNil then
      DCTM.Free;
    if MTIsNil then
      DCTMT.Free;
  end;
end;

function CnIDCT2(Data, Res: TCnFloatMatrix; DCTM: TCnFloatMatrix;
  DCTMT: TCnFloatMatrix; T: TCnFloatMatrix): Boolean;
var
  MIsNil, MTIsNil, TIsNil: Boolean;
begin
  // Res := M' * Data * M
  Result := False;
  if (Data = nil) or (Res = nil) then
    Exit;

  if Data.RowCount <> Data.ColCount then
    Exit;

  MIsNil := DCTM = nil;
  MTIsNil := DCTMT = nil;
  TIsNil := T = nil;

  try
    if MIsNil then
    begin
      DCTM := TCnFloatMatrix.Create;
      CnGenerateDCT2Matrix(DCTM, Data.RowCount);
    end;

    if MTIsNil then
    begin
      DCTMT := TCnFloatMatrix.Create;
      CnMatrixTranspose(DCTM, DCTMT);
    end;

    if TIsNil then
      T := TCnFloatMatrix.Create;

    CnMatrixMul(DCTMT, Data, T);
    CnMatrixMul(T, DCTM, Res);

    Result := True;
  finally
    if TIsNil then
      T.Free;
    if MIsNil then
      DCTM.Free;
    if MTIsNil then
      DCTMT.Free;
  end;
end;

end.
