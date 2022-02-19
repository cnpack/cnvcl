{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDFT;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：基于浮点复数的离散傅立叶变换以及基于 Int 64 的快速数论变换实现单元
* 单元作者：刘啸
* 备    注：使用快速傅立叶变换实现离散傅立叶变换以加速多项式乘法但因浮点存在会损失精度
*           使用快速数论变换则没这个问题。但快速数论变换也有限制：
*           1、多项式系数必须为正数并小于模数（负的系数还不知道咋搞）。
*           2、多项式项数小于 2^23（本单元模数限制）
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2021.08.29 V1.1
*               增加快速数论变换，使用特定素数
*           2020.11.23 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNativeDecl, CnComplex;

procedure ButterflyChangeComplex(CA: PCnComplexArray; Len: Integer);
{* 蝴蝶变换，调整复数数组内部元素的顺序以便奇偶分治}

procedure ButterflyChangeInt64(IA: PInt64Array; Len: Integer);
{* 蝴蝶变换，调整 int64 数组内部元素的顺序以便奇偶分治}

function CnFFT(Data: PCnComplexArray; Len: Integer): Boolean;
{* 快速傅立叶变换，将多项式的系数复数数组转换为点值向量复数数组，要确保 Len 为 2 的整数次幂}

function CnIFFT(Data: PCnComplexArray; Len: Integer): Boolean;
{* 快速傅立叶逆变换，将点值向量复数数组转换为多项式的系数复数数组，要确保 Len 为 2 的整数次幂}

function CnNTT(Data: PInt64Array; Len: Integer): Boolean;
{* 快速数论变换，将多项式的系数 int 64 数组转换为点值向量 int64 数组，
  注意要确保 Len 为 2 的整数次幂，并且 Data 各系数必须大于 0 且小于 CN_P}

function CnINTT(Data: PInt64Array; Len: Integer): Boolean;
{* 快速数论逆变换，将点值向量 int 64 数组转换为多项式的系数 int 64 数组，
  注意要确保 Len 为 2 的整数次幂，并且 Data 各系数必须大于 0 且小于 CN_P}

implementation

uses
  CnPrimeNumber;

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

end.
