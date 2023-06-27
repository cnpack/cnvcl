{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnComplex;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：浮点复数实现单元
* 单元作者：刘啸
* 备    注：用 record 而不用 Object。支持跨平台
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.06.26 V1.1
*               增加辐角与绝对值等函数
*           2020.11.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, SysConst, Math, CnMath;

type
  ECnComplexNumberException = class(Exception);
  {* 复数相关的异常}

  TCnComplexNumber = packed record
  {* 浮点精度的复数表示结构}
    R: Extended;
    {* 实部}
    I: Extended;
    {* 虚部}
  end;
  PCnComplexNumber = ^TCnComplexNumber;
  {* 指向复数结构的指针}

  TCnComplexArray = array[0..8191] of TCnComplexNumber;
  {* 复数结构数组}

  PCnComplexArray = ^TCnComplexArray;
  {* 指向复数结构数组的指针}

function ComplexNumberIsZero(var Complex: TCnComplexNumber): Boolean;
{* 返回复数是否为 0}

procedure ComplexNumberSetZero(var Complex: TCnComplexNumber);
{* 复数置 0}

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  AR, AI: Extended); overload;
{* 复数赋值}

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  const AR, AI: string); overload;
{* 复数赋值}

function ComplexNumberToString(var Complex: TCnComplexNumber): string;
{* 复数转换为字符串}

function ComplexNumberEqual(var Complex1, Complex2: TCnComplexNumber): Boolean;
{* 判断两个复数结构是否相等}

procedure ComplexNumberSwap(var Complex1, Complex2: TCnComplexNumber);
{* 交换两个复数值}

procedure ComplexNumberCopy(var Dst, Src: TCnComplexNumber);
{* 复数复制值}

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber); overload;
{* 复数加法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2}

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber); overload;
{* 复数减法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2}

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber); overload;
{* 复数乘法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2}

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber); overload;
{* 复数除法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2}

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的加法，Complex 和 Res 可以是同一个结构}

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的减法，Complex 和 Res 可以是同一个结构}

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的乘法，Complex 和 Res 可以是同一个结构}

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的除法，Complex 和 Res 可以是同一个结构}

procedure ComplexConjugate(var Res, Complex: TCnComplexNumber);
{* 获得共轭复数，Res 可以是 Complex}

function ComplexIsPureReal(var Complex: TCnComplexNumber): Boolean;
{* 复数是否纯实数，也就是判断虚部是否为 0}

function ComplexIsPureImaginary(var Complex: TCnComplexNumber): Boolean;
{* 复数是否纯虚数，也就是判断实部是否为 0 且虚部不为 0}

function ComplexNumberAbsolute(var Complex: TCnComplexNumber): Extended;
{* 返回复数的绝对值，也即距复平面原点的距离}

function ComplexNumberArgument(var Complex: TCnComplexNumber): Extended;
{* 返回复数的辐角主值，也即与复平面正 X 轴的夹角，范围在 0 到 2π}

implementation

function ComplexNumberIsZero(var Complex: TCnComplexNumber): Boolean;
begin
  Result := (Complex.R = 0) and (Complex.I = 0);
end;

procedure ComplexNumberSetZero(var Complex: TCnComplexNumber);
begin
  Complex.R := 0.0;
  Complex.I := 0.0;
end;

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber; AR, AI: Extended);
begin
  Complex.R := AR;
  Complex.I := AI;
end;

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  const AR, AI: string);
begin
  ComplexNumberSetZero(Complex);
  if (AR = '') and (AI = '') then
    Exit
  else if AR = '' then
    Complex.I := StrToFloat(AI)
  else if AI = '' then
    Complex.R := StrToFloat(AR)
  else
    ComplexNumberSetValue(Complex, StrToFloat(AR), StrToFloat(AI));
end;

function ComplexNumberToString(var Complex: TCnComplexNumber): string;
begin
  if ComplexIsPureReal(Complex) then
    Result := Format('%f', [Complex.R])
  else if ComplexIsPureImaginary(Complex) then
    Result := Format('%fi', [Complex.I])
  else if Complex.I < 0 then
    Result := Format('%f%fi', [Complex.R, Complex.I])
  else
    Result := Format('%f+%fi', [Complex.R, Complex.I]);
end;

function ComplexNumberEqual(var Complex1, Complex2: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex1.R, Complex2.R) and FloatEqual(Complex1.I, Complex2.I);
end;

procedure ComplexNumberSwap(var Complex1, Complex2: TCnComplexNumber);
var
  T: Extended;
begin
  T := Complex1.R;
  Complex1.R := Complex2.R;
  Complex2.R := T;

  T := Complex1.I;
  Complex1.I := Complex2.I;
  Complex2.I := T;
end;

procedure ComplexNumberCopy(var Dst, Src: TCnComplexNumber);
begin
  Dst.R := Src.R;
  Dst.I := Src.I;
end;

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
begin
  Res.R := Complex1.R + Complex2.R;
  Res.I := Complex1.I + Complex2.I;
end;

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
begin
  Res.R := Complex1.R - Complex2.R;
  Res.I := Complex1.I - Complex2.I;
end;

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
var
  T: Extended;
begin
  T := Complex1.R * Complex2.R - Complex1.I * Complex2.I;
  Res.I := Complex1.R * Complex2.I + Complex1.I * Complex2.R;
  Res.R := T;
end;

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
var
  T, D: Extended;
begin
  D := Complex2.R * Complex2.R + Complex2.I * Complex2.I;
  if FloatEqual(D, 0.0) then
    raise EZeroDivide.Create(SZeroDivide);

  T := (Complex1.R * Complex2.R + Complex1.I * Complex2.I) / D;
  Res.I := (Complex1.I * Complex2.R - Complex1.R * Complex2.I) / D;
  Res.R := T;
end;

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R + Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R - Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R * Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R / Value;
  Res.I := Complex.I;
end;

procedure ComplexConjugate(var Res, Complex: TCnComplexNumber);
begin
  Res.R := Complex.R;
  Res.I := -Complex.I;
end;

function ComplexIsPureReal(var Complex: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex.I, 0.0);
end;

function ComplexIsPureImaginary(var Complex: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex.R, 0.0) and not FloatEqual(Complex.I, 0.0);
end;

function ComplexNumberAbsolute(var Complex: TCnComplexNumber): Extended;
begin
  Result := Sqrt(Complex.R * Complex.R + Complex.I * Complex.I);
end;

function ComplexNumberArgument(var Complex: TCnComplexNumber): Extended;
begin
  if Complex.I = 0 then
  begin
    if Complex.R >= 0 then     // 正实数辐角返回 0，包括 0 也凑合着返回 0
      Result := 0
    else
      Result := CN_PI;         // 复实数辐角返回 π
  end
  else if Complex.R = 0 then
  begin
    if Complex.I > 0 then      // 正纯虚数辐角返回半 π
      Result := CN_PI / 2
    else
      Result := CN_PI + CN_PI / 2;   // 复纯虚数辐角返回 3π/2
  end
  else // 实部虚部均不为 0
  begin
    Result := ArcTan2(Complex.I, Complex.R);
    if Result < 0 then
      Result := Result + CN_PI * 2;
  end;
end;

end.
