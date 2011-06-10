{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

unit CnFitCurve;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：曲线拟合类
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：Win7 + Delphi 7.0
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: $
* 修改记录：2011.06.10 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Math, Contnrs;

type
  TCnFitCurve = class
  {* 通过多个点拟合一条曲线的类，内部使用三次曲线进行分段拟合 }
  private
    FPoints: TObjectList;
    FCurves: TObjectList;
    FChanged: Boolean;
  protected
    procedure CalcCurves;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    {* 清空所有点 }
    procedure AddPoint(x, y: Double);
    {* 添加一个坐标点。注意 x 为自变量，不应重复 }
    function Calc(x: Double): Double;
    {* 根据 x 坐标计算 y 坐标 }
  end;

implementation

type
  TCurveData = class
  private
    // 三次曲线拟合的参数
    FC3, FC2, FC1, FC0: Double;
    FLow, FHigh: Double;
    procedure DoCalcCurve2(x, y: array of Double);
    procedure DoCalcCurve3(x, y: array of Double);
    procedure DoCalcCurve4(x, y: array of Double);
  public
    function Calc(x: Double): Double;
    procedure DoCalcCubicCurve(x, y: array of Double; cnt: Integer);
  end;

  TDoublePoint = class
    x, y: Double;
  end;

{ TCurveData }

// 根据2个互异点计算一次曲线系数
// L0(x) = (x-x1)/(x0-x1)
// L1(x) = (x-x0)/(x1-x0)
// P3(x) = L0(x)y0 + L1(x)y1
procedure TCurveData.DoCalcCurve2(x, y: array of Double);
var
  c: array[0..1, 0..1] of Double;

  procedure DoCalc2(x0, y0, x1: Double; var r: array of Double);
  var
    v: Double;
  begin
    v := y0 / (x0 - x1);
    r[0] := v;
    r[1] := -x1 * v;
  end;
begin
  DoCalc2(x[0], y[0], x[1], c[0]);
  DoCalc2(x[1], y[1], x[0], c[1]);
  FC3 := 0;
  FC2 := 0;
  FC1 := c[0][0] + c[1][0];
  FC0 := c[0][1] + c[1][1];
end;

// 根据3个互异点计算二次曲线系数
// L0(x) = (x-x1)(x-x2)/(x0-x1)(x0-x2)
//       = (x^2 -(x1+x2)x + x1x2)/(x0-x1)(x0-x2)
// L1(x) = (x-x0)(x-x2)/(x1-x0)(x1-x2)
// L2(x) = (x-x0)(x-x1)/(x2-x0)(x2-x1)
// P3(x) = L0(x)y0 + L1(x)y1 + L2(x)y2
procedure TCurveData.DoCalcCurve3(x, y: array of Double);
var
  c: array[0..2, 0..2] of Double;

  procedure DoCalc3(x0, y0, x1, x2: Double; var r: array of Double);
  var
    v: Double;
  begin
    v := y0 / ((x0 - x1) * (x0 - x2));
    r[0] := v;
    r[1] := -(x1 + x2) * v;
    r[2] := (x1 * x2) * v;
  end;
begin
  DoCalc3(x[0], y[0], x[1], x[2], c[0]);
  DoCalc3(x[1], y[1], x[0], x[2], c[1]);
  DoCalc3(x[2], y[2], x[0], x[1], c[2]);
  FC3 := 0;
  FC2 := c[0][0] + c[1][0] + c[2][0];
  FC1 := c[0][1] + c[1][1] + c[2][1];
  FC0 := c[0][2] + c[1][2] + c[2][2];
end;

// 根据4个互异点计算三次曲线系数
// L0(x) = (x-x1)(x-x2)(x-x3)/(x0-x1)(x0-x2)(x0-x3)
//       = (x^3 -(x1+x2+x3)x^2 + (x1x2 + x1x3 + x2x3)x - x1x2x3)/(x0-x1)(x0-x2)(x0-x3)
// L1(x) = (x-x0)(x-x2)(x-x3)/(x1-x0)(x1-x2)(x1-x3)
// L2(x) = (x-x0)(x-x1)(x-x3)/(x2-x0)(x2-x1)(x2-x3)
// L3(x) = (x-x0)(x-x1)(x-x2)/(x3-x0)(x3-x1)(x3-x2)
// P4(x) = L0(x)y0 + L1(x)y1 + L2(x)y2 + L3(x)y3
procedure TCurveData.DoCalcCurve4(x, y: array of Double);
var
  c: array[0..3, 0..3] of Double;

  procedure DoCalc4(x0, y0, x1, x2, x3: Double; var r: array of Double);
  var
    v: Double;
  begin
    v := y0 / ((x0 - x1) * (x0 - x2) * (x0 - x3));
    r[0] := v;
    r[1] := -(x1 + x2 + x3) * v;
    r[2] := (x1 * x2 + x1 * x3 + x2 * x3) * v;
    r[3] := -(x1 * x2 * x3) * v;
  end;
begin
  DoCalc4(x[0], y[0], x[1], x[2], x[3], c[0]);
  DoCalc4(x[1], y[1], x[0], x[2], x[3], c[1]);
  DoCalc4(x[2], y[2], x[0], x[1], x[3], c[2]);
  DoCalc4(x[3], y[3], x[0], x[1], x[2], c[3]);
  FC3 := c[0][0] + c[1][0] + c[2][0] + c[3][0];
  FC2 := c[0][1] + c[1][1] + c[2][1] + c[3][1];
  FC1 := c[0][2] + c[1][2] + c[2][2] + c[3][2];
  FC0 := c[0][3] + c[1][3] + c[2][3] + c[3][3];
end;

function TCurveData.Calc(x: Double): Double;
begin
  Result := FC3 * x * x * x + FC2 * x * x + FC1 * x + FC0;
end;

procedure TCurveData.DoCalcCubicCurve(x, y: array of Double; cnt: Integer);
begin
  if cnt = 2 then
  begin
    DoCalcCurve2(x, y);
  end
  else if cnt = 3 then
  begin
    DoCalcCurve3(x, y);
  end
  else if cnt = 4 then
  begin
    DoCalcCurve4(x, y);
  end;
end;

{ TCnFitCurve }

constructor TCnFitCurve.Create;
begin
  FPoints := TObjectList.Create;
  FCurves := TObjectList.Create;
end;

destructor TCnFitCurve.Destroy;
begin
  FPoints.Free;
  FCurves.Free;
  inherited;
end;

procedure TCnFitCurve.AddPoint(x, y: Double);
var
  P: TDoublePoint;
begin
  P := TDoublePoint.Create;
  P.x := x;
  P.y := y;
  FPoints.Add(P);
  FChanged := True;
end;

function TCnFitCurve.Calc(x: Double): Double;
var
  i: Integer;
  Curve: TCurveData;
begin
  CalcCurves;
  Result := 0;
  for i := 0 to FCurves.Count - 1 do
  begin
    Curve := TCurveData(FCurves[i]);
    if ((x >= Curve.FLow) or (i = 0)) and ((x <= Curve.FHigh) or (i = FCurves.Count - 1)) then
    begin
      Result := Curve.Calc(x);
      Exit;
    end;
  end;
end;

function DoSortPoint(Item1, Item2: Pointer): Integer;
begin
  if TDoublePoint(Item1).x > TDoublePoint(Item2).x then
    Result := 1
  else if TDoublePoint(Item1).x < TDoublePoint(Item2).x then
    Result := -1
  else
    Result := 0;
end;

procedure TCnFitCurve.CalcCurves;
var
  Curve: TCurveData;
  i, j, sidx, eidx: Integer;
  x, y: array of Double;
begin
  if not FChanged then
    Exit;

  FCurves.Clear;
  FPoints.Sort(DoSortPoint);
  for i := FPoints.Count - 1 downto 1 do
    if TDoublePoint(FPoints[i]).x = TDoublePoint(FPoints[i - 1]).x then
      FPoints.Delete(i);

  for i := 0 to FPoints.Count - 2 do
  begin
    sidx := Max(0, i - 1);
    if sidx = 0 then
    begin
      eidx := Min(FPoints.Count - 1, sidx + 3);
    end
    else
    begin
      eidx := Min(FPoints.Count - 1, i + 2);
      if eidx = FPoints.Count - 1 then
        sidx := Max(0, eidx - 3);
    end;
    SetLength(x, eidx - sidx + 1);
    SetLength(y, eidx - sidx + 1);
    for j := sidx to eidx do
    begin
      x[j - sidx] := TDoublePoint(FPoints[j]).x;
      y[j - sidx] := TDoublePoint(FPoints[j]).y;
    end;
    Curve := TCurveData.Create;
    Curve.FLow := TDoublePoint(FPoints[i]).x;
    Curve.FHigh := TDoublePoint(FPoints[i + 1]).x;
    Curve.DoCalcCubicCurve(x, y, eidx - sidx + 1);
    FCurves.Add(Curve);
  end;

  FChanged := False;
end;

procedure TCnFitCurve.Clear;
begin
  FPoints.Clear;
  FCurves.Clear;
end;

end.
