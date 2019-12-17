{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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

unit CnMandelbrotImage;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：曼德布罗集图实现单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.12.17 V1.0
*               LiuXiao 移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls;

const
  CN_MANDELBROT_MAX_COUNT = 100;

type
  TMandelbrotColorEvent = function (Sender: TObject; X, Y: Extended;
    XZ, YZ: Extended; Count: Integer): TColor of object;
  {* 迭代结果取色彩函数，注意 C 如果大于 C > CN_MANDELBROT_MAX_COUNT 表示收敛，应该返回显著点儿的颜色}

  TCnMandelbrotImage = class(TGraphicControl)
  {* 曼德布罗集图实现控件}
  private
    FMaps: array of array of TColor;
    FXValues: array of Extended;
    FYValues: array of Extended;
    FMaxY: Extended;
    FMinX: Extended;
    FMinY: Extended;
    FMaxX: Extended;
    FOnColor: TMandelbrotColorEvent;
    FShowAxis: Boolean;
    procedure SetMaxX(const Value: Extended);
    procedure SetMaxY(const Value: Extended);
    procedure SetMinX(const Value: Extended);
    procedure SetMinY(const Value: Extended);

    procedure UpdatePointsValues(AWidth, AHeight: Integer);
    procedure UpdateMatrixes(AWidth, AHeight: Integer);
    procedure SetShowAxis(const Value: Boolean);
  protected
    function CalcColor(X, Y: Extended): TColor;
    procedure ReCalcColors;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;
    
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure GetComplexValues(X, Y: Integer; out R, I: Extended);
  published
    property MinX: Extended read FMinX write SetMinX;
    {* X 轴左侧值}
    property MinY: Extended read FMinY write SetMinY;
    {* Y 轴下缘值}
    property MaxX: Extended read FMaxX write SetMaxX;
    {* X 轴左侧值}
    property MaxY: Extended read FMaxY write SetMaxY;
    {* Y 轴上缘值}

    property OnColor: TMandelbrotColorEvent read FOnColor write FOnColor;
    {* 自定义曼德布罗集像素点的颜色事，如无，则内部使用黑白色}
    property ShowAxis: Boolean read FShowAxis write SetShowAxis;
    {* 是否绘制坐标轴}
    property OnClick;
    {* 点击事件输出}
  end;

implementation

{ TCnMandelbrotImage }

procedure CalcMandelbortSetPoint(X, Y: Extended; out XZ, YZ: Extended; out Count: Integer);
var
  XZ2, YZ2: Extended;
begin
  XZ := 0.0;
  YZ := 0.0;
  Count := 0;

  if X * X + Y * Y > 4.0 then
    Exit;

  repeat
    // XZ + YZi := (XZ + YZi)^2 + (X + Yi);
    XZ2 := XZ * XZ;
    YZ2 := YZ * YZ;

    // 单次迭代过程中需要保留 XZ^2 与 YZ^2 的值，避免中途发生改变
    YZ := 2.0 * XZ * YZ + Y;
    XZ := XZ2 - YZ2 + X;
    Inc(Count);
  until (XZ * XZ + YZ * YZ > 4.0) or (Count > CN_MANDELBROT_MAX_COUNT);
end;

function TCnMandelbrotImage.CalcColor(X, Y: Extended): TColor;
var
  XZ, YZ: Extended;
  C: Integer;
begin
  XZ := 0.0;
  YZ := 0.0;
  C := 0;

  CalcMandelbortSetPoint(X, Y, XZ, YZ, C);

  if C > CN_MANDELBROT_MAX_COUNT then
  begin
    if Assigned(FOnColor) then
      Result := FOnColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clNavy;
  end
  else
  begin
    if Assigned(FOnColor) then
      Result := FOnColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clWhite;
  end;
end;

constructor TCnMandelbrotImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinX := -2.0;
  FMaxX := 1.0;
  FMinY := -1.5;
  FMaxY := 1.5;
end;

destructor TCnMandelbrotImage.Destroy;
begin
  SetLength(FMaps, 0);
  SetLength(FXValues, 0);
  SetLength(FYValues, 0);
  inherited;
end;

procedure TCnMandelbrotImage.GetComplexValues(X, Y: Integer; out R,
  I: Extended);
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    R := FXValues[X];
    I := FYValues[Y];
  end
  else
    raise Exception.Create('X Y Index Out of Bounds.');
end;

procedure TCnMandelbrotImage.Loaded;
begin
  inherited;
  UpdateMatrixes(Width, Height);
  ReCalcColors;
  ReCalcColors;
end;

procedure TCnMandelbrotImage.Paint;
var
  X, Y: Integer;
begin
  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
      Canvas.Pixels[X, Y] := FMaps[X, Y];

  if ShowAxis then
  begin
    // 算出 X Y 轴的位置，画线
    X := Trunc(Width * (-FMinX) / (FMaxX - FMinX));
    Y := Trunc(Height * (FMaxY) / (FMaxY - FMinY));
    Canvas.Pen.Color := clRed;
    Canvas.Pen.Style := psSolid;
    Canvas.MoveTo(X, 0);
    Canvas.LineTo(X, Height);
    Canvas.MoveTo(0, Y);
    Canvas.LineTo(Width, Y);
  end;
end;

procedure TCnMandelbrotImage.ReCalcColors;
var
  X, Y: Integer;
begin
  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
      FMaps[X, Y] := CalcColor(FXValues[X], FYValues[Y]);
  Invalidate;
end;

procedure TCnMandelbrotImage.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    UpdateMatrixes(AWidth, AHeight);
    ReCalcColors;
  end;
end;

procedure TCnMandelbrotImage.SetMaxX(const Value: Extended);
begin
  if Value <> FMaxX then
  begin
    FMaxX := Value;
    if not (csLoading in ComponentState) then
    begin
      UpdatePointsValues(Width, Height);
      ReCalcColors;
    end;
  end;
end;

procedure TCnMandelbrotImage.SetMaxY(const Value: Extended);
begin
  if Value <> FMaxY then
  begin
    FMaxY := Value;
    if not (csLoading in ComponentState) then
    begin
      UpdatePointsValues(Width, Height);
      ReCalcColors;
    end;
  end;
end;

procedure TCnMandelbrotImage.SetMinX(const Value: Extended);
begin
  if Value <> FMinX then
  begin
    FMinX := Value;
    if not (csLoading in ComponentState) then
    begin
      UpdatePointsValues(Width, Height);
      ReCalcColors;
    end;
  end;
end;

procedure TCnMandelbrotImage.SetMinY(const Value: Extended);
begin
  if Value <> FMinY then
  begin
    FMinY := Value;
    if not (csLoading in ComponentState) then
    begin
      UpdatePointsValues(Width, Height);
      ReCalcColors;
    end;
  end;
end;

procedure TCnMandelbrotImage.SetShowAxis(const Value: Boolean);
begin
  if Value <> FShowAxis then
  begin
    FShowAxis := Value;
    Invalidate;
  end;
end;

procedure TCnMandelbrotImage.UpdateMatrixes(AWidth, AHeight: Integer);
begin
  SetLength(FXValues, AWidth);
  SetLength(FYValues, AHeight);
  SetLength(FMaps, AWidth, AHeight);

  UpdatePointsValues(AWidth, AHeight);
end;

procedure TCnMandelbrotImage.UpdatePointsValues(AWidth, AHeight: Integer);
var
  X, Y, W, H: Integer;
  WX, HY: Extended;
begin
  W := Width - 1;
  H := Height - 1;
  WX := (FMaxX - FMinX) / W;
  HY := (FMaxY - FMinY) / H;

  for X := 0 to W do
    FXValues[X] := FMinX + X * WX;

  for Y := 0 to H do
    FYValues[Y] := FMinY + (H - Y) * HY;

  for X := 0 to W do
    for Y := 0 to H do
      FMaps[X, Y] := clWhite;
end;

end.
