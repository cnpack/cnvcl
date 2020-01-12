{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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
* 备    注：精度受 Double 类型影响不能无限制放大
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.12.21 V1.1
*               用高精度无限有理数实现无限放大，但运算速度较慢
*           2019.12.18 V1.0
*               创建单元，实现功能，用 ScanLine 加速绘制
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls, Contnrs, CnBigRational;

const
  CN_MANDELBROT_MAX_COUNT = 100;

type
  TCnMandelbrotColorEvent = function (Sender: TObject; X, Y: Extended;
    XZ, YZ: Extended; Count: Integer): TColor of object;
  {* 迭代结果取色彩函数，注意 C 如果大于 C > CN_MANDELBROT_MAX_COUNT 表示收敛，应该返回显著点儿的颜色}

  TCnMandelbrotInfiniteColorEvent = function (Sender: TObject; X, Y: TCnBigRationalNumber;
    XZ, YZ: TCnBigRationalNumber; Count: Integer): TColor of object;

  TCnMandelbrotImage = class(TGraphicControl)
  {* 曼德布罗集图实现控件}
  private
    // FMaps: array of array of TColor;
    FBitmap: TBitmap;
    FXValues: array of Extended;
    FYValues: array of Extended;
    FXRationals: TObjectList;
    FYRationals: TObjectList;
    FMaxY: Extended;
    FMinX: Extended;
    FMinY: Extended;
    FMaxX: Extended;
    FMaxRX: TCnBigRationalNumber;
    FMinRX: TCnBigRationalNumber;
    FMaxRY: TCnBigRationalNumber;
    FMinRY: TCnBigRationalNumber;
    FOnColor: TCnMandelbrotColorEvent;
    FOnInfiniteColor: TCnMandelbrotInfiniteColorEvent;
    FShowAxis: Boolean;
    FAxisColor: TColor;
    FInfiniteMode: Boolean;
    procedure SetMaxX(const Value: Extended);
    procedure SetMaxY(const Value: Extended);
    procedure SetMinX(const Value: Extended);
    procedure SetMinY(const Value: Extended);

    procedure UpdatePointsValues(AWidth, AHeight: Integer);
    procedure UpdateMatrixes(AWidth, AHeight: Integer);
    procedure SetShowAxis(const Value: Boolean);
    procedure SetAxisColor(const Value: TColor);
    procedure SetOnColor(const Value: TCnMandelbrotColorEvent);
    procedure SetInfiniteMode(const Value: Boolean);
    procedure SetOnInfiniteColor(
      const Value: TCnMandelbrotInfiniteColorEvent);
  protected
    function CalcColor(X, Y: Extended): TColor;
    function CalcInfiniteColor(X, Y: TCnBigRationalNumber; XZ, YZ: TCnBigRationalNumber): TColor;
    procedure ReCalcColors;
    procedure ReCalcInfiniteColors;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;
    
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetRect(AMinX, AMaxX, AMinY, AMaxY: Extended); overload;
    procedure SetRect(AMinX, AMaxX, AMinY, AMaxY: TCnBigRationalNumber); overload;
    procedure GetComplexValues(X, Y: Integer; out R, I: Extended);
    procedure GetComplexRational(X, Y: Integer; R, I: TCnBigRationalNumber);
  published
    property InfiniteMode: Boolean read FInfiniteMode write SetInfiniteMode;
    {* 是否使用大有理数算法来计算的无限放大模式}

    property MinX: Extended read FMinX write SetMinX;
    {* X 轴左侧值}
    property MinY: Extended read FMinY write SetMinY;
    {* Y 轴下缘值}
    property MaxX: Extended read FMaxX write SetMaxX;
    {* X 轴左侧值}
    property MaxY: Extended read FMaxY write SetMaxY;
    {* Y 轴上缘值}

    property OnColor: TCnMandelbrotColorEvent read FOnColor write SetOnColor;
    {* 自定义曼德布罗集像素点的颜色事，如无，则内部使用黑白色}
    property OnInfiniteColor: TCnMandelbrotInfiniteColorEvent read FOnInfiniteColor
      write SetOnInfiniteColor;
    property ShowAxis: Boolean read FShowAxis write SetShowAxis;
    {* 是否绘制坐标轴}
    property AxisColor: TColor read FAxisColor write SetAxisColor;
    {* 坐标轴颜色}
    property OnClick;
    {* 点击事件输出}
  end;

implementation

{ TCnMandelbrotImage }

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [Byte] of TRGBTriple;

var
  TmpXZ, TmpYZ: TCnBigRationalNumber;

procedure CalcMandelbortSetInfinitePoint(X, Y: TCnBigRationalNumber; XZ, YZ: TCnBigRationalNumber;
  out Count: Integer);

  function R2SqrSumGT4(A, B: TCnBigRationalNumber): Boolean;
  begin
    Result := False;
    TmpXZ.Assign(A);
    TmpYZ.Assign(B);
    TmpXZ.Mul(TmpXZ);
    TmpYZ.Mul(TmpYZ);
    TmpXZ.Add(TmpYZ);
  if CnBigRationalNumberCompare(TmpXZ, 4) > 0 then
    Result := True;
  end;
begin
  // 以有理数的方式迭代计算
  if TmpXZ = nil then
    TmpXZ := TCnBigRationalNumber.Create;
  if TmpYZ = nil then
    TmpYZ := TCnBigRationalNumber.Create;

  if R2SqrSumGT4(X, Y) then
    Exit;

  repeat
    TmpXZ.Assign(XZ);
    TmpYZ.Assign(YZ);
    TmpXZ.Mul(XZ);
    TmpYZ.Mul(YZ);

    YZ.Mul(XZ);
    YZ.Mul(2);
    YZ.Add(Y);

    XZ.Assign(TmpXZ);
    XZ.Sub(TmpYZ);
    XZ.Add(X);

    Inc(Count);
  until R2SqrSumGT4(XZ, YZ) or (Count > CN_MANDELBROT_MAX_COUNT);
end;

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

function TCnMandelbrotImage.CalcInfiniteColor(X,
  Y: TCnBigRationalNumber; XZ, YZ: TCnBigRationalNumber): TColor;
var
  C: Integer;
begin
  XZ.SetZero;
  YZ.SetZero;
  C := 0;

  CalcMandelbortSetInfinitePoint(X, Y, XZ, YZ, C);

  if C > CN_MANDELBROT_MAX_COUNT then
  begin
    if Assigned(FOnInfiniteColor) then
      Result := FOnInfiniteColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clNavy;
  end
  else
  begin
    if Assigned(FOnInfiniteColor) then
      Result := FOnInfiniteColor(Self, X, Y, XZ, YZ, C)
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

  FAxisColor := clTeal;
  FXRationals := TObjectList.Create(True);
  FYRationals := TObjectList.Create(True);
  FMaxRX := TCnBigRationalNumber.Create;
  FMinRX := TCnBigRationalNumber.Create;
  FMaxRY := TCnBigRationalNumber.Create;
  FMinRY := TCnBigRationalNumber.Create;
end;

destructor TCnMandelbrotImage.Destroy;
begin
  FMinRY.Free;
  FMaxRY.Free;
  FMinRX.Free;
  FMaxRX.Free;
  FYRationals.Free;
  FXRationals.Free;

  FBitmap.Free;
  SetLength(FXValues, 0);
  SetLength(FYValues, 0);
  inherited;
end;

procedure TCnMandelbrotImage.GetComplexRational(X, Y: Integer; R,
  I: TCnBigRationalNumber);
begin
  if FInfiniteMode and (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    R.Assign(TCnBigRationalNumber(FXRationals[X]));
    I.Assign(TCnBigRationalNumber(FYRationals[Y]));
  end
  else
    raise Exception.Create('X Y Index Out of Bounds.');
end;

procedure TCnMandelbrotImage.GetComplexValues(X, Y: Integer; out R,
  I: Extended);
begin
  if not FInfiniteMode and (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
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
end;

procedure TCnMandelbrotImage.Paint;
var
  X, Y: Integer;
begin
  Canvas.Draw(0, 0, FBitmap);

  if ShowAxis then
  begin
    // 算出 X Y 轴的位置，画线
    X := Trunc(Width * (-FMinX) / (FMaxX - FMinX));
    Y := Trunc(Height * (FMaxY) / (FMaxY - FMinY));

    Canvas.Pen.Color := FAxisColor;
    Canvas.Pen.Style := psSolid;
    Canvas.MoveTo(X, 0);
    Canvas.LineTo(X, Height);
    Canvas.MoveTo(0, Y);
    Canvas.LineTo(Width, Y);
  end;
end;

procedure TCnMandelbrotImage.ReCalcColors;
var
  X, Y, C: Integer;
  AColor: TColor;
  R, G, B: Byte;
  Arr: PRGBTripleArray;
begin
  for Y := 0 to Height - 1 do
  begin
    Arr := PRGBTripleArray(FBitmap.ScanLine[Y]);
    for X := 0 to Width - 1 do
    begin
      AColor := CalcColor(FXValues[X], FYValues[Y]);
      C := ColorToRGB(AColor);
      B := C and $FF0000 shr 16;
      G := C and $00FF00 shr 8;
      R := C and $0000FF;

      Arr^[X].rgbtRed := R;
      Arr^[X].rgbtGreen := G;
      Arr^[X].rgbtBlue := B;
    end;
  end;
  Invalidate;
end;

procedure TCnMandelbrotImage.ReCalcInfiniteColors;
var
  X, Y, C: Integer;
  AColor: TColor;
  R, G, B: Byte;
  Arr: PRGBTripleArray;
  XZ, YZ: TCnBigRationalNumber;
begin
  XZ := nil;
  YZ := nil;
  try
    XZ := TCnBigRationalNumber.Create;
    YZ := TCnBigRationalNumber.Create;

    for Y := 0 to Height - 1 do
    begin
      Arr := PRGBTripleArray(FBitmap.ScanLine[Y]);
      for X := 0 to Width - 1 do
      begin
        AColor := CalcInfiniteColor(TCnBigRationalNumber(FXRationals[X]),
          TCnBigRationalNumber(FYRationals[Y]), XZ, YZ);
        C := ColorToRGB(AColor);
        B := C and $FF0000 shr 16;
        G := C and $00FF00 shr 8;
        R := C and $0000FF;

        Arr^[X].rgbtRed := R;
        Arr^[X].rgbtGreen := G;
        Arr^[X].rgbtBlue := B;
      end;
    end;
  finally
    XZ.Free;
    YZ.Free;
  end;
  Invalidate;
end;

procedure TCnMandelbrotImage.SetAxisColor(const Value: TColor);
begin
  if Value <> FAxisColor then
  begin
    FAxisColor := Value;
    Invalidate;
  end;
end;

procedure TCnMandelbrotImage.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    UpdateMatrixes(AWidth, AHeight);
    if FInfiniteMode then
      ReCalcInfiniteColors
    else
      ReCalcColors;
  end;
end;

procedure TCnMandelbrotImage.SetInfiniteMode(const Value: Boolean);
begin
  if Value <> FInfiniteMode then
  begin
    FInfiniteMode := Value;
    FMinRX.SetFloat(FMinX);
    FMinRY.SetFloat(FMinY);
    FMaxRX.SetFloat(FMaxX);
    FMaxRY.SetFloat(FMaxY);

    UpdateMatrixes(Width, Height);
    UpdatePointsValues(Width, Height);

    if FInfiniteMode then
      ReCalcInfiniteColors
    else
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
      if not FInfiniteMode then
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
      if not FInfiniteMode then
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
      if not FInfiniteMode then
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

procedure TCnMandelbrotImage.SetOnColor(const Value: TCnMandelbrotColorEvent);
begin
  FOnColor := Value;
  Invalidate;
end;

procedure TCnMandelbrotImage.SetOnInfiniteColor(
  const Value: TCnMandelbrotInfiniteColorEvent);
begin
  FOnInfiniteColor := Value;
  Invalidate;
end;

procedure TCnMandelbrotImage.SetRect(AMinX, AMaxX, AMinY, AMaxY: Extended);
begin
  FMinX := AMinX;
  FMinY := AMinY;
  FMaxX := AMaxX;
  FMaxY := AMaxY;

  UpdatePointsValues(Width, Height);
  ReCalcColors;
end;

procedure TCnMandelbrotImage.SetRect(AMinX, AMaxX, AMinY,
  AMaxY: TCnBigRationalNumber);
begin
  FMinRX.Assign(AMinX);
  FMinRY.Assign(AMinY);
  FMaxRX.Assign(AMaxX);
  FMaxRY.Assign(AMaxY);

  UpdatePointsValues(Width, Height);
  ReCalcInfiniteColors;
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
var
  I: Integer;
begin
  if not FInfiniteMode then
  begin
    SetLength(FXValues, AWidth);
    SetLength(FYValues, AHeight);
  end
  else
  begin
    // 初始化 X、Y 的有理数列表
    FXRationals.Clear;
    for I := 1 to AWidth do
      FXRationals.Add(TCnBigRationalNumber.Create);
    FYRationals.Clear;
    for I := 1 to AHeight do
      FYRationals.Add(TCnBigRationalNumber.Create);
  end;

  FreeAndNil(FBitmap);
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf24bit;
  FBitmap.Width := AWidth;
  FBitmap.Height := AHeight;

  UpdatePointsValues(AWidth, AHeight);
end;

procedure TCnMandelbrotImage.UpdatePointsValues(AWidth, AHeight: Integer);
var
  X, Y, W, H: Integer;
  WX, HY: Extended;
  WRX, HRY: TCnBigRationalNumber;
begin
  W := Width - 1;
  H := Height - 1;
  if FInfiniteMode then
  begin
    // 初始化 X、Y 的有理数点值
    WRX := TCnBigRationalNumber.Create;
    HRY := TCnBigRationalNumber.Create;

    CnBigRationalNumberSub(FMaxRX, FMinRX, WRX);
    WRX.Divide(W);
    CnBigRationalNumberSub(FMaxRY, FMinRY, HRY);
    HRY.Divide(H);

    for X := 0 to W do
    begin
      TCnBigRationalNumber(FXRationals[X]).Assign(WRX);
      TCnBigRationalNumber(FXRationals[X]).Mul(X);
      CnBigRationalNumberAdd(TCnBigRationalNumber(FXRationals[X]), FMinRX, TCnBigRationalNumber(FXRationals[X]));
    end;

    for Y := 0 to H do
    begin
      TCnBigRationalNumber(FYRationals[Y]).Assign(HRY);
      TCnBigRationalNumber(FYRationals[Y]).Mul(Y);
      CnBigRationalNumberAdd(TCnBigRationalNumber(FYRationals[Y]), FMinRY, TCnBigRationalNumber(FYRationals[Y]));
    end;
  end
  else
  begin
    WX := (FMaxX - FMinX) / W;
    HY := (FMaxY - FMinY) / H;

    for X := 0 to W do
      FXValues[X] := FMinX + X * WX;

    for Y := 0 to H do
      FYValues[Y] := FMinY + (H - Y) * HY;
  end;

  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.Brush.Style := bsSolid;
  FBitmap.Canvas.FillRect(Rect(0, 0, AHeight, AWidth));
end;

initialization

finalization
  TmpXZ.Free;
  TmpYZ.Free;

end.
