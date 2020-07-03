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
* 备    注：浮点精度受 Extended 类型影响不能无限制放大
*           大有理数运算特别慢，一个点迭代到十几次就慢得不能忍受了，果断删掉
*           大浮点数运算比较慢，一个点指定精度迭代到一百次得差不多 0.1 秒
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2020.07.03 V1.3
*               改用线程，至少界面没问题了。删掉跑不完的大有理数模式
*           2020.06.27 V1.2
*               用大浮点数同样实现无限放大，但运算速度也慢
*           2019.12.21 V1.1
*               用高精度无限有理数实现无限放大，但运算速度较慢
*           2019.12.18 V1.0
*               创建单元，实现功能，用 ScanLine 加速绘制
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls, Contnrs,
  CnBigDecimal;

const
  CN_MANDELBROT_MAX_COUNT = 100;

type
  TCnMandelbrotMode = (mmFloat, mmBigDecimal);
  // 浮点数运算、大浮点数运算

  TCnMandelbrotFloatColorEvent = function (Sender: TObject; X, Y: Extended;
    XZ, YZ: Extended; Count: Integer): TColor of object;
  {* 浮点数模式下迭代结果取色彩函数，注意 C 如果大于 C > CN_MANDELBROT_MAX_COUNT 表示收敛，应该返回显著点儿的颜色}

  TCnMandelbrotDecimalColorEvent = function (Sender: TObject; X, Y: TCnBigDecimal;
    XZ, YZ: TCnBigDecimal; Count: Integer): TColor of object;
  {* 大浮点数模式下迭代结果取色彩函数，注意 C 如果大于 C > CN_MANDELBROT_MAX_COUNT 表示收敛，应该返回显著点儿的颜色}

  TCnMandelbrotThreadCalcEvent = procedure (Sender: TObject; Progress, Total: Integer;
    var AbortCalc: Boolean) of object;
  {* 线程计算的内部进度事件}

  TCnMandelbrotProgressEvent = procedure (Sender: TObject; Progress, Total: Integer) of object;
  {* 线程计算的内部进度事件}

  TCnMandelbrotImage = class(TGraphicControl)
  {* 曼德布罗集图实现控件}
  private
    FLock: Boolean;   // 控件尺寸改变或边缘代表值改变时是否立即重新计算
    FThread: TThread;
    FBitmap: TBitmap;
    FXValues: array of Extended;
    FYValues: array of Extended;
    FXDecimals: TObjectList;
    FYDecimals: TObjectList;
    FMaxY: Extended;
    FMinX: Extended;
    FMinY: Extended;
    FMaxX: Extended;
    FMaxDX: TCnBigDecimal;
    FMinDX: TCnBigDecimal;
    FMaxDY: TCnBigDecimal;
    FMinDY: TCnBigDecimal;
    FOnFloatColor: TCnMandelbrotFloatColorEvent;
    FOnDecimalColor: TCnMandelbrotDecimalColorEvent;
    FShowAxis: Boolean;
    FAxisColor: TColor;
    FMode: TCnMandelbrotMode;
    FInSetCount: Integer;
    FOutSetCount: Integer;
    FDigits: Integer;
    FOnThreadCalcEvent: TCnMandelbrotThreadCalcEvent;
    FOnProgress: TCnMandelbrotProgressEvent;                           // 计算过程中设置运算精度用的
    procedure UpdatePointsValues(AWidth, AHeight: Integer); // 边缘值改变时重新给二维数组内容赋值
    procedure UpdateMatrixes(AWidth, AHeight: Integer);     // 尺寸改变时重新生成二维数组值，并调用 UpdatePointsValues 重新给每个元素赋值
    procedure SetMode(const Value: TCnMandelbrotMode);
    procedure SetShowAxis(const Value: Boolean);
    procedure SetAxisColor(const Value: TColor);
    procedure SetOnFloatColor(const Value: TCnMandelbrotFloatColorEvent);
    procedure SetOnDecimalColor(const Value: TCnMandelbrotDecimalColorEvent);
    procedure CheckLockedState;
  protected
    // 计算单个点的颜色
    function CalcFloatColor(X, Y: Extended; out InSet: Boolean): TColor;
    function CalcDecimalColor(X, Y: TCnBigDecimal; XZ, YZ: TCnBigDecimal; out InSet: Boolean): TColor;

    procedure TriggerCalcColors;
    procedure ReCalcColors;   // 根据 FMode 的值分别调用下面仨重新计算所有点的颜色，特耗时，要放线程里执行

    procedure ReCalcFloatColors;
    procedure ReCalcBigDecimalColors;
    procedure Paint; override;

    procedure DoProgress(Progress, Total: Integer); virtual;
    procedure ThreadTerminate(Sender: TObject);
    property OnThreadCalcEvent: TCnMandelbrotThreadCalcEvent read FOnThreadCalcEvent write FOnThreadCalcEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;
    
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // 以下仨函数在仨模式下改变控件上下左右四边缘代表的最大值，控件尺寸不变
    procedure SetRect(AMinX, AMaxX, AMinY, AMaxY: Extended); overload;
    procedure SetRect(AMinX, AMaxX, AMinY, AMaxY: TCnBigDecimal); overload;

    procedure GetComplexValues(X, Y: Integer; out R, I: Extended);
    procedure GetComplexDecimal(X, Y: Integer; R, I: TCnBigDecimal);
    function GetCurrentCalcDigits: Integer;
    {* 返回当前的计算精度，也就是小数点后第几位}
    procedure Lock;
    procedure UnLock;
  published
    property Mode: TCnMandelbrotMode read FMode write SetMode;
    {* 计算模式，是使用精度有限的扩展精度浮点，还是大有理数、还是大浮点数}

    property MinX: Extended read FMinX;
    {* 浮点数模式下的 X 轴左侧值}
    property MinY: Extended read FMinY;
    {* 浮点数模式下的 Y 轴下缘值}
    property MaxX: Extended read FMaxX;
    {* 浮点数模式下的 X 轴左侧值}
    property MaxY: Extended read FMaxY;
    {* 浮点数模式下的 Y 轴上缘值}

    property MinDX: TCnBigDecimal read FMinDX;
    {* 大浮点数模式下的 X 轴左侧值}
    property MinDY: TCnBigDecimal read FMinDY;
    {* 大浮点数模式下的 Y 轴下缘值}
    property MaxDX: TCnBigDecimal read FMaxDX;
    {* 大浮点数模式下的 X 轴左侧值}
    property MaxDY: TCnBigDecimal read FMaxDY;
    {* 大浮点数模式下的 Y 轴上缘值}

    property InSetCount: Integer read FInSetCount;
    {* 一次完整计算中，画面在集合内的点数量}
    property OutSetCount: Integer read FOutSetCount;
    {* 一次完整计算中，画面在集合外的点数量}

    property OnColor: TCnMandelbrotFloatColorEvent read FOnFloatColor write SetOnFloatColor;
    {* 自定义浮点模式下曼德布罗集像素点的颜色事件，如无，则内部使用纯色区分}
    property OnDecimalColor: TCnMandelbrotDecimalColorEvent read FOnDecimalColor
      write SetOnDecimalColor;
    {* 自定义大浮点数模式下曼德布罗集像素点的颜色事件，如无，则内部使用纯色区分}

    property OnProgress: TCnMandelbrotProgressEvent read FOnProgress write FOnProgress;
    {* 计算进度事件}

    property ShowAxis: Boolean read FShowAxis write SetShowAxis;
    {* 是否绘制坐标轴}
    property AxisColor: TColor read FAxisColor write SetAxisColor;
    {* 坐标轴颜色}
    property OnClick;
    {* 点击事件输出}
  end;

implementation

resourcestring
  SCnMandelbrotOutOfBounds = 'Invalid Mode or X Y Out of Bounds.';

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [Byte] of TRGBTriple;

  TCnMandelbrotThread = class(TThread)
  private
    FProgress: Integer;
    FTotal: Integer;
    FImage: TCnMandelbrotImage;
    procedure OnImageCalcEvent(Sender: TObject; Progress, Total: Integer;
      var AbortCalc: Boolean);
    procedure NotifyProgress;
  protected
    procedure Execute; override;
  public
    property Image: TCnMandelbrotImage read FImage write FImage;
  end;

var
  TmpDXZ: TCnBigDecimal = nil;
  TmpDYZ: TCnBigDecimal = nil;

procedure CalcMandelbortSetFloatPoint(X, Y: Extended; out XZ, YZ: Extended; out Count: Integer);
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

procedure CalcMandelbortSetDecimalPoint(X, Y: TCnBigDecimal; XZ, YZ: TCnBigDecimal;
  const Digits: Integer; out Count: Integer);

  function D2SqrSumGT4(A, B: TCnBigDecimal): Boolean;
  begin
    Result := False;
    BigDecimalCopy(TmpDXZ, A);
    BigDecimalCopy(TmpDYZ, B);
    BigDecimalMul(TmpDXZ, TmpDXZ, TmpDXZ, Digits);
    BigDecimalMul(TmpDYZ, TmpDYZ, TmpDYZ, Digits);
    BigDecimalAdd(TmpDXZ, TmpDXZ, TmpDYZ);

    if BigDecimalCompare(TmpDXZ, 4) > 0 then
      Result := True;
  end;

begin
  // 以大浮点数的方式迭代计算
  if TmpDXZ = nil then
    TmpDXZ := TCnBigDecimal.Create;
  if TmpDYZ = nil then
    TmpDYZ := TCnBigDecimal.Create;

  Count := 0;
  if D2SqrSumGT4(X, Y) then
    Exit;

  repeat
    BigDecimalCopy(TmpDXZ, XZ);
    BigDecimalCopy(TmpDYZ, YZ);
    BigDecimalMul(TmpDXZ, TmpDXZ, XZ, Digits);
    BigDecimalMul(TmpDYZ, TmpDYZ, YZ, Digits);

    BigDecimalMul(YZ, YZ, XZ, Digits);
    YZ.MulWord(2);
    BigDecimalAdd(YZ, YZ, Y);

    BigDecimalCopy(XZ, TmpDXZ);
    BigDecimalSub(XZ, XZ, TmpDYZ);
    BigDecimalAdd(XZ, XZ, X);

    Inc(Count);
  until D2SqrSumGT4(XZ, YZ) or (Count > CN_MANDELBROT_MAX_COUNT);
end;

{ TCnMandelbrotImage }

function TCnMandelbrotImage.CalcFloatColor(X, Y: Extended; out InSet: Boolean): TColor;
var
  XZ, YZ: Extended;
  C: Integer;
begin
  XZ := 0.0;
  YZ := 0.0;
  C := 0;

  CalcMandelbortSetFloatPoint(X, Y, XZ, YZ, C);

  if C > CN_MANDELBROT_MAX_COUNT then
  begin
    InSet := True;
    if Assigned(FOnFloatColor) then
      Result := FOnFloatColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clNavy;
  end
  else
  begin
    InSet := False;
    if Assigned(FOnFloatColor) then
      Result := FOnFloatColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clWhite;
  end;
end;

function TCnMandelbrotImage.CalcDecimalColor(X, Y, XZ,
  YZ: TCnBigDecimal; out InSet: Boolean): TColor;
var
  C: Integer;
begin
  XZ.SetZero;
  YZ.SetZero;
  C := 0;

  CalcMandelbortSetDecimalPoint(X, Y, XZ, YZ, FDigits, C);

  if C > CN_MANDELBROT_MAX_COUNT then
  begin
    InSet := True;
    if Assigned(FOnDecimalColor) then
      Result := FOnDecimalColor(Self, X, Y, XZ, YZ, C)
    else
      Result := clNavy;
  end
  else
  begin
    InSet := False;
    if Assigned(FOnDecimalColor) then
      Result := FOnDecimalColor(Self, X, Y, XZ, YZ, C)
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

  FDigits := 8;

  FAxisColor := clTeal;
  FXDecimals := TObjectList.Create(True);
  FYDecimals := TObjectList.Create(True);

  FMaxDX := TCnBigDecimal.Create;
  FMinDX := TCnBigDecimal.Create;
  FMaxDY := TCnBigDecimal.Create;
  FMinDY := TCnBigDecimal.Create;
end;

destructor TCnMandelbrotImage.Destroy;
begin
  FMaxDX.Free;
  FMinDX.Free;
  FMaxDY.Free;
  FMinDY.Free;

  FYDecimals.Free;
  FXDecimals.Free;

  FBitmap.Free;
  SetLength(FXValues, 0);
  SetLength(FYValues, 0);
  inherited;
end;

procedure TCnMandelbrotImage.GetComplexValues(X, Y: Integer; out R,
  I: Extended);
begin
  if (FMode = mmFloat) and (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    R := FXValues[X];
    I := FYValues[Y];
  end
  else
    raise Exception.Create(SCnMandelbrotOutOfBounds);
end;

procedure TCnMandelbrotImage.GetComplexDecimal(X, Y: Integer; R,
  I: TCnBigDecimal);
begin
  if (FMode = mmBigDecimal) and (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    BigDecimalCopy(R, TCnBigDecimal(FXDecimals[X]));
    BigDecimalCopy(I, TCnBigDecimal(FYDecimals[Y]));
  end
  else
    raise Exception.Create(SCnMandelbrotOutOfBounds);
end;

procedure TCnMandelbrotImage.Loaded;
begin
  inherited;
  CheckLockedState;
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

procedure TCnMandelbrotImage.ReCalcFloatColors;
var
  X, Y, C: Integer;
  AColor: TColor;
  R, G, B: Byte;
  Arr: PRGBTripleArray;
  InSet: Boolean;
begin
  FInSetCount := 0;
  FOutSetCount := 0;

  for Y := 0 to Height - 1 do
  begin
    Arr := PRGBTripleArray(FBitmap.ScanLine[Y]);
    for X := 0 to Width - 1 do
    begin
      AColor := CalcFloatColor(FXValues[X], FYValues[Y], InSet);
      if InSet then
        Inc(FInSetCount)
      else
        Inc(FOutSetCount);

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

procedure TCnMandelbrotImage.ReCalcBigDecimalColors;
var
  X, Y, C: Integer;
  AColor: TColor;
  R, G, B: Byte;
  Arr: PRGBTripleArray;
  XZ, YZ: TCnBigDecimal;
  InSet, AbortCalc: Boolean;
begin
  FInSetCount := 0;
  FOutSetCount := 0;

  XZ := nil;
  YZ := nil;
  AbortCalc := False;

  try
    XZ := TCnBigDecimal.Create;
    YZ := TCnBigDecimal.Create;

    for Y := 0 to Height - 1 do
    begin
      Arr := PRGBTripleArray(FBitmap.ScanLine[Y]);
      for X := 0 to Width - 1 do
      begin
        AColor := CalcDecimalColor(TCnBigDecimal(FXDecimals[X]),
          TCnBigDecimal(FYDecimals[Y]), XZ, YZ, InSet);

        if InSet then
          Inc(FInSetCount)
        else
          Inc(FOutSetCount);

        C := ColorToRGB(AColor);
        B := C and $FF0000 shr 16;
        G := C and $00FF00 shr 8;
        R := C and $0000FF;

        Arr^[X].rgbtRed := R;
        Arr^[X].rgbtGreen := G;
        Arr^[X].rgbtBlue := B;
      end;

      if Assigned(FOnThreadCalcEvent) then
        FOnThreadCalcEvent(Self, Y, Height - 1, AbortCalc);

      if AbortCalc then
        Exit;
    end;
  finally
    XZ.Free;
    YZ.Free;
  end;
  // Invalidate;
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
  CheckLockedState;
end;

procedure TCnMandelbrotImage.SetMode(const Value: TCnMandelbrotMode);
begin
  if Value <> FMode then
  begin
    FMode := Value;

    FMinDX.SetExtended(FMinX);
    FMinDY.SetExtended(FMinY);
    FMaxDX.SetExtended(FMaxX);
    FMaxDY.SetExtended(FMaxY);

    CheckLockedState;
  end;
end;

procedure TCnMandelbrotImage.SetRect(AMinX, AMaxX, AMinY, AMaxY: Extended);
begin
  if FMode = mmFloat then
  begin
    FMinX := AMinX;
    FMinY := AMinY;
    FMaxX := AMaxX;
    FMaxY := AMaxY;

    CheckLockedState;
  end;
end;

procedure TCnMandelbrotImage.SetRect(AMinX, AMaxX, AMinY,
  AMaxY: TCnBigDecimal);
var
  W, H: TCnBigDecimal;
  N: Integer;
begin
  if FMode = mmBigDecimal then
  begin
    BigDecimalCopy(FMinDX, AMinX);
    BigDecimalCopy(FMinDY, AMinY);
    BigDecimalCopy(FMaxDX, AMaxX);
    BigDecimalCopy(FMaxDY, AMaxY);

    // 根据控件尺寸与位置确定计算精度，已知 800 像素，3 的距离，运算精度 8 满足要求
    // 推测：相邻两个像素的值的差决定了运算精度，0.00375 需要精度 8，按指数比例，0.0375 只需要 7，0.375 只需要 6
    // 简化成 0.001 需要 8，10^-3 需要 8，10^-2 需要 7，10^-n 需要 5 + n
    // 其中 n 是最高位有效数字是小数点后多少位

    W := TCnBigDecimal.Create;
    H := TCnBigDecimal.Create;
    try
      BigDecimalSub(W, AMaxX, AMinX);
      BigDecimalSub(H, AMaxX, AMinX);

      W.DivWord(Width);
      H.DivWord(Height);

      if W.IsNegative then
        W.Negate;
      if H.IsNegative then
        H.Negate;

      if BigDecimalCompare(W, H) >= 0 then
      begin
        // H 更小
        N := BigDecimalGetHighScale(H);

      end
      else
      begin
        // W 更小
        N := BigDecimalGetHighScale(W);
      end;

      if N > 0 then
      begin
        N := N + 5;
        FDigits := N;
      end;
    finally
      W.Free;
      H.Free;
    end;

    CheckLockedState;
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
var
  I: Integer;
begin
  if FMode = mmFloat then
  begin
    // 判断并重新初始化 X、Y 的浮点数数组
    if Length(FXValues) <> AWidth then
      SetLength(FXValues, AWidth);
    if Length(FYValues) <> AHeight then
      SetLength(FYValues, AHeight);
  end
  else if FMode = mmBigDecimal then
  begin
    // 判断并重新初始化 X、Y 的大浮点数列表
    if FXDecimals.Count <> AWidth then
    begin
      FXDecimals.Clear;
      for I := 1 to AWidth do
        FXDecimals.Add(TCnBigDecimal.Create);
    end;
    if FYDecimals.Count <> AHeight then
    begin
      FYDecimals.Clear;
      for I := 1 to AHeight do
        FYDecimals.Add(TCnBigDecimal.Create);
    end;
  end;

  // 判断并重新初始化内部位图
  if (FBitmap = nil) or ((FBitmap.Width <> AWidth) or (FBitmap.Height <> AHeight)) then
  begin
    FreeAndNil(FBitmap);
    FBitmap := TBitmap.Create;
    FBitmap.PixelFormat := pf24bit;
    FBitmap.Width := AWidth;
    FBitmap.Height := AHeight;
  end;

  UpdatePointsValues(AWidth, AHeight);
end;

procedure TCnMandelbrotImage.UpdatePointsValues(AWidth, AHeight: Integer);
var
  X, Y, W, H: Integer;
  WX, HY: Extended;
  WDX, HDY: TCnBigDecimal;
begin
  W := Width - 1;
  H := Height - 1;
  if FMode = mmFloat then
  begin
    WX := (FMaxX - FMinX) / W;
    HY := (FMaxY - FMinY) / H;

    for X := 0 to W do
      FXValues[X] := FMinX + X * WX;

    for Y := 0 to H do
      FYValues[Y] := FMinY + (H - Y) * HY;
  end
  else if FMode = mmBigDecimal then
  begin
    // 初始化 X、Y 的大浮点数点值
    WDX := TCnBigDecimal.Create;
    HDY := TCnBigDecimal.Create;

    BigDecimalSub(WDX, FMaxDX, FMinDX);
    WDX.DivWord(W);
    BigDecimalSub(HDY, FMaxDY, FMinDY);
    HDY.DivWord(H);

    for X := 0 to W do
    begin
      BigDecimalCopy(TCnBigDecimal(FXDecimals[X]), WDX);
      TCnBigDecimal(FXDecimals[X]).MulWord(X);
      BigDecimalAdd(TCnBigDecimal(FXDecimals[X]), FMinDX, TCnBigDecimal(FXDecimals[X]));
    end;

    for Y := 0 to H do
    begin
      BigDecimalCopy(TCnBigDecimal(FYDecimals[Y]), HDY);
      TCnBigDecimal(FYDecimals[Y]).MulWord(H - Y);
      BigDecimalAdd(TCnBigDecimal(FYDecimals[Y]), FMinDY, TCnBigDecimal(FYDecimals[Y]));
    end;
  end;

  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.Brush.Style := bsSolid;
  FBitmap.Canvas.FillRect(Rect(0, 0, AHeight, AWidth));
end;

procedure TCnMandelbrotImage.ReCalcColors;
begin
  if FMode = mmFloat then
    ReCalcFloatColors
  else if FMode = mmBigDecimal then
    RecalcBigDecimalColors;
end;

procedure TCnMandelbrotImage.Lock;
begin
  FLock := True;
end;

procedure TCnMandelbrotImage.UnLock;
begin
  FLock := False;
  CheckLockedState;
end;

procedure TCnMandelbrotImage.CheckLockedState;
begin
  if not (csLoading in ComponentState) and not FLock then
  begin
    UpdateMatrixes(Width, Height); // 中间会调用 UpdatePointsValue
    TriggerCalcColors;
  end;
end;

procedure TCnMandelbrotImage.SetOnDecimalColor(
  const Value: TCnMandelbrotDecimalColorEvent);
begin
  FOnDecimalColor := Value;
  CheckLockedState;
end;

procedure TCnMandelbrotImage.TriggerCalcColors;
begin
  if Mode = mmFloat then
  begin
    ReCalcColors;
    Exit;
  end;

  if FThread <> nil then
  begin
    FThread.Terminate;
    try
      FThread.WaitFor;
    except
      ;
    end;
    FThread := nil;
  end;

  if FThread = nil then
  begin
    FThread := TCnMandelbrotThread.Create(True);
    TCnMandelbrotThread(FThread).Image := Self;
    FOnThreadCalcEvent := TCnMandelbrotThread(FThread).OnImageCalcEvent;
    FThread.FreeOnTerminate := True;
    FThread.OnTerminate := ThreadTerminate;

    FThread.Resume;
  end;
end;

procedure TCnMandelbrotImage.DoProgress(Progress, Total: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress, Total);
  Invalidate;
end;

procedure TCnMandelbrotImage.ThreadTerminate(Sender: TObject);
begin
  FThread := nil;
  Invalidate;
end;

procedure TCnMandelbrotImage.SetOnFloatColor(
  const Value: TCnMandelbrotFloatColorEvent);
begin
  FOnFloatColor := Value;
  CheckLockedState;
end;

function TCnMandelbrotImage.GetCurrentCalcDigits: Integer;
begin
  Result := FDigits;
end;

{ TCnMandelbrotThread }

procedure TCnMandelbrotThread.Execute;
begin
  if FImage <> nil then
    FImage.ReCalcColors;
end;

procedure TCnMandelbrotThread.NotifyProgress;
begin
  if FImage <> nil then
    FImage.DoProgress(FProgress, FTotal);
end;

procedure TCnMandelbrotThread.OnImageCalcEvent(Sender: TObject; Progress,
  Total: Integer; var AbortCalc: Boolean);
begin
  // 线程内得到进度通知
  if Terminated then
    AbortCalc := True
  else
  begin
    FProgress := Progress;
    FTotal := Total;
    Synchronize(NotifyProgress);
  end;
end;

initialization

finalization
  TmpDXZ.Free;
  TmpDYZ.Free;

end.
