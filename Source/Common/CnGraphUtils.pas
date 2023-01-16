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

unit CnGraphUtils;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：公共图像过程库单元
* 单元作者：CnPack 开发组
* 备    注：加入 GDI+ 支持后不再支持低版本 Windows
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2021.09.28 V1.1
*               加入一个平滑拉伸绘制位图的函数，使用 GDI+
*           2002.10.20 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Graphics, Math, Classes, Controls
  {$IFDEF SUPPORT_GDIPLUS}, WinApi.GDIPOBJ, WinApi.GDIPAPI {$ENDIF};

//==============================================================================
// 扩展的颜色格式转换函数
//==============================================================================

var
  HSLRange: Integer = 240;

//------------------------------------------------------------------------------
// HSL 颜色与 RGB 色转换函数
//------------------------------------------------------------------------------

function HSLToRGB(H, S, L: Double): TColor;
{* HSL 颜色转换为 RGB 颜色
 |<PRE>
   H, S, L: Double   - 分别为色调、饱和度、亮度分量，为"0"到"1"之间的小数
   Result: TColor    - 返回 RGB 颜色值
 |</PRE>}
function HSLRangeToRGB(H, S, L: Integer): TColor;
{* HSL 颜色转换为 RGB 颜色
 |<PRE>
   H, S, L: Integer  - 分别为色调、饱和度、亮度分量，0..240
   Result: TColor    - 返回 RGB 颜色值
 |</PRE>}
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
{* RGB 颜色转换为 HSL 颜色
 |<PRE>
  Color: TColor         - RGB 颜色值
  H, S, L: Integer      - 输出分别为色调、饱和度、亮度分量，为"0"到"1"之间的小数
 |</PRE>}
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
{* RGB 颜色转换为 HSL 颜色
 |<PRE>
   Color: TColor        - RGB颜色值
   H, S, L: Integer     - 输出分别为色调、饱和度、亮度分量，0..240
 |</PRE>}

function ChangeHue(Color: TColor; Hue: Double): TColor;
{* 替换颜色中的色调值，返回新的颜色}
function ChangeSaturation(Color: TColor; Saturation: Double): TColor;
{* 替换颜色中的饱和度值，返回新的颜色}
function ChangeLighteness(Color: TColor; Lighteness: Double): TColor;
{* 替换颜色中的亮度值，返回新的颜色}

function AdjustHue(Color: TColor; Added: Double): TColor;
{* 调整颜色中的色调值，返回新的颜色}
function AdjustSaturation(Color: TColor; Added: Double): TColor;
{* 调整颜色中的饱和度值，返回新的颜色}
function AdjustLighteness(Color: TColor; Added: Double): TColor;
{* 调整颜色中的亮度值，返回新的颜色}

//------------------------------------------------------------------------------
// CMY 颜色与 RGB 色转换函数
//------------------------------------------------------------------------------

function CMYToRGB(const C, M, Y: Byte): TColor;
{* CMY 颜色转换为 RGB 颜色
 |<PRE>
  C, M, Y: Byte         - 分别为 Cyan 青、Magenta 品红、Yellow 黄分量，0..255
  Result: TColor        - 返回RGB颜色值
 |</PRE>}
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
{* RGB 颜色转换为 CMY 颜色
 |<PRE>
 |<BR> Color: TColor　　　RGB 颜色值
 |<BR> C, M, Y: Byte　　　输出分别为 Cyan 青、Magenta 品红、Yellow 黄分量，0..255
 |</PRE>}

//------------------------------------------------------------------------------
// CMYK 颜色与 RGB 色转换函数
//------------------------------------------------------------------------------

function CMYKToRGB(const C, M, Y, K: Byte): TColor;
{* CMYK 颜色转换为 RGB 颜色
 |<PRE>
   C, M, Y, K: Byte     - 分别为 Cyan 青、Magenta 品红、Yellow 黄、Black 黑分量，0..255
   Result: TColor       - 返回 RGB 颜色值
 |</PRE>}
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
{* RGB 颜色转换为 CMY 颜色
 |<PRE>
   Color: TColor        - RGB 颜色值
   C, M, Y, K: Byte     - 输出分别为Cyan青、Magenta品红、Yellow黄、Black黑分量，0..255
 |</PRE>}

//==============================================================================
// 增强的颜色处理函数
//==============================================================================

function Gray(Intensity: Byte): TColor;
{* 返回一个灰度 RGB 颜色值}
function Intensity(Color: TColor): Byte;
{* 计算 RGB 颜色值的灰度值}
function RandomColor: TColor;
{* 返回一个随机 RGB 颜色值}
procedure DeRGB(Color: TColor; var r, g, b: Byte);
{* 将 Color 分解为 r、g、b 颜色分量}

//==============================================================================
// 扩展的位图处理函数
//==============================================================================

function CreateEmptyBmp24(Width, Height: Integer; Color: TColor): TBitmap;
{* 创建一个以 Color 为背景色，指定大小的 24 位位图 }

function DrawBmpToIcon(Bmp: TBitmap; Icon: TIcon): Boolean;
{* 将 Bitmap 的内容放到 Icon 里}

procedure StretchDrawBmp(Src, Dst: TBitmap; Smooth: Boolean = True);
{* 将位图 Src 拉伸绘制至 Dst，支持 GDI+ 时可以使用平滑拉伸}

{$IFNDEF SUPPORT_GDIPLUS}

procedure CnStartUpGdiPlus;
{* 由于 DLL 中不允许跟着单元来初始化/释放 GDI+，所以输出给宿主调用，初始化 GDI+}
procedure CnShutDownGdiPlus;
{* 由于 DLL 中不允许跟着单元来初始化/释放 GDI+，所以输出给宿主调用，释放 GDI+}

{$ENDIF}

implementation

{$IFNDEF SUPPORT_GDIPLUS}

//==============================================================================
// 编译器不支持 GDI+ 时手工定义 GDI+ 相关函数
//==============================================================================

const
  WINGDIPDLL = 'gdiplus.dll';
  SmoothingModeInvalid     = -1;
  SmoothingModeDefault     = 0;
  SmoothingModeHighSpeed   = 1;
  SmoothingModeHighQuality = 2;
  SmoothingModeNone        = 3;
  SmoothingModeAntiAlias   = 4;

type
  GpGraphics = Pointer;
  {* GDI+ 绘图参数类，用 GdipCreateFromHDC 等创建，用 GdipDeleteGraphics 释放}

  GpImage = Pointer;
  {* GDI+ 图像基类，和子类一起用 GdipCreateBitmapFromHBITMAP 等创建，用 GdipDisposeImage 释放}

  GpBitmap = Pointer;
  {* GDI+ GpImage 的子类，代表位图}

  TStatus = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );

  GpStatus = TStatus;

  TSmoothingMode = Integer;
  {* 使用 const 中的 SmoothingMode* 值}

  TDebugEventLevel = (DebugEventLevelFatal, DebugEventLevelWarning);

  DebugEventProc = procedure(Level: TDebugEventLevel; Message: PChar); stdcall;
  NotificationHookProc = function(out Token: ULONG): TStatus; stdcall;
  NotificationUnhookProc = procedure(Token: ULONG); stdcall;

  GdiplusStartupInput = record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : DebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs  : BOOL;
  end;
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  GdiplusStartupOutput = record
    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  // GDI+ 中所需的函数声明类型
  TGdiplusStartup = function(out Token: ULONG; Input: PGdiplusStartupInput;
    Output: PGdiplusStartupOutput): GPSTATUS; stdcall;

  TGdiplusShutdown = procedure(Token: ULONG); stdcall;

  TGdipCreateFromHDC = function(hdc: HDC; out Graphic: GPGRAPHICS): GPSTATUS; stdcall;

  TGdipDeleteGraphics = function(Graphic: GPGRAPHICS): GPSTATUS; stdcall;

  TGdipSetSmoothingMode = function(Graphic: GPGRAPHICS; Sm: TSmoothingMode):
    GPSTATUS; stdcall;

  TGdipGetSmoothingMode = function(Graphic: GPGRAPHICS; var Sm: TSmoothingMode):
    GPSTATUS; stdcall;

  TGdipCreateBitmapFromHBITMAP = function(hbm: HBITMAP; hpal: HPALETTE; out
    Bitmap: GPBITMAP): GPSTATUS; stdcall;

  TGdipDisposeImage = function(Image: GPIMAGE): GPSTATUS; stdcall;

  TGdipDrawImageRect = function(Graphic: GPGRAPHICS; Image: GPIMAGE; x: Single;
    y: Single; Width: Single; Height: Single): GPSTATUS; stdcall;

  TGdipDrawImageRectI = function(Graphic: GPGRAPHICS; Image: GPIMAGE; x: Integer;
    y: Integer; Width: Integer; Height: Integer): GPSTATUS; stdcall;

var
  GdiPlusInit: Boolean = False;
  GdiPlusHandle: THandle = 0;
  StartupInput: TGDIPlusStartupInput;
  GdiplusToken: ULONG;

  GdiplusStartup: TGdiplusStartup = nil;
  GdiplusShutdown: TGdiplusShutdown = nil;
  GdipCreateFromHDC: TGdipCreateFromHDC = nil;
  GdipDeleteGraphics: TGdipDeleteGraphics = nil;
  GdipSetSmoothingMode: TGdipSetSmoothingMode = nil;
  GdipGetSmoothingMode: TGdipGetSmoothingMode = nil;
  GdipCreateBitmapFromHBITMAP: TGdipCreateBitmapFromHBITMAP = nil;
  GdipDisposeImage: TGdipDisposeImage = nil;
  GdipDrawImageRect: TGdipDrawImageRect = nil;
  GdipDrawImageRectI: TGdipDrawImageRectI = nil;

{$ENDIF}

//==============================================================================
// 扩展的颜色格式转换函数
//==============================================================================

//------------------------------------------------------------------------------
// HSL 颜色与 RGB 色转换函数
// 算法来源：
// http:/www.r2m.com/win-developer-faq/graphics/8.html
// Grahame Marsh 12 October 1997
//------------------------------------------------------------------------------

// HSL 颜色转换为 RGB 色
function HSLToRGB(H, S, L: Double): TColor;
var
  M1, M2: Double;

  procedure CheckInput(var V: Double);
  begin
    if V < 0 then V := 0;
    if V > 1 then V := 1;
  end;

  function HueToColourValue(Hue: Double): Byte;
  var
    V: Double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else if Hue > 1 then
      Hue := Hue - 1;
    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then
      V := M2
    else if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := Round(255 * V)
  end;
var
  r, g, b: Byte;
begin
  H := H - Floor(H);                   // 保证色调在 0..1 之间
  CheckInput(S);
  CheckInput(L);
  if S = 0 then
  begin
    r := Round(255 * L);
    g := r;
    b := r
  end else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    r := HueToColourValue(H + 1 / 3);
    g := HueToColourValue(H);
    b := HueToColourValue(H - 1 / 3)
  end;
  Result := RGB(r, g, b);
end;

// HSL 颜色范围转换为 RGB 色
function HSLRangeToRGB(H, S, L: Integer): TColor;
begin
  Assert(HSLRange > 1);
  Result := HSLToRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange)
end;

// RGB 颜色转为 HSL 色
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
var
  r, g, b, D, Cmax, Cmin: Double;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color) / 255;
  g := GetGValue(Color) / 255;
  b := GetBValue(Color) / 255;
  Cmax := Max(r, Max(g, b));
  Cmin := Min(r, Min(g, b));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if r = Cmax then
      H := (g - b) / D
    else if g = Cmax then
      H := 2 + (b - r) / D
    else
      H := 4 + (r - g) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

// RGB 颜色转为 HSL 色范围
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
var
  Hd, Sd, Ld: Double;
begin
  RGBToHSL(Color, Hd, Sd, Ld);
  H := Round(Hd * (HSLRange - 1));
  S := Round(Sd * HSLRange);
  L := Round(Ld * HSLRange);
end;

// 替换颜色中的色调值，返回新的颜色
function ChangeHue(Color: TColor; Hue: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(Hue, S, L);
end;

// 替换颜色中的饱和度值，返回新的颜色
function ChangeSaturation(Color: TColor; Saturation: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, Saturation, L);
end;

// 替换颜色中的亮度值，返回新的颜色
function ChangeLighteness(Color: TColor; Lighteness: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, S, Lighteness);
end;

// 调整颜色中的色调值，返回新的颜色
function AdjustHue(Color: TColor; Added: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H + Added, S, L);
end;

// 调整颜色中的饱和度值，返回新的颜色
function AdjustSaturation(Color: TColor; Added: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, S + Added, L);
end;

// 调整颜色中的亮度值，返回新的颜色
function AdjustLighteness(Color: TColor; Added: Double): TColor;
var
  H, S, L: Double;
begin
  RGBToHSL(Color, H, S, L);
  Result := HSLToRGB(H, S, L + Added);
end;

//------------------------------------------------------------------------------
// CMY 颜色与 RGB 色转换函数
// 算法提供：CnPack开发组 铁男
//------------------------------------------------------------------------------

// CMY 颜色转换为 RGB
function CMYToRGB(const C, M, Y: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - C;
  g := 255 - M;
  b := 255 - Y;
  Result := RGB(r, g, b);
end;

// RGB 颜色转换为 CMY
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
var
  r, g, b: Byte;
begin
  DeRGB(RGB, r, g, b);
  C := 255 - r;
  M := 255 - g;
  Y := 255 - b;
end;

//------------------------------------------------------------------------------
// CMYK 颜色与 RGB 色转换函数
// 算法提供：CnPack开发组 铁男
//------------------------------------------------------------------------------

// CMYK 颜色转换为 RGB
function CMYKtoRGB(const C, M, Y, K: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - (C + K);
  g := 255 - (M + K);
  b := 255 - (Y + K);
  Result := RGB(r, g, b);
end;

// RGB 颜色转换为 CMYK
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
begin
  RGBToCMY(RGB, C, M, Y);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end;

//==============================================================================
// 增强的颜色处理函数
//==============================================================================

// 产生灰度颜色
function Gray(Intensity: Byte): TColor;
begin
  Result := Intensity shl 16 + Intensity shl 8 + Intensity;
end;

// 计算颜色亮度值
// 算法来源：Graphic32
// 算法修改：周劲羽
function Intensity(Color: TColor): Byte; assembler;
asm
// 输入:  RGB --> EAX
// 输出:  (R * 61 + G * 174 + B * 20) / 256 --> AL
        MOV     ECX,EAX
        AND     EAX,$00FF00FF      // EAX <-   0 B 0 R
        IMUL    EAX,$0014003D
        AND     ECX,$0000FF00      // ECX <-   0 0 G 0
        IMUL    ECX,$0000AE00
        MOV     EDX,EAX
        SHR     ECX,8
        SHR     EDX,16
        ADD     EAX,ECX
        ADD     EAX,EDX
        SHR     EAX,8
end;

// 产生随机颜色
function RandomColor: TColor;
begin
  Result := HSLToRGB(Random, 0.75 + Random * 0.25, 0.3 + Random * 0.25);
end;

// 取颜色 RGB 分量
procedure DeRGB(Color: TColor; var r, g, b: Byte);
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
end;

//==============================================================================
// 扩展的位图处理函数
//==============================================================================

// 创建一个以 Color 为背景色，指定大小的 24 位位图
function CreateEmptyBmp24(Width, Height: Integer; Color: TColor): TBitmap;
type
  TRGBArray = array[0..65535] of TRGBTriple;
var
  r, g, b: Byte;
  x, y: Integer;
  P: ^TRGBArray;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;
  Result.Width := Width;
  Result.Height := Height;
  DeRGB(Color, r, g, b);
  for y := 0 to Height - 1 do
  begin
    P := Result.ScanLine[y];
    for x := 0 to Width - 1 do
    begin
      with P^[x] do
      begin
        rgbtBlue := b;
        rgbtGreen := g;
        rgbtRed := r;
      end;
    end;
  end;
end;

// 将 Bitmap 的内容放到 Icon 里
function DrawBmpToIcon(Bmp: TBitmap; Icon: TIcon): Boolean;
var
  ImageList: TImageList;
begin
  Result := False;
  if (Bmp = nil) or (Icon = nil) or Bmp.Empty then
    Exit;

  ImageList := TImageList.CreateSize(Bmp.Width, Bmp.Height);
  try
    ImageList.AddMasked(Bmp, Bmp.TransparentColor);
    ImageList.GetIcon(0, Icon);
    Result := True;
  finally
    ImageList.Free;
  end;
end;

procedure StretchDrawBmp(Src, Dst: TBitmap; Smooth: Boolean = True);
var
{$IFDEF SUPPORT_GDIPLUS}
  Bmp: TGPBitmap;
  GP: TGPGraphics;
{$ELSE}
  Rd: TRect;
  GP: GpGraphics;
  Bmp: GpBitmap;
  St: TStatus;
{$ENDIF}
begin
  if (Src = nil) or (Dst = nil) then
    Exit;

{$IFDEF SUPPORT_GDIPLUS} // 如果编译器天生就有 GDIPlus 支持
  GP := nil;
  Bmp := nil;
  try
    GP := TGPGraphics.Create(Dst.Canvas.Handle);
    if Smooth then
      GP.SetSmoothingMode(SmoothingModeAntiAlias);

    Bmp := TGPBitmap.Create(Src.Handle, Src.Palette);
    GP.DrawImage(Bmp, 0, 0, Dst.Width + 1, Dst.Height + 1);
  finally
    Bmp.Free;
    GP.Free;
  end;
{$ELSE}
  if (Src.Width <> Dst.Width) or (Src.Height <> Dst.Height) then
  begin
    if not GdiPlusInit then // 没有动态找到 GDIPlus 的支持
    begin
      Rd := Rect(0, 0, Dst.Width, Dst.Height);
      Dst.Canvas.StretchDraw(Rd, Src);
    end
    else
    begin
      GP := nil;
      St := GdipCreateFromHDC(Dst.Canvas.Handle, GP);
      if (St <> Ok) or (GP = nil) then
        Exit;

      try
        if Smooth then
          GdipSetSmoothingMode(GP, SmoothingModeAntiAlias);

        Bmp := nil;
        St := GdipCreateBitmapFromHBITMAP(Src.Handle, Src.Palette, Bmp);
        if (St <> Ok) or (Bmp = nil) then
          Exit;

        GdipDrawImageRectI(GP, Bmp, 0, 0, Dst.Width + 1, Dst.Height + 1);
      finally
        if Bmp <> nil then
          GdipDisposeImage(Bmp);
        if GP <> nil then
          GdipDeleteGraphics(GP);
      end;
    end
  end
  else
    Dst.Canvas.Draw(0, 0, Src);
{$ENDIF}
end;

{$IFNDEF SUPPORT_GDIPLUS}

procedure CnStartUpGdiPlus;
begin
  if not GdiPlusInit and (GdiPlusHandle <> 0) then
  begin
    StartupInput.DebugEventCallback := nil;
    StartupInput.SuppressBackgroundThread := False;
    StartupInput.SuppressExternalCodecs   := False;
    StartupInput.GdiplusVersion := 1;

    GdiplusStartup(GdiPlusToken, @StartupInput, nil);

    GdiPlusInit := True;
  end;
end;

procedure CnShutDownGdiPlus;
begin
  if GdiPlusInit then
  begin
    GdiplusShutdown(GdiplusToken);

    GdiplusToken := 0;
    GdiPlusInit := False;
  end;
end;

{$ENDIF}

{$IFNDEF SUPPORT_GDIPLUS}

initialization
  GdiPlusHandle := LoadLibrary(WINGDIPDLL);
  if GdiPlusHandle <> 0 then
  begin
    GdiplusStartup := TGdiplusStartup(GetProcAddress(GdiPlusHandle, 'GdiplusStartup'));
    Assert(Assigned(GdiplusStartup), 'Load GdiplusStartup from GDI+ DLL.');

    GdiplusShutdown := TGdiplusShutdown(GetProcAddress(GdiPlusHandle, 'GdiplusShutdown'));
    Assert(Assigned(GdiplusShutdown), 'Load GdiplusShutdown from GDI+ DLL.');

    GdipCreateFromHDC:= TGdipCreateFromHDC(GetProcAddress(GdiPlusHandle, 'GdipCreateFromHDC'));
    Assert(Assigned(GdipCreateFromHDC), 'Load GdipCreateFromHDC from GDI+ DLL.');

    GdipDeleteGraphics:= TGdipDeleteGraphics(GetProcAddress(GdiPlusHandle, 'GdipDeleteGraphics'));
    Assert(Assigned(GdipDeleteGraphics), 'Load GdipDeleteGraphics from GDI+ DLL.');

    GdipSetSmoothingMode:= TGdipSetSmoothingMode(GetProcAddress(GdiPlusHandle, 'GdipSetSmoothingMode'));
    Assert(Assigned(GdipSetSmoothingMode), 'Load GdipSetSmoothingMode from GDI+ DLL.');

    GdipGetSmoothingMode:= TGdipGetSmoothingMode(GetProcAddress(GdiPlusHandle, 'GdipGetSmoothingMode'));
    Assert(Assigned(GdipGetSmoothingMode), 'Load GdipGetSmoothingMode from GDI+ DLL.');

    GdipCreateBitmapFromHBITMAP:= TGdipCreateBitmapFromHBITMAP(GetProcAddress(GdiPlusHandle, 'GdipCreateBitmapFromHBITMAP'));
    Assert(Assigned(GdipCreateBitmapFromHBITMAP), 'Load GdipCreateBitmapFromHBITMAP from GDI+ DLL.');

    GdipDisposeImage:= TGdipDisposeImage(GetProcAddress(GdiPlusHandle, 'GdipDisposeImage'));
    Assert(Assigned(GdipDisposeImage), 'Load GdipDisposeImage from GDI+ DLL.');

    GdipDrawImageRect:= TGdipDrawImageRect(GetProcAddress(GdiPlusHandle, 'GdipDrawImageRect'));
    Assert(Assigned(GdipDrawImageRect), 'Load GdipDrawImageRect from GDI+ DLL.');

    GdipDrawImageRectI:= TGdipDrawImageRectI(GetProcAddress(GdiPlusHandle, 'GdipDrawImageRectI'));
    Assert(Assigned(GdipDrawImageRectI), 'Load GdipDrawImageRectI from GDI+ DLL.');
  end;

finalization
  if GdiPlusHandle <> 0 then
    FreeLibrary(GdiPlusHandle);

{$ENDIF}

end.
