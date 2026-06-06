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
* 修改记录：2026.06.06 V1.4
*               GDI+ 声明移出 $IFNDEF SUPPORT_GDIPLUS，统一为 $IFDEF MSWINDOWS
*               新增 CnGdiPlusAvailable 运行时标记，CnStartUpGdiPlus 成功置 True；
*               初始化节统一两条编译路径，去掉 Assert 改静默失败；
*               GdiPlusInit 变量由 CnGdiPlusAvailable 替代
*           2026.06.05 V1.3
*               大幅扩充 GDI+ 动态导入声明，新增 Pen/Brush/Path/Matrix 等
*               SVG 渲染所需的 50 余个函数，声明移至 interface 段以便外部调用
*           2024.06.09 V1.2
*               加入几个高版本的 TPoint/TRect 封装函数
*           2021.09.28 V1.1
*               加入一个平滑拉伸绘制位图的函数，使用 GDI+
*           2002.10.20 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF MSWINDOWS} Windows, {$ELSE} MacTypes, ObjcBase, Types, {$ENDIF} Graphics, Math, Classes, Controls
  {$IFDEF SUPPORT_GDIPLUS}, WinApi.GDIPOBJ, WinApi.GDIPAPI {$ENDIF};

{$IFDEF FPC}
{$IFDEF DARWIN}
type
  ULONG = Cardinal;
  BOOL  = LongBool;
{$ENDIF}
{$ENDIF}

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
 |<BR> Color: TColor      RGB 颜色值
 |<BR> C, M, Y: Byte      输出分别为 Cyan 青、Magenta 品红、Yellow 黄分量，0..255
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

//==============================================================================
// 高版本 Rect、Point 等函数的低版本实现
//==============================================================================

function CnCreatePoint(X, Y: Integer): TPoint;
{* 根据 X、Y 坐标创建一个点}

function CnGetRectWidth(const Rect: TRect): Integer;
{* 返回 TRect 的宽度}

function CnGetRectHeight(const Rect: TRect): Integer;
{* 返回 TRect 的高度}

function CnGetRectCenter(const Rect: TRect): TPoint;
{* 返回 TRect 的中心点坐标}

function CnGetRectIsEmpty(const Rect: TRect): Boolean;
{* 返回 TRect 是否为空}

procedure CnSetRectWidth(var Rect: TRect; Value: Integer);
{* 设置一 TRect 的宽度}

procedure CnSetRectHeight(var Rect: TRect; Value: Integer);
{* 设置一 TRect 的高度}

procedure CnRectInflate(var Rect: TRect; DX, DY: Integer);
{* 放大一个 TRect。DX/DY 是正值会增加宽度和高度，负值会减小宽度和高度}

procedure CnRectOffset(var Rect: TRect; DX, DY: Integer);
{* 偏移一个 TRect}

procedure CnRectCopy(const Source: TRect; var Dest: TRect);
{* 复制一个 Rect}

function CnRectContains(const Rect: TRect; const PT: TPoint): Boolean;
{* 返回一 TRect 是否包含一个点，注意包含左上边，但不包含右下边}

procedure CnSetRectLocation(var Rect: TRect; const X, Y: Integer); overload;
{* 设置 TRect 的左上角坐标，参数为 X、Y 坐标}

procedure CnSetRectLocation(var Rect: TRect; const P: TPoint); overload;
{* 设置 TRect 的左上角坐标，参数为一个点}

procedure CnCanvasRoundRect(const Canvas: TCanvas; const Rect: TRect; CX, CY: Integer);
{* 在 Canvas 上绘制圆角矩形}

//==============================================================================
// GDI+ 扁平 API 统一声明（编译路径无关）
// 高版本 Delphi（SUPPORT_GDIPLUS）和低版本 Delphi 共用此声明。
// 高版本通过 GetModuleHandle + GetProcAddress 填充函数变量（DLL 已被
// WinApi.GDIPAPI 静态导入自动加载）；低版本通过 LoadLibrary + GetProcAddress
// 动态加载。运行时通过 CnGdiPlusAvailable 判断 GDI+ 是否可用。
//==============================================================================

{$IFDEF MSWINDOWS}

procedure CnStartUpGdiPlus;
{* 初始化 GDI+ 令牌。成功后置 CnGdiPlusAvailable := True。
     EXE 项目在单元初始化时自动调用；
     DLL 项目必须在安全时机（非 DLL_PROCESS_ATTACH）手动调用。 }

procedure CnShutDownGdiPlus;
{* 关闭 GDI+ 令牌，置 CnGdiPlusAvailable := False。 }

function MakeGDIPColor(A, R, G, B: Byte): Cardinal;
{* 构造 GDI+ ARGB 颜色值（$AARRGGBB）}

const
  WINGDIPDLL = 'gdiplus.dll';

  // SmoothingMode 枚举值
  SmoothingModeInvalid     = -1;
  SmoothingModeDefault     = 0;
  SmoothingModeHighSpeed   = 1;
  SmoothingModeHighQuality = 2;
  SmoothingModeNone        = 3;
  SmoothingModeAntiAlias   = 4;
  SmoothingModeHighQualityGDI = 5;

  // FillMode 枚举值
  FillModeAlternate   = 0;  // 奇偶填充，对应 GDI 的 ALTERNATE
  FillModeWinding     = 1;  // 缠绕填充，对应 GDI 的 WINDING

  // LineCap 枚举值
  LineCapFlat         = 0;
  LineCapSquare       = 1;
  LineCapRound        = 2;
  LineCapTriangle     = 3;
  LineCapNoAnchor     = $10;
  LineCapSquareAnchor = $11;
  LineCapRoundAnchor  = $12;
  LineCapDiamondAnchor = $13;
  LineCapArrowAnchor  = $14;
  LineCapCustom       = $FF;

  // LineJoin 枚举值
  LineJoinMiter        = 0;
  LineJoinBevel        = 1;
  LineJoinRound        = 2;
  LineJoinMiterClipped = 3;

  // PenDashStyle 枚举值
  DashStyleSolid           = 0;
  DashStyleDash            = 1;
  DashStyleDot             = 2;
  DashStyleDashDot         = 3;
  DashStyleDashDotDot      = 4;
  DashStyleCustom          = 5;

type
  GpGraphics = Pointer;
  {* GDI+ 绘图上下文，用 GdipCreateFromHDC 等创建，用 GdipDeleteGraphics 释放}

  GpImage = Pointer;
  {* GDI+ 图像基类，和子类一起用 GdipCreateBitmapFromHBITMAP 等创建，用 GdipDisposeImage 释放}

  GpBitmap = Pointer;
  {* GDI+ GpImage 的子类，代表位图}

  GpPen = Pointer;
  {* GDI+ 画笔对象，用 GdipCreatePen1 等创建，用 GdipDeletePen 释放}

  GpBrush = Pointer;
  {* GDI+ 画刷基类}

  GpSolidFill = Pointer;
  {* GDI+ 实心画刷，用 GdipCreateSolidFill 创建，用 GdipDeleteBrush 释放}

  GpPath = Pointer;
  {* GDI+ 路径对象，用 GdipCreatePath 创建，用 GdipDeletePath 释放}

  GpPathData = Pointer;
  {* GDI+ 路径数据}

  GpMatrix = Pointer;
  {* GDI+ 矩阵对象，用 GdipCreateMatrix 等创建，用 GdipDeleteMatrix 释放}

  GpFontFamily = Pointer;
  {* GDI+ 字体族对象}

  GpFont = Pointer;
  {* GDI+ 字体对象}

  GpStringFormat = Pointer;
  {* GDI+ 字符串格式对象}

  GpCachedBitmap = Pointer;
  {* GDI+ 缓存位图对象}

  TGPRectF = record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
  end;
  {* GDI+ 浮点矩形，用于 GdipDrawString / GdipMeasureString 的布局和边界框 }

  TGPColor = Cardinal;

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
  TFillMode = Integer;
  TLineCap = Integer;
  TLineJoin = Integer;
  TDashStyle = Integer;

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

  // GDI+ 扁平 API 函数声明类型

  //---------- 初始化/关闭 ----------
  TGdiplusStartup = function(out Token: ULONG; Input: PGdiplusStartupInput;
    Output: PGdiplusStartupOutput): GPSTATUS; stdcall;

  TGdiplusShutdown = procedure(Token: ULONG); stdcall;

  //---------- Graphics 对象 ----------
  TGdipCreateFromHDC = function(hdc: HDC; out Graphic: GPGRAPHICS): GPSTATUS; stdcall;

  TGdipDeleteGraphics = function(Graphic: GPGRAPHICS): GPSTATUS; stdcall;

  TGdipSetSmoothingMode = function(Graphic: GPGRAPHICS; Sm: TSmoothingMode):
    GPSTATUS; stdcall;

  TGdipGetSmoothingMode = function(Graphic: GPGRAPHICS; var Sm: TSmoothingMode):
    GPSTATUS; stdcall;

  TGdipSaveGraphics = function(Graphic: GPGRAPHICS; var State: Cardinal):
    GPSTATUS; stdcall;

  TGdipRestoreGraphics = function(Graphic: GPGRAPHICS; State: Cardinal):
    GPSTATUS; stdcall;

  TGdipSetWorldTransform = function(Graphic: GPGRAPHICS;
    Matrix: GPMATRIX): GPSTATUS; stdcall;

  TGdipMultiplyWorldTransform = function(Graphic: GPGRAPHICS;
    Matrix: GPMATRIX; Order: Integer): GPSTATUS; stdcall;

  TGdipSetClipRectI = function(Graphic: GPGRAPHICS; X, Y, Width, Height: Integer):
    GPSTATUS; stdcall;

  //---------- Pen 画笔 ----------
  TGdipCreatePen1 = function(Color: TGPColor; Width: Single;
    Unit_: Integer; out Pen: GPPEN): GPSTATUS; stdcall;

  TGdipDeletePen = function(Pen: GPPEN): GPSTATUS; stdcall;

  TGdipSetPenWidth = function(Pen: GPPEN; Width: Single): GPSTATUS; stdcall;

  TGdipSetPenColor = function(Pen: GPPEN; Color: TGPColor): GPSTATUS; stdcall;

  TGdipSetPenLineCap = function(Pen: GPPEN; StartCap, EndCap, DashCap: TLineCap):
    GPSTATUS; stdcall;

  TGdipSetPenLineJoin = function(Pen: GPPEN; LineJoin: TLineJoin):
    GPSTATUS; stdcall;

  TGdipSetPenMiterLimit = function(Pen: GPPEN; MiterLimit: Single):
    GPSTATUS; stdcall;

  TGdipSetPenDashStyle = function(Pen: GPPEN; DashStyle: TDashStyle):
    GPSTATUS; stdcall;

  TGdipSetPenDashArray = function(Pen: GPPEN; DashArr: PSingle;
    Count: Integer): GPSTATUS; stdcall;

  TGdipSetPenDashOffset = function(Pen: GPPEN; DashOffset: Single):
    GPSTATUS; stdcall;

  //---------- Brush 画刷 ----------
  TGdipCreateSolidFill = function(Color: TGPColor;
    out Brush: GPSOLIDFILL): GPSTATUS; stdcall;

  TGdipDeleteBrush = function(Brush: GPBRUSH): GPSTATUS; stdcall;

  TGdipSetSolidFillColor = function(Brush: GPSOLIDFILL;
    Color: TGPColor): GPSTATUS; stdcall;

  //---------- Path 路径 ----------
  TGdipCreatePath = function(FillMode: TFillMode;
    out Path: GPPATH): GPSTATUS; stdcall;

  TGdipDeletePath = function(Path: GPPATH): GPSTATUS; stdcall;

  TGdipResetPath = function(Path: GPPATH): GPSTATUS; stdcall;

  TGdipAddPathLine = function(Path: GPPATH; X1, Y1, X2, Y2: Single):
    GPSTATUS; stdcall;

  TGdipAddPathLineI = function(Path: GPPATH; X1, Y1, X2, Y2: Integer):
    GPSTATUS; stdcall;

  TGdipAddPathLines = function(Path: GPPATH; Points: PPoint;
    Count: Integer): GPSTATUS; stdcall;

  TGdipAddPathArc = function(Path: GPPATH; X, Y, Width, Height: Single;
    StartAngle, SweepAngle: Single): GPSTATUS; stdcall;

  TGdipAddPathArcI = function(Path: GPPATH; X, Y, Width, Height: Integer;
    StartAngle, SweepAngle: Single): GPSTATUS; stdcall;

  TGdipAddPathBezier = function(Path: GPPATH; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single):
    GPSTATUS; stdcall;

  TGdipAddPathBezierI = function(Path: GPPATH; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer):
    GPSTATUS; stdcall;

  TGdipAddPathRectangle = function(Path: GPPATH; X, Y, Width, Height: Single):
    GPSTATUS; stdcall;

  TGdipAddPathEllipse = function(Path: GPPATH; X, Y, Width, Height: Single):
    GPSTATUS; stdcall;

  TGdipClosePathFigure = function(Path: GPPATH): GPSTATUS; stdcall;

  TGdipStartPathFigure = function(Path: GPPATH): GPSTATUS; stdcall;

  TGdipSetPathFillMode = function(Path: GPPATH; FillMode: TFillMode):
    GPSTATUS; stdcall;

  TGdipGetPathPointCount = function(Path: GPPATH;
    var Count: Integer): GPSTATUS; stdcall;

  //---------- Graphics 绘制操作 ----------
  TGdipDrawLineI = function(Graphic: GPGRAPHICS; Pen: GPPEN;
    X1, Y1, X2, Y2: Integer): GPSTATUS; stdcall;

  TGdipDrawLinesI = function(Graphic: GPGRAPHICS; Pen: GPPEN;
    Points: PPoint; Count: Integer): GPSTATUS; stdcall;

  TGdipDrawRectangle = function(Graphic: GPGRAPHICS; Pen: GPPEN;
    X, Y, Width, Height: Single): GPSTATUS; stdcall;

  TGdipDrawEllipse = function(Graphic: GPGRAPHICS; Pen: GPPEN;
    X, Y, Width, Height: Single): GPSTATUS; stdcall;

  TGdipFillRectangleI = function(Graphic: GPGRAPHICS; Brush: GPBRUSH;
    X, Y, Width, Height: Integer): GPSTATUS; stdcall;

  TGdipFillEllipse = function(Graphic: GPGRAPHICS; Brush: GPBRUSH;
    X, Y, Width, Height: Single): GPSTATUS; stdcall;

  TGdipFillPolygonI = function(Graphic: GPGRAPHICS; Brush: GPBRUSH;
    Points: PPoint; Count: Integer; FillMode: TFillMode): GPSTATUS; stdcall;

  TGdipFillPath = function(Graphic: GPGRAPHICS; Brush: GPBRUSH;
    Path: GPPATH): GPSTATUS; stdcall;

  TGdipDrawPath = function(Graphic: GPGRAPHICS; Pen: GPPEN;
    Path: GPPATH): GPSTATUS; stdcall;

  //---------- Image/Bitmap ----------
  TGdipCreateBitmapFromHBITMAP = function(hbm: HBITMAP; hpal: HPALETTE; out
    Bitmap: GPBITMAP): GPSTATUS; stdcall;

  TGdipLoadImageFromFile = function(FileName: PWideChar;
    out Image: GPIMAGE): GPSTATUS; stdcall;

  TGdipDisposeImage = function(Image: GPIMAGE): GPSTATUS; stdcall;

  TGdipDrawImageRect = function(Graphic: GPGRAPHICS; Image: GPIMAGE; x: Single;
    y: Single; Width: Single; Height: Single): GPSTATUS; stdcall;

  TGdipDrawImageRectI = function(Graphic: GPGRAPHICS; Image: GPIMAGE; x: Integer;
    y: Integer; Width: Integer; Height: Integer): GPSTATUS; stdcall;

  //---------- Font 字体 ----------
  TGdipCreateFontFromLogfontW = function(hdc: HDC; Logfont: Pointer;
    out Font: GPFONT): GPSTATUS; stdcall;

  TGdipDeleteFont = function(Font: GPFONT): GPSTATUS; stdcall;

  //---------- StringFormat 字符串格式 ----------
  TGdipCreateStringFormat = function(FormatAttributes: Integer; Language: Word;
    out Format: GPSTRINGFORMAT): GPSTATUS; stdcall;

  TGdipDeleteStringFormat = function(Format: GPSTRINGFORMAT): GPSTATUS; stdcall;

  TGdipSetStringFormatAlign = function(Format: GPSTRINGFORMAT;
    Align: Integer): GPSTATUS; stdcall;

  TGdipSetStringFormatLineAlign = function(Format: GPSTRINGFORMAT;
    Align: Integer): GPSTATUS; stdcall;

  //---------- Text 文字绘制 ----------
  TGdipDrawString = function(Graphics: GPGRAPHICS; Str: PWideChar;
    Length: Integer; Font: GPFONT; LayoutRect: Pointer;
    Format: GPSTRINGFORMAT; Brush: GPBRUSH): GPSTATUS; stdcall;

  TGdipMeasureString = function(Graphics: GPGRAPHICS; Str: PWideChar;
    Length: Integer; Font: GPFONT; LayoutRect: Pointer;
    Format: GPSTRINGFORMAT; BoundingBox: Pointer;
    CodepointsFitted: PInteger; LinesFilled: PInteger): GPSTATUS; stdcall;

  TGdipSetTextRenderingHint = function(Graphics: GPGRAPHICS;
    Mode: Integer): GPSTATUS; stdcall;

  //---------- Matrix 矩阵 ----------
  TGdipCreateMatrix = function(out Matrix: GPMATRIX): GPSTATUS; stdcall;

  TGdipCreateMatrix2 = function(m11, m12, m21, m22, dx, dy: Single;
    out Matrix: GPMATRIX): GPSTATUS; stdcall;

  TGdipDeleteMatrix = function(Matrix: GPMATRIX): GPSTATUS; stdcall;

  TGdipSetMatrixElements = function(Matrix: GPMATRIX;
    m11, m12, m21, m22, dx, dy: Single): GPSTATUS; stdcall;

  TGdipMultiplyMatrix = function(Matrix: GPMATRIX; Matrix2: GPMATRIX;
    Order: Integer): GPSTATUS; stdcall;

var
  CnGdiPlusAvailable: Boolean = False;
  {* GDI+ 运行时可用性标记。
     - SUPPORT_GDIPLUS 路径：静态链接保证 DLL 可用，CnStartUpGdiPlus 成功后为 True
     - 动态路径：取决于 gdiplus.dll 是否存在及 CnStartUpGdiPlus 是否成功
     - CnSVG 等消费方据此选择 GDI+ 或纯 GDI 渲染路径 }

  GdiPlusHandle: THandle = 0;
  StartupInput: TGDIPlusStartupInput;
  GdiplusToken: ULONG;

  //---------- 初始化/关闭 ----------
  GdiplusStartup: TGdiplusStartup = nil;
  GdiplusShutdown: TGdiplusShutdown = nil;

  //---------- Graphics 对象 ----------
  GdipCreateFromHDC: TGdipCreateFromHDC = nil;
  GdipDeleteGraphics: TGdipDeleteGraphics = nil;
  GdipSetSmoothingMode: TGdipSetSmoothingMode = nil;
  GdipGetSmoothingMode: TGdipGetSmoothingMode = nil;
  GdipSaveGraphics: TGdipSaveGraphics = nil;
  GdipRestoreGraphics: TGdipRestoreGraphics = nil;
  GdipSetWorldTransform: TGdipSetWorldTransform = nil;
  GdipMultiplyWorldTransform: TGdipMultiplyWorldTransform = nil;
  GdipSetClipRectI: TGdipSetClipRectI = nil;

  //---------- Pen 画笔 ----------
  GdipCreatePen1: TGdipCreatePen1 = nil;
  GdipDeletePen: TGdipDeletePen = nil;
  GdipSetPenWidth: TGdipSetPenWidth = nil;
  GdipSetPenColor: TGdipSetPenColor = nil;
  GdipSetPenLineCap: TGdipSetPenLineCap = nil;
  GdipSetPenLineJoin: TGdipSetPenLineJoin = nil;
  GdipSetPenMiterLimit: TGdipSetPenMiterLimit = nil;
  GdipSetPenDashStyle: TGdipSetPenDashStyle = nil;
  GdipSetPenDashArray: TGdipSetPenDashArray = nil;
  GdipSetPenDashOffset: TGdipSetPenDashOffset = nil;

  //---------- Brush 画刷 ----------
  GdipCreateSolidFill: TGdipCreateSolidFill = nil;
  GdipDeleteBrush: TGdipDeleteBrush = nil;
  GdipSetSolidFillColor: TGdipSetSolidFillColor = nil;

  //---------- Path 路径 ----------
  GdipCreatePath: TGdipCreatePath = nil;
  GdipDeletePath: TGdipDeletePath = nil;
  GdipResetPath: TGdipResetPath = nil;
  GdipAddPathLine: TGdipAddPathLine = nil;
  GdipAddPathLineI: TGdipAddPathLineI = nil;
  GdipAddPathLines: TGdipAddPathLines = nil;
  GdipAddPathArc: TGdipAddPathArc = nil;
  GdipAddPathArcI: TGdipAddPathArcI = nil;
  GdipAddPathBezier: TGdipAddPathBezier = nil;
  GdipAddPathBezierI: TGdipAddPathBezierI = nil;
  GdipAddPathRectangle: TGdipAddPathRectangle = nil;
  GdipAddPathEllipse: TGdipAddPathEllipse = nil;
  GdipClosePathFigure: TGdipClosePathFigure = nil;
  GdipStartPathFigure: TGdipStartPathFigure = nil;
  GdipSetPathFillMode: TGdipSetPathFillMode = nil;
  GdipGetPathPointCount: TGdipGetPathPointCount = nil;

  //---------- Graphics 绘制操作 ----------
  GdipDrawLineI: TGdipDrawLineI = nil;
  GdipDrawLinesI: TGdipDrawLinesI = nil;
  GdipDrawRectangle: TGdipDrawRectangle = nil;
  GdipDrawEllipse: TGdipDrawEllipse = nil;
  GdipFillRectangleI: TGdipFillRectangleI = nil;
  GdipFillEllipse: TGdipFillEllipse = nil;
  GdipFillPolygonI: TGdipFillPolygonI = nil;
  GdipFillPath: TGdipFillPath = nil;
  GdipDrawPath: TGdipDrawPath = nil;

  //---------- Image/Bitmap（非核心，允许缺失）----------
  GdipCreateBitmapFromHBITMAP: TGdipCreateBitmapFromHBITMAP = nil;
  GdipLoadImageFromFile: TGdipLoadImageFromFile = nil;
  GdipDisposeImage: TGdipDisposeImage = nil;
  GdipDrawImageRect: TGdipDrawImageRect = nil;
  GdipDrawImageRectI: TGdipDrawImageRectI = nil;

  //---------- Matrix 矩阵 ----------
  GdipCreateMatrix: TGdipCreateMatrix = nil;
  GdipCreateMatrix2: TGdipCreateMatrix2 = nil;
  GdipDeleteMatrix: TGdipDeleteMatrix = nil;
  GdipSetMatrixElements: TGdipSetMatrixElements = nil;
  GdipMultiplyMatrix: TGdipMultiplyMatrix = nil;

  //---------- Font 字体（非核心，允许缺失，有 GDI 回退）----------
  GdipCreateFontFromLogfontW: TGdipCreateFontFromLogfontW = nil;
  GdipDeleteFont: TGdipDeleteFont = nil;

  //---------- StringFormat 字符串格式（非核心）----------
  GdipCreateStringFormat: TGdipCreateStringFormat = nil;
  GdipDeleteStringFormat: TGdipDeleteStringFormat = nil;
  GdipSetStringFormatAlign: TGdipSetStringFormatAlign = nil;
  GdipSetStringFormatLineAlign: TGdipSetStringFormatLineAlign = nil;

  //---------- Text 文字绘制（非核心）----------
  GdipDrawString: TGdipDrawString = nil;
  GdipMeasureString: TGdipMeasureString = nil;
  GdipSetTextRenderingHint: TGdipSetTextRenderingHint = nil;

{$ENDIF}

function FontEqual(A, B: TFont): Boolean;
{* 比较俩字体对象的各属性是否相等}

implementation

{$IFDEF MSWINDOWS}

function MakeGDIPColor(A, R, G, B: Byte): Cardinal;
begin
  Result := (Cardinal(A) shl 24) or (Cardinal(R) shl 16) or
    (Cardinal(G) shl 8) or Cardinal(B);
end;

procedure CnStartUpGdiPlus;
var
  Status: Integer;
begin
  if CnGdiPlusAvailable then
    Exit;
  if (GdiPlusHandle = 0) or not Assigned(GdiplusStartup) then
    Exit;

  StartupInput.GdiplusVersion := 1;
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs := False;

  Status := Ord(GdiplusStartup(GdiplusToken, @StartupInput, nil));
  if Status = Ord(Ok) then
    CnGdiPlusAvailable := True;
  // 失败时 CnGdiPlusAvailable 保持 False，消费方降级为纯 GDI
end;

procedure CnShutDownGdiPlus;
begin
  if CnGdiPlusAvailable then
  begin
    if Assigned(GdiplusShutdown) then
      GdiplusShutdown(GdiplusToken);
    GdiplusToken := 0;
    CnGdiPlusAvailable := False;
  end;
end;

{$ELSE}  // 非 Windows 平台补充声明实现

type
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

function RGB(r, g, b: Byte): TColor;
begin
  Result := (r or (g shl 8) or (b shl 16));
end;

function GetRValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetBValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

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

{$IFDEF MSWINDOWS}

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
    if not CnGdiPlusAvailable then // GDI+ 运行时不可用
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

{$ELSE}

procedure StretchDrawBmp(Src, Dst: TBitmap; Smooth: Boolean = True);
var
  Rd: TRect;
begin
  Rd := Rect(0, 0, Dst.Width, Dst.Height);
  Dst.Canvas.StretchDraw(Rd, Src);
end;

{$ENDIF}

function FontEqual(A, B: TFont): Boolean;
begin
  if (A = nil) and (B = nil) then
  begin
    Result := True;
    Exit;
  end
  else if (A = nil) or (B = nil) then
  begin
    Result := False;
    Exit
  end
  else
  begin
    Result := False;

    if A.Name <> B.Name then
      Exit;
    if A.Size <> B.Size then
      Exit;
    if A.Style <> B.Style then
      Exit;
    if A.Color <> B.Color then
      Exit;
    if A.Height <> B.Height then
      Exit;
    if A.Charset <> B.Charset then
      Exit;
    if A.Pitch <> B.Pitch then
      Exit;
    if A.PixelsPerInch <> B.PixelsPerInch then
      Exit;

    Result := True;
  end;
end;
function CnCreatePoint(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function CnGetRectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function CnGetRectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function CnGetRectCenter(const Rect: TRect): TPoint;
begin
  Result.X := (Rect.Right - Rect.Left) div 2 + Rect.Left;
  Result.Y := (Rect.Bottom - Rect.Top) div 2 + Rect.Top;
end;

function CnGetRectIsEmpty(const Rect: TRect): Boolean;
begin
  Result := (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top);
end;

procedure CnSetRectWidth(var Rect: TRect; Value: Integer);
begin
  Rect.Right := Rect.Left + Value;
end;

procedure CnSetRectHeight(var Rect: TRect; Value: Integer);
begin
  Rect.Bottom := Rect.Top + Value;
end;

procedure CnRectInflate(var Rect: TRect; DX, DY: Integer);
begin
  Rect.Left := Rect.Left - DX;
  Rect.Right := Rect.Right + DX;
  Rect.Top := Rect.Top - DY;
  Rect.Bottom := Rect.Bottom + DY;
end;

procedure CnRectOffset(var Rect: TRect; DX, DY: Integer);
begin
  if @Rect <> nil then
  begin
    Inc(Rect.Left, DX);
    Inc(Rect.Right, DX);
    Inc(Rect.Top, DY);
    Inc(Rect.Bottom, DY);
  end;
end;

procedure CnRectCopy(const Source: TRect; var Dest: TRect);
begin
  if (@Source <> nil) and (@Dest <> nil) then
  begin
    Dest.Left := Source.Left;
    Dest.Top := Source.Top;
    Dest.Right := Source.Right;
    Dest.Bottom := Source.Bottom;
  end;
end;

function CnRectContains(const Rect: TRect; const PT: TPoint): Boolean;
begin
  Result := (PT.X >= Rect.Left) and (PT.X < Rect.Right) and (PT.Y >= Rect.Top)
    and (PT.Y < Rect.Bottom);
end;

procedure CnSetRectLocation(var Rect: TRect; const X, Y: Integer);
begin
  OffsetRect(Rect, X - Rect.Left, Y - Rect.Top);
end;

procedure CnSetRectLocation(var Rect: TRect; const P: TPoint);
begin
  CnSetRectLocation(Rect, P.X, P.Y);
end;

procedure CnCanvasRoundRect(const Canvas: TCanvas; const Rect: TRect; CX, CY: Integer);
begin
  if Canvas <> nil then
    Canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, CX, CY);
end;

{$IFDEF MSWINDOWS}

initialization
  // ── 第一步：获取 gdiplus.dll 模块句柄 ──
  {$IFDEF SUPPORT_GDIPLUS}
  // 高版本 Delphi：DLL 已被 WinApi.GDIPAPI 静态导入自动加载
  GdiPlusHandle := GetModuleHandle(WINGDIPDLL);
  {$ELSE}
  // 低版本 Delphi：动态加载 gdiplus.dll
  GdiPlusHandle := LoadLibrary(WINGDIPDLL);
  {$ENDIF}

  // ── 第二步：填充所有函数变量（两条编译路径共用） ──
  // 每个 GetProcAddress 结果暂存，第三步统一做完整性校验
  if GdiPlusHandle <> 0 then
  begin
    //---------- 初始化/关闭 ----------
    GdiplusStartup := TGdiplusStartup(GetProcAddress(GdiPlusHandle, 'GdiplusStartup'));
    GdiplusShutdown := TGdiplusShutdown(GetProcAddress(GdiPlusHandle, 'GdiplusShutdown'));

    //---------- Graphics 对象 ----------
    GdipCreateFromHDC := TGdipCreateFromHDC(GetProcAddress(GdiPlusHandle, 'GdipCreateFromHDC'));
    GdipDeleteGraphics := TGdipDeleteGraphics(GetProcAddress(GdiPlusHandle, 'GdipDeleteGraphics'));
    GdipSetSmoothingMode := TGdipSetSmoothingMode(GetProcAddress(GdiPlusHandle, 'GdipSetSmoothingMode'));
    GdipGetSmoothingMode := TGdipGetSmoothingMode(GetProcAddress(GdiPlusHandle, 'GdipGetSmoothingMode'));
    GdipSaveGraphics := TGdipSaveGraphics(GetProcAddress(GdiPlusHandle, 'GdipSaveGraphics'));
    GdipRestoreGraphics := TGdipRestoreGraphics(GetProcAddress(GdiPlusHandle, 'GdipRestoreGraphics'));
    GdipSetWorldTransform := TGdipSetWorldTransform(GetProcAddress(GdiPlusHandle, 'GdipSetWorldTransform'));
    GdipMultiplyWorldTransform := TGdipMultiplyWorldTransform(GetProcAddress(GdiPlusHandle, 'GdipMultiplyWorldTransform'));
    GdipSetClipRectI := TGdipSetClipRectI(GetProcAddress(GdiPlusHandle, 'GdipSetClipRectI'));

    //---------- Pen 画笔 ----------
    GdipCreatePen1 := TGdipCreatePen1(GetProcAddress(GdiPlusHandle, 'GdipCreatePen1'));
    GdipDeletePen := TGdipDeletePen(GetProcAddress(GdiPlusHandle, 'GdipDeletePen'));
    GdipSetPenWidth := TGdipSetPenWidth(GetProcAddress(GdiPlusHandle, 'GdipSetPenWidth'));
    GdipSetPenColor := TGdipSetPenColor(GetProcAddress(GdiPlusHandle, 'GdipSetPenColor'));
    GdipSetPenLineCap := TGdipSetPenLineCap(GetProcAddress(GdiPlusHandle, 'GdipSetPenLineCap197819'));
    GdipSetPenLineJoin := TGdipSetPenLineJoin(GetProcAddress(GdiPlusHandle, 'GdipSetPenLineJoin'));
    GdipSetPenMiterLimit := TGdipSetPenMiterLimit(GetProcAddress(GdiPlusHandle, 'GdipSetPenMiterLimit'));
    GdipSetPenDashStyle := TGdipSetPenDashStyle(GetProcAddress(GdiPlusHandle, 'GdipSetPenDashStyle'));
    GdipSetPenDashArray := TGdipSetPenDashArray(GetProcAddress(GdiPlusHandle, 'GdipSetPenDashArray'));
    GdipSetPenDashOffset := TGdipSetPenDashOffset(GetProcAddress(GdiPlusHandle, 'GdipSetPenDashOffset'));

    //---------- Brush 画刷 ----------
    GdipCreateSolidFill := TGdipCreateSolidFill(GetProcAddress(GdiPlusHandle, 'GdipCreateSolidFill'));
    GdipDeleteBrush := TGdipDeleteBrush(GetProcAddress(GdiPlusHandle, 'GdipDeleteBrush'));
    GdipSetSolidFillColor := TGdipSetSolidFillColor(GetProcAddress(GdiPlusHandle, 'GdipSetSolidFillColor'));

    //---------- Path 路径 ----------
    GdipCreatePath := TGdipCreatePath(GetProcAddress(GdiPlusHandle, 'GdipCreatePath'));
    GdipDeletePath := TGdipDeletePath(GetProcAddress(GdiPlusHandle, 'GdipDeletePath'));
    GdipResetPath := TGdipResetPath(GetProcAddress(GdiPlusHandle, 'GdipResetPath'));
    GdipAddPathLine := TGdipAddPathLine(GetProcAddress(GdiPlusHandle, 'GdipAddPathLine'));
    GdipAddPathLineI := TGdipAddPathLineI(GetProcAddress(GdiPlusHandle, 'GdipAddPathLineI'));
    GdipAddPathLines := TGdipAddPathLines(GetProcAddress(GdiPlusHandle, 'GdipAddPathLine2'));
    GdipAddPathArc := TGdipAddPathArc(GetProcAddress(GdiPlusHandle, 'GdipAddPathArc'));
    GdipAddPathArcI := TGdipAddPathArcI(GetProcAddress(GdiPlusHandle, 'GdipAddPathArcI'));
    GdipAddPathBezier := TGdipAddPathBezier(GetProcAddress(GdiPlusHandle, 'GdipAddPathBezier'));
    GdipAddPathBezierI := TGdipAddPathBezierI(GetProcAddress(GdiPlusHandle, 'GdipAddPathBezierI'));
    GdipAddPathRectangle := TGdipAddPathRectangle(GetProcAddress(GdiPlusHandle, 'GdipAddPathRectangle'));
    GdipAddPathEllipse := TGdipAddPathEllipse(GetProcAddress(GdiPlusHandle, 'GdipAddPathEllipse'));
    GdipClosePathFigure := TGdipClosePathFigure(GetProcAddress(GdiPlusHandle, 'GdipClosePathFigure'));
    GdipStartPathFigure := TGdipStartPathFigure(GetProcAddress(GdiPlusHandle, 'GdipStartPathFigure'));
    GdipSetPathFillMode := TGdipSetPathFillMode(GetProcAddress(GdiPlusHandle, 'GdipSetPathFillMode'));
    // 该函数在 Windows 7 SP1 上可能没有，不强制检查
    GdipGetPathPointCount := TGdipGetPathPointCount(GetProcAddress(GdiPlusHandle, 'GdipGetPathPointCount'));

    //---------- Graphics 绘制操作 ----------
    GdipDrawLineI := TGdipDrawLineI(GetProcAddress(GdiPlusHandle, 'GdipDrawLineI'));
    GdipDrawLinesI := TGdipDrawLinesI(GetProcAddress(GdiPlusHandle, 'GdipDrawLinesI'));
    GdipDrawRectangle := TGdipDrawRectangle(GetProcAddress(GdiPlusHandle, 'GdipDrawRectangle'));
    GdipDrawEllipse := TGdipDrawEllipse(GetProcAddress(GdiPlusHandle, 'GdipDrawEllipse'));
    GdipFillRectangleI := TGdipFillRectangleI(GetProcAddress(GdiPlusHandle, 'GdipFillRectangleI'));
    GdipFillEllipse := TGdipFillEllipse(GetProcAddress(GdiPlusHandle, 'GdipFillEllipse'));
    GdipFillPolygonI := TGdipFillPolygonI(GetProcAddress(GdiPlusHandle, 'GdipFillPolygonI'));
    GdipFillPath := TGdipFillPath(GetProcAddress(GdiPlusHandle, 'GdipFillPath'));
    GdipDrawPath := TGdipDrawPath(GetProcAddress(GdiPlusHandle, 'GdipDrawPath'));

    //---------- Image/Bitmap（非核心）----------
    GdipCreateBitmapFromHBITMAP := TGdipCreateBitmapFromHBITMAP(GetProcAddress(GdiPlusHandle, 'GdipCreateBitmapFromHBITMAP'));
    GdipLoadImageFromFile := TGdipLoadImageFromFile(GetProcAddress(GdiPlusHandle, 'GdipLoadImageFromFile'));
    GdipDisposeImage := TGdipDisposeImage(GetProcAddress(GdiPlusHandle, 'GdipDisposeImage'));
    GdipDrawImageRect := TGdipDrawImageRect(GetProcAddress(GdiPlusHandle, 'GdipDrawImageRect'));
    GdipDrawImageRectI := TGdipDrawImageRectI(GetProcAddress(GdiPlusHandle, 'GdipDrawImageRectI'));

    //---------- Matrix 矩阵 ----------
    GdipCreateMatrix := TGdipCreateMatrix(GetProcAddress(GdiPlusHandle, 'GdipCreateMatrix'));
    GdipCreateMatrix2 := TGdipCreateMatrix2(GetProcAddress(GdiPlusHandle, 'GdipCreateMatrix2'));
    GdipDeleteMatrix := TGdipDeleteMatrix(GetProcAddress(GdiPlusHandle, 'GdipDeleteMatrix'));
    GdipSetMatrixElements := TGdipSetMatrixElements(GetProcAddress(GdiPlusHandle, 'GdipSetMatrixElements'));
    GdipMultiplyMatrix := TGdipMultiplyMatrix(GetProcAddress(GdiPlusHandle, 'GdipMultiplyMatrix'));

    //---------- Font 字体（非核心，允许缺失）----------
    GdipCreateFontFromLogfontW := TGdipCreateFontFromLogfontW(GetProcAddress(GdiPlusHandle, 'GdipCreateFontFromLogfontW'));
    GdipDeleteFont := TGdipDeleteFont(GetProcAddress(GdiPlusHandle, 'GdipDeleteFont'));

    //---------- StringFormat 字符串格式（非核心）----------
    GdipCreateStringFormat := TGdipCreateStringFormat(GetProcAddress(GdiPlusHandle, 'GdipCreateStringFormat'));
    GdipDeleteStringFormat := TGdipDeleteStringFormat(GetProcAddress(GdiPlusHandle, 'GdipDeleteStringFormat'));
    GdipSetStringFormatAlign := TGdipSetStringFormatAlign(GetProcAddress(GdiPlusHandle, 'GdipSetStringFormatAlign'));
    GdipSetStringFormatLineAlign := TGdipSetStringFormatLineAlign(GetProcAddress(GdiPlusHandle, 'GdipSetStringFormatLineAlign'));

    //---------- Text 文字绘制（非核心）----------
    GdipDrawString := TGdipDrawString(GetProcAddress(GdiPlusHandle, 'GdipDrawString'));
    GdipMeasureString := TGdipMeasureString(GetProcAddress(GdiPlusHandle, 'GdipMeasureString'));
    GdipSetTextRenderingHint := TGdipSetTextRenderingHint(GetProcAddress(GdiPlusHandle, 'GdipSetTextRenderingHint'));

    // ── 第三步：关键函数指针完整性检查 ──
    // 任何一个核心函数为 nil 说明 DLL 版本不匹配或损坏，
    // 此时禁用 GDI+ 整体功能，防止后续调用引发 Access Violation。
    // GdipGetPathPointCount / GdipCreateFontFromLogfontW / GdipDrawString
    // 等非核心函数允许缺失（注释中已标记），消费方自行回退。
    if not Assigned(GdiplusStartup) or not Assigned(GdiplusShutdown) or
       not Assigned(GdipCreateFromHDC) or not Assigned(GdipDeleteGraphics) or
       not Assigned(GdipCreatePen1) or not Assigned(GdipDeletePen) or
       not Assigned(GdipCreateSolidFill) or not Assigned(GdipDeleteBrush) or
       not Assigned(GdipCreatePath) or not Assigned(GdipDeletePath) or
       not Assigned(GdipDrawLineI) or not Assigned(GdipFillPath) or
       not Assigned(GdipDrawPath) or not Assigned(GdipCreateMatrix) or
       not Assigned(GdipDeleteMatrix) then
    begin
      // 置零句柄，使 CnStartUpGdiPlus 直接退出，CnGdiPlusAvailable 保持 False
      {$IFNDEF SUPPORT_GDIPLUS}
      FreeLibrary(GdiPlusHandle);
      {$ENDIF}
      GdiPlusHandle := 0;
    end
    else
    begin
      // ── 第四步：EXE 项目自动初始化 GDI+ 令牌 ──
      if not IsLibrary then
        CnStartUpGdiPlus;
    end;
  end;

finalization
  if CnGdiPlusAvailable then
    CnShutDownGdiPlus;

  {$IFNDEF SUPPORT_GDIPLUS}
  // 仅动态路径需要释放 DLL；静态路径由 Delphi 运行时管理
  if GdiPlusHandle <> 0 then
  begin
    FreeLibrary(GdiPlusHandle);
    GdiPlusHandle := 0;
  end;
  {$ENDIF}

{$ENDIF}

end.
