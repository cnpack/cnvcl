{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnOpenGLPaintBox;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：CnOpenGLPaintBox控件单元
* 单元作者：王乾元  wqyfavor@163.com  QQ:466798985
* 备   注：这是可以利用OpenGL硬件加速的画布控件，使用最基础的OpenGL支持（Delphi
*        自带的单元）。有如下特性：
          1. 硬件加速，绘图速度远远胜过GDI与GDIP。
          2. 支持抗锯齿，输出图像质量较高。
          3. 支持类似GDIP的坐标变换，可以完成较复杂的绘图操作。
          4. 绘图函数丰富，可以绘制直线、折线、曲线、多边形、矩形、三角形，
            填充多边形、矩形、三角形、曲线封闭区域，并可输出文字、位图。
          5. 每一个绘图方法都返回画布自身指针，支持链式操作。
          6. 支持坐标变换后，Windows自身画布坐标到实际坐标的反计算，可以
            方便实现鼠标拾取等功能。
          7. 支持OpenGL的绘制列表，可以设置Canvas为列表状态，此时所有绘制
            过程都会记录到一个绘制列表里。可以多次调用该列表以提高绘图速度。
            类似GDIP的Graphic path
        尚不支持的特性与问题：
          1. 由于OpenGL自身只支持凸多边形，所以使用本画布绘制非凸多边形时会
            有一些显示上的问题。
          2. 输出ASCII文字速度很快，列表被缓存，但输出汉字等文字速度较慢。
          3. 文字还无法实现响应坐标变换与抗锯齿。
          4. 绘制位图功能还不完善，一些格式的位图可能无法正常绘制。
          5. 不支持虚拟机，可能出错并且性能不能令人满意。

        编写这个画布的想法来自看了GLScane自带的GLCanvas，但是GLScane自带的画
        布功能太弱，主要缺点是无法实现坐标变换（需要用户自己指定变换矩阵）,
        于是在它的基础上进行了扩充，保留了绘制直线、矩形、多边形的功能。并且
        参考ReactOS操作系统的GDIP与GDI源码编写了绘制曲线、弧的函数。
*
* 开发平台：Win7 + Delphi 2009
* 兼容测试：Win9X/2000/XP + Delphi 7.0
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：
* 修改记录：2009.12.20 V1.0 创建
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF DELPHI9_UP}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

{If in your application there are more than one GLCanvas, define this.
 If MultiCanvases is defined, before every process GLCanvas will check
 if self is activated. }
{$DEFINE MultiCanvases}

uses
  Windows, Classes, Controls, Graphics, OpenGL, Math, Messages;

const
  TransformStackTop = 10;
  ListNestLevel = 9;
  MaxChar = 128;

type
  ARGB = type Cardinal;
  // Use TARGB(ARGB) to quick access elements of an ARGB color
  TARGB = packed record
    blue, green, red, alpha: Byte;
  end;

  PSingleArray = ^TSingleArray;
  TSingleArray = array of Single;
  TColorVector = array[0..3] of Single;
  TVector4f = array[0..3] of Single;
  PMatrix4f = ^TMatrix4f;
  TMatrix4f = array[0..3] of TVector4f;

  PGLPointF = ^TGLPointF;
  TGLPointF = packed record
    X: Single;
    Y: Single;
  end;

  PGLPointsF = ^TGLPointsF;
  TGLPointsF = array of TGLPointF;

  PGLPointI = ^TGLPointI;
  TGLPointI = TPoint;

  PGLPointsI = ^TGLPointsI;
  TGLPointsI = array of TGLPointI;

  TTransformType = (ttScale, ttTranslate, ttRotate);
  TTransformData = record
    TransformType: TTransformType;
    var1, var2: Single;
  end;

  TLineStippleStyle = (lssSolid, lssDash, lssDashDot, lssDashDotDot, lssDot);

  TCnOpenGLPaintBox = class;
  TAfterRendering = procedure(Sender: TCnOpenGLPaintBox) of object;
  TGLFontNotify = procedure of object;

  TCnGLFont = class
  private
    FHFont: HFONT;
    FName: WideString;
    FSize: Integer;
    FStyles: TFontStyles;
    FCharSet: Integer;
    FColor: ARGB;
    FColorVector: TColorVector;
    FNotifyChange: TGLFontNotify;

    function FGetWinColor: TColor;
    procedure FSetWinColor(value: TColor);
    procedure FSetColor(value: ARGB);
  public
    constructor Create(Name: WideString; Size: Integer; Styles: TFontStyles = [];
      CharSet: Integer = DEFAULT_CHARSET; Notify: TGLFontNotify = nil);
    destructor Destroy; override;
    procedure Update; // call Update after modifying Name, Size ...

    property HFont: HFONT read FHFont write FHFont;
    property Name: WideString read FName write FName;
    property Size: Integer read FSize write FSize;
    property Styles: TFontStyles read FStyles write FStyles;
    property CharSet: Integer read FCharSet write FCharSet;
    property Color: ARGB read FColor write FSetColor; // do not need to call Update
    property WinColor: TColor read FGetWinColor write FSetWinColor; // do not need to call Update
    property NotifyChange: TGLFontNotify read FNotifyChange write FNotifyChange;
  end;

  TCnOpenGLPaintBox = class(TCustomControl)
  private
    FHRC: HGLRC;
    FAfterRendering: TAfterRendering;

    FRenderToBmp: Boolean;
    FBufferHDC: HDC;
    FBufferBitmap: HBITMAP;
    FBufferObject: HGDIOBJ;
    FBufferWidth, FBufferHeight: Integer;

    FBackgroundColor: TColor;
    FBackgroundColorGL: TColorVector;

    FInitialized: Boolean;
    FInvertY: Boolean;
    FRendering: Boolean;
    FBlend: Boolean;
    FAntialiasing: Boolean;
    FIgnorePenWidthFactor: Boolean;

    FListLevel: Integer;
    FIgnoreColor: Boolean;
    FIgnoreColorStack: array[1..ListNestLevel] of Boolean;

    FPenWidthFactor: Single; // Actually the total scale
    FPenWidth: Single;
    FPenColorARGB, FBrushColorARGB: ARGB;
    FPenColor, FBrushColor: TColorVector;

    FMatrix: TMatrix4f; // For backup
    FTransformationUpdateCount: Integer;
    FUseTransformStack: Boolean;
    FTransformStack: array[0..TransformStackTop] of TTransformData;
    FStackTop: Integer;

    FScaleX, FScaleY: Single;
    FTranslateX, FTranslateY: Single;
    FRotation: Single;

    FDefaultFont: TCnGLFont;
    FASCIICharList: GLuint;
    FASCIICharListCreated: Boolean;

{$IFNDEF BDS2006_UP}
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
{$ENDIF}

    procedure InitOpenGL;
    procedure ActivateSelf;
    procedure CreateBufferBMP;
    procedure FreeBufferBMP;
    procedure PresentBufferBMP(DC: HDC); {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

    procedure ApplyTransformation;
    procedure DefaultFontNotify;

    procedure FSetBackgroundColor(value: TColor);
    procedure FSetAntialiasing(value: Boolean);
    procedure FSetScaleX(value: Single);
    procedure FSetScaleY(value: Single);
    procedure FSetTranslateX(value: Single);
    procedure FSetTranslateY(value: Single);
    procedure FSetRotation(value: Single);

    procedure FSetPenColor(Value: ARGB);
    procedure FSetBrushColor(Value: ARGB);
    procedure FSetPenWidth(Value: Single);

    procedure EllipseVertices(const x, y, xRadius, yRadius: Single);
  protected
    procedure Paint; override;
{$IFNDEF BDS2006_UP}
    procedure DoMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure DoMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(InvertY: Boolean = True; RenderToBMP: Boolean = False);

    function RenderingBegin: TCnOpenGLPaintBox;
    function RenderingEnd: TCnOpenGLPaintBox;

    { You can either build draw lists in Rendering process or not.
      An example:
        GLCanvas.CreateList(List1, 1).ListBegin(List1).Line(0, 0, 50, 50).ListEnd;
        GLCanvas.RenderingBegin.ListExecute(List1).SetTranslateX(50).ListExecute(box).RenderingEnd;
      If IgnoreColor is true, all color definition in fill-shape processes are
      ignored. When the list is built, you can specify color for all vertices of
      the list.  }
    function CreateList(var ListID: GLuint; Range: GLuint = 1): TCnOpenGLPaintBox;
    function DeleteList(ListID: GLuint; Range: GLuint = 1): TCnOpenGLPaintBox;
    function ListBegin(ListID: GLuint; Offset: GLuint = 0; Execute: Boolean = False;
      IgnoreColor: Boolean = True): TCnOpenGLPaintBox;
    function ListEnd: TCnOpenGLPaintBox;
    function ListExecute(ListID: GLuint; Offset: GLuint = 0): TCnOpenGLPaintBox; overload;
    function ListExecute(ListID: GLuint; Offset: GLuint; Color: ARGB;
      PenWidth: Single): TCnOpenGLPaintBox; overload; // PenWidth <= 0 for no change

    { Call Recreate if you want to manually recreate opengl. Normally if Control
      is resized, GLCanvas will detect this change at the beginning of next
      rendering process. }
    procedure Recreate;
    procedure DrawTo(DC: HDC);
    procedure StretchDrawTo(DC: HDC; X, Y, W, H: Integer);

    ///////////////////////////////////////////////
    function BeginUpdateTransformation: TCnOpenGLPaintBox;
    function EndUpdateTransformation: TCnOpenGLPaintBox;

    // Use transformation for rendering. Not effective for TextOut.
    function SetTransformation(sx, sy, tx, ty, r: Single): TCnOpenGLPaintBox; // Scale, Translate, Rotation
    function SetMatrix(const m11, m12, m13, m21, m22, m23, m31, m32, m33,
      dx, dy: Single; Backup: Boolean = True): TCnOpenGLPaintBox;
    function ResetBackupMatrix: TCnOpenGLPaintBox;

    function ResetTransformation: TCnOpenGLPaintBox;
    function PopMatrix: TCnOpenGLPaintBox; // Cancel last transformation
    function SetEqualScale(value: Single): TCnOpenGLPaintBox;
    function ScaleMatrix(x, y: Single): TCnOpenGLPaintBox;
    function TranslateMatrix(x, y: Single): TCnOpenGLPaintBox;
    function RotateMatrix(angle: Single): TCnOpenGLPaintBox;
    function SetScaleX(value: Single): TCnOpenGLPaintBox;
    function SetScaleY(value: Single): TCnOpenGLPaintBox;
    function SetTranslateX(value: Single): TCnOpenGLPaintBox;
    function SetTranslateY(value: Single): TCnOpenGLPaintBox;
    function SetRotation(value: Single): TCnOpenGLPaintBox;

    function ConvertScreenToWorld(x, y: Integer): TGLPointF; // GDI coordinate where (0, 0) is Left-Top
    function UpdateTransformation: TCnOpenGLPaintBox; // You can maually force update transformation

    ///////////////////////////////////////////////
    function SetBlendState(value: Boolean): TCnOpenGLPaintBox; // Use this to enable or disable GL_BLEND, GL_BLEND is enabled by default.

    function SetPenColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TCnOpenGLPaintBox;
    function SetBrushColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TCnOpenGLPaintBox;
    function SetPenColor(value: ARGB): TCnOpenGLPaintBox; overload; // for linked process
    function SetBrushColor(value: ARGB): TCnOpenGLPaintBox; overload; // for linked process
    // For quick OpenGL color assignment. Sync = True, update FPenColorARGB or FBrushColorARGB
    function SetPenColor(const value: TColorVector; Sync: Boolean = False): TCnOpenGLPaintBox; overload;
    function SetBrushColor(const value: TColorVector; Sync: Boolean = False): TCnOpenGLPaintBox; overload;
    function SetPenWidth(value: Single): TCnOpenGLPaintBox; // for linked process

    function LineStipple(factor: Integer; pattern: word): TCnOpenGLPaintBox; overload;
    function LineStipple(style: TLineStippleStyle; enlarge: Byte = 2): TCnOpenGLPaintBox; overload;
    function LineStippleEnd: TCnOpenGLPaintBox;

    function Line(const x1, y1, x2, y2: Integer): TCnOpenGLPaintBox; overload;
    function Line(const x1, y1, x2, y2: Single): TCnOpenGLPaintBox; overload;
    function BeginLines: TCnOpenGLPaintBox; // Remember to call EndLines
    function Lines(const x1, y1, x2, y2: Integer): TCnOpenGLPaintBox; overload;
    function Lines(const x1, y1, x2, y2: Single): TCnOpenGLPaintBox; overload;
    function EndLines: TCnOpenGLPaintBox;
    function Lines(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox; overload;
    function Lines(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox; overload;

    function Polyline(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox; overload;
    function Polyline(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox; overload;
    function Polygon(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox; overload;
    function Polygon(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox; overload;
    function FillPolygon(const points: TGLPointsF; count: Integer; Border: Boolean = False): TCnOpenGLPaintBox; overload;
    function FillPolygon(const points: TGLPointsI; count: Integer; Border: Boolean = False): TCnOpenGLPaintBox; overload;

    function Curve(const points: TGLPointsF; count: Integer; tension: Single = 0.5): TCnOpenGLPaintBox; overload;
    function Curve(const points: TGLPointsI; count: Integer; tension: Single = 0.5): TCnOpenGLPaintBox; overload;
    function ClosedCurve(const points: TGLPointsF; count: Integer; tension: Single = 0.5): TCnOpenGLPaintBox; overload;
    function ClosedCurve(const points: TGLPointsI; count: Integer; tension: Single = 0.5): TCnOpenGLPaintBox; overload;
    function FillClosedCurve(const points: TGLPointsF; count: Integer;
      Border: Boolean = False; tension: Single = 0.5): TCnOpenGLPaintBox; overload;
    function FillClosedCurve(const points: TGLPointsI; count: Integer;
      Border: Boolean = False; tension: Single = 0.5): TCnOpenGLPaintBox; overload;

    function Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Integer): TCnOpenGLPaintBox; overload;
    function Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Single): TCnOpenGLPaintBox; overload;
    function PolyBezier(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox; overload;
    function PolyBezier(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox; overload;

    // x, y, xRadius, yRadius specify an ellipse. startAngle and sweepAngle specify the range of curve.
    function Arc(const x, y, xRadius, yRadius: Single; startAngle, sweepAngle: Single): TCnOpenGLPaintBox;
    function FillPie(const x, y, xRadius, yRadius: Single;
      startAngle, sweepAngle: Single; Border: Boolean = False): TCnOpenGLPaintBox;

    // Plots a pixel at given coordinate
    function PlotPixel(const x, y: Integer): TCnOpenGLPaintBox; overload;
    function PlotPixel(const x, y: Single): TCnOpenGLPaintBox; overload;
    function BeginPixels: TCnOpenGLPaintBox; // Remember to call EndLines
    function Pixels(const x, y: Integer): TCnOpenGLPaintBox; overload;
    function Pixels(const x, y: Single): TCnOpenGLPaintBox; overload;
    function EndPixels: TCnOpenGLPaintBox;

    // Draw the (x1,y1)-(x2, y2) rectangle's frame (border). }
    function FrameRect(const x1, y1, x2, y2: Integer): TCnOpenGLPaintBox; overload;
    function FrameRect(const x1, y1, x2, y2: Single): TCnOpenGLPaintBox; overload;
    // Draw the (x1,y1)-(x2, y2) rectangle (filled with BrushColor)
    function FillRect(const x1, y1, x2, y2: Integer; Border: Boolean = False): TCnOpenGLPaintBox; overload;
    function FillRect(const x1, y1, x2, y2: Single; Border: Boolean = False): TCnOpenGLPaintBox; overload;

    function Triangle(const x1, y1, x2, y2, x3, y3: Integer): TCnOpenGLPaintBox; overload;
    function Triangle(const x1, y1, x2, y2, x3, y3: Single): TCnOpenGLPaintBox; overload;
    function FillTriangle(const x1, y1, x2, y2, x3, y3: Integer; Border: Boolean = False): TCnOpenGLPaintBox; overload;
    function FillTriangle(const x1, y1, x2, y2, x3, y3: Single; Border: Boolean = False): TCnOpenGLPaintBox; overload;

    // Draws an ellipse with (x1,y1)-(x2, y2) bounding rectangle.
    function EllipseRect(const x1, y1, x2, y2 : Single): TCnOpenGLPaintBox; overload;
    // Draws and ellipse centered at (x, y) with given radiuses.
    function Ellipse(const x, y, xRadius, yRadius: Single): TCnOpenGLPaintBox; overload;
    function FillEllipseRect(const x1, y1, x2, y2: Single; Border: Boolean = False): TCnOpenGLPaintBox; overload;
    function FillEllipse(const x, y, xRadius, yRadius: Single; Border: Boolean = False): TCnOpenGLPaintBox; overload;

    procedure RecreateDefaultFont;
    // Output only ASCII chars, other chars will be ignored automatically
    function TextOutASCII(const text: string; x, y: Integer; Font: TCnGLFont = nil): TCnOpenGLPaintBox; // Use this for ASCII chars for efficiency
    // Output any string but slow
    function TextOut(const text: WideString; x, y: Integer; Font: TCnGLFont = nil): TCnOpenGLPaintBox;

    function BuildTexture(bmp: TBitmap; var texId: GLuint): TCnOpenGLPaintBox;
    function DeleteTexture(texId: GLuint): TCnOpenGLPaintBox;

    //The difference between DrawBitmap and DrawBitmapTex is that DrawBitmapTex supports transformation}
    function DrawBitmap(bmp: TBitmap; x, y: Integer; xZoom: Single = 1.0; yZoom: Single = 1.0): TCnOpenGLPaintBox; overload;
    function DrawBitmapTex(bmp: TBitmap; x, y, w, h: Integer): TCnOpenGLPaintBox; overload;
    function DrawBitmapTex(texId: GLuint; x, y, w, h: Integer): TCnOpenGLPaintBox; overload;

    // Turn off UseTransformStack to use these parameters
    property ScaleX: Single read FScaleX write FSetScaleX;
    property ScaleY: Single read FScaleY write FSetScaleY;
    property TranslateX: Single read FTranslateX write FSetTranslateX;
    property TranslateY: Single read FTranslateY write FSetTranslateY;
    property Rotation: Single read FRotation write FSetRotation;

    property BufferHDC: HDC read FBufferHDC write FBufferHDC;
    property Rendering: Boolean read FRendering;
    property PenColor: ARGB read FPenColorARGB write FSetPenColor;
    property BrushColor: ARGB read FBrushColorARGB write FSetBrushColor;
    property PenWidth: Single read FPenWidth write FSetPenWidth;
    property DefaultFont: TCnGLFont read FDefaultFont;
    property InvertY: Boolean read FInvertY;
    property RenderToBMP: Boolean read FRenderToBmp;

  published
    property BackgroundColor: TColor read FBackgroundColor write FSetBackgroundColor;
    property UseTransformStack: Boolean read FUseTransformStack write FUseTransformStack default False;
    property Antialiasing: Boolean read FAntialiasing write FSetAntialiasing default True;
    property AfterRendering: TAfterRendering read FAfterRendering write FAfterRendering;

    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
{$IFDEF BDS2006_UP}
    property OnMouseEnter;
    property OnMouseLeave;
{$ELSE}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnResize;
  end;

const
  IdentityHmgMatrix: TMatrix4f = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

  AlphaShift = 24;
  RedShift = 16;
  GreenShift = 8;
  BlueShift = 0;

function MakeColor(r, g, b: Byte): ARGB; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function MakeColor(a, r, g, b: Byte): ARGB; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetAlpha(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetRed(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetGreen(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetBlue(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

// TColor = COLORREF
function TColorToARGB(rgb: TColor; alpha: Byte = 255): ARGB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function ARGBToTColor(Color: ARGB): TColor; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function GLColorToARGB(const glcolor: TColorVector): ARGB;  {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function ARGBToGLColor(color: ARGB): TColorVector; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function GLColorToTColor(const glcolor: TColorVector): TColor; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function TColorToGLColor(color: TColor; alpha: Byte = 255): TColorVector; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function ModifyAlphaValue(col: ARGB; alpha: Byte): ARGB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

{$IFNDEF COMPILER7_UP}
function IsZero(const A: Single; Epsilon: Single = 0): Boolean;
function SameValue(const A, B: Single; Epsilon: Single = 0): Boolean;
{$ENDIF}

implementation

var
  GLOBAL_ANTIALIASING_STATE: Boolean;

const
  // Almost all video cards now support these extensions. Even not, no error will be raised.
  GL_TEXTURE_3D = $806F;
  GL_TEXTURE_CUBE_MAP_ARB = $8513;
  GL_BGR = $80E0;

  cNoPrimitive = MaxInt;

  PiDiv180 = Pi / 180;
  _2Pi = Pi * 2;
  PiDiv2 = Pi / 2;

  opengl32 = 'OpenGL32.dll';

procedure glGenTextures(n: GLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
procedure glBindTexture(target: GLEnum; texture: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

{$IFNDEF COMPILER7_UP}

const
  FuzzFactor = 1000;
  ExtendedResolution = 1E-19 * FuzzFactor;
  DoubleResolution   = 1E-15 * FuzzFactor;
  SingleResolution   = 1E-7 * FuzzFactor;

function IsZero(const A: Single; Epsilon: Single): Boolean;
begin
  if Epsilon = 0 then
    Epsilon := SingleResolution;
  Result := Abs(A) <= Epsilon;
end;

function SameValue(const A, B: Single; Epsilon: Single): Boolean; 
begin
  if Epsilon = 0 then
    Epsilon := Max(Min(Abs(A), Abs(B)) * SingleResolution, SingleResolution);
  if A > B then
    Result := (A - B) <= Epsilon
  else
    Result := (B - A) <= Epsilon;
end;

{$ENDIF}

procedure SinCos(const Theta: Single; var Sin, Cos: Single);
asm
  FLD  Theta
  FSINCOS
  FSTP DWORD PTR [EDX]   // cosine
  FSTP DWORD PTR [EAX]   // sine
end;

function MakeColor(r, g, b: Byte): ARGB; overload;
begin
  with TARGB(Result) do
  begin
    alpha := 255;
    red := r;
    green := g;
    blue := b;
  end;
end;

function MakeColor(a, r, g, b: Byte): ARGB; overload;
begin
  with TARGB(Result) do
  begin
    alpha := a;
    red := r;
    green := g;
    blue := b;
  end;
end;

function GetAlpha(color: ARGB): BYTE;
begin
  Result := BYTE(color shr AlphaShift);
end;

function GetRed(color: ARGB): BYTE;
begin
  Result := BYTE(color shr RedShift);
end;

function GetGreen(color: ARGB): BYTE;
begin
  Result := BYTE(color shr GreenShift);
end;

function GetBlue(color: ARGB): BYTE;
begin
  Result := BYTE(color shr BlueShift);
end;

function TColorToARGB(rgb: TColor; alpha: Byte = 255): ARGB;
begin
  Result := MakeColor(alpha, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
end;

function ARGBToTColor(Color: ARGB): TColor;
begin
  with TARGB(Color) do
    Result := RGB(red, green, blue);
end;

function GLColorToARGB(const glcolor: TColorVector): ARGB;
begin
  Result := MakeColor(Trunc(255 * glcolor[3]), Trunc(255 * glcolor[0]),
    Trunc(255 * glcolor[1]), Trunc(255 * glcolor[2]));
end;

function ARGBToGLColor(color: ARGB): TColorVector;
begin
  with TARGB(color) do
  begin
    Result[0] := red / 255;
    Result[1] := green / 255;
    Result[2] := blue / 255;
    Result[3] := alpha / 255;
  end;
end;

function GLColorToTColor(const glcolor: TColorVector): TColor;
begin
  Result := RGB(Trunc(255 * glcolor[0]), Trunc(255 * glcolor[1]), Trunc(255 * glcolor[2]));
end;

function TColorToGLColor(color: TColor; alpha: Byte = 255): TColorVector;
begin
  Result[0] := GetRValue(color) / 255;
  Result[1] := GetGValue(color) / 255;
  Result[2] := GetBValue(color) / 255;
  Result[3] := alpha / 255;
end;

function ModifyAlphaValue(col: ARGB; alpha: Byte): ARGB;
begin
   Result := col;
   TARGB(Result).alpha := alpha;
end;

{ TCnGLFont }

constructor TCnGLFont.Create(Name: WideString; Size: Integer; Styles: TFontStyles = [];
  CharSet: Integer = DEFAULT_CHARSET; Notify: TGLFontNotify = nil);
begin
  FHFont := 0;
  FName := Name;
  FSize := Size;
  FStyles := Styles;
  FCharSet := CharSet;
  FColor := TColorToARGB(clBlack);
  FNotifyChange := Notify;
  Update;
end;

destructor TCnGLFont.Destroy;
begin
  DeleteObject(FHFont);
  inherited;
end;

function TCnGLFont.FGetWinColor: TColor;
begin
  Result := ARGBToTColor(FColor);
end;

procedure TCnGLFont.FSetWinColor(value: TColor);
begin
  Color := TColorToARGB(value);
end;

procedure TCnGLFont.FSetColor(value: ARGB);
begin
  FColor := value;
  FColorVector := ARGBToGLColor(FColor);
end;

procedure TCnGLFont.Update;
var
  bold, italic, underline, strikeout: Integer;
begin
  if FHFont <> 0 then
    DeleteObject(FHFont);

  if fsBold in FStyles then
    bold := FW_BOLD
  else
    bold := FW_NORMAL;

  if fsItalic in FStyles then
    italic := 1
  else
    italic := 0;

  if fsUnderline in FStyles then
    underline := 1
  else
    underline := 0;

  if fsStrikeOut in FStyles then
    strikeout := 1
  else
    strikeout := 0;

  FHFont := CreateFontW(FSize, 0, 0, 0, bold, italic, underline, strikeout,
      FCharSet, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
      DEFAULT_QUALITY, DEFAULT_PITCH or FF_SWISS, PWideChar(FName));

  FColorVector := ARGBToGLColor(FColor);

  if Assigned(FNotifyChange) then
    FNotifyChange;
end;

{ TCnGLCanvas }

constructor TCnOpenGLPaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInitialized := False;
  FRenderToBmp := False;
  FUseTransformStack := False;
  FInvertY := True;
  FRendering := False;
  FAntialiasing := True;
  FIgnorePenWidthFactor := False;
  FListLevel := 0;
  FIgnoreColor := False;
  FPenWidth := 1.0;
  FStackTop := -1;
  FTranslateX := 0.0;
  FTranslateY := 0.0;
  FScaleX := 1.0;
  FScaleY := 1.0;
  FRotation := 0.0;
  FTransformationUpdateCount := 0;
  SetPenColorWin(clBlack);
  SetBrushColorWin(clRed);
  BackgroundColor := clWhite;

  FASCIICharListCreated := False;
  RecreateDefaultFont;

  Height := 105;
  Width := 105;
end;

destructor TCnOpenGLPaintBox.Destroy;
begin
  FreeBufferBMP;
  wglDeleteContext(FHRC);
  FDefaultFont.Free;
  inherited;
end;

{$IFNDEF BDS2006_UP}
procedure TCnOpenGLPaintBox.DoMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
   FOnMouseEnter(Self);
end;

procedure TCnOpenGLPaintBox.DoMouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then
   FOnMouseLeave(Self);
end;
{$ENDIF}

procedure TCnOpenGLPaintBox.Initialize(InvertY: Boolean = True; RenderToBMP: Boolean = False);
begin
  if FRendering then
    Exit;
  FInvertY := InvertY;
  FRenderToBMP := RenderToBMP;
  Recreate;
  FInitialized := True;
  // Paint default background

end;

procedure TCnOpenGLPaintBox.CreateBufferBMP;
var
  ReInitOpenGL: Boolean;
  bmi: BITMAPINFO;
  pbits: ^DWORD;
begin
  if FBufferWidth <> -1 then
    FreeBufferBMP;

  FBufferWidth := ClientWidth;
  FBufferHeight := ClientHeight;
  // Create a memory DC compatible with the screen
  FBufferHDC := CreateCompatibleDC(0);

  FillChar(bmi, SizeOf(bmi), 0);
  with bmi.bmiHeader do
  begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := FBufferWidth;
    biHeight := FBufferHeight;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;

  FBufferBitmap := CreateDIBSection(FBufferHDC, bmi, DIB_RGB_COLORS, Pointer(pbits), 0, 0);

  // Select the bitmap into the DC
  FBufferObject := SelectObject(FBufferHDC, FBufferBitmap);
end;

procedure TCnOpenGLPaintBox.FreeBufferBMP;
begin
  SelectObject(FBufferHDC, FBufferObject); // Remove bitmap from DC
  DeleteObject(FBufferBitmap); // Delete bitmap
  DeleteDC(FBufferHDC); // Delete DC
end;

procedure TCnOpenGLPaintBox.PresentBufferBMP(DC: HDC);
begin
  StretchBlt(DC, 0, 0, ClientWidth, ClientHeight, FBufferHDC, 0, 0, FBufferWidth,
    FBufferHeight, SRCCOPY);
end;

procedure TCnOpenGLPaintBox.FSetPenColor(Value: ARGB);
begin
  if FPenColorARGB <> Value then
  begin
    FPenColorARGB := Value;
    FPenColor := ARGBToGLColor(Value);
  end;
  glColor4fv(@FPenColor);
end;

procedure TCnOpenGLPaintBox.FSetBrushColor(Value: ARGB);
begin
  if FBrushColorARGB <> Value then
  begin
    FBrushColorARGB := Value;
    FBrushColor := ARGBToGLColor(Value);
  end;
end;

procedure TCnOpenGLPaintBox.FSetPenWidth(Value: Single);
begin
  FPenWidth := Value;
  glLineWidth(Value * FPenWidthFactor);
  glPointSize(Value * FPenWidthFactor);
end;

procedure TCnOpenGLPaintBox.Paint;
var
  DC: HDC;
begin
  if FRenderToBmp then
  begin
    DC := GetDC(Handle);
    if DC <> 0 then
      PresentBufferBMP(DC);
  end;
end;

procedure TCnOpenGLPaintBox.InitOpenGL;
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pixelFormat: Integer;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do
  begin
    nSize := SizeOf(TPIXELFORMATDESCRIPTOR); // 此结构尺寸
    nVersion := 1;
    if FRenderToBmp then
      dwFlags := PFD_SUPPORT_OPENGL or PFD_DRAW_TO_BITMAP
    else
      dwFlags := PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or PFD_DRAW_TO_WINDOW;
    iPixelType := PFD_TYPE_RGBA; //使用RGBA颜色空间
    cColorBits := 32;
    cDepthBits := 16;
    iLayerType := PFD_MAIN_PLANE;
  end;

  pixelFormat := ChoosePixelFormat(FBufferHDC, @pfd);
  SetPixelFormat(FBufferHDC, pixelFormat, @pfd);
  FHRC := wglCreateContext(FBufferHDC);
  wglMakeCurrent(FBufferHDC, FHRC);
  GdiFlush();

  glPushAttrib(GL_ENABLE_BIT);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glDisable(GL_FOG);
  glDisable(GL_COLOR_MATERIAL);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_TEXTURE_1D);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_TEXTURE_3D);
  glDisable(GL_TEXTURE_CUBE_MAP_ARB);

  FBlend := True;
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  Antialiasing := FAntialiasing; // Apply antialiasing mode

  glViewPort(0, 0, FBufferWidth, FBufferHeight); //指定OpenGL在此区域内绘图。
  glMatrixMode(GL_PROJECTION);
  if FInvertY then
    gluOrtho2D(0, FBufferWidth, 0, FBufferHeight)
  else
    gluOrtho2D(0, FBufferWidth, FBufferHeight, 0);

  glMatrixMode(GL_MODELVIEW);

  // Recreate ASCII char lists
  DefaultFontNotify;
end;

procedure TCnOpenGLPaintBox.ActivateSelf;
begin
  wglMakeCurrent(FBufferHDC, FHRC);
  if GLOBAL_ANTIALIASING_STATE <> FAntialiasing then
    FSetAntialiasing(FAntialiasing);
  if FBlend then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);
end;

procedure TCnOpenGLPaintBox.ApplyTransformation;
  function ComputeCompoundPenWidthFactor(sx, sy: Single): Single;
  begin
    Result := Sqrt((sx * sx + sy * sy) / 2);
  end;
var
  i: Integer;
  tx, ty: Single;
begin
  if not FRendering then
    Exit;
  if FUseTransformStack then
  begin
    glLoadIdentity;
    tx := 1.0;
    ty := 1.0;
    for i := 0 to FStackTop do
      with FTransformStack[i] do
      begin
        case TransformType of
          ttScale:
            begin
              glScale(var1, var2, 1.0);
              tx := tx * var1;
              ty := ty * var2;
            end;
          ttTranslate: glTranslate(var1, var2, 0.0);
          ttRotate: glRotatef(var1, 0.0, 0.0, 1.0);
        end;
      end;
    if FIgnorePenWidthFactor then
      FPenWidthFactor := 1
    else
      FPenWidthFactor := ComputeCompoundPenWidthFactor(tx, ty);
  end
  else
  begin
    glLoadIdentity;
    glTranslate(FTranslateX, FTranslateY, 0.0);
    glScale(FScaleX, FScaleY, 1.0);
    glRotatef(FRotation, 0.0, 0.0, 1.0);
    if FIgnorePenWidthFactor then
      FPenWidthFactor := 1
    else
      FPenWidthFactor := ComputeCompoundPenWidthFactor(FScaleX, FScaleY);
  end;
end;

procedure TCnOpenGLPaintBox.DefaultFontNotify;
var
  i: Integer;
begin
  for i := 0 to 1 do // very odd, must call twice
  begin
    if FASCIICharListCreated then
      glDeleteLists(FASCIICharList, MaxChar);
    SelectObject(FBufferHDC, FDefaultFont.HFont);
    FASCIICharList := glGenLists(MaxChar);
    wglUseFontBitmaps(FBufferHDC, 0, MaxChar, FASCIICharList);
    FASCIICharListCreated := True;
  end;
end;

procedure TCnOpenGLPaintBox.FSetBackgroundColor(value: TColor);
begin
  FBackgroundColor := value;
  FBackgroundColorGL := TColorToGLColor(BackgroundColor);
end;

procedure TCnOpenGLPaintBox.FSetAntialiasing(value: Boolean);
begin
  if value then
  begin
    glEnable(GL_SMOOTH);
    glEnable(GL_LINE_SMOOTH);
    glEnable(GL_POINT_SMOOTH);
    glEnable(GL_POLYGON_SMOOTH);
    glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  end
  else
  begin
    glDisable(GL_SMOOTH);
    glDisable(GL_LINE_SMOOTH);
    glDisable(GL_POINT_SMOOTH);
    glDisable(GL_POLYGON_SMOOTH);
    glHint(GL_POINT_SMOOTH_HINT, GL_FASTEST);
    glHint(GL_LINE_SMOOTH_HINT, GL_FASTEST);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_FASTEST);
  end;
  FAntialiasing := value;
  GLOBAL_ANTIALIASING_STATE := FAntialiasing;
end;

procedure TCnOpenGLPaintBox.FSetScaleX(value: Single);
begin
  FScaleX := value;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
end;

procedure TCnOpenGLPaintBox.FSetScaleY(value: Single);
begin
  FScaleY := value;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
end;

procedure TCnOpenGLPaintBox.FSetTranslateX(value: Single);
begin
  FTranslateX := value;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
end;

procedure TCnOpenGLPaintBox.FSetTranslateY(value: Single);
begin
  FTranslateY := value;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
end;

procedure TCnOpenGLPaintBox.FSetRotation(value: Single);
begin
  FRotation := value;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
end;

function TCnOpenGLPaintBox.RenderingBegin: TCnOpenGLPaintBox;
begin
  Result := Self;
  if FRendering then
    Exit;
  FRendering := True;
  if (FBufferWidth <> ClientWidth) or (FBufferHeight <> ClientHeight) then
    Recreate
  {$IFDEF MultiCanvases}
  else
    ActivateSelf{$ENDIF}; // get active

  glClearColor(FBackgroundColorGL[0], FBackgroundColorGL[1], FBackgroundColorGL[2], FBackgroundColorGL[3]);
  glClear(GL_COLOR_BUFFER_BIT);

  ApplyTransformation;

  // User must keep in mind that Matrix Mode mustn't be changed during rendering.
  glColor4fv(@FPenColor);
end;

function TCnOpenGLPaintBox.RenderingEnd;
begin
  if FRenderToBMP then
  begin
    glFinish;
    // User may draw other objects using Gdi, GdiP after OpenGL process.
    if Assigned(FAfterRendering) then
      FAfterRendering(Self);
    Repaint; // Other controls must repaint whenever they are repainted by Windows.
  end
  else
    SwapBuffers(FBufferHDC);

  FRendering := False;
  Result := Self;
end;

function TCnOpenGLPaintBox.CreateList(var ListID: GLuint; Range: GLuint = 1): TCnOpenGLPaintBox;
begin
  if Range >= 1 then
  begin
    {$IFDEF MultiCanvases}
    ActivateSelf;
    {$ENDIF}
    ListID := glGenLists(Range);
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.DeleteList(ListID: GLuint; Range: GLuint = 1): TCnOpenGLPaintBox;
begin
  glDeleteLists(ListID, Range);
  Result := Self;
end;

function TCnOpenGLPaintBox.ListBegin(ListID: GLuint; Offset: GLuint = 0;
  Execute: Boolean = False; IgnoreColor: Boolean = True): TCnOpenGLPaintBox;
begin
  Result := Self;
  if FListLevel = ListNestLevel then
    Exit;
    
  if Execute then
    glNewList(ListID + Offset, GL_COMPILE_AND_EXECUTE)
  else
    glNewList(ListID + Offset, GL_COMPILE);
  Inc(FListLevel);
  FIgnoreColorStack[FListLevel] := IgnoreColor;     
  FIgnoreColor := IgnoreColor;
end;

function TCnOpenGLPaintBox.ListEnd: TCnOpenGLPaintBox;
begin
  glEndList;
  Dec(FListLevel);
  if FListLevel > 0 then
    FIgnoreColor := FIgnoreColorStack[FListLevel]
  else  
    FIgnoreColor := False;   
  Result := Self;    
end;

function TCnOpenGLPaintBox.ListExecute(ListID: GLuint; Offset: GLuint = 0): TCnOpenGLPaintBox;
begin
  if FRendering then
    glCallList(ListID + Offset);
  Result := Self;
end;

function TCnOpenGLPaintBox.ListExecute(ListID: GLuint; Offset: GLuint; Color: ARGB;
  PenWidth: Single): TCnOpenGLPaintBox;
var
  ColorVec: TColorVector;
begin
  if FRendering then
  begin
    ColorVec := ARGBToGLColor(Color);
    glColor4fv(@ColorVec);
    if PenWidth > 0 then
    begin
      glLineWidth(PenWidth * FPenWidthFactor);
      glPointSize(PenWidth * FPenWidthFactor);
    end;
    glCallList(ListID + Offset);

    // Restore color and pen width
    glColor4fv(@FPenColor);
    if PenWidth > 0 then
    begin
      glLineWidth(FPenWidth * FPenWidthFactor);
      glPointSize(FPenWidth * FPenWidthFactor);
    end;
  end;
  Result := Self;
end;

procedure TCnOpenGLPaintBox.Recreate;
begin
  // Reinitialize opengl
  if FRenderToBmp then
    CreateBufferBMP
  else
  begin
    FBufferHDC := GetDC(Handle);
    FBufferWidth := ClientWidth;
    FBufferHeight := ClientHeight;
  end;
  wglDeleteContext(FHRC);
  InitOpenGL;
end;

procedure TCnOpenGLPaintBox.DrawTo(DC: HDC);
begin
  if FRenderToBmp then
    StretchBlt(DC, 0, 0, FBufferWidth, FBufferHeight,
      FBufferHDC, 0, 0, FBufferWidth, FBufferHeight, SRCCOPY);
end;

procedure TCnOpenGLPaintBox.StretchDrawTo(DC: HDC; X, Y, W, H: Integer);
begin
  if FRenderToBmp then
    StretchBlt(DC, X, Y, W, H, FBufferHDC, 0, 0, FBufferWidth, FBufferHeight, SRCCOPY);
end;

function TCnOpenGLPaintBox.BeginUpdateTransformation: TCnOpenGLPaintBox;
begin
  Inc(FTransformationUpdateCount);
  Result := Self;
end;

function TCnOpenGLPaintBox.EndUpdateTransformation: TCnOpenGLPaintBox;
begin
  Dec(FTransformationUpdateCount);
  if FTransformationUpdateCount <= 0 then
  begin
    UpdateTransformation;
    FTransformationUpdateCount := 0;
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetTransformation(sx, sy, tx, ty, r: Single): TCnOpenGLPaintBox;
begin
  FUseTransformStack := False;
  FScaleX := sx;
  FScaleY := sy;
  FTranslateX := tx;
  FTranslateY := ty;
  FRotation := r;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetMatrix(const m11, m12, m13, m21, m22, m23, m31, m32, m33,
  dx, dy: Single; Backup: Boolean = True): TCnOpenGLPaintBox;
var
  AMatrix: TMatrix4f;
begin
  Result := Self;
  if not Rendering then
    Exit;

  AMatrix := IdentityHmgMatrix;
  AMatrix[0][0] := m11;
  AMatrix[0][1] := m12;
  AMatrix[0][2] := m13;
  AMatrix[1][0] := m21;
  AMatrix[1][1] := m22;
  AMatrix[1][2] := m23;
  AMatrix[2][0] := m31;
  AMatrix[2][1] := m32;
  AMatrix[2][2] := m33;
  AMatrix[3][0] := dx;
  AMatrix[3][1] := dy;
  glLoadMatrixf(@AMatrix);

  if Backup then
    FMatrix := AMatrix;

  // Assume that X axis and Y axis are equally scaled.
  if FIgnorePenWidthFactor then
    FPenWidthFactor := 1
  else
    FPenWidthFactor := Sqrt((m11 * m11 + m22 * m22) / 2);
end;

function TCnOpenGLPaintBox.ResetBackupMatrix: TCnOpenGLPaintBox;
begin
  Result := Self;
  if not Rendering then
    Exit;

  glLoadMatrixf(@FMatrix);
  // Assume that X axis and Y axis are equally scaled.
  if FIgnorePenWidthFactor then
    FPenWidthFactor := 1
  else
    FPenWidthFactor := Sqrt((Sqr(FMatrix[0][0]) + Sqr(FMatrix[1][1])) / 2);
end;

function TCnOpenGLPaintBox.ResetTransformation: TCnOpenGLPaintBox;
begin
  FStackTop := -1;
  FScaleX := 1.0;
  FScaleY := 1.0;
  FTranslateX := 0.0;
  FTranslateY := 0.0;
  FRotation := 0.0;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
  Result := Self;
end;

function TCnOpenGLPaintBox.PopMatrix: TCnOpenGLPaintBox;
begin
  if FStackTop >= 0 then
    Dec(FStackTop);
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetEqualScale(value: Single): TCnOpenGLPaintBox;
begin
  if not FUseTransformStack then
  begin
    FScaleX := value;
    FScaleY := value;
  end;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
  Result := Self;
end;

function TCnOpenGLPaintBox.ScaleMatrix(x, y: Single): TCnOpenGLPaintBox;
begin
  if FStackTop < TransformStackTop then
  begin
    Inc(FStackTop);
    with FTransformStack[FStackTop] do
    begin
      TransformType := ttScale;
      var1 := x;
      var2 := y;
    end;
  end;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
  Result := Self;
end;

function TCnOpenGLPaintBox.TranslateMatrix(x, y: Single): TCnOpenGLPaintBox;
begin
  if FStackTop < TransformStackTop then
  begin
    Inc(FStackTop);
    with FTransformStack[FStackTop] do
    begin
      TransformType := ttTranslate;
      var1 := x;
      var2 := y;
    end;
  end;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
  Result := Self;
end;

function TCnOpenGLPaintBox.RotateMatrix(angle: Single): TCnOpenGLPaintBox;
begin
  if FStackTop < TransformStackTop then
  begin
    Inc(FStackTop);
    with FTransformStack[FStackTop] do
    begin
      TransformType := ttRotate;
      var1 := angle;
    end;
  end;
  if FTransformationUpdateCount = 0 then
    ApplyTransformation;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetScaleX(value: Single): TCnOpenGLPaintBox;
begin
  ScaleX := value;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetScaleY(value: Single): TCnOpenGLPaintBox;
begin
  ScaleY := value;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetTranslateX(value: Single): TCnOpenGLPaintBox;
begin
  TranslateX := value;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetTranslateY(value: Single): TCnOpenGLPaintBox;
begin
  TranslateY := value;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetRotation(value: Single): TCnOpenGLPaintBox;
begin
  Rotation := value;
  Result := Self;
end;

function TCnOpenGLPaintBox.ConvertScreenToWorld(x, y: Integer): TGLPointF;
var
  Viewport: array[0..3] of GLuint;
  ModelMatrix: array[0..15] of GLdouble;
  ProjMatrix: array[0..15] of GLdouble;
  ox, oy, oz: GLdouble;
begin
  glGetIntegerv(GL_VIEWPORT, @Viewport[0]);
  glGetDoublev(GL_MODELVIEW_MATRIX, @ModelMatrix[0]);
  glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix[0]);
  gluUnProject(x, Viewport[3] - y, 0, @ModelMatrix[0],
    @ProjMatrix[0], @Viewport[0], ox, oy, oz);
  Result.X := ox;
  Result.Y := oy;
end;

function TCnOpenGLPaintBox.UpdateTransformation: TCnOpenGLPaintBox;
begin
  ApplyTransformation;
  PenWidth := FPenWidth;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetBlendState(value: Boolean): TCnOpenGLPaintBox;
begin
  if value then
    glEnable(GL_BLEND)
  else
    glDisable(GL_BLEND);
  FBlend := value;
  Result := Self;
end;

function TCnOpenGLPaintBox.SetPenColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TCnOpenGLPaintBox;
begin
  if Sync then
    FPenColorARGB := TColorToARGB(value);
  FPenColor := TColorToGLColor(value, alpha);
  glColor4fv(@FPenColor);
  Result := Self;
end;

function TCnOpenGLPaintBox.SetBrushColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TCnOpenGLPaintBox;
begin
  if Sync then
    FBrushColorARGB := TColorToARGB(value);
  FBrushColor := TColorToGLColor(value, alpha);
  Result := Self;
end;

function TCnOpenGLPaintBox.SetPenColor(value: ARGB): TCnOpenGLPaintBox;
begin
  FSetPenColor(value);
  Result := Self;
end;

function TCnOpenGLPaintBox.SetBrushColor(value: ARGB): TCnOpenGLPaintBox;
begin
  FSetBrushColor(value);
  Result := Self;
end;

function TCnOpenGLPaintBox.SetPenColor(const value: TColorVector; Sync: Boolean = False): TCnOpenGLPaintBox;
begin
  glColor4fv(@value);
  FPenColor := value;
  if Sync then
    FPenColorARGB := GLColorToARGB(value);
  Result := Self;
end;

function TCnOpenGLPaintBox.SetBrushColor(const value: TColorVector; Sync: Boolean = False): TCnOpenGLPaintBox;
begin
  FBrushColor := value;
  if Sync then
    FBrushColorARGB := GLColorToARGB(value);
  Result := Self;
end;

function TCnOpenGLPaintBox.SetPenWidth(value: Single): TCnOpenGLPaintBox;
begin
  FSetPenWidth(value);
  Result := Self;
end;

function TCnOpenGLPaintBox.LineStipple(factor: Integer; pattern: word): TCnOpenGLPaintBox;
begin
  glEnable(GL_LINE_STIPPLE);
  glLineStipple(factor, pattern);
  Result := Self;
end;

function TCnOpenGLPaintBox.LineStipple(style: TLineStippleStyle; enlarge: Byte = 2): TCnOpenGLPaintBox;
begin
  case style of
    lssSolid: LineStippleEnd;
    lssDash: LineStipple(3 * enlarge, $AAAA);
    lssDashDot: LineStipple(1 * enlarge, $6F6F);
    lssDashDotDot: LineStipple(2 * enlarge, $EAEA);
    lssDot: LineStipple(1 * enlarge, $AAAA);
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.LineStippleEnd: TCnOpenGLPaintBox;
begin
  glDisable(GL_LINE_STIPPLE);
  Result := Self;
end;

function TCnOpenGLPaintBox.Line(const x1, y1, x2, y2: Integer): TCnOpenGLPaintBox;
begin
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.Line(const x1, y1, x2, y2: Single): TCnOpenGLPaintBox;
begin
  glBegin(GL_LINES);
  glVertex2f(x1, y1);
  glVertex2f(x2, y2);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.BeginLines: TCnOpenGLPaintBox;
begin
  glBegin(GL_LINES);
  Result := Self;
end;

function TCnOpenGLPaintBox.Lines(const x1, y1, x2, y2: Integer): TCnOpenGLPaintBox;
begin
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  Result := Self;
end;

function TCnOpenGLPaintBox.Lines(const x1, y1, x2, y2: Single): TCnOpenGLPaintBox;
begin
  glVertex2f(x1, y1);
  glVertex2f(x2, y2);
  Result := Self;
end;

function TCnOpenGLPaintBox.EndLines: TCnOpenGLPaintBox;
begin
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.Lines(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  glBegin(GL_LINES);
  i := 0;
  while (i <= count - 2) do
  begin
    glVertex2f(points[i].X, points[i].Y);
    glVertex2f(points[i + 1].X, points[i + 1].Y);
    Inc(i, 2);
  end;
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.Lines(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  glBegin(GL_LINES);
  i := 0;
  while (i <= count - 2) do
  begin
    glVertex2i(points[i].X, points[i].Y);
    glVertex2i(points[i + 1].X, points[i + 1].Y);
    Inc(i, 2);
  end;
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.Polyline(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  if count > 1 then
  begin
    glBegin(GL_LINE_STRIP);
    for i := 0 to count - 1 do
      glVertex2f(points[i].X, points[i].Y);
    glEnd;
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.Polygon(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  if count > 1 then
  begin
    glBegin(GL_LINE_LOOP);
    for i := 0 to count - 1 do
      glVertex2f(points[i].X, points[i].Y);
    glEnd;
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.Polyline(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  if count > 1 then
  begin
    glBegin(GL_LINE_STRIP);
    for i := 0 to count - 1 do
      glVertex2i(points[i].X, points[i].Y);
    glEnd;
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.Polygon(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  if count > 1 then
  begin
    glBegin(GL_LINE_LOOP);
    for i := 0 to count - 1 do
      glVertex2i(points[i].X, points[i].Y);
    glEnd;
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.FillPolygon(const points: TGLPointsF; count: Integer;
  Border: Boolean = False): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  if count > 1 then
  begin
    if not FIgnoreColor then
      glColor4fv(@FBrushColor);
    glBegin(GL_POLYGON);
    for i := 0 to count - 1 do
      glVertex2f(points[i].X, points[i].Y);
    glEnd;
    if not FIgnoreColor then 
      glColor4fv(@FPenColor);

    if Border then
      Polygon(points, count);
  end;
  Result := Self;
end;

function TCnOpenGLPaintBox.FillPolygon(const points: TGLPointsI; count: Integer;
  Border: Boolean = False): TCnOpenGLPaintBox;
var
  i: Integer;
begin
  if count > 1 then
  begin
    if not FIgnoreColor then 
      glColor4fv(@FBrushColor);
    glBegin(GL_POLYGON);
    for i := 0 to count - 1 do
      glVertex2i(points[i].X, points[i].Y);
    glEnd;  
    if not FIgnoreColor then 
      glColor4fv(@FPenColor);

    if Border then
      Polygon(points, count);
  end;
  Result := Self;
end;

{ The following methods are translated into Pascal from ReactOS source.
   calc_curve_bezier_endp
   calc_curve_bezier
   BEZIERMIDDLE
   BezierCheck
   GDI_InternalBezier
   GDI_Bezier
   GenCurvePoints  }
// Calculates Bezier points from cardinal spline endpoints.
procedure calc_curve_bezier_endp(xend, yend, xadj, yadj, tension: Single;
  var x, y: Single);
begin
  // tangent at endpoints is the line from the endpoint to the adjacent point
  x := tension * (xadj - xend) + xend;
  y := tension * (yadj - yend) + yend;
end;

// Calculates Bezier points from cardinal spline points.
procedure calc_curve_bezier(const pts: TGLPointsF; tension: Single;
  var x1, y1, x2, y2: Single);
var
  xdiff, ydiff: Single;
begin
  // calculate tangent
  xdiff := pts[2].X - pts[0].X;
  ydiff := pts[2].Y - pts[0].Y;

  // apply tangent to get control points
  x1 := pts[1].X - tension * xdiff;
  y1 := pts[1].Y - tension * ydiff;
  x2 := pts[1].X + tension * xdiff;
  y2 := pts[1].Y + tension * ydiff;
end;

procedure BEZIERMIDDLE(var Mid: TGLPointF; const P1, P2: TGLPointF);
begin
  Mid.x := (P1.x + P2.x) / 2;
  Mid.y := (P1.y + P2.y) / 2;
end;

type
  TGDIBezierPoints = array[0..3] of TGLPointF;

{
* BezierCheck helper function to check
* that recursion can be terminated
*     Points[0] and Points[3] are begin and endpoint
*     Points[1] and Points[2] are control points
*     level is the recursion depth
*     returns true if the recusion can be terminated
}
function BezierCheck(level: Integer; const Points: TGDIBezierPoints): Boolean;
const
  BEZIERPIXEL = 1;
var
  dx, dy: Single;
begin
  dx := Points[3].x - Points[0].x;
  dy := Points[3].y - Points[0].y;
  if Abs(dy) <= Abs(dx) then // shallow line
  begin
    // check that control points are between begin and end
    if Points[1].x < Points[0].x then
    begin
      if Points[1].x < Points[3].x then
      begin
        Result := False;
        Exit;
      end;
    end
    else if Points[1].x > Points[3].x then
    begin
      Result := False;
      Exit;
    end;

    if Points[2].x < Points[0].x then
    begin
      if Points[2].x < Points[3].x then
      begin
        Result := False;
        Exit;
      end;
    end
    else if Points[2].x > Points[3].x then
    begin
      Result := False;
      Exit;
    end;

    if IsZero(dx) then
    begin
      Result := True;
      Exit;
    end;

    if (Abs(Points[1].y - Points[0].y - (dy / dx) * (Points[1].x - Points[0].x)) > BEZIERPIXEL) or
      (Abs(Points[2].y - Points[0].y - (dy / dx) * (Points[2].x - Points[0].x)) > BEZIERPIXEL) then
    begin
      Result := False;
      Exit;
    end
    else
    begin
      Result := True;
      Exit;
    end;
  end
  else
  begin // steep line
    // check that control points are between begin and end
    if Points[1].y < Points[0].y then
    begin
      if Points[1].y < Points[3].y then
      begin
        Result := False;
        Exit;
      end;
    end
    else if Points[1].y > Points[3].y then
    begin
      Result := False;
      Exit;
    end;

    if Points[2].y < Points[0].y then
    begin
      if Points[2].y < Points[3].y then
      begin
        Result := False;
        Exit;
      end;
    end
    else if Points[2].y > Points[3].y then
    begin
      Result := False;
      Exit;
    end;

    if IsZero(dy) then
    begin
      Result := True;
      Exit;
    end;

    if (Abs(Points[1].x - Points[0].x - (dx / dy) * (Points[1].y - Points[0].y)) > BEZIERPIXEL) or
      (Abs(Points[2].x - Points[0].x - (dx / dy) * (Points[2].y - Points[0].y)) > BEZIERPIXEL) then
    begin
      Result := False;
      Exit;
    end
    else
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure GDI_InternalBezier(var Points: TGDIBezierPoints; var PtsOut: TGLPointsF;
  var dwOut, nPtsOut: Integer; level: Integer);
var
  Points2: TGDIBezierPoints; // for the second recursive call
begin
  if nPtsOut = dwOut then
  begin
    dwOut := dwOut * 2;
    SetLength(PtsOut, dwOut);
  end;

  if (level = 0) or BezierCheck(level, Points) then // Recursion can be terminated
  begin
    if nPtsOut = 0 then
    begin
      PtsOut[0] := Points[0];
      nPtsOut := 1;
    end;
    PtsOut[nPtsOut] := Points[3];
    Inc(nPtsOut);
  end
  else
  begin
    Points2[3] := Points[3];
    BEZIERMIDDLE(Points2[2], Points[2], Points[3]);
    BEZIERMIDDLE(Points2[0], Points[1], Points[2]);
    BEZIERMIDDLE(Points2[1],Points2[0],Points2[2]);

    BEZIERMIDDLE(Points[1], Points[0],  Points[1]);
    BEZIERMIDDLE(Points[2], Points[1], Points2[0]);
    BEZIERMIDDLE(Points[3], Points[2], Points2[1]);

    Points2[0] := Points[3];

    // do the two halves
    GDI_InternalBezier(Points, PtsOut, dwOut, nPtsOut, level - 1);
    GDI_InternalBezier(Points2, PtsOut, dwOut, nPtsOut, level - 1);
  end;
end;

procedure GDI_Bezier(const Points: TGLPointsF; count: Integer;
  var PtsOut: TGLPointsF; var nPtsOut:Integer);
var
  Bezier, dwOut: Integer;
  ptBuf: TGDIBezierPoints;
begin
  dwOut := 150;
  nPtsOut := 0;

  if (count - 1) mod 3 <> 0 then
    Exit;

  SetLength(PtsOut, dwOut);
  for Bezier := 0 to (count - 1) div 3 - 1 do
  begin
    Move(Points[Bezier * 3], ptBuf[0], SizeOf(ptBuf));
    GDI_InternalBezier(ptBuf, PtsOut, dwOut, nPtsOut, 8);
  end;
end;

procedure GenCurvePoints(const points: TGLPointsF; count: Integer;
  var outPoints: TGLPointsF; var outCount: Integer; tension: Single = 0.5);
var
  i, len_pt: Integer;
  x1, x2, y1, y2: Single;
  pt: TGLPointsF;
begin
  outCount := 0;
  if count <= 1 then
    Exit;

  // PolyBezier expects count*3-2 points.
  len_pt := count * 3 - 2;
  SetLength(pt, len_pt);
  tension := tension * 0.3;

  calc_curve_bezier_endp(points[0].X, points[0].Y, points[1].X, points[1].Y,
    tension, x1, y1);

  pt[0] := points[0];
  pt[1].X := x1;
  pt[1].Y := y1;

  for i := 0 to count - 3 do
  begin
    calc_curve_bezier(TGLPointsF(@(points[i])), tension, x1, y1, x2, y2);
    pt[3 * i + 2].X := x1;
    pt[3 * i + 2].Y := y1;
    pt[3 * i + 3] := points[i + 1];
    pt[3 * i + 4].X := x2;
    pt[3 * i + 4].Y := y2;
  end;

  calc_curve_bezier_endp(points[count - 1].X, points[count - 1].Y,
     points[count - 2].X, points[count - 2].Y, tension, x1, y1);
  pt[len_pt - 2].X := x1;
  pt[len_pt - 2].Y := y1;
  pt[len_pt - 1] := points[count - 1];

  GDI_Bezier(pt, len_pt, outPoints, outCount);
end;

function TCnOpenGLPaintBox.Curve(const points: TGLPointsF; count: Integer;
  tension: Single = 0.5): TCnOpenGLPaintBox;
var
  pt2: TGLPointsF;
  pt2Count: Integer;
begin
  Result := Self;
  if count <= 1 then
    Exit;

  GenCurvePoints(points, count, pt2, pt2Count, tension);
  Polyline(pt2, pt2Count);
end;

function TCnOpenGLPaintBox.Curve(const points: TGLPointsI; count: Integer;
  tension: Single = 0.5): TCnOpenGLPaintBox;
var
  i: Integer;
  pfs: TGLPointsF;
begin
  Result := Self;
  if count <= 1 then
    Exit;

  SetLength(pfs, count);
  for i := 0 to count - 1 do
  begin
    pfs[i].X := points[i].X;
    pfs[i].Y := points[i].Y;
  end;
  Curve(pfs, count, tension);
end;

function TCnOpenGLPaintBox.ClosedCurve(const points: TGLPointsF; count: Integer;
  tension: Single = 0.5): TCnOpenGLPaintBox;
var
  ps: TGLPointsF;
begin
  Result := Self;
  if count <= 2 then
    Exit;

  SetLength(ps, count + 1);
  Move(points[0], ps[0], SizeOf(TGLPointF) * count);
  ps[count] := ps[0]; // Close the curve
  Curve(ps, count + 1, tension);
end;

function TCnOpenGLPaintBox.ClosedCurve(const points: TGLPointsI; count: Integer;
  tension: Single = 0.5): TCnOpenGLPaintBox;
var
  i: Integer;
  ps: TGLPointsF;
begin
  Result := Self;
  if count <= 2 then
    Exit;

  SetLength(ps, count + 1);
  for i := 0 to count - 1 do
  begin
    ps[i].X := points[i].X;
    ps[i].Y := points[i].Y;
  end;
  ps[count] := ps[0]; // Close the curve
  Curve(ps, count + 1, tension);
end;

function TCnOpenGLPaintBox.FillClosedCurve(const points: TGLPointsF; count: Integer;
  Border: Boolean = False; tension: Single = 0.5): TCnOpenGLPaintBox;
var
  ps, pt2: TGLPointsF;
  pt2Count: Integer;
begin
  Result := Self;
  if count <= 2 then
    Exit;

  SetLength(ps, count + 1);
  Move(points[0], ps[0], SizeOf(TGLPointF) * count);
  ps[count] := ps[0]; // Close the curve

  GenCurvePoints(ps, count + 1, pt2, pt2Count, tension);
  FillPolygon(pt2, pt2Count, Border);
end;

function TCnOpenGLPaintBox.FillClosedCurve(const points: TGLPointsI; count: Integer;
  Border: Boolean = False; tension: Single = 0.5): TCnOpenGLPaintBox;
var
  i: Integer;
  ps, pt2: TGLPointsF;
  pt2Count: Integer;
begin
  Result := Self;
  if count <= 2 then
    Exit;

  SetLength(ps, count + 1);
  for i := 0 to count - 1 do
  begin
    ps[i].X := points[i].X;
    ps[i].Y := points[i].Y;
  end;
  ps[count] := ps[0]; // Close the curve

  GenCurvePoints(ps, count + 1, pt2, pt2Count, tension);
  FillPolygon(pt2, pt2Count, Border);
end;

function TCnOpenGLPaintBox.Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Integer): TCnOpenGLPaintBox;
var
  pt: TGDIBezierPoints;
  pt2: TGLPointsF;
  ptOut: Integer;
begin
  pt[0].X := x1;
  pt[0].Y := y1;
  pt[1].X := x2;
  pt[1].Y := y2;
  pt[2].X := x3;
  pt[2].Y := y3;
  pt[3].X := x4;
  pt[3].Y := y4;

  GDI_Bezier(TGLPointsF(@pt[0]), 4, pt2, ptOut);
  Polyline(pt2, ptOut);
  Result := Self;
end;

function TCnOpenGLPaintBox.Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Single): TCnOpenGLPaintBox;
var
  pt: TGDIBezierPoints;
  pt2: TGLPointsF;
  ptOut: Integer;
begin
  pt[0].X := x1;
  pt[0].Y := y1;
  pt[1].X := x2;
  pt[1].Y := y2;
  pt[2].X := x3;
  pt[2].Y := y3;
  pt[3].X := x4;
  pt[3].Y := y4;

  GDI_Bezier(TGLPointsF(@pt[0]), 4, pt2, ptOut);
  Polyline(pt2, ptOut);
  Result := Self;
end;

function TCnOpenGLPaintBox.PolyBezier(const points: TGLPointsI; count: Integer): TCnOpenGLPaintBox;
var
  i: Integer;
  ps, pt2: TGLPointsF;
  pt2Count: Integer;
begin
  SetLength(ps, count);
  for i := 0 to count - 1 do
  begin
    ps[i].X := points[i].X;
    ps[i].Y := points[i].Y;
  end;

  GDI_Bezier(ps, count, pt2, pt2Count);
  Polyline(pt2, pt2Count);
  Result := Self;
end;

function TCnOpenGLPaintBox.PolyBezier(const points: TGLPointsF; count: Integer): TCnOpenGLPaintBox;
var
  ps2Count: Integer;
  ps2: TGLPointsF;
begin
  GDI_Bezier(points, count, ps2, ps2Count);
  Polyline(ps2, ps2Count);
  Result := Self;
end;

const
  MAX_ARC_PTS = 13;
type
  TGdiArcPoints = array[0..MAX_ARC_PTS - 1] of TGLPointF;

  PGdiArcPointsSegment = ^TGdiArcPointsSegment;
  TGdiArcPointsSegment = array[0..3] of TGLPointF;

{ We plot the curve as if it is on a circle then stretch the points.  This
  adjusts the angles so that when we stretch the points they will end in the
  right place. This is only complicated because atan and atan2 do not behave
  conveniently. }
procedure unstretch_angle(var angle: Single; rad_x, rad_y: Single);
var
  stretched: Single;
  revs_off: Integer;
begin
   angle := DegToRad(angle);

   if(Abs(Cos(angle)) < 0.00001) or (Abs(Sin(angle)) < 0.00001) then
     Exit;

   stretched := ArcTan2(Sin(angle) / Abs(rad_y), Cos(angle) / Abs(rad_x));
   revs_off := Round(angle / _2Pi) - Round(stretched / _2Pi);
   angle := stretched + revs_off * _2Pi;
end;

{ Calculates the bezier points needed to fill in the arc portion starting at
  angle start and ending at end.  These two angles should be no more than 90
  degrees from each other.  x1, y1, x2, y2 describes the bounding box (upper
  left and width and height).  Angles must be in radians. write_first indicates
  that the first bezier point should be written out (usually this is false).
  pt is the array of GpPointFs that gets written to. }
procedure add_arc_part(pt: PGdiArcPointsSegment; const x1, y1, x2, y2: Single;
  startangle, endangle: Single; write_first: Boolean);
var
  i: Integer;
  center_x, center_y, rad_x, rad_y, cos_start, cos_end,
    sin_start, sin_end, a, half: Single;
begin
   rad_x := x2 / 2.0;
   rad_y := y2 / 2.0;
   center_x := x1 + rad_x;
   center_y := y1 + rad_y;

   SinCos(startangle, sin_start, cos_start);
   SinCos(endangle, sin_end, cos_end);

   half := (endangle - startangle) / 2.0;
   a := 4.0 / 3.0 * (1 - Cos(half)) / Sin(half);

   if write_first then
   begin
     pt^[0].X := cos_start;
     pt^[0].Y := sin_start;
   end;
   pt^[1].X := cos_start - a * sin_start;
   pt^[1].Y := sin_start + a * cos_start;

   pt^[3].X := cos_end;
   pt^[3].Y := sin_end;
   pt^[2].X := cos_end + a * sin_end;
   pt^[2].Y := sin_end - a * cos_end;

   // expand the points back from the unit circle to the ellipse
   if write_first then
   begin
     for i := 0 to 3 do
     begin
       pt^[i].X := pt^[i].X * rad_x + center_x;
       pt^[i].Y := pt^[i].Y * rad_y + center_y;
     end;
   end
   else
   begin
     for i := 1 to 3 do
     begin
       pt^[i].X := pt^[i].X * rad_x + center_x;
       pt^[i].Y := pt^[i].Y * rad_y + center_y;
     end;
   end;
end;

{ Stores the bezier points that correspond to the arc in points. If points is
  null, just return the number of points needed to represent the arc. }
function arc2polybezier(var points: TGdiArcPoints; const x1, y1, x2, y2: Single;
  var startAngle, sweepAngle: Single): Integer;
var
  i, count: Integer;
  end_angle, start_angle, endAngle: Single;
begin
   endAngle := startAngle + sweepAngle;
   unstretch_angle(startAngle, x2 / 2.0, y2 / 2.0);
   unstretch_angle(endAngle, x2 / 2.0, y2 / 2.0);

   count := Ceil(Abs(endAngle - startAngle) / PiDiv2) * 3 + 1;
   count := Min(MAX_ARC_PTS, count); // don't make more than a full circle

   if count = 1 then
   begin
     Result := 0;
     Exit;
   end;

   // start_angle and end_angle are the iterative variables
   start_angle := startAngle;
   i := 0;
   while (i < count - 1) do
   begin
     // check if we've overshot the end angle
     if sweepAngle > 0.0 then
       end_angle := Min(start_angle + PiDiv2, endAngle)
     else
       end_angle := Max(start_angle - PiDiv2, endAngle);

     if SameValue(start_angle, end_angle) then
     begin
       count := i + 1;
       Break;
     end;

     add_arc_part(PGdiArcPointsSegment(@points[i]), x1, y1, x2, y2,
       start_angle, end_angle, i = 0);

     if sweepAngle < 0.0 then
       start_angle := start_angle - PiDiv2
     else
       start_angle := start_angle + PiDiv2;
     i := i + 3;
   end;

   Result := count;
end;

function TCnOpenGLPaintBox.Arc(const x, y, xRadius, yRadius: Single;
  startAngle, sweepAngle: Single): TCnOpenGLPaintBox;
var
  num_pts: Integer;
  points: TGdiArcPoints;
begin
  num_pts := arc2polybezier(points, x - xRadius, y - yRadius,
    xRadius * 2, yRadius * 2, startAngle, sweepAngle);
  PolyBezier(TGLPointsF(@points[0]), num_pts);
  Result := Self;
end;

function TCnOpenGLPaintBox.FillPie(const x, y, xRadius, yRadius: Single;
  startAngle, sweepAngle: Single; Border: Boolean = False): TCnOpenGLPaintBox;
var
  i, num_pts: Integer;
  points: TGdiArcPoints;
  ps2Count: Integer;
  ps2: TGLPointsF;
begin
  if not FIgnoreColor then
    glColor4fv(@FBrushColor);
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(x, y); // not really necessary, but may help with memory stride

  num_pts := arc2polybezier(points, x - xRadius, y - yRadius,
    xRadius * 2, yRadius * 2, startAngle, sweepAngle);
  GDI_Bezier(TGLPointsF(@points[0]), num_pts, ps2, ps2Count);
  for i := 0 to ps2Count - 1 do
    glVertex2f(ps2[i].X, ps2[i].Y);

  glEnd;
  if not FIgnoreColor then
    glColor4fv(@FPenColor);

  if Border then
  begin
    glBegin(GL_LINE_STRIP);
    glVertex2f(x, y);
    for i := 0 to ps2Count - 1 do
      glVertex2f(ps2[i].X, ps2[i].Y);
    glVertex2f(x, y);
    glEnd;
  end;

  Result := Self;
end;

function TCnOpenGLPaintBox.PlotPixel(const x, y: Integer): TCnOpenGLPaintBox;
begin
  glBegin(GL_POINTS);
  glVertex2i(x, y);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.PlotPixel(const x, y: Single): TCnOpenGLPaintBox;
begin
  glBegin(GL_POINTS);
  glVertex2f(x, y);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.BeginPixels: TCnOpenGLPaintBox;
begin
  glBegin(GL_POINTS);
  Result := Self;
end;

function TCnOpenGLPaintBox.Pixels(const x, y: Integer): TCnOpenGLPaintBox;
begin
  glVertex2i(x, y);
  Result := Self;
end;

function TCnOpenGLPaintBox.Pixels(const x, y: Single): TCnOpenGLPaintBox;
begin
  glVertex2f(x, y);
  Result := Self;
end;

function TCnOpenGLPaintBox.EndPixels: TCnOpenGLPaintBox;
begin
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.FrameRect(const x1, y1, x2, y2: Integer): TCnOpenGLPaintBox;
begin
  glBegin(GL_LINE_LOOP);
  glVertex2i(x1, y1);
  glVertex2i(x2, y1);
  glVertex2i(x2, y2);
  glVertex2i(x1, y2);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.FrameRect(const x1, y1, x2, y2: Single): TCnOpenGLPaintBox;
begin
  glBegin(GL_LINE_LOOP);
  glVertex2f(x1, y1);
  glVertex2f(x2, y1);
  glVertex2f(x2, y2);
  glVertex2f(x1, y2);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.FillRect(const x1, y1, x2, y2: Integer; Border: Boolean = False): TCnOpenGLPaintBox;
begin
  if not FIgnoreColor then 
    glColor4fv(@FBrushColor);
  glRecti(x1, y1, x2, y2);
  if not FIgnoreColor then 
    glColor4fv(@FPenColor);
  if Border then
    FrameRect(x1, y1, x2, y2);
  Result := Self;
end;

function TCnOpenGLPaintBox.FillRect(const x1, y1, x2, y2: Single; Border: Boolean = False): TCnOpenGLPaintBox;
begin
  if not FIgnoreColor then 
    glColor4fv(@FBrushColor);
  glRectf(x1, y1, x2, y2);
  if not FIgnoreColor then 
    glColor4fv(@FPenColor);
  if Border then
    FrameRect(x1, y1, x2, y2);
  Result := Self;
end;

function TCnOpenGLPaintBox.Triangle(const x1, y1, x2, y2, x3, y3: Integer): TCnOpenGLPaintBox;
begin
  glBegin(GL_LINE_LOOP);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glVertex2i(x3, y3);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.Triangle(const x1, y1, x2, y2, x3, y3: Single): TCnOpenGLPaintBox;
begin
  glBegin(GL_LINE_LOOP);
  glVertex2f(x1, y1);
  glVertex2f(x2, y2);
  glVertex2f(x3, y3);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.FillTriangle(const x1, y1, x2, y2, x3, y3: Integer;
  Border: Boolean = False): TCnOpenGLPaintBox;
begin
  if not FIgnoreColor then 
    glColor4fv(@FBrushColor);
  glBegin(GL_TRIANGLE_FAN);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glVertex2i(x3, y3);
  glEnd;
  if not FIgnoreColor then 
    glColor4fv(@FPenColor);
  if Border then
    Triangle(x1, y1, x2, y2, x3, y3);
  Result := Self;
end;

function TCnOpenGLPaintBox.FillTriangle(const x1, y1, x2, y2, x3, y3: Single;
  Border: Boolean = False): TCnOpenGLPaintBox;
begin
  if not FIgnoreColor then 
    glColor4fv(@FBrushColor);
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(x1, y1);
  glVertex2f(x2, y2);
  glVertex2f(x3, y3);
  glEnd;
  if not FIgnoreColor then 
    glColor4fv(@FPenColor);
  if Border then
    Triangle(x1, y1, x2, y2, x3, y3);
  Result := Self;
end;

// Ellipse drawing methods are borrowed from GLScene.GLCanvas unit

procedure PrepareSinCosCache(var s, c: array of Single;
  startAngle, stopAngle: Single);
var
  i: Integer;
  d, alpha, beta: Single;
begin
  stopAngle := stopAngle + 1E-5;
  if High(s) > Low(s) then
    d := PiDiv180 * (stopAngle - startAngle) / (High(s) - Low(s))
  else
    d := 0;

  if High(s) - Low(s) < 1000 then
  begin
    // Fast computation (approx 5.5x)
    alpha := 2 * Sqr(Sin(d * 0.5));
    beta := Sin(d);
    SinCos(startAngle * PiDiv180, s[Low(s)], c[Low(s)]);
    for i := Low(s) to High(s) - 1 do
    begin
      // Make use of the incremental formulae:
      // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
      // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
      c[i + 1] := c[i] - alpha * c[i] - beta * s[i];
      s[i + 1] := s[i] - alpha * s[i] + beta * c[i];
    end;
  end
  else
  begin
    // Slower, but maintains precision when steps are small
    startAngle := startAngle * PiDiv180;
    for i := Low(s) to High(s) do
      SinCos((i - Low(s)) * d + startAngle, s[i], c[i]);
  end;
end;

var
   SmallRadiusSinArray, SmallRadiusCosArray,
   MediumRadiusSinArray, MediumRadiusCosArray: TSingleArray;  // Cache

procedure TCnOpenGLPaintBox.EllipseVertices(const x, y, xRadius, yRadius: Single);
var
   maxRadius: Single;
   i, n: Integer;
   s, c: TSingleArray;
begin
   if xRadius > yRadius then
      maxRadius := xRadius
   else
      maxRadius := yRadius;

   if maxRadius <= 10 then
   begin
      SetLength(s, 4);
      SetLength(c, 4);
      Move(SmallRadiusSinArray[0], s[0], SizeOf(Single) * 4);
      Move(SmallRadiusCosArray[0], c[0], SizeOf(Single) * 4);
      n := 3;
   end
   else if maxRadius < 85 then
   begin
      SetLength(s, 12);
      SetLength(c, 12);
      Move(MediumRadiusSinArray[0], s[0], SizeOf(Single) * 12);
      Move(MediumRadiusCosArray[0], c[0], SizeOf(Single) * 12);
      n := 11;
   end
   else
   begin
      n := Round(maxRadius * 0.1) + 4;
      SetLength(s, n);
      SetLength(c, n);
      Dec(n);
      PrepareSinCosCache(s, c, 0, 90);
   end;

   // first quadrant (top right)
   for i := 0 to n do
   begin
      s[i] := s[i] * yRadius;
      c[i] := c[i] * xRadius;
      glVertex2f(x + c[i], y - s[i]);
   end;
   // second quadrant (top left)
   for i := n - 1 downto 0 do
      glVertex2f(x - c[i], y - s[i]);
   // third quadrant (bottom left)
   for i := 1 to n do
      glVertex2f(x - c[i], y + s[i]);
   // fourth quadrant (bottom right)
   for i := n - 1 downto 0 do
      glVertex2f(x + c[i], y + s[i]);
end;

function TCnOpenGLPaintBox.EllipseRect(const x1, y1, x2, y2: Single): TCnOpenGLPaintBox;
begin
  Ellipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) * 0.5);
  Result := Self;
end;

function TCnOpenGLPaintBox.Ellipse(const x, y, xRadius, yRadius: Single): TCnOpenGLPaintBox;
begin
  glBegin(GL_LINE_STRIP);
  EllipseVertices(x, y, xRadius, yRadius);
  glEnd;
  Result := Self;
end;

function TCnOpenGLPaintBox.FillEllipseRect(const x1, y1, x2, y2: Single;
  Border: Boolean = False): TCnOpenGLPaintBox;
begin
  FillEllipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) * 0.5, Border);
  Result := Self;
end;

function TCnOpenGLPaintBox.FillEllipse(const x, y, xRadius, yRadius: Single;
  Border: Boolean = False): TCnOpenGLPaintBox;
begin
  if not FIgnoreColor then
    glColor4fv(@FBrushColor);
  glBegin(GL_TRIANGLE_FAN);
  glVertex2f(x, y); // not really necessary, but may help with memory stride
  EllipseVertices(x, y, xRadius, yRadius);
  glEnd;
  if not FIgnoreColor then 
    glColor4fv(@FPenColor);
  if Border then
    Ellipse(x, y, xRadius, yRadius);
  Result := Self;
end;

procedure TCnOpenGLPaintBox.RecreateDefaultFont;
begin
  if Assigned(FDefaultFont) then
    FDefaultFont.Free;
  FDefaultFont := TCnGLFont.Create('Tahoma', 14);
  FDefaultFont.NotifyChange := DefaultFontNotify; // Mustn't assign NotifyChange by TCnGLFont.Create
end;

function TCnOpenGLPaintBox.TextOutASCII(const text: string; x, y: Integer;
  Font: TCnGLFont = nil): TCnOpenGLPaintBox;
var
  i: Integer;
  GLList: GLuint;
  NeedFreeList: Boolean;
begin
  if Assigned(Font) then
  begin
    SelectObject(FBufferHDC, Font.HFont);
    GLList := glGenLists(MaxChar);
    wglUseFontBitmaps(FBufferHDC, 0, MaxChar, GLList);
    NeedFreeList := True;
  end
  else
  begin
    Font := FDefaultFont;
    GLList := FASCIICharList;
    NeedFreeList := False;
  end;

  glPushMatrix;
  glLoadIdentity;
  glColor4fv(@Font.FColorVector);
  glRasterPos2i(x, y);
  for i := 1 to Length(text) do
    glCallList(GLList + Ord(text[i]));
  glPopMatrix;

  if NeedFreeList then
    glDeleteLists(GLList, MaxChar);
  glColor4fv(@FPenColor);
  Result := Self;
end;

function TCnOpenGLPaintBox.TextOut(const text: WideString; x, y: Integer;
  Font: TCnGLFont = nil): TCnOpenGLPaintBox;
var
  i: Integer;
  list: GLuint;
begin
  if not Assigned(Font) then
    Font := FDefaultFont;
  SelectObject(FBufferHDC, Font.HFont);
  glColor4fv(@Font.FColorVector);

  glPushMatrix;
  glLoadIdentity;
  glRasterPos2i(x, y);
  list := glGenLists(1);
  for i := 1 to Length(text) do
  begin
    wglUseFontBitmapsW(FBufferHDC, Ord(text[i]), 1, list);
    glCallList(list);
  end;
  glDeleteLists(list, 1);
  glColor4fv(@FPenColor);
  glPopMatrix;
  Result := Self;
end;

function TCnOpenGLPaintBox.BuildTexture(bmp: TBitmap; var texId: GLuint): TCnOpenGLPaintBox; // Creates Texture From A Bitmap File
var
  bmpInfo: BITMAP;
begin
  GetObject(bmp.Handle, SizeOf(bmpInfo), @bmpInfo);
  glGenTextures(1, @texId);       // Create The Texture
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  // Typical Texture Generation Using Data From The Bitmap
  glBindTexture(GL_TEXTURE_2D, texId);      // Bind To The Texture ID
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); // Linear Min Filter
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); // Linear Mag Filter
  glTexImage2D(GL_TEXTURE_2D, 0, 3, bmpInfo.bmWidth, bmpInfo.bmHeight, 0, GL_BGR, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
  Result := Self;
end;

function TCnOpenGLPaintBox.DeleteTexture(texId: GLuint): TCnOpenGLPaintBox;
begin
  glDeleteTextures(1, @texId);
  Result := Self;
end;

function TCnOpenGLPaintBox.DrawBitmap(bmp: TBitmap; x, y: Integer;
  xZoom: Single = 1.0; yZoom: Single = 1.0): TCnOpenGLPaintBox;
var
  bmpInfo: BITMAP;
begin
  GetObject(bmp.Handle, SizeOf(bmpInfo), @bmpInfo);
  glPixelZoom(xZoom, yZoom);
  glPushMatrix;
  glLoadIdentity;
  glRasterPos2i(x, y);
  glDrawPixels(bmp.Width, bmp.Height, GL_BGR, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
  glPopMatrix;
  Result := Self;
end;

function TCnOpenGLPaintBox.DrawBitmapTex(bmp: TBitmap; x, y, w, h: Integer): TCnOpenGLPaintBox;
var
  tex: GLuint;
begin
  glColor3f(1.0, 1.0, 1.0);
  glDisable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  BuildTexture(bmp, tex);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0); glVertex3i(x, y, 0);
  glTexCoord2f(1.0, 0.0); glVertex3f(x + w, y, 0);
  glTexCoord2f(1.0, 1.0); glVertex3f(x + w, y + h, 0);
  glTexCoord2f(0.0, 1.0); glVertex3f(x, y + h, 0);
  glEnd;

  glDeleteTextures(1, @tex);
  glDisable(GL_TEXTURE_2D);
  SetBlendState(FBlend);
  glColor4fv(@FPenColor); // Restore color
  Result := Self;
end;

function TCnOpenGLPaintBox.DrawBitmapTex(texId: GLuint; x, y, w, h: Integer): TCnOpenGLPaintBox;
begin
  glColor3f(1.0, 1.0, 1.0);
  glDisable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);

  glBindTexture(GL_TEXTURE_2D, texid);      // Bind To The Texture ID
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0); glVertex3i(x, y, 0);
  glTexCoord2f(1.0, 0.0); glVertex3f(x + w, y, 0);
  glTexCoord2f(1.0, 1.0); glVertex3f(x + w, y + h, 0);
  glTexCoord2f(0.0, 1.0); glVertex3f(x, y + h, 0);
  glEnd;

  glDisable(GL_TEXTURE_2D);
  SetBlendState(FBlend);
  glColor4fv(@FPenColor); // Restore color
  Result := Self;
end;

initialization
   SetLength(SmallRadiusSinArray, 4);
   SetLength(SmallRadiusCosArray, 4);
   PrepareSinCosCache(SmallRadiusSinArray, SmallRadiusCosArray, 0, 90);

   SetLength(MediumRadiusSinArray, 12);
   SetLength(MediumRadiusCosArray, 12);
   PrepareSinCosCache(MediumRadiusSinArray, MediumRadiusCosArray, 0, 90);

end.
