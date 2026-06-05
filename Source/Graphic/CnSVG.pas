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

unit CnSVG;
{* |<PRE>
================================================================================
* 单元名称：CnSVG
* 单元说明：SVG 1.1 核心子集渲染器单元
*           通过 CnXML.pas 解析 SVG 文件的 XML 结构，利用 Windows GDI（TCanvas）
*           将 SVG 图像渲染到位图或控件画布，并提供 TCnSVGImage 可视化组件。
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：严格遵守 Delphi 5 语法，不使用泛型、for..in、inline var、record 方法等
* 适用平台：PWin98SE + Delphi 5.0
* 已测平台：PWin7/10+ Delphi 5~最新
* 修改记录：2026.06.01 V1.0
*                创建本单元骨架
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Graphics, Math, Controls, {$IFNDEF FPC} Jpeg, {$ENDIF}
  CnNative, CnClasses, CnCommon, CnGraphics, CnXML;

type

//==============================================================================
// 基础数值类型
//==============================================================================

  TCnSVGFloat = Single;
  {* SVG 内部使用的浮点类型，Single 精度满足 GDI 像素级需求 }

  TCnSVGColor = record
  {* SVG 颜色，内部以 R/G/B 三分量存储 }
    R, G, B: Byte;
  end;

  PCnSVGPoint = ^TCnSVGPoint;
  TCnSVGPoint = record
  {* SVG 用户坐标系中的点 }
    X, Y: TCnSVGFloat;
  end;

  PCnSVGRect = ^TCnSVGRect;
  TCnSVGRect = record
  {* SVG 矩形区域，用于 viewBox 及内部裁剪 }
    X, Y, Width, Height: TCnSVGFloat;
  end;

//==============================================================================
// 样式枚举类型
//==============================================================================

  TCnSVGFillRule = (sfrNonZero, sfrEvenOdd);
  {* SVG 填充规则：nonzero（非零缠绕）或 evenodd（奇偶填充） }

  TCnSVGLineCap = (slcButt, slcRound, slcSquare);
  {* SVG 线端样式：butt（平直）/ round（圆形）/ square（方形） }

  TCnSVGLineJoin = (sljMiter, sljRound, sljBevel);
  {* SVG 线角样式：miter（斜接）/ round（圆角）/ bevel（斜切） }

  TCnSVGTextAnchor = (staStart, staMiddle, staEnd);
  {* SVG 文字锚点：start（左对齐）/ middle（居中）/ end（右对齐） }

//==============================================================================
// 样式记录
//==============================================================================

  TCnSVGStyle = record
  {* SVG 完整样式记录，每个字段对应一个可继承的 CSS/SVG 样式属性 }
    // 填充
    FillColor:        TCnSVGColor;
    {* fill 颜色 }
    FillNone:         Boolean;
    {* fill = none 时为 True }
    FillOpacity:      TCnSVGFloat;
    {* fill-opacity，范围 [0,1] }
    FillRule:         TCnSVGFillRule;
    {* fill-rule：nonzero / evenodd }
    // 描边
    StrokeColor:      TCnSVGColor;
    {* stroke 颜色 }
    StrokeNone:       Boolean;
    {* stroke = none 时为 True }
    StrokeOpacity:    TCnSVGFloat;
    {* stroke-opacity，范围 [0,1] }
    StrokeWidth:      TCnSVGFloat;
    {* stroke-width，用户坐标单位 }
    LineCap:          TCnSVGLineCap;
    {* stroke-linecap }
    LineJoin:         TCnSVGLineJoin;
    {* stroke-linejoin }
    DashArray:        array[0..7] of TCnSVGFloat;
    {* stroke-dasharray，最多 8 段 }
    DashCount:        Integer;
    {* stroke-dasharray 中有效段数 }
    DashOffset:       TCnSVGFloat;
    {* stroke-dashoffset }
    // 全局透明度
    Opacity:          TCnSVGFloat;
    {* opacity，范围 [0,1] }
    // 文字
    FontSize:         TCnSVGFloat;
    {* font-size，用户坐标单位 }
    FontFamily:       string;
    {* font-family }
    FontBold:         Boolean;
    {* font-weight = bold }
    FontItalic:       Boolean;
    {* font-style = italic/oblique }
    TextAnchor:       TCnSVGTextAnchor;
    {* text-anchor }
    // 可见性
    DisplayNone:      Boolean;
    {* display = none 时为 True }
    VisibilityHidden: Boolean;
    {* visibility = hidden 或 collapse 时为 True }
    // 描边斜接限制
    MiterLimit:       TCnSVGFloat;
    {* stroke-miterlimit，默认 4.0，最小 1.0 }
    // 当前颜色（供 currentColor 关键字使用）
    CurrentColor:     TCnSVGColor;
    {* color 属性值，fill/stroke 值为 currentColor 时取此颜色 }
    CurrentColorSet:  Boolean;
    {* color 属性是否在本层或祖先已显式声明 }
  end;

//==============================================================================
// 仿射变换矩阵
//==============================================================================

  PCnSVGMatrix = ^TCnSVGMatrix;
  TCnSVGMatrix = record
  {* SVG 2D 仿射变换矩阵，表示形式：
     | a  c  e |
     | b  d  f |
     | 0  0  1 |
     对应 SVG matrix(a,b,c,d,e,f) 参数顺序 }
    a, b, c, d, e, f: TCnSVGFloat;
  end;

//==============================================================================
// 渲染上下文
//==============================================================================

  TCnSVGRenderContext = record
  {* 渲染过程传递的全局状态，包含 TCanvas、当前 CTM、样式栈顶，以及 use 递归深度 }
    Canvas:   TCanvas;
    {* 目标 GDI 画布 }
    CTM:      TCnSVGMatrix;
    {* 当前变换矩阵（Current Transformation Matrix） }
    Style:    TCnSVGStyle;
    {* 当前继承样式 }
    UseDepth: Integer;
    {* <use> 实例化递归深度，最大 8 }
  end;

//==============================================================================
// 路径段类型
//==============================================================================

  TCnSVGPathSegType = (
  {* SVG 路径段类型枚举，对应 SVG 1.1 path d 属性中的各路径命令 }
    pstMoveTo,       {* M/m：移动到指定点 }
    pstLineTo,       {* L/l：从当前点画直线到指定点 }
    pstHLineTo,      {* H/h：水平线 }
    pstVLineTo,      {* V/v：垂直线 }
    pstCubicBezier,  {* C/c：三次贝塞尔曲线 }
    pstSmoothCubic,  {* S/s：平滑三次贝塞尔 }
    pstQuadBezier,   {* Q/q：二次贝塞尔曲线 }
    pstSmoothQuad,   {* T/t：平滑二次贝塞尔 }
    pstArc,          {* A/a：椭圆弧 }
    pstClosePath     {* Z/z：闭合路径 }
  );

  PCnSVGPathSeg = ^TCnSVGPathSeg;
  {* 指向 TCnSVGPathSeg 的指针类型 }

  TCnSVGPathSeg = record
  {* 单条路径段，解析后统一存储在绝对坐标下 }
    SegType:   TCnSVGPathSegType;
    {* 段类型 }
    // 通用端点
    X, Y:      TCnSVGFloat;
    {* 目标点坐标（绝对坐标） }
    // 贝塞尔控制点
    X1, Y1:    TCnSVGFloat;
    {* 第一控制点 }
    X2, Y2:    TCnSVGFloat;
    {* 第二控制点（三次贝塞尔） }
    // 圆弧参数
    RX, RY:    TCnSVGFloat;
    {* 椭圆半轴 }
    XRotation: TCnSVGFloat;
    {* x-axis-rotation（度） }
    LargeArc:  Boolean;
    {* large-arc-flag }
    Sweep:     Boolean;
    {* sweep-flag }
  end;


//==============================================================================
// 异常类
//==============================================================================

  ECnSVGException = class(Exception);
  {* CnSVG 模块专用异常类，用于报告 SVG 加载、解析和渲染时的错误 }

//==============================================================================
// 前向声明
//==============================================================================

  TCnSVGDocument = class;
  TCnSVGImage = class;

//==============================================================================
// 路径解析器
//==============================================================================

  TCnSVGPathParser = class(TObject)
  {* SVG <path> d 属性解析器。
     使用状态机分词，将 d 字符串转换为 TCnSVGPathSeg 列表（绝对坐标）。 }
  private
    FData:     string;
    {* 待解析的 d 属性字符串 }
    FPos:      Integer;
    {* 当前扫描位置（1-based） }
    FCurX:     TCnSVGFloat;
    {* 当前笔位 X 坐标（绝对坐标） }
    FCurY:     TCnSVGFloat;
    {* 当前笔位 Y 坐标（绝对坐标） }
    FLastCtlX: TCnSVGFloat;
    {* 上一个贝塞尔控制点 X（用于 S/T 平滑命令） }
    FLastCtlY: TCnSVGFloat;
    {* 上一个贝塞尔控制点 Y（用于 S/T 平滑命令） }
    FLastCmd:  Char;
    {* 上一个命令字符（用于重复参数处理） }
    procedure SkipWS;
    {* 跳过空白和逗号 }
    function ReadNumber(var Val: TCnSVGFloat): Boolean;
    {* 读取一个浮点数（含科学计数法、正负号紧跟），返回是否成功 }
    function PeekCmd: Char;
    {* 检查当前字符是否为命令字母，返回该字母，否则返回 #0 }
    procedure ParseMoveTo(Relative: Boolean; List: TList);
    {* 解析 M/m 命令 }
    procedure ParseLineTo(Relative: Boolean; List: TList);
    {* 解析 L/l 命令 }
    procedure ParseHLineTo(Relative: Boolean; List: TList);
    {* 解析 H/h 命令 }
    procedure ParseVLineTo(Relative: Boolean; List: TList);
    {* 解析 V/v 命令 }
    procedure ParseCubicBezier(Relative: Boolean; List: TList);
    {* 解析 C/c 命令 }
    procedure ParseSmoothCubic(Relative: Boolean; List: TList);
    {* 解析 S/s 命令，反射上一控制点 }
    procedure ParseQuadBezier(Relative: Boolean; List: TList);
    {* 解析 Q/q 命令 }
    procedure ParseSmoothQuad(Relative: Boolean; List: TList);
    {* 解析 T/t 命令，规范化为 Q }
    procedure ParseArc(Relative: Boolean; List: TList);
    {* 解析 A/a 命令，读取 7 个参数 }
    procedure ParseClosePath(List: TList);
    {* 解析 Z/z 命令 }
  public
    constructor Create;
    {* 构造函数 }
    destructor Destroy; override;
    {* 析构函数 }

    function ParsePathData(const D: string): TList;
    {* 解析路径 d 属性，返回 TList（元素为 PCnSVGPathSeg，堆分配）。
       调用方负责释放列表中每个 PCnSVGPathSeg 及 TList 本身。
       D 为空或仅含空白时返回空列表，不抛异常。 }
  end;

//==============================================================================
// SVG 文档类
//==============================================================================

  TCnSVGDocument = class(TObject)
  {* SVG 文档加载与渲染门面类。
     持有 TCnXMLDocument DOM 树，对外提供 Render 接口。
     内部创建 TCnSVGRenderer 执行实际渲染。 }
  private
    FXMLDoc:         TCnXMLDocument;
    {* XML DOM 文档对象，由本类创建/销毁 }
    FIsLoaded:       Boolean;
    {* 是否已成功加载 SVG }
    FViewportWidth:  TCnSVGFloat;
    {* SVG 视口宽度（px） }
    FViewportHeight: TCnSVGFloat;
    {* SVG 视口高度（px） }
    FViewBox:        TCnSVGRect;
    {* viewBox 用户坐标系矩形 }
    FSourceFileName: string;
    {* 当前 SVG 文件路径；从流加载时为空，用于解析相对 image 路径 }
    FDefsMap:        TStringList;
    {* id → TCnXMLElement 的字符串列表，存储 <defs> 内容 }
    procedure ParseViewport;
    {* 从 <svg> 元素解析 width/height/viewBox }
    procedure BuildDefsMap;
    {* 遍历 <defs> 子元素，填充 FDefsMap }
    procedure DoLoad;
    {* 公共加载后处理：调用 ParseViewport 和 BuildDefsMap }
  public
    constructor Create;
    {* 构造函数，初始化内部对象 }
    destructor Destroy; override;
    {* 析构函数，释放 FXMLDoc 和 FDefsMap }

    procedure LoadFromFile(const AFileName: string);
    {* 从文件加载 SVG。
       参数：
         AFileName: string - SVG 文件完整路径
       若文件不存在或 XML 解析失败，抛出 ECnSVGException。
       若根元素不是 <svg>，抛出 ECnSVGException。 }

    procedure LoadFromStream(AStream: TStream);
    {* 从流加载 SVG。
       参数：
         AStream: TStream - 可读流，不能为 nil
       若 AStream = nil 或 XML 解析失败，抛出 ECnSVGException。 }

    procedure Clear;
    {* 清除已加载内容，释放 DOM 树，IsLoaded 置 False }

    procedure Render(ACanvas: TCanvas; const ADestRect: TRect);
    {* 将 SVG 渲染到指定画布的目标矩形。
       ADestRect 为空矩形时立即返回（不渲染）。 }

    property IsLoaded: Boolean read FIsLoaded;
    {* 是否已成功加载 SVG }
    property ViewportWidth: TCnSVGFloat read FViewportWidth;
    {* SVG 视口宽度 }
    property ViewportHeight: TCnSVGFloat read FViewportHeight;
    {* SVG 视口高度 }
    property ViewBox: TCnSVGRect read FViewBox;
    {* SVG viewBox 用户坐标系 }
  end;

//==============================================================================
// TCnSVGImage 可视化组件
//==============================================================================

  TCnSVGImage = class(TGraphicControl)
  {* SVG 图像显示控件，继承自 TGraphicControl（Delphi 5 兼容）。
     通过 CnGraphRegister.pas 注册到"CnPack Graphic"组件面板。 }
  private
    FDocument:         TCnSVGDocument;
    {* 内部 SVG 文档对象 }
    FFileName:         string;
    {* SVG 文件路径 }
    FStretch:          Boolean;
    {* 是否拉伸到客户区 }
    FCenter:           Boolean;
    {* 是否居中（Stretch=False 时有效） }
    FProportional:     Boolean;
    {* 是否保持宽高比（Stretch=True 时有效） }
    FLastError:        string;
    {* 最近一次加载或渲染错误信息 }
    FLastRenderTimeMs: Cardinal;
    {* 最近一次渲染耗时（毫秒） }
    FOnRenderDone:     TNotifyEvent;
    {* 渲染完成事件 }
    procedure SetFileName(const Value: string);
    {* 设置 FileName 属性，立即触发加载 }
    procedure SetStretch(Value: Boolean);
    {* 设置 Stretch 属性，触发重绘 }
    procedure SetCenter(Value: Boolean);
    {* 设置 Center 属性，触发重绘 }
    procedure SetProportional(Value: Boolean);
    {* 设置 Proportional 属性，触发重绘 }
    function CalcDestRect: TRect;
    {* 根据 Stretch/Center/Proportional 及控件尺寸计算渲染目标矩形 }
  protected
    procedure Paint; override;
    {* 渲染 SVG 或绘制错误叉 }
    procedure Resize; override;
    {* 控件尺寸变化时触发重绘 }
  public
    constructor Create(AOwner: TComponent); override;
    {* 构造函数，初始化属性默认值 }
    destructor Destroy; override;
    {* 析构函数，释放内部文档对象 }

    property LastError: string read FLastError;
    {* 最近一次错误信息（只读），加载失败时记录，成功加载后清空 }
    property LastRenderTimeMs: Cardinal read FLastRenderTimeMs;
    {* 最近一次渲染耗时毫秒数（只读） }
  published
    property FileName: string read FFileName write SetFileName;
    {* SVG 文件路径，赋值后立即加载并刷新显示 }
    property Stretch: Boolean read FStretch write SetStretch default True;
    {* 是否拉伸到控件客户区 }
    property Center: Boolean read FCenter write SetCenter default False;
    {* 是否居中显示（Stretch=False 时有效） }
    property Proportional: Boolean read FProportional write SetProportional default True;
    {* 是否保持宽高比（Stretch=True 时有效） }
    property Align;
    property Anchors;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnRenderDone: TNotifyEvent read FOnRenderDone write FOnRenderDone;
    {* 每次渲染完成后触发，设计时不触发 }
  end;

//==============================================================================
// 矩阵运算过程/函数声明
//==============================================================================

procedure SVGMatrixIdentity(var M: TCnSVGMatrix);
{* 将矩阵 M 设为单位矩阵 }

procedure SVGMatrixMultiply(var Result: TCnSVGMatrix;
  const A, B: TCnSVGMatrix);
{* 矩阵乘法：Result = A × B（先应用 B，再应用 A） }

procedure SVGMatrixTranslate(var M: TCnSVGMatrix;
  TX, TY: TCnSVGFloat);
{* 在矩阵 M 右侧叠加平移 translate(TX, TY) }

procedure SVGMatrixScale(var M: TCnSVGMatrix;
  SX, SY: TCnSVGFloat);
{* 在矩阵 M 右侧叠加缩放 scale(SX, SY) }

procedure SVGMatrixRotate(var M: TCnSVGMatrix;
  AngleDeg, CX, CY: TCnSVGFloat);
{* 在矩阵 M 右侧叠加绕 (CX,CY) 旋转 AngleDeg 度 }

procedure SVGMatrixSkewX(var M: TCnSVGMatrix;
  AngleDeg: TCnSVGFloat);
{* 在矩阵 M 右侧叠加沿 X 轴方向的错切变换，等价矩阵 [1,0,tan(angle),1,0,0] }

procedure SVGMatrixSkewY(var M: TCnSVGMatrix;
  AngleDeg: TCnSVGFloat);
{* 在矩阵 M 右侧叠加沿 Y 轴方向的错切变换，等价矩阵 [1,tan(angle),0,1,0,0] }

procedure SVGMatrixTransformPoint(const M: TCnSVGMatrix;
  var X, Y: TCnSVGFloat);
{* 将用户坐标点 (X, Y) 经矩阵 M 变换，结果写回 X, Y }

function SVGMatrixInverse(const M: TCnSVGMatrix;
  var Inv: TCnSVGMatrix): Boolean;
{* 计算矩阵 M 的逆矩阵，写入 Inv；若矩阵奇异则返回 False }

//==============================================================================
// 独立渲染函数声明
//==============================================================================

procedure CnSVGRenderToCanvas(const AFileName: string;
  ACanvas: TCanvas; const ADestRect: TRect);
{* 从文件加载 SVG 并渲染到指定画布区域。
   内部创建临时 TCnSVGDocument，渲染完成后自动释放。
   ADestRect 为空矩形时立即返回；加载/渲染异常向上传播。 }

procedure CnSVGRenderToCanvasFromStream(AStream: TStream;
  ACanvas: TCanvas; const ADestRect: TRect);
{* 从流加载 SVG 并渲染到指定画布区域，行为同 CnSVGRenderToCanvas。 }

function CnSVGRenderToBitmap(const AFileName: string;
  AWidth, AHeight: Integer): TBitmap;
{* 从文件加载 SVG，渲染到新建 AWidth×AHeight 的 TBitmap（pf24bit）后返回。
   若 AWidth ≤ 0 或 AHeight ≤ 0，抛出 ECnSVGException。
   返回的 TBitmap 由调用方负责释放。 }

implementation

resourcestring
  SCnErrorSvgNotExistsFmt = 'SVG File NOT Exists %s';
  SCnErrorSvgXmlParseErrorFmt = 'SVG XML Parse Error: %s';
  SCnErrorSvgRootSvg = 'Root Element Must Be svg';
  SCnErrorSvgStreamNil = 'SVG Stream Can NOT be nil';
  SCnErrorSvgRenderSize = 'CnSVGRenderToBitmap Width and Height Must > 0';


//==============================================================================
// TCnSVGRenderer 内部渲染引擎（仅在 implementation 中声明）
//==============================================================================

type
  TCnSVGRenderer = class(TObject)
  {* SVG 渲染引擎。由 TCnSVGDocument.Render 创建，不对外暴露。
     持有 CTM 变换栈和样式继承栈，递归遍历 DOM 树调用 GDI。}
  private
    FCtx:        TCnSVGRenderContext;
    FMatrixStack: array[0..63] of TCnSVGMatrix;
    FMatrixTop:  Integer;
    FStyleStack: array[0..63] of TCnSVGStyle;
    FStyleTop:   Integer;
    FDefsMap:    TStringList;
    FViewMatrix: TCnSVGMatrix;
    FBasePath:   string;
    FSavedDC:    Integer;
    procedure PushMatrix;
    procedure PopMatrix;
    procedure PushStyle;
    procedure PopStyle;
    procedure ApplyTransformAttr(const ATransform: string);
    procedure ApplyStyleAttr(AElement: TCnXMLElement);
    function CreateGDIPenHandle(AColor: TColor): HPEN;
    procedure SetupGDIPen;
    procedure SetupGDIBrush;
    function UserToScreen(X, Y: TCnSVGFloat): TPoint;
    function UserLenToScreen(Len: TCnSVGFloat): Integer;
    function EffectiveFillAlpha: TCnSVGFloat;
    function EffectiveStrokeAlpha: TCnSVGFloat;
    procedure BlendMaskBitmap(ADestCanvas: TCanvas; const ABounds: TRect;
      AMaskBmp: TBitmap; SrcColor: TColor; Alpha: TCnSVGFloat);
    procedure AlphaBlendPixel(ACanvas: TCanvas; X, Y: Integer;
      SrcColor: TColor; Alpha: TCnSVGFloat);
    procedure FillWithAlpha(ACanvas: TCanvas; const Pts: array of TPoint;
      FillColor: TCnSVGColor; Alpha: TCnSVGFloat);
    procedure FillPolyPolygonWithAlpha(ACanvas: TCanvas;
      const Pts: array of TPoint; PolyCounts: array of Integer;
      PolyCount: Integer; FillColor: TCnSVGColor; Alpha: TCnSVGFloat;
      FillRule: TCnSVGFillRule);
    procedure StrokePolyPointsWithAlpha(ACanvas: TCanvas;
      const Pts: array of TPoint; const PolyCounts: array of Integer;
      PolyCount: Integer; StrokeColor: TCnSVGColor; Alpha: TCnSVGFloat);
    procedure RenderRectAlpha(const Bounds: TRect; RX, RY: Integer);
    procedure RenderEllipseAlpha(const Bounds: TRect);
    procedure RenderRect(AElement: TCnXMLElement);
    procedure RenderCircle(AElement: TCnXMLElement);
    procedure RenderEllipse(AElement: TCnXMLElement);
    procedure RenderLine(AElement: TCnXMLElement);
    procedure RenderPolyline(AElement: TCnXMLElement);
    procedure RenderPolygon(AElement: TCnXMLElement);
    procedure RenderPath(AElement: TCnXMLElement);
    procedure RenderText(AElement: TCnXMLElement);
    procedure RenderGroup(AElement: TCnXMLElement);
    procedure RenderUse(AElement: TCnXMLElement);
    procedure RenderDefs(AElement: TCnXMLElement);
    procedure RenderImage(AElement: TCnXMLElement);
    procedure RenderSwitch(AElement: TCnXMLElement);
    procedure RenderAnchor(AElement: TCnXMLElement);
    procedure RenderNestedSVG(AElement: TCnXMLElement);
    procedure RenderElement(AElement: TCnXMLElement);
  public
    constructor Create(ACanvas: TCanvas; const ADestRect: TRect;
      AViewBox: TCnSVGRect; ADefsMap: TStringList; const ABasePath: string;
      const APreserveAspectRatio: string);
    destructor Destroy; override;
    procedure RenderNode(ANode: TCnXMLNode);
  end;

//==============================================================================
// 矩阵运算实现
//==============================================================================

procedure SVGMatrixIdentity(var M: TCnSVGMatrix);
begin
  M.a := 1; M.b := 0; M.c := 0;
  M.d := 1; M.e := 0; M.f := 0;
end;

procedure SVGMatrixMultiply(var Result: TCnSVGMatrix;
  const A, B: TCnSVGMatrix);
var
  Tmp: TCnSVGMatrix;
begin
  // 使用局部 Tmp 避免 Result 与 A 或 B 别名时的写后读问题
  Tmp.a := A.a * B.a + A.c * B.b;
  Tmp.b := A.b * B.a + A.d * B.b;
  Tmp.c := A.a * B.c + A.c * B.d;
  Tmp.d := A.b * B.c + A.d * B.d;
  Tmp.e := A.a * B.e + A.c * B.f + A.e;
  Tmp.f := A.b * B.e + A.d * B.f + A.f;
  Result := Tmp;
end;

procedure SVGMatrixTranslate(var M: TCnSVGMatrix;
  TX, TY: TCnSVGFloat);
var
  T, R: TCnSVGMatrix;
begin
  SVGMatrixIdentity(T);
  T.e := TX;
  T.f := TY;
  SVGMatrixMultiply(R, M, T);
  M := R;
end;

procedure SVGMatrixScale(var M: TCnSVGMatrix;
  SX, SY: TCnSVGFloat);
var
  S, R: TCnSVGMatrix;
begin
  SVGMatrixIdentity(S);
  S.a := SX;
  S.d := SY;
  SVGMatrixMultiply(R, M, S);
  M := R;
end;

procedure SVGMatrixRotate(var M: TCnSVGMatrix;
  AngleDeg, CX, CY: TCnSVGFloat);
var
  Rad, CosA, SinA: Extended;
  RM, R: TCnSVGMatrix;
begin
  Rad  := AngleDeg * Pi / 180;
  CosA := Cos(Rad);
  SinA := Sin(Rad);
  SVGMatrixIdentity(RM);
  RM.a := CosA;
  RM.b := SinA;
  RM.c := -SinA;
  RM.d := CosA;
  RM.e := CX - CX * CosA + CY * SinA;
  RM.f := CY - CX * SinA - CY * CosA;
  SVGMatrixMultiply(R, M, RM);
  M := R;
end;

procedure SVGMatrixSkewX(var M: TCnSVGMatrix;
  AngleDeg: TCnSVGFloat);
var
  Rad: Extended;
  SK, R: TCnSVGMatrix;
begin
  Rad := AngleDeg * Pi / 180;
  SVGMatrixIdentity(SK);
  SK.c := Tan(Rad);
  SVGMatrixMultiply(R, M, SK);
  M := R;
end;

procedure SVGMatrixSkewY(var M: TCnSVGMatrix;
  AngleDeg: TCnSVGFloat);
var
  Rad: Extended;
  SK, R: TCnSVGMatrix;
begin
  Rad := AngleDeg * Pi / 180;
  SVGMatrixIdentity(SK);
  SK.b := Tan(Rad);
  SVGMatrixMultiply(R, M, SK);
  M := R;
end;

procedure SVGMatrixTransformPoint(const M: TCnSVGMatrix;
  var X, Y: TCnSVGFloat);
var
  NewX, NewY: TCnSVGFloat;
begin
  NewX := M.a * X + M.c * Y + M.e;
  NewY := M.b * X + M.d * Y + M.f;
  X := NewX;
  Y := NewY;
end;

function SVGMatrixInverse(const M: TCnSVGMatrix;
  var Inv: TCnSVGMatrix): Boolean;
var
  Det: Extended;
begin
  Det := M.a * M.d - M.b * M.c;
  if Abs(Det) < 1e-10 then
  begin
    Result := False;
    Exit;
  end;
  Inv.a :=  M.d / Det;
  Inv.b := -M.b / Det;
  Inv.c := -M.c / Det;
  Inv.d :=  M.a / Det;
  Inv.e := (M.c * M.f - M.d * M.e) / Det;
  Inv.f := (M.b * M.e - M.a * M.f) / Det;
  Result := True;
end;

//==============================================================================
// 颜色解析工具函数
//==============================================================================

// SVGParseColor — 见下方完整实现（HexToByte/ClampByte 定义于前）

function HexToByte(const S: string): Byte;
{* 将两字符十六进制字符串（如 'FF'）转换为 Byte（0..255）}
var
  Hi, Lo: Integer;
  Ch: Char;
begin
  // High nibble
  Ch := UpCase(S[1]);
  if Ch in ['0'..'9'] then
    Hi := Ord(Ch) - Ord('0')
  else if Ch in ['A'..'F'] then
    Hi := Ord(Ch) - Ord('A') + 10
  else
    Hi := 0;
  // Low nibble
  Ch := UpCase(S[2]);
  if Ch in ['0'..'9'] then
    Lo := Ord(Ch) - Ord('0')
  else if Ch in ['A'..'F'] then
    Lo := Ord(Ch) - Ord('A') + 10
  else
    Lo := 0;
  Result := Hi * 16 + Lo;
end;

function ClampByte(V: Integer): Byte;
{* 将整数截断到 [0, 255] 范围返回 Byte}
begin
  if V < 0 then
    Result := 0
  else if V > 255 then
    Result := 255
  else
    Result := V;
end;

function SVGParseColor(const S: string; var Color: TCnSVGColor): Boolean;
{* 解析 SVG 颜色字符串，支持 none / #RRGGBB / #RGB / rgb(r,g,b) / 命名颜色。
   返回 False 表示 none 或解析失败，返回 True 表示解析成功（颜色写入 Color）。}
var
  Trimmed, Lower: string;
  R2, G2, B2: string;
  RgbStr: string;
  Parts: TStringList;
  Val: Double;
  IntVal: Integer;
  I: Integer;
  PartStr: string;
  IsPercent: Boolean;
begin
  Trimmed := Trim(S);
  if Trimmed = '' then
  begin
    // 空字符串 → 回退 black
    Color.R := 0; Color.G := 0; Color.B := 0;
    Result := True;
    Exit;
  end;

  Lower := LowerCase(Trimmed);

  // 1. none → return False
  if Lower = 'none' then
  begin
    Result := False;
    Exit;
  end;

  // 2. #RRGGBB 或 #RGB
  if (Length(Trimmed) > 0) and (Trimmed[1] = '#') then
  begin
    if Length(Trimmed) = 7 then
    begin
      // #RRGGBB
      Color.R := HexToByte(Copy(Trimmed, 2, 2));
      Color.G := HexToByte(Copy(Trimmed, 4, 2));
      Color.B := HexToByte(Copy(Trimmed, 6, 2));
      Result := True;
    end
    else if Length(Trimmed) = 4 then
    begin
      // #RGB → expand each nibble DD
      R2 := Trimmed[2] + Trimmed[2];
      G2 := Trimmed[3] + Trimmed[3];
      B2 := Trimmed[4] + Trimmed[4];
      Color.R := HexToByte(R2);
      Color.G := HexToByte(G2);
      Color.B := HexToByte(B2);
      Result := True;
    end
    else
    begin
      // 不合法的 # 格式 → 回退 black
      Color.R := 0; Color.G := 0; Color.B := 0;
      Result := True;
    end;
    Exit;
  end;

  // 3. rgb(r,g,b)
  if (Length(Lower) > 4) and (Copy(Lower, 1, 4) = 'rgb(') and
     (Lower[Length(Lower)] = ')') then
  begin
    // 提取括号内内容
    RgbStr := Copy(Trimmed, 5, Length(Trimmed) - 5);
    Parts  := TStringList.Create;
    try
      // 将逗号替换为换行符以便拆分
      for I := 1 to Length(RgbStr) do
        if RgbStr[I] = ',' then
          RgbStr[I] := #10;
      Parts.Text := RgbStr;
      if Parts.Count >= 3 then
      begin
        // 解析三个分量
        Color.R := 0; Color.G := 0; Color.B := 0;
        for I := 0 to 2 do
        begin
          PartStr := Trim(Parts[I]);
          IsPercent := (Length(PartStr) > 0) and (PartStr[Length(PartStr)] = '%');
          if IsPercent then
            PartStr := Trim(Copy(PartStr, 1, Length(PartStr) - 1));
          try
            if IsPercent then
            begin
              Val    := StrToFloat(PartStr);
              IntVal := Round(Val * 255.0 / 100.0);
            end
            else
              IntVal := StrToInt(PartStr);
          except
            IntVal := 0;
          end;
          case I of
            0: Color.R := ClampByte(IntVal);
            1: Color.G := ClampByte(IntVal);
            2: Color.B := ClampByte(IntVal);
          end;
        end;
        Result := True;
      end
      else
      begin
        Color.R := 0; Color.G := 0; Color.B := 0;
        Result := True;
      end;
    finally
      Parts.Free;
    end;
    Exit;
  end;

  // 4. 命名颜色表（CSS Color Level 3）
  Result := True; // 默认为 True，未知颜色回退 black
  if Lower = 'black' then begin Color.R:=0;   Color.G:=0;   Color.B:=0;   end
  else if Lower = 'white' then begin Color.R:=255; Color.G:=255; Color.B:=255; end
  else if Lower = 'red' then begin Color.R:=255; Color.G:=0;   Color.B:=0;   end
  else if Lower = 'green' then begin Color.R:=0;   Color.G:=128; Color.B:=0;   end
  else if Lower = 'blue' then begin Color.R:=0;   Color.G:=0;   Color.B:=255; end
  else if Lower = 'yellow' then begin Color.R:=255; Color.G:=255; Color.B:=0;   end
  else if Lower = 'cyan' then begin Color.R:=0;   Color.G:=255; Color.B:=255; end
  else if Lower = 'magenta' then begin Color.R:=255; Color.G:=0;   Color.B:=255; end
  else if Lower = 'maroon' then begin Color.R:=128; Color.G:=0;   Color.B:=0;   end
  else if Lower = 'navy' then begin Color.R:=0;   Color.G:=0;   Color.B:=128; end
  else if Lower = 'olive' then begin Color.R:=128; Color.G:=128; Color.B:=0;   end
  else if Lower = 'purple' then begin Color.R:=128; Color.G:=0;   Color.B:=128; end
  else if Lower = 'teal' then begin Color.R:=0;   Color.G:=128; Color.B:=128; end
  else if Lower = 'gray' then begin Color.R:=128; Color.G:=128; Color.B:=128; end
  else if Lower = 'grey' then begin Color.R:=128; Color.G:=128; Color.B:=128; end
  else if Lower = 'silver' then begin Color.R:=192; Color.G:=192; Color.B:=192; end
  else if Lower = 'lime' then begin Color.R:=0;   Color.G:=255; Color.B:=0;   end
  else if Lower = 'orange' then begin Color.R:=255; Color.G:=165; Color.B:=0;   end
  else if Lower = 'pink' then begin Color.R:=255; Color.G:=192; Color.B:=203; end
  else if Lower = 'brown' then begin Color.R:=165; Color.G:=42;  Color.B:=42;  end
  else if Lower = 'coral' then begin Color.R:=255; Color.G:=127; Color.B:=80;  end
  else if Lower = 'salmon' then begin Color.R:=250; Color.G:=128; Color.B:=114; end
  else if Lower = 'gold' then begin Color.R:=255; Color.G:=215; Color.B:=0;   end
  else if Lower = 'khaki' then begin Color.R:=240; Color.G:=230; Color.B:=140; end
  else if Lower = 'violet' then begin Color.R:=238; Color.G:=130; Color.B:=238; end
  else if Lower = 'indigo' then begin Color.R:=75;  Color.G:=0;   Color.B:=130; end
  else if Lower = 'turquoise' then begin Color.R:=64;  Color.G:=224; Color.B:=208; end
  else if Lower = 'tan' then begin Color.R:=210; Color.G:=180; Color.B:=140; end
  else if Lower = 'chocolate' then begin Color.R:=210; Color.G:=105; Color.B:=30;  end
  else if Lower = 'crimson' then begin Color.R:=220; Color.G:=20;  Color.B:=60;  end
  else if Lower = 'fuchsia' then begin Color.R:=255; Color.G:=0;   Color.B:=255; end
  else if Lower = 'lavender' then begin Color.R:=230; Color.G:=230; Color.B:=250; end
  else if Lower = 'linen' then begin Color.R:=250; Color.G:=240; Color.B:=230; end
  else if Lower = 'beige' then begin Color.R:=245; Color.G:=245; Color.B:=220; end
  else if Lower = 'ivory' then begin Color.R:=255; Color.G:=255; Color.B:=240; end
  else if Lower = 'azure' then begin Color.R:=240; Color.G:=255; Color.B:=255; end
  else if Lower = 'aqua' then begin Color.R:=0;   Color.G:=255; Color.B:=255; end
  else if Lower = 'aquamarine' then begin Color.R:=127; Color.G:=255; Color.B:=212; end
  else if Lower = 'bisque' then begin Color.R:=255; Color.G:=228; Color.B:=196; end
  else if Lower = 'blanchedalmond' then begin Color.R:=255; Color.G:=235; Color.B:=205; end
  else if Lower = 'cadetblue' then begin Color.R:=95;  Color.G:=158; Color.B:=160; end
  else if Lower = 'chartreuse' then begin Color.R:=127; Color.G:=255; Color.B:=0;   end
  else if Lower = 'cornflowerblue' then begin Color.R:=100; Color.G:=149; Color.B:=237; end
  else if Lower = 'cornsilk' then begin Color.R:=255; Color.G:=248; Color.B:=220; end
  else if Lower = 'darkblue' then begin Color.R:=0;   Color.G:=0;   Color.B:=139; end
  else if Lower = 'darkcyan' then begin Color.R:=0;   Color.G:=139; Color.B:=139; end
  else if Lower = 'darkgray' then begin Color.R:=169; Color.G:=169; Color.B:=169; end
  else if Lower = 'darkgreen' then begin Color.R:=0;   Color.G:=100; Color.B:=0;   end
  else if Lower = 'darkmagenta' then begin Color.R:=139; Color.G:=0;   Color.B:=139; end
  else if Lower = 'darkorange' then begin Color.R:=255; Color.G:=140; Color.B:=0;   end
  else if Lower = 'darkorchid' then begin Color.R:=153; Color.G:=50;  Color.B:=204; end
  else if Lower = 'darkred' then begin Color.R:=139; Color.G:=0;   Color.B:=0;   end
  else if Lower = 'darkslateblue' then begin Color.R:=72;  Color.G:=61;  Color.B:=139; end
  else if Lower = 'darkviolet' then begin Color.R:=148; Color.G:=0;   Color.B:=211; end
  else if Lower = 'deeppink' then begin Color.R:=255; Color.G:=20;  Color.B:=147; end
  else if Lower = 'deepskyblue' then begin Color.R:=0;   Color.G:=191; Color.B:=255; end
  else if Lower = 'dimgray' then begin Color.R:=105; Color.G:=105; Color.B:=105; end
  else if Lower = 'dodgerblue' then begin Color.R:=30;  Color.G:=144; Color.B:=255; end
  else if Lower = 'firebrick' then begin Color.R:=178; Color.G:=34;  Color.B:=34;  end
  else if Lower = 'forestgreen' then begin Color.R:=34;  Color.G:=139; Color.B:=34;  end
  else if Lower = 'gainsboro' then begin Color.R:=220; Color.G:=220; Color.B:=220; end
  else if Lower = 'ghostwhite' then begin Color.R:=248; Color.G:=248; Color.B:=255; end
  else if Lower = 'goldenrod' then begin Color.R:=218; Color.G:=165; Color.B:=32;  end
  else if Lower = 'greenyellow' then begin Color.R:=173; Color.G:=255; Color.B:=47;  end
  else if Lower = 'hotpink' then begin Color.R:=255; Color.G:=105; Color.B:=180; end
  else if Lower = 'indianred' then begin Color.R:=205; Color.G:=92;  Color.B:=92;  end
  else if Lower = 'lightblue' then begin Color.R:=173; Color.G:=216; Color.B:=230; end
  else if Lower = 'lightcoral' then begin Color.R:=240; Color.G:=128; Color.B:=128; end
  else if Lower = 'lightcyan' then begin Color.R:=224; Color.G:=255; Color.B:=255; end
  else if Lower = 'lightgray' then begin Color.R:=211; Color.G:=211; Color.B:=211; end
  else if Lower = 'lightgreen' then begin Color.R:=144; Color.G:=238; Color.B:=144; end
  else if Lower = 'lightyellow' then begin Color.R:=255; Color.G:=255; Color.B:=224; end
  else if Lower = 'mediumblue' then begin Color.R:=0;   Color.G:=0;   Color.B:=205; end
  else if Lower = 'mediumorchid' then begin Color.R:=186; Color.G:=85;  Color.B:=211; end
  else if Lower = 'mediumpurple' then begin Color.R:=147; Color.G:=112; Color.B:=219; end
  else if Lower = 'mediumseagreen' then begin Color.R:=60;  Color.G:=179; Color.B:=113; end
  else if Lower = 'mediumturquoise' then begin Color.R:=72;  Color.G:=209; Color.B:=204; end
  else if Lower = 'midnightblue' then begin Color.R:=25;  Color.G:=25;  Color.B:=112; end
  else if Lower = 'mistyrose' then begin Color.R:=255; Color.G:=228; Color.B:=225; end
  else if Lower = 'moccasin' then begin Color.R:=255; Color.G:=228; Color.B:=181; end
  else if Lower = 'navajowhite' then begin Color.R:=255; Color.G:=222; Color.B:=173; end
  else if Lower = 'oldlace' then begin Color.R:=253; Color.G:=245; Color.B:=230; end
  else if Lower = 'olivedrab' then begin Color.R:=107; Color.G:=142; Color.B:=35;  end
  else if Lower = 'orangered' then begin Color.R:=255; Color.G:=69;  Color.B:=0;   end
  else if Lower = 'orchid' then begin Color.R:=218; Color.G:=112; Color.B:=214; end
  else if Lower = 'palegoldenrod' then begin Color.R:=238; Color.G:=232; Color.B:=170; end
  else if Lower = 'palegreen' then begin Color.R:=152; Color.G:=251; Color.B:=152; end
  else if Lower = 'paleturquoise' then begin Color.R:=175; Color.G:=238; Color.B:=238; end
  else if Lower = 'palevioletred' then begin Color.R:=219; Color.G:=112; Color.B:=147; end
  else if Lower = 'peachpuff' then begin Color.R:=255; Color.G:=218; Color.B:=185; end
  else if Lower = 'peru' then begin Color.R:=205; Color.G:=133; Color.B:=63;  end
  else if Lower = 'plum' then begin Color.R:=221; Color.G:=160; Color.B:=221; end
  else if Lower = 'powderblue' then begin Color.R:=176; Color.G:=224; Color.B:=230; end
  else if Lower = 'rosybrown' then begin Color.R:=188; Color.G:=143; Color.B:=143; end
  else if Lower = 'royalblue' then begin Color.R:=65;  Color.G:=105; Color.B:=225; end
  else if Lower = 'saddlebrown' then begin Color.R:=139; Color.G:=69;  Color.B:=19;  end
  else if Lower = 'sandybrown' then begin Color.R:=244; Color.G:=164; Color.B:=96;  end
  else if Lower = 'seagreen' then begin Color.R:=46;  Color.G:=139; Color.B:=87;  end
  else if Lower = 'seashell' then begin Color.R:=255; Color.G:=245; Color.B:=238; end
  else if Lower = 'sienna' then begin Color.R:=160; Color.G:=82;  Color.B:=45;  end
  else if Lower = 'skyblue' then begin Color.R:=135; Color.G:=206; Color.B:=235; end
  else if Lower = 'slateblue' then begin Color.R:=106; Color.G:=90;  Color.B:=205; end
  else if Lower = 'slategray' then begin Color.R:=112; Color.G:=128; Color.B:=144; end
  else if Lower = 'springgreen' then begin Color.R:=0;   Color.G:=255; Color.B:=127; end
  else if Lower = 'steelblue' then begin Color.R:=70;  Color.G:=130; Color.B:=180; end
  else if Lower = 'thistle' then begin Color.R:=216; Color.G:=191; Color.B:=216; end
  else if Lower = 'tomato' then begin Color.R:=255; Color.G:=99;  Color.B:=71;  end
  else if Lower = 'wheat' then begin Color.R:=245; Color.G:=222; Color.B:=179; end
  else if Lower = 'whitesmoke' then begin Color.R:=245; Color.G:=245; Color.B:=245; end
  else if Lower = 'yellowgreen' then begin Color.R:=154; Color.G:=205; Color.B:=50;  end
  else
  begin
    // 未知颜色 → 回退 black
    Color.R := 0; Color.G := 0; Color.B := 0;
  end;
end;

//==============================================================================
// 浮点属性读取与样式继承工具函数
//==============================================================================

function SVGAttrFloat(El: TCnXMLElement; const Name: string;
  Default: TCnSVGFloat): TCnSVGFloat;
{* 从 XML 元素读取指定属性的浮点值。解析失败或属性不存在时返回 Default，不抛异常。
   支持 px 后缀（会被去掉）。 }
var
  S: string;
begin
  Result := Default;
  if El = nil then Exit;
  if not El.HasAttribute(Name) then Exit;
  S := Trim(El.GetAttribute(Name));
  if S = '' then Exit;
  // strip 'px' suffix
  if (Length(S) >= 2) and (LowerCase(Copy(S, Length(S) - 1, 2)) = 'px') then
    S := Trim(Copy(S, 1, Length(S) - 2));
  try
    Result := StrToFloat(S);
  except
    Result := Default;
  end;
end;

procedure SVGDefaultStyle(var S: TCnSVGStyle);
{* 按 SVG 规范初始化样式记录为默认值：fill=black, stroke=none, stroke-width=1,
   opacity=1, font-size=16, stroke-miterlimit=4, color=black。
   注意：不能对包含 string 字段的记录使用 FillChar；字段逐一赋值。 }
begin
  // fill
  S.FillColor.R := 0; S.FillColor.G := 0; S.FillColor.B := 0;
  S.FillNone    := False;
  S.FillOpacity := 1.0;
  S.FillRule    := sfrNonZero;
  // stroke
  S.StrokeColor.R := 0; S.StrokeColor.G := 0; S.StrokeColor.B := 0;
  S.StrokeNone    := True;   // SVG default: no stroke
  S.StrokeOpacity := 1.0;
  S.StrokeWidth   := 1.0;
  S.LineCap       := slcButt;
  S.LineJoin      := sljMiter;
  S.MiterLimit    := 4.0;
  S.DashCount     := 0;
  S.DashOffset    := 0;
  // 清空 dasharray
  FillChar(S.DashArray, SizeOf(S.DashArray), 0);
  // opacity
  S.Opacity := 1.0;
  // font
  S.FontSize   := 16.0;
  S.FontFamily := '';
  S.FontBold   := False;
  S.FontItalic := False;
  S.TextAnchor := staStart;
  // visibility
  S.DisplayNone      := False;
  S.VisibilityHidden := False;
  // currentColor
  S.CurrentColor.R := 0; S.CurrentColor.G := 0; S.CurrentColor.B := 0;
  S.CurrentColorSet := False;
end;

function SVGClampOpacity(V: TCnSVGFloat): TCnSVGFloat;
{* 将透明度值截断至 [0.0, 1.0] 范围。 }
begin
  if V < 0 then
    Result := 0
  else if V > 1 then
    Result := 1
  else
    Result := V;
end;

function SVGParseMiterLimit(const S: string): TCnSVGFloat;
{* 解析 stroke-miterlimit 字符串，值 < 1.0 截断为 1.0，解析失败返回默认值 4.0。 }
var
  V: Extended;
begin
  Result := 4.0; // default
  try
    V := StrToFloat(Trim(S));
    if V < 1.0 then
      Result := 1.0
    else
      Result := V;
  except
    // keep default 4.0
  end;
end;

procedure SVGParseStyleAttr(El: TCnXMLElement; var Style: TCnSVGStyle;
  const ParentStyle: TCnSVGStyle);
{* 解析元素的 XML 属性和 style="" 内联样式，后者覆盖前者（CSS 优先级规则）。
   支持 inherit 关键字（强制继承父级对应字段值）和 currentColor 关键字。
   未能解析的属性静默忽略，不抛异常。 }
var
  Attrs: TStringList;
  I, J, EqPos: Integer;
  StyleStr, Part, Key, Val, LKey, LVal: string;
  C: TCnSVGColor;
  F: Extended;
  SWStr, SWTrimmed: string;
  DashStr: string;
  DashParts: TStringList;
  DashIdx: Integer;
  DashVal: Extended;
begin
  if El = nil then Exit;

  // Build attribute map: Key (lowercase) → Value
  // Use a TStringList with Name=Value pairs (separator '=')
  Attrs := TStringList.Create;
  try
    // Step 1: collect XML presentation attributes (lowercase key)
    for I := 0 to El.AttributeCount - 1 do
    begin
      Key := LowerCase(El.AttributeNames[I]);
      Val := El.AttributeValues[I];
      // Use Name=Value format; replace any existing key
      EqPos := Attrs.IndexOfName(Key);
      if EqPos >= 0 then
        Attrs[EqPos] := Key + '=' + Val
      else
        Attrs.Add(Key + '=' + Val);
    end;

    // Step 2: parse style="" and overwrite presentation attributes
    if El.HasAttribute('style') then
    begin
      StyleStr := El.GetAttribute('style');
      // split by ';'
      I := 1;
      while I <= Length(StyleStr) do
      begin
        // find next ';'
        J := I;
        while (J <= Length(StyleStr)) and (StyleStr[J] <> ';') do
          Inc(J);
        Part := Trim(Copy(StyleStr, I, J - I));
        I := J + 1;
        if Part = '' then Continue;

        // split on first ':'
        EqPos := Pos(':', Part);
        if EqPos < 1 then Continue;
        Key := LowerCase(Trim(Copy(Part, 1, EqPos - 1)));
        Val := Trim(Copy(Part, EqPos + 1, Length(Part) - EqPos));
        if Key = '' then Continue;

        // overwrite or add
        J := Attrs.IndexOfName(Key);
        if J >= 0 then
          Attrs[J] := Key + '=' + Val
        else
          Attrs.Add(Key + '=' + Val);
      end;
    end;

    // Step 3: process each key-value pair
    for I := 0 to Attrs.Count - 1 do
    begin
      LKey := Attrs.Names[I];
      Val  := Attrs.Values[LKey];
      LVal := LowerCase(Trim(Val));
      Val  := Trim(Val);

      if LKey = 'color' then
      begin
        if LVal = 'inherit' then
        begin
          Style.CurrentColor    := ParentStyle.CurrentColor;
          Style.CurrentColorSet := ParentStyle.CurrentColorSet;
        end
        else if SVGParseColor(Val, C) then
        begin
          Style.CurrentColor    := C;
          Style.CurrentColorSet := True;
        end;
      end

      else if LKey = 'display' then
      begin
        if LVal = 'none' then
          Style.DisplayNone := True;
      end

      else if LKey = 'visibility' then
      begin
        if (LVal = 'hidden') or (LVal = 'collapse') then
          Style.VisibilityHidden := True;
      end

      else if LKey = 'opacity' then
      begin
        if LVal = 'inherit' then
          Style.Opacity := ParentStyle.Opacity
        else
          try
            Style.Opacity := SVGClampOpacity(StrToFloat(Val));
          except
          end;
      end

      else if LKey = 'fill' then
      begin
        if LVal = 'inherit' then
        begin
          Style.FillColor   := ParentStyle.FillColor;
          Style.FillNone    := ParentStyle.FillNone;
          Style.FillOpacity := ParentStyle.FillOpacity;
          Style.FillRule    := ParentStyle.FillRule;
        end
        else if LVal = 'none' then
          Style.FillNone := True
        else if LVal = 'currentcolor' then
        begin
          Style.FillColor := Style.CurrentColor;
          Style.FillNone  := False;
        end
        else if SVGParseColor(Val, C) then
        begin
          Style.FillColor := C;
          Style.FillNone  := False;
        end;
      end

      else if LKey = 'fill-opacity' then
      begin
        if LVal = 'inherit' then
          Style.FillOpacity := ParentStyle.FillOpacity
        else
          try
            Style.FillOpacity := SVGClampOpacity(StrToFloat(Val));
          except
          end;
      end

      else if LKey = 'fill-rule' then
      begin
        if LVal = 'inherit' then
          Style.FillRule := ParentStyle.FillRule
        else if LVal = 'evenodd' then
          Style.FillRule := sfrEvenOdd
        else
          Style.FillRule := sfrNonZero;
      end

      else if LKey = 'stroke' then
      begin
        if LVal = 'inherit' then
        begin
          Style.StrokeColor   := ParentStyle.StrokeColor;
          Style.StrokeNone    := ParentStyle.StrokeNone;
          Style.StrokeOpacity := ParentStyle.StrokeOpacity;
          Style.StrokeWidth   := ParentStyle.StrokeWidth;
        end
        else if LVal = 'none' then
          Style.StrokeNone := True
        else if LVal = 'currentcolor' then
        begin
          Style.StrokeColor := Style.CurrentColor;
          Style.StrokeNone  := False;
        end
        else if SVGParseColor(Val, C) then
        begin
          Style.StrokeColor := C;
          Style.StrokeNone  := False;
        end;
      end

      else if LKey = 'stroke-width' then
      begin
        if LVal = 'inherit' then
          Style.StrokeWidth := ParentStyle.StrokeWidth
        else
        begin
          SWStr := Val;
          SWTrimmed := Trim(SWStr);
          if (Length(SWTrimmed) >= 2) and
             (LowerCase(Copy(SWTrimmed, Length(SWTrimmed) - 1, 2)) = 'px') then
            SWTrimmed := Trim(Copy(SWTrimmed, 1, Length(SWTrimmed) - 2));
          try
            F := StrToFloat(SWTrimmed);
            if F >= 0 then
              Style.StrokeWidth := F;
          except
          end;
        end;
      end

      else if LKey = 'stroke-opacity' then
      begin
        if LVal = 'inherit' then
          Style.StrokeOpacity := ParentStyle.StrokeOpacity
        else
          try
            Style.StrokeOpacity := SVGClampOpacity(StrToFloat(Val));
          except
          end;
      end

      else if LKey = 'stroke-linecap' then
      begin
        if LVal = 'inherit' then
          Style.LineCap := ParentStyle.LineCap
        else if LVal = 'round' then
          Style.LineCap := slcRound
        else if LVal = 'square' then
          Style.LineCap := slcSquare
        else
          Style.LineCap := slcButt;
      end

      else if LKey = 'stroke-linejoin' then
      begin
        if LVal = 'inherit' then
          Style.LineJoin := ParentStyle.LineJoin
        else if LVal = 'round' then
          Style.LineJoin := sljRound
        else if LVal = 'bevel' then
          Style.LineJoin := sljBevel
        else
          Style.LineJoin := sljMiter;
      end

      else if LKey = 'stroke-miterlimit' then
      begin
        if LVal = 'inherit' then
          Style.MiterLimit := ParentStyle.MiterLimit
        else
          Style.MiterLimit := SVGParseMiterLimit(Val);
      end

      else if LKey = 'stroke-dasharray' then
      begin
        if LVal = 'inherit' then
        begin
          Style.DashArray := ParentStyle.DashArray;
          Style.DashCount := ParentStyle.DashCount;
        end
        else if LVal = 'none' then
          Style.DashCount := 0
        else
        begin
          // parse floats separated by spaces/commas into DashArray (max 8)
          DashStr := Val;
          // replace commas with spaces
          for J := 1 to Length(DashStr) do
            if DashStr[J] = ',' then
              DashStr[J] := ' ';
          DashParts := TStringList.Create;
          try
            DashParts.Text := DashStr;
            DashIdx := 0;
            for J := 0 to DashParts.Count - 1 do
            begin
              if DashIdx >= 8 then Break;
              DashStr := Trim(DashParts[J]);
              if DashStr = '' then Continue;
              try
                DashVal := StrToFloat(DashStr);
                Style.DashArray[DashIdx] := DashVal;
                Inc(DashIdx);
              except
              end;
            end;
            Style.DashCount := DashIdx;
          finally
            DashParts.Free;
          end;
        end;
      end

      else if LKey = 'stroke-dashoffset' then
      begin
        if LVal = 'inherit' then
          Style.DashOffset := ParentStyle.DashOffset
        else
          try
            Style.DashOffset := StrToFloat(Val);
          except
          end;
      end

      else if LKey = 'font-size' then
      begin
        if LVal = 'inherit' then
          Style.FontSize := ParentStyle.FontSize
        else
        begin
          SWTrimmed := Val;
          if (Length(SWTrimmed) >= 2) and
             (LowerCase(Copy(SWTrimmed, Length(SWTrimmed) - 1, 2)) = 'px') then
            SWTrimmed := Trim(Copy(SWTrimmed, 1, Length(SWTrimmed) - 2));
          try
            F := StrToFloat(SWTrimmed);
            if F > 0 then
              Style.FontSize := F;
          except
          end;
        end;
      end

      else if LKey = 'font-family' then
      begin
        if LVal = 'inherit' then
          Style.FontFamily := ParentStyle.FontFamily
        else
          Style.FontFamily := Trim(Val);
      end

      else if LKey = 'font-weight' then
      begin
        if LVal = 'inherit' then
          Style.FontBold := ParentStyle.FontBold
        else
          Style.FontBold := (LVal = 'bold') or (LVal = 'bolder');
      end

      else if LKey = 'font-style' then
      begin
        if LVal = 'inherit' then
          Style.FontItalic := ParentStyle.FontItalic
        else
          Style.FontItalic := (LVal = 'italic') or (LVal = 'oblique');
      end

      else if LKey = 'text-anchor' then
      begin
        if LVal = 'inherit' then
          Style.TextAnchor := ParentStyle.TextAnchor
        else if LVal = 'middle' then
          Style.TextAnchor := staMiddle
        else if LVal = 'end' then
          Style.TextAnchor := staEnd
        else
          Style.TextAnchor := staStart;
      end;
    end; // for I

  finally
    Attrs.Free;
  end;
end;


//==============================================================================
// SVG 通用辅助函数
//==============================================================================

function SVGReadNumberToken(const S: string; var P: Integer;
  var Value: TCnSVGFloat): Boolean;
{* 从字符串当前位置读取一个浮点数 token，支持符号、小数和科学计数法。 }
var
  Start: Integer;
  HasDot, HasExp: Boolean;
  Ch: Char;
  Token: string;
begin
  Result := False;
  Value := 0;
  while (P <= Length(S)) and (S[P] in [' ', #9, #10, #13, ',']) do
    Inc(P);
  if P > Length(S) then
    Exit;

  Start := P;
  HasDot := False;
  HasExp := False;
  if S[P] in ['+', '-'] then
    Inc(P);

  while P <= Length(S) do
  begin
    Ch := S[P];
    if Ch in ['0'..'9'] then
      Inc(P)
    else if (Ch = '.') and (not HasDot) then
    begin
      HasDot := True;
      Inc(P);
    end
    else if (Ch in ['e', 'E']) and (not HasExp) then
    begin
      HasExp := True;
      Inc(P);
      if (P <= Length(S)) and (S[P] in ['+', '-']) then
        Inc(P);
    end
    else
      Break;
  end;

  Token := Trim(Copy(S, Start, P - Start));
  if Token = '' then
    Exit;
  try
    Value := StrToFloat(Token);
    Result := True;
  except
    Result := False;
  end;
end;

function SVGParseLengthValue(const S: string; Default: TCnSVGFloat): TCnSVGFloat;
{* 解析长度值，支持纯数值和带单位的数值；未知单位时仅取其数值前缀。 }
var
  P: Integer;
begin
  P := 1;
  if not SVGReadNumberToken(Trim(S), P, Result) then
    Result := Default;
end;

function SVGParseViewBoxValue(const S: string; var R: TCnSVGRect): Boolean;
{* 解析 viewBox="min-x min-y width height"。 }
var
  P: Integer;
  V: array[0..3] of TCnSVGFloat;
  I: Integer;
begin
  Result := False;
  P := 1;
  for I := 0 to 3 do
    if not SVGReadNumberToken(S, P, V[I]) then
      Exit;
  R.X := V[0];
  R.Y := V[1];
  R.Width := V[2];
  R.Height := V[3];
  Result := True;
end;

function SVGFindDefNode(ADefsMap: TStringList; const AID: string): TCnXMLElement;
{* 在定义字典中按区分大小写的 id 查找节点。 }
var
  I: Integer;
begin
  Result := nil;
  if (ADefsMap = nil) or (AID = '') then
    Exit;
  for I := 0 to ADefsMap.Count - 1 do
    if ADefsMap[I] = AID then
    begin
      Result := TCnXMLElement(ADefsMap.Objects[I]);
      Exit;
    end;
end;

procedure SVGParsePreserveAspectRatio(const S: string; var IsNone, IsSlice: Boolean;
  var AlignX, AlignY: Integer);
{* 解析 preserveAspectRatio。AlignX/AlignY: 0=min, 1=mid, 2=max。 }
var
  Lower: string;
begin
  Lower := LowerCase(Trim(S));
  IsNone := Lower = 'none';
  IsSlice := Pos('slice', Lower) > 0;
  AlignX := 1;
  AlignY := 1;
  if IsNone then
    Exit;
  if Pos('xmin', Lower) > 0 then
    AlignX := 0
  else if Pos('xmax', Lower) > 0 then
    AlignX := 2;
  if Pos('ymin', Lower) > 0 then
    AlignY := 0
  else if Pos('ymax', Lower) > 0 then
    AlignY := 2;
end;

procedure SVGCalcViewMatrix(const ADestRect: TRect; const AViewBox: TCnSVGRect;
  const APreserveAspectRatio: string; var AMatrix: TCnSVGMatrix;
  var NeedClip: Boolean);
{* 计算 viewBox 到目标矩形的仿射变换矩阵。 }
var
  IsNone, IsSlice: Boolean;
  AlignX, AlignY: Integer;
  SX, SY, S, TX, TY: Extended;
  DW, DH, VX, VY, VW, VH: Extended;
begin
  SVGParsePreserveAspectRatio(APreserveAspectRatio, IsNone, IsSlice, AlignX, AlignY);
  NeedClip := False;

  DW := ADestRect.Right - ADestRect.Left;
  DH := ADestRect.Bottom - ADestRect.Top;
  VX := AViewBox.X;
  VY := AViewBox.Y;
  VW := AViewBox.Width;
  VH := AViewBox.Height;
  if VW <= 0 then
    VW := 1;
  if VH <= 0 then
    VH := 1;

  SVGMatrixIdentity(AMatrix);
  if IsNone then
  begin
    SX := DW / VW;
    SY := DH / VH;
    TX := ADestRect.Left - VX * SX;
    TY := ADestRect.Top - VY * SY;
  end
  else
  begin
    if IsSlice then
    begin
      if DW / VW > DH / VH then
        S := DW / VW
      else
        S := DH / VH;
      NeedClip := True;
    end
    else
    begin
      if DW / VW < DH / VH then
        S := DW / VW
      else
        S := DH / VH;
    end;
    SX := S;
    SY := S;
    case AlignX of
      0: TX := ADestRect.Left - VX * S;
      2: TX := ADestRect.Left + (DW - VW * S) - VX * S;
    else
      TX := ADestRect.Left + (DW - VW * S) / 2 - VX * S;
    end;
    case AlignY of
      0: TY := ADestRect.Top - VY * S;
      2: TY := ADestRect.Top + (DH - VH * S) - VY * S;
    else
      TY := ADestRect.Top + (DH - VH * S) / 2 - VY * S;
    end;
  end;

  AMatrix.a := SX;
  AMatrix.d := SY;
  AMatrix.e := TX;
  AMatrix.f := TY;
end;

procedure SVGCalcNestedViewMatrix(ADestX, ADestY, ADestW, ADestH: TCnSVGFloat;
  const AViewBox: TCnSVGRect; const APreserveAspectRatio: string;
  var AMatrix: TCnSVGMatrix);
{* 计算局部用户坐标系中的子视口变换矩阵。 }
var
  IsNone, IsSlice: Boolean;
  AlignX, AlignY: Integer;
  SX, SY, S, TX, TY: Extended;
  VW, VH: Extended;
begin
  SVGParsePreserveAspectRatio(APreserveAspectRatio, IsNone, IsSlice, AlignX, AlignY);
  VW := AViewBox.Width;
  VH := AViewBox.Height;
  if VW <= 0 then
    VW := 1;
  if VH <= 0 then
    VH := 1;

  SVGMatrixIdentity(AMatrix);
  if IsNone then
  begin
    SX := ADestW / VW;
    SY := ADestH / VH;
    TX := ADestX - AViewBox.X * SX;
    TY := ADestY - AViewBox.Y * SY;
  end
  else
  begin
    if ADestW / VW < ADestH / VH then
      S := ADestW / VW
    else
      S := ADestH / VH;
    if IsSlice then
      if ADestW / VW > ADestH / VH then
        S := ADestW / VW
      else
        S := ADestH / VH;
    SX := S;
    SY := S;
    case AlignX of
      0: TX := ADestX - AViewBox.X * S;
      2: TX := ADestX + (ADestW - VW * S) - AViewBox.X * S;
    else
      TX := ADestX + (ADestW - VW * S) / 2 - AViewBox.X * S;
    end;
    case AlignY of
      0: TY := ADestY - AViewBox.Y * S;
      2: TY := ADestY + (ADestH - VH * S) - AViewBox.Y * S;
    else
      TY := ADestY + (ADestH - VH * S) / 2 - AViewBox.Y * S;
    end;
  end;
  AMatrix.a := SX;
  AMatrix.d := SY;
  AMatrix.e := TX;
  AMatrix.f := TY;
end;

function SVGCalcAlignedRect(const ABounds: TRect; ASrcWidth, ASrcHeight: Integer;
  const APreserveAspectRatio: string; var NeedClip: Boolean): TRect;
{* 依据 preserveAspectRatio 计算位图在目标矩形中的实际绘制区域。
   当为 slice 时返回超出 ABounds 的绘制区域，并通过 NeedClip 提示调用方设置裁剪。 }
var
  IsNone, IsSlice: Boolean;
  AlignX, AlignY: Integer;
  DW, DH, SW, SH, Scale, DrawW, DrawH: Double;
  OffX, OffY: Double;
begin
  Result := ABounds;
  NeedClip := False;
  if (ASrcWidth <= 0) or (ASrcHeight <= 0) then
    Exit;

  SVGParsePreserveAspectRatio(APreserveAspectRatio, IsNone, IsSlice, AlignX, AlignY);
  if IsNone then
    Exit;

  DW := ABounds.Right - ABounds.Left;
  DH := ABounds.Bottom - ABounds.Top;
  SW := ASrcWidth;
  SH := ASrcHeight;
  if (DW <= 0) or (DH <= 0) then
    Exit;

  if IsSlice then
  begin
    if DW / SW > DH / SH then
      Scale := DW / SW
    else
      Scale := DH / SH;
    NeedClip := True;
  end
  else
  begin
    if DW / SW < DH / SH then
      Scale := DW / SW
    else
      Scale := DH / SH;
  end;

  DrawW := SW * Scale;
  DrawH := SH * Scale;
  case AlignX of
    0: OffX := ABounds.Left;
    2: OffX := ABounds.Right - DrawW;
  else
    OffX := ABounds.Left + (DW - DrawW) / 2;
  end;
  case AlignY of
    0: OffY := ABounds.Top;
    2: OffY := ABounds.Bottom - DrawH;
  else
    OffY := ABounds.Top + (DH - DrawH) / 2;
  end;

  Result.Left := Round(OffX);
  Result.Top := Round(OffY);
  Result.Right := Round(OffX + DrawW);
  Result.Bottom := Round(OffY + DrawH);
end;

function SVGIsHttpLikeRef(const S: string): Boolean;
{* 判断是否为当前版本不支持的网络 URL。 }
var
  Lower: string;
begin
  Lower := LowerCase(Trim(S));
  Result := (Copy(Lower, 1, 7) = 'http://') or
    (Copy(Lower, 1, 8) = 'https://');
end;

function SVGResolveLocalFileName(const ABasePath, AHref: string): string;
{* 解析 image 本地文件名；不支持 URL 和 data URI。 }
var
  Href: string;
begin
  Result := '';
  Href := Trim(AHref);
  if Href = '' then
    Exit;
  if SVGIsHttpLikeRef(Href) then
    Exit;
  if LowerCase(Copy(Href, 1, 5)) = 'data:' then
    Exit;

  if (Pos(':\', Href) > 0) or ((Href <> '') and (Href[1] = '\')) or
    ((Href <> '') and (Href[1] = '/')) then
    Result := Href
  else if ABasePath <> '' then
    Result := ExpandFileName(ABasePath + Href);
end;


//==============================================================================
// TCnSVGPathParser 辅助函数
//==============================================================================

function NewPathSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
{* 分配并初始化一个新的路径段记录，调用方负责释放。 }
begin
  New(Result);
  FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
  Result^.SegType := SegType;
end;

//==============================================================================
// TCnSVGPathParser 实现
//==============================================================================

constructor TCnSVGPathParser.Create;
begin
  inherited Create;
  FPos  := 1;
  FCurX := 0;
  FCurY := 0;
  FLastCtlX := 0;
  FLastCtlY := 0;
  FLastCmd  := #0;
end;

destructor TCnSVGPathParser.Destroy;
begin
  inherited Destroy;
end;

procedure TCnSVGPathParser.SkipWS;
begin
  while FPos <= Length(FData) do
  begin
    if FData[FPos] in [#9, #10, #13, #32, ','] then
      Inc(FPos)
    else
      Break;
  end;
end;

function TCnSVGPathParser.ReadNumber(var Val: TCnSVGFloat): Boolean;
var
  Start: Integer;
  S: string;
  HasDot: Boolean;
  HasExp: Boolean;
  PrevCh, Ch: Char;
begin
  Result := False;
  Val    := 0;
  SkipWS;
  if FPos > Length(FData) then Exit;

  S      := '';
  HasDot := False;
  HasExp := False;
  PrevCh := #0;
  Start  := FPos;

  // Optional leading sign
  Ch := FData[FPos];
  if Ch in ['+', '-'] then
  begin
    S := Ch;
    Inc(FPos);
    PrevCh := Ch;
  end;

  while FPos <= Length(FData) do
  begin
    Ch := FData[FPos];
    if Ch in ['0'..'9'] then
    begin
      S := S + Ch;
      PrevCh := Ch;
      Inc(FPos);
    end
    else if (Ch = '.') and not HasDot and not HasExp then
    begin
      HasDot := True;
      S := S + Ch;
      PrevCh := Ch;
      Inc(FPos);
    end
    else if (Ch in ['e', 'E']) and not HasExp and (Length(S) > 0) then
    begin
      HasExp := True;
      S := S + Ch;
      PrevCh := Ch;
      Inc(FPos);
      // Optional sign after exponent
      if FPos <= Length(FData) then
      begin
        Ch := FData[FPos];
        if Ch in ['+', '-'] then
        begin
          S := S + Ch;
          PrevCh := Ch;
          Inc(FPos);
        end;
      end;
    end
    else if (Ch in ['+', '-']) and (PrevCh in ['e', 'E']) then
    begin
      // sign immediately after e/E — already handled above; shouldn't reach here
      S := S + Ch;
      PrevCh := Ch;
      Inc(FPos);
    end
    else
      Break; // implicit separator or unknown char
  end;

  // Try to convert
  if Length(S) = 0 then
  begin
    FPos := Start;
    Exit;
  end;
  // Remove lone sign with no digits
  if (S = '+') or (S = '-') then
  begin
    FPos := Start;
    Exit;
  end;

  try
    Val    := StrToFloat(S);
    Result := True;
  except
    FPos := Start;
  end;
end;

function TCnSVGPathParser.PeekCmd: Char;
begin
  if FPos <= Length(FData) then
  begin
    Result := FData[FPos];
    if not (Result in ['a'..'z', 'A'..'Z']) then
      Result := #0;
  end
  else
    Result := #0;
end;

procedure TCnSVGPathParser.ParseMoveTo(Relative: Boolean; List: TList);
{* 解析 M/m 命令：读取坐标对序列，首对为 MoveTo，后续对隐式视为 LineTo。
   完成后将 FLastCmd 设为 'L' 或 'l'，以便 ParsePathData 的隐式重复逻辑
   在下一轮循环中调用 ParseLineTo 处理多余坐标对。 }
var
  X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;
  IsFirst: Boolean;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  IsFirst := True;
  while ReadNumber(X) do
  begin
    if not ReadNumber(Y) then Break;

    if Relative then
    begin
      X := X + FCurX;
      Y := Y + FCurY;
    end;

    if IsFirst then
    begin
      Seg := NewSeg(pstMoveTo);
      IsFirst := False;
    end
    else
    begin
      // 同一 M 命令的后续坐标对视为 LineTo（SVG 规范）
      Seg := NewSeg(pstLineTo);
    end;

    Seg^.X := X;
    Seg^.Y := Y;
    List.Add(Seg);

    FCurX     := X;
    FCurY     := Y;
    FLastCtlX := FCurX;
    FLastCtlY := FCurY;

    // 后续隐式重复按 L/l 处理
    if Relative then
      FLastCmd := 'l'
    else
      FLastCmd := 'L';
  end;
end;

procedure TCnSVGPathParser.ParseLineTo(Relative: Boolean; List: TList);
{* 解析 L/l 命令：读取坐标对序列，每对生成一个 LineTo 段（绝对坐标）。 }
var
  X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(X) do
  begin
    if not ReadNumber(Y) then Break;

    if Relative then
    begin
      X := X + FCurX;
      Y := Y + FCurY;
    end;

    Seg := NewSeg(pstLineTo);
    Seg^.X := X;
    Seg^.Y := Y;
    List.Add(Seg);

    FCurX     := X;
    FCurY     := Y;
    FLastCtlX := FCurX;
    FLastCtlY := FCurY;
  end;
end;

procedure TCnSVGPathParser.ParseHLineTo(Relative: Boolean; List: TList);
{* 解析 H/h 命令：读取 X 坐标序列，Y 保持当前值，规范化为 LineTo 段。 }
var
  X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(X) do
  begin
    if Relative then
      X := X + FCurX;
    Y := FCurY;

    Seg := NewSeg(pstLineTo);
    Seg^.X := X;
    Seg^.Y := Y;
    List.Add(Seg);

    FCurX     := X;
    FLastCtlX := FCurX;
    FLastCtlY := FCurY;
  end;
end;

procedure TCnSVGPathParser.ParseVLineTo(Relative: Boolean; List: TList);
{* 解析 V/v 命令：读取 Y 坐标序列，X 保持当前值，规范化为 LineTo 段。 }
var
  X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(Y) do
  begin
    if Relative then
      Y := Y + FCurY;
    X := FCurX;

    Seg := NewSeg(pstLineTo);
    Seg^.X := X;
    Seg^.Y := Y;
    List.Add(Seg);

    FCurY     := Y;
    FLastCtlX := FCurX;
    FLastCtlY := FCurY;
  end;
end;

procedure TCnSVGPathParser.ParseCubicBezier(Relative: Boolean; List: TList);
{* 解析 C/c 命令：每组读取 个数**（X1,Y1, X2,Y2, X,Y），生成三次贝塞尔段。
   FLastCtlX/Y 保存 X2,Y2 供后续 S/s 命令使用。 }
var
  X1, Y1, X2, Y2, X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(X1) do
  begin
    if not ReadNumber(Y1) then Break;
    if not ReadNumber(X2) then Break;
    if not ReadNumber(Y2) then Break;
    if not ReadNumber(X)  then Break;
    if not ReadNumber(Y)  then Break;

    if Relative then
    begin
      X1 := X1 + FCurX;
      Y1 := Y1 + FCurY;
      X2 := X2 + FCurX;
      Y2 := Y2 + FCurY;
      X  := X  + FCurX;
      Y  := Y  + FCurY;
    end;

    Seg := NewSeg(pstCubicBezier);
    Seg^.X1 := X1;
    Seg^.Y1 := Y1;
    Seg^.X2 := X2;
    Seg^.Y2 := Y2;
    Seg^.X  := X;
    Seg^.Y  := Y;
    List.Add(Seg);

    FLastCtlX := X2;  // 保存第二控制点供 S/s 反射
    FLastCtlY := Y2;
    FCurX     := X;
    FCurY     := Y;
  end;
end;

procedure TCnSVGPathParser.ParseSmoothCubic(Relative: Boolean; List: TList);
{* 解析 S/s 命令：每组读取 4 个数（X2,Y2, X,Y），第一控制点反射上一个控制点。
   规范化为 pstCubicBezier 段存储。 }
var
  X1, Y1, X2, Y2, X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(X2) do
  begin
    if not ReadNumber(Y2) then Break;
    if not ReadNumber(X)  then Break;
    if not ReadNumber(Y)  then Break;

    if Relative then
    begin
      X2 := X2 + FCurX;
      Y2 := Y2 + FCurY;
      X  := X  + FCurX;
      Y  := Y  + FCurY;
    end;

    // 反射上一控制点（若上一命令非 C/c/S/s，FLastCtlX/Y 等于 FCurX/Y）
    X1 := 2 * FCurX - FLastCtlX;
    Y1 := 2 * FCurY - FLastCtlY;

    Seg := NewSeg(pstCubicBezier);  // 规范化为 CubicBezier
    Seg^.X1 := X1;
    Seg^.Y1 := Y1;
    Seg^.X2 := X2;
    Seg^.Y2 := Y2;
    Seg^.X  := X;
    Seg^.Y  := Y;
    List.Add(Seg);

    FLastCtlX := X2;
    FLastCtlY := Y2;
    FCurX     := X;
    FCurY     := Y;
  end;
end;

procedure TCnSVGPathParser.ParseQuadBezier(Relative: Boolean; List: TList);
{* 解析 Q/q 命令：每组读取 4 个数（X1,Y1, X,Y），生成二次贝塞尔段。
   FLastCtlX/Y 保存 X1,Y1 供后续 T/t 命令使用。 }
var
  X1, Y1, X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(X1) do
  begin
    if not ReadNumber(Y1) then Break;
    if not ReadNumber(X)  then Break;
    if not ReadNumber(Y)  then Break;

    if Relative then
    begin
      X1 := X1 + FCurX;
      Y1 := Y1 + FCurY;
      X  := X  + FCurX;
      Y  := Y  + FCurY;
    end;

    Seg := NewSeg(pstQuadBezier);
    Seg^.X1 := X1;
    Seg^.Y1 := Y1;
    Seg^.X  := X;
    Seg^.Y  := Y;
    List.Add(Seg);

    FLastCtlX := X1;  // 保存控制点供 T/t 反射
    FLastCtlY := Y1;
    FCurX     := X;
    FCurY     := Y;
  end;
end;

procedure TCnSVGPathParser.ParseSmoothQuad(Relative: Boolean; List: TList);
{* 解析 T/t 命令：每组读取 2 个数（X,Y），控制点反射上一个二次贝塞尔控制点。
   规范化为 pstQuadBezier 段存储。 }
var
  X1, Y1, X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(X) do
  begin
    if not ReadNumber(Y) then Break;

    if Relative then
    begin
      X := X + FCurX;
      Y := Y + FCurY;
    end;

    // 反射上一控制点（若上一命令非 Q/q/T/t，FLastCtlX/Y 等于 FCurX/Y）
    X1 := 2 * FCurX - FLastCtlX;
    Y1 := 2 * FCurY - FLastCtlY;

    Seg := NewSeg(pstQuadBezier);  // 规范化为 QuadBezier
    Seg^.X1 := X1;
    Seg^.Y1 := Y1;
    Seg^.X  := X;
    Seg^.Y  := Y;
    List.Add(Seg);

    FLastCtlX := X1;
    FLastCtlY := Y1;
    FCurX     := X;
    FCurY     := Y;
  end;
end;

procedure TCnSVGPathParser.ParseArc(Relative: Boolean; List: TList);
{* 解析 A/a 命令：每组读取 7 个数（RX, RY, XRotation, LargeArcFlag, SweepFlag, X, Y）。
   RX/RY 取绝对值；FlagA/FlagS 截为布尔值（非零即 True）。 }
var
  RX, RY, XRot, FlagA, FlagS, X, Y: TCnSVGFloat;
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  while ReadNumber(RX) do
  begin
    if not ReadNumber(RY)    then Break;
    if not ReadNumber(XRot)  then Break;
    if not ReadNumber(FlagA) then Break;
    if not ReadNumber(FlagS) then Break;
    if not ReadNumber(X)     then Break;
    if not ReadNumber(Y)     then Break;

    if Relative then
    begin
      X := X + FCurX;
      Y := Y + FCurY;
    end;

    Seg := NewSeg(pstArc);
    Seg^.RX        := Abs(RX);
    Seg^.RY        := Abs(RY);
    Seg^.XRotation := XRot;
    Seg^.LargeArc  := (Round(FlagA) <> 0);
    Seg^.Sweep     := (Round(FlagS) <> 0);
    Seg^.X         := X;
    Seg^.Y         := Y;
    List.Add(Seg);

    FLastCtlX := FCurX;  // 弧段不更新控制点，重置为当前笔位
    FLastCtlY := FCurY;
    FCurX     := X;
    FCurY     := Y;
  end;
end;

procedure TCnSVGPathParser.ParseClosePath(List: TList);
{* 解析 Z/z 命令：生成一个 ClosePath 段，端点记录当前笔位（渲染器负责闭合到子路径起点）。 }
var
  Seg: PCnSVGPathSeg;

  function NewSeg(SegType: TCnSVGPathSegType): PCnSVGPathSeg;
  begin
    New(Result);
    FillChar(Result^, SizeOf(TCnSVGPathSeg), 0);
    Result^.SegType := SegType;
  end;

begin
  Seg := NewSeg(pstClosePath);
  Seg^.X := FCurX;
  Seg^.Y := FCurY;
  List.Add(Seg);

  FLastCtlX := FCurX;
  FLastCtlY := FCurY;
end;

function TCnSVGPathParser.ParsePathData(const D: string): TList;
var
  Cmd: Char;
  Trimmed: string;
begin
  Result  := TList.Create;
  Trimmed := Trim(D);
  if Trimmed = '' then Exit;

  FData     := D;
  FPos      := 1;
  FCurX     := 0;
  FCurY     := 0;
  FLastCtlX := 0;
  FLastCtlY := 0;
  FLastCmd  := #0;

  while FPos <= Length(FData) do
  begin
    SkipWS;
    if FPos > Length(FData) then Break;

    Cmd := PeekCmd;
    if Cmd <> #0 then
    begin
      Inc(FPos); // consume command letter
      FLastCmd := Cmd;
    end
    else
    begin
      // Implicit repeat: use FLastCmd
      Cmd := FLastCmd;
      if Cmd = #0 then Break; // no valid command yet — skip
    end;

    case Cmd of
      'M': ParseMoveTo(False, Result);
      'm': ParseMoveTo(True,  Result);
      'L': ParseLineTo(False, Result);
      'l': ParseLineTo(True,  Result);
      'H': ParseHLineTo(False, Result);
      'h': ParseHLineTo(True,  Result);
      'V': ParseVLineTo(False, Result);
      'v': ParseVLineTo(True,  Result);
      'C': ParseCubicBezier(False, Result);
      'c': ParseCubicBezier(True,  Result);
      'S': ParseSmoothCubic(False, Result);
      's': ParseSmoothCubic(True,  Result);
      'Q': ParseQuadBezier(False, Result);
      'q': ParseQuadBezier(True,  Result);
      'T': ParseSmoothQuad(False, Result);
      't': ParseSmoothQuad(True,  Result);
      'A': ParseArc(False, Result);
      'a': ParseArc(True,  Result);
      'Z', 'z': ParseClosePath(Result);
    else
      // Unknown command — skip to next letter
      while (FPos <= Length(FData)) and (PeekCmd = #0) do
        Inc(FPos);
    end;
  end;
end;


//==============================================================================
// TCnSVGRenderer 实现存根
//==============================================================================

constructor TCnSVGRenderer.Create(ACanvas: TCanvas; const ADestRect: TRect;
  AViewBox: TCnSVGRect; ADefsMap: TStringList; const ABasePath: string;
  const APreserveAspectRatio: string);
var
  NeedClip: Boolean;
begin
  inherited Create;
  FCtx.Canvas   := ACanvas;
  FCtx.UseDepth := 0;
  FMatrixTop    := 0;
  FStyleTop     := 0;
  FDefsMap      := ADefsMap;
  FBasePath     := ABasePath;
  FSavedDC      := SaveDC(ACanvas.Handle);

  // Initialize root style
  SVGDefaultStyle(FCtx.Style);
  FStyleStack[0] := FCtx.Style;

  SVGCalcViewMatrix(ADestRect, AViewBox, APreserveAspectRatio,
    FViewMatrix, NeedClip);
  if NeedClip then
    IntersectClipRect(FCtx.Canvas.Handle, ADestRect.Left, ADestRect.Top,
      ADestRect.Right, ADestRect.Bottom);
  FCtx.CTM := FViewMatrix;
  FMatrixStack[0] := FCtx.CTM;
end;

destructor TCnSVGRenderer.Destroy;
begin
  if FSavedDC <> 0 then
    RestoreDC(FCtx.Canvas.Handle, FSavedDC);
  inherited Destroy;
end;

procedure TCnSVGRenderer.PushMatrix;
begin
  if FMatrixTop < 63 then
  begin
    FMatrixStack[FMatrixTop + 1] := FCtx.CTM;
    Inc(FMatrixTop);
  end;
end;

procedure TCnSVGRenderer.PopMatrix;
begin
  if FMatrixTop > 0 then
  begin
    FCtx.CTM := FMatrixStack[FMatrixTop];
    Dec(FMatrixTop);
  end;
end;

procedure TCnSVGRenderer.PushStyle;
begin
  if FStyleTop < 63 then
  begin
    FStyleStack[FStyleTop + 1] := FCtx.Style;
    Inc(FStyleTop);
  end;
end;

procedure TCnSVGRenderer.PopStyle;
begin
  if FStyleTop > 0 then
  begin
    FCtx.Style := FStyleStack[FStyleTop];
    Dec(FStyleTop);
  end;
end;

procedure SVGParseTransformParams(const S: string;
  var Params: array of Extended; var ParamCount: Integer);
{* 解析 transform 函数括号内的参数字符串，填入 Params 数组（最多 6 个），
   返回实际解析到的参数个数（ParamCount）。Delphi 5 兼容，不含嵌套过程。 }
var
  P, PStart: Integer;
  Tok: string;
begin
  ParamCount := 0;
  P := 1;
  while P <= Length(S) do
  begin
    // skip whitespace and commas
    while (P <= Length(S)) and (S[P] in [' ', #9, #10, #13, ',']) do Inc(P);
    if P > Length(S) then Break;
    // read token (possibly with leading sign)
    PStart := P;
    if S[P] in ['+', '-'] then Inc(P);
    while (P <= Length(S)) and (S[P] in ['0'..'9', '.', 'e', 'E', '+', '-']) do Inc(P);
    Tok := Trim(Copy(S, PStart, P - PStart));
    if Tok = '' then Break;
    if ParamCount <= High(Params) then
    begin
      try
        Params[ParamCount] := StrToFloat(Tok);
        Inc(ParamCount);
      except
        // skip unparseable token
      end;
    end
    else
      Break;
  end;
end;

procedure TCnSVGRenderer.ApplyTransformAttr(const ATransform: string);
var
  I, Start, FuncStart: Integer;
  FuncName: string;
  ParamStr: string;
  Params: array[0..5] of Extended;
  ParamCount: Integer;
  M: TCnSVGMatrix;
  FuncLower: string;
begin
  try
    I := 1;
    while I <= Length(ATransform) do
    begin
      // skip whitespace
      while (I <= Length(ATransform)) and (ATransform[I] in [' ', #9, #10, #13, ',']) do Inc(I);
      if I > Length(ATransform) then Break;

      // read function name (alpha chars and hyphen)
      FuncStart := I;
      while (I <= Length(ATransform)) and (ATransform[I] in ['a'..'z', 'A'..'Z', '-']) do Inc(I);
      FuncName := Trim(Copy(ATransform, FuncStart, I - FuncStart));
      if FuncName = '' then begin Inc(I); Continue; end;
      FuncLower := LowerCase(FuncName);

      // skip to '('
      while (I <= Length(ATransform)) and (ATransform[I] <> '(') do Inc(I);
      if I > Length(ATransform) then Break;
      Inc(I); // consume '('

      // read until ')'
      Start := I;
      while (I <= Length(ATransform)) and (ATransform[I] <> ')') do Inc(I);
      ParamStr := Copy(ATransform, Start, I - Start);
      if I <= Length(ATransform) then Inc(I); // consume ')'

      // parse params
      FillChar(Params, SizeOf(Params), 0);
      ParamCount := 0;
      SVGParseTransformParams(ParamStr, Params, ParamCount);

      // apply transform
      if FuncLower = 'translate' then
      begin
        if ParamCount >= 1 then
          if ParamCount >= 2 then
            SVGMatrixTranslate(FCtx.CTM, Params[0], Params[1])
          else
            SVGMatrixTranslate(FCtx.CTM, Params[0], 0);
      end
      else if FuncLower = 'scale' then
      begin
        if ParamCount >= 1 then
          if ParamCount >= 2 then
            SVGMatrixScale(FCtx.CTM, Params[0], Params[1])
          else
            SVGMatrixScale(FCtx.CTM, Params[0], Params[0]);
      end
      else if FuncLower = 'rotate' then
      begin
        if ParamCount >= 1 then
          if ParamCount >= 3 then
            SVGMatrixRotate(FCtx.CTM, Params[0], Params[1], Params[2])
          else
            SVGMatrixRotate(FCtx.CTM, Params[0], 0, 0);
      end
      else if FuncLower = 'matrix' then
      begin
        if ParamCount >= 6 then
        begin
          SVGMatrixIdentity(M);
          M.a := Params[0]; M.b := Params[1]; M.c := Params[2];
          M.d := Params[3]; M.e := Params[4]; M.f := Params[5];
          SVGMatrixMultiply(FCtx.CTM, FCtx.CTM, M);
        end;
      end
      else if FuncLower = 'skewx' then
      begin
        if ParamCount >= 1 then
          SVGMatrixSkewX(FCtx.CTM, Params[0]);
      end
      else if FuncLower = 'skewy' then
      begin
        if ParamCount >= 1 then
          SVGMatrixSkewY(FCtx.CTM, Params[0]);
      end;
      // unknown function: silently ignore
    end;
  except
    // silently swallow any unexpected exception
  end;
end;

procedure TCnSVGRenderer.ApplyStyleAttr(AElement: TCnXMLElement);
var
  ParentStyle: TCnSVGStyle;
begin
  // The current FCtx.Style is the parent's resolved style.
  // PushStyle already saved it before we got here, and
  // the parent's ApplyStyleAttr updated FCtx.Style with
  ParentStyle := FCtx.Style;
  SVGParseStyleAttr(AElement, FCtx.Style, ParentStyle);
end;

function TCnSVGRenderer.CreateGDIPenHandle(AColor: TColor): HPEN;
var
  S: TCnSVGStyle;
  PenWidth: Integer;
  PenStyle: DWORD;
  LogBrush: TLogBrush;
  DashStyles: array[0..7] of DWORD;
  DashCount: DWORD;
  I: Integer;
begin
  Result := 0;
  S := FCtx.Style;
  if S.StrokeNone then
    Exit;
  PenWidth := UserLenToScreen(S.StrokeWidth);
  if PenWidth < 1 then
    PenWidth := 1;

  FillChar(LogBrush, SizeOf(LogBrush), 0);
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := ColorToRGB(AColor);
  LogBrush.lbHatch := 0;

  PenStyle := PS_GEOMETRIC;
  if S.DashCount > 0 then
    PenStyle := PenStyle or PS_USERSTYLE
  else
    PenStyle := PenStyle or PS_SOLID;

  case S.LineCap of
    slcRound:
      PenStyle := PenStyle or PS_ENDCAP_ROUND;
    slcSquare:
      PenStyle := PenStyle or PS_ENDCAP_SQUARE;
  else
    PenStyle := PenStyle or PS_ENDCAP_FLAT;
  end;

  case S.LineJoin of
    sljRound:
      PenStyle := PenStyle or PS_JOIN_ROUND;
    sljBevel:
      PenStyle := PenStyle or PS_JOIN_BEVEL;
  else
    PenStyle := PenStyle or PS_JOIN_MITER;
  end;

  DashCount := 0;
  if S.DashCount > 0 then
  begin
    for I := 0 to S.DashCount - 1 do
    begin
      DashStyles[I] := UserLenToScreen(S.DashArray[I]);
      if DashStyles[I] = 0 then
        DashStyles[I] := 1;
      Inc(DashCount);
    end;
  end;

  if DashCount > 0 then
    Result := ExtCreatePen(PenStyle, PenWidth, LogBrush, DashCount, @DashStyles[0])
  else
    Result := ExtCreatePen(PenStyle, PenWidth, LogBrush, 0, nil);
end;

procedure TCnSVGRenderer.SetupGDIPen;
var
  S: TCnSVGStyle;
  PenHandle: HPEN;
begin
  S := FCtx.Style;
  if S.StrokeNone then
  begin
    FCtx.Canvas.Pen.Style := psClear;
    FCtx.Canvas.Pen.Width := 1;
    Exit;
  end;

  PenHandle := CreateGDIPenHandle(
    Windows.RGB(S.StrokeColor.R, S.StrokeColor.G, S.StrokeColor.B));
  if PenHandle <> 0 then
    FCtx.Canvas.Pen.Handle := PenHandle
  else
  begin
    FCtx.Canvas.Pen.Color := Windows.RGB(S.StrokeColor.R, S.StrokeColor.G, S.StrokeColor.B);
    FCtx.Canvas.Pen.Style := psSolid;
    FCtx.Canvas.Pen.Width := UserLenToScreen(S.StrokeWidth);
  end;
  SetMiterLimit(FCtx.Canvas.Handle, S.MiterLimit, nil);
end;

procedure TCnSVGRenderer.SetupGDIBrush;
var
  S: TCnSVGStyle;
begin
  S := FCtx.Style;
  if S.FillNone then
  begin
    FCtx.Canvas.Brush.Style := bsClear;
    Exit;
  end;
  FCtx.Canvas.Brush.Color := Windows.RGB(S.FillColor.R, S.FillColor.G, S.FillColor.B);
  FCtx.Canvas.Brush.Style := bsSolid;
end;

function TCnSVGRenderer.UserToScreen(X, Y: TCnSVGFloat): TPoint;
begin
  SVGMatrixTransformPoint(FCtx.CTM, X, Y);
  Result.X := Round(X);
  Result.Y := Round(Y);
end;

function TCnSVGRenderer.UserLenToScreen(Len: TCnSVGFloat): Integer;
var
  Scaled: TCnSVGFloat;
begin
  Scaled := Abs(Len * FCtx.CTM.a);
  Result := Round(Scaled);
  if Result < 1 then
    Result := 1;
end;

function TCnSVGRenderer.EffectiveFillAlpha: TCnSVGFloat;
begin
  if FCtx.Style.FillNone then
    Result := 0
  else
    Result := SVGClampOpacity(FCtx.Style.Opacity * FCtx.Style.FillOpacity);
end;

function TCnSVGRenderer.EffectiveStrokeAlpha: TCnSVGFloat;
begin
  if FCtx.Style.StrokeNone then
    Result := 0
  else
    Result := SVGClampOpacity(FCtx.Style.Opacity * FCtx.Style.StrokeOpacity);
end;

procedure TCnSVGRenderer.BlendMaskBitmap(ADestCanvas: TCanvas; const ABounds: TRect;
  AMaskBmp: TBitmap; SrcColor: TColor; Alpha: TCnSVGFloat);
var
  X, Y: Integer;
begin
  if (AMaskBmp = nil) or (Alpha <= 0) then
    Exit;
  for Y := 0 to AMaskBmp.Height - 1 do
    for X := 0 to AMaskBmp.Width - 1 do
      if ColorToRGB(AMaskBmp.Canvas.Pixels[X, Y]) <> ColorToRGB(clBlack) then
        AlphaBlendPixel(ADestCanvas, ABounds.Left + X, ABounds.Top + Y,
          SrcColor, Alpha);
end;

procedure TCnSVGRenderer.AlphaBlendPixel(ACanvas: TCanvas; X, Y: Integer;
  SrcColor: TColor; Alpha: TCnSVGFloat);
var
  DstColor: TColor;
  SR, SG, SB: Byte;
  DR, DG, DB: Byte;
  A: Integer;
begin
  if Alpha >= 1.0 then
  begin
    ACanvas.Pixels[X, Y] := SrcColor;
    Exit;
  end;
  if Alpha <= 0.0 then Exit;
  DstColor := ACanvas.Pixels[X, Y];
  SR := GetRValue(SrcColor);
  SG := GetGValue(SrcColor);
  SB := GetBValue(SrcColor);
  DR := GetRValue(DstColor);
  DG := GetGValue(DstColor);
  DB := GetBValue(DstColor);
  A := Round(Alpha * 255);
  DR := (SR * A + DR * (255 - A)) div 256;
  DG := (SG * A + DG * (255 - A)) div 256;
  DB := (SB * A + DB * (255 - A)) div 256;
  ACanvas.Pixels[X, Y] := Windows.RGB(DR, DG, DB);
end;

procedure TCnSVGRenderer.FillWithAlpha(ACanvas: TCanvas;
  const Pts: array of TPoint;
  FillColor: TCnSVGColor; Alpha: TCnSVGFloat);
var
  I, MinX, MinY, MaxX, MaxY, X, Y: Integer;
  TmpBmp: TBitmap;
  SrcColor: TColor;
  PolyPts: array of TPoint;
begin
  if Length(Pts) < 3 then Exit;
  if Alpha <= 0 then Exit;

  // find bounding rect
  MinX := Pts[0].X; MaxX := Pts[0].X;
  MinY := Pts[0].Y; MaxY := Pts[0].Y;
  for I := 1 to High(Pts) do
  begin
    if Pts[I].X < MinX then MinX := Pts[I].X;
    if Pts[I].X > MaxX then MaxX := Pts[I].X;
    if Pts[I].Y < MinY then MinY := Pts[I].Y;
    if Pts[I].Y > MaxY then MaxY := Pts[I].Y;
  end;
  if (MaxX <= MinX) or (MaxY <= MinY) then Exit;

  SrcColor := Windows.RGB(FillColor.R, FillColor.G, FillColor.B);

  // Draw filled polygon onto a temporary white bitmap
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.PixelFormat := pf24bit;
    TmpBmp.Width  := MaxX - MinX + 1;
    TmpBmp.Height := MaxY - MinY + 1;
    TmpBmp.Canvas.Brush.Color := clWhite;
    TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));

    // Translate polygon to bitmap local coords
    SetLength(PolyPts, Length(Pts));
    for I := 0 to High(Pts) do
    begin
      PolyPts[I].X := Pts[I].X - MinX;
      PolyPts[I].Y := Pts[I].Y - MinY;
    end;
    TmpBmp.Canvas.Brush.Color := SrcColor;
    TmpBmp.Canvas.Brush.Style := bsSolid;
    TmpBmp.Canvas.Pen.Style   := psClear;
    Windows.Polygon(TmpBmp.Canvas.Handle, PolyPts[0], Length(PolyPts));

    // Alpha blend each non-white pixel back to main canvas
    for Y := 0 to TmpBmp.Height - 1 do
      for X := 0 to TmpBmp.Width - 1 do
      begin
        if TmpBmp.Canvas.Pixels[X, Y] <> clWhite then
          AlphaBlendPixel(ACanvas, MinX + X, MinY + Y, SrcColor, Alpha);
      end;
  finally
    TmpBmp.Free;
  end;
end;

procedure TCnSVGRenderer.FillPolyPolygonWithAlpha(ACanvas: TCanvas;
  const Pts: array of TPoint; PolyCounts: array of Integer;
  PolyCount: Integer; FillColor: TCnSVGColor; Alpha: TCnSVGFloat;
  FillRule: TCnSVGFillRule);
var
  I, MinX, MinY, MaxX, MaxY: Integer;
  TmpBmp: TBitmap;
  LocalPts: array of TPoint;
  SrcColor: TColor;
  Bounds: TRect;
begin
  if (Length(Pts) = 0) or (PolyCount <= 0) or (Alpha <= 0) then
    Exit;

  MinX := Pts[0].X;
  MaxX := Pts[0].X;
  MinY := Pts[0].Y;
  MaxY := Pts[0].Y;
  for I := 1 to High(Pts) do
  begin
    if Pts[I].X < MinX then MinX := Pts[I].X;
    if Pts[I].X > MaxX then MaxX := Pts[I].X;
    if Pts[I].Y < MinY then MinY := Pts[I].Y;
    if Pts[I].Y > MaxY then MaxY := Pts[I].Y;
  end;
  if (MaxX <= MinX) or (MaxY <= MinY) then
    Exit;

  Bounds := Rect(MinX, MinY, MaxX + 1, MaxY + 1);
  SetLength(LocalPts, Length(Pts));
  for I := 0 to High(Pts) do
  begin
    LocalPts[I].X := Pts[I].X - Bounds.Left;
    LocalPts[I].Y := Pts[I].Y - Bounds.Top;
  end;

  SrcColor := Windows.RGB(FillColor.R, FillColor.G, FillColor.B);
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.PixelFormat := pf24bit;
    TmpBmp.Width := Bounds.Right - Bounds.Left;
    TmpBmp.Height := Bounds.Bottom - Bounds.Top;
    TmpBmp.Canvas.Brush.Color := clBlack;
    TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
    TmpBmp.Canvas.Brush.Color := clWhite;
    TmpBmp.Canvas.Brush.Style := bsSolid;
    TmpBmp.Canvas.Pen.Style := psClear;
    if FillRule = sfrEvenOdd then
      SetPolyFillMode(TmpBmp.Canvas.Handle, ALTERNATE)
    else
      SetPolyFillMode(TmpBmp.Canvas.Handle, WINDING);

    if PolyCount = 1 then
      Windows.Polygon(TmpBmp.Canvas.Handle, LocalPts[0], PolyCounts[0])
    else
      Windows.PolyPolygon(TmpBmp.Canvas.Handle, LocalPts[0], PolyCounts[0], PolyCount);

    BlendMaskBitmap(ACanvas, Bounds, TmpBmp, SrcColor, Alpha);
  finally
    TmpBmp.Free;
  end;
end;

procedure TCnSVGRenderer.StrokePolyPointsWithAlpha(ACanvas: TCanvas;
  const Pts: array of TPoint; const PolyCounts: array of Integer;
  PolyCount: Integer; StrokeColor: TCnSVGColor; Alpha: TCnSVGFloat);
var
  I, K, MinX, MinY, MaxX, MaxY, PenWidth, OffsetX, OffsetY: Integer;
  TmpBmp: TBitmap;
  LocalPts: array of TPoint;
  SrcColor: TColor;
  Bounds: TRect;
  PenHandle: HPEN;
begin
  if (Length(Pts) = 0) or (PolyCount <= 0) or (Alpha <= 0) then
    Exit;

  MinX := Pts[0].X;
  MaxX := Pts[0].X;
  MinY := Pts[0].Y;
  MaxY := Pts[0].Y;
  for I := 1 to High(Pts) do
  begin
    if Pts[I].X < MinX then MinX := Pts[I].X;
    if Pts[I].X > MaxX then MaxX := Pts[I].X;
    if Pts[I].Y < MinY then MinY := Pts[I].Y;
    if Pts[I].Y > MaxY then MaxY := Pts[I].Y;
  end;

  PenWidth := UserLenToScreen(FCtx.Style.StrokeWidth) + 2;
  Bounds := Rect(MinX - PenWidth, MinY - PenWidth, MaxX + PenWidth + 1,
    MaxY + PenWidth + 1);
  OffsetX := -Bounds.Left;
  OffsetY := -Bounds.Top;

  SetLength(LocalPts, Length(Pts));
  for I := 0 to High(Pts) do
  begin
    LocalPts[I].X := Pts[I].X + OffsetX;
    LocalPts[I].Y := Pts[I].Y + OffsetY;
  end;

  SrcColor := Windows.RGB(StrokeColor.R, StrokeColor.G, StrokeColor.B);
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.PixelFormat := pf24bit;
    TmpBmp.Width := Bounds.Right - Bounds.Left;
    TmpBmp.Height := Bounds.Bottom - Bounds.Top;
    TmpBmp.Canvas.Brush.Color := clBlack;
    TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
    TmpBmp.Canvas.Brush.Style := bsClear;
    PenHandle := CreateGDIPenHandle(clWhite);
    if PenHandle <> 0 then
      TmpBmp.Canvas.Pen.Handle := PenHandle
    else
    begin
      TmpBmp.Canvas.Pen.Color := clWhite;
      TmpBmp.Canvas.Pen.Width := UserLenToScreen(FCtx.Style.StrokeWidth);
    end;
    SetMiterLimit(TmpBmp.Canvas.Handle, FCtx.Style.MiterLimit, nil);

    K := 0;
    for I := 0 to PolyCount - 1 do
    begin
      if PolyCounts[I] >= 2 then
        Windows.Polyline(TmpBmp.Canvas.Handle, LocalPts[K], PolyCounts[I]);
      Inc(K, PolyCounts[I]);
    end;

    BlendMaskBitmap(ACanvas, Bounds, TmpBmp, SrcColor, Alpha);
  finally
    TmpBmp.Free;
  end;
end;

procedure TCnSVGRenderer.RenderRectAlpha(const Bounds: TRect; RX, RY: Integer);
var
  FillAlpha, StrokeAlpha: TCnSVGFloat;
  TmpBmp: TBitmap;
  LocalRect, StrokeBounds: TRect;
  PenPad: Integer;
  FillColor, StrokeColor: TColor;
  PenHandle: HPEN;
begin
  FillAlpha := EffectiveFillAlpha;
  StrokeAlpha := EffectiveStrokeAlpha;
  FillColor := Windows.RGB(FCtx.Style.FillColor.R, FCtx.Style.FillColor.G, FCtx.Style.FillColor.B);
  StrokeColor := Windows.RGB(FCtx.Style.StrokeColor.R, FCtx.Style.StrokeColor.G, FCtx.Style.StrokeColor.B);

  if (not FCtx.Style.FillNone) and (FillAlpha > 0) then
  begin
    if FillAlpha >= 1.0 then
    begin
      SetupGDIBrush;
      FCtx.Canvas.Pen.Style := psClear;
      if (RX > 0) or (RY > 0) then
        FCtx.Canvas.RoundRect(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom, RX, RY)
      else
        FCtx.Canvas.Rectangle(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
    end
    else
    begin
      TmpBmp := TBitmap.Create;
      try
        TmpBmp.PixelFormat := pf24bit;
        TmpBmp.Width := Bounds.Right - Bounds.Left;
        TmpBmp.Height := Bounds.Bottom - Bounds.Top;
        TmpBmp.Canvas.Brush.Color := clBlack;
        TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
        TmpBmp.Canvas.Brush.Color := clWhite;
        TmpBmp.Canvas.Brush.Style := bsSolid;
        TmpBmp.Canvas.Pen.Style := psClear;
        LocalRect := Rect(0, 0, TmpBmp.Width, TmpBmp.Height);
        if (RX > 0) or (RY > 0) then
          TmpBmp.Canvas.RoundRect(LocalRect.Left, LocalRect.Top, LocalRect.Right,
            LocalRect.Bottom, RX, RY)
        else
          TmpBmp.Canvas.Rectangle(LocalRect.Left, LocalRect.Top, LocalRect.Right,
            LocalRect.Bottom);
        BlendMaskBitmap(FCtx.Canvas, Bounds, TmpBmp, FillColor, FillAlpha);
      finally
        TmpBmp.Free;
      end;
    end;
  end;

  if (not FCtx.Style.StrokeNone) and (StrokeAlpha > 0) then
  begin
    if StrokeAlpha >= 1.0 then
    begin
      SetupGDIPen;
      FCtx.Canvas.Brush.Style := bsClear;
      if (RX > 0) or (RY > 0) then
        FCtx.Canvas.RoundRect(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom, RX, RY)
      else
        FCtx.Canvas.Rectangle(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
    end
    else
    begin
      PenPad := UserLenToScreen(FCtx.Style.StrokeWidth) + 2;
      StrokeBounds := Rect(Bounds.Left - PenPad, Bounds.Top - PenPad,
        Bounds.Right + PenPad, Bounds.Bottom + PenPad);
      TmpBmp := TBitmap.Create;
      try
        TmpBmp.PixelFormat := pf24bit;
        TmpBmp.Width := StrokeBounds.Right - StrokeBounds.Left;
        TmpBmp.Height := StrokeBounds.Bottom - StrokeBounds.Top;
        TmpBmp.Canvas.Brush.Color := clBlack;
        TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
        TmpBmp.Canvas.Brush.Style := bsClear;
        PenHandle := CreateGDIPenHandle(clWhite);
        if PenHandle <> 0 then
          TmpBmp.Canvas.Pen.Handle := PenHandle
        else
        begin
          TmpBmp.Canvas.Pen.Color := clWhite;
          TmpBmp.Canvas.Pen.Width := UserLenToScreen(FCtx.Style.StrokeWidth);
        end;
        SetMiterLimit(TmpBmp.Canvas.Handle, FCtx.Style.MiterLimit, nil);
        LocalRect := Rect(PenPad, PenPad, PenPad + (Bounds.Right - Bounds.Left),
          PenPad + (Bounds.Bottom - Bounds.Top));
        if (RX > 0) or (RY > 0) then
          TmpBmp.Canvas.RoundRect(LocalRect.Left, LocalRect.Top, LocalRect.Right,
            LocalRect.Bottom, RX, RY)
        else
          TmpBmp.Canvas.Rectangle(LocalRect.Left, LocalRect.Top, LocalRect.Right,
            LocalRect.Bottom);
        BlendMaskBitmap(FCtx.Canvas, StrokeBounds, TmpBmp, StrokeColor, StrokeAlpha);
      finally
        TmpBmp.Free;
      end;
    end;
  end;
end;

procedure TCnSVGRenderer.RenderEllipseAlpha(const Bounds: TRect);
var
  FillAlpha, StrokeAlpha: TCnSVGFloat;
  TmpBmp: TBitmap;
  LocalRect, StrokeBounds: TRect;
  PenPad: Integer;
  FillColor, StrokeColor: TColor;
  PenHandle: HPEN;
begin
  FillAlpha := EffectiveFillAlpha;
  StrokeAlpha := EffectiveStrokeAlpha;
  FillColor := Windows.RGB(FCtx.Style.FillColor.R, FCtx.Style.FillColor.G, FCtx.Style.FillColor.B);
  StrokeColor := Windows.RGB(FCtx.Style.StrokeColor.R, FCtx.Style.StrokeColor.G, FCtx.Style.StrokeColor.B);

  if (not FCtx.Style.FillNone) and (FillAlpha > 0) then
  begin
    if FillAlpha >= 1.0 then
    begin
      SetupGDIBrush;
      FCtx.Canvas.Pen.Style := psClear;
      FCtx.Canvas.Ellipse(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
    end
    else
    begin
      TmpBmp := TBitmap.Create;
      try
        TmpBmp.PixelFormat := pf24bit;
        TmpBmp.Width := Bounds.Right - Bounds.Left;
        TmpBmp.Height := Bounds.Bottom - Bounds.Top;
        TmpBmp.Canvas.Brush.Color := clBlack;
        TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
        TmpBmp.Canvas.Brush.Color := clWhite;
        TmpBmp.Canvas.Brush.Style := bsSolid;
        TmpBmp.Canvas.Pen.Style := psClear;
        LocalRect := Rect(0, 0, TmpBmp.Width, TmpBmp.Height);
        TmpBmp.Canvas.Ellipse(LocalRect.Left, LocalRect.Top, LocalRect.Right, LocalRect.Bottom);
        BlendMaskBitmap(FCtx.Canvas, Bounds, TmpBmp, FillColor, FillAlpha);
      finally
        TmpBmp.Free;
      end;
    end;
  end;

  if (not FCtx.Style.StrokeNone) and (StrokeAlpha > 0) then
  begin
    if StrokeAlpha >= 1.0 then
    begin
      SetupGDIPen;
      FCtx.Canvas.Brush.Style := bsClear;
      FCtx.Canvas.Ellipse(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
    end
    else
    begin
      PenPad := UserLenToScreen(FCtx.Style.StrokeWidth) + 2;
      StrokeBounds := Rect(Bounds.Left - PenPad, Bounds.Top - PenPad,
        Bounds.Right + PenPad, Bounds.Bottom + PenPad);
      TmpBmp := TBitmap.Create;
      try
        TmpBmp.PixelFormat := pf24bit;
        TmpBmp.Width := StrokeBounds.Right - StrokeBounds.Left;
        TmpBmp.Height := StrokeBounds.Bottom - StrokeBounds.Top;
        TmpBmp.Canvas.Brush.Color := clBlack;
        TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
        TmpBmp.Canvas.Brush.Style := bsClear;
        PenHandle := CreateGDIPenHandle(clWhite);
        if PenHandle <> 0 then
          TmpBmp.Canvas.Pen.Handle := PenHandle
        else
        begin
          TmpBmp.Canvas.Pen.Color := clWhite;
          TmpBmp.Canvas.Pen.Width := UserLenToScreen(FCtx.Style.StrokeWidth);
        end;
        SetMiterLimit(TmpBmp.Canvas.Handle, FCtx.Style.MiterLimit, nil);
        LocalRect := Rect(PenPad, PenPad, PenPad + (Bounds.Right - Bounds.Left),
          PenPad + (Bounds.Bottom - Bounds.Top));
        TmpBmp.Canvas.Ellipse(LocalRect.Left, LocalRect.Top, LocalRect.Right, LocalRect.Bottom);
        BlendMaskBitmap(FCtx.Canvas, StrokeBounds, TmpBmp, StrokeColor, StrokeAlpha);
      finally
        TmpBmp.Free;
      end;
    end;
  end;
end;

procedure TCnSVGRenderer.RenderRect(AElement: TCnXMLElement);
var
  X, Y, W, H, RX, RY: TCnSVGFloat;
  P1, P2: TPoint;
  SRX, SRY: Integer;
  FillAlpha, StrokeAlpha: TCnSVGFloat;
  FillPts: array[0..3] of TPoint;
  StrokePts: array[0..4] of TPoint;
  PolyCounts: array[0..0] of Integer;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  X  := SVGAttrFloat(AElement, 'x', 0);
  Y  := SVGAttrFloat(AElement, 'y', 0);
  W  := SVGAttrFloat(AElement, 'width', -1);
  H  := SVGAttrFloat(AElement, 'height', -1);
  if (W <= 0) or (H <= 0) then Exit;
  RX := SVGAttrFloat(AElement, 'rx', -1);
  RY := SVGAttrFloat(AElement, 'ry', -1);
  if (RX < 0) and (RY < 0) then begin RX := 0; RY := 0; end
  else if RX < 0 then RX := RY
  else if RY < 0 then RY := RX;
  if RX > W / 2 then RX := W / 2;
  if RY > H / 2 then RY := H / 2;
  P1 := UserToScreen(X, Y);
  P2 := UserToScreen(X + W, Y + H);
  FillAlpha := EffectiveFillAlpha;
  StrokeAlpha := EffectiveStrokeAlpha;
  if (RX > 0) or (RY > 0) then
  begin
    SRX := UserLenToScreen(RX * 2);
    SRY := UserLenToScreen(RY * 2);
    if (FillAlpha < 1.0) or (StrokeAlpha < 1.0) then
      RenderRectAlpha(Rect(P1.X, P1.Y, P2.X, P2.Y), SRX, SRY)
    else
    begin
      SetupGDIPen;
      SetupGDIBrush;
      FCtx.Canvas.RoundRect(P1.X, P1.Y, P2.X, P2.Y, SRX, SRY);
    end;
  end
  else
  begin
    FillPts[0] := Point(P1.X, P1.Y);
    FillPts[1] := Point(P2.X, P1.Y);
    FillPts[2] := Point(P2.X, P2.Y);
    FillPts[3] := Point(P1.X, P2.Y);
    StrokePts[0] := FillPts[0];
    StrokePts[1] := FillPts[1];
    StrokePts[2] := FillPts[2];
    StrokePts[3] := FillPts[3];
    StrokePts[4] := FillPts[0];
    PolyCounts[0] := 4;

    if (not FCtx.Style.FillNone) and (FillAlpha > 0) then
    begin
      if FillAlpha < 1.0 then
        FillPolyPolygonWithAlpha(FCtx.Canvas, FillPts, PolyCounts, 1,
          FCtx.Style.FillColor, FillAlpha, sfrNonZero)
      else
      begin
        SetupGDIBrush;
        FCtx.Canvas.Pen.Style := psClear;
        FCtx.Canvas.Rectangle(P1.X, P1.Y, P2.X, P2.Y);
      end;
    end;

    if (not FCtx.Style.StrokeNone) and (StrokeAlpha > 0) then
    begin
      PolyCounts[0] := 5;
      if StrokeAlpha < 1.0 then
        StrokePolyPointsWithAlpha(FCtx.Canvas, StrokePts, PolyCounts, 1,
          FCtx.Style.StrokeColor, StrokeAlpha)
      else
      begin
        SetupGDIPen;
        FCtx.Canvas.Brush.Style := bsClear;
        Windows.Polyline(FCtx.Canvas.Handle, StrokePts[0], 5);
      end;
    end;
  end;
end;

procedure TCnSVGRenderer.RenderCircle(AElement: TCnXMLElement);
var
  CX, CY, R: TCnSVGFloat;
  P1, P2: TPoint;
  FillAlpha, StrokeAlpha: TCnSVGFloat;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  CX := SVGAttrFloat(AElement, 'cx', 0);
  CY := SVGAttrFloat(AElement, 'cy', 0);
  R  := SVGAttrFloat(AElement, 'r', -1);
  if R <= 0 then Exit;
  P1 := UserToScreen(CX - R, CY - R);
  P2 := UserToScreen(CX + R, CY + R);
  FillAlpha := EffectiveFillAlpha;
  StrokeAlpha := EffectiveStrokeAlpha;
  if (FillAlpha < 1.0) or (StrokeAlpha < 1.0) then
    RenderEllipseAlpha(Rect(P1.X, P1.Y, P2.X, P2.Y))
  else
  begin
    SetupGDIPen;
    SetupGDIBrush;
    FCtx.Canvas.Ellipse(P1.X, P1.Y, P2.X, P2.Y);
  end;
end;

procedure TCnSVGRenderer.RenderEllipse(AElement: TCnXMLElement);
var
  CX, CY, RX, RY: TCnSVGFloat;
  P1, P2: TPoint;
  FillAlpha, StrokeAlpha: TCnSVGFloat;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  CX := SVGAttrFloat(AElement, 'cx', 0);
  CY := SVGAttrFloat(AElement, 'cy', 0);
  RX := SVGAttrFloat(AElement, 'rx', -1);
  RY := SVGAttrFloat(AElement, 'ry', -1);
  if (RX <= 0) or (RY <= 0) then Exit;
  P1 := UserToScreen(CX - RX, CY - RY);
  P2 := UserToScreen(CX + RX, CY + RY);
  FillAlpha := EffectiveFillAlpha;
  StrokeAlpha := EffectiveStrokeAlpha;
  if (FillAlpha < 1.0) or (StrokeAlpha < 1.0) then
    RenderEllipseAlpha(Rect(P1.X, P1.Y, P2.X, P2.Y))
  else
  begin
    SetupGDIPen;
    SetupGDIBrush;
    FCtx.Canvas.Ellipse(P1.X, P1.Y, P2.X, P2.Y);
  end;
end;

procedure TCnSVGRenderer.RenderLine(AElement: TCnXMLElement);
var
  X1, Y1, X2, Y2: TCnSVGFloat;
  P1, P2: TPoint;
  StrokePts: array[0..1] of TPoint;
  PolyCounts: array[0..0] of Integer;
  StrokeAlpha: TCnSVGFloat;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  X1 := SVGAttrFloat(AElement, 'x1', 0);
  Y1 := SVGAttrFloat(AElement, 'y1', 0);
  X2 := SVGAttrFloat(AElement, 'x2', 0);
  Y2 := SVGAttrFloat(AElement, 'y2', 0);
  P1 := UserToScreen(X1, Y1);
  P2 := UserToScreen(X2, Y2);
  StrokeAlpha := EffectiveStrokeAlpha;
  if FCtx.Style.StrokeNone or (StrokeAlpha <= 0) then
    Exit;
  StrokePts[0] := P1;
  StrokePts[1] := P2;
  PolyCounts[0] := 2;
  if StrokeAlpha < 1.0 then
    StrokePolyPointsWithAlpha(FCtx.Canvas, StrokePts, PolyCounts, 1,
      FCtx.Style.StrokeColor, StrokeAlpha)
  else
  begin
    SetupGDIPen;
    FCtx.Canvas.Brush.Style := bsClear;
    FCtx.Canvas.MoveTo(P1.X, P1.Y);
    FCtx.Canvas.LineTo(P2.X, P2.Y);
  end;
end;

procedure TCnSVGRenderer.RenderPolyline(AElement: TCnXMLElement);
var
  PointsStr: string;
  I, J, Count: Integer;
  Coords: array of TCnSVGFloat;
  ScreenPts: array of TPoint;
  NumStr: string;
  V: Extended;
  PolyCounts: array[0..0] of Integer;
  StrokeAlpha: TCnSVGFloat;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  PointsStr := AElement.GetAttribute('points');
  if Trim(PointsStr) = '' then Exit;
  // replace commas and line breaks with spaces
  for I := 1 to Length(PointsStr) do
    if PointsStr[I] in [',', #9, #10, #13] then
      PointsStr[I] := ' ';
  // parse numbers
  SetLength(Coords, 0);
  I := 1;
  while I <= Length(PointsStr) do
  begin
    // skip spaces
    while (I <= Length(PointsStr)) and (PointsStr[I] = ' ') do Inc(I);
    if I > Length(PointsStr) then Break;
    // find end of token
    J := I;
    if PointsStr[J] in ['+', '-'] then Inc(J);
    while (J <= Length(PointsStr)) and (PointsStr[J] in ['0'..'9', '.', 'e', 'E', '+', '-']) do Inc(J);
    NumStr := Copy(PointsStr, I, J - I);
    I := J;
    if NumStr = '' then Continue;
    try
      V := StrToFloat(NumStr);
      SetLength(Coords, Length(Coords) + 1);
      Coords[High(Coords)] := V;
    except end;
  end;
  Count := Length(Coords) div 2;
  if Count < 2 then Exit;
  SetLength(ScreenPts, Count);
  for I := 0 to Count - 1 do
    ScreenPts[I] := UserToScreen(Coords[I * 2], Coords[I * 2 + 1]);
  StrokeAlpha := EffectiveStrokeAlpha;
  if FCtx.Style.StrokeNone or (StrokeAlpha <= 0) then
    Exit;
  PolyCounts[0] := Count;
  if StrokeAlpha < 1.0 then
    StrokePolyPointsWithAlpha(FCtx.Canvas, ScreenPts, PolyCounts, 1,
      FCtx.Style.StrokeColor, StrokeAlpha)
  else
  begin
    SetupGDIPen;
    FCtx.Canvas.Brush.Style := bsClear;
    FCtx.Canvas.Polyline(ScreenPts);
  end;
end;

procedure TCnSVGRenderer.RenderPolygon(AElement: TCnXMLElement);
var
  PointsStr: string;
  I, J, Count: Integer;
  Coords: array of TCnSVGFloat;
  ScreenPts: array of TPoint;
  ClosedPts: array of TPoint;
  FillPts: array of TPoint;
  NumStr: string;
  V: Extended;
  FillCounts: array[0..0] of Integer;
  StrokeCounts: array[0..0] of Integer;
  FillAlpha, StrokeAlpha: TCnSVGFloat;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  PointsStr := AElement.GetAttribute('points');
  if Trim(PointsStr) = '' then Exit;
  for I := 1 to Length(PointsStr) do
    if PointsStr[I] in [',', #9, #10, #13] then
      PointsStr[I] := ' ';
  SetLength(Coords, 0);
  I := 1;
  while I <= Length(PointsStr) do
  begin
    while (I <= Length(PointsStr)) and (PointsStr[I] = ' ') do Inc(I);
    if I > Length(PointsStr) then Break;
    J := I;
    if PointsStr[J] in ['+', '-'] then Inc(J);
    while (J <= Length(PointsStr)) and (PointsStr[J] in ['0'..'9', '.', 'e', 'E', '+', '-']) do Inc(J);
    NumStr := Copy(PointsStr, I, J - I);
    I := J;
    if NumStr = '' then Continue;
    try
      V := StrToFloat(NumStr);
      SetLength(Coords, Length(Coords) + 1);
      Coords[High(Coords)] := V;
    except end;
  end;
  Count := Length(Coords) div 2;
  if Count < 3 then Exit;
  SetLength(ScreenPts, Count);
  for I := 0 to Count - 1 do
    ScreenPts[I] := UserToScreen(Coords[I * 2], Coords[I * 2 + 1]);
  FillAlpha := EffectiveFillAlpha;
  StrokeAlpha := EffectiveStrokeAlpha;

  if (not FCtx.Style.FillNone) and (FillAlpha > 0) then
  begin
    SetLength(FillPts, Count);
    for I := 0 to Count - 1 do
      FillPts[I] := ScreenPts[I];
    FillCounts[0] := Count;
    if FillAlpha < 1.0 then
      FillPolyPolygonWithAlpha(FCtx.Canvas, FillPts, FillCounts, 1,
        FCtx.Style.FillColor, FillAlpha, FCtx.Style.FillRule)
    else
    begin
      SetupGDIBrush;
      FCtx.Canvas.Pen.Style := psClear;
      if FCtx.Style.FillRule = sfrEvenOdd then
        SetPolyFillMode(FCtx.Canvas.Handle, ALTERNATE)
      else
        SetPolyFillMode(FCtx.Canvas.Handle, WINDING);
      Windows.Polygon(FCtx.Canvas.Handle, FillPts[0], Count);
    end;
  end;

  if (not FCtx.Style.StrokeNone) and (StrokeAlpha > 0) then
  begin
    SetLength(ClosedPts, Count + 1);
    for I := 0 to Count - 1 do
      ClosedPts[I] := ScreenPts[I];
    ClosedPts[Count] := ScreenPts[0];
    StrokeCounts[0] := Count + 1;
    if StrokeAlpha < 1.0 then
      StrokePolyPointsWithAlpha(FCtx.Canvas, ClosedPts, StrokeCounts, 1,
        FCtx.Style.StrokeColor, StrokeAlpha)
    else
    begin
      SetupGDIPen;
      FCtx.Canvas.Brush.Style := bsClear;
      Windows.Polyline(FCtx.Canvas.Handle, ClosedPts[0], Count + 1);
    end;
  end;
end;


//==============================================================================
// Path rendering helpers (unit-level, not in class)
//==============================================================================

procedure SVGSubdivideCubic(
  P0X, P0Y, P1X, P1Y, P2X, P2Y, P3X, P3Y: Extended;
  const M: TCnSVGMatrix; OutPts: TList; Depth: Integer);
{* De Casteljau 三次贝塞尔递归细分，输出 TPoint 指针至 OutPts }
var
  SP0X, SP0Y, SP1X, SP1Y, SP2X, SP2Y, SP3X, SP3Y: TCnSVGFloat;
  MinX, MaxX, MinY, MaxY, Diag: Extended;
  M01X, M01Y, M12X, M12Y, M23X, M23Y: Extended;
  M012X, M012Y, M123X, M123Y, M0123X, M0123Y: Extended;
  Pt: PPoint;
begin
  if Depth > 20 then
  begin
    New(Pt);
    SP3X := P3X; SP3Y := P3Y; SVGMatrixTransformPoint(M, SP3X, SP3Y);
    Pt^.X := Round(SP3X); Pt^.Y := Round(SP3Y); OutPts.Add(Pt);
    Exit;
  end;
  SP0X := P0X; SP0Y := P0Y; SVGMatrixTransformPoint(M, SP0X, SP0Y);
  SP1X := P1X; SP1Y := P1Y; SVGMatrixTransformPoint(M, SP1X, SP1Y);
  SP2X := P2X; SP2Y := P2Y; SVGMatrixTransformPoint(M, SP2X, SP2Y);
  SP3X := P3X; SP3Y := P3Y; SVGMatrixTransformPoint(M, SP3X, SP3Y);
  MinX := SP0X; MaxX := SP0X;
  if SP1X < MinX then MinX := SP1X; if SP1X > MaxX then MaxX := SP1X;
  if SP2X < MinX then MinX := SP2X; if SP2X > MaxX then MaxX := SP2X;
  if SP3X < MinX then MinX := SP3X; if SP3X > MaxX then MaxX := SP3X;
  MinY := SP0Y; MaxY := SP0Y;
  if SP1Y < MinY then MinY := SP1Y; if SP1Y > MaxY then MaxY := SP1Y;
  if SP2Y < MinY then MinY := SP2Y; if SP2Y > MaxY then MaxY := SP2Y;
  if SP3Y < MinY then MinY := SP3Y; if SP3Y > MaxY then MaxY := SP3Y;
  Diag := Sqrt((MaxX - MinX) * (MaxX - MinX) + (MaxY - MinY) * (MaxY - MinY));
  if Diag < 0.5 then
  begin
    New(Pt); Pt^.X := Round(SP3X); Pt^.Y := Round(SP3Y); OutPts.Add(Pt); Exit;
  end;
  M01X  := (P0X + P1X) / 2;    M01Y  := (P0Y + P1Y) / 2;
  M12X  := (P1X + P2X) / 2;    M12Y  := (P1Y + P2Y) / 2;
  M23X  := (P2X + P3X) / 2;    M23Y  := (P2Y + P3Y) / 2;
  M012X := (M01X + M12X) / 2;  M012Y := (M01Y + M12Y) / 2;
  M123X := (M12X + M23X) / 2;  M123Y := (M12Y + M23Y) / 2;
  M0123X := (M012X + M123X) / 2; M0123Y := (M012Y + M123Y) / 2;
  SVGSubdivideCubic(P0X, P0Y, M01X, M01Y, M012X, M012Y, M0123X, M0123Y, M, OutPts, Depth + 1);
  SVGSubdivideCubic(M0123X, M0123Y, M123X, M123Y, M23X, M23Y, P3X, P3Y, M, OutPts, Depth + 1);
end;

procedure SVGSubdivideQuad(
  P0X, P0Y, P1X, P1Y, P2X, P2Y: Extended;
  const M: TCnSVGMatrix; OutPts: TList; Depth: Integer);
{* De Casteljau 二次贝塞尔递归细分 }
var
  SP0X, SP0Y, SP1X, SP1Y, SP2X, SP2Y: TCnSVGFloat;
  MinX, MaxX, MinY, MaxY, Diag: Extended;
  M01X, M01Y, M12X, M12Y, M012X, M012Y: Extended;
  Pt: PPoint;
begin
  if Depth > 20 then
  begin
    New(Pt); SP2X := P2X; SP2Y := P2Y; SVGMatrixTransformPoint(M, SP2X, SP2Y);
    Pt^.X := Round(SP2X); Pt^.Y := Round(SP2Y); OutPts.Add(Pt); Exit;
  end;
  SP0X := P0X; SP0Y := P0Y; SVGMatrixTransformPoint(M, SP0X, SP0Y);
  SP1X := P1X; SP1Y := P1Y; SVGMatrixTransformPoint(M, SP1X, SP1Y);
  SP2X := P2X; SP2Y := P2Y; SVGMatrixTransformPoint(M, SP2X, SP2Y);
  MinX := SP0X; MaxX := SP0X;
  if SP1X < MinX then MinX := SP1X; if SP1X > MaxX then MaxX := SP1X;
  if SP2X < MinX then MinX := SP2X; if SP2X > MaxX then MaxX := SP2X;
  MinY := SP0Y; MaxY := SP0Y;
  if SP1Y < MinY then MinY := SP1Y; if SP1Y > MaxY then MaxY := SP1Y;
  if SP2Y < MinY then MinY := SP2Y; if SP2Y > MaxY then MaxY := SP2Y;
  Diag := Sqrt((MaxX - MinX) * (MaxX - MinX) + (MaxY - MinY) * (MaxY - MinY));
  if Diag < 0.5 then
  begin
    New(Pt); Pt^.X := Round(SP2X); Pt^.Y := Round(SP2Y); OutPts.Add(Pt); Exit;
  end;
  M01X  := (P0X + P1X) / 2; M01Y  := (P0Y + P1Y) / 2;
  M12X  := (P1X + P2X) / 2; M12Y  := (P1Y + P2Y) / 2;
  M012X := (M01X + M12X) / 2; M012Y := (M01Y + M12Y) / 2;
  SVGSubdivideQuad(P0X, P0Y, M01X, M01Y, M012X, M012Y, M, OutPts, Depth + 1);
  SVGSubdivideQuad(M012X, M012Y, M12X, M12Y, P2X, P2Y, M, OutPts, Depth + 1);
end;

function SVGAngleBetween(UX, UY, VX, VY: Extended): Extended;
{* 计算两向量之间的有符号角度（弧度） }
var
  Dot, Cross, D: Extended;
begin
  D := Sqrt((UX * UX + UY * UY) * (VX * VX + VY * VY));
  if D < 1e-10 then begin Result := 0; Exit; end;
  Dot := UX * VX + UY * VY;
  Cross := UX * VY - UY * VX;
  if Dot / D > 1.0 then Dot := D;
  if Dot / D < -1.0 then Dot := -D;
  Result := ArcCos(Dot / D);
  if Cross < 0 then Result := -Result;
end;

procedure SVGArcToPoints(
  X1, Y1, X2, Y2, RX, RY, PhiDeg: Extended;
  LargeArc, Sweep: Boolean;
  const M: TCnSVGMatrix; OutPts: TList);
{* SVG 圆弧端点参数→中心参数转换（Appendix F.6），以 1° 步长线性化输出点 }
var
  PhiRad, CosP, SinP: Extended;
  DX2, DY2, X1P, Y1P, Lambda: Extended;
  Num, Den, Sq: Extended;
  CXP, CYP, CX, CY: Extended;
  Theta1, DTheta, Angle: Extended;
  UX, UY, VX, VY: Extended;
  StepCount, I: Integer;
  PX, PY, TX, TY: TCnSVGFloat;
  Pt: PPoint;
begin
  if (Abs(RX) < 1e-6) or (Abs(RY) < 1e-6) then
  begin
    TX := X2; TY := Y2; SVGMatrixTransformPoint(M, TX, TY);
    New(Pt); Pt^.X := Round(TX); Pt^.Y := Round(TY); OutPts.Add(Pt);
    Exit;
  end;
  RX := Abs(RX); RY := Abs(RY);
  PhiRad := PhiDeg * Pi / 180;
  CosP := Cos(PhiRad); SinP := Sin(PhiRad);
  DX2 := (X1 - X2) / 2; DY2 := (Y1 - Y2) / 2;
  X1P :=  CosP * DX2 + SinP * DY2;
  Y1P := -SinP * DX2 + CosP * DY2;
  Lambda := (X1P / RX) * (X1P / RX) + (Y1P / RY) * (Y1P / RY);
  if Lambda > 1 then
  begin
    Lambda := Sqrt(Lambda);
    RX := RX * Lambda; RY := RY * Lambda;
  end;
  Num := (RX * RY) * (RX * RY) - (RX * Y1P) * (RX * Y1P) - (RY * X1P) * (RY * X1P);
  Den := (RX * Y1P) * (RX * Y1P) + (RY * X1P) * (RY * X1P);
  if Den < 1e-10 then Sq := 0
  else begin Sq := Num / Den; if Sq < 0 then Sq := 0; Sq := Sqrt(Sq); end;
  if LargeArc = Sweep then Sq := -Sq;
  CXP :=  Sq * RX * Y1P / RY;
  CYP := -Sq * RY * X1P / RX;
  CX := CosP * CXP - SinP * CYP + (X1 + X2) / 2;
  CY := SinP * CXP + CosP * CYP + (Y1 + Y2) / 2;
  UX := (X1P - CXP) / RX; UY := (Y1P - CYP) / RY;
  VX := (-X1P - CXP) / RX; VY := (-Y1P - CYP) / RY;
  Theta1 := SVGAngleBetween(1, 0, UX, UY);
  DTheta  := SVGAngleBetween(UX, UY, VX, VY);
  if (not Sweep) and (DTheta > 0) then DTheta := DTheta - 2 * Pi;
  if Sweep and (DTheta < 0) then DTheta := DTheta + 2 * Pi;
  if Abs(DTheta) < 1e-10 then StepCount := 1
  else StepCount := Ceil(Abs(DTheta) * 180 / Pi);
  if StepCount < 1 then StepCount := 1;
  for I := 1 to StepCount do
  begin
    Angle := Theta1 + DTheta * I / StepCount;
    PX := CX + RX * Cos(Angle) * CosP - RY * Sin(Angle) * SinP;
    PY := CY + RX * Cos(Angle) * SinP + RY * Sin(Angle) * CosP;
    TX := PX; TY := PY; SVGMatrixTransformPoint(M, TX, TY);
    New(Pt); Pt^.X := Round(TX); Pt^.Y := Round(TY); OutPts.Add(Pt);
  end;
end;

procedure TCnSVGRenderer.RenderPath(AElement: TCnXMLElement);
var
  D: string;
  Parser: TCnSVGPathParser;
  Segs: TList;
  SubPaths: TList;   // list of TList (each subpath = list of PPoint)
  CurSub: TList;
  I, J, K, TotalPts: Integer;
  Seg: PCnSVGPathSeg;
  CurX, CurY, StartX, StartY: Extended;
  TX, TY: TCnSVGFloat;
  Pt: PPoint;
  SubList: TList;
  ScreenPts: array of TPoint;
  PolyCounts: array of Integer;
  FillAlpha, StrokeAlpha: TCnSVGFloat;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  D := Trim(AElement.GetAttribute('d'));
  if D = '' then Exit;

  Parser   := TCnSVGPathParser.Create;
  SubPaths := TList.Create;
  CurSub   := nil;
  CurX := 0; CurY := 0; StartX := 0; StartY := 0;
  Segs := nil;

  try
    Segs := Parser.ParsePathData(D);

    for I := 0 to Segs.Count - 1 do
    begin
      Seg := PCnSVGPathSeg(Segs[I]);
      case Seg^.SegType of
        pstMoveTo:
        begin
          if (CurSub <> nil) and (CurSub.Count > 0) then
            SubPaths.Add(CurSub)
          else if CurSub <> nil then
            CurSub.Free;
          CurSub := TList.Create;
          CurX := Seg^.X; CurY := Seg^.Y;
          StartX := CurX; StartY := CurY;
          TX := CurX; TY := CurY;
          SVGMatrixTransformPoint(FCtx.CTM, TX, TY);
          New(Pt); Pt^.X := Round(TX); Pt^.Y := Round(TY);
          CurSub.Add(Pt);
        end;
        pstLineTo, pstHLineTo, pstVLineTo:
        begin
          if CurSub = nil then CurSub := TList.Create;
          CurX := Seg^.X; CurY := Seg^.Y;
          TX := CurX; TY := CurY;
          SVGMatrixTransformPoint(FCtx.CTM, TX, TY);
          New(Pt); Pt^.X := Round(TX); Pt^.Y := Round(TY);
          CurSub.Add(Pt);
        end;
        pstCubicBezier, pstSmoothCubic:
        begin
          if CurSub = nil then CurSub := TList.Create;
          SVGSubdivideCubic(CurX, CurY, Seg^.X1, Seg^.Y1,
            Seg^.X2, Seg^.Y2, Seg^.X, Seg^.Y, FCtx.CTM, CurSub, 0);
          CurX := Seg^.X; CurY := Seg^.Y;
        end;
        pstQuadBezier, pstSmoothQuad:
        begin
          if CurSub = nil then CurSub := TList.Create;
          SVGSubdivideQuad(CurX, CurY, Seg^.X1, Seg^.Y1,
            Seg^.X, Seg^.Y, FCtx.CTM, CurSub, 0);
          CurX := Seg^.X; CurY := Seg^.Y;
        end;
        pstArc:
        begin
          if CurSub = nil then CurSub := TList.Create;
          SVGArcToPoints(CurX, CurY, Seg^.X, Seg^.Y,
            Seg^.RX, Seg^.RY, Seg^.XRotation,
            Seg^.LargeArc, Seg^.Sweep, FCtx.CTM, CurSub);
          CurX := Seg^.X; CurY := Seg^.Y;
        end;
        pstClosePath:
        begin
          if CurSub = nil then CurSub := TList.Create;
          // add start point to close the subpath
          TX := StartX; TY := StartY;
          SVGMatrixTransformPoint(FCtx.CTM, TX, TY);
          New(Pt); Pt^.X := Round(TX); Pt^.Y := Round(TY);
          CurSub.Add(Pt);
          SubPaths.Add(CurSub);
          CurSub := nil;
          CurX := StartX; CurY := StartY;
        end;
      end;
    end;
    // save last open subpath
    if (CurSub <> nil) and (CurSub.Count > 0) then
      SubPaths.Add(CurSub)
    else if CurSub <> nil then
      CurSub.Free;

    FillAlpha := EffectiveFillAlpha;
    StrokeAlpha := EffectiveStrokeAlpha;

    // --- FILL ---
    if (not FCtx.Style.FillNone) and (FillAlpha > 0) and (SubPaths.Count > 0) then
    begin
      // Build flat ScreenPts + PolyCounts for PolyPolygon
      TotalPts := 0;
      for I := 0 to SubPaths.Count - 1 do
        Inc(TotalPts, TList(SubPaths[I]).Count);
      SetLength(ScreenPts, TotalPts);
      SetLength(PolyCounts, SubPaths.Count);
      K := 0;
      for I := 0 to SubPaths.Count - 1 do
      begin
        SubList := TList(SubPaths[I]);
        PolyCounts[I] := SubList.Count;
        for J := 0 to SubList.Count - 1 do
        begin
          ScreenPts[K] := PPoint(SubList[J])^;
          Inc(K);
        end;
      end;
      if TotalPts > 0 then
      begin
        if FillAlpha < 1.0 then
          FillPolyPolygonWithAlpha(FCtx.Canvas, ScreenPts, PolyCounts, SubPaths.Count,
            FCtx.Style.FillColor, FillAlpha, FCtx.Style.FillRule)
        else
        begin
          SetupGDIBrush;
          FCtx.Canvas.Pen.Style := psClear;
          if FCtx.Style.FillRule = sfrEvenOdd then
            SetPolyFillMode(FCtx.Canvas.Handle, ALTERNATE)
          else
            SetPolyFillMode(FCtx.Canvas.Handle, WINDING);
          Windows.PolyPolygon(FCtx.Canvas.Handle, ScreenPts[0],
            PolyCounts[0], SubPaths.Count);
        end;
      end;
    end;

    // --- STROKE ---
    if (not FCtx.Style.StrokeNone) and (StrokeAlpha > 0) then
    begin
      TotalPts := 0;
      for I := 0 to SubPaths.Count - 1 do
        Inc(TotalPts, TList(SubPaths[I]).Count);
      SetLength(ScreenPts, TotalPts);
      SetLength(PolyCounts, SubPaths.Count);
      K := 0;
      for I := 0 to SubPaths.Count - 1 do
      begin
        SubList := TList(SubPaths[I]);
        PolyCounts[I] := SubList.Count;
        for J := 0 to SubList.Count - 1 do
        begin
          ScreenPts[K] := PPoint(SubList[J])^;
          Inc(K);
        end;
      end;

      if StrokeAlpha < 1.0 then
        StrokePolyPointsWithAlpha(FCtx.Canvas, ScreenPts, PolyCounts, SubPaths.Count,
          FCtx.Style.StrokeColor, StrokeAlpha)
      else
      begin
        SetupGDIPen;
        FCtx.Canvas.Brush.Style := bsClear;
        K := 0;
        for I := 0 to SubPaths.Count - 1 do
        begin
          if PolyCounts[I] >= 2 then
            Windows.Polyline(FCtx.Canvas.Handle, ScreenPts[K], PolyCounts[I]);
          Inc(K, PolyCounts[I]);
        end;
      end;
    end;

  finally
    if Segs <> nil then
    begin
      for I := 0 to Segs.Count - 1 do
        Dispose(PCnSVGPathSeg(Segs[I]));
      Segs.Free;
    end;
    for I := 0 to SubPaths.Count - 1 do
    begin
      SubList := TList(SubPaths[I]);
      for J := 0 to SubList.Count - 1 do
        Dispose(PPoint(SubList[J]));
      SubList.Free;
    end;
    SubPaths.Free;
    Parser.Free;
  end;
end;

procedure TCnSVGRenderer.RenderText(AElement: TCnXMLElement);
var
  X, Y, DX, DY, FontSzPx: TCnSVGFloat;
  TextContent: string;
  I: Integer;
  Child: TCnXMLNode;
  TSpan: TCnXMLElement;
  CurX, CurY: TCnSVGFloat;
  OldFont: TFont;
  OldBrushStyle: TBrushStyle;

  procedure ApplyCanvasFontFromStyle(const AStyle: TCnSVGStyle);
  var
    LocalFontName: string;
    P: Integer;
  begin
    FontSzPx := Abs(AStyle.FontSize * FCtx.CTM.a);
    if FontSzPx < 6 then
      FontSzPx := 6;
    FCtx.Canvas.Font.Size := -Round(FontSzPx);

    LocalFontName := AStyle.FontFamily;
    if LocalFontName <> '' then
    begin
      P := Pos(',', LocalFontName);
      if P > 0 then
        LocalFontName := Trim(Copy(LocalFontName, 1, P - 1));
      if (Length(LocalFontName) >= 2) and
         (((LocalFontName[1] = '''') and (LocalFontName[Length(LocalFontName)] = '''')) or
          ((LocalFontName[1] = '"') and (LocalFontName[Length(LocalFontName)] = '"'))) then
        LocalFontName := Copy(LocalFontName, 2, Length(LocalFontName) - 2);
      if LocalFontName <> '' then
        FCtx.Canvas.Font.Name := LocalFontName;
    end;

    FCtx.Canvas.Font.Style := [];
    if AStyle.FontBold then
      FCtx.Canvas.Font.Style := FCtx.Canvas.Font.Style + [fsBold];
    if AStyle.FontItalic then
      FCtx.Canvas.Font.Style := FCtx.Canvas.Font.Style + [fsItalic];

    if not AStyle.FillNone then
      FCtx.Canvas.Font.Color :=
        Windows.RGB(AStyle.FillColor.R, AStyle.FillColor.G, AStyle.FillColor.B);
  end;

  procedure DrawTextAt(const S: string; AX, AY: TCnSVGFloat; const AStyle: TCnSVGStyle);
  var
    Scr: TPoint;
    TW, TH, BX: Integer;
  begin
    if S = '' then Exit;
    ApplyCanvasFontFromStyle(AStyle);
    Scr := UserToScreen(AX, AY);
    TW  := FCtx.Canvas.TextWidth(S);
    TH  := FCtx.Canvas.TextHeight(S);
    // SVG y = baseline; GDI origin = top-left → shift up ~80% of height
    case AStyle.TextAnchor of
      staMiddle: BX := Scr.X - TW div 2;
      staEnd:    BX := Scr.X - TW;
    else         BX := Scr.X; // staStart
    end;
    FCtx.Canvas.TextOut(BX, Scr.Y - Round(TH * 0.8), S);
  end;

  function CalcAdvanceUser(const S: string): TCnSVGFloat;
  begin
    Result := 0;
    if S = '' then
      Exit;
    if Abs(FCtx.CTM.a) > 1e-6 then
      Result := FCtx.Canvas.TextWidth(S) / Abs(FCtx.CTM.a);
  end;

begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then Exit;
  X    := SVGAttrFloat(AElement, 'x', 0);
  Y    := SVGAttrFloat(AElement, 'y', 0);
  CurX := X; CurY := Y;

  OldBrushStyle := FCtx.Canvas.Brush.Style;
  OldFont := nil;
  try
    OldFont := TFont.Create;
    OldFont.Assign(FCtx.Canvas.Font);
    FCtx.Canvas.Brush.Style := bsClear;

    // iterate children: text nodes and <tspan> elements
    for I := 0 to AElement.ChildCount - 1 do
    begin
      Child := AElement.Children[I];
      if Child.NodeType = xntText then
      begin
        TextContent := Trim(Child.NodeValue);
        if TextContent <> '' then
        begin
          DrawTextAt(TextContent, CurX, CurY, FCtx.Style);
          CurX := CurX + CalcAdvanceUser(TextContent);
        end;
      end
      else if (Child.NodeType = xntElement) and
              (LowerCase(TCnXMLElement(Child).TagName) = 'tspan') then
      begin
        TSpan := TCnXMLElement(Child);
        PushStyle;
        try
          ApplyStyleAttr(TSpan);
          if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then
            Continue;
          if TSpan.HasAttribute('x') then CurX := SVGAttrFloat(TSpan, 'x', CurX);
          if TSpan.HasAttribute('y') then CurY := SVGAttrFloat(TSpan, 'y', CurY);
          DX := SVGAttrFloat(TSpan, 'dx', 0);
          DY := SVGAttrFloat(TSpan, 'dy', 0);
          CurX := CurX + DX;
          CurY := CurY + DY;
          TextContent := Trim(TSpan.Text);
          if TextContent <> '' then
          begin
            DrawTextAt(TextContent, CurX, CurY, FCtx.Style);
            CurX := CurX + CalcAdvanceUser(TextContent);
          end;
        finally
          PopStyle;
        end;
      end;
    end;

  finally
    FCtx.Canvas.Brush.Style := OldBrushStyle;
    FCtx.Canvas.Font.Assign(OldFont);
    OldFont.Free;
  end;
end;

procedure TCnSVGRenderer.RenderGroup(AElement: TCnXMLElement);
var
  I: Integer;
  HasTransform: Boolean;
begin
  if FCtx.Style.DisplayNone then Exit;
  HasTransform := AElement.HasAttribute('transform');
  PushMatrix;
  PushStyle;
  if HasTransform then
    ApplyTransformAttr(AElement.GetAttribute('transform'));
  ApplyStyleAttr(AElement);
  for I := 0 to AElement.ChildCount - 1 do
    RenderNode(AElement.Children[I]);
  PopStyle;
  PopMatrix;
end;

procedure TCnSVGRenderer.RenderUse(AElement: TCnXMLElement);
var
  RefID, HRef: string;
  Target: TCnXMLElement;
  I: Integer;
  X, Y, W, H: TCnSVGFloat;
  LocalViewBox: TCnSVGRect;
  M: TCnSVGMatrix;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then
    Exit;
  if FCtx.UseDepth >= 8 then
    Exit;

  if AElement.HasAttribute('href') then
    HRef := Trim(AElement.GetAttribute('href'))
  else
    HRef := Trim(AElement.GetAttribute('xlink:href'));
  if (HRef = '') or (HRef[1] <> '#') then
    Exit;
  RefID := Copy(HRef, 2, Length(HRef) - 1);
  Target := SVGFindDefNode(FDefsMap, RefID);
  if Target = nil then
    Exit;

  PushMatrix;
  Inc(FCtx.UseDepth);
  try
    X := SVGAttrFloat(AElement, 'x', 0);
    Y := SVGAttrFloat(AElement, 'y', 0);
    SVGMatrixTranslate(FCtx.CTM, X, Y);

    if LowerCase(Target.TagName) = 'symbol' then
    begin
      W := SVGAttrFloat(AElement, 'width', 0);
      H := SVGAttrFloat(AElement, 'height', 0);
      if (W > 0) and (H > 0) and Target.HasAttribute('viewBox') and
        SVGParseViewBoxValue(Target.GetAttribute('viewBox'), LocalViewBox) then
      begin
        SVGCalcNestedViewMatrix(0, 0, W, H, LocalViewBox,
          Target.GetAttribute('preserveAspectRatio'), M);
        SVGMatrixMultiply(FCtx.CTM, FCtx.CTM, M);
      end;
      for I := 0 to Target.ChildCount - 1 do
        RenderNode(Target.Children[I]);
    end
    else
      RenderElement(Target);
  finally
    Dec(FCtx.UseDepth);
    PopMatrix;
  end;
end;

procedure TCnSVGRenderer.RenderDefs(AElement: TCnXMLElement);
begin
  // <defs> 在加载阶段已建表；运行时不直接绘制
end;

procedure TCnSVGRenderer.RenderImage(AElement: TCnXMLElement);
var
  HRef, FileName, PreserveAR: string;
  X, Y, W, H: TCnSVGFloat;
  P1, P2: TPoint;
  R, BoundsRect: TRect;
  Pic: TPicture;
  ImgW, ImgH: Integer;
  Tmp: Integer;
  NeedClip: Boolean;
  SavedDC: Integer;
begin
  if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then
    Exit;

  W := SVGAttrFloat(AElement, 'width', 0);
  H := SVGAttrFloat(AElement, 'height', 0);
  if (W <= 0) or (H <= 0) then
    Exit;
  X := SVGAttrFloat(AElement, 'x', 0);
  Y := SVGAttrFloat(AElement, 'y', 0);

  if AElement.HasAttribute('href') then
    HRef := AElement.GetAttribute('href')
  else
    HRef := AElement.GetAttribute('xlink:href');
  FileName := SVGResolveLocalFileName(FBasePath, HRef);
  if (FileName = '') or (not FileExists(FileName)) then
    Exit;

  Pic := TPicture.Create;
  try
    try
      Pic.LoadFromFile(FileName);
    except
      Exit;
    end;
    if Pic.Graphic = nil then
      Exit;

    P1 := UserToScreen(X, Y);
    P2 := UserToScreen(X + W, Y + H);
    R := Rect(P1.X, P1.Y, P2.X, P2.Y);
    if R.Left > R.Right then
    begin
      Tmp := R.Left;
      R.Left := R.Right;
      R.Right := Tmp;
    end;
    if R.Top > R.Bottom then
    begin
      Tmp := R.Top;
      R.Top := R.Bottom;
      R.Bottom := Tmp;
    end;
    BoundsRect := R;

    PreserveAR := AElement.GetAttribute('preserveAspectRatio');
    if PreserveAR = '' then
      PreserveAR := 'xMidYMid meet';
    ImgW := Pic.Width;
    ImgH := Pic.Height;
    R := SVGCalcAlignedRect(R, ImgW, ImgH, PreserveAR, NeedClip);
    if NeedClip then
    begin
      SavedDC := SaveDC(FCtx.Canvas.Handle);
      try
        IntersectClipRect(FCtx.Canvas.Handle, BoundsRect.Left, BoundsRect.Top,
          BoundsRect.Right, BoundsRect.Bottom);
        FCtx.Canvas.StretchDraw(R, Pic.Graphic);
      finally
        RestoreDC(FCtx.Canvas.Handle, SavedDC);
      end;
    end;
    if not NeedClip then
      FCtx.Canvas.StretchDraw(R, Pic.Graphic);
  finally
    Pic.Free;
  end;
end;

procedure TCnSVGRenderer.RenderSwitch(AElement: TCnXMLElement);
var
  I: Integer;
begin
  for I := 0 to AElement.ChildCount - 1 do
    if AElement.Children[I].NodeType = xntElement then
    begin
      RenderNode(AElement.Children[I]);
      Break;
    end;
end;

procedure TCnSVGRenderer.RenderAnchor(AElement: TCnXMLElement);
begin
  RenderGroup(AElement);
end;

procedure TCnSVGRenderer.RenderNestedSVG(AElement: TCnXMLElement);
var
  X, Y, W, H: TCnSVGFloat;
  I: Integer;
  LocalViewBox: TCnSVGRect;
  M: TCnSVGMatrix;
  IsRoot: Boolean;
begin
  if FCtx.Style.DisplayNone then
    Exit;

  X := SVGAttrFloat(AElement, 'x', 0);
  Y := SVGAttrFloat(AElement, 'y', 0);
  W := SVGAttrFloat(AElement, 'width', 0);
  H := SVGAttrFloat(AElement, 'height', 0);
  if (W <= 0) or (H <= 0) then
    Exit;

  // 判断是否为根 <svg> 元素: 构造函数已通过 SVGCalcViewMatrix 处理了 viewBox 变换
  // 此时 FMatrixTop = 1（来自 RenderElement 的 PushMatrix）
  IsRoot := (FMatrixTop = 1);

  PushMatrix;
  try
    if IsRoot then
    begin
      // 根 <svg>：构造函数已将 FViewBox 映射到 ADestRect 并设入 FCtx.CTM，
      // 此处仅处理 x/y 偏移（通常为 0），避免 viewBox 被双重应用
      if (X <> 0) or (Y <> 0) then
        SVGMatrixTranslate(FCtx.CTM, X, Y);
    end
    else if AElement.HasAttribute('viewBox') and
      SVGParseViewBoxValue(AElement.GetAttribute('viewBox'), LocalViewBox) then
    begin
      SVGCalcNestedViewMatrix(X, Y, W, H, LocalViewBox,
        AElement.GetAttribute('preserveAspectRatio'), M);
      SVGMatrixMultiply(FCtx.CTM, FCtx.CTM, M);
    end
    else
      SVGMatrixTranslate(FCtx.CTM, X, Y);

    for I := 0 to AElement.ChildCount - 1 do
      RenderNode(AElement.Children[I]);
  finally
    PopMatrix;
  end;
end;

procedure TCnSVGRenderer.RenderElement(AElement: TCnXMLElement);
var
  Tag: string;
  I: Integer;
begin
  if AElement = nil then
    Exit;
  Tag := LowerCase(AElement.TagName);

  if (Tag = 'title') or (Tag = 'desc') or (Tag = 'metadata') then
    Exit;

  if Tag <> 'g' then
  begin
    PushStyle;
    PushMatrix;
    try
      ApplyStyleAttr(AElement);
      if FCtx.Style.DisplayNone or FCtx.Style.VisibilityHidden then
        Exit;
      if AElement.HasAttribute('transform') then
        ApplyTransformAttr(AElement.GetAttribute('transform'));

      if Tag = 'svg' then
        RenderNestedSVG(AElement)
      else if Tag = 'rect' then
        RenderRect(AElement)
      else if Tag = 'circle' then
        RenderCircle(AElement)
      else if Tag = 'ellipse' then
        RenderEllipse(AElement)
      else if Tag = 'line' then
        RenderLine(AElement)
      else if Tag = 'polyline' then
        RenderPolyline(AElement)
      else if Tag = 'polygon' then
        RenderPolygon(AElement)
      else if Tag = 'path' then
        RenderPath(AElement)
      else if Tag = 'text' then
        RenderText(AElement)
      else if Tag = 'use' then
        RenderUse(AElement)
      else if Tag = 'defs' then
        RenderDefs(AElement)
      else if Tag = 'image' then
        RenderImage(AElement)
      else if Tag = 'switch' then
        RenderSwitch(AElement)
      else if Tag = 'a' then
        RenderAnchor(AElement)
      else if Tag = 'symbol' then
      begin
        // symbol 仅定义，不直接渲染
      end
      else
        for I := 0 to AElement.ChildCount - 1 do
          RenderNode(AElement.Children[I]);
    finally
      PopMatrix;
      PopStyle;
    end;
  end
  else
    RenderGroup(AElement);
end;

procedure TCnSVGRenderer.RenderNode(ANode: TCnXMLNode);
begin
  if ANode = nil then
    Exit;
  if ANode.NodeType = xntElement then
    RenderElement(TCnXMLElement(ANode));
end;


//==============================================================================
// TCnSVGDocument 实现存根
//==============================================================================

constructor TCnSVGDocument.Create;
begin
  inherited Create;
  FXMLDoc         := nil;
  FIsLoaded       := False;
  FViewportWidth  := 100;
  FViewportHeight := 100;
  FViewBox.X      := 0;
  FViewBox.Y      := 0;
  FViewBox.Width  := 100;
  FViewBox.Height := 100;
  FDefsMap        := TStringList.Create;
end;

destructor TCnSVGDocument.Destroy;
begin
  Clear;
  FDefsMap.Free;
  inherited Destroy;
end;

procedure TCnSVGDocument.Clear;
begin
  if FXMLDoc <> nil then
  begin
    FXMLDoc.Free;
    FXMLDoc := nil;
  end;
  FDefsMap.Clear;
  FIsLoaded := False;
end;

procedure TCnSVGDocument.ParseViewport;
var
  Root: TCnXMLElement;
  VB: TCnSVGRect;
  HasVB: Boolean;
begin
  FViewportWidth := 100.0;
  FViewportHeight := 100.0;
  FViewBox.X := 0;
  FViewBox.Y := 0;
  FViewBox.Width := 100.0;
  FViewBox.Height := 100.0;

  if (FXMLDoc = nil) or (FXMLDoc.DocumentElement = nil) then
    Exit;
  Root := FXMLDoc.DocumentElement;

  HasVB := Root.HasAttribute('viewBox') and
    SVGParseViewBoxValue(Root.GetAttribute('viewBox'), VB);
  if Root.HasAttribute('width') then
    FViewportWidth := SVGParseLengthValue(Root.GetAttribute('width'), FViewportWidth);
  if Root.HasAttribute('height') then
    FViewportHeight := SVGParseLengthValue(Root.GetAttribute('height'), FViewportHeight);

  if HasVB then
  begin
    FViewBox := VB;
    if not Root.HasAttribute('width') then
      FViewportWidth := VB.Width;
    if not Root.HasAttribute('height') then
      FViewportHeight := VB.Height;
  end
  else
  begin
    FViewBox.X := 0;
    FViewBox.Y := 0;
    FViewBox.Width := FViewportWidth;
    FViewBox.Height := FViewportHeight;
  end;
end;

procedure TCnSVGDocument.BuildDefsMap;

  procedure ScanNode(ANode: TCnXMLNode);
  var
    I: Integer;
    El: TCnXMLElement;
    ID: string;
  begin
    if (ANode = nil) or (ANode.NodeType <> xntElement) then
      Exit;
    El := TCnXMLElement(ANode);
    if LowerCase(El.TagName) = 'defs' then
    begin
      for I := 0 to El.ChildCount - 1 do
        if El.Children[I].NodeType = xntElement then
        begin
          ID := TCnXMLElement(El.Children[I]).GetAttribute('id');
          if ID <> '' then
            if SVGFindDefNode(FDefsMap, ID) = nil then
              FDefsMap.AddObject(ID, TObject(El.Children[I]));
        end;
    end;
    if LowerCase(El.TagName) = 'symbol' then
    begin
      ID := El.GetAttribute('id');
      if (ID <> '') and (SVGFindDefNode(FDefsMap, ID) = nil) then
        FDefsMap.AddObject(ID, El);
    end;
    for I := 0 to El.ChildCount - 1 do
      ScanNode(El.Children[I]);
  end;

begin
  FDefsMap.Clear;
  if (FXMLDoc <> nil) and (FXMLDoc.DocumentElement <> nil) then
    ScanNode(FXMLDoc.DocumentElement);
end;

procedure TCnSVGDocument.DoLoad;
begin
  ParseViewport;
  BuildDefsMap;
  FIsLoaded := True;
end;

procedure TCnSVGDocument.LoadFromFile(const AFileName: string);
begin
  Clear;
  FSourceFileName := AFileName;
  if not FileExists(AFileName) then
    raise ECnSVGException.CreateFmt(SCnErrorSvgNotExistsFmt, [AFileName]);
  FXMLDoc := TCnXMLDocument.Create;
  try
    FXMLDoc.LoadFromFile(AFileName);
  except
    on E: Exception do
    begin
      FXMLDoc.Free;
      FXMLDoc := nil;
      raise ECnSVGException.CreateFmt(SCnErrorSvgXmlParseErrorFmt, [E.Message]);
    end;
  end;
  if (FXMLDoc.DocumentElement = nil) or
     (FXMLDoc.DocumentElement.TagName <> 'svg') then
  begin
    FXMLDoc.Free;
    FXMLDoc := nil;
    raise ECnSVGException.Create(SCnErrorSvgRootSvg);
  end;
  DoLoad;
end;

procedure TCnSVGDocument.LoadFromStream(AStream: TStream);
begin
  Clear;
  if AStream = nil then
    raise ECnSVGException.Create(SCnErrorSvgStreamNil);
  FSourceFileName := '';
  FXMLDoc := TCnXMLDocument.Create;
  try
    FXMLDoc.LoadFromStream(AStream);
  except
    on E: Exception do
    begin
      FXMLDoc.Free;
      FXMLDoc := nil;
      raise ECnSVGException.CreateFmt(SCnErrorSvgXmlParseErrorFmt, [E.Message]);
    end;
  end;
  if (FXMLDoc.DocumentElement = nil) or
     (FXMLDoc.DocumentElement.TagName <> 'svg') then
  begin
    FXMLDoc.Free;
    FXMLDoc := nil;
    raise ECnSVGException.Create(SCnErrorSvgRootSvg);
  end;
  DoLoad;
end;

procedure TCnSVGDocument.Render(ACanvas: TCanvas; const ADestRect: TRect);
var
  Renderer: TCnSVGRenderer;
  PreserveAR: string;
begin
  if not FIsLoaded then
    Exit;
  if (ADestRect.Right <= ADestRect.Left) or
     (ADestRect.Bottom <= ADestRect.Top) then
    Exit;
  if FXMLDoc = nil then
    Exit;

  if FXMLDoc.DocumentElement <> nil then
    PreserveAR := FXMLDoc.DocumentElement.GetAttribute('preserveAspectRatio')
  else
    PreserveAR := '';

  Renderer := TCnSVGRenderer.Create(ACanvas, ADestRect, FViewBox,
    FDefsMap, ExtractFilePath(FSourceFileName), PreserveAR);
  try
    Renderer.RenderNode(FXMLDoc.DocumentElement);
  finally
    Renderer.Free;
  end;
end;

procedure TCnSVGImage.Resize;
begin
  inherited Resize;
  Invalidate;
end;

//==============================================================================
// TCnSVGImage 实现存根
//==============================================================================

constructor TCnSVGImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument         := TCnSVGDocument.Create;
  FStretch          := True;
  FCenter           := False;
  FProportional     := True;
  FLastError        := '';
  FLastRenderTimeMs := 0;
end;

destructor TCnSVGImage.Destroy;
begin
  FDocument.Free;
  inherited Destroy;
end;

procedure TCnSVGImage.SetFileName(const Value: string);
begin
  if FFileName = Value then
    Exit;
  FFileName  := Value;
  FLastError := '';
  if Value = '' then
  begin
    FDocument.Clear;
  end
  else
  begin
    try
      FDocument.LoadFromFile(Value);
      FLastError := '';
    except
      on E: ECnSVGException do
      begin
        FDocument.Clear;
        FLastError := E.Message;
      end;
    end;
  end;
  Invalidate;
end;

procedure TCnSVGImage.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Invalidate;
  end;
end;

procedure TCnSVGImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TCnSVGImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    Invalidate;
  end;
end;

function TCnSVGImage.CalcDestRect: TRect;
var
  CR: TRect;
  VW, VH: Integer;
  ScaleX, ScaleY, Scale: Double;
  OffX, OffY: Integer;
begin
  CR := ClientRect;
  if FStretch then
  begin
    if FProportional and FDocument.IsLoaded then
    begin
      VW := Round(FDocument.ViewportWidth);
      VH := Round(FDocument.ViewportHeight);
      if (VW > 0) and (VH > 0) then
      begin
        ScaleX := (CR.Right - CR.Left) / VW;
        ScaleY := (CR.Bottom - CR.Top) / VH;
        if ScaleX < ScaleY then
          Scale := ScaleX
        else
          Scale := ScaleY;
        OffX   := CR.Left + Round(((CR.Right - CR.Left) - VW * Scale) / 2);
        OffY   := CR.Top  + Round(((CR.Bottom - CR.Top) - VH * Scale) / 2);
        Result.Left   := OffX;
        Result.Top    := OffY;
        Result.Right  := OffX + Round(VW * Scale);
        Result.Bottom := OffY + Round(VH * Scale);
      end
      else
        Result := CR;
    end
    else
      Result := CR;
  end
  else
  begin
    VW := Round(FDocument.ViewportWidth);
    VH := Round(FDocument.ViewportHeight);
    if FCenter then
    begin
      OffX := CR.Left + ((CR.Right - CR.Left) - VW) div 2;
      OffY := CR.Top  + ((CR.Bottom - CR.Top) - VH) div 2;
    end
    else
    begin
      OffX := CR.Left;
      OffY := CR.Top;
    end;
    Result.Left   := OffX;
    Result.Top    := OffY;
    Result.Right  := OffX + VW;
    Result.Bottom := OffY + VH;
  end;
end;

procedure TCnSVGImage.Paint;
var
  DestRect: TRect;
  StartTick: Cardinal;
begin
  if FDocument.IsLoaded then
  begin
    StartTick := GetTickCount;
    DestRect  := CalcDestRect;
    try
      FDocument.Render(Canvas, DestRect);
    except
      on E: ECnSVGException do
        FLastError := E.Message;
    end;
    FLastRenderTimeMs := GetTickCount - StartTick;
    if not (csDesigning in ComponentState) then
      if Assigned(FOnRenderDone) then
        FOnRenderDone(Self);
  end
  else if FLastError <> '' then
  begin
    // 绘制红色对角叉表示加载失败
    Canvas.Pen.Color := clRed;
    Canvas.Pen.Width := 2;
    Canvas.MoveTo(ClientRect.Left, ClientRect.Top);
    Canvas.LineTo(ClientRect.Right, ClientRect.Bottom);
    Canvas.MoveTo(ClientRect.Right, ClientRect.Top);
    Canvas.LineTo(ClientRect.Left, ClientRect.Bottom);
  end;
end;

//==============================================================================
// 独立渲染函数实现存根
//==============================================================================

procedure CnSVGRenderToCanvas(const AFileName: string;
  ACanvas: TCanvas; const ADestRect: TRect);
var
  Doc: TCnSVGDocument;
begin
  if (ADestRect.Right <= ADestRect.Left) or
     (ADestRect.Bottom <= ADestRect.Top) then
    Exit;
  Doc := TCnSVGDocument.Create;
  try
    Doc.LoadFromFile(AFileName);
    Doc.Render(ACanvas, ADestRect);
  finally
    Doc.Free;
  end;
end;

procedure CnSVGRenderToCanvasFromStream(AStream: TStream;
  ACanvas: TCanvas; const ADestRect: TRect);
var
  Doc: TCnSVGDocument;
begin
  if (ADestRect.Right <= ADestRect.Left) or
     (ADestRect.Bottom <= ADestRect.Top) then
    Exit;
  Doc := TCnSVGDocument.Create;
  try
    Doc.LoadFromStream(AStream);
    Doc.Render(ACanvas, ADestRect);
  finally
    Doc.Free;
  end;
end;

function CnSVGRenderToBitmap(const AFileName: string;
  AWidth, AHeight: Integer): TBitmap;
var
  Doc: TCnSVGDocument;
  DestRect: TRect;
begin
  if (AWidth <= 0) or (AHeight <= 0) then
    raise ECnSVGException.Create(SCnErrorSvgRenderSize);
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf24bit;
    Result.Width       := AWidth;
    Result.Height      := AHeight;
    DestRect.Left      := 0;
    DestRect.Top       := 0;
    DestRect.Right     := AWidth;
    DestRect.Bottom    := AHeight;
    Doc := TCnSVGDocument.Create;
    try
      Doc.LoadFromFile(AFileName);
      Doc.Render(Result.Canvas, DestRect);
    finally
      Doc.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.

