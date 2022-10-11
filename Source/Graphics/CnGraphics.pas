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

unit CnGraphics;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：界面控件包原快速图像处理单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元为旧的图像库，新图像库正在重新制作中
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.09.25 V0.12
*               增加 Win64 位的支持
*           2002.03.14 V0.11Alpha
*               原图像处理库最后版本
*               新库正在制作中
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Graphics, ExtCtrls, Math, Controls, Messages,
  CnNative, CnClasses, CnCommon, CnGraphConsts;

type

//--------------------------------------------------------//
// 图像处理基本类型定义                                   //
//--------------------------------------------------------//

  PCnColor = ^TCnColor;
  {* 指向TCnColor的指针类型}
  TCnColor = packed record
  {* 24位颜色值记录}
    b, g, r: Byte;
  end;

  PCnLine = ^TCnLine;
  {* 指向TCnLine的指针类型，一般指向一行位图扫描线数据}
  TCnLine = array[0..65535] of TCnColor;
  {* TCnColor数组类型，一般为一行位图扫描线数据}
  PPCnLines = ^TPCnLines;
  {* 指向TPCnLines的指针类型，一般用来指向位图扫描线地址数组}
  TPCnLines = array[0..65535] of PCnLine;
  {* PCnLine数组类型，一般用于存储位图扫描线地址数组}

  TFilterCore = array[0..2, 0..2] of SmallInt;
  {* 用于3X3图像卷积处理的卷积核陈列}

  TPointF = record
  {* 浮点数坐标记录类型}
    x, y: Single;
  end;
  TPointFArray = array of TPointF;
  {* TPointF浮点数坐标动态数组，一般用于图形绘制参数传递}

  TRectF = record
  {* 浮点数矩形记录类型}
    case Integer of
      0: (Left, Top, Right, Bottom: Single);
      1: (TopLeft, BottomRight: TPointF);
  end;

  TCnDrawMode = (dmDraw, dmCenter, dmStretched, dmResize, dmTiled);
  {* 图像绘制模式
   |<BR>
   |<BR>　　dmDraw　　　将源图像直接绘制在目标图像左上角
   |<BR>　　dmCenter　　将源图像绘制到目标图像中心位置
   |<BR>　　dmStretched　将源图像缩放绘制到目标图像
   |<BR>　　dmResize　　缩放绘制时保持源图像长宽比
   |<BR>　　dmTiled　　　源图像平铺绘制到目标图像
  }

const
  csMaxAlpha = High(Byte);
  csMaxProgress = 100;

type
  TCnAlpha = Byte;
  {* 图像Alpha混合系数0..255，0表示全透明，255表示不透明}
  TCnProgress = 0..csMaxProgress;
  {* 处理进程百分比类型0..100}

//--------------------------------------------------------//
// 图像处理异常类型定义                                   //
//--------------------------------------------------------//

type
  ECnGraphics = class(Exception);
  {* CnPack快速图像库异常基类}

  EInvalidPixel = class(ECnGraphics);
  {* 无效的象素点异常，通常是因为访问TCnBitmap的象素点时范围超界}
  EInvalidScanLine = class(ECnGraphics);
  {* 无效的扫描线异常，通常是因为访问TCnBitmap的扫描线时范围超界}
  EBitmapIsEmpty = class(ECnGraphics);
  {* 无法访问空位图异常，通常是因为访问没有位图数据的TCnBitmap的象素或扫描线}
  EInvalidForeBmp = class(ECnGraphics);
  {* 无效的前景图异常，在绘制平滑字体时产生，内部调试用}

//--------------------------------------------------------//
// 渐变颜色类                                             //
//--------------------------------------------------------//

const
  csMaxGradPos = 100;

type
  TCnGradPos = 0..csMaxGradPos;
  {* 渐变颜色位置类型0..100，用于标识一个中间颜色在渐变色带中的位置}
  TCnGradStyle = (gsLeftToRight, gsRightToLeft, gsTopToBottom, gsBottomToTop,
    gsCenterToLR, gsCenterToTB, gsRadial);
  {* 渐变色绘制模式
   |<BR>
   |<BR>　　gsLeftToRight　　从左到右渐变
   |<BR>　　gsRightToLeft　　从右到左渐变
   |<BR>　　gsTopToBottom　　从上到下渐变
   |<BR>　　gsBottomToTop　　从下到上渐变
   |<BR>　　gsCenterToLR　　　从中间到两边渐变
   |<BR>　　gsCenterToTB　　　从中间到上下渐变
   |<BR>　　gsRadial　　　   从中间四周辐射渐变
  }
  TCnMiddleColor = class;

{ TCnMiddleColorItem }

  TCnMiddleColorItem = class(TCollectionItem)
  {* 渐变颜色带的中间色子项类}
  private
    FColor: TColor;
    FPos: TCnGradPos;
    procedure SetColor(const Value: TColor);
    procedure SetPos(const Value: TCnGradPos);
  public
    constructor Create(Collection: TCollection); override;
    {* 构造器，应传递一个TCnMiddleColor对象做参数，一般不需要手动调用}
    procedure Assign(Source: TPersistent); override;
    {* 赋值方法，一般不需要手动调用}
  published
    property Color: TColor read FColor write SetColor;
    {* 渐变颜色带的中间颜色值}
    property Pos: TCnGradPos read FPos write SetPos;
    {* 渐变颜色带的中间颜色在色带中的位置}
  end;

{ TCnMiddleColor }

  TCnMiddleColor = class(TOwnedCollection)
  {* 渐变颜色带的中间色收集器类，主要用于TCnGradientColor对象中}
  private
    FSorting: Boolean;
    function GetItem(Index: Integer): TCnMiddleColorItem;
    procedure SetItem(Index: Integer; const Value: TCnMiddleColorItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
  public
    constructor Create(AOwner: TPersistent);
    {* 构造器，用于创建一个该类的实例}
    procedure Add(AColor: TColor; APos: TCnGradPos);
    {* 增加子项方法，用于向中间色带中增加一个颜色项
     |<BR>
     |<BR>　　AColor: TColor　　　增加的颜色值
     |<BR>　　APos: TCnGradPos　　该颜色在渐变色带中的位置
    }
    procedure Sort;
    {* 对渐变色按位置排序，一般不需要手动调用}
    property Items[Index: Integer]: TCnMiddleColorItem read GetItem write SetItem;
    default;
    {* 渐变色带颜色项，允许按数组的形式访问色带中的子项}
  end;

{ TCnGradientColor }

  TCnGradientColor = class(TCnPersistent)
  {* 渐变颜色类，保存了渐变色绘制参数，可作为参数传递给TCnBitmap的渐变色绘制方式}
  private
    FColorStart: TColor;
    FColorEnd: TColor;
    FStyle: TCnGradStyle;
    FColorMiddle: TCnMiddleColor;
    procedure SetColorEnd(const Value: TColor);
    procedure SetColorStart(const Value: TColor);
    procedure SetStyle(const Value: TCnGradStyle);
    function GetColorMiddle: TCnMiddleColor;
    procedure SetColorMiddle(const Value: TCnMiddleColor);
  public
    constructor Create; override;
    {* 构造器，用于创建一个该类的实例}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {* 赋值方法，一般不需要手动调用}
  published
    property ColorMiddle: TCnMiddleColor read GetColorMiddle write SetColorMiddle;
    {* 渐变颜色的中间色带项}
    property ColorStart: TColor read FColorStart write SetColorStart default clBlack;
    {* 渐变起始色}
    property ColorEnd: TColor read FColorEnd write SetColorEnd default clBlack;
    {* 渐变结束色}
    property Style: TCnGradStyle read FStyle write SetStyle default gsLeftToRight;
    {* 渐变方式}
  end;

//--------------------------------------------------------//
// 平滑特效字体类                                         //
//--------------------------------------------------------//

  TFontQuality = (fqHigh, fqNormal, fqLow, fqNone);
  {* 平滑字体绘制精度类型
   |<BR>
   |<BR>　　fqHigh　　　高精度绘制，采用4X4采样，速度较慢
   |<BR>　　fqNormal　　普通精度绘制，采用3X3采样，最佳速度质量比，默认值
   |<BR>　　fqLow　　　　低精度绘制，采用2X2采样，速度较快
   |<BR>　　fqNone　　　无平滑效果
  }
  TFontStyleEx = (fsShadow, fsGradient, fsTexture, fsNoise, fsOutline,
    fsLighting, fsSpray);
  {* 平滑字体扩展特效风格
   |<BR>
   |<BR>　　fsShadow　　　设置阴影效果，见TCnShadow
   |<BR>　　fsGradient　　设置文本颜色渐变效果，见TCnGradientColor
   |<BR>　　fsTexture　　　设置文本纹理图
   |<BR>　　fsNoise　　　　设置文本噪声纹理
   |<BR>　　fsOutline　　　设置文本以轮廓线方式显示
   |<BR>　　fsLighting　　设置灯光效果，见TCnLighting
   |<BR>　　fsSpray　　　　设置喷溅效果
  }
  TFontStyleExs = set of TFontStyleEx;
  {* 平滑字体扩展特效风格集合}

{ TCnShadow }

  TShadowOffset = -20..20;
  {* 阴影偏移范围}
  TShadowBlur = 0..10;
  {* 阴影模糊度}

  TCnShadow = class(TCnPersistent)
  {* 阴影参数类，定义用于平滑字体或图像特效的阴影参数}
  private
    FBlur: TShadowBlur;
    FAlpha: TCnAlpha;
    FColor: TColor;
    FOffsetX: TShadowOffset;
    FOffsetY: TShadowOffset;
    procedure SetBlur(const Value: TShadowBlur);
    procedure SetColor(const Value: TColor);
    procedure SetOffsetX(const Value: TShadowOffset);
    procedure SetOffsetY(const Value: TShadowOffset);
    procedure SetAlpha(const Value: TCnAlpha);
  public
    constructor Create; override;
    {* 构造器，用于创建一个该类的实例}
    procedure Assign(Source: TPersistent); override;
    {* 赋值方法，一般不需要手动调用}
  published
    property Blur: TShadowBlur read FBlur write SetBlur default 1;
    {* 阴影模糊属性，采用3X3高斯模糊算法进行阴影模糊处理}
    property Alpha: TCnAlpha read FAlpha write SetAlpha default 180;
    {* 阴影的不透明度属性}
    property Color: TColor read FColor write SetColor default $00444444;
    {* 阴影颜色}
    property OffsetX: TShadowOffset read FOffsetX write SetOffsetX default 2;
    {* 阴影在水平方向的偏移量，范围为-20..20，为负表示向左偏}
    property OffsetY: TShadowOffset read FOffsetY write SetOffsetY default 2;
    {* 阴影在垂直方向的偏移量，范围为-20..20，为负表示向上偏}
  end;

{ TCnShadow }

  TLightingOffset = -200..200;
  {* 光照中心点偏移范围，百分比，为负表示左偏}
  TLightingRange = 0..1000;
  {* 光照范围，百分比}

  TCnLighting = class(TCnPersistent)
  {* 光照效果参数类，定义用于平滑字体或图像特效的灯光效果参数}
  private
    FAlpha: TCnAlpha;
    FColor: TColor;
    FOffsetX: TLightingOffset;
    FOffsetY: TLightingOffset;
    FAngle: Double;
    FHeight: TLightingRange;
    FWidth: TLightingRange;
    procedure SetColor(const Value: TColor);
    procedure SetOffsetX(const Value: TLightingOffset);
    procedure SetOffsetY(const Value: TLightingOffset);
    procedure SetAlpha(const Value: TCnAlpha);
    procedure SetAngle(const Value: Double);
    procedure SetHeight(const Value: TLightingRange);
    procedure SetWidth(const Value: TLightingRange);
  public
    constructor Create; override;
    {* 构造器，用于创建一个该类的实例}
    procedure Assign(Source: TPersistent); override;
    {* 赋值方法，一般不需要手动调用}
  published
    property Alpha: TCnAlpha read FAlpha write SetAlpha default 180;
    {* 光照效果的不透明度属性}
    property Color: TColor read FColor write SetColor default clWhite;
    {* 灯光颜色}
    property OffsetX: TLightingOffset read FOffsetX write SetOffsetX default 0;
    {* 光照中心点偏移范围（目标矩形宽度的百分比），为负表示左偏，范围为-200..200}
    property OffsetY: TLightingOffset read FOffsetY write SetOffsetY default 0;
    {* 光照中心点偏移范围（目标矩形高度的百分比），为负表示上偏，范围为-200..200}
    property Width: TLightingRange read FWidth write SetWidth default 80;
    {* 光照范围宽度（目标矩形宽度的百分比），范围为0..1000}
    property Height: TLightingRange read FHeight write SetHeight default 80;
    {* 光照范围高度（目标矩形宽度的百分比），范围为0..1000}
    property Angle: Double read FAngle write SetAngle;
  end;

{ TCnFont }

  TCnFont = class(TFont)
  {* 平滑特效字体类，从TFont派生而来，提供了一些特效显示参数}
  private
    FOwner: TPersistent;
    FNoise: Byte;
    FAlpha: TCnAlpha;
    FGradient: TCnGradientColor;
    FShadow: TCnShadow;
    FTextureMode: TCnDrawMode;
    FStyleEx: TFontStyleExs;
    FTexture: TPicture;
    FQuality: TFontQuality;
    FScale: Integer;
    FUpdateCount: Integer;
    FLighting: TCnLighting;
    FSpray: Byte;
    function GetTexture: TPicture;
    procedure SetAlpha(const Value: TCnAlpha);
    procedure SetGradient(const Value: TCnGradientColor);
    procedure SetNoise(const Value: Byte);
    procedure SetShadow(const Value: TCnShadow);
    procedure SetStyleEx(const Value: TFontStyleExs);
    procedure SetTexture(const Value: TPicture);
    procedure SetTextureMode(const Value: TCnDrawMode);
    procedure SetLighting(const Value: TCnLighting);
    procedure SetSpray(const Value: Byte);
    procedure SetQuality(const Value: TFontQuality);
    procedure ChildChanged(Sender: TObject);
  protected
    function GetOwner: TPersistent; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Changed; override;
    property Owner: TPersistent read FOwner write FOwner;
    property UpdateCount: Integer read FUpdateCount;
    property Scale: Integer read FScale;
  public
    constructor Create;
    {* 构造器，用于创建一个该类的实例}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {* 赋值方法，允许从TFont中赋值}
  published
    property StyleEx: TFontStyleExs read FStyleEx write SetStyleEx;
    {* 扩展的字体特效显示参数，TFontStyleExs集合类型，同时控制其它一些参数是否开启}
    property Quality: TFontQuality read FQuality write SetQuality default fqNormal;
    {* 字体平滑显示精度}
    property Shadow: TCnShadow read FShadow write SetShadow;
    {* 字体阴影显示参数，受StyleEx属性影响}
    property Gradient: TCnGradientColor read FGradient write SetGradient;
    {* 字体前景渐变效果参数，受StyleEx属性影响}
    property Lighting: TCnLighting read FLighting write SetLighting;
    {* 字体前景光照效果参数，受StyleEx属性影响}
    property Texture: TPicture read GetTexture write SetTexture;
    {* 字体前景纹理效果参数，受StyleEx属性影响}
    property TextureMode: TCnDrawMode read FTextureMode write SetTextureMode
      default dmTiled;
    {* 字体前景纹理效果显示模式}
    property Alpha: TCnAlpha read FAlpha write SetAlpha default csMaxAlpha;
    {* 字体不透明度参数}
    property Noise: Byte read FNoise write SetNoise default 0;
    {* 字体前景随机噪声效果参数，受StyleEx属性影响}
    property Spray: Byte read FSpray write SetSpray default 0;
    {* 字体前景喷溅效果参数，受StyleEx属性影响}
  end;

{ TCnFontMask }

  TCnFontMask = class
  private
    FBuff: PByteArray;
    FHeight: Integer;
    FWidth: Integer;
    FRowInc: Integer;
    function GetScanLine(Row: Integer): PByteArray;
    procedure SplitBlur(Amount: Integer);
  public
    destructor Destroy; override;
    procedure Outline;
    procedure SetSize(AWidth, AHeight: Integer);
    procedure CopyTo(Dst: TCnFontMask);
    procedure Blur(Amount: Integer);
    procedure Spray(Amount: Integer);
    property ScanLine[Row: Integer]: PByteArray read GetScanLine;
    property Buff: PByteArray read FBuff;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

//--------------------------------------------------------//
// 快速图像处理类                                         //
//--------------------------------------------------------//

  TGdiAllocStyle = (gsInternal, gsNormal, gsActive);
  {* GDI 资源管理方式
   |<BR>
   |<BR>　　　　　　　　　DC　　　　HBITMAP
   |<BR>　　gsInternal:　即时释放　即时释放
   |<BR>　　gsNormal:　　即时释放　定时释放
   |<BR>　　gsActive:　　即时释放　从不释放
  }

  TAdjustRange = -100..100;
  {* 图像属性变化范围}

  TPenWeight = (pwThin, pwNormal, pwThick);
  {* 图像抗锯齿画笔粗细程度，均为1个象素宽度以内
   |<BR>
   |<BR>　　pwThin:　　细画笔，绘制出的图形较浅
   |<BR>　　pwNormal:　常规画笔，绘制出的图形适中
   |<BR>　　pwThick:　　粗画笔，绘制出的图形较粗
  }

  TColorChannel = (ccRed, ccGreen, ccBlue);
  {* 图像颜色通道
   |<BR>
   |<BR>　　ccRed:　　红色通道
   |<BR>　　ccGreen:　绿色通道
   |<BR>　　ccBlue:　　蓝色通道
  }
  TColorChannels = set of TColorChannel;
  {* 图像颜色通道集合}

  TTurnAngle = (ta90, ta180, ta270);

const
  csAllChannels = [ccRed, ccGreen, ccBlue];

type
  TCnBitmap = class;

{ TCnCanvas }

  TCnCanvas = class(TCanvas)
  {* 用于TCnBitmap内部的画布类，请勿直接使用}
  private
    FBitmap: TCnBitmap;
    FDC: HDC;
    procedure FreeContext;
    function GetHandle: HDC;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(ABitmap: TCnBitmap);
    {* 构造器，用于创建一个该类的实例}
    destructor Destroy; override;
    property Handle: HDC read GetHandle;
    {* 画布句柄，重申明为只读属性}
  end;

{ TCnBitmap }

  TCnBitmap = class(TCnPersistent)
  {* CnPack快速图像类}
  private
    FHandle: HBITMAP;
    FBitmapInfo: TBitmapInfo;
    FHeight: Integer;
    FWidth: Integer;
    FSize: Integer;
    FRowInc: Integer;
    FGap: Integer;
    FBits: Pointer;
    FScanLine: PPCnLines;
    FCanvas: TCnCanvas;
    FSmoothFilter: Boolean;
    FTransparentColor: TColor;
    FTransparent: Boolean;
    FGdiLastAccess: Cardinal;

    FGdiAllocStyle: TGdiAllocStyle;
    FPenPosF: TPointF;
    FPenWeight: TPenWeight;
    FPenColor: TColor;
    FFont: TCnFont;
    FGrayBmp: TBitmap;
    FFontMask: TCnFontMask;
    FFontClear: Boolean;
    FFontBkColor: TColor;

    class procedure OnGdiActTimer(Sender: TObject);
    function GetDC: HDC;
    function GetCanvas: TCnCanvas;
    procedure UpdateScanLine;
    function GetClientRect: TRect;
    function GetEmpty: Boolean;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    function GetScanLine(Row: Integer): PCnLine;
    function GetPixel(x, y: Integer): TCnColor;
    procedure SetPixel(x, y: Integer; const Value: TCnColor);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

    procedure NormalResize(Dst: TCnBitmap);
    procedure SmoothResize(Dst: TCnBitmap);
    procedure SplitBlur(Amount: Integer);
    procedure SplitSharpen(Amount: Integer);
    function CheckAlphaSrc(Src: TCnBitmap; ARect: TRect;
      Stretch: Boolean): TCnBitmap;
    function GetPixelsF(x, y: Single): TCnColor;
    procedure SetPixelsF(x, y: Single; const Value: TCnColor);
    function CalcDrawRect(DstX, DstY: Integer; SrcRect, SrcClientRect:
      TRect; var dx, dy, sx, sy, w, h: Integer): Boolean;
    function ClipLineF(var X0, Y0, X1, Y1: Single; MinX, MaxX, MinY,
      MaxY: Single): Boolean;
    function GetResizeRect(Src: TRect): TRect;
    function GetFont: TCnFont;
    procedure SetFont(const Value: TCnFont);
    function GetHandle: HBITMAP;
    procedure InitGrayBmp;
    procedure FreeGrayBmp;
    procedure DrawFontMaskEx(const Text: string; Extend: TSize; Point: TPoint);
    procedure DrawFontMask(const Text: string);
    function GetShadowPoint: TPoint;
    function GetTextPoint: TPoint;
    procedure FontMaskBlend(x, y: Integer; AColor: TColor; Alpha: TCnAlpha;
      Mask: TCnFontMask);
    procedure FontMaskBlendEx(x, y: Integer; Alpha: TCnAlpha;
      Mask: TCnFontMask; ForeBmp: TCnBitmap);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CleanUp; virtual;
    function GetTranColor: TCnColor;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect;
      Tran: Boolean);
    procedure DoSetPixelF(x, y: Integer; ARGB: TCnColor);
    function DoGetPixelF(x, y: Integer): TCnColor;

    function Compress(var pData: Pointer; ASize: Integer): Integer; virtual;
    function DeCompress(var pData: Pointer; ASize: Integer): Integer; virtual;
    function HandleAllocated: Boolean;
    procedure HandleNeeded; virtual;
    procedure HandleRelease(KeepData: Boolean = True); virtual;

    property BitmapInfo: TBitmapInfo read FBitmapInfo;
    property RowInc: Integer read FRowInc;
    property Gap: Integer read FGap;
  public
    function Equals(Obj: TObject): Boolean; {$IFDEF OBJECT_HAS_EQUAL}override;{$ENDIF}
    constructor Create; override;
    {* 构造器，用于创建一个该类的实例}
    destructor Destroy; override;

    // 辅助功能
    procedure Fill(Color: TColor = clBlack);
    {* 以指定颜色填充整个图像，默认为黑色}
    procedure FillRect(Rect: TRect; Color: TColor = clBlack);
    {* 以指定颜色填充整个一个图像区域}
    procedure DrawLine(x1, y1, x2, y2: Integer; Color: TColor);
    {* 以指定颜色绘制一条直线
     |<BR>
     |<BR> x1, y1: Integer　　起始点坐标
     |<BR> x2, y2: Integer　　结束点坐标}
    procedure FrameRect(Rect: TRect; Color: TColor);
    {* 以指定颜色绘制一个矩形框架}
    procedure FreeImage; virtual;
    {* 将整个位图清空，释放所有已分配的资源}
    function CreateRegion(var RgnData: PRgnData): Integer;
    {* 根据当前的透明色属性TransparentColor从位图中创建一个Region区域数据。
     |<BR> 变量参数RgnData为PRgnData指针类型，用于接收数据结果。
     |<BR> 返回值为结果数据块的字节数。}

    // 与外部交换数据
    procedure Assign(Source: TPersistent); override;
    {* 赋值过程，允许从其它图像对象中获取数据。
     |<BR>
     |<BR> 支持以下的源图像类型：
     |<BR> TCnBitmap、TBitmap、TGraphic、TPicture以及它们的派生类如TIcon、TJpegImage等
     |<BR> 当Source为nil时，清空当前位图，释放所有已分配的资源。}
    procedure SetSize(AWidth, AHeight: Integer); overload;
    {* 设置当前图像大小，如果变更大小，原图像数据将丢失。}
    procedure SetSize(ARect: TRect); overload;
    {* 设置当前图像大小，如果变更大小，原图像数据将丢失}
    procedure LoadBlank(AWidth, AHeight: Integer);
    {* 设置当前图像大小，功能同SetSize。}
    procedure LoadFromMemory(ABits: Pointer; AWidth, AHeight: Integer);
    {* 从内存中装载位图
     |<BR>
     |<BR> ABits: Pointer　　指向一块数据区，数据内容应该是24位格式的图像数据
     |<BR> AWidth, AHeight: Integer　　位图的宽度和高度}
    procedure LoadFromStream(Stream: TStream);
    {* 从流中装载位图}
    procedure LoadFromFile(const FileName: string);
    {* 从图像文件中装载位图
     |<BR> 内部使用TPicture来读取文件，允许系统中支持的图像文件格式。
     |<BR> 如Icon、Wmf、Jpeg（需要Jpeg单元）等图像文件}
    procedure LoadFromResourceName(instance: THandle; const ResName: string);
    {* 从资源中装载位图，参数为模块句柄和BITMAP资源名}
    procedure LoadFromResourceID(instance: THandle; ResID: Integer);
    {* 从资源中装载位图，参数为模块句柄和BITMAP资源ID}
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE);
    {* 从剪帖板中装载位图}
    procedure SaveToStream(Stream: TStream);
    {* 将当前位图保存到流}
    procedure SaveToFile(const FileName: string);
    {* 将当前位图保存到Bmp文件，文件格式为24位Bmp位图文件}
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE);
    {* 复制位图到剪帖板中}

    // 图像绘制方法
    procedure Draw(DstX, DstY: Integer; Src: TCnBitmap); overload;
    {* 图像绘制方法，将源图像全部绘制到当前位图中
     |<BR>
     |<BR> DstX, DstY: Integer　　当前图像的左上角坐标
     |<BR> Src: TCnBitmap　　源图像}
    procedure DrawEx(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect); overload;
    {* 增强的图像绘制方法，将源图像中的一部分绘制到当前位图中
     |<BR>
     |<BR> DstX, DstY: Integer　　当前图像的左上角坐标
     |<BR> Src: TCnBitmap　　源图像
     |<BR> SrcRect: TRect　　源图像矩形}
    procedure Draw(DstX, DstY: Integer; Src: TGraphic); overload;
    {* 图像绘制方法，将源图像全部绘制到当前位图中
     |<BR>
     |<BR> DstX, DstY: Integer　　当前图像的左上角坐标
     |<BR> Src: TGraphic　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类}
    procedure DrawEx(DstX, DstY: Integer; Src: TGraphic; SrcRect: TRect); overload;
    {* 增强的图像绘制方法，将源图像中的一部分绘制到当前位图中
     |<BR>
     |<BR> DstX, DstY: Integer　　当前图像的左上角坐标
     |<BR> Src: TGraphic　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类
     |<BR> SrcRect: TRect　　源图像矩形}
    procedure Draw(DstX, DstY: Integer; hSrc: HDC; SrcRect: TRect); overload;
    {* 图像绘制方法，将源DC上的一部分绘制到当前位图中
     |<BR>
     |<BR> DstX, DstY: Integer　　为当前图像的左上角坐标
     |<BR> hSrc: HDC　　　　源DC句柄，可以是TCanvas.Handle
     |<BR> SrcRect: TRect　　源图像矩形}
    procedure DrawTo(hDst: HDC; DstX, DstY: Integer); overload;
    {* 图像绘制方法，将当前位图全部绘制到目标DC上
     |<BR>
     |<BR> hDst: HDC　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> DstX, DstY: Integer　　目标画布的左上角坐标}
    procedure DrawToEx(hDst: HDC; DstX, DstY: Integer; SrcRect: TRect); overload;
    {* 增强的图像绘制方法，将当前的一部分绘制到目标DC上
     |<BR>
     |<BR> hDst: HDC　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> DstX, DstY: Integer　　目标画布的左上角坐标
     |<BR> SrcRect: TRect　　当前图像源矩形}
    procedure DrawMode(Src: TCnBitmap; Mode: TCnDrawMode); overload;
    {* 指定模式的图像绘制方法，将源图像按指定的模式绘制到当前图像上
     |<BR>
     |<BR> Src: TCnBitmap　　源图像
     |<BR> Mode: TCnDrawMode　绘制方式}
    procedure DrawModeEx(Src: TCnBitmap; Mode: TCnDrawMode; Alpha: TCnAlpha); overload;
    {* 指定模式的图像绘制方法，将源图像按指定的模式绘制到当前图像上，支持Alpha混合
     |<BR>
     |<BR> Src: TCnBitmap　　源图像
     |<BR> Mode: TCnDrawMode　绘制方式}
    procedure DrawMode(Src: TGraphic; Mode: TCnDrawMode); overload;
    {* 指定模式的图像绘制方法，将源图像按指定的模式绘制到当前图像上
     |<BR>
     |<BR> Src: TGraphic　　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类
     |<BR> Mode: TCnDrawMode　绘制方式}
    procedure DrawModeEx(Src: TGraphic; Mode: TCnDrawMode; Alpha: TCnAlpha); overload;
    {* 指定模式的图像绘制方法，将源图像按指定的模式绘制到当前图像上，支持Alpha混合
     |<BR>
     |<BR> Src: TGraphic　　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类
     |<BR> Mode: TCnDrawMode　绘制方式}

    // 中心绘制
    procedure CenterDraw(Src: TCnBitmap); overload;
    {* 将源图像绘制到当前图像的中心位置上
     |<BR>
     |<BR> Src: TCnBitmap　　　源图像}
    procedure CenterDraw(Src: TGraphic); overload;
    {* 将源图像绘制到当前图像的中心位置上
     |<BR>
     |<BR> Src: TGraphic　　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类}

    // 平铺绘制
    procedure TileDraw(Src: TCnBitmap); overload;
    {* 将源图像平铺绘制到当前图像
     |<BR>
     |<BR> Src: TCnBitmap　　　源图像}
    procedure TileDrawEx(DstRect: TRect; Src: TCnBitmap); overload;
    {* 将源图像平铺绘制到当前图像指定区域内
     |<BR>
     |<BR> DstRect: TRect　　　当前图像目标矩形
     |<BR> Src: TCnBitmap　　　源图像}
    procedure TileDraw(Src: TGraphic); overload;
    {* 将源图像平铺绘制到当前图像
     |<BR>
     |<BR> Src: TGraphic　　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类}
    procedure TileDrawEx(DstRect: TRect; Src: TGraphic); overload;
    {* 将源图像平铺绘制到当前图像指定区域内
     |<BR>
     |<BR> DstRect: TRect　　　当前图像目标矩形
     |<BR> Src: TCnBitmap　　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类}
    procedure TileDrawTo(hDst: HDC; DstRect: TRect); overload;
    {* 将当前图像平铺绘制到目标DC指定区域内
     |<BR>
     |<BR> hDst: HDC　　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> DstRect: TRect　　　目标矩形}

    // 缩放绘制
    procedure StretchDraw(Src: TCnBitmap); overload;
    {* 将源图像缩放绘制到当前图像中
     |<BR>
     |<BR> Src: TCnBitmap　　源图像}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; Src: TCnBitmap); overload;
    {* 将源图像的一部分缩放绘制到当前图像中指定区域内
     |<BR>
     |<BR> DstRect: TRect　　目标矩形
     |<BR> SrcRect: TRect　　源矩形
     |<BR> Src: TCnBitmap　　源图像}
    procedure StretchDraw(Src: TGraphic); overload;
    {* 将源图像缩放绘制到当前图像中
     |<BR>
     |<BR> Src: TGraphic　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; Src: TGraphic); overload;
    {* 将源图像的一部分缩放绘制到当前图像指定区域内
     |<BR>
     |<BR> DstRect: TRect　　目标矩形
     |<BR> SrcRect: TRect　　源矩形
     |<BR> Src: TGraphic　　　源图像，可以是TIcon、TBitmap、TJpegImage等派生类}
    procedure StretchDraw(SrcRect: TRect; hSrc: HDC); overload;
    {* 将源DC上的指定区域缩放绘制到当前图像中
     |<BR>
     |<BR> SrcRect: TRect　　源矩形
     |<BR> hSrc: HDC　　　　　源DC句柄，可以是TCanvas.Handle}
    procedure StretchDrawEx(DstRect, SrcRect: TRect; hSrc: HDC); overload;
    {* 将源DC上的指定区域缩放绘制到当前图像指定区域内
     |<BR>
     |<BR> DstRect: TRect　　目标矩形
     |<BR> SrcRect: TRect　　源矩形
     |<BR> hSrc: HDC　　　　　源DC句柄，可以是TCanvas.Handle}
    procedure StretchDrawTo(Dst: TImage); overload;
    {* 将当前图像缩放绘制到TImage控件中
     |<BR>
     |<BR> Dst: TImage　　　目标控件}
    procedure StretchDrawTo(hDst: HDC; DstRect: TRect); overload;
    {* 将当前图像缩放绘制到DC中
     |<BR>
     |<BR> hDst: HDC　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> DstRect: TRect　　目标矩形}
    procedure StretchDrawToEx(hDst: HDC; DstRect, SrcRect: TRect); overload;
    {* 将当前图像的一部分缩放绘制到DC中
     |<BR>
     |<BR> hDst: HDC　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> DstRect: TRect　　目标矩形
     |<BR> SrcRect: TRect　　源矩形}

    // Alpha混合绘制
    procedure AlphaDraw(Src: TCnBitmap; Alpha: TCnAlpha; Stretch: Boolean); overload;
    {* 将源图像与当前图像按指定的比例混合到当前图像中
     |<BR>
     |<BR> Src: TCnBitmap　　源图像
     |<BR> Alpha: TCnAlpha　　源图像的不透明度
     |<BR> Stretch: Boolean　当图像大小不一致时，是否自动对源图像进行缩放}
    procedure AlphaDraw(DstX, DstY: Integer; Src: TCnBitmap; SrcRect: TRect;
      Alpha: TCnAlpha); overload;
    {* 将源图像中的一部分与当前图像按指定的比例混合到当前图像指定位置中
     |<BR>
     |<BR> DstX, DstY: Integer　　目标位置左上角坐标
     |<BR> Src: TCnBitmap　　源图像
     |<BR> SrcRect: TRect　　源矩形
     |<BR> Alpha: TCnAlpha　　源图像的不透明度}
    procedure AlphaDrawGrad(Src: TCnBitmap; Style: TCnGradStyle;
      Stretch: Boolean; StartAlpha: TCnAlpha = 0; EndAlpha: TCnAlpha = csMaxAlpha);
    {* 将源图像与当前图像按渐变的比例混合到当前图像指定位置中
     |<BR>
     |<BR> Src: TCnBitmap　　　　源图像
     |<BR> Style: TCnGradStyle　　渐变混合方式
     |<BR> Stretch: Boolean　　　当图像大小不一致时，是否自动对源图像进行缩放
     |<BR> StartAlpha: TCnAlpha　渐变起始的不透明度
     |<BR> EndAlpha: TCnAlpha　　渐变结束的不透明度}
    procedure AlphaDrawEx(DstRect: TRect; Front, Back: TCnBitmap; Alpha: TCnAlpha;
      Stretch: Boolean);
    {* 将两个图像按指定的比例混合到当前图像指定区域中
     |<BR>
     |<BR> Front: TCnBitmap　　　前景图像
     |<BR> Back: TCnBitmap　　　　背景图像
     |<BR> Alpha: TCnAlpha　　　　前景图像的不透明度
     |<BR> Stretch: Boolean　　　当图像大小不一致时，是否自动对源图像进行缩放}

    // 渐变色绘制
    procedure DrawGradient(GradColor: TCnGradientColor);
    {* 在当前图像中产生渐变颜色效果
     |<BR>
     |<BR> GradColor: TCnGradientColor　　渐变效果参数}
    procedure DrawGradientEx(GradColor: TCnGradientColor; Rect: TRect; Alpha:
      TCnAlpha);
    {* 在当前图像指定区域中产生半透明的渐变颜色效果
     |<BR>
     |<BR> GradColor: TCnGradientColor　　渐变效果参数
     |<BR> Rect: TRect　　　　指定区域
     |<BR> Alpha: TCnAlpha　　渐变效果的不透明度
     |<BR> 注：当选择辐射渐变方式时，根据当前SmoothFilter属性可支持抗锯齿处理 }

    // 按钮位图绘制
    procedure Disabled;
    {* 将当前图像按失效按钮的风格进行绘制，根据当前的透明色属性判断}
    procedure DisabledEx(OutlineColor, BackColor, HighlightColor,
      ShadowColor: TColor; DrawHighlight: Boolean);
    {* 将当前图像按失效按钮的风格进行绘制，根据当前的透明色属性判断
     |<BR>
     |<BR> OutlineColor: TColor　　目标图像轮廓颜色
     |<BR> BackColor: TColor　　　目标图像背景颜色
     |<BR> HighlightColor: TColor　目标图像高亮区颜色
     |<BR> ShadowColor: TColor　　目标图像阴影颜色
     |<BR> DrawHighlight: Boolean　是否绘制高亮区}
    procedure DrawDisabled(hDst: HDC; ARect: TRect);
    {* 将当前图像按失效按钮的风格绘制到目标DC上，根据当前的透明色属性判断
       完成绘制后当前图像内容不变
     |<BR>
     |<BR> hDst: HDC　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> ARect: TRect　　　目标矩形}
    procedure DrawDisabledEx(hDst: HDC; ARect: TRect; OutlineColor,
      BackColor, HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean);
    {* 将当前图像按失效按钮的风格绘制到目标DC上，根据当前的透明色属性判断
       完成绘制后当前图像内容不变
     |<BR>
     |<BR> hDst: HDC　　　　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> ARect: TRect　　　　　　目标矩形
     |<BR> OutlineColor: TColor　　目标图像轮廓颜色
     |<BR> BackColor: TColor　　　目标图像背景颜色
     |<BR> HighlightColor: TColor　目标图像高亮区颜色
     |<BR> ShadowColor: TColor　　目标图像阴影颜色
     |<BR> DrawHighlight: Boolean　是否绘制高亮区}
    procedure Shadowed;
    {* 将当前图像按带阴影按钮的风格进行绘制，根据当前的透明色属性判断}
    procedure ShadowedEx(OutlineColor, ShadowColor, BackColor: TColor;
      Blur: Boolean; OffsetX, OffsetY: Integer);
    {* 将当前图像按带阴影按钮的风格进行绘制，根据当前的透明色属性判断
     |<BR>
     |<BR> OutlineColor: TColor　　目标图像轮廓颜色
     |<BR> ShadowColor: TColor　　目标图像阴影颜色
     |<BR> BackColor: TColor　　　目标图像背景颜色
     |<BR> Blur: Boolean　　　　　阴影是否模糊
     |<BR> OffsetX: Integer　　　　阴影水平偏移量，为负表示左偏
     |<BR> OffsetY: Integer　　　　阴影垂直偏移量，为负表示上偏}
    procedure DrawShadowed(hDst: HDC; ARect: TRect);
    {* 将当前图像按带阴影按钮的风格绘制到目标DC上，根据当前的透明色属性判断
       完成绘制后当前图像内容不变
     |<BR>
     |<BR> hDst: HDC　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> ARect: TRect　　　目标矩形}
    procedure DrawShadowedEx(hDst: HDC; ARect: TRect; OutlineColor, ShadowColor,
      BackColor: TColor; Blur: Boolean; OffsetX, OffsetY: Integer);
    {* 将当前图像按带阴影按钮的风格绘制到目标DC上，根据当前的透明色属性判断
       完成绘制后当前图像内容不变
     |<BR>
     |<BR> hDst: HDC　　　　　　　目标DC句柄，可以是TCanvas.Handle
     |<BR> ARect: TRect　　　　　　目标矩形
     |<BR> OutlineColor: TColor　　目标图像轮廓颜色
     |<BR> ShadowColor: TColor　　目标图像阴影颜色
     |<BR> BackColor: TColor　　　目标图像背景颜色
     |<BR> Blur: Boolean　　　　　阴影是否模糊
     |<BR> OffsetX: Integer　　　　阴影水平偏移量，为负表示左偏
     |<BR> OffsetY: Integer　　　　阴影垂直偏移量，为负表示上偏}

    // 图像颜色属性调整方法
    procedure RGB(ra, ga, ba: TAdjustRange);
    {* 调整当前图像的各颜色分量
     |<BR>
     |<BR> ra, ga, ba: TAdjustRange　分别为红、绿、蓝分量调整范围
     |<BR> 范围值为-100..100，0表示不变，正为增加，负为减少}
    procedure Brightness(Range: TAdjustRange; Channels: TColorChannels =
      csAllChannels);
    {* 调整当前图像的亮度
     |<BR>
     |<BR> Range: TAdjustRange　亮度范围值为-100..100，0表示不变，正为增加，负为减少
     |<BR> Channels: TColorChannels　　颜色通道设置}
    procedure Contrast(Range: TAdjustRange; Channels: TColorChannels = csAllChannels);
    {* 调整当前图像的对比度
     |<BR>
     |<BR> Range: TAdjustRange　比对度范围值为-100..100，0表示不变，正为增加，负为减少
     |<BR> Channels: TColorChannels　　颜色通道设置}
    procedure Saturation(Range: TAdjustRange; Channels: TColorChannels =
      csAllChannels);
    {* 调整当前图像的颜色饱和度
     |<BR>
     |<BR> Range: TAdjustRange　饱和度范围值为-100..100，0表示不变，正为增加，负为减少
     |<BR> Channels: TColorChannels　　颜色通道设置}
    procedure Levels(InLow, InHigh, OutLow, OutHigh: Byte;
      Channels: TColorChannels = csAllChannels);
    {* 调整当前图像的色阶
     |<BR>
     |<BR> InLow, InHigh: Byte　　输入图像的灰度值上、下限
     |<BR> OutLow, OutHigh: Byte　输出图像的灰度值上、下限
     |<BR> Channels: TColorChannels　　颜色通道设置}
    procedure Grayscale(Channels: TColorChannels = csAllChannels);
    {* 将当前图像转换为灰度图
     |<BR>
     |<BR> Channels: TColorChannels　　颜色通道设置}
    procedure Invert(Channels: TColorChannels = csAllChannels);
    {* 将当前图像所有象素的颜色反转
     |<BR>
     |<BR> Channels: TColorChannels　　颜色通道设置}
    procedure Colorize(Color: TColor); overload;
    {* 调整当前图像按指定颜色彩色化，支持图像透明设置
     |<BR>
     |<BR> Color: TColor　指定颜色值}
    procedure Colorize(Color: TCnColor); overload;
    {* 调整当前图像按指定颜色彩色化，支持图像透明设置
     |<BR>
     |<BR> Color: TCnColor　指定颜色值}

    // 图像几何变换
    procedure Flip(Horizontal: Boolean);
    {* 将当前图像几何位置翻转
     |<BR>
     |<BR> Horizontal: Boolean　为真表示水平翻转，为假表示垂直翻转}
    procedure Turn(Angle: TTurnAngle);
    {* 将当前图像几何位置旋转
     |<BR>
     |<BR> Angle: TTurnAngle　转动角度、可为ta90、ta180、ta270}

    procedure VShift(Amount: Integer);
    {* 将当前图像进行垂直平移
     |<BR>
     |<BR> Amount: Integer　平移量，允许为负}
    procedure HShift(Amount: Integer);
    {* 将当前图像进行水平平移
     |<BR>
     |<BR> Amount: Integer　平移量，允许为负}
    procedure Rotate(DstCenter: TPoint; Src: TCnBitmap; Angle: Double);
    {* 将源图像旋转后绘制到当前图像指定位置上，支持源图像透明属性
     |<BR>
     |<BR> DstCenter: TPoint　目标中心点位置，允许在当前图像外
     |<BR> Src: TCnBitmap　　　源图像
     |<BR> Angle: Double　　　源图像旋转角度，单位为度数}

    // 滤镜处理方法
    procedure ApplyFilter(Core: TFilterCore; Cent: Integer = 0);
    {* 对当前图像按指定模板进行卷积运算
     |<BR>
     |<BR> Core: TFilterCore　3X3卷积核
     |<BR> Cent: Integer 　　　卷积结果因素}
    procedure Blur;
    {* 对当前图像进行模糊处理，使用3X3均值算子}
    procedure GaussianBlur(Amount: Integer);
    {* 对当前图像进行快速高斯模糊处理
     |<BR>
     |<BR> Amount: Integer　　模糊半径}
    procedure Sharpen;
    {* 对当前图像进行锐化处理}
    procedure SharpenMore(Amount: Integer);
    {* 对当前图像进行更多的锐化处理
     |<BR>
     |<BR> Amount: Integer　　锐化程度}
    procedure Spray(Amount: Integer);
    {* 对当前图像进行喷溅滤镜处理}
    procedure Emboss;
    {* 将当前图像浮雕化}
    procedure Posterize(Amount: Integer);
    {* 产生壁画效果}
    procedure HeightMap(Amount: Integer);
    {* 产生地图效果}
    procedure Marble(Scale: Double; Turbulence: Integer);
    {* 产生水滴效果}
    procedure Wave(XDiv, YDiv, RatioVal: Double; Wrap: Boolean);
    {* 对当前图像进行扭曲变形
     |<BR>
     |<BR> XDiv, YDiv: Double　　水平、垂直方向的扭曲系数
     |<BR> RatioVal: Double　　　扭曲程度
     |<BR> Wrap: Boolean　　　　　指定是否自动环绕}
    procedure Mosaic(xAmount, yAmount: Integer);
    {* 将当前图像马赛克化
     |<BR>
     |<BR> xAmount: Integer　　矩形块宽度
     |<BR> yAmount: Integer　　矩形块高度}
    procedure Twist(Amount: Integer);
    {* 将当前图像转化为旋涡图
     |<BR>
     |<BR> Amount: Integer　　半径系数}
    procedure Lighting(Center: TPoint; OffX, OffY: Integer; Angle: Double;
      Color: TColor; Amount: TCnAlpha); overload;
    {* 在当前图像上产生光照效果
     |<BR>
     |<BR> Center: TPoint　　　光照中心点
     |<BR> OffX, OffY: Integer　光照范围，长、短轴半径
     |<BR> Angle: Double　　　　角度，OffX指定的长轴与水平轴的夹角
     |<BR> Color: TColor　　　　光照颜色
     |<BR> Amount: TCnAlpha　　光照强度}
    procedure Lighting(Rect: TRect; Data: TCnLighting); overload;
    {* 在当前图像上产生光照效果
     |<BR>
     |<BR> Rect: TRect　　　　光照目标范围（未旋转前）
     |<BR> Data: TCnLighting　光照参数}
    procedure Mask(MaskColor: TCnColor); overload;
    {* 将当前图像按指定颜色为标准二值化
     |<BR>
     |<BR> MaskColor: TCnColor　　指定的颜色，与该颜色相同的变为白色，反之为黑色}
    procedure MaskEx(MaskColor, InColor, BackColor: TCnColor); overload;
    {* 将当前图像按指定颜色为标准二值化
     |<BR>
     |<BR> MaskColor: TCnColor　　指定的颜色
     |<BR> InColor: TCnColor　　　图像中与指定色相同的象素用该颜色替代
     |<BR> BackColor: TCnColor　　图像中与指定色不同的象素用该颜色替代}
    procedure Mask(MaskColor: TColor); overload;
    {* 将当前图像按指定颜色为标准二值化
     |<BR>
     |<BR> MaskColor: TColor　　指定的颜色，与该颜色相同的变为白色，反之为黑色}
    procedure MaskEx(MaskColor, InColor, BackColor: TColor); overload;
    {* 将当前图像按指定颜色为标准二值化
     |<BR>
     |<BR> MaskColor: TColor　　指定的颜色
     |<BR> InColor: TColor　　　图像中与指定色相同的象素用该颜色替代
     |<BR> BackColor: TColor　　图像中与指定色不同的象素用该颜色替代}
    procedure AddColorNoise(Amount: Integer);
    {* 在当前图像中增加彩色噪声点
     |<BR>
     |<BR> Amount: Integer　　噪声系数}
    procedure AddMonoNoise(Amount: Integer);
    {* 在当前图像中增加黑白噪声点
     |<BR>
     |<BR> Amount: Integer　　噪声系数}
    procedure RemoveNoise(Amount: Integer);
    {* 从当前图像中移去噪声点，用于图像降噪处理
     |<BR>
     |<BR> Amount: Integer　　噪声系数}
    procedure AddMiddleColor(Color: TColor);
    {* 将当前图像与指定颜色做均值运算，类似于蒙板处理
     |<BR>
     |<BR> Color: TColor　　前景颜色}
    procedure AddMiddleColorEx(Color: TColor; Rect: TRect);
    {* 将当前图像的指定区域与指定颜色做均值运算，类似于蒙板处理
     |<BR>
     |<BR> Color: TColor　　前景颜色
     |<BR> Rect: TRect　　　指定矩形}

    // 其它图像处理方法
    procedure InterpolateRect(Rect: TRect; c00, c10, c01, c11: TCnColor); overload;
    {* 根据四角颜色值用渐变色填充矩形
     |<BR>
     |<BR> Rect: TRect　　　矩形块
     |<BR> c00: TCnColor　　左上角颜色
     |<BR> c10: TCnColor　　右上角颜色
     |<BR> c01: TCnColor　　左下角颜色
     |<BR> c11: TCnColor　　右上角颜色}
    procedure InterpolateRect(Rect: TRect; c00, c10, c01, c11: TColor); overload;
    {* 根据四角颜色值用渐变色填充矩形
     |<BR>
     |<BR> Rect: TRect　　矩形块
     |<BR> c00: TColor　　左上角颜色
     |<BR> c10: TColor　　右上角颜色
     |<BR> c01: TColor　　左下角颜色
     |<BR> c11: TColor　　右上角颜色}

    // 抗锯齿画笔绘制方法（支持小数）
    procedure DrawLineF(x1, y1, x2, y2: Single; Color: TColor);
    {* 以指定颜色绘制一条直线，使用抗锯齿算法
     |<BR>
     |<BR> x1, y1: Single　　起始点坐标
     |<BR> x2, y2: Single　　结束点坐标
     |<BR> Color: TColor　　直线颜色}
    procedure LineToF(x, y: Single); overload;
    {* 从当前点PenPosF绘制直线到目标点，同时移动画笔坐标，使用抗锯齿算法
     |<BR>
     |<BR> x, y: Single　　目标点坐标}
    procedure LineToF(Point: TPointF); overload;
    {* 从当前点PenPosF绘制直线到目标点，同时移动画笔坐标，使用抗锯齿算法
     |<BR>
     |<BR> Point: TPointF　　目标点坐标}
    procedure MoveToF(x, y: Single); overload;
    {* 移动当前画笔到目标点
     |<BR>
     |<BR> x, y: Single　　目标点坐标}
    procedure MoveToF(Point: TPointF); overload;
    {* 移动当前画笔到目标点
     |<BR>
     |<BR> Point: TPointF　　目标点坐标}
    procedure DrawRectF(const Rect: TRectF);
    {* 使用画笔绘制一个矩形，使用抗锯齿算法
     |<BR>
     |<BR> Rect: TRectF　　目标矩形}
    procedure PolylineF(const Points: TPointFArray);
    {* 使用画笔绘制折线，使用抗锯齿算法
     |<BR>
     |<BR> Points: TPointFArray　　各顶点坐标数组}
    procedure EllipseF(x1, y1, x2, y2: Single); overload;
    {* 使用画笔绘制椭圆，使用抗锯齿算法
     |<BR>
     |<BR> x1, y1: Single　　外接矩形的左上角坐标
     |<BR> x2, y2: Single　　外接矩形的右下角坐标}
    procedure EllipseF(const Rect: TRectF); overload;
    {* 使用画笔绘制椭圆，使用抗锯齿算法
     |<BR>
     |<BR> Rect: TRectF　　外接矩形}

    // 平滑字体绘制方法
    function TextExtent(const Text: string): TSize;
    {* 计算文本显示区域，使用平滑字体Font属性，不支持多行文本
     |<BR>
     |<BR> Text: string　　文本内容
     |<BR> Result: TSize　　文本区域}
    function TextHeight(const Text: string): Integer;
    {* 计算文本显示高度，使用平滑字体Font属性，不支持多行文本
     |<BR>
     |<BR> Text: string　　　文本内容
     |<BR> Result: Integer　　文本显示高度}
    function TextWidth(const Text: string): Integer;
    {* 计算文本显示宽度，使用平滑字体Font属性，不支持多行文本
     |<BR>
     |<BR> Text: string　　　文本内容
     |<BR> Result: Integer　　文本显示宽度}
    procedure TextOut(x, y: Integer; const Text: string);
    {* 在当前当前图像中绘制文本，使用平滑字体Font属性，不支持多行文本
     |<BR> FontClear属性决定文本背景是否透明，FontBkColor为不透明时的背景填充色
     |<BR>
     |<BR> x, y: Integer　　文本左上角坐标
     |<BR> Text: string　　　文本内容}

    // 高级属性
    property Handle: HBITMAP read GetHandle;
    {* 当前位图HBITMAP句柄，只读属性。如果位图为空，将出现异常}
    property DC: HDC read GetDC;
    {* 当前位图DC句柄，只读属性。如果位图为空，将出现异常}
    property Bits: Pointer read FBits;
    {* 当前位图象素数据存放的地址，只读属性。如果位图为空，返回nil}
    property Size: Integer read FSize;
    {* 当前位图象素数据块的大小，只读属性。如果位图为空，返回0}
    property GdiAllocStyle: TGdiAllocStyle read FGdiAllocStyle write FGdiAllocStyle;
    {* 当前位图的GDI资源管理方式，高级属性}
    property ScanLine[Row: Integer]: PCnLine read GetScanLine;
    {* 取得当前位图一行扫描线地址，只读属性。如果位图为空或范围超限，将出现异常}
    property Pixels[x, y: Integer]: TCnColor read GetPixel write SetPixel;
    {* 访问位图中的某个象素。如果位图为空或范围超限，将出现异常}

    // 常规属性
    property Width: Integer read FWidth write SetWidth;
    {* 当前位图的宽度}
    property Height: Integer read FHeight write SetHeight;
    {* 当前位图的高度}
    property ClientRect: TRect read GetClientRect;
    {* 当前位图的整个区域，只读属性}
    property Canvas: TCnCanvas read GetCanvas;
    {* 访问当前位图的画布，只读属性}
    property Empty: Boolean read GetEmpty;
    {* 当前位图是否为空，只读属性}
    property Font: TCnFont read GetFont write SetFont;
    {* 平滑字体属性，派生自TFont，提供一些特效显示参数}
    property FontClear: Boolean read FFontClear write FFontClear default False;
    {* 绘制平滑字体文本时，背景是否透明}
    property FontBkColor: TColor read FFontBkColor write FFontBkColor default clWhite;
    {* 绘制平滑字体文本时，如果背景不透明，用于填充背景的颜色}
    property SmoothFilter: Boolean read FSmoothFilter write FSmoothFilter default True;
    {* 在对图像进行缩放、旋转等几何变换时，是否使用抗锯齿算法进行平滑处理}

    // 抗锯齿图形绘制属性
    property PixelsF[x, y: Single]: TCnColor read GetPixelsF write SetPixelsF;
    {* 访问位图中的小数坐标的象素。如果位图为空或范围超限，将出现异常}
    property PenPosF: TPointF read FPenPosF write FPenPosF;
    {* 在抗锯齿图形绘制中，当前画笔的位置}
    property PenColor: TColor read FPenColor write FPenColor default clBlack;
    {* 在抗锯齿图形绘制中，当前画笔的颜色}
    property PenWeight: TPenWeight read FPenWeight write FPenWeight default pwNormal;
    {* 在抗锯齿图形绘制中，当前画笔的粗细程度}
  published
    property Transparent: Boolean read FTransparent write FTransparent default False;
    {* 图像的透明属性，在所有的图像绘制过程中有效，根据TransparentColor来判断}
    property TransparentColor: TColor read FTransparentColor write
      FTransparentColor default clDefault;
    {* 图像的透明色属性，位图中与该颜色相同的象素点按透明处理。
     |<BR> 当值为clDefault时，使用图像左下角象素颜色值来代替。}
  end;

procedure FreeBmpDC;
{* 释放程序中所有位图已分配的DC句柄}
procedure FreeBmpHandle(All: Boolean);
{* 释放程序中所有位图已分配的HBITMAP句柄，如果参数为假，根据GdiAllocStyle属性判断}

//--------------------------------------------------------//
// 公用运行时间过程库                                     //
//--------------------------------------------------------//

var
  HSLRange: Integer = 240;

// HSL颜色与RGB色转换函数
function HSLToRGB(H, S, L: Double): TColor;
{* HSL颜色转换为RGB颜色
 |<BR>
 |<BR> H, S, L: Double　　分别为色调、饱和度、亮度分量，为"0"到"1"之间的小数
 |<BR> Result: TColor　　　返回RGB颜色值}
function HSLRangeToRGB(H, S, L: Integer): TColor;
{* HSL颜色转换为RGB颜色
 |<BR>
 |<BR> H, S, L: Integer　　分别为色调、饱和度、亮度分量，0..240
 |<BR> Result: TColor　　　返回RGB颜色值}
procedure RGBToHSL(Color: TColor; out H, S, L: Double);
{* RGB颜色转换为HSL颜色
 |<BR>
 |<BR> Color: TColor　　　RGB颜色值
 |<BR> H, S, L: Integer　　输出分别为色调、饱和度、亮度分量，为"0"到"1"之间的小数}
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
{* RGB颜色转换为HSL颜色
 |<BR>
 |<BR> Color: TColor　　　RGB颜色值
 |<BR> H, S, L: Integer　　输出分别为色调、饱和度、亮度分量，0..240}

// CMY颜色与RGB色转换函数
function CMYToRGB(const C, M, Y: Byte): TColor;
{* CMY颜色转换为RGB颜色
 |<BR>
 |<BR> C, M, Y: Byte　　　分别为Cyan青、Magenta品红、Yellow黄分量，0..255
 |<BR> Result: TColor　　　返回RGB颜色值}
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
{* RGB颜色转换为CMY颜色
 |<BR>
 |<BR> Color: TColor　　　RGB颜色值
 |<BR> C, M, Y: Byte　　　输出分别为Cyan青、Magenta品红、Yellow黄分量，0..255}

// CMYK颜色与RGB色转换函数
function CMYKToRGB(const C, M, Y, K: Byte): TColor;
{* CMYK颜色转换为RGB颜色
 |<BR>
 |<BR> C, M, Y, K: Byte　　分别为Cyan青、Magenta品红、Yellow黄、Black黑分量，0..255
 |<BR> Result: TColor　　　返回RGB颜色值}
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
{* RGB颜色转换为CMY颜色
 |<BR>
 |<BR> Color: TColor　　　RGB颜色值
 |<BR> C, M, Y, K: Byte　　输出分别为Cyan青、Magenta品红、Yellow黄、Black黑分量，0..255}

// 增强的颜色处理函数
function Gray(Intensity: Byte): TColor;
{* 返回一个灰度RGB颜色值}
function Intensity(Color: TColor): Byte;
{* 计算RGB颜色值的灰度值}
function RandomColor: TColor;
{* 返回一个随机RGB颜色值}
procedure DeRGB(Color: TColor; var r, g, b: Byte);
{* 将Color分解为r、g、b颜色分量}

// CnColor颜色处理函数
function CnColor(r, g, b: Byte): TCnColor; overload;
{* 根据r、g、b颜色分量返回一个TCnColor颜色值}
function CnColor(Color: TColor): TCnColor; overload;
{* 转换TColor颜色为一个TCnColor颜色值，允许使用系统颜色值}
function CnGray(Intensity: Byte): TCnColor;
{* 返回一个灰度级的TCnColor颜色值}
function CnWinColor(RGB: TCnColor): TColor;
{* 转换TCnColor颜色为一个TColor颜色值}
function CnColorEqu(RGB1, RGB2: TCnColor): Boolean;
{* 判断两个TCnColor颜色是否相等}

function PointF(x, y: Single): TPointF;
{* 返回一个浮点数坐标TPointF}
function RectF(Left, Top, Right, Bottom: Single): TRectF;
{* 返回一个浮点数矩形TRectF}

// 从父控件复制背景。这个过程来自 RxLibrary VCLUtils。注意并不总是有效，尤其是 IDE 编辑器中
procedure CopyControlParentImageToCanvas(AControl: TControl; Dest: TCanvas);

// 常用卷积核
const
  BlurFilter: TFilterCore = (
    (-1, -1, -1),
    (-1, 1, -1),
    (-1, -1, -1));
  SharpFilter: TFilterCore = (
    (-5, -5, -5),
    (-5, 160, -5),
    (-5, -5, -5));
  EdgeFilter: TFilterCore = (
    (-1, -1, -1),
    (-1, 8, -1),
    (-1, -1, -1));
  EmbossFilter: TFilterCore = (
    (100, 0, 0),
    (0, 0, 0),
    (0, 0, -100));
  Enhance3DFilter: TFilterCore = (
    (-100, 5, 5),
    (5, 5, 5),
    (5, 5, 100));

implementation

type
  TGraphicAccess = class(TGraphic);
  TCnPersistentAccess = class(TCnPersistent);

var
  BitmapList: TThreadList;    // TCnBitmap 位图列表
  CnCanvasList: TThreadList;  // TCnCanvas 列表
  GdiActTimer: TTimer;        // GDI资源释放定时器
  DefGdiAllocStyle: TGdiAllocStyle = gsNormal; // 默认GDI释放方式
  FreeGdiWaitTime: Cardinal = 3000; // 自动释放GDI资源等待时间

const
  FreeGdiInterval: Cardinal = 1000; // 自动释放GDI资源定时间隔
  csItalicAdjust = 0.3;       // 斜体字宽度校正系数

type
  TLogPal = record
    lpal: TLogPalette;
    dummy: array[0..255] of TPaletteEntry;
  end;

var
  GrayLogPal: TLogPal;

//--------------------------------------------------------//
// 公用运行时间过程库                                     //
//--------------------------------------------------------//

// HSL、RGB转换函数算法来源：
// http:/www.r2m.com/win-developer-faq/graphics/8.html
// Grahame Marsh 12 October 1997

// HSL颜色转换为RGB色
function HSLToRGB(H, S, L: Double): TColor;
var
  M1, M2: Double;

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

// HSL颜色范围转换为RGB色
function HSLRangeToRGB(H, S, L: Integer): TColor;
begin
  Result := HSLToRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange)
end;

// RGB颜色转为HSL色
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

// RGB颜色转为HSL色范围
procedure RGBToHSLRange(Color: TColor; out H, S, L: Integer);
var
  Hd, Sd, Ld: Double;
begin
  RGBToHSL(Color, Hd, Sd, Ld);
  H := Round(Hd * (HSLRange - 1));
  S := Round(Sd * HSLRange);
  L := Round(Ld * HSLRange);
end;

// CMY颜色与RGB色转换函数
// 算法提供：CnPack开发组 铁男

// CMY颜色转换为RGB
function CMYToRGB(const C, M, Y: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - C;
  g := 255 - M;
  b := 255 - Y;
  Result := RGB(r, g, b);
end;

// RGB颜色转换为CMY
procedure RGBToCMY(const RGB: TColor; out C, M, Y: Byte);
var
  r, g, b: Byte;
begin
  DeRGB(RGB, r, g, b);
  C := 255 - r;
  M := 255 - g;
  Y := 255 - b;
end;

// CMYK颜色与RGB色转换函数
// 算法提供：CnPack开发组 铁男

// CMYK颜色转换为RGB
function CMYKtoRGB(const C, M, Y, K: Byte): TColor;
var
  r, g, b: Byte;
begin
  r := 255 - (C + K);
  g := 255 - (M + K);
  b := 255 - (Y + K);
  Result := RGB(r, g, b);
end;

// RGB颜色转换为CMYK
procedure RGBToCMYK(const RGB: TColor; out C, M, Y, K: Byte);
begin
  RGBToCMY(RGB, C, M, Y);
  K := MinIntValue([C, M, Y]);
  C := C - K;
  M := M - K;
  Y := Y - K;
end;

// 产生灰度颜色
function Gray(Intensity: Byte): TColor;
begin
  Result := Intensity shl 16 + Intensity shl 8 + Intensity;
end;

// 计算颜色亮度值
// 算法来源：Graphic32
// 算法修改：周劲羽
function Intensity(Color: TColor): Byte;
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

// 取颜色RGB分量
procedure DeRGB(Color: TColor; var r, g, b: Byte);
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
end;

// CnColor颜色处理函数
function CnColor(r, g, b: Byte): TCnColor;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
end;

// 系统颜色转为TCnColor
function CnColor(Color: TColor): TCnColor;
begin
  Color := ColorToRGB(Color);
  Result.r := Color;
  Result.g := Color shr 8;
  Result.b := Color shr 16;
end;

// 产生灰度级TCnColor
function CnGray(Intensity: Byte): TCnColor;
begin
  Result.r := Intensity;
  Result.g := Intensity;
  Result.b := Intensity;
end;

// TCnColor转为TColor
function CnWinColor(RGB: TCnColor): TColor;
begin
  Result := RGB.b shl 16 + RGB.g shl 8 + RGB.r;
end;

// 颜色值相等
function CnColorEqu(RGB1, RGB2: TCnColor): Boolean;
begin
  Result := (RGB1.r = RGB2.r) and (RGB1.g = RGB2.g) and (RGB1.b = RGB2.b);
end;

// 取点
function PointF(x, y: Single): TPointF;
begin
  Result.x := x;
  Result.y := y;
end;

// 取矩形
function RectF(Left, Top, Right, Bottom: Single): TRectF;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

//--------------------------------------------------------//
// 私有过程库                                             //
//--------------------------------------------------------//

// 范围转换
function RangeTran(Range, Low, High, Min, Max: Integer): Integer;
begin
  if (Low = High) or (Min = Max) then
    Result := 0
  else
  begin
    Range := TrimInt(Range, Low, High);
    Result := Round(Min + (Max - Min) / (High - Low) * (Range - Low));
  end;
end;

// 范围转换为实际值
function RangeToInt(Range: TAdjustRange; Min, Max: Integer): Integer;
begin
  Result := RangeTran(Range, Low(Range), High(Range), Min, Max);
end;

// 透明度转换
function AlphaToInt(Alpha: TCnAlpha): Integer;
begin
  Result := RangeTran(Alpha, Low(Alpha), High(Alpha), 0, 255);
end;

// 取矩形旋转后之外接矩形与目标矩形之交
function GetRotateRect(DstRect: TRect; DstCenter: TPoint; W, H: Integer;
  Angle: Double; var Rect: TRect): Boolean;
var
  p1, p2, p3, p4: TPoint;
  FAngle: Double;
  cAngle, sAngle: Double;
  wCos, hCos, wSin, hSin: Double;
  SrcW2, SrcH2: Double;
begin
  FAngle := Angle * Pi / 180;
  sAngle := Sin(FAngle);
  cAngle := Cos(FAngle);

  // 计算目标顶点位置
  SrcW2 := W / 2 + 1;
  SrcH2 := H / 2 + 1;
  wCos := SrcW2 * cAngle;
  hCos := SrcH2 * cAngle;
  wSin := SrcW2 * sAngle;
  hSin := SrcH2 * sAngle;
  p1.x := Round(-wCos - hSin + DstCenter.x); // 左上
  p1.y := Round(-wSin + hCos + DstCenter.y);
  p2.x := Round(wCos - hSin + DstCenter.x); // 右上
  p2.y := Round(wSin + hCos + DstCenter.y);
  p3.x := Round(-wCos + hSin + DstCenter.x); // 左下
  p3.y := Round(-wSin - hCos + DstCenter.y);
  p4.x := Round(wCos + hSin + DstCenter.x); // 右下
  p4.y := Round(wSin - hCos + DstCenter.y);

  // 计算包含矩形
  Rect.Left := MinIntValue([p1.x, p2.x, p3.x, p4.x]) - 1;
  Rect.Right := MaxIntValue([p1.x, p2.x, p3.x, p4.x]) + 1;
  Rect.Top := MinIntValue([p1.y, p2.y, p3.y, p4.y]) - 1;
  Rect.Bottom := MaxIntValue([p1.y, p2.y, p3.y, p4.y]) + 1;
  Result := IntersectRect(Rect, Rect, DstRect);
end;

//--------------------------------------------------------//
// 渐变颜色类                                             //
//--------------------------------------------------------//

{ TCnMiddleColorItem }

// 赋值
procedure TCnMiddleColorItem.Assign(Source: TPersistent);
begin
  if Source is TCnMiddleColorItem then
  begin
    FColor := TCnMiddleColorItem(Source).FColor;
    FPos := TCnMiddleColorItem(Source).FPos;
    Changed(False);
  end
  else
    inherited;                // TCollectionItem 未实现该方法
end;

// 初始化
constructor TCnMiddleColorItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColor := clBlack;
  FPos := 50;
end;

// 设颜色值
procedure TCnMiddleColorItem.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed(False);
  end;
end;

// 设位置值
procedure TCnMiddleColorItem.SetPos(const Value: TCnGradPos);
begin
  if FPos <> Value then
  begin
    FPos := Value;
    Changed(False);
  end;
end;

{ TCnMiddleColor }

// 初始化
constructor TCnMiddleColor.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnMiddleColorItem);
  FSorting := False;
end;

// 增加一项
procedure TCnMiddleColor.Add(AColor: TColor; APos: TCnGradPos);
begin
  BeginUpdate;
  try
    with TCnMiddleColorItem(inherited Add) do
    begin
      FColor := AColor;
      FPos := APos;
    end;
  finally
    EndUpdate;
  end;
end;

// 按位置排序
procedure TCnMiddleColor.Sort;
var
  I, J, Idx, Pos: Integer;
  Item: TCnMiddleColorItem;
begin
  if FSorting then Exit;
  FSorting := True;
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
    begin
      Pos := Items[I].FPos;
      Idx := I;
      for J := I + 1 to Count - 1 do
      begin
        Item := Items[J];
        if Item.FPos < Pos then
        begin
          Pos := Item.FPos;
          Idx := J;
        end;
      end;
      if Idx <> I then
        Items[Idx].Index := I;
    end;
    if GetOwner is TCnPersistent then // 通知更新
      TCnPersistentAccess(GetOwner).Changed;
  finally
    EndUpdate;
    FSorting := False;
  end;
end;

// 内容已更新
procedure TCnMiddleColor.Update(Item: TCollectionItem);
begin
  inherited;
  Sort;
end;

// 下面的方法用在设计期属性编辑器中
// 显示在属性编辑器中的栏名
function TCnMiddleColor.GetAttr(Index: Integer): string;
begin
  case Index of
    0: Result := 'Color';
    1: Result := 'Position';
  else Result := inherited GetAttr(Index);
  end;
end;

// 显示在属性编辑器中的栏数
function TCnMiddleColor.GetAttrCount: Integer;
begin
  Result := 2;
end;

// 显示在属性编辑器中的子项内容
function TCnMiddleColor.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  case Index of
    0: Result := ColorToString(Items[ItemIndex].FColor);
    1: Result := Format('[%d%%]', [Items[ItemIndex].FPos]);
  else Result := inherited GetItemAttr(Index, ItemIndex);
  end;
end;

// 取子项
function TCnMiddleColor.GetItem(Index: Integer): TCnMiddleColorItem;
begin
  Result := TCnMiddleColorItem(inherited GetItem(Index));
end;

// 设子项
procedure TCnMiddleColor.SetItem(Index: Integer;
  const Value: TCnMiddleColorItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCnGradientColor }

// 赋值
procedure TCnGradientColor.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnGradientColor then
    begin
      FColorStart := TCnGradientColor(Source).FColorStart;
      FColorEnd := TCnGradientColor(Source).FColorEnd;
      FStyle := TCnGradientColor(Source).FStyle;
      ColorMiddle := TCnGradientColor(Source).FColorMiddle;
    end
    else
      inherited;
  finally
    EndUpdate;
  end;
end;

// 初始化
constructor TCnGradientColor.Create;
begin
  inherited;
  FColorStart := clBlack;
  FColorEnd := clBlack;
  FStyle := gsLeftToRight;
  FColorMiddle := nil;
end;

// 释放
destructor TCnGradientColor.Destroy;
begin
  if FColorMiddle <> nil then
    FreeAndNil(FColorMiddle);
  inherited;
end;

// 取中间色
function TCnGradientColor.GetColorMiddle: TCnMiddleColor;
begin
  if FColorMiddle = nil then
    FColorMiddle := TCnMiddleColor.Create(Self);
  Result := FColorMiddle;
end;

// 设结束色
procedure TCnGradientColor.SetColorEnd(const Value: TColor);
begin
  if FColorEnd <> Value then
  begin
    FColorEnd := Value;
    Changed;
  end;
end;

// 设中间色
procedure TCnGradientColor.SetColorMiddle(const Value: TCnMiddleColor);
begin
  BeginUpdate;
  try
    if (Value <> nil) and (Value.Count > 0) then
      ColorMiddle.Assign(Value) // 自动调用Get方法保证实例化
    else if FColorMiddle <> nil then // 无中间色时释放实例
      FreeAndNil(FColorMiddle);
  finally
    EndUpdate;
  end;
end;

// 设起始色
procedure TCnGradientColor.SetColorStart(const Value: TColor);
begin
  if FColorStart <> Value then
  begin
    FColorStart := Value;
    Changed;
  end;
end;

// 设渐变风格
procedure TCnGradientColor.SetStyle(const Value: TCnGradStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

//--------------------------------------------------------//
// 平滑特效字体类                                         //
//--------------------------------------------------------//

{ TCnShadow }

// 赋值
procedure TCnShadow.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnShadow then
    begin
      FBlur := TCnShadow(Source).FBlur;
      FAlpha := TCnShadow(Source).FAlpha;
      FColor := TCnShadow(Source).FColor;
      FOffsetX := TCnShadow(Source).FOffsetX;
      FOffsetY := TCnShadow(Source).FOffsetY;
    end
    else
      inherited;
  finally
    EndUpdate;
  end;
end;

// 初始化
constructor TCnShadow.Create;
begin
  inherited;
  FBlur := 1;
  FAlpha := 180;
  FColor := $00444444;
  FOffsetX := 2;
  FOffsetY := 2;
end;

// 设置不透明度
procedure TCnShadow.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// 设置阴影模糊
procedure TCnShadow.SetBlur(const Value: TShadowBlur);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Changed;
  end;
end;

// 设置阴影色
procedure TCnShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// 设置阴影水平偏移
procedure TCnShadow.SetOffsetX(const Value: TShadowOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// 设置阴影垂直偏移
procedure TCnShadow.SetOffsetY(const Value: TShadowOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnLighting }

// 赋值
procedure TCnLighting.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnLighting then
    begin
      FAlpha := TCnLighting(Source).FAlpha;
      FColor := TCnLighting(Source).FColor;
      FOffsetX := TCnLighting(Source).FOffsetX;
      FOffsetY := TCnLighting(Source).FOffsetY;
      FWidth := TCnLighting(Source).FWidth;
      FHeight := TCnLighting(Source).FHeight;
      FAngle := TCnLighting(Source).FAngle;
    end
    else
      inherited;
  finally
    EndUpdate;
  end;
end;

// 初始化
constructor TCnLighting.Create;
begin
  inherited;
  FAlpha := 180;
  FColor := clWhite;
  FOffsetX := 0;
  FOffsetY := 0;
  FWidth := 80;
  FHeight := 80;
  FAngle := 0;
end;

// 设置不透明度
procedure TCnLighting.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// 设置角度
procedure TCnLighting.SetAngle(const Value: Double);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

// 设置灯光颜色
procedure TCnLighting.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// 设置光照范围宽度
procedure TCnLighting.SetWidth(const Value: TLightingRange);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

// 设置光照范围高度
procedure TCnLighting.SetHeight(const Value: TLightingRange);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

// 设置光照中心偏移
procedure TCnLighting.SetOffsetX(const Value: TLightingOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// 设置光照中心偏移
procedure TCnLighting.SetOffsetY(const Value: TLightingOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnFont }

// 赋值
procedure TCnFont.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source is TCnFont then
    begin
      FStyleEx := TCnFont(Source).FStyleEx;
      FNoise := TCnFont(Source).FNoise;
      FSpray := TCnFont(Source).FSpray;
      FAlpha := TCnFont(Source).FAlpha;
      FTextureMode := TCnFont(Source).FTextureMode;
      Quality := TCnFont(Source).FQuality;
      FTexture.Assign(TCnFont(Source).FTexture);
      FShadow.Assign(TCnFont(Source).FShadow);
      FGradient.Assign(TCnFont(Source).FGradient);
    end;
    inherited;
  finally
    EndUpdate;
  end;
end;

// 初始化
constructor TCnFont.Create;
begin
  inherited;
  FNoise := 0;
  FSpray := 0;
  FAlpha := csMaxAlpha;
  FStyleEx := [];
  FTextureMode := dmTiled;
  FQuality := fqNormal;
  FScale := 3;
  FShadow := TCnShadow.Create(ChildChanged);
  FGradient := TCnGradientColor.Create(ChildChanged);
  FLighting := TCnLighting.Create(ChildChanged);
end;

// 释放
destructor TCnFont.Destroy;
begin
  FLighting.Free;
  FGradient.Free;
  FShadow.Free;
  if FTexture <> nil then FTexture.Free;
  inherited;
end;

// 开始更新
procedure TCnFont.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// 取所有者
function TCnFont.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// 结束更新
procedure TCnFont.EndUpdate;
begin
  Assert(FUpdateCount > 0, 'Unpaired TCnFont.EndUpdate');
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then Changed;
end;

// 属性已变更
procedure TCnFont.Changed;
begin
  if FUpdateCount = 0 then inherited;
end;

// 子元素更新通知
procedure TCnFont.ChildChanged(Sender: TObject);
begin
  if (Sender is TCnShadow) and (fsShadow in FStyleEx) then
    Changed
  else if (Sender is TCnGradientColor) and (fsGradient in FStyleEx) then
    Changed
  else if (Sender is TCnLighting) and (fsLighting in FStyleEx) then
    Changed
  else if (Sender is TPicture) and (fsTexture in FStyleEx) then
    Changed;
end;

// 取纹理
function TCnFont.GetTexture: TPicture;
begin
  if FTexture = nil then
  begin
    FTexture := TPicture.Create;
    FTexture.OnChange := ChildChanged;
  end;
  Result := FTexture;
end;

// 设置透明度
procedure TCnFont.SetAlpha(const Value: TCnAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// 设置渐变色
procedure TCnFont.SetGradient(const Value: TCnGradientColor);
begin
  BeginUpdate;
  try
    FGradient.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// 设置噪声
procedure TCnFont.SetNoise(const Value: Byte);
begin
  if FNoise <> Value then
  begin
    FNoise := Value;
    Changed;
  end;
end;

// 设置喷溅
procedure TCnFont.SetSpray(const Value: Byte);
begin
  if FSpray <> Value then
  begin
    FSpray := Value;
    Changed;
  end;
end;

// 设置阴影
procedure TCnFont.SetShadow(const Value: TCnShadow);
begin
  BeginUpdate;
  try
    FShadow.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// 设置光照
procedure TCnFont.SetLighting(const Value: TCnLighting);
begin
  BeginUpdate;
  try
    FLighting.Assign(Value);
  finally
    EndUpdate;
  end;
end;

// 设置扩展风格
procedure TCnFont.SetStyleEx(const Value: TFontStyleExs);
begin
  if FStyleEx <> Value then
  begin
    FStyleEx := Value;
    Changed;
  end;
end;

// 设置纹理
procedure TCnFont.SetTexture(const Value: TPicture);
begin
  BeginUpdate;
  try
    if (Value <> nil) and (Value.Graphic <> nil) and not Value.Graphic.Empty then
      Texture.Assign(Value)
    else if FTexture <> nil then
      FreeAndNil(FTexture)
  finally
    EndUpdate;
  end;
end;

// 设置纹理模式
procedure TCnFont.SetTextureMode(const Value: TCnDrawMode);
begin
  if FTextureMode <> Value then
  begin
    FTextureMode := Value;
    Changed;
  end;
end;

// 设置显示精度
procedure TCnFont.SetQuality(const Value: TFontQuality);
begin
  if FQuality <> Value then
  begin
    FQuality := Value;
    case FQuality of
      fqHigh: FScale := 4;
      fqNormal: FScale := 3;
      fqLow: FScale := 2;
      fqNone: FScale := 1;
    end;
    Changed;
  end;
end;

{ TCnFontMask }

// 模糊处理
procedure TCnFontMask.SplitBlur(Amount: Integer);
var
  p1, p2, Dst: PByteArray;
  x1, x2: Integer;
  x, y: Integer;
  Tmp: TCnFontMask;
begin
  Tmp := TCnFontMask.Create;
  try
    CopyTo(Tmp);
    for y := 0 to FHeight - 1 do
    begin
      p1 := Tmp.ScanLine[TrimInt(y + Amount, 0, FHeight - 1)];
      p2 := Tmp.ScanLine[TrimInt(y - Amount, 0, FHeight - 1)];
      Dst := ScanLine[y];
      for x := 0 to FWidth - 1 do
      begin
        x1 := TrimInt(x - Amount, 0, FWidth - 1);
        x2 := TrimInt(x + Amount, 0, FWidth - 1);
        Dst[x] := (p1[x1] + p1[x2] + p2[x1] + p2[x2]) shr 2;
      end;
    end;
  finally
    Tmp.Free;
  end;
end;

// 高斯模糊处理
procedure TCnFontMask.Blur(Amount: Integer);
var
  I: Integer;
begin
  if Amount > 0 then
    for I := Amount downto 1 do
      SplitBlur(I);
end;

// 喷溅效果
procedure TCnFontMask.Spray(Amount: Integer);
var
  r, x, y: Integer;
begin
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
    begin
      r := Random(Amount);
      ScanLine[y][x] := ScanLine
        [TrimInt(y + (r - Random(r * 2)), 0, FHeight - 1)]
        [TrimInt(x + (r - Random(r * 2)), 0, FWidth - 1)];
    end;
end;

// 复制
procedure TCnFontMask.CopyTo(Dst: TCnFontMask);
begin
  Dst.SetSize(FWidth, FHeight);
  Move(FBuff^, Dst.Buff^, FRowInc * FHeight);
end;

// 释放
destructor TCnFontMask.Destroy;
begin
  SetSize(0, 0);
  inherited;
end;

// 轮廓化（Sobel算子边缘检测）
procedure TCnFontMask.Outline;
var
  x, y: Integer;
  s1, s2, s3, s4, Sum: Integer;
  Tmp: TCnFontMask;
  pDst: PByteArray;
  pUp, pMiddle, pDown: PByteArray; //卷积用指针
begin
  Tmp := TCnFontMask.Create;
  try
    CopyTo(Tmp);
    for y := 1 to Height - 2 do
    begin
      pUp := Tmp.ScanLine[y - 1];
      pMiddle := Tmp.ScanLine[y];
      pDown := Tmp.ScanLine[y + 1];
      pDst := ScanLine[y];
      for x := 1 to Width - 2 do
      begin
        s1 := Abs(pDown^[x] - pUp^[x]);
        s2 := Abs(pMiddle^[x + 1] - pMiddle^[x - 1]);
        s3 := Abs(pDown^[x - 1] - pUp^[x + 1]);
        s4 := Abs(pDown^[x + 1] - pUp^[x - 1]);
        Sum := (s1 + s2 + s3 + s4) shr 2;
        if Sum > 255 then
          pDst^[x] := 255
        else
          pDst^[x] := Sum;
      end;
    end;
  finally
    Tmp.Free;
  end;
end;

// 扫描线
function TCnFontMask.GetScanLine(Row: Integer): PByteArray;
begin
  Result := PByteArray(TCnNativeInt(FBuff) + Row * FRowInc);
end;

// 设置大小
procedure TCnFontMask.SetSize(AWidth, AHeight: Integer);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then Exit;
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  FWidth := AWidth;
  FHeight := AHeight;
  FRowInc := (AWidth + 3) div 4 * 4;
  ReallocMem(FBuff, FRowInc * FHeight);
end;

//--------------------------------------------------------//
// 快速图像处理类                                         //
//--------------------------------------------------------//

{ TCnCanvas }

// 初始化
constructor TCnCanvas.Create(ABitmap: TCnBitmap);
begin
  inherited Create;
  FDC := 0;
  FBitmap := ABitmap;
end;

// 释放
destructor TCnCanvas.Destroy;
begin
  FreeContext;
  inherited;
end;

// 取DC
function TCnCanvas.GetHandle: HDC;
begin
  Result := inherited Handle; // 取继承而来的句柄
end;

// 创建DC，重载方法，由TCanvas内部调用
procedure TCnCanvas.CreateHandle;
var
  H: HDC;
begin
  Lock;
  try
    FBitmap.HandleNeeded;     // 分配位图句柄
    H := CreateCompatibleDC(0); // 创建兼容DC
    if H = 0 then raise ECnGraphics.Create(SCreateDCFail);
    if SelectObject(H, FBitmap.FHandle) = 0 then // 选择位图到内部DC
      raise ECnGraphics.Create(SSelectBmpToDCFail);
    FDC := H;
    inherited Handle := H;    // 设置继承而来的句柄
    CnCanvasList.Add(Self);   // 增加自身到列表中
  finally
    Unlock;
  end;
end;

// 释放DC
procedure TCnCanvas.FreeContext;
begin
  if FDC <> 0 then
  begin
    Lock;
    try
      inherited Handle := 0;  // 释放继承而来的句柄
      DeleteDC(FDC);          // 删除DC
      FDC := 0;
      CnCanvasList.Remove(Self); // 从列表中删除
    finally
      Unlock;
    end;
  end;
end;

{ TCnBitmap }

// 初始化
constructor TCnBitmap.Create;
begin
  inherited;
  FSmoothFilter := True;
  FTransparent := False;
  FTransparentColor := clDefault;
  FGdiAllocStyle := DefGdiAllocStyle;
  FPenColor := clBlack;
  FPenWeight := pwNormal;
  FFontClear := False;
  FFontBkColor := clWhite;
end;

// 释放
destructor TCnBitmap.Destroy;
begin
  Lock;                       // 多线程同步
  try
    CleanUp;
    if FFont <> nil then FFont.Free;
    if FCanvas <> nil then FCanvas.Free;
    FreeGrayBmp;
  finally
    Unlock;
  end;
  inherited;
end;

//--------------------------------------------------------//
// 动态位图资源、DC处理部分                               //
// 算法设计：周劲羽                                       //
// 算法参考：Graphic32、pnBitmap、Graphic.pas             //
//--------------------------------------------------------//

// 释放所有内存DC
procedure FreeBmpDC;
var
  I: Integer;
  Canvas: TCnCanvas;
begin
  with CnCanvasList.LockList do // 如果其它线程也在访问则等待
  try
    for I := Count - 1 downto 0 do
    begin
      Canvas := TCnCanvas(Items[I]);
      if Canvas.TryLock then
      try
        Canvas.FreeContext;   // 释放DC
      finally
        Canvas.Unlock;
      end;
    end;
  finally
    CnCanvasList.UnlockList;
  end;
end;

// 释放位图句柄
procedure FreeBmpHandle(All: Boolean);
var
  I: Integer;
  Bmp: TCnBitmap;
begin
  with BitmapList.LockList do // 如果其它线程也在访问则等待
  try
    for I := Count - 1 downto 0 do
    begin
      Bmp := TCnBitmap(Items[I]);
      if (All or (Bmp.GdiAllocStyle = gsInternal) or (Bmp.GdiAllocStyle =
        gsNormal) and (GetTickCount - Bmp.FGdiLastAccess > FreeGdiWaitTime))
        and Bmp.HandleAllocated then
      begin
        if Bmp.TryLock then
        try
          Bmp.HandleRelease;  // 释放位图句柄
        finally
          Bmp.Unlock;
        end;
      end;
    end;
  finally
    BitmapList.UnlockList;
  end;
end;

// 位图资源定时管理（类方法）
// TTimer.OnTimer要求一个对象方法，故定义该类方法
class procedure TCnBitmap.OnGdiActTimer(Sender: TObject);
begin
  FreeBmpDC;
  FreeBmpHandle(False);
end;

// 取Canvas
function TCnBitmap.GetCanvas: TCnCanvas;
begin
  if FCanvas = nil then       // 第一次访问时创建
  begin
    FCanvas := TCnCanvas.Create(Self);
    FCanvas.OnChange := OnChildChange;
    FCanvas.OnChanging := OnChildChanging;
  end;
  FGdiLastAccess := GetTickCount; // 最后访问画布的时间
  Result := FCanvas;
end;

// 释放句柄、资源，清变量（保护方法）
procedure TCnBitmap.CleanUp;
begin
  if HandleAllocated then     // 已分配位图句柄
    HandleRelease(False)      // 释放位图资源
  else
  begin
    if Assigned(FBits) then FreeMem(FBits); // 释放位图数据块
    if Assigned(FScanLine) then FreeMem(FScanLine); // 删除扫描线数组
    FBits := nil;
    FScanLine := nil;
    FWidth := 0;
    FHeight := 0;
    FSize := 0;
    FRowInc := 0;
    FGap := 0;
    FPenPosF.x := 0;
    FPenPosF.y := 0;
  end;
end;

// 释放句柄、清变量
procedure TCnBitmap.FreeImage;
begin
  CleanUp;
  Changed;
end;

// 更新扫描线指针数组
procedure TCnBitmap.UpdateScanLine;
var
  I: Integer;
  x: TCnNativeInt;
begin
  ReallocMem(FScanLine, FHeight * SizeOf(PCnLine)); // 重新分配扫描线指针数组空间
  x := TCnNativeInt(FBits);
  for I := 0 to Height - 1 do
  begin
    FScanLine[I] := Pointer(x); // 初始化扫描线指针数组内容
    Inc(x, FRowInc);
  end;
end;

// GDI句柄已分配
function TCnBitmap.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

// 需要HBITMAP句柄
procedure TCnBitmap.HandleNeeded;
var
  Tmp: Pointer;
begin
  if HandleAllocated then Exit;
  if Empty then               // 不能为空位图分配句柄
    raise EBitmapIsEmpty.Create(SCreateDCFromEmptyBmp);

  Lock;
  try
    FillChar(FBitmapInfo, SizeOf(TBitmapInfo), 0);
    with FBitmapInfo.bmiHeader do // 初始化FBitmapInfo
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biWidth := Width;
      biHeight := -Height;    // 高度为负，扫描线按低地址到高地址方式存放
      biPlanes := 1;
      biBitCount := 24;       // 24Bit RGB位图
      biCompression := BI_RGB;
    end;                      // 原图像数据
    Tmp := FBits;             // 创建位图
    FHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, FBits, 0, 0);
    if FHandle = 0 then       // 无法创建位图句柄错
      raise ECnGraphics.Create(SAllocDIBFail);
    Move(Tmp^, FBits^, Size); // 移动原数据到新数据区
    FreeMem(Tmp);             // 释放原数据块
    UpdateScanLine;           // 更新扫描线指针数组内容
    BitmapList.Add(Self);     // 增加到位图列表
  finally
    Unlock;
  end;
end;

// 释放HBITMAP位图句柄（参数为True将保留位图数据）
procedure TCnBitmap.HandleRelease(KeepData: Boolean);
var
  Tmp: Pointer;
begin
  if not HandleAllocated then Exit;

  if Assigned(FCanvas) then
    TCnCanvas(FCanvas).FreeContext; // 释放DC
  Lock;
  try
    if KeepData then          // 保留数据，仅删除位图句柄
    begin
      Tmp := FBits;
      GetMem(FBits, FSize);
      Move(Tmp^, FBits^, FSize);
      UpdateScanLine;         // 更新扫描线指针数组内容
    end else
    begin                     // 同时释放位图数据
      if Assigned(FScanLine) then FreeMem(FScanLine); // 删除扫描线数组
      FBits := nil;
      FScanLine := nil;
      FWidth := 0;
      FHeight := 0;
      FSize := 0;
      FRowInc := 0;
      FGap := 0;
      FPenPosF.x := 0;
      FPenPosF.y := 0;
    end;
    DeleteObject(FHandle);    // 删除位图对象
    FHandle := 0;
    BitmapList.Remove(Self);  // 从位图列表中删除
  finally
    Unlock;
  end;
end;

// 取位图句柄HBITMAP
function TCnBitmap.GetHandle: HBITMAP;
begin
  if not HandleAllocated then HandleNeeded;
  Result := FHandle;
end;

// 取得DC
function TCnBitmap.GetDC: HDC;
begin
  Result := Canvas.Handle;
end;

// 取图像空状态
function TCnBitmap.GetEmpty: Boolean;
begin
  Result := FBits = nil;
end;

// 设置图像尺寸
procedure TCnBitmap.SetSize(AWidth, AHeight: Integer);
begin
  if AWidth < 0 then AWidth := 0;
  if AHeight < 0 then AHeight := 0;
  if (AWidth = Width) and (AHeight = Height) then Exit;

  Changing;
  CleanUp;                    // 释放原内容
  FWidth := AWidth;
  FHeight := AHeight;
  if (AWidth > 0) and (AHeight > 0) then
  begin
    FRowInc := (FWidth * 3) + FWidth mod 4; // 每行扫描线长度，按4字节对齐
    FGap := FWidth mod 4;     // 每行扫描线最后的无效字数
    FSize := FRowInc * FHeight; // 图像大小
    GetMem(FBits, FSize);     // 分配图像空间
    UpdateScanLine;           // 更新扫描线指针数组内容
    //Fill(CnWinColor(GetTranColor));  // 以透明色填充
  end;
  Changed;
end;

// 设置图像尺寸（TRect类型参数）
procedure TCnBitmap.SetSize(ARect: TRect);
begin
  SetSize(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
end;

// 设置高度
procedure TCnBitmap.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then SetSize(FWidth, Value);
end;

// 设置宽度
procedure TCnBitmap.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then SetSize(Value, FHeight);
end;

//--------------------------------------------------------//
// 辅助代码                                               //
//--------------------------------------------------------//

// 数据压缩，返回目标数据长度
function TCnBitmap.Compress(var pData: Pointer; ASize: Integer): Integer;
begin
  Result := ASize;            { TODO -o周劲羽 -c压缩解压 : 位图数据压缩解压。 }
end;

// 数据解压，返回目标数据长度（出错返回-1）
function TCnBitmap.DeCompress(var pData: Pointer; ASize: Integer): Integer;
begin
  Result := ASize;
end;

// 取绘制矩形区
function TCnBitmap.GetClientRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

// 取透明色
function TCnBitmap.GetTranColor: TCnColor;
begin
  if TransparentColor <> clDefault then
    Result := CnColor(TransparentColor)
  else if not Empty then
    Result := Pixels[0, Height - 1] // 默认为左下角象素（与TBitmap保持一致）
  else
    Result := CnColor(0, 0, 0);
end;

// 取平滑字体
function TCnBitmap.GetFont: TCnFont;
begin
  if FFont = nil then
  begin
    FFont := TCnFont.Create;
    FFont.OnChange := OnChildChange;
  end;
  Result := FFont;
end;

// 设置平滑字体
procedure TCnBitmap.SetFont(const Value: TCnFont);
begin
  if Value <> nil then
    Font.Assign(Value);       // 自动调用GetFont方法保证实例化
end;

// 以指定色填充位图
procedure TCnBitmap.Fill(Color: TColor);
begin
  FillRect(ClientRect, Color);
end;

// 以指定色填充矩形区域
// 算法：周劲羽
procedure TCnBitmap.FillRect(Rect: TRect; Color: TColor);
var
  x, y, w, h, I, lw: Integer;
  ARect: TRect;
  Tmp: PCnColor;
  ARGB: TCnColor;
begin
  if Empty then
    Exit;         // 无有效区域
  if not IntersectRect(ARect, Rect, ClientRect) then Exit;
  Changing;
  if (Color = clBlack) and RectEqu(ARect, ClientRect) then
    FillChar(Bits^, Size, 0)  // 全区域清零
  else
  begin
    DeRect(ARect, x, y, w, h);
    lw := w * 3;              // 存储宽度
    ARGB := CnColor(Color);   // 转换为RGB格式
    Tmp := @FScanLine[y][x];
    if Color = clBlack then   // 黑色全部清零
      FillChar(Tmp^, lw, 0)
    else
    begin
      for I := 0 to w - 1 do  // 填充第一行扫描线
      begin
        Tmp^ := ARGB;
        Inc(Tmp);
      end;
    end;
    Tmp := @FScanLine[y][x];
    for I := y + 1 to y + h - 1 do // 复制到其它扫描线
      Move(Tmp^, (@FScanLine[I][x])^, lw);
  end;
  Changed;
end;

// 以指定色画线
procedure TCnBitmap.DrawLine(x1, y1, x2, y2: Integer; Color: TColor);
var
  x, y: Integer;
  l, r, t, b: Integer;
  RGB: TCnColor;
begin
  if Empty then Exit;
  if (InBound(x1, 0, Width - 1) or InBound(x2, 0, Width - 1)) and
    (InBound(y1, 0, Height - 1) or InBound(y2, 0, Height - 1)) then
  begin
    RGB := CnColor(Color);
    l := TrimInt(Min(x1, x2), 0, Width - 1);
    r := TrimInt(Max(x1, x2), 0, Width - 1);
    t := TrimInt(Min(y1, y2), 0, Height - 1);
    b := TrimInt(Max(y1, y2), 0, Height - 1);
    if x1 = x2 then
      for y := t to b do
        FScanLine[y][x1] := RGB
    else if y1 = y2 then
      for x := l to r do
        FScanLine[y1][x] := RGB
    else if Abs(x1 - x2) > Abs(y1 - y2) then
      for x := l to r do
        FScanLine[Round((x - x1) / (x2 - x1) * (y2 - y1)) + y1][x] := RGB
    else
      for y := t to b do
        FScanLine[y][Round((y - y1) / (y2 - y1) * (x2 - x1)) + x1] := RGB;
  end;
end;

// 以指定色画矩形
procedure TCnBitmap.FrameRect(Rect: TRect; Color: TColor);
begin
  if Empty then
    Exit;

  with Rect do
  begin
    DrawLine(Left, Top, Left, Bottom, Color);
    DrawLine(Left, Bottom, Right, Bottom, Color);
    DrawLine(Right, Bottom, Right, Top, Color);
    DrawLine(Right, Top, Left, Top, Color);
  end;
end;

// 以当前图像创建一个Region
function TCnBitmap.CreateRegion(var RgnData: PRgnData): Integer;
const
  Max = 10000;
var
  x, y, Tmp: Integer;
  Rts: array[0..Max] of TRect;
  Count: Integer;
  TranColor, c: TCnColor;
begin
  Result := 0;
  if Empty then
    Exit;

  Count := 0;
  TranColor := GetTranColor;
  for y := 0 to Height - 1 do
  begin
    x := 0;
    while x < Width do
    begin
      c := Pixels[x, y];
      if not CnColorEqu(c, TranColor) then
      begin
        Tmp := x;
        c := Pixels[Tmp, y];
        while not CnColorEqu(c, TranColor) do
        begin
          Inc(Tmp);
          c := Pixels[Tmp, y];
          if Tmp >= Width then Break;
        end;
        Rts[Count] := Rect(x, y, Tmp, y + 1);
        Inc(Count);
        x := Tmp;
        Continue;
      end;
      Inc(x);
    end;
  end;
  // 创建Region数据
  Result := Count * SizeOf(TRect);
  GetMem(Rgndata, SizeOf(TRgnDataHeader) + Result);
  FillChar(Rgndata^, SizeOf(TRgnDataHeader) + Result, 0);
  RgnData^.rdh.dwSize := SizeOf(TRgnDataHeader);
  RgnData^.rdh.iType := RDH_RECTANGLES;
  RgnData^.rdh.nCount := Count;
  RgnData^.rdh.nRgnSize := 0;
  RgnData^.rdh.rcBound := Rect(0, 0, Width, Height);
  // 更新Region
  Move(Rts, RgnData^.Buffer, Result);
  Result := SizeOf(TRgnDataHeader) + Count * SizeOf(TRect);
end;

//--------------------------------------------------------//
// 赋值操作                                               //
// 算法来源：Graphic32、pnBitmap                          //
// 算法修改：周劲羽                                       //
//--------------------------------------------------------//

// 赋值操作
procedure TCnBitmap.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    if Source = nil then      // 赋空值置空
    begin
      SetSize(0, 0);
      Exit;
    end
    else if Source is TCnBitmap then
    begin
      SetSize(TCnBitmap(Source).Width, TCnBitmap(Source).Height);
      CopyMemory(Bits, TCnBitmap(Source).Bits, Size);
      FSmoothFilter := TCnBitmap(Source).FSmoothFilter;
      FTransparentColor := TCnBitmap(Source).FTransparentColor;
      FTransparent := TCnBitmap(Source).FTransparent;
      Exit;
    end
    else if Source is TBitmap then
    begin
      SetSize(TBitmap(Source).Width, TBitmap(Source).Height);
      if not Empty then
        Bitblt(DC, 0, 0, Width, Height, TBitmap(Source).Canvas.Handle, 0, 0, SRCCOPY);
      FTransparentColor := TBitmap(Source).TransparentColor;
      FTransparent := TBitmap(Source).Transparent;
      Exit;
    end
    else if Source is TGraphic then // TIcon、TJpegImage等等
    begin
      SetSize(TGraphic(Source).Width, TGraphic(Source).Height);
      if not Empty then
      begin
        Fill(CnWinColor(GetTranColor));
        TGraphicAccess(Source).Draw(Canvas, Rect(0, 0, Width, Height));
      end;
      FTransparent := TGraphicAccess(Source).Transparent;
      Exit;
    end
    else if Source is TPicture then
    begin
      Assign(TPicture(Source).Graphic);
      Exit;
    end
    else
      inherited;
  finally
    if GdiAllocStyle = gsInternal then HandleRelease; // 释放句柄
    EndUpdate;
  end;
end;

// 赋值到目标（保护方法）
procedure TCnBitmap.AssignTo(Dest: TPersistent);
var
  Bmp: TBitmap;
begin
  try
    if Dest is TBitmap then
    begin
      TBitmap(Dest).HandleType := bmDIB;
      TBitmap(Dest).PixelFormat := pf24Bit;
      TBitmap(Dest).Width := Width;
      TBitmap(Dest).Height := Height;
      TBitmap(Dest).TransparentColor := FTransparentColor;
      TBitmap(Dest).Transparent := FTransparent;
      if not Empty then
        Bitblt(TBitmap(Dest).Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY);
      Exit;
    end
    else if Dest is TGraphic then // TIcon、TJpegImage等等
    begin
      Bmp := TBitmap.Create;
      try
        AssignTo(Bmp);
        Dest.Assign(Bmp);
      finally
        Bmp.Free;
      end;
      Exit;
    end
    else if Dest is TPicture then
    begin
      AssignTo(TPicture(Dest).Graphic);
      Exit;
    end
    else
      inherited;
  finally
    if GdiAllocStyle = gsInternal then HandleRelease; //释放句柄
  end;
end;

//--------------------------------------------------------//
// 存储数据属性                                           //
// 算法来源：Delphi Graphic.pas                           //
// 算法修改：周劲羽                                       //
//--------------------------------------------------------//

// 定义存储属性（保存在DFM中的自定义属性）
procedure TCnBitmap.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then // 是从基窗体继承而来
      Result := not (Filer.Ancestor is TCnBitmap) or
        not Equals(TCnBitmap(Filer.Ancestor))
    else
      Result := not Empty;
  end;
begin                         // 定义属性Data保存图像数据
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

// 从DFM流中读数据过程
procedure TCnBitmap.ReadData(Stream: TStream);
var
  AWidth, AHeight, ASize: Integer;
  Buff: Pointer;
begin
  Stream.Read(AWidth, SizeOf(AWidth));
  Stream.Read(AHeight, SizeOf(AHeight));
  Stream.Read(ASize, SizeOf(ASize));
  if ASize = 0 then Exit;
  GetMem(Buff, ASize);
  try
    Stream.Read(Buff^, ASize);
    if DeCompress(Buff, ASize) > 0 then // 数据解压
      LoadFromMemory(Buff, AWidth, AHeight)
    else
      raise ECnGraphics.Create(SReadBmpError);
  finally
    FreeMem(Buff);
  end;
end;

// 写数据到DFM流过程
procedure TCnBitmap.WriteData(Stream: TStream);
var
  ASize: Integer;
  Buff: Pointer;
begin
  Stream.Write(Width, SizeOf(Width));
  Stream.Write(Height, SizeOf(Height));
  if Size = 0 then
    Stream.Write(Size, SizeOf(Size))
  else
  begin
    GetMem(Buff, Size);
    try
      Move(Bits^, Buff^, Size); // 临时缓冲
      ASize := Compress(Buff, Size); // 压缩数据
      Stream.Write(ASize, SizeOf(ASize));
      Stream.Write(Buff^, ASize);
    finally
      FreeMem(Buff);
    end;
  end;
end;

// 判断两个图像数据是否全等
function TCnBitmap.Equals(Obj: TObject): Boolean;
var
  MyImage, BmpImage: TMemoryStream;
  Bitmap: TCnBitmap;
begin
  Bitmap := TCnBitmap(Obj);
  Result := (Bitmap <> nil) and (ClassType = Bitmap.ClassType);
  if Empty or Bitmap.Empty then
  begin
    Result := Empty and Bitmap.Empty;
    Exit;
  end;
  if Result then
  begin
    MyImage := TMemoryStream.Create;
    try
      WriteData(MyImage);
      BmpImage := TMemoryStream.Create;
      try
        Bitmap.WriteData(BmpImage);
        Result := (MyImage.Size = BmpImage.Size) and
          CompareMem(MyImage.memory, BmpImage.memory, MyImage.Size);
      finally
        BmpImage.Free;
      end;
    finally
      MyImage.Free;
    end;
  end;
end;
{$WARNINGS ON}

//--------------------------------------------------------//
// 象素访问用代码                                         //
// 算法设计：周劲羽                                       //
//--------------------------------------------------------//

// 取象素颜色值
function TCnBitmap.GetPixel(x, y: Integer): TCnColor;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixel, [x, y])
  else
    Result := FScanLine[y, x];
end;

// 写象素
procedure TCnBitmap.SetPixel(x, y: Integer; const Value: TCnColor);
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixel, [x, y])
  else
    FScanLine[y, x] := Value;
end;

// 取扫描线
function TCnBitmap.GetScanLine(Row: Integer): PCnLine;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (Row < 0) or (Row > Height - 1) then
    raise EInvalidScanLine.CreateFmt(SInvalidScanLine, [Row])
  else
    Result := FScanLine[Row];
end;

//--------------------------------------------------------//
// 与外部数据交换                                         //
// 算法设计：周劲羽                                       //
// 算法参考：Graphic32、pnBitmap                          //
//--------------------------------------------------------//

// 设置空位图
procedure TCnBitmap.LoadBlank(AWidth, AHeight: Integer);
begin
  SetSize(AWidth, AHeight);
end;

// 内存中装载位图（RGB数据块）
procedure TCnBitmap.LoadFromMemory(ABits: Pointer; AWidth, AHeight: Integer);
begin
  Changing;
  SetSize(AWidth, AHeight);   // 设置位图尺寸
  if not Empty then
    Move(ABits^, FBits^, FSize); // 复制位图数据
  Changed;
end;

// 从流中装载位图
procedure TCnBitmap.LoadFromStream(Stream: TStream);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromStream(Stream);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// 从文件中装载位图（通过TPicture装载，支持BMP、ICO、JPEG等格式）
procedure TCnBitmap.LoadFromFile(const FileName: string);
var
  Picture: TPicture;
begin
  Changing;
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    Assign(Picture);
  finally
    Picture.Free;
  end;
  Changed;
end;

// 从资源中装载位图（资源ID）
procedure TCnBitmap.LoadFromResourceID(instance: THandle; ResID: Integer);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceID(instance, ResID);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// 从资源中装载位图（资源名）
procedure TCnBitmap.LoadFromResourceName(instance: THandle;
  const ResName: string);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(instance, ResName);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// 从剪帖板中装载位图
procedure TCnBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  Bmp: TBitmap;
begin
  Changing;
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed;
end;

// 保存位图到流
procedure TCnBitmap.SaveToStream(Stream: TStream);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    AssignTo(Bmp);
    Bmp.SaveToStream(Stream);
  finally
    Bmp.Free;
  end;
end;

// 保存位图到文件
procedure TCnBitmap.SaveToFile(const FileName: string);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    AssignTo(Bmp);
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;

// 复制位图到剪帖板中
procedure TCnBitmap.SaveToClipboardFormat(var Format: Word;
  var Data: THandle; var APalette: HPALETTE);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    AssignTo(Bmp);
    Bmp.SaveToClipboardFormat(Format, Data, APalette);
  finally
    Bmp.Free;
  end;
end;

//--------------------------------------------------------//
// 图像绘制过程                                           //
// 原始算法：FastLib                                      //
// 算法修改：周劲羽（增加透明绘制功能、扩展和部分修正） //
//--------------------------------------------------------//

// 计算两位图相交位置
function TCnBitmap.CalcDrawRect(DstX, DstY: Integer; SrcRect,
  SrcClientRect: TRect; var dx, dy, sx, sy, w, h: Integer): Boolean;
begin
  dx := DstX;
  dy := DstY;
  DeRect(SrcRect, sx, sy, w, h);
  if dx < 0 then              // 目标坐标超出范围
  begin
    Dec(sx, dx);
    Inc(w, dx);
    dx := 0;
  end;
  if dy < 0 then
  begin
    Dec(sy, dy);
    Inc(h, dy);
    dy := 0;
  end;
  if sx < 0 then              // 源坐标超出范围
  begin
    Dec(dx, sx);
    Inc(w, sx);
    sx := 0;
  end;
  if sy < 0 then
  begin
    Dec(dy, sy);
    Inc(h, sy);
    sy := 0;
  end;
  Result := (sx < SrcClientRect.Right) and (sy < SrcClientRect.Bottom);
  if Result then
  begin
    if sx + w > RectWidth(SrcClientRect) then
      Dec(w, sx + w - RectWidth(SrcClientRect));
    if sy + h > RectHeight(SrcClientRect) then
      Dec(h, sy + h - RectHeight(SrcClientRect));
    if dx + w > Width then Dec(w, dx + w - Width);
    if dy + h > Height then Dec(h, dy + h - Height);
    Result := (w > 0) and (h > 0);
  end;
end;

// 绘制TCnBitmap位图增强版
procedure TCnBitmap.DoDraw(DstX, DstY: Integer; Src: TCnBitmap;
  SrcRect: TRect; Tran: Boolean);
var
  n1, n2: Pointer;
  I, J: Integer;
  p1, p2: PCnColor;
  x, y, sx, sy, w, h: Integer;
  TranColor: TCnColor;
begin
  if Empty or not Assigned(Src) or Src.Empty or not CalcDrawRect(DstX, DstY,
    SrcRect, Src.ClientRect, x, y, sx, sy, w, h) then Exit;

  Changing;
  if Tran then                // 透明绘制
  begin
    TranColor := Src.GetTranColor;
    for I := 0 to h - 1 do
    begin
      p1 := @FScanLine[y + I][x];
      P2 := @Src.FScanLine[sy + I][sx];
      for J := 0 to w - 1 do
      begin                   // 透明判断
        if (p2.b <> TranColor.b) or (p2.g <> TranColor.g) or (p2.r <> TranColor.r) then
          p1^ := p2^;
        Inc(p1);
        Inc(p2);
      end;
    end;
  end
  else
  begin
    n1 := @FScanLine[y][x];   // 目标位图左上角象素地址
    n2 := @Src.FScanLine[sy][sx]; // 源位图左上角象素地址
    for I := 0 to h - 1 do
    begin
      Move(n2^, n1^, w * 3);  // 复制图像数据
      n1 := Pointer(TCnNativeInt(n1) + RowInc); // 增长一行扫描线
      n2 := Pointer(TCnNativeInt(n2) + Src.RowInc);
    end;
  end;
  Changed;
end;

// 绘制TCnBitmap位图
procedure TCnBitmap.Draw(DstX, DstY: Integer; Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DrawEx(DstX, DstY, Src, Src.ClientRect);
end;

// 绘制TCnBitmap位图增强版
procedure TCnBitmap.DrawEx(DstX, DstY: Integer; Src: TCnBitmap;
  SrcRect: TRect);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DoDraw(DstX, DstY, Src, SrcRect, Src.Transparent);
end;

// 绘制TGraphic位图，TBitmap、TIcon、TJpegImage等
procedure TCnBitmap.Draw(DstX, DstY: Integer; Src: TGraphic);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  DrawEx(DstX, DstY, Src, Rect(0, 0, Src.Width, Src.Height));
end;

// 绘制TGraphic位图增强版
procedure TCnBitmap.DrawEx(DstX, DstY: Integer; Src: TGraphic;
  SrcRect: TRect);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Changing;
  TGraphicAccess(Src).Draw(Canvas, Rect(DstX, DstY, DstX +
    SrcRect.Right - SrcRect.Left, DstY + SrcRect.Bottom - SrcRect.Top));
  if GdiAllocStyle = gsInternal then HandleRelease; //释放句柄
  Changed;
end;

// 从DC上复制位图，必须指定源矩形
procedure TCnBitmap.Draw(DstX, DstY: Integer; hSrc: HDC; SrcRect: TRect);
begin
  if Empty then Exit;
  Changing;
  Bitblt(DC, DstX, DstY, SrcRect.Right - SrcRect.Left, SrcRect.Bottom -
    SrcRect.Top, hSrc, SrcRect.Left, SrcRect.Top, SRCCOPY);
  if GdiAllocStyle = gsInternal then HandleRelease; //释放句柄
  Changed;
end;

// 绘制自身到DC
procedure TCnBitmap.DrawTo(hDst: HDC; DstX, DstY: Integer);
begin
  DrawToEx(hDst, DstX, DstY, ClientRect);
end;

// 绘制自身到DC增强版
procedure TCnBitmap.DrawToEx(hDst: HDC; DstX, DstY: Integer; SrcRect: TRect);
var
  Bmp: TCnBitmap;
  x, y, w, h: Integer;
begin
  if Empty then Exit;
  try
    DeRect(SrcRect, x, y, w, h);
    if Transparent then
    begin
      Bmp := TCnBitmap.Create;
      try
        Bmp.SetSize(SrcRect);
        Bmp.Draw(0, 0, hDst, Bounds(DstX, DstY, w, h));
        Bmp.DrawEx(0, 0, Self, SrcRect);
        Bitblt(hDst, DstX, DstY, w, h, Bmp.DC, 0, 0, SRCCOPY);
      finally
        Bmp.Free;
      end;
    end
    else
      Bitblt(hDst, DstX, DstY, w, h, DC, x, y, SRCCOPY);
  finally
    if GdiAllocStyle = gsInternal then HandleRelease; //释放句柄
  end;
end;

// 取等比缩放矩形
function TCnBitmap.GetResizeRect(Src: TRect): TRect;
var
  cx, cy: Single;
  w, h: Integer;
begin
  cx := Width / RectWidth(Src);
  cy := Height / RectHeight(Src);
  w := Round(Min(cx, cy) * RectWidth(Src));
  h := Round(Min(cx, cy) * RectHeight(Src));
  Result.Left := (Width - w) div 2;
  Result.Right := Result.Left + w;
  Result.Top := (Height - h) div 2;
  Result.Bottom := Result.Top + h;
end;

// 按指定模式绘制
procedure TCnBitmap.DrawMode(Src: TCnBitmap; Mode: TCnDrawMode);
begin
  case Mode of
    dmDraw: Draw(0, 0, Src);
    dmCenter: CenterDraw(Src);
    dmStretched: StretchDraw(Src);
    dmTiled: TileDraw(Src);
    dmResize: StretchDrawEx(GetResizeRect(Src.ClientRect), Src.ClientRect, Src);
  end;
end;

// 按指定模式绘制，支持Alpha混合
procedure TCnBitmap.DrawModeEx(Src: TCnBitmap; Mode: TCnDrawMode;
  Alpha: TCnAlpha);
var
  Bmp: TCnBitmap;
  ARect: TRect;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  case Mode of
    dmDraw: AlphaDraw(0, 0, Src, Src.ClientRect, Alpha);
    dmCenter: AlphaDraw((Width - Src.Width) div 2, (Height - Src.Height) div 2,
        Src, Src.ClientRect, Alpha);
    dmStretched: AlphaDraw(Src, Alpha, True);
    dmTiled:
      begin
        Bmp := TCnBitmap.Create; // 临时位图
        try
          if Src.Transparent then
            Bmp.Assign(Self)  // 透明时复制自身
          else
            Bmp.SetSize(ClientRect);
          Bmp.TileDraw(Src);
          AlphaDraw(0, 0, Bmp, Bmp.ClientRect, Alpha);
        finally
          Bmp.Free;
        end;
      end;
    dmResize:
      begin
        Bmp := TCnBitmap.Create; // 临时位图
        try
          ARect := GetResizeRect(Src.ClientRect);
          Bmp.SetSize(ARect);
          if Src.Transparent then // 透明时复制自身
            Bmp.DoDraw(0, 0, Self, ARect, False);
          Bmp.SmoothFilter := SmoothFilter;
          Bmp.StretchDraw(Src);
          AlphaDraw(ARect.Left, ARect.Top, Bmp, Bmp.ClientRect, Alpha);
        finally
          Bmp.Free;
        end;
      end;
  end;
end;

// 按指定模式绘制
procedure TCnBitmap.DrawMode(Src: TGraphic; Mode: TCnDrawMode);
var
  ARect: TRect;
begin
  case Mode of
    dmDraw: Draw(0, 0, Src);
    dmCenter: CenterDraw(Src);
    dmStretched: StretchDraw(Src);
    dmTiled: TileDraw(Src);
    dmResize:
      begin
        ARect := Rect(0, 0, Src.Width, Src.Height);
        StretchDrawEx(GetResizeRect(ARect), ARect, Src);
      end;
  end;
end;

// 按指定模式绘制，支持Alpha混合
procedure TCnBitmap.DrawModeEx(Src: TGraphic; Mode: TCnDrawMode;
  Alpha: TCnAlpha);
var
  Bmp: TCnBitmap;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Src);
    DrawModeEx(Bmp, Mode, Alpha);
  finally
    Bmp.Free;
  end;
end;

//--------------------------------------------------------//
// 图像中心绘制过程                                       //
// 算法设计：周劲羽                                       //
//--------------------------------------------------------//

// 绘制TCnBitmap位图到中心
procedure TCnBitmap.CenterDraw(Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Draw((Width - Src.Width) div 2, (Height - Src.Height) div 2, Src);
end;

// 绘制TGraphic到中心
procedure TCnBitmap.CenterDraw(Src: TGraphic);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  Draw((Width - Src.Width) div 2, (Height - Src.Height) div 2, Src);
end;

//--------------------------------------------------------//
// 图像平铺绘制过程                                       //
// 算法设计：周劲羽                                       //
//--------------------------------------------------------//

// 平铺绘制TCnBitmap位图
procedure TCnBitmap.TileDraw(Src: TCnBitmap);
begin
  TileDrawEx(ClientRect, Src);
end;

// 平铺绘制TCnBitmap位图增强版
procedure TCnBitmap.TileDrawEx(DstRect: TRect; Src: TCnBitmap);
var
  I, J, x, y, w, h: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    DeRect(DstRect, x, y, w, h);
    for I := 0 to w div Src.Width do
      for J := 0 to h div Src.Height do
        Draw(x + I * Src.Width, y + J * Src.Height, Src);
  finally
    EndUpdate;
  end;
end;

// 平铺绘制TGraphic
procedure TCnBitmap.TileDraw(Src: TGraphic);
begin
  TileDrawEx(ClientRect, Src);
end;

// 平铺绘制TGraphic增强版
procedure TCnBitmap.TileDrawEx(DstRect: TRect; Src: TGraphic);
var
  I, J, x, y, w, h: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    DeRect(DstRect, x, y, w, h);
    for I := 0 to w div Src.Width do
      for J := 0 to h div Src.Height do
        Draw(x + I * Src.Width, y + J * Src.Height, Src);
  finally
    EndUpdate;
  end;
end;

// 平铺绘制自身到DC
procedure TCnBitmap.TileDrawTo(hDst: HDC; DstRect: TRect);
var
  I, J, x, y, w, h: Integer;
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  if Transparent then         // 透明绘制时增加一级缓冲
  begin
    Bmp := TCnBitmap.Create;
    try
      Bmp.SetSize(DstRect);
      Bmp.Draw(0, 0, hDst, DstRect);
      Bmp.TileDraw(Self);
      Bmp.DrawTo(hDst, DstRect.Left, DstRect.Top);
    finally
      Bmp.Free;
    end;
  end
  else
  begin
    DeRect(DstRect, x, y, w, h);
    for I := 0 to w div Width do
      for J := 0 to h div Height do
        DrawTo(hDst, x + I * Width, y + J * Height);
  end;
end;

//--------------------------------------------------------//
// 图像缩放处理过程                                       //
// 原始算法：FastLib                                      //
// 算法修改：周劲羽（增加透明绘制功能、扩展和部分修正） //
//--------------------------------------------------------//

// 最邻近插值算法缩放（粗糙）
procedure TCnBitmap.NormalResize(Dst: TCnBitmap);
var
  xCount, yCount: Integer;
  x, y, xP, yP, xD, yD: Integer;
  yiScale, xiScale: Integer;
  xScale, yScale: Single;
  Read, Line: PCnLine;
  Tmp: TCnColor;
  pc: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
begin
  Tran := Transparent;
  TranColor := GetTranColor;
  xScale := Dst.Width / Width;
  yScale := Dst.Height / Height;
  if (xScale < 1) or (yScale < 1) then // 缩小
  begin
    xiScale := (Width shl 16) div Dst.Width;
    yiScale := (Height shl 16) div Dst.Height;
    yP := 0;
    if Tran then              // 透明
    begin
      for y := 0 to Dst.Height - 1 do
      begin
        xP := 0;
        Read := FScanLine[yP shr 16]; // 源图像最邻近行
        pc := @Dst.FScanLine[y][0];
        for x := 0 to Dst.Width - 1 do
        begin
          with Read[xP shr 16] do // 透明处理
            if (b <> TranColor.b) or (g <> TranColor.g) or (r <> TranColor.r) then
              pc^ := Read[xP shr 16]; // 源图像最邻近象素
          Inc(pc);
          Inc(xP, xiScale);
        end;
        Inc(yP, yiScale);
      end;
    end
    else
    begin                     // 不透明
      for y := 0 to Dst.Height - 1 do
      begin
        xP := 0;
        Read := FScanLine[yP shr 16]; // 源图像最邻近行
        pc := @Dst.FScanLine[y][0];
        for x := 0 to Dst.Width - 1 do
        begin
          pc^ := Read[xP shr 16]; // 源图像最邻近象素
          Inc(pc);
          Inc(xP, xiScale);
        end;
        Inc(yP, yiScale);
      end;
    end;
  end
  else
  begin                       // 放大
    if Tran then              // 透明
    begin
      for y := 0 to Height - 1 do
      begin
        yP := Round(yScale * y); // 目标图像最邻近起始行
        yD := Round(yScale * (y + 1)) - 1; // 目标图像最邻近结束行
        if yD > Dst.Height - 1 then yD := Dst.Height - 1;
        Read := FScanLine[y]; // 源图像当前行
        for x := 0 to Width - 1 do
        begin
          Tmp := Read[x];     // 源图像当前象素
          if (Tmp.b <> TranColor.b) or (Tmp.g <> TranColor.g) or
            (Tmp.r <> TranColor.r) then // 透明处理
          begin
            xP := Round(xScale * x); // 目标图像最邻近起始象素
            xD := Round(xScale * (x + 1)) - 1; // 目标图像最邻近结束象素
            if xD > Dst.Width - 1 then xD := Dst.Width - 1;
            for xCount := xP to xD do
              Dst.FScanLine[yP][xCount] := Tmp; // 复制一行
            for yCount := yP + 1 to yD do
              Move(Dst.FScanLine[yP][xP], Dst.FScanLine[yCount][xP],
                (xD - xP + 1) * 3); //复制到相邻行
          end;
        end;
      end;
    end
    else
    begin                     // 不透明
      yiScale := Round(yScale + 0.5); // 放大倍数（大值）
      xiScale := Round(xScale + 0.5);
      GetMem(Line, Dst.Width * 3); // 临时扫描线
      for y := 0 to Height - 1 do
      begin
        yP := Round(yScale * y); // 目标图像最邻近起始行
        Read := FScanLine[y]; // 源图像当前行
        for x := 0 to Width - 1 do
        begin
          xP := Round(xScale * x); // 目标图像最邻近起始象素
          Tmp := Read[x];     // 源图像当前象素
          for xCount := 0 to xiScale - 1 do
          begin
            xD := xCount + xP;
            if xD >= Dst.Width then Break;
            Line[xD] := Tmp;
          end;
        end;
        for yCount := 0 to yiScale - 1 do // 复制到邻近行
        begin
          yD := yCount + yP;
          if yD >= Dst.Height then Break;
          CopyMemory(Dst.FScanLine[yD], Line, Dst.Width * 3);
        end;
      end;
      FreeMem(Line);
    end;
  end;
end;

// 快速二次插值算法缩放（平滑）
procedure TCnBitmap.SmoothResize(Dst: TCnBitmap);
var
  x, y, xP, yP: Integer;
  yP2, xP2: Integer;
  Read, Read2: PCnLine;
  t, z, z2, iz2: Integer;
  pc: PCnColor;
  w1, w2, w3, w4: Integer;
  Col1, Col2, Col3, Col4: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
begin
  Tran := Transparent;
  TranColor := GetTranColor;
  if (Width = 1) or (Height = 1) then
  begin
    NormalResize(Dst);
    Exit;
  end;
  xP2 := ((Width - 1) shl 15) div Dst.Width; // 缩放比例
  yP2 := ((Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    pc := @Dst.FScanLine[y][0]; // 目标扫描线
    Read := FScanLine[yP shr 15]; // 源上扫描线
    Read2 := FScanLine[yP shr 15 + 1]; // 源下扫描线
    z2 := yP and $7FFF;       // 源计算行与上扫描线之差 "y"
    iz2 := $8000 - z2;        // 源计算行与下扫描线之差 "1-y"
    xP := 0;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      z := xP and $7FFF;      // 源计算象素与左象素之差 "x"
      Col1 := @Read[t];       // 左上象素 "f(0,0)"
      Col2 := @Read[t + 1];   // 右上象素 "f(1,0)"
      Col3 := @Read2[t];      // 左下象素 "f(0,1)"
      Col4 := @Read2[t + 1];  // 右下象素 "f(1,1)"
      if Tran then
        with TranColor do
        begin                 // 透明时取目标象素
          if (Col1.b = b) and (Col1.g = g) and (Col1.r = r) then Col1 := pc;
          if (Col2.b = b) and (Col2.g = g) and (Col2.r = r) then Col2 := pc;
          if (Col3.b = b) and (Col3.g = g) and (Col3.r = r) then Col3 := pc;
          if (Col4.b = b) and (Col4.g = g) and (Col4.r = r) then Col4 := pc;
        end;                  // 至少有一点不透明
      if (Col1 <> pc) or (Col2 <> pc) or (Col3 <> pc) or (Col4 <> pc) then
      begin
        // 计算加权值
        w2 := (z * iz2) shr 15; // 右上 p(1,0) = x(1-y);
        w1 := iz2 - w2;       // 左上 p(0,0) = (1-y)(1-x) = (1-y)-p(0,1)
        w4 := (z * z2) shr 15; // 右下 p(1,1) = x*y
        w3 := z2 - w4;        // 左下 p(0,1) = y(1-x) = y-p(1,1)
        // f(x,y) = [f(1,0) - f(0,0)]x + [f(0,1) - f(0,0)y +
        //          [f(1,1) + (f0,0) - f(0,1) - f(1,0)]xy + f(0,0)
        //        = f(0,0)p(0,0) + f(1,0)p(1,0) + f(0,1)p(0,1) + f(1,1)p(1,1)
        pc.b := (Col1.b * w1 + Col2.b * w2 + Col3.b * w3 + Col4.b * w4) shr 15;
        pc.g := (Col1.g * w1 + Col2.g * w2 + Col3.g * w3 + Col4.g * w4) shr 15;
        pc.r := (Col1.r * w1 + Col2.r * w2 + Col3.r * w3 + Col4.r * w4) shr 15;
      end;
      Inc(pc);
      Inc(xP, xP2);
    end;
    Inc(yP, yP2);
  end;
end;

//--------------------------------------------------------//
// 图像缩放绘制过程                                       //
// 算法设计：周劲羽                                       //
//--------------------------------------------------------//

// 缩放绘制TCnBitmap位图
procedure TCnBitmap.StretchDraw(Src: TCnBitmap);
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    if (Src.Width = Width) and (Src.Height = Height) then
    begin
      Draw(0, 0, Src);        // 尺寸相等
      Exit;
    end;
    if SmoothFilter then      // 平滑缩放
      Src.SmoothResize(Self)  // 二次插值算法
    else
      Src.NormalResize(Self); // 最邻近插值算法
  finally
    EndUpdate;
  end;
end;

// 缩放绘制TCnBitmap位图增强版
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; Src: TCnBitmap);
var
  SrcBmp, DstBmp: TCnBitmap;
  x, y, w, h: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := nil;
    try
      if RectEqu(SrcRect, Src.ClientRect) then
        SrcBmp := Src
      else
      begin
        SrcBmp := TCnBitmap.Create; // 源位图中的源矩形区域
        SrcBmp.SetSize(SrcRect);
        SrcBmp.Transparent := Src.Transparent;
        SrcBmp.TransparentColor := Src.TransparentColor;
        SrcBmp.DoDraw(0, 0, Src, SrcRect, False); // 不透明绘制
      end;
      if RectEqu(DstRect, ClientRect) then
        StretchDraw(SrcBmp)
      else
      begin
        DstBmp := TCnBitmap.Create; // 目标位图的目标矩形区域
        try
          DeRect(DstRect, x, y, w, h);
          DstBmp.SetSize(w, h);
          if SrcBmp.Transparent then // 透明时复制自身
            DstBmp.DoDraw(0, 0, Self, DstRect, False);
          DstBmp.SmoothFilter := SmoothFilter; // 缩放方式
          DstBmp.StretchDraw(SrcBmp);
          Draw(x, y, DstBmp);
        finally
          DstBmp.Free;
        end;
      end;
    finally
      if Assigned(SrcBmp) and (SrcBmp <> Src) then SrcBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// 缩放绘制TGraphic
procedure TCnBitmap.StretchDraw(Src: TGraphic);
begin
  StretchDrawEx(ClientRect, Rect(0, 0, Src.Width, Src.Height), Src);
end;

// 缩放绘制TGraphic增强版
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; Src: TGraphic);
var
  SrcBmp: TCnBitmap;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := TCnBitmap.Create; // 源矩形
    try
      SrcBmp.SetSize(SrcRect);
      SrcBmp.DrawEx(0, 0, Src, SrcRect);
      StretchDrawEx(DstRect, SrcBmp.ClientRect, SrcBmp);
    finally
      SrcBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// 缩放绘制DC
procedure TCnBitmap.StretchDraw(SrcRect: TRect; hSrc: HDC);
var
  SrcBmp: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    SrcBmp := TCnBitmap.Create; // 源矩形
    try
      SrcBmp.SetSize(SrcRect);
      SrcBmp.Draw(0, 0, hSrc, SrcRect);
      StretchDraw(SrcBmp);
    finally
      SrcBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// 缩放绘制DC增强版
procedure TCnBitmap.StretchDrawEx(DstRect, SrcRect: TRect; hSrc: HDC);
var
  DstBmp: TCnBitmap;
  x, y, w, h: Integer;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    DstBmp := TCnBitmap.Create; // 目标矩形
    try
      DeRect(DstRect, x, y, w, h);
      DstBmp.SetSize(w, h);
      DstBmp.SmoothFilter := SmoothFilter;
      DstBmp.StretchDraw(SrcRect, hSrc);
      Draw(x, y, DstBmp);
    finally
      DstBmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// 自身缩放绘制到TImage
procedure TCnBitmap.StretchDrawTo(Dst: TImage);
begin
  if Assigned(Dst) then
  begin
    StretchDrawTo(Dst.Canvas.Handle, Dst.ClientRect);
    Dst.Refresh;
  end;
end;

// 自身缩放绘制到DC
procedure TCnBitmap.StretchDrawTo(hDst: HDC; DstRect: TRect);
var
  DstBmp: TCnBitmap;
begin
  if Empty then Exit;
  DstBmp := TCnBitmap.Create;
  try
    DstBmp.SetSize(DstRect);
    DstBmp.SmoothFilter := SmoothFilter;
    DstBmp.StretchDraw(Self);
    DstBmp.DrawTo(hDst, DstRect.Left, DstRect.Top);
  finally
    DstBmp.Free;
  end;
end;

// 自身缩放绘制到DC增强版
procedure TCnBitmap.StretchDrawToEx(hDst: HDC; DstRect, SrcRect: TRect);
var
  SrcBmp: TCnBitmap;
begin
  if Empty then Exit;
  SrcBmp := TCnBitmap.Create;
  try
    SrcBmp.SetSize(SrcRect);
    SrcBmp.DrawEx(0, 0, Self, SrcRect);
    SrcBmp.SmoothFilter := SmoothFilter;
    SrcBmp.StretchDrawTo(hDst, DstRect);
  finally
    SrcBmp.Free;
  end;
end;

//--------------------------------------------------------//
// Alpha混合绘制                                          //
// 算法来源：FastLib、pnBitmap                            //
// 算法改进：周劲羽（增加透明绘制功能、增强功能及改进） //
//--------------------------------------------------------//

// 检查源图像是否需要缩放
function TCnBitmap.CheckAlphaSrc(Src: TCnBitmap; ARect: TRect;
  Stretch: Boolean): TCnBitmap;
begin
  if (RectWidth(ARect) <> Src.Width) or (RectHeight(ARect) <> Src.Height) then
  begin                       // 需要缩放
    if not Stretch then       // 用于混合的位图大小必须相等
      raise ECnGraphics.Create(SInvalidAlphaBitmap)
    else
    begin                     // 创建临时位图
      Result := TCnBitmap.Create;
      Result.SetSize(ARect);
      Result.SmoothFilter := SmoothFilter;
      if Src.Transparent then // 透明时先绘制自身
        Result.DoDraw(0, 0, Self, ARect, False);
      Result.StretchDraw(Src);
    end;
  end
  else
    Result := Src;
end;

// Alpha混合绘制
procedure TCnBitmap.AlphaDraw(Src: TCnBitmap; Alpha: TCnAlpha;
  Stretch: Boolean);
var
  x, y, I: Integer;
  c1, c2: PCnColor;
  Table: array[-255..255] of Integer;
  Bmp: TCnBitmap;
  TranColor: TCnColor;
  FAlpha: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;
  Bmp := nil;
  BeginUpdate;
  try
    Bmp := CheckAlphaSrc(Src, ClientRect, Stretch); // 处理源位图缩放
    if FAlpha = 255 then
    begin
      Draw(0, 0, Bmp);
      Exit;
    end;

    for I := -255 to 255 do   // 建立Alpha混合表
      Table[I] := (FAlpha * I) shr 8;

    c1 := Bits;
    c2 := Bmp.Bits;
    if (Bmp = Src) and Src.Transparent then  // 源位图透明且未缩放（缩放时已处理过透明了）
    begin
      TranColor := Src.GetTranColor; // 混合时透明处理
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          if (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or (TranColor.r <> c2.r)
            then
          begin
            c1.b := Table[c2.b - c1.b] + c1.b; // 查表法快速混合
            c1.g := Table[c2.g - c1.g] + c1.g;
            c1.r := Table[c2.r - c1.r] + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end
    else
    begin
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          c1.b := Table[c2.b - c1.b] + c1.b; // 查表法快速混合
          c1.g := Table[c2.g - c1.g] + c1.g;
          c1.r := Table[c2.r - c1.r] + c1.r;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end;
  finally
    if Assigned(Bmp) and (Bmp <> Src) then Bmp.Free; // 释放临时位图
    EndUpdate;
  end;
end;

// Alpha混合绘制（允许指定位置）
// 算法设计：周劲羽
procedure TCnBitmap.AlphaDraw(DstX, DstY: Integer; Src: TCnBitmap;
  SrcRect: TRect; Alpha: TCnAlpha);
var
  I, J: Integer;
  p1, p2: PCnColor;
  x, y, sx, sy, w, h: Integer;
  Tran: Boolean;
  TranColor: TCnColor;
  Table: array[-255..255] of Integer;
  FAlpha: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty or not CalcDrawRect(DstX, DstY,
    SrcRect, Src.ClientRect, x, y, sx, sy, w, h) then Exit;

  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;
  if FAlpha = 255 then
  begin
    DrawEx(DstX, DstY, Src, SrcRect);
    Exit;
  end;

  Changing;
  for I := -255 to 255 do     // 建立Alpha混合表
    Table[I] := (FAlpha * I) shr 8;
  Tran := Src.Transparent;
  TranColor := Src.GetTranColor;
  for I := 0 to h - 1 do
  begin
    p1 := @FScanLine[y + I][x];
    P2 := @Src.FScanLine[sy + I][sx];
    for J := 0 to w - 1 do
    begin                     // 透明判断
      if not Tran or (p2.b <> TranColor.b) or (p2.g <> TranColor.g) or (p2.r <>
        TranColor.r) then
      begin
        p1.b := Table[p2.b - p1.b] + p1.b; // 查表法快速混合
        p1.g := Table[p2.g - p1.g] + p1.g;
        p1.r := Table[p2.r - p1.r] + p1.r;
      end;
      Inc(p1);
      Inc(p2);
    end;
  end;
  Changed;
end;

// 渐变透明的Alpha混合操作
// 算法设计：周劲羽
procedure TCnBitmap.AlphaDrawGrad(Src: TCnBitmap; Style: TCnGradStyle;
  Stretch: Boolean; StartAlpha: TCnAlpha; EndAlpha: TCnAlpha);
var
  x, y, I: Integer;
  c1, c2: PCnColor;
  Bmp: TCnBitmap;
  Tran: Boolean;
  TranColor: TCnColor;
  SA, EA, AddA, CurA: Integer;
  BufLen, Len: Integer;
  Alpha: PByteArray;
  ox, oy, Rate: Double;
  ta, tb, tab: Double;
  Weight: Integer;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;

  Bmp := nil;
  Alpha := nil;
  BeginUpdate;
  try
    Bmp := CheckAlphaSrc(Src, ClientRect, Stretch); // 处理源位图缩放
    Tran := (Bmp = Src) and Src.Transparent; // 源位图透明且未缩放
    TranColor := Src.GetTranColor;
    // 计算渐变Alpha表
    if Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      BufLen := FWidth        // 缓冲区长度
    else if Style in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      BufLen := FHeight
    else if Style = gsRadial then
      BufLen := Max(FWidth, FHeight)
    else
      Exit;
    if Style in [gsCenterToLR, gsCenterToTB] then
      Len := (BufLen + 1) div 2 // 渐变带长度
    else
      Len := BufLen;

    GetMem(Alpha, BufLen);
    if Style in [gsLeftToRight, gsTopToBottom, gsRadial] then
    begin
      SA := AlphaToInt(StartAlpha) shl 16; // 正向渐变
      EA := AlphaToInt(EndAlpha) shl 16;
    end else
    begin
      SA := AlphaToInt(EndAlpha) shl 16;
      EA := AlphaToInt(StartAlpha) shl 16;
    end;
    AddA := Round((EA - SA) / Len); // 每象素增量
    CurA := SA;
    for I := 0 to Len - 1 do
    begin
      Alpha[I] := CurA shr 16; // 小数转整数优化
      Inc(CurA, AddA);
    end;

    if Style in [gsCenterToLR, gsCenterToTB] then // 对称渐变
      for I := 0 to Len - 1 do
        Alpha[BufLen - 1 - I] := Alpha[I];

    c1 := Bits;
    c2 := Bmp.Bits;
    if Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
    begin                     // 水平方向渐变
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          if not Tran or (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or
            (TranColor.r <> c2.r) then
          begin
            c1.b := Alpha[x] * (c2.b - c1.b) shr 8 + c1.b;
            c1.g := Alpha[x] * (c2.g - c1.g) shr 8 + c1.g;
            c1.r := Alpha[x] * (c2.r - c1.r) shr 8 + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end else if Style in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
    begin                     // 垂直方向渐变
      for y := 0 to FHeight - 1 do
      begin
        for x := 0 to FWidth - 1 do
        begin
          if not Tran or (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or
            (TranColor.r <> c2.r) then
          begin
            c1.b := Alpha[y] * (c2.b - c1.b) shr 8 + c1.b;
            c1.g := Alpha[y] * (c2.g - c1.g) shr 8 + c1.g;
            c1.r := Alpha[y] * (c2.r - c1.r) shr 8 + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end;
    end
    else if Style = gsRadial then
    begin                     // 辐射渐变
      ta := FWidth / 2;       // 椭圆长轴
      tb := FHeight / 2;      // 椭圆短轴
      tab := ta * tb;
      for y := 0 to FHeight - 1 do
      begin
        oy := Abs(y - tb);    // 垂直中心距
        for x := 0 to FWidth - 1 do
        begin
          ox := Abs(x - ta);  // 水平中心距
          if ox = 0 then
            Rate := oy / tb
          else if oy = 0 then
            Rate := ox / ta
          else                // 计算当前点中心距与该方向半径之比
            Rate := ox * Hypot(tb, ta * oy / ox) / tab;
          if Rate >= 1 then
            Weight := Alpha[BufLen - 1]
          else                // 当前点渐变Alpha值
            Weight := Alpha[Round(Rate * (BufLen - 1))];
          if not Tran or (TranColor.b <> c2.b) or (TranColor.g <> c2.g) or
            (TranColor.r <> c2.r) then
          begin
            c1.b := Weight * (c2.b - c1.b) shr 8 + c1.b;
            c1.g := Weight * (c2.g - c1.g) shr 8 + c1.g;
            c1.r := Weight * (c2.r - c1.r) shr 8 + c1.r;
          end;
          Inc(c1);
          Inc(c2);
        end;
        c1 := Pointer(TCnNativeInt(c1) + Gap);
        c2 := Pointer(TCnNativeInt(c2) + Bmp.Gap);
      end
    end;
  finally
    if Assigned(Bmp) and (Bmp <> Src) then Bmp.Free; // 释放临时位图
    if Alpha <> nil then FreeMem(Alpha);
    EndUpdate;
  end;
end;

// 增强的Alpha混合操作（前景、背景混合到自身）
// 算法设计：周劲羽
procedure TCnBitmap.AlphaDrawEx(DstRect: TRect; Front, Back: TCnBitmap;
  Alpha: TCnAlpha; Stretch: Boolean);
var
  x, y, I: Integer;
  c1, c2, c3: PCnColor;
  cFt, cBk: PCnColor;
  Table: array[-255..255] of Integer;
  BmpFt, BmpBk: TCnBitmap;
  xd, yd, xs, ys, w, h: Integer;
  TranFt, TranBk: Boolean;
  TranColorFt, TranColorBk: TCnColor;
  FAlpha: Integer;
begin
  if Empty or not Assigned(Front) or Front.Empty or not Assigned(Back) or
    Back.Empty then Exit;
  BmpFt := nil;
  BmpBk := nil;
  BeginUpdate;
  try
    DeRect(DstRect, xd, yd, w, h);
    if (xd > Width - 1) or (yd > Height - 1) then Exit;
    xs := 0;
    ys := 0;
    if xd < 0 then            // 目标坐标超出范围
    begin
      Inc(w, xd);
      Dec(xs, xd);
      xd := 0;
    end;
    if yd < 0 then
    begin
      Inc(h, yd);
      Dec(ys, yd);
      yd := 0;
    end;
    if xd + w > Width then Dec(w, xd + w - Width);
    if yd + h > Height then Dec(h, yd + h - Height);

    BmpFt := CheckAlphaSrc(Front, DstRect, Stretch); // 处理源位图缩放
    BmpBk := CheckAlphaSrc(Back, DstRect, Stretch);
    TranFt := (BmpFt = Front) and Front.Transparent;
    TranColorFt := Front.GetTranColor;
    TranBk := (BmpBk = Back) and Back.Transparent;
    TranColorBk := Back.GetTranColor;

    FAlpha := AlphaToInt(Alpha);
    for I := -255 to 255 do   // 建立Alpha混合表
      Table[I] := (FAlpha * I) shr 8;

    if TranFt or TranBk then  // 需要处理透明（速度下降约20%）
    begin
      for y := 0 to h - 1 do
      begin
        c1 := @FScanLine[y + yd][xd];
        c2 := @BmpFt.FScanLine[y + ys][xs];
        c3 := @BmpBk.FScanLine[y + ys][xs];
        for x := 0 to w - 1 do
        begin
          if not TranFt or (TranColorFt.b <> c2.b) or (TranColorFt.g <> c2.g)
            or (TranColorFt.r <> c2.r) then // 前景图透明判断
            cFt := c2
          else
            cFt := c1;
          if not TranBk or (TranColorBk.b <> c2.b) or (TranColorBk.g <> c2.g)
            or (TranColorBk.r <> c2.r) then // 背景图透明判断
            cBk := c3
          else
            cBk := c1;
          c1.b := Table[cFt.b - cBk.b] + cBk.b; // 查表法快速混合
          c1.g := Table[cFt.g - cBk.g] + cBk.g;
          c1.r := Table[cFt.r - cBk.r] + cBk.r;
          Inc(c1);
          Inc(c2);
          Inc(c3);
        end;
      end;
    end
    else                      // 不需要处理透明
    begin
      for y := 0 to h - 1 do
      begin
        c1 := @FScanLine[y + yd][xd];
        c2 := @BmpFt.FScanLine[y + ys][xs];
        c3 := @BmpBk.FScanLine[y + ys][xs];
        for x := 0 to w - 1 do
        begin
          c1.b := Table[c2.b - c3.b] + c3.b; // 查表法快速混合
          c1.g := Table[c2.g - c3.g] + c3.g;
          c1.r := Table[c2.r - c3.r] + c3.r;
          Inc(c1);
          Inc(c2);
          Inc(c3);
        end;
      end;
    end;
  finally
    if Assigned(BmpFt) and (BmpFt <> Front) then BmpFt.Free; // 释放临时位图
    if Assigned(BmpBk) and (BmpBk <> Back) then BmpBk.Free;
    EndUpdate;
  end;
end;

//--------------------------------------------------------//
// 渐变颜色绘制                                           //
// 算法设计：周劲羽                                       //
//--------------------------------------------------------//

// 绘制渐变色
procedure TCnBitmap.DrawGradient(GradColor: TCnGradientColor);
begin
  DrawGradientEx(GradColor, ClientRect, csMaxAlpha);
end;

// 绘制渐变色增强版
procedure TCnBitmap.DrawGradientEx(GradColor: TCnGradientColor; Rect: TRect;
  Alpha: TCnAlpha);
var
  Buf: PCnLine;
  BufLen, Len: Integer;
  IsInvert: Boolean;
  SCol, ECol: TCnColor;
  Col, Col1: PCnColor;
  Bd: TCnColor;
  BdWeight: Integer;
  BufSize: Integer;
  x, y, I, J, Cur, SPos, EPos, Added, Head: Integer;
  ARect: TRect;
  Table: array[-255..255] of Integer;
  FAlpha: Integer;
  OffX, OffY, LineSize: Integer;
  cx, cy, ox, oy, lx, Rate: Double;
  ta, tb, ta2, tab, tab2: Double;
  XL, XR: Integer;
begin
  if Empty or not Assigned(GradColor) then Exit;
  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;
  if not IntersectRect(ARect, ClientRect, Rect) then Exit;

  BeginUpdate;
  try
    if FAlpha < 255 then
      for I := -255 to 255 do // 建立Alpha混合表
        Table[I] := (FAlpha * I) shr 8;

    if GradColor.FStyle in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      BufLen := RectWidth(Rect) // 缓冲区长度
    else if GradColor.FStyle in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      BufLen := RectHeight(Rect)
    else if GradColor.FStyle = gsRadial then
      BufLen := Max(RectWidth(Rect), RectHeight(Rect))
    else
      Exit;
    if GradColor.FStyle in [gsCenterToLR, gsCenterToTB] then
      Len := (BufLen + 1) div 2 // 渐变带长度
    else
      Len := BufLen;
    BufSize := BufLen * 3;
    GetMem(Buf, BufSize);
    try
      // 创建渐变色带缓冲区
      IsInvert := GradColor.FStyle in [gsRightToLeft, gsBottomToTop,
        gsCenterToLR, gsCenterToTB]; // 反序
      I := 0;
      SCol := CnColor(GradColor.FColorStart); // 起始色
      SPos := 0;              // 起始位置
      repeat
        if Assigned(GradColor.FColorMiddle) and (I < GradColor.FColorMiddle.Count) then
        begin                 // 从中间色中查找
          ECol := CnColor(GradColor.FColorMiddle[I].FColor);
          EPos := GradColor.FColorMiddle[I].FPos;
          Inc(I);
        end
        else
        begin
          ECol := CnColor(GradColor.FColorEnd); // 结束色
          EPos := csMaxGradPos;
        end;
        Head := SPos * Len div csMaxGradPos; // 开始位置及增量
        Added := Min((EPos - SPos) * Len div csMaxGradPos + 1, Len - Head);
        for J := 0 to Added - 1 do
        begin
          if IsInvert then    // 检查反序
            Cur := Len - 1 - (J + Head)
          else
            Cur := J + Head;
          Buf[Cur].r := SCol.r + (ECol.r - SCol.r) * J div Added;
          Buf[Cur].g := SCol.g + (ECol.g - SCol.g) * J div Added;
          Buf[Cur].b := SCol.b + (ECol.b - SCol.b) * J div Added;
        end;
        SCol := ECol;
        SPos := EPos;
      until EPos = csMaxGradPos;

      if GradColor.FStyle in [gsCenterToLR, gsCenterToTB] then // 对称渐变
        for I := 0 to Len - 1 do
          Buf[BufLen - 1 - I] := Buf[I];

      OffX := ARect.Left - Rect.Left;
      OffY := ARect.Top - Rect.Top;
      if GradColor.FStyle in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      begin                   // 水平渐变
        if FAlpha = 255 then  // 不透明
        begin
          LineSize := RectWidth(ARect) * 3;
          for I := ARect.Top to ARect.Bottom - 1 do
            Move(Buf[OffX], FScanLine[I][ARect.Left], LineSize);
        end
        else
        begin                 // 透明度
          for y := ARect.Top to ARect.Bottom - 1 do
          begin
            Col := @FScanLine[y][ARect.Left];
            Col1 := @Buf[OffX];
            for x := 0 to RectWidth(ARect) - 1 do
            begin             // 查表法快速混合
              Col.b := Table[Col1.b - Col.b] + Col.b;
              Col.g := Table[Col1.g - Col.g] + Col.g;
              Col.r := Table[Col1.r - Col.r] + Col.r;
              Inc(Col);
              Inc(Col1);
            end;
          end;
        end;
      end
      else if GradColor.FStyle in [gsTopToBottom, gsBottomToTop, gsCenterToTB] then
      begin                   // 垂直渐变
        if FAlpha = 255 then  // 不透明
        begin
          for y := 0 to RectHeight(ARect) - 1 do
          begin
            Col := @FScanLine[y + ARect.Top][ARect.Left];
            for x := 0 to RectWidth(ARect) - 1 do
            begin
              Col^ := Buf[y + OffY];
              Inc(Col);
            end;
          end;
        end
        else
        begin                 // 透明度
          for y := 0 to RectHeight(ARect) - 1 do
          begin               // 查表法快速混合
            Col := @FScanLine[y + ARect.Top][ARect.Left];
            Col1 := @Buf[y + OffY];
            for x := 0 to RectWidth(ARect) - 1 do
            begin
              Col.b := Table[Col1.b - Col.b] + Col.b;
              Col.g := Table[Col1.g - Col.g] + Col.g;
              Col.r := Table[Col1.r - Col.r] + Col.r;
              Inc(Col);
            end;
          end;
        end;
      end
      else if GradColor.FStyle = gsRadial then
      begin                   // 辐射渐变
        ta := RectWidth(Rect) / 2; // 椭圆长轴
        tb := RectHeight(Rect) / 2; // 椭圆短轴
        ta2 := Sqr(ta);
        tab := ta * tb;
        tab2 := Sqr(tab);
        cx := (Rect.Left + Rect.Right - 1) / 2; // 中心点
        cy := (Rect.Top + Rect.Bottom - 1) / 2;
        for y := ARect.Top to ARect.Bottom - 1 do
        begin
          oy := Abs(y - cy);  // 垂直中心距
          if FSmoothFilter then
            lx := Sqrt(tab2 - ta2 * Sqr(oy - 1)) / tb // 考虑抗锯齿处理
          else                // 计算有效绘制宽度
            lx := Sqrt(tab2 - ta2 * Sqr(oy)) / tb;
          XL := Max(Floor(cx - lx), ARect.Left); // 椭圆左边界
          XR := Min(Ceil(cx + lx), ARect.Right - 1); // 右边界
          Col := @FScanLine[y][XL];
          for x := XL to XR do
          begin
            ox := Abs(x - cx); // 水平中心距
            if ox = 0 then
              Rate := oy / tb
            else if oy = 0 then
              Rate := ox / ta
            else              // 计算当前点中心距与该方向半径之比
              Rate := ox * Hypot(tb, ta * oy / ox) / tab;
            if Rate <= 1 then
            begin             // 当前点渐变颜色值
              Col1 := @Buf[Round(Rate * (BufLen - 1))];
              if FAlpha = 255 then // 不透明
                Col^ := Col1^
              else
              begin           // 查表法快速混合
                Col.b := Table[Col1.b - Col.b] + Col.b;
                Col.g := Table[Col1.g - Col.g] + Col.g;
                Col.r := Table[Col1.r - Col.r] + Col.r;
              end;
            end
            else if FSmoothFilter then // 边界点抗锯齿处理
            begin
              BdWeight := Round((1 - (Rate - 1) * (BufLen - 1)) * FAlpha);
              if BdWeight > 0 then // 不透明度
              begin
                Col1 := @Buf[BufLen - 1];
                Col.b := BdWeight * (Col1.b - Col.b) shr 8 + Col.b;
                Col.g := BdWeight * (Col1.g - Col.g) shr 8 + Col.g;
                Col.r := BdWeight * (Col1.r - Col.r) shr 8 + Col.r;
              end;
            end;
            Inc(Col);
          end;
        end
      end;
    finally
      FreeMem(Buf);
    end;
  finally
    EndUpdate;
  end;
end;

//--------------------------------------------------------//
// 按钮位图绘制过程                                       //
// 算法设计：周劲羽                                       //
//           比Rxlib中算法快一倍以上                      //
// 算法参考：Rxlib、pnBitmap                              //
//--------------------------------------------------------//

// 失效位图
procedure TCnBitmap.Disabled;
begin
  DisabledEx(CnWinColor(GetTranColor), clBtnface, clBtnHighlight, clBtnShadow, True);
end;

// 失效位图增强版
procedure TCnBitmap.DisabledEx(OutlineColor, BackColor, HighlightColor,
  ShadowColor: TColor; DrawHighlight: Boolean);
var
  BmpLight: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    MaskEx(OutlineColor, BackColor, ShadowColor); // 对自身二色化
    TransparentColor := BackColor; // 透明色改为背景色
    if DrawHighlight then     // 在右下角绘制高亮图
    begin
      BmpLight := TCnBitmap.Create;
      try
        BmpLight.SetSize(ClientRect);
        BmpLight.Fill(BackColor);
        BmpLight.DoDraw(1, 1, Self, ClientRect, True);
        BmpLight.MaskEx(ShadowColor, HighlightColor, BackColor);
        BmpLight.DoDraw(0, 0, Self, ClientRect, True);
        Move(BmpLight.FBits^, FBits^, FSize);
      finally
        BmpLight.Free;
      end;
    end
  finally
    EndUpdate;
  end;
end;

// 绘制失效位图
procedure TCnBitmap.DrawDisabled(hDst: HDC; ARect: TRect);
begin
  DrawDisabledEx(hDst, ARect, CnWinColor(GetTranColor), clBtnface,
    clBtnHighlight, clBtnShadow, True);
end;

// 绘制失效位图增强版
procedure TCnBitmap.DrawDisabledEx(hDst: HDC; ARect: TRect; OutlineColor, BackColor,
  HighlightColor, ShadowColor: TColor; DrawHighlight: Boolean);
var
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    Bmp.DisabledEx(OutlineColor, BackColor, HighlightColor, ShadowColor,
      DrawHighlight);
    Bmp.DrawTo(hDst, ARect.Left, ARect.Top);
  finally
    Bmp.Free;
  end;
end;

// 阴影位图
procedure TCnBitmap.Shadowed;
begin
  ShadowedEx(CnWinColor(GetTranColor), clBlack, clBtnface, True, 2, 2);
end;

// 阴影位图增强版
procedure TCnBitmap.ShadowedEx(OutlineColor, ShadowColor,
  BackColor: TColor; Blur: Boolean; OffsetX, OffsetY: Integer);
var
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  BeginUpdate;
  try
    Bmp := TCnBitmap.Create;
    try
      Bmp.SetSize(ClientRect); // 临时位图
      Bmp.Fill(BackColor);
      TransparentColor := OutlineColor; // 透明绘制到临时位图
      Bmp.DoDraw(-OffsetX, -OffsetY, Self, ClientRect, True);
      Bmp.Transparent := True;
      Bmp.TransparentColor := BackColor; // 对自身二色化
      MaskEx(OutlineColor, BackColor, ShadowColor);
      TransparentColor := BackColor;
      if Blur then Self.Blur; // 阴影模糊
      DoDraw(0, 0, Bmp, ClientRect, True); // 从临时位图复制
    finally
      Bmp.Free;
    end;
  finally
    EndUpdate;
  end;
end;

// 绘制阴影位图
procedure TCnBitmap.DrawShadowed(hDst: HDC; ARect: TRect);
begin
  DrawShadowedEx(hDst, ARect, CnWinColor(GetTranColor), clBlack, clBtnface,
    True, 2, 2);
end;

// 绘制阴影位图增强版
procedure TCnBitmap.DrawShadowedEx(hDst: HDC; ARect: TRect; OutlineColor,
  ShadowColor, BackColor: TColor; Blur: Boolean; OffsetX, OffsetY: Integer);
var
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    Bmp.ShadowedEx(OutlineColor, ShadowColor, BackColor, Blur, OffsetX, OffsetY);
    Bmp.DrawTo(hDst, ARect.Left, ARect.Top);
  finally
    Bmp.Free;
  end;
end;

//--------------------------------------------------------//
// 图像颜色属性调整                                       //
// 算法来源：FastLib、pnBitmap                            //
// 算法修改：周劲羽                                       //
//--------------------------------------------------------//

// 调整各颜色分量
procedure TCnBitmap.RGB(ra, ga, ba: TAdjustRange);
var
  Table: array[0..255] of TCnColor;
  x, y, I: Integer;
  CurBits: PCnColor;
  r, g, b: Integer;
begin
  if Empty then Exit;
  Changing;
  b := RangeToInt(ba, -255, 255);
  g := RangeToInt(ga, -255, 255);
  r := RangeToInt(ra, -255, 255);
  for I := 0 to 255 do
  begin
    Table[I].b := IntToByte(I + b);
    Table[I].g := IntToByte(I + g);
    Table[I].r := IntToByte(I + r);
  end;
  CurBits := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      CurBits.b := Table[CurBits.b].b;
      CurBits.g := Table[CurBits.g].g;
      CurBits.r := Table[CurBits.r].r;
      Inc(CurBits);
    end;
    CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
  end;
  Changed;
end;

// 调整图像亮度
procedure TCnBitmap.Brightness(Range: TAdjustRange; Channels: TColorChannels);
var
  x, y: Integer;
  Table: array[0..255] of Byte;
  CurBits: PCnColor;
  Amount: Integer;
begin
  if Empty or (Channels = []) then Exit;
  Changing;
  Amount := RangeToInt(Range, -256, 256);
  if Amount > 0 then          // 变亮
    for x := 0 to 255 do
      Table[x] := IntToByte(x + ((Amount * (x xor 255)) shr 8))
  else
  begin
    Amount := -Amount;        // 变暗
    for x := 0 to 255 do
      Table[x] := IntToByte(x - ((Amount * x) shr 8));
  end;

  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        CurBits.b := Table[CurBits.b];
        CurBits.g := Table[CurBits.g];
        CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        if ccBlue in Channels then CurBits.b := Table[CurBits.b];
        if ccGreen in Channels then CurBits.g := Table[CurBits.g];
        if ccRed in Channels then CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// 调整图像饱和度
procedure TCnBitmap.Saturation(Range: TAdjustRange; Channels: TColorChannels);
var
  Grays: array[0..255] of Byte;
  Alpha: array[0..255] of WORD;
  Gray: Byte;
  x, y, ag: Integer;
  CurBits: TCnColor;
  pc: PCnColor;
  Amount: Integer;
begin
  if Empty or (Channels = []) then Exit;
  Changing;
  x := 0;
  y := 0;
  for ag := 0 to 85 do
  begin
    Grays[x + 0] := y;        // Grays[i] := i div 3
    Grays[x + 1] := y;
    Grays[x + 2] := y;
    Inc(y);
    Inc(x, 3);
  end;

  Amount := RangeToInt(Range, 0, 510);
  for x := 0 to 255 do
    Alpha[x] := (x * Amount) shr 8;
  pc := Bits;
  if Channels = csAllChannels then
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // 查表法
        CurBits := pc^;       // Gray := (r + g + b) div 3
        Gray := Grays[CurBits.r] + Grays[CurBits.g] + Grays[CurBits.b];
        ag := Gray - Alpha[Gray]; // r := r * Alpha + Gray * (1 - Alpha)
        pc.b := IntToByte(Alpha[CurBits.b] + ag);
        pc.g := IntToByte(Alpha[CurBits.g] + ag);
        pc.r := IntToByte(Alpha[CurBits.r] + ag);
        Inc(pc);
      end;
      pc := Pointer(TCnNativeInt(pc) + Gap);
    end;
  end
  else
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // 查表法
        CurBits := pc^;       // Gray := (r + g + b) div 3
        Gray := Grays[CurBits.r] + Grays[CurBits.g] + Grays[CurBits.b];
        ag := Gray - Alpha[Gray]; // r := r * Alpha + Gray * (1 - Alpha)
        if ccBlue in Channels then pc.b := IntToByte(Alpha[CurBits.b] + ag);
        if ccGreen in Channels then pc.g := IntToByte(Alpha[CurBits.g] + ag);
        if ccRed in Channels then pc.r := IntToByte(Alpha[CurBits.r] + ag);
        Inc(pc);
      end;
      pc := Pointer(TCnNativeInt(pc) + Gap);
    end;
  end;
  Changed;
end;

// 调整图像对比度
procedure TCnBitmap.Contrast(Range: TAdjustRange; Channels: TColorChannels);
var
  x, y: Integer;
  Table: array[0..255] of Byte;
  CurBits: PCnColor;
  Amount: Integer;
begin
  if Empty or (Channels = []) then Exit;
  Changing;
  Amount := RangeToInt(Range, -256, 256);
  for x := 0 to 255 do
    Table[x] := IntToByte(x + (x - 128) * Amount div 256);
  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        CurBits.b := Table[CurBits.b];
        CurBits.g := Table[CurBits.g];
        CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        if ccBlue in Channels then CurBits.b := Table[CurBits.b];
        if ccGreen in Channels then CurBits.g := Table[CurBits.g];
        if ccRed in Channels then CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// 调整当前图像的色阶
// 算法设计：周劲羽
procedure TCnBitmap.Levels(InLow, InHigh, OutLow, OutHigh: Byte;
  Channels: TColorChannels);
var
  x, y: Integer;
  Table: array[0..255] of Byte;
  CurBits: PCnColor;
begin
  if Empty or (InHigh <= InLow) then
    Exit;

  Changing;
  for x := 0 to InLow - 1 do
    Table[x] := OutLow;
  for x := InHigh + 1 to 255 do
    Table[x] := OutHigh;
  for x := InLow to InHigh do
    Table[x] := (x - InLow) * (OutHigh - OutLow) div (InHigh - InLow) + OutLow;

  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        CurBits.b := Table[CurBits.b];
        CurBits.g := Table[CurBits.g];
        CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 1 to FHeight do
    begin
      for x := 1 to FWidth do
      begin
        if ccBlue in Channels then CurBits.b := Table[CurBits.b];
        if ccGreen in Channels then CurBits.g := Table[CurBits.g];
        if ccRed in Channels then CurBits.r := Table[CurBits.r];
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// 图像灰度化
procedure TCnBitmap.Grayscale(Channels: TColorChannels);
var
  Grays: array[0..256] of Byte;
  I, x, y: Integer;
  CurBits: PCnColor;
begin
  if Empty then Exit;
  Changing;
  x := 0;
  y := 0;
  for I := 0 to 85 do
  begin
    Grays[x + 0] := y;        // Grays[i] := i div 3
    Grays[x + 1] := y;
    Grays[x + 2] := y;
    Inc(y);
    Inc(x, 3);
  end;
  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // Gray := (r + g + b) div 3
        I := Grays[CurBits.b] + Grays[CurBits.g] + Grays[CurBits.r];
        CurBits.b := I;
        CurBits.g := I;
        CurBits.r := I;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin                   // Gray := (r + g + b) div 3
        I := Grays[CurBits.b] + Grays[CurBits.g] + Grays[CurBits.r];
        if ccBlue in Channels then CurBits.b := I;
        if ccGreen in Channels then CurBits.g := I;
        if ccRed in Channels then CurBits.r := I;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// 颜色反转
procedure TCnBitmap.Invert(Channels: TColorChannels);
var
  x, y: Integer;
  CurBits: PCnColor;
begin
  if Empty then Exit;
  Changing;
  CurBits := Bits;
  if Channels = csAllChannels then
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        CurBits.b := not CurBits.b; // 颜色取反
        CurBits.g := not CurBits.g; // 一条汇编指令完成
        CurBits.r := not CurBits.r;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end
  else
  begin
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        if ccBlue in Channels then CurBits.b := not CurBits.b; // 颜色取反
        if ccGreen in Channels then CurBits.g := not CurBits.g; // 一条汇编指令完成
        if ccRed in Channels then CurBits.r := not CurBits.r;
        Inc(CurBits);
      end;
      CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
    end;
  end;
  Changed;
end;

// 图像按指定颜色彩色化（支持透明方式）
procedure TCnBitmap.Colorize(Color: TColor);
begin
  Colorize(CnColor(Color));
end;

procedure TCnBitmap.Colorize(Color: TCnColor);
var
  x, y: Integer;
  CurBits: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
  ra, ga, ba: Byte;
begin
  if Empty then Exit;
  Changing;
  ra := Color.r;
  ga := Color.g;
  ba := Color.b;
  CurBits := Bits;
  Tran := FTransparent;
  TranColor := GetTranColor;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      with TranColor do
        if not Tran or (r <> CurBits.r) or (g <> CurBits.g) or (b <> CurBits.b) then
        begin
          CurBits.b := IntToByte((CurBits.b - 192) + ba);
          CurBits.g := IntToByte((CurBits.g - 192) + ga);
          CurBits.r := IntToByte((CurBits.r - 192) + ra);
        end;
      Inc(CurBits);
    end;
    CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
  end;
  Changed;
end;

//--------------------------------------------------------//
// 图像几何变换                                           //
// 算法来源：FastLib、pnBitmap、周劲羽                    //
// 算法修改：周劲羽                                       //
//--------------------------------------------------------//

// 图像垂直（水平）翻转
procedure TCnBitmap.Flip(Horizontal: Boolean);
var
  w, h, x, y: Integer;
  CurBits: TCnColor;
  TmPLine, TmPLine2, Line: PCnLine;
  TopY: Integer;
begin
  if Empty then Exit;
  Changing;
  TmPLine := nil;
  w := FWidth - 1;
  h := FHeight - 1;

  TopY := FHeight - 1;
  if not Horizontal then
  begin
    TopY := h div 2;
    GetMem(TmPLine, RowInc);
  end;

  try
    Line := Bits;
    for y := 0 to TopY do
    begin
      if Horizontal then      // 水平翻转
        for x := 0 to w div 2 do
        begin
          CurBits := Line[x];
          Line[x] := Line[w - x];
          Line[w - x] := CurBits;
        end
      else
      begin                   // 垂直翻转
        TmPLine2 := Pointer(TCnNativeInt(Bits) + (h - y) * RowInc);
        CopyMemory(TmPLine, Line, RowInc);
        CopyMemory(Line, TmPLine2, RowInc);
        CopyMemory(TmPLine2, TmPLine, RowInc);
      end;
      Line := Pointer(TCnNativeInt(Line) + RowInc);
    end;
  finally
    if not Horizontal then FreeMem(TmPLine);
  end;
  Changed;
end;

// 图像旋转
procedure TCnBitmap.Turn(Angle: TTurnAngle);
var
  Bmp: TCnBitmap;
  x, y, tx, ty: Integer;
  Dst: PCnLine;
begin
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    case Angle of
      ta90:
        begin
          SetSize(Height, Width);
          tx := Width - 1;
          for y := 0 to Height - 1 do
          begin
            Dst := FScanLine[y];
            for x := 0 to Width - 1 do
              Dst[x] := Bmp.FScanLine[tx - x][y];
          end;
        end;
      ta180:
        begin
          tx := Width - 1;
          ty := Height - 1;
          for y := 0 to Height - 1 do
          begin
            Dst := FScanLine[y];
            for x := 0 to Width - 1 do
              Dst[x] := Bmp.FScanLine[ty - y][tx - x];
          end;
        end;
      ta270:
        begin
          SetSize(Height, Width);
          ty := Height - 1;
          for y := 0 to Height - 1 do
          begin
            Dst := FScanLine[y];
            for x := 0 to Width - 1 do
              Dst[x] := Bmp.FScanLine[x][ty - y];
          end;
        end;
    end;
  finally
    Bmp.Free;
  end;
end;

// 水平移动图像
procedure TCnBitmap.VShift(Amount: Integer);
var
  p, Line: Pointer;
  y: Integer;
begin
  if Empty then Exit;
  if Amount < 0 then Amount := Width - (Abs(Amount) mod Width);
  if Amount >= Width then Amount := Amount mod Width;
  if Amount = 0 then Exit;
  Changing;
  GetMem(Line, Amount * 3);   // 临时缓冲区
  try
    p := Bits;
    for y := 0 to Height - 1 do
    begin                     // 复制左半部分到临时缓冲区
      CopyMemory(Line, Pointer(TCnNativeInt(p) + ((Width - Amount) * 3)), Amount * 3);
      MoveMemory(Pointer(TCnNativeInt(p) + (Amount * 3)), p, (Width - Amount) * 3);
      CopyMemory(p, Line, Amount * 3);
      p := Pointer(TCnNativeInt(p) + RowInc);
    end;
  finally
    FreeMem(Line);
  end;
  Changed;
end;

// 垂直移动图像
procedure TCnBitmap.HShift(Amount: Integer);
var
  Buff: Pointer;
  p, y: TCnNativeInt;
begin
  if Empty then
    Exit;

  if Amount < 0 then Amount := Height mod Abs(Amount);
  if Amount >= Height then Amount := Amount mod Height;
  if Amount = 0 then
    Exit;

  Changing;
  p := TCnNativeInt(Bits) + (Height * (Gap)) + ((Height * Width) * 3);
  p := p - TCnNativeInt(FScanLine[Amount]);
  y := TCnNativeInt(FScanLine[Amount]) - TCnNativeInt(Bits);
  GetMem(Buff, y);            // 临时缓冲区

  try
    CopyMemory(Buff, FScanLine[Height - Amount], y);
    MoveMemory(FScanLine[Amount], Bits, p);
    CopyMemory(Bits, Buff, y);
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// 位图旋转
// 算法设计：周劲羽 2002.01.27
// 支持最邻近插值算法和快速二次插值算法（SmoothFilter属性）
// 支持完整的透明方式
// Angle: 角度-360..360;
procedure TCnBitmap.Rotate(DstCenter: TPoint; Src: TCnBitmap; Angle: Double);
var
  FAngle: Double;
  ARect: TRect;
  iCos, iSin: Integer;
  SrcX, SrcY: Integer;
  px, py: Integer;
  x, y, x1, x2, y1, y2, xs, ys: Integer;
  zx, zy, izy: Integer;
  w1, w2, w3, w4: Integer;
  Col1, Col2, Col3, Col4, Dst: PCnColor;
  Tran: Boolean;
  TranColor: TCnColor;
begin
  if Empty or not Assigned(Src) or Src.Empty then Exit;
  if not GetRotateRect(ClientRect, DstCenter, Src.Width, Src.Height, Angle,
    ARect) then Exit;

  Changing;
  Tran := Src.Transparent;
  TranColor := Src.GetTranColor;
  FAngle := -Angle * Pi / 180; // 反方向计算原始点
  iSin := Round(Sin(FAngle) * $8000);
  iCos := Round(Cos(FAngle) * $8000);
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    py := y - DstCenter.y;
    for x := ARect.Left to ARect.Right - 1 do
    begin
      px := x - DstCenter.x;
      SrcX := px * iCos - py * iSin + Src.Width shl 14; // 原始点
      SrcY := px * iSin + py * iCos + Src.Height shl 14;
      if not SmoothFilter then
      begin                   // 最邻近插值算法
        xs := SrcX + $3FFF;
        ys := SrcY + $3FFF;
        if (xs >= 0) and (ys >= 0) then
        begin
          xs := xs shr 15;
          ys := ys shr 15;
          if (xs < Src.Width) and (ys < Src.Height) then // 范围检测
          begin
            Col1 := @Src.FScanLine[ys][xs];
            if not Tran or (TranColor.b <> Col1.b) or (TranColor.g <> Col1.g) or
              (TranColor.r <> Col1.r) then // 透明检查
              FScanLine[y][x] := Col1^;
          end
        end
      end
      else
      begin                   // 二次插值算法
        if SrcX > 0 then
          x1 := SrcX shr 15   // 左邻近象素
        else
          x1 := -(-SrcX shr 15);
        x2 := x1 + 1;         // 右邻近象素
        if SrcY > 0 then
          y1 := SrcY shr 15
        else
          y1 := -(-SrcY shr 15); // 上邻近扫描线
        y2 := y1 + 1;         // 下邻近扫描线
        if (x2 >= 0) and (x1 < Src.Width) and (y2 >= 0) and
          (y1 < Src.Height) then // 范围检测
        begin
          Dst := @FScanLine[y][x];
          if (x1 >= 0) and (y1 >= 0) then
          begin
            Col1 := @Src.FScanLine[y1][x1]; // 左上角象素
            if Tran and (TranColor.b = Col1.b) and (TranColor.g = Col1.g) and
              (TranColor.r = Col1.r) then // 透明检查
              Col1 := Dst;
          end
          else
            Col1 := Dst;
          if (x2 < Src.Width) and (y1 >= 0) then // 右上角象素
          begin
            Col2 := @Src.FScanLine[y1][x2]; // 透明检查
            if Tran and (TranColor.b = Col2.b) and (TranColor.g = Col2.g) and
              (TranColor.r = Col2.r) then
              Col2 := Dst;
          end
          else
            Col2 := Dst;
          if (x1 >= 0) and (y2 < Src.Height) then // 左下角象素
          begin
            Col3 := @Src.FScanLine[y2][x1]; // 透明检查
            if Tran and (TranColor.b = Col3.b) and (TranColor.g = Col3.g) and
              (TranColor.r = Col3.r) then
              Col3 := Dst;
          end
          else
            Col3 := Dst;
          if (x2 < Src.Width) and (y2 < Src.Height) then // 右下角象素
          begin
            Col4 := @Src.FScanLine[y2][x2];
            if Tran and (TranColor.b = Col4.b) and (TranColor.g = Col4.g) and
              (TranColor.r = Col4.r) then // 透明检查
              Col4 := Dst;
          end
          else
            Col4 := Dst;
          if (Col1 <> Dst) or (Col2 <> Dst) or (Col3 <> Dst) or (Col4 <> Dst) then
          begin               // 至少有一点不透明
            zx := SrcX and $7FFF;
            zy := SrcY and $7FFF;
            izy := zy xor $7FFF;
            w2 := (zx * izy) shr 15; // 计算加权值
            w1 := izy - w2;
            w4 := (zx * zy) shr 15;
            w3 := zy - w4;
            Dst.b := (Col1.b * w1 + Col2.b * w2 + Col3.b * w3 + Col4.b * w4) shr 15;
            Dst.g := (Col1.g * w1 + Col2.g * w2 + Col3.g * w3 + Col4.g * w4) shr 15;
            Dst.r := (Col1.r * w1 + Col2.r * w2 + Col3.r * w3 + Col4.r * w4) shr 15;
          end;
        end;
      end;
    end;
  end;
  Changed;
end;

//--------------------------------------------------------//
// 图像滤镜处理                                           //
// 算法来源：FastLib、pnBitmap                            //
// 算法修改：周劲羽                                       //
//--------------------------------------------------------//

// 3X3卷积运算
// 算法修改：周劲羽（使用数据缓冲区处理、速度优化约一倍）
procedure TCnBitmap.ApplyFilter(Core: TFilterCore; Cent: Integer);
var
  x, y, Sum: Integer;
  Buff: Pointer;
  p1, p2, p3, Dst: PCnLine;
  y1, y2, y3: PCnLine;
  x1, x2, x3: Integer;
begin
  if Empty then Exit;
  Changing;
  GetMem(Buff, Size);
  try
    CopyMemory(Buff, Bits, Size);
    if Cent <> 0 then
      Sum := Cent
    else
    begin
      Sum := Core[0, 0] + Core[1, 0] + Core[2, 0] +
        Core[0, 1] + Core[1, 1] + Core[2, 1] +
        Core[0, 2] + Core[1, 2] + Core[2, 2];
      if Sum = 0 then Sum := 1;
    end;

    p1 := Pointer(TCnNativeInt(Buff) - RowInc);
    p2 := Buff;
    p3 := Pointer(TCnNativeInt(Buff) + RowInc);
    Dst := Bits;              // 目标象素
    for y := 0 to Height - 1 do
    begin
      if y > 0 then
        y1 := p1
      else
        y1 := p2;
      y2 := p2;
      if y < Height - 1 then
        y3 := p3
      else
        y3 := p2;
      for x := 0 to Width - 1 do
      begin
        if x > 0 then
          x1 := x - 1
        else
          x1 := 0;
        x2 := x;
        if x < Width - 1 then
          x3 := x + 1
        else
          x3 := x;
        Dst[x].b := IntToByte((y1[x1].b * Core[0, 0] + y1[x2].b * Core[0, 1] +
          y1[x3].b * Core[0, 2] + y2[x1].b * Core[1, 0] + y2[x2].b * Core[1, 1] +
          y2[x3].b * Core[1, 2] + y3[x1].b * Core[2, 0] + y3[x2].b * Core[2, 1] +
          y3[x3].b * Core[2, 2]) div Sum);
        Dst[x].g := IntToByte((y1[x1].g * Core[0, 0] + y1[x2].g * Core[0, 1] +
          y1[x3].g * Core[0, 2] + y2[x1].g * Core[1, 0] + y2[x2].g * Core[1, 1] +
          y2[x3].g * Core[1, 2] + y3[x1].g * Core[2, 0] + y3[x2].g * Core[2, 1] +
          y3[x3].g * Core[2, 2]) div Sum);
        Dst[x].r := IntToByte((y1[x1].r * Core[0, 0] + y1[x2].r * Core[0, 1] +
          y1[x3].r * Core[0, 2] + y2[x1].r * Core[1, 0] + y2[x2].r * Core[1, 1] +
          y2[x3].r * Core[1, 2] + y3[x1].r * Core[2, 0] + y3[x2].r * Core[2, 1] +
          y3[x3].r * Core[2, 2]) div Sum);
      end;
      p1 := Pointer(TCnNativeInt(p1) + RowInc);
      p2 := Pointer(TCnNativeInt(p2) + RowInc);
      p3 := Pointer(TCnNativeInt(p3) + RowInc);
      Dst := Pointer(TCnNativeInt(Dst) + RowInc);
    end;
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// 模糊处理（低通滤波）
procedure TCnBitmap.Blur;
var
  p1, p2, p3, Dst: PCnLine;
  x1, x2, x3: Integer;
  x, y: Integer;
  Bmp: TCnBitmap;
begin
  if Empty then Exit;
  Changing;
  Bmp := TCnBitmap.Create;
  try
    Bmp.SetSize(ClientRect);
    Move(FBits^, Bmp.FBits^, FSize);
    for y := 0 to FHeight - 1 do
    begin
      p1 := Bmp.FScanLine[TrimInt(y - 1, 0, FHeight - 1)];
      p2 := Bmp.FScanLine[y];
      p3 := Bmp.FScanLine[TrimInt(y + 1, 0, FHeight - 1)];
      Dst := FScanLine[y];
      for x := 0 to FWidth - 1 do
      begin
        x1 := TrimInt(x - 1, 0, FWidth - 1);
        x2 := x;
        x3 := TrimInt(x + 1, 0, FWidth - 1);
        Dst[x].b := (p1[x1].b shr 1 + p1[x2].b + p1[x3].b shr 1 + p2[x1].b +
          p2[x2].b shl 1 + p2[x3].b + p3[x1].b shr 1 + p3[x2].b +
          p3[x3].b shr 1) shr 3;
        Dst[x].g := (p1[x1].g shr 1 + p1[x2].g + p1[x3].g shr 1 + p2[x1].g +
          p2[x2].g shl 1 + p2[x3].g + p3[x1].g shr 1 + p3[x2].g +
          p3[x3].g shr 1) shr 3;
        Dst[x].r := (p1[x1].r shr 1 + p1[x2].r + p1[x3].r shr 1 + p2[x1].r +
          p2[x2].r shl 1 + p2[x3].r + p3[x1].r shr 1 + p3[x2].r +
          p3[x3].r shr 1) shr 3;
      end;
    end;
  finally
    Bmp.Free;
  end;
  Changed;
end;

// 分裂模糊
procedure TCnBitmap.SplitBlur(Amount: Integer);
var
  Lin1, Lin2: PCnLine;
  pc: PCnColor;
  cx, x, y: Integer;
  Buf: array[0..3] of TCnColor;
begin
  if Empty then Exit;
  Changing;
  pc := Bits;
  for y := 0 to FHeight - 1 do
  begin
    Lin1 := FScanLine[TrimInt(y + Amount, 0, FHeight - 1)];
    Lin2 := FScanLine[TrimInt(y - Amount, 0, FHeight - 1)];
    for x := 0 to FWidth - 1 do
    begin
      cx := TrimInt(x + Amount, 0, FWidth - 1);
      Buf[0] := Lin1[cx];
      Buf[1] := Lin2[cx];
      cx := TrimInt(x - Amount, 0, Width - 1);
      Buf[2] := Lin1[cx];
      Buf[3] := Lin2[cx];
      pc.b := (Buf[0].b + Buf[1].b + Buf[2].b + Buf[3].b) shr 2;
      pc.g := (Buf[0].g + Buf[1].g + Buf[2].g + Buf[3].g) shr 2;
      pc.r := (Buf[0].r + Buf[1].r + Buf[2].r + Buf[3].r) shr 2;
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// 快速高斯模糊
procedure TCnBitmap.GaussianBlur(Amount: Integer);
var
  I: Integer;
begin
  if Empty or (Amount <= 0) then Exit;
  BeginUpdate;
  try
    for I := Amount downto 1 do
      SplitBlur(I);
  finally
    EndUpdate;
  end;
end;

// 锐化处理（高通滤波）
procedure TCnBitmap.Sharpen;
begin
  SplitSharpen(1);
end;

// 分裂锐化
procedure TCnBitmap.SplitSharpen(Amount: Integer);
var
  Lin0, Lin1, Lin2: PCnLine;
  pc: PCnColor;
  cx, x, y: Integer;
  Buf: array[0..8] of TCnColor;
begin
  if Empty then Exit;
  Changing;
  pc := Bits;
  for y := 0 to FHeight - 1 do
  begin
    Lin0 := FScanLine[TrimInt(y - Amount, 0, Height - 1)];
    Lin1 := FScanLine[y];
    Lin2 := FScanLine[TrimInt(y + Amount, 0, FHeight - 1)];
    for x := 0 to FWidth - 1 do
    begin
      cx := TrimInt(x - Amount, 0, FWidth - 1);
      Buf[0] := Lin0[cx];
      Buf[1] := Lin1[cx];
      Buf[2] := Lin2[cx];
      Buf[3] := Lin0[x];
      Buf[4] := Lin1[x];
      Buf[5] := Lin2[x];
      cx := TrimInt(x + Amount, 0, FWidth - 1);
      Buf[6] := Lin0[cx];     // 卷积核：-1/8 -1/8 -1/8
      Buf[7] := Lin1[cx];     //         -1/8   2  -1/8
      Buf[8] := Lin2[cx];     //         -1/8 -1/8 -1/8
      pc.b := IntToByte((16 * Buf[4].b - (Buf[0].b + Buf[1].b + Buf[2].b +
        Buf[3].b + Buf[5].b + Buf[6].b + Buf[7].b + Buf[8].b)) div 8);
      pc.g := IntToByte((16 * Buf[4].g - (Buf[0].g + Buf[1].g + Buf[2].g +
        Buf[3].g + Buf[5].g + Buf[6].g + Buf[7].g + Buf[8].g)) div 8);
      pc.r := IntToByte((16 * Buf[4].r - (Buf[0].r + Buf[1].r + Buf[2].r +
        Buf[3].r + Buf[5].r + Buf[6].r + Buf[7].r + Buf[8].r)) div 8);
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// 增强锐化
procedure TCnBitmap.SharpenMore(Amount: Integer);
var
  I: Integer;
begin
  if Empty or (Amount <= 0) then Exit;
  BeginUpdate;
  try
    for I := Amount downto 1 do
      SplitSharpen(I);
  finally
    EndUpdate;
  end;
end;

// 喷溅效果
procedure TCnBitmap.Spray(Amount: Integer);
var
  r, x, y: Integer;
begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  for y := 0 to FHeight - 1 do
    for x := 0 to FWidth - 1 do
    begin
      r := Random(Amount);
      FScanLine[y][x] := FScanLine
        [TrimInt(y + (r - Random(r * 2)), 0, FHeight - 1)]
        [TrimInt(x + (r - Random(r * 2)), 0, FWidth - 1)];
    end;
  Changed;
end;

// 浮雕效果
procedure TCnBitmap.Emboss;
var
  x, y: Integer;
  p1, p2: PCnColor;
  Line: PPCnLines;
begin
  if Empty then Exit;
  Changing;
  p1 := Bits;                 // 第一行
  p2 := Pointer(TCnNativeInt(p1) + RowInc + 3); // 右下各加一象素
  GetMem(Line, RowInc);       // 临时行保存最后一行扫描线内容
  try
    CopyMemory(Line, FScanLine[FHeight - 1], RowInc);
    for y := 0 to Height - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        p1.b := (p1.b + not p2.b) shr 1; // 当前象素与右下角象素取反的平均值
        p1.g := (p1.g + not p2.g) shr 1;
        p1.r := (p1.r + not p2.r) shr 1;
        Inc(p1);
        if (y < FHeight - 2) and (x < FWidth - 2) then Inc(p2);
      end;
      p1 := Pointer(TCnNativeInt(p1) + FGap);
      if y < FHeight - 2 then // 最后两行
        p2 := Pointer(TCnNativeInt(p2) + Gap + 6)
      else
        p2 := Pointer(TCnNativeInt(Line) + 3);
    end;
  finally
    FreeMem(Line);
  end;
  Changed;
end;

// 壁画效果
procedure TCnBitmap.Posterize(Amount: Integer);
var
  x, y: Integer;
  CurBits: PCnColor;
  Table: array[0..255] of Byte;
begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  for x := 0 to 255 do
    Table[x] := IntToByte(Round(x / Amount) * Amount);
  CurBits := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin
      CurBits.b := Table[CurBits.b];
      CurBits.g := Table[CurBits.g];
      CurBits.r := Table[CurBits.r];
      Inc(CurBits);
    end;
    CurBits := Pointer(TCnNativeInt(CurBits) + Gap);
  end;
  Changed;
end;

// 地图效果
procedure TCnBitmap.HeightMap(Amount: Integer);
var
  x, y, c: Integer;
  Src: PCnColor;
  Bmp: TCnBitmap;
  Table: array[0..765] of Byte;
begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    for x := Low(Table) to High(Table) do
      Table[x] := x * Amount shr 8 div 3;
    Src := Bmp.Bits;
    for y := 0 to Height - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        c := y - Table[Src.b + Src.g + Src.r];
        if c >= 0 then FScanLine[c][x] := Src^;
        Inc(Src);
      end;
      Src := Pointer(TCnNativeInt(Src) + Gap);
    end;
  finally
    Bmp.Free;
    Changed;
  end;
end;

// 水滴效果
procedure TCnBitmap.Marble(Scale: Double; Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Double;
  Src: PCnColor;
  Bmp: TCnBitmap;
  Buf: array of Double;
begin
  if Empty or (Scale <= 0) or (Turbulence <= 0) then Exit;
  Changing;
  Bmp := TCnBitmap.Create;
  try
    Bmp.Assign(Self);
    SetLength(Buf, Turbulence);
    for x := 0 to Turbulence - 1 do
    begin
      Buf[x] := -Scale * Sin(x / Scale);
    end;

    Src := Bmp.Bits;
    for y := 0 to Height - 1 do
    begin
      yy := Scale * Cos((y mod Turbulence) / Scale);
      for x := 0 to Width - 1 do
      begin
        xx := Buf[x mod Turbulence];
        xm := Round(Abs(x + xx + yy));
        ym := Round(Abs(y + yy + xx));
        if (ym > 0) and (ym < Height) and (xm > 0) and (xm < Width) then
          FScanLine[ym][xm] := Src^;
        Inc(Src);
      end;
      Src := Pointer(TCnNativeInt(Src) + Gap);
    end;
  finally
    Buf := nil;
    Bmp.Free;
    Changed;
  end;
end;

// 波浪效果
procedure TCnBitmap.Wave(XDiv, YDiv, RatioVal: Double; Wrap: Boolean);
type
  TArray = array[0..0] of Integer;
  PArray = ^TArray;
var
  I, J, XSrc, YSrc: Integer;
  st: PArray;
  Pix: PCnColor;
  Line: PCnLine;
  Dst: TCnBitmap;
  Max: TCnNativeInt;
  PInt: PInteger;
begin
  if Empty or (YDiv <= 0) or (XDiv <= 0) or (RatioVal <= 0) then
    Exit;

  Changing;
  Line := nil;
  Max := 0;
  st := nil;
  Dst := TCnBitmap.Create;
  try
    Dst.LoadBlank(FWidth, FHeight);
    GetMem(st, 4 * FHeight);
    for J := 0 to FHeight - 1 do
      st[J] := Round(RatioVal * Sin(J / YDiv));

    if Wrap then
      Max := TCnNativeInt(FScanLine[FHeight - 1]) + RowInc;

    for I := 0 to FWidth - 1 do
    begin
      YSrc := Round(RatioVal * Sin(I / XDiv));

      if Wrap then
      begin
        if YSrc < 0 then
          YSrc := FHeight - 1 - (-YSrc mod FHeight)
        else if YSrc >= FHeight then
          YSrc := YSrc mod (FHeight - 1);
      end;

      Pix := Pointer(TCnNativeInt(Dst.Bits) + I * 3);
      if ((YSrc >= 0) and (YSrc < FHeight)) or Wrap then
        Line := FScanLine[YSrc];
      PInt := PInteger(st);

      for J := 0 to FHeight - 1 do
      begin
        if Wrap then
        begin
          XSrc := I + PInt^;
          Inc(PInt);
          if XSrc < 0 then
            XSrc := FWidth - 1 - (-XSrc mod FWidth)
          else if XSrc >= FWidth then
            XSrc := XSrc mod FWidth;
          Pix^ := Line[XSrc];
          Pix := Pointer(TCnNativeInt(Pix) + Dst.RowInc);
          Line := Pointer(TCnNativeInt(Line) + FRowInc);
          if TCnNativeInt(Line) >= Max then
            Line := FBits;
        end
        else
        begin
          if (YSrc >= FHeight) then Break;
          XSrc := I + st[J];
          if (XSrc > -1) and (XSrc < FWidth) and (YSrc > -1) then
            Pix^ := Line^[XSrc]
          else if YSrc = -1 then
          begin
            Pix := Pointer(TCnNativeInt(Pix) + Dst.RowInc);
            Line := FBits;
            YSrc := 0;
            Continue;
          end;
          Pix := Pointer(TCnNativeInt(Pix) + Dst.RowInc);
          Line := Pointer(TCnNativeInt(Line) + RowInc);
          Inc(YSrc);
        end;
      end;
    end;
    CopyMemory(FBits, Dst.Bits, FSize);
  finally
    if st <> nil then
      FreeMem(st);
    Dst.Free;
  end;
  Changed;
end;

// 马赛克化
procedure TCnBitmap.Mosaic(xAmount, yAmount: Integer);
var
  Delta: Integer;
  tx, ty, cx, cy, ix, iy, x, y: Integer;
  Col: TCnColor;
  Pix: PCnColor;
  Line: PCnLine;
begin
  if Empty or (xAmount < 1) or (yAmount < 1) then
    Exit;

  Changing;
  ix := (xAmount shr 1) + (xAmount and 1);
  iy := (yAmount shr 1) + (yAmount and 1);
  y := 0;
  while y < Height do
  begin
    x := 0;
    cy := y + iy;
    if cy >= Height then
      Line := FScanLine[Height - 1]
    else
      Line := FScanLine[cy];
    if y + yAmount - 1 >= Height then
      ty := Height - 1 - y
    else
      ty := yAmount;
    while x < Width do
    begin
      cx := x + ix;
      if cx >= Width then
        Col := Line[Width - 1]
      else
        Col := Line[cx];
      if x + xAmount - 1 >= Width then
        tx := Width - 1 - x
      else
        tx := xAmount;
      Delta := RowInc - tx * 3;
      Pix := PTR(TCnNativeInt(FScanLine[y]) + x * 3);
      for cy := 1 to ty do
      begin
        for cx := 1 to tx do
        begin
          Pix^ := Col;
          Inc(Pix);
        end;
        Pix := PTR(TCnNativeInt(Pix) + Delta);
      end;
      Inc(x, xAmount);
    end;
    Inc(y, yAmount);
  end;
  Changed;
end;

// 旋涡效果
procedure TCnBitmap.Twist(Amount: Integer);
var
  fxmid, fymid: Single;
  txmid, tymid: Single;
  fx, fy: Single;
  tx2, ty2: Single;
  r: Single;
  theta: Single;
  ifx, ify: Integer;
  dx, dy: Single;
  OFFSET: Single;
  ty, tx: Integer;
  weight_x, weight_y: array[0..1] of Single;
  Weight: Single;
  new_red, new_green: Integer;
  new_blue: Integer;
  total_red, total_green: Single;
  total_blue: Single;
  ix, iy: Integer;
  sli, slo: PCnLine;
  Buff: Pointer;
  BuffOff: TCnNativeInt;

  function ArcTan2(xt, yt: Single): Single;
  begin
    if xt = 0 then
      if yt > 0 then
        Result := Pi / 2
      else
        Result := -(Pi / 2)
    else
    begin
      Result := ArcTan(yt / xt);
      if xt < 0 then
        Result := Pi + ArcTan(yt / xt);
    end;
  end;

begin
  if Empty or (Amount <= 0) then Exit;
  Changing;
  GetMem(Buff, Size);
  try
    Move(Bits^, Buff^, Size);
    BuffOff := TCnNativeInt(Buff) - TCnNativeInt(Bits);
    OFFSET := -(Pi / 2);
    dx := Width - 1;
    dy := Height - 1;
    r := Sqrt(dx * dx + dy * dy);
    tx2 := r;
    ty2 := r;
    txmid := (Width - 1) / 2; //Adjust these to move center of rotation
    tymid := (Height - 1) / 2; //Adjust these to move ......
    fxmid := (Width - 1) / 2;
    fymid := (Height - 1) / 2;
    if tx2 >= Width then tx2 := Width - 1;
    if ty2 >= Height then ty2 := Height - 1;

    for ty := 0 to Round(ty2) do
    begin
      for tx := 0 to Round(tx2) do
      begin
        dx := tx - txmid;
        dy := ty - tymid;
        r := Sqrt(dx * dx + dy * dy);
        if r = 0 then
        begin
          fx := 0;
          fy := 0;
        end
        else
        begin
          theta := ArcTan2(dx, dy) - r / Amount - OFFSET;
          fx := r * Cos(theta);
          fy := r * Sin(theta);
        end;
        fx := fx + fxmid;
        fy := fy + fymid;

        ify := Trunc(fy);
        ifx := Trunc(fx);
                // Calculate the weights.
        if fy >= 0 then
        begin
          weight_y[1] := fy - ify;
          weight_y[0] := 1 - weight_y[1];
        end
        else
        begin
          weight_y[0] := -(fy - ify);
          weight_y[1] := 1 - weight_y[0];
        end;
        if fx >= 0 then
        begin
          weight_x[1] := fx - ifx;
          weight_x[0] := 1 - weight_x[1];
        end
        else
        begin
          weight_x[0] := -(fx - ifx);
          weight_x[1] := 1 - weight_x[0];
        end;

        if ifx < 0 then
          ifx := Width - 1 - (-ifx mod Width)
        else if ifx > Width - 1 then
          ifx := ifx mod Width;
        if ify < 0 then
          ify := Height - 1 - (-ify mod Height)
        else if ify > Height - 1 then
          ify := ify mod Height;

        total_red := 0.0;
        total_green := 0.0;
        total_blue := 0.0;
        for ix := 0 to 1 do
        begin
          for iy := 0 to 1 do
          begin
            if ify + iy < Height then
              sli := Pointer(TCnNativeInt(FScanLine[ify + iy]) + BuffOff)
            else
              sli := Pointer(TCnNativeInt(FScanLine[Height - ify - iy]) + BuffOff);
            if ifx + ix < Width then
            begin
              new_red := sli^[ifx + ix].r;
              new_green := sli^[ifx + ix].g;
              new_blue := sli^[ifx + ix].b;
            end
            else
            begin
              new_red := sli^[Width - ifx - ix].r;
              new_green := sli^[Width - ifx - ix].g;
              new_blue := sli^[Width - ifx - ix].b;
            end;
            Weight := weight_x[ix] * weight_y[iy];
            total_red := total_red + new_red * Weight;
            total_green := total_green + new_green * Weight;
            total_blue := total_blue + new_blue * Weight;
          end;
        end;
        slo := FScanLine[ty];
        slo^[tx].r := Round(total_red);
        slo^[tx].g := Round(total_green);
        slo^[tx].b := Round(total_blue);
      end;
    end;
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// 在当前图像上产生光照效果
// 算法设计：周劲羽 2002.03.04
// 使用优化算法，速度较快
procedure TCnBitmap.Lighting(Center: TPoint; OffX, OffY: Integer;
  Angle: Double; Color: TColor; Amount: TCnAlpha);
var
  Col: PCnColor;
  FAlpha: Integer;
  ARect: TRect;
  Table: array[0..90] of Integer; // 光照椭圆范围90度内半径长度
  I, x, y: Integer;
  r, g, b: Byte;
  tx, ty, tz, ta, tb, tab: Double;
  Len, MLen, beta, Weight: Integer;
begin
  if Empty then Exit;
  FAlpha := AlphaToInt(Amount);
  if FAlpha = 0 then Exit;
  OffX := Abs(OffX);
  OffY := Abs(OffY);
  if (OffX = 0) or (OffY = 0) then Exit;
  if not GetRotateRect(ClientRect, Center, OffX * 2, OffY * 2, Angle, ARect) then Exit;

  Changing;
  ta := Sqr(OffX);
  tb := Sqr(OffY);
  tab := ta * tb;
  Table[0] := OffX;           // 0 度时为长轴
  Table[90] := OffY;          // 90 度时为短轴
  for I := 1 to 89 do
  begin
    tz := Tan(I * PI / 180);
    tx := Sqrt(tab / (tb + ta * Sqr(tz))); // i 度时 X 坐标
    ty := tx * tz;            // i 度时 Y 坐标
    Table[I] := Round(Hypot(tx, ty)); // 中心距（i 度半径）
  end;

  DeRGB(Color, r, g, b);
  MLen := Max(OffX, OffY);
  for y := ARect.Top to ARect.Bottom - 1 do
  begin
    Col := @FScanLine[y, ARect.Left];
    for x := ARect.Left to ARect.Right - 1 do
    begin
      Len := Round(Hypot(x - Center.x, y - Center.y)); // 中心距
      if Len < MLen then
      begin
        if Center.x = x then  // 垂直线上
          beta := 90
        else if Center.y = y then // 水平线上
          beta := 0
        else
          beta := Round(ArcTan((y - Center.y) / (x - Center.x)) * 180 / PI);
        beta := Round(beta - Angle) mod 360; // 点与中心连线与X轴的夹角
        if beta < 0 then Inc(beta, 360);
        if beta > 270 then
          beta := 360 - beta
        else if beta > 180 then
          beta := beta - 180
        else if beta > 90 then
          beta := 180 - beta; // 变换到90度范围内
        if Len <= Table[beta] then
        begin                 // 加权值为点到中心的距离与中心到该方向椭圆周距离之比
          Weight := FAlpha * (Table[beta] - Len) div Table[beta];
          Col.b := Col.b + (b - Col.b) * Weight shr 8;
          Col.g := Col.g + (g - Col.g) * Weight shr 8;
          Col.r := Col.r + (r - Col.r) * Weight shr 8;
        end;
      end;
      Inc(Col);
    end;
  end;
  Changed;
end;

// 光照效果
procedure TCnBitmap.Lighting(Rect: TRect; Data: TCnLighting);
var
  Center: TPoint;
  W, H: Integer;
begin
  if Empty or not Assigned(Data) then Exit;
  W := RectWidth(Rect);
  H := RectHeight(Rect);
  Center.x := (Rect.Left + Rect.Right) div 2 + Data.OffsetX * W div 100;
  Center.y := (Rect.Top + Rect.Bottom) div 2 + Data.OffsetY * H div 100;
  Lighting(Center, Data.Width * W div 100, Data.Height * H div 100, Data.Angle,
    Data.Color, Data.Alpha);
end;

// 转换为黑白掩码图
procedure TCnBitmap.Mask(MaskColor: TCnColor);
begin
  MaskEx(MaskColor, CnColor(0, 0, 0), CnColor(255, 255, 255));
end;

procedure TCnBitmap.Mask(MaskColor: TColor);
begin
  Mask(CnColor(MaskColor));
end;

// 转换为二色掩码图
procedure TCnBitmap.MaskEx(MaskColor, InColor, BackColor: TCnColor);
var
  x, y: Integer;
  Col: PCnColor;
begin
  if Empty then Exit;
  Changing;
  Col := FBits;
  for y := 0 to FHeight - 1 do
  begin
    for x := 0 to FWidth - 1 do
    begin
      if (Col.r = MaskColor.r) and (Col.g = MaskColor.g) and (Col.b = MaskColor.b) then
        Col^ := InColor
      else
        Col^ := BackColor;
      Inc(Col);
    end;
    Col := Pointer(TCnNativeInt(Col) + FGap);
  end;
  Changed;
end;

procedure TCnBitmap.MaskEx(MaskColor, InColor, BackColor: TColor);
begin
  MaskEx(CnColor(MaskColor), CnColor(InColor), CnColor(BackColor));
end;

// 增加彩色噪声点
procedure TCnBitmap.AddColorNoise(Amount: Integer);
var
  x, y: Integer;
  pc: PCnColor;
begin
  if Empty then Exit;
  Changing;
  Amount := TrimInt(Amount, 0, 255);
  pc := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin                     // 增加随机色点
      pc.b := IntToByte(pc.b + (Random(Amount) - (Amount shr 1)));
      pc.g := IntToByte(pc.g + (Random(Amount) - (Amount shr 1)));
      pc.r := IntToByte(pc.r + (Random(Amount) - (Amount shr 1)));
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// 增加黑白噪声点
procedure TCnBitmap.AddMonoNoise(Amount: Integer);
var
  x, y, a: Integer;
  pc: PCnColor;
begin
  if Empty then Exit;
  Changing;
  Amount := TrimInt(Amount, 0, 255);
  pc := Bits;
  for y := 0 to Height - 1 do
  begin
    for x := 0 to Width - 1 do
    begin                     // 增加随机灰度点
      a := Random(Amount) - (Amount shr 1);
      pc.b := IntToByte(pc.b + a);
      pc.g := IntToByte(pc.g + a);
      pc.r := IntToByte(pc.r + a);
      Inc(pc);
    end;
    pc := Pointer(TCnNativeInt(pc) + Gap);
  end;
  Changed;
end;

// 移去噪声点（阀值平滑算子3X3卷积）
// 算法设计：周劲羽
procedure TCnBitmap.RemoveNoise(Amount: Integer);
var
  dr, dg, db: Byte;
  y1, y2, y3, Dst: PCnLine;
  Buff: Pointer;
  x, y: Integer;
begin
  if Empty then Exit;
  Changing;
  Amount := TrimInt(Amount, 0, 255);
  GetMem(Buff, Size);
  try
    CopyMemory(Buff, Bits, Size);
    y1 := Buff;
    y2 := Pointer(TCnNativeInt(y1) + RowInc);
    y3 := Pointer(TCnNativeInt(y2) + RowInc);
    Dst := FScanLine[1];
    for y := 1 to Height - 2 do
    begin
      for x := 1 to Width - 2 do
      begin                   // 邻近八象素平均值
        db := (y1[x - 1].b + y1[x].b + y1[x + 1].b + y2[x - 1].b +
          y2[x + 1].b + y3[x - 1].b + y3[x].b + y3[x + 1].b) shr 3;
        dg := (y1[x - 1].g + y1[x].g + y1[x + 1].g + y2[x - 1].g +
          y2[x + 1].g + y3[x - 1].g + y3[x].g + y3[x + 1].g) shr 3;
        dr := (y1[x - 1].r + y1[x].r + y1[x + 1].r + y2[x - 1].r +
          y2[x + 1].r + y3[x - 1].r + y3[x].r + y3[x + 1].r) shr 3;
        if (db - Dst[x].b >= Amount) or (Dst[x].b - db <= Amount) or
          (dg - Dst[x].g >= Amount) or (Dst[x].g - dg <= Amount) or
          (dr - Dst[x].r >= Amount) or (Dst[x].r - dr <= Amount) then
        begin
          Dst[x].b := db;
          Dst[x].g := dg;
          Dst[x].r := dr;
        end;
      end;
      y1 := Pointer(TCnNativeInt(y1) + RowInc);
      y2 := Pointer(TCnNativeInt(y2) + RowInc);
      y3 := Pointer(TCnNativeInt(y3) + RowInc);
      Dst := Pointer(TCnNativeInt(Dst) + RowInc);
    end;
  finally
    FreeMem(Buff);
  end;
  Changed;
end;

// 增加蒙板色
procedure TCnBitmap.AddMiddleColor(Color: TColor);
begin
  AddMiddleColorEx(Color, ClientRect);
end;

// 增加蒙板色（指定区域）
procedure TCnBitmap.AddMiddleColorEx(Color: TColor; Rect: TRect);
var
  I, J: Integer;
  r, g, b: Byte;
  pc: PCnColor;
  ARect: TRect;
  x, y, w, h: Integer;
begin
  if Empty then Exit;
  if not IntersectRect(ARect, Rect, ClientRect) then Exit;
  Changing;
  DeRect(ARect, x, y, w, h);
  DeRGB(Color, r, g, b);
  for I := y to y + h - 1 do
  begin
    pc := @FScanLine[I][x];
    for J := x to x + w - 1 do
    begin
      pc.b := (pc.b + b) shr 1; // 颜色平均值
      pc.g := (pc.g + g) shr 1;
      pc.r := (pc.r + r) shr 1;
      Inc(pc);
    end;
  end;
  Changed;
end;

//--------------------------------------------------------//
// 其它图像处理                                           //
// 算法来源：FastLib                                      //
// 算法修改：周劲羽                                       //
//--------------------------------------------------------//

// 根据四角颜色值用渐变色填充矩形
// ( c[0,0]    c[1,0]
//   c[0,1]    c[1,1] )
procedure TCnBitmap.InterpolateRect(Rect: TRect; c00, c10, c01, c11: TColor);
begin
  InterpolateRect(Rect, CnColor(c00), CnColor(c10),
    CnColor(c01), CnColor(c11));
end;

procedure TCnBitmap.InterpolateRect(Rect: TRect; c00, c10, c01, c11: TCnColor);
var
  xCount, yCount: Integer;
  t, t2, z, iz: Integer;
  rp, rp2, gp: Integer;
  gp2, bp, bp2: Integer;
  xx, dx: Integer;
  x1, x2, y1, y2: Integer;
  pb: PCnColor;
  ARect: TRect;
begin
  if Empty then Exit;
  if not IntersectRect(ARect, ClientRect, Rect) then Exit;
  Changing;
  x1 := ARect.Left;
  y1 := ARect.Top;
  x2 := ARect.Right;
  y2 := ARect.Bottom;
  z := 0;
  iz := $100000;
  if x2 <> x1 then
    t := $100000 div (x2 - x1)
  else
    t := 0;
  if y2 <> y1 then
    t2 := $100000 div (y2 - y1)
  else
    t2 := 0;
  dx := x2 - x1;
  for yCount := y1 to y2 do
  begin
    xx := ((c00.r * iz + c01.r * z) shr 20);
    rp := xx shl 20;          // 起始值（垂直方向已渐变）
    rp2 := (((c10.r * iz + c11.r * z) shr 20) - xx) * t; // 水平增量
    xx := ((c00.g * iz + c01.g * z) shr 20);
    gp := xx shl 20;
    gp2 := (((c10.g * iz + c11.g * z) shr 20) - xx) * t;
    xx := ((c00.b * iz + c01.b * z) shr 20);
    bp := xx shl 20;
    bp2 := (((c10.b * iz + c11.b * z) shr 20) - xx) * t;
    pb := @FScanLine[yCount][x1];
    for xCount := 0 to dx do
    begin
      pb.b := bp shr 20;
      Inc(bp, bp2);
      pb.g := gp shr 20;
      Inc(gp, gp2);
      pb.r := rp shr 20;
      Inc(rp, rp2);
      Inc(pb);
    end;
    Inc(z, t2);
    Dec(iz, t2);
  end;
  Changed;
end;

//--------------------------------------------------------//
// 抗锯齿画笔绘制方法（支持小数）                         //
// 算法设计：周劲羽                                       //
// 算法参考：Graphic32、FastLib                           //
//--------------------------------------------------------//

// 取浮点数象素颜色
function TCnBitmap.GetPixelsF(x, y: Single): TCnColor;
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixelF, [x, y])
  else
    Result := DoGetPixelF(Round(x * $8000), Round(y * $8000));
end;

// 写浮点数象素颜色
procedure TCnBitmap.SetPixelsF(x, y: Single; const Value: TCnColor);
begin
  if Empty then
    raise EBitmapIsEmpty.Create(SBitmapIsEmpty);
  if (x < 0) or (x > Width - 1) or (y < 0) or (y > Height - 1) then
    raise EInvalidPixel.CreateFmt(SInvalidPixelF, [x, y])
  else
    DoSetPixelF(Round(x * $8000), Round(y * $8000), Value);
end;

// 取小数（乘$7FFF）点象素
function TCnBitmap.DoGetPixelF(x, y: Integer): TCnColor;
var
  x1, x2, y1, y2: Integer;
  zx, zy, izy: Integer;
  w1, w2, w3, w4: Integer;
  Col1, Col2, Col3, Col4: PCnColor;
begin
  x1 := x shr 15;
  x2 := x1 + 1;
  y1 := y shr 15;
  y2 := y1 + 1;
  if (x1 < 0) or (x2 >= Width) or (y1 < 0) or (y2 >= Height) then Exit;
  zx := x and $7FFF;
  zy := y and $7FFF;
  izy := zy xor $7FFF;
  w2 := (zx * izy) shr 15;    // 计算加权值
  w1 := izy - w2;
  w4 := (zx * zy) shr 15;
  w3 := zy - w4;
  Col1 := @FScanLine[y1][x1];
  Col2 := @FScanLine[y1][x2];
  Col3 := @FScanLine[y2][x1];
  Col4 := @FScanLine[y2][x2];
  Result.b := (Col1.b * w1 + Col2.b * w2 + Col3.b * w3 + Col4.b * w4) shr 15;
  Result.g := (Col1.g * w1 + Col2.g * w2 + Col3.g * w3 + Col4.g * w4) shr 15;
  Result.r := (Col1.r * w1 + Col2.r * w2 + Col3.r * w3 + Col4.r * w4) shr 15;
end;

// 写小数（乘$7FFF）点象素
procedure TCnBitmap.DoSetPixelF(x, y: Integer; ARGB: TCnColor);
var
  x1, x2, y1, y2: Integer;
  zx, zy, izy: Integer;
  w1, w2, w3, w4: Integer;
begin
  x1 := x shr 15;
  x2 := x1 + 1;
  y1 := y shr 15;
  y2 := y1 + 1;
  if (x2 < 0) or (x1 >= Width) or (y2 < 0) or (y1 >= Height) then Exit;
  zx := x and $7FFF;
  zy := y and $7FFF;
  izy := zy xor $7FFF;
  w2 := (zx * izy) shr 15;    // 计算加权值
  w1 := izy - w2;
  w4 := (zx * zy) shr 15;
  w3 := zy - w4;
  if (y1 >= 0) and (x1 >= 0) then
    with FScanLine[y1][x1] do
    begin
      b := b + (ARGB.b - b) * w1 shr 15;
      g := g + (ARGB.g - g) * w1 shr 15;
      r := r + (ARGB.r - r) * w1 shr 15;
    end;
  if (y1 >= 0) and (x2 < Width) then
    with FScanLine[y1][x2] do
    begin
      b := b + (ARGB.b - b) * w2 shr 15;
      g := g + (ARGB.g - g) * w2 shr 15;
      r := r + (ARGB.r - r) * w2 shr 15;
    end;
  if (y2 < Height) and (x1 > 0) then
    with FScanLine[y2][x1] do
    begin
      b := b + (ARGB.b - b) * w3 shr 15;
      g := g + (ARGB.g - g) * w3 shr 15;
      r := r + (ARGB.r - r) * w3 shr 15;
    end;
  if (y2 < Height) and (x2 < Width) then
    with FScanLine[y2][x2] do
    begin
      b := b + (ARGB.b - b) * w4 shr 15;
      g := g + (ARGB.g - g) * w4 shr 15;
      r := r + (ARGB.r - r) * w4 shr 15;
    end;
end;

// 限制直线范围
// 算法来源：Graphic32
function TCnBitmap.ClipLineF(var X0, Y0, X1, Y1: Single;
  MinX, MaxX, MinY, MaxY: Single): Boolean;
type
  Edge = (Left, Right, Top, Bottom);
  OutCode = set of Edge;
var
  Accept, AllDone: Boolean;
  OutCode0, OutCode1, OutCodeOut: OutCode;
  x, y: Single;

  procedure CompOutCode(x, y: Single; var Code: OutCode);
  begin
    Code := [];
    if x < MinX then Code := Code + [Left];
    if x > MaxX then Code := Code + [Right];
    if y < MinY then Code := Code + [Top];
    if y > MaxY then Code := Code + [Bottom];
  end;

begin
  Accept := False;
  AllDone := False;
  CompOutCode(X0, Y0, OutCode0);
  CompOutCode(X1, Y1, OutCode1);
  repeat
    if (OutCode0 = []) and (OutCode1 = []) then // 完成
    begin
      Accept := True;
      AllDone := True;
    end
    else if (OutCode0 * OutCode1) <> [] then AllDone := True
    else                      // 计算交叉
    begin
      if OutCode0 <> [] then OutCodeOut := OutCode0
      else OutCodeOut := OutCode1;
      x := 0;
      y := 0;
      if Left in OutCodeOut then
      begin
        y := Y0 + (Y1 - Y0) * (MinX - X0) / (X1 - X0);
        x := MinX;
      end
      else if Right in OutCodeOut then
      begin
        y := Y0 + (Y1 - Y0) * (MaxX - X0) / (X1 - X0);
        x := MaxX - 1;
      end
      else if Top in OutCodeOut then
      begin
        x := X0 + (X1 - X0) * (MinY - Y0) / (Y1 - Y0);
        y := MinY;
      end
      else if Bottom in OutCodeOut then
      begin
        x := X0 + (X1 - X0) * (MaxY - Y0) / (Y1 - Y0);
        y := MaxY;
      end;
      if OutCodeOut = OutCode0 then
      begin
        X0 := x;
        Y0 := y;
        CompOutCode(X0, Y0, OutCode0);
      end
      else
      begin
        X1 := x;
        Y1 := y;
        CompOutCode(X1, Y1, OutCode1);
      end
    end;
  until AllDone;
  Result := Accept;
end;

// 绘制直线
procedure TCnBitmap.DrawLineF(X1, Y1, X2, Y2: Single; Color: TColor);
var
  n, I: Integer;
  px, py, ex, ey, nx, ny, hyp: Integer;
  ARGB: TCnColor;
begin
  if not ClipLineF(x1, y1, x2, y2, 0, Width - 1, 0, Height - 1) then Exit;
  Changing;
  ARGB := CnColor(Color);
  px := Round(x1 * $8000);
  py := Round(y1 * $8000);
  ex := Round(x2 * $8000);
  ey := Round(y2 * $8000);
  nx := ex - px;              // 宽度
  ny := ey - py;              // 高度
  if (nx = 0) or (ny = 0) then
    hyp := Round(Hypot(nx, ny))
  else
    case FPenWeight of        // 斜边长
      pwThin: hyp := Round(Hypot(nx, ny));
      pwNormal: hyp := Round(Hypot(nx, ny) * 1.4); // 粗细修正
      pwThick: hyp := Round(Hypot(nx, ny) * 1.8);
    else hyp := Round(Hypot(nx, ny));
    end;

  if hyp < 256 then Exit;
  n := hyp shr 15;
  if n > 0 then
  begin
    nx := Round(nx / hyp * $8000);
    ny := Round(ny / hyp * $8000);
    for I := 0 to n - 1 do
    begin
      DoSetPixelF(px, py, ARGB); // 绘制点
      px := px + nx;
      py := py + ny;
    end;
    DoSetPixelF(ex, ey, ARGB); // 绘制端点
  end;
  Changed;
end;

// 绘制到指定点
procedure TCnBitmap.LineToF(x, y: Single);
begin
  DrawLineF(FPenPosF.x, FPenPosF.y, x, y, FPenColor);
  MoveToF(x, y);
end;

// 绘制到指定点
procedure TCnBitmap.LineToF(Point: TPointF);
begin
  LineToF(Point.x, Point.y);
end;

// 移动画笔
procedure TCnBitmap.MoveToF(Point: TPointF);
begin
  FPenPosF := Point;
end;

// 移动画笔
procedure TCnBitmap.MoveToF(x, y: Single);
begin
  MoveToF(PointF(x, y));
end;

// 绘制矩形
procedure TCnBitmap.DrawRectF(const Rect: TRectF);
begin
  with Rect do
  begin
    DrawLineF(Left, Top, Left, Bottom, FPenColor);
    DrawLineF(Left, Bottom, Right, Bottom, FPenColor);
    DrawLineF(Right, Bottom, Right, Top, FPenColor);
    DrawLineF(Right, Top, Left, Top, FPenColor);
  end;
end;

// 绘制折线
procedure TCnBitmap.PolylineF(const Points: TPointFArray);
var
  I: Integer;
  SavePenPos: TPointF;
begin
  if Empty or (Length(Points) < 2) then Exit;
  BeginUpdate;
  try
    SavePenPos := FPenPosF;
    FPenPosF := Points[Low(Points)];
    for I := Low(Points) + 1 to High(Points) do
      LineToF(Points[I]);
    FPenPosF := SavePenPos;
  finally
    EndUpdate;
  end;
end;

// 绘制椭圆
procedure TCnBitmap.EllipseF(X1, Y1, X2, Y2: Single);
var
  x, y: Integer;
  a, b: Single;
  cx, cy: Single;
  xl, xr, yt, yb: Single;
  xl1, xr1, yt1, yb1: Single;
  tmp: Single;
begin
  Changing;
  a := Abs(x2 - x1) / 2;      // 半径
  b := Abs(y2 - y1) / 2;
  if a = 0 then
    DrawLineF(x1, y1, x1, y2, FPenColor) // 水平线
  else if b = 0 then
    DrawLineF(x1, y1, x2, y1, FPenColor) // 垂直线
  else if a <= b then
  begin
    cx := (x1 + x2) / 2;      // 中心点
    cy := (y1 + y2) / 2;

    xl1 := cx;
    xr1 := cx;
    yt1 := cy - b;
    yb1 := cy + b;
    for x := 1 to Ceil(a) do
    begin
      if x < a then           // 半径内
      begin
        xl := cx - x;
        xr := cx + x;
        tmp := b * Sqrt(1 - Sqr(x / a));
      end else
      begin                   // 边界点
        xl := cx - a;
        xr := cx + a;
        tmp := 0;
      end;
      yt := cy - tmp;
      yb := cy + tmp;
      DrawLineF(xl1, yt1, xl, yt, FPenColor); // 左上部分
      DrawLineF(xl1, yb1, xl, yb, FPenColor); // 左下部分
      DrawLineF(xr1, yt1, xr, yt, FPenColor); // 右上部分
      DrawLineF(xr1, yb1, xr, yb, FPenColor); // 右下部分
      xl1 := xl;
      xr1 := xr;
      yt1 := yt;
      yb1 := yb;
    end;
  end
  else
  begin
    cx := (x1 + x2) / 2;      // 中心点
    cy := (y1 + y2) / 2;

    yt1 := cy;
    yb1 := cy;
    xl1 := cx - a;
    xr1 := cx + a;
    for y := 1 to Ceil(b) do
    begin
      if y < b then           // 半径内
      begin
        yt := cy - y;
        yb := cy + y;
        tmp := a * Sqrt(1 - Sqr(y / b));
      end else
      begin                   // 边界点
        yt := cy - b;
        yb := cy + b;
        tmp := 0;
      end;
      xl := cx - tmp;
      xr := cx + tmp;
      DrawLineF(xl1, yt1, xl, yt, FPenColor); // 左上部分
      DrawLineF(xl1, yb1, xl, yb, FPenColor); // 左下部分
      DrawLineF(xr1, yt1, xr, yt, FPenColor); // 右上部分
      DrawLineF(xr1, yb1, xr, yb, FPenColor); // 右下部分
      xl1 := xl;
      xr1 := xr;
      yt1 := yt;
      yb1 := yb;
    end;
  end;
  Changed;
end;

// 绘制椭圆
procedure TCnBitmap.EllipseF(const Rect: TRectF);
begin
  EllipseF(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

//--------------------------------------------------------//
// 平滑字体绘制方法                                       //
// 算法设计：周劲羽                                       //
// 算法来源：平滑特效字体控件包 AAFont V2.36（周劲羽）  //
// 原始算法：李文松兄提供的 AAFont V1.2 1999.07.13        //
//           liwensong@hotmail.com                        //
//           http://member.netease.com/~lws               //
//--------------------------------------------------------//

// 取文本大小
function TCnBitmap.TextExtent(const Text: string): TSize;
var
  ADC: HDC;
  SaveFont: HFont;
  SaveSize: Integer;
begin
  if Text = '' then
  begin
    Result := EnSize(0, 0);
    Exit;
  end;
  ADC := Windows.GetDC(0);
  SaveSize := Font.Size;
  Font.Size := Font.Size * Font.Scale;
  SaveFont := SelectObject(ADC, Font.Handle);
  Windows.GetTextExtentPoint(ADC, PChar(Text), Length(Text), Result);
  SelectObject(ADC, SaveFont);
  Font.Size := SaveSize;
  ReleaseDC(0, ADC);
  Result.cx := (Result.cx + Font.Scale - 1) div Font.Scale;
  Result.cy := (Result.cy + Font.Scale - 1) div Font.Scale;
  // 计算阴影
  if fsShadow in Font.StyleEx then
  begin
    Inc(Result.cx, Abs(Font.Shadow.FOffsetX));
    Inc(Result.cy, Abs(Font.Shadow.FOffsetY));
  end;
  // 斜体字宽度校正
  if fsItalic in Font.Style then
    Inc(Result.cx, Round(Result.cx / Length(Text) * csItalicAdjust));
end;

// 取文本高度
function TCnBitmap.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cy;
end;

// 取文本宽度
function TCnBitmap.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cx;
end;

// 初始化灰度调化板数据
procedure InitGrayPal;
var
  I: Integer;
begin
  GrayLogPal.lpal.palVersion := $300;
  GrayLogPal.lpal.palNumEntries := 256;
  for I := 0 to 255 do
  begin
    GrayLogPal.dummy[I].peRed := I;
    GrayLogPal.dummy[I].peGreen := I;
    GrayLogPal.dummy[I].peBlue := I;
    GrayLogPal.dummy[I].peFlags := 0;
  end;
end;

// 初始化用于平滑字体绘制的灰度图
procedure TCnBitmap.InitGrayBmp;
begin
  if FGrayBmp = nil then
  begin
    FGrayBmp := TBitmap.Create;
    FGrayBmp.PixelFormat := pf8Bit;
    FGrayBmp.Canvas.Brush.Style := bsSolid;
    FGrayBmp.Canvas.Brush.Color := clBlack;
    FGrayBmp.Palette := CreatePalette(GrayLogPal.lpal);
  end;
  if FFontMask = nil then FFontMask := TCnFontMask.Create;
end;

// 释放用于平滑字体绘制的灰度图
procedure TCnBitmap.FreeGrayBmp;
var
  HPal: HPALETTE;
begin
  if FGrayBmp <> nil then
  begin
    HPal := FGrayBmp.Palette;
    FGrayBmp.Palette := 0;
    FreeAndNil(FGrayBmp);
    DeleteObject(HPal);
  end;
  if FFontMask <> nil then FreeAndNil(FFontMask);
end;

// 绘制平滑字体蒙板
procedure TCnBitmap.DrawFontMaskEx(const Text: string; Extend: TSize; Point: TPoint);
var
  I, J: Integer;
  pS1, pS2, pS3, pS4: PByteArray;
  pDst: PByteArray;
  GrayRowInc: Integer;
  x: Integer;
begin
  InitGrayBmp;
  FGrayBmp.Width := Extend.cx * Font.Scale;
  FGrayBmp.Height := Extend.cy * Font.Scale;
  FFontMask.SetSize(Extend.cx, Extend.cy);

  FGrayBmp.Canvas.Font.Assign(Font); // 设定字体
  FGrayBmp.Canvas.Font.Height := FGrayBmp.Canvas.Font.Height * Font.Scale;
  FGrayBmp.Canvas.Font.Color := clWhite;
  Windows.FillRect(FGrayBmp.Canvas.Handle, Bounds(0, 0, FGrayBmp.Width,
    FGrayBmp.Height), 0);
  Windows.TextOut(FGrayBmp.Canvas.Handle, Point.x, Point.y, PChar(Text), Length(Text));

  GrayRowInc := (FGrayBmp.Width + 3) div 4 * 4; // 扫描线宽度
  pS1 := FGrayBmp.ScanLine[0]; // 源灰度图
  pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
  pS3 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
  pS4 := PByteArray(TCnNativeInt(pS3) - GrayRowInc);
  pDst := FFontMask.ScanLine[0];
  // 目标灰度为源矩形块的平均值
  case Font.Quality of
    fqHigh:
      begin                   // 高精度4X4采样
        for I := 0 to Extend.cy - 1 do
        begin
          for J := 0 to Extend.cx - 1 do
          begin
            x := J * 4;
            pDst^[J] :=
              (pS1^[x] + pS1^[x + 1] + pS1^[x + 2] + pS1^[x + 3] +
              pS2^[x] + pS2^[x + 1] + pS2^[x + 2] + pS2^[x + 3] +
              pS3^[x] + pS3^[x + 1] + pS3^[x + 2] + pS3^[x + 3] +
              pS4^[x] + pS4^[x + 1] + pS4^[x + 2] + pS4^[x + 3]) shr 4;
          end;
          pS1 := PByteArray(TCnNativeInt(pS4) - GrayRowInc);
          pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pS3 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
          pS4 := PByteArray(TCnNativeInt(pS3) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
    fqNormal:
      begin                   // 普通精度3X3采样
        for I := 0 to Extend.cy - 1 do
        begin
          for J := 0 to Extend.cx - 1 do
          begin
            x := J * 3;
            pDst^[J] :=
              (pS1^[x] + pS1^[x + 1] + pS1^[x + 2] shr 1 +
              pS2^[x] + pS2^[x + 1] + pS2^[x + 2] +
              pS3^[x] shr 1 + pS3^[x + 1] + pS3^[x + 2]) shr 3;
          end;
          pS1 := PByteArray(TCnNativeInt(pS3) - GrayRowInc);
          pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pS3 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
    fqLow:
      begin                   // 低精度2X2采样
        for I := 0 to Extend.cy - 1 do
        begin
          for J := 0 to Extend.cx - 1 do
          begin
            x := J * 2;
            pDst^[J] :=
              (pS1^[x] + pS1^[x + 1] +
              pS2^[x] + pS2^[x + 1]) shr 2;
          end;
          pS1 := PByteArray(TCnNativeInt(pS2) - GrayRowInc);
          pS2 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
    fqNone:
      begin                   // 无平滑效果
        for I := 0 to Extend.cy - 1 do
        begin
          CopyMemory(pDst, pS1, Extend.cx);
          pS1 := PByteArray(TCnNativeInt(pS1) - GrayRowInc);
          pDst := PByteArray(TCnNativeInt(pDst) + FFontMask.FRowInc);
        end;
      end;
  end;
end;

// 绘制平滑字体蒙板
procedure TCnBitmap.DrawFontMask(const Text: string);
begin
  DrawFontMaskEx(Text, TextExtent(Text), Point(0, 0));
end;

// 计算阴影偏移
function TCnBitmap.GetShadowPoint: TPoint;
begin
  if fsShadow in Font.StyleEx then
  begin
    if Font.Shadow.OffsetX > 0 then
      Result.x := Font.Shadow.OffsetX
    else
      Result.x := 0;
    if Font.Shadow.OffsetY > 0 then
      Result.y := Font.Shadow.OffsetY
    else
      Result.y := 0;
  end
  else
  begin
    Result.x := 0;
    Result.y := 0;
  end;
end;

// 计算文本偏移
function TCnBitmap.GetTextPoint: TPoint;
begin
  if fsShadow in Font.StyleEx then
  begin
    if Font.Shadow.OffsetX < 0 then
      Result.x := Abs(Font.Shadow.OffsetX)
    else
      Result.x := 0;
    if Font.Shadow.OffsetY < 0 then
      Result.y := Abs(Font.Shadow.OffsetY)
    else
      Result.y := 0;
  end
  else
  begin
    Result.x := 0;
    Result.y := 0;
  end;
end;

// 文本按前景色与背景混合
procedure TCnBitmap.FontMaskBlend(x, y: Integer; AColor: TColor; Alpha: TCnAlpha;
  Mask: TCnFontMask);
var
  r, b, g: Byte;
  Src: PByteArray;
  Dst: PCnLine;
  Weight: Byte;
  dx, dy, sx, sy, w, h: Integer;
  I, J: Integer;
  FAlpha: Integer;
begin
  if Empty or not CalcDrawRect(x, y, Rect(0, 0, Mask.Width, Mask.Height),
    Rect(0, 0, Mask.Width, Mask.Height), dx, dy, sx, sy, w, h) then Exit;
  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;

  DeRGB(AColor, r, g, b);     // 色彩分量
  for J := 0 to h - 1 do
  begin
    Src := @Mask.ScanLine[sy + J][sx];
    Dst := @ScanLine[dy + J][dx];
    for I := 0 to w - 1 do
    begin
      Weight := Src[I] * FAlpha shr 8; // 混合系数
      if Weight <> 0 then
      begin
        if Weight = 255 then
        begin                 // 前景色
          Dst[I].b := b;
          Dst[I].g := g;
          Dst[I].r := r;
        end
        else
        begin                 // 混合
          Inc(Dst[I].b, Weight * (b - Dst[I].b) shr 8);
          Inc(Dst[I].g, Weight * (g - Dst[I].g) shr 8);
          Inc(Dst[I].r, Weight * (r - Dst[I].r) shr 8);
        end;
      end;
    end;
  end;
end;

// 文本按纹理与背景混合
procedure TCnBitmap.FontMaskBlendEx(x, y: Integer; Alpha: TCnAlpha;
  Mask: TCnFontMask; ForeBmp: TCnBitmap);
var
  Src: PByteArray;
  Fore, Dst: PCnLine;
  Weight: Byte;
  dx, dy, sx, sy, w, h: Integer;
  I, J: Integer;
  FAlpha: Integer;
begin
  if Empty or not CalcDrawRect(x, y, Rect(0, 0, Mask.Width, Mask.Height),
    Rect(0, 0, Mask.Width, Mask.Height), dx, dy, sx, sy, w, h) then Exit;

  if (ForeBmp.Width <> Mask.Width) or (ForeBmp.Height <> Mask.Height) then
    raise EInvalidForeBmp.Create(SInvalidForeBitmap); // 错误的纹理图

  FAlpha := AlphaToInt(Alpha);
  if FAlpha = 0 then Exit;

  for J := 0 to h - 1 do
  begin
    Src := @Mask.ScanLine[J + sy][sx];
    Fore := @ForeBmp.ScanLine[J + sy][sx];
    Dst := @ScanLine[J + dy][dx];
    for I := 0 to w - 1 do
    begin
      Weight := Src[I] * FAlpha shr 8; // 混合系数
      if Weight <> 0 then
      begin
        if Weight = 255 then
          Dst[I] := Fore[I]   // 前景色
        else
        begin                 // 混合
          Inc(Dst[I].b, Weight * (Fore[I].b - Dst[I].b) shr 8);
          Inc(Dst[I].g, Weight * (Fore[I].g - Dst[I].g) shr 8);
          Inc(Dst[I].r, Weight * (Fore[I].r - Dst[I].r) shr 8);
        end;
      end;
    end;
  end;
end;

// 平滑文本输出
procedure TCnBitmap.TextOut(x, y: Integer; const Text: string);
var
  TextPoint, ShadowPoint: TPoint;
  ShadowMask: TCnFontMask;
  Fore: TCnBitmap;
  IsTexture, IsGrad, IsLight, IsNoise: Boolean;
  I, ABlur: Integer;
begin
  if Empty or (Text = '') then Exit;
  BeginUpdate;
  try
    if fsShadow in Font.StyleEx then // 阴影计算
    begin
      TextPoint := GetTextPoint;
      ShadowPoint := GetShadowPoint;
      TextPoint.x := TextPoint.x + x;
      TextPoint.y := TextPoint.y + y;
      ShadowPoint.x := ShadowPoint.x + x;
      ShadowPoint.y := ShadowPoint.y + y;
    end
    else
    begin
      TextPoint := Point(x, y);
    end;

    DrawFontMask(Text);       // 创建字体蒙板
    if fsOutline in Font.StyleEx then // 轮廓化
      FFontMask.Outline;
    if fsSpray in Font.StyleEx then // 喷溅效果
      FFontMask.Spray(Font.Spray);

    if not FontClear then     // 背景不透明
      FillRect(Bounds(x, y, FFontMask.Width, FFontMask.Height), FontBkColor);

    if fsShadow in Font.StyleEx then // 阴影处理
    begin
      ShadowMask := TCnFontMask.Create; // 阴影蒙板
      try
        ABlur := Font.Shadow.Blur;
        if ABlur > 0 then     // 考虑模糊
        begin
          ShadowMask.SetSize(FFontMask.Width + 4 * ABlur, FFontMask.Height +
            4 * ABlur);
          with ShadowMask do
            FillChar(FBuff^, FRowInc * FHeight, 0);
          for I := 0 to FFontMask.FHeight - 1 do
            Move(FFontMask.ScanLine[I][0], ShadowMask.ScanLine[2 * ABlur + I][2 *
              ABlur],
                FFontMask.Width);
          ShadowMask.Blur(ABlur); // 阴影模糊
        end
        else
          FFontMask.CopyTo(ShadowMask);
        FontMaskBlend(ShadowPoint.x - 2 * ABlur, ShadowPoint.y - 2 * ABlur,
          Font.Shadow.Color,
          Font.Shadow.Alpha * Font.Alpha div csMaxAlpha, ShadowMask);
      finally
        ShadowMask.Free;
      end;
    end;

    IsTexture := (fsTexture in Font.StyleEx) and Assigned(Font.Texture.Graphic) and
      not Font.Texture.Graphic.Empty; // 字体纹理
    IsGrad := fsGradient in Font.StyleEx; // 字体渐变
    IsLight := fsLighting in Font.StyleEx; // 光照效果
    IsNoise := (fsNoise in Font.StyleEx) and (Font.Noise > 0); // 噪声效果
    if IsTexture or IsGrad or IsLight or IsNoise then
    begin
      Fore := TCnBitmap.Create;
      try
        Fore.SetSize(FFontMask.Width, FFontMask.Height);
        if not IsGrad then
          Fore.Fill(Font.Color)  // 无渐变效果时填充字体色
        else if Font.Gradient.Style = gsRadial then
          Fore.Fill(Font.Gradient.ColorEnd);
        if IsTexture then
          Fore.DrawMode(Font.Texture.Graphic, Font.TextureMode)
        else if IsGrad then
          Fore.DrawGradient(Font.Gradient);
        if IsNoise then
          Fore.AddColorNoise(Font.Noise);
        if IsLight then
          Fore.Lighting(Fore.ClientRect, Font.Lighting);
        FontMaskBlendEx(TextPoint.x, TextPoint.y, Font.Alpha, FFontMask, Fore);
      finally
        Fore.Free;
      end;
    end
    else
      FontMaskBlend(TextPoint.x, TextPoint.y, Font.Color, Font.Alpha, FFontMask);
  finally
    EndUpdate;
  end;
end;

type
  TCnParentControl = class(TWinControl);

// 从父控件复制背景。这个过程来自 RxLibrary VCLUtils
procedure CopyControlParentImageToCanvas(AControl: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if AControl.Parent = nil then Exit;
  Count := AControl.Parent.ControlCount;
  DC := Dest.Handle;
  with AControl.Parent do
    ControlState := ControlState + [csPaintCopy];

  try
    SelfR := Bounds(AControl.Left, AControl.Top, AControl.Width, AControl.Height);
    X := -AControl.Left;
    Y := -AControl.Top;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, AControl.Parent.ClientWidth,
        AControl.Parent.ClientHeight);
      try
        with TCnParentControl(AControl.Parent) do
        begin
          Perform(WM_ERASEBKGND, DC, 0);
          PaintWindow(DC);
        end;
      except
        ;
      end;
    finally
      RestoreDC(DC, SaveIndex);
    end;
    { Copy images of graphic controls }
    for I := 0 to Count - 1 do
    begin
      if AControl.Parent.Controls[I] = AControl then
        Break
      else if (AControl.Parent.Controls[I] <> nil) and
        (AControl.Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(AControl.Parent.Controls[I]) do
        begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            ControlState := ControlState + [csPaintCopy];
            SaveIndex := SaveDC(DC);
            try
              SetViewportOrgEx(DC, Left + X, Top + Y, nil);
              IntersectClipRect(DC, 0, 0, Width, Height);
              Perform(WM_PAINT, DC, 0);
            finally
              RestoreDC(DC, SaveIndex);
              ControlState := ControlState - [csPaintCopy];
            end;
          end;
        end;
      end;
    end;
  finally
    with AControl.Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

initialization
  BitmapList := TThreadList.Create;
  BitmapList.duplicates := dupIgnore; // 重复加入时忽略
  CnCanvasList := TThreadList.Create;
  CnCanvasList.duplicates := dupIgnore; // 重复加入时忽略
  GdiActTimer := TTimer.Create(nil);
  GdiActTimer.Interval := FreeGdiInterval;
  GdiActTimer.OnTimer := TCnBitmap.OnGdiActTimer;
  GdiActTimer.Enabled := True;
  InitGrayPal;

finalization
  GdiActTimer.Free;
  BitmapList.Free;
  CnCanvasList.Free;

end.

