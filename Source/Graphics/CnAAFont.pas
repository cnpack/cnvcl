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

unit CnAAFont;
{* |<PRE>
================================================================================
* 软件名称：CnPack 控件包
* 单元名称：平滑特效字体单元
* 单元作者：CnPack 开发组 周劲羽 (zjy@cnpack.org)
*           移植：e- 
*           樊升
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* 备　　注：平滑字体算法由李文松朋友提供的AAFont修改而来
* 最后更新：2021.07.23
*               TCnAABlend 的 BlendEx 方法以及 TCnAAFontEx 的 TextOutput 方法支持
*               目标为 32 位带 Alpha 通道的位图，需要调用者指定 DestIsAlpha 为 True
*               注意源图要 UnPreMultiply 的，输出为 PreMultiply 的。
*           2021.07.17
*               TCnAABlend 的 Blend 方法以及 TCnAAFont 的 TextOutput 方法支持目
*               标为 32 位带 Alpha 通道的位图，需要调用者指定 DestIsAlpha 为 True
*               注意在 XE 或以下版本中 32 位 Bitmap 对 Alpha 通道支持不完整，
*               要显示 TCnAAFont 所输出的带透明度的平滑文字图，需要自行调用
*               Windows.AlphaBlend 来显示。
* 移植日期：2015.06.15
*               修改输出名以躲过 BCB Unicode 下命名混淆的问题
* 移植日期：2006.08.18
*           2004.11.29
*               写成
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, Classes, Graphics, SysUtils, Consts, Controls, Forms,
  Registry, StdCtrls, ExtCtrls, Math, IniFiles, CnNative, CnClasses;

type

  TAAQuality = (aqHigh, aqNormal, aqLow, aqNone);
  {* 平滑字体显示精度类型
   |<PRE>
     aqHigh     - 4X4 采样的最高显示精度，速度较慢
     aqNormal   - 3X3 采样的普通显示精度，最佳性能速度比
     aqLow      - 2X2 采样的低显示精度，速度较快
     aqNone     - 无平滑效果
   |</PRE>}
  TAlpha = 0..100;
  {* 不透明度类型，0 为完全透明，100 为完全不透明}
  TBlurStrength = 0..100;
  {* 模糊度类型，0 为不模糊，100 为最大模糊度}
  TOffset = -20..20;
  {* 阴影偏移量范围}
  TSprayRange = 0..100;
  {* 喷溅效果参数范围}
  TAngle = -360..360;
  {* 旋转角度}
  TGradualStyle = (gsLeftToRight, gsRightToLeft, gsTopToBottom, gsBottomToTop,
    gsCenterToLR, gsCenterToTB);
  {* 渐变方式类型
   |<PRE>
     gsLeftToRight      - 从左向右渐变
     gsRightToLeft      - 从右向左渐变
     gsTopToBottom      - 从上向下渐变
     gsBottomToTop      - 从下向上渐变
     gsCenterToLR       - 从中间向左右渐变
     gsCenterToTB       - 从中间向上下渐变
   |</PRE>}
  TTextureMode = (tmTiled, tmStretched, tmCenter, tmNormal);
  {* 纹理图像显示模式
   |<PRE>
     tmTiled            - 平铺显示
     tmStretched        - 自动缩放显示
     tmCenter           - 在中心位置显示
     tmNormal           - 在左上角显示
   |</PRE>}
  THoriScrollType = (stNone, stRightToLeft, stLeftToRight);
  {* 横向滚动类型
   |<PRE>
     stNone             - 不动
     stRightToLeft      - 从右到左滚动
     stLeftToRight      - 从左到右滚动
   |</PRE>}

  TCnAAEffect = class;
  TCnAAMask = class;
  TCnAABlend = class;
  TCnAAFont = class;
  TCnAAFontEx = class;

{ TCnEnabledClass }

  TCnEnabledClass = class(TCnNotifyClass)
  {* 带 Enabled 功能的更新通知的持久性类，一般不需要直接使用}
  private
    FEnabled: Boolean;
  protected
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure Changed; override;
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* 类构造器，参数为通知事件}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    {* 是否允许使用}
  end;

{ TCnAAShadow }

  TCnAAShadow = class(TCnEnabledClass)
  {* 平滑特效字体阴影显示参数类，一般不需要用户直接创建}
  private
    FBlur: TBlurStrength;
    FAlpha: TAlpha;
    FColor: TColor;
    FOffsetX: TOffset;
    FOffsetY: TOffset;
    procedure SetBlur(const Value: TBlurStrength);
    procedure SetColor(const Value: TColor);
    procedure SetOffsetX(const Value: TOffset);
    procedure SetOffsetY(const Value: TOffset);
    procedure SetAlpha(const Value: TAlpha);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* 类构造器，参数为通知事件}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  published
    property Blur: TBlurStrength read FBlur write SetBlur default 80;
    {* 阴影模糊度，参数范围为 0..100，0 为不模糊}
    property Alpha: TAlpha read FAlpha write SetAlpha default 70;
    {* 阴影不透明度，参数范围为 0..100，0 为全透明，100 为不透明}
    property Color: TColor read FColor write SetColor default $00444444;
    {* 阴影颜色}
    property OffsetX: TOffset read FOffsetX write SetOffsetX default 2;
    {* 阴影水平方向偏移量，为负值时向左偏移}
    property OffsetY: TOffset read FOffsetY write SetOffsetY default 2;
    {* 阴影垂直方向偏移量，为负值时向上偏移}
  end;

{ TCnAAGradual }

  TCnAAGradual = class(TCnEnabledClass)
  {* 平滑特效字体渐变显示参数类，一般不需要用户直接创建}
  private
    FStyle: TGradualStyle;
    FStartColor: TColor;
    FEndColor: TColor;
    procedure SetStyle(const Value: TGradualStyle);
    procedure SetStartColor(const Value: TColor);
    procedure SetEndColor(const Value: TColor);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* 类构造器，参数为通知事件}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  published
    property Style: TGradualStyle read FStyle write SetStyle default gsLeftToRight;
    {* 渐变显示风格}
    property StartColor: TColor read FStartColor write SetStartColor
      default clWhite;
    {* 渐变起始色}
    property EndColor: TColor read FEndColor write SetEndColor default clBlack;
    {* 渐变结束色}
  end;

{ TCnAATexture }

  TCnAATexture = class(TCnEnabledClass)
  {* 平滑特效字体纹理参数类，一般不需要用户直接创建}
  private
    FPicture: TPicture;
    FMode: TTextureMode;
    procedure SetMode(const Value: TTextureMode);
    procedure SetPicture(const Value: TPicture);
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* 类构造器，参数为通知事件}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  published
    property Mode: TTextureMode read FMode write SetMode default tmTiled;
    {* 纹理图像显示模式}
    property Picture: TPicture read FPicture write SetPicture;
    {* 纹理图像}
  end;

{ TCnAAEffect }

  TCnAAEffect = class(TCnNotifyClass)
  {* 平滑特效字体参数类，一般不需要用户直接创建}
  private
    FAlpha: TAlpha;
    FBlur: TBlurStrength;
    FGradual: TCnAAGradual;
    FShadow: TCnAAShadow;
    FTexture: TCnAATexture;
    FOutline: Boolean;
    FNoise: Byte;
    FSpray: TSprayRange;
    FAngle: TAngle;
    FHorzMirror: Boolean;
    FVertMirror: Boolean;
    procedure SetAlpha(const Value: TAlpha);
    procedure SetBlur(const Value: TBlurStrength);
    procedure SetGradual(const Value: TCnAAGradual);
    procedure SetShadow(const Value: TCnAAShadow);
    procedure SetTexture(const Value: TCnAATexture);
    procedure SetOutline(const Value: Boolean);
    procedure SetNoise(const Value: Byte);
    procedure SetSpray(const Value: TSprayRange);
    procedure SetAngle(const Value: TAngle);
    procedure SetHorzMirror(const Value: Boolean);
    procedure SetVertMirror(const Value: Boolean);
  public
    constructor Create(ChangedProc: TNotifyEvent); override;
    {* 类构造器，参数为通知事件}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromIni(Ini: TCustomIniFile; const Section: string); virtual;
    procedure SaveToIni(Ini: TCustomIniFile; const Section: string); virtual;
    {* 对象赋值方法}
  published
    property Shadow: TCnAAShadow read FShadow write SetShadow;
    {* 字体阴影参数}
    property Gradual: TCnAAGradual read FGradual write SetGradual;
    {* 渐变显示参数}
    property Texture: TCnAATexture read FTexture write SetTexture;
    {* 字体纹理参数}
    property Alpha: TAlpha read FAlpha write SetAlpha default 100;
    {* 不透明度，参数范围为 0..100，0 为完全透明，100 为不透明}
    property Blur: TBlurStrength read FBlur write SetBlur default 0;
    {* 模糊度，参数范围为 0..100，0 为不模糊}
    property Angle: TAngle read FAngle write SetAngle default 0;
    {* 字体旋转角度，参数范围为 -360..360，单位为度}
    property Noise: Byte read FNoise write SetNoise default 0;
    {* 噪声效果，参数范围为 0..255}
    property Spray: TSprayRange read FSpray write SetSpray default 0;
    {* 喷溅效果，参数范围为 0..100}
    property Outline: Boolean read FOutline write SetOutline default False;
    {* 是否显示轮廓效果}
    property HorzMirror: Boolean read FHorzMirror write SetHorzMirror default False;
    {* 是否水平镜象}
    property VertMirror: Boolean read FVertMirror write SetVertMirror default False;
    {* 是否垂直镜象}
  end;

{ TCnAAMask }

  EInvalidPixel = class(Exception);
  EInvalidLine = class(Exception);

  PByteArray = ^TByteArray;
  {* 字节数组指针}
  TByteArray = array[0..32767] of Byte;
  {* 字节数组类型}

  PRGBArray = ^TRGBArray;
  {* RGB 数组指针}
  TRGBArray = array[0..8192] of tagRGBTriple;
  {* RGB 数组类型}

  PBGRAArray = ^TBGRAArray;
  {* BGRA 数组指针}
  TBGRAArray = array[0..8192] of tagRGBQUAD;
  {* BGRA 数组类型}

  TCnAAMask = class(TPersistent)
  {* 平滑特效字体蒙板处理类，供绘制平滑字体时内部使用}
  private
    FQuality: TAAQuality;
    FPMaskBuff: PByteArray; // 存储画出的经过平滑处理的 8bit 灰度内容
    FHeight: Integer;
    FWidth: Integer;
    BytesLineGray: Integer;
    BytesLineMask: Integer;
    Scale: Integer;
    FAAFont: TCnAAFont;
    FGrayBmp: TBitmap;
    procedure InitGrayBmp;
    procedure FreeGrayBmp;
    procedure SetQuality(const Value: TAAQuality);
    function TextExtentEx(const S: string; var Point: TPoint): TSize;
  protected
    function ScanLine(Line: Integer; pAData: PByteArray): PByteArray; overload;
    property pMaskBuff: PByteArray read FPMaskBuff;
  public
    constructor Create(AOwner: TCnAAFont);
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
    procedure DrawMask(const Text: string);
    {* 绘制文本蒙板图}
    procedure DrawMaskEx(const Text: string; Extend: TSize; Point: TPoint);
    {* 绘制文本蒙板图增强版}
    procedure Blur(Blur: TBlurStrength);
    {* 对蒙板图进行模糊处理}
    procedure Outline;
    {* 对蒙板图进行轮廓处理}
    procedure Spray(Amount: Integer);
    {* 对蒙板图进行喷溅处理}
    procedure HorzMirror;
    {* 对蒙板图进行水平镜象处理}
    procedure VertMirror;
    {* 对蒙板图进行垂直镜象处理}
    function TextExtent(const S: string): TSize;
    {* 返回文本高、宽}
    function TextHeight(const S: string): Integer;
    {* 返回文本高度}
    function TextWidth(const S: string): Integer;
    {* 返回文本宽度}
    function ScanLine(Line: Integer): Pointer; overload;
    {* 返回蒙板图扫描线地址}
    function Pixel(X, Y: Integer): Byte;
    {* 返回蒙板图指定象素灰度值}
    function PixelAddr(X, Y: Integer): Pointer;
    {* 返回蒙板图指定象素地址}
    property Height: Integer read FHeight;
    {* 蒙板图的高度}
    property Width: Integer read FWidth;
    {* 蒙板图的宽度}
    property Quality: TAAQuality read FQuality write SetQuality;
    {* 平滑字体绘制精度}
  end;

{ TCnAABlend }

  EInvalidForeBmp = class(Exception);

  TCnAABlend = class(TPersistent)
  {* 平滑特效字体图像混合处理类，供绘制平滑字体时内部使用}
  private
    FForeBmp: TBitmap;
    FRGBBmp: TBitmap;
    FForeBmp32: TBitmap;
    FBGRABmp: TBitmap;
    FAAFont: TCnAAFont;
    procedure SetForeBmp(const Value: TBitmap);
    procedure SetForeBmp32(const Value: TBitmap);
  public
    constructor Create(AOwner: TCnAAFont);
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
    procedure SyncForeBmp32;
    {* 把 24 位前景图内容塞给 32 位前景图}
    procedure Blend(X, Y: Integer; AColor: TColor; Alpha: TAlpha;
      Mask: TCnAAMask; DestIsAlpha: Boolean = False);
    {* 按指定颜色进行混合，当目标需要支持 Alpha 透明度时需指定 DestIsAlpha 为 True
      注意 DestIsAlpha 为 True 时，要求 AAFont.Canvas 中的源是 32 位
        并且在不支持 AlphaFormat 的情况下，要求内容是 UnPreMultiply 的（内部进行 PreMultiply）
      输出则是 PreMultiply 过的内容}
    procedure BlendEx(X, Y: Integer; Alpha: TAlpha; Mask: TCnAAMask;
      DestIsAlpha: Boolean = False);
    {* 使用前景图进行混合，DestIsAlpha 为 True 时使用 ForeBmp32，否则使用 ForeBmp
      注意 DestIsAlpha 为 True 时，要求 ForeBmp32 在不支持 AlphaFormat 的情况下
        要求内容是 PreMultiply 的（内部进行 UnPreMultiply）
      输出则是 PreMultiply 过的内容 }
    property ForeBmp: TBitmap read FForeBmp write SetForeBmp;
    {* 字体前景图，24 位 RGB 色彩的}
    property ForeBmp32: TBitmap read FForeBmp32 write SetForeBmp32;
    {* 字体前景图，32 位 Alpha 通道的，内容是 UnPreMultiply 的}
  end;

{ TCnAAFont }

  TCnAAFont = class
  {* 平滑特效字体绘制类，封装了基本的平滑字体绘制方法，用户可手动使用。
   |<BR>如果需要在其它地方绘制平滑字体，可使用下面的方法：
   !var
   !  AAFont: TCnAAFont;
   !  W, H: Integer;
   !  S: string;
   !begin
   !  // 创建 TCnAAFont 实例，并指定它使用 PaintBox1 的画布进行绘制
   !  AAFont := TCnAAFont.Create(PaintBox1.Canvas);
   !  try
   !    with PaintBox1.Canvas do
   !    begin
   !      Font.Name := '宋体'; // 设置字体
   !      Font.Size := 24;
   !      Font.Color := clBlue;
   !      Brush.Style := bsClear; // 设置透明绘制
   !    end;
   !    S := '这是一个使用平滑字体的例子';
   !    W := AAFont.TextWidth(S);
   !    H := AAFont.TextHeight(S);
   !    with PaintBox1 do // 在控件中心输出文本
   !      AAFont.TextOutput((Width - W) div 2, (Height - H) div 2, S, 80, 0);
   !    AAFont.Canvas := Image1.Canvas; // 也可以切换到另一画布
   !    AAFont.TextOut(10, 10, S); // 绘制时将使用Image1.Canvas的字体属性
   !  finally
   !    AAFont.Free;
   !  end;
   !end;}
  private
    FCanvas: TCanvas;
    function GetQuality: TAAQuality;
    procedure SetQuality(const Value: TAAQuality);
  protected
    Mask: TCnAAMask;
    Blend: TCnAABlend;
  public
    constructor Create(ACanvas: TCanvas); virtual;
    {* 类构造器，参数为绘制平滑字体文本和计算文本大小时使用的画布。
     |<BR> 允许为 nil，如果为 nil，请在调用文本方法前对 Canvas 属性赋值}
    destructor Destroy; override;
    {* 类析构器}
    procedure TextOutput(X, Y: Integer; const S: string; Alpha: TAlpha = 100;
      Blur: TBlurStrength = 0; DestIsAlpha: Boolean = False);
    {* 输出平滑字体文本到当前设置的 Canvas 中，使用它的字体属性和画刷设置。
     |<BR> 如果要输出背景透明的文本，需要将 Canvas.Brush.Style 设为 bsClear。
     |<BR> 注：该方法不支持多行文本。
     |<PRE>
       X, Y: Integer    - 文本输出位置
       S: string        - 要绘制的字符串
       Alpha: TAlpha    - 文本的不透明度，0~100，默认为完全不透明 100
       Blur: TBlurStrength  - 文本的模糊度，默认为不进行模糊处理
       DestIsAlpha: Boolean - 当目标需要支持 Alpha 透明度时需指定 DestIsAlpha 为 True
     |</PRE>}
    function TextExtent(const S: string): TSize; virtual;
    {* 返回文本高、宽}
    function TextHeight(const S: string): Integer; virtual;
    {* 返回指定文本的显示高度，使用当前的 Canvas 属性}
    function TextWidth(const S: string): Integer; virtual;
    {* 返回指定文本的显示宽度，使用当前的Canvas属性}
    property Quality: TAAQuality read GetQuality write SetQuality;
    {* 平滑字体绘制精度}
    property Canvas: TCanvas read FCanvas write FCanvas;
    {* 用于平滑字体绘制输出和文本尺寸检测的画布}
  end;

{ TCnAAFontEx }

  TCnAAFontEx = class(TCnAAFont)
  {* 扩展的平滑特效字体绘制类，实现了阴影、渐变、纹理等特效。
   |<BR> 用户可手动创建 TCnAAFontEx 对象来绘制带特效的平滑字体文本，
   使用方法类似于 TCnAAFont。}
  private
    FEffect: TCnAAEffect;
    procedure SetEffect(const Value: TCnAAEffect);
  protected
    function GetShadowPoint: TPoint;
    function GetTextPoint: TPoint;
    procedure CreateGradual;
    procedure DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
    procedure CreateForeBmp;
    procedure CreateNoiseBmp;
    procedure AddNoise(Amount: Byte);
  public
    constructor Create(ACanvas: TCanvas); override;
    {* 类构造器，参数为绘制平滑字体文本和计算文本大小时使用的画布。
     |<BR> 允许为 nil，如果为 nil，请在调用文本方法前对 Canvas 属性赋值}
    destructor Destroy; override;
    {* 类析构器}
    function TextExtent(const S: string): TSize; override;
    {* 返回文本高、宽
     |<BR> 注：Effect 参数中的阴影、旋转角度等设置将影响返回结果}
    procedure TextOutput(X, Y: Integer; const S: string; DestIsAlpha: Boolean = False);
    {* 使用 Effect 设置的字体特效，输出平滑字体文本到当前设置的 Canvas 中，使用它的字体属性和画刷设置。
     |<BR> 如果要输出背景透明的文本，需要将 Canvas.Brush.Style 设为 bsClear。
     |<BR> 注：该方法不支持多行文本。
     |<PRE>
       X, Y: Integer    - 文本输出位置
       S: string        - 要绘制的字符串
       DestIsAlpha: Boolean - 当目标需要支持 Alpha 透明度时需指定 DestIsAlpha 为 True
     |</PRE>}
    property Effect: TCnAAEffect read FEffect write SetEffect;
    {* 平滑字体绘制时的特效参数}
  end;

const
  csMaxProgress = 255;

type
  TCnFontLabel = class;
  TCnFontLabels = class;
  TCnUserLabel = class;
  TCnUserLabels = class;
  TCnAAGraphicControl = class;

{ TCnFontLabel }

  TCnFontLabel = class(TCollectionItem)
  {* 字体标签列表项类，TCnFontLabels 的子项，一般不需要用户直接创建}
  private
    FName: string;
    FFont: TFont;
    FEffect: TCnAAEffect;
    function GetFontLabels: TCnFontLabels;
    procedure Changed;
    procedure SetFont(const Value: TFont);
    procedure SetName(const Value: string);
    procedure OnEffectChanged(Sender: TObject);
    procedure SetEffect(const Value: TCnAAEffect);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
    property FontLabels: TCnFontLabels read GetFontLabels;
    {* 所有者}
  published
    property Name: string read FName write SetName;
    {* 字体标签名}
    property Font: TFont read FFont write SetFont;
    {* 字体属性}
    property Effect: TCnAAEffect read FEffect write SetEffect;
    {* 平滑字体特效显示属性}
  end;

{ TCnFontLabels }

  TCnFontLabels = class(TOwnedCollection)
  {* 字体标签列表类，定义了在文本类控件中可使用的字体标签集，一般不需要用户直接创建}
  private
    FOnChanged: TNotifyEvent;
    function GetItem(Index: Integer): TCnFontLabel;
    procedure SetItem(Index: Integer; const Value: TCnFontLabel);
  protected
    procedure Changed;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    {* 类构造器}
    function AddItem(AName: string; AFontName: string; AFontSize: Integer;
      AFontColor: TColor; AFontEffect: TFontStyles; Shadow: Boolean;
      OffsetX, OffsetY: Integer): TCnFontLabel;
    {* 增加一个新的字体标签}
    function IndexOf(const Name: string): Integer;
    {* 根据标签名查找子项索引号}
    procedure Check(var AText: string; AFont: TFont; AEffect: TCnAAEffect);
    {* 检查可能带字体标签的字符串。
     |<BR> 如果找到相应的标签，删除字符串中的标签（包含<>标记），并用该标签
       定义的 Font 和 Effect 属性设置参数中的对应属性}
    property Items[Index: Integer]: TCnFontLabel read GetItem write SetItem; default;
    {* 字体标签项数组属性}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    {* 属性变更通知}
  end;

{ TCnUserLabel }

  TGetTextEvent = procedure(Sender: TCnUserLabel; var Text: string) of object;
  {* 取用户标签所对应的文本事件
   |<PRE>
     Sender: TCnUserLabel  - 产生该事件的对象
     Text: string        - 由用户返回该标签对应的文本，变量参数
   |</PRE>}
  TLabelStyle = (lsLeftJustify, lsCenter, lsRightJustify, lsRegOwner,
    lsRegOrganization, lsAppTitle, lsDate, lsTime, lsCustom);
  {* 用户标签类型，分对齐控制标签和文本标签，文本标签在运行时被指定的文本取代：
   |<PRE>
     lsLeftJustify      - 左对齐标签，不可见控制标签，决定文本对齐方式
     lsCenter           - 中心对齐标签，不可见控制标签，决定文本对齐方式
     lsRightJustify     - 右对齐标签，不可见控制标签，决定文本对齐方式
     lsRegOwner         - 注册表中用户名标签，系统定义标签
     lsRegOrganization  - 注册表中组织名标签，系统定义标签（NT下无效）
     lsAppTitle         - 应用程序标题标签，系统定义标签
     lsDate             - 当前日期标签，系统定义标签
     lsTime             - 当前时间标签，系统定义标签
     lsCustom           - 用户自定义标签类型
   |</PRE>}

  TCnUserLabel = class(TCollectionItem)
  {* 用户文本标签列表项类，TCnUserLabels 的子项，一般不需要用户直接创建}
  private
    FName: string;
    FText: string;
    FOnGetText: TGetTextEvent;
    FStyle: TLabelStyle;
    function GetUserLabels: TCnUserLabels;
    procedure Changed;
    procedure SetName(const Value: string);
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetStyle(const Value: TLabelStyle);
    function IsTextStored: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    {* 类构造器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
    property UserLabels: TCnUserLabels read GetUserLabels;
    {* 所有者}
  published
    property Name: string read FName write SetName;
    {* 用户标签名}
    property Text: string read GetText write SetText stored IsTextStored;
    {* 标签所对应的文本，运行期显示文本中的标签用该值代替}
    property Style: TLabelStyle read FStyle write SetStyle default lsCustom;
    {* 标签类型}
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
    {* 取用户标签所对应的文本事件，对系统标签也有效}
  end;

{ TCnUserLabels }

  TCnUserLabels = class(TOwnedCollection)
  {* 用户标签列表类，定义了在文本类控件中可使用的用户标签集，一般不需要用户直接创建}
  private
    RegOwner: string;
    RegOrganization: string;
    FOnChanged: TNotifyEvent;
    function GetItem(Index: Integer): TCnUserLabel;
    procedure SetItem(Index: Integer; const Value: TCnUserLabel);
    procedure InitRegInfo;
  protected
    procedure Changed;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    {* 类构造器}
    function AddItem(AName: string; AText: string; AStyle: TLabelStyle): TCnUserLabel;
    {* 增加一个新的用户标签}
    function IndexOf(const Name: string): Integer;
    {* 根据标签名查找子项索引}
    procedure Check(var AText: string; var Align: TAlignment);
    {* 检查可能带文本标签的字符串。
     |<BR> 如果找到相应的标签，将文本中的标签（包含<>符号）用标签的 Text 属性取代，
       同时，还将调用标签的 OnGetText 事件。如果是对齐标签，将设置参数中的 Align 属性。}
    property Items[Index: Integer]: TCnUserLabel read GetItem write SetItem; default;
    {* 用户标签项数组属性}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    {* 属性变更通知}
  end;

{ TCnPackParam }

  TCnPackParam = class(TPersistent)
  {* 打包的参数属性类}
  private
    FOwner: TControl;
  protected
    property Owner: TControl read FOwner;
  public
    constructor Create(AOwner: TControl); virtual;
    {* 类构造器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  end;

{ TCnDrag }

  TCnDrag = class(TCnPackParam)
  {* 打包的拖放相关属性类}
  private
    function GetDragCursor: TCursor;
    function GetDragKind: TDragKind;
    function GetDragMode: TDragMode;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetDragKind(const Value: TDragKind);
    procedure SetDragMode(const Value: TDragMode);
  published
    property DragKind: TDragKind read GetDragKind write SetDragKind default dkDrag;
    {* 拖放类型，同 TControl 中定义}
    property DragCursor: TCursor read GetDragCursor write SetDragCursor default crDrag;
    {* 拖放光标，同 TControl 中定义}
    property DragMode: TDragMode read GetDragMode write SetDragMode default dmManual;
    {* 拖放模式，同 TControl 中定义}
  end;

{ TCnParentEffect }

  TCnParentEffect = class(TCnPackParam)
  {* 打包的父控件影响相关属性类}
  private
    function GetParentBiDiMode: Boolean;
    function GetParentColor: Boolean;
    function GetParentFont: Boolean;
    function GetParentShowHint: Boolean;
    procedure SetParentBiDiMode(const Value: Boolean);
    procedure SetParentColor(const Value: Boolean);
    procedure SetParentFont(const Value: Boolean);
    procedure SetParentShowHint(const Value: Boolean);
  protected
    property ParentBiDiMode: Boolean read GetParentBiDiMode write SetParentBiDiMode
      default True;
  published
    property ParentColor: Boolean read GetParentColor write SetParentColor default
      True;
    {* 使用父控件的颜色，同 TControl 中定义}
    property ParentFont: Boolean read GetParentFont write SetParentFont default True;
    {* 使用父控件的字体，同 TControl 中定义}
    property ParentShowHint: Boolean read GetParentShowHint write SetParentShowHint
      default True;
    {* 使用父控件的提示显示设置，同 TControl 中定义}
  end;

{ TCnCustomParam }

  TBackGroundMode = (bmTiled, bmStretched, bmCenter, bmNormal);
  {* 背景图像显示模式
   |<PRE>
     bmTiled            - 平铺显示
     bmStretched        - 自动缩放显示
     bmCenter           - 在中心位置显示
     bmNormal           - 在左上角显示
   |</PRE>}

  TCnCustomParam = class(TCnNotifyClass)
  {* 可定制的平滑字体控件参数基类，一般不需要用户直接创建}
  private
    FAlignment: TAlignment;
    FWordWrap: Boolean;
    FTransparent: Boolean;
    FLayout: TTextLayout;
    FOwner: TCnAAGraphicControl;
    FBackGround: TPicture;
    FBackGroundMode: TBackGroundMode;

    procedure BackGroundChanged(Sender: TObject);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetQuality(const Value: TAAQuality);
    procedure SetFontEffect(const Value: TCnAAEffect);
    function GetQuality: TAAQuality;
    function GetFontEffect: TCnAAEffect;
    function GetColor: TColor;
    function GetFont: TFont;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetBackGround(const Value: TPicture);
    procedure SetBackGroundMode(const Value: TBackGroundMode);
    function IsColorStroed: Boolean;
  protected
    function IsBackEmpty: Boolean;
    property Owner: TCnAAGraphicControl read FOwner;
    property Font: TFont read GetFont write SetFont;
    {* 控件字体}
    property Quality: TAAQuality read GetQuality write SetQuality default aqNormal;
    {* 平滑字体显示精度}
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    {* 文本对齐方式}
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    {* 文本垂直方向对齐方式}
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    {* 文本是否自动换行}
    property Transparent: Boolean read FTransparent write SetTransparent
      default False;
    {* 控件是否透明}
    property FontEffect: TCnAAEffect read GetFontEffect write SetFontEffect;
    {* 平滑特效字体属性}
    property BackGround: TPicture read FBackGround write SetBackGround;
    {* 控件背景图像}
    property BackGroundMode: TBackGroundMode read FBackGroundMode
      write SetBackGroundMode default bmCenter;
    {* 控件背景图像显示模式}
    property BackColor: TColor read GetColor write SetColor stored IsColorStroed;
    {* 控件背景颜色}
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc: TNotifyEvent);
      reintroduce; virtual;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  end;

{ TCnCustomTextParam }

  TLabelEffect = (leOnlyALine, leUntilNextLabel);
  {* 标签作用范围类型
   |<PRE>
     leOnlyALine        - 字体、控制标签仅在当前行有效，无标签的行字体、对齐等由默认值决定
     leUntilNextLabel   - 标签所对应的参数影响当前行和后续行，直到遇到下一标签为止
   |</PRE>}
  TRowPitch = -100..150;
  {* 行间隔类型，单位为字体高度的百分比，允许为负以产生特殊效果}

  TCnCustomTextParam = class(TCnCustomParam)
  {* 可定制的平滑字体文本类控件参数基类，一般不需要用户直接创建}
  private
    FLines: TStrings;
    FLabelEffect: TLabelEffect;
    FRowPitch: TRowPitch;
    FFontEffect: TCnAAEffect;

    procedure LinesChanged(Sender: TObject);
    procedure SetLines(const Value: TStrings);
    procedure SetLabelEffect(const Value: TLabelEffect);
    procedure SetRowPitch(const Value: TRowPitch);
    procedure SetFontEffect(const Value: TCnAAEffect);
  protected
    function IsLinesStored: Boolean; virtual;
    property Lines: TStrings read FLines write SetLines stored IsLinesStored;
    {* 控件文本内容}
    property RowPitch: TRowPitch read FRowPitch write SetRowPitch default 20;
    {* 文本行间距}
    property LabelEffect: TLabelEffect read FLabelEffect write SetLabelEffect
      default leUntilNextLabel;
    {* 文本中字体、控制标签的作用范围}
    property FontEffect: TCnAAEffect read FFontEffect write SetFontEffect;
    {* 平滑特效字体属性}
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc: TNotifyEvent);
      override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  end;

{ TCnAAGraphicControl }

  TProgress = 0..csMaxProgress;

  TCnAAGraphicControl = class(TGraphicControl)
  {* 平滑字体控件基类，所有平滑字体控件由该基类派生而来，一般不需要用户直接创建
   |<BR> 如果用户需要编写自己的平滑字体控件，可仔细分析该基类源码}
  private
{$IFNDEF COMPILER6_UP}
    FAutoSize: Boolean;
{$ENDIF}
    FAAFont: TCnAAFontEx;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAutoUpdate: Boolean;
    FDrag: TCnDrag;
    FParentEffect: TCnParentEffect;
    FUpdateCount: Integer;
    FBorder: TBorderWidth;
    Inited: Boolean;
    Drawing: Boolean;
    AHeight: Integer;
    AWidth: Integer;

    procedure SetBorder(const Value: TBorderWidth);
    procedure SetDrag(const Value: TCnDrag);
    procedure SetParentEffect(const Value: TCnParentEffect);
    function GetWrapText(const Line, BreakStr: string;
      BreakChars: TSysCharSet; MaxCol: Integer): string;
    procedure SetAutoUpdate(const Value: Boolean);
  protected
{$IFDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); override;
{$ELSE}
    procedure SetAutoSize(const Value: Boolean); virtual;
{$ENDIF}
    procedure OnEffectChanged(Sender: TObject);
    procedure CopyParentImage(Dest: TCanvas);
    procedure WrapText(const S: string; Strs: TStrings; Col: Integer);
    procedure Blend(DesBmp, BkBmp, ForeBmp: TBitmap; AProgress: TProgress);
    procedure DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
    procedure DrawBackGround(Canvas: TCanvas; Rect: TRect; G: TGraphic;
      Mode: TBackGroundMode);
    procedure WndProc(var message: TMessage); override;
    procedure PaintCanvas; virtual;
    procedure Paint; override;
    procedure Loaded; override;
    procedure LoadedEx; virtual;
    procedure Reset; virtual;
    procedure Resize; override;
    property UpdateCount: Integer read FUpdateCount;
    property AAFont: TCnAAFontEx read FAAFont;
{$IFDEF COMPILER6_UP}
    property AutoSize default True;
{$ELSE}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
{$ENDIF}
    property AutoUpdate: Boolean read FAutoUpdate write SetAutoUpdate default True;
    property Border: TBorderWidth read FBorder write SetBorder default 0;
    {* 控件边界保留宽度}
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    property Canvas;
    {* 控件画布}
    procedure BeginUpdate;
    {* 开始更新，调用该方法后，对控件属性的更改不会导致控件重绘，建议在批量修改
       控件时使用。
     |<BR> 注意该方法必须与 EndUpate 成对使用。}
    procedure EndUpdate;
    {* 结束更新，见 BeginUpdate。用户结束更新后通常还应调用 Changed 方法通知控件重绘。}
    procedure Changed;
    {* 通知控件属性已变更，要求控件重绘}
  published
    property Drag: TCnDrag read FDrag write SetDrag;
    {* 拖动相关属性打包}
    property ParentEffect: TCnParentEffect read FParentEffect write SetParentEffect;
    {* 父控件影响相关属性打包}
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ShowHint;
    property Hint;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {* 鼠标移入控件内部事件}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    {* 鼠标移出控件内部事件}
    property OnStartDock;
    property OnStartDrag;
  end;

{ TCnAACustomText }

  TCnAACustomText = class(TCnAAGraphicControl)
  {* 平滑字体文本类控件基类，所有使用多行文本的平滑字体控件由该基类派生而来，
     一般不需要用户直接创建。
   |<BR> 如果用户需要编写自己的平滑字体控件，可分析该基类源码}
  private
    FFonts: TCnFontLabels;
    FOnTextReady: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    FOnPainted: TNotifyEvent;
    FLabels: TCnUserLabels;
    FLabelsInited: Boolean;
    FFontsInited: Boolean;
    procedure SetFonts(const Value: TCnFontLabels);
    procedure SetLabels(const Value: TCnUserLabels);
  protected
    procedure CreateDefLabels; virtual;
    procedure CreateDefFonts; virtual;
    procedure CreateDefault;
    function UseDefaultLabels: Boolean; virtual;
    procedure LoadedEx; override;
    procedure OnLabelChanged(Sender: TObject);
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnTextReady: TNotifyEvent read FOnTextReady write FOnTextReady;
    property OnPainted: TNotifyEvent read FOnPainted write FOnPainted;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
  published
    property Fonts: TCnFontLabels read FFonts write SetFonts;
    {* 字体标签属性}
    property Labels: TCnUserLabels read FLabels write SetLabels;
    {* 用户标签属性}
  end;

var
  HSLRange: Integer = 240;
  {* HSL整数型颜色的范围值}

// HSL颜色与RGB色转换函数
function HSLtoRGB(H, S, L: Double): TColor;
{* HSL颜色转换为RGB颜色
 |<PRE>
   H, S, L: Double      - 分别为色调、饱和度、亮度分量，为"0"到"1"之间的小数
   Result: TColor       - 返回 RGB 颜色值
 |</PRE>}
function HSLRangeToRGB(H, S, L: Integer): TColor;
{* HSL颜色转换为RGB颜色
 |<PRE>
   H, S, L: Integer     - 分别为色调、饱和度、亮度分量，0..240
   Result: TColor       - 返回RGB颜色值
 |</PRE>}
procedure RGBtoHSL(RGB: TColor; var H, S, L: Double);
{* RGB颜色转换为HSL颜色
 |<PRE>
   Color: TColor        - RGB颜色值
   H, S, L: Integer     - 输出分别为色调、饱和度、亮度分量，为"0"到"1"之间的小数
 |</PRE>}
procedure RGBtoHSLRange(RGB: TColor; var H, S, L: Integer);
{* RGB颜色转换为HSL颜色
 |<PRE>
   Color: TColor        - RGB颜色值
   H, S, L: Integer     - 输出分别为色调、饱和度、亮度分量，0..240
 |</PRE>}

implementation

{$R-}
{$OVERFLOWCHECKS OFF}

const
  ItalicAdjust = 0.3;                   // 斜体字宽度校正系数
{$IFNDEF COMPILER6_UP}
  AC_SRC_ALPHA = $01;
{$ENDIF}

resourcestring
  SInvalidForeground = 'Invalid foreground bitmap!';
  SDuplicateString = 'Duplicate string!';
//  SAlreadyMultiplied = 'Already Multiplied!';
//  SAlreadyUnMultiplied = 'Already UnMultiplied!';

type
  PDWordArray = ^TDWordArray;
  TDWordArray = array[0..8192] of DWORD;
  {* DWORD 数组类型}

  TCnParentControl = class(TWinControl);
  TCnMyControl = class(TControl);

function HSLtoRGB(H, S, L: Double): TColor;
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
  R, G, B: Byte;
begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColourValue(H + 1 / 3);
    G := HueToColourValue(H);
    B := HueToColourValue(H - 1 / 3)
  end;
  Result := RGB(R, G, B)
end;

function HSLRangeToRGB(H, S, L: Integer): TColor;
begin
  Result := HSLtoRGB(H / (HSLRange - 1), S / HSLRange, L / HSLRange)
end;

procedure RGBtoHSL(RGB: TColor; var H, S, L: Double);
  function Max(a, b: Double): Double;
  begin
    if a > b then
      Result := a
    else
      Result := b
  end;
  function Min(a, b: Double): Double;
  begin
    if a < b then
      Result := a
    else
      Result := b
  end;
var
  R, G, B, D, Cmax, Cmin: Double;
begin
  R := GetRValue(RGB) / 255;
  G := GetGValue(RGB) / 255;
  B := GetBValue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;
  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else if G = Cmax then
      H := 2 + (B - R) / D
    else
      H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

procedure RGBtoHSLRange(RGB: TColor; var H, S, L: Integer);
var
  Hd, Sd, Ld: Double;
begin
  RGBtoHSL(RGB, Hd, Sd, Ld);
  H := Round(Hd * (HSLRange - 1));
  S := Round(Sd * HSLRange);
  L := Round(Ld * HSLRange);
end;

procedure StrectchDrawGraphic(ACanvas: TCanvas; ARect: TRect; AGraphic: TGraphic;
  BkColor: TColor);
var
  Bmp: TBitmap;
begin
  if AGraphic is TIcon then
  begin
    //  TIcon 不支持缩放绘制，通过 TBitmap 中转
    Bmp := TBitmap.Create;
    try
      Bmp.Canvas.Brush.Color := BkColor;
      Bmp.Canvas.Brush.Style := bsSolid;
      Bmp.Width := AGraphic.Width;
      Bmp.Height := AGraphic.Height;
      // Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
      Bmp.Canvas.Draw(0, 0, AGraphic);
      ACanvas.StretchDraw(ARect, Bmp);
    finally
      Bmp.Free;
    end;
  end
  else
    ACanvas.StretchDraw(ARect, AGraphic);
end;

procedure PreMultiplyBitmap(Bmp: TBitmap);
{$IFNDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
var
  W, H: Integer;
  Alpha: Word;
  PBGRA: PBGRAArray;
{$ENDIF}
begin
  if (Bmp = nil) or (Bmp.PixelFormat <> pf32bit) then
    Exit;

{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  if Bmp.AlphaFormat <> afPremultiplied then
    Bmp.AlphaFormat := afPremultiplied;
//  else
//    raise Exception.Create(SAlreadyMultiplied);
{$ELSE}
  // 手工 PreMultiply
  for H := 0 to Bmp.Height - 1 do
  begin
    PBGRA := Bmp.ScanLine[H];
    for W := 0 to Bmp.Width - 1 do
    begin
      Alpha := PBGRA[W].rgbReserved;
      PBGRA[W].rgbBlue := MulDiv(PBGRA[W].rgbBlue, Alpha, 255);
      PBGRA[W].rgbGreen := MulDiv(PBGRA[W].rgbGreen, Alpha, 255);
      PBGRA[W].rgbRed := MulDiv(PBGRA[W].rgbRed, Alpha, 255);
    end;
  end;
{$ENDIF}
end;

procedure UnPreMultiplyBitmap(Bmp: TBitmap);
{$IFNDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
var
  W, H: Integer;
  Alpha: Word;
  PBGRA: PBGRAArray;
{$ENDIF}
begin
  if (Bmp = nil) or (Bmp.PixelFormat <> pf32bit) then
    Exit;

{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  if Bmp.AlphaFormat = afPremultiplied then
    Bmp.AlphaFormat := afIgnored;
//  else
//    raise Exception.Create(SAlreadyUnMultiplied);
{$ELSE}
  // 手工 UnPreMultiply
  for H := 0 to Bmp.Height - 1 do
  begin
    PBGRA := Bmp.ScanLine[H];
    for W := 0 to Bmp.Width - 1 do
    begin
      Alpha := PBGRA[W].rgbReserved;
      PBGRA[W].rgbBlue := MulDiv(PBGRA[W].rgbBlue, 255, Alpha);
      PBGRA[W].rgbGreen := MulDiv(PBGRA[W].rgbGreen, 255, Alpha);
      PBGRA[W].rgbRed := MulDiv(PBGRA[W].rgbRed, 255, Alpha);
    end;
  end;
{$ENDIF}
end;

type
  TLogPal = record
    lpal: TLogPalette;
    dummy: array[0..255] of TPaletteEntry;
  end;

var
  HGrayPal: HPALETTE = 0;
  LogPal: TLogPal;

// 初始化灰度位图
procedure InitGrayPal;
var
  I: Integer;
begin
  LogPal.lpal.palVersion := $300;
  LogPal.lpal.palNumEntries := 256;
  for I := 0 to 255 do
  begin
    LogPal.dummy[I].peRed := I;
    LogPal.dummy[I].peGreen := I;
    LogPal.dummy[I].peBlue := I;
    LogPal.dummy[I].peFlags := 0;
  end;
  HGrayPal := CreatePalette(LogPal.lpal);
end;

{ TCnAAMask }

//--------------------------------------------------------//
// 平滑字体蒙板类                                         //
//--------------------------------------------------------//

// 赋值
procedure TCnAAMask.Assign(Source: TPersistent);
begin
  if Source is TCnAAMask then
  begin
    FWidth := TCnAAMask(Source).Width;
    FHeight := TCnAAMask(Source).Height;
    Quality := TCnAAMask(Source).Quality;
    BytesLineGray := TCnAAMask(Source).BytesLineGray;
    BytesLineMask := TCnAAMask(Source).BytesLineMask;
    ReAllocMem(FPMaskBuff, FHeight * BytesLineMask);
    CopyMemory(FPMaskBuff, TCnAAMask(Source).FPMaskBuff, FHeight * BytesLineMask);
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

// 初始化
constructor TCnAAMask.Create(AOwner: TCnAAFont);
begin
  FAAFont := AOwner;
  FPMaskBuff := nil;
  Quality := aqNormal;
end;

// 释放
destructor TCnAAMask.Destroy;
begin
  FreeGrayBmp;
  FreeMem(FPMaskBuff);
  inherited;
end;

procedure TCnAAMask.InitGrayBmp;
begin
  if FGrayBmp = nil then
  begin
    FGrayBmp := TBitmap.Create;
    FGrayBmp.PixelFormat := pf8bit;
    FGrayBmp.Canvas.Brush.Style := bsSolid;
    FGrayBmp.Canvas.Brush.Color := clBlack;
    FGrayBmp.Palette := CopyPalette(HGrayPal);
  end;
end;

procedure TCnAAMask.FreeGrayBmp;
var
  P: HPALETTE;
begin
  if FGrayBmp <> nil then
  begin
    P := FGrayBmp.Palette;
    FGrayBmp.Palette := 0;
    FreeAndNil(FGrayBmp);
    DeleteObject(P);
  end;
end;

// 绘制平滑字体蒙板
procedure TCnAAMask.DrawMaskEx(const Text: string; Extend: TSize; Point: TPoint);
var
  I, J: Integer;
  pS1, pS2, pS3, pS4: PByteArray;
  pDes: PByteArray;
  X, Y: Integer;
  P: TPoint;
  LogFont: TLogFont;
  Beta: Double;
  TextSize: TSize;
  R: TRect;
begin
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  InitGrayBmp;
  FWidth := Extend.cx;                  // 大小
  FHeight := Extend.cy;
  if FGrayBmp.Width < Width * Scale then // 放大
    FGrayBmp.Width := Width * Scale;
  if FGrayBmp.Height < Height * Scale then
    FGrayBmp.Height := Height * Scale;

  GetObject(FAAFont.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
  with LogFont do
  begin
    lfHeight := lfHeight * Scale;
    lfWidth := lfWidth * Scale;
    Beta := lfEscapement * Pi / 1800;
  end;

  FGrayBmp.Canvas.Font.Handle := CreateFontIndirect(LogFont);
  FGrayBmp.Canvas.Font.Color := clWhite;
  FillRect(FGrayBmp.Canvas.Handle, Bounds(0, 0, FGrayBmp.Width, FGrayBmp.Height), 0);
  X := Point.X * Scale;
  Y := Point.Y * Scale;
  if Beta <> 0 then      // 考虑字体旋转
  begin
    TextSize := TextExtentEx(Text, P);
    Inc(X, P.X * Scale);
    Inc(Y, P.Y * Scale);
  end;
  R := Bounds(0, 0, FGrayBmp.Width, FGrayBmp.Height);
  Windows.TextOut(FGrayBmp.Canvas.Handle, X, Y, PChar(Text), Length(Text));

  BytesLineGray := (FGrayBmp.Width + 3) div 4 * 4; // 扫描线宽度
  BytesLineMask := (Width + 3) div 4 * 4;
  ReAllocMem(FPMaskBuff, BytesLineMask * Height);

  pS1 := FGrayBmp.ScanLine[0];           // 源灰度图
  pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
  pS3 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
  pS4 := PByteArray(TCnNativeInt(pS3) - BytesLineGray);
  pDes := PByteArray(TCnNativeInt(pMaskBuff) + (Height - 1) * BytesLineMask);
  // 目标灰度为源矩形块的平均值

  case Quality of
    aqHigh:
      begin                             // 高精度 4X4 采样
        for I := 0 to Height - 1 do
        begin
          for J := 0 to Width - 1 do
          begin
            X := J * 4;
            pDes^[J] :=
              (pS1^[X] + pS1^[X + 1] + pS1^[X + 2] + pS1^[X + 3] +
              pS2^[X] + pS2^[X + 1] + pS2^[X + 2] + pS2^[X + 3] +
              pS3^[X] + pS3^[X + 1] + pS3^[X + 2] + pS3^[X + 3] +
              pS4^[X] + pS4^[X + 1] + pS4^[X + 2] + pS4^[X + 3]) shr 4;
          end;
          pS1 := PByteArray(TCnNativeInt(pS4) - BytesLineGray);
          pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pS3 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
          pS4 := PByteArray(TCnNativeInt(pS3) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
    aqNormal:
      begin                             // 普通精度 3X3 采样
        for I := 0 to Height - 1 do
        begin
          for J := 0 to Width - 1 do
          begin
            X := J * 3;
            pDes^[J] :=
              (pS1^[X] + pS1^[X + 1] + pS1^[X + 2] shr 1 +
              pS2^[X] + pS2^[X + 1] + pS2^[X + 2] +
              pS3^[X] shr 1 + pS3^[X + 1] + pS3^[X + 2]) shr 3;
          end;
          pS1 := PByteArray(TCnNativeInt(pS3) - BytesLineGray);
          pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pS3 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
    aqLow:
      begin                             // 低精度 2X2 采样
        for I := 0 to Height - 1 do
        begin
          for J := 0 to Width - 1 do
          begin
            X := J * 2;
            pDes^[J] :=
              (pS1^[X] + pS1^[X + 1] +
              pS2^[X] + pS2^[X + 1]) shr 2;
          end;
          pS1 := PByteArray(TCnNativeInt(pS2) - BytesLineGray);
          pS2 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
    aqNone:
      begin                             // 无平滑效果
        for I := 0 to Height - 1 do
        begin
          CopyMemory(pDes, pS1, Width);
          pS1 := PByteArray(TCnNativeInt(pS1) - BytesLineGray);
          pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
        end;
      end;
  end;
  FreeGrayBmp;
end;

// 绘制平滑字体
procedure TCnAAMask.DrawMask(const Text: string);
begin
  DrawMaskEx(Text, TextExtent(Text), Point(0, 0));
end;

// 边缘检测
procedure TCnAAMask.Outline;
var
  X, Y: Integer;
  s1, s2, s3, s4, Sum: Integer;
  pTempBuff: PByteArray;
  pDes: PByteArray;
  pUp, pMiddle, pDown: PByteArray;      // 卷积用指针
begin
  GetMem(pTempBuff, BytesLineMask * Height); // 临时缓冲区
  try
    CopyMemory(pTempBuff, pMaskBuff, BytesLineMask * Height);
    for Y := 1 to Height - 2 do
    begin
      pUp := ScanLine(Y - 1, pTempBuff);
      pMiddle := ScanLine(Y, pTempBuff);
      pDown := ScanLine(Y + 1, pTempBuff);
      pDes := ScanLine(Y);
      for X := 1 to Width - 2 do
      begin
        s1 := Abs(pDown^[X] - pUp^[X]);
        s2 := Abs(pMiddle^[X + 1] - pMiddle^[X - 1]);
        s3 := Abs(pDown^[X - 1] - pUp^[X + 1]);
        s4 := Abs(pDown^[X + 1] - pUp^[X - 1]);
        Sum := (s1 + s2 + s3 + s4) shr 2;
        if Sum > 255 then
          pDes^[X] := 255
        else
          pDes^[X] := Sum;
      end;
    end;
  finally
    FreeMem(pTempBuff);
  end;
end;

// 字体模糊
procedure TCnAAMask.Blur(Blur: TBlurStrength);
type
  TLine = array[0..4] of Integer;
const
  csLine: array[0..4] of TLine = (
    (0, 0, 0, 1, 2), (-1, -1, 0, 1, 2), (-2, -1, 0, 1, 2),
    (-2, -1, 0, 1, 1), (-2, -1, 0, 0, 0)); // 边界处理常量
var
  pTempBuff: PByteArray;
  pSour: array[0..4] of PByteArray;
  pDes: PByteArray;
  xLine: TLine;
  yLine: TLine;
  X, Y, I: Integer;
  Sum: Integer;
  ABlur: Byte;
begin
  GetMem(pTempBuff, BytesLineMask * Height); // 临时缓冲区
  try
    CopyMemory(pTempBuff, pMaskBuff, BytesLineMask * Height);
    ABlur := Round(Blur * 255 / 100);
    for Y := 0 to Height - 1 do         // 边界处理
    begin
      if Y = 0 then
        yLine := csLine[0]
      else if Y = 1 then
        yLine := csLine[1]
      else if Y = Height - 2 then
        yLine := csLine[3]
      else if Y = Height - 1 then
        yLine := csLine[4]
      else
        yLine := csLine[2];
      for I := 0 to 4 do
        pSour[I] := ScanLine(yLine[I] + Y, pTempBuff);
      pDes := ScanLine(Y, pMaskBuff);
      for X := 0 to Width - 1 do        // 边界处理
      begin
        if X = 0 then
          xLine := csLine[0]
        else if X = 1 then
          xLine := csLine[1]
        else if X = Width - 2 then
          xLine := csLine[3]
        else if X = Width - 1 then
          xLine := csLine[4]
        else
          xLine := csLine[2];
        Sum := 0;
        for I := 0 to 4 do              // 5X5均值处理
          Inc(Sum, pSour[I]^[X + xLine[0]] + pSour[I]^[X + xLine[1]] +
            pSour[I]^[X + xLine[2]] + pSour[I]^[X + xLine[3]] +
            pSour[I]^[X + xLine[3]]);
        if ABlur = 255 then             // 模糊度
          pDes^[X] := Round(Sum / 25)
        else
          pDes^[X] := (Round(Sum / 25) - pDes^[X]) * ABlur shr 8 + pDes^[X];
      end;
    end;
  finally
    FreeMem(pTempBuff);
  end;
end;

//  喷溅效果
procedure TCnAAMask.Spray(Amount: Integer);
var
  r, X, Y, ax, ay: Integer;
  pDes: PByteArray;
begin
  pDes := ScanLine(0);
  for Y := 0 to FHeight - 1 do
  begin
    for X := 0 to FWidth - 1 do
    begin
      r := Random(Amount + 1);
      ax := X + r - Random(r * 2);
      if ax < 0 then
        ax := 0
      else if ax > FWidth - 1 then
        ax := FWidth - 1;
      ay := Y + r - Random(r * 2);
      if ay < 0 then
        ay := 0
      else if ay > FHeight - 1 then
        ay := FHeight - 1;
      pDes^[X] := PByteArray(ScanLine(ay))[ax];
    end;
    pDes := PByteArray(TCnNativeInt(pDes) - BytesLineMask);
  end;
end;

// 对蒙板图进行水平镜象处理
procedure TCnAAMask.HorzMirror;
var
  X, Y: Integer;
  c: Byte;
  pLine: PByteArray;
begin
  for Y := 0 to FHeight - 1 do
  begin
    pLine := ScanLine(Y);
    for X := 0 to FWidth div 2 - 1 do
    begin
      c := pLine[X];
      pLine[X] := pLine[FWidth - 1 - X];
      pLine[FWidth - 1 - X] := c;
    end;
  end;
end;

// 对蒙板图进行垂直镜象处理
procedure TCnAAMask.VertMirror;
var
  pSrc, pDst, pBuf: PByteArray;
  I: Integer;
begin
  GetMem(pBuf, BytesLineMask);
  try
    for I := 0 to FHeight div 2 - 1 do
    begin
      pSrc := ScanLine(I);
      pDst := ScanLine(FHeight - 1 - I);
      CopyMemory(pBuf, pSrc, BytesLineMask);
      CopyMemory(pSrc, pDst, BytesLineMask);
      CopyMemory(pDst, pBuf, BytesLineMask);
    end;
  finally
    FreeMem(pBuf);
  end;
end;

// 象素地址
function TCnAAMask.PixelAddr(X, Y: Integer): Pointer;
begin
  if (X < 0) or (X > Width - 1) or (Y < 0) or (Y > Height - 1) then
    raise EInvalidPixel.Create('Invalid pixel!')
  else
    Result := Pointer(TCnNativeInt(FPMaskBuff) + (Height - 1 + Y) * BytesLineMask + X);
end;

// 象素
function TCnAAMask.Pixel(X, Y: Integer): Byte;
begin
  if (X < 0) or (X > Width - 1) or (Y < 0) or (Y > Height - 1) then
    raise EInvalidPixel.Create('Invalid pixel!')
  else
    Result := PByteArray(TCnNativeInt(FPMaskBuff) + (Height - 1 + Y) * BytesLineMask)[X];
end;

// 扫描线地址
function TCnAAMask.ScanLine(Line: Integer): Pointer;
begin
  if (Line < 0) or (Line > Height - 1) then
    raise EInvalidLine.Create('Invalid line!')
  else
    Result := Pointer(TCnNativeInt(FPMaskBuff) + (Height - 1 - Line) * BytesLineMask);
end;

function TCnAAMask.ScanLine(Line: Integer; pAData: PByteArray): PByteArray;
begin
  Result := PByteArray(TCnNativeInt(pAData) + (Height - 1 - Line) * BytesLineMask);
end;

// 设置精度
procedure TCnAAMask.SetQuality(const Value: TAAQuality);
begin
  FQuality := Value;
  case FQuality of
    aqHigh: Scale := 4;
    aqNormal: Scale := 3;
    aqLow: Scale := 2;
    aqNone: Scale := 1;
  else
    Scale := 1;
  end;
end;

function GetRotateSize(Size: TSize; Angle: Double; var StartPoint: TPoint): TSize;
var
  p1, p2, p3, p4: TPoint;
  cAngle, sAngle: Double;
  wCos, hCos, wSin, hSin: Double;
  SrcW2, SrcH2: Double;
  Rect: TRect;
begin
  sAngle := Sin(-Angle);
  cAngle := Cos(-Angle);

  // 计算目标顶点位置
  SrcW2 := Size.cx / 2;
  SrcH2 := Size.cy / 2;
  wCos := SrcW2 * cAngle;
  hCos := SrcH2 * cAngle;
  wSin := SrcW2 * sAngle;
  hSin := SrcH2 * sAngle;
  p1.X := Round(-wCos + hSin); // 左下
  p1.Y := Round(-wSin - hCos);
  p2.X := Round(wCos + hSin); // 右下
  p2.Y := Round(wSin - hCos);
  p3.X := Round(-wCos - hSin); // 左上
  p3.Y := Round(-wSin + hCos);
  p4.X := Round(wCos - hSin); // 右上
  p4.Y := Round(wSin + hCos);

  // 计算包含矩形
  Rect.Left := MinIntValue([p1.X, p2.X, p3.X, p4.X]);
  Rect.Right := MaxIntValue([p1.X, p2.X, p3.X, p4.X]);
  Rect.Top := MinIntValue([p1.Y, p2.Y, p3.Y, p4.Y]);
  Rect.Bottom := MaxIntValue([p1.Y, p2.Y, p3.Y, p4.Y]);

  Result.cx := Rect.Right - Rect.Left;
  Result.cy := Rect.Bottom - Rect.Top;
  StartPoint.X := p1.X + Result.cx div 2;
  StartPoint.Y := p1.Y + Result.cy div 2;
end;

// 文本高、宽，旋转后起始位置
function TCnAAMask.TextExtentEx(const S: string; var Point: TPoint): TSize;
var
  LogFont: TLogFont;
  TempFont, SaveFont: HFONT;
  DC: HDC;
  Beta: Double;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  DC := GetDC(0);
  try
    GetObject(FAAFont.Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
    with LogFont do
    begin
      lfHeight := lfHeight * Scale;
      lfWidth := lfWidth * Scale;
      Beta := lfEscapement * Pi / 1800;
    end;
    TempFont := CreateFontIndirect(LogFont);
    try
      SaveFont := SelectObject(DC, TempFont);
      GetTextExtentPoint32(DC, PChar(S), Length(S), Result);
      Result.cx := (Result.cx + Scale - 1) div Scale;
      Result.cy := (Result.cy + Scale - 1) div Scale;
      if (fsItalic in FAAFont.Canvas.Font.Style) and (Length(S) > 0) then
        Result.cx := Result.cx + Round(Result.cx / Length(S) * ItalicAdjust);
      SelectObject(DC, SaveFont);
      if Beta <> 0 then
      begin
        Result := GetRotateSize(Result, Beta, Point);
      end;
    finally
      DeleteObject(TempFont);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

// 文本高、宽
function TCnAAMask.TextExtent(const S: string): TSize;
var
  Point: TPoint;
begin
  Result := TextExtentEx(S, Point);
end;

// 文本高度
function TCnAAMask.TextHeight(const S: string): Integer;
begin
  Result := TextExtent(S).cy;
end;

// 文本宽度
function TCnAAMask.TextWidth(const S: string): Integer;
begin
  Result := TextExtent(S).cx;
end;

{ TCnAABlend }

//--------------------------------------------------------//
// 前景背景蒙板混合类                                     //
//--------------------------------------------------------//

// 初始化
constructor TCnAABlend.Create(AOwner: TCnAAFont);
begin
  FAAFont := AOwner;
  FForeBmp := TBitmap.Create;
  FForeBmp.PixelFormat := pf24bit;
  FRGBBmp := TBitmap.Create;
  FRGBBmp.PixelFormat := pf24bit;

  FForeBmp32 := TBitmap.Create;
  FForeBmp32.PixelFormat := pf32bit;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  FForeBmp32.AlphaFormat := afIgnored;
{$ENDIF}

  FBGRABmp := TBitmap.Create;
  FBGRABmp.PixelFormat := pf32bit;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
  FBGRABmp.AlphaFormat := afIgnored;
{$ENDIF}
end;

// 释放
destructor TCnAABlend.Destroy;
begin
  ForeBmp.Free;
  FForeBmp32.Free;
  FRGBBmp.Free;
  FBGRABmp.Free;
  inherited;
end;

// 赋值
procedure TCnAABlend.Assign(Source: TPersistent);
begin
  if Source is TCnAABlend then
  begin
    ForeBmp.Assign(TCnAABlend(Source).ForeBmp);
    ForeBmp32.Assign(TCnAABlend(Source).ForeBmp32);
  end
  else
    inherited Assign(Source);
end;

procedure TCnAABlend.SyncForeBmp32;
var
  Bf: TBlendFunction;
begin
  if FForeBmp.Empty then
    Exit;

  FForeBmp32.Width := FForeBmp.Width;
  FForeBmp32.Height := FForeBmp.Height;

  // 全盘复制过去
  Bf.BlendOp := AC_SRC_OVER;
  Bf.BlendFlags := 0;
  Bf.SourceConstantAlpha := 255;
  Bf.AlphaFormat := 0;  // 不能用透明度混合，必须复制

  AlphaBlend(FForeBmp32.Canvas.Handle, 0, 0, FForeBmp.Width, FForeBmp.Height,
    FForeBmp.Canvas.Handle, 0, 0, FForeBmp.Width, FForeBmp.Height, Bf);
end;

// 文本按前景色与背景混合
procedure TCnAABlend.Blend(X, Y: Integer; AColor: TColor; Alpha: TAlpha;
  Mask: TCnAAMask; DestIsAlpha: Boolean);
var
  R, B, G: Byte;
  AAlpha, DiffForeAlpha: DWORD;
  ForeAlpha, BkAlpha: Byte;
  pMask: PByteArray;
  pRGB: PRGBArray;
  pBGRA: PBGRAArray;
  PDW: PDWordArray;
  Weight: Byte;
  I, J: Integer;
  Color, BColor: TColor;
  Color32Rec: TRGBQuad;
  Bf: TBlendFunction;
begin
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  Color := ColorToRGB(AColor);          // 实际前景色
  R := GetRValue(Color);                // 色彩分量
  G := GetGValue(Color);
  B := GetBValue(Color);
  AAlpha := Alpha * $100 div 100;       // 得到前景透明度运算系数（0 到 256 范围）

  // 如果目标是 32 位带 Alpha 通道的需要 Alpha 支持，则使用 32 位带 Alpha 的中间图混合
  // 带 Alpha 混合完毕后再通过 AlphaBlend 复制透明度内容过去
  if DestIsAlpha then
  begin
    FBGRABmp.Width := Mask.Width;
    FBGRABmp.Height := Mask.Height;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
    FBGRABmp.AlphaFormat := afPremultiplied;
{$ENDIF}

    FBGRABmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);
    if FBGRABmp.Canvas.Brush.Style <> bsSolid then
    begin
      J := FBGRABmp.Width * SizeOf(TRGBQuad);
      for I := 0 to FBGRABmp.Height - 1 do
        FillChar(FBGRABmp.ScanLine[I]^, J, $00); // 先填充全透明，否则默认会白色填充

      // 透明，先带透明度地复制目标背景到 FBGRABmp
      Bf.BlendOp := AC_SRC_OVER;
      Bf.BlendFlags := 0;
      Bf.SourceConstantAlpha := $FF;
      Bf.AlphaFormat := AC_SRC_ALPHA;

      AlphaBlend(FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height,
        FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height, Bf);

      // UnPremutiply 化才能做下文的混合
      UnPreMultiplyBitmap(FBGRABmp);
    end
    else
    begin
      // Solid 时，用 FBGRABmp.Canvas.Brush.Color 来填充全不透明
      // FBGRABmp.Canvas.FillRect(Bounds(0, 0, FBGRABmp.Width, FBGRABmp.Height)); 无效，改手工

      BColor := ColorToRGB(FBGRABmp.Canvas.Brush.Color);     // 实际背景色
      Color32Rec.rgbRed := GetRValue(BColor);                // 色彩分量
      Color32Rec.rgbGreen := GetGValue(BColor);
      Color32Rec.rgbBlue := GetBValue(BColor);
      Color32Rec.rgbReserved := $FF;

      for I := 0 to FBGRABmp.Height - 1 do
      begin
        PDW := FBGRABmp.ScanLine[I];
        for J := 0 to FBGRABmp.Width - 1 do
          PDW^[J] := PDWORD(@Color32Rec)^; // 直接访问内存填色，已经是 UnPreMultiply 的了
      end;
    end;

    for J := 0 to FBGRABmp.Height - 1 do
    begin
      // 对于每一个像素的 UnPreMultiply 的前后两像素颜色混合公式：
      // 目标透明度=1-（1-前透明度）*（1-后透明度）
      // 目标分量=（前分量*前透明度 + 后分量*后透明度*（1-前透明度））/目标透明度
      // （其中透明度在 0 到 1 闭区间内，1 表示不透明，要想办法转换成 0 到 255

      pMask := Mask.ScanLine(J);
      pBGRA := FBGRABmp.ScanLine[J];
      for I := 0 to FBGRABmp.Width - 1 do
      begin
        // pMask[i] 是 0 到 255 范围内的文字前透明度，需要 * AAlpha shr 8 得到实际前透明度
        // pBGRA^[i].rgbReserved 是 0 到 255 范围内的后透明度
        // 所以目标透明度 = (65536 - (255 - ForeAlpha)(255 - BkAlpha)) / 255
        ForeAlpha := pMask[I] * AAlpha shr 8;
        BkAlpha := pBGRA^[I].rgbReserved;
        DiffForeAlpha := ($FF - ForeAlpha) * BkAlpha;

        // Weight 得到目标透明度
        Weight := ($FF * $FF - ($FF - ForeAlpha) * ($FF - BkAlpha)) div $FF;
        // 当前景完全不透明也就是 ForeAlpha 为 255 时，无论背景透明度多少，混合透明度为 255，也就是完全不透明
        // 当前景完全透明也就是 ForeAlpha 为 0 时，混合透明度为背景透明度 BkAlpha

        pBGRA^[I].rgbReserved := Weight;
        if Weight <> 0 then // 0 表示全透明，就不需要填色了
        begin
          pBGRA^[I].rgbBlue := (B * ForeAlpha + (pBGRA^[I].rgbBlue * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbGreen := (G * ForeAlpha + (pBGRA^[I].rgbGreen * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbRed := (R * ForeAlpha + (pBGRA^[I].rgbRed * DiffForeAlpha) div $FF) div Weight;
        end;
      end;
    end;

    PreMultiplyBitmap(FBGRABmp); // 很重要，做过 PreMultiply 后才能正常绘制

    // 注意此处 FBGRABmp 是已经混合了 FAAFont 原始内容了，
    // 不能再次和 FAAFont 内容再次透明度混合，而是全盘复制过去
    Bf.BlendOp := AC_SRC_OVER;
    Bf.BlendFlags := 0;
    Bf.SourceConstantAlpha := 255;
    Bf.AlphaFormat := 0;  // 不能再次用透明度混合了，必须复制

    AlphaBlend(FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height,
      FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height, Bf); // 输出
  end
  else // 如果目标不需要 Alpha 支持，无论目标是 24 位还是 32 位，都用 24 位中间图，再 BitBlt 过去
  begin
    FRGBBmp.Width := Mask.Width;
    FRGBBmp.Height := Mask.Height;
    FRGBBmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);
    if FRGBBmp.Canvas.Brush.Style <> bsSolid then
      BitBlt(FRGBBmp.Canvas.Handle, 0, 0, FRGBBmp.Width, FRGBBmp.Height,
        FAAFont.Canvas.Handle, X, Y, SRCCOPY) // 透明
    else
      FillRect(FRGBBmp.Canvas.Handle, Bounds(0, 0, FRGBBmp.Width, FRGBBmp.Height), 0);

    for J := 0 to FRGBBmp.Height - 1 do
    begin
      pMask := Mask.ScanLine(J);
      pRGB := FRGBBmp.ScanLine[J];
      for I := 0 to FRGBBmp.Width - 1 do
      begin
        Weight := pMask^[I] * AAlpha shr 8; // 混合系数
        if Weight <> 0 then
        begin
          if Weight = 255 then
          begin                           // 前景色
            pRGB^[I].rgbtBlue := B;
            pRGB^[I].rgbtGreen := G;
            pRGB^[I].rgbtRed := R;
          end
          else
          begin                           // 混合
            Inc(pRGB^[I].rgbtBlue, Weight * (B - pRGB^[I].rgbtBlue) shr 8);
            Inc(pRGB^[I].rgbtGreen, Weight * (G - pRGB^[I].rgbtGreen) shr 8);
            Inc(pRGB^[I].rgbtRed, Weight * (R - pRGB^[I].rgbtRed) shr 8);
          end;
        end;
      end;
    end;

    BitBlt(FAAFont.Canvas.Handle, X, Y, FRGBBmp.Width, FRGBBmp.Height,
      FRGBBmp.Canvas.Handle, 0, 0, SRCCOPY); // 输出
  end;
end;

// 文本按纹理与背景混合
procedure TCnAABlend.BlendEx(X, Y: Integer; Alpha: TAlpha; Mask: TCnAAMask;
  DestIsAlpha: Boolean);
var
  AAlpha, DiffForeAlpha: DWORD;
  ForeAlpha, BkAlpha: Byte;
  pMask: PByteArray;
  pRGB: PRGBArray;
  pFore: PRGBArray;
  pFor32: PBGRAArray;
  pBGRA: PBGRAArray;
  Weight: Byte;
  I, J: Integer;
  Bf: TBlendFunction;
  PDW: PDWordArray;
  BColor: TColor;
  Color32Rec: TRGBQuad;
begin
  if (FAAFont = nil) or (FAAFont.Canvas = nil) then
    Exit;

  if DestIsAlpha then
  begin
    if (ForeBmp32.Width <> Mask.Width) or (ForeBmp32.Height <> Mask.Height)
      or (ForeBmp32.PixelFormat <> pf32bit) then
    begin                                 // 错误的纹理图
      raise EInvalidForeBmp.Create(SInvalidForeground);
      Exit;
    end;

    FBGRABmp.Width := Mask.Width;
    FBGRABmp.Height := Mask.Height;
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
    FBGRABmp.AlphaFormat := afPremultiplied;
{$ENDIF}

    AAlpha := Alpha * $100 div 100;       // 透明度
    FBGRABmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);

    if FBGRABmp.Canvas.Brush.Style <> bsSolid then
    begin
      J := FBGRABmp.Width * SizeOf(TRGBQuad);
      for I := 0 to FBGRABmp.Height - 1 do
        FillChar(FBGRABmp.ScanLine[I]^, J, $00); // 先填充全透明，否则默认会白色填充

      // 透明，先带透明度地复制目标背景到 FBGRABmp
      Bf.BlendOp := AC_SRC_OVER;
      Bf.BlendFlags := 0;
      Bf.SourceConstantAlpha := $FF;
      Bf.AlphaFormat := AC_SRC_ALPHA;

      AlphaBlend(FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height,
        FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height, Bf);

      // UnPremutiply 化才能做下文的混合
      UnPreMultiplyBitmap(FBGRABmp);
    end
    else
    begin
      // Solid 时，用 FBGRABmp.Canvas.Brush.Color 来填充全不透明
      BColor := ColorToRGB(FBGRABmp.Canvas.Brush.Color);     // 实际背景色
      Color32Rec.rgbRed := GetRValue(BColor);                // 色彩分量
      Color32Rec.rgbGreen := GetGValue(BColor);
      Color32Rec.rgbBlue := GetBValue(BColor);
      Color32Rec.rgbReserved := $FF;

      for I := 0 to FBGRABmp.Height - 1 do
      begin
        PDW := FBGRABmp.ScanLine[I];
        for J := 0 to FBGRABmp.Width - 1 do
          PDW^[J] := PDWORD(@Color32Rec)^; // 直接访问内存填色，已经是 UnPreMultiply 的了
      end;
    end;

    // 在不支持 AlphaFormat 的版本中，ForeBmp32 的内容由外界保证 PreMultiply，在此处进行 UnPreMultiply
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
    ForeBmp32.AlphaFormat := afIgnored;
{$ENDIF}

    for J := 0 to FBGRABmp.Height - 1 do
    begin
      pMask := Mask.ScanLine(J);
      pBGRA := FBGRABmp.ScanLine[J];
      pFor32 := ForeBmp32.ScanLine[J];

      for I := 0 to FBGRABmp.Width - 1 do
      begin
        // pMask[i] 是 0 到 255 范围内的文字前透明度，需要 * AAlpha shr 8 得到实际前透明度
        // pBGRA^[i].rgbReserved 是 0 到 255 范围内的后透明度
        // 所以目标透明度 = (65536 - (255 - ForeAlpha)(255 - BkAlpha)) / 255
        ForeAlpha := pMask[I] * AAlpha shr 8;
        BkAlpha := pBGRA^[I].rgbReserved;
        DiffForeAlpha := ($FF - ForeAlpha) * BkAlpha;

        // Weight 得到目标透明度
        Weight := ($FF * $FF - ($FF - ForeAlpha) * ($FF - BkAlpha)) div $FF;
        // 当前景完全不透明也就是 ForeAlpha 为 255 时，无论背景透明度多少，混合透明度为 255，也就是完全不透明
        // 当前景完全透明也就是 ForeAlpha 为 0 时，混合透明度为背景透明度 BkAlpha

        pBGRA^[I].rgbReserved := Weight;
        if Weight <> 0 then // 0 表示全透明，就不需要填色了
        begin
          pBGRA^[I].rgbBlue := (pFor32^[I].rgbBlue * ForeAlpha +
            (pBGRA^[I].rgbBlue * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbGreen := (pFor32^[I].rgbGreen * ForeAlpha +
            (pBGRA^[I].rgbGreen * DiffForeAlpha) div $FF) div Weight;
          pBGRA^[I].rgbRed := (pFor32^[I].rgbRed * ForeAlpha +
            (pBGRA^[I].rgbRed * DiffForeAlpha) div $FF) div Weight;
        end;
      end;
    end;

    PreMultiplyBitmap(FBGRABmp); // 很重要，做过 PreMultiply 后才能正常绘制

    // 注意此处 FBGRABmp 是已经混合了 FAAFont 原始内容了，
    // 不能再次和 FAAFont 内容再次透明度混合，而是全盘复制过去
    Bf.BlendOp := AC_SRC_OVER;
    Bf.BlendFlags := 0;
    Bf.SourceConstantAlpha := 255;
    Bf.AlphaFormat := 0;  // 不能再次用透明度混合了，必须复制

    AlphaBlend(FAAFont.Canvas.Handle, X, Y, FBGRABmp.Width, FBGRABmp.Height,
      FBGRABmp.Canvas.Handle, 0, 0, FBGRABmp.Width, FBGRABmp.Height, Bf); // 输出
  end
  else
  begin
    if (ForeBmp.Width <> Mask.Width) or (ForeBmp.Height <> Mask.Height)
      or (ForeBmp.PixelFormat <> pf24bit) then
    begin                                 // 错误的纹理图
      raise EInvalidForeBmp.Create(SInvalidForeground);
      Exit;
    end;

    FRGBBmp.Width := Mask.Width;
    FRGBBmp.Height := Mask.Height;
    AAlpha := Alpha * $100 div 100;       // 透明度
    FRGBBmp.Canvas.Brush.Assign(FAAFont.Canvas.Brush);
    if FRGBBmp.Canvas.Brush.Style <> bsSolid then
      Bitblt(FRGBBmp.Canvas.Handle, 0, 0, FRGBBmp.Width, FRGBBmp.Height,
        FAAFont.Canvas.Handle, X, Y, SRCCOPY) // 透明
    else
      FillRect(FRGBBmp.Canvas.Handle, Bounds(0, 0, FRGBBmp.Width, FRGBBmp.Height), 0);

    for J := 0 to FRGBBmp.Height - 1 do
    begin
      pMask := Mask.ScanLine(J);
      pRGB := FRGBBmp.ScanLine[J];
      pFore := ForeBmp.ScanLine[J];

      for I := 0 to FRGBBmp.Width - 1 do
      begin
        Weight := pMask^[I] * AAlpha shr 8; // 混合系数
        if Weight = 255 then
        begin
          pRGB^[I].rgbtBlue := pFore^[I].rgbtBlue;
          pRGB^[I].rgbtGreen := pFore^[I].rgbtGreen;
          pRGB^[I].rgbtRed := pFore^[I].rgbtRed;
        end
        else if Weight <> 0 then          // 以 pMask 为透明度，pFore 为原始分量进行混合
        begin
          Inc(pRGB^[I].rgbtBlue, Weight * (pFore^[I].rgbtBlue - pRGB^[I].rgbtBlue) shr
            8);
          Inc(pRGB^[I].rgbtGreen, Weight * (pFore^[I].rgbtGreen - pRGB^[I].rgbtGreen) shr
            8);
          Inc(pRGB^[I].rgbtRed, Weight * (pFore^[I].rgbtRed - pRGB^[I].rgbtRed) shr 8);
        end;
      end;
    end;

    Bitblt(FAAFont.Canvas.Handle, X, Y, FRGBBmp.Width, FRGBBmp.Height,
      FRGBBmp.Canvas.Handle, 0, 0, SRCCOPY); // 输出
  end;
end;

// 设置前景纹理图
procedure TCnAABlend.SetForeBmp(const Value: TBitmap);
begin
  FForeBmp.Assign(Value);
end;

procedure TCnAABlend.SetForeBmp32(const Value: TBitmap);
begin
  FForeBmp32.Assign(Value);
end;

{ TCnAAFont }

//--------------------------------------------------------//
// 平滑字体类                                             //
//--------------------------------------------------------//

// 初始化
constructor TCnAAFont.Create(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  Mask := TCnAAMask.Create(Self);
  Blend := TCnAABlend.Create(Self);
end;

// 释放
destructor TCnAAFont.Destroy;
begin
  Mask.Free;
  Blend.Free;
  inherited;
end;

// 取显示精度
function TCnAAFont.GetQuality: TAAQuality;
begin
  Result := Mask.Quality;
end;

// 设置显示精度
procedure TCnAAFont.SetQuality(const Value: TAAQuality);
begin
  Mask.Quality := Value;
end;

// 文本高、宽
function TCnAAFont.TextExtent(const S: string): TSize;
begin
  Result := Mask.TextExtent(S);
end;

// 文本高度
function TCnAAFont.TextHeight(const S: string): Integer;
begin
  Result := TextExtent(S).cy;
end;

// 文本宽度
function TCnAAFont.TextWidth(const S: string): Integer;
begin
  Result := TextExtent(S).cx;
end;

// 平滑文本输出
procedure TCnAAFont.TextOutput(X, Y: Integer; const S: string; Alpha: TAlpha;
  Blur: TBlurStrength; DestIsAlpha: Boolean);
begin
  if (Canvas = nil) or (S = '') then
    Exit;

  Mask.DrawMask(S);                     // 创建字体蒙板
  if Blur > 0 then
    Mask.Blur(Blur);                    // 模糊
  Blend.Blend(X, Y, Canvas.Font.Color, Alpha, Mask, DestIsAlpha); // 与前景色混合
end;

{ TCnAAFontEx }

//--------------------------------------------------------//
// 增强平滑字体类                                         //
//--------------------------------------------------------//

// 初始化
constructor TCnAAFontEx.Create(ACanvas: TCanvas);
begin
  inherited Create(ACanvas);
  FEffect := TCnAAEffect.Create(nil);
end;

// 释放
destructor TCnAAFontEx.Destroy;
begin
  FEffect.Free;
  inherited;
end;

// 设置显示风格
procedure TCnAAFontEx.SetEffect(const Value: TCnAAEffect);
begin
  FEffect.Assign(Value);
end;

// 计算阴影偏移
function TCnAAFontEx.GetShadowPoint: TPoint;
begin
  if Effect.Shadow.Enabled then
  begin
    if Effect.Shadow.OffsetX > 0 then
      Result.X := Effect.Shadow.OffsetX
    else
      Result.X := 0;
    if Effect.Shadow.OffsetY > 0 then
      Result.Y := Effect.Shadow.OffsetY
    else
      Result.Y := 0;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

// 计算文本偏移
function TCnAAFontEx.GetTextPoint: TPoint;
begin
  if Effect.Shadow.Enabled then
  begin
    if Effect.Shadow.OffsetX < 0 then
      Result.X := Abs(Effect.Shadow.OffsetX)
    else
      Result.X := 0;
    if Effect.Shadow.OffsetY < 0 then
      Result.Y := Abs(Effect.Shadow.OffsetY)
    else
      Result.Y := 0;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

// 文本高、宽
function TCnAAFontEx.TextExtent(const S: string): TSize;
var
  LogFont: TLogFont;
  TempFont: HFONT;
  SaveFont: TFont;
begin
  if Effect.Angle <> 0 then
  begin
    GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
    LogFont.lfEscapement := Effect.Angle * 10;
    SaveFont := TFont.Create;
    try
      SaveFont.Assign(Canvas.Font);
      TempFont := CreateFontIndirect(LogFont);
      Canvas.Font.Handle := TempFont;
      Result := inherited TextExtent(S);
      Canvas.Font.Assign(SaveFont);
      DeleteObject(TempFont);
    finally
      SaveFont.Free;
    end;
  end
  else
    Result := inherited TextExtent(S);

  if Effect.Shadow.Enabled then
  begin
    Inc(Result.cx, Abs(Effect.Shadow.OffsetX));
    Inc(Result.cy, Abs(Effect.Shadow.OffsetY));
  end;
end;

// 创建渐变色前景
procedure TCnAAFontEx.CreateGradual;
var
  Buf, Dst: PRGBArray;
  BufLen, Len: Integer;
  SCol, ECol: TColor;
  sr, sb, sg: Byte;
  er, eb, eg: Byte;
  BufSize: Integer;
  I, J: Integer;
  Width, Height: Integer;
begin
  if (Canvas = nil) or not Effect.Gradual.Enabled then
    Exit;

  Height := Mask.Height;
  Width := Mask.Width;
  Blend.ForeBmp.Height := Height;
  Blend.ForeBmp.Width := Width;

  if Effect.Gradual.Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
    BufLen := Width                     //  缓冲区长度
  else
    BufLen := Height;
  if Effect.Gradual.Style in [gsCenterToLR, gsCenterToTB] then
    Len := (BufLen + 1) div 2           // 渐变带长度
  else
    Len := BufLen;
  BufSize := BufLen * 3;
  GetMem(Buf, BufSize);
  try
    // 创建渐变色带缓冲区
    if Effect.Gradual.Style in [gsLeftToRight, gsTopToBottom] then
    begin
      SCol := ColorToRGB(Effect.Gradual.StartColor);
      ECol := ColorToRGB(Effect.Gradual.EndColor);
    end
    else begin
      SCol := ColorToRGB(Effect.Gradual.EndColor);
      ECol := ColorToRGB(Effect.Gradual.StartColor);
    end;
    sr := GetRValue(SCol);              // 起始色
    sg := GetGValue(SCol);
    sb := GetBValue(SCol);
    er := GetRValue(ECol);              // 结束色
    eg := GetGValue(ECol);
    eb := GetBValue(ECol);
    for I := 0 to Len - 1 do
    begin
      Buf[I].rgbtRed := sr + (er - sr) * I div Len;
      Buf[I].rgbtGreen := sg + (eg - sg) * I div Len;
      Buf[I].rgbtBlue := sb + (eb - sb) * I div Len;
    end;

    if Effect.Gradual.Style in [gsCenterToLR, gsCenterToTB] then // 对称渐变
      for I := 0 to Len - 1 do
        Buf[BufLen - 1 - I] := Buf[I];

    if Effect.Gradual.Style in [gsLeftToRight, gsRightToLeft, gsCenterToLR] then
      for I := 0 to Height - 1 do  // 水平渐变
        Move(Buf[0], Blend.ForeBmp.ScanLine[I]^, BufSize)
    else
      for I := 0 to Height - 1 do  // 垂直渐变
      begin
        Dst := Blend.ForeBmp.ScanLine[I];
        for J := 0 to Width - 1 do
          Dst^[J] := Buf[I];
      end;
  finally
    FreeMem(Buf);
  end;
end;

// 创建噪声前景图
procedure TCnAAFontEx.CreateNoiseBmp;
var
  pLine: PRGBArray;
  X, Y: Integer;
  r, g, b: Byte;
  nr, ng, nb: Integer;
  Amount: Byte;
begin
  r := GetRValue(ColorToRGB(Canvas.Font.Color));
  g := GetGValue(ColorToRGB(Canvas.Font.Color));
  b := GetBValue(ColorToRGB(Canvas.Font.Color));
  Amount := Effect.Noise;

  Blend.ForeBmp.Height := Mask.Height;
  Blend.ForeBmp.Width := Mask.Width;

  for Y := 0 to Blend.ForeBmp.Height - 1 do
  begin
    pLine := Blend.ForeBmp.ScanLine[Y];
    for X := 0 to Blend.ForeBmp.Width - 1 do
    begin
      nr := r + Random(Amount) - Amount shr 1;
      ng := g + Random(Amount) - Amount shr 1;
      nb := b + Random(Amount) - Amount shr 1;
      if nr < 0 then
        nr := 0
      else if nr > 255 then
        nr := 255;
      if ng < 0 then
        ng := 0
      else if ng > 255 then
        ng := 255;
      if nb < 0 then
        nb := 0
      else if nb > 255 then
        nb := 255;
      pLine^[X].rgbtRed := nr;
      pLine^[X].rgbtGreen := ng;
      pLine^[X].rgbtBlue := nb;
    end;
  end;
end;

// 增强平滑文本输出
procedure TCnAAFontEx.TextOutput(X, Y: Integer; const S: string;
  DestIsAlpha: Boolean);
var
  TextPoint, ShadowPoint: TPoint;
  OldBrushStyle: TBrushStyle;
  ShadowMask: TCnAAMask;
  LogFont: TLogFont;
  TempFont: HFONT;
  SaveFont: TFont;
begin
  if (Canvas = nil) or (S = '') then
    Exit;

  TempFont := 0;
  SaveFont := nil;
  try
    if Effect.Angle <> 0 then
    begin
      SaveFont := TFont.Create;
      SaveFont.Assign(Canvas.Font);
      GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @LogFont);
      LogFont.lfEscapement := Effect.Angle * 10;
      TempFont := CreateFontIndirect(LogFont);
      Canvas.Font.Handle := TempFont;
    end;

    if Effect.Shadow.Enabled then         // 阴影计算
    begin
      TextPoint := GetTextPoint;
      ShadowPoint := GetShadowPoint;
      TextPoint.X := TextPoint.X + X;
      TextPoint.Y := TextPoint.Y + Y;
      ShadowPoint.X := ShadowPoint.X + X;
      ShadowPoint.Y := ShadowPoint.Y + Y;
    end
    else
    begin
      TextPoint := Point(X, Y);
    end;

    Mask.DrawMask(S);                     // 创建字体蒙板
    if Effect.Outline then
      Mask.Outline;
    if Effect.Spray > 0 then
      Mask.Spray(Effect.Spray);
    if Effect.HorzMirror then
      Mask.HorzMirror;
    if Effect.VertMirror then
      Mask.VertMirror;

    OldBrushStyle := Canvas.Brush.Style;
    if Effect.Shadow.Enabled then         // 阴影处理
    begin
      ShadowMask := TCnAAMask.Create(Self);
      ShadowMask.Assign(Mask);            // 阴影蒙板
      if Effect.Shadow.Blur > 0 then
        ShadowMask.Blur(Effect.Shadow.Blur); // 阴影模糊
      Blend.Blend(ShadowPoint.X, ShadowPoint.Y, Effect.Shadow.Color,
        Effect.Shadow.Alpha * Effect.Alpha div 100, ShadowMask, DestIsAlpha);
      // 此处把 ShadowMask 中的模糊阴影复制到了 Blend.AAFont.Canvas 中
      ShadowMask.Free;
      Canvas.Brush.Style := bsClear;      // 透明
    end;

    if Effect.Blur > 0 then               // 文本模糊
      Mask.Blur(Effect.Blur);

    if Effect.Texture.Enabled and Assigned(Effect.Texture.Picture.Graphic) and
      not Effect.Texture.Picture.Graphic.Empty then
    begin
      CreateForeBmp;                      // 创建字体纹理图
      if Effect.Noise > 0 then
        AddNoise(Effect.Noise);

      if DestIsAlpha then
        Blend.SyncForeBmp32;
      Blend.BlendEx(TextPoint.X, TextPoint.Y, Effect.Alpha, Mask, DestIsAlpha);
    end
    else if Effect.Gradual.Enabled then
    begin                                 // 创建渐变色前景图
      CreateGradual;
      if Effect.Noise > 0 then
        AddNoise(Effect.Noise);

      if DestIsAlpha then
        Blend.SyncForeBmp32;
      Blend.BlendEx(TextPoint.X, TextPoint.Y, Effect.Alpha, Mask, DestIsAlpha);
    end
    else
    begin                                 // 混合输出
      if Effect.Noise > 0 then
      begin
        CreateNoiseBmp;

        if DestIsAlpha then
          Blend.SyncForeBmp32;
        Blend.BlendEx(TextPoint.X, TextPoint.Y, Effect.Alpha, Mask, DestIsAlpha);
      end
      else
        Blend.Blend(TextPoint.X, TextPoint.Y, Canvas.Font.Color, Effect.Alpha,
          Mask, DestIsAlpha);
    end;

    if Effect.Shadow.Enabled then
      Canvas.Brush.Style := OldBrushStyle;
  finally
    if Effect.Angle <> 0 then
    begin
      Canvas.Font.Assign(SaveFont);
      SaveFont.Free;
      DeleteObject(TempFont);
    end;
  end;
end;

// 绘制平铺图
procedure TCnAAFontEx.DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
var
  R, Rows, C, Cols: Integer;
begin
  if (G <> nil) and (not G.Empty) then
  begin
    Rows := ((Rect.Bottom - Rect.Top) div G.Height) + 1;
    Cols := ((Rect.Right - Rect.Left) div G.Width) + 1;
    for R := 1 to Rows do
      for C := 1 to Cols do
        Canvas.Draw(Rect.Left + (C - 1) * G.Width, Rect.Top + (R - 1) * G.Height, G);
  end;
end;

// 创建纹理图
procedure TCnAAFontEx.CreateForeBmp;
var
  Width, Height: Integer;
begin
  if (Canvas = nil) or not Effect.Texture.Enabled or
    not Assigned(Effect.Texture.Picture) then
    Exit;

  Height := Mask.Height;
  Width := Mask.Width;
  Blend.ForeBmp.Height := Height;
  Blend.ForeBmp.Width := Width;
  Blend.ForeBmp.Canvas.Brush.Color := Canvas.Font.Color;
  Blend.ForeBmp.Canvas.Brush.Style := bsSolid;
  Blend.ForeBmp.Canvas.FillRect(Rect(0, 0, Width, Height));
  case Effect.Texture.Mode of
    tmTiled:                            // 平铺
      with Blend.ForeBmp do
        DrawTiled(Canvas, Rect(0, 0, Width, Height),
          Effect.Texture.Picture.Graphic);
    tmStretched:                        // 拉伸
      with Blend.ForeBmp do
        StrectchDrawGraphic(Canvas, Rect(0, 0, Width, Height),
          Effect.Texture.Picture.Graphic, Canvas.Font.Color);
    tmCenter:                           // 中心
      with Effect.Texture.Picture do
        Blend.ForeBmp.Canvas.Draw((Blend.ForeBmp.Width - Graphic.Width) div 2,
          (Blend.ForeBmp.Height - Graphic.Height) div 2, Graphic);
    tmNormal:                           // 普通
      with Effect.Texture.Picture do
        Blend.ForeBmp.Canvas.Draw(0, 0, Graphic);
  end;
end;

// 增加噪声点
procedure TCnAAFontEx.AddNoise(Amount: Byte);
var
  pLine: PByteArray;
  X, Y: Integer;
  Val: Integer;
begin
  for Y := 0 to Blend.ForeBmp.Height - 1 do
  begin
    pLine := Blend.ForeBmp.ScanLine[Y];
    for X := 0 to Blend.ForeBmp.Width * 3 - 1 do
    begin
      Val := pLine^[X];
      Val := Val + Random(Amount) - Amount shr 1;
      if Val < 0 then
        Val := 0
      else if Val > 255 then
        Val := 255;
      pLine^[X] := Val;
    end;
  end;
end;

{ TCnEnabledClass }

//--------------------------------------------------------//
// 带Enabled功能的更新通知类                              //
//--------------------------------------------------------//

//赋值
procedure TCnEnabledClass.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnEnabledClass then
  begin
    FEnabled := TCnEnabledClass(Source).Enabled;
  end;
end;

// 更新通知
procedure TCnEnabledClass.Changed;
begin
  if FEnabled then                      // 如果能用则通知
    inherited Changed;
end;

// 创建
constructor TCnEnabledClass.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FEnabled := False;
end;

// 设置参数
procedure TCnEnabledClass.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

{ TCnAAShadow }

//--------------------------------------------------------//
// 阴影参数类                                             //
//--------------------------------------------------------//

// 赋值
procedure TCnAAShadow.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAAShadow then
  begin
    FBlur := TCnAAShadow(Source).Blur;
    FColor := TCnAAShadow(Source).Color;
    FOffsetX := TCnAAShadow(Source).OffsetX;
    FOffsetY := TCnAAShadow(Source).OffsetY;
  end;
end;

// 创建
constructor TCnAAShadow.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FBlur := 80;
  FAlpha := 70;
  FColor := $00444444;
  FOffsetX := 2;
  FOffsetY := 2;
end;

// 设置不透明度
procedure TCnAAShadow.SetAlpha(const Value: TAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// 设置模糊度
procedure TCnAAShadow.SetBlur(const Value: TBlurStrength);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Changed;
  end;
end;

// 设置阴影色
procedure TCnAAShadow.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

// 设置水平偏移
procedure TCnAAShadow.SetOffsetX(const Value: TOffset);
begin
  if FOffsetX <> Value then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;

// 设置垂直偏移
procedure TCnAAShadow.SetOffsetY(const Value: TOffset);
begin
  if FOffsetY <> Value then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;

{ TCnAAGradual }

//--------------------------------------------------------//
// 渐变色参数类                                           //
//--------------------------------------------------------//

// 赋值
procedure TCnAAGradual.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAAGradual then
  begin
    FStyle := TCnAAGradual(Source).Style;
    FStartColor := TCnAAGradual(Source).StartColor;
    FEndColor := TCnAAGradual(Source).EndColor;
  end;
end;

// 创建
constructor TCnAAGradual.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FStyle := gsLeftToRight;
  FStartColor := clWhite;
  FEndColor := clBlack;
end;

// 设置结束色
procedure TCnAAGradual.SetEndColor(const Value: TColor);
begin
  if FEndColor <> Value then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

// 设置开始色
procedure TCnAAGradual.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

// 设置渐变方式
procedure TCnAAGradual.SetStyle(const Value: TGradualStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

{ TCnAATexture }

//--------------------------------------------------------//
//字体纹理参数类                                          //
//--------------------------------------------------------//

// 赋值
procedure TCnAATexture.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAATexture then
  begin
    FMode := TCnAATexture(Source).Mode;
    FPicture.Assign(TCnAATexture(Source).Picture);
  end;
end;

// 创建
constructor TCnAATexture.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FMode := tmTiled;
end;

// 释放
destructor TCnAATexture.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

// 图像内容改变
procedure TCnAATexture.PictureChanged(Sender: TObject);
begin
  Changed;
end;

// 设置显示模式
procedure TCnAATexture.SetMode(const Value: TTextureMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

// 设置图像
procedure TCnAATexture.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ TCnAAEffect }

//--------------------------------------------------------//
// 字体特效参数类                                         //
//--------------------------------------------------------//

// 赋值
procedure TCnAAEffect.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TCnAAEffect then
  begin
    FAlpha := TCnAAEffect(Source).Alpha;
    FBlur := TCnAAEffect(Source).Blur;
    FNoise := TCnAAEffect(Source).Noise;
    FOutline := TCnAAEffect(Source).Outline;
    FHorzMirror := TCnAAEffect(Source).FHorzMirror;
    FVertMirror := TCnAAEffect(Source).FVertMirror;
    FSpray := TCnAAEffect(Source).FSpray;
    FAngle := TCnAAEffect(Source).FAngle;
    FShadow.Assign(TCnAAEffect(Source).Shadow);
    FGradual.Assign(TCnAAEffect(Source).Gradual);
    FTexture.Assign(TCnAAEffect(Source).Texture);
  end;
end;

// 创建
constructor TCnAAEffect.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FAlpha := 100;
  FBlur := 0;
  FNoise := 0;
  FSpray := 0;
  FAngle := 0;
  FOutline := False;
  FHorzMirror := False;
  FVertMirror := False;
  FShadow := TCnAAShadow.Create(OnChildChanged);
  FGradual := TCnAAGradual.Create(OnChildChanged);
  FTexture := TCnAATexture.Create(OnChildChanged);
end;

// 释放
destructor TCnAAEffect.Destroy;
begin
  FShadow.Free;
  FGradual.Free;
  FTexture.Free;
  inherited;
end;

const
  vsAlpha = 'Alpha';
  vsBlur = 'Blur';
  vsNoise = 'Noise';
  vsSpray = 'Spray';
  vsAngle = 'Angle';
  vsOutline = 'Outline';
  vsHorzMirror = 'HorzMirror';
  vsVertMirror = 'VertMirror';
  vsShadow = 'Shadow';
  vsShadowAlpha = 'ShadowAlpha';
  vsShadowBlur = 'ShadowBlur';
  vsShadowColor = 'ShadowColor';
  vsShadowOffsetX = 'ShadowOffsetX';
  vsShadowOffsetY = 'ShadowOffsetY';
  vsGradual = 'Gradual';
  vsGradualStartColor = 'GradualStartColor';
  vsGradualEndColor = 'GradualEndColor';
  vsGradualStyle = 'GradualStyle';
  vsTexture = 'Texture';
  vsTextureMode = 'TextureMode';
  vsTextPicture = 'TexturePicture';

// 从INI中装载参数
procedure TCnAAEffect.LoadFromIni(Ini: TCustomIniFile; const Section: string);
begin
  with Ini do
  begin
    FAlpha := ReadInteger(Section, vsAlpha, FAlpha);
    FBlur := ReadInteger(Section, vsBlur, FBlur);
    FNoise := ReadInteger(Section, vsNoise, FNoise);
    FSpray := ReadInteger(Section, vsSpray, FSpray);
    FAngle := ReadInteger(Section, vsAngle, FAngle);
    FOutline := ReadBool(Section, vsOutline, FOutline);
    FHorzMirror := ReadBool(Section, vsHorzMirror, FHorzMirror);
    FVertMirror := ReadBool(Section, vsVertMirror, FVertMirror);
    FShadow.FEnabled := ReadBool(Section, vsShadow, FShadow.FEnabled);
    FShadow.FAlpha := ReadInteger(Section, vsShadowAlpha, FShadow.FAlpha);
    FShadow.FBlur := ReadInteger(Section, vsShadowBlur, FShadow.FBlur);
    FShadow.FColor := ReadInteger(Section, vsShadowColor, FShadow.FColor);
    FShadow.FOffsetX := ReadInteger(Section, vsShadowOffsetX, FShadow.FOffsetX);
    FShadow.FOffsetY := ReadInteger(Section, vsShadowOffsetY, FShadow.FOffsetY);
    FGradual.FEnabled := ReadBool(Section, vsGradual, FGradual.FEnabled);
    FGradual.FStartColor := ReadInteger(Section, vsGradualStartColor, FGradual.FStartColor);
    FGradual.FEndColor := ReadInteger(Section, vsGradualEndColor, FGradual.FEndColor);
    FGradual.FStyle := TGradualStyle(ReadInteger(Section, vsGradualStyle, Ord(FGradual.FStyle)));
    if not (FGradual.FStyle in [Low(TGradualStyle)..High(TGradualStyle)]) then
      FGradual.FStyle := Low(TGradualStyle);
    FTexture.FEnabled := ReadBool(Section, vsTexture, FTexture.FEnabled);
    FTexture.FMode := TTextureMode(ReadInteger(Section, vsTextureMode, Ord(FTexture.FMode)));
    if not (FTexture.FMode in [Low(TTextureMode)..High(TTextureMode)]) then
      FTexture.FMode := Low(TTextureMode);
  end;
end;

// 从保存参数到INI中
procedure TCnAAEffect.SaveToIni(Ini: TCustomIniFile; const Section: string);
begin
  with Ini do
  begin
    WriteInteger(Section, vsAlpha, FAlpha);
    WriteInteger(Section, vsBlur, FBlur);
    WriteInteger(Section, vsNoise, FNoise);
    WriteInteger(Section, vsSpray, FSpray);
    WriteInteger(Section, vsAngle, FAngle);
    WriteBool(Section, vsOutline, FOutline);
    WriteBool(Section, vsHorzMirror, FHorzMirror);
    WriteBool(Section, vsVertMirror, FVertMirror);
    WriteBool(Section, vsShadow, FShadow.FEnabled);
    WriteInteger(Section, vsShadowAlpha, FShadow.FAlpha);
    WriteInteger(Section, vsShadowBlur, FShadow.FBlur);
    WriteInteger(Section, vsShadowColor, FShadow.FColor);
    WriteInteger(Section, vsShadowOffsetX, FShadow.FOffsetX);
    WriteInteger(Section, vsShadowOffsetY, FShadow.FOffsetY);
    WriteBool(Section, vsGradual, FGradual.FEnabled);
    WriteInteger(Section, vsGradualStartColor, FGradual.FStartColor);
    WriteInteger(Section, vsGradualEndColor, FGradual.FEndColor);
    WriteInteger(Section, vsGradualStyle, Ord(FGradual.FStyle));
    WriteBool(Section, vsTexture, FTexture.FEnabled);
    WriteInteger(Section, vsTextureMode, Ord(FTexture.FMode));
  end;
end;

//设置不透明度
procedure TCnAAEffect.SetAlpha(const Value: TAlpha);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    Changed;
  end;
end;

// 设置模糊度
procedure TCnAAEffect.SetBlur(const Value: TBlurStrength);
begin
  if FBlur <> Value then
  begin
    FBlur := Value;
    Changed;
  end;
end;

// 设置字体边缘
procedure TCnAAEffect.SetOutline(const Value: Boolean);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    Changed;
  end;
end;

// 设置噪声参数
procedure TCnAAEffect.SetNoise(const Value: Byte);
begin
  if FNoise <> Value then
  begin
    FNoise := Value;
    Changed;
  end;
end;

// 设置水平镜象
procedure TCnAAEffect.SetHorzMirror(const Value: Boolean);
begin
  if FHorzMirror <> Value then
  begin
    FHorzMirror := Value;
    Changed;
  end;
end;

// 设置垂直镜象
procedure TCnAAEffect.SetVertMirror(const Value: Boolean);
begin
  if FVertMirror <> Value then
  begin
    FVertMirror := Value;
    Changed;
  end;
end;

// 设置喷溅
procedure TCnAAEffect.SetSpray(const Value: TSprayRange);
begin
  if FSpray <> Value then
  begin
    FSpray := Value;
    Changed;
  end;
end;

// 设置旋转角度
procedure TCnAAEffect.SetAngle(const Value: TAngle);
begin
  if FAngle <> Value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

// 设置渐变色
procedure TCnAAEffect.SetGradual(const Value: TCnAAGradual);
begin
  FGradual.Assign(Value);
  Changed;
end;

// 设置阴影参数
procedure TCnAAEffect.SetShadow(const Value: TCnAAShadow);
begin
  FShadow.Assign(Value);
  Changed;
end;

// 设置字体纹理参数
procedure TCnAAEffect.SetTexture(const Value: TCnAATexture);
begin
  FTexture.Assign(Value);
  Changed;
end;

{ TCnFontLabel }

//--------------------------------------------------------//
// 字体标签类                                             //
//--------------------------------------------------------//

// 字体变动通知
procedure TCnFontLabel.Changed;
begin
  if Assigned(FontLabels) then
    FontLabels.Changed;
end;

// 初始化
constructor TCnFontLabel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FFont := TFont.Create;
  FFont.OnChange := OnEffectChanged;
  FEffect := TCnAAEffect.Create(OnEffectChanged);
  FEffect.FOwner := Self;
end;

// 释放
destructor TCnFontLabel.Destroy;
begin
  FFont.Free;
  FEffect.Free;
  inherited;
end;

// 对象赋值方法
procedure TCnFontLabel.Assign(Source: TPersistent);
begin
  if Source is TCnFontLabel then
  begin
    FName := TCnFontLabel(Source).FName;
    FFont.Assign(TCnFontLabel(Source).FFont);
    FEffect.Assign(TCnFontLabel(Source).FEffect);
  end
  else
    inherited;
end;

// 取显示名
function TCnFontLabel.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetDisplayName;
end;

// 取字体标签集
function TCnFontLabel.GetFontLabels: TCnFontLabels;
begin
  if Collection is TCnFontLabels then
    Result := TCnFontLabels(Collection)
  else
    Result := nil;
end;

// 字体风格变动通知
procedure TCnFontLabel.OnEffectChanged(Sender: TObject);
begin
  Changed;
end;

// 设置字体
procedure TCnFontLabel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

// 设置标签名
procedure TCnFontLabel.SetName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TCnFontLabels) and (TCnFontLabels(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(SDuplicateString); // 命名重复
  FName := Value;
  Changed;
end;

// 设置字体显示风格
procedure TCnFontLabel.SetEffect(const Value: TCnAAEffect);
begin
  FEffect.Assign(Value);
  Changed;
end;

{ TCnFontLabels }

//--------------------------------------------------------//
// 字体标签集类                                           //
//--------------------------------------------------------//

// 新增标签
function TCnFontLabels.AddItem(AName, AFontName: string; AFontSize: Integer;
  AFontColor: TColor; AFontEffect: TFontStyles; Shadow: Boolean;
  OffsetX, OffsetY: Integer): TCnFontLabel;
begin
  if IndexOf(AName) < 0 then
  begin
    Result := TCnFontLabel(Add);
    with Result do
    begin
      FName := AName;
      FFont.Name := AFontName;
      FFont.Size := AFontSize;
      FFont.Color := AFontColor;
      FFont.Style := AFontEffect;
      FEffect.Shadow.Enabled := Shadow;
      FEffect.Shadow.OffsetX := OffsetX;
      FEffect.Shadow.OffsetY := OffsetY;
    end;
  end
  else
    Result := nil;
end;

// 设置改变通知
procedure TCnFontLabels.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

// 更新
procedure TCnFontLabels.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

// 标签检查
procedure TCnFontLabels.Check(var AText: string; AFont: TFont;
  AEffect: TCnAAEffect);
var
  StartPos, Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin                                 // 查找字体标签
    StartPos := Pos('<' + UpperCase(Items[Index].Name) + '>',
      UpperCase(AText));
    if StartPos >= 1 then
    begin                               // 切换字体
      if Assigned(AFont) then
        AFont.Assign(Items[Index].Font);
      if Assigned(AEffect) then
        AEffect.Assign(Items[Index].Effect);
      system.Delete(AText, StartPos, Length(Items[Index].Name) + 2);
    end;
  end;
end;

// 初始化
constructor TCnFontLabels.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCnFontLabel);
end;

// 取子项
function TCnFontLabels.GetItem(Index: Integer): TCnFontLabel;
begin
  Result := TCnFontLabel(inherited Items[Index]);
end;

// 取索引
function TCnFontLabels.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Name, Name) = 0 then Exit;
  Result := -1;
end;

// 设置子项
procedure TCnFontLabels.SetItem(Index: Integer; const Value: TCnFontLabel);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCnUserLabel }

//--------------------------------------------------------//
// 用户标签类                                             //
//--------------------------------------------------------//

// 对象赋值方法
procedure TCnUserLabel.Assign(Source: TPersistent);
begin
  if Source is TCnUserLabel then
  begin
    FName := TCnUserLabel(Source).FName;
    FText := TCnUserLabel(Source).FText;
    FOnGetText := TCnUserLabel(Source).FOnGetText;
    FStyle := TCnUserLabel(Source).FStyle;
  end
  else
    inherited;
end;

// 变更通知
procedure TCnUserLabel.Changed;
begin
  if Assigned(UserLabels) then
    UserLabels.Changed;
end;

// 初始化
constructor TCnUserLabel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FText := '';
  FStyle := lsCustom;
  FOnGetText := nil;
end;

// 取显示名
function TCnUserLabel.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else
    Result := inherited GetDisplayName;
end;

// 取文本
function TCnUserLabel.GetText: string;
begin
  case Style of
    lsLeftJustify, lsCenter, lsRightJustify: Result := ''; // 对齐标签
    lsRegOwner: Result := TCnUserLabels(Collection).RegOwner; // 用户名
    lsRegOrganization: Result := TCnUserLabels(Collection).RegOrganization; // 组织名
    lsAppTitle: Result := Application.Title; // 应用程序标题
    lsDate: Result := DateToStr(Date);  // 当前日期
    lsTime: Result := TimeToStr(Time);  // 当前时间
  else
    Result := FText;                    // 自定义
  end;
  if Assigned(OnGetText) then
    OnGetText(Self, Result);            // 取文本事件
end;

// 取标签集
function TCnUserLabel.GetUserLabels: TCnUserLabels;
begin
  if Collection is TCnUserLabels then
    Result := TCnUserLabels(Collection)
  else
    Result := nil;
end;

// 文本是否存储
function TCnUserLabel.IsTextStored: Boolean;
begin
  Result := FStyle = lsCustom;
end;

// 设置标签名
procedure TCnUserLabel.SetName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TCnUserLabels) and (TCnUserLabels(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(SDuplicateString);
  FName := Value;
  Changed;
end;

// 设置标签风格
procedure TCnUserLabel.SetStyle(const Value: TLabelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

// 设置文本
procedure TCnUserLabel.SetText(const Value: string);
begin
  if (FStyle = lsCustom) and (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TCnUserLabels }

//--------------------------------------------------------//
// 用户标签集类                                           //
//--------------------------------------------------------//

// 初始化
constructor TCnUserLabels.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCnUserLabel);
  InitRegInfo;
end;

// 增加新标签
function TCnUserLabels.AddItem(AName, AText: string;
  AStyle: TLabelStyle): TCnUserLabel;
begin
  if IndexOf(AName) < 0 then
  begin
    Result := TCnUserLabel(Add);
    with Result do
    begin
      FName := AName;
      FText := AText;
      FStyle := AStyle;
    end;
  end
  else
    Result := nil;
end;

// 标签变动通知
procedure TCnUserLabels.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

// 标签检查
procedure TCnUserLabels.Check(var AText: string; var Align: TAlignment);
const
  csAlignArray: array[lsLeftJustify..lsRightJustify] of TAlignment =
  (taLeftJustify, taCenter, taRightJustify);
var
  StartPos, Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin                                 // 查找用户标签
    StartPos := Pos('<' + UpperCase(Items[Index].Name) + '>',
      UpperCase(AText));
    if StartPos >= 1 then
    begin                               // 删除标签
      system.Delete(AText, StartPos, Length(Items[Index].Name) + 2);
      case Items[Index].Style of        // 对齐标签
        lsLeftJustify, lsCenter, lsRightJustify:
          begin
            Align := csAlignArray[Items[Index].Style];
          end;
      else                              // 置换为文本
        system.insert(Items[Index].Text, AText, StartPos);
      end;
    end;
  end;
end;

// 更新
procedure TCnUserLabels.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

// 初始化注册表信息
procedure TCnUserLabels.InitRegInfo;
var
  V: TOSVersionInfo;
  Reg: TRegistry;
  UserName: array[0..255] of Char;
  Size: Cardinal;
  IsWin98: Boolean;
begin
  V.dwOSVersionInfoSize := SizeOf(V);
  IsWin98 := GetVersionEx(V) and (V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);
  Reg := TRegistry.Create;
  try                                   // 从注册表中读取用户名和组织名
    Reg.Rootkey := HKEY_LOCAL_MACHINE;
    if IsWin98 then
      Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion')
    else
      Reg.OpenKeyReadOnly('Software\Microsoft\Windows NT\CurrentVersion');
    RegOwner := Reg.ReadString('RegisteredOwner');
    RegOrganization := Reg.ReadString('RegisteredOrganization');
    Reg.CloseKey;
    if RegOwner = '' then
    begin
      Size := 255;
      GetUserName(UserName, Size);
      RegOwner := UserName;
    end;
  finally
    Reg.Free;
  end;
end;

// 取子项
function TCnUserLabels.GetItem(Index: Integer): TCnUserLabel;
begin
  Result := TCnUserLabel(inherited Items[Index]);
end;

// 查找索引号
function TCnUserLabels.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(Items[Result].Name, Name) = 0 then Exit;
  Result := -1;
end;

// 设置子项
procedure TCnUserLabels.SetItem(Index: Integer; const Value: TCnUserLabel);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

{ TCnPackParam }

//--------------------------------------------------------//
// 打包参数类                                             //
//--------------------------------------------------------//

// 赋值
procedure TCnPackParam.Assign(Source: TPersistent);
begin
  if Source is TCnPackParam then
    FOwner := TCnPackParam(Source).Owner
  else
    inherited;
end;

// 创建
constructor TCnPackParam.Create(AOwner: TControl);
begin
  FOwner := AOwner;
end;

{ TCnDrag }

//--------------------------------------------------------//
// 拖动打包参数类                                         //
//--------------------------------------------------------//

function TCnDrag.GetDragCursor: TCursor;
begin
  Result := TCnMyControl(FOwner).DragCursor;
end;

function TCnDrag.GetDragKind: TDragKind;
begin
  Result := TCnMyControl(FOwner).DragKind;
end;

function TCnDrag.GetDragMode: TDragMode;
begin
  Result := TCnMyControl(FOwner).DragMode;
end;

procedure TCnDrag.SetDragCursor(const Value: TCursor);
begin
  TCnMyControl(FOwner).DragCursor := Value;
end;

procedure TCnDrag.SetDragKind(const Value: TDragKind);
begin
  TCnMyControl(FOwner).DragKind := Value;
end;

procedure TCnDrag.SetDragMode(const Value: TDragMode);
begin
  TCnMyControl(FOwner).DragMode := Value;
end;

{ TCnParentEffect }

//--------------------------------------------------------//
// Parent打包参数类                                       //
//--------------------------------------------------------//

function TCnParentEffect.GetParentBiDiMode: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentBiDiMode;
end;

function TCnParentEffect.GetParentColor: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentColor;
end;

function TCnParentEffect.GetParentFont: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentFont;
end;

function TCnParentEffect.GetParentShowHint: Boolean;
begin
  Result := TCnMyControl(FOwner).ParentShowHint;
end;

procedure TCnParentEffect.SetParentBiDiMode(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentBiDiMode := Value;
end;

procedure TCnParentEffect.SetParentColor(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentColor := Value;
end;

procedure TCnParentEffect.SetParentFont(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentFont := Value;
end;

procedure TCnParentEffect.SetParentShowHint(const Value: Boolean);
begin
  TCnMyControl(FOwner).ParentShowHint := Value;
end;

{ TCnCustomParam }

//--------------------------------------------------------//
// 控件定制参数类                                         //
//--------------------------------------------------------//

// 初始化
constructor TCnCustomParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited Create(ChangedProc);
  FOwner := AOwner;
  FBackGround := TPicture.Create;
  FBackGround.OnChange := BackGroundChanged;
  FBackGroundMode := bmCenter;
  FAlignment := taLeftJustify;
  FTransparent := False;
  FLayout := tlTop;
  FWordWrap := False;
end;

// 释放
destructor TCnCustomParam.Destroy;
begin
  FBackGround.Free;
  inherited;
end;

// 赋值
procedure TCnCustomParam.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnCustomParam then
  begin
    FAlignment := TCnCustomParam(Source).Alignment;
    FWordWrap := TCnCustomParam(Source).WordWrap;
    FTransparent := TCnCustomParam(Source).Transparent;
    FLayout := TCnCustomParam(Source).Layout;
    FOwner := TCnCustomParam(Source).Owner;
    FBackGround.Assign(TCnCustomParam(Source).BackGround);
    FBackGroundMode := TCnCustomParam(Source).BackGroundMode;
  end;
end;

// 背景为空
function TCnCustomParam.IsBackEmpty: Boolean;
begin
  Result := not Assigned(FBackGround.Graphic) or
    FBackGround.Graphic.Empty;
end;

// 取背景色
function TCnCustomParam.GetColor: TColor;
begin
  Result := FOwner.Color;
end;

// 取字体
function TCnCustomParam.GetFont: TFont;
begin
  Result := FOwner.Font;
end;

// 设置背景色
procedure TCnCustomParam.SetColor(const Value: TColor);
begin
  FOwner.Color := Value;
end;

// 设置字体
procedure TCnCustomParam.SetFont(const Value: TFont);
begin
  FOwner.Font := Value;
end;

// 背景图更新
procedure TCnCustomParam.BackGroundChanged(Sender: TObject);
begin
  Changed;
end;

// 设置字体风格
function TCnCustomParam.GetFontEffect: TCnAAEffect;
begin
  Result := FOwner.FAAFont.Effect;
end;

// 取字体风格
procedure TCnCustomParam.SetFontEffect(const Value: TCnAAEffect);
begin
  FOwner.FAAFont.Effect.Assign(Value);
  Changed;
end;

// 设置垂直对齐
procedure TCnCustomParam.SetLayout(const Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

// 设置缺省对齐方式
procedure TCnCustomParam.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

// 设置自动换行
procedure TCnCustomParam.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

// 设置背景图模式
procedure TCnCustomParam.SetBackGroundMode(const Value: TBackGroundMode);
begin
  if FBackGroundMode <> Value then
  begin
    FBackGroundMode := Value;
    Changed;
  end;
end;

// 取显示精度
function TCnCustomParam.GetQuality: TAAQuality;
begin
  Result := FOwner.FAAFont.Quality;
end;

// 设置显示精度
procedure TCnCustomParam.SetQuality(const Value: TAAQuality);
begin
  if FOwner.FAAFont.Quality <> Value then
  begin
    FOwner.FAAFont.Quality := Value;
    Changed;
  end;
end;

// 设置透明
procedure TCnCustomParam.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

// 设置背景图
procedure TCnCustomParam.SetBackGround(const Value: TPicture);
begin
  FBackGround.Assign(Value);
end;

// 存储背景色
function TCnCustomParam.IsColorStroed: Boolean;
begin
  Result := not FOwner.ParentColor;
end;

{ TCnCustomTextParam }

//--------------------------------------------------------//
// 可定制的文本参数类                                     //
//--------------------------------------------------------//

// 初始化
constructor TCnCustomTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChanged;
  FFontEffect := TCnAAEffect.Create(OnChildChanged);
  FRowPitch := 20;
  FLabelEffect := leUntilNextLabel;
end;

// 释放
destructor TCnCustomTextParam.Destroy;
begin
  FLines.Free;
  FFontEffect.Free;
  inherited;
end;

// 赋值
procedure TCnCustomTextParam.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnCustomTextParam then
  begin
    FRowPitch := TCnCustomTextParam(Source).RowPitch;
    FLabelEffect := TCnCustomTextParam(Source).LabelEffect;
    FLines.Assign(TCnCustomTextParam(Source).Lines);
  end;
end;

// 文本内容是否存储
function TCnCustomTextParam.IsLinesStored: Boolean;
begin
  Result := True;
end;

// 文本内容改变
procedure TCnCustomTextParam.LinesChanged(Sender: TObject);
begin
  Changed;
end;

// 设置文本内容
procedure TCnCustomTextParam.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
  Changed;
end;

// 设置行间距
procedure TCnCustomTextParam.SetRowPitch(const Value: TRowPitch);
begin
  if FRowPitch <> Value then
  begin
    FRowPitch := Value;
    Changed;
  end;
end;

// 设置标签作用范围
procedure TCnCustomTextParam.SetLabelEffect(const Value: TLabelEffect);
begin
  if FLabelEffect <> Value then
  begin
    FLabelEffect := Value;
    Changed;
  end;
end;

// 设置默认字体风格
procedure TCnCustomTextParam.SetFontEffect(const Value: TCnAAEffect);
begin
  FFontEffect.Assign(Value);
end;

{ TCnAAGraphicControl }

//--------------------------------------------------------//
// 平滑字体控件基类                                       //
//--------------------------------------------------------//

// 初始化
constructor TCnAAGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrag := TCnDrag.Create(Self);
  FParentEffect := TCnParentEffect.Create(Self);
  FAAFont := TCnAAFontEx.Create(Canvas);
  FAAFont.Effect.OnChanged := OnEffectChanged;
{$IFNDEF COMPILER6_UP}
  FAutoSize := True;
{$ELSE}
  AutoSize := True;
{$ENDIF}
  FAutoUpdate := True;
  FBorder := 0;
  FUpdateCount := 0;
  Inited := False;
  Drawing := False;
  AHeight := 0;
  AWidth := 0;
end;

// 释放
destructor TCnAAGraphicControl.Destroy;
begin
  FAAFont.Free;
  FDrag.Free;
  FParentEffect.Free;
  inherited;
end;

// 参数改变
procedure TCnAAGraphicControl.Changed;
begin
  if Inited and AutoUpdate and ([csLoading, csDestroying, csReading,
    csUpdating, csWriting] * ComponentState = []) and (FUpdateCount
    = 0) then
    Reset;
end;

// 重新设置
procedure TCnAAGraphicControl.Reset;
begin
  Invalidate;
end;

// 运行时属性已装载
procedure TCnAAGraphicControl.Loaded;
begin
  inherited;
  Inited := True;
  LoadedEx;
end;

// 属性已装载（设计时和运行时）供重载用
procedure TCnAAGraphicControl.LoadedEx;
begin
  Changed;
end;

// 控件重绘
procedure TCnAAGraphicControl.Paint;
begin
  if [csLoading, csDestroying, csReading, csUpdating, csWriting]
    * ComponentState <> [] then
    Exit;
  if not Inited then
  begin
    Inited := True;
    LoadedEx;
  end;
  if not Visible and not (csDesigning in ComponentState) then
    Exit;
  if Drawing then
    Exit;
  Drawing := True;
  PaintCanvas;
  Drawing := False;
end;

// 重绘画布（子控件中重载该方法输出）
procedure TCnAAGraphicControl.PaintCanvas;
begin

end;

// 消息处理
procedure TCnAAGraphicControl.WndProc(var message: TMessage);
begin
  case message.Msg of
    CM_COLORCHANGED, CM_TEXTCHANGED, CM_FONTCHANGED:
      Changed;
    CM_MOUSEENTER:
      if Assigned(FOnMouseEnter) then
        FOnMouseEnter(Self);
    CM_MOUSELEAVE:
      if Assigned(FOnMouseLeave) then
        FOnMouseLeave(Self);
  end;
  inherited;
end;

// 大小变化
procedure TCnAAGraphicControl.Resize;
begin
  if (Height <> AHeight) or (Width <> AWidth) then
  begin
    AHeight := Height;
    AWidth := Width;
    Changed;
  end;
  inherited Resize;
end;

// 字体风格变化
procedure TCnAAGraphicControl.OnEffectChanged(Sender: TObject);
begin
  Changed;
end;

// 设置自动大小
{$IFDEF COMPILER6_UP}
procedure TCnAAGraphicControl.SetAutoSize(Value: Boolean);
begin
  inherited;
  Changed;
end;

{$ELSE}
procedure TCnAAGraphicControl.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;
{$ENDIF}

// 设置边界宽度
procedure TCnAAGraphicControl.SetBorder(const Value: TBorderWidth);
begin
  if FBorder <> Value then
  begin
    FBorder := Value;
    Changed;
  end;
end;

// 设置拖动属性
procedure TCnAAGraphicControl.SetDrag(const Value: TCnDrag);
begin
  FDrag.Assign(Value);
end;

// 设置属性
procedure TCnAAGraphicControl.SetParentEffect(const Value: TCnParentEffect);
begin
  FParentEffect.Assign(Value);
end;

// 设置自动更新
procedure TCnAAGraphicControl.SetAutoUpdate(const Value: Boolean);
begin
  if FAutoUpdate <> Value then
  begin
    FAutoUpdate := Value;
    if FAutoUpdate and (csDesigning in ComponentState) then
      Changed;
  end;
end;

// 开始更新
procedure TCnAAGraphicControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// 结束更新
procedure TCnAAGraphicControl.EndUpdate;
begin
  Dec(FUpdateCount);
end;

// 图像混合
procedure TCnAAGraphicControl.Blend(DesBmp, BkBmp, ForeBmp: TBitmap;
  AProgress: TProgress);
var
  pMem, pHot, pBlend: PByteArray;
  X, Y, I: Integer;
  Weight: Byte;
  AHeight, AWidth: Integer;
  Table: array[-255..255] of Byte;
begin
  AHeight := Min(BkBmp.Height, ForeBmp.Height);
  AWidth := Min(BkBmp.Width, ForeBmp.Width);
  DesBmp.Height := AHeight;
  DesBmp.Width := AWidth;
  if AProgress = 0 then
  begin
    DesBmp.Canvas.Draw(0, 0, BkBmp);
  end
  else if AProgress = csMaxProgress then
  begin
    DesBmp.Canvas.Draw(0, 0, ForeBmp);
  end
  else
  begin
    Weight := Round(AProgress * $FF / csMaxProgress);
    for I := Low(Table) to High(Table) do
      Table[I] := I * Weight shr 8;
    for Y := 0 to AHeight - 1 do
    begin
      pMem := BkBmp.ScanLine[Y];
      pHot := ForeBmp.ScanLine[Y];
      pBlend := DesBmp.ScanLine[Y];
      for X := 0 to AWidth * 3 - 1 do
        pBlend[X] := Table[pHot[X] - pMem[X]] + pMem[X];
    end;
  end;
end;

// 从父控件复制背景
// 这个过程来自 RxLibrary VCLUtils
procedure TCnAAGraphicControl.CopyParentImage(Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if Parent = nil then Exit;
  Count := Parent.ControlCount;
  DC := Dest.Handle;
  with Parent do
    ControlState := ControlState + [csPaintCopy];
  try
    SelfR := Bounds(Left, Top, Width, Height);
    X := -Left;
    Y := -Top;
    { Copy parent control image }
    SaveIndex := SaveDC(DC);
    try
      SetViewportOrgEx(DC, X, Y, nil);
      IntersectClipRect(DC, 0, 0, Parent.ClientWidth,
        Parent.ClientHeight);
      try
        with TCnParentControl(Parent) do
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
      if Parent.Controls[I] = Self then
        Break
      else if (Parent.Controls[I] <> nil) and
        (Parent.Controls[I] is TGraphicControl) then
      begin
        with TGraphicControl(Parent.Controls[I]) do
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
    with Parent do
      ControlState := ControlState - [csPaintCopy];
  end;
end;

// 绘制平铺图
procedure TCnAAGraphicControl.DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
var
  R, Rows, C, Cols: Integer;
begin
  if (G <> nil) and (not G.Empty) then
  begin
    Rows := ((Rect.Bottom - Rect.Top) div G.Height) + 1;
    Cols := ((Rect.Right - Rect.Left) div G.Width) + 1;
    for R := 1 to Rows do
      for C := 1 to Cols do
        Canvas.Draw(Rect.Left + (C - 1) * G.Width, Rect.Top + (R - 1) * G.Height, G);
  end;
end;

// 绘制背景图
procedure TCnAAGraphicControl.DrawBackGround(Canvas: TCanvas; Rect: TRect;
  G: TGraphic; Mode: TBackGroundMode);
var
  AStyle: TBrushStyle;
begin
  AStyle := Canvas.Brush.Style;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(Rect);
  Canvas.Brush.Style := AStyle;
  case Mode of
    bmTiled:
      DrawTiled(Canvas, Rect, G);
    bmStretched:
      StrectchDrawGraphic(Canvas, Rect, G, Canvas.Brush.Color);
    bmCenter:
      Canvas.Draw((Rect.Right + Rect.Left - G.Width) div 2,
        (Rect.Bottom + Rect.Top - G.Height) div 2, G);
    bmNormal:
      Canvas.Draw(Rect.Left, Rect.Top, G);
  end;
end;

// 自动换行
function TCnAAGraphicControl.GetWrapText(const Line, BreakStr: string; BreakChars:
  TSysCharSet; MaxCol: Integer): string;
const
  QuoteChars = ['''', '"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
  DoubleCharBreak: Boolean;
begin
  if MaxCol < 2 then MaxCol := 2;
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := ' ';
  ExistingBreak := False;
  DoubleCharBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if {$IFDEF UNICODE}CharInSet(CurChar, LeadBytes){$ELSE}CurChar in LeadBytes{$ENDIF} then
    begin
      if Col >= MaxCol then
      begin
        DoubleCharBreak := True;
        BreakPos := Pos - 1;
      end;
      Inc(Pos);
      Inc(Col);
    end
    else if CurChar = BreakStr[1] then
    begin
      if QuoteChar = ' ' then
      begin
        ExistingBreak := CompareText(BreakStr, Copy(Line, Pos, BreakLen)) = 0;
        if ExistingBreak then
        begin
          Inc(Pos, BreakLen - 1);
          BreakPos := Pos;
        end;
      end
    end
    else if {$IFDEF UNICODE}CharInSet(CurChar, BreakChars){$ELSE}CurChar in BreakChars{$ENDIF} then
    begin
      if QuoteChar = ' ' then
        BreakPos := Pos
    end
    else if {$IFDEF UNICODE}CharInSet(CurChar, QuoteChars){$ELSE}CurChar in QuoteChars{$ENDIF} then
      if CurChar = QuoteChar then
        QuoteChar := ' '
      else if QuoteChar = ' ' then
        QuoteChar := CurChar;
    Inc(Pos);
    Inc(Col);
    if (not ({$IFDEF UNICODE}CharInSet(QuoteChar, QuoteChars){$ELSE}QuoteChar in QuoteChars{$ENDIF}) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos)))) or DoubleCharBreak then
    begin
      Col := Pos - BreakPos;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not ({$IFDEF UNICODE}CharInSet(CurChar, QuoteChars){$ELSE}CurChar in QuoteChars{$ENDIF}) then
        while (Pos <= LineLen) and ({$IFDEF UNICODE}CharInSet(Line[Pos], BreakChars + [#13, #10]){$ELSE}Line[Pos] in BreakChars + [#13, #10]{$ENDIF}) do
          Inc(Pos);
      if not ExistingBreak and (Pos < LineLen) then
        Result := Result + BreakStr;
      Inc(BreakPos);
      LinePos := BreakPos;
      ExistingBreak := False;
      DoubleCharBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

// 文字自动换行
procedure TCnAAGraphicControl.WrapText(const S: string; Strs: TStrings;
  Col: Integer);
begin
  if not Assigned(Strs) then Exit;

  Strs.Clear;
  Strs.Text := GetWrapText(S, #13#10, ['.', ',', '?', '!', ' ', ';', ':',
    #9, '-'], Col);
end;

{ TCnAACustomText }

//--------------------------------------------------------//
// 平滑文本控件基类                                       //
//--------------------------------------------------------//

// 初始化
constructor TCnAACustomText.Create(AOwner: TComponent);
begin
  inherited;
  FFonts := TCnFontLabels.Create(Self);
  FFonts.OnChanged := OnLabelChanged;
  FLabels := TCnUserLabels.Create(Self);
  FLabels.OnChanged := OnLabelChanged;
  FFontsInited := False;
  FLabelsInited := False;
end;

// 创建默认字体和标签
procedure TCnAACustomText.CreateDefault;
begin
  BeginUpdate;
  try
    if (csDesigning in ComponentState) and UseDefaultLabels
      and (Fonts.Count = 0) and not FFontsInited then
    begin
      FFontsInited := True;
      CreateDefFonts;
    end;
    if (csDesigning in ComponentState) and UseDefaultLabels
      and (Labels.Count = 0) and not FLabelsInited then
    begin
      FLabelsInited := True;
      CreateDefLabels;
    end;
  finally
    EndUpdate;
  end;
end;

// 是否创建默认字体和标签
function TCnAACustomText.UseDefaultLabels: Boolean;
begin
  Result := True;
end;

// 创建默认字体
procedure TCnAACustomText.CreateDefFonts;
begin
  with Fonts do
  begin
    AddItem('Title1', '黑体', 12, clBlack, [fsBold], True, 2, 2);
    AddItem('Title2', '黑体', 9, clBlue, [fsBold], True, 1, 1);
    AddItem('Title3', '楷体_GB2312', 11, clBlue, [], True, 2, 2);
    AddItem('Text1', '楷体_GB2312', 9, clBlack, [], False, 1, 1);
    AddItem('Text2', '楷体_GB2312', 8, clTeal, [], True, 1, 1);
  end;
end;

// 创建默认标签
procedure TCnAACustomText.CreateDefLabels;
begin
  with Labels do
  begin
    AddItem('Left', '', lsLeftJustify); // 左对齐
    AddItem('Center', '', lsCenter);    // 中心对齐
    AddItem('Right', '', lsRightJustify); // 右对齐
    AddItem('Owner', '', lsRegOwner);   // 用户名
    AddItem('Organization', '', lsRegOrganization); // 组织名
    AddItem('AppTitle', '', lsAppTitle); // 应用程序标题
    AddItem('Date', '', lsDate);        // 当前日期
    AddItem('Time', '', lsTime);        // 当前时间
  end;
end;

// 属性已装载
procedure TCnAACustomText.LoadedEx;
begin
  CreateDefault;
end;

// 释放
destructor TCnAACustomText.Destroy;
begin
  FFonts.Free;
  FLabels.Free;
  inherited;
end;

// 标签变动
procedure TCnAACustomText.OnLabelChanged(Sender: TObject);
begin
  Changed;
end;

// 设置字体
procedure TCnAACustomText.SetFonts(const Value: TCnFontLabels);
begin
  FFonts.Assign(Value);
end;

// 设置用户标签
procedure TCnAACustomText.SetLabels(const Value: TCnUserLabels);
begin
  FLabels.Assign(Value);
end;

initialization
  InitGrayPal;

finalization
  DeleteObject(HGrayPal);

end.



