{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnAACtrls;
{* |<PRE>
================================================================================
* 软件名称：CnPack 控件包
* 单元名称：平滑特效字体控件单元
* 单元作者：CnPack 开发组 周劲羽 (zjy@cnpack.org)
*           移植：e- 
*           TCnAAMarqueeText：樊升 (fansheng_hx@yahoo.com.cn)
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* 备　　注：该单元实现了以下几个控件：
*           平滑特效字体标签 TCnAALabel
*           平滑特效超链接标签 TCnAALinkLabel
*           平滑特效文本控件 TCnAAText
*           平滑滚动文本控件 TCnAAScrollText
*           平滑字幕文本控件 TCnAAMarqueeText
*           平滑特效渐隐文本控件 TCnAAFadeText
* 最后更新：2015.06.15
*               修改输出名以躲过 BCB Unicode 下命名混淆的问题
*           2007.12.29
* 移植日期：2006.08.18
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnAAFont, CnTimer, ExtCtrls, StdCtrls, ShellAPI;

const
  // 暂时不使用版本号
  verCnAAFont = '';

type

{ TCnAAFontEffect }

  TCnAAFontEffect = class(TCnCustomParam)
  {* 平滑特效字体标签控件参数类}
  published
    property Transparent;
    {* 控件是否透明}
    property Layout;
    {* 文本垂直方向对齐方式}
    property Alignment;
    {* 文本水平对齐方式}
    property Quality;
    {* 平滑字体显示精度}
    property FontEffect;
    {* 平滑特效字体属性}
    property BackColor;
    {* 控件背景颜色}
    property BackGround;
    {* 控件背景图像}
    property BackGroundMode;
    {* 控件背景图像显示模式}
  end;

{ TAALabel }

  TCnAALabel = class(TCnAAGraphicControl)
  {* 平滑特效字体标签控件，用于显示单行文本，在控件的Effect属性中定义了所有与
     特效显示相关的设置。
   |<BR> 注：该控件不支持多行文本，如果需要显示多行文本，用TCnAAText来代替。
   |<BR> 在设计期，可通过双击控件来快速设置字体特效属性}
  private
    { Private declarations }
    FEffect: TCnAAFontEffect;
    MemBmp: TBitmap;
    procedure SetEffect(const Value: TCnAAFontEffect);
  protected
    { Protected declarations }
    procedure PaintCanvas; override;
    procedure Reset; override;
    procedure TransparentPaint;
    procedure DrawMem;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
  published
    { Published declarations }
    property AutoSize;
    {* 是否自动设置控件尺寸}
    property Border;
    {* 控件边界保留宽度}
    property Caption;
    {* 控件标题}
    property Font;
    {* 字体}
    property Width default 46;
    {* 控件宽度}
    property Height default 12;
    {* 控件高度}
    property Effect: TCnAAFontEffect read FEffect write SetEffect;
    {* 平滑特效字体属性}
  end;

{ TCnHotLink }

  TCnHotLink = class(TCnCustomParam)
  {* 平滑特效字体超链接标签控件超链接参数类}
  private
    FFade: Boolean;
    FUnderLine: Boolean;
    FFadeDelay: Cardinal;
    FURL: string;
    FFontEffect: TCnAAEffect;
    FColor: TColor;
    FBackColor: TColor;
    procedure SetFontEffect(const Value: TCnAAEffect);
  public
    constructor Create; reintroduce;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  published
    property Fade: Boolean read FFade write FFade default True;
    {* 是否允许淡入淡出显示}
    property FadeDelay: Cardinal read FFadeDelay write FFadeDelay
      default 600;
    {* 淡入淡出显示延时}
    property Color: TColor read FColor write FColor default clBlue;
    {* 高亮时的高亮时的字体颜色}
    property BackColor: TColor read FBackColor write FBackColor default clBtnface;
    {* 高亮时的背景颜色}
    property FontEffect: TCnAAEffect read FFontEffect write SetFontEffect;
    {* 高亮时的字体特效参数}
    property URL: string read FURL write FURL;
    {* 超链接内容或文件名，例如：
     |<PRE>
       http://www.cnpack.org      - 网页
       mailto:zjy@cnpack.org      - 邮件地址
       mailto:zjy@cnpack.org?subject=你好 - 带邮件标题的邮件地址链接
       c:\tools\anyexe.exe      - 可执行文件
       d:\aafont\readme.txt     - 文本文件等其它文件
       其它有效的超链接地址或文件名，相当于“开始”菜单中的“运行”命令
     |</PRE>}
    property UnderLine: Boolean read FUnderLine write FUnderLine
      default False;
    {* 高亮时是否显示下划线}
    property Transparent;
    {* 高亮时的透明设置}
    property BackGround;
    {* 高亮时的背景图像}
    property BackGroundMode;
    {* 高亮时的背景图像显示模式}
  end;

{ TCnAALinkLabel }

  TCnFadeStyle = (fsNone, fsIn, fsOut);

  TCnAALinkLabel = class(TCnAALabel)
  {* 平滑特效超链接标签控件，用于显示超链接，支持切换时的淡入淡出效果}
  private
    { Private declarations }
    HotBmp: TBitmap;
    BlendBmp: TBitmap;
    FadeTimer: TTimer;
    FFadeStyle: TCnFadeStyle;
    FProgress: TProgress;
    FHotLink: TCnHotLink;
    FMouseIn: Boolean;
    NewProg: Double;

    procedure OnFadeTimer(Sender: TObject);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetProgress(const Value: TProgress);
    procedure SeTCnFadeStyle(const Value: TCnFadeStyle);
    procedure SeTCnHotLink(const Value: TCnHotLink);
  protected
    { Protected declarations }
    property Progress: TProgress read FProgress write SetProgress;
    property FadeStyle: TCnFadeStyle read FFadeStyle write SeTCnFadeStyle;
    procedure DrawHot;
    procedure PaintCanvas; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure LoadedEx; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Click; override;
    {* 模拟用户点击该控件，调用HotLink的URL属性}
  published
    { Published declarations }
    property HotLink: TCnHotLink read FHotLink write SeTCnHotLink;
    {* 超链接属性}
  end;

{ TCnTextParam }

  TCnTextParam = class(TCnCustomTextParam)
  {* 平滑特效文本控件参数类}
  protected
    function IsLinesStored: Boolean; override;
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc:
      TNotifyEvent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
  published
    property WordWrap;
    {* 是否允许自动换行}
    property RowPitch;
    {* 文本行间距，单位为字体高度的百分比}
    property Lines;
    {* 文本内容属性，允许使用字体标签和用户标签来控制每一行文本的对齐方式和字体特效。
       使用标签时用一对尖括号'<'、'>'将标签名引起来，控制标签的作用范围由LabelEffect
       决定。另见文本控件的Fonts、Labels属性。}
    property Transparent;
    {* 是否允许控件透明}
    property Alignment;
    {* 默认的文本对齐方式，如果文本内有对齐标签，则由对齐标签决定。
     |<BR> 另见LabelEffect、Lines、Labels属性}
    property Quality;
    {* 平滑字显示精度}
    property FontEffect;
    {* 默认的字体特效参数，如果文本内有字体标签，则由字体标签决定。
     |<BR> 另见LabelEffect、Lines、Fonts、Font属性}
    property LabelEffect;
    {* 字体、对齐标签作用范围}
    property BackColor;
    {* 控件背景颜色}
    property BackGround;
    {* 控件背景图像}
    property BackGroundMode;
    {* 控件背景显示模式}
  end;

{ TCnAAText }

  TCnAAText = class(TCnAACustomText)
  {* 平滑特效文本控件，用于显示多行文本，通过使用标签，允许每行文本使用不同的
     对齐方式和字体特效。}
  private
    { Private declarations }
    FText: TCnTextParam;
    procedure SetText(const Value: TCnTextParam);
  protected
    { Protected declarations }
    TextBmp: TBitmap;
    procedure PaintCanvas; override;
    procedure LoadedEx; override;
    function UseDefaultLabels: Boolean; override;
    procedure CalcSize;
    procedure DrawCanvas(ACanvas: TCanvas);
    procedure CreateText;
    procedure TransparentPaint;
    procedure Reset; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
  published
    { Published declarations }
    property AutoSize;
    {* 是否自动设置控件尺寸}
    property Border;
    {* 控件边界保留宽度}
    property Font;
    {* 控件字体}
    property Width default 46;
    {* 控件宽度}
    property Height default 12;
    {* 控件高度}
    property Text: TCnTextParam read FText write SetText;
    {* 控件文本内容及显示参数}
  end;

  TCnAAScrollText = class;

{ TCnScrollTextParam }

  TCnScrollTextParam = class(TCnCustomTextParam)
  {* 平滑滚动文本控件参数类}
  private
    FFade: Boolean;
    FFadeHeight: Integer;
    FTailSpace: Integer;
    FHeadSpace: Integer;

    procedure SetFade(const Value: Boolean);
    procedure SetFadeHeight(const Value: Integer);
    procedure SetTailSpace(const Value: Integer);
    procedure SetHeadSpace(const Value: Integer);
  protected
    function IsLinesStored: Boolean; override;
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc:
    {* 类构造器}
      TNotifyEvent); override;
    destructor Destroy; override;
    {* 类析构器}
  published
    property Fade: Boolean read FFade write SetFade default True;
    {* 是否允许控件上下边界淡入淡出}
    property FadeHeight: Integer read FFadeHeight write SetFadeHeight default 10;
    {* 淡入淡出边界的高度}
    property HeadSpace: Integer read FHeadSpace write SetHeadSpace default 0;
    {* 滚动内容头部空白高度，单位为控件高度的百分比}
    property TailSpace: Integer read FTailSpace write SetTailSpace default 60;
    {* 滚动内容尾部空白高度，单位为控件高度的百分比}
    property Alignment default taCenter;
    {* 默认的文本对齐方式，如果文本内有对齐标签，则由对齐标签决定。
     |<BR> 另见LabelEffect、Lines、Labels属性}
    property RowPitch;
    {* 文本行间距，单位为字体高度的百分比}
    property WordWrap;
    {* 是否允许自动换行}
    property Lines;
    {* 文本内容属性，允许使用字体标签和用户标签来控制每一行文本的对齐方式和字体特效。
       使用标签时用一对尖括号'<'、'>'将标签名引起来，控制标签的作用范围由LabelEffect
       决定。另见文本控件的Fonts、Labels属性。}
    property Quality;
    {* 平滑字显示精度}
    property FontEffect;
    {* 默认的字体特效参数，如果文本内有字体标签，则由字体标签决定。
     |<BR> 另见LabelEffect、Lines、Fonts、Font属性}
    property LabelEffect;
    {* 字体、对齐标签作用范围}
    property Font;
    {* 默认的字体参数，如果文本内有字体标签，则由字体标签决定。
     |<BR> 另见LabelEffect、Lines、Fonts属性}
    property BackColor default clWhite;
    {* 控件背景颜色}
    property BackGround;
    {* 控件背景图像}
    property BackGroundMode default bmTiled;
    {* 控件背景显示模式}
  end;

{ TCnAAScrollText }

  TCnAAScrollText = class(TCnAACustomText)
  {* 平滑滚动文本控件，用于多行文本的动态滚动显示}
  private
    { Private declarations }
    FScrollDelay: Word;
    FScrollStep: Integer;
    FRepeatDelay: Word;
    FRepeatCount: TBorderWidth;
    FRepeatedCount: Integer;
    FText: TCnScrollTextParam;
    FCurrPos: Integer;
    TextBmp: TBitmap;
    CurrBmp: TBitmap;
    DelayTimer: TTimer;
    ScrollTimer: TCnTimer;
    FActive: Boolean;

    procedure CreateText;
    procedure OnDelayTimer(Sender: TObject);
    procedure OnScrollTimer(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetScrollDelay(const Value: Word);
    procedure SetScrollStep(const Value: Integer);
    procedure SetRepeatDelay(const Value: Word);
    procedure SetRepeatCount(const Value: TBorderWidth);
    procedure SetText(const Value: TCnScrollTextParam);
    procedure SetCurrPos(const Value: Integer);
    function GetBmpHeight: Integer;
  protected
    { Protected declarations }
    procedure CreateDefFonts; override;
    procedure PaintCanvas; override;
    function UseDefaultLabels: Boolean; override;
    procedure LoadedEx; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Reset; override;
    {* 重新创建滚动内容，用于AutoUpdate为假时，在运行期动态修改控件参数后初始化控件滚动。}
    procedure ReStart;
    {* 重新开始滚动，清滚动计数器，文本从头开始滚动}
    property RepeatedCount: Integer read FRepeatedCount;
    {* 已循环滚动次数，运行期只读属性}
    property CurrPos: Integer read FCurrPos write SetCurrPos;
    {* 当前显示的内容在整个图像中的位置，用户可用它来手动控制控件滚动}
    property BmpHeight: Integer read GetBmpHeight;
    {* 整个图象的高度}
  published
    { Published declarations }
    property AutoUpdate;
    {* 是否允许控件参数变更时自动重新创建滚动内容。如果有很多参数需要在运行时设置，
       可将该属性设为False，待设定完参数后调用Reset方法。}
    property Active: Boolean read FActive write SetActive default True;
    {* 是否允许文本滚动}
    property Height default 280;
    {* 控件高度}
    property Width default 240;
    {* 控件宽度}
    property ScrollDelay: Word read FScrollDelay write SetScrollDelay default 60;
    {* 滚动时的延时，单位为毫秒}
    property ScrollStep: Integer read FScrollStep write SetScrollStep default 1;
    {* 一次滚动的象素数，如果设定为负数将向下滚动}
    property RepeatCount: TBorderWidth read FRepeatCount write SetRepeatCount default 0;
    {* 允许循环次数，指定次数的循环结束将自动停止滚动，并产生OnComplete事件。
     |<BR> 该值设为0将无限循环。}
    property RepeatDelay: Word read FRepeatDelay write SetRepeatDelay default 2000;
    {* 完成一次滚动循环后的延时，如果不需要延时，可设为0}
    property Text: TCnScrollTextParam read FText write SetText;
    {* 滚动文本内容和参数属性}
    property OnComplete;
    {* 指定次数的滚动循环结束事件，见RepeatCount}
    property OnTextReady;
    {* 滚动内容已初始化事件}
    property OnPainted;
    {* 控件重绘事件}
  end;
  
{ TCnAAMarqueeText }

  TCnAAMarqueeText = class(TCnAAGraphicControl)
   {* 平滑字幕文本控件，用于文本的水平滚动显示}
  private
    {* 滚动类型 }
    FScrollType: THoriScrollType;
    FActive: Boolean;
    FSteps: Integer;
    FScrollStep: Word;
    FCurrentStep: Integer;
    FTextWidth: Integer;
    FScrollDelay: Word;
    FTimer: TTimer;
    FEffect: TCnAAFontEffect;
    FOnPainted: TNotifyEvent;
  protected
    procedure SetScrollType(Value: THoriScrollType);
    procedure SetActive(Value: Boolean );
    procedure SetScrollStep(Value: Word );
    procedure SetScrollDelay(Value: Word );
    procedure OnTimer(Sender: TObject);
    procedure SetEffect(const Value: TCnAAFontEffect);

    procedure PaintCanvas; override;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Reset; override;
    {* 重新创建滚动内容，用于滚动发生变化时重新设置滚动参数。}
  published  
    property Active: Boolean read FActive write SetActive default False;
    {* 是否允许文本淡入淡出切换} 
    property Height default 34;
    {* 控件高度}
    property Width default 240;
    {* 控件宽度}
    property Font;
    {* 控件字体}
    property Caption;
    {* 控件标题}
    property AutoSize;
    {* 自适用大小}
    property Effect: TCnAAFontEffect read FEffect write SetEffect;
    {* 平滑特效字体属性}
    property ScrollType: THoriScrollType read FScrollType write SetScrollType;
    {* 水平滚动类型}
    property ScrollStep: Word read FScrollStep write SetScrollStep;
    {* 水平滚动步长}
    property ScrollDelay: Word read FScrollDelay write SetScrollDelay;
    {* 滚动时间间隔}
    property OnPainted: TNotifyEvent read FOnPainted write FOnPainted;
    {* 控件重绘事件}
  end;

{ TCnFadeTextParam }

  TCnFadeTextParam = class(TCnCustomTextParam)
  {* 平滑特效渐隐文本控件参数类}
  private
    FFadeDelay: Cardinal;
    procedure SetFadeDelay(const Value: Cardinal);
    procedure SetLineDelay(const Value: Cardinal);
    function GetLineDelay: Cardinal;
  protected
    function IsLinesStored: Boolean; override;
  public
    constructor Create(AOwner: TCnAAGraphicControl; ChangedProc:
      TNotifyEvent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
  published
    property FadeDelay: Cardinal read FFadeDelay write SetFadeDelay default 600;
    {* 文本淡入淡出切换延时}
    property LineDelay: Cardinal read GetLineDelay write SetLineDelay default 3000;
    {* 每行文本显示延时}
    property Lines;
    {* 文本内容属性，允许使用字体标签和用户标签来控制每一行文本的对齐方式和字体特效。
       使用标签时用一对尖括号'<'、'>'将标签名引起来，控制标签的作用范围由LabelEffect
       决定。另见文本控件的Fonts、Labels属性。}
    property Transparent;
    {* 是否允许控件透明}
    property Alignment default taCenter;
    {* 默认的文本对齐方式，如果文本内有对齐标签，则由对齐标签决定。
     |<BR> 另见LabelEffect、Lines、Labels属性}
    property Layout default tlCenter;
    {* 文本垂直方向对齐方式}
    property Quality;
    {* 平滑字显示精度}
    property FontEffect;
    {* 默认的字体特效参数，如果文本内有字体标签，则由字体标签决定。
     |<BR> 另见LabelEffect、Lines、Fonts、Font属性}
    property LabelEffect;
    {* 字体、对齐标签作用范围}
    property BackColor default clWhite;
    {* 控件背景颜色}
    property BackGround;
    {* 控件背景图像}
    property BackGroundMode;
    {* 控件背景显示模式}
  end;

{ TCnAAFadeText }

  TCnAAFadeText = class(TCnAACustomText)
  {* 平滑特效渐隐文本控件，用于多行文本的淡入淡出切换显示}
  private
    { Private declarations }
    FActive: Boolean;
    FLineIndex: Integer;
    FText: TCnFadeTextParam;
    FFadeProgress: TProgress;
    InBmp, OutBmp, TextBmp: TBitmap;
    FadeTimer: TTimer;
    DelayTimer: TTimer;
    LastText: string;
    CurrText: string;
    CurrAlign: TAlignment;
    FRepeatedCount: Integer;
    FRepeatCount: TBorderWidth;
    NewProg: Double;

    procedure SetActive(const Value: Boolean);
    procedure SetLineIndex(const Value: Integer);
    procedure SetText(const Value: TCnFadeTextParam);
    procedure OnFadeTimer(Sender: TObject);
    procedure OnDelayTimer(Sender: TObject);
    procedure SetFadeProgress(const Value: TProgress);
    procedure DrawFadeBmp(AText: string; Bmp: TBitmap);
    procedure SetRepeatCount(const Value: TBorderWidth);
  protected
    { Protected declarations }
    procedure CreateDefFonts; override;
    procedure PaintCanvas; override;
    function UseDefaultLabels: Boolean; override;
    procedure LoadedEx; override;
    procedure Reset; override;
    property FadeProgress: TProgress read FFadeProgress write SetFadeProgress;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    property LineIndex: Integer read FLineIndex write SetLineIndex;
    {* 当前显示的行索引号，用户可手动设置}
    property RepeatedCount: Integer read FRepeatedCount;
    {* 已循环滚动次数，运行期只读属性}
    procedure FadeTo(Line: Integer);
    {* 淡入淡出切换到指定行}
    procedure FadeToNext;
    {* 淡入淡出切换到下一行}
    procedure FadeToStr(AText: string);
    {* 淡入淡出切换到指定文本}
  published
    { Published declarations }
    property Active: Boolean read FActive write SetActive default True;
    {* 是否允许文本淡入淡出切换}
    property Height default 34;
    {* 控件高度}
    property Width default 240;
    {* 控件宽度}
    property Font;
    {* 控件字体}
    property RepeatCount: TBorderWidth read FRepeatCount write SetRepeatCount default 0;
    {* 允许循环次数，指定次数的循环结束将自动停止滚动，并产生 OnComplete 事件。
     |<BR> 该值设为 0 将无限循环。}
    property Text: TCnFadeTextParam read FText write SetText;
    {* 控件文本内容和参数属性}
    property OnComplete;
    {* 指定次数的滚动循环结束事件，见RepeatCount}
    property OnPainted;
    {* 控件重绘事件}
  end;

implementation

{$R-}
{$OVERFLOWCHECKS OFF}

const
  csAACopyRight =
    '<Title2>版权声明'#13#10 +
    '<Text1>本控件为免费控件'#13#10 +
    '允许免费用于共享、商业软件中'#13#10 +
    '更多说明参见帮助文件'#13#10 +
    '如发现错误请与作者联系'#13#10#13#10 +

  '<Title2>控件作者'#13#10 +
    '<Text1>作者：周劲羽'#13#10 +
    'Email：zjy@cnpack.org'#13#10 +
    'Http://www.cnpack.org'#13#10 +
    'CnPack 开发组'#13#10;

  csAACopyRightStart =
    #13#10'<Title2>用户资料'#13#10 +
    '<Text1><Owner>'#13#10 +
    '<Organization>'#13#10#13#10 +

  '<Title2>控件功能'#13#10;

  csAACopyRightEnd =
    '允许使用不同风格的字体'#13#10 +
    '和对齐方式'#13#10 +
    '支持阴影、渐变色、纹理等特效'#13#10 +
    '提供多个系统变量并'#13#10 +
    '允许自定义变量'#13#10 +
    '所有字体采用平滑显示'#13#10#13#10 +

  '<Title2>使用说明'#13#10 +
    '<Text1>控件的属性、方法、事件'#13#10 +
    '详见帮助文件'#13#10#13#10 +

  '<Title2>特别感谢'#13#10 +
    '<Text1>李文松朋友提供'#13#10 +
    '平滑字体显示算法'#13#10 +
    'liwensong@hotmail.com'#13#10 +
    'http://member.netease.com/~lws'#13#10 +
    'Passion兄帮助制作控件图标'#13#10 +
    'liuxiao@cnpack.org'#13#10#13#10 +

  '<Title2>备注'#13#10 +
    '<Text1>该控件为免费控件'#13#10 +
    '如果您对这个控件还感满意'#13#10 +
    '请给作者发一封贺卡或邮件'#13#10 +
    '以示支持'#13#10#13#10#13#10 +

  '<Title3>CnPack 开发组'#13#10 +
    '2006.08'#13#10;

  csAATextCopyRight =
    '<Title1><Center>平滑特效文本控件 ' + verCnAAFont + #13#10#13#10 +
    csAACopyRight;

  csAAFadeTextCopyRight =
    '<Title1><Center>平滑特效渐隐文本控件 ' + verCnAAFont + #13#10#13#10 +
    csAACopyRight + csAACopyRightStart +
    '<Text1>用于显示淡入淡出文本'#13#10 +
    csAACopyRightEnd;

  csAAScrollTextCopyRight =
    '<Title1>平滑滚动文本控件 ' + verCnAAFont + #13#10#13#10 +
    csAACopyRight + csAACopyRightStart +
    '<Text1>用于显示滚动文本信息'#13#10 +
    csAACopyRightEnd;

{ TCnAALabel }

//--------------------------------------------------------//
//平滑特效字体标签                                        //
//--------------------------------------------------------//

//初始化
constructor TCnAALabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MemBmp := TBitmap.Create;
  MemBmp.PixelFormat := pf24bit;
  FEffect := TCnAAFontEffect.Create(Self, OnEffectChanged);
  ControlStyle := ControlStyle + [csReplicatable, csSetCaption];
  Width := 46;
  Height := 12;
end;

//释放
destructor TCnAALabel.Destroy;
begin
  FEffect.Free;
  MemBmp.Free;
  inherited;
end;

//重绘
procedure TCnAALabel.Reset;
begin
  if not Effect.Transparent then
    DrawMem;
  inherited;
end;

//绘制缓冲区
procedure TCnAALabel.DrawMem;
var
  OffPoint: TPoint;
  th, tw: Integer;
begin
  AAFont.Canvas := MemBmp.Canvas;
  MemBmp.Canvas.Font.Assign(Font); //字体
  th := AAFont.TextHeight(Caption); //文本高度
  tw := AAFont.TextWidth(Caption); //文本宽度
  //自动设定大小
  if AutoSize and (Align in [alNone, alLeft, alRight]) then
    ClientWidth := tw + 2 * Border;
  if AutoSize and (Align in [alNone, alTop, alBottom]) then
    ClientHeight := th + 2 * Border;
  case Effect.Alignment of    //水平对齐方式
    taLeftJustify: OffPoint.x := Border;
    taCenter: OffPoint.x := (ClientWidth - tw) div 2;
    taRightJustify: OffPoint.x := ClientWidth - Border - tw;
  end;
  case Effect.Layout of       //垂直对齐方式
    tlTop: OffPoint.y := Border;
    tlCenter: OffPoint.y := (ClientHeight - th) div 2;
    tlBottom: OffPoint.y := ClientHeight - Border - th;
  end;
  MemBmp.Height := ClientHeight;
  MemBmp.Width := ClientWidth;
  MemBmp.Canvas.Brush.Color := Color;
  MemBmp.Canvas.Brush.Style := bsSolid;
  if Effect.Transparent then  //透明
  begin
    CopyParentImage(MemBmp.Canvas); //复制父控件画布
  end else if not Effect.IsBackEmpty then
  begin                       //绘制背景图
    DrawBackGround(MemBmp.Canvas, Rect(0, 0, MemBmp.Width, MemBmp.Height),
      Effect.BackGround.Graphic, Effect.BackGroundMode);
  end else
  begin                       //填充背景色
    MemBmp.Canvas.FillRect(ClientRect);
  end;
  MemBmp.Canvas.Brush.Style := bsClear;
  AAFont.TextOutput(OffPoint.x, OffPoint.y, Caption); //平滑字体输出
end;

// 透明绘制
procedure TCnAALabel.TransparentPaint;
var
  OffPoint: TPoint;
  th, tw: Integer;
begin
  AAFont.Canvas := Canvas;
  Canvas.Font.Assign(Font); //字体
  th := AAFont.TextHeight(Caption); //文本高度
  tw := AAFont.TextWidth(Caption); //文本宽度
  //自动设定大小
  if AutoSize and (Align in [alNone, alLeft, alRight]) then
    ClientWidth := tw + 2 * Border;
  if AutoSize and (Align in [alNone, alTop, alBottom]) then
    ClientHeight := th + 2 * Border;
  case Effect.Alignment of    //水平对齐方式
    taLeftJustify: OffPoint.x := Border;
    taCenter: OffPoint.x := (ClientWidth - tw) div 2;
    taRightJustify: OffPoint.x := ClientWidth - Border - tw;
  end;
  case Effect.Layout of       //垂直对齐方式
    tlTop: OffPoint.y := Border;
    tlCenter: OffPoint.y := (ClientHeight - th) div 2;
    tlBottom: OffPoint.y := ClientHeight - Border - th;
  end;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Style := bsClear;
  AAFont.TextOutput(OffPoint.x, OffPoint.y, Caption); //平滑字体输出
end;

//控件重绘
procedure TCnAALabel.PaintCanvas;
begin
  if Effect.Transparent then
    TransparentPaint
  else
    Bitblt(Canvas.Handle, 0, 0, Width, Height, MemBmp.Canvas.Handle, 0, 0,
      SRCCOPY);
end;

//设置字体特效
procedure TCnAALabel.SetEffect(const Value: TCnAAFontEffect);
begin
  FEffect.Assign(Value);
end;

{ TCnHotLink }

//--------------------------------------------------------//
//超链接参数类                                            //
//--------------------------------------------------------//

//链接参数
procedure TCnHotLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnHotLink then
  begin
    FFade := TCnHotLink(Source).Fade;
    FUnderLine := TCnHotLink(Source).UnderLine;
    FFadeDelay := TCnHotLink(Source).FadeDelay;
    FURL := TCnHotLink(Source).URL;
    FColor := TCnHotLink(Source).Color;
    FBackColor := TCnHotLink(Source).BackColor;
    FFontEffect.Assign(TCnHotLink(Source).FontEffect);
  end;
end;

//初始化
constructor TCnHotLink.Create;
begin
  inherited Create(nil, nil);
  FFade := True;
  FUnderLine := False;
  FFadeDelay := 600;
  FURL := '';
  FColor := clBlue;
  FBackColor := clBtnface;
  FFontEffect := TCnAAEffect.Create(nil);
end;

//释放
destructor TCnHotLink.Destroy;
begin
  FFontEffect.Free;
  inherited;
end;

procedure TCnHotLink.SetFontEffect(const Value: TCnAAEffect);
begin
  FFontEffect.Assign(Value);
  Changed;
end;

{ TCnAALinkLabel }

//--------------------------------------------------------//
//平滑特效超链接标签                                      //
//--------------------------------------------------------//

//初始化
constructor TCnAALinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHotLink := TCnHotLink.Create;
  HotBmp := TBitmap.Create;
  HotBmp.PixelFormat := pf24bit;
  BlendBmp := TBitmap.Create;
  BlendBmp.PixelFormat := pf24bit;
  FadeTimer := TTimer.Create(Self);
  FadeTimer.Interval := 55;
  FadeTimer.OnTimer := OnFadeTimer;
  FadeTimer.Enabled := False;
  FProgress := 0;
  FFadeStyle := fsNone;
  NewProg := 0;
end;

//释放
destructor TCnAALinkLabel.Destroy;
begin
  HotBmp.Free;
  BlendBmp.Free;
  FadeTimer.Free;
  HotLink.Free;
  inherited;
end;

//绘制画布
procedure TCnAALinkLabel.PaintCanvas;
begin
  if FMouseIn or (FadeStyle <> fsNone) then
    Bitblt(Canvas.Handle, 0, 0, Width, Height, BlendBmp.Canvas.Handle, 0, 0,
      SRCCOPY)
  else
    inherited;
end;

//淡入淡出
procedure TCnAALinkLabel.OnFadeTimer(Sender: TObject);
begin
  if Abs(NewProg - Progress) > 1 then
    NewProg := Progress;
  case FadeStyle of
    fsIn: begin               //淡入
        NewProg := NewProg + csMaxProgress * FadeTimer.Interval div HotLink.FadeDelay;
        if NewProg > csMaxProgress then
        begin
          NewProg := csMaxProgress;
          FadeStyle := fsNone;
        end;
        Progress := Round(NewProg);
      end;
    fsOut: begin              //淡出
        NewProg := NewProg - csMaxProgress * FadeTimer.Interval div HotLink.FadeDelay;
        if NewProg < 0 then
        begin
          NewProg := 0;
          FadeStyle := fsNone;
        end;
        Progress := Round(NewProg);
      end;
    fsNone: begin             //无
        FadeTimer.Enabled := False;
      end;
  end;
end;

//绘制热点画布
procedure TCnAALinkLabel.DrawHot;
var
  OffPoint: TPoint;
  th, tw: Integer;
  AAEffect: TCnAAEffect;
begin
  BeginUpdate;
  try
    AAEffect := TCnAAEffect.Create(nil);
    AAEffect.Assign(AAFont.Effect);

    AAFont.Canvas := HotBmp.Canvas;
    AAFont.Effect.Assign(HotLink.FontEffect);
    HotBmp.Canvas.Font.Assign(Font); //字体
    HotBmp.Canvas.Font.Color := HotLink.Color;
    if HotLink.UnderLine then
      HotBmp.Canvas.Font.Style := HotBmp.Canvas.Font.Style + [fsUnderline];
    th := AAFont.TextHeight(Caption); //文本高度
    tw := AAFont.TextWidth(Caption); //文本宽度
    if AutoSize and (Align = alNone) then //自动设定大小
    begin
      OffPoint := Point(Border, Border);
    end else begin
      case Effect.Alignment of //水平对齐方式
        taLeftJustify: OffPoint.x := Border;
        taCenter: OffPoint.x := (ClientWidth - tw) div 2;
        taRightJustify: OffPoint.x := ClientWidth - Border - tw;
      end;
      case Effect.Layout of   //垂直对齐方式
        tlTop: OffPoint.y := Border;
        tlCenter: OffPoint.y := (ClientHeight - th) div 2;
        tlBottom: OffPoint.y := ClientHeight - Border - th;
      end;
    end;
    HotBmp.Height := ClientHeight;
    HotBmp.Width := ClientWidth;
    HotBmp.Canvas.Brush.Color := HotLink.BackColor;
    HotBmp.Canvas.Brush.Style := bsSolid;
    if HotLink.Transparent then
    begin
      CopyParentImage(HotBmp.Canvas);
    end else if not HotLink.IsBackEmpty then
    begin
      DrawBackGround(HotBmp.Canvas, Rect(0, 0, HotBmp.Width, HotBmp.Height),
        HotLink.BackGround.Graphic, HotLink.BackGroundMode);
    end else
    begin
      HotBmp.Canvas.FillRect(ClientRect);
    end;
    HotBmp.Canvas.Brush.Style := bsClear;
    AAFont.TextOutput(OffPoint.x, OffPoint.y, Caption); //平滑字体输出

    AAFont.Effect.Assign(AAEffect);
    AAEffect.Free;
  finally
    EndUpdate;
  end;
end;

//鼠标移入开始淡入
procedure TCnAALinkLabel.CMMouseEnter(var Message: TMessage);
begin
  if Enabled then
  begin
    FMouseIn := True;
    DrawMem;
    DrawHot;
    if HotLink.Fade then
    begin
      FadeStyle := fsIn;
    end else
      Progress := csMaxProgress;
  end;
  inherited;
end;

//鼠标称出开始淡出
procedure TCnAALinkLabel.CMMouseLeave(var Message: TMessage);
begin
  if Enabled then
  begin
    if HotLink.Fade then
    begin
      FadeStyle := fsOut;
    end else
      Progress := 0;
    FMouseIn := False;
  end;
  inherited;
end;

//点击控件
procedure TCnAALinkLabel.Click;
var
  Wnd: THandle;
begin
  if HotLink.URL <> EmptyStr then
  begin
    if Parent is TForm then
      Wnd := Parent.Handle
    else
      Wnd := 0;               //NULL;
    ShellExecute(Wnd, nil, PChar(HotLink.URL), nil, nil, SW_SHOWNORMAL);
  end;
  inherited;
end;

//属性已装载
procedure TCnAALinkLabel.LoadedEx;
begin
  inherited;
  Reset;
end;

//设置淡入淡出进度
procedure TCnAALinkLabel.SetProgress(const Value: TProgress);
begin
  if FProgress <> Value then
  begin
    FProgress := Value;
    Blend(BlendBmp, MemBmp, HotBmp, Progress);
    Paint;
  end;
end;

//设置启用
procedure TCnAALinkLabel.SetEnabled(Value: Boolean);
begin
  inherited;
  if not Value then
  begin
    FadeStyle := fsNone;
    Progress := 0;
  end;
end;

//设置淡入淡出
procedure TCnAALinkLabel.SeTCnFadeStyle(const Value: TCnFadeStyle);
begin
  if FFadeStyle <> Value then
  begin
    FFadeStyle := Value;
    FadeTimer.Enabled := FFadeStyle <> fsNone;
  end;
end;

//设置链接参数
procedure TCnAALinkLabel.SeTCnHotLink(const Value: TCnHotLink);
begin
  FHotLink.Assign(Value);
end;

{ TCnAAText }

//--------------------------------------------------------//
//平滑特效超链接标签                                      //
//--------------------------------------------------------//

//调整尺寸
procedure TCnAAText.CalcSize;
var
  i, j: Integer;
  DispLines: TStrings;
  WrapLines: TStrings;
  CurrText: string;
  CurrAlign: TAlignment;
  TextWidth: Integer;
  TextHeight: Integer;
  AWidth, AHeight: Integer;
  xFree, yFree: Boolean;
  MaxCol: Integer;
begin
  BeginUpdate;
  DispLines := nil;
  WrapLines := nil;
  try
    DispLines := TStringList.Create; //临时文本
    WrapLines := TStringList.Create;
    with FText do
    begin
      xFree := not WordWrap and AutoSize and (Align in [alNone, alLeft, alRight]);
      yFree := AutoSize and (Align in [alNone, alTop, alBottom]);
      if xFree then AWidth := 0
      else AWidth := ClientWidth;
      if yFree then AHeight := 0
      else AHeight := ClientHeight;
      if xFree or yFree then
      begin
        DispLines.Clear;
        DispLines.AddStrings(Lines);
        AAFont.Canvas := Canvas;
        AAFont.Effect.Assign(FText.FontEffect);
        Canvas.Font.Assign(Font);
        for i := 0 to DispLines.Count - 1 do
        begin
          CurrText := DispLines[i]; //当前处理字符串
          if LabelEffect = leOnlyALine then
          begin
            Canvas.Font.Assign(Font);
            AAFont.Effect.Assign(FText.FontEffect);
          end;
          Fonts.Check(CurrText, Canvas.Font, AAFont.Effect); //检查字体标签
          Labels.Check(CurrText, CurrAlign); //检查用户标签
          TextWidth := AAFont.TextWidth(CurrText);
          if WordWrap and (TextWidth > AWidth) then //自动换行
          begin
            MaxCol := AWidth * Length(CurrText) div TextWidth;
            while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > AWidth do
              Dec(MaxCol);
            WrapText(CurrText, WrapLines, MaxCol);
          end else if CurrText <> '' then
            WrapLines.Text := CurrText
          else
            WrapLines.Text := ' ';
          if xFree and (TextWidth > AWidth) then //确定宽度
          begin
            AWidth := TextWidth;
          end;
          if yFree then       //确定高度
          begin
            for j := 0 to WrapLines.Count - 1 do
            begin
              CurrText := WrapLines[j];
              TextHeight := AAFont.TextHeight(CurrText + ' ');
              Inc(AHeight, TextHeight);
              if (i < DispLines.Count - 1) or (j < WrapLines.Count - 1) then
                Inc(AHeight, Round(TextHeight * RowPitch / 100));
            end;
          end;
        end;
        if xFree then ClientWidth := AWidth + 2 * Border;
        if yFree then ClientHeight := AHeight + 2 * Border;
      end;
    end;
  finally
    DispLines.Free;
    WrapLines.Free;
    EndUpdate;
  end;
end;

//创建
constructor TCnAAText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FText := TCnTextParam.Create(Self, OnLabelChanged);
  TextBmp := TBitmap.Create;
  TextBmp.PixelFormat := pf24bit;
  Width := 46;
  Height := 12;
end;

//创建显示文本
procedure TCnAAText.CreateText;
begin
  CalcSize;
  TextBmp.Canvas.Brush.Color := Color;
  TextBmp.Canvas.Brush.Style := bsSolid;
  TextBmp.Width := ClientWidth;
  TextBmp.Height := ClientHeight;
  if FText.Transparent then     //透明
  begin
    CopyParentImage(TextBmp.Canvas); //复制父控件画布
  end else if not FText.IsBackEmpty then
  begin                   //绘制背景图
    DrawBackGround(TextBmp.Canvas, Rect(0, 0, TextBmp.Width, TextBmp.Height),
      FText.BackGround.Graphic, FText.BackGroundMode);
  end else
  begin                   //填充背景色
    TextBmp.Canvas.FillRect(ClientRect);
  end;
  TextBmp.Canvas.Brush.Style := bsClear;
  DrawCanvas(TextBmp.Canvas);
end;

//释放
destructor TCnAAText.Destroy;
begin
  TextBmp.Free;
  FText.Free;
  inherited;
end;

//绘制
procedure TCnAAText.DrawCanvas(ACanvas: TCanvas);
var
  i, j: Integer;
  DispLines: TStrings;
  WrapLines: TStrings;
  CurrText: string;
  CurrAlign: TAlignment;
  x, y: Integer;
  TextWidth: Integer;
  TextHeight: Integer;
  MaxCol: Integer;
begin
  BeginUpdate;
  DispLines := nil;
  WrapLines := nil;
  try
    DispLines := TStringList.Create; //临时文本
    WrapLines := TStringList.Create;
    with FText do
    begin
      DispLines.AddStrings(Lines);
      ACanvas.Brush.Color := Color;
      ACanvas.Brush.Style := bsClear;
      ACanvas.Font.Assign(Font);
      AAFont.Canvas := ACanvas;
      AAFont.Effect.Assign(FText.FontEffect);
      CurrAlign := Alignment; //默认对齐方式
      y := Border;
      for i := 0 to DispLines.Count - 1 do
      begin
        if y > ClientHeight - Border then
          Break;
        CurrText := DispLines[i]; //当前处理字符串
        if LabelEffect = leOnlyALine then
        begin
          ACanvas.Font.Assign(Font);
          AAFont.Effect.Assign(FText.FontEffect);
          CurrAlign := Alignment;
        end;
        Fonts.Check(CurrText, ACanvas.Font, AAFont.Effect); //检查字体标签
        Labels.Check(CurrText, CurrAlign); //检查用户标签
        TextWidth := AAFont.TextWidth(CurrText);
        if WordWrap and (TextWidth > ClientWidth - 2 * Border) then //自动换行
        begin
          MaxCol := (ClientWidth - 2 * Border) * Length(CurrText) div TextWidth;
          while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > ClientWidth - 2
            * Border do
            Dec(MaxCol);
          WrapText(CurrText, WrapLines, MaxCol);
        end else if CurrText <> '' then
          WrapLines.Text := CurrText
        else
          WrapLines.Text := ' ';
        for j := 0 to WrapLines.Count - 1 do
        begin
          CurrText := WrapLines[j];
          TextHeight := AAFont.TextHeight(CurrText + ' ');
          TextWidth := AAFont.TextWidth(CurrText);
          case CurrAlign of   //对齐方式
            taLeftJustify: x := Border;
            taCenter: x := (ClientWidth - TextWidth) div 2;
            taRightJustify: x := ClientWidth - Border - TextWidth;
          else x := 0;
          end;
          AAFont.TextOutput(x, y, CurrText);
          y := y + Round(TextHeight * (1 + RowPitch / 100));
        end;
      end;
      AAFont.Effect.Assign(FText.FontEffect);
    end;
  finally
    DispLines.Free;
    WrapLines.Free;
    EndUpdate;
  end;
end;

//控件属性已装载
procedure TCnAAText.LoadedEx;
begin
  inherited;
  Reset;
end;

//绘制画布
procedure TCnAAText.PaintCanvas;
begin
  if Text.Transparent then
    TransparentPaint    //透明
  else
    Bitblt(Canvas.Handle, 0, 0, Width, Height, TextBmp.Canvas.Handle, 0, 0,
      SRCCOPY);
end;

//复位
procedure TCnAAText.Reset;
begin
  if not Text.Transparent then
    CreateText;
  inherited;
end;

//设置文本
procedure TCnAAText.SetText(const Value: TCnTextParam);
begin
  Text.Assign(Value);
end;

//透明绘制
procedure TCnAAText.TransparentPaint;
begin
  CalcSize;
  DrawCanvas(Canvas);
end;

//默认文本创建默认标签
function TCnAAText.UseDefaultLabels: Boolean;
begin
  Result := not FText.IsLinesStored;
end;

{ TCnTextParam }

//--------------------------------------------------------//
//平滑文本参数类                                          //
//--------------------------------------------------------//

//创建
constructor TCnTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  Lines.Text := csAATextCopyRight;
end;

//释放
destructor TCnTextParam.Destroy;
begin
  inherited;
end;

//文本存储
function TCnTextParam.IsLinesStored: Boolean;
begin
  Result := Lines.Text <> csAATextCopyRight;
end;

{ TCnAAScrollText }

//--------------------------------------------------------//
//平滑滚动文本控件                                        //
//--------------------------------------------------------//

//控件初始化
constructor TCnAAScrollText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque]; //由控件绘制所有客户区
  FText := TCnScrollTextParam.Create(Self, OnLabelChanged);
  TextBmp := TBitmap.Create;
  TextBmp.PixelFormat := pf24bit;
  CurrBmp := TBitmap.Create;
  CurrBmp.PixelFormat := pf24bit;
  ScrollTimer := TCnTimer.Create(Self);
  ScrollTimer.Enabled := False;
  ScrollTimer.OnTimer := OnScrollTimer;
  DelayTimer := TTimer.Create(Self);
  DelayTimer.Enabled := False;
  DelayTimer.OnTimer := OnDelayTimer;
  FCurrPos := 0;
  FRepeatCount := 0;
  FActive := True;
  RepeatDelay := 2000;
  ScrollStep := 1;
  ScrollDelay := 60;
  Color := clWhite;
  SetBounds(0, 0, 240, 280);
end;

//释放
destructor TCnAAScrollText.Destroy;
begin
  Active := False;
  ScrollTimer.Free;
  DelayTimer.Free;
  TextBmp.Free;
  CurrBmp.Free;
  FText.Free;
  inherited;
end;

//显示文本复位
procedure TCnAAScrollText.Reset;
var
  tActive: Boolean;
begin
  tActive := Active;
  FRepeatedCount := -1;
  Active := False;
  CreateText;
  FCurrPos := 0;
  Paint;
  Active := tActive;
end;

//绘制控件
procedure TCnAAScrollText.PaintCanvas;
var
  i: Integer;
  BkRed, BkGreen, BkBlue: Byte;
  tBkColor: TColor;

  //透明混合
  procedure DrawFade(y: Integer; Transparency: Integer);
  const
    MaxPixelCount = 32768;
  type
    PRGBTripleArray = ^TRGBTripleArray;
    TRGBTripleArray = array[0..MaxPixelCount] of TRGBTriple;
  var
    Row: PRGBTripleArray;
    x: Integer;
  begin
    Row := CurrBmp.ScanLine[y];
    for x := 0 to CurrBmp.Width - 1 do
    begin
      if Row[x].rgbtRed <> BkRed then
        Row[x].rgbtRed := Transparency * (Row[X].rgbtRed - BkRed) shr 8 + BkRed;
      if Row[x].rgbtGreen <> BkGreen then
        Row[x].rgbtGreen := Transparency * (Row[X].rgbtGreen - BkGreen) shr 8 + BkGreen;
      if Row[x].rgbtBlue <> BkBlue then
        Row[x].rgbtBlue := Transparency * (Row[X].rgbtBlue - BkBlue) shr 8 + BkBlue;
    end;
  end;
begin
  CurrBmp.Height := Height;
  CurrBmp.Width := Width;
  if FCurrPos + Height <= TextBmp.Height then //完整显示
    BitBlt(CurrBmp.Canvas.Handle, 0, 0, Width, Height, TextBmp.Canvas.Handle, 0,
      FCurrPos, SRCCopy)
  else
  begin                       //首尾相接
    BitBlt(CurrBmp.Canvas.Handle, 0, 0, Width, TextBmp.Height - FCurrPos,
      TextBmp.Canvas.Handle, 0, FCurrPos, SRCCopy);
    BitBlt(CurrBmp.Canvas.Handle, 0, TextBmp.Height - FCurrPos, Width, Height -
      (TextBmp.Height - FCurrPos), TextBmp.Canvas.Handle, 0, 0, SRCCopy);
  end;
  if FText.Fade then          //淡入淡出
  begin
    tBkColor := ColorToRGB(Color);
    BkRed := GetRValue(tBkColor);
    BkGreen := GetGValue(tBkColor);
    BkBlue := GetBValue(tBkColor);
    for i := 0 to FText.FadeHeight - 1 do
    begin
      DrawFade(i, 255 * i div (FText.FadeHeight - 1));
      DrawFade(Height - 1 - i, 255 * i div (FText.FadeHeight - 1));
    end;
  end;                        //绘制到控件画布
  if not (csDestroying in ComponentState) then
    BitBlt(Canvas.Handle, 0, 0, Width, Height, CurrBmp.Canvas.Handle, 0, 0, SRCCopy);
  if Assigned(OnPainted) then
    OnPainted(Self);
end;

//执行滚动
procedure TCnAAScrollText.OnScrollTimer(Sender: TObject);
begin
  if CurrPos = 0 then         //单次滚动完成
  begin
    FRepeatedCount := FRepeatedCount + 1;
    if (RepeatCount > 0) and (RepeatedCount >= RepeatCount) then
    begin                     //滚动完成
      Active := False;
      FRepeatedCount := -1;
      if Assigned(OnComplete) then
        OnComplete(Self);
      Exit;
    end
    else if DelayTimer.Interval > 0 then
    begin                     //循环延时
      ScrollTimer.Enabled := False;
      DelayTimer.Enabled := True;
      Exit;
    end;
  end;

  if (FScrollStep > 0) and (CurrPos + FScrollStep >= TextBmp.Height) then
    CurrPos := 0
  else if (FScrollStep < 0) and (CurrPos + FScrollStep < 0) then
    CurrPos := 0
  else
    CurrPos := CurrPos + FScrollStep; //当前位置增加
end;

//创建文本位图
procedure TCnAAScrollText.CreateText;
var
  i, j: Integer;
  DispLines: TStrings;
  CurrText: string;
  WrapLines: TStrings;
  CurrHeight: Integer;
  CurrAlign: TAlignment;
  x, y: Integer;
  TextWidth: Integer;
  TextHeight: Integer;
  MaxCol: Integer;
begin
  BeginUpdate;
  DispLines := nil;
  WrapLines := nil;
  try
    DispLines := TStringList.Create; //临时文本
    WrapLines := TStringList.Create;
    with FText do
    begin
      TextBmp.Height := 0;
      TextBmp.Width := Width;
      TextBmp.Canvas.Brush.Color := Color;
      TextBmp.Canvas.Brush.Style := bsSolid;
      DispLines.Clear;
      DispLines.AddStrings(Lines);
      AAFont.Canvas := TextBmp.Canvas;
      AAFont.Effect.Assign(FText.FontEffect);
      if Fade then            //淡入淡出空白
        CurrHeight := FadeHeight
      else
        CurrHeight := 0;
      CurrHeight := CurrHeight + Height * HeadSpace div 100; //头部空白
      TextBmp.Canvas.Font.Assign(Font);
      for i := 0 to DispLines.Count - 1 do
      begin
        CurrText := DispLines[i]; //当前处理字符串
        if LabelEffect = leOnlyALine then
        begin
          TextBmp.Canvas.Font.Assign(Font);
          AAFont.Effect.Assign(FText.FontEffect);
        end;
        Fonts.Check(CurrText, TextBmp.Canvas.Font, AAFont.Effect); //检查字体标签
        Labels.Check(CurrText, CurrAlign); //检查用户标签
        TextHeight := AAFont.TextHeight(CurrText + ' ');
        TextWidth := AAFont.TextWidth(CurrText);
        if WordWrap and (TextWidth > Width) then //自动换行
        begin
          MaxCol := Width * Length(CurrText) div TextWidth;
          while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > Width do
            Dec(MaxCol);
          WrapText(CurrText, WrapLines, MaxCol);
        end else if CurrText <> '' then
          WrapLines.Text := CurrText
        else
          WrapLines.Text := ' ';
        CurrHeight := CurrHeight + Round(TextHeight * (1 + RowPitch / 100)) *
          WrapLines.Count;
      end;
      TextBmp.Canvas.Brush.Color := Color;
      TextBmp.Canvas.Brush.Style := bsSolid;
      CurrHeight := CurrHeight + Height * TailSpace div 100; //尾部空白
      if CurrHeight < ClientHeight then
        CurrHeight := ClientHeight;
      TextBmp.Height := CurrHeight;
      if Assigned(FText.BackGround.Graphic) and not
        FText.BackGround.Graphic.Empty then
        DrawBackGround(TextBmp.Canvas, Rect(0, 0, TextBmp.Width,
          TextBmp.Height), FText.BackGround.Graphic, FText.BackGroundMode);

      DispLines.Clear;
      DispLines.AddStrings(Lines);
      TextBmp.Canvas.Brush.Style := bsClear;
      AAFont.Effect.Assign(FText.FontEffect);
      if Fade then            //淡入淡出空白
        CurrHeight := FadeHeight
      else
        CurrHeight := 0;
      CurrHeight := CurrHeight + Height * HeadSpace div 100; //头部空白
      TextBmp.Canvas.Font.Assign(Font);
      CurrAlign := Alignment; //默认对齐方式
      for i := 0 to DispLines.Count - 1 do
      begin
        CurrText := DispLines[i]; //当前处理字符串
        if LabelEffect = leOnlyALine then
        begin
          TextBmp.Canvas.Font.Assign(Font);
          AAFont.Effect.Assign(FText.FontEffect);
          CurrAlign := Alignment;
        end;
        Fonts.Check(CurrText, TextBmp.Canvas.Font, AAFont.Effect); //检查字体标签
        Labels.Check(CurrText, CurrAlign); //检查用户标签
        TextWidth := AAFont.TextWidth(CurrText);
        if WordWrap and (TextWidth > Width) then //自动换行
        begin
          MaxCol := Width * Length(CurrText) div TextWidth;
          while AAFont.TextWidth(Copy(CurrText, 1, MaxCol)) > Width do
            Dec(MaxCol);
          WrapText(CurrText, WrapLines, MaxCol);
        end else if CurrText <> '' then
          WrapLines.Text := CurrText
        else
          WrapLines.Text := ' ';
        for j := 0 to WrapLines.Count - 1 do
        begin
          CurrText := WrapLines[j];
          TextHeight := AAFont.TextHeight(CurrText + ' ');
          TextWidth := AAFont.TextWidth(CurrText);
          case CurrAlign of     //对齐方式
            taLeftJustify: x := 0;
            taCenter: x := (TextBmp.Width - TextWidth) div 2;
            taRightJustify: x := TextBmp.Width - TextWidth;
          else x := 0;
          end;
          y := CurrHeight;      //行间距
          AAFont.TextOutput(x, y, CurrText);
          CurrHeight := CurrHeight + Round(TextHeight * (1 + RowPitch / 100));
        end;
      end;
      if Assigned(OnTextReady) then //调用OnTextReady事件
        OnTextReady(Self);
    end;
  finally
    WrapLines.Free;
    DispLines.Free;
    EndUpdate;
  end;
end;

//设置活动
procedure TCnAAScrollText.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    ScrollTimer.Enabled := FActive;
    if not FActive then
      DelayTimer.Enabled := False;
  end;
end;

//设置循环延时
procedure TCnAAScrollText.SetRepeatDelay(const Value: Word);
begin
  if FRepeatDelay <> Value then
  begin
    FRepeatDelay := Value;
    if FRepeatDelay <= 0 then
      FRepeatDelay := 0;
    DelayTimer.Interval := Value;
  end;
end;

//设置滚动延时
procedure TCnAAScrollText.SetScrollDelay(const Value: Word);
begin
  if FScrollDelay <> Value then
  begin
    FScrollDelay := Value;
    if FScrollDelay <= 0 then
      FScrollDelay := 0;
    ScrollTimer.Interval := FScrollDelay;
  end;
end;

//设置每次滚动增量
procedure TCnAAScrollText.SetScrollStep(const Value: Integer);
begin
  if FScrollStep <> Value then
  begin
    FScrollStep := Value;
  end;
end;

//设置循环次数
procedure TCnAAScrollText.SetRepeatCount(const Value: TBorderWidth);
begin
  if FRepeatCount <> Value then
  begin
    FRepeatCount := Value;
    if FRepeatCount <= 0 then
      FRepeatCount := 0;
    Changed;
  end;
end;

//设置文本内容
procedure TCnAAScrollText.SetText(const Value: TCnScrollTextParam);
begin
  FText.Assign(Value);
end;

//重头开始滚动
procedure TCnAAScrollText.ReStart;
begin
  FRepeatedCount := -1;
  CurrPos := 0;
end;

//设置当前位置
procedure TCnAAScrollText.SetCurrPos(const Value: Integer);
begin
  if FCurrPos <> Value then
  begin
    FCurrPos := Value mod TextBmp.Height;
    if FCurrPos < 0 then
      Inc(FCurrPos, TextBmp.Height);
    Paint;
  end;
end;

//大小变化消息
function TCnAAScrollText.CanResize(var NewWidth,
  NewHeight: Integer): Boolean;
begin
  if NewWidth < 20 then NewWidth := 20;
  if NewHeight < 20 then NewHeight := 20;
  Result := inherited CanResize(NewWidth, NewHeight);
end;

//循环延时
procedure TCnAAScrollText.OnDelayTimer(Sender: TObject);
begin
  DelayTimer.Enabled := False;
  CurrPos := CurrPos + FScrollStep;
  if Active then
    ScrollTimer.Enabled := True;
end;

//创建默认字体集
procedure TCnAAScrollText.CreateDefFonts;
var
  FLabel: TCnFontLabel;
begin
  inherited;
  FLabel := Fonts.AddItem('Title4', '隶书', 22, clBlack, [fsBold], True, 2, 2);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsLeftToRight;
    FLabel.Effect.Gradual.StartColor := $00FF2200;
    FLabel.Effect.Gradual.EndColor := $002210FF;
    FLabel.Effect.Outline := True;
    FLabel.Effect.Blur := 50;
  end;
  FLabel := Fonts.AddItem('Text3', '隶书', 11, clBlue, [], True, 1, 1);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsTopToBottom;
    FLabel.Effect.Gradual.StartColor := $00CC3311;
    FLabel.Effect.Gradual.EndColor := $00FF22AA;
  end;
end;

//默认文本创建默认标签
function TCnAAScrollText.UseDefaultLabels: Boolean;
begin
  Result := not FText.IsLinesStored;
end;

//控件属性已装载
procedure TCnAAScrollText.LoadedEx;
begin
  inherited;
  Reset;
end;

{ TCnAAMarqueeText }

//--------------------------------------------------------//
//平滑字幕文本控件                                        //
//--------------------------------------------------------//

//初始化
constructor TCnAAMarqueeText.Create(AOwner: TComponent);
begin
  inherited;
  FEffect := TCnAAFontEffect.Create(Self, OnEffectChanged);
  ControlStyle := ControlStyle + [csReplicatable, csSetCaption];
  FTimer := TTimer.Create(Self);
  Height := 34;
  Width := 240;
  FScrollType := stRightToLeft;
  FActive := False;
  FSteps := 0;
  FScrollStep := 1;
  FScrollDelay := 100;
  FTimer.Enabled := FActive;
  FTimer.Interval := FScrollDelay;
  FTimer.OnTimer := OnTimer;
end;

//释放
destructor TCnAAMarqueeText.Destroy;
begin
  FEffect.Free;
  FTimer.Free;
  inherited;
end;

//定时器事件
procedure TCnAAMarqueeText.OnTimer(Sender: TObject);
begin
  if not FTimer.Enabled or not Visible then Exit;
  Inc(FCurrentStep, FScrollStep);
  Paint;
  if FCurrentStep >= FSteps then
    FCurrentStep := 0;
end;

//绘制画布
procedure TCnAAMarqueeText.PaintCanvas;
var
  R: TRect;
  X, Y: Integer;
  lpPaint: tagPAINTSTRUCT;
  MemBmp: TBitmap;
begin
  inherited;
  X := 0;
  BeginPaint(Canvas.Handle, lpPaint);
  MemBmp := TBitmap.Create;
  try
    MemBmp.PixelFormat := pf24bit;
    AAFont.Canvas := MemBmp.Canvas;
    MemBmp.Canvas.Font.Assign(Font); //字体
    R := ClientRect;
    case FEffect.Layout of
      tlTop: Y := 0;
      tlCenter: Y := R.Top + (R.Bottom - R.Top - MemBmp.Canvas.TextHeight('Pp')) div 2;
      tlBottom: Y := R.Bottom - R.Top - MemBmp.Canvas.TextHeight(Caption);
      else Y := 0;
    end;
    case FScrollType of
      stRightToLeft: X := Width - FCurrentStep;
      stLeftToRight: X := - FTextWidth + FCurrentStep;
      stNone:
        case FEffect.Alignment of
          taCenter: X := (Width - FTextWidth) div 2;
          taLeftJustify: X := 0;
          taRightJustify: X := Width - FTextWidth;
        end;
    else X := 0;
    end;
    MemBmp.Height := ClientHeight;
    MemBmp.Width := ClientWidth;
    MemBmp.Canvas.Brush.Color := Color;
    MemBmp.Canvas.Brush.Style := bsSolid;
    if FEffect.Transparent then  //透明
    begin
      CopyParentImage(MemBmp.Canvas); //复制父控件画布
    end else if not FEffect.IsBackEmpty then
    begin                       //绘制背景图
      DrawBackGround(MemBmp.Canvas, Rect(0, 0, MemBmp.Width, MemBmp.Height),
        FEffect.BackGround.Graphic, FEffect.BackGroundMode);
    end else
    begin                       //填充背景色
      MemBmp.Canvas.FillRect(ClientRect);
    end;
    MemBmp.Canvas.Brush.Style := bsClear;
    AAFont.TextOutput(X, Y, Caption); //平滑字体输出
    Bitblt(Canvas.Handle, 0, 0, Width, Height, MemBmp.Canvas.Handle, 0, 0,
      SRCCOPY);
  finally
    MemBmp.Free;
    EndPaint(Canvas.Handle, lpPaint);
  end;
  if Assigned(OnPainted) then
    OnPainted(Self);
end;

//复位
procedure TCnAAMarqueeText.Reset;
var
  Bmp: TBitmap;
  tActive: Boolean;
begin
  inherited Reset;
  tActive := Active;
  Active := False;
  Active := tActive;
  
  Bmp := TBitmap.Create;
  try
    if AutoSize and (FEffect.BackGround.Graphic <> nil) then
    begin
      Width := FEffect.BackGround.Width;
      Height := FEffect.BackGround.Height;
    end;
    Bmp.Canvas.Font.Assign(Font);
    FTextWidth := Bmp.Canvas.TextWidth(Caption);
    FSteps := FTextWidth + Width;
  finally
    Bmp.Free;
  end;
end;

//设置滚动延时
procedure TCnAAMarqueeText.SetScrollDelay(Value: Word);
begin
  if FScrollDelay <> Value then
  begin
    FScrollDelay := Value;
    if FTimer <> nil then
      FTimer.Interval := FScrollDelay;
  end;
end;

//设置每次滚动增量
procedure TCnAAMarqueeText.SetScrollStep(Value: Word);
begin
  if FScrollStep <> Value then
  begin
    if Value < 1 then
      Value := 1;
    FScrollStep := Value;
  end;
end;

//设置活跃
procedure TCnAAMarqueeText.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    FTimer.Enabled := FActive;
    if not FActive then
      FCurrentStep := 0;
    Invalidate;
  end;
end;

//设置字体特效
procedure TCnAAMarqueeText.SetEffect(const Value: TCnAAFontEffect);
begin
  FEffect.Assign(Value);
end;

//设置滚动类型
procedure TCnAAMarqueeText.SetScrollType(Value: THoriScrollType);
begin
  if FScrollType <> Value then
  begin
    FScrollType := Value;
    FTimer.Enabled := FScrollType <> stNone;
    Invalidate;
  end;
end;

{ TCnScrollTextParam }

//--------------------------------------------------------//
//平滑滚动文本参数                                        //
//--------------------------------------------------------//

//初始化
constructor TCnScrollTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  TStringList(Lines).Text := csAAScrollTextCopyRight;
  FFade := True;
  FFadeHeight := 10;
  FHeadSpace := 0;
  FTailSpace := 60;
  Alignment := taCenter;
  BackGroundMode := bmTiled;
end;

//释放
destructor TCnScrollTextParam.Destroy;
begin
  inherited;
end;

//设置淡入淡出
procedure TCnScrollTextParam.SetFade(const Value: Boolean);
begin
  if FFade <> Value then
  begin
    FFade := Value;
    Changed;
  end;
end;

//设置淡入淡出高度
procedure TCnScrollTextParam.SetFadeHeight(const Value: Integer);
begin
  if FFadeHeight <> Value then
  begin
    FFadeHeight := Value;
    Changed;
  end;
end;

//设置头部空白
procedure TCnScrollTextParam.SetHeadSpace(const Value: Integer);
begin
  if FHeadSpace <> Value then
  begin
    FHeadSpace := Value;
    if FHeadSpace < 0 then
      FHeadSpace := 0;
    if FHeadSpace > 150 then
      FHeadSpace := 150;
    Changed;
  end;
end;

//设置尾部空白
procedure TCnScrollTextParam.SetTailSpace(const Value: Integer);
begin
  if FTailSpace <> Value then
  begin
    FTailSpace := Value;
    if FTailSpace < 0 then
      FTailSpace := 0;
    if FTailSpace > 150 then
      FTailSpace := 150;
    Changed;
  end;
end;

//文本内容是否存储
function TCnScrollTextParam.IsLinesStored: Boolean;
begin
  Result := Lines.Text <> csAAScrollTextCopyRight;
end;

{ TCnAAFadeText }

//--------------------------------------------------------//
//平滑特效渐隐文本控件                                    //
//--------------------------------------------------------//

//创建
constructor TCnAAFadeText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  TextBmp := TBitmap.Create;
  TextBmp.PixelFormat := pf24bit;
  InBmp := TBitmap.Create;
  InBmp.PixelFormat := pf24bit;
  OutBmp := TBitmap.Create;
  OutBmp.PixelFormat := pf24bit;
  FadeTimer := TTimer.Create(Self);
  FadeTimer.Interval := 25;
  FadeTimer.Enabled := False;
  FadeTimer.OnTimer := OnFadeTimer;
  DelayTimer := TTimer.Create(Self);
  DelayTimer.Enabled := False;
  DelayTimer.OnTimer := OnDelayTimer;
  FText := TCnFadeTextParam.Create(Self, OnLabelChanged);
  FLineIndex := -1;
  FFadeProgress := 0;
  FRepeatCount := 0;
  FRepeatedCount := 0;
  FActive := True;
  Color := clWhite;
  LastText := '';
  CurrText := '';
  NewProg := 0;
  SetBounds(0, 0, 240, 34);
end;

//创建默认字体标签
procedure TCnAAFadeText.CreateDefFonts;
var
  FLabel: TCnFontLabel;
begin
  inherited;
  FLabel := Fonts.AddItem('Title4', '隶书', 22, clBlack, [], True, 2, 2);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsLeftToRight;
    FLabel.Effect.Gradual.StartColor := $00FF2200;
    FLabel.Effect.Gradual.EndColor := $002210FF;
    FLabel.Effect.Outline := True;
    FLabel.Effect.Blur := 50;
  end;
  FLabel := Fonts.AddItem('Text3', '隶书', 11, clBlue, [], True, 1, 1);
  if Assigned(FLabel) then
  begin
    FLabel.Effect.Gradual.Enabled := True;
    FLabel.Effect.Gradual.Style := gsTopToBottom;
    FLabel.Effect.Gradual.StartColor := $00CC8811;
    FLabel.Effect.Gradual.EndColor := $00FF22AA;
  end;
end;

//释放
destructor TCnAAFadeText.Destroy;
begin
  FText.Free;
  DelayTimer.Free;
  FadeTimer.Free;
  OutBmp.Free;
  InBmp.Free;
  TextBmp.Free;
  inherited;
end;

//绘制渐隐图
procedure TCnAAFadeText.DrawFadeBmp(AText: string; Bmp: TBitmap);
var
  OffPoint: TPoint;
  th, tw: Integer;
begin
  AAFont.Canvas := Bmp.Canvas;
  if Text.LabelEffect = leOnlyALine then
  begin
    Bmp.Canvas.Font.Assign(Font);
    AAFont.Effect.Assign(Text.FontEffect);
    CurrAlign := Text.Alignment;
  end;
  Fonts.Check(AText, Bmp.Canvas.Font, AAFont.Effect); //检查字体标签
  Labels.Check(AText, CurrAlign); //检查用户标签
  th := AAFont.TextHeight(AText); //文本高度
  tw := AAFont.TextWidth(AText); //文本宽度
  case CurrAlign of           //水平对齐方式
    taLeftJustify: OffPoint.x := 0;
    taRightJustify: OffPoint.x := ClientWidth - tw;
    taCenter: OffPoint.x := (ClientWidth - tw) div 2;
  end;
  case Text.Layout of         //垂直对齐方式
    tlTop: OffPoint.y := 0;
    tlCenter: OffPoint.y := (ClientHeight - th) div 2;
    tlBottom: OffPoint.y := ClientHeight - th;
  end;
  Bmp.Height := ClientHeight;
  Bmp.Width := ClientWidth;
  Bmp.Canvas.Brush.Color := Color;
  Bmp.Canvas.Brush.Style := bsSolid;
  if Text.Transparent then    //透明
  begin
    CopyParentImage(Bmp.Canvas); //复制父控件画布
  end else if not Text.IsBackEmpty then
  begin                       //绘制背景图
    DrawBackGround(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height),
      Text.BackGround.Graphic, Text.BackGroundMode);
  end else
  begin                       //填充背景色
    Bmp.Canvas.FillRect(ClientRect);
  end;
  Bmp.Canvas.Brush.Style := bsClear;
  AAFont.TextOutput(OffPoint.x, OffPoint.y, AText); //平滑字体输出
end;

//渐隐到指定行
procedure TCnAAFadeText.FadeTo(Line: Integer);
begin
  if Text.Lines.Count <= 0 then
    Exit;
  if Line < 0 then
    Line := 0;
  if Line > Text.Lines.Count - 1 then
  begin
    Line := 0;
    Inc(FRepeatedCount);
    if (FRepeatCount > 0) and (FRepeatedCount >= FRepeatCount) then
    begin
      Active := False;
      FRepeatedCount := 0;
      FLineIndex := -1;
      FadeToStr('');
      if Assigned(OnComplete) then
        OnComplete(Self);
      Exit;
    end;
  end;
  FadeToStr(Text.Lines[Line]);
  FLineIndex := Line;
end;

//渐隐到下一行
procedure TCnAAFadeText.FadeToNext;
begin
  FadeTo(LineIndex + 1);
end;

//渐隐到指定文本
procedure TCnAAFadeText.FadeToStr(AText: string);
begin
  OutBmp.Assign(TextBmp);
  DrawFadeBmp(AText, InBmp);
  LastText := CurrText;
  CurrText := AText;
  FFadeProgress := 0;
  FadeTimer.Enabled := False;
  FadeTimer.Enabled := True;
  if DelayTimer.Enabled then
  begin
    DelayTimer.Enabled := False;
    DelayTimer.Enabled := True;
  end;
end;

//属性已装载
procedure TCnAAFadeText.LoadedEx;
begin
  inherited;
  CurrAlign := Text.Alignment;
  Reset;
  FRepeatedCount := 0;
  DelayTimer.Enabled := FActive;
  if FActive then
    OnDelayTimer(Self);
end;

//渐隐切换文本定时事件
procedure TCnAAFadeText.OnDelayTimer(Sender: TObject);
begin
  FadeToNext;
end;


//渐隐过程定时事件
procedure TCnAAFadeText.OnFadeTimer(Sender: TObject);
begin
  if Abs(NewProg - FadeProgress) > 1 then
    NewProg := FadeProgress;
  NewProg := NewProg + csMaxProgress * FadeTimer.Interval div Text.FadeDelay;
  if NewProg > csMaxProgress then
  begin
    NewProg := csMaxProgress;
    FadeTimer.Enabled := False;
  end;
  FadeProgress := Round(NewProg);
end;

//绘制控件画布
procedure TCnAAFadeText.PaintCanvas;
begin
  inherited;
  if Text.Transparent then
  begin                       //透明且完整重绘
    if FadeProgress = 0 then
      DrawFadeBmp(CurrText, TextBmp)
    else begin
      DrawFadeBmp(LastText, OutBmp);
      DrawFadeBmp(CurrText, InBmp);
    end;
  end;
  if FadeProgress <> 0 then   //渐隐中
    Blend(TextBmp, OutBmp, InBmp, FFadeProgress);
  Bitblt(Canvas.Handle, 0, 0, Width, Height, TextBmp.Canvas.Handle, 0, 0,
    SRCCOPY);
  if Assigned(OnPainted) then
    OnPainted(Self);
end;

//更新显示
procedure TCnAAFadeText.Reset;
begin
  if FadeProgress = 0 then
    DrawFadeBmp(CurrText, TextBmp)
  else begin
    DrawFadeBmp(LastText, OutBmp);
    DrawFadeBmp(CurrText, InBmp);
    Blend(TextBmp, OutBmp, InBmp, FFadeProgress);
  end;
  inherited;
end;

//设置活跃
procedure TCnAAFadeText.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    DelayTimer.Enabled := FActive;
    if FActive then
    begin
      FRepeatedCount := 0;
      OnDelayTimer(Self);
    end;
  end;
end;

//设置渐隐进程
procedure TCnAAFadeText.SetFadeProgress(const Value: TProgress);
begin
  if FFadeProgress <> Value then
  begin
    FFadeProgress := Value;
    Paint;
  end;
end;

//设置当前行
procedure TCnAAFadeText.SetLineIndex(const Value: Integer);
begin
  if FLineIndex <> Value then
  begin
    FadeTo(FLineIndex);
  end;
end;

//设置总循环次数
procedure TCnAAFadeText.SetRepeatCount(const Value: TBorderWidth);
begin
  if FRepeatCount <> Value then
  begin
    FRepeatCount := Value;
    if FRepeatedCount >= FRepeatCount then
  end;
end;

//设置文本
procedure TCnAAFadeText.SetText(const Value: TCnFadeTextParam);
begin
  FText.Assign(Value);
end;

//是默认文本时创建默认标签
function TCnAAFadeText.UseDefaultLabels: Boolean;
begin
  Result := not FText.IsLinesStored;
end;

{ TCnFadeTextParam }

//--------------------------------------------------------//
//平滑特效渐隐文本参数                                    //
//--------------------------------------------------------//

//赋值
procedure TCnFadeTextParam.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCnFadeTextParam then
  begin
    FFadeDelay := TCnFadeTextParam(Source).FadeDelay;
    LineDelay := TCnFadeTextParam(Source).LineDelay;
  end;
end;

//创建
constructor TCnFadeTextParam.Create(AOwner: TCnAAGraphicControl;
  ChangedProc: TNotifyEvent);
begin
  inherited;
  TStringList(Lines).Text := csAAFadeTextCopyRight;
  FadeDelay := 600;
  LineDelay := 3000;
  Alignment := taCenter;
  Layout := tlCenter;
end;

//释放
destructor TCnFadeTextParam.Destroy;
begin
  inherited;
end;

//取行延时
function TCnFadeTextParam.GetLineDelay: Cardinal;
begin
  Result := TCnAAFadeText(Owner).DelayTimer.Interval;
end;

// 取图像高度
function TCnAAScrollText.GetBmpHeight: Integer;
begin
  Result := TextBmp.Height;
end;

//存储文本
function TCnFadeTextParam.IsLinesStored: Boolean;
begin
  Result := Lines.Text <> csAAFadeTextCopyRight;
end;

//设置渐隐延时
procedure TCnFadeTextParam.SetFadeDelay(const Value: Cardinal);
begin
  if FFadeDelay <> Value then
  begin
    FFadeDelay := Value;
    if FFadeDelay > LineDelay - 200 then
      FFadeDelay := LineDelay - 200;
    if FFadeDelay < 50 then
      FFadeDelay := 50;
  end;
end;

//设置行延时
procedure TCnFadeTextParam.SetLineDelay(const Value: Cardinal);
var
  T: Cardinal;
begin
  T := Value;
  if T < FFadeDelay + 200 then
    T := FFadeDelay + 200;
  TCnAAFadeText(Owner).DelayTimer.Interval := T;
end;

end.
