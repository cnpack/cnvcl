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

unit CnTextControl;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：文本显示与编辑控件实现单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元当前仅为内部参考测试用
* 相关说明：ScreenLineNumber（或 ScreenRow）: 控件内显示用的物理行号，最上面是 1，该概念不明显
*           LineNumber（或 Row）: 滚动后的虚拟行号，1 开始，和 ScreenLineNumber 差个 FVertOffset
*           PaintLineNumber: 在行号栏上绘制的行号，没有折叠的情况下等于 LineNumber
*           控件内显示不应该有物理列号的概念，因为滚动的字符横向不一定等宽，基类中只处理等宽且无汉字的情形
*           ColNumber（或 Col）：滚动后的虚拟列号，1 开始，和基类中的 ScreenColNumber 差个 FHoriOffset
*           CaretRow：当前光标所在的虚拟行号，1 开始，等于 ScreenLineNumber
*           CaretCol：当前光标所在的虚拟列号，左边第 1 个字符框左边是第 1 个光标位置
*           滚动行为：拖动滚动条、点击滚动条箭头、鼠标滚动时，虚拟光标位置均不应变化
*                     方向键、翻页键时，虚拟光标位置发生变化，再内容滚动到可见
*           文字区：控件物理区域内显示具体文字的区域，左上角原点为控件行号区右边，
*                 离顶部有空隙，像素坐标原点是控件物理区域左上角
*           虚拟文字区：包括滚动到控件外的文字显示区域，像素坐标原点也是控件物理区域左上角
*                 内部以文字排版为主体，文字行列以 1 开始，左上角可能在控件物理区域外
*           虚拟排版区：大小形状等同于虚拟文字区，但其左上角为原点
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2021.04.20 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Messages, Windows, Graphics;

type
  ECnTextControlException = class(Exception);

  TCnVirtualTextControl = class(TCustomControl)
  {* 能够显示不同字体的文字并滚动的基类，和具体字符串内容无关}
  private
    FFontIsFixedWidth: Boolean;   // 字体是否等宽
    FCharFrameSize: TPoint;       // 单个字符的外框尺寸，供绘制用
    FCharFrameWidthHalf: Integer; // 单个字符的外框尺寸的一半，供判断点击时光标在字符前后使用
    FLineHeight: Integer;         // 行高，由字体计算而来
    FShowLineNumber: Boolean;     // 是否显示行号区
    FLineNumCount: Integer;       // 最大行号的位数
    FLineNumPattern: string;      // 最大虚拟行号所对应的字符串样本，如 0000
    FLineNumWidth: Integer;       // 最大虚拟行号所对应的字符串宽度
    FMaxLineCount: Integer;       // 最大虚拟行号，由外界设置，行数从 1 到 FMaxLineCount
    FWheelLinesCount: Integer;    // 鼠标纵向滚动下所跳的行数
    FLineNumColor: TColor;        // 行号字的颜色
    FLineNumFocusBkColor: TColor;     // 有焦点时行号区的背景色
    FLineNumNoFocusBkColor: TColor;   // 无焦点时行号区的背景色
    FGutterRect: TRect;               // 侧边区
    FTextRect: TRect;                 // 文字绘制区
    FUseCaret: Boolean;               // 是否使用光标
    FCaretVisible: Boolean;           // 光标是否可见
    FCharFrameIsLeft: Boolean;        // 是否处于当前字符框的左半边
    FCharFrameDoubleWidth: Boolean;   // 当前字符框是否双倍宽，用于宽字节字符
    FCaretRow: Integer;               // 光标所在的虚拟行号，1 开始，是 LineNumber
    FCaretCol: Integer;               // 光标所在的虚拟列号，1 开始
    FCaretAfterLineEnd: Boolean;      // 是否允许光标越过行尾
    FSelectStartRow: Integer;         // 选择区起始虚拟行号，注意起始和结束不一定哪个靠前
    FSelectEndRow: Integer;           // 选择区结束虚拟行号
    FSelectStartCol: Integer;         // 选择区起始虚拟列号
    FSelectEndCol: Integer;           // 选择区结束虚拟列号
    FLeftMouseDown: Boolean;          // 记录鼠标左键按下与否
    FLeftMouseMoveAfterDown: Boolean; // 记录鼠标左键按下后是否拖动过
    FIsWheeling: Boolean;             // 当前滚动时是否是由鼠标滚轮事件触发
    FOnCaretChange: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FUseSelection: Boolean;
    FOnSelectChange: TNotifyEvent;
    procedure UpdateScrollBars;       // 根据屏幕状况确定滚动条的位置尺寸等
    procedure UpdateRects;
    {* 计算文本区、行号区等的尺寸，注意在 Paint 中没有使用}
    procedure CalcMetrics;
    {* 字体改变时调用，全部重算}
    function GetVisibleLineCount: Integer;
    function GetScreenBottomLine: Integer;
    procedure SetMaxLineCount(const Value: Integer);
    procedure SetShowLineNumber(const Value: Boolean);
    procedure SetLineNumColor(const Value: TColor);
    procedure SetLineNumFocusBkColor(const Value: TColor);
    procedure SetLineNumNoFocusBkColor(const Value: TColor);
    procedure SetUseCaret(const Value: Boolean);

    procedure DisplayCaret(CaretVisible: Boolean);
    {* 控制光标显示与否}
    function GetTextRectLeft: Integer;
    {* 动态计算文本显示区的相对于控件左上角的物理左坐标，考虑行号显示与否的情形}
    function GetTextRect: TRect;
    {* 获得整个多行文本的显示区域，坐标以控件物理区域为基础，刨去了上、左、下边界}
    function ScreenLineNumberToLineNumber(ScreenLineNumber: Integer): Integer;
    {* 将屏幕上的物理行号（1 开始的）转换成滚动后的虚拟行号（1 开始的）}
    function ScreenColNumberToColNumber(ScreenColNumber: Integer): Integer;
    {* 将屏幕上的物理列号（1 开始的）转换成滚动后的虚拟列号（1 开始的）}
    function LineNumberToScreenLineNumber(LineNumber: Integer): Integer;
    {* 将屏幕上的虚拟行号（1 开始的）转换成屏幕上的物理行号（1 开始的）}
    function ColNumberToScreenColNumber(ColNumber: Integer): Integer;
    {* 将屏幕上的虚拟列号（1 开始的）转换成屏幕上的物理列号（1 开始的）}
    function CalcRowCol(Pt: TPoint; out ACaretRow, ACaretCol: Integer;
      out ACharFrameIsLeft, ACharFrameDoubleWidth: Boolean): Boolean;
    {* 根据控件内坐标计算虚拟字符位置，返回计算是否成功}
    procedure UpdateCursorFrameCaret;
    {* 根据当前鼠标位置定位字符框位置}
    procedure LimitRowColumnInLine(var LineNumber, Column: Integer);
    {* 根据当前光标位置以及最大行数以及是否允许光标超行尾的设置来调整光标行列位置}
    procedure SyncCaretPosition;
    {* 根据当前虚拟行列重新设置物理光标位置}
    function SyncSelectionStartEnd(Force: Boolean = False): Boolean;
    {* 不在选择状态时（或强制），将光标位置同步塞给选择起始结束位置，也意味着取消选择
      返回值为 True 时表示执行了同步并取消选择动作}
    procedure CalcSelectEnd(Pt: TPoint);
    {* 根据控件内坐标计算并更新选择区末尾，会控制在光标可移动范围内}

    procedure SetCaretCol(const Value: Integer);
    procedure SetCaretRow(const Value: Integer);
    procedure SetCaretRowCol(Row, Col: Integer);
    procedure SetCaretAfterLineEnd(const Value: Boolean);
    procedure SetSelectEndCol(const Value: Integer);    // 光标跟着选择区尾走
    procedure SetSelectEndRow(const Value: Integer);    // 光标跟着选择区尾走
    procedure SetSelectStartCol(const Value: Integer);
    procedure SetSelectStartRow(const Value: Integer);
    function GetTopLine: Integer;                       // 只有行没有列的原因是列不等宽
    function GetBottomLine: Integer;
    function GetHoriPixelsOffset: Integer;              // 横向滚动的像素数
    procedure SetUseSelection(const Value: Boolean);
    procedure SetOnSelectChange(const Value: TNotifyEvent);
    function GetScreenCaretRow: Integer;
  protected
    FVertExp: Integer;            // 纵向滚动的指数，用于控制纵向行太多时避免竖向滚动条太细
    FVertOffset: Integer;         // 纵向滚动偏移量，以物理行为单位，0 开始，加 1 就是 TopLine
    FHoriOffset: Integer;         // 横向滚动偏移量，以平均字符宽度为单位，0 开始，非等宽字体下和列号没有直接关系
    FAveCharWidth: Integer;       // 字体的字符平均宽度，用来计算横向滚动
    FMaxCharWidth: Integer;       // 字体的字符的最大宽度

    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSetFont(var message: TMessage); message WM_SETFONT;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;

    procedure WMVScroll(var message: TWMScroll); message WM_VSCROLL;
    {* 竖向滚动消息处理，更新纵向滚动条与起始行，不处理光标位置}
    procedure WMHScroll(var message: TWMScroll); message WM_HSCROLL;
    {* 横向滚动消息处理，更新横向滚动条与起始列，不处理光标位置}

    procedure WMSetFocus(var message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMSize(var message: TWMSize); message WM_SIZE;
    procedure WMMouseWheel(var message: TWMMouseWheel); message WM_MOUSEWHEEL;
    {* 鼠标滚轮滚动，该动作不影响虚拟的光标位置，只整体滚动，因此物理光标位置可能移出屏幕外}

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: WORD; Shift: TShiftState); override;
    procedure DoScroll; virtual;
    procedure DoCaretChange; virtual;
    {* 光标位置发生改变时调用}
    procedure DoSelectChange; virtual;
    {* 选择区发生改变时调用}

    procedure NavigationKey(Key: WORD; Shift: TShiftState); virtual;
    {* 收到左右上下方向键以及 PageUp/PageDown Home/End 键的处理，基类只滚动内容
      子类还需额外重写以处理光标移动}
    procedure ScrollToVisibleCaret;
    {* 虚拟位置在屏幕外时滚动屏幕以将光标露出，虚拟光标不变，物理光标可能发生变化
      内部实现是修改 FVertOffset 和 FHoriOffset 并重绘}

    procedure ScrollUpLine;      // 上滚一行
    procedure ScrollDownLine;    // 下滚一行
    procedure ScrollLeftCol;     // 左滚一字符宽度
    procedure ScrollRightCol;    // 右滚一字符宽度
    procedure ScrollUpPage;      // 上滚一屏
    procedure ScrollDownPage;    // 下滚一屏
    procedure ScrollLeftPage;    // 左滚一屏
    procedure ScrollRightPage;   // 右滚一屏

    function ClientXToVirtualX(X: Integer): Integer;
    function VirtualXToClientX(X: Integer): Integer;
    {* 虚拟排版区里的横向像素坐标与控件的横向物理像素坐标转换}

    procedure GetVirtualCharPosVirtualRect(ARow, ACol: Integer; var Rect: TRect);
    {* 从虚拟文字坐标获得在虚拟排版区内的 Rect，虚拟排版区左上角是原点}
    procedure GetVirtualCharPosPhysicalRect(ARow, ACol: Integer; var Rect: TRect);
    {* 从虚拟文字坐标获得其控件上的物理 Rect，注意可能超出 TextRect}
    function GetColumnVirtualX(ARow, ACol: Integer): Integer;
    {* 获得某虚拟文字坐标左侧的光标位置相对于整个虚拟文字区左侧的像素横坐标}
    procedure Paint; override;
    {* 绘制方法}
    procedure DoPaintLineNum(ScreenLineNumber, LineNumber: Integer; LineNumRect: TRect); virtual;
    {* 默认的绘制行号的方法，参数 ScreenLineNumber 为 1 开始的物理行号，
      LineNumber 为滚动后的虚拟行号，LineNumRect 为行号待绘制的方框}

    procedure DoPaintLineBackground(LineCanvas: TCanvas; LineNumber: Integer;
      LineRect: TRect); virtual;
    {* 绘制行背景，默认实现是用控件 Color 做背景色填充}
    procedure DoPaintLine(LineCanvas: TCanvas; LineNumber, HoriCharOffset: Integer;
      LineRect: TRect); virtual; abstract;
    {* 子类必须实现的绘制行内容的方法，参数 ScreenLineNumber 为 1 开始的物理行号，
      LineNumber 为滚动后的虚拟行号，HoriCharOffset 为左侧已经滚过的字符宽度数（并非严格等于列数），
      LineNumRect 为行内容待绘制的长条方框}

    function GetPaintLineNumber(LineNumber: Integer): Integer; virtual;
    {* 从滚动后的虚拟行号返回在行号栏上绘制的行号，默认情况直接返回原始值即可
      子类返回不同值以处理折叠情况，注意返回值不要超过最大行号值}

    function ClientPosToVirtualCharPos(Pt: TPoint; out Row, Col: Integer;
      out LeftHalf, DoubleWidth: Boolean; ExtendOut: Boolean = True): Boolean; virtual;
    {* 将控件内的像素坐标转换为虚拟文字坐标，也就是落在虚拟第几行，行内第几个字符方框内，
      以及在字符矩形内属于靠左半还是靠右半，用于确定插入光标的位置。返回是否成功
      Row、Col 均以 1 开始，ExtendOut 为 True 时表示光标在文字区外也往里靠地算进去
      注意点在文字区左边一点点也能计算成功返回 True}

    function CalcColumnFromPixelOffsetInLine(ARow, VirtualX: Integer;
      out Col: Integer; out LeftHalf, DoubleWidth: Boolean): Boolean; virtual;
    {*** 根据虚拟行号与点的虚拟排版区内的横坐标获得所在的字符方框（或者字符方框左侧边线）的序号也就是虚拟列号，1 开始，
      LeftHalf 返回 X 位置是否在此字符方框的左半边，DoubleWidth 返回该字符方框是否是双倍宽
      内部实现是：等宽字体时，直接根据字符尺寸 FCharFrameSize 计算，但没考虑内容是否汉字
      返回是否成功}

    function CalcPixelOffsetFromColumnInLine(ARow, ACol: Integer; out Rect: TRect;
      out DoubleWidth: Boolean): Boolean; virtual;
    {*** 根据虚拟行号与虚拟列号，计算返回该列所在字符方框相对于当前行所在虚拟行方框的位置
     （Rect 的原点是当前完整行的左上角，其 Left 就是光标在当前完整行内的坐标）
      返回是否成功，False 表示该列不存在（譬如双字节字符不可分割）}

    function GetLastColumnFromLine(LineNumber: Integer): Integer; virtual; abstract;
    {*** 根据虚拟行号获得该行行尾的虚拟列值，子类必须实现，一般是内容长度加 1，和具体绘制无关
      如无内容，必须返回 1}

    function GetPrevColumn(AColumn, ARow: Integer): Integer; virtual;
    {** 某虚拟行里，返回某列的前一列，和具体绘制无关}
    function GetNextColumn(AColumn, ARow: Integer): Integer; virtual;
    {** 某虚拟行里，返回某列的后一列，和具体绘制无关}
  public
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;

    constructor Create(AOwner: TComponent); override;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function HasSelection: Boolean;
    {* 是否有选择区存在，也就是判断起始和结束位置是否相同}

    property LineHeight: Integer read FLineHeight;
    {* 行高，包括文字高度、文字 ExternalLeading 以及行间画波浪线预留的空隙}
    property TopLine: Integer read GetTopLine;
    {* 控件显示区域最顶上一行的虚拟行号，等于 FVertOffset + 1}
    property BottomLine: Integer read GetBottomLine;
    {* 控件显示区域最下面一行的虚拟行号，不超过 FMaxLineCount
      等于 FVertOffset + 1 + (TextRect.Bottom - TextRect.Top) div FLineHeight}

    property ScreenBottomLine: Integer read GetScreenBottomLine;
    {* 最下面一行的物理行号，（最上面一行的物理行号是 1）}
    property VisibleLineCount: Integer read GetVisibleLineCount;
    {* 可视化的行数，等于显示区域高度整除以行高}
    property MaxLineCount: Integer read FMaxLineCount write SetMaxLineCount;
    {* 由外界控制的最大虚拟行数}

    property WheelLinesCount: Integer read FWheelLinesCount write FWheelLinesCount;
    {* 鼠标滚轮一次滚动的行数}
    property ShowLineNumber: Boolean read FShowLineNumber write SetShowLineNumber;
    {* 是否显示行号区}
    property LineNumFocusBkColor: TColor read FLineNumFocusBkColor write SetLineNumFocusBkColor;
    {* 有焦点时行号区的背景颜色}
    property LineNumNoFocusBkColor: TColor read FLineNumNoFocusBkColor write SetLineNumNoFocusBkColor;
    {* 无焦点时行号区的背景颜色}
    property LineNumColor: TColor read FLineNumColor write SetLineNumColor;
    {* 行号区的文字颜色}
    property UseCaret: Boolean read FUseCaret write SetUseCaret;
    {* 是否显示插入光标}
    property CaretAfterLineEnd: Boolean read FCaretAfterLineEnd write SetCaretAfterLineEnd;
    {* 是否允许光标越过行尾}
    property ScreenCaretRow: Integer read GetScreenCaretRow;
    {* 当前光标位置所在的屏幕行号，1 开始，应该随内容滚动而变化（虚拟的不变），
      值可小于 0，表示当前光标在控件区域外。没有屏幕列号的原因是字体不等宽}
    property CaretRow: Integer read FCaretRow write SetCaretRow;
    {* 当前光标位置所在的滚动后的虚拟行号，1 开始，滚动时不变，允许外界设置
      和 ScreenCaretRow 差一个 FVertOffset}
    property CaretCol: Integer read FCaretCol write SetCaretCol;
    {* 当前光标位置所在的滚动后的虚拟列号，1 开始，滚动时不变，允许外界设置
      和 ScreenCaretCol 差一个 FHoriOffset}

    property UseSelection: Boolean read FUseSelection write SetUseSelection;
    {* 是否启用选择区功能}

    property SelectStartRow: Integer read FSelectStartRow write SetSelectStartRow;
    {* 选择区起始行，1 开始，虚拟行号}
    property SelectStartCol: Integer read FSelectStartCol write SetSelectStartCol;
    {* 选择区起始列，1 开始，虚拟列号}
    property SelectEndRow: Integer read FSelectEndRow write SetSelectEndRow;
    {* 选择区结束行，1 开始，虚拟行号}
    property SelectEndCol: Integer read FSelectEndCol write SetSelectEndCol;
    {* 选择区结束列，1 开始，虚拟列号}

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    {* 滚动事件}
    property OnCaretChange: TNotifyEvent read FOnCaretChange write FOnCaretChange;
    {* 光标移动事件}
    property OnSelectChange: TNotifyEvent read FOnSelectChange write SetOnSelectChange;
    {* 选择区发生改变时触发，注意此时鼠标不一定抬起了}
  published
    property Align;
    property Ctl3D;
    property Color;
    property Font;
  end;

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

const
  MAX_NO_EXP_LINES = 32768;
  LEFT_MARGIN = 3;             // 行号区或正文区左边的空隙
  COMMON_MARGIN = 3;           // 正文区顶部的空隙
  SEP_WIDTH = 3;               // 行号区与正文区分隔线的宽度
  LINE_GAP = 2;                // 行与行之间的空隙，让画下划线波浪线用
  CARET_MARGIN = 3;
  DEFAULT_MAX_WIDTH = 255;

resourcestring
  SCnErrorColumn = 'Error Column';

function GetNumWidth(Int: Integer): Integer;
begin
  Result := Length(IntToStr(Int));
end;

function EnumFontsProc(var ELF: TEnumLogFont; var TM: TNewTextMetric;
  FontType: Integer; Data: LPARAM): Integer; stdcall;
begin;
  Result := Integer(FIXED_PITCH = (ELF.elfLogFont.lfPitchAndFamily and FIXED_PITCH));
end;

{ TCnTextControl }

procedure TCnVirtualTextControl.CalcMetrics;
const
  csAlphaText = 'abcdefghijklmnopqrstuvwxyz';
  csHeightText = 'Wj_';
  csWidthText = '1';
var
  LogFont: TLogFont;
  DC: HDC;
  SaveFont: HFONT;
  AHandle: THandle;
  TM: TEXTMETRIC;
  ASize: TSize;
begin
  FLineHeight := 0;

  if GetObject(Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
  begin
    DC := CreateCompatibleDC(0);
    SaveFont := 0;
    try
      AHandle := CreateFontIndirect(LogFont);
      AHandle := SelectObject(DC, AHandle);
      if SaveFont = 0 then
        SaveFont := AHandle
      else if AHandle <> 0 then
        DeleteObject(AHandle);

      GetTextMetrics(DC, TM);
      FAveCharWidth := TM.tmAveCharWidth; // 得到字符平均宽度
      FMaxCharWidth := TM.tmMaxCharWidth; // 得到字符的最大宽度

      GetTextExtentPoint(DC, csAlphaText, Length(csAlphaText), ASize);

      // 取文本高度来计算行高
      if TM.tmHeight + TM.tmExternalLeading > FLineHeight then
        FLineHeight := TM.tmHeight + TM.tmExternalLeading;

      if ASize.cy > FLineHeight then
        FLineHeight := ASize.cy;

      // FLineHeight 要留出上下空隙以及下划线的空间来？
      Inc(FLineHeight, LINE_GAP);

      // 更新
      if ASize.cx div Length(csAlphaText) > FAveCharWidth then
        FAveCharWidth := ASize.cx div Length(csAlphaText);

      // 通过另一种方式获得字符框的大小
      GetTextExtentPoint32(DC, csWidthText, Length(csWidthText), ASize);
      FCharFrameSize.x := ASize.cx;
      FCharFrameWidthHalf := FCharFrameSize.x shr 1;

      GetTextExtentPoint32(DC, csHeightText, Length(csHeightText), ASize);
      FCharFrameSize.y := ASize.cy;

      // 计算行号宽度
      GetTextExtentPoint32(DC, PChar(FLineNumPattern), Length(FLineNumPattern), ASize);
      FLineNumWidth := ASize.cx;

      // 判断是否等宽
      FFontIsFixedWidth := EnumFontFamiliesEx(DC, LogFont, @EnumFontsProc, 0, 0);
    finally
      SaveFont := SelectObject(DC, SaveFont);
      if SaveFont <> 0 then
        DeleteObject(SaveFont);
      DeleteDC(DC);
    end;
  end;
end;

function TCnVirtualTextControl.ClientPosToVirtualCharPos(Pt: TPoint; out Row,
  Col: Integer; out LeftHalf, DoubleWidth: Boolean; ExtendOut: Boolean): Boolean;
var
  TR: TRect;
  SR: Integer;
begin
  Result := False;
  TR := GetTextRect;

  if ExtendOut then
  begin
    // 坐标在方框外时，判断方向并往里移
    if Pt.x < TR.Left then
      Pt.x := TR.Left + 1;
    if Pt.x > TR.Right then
      Pt.x := TR.Right - 1;
    if Pt.y < TR.Top then
      Pt.y := TR.Top + 1;
    if Pt.y > TR.Bottom then
      Pt.y := TR.Bottom - 1;
  end;

  if PtInRect(TR, Pt) then
  begin
    SR := ((Pt.y - TR.Top) div FLineHeight) + 1;
    Row := ScreenLineNumberToLineNumber(SR);

    // 控件文字区内的物理像素坐标需要转换成虚拟文字区里的像素坐标
    Result := CalcColumnFromPixelOffsetInLine(Row, ClientXToVirtualX(Pt.x), Col,
      LeftHalf, DoubleWidth);
  end;
end;

constructor TCnVirtualTextControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csDoubleClicks];
  SetBounds(Left, Top, 300, 200);
  ParentFont := True;
  ParentColor := False;
  TabStop := True;
  DoubleBuffered := True;

  FLineNumCount := 1;
  FLineNumPattern := '0';
  FLineHeight := 12;
  FWheelLinesCount := 3;

  FLineNumColor := clNavy;
  FLineNumFocusBkColor := clSilver;
  FLineNumNoFocusBkColor := clGray;

  FCaretRow := 1;
  FCaretCol := 1;
  FCaretAfterLineEnd := True;

  CalcMetrics;
end;

procedure TCnVirtualTextControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := Style or WS_VSCROLL or WS_HSCROLL or WS_TABSTOP;
    if NewStyleControls and Ctl3D then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      Style := Style or WS_BORDER;
  end;
end;

destructor TCnVirtualTextControl.Destroy;
begin

  inherited;
end;


procedure TCnVirtualTextControl.DoPaintLineNum(ScreenLineNumber,
  LineNumber: Integer; LineNumRect: TRect);
begin
  Canvas.TextOut(LineNumRect.Left, LineNumRect.Top, IntToStr(LineNumber));
end;

procedure TCnVirtualTextControl.DoScroll;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

//procedure TCnVirtualTextControl.GetScreenCharPosRect(ScreenRow,
//  ScreenCol: Integer; var Rect: TRect);
//begin
//  Rect := GetTextRect;
//  Inc(Rect.Top, (ScreenRow - 1) * FLineHeight);
//  Inc(Rect.Left, (ScreenCol - 1) * FCharFrameSize.x);
//
//  Rect.Bottom := Rect.Top + FLineHeight;
//  Rect.Right := Rect.Left + FCharFrameSize.x;
//end;

procedure TCnVirtualTextControl.GetVirtualCharPosVirtualRect(ARow,
  ACol: Integer; var Rect: TRect);
var
  DW: Boolean;
  DV: Integer;
begin
  if not CalcPixelOffsetFromColumnInLine(ARow, ACol, Rect, DW) then
    raise ECnTextControlException.Create(SCnErrorColumn);

  // 拿到的 Rect 是一行内的字符方框坐标，左上角为完整行头，需要转换成虚拟排版区内坐标
  // 注意横向不用改动

  // 返回的纵坐标加上纵向滚动的像素数就是新的 Rect.Top
  DV := (ARow - 1) * FLineHeight;
  Inc(Rect.Top, DV);
  Inc(Rect.Bottom, DV);
end;

procedure TCnVirtualTextControl.GetVirtualCharPosPhysicalRect(ARow, ACol: Integer;
  var Rect: TRect);
var
  DW: Boolean;
  DH, DV: Integer;
begin
  if not CalcPixelOffsetFromColumnInLine(ARow, ACol, Rect, DW) then
    raise ECnTextControlException.Create(SCnErrorColumn);

  // 拿到的 Rect 是一行内的字符方框坐标，左上角为完整行头，需要转换成控件内坐标

  // 返回的纵坐标加上纵向滚动的像素数就是 Rect.Top 离 TextRect 上边的距离
  DV := GetTextRect.Top + ((LineNumberToScreenLineNumber(ARow) - 1) * FLineHeight);
  Inc(Rect.Top, DV);
  Inc(Rect.Bottom, DV);

  DH := GetTextRect.Left - GetHoriPixelsOffset;

  // 返回的横坐标减去横向滚动的像素数就是 Rect.Left 离 TextRect 左边的距离
  Inc(Rect.Left, DH);
  Inc(Rect.Right, DH);
end;

function TCnVirtualTextControl.CalcColumnFromPixelOffsetInLine(ARow,
  VirtualX: Integer; out Col: Integer; out LeftHalf, DoubleWidth: Boolean): Boolean;
var
  T: Integer;
begin
  // 等宽字体这样计算没啥问题，但不适用汉字
  // 非等宽字体就惨了，得子类好好实现
  Col := (VirtualX div FAveCharWidth) + 1;

  T := VirtualX - (Col - 1) * FAveCharWidth;
  LeftHalf := T <= FCharFrameWidthHalf;
  DoubleWidth := False;

  Result := True;
end;

function TCnVirtualTextControl.GetPaintLineNumber(LineNumber: Integer): Integer;
begin
  Result := LineNumber;
end;

function TCnVirtualTextControl.GetTextRect: TRect;
begin
  Result.Top := COMMON_MARGIN;
  Result.Left := GetTextRectLeft;
  Result.Bottom := Result.Top + FLineHeight * VisibleLineCount;
  Result.Right := ClientWidth;
end;

function TCnVirtualTextControl.GetTextRectLeft: Integer;
begin
  Result := LEFT_MARGIN;
  if FShowLineNumber then
    Inc(Result, LEFT_MARGIN + FLineNumWidth + COMMON_MARGIN + SEP_WIDTH);
    // 文本区左边距往右移动，移的距离为行号区宽度加上划线宽度
end;

function TCnVirtualTextControl.GetTopLine: Integer;
begin
  Result := FVertOffset + 1;
end;

function TCnVirtualTextControl.GetBottomLine: Integer;
begin
  Result := FVertOffset + GetVisibleLineCount;
  if Result > FMaxLineCount then
    Result := FMaxLineCount;
end;

function TCnVirtualTextControl.GetVisibleLineCount: Integer;
begin
  if HandleAllocated then
    Result := (ClientHeight - COMMON_MARGIN * 2) div FLineHeight
  else
    Result := 0;
end;

procedure TCnVirtualTextControl.NavigationKey(Key: WORD; Shift: TShiftState);
var
  Msg: TWMScroll;
begin
  if FUseCaret then
  begin
    // 处理有光标时的方向键
    if not (ssShift in Shift) then
    begin
      // 没按 Shift，不是选择模式，纯移动光标，先取消选择并更新界面
      if SyncSelectionStartEnd(True) then
        Invalidate;

      case Key of
        VK_LEFT:
          begin
            // 虚拟光标左移并保持可见
            CaretCol := GetPrevColumn(CaretCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_RIGHT:
          begin
            // 虚拟光标右移并保持可见
            CaretCol := GetNextColumn(CaretCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_UP:
          begin
            // 虚拟光标上移并保持可见
            CaretRow := CaretRow - 1;
            ScrollToVisibleCaret;
          end;
        VK_DOWN:
          begin
            // 虚拟光标下移并保持可见
            CaretRow := CaretRow + 1;
            ScrollToVisibleCaret;
          end;
        VK_PRIOR:
          begin
            CaretRow := CaretRow - GetVisibleLineCount;
            ScrollToVisibleCaret;
          end;
        VK_NEXT:
          begin
            CaretRow := CaretRow + GetVisibleLineCount;
            ScrollToVisibleCaret;
          end;
        VK_HOME:
          begin
            if ssCtrl in Shift then // Ctrl 按下时回到首行首列
              CaretRow := 1;

            CaretCol := 1; // 回到首列
            ScrollToVisibleCaret;
          end;
        VK_END:
          begin
            if ssCtrl in Shift then // Ctrl 按下时回到尾行尾列
              CaretRow := FMaxLineCount;

            CaretCol := GetLastColumnFromLine(FMaxLineCount); // 回到尾列
            ScrollToVisibleCaret;
          end;
      end;
    end
    else
    begin
      // TODO: 按了 Shift，变更选择区终点位置
      if not FUseSelection then
        Exit;

      case Key of
        VK_LEFT:
          begin
            // 选择区终点列左移并保持可见
            SelectEndCol := GetPrevColumn(SelectEndCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_RIGHT:
          begin
            // 选择区终点列右移并保持可见
            SelectEndCol := GetNextColumn(SelectEndCol, CaretRow);;
            ScrollToVisibleCaret;
          end;
        VK_UP:
          begin
            // 选择区终点行上移并保持可见
            SelectEndRow := SelectEndRow - 1;
            ScrollToVisibleCaret;
          end;
        VK_DOWN:
          begin
            // 选择区终点行下移并保持可见
            SelectEndRow := SelectEndRow + 1;
            ScrollToVisibleCaret;
          end;
        VK_PRIOR:
          begin
            SelectEndRow := SelectEndRow - GetVisibleLineCount;
            ScrollToVisibleCaret;
          end;
        VK_NEXT:
          begin
            SelectEndRow := SelectEndRow + GetVisibleLineCount;
            ScrollToVisibleCaret;
          end;
        VK_HOME:
          begin
            if ssCtrl in Shift then // Ctrl 按下时选择到首行首列
              SelectEndRow := 1;

            SelectEndCol := 1; // 选择到首列
            ScrollToVisibleCaret;
          end;
        VK_END:
          begin
            if ssCtrl in Shift then // Ctrl 按下时选择到尾行尾列
              SelectEndRow := FMaxLineCount;

            SelectEndCol := GetLastColumnFromLine(FMaxLineCount); // 选择到尾列
            ScrollToVisibleCaret;
          end;
      end;
    end;
  end
  else // 没有光标，单纯移动绘制区域，不处理 Ctrl 到文件头尾的情况
  begin
    case Key of
      VK_LEFT: ScrollLeftCol;
      VK_RIGHT: ScrollRightCol;
      VK_UP: ScrollUpLine;
      VK_DOWN: ScrollDownLine;
      VK_PRIOR: ScrollUpPage;
      VK_NEXT: ScrollDownPage;
      VK_HOME:
        begin
          Msg.ScrollCode := SB_THUMBTRACK;
          Msg.Pos := 0;
          WMHScroll(Msg);

          if ssCtrl in Shift then
          begin
            Msg.ScrollCode := SB_THUMBTRACK;
            Msg.Pos := 0;
            WMVScroll(Msg);
          end;
        end;
      VK_END:
        begin
          if ssCtrl in Shift then
          begin
            Msg.ScrollCode := SB_THUMBTRACK;
            Msg.Pos := FMaxLineCount;
            WMVScroll(Msg);
          end;

          Msg.ScrollCode := SB_THUMBTRACK;
          Msg.Pos := DEFAULT_MAX_WIDTH;
          WMHScroll(Msg);
        end;
    end;
  end;
end;

procedure TCnVirtualTextControl.KeyDown(var Key: WORD; Shift: TShiftState);
begin
  inherited;
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);

  case Key of
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END, VK_LEFT, VK_RIGHT:
      NavigationKey(Key, Shift);
  end;
end;

procedure TCnVirtualTextControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if not Focused then
    Windows.SetFocus(Handle);

  UpdateCursorFrameCaret;

  if Button = mbLeft then
  begin
    FLeftMouseDown := True;
    FLeftMouseMoveAfterDown := False;
  end;
end;

procedure TCnVirtualTextControl.Paint;
var
  TR, LR, LineRect: TRect;
  LC: TColor;
  I, V, H: Integer;
  LineBmp: TBitmap;
  LineCanvas: TCanvas;
begin
  // 先画行号区
  TR := ClientRect;
  LR := ClientRect;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(TR);

  TR.Left := GetTextRectLeft;
  TR.Top := COMMON_MARGIN;
  V := VisibleLineCount;
  H := GetHoriPixelsOffset;

  if FShowLineNumber then // 绘制行号区的底色并调用默认的行号绘制
  begin
    if Focused then
      LC := FLineNumFocusBkColor
    else
      LC := FLineNumNoFocusBkColor;

    // 行号区宽度为 Margin + LineNumWidth + Margin
    // 绘制文字区的 Left 为 Margin，Width 为 LineNumWidth + Margin

    LR.Right := LEFT_MARGIN + FLineNumWidth + LEFT_MARGIN + SEP_WIDTH;  // SEP_WIDTH 是划线宽度

    Canvas.Brush.Color := LC;
    Canvas.FillRect(LR);

    Inc(LR.Left, LEFT_MARGIN);
    Inc(LR.Top, COMMON_MARGIN);

    Canvas.Pen.Color := clGray;
    Dec(LR.Right);
    Canvas.MoveTo(LR.Right, 0);
    Canvas.LineTo(LR.Right, ClientHeight);
    Canvas.Pen.Color := clWhite;
    Dec(LR.Right);
    Canvas.MoveTo(LR.Right, 0);
    Canvas.LineTo(LR.Right, ClientHeight);
    Canvas.Pen.Color := clSilver;
    Dec(LR.Right);
    Canvas.MoveTo(LR.Right, 0);
    Canvas.LineTo(LR.Right, ClientHeight);

    LR.Bottom := COMMON_MARGIN + FLineHeight;

    Canvas.Font.Color := FLineNumColor;
    Canvas.Brush.Style := bsClear;

    for I := 1 to V do
    begin
      if I + FVertOffset <= FMaxLineCount then
        DoPaintLineNum(I, GetPaintLineNumber(I + FVertOffset), LR);

      Inc(LR.Top, FLineHeight);
      Inc(LR.Bottom, FLineHeight);
    end;
  end;

  TR.Bottom := TR.Top + FLineHeight;
  // TR 是控件内部要绘制的当前行的物理框

  LineBmp := TBitmap.Create;
  LineBmp.PixelFormat := pf24bit;
  LineBmp.Height := FLineHeight;
  LineBmp.Width := H + (FTextRect.Right - FTextRect.Left);

  LineCanvas := LineBmp.Canvas;
  LineCanvas.Font.Assign(Font);

  try
    for I := 1 to V do // I 是物理行
    begin
      LineRect := Bounds(0, 0, LineBmp.Width, LineBmp.Height);

      // 绘制 LineBmp Background
      DoPaintLineBackground(LineCanvas, I + FVertOffset, LineRect);

      // 子类绘制内容
      DoPaintLine(LineCanvas, I + FVertOffset, FHoriOffset, LineRect);

      // 内容复制过去
      BitBlt(Canvas.Handle, TR.Left, TR.Top, LineBmp.Width - H,
        LineBmp.Height, LineCanvas.Handle, H, 0, SRCCOPY);

      Inc(TR.Top, FLineHeight);
      Inc(TR.Bottom, FLineHeight);
    end;
  finally
    LineBmp.Free;
  end;
end;

function TCnVirtualTextControl.ScreenLineNumberToLineNumber(
  ScreenLineNumber: Integer): Integer;
begin
  Result := ScreenLineNumber + FVertOffset;
end;

procedure TCnVirtualTextControl.SetLineNumColor(const Value: TColor);
begin
  if FLineNumColor <> Value then
  begin
    FLineNumColor := Value;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetLineNumFocusBkColor(const Value: TColor);
begin
  if FLineNumFocusBkColor <> Value then
  begin
    FLineNumFocusBkColor := Value;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetLineNumNoFocusBkColor(const Value: TColor);
begin
  if FLineNumNoFocusBkColor <> Value then
  begin
    FLineNumNoFocusBkColor := Value;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetMaxLineCount(const Value: Integer);
var
  Old: Integer;
begin
  if FMaxLineCount <> Value then
  begin
    FMaxLineCount := Value;

    Old := FLineNumCount;
    FLineNumCount := GetNumWidth(Value);
    FLineNumPattern := StringOfChar('0', FLineNumCount);

    if FLineNumCount <> Old then // 位数发生变化才重新计算 FLineNumWidth
    begin
      CalcMetrics;
      UpdateRects;
      UpdateScrollBars;
    end;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetShowLineNumber(const Value: Boolean);
begin
  if FShowLineNumber <> Value then
  begin
    FShowLineNumber := Value;
    UpdateRects;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.UpdateScrollBars;
var
  SI: TScrollInfo;
begin
  if not HandleAllocated then
    Exit;

  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;
  SI.nMin := 0;

  // 纵向滚动条
  FVertExp := 0;
  SI.nMax := FMaxLineCount - 1;       // nMax 是最大行数
  while SI.nMax > MAX_NO_EXP_LINES do // 行号太多时以指数方式调整，避免太细
  begin
    SI.nMax := SI.nMax div 2;
    Inc(FVertExp);
  end;

  SI.nPage := VisibleLineCount shr FVertExp; // nPage 是一屏内容对应的高度
  SI.nPos := FVertOffset shr FVertExp;
  SetScrollInfo(Handle, SB_VERT, SI, True);

  // 横向滚动条
  SI.nMax := DEFAULT_MAX_WIDTH;              // 不知道最宽，写死 256 先
  SI.nPage := ClientWidth div FAveCharWidth;    // nPage 是一屏内容对应的字符宽度
  SI.nPos := FHoriOffset;
  SetScrollInfo(Handle, SB_HORZ, SI, True);
end;

procedure TCnVirtualTextControl.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TCnVirtualTextControl.WMHScroll(var message: TWMScroll);
var
  SI: TScrollInfo;
  Old: Integer;
begin
  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  GetScrollInfo(Handle, SB_HORZ, SI);

  Old := FHoriOffset;
  case message.ScrollCode of
    SB_PAGEUP: Dec(FHoriOffset, (FTextRect.Right - FTextRect.Left) div FAveCharWidth);  // 横向一屏的宽度
    SB_PAGEDOWN: Inc(FHoriOffset, (FTextRect.Right - FTextRect.Left) div FAveCharWidth);
    SB_LINEUP: Dec(FHoriOffset);
    SB_LINEDOWN: Inc(FHoriOffset);
    SB_THUMBTRACK: FHoriOffset := message.Pos;
  end;

  if FHoriOffset > SI.nMax - (FTextRect.Right - FTextRect.Left) div FAveCharWidth then
    FHoriOffset := SI.nMax - (FTextRect.Right - FTextRect.Left) div FAveCharWidth;

  if FHoriOffset < 0 then
    FHoriOffset := 0;

  if FHoriOffset = Old then
    Exit;

  SI.nPos := FHoriOffset;
  SetScrollInfo(Handle, SB_HORZ, SI, True);

  Refresh;

  // 滚动时虚拟光标不动，物理光标可能会动
  SyncCaretPosition;
  SyncSelectionStartEnd;

  DoScroll;
end;

procedure TCnVirtualTextControl.WMKillFocus(var message: TWMSetFocus);
begin
  inherited;
  DestroyCaret;
  FCaretVisible := False;

  Invalidate;
end;

procedure TCnVirtualTextControl.WMMouseWheel(var message: TWMMouseWheel);
var
  I: Integer;
begin
  // 注意滚动时 FCaretRow 和 FCaretCol 不变，并且可能跑到 TextRect 外头去
  if GetKeyState(VK_CONTROL) < 0 then
  begin
    FIsWheeling := True;
    try
      if message.WheelDelta > 0 then
        ScrollUpPage
      else
        ScrollDownPage;
    finally
      FIsWheeling := False;
    end;
  end
  else
  begin
    FIsWheeling := True;
    try
      if message.WheelDelta > 0 then
      begin
        for I := 0 to FWheelLinesCount - 1 do
          ScrollUpLine;
      end
      else
      begin
        for I := 0 to FWheelLinesCount - 1 do
          ScrollDownLine;
      end;
    finally
      FIsWheeling := False;
    end;
  end;
end;

procedure TCnVirtualTextControl.WMSetFocus(var message: TWMSetFocus);
begin
  inherited;

  if FLineHeight <= 0 then
    CalcMetrics;

  if FUseCaret then
  begin
    CreateCaret(Handle, HBITMAP(0), 2, FLineHeight);
    SetCaretBlinkTime(GetCaretBlinkTime);

    DisplayCaret(True);
  end;

  Invalidate;
end;

procedure TCnVirtualTextControl.WMSetFont(var message: TMessage);
begin
  inherited;
  Canvas.Font := Font;

  CalcMetrics;
  UpdateRects;
  UpdateScrollbars;
end;

procedure TCnVirtualTextControl.WMSize(var message: TWMSize);
begin
  inherited;
  UpdateRects;
  UpdateScrollBars;
end;

procedure TCnVirtualTextControl.WMVScroll(var message: TWMScroll);
var
  SI: TScrollInfo;
  Old, VL: Integer;
begin
  VL := VisibleLineCount;
  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  GetScrollInfo(Handle, SB_VERT, SI);

  Old := FVertOffset;
  case message.ScrollCode of
    SB_PAGEUP: Dec(FVertOffset, VL);
    SB_PAGEDOWN: Inc(FVertOffset, VL);
    SB_LINEUP: Dec(FVertOffset);
    SB_LINEDOWN: Inc(FVertOffset);
    SB_THUMBTRACK: FVertOffset := message.Pos shl FVertExp;
  end;

  if FVertOffset > FMaxLineCount - VL then
    FVertOffset := FMaxLineCount - VL;
  if FVertOffset < 0 then
    FVertOffset := 0;

  if FVertOffset = Old then
    Exit;

  SI.nPos := FVertOffset shr FVertExp;
  SetScrollInfo(Handle, SB_VERT, SI, True);

  Refresh;

  // 滚动时虚拟光标一般不动，所以物理光标会动
  SyncCaretPosition;
  SyncSelectionStartEnd;

  DoScroll;
end;

procedure TCnVirtualTextControl.SetUseCaret(const Value: Boolean);
begin
  if FUseCaret <> Value then
  begin
    FUseCaret := Value;
    if FUseCaret then   // 只创建
    begin
      if HandleAllocated then
      begin
        CreateCaret(Handle, HBITMAP(0), 2, FLineHeight - 2);
        SetCaretBlinkTime(GetCaretBlinkTime);

        DisplayCaret(Focused);
      end;
    end
    else
    begin
      DisplayCaret(False);
      DestroyCaret;
    end;
  end;
end;

procedure TCnVirtualTextControl.DisplayCaret(CaretVisible: Boolean);
begin
  if CaretVisible and Focused then
  begin
    if HandleAllocated then
    begin
      // FCaretVisible := True;
      SyncCaretPosition;
    end;
  end
  else if not CaretVisible then
  begin
    HideCaret(Handle);
    FCaretVisible := False;
  end;
end;

procedure TCnVirtualTextControl.UpdateCursorFrameCaret;
var
  P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  if CalcRowCol(P, FCaretRow, FCaretCol, FCharFrameIsLeft, FCharFrameDoubleWidth) then
  begin
    // 设置光标位置
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

function TCnVirtualTextControl.ScreenColNumberToColNumber(
  ScreenColNumber: Integer): Integer;
begin
  Result := ScreenColNumber + FHoriOffset;
end;

procedure TCnVirtualTextControl.SetCaretCol(const Value: Integer);
begin
  FCaretCol := Value;

  // 根据 FCaretRow 判断 FCaretCol 是否超出行尾
  // 如果超出，则重新修正 FCaretCol
  LimitRowColumnInLine(FCaretRow, FCaretCol);

  if FUseCaret then
  begin
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

procedure TCnVirtualTextControl.SetCaretRow(const Value: Integer);
begin
  FCaretRow := Value;

  // 根据 FCaretRow 判断 FCaretRow 是否大于 FMaxLineCount 并且 FCaretCol 是否超出行尾
  // 如果超出，则重新修正 FCaretCol
  LimitRowColumnInLine(FCaretRow, FCaretCol);

  // 同步更新 ScreenCaretCol 和 ScreenCaretRow
  if FUseCaret then
  begin
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

procedure TCnVirtualTextControl.UpdateRects;
begin
  if not HandleAllocated then
    Exit;

  FTextRect.Left := GetTextRectLeft;
  FTextRect.Top := COMMON_MARGIN;
  FTextRect.Bottom := ClientRect.Bottom - COMMON_MARGIN;
  FTextRect.Right := ClientRect.Right - COMMON_MARGIN;

  FGutterRect.Top := COMMON_MARGIN;
  FGutterRect.Bottom := COMMON_MARGIN;
  if ShowLineNumber then
  begin
    FGutterRect.Left := LEFT_MARGIN;
    FGutterRect.Right := LEFT_MARGIN + FLineNumWidth + COMMON_MARGIN;
  end
  else
  begin
    FGutterRect.Left := 0;
    FGutterRect.Right := 0;
  end;
end;

function TCnVirtualTextControl.GetScreenBottomLine: Integer;
begin
  Result := GetBottomLine - GetTopLine + 1;
end;

function TCnVirtualTextControl.ColNumberToScreenColNumber(
  ColNumber: Integer): Integer;
begin
  Result := ColNumber - FHoriOffset;
end;

function TCnVirtualTextControl.LineNumberToScreenLineNumber(
  LineNumber: Integer): Integer;
begin
  Result := LineNumber - FVertOffset;
end;

procedure TCnVirtualTextControl.DoCaretChange;
begin
  if Assigned(FOnCaretChange) then
    FOnCaretChange(Self);
end;

procedure TCnVirtualTextControl.SetCaretAfterLineEnd(const Value: Boolean);
begin
  if FCaretAfterLineEnd <> Value then
  begin
    FCaretAfterLineEnd := Value;
    LimitRowColumnInLine(FCaretRow, FCaretCol);

    // 设置光标位置
    if FUseCaret then
    begin
      SyncCaretPosition;
      SyncSelectionStartEnd;
      DoCaretChange;
    end;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.LimitRowColumnInLine(var LineNumber, Column: Integer);
var
  C: Integer;
begin
  if LineNumber <= 0 then
    LineNumber := 1;

  if LineNumber > FMaxLineCount then
    LineNumber := FMaxLineCount;

  if Column <= 0 then
    Column := 1;

  if not CaretAfterLineEnd then
  begin
    C := GetLastColumnFromLine(LineNumber);
    if C < Column then
    begin
      Column := C;
      if Column <= 0 then
        Column := 1;
    end;
  end;
end;

procedure TCnVirtualTextControl.SetSelectEndCol(const Value: Integer);
begin
  if FSelectEndCol <> Value then
  begin
    FSelectEndCol := Value;
    LimitRowColumnInLine(FSelectEndRow, FSelectEndCol);
    SetCaretRowCol(FSelectEndRow, FSelectEndCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.SetSelectEndRow(const Value: Integer);
begin
  if FSelectEndRow <> Value then
  begin
    FSelectEndRow := Value;
    LimitRowColumnInLine(FSelectEndRow, FSelectEndCol);
    SetCaretRowCol(FSelectEndRow, FSelectEndCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.SetSelectStartCol(const Value: Integer);
begin
  if FSelectStartCol <> Value then
  begin
    FSelectStartCol := Value;
    LimitRowColumnInLine(FSelectStartRow, FSelectStartCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.SetSelectStartRow(const Value: Integer);
begin
  if FSelectStartRow <> Value then
  begin
    FSelectStartRow := Value;
    LimitRowColumnInLine(FSelectStartRow, FSelectStartCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
  TR: TRect;
begin
  inherited;

  if FLeftMouseDown then
  begin
    P.x := X;
    P.y := Y;

    // 拖动到了边缘，先滚动一次区域，无需 SetCapture
    TR := GetTextRect;
    if P.x < TR.Left then
      ScrollLeftCol
    else if P.x > TR.Right then
      ScrollRightCol;

    if P.y < TR.Top then
      ScrollUpLine
    else if P.y > TR.Bottom then
      ScrollDownLine;

    if FUseSelection then     // 左键拖动时如果支持选择区，则更新选择区尾
    begin
      if not FLeftMouseMoveAfterDown then // 本次拖动是 MouseDown 后的首次 Move
      begin
        // TODO: 判断是否在选择区内再次拖动，如果是则处理拖拽，现在简单地取消选择准备再次选择
        SyncSelectionStartEnd(True);
      end;

      FLeftMouseMoveAfterDown := True;

      CalcSelectEnd(P); // 然后再连续更新这次的选择区
      SetCaretRowCol(FSelectEndRow, FSelectEndCol); // 同时也移动光标
    end
    else
    begin
      UpdateCursorFrameCaret; // 左键拖动时如果不支持选择区，则也移动光标
      ScrollToVisibleCaret;
    end;
  end;
end;

procedure TCnVirtualTextControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button = mbLeft then
  begin
    FLeftMouseDown := False;
    if FUseSelection then  // 鼠标左键抬起时，要通过什么条件取消当前选择区？本次未拖动
    begin
      if not FLeftMouseMoveAfterDown then // 拖动过的啥都不做，未拖动过的取消选择
      begin
        SyncSelectionStartEnd(True);
        DoSelectChange;
      end;
    end;
  end;
end;

procedure TCnVirtualTextControl.SetCaretRowCol(Row, Col: Integer);
begin
  FCaretRow := Row;
  FCaretCol := Col;

  // 根据 FCaretRow 判断 FCaretCol 是否超出行尾
  // 如果超出，则重新修正 FCaretCol
  LimitRowColumnInLine(FCaretRow, FCaretCol);

  if FUseCaret then
  begin
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

procedure TCnVirtualTextControl.ScrollToVisibleCaret;
var
  M: Boolean;
  L: Integer;
begin
  if not HandleAllocated and not FUseCaret then
    Exit;

  M := False;
  if FCaretRow < GetTopLine then
  begin
    Dec(FVertOffset, GetTopLine - FCaretRow);
    M := True;
  end
  else if FCaretRow > GetBottomLine then
  begin
    Inc(FVertOffset, FCaretRow - GetBottomLine);
    M := True;
  end;

  // 非等宽列，不能直接比较列。得先计算出当前 Column 对应虚拟文字区的左侧距，与已滚过的左侧距比较
  L := GetColumnVirtualX(FCaretRow, FCaretCol);
  if GetHoriPixelsOffset > L - CARET_MARGIN then
  begin
    // 如果已滚过的左侧距更大，说明当前 Column 隐藏在左侧，需要减少左侧距到刚好比 L 对应的字符宽度小一点点
    FHoriOffset := L div FAveCharWidth;
    M := True;
  end
  else if GetHoriPixelsOffset + (FTextRect.Right - FTextRect.Left) < L + CARET_MARGIN then
  begin
    // 如果已滚过的左侧距加文本区宽度（也就是已滚过的右侧距）太小，说明当前 Column 隐藏在右侧，
    // 需要增加左侧距到右侧距对应的字符宽度大一点点
    FHoriOffset := ((L - (FTextRect.Right - FTextRect.Left)) div FAveCharWidth) + 1;
    M := True;
  end;

  if M then
  begin
    Invalidate;
    SyncCaretPosition;
    UpdateScrollBars;
    DoScroll;
  end;
end;

procedure TCnVirtualTextControl.SetUseSelection(const Value: Boolean);
begin
  if FUseSelection <> Value then
  begin
    FUseSelection := Value;
    SyncSelectionStartEnd(True);
    Invalidate;
  end;
end;

function TCnVirtualTextControl.HasSelection: Boolean;
begin
  Result := (FSelectStartRow <> FSelectEndRow) or (FSelectStartCol <> FSelectEndCol);
end;

function TCnVirtualTextControl.SyncSelectionStartEnd(Force: Boolean): Boolean;
begin
  if FUseSelection and (Force or not HasSelection) then
  begin
    FSelectStartRow := FCaretRow;
    FSelectEndRow := FCaretRow;
    FSelectStartCol := FCaretCol;
    FSelectEndCol := FCaretCol;

    Result := True;
  end
  else
    Result := False;
end;

function TCnVirtualTextControl.CalcRowCol(Pt: TPoint; out ACaretRow, ACaretCol: Integer;
  out ACharFrameIsLeft, ACharFrameDoubleWidth: Boolean): Boolean;
begin
  Result := ClientPosToVirtualCharPos(Pt, ACaretRow, ACaretCol, ACharFrameIsLeft,
    ACharFrameDoubleWidth);

  if Result then
  begin
    if not ACharFrameIsLeft then
      ACaretCol := GetNextColumn(ACaretCol, ACaretRow);

    // 通过虚拟 Row/Col 判断限制
    LimitRowColumnInLine(ACaretRow, ACaretCol);
  end;
end;

procedure TCnVirtualTextControl.DoSelectChange;
begin
  if Assigned(FOnSelectChange) then
    FOnSelectChange(Self);
end;

procedure TCnVirtualTextControl.CalcSelectEnd(Pt: TPoint);
var
  ACaretRow, ACaretCol: Integer;
  OldSelEndRow, OldSelEndCol: Integer;
  ACharFrameIsLeft, ACharFrameDoubleWidth: Boolean;
begin
  // 处理拖动选择，注意 Down 时已经确定好了选择起始处
  OldSelEndRow := FSelectEndRow;
  OldSelEndCol := FSelectEndCol;

  if CalcRowCol(Pt, ACaretRow, ACaretCol, ACharFrameIsLeft, ACharFrameDoubleWidth) then
  begin
    // 光标在文字区内或文字区左侧包括行号区
    FSelectEndRow := ACaretRow;
    FSelectEndCol := ACaretCol;

    LimitRowColumnInLine(FSelectEndRow, FSelectEndCol); // 限制避免拖出范围

    if (FSelectEndRow <> OldSelEndRow) or (FSelectEndCol <> OldSelEndCol) then
    begin
      Invalidate;
      DoSelectChange;
    end;
  end;
end;

procedure TCnVirtualTextControl.SetOnSelectChange(
  const Value: TNotifyEvent);
begin
  FOnSelectChange := Value;
end;

procedure TCnVirtualTextControl.ScrollDownLine;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINEDOWN;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollDownPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGEDOWN;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollLeftCol;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINELEFT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollLeftPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGELEFT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollRightCol;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINERIGHT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollRightPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGERIGHT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollUpLine;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINEUP;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollUpPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGEUP;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  UpdateRects;
  UpdateScrollBars;
end;

function TCnVirtualTextControl.GetNextColumn(AColumn,
  ARow: Integer): Integer;
begin
  Result := AColumn + 1;
end;

function TCnVirtualTextControl.GetPrevColumn(AColumn,
  ARow: Integer): Integer;
begin
  Result := AColumn - 1;
end;

function TCnVirtualTextControl.GetHoriPixelsOffset: Integer;
begin
  Result := FHoriOffset * FAveCharWidth;
end;

function TCnVirtualTextControl.CalcPixelOffsetFromColumnInLine(
  ARow, ACol: Integer; out Rect: TRect; out DoubleWidth: Boolean): Boolean;
begin
  Rect.Top := 0;
  Rect.Left := FAveCharWidth * (ACol - 1);
  Rect.Right := Rect.Left + FAveCharWidth;
  Rect.Bottom := FLineHeight;

  DoubleWidth := False;
  Result := True;
end;

function TCnVirtualTextControl.GetScreenCaretRow: Integer;
begin
  Result := LineNumberToScreenLineNumber(FCaretRow);
end;

function TCnVirtualTextControl.GetColumnVirtualX(ARow,
  ACol: Integer): Integer;
var
  R: TRect;
begin
  GetVirtualCharPosVirtualRect(ARow, ACol, R);
  Result := R.Left;
end;

function TCnVirtualTextControl.ClientXToVirtualX(X: Integer): Integer;
begin
  // 物理横坐标，减去文字区的物理左坐标，就是该点离文字区的左边距，加上横向滚动长度
  Result := X - GetTextRect.Left + GetHoriPixelsOffset;
end;

function TCnVirtualTextControl.VirtualXToClientX(X: Integer): Integer;
begin
  Result := X - GetHoriPixelsOffset + GetTextRect.Left
end;

procedure TCnVirtualTextControl.SyncCaretPosition;
var
  R: TRect;
begin
  if FUseCaret then
  begin
    GetVirtualCharPosPhysicalRect(FCaretRow, FCaretCol, R);
    if (R.Left >= FTextRect.Left) and (R.Left <= FTextRect.Right) then
    begin
      if not FCaretVisible then
      begin
        ShowCaret(Handle);
        FCaretVisible := True;
      end;
      SetCaretPos(R.Left, R.Top);
    end
    else
    begin
      if FCaretVisible then
      begin
        HideCaret(Handle);
        FCaretVisible := False;
      end;
    end;
  end;
end;

procedure TCnVirtualTextControl.DoPaintLineBackground(LineCanvas: TCanvas;
  LineNumber: Integer; LineRect: TRect);
begin
  LineCanvas.Brush.Color := Color;
  LineCanvas.Brush.Style := bsSolid;
  LineCanvas.FillRect(LineRect);
end;

end.
