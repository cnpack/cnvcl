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

{*******************************************************}
{                                                       }
{       具有类似Visual Studio.NET的停靠风格             }
{       CnVSNETDockStyle 单元                           }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnVSNETDockStyle;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：具有类似Visual Studio.NET的停靠风格的单元 
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.07.13 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, Controls, Math, Messages, Graphics, ComCtrls, Extctrls,
  ImgList, Forms, CnConsts, CnCompConsts, CnDockFormControl,
  CnDockSupportControl, CnDockTree, CnVIDDockStyle;

const
  { 自动隐藏按钮 }
  HTAUTOHIDE  = 40;
  { 默认的VSNET风格的把手的大小 }
  DefaultVSNETGrabberSize = 19;
  { 当一个Block中的Pane获得焦点的时候他允许的最大宽度 }
  MaxActivePaneWidth = 100;
  { 没有获得焦点的Tab的字体颜色 }
  VSNETPageInactiveFontColor  = $00525552;
  { 没有获得焦点的Tab的颜色 }
  VSNETPageInactiveSheetColor = $00EFF3F7;

type

  TCnVSNETConjoinServerOption = class(TCnVIDConjoinServerOption)
  protected
    procedure SetDefaultSystemCaptionInfo; override;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
  end;

  TCnVSNETTabServerOption = class(TCnVIDTabServerOption)
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
  end;

  { Channel的选项类 }
  TCnVSNETChannelOption = class(TCnBasicServerOption)
  private
    FActivePaneSize: Integer; //获得焦点的Pane的最大值
    FShowImage: Boolean;      //显示图标
    procedure SetActivePaneSize(const Value: Integer);
    procedure SetShowImage(const Value: Boolean);
  protected
    procedure ResetDockControlOption; override;
    { 重新设置ADockServer的配置 }
    procedure ResetDockServerOption(ADockServer: TCnDockServer); override;
    { 重新设置ADockClient的配置 }
    procedure ResetDockClientOption(ADockClient: TCnDockClient); override;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
  published
    property ActivePaneSize: Integer read FActivePaneSize write SetActivePaneSize;
    property ShowImage: Boolean read FShowImage write SetShowImage;
  end;

  TCnVSNETChannelOptionClass = class of TCnVSNETChannelOption;

  TCnVSBlock = class;
  TCnVSChannel = class;
  TCnVSNETDockPanel = class;
  TCnVSPopupPanel = class;
  TCnVSPopupPanelSplitter = class;

  TCnVSPane = class(TObject)
  public
    Block: TCnVSBlock;
    DockForm: TForm;
    Index: Integer;  // 在Block中的索引
    Width: Integer;  // 宽度
    Active: Boolean; // 是否处于激活状态
    Visible: Boolean;// 是否可见
    constructor Create(ABlock: TCnVSBlock; AForm: TForm; AWidth: Integer; AIndex: Integer); virtual;
    destructor Destroy; override;
  end;

  // 组块的类型,分别是平铺方式，分页方式
  TBlockType = (btConjoinBlock, btTabBlock);

  // 定义一个结构，用来存储每一个组块的信息
  TCnVSBlock = class(TObject)
  private
    FVSChannel: TCnVSChannel;
    // 停靠窗体的列表
    FVSPaneList: TList;
    // 获得焦点的组块的宽度
    FActiveBlockWidth: Integer;
    // 失去焦点的组块的宽度
    FInactiveBlockWidth: Integer;
    // 获得焦点的停靠窗体
    FActiveDockControl: TWinControl;
    // 组块的类型,平铺或者分页
    FBlockType: TBlockType;
    // 用来显示图标的TImageList
    FImageList: TImageList;
    // 组块的开始位置
    FBlockStartPos: Integer;
    function GetVSPane(Index: Integer): TCnVSPane;
    function GetVSPaneCount: Integer;
  protected
    // 重新设置获得焦点的组块的宽度
    procedure ResetActiveBlockWidth;
    procedure DeletePane(Index: Integer);
    property ActiveBlockWidth: Integer read FActiveBlockWidth write FActiveBlockWidth;
    property InactiveBlockWidth: Integer read FInactiveBlockWidth write FInactiveBlockWidth;
    property ActiveDockControl: TWinControl read FActiveDockControl write FActiveDockControl;
    property BlockType: TBlockType read FBlockType;
    property VSChannel: TCnVSChannel read FVSChannel;
  public
    constructor Create(Owner: TCnVSChannel); virtual;
    destructor Destroy; override;
    // 添加一个DockForm
    procedure AddDockControl(Control: TWinControl);
    // 删除一个DockForm
    procedure RemoveDockControl(Control: TWinControl);
    // 获得组块占用了多长的宽度
    function GetTotalWidth: Integer;
    property VSPaneCount: Integer read GetVSPaneCount;
    property VSPanes[Index: Integer]: TCnVSPane read GetVSPane;
  end;

  // TCnVSChannel显示的状态
  TVSChannelState = (csShow, csHide);

  // 动画的类型，分别是显示或者隐藏
  TPopupPanelAnimateStyle = (pasShow, pasHide);

  // 用来显示VS.NET风格当停靠客户隐藏的时候的沟
  TCnVSChannel = class(TCustomControl)
  private
    // 弹出的停靠窗体
    FActiveDockForm: TForm;
    // 获得焦点的面板
    FActivePane: TCnVSPane;
    // 和哪个TCnVSNETDockPanel对应
    FVSNETDockPanel: TCnVSNETDockPanel;
    // 用来指示当前遍历的组块的开始位置
    FCurrentPos: Integer;
    // VSChannel是属于哪一个TCnDockServer
    FDockServer: TCnDockServer;
    // 组块的列表
    FBlockList: TList;
    // 沟的宽度
    FChannelWidth: Integer;
    // 组块离Channel开始位置的距离
    FBlockStartOffset: Integer;
    // 组块离
    FBlockUpOffset: Integer;
    // 组块之间的间隔距离
    FBlockInterval: Integer;
    // 显示弹出的停靠窗体的容器
    FVSPopupPanel: TCnVSPopupPanel;
    // 容器的分割条
    FVSPopupPanelSplitter: TCnVSPopupPanelSplitter;
    // 获得焦点的Pane的最大值
    FActivePaneSize: Integer;
    function GetBlockCount: Integer;
    function GetBlocks(Index: Integer): TCnVSBlock;
    // 得到组块的大小，其中Block为组块，Index为组块中的索引，ARect为得到的矩形大小
    procedure GetBlockRect(Block: TCnVSBlock; Index: Integer; var ARect: TRect);
    // 根据鼠标的位置MousePos得到指定的停靠窗体
    function GetDockFormWithMousePos(MousePos: TPoint): TCnVSPane;
    procedure SetVSPopupPanelSplitter(const Value: TCnVSPopupPanelSplitter);
    procedure SetBlockStartOffset(const Value: Integer);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure FreeBlockList;
    procedure SetActivePaneSize(const Value: Integer);
  protected
    { 重新设置字体的角度 }
    procedure ResetFontAngle; virtual;
    procedure ResetBlock; virtual;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure SetVSPopupPanelSplitterPosition;
    property ChannelWidth: Integer read FChannelWidth;
    property BlockStartOffset: Integer read FBlockStartOffset write SetBlockStartOffset;
    property BlockUpOffset: Integer read FBlockUpOffset;
    property BlockInterval: Integer read FBlockInterval;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { 根据输入的AControl查找到和他对应的TCnVSPane }
    function GetPaneWithControl(AControl: TControl): TCnVSPane;
    procedure CreateVSPopupPanel;   // 创建弹出的停靠窗体的容器
    procedure DestroyVSPopupPanel;  // 释放弹出的停靠窗体的容器
    procedure ResetPosition;
    // 添加一个DockForm
    procedure AddDockControl(Control: TWinControl);
    // 删除一个DockForm
    procedure RemoveDockControl(Control: TWinControl);
    // 查找一个DockForm，如果找到就返回它的索引，没有找到就返回-1
    function FindDockControl(Control: TWinControl; var BlockIndex: Integer;
      var PaneIndex: Integer): Boolean;
    function FindPane(Control: TWinControl): TCnVSPane;
    procedure PopupDockForm(Pane: TCnVSPane); overload;       // 弹出停靠窗体
    procedure PopupDockForm(Control: TWinControl); overload;  // 弹出停靠窗体
    procedure HidePopupPanel(Pane: TCnVSPane); overload;      // 隐藏弹出的TCnVSPopupPanel
    procedure HidePopupPanel(Control: TWinControl); overload; // 隐藏弹出的TCnVSPopupPanel
    procedure HidePopupPanelWithAnimate(Pane: TCnVSPane);     // 隐藏TCnVSPopupPanel并且伴随动画效果
    procedure ResetActivePaneWidth;  // 重新设置获得焦点的面板的宽度
    procedure ResetPopupPanelHeight; // 重新设置弹出Panel的高度;
    procedure RemoveAllBlock;// 删除所有的Block
    procedure DeleteBlock(Index: Integer);
    procedure AnimatePopupPanel(AnimateStyle: TPopupPanelAnimateStyle);
    property DockServer: TCnDockServer read FDockServer write FDockServer;
    property BlockCount: Integer read GetBlockCount;
    property Blocks[Index: Integer]: TCnVSBlock read GetBlocks;
    property VSPopupPanel: TCnVSPopupPanel read FVSPopupPanel;
    property VSPopupPanelSplitter: TCnVSPopupPanelSplitter read FVSPopupPanelSplitter
      write SetVSPopupPanelSplitter;
    property ActiveDockForm: TForm read FActiveDockForm;
    property ActivePaneSize: Integer read FActivePaneSize write SetActivePaneSize;
  end;

  { TCnVSChannel 的类引用(类元) }
  TCnVSChannelClass = class of TCnVSChannel;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnVSNETDockStyle = class(TCnVIDDockStyle)
  private
    FCnChannelOption: TCnVSNETChannelOption;
    FCnChannelOptionClass: TCnVSNETChannelOptionClass;
    procedure SetChannelOption(const Value: TCnVSNETChannelOption);
    function GetChannelOption: TCnVSNETChannelOption;
//    FCnVSChannelClass: TCnVSChannelClass;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure CreateConjoinServerOption(var Option: TCnBasicConjoinServerOption); override;
    procedure CreateTabServerOption(var Option: TCnBasicTabServerOption); override;
    { 捕获TCnDockServer的WindowProc消息，如果要还要执行默认的消息处理就返回False,否则就返回True }
    function DockServerWindowProc(DockServer: TCnDockServer; var Message: TMessage): Boolean; override;
    { 捕获TCnDockClient的WindowProc消息，如果要还要执行默认的消息处理就返回False,否则就返回True }
    function DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean; override;
    { ------------------------------------------------------------------------ }
    { 把ADockBaseControl添加到FDockBaseControlList中，
      如果已经存在了就不插入，反之插入到列表的结尾处 }
    procedure AddDockBaseControl(ADockBaseControl: TCnDockBaseControl); override;
    procedure CreateServerOption; override;
    procedure FreeServerOption; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetControlName: string; override;
    { ------------------------------------------------------------------------ }
    { ShowDockForm和HideDockForm都调用这个方法, 用AVisible来判断是显示还是隐藏 }
    procedure SetDockFormVisible(ADockClient: TCnDockClient; AVisible: Boolean);
    procedure ShowDockForm(ADockClient: TCnDockClient); override;// 显示ADockClient中的ParentForm;
    procedure HideDockForm(ADockClient: TCnDockClient); override;// 隐藏ADockClient中的ParentForm;
    { 得到ADockClient中的ParentForm是否可见 }
    function GetDockFormVisible(ADockClient: TCnDockClient): Boolean; override;
    { 还原原先的客户的状态 }
    procedure RestoreClient(DockClient: TCnDockClient); override;
//    property CnVSChannelClass: TCnVSChannelClass read FCnVSChannelClass write FCnVSChannelClass;
  published
    property ChannelOption: TCnVSNETChannelOption read GetChannelOption write SetChannelOption;
  end;

  TCnVSNETDockSplitter = class(TCnVIDDockSplitter);

  TCnVSNETDockPanel = class(TCnVIDDockPanel)
  private
    FVSChannelClass: TCnVSChannelClass;
    FVSChannel: TCnVSChannel;
  protected
    procedure SetDockServer(const Value: TCnDockServer); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateVSChannel;      // 创建和TCnVSNETDockPanel对应的VS.NET风格中的沟
    procedure DestroyVSChannel;     // 释放和TCnVSNETDockPanel对应的VS.NET风格中的沟
    procedure DoAutoHideControl(Control: TWinControl);
    procedure DoHideControl(Control: TWinControl);
    procedure DoShowControl(Control: TWinControl);
    property VSChannel: TCnVSChannel read FVSChannel;
  end;

  { 在VS.NET中用来显示弹出的停靠窗体的容器 }
  TCnVSPopupPanel = class(TCnVSNETDockPanel)
  private
    FVSNETDockPanel: TCnVSNETDockPanel;
    procedure SetVSNETDockPanel(const Value: TCnVSNETDockPanel);
    function GetVSChannel: TCnVSChannel;
  protected
    function CreateDockManager: IDockManager; override;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowDockPanel(MakeVisible: Boolean; Client: TControl;
      PanelSizeFrom: TSetDockPanelSizeFrom); override;
    property VSChannel: TCnVSChannel read GetVSChannel;
    property VSNETDockPanel: TCnVSNETDockPanel read FVSNETDockPanel
      write SetVSNETDockPanel;
  end;

  TCnVSNETConjoinPanel = class(TCnVIDConjoinPanel);

  { 按钮边框的状态，分别是凸起状态，正常状态，凹下状态 }
  TBtnState = (bsUp, bsNormal, bsDown);

  TCnVSNETDockZone = class(TCnVIDDockZone)
  private
    { 自动隐藏按钮是否被按下 }
    FAutoHideBtnDown: Boolean;
    { 自动隐藏按钮边框的状态 }
    FAutoHideBtnState: TBtnState;
    { 关闭按钮边框的状态 }
    FCloseBtnState: TBtnState;
    { 在VSChannel中的Pane是否是可见的 }
    FVSPaneVisible: Boolean;
    procedure SetAutoHideBtnState(const Value: TBtnState);
    procedure SetCloseBtnState(const Value: TBtnState);
    procedure SetAutoHideBtnDown(const Value: Boolean);
    procedure SetVSPaneVisible(const Value: Boolean);
  protected
    procedure DoCustomSetControlName; override;
    procedure SetChildControlVisible(Client: TControl; AViisible: Boolean); override;
    property AutoHideBtnDown: Boolean read FAutoHideBtnDown write SetAutoHideBtnDown;
    property AutoHideBtnState: TBtnState read FAutoHideBtnState write SetAutoHideBtnState;
    property CloseBtnState: TBtnState read FCloseBtnState write SetCloseBtnState;
    property VSPaneVisible: Boolean read FVSPaneVisible write SetVSPaneVisible;
  public
    constructor Create(Tree: TCnDockTree); override;
  end;

  TCnVSNETDockTree = class(TCnVIDDockTree)
  private
    FAutoHideZone: TCnVSNETDockZone;
  protected
    procedure IgnoreZoneInfor(Stream: TMemoryStream); override;
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); override;
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoLButtonDbClk(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    { 隐藏AZone中的ChildControl }
    procedure DoHideZoneChild(AZone: TCnDockZone); override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; override;
    procedure DrawDockGrabber(Control: TControl; const ARect: TRect); override;
    procedure PaintDockGrabberRect(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); override;
    procedure DrawCloseButton(Canvas: TCanvas; Zone: TCnDockZone;
      Left, Top: Integer); override;
    procedure DrawAutoHideButton(Zone: TCnDockZone;
      Left, Top: Integer); virtual;
    procedure GetCaptionRect(var Rect: TRect); override;
    { 其他的提示信息 }
    procedure DoOtherHint(Zone: TCnDockZone;
      HTFlag: Integer; var HintStr: string); override;
    procedure CustomSaveZone(Stream: TStream;
      Zone: TCnDockZone); override;
    procedure CustomLoadZone(Stream: TStream;
      var Zone: TCnDockZone); override;
    property AutoHideZone: TCnVSNETDockZone read FAutoHideZone
      write FAutoHideZone;
  public
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); override;
    destructor Destroy; override;
  end;

  TCnVSNETDockTabSheet = class(TCnVIDDockTabSheet)
  private
    FOldVisible: Boolean;
    procedure SetOldVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    property OldVisible: Boolean read FOldVisible write SetOldVisible;
  end;

  TCnVSNETTabPanel = class(TCnTabPanel)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCnVSNETTabPageControl = class(TCnVIDTabPageControl)
  protected
    procedure CreatePanel; override;
    procedure ShowControl(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCnVSNETDragDockObject = class(TCnVIDDragDockObject);

  TCnVSPopupPanelSplitter = class(TCustomControl)
  private
    FVSPopupPanel: TCnVSPopupPanel;
    FSplitWidth: Integer;
    FActiveControl: TWinControl;
    FAutoSnap: Boolean;
    FBeveled: Boolean;
    FBrush: TBrush;
    FControl: TControl;
    FDownPos: TPoint;
    FLineDC: HDC;
    FLineVisible: Boolean;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldKeyDown: TKeyEvent;
    FOldSize: Integer;
    FPrevBrush: HBrush;
    FResizeStyle: TResizeStyle;
    FSplit: Integer;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure AllocateLineDC;
    procedure CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
    procedure DrawLine;
    function FindControl: TControl;
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
    procedure SetVSPopupPanel(const Value: TCnVSPopupPanel);
    function GetVSChannelAlign: TAlign;
    procedure SetSplitWidth(const Value: Integer);
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure RequestAlign; override;
    procedure StopSizing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property VSPopupPanel: TCnVSPopupPanel read FVSPopupPanel write SetVSPopupPanel;
    property SplitWidth: Integer read FSplitWidth write SetSplitWidth;
  published
    property Align default alLeft;
    property VSChannelAlign: TAlign read GetVSChannelAlign;
    property AutoSnap: Boolean read FAutoSnap write FAutoSnap default True;
    property Beveled: Boolean read FBeveled write SetBeveled default False;
    property Color;
    property Constraints;
    property MinSize: NaturalNumber read FMinSize write FMinSize default 30;
    property ParentColor;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle
      default rsPattern;
    property Visible;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

// 隐藏所有的PopupPanel,但是不包括ExcludeChannel中的PopupPanel
procedure HideAllPopupPanel(ExcludeChannel: TCnVSChannel);

var
  { 默认的沟是nil }
  DefaultVSChannelClass: TCnVSChannelClass = nil;

implementation

uses SysUtils, CnDockSupportProc, CnDockGlobal, Dialogs, AppEvnts;

type
  TAnimateState = (asPopup, asHide);

  TPopupPanelAnimate = class(TTimer)
  private
    FMaxWidth: Integer;           // 最大的宽度
    FCurrentWidth: Integer;       // 当前的宽度
    FVSChannel: TCnVSChannel;     // 当前处理的TCnVSChannel
    FState: TAnimateState;
  protected
    procedure Timer; override;
    procedure OnCustomTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PopupForm(VSChannel: TCnVSChannel; MaxWidth: Integer); virtual;
    procedure HideForm(VSChannel: TCnVSChannel; MaxWidth: Integer); virtual;
  end;

  TCnAppEvents = class(TApplicationEvents)
  private
    FOldOnMessage: TMessageEvent;
    procedure NewOnMessage(var Msg: TMsg; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  PopupPanelAnimate: TPopupPanelAnimate;
  ApplicationEvents: TCnAppEvents;

const
  { 弹出动画定时的间隔 }
  PopupPanelAnimateInterval: Integer = 10;
  { 弹出动画每次移动多少宽度 }
  PopupPanelAnimateMoveWidth: Integer = 30;
  { 弹出动画开始前的暂停时间 }
  //AnimateSleepTime: Integer = 500;

// 隐藏所有的PopupPanel,但是不包括ExcludeChannel中的PopupPanel
procedure HideAllPopupPanel(ExcludeChannel: TCnVSChannel);
var i, j: Integer;
  Channel: TCnVSChannel;
  DockServer: TCnDockServer;
begin
//  if ExcludeChannel = nil then Exit;
  for i := 0 to CnGlobalDockPresident.DockServersList.Count - 1 do
  begin
    DockServer := FindDockServer(CnGlobalDockPresident.DockServersList[i]);
    if (DockServer <> nil) and (DockServer.DockPanel[0] is TCnVSNETDockPanel) then
    for j := 0 to 3 do
    begin
      Channel := TCnVSNETDockPanel(DockServer.DockPanel[j]).VSChannel;
      if (Channel <> nil) and (Channel <> ExcludeChannel) then
        Channel.HidePopupPanel(Channel.FActivePane);
    end;
  end;
end;

// 重新设置Channel中的Block的开始位置
procedure ResetChannelBlockStartOffset(Channel: TCnVSChannel);
var i: Integer;
  LeftChannel: TCnVSChannel;
  CurrChannel: TCnVSChannel;
  OldOffset: Integer;
  LeftAlignArea: Integer;
begin
  LeftChannel := TCnVSNETDockPanel(Channel.DockServer.LeftDockPanel).VSChannel;
  if (LeftChannel <> nil) then
  begin
    LeftAlignArea := GetClientAlignControlArea(LeftChannel.Parent, alLeft);
    for i := 0 to 3 do
    begin
      CurrChannel := TCnVSNETDockPanel(Channel.DockServer.DockPanel[i]).VSChannel;
      if (CurrChannel.Align in [alTop, alBottom]) then
      begin
        OldOffset := CurrChannel.BlockStartOffset;
        CurrChannel.BlockStartOffset := 2 + LeftAlignArea;
        if OldOffset <> CurrChannel.BlockStartOffset then
          CurrChannel.Invalidate;
      end;
    end;
  end;
end;

{ TCnVSNETDockStyle }

procedure TCnVSNETDockStyle.AddDockBaseControl(
  ADockBaseControl: TCnDockBaseControl);
begin
  if ADockBaseControl = nil then Exit;
  if DockBaseControlList.IndexOf(ADockBaseControl) = -1 then
  begin
    inherited;
    ChannelOption.ResetDockControlOption;
  end;
end;

constructor TCnVSNETDockStyle.Create(AOwner: TComponent);
begin
  inherited;
  CnDockPanelClass := TCnVSNETDockPanel;
  CnDockSplitterClass := TCnVSNETDockSplitter;
  CnConjoinPanelClass := TCnVSNETConjoinPanel;
  CnTabDockClass := TCnVSNETTabPageControl;
  CnDockPanelTreeClass := TCnVSNETDockTree;
  CnDockPanelZoneClass := TCnVSNETDockZone;
  CnConjoinPanelTreeClass := TCnVSNETDockTree;
  CnConjoinPanelZoneClass := TCnVSNETDockZone;
  CnConjoinServerOptionClass := TCnVSNETConjoinServerOption;
  CnTabServerOptionClass := TCnVSNETTabServerOption;
  FCnChannelOptionClass := TCnVSNETChannelOption;
//  CnVSChannelClass := TCnVSChannel;
end;

procedure TCnVSNETDockStyle.CreateConjoinServerOption(
  var Option: TCnBasicConjoinServerOption);
begin
  Option := TCnVSNETConjoinServerOption.Create(Self);
end;

procedure TCnVSNETDockStyle.CreateServerOption;
begin
  inherited;
  if FCnChannelOptionClass <> nil then
    FCnChannelOption := FCnChannelOptionClass.Create(Self);
end;

procedure TCnVSNETDockStyle.CreateTabServerOption(
  var Option: TCnBasicTabServerOption);
begin
  Option := TCnVSNETTabServerOption.Create(Self);
end;

destructor TCnVSNETDockStyle.Destroy;
begin
  inherited;

end;

function TCnVSNETDockStyle.DockClientWindowProc(DockClient: TCnDockClient;
  var Message: TMessage): Boolean;
var Channel: TCnVSChannel;
begin
  Result := inherited DockClientWindowProc(DockClient, Message);
  if (Message.Msg = CM_ENTER) or (Message.Msg = CM_EXIT){ or ((Message.Msg = WM_ACTIVATE){ and (Message.ResultLo = WA_INACTIVE))} then
  begin
    Channel := nil;
    if (DockClient.ParentForm.HostDockSite is TCnVSPopupPanel) then
      Channel := TCnVSPopupPanel(DockClient.ParentForm.HostDockSite).VSChannel
    else if DockClient.ParentForm.HostDockSite <> nil then
    begin
      if (DockClient.ParentForm.HostDockSite.Parent is TCnVSPopupPanel) then
        Channel := TCnVSPopupPanel(DockClient.ParentForm.HostDockSite.Parent).VSChannel
      else if (DockClient.ParentForm.HostDockSite.Parent <> nil)
        and (DockClient.ParentForm.HostDockSite.Parent.Parent is TCnVSPopupPanel) then
        Channel := TCnVSPopupPanel(DockClient.ParentForm.HostDockSite.Parent.Parent).VSChannel;
    end;
    if (Message.Msg = CM_EXIT){ or (Message.Msg = WM_ACTIVATE)} then
    begin
      if Channel <> nil then
        Channel.HidePopupPanelWithAnimate(Channel.FActivePane);
    end else if (Message.Msg = CM_ENTER) then
    begin
      HideAllPopupPanel(Channel);
    end;
  end;
end;

function TCnVSNETDockStyle.DockServerWindowProc(DockServer: TCnDockServer;
  var Message: TMessage): Boolean;
var i: Integer;
  Channel: TCnVSChannel;
begin
  Result := inherited DockServerWindowProc(DockServer, Message);
  if (Message.Msg = WM_SIZE){ or (Message.Msg = CM_EXIT) }then
  begin
    for i := 0 to 3 do
    begin
      Channel := nil;
      if DockServer.DockPanel[i] <> nil then
        Channel := TCnVSNETDockPanel(DockServer.DockPanel[i]).VSChannel;
      if Channel <> nil then
        Channel.HidePopupPanel(Channel.FActivePane);
    end;
  end;
end;

procedure TCnVSNETDockStyle.FreeServerOption;
begin
  inherited;
  if FCnChannelOption <> nil then
    FCnChannelOption.Free;
end;

function TCnVSNETDockStyle.GetChannelOption: TCnVSNETChannelOption;
begin
  Result := FCnChannelOption;
end;

procedure TCnVSNETDockStyle.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnVSNETDockStyleName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnVSNETDockStyleComment;
end;

function TCnVSNETDockStyle.GetControlName: string;
begin
  Result := Format(gs_LikeVSNETStyle, [gs_CnDockStyleName]);
end;

function TCnVSNETDockStyle.GetDockFormVisible(
  ADockClient: TCnDockClient): Boolean;
var VSChannel: TCnVSChannel;
  Pane: TCnVSPane;
begin
  Result := True;
  if ADockClient <> nil then
  begin
    if not (ADockClient.ParentForm is TCnTabDockHostForm) and
      (ADockClient.ParentForm.HostDockSite is TCnVSPopupPanel) then
    begin
      // 是TCnVSChannel的平铺方式
      VSChannel := TCnVSPopupPanel(ADockClient.ParentForm.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else Pane := nil;
      if Pane <> nil then
        Result := Pane.Visible;
    end
    else if (ADockClient.ParentForm.HostDockSite <> nil) and (ADockClient.ParentForm.HostDockSite.Parent <> nil) and
      (ADockClient.ParentForm.HostDockSite.Parent.HostDockSite is TCnVSPopupPanel) then
    begin
      // 是TCnVSChannel的分页方式
      VSChannel := TCnVSPopupPanel(ADockClient.ParentForm.HostDockSite.Parent.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else Pane := nil;
      if Pane <> nil then
        Result := Pane.Visible;
    end else Result := inherited GetDockFormVisible(ADockClient);
  end;
end;

procedure TCnVSNETDockStyle.HideDockForm(ADockClient: TCnDockClient);
begin
  inherited;
  SetDockFormVisible(ADockClient, False);
end;

procedure TCnVSNETDockStyle.RestoreClient(DockClient: TCnDockClient);
begin
  { 如果当前的服务器是TCnCSPopupPanel,就不调用父类的ResetDockClient函数 }
  if (DockClient.ParentForm.HostDockSite is TCnVSPopupPanel) or
    ((DockClient.ParentForm.Parent <> nil) and (DockClient.ParentForm.Parent.HostDockSite is TCnVSPopupPanel)) then
      Exit;
  inherited;
end;

procedure TCnVSNETDockStyle.SetChannelOption(
  const Value: TCnVSNETChannelOption);
begin
  FCnChannelOption.Assign(Value);
end;

procedure TCnVSNETDockStyle.SetDockFormVisible(ADockClient: TCnDockClient;
  AVisible: Boolean);
var VSChannel: TCnVSChannel;
  Pane: TCnVSPane;
  { 重新设置激活的客户 }
  procedure ResetActiveControl;
  var i: Integer;
  begin
    if AVisible then
      // 如果是显示,当前的客户窗体就是激活的客户
      Pane.Block.ActiveDockControl := ADockClient.ParentForm
    else
    begin
      // 首先从当前索引向上查找到第一个,如果找到一个属性Visible为True的Pane,
      // 就把当前激活的客户设置为这个Pane的DockForm;
      for i := Pane.Index downto 0 do
      begin
        if Pane.Block.VSPanes[i].Visible then
        begin
          Pane.Block.ActiveDockControl := Pane.Block.VSPanes[i].DockForm;
          Exit;
        end;
      end;
      // 同上,只是搜索的方向不同.
      for i := Pane.Index + 1 to Pane.Block.VSPaneCount - 1 do
      begin
        if Pane.Block.VSPanes[i].Visible then
        begin
          Pane.Block.ActiveDockControl := Pane.Block.VSPanes[i].DockForm;
          Exit;
        end;
      end;
    end;
  end;
begin
  if (ADockClient <> nil) then
  begin
    VSChannel := nil;
    if not (ADockClient.ParentForm is TCnTabDockHostForm) and
      (ADockClient.ParentForm.HostDockSite is TCnVSPopupPanel) then
    begin
      // 平铺方式
      VSChannel := TCnVSPopupPanel(ADockClient.ParentForm.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else Pane := nil;
      Pane := VSChannel.FindPane(ADockClient.ParentForm);
      if Pane <> nil then
      begin
        Pane.Visible := AVisible;
        ResetActiveControl;
      end;
    end else if (ADockClient.ParentForm.HostDockSite <> nil) and (ADockClient.ParentForm.HostDockSite.Parent <> nil) and
      (ADockClient.ParentForm.HostDockSite.Parent.HostDockSite is TCnVSPopupPanel) then
    begin
      // 分页方式
      VSChannel := TCnVSPopupPanel(ADockClient.ParentForm.HostDockSite.Parent.HostDockSite).VSChannel;
      if VSChannel <> nil then
        Pane := VSChannel.FindPane(ADockClient.ParentForm)
      else Pane := nil;
      Pane := VSChannel.FindPane(ADockClient.ParentForm);
      if Pane <> nil then
      begin
        Pane.Visible := AVisible;
        ResetActiveControl;
        TCnVSNETDockTabSheet(ADockClient.ParentForm.Parent).OldVisible := AVisible;
      end;
    end;
    if VSChannel <> nil then
    begin
      VSChannel.ResetPosition;
      VSChannel.Invalidate;
    end;
  end;
end;

procedure TCnVSNETDockStyle.ShowDockForm(ADockClient: TCnDockClient);
begin
  inherited;
  SetDockFormVisible(ADockClient, True);
end;

{ TCnVSNETDockTree }

procedure TCnVSNETDockTree.BeginDrag(Control: TControl; Immediate: Boolean;
  Threshold: Integer);
begin
  // 如果是弹出Panel就不处理
  if not (DockSite is TCnVSPopupPanel) then
    inherited;
end;

constructor TCnVSNETDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
begin
  inherited;
  GrabberSize     := DefaultVSNETGrabberSize;
  ButtonHeight    := 12;
  ButtonWidth     := 16;
  LeftOffset      := 2;
  RightOffset     := 3;
  TopOffset       := 4;
  BottomOffset    := 3;
  ButtonSplitter  := 2;
  CaptionLeftOffset := 5;
  CaptionRightOffset := 5;
end;

procedure TCnVSNETDockTree.CustomLoadZone(Stream: TStream;
  var Zone: TCnDockZone);
var Pane: TCnVSPane;
  i: Integer;

  procedure SetPaneVisible(ChildControl: TControl; VSPaneVisible: Boolean);
  var DockClient: TCnDockClient;
  begin
    if (Pane <> nil) then
    begin
      Pane.Visible := VSPaneVisible;
      DockClient := FindDockClient(Pane.DockForm);
      if DockClient <> nil then
      begin
        if Pane.Visible then
        begin
          DockClient.ParentVisible := False;
          DockClient.ParentForm.Visible := True;
          DockClient.MakeShowEvent;
        end
        else
          DockClient.MakeHideEvent;
      end;
    end;
  end;

var Sheet: TCnVSNETDockTabSheet;

begin
  inherited CustomLoadZone(Stream, Zone);
  Stream.Read(TCnVSNETDockZone(Zone).FVSPaneVisible, SizeOf(TCnVSNETDockZone(Zone).VSPaneVisible));
  if DockSite is TCnVSPopupPanel then
  begin
    With TCnVSPopupPanel(DockSite).VSChannel, TCnVSNETDockZone(Zone) do
    begin
      if ChildControl is TCnTabDockHostForm then
      begin
        for i := 0 to TCnTabDockHostForm(ChildControl).PageControl.PageCount - 1 do
        begin
          Sheet := TCnVSNETDockTabSheet(TCnTabDockHostForm(ChildControl).PageControl.Pages[i]);
          Pane := FindPane(TWinControl(Sheet.Controls[0]));
          SetPaneVisible(ChildControl, Sheet.OldVisible);
        end;
      end
      else
      begin
        Pane := FindPane(ChildControl);
        SetPaneVisible(ChildControl, VSPaneVisible);
      end;
      ResetPosition;
    end;
  end;
end;

procedure TCnVSNETDockTree.CustomSaveZone(Stream: TStream;
  Zone: TCnDockZone);
var Pane: TCnVSPane;
begin
  inherited CustomSaveZone(Stream, Zone);
  if DockSite is TCnVSPopupPanel then
  begin
    With TCnVSPopupPanel(DockSite).VSChannel, TCnVSNETDockZone(Zone) do
    begin
      Pane := FindPane(ChildControl);
      if (Pane <> nil) then
        VSPaneVisible := Pane.Visible;
    end;
  end;
  Stream.Write(TCnVSNETDockZone(Zone).VSPaneVisible, SizeOf(TCnVSNETDockZone(Zone).VSPaneVisible));
end;

destructor TCnVSNETDockTree.Destroy;
begin
  inherited;

end;

function TCnVSNETDockTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): Boolean;
begin
  Result := inherited DoLButtonDown(Message, Zone, HTFlag);
  if (Zone <> nil) then
  begin
    if (HTFlag = HTCLOSE) then
      TCnVSNETDockZone(Zone).CloseBtnState := bsDown
    else if HTFlag = HTAUTOHIDE then
    begin
      AutoHideZone := TCnVSNETDockZone(Zone);
      AutoHideZone.AutoHideBtnDown := True;
      AutoHideZone.AutoHideBtnState := bsDown;
    end;
  end;
end;

procedure TCnVSNETDockTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
begin
  if CloseBtnZone <> nil then
  begin
    TCnVSNETDockZone(CloseBtnZone).CloseBtnState := bsNormal;
  end;                 
  inherited;
  if (AutoHideZone <> nil) then
  begin
    AutoHideZone.AutoHideBtnDown := False;
    AutoHideZone.AutoHideBtnState := bsNormal;
    if HTFlag = HTAUTOHIDE then
    begin
      if DockSite is TCnVSNETDockPanel then
        TCnVSNETDockPanel(DockSite).DoAutoHideControl(AutoHideZone.ChildControl);
    end;
    AutoHideZone := nil;
  end;
end;

procedure TCnVSNETDockTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var
  AZone: TCnVSNETDockZone;
begin
  inherited;
  if Zone <> nil then
  begin
    AZone := TCnVSNETDockZone(Zone);
    if AZone.AutoHideBtnDown then
    begin
      if HTFlag = HTAUTOHIDE then
        AZone.AutoHideBtnState := bsDown
      else
        AZone.AutoHideBtnState := bsUp;
    end else if (HTFlag = HTAUTOHIDE) and not AZone.CloseBtnDown then
      AZone.AutoHideBtnState := bsUp
    else
      AZone.AutoHideBtnState := bsNormal;

    if AZone.CloseBtnDown then
    begin
      if HTFlag = HTCLOSE then
        AZone.CloseBtnState := bsDown
      else AZone.CloseBtnState := bsUp;
    end else if (HTFlag = HTCLOSE) and not AZone.AutoHideBtnDown then
      AZone.CloseBtnState := bsUp
    else
      AZone.CloseBtnState := bsNormal;
  end;
end;

procedure TCnVSNETDockTree.DoOtherHint(Zone: TCnDockZone; HTFlag: Integer;
  var HintStr: string);
begin
  inherited;
  if (HTFlag = HTAUTOHIDE) then
    HintStr := gs_CnVSNETDockTreeAutoHideBtnHint;
end;

procedure TCnVSNETDockTree.DrawAutoHideButton(
  Zone: TCnDockZone; Left, Top: Integer);
var
  AZone: TCnVSNETDockZone;
  ColorArr: array[1..2] of TColor;
begin
  if Zone <> nil then
  begin
    AZone := TCnVSNETDockZone(Zone);
    { 画自动隐藏按钮的边框 }
    if AZone.AutoHideBtnState <> bsNormal then
    begin
      if AZone.AutoHideBtnState = bsUp then
      begin
        ColorArr[1] := clBlack;
        if GetActiveControl = AZone.ChildControl then
          ColorArr[2] := clBtnface
        else ColorArr[2] := clWhite;
      end else if AZone.AutoHideBtnState = bsDown then
      begin
        ColorArr[1] := clBtnface;
        ColorArr[2] := clBlack;
      end;
      Canvas.Pen.Color := ColorArr[1];
      Canvas.MoveTo(Left, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top);
      Canvas.Pen.Color := ColorArr[2];
      Canvas.LineTo(Left, Top);
      Canvas.LineTo(Left, Top + ButtonHeight);
    end;
    { 如果自动隐藏按钮是按下的，图钉的位置就要向右下角移动一个像素的单位 }
    if AZone.AutoHideBtnState = bsDown then
    begin
      Inc(Left);
      Inc(Top);
    end;
    { 画自动隐藏按钮的图钉 }
    if AZone.ChildControl = GetActiveControl then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;
    if DockSite.Align in [alLeft, alRight, alTop, alBottom] then
    begin
      Canvas.MoveTo(Left + 9, Top + 10);
      Canvas.LineTo(Left + 9, Top + 7);
      Canvas.MoveTo(Left + 6, Top + 7);
      Canvas.LineTo(Left + 13, Top + 7);
      Canvas.MoveTo(Left + 7, Top + 6);
      Canvas.LineTo(Left + 7, Top + 2);
      Canvas.LineTo(Left + 10, Top + 2);
      Canvas.LineTo(Left + 10, Top + 6);
      Canvas.LineTo(Left + 11, Top + 6);
      Canvas.LineTo(Left + 11, Top + 1);
    end else if DockSite.Align in [alNone] then
    begin
      Canvas.MoveTo(Left + 5, Top + 6);
      Canvas.LineTo(Left + 8, Top + 6);
      Canvas.MoveTo(Left + 8, Top + 3);
      Canvas.LineTo(Left + 8, Top + 10);
      Canvas.MoveTo(Left + 9, Top + 4);
      Canvas.LineTo(Left + 12, Top + 4);
      Canvas.LineTo(Left + 12, Top + 7);
      Canvas.LineTo(Left + 9, Top + 7);
      Canvas.LineTo(Left + 9, Top + 8);
      Canvas.LineTo(Left + 13, Top + 8);
    end;
  end;
end;

procedure TCnVSNETDockTree.DrawCloseButton(Canvas: TCanvas;
  Zone: TCnDockZone; Left, Top: Integer);
var DrawRect: TRect;
  AZone: TCnVSNETDockZone;
  ColorArr: array[1..2] of TColor;
  ADockClient: TCnDockClient;
  AForm: TForm;
begin
  if Zone <> nil then
  begin
    { 如果EnableCloseBtn属性为False,就不画关闭按钮 }
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then Exit;
    if Zone.ChildControl is TCnTabDockHostForm then
    begin
      AForm := TCnTabDockHostForm(Zone.ChildControl).GetActiveDockForm;
      if AForm <> nil then
      begin
        ADockClient := FindDockClient(AForm);
        if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then
          Exit;
      end;
    end;
    AZone := TCnVSNETDockZone(Zone);
    { 得到所要画图的区域的大小 }
    DrawRect.Left := Left + 6;
    DrawRect.Right := DrawRect.Left + 7;
    DrawRect.Top := Top + 3;
    DrawRect.Bottom := DrawRect.Top + 7;
    { 画关闭按钮的边框 }
    if AZone.CloseBtnState <> bsNormal then
    begin
      if AZone.CloseBtnState = bsUp then
      begin
        ColorArr[1] := clBlack;
        if GetActiveControl = AZone.ChildControl then
          ColorArr[2] := clBtnface
        else ColorArr[2] := clWhite;
      end else if AZone.CloseBtnState = bsDown then
      begin
        ColorArr[1] := clBtnface;
        ColorArr[2] := clBlack;
      end;
      Canvas.Pen.Color := ColorArr[1];
      Canvas.MoveTo(Left, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top + ButtonHeight);
      Canvas.LineTo(Left + ButtonWidth, Top);
      Canvas.Pen.Color := ColorArr[2];
      Canvas.LineTo(Left, Top);
      Canvas.LineTo(Left, Top + ButtonHeight);
    end;
    { 如果关闭按钮是按下的，X的位置就要向右下角移动一个像素的单位 }
    if AZone.CloseBtnState = bsDown then
      OffsetRect(DrawRect, 1, 1);
    { 画关闭按钮的X }
    if AZone.ChildControl = GetActiveControl then
      Canvas.Pen.Color := clWhite
    else
      Canvas.Pen.Color := clBlack;
    Canvas.MoveTo(DrawRect.Left, DrawRect.Top);
    Canvas.LineTo(DrawRect.Right, DrawRect.Bottom);
    Canvas.MoveTo(DrawRect.Right-1, DrawRect.Top);
    Canvas.LineTo(DrawRect.Left-1, DrawRect.Bottom);
  end;
end;

procedure TCnVSNETDockTree.GetCaptionRect(var Rect: TRect);
begin
  if DockSite.Align = alClient then
    inherited
  else
  begin
    Inc(Rect.Left, 2 + CaptionLeftOffset);
    Inc(Rect.Top, 3);
    Dec(Rect.Right, 2*ButtonWidth + ButtonSplitter + CaptionRightOffset - 1);
    Dec(Rect.Bottom, 2);
  end;
end;

function TCnVSNETDockTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  Result := inherited GetTopGrabbersHTFlag(MousePos, HTFlag, Zone);
  if (Zone <> nil) and (DockSite.Align <> alClient) and (HTFlag <> HTCLOSE) then
  begin
    with Zone.ChildControl do
      if PtInRect(Rect(
        Left + Width - 2*ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset,
        Left + Width - ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTAUTOHIDE;
  end;
end;

procedure TCnVSNETDockTree.DrawDockGrabber(
  Control: TControl; const ARect: TRect);
begin
  inherited;
  if DockSite.Align <> alClient then
    DrawAutoHideButton(FindControlZone(Control), ARect.Right-RightOffset-2*ButtonWidth-ButtonSplitter, ARect.Top+TopOffset);
end;

procedure TCnVSNETDockTree.PaintDockGrabberRect(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
var DrawRect: TRect;
begin
  inherited;
  if GetActiveControl <> Control then
  begin
    Canvas.Pen.Color := clGray;
    DrawRect := ARect;
    Inc(DrawRect.Left);
    Canvas.RoundRect(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom, 2, 2);
  end;
end;

procedure TCnVSNETDockTree.DoLButtonDbClk(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
begin
  if DockSite is TCnVSPopupPanel then Exit;
  inherited;
end;

procedure TCnVSNETDockTree.DoHideZoneChild(AZone: TCnDockZone);
var AForm: TForm;
  ADockClient: TCnDockClient;
begin
  { 根据AZone的ChildControl中的值来判断是否关闭当前的客户窗体 }
  if (AZone <> nil) and (AZone.ChildControl <> nil) then
  begin
    if AZone.ChildControl is TCnTabDockHostForm then
    begin
      AForm := TCnTabDockHostForm(AZone.ChildControl).GetActiveDockForm;
      if AForm <> nil then
      begin
        ADockClient := FindDockClient(AForm);
        if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then
          Exit else
          AForm.Close;
      end;
    end else inherited;
  end;
end;

procedure TCnVSNETDockTree.IgnoreZoneInfor(Stream: TMemoryStream);
begin
  inherited;
  Stream.Position := Stream.Position + 1;
end;

{ TCnVSNETConjoinServerOption }

constructor TCnVSNETConjoinServerOption.Create(
  ADockStyle: TCnBasicDockStyle);
begin
  inherited;
  SystemInfo := True;
end;

destructor TCnVSNETConjoinServerOption.Destroy;
begin
  inherited;

end;

procedure TCnVSNETConjoinServerOption.SetDefaultSystemCaptionInfo;
begin
  inherited;
  { 默认的获得焦点时的字体颜色是白色 }
  ActiveFont.Color := clWhite;
  ActiveFont.Style := [];
  { 默认的获得焦点时的字体颜色是黑色 }
  InactiveFont.Color := clBlack;
  InactiveFont.Style := [];

  SetActiveTitleEndColor_WithoutChangeSystemInfo(ActiveTitleStartColor);
  SetInactiveTitleStartColor_WithoutChangeSystemInfo(clBtnFace);
  SetInactiveTitleEndColor_WithoutChangeSystemInfo(clBtnFace);

//  SetGrabbersSize_WithoutChangeSystemInfo(19);
end;

{ TCnVSNETTabServerOption }

constructor TCnVSNETTabServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  inherited;
  InactiveFont.Color := VSNETPageInactiveFontColor;
  InactiveSheetColor := VSNETPageInactiveSheetColor;
  ShowTabImages := True;
end;

{ TCnVSNETDockZone }

constructor TCnVSNETDockZone.Create(Tree: TCnDockTree);
begin
  inherited;
  FAutoHideBtnState := bsNormal;
  FCloseBtnState := bsNormal;
  FVSPaneVisible := True;
end;

procedure TCnVSNETDockZone.DoCustomSetControlName;
var i: Integer;
  Pane: TCnVSPane;
  DockClient: TCnDockClient;
begin
  inherited;
  if Tree.DockSite is TCnVSPopupPanel then
  begin
    With TCnVSPopupPanel(Tree.DockSite).VSChannel do
    begin
      AddDockControl(ChildControl);
      if ChildControl is TCnTabDockHostForm then
      begin
        With TCnTabDockHostForm(ChildControl).PageControl do
        begin
          for i := 0 to DockClientCount - 1 do
          begin
            Pane := FindPane(TWinControl(DockClients[i]));
            DockClient := FindDockClient(DockClients[i]);
            if (Pane <> nil) and (DockClient <> nil) then
            begin
              Pane.Width := DockClient.VSPaneWidth;
            end;
          end;
        end;
      end else
      begin
        Pane := FindPane(ChildControl);
        DockClient := FindDockClient(ChildControl);
        if (Pane <> nil) and (DockClient <> nil) then
        begin
          Pane.Width := DockClient.VSPaneWidth;
        end;
      end;
    end;
  end;
end;

procedure TCnVSNETDockZone.SetAutoHideBtnDown(const Value: Boolean);
begin
  FAutoHideBtnDown := Value;
end;

procedure TCnVSNETDockZone.SetAutoHideBtnState(const Value: TBtnState);
begin
  if FAutoHideBtnState <> Value then
  begin
    FAutoHideBtnState := Value;
    Tree.DockSite.Invalidate;
  end;
end;

procedure TCnVSNETDockZone.SetChildControlVisible(Client: TControl;
  AViisible: Boolean);
//var VSChannel: TCnVSChannel;
//  Pane: TCnVSPane;
begin
  inherited;
{  if Tree.DockSite is TCnVSPopupPanel then
  begin
    VSChannel := TCnVSPopupPanel(Tree.DockSite).VSChannel;
    Pane := VSChannel.FindPane(TWinControl(Client));
    if Pane <> nil then
      Pane.Visible := AViisible;
  end;}
end;

procedure TCnVSNETDockZone.SetCloseBtnState(const Value: TBtnState);
begin
  if FCloseBtnState <> Value then
  begin
    FCloseBtnState := Value;
    Tree.DockSite.Invalidate;
  end;
end;

procedure TCnVSNETDockZone.SetVSPaneVisible(const Value: Boolean);
begin
  FVSPaneVisible := Value;
end;

{ TCnVSNETTabPanel }

constructor TCnVSNETTabPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabHeight := 25;
  CaptionTopOffset := 1;
end;

{ TCnVSNETTabPageControl }

constructor TCnVSNETTabPageControl.Create(AOwner: TComponent);
begin
  inherited;
  CnDockTabSheetClass := TCnVSNETDockTabSheet;
  CnTabPanelClass := TCnVSNETTabPanel;
end;

procedure TCnVSNETTabPageControl.CreatePanel;
begin
  inherited;
end;

procedure TCnVSNETTabPageControl.ShowControl(AControl: TControl);
begin
  inherited;
end;

{ TCnVSChannel }

procedure TCnVSChannel.AddDockControl(Control: TWinControl);
var Block: TCnVSBlock;
begin
  if Control is TCnTabDockHostForm then
  begin
    Block := TCnVSBlock.Create(Self);
    Block.AddDockControl(Control);
    FBlockList.Add(Block);
  end else
  begin
    if (BlockCount >= 1) and (Blocks[0].BlockType = btConjoinBlock) then
    begin
      Blocks[0].AddDockControl(Control);
    end else
    begin
      Block := TCnVSBlock.Create(Self);
      Block.AddDockControl(Control);
      FBlockList.Insert(0, Block);
    end;
  end;
  HideAllPopupPanel(Self);
  ResetPosition;
  Invalidate;
end;

constructor TCnVSChannel.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TCnVSNETDockPanel then
  begin
    FVSNETDockPanel := TCnVSNETDockPanel(AOwner);
    DockServer := FVSNETDockPanel.DockServer;
  end;
  FBlockList := TList.Create;
  FChannelWidth := 22;
  FBlockStartOffset := 2;
  FBlockUpOffset := 2;
  FBlockInterval := 13;
  Color := VSNETPageInactiveSheetColor;
  ParentFont := True;
  ActivePaneSize := MaxActivePaneWidth;
end;

procedure TCnVSChannel.CreateVSPopupPanel;
begin
  FVSPopupPanel := TCnVSPopupPanel.Create(FVSNETDockPanel);
  FVSPopupPanel.Name := FVSNETDockPanel.Name + '_PopupPanel';
  FVSPopupPanel.Visible := False;
  if Parent is TForm then
  begin
    FVSPopupPanel.Parent := Parent;
    FVSPopupPanel.Align := alNone;
    FVSPopupPanel.BringToFront;
  end;
  FVSPopupPanelSplitter := TCnVSPopupPanelSplitter.Create(Parent);
  if Parent is TForm then
  begin
    FVSPopupPanelSplitter.Parent := Parent;
    FVSPopupPanelSplitter.Align := alNone;
    FVSPopupPanelSplitter.VSPopupPanel := VSPopupPanel;
    FVSPopupPanelSplitter.Color := clBtnface;
    FVSPopupPanelSplitter.Visible := False;
  end;
end;

procedure TCnVSChannel.DeleteBlock(Index: Integer);
begin
  Blocks[Index].Free;
  FBlockList.Delete(Index);
end;

destructor TCnVSChannel.Destroy;
begin
  FreeBlockList;
  inherited;
end;

procedure TCnVSChannel.DestroyVSPopupPanel;
begin

end;

function TCnVSChannel.FindDockControl(Control: TWinControl; var BlockIndex: Integer;
      var PaneIndex: Integer): Boolean;
var i, j: Integer;
begin
  Result := False;
  BlockIndex := -1;
  PaneIndex := -1;
  if Control = nil then Exit;
  for i := 0 to BlockCount - 1 do
  begin
    for j := 0 to Blocks[i].VSPaneCount - 1 do
      if Blocks[i].VSPanes[j].DockForm = Control then
      begin
        BlockIndex := i;
        PaneIndex := j;
        Result := True;
        Exit;
      end;
    if Blocks[i].FBlockType = btTabBlock then
    begin
      j := 0;
      if Blocks[i].VSPanes[0].DockForm.HostDockSite.Parent = Control then
      begin
        BlockIndex := i;
        PaneIndex := j;
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TCnVSChannel.GetBlockCount: Integer;
begin
  Result := FBlockList.Count;
end;

procedure TCnVSChannel.GetBlockRect(Block: TCnVSBlock; Index: Integer;
  var ARect: TRect);
var BlockWidth: Integer;
begin
  if Block.VSPanes[Index].DockForm <> Block.FActiveDockControl then
    BlockWidth := Block.InactiveBlockWidth
  else
    BlockWidth := Block.ActiveBlockWidth;
  { 首先得到画图的区域 }
  case Align of
    alLeft:
    begin
      ARect.Left := -1;
      ARect.Top := FCurrentPos;
      ARect.Right := Width - FBlockUpOffset;
      ARect.Bottom := ARect.Top + BlockWidth;
    end;
    alRight:
    begin
      ARect.Left := FBlockUpOffset;
      ARect.Top := FCurrentPos;
      ARect.Right := Width + 1;
      ARect.Bottom := ARect.Top + BlockWidth;
    end;
    alTop:
    begin
      ARect.Left := FCurrentPos;
      ARect.Top := -1;
      ARect.Right := ARect.Left + BlockWidth;
      ARect.Bottom := Height - FBlockUpOffset;
    end;
    alBottom:
    begin
      ARect.Left := FCurrentPos;
      ARect.Top := FBlockUpOffset;
      ARect.Right := ARect.Left + BlockWidth;
      ARect.Bottom := Height + 1;
    end;
  end;
  { 移动位置 }
  Inc(FCurrentPos, BlockWidth - 1);
end;

function TCnVSChannel.GetBlocks(Index: Integer): TCnVSBlock;
begin
  Result := TCnVSBlock(FBlockList[Index]);
end;

function TCnVSChannel.GetDockFormWithMousePos(MousePos: TPoint): TCnVSPane;
var i, j: Integer;
  ARect: TRect;
begin
  Result := nil;
  FCurrentPos := FBlockStartOffset;
  for i := 0 to BlockCount - 1 do
  begin
    for j := 0 to Blocks[i].VSPaneCount - 1 do
    begin
      if not Blocks[i].VSPanes[j].Visible then Continue;
      GetBlockRect(Blocks[i], j, ARect);
      if PtInRect(ARect, MousePos) then
      begin
        Result := Blocks[i].VSPanes[j];
        Exit;
      end;
    end;
    Inc(FCurrentPos, FBlockInterval);
  end;
end;

procedure TCnVSChannel.HidePopupPanel(Pane: TCnVSPane);
begin
  if Pane <> nil then
  begin
    if Align in [alLeft, alRight] then
    begin
      VSPopupPanel.Width := 0;
      VSPopupPanelSplitter.Width := 0;
    end
    else if Align in [alTop, alBottom] then
    begin
      VSPopupPanel.Height := 0;
      VSPopupPanelSplitter.Height := 0;
    end;
    FActiveDockForm := nil;
    FActivePane := nil;
  end;
  VSPopupPanel.Visible := False;
  VSPopupPanelSplitter.Visible := False;
  FActivePane := nil;
end;

procedure TCnVSChannel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var VSPane: TCnVSPane;
begin
  inherited;
  VSPane := GetDockFormWithMousePos(Point(X, Y));
  if VSPane <> nil then
  begin
    VSPane.Active := True;
    if VSPane.DockForm.CanFocus then
      VSPane.DockForm.SetFocus;
  end;
end;

procedure TCnVSChannel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  PopupDockForm(GetDockFormWithMousePos(Point(X, Y)));
end;

procedure TCnVSChannel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TCnVSChannel.Paint;

  procedure DrawSingleBlock(Block: TCnVSBlock);
  var DrawRect: TRect;
    i: Integer;

    { 调整需要重画的图象的位置 }
    procedure AdjustImagePos;
    begin
      if Align = alLeft then
      begin
        Inc(DrawRect.Left, 3);
        Inc(DrawRect.Top, 4);
      end else if Align = alTop then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 2);
      end else
      if Align = alRight then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 4);
      end else if Align = alBottom then
      begin
        Inc(DrawRect.Left, 4);
        Inc(DrawRect.Top, 3);
      end;
    end;

  var OldGraphicsMode: Integer;
      VisiblePaneCount: Integer;
  begin
    VisiblePaneCount := 0;
    for i := 0 to Block.VSPaneCount - 1 do
    begin
      if not Block.VSPanes[i].Visible then Continue;
      // 得到当前区块的大小
      GetBlockRect(Block, i, DrawRect);
      { 画一个矩形 }
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(DrawRect);
      Canvas.Brush.Color := clGray;
      Canvas.FrameRect(DrawRect);
      { 画图标 }
      AdjustImagePos;
      Block.FImageList.Draw(Canvas, DrawRect.Left, DrawRect.Top, i);
      { 画文字 }
      if Block.ActiveDockControl = Block.VSPanes[i].DockForm then
      begin
        if Align in [alTop, alBottom] then
          Inc(DrawRect.Left, Block.InactiveBlockWidth)
        else if Align in [alLeft, alRight] then
        begin
          Inc(DrawRect.Top, Block.InactiveBlockWidth);
          if Align = alLeft then
            DrawRect.Left := 15
          else DrawRect.Left := 20;
          DrawRect.Right := DrawRect.Left + (DrawRect.Bottom - DrawRect.Top);
        end;
        Canvas.Brush.Color := clBtnFace;
        Canvas.Pen.Color := clBlack;

        Dec(DrawRect.Right, 3);

        OldGraphicsMode := SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
        DrawText(Canvas.Handle, PChar(Block.VSPanes[i].DockForm.Caption), -1, DrawRect, DT_END_ELLIPSIS or DT_NOCLIP);
        SetGraphicsMode(Canvas.Handle, OldGraphicsMode);
      end;
      Inc(VisiblePaneCount);
    end;
    if VisiblePaneCount > 0 then
      Inc(FCurrentPos, FBlockInterval);
  end;

var i: Integer;
begin
  inherited;
  { 开始位置 }
  FCurrentPos := FBlockStartOffset;
  for i := 0 to BlockCount - 1 do
  begin
    DrawSingleBlock(Blocks[i]);
  end;
end;

procedure TCnVSChannel.PopupDockForm(Pane: TCnVSPane);
  procedure SetSingleDockFormVisible(HostDockSite: TWinControl; AForm: TForm);
  var i: Integer;
  begin
    for i := 0 to HostDockSite.DockClientCount - 1 do
      HostDockSite.DockClients[i].Visible := AForm = HostDockSite.DockClients[i];
  end;

begin
  if (Pane <> nil) and (ActiveDockForm <> Pane.DockForm) then
  begin
    HidePopupPanel(FActivePane);
    Pane.DockForm.Visible := True;
    PopupPanelAnimate.PopupForm(Self, Pane.Width);
    if (Pane.DockForm <> nil) and (Pane.DockForm.HostDockSite.Parent is TCnTabDockHostForm) then
    begin
      FVSPopupPanel.CnDockManager.ShowSingleControl(Pane.DockForm.HostDockSite.Parent);
      SetSingleDockFormVisible(Pane.DockForm.HostDockSite, Pane.DockForm);
      // 如果是分页方式，就需要改变Caption的值为选中的停靠窗体的Caption
      TCnTabDockHostForm(Pane.DockForm.HostDockSite.Parent).Caption := Pane.DockForm.Caption;
    end
    else
      FVSPopupPanel.CnDockManager.ShowSingleControl(Pane.DockForm);
    FActiveDockForm := Pane.DockForm;
    FActivePane := Pane;
    FVSPopupPanel.CnDockManager.ResetBounds(True);
    // 重新设置FActiveDockControl为当前选中的停靠窗体
    Pane.Block.FActiveDockControl := Pane.DockForm;
    Invalidate;
  end;
end;

procedure TCnVSChannel.RemoveDockControl(Control: TWinControl);
var BlockIndex, PaneIndex: Integer;
begin
  VSPopupPanel.Visible := False;
  if FindDockControl(Control, BlockIndex, PaneIndex) then
  begin
    Blocks[BlockIndex].DeletePane(PaneIndex);
    if (Blocks[BlockIndex].VSPaneCount <= 0) or (Blocks[BlockIndex].FBlockType = btTabBlock) then
      DeleteBlock(BlockIndex);
  end;
  ResetPosition;
  Invalidate;
end;

procedure TCnVSChannel.ResetBlock;
var i: Integer;
begin
  if BlockCount > 0 then
  begin
    Blocks[0].FBlockStartPos := FBlockStartOffset;
    for i := 1 to BlockCount - 1 do
      Blocks[i].FBlockStartPos := Blocks[i - 1].FBlockStartPos + Blocks[i - 1].GetTotalWidth + FBlockInterval;
  end;
end;

procedure TCnVSChannel.ResetPosition;
var i, j: Integer;
  PaneCount: Integer;
begin
  PaneCount := 0;
  for i := 0 to BlockCount - 1 do
    for j := 0 to Blocks[i].VSPaneCount - 1 do
      if Blocks[i].VSPanes[j].Visible then
        Inc(PaneCount);
  { 调整VSChannel的位置，使它总是在服务窗体的客户区的最内层 }
  Visible := PaneCount > 0;
  case Align of
    alLeft:
    begin
      Width := FChannelWidth;
      Left := GetClientAlignControlArea(Parent, Align, Self);
    end;
    alRight:
    begin
      Width := FChannelWidth;
      Left := Parent.ClientWidth - GetClientAlignControlArea(Parent, Align, Self) - FChannelWidth + 1;
    end;
    alTop:
    begin
      Height := FChannelWidth;
      Top := GetClientAlignControlArea(Parent, Align, Self);
    end;
    alBottom:
    begin
      Height := FChannelWidth;
      Top := Parent.ClientHeight - GetClientAlignControlArea(Parent, Align, Self) - FChannelWidth + 1;
    end;
  end;
end;

procedure TCnVSChannel.SetVSPopupPanelSplitterPosition;
begin
  case Align of
    alLeft:
    begin
      VSPopupPanelSplitter.Left := VSPopupPanel.Left + VSPopupPanel.Width;
      VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
      VSPopupPanelSplitter.Top := VSPopupPanel.Top;
      VSPopupPanelSplitter.Height := VSPopupPanel.Height;
    end;
    alRight:
    begin
      VSPopupPanelSplitter.Left := VSPopupPanel.Left - VSPopupPanelSplitter.SplitWidth;
      VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
      VSPopupPanelSplitter.Top := VSPopupPanel.Top;
      VSPopupPanelSplitter.Height := VSPopupPanel.Height;
    end;
    alTop:
    begin
      VSPopupPanelSplitter.Left := VSPopupPanel.Left;
      VSPopupPanelSplitter.Width := VSPopupPanel.Width;
      VSPopupPanelSplitter.Top := VSPopupPanel.Top + VSPopupPanel.Height;
      VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
    end;
    alBottom:
    begin
      VSPopupPanelSplitter.Left := VSPopupPanel.Left;
      VSPopupPanelSplitter.Width := VSPopupPanel.Width;
      VSPopupPanelSplitter.Top := VSPopupPanel.Top - VSPopupPanelSplitter.SplitWidth;
      VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
    end;
  end;
  VSPopupPanelSplitter.Visible := True;
  VSPopupPanelSplitter.BringToFront;
end;

procedure TCnVSChannel.SetVSPopupPanelSplitter(
  const Value: TCnVSPopupPanelSplitter);
begin
  FVSPopupPanelSplitter := Value;
end;

function TCnVSChannel.GetPaneWithControl(AControl: TControl): TCnVSPane;
var i, j: Integer;
begin
  Result := nil;
  for i := 0 to BlockCount - 1 do
    for j := 0 to Blocks[i].VSPaneCount - 1 do
      if AControl = Blocks[i].VSPanes[j].DockForm then
      begin
        Result := Blocks[i].VSPanes[j];
        Exit;
      end;
end;

procedure TCnVSChannel.SetBlockStartOffset(const Value: Integer);
begin
  FBlockStartOffset := Value;
end;

procedure TCnVSChannel.AnimatePopupPanel(
  AnimateStyle: TPopupPanelAnimateStyle);
begin
  if AnimateStyle = pasShow then
  begin

  end else if AnimateStyle = pasHide then
  begin

  end;
end;

procedure TCnVSChannel.ResetFontAngle;
var
  LogFont: TLogFont;
begin
  if Align in [alLeft, alRight] then
  begin
    if GetObject(Canvas.Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
    begin
      LogFont.lfEscapement := 2700;
      LogFont.lfOrientation := 2700;
      Canvas.Font.Handle := CreateFontIndirect(LogFont);
    end;
  end;
end;

procedure TCnVSChannel.RemoveAllBlock;
var i: Integer;
begin
  for i := BlockCount - 1 downto 0 do
    DeleteBlock(i);
  FActiveDockForm := nil;
end;

procedure TCnVSChannel.HidePopupPanel(Control: TWinControl);
var BlockIndex, PaneIndex: Integer;
begin
  FindDockControl(Control, BlockIndex, PaneIndex);
  if (BlockIndex >= 0) and (PaneIndex >= 0) then
    HidePopupPanel(Blocks[BlockIndex].VSPanes[PaneIndex]);
end;

procedure TCnVSChannel.PopupDockForm(Control: TWinControl);
var BlockIndex, PaneIndex: Integer;
begin
  FindDockControl(Control, BlockIndex, PaneIndex);
  if (BlockIndex >= 0) and (PaneIndex >= 0) then
    PopupDockForm(Blocks[BlockIndex].VSPanes[PaneIndex]);
end;

function TCnVSChannel.FindPane(Control: TWinControl): TCnVSPane;
var i, j: Integer;
begin
  Result := nil;
  if FindDockControl(Control, i, j) then
    Result := Blocks[i].VSPanes[j];
end;

procedure TCnVSChannel.HidePopupPanelWithAnimate(Pane: TCnVSPane);
begin
  if Pane <> nil then
    PopupPanelAnimate.HideForm(Self, Pane.Width);
end;

procedure TCnVSChannel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

procedure TCnVSChannel.ResetActivePaneWidth;
var DockClient: TCnDockClient;
begin
  if FActivePane = nil then Exit;
  DockClient := FindDockClient(FActivePane.DockForm);
  if Align in [alLeft, alRight] then
  begin
    FActivePane.Width := VSPopupPanel.Width;
  end
  else if Align in [alTop, alBottom] then
  begin
    FActivePane.Width := VSPopupPanel.Height + VSPopupPanel.CnDockManager.GrabberSize;
  end;
  if DockClient <> nil then
    DockClient.VSPaneWidth := FActivePane.Width;
end;

procedure TCnVSChannel.ResetPopupPanelHeight;
begin
  if Align in [alLeft, alRight] then
  begin
    VSPopupPanel.Top := Top;
    VSPopupPanel.Height := Height;
    VSPopupPanelSplitter.Top := Top;
    VSPopupPanelSplitter.Height := Height;
  end;
end;

procedure TCnVSChannel.FreeBlockList;
var i: Integer;
begin
  for i := 0 to FBlockList.Count - 1 do
    Blocks[i].Free;
  FBlockList.Free;
end;

procedure TCnVSChannel.SetActivePaneSize(const Value: Integer);
begin
  if FActivePaneSize <> Value then
  begin
    FActivePaneSize := Value;
    Invalidate;
  end;
end;

{ TCnVSBlock }

procedure TCnVSBlock.AddDockControl(Control: TWinControl);

  function GetPaneWidth: Integer;
  begin
    Result := 100;
    if Control = nil then Exit;
    case VSChannel.Align of
      alLeft, alRight:
        Result := Control.Width;
      alTop, alBottom:
        Result := Control.Height;
    end;
  end;

var i, PaneWidth: Integer;
  Icon: TIcon;
  DockClient: TCnDockClient;
begin
  PaneWidth := GetPaneWidth;
  if Control is TCnTabDockHostForm then
  begin
    FBlockType := btTabBlock;
    with TCnTabDockHostForm(Control) do
    begin
      for i := 0 to DockableControl.DockClientCount - 1 do
      begin
        FVSPaneList.Add(TCnVSPane.Create(Self, TForm(PageControl.DockClients[i]), PaneWidth, FVSPaneList.Count));
        if not IsLoading then
        begin
          DockClient := FindDockClient(PageControl.DockClients[i]);
          if DockClient <> nil then
            DockClient.VSPaneWidth := PaneWidth;
        end;
        if TForm(PageControl.DockClients[i]).Icon = nil then
        begin
          Icon := TIcon.Create;
          Icon.Width := 16;
          Icon.Height := 16;
          FImageList.AddIcon(Icon);
          Icon.Free;
        end
        else
          FImageList.AddIcon(TForm(PageControl.DockClients[i]).Icon);
        TCnVSNETDockTabSheet(PageControl.Pages[i]).OldVisible := PageControl.DockClients[i].Visible;
        if PageControl.Pages[i] <> PageControl.ActivePage then
          PageControl.DockClients[i].Visible := False;
      end;
      for i := 0 to VSPaneCount - 1 do
      begin
        if VSPanes[i].Visible then
        begin
          FActiveDockControl := VSPanes[i].DockForm;
          Break;
        end;
      end;
    end;
  end else
  begin
    FBlockType := btConjoinBlock;
    FVSPaneList.Add(TCnVSPane.Create(Self, TForm(Control), PaneWidth, FVSPaneList.Count));
    if not IsLoading then
    begin
      DockClient := FindDockClient(Control);
      if DockClient <> nil then
        DockClient.VSPaneWidth := PaneWidth;
    end;
    if TForm(Control).Icon = nil then
    begin
      Icon := TIcon.Create;
      Icon.Width := 16;
      Icon.Height := 16;
      FImageList.AddIcon(Icon);
      Icon.Free;
    end else
      FImageList.AddIcon(TForm(Control).Icon);
    FActiveDockControl := Control;
  end;
  ResetActiveBlockWidth;
end;

constructor TCnVSBlock.Create(Owner: TCnVSChannel);
begin
  FVSChannel := Owner;
  FVSPaneList := TList.Create;
  FImageList := TImageList.CreateSize(16, 16);
  FInactiveBlockWidth := 24;
  FActiveBlockWidth := 24;
end;

destructor TCnVSBlock.Destroy;
var i: Integer;
begin
  FImageList.Free;
  for i := 0 to VSPaneCount - 1 do
    VSPanes[i].Free;
  FVSPaneList.Free;
  inherited;
end;

function TCnVSBlock.GetVSPane(Index: Integer): TCnVSPane;
begin
  Result := TCnVSPane(FVSPaneList[Index]);
end;

function TCnVSBlock.GetVSPaneCount: Integer;
begin
  Result := FVSPaneList.Count;
end;

function TCnVSBlock.GetTotalWidth: Integer;
begin
  Result := (FVSPaneList.Count - 1) * FInactiveBlockWidth + FActiveBlockWidth;
end;

procedure TCnVSBlock.RemoveDockControl(Control: TWinControl);
begin
  ResetActiveBlockWidth;
end;

procedure TCnVSBlock.ResetActiveBlockWidth;
var i: Integer;
begin
  for i := 0 to VSPaneCount - 1 do
  begin
    FActiveBlockWidth := Max(FActiveBlockWidth, min(VSChannel.ActivePaneSize,
      TForm(VSChannel.Parent).Canvas.TextWidth(VSPanes[i].DockForm.Caption) + InactiveBlockWidth + 10));
  end;
end;

procedure TCnVSBlock.DeletePane(Index: Integer);
var i: Integer;
begin
  for i := Index to FVSPaneList.Count - 2 do
    VSPanes[i+1].Index := VSPanes[i].Index;
  VSPanes[Index].Free;
  FVSPaneList.Delete(Index);
end;

{ TCnVSNETDockPanel }

constructor TCnVSNETDockPanel.Create(AOwner: TComponent);
begin
  inherited;
  FVSChannelClass := TCnVSChannel;
end;

procedure TCnVSNETDockPanel.CreateVSChannel;
begin
  if (FVSChannelClass <> nil) and
    (FVSChannelClass <> TCnVSChannelClass(ClassType)) then
  begin
    FVSChannel := FVSChannelClass.Create(Self);
    FVSChannel.Parent := Parent;
    FVSChannel.Align := Align;
    // 重新设置字体的角度
    FVSChannel.ResetFontAngle;
    // 重新设置位置
    FVSChannel.ResetPosition;
    // 刚开始创建的时候就要把它隐藏
    FVSChannel.Visible := False;
    // 设置它的名称
    FVSChannel.Name := Name + '_VSChannel';
    // 创建TCnVSPopupPanel
    FVSChannel.CreateVSPopupPanel;
  end;
end;

procedure TCnVSNETDockPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  inherited;
  VSChannel.ActiveDockForm.Perform(CM_EXIT, 0, 0);
end;

destructor TCnVSNETDockPanel.Destroy;
begin
  inherited;

end;

procedure TCnVSNETDockPanel.DestroyVSChannel;
begin
end;

procedure TCnVSNETDockPanel.DoAutoHideControl(Control: TWinControl);
//var
//  ADockClient: TCnDockClient;
//  ADockServer: TCnDockServer;
//  Panel: TCnVSNETDockPanel;
begin
  { 必须是能够符合停靠条件的才行 }
(*  if self is TCnVSPopupPanel then
  begin
    Panel := TCnVSPopupPanel(self).FVSNETDockPanel;
    ADockClient := FindDockClient(Control);
    if ADockClient <> nil then
    begin
      with ADockClient, Panel do
      begin
        { 对于停靠客户 }
        if (not ADockClient.EnableDock) or
          ((not LeftDock) and (Align = alLeft)) or
          ((not RightDock) and (Align = alRight)) or
          ((not TopDock) and (Align = alTop)) or
          ((not BottomDock) and (Align = alBottom)) then
            Exit;
        { 对于停靠服务器 }
        ADockServer := DockServer;
        if ADockServer <> nil then
          if (not ADockServer.EnableDock) or
            ((not ADockServer.LeftDock) and (Align = alLeft)) or
            ((not ADockServer.RightDock) and (Align = alRight)) or
            ((not ADockServer.TopDock) and (Align = alTop)) or
            ((not ADockServer.BottomDock) and (Align = alBottom)) then
              Exit;
      end;
    end;
  end;*)
  if Align = alNone then
    DoShowControl(Control)
  else
    DoHideControl(Control);
end;

procedure TCnVSNETDockPanel.DoHideControl(Control: TWinControl);
begin
  VSChannel.AddDockControl(Control);
  ShowDockPanel(VisibleDockClientCount > 1, Control, sdfDockPanel);
  Control.Dock(VSChannel.VSPopupPanel, Rect(0, 0, 0, 0));
  VSChannel.VSPopupPanel.CnDockManager.InsertControl(Control, alNone, nil);
  VSChannel.VSPopupPanel.CnDockManager.ShowSingleControl(Control);
  CnDockManager.HideControl(Control);
  ResetChannelBlockStartOffset(VSChannel);
end;

procedure TCnVSNETDockPanel.DoShowControl(Control: TWinControl);
var Panel: TCnVSNETDockPanel;
  // 重新设置停靠窗体的Visible
  procedure ResetDockFormVisible;
  var i: Integer;
  begin
    if Control is TCnTabDockHostForm then
    begin
      with TCnTabDockHostForm(Control) do
        for i := 0 to PageControl.PageCount - 1 do
        begin
          PageControl.Pages[i].Visible := TCnVSNETDockTabSheet(PageControl.Pages[i]).OldVisible;
          PageControl.Pages[i].Controls[0].Visible := PageControl.Pages[i].Visible;
        end;
    end;
  end;
begin
  if self is TCnVSPopupPanel then
  begin
    Panel := TCnVSPopupPanel(self).FVSNETDockPanel;
    Control.Dock(Panel, Rect(0, 0, 0, 0));
    Panel.CnDockManager.ShowControl(Control);
    CnDockManager.RemoveControl(Control);
    Panel.VSChannel.RemoveDockControl(Control);
    Panel.ShowDockPanel(Panel.VisibleDockClientCount > 0, Control, sdfDockPanel);
    if (Panel.VSChannel.ActiveDockForm <> nil) and Panel.VSChannel.ActiveDockForm.CanFocus then
      Panel.VSChannel.ActiveDockForm.SetFocus;
    Panel.VSChannel.HidePopupPanel(Panel.VSChannel.FActivePane);
    ResetDockFormVisible;
    ResetChannelBlockStartOffset(Panel.VSChannel);
  end;
end;

procedure TCnVSNETDockPanel.Resize;
begin
  inherited;
  if Align in [alTop, alBottom] then
  begin
    TCnVSNETDockPanel(DockServer.DockPanelWithAlign[alleft]).VSChannel.ResetPopupPanelHeight;
    TCnVSNETDockPanel(DockServer.DockPanelWithAlign[alRight]).VSChannel.ResetPopupPanelHeight;
  end;
end;

procedure TCnVSNETDockPanel.SetDockServer(const Value: TCnDockServer);
begin
  inherited;
  if not (Self is TCnVSPopupPanel) then
    CreateVSChannel;

end;

{ TCnVSPane }

constructor TCnVSPane.Create(ABlock: TCnVSBlock; AForm: TForm; AWidth: Integer; AIndex: Integer);
begin
  Block := ABlock;
  DockForm := AForm;
  Width := AWidth;
  Active := False;
  Index := AIndex;
  Visible := AForm.Visible;
end;

destructor TCnVSPane.Destroy;
begin
  inherited;

end;

{ TCnVSPopupPanel }

constructor TCnVSPopupPanel.Create(AOwner: TComponent);
begin
  inherited;
  DockSite := True;
  if AOwner is TCnVSNETDockPanel then
  begin
    FVSNETDockPanel := TCnVSNETDockPanel(AOwner);
    FVSChannel := FVSNETDockPanel.VSChannel;
    DockServer := FVSNETDockPanel.DockServer;
  end;
  Anchors := [akLeft, akRight, akTop, akBottom];
  BoundsRect := Rect(0, 0, 0, 0);
end;

function TCnVSPopupPanel.CreateDockManager: IDockManager;
begin
  if (DockManager = nil) and DockSite and UseDockManager then
    Result := TCnVSNETDockTree.Create(
      Self, TCnVSNETDockZone) as ICnDockManager
    else Result := DockManager;
end;

destructor TCnVSPopupPanel.Destroy;
begin
  inherited;

end;

function TCnVSPopupPanel.GetVSChannel: TCnVSChannel;
begin
  if FVSNETDockPanel <> nil then
    Result := FVSNETDockPanel.VSChannel
  else Result := nil;
end;

procedure TCnVSPopupPanel.SetParent(AParent: TWinControl);
begin
  inherited;
  if AParent = nil then Exit;
end;

procedure TCnVSPopupPanel.SetVSNETDockPanel(
  const Value: TCnVSNETDockPanel);
begin
  FVSNETDockPanel := Value;
end;

procedure TCnVSPopupPanel.ShowDockPanel(MakeVisible: Boolean;
  Client: TControl; PanelSizeFrom: TSetDockPanelSizeFrom);
begin
  if Align <> alNone then
    inherited;
end;

{ TCnVSNETDockTabSheet }

constructor TCnVSNETDockTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  FOldVisible := True;
end;

procedure TCnVSNETDockTabSheet.SetOldVisible(const Value: Boolean);
begin
  FOldVisible := Value;
end;

{ TCnVSPopupPanelSplitter }

type
  TWinControlAccess = class(TWinControl);

constructor TCnVSPopupPanelSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := False;
  Align := alNone;
  Height := 0;
  Width := 0;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
  FSplitWidth := 4;
  Anchors := [akLeft, akRight, akTop, akBottom];
end;

destructor TCnVSPopupPanelSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TCnVSPopupPanelSplitter.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS
    or DCX_LOCKWINDOWUPDATE);
  if ResizeStyle = rsPattern then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    end;
    FPrevBrush := SelectObject(FLineDC, FBrush.Handle);
  end;
end;

procedure TCnVSPopupPanelSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if VSChannelAlign in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TCnVSPopupPanelSplitter.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

function TCnVSPopupPanelSplitter.FindControl: TControl;
begin
  Result := FVSPopupPanel;
end;

procedure TCnVSPopupPanelSplitter.RequestAlign;
begin
  inherited RequestAlign;
//  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then Exit;
  if VSChannelAlign in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TCnVSPopupPanelSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  InflateRect(R, 2, 2);
  case VSChannelAlign of
    alLeft:
    begin
      Dec(R.Right, 2);
    end;
    alRight:
    begin
      Inc(R.Left, 3);
    end;
    alTop:
    begin
      Dec(R.Bottom, 2);
    end;
    alBottom:
    begin
      Inc(R.Top, 3);
    end;
  end;
  DrawFrameControl(Canvas.Handle, R, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
  R := ClientRect;
  if Beveled then
  begin
    if VSChannelAlign in [alLeft, alRight] then
      InflateRect(R, -1, 2) else
      InflateRect(R, 2, -1);
    OffsetRect(R, 1, 1);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnHighlight));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
    OffsetRect(R, -2, -2);
    FrameBrush := CreateSolidBrush(ColorToRGB(clBtnShadow));
    FrameRect(Canvas.Handle, R, FrameBrush);
    DeleteObject(FrameBrush);
  end;
  if csDesigning in ComponentState then
    { Draw outline }
    with Canvas do
    begin
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Color := XorColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, ClientWidth, ClientHeight);
    end;
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

function TCnVSPopupPanelSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

function TCnVSPopupPanelSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewSize, Result);
end;

procedure TCnVSPopupPanelSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FControl := FindControl;
    FDownPos := Point(X, Y);
    if Assigned(FControl) then
    begin
      if VSChannelAlign in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alLeft, alRight] then Dec(FMaxSize, Width);
        Inc(FMaxSize, FControl.Width);
      end
      else
      begin
        FMaxSize := Parent.ClientHeight - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Align in [alTop, alBottom] then Dec(FMaxSize, Height);
        Inc(FMaxSize, FControl.Height);
      end;
      UpdateSize(X, Y);
      AllocateLineDC;
      with ValidParentForm(Self) do
        if ActiveControl <> nil then
        begin
          FActiveControl := ActiveControl;
          FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
          TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TCnVSPopupPanelSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case VSChannelAlign of
      alLeft:
        begin
          FControl.Width := FNewSize;
          Left := FControl.Left + FNewSize;
        end;
      alTop:
        begin
          FControl.Height := FNewSize;
          Top := FControl.Top + FNewSize;
        end;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
            Left := FControl.Left - Width;
          finally
            Parent.EnableAlign;
          end;
        end;
      alBottom:
        begin
          Parent.DisableAlign;
          try
            FControl.Top := FControl.Top + (FControl.Height - FNewSize);
            FControl.Height := FNewSize;
            Top := FControl.Top - Height;
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    FVSPopupPanel.VSChannel.ResetActivePaneWidth;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TCnVSPopupPanelSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if VSChannelAlign in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case VSChannelAlign of
    alLeft: S := FControl.Width + Split;
    alRight: S := FControl.Width - Split;
    alTop: S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if VSChannelAlign in [alRight, alBottom] then
      S := S - NewSize else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TCnVSPopupPanelSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TCnVSPopupPanelSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TCnVSPopupPanelSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Assigned(FControl) then
  begin
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    StopSizing;
  end;
end;

procedure TCnVSPopupPanelSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TCnVSPopupPanelSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TCnVSPopupPanelSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TCnVSPopupPanelSplitter.SetVSPopupPanel(
  const Value: TCnVSPopupPanel);
begin
  Assert((Value <> nil) and (Value is TCnVSPopupPanel));
  FVSPopupPanel := Value;
end;

function TCnVSPopupPanelSplitter.GetVSChannelAlign: TAlign;
begin
  Result := alNone;
  if (VSPopupPanel <> nil) and (VSPopupPanel.FVSNETDockPanel <> nil) then
    Result := VSPopupPanel.FVSNETDockPanel.Align;
end;

procedure TCnVSPopupPanelSplitter.SetSplitWidth(const Value: Integer);
begin
  FSplitWidth := Value;
end;

{ TPopupPanelAnimate }

constructor TPopupPanelAnimate.Create(AOwner: TComponent);
begin
  inherited;
  Interval := PopupPanelAnimateInterval;
  Enabled := False;
  FMaxWidth := 0;
  FCurrentWidth := 0;
  OnTimer := OnCustomTimer;
  FState := asPopup;
end;

destructor TPopupPanelAnimate.Destroy;
begin
  inherited;

end;

procedure TPopupPanelAnimate.HideForm(VSChannel: TCnVSChannel; MaxWidth: Integer);
begin
  FVSChannel := VSChannel;
  Enabled := (FVSChannel <> nil) and (FVSChannel.ActiveDockForm <> nil);
  if FVSChannel <> nil then
  begin
    FMaxWidth := MaxWidth;
    FCurrentWidth := 0;
    FState := asHide;
  end;
end;

procedure TPopupPanelAnimate.OnCustomTimer(Sender: TObject);
begin
//
end;

procedure TPopupPanelAnimate.PopupForm(VSChannel: TCnVSChannel; MaxWidth: Integer);
begin
  if (FCurrentWidth > 0) and (FVSChannel <> nil) then
  begin
    FVSChannel.Parent.EnableAlign;
  end;
  FVSChannel := VSChannel;
  Enabled := FVSChannel <> nil;
  if FVSChannel <> nil then
  begin
    FMaxWidth := MaxWidth;
    FCurrentWidth := 0;
    FState := asPopup;
  end;
end;

procedure TPopupPanelAnimate.Timer;
  procedure SetControlBringToFront(Control: TWincontrol; Align: TAlign);
  var i: Integer;
  begin
    for i := Control.ControlCount - 1 downto 0 do
    begin
      if Control.Controls[i].Visible and (Control.Controls[i].Align = Align)
        and not (Control.Controls[i] is TCnVSChannel) and not (Control.Controls[i] is TCnDockPanel)
        and not (Control.Controls[i] is TCnDockSplitter) then
        Control.Controls[i].BringToFront;
    end;
  end;
var SuitablyWidth: Integer;
begin
  inherited;
  if FVSChannel <> nil then
  begin
    SuitablyWidth := min(FCurrentWidth, FMaxwidth);
    with FVSChannel do
    begin
      if FCurrentWidth = 0 then
      begin
        Parent.DisableAlign;
        VSPopupPanel.BringToFront;
        VSPopupPanelSplitter.BringToFront;
        SetControlBringToFront(Parent, Align);
        BringToFront;
      end;
      case Align of
        alLeft:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Width := FMaxWidth;
              VSPopupPanel.Top := Top;
              VSPopupPanel.Height := Height;
              VSPopupPanelSplitter.Top := Top;
              VSPopupPanelSplitter.Height := Height;
              VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Left := Left + Width + SuitablyWidth - VSPopupPanel.Width;
          end else if FState = asHide then
            VSPopupPanel.Left := Left - FCurrentWidth;

          VSPopupPanelSplitter.Left := VSPopupPanel.Left + VSPopupPanel.Width;
        end;

        alRight:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Width := FMaxWidth;
              VSPopupPanel.Top := Top;
              VSPopupPanel.Height := Height;
              VSPopupPanelSplitter.Top := Top;
              VSPopupPanelSplitter.Height := Height;
              VSPopupPanelSplitter.Width := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Left := Left - SuitablyWidth;
          end else if FState = asHide then
            VSPopupPanel.Left := Left - VSPopupPanel.Width + FCurrentWidth;

          VSPopupPanelSplitter.Left := VSPopupPanel.Left - VSPopupPanelSplitter.SplitWidth;
        end;

        alTop:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Left := Left;
              VSPopupPanel.Height := FMaxWidth;
              VSPopupPanel.Width := Width;
              VSPopupPanelSplitter.Left := Left;
              VSPopupPanelSplitter.Width := Width;
              VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Top := Top + Height + SuitablyWidth - VSPopupPanel.Height;
          end else if FState = asHide then
            VSPopupPanel.Top := Top - FCurrentWidth;

          VSPopupPanelSplitter.Top := VSPopupPanel.Top + VSPopupPanel.Height;
        end;

        alBottom:
        begin
          if FState = asPopup then
          begin
            if FCurrentWidth = 0 then
            begin
              VSPopupPanel.Left  := Left;
              VSPopupPanel.Width := Width;
              VSPopupPanel.Height:= FMaxWidth;
              VSPopupPanelSplitter.Left := Left;
              VSPopupPanelSplitter.Width := Width;
              VSPopupPanelSplitter.Height := VSPopupPanelSplitter.SplitWidth;
            end;
            VSPopupPanel.Top := Top - SuitablyWidth;
          end else if FState = asHide then
            VSPopupPanel.Top := Top - VSPopupPanel.Height + FCurrentWidth;

          VSPopupPanelSplitter.Top := VSPopupPanel.Top - VSPopupPanelSplitter.SplitWidth;
        end;
      end;
      VSPopupPanel.Visible := True;
      VSPopupPanelSplitter.Visible := True;
    end;
    if FCurrentWidth >= FMaxwidth then
    begin
      FVSChannel.Parent.EnableAlign;
      Enabled := False;
      if FState = asHide then
        FVSChannel.HidePopupPanel(FVSChannel.FActivePane)
      else
      begin
        if FVSChannel.ActiveDockForm <> nil then
        begin
          if FVSChannel.ActiveDockForm.CanFocus then
            FVSChannel.ActiveDockForm.SetFocus;
        end;
      end;
      FVSChannel := nil;
      FCurrentWidth := 0;
      FMaxwidth := 0;
    end else
      Inc(FCurrentWidth, PopupPanelAnimateMoveWidth);
  end;

end;

{ TCnVSNETChannelOption }

constructor TCnVSNETChannelOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  inherited;
  FActivePaneSize := 100;
  FShowImage := True;
end;

procedure TCnVSNETChannelOption.ResetDockClientOption(
  ADockClient: TCnDockClient);
var VSChannel: TCnVSChannel;

  procedure ResetActiveBlockSize;
  begin
    if VSChannel <> nil then
      VSChannel.ActivePaneSize := ActivePaneSize;
  end;

begin
  if ADockClient = nil then Exit;
  if ADockClient.ParentForm.HostDockSite is TCnVSPopupPanel then
    VSChannel := TCnVSPopupPanel(ADockClient.ParentForm.HostDockSite).VSChannel
  else if (ADockClient.ParentForm.HostDockSite is TCnVSNETTabPageControl) and
    (ADockClient.ParentForm.HostDockSite.Parent.HostDockSite is TCnVSPopupPanel) then
    VSChannel := TCnVSPopupPanel(ADockClient.ParentForm.HostDockSite.Parent.HostDockSite).VSChannel;
  ResetActiveBlockSize;
end;

procedure TCnVSNETChannelOption.ResetDockControlOption;
var i: Integer;
  ADockServer: TCnDockServer;
begin
  if DockStyle = nil then Exit;
  { 循环DockStyle的DockBaseControlList列表，然后把每一个TCnDockServer或者
    TCnDockClient取出来，然后调用各自的函数重新设置它们的选项 }
  for i := 0 to DockStyle.DockBaseControlListCount - 1 do
  begin
    if DockStyle.DockBaseControlLists[i] is TCnDockServer then
    begin
      { 重新设置TCnDockServer的选项 }
      ADockServer := TCnDockServer(DockStyle.DockBaseControlLists[i]);
      ResetDockServerOption(ADockServer);
    end;
  end;
end;

procedure TCnVSNETChannelOption.ResetDockServerOption(
  ADockServer: TCnDockServer);
var VSChannel: TCnVSChannel;

  procedure ResetActiveBlockSize;
  begin
    if VSChannel <> nil then
      VSChannel.ActivePaneSize := ActivePaneSize;
  end;

var i: Integer;
begin
  if ADockServer = nil then Exit;
  for i := 0 to 3 do
  begin
    if ADockServer.DockPanel[i] = nil then Continue;
    VSChannel := TCnVSNETDockPanel(ADockServer.DockPanel[i]).VSChannel;
    ResetActiveBlockSize;
  end;
end;

procedure TCnVSNETChannelOption.SetActivePaneSize(const Value: Integer);
begin
  if FActivePaneSize <> Value then
  begin
    FActivePaneSize := Max(24, Value);
    ResetDockControlOption;
  end;
end;

procedure TCnVSNETChannelOption.SetShowImage(const Value: Boolean);
begin
  FShowImage := Value;
end;

{ TCnAppEvents }

constructor TCnAppEvents.Create(AOwner: TComponent);
begin
  inherited;
  FOldOnMessage := OnMessage;
  OnMessage := NewOnMessage;
end;

procedure TCnAppEvents.NewOnMessage(var Msg: TMsg; var Handled: Boolean);
var CurrControl: TWinControl;
  DockServer: TCnDockServer;
  VSChannel: TCnVSChannel;
  i, j: Integer;
  { 是否可以隐藏 }
  function CanHide: Boolean;
  begin
    Result := True;
    CurrControl := FindControl(Msg.hwnd);
    if CurrControl = nil then Exit;
    repeat
      { 只有在运行期才能隐藏 }
      if csDesigning in CurrControl.ComponentState then
      begin
        Result := False;
        Exit;
      end;
      { 鼠标点击的控件不能是TCnVSChannel, TCnVSPopupPanel, TCnVSPopupPanelSplitter或者是前面三种控件的子控件}
      Result := not ((CurrControl is TCnVSChannel) or (CurrControl is TCnVSPopupPanel) or (CurrControl is TCnVSPopupPanelSplitter));
      CurrControl := CurrControl.Parent;
    until (CurrControl = nil) or not Result;

  end;
begin
  if Assigned(FOldOnMessage) then
    FOldOnMessage(Msg, Handled);
  if (Msg.message = WM_LBUTTONDOWN){ or (Msg.message = WM_NCLBUTTONDOWN)} then
  begin
    if CanHide then
    begin
      for i := 0 to Screen.CustomFormCount - 1 do
      begin
        DockServer := FindDockServer(Screen.CustomForms[i]);
        if (DockServer <> nil) and (DockServer.DockStyle is TCnVSNETDockStyle) then
        begin
          if DockServer.DockPanel[0] = nil then Exit;
          for j := 0 to 3 do
          begin
            VSChannel := TCnVSNETDockPanel(DockServer.DockPanel[j]).VSChannel;
            VSChannel.HidePopupPanelWithAnimate(VSChannel.FActivePane);
          end;
        end;
      end;
//      Windows.SetFocus(Msg.hwnd);
    end;
  end;
end;

initialization
  PopupPanelAnimate := TPopupPanelAnimate.Create(nil);
  ApplicationEvents := TCnAppEvents.Create(nil);

finalization
  PopupPanelAnimate.Free;
  ApplicationEvents.Free;

end.
