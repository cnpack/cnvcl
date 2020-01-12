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

{*******************************************************}
{                                                       }
{       具有类似Visual InterDev的停靠风格               }
{       lbVIDDockStyle 单元                             }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

{WM_SETTINGCHANGE: 当设置桌面属性的外观后，Windows会给每一个应用程序发送
                   WM_SETTINGCHANGE消息，这时候可以截获这个消息来设置
                   TlbVIDConjoinServerOption中的属性。}

{WM_SYSCOLORCHANGE: 当设置桌面属性的颜色外观后，Windows会给每一个应用程序发送
                   WM_SYSCOLORCHANGE消息，这时候可以截获这个消息来设置
                   TlbVIDConjoinServerOption中的属性的颜色部分。}

unit CnVIDDockStyle;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：类似于Visual InterDev的停靠风格的单元 
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
  ImgList, Forms, SysUtils, Dialogs, CnDockFormControl, CnDockSupportControl,
  CnDockTree, CnConsts, CnCompConsts;

const
  VIDDefaultGrabbersSize   = 18;
  VIDDefaultSplitterWidth = 4;

type
  { VID平铺服务器的选项类 }
  TCnVIDConjoinServerOption = class(TCnBasicConjoinServerOption)
  private
    FTextEllipsis: Boolean;
    FTextAlignment: TAlignment;
    FInactiveTitleEndColor: TColor;
    FInactiveTitleStartColor: TColor;
    FActiveTitleEndColor: TColor;
    FActiveTitleStartColor: TColor;
    FSystemInfo: Boolean;
    FActiveFont: TFont;
    FInactiveFont: TFont;
    procedure SetActiveTitleEndColor(const Value: TColor);
    procedure SetActiveTitleStartColor(const Value: TColor);
    procedure SetInactiveTitleEndColor(const Value: TColor);
    procedure SetInactiveTitleStartColor(const Value: TColor);
    procedure SetTextAlignment(const Value: TAlignment);
    procedure SetTextEllipsis(const Value: Boolean);
    procedure SetSystemInfo(const Value: Boolean);
    procedure SetActiveFont(const Value: TFont);
    procedure SetInactiveFont(const Value: TFont);
  protected
    // 重新设置DockStyle的选项
    procedure ResetDockControlOption; override;
    procedure SetDefaultSystemCaptionInfo; virtual;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetActiveTitleEndColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetActiveTitleStartColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetInactiveTitleEndColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetInactiveTitleStartColor_WithoutChangeSystemInfo(const Value: TColor);
    procedure SetTextAlignment_WithoutChangeSystemInfo(const Value: TAlignment);
    procedure SetTextEllipsis_WithoutChangeSystemInfo(const Value: Boolean);
    procedure SetActiveFont_WithoutChangeSystemInfo(const Value: TFont);
    procedure SetInactiveFont_WithoutChangeSystemInfo(const Value: TFont);
  published
    { 当页面获得焦点的时候的字体颜色 }
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    { 当页面失去焦点的时候的字体颜色 }
    property InactiveFont: TFont read FInactiveFont write SetInactiveFont;
    { Caption的对齐方式 }
    property TextAlignment: TAlignment read FTextAlignment
      write SetTextAlignment;
    { 当客户窗体获得焦点的时候标题栏的开始部分颜色 }
    property ActiveTitleStartColor: TColor read FActiveTitleStartColor
      write SetActiveTitleStartColor;
    { 当客户窗体获得焦点的时候标题栏的结束部分颜色 }
    property ActiveTitleEndColor: TColor read FActiveTitleEndColor
      write SetActiveTitleEndColor;
    { 当客户窗体失去焦点的时候标题栏的开始部分颜色 }
    property InactiveTitleStartColor: TColor read FInactiveTitleStartColor
      write SetInactiveTitleStartColor;
    { 当客户窗体失去焦点的时候标题栏的结束部分颜色 }
    property InactiveTitleEndColor: TColor read FInactiveTitleEndColor
      write SetInactiveTitleEndColor;
    { 标题栏上的文字是否有省略号 }
    property TextEllipsis: Boolean read FTextEllipsis write SetTextEllipsis;
    { 是否和系统信息是一样的 }
    property SystemInfo: Boolean read FSystemInfo write SetSystemInfo;
  end;

  { VID分页服务器的选项类 }
  TCnVIDTabServerOption = class(TCnBasicTabServerOption)
  private
    FActiveFont: TFont;
    FActiveSheetColor: TColor;
    FHotTrackColor: TColor;
    FInactiveFont: TFont;
    FInactiveSheetColor: TColor;
    FShowTabImages: Boolean;

    function GetActiveFont: TFont;
    function GetActiveSheetColor: TColor;
    function GetHotTrackColor: TColor;
    function GetInactiveFont: TFont;
    function GetInactiveSheetColor: TColor;
    function GetShowTabImages: Boolean;
    procedure SetActiveFont(const Value: TFont);
    procedure SetActiveSheetColor(const Value: TColor);
    procedure SetHotTrackColor(const Value: TColor);
    procedure SetInactiveFont(const Value: TFont);
    procedure SetInactiveSheetColor(const Value: TColor);
    procedure SetShowTabImages(const Value: Boolean);
  protected
    // 重新设置DockStyle的选项
    procedure ResetDockControlOption; override;
    { 重新设置TCnTabPageControl的属性 }
    procedure ResetTabPageControl(APage: TCnTabPageControl); override;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetTabPosition(const Value: TTabPosition); override;
  published
    { 当页面获得焦点的时候的颜色 }
    property ActiveSheetColor: TColor read GetActiveSheetColor write SetActiveSheetColor;
    { 当页面失去焦点的时候的颜色 }
    property InactiveSheetColor: TColor read GetInactiveSheetColor write SetInactiveSheetColor;
    { 当页面获得焦点的时候的字体颜色 }
    property ActiveFont: TFont read GetActiveFont write SetActiveFont;
    { 当页面失去焦点的时候的字体颜色 }
    property InactiveFont: TFont read GetInactiveFont write SetInactiveFont;
    { 高亮显示的颜色 }
    property HotTrackColor: TColor read GetHotTrackColor write SetHotTrackColor;
    { 是否显示图片 }
    property ShowTabImages: Boolean read GetShowTabImages write SetShowTabImages;
  end;

  { 当TCnVIDConjoinServerOption的SystemInfo属性改变的时候触发这个事件 }
  TSystemInfoChange = procedure(Value: Boolean) of object;

  TCnVIDDockStyle = class(TCnAdvDockStyle)
  private
    FSystemInfoChange: TSystemInfoChange;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    function DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean; override;
    procedure ParentFormWindowProc(var Message: TMessage); override;

    procedure FormDockDrop(DockClient: TCnDockClient;
      Source: TCnDragDockObject; X, Y: Integer); override;
    procedure FormGetSiteInfo(Source: TCnDragDockObject; DockClient: TCnDockClient;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure FormDockOver(DockClient: TCnDockClient; Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure FormStartDock(DockClient: TCnDockClient;
      var Source: TCnDragDockObject); override;
    procedure FormGetDockEdge(DockClient: TCnDockClient; Source: TCnDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); override;
    { ------------------------------------------------------------------------ }
    procedure CreateConjoinServerOption(var Option: TCnBasicConjoinServerOption); override;
    procedure CreateTabServerOption(var Option: TCnBasicTabServerOption); override;
    { ------------------------------------------------------------------------ }
    procedure AssignConjoinServerOption(APanel: TCnCustomDockPanel); override;
    procedure AssignTabServerOption(APage: TCnTabPageControl); override;
    { ------------------------------------------------------------------------ }
    procedure DoSystemInfoChange(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    class function GetControlName: string; override;
    function GetControlName: string; override;
    procedure SetDockBaseControl(IsCreate: Boolean;
      DockBaseControl: TCnDockBaseControl); override;
  published
    property SystemInfoChange: TSystemInfoChange read FSystemInfoChange
      write FSystemInfoChange;
    property ConjoinServerOption;
    property TabServerOption;
    { ------------------------------------------------------------------------ }
  end;

  TCnVIDDockSplitter = class(TCnDockSplitter);

  TCnVIDDockPanel = class(TCnAdvDockPanel)
  protected
    procedure CustomGetSiteInfo(Source: TCnDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomStartDock(var Source: TCnDragDockObject); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint;
      var DropAlign: TAlign); override;
    function CreateDockManager: IDockManager; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TCnVIDConjoinPanel = class(TCnAdvConjoinPanel)
  protected
    procedure CustomGetSiteInfo(Source: TCnDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    function CreateDockManager: IDockManager; override;
  public
    procedure UpdateCaption(Exclude: TControl); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TCnVIDDockZone = class(TCnAdvDockZone)
  protected
    function GetSplitterLimit(IsMin: Boolean): Integer; override;
  public
    destructor Destroy; override;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
  end;

  TCnVIDDockTree = class(TCnAdvDockTree)
  private
    FDropOnZone: TCnDockZone;
    FLockDropDockSizeCount: Integer;
    // 标题栏文字离左边界的距离
    FCaptionLeftOffset: Integer;
    // 标题栏文字离右边界的距离
    FCaptionRightOffset: Integer;

    procedure LockDropDockSize;
    procedure UnlockDropDockSize;
    procedure SetCaptionLeftOffset(const Value: Integer);
    procedure SetCaptionRightOffset(const Value: Integer);
  protected
    { 当停靠来的控件是TCnConjoinPanel的时候，先把TCnConjoinPanel中的停靠信息
    存储在Stream流中，然后再把这些信息取出来重新构造DockTree的结构，使其符合原来的结构 }
    procedure InsertControlFromConjoinHost(Control: TControl;
      InsertAt: TAlign; DropCtl: TControl); virtual;
    { 忽略掉没有用的信息 }
    procedure IgnoreZoneInfor(Stream: TMemoryStream); virtual;
    { 调整Control控件的大小 }
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); override;
    procedure WindowProc(var Message: TMessage); override;
    procedure SplitterMouseUp; override;

    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; override;
    function GetGrabbersPosition: TGrabbersPosition; override;
    procedure GetSiteInfo(Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure InsertSibling(NewZone, SiblingZone: TCnDockZone;
        InsertLast, Update: Boolean); override;
    procedure InsertNewParent(NewZone, SiblingZone: TCnDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure DrawDockGrabber(Control: TControl; const ARect: TRect); override;
    procedure DrawSplitterRect(const ARect: TRect); override;
    { 重画把手的边缘 }
    procedure PaintDockGrabberRect(Canvas: TCanvas; Control: TControl;
      const ARect: TRect); virtual;
    { 重画关闭按钮 }
    procedure DrawCloseButton(Canvas: TCanvas; Zone: TCnDockZone;
      Left, Top: Integer); virtual;
    procedure ResetBounds(Force: Boolean); override;
    procedure SetActiveControl(const Value: TControl); override;
    procedure DrawDockSiteRect; override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); override;
    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; override;
    procedure RemoveZone(Zone: TCnDockZone; Hide: Boolean = True); override;
    procedure GetCaptionRect(var Rect: TRect); override;
    property CaptionLeftOffset: Integer read FCaptionLeftOffset write SetCaptionLeftOffset;
    property CaptionRightOffset: Integer read FCaptionRightOffset write SetCaptionRightOffset;
  public
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); override;
    destructor Destroy; override;
  end;

  TCnVIDTabPageControl = class;

  TCnVIDDockTabSheet = class(TCnDockTabSheet)
  private
    FTabWidth: Integer;
    FShowTabWidth: Integer;
    FIsSourceDockClient: Boolean;
    procedure SetTabWidth(const Value: Integer);
    procedure WMSETTEXT(var Message: TMessage); message WM_SETTEXT;
    procedure SetSheetSort(CaptionStr: string);
  protected
    procedure SetPageControl(APageControl: TCnDockPageControl); override;
    property TabWidth: Integer read FTabWidth write SetTabWidth;
    property ShowTabWidth: Integer read FShowTabWidth;
    procedure Loaded; override;
    procedure UpdateTabShowing; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BorderWidth;
    property Caption;
    property DragMode;
    property Enabled;
    property Font;
    property Height stored False;
    property Highlighted;
    property ImageIndex;
    property Left stored False;
    property Constraints;
    property PageIndex;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow;
    property OnStartDrag;
  end;

  TCnTabPanel = class(TCustomControl)
  private
    FPage: TCnVIDTabPageControl;
    FActiveSheetColor: TColor;     //获得焦点的页面的背景颜色
    FHotTrackColor: TColor;        //热点的颜色
    FActiveFont,                   //获得焦点的页面的字体
    FInactiveFont: TFont;          //失去焦点的页面的字体
    FTabLeftOffset: Integer;       //页面左边距
    FTabRightOffset: Integer;      //页面右边距
    FTabTopOffset: Integer;        //页面上边距
    FTabBottomOffset: Integer;     //页面下边距
    FCaptionLeftOffset: Integer;   //标题左边距
    FCaptionRightOffset: Integer;  //标题右边距
    FCaptionTopOffset: Integer;    //标题上边距
    FTabSplitterWidth: Integer;    //页面间距
    FTabHeight: Integer;           //页面高度
    FSortList: TList;              //按照Tab中的文字的长度排列循序的列表
    FSelectSheet: TCnVIDDockTabSheet;//当前被选中的TabSheet
    FTempPages: TList;             //存储临时Tab的列表
    FSelectHotIndex: Integer;      //当前被选中的高亮显示的Tab的索引
    FShowTabImages: Boolean;       //是否显示Tab中的图象
    procedure SetPage(const Value: TCnVIDTabPageControl);
    function GetTotalTabWidth: Integer;
    procedure SetTotalTabWidth(const Value: Integer);
    function GetMinTabWidth: TCnDockTabSheet;
    function GetMaxTabWidth: TCnDockTabSheet;
    procedure SetTabBottomOffset(const Value: Integer);
    procedure SetTabLeftOffset(const Value: Integer);
    procedure SetTabRightOffset(const Value: Integer);
    procedure SetTabTopOffset(const Value: Integer);
    procedure SetCaptionLeftOffset(const Value: Integer);
    procedure SetCaptionRightOffset(const Value: Integer);
    procedure SetCaptionTopOffset(const Value: Integer);
    procedure SetTabSplitterWidth(const Value: Integer);
    function GetSorts(Index: Integer): TCnVIDDockTabSheet;
    function GetPanelHeight: Integer;
    function GetPanelWidth: Integer;
    procedure SetPanelHeight(const Value: Integer);
    function FindSheetWithPos(cX, cY, cTopOffset, cBottomOffset: Integer): Integer;
    function GetDockClientFromPageIndex(Index: Integer): TControl;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetShowTabImages(const Value: Boolean);
    procedure SetTabHeight(const Value: Integer);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    function GetPageIndexFromMousePos(X, Y: Integer): Integer; virtual;
    procedure SetShowTabWidth;
    property TotalTabWidth: Integer read GetTotalTabWidth write SetTotalTabWidth;
    property MinTabWidth: TCnDockTabSheet read GetMinTabWidth;
    property MaxTabWidth: TCnDockTabSheet read GetMaxTabWidth;
    property TabLeftOffset: Integer read FTabLeftOffset write SetTabLeftOffset default 5;
    property TabRightOffset: Integer read FTabRightOffset write SetTabRightOffset default 5;
    property TabTopOffset: Integer read FTabTopOffset write SetTabTopOffset default 2;
    property TabBottomOffset: Integer read FTabBottomOffset write SetTabBottomOffset default 3;
    property TabSplitterWidth: Integer read FTabSplitterWidth write SetTabSplitterWidth default 2;
    property CaptionTopOffset: Integer read FCaptionTopOffset write SetCaptionTopOffset default 0;
    property CaptionLeftOffset: Integer read FCaptionLeftOffset write SetCaptionLeftOffset default 5;
    property CaptionRightOffset: Integer read FCaptionRightOffset write SetCaptionRightOffset default 5;
    property Sorts[Index: Integer]: TCnVIDDockTabSheet read GetSorts;
    property PanelHeight: Integer read GetPanelHeight write SetPanelHeight;
    property PanelWidth: Integer read GetPanelWidth;
    property TabHeight: Integer read FTabHeight write SetTabHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure DeleteSorts(Sheet: TCnVIDDockTabSheet);
    property Page: TCnVIDTabPageControl read FPage write SetPage;
    property SelectSheet: TCnVIDDockTabSheet read FSelectSheet write FSelectSheet;
    property ShowTabImages: Boolean read FShowTabImages write SetShowTabImages;
  end;

  TCnTabPanelClass = class of TCnTabPanel;

  TCnVIDTabPageControl = class(TCnAdvTabPageControl)
  private
    FCnTabPanelClass: TCnTabPanelClass;
    FPanel: TCnTabPanel;
    FTempSheet: TCnVIDDockTabSheet;
    FTabImageList: TCustomImageList;
    procedure SetActiveSheetColor(const Value: TColor);
    procedure SetInactiveSheetColor(const Value: TColor);
    procedure SetTabBottomOffset(const Value: Integer);
    procedure SetTabLeftOffset(const Value: Integer);
    procedure SetTabRightOffset(const Value: Integer);
    procedure SetTabTopOffset(const Value: Integer);
    procedure SetActiveFont(const Value: TFont);
    procedure SetInactiveFont(const Value: TFont);
    procedure SetHotTrackColor(const Value: TColor);
    function GetTabBottomOffset: Integer;
    function GetTabLeftOffset: Integer;
    function GetTabRightOffset: Integer;
    function GetTabTopOffset: Integer;
    function GetInactiveSheetColor: TColor;
    function GetActiveSheetColor: TColor;
    function GetActiveFont: TFont;
    function GetInactiveFont: TFont;
    function GetVisibleTheetCount: Integer;
    function GetHotTrackColor: TColor;
    function GetShowTabImages: Boolean;
    procedure SetShowTabImages(const Value: Boolean);
    function GetPage(Index: Integer): TCnVIDDockTabSheet;
    function GetActiveVIDPage: TCnVIDDockTabSheet;
    procedure SetActiveVIDPage(const Value: TCnVIDDockTabSheet);
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreatePanel; virtual;
    procedure Change; override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetDockClientFromMousePos(MousePos: TPoint): TControl; override;
    procedure Paint; override;
    procedure SetActivePage(Page: TCnDockTabSheet); override;
    procedure SetTabHeight(Value: Smallint); override;
    procedure SetTabPosition(Value: TTabPosition); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure SetHotTrack(Value: Boolean); override;
    procedure SetImages(Value: TCustomImageList); override;
    property CnTabPanelClass: TCnTabPanelClass read FCnTabPanelClass write FCnTabPanelClass;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    property ActiveVIDPage: TCnVIDDockTabSheet read GetActiveVIDPage write SetActiveVIDPage;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure UpdateCaption(Exclude: TControl); override;
    procedure Resize; override;
    property Pages[Index: Integer]: TCnVIDDockTabSheet read GetPage;
    property Panel: TCnTabPanel read FPanel;
    property TempSheet: TCnVIDDockTabSheet read FTempSheet write FTempSheet;
    property VisibleTheetCount: Integer read GetVisibleTheetCount;
  published
    { 当页面获得焦点的时候的颜色 }
    property ActiveSheetColor: TColor read GetActiveSheetColor write SetActiveSheetColor;
    { 当页面失去焦点的时候的颜色 }
    property InactiveSheetColor: TColor read GetInactiveSheetColor write SetInactiveSheetColor;
    property TabLeftOffset: Integer read GetTabLeftOffset write SetTabLeftOffset default 5;
    property TabRightOffset: Integer read GetTabRightOffset write SetTabRightOffset default 5;
    property TabTopOffset: Integer read GetTabTopOffset write SetTabTopOffset default 2;
    property TabBottomOffset: Integer read GetTabBottomOffset write SetTabBottomOffset default 3;
    property ActiveFont: TFont read GetActiveFont write SetActiveFont;
    property InactiveFont: TFont read GetInactiveFont write SetInactiveFont;
    property HotTrackColor: TColor read GetHotTrackColor write SetHotTrackColor;
    property ShowTabImages: Boolean read GetShowTabImages write SetShowTabImages;
    property ActivePage;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabIndex;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TCnVIDDragDockObject = class(TCnDragDockObject)
  private
    FOldDropAlign: TAlign;          //前一次停靠位置
    FCurrState,                     //当前的DockOver的State;
    FOldState: TDragState;          //前一次DockOver的State;
    FOldTarget: Pointer;            //前一次停靠服务器
    FSourceDockClientList: TList;
    FDropTabControl: TCnVIDTabPageControl;
//    FOldDropTabControl: TCnVIDTabPageControl;
    FIsTabDockOver: Boolean;
    FErase: Boolean;
    function GetSourceDockClient(Index: Integer): TControl;
    function GetSourceDockClientCount: Integer;
    procedure SetOldState(const Value: TDragState);
    procedure SetCurrState(const Value: TDragState);
  protected
    procedure GetBrush_PenSize_DrawRect(
      var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean); override;
    procedure MouseMsg(var Msg: TMessage); override;
    procedure DefaultDockImage(Erase: Boolean); override;
    function CanLeave(NewTarget: TWinControl): Boolean; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    function DragFindWindow(const Pos: TPoint): HWND; override;
    function GetDropCtl: TControl; override;
    property SourceDockClients[Index: Integer]: TControl read GetSourceDockClient;
    property SourceDockClientCount: Integer read GetSourceDockClientCount;
    property CurrState: TDragState read FCurrState write SetCurrState;
    property OldState: TDragState read FOldState write SetOldState;
  end;

procedure PaintGradientBackground(Canvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor);

{ 当在TCnVIDTabPageControl上进行停靠预览的时候，这个函数用来实现这个功能 }
procedure SetTabControlPreview(VIDSource: TCnVIDDragDockObject;
  TabControl: TCnVIDTabPageControl;
  State: TDragState; DropAlign: TAlign);

implementation

uses
  CnDockSupportProc, CnDockGlobal;

type
  TCnTempWinControl = class(TWinControl);

var gi_DockRect: TRect;

procedure PaintGradientBackground(Canvas: TCanvas; ARect: TRect;
  StartColor, EndColor: TColor);
{Paints the caption bar's background color(s)}
const
  D = 256;  //will divide the gradient into 256 colors
var
  X, C1, C2, R1, G1, B1, W: Integer;
  DR, DG, DB, DH: Real;

  procedure InitRGBValues(C1, C2: Integer);
  {Sets the initial values for each color}
  begin
    R1:= GetRValue(C1);  //store the red value of FCaptionStartColor
    G1:= GetGValue(C1);  //store the green value of FCaptionStartColor
    B1:= GetBValue(C1);  //store the blue value of FCaptionStartColor
    DR:= (GetRValue(C2) - R1 {+1}) / D;  //store the red increment
    DG:= (GetGValue(C2) - G1 {+1}) / D;  //store the green increment
    DB:= (GetBValue(C2) - B1 {+1}) / D;  //store the blue increment
  end;
begin
  With Canvas do begin
    Lock;   //suspend painting of the canvas
    try
      Brush.Style := bsSolid;  //set the brush style to paint solid strokes

      if StartColor <> EndColor then begin  //colors differ
        C1 := StartColor;//ColorToRgb(FCaptionStartColor);  //get RGB value of Start Color
        C2 := EndColor;//ColorToRgb(FCaptionEndColor);  //get RGB value of End Color

        InitRGBValues(C1, C2);  //get the initial values for the variables

//          If HorizontalGrabber then  //caption bar is horizontal
          DH := (ARect.Right - ARect.Left) / D;  //get width of each small rect
        for X := 0 to 255 do begin  //paint 256 small rects
          Brush.Color := RGB(R1 + Round(DR*X), G1 + Round(DG*X),
                             B1 + Round(DB*X));  //get brush color for this rect
          With ARect do
          begin
//              If HorizontalGrabber then begin  //caption bar is horizontal
              //add five to the width of each to prevent rounding problems
              If Right <= Left + Round((X+1)*DH){ + 5} then  //at the right edge
                W := Right  //set the width to the right edge--won't over-paint
              else  //not at the right edge
                W := Left + Round((X+1)*DH) {+ 5};  //set normal width

              FillRect(Rect(Left + Round(X*DH), Top, W, Bottom))  //paint rect
          end;
        end;
      end
      else begin  //the start and end colors are identical--just paint normally
        Brush.Color := StartColor;  //set the brush's color
        FillRect(ARect);  //paint the rect
      end;
    finally
      Unlock;  //resume painting of the canvas
    end;
  end;
end;

procedure AssignList(FromList, ToList: TList);
var i: Integer;
begin
  ToList.Clear;
  for i := 0 to FromList.Count - 1 do
    ToList.Add(FromList[i]);
end;

{计算停靠的区域}
function ComputeVIDDockingRect(Target, Control: TControl; var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect,
  DockLeftRect,
  DockBottomRect,
  DockRightRect,
  DockCenterRect,
  DockTabRect: TRect;
begin
  Result := alNone;
  //划分停靠区域
  if Target = nil then Exit;
  with Target do
  begin
    DockLeftRect.TopLeft := Point(0, 0);
    DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

    DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
    DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

    DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
    DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

    if Target is TCnDockCustomTabControl then
    begin
      DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientWidth div 5 * 4);
      DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight  -Cn_GetSysCaptionHeight);
    end else
    begin
      DockBottomRect.TopLeft := Point(0, ClientHeight div 5 * 4);
      DockBottomRect.BottomRight := Point(ClientWidth, ClientHeight);
    end;

    DockCenterRect.TopLeft := Point(0, -Cn_GetSysCaptionHeight);
    DockCenterRect.BottomRight := Point(ClientWidth, 0);

    if Target is TCnDockCustomTabControl then
    begin
      DockTabRect.TopLeft := Point(0, ClientHeight - Cn_GetSysCaptionHeight);
      DockTabRect.BottomRight := Point(ClientWidth, ClientHeight);
    end else DockTabRect := Rect(0, 0, 0, 0);

    //发现鼠标在哪个停靠区域
    if PtInRect(DockCenterRect, MousePos) or
      PtInRect(DockTabRect, MousePos) then
    begin
      Result := alClient;
      DockRect := DockCenterRect;
//      DockRect.TopLeft := Point(0, 0);
      DockRect.BottomRight := Point(ClientWidth, ClientHeight);
    end else
    if PtInRect(DockLeftRect, MousePos) then
    begin
      Result := alLeft;
      DockRect := DockLeftRect;
      DockRect.Right := Min(ClientWidth div 2, Control.ClientWidth);
    end else if PtInRect(DockTopRect, MousePos) then
    begin
      Result := alTop;
      DockRect := DockTopRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Bottom := Min(ClientHeight div 2, Control.ClientHeight);
    end else if PtInRect(DockRightRect, MousePos) then
    begin
      Result := alRight;
      DockRect := DockRightRect;
      DockRect.Left := Max(ClientWidth div 2, ClientWidth - Control.ClientWidth);
    end else if PtInRect(DockBottomRect, MousePos) then
    begin
      Result := alBottom;
      DockRect := DockBottomRect;
      DockRect.Top := Max(ClientHeight div 2, ClientHeight - Control.ClientHeight);
    end;
    if Result = alNone then Exit;

    //DockRect是屏幕坐标
    DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
  end;
end;

{ 当在TCnVIDTabPageControl上进行停靠预览的时候，这个函数用来实现这个功能 }
procedure SetTabControlPreview(VIDSource: TCnVIDDragDockObject;
  TabControl: TCnVIDTabPageControl;
  State: TDragState; DropAlign: TAlign);

var i: Integer;
  Index: Integer;
begin
  if TabControl <> nil then
  begin
    if (DropAlign = alClient) then
    begin
      { 如果停靠位置为alClient时，就进行预览操作 }
      if (TabControl.FTempSheet = nil) then
      begin
        { TabControl.FTempSheet等于nil，就说明TabControl不是处于预览状态，
          就需要根据VIDSource.Control中的停靠客户来创建TabControl的TabSheet }
        for i := VIDSource.SourceDockClientCount - 1 downto 0 do
        begin
          { 一共创建VIDSource.SourceDockClientCount个TabSheet }
          TabControl.FTempSheet := TCnVIDDockTabSheet.Create(TabControl);
          TabControl.FTempSheet.PageControl := TabControl;
          { 只是创建一个空的TabSheet，里面不放任何东西，但是需要改变TabSheet的Caption属性 }
          TabControl.FTempSheet.Caption := TCnTempWinControl(VIDSource.SourceDockClients[i]).Caption;
          Index := TabControl.FTabImageList.AddIcon(TForm(VIDSource.SourceDockClients[i]).Icon);
          if Index <> -1 then
            TabControl.FTempSheet.ImageIndex := Index;
          { FIsSourceDockClient这个属性用来指示这个TabSheet是否是预览界面时使用的，
            看下面的语句，当State = dsDragLeave的时候，就需要释放这些用来预览的TabSheet，
            这时候就是通过TabSheet的FIsSourceDockClient属性来判断的。 }
          TabControl.FTempSheet.FIsSourceDockClient := True;
        end;
        { 设置TabControl的ActivePage和Panel的SelectSheet，
          SelectSheet是当鼠标按下的时候，鼠标位置所处的那个TabSheet }
        TabControl.ActivePage := TabControl.FTempSheet;
        TabControl.Panel.SelectSheet := TabControl.FTempSheet;
        { Panel.FTempPages是用来保存在鼠标按下之前的PageSheet,
          具体的实现部分见TCnTabPanel的FindSheetWithPos函数 }

{$IFDEF COMPILER6_UP}
        TabControl.Panel.FTempPages.Assign(TabControl.PageSheets);
{$ELSE}
        AssignList(TabControl.PageSheets, TabControl.Panel.FTempPages);
{$ENDIF}

        TabControl.ActivePage.Invalidate;
//        SendMessage(TabControl.ParentForm.Handle, WM_NCPAINT, 0, 0);
      end;
    end;

//    if State = dsDragLeave then
//      State := dsDragLeave;
    if ((State = dsDragLeave) or (VIDSource.DropAlign <> alClient)) and (TabControl.FTempSheet <> nil) then
    begin
      { 当State = dsDragLeave的时候，并且TabControl.FTempSheet <> nil(说明有预览操作)，
        就把一些用来预览的TabSheet删除掉 }
      for i := TabControl.PageCount - 1 downto 0 do
      begin
        if TCnVIDDockTabSheet(TabControl.Pages[i]).FIsSourceDockClient then
        begin
          { 首先找到TabControl.Pages[i]在TabControl.Panel.FTempPages中的位置 }
          Index := TabControl.Panel.FTempPages.IndexOf(TabControl.Pages[i]);
          { 然后删除掉这个指针 }
          if Index >= 0 then
          begin
            TabControl.Panel.FTempPages.Delete(Index);
            if TabControl.FTabImageList.Count > Index then
              TabControl.FTabImageList.Delete(Index);
          end;
          { 删除TabControl.Pages[i] }
          TabControl.Pages[i].Free;
        end;
      end;
      { TabControl.FTempSheet赋值为nil，说明已经没有预览操作 }
      TabControl.FTempSheet := nil;
//      for i := 0 to TWinControl(TabControl.ActivePage.Controls[0]).ControlCount - 1 do
//        TWinControl(TabControl.ActivePage.Controls[0]).Controls[i].Invalidate;
    end;
    { 可以改变TabControl.ParentForm的标题，和TabControl.ActivePage的标题一样 }
    TabControl.ParentForm.Caption := TabControl.ActivePage.Caption;
    { 需要重画这个标题 }
    if TabControl.ParentForm.HostDockSite is TCnCustomDockPanel then
      TabControl.ParentForm.HostDockSite.Invalidate;
  end;
end;


{ TCnVIDDockStyle }

procedure TCnVIDDockStyle.FormDockOver(DockClient: TCnDockClient; Source: TCnDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  with DockClient do
  begin
    { 首先得到Accept的值 }
    Accept := EnableDock and EachOtherDock and
      IsDockable(ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign);
    if State = dsDragMove then
    begin
      { 然后计算停靠位置和预览矩形的大小 }
      Source.DropAlign := ComputeVIDDockingRect(ParentForm, Source.Control, ARect, Point(X, Y));
      if Accept and (Source.DropAlign <> alNone) then
      begin
        if Source.DropAlign = alClient then
          { 如果Source.DropAlign = alClient，就要减去矩形的Top一个窗体高度的大小 }
          Inc(ARect.Top, Cn_GetSysCaptionHeightAndBorderWidth + 1);
        { 给Source.DockRect赋值为ARect }
        Source.DockRect := ARect;
      end;
      { 给gi_DockRect赋值，这个gi_DockRect有可能会在别的地方使用到 }
      gi_DockRect := ARect;
    end else if State = dsDragLeave then
      Source.DropAlign := alNone;
    if Source is TCnVIDDragDockObject then
    begin
      TCnVIDDragDockObject(Source).OldState := TCnVIDDragDockObject(Source).CurrState;
      TCnVIDDragDockObject(Source).CurrState := State;
    end;
  end;
end;

procedure TCnVIDDockStyle.FormGetSiteInfo(Source: TCnDragDockObject; DockClient: TCnDockClient;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
const
  DefExpandoRect = 20;
var
  CH_BW: Integer;
  ARect: TRect;
begin
  with DockClient do
  begin
    { 获得CanDock的值 }
    CanDock := IsDockable(ParentForm, Client, Source.DropOnControl, Source.DropAlign);
    if CanDock then
    begin
      {获得停靠控件的矩形区域}
      GetWindowRect(ParentForm.Handle, InfluenceRect);
      if ParentForm.HostDockSite is TCnCustomDockPanel then
        { 需要减去GrabberSize(把手的高度) }
        Dec(InfluenceRect.Top, TCnCustomDockPanel(ParentForm.HostDockSite).CnDockManager.GrabberSize);
      if PtInRect(InfluenceRect, MousePos) then
      begin
        ARect := InfluenceRect;
        InflateRect(ARect, -DefExpandoRect, -DefExpandoRect);
        {获得标题栏的高度和边框的宽度}
        CH_BW := Cn_GetSysCaptionHeightAndBorderWidth;
        Inc(ARect.Top, CH_BW + 1);
        if PtInRect(ARect, MousePos) then
        begin
          { 如果鼠标位置在InfluenceRect中，但是不是在InfluenceRect的边缘，
            就告诉系统不能停靠 }
          InfluenceRect := Rect(0, 0, 0, 0);
          CanDock := False;
        end;
      end;
    end;
  end;
end;

constructor TCnVIDDockStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnDockPanelClass := TCnVIDDockPanel;
  CnDockSplitterClass := TCnVIDDockSplitter;
  CnConjoinPanelClass := TCnVIDConjoinPanel;
  CnTabDockClass := TCnVIDTabPageControl;
  CnDockPanelTreeClass := TCnVIDDockTree;
  CnDockPanelZoneClass := TCnVIDDockZone;
  CnConjoinPanelTreeClass := TCnVIDDockTree;
  CnConjoinPanelZoneClass := TCnVIDDockZone;
  CnConjoinServerOptionClass := TCnVIDConjoinServerOption;
  CnTabServerOptionClass := TCnVIDTabServerOption;
end;

procedure TCnVIDDockStyle.FormDockDrop(DockClient: TCnDockClient;
  Source: TCnDragDockObject; X, Y: Integer);
var
  ARect,DRect: TRect;
  DockType: TAlign;
  Host: TCnDockableForm;
  APanelDock: TWinControl;
  VIDSource: TCnVIDDragDockObject;
  i: Integer;
begin
  if Source is TCnVIDDragDockObject then
  begin
    TCnVIDDragDockObject(Source).CurrState := dsDragEnter;
    TCnVIDDragDockObject(Source).OldState := dsDragEnter;
  end;

  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;
    { 锁住Windows桌面 }
    if not IsLoading then
      Cn_LockWindow(nil);
    try
      with DockClient do
      begin
        // 调用ComputeVIDDockingRect函数知道停靠的类型
        DockType := ComputeVIDDockingRect(DockClient.ParentForm, Source.Control, ARect, Point(X, Y));

        if (ParentForm.HostDockSite is TCnDockPanel) then
        begin
          // 如果停靠服务器是TDockPanel，就停靠在TDockServer的DockPanel上。
          if DockType = alClient then
          begin
            // 如果停靠类型是alClient
            if Source.Control is TCnTabDockHostForm then
            begin
              // 如果停靠客户是TCnTabDockHostForm，
              // 就先把Parent停靠到TCnTabDockHostForm中，
              // 再把TCnTabDockHostForm停靠到TCnDockPanel中。
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TCnTabDockHostForm(Source.Control).PageControl, nil, alClient);
              TCnTabDockHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end else
            begin
              // 否则就创建TCnTabDockHostForm，
              // 把把Parent停靠到TCnTabDockHostForm中，
              // 再把TCnTabDockHostForm停靠到TCnDockPanel中。
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.Visible := True;
              Host.ManualDock(APanelDock, nil, alClient);
            end;
          end
          else
          begin
            // 如果停靠类型不是alClient,
            // 就把停靠窗体停靠到TCnDockPanel.
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
            SetDockSite(TWinControl(Source.Control), False);
          end;
          Exit;
        end;

        // 创建分页的服务窗体
        if DockType = alClient then
        begin
          if Source.Control is TCnTabDockHostForm then
          begin
            // 如果停靠客户是TCnTabDockHostForm，
            // 就先把Parent停靠到TCnTabDockHostForm中，
            // 再把TCnTabDockHostForm停靠到TCnDockPanel中。
            APanelDock := ParentForm.HostDockSite;
            ARect := ParentForm.BoundsRect;
            ParentForm.ManualDock(TCnTabDockHostForm(Source.Control).PageControl, nil, alClient);
            TCnTabDockHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
            Source.Control.BoundsRect := ARect;
            Source.Control.ManualDock(APanelDock, nil, alClient);
            if ParentForm.FormStyle = fsStayOnTop then
              TForm(Source.Control).FormStyle := fsStayOnTop;
            Exit;
          end else
          begin
            if Source is TCnVIDDragDockObject then
            begin
              VIDSource := TCnVIDDragDockObject(Source);
              DoFloatForm(Source.Control);
              FreeAllDockableForm;
              for i := 0 to VIDSource.SourceDockClientCount - 1 do
              begin
                VIDSource.Control := VIDSource.SourceDockClients[i];
                if Host = nil then
                  Host := DockClient.CreateTabHostAndDockControl(DockClient.ParentForm, Source.Control)
                else Source.Control.ManualDock(TCnTabDockHostForm(Host).PageControl, nil, alClient);
              end;
              Host.Visible := True;
  //            CnGlobalDockPresident.DragObject.Control := nil;
            end;
          end;
        end
        else if DockType <> alNone then
        begin
          // 创建平铺的服务窗体
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      { 解锁Windows桌面 }
      if not IsLoading then
        Cn_UnLockWindow;
    end;
  end;
end;

procedure TCnVIDDockStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TCnDockBaseControl);
var ADockClient: TCnDockClient;
begin
  if DockBaseControl is TCnDockClient then
  begin
    ADockClient := TCnDockClient(DockBaseControl);
    if IsCreate then
      ADockClient.DirectDrag := False;
  end;
end;

procedure TCnVIDDockStyle.FormStartDock(DockClient: TCnDockClient;
  var Source: TCnDragDockObject);
begin
  inherited FormStartDock(DockClient, Source);
  { 创建一个TCnVIDDragDockObject返回给系统 }
  Source := TCnVIDDragDockObject.Create(DockClient.ParentForm);
end;

procedure TCnVIDDockStyle.FormGetDockEdge(DockClient: TCnDockClient;
  Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
var ARect: TRect;
begin
  { 获得停靠的位置 }
  DropAlign := ComputeVIDDockingRect(DockClient.ParentForm, Source.Control, ARect, MousePos);
end;

function TCnVIDDockStyle.DockClientWindowProc(DockClient: TCnDockClient;
  var Message: TMessage): Boolean;
begin
  Result := inherited DockClientWindowProc(DockClient, Message);
end;

procedure TCnVIDDockStyle.CreateConjoinServerOption(
  var Option: TCnBasicConjoinServerOption);
begin
  Option := TCnVIDConjoinServerOption.Create(Self);
end;

procedure TCnVIDDockStyle.CreateTabServerOption(
  var Option: TCnBasicTabServerOption);
begin
  Option := TCnVIDTabServerOption.Create(Self);
end;
  
procedure TCnVIDDockStyle.AssignConjoinServerOption(
  APanel: TCnCustomDockPanel);
begin
  inherited AssignConjoinServerOption(APanel);
end;

procedure TCnVIDDockStyle.AssignTabServerOption(APage: TCnTabPageControl);
var TmpPage: TCnVIDTabPageControl;
  TmpOption: TCnVIDTabServerOption;
begin
  inherited AssignTabServerOption(APage);
  if (APage is TCnVIDTabPageControl) and (TabServerOption is TCnVIDTabServerOption) then
  begin
    TmpPage := APage as TCnVIDTabPageControl;
    TmpOption := TabServerOption as TCnVIDTabServerOption;
    TmpPage.ActiveFont.Assign(TmpOption.ActiveFont);
    TmpPage.ActiveSheetColor := TmpOption.ActiveSheetColor;
    TmpPage.InactiveFont.Assign(TmpOption.InactiveFont);
    TmpPage.InactiveSheetColor := TmpOption.InactiveSheetColor;
    TmpPage.HotTrackColor := TmpOption.HotTrackColor;
    TmpPage.ShowTabImages := TmpOption.ShowTabImages;
  end;
end;

procedure TCnVIDDockStyle.ParentFormWindowProc(var Message: TMessage);
begin
  inherited ParentFormWindowProc(Message);
  if (Message.Msg = WM_SETTINGCHANGE) or (Message.Msg = WM_SYSCOLORCHANGE) then
  begin
    ParentForm.Caption := '';
    { 如果是WM_SETTINGCHANGE或者WM_SYSCOLORCHANGE消息，也就是当设置桌面属性的外观后。
      就重新设置ConjoinServerOption的属性 }
    if (ConjoinServerOption is TCnVIDConjoinServerOption) then
    begin
      if TCnVIDConjoinServerOption(ConjoinServerOption).SystemInfo then
        TCnVIDConjoinServerOption(ConjoinServerOption).SetDefaultSystemCaptionInfo;
    end;
  end;
end;

destructor TCnVIDDockStyle.Destroy;
begin
  inherited;

end;

procedure TCnVIDDockStyle.DoSystemInfoChange(Value: Boolean);
begin
  if Assigned(FSystemInfoChange) then
    FSystemInfoChange(Value);
end;

procedure TCnVIDDockStyle.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnVIDDockStyleName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnVIDDockStyleComment;
end;

{ TCnVIDDockPanel }

constructor TCnVIDDockPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCnVIDDockPanel.CreateDockManager: IDockManager;
var Option: TCnVIDConjoinServerOption;
begin
  Result := inherited CreateDockManager;
  if (DockServer <> nil) and (Result <> nil) then
  begin
    Option := TCnVIDConjoinServerOption(DockServer.DockStyle.ConjoinServerOption);
    (Result as ICnDockManager).GrabberSize := Option.GrabbersSize;
  end;
end;

procedure TCnVIDDockPanel.CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer);
//var
//  VIDSource: TCnVIDDragDockObject;
begin
{  if Source is TCnVIDDragDockObject then
  begin
    VIDSource := TCnVIDDragDockObject(Source);
    SetTabControlPreview(VIDSource, VIDSource.FDropTabControl, dsDragLeave, VIDSource.DropAlign);
  end;}
  if Source.Control is TCnDockableForm then
    { 显示DockPanel }
    ShowDockPanel(True, Source.Control);
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    inherited CustomDockDrop(Source, X, Y);
    { 因为是平铺风格，所以要设置ActiveControl为当前Source的Control }
    CnDockManager.ActiveControl := Source.Control;
    { 设置Source.Control为获得焦点，这一步一定需要设置，
      不然程序可能会把Source.Control中的控件的内容清空 }
    if (Source.Control is TWinControl) and TWinControl(Source.Control).CanFocus then
      TWinControl(Source.Control).SetFocus;
  end;
end;

procedure TCnVIDDockPanel.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var DropAlign: TAlign;
//  VIDSource: TCnVIDDragDockObject;
//  DropCtl: TControl;
begin
  { 首先调用父类的CustomDockOver }
  inherited CustomDockOver(Source, X, Y, State, Accept);
  if Accept and (Source is TCnVIDDragDockObject) then
  begin
(*    VIDSource := TCnVIDDragDockObject(Source);
    DropCtl := VIDSource.GetDropCtl;
    if (DropCtl <> VIDSource.DropOnControl) or
      (VIDSource.FOldDropTabControl <> VIDSource.FDropTabControl) then
      SetTabControlPreview(VIDSource, VIDSource.FOldDropTabControl, dsDragLeave, VIDSource.DropAlign);
    { 调用SetTabControlPreview函数用来显示预览界面 }
    SetTabControlPreview(VIDSource, VIDSource.FDropTabControl, State, VIDSource.DropAlign);
    if State = dsDragLeave then
      { 离开的时候要设置FDropTabControl为空 }
      VIDSource.FDropTabControl := nil;
    VIDSource.FOldDropTabControl := VIDSource.FDropTabControl;*)
    if State = dsDragMove then
    begin
      DropAlign := Source.DropAlign;
      { 调用CnDockManager的GetDockEdge来得到停靠的位置 }
      CnDockManager.GetDockEdge(Source.DockRect, Source.DragPos, DropAlign, Source.Control);
    end;// else if (State = dsDragLeave) then
//      if (Source.DropAlign = alClient) and (Source.DropOnControl = nil) then
//      Source.DropAlign := alNone;
  end;
end;

procedure TCnVIDDockPanel.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
//  inherited CustomGetDockEdge(Source, MousePos, DropAlign);
end;

procedure TCnVIDDockPanel.CustomGetSiteInfo(Source: TCnDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  { 如果VisibleDockClientCount = 0，也就是说停靠服务器中的TCnVIDDockPanel还没有显示出来，
    就调用默认的CustomGetSiteInfo，否者就要调用CnDockManager.GetSiteInfo来获得InfluenceRect }
  if VisibleDockClientCount = 0 then
    inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock)
  else
  begin
    CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
    if CanDock then
      CnDockManager.GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
  end;
end;

procedure TCnVIDDockPanel.CustomStartDock(var Source: TCnDragDockObject);
begin
  Source := TCnVIDDragDockObject.Create(Self);
end;

procedure TCnVIDDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  inherited;

end;

{ TCnVIDDockTree }

constructor TCnVIDDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
begin
  inherited Create(DockSite, CnDockZoneClass);
  FDropOnZone := nil;
  GrabberSize     := 18;
  ButtonHeight    := 11;
  ButtonWidth     := 13;
  LeftOffset      := 2;
  RightOffset     := 2;
  TopOffset       := 4;
  BottomOffset    := 3;
  ButtonSplitter  := 2;
  BorderWidth     := 0;
  MinSize         := 20;  //节点的最小值为20
  CaptionLeftOffset := 0;
  CaptionRightOffset := 0;
end;

destructor TCnVIDDockTree.Destroy;
begin
  inherited Destroy;
end;

function TCnVIDDockTree.GetGrabbersPosition: TGrabbersPosition;
begin
  { 在这里默认的把手位置都是gpTop }
  Result := gpTop;
end;

function TCnVIDDockTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + GrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left + Width - ButtonWidth - RightOffset,
        Top - GrabberSize + TopOffset,
        Left + Width - RightOffset,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else HTFlag := HTCAPTION;
    end;
  end else Result := nil;
end;

procedure TCnVIDDockTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);

  { 根据Client，DropCtl和InsertAt来创建一个TCnTabDockHostForm窗体，
    并且把Client和DropCtl停靠到它的PageControl中 }
  function CreateDockPageControl(Client: TControl): TCnTabDockHostForm;
  var
    Zone: TCnDockZone;          //根据DropCtl查找到的节点
    TempCtl: TControl;          //用来存储原来的DropCtl，因为DropCtl有可能会被改变
    TempPanel: TCnConjoinPanel; //存储TempCtl的HostDockSite
    DockClient: TCnDockClient;  //在DropCtl中的DockClient
    APoint: TPoint;
  begin
    Result := nil;
    Zone := FindControlZone(DropCtl);
    DockClient := FindDockClient(DropCtl);
    if (DockClient <> nil) and (Zone <> nil) then
    begin
      TempCtl := DropCtl;
      { 下面的语句用来给DropCtl和InsertAt赋值 }
      if Zone.ParentZone.Orientation = doHorizontal then
      begin
        if (Zone.PrevSibling = nil) then
        begin
          if Zone.NextSibling <> nil then
            DropCtl := Zone.NextSibling.ChildControl;
          InsertAt := alTop;
        end else 
        begin
          DropCtl := Zone.PrevSibling.ChildControl;
          InsertAt := alBottom;
        end;
      end else if Zone.ParentZone.Orientation = doVertical then
      begin
        if (Zone.PrevSibling = nil) then
        begin
          if Zone.NextSibling <> nil then
            DropCtl := Zone.NextSibling.ChildControl;
          InsertAt := alLeft;
        end else //if Zone.NextSibling = nil then
        begin
          DropCtl := Zone.PrevSibling.ChildControl;
          InsertAt := alRight;
        end;
      end;

      { 考虑到在调用完DockClient的CreateTabHostAndDockControl函数后TempCtl.HostDockSite可能会改变
        所以先把它保存下来，供以后使用 }

      if TempCtl.HostDockSite is TCnConjoinPanel then
        TempPanel := TCnConjoinPanel(TempCtl.HostDockSite)
      else TempPanel := nil;

      { 调用DockClient的CreateTabHostAndDockControl函数，根据TempCtl和Client创建TCnTabDockHostForm }
      Result := DockClient.CreateTabHostAndDockControl(TempCtl, Client);
      if TempPanel <> nil then
        { 这条语句是用在TCnDockableForm的DoClose中的，用来判断UnDockControl是否是将要又被停靠进TempPanel，
          如果是的话，就不把TempPanel.ParentForm释放掉，否者就有可能被释放掉，具体代码请参看TCnDockableForm的DoClose函数 }
        TempPanel.ParentForm.UnDockControl := Result;

      { 设置TempCtl和Control的DockSite属性为False }
      SetDockSite(TWinControl(TempCtl), False);
      SetDockSite(TWinControl(Client), False);

      { 重新设置Result的位置 }
      if DockSite.Align = alBottom then
        APoint := Point(0, -TempCtl.TBDockHeight)
      else if DockSite.Align = alRight then
        APoint := Point(-TempCtl.LRDockWidth, 0)
      else APoint := Point(0, 0);
      APoint :=  DockSite.ClientToScreen(APoint);
      Result.Left := APoint.x;
      Result.Top := APoint.y;
      Result.UndockWidth := TempCtl.UndockWidth;
      Result.UndockHeight := TempCtl.UndockHeight;
      Result.LRDockWidth := TempCtl.LRDockWidth;
      Result.TBDockHeight := TempCtl.TBDockHeight + GrabberSize;

      { 显示TCnTabDockHostForm }
      Result.Visible := True;
    end;
  end;

var i: Integer;
  Host: TCnTabDockHostForm;
  ChildCount: Integer;
  VIDSource: TCnVIDDragDockObject;
  TempControl: TControl;
  ARect: TRect;
  AZone: TCnDockZone;
begin
  { 锁住Windows桌面 }
  if not IsLoading then
    Cn_LockWindow(nil);
  try
    VIDSource := nil;
    if (Control is TCnDockableForm){ and (DockSite is TCnConjoinPanel) }then
    begin
      { 如果将要停靠进来的Control是TCnDockableForm，也就是说Control是一个停靠服务器 }
      if InsertAt in [alClient] then
      begin
        { 如果停靠类型是alClient，就说明要把Control和DropCtl停靠到一个TCnVIDTabPageControl中去，
          然后把TCnVIDTabPageControl的ParentForm--也就是TCnTabDockHostForm停靠到DockSite中 }
        if DropCtl is TCnTabDockHostForm then
        begin
//          IsLoading := True;
          { 如果DropCtl本身就是一个TCnTabDockHostForm，那只要把Control中的所有停靠客户依次停靠到DropCtl的PageControl中去 }
          try
            VIDSource := TCnVIDDragDockObject.Create(Control);
            DoFloatForm(Control);
            FreeAllDockableForm;
            for i := VIDSource.SourceDockClientCount - 1 downto 0 do
            begin
              TempControl := VIDSource.SourceDockClients[i];
              TempControl.ManualDock(TCnTabDockHostForm(DropCtl).PageControl);
              if TempControl is TForm then
              begin
                TForm(TempControl).ActiveControl := nil;
                SetDockSite(TForm(TempControl), False);
              end;
            end;
          finally
//            IsLoading := False;
//            ReshowAllVisibleWindow;
            VIDSource.Free;
            CnGlobalDockPresident.DragObject.Control := nil;
          end;
        end else
        begin
          if (DockSite is TCnCustomDockPanel) and (DockSite.VisibleDockClientCount > 1) and (DropCtl <> nil) then
          begin
//            IsLoading := True;
            try
              VIDSource := TCnVIDDragDockObject.Create(Control);
              DoFloatForm(Control);
              FreeAllDockableForm;
              { 否者，就首先创建一个TCnDockableForm用来作为Control的DockClients[0]和DropCtl的服务器Host }
              Host := CreateDockPageControl(VIDSource.SourceDockClients[0]);//TCnDockableForm(Control).DockableControl.DockClients[0]);
              if Host <> nil then
              begin
                { 然后再把Control中的所有停靠客户依次停靠到DropCtl的PageControl中去 }
                for i := VIDSource.SourceDockClientCount - 1 downto 1 do
                begin
                  TempControl := VIDSource.SourceDockClients[i];
                  TempControl.ManualDock(Host.PageControl);
                  if TempControl is TForm then
                  begin
                    TForm(TempControl).ActiveControl := nil;
                    SetDockSite(TForm(TempControl), False);
                  end;
                end;
                { 最后把Host停靠到DockSite中，注意：这时候DropCtl和InsertAt已经改变了 }
                Host.ManualDock(DockSite, nil, InsertAt);
              end;
            finally
//              IsLoading := False;
//              ReshowAllVisibleWindow;
              VIDSource.Free;
              CnGlobalDockPresident.DragObject.Control := nil;
            end;
          end else
            { 否者调用原来的虚拟函数 }
            inherited InsertControl(Control, InsertAt, DropCtl);
        end;
      end else if Control is TCnConjoinDockHostForm then
      begin
        { 否则如果Control是一个平铺服务器，就要进行特殊的处理，
          调用InsertControlFromConjoinHost函数，把Control中的停靠信息还原到DockSite中 }
        TCnTempWinControl(TCnDockableForm(Control).DockableControl).DockManager.ResetBounds(True);
        InsertControlFromConjoinHost(Control, InsertAt, DropCtl);
      end else
        { 否者调用原来的虚拟函数 }
        inherited InsertControl(Control, InsertAt, DropCtl);
    end else
    begin
      { 否者，也就是说Control是一个普通的窗体 }
      if InsertAt in [alLeft, alTop] then
        { 对DockSize进行必要的调整 }
        DropDockSize := DropDockSize + SplitterWidth div 2;
      if InsertAt in [alClient] then
      begin
        { 如果停靠类型是alClient，就说明要把Control和DropCtl停靠到一个TCnVIDTabPageControl中去，
          然后把TCnVIDTabPageControl的ParentForm--也就是TCnTabDockHostForm停靠到DockSite中 }
        if DropCtl is TCnTabDockHostForm then
        begin
          { 如果DropCtl本身就是一个TCnTabDockHostForm，那只要简单的把Control停靠到DropCtl的PageControl中去 }
          Control.ManualDock(TCnTabDockHostForm(DropCtl).PageControl, nil, alClient);
        end
        else if TopZone.ChildZones <> nil then
        begin
          { 否者如果DockSite本身有停靠客户 }
          ChildCount := TopZone.ChildCount;
          if DropCtl <> nil then
          begin
            ARect := DropCtl.BoundsRect;
            AZone := FindControlZone(DropCtl);
            // 当这个节点紧靠DockSite的右边或者下边的时候，不用给它加偏移量SplitterWidth
            if DropCtl.DockOrientation = doHorizontal then
            begin
              if ((AZone <> nil) and (AZone.ZoneLimit <> DockSite.Height)) then
                ARect.Bottom := ARect.Bottom + SplitterWidth;
            end else
            begin
              if ((AZone <> nil) and (AZone.ZoneLimit <> DockSite.Width)) then
                ARect.Right := ARect.Right + SplitterWidth;
            end;
            DockRect := ARect;
          end
          else DockRect := Rect(0, 0, TopZone.Width, TopZone.Height);
          { 创建一个叫做Host的窗体作为Control和DropCtl的服务器 }
          Host := CreateDockPageControl(Control);
          if Host <> nil then
          begin
            if (ChildCount >= 2) or (DockSite is TCnDockPanel) then
            begin
              { 如果DockSite的客户大于一个，就把Host停靠到DockSite中 }
              if InsertAt in [alLeft, alRight] then
                DropDockSize := DockRect.Right - DockRect.Left
              else DropDockSize := DockRect.Bottom - DockRect.Top + GrabberSize;
              // 锁住DropDockSize
              LockDropDockSize;
              Host.ManualDock(DockSite, DropCtl, InsertAt);
              // 解锁DropDockSize
              UnlockDropDockSize;

            end else
            begin
              { 否者只是简单的设置Host的位置大小 }
              Host.BoundsRect := DockSite.Parent.BoundsRect;
            end;
          end;
        end
        else inherited InsertControl(Control, InsertAt, DropCtl);
      end else
      { 否者调用原来的虚拟函数 }
      inherited InsertControl(Control, InsertAt, DropCtl);
      { 这条语句是用来给DockRect赋值，这是因为有可能InsertControl会调用好几次,
        下次调用InsertControl的时候可能会使用到DockRect }
      DockRect := gi_DockRect;
    end;
    ForEachAt(nil, UpdateZone);
  finally
    { 解锁Windows桌面 }
    if not IsLoading then
      Cn_UnLockWindow;
  end;
end;

procedure TCnVIDDockTree.InsertControlFromConjoinHost(Control: TControl;
  InsertAt: TAlign; DropCtl: TControl);
const
{ Delphi6.0 }
{$IFDEF COMPILER6_UP}
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient, doNoOrient); { alCustom }
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False, False);  { alCustom }
  ReverseAt: array[TAlign] of TAlign = (alClient, alBottom, alTop, alRight, alLeft, alNone, alCustom); { alCustom }
{$ELSE}
{ Delphi5.0 OR LAST}
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient);
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False);
  ReverseAt: array[TAlign] of TAlign = (alClient, alBottom, alTop, alRight, alLeft, alNone);
{$ENDIF}

var
  Stream: TMemoryStream;

  TopOrientation,                       //Control中的TopZone的Orientation属性
  InsertOrientation,                    //要插入的方向
  CurrentOrientation: TDockOrientation; //当前的方向
  ZoneLimit: Integer;

  Level, LastLevel, I: Integer;
  Zone, NextZone: TCnDockZone;
  DropCtlZone, LastZone: TCnDockZone;

  OffsetXYLimitArr: array[TDockOrientation] of Integer;  //偏移量
  ControlXYLimitArr: array[TDockOrientation] of Integer; //控件的宽度和高度

  { 读出控件的名称 }
{  procedure ReadControlName(var ControlName: string);
  var
    Size: Integer;
  begin
    ControlName := '';
    Stream.Read(Size, SizeOf(Size));
    if Size > 0 then
    begin
      SetLength(ControlName, Size);
      Stream.Read(Pointer(ControlName)^, Size);
    end;
  end;}

  { 读出Zone并且按要求设置它在DockTree中的位置 }
  procedure ReadZone(SetZone: Boolean);
  var I: Integer;
  begin
    { 读Control中DockTree的Zone }
    with Stream do
    begin
      { 读出Level值，这个值代表了Zone的层次，如果是0就是TopZone，
      如果是1就是TopZone的子女，依次类推 }
      Read(Level, SizeOf(Level));
      if Level = TreeStreamEndFlag then Exit;
      { 创建Zone }
      Zone := CnDockZoneClass.Create(Self);
      { 读出Zone的Orientation属性 }
      CustomLoadZone(Stream, Zone);
      { 读出Zone的ZoneLimit属性，这个值不能直接赋值给Zone，还需要加上偏移量 }
      ZoneLimit := Zone.ZoneLimit;
    end;
    if SetZone then
    begin
      { 如果SetZone等于True，也就是说要设置Zone在DockTree中的关系 }
      if Level = LastLevel then
      begin
        { 如果Level和LastLevel相等，也就是说Zone和LastZone是兄弟关系，就把Zone
        加为LastZone的兄弟。}
        Zone.NextSibling := LastZone.NextSibling;
        if LastZone.NextSibling <> nil then
          LastZone.NextSibling.PrevSibling := Zone;
        LastZone.NextSibling := Zone;
        Zone.PrevSibling := LastZone;
        Zone.ParentZone := LastZone.ParentZone;
      end
      else if Level > LastLevel then
      begin
        { 如果Level大于LastLevel，也就是说Zone是LastZone的子女，就把Zone
        加为LastZone的子女。}
        LastZone.ChildZones := Zone;
        Zone.ParentZone := LastZone;
        InsertOrientation := LastZone.Orientation;
      end
      else if Level < LastLevel then
      begin
        { 如果Level小于LastLevel，就找到和Zone等级相等的NextZone，并且把Zone
        加为NextZone的兄弟。}
        NextZone := LastZone;
        for I := 1 to LastLevel - Level do
          NextZone := NextZone.ParentZone;
        Zone.NextSibling := NextZone.NextSibling;
        if NextZone.NextSibling <> nil then
          NextZone.NextSibling.PrevSibling := Zone;
        NextZone.NextSibling := Zone;
        Zone.PrevSibling := NextZone;
        Zone.ParentZone := NextZone.ParentZone;
        InsertOrientation := Zone.ParentZone.Orientation;
      end;
      { Zone的ZoneLimit要加上偏移量 }
      Zone.ZoneLimit := OffsetXYLimitArr[InsertOrientation] + ZoneLimit;
    end;
    { 把当前的值赋值给Last，进行下一次循环 }
    LastLevel := Level;
    LastZone := Zone;
  end;

begin
  { 控件的长宽 }
  ControlXYLimitArr[doNoOrient] := 0;
  ControlXYLimitArr[doHorizontal] := DockRect.Bottom - DockRect.Top ;//}Control.Height;// - GrabberSize;
  ControlXYLimitArr[doVertical] := DockRect.Right - DockRect.Left;//}Control.Width;// - BorderWidth;

  { 创建并且存储停靠信息到流中 }
  Stream := TMemoryStream.Create;
  if Control is TCnConjoinDockHostForm then
    TCnConjoinDockHostForm(Control).Panel.CnDockManager.SaveToStream(Stream);
  Stream.Position := 0;
  { 开始更新 }
  BeginUpdate;
  try
    { 读版本 }
    Stream.Read(I, SizeOf(I));
    { TopZone中的TopXYLimit(四字节)和Level(四字节)没有用处，被忽略掉 }
    Stream.Position := Stream.Position + 8;

    { 读出停靠窗体的停靠方向 }
    Stream.Read(TopOrientation, SizeOf(TopOrientation));
    { 读出停靠窗体的Limit }
    Stream.Read(ZoneLimit, SizeOf(ZoneLimit));
    
    IgnoreZoneInfor(Stream);

    if (DropCtl = nil) and (TopZone.ChildCount = 1) then
      { 如果TopZone只有一个子女，就把这个子女赋值给DropCtl }
      DropCtl := TopZone.ChildZones.ChildControl;
    { 查找到DropCtl属于哪一个Zone }
    DropCtlZone := FindControlZone(DropCtl);
    { 默认的插入方向是右边 }
    if InsertAt in [alClient, alNone] then InsertAt := alRight;
    InsertOrientation := OrientArray[InsertAt];

    if TopZone.ChildCount = 0 then
    begin
      { DockSite中还没有控件(可视的)，插入方向和Top的方向就设置成和Control中的TopZone一样 }
      TopZone.Orientation := TopOrientation;
      InsertOrientation := TopOrientation;
    end
    else if TopZone.ChildCount = 1 then
    begin
      // 如果树只有一个子女，并且第二个正在被添加进去，
      // 所以方向和位置必须被设置
      TopZone.Orientation := InsertOrientation;
      case InsertOrientation of
        doHorizontal:
          begin
            TopZone.ZoneLimit := TopZone.ChildZones.Width;
            TopXYLimit := TopZone.ChildZones.Height;
          end;
        doVertical:
          begin
            TopZone.ZoneLimit := TopZone.ChildZones.Height;
            TopXYLimit := TopZone.ChildZones.Width;
          end;
      end;
    end;

    { 设置当前的DropCtlZone的方向 }
    if DropCtlZone <> nil then
      CurrentOrientation := DropCtlZone.ParentZone.Orientation
    else
      CurrentOrientation := TopZone.Orientation;

    { 设置DockSize的大小 }
    if InsertOrientation = doHorizontal then
      DropDockSize := DockRect.Bottom - DockRect.Top
    else if InsertOrientation = doVertical then
      DropDockSize := DockRect.Right - DockRect.Left
    else DropDockSize := 0;

    { 设置偏移量 }
    OffsetXYLimitArr[doNoOrient] := 0;
    if DropCtlZone <> nil then
    begin
      { 首先计算水平偏移量 }
      OffsetXYLimitArr[doHorizontal] := DropCtlZone.TopLeft[doHorizontal] +
        Integer(MakeLast[InsertAt]) * (DropCtlZone.HeightWidth[doHorizontal] - ControlXYLimitArr[doHorizontal]);
      { 如果停靠操作是在水平分割条附近，就重新计算水平偏移量 }
      if (FDropOnZone <> nil) and (InsertOrientation = doHorizontal) then
        OffsetXYLimitArr[doHorizontal] := FDropOnZone.ZoneLimit - Round((FDropOnZone.ZoneLimit -
          FDropOnZone.ParentZone.ChildZones.LimitBegin) * (DropDockSize + BorderWidth) / (FDropOnZone.ParentZone.Height));

      { 然后计算垂直偏移量 }
      OffsetXYLimitArr[doVertical] := DropCtlZone.TopLeft[doVertical] +
        Integer(MakeLast[InsertAt]) * (DropCtlZone.HeightWidth[doVertical] - ControlXYLimitArr[doVertical]);
      { 如果停靠操作是在垂直分割条附近，就重新计算垂直偏移量 }
      if (FDropOnZone <> nil) and (InsertOrientation = doVertical) then
        OffsetXYLimitArr[doVertical] := FDropOnZone.ZoneLimit - Round((FDropOnZone.ZoneLimit -
          FDropOnZone.ParentZone.ChildZones.LimitBegin) * (DropDockSize + BorderWidth) / (FDropOnZone.ParentZone.Width));

    end else
    begin
      { 这一项可能没有用处，因为DropCtlZone在这里一直为nil }
      if TopZone.VisibleChildCount = 0 then
//      if TopZone.ChildZones = nil then
      begin
        OffsetXYLimitArr[doHorizontal] := 0;
        OffsetXYLimitArr[doVertical]   := 0;
      end else
      begin
        OffsetXYLimitArr[doHorizontal] := Integer(MakeLast[InsertAt]) * ControlXYLimitArr[doHorizontal];
//        InsertAt := ReverseAt[InsertAt];
        OffsetXYLimitArr[doVertical] := Integer(MakeLast[InsertAt]) * ControlXYLimitArr[doVertical];
      end;
    end;

    if TopOrientation <> InsertOrientation then
    begin
      { TopOrientation和InsertOrientation的方向不同，
      就要先创建一个LastZone作为Control中的Zone父亲 }
      LastZone := CnDockZoneClass.Create(Self);
      if InsertOrientation <> CurrentOrientation then
        { InsertOrientation和CurrentOrientation的方向不同，
        就要创建一个Zone作为LastZone和DropCtlZone的父亲 }
        InsertNewParent(LastZone, DropCtlZone, InsertOrientation, MakeLast[InsertAt], True)
      else
        { 否者就只是简单的使LastZone和DropCtlZone成为兄弟关系 }
        InsertSibling(LastZone, DropCtlZone, MakeLast[InsertAt], True);
      { 调整LastZone的ZoneLimit属性 }
//      LastZone.ZoneLimit := OffsetXYLimitArr[InsertOrientation] + ControlXYLimitArr[InsertOrientation];
      { LastZone的Orientation属性和Control中的TopZone的方向相等 }
      LastZone.Orientation := TopOrientation;
      { LastZone是在根节点 }
      LastLevel := 0;
    end else
    begin
      LastLevel := 1;
      if TopZone.ChildCount > 0 then
      begin
        ReadZone(False);
        if InsertOrientation <> CurrentOrientation then
          InsertNewParent(LastZone, DropCtlZone, InsertOrientation, MakeLast[InsertAt], True)
        else InsertSibling(LastZone, DropCtlZone, MakeLast[InsertAt], True);
        LastZone.ZoneLimit := ZoneLimit + OffsetXYLimitArr[InsertOrientation];
      end else
      begin
        LastLevel := 0;
        LastZone := TopZone;
      end;
      { 设置DropCtlZone的ZoneLimit属性 }
{      if DropCtlZone <> nil then
      begin
        if TopZone.ChildCount = 1 then
          DropCtlZone.ZoneLimit := TopXYLimit - OffsetXYLimitArr[InsertOrientation]
        else
        begin
          if InsertAt in [alRight, alBottom] then
            DropCtlZone.ZoneLimit := OffsetXYLimitArr[InsertOrientation]
          else DropCtlZone.ZoneLimit := DropCtlZone.LimitBegin +
            ControlXYLimitArr[InsertOrientation];
        end;
      end;}
    end;

    { 重新调整OffsetXYLimitArr数组的值 }
    OffsetXYLimitArr[doHorizontal] := LastZone.TopLeft[doHorizontal];
    OffsetXYLimitArr[doVertical] := LastZone.TopLeft[doVertical];

//    Stream.Position := 0;

    { 读数据 }
    while True do
    begin
      ReadZone(True);
      { 如果已经到达了Stream的结尾，就退出这次循环 }
      if Level = TreeStreamEndFlag then
        break;
    end;
  finally
    Stream.Free;
    EndUpdate;
  end;
  { 进行一些必要的设置 }
  SetNewBounds(nil);
end;

procedure TCnVIDDockTree.DrawDockGrabber(Control: TControl; const ARect: TRect);

var Option: TCnVIDConjoinServerOption;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Right, Top);
      LineTo(Left, Top);
      LineTo(Left, Bottom);
      Pen.Color := clBtnShadow;
      LineTo(Right, Bottom);
      LineTo(Right, Top-1);
    end;
  end;

var DrawRect: TRect;
  uFormat: UINT;
  ActiveControl: TControl;
const
  TextAlignment: array[TAlignment] of UINT = (DT_LEFT, DT_RIGHT, DT_CENTER);
begin
  with ARect do
    if GrabbersPosition = gpLeft then
    begin
    end
    else if GrabbersPosition = gpTop then
    begin
      if DockSite is TCnDockPanel then
        Option := TCnVIDConjoinServerOption(TCnDockPanel(DockSite).DockServer.DockStyle.ConjoinServerOption)
      else if DockSite is TCnConjoinPanel then
        Option := TCnVIDConjoinServerOption(TCnConjoinDockHostForm(TCnConjoinPanel(DockSite).ParentForm).DockClient.DockStyle.ConjoinServerOption)
      else
        Option := nil;
      { 首先调用PaintGradientBackground函数来画标题栏的背景色 }
      ActiveControl := GetActiveControl;
      DrawRect := ARect;
      Inc(DrawRect.Top, 2);
      DrawRect.Bottom := DrawRect.Top + GrabberSize - 3;
      if Option <> nil then
      begin
        if ActiveControl = Control then
          PaintGradientBackground(Canvas, DrawRect, Option.ActiveTitleStartColor, Option.ActiveTitleEndColor)
        else
          PaintGradientBackground(Canvas, DrawRect, Option.InactiveTitleStartColor, Option.InactiveTitleEndColor);
      end;
      PaintDockGrabberRect(Canvas, Control, DrawRect);
      { 设置Canvas的字体和画刷的属性 }
      if ActiveControl = Control then
        Canvas.Font.Assign(Option.ActiveFont)
      else Canvas.Font.Assign(Option.InactiveFont);
      Canvas.Brush.Style := bsClear;
      DrawRect := ARect;
      GetCaptionRect(DrawRect);
      uFormat := DT_SINGLELINE or (UINT(Option.TextEllipsis) * DT_END_ELLIPSIS) or TextAlignment[Option.TextAlignment];
      DrawText(Canvas.Handle, PChar(TForm(Control).Caption), -1, DrawRect, uFormat);
      DrawCloseButton(Canvas, FindControlZone(Control), Right-RightOffset-ButtonWidth, Top+TopOffset);
    end
    else if GrabbersPosition = gpBottom then
    begin

    end
    else if GrabbersPosition = gpRight then
    begin

    end;
end;

procedure TCnVIDDockTree.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  { 当用户改变DockSite的大小的时候，程序会自动调用这个函数 }
  if not (csLoading in DockSite.ComponentState) and
    (TopZone <> nil) and (DockSite.DockClientCount > 0) then
  begin
    R := DockSite.ClientRect;
    if DockSite is TCnConjoinPanel then
    begin
      { 最好不要使R.Right = R.Left，R.Bottom = R.Top }
      if (R.Right = R.Left) then
        Inc(R.Right, DockSite.Parent.UndockWidth);
      if R.Bottom = R.Top then
        Inc(R.Bottom, DockSite.Parent.UndockHeight);
    end;
    if Force or (not CompareMem(@R, @OldRect, SizeOf(TRect))) then
    begin
      case TopZone.Orientation of
        doHorizontal:
          begin
            if R.Right - R.Left > 0 then
              TopZone.ZoneLimit := R.Right - R.Left;
            if R.Bottom - R.Top > 0 then
              TopXYLimit := R.Bottom - R.Top;
          end;
        doVertical:
          begin
            if R.Bottom - R.Top > 0 then
              TopZone.ZoneLimit := R.Bottom - R.Top;
            if R.Right - R.Left > 0 then
              TopXYLimit := R.Right - R.Left;
          end;
      end;
      if DockSite.DockClientCount > 0 then
      begin
        { 首先确保这个函数不是在装载停靠信息的时候调用的 }
        if not IsLoading then
        begin
          { 然后计算水平方向的缩放比例 }
          if (R.Bottom - R.Top > 0 ) and (OldRect.Bottom - OldRect.Top > 0) then
            ScaleBy := (R.Bottom - R.Top) / (OldRect.Bottom - OldRect.Top)
          else ScaleBy := 1;

          ShiftScaleOrient := doHorizontal;
                             
          { 调用ForEachAt对整个树调整 }
          if (UpdateCount = 0) and (ScaleBy <> 1) then
            ForEachAt(nil, ScaleZone, tskForward);

          { 计算垂直方向的缩放比例 }
          if (R.Right - R.Left > 0) and (OldRect.Right - OldRect.Left > 0) then
            ScaleBy := (R.Right - R.Left) / (OldRect.Right - OldRect.Left)
          else ScaleBy := 1;

          ShiftScaleOrient := doVertical;

          { 调用ForEachAt对整个树调整 }
          if (UpdateCount = 0) and (ScaleBy <> 1) then
            ForEachAt(nil, ScaleZone, tskForward);
        end;

        SetNewBounds(nil);
        if UpdateCount = 0 then ForEachAt(nil, UpdateZone, tskForward);
        { OldRect在下一次调用的时候会用到 }
        OldRect := R;
      end;
    end;
  end;
end;

procedure TCnVIDDockTree.DrawSplitterRect(const ARect: TRect);
begin
  inherited;
  { 不做什么事情 }
end;

procedure TCnVIDDockTree.SetActiveControl(const Value: TControl);
begin
  if GetActiveControl <> Value then
  begin
    inherited SetActiveControl(Value);
    DockSite.Invalidate;
  end;
end;

procedure TCnVIDDockTree.WindowProc(var Message: TMessage);
var AAlign: TAlign;
begin
  if Message.Msg = CM_DOCKCLIENT then
  begin
    { 用来获得DockSize的大小 }
    AAlign := TCMDockClient(Message).DockSource.DropAlign;
    TCMDockClient(Message).DockSource.DockRect := gi_DockRect;
    GetDockEdge(gi_DockRect, TCMDockClient(Message).DockSource.DragPos, AAlign, TCMDockClient(Message).DockSource.Control);
  end;
  inherited WindowProc(Message);
end;

procedure TCnVIDDockTree.SplitterMouseUp;
var OldLimit: Integer;
  Zone: TCnDockZone;
begin
  Mouse.Capture := 0;
  DrawSizeSplitter;
  ReleaseDC(SizingWnd, SizingDC);

  OldLimit := SizingZone.ZoneLimit;
  { 只有是和SizingZone.ParentZone.Orientation相同的Orientation才能够在遍历中执行 }
  ShiftScaleOrient := SizingZone.ParentZone.Orientation;
  if SizingZone.ParentZone.Orientation = doHorizontal then
    SizingZone.ZoneLimit := SizePos.y + (SplitterWidth div 2)
  else
    SizingZone.ZoneLimit := SizePos.x + (SplitterWidth div 2);

  ParentLimit := SizingZone.LimitBegin;
  if OldLimit - ParentLimit > 0 then
    ScaleBy := (SizingZone.ZoneLimit - ParentLimit) / (OldLimit - ParentLimit)
  else ScaleBy := 1;
  { 调整当前的Zone的子女的ZoneLimit }
  if SizingZone.ChildZones <> nil then
    ForEachAt(SizingZone.ChildZones, ScaleChildZone, tskForward);

  Zone := SizingZone;
  while (Zone.NextSibling <> nil) and (not Zone.NextSibling.Visibled) do
  begin
    Zone.NextSibling.ZoneLimit := SizingZone.ZoneLimit;
    Zone := Zone.NextSibling;
  end;

  if SizingZone.NextSibling <> nil then
  begin
    { 如果当前的Zone有后一个兄弟，
    就用当前的Zone的后一个兄弟的ZoneLimit减去当前的Zone的ZoneLimit }
    if SizingZone.NextSibling.ZoneLimit - OldLimit > 0 then
      ScaleBy := (SizingZone.NextSibling.ZoneLimit - SizingZone.ZoneLimit) / (SizingZone.NextSibling.ZoneLimit - OldLimit)
    else ScaleBy := 1;
    ParentLimit := SizingZone.NextSibling.ZoneLimit;
    { 调整当前的Zone的下一个兄弟的子女的ZoneLimit }
    if SizingZone.NextSibling.ChildZones <> nil then
      ForEachAt(SizingZone.NextSibling.ChildZones, ScaleSiblingZone, tskForward);
  end;

  SetNewBounds(SizingZone.ParentZone);
  ForEachAt(SizingZone.ParentZone, UpdateZone, tskForward);
  SizingZone := nil;

end;

procedure TCnVIDDockTree.DrawDockSiteRect;
begin
//  inherited;

end;

procedure TCnVIDDockTree.InsertSibling(NewZone, SiblingZone: TCnDockZone;
  InsertLast, Update: Boolean);
begin
  if (FDropOnZone <> nil) then
    SiblingZone := FDropOnZone;
  inherited;
end;

procedure TCnVIDDockTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);

var
  VisibleClients,
  NewX, NewY, NewWidth, NewHeight: Integer;
  Zone: TCnDockZone;
  HTFlag: Integer;
  MousePos: TPoint;
  Scale: Double;
  CtrlRect: TRect;

  procedure DockOverSplitter;
  begin
    NewX := Zone.ParentZone.Left;
    NewY := Zone.ParentZone.Top;
    NewWidth := Zone.ParentZone.Width;
    NewHeight := Zone.ParentZone.Height;
    case Zone.ParentZone.Orientation of
      doHorizontal:
      begin
        Scale := (Zone.ZoneLimit - Zone.ParentZone.ChildZones.LimitBegin) / NewHeight;
        NewHeight := Min(NewHeight div 2, Client.ClientHeight);
        NewY := Zone.ZoneLimit - Round(NewHeight * Scale);
      end;
      doVertical:
      begin
        Scale := (Zone.ZoneLimit - Zone.ParentZone.ChildZones.LimitBegin) / NewWidth;
        NewWidth := Min(NewWidth div 2, Client.ClientWidth);
        NewX := Zone.ZoneLimit - Round(NewWidth * Scale);
      end;
    end;
    DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
    if Zone.Visibled then
    begin
      if Zone.ParentZone.Orientation = doHorizontal then
        CnGlobalDockPresident.DragObject.DropAlign := alBottom
      else if Zone.ParentZone.Orientation = doVertical then
        CnGlobalDockPresident.DragObject.DropAlign := alRight;
      CnGlobalDockPresident.DragObject.DropOnControl := Zone.ChildControl;
      FDropOnZone := Zone;
    end;
  end;

Label LBDropCtlExist;
begin
  if DropAlign = alNone then
    DropAlign := alClient;
  VisibleClients := DockSite.VisibleDockClientCount;
  FDropOnZone := nil;

  MousePos := CnGlobalDockPresident.DragObject.DragPos;
  MapWindowPoints(0, DockSite.Handle, MousePos, 2);
  Zone := InternalHitTest(MousePos, HTFlag);
  if Zone <> nil then
  begin
    if Zone.ChildControl <> nil then
    begin
      if (HTFlag = HTCaption) or (HTFlag = HTClose) then
      begin
        DockRect := Zone.ChildControl.BoundsRect;
        CnGlobalDockPresident.DragObject.DropAlign := alClient;
        if Zone.ChildControl is TCnTabDockHostForm then
        begin
          if CnGlobalDockPresident.DragObject is TCnVIDDragDockObject then
            TCnVIDDragDockObject(CnGlobalDockPresident.DragObject).FDropTabControl :=
            TCnVIDTabPageControl(TCnTabDockHostForm(Zone.ChildControl).PageControl);
        end else
        begin
          if CnGlobalDockPresident.DragObject is TCnVIDDragDockObject then
            TCnVIDDragDockObject(CnGlobalDockPresident.DragObject).FDropTabControl := nil;
        end;
      end;
    end;
  end;
  { 当DockSite小于两个停靠控件在她里面，DockRect就应该作为被设置成DockSite的客户区域 }
  if (DropCtl = nil)(* or (DropCtl.DockOrientation = doNoOrient) or
     {(DropCtl = Client) or }(VisibleClients < 2) *)then
  begin
    if Zone <> nil then
    begin
      if Zone.ChildControl <> nil then
      begin
        if (HTFlag = HTCaption) or (HTFlag = HTClose) then
        begin
          CnGlobalDockPresident.DragObject.DropOnControl := Zone.ChildControl;
        end else if HTFlag = HTClient then
        begin
          DropCtl := Zone.ChildControl;
          goto LBDropCtlExist;
        end else if HTFlag = HTSplitter then
          DockOverSplitter;
      end else if HTFlag = HTSplitter then
      begin
        DockOverSplitter;
      end else Exit;
    end else
    begin
      DockRect := Rect(0, 0, DockSite.ClientWidth, DockSite.ClientHeight);
      { 当那里有一个停靠客户我们把DockSite的客户区分成一半 }
      if VisibleClients > 0 then
      with DockRect do
        case DropAlign of
          alLeft: Right := Right div 2;
          alRight: Left := Right div 2;
          alTop: Bottom := Bottom div 2;
          alBottom: Top := Bottom div 2;
        end;
    end;
  end
  else begin
LBDropCtlExist:
    { 否者，如果DockSite包含超过一个客户的时候， 根据鼠标下面的控件设置DockRect的坐标}
    Zone := FindControlZone(DropCtl);
    CtrlRect := DockRect;
    MapWindowPoints(0, DockSite.Handle, CtrlRect, 2);
    if Zone <> nil then
    begin
      { 判断鼠标是否在分割条附近，如果是的话，就调用DockOverSplitter函数然后退出，
        否者就计算CtrlRect的大小，在这里CtrlRect是指的是DropOnControl的大小 }
      if Zone.ParentZone.Orientation = doVertical then
      begin
        if (DropAlign = alRight) and (Zone.NextSibling <> nil) then
        begin
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end else if (DropAlign = alLeft) and (Zone.PrevSibling <> nil) then
        begin
          Zone := Zone.PrevSibling;
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end else
        begin
          if DropAlign in [alLeft, alRight] then
            CtrlRect := Bounds(Zone.ParentZone.Left, Zone.ParentZone.Top, Zone.ParentZone.Width, Zone.ParentZone.Height)
          else if (DropAlign in [alTop, alBottom, alClient]) then// or ((DockSite is TCnConjoinPanel) and (DropAlign = alClient)) then
          begin
            CtrlRect := DropCtl.BoundsRect;
            Dec(CtrlRect.Top, GrabberSize);
          end;
          OffsetRect(CtrlRect, 0, GrabberSize);
        end;
      end else if Zone.ParentZone.Orientation = doHorizontal then
      begin
        if (DropAlign = alBottom) and (Zone.NextSibling <> nil) then
        begin
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end else if (DropAlign = alTop) and (Zone.PrevSibling <> nil) then
        begin
          Zone := Zone.PrevSibling;
          DockOverSplitter;
          MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
          Exit;
        end else
        begin
          if DropAlign in [alTop, alBottom] then
            CtrlRect := Bounds(Zone.ParentZone.Left, Zone.ParentZone.Top, Zone.ParentZone.Width, Zone.ParentZone.Height)
          else if (DropAlign in [alLeft, alRight, alClient]) then //or ((DockSite is TCnConjoinPanel) and (DropAlign = alClient)) then
          begin
            CtrlRect := DropCtl.BoundsRect;
            Dec(CtrlRect.Top, GrabberSize);
          end;
          OffsetRect(CtrlRect, 0, GrabberSize);
        end;
      end else
      begin
        CtrlRect := DropCtl.BoundsRect;
        Dec(CtrlRect.Top, GrabberSize);
        OffsetRect(CtrlRect, 0, GrabberSize);
      end;
      { 然后根据CtrlRect和Control自身的位置来计算停靠的预览界面的大小 }
      NewX := CtrlRect.Left;
      NewY := CtrlRect.Top - GrabberSize;
      NewWidth := CtrlRect.Right - CtrlRect.Left;
      NewHeight := CtrlRect.Bottom - CtrlRect.Top;// + GrabberSize;
      if DropAlign in [alLeft, alRight] then
        NewWidth := Min(Client.UndockWidth, NewWidth div 2)
      else if DropAlign in [alTop, alBottom] then
        NewHeight := Min(Client.UndockHeight, NewHeight div 2);
      case DropAlign of
        alRight: Inc(NewX, CtrlRect.Right - CtrlRect.Left - NewWidth);
        alBottom: Inc(NewY, CtrlRect.Bottom - CtrlRect.Top - NewHeight);
      end;
      DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
      if DropAlign = alClient then
        DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
      if DropAlign = alNone then
      begin
      end;
    end;
  end;
  MapWindowPoints(DockSite.Handle, 0, DockRect, 2);
end;

function TCnVIDDockTree.GetDockEdge(DockRect: TRect; MousePos: TPoint;
  var DropAlign: TAlign; Control: TControl): TControl;
begin
  Result := inherited GetDockEdge(DockRect, MousePos, DropAlign, Control);
  if FLockDropDockSizeCount = 0 then
  begin
    // 只有在锁打开的时候才能调用这个程序
    if DropAlign in [alLeft, alRight] then
      DropDockSize := DockRect.Right - DockRect.Left
    else if DropAlign in [alTop, alBottom] then
      DropDockSize := DockRect.Bottom - DockRect.Top
    else DropDockSize := 0;
    Self.DockRect := DockRect;
  end;
end;

procedure TCnVIDDockTree.InsertNewParent(NewZone, SiblingZone: TCnDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
begin
  if FDropOnZone <> nil then
  begin
    SiblingZone := FDropOnZone;
    InsertSibling(NewZone, SiblingZone, InsertLast, Update);
  end else
    inherited;
end;

procedure TCnVIDDockTree.RemoveZone(Zone: TCnDockZone; Hide: Boolean);
begin
  if (FDropOnZone <> nil) and
    ((FDropOnZone.NextSibling = Zone) or (FDropOnZone = Zone)) then
    FDropOnZone := nil;
  inherited;
end;

procedure TCnVIDDockTree.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
var Zone: TCnDockZone;
  HTFlag: Integer;
  Pos: TPoint;
  Align: TAlign;
begin
  { 根据鼠标位置来判断是否可以进行停靠操作 }
  Pos := DockSite.ScreenToClient(MousePos);
  Zone := InternalHitTest(Pos, HTFlag);
  if Zone <> nil then
  begin
    if HTFlag = HTSPLITTER then
    begin
      InfluenceRect := GetSpiltterRect(Zone);
      MapWindowPoints(DockSite.Handle, 0, InfluenceRect, 2);
    end else
    begin
      Pos := MousePos;
      if Zone.ChildControl <> nil then
        Pos := Zone.ChildControl.ScreenToClient(MousePos);
      Align := ComputeVIDDockingRect(Zone.ChildControl, Client, InfluenceRect, Pos);
      if (Align = alNone) or (Client = Zone.ChildControl) then
      begin
        InfluenceRect := Rect(0, 0, 0, 0);
        CanDock := False;
      end else
      begin
        if Zone.ParentZone.Orientation = doVertical then
        begin
          if (Align = alRight) and (Zone.NextSibling <> nil) and (Zone.NextSibling.Visibled)  then
          begin
            InfluenceRect := GetSpiltterRect(Zone);
            InflateRect(InfluenceRect, DefExpandoRect, 0);
          end else if (Align = alLeft) and (Zone.PrevSibling <> nil) and (Zone.PrevSibling.Visibled) then
          begin
            InfluenceRect := GetSpiltterRect(Zone.PrevSibling);
            InflateRect(InfluenceRect, DefExpandoRect, 0);
          end else
            Exit;
        end else if Zone.ParentZone.Orientation = doHorizontal then
        begin
          if (Align = alBottom) and (Zone.NextSibling <> nil) and (Zone.NextSibling.Visibled) then
          begin
            InfluenceRect := GetSpiltterRect(Zone);
            InflateRect(InfluenceRect, 0, DefExpandoRect);
          end else if (Align = alTop) and (Zone.PrevSibling <> nil) and (Zone.PrevSibling.Visibled) then
          begin
            InfluenceRect := GetSpiltterRect(Zone.PrevSibling);
            InflateRect(InfluenceRect, 0, DefExpandoRect);
          end else
            Exit;
        end else
          Exit;
      end;
      MapWindowPoints(DockSite.Handle, 0, InfluenceRect, 2);
    end;
  end else
  begin
    InfluenceRect := Rect(0, 0, 0, 0);
    CanDock := False;
  end;
end;

procedure TCnVIDDockTree.LockDropDockSize;
begin
  Inc(FLockDropDockSizeCount);
end;

procedure TCnVIDDockTree.UnlockDropDockSize;
begin
  Dec(FLockDropDockSizeCount);
  if FLockDropDockSizeCount < 0 then
    FLockDropDockSizeCount := 0;
end;

procedure TCnVIDDockTree.PaintDockGrabberRect(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  { 没事做 }
end;

procedure TCnVIDDockTree.SetCaptionLeftOffset(const Value: Integer);
begin
  FCaptionLeftOffset := Value;
end;

procedure TCnVIDDockTree.SetCaptionRightOffset(const Value: Integer);
begin
  FCaptionRightOffset := Value;
end;

procedure TCnVIDDockTree.DrawCloseButton(Canvas: TCanvas; Zone: TCnDockZone; Left, Top: Integer);
var AZone: TCnAdvDockZone;
  ADockClient: TCnDockClient;
begin
  AZone := TCnAdvDockZone(Zone);
  if AZone <> nil then
  begin
    { 如果EnableCloseBtn属性为False,就不画关闭按钮 }
    ADockClient := FindDockClient(Zone.ChildControl);
    if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then Exit;
    DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+ButtonWidth,
      Top+ButtonHeight), DFC_CAPTION, DFCS_CAPTIONCLOSE or Integer(AZone.CloseBtnDown) * DFCS_PUSHED)
  end;
end;

procedure TCnVIDDockTree.GetCaptionRect(var Rect: TRect);
begin
  Inc(Rect.Left, 2 + CaptionLeftOffset);
  Inc(Rect.Top, 3);
  Dec(Rect.Right, ButtonWidth + CaptionRightOffset - 1);
  Dec(Rect.Bottom, 2);
end;

procedure TCnVIDDockTree.AdjustDockRect(Control: TControl;
  var ARect: TRect);
begin
  if (DockSite.Align <> alClient) or (TopZone.VisibleChildTotal > 1) then
    inherited;
end;

procedure TCnVIDDockTree.IgnoreZoneInfor(Stream: TMemoryStream);
var CompName: string;
begin
  { TopZone中的Visibled(一字节)和VisibleSize(四字节)没有用，被忽略掉 }
  Stream.Position := Stream.Position + 6;
  { 读出控件的名称 }
  ReadControlName(Stream, CompName);
end;

{ TCnVIDConjoinPanel }

function TCnVIDConjoinPanel.CreateDockManager: IDockManager;
var Option: TCnVIDConjoinServerOption;
begin
  Result := inherited CreateDockManager;
  if (ParentForm <> nil) and (ParentForm.DockClient.DockStyle <> nil) and (Result <> nil) then
  begin
    Option := TCnVIDConjoinServerOption(ParentForm.DockClient.DockStyle.ConjoinServerOption);
    (Result as ICnDockManager).GrabberSize := Option.GrabbersSize;
  end;
end;

procedure TCnVIDConjoinPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
//var
//  VIDSource: TCnVIDDragDockObject;
begin
{  if Source is TCnVIDDragDockObject then
  begin
    VIDSource := TCnVIDDragDockObject(Source);
    VIDSource.CurrState := dsDragEnter;
    VIDSource.OldState := dsDragEnter;
    SetTabControlPreview(VIDSource, VIDSource.FDropTabControl, dsDragLeave, VIDSource.DropAlign);
  end;}
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    inherited CustomDockDrop(Source, X, Y);
    ParentForm.Caption := '';
    if CnDockManager <> nil then
      CnDockManager.ActiveControl := Source.Control;
    if (Source.Control is TWinControl) and (Source.Control.Visible)
      and TWinControl(Source.Control).CanFocus then
      TWinControl(Source.Control).SetFocus;
  end;
end;


procedure TCnVIDConjoinPanel.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var DropAlign: TAlign;
//  VIDSource: TCnVIDDragDockObject;
//  DropCtl: TControl;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  if Accept and (Source is TCnVIDDragDockObject) then
  begin
{    VIDSource := TCnVIDDragDockObject(Source);
    DropCtl := VIDSource.GetDropCtl;
    if (DropCtl <> VIDSource.DropOnControl) or
      (VIDSource.FOldDropTabControl <> VIDSource.FDropTabControl) then
      SetTabControlPreview(VIDSource, VIDSource.FOldDropTabControl, dsDragLeave, VIDSource.DropAlign);
    SetTabControlPreview(VIDSource, VIDSource.FDropTabControl, State, VIDSource.DropAlign);
    if State = dsDragLeave then
      VIDSource.FDropTabControl := nil;
    VIDSource.FOldDropTabControl := VIDSource.FDropTabControl;}
    if State = dsDragMove then
    begin
      DropAlign := Source.DropAlign;
      CnDockManager.GetDockEdge(Source.EraseDockRect, Source.DragPos, DropAlign, Source.Control);
    end;
//    VIDSource.OldState := VIDSource.CurrState;
//    VIDSource.CurrState := State;
  end;
end;

procedure TCnVIDConjoinPanel.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TCnVIDConjoinPanel.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CnDockManager.GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

function TCnVIDConjoinPanel.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TCnVIDConjoinPanel.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  inherited DockDrop(Source, X, Y);
end;

procedure TCnVIDConjoinPanel.UpdateCaption(Exclude: TControl);
begin
  if VisibleDockClientCount > 1 then
    ParentForm.Caption := ''
  else
    inherited UpdateCaption(Exclude);
end;

{ TCnVIDTabPageControl }

procedure TCnVIDTabPageControl.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
var ARect: TRect;
  i: Integer;
  VIDSource: TCnVIDDragDockObject;
  DockClient: TCnDockClient;
  Host: TCnConjoinDockHostForm;
  Index: Integer;
begin
  if Source.DropAlign in [alClient, alNone] then
  begin
    if Source is TCnVIDDragDockObject then
    begin
      Cn_LockWindow(nil);
      IsLoading := True;
      try
        DoFloatForm(Source.Control);
        FreeAllDockableForm;
        VIDSource := TCnVIDDragDockObject(Source);
        //SetTabControlPreview(VIDSource, VIDSource.FDropTabControl, dsDragLeave, VIDSource.DropAlign);
        for i := 0 to VIDSource.SourceDockClientCount - 1 do
        begin
          Source.Control := VIDSource.SourceDockClients[i];
          inherited CustomDockDrop(Source, X, Y);
          if Source.Control is TCustomForm then
          begin
            if FTabImageList <> nil then
            begin
              Index := FTabImageList.AddIcon(TForm(Source.Control).Icon);
              if Index <> -1 then
                ActivePage.ImageIndex := Index;
            end;
          end;
        end;
      finally
        IsLoading := False;
        Cn_UnLockWindow;
        ReshowAllVisibleWindow;
        CnGlobalDockPresident.DragObject.Control := nil;
      end;
    end;
  end else
  begin
    // 创建平铺的服务窗体
    DockClient := FindDockClient(ParentForm);
    if DockClient <> nil then
    begin
      ARect := ParentForm.BoundsRect;
      Host := DockClient.CreateConjoinHostAndDockControl(ParentForm, Source.Control, Source.DropAlign);
      Host.BoundsRect := ARect;
      SetDockSite(ParentForm, False);
      SetDockSite(TWinControl(Source.Control), False);
      Host.Visible := True;
    end;
  end;
  FPanel.SelectSheet := nil;
  ParentForm.Caption := ActivePage.Caption;
end;

procedure TCnVIDTabPageControl.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var ARect: TRect;
//  VIDSource: TCnVIDDragDockObject;
begin
  {如果停靠客户上面有TDockClient控件，就同意停靠}
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  if Accept then
  begin
    if ParentForm.HostDockSite = nil then
    begin
      Source.DropAlign := ComputeVIDDockingRect(Self, Source.Control, ARect, Point(X, Y));
      if Source.DropAlign = alClient then
        ARect.Top := ARect.Top + Cn_GetSysCaptionHeight;
{      if Source is TCnVIDDragDockObject then
      begin
        VIDSource := TCnVIDDragDockObject(Source);
        SetTabControlPreview(VIDSource, Self, State, VIDSource.DropAlign);
        VIDSource.FDropTabControl := Self;
      end;}
      if Accept and (Source.DropAlign <> alNone) then
      begin
        Source.DockRect := ARect;
        gi_DockRect := ARect;
      end;
    end else
    begin
      if ParentForm.HostDockSite is TCnCustomDockPanel then
      begin
        ARect := Source.DockRect;
        TCnCustomDockPanel(ParentForm.HostDockSite).CnDockManager.PositionDockRect(Source.Control, Source.DropOnControl, Source.DropAlign, ARect);
        Source.DockRect := ARect;
      end;
    end;
  end;
end;

procedure TCnVIDTabPageControl.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
const
  DefExpandoRect = 20;
var
  CH_BW: Integer;
  ARect: TRect;
begin
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
  if ParentForm.HostDockSite <> nil then
    CanDock := False;
  if CanDock then
  begin
    {获得停靠控件的矩形区域}
    GetWindowRect(Parent.Handle, InfluenceRect);
    if PtInRect(InfluenceRect, MousePos) then
    begin
      ARect := InfluenceRect;
      InflateRect(ARect, -DefExpandoRect, -DefExpandoRect);
      {获得标题栏的高度和边框的宽度}
      CH_BW := Cn_GetSysCaptionHeightAndBorderWidth;
      Inc(ARect.Top, CH_BW + 1);
      Dec(ARect.Bottom, TabHeight);
      if PtInRect(ARect, MousePos) then
        InfluenceRect := Rect(0, 0, 0, 0);
    end;
  end;
end;

procedure TCnVIDTabPageControl.Change;
begin
  // 当PageControl的当前Tab改变的时候，就要改变标题栏的Caption。
  inherited Change;
  ParentForm.Caption := ActivePage.Caption;
  if ParentForm.HostDockSite is TCnCustomDockPanel then
  begin
    // 刷新ParentForm.HostDockSite中的把手部分。
    if ParentForm.Visible and ParentForm.CanFocus then
      ParentForm.SetFocus;
    ParentForm.HostDockSite.Invalidate;
  end;
  if (ActivePage <> nil) and (ActivePage.Visible) and (ActivePage.CanFocus) then
  begin
    if ParentForm.Visible and ParentForm.CanFocus then
      ActivePage.SetFocus;
  end;
end;

procedure TCnVIDTabPageControl.AdjustClientRect(var Rect: TRect);
begin
  // 调整Tab的Client的大小。
  Rect := ClientRect;
  if (Parent is TCnTabDockHostForm) and (VisibleDockClientCount = 1) then Exit;
  case TabPosition of
    tpTop:    Inc(Rect.Top, Panel.FTabHeight - 1);
    tpBottom: Dec(Rect.Bottom, Panel.FTabHeight - 1);
    tpLeft:   Inc(Rect.Left, Panel.FTabHeight - 1);
    tpRight:  Dec(Rect.Right, Panel.FTabHeight - 1);
  end;
end;

procedure TCnVIDTabPageControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TCnVIDTabPageControl.DrawTab(TabIndex: Integer;
  const Rect: TRect; Active: Boolean);
begin
  inherited DrawTab(TabIndex, Rect, Active);
end;

function TCnVIDTabPageControl.GetActiveFont: TFont;
begin
  Result := FPanel.FActiveFont;
end;

function TCnVIDTabPageControl.GetActiveSheetColor: TColor;
begin
  Result := FPanel.FActiveSheetColor;
end;

function TCnVIDTabPageControl.GetInactiveFont: TFont;
begin
  Result := FPanel.FInactiveFont;
end;

function TCnVIDTabPageControl.GetInactiveSheetColor: TColor;
begin
  Result := FPanel.Color;
end;

function TCnVIDTabPageControl.GetTabBottomOffset: Integer;
begin
  Result := FPanel.TabBottomOffset;
end;

function TCnVIDTabPageControl.GetTabLeftOffset: Integer;
begin
  Result := FPanel.TabLeftOffset;
end;

function TCnVIDTabPageControl.GetTabRightOffset: Integer;
begin
  Result := FPanel.TabRightOffset;
end;

function TCnVIDTabPageControl.GetTabTopOffset: Integer;
begin
  Result := FPanel.TabTopOffset;
end;

procedure TCnVIDTabPageControl.Paint;
begin
  inherited Paint;
end;

procedure TCnVIDTabPageControl.Resize;
begin
  // 当PageControl的大小改变的时候，也需要改变Fpanel的位置。
  inherited Resize;
  if Fpanel = nil then Exit;
  case TabPosition of
    tpLeft:
    begin
      // Fpanel在左边
      FPanel.Left := 0;
      FPanel.Width := Panel.FTabHeight;
      FPanel.Top := 0;
      FPanel.Height := Height;
    end;
    tpRight:
    begin
      // Fpanel在右边
      FPanel.Left := Width - Panel.FTabHeight;
      FPanel.Top := 0;
      FPanel.Width := Panel.FTabHeight;
      FPanel.Height := Height;
    end;
    tpTop:
    begin
      // Fpanel在上边
      FPanel.Left := 0;
      FPanel.Top := 0;
      FPanel.Width := Width;
      FPanel.Height := Panel.FTabHeight;
    end;
    tpBottom:
    begin
      // Fpanel在下边
      FPanel.Left := 0;
      FPanel.Top := Height - Panel.FTabHeight;
      FPanel.Width := Width;
      FPanel.Height := Panel.FTabHeight;
    end;
  end;
end;

procedure TCnVIDTabPageControl.SetActiveFont(const Value: TFont);
begin
  FPanel.FActiveFont.Assign(Value);
  if ActivePage <> nil then
    TCnVIDDockTabSheet(ActivePage).SetSheetSort(ActivePage.Caption);
  FPanel.Invalidate;
end;

procedure TCnVIDTabPageControl.SetActiveSheetColor(const Value: TColor);
begin
  FPanel.FActiveSheetColor := Value;
  FPanel.Invalidate;
end;

procedure TCnVIDTabPageControl.SetInactiveFont(const Value: TFont);
var i: Integer;
begin
  FPanel.FInactiveFont.Assign(Value);
  for i := 0 to PageCount - 1 do
    if Pages[i] <> ActivePage then
      TCnVIDDockTabSheet(Pages[i]).SetSheetSort(Pages[i].Caption);
  FPanel.Invalidate;
end;

procedure TCnVIDTabPageControl.SetInactiveSheetColor(const Value: TColor);
begin
  if FPanel.Color <> Value then
  begin
    FPanel.Color := Value;
    FPanel.Invalidate;
  end;
end;

procedure TCnVIDTabPageControl.SetTabBottomOffset(const Value: Integer);
begin
  if FPanel.TabBottomOffset <> Value then
  begin
    FPanel.TabBottomOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TCnVIDTabPageControl.SetTabHeight(Value: Smallint);
begin
  inherited SetTabHeight(Value);
  if Panel.FTabHeight <> Value then
  begin
    Panel.FTabHeight := Value;
    FPanel.Invalidate;
  end;
end;

procedure TCnVIDTabPageControl.SetTabLeftOffset(const Value: Integer);
begin
  if FPanel.TabLeftOffset <> Value then
  begin
    FPanel.TabLeftOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TCnVIDTabPageControl.SetTabPosition(Value: TTabPosition);
begin
  Assert(Value in [tpTop, tpBottom], gs_CannotSetTabPosition);
  inherited SetTabPosition(Value);
  Resize;
end;

procedure TCnVIDTabPageControl.SetTabRightOffset(const Value: Integer);
begin
  if FPanel.TabRightOffset <> Value then
  begin
    FPanel.TabRightOffset := Value;
    FPanel.Invalidate;
  end;
end;

procedure TCnVIDTabPageControl.SetTabTopOffset(const Value: Integer);
begin
  if FPanel.TabTopOffset <> Value then
  begin
    FPanel.TabTopOffset := Value;
    FPanel.Invalidate;
  end;
end;

constructor TCnVIDTabPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanel := nil;
  TabWidth := 1;
  MultiLine := True;
  CnDockTabSheetClass := TCnVIDDockTabSheet;
  CnTabPanelClass := TCnTabPanel;
  FTempSheet := nil;
  TabPosition := tpBottom;
  FTabImageList := nil;
  Images := nil;
  if AOwner is TCnTabDockHostForm then
  begin
    FTabImageList := TCustomImageList.Create(AOwner);
    Images := FTabImageList;
  end;
end;

destructor TCnVIDTabPageControl.Destroy;
begin
  if FTabImageList <> nil then
  begin
    FTabImageList.Free;
    FTabImageList := nil;
  end;
  if FPanel <> nil then
  begin
    FPanel.Free;
    FPanel := nil;
  end;
  inherited;
end;

procedure TCnVIDTabPageControl.Loaded;
begin
  inherited;
  CreatePanel;
end;

procedure TCnVIDTabPageControl.CreatePanel;
begin
  if FPanel = nil then
  begin
    FPanel := CnTabPanelClass.Create(Self);
    FPanel.Page := Self;
    FPanel.Parent := Self;
    FPanel.TabLeftOffset := 5;
    FPanel.TabRightOffset := 5;
    FPanel.TabTopOffset := 3;
    FPanel.TabBottomOffset := 3;
    ActiveSheetColor := clBtnFace;
    InactiveSheetColor := clBtnShadow;
  end;
  Resize;
end;

procedure TCnVIDTabPageControl.CreateWnd;
begin
  inherited;
end;

procedure TCnVIDTabPageControl.SetActivePage(Page: TCnDockTabSheet);
begin
  inherited SetActivePage(Page);
  FPanel.Invalidate;
end;

procedure TCnVIDTabPageControl.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
var Index: Integer;
begin
  inherited DockDrop(Source, X, Y);
  FPanel.SelectSheet := nil;
  ParentForm.Caption := ActivePage.Caption;
  if Source.Control is TCustomForm then
  begin
    if Source.Control.Visible and (Source.Control.Parent is TCnDockTabSheet) then
      ActivePage := TCnDockTabSheet(Source.Control.Parent);
    if FTabImageList <> nil then
    begin
      Index := FTabImageList.AddIcon(TForm(Source.Control).Icon);
      if (Index <> -1) and (ActivePage <> nil) then
        ActivePage.ImageIndex := Index;
    end;
  end;
end;

function TCnVIDTabPageControl.GetDockClientFromMousePos(
  MousePos: TPoint): TControl;
var PageIndex: Integer;
begin
  Result := nil;
  case TabPosition of
  tpTop:
    PageIndex := Panel.FindSheetWithPos(MousePos.X, MousePos.y, 0, Panel.Height - TabBottomOffset);
  tpBottom:
    PageIndex := Panel.FindSheetWithPos(MousePos.x, MousePos.y, TabBottomOffset, Panel.Height);
  tpLeft:
    PageIndex := Panel.FindSheetWithPos(MousePos.y, MousePos.x, 0, Panel.Height - TabBottomOffset);
  tpRight:
    PageIndex := Panel.FindSheetWithPos(MousePos.y, MousePos.x, TabBottomOffset, Panel.Height);
  else
    PageIndex := -1;
  end;
  if PageIndex >= 0 then
  begin
    Result := Pages[PageIndex].Controls[0];
    if Result.HostDockSite <> Self then Result := nil;
  end;
end;

procedure TCnVIDTabPageControl.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
var ARect: TRect;
begin
  DropAlign := ComputeVIDDockingRect(Self, Source.Control, ARect, MousePos);
end;

function TCnVIDTabPageControl.GetVisibleTheetCount: Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to PageCount - 1 do
    if Pages[i].TabVisible then
      Inc(Result);
end;

procedure TCnVIDTabPageControl.UpdateCaption(Exclude: TControl);
begin
  ParentForm.Caption := ActivePage.Caption;
end;

procedure TCnVIDTabPageControl.SetHotTrack(Value: Boolean);
begin
  inherited SetHotTrack(Value);

end;

procedure TCnVIDTabPageControl.SetImages(Value: TCustomImageList);
begin
  inherited SetImages(Value);
  if Panel <> nil then
  begin
    Panel.ShowTabImages := Value <> nil;
    Panel.Invalidate;
  end;
end;

function TCnVIDTabPageControl.GetHotTrackColor: TColor;
begin
  Result := Panel.FHotTrackColor;
end;

procedure TCnVIDTabPageControl.SetHotTrackColor(const Value: TColor);
begin
  if Panel.FHotTrackColor <> Value then
  begin
    Panel.FHotTrackColor := Value;
    Panel.Invalidate;
  end;
end;

function TCnVIDTabPageControl.GetShowTabImages: Boolean;
begin
  Result := FPanel.FShowTabImages;
end;

procedure TCnVIDTabPageControl.SetShowTabImages(const Value: Boolean);
begin
  FPanel.ShowTabImages := Value;
end;

function TCnVIDTabPageControl.CustomUnDock(Source: TCnDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
var CurrPage: TCnDockTabSheet;
  i: Integer;
begin
  if not ((Source.Control.HostDockSite <> nil) and
    (Source.DropOnControl = Source.Control.HostDockSite.Parent) and
    (Source.DropAlign = alClient)) then
  begin
    CurrPage := GetPageFromDockClient(Client);
    if (CurrPage <> nil) then
    begin
      if (FTabImageList <> nil) and ShowTabImages and
        (FTabImageList.Count > CurrPage.ImageIndex) then
      begin
        FTabImageList.Delete(CurrPage.ImageIndex);
        for i := 0 to PageCount - 1 do
          if Pages[i].ImageIndex > CurrPage.ImageIndex then
            Pages[i].ImageIndex := Pages[i].ImageIndex - 1;
      end;
    end;
    Result := inherited CustomUnDock(Source, NewTarget, Client);
  end else Result := True;
end;

procedure TCnVIDTabPageControl.AfterConstruction;
begin
  inherited;
  CreatePanel;
end;

function TCnVIDTabPageControl.GetPage(Index: Integer): TCnVIDDockTabSheet;
begin
  Result := TCnVIDDockTabSheet(inherited Pages[Index]);
end;

function TCnVIDTabPageControl.GetActiveVIDPage: TCnVIDDockTabSheet;
begin
  Result := TCnVIDDockTabSheet(inherited ActivePage);
end;

procedure TCnVIDTabPageControl.SetActiveVIDPage(
  const Value: TCnVIDDockTabSheet);
begin
  ActivePage := Value;
end;

{ TCnTabPanel }

constructor TCnTabPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Page := nil;
  FCaptionTopOffset := 0;
  FCaptionLeftOffset := 5;
  FCaptionRightOffset := 5;
  FTabBottomOffset := 3;
  FTabSplitterWidth := 3;
  FTabHeight := 22;
  FSortList := TList.Create;
  FActiveFont := TFont.Create;
  FActiveFont.Color := clBlack;
  FInactiveFont := TFont.Create;
  FInactiveFont.Color := clWhite;
  FHotTrackColor := clBlue;
  FTempPages := TList.Create;
  FSelectHotIndex := -1;
  FShowTabImages := False;
  FSelectSheet := nil;
end;

procedure TCnTabPanel.DeleteSorts(Sheet: TCnVIDDockTabSheet);
var SheetIndex: Integer;
begin
  SheetIndex := FSortList.IndexOf(Sheet);
  if SheetIndex >= 0 then
    FSortList.Delete(SheetIndex);
  if Sheet <> nil then
    Sheet.TabVisible := False;
  SetShowTabWidth;
  Page.Invalidate;
end;

destructor TCnTabPanel.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  FSortList.Free;
  FTempPages.Free;
  inherited;
end;

function TCnTabPanel.FindSheetWithPos(cX, cY, cTopOffset, cBottomOffset: Integer): Integer;
var i: Integer;
  CompleteWidth, CurrTabWidth: Integer;
  Pages: TList;
begin
  Result := -1;
  if (cY > cBottomOffset) or (cY < cTopOffset) then Exit;
  CompleteWidth := 0;
  if FSelectSheet = nil then
    Pages := Page.PageSheets
  else Pages := FTempPages;
  for i := 0 to Pages.Count - 1 do
  begin
    if not TCnVIDDockTabSheet(Pages[i]).TabVisible then Continue;
    CurrTabWidth := TCnVIDDockTabSheet(Pages[i]).ShowTabWidth;
    if (cX >= FTabLeftOffset + CompleteWidth) and (cX <= FTabLeftOffset + CurrTabWidth + CompleteWidth + FTabSplitterWidth) then
    begin
      Result := i;
      Exit;
    end;
    Inc(CompleteWidth, CurrTabWidth + FTabSplitterWidth);
  end;
end;

function TCnTabPanel.GetPageIndexFromMousePos(X, Y: Integer): Integer;
begin
  Result := -1;
  case Page.TabPosition of
  tpTop:
    Result := FindSheetWithPos(X, y, 0, Height - TabBottomOffset);
  tpBottom:
    Result := FindSheetWithPos(x, y, TabBottomOffset, Height);
  tpLeft:
    Result := FindSheetWithPos(y, x, 0, Height - TabBottomOffset);
  tpRight:
    Result := FindSheetWithPos(y, x, TabBottomOffset, Height);
  end;
end;

function TCnTabPanel.GetMaxTabWidth: TCnDockTabSheet;
var i: Integer;
  MaxWidth, CurrWidth: Integer;
begin
  Result := nil;
  MaxWidth := 0;
  if Page = nil then Exit;
  for i := 0 to Page.PageCount - 1 do
  begin
    CurrWidth := Canvas.TextWidth(Page.Tabs[i]);
    if MaxWidth < CurrWidth then
    begin
      Result := Page.Pages[i];
      MaxWidth := CurrWidth;
    end;
  end;
end;

function TCnTabPanel.GetMinTabWidth: TCnDockTabSheet;
var i: Integer;
  MinWidth, CurrWidth: Integer;
begin
  Result := nil;
  MinWidth := 0;
  for i := 0 to Page.PageCount - 1 do
  begin
    CurrWidth := Canvas.TextWidth(Page.Tabs[i]);
    if MinWidth > CurrWidth then
    begin
      Result := Page.Pages[i];
      MinWidth := CurrWidth;
    end;
  end;
end;

function TCnTabPanel.GetPanelHeight: Integer;
begin
  Result := 0;
  case Page.TabPosition of
    tpLeft, tpRight:
      Result := Width;
    tpTop, tpBottom:
      Result := Height;
  end;
end;

function TCnTabPanel.GetPanelWidth: Integer;
begin
  Result := 0;
  case Page.TabPosition of
    tpLeft, tpRight:
      Result := Height;
    tpTop, tpBottom:
      Result := Width;
  end;
end;

function TCnTabPanel.GetSorts(Index: Integer): TCnVIDDockTabSheet;
begin
  Result := FSortList[Index];
end;

function TCnTabPanel.GetTotalTabWidth: Integer;
var i: Integer;
begin
  Result := 0;
  if FSortList = nil then Exit;
  for i := 0 to FSortList.Count - 1 do
    Inc(Result, Sorts[i].TabWidth + Integer(i <> FSortList.Count - 1) * FTabSplitterWidth);
end;

procedure TCnTabPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var Ctrl: TControl;
  Index: Integer;
  Msg: TWMMouse;
  Sheet: TCnVIDDockTabSheet;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Page = nil then Exit;
  { 首先根据鼠标的坐标确定是哪个Sheet }
  Index := GetPageIndexFromMousePos(X, Y);
  if (Index >= 0) then
  begin
    if Index <> Page.ActivePageIndex then
    begin
      Sheet := Page.ActiveVIDPage;
      Page.ActivePageIndex := Index;
      Sheet.SetSheetSort(Sheet.Caption);
      Page.ActiveVIDPage.SetSheetSort(Page.ActiveVIDPage.Caption);
      Page.Change;
      Invalidate;
    end;

    if Button = mbLeft then
    begin
      // 只有在鼠标左键操作的时候才可以移动Tab。
      FSelectSheet := TCnVIDDockTabSheet(Page.ActivePage);
      { Delphi6.0以上版本 }
      {$IFDEF COMPILER6_UP}
      FTempPages.Assign(Page.PageSheets);
      {$ELSE}
      { Delphi5.0以下版本 }
      AssignList(Page.PageSheets, FTempPages);
      {$ENDIF}

    end;

    Ctrl := GetDockClientFromPageIndex(Index);
    if Ctrl <> nil then
    begin
      { 查找到DockCtl上的TCnDockClient，然后把她赋值给全局变量GlobalDockClient }
      GlobalDockClient := FindDockClient(Ctrl);

      if GlobalDockClient <> nil then
      begin
        Msg.Msg := WM_NCLBUTTONDOWN + Integer(Button) * 3 + Integer(ssDouble in Shift) * 2;
        Msg.Pos.x := X;
        Msg.Pos.y := Y;
        if not (ssDouble in Shift) then
          // 单击鼠标
          GlobalDockClient.DoNCButtonDown(Page.DoMouseEvent(Msg, Page), Button, msTabPage)
        else
        begin
          // 双击鼠标
          GlobalDockClient.DoNCButtonDblClk(Page.DoMouseEvent(Msg, Page), Button, msTabPage);
          if (Button = mbLeft) and GlobalDockClient.CanFloat then
            Ctrl.ManualDock(nil, nil, alNone);
        end;
      end;
    end;
  end;
end;

procedure TCnTabPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var Index: Integer;
  Ctrl: TControl;
  ARect: TRect;
begin
  inherited MouseMove(Shift, X, Y);

  Index := GetPageIndexFromMousePos(X, Y);

  if Page.HotTrack and (Index <> FSelectHotIndex) then
  begin
    FSelectHotIndex := Index;
    Invalidate;
  end;

  if Assigned(FSelectSheet) then
  begin
    Index := GetPageIndexFromMousePos(X, Y);
    if Index >= 0 then
    begin
      if (Index <> Page.ActivePageIndex) and (Page.PageCount > Index) then
      begin
        FSelectSheet.PageIndex := Index;
        Invalidate;
      end;
    end else
    begin
      case Page.TabPosition of
      tpTop:
        ARect := Rect(0, 0, Width, Height - FTabBottomOffset);
      tpBottom:
        ARect := Rect(0, FTabBottomOffset, Width, Height);
      tpLeft:
        ARect := Rect(0, 0, Width - FTabBottomOffset, Height);
      tpRight:
        ARect := Rect(FTabBottomOffset, 0, Width, Height);
      else
        ARect := Rect(0, 0, 0, 0);
      end;
      if PtInRect(ARect, Point(X, Y)) then Exit;
      if Page.FTempSheet = nil then
      begin
        Ctrl := GetDockClientFromPageIndex(FSelectSheet.PageIndex);
        if Ctrl <> nil then
          { 如果条件都满足，就调用CnGlobalDockPresident的BeginDrag方法，开始停靠操作 }
          CnGlobalDockPresident.BeginDrag(Ctrl, False, 1);
      end;
    end;
  end;
end;

procedure TCnTabPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Ctrl: TControl;
  Index: Integer;
  Msg: TWMMouse;
begin
  inherited;
  FSelectSheet := nil;
  if Page = nil then Exit;
  { 首先根据鼠标的坐标确定是哪个Sheet }
  Index := GetPageIndexFromMousePos(X, Y);
  Ctrl := GetDockClientFromPageIndex(Index);
  if Ctrl <> nil then
  begin
    { 查找到DockCtl上的TCnDockClient，然后把她赋值给全局变量GlobalDockClient }
    GlobalDockClient := FindDockClient(Ctrl);

    if (GlobalDockClient <> nil) then
    begin
      Msg.Msg := WM_NCLBUTTONUP + Integer(Button) * 3 + Integer(ssDouble in Shift) * 2;
      Msg.Pos := PointToSmallPoint(Page.ScreenToClient(ClientToScreen(Point(X, Y))));
      if not (ssDouble in Shift) then
        // 单击鼠标
        GlobalDockClient.DoNCButtonUp(Page.DoMouseEvent(Msg, Page), Button, msTabPage);
    end;
  end;
end;

procedure TCnTabPanel.Paint;
var ARect: TRect;
  CurrTabWidth: Integer;
  i, CompleteWidth: Integer;
  ImageWidth: Integer;
  CaptionString: string;
//  LogFont : TLogFont;
begin
  inherited Paint;
  if Page = nil then Exit;

  if (Page.Images <> nil) and (Page.ShowTabImages) then
    ImageWidth := Page.Images.Width
  else ImageWidth := 0;

  { 首先填充整个Panel的颜色 }
  Canvas.Brush.Color := Page.ActiveSheetColor;
  case Page.TabPosition of
    tpLeft:   Canvas.FillRect(Rect(PanelHeight - FTabBottomOffset, 0, PanelHeight, PanelWidth));
    tpRight:  Canvas.FillRect(Rect(0, 0, FTabBottomOffset, PanelWidth));
    tpTop:    Canvas.FillRect(Rect(0, PanelHeight - FTabBottomOffset, PanelWidth, PanelHeight));
    tpBottom: Canvas.FillRect(Rect(0, 0, PanelWidth, FTabBottomOffset));
  end;

  { 再画一条黑色的阴影线 }
  case Page.TabPosition of
    tpTop, tpLeft: Canvas.Pen.Color := clWhite;
    tpBottom, tpRight: Canvas.Pen.Color := clBlack;
  end;

  case Page.TabPosition of
    tpLeft:
    begin
      Canvas.MoveTo(PanelHeight - FTabBottomOffset, 0);
      Canvas.LineTo(PanelHeight - FTabBottomOffset, PanelWidth);
    end;
    tpRight:
    begin
      Canvas.MoveTo(FTabBottomOffset, 0);
      Canvas.LineTo(FTabBottomOffset, PanelWidth);
    end;
    tpTop:
    begin
      Canvas.MoveTo(0, PanelHeight - FTabBottomOffset);
      Canvas.LineTo(PanelWidth, PanelHeight - FTabBottomOffset);
    end;
    tpBottom:
    begin
      Canvas.MoveTo(0, FTabBottomOffset);
      Canvas.LineTo(PanelWidth, FTabBottomOffset);
    end;
  end;

  CompleteWidth := 0;

  Canvas.Brush.Style := bsClear;

  for i := 0 to Page.PageCount - 1 do
  begin
    if not Page.Pages[i].TabVisible then Continue;

    { 获得当前页面的宽度, 宽度 = 字符串的宽度加上左边距和右边距 }
    CurrTabWidth := TCnVIDDockTabSheet(Page.Pages[i]).ShowTabWidth;// + ImageWidth;

    if Page.ActivePageIndex = i then
    begin
      { 画一个被选中的页面 }
      Canvas.Brush.Color := Page.ActiveSheetColor;
      case Page.TabPosition of
        tpLeft:   Canvas.FillRect(Rect(FTabTopOffset, CompleteWidth + FTabLeftOffset,
                    PanelHeight, CompleteWidth + FTabLeftOffset + CurrTabWidth));
        tpRight:  Canvas.FillRect(Rect(FTabBottomOffset, CompleteWidth + FTabLeftOffset,
                    PanelHeight - FTabTopOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth));
        tpTop:    Canvas.FillRect(Rect(CompleteWidth + FTabLeftOffset, FTabTopOffset,
                    CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight));
        tpBottom: Canvas.FillRect(Rect(CompleteWidth + FTabLeftOffset, FTabBottomOffset,
                    CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight - FTabTopOffset));
      end;

      { 画两条被照亮的白线 }
      Canvas.Pen.Color := clWhite;
      case Page.TabPosition of
        tpLeft:
        begin
          Canvas.MoveTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset);
          Canvas.LineTo(FTabTopOffset, CompleteWidth + FTabLeftOffset);
          Canvas.LineTo(FTabTopOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
          Canvas.Pen.Color := clBlack;
          Canvas.LineTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
        end;
        tpRight:
        begin
          Canvas.MoveTo(FTabTopOffset, CompleteWidth + FTabLeftOffset);
          Canvas.LineTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset);
          Canvas.Pen.Color := clBlack;
          Canvas.LineTo(PanelHeight - FTabBottomOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
          Canvas.LineTo(FTabTopOffset, CompleteWidth + FTabLeftOffset + CurrTabWidth);
        end;
        tpTop:
        begin
          Canvas.MoveTo(CompleteWidth + FTabLeftOffset, PanelHeight - FTabBottomOffset);
          Canvas.LineTo(CompleteWidth + FTabLeftOffset, FTabTopOffset);
          Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, FTabTopOffset);
          Canvas.Pen.Color := clBlack;
          Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight - FTabTopOffset);
        end;
        tpBottom:
        begin
          Canvas.MoveTo(CompleteWidth + FTabLeftOffset, FTabBottomOffset);
          Canvas.LineTo(CompleteWidth + FTabLeftOffset, PanelHeight - FTabTopOffset);
          Canvas.Pen.Color := clBlack;
          Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, PanelHeight - FTabTopOffset);
          Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth, FTabBottomOffset);
        end;
      end;

      { 字体为焦点字体 }
      Canvas.Font.Assign(FActiveFont);
    end else
    begin
      { 画页面之间的分割线 }
      if (i < Page.ActivePageIndex - 1) or (i > Page.ActivePageIndex) then
      begin
        Canvas.Pen.Color := Page.InactiveFont.Color;
        case Page.TabPosition of
          tpLeft, tpRight:
          begin
            Canvas.MoveTo(PanelHeight - FTabBottomOffset - 3, CompleteWidth + FTabLeftOffset + CurrTabWidth);
            Canvas.LineTo(FTabTopOffset + 2, CompleteWidth + FTabLeftOffset + CurrTabWidth);
          end;
          tpTop, tpBottom:
          begin
            Canvas.MoveTo(CompleteWidth + FTabLeftOffset + CurrTabWidth , PanelHeight - FTabBottomOffset - 3);
            Canvas.LineTo(CompleteWidth + FTabLeftOffset + CurrTabWidth , FTabTopOffset + 2);
          end;
        end;
      end;
      Canvas.Brush.Color := Page.InactiveSheetColor;
      { 字体为非焦点字体 }
      Canvas.Font.Assign(FInactiveFont);
    end;

    if FSelectHotIndex = i then
      Canvas.Font.Color := FHotTrackColor;

    case Page.TabPosition of
      tpLeft:   ARect := Rect(FTabTopOffset + FCaptionTopOffset + 1,
                  CompleteWidth + FTabLeftOffset + FCaptionLeftOffset,
                  PanelHeight,
                  CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset);

      tpRight:  ARect := Rect(FTabBottomOffset + FCaptionTopOffset + 1,
                  CompleteWidth + FTabLeftOffset + FCaptionLeftOffset,
                  PanelHeight,
                  CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset);

      tpTop:    ARect := Rect(CompleteWidth + FTabLeftOffset + FCaptionLeftOffset + Integer(FShowTabImages) * (ImageWidth + FCaptionLeftOffset),
                  FTabTopOffset + FCaptionTopOffset + 1,
                  CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset,// + Integer(FShowTabImages) * FCaptionRightOffset,
                  PanelHeight);

      tpBottom: ARect := Rect(CompleteWidth + FTabLeftOffset + FCaptionLeftOffset + Integer(FShowTabImages) * (ImageWidth + FCaptionLeftOffset),
                  FTabBottomOffset + FCaptionTopOffset + 1,
                  CompleteWidth + FTabLeftOffset + CurrTabWidth - FCaptionRightOffset,// + Integer(FShowTabImages) * FCaptionRightOffset,
                  PanelHeight);
    end;

    CaptionString := Page.Pages[i].Caption;

    { 画文字 }
    DrawText(Canvas.Handle, PChar(CaptionString), Length(CaptionString),
        ARect, DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS);
    { 画图象 }
    if FShowTabImages and (Page.Images <> nil) and (CurrTabWidth > ImageWidth + 2 * FCaptionLeftOffset) then
      Page.Images.Draw(Canvas, CompleteWidth + FTabLeftOffset + FCaptionLeftOffset,
        FTabBottomOffset + FCaptionTopOffset + 1, Page.Pages[i].ImageIndex, True);

    Inc(CompleteWidth, CurrTabWidth + FTabSplitterWidth);
  end;

  { 最后画一个边框 }
  Canvas.Brush.Color := Page.ActiveSheetColor;
  ARect := ClientRect;
  Canvas.FrameRect(ARect);
end;

procedure TCnTabPanel.Resize;
begin
  inherited Resize;
  SetShowTabWidth;
end;

procedure TCnTabPanel.SetCaptionLeftOffset(const Value: Integer);
begin
  if FCaptionLeftOffset <> Value then
  begin
    FCaptionLeftOffset := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetCaptionRightOffset(const Value: Integer);
begin
  if FCaptionRightOffset <> Value then
  begin
    FCaptionRightOffset := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetCaptionTopOffset(const Value: Integer);
begin
  if FCaptionTopOffset <> Value then
  begin
    FCaptionTopOffset := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetPage(const Value: TCnVIDTabPageControl);
begin
  FPage := Value;
end;

procedure TCnTabPanel.SetPanelHeight(const Value: Integer);
begin
  if PanelHeight <> Value then
  begin
    case Page.TabPosition of
      tpLeft, tpRight: Width := Value;
      tpTop, tpBottom: Height := Value;
    end;
    SetShowTabWidth;
  end;
end;

procedure TCnTabPanel.SetTabBottomOffset(const Value: Integer);
begin
  if FTabBottomOffset <> Value then
  begin
    FTabBottomOffset := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetTabLeftOffset(const Value: Integer);
begin
  if FTabLeftOffset <> Value then
  begin
    FTabLeftOffset := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetTabRightOffset(const Value: Integer);
begin
  if FTabRightOffset <> Value then
  begin
    FTabRightOffset := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetTabSplitterWidth(const Value: Integer);
begin
  if FTabSplitterWidth <> Value then
  begin
    FTabSplitterWidth := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetTabTopOffset(const Value: Integer);
begin
  if FTabTopOffset <> Value then
  begin
    FTabTopOffset := Value;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetTotalTabWidth(const Value: Integer);
begin
end;

function TCnTabPanel.GetDockClientFromPageIndex(Index: Integer): TControl;
begin
  Result := nil;
  if Index >= 0 then
  begin
    if Page.Pages[Index].ControlCount = 1 then
    begin
      Result := Page.Pages[Index].Controls[0];
      if Result.HostDockSite <> Page then Result := nil;
    end;
  end;
end;

procedure TCnTabPanel.SetShowTabWidth;
var i, j, TempWidth: Integer;
  PanelWidth, VisibleCount: Integer;
  ImageWidth: Integer;
begin
  if Page = nil then Exit;
  if FSortList = nil then Exit;
  PanelWidth := 0;
  case Page.TabPosition of
  tpTop, tpBottom:
    PanelWidth := Width;
  tpLeft, tpRight:
    PanelWidth := Height;
  end;
  // 总共的宽度
  TempWidth := PanelWidth - FCaptionLeftOffset - FCaptionRightOffset;
  if Page.ShowTabImages then
    ImageWidth := Page.Images.Width + FCaptionLeftOffset
  else ImageWidth := 0;
  VisibleCount := Page.VisibleTheetCount;
  j := 0;
  for i := 0 to FSortList.Count - 1 do
  begin
    // 只有可见的Tab才能调整Tab的宽度
    if not Sorts[i].TabVisible then Continue;
    // 只有当Tab能够显示的宽度小于Tab本身的宽度TabWidth的时候才重新设置Tab的ShowTabWidth，
    // 否者就把Tab的ShowTabWidth设置成Tab本身的宽度TabWidth。
    if (VisibleCount - j) * (Sorts[i].TabWidth + FTabSplitterWidth + ImageWidth) > TempWidth then
    begin
      Sorts[i].FShowTabWidth := TempWidth div (VisibleCount - j) - FTabSplitterWidth;
    end else
      Sorts[i].FShowTabWidth := Sorts[i].TabWidth + ImageWidth;
    // TempWidth需要减去显示宽度ShowTabWidth和分割宽度TabSplitterWidth。
    Dec(TempWidth, Sorts[i].FShowTabWidth + FTabSplitterWidth);
    // 可见的Tab+1。
    Inc(j);
  end;
end;

procedure TCnTabPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FSelectHotIndex <> -1 then
  begin
    FSelectHotIndex := -1;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetShowTabImages(const Value: Boolean);
begin
  if FShowTabImages <> Value then
  begin
    FShowTabImages := Value;
    SetShowTabWidth;
    Invalidate;
  end;
end;

procedure TCnTabPanel.SetTabHeight(const Value: Integer);
begin
  FTabHeight := Value;
  Height := FTabHeight + FTabTopOffset + FTabBottomOffset;
end;

{ TCnVIDDockTabSheet }

constructor TCnVIDDockTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsSourceDockClient := False;
end;

destructor TCnVIDDockTabSheet.Destroy;
begin
  if (PageControl is TCnVIDTabPageControl) and (PageControl <> nil) then
    TCnVIDTabPageControl(PageControl).Panel.DeleteSorts(Self);
  inherited Destroy;
end;

procedure TCnVIDDockTabSheet.Loaded;
begin
  inherited;
  SetSheetSort(Caption);
end;

procedure TCnVIDDockTabSheet.SetPageControl(
  APageControl: TCnDockPageControl);
begin
  inherited;

end;

procedure TCnVIDDockTabSheet.SetSheetSort(CaptionStr: string);
var TempWidth: Integer;

  procedure DoSetSheetSort;
  var i: Integer;
  begin
    TCnVIDTabPageControl(PageControl).Panel.FSortList.Remove(Self);
    for i := 0 to TCnVIDTabPageControl(PageControl).Panel.FSortList.Count - 1 do
    begin
      if TCnVIDTabPageControl(PageControl).Panel.Sorts[i].TabWidth > TempWidth then
      begin
        TCnVIDTabPageControl(PageControl).Panel.FSortList.Insert(i, Self);
        Exit;
      end;
    end;
    TCnVIDTabPageControl(PageControl).Panel.FSortList.Add(Self);
  end;

var TabPanel: TCnTabPanel;

begin
  if (PageControl is TCnVIDTabPageControl) and (PageControl <> nil) then
  begin
    TabPanel := TCnVIDTabPageControl(PageControl).Panel;
    if PageControl.ActivePage = Self then
      TabPanel.Canvas.Font.Assign(TabPanel.Page.ActiveFont)
    else TabPanel.Canvas.Font.Assign(TabPanel.Page.InactiveFont);
    TempWidth := TabPanel.Canvas.TextWidth(
      CaptionStr) + TabPanel.CaptionLeftOffset + TabPanel.CaptionRightOffset;
    if TempWidth <> FTabWidth then
    begin
      DoSetSheetSort;
      FTabWidth := TempWidth;
      TabPanel.SetShowTabWidth;
      TabPanel.Invalidate;
    end;
  end;
end;

procedure TCnVIDDockTabSheet.SetTabWidth(const Value: Integer);
begin
  FTabWidth := Value;
end;

procedure TCnVIDDockTabSheet.UpdateTabShowing;
begin
  inherited UpdateTabShowing;
  TCnVIDTabPageControl(PageControl).Panel.SetShowTabWidth;
end;

procedure TCnVIDDockTabSheet.WMSETTEXT(var Message: TMessage);
begin
  inherited;
  SetSheetSort(PChar(Message.LParam));
end;

function TCnVIDDockStyle.GetControlName: string;
begin
  Result := Format(gs_LikeVIDStyle, [inherited GetControlName]);
end;

{ TCnVIDDragDockObject }

constructor TCnVIDDragDockObject.Create(AControl: TControl);

  procedure DoGetSourceDockClients(Control: TControl);
  var i: Integer;
    DockableControl: TWinControl;
  begin
    if (Control is TCnDockableForm) then
    begin
      DockableControl := TCnDockableForm(Control).DockableControl;
      for i := 0 to DockableControl.DockClientCount - 1 do
      begin
//        if not DockableControl.DockClients[i].Visible then Continue;
        DoGetSourceDockClients(DockableControl.DockClients[i]);
      end;
    end else
      FSourceDockClientList.Add(Control);
  end;

begin
  inherited Create(AControl);
  FSourceDockClientList := TList.Create;
  DoGetSourceDockClients(AControl);
  FDropTabControl := nil;
  FIsTabDockOver := False;
  CurrState := dsDragEnter;
  OldState := CurrState;
end;

procedure TCnVIDDragDockObject.GetBrush_PenSize_DrawRect(
  var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);
begin
  if DragTarget = nil then DropAlign := alNone;
  inherited GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);
  FIsTabDockOver := ((FOldDropAlign = alClient) and FErase) or
    ((DropAlign = alClient) and not FErase);
  FOldDropAlign := DropAlign;
  FOldTarget := DragTarget;
end;

{$J+}
procedure TCnVIDDragDockObject.DefaultDockImage(Erase: Boolean);
var
  DesktopWindow: HWND;
  DC: HDC;
  OldBrush: HBrush;
  DrawRect: TRect;
  PenSize: Integer;
  ABrush: TBrush;

  ButtomOffset: Integer;
  MaxTabWidth: Integer;

const
  LeftOffset = 4;

  procedure DoDrawDefaultImage;
  begin
    with DrawRect do
    begin
      PatBlt(DC, Left + PenSize, Top, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Right - PenSize, Top + PenSize, PenSize, Bottom - Top - PenSize, PATINVERT);
      PatBlt(DC, Left, Bottom - PenSize, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Left, Top, PenSize, Bottom - Top - PenSize, PATINVERT);
    end;
  end;

  procedure DoDrawTabImage;
  begin
    with DrawRect do
    begin
      ButtomOffset := 15;
      MaxTabWidth := 30;

      PatBlt(DC, Left + PenSize, Top, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Right - PenSize, Top + PenSize, PenSize, Bottom - Top - 2 * PenSize - ButtomOffset, PATINVERT);

      if DrawRect.Right - DrawRect.Left - 2 * PenSize < LeftOffset + 2 * PenSize + 2 * MaxTabWidth then
        MaxTabWidth := (DrawRect.Right - DrawRect.Left - 4 * PenSize - LeftOffset) div 2;

      if DrawRect.Bottom - DrawRect.Top - 2 * PenSize < 2 * ButtomOffset then
        ButtomOffset := Max((DrawRect.Bottom - DrawRect.Top - 2 * PenSize) div 2, 0);

      PatBlt(DC, Left, Bottom - PenSize - ButtomOffset, 2*PenSize + LeftOffset, PenSize, PATINVERT);
      PatBlt(DC, Left + PenSize + LeftOffset, Bottom - ButtomOffset, PenSize, ButtomOffset, PATINVERT);
      PatBlt(DC, Left + 2*PenSize + LeftOffset, Bottom - PenSize, MaxTabWidth, PenSize, PATINVERT);
      PatBlt(DC, Left + 2*PenSize + LeftOffset + MaxTabWidth, Bottom - PenSize - ButtomOffset, PenSize, PenSize + ButtomOffset, PATINVERT);
      PatBlt(DC, Left + 3*PenSize + LeftOffset + MaxTabWidth, Bottom - PenSize - ButtomOffset, Right - Left - 3*PenSize - LeftOffset - MaxTabWidth, PenSize, PATINVERT);

      PatBlt(DC, Left, Top, PenSize, Bottom - Top - PenSize - ButtomOffset, PATINVERT);
    end;
  end;

begin
  { 获得画刷句柄，画笔宽度和绘画区域 }
  FErase := Erase;
  GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);

  DesktopWindow := GetDesktopWindow;
  DC := GetDCEx(DesktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
  try
    OldBrush := SelectObject(DC, ABrush.Handle);
    if not FIsTabDockOver then
      DoDrawDefaultImage
    else DoDrawTabImage;
    SelectObject(DC, OldBrush);
  finally
    ReleaseDC(DesktopWindow, DC);
  end;
end;
{$J-}

destructor TCnVIDDragDockObject.Destroy;
begin
  FDropTabControl := nil;
  FSourceDockClientList.Free;
  inherited Destroy;
end;

function TCnVIDDragDockObject.DragFindWindow(const Pos: TPoint): HWND;
begin
  Result := 0;
end;

function TCnVIDDragDockObject.GetDropCtl: TControl;
var ARect: TRect;
  i: Integer;
begin
  Result := inherited GetDropCtl;
  if (Result = nil) and (TargetControl is TCnCustomDockPanel) then
  begin
    for i := 0 to TargetControl.DockClientCount - 1 do
    begin
      if TargetControl.DockClients[i].Visible then
      begin
        ARect := TCnCustomDockPanel(DragTarget).CnDockManager.GetFrameRectEx(TargetControl.DockClients[i]);
        if PtInRect(ARect, DragPos) then
        begin
          Result := TargetControl.DockClients[i];
          Exit;
        end;
      end;
    end;
  end;
end;

function TCnVIDDragDockObject.GetSourceDockClient(
  Index: Integer): TControl;
begin
  Result := TControl(FSourceDockClientList[Index]);
end;

function TCnVIDDragDockObject.GetSourceDockClientCount: Integer;
begin
  Result := FSourceDockClientList.Count;
end;

procedure TCnVIDDragDockObject.MouseMsg(var Msg: TMessage);
var APos: TPoint;
  Page: TCnVIDTabPageControl;
begin
  inherited MouseMsg(Msg);
  case Msg.Msg of
    WM_CAPTURECHANGED:
    begin
      if GlobalDockClient.ParentForm.HostDockSite is TCnVIDTabPageControl then
        TCnVIDTabPageControl(GlobalDockClient.ParentForm.HostDockSite).Panel.MouseUp(mbLeft, [], 0, 0)
      else if TWinControl(CnGlobalDockPresident.DragObject.DragTarget) is TCnVIDTabPageControl then
        TCnVIDTabPageControl(CnGlobalDockPresident.DragObject.TargetControl).Panel.MouseUp(mbLeft, [], 0, 0);
    end;
    WM_MOUSEMOVE:
    begin
      if CnGlobalDockPresident.DragObject.TargetControl is TCnVIDTabPageControl then
      begin
        Page := TCnVIDTabPageControl(CnGlobalDockPresident.DragObject.TargetControl);
        if Page.FTempSheet <> nil then
        begin
          APos := Point(TWMMouse(Msg).XPos, TWMMouse(Msg).YPos);
          APos := Page.Panel.ScreenToClient(APos);
          Page.Panel.MouseMove([], APos.X, APos.Y);
        end;
      end;
    end;
  end;
end;

procedure TCnVIDDragDockObject.SetOldState(const Value: TDragState);
begin
  FOldState := Value;
end;

procedure TCnVIDDragDockObject.SetCurrState(const Value: TDragState);
begin
  FCurrState := Value;
end;

function TCnVIDDragDockObject.CanLeave(NewTarget: TWinControl): Boolean;
begin
  Result := inherited CanLeave(NewTarget);
end;

{ TCnVIDDockZone }

{constructor TCnVIDDockZone.Create(Tree: TCnDockTree);
begin
  inherited;
end;}

destructor TCnVIDDockZone.Destroy;
begin
  inherited;
end;

function TCnVIDDockZone.GetSplitterLimit(IsMin: Boolean): Integer;
begin
  if IsMin then
    Result := ZoneLimit
  else Result := LimitBegin;
end;

procedure TCnVIDDockZone.Insert(DockSize: Integer; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderSize: Integer;
    BeforeVisibleZone,
    AfterVisibleZone: TCnDockZone;
    BeginSize: Integer;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := True;
    Exit;
  end;

  if (ParentZone <> nil) and (ParentZone.ChildZones <> nil) then
    BeginSize := ParentZone.ChildZones.LimitBegin
  else BeginSize := 0;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone  := AfterClosestVisibleZone;

  BorderSize := TCnVIDDockTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone <> nil) div 2;

  TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

  Visibled := False;

  if DockSize >= TempSize - (ParentZone.VisibleChildCount) * TCnVIDDockTree(Tree).MinSize then
    DockSize := (TempSize - (ParentZone.VisibleChildCount) * TCnVIDDockTree(Tree).MinSize) div 2;

  if DockSize < TCnVIDDockTree(Tree).MinSize then
    DockSize := TempSize div 2;

  if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
  begin
    PrevShift := 0;
    NextShift := 0;
    ZoneLimit := TempSize + BeginSize;
  end else
  if BeforeVisibleZone = nil then
  begin
    { 要插入的节点是在父节点的最前面 }
    PrevShift := 0;
    NextShift := DockSize + BorderSize;
    ZoneLimit := DockSize + LimitBegin + BorderSize;
    if ParentZone.VisibleChildCount = 1 then
      AfterVisibleZone.ZoneLimit := TempSize + BeginSize;
  end else if AfterVisibleZone = nil then
  begin
    { 要插入的节点是在父节点的最后面 }
    PrevShift := DockSize + BorderSize;
    NextShift := 0;
    if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
      BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
    else
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := TempSize + BeginSize;
  end else
  begin
    { 要插入的节点是在父节点的中间 }
    PrevShift := Round((BeforeVisibleZone.ZoneLimit - BeginSize) * (DockSize + BorderSize) / TempSize);
    NextShift := DockSize - PrevShift;
    if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
      BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
    else
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize;
  end;

  { 如果新节点有上一个兄弟节点 }
  if PrevShift <> 0 then
  begin
    with TCnVIDDockTree(Tree) do
    begin
      { 当遍历到新节点的上一个兄弟节点的时候，就停止遍历 }
      ReplacementZone := BeforeVisibleZone;
      try
        if (BeforeVisibleZone.ZoneLimit - BeginSize) * (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift) <> 0 then
          ScaleBy := (BeforeVisibleZone.ZoneLimit - BeginSize) / (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift)
        else ScaleBy := 1;
        ParentLimit := BeginSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(ParentZone.ChildZones, ScaleChildZone, tskMiddle, tspChild);
      finally
        ReplacementZone := nil;
      end;
    end;
    { 对PrevSibling的ZoneLimit进行调整 }
    if BeforeVisibleZone.LimitSize < TCnVIDDockTree(Tree).MinSize then
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TCnVIDDockTree(Tree).MinSize;
  end;


  { 如果新节点有下一个兄弟节点 }
  if NextShift <> 0 then
  begin
    with TCnVIDDockTree(Tree) do
    begin
      if (TempSize + BeginSize - LimitBegin - NextShift) * (TempSize + BeginSize - LimitBegin) <> 0 then
        ScaleBy := (TempSize + BeginSize - LimitBegin - NextShift) / (TempSize + BeginSize - LimitBegin)
      else ScaleBy := 1;
      ParentLimit := TempSize + BeginSize;
      ShiftScaleOrient := ParentZone.Orientation;
      if ScaleBy <> 1 then
        ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
    end;
  end;
  Visibled := True;
end;

procedure TCnVIDDockZone.Remove(DockSize: Integer; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderSize: Integer;
    BeforeVisibleZone,
    AfterVisibleZone: TCnDockZone;
    BeginSize: Integer;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 1) and (ParentZone <> Tree.TopZone) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := False;
    Exit;
  end;

  if (ParentZone <> nil) and (ParentZone.ChildZones <> nil) then
    BeginSize := ParentZone.ChildZones.LimitBegin
  else BeginSize := 0;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone  := AfterClosestVisibleZone;

  BorderSize := TCnVIDDockTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone <> nil) div 2;

  TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

  if DockSize > TempSize - (ParentZone.VisibleChildCount-1) * TCnVIDDockTree(Tree).MinSize then
    DockSize := TempSize - (ParentZone.VisibleChildCount-1) * TCnVIDDockTree(Tree).MinSize;

  if DockSize = 0 then
    DockSize := TempSize div 2;

  Visibled := False;

  if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
    Exit;

  if BeforeVisibleZone = nil then
  begin
    { 要插入的节点是在父节点的最前面 }
    PrevShift := 0;
    NextShift := -DockSize + BorderSize;
    ZoneLimit := -DockSize + BorderSize + BeginSize;
  end else if AfterVisibleZone = nil then
  begin
    { 要插入的节点是在父节点的最后面 }
    PrevShift := -DockSize + BorderSize;
    NextShift := 0;
    BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := TempSize + BeginSize;
  end else
  begin
    { 要插入的节点是在父节点的中间 }
    PrevShift := -Round((BeforeVisibleZone.ZoneLimit - BeginSize) * (DockSize + BorderSize) / (TempSize - (DockSize + BorderSize)));
    NextShift := -DockSize - PrevShift;
    BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
    ZoneLimit := BeforeVisibleZone.ZoneLimit;
  end;

  { 如果新节点有上一个兄弟节点 }
  if PrevShift <> 0 then
  begin
    with TCnVIDDockTree(Tree) do
    begin
      { 当遍历到新节点的上一个兄弟节点的时候，就停止遍历 }
      ReplacementZone := BeforeVisibleZone;
      try
        if (BeforeVisibleZone.ZoneLimit - BeginSize)*(BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift) <> 0 then
          ScaleBy := (BeforeVisibleZone.ZoneLimit - BeginSize) / (BeforeVisibleZone.ZoneLimit - BeginSize + PrevShift)
        else ScaleBy := 1;
        ParentLimit := BeginSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(ParentZone.ChildZones, ScaleChildZone, tskMiddle, tspChild);
      finally
        ReplacementZone := nil;
      end;
    end;
    { 对PrevSibling的ZoneLimit进行调整 }
    if BeforeVisibleZone.LimitSize < TCnVIDDockTree(Tree).MinSize then
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TCnVIDDockTree(Tree).MinSize;
  end;


  { 如果新节点有下一个兄弟节点 }
  if NextShift <> 0 then
  begin
    with TCnVIDDockTree(Tree) do
    begin
      if (TempSize + BeginSize - LimitBegin) * (TempSize + BeginSize - LimitBegin + NextShift) <> 0 then
        ScaleBy := (TempSize + BeginSize - LimitBegin) / (TempSize + BeginSize - LimitBegin + NextShift)
      else ScaleBy := 1;
      ParentLimit := TempSize + BeginSize;
      ShiftScaleOrient := ParentZone.Orientation;
      if ScaleBy <> 1 then
        ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
    end;
  end;
end;

{ TCnVIDTabServerOption }

procedure TCnVIDTabServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnVIDTabServerOption then
  begin
    FActiveFont.Assign(TCnVIDTabServerOption(Source).FActiveFont);
    FActiveSheetColor := TCnVIDTabServerOption(Source).FActiveSheetColor;
    FHotTrackColor := TCnVIDTabServerOption(Source).FHotTrackColor;
    FInactiveFont.Assign(TCnVIDTabServerOption(Source).FInactiveFont);
    FInactiveSheetColor := TCnVIDTabServerOption(Source).FInactiveSheetColor;
    FShowTabImages := TCnVIDTabServerOption(Source).FShowTabImages;
  end;
  inherited Assign(Source);
end;

constructor TCnVIDTabServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  inherited Create(ADockStyle);
  TabPosition := tpBottom;
  FActiveFont := TFont.Create;
  FActiveSheetColor := clBtnFace;
  FHotTrackColor := clBlue;
  FInactiveFont := TFont.Create;
  FInactiveFont.Color := clWhite;
  FInactiveSheetColor := clBtnShadow;
  FShowTabImages := False;
end;

destructor TCnVIDTabServerOption.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  inherited;
end;

function TCnVIDTabServerOption.GetActiveFont: TFont;
begin
  Result := FActiveFont;
end;

function TCnVIDTabServerOption.GetActiveSheetColor: TColor;
begin
  Result := FActiveSheetColor;
end;

function TCnVIDTabServerOption.GetHotTrackColor: TColor;
begin
  Result := FHotTrackColor;
end;

function TCnVIDTabServerOption.GetInactiveFont: TFont;
begin
  Result := FInactiveFont;
end;

function TCnVIDTabServerOption.GetInactiveSheetColor: TColor;
begin
  Result := FInactiveSheetColor;
end;

function TCnVIDTabServerOption.GetShowTabImages: Boolean;
begin
  Result := FShowTabImages;
end;

procedure TCnVIDTabServerOption.ResetDockControlOption;
begin
  inherited;

end;

procedure TCnVIDTabServerOption.ResetTabPageControl(
  APage: TCnTabPageControl);
begin
  inherited;
  if APage is TCnVIDTabPageControl then
  begin
    TCnVIDTabPageControl(APage).ActiveFont := ActiveFont;
    TCnVIDTabPageControl(APage).ActiveSheetColor := ActiveSheetColor;
    TCnVIDTabPageControl(APage).HotTrackColor := HotTrackColor;
    TCnVIDTabPageControl(APage).InactiveFont := InactiveFont;
    TCnVIDTabPageControl(APage).InactiveSheetColor := InactiveSheetColor;
    TCnVIDTabPageControl(APage).ShowTabImages := ShowTabImages;
    TCnVIDTabPageControl(APage).TabPosition := TabPosition;
  end;
end;

procedure TCnVIDTabServerOption.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
  ResetDockControlOption;
end;

procedure TCnVIDTabServerOption.SetActiveSheetColor(const Value: TColor);
begin
  if FActiveSheetColor <> Value then
  begin
    FActiveSheetColor := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDTabServerOption.SetHotTrackColor(const Value: TColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDTabServerOption.SetInactiveFont(const Value: TFont);
begin
  FInactiveFont.Assign(Value);
  ResetDockControlOption;
end;

procedure TCnVIDTabServerOption.SetInactiveSheetColor(const Value: TColor);
begin
  if FInactiveSheetColor <> Value then
  begin
    FInactiveSheetColor := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDTabServerOption.SetShowTabImages(const Value: Boolean);
begin
  if FShowTabImages <> Value then
  begin
    FShowTabImages := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDTabServerOption.SetTabPosition(const Value: TTabPosition);
begin
  if Value = tpBottom then
    inherited SetTabPosition(Value)
  else
    raise Exception.Create(gs_TabPositionMustBetpBottom);
end;

{ TCnVIDConjoinServerOption }

procedure TCnVIDConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnVIDConjoinServerOption then
  begin
    FTextEllipsis := TCnVIDConjoinServerOption(Source).FTextEllipsis;
    FTextAlignment := TCnVIDConjoinServerOption(Source).FTextAlignment;
    FInactiveTitleEndColor := TCnVIDConjoinServerOption(Source).FInactiveTitleEndColor;
    FInactiveTitleStartColor := TCnVIDConjoinServerOption(Source).FInactiveTitleStartColor;
    FActiveTitleEndColor := TCnVIDConjoinServerOption(Source).FActiveTitleEndColor;
    FActiveTitleStartColor := TCnVIDConjoinServerOption(Source).FActiveTitleStartColor;
    FActiveFont.Assign(TCnVIDConjoinServerOption(Source).FActiveFont);
    FInactiveFont.Assign(TCnVIDConjoinServerOption(Source).FInactiveFont);
    FSystemInfo := TCnVIDConjoinServerOption(Source).FSystemInfo;
  end;
  inherited Assign(Source);
end;

constructor TCnVIDConjoinServerOption.Create(
  ADockStyle: TCnBasicDockStyle);
begin
  inherited Create(ADockStyle);
  GrabbersSize := 18;
  FActiveFont := TFont.Create;
  FInactiveFont := TFont.Create;
  SystemInfo := True;
end;

destructor TCnVIDConjoinServerOption.Destroy;
begin
  FActiveFont.Free;
  FInactiveFont.Free;
  inherited;
end;

procedure TCnVIDConjoinServerOption.SetActiveTitleEndColor(
  const Value: TColor);
begin
  if FActiveTitleEndColor <> Value then
  begin
    FActiveTitleEndColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDConjoinServerOption.SetActiveTitleStartColor(
  const Value: TColor);
begin
  if FActiveTitleStartColor <> Value then
  begin
    FActiveTitleStartColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDConjoinServerOption.SetInactiveTitleEndColor(
  const Value: TColor);
begin
  if FInactiveTitleEndColor <> Value then
  begin
    FInactiveTitleEndColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDConjoinServerOption.SetInactiveTitleStartColor(
  const Value: TColor);
begin
  if FInactiveTitleStartColor <> Value then
  begin
    FInactiveTitleStartColor := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDConjoinServerOption.SetSystemInfo(const Value: Boolean);
begin
  if FSystemInfo <> Value then
  begin
    FSystemInfo := Value;
    if FSystemInfo then
      SetDefaultSystemCaptionInfo;

    ResetDockControlOption;
  end;
end;

procedure TCnVIDConjoinServerOption.SetTextAlignment(
  const Value: TAlignment);
begin
  if FTextAlignment <> Value then
  begin
    FTextAlignment := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDConjoinServerOption.SetTextEllipsis(const Value: Boolean);
begin
  if FTextEllipsis <> Value then
  begin
    FTextEllipsis := Value;
    FSystemInfo := False;
    ResetDockControlOption;
  end;
end;

procedure TCnVIDConjoinServerOption.SetDefaultSystemCaptionInfo;
begin
  FActiveTitleStartColor    := Cn_GetActiveTitleBeginColor;
  FActiveTitleEndColor      := Cn_GetActiveTitleEndColor;
  FInactiveTitleStartColor  := Cn_GetInactiveTitleBeginColor;
  FInactiveTitleEndColor    := Cn_GetInactiveTitleEndColor;
  FTextAlignment            := taLeftJustify;
  FTextEllipsis             := True;
  FActiveFont.Assign(Cn_GetTitleFont);
  FActiveFont.Style         := FActiveFont.Style + [fsBold];
  FInactiveFont.Assign(FActiveFont);
  FActiveFont.Color         := Cn_GetActiveTitleFontColor;
  FInactiveFont.Color       := Cn_GetInactiveTitleFontColor;
  GrabbersSize              := VIDDefaultGrabbersSize;
  SplitterWidth             := VIDDefaultSplitterWidth;
end;

procedure TCnVIDConjoinServerOption.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
  FSystemInfo := False;
  ResetDockControlOption;
end;

procedure TCnVIDConjoinServerOption.SetInactiveFont(const Value: TFont);
begin
  FInactiveFont.Assign(Value);
  FSystemInfo := False;
  ResetDockControlOption;
end;

procedure TCnVIDConjoinServerOption.ResetDockControlOption;
begin
  inherited ResetDockControlOption;
  FSystemInfo := FSystemInfo and (GrabbersSize = VIDDefaultGrabbersSize)
                  and (SplitterWidth = VIDDefaultSplitterWidth);
  TCnVIDDockStyle(DockStyle).DoSystemInfoChange(FSystemInfo);
end;

procedure TCnVIDConjoinServerOption.SetActiveFont_WithoutChangeSystemInfo(
  const Value: TFont);
begin
  FActiveFont.Assign(Value);
end;

procedure TCnVIDConjoinServerOption.SetActiveTitleEndColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FActiveTitleEndColor := Value;
end;

procedure TCnVIDConjoinServerOption.SetActiveTitleStartColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FActiveTitleStartColor := Value;
end;

procedure TCnVIDConjoinServerOption.SetInactiveFont_WithoutChangeSystemInfo(
  const Value: TFont);
begin
  FInactiveFont.Assign(Value);
end;

procedure TCnVIDConjoinServerOption.SetInactiveTitleEndColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FInactiveTitleEndColor := Value;
end;

procedure TCnVIDConjoinServerOption.SetInactiveTitleStartColor_WithoutChangeSystemInfo(
  const Value: TColor);
begin
  FInactiveTitleStartColor := Value;
end;

procedure TCnVIDConjoinServerOption.SetTextAlignment_WithoutChangeSystemInfo(
  const Value: TAlignment);
begin
  FTextAlignment := Value;
end;

procedure TCnVIDConjoinServerOption.SetTextEllipsis_WithoutChangeSystemInfo(
  const Value: Boolean);
begin
  FTextEllipsis := Value;
end;

end.
