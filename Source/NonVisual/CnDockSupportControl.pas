{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2012 CnPack 开发组                       }
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
{       停靠的基础控件                                  }
{       CnDockSupportControl 单元                       }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnDockSupportControl;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：停靠的基础控件单元 
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2007.07.13 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Messages, Windows, SysUtils, CommCtrl, Controls, Forms, Classes, ComCtrls,
  Graphics, ImgList, ExtCtrls, CnDockHashTable, CnDockTree;

type

  TCnDragDockObject = class(TObject)
  private
    FMouseDeltaX: Double;
    FMouseDeltaY: Double;
    FControl: TControl;
    FDragTarget: Pointer;
    FDragPos: TPoint;
    FDropOnControl: TControl;
    FDropAlign: TAlign;
    FDragHandle: HWND;
    FDragTargetPos: TPoint;
    FCancelling: Boolean;
    FFloating: Boolean;
    FFrameWidth: Integer;
    FBrush: TBrush;
    FCtrlDown: Boolean;
    procedure SetBrush(const Value: TBrush);
    procedure SetDropAlign(const Value: TAlign);
    procedure SetDropOnControl(const Value: TControl);
    function GetTargetControl: TWinControl;
    procedure SetTargetControl(const Value: TWinControl);
  protected
    procedure DefaultDockImage(Erase: Boolean); virtual;
    procedure DrawDragRect(DoErase: Boolean); virtual;
    procedure GetBrush_PenSize_DrawRect(
      var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean); virtual;
    function GetFrameWidth: Integer; virtual;
    procedure SetFrameWidth(const Value: Integer); virtual;
    procedure MouseMsg(var Msg: TMessage); virtual;
    function CanLeave(NewTarget: TWinControl): Boolean; virtual;
  public
    DockRect: TRect;
    EraseDockRect: TRect;
    constructor Create(AControl: TControl); virtual;
    destructor Destroy; override;
    procedure AdjustDockRect(var ARect: TRect); virtual;
    { ------------------------------------------------------------------------ }
    function Capture: HWND;
    { 查找鼠标位置的控件句柄 }
    function DragFindWindow(const Pos: TPoint): HWND; virtual;
    procedure ReleaseCapture(Handle: HWND);
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
    function GetDragImages: TDragImageList; virtual;
    { ------------------------------------------------------------------------ }
    procedure DrawDragDockImage; virtual;
    procedure EraseDragDockImage; virtual;
    function GetDropCtl: TControl; virtual;
    property MouseDeltaX: Double read FMouseDeltaX write FMouseDeltaX;
    property MouseDeltaY: Double read FMouseDeltaY write FMouseDeltaY;
    property Control: TControl read FControl write FControl;
    property DragTarget: Pointer read FDragTarget write FDragTarget;
    property DragPos: TPoint read FDragPos write FDragPos;
    property DropOnControl: TControl read FDropOnControl write SetDropOnControl;
    property DropAlign: TAlign read FDropAlign write SetDropAlign;
    property DragHandle: HWND read FDragHandle write FDragHandle;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
    property Cancelling: Boolean read FCancelling write FCancelling;
    property Floating: Boolean read FFloating write FFloating;
    property FrameWidth: Integer read GetFrameWidth write SetFrameWidth;
    property Brush: TBrush read FBrush write SetBrush;
    property CtrlDown: Boolean read FCtrlDown write FCtrlDown;
    property TargetControl: TWinControl read GetTargetControl write SetTargetControl; 
  end;

  TCnCustomDockControl = class(TCustomControl)
  private
    function GetCnDockManager: ICnDockManager;
    procedure SetCnDockManager(const Value: ICnDockManager);
  protected
    procedure WndProc(var Message: TMessage); override;
    { ------------------------------------------------------------------------ }
    procedure CustomStartDock(var Source: TCnDragDockObject); virtual;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); virtual;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); virtual;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); virtual;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); virtual;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    { ------------------------------------------------------------------------ }
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); virtual;
  public
    procedure UpdateCaption(Exclude: TControl); virtual;
    property DockManager;
    property CnDockManager: ICnDockManager read GetCnDockManager write SetCnDockManager;
  end;

  TCnCustomDockPanel = class(TCnCustomDockControl)
  protected
    function CreateDockManager: IDockManager; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DockSite;
  end;

  TCnDockCustomTabControl = class;

  TCnDockDrawTabEvent = procedure(Control: TCnDockCustomTabControl; TabIndex: Integer;
    const Rect: TRect; Active: Boolean) of object;

  TCnDockPageControl = class;

  TCnDockCustomTabControl = class(TCnCustomDockControl)
  private
    FHotTrack: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FMultiLine: Boolean;
    FMultiSelect: Boolean;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FSaveTabIndex: Integer;
    FSaveTabs: TStringList;
    FScrollOpposite: Boolean;
    FStyle: TTabStyle;
    FTabPosition: TTabPosition;
    FTabs: TStrings;
    FTabSize: TSmallPoint;
    FUpdating: Boolean;
    FSavedAdjustRect: TRect;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnDrawTab: TCnDockDrawTabEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    function GetDisplayRect: TRect;
    function GetTabIndex: Integer;
    procedure ImageListChange(Sender: TObject);
    function InternalSetMultiLine(Value: Boolean): Boolean;
    procedure SetMultiLine(Value: Boolean);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetOwnerDraw(Value: Boolean);
    procedure SetRaggedRight(Value: Boolean);
    procedure SetScrollOpposite(Value: Boolean);
    procedure SetStyle(Value: TTabStyle);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabs(Value: TStrings);
    procedure SetTabWidth(Value: Smallint);
    procedure TabsChanged;
    procedure UpdateTabSize;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure TCMAdjustRect(var Message: TMessage); message TCM_ADJUSTRECT;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMNotifyFormat(var Message: TMessage); message WM_NOTIFYFORMAT;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    function CanChange: Boolean; dynamic;
    function CanShowTab(TabIndex: Integer): Boolean; virtual;
    procedure Change; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); virtual;
    function GetImageIndex(TabIndex: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetHotTrack(Value: Boolean); virtual;
    procedure SetImages(Value: TCustomImageList); virtual;
    procedure SetTabHeight(Value: Smallint); virtual;
    procedure SetTabPosition(Value: TTabPosition); virtual;
    procedure UpdateTabImages;
    property DisplayRect: TRect read GetDisplayRect;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw default False;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight default False;
    property ScrollOpposite: Boolean read FScrollOpposite
      write SetScrollOpposite default False;
    property Style: TTabStyle read FStyle write SetStyle default tsTabs;
    property TabHeight: Smallint read FTabSize.Y write SetTabHeight default 0;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
      default tpTop;
    property Tabs: TStrings read FTabs write SetTabs;
    property TabWidth: Smallint read FTabSize.X write SetTabWidth default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TCnDockDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex write FOnGetImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOfTabAt(X, Y: Integer): Integer;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function TabRect(Index: Integer): TRect;
    function RowCount: Integer;
    procedure ScrollTabs(Delta: Integer);
    property TabStop default True;
  end;

  TCnDockTabSheet = class(TWinControl)
  private
    FImageIndex: TImageIndex;
    FPageControl: TCnDockPageControl;
    FTabVisible: Boolean;
    FTabShowing: Boolean;
    FHighlighted: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetPageIndex: Integer;
    function GetTabIndex: Integer;
    procedure SetHighlighted(Value: Boolean);
    procedure SetImageIndex(Value: TImageIndex);
    procedure SetPageIndex(Value: Integer);
    procedure SetTabShowing(Value: Boolean);
    procedure SetTabVisible(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetPageControl(APageControl: TCnDockPageControl); virtual;
    procedure ReadState(Reader: TReader); override;
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
    procedure UpdateTabShowing; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TCnDockPageControl read FPageControl write SetPageControl;
    property TabIndex: Integer read GetTabIndex;
  published
    property Caption;
    property Height stored False;
    property Highlighted: Boolean read FHighlighted write SetHighlighted default False;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default 0;
    property Left stored False;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default True;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TCnDockTabSheetClass = class of TCnDockTabSheet;

  TCnDockPageControl = class(TCnDockCustomTabControl)
  private
    FPages: TList;
    FActivePage: TCnDockTabSheet;
    FNewDockSheet: TCnDockTabSheet;
    FUndockingPage: TCnDockTabSheet;
    FCnDockTabSheetClass: TCnDockTabSheetClass;
    procedure ChangeActivePage(Page: TCnDockTabSheet);
    procedure DeleteTab(Page: TCnDockTabSheet; Index: Integer);
    function GetActivePageIndex: Integer;
    function GetPage(Index: Integer): TCnDockTabSheet;
    function GetPageCount: Integer;
    procedure InsertPage(Page: TCnDockTabSheet);
    procedure InsertTab(Page: TCnDockTabSheet);
    procedure MoveTab(CurIndex, NewIndex: Integer);
    procedure RemovePage(Page: TCnDockTabSheet);
    procedure SetActivePageIndex(const Value: Integer);
    procedure UpdateTab(Page: TCnDockTabSheet);
    procedure UpdateTabHighlights;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;

    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonDblClk(var Message: TWMRButtonDblClk); message WM_RBUTTONDBLCLK;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;

    procedure WMMButtonDown(var Message: TWMMButtonDown); message WM_MBUTTONDOWN;
    procedure WMMButtonDblClk(var Message: TWMMButtonDblClk); message WM_MBUTTONDBLCLK;
    procedure WMMButtonUp(var Message: TWMMButtonUp); message WM_MBUTTONUP;

  protected
    function CanShowTab(TabIndex: Integer): Boolean; override;
    procedure Change; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function DoMouseEvent(var Message: TWMMouse; Control: TControl): TWMNCHitMessage; virtual;
    procedure DoRemoveDockClient(Client: TControl); override;

    function GetDockClientFromMousePos(MousePos: TPoint): TControl; virtual;
    function GetImageIndex(TabIndex: Integer): Integer; override;
    function GetPageFromDockClient(Client: TControl): TCnDockTabSheet;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    procedure Loaded; override;
    procedure SetActivePage(Page: TCnDockTabSheet); virtual;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateActivePage; virtual;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TCnDockTabSheet;
      GoForward, CheckTabVisible: Boolean): TCnDockTabSheet;
    procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    property ActivePage: TCnDockTabSheet read FActivePage write SetActivePage;
    property ActivePageIndex: Integer read GetActivePageIndex
      write SetActivePageIndex;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TCnDockTabSheet read GetPage;
    property PageSheets: TList read FPages;
    property CnDockTabSheetClass: TCnDockTabSheetClass read FCnDockTabSheetClass
      write FCnDockTabSheetClass;
  end;

  TCnDockTabStrings = class(TStrings)
  private
    FTabControl: TCnDockCustomTabControl;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

  TDragOperation = (dopNone, dopDrag, dopDock);

  PSiteInfoRec = ^TSiteInfoRec;
  TSiteInfoRec = record
    Site: TWinControl;
    TopParent: HWND;
  end;

  TSiteList = class(TList)
  public
    procedure AddSite(ASite: TWinControl);
    procedure Clear; override;
    function Find(ParentWnd: Hwnd; var Index: Integer): Boolean;
    function GetTopSite: TWinControl;
  end;

  TCnDockPresident  = class(TObject)
  private
    FDockServersList: TList;                   //存放停靠服务器的列表
    FDockClientsList: TList;                   //存放停靠客户的列表
    FDockServersHash: TCnDockControlHashTable; //存放停靠服务器的散列
    FDockClientsHash: TCnDockControlHashTable; //存放停靠客户的散列
    FDockableFormList: TList;                  //所有停靠服务器的列表
    FLoadCount: Integer;
    FSaveCount: Integer;
    procedure BeginLoad;
    procedure EndLoad;
    procedure BeginSave;
    procedure EndSave;
  public
    { ------------------------------------------------------------------------ }
    DragControl: TControl;
    DragObject: TCnDragDockObject;
    DragFreeObject: Boolean;
    DragCapture: HWND;
    DragStartPos: TPoint;
    DragSaveCursor: HCURSOR;
    DragThreshold: Integer;
    ActiveDrag: TDragOperation;
    DragImageList: TDragImageList;
    DockSiteList: TList;
    QualifyingSites: TSiteList;
    { ------------------------------------------------------------------------ }
    procedure CalcDockSizes(Control: TControl);
//    constructor Create(AOwner: TComponent); override;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddDockServerToDockManager(AControl: TControl);
    procedure AddDockClientToDockManager(AControl: TControl);
    procedure RemoveDockServerFromDockManager(AControl: TControl);
    procedure RemoveDockClientFromDockManager(AControl: TControl);
    function FindDockServerForm(AName: string): TControl;
    function FindDockClientForm(AName: string): TControl;
    function FindDockControlForm(AName: string): TControl;
    function IsLoading: Boolean;
    function IsSaving: Boolean;
    { 显示窗体 }
    procedure ShowDockForm(DockWindow: TWinControl);
    { 隐藏窗体 }
    procedure HideDockForm(DockWindow: TControl);
    { 获得窗体的可见性 }
    function GetFormVisible(DockWindow: TWinControl): Boolean;
    { 设置分页的服务窗体的显示风格 }
    procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
    { 设置平铺的服务窗体的显示风格 }
    procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
    { 把停靠窗体的信息保存进文件 }
    procedure SaveDockTreeToFile(FileName: string);
    { 从文件里读出停靠窗体的信息 }
    procedure LoadDockTreeFromFile(FileName: string);
    { 把停靠窗体的信息保存进注册表 }
    procedure SaveDockTreeToReg(RootKey: DWORD; RegPath: string);
    { 从注册表里读出停靠窗体的信息 }
    procedure LoadDockTreeFromReg(RootKey: DWORD; RegPath: string);
    { 当鼠标左键按下的时候，开始拖动 }
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); virtual;
    { 初始化停靠控件 }
    procedure DragInitControl(Control: TControl;
      Immediate: Boolean; Threshold: Integer); virtual;
    { 初始化拖动 }
    procedure DragInit(ADragObject: TCnDragDockObject;
      Immediate: Boolean; Threshold: Integer); virtual;

    { 当鼠标移动的时候，调用DragTo }
    procedure DragTo(const Pos: TPoint); virtual;
    { 当鼠标释放的时候，调用DragDone }
    procedure DragDone(Drop: Boolean); virtual;
    { 取消拖动操作 }
    procedure CancelDrag; virtual;
    { 重新设置光标的形状 }
    procedure ResetCursor; virtual;

    { 查找可能的停靠服务器，Handle是这个服务器的句柄 }
    function DragFindTarget(const Pos: TPoint; var Handle: HWND;
      DragKind: TDragKind; Client: TControl): Pointer; virtual;
    { 调用控件的OnxxxGetSiteInfo方法 }
    procedure DoGetSiteInfo(Target, Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); virtual;
    { 调用控件的OnxxxDockOver方法 }
    function DoDockOver(DragState: TDragState): Boolean; virtual;
    { 调用控件的OnxxxDockDrop方法 }
    procedure DoDockDrop(Source: TCnDragDockObject; Pos: TPoint); virtual;
    { 调用控件的OnxxxUnDock方法 }
    function DoUnDock(Source: TCnDragDockObject; Target: TWinControl; Client: TControl): Boolean; virtual;
    { 调用控件的OnxxxEndDock方法 }
    procedure DoEndDrag(Target: TObject; X, Y: Integer); virtual;
    { 查找鼠标位置的控件句柄 }
    function DragFindWindow(const Pos: TPoint): HWND; virtual;
    { 获得鼠标位置的控件对象 }
    function GetDockSiteAtPos(MousePos: TPoint; Client: TControl): TWinControl; virtual;
    { 获得鼠标位置的控件的排列位置 }
    procedure DoGetDockEdge(Target: TControl; MousePos: TPoint; var DropAlign: TAlign); virtual;

    { 注册停靠服务器 }
    procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean); virtual;

    property DockServersList: TList read FDockServersList;
    property DockClientsList: TList read FDockClientsList;
    property DockServersHash: TCnDockControlHashTable
      read FDockServersHash;
    property DockClientsHash: TCnDockControlHashTable
      read FDockClientsHash;
    property DockableFormList: TList read FDockableFormList;
  end;

  TCustomDockPanelSplitter = class(TCustomControl)
  private
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
    procedure FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ReleaseLineDC;
    procedure SetBeveled(Value: Boolean);
    procedure UpdateControlSize;
    procedure UpdateSize(X, Y: Integer);
  protected
    function CanResize(var NewSize: Integer): Boolean; reintroduce; virtual;
    function DoCanResize(var NewSize: Integer): Boolean; virtual;
    function FindControl: TControl; virtual;
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
  published
    property Align default alLeft;
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

implementation

uses
  Consts, ComStrs, CnDockGlobal, CnDockFormControl, CnDockSupportProc;

type

  TCnNCButtonProc = procedure(Message: TWMNCHitMessage; Button: TMouseButton;
    MouseStation: TMouseStation) of object;

function ButtonEvent(Page: TCnDockPageControl; Message: TWMMouse;
  Button: TMouseButton; MouseStation: TMouseStation; Proc: TCnNCButtonProc): TControl;
begin
  { 查找到鼠标所在位置的TCnDockTabSheet上的Control }
  Result := Page.GetDockClientFromMousePos(SmallPointToPoint(Message.Pos));
  if (Result <> nil) and Assigned(Proc) then
  begin
    { 查找到DockCtl上的TCnDockClient，然后把她赋值给全局变量GlobalDockClient }
    GlobalDockClient := FindDockClient(Result);
    { 调用全局变量GlobalDockClient的DoNCButtonDown方法 }
    Proc(Page.DoMouseEvent(Message, Page), Button, MouseStation);
  end;
end;

type

  TCnControlAccess = class(TControl);
  TCnWinControlAccess = class(TWinControl);

  PCheckTargetInfo = ^TCheckTargetInfo;
  TCheckTargetInfo = record
    ClientWnd, TargetWnd: HWnd;
    CurrentWnd: HWnd;
    MousePos: TPoint;
    Found: Boolean;
  end;

procedure TabControlError(const S: string);
begin
  raise EListError.Create(S);
end;

procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean);
var
  Style: Integer;
begin
  if Ctl.HandleAllocated then
  begin
    Style := GetWindowLong(Ctl.Handle, GWL_STYLE);
    if not UseStyle then Style := Style and not Value
    else Style := Style or Value;
    SetWindowLong(Ctl.Handle, GWL_STYLE, Style);
  end;
end;

function IsBeforeTargetWindow(Window: HWnd; Data: Longint): Bool; stdcall;
var
  R: TRect;
begin
  if Window = PCheckTargetInfo(Data)^.TargetWnd then
    Result := False
  else
  begin
    if PCheckTargetInfo(Data)^.CurrentWnd = 0 then
    begin
      GetWindowRect(Window, R);
      if PtInRect(R, PCheckTargetInfo(Data)^.MousePos) then
        PCheckTargetInfo(Data)^.CurrentWnd := Window;
    end;
    if Window = PCheckTargetInfo(Data)^.CurrentWnd then
    begin
      Result := False;
      PCheckTargetInfo(Data)^.Found := True;
    end
    else if Window = PCheckTargetInfo(Data)^.ClientWnd then
    begin
      Result := True;
      PCheckTargetInfo(Data)^.CurrentWnd := 0; // Look for next window
    end
    else
      Result := True;
  end;
end;

{ TCnDragDockObject }

procedure TCnDragDockObject.AdjustDockRect(var ARect: TRect);
var
  DeltaX, DeltaY: Integer;

  function AbsMin(Value1, Value2: Integer): Integer;
  begin
    if Abs(Value1) < Abs(Value2) then Result := Value1
    else Result := Value2;
  end;

begin
  { Make sure dock rect is touching mouse point }
  if (ARect.Left > FDragPos.x) or (ARect.Right < FDragPos.x) then
    DeltaX := AbsMin(ARect.Left - FDragPos.x, ARect.Right - FDragPos.x)
  else DeltaX := 0;
  if (ARect.Top > FDragPos.y) or (ARect.Bottom < FDragPos.y) then
    DeltaY := AbsMin(ARect.Top - FDragPos.y, ARect.Bottom - FDragPos.y)
  else DeltaY := 0;
  if (DeltaX <> 0) or (DeltaY <> 0) then
    OffsetRect(DockRect, -DeltaX, -DeltaY);
end;

function TCnDragDockObject.CanLeave(NewTarget: TWinControl): Boolean;
begin
  Result := NewTarget <> TWinControl(FDragTarget);
end;

function TCnDragDockObject.Capture: HWND;
begin
  Result := AllocateHWND(MouseMsg);
  SetCapture(Result);
end;

constructor TCnDragDockObject.Create(AControl: TControl);
begin
  FControl := AControl;
  FBrush := TBrush.Create;
  FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  FFrameWidth := 4;
  FCtrlDown := False;
end;

procedure TCnDragDockObject.DefaultDockImage(Erase: Boolean);
var
  DesktopWindow: HWND;
  DC: HDC;
  OldBrush: HBrush;
  DrawRect: TRect;
  PenSize: Integer;
  ABrush: TBrush;
begin
  { 获得画刷句柄，画笔宽度和绘画区域 }
  GetBrush_PenSize_DrawRect(ABrush, PenSize, DrawRect, Erase);

  DesktopWindow := GetDesktopWindow;
  DC := GetDCEx(DesktopWindow, 0, DCX_CACHE or DCX_LOCKWINDOWUPDATE);
  try
    OldBrush := SelectObject(DC, ABrush.Handle);
    with DrawRect do
    begin
      PatBlt(DC, Left + PenSize, Top, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Right - PenSize, Top + PenSize, PenSize, Bottom - Top - PenSize, PATINVERT);
      PatBlt(DC, Left, Bottom - PenSize, Right - Left - PenSize, PenSize, PATINVERT);
      PatBlt(DC, Left, Top, PenSize, Bottom - Top - PenSize, PATINVERT);
    end;
    SelectObject(DC, OldBrush);
  finally
    ReleaseDC(DesktopWindow, DC);
  end;
end;

destructor TCnDragDockObject.Destroy;
begin
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
  inherited;

end;

function TCnDragDockObject.DragFindWindow(const Pos: TPoint): HWND;
var WinControl: TWinControl;
begin
{  Result := WindowFromPoint(Pos);
  while Result <> 0 do
    if not IsDelphiHandle(Result) then Result := GetParent(Result)
    else Exit;}
//    FindControl
  WinControl := FindVCLWindow(Pos);
  if WinControl <> nil then
    Result := WinControl.Handle
  else Result := 0;
end;

procedure TCnDragDockObject.DrawDragDockImage;
begin
  DefaultDockImage(False);
end;

procedure TCnDragDockObject.DrawDragRect(DoErase: Boolean);
begin
  if not CompareMem(@DockRect, @EraseDockRect, SizeOf(TRect)) then
  begin
    if DoErase then EraseDragDockImage;
    DrawDragDockImage;
    EraseDockRect := DockRect;
  end;
end;

procedure TCnDragDockObject.EndDrag(Target: TObject; X, Y: Integer);
begin
  CnGlobalDockPresident.DoEndDrag(Target, X, Y);
end;

procedure TCnDragDockObject.EraseDragDockImage;
begin
  DefaultDockImage(True);
end;

procedure TCnDragDockObject.Finished(Target: TObject; X, Y: Integer;
  Accepted: Boolean);
begin
  if not Accepted then
  begin
    Target := nil;
  end;
  EndDrag(Target, X, Y);
end;

procedure TCnDragDockObject.GetBrush_PenSize_DrawRect(
  var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);
begin
  ABrush := Brush;
  PenSize := FrameWidth;
  if Erase then DrawRect := EraseDockRect
  else DrawRect := DockRect;
end;

function TCnDragDockObject.GetDragCursor(Accepted: Boolean; X,
  Y: Integer): TCursor;
begin
  Result := crDefault;
end;

function TCnDragDockObject.GetDragImages: TDragImageList;
begin
  Result := nil;
end;

function TCnDragDockObject.GetDropCtl: TControl;
var
  NextCtl: TControl;
  TargetCtl: TWinControl;
  CtlIdx: Integer;

  function GetDockClientsIndex: Integer;
  begin
    for Result := 0 to TCnWinControlAccess(TargetCtl).DockClientCount - 1 do
    begin
      if TCnWinControlAccess(TargetCtl).DockClients[Result] = NextCtl then
        Exit;
    end;
    Result := -1;
  end;

begin
  Result := nil;
  TargetCtl := DragTarget;
  if (TargetCtl = nil) or not TCnWinControlAccess(TargetCtl).UseDockManager or
    (TargetCtl.DockClientCount = 0) or
    ((TargetCtl.DockClientCount = 1) and
      (TCnWinControlAccess(TargetCtl).DockClients[0] = Control)) then
    Exit;
  NextCtl := FindDragTarget(DragPos, False);
  while (NextCtl <> nil) and (NextCtl <> TargetCtl) do
  begin
    CtlIdx := GetDockClientsIndex;
    if CtlIdx <> -1 then
    begin
      Result := TargetCtl.DockClients[CtlIdx];
      Exit;
    end
    else
      NextCtl := NextCtl.Parent;
  end;
end;

function TCnDragDockObject.GetFrameWidth: Integer;
begin
  Result := FFrameWidth;
end;

function TCnDragDockObject.GetTargetControl: TWinControl;
begin
  if FDragTarget <> nil then
    Result := TWinControl(FDragTarget)
  else Result := nil;
end;

procedure TCnDragDockObject.MouseMsg(var Msg: TMessage);
var
  P: TPoint;
begin
  try
    case Msg.Msg of
      WM_MOUSEMOVE:
        begin
          P := SmallPointToPoint(TWMMouse(Msg).Pos);
          ClientToScreen(CnGlobalDockPresident.DragCapture, P);
          CnGlobalDockPresident.DragTo(P);
        end;
      WM_CAPTURECHANGED:
      begin
        CnGlobalDockPresident.DragDone(False);
      end;
      WM_LBUTTONUP, WM_RBUTTONUP:
        if not GlobalDockClient.CanFloat then
        begin
          if (TargetControl = nil) and (GlobalDockClient.ParentForm.HostDockSite = nil) then
            CnGlobalDockPresident.DragDone(True)
          else
            CnGlobalDockPresident.DragDone(TargetControl <> nil);
        end
        else
          CnGlobalDockPresident.DragDone(True);
      { Forms.IsKeyMsg sends WM_KEYxxx messages here (+CN_BASE) when a
        TPUtilWindow has the mouse capture. }
      CN_KEYUP:
        if Msg.WParam = VK_CONTROL then
        begin
          FCtrlDown := False;
          CnGlobalDockPresident.DragTo(CnGlobalDockPresident.DragObject.DragPos);
        end;
      CN_KEYDOWN:
        begin
          case Msg.WParam of
            VK_CONTROL: // 当按下了Ctrl键的后，窗体不允许停靠
            begin
              FCtrlDown := True;
              CnGlobalDockPresident.DragTo(CnGlobalDockPresident.DragObject.DragPos);
            end;
            VK_ESCAPE:
              begin
                { Consume keystroke and cancel drag operation }
                Msg.Result := 1;
                CnGlobalDockPresident.DragDone(False);
              end;
          end;
        end;
    end;
  except
    if CnGlobalDockPresident.DragControl <> nil then
      CnGlobalDockPresident.DragDone(False);
    raise;
  end;
end;

procedure TCnDragDockObject.ReleaseCapture(Handle: HWND);
begin
  Windows.ReleaseCapture;
  DeallocateHWND(Handle);
end;

procedure TCnDragDockObject.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TCnDragDockObject.SetDropAlign(const Value: TAlign);
begin
  if FDropAlign <> Value then
    FDropAlign := Value;
end;

procedure TCnDragDockObject.SetDropOnControl(const Value: TControl);
begin
  FDropOnControl := Value;
end;

procedure TCnDragDockObject.SetFrameWidth(const Value: Integer);
begin
  FFrameWidth := Value;
end;

procedure TCnDragDockObject.SetTargetControl(const Value: TWinControl);
begin
  FDragTarget := Value;
end;

{ TCnCustomDockControl }

function TCnCustomDockControl.GetCnDockManager: ICnDockManager;
begin
  Result := ICnDockManager(DockManager);
end;

procedure TCnCustomDockControl.SetCnDockManager(const Value: ICnDockManager);
begin
  DockManager := Value;
end;

procedure TCnCustomDockControl.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
var
  DestRect: TRect;
  Form: TCustomForm;
begin
  { Map DockRect to dock site's client coordinates }
  DestRect := Source.DockRect;
  MapWindowPoints(0, Handle, DestRect, 2);

  DisableAlign;
  try
    Source.Control.Dock(Self, DestRect);
    if UseDockManager and (DockManager <> nil) then
    begin
      DockManager.InsertControl(Source.Control,
        Source.DropAlign, Source.DropOnControl);
    end;
  finally
    EnableAlign;
  end;
  Form := GetParentForm(Self);
  if Form <> nil then Form.BringToFront;
  { -------------------------------------------------------------------------- }
  if Source.Control is TForm then
  begin
    { 下面一行代码是为了防止窗体的标题栏和窗体上面的控件的内容被清空 }
    TForm(Source.Control).ActiveControl := nil;
    { ------------------------------------------------------------------------ }
    SetDockSite(TForm(Source.Control), False);
  end;
end;

procedure TCnCustomDockControl.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  CustomPositionDockRect(Source, X, Y);
end;

procedure TCnCustomDockControl.CustomEndDock(Target: TObject; X,
  Y: Integer);
begin

end;

procedure TCnCustomDockControl.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
  DropAlign := GetDockEdge(MousePos);
end;

procedure TCnCustomDockControl.CustomGetSiteInfo(Source: TCnDragDockObject;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(Handle, InfluenceRect);
  InflateRect(InfluenceRect, DefExpandoRect, DefExpandoRect);
end;

procedure TCnCustomDockControl.CustomPositionDockRect(
  Source: TCnDragDockObject; X, Y: Integer);
var
  NewWidth, NewHeight: Integer;
  TempX, TempY: Double;
begin
  with Source do
  begin
    if (DragTarget = nil) or (not TCnWinControlAccess(DragTarget).UseDockManager) then
    begin
      NewWidth := Control.UndockWidth;
      NewHeight := Control.UndockHeight;
      // Drag position for dock rect is scaled relative to control's click point.
      TempX := DragPos.X - ((NewWidth) * MouseDeltaX);
      TempY := DragPos.Y - ((NewHeight) * MouseDeltaY);
      with DockRect do
      begin
        Left := Round(TempX);
        Top := Round(TempY);
        Right := Left + NewWidth;
        Bottom := Top + NewHeight;
      end;
      { Allow DragDockObject final say on this new dock rect }
      AdjustDockRect(DockRect);
    end
    else begin
      GetWindowRect(TargetControl.Handle, DockRect);
      if TCnWinControlAccess(DragTarget).UseDockManager then
      begin
        if TargetControl is TCnCustomDockPanel then
        begin
          if (TCnCustomDockPanel(DragTarget).CnDockManager <> nil) then
            TCnCustomDockPanel(DragTarget).CnDockManager.PositionDockRect(Control,
              DropOnControl, DropAlign, DockRect);
        end;
      end;
    end;
  end;
end;

procedure TCnCustomDockControl.CustomStartDock(
  var Source: TCnDragDockObject);
begin

end;

function TCnCustomDockControl.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := (Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0);
end;

procedure TCnCustomDockControl.UpdateCaption(Exclude: TControl);
var i: Integer;
  Host: TCnDockableForm;
begin
  if Parent is TCnDockableForm then
  begin
    Host := TCnDockableForm(Parent);
    Host.Caption := '';
    { 更新本身的Caption }
    for I := 0 to Host.DockableControl.DockClientCount - 1 do
    begin
      if Host.DockableControl.DockClients[I].Visible and (Host.DockableControl.DockClients[I] <> Exclude) then
        Host.Caption := Host.Caption + TCustomForm(Host.DockableControl.DockClients[I]).Caption + gs_CnStringSplitter;
    end;
    { 更新TCnTabPageControl的标签 }
    if (Host.HostDockSite is TCnTabPageControl) then
    begin
      with TCnTabPageControl(Host.HostDockSite) do
        if (ActivePage <> nil) and (ActivePage.Controls[0] = Self) then
          ActivePage.Caption := Host.Caption;
    end;
    if Host.HostDockSite is TCnCustomDockControl then
      TCnCustomDockControl(Host.HostDockSite).UpdateCaption(nil);
  end;
end;

procedure TCnCustomDockControl.WndProc(var Message: TMessage);
var CMUnDockClient: TCMUnDockClient;
  DockableForm: TCnDockableForm;
begin
  if Message.Msg = CM_UNDOCKCLIENT then
  begin
    CMUnDockClient := TCMUnDockClient(Message);
    if CMUnDockClient.Client is TCnDockableForm then
    begin
      DockableForm := TCnDockableForm(CMUnDockClient.Client);
      if DockableForm.FloatingChild <> nil then
      begin
//        Cn_LockWindow(Self);
        try
          { 说明要把停靠客户重新停靠到服务器上,只要调用ReplaceZoneChild替换原先的
            Zone的ChildControl就可以了,不要忘记首先把这个FloatingChild显示出来,
            因为可能在前面FloatingChild被隐藏了,见CnDockFormControl.pas中的
            TCnDockableForm.DoClose函数 }
          if Self is TCnTabPageControl then
            DockableForm.FloatingChild.ManualDock(Self)
          else
            DockableForm.FloatingChild.Dock(Self, Rect(0,0,0,0));
          DockableForm.FloatingChild.Visible := True;
          if Self is TCnCustomDockPanel then
            CnDockManager.ReplaceZoneChild(DockableForm, DockableForm.FloatingChild);
        finally
//          Cn_UnLockWindow;
        end;
      end;
    end;
  end;
  inherited WndProc(Message);
end;

{ TCnCustomDockPanel }

constructor TCnCustomDockPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  Color := clBtnFace;
  UseDockManager := True;
end;

function TCnCustomDockPanel.CreateDockManager: IDockManager;
begin
  if (Self is TCnConjoinPanel) and
    (TCnConjoinPanel(Self).DockClient <> nil) and
    (TCnConjoinPanel(Self).DockClient.DockStyle <> nil) and
    (TCnConjoinPanel(Self).DockClient.DockStyle.CnConjoinPanelTreeClass <> nil) and
    (TCnConjoinPanel(Self).DockClient.DockStyle.CnConjoinPanelTreeClass <>
      TCnDockTreeClass(ClassType)) then
  begin
    if (DockManager = nil) and DockSite and UseDockManager then
      Result := TCnConjoinPanel(Self).DockClient.DockStyle.CnConjoinPanelTreeClass.Create(
        Self, TCnDockPanel(Self).DockServer.DockStyle.CnConjoinPanelZoneClass) as ICnDockManager
      else Result := DockManager;
  end else
  if (Self is TCnDockPanel) and
    (TCnDockPanel(Self).DockServer <> nil) and
    (TCnDockPanel(Self).DockServer.DockStyle <> nil) and
    (TCnDockPanel(Self).DockServer.DockStyle.CnDockPanelTreeClass <> nil) and
    (TCnDockPanel(Self).DockServer.DockStyle.CnDockPanelTreeClass <>
      TCnDockTreeClass(ClassType)) then
  begin
    if (DockManager = nil) and DockSite and UseDockManager then
      Result := TCnDockPanel(Self).DockServer.DockStyle.CnDockPanelTreeClass.Create(
        Self, TCnDockPanel(Self).DockServer.DockStyle.CnDockPanelZoneClass) as ICnDockManager
      else Result := DockManager;
  end else
  begin
    if (DockManager = nil) and DockSite and UseDockManager then
      Result := DefaultDockTreeClass.Create(Self, DefaultDockZoneClass) as ICnDockManager
      else Result := DockManager;
  end;
  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;

destructor TCnCustomDockPanel.Destroy;
begin
  SetDockSite(Self, False);
  inherited Destroy;
end;

{ TCnDockCustomTabControl }

constructor TCnDockCustomTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 289;
  Height := 193;
  TabStop := True;
  ControlStyle := [csAcceptsControls, csDoubleClicks];
  FTabs := TCnDockTabStrings.Create;
  TCnDockTabStrings(FTabs).FTabControl := Self;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TCnDockCustomTabControl.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(FSaveTabs);
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;

function TCnDockCustomTabControl.CanChange: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then FOnChanging(Self, Result);
end;

function TCnDockCustomTabControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TCnDockCustomTabControl.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCnDockCustomTabControl.CreateParams(var Params: TCreateParams);
const
  AlignStyles: array[Boolean, TTabPosition] of DWORD =
    ((0, TCS_BOTTOM, TCS_VERTICAL, TCS_VERTICAL or TCS_RIGHT),
     (0, TCS_BOTTOM, TCS_VERTICAL or TCS_RIGHT, TCS_VERTICAL));
  TabStyles: array[TTabStyle] of DWORD = (TCS_TABS, TCS_BUTTONS,
    TCS_BUTTONS or TCS_FLATBUTTONS);
   RRStyles: array[Boolean] of DWORD = (0, TCS_RAGGEDRIGHT);
begin
  InitCommonControl(ICC_TAB_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_TABCONTROL);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN or
      AlignStyles[UseRightToLeftAlignment, FTabPosition] or
      TabStyles[FStyle] or RRStyles[FRaggedRight];
    if not TabStop then Style := Style or TCS_FOCUSNEVER;
    if FMultiLine then Style := Style or TCS_MULTILINE;
    if FMultiSelect then Style := Style or TCS_MULTISELECT;
    if FOwnerDraw then Style := Style or TCS_OWNERDRAWFIXED;
    if FTabSize.X <> 0 then Style := Style or TCS_FIXEDWIDTH;
    if FHotTrack and (not (csDesigning in ComponentState)) then
      Style := Style or TCS_HOTTRACK;
    if FScrollOpposite then Style := Style or TCS_SCROLLOPPOSITE;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW) or
      CS_DBLCLKS;
  end;
end;

procedure TCnDockCustomTabControl.CreateWnd;
begin
  inherited CreateWnd;
  if (Images <> nil) and Images.HandleAllocated then
    Perform(TCM_SETIMAGELIST, 0, Images.Handle);
  if Integer(FTabSize) <> 0 then UpdateTabSize;
  if FSaveTabs <> nil then
  begin
    FTabs.Assign(FSaveTabs);
    SetTabIndex(FSaveTabIndex);
    FSaveTabs.Free;
    FSaveTabs := nil;
  end;
end;

procedure TCnDockCustomTabControl.DrawTab(TabIndex: Integer; const Rect: TRect;
  Active: Boolean);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, TabIndex, Rect, Active)
  else
    Canvas.FillRect(Rect);
end;

function TCnDockCustomTabControl.GetDisplayRect: TRect;
begin
  Result := ClientRect;
  SendMessage(Handle, TCM_ADJUSTRECT, 0, Integer(@Result));
  if TabPosition = tpTop then
    Inc(Result.Top, 2);
end;

function TCnDockCustomTabControl.GetImageIndex(TabIndex: Integer): Integer;
begin
  Result := TabIndex;
  if Assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, TabIndex, Result);
end;

function TCnDockCustomTabControl.GetTabIndex: Integer;
begin
  Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
end;

procedure TCnDockCustomTabControl.Loaded;
begin
  inherited Loaded;
  if Images <> nil then UpdateTabImages;
end;

procedure TCnDockCustomTabControl.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    RecreateWnd;
  end;
end;

procedure TCnDockCustomTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TCnDockCustomTabControl.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    Perform(TCM_SETIMAGELIST, 0, Images.Handle);
  end
  else Perform(TCM_SETIMAGELIST, 0, 0);
end;

procedure TCnDockCustomTabControl.ImageListChange(Sender: TObject);
begin
  Perform(TCM_SETIMAGELIST, 0, TCustomImageList(Sender).Handle);
end;

function TCnDockCustomTabControl.InternalSetMultiLine(Value: Boolean): Boolean;
begin
  Result := FMultiLine <> Value;
  if Result then
  begin
    if not Value and ((TabPosition = tpLeft) or (TabPosition = tpRight)) then
      TabControlError(sTabMustBeMultiLine);
    FMultiLine := Value;
    if not Value then FScrollOpposite := False;
  end;
end;

procedure TCnDockCustomTabControl.SetMultiLine(Value: Boolean);
begin
  if InternalSetMultiLine(Value) then RecreateWnd;
end;

procedure TCnDockCustomTabControl.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

procedure TCnDockCustomTabControl.SetOwnerDraw(Value: Boolean);
begin
  if FOwnerDraw <> Value then
  begin
    FOwnerDraw := Value;
    RecreateWnd;
  end;
end;

procedure TCnDockCustomTabControl.SetRaggedRight(Value: Boolean);
begin
  if FRaggedRight <> Value then
  begin
    FRaggedRight := Value;
    SetComCtlStyle(Self, TCS_RAGGEDRIGHT, Value);
  end;
end;

procedure TCnDockCustomTabControl.SetScrollOpposite(Value: Boolean);
begin
  if FScrollOpposite <> Value then
  begin
    FScrollOpposite := Value;
    if Value then FMultiLine := Value;
    RecreateWnd;
  end;
end;

procedure TCnDockCustomTabControl.SetStyle(Value: TTabStyle);
begin
  if FStyle <> Value then
  begin
    if (Value <> tsTabs) and (TabPosition <> tpTop) then
      raise EInvalidOperation.Create(SInvalidTabStyle);
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCnDockCustomTabControl.SetTabHeight(Value: Smallint);
begin
  if FTabSize.Y <> Value then
  begin
    if Value < 0 then
      raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
    FTabSize.Y := Value;
    UpdateTabSize;
  end;
end;

procedure TCnDockCustomTabControl.SetTabIndex(Value: Integer);
begin
  SendMessage(Handle, TCM_SETCURSEL, Value, 0);
end;

procedure TCnDockCustomTabControl.SetTabPosition(Value: TTabPosition);
begin
  if FTabPosition <> Value then
  begin
    if (Value <> tpTop) and (Style <> tsTabs) then
      raise EInvalidOperation.Create(SInvalidTabPosition);
    FTabPosition := Value;
    if not MultiLine and ((Value = tpLeft) or (Value = tpRight)) then
      InternalSetMultiLine(True);
    RecreateWnd;
  end;
end;

procedure TCnDockCustomTabControl.SetTabs(Value: TStrings);
begin
  FTabs.Assign(Value);
end;

procedure TCnDockCustomTabControl.SetTabWidth(Value: Smallint);
var
  OldValue: Smallint;
begin
  if FTabSize.X <> Value then
  begin
    if Value < 0 then
      raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
    OldValue := FTabSize.X;
    FTabSize.X := Value;
    if (OldValue = 0) or (Value = 0) then RecreateWnd
    else UpdateTabSize;
  end;
end;

procedure TCnDockCustomTabControl.TabsChanged;
begin
  if not FUpdating then
  begin
    if HandleAllocated then
      SendMessage(Handle, WM_SIZE, SIZE_RESTORED,
        Word(Width) or Word(Height) shl 16);
    Realign;
  end;
end;

procedure TCnDockCustomTabControl.UpdateTabSize;
begin
  SendMessage(Handle, TCM_SETITEMSIZE, 0, Integer(FTabSize));
  TabsChanged;
end;

procedure TCnDockCustomTabControl.UpdateTabImages;
var
  I: Integer;
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_IMAGE;
  for I := 0 to FTabs.Count - 1 do
  begin
    TCItem.iImage := GetImageIndex(I);
    if SendMessage(Handle, TCM_SETITEM, I,
      Longint(@TCItem)) = 0 then
      TabControlError(Format(sTabFailSet, [FTabs[I], I]));
  end;
  TabsChanged;
end;

procedure TCnDockCustomTabControl.CNDrawItem(var Message: TWMDrawItem);
var
  SaveIndex: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
    SaveIndex := SaveDC(hDC);
    Canvas.Lock;
    try
      Canvas.Handle := hDC;
      Canvas.Font := Font;
      Canvas.Brush := Brush;
      DrawTab(itemID, rcItem, itemState and ODS_SELECTED <> 0);
    finally
      Canvas.Handle := 0;
      Canvas.Unlock;
      RestoreDC(hDC, SaveIndex);
    end;
  end;
  Message.Result := 1;
end;

procedure TCnDockCustomTabControl.WMDestroy(var Message: TWMDestroy);
var
  FocusHandle: HWnd;
begin
  if (FTabs <> nil) and (FTabs.Count > 0) then
  begin
    FSaveTabs := TStringList.Create;
    FSaveTabs.Assign(FTabs);
    FSaveTabIndex := GetTabIndex;
  end;
  FocusHandle := GetFocus;
  if (FocusHandle <> 0) and ((FocusHandle = Handle) or
    IsChild(Handle, FocusHandle)) then
    Windows.SetFocus(0);
  inherited;
  WindowHandle := 0;
end;

procedure TCnDockCustomTabControl.WMNotifyFormat(var Message: TMessage);
begin
  with Message do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TCnDockCustomTabControl.WMSize(var Message: TMessage);
begin
  inherited;
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE);
end;

procedure TCnDockCustomTabControl.CMFontChanged(var Message);
begin
  inherited;
  if HandleAllocated then Perform(WM_SIZE, 0, 0);
end;

procedure TCnDockCustomTabControl.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    Message.Msg := WM_SYSCOLORCHANGE;
    DefaultHandler(Message);
  end;
end;

procedure TCnDockCustomTabControl.CMTabStopChanged(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then RecreateWnd;
end;

procedure TCnDockCustomTabControl.CNNotify(var Message: TWMNotify);
begin
  with Message do
    case NMHdr^.code of
      TCN_SELCHANGE:
        Change;
      TCN_SELCHANGING:
        begin
          Result := 1;
          if CanChange then Result := 0;
        end;
    end;
end;

procedure TCnDockCustomTabControl.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I := 0 to FTabs.Count - 1 do
    if IsAccel(Message.CharCode, FTabs[I]) and CanShowTab(I) and CanFocus then
    begin
      Message.Result := 1;
      if CanChange then
      begin
        TabIndex := I;
        Change;
      end;
      Exit;
    end;
  inherited;
end;

procedure TCnDockCustomTabControl.AdjustClientRect(var Rect: TRect);
begin
  Rect := DisplayRect;
  inherited AdjustClientRect(Rect);
end;

function TCnDockCustomTabControl.IndexOfTabAt(X, Y: Integer): Integer;
var
  HitTest: TTCHitTestInfo;
begin
  Result := -1;
  if PtInRect(ClientRect, Point(X, Y)) then
    with HitTest do
    begin
      pt.X := X;
      pt.Y := Y;
      Result := TabCtrl_HitTest(Handle, @HitTest);
    end;
end;

function TCnDockCustomTabControl.GetHitTestInfoAt(X, Y: Integer): THitTests;
var
  HitTest: TTCHitTestInfo;
begin
  Result := [];
  if PtInRect(ClientRect, Point(X, Y)) then
    with HitTest do
    begin
      pt.X := X;
      pt.Y := Y;
      if TabCtrl_HitTest(Handle, @HitTest) <> -1 then
      begin
        if (flags and TCHT_NOWHERE) <> 0 then
          Include(Result, htNowhere);
        if (flags and TCHT_ONITEM) = TCHT_ONITEM then
          Include(Result, htOnItem)
        else
        begin
          if (flags and TCHT_ONITEM) <> 0 then
            Include(Result, htOnItem);
          if (flags and TCHT_ONITEMICON) <> 0 then
            Include(Result, htOnIcon);
          if (flags and TCHT_ONITEMLABEL) <> 0 then
            Include(Result, htOnLabel);
        end;
      end
      else
        Result := [htNowhere];
    end;
end;

function TCnDockCustomTabControl.TabRect(Index: Integer): TRect;
begin
  TabCtrl_GetItemRect(Handle, Index, Result);
end;

function TCnDockCustomTabControl.RowCount: Integer;
begin
  Result := TabCtrl_GetRowCount(Handle);
end;

procedure TCnDockCustomTabControl.ScrollTabs(Delta: Integer);
var
  Wnd: HWND;
  P: TPoint;
  Rect: TRect;
  I: Integer;
begin
  Wnd := FindWindowEx(Handle, 0, 'msctls_updown32', nil);
  if Wnd <> 0 then
  begin
    Windows.GetClientRect(Wnd, Rect);
    if Delta < 0 then
      P.X := Rect.Left + 2
    else
      P.X := Rect.Right - 2;
    P.Y := Rect.Top + 2;
    for I := 0 to Abs(Delta) - 1 do
    begin
      SendMessage(Wnd, WM_LBUTTONDOWN, 0, MakeLParam(P.X, P.Y));
      SendMessage(Wnd, WM_LBUTTONUP, 0, MakeLParam(P.X, P.Y));
    end;
  end;
end;

procedure TCnDockCustomTabControl.TCMAdjustRect(var Message: TMessage);
begin
  { Major hack around a problem in the Windows tab control. Don't try this
    at home. The tab control (4.71) will AV when in a TCM_ADJUSTRECT message
    when the height of the control is the same as the height of the tab (or the
    width of the control for tpBottom). This hack will return the last value
    successfully returned if an exception is encountered. This allows the
    control to function but the AV is till generated and and reported by the
    debugger. }
  try
    inherited;
    if (TabPosition <> tpTop) and (Message.WParam = 0) then
      FSavedAdjustRect := PRect(Message.LParam)^;
  except
    PRect(Message.LParam)^ := FSavedAdjustRect;
  end;
end;

procedure TCnDockCustomTabControl.PaintWindow(DC: HDC);
var
  Message: TMessage;
begin
  if not OwnerDraw then
  begin
    { 如果不是自己画,就调用系统的重画事件 }
    Message.Msg := WM_PAINT;
    Message.WParam := DC;
    Message.LParam := 0;
    Message.Result := 0;
    DefaultHandler(Message);
  end;
  inherited PaintWindow(DC);
end;

{ TCnDockTabSheet }

constructor TCnDockTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  FTabVisible := True;
  FHighlighted := False;
end;

destructor TCnDockTabSheet.Destroy;
begin
  if FPageControl <> nil then
  begin
    if FPageControl.FUndockingPage = Self then FPageControl.FUndockingPage := nil;
    FPageControl.RemovePage(Self);
  end;
  inherited Destroy;
end;

procedure TCnDockTabSheet.DoHide;
begin
  if Assigned(FOnHide) then FOnHide(Self);
end;

procedure TCnDockTabSheet.DoShow;
begin
  if Assigned(FOnShow) then FOnShow(Self);
end;

function TCnDockTabSheet.GetPageIndex: Integer;
begin
  if (FPageControl <> nil) and (FPageControl.FPages <> nil) then
    Result := FPageControl.FPages.IndexOf(Self)
  else Result := -1;
end;

function TCnDockTabSheet.GetTabIndex: Integer;
var
  I: Integer;
begin
  Result := 0;
  if not FTabShowing then Dec(Result) else
    for I := 0 to PageIndex - 1 do
      if TCnDockTabSheet(FPageControl.FPages[I]).FTabShowing then
        Inc(Result);
end;

procedure TCnDockTabSheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TCnDockTabSheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TCnDockPageControl then
    PageControl := TCnDockPageControl(Reader.Parent);
end;

procedure TCnDockTabSheet.SetImageIndex(Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    if FTabShowing then FPageControl.UpdateTab(Self);
  end;
end;

procedure TCnDockTabSheet.SetPageControl(APageControl: TCnDockPageControl);
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then FPageControl.RemovePage(Self);
    Parent := APageControl;
    if APageControl <> nil then APageControl.InsertPage(Self);
  end;
end;

procedure TCnDockTabSheet.SetPageIndex(Value: Integer);
var
  I, MaxPageIndex: Integer;
begin
  if (FPageControl <> nil) and (FPageControl.FPages <> nil) then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateResFmt(@SPageIndexError, [Value, MaxPageIndex]);
    I := TabIndex;
    FPageControl.FPages.Move(PageIndex, Value);
    if I >= 0 then FPageControl.MoveTab(I, TabIndex);
  end;
end;

procedure TCnDockTabSheet.SetTabShowing(Value: Boolean);
var
  Index: Integer;
begin
  if FTabShowing <> Value then
    if Value then
    begin
      FTabShowing := True;
      FPageControl.InsertTab(Self);
    end else
    begin
      Index := TabIndex;
      FTabShowing := False;
      FPageControl.DeleteTab(Self, Index);
    end;
end;

procedure TCnDockTabSheet.SetTabVisible(Value: Boolean);
begin
  if FTabVisible <> Value then
  begin
    FTabVisible := Value;
    UpdateTabShowing;
  end;
end;

procedure TCnDockTabSheet.UpdateTabShowing;
begin
  SetTabShowing((FPageControl <> nil) and FTabVisible);
end;

procedure TCnDockTabSheet.CMTextChanged(var Message: TMessage);
begin
  if FTabShowing then FPageControl.UpdateTab(Self);
end;

procedure TCnDockTabSheet.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    try
      DoShow
    except
      Application.HandleException(Self);
    end;
  end else if not Showing then
  begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TCnDockTabSheet.SetHighlighted(Value: Boolean);
begin
  if not (csReading in ComponentState) then
    SendMessage(PageControl.Handle, TCM_HIGHLIGHTITEM, TabIndex,
      MakeLong(Word(Value), 0));
  FHighlighted := Value;
end;


{ TCnDockPageControl }

constructor TCnDockPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDoubleClicks, csOpaque];
  FPages := TList.Create;
  FCnDockTabSheetClass := TCnDockTabSheet;
end;

destructor TCnDockPageControl.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do TCnDockTabSheet(FPages[I]).FPageControl := nil;
  FPages.Free;
  inherited Destroy;
end;

procedure TCnDockPageControl.UpdateTabHighlights;
var
  I: Integer;
begin
  for I := 0 to PageCount - 1 do
    Pages[I].SetHighlighted(Pages[I].FHighlighted);
end;

procedure TCnDockPageControl.Loaded;
begin
  inherited Loaded;
  UpdateTabHighlights;
end;


function TCnDockPageControl.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := TCnDockTabSheet(FPages[TabIndex]).Enabled;
end;

procedure TCnDockPageControl.Change;
var
  Form: TCustomForm;
begin
  if TabIndex >= 0 then
    UpdateActivePage;
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
  end;
  inherited Change;
end;

procedure TCnDockPageControl.ChangeActivePage(Page: TCnDockTabSheet);
var
  ParentForm: TCustomForm;
begin
  if FActivePage <> Page then
  begin
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
      if ParentForm.ActiveControl <> FActivePage then
      begin
        TabIndex := FActivePage.TabIndex;
        Exit;
      end;
    end;
    if Page <> nil then
    begin
      Page.BringToFront;
      Page.Visible := True;
      if (ParentForm <> nil) and (FActivePage <> nil) and
        (ParentForm.ActiveControl = FActivePage) then
        if Page.CanFocus then
          ParentForm.ActiveControl := Page else
          ParentForm.ActiveControl := Self;
    end;
    if FActivePage <> nil then FActivePage.Visible := False;
    FActivePage := Page;
    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirst;
  end;
end;

procedure TCnDockPageControl.DeleteTab(Page: TCnDockTabSheet; Index: Integer);
var
  UpdateIndex: Boolean;
begin
  UpdateIndex := Page = ActivePage;
  Tabs.Delete(Index);
  if UpdateIndex then
  begin
    if Index >= Tabs.Count then
      Index := Tabs.Count - 1;
    TabIndex := Index;
  end;
  UpdateActivePage;
end;

procedure TCnDockPageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewDockSheet <> nil then Client.Parent := FNewDockSheet;
end;

procedure TCnDockPageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TCnDockPageControl.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingPage <> nil) and not (csDestroying in ComponentState) then
  begin
    SelectNextPage(True);
    FUndockingPage.Free;
    FUndockingPage := nil;
  end;
end;

function TCnDockPageControl.FindNextPage(CurPage: TCnDockTabSheet;
  GoForward, CheckTabVisible: Boolean): TCnDockTabSheet;
var
  I, StartIndex: Integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then StartIndex := FPages.Count - 1 else StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then I := 0;
      end else
      begin
        if I = 0 then I := FPages.Count;
        Dec(I);
      end;
      Result := FPages[I];
      if not CheckTabVisible or Result.TabVisible then Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

procedure TCnDockPageControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do Proc(TComponent(FPages[I]));
end;

function TCnDockPageControl.GetImageIndex(TabIndex: Integer): Integer;
var
  I,
  Visible,
  NotVisible: Integer;
begin
  if Assigned(FOnGetImageIndex) then
    Result := inherited GetImageIndex(TabIndex) else
    begin
     { For a PageControl, TabIndex refers to visible tabs only. The control
     doesn't store }
      Visible := 0;
      NotVisible := 0;
      for I := 0 to FPages.Count - 1 do
      begin
        if not GetPage(I).TabVisible then Inc(NotVisible)
        else Inc(Visible);
        if Visible = TabIndex + 1 then Break;
      end;
//      Result := 0;
//      if TabIndex + NotVisible >= PageCount then Exit;
      Result := GetPage(TabIndex + NotVisible).ImageIndex;
    end;
end;

function TCnDockPageControl.GetPageFromDockClient(Client: TControl): TCnDockTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PageCount - 1 do
  begin
    if (Client.Parent = Pages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := Pages[I];
      Exit;
    end;
  end;
end;

function TCnDockPageControl.GetPage(Index: Integer): TCnDockTabSheet;
begin
  Result := FPages[Index];
end;

function TCnDockPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TCnDockPageControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := GetPageFromDockClient(Client) = nil;
  inherited GetSiteInfo(Client, InfluenceRect, MousePos, CanDock);
end;

procedure TCnDockPageControl.InsertPage(Page: TCnDockTabSheet);
begin
  FPages.Add(Page);
  Page.FPageControl := Self;
  Page.UpdateTabShowing;
end;

procedure TCnDockPageControl.InsertTab(Page: TCnDockTabSheet);
begin
  Tabs.InsertObject(Page.TabIndex, Page.Caption, Page);
  UpdateActivePage;
end;

procedure TCnDockPageControl.MoveTab(CurIndex, NewIndex: Integer);
begin
  Tabs.Move(CurIndex, NewIndex);
end;

procedure TCnDockPageControl.RemovePage(Page: TCnDockTabSheet);
var
  NextSheet: TCnDockTabSheet;
begin
  NextSheet := FindNextPage(Page, True, not (csDesigning in ComponentState));
  if NextSheet = Page then NextSheet := nil;
  Page.SetTabShowing(False);
  Page.FPageControl := nil;
  FPages.Remove(Page);
  SetActivePage(NextSheet);
end;

procedure TCnDockPageControl.SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
var
  Page: TCnDockTabSheet;
begin
  Page := FindNextPage(ActivePage, GoForward, CheckTabVisible);
  if (Page <> nil) and (Page <> ActivePage) and CanChange then
  begin
    SetActivePage(Page);
    Change;
  end;
end;

procedure TCnDockPageControl.SetActivePage(Page: TCnDockTabSheet);
begin
  if (Page <> nil) and (Page.PageControl <> Self) then Exit;
  ChangeActivePage(Page);
  if Page = nil then
    TabIndex := -1
  else if Page = FActivePage then
    TabIndex := Page.TabIndex;
end;

procedure TCnDockPageControl.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TCnDockTabSheet(Child).PageIndex := Order;
end;

procedure TCnDockPageControl.ShowControl(AControl: TControl);
begin
  if (AControl is TCnDockTabSheet) and (TCnDockTabSheet(AControl).PageControl = Self) then
    SetActivePage(TCnDockTabSheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TCnDockPageControl.UpdateTab(Page: TCnDockTabSheet);
begin
  Tabs[Page.TabIndex] := Page.Caption;
end;

procedure TCnDockPageControl.UpdateActivePage;
begin
  if TabIndex >= 0 then
    SetActivePage(TCnDockTabSheet(Tabs.Objects[TabIndex]))
  else
    SetActivePage(nil);
end;

procedure TCnDockPageControl.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
begin
  HitTestInfo.pt := SmallPointToPoint(Message.Pos);
  HitIndex := SendMessage(Handle, TCM_HITTEST, 0, Longint(@HitTestInfo));
  if (HitIndex >= 0) and (HitIndex <> TabIndex) then Message.Result := 1;
end;

procedure TCnDockPageControl.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end else
    inherited;
end;

procedure TCnDockPageControl.CMDockClient(var Message: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
begin
  Message.Result := 0;
  if FCnDockTabSheetClass <> nil then
    FNewDockSheet := FCnDockTabSheetClass.Create(Self)
  else FNewDockSheet := TCnDockTabSheet.Create(Self);
  try
    try
      DockCtl := Message.DockSource.Control;
      FNewDockSheet.PageControl := Self;
      if DockCtl is TCustomForm then
        FNewDockSheet.Caption := TCustomForm(DockCtl).Caption;
      DockCtl.Dock(Self, Message.DockSource.DockRect);
    except
      FNewDockSheet.Free;
      raise;
    end;
    IsVisible := DockCtl.Visible;
    FNewDockSheet.TabVisible := IsVisible;
    if IsVisible then ActivePage := FNewDockSheet;
    DockCtl.Align := alClient;
  finally
    FNewDockSheet := nil;
  end;
end;

procedure TCnDockPageControl.CMDockNotification(var Message: TCMDockNotification);
var
  I: Integer;
  S: string;
  Page: TCnDockTabSheet;
begin
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
          S := PChar(Message.NotifyRec.MsgLParam);
          { Search for first CR/LF and end string there }
          for I := 1 to Length(S) do
            if {$IFDEF DELPHI12_UP}CharInSet(S[i], [#13, #10]){$ELSE}S[I] in [#13, #10]{$ENDIF} then
            begin
              SetLength(S, I - 1);
              Break;
            end;
          Page.Caption := S;
        end;
      CM_VISIBLECHANGED:
        Page.TabVisible := Boolean(Message.NotifyRec.MsgWParam);
    end;
  inherited;
end;

procedure TCnDockPageControl.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TCnDockTabSheet;
begin
  Message.Result := 0;
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
  begin
    FUndockingPage := Page;
    Message.Client.Align := alNone;
  end;
end;

function TCnDockPageControl.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  i, HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
  Page: TCnDockTabSheet;
begin
  Result := nil;
  if DockSite then
  begin
    HitTestInfo.pt := MousePos;
    HitIndex := SendMessage(Handle, TCM_HITTEST, 0, Longint(@HitTestInfo));
    if HitIndex >= 0 then
    begin
      Page := nil;
      for i := 0 to HitIndex do
        Page := FindNextPage(Page, True, True);
      if (Page <> nil) and (Page.ControlCount > 0) then
      begin
        Result := Page.Controls[0];
        if Result.HostDockSite <> Self then Result := nil;
      end;
    end;
  end;
end;

procedure TCnDockPageControl.WMLButtonDown(var Message: TWMLButtonDown);
var
  DockCtl: TControl;
begin
  inherited;
  if GlobalDockClient <> nil then
    DockCtl := ButtonEvent(Self, Message, mbLeft, msTabPage, GlobalDockClient.DoNCButtonDown)
  else DockCtl := nil;
  if (DockCtl <> nil) and (Style = tsTabs) then
    { 如果条件都满足，就调用CnGlobalDockPresident的BeginDrag方法，开始停靠操作 }
    CnGlobalDockPresident.BeginDrag(DockCtl, False);
end;

procedure TCnDockPageControl.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  DockCtl: TControl;
begin
  inherited;
  if GlobalDockClient <> nil then
    DockCtl := ButtonEvent(Self, Message, mbLeft, msTabPage, GlobalDockClient.DoNCButtonDblClk)
  else DockCtl := nil;
  if (DockCtl <> nil) and (GlobalDockClient.CanFloat) then
    DockCtl.ManualDock(nil, nil, alNone);
end;

function TCnDockPageControl.GetActivePageIndex: Integer;
begin
  if ActivePage <> nil then
    Result := ActivePage.GetPageIndex
  else
    Result := -1;
end;

procedure TCnDockPageControl.SetActivePageIndex(const Value: Integer);
begin
  if (Value > -1) and (Value < PageCount) then
    ActivePage := Pages[Value]
  else
    ActivePage := nil;
end;

{ TTabStrings }

procedure TCnDockTabStrings.Clear;
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEALLITEMS, 0, 0) = 0 then
    TabControlError(sTabFailClear);
  FTabControl.TabsChanged;
end;

procedure TCnDockTabStrings.Delete(Index: Integer);
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEITEM, Index, 0) = 0 then
    TabControlError(Format(sTabFailDelete, [Index]));
  FTabControl.TabsChanged;
end;

function TCnDockTabStrings.Get(Index: Integer): string;
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
  Buffer: array[0..4095] of Char;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading];
  TCItem.pszText := Buffer;
  TCItem.cchTextMax := SizeOf(Buffer);
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailRetrieve, [Index]));
  Result := Buffer;
end;

function TCnDockTabStrings.GetCount: Integer;
begin
  Result := SendMessage(FTabControl.Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TCnDockTabStrings.GetObject(Index: Integer): TObject;
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailGetObject, [Index]));
  Result := TObject(TCItem.lParam);
end;

procedure TCnDockTabStrings.Put(Index: Integer; const S: string);
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading] or
    TCIF_IMAGE;
  TCItem.pszText := PChar(S);
  TCItem.iImage := FTabControl.GetImageIndex(Index);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailSet, [S, Index]));
  FTabControl.TabsChanged;
end;

procedure TCnDockTabStrings.PutObject(Index: Integer; AObject: TObject);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  TCItem.lParam := Longint(AObject);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index,
    Longint(@TCItem)) = 0 then
    TabControlError(Format(sTabFailSetObject, [Index]));
end;

procedure TCnDockTabStrings.Insert(Index: Integer; const S: string);
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading] or
    TCIF_IMAGE;
  TCItem.pszText := PChar(S);
  TCItem.iImage := FTabControl.GetImageIndex(Index);
  if SendMessage(FTabControl.Handle, TCM_INSERTITEM, Index,
    Longint(@TCItem)) < 0 then
    TabControlError(Format(sTabFailSet, [S, Index]));
  FTabControl.TabsChanged;
end;

procedure TCnDockTabStrings.SetUpdateState(Updating: Boolean);
begin
  FTabControl.FUpdating := Updating;
  SendMessage(FTabControl.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
  begin
    FTabControl.Invalidate;
    FTabControl.TabsChanged;
  end;
end;

function TCnDockPageControl.DoMouseEvent(var Message: TWMMouse;
  Control: TControl): TWMNCHitMessage;
begin
  Result := Cn_CreateNCMessage(Control, Message.Msg + WM_NCMOUSEFIRST - WM_MOUSEFIRST,
    HTCAPTION, SmallPointToPoint(Message.Pos));
end;

procedure TCnDockPageControl.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if GlobalDockClient <> nil then
    ButtonEvent(Self, Message, mbLeft, msTabPage, GlobalDockClient.DoNCButtonUp);
end;

procedure TCnDockPageControl.WMMButtonDblClk(
  var Message: TWMMButtonDblClk);
begin
  inherited;
  if GlobalDockClient <> nil then
    ButtonEvent(Self, Message, mbMiddle, msTabPage, GlobalDockClient.DoNCButtonDblClk);
end;

procedure TCnDockPageControl.WMMButtonDown(var Message: TWMMButtonDown);
begin
  inherited;
  if GlobalDockClient <> nil then
    ButtonEvent(Self, Message, mbMiddle, msTabPage, GlobalDockClient.DoNCButtonDown);
end;

procedure TCnDockPageControl.WMMButtonUp(var Message: TWMMButtonUp);
begin
  inherited;
  if GlobalDockClient <> nil then
    ButtonEvent(Self, Message, mbMiddle, msTabPage, GlobalDockClient.DoNCButtonUp);
end;

procedure TCnDockPageControl.WMRButtonDblClk(
  var Message: TWMRButtonDblClk);
begin
  inherited;
  if GlobalDockClient <> nil then
    ButtonEvent(Self, Message, mbRight, msTabPage, GlobalDockClient.DoNCButtonDblClk);
end;

procedure TCnDockPageControl.WMRButtonDown(var Message: TWMRButtonDown);
begin
  { 欺骗系统以为是鼠标左键按下 }
  Message.Msg := WM_LBUTTONDOWN;
  inherited;
  if GlobalDockClient <> nil then
    ButtonEvent(Self, Message, mbRight, msTabPage, GlobalDockClient.DoNCButtonDown);
end;

procedure TCnDockPageControl.WMRButtonUp(var Message: TWMRButtonUp);
begin
  inherited;
  if GlobalDockClient <> nil then
    ButtonEvent(Self, Message, mbRight, msTabPage, GlobalDockClient.DoNCButtonUp);
end;

{ TCnDockPresident }

constructor TCnDockPresident.Create;
begin
  { 创建列表和Hash表 }
  FDockServersList := TList.Create;
  FDockClientsList := TList.Create;
  FDockServersHash := TCnDockControlHashTable.Create(10, False);
  FDockClientsHash := TCnDockControlHashTable.Create(30, False);
  FDockableFormList := TList.Create;
  DockSiteList := TList.Create;
end;

destructor TCnDockPresident.Destroy;
begin
  { 删除列表和Hash表 }
  FDockableFormList.Free;
  FDockServersList.Free;
  FDockClientsList.Free;
  FDockServersHash.Free;
  FDockClientsHash.Free;
  DockSiteList.Free;
  inherited Destroy;
end;

function TCnDockPresident.FindDockClientForm(AName: string): TControl;
var i: Integer;
begin
  Result := nil;
  for i := 0 to FDockServersList.Count - 1 do
  begin
    if TControl(FDockServersList[i]).Name = AName then
    begin
      Result := TControl(FDockServersList[i]);
      Exit;
    end;
  end;
end;

function TCnDockPresident.FindDockServerForm(AName: string): TControl;
var i: Integer;
begin
  Result := nil;
  for i := 0 to FDockClientsList.Count - 1 do
  begin
    if TControl(FDockClientsList[i]).Name = AName then
    begin
      Result := TControl(FDockClientsList[i]);
      Exit;
    end;
  end;
end;

function TCnDockPresident.FindDockControlForm(AName: string): TControl;
begin
  Result := FindDockServerForm(AName);
  if Result = nil then
    FindDockClientForm(AName);
end;

function TCnDockPresident.GetFormVisible(DockWindow: TWinControl): Boolean;
begin
  Result := CnDockFormControl.GetFormVisible(DockWindow);
end;

procedure TCnDockPresident.HideDockForm(DockWindow: TControl);
begin
  CnDockFormControl.HideDockForm(DockWindow);
end;

procedure TCnDockPresident.LoadDockTreeFromFile(FileName: string);
begin
  BeginLoad;
  try
    CnDockFormControl.LoadDockTreeFromFile(FileName);
  finally
    EndLoad;
  end;
end;

procedure TCnDockPresident.LoadDockTreeFromReg(RootKey: DWORD;
  RegPath: string);
begin
  BeginLoad;
  try
    CnDockFormControl.LoadDockTreeFromReg(RootKey, RegPath);
  finally
    EndLoad;
  end;
end;

procedure TCnDockPresident.SaveDockTreeToFile(FileName: string);
begin
  BeginSave;
  try
    CnDockFormControl.SaveDockTreeToFile(FileName);
  finally
    EndSave;
  end;
end;

procedure TCnDockPresident.SaveDockTreeToReg(RootKey: DWORD;
  RegPath: string);
begin
  BeginSave;
  try
    CnDockFormControl.SaveDockTreeToReg(RootKey, RegPath);
  finally
    EndSave;
  end;
end;

procedure TCnDockPresident.SetConjoinDockHostBorderStyle(
  Value: TFormBorderStyle);
begin
  CnDockFormControl.SetConjoinDockHostBorderStyle(Value);
end;

procedure TCnDockPresident.SetTabDockHostBorderStyle(
  Value: TFormBorderStyle);
begin
  CnDockFormControl.SetTabDockHostBorderStyle(Value);
end;

procedure TCnDockPresident.ShowDockForm(DockWindow: TWinControl);
begin
  CnDockFormControl.ShowDockForm(DockWindow);
end;

procedure TCnDockPresident.BeginLoad;
var i: Integer;
begin
  Inc(FLoadCount);
  if FLoadCount = 1 then
  begin
    FDockServersHash.MakeEmpty;
    for i := 0 to FDockServersList.Count - 1 do
      FDockServersHash.Insert(TControl(FDockServersList[i]).Name, FDockServersList[i]);

    FDockClientsHash.MakeEmpty;
    for i := 0 to FDockClientsList.Count - 1 do
      FDockClientsHash.Insert(TControl(FDockClientsList[i]).Name, FDockClientsList[i]);
  end;
end;

procedure TCnDockPresident.EndLoad;
begin
  Dec(FLoadCount);
  if FLoadCount <= 0 then
  begin
    FLoadCount := 0;
    FDockServersHash.MakeEmpty;
    FDockClientsHash.MakeEmpty;
  end;
end;

function TCnDockPresident.IsLoading: Boolean;
begin
  Result := FLoadCount > 0;
end;

procedure TCnDockPresident.BeginSave;
begin
  Inc(FSaveCount);
end;

procedure TCnDockPresident.EndSave;
begin
  Dec(FSaveCount);
  if FSaveCount <= 0 then
    FSaveCount := 0;
end;

function TCnDockPresident.IsSaving: Boolean;
begin
  Result := FSaveCount > 0;
end;

procedure TCnDockPresident.AddDockClientToDockManager(
  AControl: TControl);
begin
  FDockClientsList.Add(AControl);
  FDockClientsHash.Insert(AControl.Name, AControl);
end;

procedure TCnDockPresident.AddDockServerToDockManager(
  AControl: TControl);
begin
  FDockServersList.Add(AControl);
  FDockServersHash.Insert(AControl.Name, AControl);
end;

procedure TCnDockPresident.RemoveDockClientFromDockManager(
  AControl: TControl);
begin
  FDockClientsList.Remove(AControl);
  FDockClientsHash.Remove(AControl.Name);
end;

procedure TCnDockPresident.RemoveDockServerFromDockManager(
  AControl: TControl);
begin
  FDockServersList.Remove(AControl);
  FDockServersHash.Remove(AControl.Name);
end;

procedure TCnDockPresident.BeginDrag(Control: TControl; Immediate: Boolean; Threshold: Integer);
var
  P: TPoint;
begin
  if (TCnControlAccess(Control).DragKind <> dkDock) then
    { 如果Control的DragKind属性不是dkDock,就推出 }
    Exit;
//    raise EInvalidOperation.CreateRes(@SCannotDragForm);
  CalcDockSizes(Control);
  if (DragControl = nil) or (DragControl = Pointer($FFFFFFFF)) then
  begin
    DragControl := nil;
    if csLButtonDown in Control.ControlState then
    begin
      GetCursorPos(P);
      P := Control.ScreenToClient(P);
      Control.Perform(WM_LBUTTONUP, 0, Longint(PointToSmallPoint(P)));
    end;
    { 当Threshold<0的时候，使用默认的值Mouse.DragThreshold }
    if Threshold < 0 then
      Threshold := Mouse.DragThreshold;
    { 防止在BeginDrag里面调用EndDrag }
    if DragControl <> Pointer($FFFFFFFF) then
      DragInitControl(Control, Immediate, Threshold);
  end;
end;

procedure TCnDockPresident.DragInitControl(Control: TControl;
  Immediate: Boolean; Threshold: Integer);
var
//  ADragObject: TCnDragDockObject;
  ARect: TRect;

  procedure DoStartDock;
  begin
    if Assigned(GlobalDockClient) then
      GlobalDockClient.FormStartDock(DragObject);
    if DragObject = nil then
    begin
      DragObject := TCnDragDockObject.Create(Control);
      DragFreeObject := True;
    end;
  end;

begin
  DragControl := Control;
  try
    DragObject := nil;
    DragFreeObject := False;
    { 调用StartDock事件 }
    DoStartDock;
    if DragControl = nil then Exit;
    with DragObject do
    begin
      if Control.HostDockSite is TCnCustomDockPanel then
        ARect := TCnCustomDockPanel(Control.HostDockSite).CnDockManager.GetFrameRectEx(Control)
      else GetWindowRect(TWinControl(Control).Handle, ARect);
      DockRect := ARect;
      EraseDockRect := DockRect;
    end;
    DragInit(DragObject, Immediate, Threshold);
  except
    DragControl := nil;
    raise;
  end;
end;

procedure TCnDockPresident.DragInit(ADragObject: TCnDragDockObject;
  Immediate: Boolean; Threshold: Integer);
begin
  DragObject := ADragObject;
  DragObject.DragTarget := nil;
  GetCursorPos(DragStartPos);
  DragObject.DragPos := DragStartPos;
  DragSaveCursor := Windows.GetCursor;
  DragCapture := DragObject.Capture;
  DragThreshold := Threshold;
  with ADragObject, DockRect do
  begin
    if Right - Left > 0 then
      MouseDeltaX :=  (DragPos.x - Left) / (Right - Left) else
      MouseDeltaX := 0;
    if Bottom - Top > 0 then
      MouseDeltaY :=  (DragPos.y - Top) / (Bottom - Top) else
      MouseDeltaY := 0;
    if Immediate then
    begin
      ActiveDrag := dopDock;
      DrawDragDockImage;
    end
    else ActiveDrag := dopNone;
  end;
  DragImageList := DragObject.GetDragImages;
  if DragImageList <> nil then
    with DragStartPos do DragImageList.BeginDrag(GetDeskTopWindow, X, Y);
  QualifyingSites := TSiteList.Create;
  if ActiveDrag <> dopNone then DragTo(DragStartPos);
end;

procedure TCnDockPresident.DragTo(const Pos: TPoint);
var
  DragCursor: TCursor;
  Target: TControl;
  TargetHandle: HWND;
  DoErase: Boolean;
  TempAlign: TAlign;
//  CanDock: Boolean;
//  R: TRect;
begin
  if {(ActiveDrag <> dopNone) or }(Abs(DragStartPos.X - Pos.X) >= DragThreshold) or
    (Abs(DragStartPos.Y - Pos.Y) >= DragThreshold) then
  begin
    { 查找到停靠目标 }
    Target := DragFindTarget(Pos, TargetHandle, TCnControlAccess(DragControl).DragKind, DragControl);
    if (ActiveDrag = dopNone) and (DragImageList <> nil) then
      with DragStartPos do DragImageList.BeginDrag(GetDeskTopWindow, X, Y);
    DoErase := ActiveDrag <> dopNone;
    ActiveDrag := dopDock;

    if DragObject.CanLeave(TWinControl(Target)) then
    begin
      DoDockOver(dsDragLeave);
      if DragObject = nil then Exit;
      DragObject.DragTarget := Target;
      DragObject.DragHandle := TargetHandle;
      DragObject.DragPos := Pos;
      DoDockOver(dsDragEnter);
      if DragObject = nil then Exit;
    end;
    DragObject.DragPos := Pos;
    if DragObject.DragTarget <> nil then
      DragObject.DragTargetPos := TControl(DragObject.DragTarget).ScreenToClient(Pos);
    DragCursor := DragObject.GetDragCursor(DoDockOver(dsDragMove), Pos.X, Pos.Y);
    if DragImageList <> nil then
    begin
      if (Target = nil) or (csDisplayDragImage in Target.ControlStyle) then
      begin
        DragImageList.DragCursor := DragCursor;
        if not DragImageList.Dragging then
          DragImageList.BeginDrag(GetDeskTopWindow, Pos.X, Pos.Y)
        else DragImageList.DragMove(Pos.X, Pos.Y);
      end
      else begin
        DragImageList.EndDrag;
        Windows.SetCursor(Screen.Cursors[DragCursor]);
      end;
    end;
//    if Target = nil then DragObject.DropOnControl := nil;
    ResetCursor;
    if ActiveDrag = dopDock then
    begin
      with DragObject do
      begin
{        DoGetSiteInfo(Target, Control, R, Pos, CanDock);
        if not CanDock then
        begin
          Target := nil;
          DropOnControl := nil;
        end;}
        if Target = nil then
        begin
          if Assigned(GlobalDockClient) then
            GlobalDockClient.FormPositionDockRect(DragObject);
        end
        else begin
          { 获得DropOnControl }
          DropOnControl := GetDropCtl;
          TempAlign := DropAlign;
          if DropOnControl = nil then
            DoGetDockEdge(TargetControl, DragTargetPos, TempAlign)
          else
            DoGetDockEdge(DropOnControl, DropOnControl.ScreenToClient(Pos), TempAlign);
//          if TempAlign <> DropAlign then
          DropAlign := TempAlign;
        end;
      end;
      if DragObject <> nil then
        DragObject.DrawDragRect(DoErase);
    end;
  end;
end;

function TCnDockPresident.DragFindTarget(const Pos: TPoint; var Handle: HWND;
  DragKind: TDragKind; Client: TControl): Pointer;
begin
  Result := GetDockSiteAtPos(Pos, Client);
  if Result <> nil then
    Handle := TWinControl(Result).Handle;
end;

function TCnDockPresident.DoDockOver(DragState: TDragState): Boolean;
var Target: TControl;
  DockClient: TCnDockClient;
begin
  Result := True;
  if DragObject.DragTarget <> nil then
  begin
    Target := TControl(DragObject.DragTarget);
    with Target.ScreenToClient(DragObject.DragPos) do
      if Target is TCnCustomDockControl then
        TCnCustomDockControl(Target).CustomDockOver(DragObject, X, Y, DragState, Result)
      else if Target is TForm then
      begin
        DockClient := FindDockClient(Target);
        if DockClient <> nil then
          DockClient.FormDockOver(DragObject, X, Y, DragState, Result);
      end;
  end;
end;

function TCnDockPresident.DragFindWindow(const Pos: TPoint): HWND;
begin
  Result := DragObject.DragFindWindow(Pos);
end;

function TCnDockPresident.GetDockSiteAtPos(MousePos: TPoint;
  Client: TControl): TWinControl;
var
  I: Integer;
  R: TRect;
  Site: TWinControl;
  CanDock, ControlKeyDown: Boolean;

  function ValidDockTarget(Target: TWinControl): Boolean;
  var
    Info: TCheckTargetInfo;
    Control: TWinControl;
    R1, R2: TRect;
  begin
    Result := True;
    { Find handle for topmost container of current }
    Info.CurrentWnd := DragFindWindow(MousePos);
    if Info.CurrentWnd = 0 then Exit;
    if (GetWindow(Info.CurrentWnd, GW_OWNER) <> Application.Handle) then
    begin
      Control := FindControl(Info.CurrentWnd);
      if Control = nil then Exit;
      while Control.Parent <> nil do Control := Control.Parent;
      Info.CurrentWnd := Control.Handle;
    end;

    { Find handle for topmost container of target }
    Control := Target;
    while Control.Parent <> nil do Control := Control.Parent;
    Info.TargetWnd := Control.Handle;
    if Info.CurrentWnd = Info.TargetWnd then Exit;

    { Find handle for topmost container of client }
    if Client.Parent <> nil then
    begin
      Control := Client.Parent;
      while Control.Parent <> nil do Control := Control.Parent;
      Info.ClientWnd := Control.Handle;
    end
    else if Client is TWinControl then
      Info.ClientWnd := TWinControl(Client).Handle
    else
      Info.ClientWnd := 0;

    Info.Found := False;
    Info.MousePos := MousePos;
    EnumThreadWindows(GetCurrentThreadID, @IsBeforeTargetWindow, Longint(@Info));
    { CurrentWnd is in front of TargetWnd, so check whether they're overlapped. }
    if Info.Found then
    begin
      GetWindowRect(Info.CurrentWnd, R1);
      DoGetSiteInfo(Target, Client, R2, MousePos, CanDock);
//      TCnWinControlAccess(Target).GetSiteInfo(Client, R2, MousePos, CanDock);
      { Docking control's host shouldn't count as an overlapped window }
      if (DragObject.Control.HostDockSite <> nil)
      and (DragObject.Control.HostDockSite.Handle = Info.CurrentWnd) then
        Exit;
      if IntersectRect(R1, R1, R2) then
        Result := False;
    end;
  end;

  function IsSiteChildOfClient: Boolean;
  begin
    if Client is TWinControl then
      Result := IsChild(TWinControl(Client).Handle, Site.Handle)
    else
      Result := False;
  end;

begin
  Result := nil;
  ControlKeyDown := (GetKeyState(VK_CONTROL) and not $7FFF) <> 0;
  if (DockSiteList = nil) or ControlKeyDown then Exit;
  QualifyingSites.Clear;
  for I := 0 to DockSiteList.Count - 1 do
  begin
    Site := TWinControl(DockSiteList[I]);
    if (Site <> Client) and Site.Showing and Site.Enabled and
      IsWindowVisible(Site.Handle) and (not IsSiteChildOfClient) //and
      {((Client.HostDockSite <> Site) or (Site.VisibleDockClientCount > 1)) }then
    begin
      CanDock := True;
      DoGetSiteInfo(Site, Client, R, MousePos, CanDock);
      if CanDock and PtInRect(R, MousePos) then
        QualifyingSites.AddSite(Site);
    end;
  end;
  if QualifyingSites.Count > 0 then
    Result := QualifyingSites.GetTopSite;
  if (Result <> nil) and not ValidDockTarget(Result) then
    Result := nil;
{  if Result <> nil then
  begin
    DoGetSiteInfo(Site, Client, R, MousePos, CanDock);
    if not CanDock then
      Result := nil;
  end;}
end;

procedure TCnDockPresident.DragDone(Drop: Boolean);

  function CheckUndock: Boolean;
  begin
    Result := DragObject.DragTarget <> nil;
    with DragControl do
      if Drop and (ActiveDrag = dopDock) then
        if Floating or (HostDockSite = nil) then
          Result := True
        else
          Result := DoUnDock(DragObject, DragObject.DragTarget, DragControl);
  end;

  procedure DoFloatForm(Control: TControl);
  var
    WasVisible: Boolean;
  begin
    if (Control.FloatingDockSiteClass = Control.ClassType) then
    begin
      WasVisible := Control.Visible;
      try
        Control.Dock(nil, DragObject.DockRect);
        if (Control.Left <> DragObject.DockRect.Left) or (Control.Top <> DragObject.DockRect.Top) then
        begin
          Control.Left := DragObject.DockRect.Left;
          Control.Top := DragObject.DockRect.Top;
        end;
      finally
        if WasVisible then Control.BringToFront;
      end;
    end;
  end;

var
  DragSave: TCnDragDockObject;
  DockObject: TCnDragDockObject;
  Accepted: Boolean;
  TargetPos: TPoint;
  ParentForm: TCustomForm;
begin
  DockObject := nil;
  DragSave := nil;
  Accepted := False;
  if (DragObject = nil) or DragObject.Cancelling then Exit;  // recursion control
  try
    DragSave := DragObject;
    try
      DragObject.Cancelling := True;
      DragObject.ReleaseCapture(DragCapture);
      if ActiveDrag = dopDock then
      begin
        DockObject := DragObject;
        DockObject.EraseDragDockImage;
        DockObject.Floating := DockObject.DragTarget = nil;
      end;
      if (DragObject.DragTarget <> nil) and
        (TObject(DragObject.DragTarget) is TControl) then
        TargetPos := DragObject.DragTargetPos
      else
        TargetPos := DragObject.DragPos;
      Accepted := CheckUndock and
        {(((ActiveDrag = dopDock) and DockObject.Floating) or
        ((ActiveDrag <> dopNone) and DoDockOver(dsDragLeave))) and}
        Drop;
      if ActiveDrag = dopDock then
      begin
        if Accepted and DockObject.Floating then
        begin
          ParentForm := GetParentForm(DockObject.Control);
          if (ParentForm <> nil) and
            (ParentForm.ActiveControl = DockObject.Control) then
            ParentForm.ActiveControl := nil;
          DoFloatForm(DragControl);
        end;
      end
      else begin
        if DragImageList <> nil then DragImageList.EndDrag
        else Windows.SetCursor(DragSaveCursor);
      end;
      DragControl := nil;
      if DragSave.DragTarget <> nil then
      begin
        if not Accepted then
        begin
          DragSave.DragPos := Point(0, 0);
          TargetPos.X := 0;
          TargetPos.Y := 0;
        end else
//        if Drop then
          DoDockDrop(DragSave, DragSave.DragPos);
      end;
      DragObject := nil;
    finally
      QualifyingSites.Free;
      QualifyingSites := nil;
      DragSave.Cancelling := False;
      DragSave.Finished(DragSave.DragTarget, TargetPos.X, TargetPos.Y, Accepted);
      DragObject := nil;
    end;
  finally
    DragControl := nil;
//    if DragFreeObject then
    DragSave.Free;
  end;
end;

procedure TCnDockPresident.RegisterDockSite(Site: TWinControl;
  DoRegister: Boolean);
var
  Index: Integer;
begin
  if (Site <> nil) then
  begin
    if DockSiteList = nil then DockSiteList := TList.Create;
    Index := DockSiteList.IndexOf(Pointer(Site));
    if DoRegister then
    begin
      if Index = -1 then DockSiteList.Add(Pointer(Site));
    end
    else begin
      if Index <> -1 then DockSiteList.Delete(Index);
    end;
  end;
end;

procedure TCnDockPresident.DoGetSiteInfo(Target, Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
var DockClient: TCnDockClient;
begin
  if Target is TCnCustomDockControl then
    TCnCustomDockControl(Target).CustomGetSiteInfo(DragObject, Client, InfluenceRect, MousePos, CanDock)
  else if Target is TForm then
  begin
    DockClient := FindDockClient(Target);
    if DockClient <> nil then
      DockClient.FormGetSiteInfo(DragObject, Client, InfluenceRect, MousePos, CanDock);
  end
  else CanDock := False;
end;

procedure TCnDockPresident.DoDockDrop(Source: TCnDragDockObject; Pos: TPoint);
var Target: TWinControl;
  DockClient: TCnDockClient;
begin
  if Source.DragTarget <> nil then
  begin
    Target := Source.TargetControl;
    with Target.ScreenToClient(Pos) do
      if Target is TCnCustomDockControl then
        TCnCustomDockControl(Target).CustomDockDrop(Source, X, Y)
      else if Target is TForm then
      begin
        DockClient := FindDockClient(Target);
        if DockClient <> nil then
          DockClient.FormDockDrop(Source, X, Y);
      end;
  end;
end;

function TCnDockPresident.DoUnDock(Source: TCnDragDockObject; Target: TWinControl; Client: TControl): Boolean;
begin
  if Client.HostDockSite is TCnCustomDockControl then
    Result := TCnCustomDockControl(Client.HostDockSite).CustomUnDock(Source, Target, Client)
  else Result := False;
end;

procedure TCnDockPresident.DoEndDrag(Target: TObject; X, Y: Integer);
var DockClient: TCnDockClient;
begin
  if Target is TCnCustomDockControl then
    TCnCustomDockControl(Target).CustomEndDock(Target, X, Y)
  else if Target is TForm then
  begin
    DockClient := FindDockClient(TControl(Target));
    if DockClient <> nil then
      DockClient.FormEndDock(Target, X, Y);
  end;
end;

procedure TCnDockPresident.CalcDockSizes(Control: TControl);
var //BorderWidth: Integer;
  Rect: TRect;
begin
  with Control do
    if Floating then
    begin
      UndockHeight := Height;
      UndockWidth := Width;
    end
    else if HostDockSite is TCnCustomDockPanel then
    begin
      //BorderWidth := TCnCustomDockPanel(HostDockSite).CnDockManager.BorderWidth;
      Rect := TCnCustomDockPanel(HostDockSite).CnDockManager.GetFrameRect(Control);
      if (HostDockSite.Align in [alTop, alBottom]) then
        TBDockHeight := Rect.Bottom - Rect.Top//Height + 2*BorderWidth
      else if (HostDockSite.Align in [alLeft, alRight]) then
        LRDockWidth := Rect.Right - Rect.Left;//Width + 2*BorderWidth;
    end;
end;

procedure TCnDockPresident.CancelDrag;
begin
  if DragObject <> nil then DragDone(False);
  DragControl := nil;
end;

procedure TCnDockPresident.DoGetDockEdge(Target: TControl; MousePos: TPoint; var DropAlign: TAlign);
var DockClient: TCnDockClient;
begin
  if Target is TCnCustomDockControl then
    TCnCustomDockControl(Target).CustomGetDockEdge(DragObject, MousePos, DropAlign)
  else if Target is TForm then
  begin
    DockClient := FindDockClient(Target);
    if DockClient <> nil then
      DockClient.FormGetDockEdge(DragObject, MousePos, DropAlign);
  end;
end;

procedure TCnDockPresident.ResetCursor;
begin
  if (GlobalDockClient <> nil) and (GlobalDockClient.DockStyle <> nil) then
    GlobalDockClient.DockStyle.ResetCursor(DragObject);
end;

{ TSiteList }

procedure TSiteList.AddSite(ASite: TWinControl);

  function GetTopParent: HWND;
  var
    NextParent: HWND;
  begin
    NextParent := ASite.Handle;
    Result := NextParent;
    while NextParent <> 0 do
    begin
      Result := NextParent;
      NextParent := GetParent(NextParent);
    end;
  end;

var
  SI: PSiteInfoRec;
  Index: Integer;
begin
  New(SI);
  SI.Site := ASite;
  SI.TopParent := GetTopParent;
  if Find(SI.TopParent, Index) then
    Insert(Index, SI) else
    Add(SI);
end;

procedure TSiteList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dispose(PSiteInfoRec(Items[I]));
  inherited Clear;
end;

function TSiteList.Find(ParentWnd: Hwnd; var Index: Integer): Boolean;
begin
  Index := 0;
  Result := False;
  while Index < Count do
  begin
    Result := (PSiteInfoRec(Items[Index]).TopParent = ParentWnd);
    if Result then Exit;
    Inc(Index);
  end;
end;

function TSiteList.GetTopSite: TWinControl;
var
  Index: Integer;
  DesktopWnd, CurrentWnd: HWND;
begin
  Result := nil;
  if Count = 0 then Exit
  else if Count = 1 then Result := PSiteInfoRec(Items[0]).Site
  else begin
    DesktopWnd := GetDesktopWindow;
    CurrentWnd := GetTopWindow(DesktopWnd);
    while (Result = nil) and (CurrentWnd <> 0) do
    begin
      if Find(CurrentWnd, Index) then
        Result := PSiteInfoRec(List[Index])^.Site
      else
        CurrentWnd := GetNextWindow(CurrentWnd, GW_HWNDNEXT);
    end;
  end;
end;

{ TCustomDockPanelSplitter }

constructor TCustomDockPanelSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Align := alLeft;
  Width := 3;
  Cursor := crHSplit;
  FMinSize := 30;
  FResizeStyle := rsPattern;
  FOldSize := -1;
end;

destructor TCustomDockPanelSplitter.Destroy;
begin
  FBrush.Free;
  inherited Destroy;
end;

procedure TCustomDockPanelSplitter.AllocateLineDC;
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

procedure TCustomDockPanelSplitter.DrawLine;
var
  P: TPoint;
begin
  FLineVisible := not FLineVisible;
  P := Point(Left, Top);
  if Align in [alLeft, alRight] then
    P.X := Left + FSplit else
    P.Y := Top + FSplit;
  with P do PatBlt(FLineDC, X, Y, Width, Height, PATINVERT);
end;

procedure TCustomDockPanelSplitter.ReleaseLineDC;
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

function TCustomDockPanelSplitter.FindControl: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft: Dec(P.X);
    alRight: Inc(P.X, Width);
    alTop: Dec(P.Y);
    alBottom: Inc(P.Y, Height);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled then
    begin
      R := Result.BoundsRect;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if PtInRect(R, P) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TCustomDockPanelSplitter.RequestAlign;
begin
  inherited RequestAlign;
  if (Cursor <> crVSplit) and (Cursor <> crHSplit) then Exit;
  if Align in [alBottom, alTop] then
    Cursor := crVSplit
  else
    Cursor := crHSplit;
end;

procedure TCustomDockPanelSplitter.Paint;
const
  XorColor = $00FFD8CE;
var
  FrameBrush: HBRUSH;
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);
  if Beveled then
  begin
    if Align in [alLeft, alRight] then
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

function TCustomDockPanelSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  Result := CanResize(NewSize);
  if Result and (NewSize <= MinSize) and FAutoSnap then
    NewSize := 0;
end;

function TCustomDockPanelSplitter.CanResize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(FOnCanResize) then FOnCanResize(Self, NewSize, Result);
end;

procedure TCustomDockPanelSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
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
      if Align in [alLeft, alRight] then
      begin
        FMaxSize := Parent.ClientWidth - FMinSize;
        for I := 0 to Parent.ControlCount - 1 do
          with Parent.Controls[I] do
            if Visible and (Align in [alLeft, alRight]) then Dec(FMaxSize, Width);
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
          FOldKeyDown := TCnWinControlAccess(FActiveControl).OnKeyDown;
          TCnWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
        end;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TCustomDockPanelSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft: FControl.Width := FNewSize;
      alTop: FControl.Height := FNewSize;
      alRight:
        begin
          Parent.DisableAlign;
          try
            FControl.Left := FControl.Left + (FControl.Width - FNewSize);
            FControl.Width := FNewSize;
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
          finally
            Parent.EnableAlign;
          end;
        end;
    end;
    TCnControlAccess(FControl).Resize;
    Update;
    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
//    Canvas.FillRect(GetClientRect);
  end;
end;

procedure TCustomDockPanelSplitter.CalcSplitSize(X, Y: Integer; var NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
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
    if Align in [alRight, alBottom] then
      S := S - NewSize else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

procedure TCustomDockPanelSplitter.UpdateSize(X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TCustomDockPanelSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewSize, Split: Integer;
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if (DoCanResize(NewSize) and (FNewSize <> NewSize)) then
    begin
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      if ResizeStyle = rsUpdate then UpdateControlSize;
      if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TCustomDockPanelSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState;
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

procedure TCustomDockPanelSplitter.FocusKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    StopSizing
  else if Assigned(FOldKeyDown) then
    FOldKeyDown(Sender, Key, Shift);
end;

procedure TCustomDockPanelSplitter.SetBeveled(Value: Boolean);
begin
  FBeveled := Value;
  Repaint;
end;

procedure TCustomDockPanelSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    if FLineVisible then DrawLine;
    FControl := nil;
    ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TCnWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;
  end;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;


end.
