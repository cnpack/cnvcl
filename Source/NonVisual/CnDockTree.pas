{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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
{       管理停靠控件的管理器                            }
{       CnDockTree 单元                                 }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnDockTree;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：管理停靠控件的管理器 
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

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Consts, CnDockSupportClass;

const
  { 鼠标位置在分割条上面 }
  HTSPLITTER = 30;
  { 没有在特定的位置上 }
  HTNONE = 31;

type

  TCnDockTree = class;

  // ICnDockManager接口继承自IDockManager，用户可以在ICnDockManager中再定义一些方法，函数或属性。
  ICnDockManager = interface(IDockManager)
    ['{7B0AACBC-E9BF-42F8-9629-E551067090B2}']
    function GetActiveControl: TControl;               //获得焦点的控件
    procedure SetActiveControl(const Value: TControl); //设置某个控件获得焦点
    function GetGrabberSize: Integer;                  //获得把手的大小
    procedure SetGrabberSize(const Value: Integer);    //设置把手的大小
    function GetSplitterWidth: Integer;                //获得分割条的宽度
    procedure SetSplitterWidth(const Value: Integer);  //设置分割条的宽度
    function GetBorderWidth: Integer;                  //获得边框的宽度
    procedure SetBorderWidth(const Value: Integer);    //设置边框的宽度
    function GetDockRect: TRect;                       //获得停靠矩形
    procedure SetDockRect(const Value: TRect);         //设置停靠矩形

    function GetDockSiteSize: Integer;                 //获得停靠服务器的宽度或者高度
    procedure SetDockSiteSize(const Value: Integer);   //设置停靠服务器的宽度或者高度

    function GetMinSize: Integer;                      //获得停靠客户之间的最小距离

    procedure BeginResizeDockSite;
    procedure EndResizeDockSite;

    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl;  //获得停靠预览矩形下面的Control;

    function GetHTFlag(MousePos: TPoint): Integer;

    procedure GetSiteInfo(Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);

    procedure ShowControl(Control: TControl);           //显示Control
    procedure HideControl(Control: TControl);           //隐藏Control

    procedure ShowAllControl;                           // 显示所有的Control
    procedure HideAllControl;                           // 隐藏所有的Control
    procedure ShowSingleControl(Control: TControl);     // 只显示一个Control，其他的都隐藏
    procedure HideSingleControl(Control: TControl);     // 只隐藏一个Control，其他的都显示

    { 用新的NewControl替换老的OldControl }
    procedure ReplaceZoneChild(OldControl, NewControl: TControl);

    { 根据输入的Control参数查找Zone，如果找到就返回True，否则返回False }
    function HasZoneWithControl(Control: TControl): Boolean;

    { 获得停靠服务器上的停靠客户的极限, 相对于DockSite }
    function GetDockClientLimit(Orient: TDockOrientation; IsMin: Boolean): Integer;

    function GetFrameRect(Control: TControl): TRect;   //获得停靠控件的矩形大小，相对于DockSite。
    function GetFrameRectEx(Control: TControl): TRect; //获得停靠控件的矩形大小，相对于屏幕。
    property ActiveControl: TControl read GetActiveControl
      write SetActiveControl;
    property GrabberSize: Integer read GetGrabberSize
      write SetGrabberSize;
    property SplitterWidth: Integer read GetSplitterWidth
      write SetSplitterWidth;
    property BorderWidth: Integer read GetBorderWidth
      write SetBorderWidth;

    property DockSiteSize: Integer read GetDockSiteSize write SetDockSiteSize;

    property DockRect: TRect read GetDockRect write SetDockRect;

    property MinSize: Integer read GetMinSize;
  end;

  TCnDockZone = class
  private
    FChildControl: TWinControl;     //上面的控件
    FChildZones: TCnDockZone;       //左子女
    FNextSibling: TCnDockZone;      //右兄弟
    FOrientation: TDockOrientation; //停靠的方式，是水平还是垂直，或者没有
    FParentZone: TCnDockZone;       //父亲
    FPrevSibling: TCnDockZone;      //上一个兄弟
    FTree: TCnDockTree;             //属于哪棵树
    FZoneLimit: Integer;            //节点的下边或者右边的坐标
                                    //如果FOrientation是水平的，就表示下边的坐标，
                                    //如果FOrientation是垂直的，就表示右边的坐标。
    FVisibleSize: Integer;          //存储上当Zone可见的时候的ZoneSize的值,
    FVisibled: Boolean;             //是否可见
    FControlVisibled: Boolean;      //ChildControl是否可见
    FIsInside: Boolean;             //是否在DockSite的里面
    function GetFirstSibling: TCnDockZone;//获得最前一个兄弟
    function GetLastSibling: TCnDockZone; //获得最后一个兄弟
    function GetFirstChild: TCnDockZone;  //获得最前一个子女
    function GetLastChild: TCnDockZone;   //获得最后一个子女
    function GetTopLeftArr(Orient: TDockOrientation): Integer;
    function GetHeightWidthArr(Orient: TDockOrientation): Integer;
    function GetAfterClosestVisibleZone: TCnDockZone;
    function GetBeforeClosestVisibleZone: TCnDockZone;
    function GetAfterApoapsisVisibleZone: TCnDockZone;
    function GetBeforeApoapsisVisibleZone: TCnDockZone;
    function GetNextSiblingCount: Integer;
    function GetPrevSiblingCount: Integer;
    procedure SetVisibled(const Value: Boolean);
    procedure SetZoneLimit(const Value: Integer);
    function GetVisibleNextSiblingCount: Integer;
    function GetVisibleNextSiblingTotal: Integer;
    function GetVisiblePrevSiblingCount: Integer;
    function GetVisiblePrevSiblingTotal: Integer;
    function GetFirstVisibleChildZone: TCnDockZone;
    function GetLastVisibleChildZone: TCnDockZone;
    procedure SetIsInside(const Value: Boolean);
  protected
    procedure AdjustZoneLimit(Value: Integer); virtual;//改变FZoneLimit值并且调整她的兄弟
    procedure LButtonDbClkMothed; virtual;    // 当鼠标左键双击的时候，判断Zone上的ChildControl如何动作
    function GetChildCount: Integer;          //获得子女的个数
    function GetVisibleChildCount: Integer;   //获得可见子女的个数
    function GetChildTotal: Integer;          //获得末段子女的总数
    function GetVisibleChildTotal: Integer;   //获得末段可见子女的个数
    function GetLimitBegin: Integer;          //获得节点的上边或者左边的坐标
    function GetLimitSize: Integer;           //获得节点的高度或者宽度
    function GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
    function GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
    function GetControlName: string;//获得控件的名称，如果没有控件就返回空字符串
    { 获得当前的节点内的子节点的分割条的ZoneLimit, 如果IsMin为True, 说明是取最小值，反之就取最大值 }
    function GetSplitterLimit(IsMin: Boolean): Integer; virtual;
    function DoGetSplitterLimit(Orientation: TDockOrientation;
      IsMin: Boolean; var LimitResult: Integer): Integer; virtual;
    {查找名字为Value的控件，如果找到就把她停靠进FTree中}
    function SetControlName(const Value: string): Boolean;
    procedure DoCustomSetControlName; virtual;
    procedure SetChildControlVisible(Client: TControl; AViisible: Boolean); virtual;
  public
    constructor Create(Tree: TCnDockTree); virtual;
    { 插入一个大小为DockSize的节点 }
    procedure Insert(DockSize: Integer; Hide: Boolean); virtual;
    { 去掉一个大小为DockSize的节点 }
    procedure Remove(DockSize: Integer; Hide: Boolean); virtual;
    { 当有一个控件停靠进来的时候，调用InsertOrRemove重新调整ParentZone上的子节点,
      其中Insert指示是否是插入操作，如果Insert=True就是插入操作，否者就是删除操作 }
    procedure InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean); virtual;
    procedure ResetChildren(Exclude: TCnDockZone); virtual;//重新设置子女的属性，如坐标等
    { 更新当前节点(不包括子节点)，调整当前的ChildControl的位置和大小 }
    procedure Update; virtual;
    { 获得这个节点的矩形位置 }
    function GetFrameRect: TRect; virtual;
    { 设置节点的大小 }
    procedure SetZoneSize(Size: Integer; Show: Boolean); virtual;
    { 离本节点最近的可见的前兄弟节点 }
    property BeforeClosestVisibleZone: TCnDockZone read GetBeforeClosestVisibleZone;
    { 离本节点最近的可见的后兄弟节点 }
    property AfterClosestVisibleZone: TCnDockZone read GetAfterClosestVisibleZone;
    { 离本节点最远的可见的前兄弟节点 }
    property BeforeApoapsisVisibleZone: TCnDockZone read GetBeforeApoapsisVisibleZone;
    { 离本节点最远的可见的后兄弟节点 }
    property AfterApoapsisVisibleZone: TCnDockZone read GetAfterApoapsisVisibleZone;
    { 第一个可见的子女节点 }
    property FirstVisibleChildZone: TCnDockZone read GetFirstVisibleChildZone;
    { 第一个可见的子女节点 }
    property LastVisibleChildZone: TCnDockZone read GetLastVisibleChildZone;
    { 子女个数 }
    property ChildCount: Integer read GetChildCount;
    { 末端子女个数 }
    property ChildTotal: Integer read GetChildTotal;
    { 第一个子女 }
    property ChildZones: TCnDockZone read FChildZones write FChildZones;
    { 在节点上的Control控件 }
    property ChildControl: TWinControl read FChildControl write FChildControl;
    { 第一个子女 }
    property FirstChild: TCnDockZone read GetFirstChild;
    { 第一个兄弟 }
    property FirstSibling: TCnDockZone read GetFirstSibling;
    { 高度 }
    property Height: Integer index Ord(doHorizontal) read GetHeightWidth;
    { 根据停靠方向来获得高度和宽度 }
    property HeightWidth[Orient: TDockOrientation]: Integer read GetHeightWidthArr;
    { 最后一个子女 }
    property LastChild: TCnDockZone read GetLastChild;
    { 最后一个兄弟 }
    property LastSibling: TCnDockZone read GetLastSibling;
    { 左边位置 }
    property Left: Integer index Ord(doVertical) read GetTopLeft;
    { 开始位置的坐标 }
    property LimitBegin: Integer read GetLimitBegin;
    { 大小 }
    property LimitSize: Integer read GetLimitSize;
    { 下一个兄弟 }
    property NextSibling: TCnDockZone read FNextSibling write FNextSibling;
    { 获得后兄弟的个数 }
    property NextSiblingCount: Integer read GetNextSiblingCount;
    { 停靠方向 }
    property Orientation: TDockOrientation read FOrientation write FOrientation;
    { 父节点 }
    property ParentZone: TCnDockZone read FParentZone write FParentZone;
    { 上一个兄弟 }
    property PrevSibling: TCnDockZone read FPrevSibling write FPrevSibling;
    { 获得前兄弟的个数 }
    property PrevSiblingCount: Integer read GetPrevSiblingCount;
    { 上边位置 }
    property Top: Integer index Ord(doHorizontal) read GetTopLeft;
    { 根据停靠方向来获得左边和上边位置 }
    property TopLeft[Orient: TDockOrientation]: Integer read GetTopLeftArr;
    { 属于哪个树 }
    property Tree: TCnDockTree read FTree write FTree;
    { 可见子女节点的个数 }
    property VisibleChildCount: Integer read GetVisibleChildCount;
    { 可见末端节点的个数 }
    property VisibleChildTotal: Integer read GetVisibleChildTotal;
    { 可见的上兄弟的个数 }
    property VisiblePrevSiblingCount: Integer read GetVisiblePrevSiblingCount;
    { 可见的上兄弟的末端节点的个数 }
    property VisiblePrevSiblingTotal: Integer read GetVisiblePrevSiblingTotal;
    { 可见的下兄弟的个数 }
    property VisibleNextSiblingCount: Integer read GetVisibleNextSiblingCount;
    { 可见的下兄弟的末端节点的个数 }
    property VisibleNextSiblingTotal: Integer read GetVisibleNextSiblingTotal;
    { 可见的大小 }
    property VisibleSize: Integer read FVisibleSize write FVisibleSize;
    { 宽度 }
    property Width: Integer index Ord(doVertical) read GetHeightWidth;
    { 相对于DockSite的绝对位置 }
    property ZoneLimit: Integer read FZoneLimit write SetZoneLimit;
    { 是否可见 }
    property Visibled: Boolean read FVisibled write SetVisibled;
    { 是否在里面 }
    property IsInside: Boolean read FIsInside write SetIsInside;
  end;

  TCnAdvDockZone = class(TCnDockZone)
  private
    FCloseBtnDown: Boolean;
    FMouseDown: Boolean;
  protected
    procedure LButtonDbClkMothed; override;
  public
    constructor Create(Tree: TCnDockTree); override;
    destructor Destroy; override;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
    property CloseBtnDown: Boolean read FCloseBtnDown write FCloseBtnDown;
    property MouseDown: Boolean read FMouseDown write FMouseDown;
  end;

  { 树的遍历方式，分别是前序，中序和后序遍历 }
  TTreeScanKind = (tskForward, tskMiddle, tskBackward);

  { 树的遍历优先级别，分别是先遍历兄弟，先遍历子女 }
  TTreeScanPriority = (tspSibling, tspChild);

  TCnForEachZoneProc = procedure(Zone: TCnDockZone) of object;

  { 把手的位置，可以有四个位置，分别是上下左右 }
  TGrabbersPosition = (gpTop, gpBottom, gpLeft, gpRight);

  TCnDockZoneClass = class of TCnDockZone;

  TCnDockTree = class(TInterfacedObject, ICnDockManager)
  private

    {节点类的引用}
    FCnDockZoneClass: TCnDockZoneClass;
    {在树中哪个控件获得了焦点}
    FActiveControl: TControl;

    FBorderWidth: Integer;         //边框的宽度

    FSplitterWidth: Integer;       //分割条的宽度
    FBrush: TBrush;                //用来画把手的刷子
    FDockSite: TWinControl;        //停靠的服务控件
    FGrabberSize: Integer;         //把手的大小
    FOldRect: TRect;               //当DockSite的大小调整的时候，
                                   //这个值存储最后一次的DockSite的大小
    FDockRect: TRect;

    FOldWndProc: TWndMethod;
    FReplacementZone: TCnDockZone;
    FResizeCount: Integer;         //调整DockSite大小的计数器
    FScaleBy: Double;              //比例的大小

    { 当进行偏移量调整的时候，用来指示到底是什么停靠方向的Zone需要调整 }
    FShiftScaleOrient: TDockOrientation;
    FShiftBy: Integer;             //偏移量
    FSizePos: TPoint;              //当鼠标点击分割条时，记录下鼠标的坐标
    FSizingDC: HDC;                //画分割条的设备上下文
    FSizingWnd: HWND;
    FSizingZone: TCnDockZone;      //分割条属于哪个节点
    FTopZone: TCnDockZone;         //根节点
    FTopXYLimit: Integer;
    FUpdateCount: Integer;         //更新计数器
    FVersion: Integer;             //版本
    FOldHTFlag: Integer;           //老的鼠标的位置
    FParentLimit: Integer;         //这个值是用来在调整ZoneLimit的时候用的，
                                   //具体公式见ScaleChildZone函数
    FMinSize: Integer;             //停靠控件之间的最小距离

    FCanvas: TControlCanvas;       //用来画DockSite的画布

    procedure SetTopZone(const Value: TCnDockZone);
    procedure SetTopXYLimit(const Value: Integer);
    { 设置TCnDockZone的类引用 }
    procedure SetCnDockZoneClass(const Value: TCnDockZoneClass);

    { 获得分割条的宽度 }
    function GetSplitterWidth: Integer;
    { 获得边框的宽度 }
    function GetBorderWidth: Integer;
    { 设置分割条的宽度 }
    procedure SetSplitterWidth(const Value: Integer);
    { 设置边框的宽度 }
    procedure SetBorderWidth(const Value: Integer);
    function GetDockSiteOrient: TDockOrientation;
    function GetDockSiteSize: Integer;
    procedure SetDockSiteSize(const Value: Integer);
    procedure SetMinSize(const Value: Integer);
    function GetDockSiteBegin: Integer;
    procedure SetDockSiteBegin(const Value: Integer);
    function GetDockSiteSizeA: Integer;
    procedure SetDockSiteSizeA(const Value: Integer);
    procedure SetVersion(const Value: Integer);
    function GetDockSiteSizeWithOrient(Orient: TDockOrientation): Integer;
    procedure SetDockSiteSizeWithOrient(Orient: TDockOrientation;
      const Value: Integer);
    function GetDockRect: TRect;                       //获得停靠矩形
    procedure SetDockRect(const Value: TRect);        //设置停靠矩形
    function GetMinSize: Integer;
  protected
    function HasZoneWithControl(Control: TControl): Boolean;
    { 捕获的DockSite的窗口消息 }
    procedure WindowProc(var Message: TMessage); virtual;
    { ------------------------------------------------------------------------ }
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); virtual;
    { ------------------------------------------------------------------------ }
    function DoMouseEvent(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): TWMNCHitMessage; virtual;
    { ------------------------------------------------------------------------ }
    { 当DockSite上有鼠标移动的时候调用DoMouseMove方法 }
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标左键按下的时候调用DoLButtonDown函数 }
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): Boolean; virtual;
    { 当DockSite上有鼠标左键释放的时候调用DoLButtonUp方法 }
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标左键双击的时候调用DoLButtonDbClk方法 }
    procedure DoLButtonDbClk(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标中键按下的时候调用DoLButtonDown函数 }
    procedure DoMButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标中键释放的时候调用DoLButtonUp方法 }
    procedure DoMButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标左键双击的时候调用DoMButtonDbClk方法 }
    procedure DoMButtonDbClk(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标右键按下的时候调用DoLButtonDown函数 }
    procedure DoRButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标右键释放的时候调用DoLButtonUp方法 }
    procedure DoRButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上有鼠标左键双击的时候调用DoRButtonDbClk方法 }
    procedure DoRButtonDbClk(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { ------------------------------------------------------------------------ }
    { 隐藏AZone中的ChildControl }
    procedure DoHideZoneChild(AZone: TCnDockZone); virtual;
    { ------------------------------------------------------------------------ }
    { 当DockSite上要设置光标形状的时候调用DoSetCursor方法 }
    procedure DoSetCursor(var Message: TWMSetCursor;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 当DockSite上出现提示框的时候调用DoHintShow方法 }
    procedure DoHintShow(var Message: TCMHintShow;
      var Zone: TCnDockZone; out HTFlag: Integer); virtual;
    { 其他的提示信息 }
    procedure DoOtherHint(Zone: TCnDockZone;
      HTFlag: Integer; var HintStr: string); virtual;
    { ------------------------------------------------------------------------ }
    procedure CustomSaveZone(Stream: TStream;
      Zone: TCnDockZone); virtual;
    procedure CustomLoadZone(Stream: TStream;
      var Zone: TCnDockZone); virtual;
    procedure DoSaveZone(Stream: TStream;
      Zone: TCnDockZone; Level: Integer); virtual;
    procedure DoLoadZone(Stream: TStream); virtual;
    { ------------------------------------------------------------------------ }
    { 调整Control控件的大小 }
    procedure AdjustDockRect(Control: TControl; var ARect: TRect); virtual;
    { 开始调整DockSite的大小，简单的使FResizeCount加一 }
    procedure BeginResizeDockSite;
    { 开始更新，简单的使FUpdateCount加一 }
    procedure BeginUpdate;
    { 计算并且限制分割条的位置 }
    procedure CalcSplitterPos; virtual;
    { 当Control控件的Visible属性改变的时候，调用ControlVisibilityChanged方法 }
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean); virtual;
    { 获得Client在DropCtl中的位置 }
    function GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign; virtual;
    { 这个函数是确定光标是在DockSite的什么位置 }
    function DoFindZone(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    { 画分割条移动的时候的外形 }
    procedure DrawSizeSplitter; virtual;
    { 结束调整DockSite的大小，把FResizeCount减一 }
    procedure EndResizeDockSite;
    { 结束更新，把FUpdateCount减一，如果FUpdateCount小于等于零的时候，就调用UpdateAll方法更新 }
    procedure EndUpdate;
    ////////////////////////////////////////////////////////////////////////////
    { 根据输入的Control参数查找到对应的Zone }
    function FindControlZone(Control: TControl; IncludeHide: Boolean = False): TCnDockZone; virtual;
    { 根据输入的Control参数查找到对应的Zone，并且返回这个Zone的Level }
    function FindControlZoneAndLevel(Control: TControl;
      var CtlLevel: Integer; IncludeHide: Boolean = False): TCnDockZone; virtual;
    ////////////////////////////////////////////////////////////////////////////
    { 对整棵树进行遍历 }
    procedure ForEachAt(Zone: TCnDockZone; Proc: TCnForEachZoneProc;
      ScanKind: TTreeScanKind = tskForward; ScanPriority: TTreeScanPriority = tspSibling); virtual;
    { 获得在DockSite中的活动的Control控件 }
    function GetActiveControl: TControl; virtual;
    { 获得把手的大小 }
    function GetGrabberSize: Integer; virtual;
    ////////////////////////////////////////////////////////////////////////////
    function GetBorderHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetLeftGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetRightGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    function GetBottomGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; virtual;
    ////////////////////////////////////////////////////////////////////////////
    { 获得停靠预览矩形下面的Control }
    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; virtual;
    ////////////////////////////////////////////////////////////////////////////
    { 获得停靠服务器上的停靠客户的极限, 相对于DockSite }
    function GetDockClientLimit(Orient: TDockOrientation; IsMin: Boolean): Integer; virtual;
    { 获得把手的矩形大小 }
    function GetFrameRect(Control: TControl): TRect; virtual;
    function GetFrameRectEx(Control: TControl): TRect; virtual;
    { 获得分割条的矩形大小 }
    function GetSpiltterRect(Zone: TCnDockZone): TRect; virtual;
    { 设置把手在什么位置 }
    function GetGrabbersPosition: TGrabbersPosition; virtual;
    { 获得Control控件的大小 }
    procedure GetControlBounds(Control: TControl; out CtlBounds: TRect); virtual;
    { 获得分割条的Limit }
    function GetSplitterLimit(AZone: TCnDockZone; IsCurrent, IsMin: Boolean): Integer; virtual;
    procedure DoGetNextLimit(Zone, AZone: TCnDockZone; var LimitResult: Integer); virtual;
    { 获得MousePos位置的HTFlag }
    function GetHTFlag(MousePos: TPoint): Integer; virtual;
    procedure GetSiteInfo(Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    { 根据输入的鼠标位置判断它下面的Control控件 }
    function HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl; virtual;
    { 根据输入的鼠标位置判断它下面的Zone }
    function InternalHitTest(const MousePos: TPoint;
      out HTFlag: Integer): TCnDockZone; virtual;
    { 插入一个控件，其中InsertAt是插入的位置 }
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); virtual;
    { ------------------------------------------------------------------------ }
    { 创建一个新的Zone，然后把NewZone和SiblingZone作为它的ChildZones }
    procedure InsertNewParent(NewZone, SiblingZone: TCnDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); virtual;
    { 把NewZone作为SiblingZone的兄弟 }
    procedure InsertSibling(NewZone, SiblingZone: TCnDockZone;
        InsertLast, Update: Boolean); virtual;
    { ------------------------------------------------------------------------ }
    { 从流中装载停靠信息 }
    procedure LoadFromStream(Stream: TStream); virtual;
    { 把停靠信息存储到流中 }
    procedure SaveToStream(Stream: TStream); virtual;

    { 以下函数是画图函数 }
    {==========================================================================}
    { 重画整个区域 }
    procedure PaintDockSite; virtual;
    { 重画HostDockSite的矩形外形 }
    procedure DrawDockSiteRect; virtual;
    { 重画每一个节点 }
    procedure DrawZone(Zone: TCnDockZone); virtual;
    { 重画把手 }
    procedure DrawZoneGrabber(Zone: TCnDockZone); virtual;
    procedure DrawDockGrabber(Control: TControl; const ARect: TRect); virtual;
    { 重画分割条 }
    procedure DrawZoneSplitter(Zone: TCnDockZone); virtual;
    procedure DrawSplitterRect(const ARect: TRect); virtual;
    { 重画边框 }
    procedure DrawZoneBorder(Zone: TCnDockZone); virtual;
    { R1为内框大小，R2为外框大小 }
    procedure DrawDockBorder(DockControl: TControl; R1, R2: TRect); virtual;

    {==========================================================================}
    { 得到标题栏的大小 }
    procedure GetCaptionRect(var Rect: TRect); virtual;
    { 定位停靠位置 }
    procedure PositionDockRect(Client, DropCtl: TControl;
      DropAlign: TAlign; var DockRect: TRect); virtual;
    ////////////////////////////////////////////////////////////////////////////
    { 删除全部Zone }
    procedure PruneZone(Zone: TCnDockZone); virtual;
    { 删除单个Zone }
    procedure RemoveZone(Zone: TCnDockZone; Hide: Boolean = True); virtual;
    { 设置Zone的比例 }
    procedure ScaleZone(Zone: TCnDockZone); virtual;

    procedure ScaleChildZone(Zone: TCnDockZone); virtual;   //遍历的时候调用这个函数调整
                                                            //子女Zone的ZoneLimit的值
    procedure ScaleSiblingZone(Zone: TCnDockZone); virtual; //遍历的时候调用这个函数调整
                                                            //兄弟Zone子女Zone的ZoneLimit的值
    { 调整Zone的偏移量 }
    procedure ShiftZone(Zone: TCnDockZone); virtual;
    { 更新Zone }
    procedure UpdateZone(Zone: TCnDockZone); virtual;
    { 画分割条 }
    procedure DrawSplitter(Zone: TCnDockZone); virtual;
    ////////////////////////////////////////////////////////////////////////////
    { 删除Control控件 }
    procedure RemoveControl(Control: TControl); virtual;
    { 设置DockSite中的活动Control控件 }
    procedure SetActiveControl(const Value: TControl); virtual;
    { 设置把手的大小 }
    procedure SetGrabberSize(const Value: Integer); virtual;
    { 设置Zone中ChildControl的大小，包括Zone的ChildZones }
    procedure SetNewBounds(Zone: TCnDockZone); virtual;
    procedure SetReplacingControl(Control: TControl);
    { 当鼠标点击分割条的时候调用SplitterMouseDown方法 }
    procedure SplitterMouseDown(OnZone: TCnDockZone; MousePos: TPoint); virtual;
    { 当鼠标释放分割条的时候调用SplitterMouseUp方法 }
    procedure SplitterMouseUp; virtual;
    { 重新设置范围 }
    procedure ResetBounds(Force: Boolean); virtual;
    { 把控件的名称写到流Stream里面 }
    procedure WriteControlName(Stream: TStream; ControlName: string);
    { 流Stream里面读出控件的名称 }
    procedure ReadControlName(Stream: TStream; var ControlName: string);
    ////////////////////////////////////////////////////////////////////////////
    procedure ShowControl(Control: TControl);           //显示Control
    procedure HideControl(Control: TControl);           //隐藏Control
    procedure ShowAllControl;                           // 显示所有的Control
    procedure HideAllControl;                           // 隐藏所有的Control
    procedure ShowSingleControl(Control: TControl);     // 只显示一个Control，其他的都隐藏
    procedure HideSingleControl(Control: TControl);     // 只隐藏一个Control，其他的都显示
    ////////////////////////////////////////////////////////////////////////////
    { 用新的NewControl替换老的OldControl }
    procedure ReplaceZoneChild(OldControl, NewControl: TControl);
    ////////////////////////////////////////////////////////////////////////////
    property BorderWidth: Integer read GetBorderWidth write SetBorderWidth;
    property Canvas: TControlCanvas read FCanvas;
    property DockSiteSize: Integer read GetDockSiteSize write SetDockSiteSize;
    property DockSiteSizeA: Integer read GetDockSiteSizeA write SetDockSiteSizeA;
    property DockSiteBegin: Integer read GetDockSiteBegin write SetDockSiteBegin;
    property DockSiteSizeWithOrient[Orient: TDockOrientation]: Integer
      read GetDockSiteSizeWithOrient write SetDockSiteSizeWithOrient;
    property GrabberSize: Integer read FGrabberSize write SetGrabberSize;
    property GrabbersPosition: TGrabbersPosition read GetGrabbersPosition;
    property MinSize: Integer read GetMinSize write SetMinSize;
    property DockRect: TRect read GetDockRect write SetDockRect;
    property OldRect: TRect read FOldRect write FOldRect;
    property ParentLimit: Integer read FParentLimit write FParentLimit;
    property ReplacementZone: TCnDockZone read FReplacementZone write FReplacementZone;
    property ResizeCount: Integer read FResizeCount write FResizeCount;
    property ScaleBy: Double read FScaleBy write FScaleBy;
    property ShiftBy: Integer read FShiftBy write FShiftBy;
    property ShiftScaleOrient: TDockOrientation read FShiftScaleOrient write FShiftScaleOrient;
    property SizePos: TPoint read FSizePos write FSizePos;
    property SizingDC: HDC read FSizingDC;
    property SizingWnd: HWND read FSizingWnd;
    property SizingZone: TCnDockZone read FSizingZone write FSizingZone;
    property SplitterWidth: Integer read GetSplitterWidth write SetSplitterWidth;
    property UpdateCount: Integer read FUpdateCount write FUpdateCount;
    property Version: Integer read FVersion write SetVersion;
  public
    SplitterCanvas: TControlCanvas;
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); virtual;
    destructor Destroy; override;
    property DockSite: TWinControl read FDockSite write FDockSite;
    property DockSiteOrient: TDockOrientation read GetDockSiteOrient;
    { 设置分割条的鼠标形状，用户可以重载这个函数来改变鼠标的形状 }
    procedure SetSplitterCursor(CursorIndex: TDockOrientation); virtual;
    { 重画DockSite的界面 }
    procedure PaintSite(DC: HDC); virtual;
    property TopXYLimit: Integer read FTopXYLimit write SetTopXYLimit;
    property TopZone: TCnDockZone read FTopZone write SetTopZone;
    { 更新全部 }
    procedure UpdateAll;
    { 更新当前Zone的子女 }
    procedure UpdateChild(Zone: TCnDockZone);

    property CnDockZoneClass: TCnDockZoneClass read FCnDockZoneClass
      write SetCnDockZoneClass;
  end;

  TCnDockTreeClass = class of TCnDockTree;

  TCnAdvDockTree = class(TCnDockTree)
  private
    FButtonHeight,                  //关闭按钮的高度
    FButtonWidth,                   //关闭按钮的宽度
    FLeftOffset,                    //关闭按钮的左边偏移量
    FRightOffset,                   //关闭按钮的右边偏移量
    FTopOffset,                     //关闭按钮的上边偏移量
    FBottomOffset: Integer;         //关闭按钮的下边偏移量
    FButtonSplitter: Integer;       //按钮之间的间隔
    FCloseBtnZone: TCnAdvDockZone;

    FDropDockSize: Integer;
    FDockHeightWidth: array[TDockOrientation] of Integer;
    FDockRectArr: array[TDockOrientation, Boolean] of Integer;

    procedure SetBottomOffset(const Value: Integer);
    procedure SetButtonHeight(const Value: Integer);
    procedure SetButtonSplitter(const Value: Integer);
    procedure SetButtonWidth(const Value: Integer);
    procedure SetLeftOffset(const Value: Integer);
    procedure SetRightOffset(const Value: Integer);
    procedure SetTopOffset(const Value: Integer);

    function GetDockHeightWidth(Orient: TDockOrientation): Integer;
    procedure SetDockHeightWidth(Orient: TDockOrientation;
      const Value: Integer);

    function GetDockRectFromArr(Orient: TDockOrientation;
      AtLast: Boolean): Integer;
    procedure SetDockRectToArr(Orient: TDockOrientation; AtLast: Boolean;
      const Value: Integer);
    procedure SetDropDockSize(const Value: Integer);

  protected
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure InsertSibling(NewZone, SiblingZone: TCnDockZone;
      InsertLast, Update: Boolean); override;
    procedure InsertNewParent(NewZone, SiblingZone: TCnDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure SetDockHeightWidthArr(NoOrValue, HorValue, VerValue: Integer);
    procedure SetDockRectArr(ARect: TRect);

    procedure ScaleZone(Zone: TCnDockZone); override;
    procedure ScaleChildZone(Zone: TCnDockZone); override;
    procedure ScaleSiblingZone(Zone: TCnDockZone); override;
    procedure ShiftZone(Zone: TCnDockZone); override;

    procedure RemoveZone(Zone: TCnDockZone; Hide: Boolean); override;
  public
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); override;
    property BottomOffset: Integer read FBottomOffset write SetBottomOffset;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property ButtonSplitter: Integer read FButtonSplitter write SetButtonSplitter;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property LeftOffset: Integer read FLeftOffset write SetLeftOffset;
    property RightOffset: Integer read FRightOffset write SetRightOffset;
    property TopOffset: Integer read FTopOffset write SetTopOffset;
    property CloseBtnZone: TCnAdvDockZone read FCloseBtnZone write FCloseBtnZone;
    property DockHeightWidth[Orient: TDockOrientation]: Integer read GetDockHeightWidth write SetDockHeightWidth;
    property DockRectArr[Orient: TDockOrientation; AtLast: Boolean]: Integer read GetDockRectFromArr write SetDockRectToArr;
    property DropDockSize: Integer read FDropDockSize write SetDropDockSize;
  end;
  
var
  //存储树信息的流的结束标志
  TreeStreamEndFlag: Integer = -1;

implementation

uses
  Math, CnDockFormControl, CnDockSupportProc, CnDockGlobal, CnVSNETDockStyle;

type
  TCnWinControlAccess = class(TWinControl);

{ TCnDockZone }

constructor TCnDockZone.Create(Tree: TCnDockTree);
begin
//  FVisibleSize := 0;
  ParentZone := nil;
  PrevSibling := nil;
  NextSibling := nil;
  ChildZones := nil;
  ChildControl := nil;
  FTree := Tree;
  FVisibled := True;
end;

function TCnDockZone.GetChildCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := ChildZones;
  while Zone <> nil do
  begin
    Zone := Zone.NextSibling;
    Inc(Result);
  end;
end;

function TCnDockZone.GetLimitBegin: Integer;
var
  CheckZone: TCnDockZone;
begin
  if FTree.FTopZone = Self then CheckZone := Self
  else CheckZone := FParentZone;
  if CheckZone.Orientation = doHorizontal then Result := Top
  else if CheckZone.Orientation = doVertical then Result := Left
  else Result := 0;//raise Exception.Create('');
end;

function TCnDockZone.GetLimitSize: Integer;
var
  CheckZone: TCnDockZone;
begin
  if FTree.FTopZone = Self then CheckZone := Self
  else CheckZone := FParentZone;
  if CheckZone.Orientation = doHorizontal then Result := Height
  else if CheckZone.Orientation = doVertical then Result := Width
  else Result := Tree.TopXYLimit; //raise Exception.Create('');
end;

function TCnDockZone.GetTopLeft(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TCnDockZone;
  R: TRect;
begin
  Zone := Self;
  while Zone <> FTree.FTopZone do
  begin
    // 找到可见的上兄弟。
    if (Zone.VisiblePrevSiblingCount > 0) and (Zone.ParentZone.Orientation = TDockOrientation(Orient)) then
    begin
      Result := Zone.BeforeClosestVisibleZone.ZoneLimit;
      Exit;
    end else Zone := Zone.ParentZone;
  end;
  R := FTree.FDockSite.ClientRect;
  TCnWinControlAccess(FTree.FDockSite).AdjustClientRect(R);
  case TDockOrientation(Orient) of
    doVertical: Result := R.Left;
    doHorizontal: Result := R.Top;
  else
    Result := 0;
  end;
end;

function TCnDockZone.GetHeightWidth(Orient: Integer{TDockOrientation}): Integer;
var
  Zone: TCnDockZone;
  R: TRect;
begin
  if (Self = FTree.FTopZone) or ((FParentZone = FTree.FTopZone) and
    (ChildControl <> nil) and (FTree.FTopZone.ChildCount = 1)) then
  begin
    R := FTree.FDockSite.ClientRect;
    TCnWinControlAccess(FTree.FDockSite).AdjustClientRect(R);
    if TDockOrientation(Orient) = doHorizontal then
      Result := R.Bottom - R.Top
    else
      Result := R.Right - R.Left;
  end
  else begin
    Zone := Self;
    while (Zone <> FTree.FTopZone) and (Zone.ParentZone <> nil) do
    begin
      // 存储
//      BeginLimit := Zone.LimitBegin;
//      while (Zone.NextSibling <> nil) and (not Zone.NextSibling.Visibled) do
//        Zone := Zone.NextSibling;
      if {(Zone.VisiblePrevSiblingCount > 0) and }(Zone.ParentZone.Orientation = TDockOrientation(Orient)) then
      begin
        Result := Zone.ZoneLimit - Zone.LimitBegin;
        Exit;
      end
      else
        Zone := Zone.ParentZone;
    end;
    if FTree.FTopZone.Orientation = TDockOrientation(Orient) then
      Result := FTree.TopXYLimit
    else
      Result := FTree.FTopZone.ZoneLimit;
  end;
end;

procedure TCnDockZone.ResetChildren(Exclude: TCnDockZone);
var
  SumLimit,
  NewLimit,
  FirstChildBegin,
  OldPrevLimit: Integer;
  ChildNode: TCnDockZone;  // 当前的子女节点(可见的)
  PrevNode: TCnDockZone;   // 当前的子女节点的前兄弟(可见的)
begin
  //
  case Orientation of
    doHorizontal: NewLimit := Height;
    doVertical: NewLimit := Width;
  else
    Exit;
  end;
  // 得到第一个可见的子女节点，并且确保它是存在的。
  ChildNode := FirstVisibleChildZone;
  if ChildNode = nil then Exit;
  // 得到平分的ZoneLimit的值
  SumLimit := NewLimit;
  NewLimit := NewLimit div VisibleChildCount;

  FirstChildBegin := ChildNode.LimitBegin;

  Tree.ShiftScaleOrient := Orientation;
  Tree.ParentLimit := 0;
  if ChildNode.ZoneLimit - FirstChildBegin > 0 then
    Tree.ScaleBy := NewLimit / (ChildNode.ZoneLimit - FirstChildBegin)
  else Tree.ScaleBy := 1;
  if (Tree.ScaleBy <> 1) and (ChildNode.VisibleChildCount > 0) then
    Tree.ForEachAt(ChildNode.ChildZones, Tree.ScaleChildZone, tskMiddle, tspChild);

  if ChildNode <> Exclude then
    OldPrevLimit := ChildNode.ZoneLimit
  else OldPrevLimit := FirstChildBegin;


  // 给第一个可见的子女节点的ZoneLimit赋值
  ChildNode.ZoneLimit := FirstChildBegin + NewLimit;
  ChildNode.Update;
  // 保存ChildNode，因为在后面的程序中要用到
  PrevNode := ChildNode;
  ChildNode := ChildNode.AfterClosestVisibleZone;

  // 一直不断的循环，直到最后一个可见的可见的子女节点为止。
  while ChildNode <> nil do
  begin
    if ChildNode.ZoneLimit - OldPrevLimit > 0 then
      Tree.ScaleBy := NewLimit / (ChildNode.ZoneLimit - OldPrevLimit)
    else Tree.ScaleBy := 1;

    Tree.ShiftBy := PrevNode.ZoneLimit - OldPrevLimit;
    if (Tree.ShiftBy <> 0) and (ChildNode.VisibleChildCount > 0){ and (PrevNode <> Exclude) }then
      Tree.ForEachAt(ChildNode.ChildZones, Tree.ShiftZone, tskForward);

    Tree.ParentLimit := PrevNode.ZoneLimit;

    if (Tree.ScaleBy <> 1) and (ChildNode.VisibleChildCount > 0) then
      Tree.ForEachAt(ChildNode.ChildZones, Tree.ScaleChildZone, tskForward);

    if ChildNode <> Exclude then
      OldPrevLimit := ChildNode.ZoneLimit;
//    else OldPrevLimit := PrevNode.ZoneLimit;

    ChildNode.ZoneLimit := PrevNode.ZoneLimit + NewLimit;

    if ChildNode.AfterClosestVisibleZone = nil then
    begin
      // 取消除零错误
      if NewLimit = 0 then
        NewLimit := 1;
      ChildNode.ZoneLimit := ChildNode.ZoneLimit + (SumLimit mod NewLimit);
    end;
    ChildNode.Update;
    PrevNode := ChildNode;
    ChildNode := ChildNode.AfterClosestVisibleZone;
  end;
end;

function TCnDockZone.GetControlName: string;
begin
  Result := '';
  if ChildControl <> nil then
  begin
    if ChildControl.Name = '' then
      raise Exception.CreateRes(@SDockedCtlNeedsName);
    Result := ChildControl.Name;
  end;
end;

function TCnDockZone.SetControlName(const Value: string): Boolean;
var
  Client: TControl;
begin
  Client := nil;
  with FTree do
  begin
    TCnWinControlAccess(FDockSite).ReloadDockedControl(Value, Client);
    Result := Client <> nil;
    if Result then
    begin
      FReplacementZone := Self;
      ChildControl := TWinControl(Client);
      DoCustomSetControlName;
      try
        if IsInside then
        begin
          Client.ManualDock(FDockSite, nil, alNone);
//          CnGlobalDockPresident.CalcDockSizes(Client);
        end;
//        ResetBounds(True);
      finally
        SetChildControlVisible(Client, FControlVisibled);
        FReplacementZone := nil;
      end;
    end;
  end;
end;

procedure TCnDockZone.Update;

  function ParentNotLast: Boolean;
  var
    Parent: TCnDockZone;
  begin
    Result := False;
    Parent := FParentZone;
    while Parent <> nil do
    begin
      if (Parent.VisibleNextSiblingCount > 0) and (Parent.Orientation = ParentZone.Orientation) then
      begin
        Result := True;
        Exit;
      end;
      Parent := Parent.FParentZone;
    end;
  end;

var
  NewWidth, NewHeight: Integer;
  R: TRect;
begin
  if Visibled and (ChildControl <> nil) and (FTree.FUpdateCount = 0) then
  begin
    ChildControl.DockOrientation := FParentZone.Orientation;
    NewWidth := Width;
    NewHeight := Height;
    if ParentNotLast then
    begin
      if FParentZone.Orientation = doHorizontal then
        Dec(NewWidth, FTree.SplitterWidth)
      else
        Dec(NewHeight, FTree.SplitterWidth);
    end;

    if ((NextSibling <> nil) and (VisibleNextSiblingTotal > 0)) or ((FParentZone <> FTree.FTopZone) and
      ((FParentZone.Orientation = FTree.FTopZone.Orientation) and
      (FZoneLimit < FTree.TopXYLimit)) or
      ((FParentZone.Orientation <> FTree.FTopZone.Orientation) and
      (FZoneLimit < FTree.FTopZone.ZoneLimit))) then
    begin
      if FParentZone.Orientation = doHorizontal then
        Dec(NewHeight, FTree.SplitterWidth)
      else
        Dec(NewWidth, FTree.SplitterWidth);
    end;
    R := Bounds(Left, Top, NewWidth, NewHeight);
    FTree.AdjustDockRect(ChildControl, R);
    ChildControl.BoundsRect := R;
  end;
end;

function TCnDockZone.GetFrameRect: TRect;
var
  ALeft, ATop, ARight, ABottom, BorderWidth: Integer;
begin
  ALeft := Left;
  ATop := Top;
  if NextSibling <> nil then
    BorderWidth := Tree.BorderWidth
  else
  BorderWidth := 0;
  ARight := ALeft + Width - BorderWidth;
  ABottom := ATop + Height - BorderWidth;
  Result := Rect(ALeft, ATop, ARight, ABottom);
end;

function TCnDockZone.GetFirstSibling: TCnDockZone;
begin
  Result := Self;
  while Result.PrevSibling <> nil do
    Result := Result.PrevSibling;
end;

function TCnDockZone.GetLastSibling: TCnDockZone;
begin
  Result := Self;
  while (Result <> nil) and (Result.NextSibling <> nil) do
    Result := Result.NextSibling;
end;

function TCnDockZone.GetFirstChild: TCnDockZone;
begin
  Result := ChildZones;
end;

function TCnDockZone.GetLastChild: TCnDockZone;
begin
  Result := ChildZones;
  if Result <> nil then
    Result := Result.LastSibling;
end;

function TCnDockZone.GetTopLeftArr(Orient: TDockOrientation): Integer;
begin
  Result := 0;
  case Orient of
    doHorizontal: Result := Top;
    doVertical: Result := Left;
  else
//    raise Exception.Create('');
  end;
end;

function TCnDockZone.GetHeightWidthArr(Orient: TDockOrientation): Integer;
begin
  Result := 0;
  case Orient of
    doHorizontal: Result := Height;
    doVertical: Result := Width;
  else
//    raise Exception.Create('');
  end;
end;

procedure TCnDockZone.AdjustZoneLimit(Value: Integer);
begin
  FZoneLimit := Value;
  if PrevSibling <> nil then
    PrevSibling.ZoneLimit := PrevSibling.ZoneLimit + Value;
//  else if NextSibling <> nil then
//    NextSibling.ZoneLimit := NextSibling.ZoneLimit
end;

procedure TCnDockZone.SetZoneSize(Size: Integer; Show: Boolean);
begin
  InsertOrRemove(Size, Show, False);
end;

procedure TCnDockZone.InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean);
begin
end;

procedure TCnDockZone.Insert(DockSize: Integer; Hide: Boolean);
begin
  InsertOrRemove(DockSize, True, Hide);
  // 如果ParentZone的VisibleChildCount等于0，
  // 说明父节点也是隐藏的，就要调用父节点的Insert函数，
  // 注意，这是一个递归函数，一直调用到父节点可见为止。
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  Visibled := True;
  if ParentZone <> nil then
    ParentZone.ResetChildren(Self);

  // 重新更新ParentZone上的子女节点的位置
  Tree.SetNewBounds(ParentZone);
  Tree.UpdateChild(ParentZone);
end;

procedure TCnDockZone.Remove(DockSize: Integer; Hide: Boolean);
var Zone: TCnDockZone;
begin
  InsertOrRemove(DockSize, False, Hide);
  // 首先设置Visibled。
  Visibled := not Hide;

  // 如果有ParentZone，并且这个ParentZone上没有可见的子女，就把ParentZone也Remove,
  // 这是一个递归函数，目的是使调整从最上层的Zone开始。
  if (ParentZone <> Tree.TopZone) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  // 如果已经没有可见的后面的兄弟，就把前兄弟的ZoneLimit设成当前Zone的ZoneLimit。
  if AfterClosestVisibleZone = nil then
  begin
    // 找到离当前Zone最近的可见的前兄弟。
    Zone := BeforeClosestVisibleZone;
    if Zone <> nil then
    begin
      // 如果有可见的前兄弟，就调整这个兄弟的所有子女的位置。
      Zone.ZoneLimit := ZoneLimit;
      Tree.SetNewBounds(Zone);
    end;
  end;
  // 因为当前的Zone将被撤消，所以ZoneLimit被设成和LimitBegin一样的。
  ZoneLimit := LimitBegin;
end;

function TCnDockZone.GetVisibleChildCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := ChildZones;
  while Zone <> nil do
  begin
    if Zone.Visibled then
      Inc(Result);
    Zone := Zone.NextSibling;
  end;
end;

function TCnDockZone.GetChildTotal: Integer;

  procedure DoFindChildCount(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      DoFindChildCount(Zone.NextSibling);
      DoFindChildCount(Zone.ChildZones);
      {if Zone.Orientation = doNoOrient then}
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindChildCount(ChildZones);
end;

function TCnDockZone.GetVisibleChildTotal: Integer;

  procedure DoFindVisibleChildCount(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      DoFindVisibleChildCount(Zone.NextSibling);
      DoFindVisibleChildCount(Zone.ChildZones);
      if {(Zone.Orientation = doNoOrient) and }(Zone.Visibled) then
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindVisibleChildCount(ChildZones);
end;

function TCnDockZone.GetAfterClosestVisibleZone: TCnDockZone;
begin
  Result := NextSibling;
  while Result <> nil do
  begin
    if Result.Visibled then
      Exit;
    Result := Result.NextSibling;
  end;
end;

function TCnDockZone.GetBeforeClosestVisibleZone: TCnDockZone;
begin
  Result := PrevSibling;
  while Result <> nil do
  begin
    if Result.Visibled then
      Exit;
    Result := Result.PrevSibling;
  end;
end;

function TCnDockZone.GetAfterApoapsisVisibleZone: TCnDockZone;
begin
  Result := LastSibling;
  if Result <> nil then
    Result := Result.BeforeClosestVisibleZone;
  if Self = Result then
    Result := nil;
end;

function TCnDockZone.GetBeforeApoapsisVisibleZone: TCnDockZone;
begin
  Result := ParentZone.ChildZones;
  if Result <> Self then
    Result := Result.AfterClosestVisibleZone;
  if Self = Result then
    Result := nil;
end;

function TCnDockZone.GetNextSiblingCount: Integer;
var AZone: TCnDockZone;
begin
  Result := 0;
  AZone := NextSibling;
  while AZone <> nil do
  begin
    Inc(Result);
    AZone := AZone.NextSibling;
  end;
end;

function TCnDockZone.GetPrevSiblingCount: Integer;
var AZone: TCnDockZone;
begin
  Result := 0;
  AZone := PrevSibling;
  while AZone <> nil do
  begin
    Inc(Result);
    AZone := AZone.PrevSibling;
  end;
end;

procedure TCnDockZone.SetVisibled(const Value: Boolean);
begin
  FVisibled := Value;
  if (not FVisibled) and (Self <> Tree.TopZone) then
  begin
    if ParentZone.Orientation = doNoOrient then
      VisibleSize := Tree.TopXYLimit
    else VisibleSize := LimitSize;
  end else
  begin

  end;
end;

function TCnDockZone.GetVisibleNextSiblingCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := NextSibling;
  while Zone <> nil do
  begin
    if Zone.Visibled then
      Inc(Result);
    Zone := Zone.NextSibling;
  end;
end;

function TCnDockZone.GetVisibleNextSiblingTotal: Integer;

  procedure DoFindVisibleNextSiblingCount(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      DoFindVisibleNextSiblingCount(Zone.NextSibling);
      DoFindVisibleNextSiblingCount(Zone.ChildZones);
      if {(Zone.Orientation = doNoOrient) and }(Zone.Visibled) then
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindVisibleNextSiblingCount(NextSibling);
end;

function TCnDockZone.GetVisiblePrevSiblingCount: Integer;
var
  Zone: TCnDockZone;
begin
  Result := 0;
  Zone := PrevSibling;
  while Zone <> nil do
  begin
    if Zone.Visibled then
      Inc(Result);
    Zone := Zone.PrevSibling;
  end;
end;

function TCnDockZone.GetVisiblePrevSiblingTotal: Integer;

  procedure DoFindVisibleNextSiblingCount(Zone: TCnDockZone);
  begin
    if (Zone <> nil) and (Zone <> Self) then
    begin
      DoFindVisibleNextSiblingCount(Zone.NextSibling);
      DoFindVisibleNextSiblingCount(Zone.ChildZones);
      if {(Zone.Orientation = doNoOrient) and }(Zone.Visibled) then
        Inc(Result);
    end;
  end;

begin
  Result := 0;
  DoFindVisibleNextSiblingCount(ParentZone);
end;

procedure TCnDockZone.SetZoneLimit(const Value: Integer);
begin
  FZoneLimit := Value;
end;

function TCnDockZone.GetFirstVisibleChildZone: TCnDockZone;
begin
  Result := ChildZones;
  while (Result <> nil) and (not Result.Visibled) do
    Result := Result.NextSibling;
end;

function TCnDockZone.GetSplitterLimit(IsMin: Boolean): Integer;
begin
  if IsMin then
    Result := ZoneLimit
  else Result := LimitBegin;
  
  if ChildZones <> nil then
    ChildZones.DoGetSplitterLimit(ParentZone.Orientation, IsMin, Result);
end;

function TCnDockZone.DoGetSplitterLimit(Orientation: TDockOrientation;
  IsMin: Boolean; var LimitResult: Integer): Integer;
begin
  Result := 0;
  if (ParentZone <> nil) and (ParentZone.Orientation = Orientation) and Visibled then
  begin
    if IsMin then
      LimitResult := Min(LimitResult, ZoneLimit)
    else
    begin
      if AfterClosestVisibleZone <> nil then
        LimitResult := Max(LimitResult, ZoneLimit);
    end;
  end;

  if NextSibling <> nil then
    NextSibling.DoGetSplitterLimit(Orientation, IsMin, LimitResult);

  if ChildZones <> nil then
    ChildZones.DoGetSplitterLimit(Orientation, IsMin, LimitResult);
end;

function TCnDockZone.GetLastVisibleChildZone: TCnDockZone;
var Zone: TCnDockZone;
begin
  Result := nil;
  Zone := ChildZones;
  while (Zone <> nil) and Zone.Visibled do
  begin
    Result := Zone;
    Zone := Zone.NextSibling;
  end;
end;

procedure TCnDockZone.DoCustomSetControlName;
begin
  { 没事做 }
end;

procedure TCnDockZone.LButtonDbClkMothed;
begin
  if ChildControl <> nil then
    ChildControl.ManualDock(nil, nil, alTop);
end;

procedure TCnDockZone.SetIsInside(const Value: Boolean);
begin
  FIsInside := Value;
end;

procedure TCnDockZone.SetChildControlVisible(Client: TControl; AViisible: Boolean);
begin
  if Client <> nil then
  begin
    Client.Visible := {(not IsInside) or }FControlVisibled;
  end;
end;

{ TCnDockTree }

constructor TCnDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
var
  I: Integer;
begin
  FCnDockZoneClass := CnDockZoneClass;
  FBorderWidth := 0;
  FSplitterWidth := 4;
  FDockSite := TWinControl(DockSite);
  FDockSite.ShowHint := True;
  FVersion := gs_BaseDockTreeVersion;
  GrabberSize := 12;
  FMinSize := 12;
  FTopZone := FCnDockZoneClass.Create(Self);
  FBrush := TBrush.Create;
  FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  // 插入已经存在的控件到树中
  BeginUpdate;
  try
    for I := 0 to DockSite.ControlCount - 1 do
      InsertControl(DockSite.Controls[I], alLeft, nil);
    FTopZone.ResetChildren(nil);
  finally
    EndUpdate;
  end;
  if not (csDesigning in DockSite.ComponentState) then
  begin
    FOldWndProc := FDockSite.WindowProc;
    FDockSite.WindowProc := WindowProc;
  end;
end;

destructor TCnDockTree.Destroy;
begin
  if @FOldWndProc <> nil then
    FDockSite.WindowProc := FOldWndProc;
  PruneZone(FTopZone);
  FBrush.Free;
  inherited Destroy;
end;

procedure TCnDockTree.AdjustDockRect(Control: TControl; var ARect: TRect);
begin
  { 为控件分配空间 }
  { 首先减去边框的宽度 }
  InflateRect(ARect, -BorderWidth, -BorderWidth);
  { 然后再减去把手的宽度 }
  case GrabbersPosition of
    gpTop:
      Inc(ARect.Top, GrabberSize);
    gpBottom:
      Dec(ARect.Bottom, GrabberSize);
    gpLeft:
      Inc(ARect.Left, GrabberSize);
    gpRight:
      Dec(ARect.Right, GrabberSize);
  end;
end;

procedure TCnDockTree.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCnDockTree.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    UpdateAll;
  end;
end;

function TCnDockTree.FindControlZone(Control: TControl; IncludeHide: Boolean): TCnDockZone;
var
  CtlZone: TCnDockZone;

  procedure DoFindControlZone(StartZone: TCnDockZone);
  begin
    if (StartZone.ChildControl = Control) and (StartZone.Visibled or IncludeHide) then
      CtlZone := StartZone
    else begin
      // 遍历右兄弟
      if (CtlZone = nil) and (StartZone.NextSibling <> nil) then
        DoFindControlZone(StartZone.NextSibling);
      // 遍历左子女
      if (CtlZone = nil) and (StartZone.ChildZones <> nil) then
        DoFindControlZone(StartZone.ChildZones);
    end;
  end;

begin
  CtlZone := nil;
  if (Control <> nil) and (FTopZone <> nil) then DoFindControlZone(FTopZone);
  Result := CtlZone;
end;

procedure TCnDockTree.ForEachAt(Zone: TCnDockZone; Proc: TCnForEachZoneProc;
  ScanKind: TTreeScanKind; ScanPriority: TTreeScanPriority);

  { 前序遍历 }
  procedure DoForwardForEach(Zone: TCnDockZone);
  begin
    Proc(Zone);
    if ScanPriority = tspSibling then
    begin
      // 遍历右兄弟
      if Zone.NextSibling <> nil then DoForwardForEach(Zone.NextSibling);
      // 遍历左子女
      if Zone.ChildZones <> nil then DoForwardForEach(Zone.ChildZones);
    end else
    begin
      // 遍历左子女
      if Zone.ChildZones <> nil then DoForwardForEach(Zone.ChildZones);
      // 遍历右兄弟
      if Zone.NextSibling <> nil then DoForwardForEach(Zone.NextSibling);
    end;
  end;

  { 中序遍历 }
  procedure DoMiddleForEach(Zone: TCnDockZone);
  begin
    if ScanPriority = tspSibling then
    begin
      // 遍历右兄弟
      if Zone.NextSibling <> nil then DoMiddleForEach(Zone.NextSibling);
    end else
    begin
      // 遍历左子女
      if Zone.ChildZones <> nil then DoMiddleForEach(Zone.ChildZones);
    end;

    Proc(Zone);

    if ScanPriority = tspSibling then
    begin
      // 遍历左子女
      if Zone.ChildZones <> nil then DoMiddleForEach(Zone.ChildZones);
    end else
      // 遍历右兄弟
      if Zone.NextSibling <> nil then DoMiddleForEach(Zone.NextSibling);
  end;

  { 后序遍历 }
  procedure DoBackwardForEach(Zone: TCnDockZone);
  begin
    if ScanPriority = tspSibling then
    begin
      // 遍历右兄弟
      if Zone.NextSibling <> nil then DoBackwardForEach(Zone.NextSibling);
      // 遍历左子女
      if Zone.ChildZones <> nil then DoBackwardForEach(Zone.ChildZones);
    end else
    begin
      // 遍历左子女
      if Zone.ChildZones <> nil then DoForwardForEach(Zone.ChildZones);
      // 遍历右兄弟
      if Zone.NextSibling <> nil then DoForwardForEach(Zone.NextSibling);
    end;
    Proc(Zone);
  end;

begin
  { 如果传过来的参数Zone是nil,就从根目录开始遍历 }
  if Zone = nil then
  begin
    if FTopZone = nil then
      FTopZone := FCnDockZoneClass.Create(Self);
    Zone := FTopZone;
  end;
  { 根据ScanKind参数进行各自的遍历 }
  case ScanKind of
    tskForward: DoForwardForEach(Zone);
    tskMiddle:  DoMiddleForEach(Zone);
    tskBackward:DoBackwardForEach(Zone);
  end;
end;

procedure TCnDockTree.GetControlBounds(Control: TControl; out CtlBounds: TRect);
var
  Z: TCnDockZone;
begin
  Z := FindControlZone(Control);
  if Z = nil then
    FillChar(CtlBounds, SizeOf(CtlBounds), 0)
  else
    with Z do
    begin
      CtlBounds := Bounds(Left, Top, Width, Height);
//      AdjustRect(
    end;
end;

function TCnDockTree.HitTest(const MousePos: TPoint; out HTFlag: Integer): TControl;
var
  Zone: TCnDockZone;
begin
  Zone := InternalHitTest(MousePos, HTFlag);
  if Zone <> nil then Result := Zone.ChildControl
  else Result := nil;
end;

procedure TCnDockTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
const
{ Delphi6.0 }
{$IFDEF COMPILER6_UP}
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient, doNoOrient); { alCustom }
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False, False);  { alCustom }
{$ELSE}
{ Delphi5.0 OR LAST }
  OrientArray: array[TAlign] of TDockOrientation = (doNoOrient, doHorizontal,
    doHorizontal, doVertical, doVertical, doNoOrient);
  MakeLast: array[TAlign] of Boolean = (False, False, True, False, True, False);
{$ENDIF}

var
  Sibling,                  // 下一个兄弟
  Me: TCnDockZone;   // 当前正要被创建的节点
  InsertOrientation,        // 插入的方向
  CurrentOrientation: TDockOrientation;// 当前的方向
  NewWidth, NewHeight: Integer;
  R: TRect;                 // 控件的矩形大小
begin
//  if not Control.Visible then Exit;
  if FReplacementZone <> nil then
  begin
    { 如果FReplacementZone <> nil，说明正好在执行装载停靠信息的操作 }
    FReplacementZone.ChildControl := TWinControl(Control);
    FReplacementZone.Update;
    Exit;
  end
  else if FTopZone <> nil then
  begin
    if FTopZone.ChildZones = nil then
    begin
      // 如果树是空的，就要添加第一个子女
      R := FDockSite.ClientRect;
      TCnWinControlAccess(FDockSite).AdjustClientRect(R);
      NewWidth := R.Right - R.Left;
      NewHeight := R.Bottom - R.Top;
      if TCnWinControlAccess(FDockSite).AutoSize then
      begin
        if NewWidth = 0 then NewWidth := Control.UndockWidth;
        if NewHeight = 0 then NewHeight := Control.UndockHeight;
      end;
      R := Bounds(R.Left, R.Top, NewWidth, NewHeight);
      AdjustDockRect(Control, R);
      Control.BoundsRect := R;
      Me := FCnDockZoneClass.Create(Self);
      FTopZone.ChildZones := Me;
      Me.FParentZone := FTopZone;
      Me.ChildControl := TWinControl(Control);
    end
    else begin
      // 默认是停靠到右边
      if InsertAt in [alClient, alNone] then InsertAt := alRight;
      { 查找Control是否已经被停靠进了DockSite中，
        如果是的话就删除这个节点 }
      Me := FindControlZone(Control, True);
      if Me <> nil then RemoveZone(Me, False);
      { 查找到DropCtl所在的节点 }
      Sibling := FindControlZone(DropCtl);
      { 设置插入的方向 }
      InsertOrientation := OrientArray[InsertAt];
      if FTopZone.ChildCount = 1 then
      begin
        // 如果树只有一个子女，并且第二个正在被添加进去，
        // 所以方向和位置必须被重新设置
        FTopZone.Orientation := InsertOrientation;
        case InsertOrientation of
          doHorizontal:
            begin
              FTopZone.ZoneLimit := FTopZone.ChildZones.Width;
              TopXYLimit := FTopZone.ChildZones.Height;
            end;
          doVertical:
            begin
              FTopZone.ZoneLimit := FTopZone.ChildZones.Height;
              TopXYLimit := FTopZone.ChildZones.Width;
            end;
        end;
      end;
      { 创建一个节点，并且把Control赋值给这个节点的ChildControl }
      Me := FCnDockZoneClass.Create(Self);
      Me.ChildControl := TWinControl(Control);
      { 设置CurrentOrientation的方向 }
      if Sibling <> nil then
        { 是DropCtl所在的节点的父节点的方向 }
        CurrentOrientation := Sibling.FParentZone.Orientation
        { 是根节点的方向 }
      else CurrentOrientation := FTopZone.Orientation;
      if InsertOrientation = doNoOrient then
        InsertOrientation := CurrentOrientation;

      // 控件正在被停靠进一个和她相同方向的节点的时候，
      // 需要将自己添加到兄弟的前面或后面
      if InsertOrientation = CurrentOrientation then
        InsertSibling(Me, Sibling, MakeLast[InsertAt], True)
      else
      // 控件正在被停靠进一个和她不同方向的节点的时候，
      // 需要创建一个父节点，并且将自己和兄弟作为这个父节点的子女。
      // 这个父节点的方向和插入方向(InsertOrientation)相同
        InsertNewParent(Me, Sibling, InsertOrientation, MakeLast[InsertAt], True);
    end;
    { 重新画客户区的停靠框架 }
    FDockSite.Invalidate;
  end;

(*  if FTopStoreZone <> nil then
  begin
    if FTopStoreZone.ChildZones = nil then
    begin
      Store := TCnDockZone.Create(Self, True);
      FTopStoreZone.ChildZones := Store;
      Store.FParentZone := FTopStoreZone;
      Store.ChildControl := TWinControl(Control);
    end else
    begin
      // 默认是停靠到右边
      if InsertAt in [alClient, alNone] then InsertAt := alRight;
      { 查找Control是否已经被停靠进了DockSite中，
        如果是的话就删除这个节点 }
      Store := FindControlZone(Control, True);
      if Store <> nil then RemoveZone(Store, True);
      { 查找到DropCtl所在的节点 }
      Sibling := FindControlZone(DropCtl, True);
      { 设置插入的方向 }
      InsertOrientation := OrientArray[InsertAt];
      if FTopStoreZone.ChildCount = 1 then
      begin
        // 如果树只有一个子女，并且第二个正在被添加进去，
        // 所以方向和位置必须被重新设置
        FTopStoreZone.Orientation := InsertOrientation;
      end;

      { 创建一个节点，并且把Control赋值给这个节点的ChildControl }
      Store := TCnDockZone.Create(Self, True);
      Store.ChildControl := TWinControl(Control);
      { 设置CurrentOrientation的方向 }
      if Sibling <> nil then
        { 是DropCtl所在的节点的父节点的方向 }
        CurrentOrientation := Sibling.FParentZone.Orientation
        { 是根节点的方向 }
      else CurrentOrientation := FTopStoreZone.Orientation;
      if InsertOrientation = doNoOrient then
        InsertOrientation := CurrentOrientation;

      // 控件正在被停靠进一个和她相同方向的节点的时候，
      // 需要将自己添加到兄弟的前面或后面
      if InsertOrientation = CurrentOrientation then
        InsertSibling(Store, Sibling, MakeLast[InsertAt], False, True)
      else
      // 控件正在被停靠进一个和她不同方向的节点的时候，
      // 需要创建一个父节点，并且将自己和兄弟作为这个父节点的子女。
      // 这个父节点的方向和插入方向(InsertOrientation)相同
        InsertNewParent(Store, Sibling, InsertOrientation, MakeLast[InsertAt], False, True);
    end;
  end;*)
end;

procedure TCnDockTree.InsertNewParent(NewZone, SiblingZone: TCnDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
var
  NewParent: TCnDockZone;
begin
  NewParent := FCnDockZoneClass.Create(Self);

  NewParent.Orientation := ParentOrientation;
  if SiblingZone = nil then
  begin
    // 如果SiblingZone是空的话，我们需要把这个节点作为根节点的子女插入树中
    NewParent.ZoneLimit := TopXYLimit;
    TopXYLimit := FTopZone.ZoneLimit;
    ShiftScaleOrient := ParentOrientation;
    ScaleBy := 0.5;
    if InsertLast then
    begin
      FTopZone.Visibled := FTopZone.VisibleChildCount > 0;
      NewParent.ChildZones := FTopZone;
      FTopZone.ParentZone := NewParent;
      FTopZone.NextSibling := NewZone;
      NewZone.PrevSibling := FTopZone;
      NewZone.ParentZone := NewParent;
      FTopZone := NewParent;
      ForEachAt(NewParent.ChildZones, ScaleZone, tskForward);
    end
    else begin
      NewParent.ChildZones := NewZone;
      FTopZone.ParentZone := NewParent;
      FTopZone.PrevSibling := NewZone;
      NewZone.NextSibling := FTopZone;
      NewZone.ParentZone := NewParent;
      FTopZone := NewParent;

      if ParentOrientation <> FTopZone.Orientation then
        NewZone.ZoneLimit := FTopZone.ZoneLimit div 2
      else NewZone.ZoneLimit := TopXYLimit div 2;

      ForEachAt(NewZone.NextSibling, ScaleZone, tskForward);
      if ParentOrientation <> FTopZone.Orientation then
        ShiftBy := FTopZone.ZoneLimit div 2
      else ShiftBy := TopXYLimit div 2;
      ForEachAt(NewZone.NextSibling, ShiftZone, tskForward);

    end;
    ForEachAt(nil, UpdateZone, tskForward);
  end
  else begin
    // 如果SiblingZone不是空的，我们就要创建一个节点，
    // 这个节点是Me和SiblingZone共同的父亲
    NewParent.ZoneLimit := SiblingZone.ZoneLimit;
    NewParent.ParentZone := SiblingZone.ParentZone;
    NewParent.PrevSibling := SiblingZone.PrevSibling;
    if NewParent.PrevSibling <> nil then
      NewParent.PrevSibling.NextSibling := NewParent;
    NewParent.NextSibling := SiblingZone.NextSibling;
    if NewParent.NextSibling <> nil then
      NewParent.NextSibling.PrevSibling := NewParent;
    if NewParent.ParentZone.ChildZones = SiblingZone then
      NewParent.ParentZone.ChildZones := NewParent;
    NewZone.ParentZone := NewParent;
    SiblingZone.ParentZone := NewParent;
    if InsertLast then
    begin
      // 插入到SiblingZone的后面
      NewParent.ChildZones := SiblingZone;
      SiblingZone.ZoneLimit := NewParent.ParentZone.ZoneLimit;
      SiblingZone.PrevSibling := nil;
      SiblingZone.NextSibling := NewZone;
      NewZone.PrevSibling := SiblingZone;
    end
    else begin
      // 插入到SiblingZone的前面
      NewParent.ChildZones := NewZone;
      SiblingZone.PrevSibling := NewZone;
      SiblingZone.NextSibling := nil;
      NewZone.NextSibling := SiblingZone;
    end;
  end;
  if Update then
  begin
    // 重新设置新子女的范围
    NewParent.ResetChildren(nil);
    ForEachAt(nil, UpdateZone, tskForward);
  end;
end;

procedure TCnDockTree.InsertSibling(NewZone, SiblingZone: TCnDockZone;
  InsertLast, Update: Boolean);
begin
  if (NewZone <> nil) and (SiblingZone <> nil) and
    (NewZone.ChildControl = SiblingZone.ChildControl) then
    SiblingZone := nil;
  if SiblingZone = nil then
  begin
    SiblingZone := FTopZone.ChildZones;
    if InsertLast then
      SiblingZone := SiblingZone.LastSibling;
  end;
  if InsertLast then
  begin
    // 把NewZone插入到SiblingZone后
    NewZone.ParentZone := SiblingZone.ParentZone;
    NewZone.PrevSibling := SiblingZone;
    NewZone.NextSibling := SiblingZone.NextSibling;
    if NewZone.NextSibling <> nil then
      NewZone.NextSibling.PrevSibling := NewZone;
    SiblingZone.NextSibling := NewZone;
  end
  else begin
    // 把NewZone插入到SiblingZone前
    NewZone.NextSibling := SiblingZone;
    NewZone.PrevSibling := SiblingZone.PrevSibling;
    if NewZone.PrevSibling <> nil then
      NewZone.PrevSibling.NextSibling := NewZone;
    SiblingZone.PrevSibling := NewZone;
    NewZone.ParentZone := SiblingZone.ParentZone;
    if NewZone.ParentZone.ChildZones = SiblingZone then
      NewZone.ParentZone.ChildZones := NewZone;
  end;
  if Update then
  begin
    // 重新设置所有的兄弟
    SiblingZone.ParentZone.ResetChildren(nil);
    UpDateChild(SiblingZone.ParentZone);
  end;
end;

function TCnDockTree.DoFindZone(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
const HTFlagArr : array[Boolean] of Integer = (HTCLIENT, HTSPLITTER);
begin
  Result := nil;
  // 鼠标是否在分割条的底部...
  if (Zone.ParentZone.Orientation = doHorizontal) and
    (Zone.NextSibling <> nil) and
    ((MousePos.Y <= Zone.FZoneLimit) and
    (MousePos.Y >= Zone.FZoneLimit - SplitterWidth)) and
    ((MousePos.X <= Zone.ParentZone.FZoneLimit) and
    (MousePos.X >= Zone.ParentZone.LimitBegin)) then
  begin
    HTFlag := HTFlagArr[Zone.VisibleNextSiblingTotal > 0];
    Result := Zone;
  end

  // 鼠标是否在分割条的左部...
  else if (Zone.FParentZone.Orientation = doVertical) and
    (Zone.NextSibling <> nil) and
    ((MousePos.X <= Zone.FZoneLimit) and
    (MousePos.X >= Zone.FZoneLimit - SplitterWidth)) and
    ((MousePos.Y <= Zone.ParentZone.FZoneLimit) and
    (MousePos.Y >= Zone.ParentZone.LimitBegin)) then
  begin
    HTFlag := HTFlagArr[Zone.VisibleNextSiblingTotal > 0];
    Result := Zone;
  end

  // 鼠标是否在把手内...
  else if Zone.ChildControl <> nil then
  begin
    { 鼠标是否在边框内... }
    case GrabbersPosition of
      gpTop:    Result := GetTopGrabbersHTFlag(MousePos, HTFlag, Zone);
      gpLeft:   Result := GetLeftGrabbersHTFlag(MousePos, HTFlag, Zone);
      gpBottom: Result := GetBottomGrabbersHTFlag(MousePos, HTFlag, Zone);
      gpRight:  Result := GetRightGrabbersHTFlag(MousePos, HTFlag, Zone);
    end;
    { 如果不在把手上，就查找是否在边框内... }
    if Result = nil then
      Result := GetBorderHTFlag(MousePos, HTFlag, Zone);
  end else Result := nil;

  if (Result <> nil) and (not Result.Visibled) then Result := nil;
  
  // 遍历别的节点...
  if (Result = nil) and (Zone.NextSibling <> nil) then
    Result := DoFindZone(MousePos, HTFlag, Zone.NextSibling);
  if (Result = nil) and (Zone.ChildZones <> nil) then
    Result := DoFindZone(MousePos, HTFlag, Zone.ChildZones);
end;

function TCnDockTree.InternalHitTest(const MousePos: TPoint; out HTFlag: Integer): TCnDockZone;
var
  ResultZone: TCnDockZone;

  function FindControlAtPos(const Pos: TPoint): TControl;
  var
    I: Integer;
    P: TPoint;
  begin
    for I := FDockSite.ControlCount - 1 downto 0 do
    begin
      Result := FDockSite.Controls[I];
      with Result do
      begin
        { 控件必须被显示出来... }
        if not Result.Visible or ((Result is TWinControl) and
           not TWinControl(Result).Showing) then continue;
        P := Point(Pos.X - Left, Pos.Y - Top);
        if PtInRect(ClientRect, P) then Exit;
      end;
    end;
    Result := nil;
  end;

var
  CtlAtPos: TControl;
begin
  ResultZone := nil;
  HTFlag := HTNOWHERE;
  CtlAtPos := FindControlAtPos(MousePos);
  if (CtlAtPos <> nil) and (CtlAtPos.HostDockSite = FDockSite) then
  begin
    ResultZone := FindControlZone(CtlAtPos);
    if ResultZone <> nil then HTFlag := HTCLIENT;
  end
  else if (FTopZone <> nil) and (FTopZone.ChildZones <> nil) and (FTopZone.ChildCount >= 1) and
    (CtlAtPos = nil) then
    ResultZone := DoFindZone(MousePos, HTFlag, FTopZone.ChildZones);
  Result := ResultZone;
end;

procedure TCnDockTree.LoadFromStream(Stream: TStream);
var
//  CompName: string;
//  Client: TControl;
//  I, InVisCount: Integer;
  I: Integer;
begin
  PruneZone(FTopZone);
  // 读版本, 如果版本不同,就退出
  Stream.Read(I, SizeOf(I));
  if I <> Version then
    Exit;

  BeginUpdate;
  try
    // 读根节点的数据
    Stream.Read(FTopXYLimit, SizeOf(FTopXYLimit));
    // 读整棵树的节点
    DoLoadZone(Stream);
  finally
    EndUpdate;
  end;
end;

{procedure TCnDockTree.PaintDockGrabber(Canvas: TCanvas; Control: TControl;
  const ARect: TRect);

  procedure DrawCloseButton(Left, Top: Integer);
  begin
    DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+FGrabberSize-2,
      Top+FGrabberSize-2), DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;

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

begin
  with ARect do
  begin
    case GrabbersPosition of
      gpLeft:
      begin
        DrawCloseButton(Left+BorderWidth+BorderWidth+1, Top+BorderWidth+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+3, Top+FGrabberSize+BorderWidth+1, Left+BorderWidth+5, Bottom+BorderWidth-2);
        DrawGrabberLine(Left+BorderWidth+6, Top+FGrabberSize+BorderWidth+1, Left+BorderWidth+8, Bottom+BorderWidth-2);
      end;
      gpTop:
      begin
        DrawCloseButton(Right-FGrabberSize+BorderWidth+1, Top+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+3, Right-FGrabberSize+BorderWidth-2, Top+BorderWidth+5);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+6, Right-FGrabberSize+BorderWidth-2, Top+BorderWidth+8);
      end;
//      gpBottom:
//      gpRight:
    end;
  end;
end;}

procedure TCnDockTree.PaintSite(DC: HDC);
(*var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
  DockClient: TCnDockClient;
begin
  { 创建一个TControlCanvas对象 }
  Canvas := TControlCanvas.Create;
  try
    { 然后把DockSite赋给Canvas的Control属性，这样Canvas就可以在DockSite上画图案了 }
    Canvas.Control := FDockSite;
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        { 开始循环重画DockSite上的Clients }
        for I := 0 to FDockSite.ControlCount - 1 do
        begin
          Control := FDockSite.Controls[I];
          if Control.Visible and (Control.HostDockSite = FDockSite) then
          begin
            { 查找到Control上面的TCnDockClient类 }
            DockClient := FindDockClient(Control);
            { 获得把手的矩形大小 }
            R := GetFrameRect(Control);
            { 画把手 }
            PaintDockGrabber(Canvas, Control, R);
            { 调用用户自定义画事件 }
            if DockClient <> nil then
              DockClient.DoPaintDockGrabber(Canvas, Control, R);
          end;
        end;
        SplitterCanvas := Canvas;
        SplitterCanvas.Brush.Color := TCnWinControlAccess(DockSite).Color;
        { 画分割条 }
        ForEachAt(nil, DrawSplitter, tskBackward);
        SplitterCanvas := nil;
        { 画DockSite的边框 }
        PaintDockSiteRect(Canvas);
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    Canvas.Free;
  end;*)

begin
  { 创建一个TControlCanvas对象 }
  FCanvas := TControlCanvas.Create;
  try
    { 然后把DockSite赋给Canvas的Control属性，这样Canvas就可以在DockSite上画图案了 }
    FCanvas.Control := FDockSite;
    FCanvas.Lock;
    try
      FCanvas.Handle := DC;
      try
        PaintDockSite;
      finally
        FCanvas.Handle := 0;
      end;
    finally
      FCanvas.Unlock;
    end;
  finally
    FCanvas.Free;
    FCanvas := nil;
  end;

end;

procedure TCnDockTree.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
var
  VisibleClients,
  NewX, NewY, NewWidth, NewHeight: Integer;
begin
  VisibleClients := FDockSite.VisibleDockClientCount;
  { 当DockSite小于两个停靠控件在她里面，DockRect就应该作为被设置成DockSite的客户区域 }
  if (DropCtl = nil) or (DropCtl.DockOrientation = doNoOrient) or
     {(DropCtl = Client) or }(VisibleClients < 2) then
  begin
    DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
    { 当那里有一个停靠客户我们把DockSite的客户区分成一半 }
    if VisibleClients > 0 then
    with DockRect do
      case DropAlign of
        alLeft: Right := Right div 2;
        alRight: Left := Right div 2;
        alTop: Bottom := Bottom div 2;
        alBottom: Top := Bottom div 2;
      end;
  end
  else begin
    { 否者，如果DockSite包含超过一个客户的时候， 根据鼠标下面的控件设置DockRect的坐标}
    NewX := DropCtl.Left;
    NewY := DropCtl.Top;
    NewWidth := DropCtl.Width;
    NewHeight := DropCtl.Height;
    if DropAlign in [alLeft, alRight] then
      NewWidth := DropCtl.Width div 2
    else if DropAlign in [alTop, alBottom] then
      NewHeight := DropCtl.Height div 2;
    case DropAlign of
      alRight: Inc(NewX, NewWidth);
      alBottom: Inc(NewY, NewHeight);
    end;
    DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
    if DropAlign = alClient then
      DockRect := Bounds(NewX, NewY, NewWidth, NewHeight);
  end;
  MapWindowPoints(FDockSite.Handle, 0, DockRect, 2);
end;

procedure TCnDockTree.PruneZone(Zone: TCnDockZone);

  procedure DoPrune(Zone: TCnDockZone);
  begin
    // 遍历右兄弟
    if Zone.NextSibling <> nil then
      DoPrune(Zone.NextSibling);
    // 遍历左子女
    if Zone.ChildZones <> nil then
      DoPrune(Zone.ChildZones);
    // 删除节点
    Zone.Free;
  end;

begin
  if Zone = nil then Exit;
  // 首先递归的删除子女
  if Zone.ChildZones <> nil then DoPrune(Zone.ChildZones);
  // 修正这个Zone
  if Zone.FPrevSibling <> nil then
    Zone.FPrevSibling.NextSibling := Zone.NextSibling
  else if Zone.FParentZone <> nil then
    Zone.FParentZone.ChildZones := Zone.NextSibling;
  if Zone.NextSibling <> nil then
    Zone.NextSibling.FPrevSibling := Zone.FPrevSibling;
  // 删除这个Zone
  if Zone = FTopZone then FTopZone := nil;
  Zone.Free;
end;

procedure TCnDockTree.RemoveControl(Control: TControl);
var
  Z: TCnDockZone;
begin
  Z := FindControlZone(Control, True);
  if (Z <> nil) then
  begin
    if Z = FReplacementZone then
      Z.ChildControl := nil
    else
    begin
      if (Z.ParentZone.Orientation <> doNoOrient) and Z.Visibled then
        Z.Remove(Z.LimitSize, False);
      RemoveZone(Z, False);
      SetNewBounds(nil);
      UpdateAll;
    end;
    Control.DockOrientation := doNoOrient;
    { 重画DockSite }
    FDockSite.Invalidate;
  end;
end;

procedure TCnDockTree.RemoveZone(Zone: TCnDockZone; Hide: Boolean);
var
  Sibling, LastChild: TCnDockZone;
  VisibleZoneChildCount,
  ZoneChildCount: Integer;
  label LOOP;
begin
  if not Hide then
  begin
    if Zone = nil then
      raise Exception.Create(SDockTreeRemoveError + SDockZoneNotFound);
    if Zone.ChildControl = nil then
      raise Exception.Create(SDockTreeRemoveError + SDockZoneHasNoCtl);
    VisibleZoneChildCount := Zone.ParentZone.VisibleChildCount;
    ZoneChildCount := Zone.ParentZone.ChildCount;
    if VisibleZoneChildCount <= 1 then
    begin
      // 如果当前节点的父节点只有一个可见的节点，就需要把父节点隐藏掉

      // 首先连接前兄弟和后兄弟
      if Zone.PrevSibling = nil then
      begin
        Zone.ParentZone.ChildZones := Zone.NextSibling;
        if Zone.NextSibling <> nil then
          Zone.NextSibling.PrevSibling := nil;
      end else if Zone.NextSibling = nil then
        Zone.PrevSibling.NextSibling := nil
      else
      begin
        Zone.PrevSibling.NextSibling := Zone.NextSibling;
        Zone.NextSibling.PrevSibling := Zone.PrevSibling;
      end;
      // 隐藏掉父节点，注意，这是一个递归函数，如果父节点的父节点的可见子女个数也是0，
      // 就要隐藏掉父节点的父节点
{      if Zone.ParentZone <> TopZone then
        Zone.ParentZone.Remove(Zone.ParentZone.LimitSize, True)
      else}
//      goto LOOP;
//      Exit;
    end;
    if ZoneChildCount = 2 then
    begin
      // 这个节点只有一个兄弟节点
      if Zone.PrevSibling = nil then Sibling := Zone.NextSibling
      else Sibling := Zone.PrevSibling;
      if Sibling.ChildControl <> nil then
      begin
        // 兄弟节点是一个有ChildControl并且没有子节点的节点
        if Zone.ParentZone = FTopZone then
        begin
          // 如果父节点是根节点，就删除这个节点
          FTopZone.ChildZones := Sibling;
          Sibling.PrevSibling := nil;
          Sibling.NextSibling := nil;
          Sibling.ZoneLimit := FTopZone.LimitSize;
          Sibling.Update;
        end
        else begin
          // 否则，就把兄弟节点的ChildControl移动到父节点上，然后删除这个兄弟节点
          Zone.ParentZone.Orientation := doNoOrient;
          Zone.ParentZone.ChildControl := Sibling.ChildControl;
          Zone.ParentZone.ChildZones := nil;
          Sibling.Free;
        end;
        // 更新父节点
        ForEachAt(Zone.ParentZone, UpdateZone, tskForward);
      end
      else begin
        // 兄弟节点是一个有子节点的节点，所以兄弟节点必须被放到根节点上，
        // 或者放到上一级
        if Zone.ParentZone = FTopZone then
        begin
          // 节点是根节点的子节点，所以兄弟节点必须成为根节点
          Sibling.ZoneLimit := TopXYLimit;
          TopXYLimit := FTopZone.ZoneLimit;
          FTopZone.Free;
          FTopZone := Sibling;
          Sibling.NextSibling := nil;
          Sibling.PrevSibling := nil;
          Sibling.ParentZone := nil;
        end
        else
        begin
          // 节点的父节点不是根节点，所以子节点必须被放到父节点上
          Sibling.ChildZones.PrevSibling := Zone.ParentZone.PrevSibling;
          if Sibling.ChildZones.PrevSibling = nil then
            Zone.ParentZone.ParentZone.ChildZones := Sibling.ChildZones
          else
            Sibling.ChildZones.PrevSibling.NextSibling := Sibling.ChildZones;
          LastChild := Sibling.ChildZones;
          LastChild.ParentZone := Zone.ParentZone.ParentZone;
          repeat
            LastChild := LastChild.NextSibling;
            if LastChild <> nil then
              LastChild.ParentZone := Zone.ParentZone.ParentZone
            else
              Break;
          until LastChild.NextSibling = nil;
          if LastChild <> nil then
          begin
          LastChild.NextSibling := Zone.ParentZone.NextSibling;
            if LastChild.NextSibling <> nil then
              LastChild.NextSibling.PrevSibling := LastChild;
            ForEachAt(LastChild.ParentZone, UpdateZone, tskForward);
          end;
          Zone.ParentZone.Free;
          Sibling.Free;
        end;
      end;
    end
    else begin
      // 这个节点有多个兄弟节点
      if Zone.PrevSibling = nil then
      begin
        // 存在父节点的第一个节点，所以使下一个兄弟节点作为父节点的子女，
        // 并且把本身删除掉
        Zone.ParentZone.ChildZones := Zone.NextSibling;
        if Zone.NextSibling <> nil then
        begin
          Zone.NextSibling.PrevSibling := nil;
          Zone.NextSibling.Update;
        end;
      end
      else begin
        // 不存在父节点的第一个节点，所以删除这个节点，并且调整兄弟节点
        Zone.PrevSibling.NextSibling := Zone.NextSibling;
        if Zone.NextSibling <> nil then
          Zone.NextSibling.PrevSibling := Zone.PrevSibling;
        Zone.PrevSibling.ZoneLimit := Zone.ZoneLimit;
        Zone.PrevSibling.Update;
      end;
      ForEachAt(Zone.ParentZone, UpdateZone, tskForward);
    end;
LOOP:
    Zone.Free;
  end;
  SetNewBounds(nil);
  UpdateAll;
end;

procedure TCnDockTree.ResetBounds(Force: Boolean);
var
  R: TRect;
begin
  if not (csLoading in FDockSite.ComponentState) and
    (FTopZone <> nil) and (FDockSite.DockClientCount > 0) then
  begin
    R := FDockSite.ClientRect;
    TCnWinControlAccess(FDockSite).AdjustClientRect(R);
    if Force or (not CompareMem(@R, @FOldRect, SizeOf(TRect))) then
    begin
      FOldRect := R;
      case FTopZone.Orientation of
        doHorizontal:
          begin
            FTopZone.ZoneLimit := R.Right - R.Left;
            // 当R.Bottom = R.Top的时候，说明DockSite的高度为0，
            // 因为程序是在从新设置高度后再调整ZoneLimit的，
            // 所以这条语句是使当高度设置成0的时候不能调整TopXYLimit
            if R.Bottom - R.Top > 0 then
              TopXYLimit := R.Bottom - R.Top;
          end;
        doVertical:
          begin
            FTopZone.ZoneLimit := R.Bottom - R.Top;
            // 当R.Right = R.Left的时候，说明DockSite的宽度为0，
            // 因为程序是在从新设置宽度后再调整ZoneLimit的，
            // 所以这条语句是使当宽度设置成0的时候不能调整TopXYLimit
            if R.Right - R.Left > 0 then
              TopXYLimit := R.Right - R.Left;
          end;
      end;
      SetNewBounds(nil);
      if FUpdateCount = 0 then ForEachAt(nil, UpdateZone, tskForward);
    end;
  end;
end;

procedure TCnDockTree.ScaleZone(Zone: TCnDockZone);
begin
  { ScaleZone的公式是ScaleChildZone公式的特殊情况,
    当FParentLimit是0的时候,这两个公式相等 }
  FParentLimit := 0;
  ScaleChildZone(Zone);
end;

procedure TCnDockTree.SaveToStream(Stream: TStream);
begin
  // 写流的版本
  Stream.Write(FVersion, SizeOf(FVersion));
  // 写根节点数据
  Stream.Write(FTopXYLimit, SizeOf(FTopXYLimit));
  // 从树上写所有节点的数据
  DoSaveZone(Stream, FTopZone, 0);
  Stream.Write(TreeStreamEndFlag, SizeOf(TreeStreamEndFlag));
end;

procedure TCnDockTree.SetNewBounds(Zone: TCnDockZone);

  procedure DoSetNewBounds(Zone: TCnDockZone);
  begin
    if Zone <> nil then
    begin
      if (Zone.VisibleNextSiblingCount = 0) and (Zone <> FTopZone) then
      begin
        if Zone.ParentZone = FTopZone then
          Zone.ZoneLimit := FTopXYLimit
        else
          Zone.ZoneLimit := Zone.ParentZone.ParentZone.FZoneLimit;
      end;
      if Zone.ChildZones <> nil then DoSetNewBounds(Zone.ChildZones);
      if Zone.NextSibling <> nil then DoSetNewBounds(Zone.NextSibling);
    end;
  end;

begin
  if IsLoading then Exit;
  if Zone = nil then Zone := FTopZone.ChildZones;
  DoSetNewBounds(Zone);
  { 重新画停靠框架 }
  FDockSite.Invalidate;
end;

procedure TCnDockTree.SetReplacingControl(Control: TControl);
begin
  FReplacementZone := FindControlZone(Control);
end;

procedure TCnDockTree.ShiftZone(Zone: TCnDockZone);
begin
  if (Zone <> nil) and (Zone <> FTopZone) and
    (Zone.ParentZone.Orientation = FShiftScaleOrient) then
  begin
    Inc(Zone.FZoneLimit, FShiftBy);
    if Zone.LimitSize < FMinSize then
      Zone.FZoneLimit := Zone.LimitBegin + FMinSize;
  end;
end;

procedure TCnDockTree.SplitterMouseDown(OnZone: TCnDockZone; MousePos: TPoint);
begin
  FSizingZone := OnZone;
  Mouse.Capture := FDockSite.Handle;
  FSizingWnd := FDockSite.Handle;
  FSizingDC := GetDCEx(FSizingWnd, 0, DCX_CACHE or DCX_CLIPSIBLINGS or
    DCX_LOCKWINDOWUPDATE);
  FSizePos := MousePos;
  DrawSizeSplitter;
end;

procedure TCnDockTree.SplitterMouseUp;

  procedure SetSiblingZoneSize(PosXY: Integer);
  var AZone: TCnDockZone;
    PrevCount, NextCount: Integer;
  begin
    { 处理PrevSibling }
    PrevCount := FSizingZone.PrevSiblingCount;
    AZone := FSizingZone.ParentZone.ChildZones;
    while (AZone <> nil) and (AZone <> FSizingZone) do
    begin
      if AZone.ZoneLimit >= PosXY - PrevCount * MinSize +
        Integer(AZone.PrevSibling = nil) * (SplitterWidth div 2) then
      begin
        AZone.ZoneLimit := PosXY - PrevCount * MinSize +
          Integer(AZone.PrevSibling = nil) * (SplitterWidth div 2);
        Break;
      end;
      Dec(PrevCount);
      AZone := AZone.NextSibling;
    end;

    AZone := AZone.NextSibling;
    while PrevCount > 0 do
    begin
      Dec(PrevCount);
      AZone.ZoneLimit := AZone.LimitBegin + MinSize;
      AZone := AZone.NextSibling;
    end;

    { 处理NextSibling }
    NextCount := 1;
    AZone := FSizingZone.NextSibling;
    while (AZone <> nil) do
    begin
      if AZone.ZoneLimit <= PosXY + NextCount * MinSize +
        Integer(AZone.NextSibling <> nil) * (SplitterWidth div 2) then
        AZone.ZoneLimit := PosXY + NextCount * MinSize +
          Integer(AZone.NextSibling <> nil) * (SplitterWidth div 2);
      Inc(NextCount);
      AZone := AZone.NextSibling;
    end;
  end;

begin
  Mouse.Capture := 0;
  DrawSizeSplitter;
  ReleaseDC(FSizingWnd, FSizingDC);
  if FSizingZone.ParentZone.Orientation = doHorizontal then
  begin
    FSizingZone.ZoneLimit := FSizePos.y + (SplitterWidth div 2);
    SetSiblingZoneSize(FSizePos.y);
  end else
  begin
    FSizingZone.ZoneLimit := FSizePos.x + (SplitterWidth div 2);
    SetSiblingZoneSize(FSizePos.x);
  end;
  SetNewBounds(FSizingZone.ParentZone);
  ForEachAt(FSizingZone.ParentZone, UpdateZone, tskForward);
  FSizingZone := nil;
end;

procedure TCnDockTree.UpdateAll;
begin
  if (FUpdateCount = 0) and (FDockSite.DockClientCount > 0) then
    ForEachAt(nil, UpdateZone, tskForward);
end;

procedure TCnDockTree.UpdateZone(Zone: TCnDockZone);
begin
  if (FUpdateCount = 0) then
    Zone.Update;
end;

procedure TCnDockTree.DrawSizeSplitter;
var
  R: TRect;
  PrevBrush: HBrush;
begin
  if FSizingZone <> nil then
  begin
    with R do
    begin
      if FSizingZone.ParentZone.Orientation = doHorizontal then
      begin
        Left := FSizingZone.Left;
        Top := FSizePos.Y - (SplitterWidth div 2);
        Right := Left + FSizingZone.Width;
        Bottom := Top + SplitterWidth;
      end
      else begin
        Left := FSizePos.X - (SplitterWidth div 2);
        Top := FSizingZone.Top;
        Right := Left + SplitterWidth;
        Bottom := Top + FSizingZone.Height;
      end;
    end;
    PrevBrush := SelectObject(FSizingDC, FBrush.Handle);
    with R do
      PatBlt(FSizingDC, Left, Top, Right - Left, Bottom - Top, PATINVERT);
    SelectObject(FSizingDC, PrevBrush);
  end;
end;

function TCnDockTree.GetSplitterLimit(AZone: TCnDockZone; IsCurrent, IsMin: Boolean): Integer;
begin
  if IsCurrent then
  begin
    Result := AZone.GetSplitterLimit(False);
  end else
  begin
    if AZone.AfterClosestVisibleZone <> nil then
      Result := AZone.AfterClosestVisibleZone.GetSplitterLimit(True)
    else
      Result := AZone.ZoneLimit + AZone.LimitSize;
  end;
end;

procedure TCnDockTree.ControlVisibilityChanged(Control: TControl;
  Visible: Boolean);
begin
  if Visible then
  begin
    ShowControl(Control);
  end else
    HideControl(Control);
end;

procedure TCnDockTree.WindowProc(var Message: TMessage);
var TempZone: TCnDockZone;
  HitTestValue: Integer;
begin
  case Message.Msg of
    CM_DOCKNOTIFICATION:
      with TCMDockNotification(Message) do
        if (NotifyRec.ClientMsg = CM_VISIBLECHANGED) then
          ControlVisibilityChanged(Client, Boolean(NotifyRec.MsgWParam));
    WM_MOUSEMOVE:
      { 捕获鼠标移动的消息 }
      DoMouseMove(TWMMouse(Message), TempZone, HitTestValue);
    WM_LBUTTONDBLCLK:
      { 捕获到鼠标左键双击的消息 }
      DoLButtonDbClk(TWMMouse(Message), TempZone, HitTestValue);
    WM_LBUTTONDOWN:
      { 捕获到鼠标左键按下的消息 }
      if DoLButtonDown(TWMMouse(Message), TempZone, HitTestValue) then Exit;
    WM_LBUTTONUP:
      { 捕获到鼠标左键放开的消息 }
      DoLButtonUp(TWMMouse(Message), TempZone, HitTestValue);
    WM_MBUTTONDOWN:
      { 捕获到鼠标中键按下的消息 }
      DoMButtonDown(TWMMouse(Message), TempZone, HitTestValue);
    WM_MBUTTONUP:
      { 捕获到鼠标中键放开的消息 }
      DoMButtonUp(TWMMouse(Message), TempZone, HitTestValue);
    WM_RBUTTONDOWN:
      { 捕获到鼠标右键按下的消息 }
      DoRButtonDown(TWMMouse(Message), TempZone, HitTestValue);
    WM_RBUTTONUP:
      { 捕获到鼠标右键放开的消息 }
      DoRButtonUp(TWMMouse(Message), TempZone, HitTestValue);
    WM_SETCURSOR:
      begin
        { 捕获到设置光标的消息 }
        DoSetCursor(TWMSetCursor(Message), TempZone, HitTestValue);
        if Message.Result = 1 then Exit;
      end;
  end;

  { 调用老的WndProc }
  FOldWndProc(Message);

  if Message.Msg = CM_HINTSHOW then
    { 捕获到提示窗体显示的消息 }
    DoHintShow(TCMHintShow(Message), TempZone, HitTestValue);
end;

procedure TCnDockTree.SetGrabberSize(const Value: Integer);
begin
  if FGrabberSize <> Value then
  begin
    FGrabberSize := Value;
    UpdateAll;
    Docksite.Invalidate;
  end;
end;

function TCnDockTree.GetGrabbersPosition: TGrabbersPosition;
begin
  if DockSite.Align in [alTop, alBottom] then
    Result := gpLeft
  else Result := gpTop;
end;

function TCnDockTree.GetBottomGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  Result := nil;
end;

function TCnDockTree.GetBorderHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
var ARect: TRect;
begin
  Result := nil;
  { 获得节点的矩形大小 }
  ARect := Zone.GetFrameRect;
  { 如果鼠标的坐标在矩形内 }
  if PtInRect(ARect, MousePos) then
  begin
    { 减去边框的宽度 }
    InflateRect(ARect, -BorderWidth, -BorderWidth);
    if not PtInRect(ARect, MousePos) then
    begin
      Result := Zone;
      HTFlag := HTBORDER;
    end;
  end;
end;

function TCnDockTree.GetLeftGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.X >= Zone.Left + BorderWidth) and (MousePos.X <= Zone.Left + BorderWidth + FGrabberSize) and
    (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + Zone.Height) then
  begin
    Result := Zone;
    if MousePos.Y < Zone.ChildControl.Top + FGrabberSize + 3 then HTFlag := HTCLOSE
    else HTFlag := HTCAPTION;
  end else Result := nil;
end;

function TCnDockTree.GetRightGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  Result := nil;
end;

function TCnDockTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.Y >= Zone.Top + BorderWidth) and (MousePos.Y <= Zone.Top + BorderWidth + FGrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
      if MousePos.X > Left + Width - FGrabberSize - 3 then HTFlag := HTCLOSE
      else HTFlag := HTCAPTION;
  end else Result := nil;
end;

function TCnDockTree.GetActiveControl: TControl;
begin
  Result := FActiveControl;
end;

procedure TCnDockTree.SetActiveControl(const Value: TControl);
begin
  FActiveControl := Value;
end;

function TCnDockTree.GetGrabberSize: Integer;
begin
  Result := FGrabberSize;
end;

function TCnDockTree.FindControlZoneAndLevel(Control: TControl;
  var CtlLevel: Integer; IncludeHide: Boolean): TCnDockZone;
var
  CtlZone: TCnDockZone;

  procedure DoFindControlZone(StartZone: TCnDockZone; Level: Integer);
  begin
    if (StartZone.ChildControl = Control) and (StartZone.Visibled or IncludeHide) then
    begin
      CtlZone := StartZone;
      CtlLevel := Level;
    end
    else begin
      // 遍历右兄弟
      if (CtlZone = nil) and (StartZone.NextSibling <> nil) then
        DoFindControlZone(StartZone.NextSibling, Level);
      // 遍历左子女
      if (CtlZone = nil) and (StartZone.ChildZones <> nil) then
        DoFindControlZone(StartZone.ChildZones, Level + 1);
      if (CtlZone <> nil) and (not CtlZone.Visibled) then CtlZone := nil;
    end;
  end;

begin
  CtlZone := nil;
  CtlLevel := 0;
  if (Control <> nil) and (FTopZone <> nil) then DoFindControlZone(FTopZone, 0);
  Result := CtlZone;
end;

procedure TCnDockTree.SetSplitterWidth(const Value: Integer);
begin
  if FSplitterWidth <> Value then
  begin
    FSplitterWidth := Value;
    if FUpdateCount <= 0 then
      UpdateAll;
  end;
end;

procedure TCnDockTree.SetTopZone(const Value: TCnDockZone);
begin
  FTopZone := Value;
end;

procedure TCnDockTree.SetTopXYLimit(const Value: Integer);
begin
  FTopXYLimit := Value;
end;

procedure TCnDockTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);

var Control: TControl;
  DockClient: TCnDockClient;
begin
  { 如果鼠标按下的地方是分割条的位置上 }
  if FSizingZone <> nil then
  begin
    { 画分割条的形状 }
    DrawSizeSplitter;
    { 把鼠标的位置保存起来 }
    FSizePos := SmallPointToPoint(Message.Pos);
    { 计算鼠标的位置，对其做一些限制 }
    CalcSplitterPos;
    { 画分割条的形状 }
    DrawSizeSplitter;
  end;
  { 返回鼠标的位置 }
  Zone := InternalHitTest(SmallPointToPoint(Message.Pos), HTFlag);
  if Zone <> nil then
  begin
    { 首先调用TCnDockClient中的DoNCMouseMove方法 }
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      DockClient.DoNCMouseMove(Cn_CreateNCMessage(
        DockSite, WM_NCMOUSEMOVE, HTFlag, FSizePos), msConjoin);
    Control := Zone.ChildControl;
  end
  else Control := nil;
  if (Control <> nil) and (HTFlag <> FOldHTFlag) then
  begin
    { 如果鼠标的位置和原来的位置不一样，就更新提示窗体 }
    Application.HideHint;
    Application.HintMouseMessage(Control, TMessage(Message));
    Application.ActivateHint(SmallPointToPoint(Message.Pos));
    FOldHTFlag := HTFlag;
  end;
end;

function TCnDockTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): Boolean;
var P: TPoint;
  Msg: TMsg;
//  DockClient: TCnDockClient;
begin
  Result := False;
  P := SmallPointToPoint(Message.Pos);
  { 返回鼠标的位置和当前的节点 }
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) then
  begin
    if HTFlag = HTSPLITTER then
      { 如果鼠标的位置刚好在分割条的位置上，
      就调用SplitterMouseDown函数进行一些必要的处理 }
      SplitterMouseDown(Zone, P)
    else if (HTFlag = HTCAPTION) or (HTFlag = HTBORDER) then
    begin
      { 如果鼠标的位置是在标题栏上 }
      { 给全局变量GlobalDockClient赋值 }
      GlobalDockClient := FindDockClient(Zone.ChildControl);
      if GlobalDockClient <> nil then
        { 首先调用TCnDockClient中的DoNCButtonDown方法 }
        GlobalDockClient.DoNCButtonDown(Cn_CreateNCMessage(
          DockSite, WM_NCLBUTTONDOWN, HTFlag, P), mbLeft, msConjoin);

      if (not PeekMessage(Msg, FDockSite.Handle, WM_LBUTTONDBLCLK,
        WM_LBUTTONDBLCLK, PM_NOREMOVE)) and
        (Zone.ChildControl is TWinControl) then
        { 使节点上面的控件获得焦点 }
//        if not Zone.ChildControl.Focused then
//          TWinControl(Zone.ChildControl).SetFocus;
        if (GetActiveControl <> Zone.ChildControl) and Zone.ChildControl.CanFocus then
          Zone.ChildControl.SetFocus;
        if (TCnWinControlAccess(Zone.ChildControl).DragKind = dkDock) and
        (TCnWinControlAccess(Zone.ChildControl).DragMode = dmAutomatic)then
        begin
          { 开始拖动 }
          BeginDrag(Zone.ChildControl, True);
        end;
      Result := True;
    end;
  end;
end;

procedure TCnDockTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var P: TPoint;
  DockClient: TCnDockClient;
begin
  if FSizingZone = nil then
  begin
    { 如果鼠标按下的位置不是在分割条上，
    也就是说是在标题栏上面，就判断它具体是在标题栏的哪个具体位置 }
    P := SmallPointToPoint(Message.Pos);
    Zone := InternalHitTest(P, HTFlag);
    if (Zone <> nil) then
    begin
      if (HTFlag <> HTSPLITTER) and (Zone.ChildControl <> nil) then
      begin
        DockClient := FindDockClient(Zone.ChildControl);
        if DockClient <> nil then
          { 首先调用TCnDockClient中的DoNCButtonDown方法 }
          DockClient.DoNCButtonUp(Cn_CreateNCMessage(
            DockSite, WM_NCLBUTTONUP, HTFlag, P), mbLeft, msConjoin);
        if (HTFlag = HTCLOSE) then
        begin
          if (DockClient <> nil) and (not DockClient.EnableCloseBtn) then Exit;
          DoHideZoneChild(Zone);
        end;
      end;
    end;
  end
  else
    { 如果鼠标按下的位置是在分割条上，就调用SplitterMouseUp函数进行一些必要的设置}
    SplitterMouseUp;
end;

procedure TCnDockTree.DoLButtonDbClk(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var //AControl: TWinControl;
  P: TPoint;
begin
  { 首先判断鼠标的位置是在哪里 }
  P := SmallPointToPoint(Message.Pos);
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil)
    and (HTFlag = HTCAPTION) or (HTFlag = HTBORDER) then
  begin
    if (HTFlag <> HTSPLITTER) then
      { 首先调用TCnDockClient中的DoNCButtonDown方法 }
      GlobalDockClient.DoNCButtonDblClk(Cn_CreateNCMessage(
        DockSite, WM_NCLBUTTONUP, HTFlag, P), mbLeft, msConjoin);
    if GlobalDockClient.CanFloat then
    begin
      { 如果鼠标的位置是在标题栏上面，就使节点上面的控件浮动 }
      CnGlobalDockPresident.CancelDrag;
      Zone.LButtonDbClkMothed;
    end;
    Zone := nil;
  end;
end;

procedure TCnDockTree.DoSetCursor(var Message: TWMSetCursor;
  var Zone: TCnDockZone; out HTFlag: Integer);
var
  P: TPoint;
begin
  { 获得鼠标的位置并且进行坐标转换 }
  GetCursorPos(P);
  P := FDockSite.ScreenToClient(P);
  with Message do
    if (Smallint(HitTest) = HTCLIENT) and (CursorWnd = FDockSite.Handle)
      and (FDockSite.VisibleDockClientCount > 0) then
    begin
      { 确定鼠标在哪个位置 }
      Zone := InternalHitTest(P, HTFlag);
      if (Zone <> nil) and (HTFlag = HTSPLITTER) then
      begin
        { 如果鼠标是在分割条上，就调用SetSplitterCursor设置光标的形状 }
        SetSplitterCursor(Zone.ParentZone.Orientation);
        Result := 1;
      end;
    end;
end;

procedure TCnDockTree.DoHintShow(var Message: TCMHintShow;
  var Zone: TCnDockZone; out HTFlag: Integer);
var
  Control: TWinControl;
  R: TRect;
  ADockClient: TCnDockClient;
  CanShow: Boolean;
begin
  with Message do
  begin
    if Result = 0 then
    begin
      { 确定鼠标在哪个位置 }
      Zone := InternalHitTest(Message.HintInfo.CursorPos, HTFlag);
      if Zone <> nil then
        Control := Zone.ChildControl
      else Control := nil;
      { 查找到Control上面的TCnDockClient，如果她的ShowHint属性为False，就退出 }
      ADockClient := FindDockClient(Control);
      if (ADockClient <> nil) and (not ADockClient.ShowHint) then
        Exit;

      if HTFlag = HTSPLITTER then
        HintInfo^.HintStr := ''
      else if (Control <> nil){ and (HTFlag in [HTCAPTION, HTCLOSE]) }then
      begin
        { 设置提示窗体的大小 }
        R := GetFrameRect(Control);
        if HTFlag = HTCAPTION then
        begin
          { 标题栏的提示信息 }
          HintInfo^.HintStr := TCnWinControlAccess(Control).Caption;
        end else if HTFlag = HTCLOSE then
          { 关闭按钮的提示信息 }
          HintInfo^.HintStr := gs_CnDockTreeCloseBtnHint
        else DoOtherHint(Zone, HTFlag, HintInfo^.HintStr);

        HintInfo^.CursorRect := R;
        { 调用TCnDockClient的DoFormShowHint方法。}
        CanShow := True;
        if ADockClient <> nil then
          ADockClient.DoFormShowHint(HTFlag, HintInfo^.HintStr, CanShow);
        if not CanShow then
          HintInfo^.HintStr := '';
      end;
    end;
  end;
end;

procedure TCnDockTree.SetSplitterCursor(CursorIndex: TDockOrientation);
const
  SizeCursors: array[TDockOrientation] of TCursor = (crDefault, crVSplit, crHSplit);
begin
  Windows.SetCursor(Screen.Cursors[SizeCursors[CursorIndex]]);
end;

procedure TCnDockTree.SetCnDockZoneClass(const Value: TCnDockZoneClass);
begin
  FCnDockZoneClass := Value;
end;

procedure TCnDockTree.DoMButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { 查找到控件上的TCnDockClient，并且调用TCnDockClient的DoNCButtonDown方法 }
      DockClient.DoNCButtonDown(Msg, mbMiddle, msConjoin);
  end;
end;

procedure TCnDockTree.DoMButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { 查找到控件上的TCnDockClient，并且调用TCnDockClient的DoNCButtonUp方法 }
      DockClient.DoNCButtonUp(Msg, mbMiddle, msConjoin);
  end;
end;

procedure TCnDockTree.DoRButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { 查找到控件上的TCnDockClient，并且调用TCnDockClient的DoNCButtonDown方法 }
      DockClient.DoNCButtonDown(Msg, mbRight, msConjoin);
  end;
end;

procedure TCnDockTree.DoRButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var Msg: TWMNCHitMessage;
  DockClient: TCnDockClient;
begin
  Msg := DoMouseEvent(Message, Zone, HTFlag);
  if Msg.Result > 0 then
  begin
    DockClient := FindDockClient(Zone.ChildControl);
    if DockClient <> nil then
      { 查找到控件上的TCnDockClient，并且调用TCnDockClient的DoNCButtonUp方法 }
      DockClient.DoNCButtonUp(Msg, mbRight, msConjoin);
  end;
end;

function TCnDockTree.DoMouseEvent(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): TWMNCHitMessage;
var APoint: TPoint;
begin
  Result.Result := 0;
  APoint := SmallPointToPoint(Message.Pos);
  { 首先检测到鼠标的位置是在哪个控件的标题上 }
  Zone := InternalHitTest(APoint, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil) and (HTFlag <> HTSPLITTER) then
  begin
    { 创建一个TWMNCHitMessage结构，其中'Message.Msg + WM_NCMOUSEFIRST - WM_MOUSEFIRST'，
      这是因为WM_MOUSExxx和WM_NCMOUSExxx是一一对应的，他们相差了WM_NCMOUSEFIRST - WM_MOUSEFIRST。 }
    Result := Cn_CreateNCMessage(DockSite, Message.Msg + WM_NCMOUSEFIRST - WM_MOUSEFIRST, HTFlag, APoint);
    Result.Result := 1;
  end;
end;

procedure TCnDockTree.DoMButtonDbClk(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var P: TPoint;
  DockClient: TCnDockClient;
begin
  { 首先判断鼠标的位置是在哪里 }
  P := SmallPointToPoint(Message.Pos);
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil) and (HTFlag = HTCAPTION) then
  begin
    if (HTFlag <> HTSPLITTER) then
    begin
      DockClient := FindDockClient(Zone.ChildControl);
      if DockClient <> nil then
        { 首先调用TCnDockClient中的DoNCButtonDown方法 }
        DockClient.DoNCButtonDblClk(Cn_CreateNCMessage(
          DockSite, WM_NCLBUTTONUP, HTFlag, P), mbMiddle, msConjoin);
    end;
  end;
end;

procedure TCnDockTree.DoRButtonDbClk(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var P: TPoint;
  DockClient: TCnDockClient;
begin
  { 首先判断鼠标的位置是在哪里 }
  P := SmallPointToPoint(Message.Pos);
  Zone := InternalHitTest(P, HTFlag);
  if (Zone <> nil) and (Zone.ChildControl <> nil) and (HTFlag = HTCAPTION) then
  begin
    if (HTFlag <> HTSPLITTER) then
    begin
      DockClient := FindDockClient(Zone.ChildControl);
      if DockClient <> nil then
        { 首先调用TCnDockClient中的DoNCButtonDown方法 }
        DockClient.DoNCButtonDblClk(Cn_CreateNCMessage(
          DockSite, WM_NCLBUTTONUP, HTFlag, P), mbRight, msConjoin);
    end;
  end;
end;

function TCnDockTree.GetFrameRect(Control: TControl): TRect;
var ALeft, ATop: Integer;
begin
  if Control <> nil then
  begin
    Result := Control.BoundsRect;
    ALeft := Result.Left;
    Atop := Result.Top;
    AdjustDockRect(Control, Result);
    Dec(Result.Left, 2 * (Result.Left - Control.Left) + 1);
    Dec(Result.Top, 2 * (Result.Top - Control.Top));
    Dec(Result.Right, 2 * (Result.Right - ALeft - Control.Width));
    Dec(Result.Bottom, 2 * (Result.Bottom - ATop - Control.Height));
  end else raise Exception.Create(gs_ControlCannotIsNil);
end;

function TCnDockTree.GetSpiltterRect(Zone: TCnDockZone): TRect;
var A, B, C, D: Integer;
begin
  if (Zone <> nil) and Zone.Visibled and (Zone.ParentZone <> nil)
    and (Zone.VisibleNextSiblingCount >= 1)
    and (Zone.ParentZone.Orientation <> doNoOrient) then
  begin
    A := Zone.ParentZone.LimitBegin;
    B := Zone.ParentZone.ZoneLimit;
    C := Zone.ZoneLimit - SplitterWidth;
    D := C + 1 * SplitterWidth;
    if Zone.ParentZone.Orientation = doHorizontal then
      Result := Rect(A, C, B, D)
    else if Zone.ParentZone.Orientation = doVertical then
      Result := Rect(C, A, D, B);
  end else Result := Rect(0, 0, 0, 0);
end;

{procedure TCnDockTree.PaintDockSplitter(Canvas: TCanvas; Control: TControl;
  const ARect: TRect);
begin
  with Canvas do
  begin
    FillRect(ARect);
  end;
end;}

procedure TCnDockTree.BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer);
var ADockClient: TCnDockClient;
begin
  ADockClient := FindDockClient(Control);
  { 开始拖动 }
  if ADockClient <> nil then
    CnGlobalDockPresident.BeginDrag(Control, ADockClient.DirectDrag, Threshold);
end;

function TCnDockTree.GetFrameRectEx(Control: TControl): TRect;
begin
  if Control <> nil then
  begin
    Result := GetFrameRect(Control);
    MapWindowPoints(DockSite.Handle, 0, Result, 2);
  end;
end;

procedure TCnDockTree.DrawDockSiteRect;
begin
  { 没事做 }
end;

procedure TCnDockTree.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    if FUpdateCount <= 0 then
      UpdateAll;
  end;
end;

function TCnDockTree.GetBorderWidth: Integer;
begin
  Result := FBorderWidth;
end;

function TCnDockTree.GetSplitterWidth: Integer;
begin
  Result := FSplitterWidth;
end;

procedure TCnDockTree.DrawSplitter(Zone: TCnDockZone);
var R: TRect;
begin
  { 获得分割条的矩形大小 }
  R := GetSpiltterRect(Zone);
  { 画分割条 }
  DrawSplitterRect(R);
end;

function TCnDockTree.GetDockEdge(DockRect: TRect; MousePos: TPoint;
  var DropAlign: TAlign; Control: TControl): TControl;
begin
  Result := nil;
  { 没事情做 }
end;

function TCnDockTree.GetDockSiteOrient: TDockOrientation;
begin
  Result := Cn_GetControlOrient(DockSite);
end;

procedure TCnDockTree.BeginResizeDockSite;
begin
  Inc(FResizeCount);
end;

procedure TCnDockTree.EndResizeDockSite;
begin
  Dec(FResizeCount);
  if FResizeCount < 0 then
    FResizeCount := 0;
end;

procedure TCnDockTree.ScaleChildZone(Zone: TCnDockZone);
begin
  // 必须是可见的，停靠方向和ShiftScaleOrient要相同。
  if (Zone <> nil) and (Zone.ParentZone <> nil) and Zone.Visibled and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
  begin
    // 根据计算公式得到Zone的ZoneLimit。
    Zone.ZoneLimit := Integer(Round(Zone.ZoneLimit * ScaleBy + FParentLimit * (1 - ScaleBy)));
    // Zone的LimitSize不能小于规定的最小尺寸MinSize。
(*    if (Zone.LimitSize < FMinSize) then
      Zone.FZoneLimit := Zone.LimitBegin + FMinSize;
    // Zone的LimitBegin和前一个可见的节点的ZoneLimit的位置不能小于规定的最小尺寸MinSize
    if (Zone.BeforeClosestVisibleZone <> nil) and (Zone.LimitBegin > DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
      (Zone.VisibleNextSiblingCount + 1) * MinSize + {Integer(Zone.NextSibling <> nil) * }SplitterWidth div 2) then
      Zone.BeforeClosestVisibleZone.ZoneLimit := DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
        (Zone.VisibleNextSiblingCount + 1) * MinSize + {Integer(Zone.NextSibling <> nil) * }SplitterWidth div 2;
*)  end;
end;

procedure TCnDockTree.ScaleSiblingZone(Zone: TCnDockZone);
begin
  {和ScaleChildZone的计算公式是一样的}
  ScaleChildZone(Zone);
{  if Zone = nil then Exit;
  if (Zone <> nil) and (Zone.ParentZone <> nil) and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
      Zone.ZoneLimit := Integer(Round(Zone.ZoneLimit * ScaleBy + FParentLimit * (1 - ScaleBy)));
}
end;

function TCnDockTree.GetDockSiteSize: Integer;
begin
  case DockSiteOrient of
    doVertical: Result := DockSite.Width;
    doHorizontal: Result := DockSite.Height;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.SetDockSiteSize(const Value: Integer);
begin
  DockSite.Parent.DisableAlign;
  try
    // 如果DockSite是在右边或者下边，就要重新设置DockSiteBegin(Top或者Left),
    // 这样不至于打乱控件之间原来固有的次序。
    if DockSite.Align in [alRight, alBottom] then
      DockSiteBegin := DockSiteBegin - (Value - DockSiteSize);
    case DockSiteOrient of
      doVertical: DockSite.Width := Value;
      doHorizontal: DockSite.Height := Value;
    else
      raise Exception.Create(gs_CannotSetValueWithNoOrient);
    end;
  finally
    DockSite.Parent.EnableAlign;
  end;
end;

procedure TCnDockTree.SetMinSize(const Value: Integer);
begin
  FMinSize := Value;
end;

function TCnDockTree.GetDockSiteBegin: Integer;
begin
  case DockSiteOrient of
    doVertical: Result := DockSite.Left;
    doHorizontal: Result := DockSite.Top;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.SetDockSiteBegin(const Value: Integer);
begin
  case DockSiteOrient of
    doVertical: DockSite.Left := Value;
    doHorizontal: DockSite.Top := Value;
  else
    raise Exception.Create(gs_CannotSetValueWithNoOrient);
  end;
end;

function TCnDockTree.GetDockSiteSizeA: Integer;
begin
  case DockSiteOrient of
    doVertical: Result := DockSite.Height;
    doHorizontal: Result := DockSite.Width;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.SetDockSiteSizeA(const Value: Integer);
begin
  case DockSiteOrient of
    doVertical: DockSite.Height := Value;
    doHorizontal: DockSite.Width := Value;
  else
    raise Exception.Create(gs_CannotSetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.CalcSplitterPos;
var
  MinWidth,
  TestLimit: Integer;
begin
  MinWidth := MinSize;
  if (FSizingZone.ParentZone.Orientation = doHorizontal) then
  begin
    TestLimit := GetSplitterLimit(FSizingZone, True, False) + MinWidth;
    if FSizePos.y <= TestLimit then FSizePos.y := TestLimit;
    TestLimit := GetSplitterLimit(FSizingZone, False, True) - MinWidth - SplitterWidth;
    if FSizePos.y >= TestLimit then FSizePos.y := TestLimit;
  end
  else begin
    TestLimit := GetSplitterLimit(FSizingZone, True, False) + MinWidth;
    if FSizePos.x <= TestLimit then FSizePos.x := TestLimit;
    TestLimit := GetSplitterLimit(FSizingZone, False, True) - MinWidth - SplitterWidth;
    if FSizePos.x >= TestLimit then FSizePos.x := TestLimit;
  end;
end;

procedure TCnDockTree.SetVersion(const Value: Integer);
begin
  FVersion := Value;
end;

procedure TCnDockTree.DoSaveZone(Stream: TStream;
  Zone: TCnDockZone; Level: Integer);
begin
  with Stream do
  begin
    { 节点的等级, TopZone为0 }
    Write(Level, SizeOf(Level));
    CustomSaveZone(Stream, Zone);
  end;
  // 遍历左子女
  if Zone.ChildZones <> nil then
    DoSaveZone(Stream, Zone.ChildZones, Level + 1);
  // 遍历右兄弟
  if Zone.NextSibling <> nil then
    DoSaveZone(Stream, Zone.NextSibling, Level);
end;

procedure TCnDockTree.WriteControlName(Stream: TStream; ControlName: string);
var
  NameLen: Integer;
begin
  NameLen := Length(ControlName);
  Stream.Write(NameLen, SizeOf(NameLen));
  if NameLen > 0 then Stream.Write(Pointer(ControlName)^, NameLen * SizeOf(Char));
end;

procedure TCnDockTree.DoLoadZone(Stream: TStream);
var
  Level, LastLevel, I: Integer;
  Zone, LastZone, NextZone: TCnDockZone;
begin
  LastLevel := 0;
  LastZone := nil;
  while True do
  begin
    with Stream do
    begin
      Read(Level, SizeOf(Level));
      if Level = TreeStreamEndFlag then Break;
      Zone := FCnDockZoneClass.Create(Self);
      CustomLoadZone(Stream, Zone);
      if Zone = nil then
        Continue;
    end;
    if Level = 0 then FTopZone := Zone
    else if Level = LastLevel then
    begin
      LastZone.NextSibling := Zone;
      Zone.FPrevSibling := LastZone;
      Zone.FParentZone := LastZone.FParentZone;
    end
    else if Level > LastLevel then
    begin
      LastZone.ChildZones := Zone;
      Zone.FParentZone := LastZone;
    end
    else if Level < LastLevel then
    begin
      NextZone := LastZone;
      for I := 1 to LastLevel - Level do NextZone := NextZone.FParentZone;
      NextZone.NextSibling := Zone;
      Zone.FPrevSibling := NextZone;
      Zone.FParentZone := NextZone.FParentZone;
    end;
    LastLevel := Level;
    LastZone := Zone;
  end;
end;

procedure TCnDockTree.ReadControlName(Stream: TStream;
  var ControlName: string);
var
  Size: Integer;
begin
  ControlName := '';
  Size := 0;
  Stream.Read(Size, SizeOf(Size));
  if Size > 0 then
  begin
    SetLength(ControlName, Size);
    Stream.Read(Pointer(ControlName)^, Size * SizeOf(Char));
  end;
end;

procedure TCnDockTree.CustomLoadZone(Stream: TStream; var Zone: TCnDockZone);
var CompName: string;
begin
  with Stream do
  begin
    Read(Zone.FOrientation, SizeOf(Zone.Orientation));
    Read(Zone.FZoneLimit, SizeOf(Zone.FZoneLimit));
    Read(Zone.FVisibled, SizeOf(Zone.Visibled));
    Read(Zone.FControlVisibled, SizeOf(Zone.FControlVisibled));
    Read(Zone.FVisibleSize, SizeOf(Zone.VisibleSize));
    Read(Zone.FIsInside, SizeOf(Zone.FIsInside));
    ReadControlName(Stream, CompName);
    if CompName <> '' then
    begin
      if not Zone.SetControlName(CompName) then
      begin
        { 如果没有找到这个控件就把节点删除 }
        Zone.Free;
        Zone := nil;
        Exit;
      end;
    end;
  end;
end;

procedure TCnDockTree.CustomSaveZone(Stream: TStream; Zone: TCnDockZone);
var AVisible: Boolean;
begin
  with Stream do
  begin
    { 节点的方向 }
    Write(Zone.Orientation, SizeOf(Zone.Orientation));
    { 节点的绝对坐标 }
    Write(Zone.ZoneLimit, SizeOf(Zone.ZoneLimit));
    { 节点是否可见 }
    if Zone.ChildControl <> nil then
      AVisible := Zone.ChildControl.Visible;
    Write(Zone.Visibled, SizeOf(Zone.Visibled));
    { 控件是否可见 }
    AVisible := False;
    if Zone.ChildControl <> nil then
      AVisible := Zone.ChildControl.Visible;
    Write(AVisible, SizeOf(AVisible));
    { 节点在可见的时候的LimitSize }
    Write(Zone.VisibleSize, SizeOf(Zone.VisibleSize));
    { 是否在DockSite的里面 }
    Zone.IsInside := True;
    if (Zone.ChildControl <> nil) and (Zone.ChildControl.HostDockSite <> DockSite)
      and not (DockSite is TCnVSPopupPanel) then
      Zone.IsInside := False;
    Write(Zone.IsInside, SizeOf(Zone.IsInside));
    { 节点所含有的ChildControl的名字 }
    WriteControlName(Stream, Zone.GetControlName);
  end;
end;

procedure TCnDockTree.SetDockSiteSizeWithOrient(Orient: TDockOrientation;
  const Value: Integer);
begin
  case Orient of
    doVertical: DockSite.Width := Value;
    doHorizontal: DockSite.Height := Value;
  else
    raise Exception.Create(gs_CannotSetValueWithNoOrient);
  end;
end;

procedure TCnDockTree.DoOtherHint(Zone: TCnDockZone;
  HTFlag: Integer; var HintStr: string);
begin
  { 没事做 }
end;

function TCnDockTree.GetHTFlag(MousePos: TPoint): Integer;
var Zone: TCnDockZone;
begin
  Zone := InternalHitTest(MousePos, Result);
  if Zone = nil then Result := HTNONE;
end;

procedure TCnDockTree.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(DockSite.Handle, InfluenceRect);
  InflateRect(InfluenceRect, DefExpandoRect, DefExpandoRect);
end;

function TCnDockTree.GetDockRect: TRect;
begin
  Result := FDockRect;
end;

procedure TCnDockTree.SetDockRect(const Value: TRect);
begin
  FDockRect := Value;
end;

function TCnDockTree.GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign;
var
  CRect, DRect: TRect;
begin
  Result := alRight;
  if DropCtl <> nil then
  begin
    CRect := Client.BoundsRect;
    DRect := DropCtl.BoundsRect;
    if (CRect.Top <= DRect.Top) and (CRect.Bottom < DRect.Bottom) and
       (CRect.Right >= DRect.Right) then
      Result := alTop
    else if (CRect.Left <= DRect.Left) and (CRect.Right < DRect.Right) and
       (CRect.Bottom >= DRect.Bottom) then
      Result := alLeft
    else if CRect.Top >= ((DRect.Top + DRect.Bottom) div 2) then
      Result := alBottom;
  end;
end;

procedure TCnDockTree.HideControl(Control: TControl);
var
  Z: TCnDockZone;
begin
  { 如果ReplacementZone <> nil，就说明是在进行Load操作，不用执行下面的操作，直接退出就可以了 }
  if ReplacementZone <> nil then Exit;
  Z := FindControlZone(Control);
  if (Z <> nil) then
  begin
    if Z = FReplacementZone then
      Z.ChildControl := nil
    else
    begin
      if TopZone.VisibleChildTotal = 1 then
        Z.Remove(TopXYLimit, True)
      else Z.Remove(Z.LimitSize, True);
//      UpdateAll;
    end;
    Control.DockOrientation := doNoOrient;
    SetNewBounds(nil);
    UpdateAll;
    { 重画DockSite }
    FDockSite.Invalidate;
  end;
end;

procedure TCnDockTree.ShowControl(Control: TControl);
var Z: TCnDockZone;
begin
  { 如果ReplacementZone <> nil，就说明是在进行Load操作，不用执行下面的操作，直接退出就可以了 }
  if ReplacementZone <> nil then Exit;
  Z := FindControlZone(Control, True);
  if Z <> nil then
    Z.Insert(Z.VisibleSize, False);

  SetNewBounds(nil);
  UpdateAll;
  DockSite.Invalidate;
end;

procedure TCnDockTree.DoGetNextLimit(Zone, AZone: TCnDockZone; var LimitResult: Integer);
begin
  if (Zone <> AZone) and
    (Zone.ParentZone.Orientation = AZone.ParentZone.Orientation) and
    (Zone.ZoneLimit > AZone.FZoneLimit) and ((Zone.ChildControl = nil) or
    ((Zone.ChildControl <> nil) and (Zone.ChildControl.Visible))) then
    LimitResult := Min(LimitResult, Zone.ZoneLimit);
  if Zone.NextSibling <> nil then DoGetNextLimit(Zone.NextSibling, AZone, LimitResult);
  // 下面这条被注释掉的语句，表示可以忽略掉不是同一个父节点的节点，
  // 使鼠标调节分割条更加自由。
  if (Zone.ChildZones <> nil){ and (Zone = AZone.AfterClosestVisibleZone) }then
    DoGetNextLimit(Zone.ChildZones, AZone, LimitResult);
end;

procedure TCnDockTree.UpdateChild(Zone: TCnDockZone);
begin
  if (FUpdateCount = 0) and (FDockSite.DockClientCount > 0) then
    ForEachAt(Zone, UpdateZone, tskForward);
end;

function TCnDockTree.GetDockClientLimit(Orient: TDockOrientation; IsMin: Boolean): Integer;
var Zone: TCnDockZone;
begin
  Result := 0;
  if TopZone.ChildCount = 1 then
    Result := Integer(not IsMin) * DockSiteSizeWithOrient[Orient]
  else
  begin
    if IsMin then
    begin
      if TopZone.Orientation = Orient then
        Zone := TopZone.LastVisibleChildZone
      else Zone := TopZone;
      if Zone <> nil then
        Result := Zone.LimitBegin;
    end else
    begin
      if TopZone.Orientation = Orient then
        Zone := TopZone.FirstVisibleChildZone
      else Zone := TopZone;
      if Zone <> nil then
        Result := Zone.ZoneLimit;
    end;

    TopZone.DoGetSplitterLimit(Orient, IsMin, Result);
  end;

{  if TopZone <> nil then
  begin
    if TopZone.ChildCount = 1 then
      Result := Integer(not IsMin) * DockSiteSizeWithOrient[Orient]
    else
      TopZone.DoGetSplitterLimit(Orient, IsMin, Result);
  end;}
end;

function TCnDockTree.GetDockSiteSizeWithOrient(
  Orient: TDockOrientation): Integer;
begin
  case Orient of
    doVertical: Result := DockSite.Width;
    doHorizontal: Result := DockSite.Height;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

function TCnDockTree.GetMinSize: Integer;
begin
  Result := FMinSize;
end;

procedure TCnDockTree.GetCaptionRect(var Rect: TRect);
begin
  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := 0;
  Rect.Bottom := 0;
end;

procedure TCnDockTree.HideAllControl;

  procedure DoHideAllControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoHideAllControl(AZone.NextSibling);
      DoHideAllControl(AZone.ChildZones);
      if (AZone.ChildControl <> nil) and (AZone.visibled) then
        AZone.Remove(AZone.LimitSize, True);
    end;
  end;

begin
  { 如果ReplacementZone <> nil，就说明是在进行Load操作，不用执行下面的操作，直接退出就可以了 }
  if ReplacementZone <> nil then Exit;
  DoHideAllControl(TopZone.ChildZones);
  SetNewBounds(nil);
  UpdateAll;
  DockSite.Invalidate;
end;

procedure TCnDockTree.HideSingleControl(Control: TControl);

  procedure DoHideSingleControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoHideSingleControl(AZone.NextSibling);
      DoHideSingleControl(AZone.ChildZones);
      if AZone.ChildControl <> nil then
      begin
        if (AZone.ChildControl = Control) then
        begin
          if (AZone.ChildControl.Visible) then
          begin
            AZone.Remove(AZone.LimitSize, True);
            AZone.ChildControl.Visible := False;
          end;
        end else
        begin
          AZone.Insert(AZone.VisibleSize, False);
          AZone.ChildControl.Visible := True;
        end;
      end;
    end;
  end;

begin
  { 如果ReplacementZone <> nil，就说明是在进行Load操作，不用执行下面的操作，直接退出就可以了 }
  if ReplacementZone <> nil then Exit;
  if Control <> nil then
  begin
    DoHideSingleControl(TopZone.ChildZones);
    SetNewBounds(nil);
    UpdateAll;
    DockSite.Invalidate;
  end;
end;

procedure TCnDockTree.ShowAllControl;

  procedure DoShowAllControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoShowAllControl(AZone.NextSibling);
      DoShowAllControl(AZone.ChildZones);
      if (AZone.ChildControl <> nil) and (not AZone.visibled) then
        AZone.Insert(AZone.VisibleSize, True);
    end;
  end;

begin
  { 如果ReplacementZone <> nil，就说明是在进行Load操作，不用执行下面的操作，直接退出就可以了 }
  if ReplacementZone <> nil then Exit;
  DoShowAllControl(TopZone.ChildZones);
  SetNewBounds(nil);
  UpdateAll;
  DockSite.Invalidate;
end;

procedure TCnDockTree.ShowSingleControl(Control: TControl);

  procedure DoShowSingleControl(AZone: TCnDockZone);
  begin
    if AZone <> nil then
    begin
      DoShowSingleControl(AZone.NextSibling);
      DoShowSingleControl(AZone.ChildZones);
      if AZone.ChildControl <> nil then
      begin
        if (AZone.ChildControl = Control) then
        begin
          if (not AZone.ChildControl.Visible) then
          begin
            AZone.Insert(AZone.VisibleSize, False);
            AZone.ChildControl.Visible := True;
          end;
        end else
        begin
          AZone.Remove(AZone.LimitSize, True);
          AZone.ChildControl.Visible := False;
        end;
      end;
    end;
  end;

begin
  { 如果ReplacementZone <> nil，就说明是在进行Load操作，不用执行下面的操作，直接退出就可以了 }
  if ReplacementZone <> nil then Exit;
  if Control <> nil then
  begin
    DoShowSingleControl(TopZone.ChildZones);
    SetNewBounds(nil);
    UpdateAll;
    DockSite.Invalidate;
  end;
end;

procedure TCnDockTree.DrawDockBorder(DockControl: TControl; R1, R2: TRect);
begin

end;

procedure TCnDockTree.DrawDockGrabber(Control: TControl;
  const ARect: TRect);
  procedure DrawCloseButton(Left, Top: Integer);
  var ADockClient: TCnDockClient;
  begin
    ADockClient := FindDockClient(Control);
    if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then Exit;
    DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+GrabberSize-2,
      Top+GrabberSize-2), DFC_CAPTION, DFCS_CAPTIONCLOSE);
  end;

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

begin
  with ARect do
  begin
    case GrabbersPosition of
      gpLeft:
      begin
        DrawCloseButton(Left+BorderWidth+BorderWidth+1, Top+BorderWidth+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+3, Top+GrabberSize+BorderWidth+1, Left+BorderWidth+5, Bottom+BorderWidth-2);
        DrawGrabberLine(Left+BorderWidth+6, Top+GrabberSize+BorderWidth+1, Left+BorderWidth+8, Bottom+BorderWidth-2);
      end;
      gpTop:
      begin
        DrawCloseButton(Right-GrabberSize+BorderWidth+1, Top+BorderWidth+1);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+3, Right-GrabberSize+BorderWidth-2, Top+BorderWidth+5);
        DrawGrabberLine(Left+BorderWidth+2, Top+BorderWidth+BorderWidth+6, Right-GrabberSize+BorderWidth-2, Top+BorderWidth+8);
      end;
//      gpBottom:
//      gpRight:
    end;
  end;
end;

procedure TCnDockTree.DrawSplitterRect(const ARect: TRect);
begin
  Canvas.Brush.Color := TCnWinControlAccess(DockSite).Color;
  Canvas.FillRect(ARect);
end;

procedure TCnDockTree.DrawZone(Zone: TCnDockZone);
begin
  { 依次调用DrawZoneBorder,DrawGrabber和DrawSplitter方法，
    子类可以重载这个方法改变重画的内容 }
  DrawZoneBorder(Zone);
  DrawZoneGrabber(Zone);
  DrawZoneSplitter(Zone);
  DrawDockSiteRect;
end;

procedure TCnDockTree.DrawZoneBorder(Zone: TCnDockZone);
var ChildControl: TControl;
//  R1, R2: TRect;
begin
  if Zone = nil then Exit;
  ChildControl := Zone.ChildControl;
  if (ChildControl <> nil) and ChildControl.Visible and
    (ChildControl.HostDockSite = DockSite) then
  begin
//    R := GetFrameRect(ChildControl);
//    DrawDockGrabber(ChildControl, R);
  end;
end;

procedure TCnDockTree.DrawZoneGrabber(Zone: TCnDockZone);
var ChildControl: TControl;
  R: TRect;
begin
  if Zone = nil then Exit;
  ChildControl := Zone.ChildControl;
  if (ChildControl <> nil) and ChildControl.Visible and
    (ChildControl.HostDockSite = DockSite) then
  begin
    R := GetFrameRect(ChildControl);
    DrawDockGrabber(ChildControl, R);
  end;
end;

procedure TCnDockTree.DrawZoneSplitter(Zone: TCnDockZone);
var R: TRect;
begin
  { 获得分割条的矩形大小 }
  R := GetSpiltterRect(Zone);
  { 画分割条 }
  DrawSplitterRect(R);
end;

procedure TCnDockTree.PaintDockSite;
begin
  ForEachAt(nil, DrawZone, tskBackward);
end;

function TCnDockTree.HasZoneWithControl(Control: TControl): Boolean;
begin
  Result := FindControlZone(Control, True) <> nil;
end;

procedure TCnDockTree.ReplaceZoneChild(OldControl, NewControl: TControl);
var Zone: TCnDockZone;
begin
  Zone := FindControlZone(OldControl, True);
  if Zone <> nil then
  begin
    Zone.ChildControl := TWinControl(NewControl);
    UpdateAll;
  end;
end;

procedure TCnDockTree.DoHideZoneChild(AZone: TCnDockZone);
var AForm: TCustomForm;
begin
  if (AZone <> nil) and (AZone.ChildControl <> nil) then
  begin
    if AZone.ChildControl.InheritsFrom(TCustomForm) then
    begin
      { 当调用Close函数后，AZone将被释放 }
      AForm := TCustomForm(AZone.ChildControl);
      AForm.Close;
    end else
      AZone.ChildControl.Visible := False;
  end;
end;

{ TCnAdvDockZone }

constructor TCnAdvDockZone.Create(Tree: TCnDockTree);
begin
  inherited;
  FCloseBtnDown := False;
  FMouseDown := False;
end;

destructor TCnAdvDockZone.Destroy;
begin
  if Self = TCnAdvDockTree(Tree).CloseBtnZone then
    TCnAdvDockTree(Tree).CloseBtnZone := nil;
  inherited Destroy;
end;

procedure TCnAdvDockZone.Insert(DockSize: Integer; Hide: Boolean);
begin
  InsertOrRemove(DockSize, True, Hide);
end;

procedure TCnAdvDockZone.LButtonDbClkMothed;
begin
  if GlobalDockClient <> nil then
    GlobalDockClient.RestoreChild;
end;

procedure TCnAdvDockZone.Remove(DockSize: Integer; Hide: Boolean);
begin
  InsertOrRemove(DockSize, False, Hide);
end;

{ TCnAdvDockTree }

constructor TCnAdvDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
begin
  inherited Create(DockSite, CnDockZoneClass);
  GrabberSize     := 15;
  FButtonHeight   := 12;
  FButtonWidth    := 12;
  FLeftOffset     := 0;
  FRightOffset    := 0;
  FTopOffset      := 0;
  FBottomOffset   := 0;
  FButtonSplitter := 2;
end;

function TCnAdvDockTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): Boolean;
var TempZone: TCnAdvDockZone;
begin
  Result := inherited DoLButtonDown(Message, Zone, HTFlag);
  if (Zone <> nil) and (HTFlag = HTCLOSE) then
  begin
    TempZone := TCnAdvDockZone(Zone);
    TempZone.CloseBtnDown := True;
    TempZone.MouseDown := True;
    FCloseBtnZone := TempZone;
    DockSite.Invalidate;
  end;
end;

procedure TCnAdvDockTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
//var TempZone: TCnAdvDockZone;
begin
  inherited DoLButtonUp(Message, Zone, HTFlag);
  if SizingZone = nil then
  begin
    FCloseBtnZone := nil;
    if (Zone <> nil) and (HTFlag = HTCLOSE) then
    begin
      // 在这里会有地址偏差，TCnAdvDockZone(Zone).CloseBtnDown
      // 会莫名其妙的指向一个类的指针，需要注意。
//      TempZone := TCnAdvDockZone(Zone);
      TCnAdvDockZone(Zone).CloseBtnDown := False;
    end;
  end;
end;

procedure TCnAdvDockTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var TempZone: TCnAdvDockZone;
begin
  inherited DoMouseMove(Message, Zone, HTFlag);
  if SizingZone = nil then
  begin
    TempZone := TCnAdvDockZone(Zone);
    if ((TempZone <> nil) and (TempZone.CloseBtnDown <> (HTFlag = HTCLOSE))
      and ((FCloseBtnZone = TempZone) and FCloseBtnZone.MouseDown)) then
    begin
      TempZone.CloseBtnDown := (HTFlag = HTCLOSE) and FCloseBtnZone.MouseDown;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnAdvDockTree.InsertSibling(NewZone, SiblingZone: TCnDockZone;
  InsertLast, Update: Boolean);
var
  TempUpdate: Boolean;
begin
  TempUpdate := Update;
  Update := False;
  try
    inherited;
    if NewZone.ChildControl <> nil then
      SetDockHeightWidthArr(0, NewZone.ChildControl.TBDockHeight + BorderWidth,
        NewZone.ChildControl.LRDockWidth + BorderWidth)
    else SetDockHeightWidthArr(0, 0, 0);
  finally
    Update := TempUpdate;
  end;

  if Update then
  begin
    NewZone.Insert(FDropDockSize, False);
    SetNewBounds(NewZone.ParentZone);
    ForEachAt(NewZone.ParentZone, UpdateZone, tskForward);
  end;
end;

procedure TCnAdvDockTree.SetBottomOffset(const Value: Integer);
begin
  FBottomOffset := Value;
end;

procedure TCnAdvDockTree.SetButtonHeight(const Value: Integer);
begin
  FButtonHeight := Value;
end;

procedure TCnAdvDockTree.SetButtonSplitter(const Value: Integer);
begin
  FButtonSplitter := Value;
end;

procedure TCnAdvDockTree.SetButtonWidth(const Value: Integer);
begin
  FButtonWidth := Value;
end;

procedure TCnAdvDockTree.SetLeftOffset(const Value: Integer);
begin
  FLeftOffset := Value;
end;

procedure TCnAdvDockTree.SetRightOffset(const Value: Integer);
begin
  FRightOffset := Value;
end;

procedure TCnAdvDockTree.SetTopOffset(const Value: Integer);
begin
  FTopOffset := Value;
end;

function TCnAdvDockTree.GetDockHeightWidth(
  Orient: TDockOrientation): Integer;
begin
  Result := FDockHeightWidth[Orient];
end;

procedure TCnAdvDockTree.SetDockHeightWidth(Orient: TDockOrientation;
  const Value: Integer);
begin
  FDockHeightWidth[Orient] := Value;
end;

function TCnAdvDockTree.GetDockRectFromArr(Orient: TDockOrientation;
  AtLast: Boolean): Integer;
begin
  Result := FDockRectArr[Orient, Atlast];
end;

procedure TCnAdvDockTree.SetDockRectToArr(Orient: TDockOrientation;
  AtLast: Boolean; const Value: Integer);
begin
  FDockRectArr[Orient, Atlast] := Value;
end;

procedure TCnAdvDockTree.SetDockRectArr(ARect: TRect);
begin
  FDockRectArr[doNoOrient, False] := 0;
  FDockRectArr[doNoOrient, True] := 0;
  FDockRectArr[doHorizontal, False] := ARect.Top;
  FDockRectArr[doHorizontal, True] := ARect.Bottom;
  FDockRectArr[doVertical, False] := ARect.Left;
  FDockRectArr[doVertical, True] := ARect.Right;
end;

procedure TCnAdvDockTree.SetDockHeightWidthArr(NoOrValue, HorValue,
  VerValue: Integer);
begin
  FDockHeightWidth[doNoOrient] := NoOrValue;
  FDockHeightWidth[doHorizontal] := HorValue;
  FDockHeightWidth[doVertical] := VerValue;
end;

procedure TCnAdvDockTree.ScaleChildZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ScaleChildZone(Zone);
end;

procedure TCnAdvDockTree.ScaleSiblingZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ScaleSiblingZone(Zone);
end;

procedure TCnAdvDockTree.ScaleZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ScaleZone(Zone);
end;

procedure TCnAdvDockTree.ShiftZone(Zone: TCnDockZone);
begin
  if Zone = ReplacementZone then
    ShiftScaleOrient := doNoOrient;
  inherited ShiftZone(Zone);
end;

procedure TCnAdvDockTree.InsertNewParent(NewZone, SiblingZone: TCnDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
var
  TempUpdate: Boolean;
begin
  TempUpdate := Update;
  Update := False;
  if NewZone.ChildControl <> nil then
    SetDockHeightWidthArr(0, NewZone.ChildControl.TBDockHeight + BorderWidth,
      NewZone.ChildControl.LRDockWidth + BorderWidth)
  else SetDockHeightWidthArr(0, 0, 0);

  if SiblingZone = nil then
  begin
    if InsertLast then
      ReplacementZone := TopZone
    else ReplacementZone := NewZone;
  end;

  try
    inherited;
  finally
    Update := TempUpdate;
    ReplacementZone := nil;
  end;

  if Update then
  begin
    NewZone.Insert(DropDockSize, False);
    ForEachAt(NewZone.ParentZone, UpdateZone, tskForward);
    SetNewBounds(NewZone.ParentZone);
  end;
end;

procedure TCnAdvDockTree.RemoveZone(Zone: TCnDockZone; Hide: Boolean);
begin
  { 调用默认的RemoveZone函数 }
  inherited;
end;

procedure TCnAdvDockTree.SetDropDockSize(const Value: Integer);
begin
  FDropDockSize := Value;
end;

end.
