{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2010 CnPack 开发组                       }
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
{       停靠服务和客户控件                              }
{       CnDockFormControl 单元                          }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnDockFormControl;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：停靠服务和客户控件单元 
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

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.00
    作者: 周益波
    E-mail: zhouyibo2000@sina.com
    QQ: 51571811
    开始时间 -- 2002-01-14
    结束时间 -- 2002-01-20
    主要功能：
    使窗体具有停靠功能，并且管理停靠窗体。
    如何使用：
    在您的主窗体上放一个TCnDockServer，在一般的工具窗体上放一个TCnDockClient
    控件，这样，工具窗体就可以停靠到主窗体上了，可以停靠到主窗体的上下左右
    四个方向上。同时，两个工具窗体还有相互停靠的功能，它们可以停靠成平铺型和
    分页型的窗体。

    CnDockFormControl单元还提供了一些函数和过程来操作停靠窗体，它们的定义见下面的
    注释。

    在下面的代码中，还定义了两个类的引用，这样，用户就可以用自定义的类替换掉原先
    的类，改变她们的默认特性。

    BUG--
    1.在进行停靠操作时，停靠窗体的第一个Windows标准控件里面的内容有可能被清空，

    2.停靠窗体的Caption有可能被清空。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.01
    结束时间 -- 2002-02-09
    增加了保存停靠信息和还原停靠信息的功能，
    用户不但可以把停靠信息存储在INI文件中，还可以保存在注册表中。
    因为在还原停靠信息的时候把桌面锁住了，所以不会产生闪烁现象。

    增加了对停靠选项的处理函数
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.02
    结束时间 -- 2002-02-20
    增加了辅助控件TCnBasicDockStyle，用来改变停靠的风格，如：
    停靠的预览效果，停靠后把手的外观等。
    而且用户可以继承TCnBasicDockStyle，用来自定义外观，提供最大的灵活性
    用户可以分别继承TCnDockSplitter，TCnDockPanel，TCnConjoinPanel，
    TCnTabPageControl，TCnDockZone和TCnDockTree类来改变停靠风格。
    目前增加的停靠风格类是CnDelphiDockStyle和CnVCDockStyle，
    她们分别表示类似Delphi和类似VC++的风格。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.03
    结束时间 -- 2002-03-10
    修正了0.00版中的两个BUG，程序运行的更加稳定、可靠了。
    增加了TCnVIDDockStyle类，她是模仿Visual InderDev的停靠的风格，还没有完全完成，
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.031
    结束时间 -- 2002-03-17
    完成了一个很有难度的功能，就是当停靠控件是TCnConjoinDockHostForm的时候，
    就把它上面的TCnConjoinPanel的停靠信息保存起来，然后再把它还原到DropOnControl中。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.04
    结束时间 -- 2002-04-17
    对程序的底层代码进行了大幅度的修改，现在，这套控件已经不再受Delphi的VCL类库的
    限制了，我们完全可以根据自己的意愿来设置停靠的风格了。
    其中增加了TCnDockPresident类，这个类用于当鼠标拖动控件的时候处理停靠事件。
    又增加了TCnCustomDockControl类，这个类从TCustomControl继承过来，在她里面定义
    了一些和停靠相关的函数和方法。后面的TCnDockPanel, TCnConjoinPanel和
    TCnTabPageControl都从TCnCustomDockControl继承。这样，只要把她们三个类中相同的
    代码都放在TCnCustomDockControl中，就起到了代码重用的作用；还有，当检测这个类
    的级别的时候也很方便。由于代码大幅度的修改，暂时停靠风格类还不能完全使用，
    不过应该马上可以修改好的。关于停靠风格，以后还要增加：
    TCnWPSDockStyle(类似WPS的停靠风格)，
    TCnPhotoShopDockStyle(类似PhotoShop的停靠风格),
    TCnTancentDockStyle(类似腾讯浏览器的停靠风格),
    TCnOfficeDockStyle(类似Office XP的停靠风格),
    TCnNETDockStyle(类似.NET的停靠风格),
    TCnVBDockStyle(类似Visual Basic的停靠风格)，
    TCnCustomDockStyle(自定义的停靠风格)。
    ...
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.041
    结束时间 -- 2002-04-24
    增加了一些TCnDockClient中的属性，如：
    DirectDrag(当鼠标点击标题栏的时候是否立刻进行停靠操作)，
    NCPopupMenu(当鼠标右键点击标题栏的时候是否产生弹出菜单)，
    ShowHint(当鼠标移动到标题栏上的时候是否显示提示信息)。
    又增加了一些TCnDockClient的事件，如：
    OnFormShowHint(当提示信息显示的时候触发)
    OnNCButtonDown(当鼠标在标题栏上按下的时候触发)
    OnNCButtonUp(当鼠标在标题栏上释放的时候触发)
    OnNCMouseMove(当鼠标在标题栏上移动的时候触发)
    OnNCButtonDblClk(当鼠标在标题栏上双击的时候触发)
    OnPaintDockGrabber(当重画把手的时候触发)
    OnPaintDockSplitter(当重画分割条的时候触发)

    对TCnVCDockStyle类进行了一些改进，使它看上去更象VC的停靠风格，不过有些地方
    还有待改进，这只是时间问题。

    现在这套控件看上去功能越来越强大了，嘻嘻，不过我还不急着发布她，希望把她做地完美
    无缺后再发布。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.041
    结束时间 -- 2002-05-10
    对TCnVCDockStyle类进行了进一步的改进，现在她的特性已经很类似于VC++的停靠风格了。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.042
    结束时间 -- 2002-05-24
    做了一个类似Visual C++界面的程序,用来演示TCnVCDockStyle控件,做的很酷哦.
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.050
    结束时间 -- 2002-06-13
    初步完成了Visual InterDev中的停靠分页的界面。而且有一个副产物TCnVIDTabPageControl
    控件，可以作为一般的分页控件来使用。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.051
    结束时间 -- 2002-07-03
    基本上完成了Visual InterDev中的停靠分页的界面。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.060
    结束时间 -- 2002-08-11
    已经完成Visual InterDev中的停靠分页的界面，在TCnBasicDockStyle类中增加了两个
    属性：TCnBasicConjoinServerOption和TCnBasicTabServerOption，用户可以继承这
    两个类来实现自己的停靠风格的属性。
    并且制作了一个帮助文件，接下去就开始测试了，准备在十月份发布这套控件。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.065
    结束时间 -- 2002-08-20
    找到了一些BUG,并且已经改正,继续测试,可靠性是软件的根本。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.070
    结束时间 -- 2002-09-14
    增加了一个很重要的功能，就是当用户关闭一个已经停靠了的窗体后，可以把这个窗体
    在停靠服务器中的原来的位置保存下来，用户再次显示它的时候，可以还原到原来的位置
    和大小，这个功能在Visual C++和Visual InterDev中都有，可是在Delphi中没有，
    现在我把这个功能也加到CnDelphiDockStyle中了。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.100
    结束时间 -- 2002-11-20
    程序的试用版已经发布了，反响还不错，用户发现了一些Bug，现在已经修改了。
    后来又增加了一个类似Visual InterDev的Demo，本来想增加一个双击标题栏还原的
    功能，结果改啊改，但是最后发现改不下去了，为什么？说出来丢脸，由于刚开始做
    这个控件的时候没有想到会做成象现在功能这么强大，程序结构没有设计好，到后来
    发觉再改下去的话，程序就会发展到不可控制的混乱程度。最后，只能忍痛割爱，暂时
    把双击还原的功能去掉，然后初步决定等到做完了类似VS.NET的停靠风格后就发布正式
    版，所以这几天正忙着做类似VS.NET的停靠风格TCnVSNETDockStyle呢。大概能到12月中
    能做完，然后还要测试，恐怕只能等到2003年1月份才能发布了。

    等正式版发布的时候，我还要把帮助文件翻译成English，让老外也来用我们中国人的
    控件，但是我还不知道是作为免费软件还是共享软件发布，到时候再说吧。

    本人初步打算把DockPresident移植到C#上面，不过不知道能在什么时候开始，因为我
    对C#也是一知半解，前一阵子学了一下，现在又放下了，唉，没时间啊。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.0110
    结束时间 -- 2002-12-14
    初步完成了类似VS.NET停靠风格的类TCnVSNETDockStyle,功能基本上和VS.NET停靠风格
    差不多，不过由于Delphi的特殊性，有些地方没有办法和VS.NET停靠风格完全一样，如果
    读者想了解VS.NET的停靠风格，请参看《VS.NET的停靠风格》。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.0112
    结束时间 -- 2002-12-20
    做了一个类似MSDN2002停靠风格的Demo，效果还不错，呵呵。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.0120
    结束时间 -- 2003-1-4
    新版本的控件已经基本上完成,现在进入测试阶段,预计春节后发布。接下去的时间主要
    是作测试,并且修改一些细节的东东,完善帮助,补全文档等等。
    程序的代码量已经突破25000行，如果去掉注释部分应该也有20000多行，也算是中等规模
    的控件了，呵呵，足足花了我一年的业余时间，中途也曾经打过退堂鼓，但还是咬牙坚持
    了下来，不管这个控件以后的命运会怎么样，能够完成她其实已经是一种胜利了，呵呵。
    接下去的任务就是要把她养大，花点时间来推广她，如果哪位朋友觉得Dock President
    还不错，就请向您的朋友推荐一下，在这里鲁小班先向您表示感谢了。
+------------------------------------------------------------------------------}

{------------------------------------------------------------------------------+
    CnDockFormControl -- 版本 0.0121
    结束时间 -- 2003-2-16
    改正了一个问题，那就是在WinXP下面装载停靠信息的时候，屏幕会闪烁，其实这个
    问题在98或者2000以下是没有的，因为程序已经调用了LockWindowUpdate函数，
    但是在XP下面不知道是什么原因，还是会出现这种闪烁的情况，后来没办法，只能在
    装载的时候过滤掉CM_SHOWINGCHANGED消息，然后在装载的最后把应该显示但是没有
    显示的客户窗体显示出来。
+------------------------------------------------------------------------------}

{$P+,S-,W-,R-,T-,H+,X+}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Extctrls, Comctrls, Stdctrls, Consts, Inifiles, CommCtrl, Registry, CnDockTree,
  CnDockSupportClass, CnDockSupportControl, CnClasses, CnConsts, CnCompConsts;

const
  { 停靠的状态 }
  DS_Unknow   = 0;  // 不知道是什么状态
  DS_Docking  = 1;  // 停靠状态
  DS_Floating = 2;  // 浮动状态

type
  TCnSplitterSize = 0..32767;

  TCnDockBaseControl = class;
  TCnDockServer = class;
  TCnDockClient = class;
  TCnConjoinPanel = class;
  TCnTabPageControl = class;
  TCnConjoinDockHostForm = class;
  TCnTabDockHostForm = class;

  { 用来在停靠服务端中调整客户端的分割条 }
  TCnDockSplitter = class({TSplitter)//}TCustomDockPanelSplitter)
  private
    { 表示被哪个TDockServer拥有 }
    FDockServer: TCnDockServer;
    function GetSplitterIndex: Integer;
  protected
    function FindControl: TControl; override;
    property DockServer: TCnDockServer read FDockServer write FDockServer;
  public
    constructor Create(AOwner: TComponent); override;
    property SplitterIndex: Integer read GetSplitterIndex;
  end;

  // 当调用ShowDockPanel函数设置DockPanel的宽度或者高度的时候，
  // 用来指定到底是按照DockPanel本身的TBDockHeight和LRDockWidth
  // 还是按照Client的TBDockHeight和LRDockWidth来设置。
  TSetDockPanelSizeFrom = (sdfDockPanel, sdfClient);

  { 停靠服务端中用来停靠客户端的Panel }
  TCnDockPanel = class(TCnCustomDockPanel)
  private
    { 表示被哪个TDockServer拥有 }
    FDockServer: TCnDockServer;
    function GetPanelIndex: Integer;
  protected
    procedure SetDockServer(const Value: TCnDockServer); virtual;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    { ------------------------------------------------------------------------ }
    procedure CustomStartDock(var Source: TCnDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    { 显示停靠的窗体 }
    procedure ShowDockPanel(MakeVisible: Boolean; Client: TControl;
      PanelSizeFrom: TSetDockPanelSizeFrom = sdfClient); virtual;
    procedure ResetPosition;
    property PanelIndex: Integer read GetPanelIndex;
    property DockServer: TCnDockServer read FDockServer write SetDockServer;
  end;

  TCnAdvDockPanel = class(TCnDockPanel)
  private
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TCnDockPanelClass = class of TCnDockPanel;
  TCnDockSplitterClass = class of TCnDockSplitter;
  TCnConjoinPanelClass = class of TCnConjoinPanel;
  TCnTabDockClass = class of TCnTabPageControl;

  { 分割条的风格 }
  TCnSplitterStyle = class(TPersistent)
  private
    FSplitter: TCnDockSplitter;
    FDockServer: TCnDockServer;
    FColor: TColor;
    FCursor: TCursor;
    FParentColor: Boolean;
    FResizeStyle: TResizeStyle;
    FSize: TCnSplitterSize;          // 大小，当是[左，右]时就是宽度，当是[上，下]时就是高度。
    FMinSize: TCnSplitterSize;       // 距离父控件最小的位置
    procedure SetColor(const Value: TColor);
    procedure SetCursor(const Value: TCursor);
    procedure SetParentColor(const Value: Boolean);
    procedure SetResizeStyle(const Value: TResizeStyle);
    procedure SetSize(const Value: TCnSplitterSize);
    procedure SetMinSize(const Value: TCnSplitterSize);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToSplitter(Dest: TCnDockSplitter);
    procedure SetSplitterStyle;
    property Splitter: TCnDockSplitter read FSplitter write FSplitter;
  public
    constructor Create(ASplitter: TCnDockSplitter; ACursor: TCursor); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Cursor: TCursor read FCursor write SetCursor;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ResizeStyle: TResizeStyle read FResizeStyle write SetResizeStyle
      default rsPattern;
    property Size: TCnSplitterSize read FSize write SetSize default 3;
    property MinSize: TCnSplitterSize read FMinSize write SetMinSize default 30;
  end;

  TCnBasicDockStyle = class;

  { TCnBasicConjoinServerOption和TCnBasicTabServerOption共同的父类 }
  TCnBasicServerOption = class(TPersistent)
  private
    FDockStyle: TCnBasicDockStyle;
  protected
    { 重新设置和DockStyle有联系的TCnDockBaseControl的配置 }
    procedure ResetDockControlOption; virtual; abstract;
    { 重新设置ADockServer的配置 }
    procedure ResetDockServerOption(ADockServer: TCnDockServer); virtual;//  abstract;
    { 重新设置ADockClient的配置 }
    procedure ResetDockClientOption(ADockClient: TCnDockClient); virtual;//  abstract;
    property DockStyle: TCnBasicDockStyle read FDockStyle write FDockStyle;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TGrabbersSize = 1..MaxInt;
  TSplitterWidth = 1..MaxInt;

  { 平铺服务器的选项类 }
  TCnBasicConjoinServerOption = class(TCnBasicServerOption)
  private
    FGrabbersSize: TGrabbersSize;
    FSplitterWidth: TSplitterWidth;
  protected
    procedure SetGrabbersSize(const Value: TGrabbersSize); virtual;
    procedure SetSplitterWidth(const Value: TSplitterWidth); virtual;
    { ------------------------------------------------------------------------ }
    procedure ResetDockControlOption; override;
    procedure ResetDockServerOption(ADockServer: TCnDockServer); override;
    procedure ResetDockClientOption(ADockClient: TCnDockClient); override;
    { 重新设置TCnDockPanel的属性 }
    procedure ResetDockPanel(APanel: TCnDockPanel); virtual;
    { 重新设置TCnConjoinPanel的属性 }
    procedure ResetConjoinPanel(APanel: TCnConjoinPanel); virtual;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetGrabbersSize_WithoutChangeSystemInfo(const Value: TGrabbersSize);
    procedure SetSplitterWidth_WithoutChangeSystemInfo(const Value: TSplitterWidth);
  published
    property GrabbersSize: TGrabbersSize read FGrabbersSize write SetGrabbersSize;
    property SplitterWidth: TSplitterWidth read FSplitterWidth write SetSplitterWidth;
  end;

  { 分页服务器的选项类 }
  TCnBasicTabServerOption = class(TCnBasicServerOption)
  private
    FTabPosition: TTabPosition;
    FHotTrack: Boolean;
  protected
    procedure SetTabPosition(const Value: TTabPosition); virtual;
    procedure SetHotTrack(const Value: Boolean); virtual;
    { ------------------------------------------------------------------------ }
    procedure ResetDockControlOption; override;
    procedure ResetDockServerOption(ADockServer: TCnDockServer); override;
    procedure ResetDockClientOption(ADockClient: TCnDockClient); override;
    { 重新设置TCnTabPageControl的属性 }
    procedure ResetTabPageControl(APage: TCnTabPageControl); virtual;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
      default tpTop;
  end;

  TCnBasicConjoinServerOptionClass = class of TCnBasicConjoinServerOption;
  TCnBasicTabServerOptionClass = class of TCnBasicTabServerOption;

  { 所有停靠风格的基类 }
  TCnBasicDockStyle = class(TCnComponent)
  private
    FCnDockPanelClass: TCnDockPanelClass;      // TCnDockServer中四个TCnDockPanel的类的引用
    FCnDockSplitterClass: TCnDockSplitterClass;// TCnDockServer中四个TSplitter的类的引用
    FCnConjoinPanelClass: TCnConjoinPanelClass;// 平铺服务器的类的引用
    FCnTabDockClass: TCnTabDockClass;          // 分页服务器的类的引用
    FCnDockPanelTreeClass: TCnDockTreeClass;   // TCnDockPanel中IDockManager的类的引用
    FCnDockPanelZoneClass: TCnDockZoneClass;   // TCnDockPanel中IDockManager的节点类的引用
    FCnConjoinPanelTreeClass: TCnDockTreeClass;// TCnConjoinPanel中IDockManager的类的引用
    FCnConjoinPanelZoneClass: TCnDockZoneClass;
    FCnConjoinServerOptionClass: TCnBasicConjoinServerOptionClass;
    FCnTabServerOptionClass: TCnBasicTabServerOptionClass;

    FCnConjoinServerOption: TCnBasicConjoinServerOption;
    FCnTabServerOption: TCnBasicTabServerOption;

    FParentForm: TForm;         // 父窗体
    FOldWindowProc: TWndMethod;

    { 一个列表，用来存储所有引用了TCnBasicDockStyle对象的TCnDockBaseControl对象 }
    FDockBaseControlList: TList;

    function GetDockBaseControlListCount: Integer;
    function GetDockBaseControlLists(Index: Integer): TCnDockBaseControl;

  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure FormStartDock(DockClient: TCnDockClient; var Source: TCnDragDockObject); virtual;
    procedure FormGetSiteInfo(Source: TCnDragDockObject; DockClient: TCnDockClient; Client: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); virtual;
    procedure FormDockOver(DockClient: TCnDockClient; Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormPositionDockRect(DockClient: TCnDockClient; Source: TCnDragDockObject); virtual;
    procedure FormDockDrop(DockClient: TCnDockClient; Source: TCnDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(DockClient: TCnDockClient; Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(DockClient: TCnDockClient; NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetDockEdge(DockClient: TCnDockClient; Source: TCnDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); virtual;
    { ------------------------------------------------------------------------ }
    { 创建平铺停靠分割的选项类 }
    procedure CreateConjoinServerOption(var Option: TCnBasicConjoinServerOption); virtual;
    { 创建分页停靠分割的选项类 }
    procedure CreateTabServerOption(var Option: TCnBasicTabServerOption); virtual;
    { 创建平铺和分页停靠分割的选项类 }
    procedure CreateServerOption; virtual;
    { 释放平铺和分页停靠分割的选项类 }
    procedure FreeServerOption; virtual;
    { ------------------------------------------------------------------------ }

    procedure SetDockBaseControl(IsCreate: Boolean;
      DockBaseControl: TCnDockBaseControl); virtual;

    { 捕获TCnDockServer的WindowProc消息，如果要还要执行默认的消息处理就返回False,否则就返回True }
    function DockServerWindowProc(DockServer: TCnDockServer; var Message: TMessage): Boolean; virtual;
    { 捕获TCnDockClient的WindowProc消息，如果要还要执行默认的消息处理就返回False,否则就返回True }
    function DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean; virtual;

    { 捕获父控件的WindowProc消息 }
    procedure ParentFormWindowProc(var Message: TMessage); virtual;

    { 把ADockBaseControl添加到FDockBaseControlList中，
      如果已经存在了就不插入，反之插入到列表的结尾处 }
    procedure AddDockBaseControl(ADockBaseControl: TCnDockBaseControl); virtual;
    { 把ADockBaseControl从FDockBaseControlList中删除，
      如果不存在了就不删除，反之删除 }
    procedure RemoveDockBaseControl(ADockBaseControl: TCnDockBaseControl); virtual;
    { ------------------------------------------------------------------------ }
    procedure SetConjoinServerOption(
      const Value: TCnBasicConjoinServerOption); virtual;
    procedure SetTabServerOption(const Value: TCnBasicTabServerOption); virtual;
    function GetConjoinServerOption: TCnBasicConjoinServerOption; virtual;
    function GetTabServerOption: TCnBasicTabServerOption; virtual;
    { 给当前的APanel的平铺的风格赋值,子类可以重载这个方法添加新的属性 }
    procedure AssignConjoinServerOption(APanel: TCnCustomDockPanel); virtual;
    { 给当前的APage的分页的风格赋值,子类可以重载这个方法添加新的属性 }
    procedure AssignTabServerOption(APage: TCnTabPageControl); virtual;
    procedure Loaded; override;
    property DockBaseControlList: TList read FDockBaseControlList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function CanSetEnableDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetLeftDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetRightDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetTopDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetBottomDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function CanSetEachOtherDocked(ADockBaseControl: TCnDockBaseControl): Boolean; virtual;
    function GetControlName: string; virtual;
    function GetDockStyleVersion: string; virtual;
    { 重新设置光标的形状 }
    procedure ResetCursor(Source: TCnDragDockObject); virtual;
    { 获得DockClient参数的停靠状态 }
    function GetDockState(DockClient: TCnDockClient): Integer; virtual;
    procedure ResetDockControlConjoinOption;
    procedure ResetDockControlTabOption;
    { ------------------------------------------------------------------------ }
    procedure ShowDockForm(ADockClient: TCnDockClient); virtual;// 显示ADockClient中的ParentForm;
    procedure HideDockForm(ADockClient: TCnDockClient); virtual;// 隐藏ADockClient中的ParentForm;
    function GetDockFormVisible(ADockClient: TCnDockClient): Boolean; virtual;// 得到ADockClient中的ParentForm是否可见.
    { ------------------------------------------------------------------------ }
    property DockBaseControlListCount: Integer read GetDockBaseControlListCount;
    property DockBaseControlLists[Index: Integer]: TCnDockBaseControl read GetDockBaseControlLists;
    { 还原原先的客户的状态 }
    procedure RestoreClient(DockClient: TCnDockClient); virtual;
    { ------------------------------------------------------------------------ }
    property Version: string read GetDockStyleVersion;// 版本号
    property ControlName: string read GetControlName; // 控件名称
    { ------------------------------------------------------------------------ }
    property CnDockPanelClass: TCnDockPanelClass
      read FCnDockPanelClass write FCnDockPanelClass;
    property CnDockSplitterClass: TCnDockSplitterClass
      read FCnDockSplitterClass write FCnDockSplitterClass;
    property CnConjoinPanelClass: TCnConjoinPanelClass
      read FCnConjoinPanelClass write FCnConjoinPanelClass;
    property CnTabDockClass: TCnTabDockClass
      read FCnTabDockClass write FCnTabDockClass;
    property CnDockPanelTreeClass: TCnDockTreeClass
      read FCnDockPanelTreeClass write FCnDockPanelTreeClass;
    property CnDockPanelZoneClass: TCnDockZoneClass
      read FCnDockPanelZoneClass write FCnDockPanelZoneClass;
    property CnConjoinPanelTreeClass: TCnDockTreeClass
      read FCnConjoinPanelTreeClass write FCnConjoinPanelTreeClass;
    property CnConjoinPanelZoneClass: TCnDockZoneClass
      read FCnConjoinPanelZoneClass write FCnConjoinPanelZoneClass;
    { ------------------------------------------------------------------------ }
    property CnConjoinServerOptionClass: TCnBasicConjoinServerOptionClass
      read FCnConjoinServerOptionClass write FCnConjoinServerOptionClass;
    property CnTabServerOptionClass: TCnBasicTabServerOptionClass
      read FCnTabServerOptionClass write FCnTabServerOptionClass;
    { ------------------------------------------------------------------------ }
    property ParentForm: TForm read FParentForm write FParentForm;
    { ------------------------------------------------------------------------ }
    property ConjoinServerOption: TCnBasicConjoinServerOption
      read GetConjoinServerOption write SetConjoinServerOption;
    property TabServerOption: TCnBasicTabServerOption
      read GetTabServerOption write SetTabServerOption;
  end;

  TCnAdvDockStyle = class(TCnBasicDockStyle)
  protected
    function DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean; override;
  end;

  { 停靠服务端和客户端共同的父类 }
  TCnDockBaseControl = class(TCnComponent)
  private
    FEnableDock,            // 是否可以停靠在服务端
    FLeftDock,              // 是否可以停靠在服务端的左边
    FTopDock,               // 是否可以停靠在服务端的上边
    FRightDock,             // 是否可以停靠在服务端的右边
    FBottomDock,            // 是否可以停靠在服务端的下边
    FEachOtherDock: Boolean;// 是否可以相互停靠
    FDockStyle: TCnBasicDockStyle;

    FOldOnClose: TCloseEvent;
    FOldOnCreate: TNotifyEvent;

    FParentForm: TForm;

    FOldWindowProc: TWndMethod;

  protected
    procedure Loaded; override;
    procedure SetParentComponent(Value: TComponent); override;
    { ------------------------------------------------------------------------ }
    function CanSetEnableDocked: Boolean; virtual;
    function CanSetLeftDocked: Boolean; virtual;
    function CanSetRightDocked: Boolean; virtual;
    function CanSetTopDocked: Boolean; virtual;
    function CanSetBottomDocked: Boolean; virtual;
    function CanSetEachOtherDocked: Boolean; virtual;
    { ------------------------------------------------------------------------ }
    procedure DoFormOnClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure DoFormOnCreate(Sender: TObject); virtual;
    { ------------------------------------------------------------------------ }
    procedure SetBottomDock(const Value: Boolean); virtual;
    procedure SetEachotherDock(const Value: Boolean); virtual;
    procedure SetEnableDock(const Value: Boolean); virtual;
    procedure SetLeftDock(const Value: Boolean); virtual;
    procedure SetRightDock(const Value: Boolean); virtual;
    procedure SetTopDock(const Value: Boolean); virtual;
    procedure SetDockStyle(const Value: TCnBasicDockStyle); virtual;
    { ------------------------------------------------------------------------ }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure WindowProc(var Message: TMessage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ParentForm: TForm read FParentForm;
    { ------------------------------------------------------------------------ }
//    class function GetDockStyleVersion: string; virtual;
    function GetDockStyleVersion: string; virtual;
    { ------------------------------------------------------------------------ }
    property EnableDock: Boolean read FEnableDock write SetEnableDock default True;
    property LeftDock: Boolean read FLeftDock write SetLeftDock default True;
    property TopDock: Boolean read FTopDock write SetTopDock default True;
    property RightDock: Boolean read FRightDock write SetRightDock default True;
    property BottomDock: Boolean read FBottomDock write SetBottomDock default True;
    property EachOtherDock: Boolean read FEachotherDock write SetEachotherDock default True;
    { ------------------------------------------------------------------------ }
    property Version: string read GetDockStyleVersion; // 版本号
    { ------------------------------------------------------------------------ }
    property DockStyle: TCnBasicDockStyle read FDockStyle write SetDockStyle;
  end;

  TGetClientAlignSizeEvent = procedure(Align: TAlign; var Value: Integer) of object;
  TFinishSetDockPanelSizeEvent = procedure(DockPanel: TCnDockPanel) of object;

  { 停靠服务端 }
  TCnDockServer = class(TCnDockBaseControl)
  private
    FDockPanelClass: TCnDockPanelClass;
    FLeftDockPanel,                         // 左边的停靠面板
    FTopDockPanel,                          // 上边的停靠面板
    FBottomDockPanel,                       // 下边的停靠面板
    FRightDockPanel: TCnDockPanel;          // 右边的停靠面板

    FDockSplitterClass: TCnDockSplitterClass;
    FLeftSplitter,                          // 左边的分割条
    FTopSplitter,                           // 上边的分割条
    FBottomSplitter,                        // 下边的分割条
    FRightSplitter: TCnDockSplitter;        // 右边的分割条

    FLeftSplitterStyle,                     // 左边的分割条风格
    FTopSplitterStyle,                      // 上边的分割条风格
    FRightSplitterStyle,                    // 下边的分割条风格
    FBottomSplitterStyle: TCnSplitterStyle; // 右边的分割条风格

    FOnGetClientAlignSize: TGetClientAlignSizeEvent;
    FOnFinishSetDockPanelSize: TFinishSetDockPanelSizeEvent;

    procedure CreateDockPanelAndSplitter; // 创建四个停靠面板和分割条
//    procedure DestroyDockPanelAndSplitter;// 释放四个停靠面板和分割条

    procedure CreateSplitterStyle;  // 创建风格条的风格
    procedure DestroySplitterStyle; // 释放风格条的风格

    { 设置分割条的风格 }
    procedure SetSplitterStyle;     // 设置风格条的风格
    { 设置左边的风格条的风格 }
    procedure SetLeftSplitterStyle(const Value: TCnSplitterStyle);
    { 设置上边的风格条的风格 }
    procedure SetTopSplitterStyle(const Value: TCnSplitterStyle);
    { 设置右边的风格条的风格 }
    procedure SetRightSplitterStyle(const Value: TCnSplitterStyle);
    { 设置下边的风格条的风格 }
    procedure SetBottomSplitterStyle(const Value: TCnSplitterStyle);
    { 获得索引为Index的TCnDockPanel }
    function GetDockPanel(Index: Integer): TCnDockPanel;
    { 获得索引为Index的TCnDockSplitter }
    function GetDockSplitter(Index: Integer): TCnDockSplitter;
    procedure DoGetClientAlignControl(Align: TAlign; var Value: Integer);
    function GetDockPanelWithAlign(Index: TAlign): TCnDockPanel;
    function GetDockSplitterWithAlign(Index: TAlign): TCnDockSplitter;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
    { ------------------------------------------------------------------------ }
    procedure DoFinishSetDockPanelSize(DockPanel: TCnDockPanel);
    procedure DoFloatDockClients(DockPanel: TCnDockPanel);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;
    { ------------------------------------------------------------------------ }
   procedure WMActivate(var Message: TWMActivate);
   procedure WindowProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetClientAlignControl(Align: TAlign): Integer;

    property LeftDockPanel: TCnDockPanel read FLeftDockPanel;
    property RightDockPanel: TCnDockPanel read FRightDockPanel;
    property TopDockPanel: TCnDockPanel read FTopDockPanel;
    property BottomDockPanel: TCnDockPanel read FBottomDockPanel;

    property LeftSplitter: TCnDockSplitter read FLeftSplitter;
    property RightSplitter: TCnDockSplitter read FRightSplitter;
    property TopSplitter: TCnDockSplitter read FTopSplitter;
    property BottomSplitter: TCnDockSplitter read FBottomSplitter;

    property DockPanel[Index: Integer]: TCnDockPanel read GetDockPanel;
    property DockPanelWithAlign[Index: TAlign]: TCnDockPanel read GetDockPanelWithAlign;
    property DockSplitter[Index: Integer]: TCnDockSplitter read GetDockSplitter;
    property DockSplitterWithAlign[Index: TAlign]: TCnDockSplitter read GetDockSplitterWithAlign;
    property Version: string read GetDockStyleVersion; // 版本号
  published
    property LeftSplitterStyle: TCnSplitterStyle read FLeftSplitterStyle write SetLeftSplitterStyle;
    property TopSplitterStyle: TCnSplitterStyle read FTopSplitterStyle write SetTopSplitterStyle;
    property RightSplitterStyle: TCnSplitterStyle read FRightSplitterStyle write SetRightSplitterStyle;
    property BottomSplitterStyle: TCnSplitterStyle read FBottomSplitterStyle write SetBottomSplitterStyle;

    property OnGetClientAlignSize: TGetClientAlignSizeEvent
      read FOnGetClientAlignSize write FOnGetClientAlignSize;
    property OnFinishSetDockPanelSize: TFinishSetDockPanelSizeEvent
      read FOnFinishSetDockPanelSize write FOnFinishSetDockPanelSize;

    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property DockStyle;
  end;

  { 鼠标的位置，分别是'在浮动窗体上'，'在平铺服务器上'，'在分页服务器上' }
  TMouseStation = (msFloat, msConjoin, msTabPage);

  TNCButtonEvent = procedure(DockClient: TCnDockClient; Button: TMouseButton;
    X, Y: Smallint; HitTest: Longint; MouseStation: TMouseStation) of object;

  TNCButtonDownEvent = TNCButtonEvent;

  TNCButtonUpEvent = TNCButtonEvent;

  TNCButtonDblClkEvent = TNCButtonEvent;

  TNCMouseMoveEvent = procedure(DockClient: TCnDockClient;
    X, Y: Smallint; HitTest: Longint; MouseStation: TMouseStation) of object;

  TPaintDockEvent = procedure(Canvas: TCanvas;
    Control: TControl; const ARect: TRect) of object;

  TPaintDockGrabberEvent = TPaintDockEvent;

  TPaintDockSplitterEvent = TPaintDockEvent;

  TFormHintEvent = procedure(HTFlag: Integer; var HintStr: string; var CanShow: Boolean) of object;

  { 停靠客户端 }
  TCnDockClient = class(TCnDockBaseControl)
  private
    FConjoinPanelClass: TCnConjoinPanelClass;
    FTabDockClass: TCnTabDockClass;
    FParentVisible: Boolean;    // 父窗口是否是可见的。
    FNCPopupMenu: TPopupMenu;   // 当在窗体的标题栏上右击鼠标右键时，弹出的菜单。
    FDirectDrag: Boolean;       // 当鼠标按下的时候是否立即进行停靠操作。
    FShowHint: Boolean;         // 是否显示提示信息

    FCanFloat: Boolean;             // 是否可以浮动,这个功能是应一个用户的要求添加的,
                                    // 为了防止程序的界面过于凌乱,有时候用户并不希望
                                    // 停靠的窗体浮动出来,就用这个属性来限制,如果是True,
                                    // 就允许浮动,否者就不允许,只能停靠到服务器上,
                                    // 当然在服务器之间是可以相互拖动的,默认为True.
    FRelativeServer: TCnDockServer; // 有关系的TCnDockServer的实例,这个功能也是应一个
                                    // 用户的要求添加的,当一个客户窗体打算停靠到服务器
                                    // 上面的时候,需要判断FRelativeServer和当前打算停靠
                                    // 的服务器是否是同一个,如果不是,就不能停靠,否者就
                                    // 能够停靠,默认值为nil,当这个属性是nil的时候,就说明
                                    // 可以停靠到所有的服务器上面.
    FDockLevel: Integer;            // 停靠的级别,只有相同级别的客户窗体才可以相互停靠

    FEnableCloseBtn: Boolean;       // 是否允许关闭按钮可用.

    FOnNCButtonDown:      TNCButtonDownEvent;
    FOnNCButtonUp:        TNCButtonUpEvent;
    FOnNCMouseMove:       TNCMouseMoveEvent;
    FOnNCButtonDblClk:    TNCButtonDblClkEvent;

    FOnPaintDockGrabber:  TPaintDockGrabberEvent;
    FOnPaintDockSplitter: TPaintDockSplitterEvent;

    FOnFormShowHint: TFormHintEvent;

    FOnFormShow: TNotifyEvent;
    FOnFormHide: TNotifyEvent;

    FCurrentDockSite,
    FLastDockSite:  TWinControl;// 上一次停靠的服务器
    FUnDockLeft,                // 浮动时候的左边的位置
    FUnDockTop: Integer;        // 浮动时候的右边的位置

    FVSPaneWidth: Integer;      // 面板的宽度

    procedure SetParentVisible(const Value: Boolean);
    function GetLRDockWidth: Integer;
    function GetTBDockHeight: Integer;
    procedure SetLRDockWidth(const Value: Integer);
    procedure SetTBDockHeight(const Value: Integer);
    procedure SetNCPopupMenu(const Value: TPopupMenu);
    { ------------------------------------------------------------------------ }
    procedure WMNCLButtonDown(var Message: TWMNCHitMessage);
    procedure WMNCLButtonUp(var Message: TWMNCHitMessage);
    procedure WMNCLButtonDblClk(var Message: TWMNCHitMessage);
    procedure WMNCMButtonDown(var Message: TWMNCHitMessage);
    procedure WMNCMButtonUp(var Message: TWMNCHitMessage);
    procedure WMNCMButtonDblClk(var Message: TWMNCHitMessage);
    procedure WMNCRButtonDown(var Message: TWMNCHitMessage);
    procedure WMNCRButtonUp(var Message: TWMNCHitMessage);
    procedure WMNCRButtonDblClk(var Message: TWMNCHitMessage);
    procedure WMNCMouseMove(var Message: TWMNCHitMessage);
    procedure CMVisibleChanged(var Message: TMessage);
    procedure SetCurrentDockSite(const Value: TWinControl);
    procedure SetLastDockSite(const Value: TWinControl);
    procedure SetVSPaneWidth(const Value: Integer);
    procedure SetUnDockLeft(const Value: Integer);
    procedure SetUnDockTop(const Value: Integer);
    function GetDockState: Integer;
    procedure SetCanFloat(const Value: Boolean);
    procedure SetRelativeServer(const Value: TCnDockServer);
    procedure SetDockLevel(const Value: Integer);
    procedure SetEnableCloseBtn(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
    procedure DoMenuPopup(X, Y: Integer); virtual;
    { ------------------------------------------------------------------------ }
    procedure Deactivate; virtual;
    procedure Activate; virtual;
    { ------------------------------------------------------------------------ }
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    { ------------------------------------------------------------------------ }
    procedure DoFloatDockClients(PanelAlign: TAlign);
    procedure DoFloatDockEachOther;
    procedure SetBottomDock(const Value: Boolean); override;
    procedure SetEachotherDock(const Value: Boolean); override;
    procedure SetEnableDock(const Value: Boolean); override;
    procedure SetLeftDock(const Value: Boolean); override;
    procedure SetRightDock(const Value: Boolean); override;
    procedure SetTopDock(const Value: Boolean); override;
    { ------------------------------------------------------------------------ }
    procedure DoFormOnClose(Sender: TObject;
      var Action: TCloseAction); override;
    { ------------------------------------------------------------------------ }
    procedure WMSize(var Message: TWMSize);
    procedure WMActivate(var Message: TWMActivate);
//    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged)
    procedure WindowProc(var Message: TMessage); override;
    property RelativeServer: TCnDockServer read FRelativeServer write SetRelativeServer default nil;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { ------------------------------------------------------------------------ }
    procedure FormStartDock(var Source: TCnDragDockObject); virtual;
    procedure FormPositionDockRect(Source: TCnDragDockObject); virtual;
    procedure FormDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); virtual;
    procedure FormDockDrop(Source: TCnDragDockObject; X, Y: Integer); virtual;
    procedure FormEndDock(Target: TObject; X, Y: Integer); virtual;
    function FormUnDock(NewTarget: TWinControl; Client: TControl): Boolean; virtual;
    procedure FormGetSiteInfo(Source: TCnDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); virtual;
    procedure FormGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); virtual;
    { ------------------------------------------------------------------------ }
    procedure Assign(Source: TPersistent); override;
    { 触发OnFormShow事件 }
    procedure MakeShowEvent;
    { 触发OnFormClose事件 }
    procedure MakeHideEvent;
    { 创建DefaultConjoinPanelClass的实例 }
    function CreateConjoinPanelClass(ConjoinHost: TForm): TCnConjoinPanel;
    { 创建DefaultTabDockClass的实例 }
    function CreateTabDockClass(TabHost: TForm): TCnTabPageControl;
    { ------------------------------------------------------------------------ }
    function CreateConjoinHostAndDockControl(Control1,
      Control2: TControl; DockType: TAlign): TCnConjoinDockHostForm; virtual;
    function CreateTabHostAndDockControl(Control1,
      Control2: TControl): TCnTabDockHostForm; virtual;
    { ------------------------------------------------------------------------ }
    procedure DoNCButtonDown(Message: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TMouseStation); virtual;
    procedure DoNCButtonUp(Message: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TMouseStation); virtual;
    procedure DoNCMouseMove(Message: TWMNCHitMessage;
      MouseStation: TMouseStation); virtual;
    procedure DoNCButtonDblClk(Message: TWMNCHitMessage; Button: TMouseButton;
      MouseStation: TMouseStation); virtual;
    { ------------------------------------------------------------------------ }
    procedure DoPaintDockGrabber(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoPaintDockSplitter(Canvas: TCanvas;
      Control: TControl; const ARect: TRect);
    procedure DoFormShowHint(HTFlag: Integer; var HintStr: string; var CanShow: Boolean);
    { ------------------------------------------------------------------------ }
    procedure ShowParentForm;   // 显示ParentForm
    procedure HideParentForm;   // 隐藏ParentForm
    { 还原所有的停靠客户 }
    procedure RestoreChild;
    { ------------------------------------------------------------------------ }
    property VSPaneWidth: Integer read FVSPaneWidth write SetVSPaneWidth;
    { ------------------------------------------------------------------------ }
    { 表示父控件的可见性 }
    property ParentVisible: Boolean read FParentVisible write SetParentVisible;
    { ------------------------------------------------------------------------ }
    { 当前的停靠服务器 }
    property CurrentDockSite: TWinControl read FCurrentDockSite write SetCurrentDockSite;
    { 当前的停靠服务器 }
    property LastDockSite: TWinControl read FLastDockSite write SetLastDockSite;
    property UnDockLeft: Integer read FUnDockLeft write SetUnDockLeft;
    property UnDockTop: Integer read FUnDockTop write SetUnDockTop;

    { 停靠客户的状态，是在浮动状态或者停靠状态，还是其他别的状态 }
    property DockState: Integer read GetDockState;

  published
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnFormHide: TNotifyEvent read FOnFormHide write FOnFormHide;
    property OnNCButtonDown: TNCButtonDownEvent read FOnNCButtonDown write FOnNCButtonDown;
    property OnNCButtonUp: TNCButtonUpEvent read FOnNCButtonUp write FOnNCButtonUp;
    property OnNCMouseMove: TNCMouseMoveEvent read FOnNCMouseMove write FOnNCMouseMove;
    property OnNCButtonDblClk: TNCButtonDblClkEvent read FOnNCButtonDblClk write FOnNCButtonDblClk;
    property OnPaintDockGrabber: TPaintDockGrabberEvent read FOnPaintDockGrabber write FOnPaintDockGrabber;
    property OnPaintDockSplitter: TPaintDockSplitterEvent read FOnPaintDockSplitter write FOnPaintDockSplitter;
    property OnFormShowHint: TFormHintEvent read FOnFormShowHint write FOnFormShowHint;
    { ------------------------------------------------------------------------ }
    property LRDockWidth: Integer read GetLRDockWidth write SetLRDockWidth;
    property TBDockHeight: Integer read GetTBDockHeight write SetTBDockHeight;
    property NCPopupMenu: TPopupMenu read FNCPopupMenu write SetNCPopupMenu;
    property DirectDrag: Boolean read FDirectDrag write FDirectDrag;
    property ShowHint: Boolean read FShowHint write FShowHint;
    property CanFloat: Boolean read FCanFloat write SetCanFloat default True;
    property DockLevel: Integer read FDockLevel write SetDockLevel default 0;
    property EnableCloseBtn: Boolean read FEnableCloseBtn write SetEnableCloseBtn;
    property EnableDock;
    property LeftDock;
    property TopDock;
    property RightDock;
    property BottomDock;
    property EachOtherDock;
    property DockStyle;
  end;

  TCnConjoinPanel = class(TCnCustomDockPanel)
  private
    FDockClient: TCnDockClient;
    function GetParentForm: TCnConjoinDockHostForm;
//    procedure SetUnDockControl(const Value: TControl);
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    { ------------------------------------------------------------------------ }
    procedure CustomStartDock(var Source: TCnDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject;
      Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
      MousePos: TPoint; var CanDock: Boolean); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    property DockClient: TCnDockClient read FDockClient write FDockClient;
    property ParentForm: TCnConjoinDockHostForm read GetParentForm;
  end;

  TCnAdvConjoinPanel = class(TCnConjoinPanel)
  private
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  TCnTabPageControl = class(TCnDockPageControl)
  private
    FDockClient: TCnDockClient;
    FVersion: Integer;
    function GetParentForm: TCnTabDockHostForm;
  protected
    { ------------------------------------------------------------------------ }
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure ReloadDockedControl(const AControlName: string;
      var AControl: TControl); override;
    { ------------------------------------------------------------------------ }
    procedure CustomStartDock(var Source: TCnDragDockObject); override;
    procedure CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomEndDock(Target: TObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property DockClient: TCnDockClient read FDockClient write FDockClient;
    property ParentForm: TCnTabDockHostForm read GetParentForm;
    property TabPosition;
  end;

  TCnAdvTabPageControl = class(TCnTabPageControl)
  private
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
  protected
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    { ------------------------------------------------------------------------ }
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  public
    destructor Destroy; override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  end;

  // TCnDockableForm是TCnConjoinDockHostForm和TCnTabDockHostForm共同的父类
  // 处理一些这两个类共同的事件。
  // 她上面也有一个TCnDockClient控件，也就是说她也是一个可停靠控件。
  // 和一般的放了TCnDockClient控件的窗口有一样的特性。
  TCnDockableForm = class(TForm)
  private
    FDockClient: TCnDockClient;
    FDockableControl: TWinControl;
    FUnDockControl: TControl;
    FFloatingChild: TControl;
    function GetDockableControl: TWinControl;
    procedure SetDockableControl(const Value: TWinControl);
    procedure SetUnDockControl(const Value: TControl);
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DockClient: TCnDockClient read FDockClient write FDockClient;
    property DockableControl: TWinControl read GetDockableControl write SetDockableControl;
    property UnDockControl: TControl read FUnDockControl write SetUnDockControl;
    property FloatingChild: TControl read FFloatingChild;
  published
  end;

  { 平铺的服务窗体 }
  TCnConjoinDockHostForm = class(TCnDockableForm)
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
    Panel: TCnConjoinPanel;
    constructor Create(AOwner: TComponent); override;
    property DockClient;
  published
  end;

  { 分页的服务窗体 }
  TCnTabDockHostForm = class(TCnDockableForm)
  public
    PageControl: TCnTabPageControl;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetActiveDockForm: TForm;
    property DockClient;

  published
  end;

var
  { 默认的TDockServer停靠Panel是TCnDockPanel }
  DefaultDockPanelClass: TCnDockPanelClass = TCnDockPanel;

  { 默认的停靠分割条是TCnDockSplitter }
  DefaultDockSplitterClass: TCnDockSplitterClass = TCnDockSplitter;

  { 默认的平铺停靠类是TCnConjoinPanel }
  DefaultConjoinPanelClass: TCnConjoinPanelClass = TCnConjoinPanel;

  { 默认的分页停靠类是TCnTabPageControl }
  DefaultTabDockClass: TCnTabDockClass = TCnTabPageControl;

  { 默认的停靠管理器节点是TCnDockZone }
  DefaultDockZoneClass: TCnDockZoneClass = TCnDockZone;

  { 默认的停靠管理器是TCnDockTree }
  DefaultDockTreeClass: TCnDockTreeClass = TCnDockTree;

//  FCnConjoinServerOptionClass: TCnBasicConjoinServerOptionClass;
//  FCnTabServerOptionClass: TCnBasicTabServerOptionClass;

{ 显示窗体 }
procedure ShowDockForm(DockWindow: TWinControl);
{ 隐藏窗体 }
procedure HideDockForm(DockWindow: TControl);
{ 使在Host上的TDockClient控件触发OnShowEvent或OnHideEvent，递归调用 }
procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
{ 获得窗体的可见性 }
function GetFormVisible(DockWindow: TWinControl): Boolean;
{ 设置分页的服务窗体中的PageControl的PopMenu }
procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
{ 设置分页的服务窗体中的PageControl的HotTrack }
procedure SetDockPageControlHotTrack(Value: Boolean);
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
{ 在窗体上查找TDockBaseControl控件 }
function FindDockBaseControl(Client: TControl): TCnDockBaseControl;
{ 在窗体上查找TDockClient控件 }
function FindDockClient(Client: TControl): TCnDockClient;
{ 在窗体上查找TDockServer控件 }
function FindDockServer(Client: TControl): TCnDockServer;
{ 检查停靠服务器和客户端或者停靠客户端之间是否有停靠关系 }
function IsDockable(Sender: TWinControl; Client: TControl;
  DropCtl: TControl = nil; DockAlign: TAlign = alNone): Boolean;
{ 计算停靠的区域 }
function ComputeDockingRect(AControl: TControl;
  var DockRect: TRect; MousePos: TPoint): TAlign;
{ 使停靠窗体AControl从Sender中拖出来 }
procedure DoFloat(Sender, AControl: TControl);
{ 设置控件的DockSite属性 }
procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
{ 使DockForm控件浮动，包括它上面的DockClients }
procedure DoFloatForm(DockForm: TControl);
{ 删除所有无用的TCnDockableForm窗体 }
procedure FreeAllDockableForm;
{ 使应用程序的所有的窗体都浮动 }
procedure DoFloatAllForm;
{ 得到AControl客户区中Align方向排列的子控件累加的大小到AControl边框的距离 }
function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl = nil): Integer;
procedure ResetDockClient(Control: TControl; NewTarget: TControl); overload;
procedure ResetDockClient(DockClient: TCnDockClient; NewTarget: TControl); overload;
procedure ReshowAllVisibleWindow;
{ 释放所有没有客户的服务窗体 }
//procedure FreeAllHostOfNotClient;



implementation

uses Dialogs, Math, CnDockSupportProc, CnDockGlobal, CnDockInfo, CnVSNETDockStyle;//, CnVSNETDockStyle;

var { 停靠的PageConrol的弹出菜单，默认为没有 }
    DockPageControlPopupMenu: TPopupMenu = nil;
    { 停靠的PageConrol是否高亮显示，默认为False }
    DockPageControlHotTrack: Boolean = False;
    { 分页式窗口的风格，默认为bsSizeToolWin }
    TabDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;
    { 平铺式窗体的风格，默认为bsSizeToolWin }
    ConjoinDockHostBorderStyle: TFormBorderStyle = bsSizeToolWin;
    { 操作系统是否是WinXP }
    IsWinXP: Boolean;

type
  TCnControlAccess = class(TControl);
  TCnWinControlAccess = class(TWinControl);

{$R CnDockableForm.DFM}
{$R CnConjoinDockHost.DFM}
{$R CnTabDockHost.DFM}

{ 在窗体上查找TDockBaseControl控件 }
function FindDockBaseControl(Client: TControl): TCnDockBaseControl;
var i: Integer;
begin
  Result := nil;
  if Client <> nil then
  begin
    for i := 0 to Client.ComponentCount - 1 do
      if (Client.Components[i] is TCnDockBaseControl) then
      begin
        Result := TCnDockBaseControl(Client.Components[i]);
        Exit;
      end;
  end;
end;

{ 在窗体上查找TDockServer控件 }
function FindDockServer(Client: TControl): TCnDockServer;
var ADockControl: TCnDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TCnDockServer then
    Result := TCnDockServer(ADockControl)
  else Result := nil;
end;

{ 在窗体上查找TDockClient控件 }
function FindDockClient(Client: TControl): TCnDockClient;
var ADockControl: TCnDockBaseControl;
begin
  ADockControl := FindDockBaseControl(Client);
  if ADockControl is TCnDockClient then
    Result := TCnDockClient(ADockControl)
  else Result := nil;
end;

{ 检查停靠服务器和客户端或者停靠客户端之间是否有停靠关系 }
function IsDockable(Sender: TWinControl; Client: TControl; DropCtl: TControl = nil;
  DockAlign: TAlign = alNone): Boolean;
var ADockClient: TCnDockClient;
  i: Integer;
  SenderDockStyle,
  ClientDockStyle: TCnBasicDockStyle;
  SenderStyleName,
  ClientStyleName: string;
//  Level_1: Integer;
Label JudgeRelation;

begin
  ADockClient := FindDockClient(Client);
  Result := False;
  if (ADockClient <> nil) and (ADockClient.EnableDock) then
  begin
    if Sender is TCnDockPanel then
    begin
      with TCnDockPanel(Sender) do
      begin
        Result := DockServer.EnableDock and
//          ((DockAlign <> alClient) or (ADockClient.EachOtherDock and (DockAlign = alClient))) and
          (((Align = alLeft) and DockServer.LeftDock and (ADockClient.LeftDock)) or
          ((Align = alTop) and DockServer.TopDock and (ADockClient.TopDock)) or
          ((Align = alRight) and DockServer.RightDock and (ADockClient.RightDock)) or
          ((Align = alBottom) and DockServer.BottomDock and (ADockClient.BottomDock)));
        SenderDockStyle := DockServer.DockStyle;
      end;
    end else
    begin
      if (Sender <> nil) and (Sender.Parent is TCnDockableForm) then
      begin
        with TCnDockableForm(Sender.Parent).DockableControl do
          for i := 0 to DockClientCount - 1 do
            if DockClients[i] = Client then
              Exit;
      end;
      Result := ADockClient.EachOtherDock;
      if Sender <> nil then
        ADockClient := FindDockClient(Sender.Parent);
      if ADockClient <> nil then
        Result := Result and ADockClient.EachOtherDock;
//      else Result := False;
      if ADockClient <> nil then
        SenderDockStyle := ADockClient.DockStyle
      else Exit;
    end;

    ADockClient := FindDockClient(Client);
    if ADockClient <> nil then
      ClientDockStyle := ADockClient.DockStyle
    else Exit;

    if SenderDockStyle = nil then
      SenderStyleName := ''
    else SenderStyleName := SenderDockStyle.ClassName;

    if ClientDockStyle = nil then
      ClientStyleName := ''
    else ClientStyleName := ClientDockStyle.ClassName;

    Result := Result and (SenderStyleName = ClientStyleName);

JudgeRelation:
{    if Result and (GlobalDockClient <> nil) then
    begin
      ADockClient := nil;
      Level_1 := GlobalDockClient.DockLevel;
      if DropCtl <> nil then
        ADockClient := FindDockClient(DropCtl);
      if ADockClient = nil then
      begin
        if Sender.Parent is TCnDockableForm then
          ADockClient := FindDockClient(Sender.Parent)
        else
          ADockClient := FindDockClient(Sender);
      end;
      if ADockClient <> nil then
        Result := Level_1 = ADockClient.DockLevel;
    end;}
(*    ADockClient := GlobalDockClient;
    if Result and (Sender <> nil) and (ADockClient <> nil) then
    begin
      ADockServer := FindDockServer(Sender.Parent);
      if (ADockServer <> nil) and (ADockClient.RelativeServer <> nil) then
        Result := Result and (ADockServer = ADockClient.RelativeServer)
      else
      begin
        ADockServer := ADockClient.RelativeServer;
        if Sender.Parent is TCnDockableForm then
          ADockClient := FindDockClient(Sender.Parent)
        else
          ADockClient := FindDockClient(Sender);
        if ((ADockClient <> nil) and (ADockClient.RelativeServer = nil)) or (ADockServer = nil) then
          Exit;
        if (ADockClient <> nil){ or (ADockServer <> nil) }then
          Result := Result and (ADockServer = ADockClient.RelativeServer);
      end;
    end;*)
  end;
end;

{ 更新窗体的Caption，同时更新父窗口的Caption，递归调用 }
procedure UpdateCaption(Source: TWinControl; Exclude: TControl);
var i: Integer;
  Host: TCnDockableForm;
begin
  if (Source <> nil) and (Source.Parent is TCnDockableForm) then
  begin
    Host := TCnDockableForm(Source.Parent);
    Host.Caption := '';
    { 更新本身的Caption }
    for I := 0 to Source.DockClientCount - 1 do
    begin
      if Source.DockClients[I].Visible and (Source.DockClients[I] <> Exclude) then
        Host.Caption := Host.Caption + TCustomForm(Source.DockClients[I]).Caption + gs_CnStringSplitter;
    end;
    { 更新TCnTabPageControl的标签 }
    if (Host.HostDockSite is TCnTabPageControl) then
    begin
      with TCnTabPageControl(Host.HostDockSite) do
        if (ActivePage <> nil) and (ActivePage.Controls[0] = Source) then
          ActivePage.Caption := Host.Caption;
    end;
    UpdateCaption(Host.HostDockSite, nil);
  end;
end;

{ 使停靠窗体AControl从Sender中拖出来 }
procedure DoFloat(Sender, AControl: TControl);
var
  ARect: TRect;
  CH{ CaptionHeight }, BW{ BorderWidth }: Integer;
begin
  { 获得标题栏的高度和边框的宽度 }
  BW := Cn_GetSysBorderWidth;
  CH := Cn_GetSysCaptionHeight;
  { 进行坐标转换 }
  ARect.TopLeft := Sender.ClientToScreen(Point(
    -(BW + 3), -(CH + BW + 1)));
  ARect.BottomRight := Sender.ClientToScreen(Point(
    Sender.UndockWidth - (BW + 3),
    Sender.UndockHeight - (BW + CH + 1)));
//  Cn_LockWindow(TWinControl(Sender));
  try
    { 使AControl浮动 }
    AControl.ManualFloat(ARect);
  finally
//    Cn_UnLockWindow;
  end;
  if (AControl.Left <> ARect.Left) or (AControl.Top <> ARect.Top) then
  begin
    AControl.Left := ARect.Left;
    AControl.Top := ARect.Top;
  end;
//  SetDockSite(AControl, True);
end;

{ 计算停靠的区域 }
function ComputeDockingRect(AControl: TControl; var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect,
  DockLeftRect,
  DockBottomRect,
  DockRightRect,
  DockCenterRect: TRect;
begin
  Result := alNone;
  // 划分停靠区域
  if AControl = nil then Exit;
  with AControl do
  begin
    DockLeftRect.TopLeft := Point(0, 0);
    DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

    DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
    DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

    DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
    DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

    DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5 * 4);
    DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight);

    DockCenterRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5);
    DockCenterRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5 * 4);

    // 发现鼠标在哪个停靠区域
    if PtInRect(DockLeftRect, MousePos) then
    begin
      Result := alLeft;
      DockRect := DockLeftRect;
      DockRect.Right := ClientWidth div 2;
    end else if PtInRect(DockTopRect, MousePos) then
    begin
      Result := alTop;
      DockRect := DockTopRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Bottom := ClientHeight div 2;
    end else if PtInRect(DockRightRect, MousePos) then
    begin
      Result := alRight;
      DockRect := DockRightRect;
      DockRect.Left := ClientWidth div 2;
    end else if PtInRect(DockBottomRect, MousePos) then
    begin
      Result := alBottom;
      DockRect := DockBottomRect;
      DockRect.Left := 0;
      DockRect.Right := ClientWidth;
      DockRect.Top := ClientHeight div 2;
    end else if PtInRect(DockCenterRect, MousePos) then
    begin
      Result := alClient;
      DockRect := DockCenterRect;
    end;
    if Result = alNone then Exit;

    // DockRect是屏幕坐标
    DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
    DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
  end;
end;

{ 显示窗体，如果父窗口也是不可见的，就显示父窗口，递归调用 }
procedure ShowDockForm(DockWindow: TWinControl);
  procedure ShowClient(Client, DockParent: TWinControl);
  var ADockClient: TCnDockClient;
    i: Integer;
  begin
    { 如果窗体上面还有停靠窗体，就分别调用她们的MakeDockClientEvent }
    if (DockParent is TCnDockableForm) and (Client <> nil) then
    begin
      with TCnDockableForm(DockParent).DockableControl do
        for i := 0 to DockClientCount - 1 do
        begin
          if DockClients[i] <> Client then
            MakeDockClientEvent(DockClients[i], True);
        end;
      if Client.HostDockSite is TCnCustomDockControl then
        TCnCustomDockControl(Client.HostDockSite).UpdateCaption(nil);
    end else
    begin
      { 否则就调用自己的MakeShowEvent }
      ADockClient := FindDockClient(DockParent);
      if ADockClient <> nil then
      begin
        ADockClient.DockStyle.ShowDockForm(ADockClient);
        if DockParent.CanFocus then
          DockParent.SetFocus;
      end;
    end;
    if DockParent.Parent = nil then
      SetForegroundWindow(DockParent.Handle);
  end;

  { 显示TCnDockPanel }
  function ShowDockPanel(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Client <> nil then
    begin
      if Client.HostDockSite is TCnDockPanel then
      begin
        { 根据具体情况显示TCnDockPanel }
        TCnDockPanel(Client.HostDockSite).ShowDockPanel(True, Client, sdfDockPanel);
        Result := nil;
      end;
    end;
  end;

  { 显示分页窗体 }
  function ShowTabDockHost(Client: TWinControl): TWinControl;
  var i: Integer;
  begin
    Result := Client;
    if Client <> nil then
    begin
      ShowClient(nil, Client);
      if Client.HostDockSite is TCnTabPageControl then
      begin
        with TCnTabPageControl(Client.HostDockSite) do
          for i:=0 to PageCount - 1 do
          begin
            if Pages[i].Controls[0] = Client then
            begin
              Pages[i].Show;
              Break;
            end;
          end;
        if (Client.HostDockSite <> nil) and not (Client.HostDockSite is TCnDockPanel) then
        begin
          Result := Client.HostDockSite.Parent;
          ShowClient(Client, Result);
          if (Result <> nil) and (Result.HostDockSite is TCnTabPageControl) then
            Result := ShowTabDockHost(Result);
        end;
      end;
    end;
  end;

  { 显示平铺窗体 }
  function ShowConjoinDockHost(Client: TWinControl): TWinControl;
  begin
    Result := Client;
    if Client <> nil then
    begin
      ShowClient(nil, Client);
      if (Client.HostDockSite <> nil) and not (Client.HostDockSite is TCnDockPanel) then
      begin
        if Client.HostDockSite.Parent <> nil then
        begin
          Result := Client.HostDockSite.Parent;
          ShowClient(Client, Result);
          if (Result <> nil) and (Result.HostDockSite is TCnConjoinPanel) then
            Result := ShowConjoinDockHost(Result);
        end;
      end;
    end;
  end;

  procedure ShowPopupPanel(Client: TWinControl);
  begin
    if Client.HostDockSite is TCnVSPopupPanel then
      TCnVSPopupPanel(Client.HostDockSite).VSChannel.PopupDockForm(Client)
    else if (Client.HostDockSite <> nil) and (Client.HostDockSite.Parent <> nil) then
    begin
      if (Client.HostDockSite.Parent.HostDockSite is TCnVSPopupPanel) then
        TCnVSPopupPanel(Client.HostDockSite.Parent.HostDockSite).VSChannel.PopupDockForm(Client)
      else if (Client.HostDockSite.Parent.HostDockSite is TCnDockPanel) then
        Client.HostDockSite.Parent.HostDockSite.Invalidate;
    end;
  end;

var TmpDockWindow: TWinControl;
begin
  TmpDockWindow := DockWindow;
  repeat
    DockWindow := ShowTabDockHost(DockWindow);
    DockWindow := ShowConjoinDockHost(DockWindow);
    DockWindow := ShowDockPanel(DockWindow);
  until (DockWindow = nil) or (DockWindow.Parent = nil);
  ShowPopupPanel(TmpDockWindow);
end;

{ 隐藏窗体，递归调用 }
procedure HideDockForm(DockWindow: TControl);
  { 隐藏停靠客户窗体 }
  procedure HideDockChild(DockWindow: TControl);
  var i: Integer;
    DockClient: TCnDockClient;
  begin
    if DockWindow = nil then Exit;
    if (DockWindow is TCnDockableForm) and (DockWindow.Visible) then
    begin
      { 如果窗体上还有子窗体并且窗体是可见的，就调用子窗体的DockStyle的HideDockForm }
      with TCnDockableForm(DockWindow).DockableControl do
        for i := 0 to DockClientCount - 1 do
          HideDockChild(DockClients[i]);
    end;
    DockClient := FindDockClient(DockWindow);
    if (DockWindow is TForm) and (TForm(DockWindow).FormStyle <> fsMDIChild) then
      DockClient.DockStyle.HideDockForm(DockClient);
  end;

  { 隐藏停靠父窗体 }
  procedure HideDockParent(DockWindow: TControl);
  var
    Host: TWinControl;
    DockClient: TCnDockClient;
  begin
    if (DockWindow <> nil) and (DockWindow.HostDockSite <> nil) then
    begin
      Host := DockWindow.HostDockSite;
      if Host.VisibleDockClientCount = 0 then
      begin
        { 如果停靠窗体的父的上面的可见停靠窗体的个数为0，就应该隐藏这个停靠窗体的父 }
        if Host is TCnDockPanel then
          { 调整TCnDockPanel的大小 }
          TCnDockPanel(Host).ShowDockPanel(False, nil)
        else begin
          if Host.Parent <> nil then
          begin
            { 使停靠窗体的父隐藏，并且调用她上面的TCnDockClient的DockStyle的HideDockForm函数 }
            DockClient := FindDockClient(Host.Parent);
            DockClient.DockStyle.HideDockForm(DockClient);
            HideDockParent(Host.Parent);
          end;
        end
      end;
    end;
  end;

  procedure HidePopupPanel(Client: TWinControl);
  begin
    if Client.HostDockSite is TCnVSPopupPanel then
      TCnVSPopupPanel(Client.HostDockSite).VSChannel.HidePopupPanel(Client)
    else if (Client.HostDockSite <> nil) and (Client.HostDockSite.Parent <> nil) then
    begin
      if (Client.HostDockSite.Parent.HostDockSite is TCnVSPopupPanel) then
        TCnVSPopupPanel(Client.HostDockSite.Parent.HostDockSite).VSChannel.HidePopupPanel(Client)
      else if (Client.HostDockSite.Parent.HostDockSite is TCnDockPanel) then
        Client.HostDockSite.Parent.HostDockSite.Invalidate
    end;
  end;

var TmpDockWindow: TWinControl;

begin
  TmpDockWindow := TWinControl(DockWindow);
  HideDockChild(DockWindow);
  HideDockParent(DockWindow);
  if (DockWindow.HostDockSite is TCnCustomDockControl) then
    TCnCustomDockControl(DockWindow.HostDockSite).UpdateCaption(DockWindow);
  HidePopupPanel(TmpDockWindow);
end;

{ 使在Host上的TDockClient控件触发OnShowEvent或OnHideEvent，递归调用 }
procedure MakeDockClientEvent(Host: TControl; Visible: Boolean);
var i: Integer;
  ADockClient: TCnDockClient;
begin
  ADockClient := FindDockClient(Host);
  if ADockClient <> nil then
  begin
    if Visible then ADockClient.MakeShowEvent
    else ADockClient.MakeHideEvent;
    if (Host is TCnDockableForm) and (Host.Visible) then
    begin
      with TCnDockableForm(Host).DockableControl do
        for i := 0 to DockClientCount - 1 do
          MakeDockClientEvent(DockClients[i], Visible);
    end;
  end;
end;

{ 获得窗体的可见性，递归调用 }
function GetFormVisible(DockWindow: TWinControl): Boolean;
var
  ADockClient: TCnDockClient;
begin
  Result := True;
  ADockClient := FindDockClient(DockWindow);
  if ADockClient <> nil then
  begin
    Result := ADockClient.DockStyle.GetDockFormVisible(ADockClient);
  end;
end;

{ 设置分页的服务窗体中的PageControl的PopMenu }
procedure SetDockPageControlPopupMenu(Value: TPopupMenu);
var i: Integer;
begin
  DockPageControlPopupMenu := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.CustomForms[i] is TCnTabDockHostForm then
      TCnTabDockHostForm(Screen.CustomForms[i]).PageControl.PopupMenu := Value;
  end;
end;

{ 设置分页的服务窗体中的PageControl的HotTrack }
procedure SetDockPageControlHotTrack(Value: Boolean);
var i: Integer;
begin
  DockPageControlHotTrack := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.CustomForms[i] is TCnTabDockHostForm then
      TCnTabDockHostForm(Screen.CustomForms[i]).PageControl.HotTrack := Value;
  end;
end;

{ 设置分页的服务窗体的显示风格 }
procedure SetTabDockHostBorderStyle(Value: TFormBorderStyle);
var i: Integer;
begin
  TabDockHostBorderStyle := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if (Screen.CustomForms[i] is TCnTabDockHostForm) and (Screen.CustomForms[i].HostDockSite = nil) then
      TCnTabDockHostForm(Screen.CustomForms[i]).BorderStyle := Value;
  end;
end;

{ 设置平铺的服务窗体的显示风格 }
procedure SetConjoinDockHostBorderStyle(Value: TFormBorderStyle);
var i: Integer;
begin
  ConjoinDockHostBorderStyle := Value;
  for i := 0 to Screen.FormCount - 1 do
  begin
    if (Screen.CustomForms[i] is TCnConjoinDockHostForm) and (Screen.CustomForms[i].HostDockSite = nil) then
      TCnConjoinDockHostForm(Screen.CustomForms[i]).BorderStyle := Value;
  end;
end;

{ 把停靠窗体的信息保存进文件 }
procedure SaveDockTreeToFile(FileName: string);
var CnDockInfoTree: TCnDockInfoTree;
  i: Integer;
begin
  HideAllPopupPanel(nil);
  { 创建停靠信息树 }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  try
    { 遍历应用程序的所有窗口 }
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      if (Screen.CustomForms[i].Parent = nil) and
      ((FindDockClient(Screen.CustomForms[i]) <> nil) or (FindDockServer(Screen.CustomForms[i]) <> nil)) then
        { 把符合条件的窗体的停靠信息保存到信息树中 }
        CnDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[i]);
    end;
    { 打开INI文件 }
    CnDockInfoTree.DockInfoIni := TIniFile.Create(FileName);
    try
      { 把信息树中的停靠信息写到INI文件中 }
      CnDockInfoTree.WriteInfoToIni;
    finally
      { 关闭INI文件 }
      CnDockInfoTree.DockInfoIni.Free;
    end;
  finally
    { 释放停靠信息树 }
    CnDockInfoTree.Free;
  end;
end;

procedure ReshowAllVisibleWindow;
var i: Integer;
begin
  if IsWinXP then
  begin
    for i := 0 to Screen.FormCount - 1 do
    begin
      if Screen.Forms[i].Visible then
        Windows.ShowWindow(Screen.Forms[i].Handle, SW_SHOW)
      else
        Windows.ShowWindow(Screen.Forms[i].Handle, SW_HIDE);
    end;
  end;
end;

{ 从文件里读出停靠窗体的信息 }
procedure LoadDockTreeFromFile(FileName: string);
var CnDockInfoTree: TCnDockInfoTree;
  Form: TForm;
begin
  HideAllPopupPanel(nil);
  // 创建一个没有边框，大小为0的窗体，这个窗体的作用是防止屏幕闪烁
  Form := TForm.CreateNew(nil);
  Form.BorderStyle := bsNone;
  Form.BoundsRect := Rect(0,0,0,0);
  Form.Visible := True;
  Form.Name := 'A_B_C_D_E_F_G_H_I_J_K_L_M_N';
  { 创建停靠信息树 }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  { 锁住Windows桌面 }
  Cn_LockWindow(nil);
  try
    { 打开INI文件 }
    CnDockInfoTree.DockInfoIni := TIniFile.Create(FileName);
    try
      IsLoading := True;
      { 从INI文件里读出数据并且还原停靠信息 }
      CnDockInfoTree.ReadInfoFromIni;
    finally
      { 关闭INI文件 }
      CnDockInfoTree.DockInfoIni.Free;
      IsLoading := False;
    end;
  finally
    Form.Release;
    { 解锁Windows桌面 }
    Cn_UnLockWindow;
    { 释放停靠信息树 }
    CnDockInfoTree.Free;
  end;
  ReshowAllVisibleWindow;
end;

{ 把停靠窗体的信息保存进注册表 }
procedure SaveDockTreeToReg(RootKey: DWORD; RegPath: string);
var CnDockInfoTree: TCnDockInfoTree;
  i: Integer;
begin
  HideAllPopupPanel(nil);
  { 创建停靠信息树 }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  try
    { 遍历应用程序的所有窗口 }

    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      if (Screen.CustomForms[i].Parent = nil) and
      (FindDockClient(Screen.CustomForms[i]) <> nil) then
        { 把符合条件的窗体的停靠信息保存到信息树中(接着是停靠客户) }
        CnDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[i]);
    end;

    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      if (Screen.CustomForms[i].Parent = nil) and
      (FindDockServer(Screen.CustomForms[i]) <> nil) then
        { 把符合条件的窗体的停靠信息保存到信息树中(首先是停靠服务器) }
        CnDockInfoTree.CreateZoneAndAddInfoFromApp(Screen.CustomForms[i]);
    end;

    { 打开TRegistry }
    CnDockInfoTree.DockInfoReg := TRegistry.Create;
    try
      { 把信息树中的停靠信息写到注册表中 }
      CnDockInfoTree.DockInfoReg.RootKey := RootKey;
      CnDockInfoTree.WriteInfoToReg(RegPath);
    finally
      { 关闭注册表 }
      CnDockInfoTree.DockInfoReg.Free;
    end;
  finally
    { 释放停靠信息树 }
    CnDockInfoTree.Free;
  end;
end;

{ 从注册表里读出停靠窗体的信息 }
procedure LoadDockTreeFromReg(RootKey: DWORD; RegPath: string);
var CnDockInfoTree: TCnDockInfoTree;
  Form: TForm;
begin
  HideAllPopupPanel(nil);
  // 创建一个没有边框，大小为0的窗体，这个窗体的作用是防止屏幕闪烁
  Form := TForm.CreateNew(nil);
  Form.BorderStyle := bsNone;
  Form.BoundsRect := Rect(0,0,0,0);
  Form.Visible := True;
  Form.Name := 'A_B_C_D_E_F_G_H_I_J_K_L_M_N';
  { 创建停靠信息树 }
  CnDockInfoTree := TCnDockInfoTree.Create(TCnDockInfoZone);
  { 锁住Windows桌面 }
  Cn_LockWindow(nil);
  try
    { 打开TRegistry }
    CnDockInfoTree.DockInfoReg := TRegistry.Create;
    try
      IsLoading := True;
      { 从注册表里读出数据并且还原停靠信息 }
      CnDockInfoTree.DockInfoReg.RootKey := RootKey;
      CnDockInfoTree.ReadInfoFromReg(RegPath);
    finally
      { 关闭TRegistry }
      CnDockInfoTree.DockInfoReg.Free;
      IsLoading := False;
    end;
  finally
    { 解锁Windows桌面 }
    Cn_UnLockWindow;
    { 释放停靠信息树 }
    CnDockInfoTree.Free;
    Form.Release;
  end;
  ReshowAllVisibleWindow;
end;

{ 设置控件的DockSite属性 }
procedure SetDockSite(Control: TWinControl; SiteValue: Boolean);
begin
  TCnWinControlAccess(Control).DockSite := SiteValue;
  if (not (csDesigning in Control.ComponentState)) and (CnGlobalDockPresident <> nil) then
    CnGlobalDockPresident.RegisterDockSite(Control, SiteValue);
end;

{ 使DockForm控件浮动，包括它上面的DockClients }
procedure DoFloatForm(DockForm: TControl);
var i, j: Integer;
  ADockServer: TCnDockServer;
  ARect: TRect;
begin
  if (DockForm is TCnDockableForm) then
  begin
    {如果窗体是TCnDockableForm，就使她上面的停靠窗体浮动}
    with TCnDockableForm(DockForm).DockableControl do
    begin
      for i := DockClientCount - 1 downto 0 do
      begin
        DoFloatForm(DockClients[i]);
      end;
//      DockForm.Hide;
      DockForm.ManualDock(nil);
    end;
  end else
  begin
    ADockServer := FindDockServer(DockForm);
    if ADockServer <> nil then
    begin
      {如果窗体上有TCnDockServer，就使她四个上面的TCnDockPanel上面的窗体浮动}
      for i := 0 to 3 do
      begin
        for j := ADockServer.DockPanel[i].DockClientCount - 1 downto 0 do
          DoFloatForm(ADockServer.DockPanel[i].DockClients[j]);
        if ADockServer.DockPanel[i] is TCnVSNETDockPanel then
        begin
          with TCnVSNETDockPanel(ADockServer.DockPanel[i]).VSChannel do
          begin
            RemoveAllBlock;
            HidePopupPanel(ActiveDockForm);
          end;
        end;
      end;
    end else
    begin
      if DockForm.HostDockSite <> nil then
      begin
        {如果窗体是停靠状态的，就转换坐标系}
        if (DockForm.HostDockSite.Parent is TCnDockableForm) and
          (DockForm.HostDockSite.DockClientCount <= 2) then
          PostMessage(DockForm.HostDockSite.Parent.Handle, WM_CLOSE, 0, 0);
      end else ARect := DockForm.BoundsRect;
//      DockForm.Hide;
      {使窗体浮动}
      if DockForm.HostDockSite is TCnVSPopupPanel then
      begin
        TCnVSPopupPanel(DockForm.HostDockSite).VSNETDockPanel.VSChannel.RemoveDockControl(TWinControl(DockForm));
        DockForm.Dock(nil, Bounds(DockForm.Left, DockForm.Top, DockForm.UndockWidth, DockForm.UndockHeight));
      end else
        DockForm.ManualDock(nil);
    end;
  end;
end;

{ 删除所有无用的TCnDockableForm窗体 }
procedure FreeAllDockableForm;
var i: Integer;
begin
  Assert(CnGlobalDockPresident <> nil);
  for i := CnGlobalDockPresident.DockableFormList.Count - 1 downto 0 do
  begin
    if TCnDockableForm(CnGlobalDockPresident.DockableFormList[i]).DockableControl.DockClientCount = 0 then
      TCnDockableForm(CnGlobalDockPresident.DockableFormList[i]).Free;
  end;
end;

{ 使应用程序的所有的窗体都浮动 }
procedure DoFloatAllForm;
var i: Integer;
  TempList: TList;
begin
  TempList := TList.Create;
  try
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      // 遍历整个应用程序的所有窗体，然后把一般的窗体保存到TempList中
      if not (Screen.CustomForms[i] is TCnDockableForm) then
        TempList.Add(Screen.CustomForms[i]);
    end;
    {遍历应用程序的窗口，使她们浮动}
    for i := 0 to TempList.Count - 1 do
      DoFloatForm(TempList[i]);
  finally
    TempList.Free;
  end;
  FreeAllDockableForm;
end;

{ 得到AControl客户区中Align方向排列的子控件累加的大小到AControl边框的距离 }
function GetClientAlignControlArea(AControl: TWinControl; Align: TAlign; Exclude: TControl): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to AControl.ControlCount - 1 do
  begin
    if (AControl.Controls[i].Align = Align) and AControl.Controls[i].Visible and (AControl.Controls[i] <> Exclude) and
      not ((AControl.Controls[i] is TCnDockSplitter) or (AControl.Controls[i] is TCnDockPanel)) then
    begin
      if Align in [alLeft, alRight] then
        Inc(Result, AControl.Controls[i].Width)
      else Inc(Result, AControl.Controls[i].Height);
    end;
  end;
end;

{******************************************************************************}
{           下面的函数和方法都是内部函数                                       }
{******************************************************************************}

function GetActiveControl(AForm: TCustomForm): TWinControl;
var AControl: TWinControl;
begin
  Result := nil;
  AControl := AForm.ActiveControl;
  while AControl <> nil do
  begin
    if (AControl.HostDockSite <> nil) then
    begin
      Result := AControl;
      Exit;
    end;
    AControl := AControl.Parent;
  end;
end;

function GetHostDockParent(AControl: TWinControl): TWinControl;
begin
  Result := nil;
  while AControl <> nil do
  begin
    if (AControl.HostDockSite <> nil) then
    begin
      Result := AControl.HostDockSite;
      Exit;
    end;
    AControl := AControl.Parent;
  end;
end;

{ 重新设置在Control上的TCnDockClint对象的属性 }
procedure ResetDockClient(Control: TControl; NewTarget: TControl);
begin
  ResetDockClient(FindDockClient(Control), NewTarget);
end;

procedure ResetDockClient(DockClient: TCnDockClient; NewTarget: TControl);
var point: TPoint;
begin
  if DockClient <> nil then
  begin
    if (DockClient.ParentForm.HostDockSite is TCnDockPanel) and (NewTarget is TCnDockPanel) then
    begin

    end else
    begin

      if (DockClient.LastDockSite is TCnDockPanel) and (NewTarget is TCnDockPanel) and (DockClient.LastDockSite <> NewTarget) then
      begin
        with TCnDockPanel(DockClient.LastDockSite) do
        begin
          if UseDockManager and (CnDockManager <> nil) then
            CnDockManager.RemoveControl(DockClient.ParentForm);
        end;
      end;

      if (DockClient.ParentForm.HostDockSite is TCnDockPanel) then
      begin
//        if (NewTarget is TCnTabPageControl) or (NewTarget is TCnConjoinPanel) then
//          DockClient.LastDockSite := nil
//        else
          DockClient.LastDockSite := DockClient.ParentForm.HostDockSite;
      end
      else //if not (DockClient.LastDockSite is TCnDockPanel) then
        DockClient.LastDockSite := nil;

      if DockClient.ParentForm.HostDockSite = nil then
      begin
        DockClient.UnDockLeft := DockClient.ParentForm.BoundsRect.TopLeft.x;
        DockClient.UnDockTop := DockClient.ParentForm.BoundsRect.TopLeft.y;
      end else
      begin
        point := DockClient.ParentForm.BoundsRect.TopLeft;
        point := DockClient.ParentForm.HostDockSite.ClientToScreen(point);
        DockClient.UnDockLeft := point.x;
        DockClient.UnDockTop := point.y;
      end;
    end;
  end;
end;

{ TCnDockPanel }

constructor TCnDockPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  BevelOuter := bvNone;
  Width := 10;
  Height := 10;
end;

destructor TCnDockPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TCnDockPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);
  // 如果Source.Control是TCnDockableForm,就有可能被释放掉，如果它已经被释放掉了，
  // 就不要再调用ShowDockPanel函数了，不然会使TCnDockPanel宽度或者高度为0。
  if Source.Control <> nil then
    ShowDockPanel(True, Source.Control);
end;

procedure TCnDockPanel.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  if Accept then
    inherited CustomDockOver(Source, X, Y, State, Accept);
end;

procedure TCnDockPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TCnDockPanel.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TCnDockPanel.CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer);
var
  ARect: TRect;
//  Tmp: Integer;
{  function GetParentClientW(Control: TWinControl): Integer;
  var i: Integer;
  begin
    Result := Control.ClientWidth;
    for i := 0 to Control.ControlCount - 1 do
    begin
      if (Control.Controls[i].Align in [alLeft, alRight])
        and (Control.Controls[i] <> Self)
        and (Control.Controls[i].Visible) then
        Result := Result - Control.Controls[i].Width;
    end;
  end;

  function GetParentClientH(Control: TWinControl): Integer;
  var i: Integer;
  begin
    Result := Control.ClientHeight;
    for i := 0 to Control.ControlCount - 1 do
    begin
      if (Control.Controls[i].Align in [alTop, alBottom])
        and (Control.Controls[i] <> Self)
        and (Control.Controls[i].Visible) then
        Result := Result - Control.Controls[i].Height;
    end;
  end;
var ParentClientW, ParentClientH: Integer;}
begin
  inherited CustomPositionDockRect(Source, X, Y);
  if VisibleDockClientCount = 0 then
  begin
    // 记住DockRect预览停靠区域.
    if GlobalDockClient <> nil then
    begin
      case Align of
        alTop:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(
            Point(Width, Source.Control.TBDockHeight));
        end;
        alBottom:
        begin
          ARect.TopLeft := ClientToScreen(
            Point(0, -Source.Control.TBDockHeight));
          ARect.BottomRight := ClientToScreen(
            Point(Width, 0));
        end;
        alLeft:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(
            Point(Source.Control.LRDockWidth, Height));
        end;
        alRight:
        begin
          ARect.TopLeft := ClientToScreen(Point(-Source.Control.LRDockWidth, 0));
          ARect.BottomRight := ClientToScreen(Point(Width, Height));
        end;
      end;
      Source.DockRect := ARect;
    end;
  end;
end;

procedure TCnDockPanel.CustomStartDock(var Source: TCnDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TCnDockPanel.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ShowDockPanel(False, nil);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
end;

procedure TCnDockPanel.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  { 在整个应用程序中查找名字为AControlName的窗体 }
  AControl := Cn_FindDockFormWithName(AControlName);
end;

procedure TCnDockPanel.ShowDockPanel(MakeVisible: Boolean;
  Client: TControl; PanelSizeFrom: TSetDockPanelSizeFrom);
const
  DefaultDockSize = 100; // 默认的宽度或者大小
var
  DockHeight, DockWidth: Integer;
//Label Loop;
begin
  if (not MakeVisible and (VisibleDockClientCount > 1)) or
    (GlobalDockClient = nil){ or IsLoading }then Exit;
  { 设置TCnDockSplitter的属性 }
  DockServer.DockSplitter[Integer(Align) - 1].Visible := MakeVisible;

  if (MakeVisible and (Client <> nil)) then// or ((not MakeVisible) and (Client <> nil)) then
  begin
    if (Width * Height = 0) then
    begin
      { 如果宽度和高度有一个是0 }

      if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
      begin
        DockHeight := TBDockHeight;
        DockWidth := LRDockWidth;
      end else
      begin
        DockHeight := Client.TBDockHeight;
        DockWidth := Client.LRDockWidth;
      end;

      { 不能等于0，不然显示会不正常 }
      if DockHeight = 0 then
        DockHeight := DefaultDockSize;
      if DockWidth = 0 then
        DockWidth := DefaultDockSize;

      Parent.DisableAlign;
      try
        case Align of
          alTop:
          begin
            { 如果主窗体的客户区的上边有别的控件，就把这个高度计算在内 }
            Top := DockServer.GetClientAlignControl(alTop);
            Height := DockHeight;
            DockServer.TopSplitter.Top := Top + Height;
          end;
          alBottom:
          begin
            { 如果主窗体的客户区的下边有别的控件，就把这个高度计算在内 }
            Top := Parent.ClientHeight - DockServer.GetClientAlignControl(alBottom) - DockHeight + 1;
            Height := DockHeight;
            DockServer.BottomSplitter.Top := Top + DockServer.BottomSplitter.Height;
          end;
          alLeft:
          begin
            { 如果主窗体的客户区的左边有别的控件，就把这个宽度计算在内 }
            Left := DockServer.GetClientAlignControl(alLeft);
            Width := DockWidth;
            DockServer.LeftSplitter.Left := Left + Width;
          end;
          alRight:
          begin
            { 如果主窗体的客户区的右边有别的控件，就把这个宽度计算在内 }
            Width := DockWidth;
            Left := Parent.ClientWidth - DockServer.GetClientAlignControl(alRight) - DockWidth + 1;
            DockServer.RightSplitter.Left := Left - DockServer.RightSplitter.Width;
          end;
        end;
      finally
        Parent.EnableAlign;
        if UseDockManager and (CnDockManager <> nil) then
          CnDockManager.ResetBounds(True);
      end;
      DockServer.DoFinishSetDockPanelSize(Self);
    end;
  end else
  begin
//    if (Client <> nil) and (not Client.Visible) then goto Loop;
    if (PanelSizeFrom = sdfDockPanel) or (Client = nil) then
    begin
      if Height > 0 then
        TBDockHeight := Height;
      if Width > 0 then
        LRDockWidth := Width;
    end else
    begin
      if Height > 0 then
        Client.TBDockHeight := Height;
      if Width > 0 then
        Client.LRDockWidth := Width;
    end;
    if Align in [alLeft, alRight] then
      Width := 0
    else Height := 0;
    { 重新设置位置 }
    ResetPosition;
  end;
  { 条件满足的话就显示Client }
//Loop:
  if MakeVisible and (Client <> nil) then
  begin
    if (not Client.Visible) then
      Client.Show;
    if (not TWinControl(Client).Focused) and (TWinControl(Client).CanFocus) then
      TWinControl(Client).SetFocus;
  end;
end;

function TCnDockPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := IsDockable(Self, Client);
  ShowDockPanel(False, nil);
  Result := Result and (Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0);
  if Result then
  begin
    if not (Client is TCnDockableForm) then
      SetDockSite(TForm(Client), True);
  end;
end;

procedure TCnDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      TForm(Source.Control).ActiveControl := nil;
      { ------------------------------------------------------------------------ }
      SetDockSite(TForm(Source.Control), False);
    end;
    UpdateCaption(nil);
  end;
  ShowDockPanel(TWinControl(Source.DragTarget).VisibleDockClientCount > 0, Source.Control);
end;

function TCnDockPanel.GetPanelIndex: Integer;
begin
  case Align of
    alTop:    Result := 0;
    alBottom: Result := 1;
    alLeft:   Result := 2;
    alRight:  Result := 3;
  else
    Result := -1;
  end;
end;

procedure TCnDockPanel.Resize;
begin
  inherited;

end;

procedure TCnDockPanel.SetDockServer(const Value: TCnDockServer);
begin
  FDockServer := Value;
end;

procedure TCnDockPanel.ResetPosition;
begin
  { 调整TCnDockPanel的位置，使它总是在服务窗体的客户区的最内层 }
  case Align of
    alLeft:
      Left := GetClientAlignControlArea(Parent, Align) + 1;
    alRight:
      Left := Parent.ClientWidth - GetClientAlignControlArea(Parent, Align) - Width - 1;
    alTop:
      Top := GetClientAlignControlArea(Parent, Align) + 1;
    alBottom:
      Top := Parent.ClientHeight - GetClientAlignControlArea(Parent, Align) - Height - 1;
  end;
end;

{ TCnAdvDockPanel }

//procedure TCnAdvDockPanel.CMDockClient(var Message: TCMDockClient);
//var DockClient: TCnDockClient;
//begin
{  with Message do
  begin
    DockClient := FindDockClient(DockSource.Control);
    if DockClient <> nil then
    begin
      if (DockClient.LastDockSite is TCnDockPanel) then
      begin
        with TCnDockPanel(DockClient.LastDockSite) do
        begin
          if UseDockManager and (CnDockManager <> nil) then
            CnDockManager.RemoveControl(DockSource.Control);

        end;
      end else
      begin

      end;
    end;
  end;}
//end;

procedure TCnAdvDockPanel.CMUnDockClient(var Message: TCMUnDockClient);
var DockClient: TCnDockClient;
begin
  if IsLoading then Exit;
  with Message do
  begin
    Result := 0;
    if UseDockManager and (CnDockManager <> nil) then
    begin
      DockClient := FindDockClient(Client);
      if (NewTarget <> nil) or
        ((Client <> nil) and (csDestroying in Client.ComponentState)) then
      begin
        if DockClient <> nil then
          DockClient.LastDockSite := nil;
        CnDockManager.RemoveControl(Client);
      end else
      begin
        if (DockClient <> nil) then
          DockClient.LastDockSite := Self;
        CnDockManager.HideControl(Client);
      end;
    end;
  end;
end;

procedure TCnAdvDockPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited;
end;

function TCnAdvDockPanel.CustomUnDock(Source: TCnDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TCnAdvDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited;
end;

function TCnAdvDockPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

{ TCnSplitterStyle }

procedure TCnSplitterStyle.Assign(Source: TPersistent);
begin
  if Source is TCnSplitterStyle then
  begin
    Color := TCnSplitterStyle(Source).Color;
    Cursor := TCnSplitterStyle(Source).Cursor;
    ParentColor := TCnSplitterStyle(Source).ParentColor;
    ResizeStyle := TCnSplitterStyle(Source).ResizeStyle;
    Size := TCnSplitterStyle(Source).Size;
  end;
  inherited Assign(Source);
end;

procedure TCnSplitterStyle.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnSplitterStyle then
    with TCnSplitterStyle(Dest) do
    begin
      Color := Self.Color;
      Cursor := Self.Cursor;
      ParentColor := Self.ParentColor;
      ResizeStyle := Self.ResizeStyle;
      Size := Self.Size;
    end
  else inherited AssignTo(Dest);
end;

procedure TCnSplitterStyle.AssignToSplitter(Dest: TCnDockSplitter);
begin
  Dest.Color := Color;
  Dest.Cursor := Cursor;
  Dest.ParentColor := ParentColor;
  Dest.ResizeStyle := ResizeStyle;
  if Dest.Align in [alTop, alBottom] then
    Dest.Height := Size
  else Dest.Width := Size;
end;

constructor TCnSplitterStyle.Create(ASplitter: TCnDockSplitter; ACursor: TCursor);
begin
  inherited Create;
  FSplitter := ASplitter;
  Color := clBtnFace;
  Cursor := ACursor;
  ParentColor := False;
  ResizeStyle := rsPattern;
  FSize := 3;
  FMinSize := 30;
end;

procedure TCnSplitterStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ParentColor := False;
    if Assigned(FSplitter) then
      FSplitter.Color := Value;
  end;
end;

procedure TCnSplitterStyle.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  if Assigned(FSplitter) then
    FSplitter.Cursor := Value;
end;

procedure TCnSplitterStyle.SetMinSize(const Value: TCnSplitterSize);
begin
  FMinSize := Value;
  if Assigned(FSplitter) then
    FSplitter.MinSize := Value;
end;

procedure TCnSplitterStyle.SetParentColor(const Value: Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Value then FColor := FDockServer.ParentForm.Color;
    if Assigned(FSplitter) then
      FSplitter.ParentColor := Value;
  end;
end;

procedure TCnSplitterStyle.SetResizeStyle(const Value: TResizeStyle);
begin
  FResizeStyle := Value;
  if Assigned(FSplitter) then
    FSplitter.ResizeStyle := Value;
end;

procedure TCnSplitterStyle.SetSize(const Value: TCnSplitterSize);
begin
  FSize := Value;
  if Assigned(FSplitter) then
  begin
    if FSplitter.Align in [alTop, alBottom] then
      FSplitter.Height := Value
    else FSplitter.Width := Value;
  end;
end;

procedure TCnSplitterStyle.SetSplitterStyle;
begin
  AssignToSplitter(FSplitter);
end;

{ TCnDockBaseControl }

destructor TCnDockBaseControl.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if @FOldWindowProc <> nil then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
    if Assigned(FDockStyle) and not (FDockStyle is TCnBasicDockStyle) then
      FDockStyle.SetDockBaseControl(False, Self);
  end;
  if FDockStyle <> nil then
    FDockStyle.RemoveDockBaseControl(Self);
  inherited Destroy;
end;

constructor TCnDockBaseControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentForm := TForm(AOwner);
  FEnableDock := True;
  FLeftDock := True;
  FTopDock := True;
  FRightDock := True;
  FBottomDock := True;
  FEachOtherDock := True;
  FDockStyle := nil;
  if not (csDesigning in ComponentState) then
  begin
    FOldOnClose := FParentForm.OnClose;
    ParentForm.OnClose := DoFormOnClose;
    FOldOnCreate := FParentForm.OnCreate;
    ParentForm.OnCreate := DoFormOnCreate;
    { 保存老的窗口过程 }
    FOldWindowProc := FParentForm.WindowProc;
    { 重载窗口过程 }
    FParentForm.WindowProc := WindowProc;
  end;
end;

procedure TCnDockBaseControl.SetBottomDock(const Value: Boolean);
begin
  if CanSetBottomDocked then
    FBottomDock := Value;
end;

procedure TCnDockBaseControl.SetEachotherDock(const Value: Boolean);
begin
  if CanSetEachOtherDocked then
    FEachotherDock := Value;
end;

procedure TCnDockBaseControl.SetEnableDock(const Value: Boolean);
begin
  if CanSetEnableDocked then
    FEnableDock := Value;
end;

procedure TCnDockBaseControl.SetLeftDock(const Value: Boolean);
begin
  if CanSetLeftDocked then
    FLeftDock := Value;
end;

procedure TCnDockBaseControl.SetRightDock(const Value: Boolean);
begin
  if CanSetRightDocked then
    FRightDock := Value;
end;

procedure TCnDockBaseControl.SetTopDock(const Value: Boolean);
begin
  if CanSetTopDocked then
    FTopDock := Value;
end;

procedure TCnDockBaseControl.Assign(Source: TPersistent);
begin
  if Source is TCnDockBaseControl then
  begin
    FEnableDock := TCnDockBaseControl(Source).EnableDock;
    FLeftDock := TCnDockBaseControl(Source).LeftDock;
    FTopDock := TCnDockBaseControl(Source).TopDock;
    FRightDock := TCnDockBaseControl(Source).RightDock;
    FBottomDock := TCnDockBaseControl(Source).BottomDock;
    FEachOtherDock := TCnDockBaseControl(Source).EachOtherDock;
    FDockStyle := TCnDockBaseControl(Source).DockStyle;
    if FDockStyle <> nil then
      FDockStyle.AddDockBaseControl(Self);
  end else inherited Assign(Source);
end;

procedure TCnDockBaseControl.SetDockStyle(const Value: TCnBasicDockStyle);
begin
  { 根据原来的FDockStyle和输入的Value添加TCnDockBaseControl对象 }
  if FDockStyle <> nil then
    FDockStyle.RemoveDockBaseControl(Self);
  if Value <> nil then
    Value.AddDockBaseControl(Self);

  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    // 当前是设计期，就修改FDockStyle的值。
    if Value <> nil then
    begin
      Value.SetDockBaseControl(True, Self);
      // 调用FreeNotification函数来使一个窗体的TCnDockBaseControl设置DockStyle
      // 属性，这个DockStyle属性是另一个窗体上的控件，不会出现异常。
      Value.FreeNotification(ParentForm);
    end;
    FDockStyle := Value;
  end else
  begin
    // 在运行期不能设置DockStyle属性。
    ShowMessage(gs_CannotChangeDockStyleProperty);
  end;
end;

procedure TCnDockBaseControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  // 当DockStyle属性的控件插入或者释放的时候，会调用Notification函数。
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    // 如果是释放，就要把DockStyle属性设成nil。
    if AComponent = FDockStyle then
    begin
      FDockStyle.SetDockBaseControl(False, Self);
      FDockStyle := nil;
    end;
end;

procedure TCnDockBaseControl.Loaded;
begin
  if not (csDesigning in ComponentState) then
  begin
    // 默认的DockStyle属性是TCnBasicDockStyle类。
    if not Assigned(DockStyle) then
      DockStyle := TCnBasicDockStyle.Create(ParentForm);
    if Assigned(DockStyle) then
    begin
      DockStyle.SetDockBaseControl(True, Self);
    end;
  end;
  inherited Loaded;
end;

function TCnDockBaseControl.GetDockStyleVersion: string;
begin
  Result := gs_CnDockManagerVersion;
end;

procedure TCnDockBaseControl.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (Action = caFree) and (CnGlobalDockPresident <> nil) then
  begin
    { 把ParentForm添加到CnDockManager中 }
    if Self is TCnDockServer then
      CnGlobalDockPresident.RemoveDockServerFromDockManager(ParentForm)
    else if Self is TCnDockClient then
      CnGlobalDockPresident.RemoveDockClientFromDockManager(ParentForm);
  end;
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TCnDockBaseControl.DoFormOnCreate(Sender: TObject);
begin
  if CnGlobalDockPresident <> nil then
  begin
    { 从CnDockManager清除ParentForm }
    if Self is TCnDockServer then
      CnGlobalDockPresident.AddDockServerToDockManager(ParentForm)
    else if Self is TCnDockClient then
      CnGlobalDockPresident.AddDockClientToDockManager(ParentForm);
  end;
  if Assigned(FOldOnCreate) then
    FOldOnCreate(Sender);
end;

procedure TCnDockBaseControl.WindowProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FOldWindowProc(Message);
  end;
end;

procedure TCnDockBaseControl.SetParentComponent(Value: TComponent);
var ADockBaseControl: TCnDockBaseControl;
begin
  ADockBaseControl := FindDockBaseControl(ParentForm);
  if (Assigned(ADockBaseControl)) and (ADockBaseControl <> Self) then
    raise EInvalidOperation.Create(Format(gs_CannotLayAnother, [ADockBaseControl.ClassName, ClassName]));
  inherited SetParentComponent(Value);
end;

function TCnDockBaseControl.CanSetBottomDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetBottomDocked(Self);
end;

function TCnDockBaseControl.CanSetEachOtherDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetEachOtherDocked(Self);
end;

function TCnDockBaseControl.CanSetEnableDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetEnableDocked(Self);
end;

function TCnDockBaseControl.CanSetLeftDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetLeftDocked(Self);
end;

function TCnDockBaseControl.CanSetRightDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetRightDocked(Self);
end;

function TCnDockBaseControl.CanSetTopDocked: Boolean;
begin
  Result := True;
  if DockStyle <> nil then
    Result := DockStyle.CanSetTopDocked(Self);
end;

{ TCnDockServer }

constructor TCnDockServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateSplitterStyle;
end;

destructor TCnDockServer.Destroy;
begin
//  if not (csDesigning in ComponentState) then
//    DestroyDockPanelAndSplitter;
  DestroySplitterStyle;
  inherited Destroy;
end;

procedure TCnDockServer.CreateDockPanelAndSplitter;
var ControlList: TList;

  function CreatePanel(Align: TAlign; Name: string): TCnDockPanel;
  begin
    if (FDockPanelClass <> nil) and
      (FDockPanelClass <> TCnDockPanelClass(ClassType)) then
    begin
      Result := FDockPanelClass.Create(Owner);
      Result.Parent := ParentForm;
      Result.Name := Name;
      Result.Caption := '';
      Result.Align := Align;
      Result.DockServer := Self;
      Result.ResetPosition;
      if Align in [alTop, alBottom] then
        Result.Height := 0
      else
        Result.Width := 0;
      SetDockSite(Result, True);
      { 调用DockStyle的AssignConjoinServerOption函数重新设置平铺属性 }
      if DockStyle <> nil then
        DockStyle.AssignConjoinServerOption(Result);
    end else Result := nil;
  end;

  function CreateSplitter(Align: TAlign; Name: string): TCnDockSplitter;
  begin
    if (FDockSplitterClass <> nil) and
      (FDockSplitterClass <> TCnDockSplitterClass(ClassType)) then
    begin
      Result := FDockSplitterClass.Create(Owner);
      Result.Parent := ParentForm;
      Result.Name := Name;
      Result.Visible := False;
      Result.Align := Align;
      Result.DockServer := Self;
    end else Result := nil;
  end;

begin
  ControlList := TList.Create;
  try
    FLeftDockPanel    := CreatePanel(alLeft, 'LeftDockPanel_A_B_C_D_E_F_G');
    FLeftSplitter     := CreateSplitter(alLeft, 'LeftSplitter_A_B_C_D_E_F_G');
    FRightDockPanel   := CreatePanel(alRight, 'RightDockPanel_A_B_C_D_E_F_G');
    FRightSplitter    := CreateSplitter(alRight, 'RightSplitter_A_B_C_D_E_F_G');
    FTopDockPanel     := CreatePanel(alTop, 'TopDockPanel_A_B_C_D_E_F_G');
    FTopSplitter      := CreateSplitter(alTop, 'TopSplitter_A_B_C_D_E_F_G');
    FBottomDockPanel  := CreatePanel(alBottom, 'BottomDockPanel_A_B_C_D_E_F_G');
    FBottomSplitter   := CreateSplitter(alBottom, 'BottomSplitter_A_B_C_D_E_F_G');
  finally
    ControlList.Free;
  end;
end;

procedure TCnDockServer.CreateSplitterStyle;
begin
  FLeftSplitterStyle    := TCnSplitterStyle.Create(FLeftSplitter, crHSplit);
  FTopSplitterStyle     := TCnSplitterStyle.Create(FTopSplitter, crVSplit);
  FRightSplitterStyle   := TCnSplitterStyle.Create(FRightSplitter, crHSplit);
  FBottomSplitterStyle  := TCnSplitterStyle.Create(FBottomSplitter, crVSplit);

  FLeftSplitterStyle.FDockServer := Self;
  FTopSplitterStyle.FDockServer := Self;
  FRightSplitterStyle.FDockServer := Self;
  FBottomSplitterStyle.FDockServer := Self;
end;

procedure TCnDockServer.DestroySplitterStyle;
begin
  FLeftSplitterStyle.Free;
  FTopSplitterStyle.Free;
  FRightSplitterStyle.Free;
  FBottomSplitterStyle.Free;
end;

procedure TCnDockServer.SetLeftSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FLeftSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetTopSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FTopSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetRightSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FRightSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetBottomSplitterStyle(
  const Value: TCnSplitterStyle);
begin
  FBottomSplitterStyle.Assign(Value);
end;

procedure TCnDockServer.SetSplitterStyle;
begin
  LeftSplitterStyle.Splitter := FLeftSplitter;
  LeftSplitterStyle.SetSplitterStyle;
  TopSplitterStyle.Splitter := FTopSplitter;
  TopSplitterStyle.SetSplitterStyle;
  RightSplitterStyle.Splitter := FRightSplitter;
  RightSplitterStyle.SetSplitterStyle;
  BottomSplitterStyle.Splitter := FBottomSplitter;
  BottomSplitterStyle.SetSplitterStyle;
end;

procedure TCnDockServer.WindowProc(var Message: TMessage);
begin
  { 执行FDockStyle的消息处理 }
  if Assigned(FDockStyle) then
  begin
    if FDockStyle.DockServerWindowProc(Self, Message) then
      { 如果DockStyle已经执行了消息处理，就不执行默认的处理 }
      Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    if Message.Msg = WM_ACTIVATE then
      WMActivate(TWMActivate(Message));
  end;
  inherited WindowProc(Message);
end;

function TCnDockServer.GetDockPanel(Index: Integer): TCnDockPanel;
begin
  Result := nil;
  case Index of
    0: Result := FTopDockPanel;
    1: Result := FBottomDockPanel;
    2: Result := FLeftDockPanel;
    3: Result := FRightDockPanel;
  end;
end;

function TCnDockServer.GetDockSplitter(Index: Integer): TCnDockSplitter;
begin
  Result := nil;
  case Index of
    0: Result := FTopSplitter;
    1: Result := FBottomSplitter;
    2: Result := FLeftSplitter;
    3: Result := FRightSplitter;
  end;
end;

procedure TCnDockServer.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(BottomDockPanel);
  inherited SetBottomDock(Value);
end;

procedure TCnDockServer.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(TopDockPanel);
    DoFloatDockClients(BottomDockPanel);
    DoFloatDockClients(LeftDockPanel);
    DoFloatDockClients(RightDockPanel);
  end;
  inherited SetEnableDock(Value);
end;

procedure TCnDockServer.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(LeftDockPanel);
  inherited SetLeftDock(Value);
end;

procedure TCnDockServer.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(RightDockPanel);
  inherited SetRightDock(Value);
end;

procedure TCnDockServer.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(TopDockPanel);
  inherited SetTopDock(Value);
end;

procedure TCnDockServer.DoFloatDockClients(DockPanel: TCnDockPanel);
var
  i: Integer;
  ADockClient: TCnDockClient;
begin
  if not (csDesigning in ComponentState) and (DockPanel <> nil) then
  begin
    for i := DockPanel.DockClientCount - 1 downto 0 do
    begin
      ADockClient := FindDockClient(DockPanel.DockClients[i]);
      if ADockClient <> nil then
        ADockClient.RestoreChild;
    end;
  end;
end;

procedure TCnDockServer.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnDockServerName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnDockServerComment;
end;

procedure TCnDockServer.Loaded;
begin
  if Assigned(DockStyle) and Assigned(DockStyle.CnDockPanelClass) then
    FDockPanelClass := DockStyle.CnDockPanelClass
  else FDockPanelClass := DefaultDockPanelClass;
  { ------------------------------------------------------------------------ }
  if Assigned(DockStyle) and Assigned(DockStyle.CnDockSplitterClass) then
    FDockSplitterClass := DockStyle.CnDockSplitterClass
  else FDockSplitterClass := DefaultDockSplitterClass;
  { ------------------------------------------------------------------------ }
  if not (csDesigning in ComponentState) then
  begin
    CreateDockPanelAndSplitter;
    SetSplitterStyle;
  end;
  inherited Loaded;
end;

procedure TCnDockServer.WMActivate(var Message: TWMActivate);
var i: Integer;
  Control: TWinControl;
begin
  if (Message.Active = WA_INACTIVE) then
  begin
    for i := 0 to 3 do
      DockPanel[i].CnDockManager.ActiveControl := nil;
  end else
  begin
    Control := GetActiveControl(ParentForm);
    for i := 0 to 3 do
    begin
      if GetHostDockParent(Control) = DockPanel[i] then
      begin
        DockPanel[i].CnDockManager.ActiveControl := Control;
        if Control.CanFocus then
          Control.SetFocus;
      end;
    end;
  end;
end;

procedure TCnDockServer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TCnDockServer.DoGetClientAlignControl(Align: TAlign; var Value: Integer);
begin
  if Assigned(FOnGetClientAlignSize) then
    FOnGetClientAlignSize(Align, Value);
end;

procedure TCnDockServer.DoFinishSetDockPanelSize(DockPanel: TCnDockPanel);
begin
  if Assigned(FOnFinishSetDockPanelSize) then
    FOnFinishSetDockPanelSize(DockPanel);
end;

function TCnDockServer.GetClientAlignControl(Align: TAlign): Integer;
begin
  Result := GetClientAlignControlArea(ParentForm, Align);
  DoGetClientAlignControl(Align, Result);
end;

function TCnDockServer.GetDockPanelWithAlign(Index: TAlign): TCnDockPanel;
begin
  Result := nil;
  case Index of
    alLeft:   Result := FLeftDockPanel;
    alRight:  Result := FRightDockPanel;
    alTop:    Result := FTopDockPanel;
    alBottom: Result := FBottomDockPanel;
  end;
end;

function TCnDockServer.GetDockSplitterWithAlign(
  Index: TAlign): TCnDockSplitter;
begin
  Result := nil;
  case Index of
    alLeft:   Result := FLeftSplitter;
    alRight:  Result := FRightSplitter;
    alTop:    Result := FTopSplitter;
    alBottom: Result := FBottomSplitter;
  end;
end;

{ TCnDockClient }

constructor TCnDockClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentVisible := ParentForm.Visible;
  ParentForm.DragKind := dkDock;
  ParentForm.DragMode := dmAutomatic;
  ParentForm.UseDockManager := False;
  if not (ParentForm is TCnDockableForm) then
    SetDockSite(ParentForm, True);
  LRDockWidth := 100;
  TBDockHeight := 100;
  if GlobalDockClient = nil then
    GlobalDockClient := Self;
  FDirectDrag := False;
  FShowHint := True;
  FCanFloat := True;
  FRelativeServer := nil;
  FDockLevel := 0;
  EnableCloseBtn := True;
end;

destructor TCnDockClient.Destroy;
begin
  if not (ParentForm is TCnDockableForm) then
    SetDockSite(ParentForm, False);
  ParentForm.DragKind := dkDrag;
  ParentForm.DragMode := dmManual;
  inherited Destroy;
end;

procedure TCnDockClient.Assign(Source: TPersistent);
begin
  if Source is TCnDockClient then
  begin
    FConjoinPanelClass := TCnDockClient(Source).FConjoinPanelClass;
    FTabDockClass := TCnDockClient(Source).FTabDockClass;
    FParentVisible := TCnDockClient(Source).FParentVisible;
    FNCPopupMenu := TCnDockClient(Source).FNCPopupMenu;
    FDirectDrag := TCnDockClient(Source).FDirectDrag;
    FShowHint := TCnDockClient(Source).FShowHint;
    FCanFloat := TCnDockClient(Source).FCanFloat;
    FRelativeServer := TCnDockClient(Source).FRelativeServer;
    FDockLevel := TCnDockClient(Source).DockLevel;
  end;
  inherited Assign(Source);
end;

procedure TCnDockClient.WindowProc(var Message: TMessage);
var OldOrient: TDockOrientation;
begin
  { 执行FDockStyle的消息处理 }
  if Assigned(FDockStyle) then
  begin
    if FDockStyle.DockClientWindowProc(Self, Message) then
      { 如果DockStyle已经执行了消息处理，就不执行默认的处理 }
      Exit;
  end;
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      CM_SHOWINGCHANGED:
      begin
        { 在WinXP中，当在装载的时候，会出现闪烁，在这里把显示改变的消息捕获掉。
          在98或者2000中是没有这种情况的，因为已经调用了LockWindowUpdate函数，
          但是在XP下面不知道是什么原因，还是会出现这种闪烁的情况，没办法，只能
          在装载的时候先过滤掉CM_SHOWINGCHANGED消息，然后在装载的最后把应该显示
          但是没有显示的客户窗体显示出来。}
        if IsWinXP and IsLoading then
          Exit;
      end;
      WM_NCLBUTTONDOWN:
      begin
        // 如果捕获到WM_NCLBUTTONDOWN消息，就调用WMNCLButtonDown函数。
        WMNCLButtonDown(TWMNCHitMessage(Message));
        if (Message.Result = 1) then
          Exit;
      end;
      WM_NCLBUTTONUP:
        WMNCLButtonUp(TWMNCHitMessage(Message));
      WM_NCLBUTTONDBLCLK:
        WMNCLButtonDblClk(TWMNCHitMessage(Message));
      WM_NCMBUTTONDOWN:
        WMNCMButtonDown(TWMNCHitMessage(Message));
      WM_NCMBUTTONUP:
        WMNCMButtonUp(TWMNCHitMessage(Message));
      WM_NCMBUTTONDBLCLK:
        WMNCMButtonDblClk(TWMNCHitMessage(Message));
      WM_NCRBUTTONDOWN:
      begin
        WMNCRButtonDown(TWMNCHitMessage(Message));
        { 如果有自定义的弹出菜单，就不要调用系统默认的弹出菜单，
          要实现这个功能，只要不把这个消息发送给Windows就可以办到。 }
        if FNCPopupMenu <> nil then
          Exit;
      end;
      WM_NCRBUTTONUP:
        WMNCRButtonUp(TWMNCHitMessage(Message));
      WM_NCRBUTTONDBLCLK:
        WMNCRButtonDblClk(TWMNCHitMessage(Message));
      WM_NCMOUSEMOVE:
        WMNCMOUSEMOVE(TWMNCHitMessage(Message));

      WM_SIZE:
        WMSize(TWMSize(Message));
      WM_ACTIVATE:
        WMActivate(TWMActivate(Message));
      WM_WINDOWPOSCHANGED:
      begin
        //下面一条代码的作用是使TControl.CalcDockSizes方法失效,
        //请参看Controls.pas文件中的4462--4466行(D5版本),或者4816--4820行(D6版本)
        ParentForm.ControlState := ParentForm.ControlState + [csDocking];
        OldOrient := ParentForm.DockOrientation;
        ParentForm.DockOrientation := doNoOrient;
        try
          inherited WindowProc(Message);
        finally
          ParentForm.ControlState := ParentForm.ControlState - [csDocking];
          ParentForm.DockOrientation := OldOrient;
        end;
        Exit;
      end;
      CM_ENTER:
        Activate;
      CM_EXIT:
        Deactivate;
      CM_VISIBLECHANGED:
        CMVisibleChanged(Message);
    end;
  end;
  inherited WindowProc(Message);
end;

{ 创建平铺的服务窗体并且把控件停靠进去 }
function TCnDockClient.CreateConjoinHostAndDockControl(
  Control1, Control2: TControl; DockType: TAlign): TCnConjoinDockHostForm;
var APanel: TCnConjoinPanel;
  OldDockWidth, OldDockHeight: Integer;
begin
  { 创建一个TCnConjoinDockHostForm窗体作为TCnConjoinPanel的Parent }
  Result := TCnConjoinDockHostForm.Create(Application);
  { 创建一个TCnConjoinPanel，并且把它放在Result上 }
  APanel := CreateConjoinPanelClass(Result);
  { Result的位置大小和Control1的相同，所以在调用CreateConjoinHostAndDockControl
    的时候要注意：Control1为停靠目标窗体，Control2为停靠源窗体 }
  Result.BoundsRect := Control1.BoundsRect;
  { 调整Result的宽度和高度为UndockWidth和UndockHeight }
  Result.Width := Control1.UndockWidth;
  Result.Height := Control1.UndockHeight;
  { 分别把Control1和Control2停靠进APage }
  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(APanel, nil, alNone);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;
  {============================================================================}
  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(APanel, nil, DockType);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;
  { 把Result的DockSite属性设置成False }
  SetDockSite(Result, False);
end;

{ 创建分页的服务窗体并且把控件停靠进去 }
function TCnDockClient.CreateTabHostAndDockControl(
  Control1, Control2: TControl): TCnTabDockHostForm;
var APage: TCnTabPageControl;
  OldDockWidth, OldDockHeight: Integer;
begin
  { 创建一个TCnTabDockHostForm窗体作为TCnTabPageControl的Parent }
  Result := TCnTabDockHostForm.Create(Application);
  { 创建一个TCnTabPageControl，并且把它放在Result上 }
  APage := CreateTabDockClass(Result);
  { Result的位置大小和Control1的相同，所以在调用CreateTabHostAndDockControl
    的时候要注意：Control1为停靠目标窗体，Control2为停靠源窗体 }
  Result.BoundsRect := Control1.BoundsRect;
  { 调整Result的宽度和高度为UndockWidth和UndockHeight }
  Result.Width := Control1.UndockWidth;
  Result.Height := Control1.UndockHeight;
  { 分别把Control1和Control2停靠进APage }
  OldDockWidth := Control1.LRDockWidth;
  OldDockHeight := Control1.TBDockHeight;
  Control1.ManualDock(APage, nil, alClient);
  Control1.LRDockWidth := OldDockWidth;
  Control1.TBDockHeight := OldDockHeight;
  {============================================================================}
  OldDockWidth := Control2.LRDockWidth;
  OldDockHeight := Control2.TBDockHeight;
  Control2.ManualDock(APage, nil, alClient);
  Control2.LRDockWidth := OldDockWidth;
  Control2.TBDockHeight := OldDockHeight;
  { 把Result的DockSite属性设置成False }
  SetDockSite(Result, False);
end;

procedure TCnDockClient.MakeHideEvent;
begin
  ParentVisible := False;
  if Assigned(FOnFormHide) then
    FOnFormHide(Self);
end;

procedure TCnDockClient.MakeShowEvent;
begin
  if {(not ParentVisible) and }ParentForm.Visible then
  begin
    if Assigned(FOnFormShow) then
      FOnFormShow(Self);
    ParentVisible := True;
  end;
end;

procedure TCnDockClient.SetParentVisible(const Value: Boolean);
begin
  FParentVisible := Value;
end;

procedure TCnDockClient.DoFloatDockClients(PanelAlign: TAlign);
begin
  if (ParentForm.HostDockSite is TCnDockPanel) and
    (PanelAlign = ParentForm.HostDockSite.Align) then
    RestoreChild;
end;

procedure TCnDockClient.SetBottomDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alBottom);
  inherited SetBottomDock(Value);
end;

procedure TCnDockClient.SetEachotherDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockEachOther;
  inherited SetEachotherDock(Value);
end;

procedure TCnDockClient.SetEnableDock(const Value: Boolean);
begin
  if not Value then
  begin
    DoFloatDockClients(alTop);
    DoFloatDockClients(alBottom);
    DoFloatDockClients(alLeft);
    DoFloatDockClients(alRight);
    DoFloatDockEachOther;
  end;
  if ParentForm <> nil then
  begin
    if Value then
      ParentForm.DragKind := dkDock
    else
      ParentForm.DragKind := dkDrag;
  end;
  inherited SetEnableDock(Value);
end;

procedure TCnDockClient.SetLeftDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alLeft);
  inherited SetLeftDock(Value);
end;

procedure TCnDockClient.SetRightDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alRight);
  inherited SetRightDock(Value);
end;

procedure TCnDockClient.SetTopDock(const Value: Boolean);
begin
  if not Value then
    DoFloatDockClients(alTop);
  inherited SetTopDock(Value);
end;

procedure TCnDockClient.DoFloatDockEachOther;
begin
  if (ParentForm.HostDockSite <> nil) and
    (ParentForm.HostDockSite.Parent is TCnDockableForm) then
    RestoreChild;
end;

procedure TCnDockClient.WMSize(var Message: TWMSize);
begin
{  if (ParentForm.HostDockSite <> nil) and (Message.Width * Message.Height <> 0) then
  begin
    if ParentForm.DockOrientation = doVertical then // 垂直
      ParentForm.LRDockWidth := Message.Width
    else if ParentForm.DockOrientation = doHorizontal then
      ParentForm.TBDockHeight := Message.Height
    else
    begin
      if (ParentForm.HostDockSite is TCnDockPanel) and
        (ParentForm.HostDockSite.FindChildControl(ParentForm.Name) <> nil) then
      begin
        if ParentForm.HostDockSite.Align in [alLeft, alRight] then
          ParentForm.LRDockWidth := Message.Width
        else
          ParentForm.TBDockHeight := Message.Height;
      end;
    end;
  end;}
  inherited;
end;

procedure TCnDockClient.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnDockClientName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnDockClientComment;
end;

procedure TCnDockClient.Loaded;
begin
  { 设置FConjoinPanelClass }
  if Assigned(DockStyle) and Assigned(DockStyle.CnConjoinPanelClass) then
    FConjoinPanelClass := DockStyle.CnConjoinPanelClass
  else FConjoinPanelClass := DefaultConjoinPanelClass;
  { 设置FTabDockClass }
  if Assigned(DockStyle) and Assigned(DockStyle.CnTabDockClass) then
    FTabDockClass := DockStyle.CnTabDockClass
  else FTabDockClass := DefaultTabDockClass;
  inherited Loaded; 
end;

function TCnDockClient.CreateConjoinPanelClass(
  ConjoinHost: TForm): TCnConjoinPanel;
begin
  Result := nil;
  TCnConjoinDockHostForm(ConjoinHost).DockClient.Assign(Self);
  if (FConjoinPanelClass <> nil) and
    (FConjoinPanelClass <> TCnConjoinPanelClass(ClassType)) then
  begin
    Result := FConjoinPanelClass.Create(ConjoinHost);
    Result.Align := alClient;
    TCnConjoinDockHostForm(ConjoinHost).DockableControl := Result;
    TCnConjoinDockHostForm(ConjoinHost).Panel := Result;
    SetDockSite(Result, True);
    { 调用DockStyle.AssignConjoinServerOption来设置一些在DockStyle中设置的属性 }
    DockStyle.AssignConjoinServerOption(TCnConjoinDockHostForm(ConjoinHost).Panel);
  end;
end;

function TCnDockClient.CreateTabDockClass(
  TabHost: TForm): TCnTabPageControl;
begin
  Result := nil;
  TCnTabDockHostForm(TabHost).DockClient.Assign(Self);
  if (FTabDockClass <> nil) and
    (FTabDockClass <> TCnTabDockClass(ClassType)) then
  begin
    Result := FTabDockClass.Create(TabHost);
    Result.Align := alClient;
    TCnTabDockHostForm(TabHost).DockableControl := Result;
    TCnTabDockHostForm(TabHost).PageControl := Result;
    SetDockSite(Result, True);
    { 调用DockStyle.AssignTabServerOption来设置一些在DockStyle中设置的属性 }
    DockStyle.AssignTabServerOption(TCnTabDockHostForm(TabHost).PageControl);
  end;
end;

procedure TCnDockClient.WMActivate(var Message: TWMActivate);
begin
  if (ParentForm is TCnConjoinDockHostForm) then
  begin
    if (Message.Active = WA_INACTIVE) then
      TCnConjoinPanel(TCnConjoinDockHostForm(ParentForm).Panel).CnDockManager.ActiveControl := nil
    else
    begin
      TCnConjoinPanel(TCnConjoinDockHostForm(ParentForm).Panel).CnDockManager.ActiveControl :=
        GetActiveControl(ParentForm);
    end;
  end;
end;

procedure TCnDockClient.Activate;
begin
  if ParentForm.HostDockSite is TCnCustomDockPanel then
  begin
    TCnCustomDockPanel(
      ParentForm.HostDockSite).CnDockManager.ActiveControl := ParentForm;
  end;
end;

procedure TCnDockClient.Deactivate;
begin
  if ParentForm.HostDockSite is TCnCustomDockPanel then
  begin
    if TCnCustomDockPanel(ParentForm.HostDockSite).CnDockManager <> nil then
      TCnCustomDockPanel(ParentForm.HostDockSite).CnDockManager.ActiveControl := nil;
  end;
end;

procedure TCnDockClient.DoFormOnClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Action = caHide then
  begin
    HideDockForm(ParentForm);
    FParentVisible := True;
  end;
end;

procedure TCnDockClient.FormDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormDockDrop(Self, Source, X, Y);
end;

procedure TCnDockClient.FormDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  FormPositionDockRect(Source);
  if Assigned(DockStyle) then
    DockStyle.FormDockOver(Self, Source, X, Y, State, Accept);
end;

procedure TCnDockClient.FormEndDock(Target: TObject; X, Y: Integer);
begin
  if Assigned(DockStyle) then
    DockStyle.FormEndDock(Self, Target, X, Y);
end;

procedure TCnDockClient.FormPositionDockRect(Source: TCnDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormPositionDockRect(Self, Source);
end;

procedure TCnDockClient.FormStartDock(var Source: TCnDragDockObject);
begin
  if Assigned(DockStyle) then
    DockStyle.FormStartDock(Self, Source);
end;

function TCnDockClient.FormUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  if Assigned(DockStyle) then
    Result := DockStyle.FormUnDock(Self, Newtarget, Client)
  else Result := False;
end;

procedure TCnDockClient.FormGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(ParentForm.Handle, InfluenceRect);
  InflateRect(InfluenceRect, -4, -4);
  if Assigned(DockStyle) then
    DockStyle.FormGetSiteInfo(Source, Self, Client, InfluenceRect, MousePos, CanDock);
end;

function TCnDockClient.GetLRDockWidth: Integer;
begin
  Result := ParentForm.LRDockWidth;
end;

function TCnDockClient.GetTBDockHeight: Integer;
begin
  Result := ParentForm.TBDockHeight;
end;

procedure TCnDockClient.SetLRDockWidth(const Value: Integer);
begin
  if ParentForm.LRDockWidth <> Value then
    ParentForm.LRDockWidth := Value;
end;

procedure TCnDockClient.SetTBDockHeight(const Value: Integer);
begin
  if ParentForm.TBDockHeight <> Value then
    ParentForm.TBDockHeight := Value;
end;

procedure TCnDockClient.DoNCButtonDown(Message: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TMouseStation);
begin
  if Assigned(FOnNCButtonDown) then
    FOnNCButtonDown(Self, Button, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
end;

procedure TCnDockClient.DoNCButtonUp(Message: TWMNCHitMessage;
  Button: TMouseButton; MouseStation: TMouseStation);
begin
  if Assigned(FOnNCButtonUp) then
    FOnNCButtonUp(Self, Button, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
  if Button = mbRight then
    { 如果是鼠标右键，就弹出菜单 }
    DoMenuPopup(Message.XCursor, Message.YCursor);
end;

procedure TCnDockClient.DoNCMouseMove(Message: TWMNCHitMessage;
  MouseStation: TMouseStation);
begin
  if Assigned(FOnNCMouseMove) then
    FOnNCMouseMove(Self, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
end;

procedure TCnDockClient.DoNCButtonDblClk(Message: TWMNCHitMessage; Button: TMouseButton;
  MouseStation: TMouseStation);
begin
  if Assigned(FOnNCButtonDblClk) then
    FOnNCButtonDblClk(Self, Button, Message.XCursor, Message.YCursor,
      Message.HitTest, MouseStation);
end;

procedure TCnDockClient.WMNCLButtonDown(var Message: TWMNCHitMessage);
begin
  DoNCButtonDown(Message, mbLeft, msFloat);

  GlobalDockClient := Self;
  
  if (Message.HitTest = HTCAPTION) and (ParentForm.DragKind = dkDock) and not
    (csDesigning in ComponentState) and not IsIconic(ParentForm.Handle) then
  begin
      { Activate window since we override WM_NCLBUTTON behavior }
      SetWindowPos(ParentForm.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_NOMOVE or
        SWP_NOSIZE);
      PostMessage(ParentForm.Handle, WM_NCLBUTTONUP, TMessage(Message).WParam,
        TMessage(Message).LParam);
      if ParentForm.Active then
        CnGlobalDockPresident.BeginDrag(ParentForm, DirectDrag, Integer(DirectDrag) * 2 - 1);
      Message.Result := 1;
  end else
    Message.Result := 0;
end;

procedure TCnDockClient.WMNCLButtonUp(var Message: TWMNCHitMessage);
begin
  DoNCButtonUp(Message, mbLeft, msFloat);
end;

procedure TCnDockClient.WMNCMButtonDown(var Message: TWMNCHitMessage);
begin
  DoNCButtonDown(Message, mbMiddle, msFloat);
end;

procedure TCnDockClient.WMNCMButtonUp(var Message: TWMNCHitMessage);
begin
  DoNCButtonUp(Message, mbMiddle, msFloat);
end;

procedure TCnDockClient.WMNCRButtonDown(var Message: TWMNCHitMessage);
begin
  DoNCButtonDown(Message, mbRight, msFloat);
end;

procedure TCnDockClient.WMNCRButtonUp(var Message: TWMNCHitMessage);
begin
  DoNCButtonUp(Message, mbRight, msFloat);
end;

procedure TCnDockClient.WMNCMOUSEMOVE(var Message: TWMNCHitMessage);
begin
  DoNCMouseMove(Message, msFloat);
end;

procedure TCnDockClient.WMNCLButtonDblClk(var Message: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Message, mbLeft, msFloat);
end;

procedure TCnDockClient.WMNCMButtonDblClk(var Message: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Message, mbMiddle, msFloat);
end;

procedure TCnDockClient.WMNCRButtonDblClk(var Message: TWMNCHitMessage);
begin
  DoNCButtonDblClk(Message, mbRight, msFloat);
end;

procedure TCnDockClient.SetNCPopupMenu(const Value: TPopupMenu);
begin
  FNCPopupMenu := Value;
end;

procedure TCnDockClient.DoMenuPopup(X, Y: Integer);
begin
  if FNCPopupMenu <> nil then
  begin
    FNCPopupMenu.PopupComponent := ParentForm;
    FNCPopupMenu.Popup(X, Y);
  end;
end;

procedure TCnDockClient.DoPaintDockGrabber(
  Canvas: TCanvas; Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockGrabber) then
    FOnPaintDockGrabber(Canvas, Control, ARect);
end;

procedure TCnDockClient.DoPaintDockSplitter(Canvas: TCanvas;
  Control: TControl; const ARect: TRect);
begin
  if Assigned(FOnPaintDockSplitter) then
    FOnPaintDockSplitter(Canvas, Control, ARect);
end;

procedure TCnDockClient.FormGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
  if Assigned(DockStyle) then
    DockStyle.FormGetDockEdge(Self, Source, MousePos, DropAlign)
  else DropAlign := alNone;
end;

procedure TCnDockClient.DoFormShowHint(HTFlag: Integer; var HintStr: string;
  var CanShow: Boolean);
begin
  if Assigned(FOnFormShowHint) then
   FOnFormShowHint(HTFlag, HintStr, CanShow);
end;

procedure TCnDockClient.SetCurrentDockSite(const Value: TWinControl);
begin
  FCurrentDockSite := Value;
end;

procedure TCnDockClient.SetLastDockSite(const Value: TWinControl);
begin
  FLastDockSite := Value;
end;

procedure TCnDockClient.SetVSPaneWidth(const Value: Integer);
begin
  FVSPaneWidth := Value;
end;

procedure TCnDockClient.RestoreChild;
begin
  DockStyle.RestoreClient(Self);
end;

procedure TCnDockClient.SetUnDockLeft(const Value: Integer);
begin
  FUnDockLeft := Value;
end;

procedure TCnDockClient.SetUnDockTop(const Value: Integer);
begin
  FUnDockTop := Value;
end;

procedure TCnDockClient.HideParentForm;
begin
  HideDockForm(ParentForm);
end;

procedure TCnDockClient.ShowParentForm;
begin
  ShowDockForm(ParentForm);
end;

function TCnDockClient.GetDockState: Integer;
begin
  Result := DS_Unknow;
  if DockStyle <> nil then
    Result := DockStyle.GetDockState(Self);
end;

procedure TCnDockClient.CMVisibleChanged(var Message: TMessage);
begin
end;

procedure TCnDockClient.SetCanFloat(const Value: Boolean);
begin
  FCanFloat := Value;
end;

procedure TCnDockClient.SetRelativeServer(const Value: TCnDockServer);
begin
  if (csDesigning in ComponentState){ or (csLoading in ComponentState) }then
  begin
    if Value <> nil then
      Value.FreeNotification(ParentForm);
  end;
  FRelativeServer := Value;
end;

procedure TCnDockClient.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRelativeServer) then
    // 如果是释放，就要把FRelativeServer属性设成nil。
    FRelativeServer := nil;
end;

procedure TCnDockClient.SetDockLevel(const Value: Integer);
begin
  if not ParentForm.Floating then
  begin
    if FDockLevel <> Value then
      DoFloatForm(ParentForm);
  end;
  FDockLevel := Value;
end;

procedure TCnDockClient.SetEnableCloseBtn(const Value: Boolean);
begin
  FEnableCloseBtn := Value;
end;

{ TCnDockableForm }

constructor TCnDockableForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragKind := dkDock;
  FDockClient := TCnDockClient.Create(self);
  CnGlobalDockPresident.DockableFormList.Add(Self);
  FFloatingChild := nil;
  TBDockHeight := FDockClient.TBDockHeight;
  LRDockWidth := FDockClient.LRDockWidth;
end;

destructor TCnDockableForm.Destroy;
var Index: Integer;
begin
  if CnGlobalDockPresident <> nil then
  begin
    Index := CnGlobalDockPresident.DockableFormList.IndexOf(Self);
    if Index <> -1 then
      CnGlobalDockPresident.DockableFormList.Delete(Index);
  end;
  if DockClient.LastDockSite is TCnDockPanel then
    TCnDockPanel(DockClient.LastDockSite).CnDockManager.RemoveControl(Self);
  inherited Destroy;
  FFloatingChild := nil;
end;

procedure TCnDockableForm.DoClose(var Action: TCloseAction);
var i: Integer;
begin
  if DockableControl.DockClientCount = 1 then
  begin
    { 保存这个客户,可能以后还要重新停靠到服务器上面 }
    FFloatingChild := DockableControl.DockClients[0];
    { 如果HostDockSite不为空,说明以后要把它停靠到服务器上面,为了防止闪烁,先把
      FFloatingChild隐藏掉,然后等到重新停靠后再显示出来,就可以防止闪烁,显示的
      函数见CnDockSupportControl.pas的TCnCustomDockPanel.WndProc函数 }
    if HostDockSite <> nil then
      FFloatingChild.Visible := False;
    { 如果停靠客户的个数为1，就把她拖出，并且释放本身 }
    DoFloat(Self, DockableControl.DockClients[0]);
    Action := caFree;
  end else if DockableControl.DockClientCount = 0 then
    { 如果已经没有停靠客户，就把窗体释放掉 }
    Action := caFree
  else
  begin
    { 否则就隐藏 }
    Action := caHide;
    { 如果查找到DockableControl中的客户有一个是FUnDockControl，就不隐藏自己 }
    if (FUnDockControl <> nil) and (DockableControl.DockClientCount = 2) then
    begin
      for i := 0 to DockableControl.DockClientCount - 1 do
        if FUnDockControl = DockableControl.DockClients[i] then
        begin
          Action := caNone;
          Break;
        end;
    end;
  end;
  if (HostDockSite is TCnDockPanel) and (HostDockSite.VisibleDockClientCount = 1) and
    (FFloatingChild = nil) then
    // 如果停靠服务器是TDockServer中的DockPanel,就调用ShowDockPanel函数。
    TCnDockPanel(HostDockSite).ShowDockPanel(False, Self);
    
  inherited DoClose(Action);
  FUnDockControl := nil;
end;

function TCnDockableForm.GetDockableControl: TWinControl;
begin
  Result := FDockableControl;
end;

procedure TCnDockableForm.SetDockableControl(const Value: TWinControl);
begin
  FDockableControl := Value;
end;

procedure TCnDockableForm.SetUnDockControl(const Value: TControl);
begin
  FUnDockControl := Value;
end;

{ TCnConjoinDockHostForm }

constructor TCnConjoinDockHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := ConjoinDockHostBorderStyle;
end;

procedure TCnConjoinDockHostForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
end;

{ TCnTabDockHostForm }

constructor TCnTabDockHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := TabDockHostBorderStyle;
end;

destructor TCnTabDockHostForm.Destroy;
begin
  inherited;
end;

function TCnTabDockHostForm.GetActiveDockForm: TForm;
begin
  if PageControl.ActivePage.ControlCount = 1 then
    Result := TForm(PageControl.ActivePage.Controls[0])
  else Result := nil;
end;

{ TCnConjoinPanel }

constructor TCnConjoinPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  DockClient := TCnConjoinDockHostForm(AOwner).DockClient;
  Align := alClient;
  BevelOuter := bvNone;
  DoubleBuffered := True;
  ParentFont := False;
  Caption := '';
end;

procedure TCnConjoinPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      { 下面两行代码是为了防止窗体的标题栏和窗体上面的控件的内容被清空 }
      ParentForm.ActiveControl := nil;
      TForm(Source.Control).ActiveControl := nil;
      { ------------------------------------------------------------------------ }
      SetDockSite(TForm(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

procedure TCnConjoinPanel.DoDockOver(Source: TDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  { 如果停靠客户是TDockableForm，并且停靠客户上面有TDockClient控件，就同意停靠 }
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

function TCnConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;
  { 如果停靠客户是TDockableForm，就还原停靠客户的DockSite属性为True }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2){ and (NewTarget <> Self) }then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

function TCnConjoinPanel.GetParentForm: TCnConjoinDockHostForm;
begin
  Result := TCnConjoinDockHostForm(Parent);
end;

procedure TCnConjoinPanel.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client);
  if CanDock then
  begin
    GetWindowRect(Handle, InfluenceRect);
  end;
end;

procedure TCnConjoinPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  inherited CustomDockDrop(Source, X, Y);
  if Source.Control is TForm then
  begin
    { 下面代码是为了防止窗体的标题栏和窗体上面的控件的内容被清空 }
    ParentForm.ActiveControl := nil;
    if TForm(Source.Control).FormStyle = fsStayOnTop then
      TForm(Parent).FormStyle := fsStayOnTop;
  end;
  UpdateCaption(nil);
end;

procedure TCnConjoinPanel.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  { 如果停靠客户是TDockableForm，并且停靠客户上面有TDockClient控件，就同意停靠 }
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
end;

procedure TCnConjoinPanel.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);;
end;

procedure TCnConjoinPanel.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
end;

procedure TCnConjoinPanel.CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TCnConjoinPanel.CustomStartDock(var Source: TCnDragDockObject);
begin
  ParentForm.FUnDockControl := nil;
  inherited CustomStartDock(Source);
end;

function TCnConjoinPanel.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  ParentForm.FUnDockControl := Client;
  { 如果停靠客户是TDockableForm，就还原停靠客户的DockSite属性为True }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if ((VisibleDockClientCount = 1) or
    (DockClientCount <= 2)) and (NewTarget <> ParentForm.DockableControl) then
  begin
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  end;
  UpdateCaption(Client);
  Result := inherited CustomUnDock(Source, NewTarget, Client);
end;

procedure TCnConjoinPanel.ReloadDockedControl(
  const AControlName: string; var AControl: TControl);
begin
  AControl := Cn_FindDockFormWithName(AControlName);
end;

procedure TCnConjoinPanel.CMUnDockClient(var Message: TCMUnDockClient);
begin
  inherited;
  { 如果有两个停靠客户，就要释放ParentForm }
  if (DockClientCount = 2) and (VisibleDockClientCount = 1) then
    PostMessage(ParentForm.Handle, WM_CLOSE, 0, 0);
  if VisibleDockClientCount <= 2 then
    CnDockFormControl.UpdateCaption(Self, Message.Client);
  if UseDockManager and (CnDockManager <> nil) then
    CnDockManager.ResetBounds(True);
end;

{ TCnTabPageControl }

procedure TCnTabPageControl.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  case TabPosition of
    tpLeft:
      Inc(Rect.Left, 2);
    tpRight:
      Dec(Rect.Right, 2);
    tpBottom:
    begin
      Dec(Rect.Top, 1);
      Dec(Rect.Bottom, 2);
    end;
  end;
end;

constructor TCnTabPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);
  DockClient := FindDockClient(Parent);
  SetDockSite(Self, True);
  PopupMenu := DockPageControlPopupMenu;
  HotTrack := DockPageControlHotTrack;
  DoubleBuffered := True;
  Caption := '';
  FVersion := $00040000;
end;

var TabPageStreamEndFlag: Integer = -10;

destructor TCnTabPageControl.Destroy;
begin
  SetDockSite(Self, False);
  inherited Destroy;
end;

procedure TCnTabPageControl.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  if Perform(CM_DOCKCLIENT, Integer(Source), Integer(SmallPoint(X, Y))) >= 0 then
  begin
    if Source.Control is TForm then
    begin
      { 下面一行代码是为了防止窗体的标题栏和窗体上面的控件的内容被清空 }
      TForm(Source.Control).ActiveControl := nil;
      SetDockSite(TWinControl(Source.Control), False);
      if TForm(Source.Control).FormStyle = fsStayOnTop then
        TForm(Parent).FormStyle := fsStayOnTop;
    end;
    UpdateCaption(nil);
  end;
end;

function TCnTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  { 如果停靠客户是TDockableForm，就还原停靠客户的DockSite属性为True }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2){ and (NewTarget <> Self) }then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

function TCnTabPageControl.GetParentForm: TCnTabDockHostForm;
begin
  Result := TCnTabDockHostForm(Parent);
end;

procedure TCnTabPageControl.LoadFromStream(Stream: TStream);
var i, Count, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: string;
  AControl: TControl;
begin
  try
    Stream.Read(i, Sizeof(i));
    { 从流中读出标签的个数，并且依次从流中读出她们的信息 }
    Stream.Read(Count, Sizeof(Count));
    for i := 0 to Count - 1 do
    begin
      ControlName := '';
      { 读出名字的长度 }
      Stream.Read(NameLen, SizeOf(NameLen));
      if NameLen > 0 then
      begin
        { 读出名字的字符串 }
        SetLength(ControlName, NameLen);
        Stream.Read(Pointer(ControlName)^, NameLen * SizeOf(Char));
      end;
      if ControlName <> '' then
      begin
        { 找到名字为ControlName的停靠窗体，并且把她停靠进TCnTabPageControl }
        ReloadDockedControl(ControlName, AControl);
        if AControl <> nil then
          AControl.ManualDock(Self, nil, alClient);
      end;
      { 读出标签的Visible }
      Stream.Read(SheetVisible, SizeOf(SheetVisible));
      DockClients[i].Visible := Boolean(SheetVisible);
    end;
    { 读出被激活的标签 }
    Stream.Read(ActiveSheetIndex, SizeOf(ActiveSheetIndex));
    ActivePageIndex := ActiveSheetIndex;
  finally

  end;
end;

procedure TCnTabPageControl.SaveToStream(Stream: TStream);
var i, Count, NameLen, SheetVisible, ActiveSheetIndex: Integer;
  ControlName: string;
  CurrentControl: TControl;
begin
  { 写入流的版本 }
  Stream.Write(FVersion, SizeOf(FVersion));
  Count := PageCount;
  { 写入TabSheet的个数 }
  Stream.Write(Count, SizeOf(Count));
  for i := 0 to PageCount - 1 do
  begin
    CurrentControl := Pages[i].Controls[0];
    ControlName := CurrentControl.Name;
    NameLen := Length(ControlName);
    { 写入名字的长度 }
    Stream.Write(NameLen, SizeOf(NameLen));
    { 写入名字 }
    if NameLen > 0 then Stream.Write(Pointer(ControlName)^, NameLen * SizeOf(Char));
    SheetVisible := 0;
    if (Self is TCnVSNETTabPageControl) and (ParentForm.HostDockSite is TCnDockPanel) then
      SheetVisible := Integer(TCnVSNETDockTabSheet(Pages[i]).OldVisible)
    else
      SheetVisible := SheetVisible + Integer(CurrentControl.Visible);
    { 写入是否可见 }
    Stream.Write(SheetVisible, SizeOf(SheetVisible));
  end;
  ActiveSheetIndex := ActivePageIndex;
  { 写入激活的TabSheet的索引 }
  Stream.Write(ActiveSheetIndex, SizeOf(ActiveSheetIndex));
  { 写入结束标志 }
  Stream.Write(TabPageStreamEndFlag, SizeOf(TabPageStreamEndFlag));
end;

procedure TCnTabPageControl.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
var DragDockObject: TDragDockObject;
begin
  if Source.DropAlign in [alClient, alNone] then
  begin
    DragDockObject := TDragDockObject.Create(Source.Control);
    try
      DragDockObject.DockRect := Source.DockRect;
      DragDockObject.Control := Source.Control;
      Perform(CM_DOCKCLIENT, Integer(DragDockObject), Integer(SmallPoint(X, Y)));
      UpdateCaption(nil);
    finally
      DragDockObject.Free;
    end;
  end else
  begin
    inherited CustomDockDrop(Source, X, Y);
  end;
  if Source.Control is TForm then
  begin
    { 下面一行代码是为了防止窗体的标题栏和窗体上面的控件的内容被清空 }
    TForm(Source.Control).ActiveControl := nil;
    { ------------------------------------------------------------------------ }
    SetDockSite(TForm(Source.Control), False);
  end;
end;

procedure TCnTabPageControl.CustomDockOver(Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var ARect: TRect;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);
  { 如果停靠客户上面有TDockClient控件，就同意停靠 }
  Accept := IsDockable(Self, Source.Control, Source.DropOnControl, Source.DropAlign);
  // 画停靠客户的预览停靠位置
  if Accept then
  begin
    ComputeDockingRect(self, ARect,
      Point(ClientWidth div 2, ClientHeight div 2));
    Source.DockRect := ARect;
  end;
end;

procedure TCnTabPageControl.CustomEndDock(Target: TObject; X, Y: Integer);
begin
  inherited CustomEndDock(Target, X, Y);
end;

procedure TCnTabPageControl.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited CustomGetSiteInfo(Source, Client, InfluenceRect, MousePos, CanDock);
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

procedure TCnTabPageControl.CustomPositionDockRect(
  Source: TCnDragDockObject; X, Y: Integer);
begin
  inherited CustomPositionDockRect(Source, X, Y);
end;

procedure TCnTabPageControl.CustomStartDock(var Source: TCnDragDockObject);
begin
  inherited CustomStartDock(Source);
end;

function TCnTabPageControl.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  { 如果停靠客户是TDockableForm，就还原停靠客户的DockSite属性为True }
  if not (Client is TCnDockableForm) then
    SetDockSite(TForm(Client), True);
  if (VisibleDockClientCount = 1) or
    (DockClientCount <= 2) then
    PostMessage(Parent.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
  Result := Perform(CM_UNDOCKCLIENT, Integer(NewTarget), Integer(Client)) = 0;
end;

procedure TCnTabPageControl.ReloadDockedControl(
  const AControlName: string; var AControl: TControl);
begin
  AControl := Cn_FindDockFormWithName(AControlName);
end;

procedure TCnTabPageControl.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
var ARect: TRect;
begin
  ARect := Source.DockRect;
  DropAlign := ComputeDockingRect(Source.Control, ARect, MousePos);
end;

{ TCnBasicDockStyle }

constructor TCnBasicDockStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { 设置一些默认的类的引用 }
  CnDockPanelClass := DefaultDockPanelClass;
  CnDockSplitterClass := DefaultDockSplitterClass;
  CnConjoinPanelClass := DefaultConjoinPanelClass;
  CnTabDockClass := DefaultTabDockClass;
  CnDockPanelTreeClass := DefaultDockTreeClass;
  CnDockPanelZoneClass := DefaultDockZoneClass;
  CnConjoinPanelTreeClass := DefaultDockTreeClass;
  CnConjoinPanelZoneClass := DefaultDockZoneClass;
  FCnConjoinServerOptionClass := TCnBasicConjoinServerOption;
  FCnTabServerOptionClass := TCnBasicTabServerOption;
  FDockBaseControlList := TList.Create;
  if AOwner is TCustomForm then
    FParentForm := TForm(AOwner)
  else FParentForm := nil;

  if not (csDesigning in ComponentState) then
  begin
    { 保存老的窗口过程 }
    FOldWindowProc := FParentForm.WindowProc;
    { 重载窗口过程 }
    FParentForm.WindowProc := ParentFormWindowProc;
  end;
end;

function TCnBasicDockStyle.GetControlName: string;
begin
  Result := gs_CnDockStyleName;
end;

function TCnBasicDockStyle.GetDockStyleVersion: string;
begin
  Result := gs_CnDockStyleVersion;
end;

procedure TCnBasicDockStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TCnDockBaseControl);
begin
  { 没有事做 }
end;

procedure TCnBasicDockStyle.FormDockDrop(DockClient: TCnDockClient; Source: TCnDragDockObject; X,
  Y: Integer);
var
  ARect,DRect: TRect;
  DockType: TAlign;
  Host: TCustomForm;
  APanelDock: TWinControl;
begin
  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    Host := nil;
    { 锁住Windows桌面 }
    if not IsLoading then
      Cn_LockWindow(nil);
    try
      // 调用ComputeDockingRect函数知道停靠的类型
      with DockClient do
      begin
        DockType := ComputeDockingRect(DockClient.ParentForm, ARect, Point(X, Y));

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
              Host.ManualDock(APanelDock, nil, alClient);
              Host.Visible := True;
            end;
          end
          else
          begin
            // 如果停靠类型不是alClient,
            // 就把停靠窗体停靠到TCnDockPanel.
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
          end;
          Exit;
        end;

        // 创建分页的服务窗体
        if DockType = alClient then
        begin
          Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end
        // 创建平铺的服务窗体
        else if DockType <> alNone then
        begin
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

procedure TCnBasicDockStyle.FormDockOver(DockClient: TCnDockClient; Source: TCnDragDockObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  with DockClient do
  begin
    Accept := EnableDock and EachOtherDock and
      IsDockable(ParentForm.HostDockSite, Source.Control, Source.DropOnControl, Source.DropAlign);
    if Accept and (State = dsDragMove) and
      (ComputeDockingRect(ParentForm, ARect, Point(X, Y)) <> alNone) then
      Source.DockRect := ARect;
  end;
end;

procedure TCnBasicDockStyle.FormEndDock(DockClient: TCnDockClient; Target: TObject; X, Y: Integer);
begin
  { 没事做 }
end;

procedure TCnBasicDockStyle.FormPositionDockRect(DockClient: TCnDockClient;
  Source: TCnDragDockObject);
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

procedure TCnBasicDockStyle.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
// 基类不干
end;

procedure TCnBasicDockStyle.FormStartDock(DockClient: TCnDockClient; var Source: TCnDragDockObject);
begin
  { 没事做 }
end;

function TCnBasicDockStyle.FormUnDock(DockClient: TCnDockClient; NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := True;
end;

procedure TCnBasicDockStyle.FormGetSiteInfo(Source: TCnDragDockObject;
  DockClient: TCnDockClient;
  Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  with DockClient do
    CanDock := EnableDock and EachOtherDock and
      IsDockable(ParentForm, Client, Source.DropOnControl, Source.DropAlign);
end;

function TCnBasicDockStyle.DockClientWindowProc(DockClient: TCnDockClient; var Message: TMessage): Boolean;
begin
  { 默认要执行原来的消息过程 }
  Result := False;
end;

procedure TCnBasicDockStyle.FormGetDockEdge(DockClient: TCnDockClient;
  Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
  DropAlign := TCnControlAccess(DockClient.ParentForm).GetDockEdge(MousePos);
end;

procedure TCnBasicDockStyle.SetConjoinServerOption(
  const Value: TCnBasicConjoinServerOption);
begin
  FCnConjoinServerOption.Assign(Value);
end;

procedure TCnBasicDockStyle.SetTabServerOption(
  const Value: TCnBasicTabServerOption);
begin
  FCnTabServerOption.Assign(Value);
end;

procedure TCnBasicDockStyle.CreateConjoinServerOption(
  var Option: TCnBasicConjoinServerOption);
begin
  Option := TCnBasicConjoinServerOption.Create(Self);
end;

procedure TCnBasicDockStyle.CreateTabServerOption(
  var Option: TCnBasicTabServerOption);
begin
  Option := TCnBasicTabServerOption.Create(Self);
end;

procedure TCnBasicDockStyle.CreateServerOption;
begin
  if FCnConjoinServerOption = nil then
    FCnConjoinServerOption := FCnConjoinServerOptionClass.Create(Self);
  if FCnTabServerOption = nil then
    FCnTabServerOption := FCnTabServerOptionClass.Create(Self);
end;

function TCnBasicDockStyle.GetConjoinServerOption: TCnBasicConjoinServerOption;
begin
  Result := FCnConjoinServerOption;
end;

function TCnBasicDockStyle.GetTabServerOption: TCnBasicTabServerOption;
begin
  Result := FCnTabServerOption;
end;

procedure TCnBasicDockStyle.FreeServerOption;
begin
  if FCnConjoinServerOption <> nil then
    FCnConjoinServerOption.Free;
  if FCnTabServerOption <> nil then
    FCnTabServerOption.Free;
end;

destructor TCnBasicDockStyle.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if @FOldWindowProc <> nil then
      FParentForm.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
  end;
  FDockBaseControlList.Free;
  FreeServerOption;
  inherited Destroy;
end;

procedure TCnBasicDockStyle.AssignConjoinServerOption(
  APanel: TCnCustomDockPanel);
begin
  APanel.CnDockManager.GrabberSize := ConjoinServerOption.GrabbersSize;
  APanel.CnDockManager.SplitterWidth := ConjoinServerOption.SplitterWidth;
end;

procedure TCnBasicDockStyle.AssignTabServerOption(
  APage: TCnTabPageControl);
begin
  APage.HotTrack := TabServerOption.HotTrack;
  APage.TabPosition := TabServerOption.TabPosition;
end;

procedure TCnBasicDockStyle.Loaded;
begin
  inherited;
end;

procedure TCnBasicDockStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateServerOption;
end;

procedure TCnBasicDockStyle.ParentFormWindowProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOldWindowProc) then
      FOldWindowProc(Message);
  end;
end;

procedure TCnBasicDockStyle.AddDockBaseControl(
  ADockBaseControl: TCnDockBaseControl);
begin
  if ADockBaseControl = nil then Exit;
  if FDockBaseControlList.IndexOf(ADockBaseControl) = -1 then
  begin
    FDockBaseControlList.Add(ADockBaseControl);
    ConjoinServerOption.ResetDockControlOption;
    TabServerOption.ResetDockControlOption;
  end;
end;

procedure TCnBasicDockStyle.RemoveDockBaseControl(
  ADockBaseControl: TCnDockBaseControl);
begin
  if ADockBaseControl = nil then Exit;
  FDockBaseControlList.Remove(ADockBaseControl);
end;

procedure TCnBasicDockStyle.ResetDockControlConjoinOption;
begin

end;

procedure TCnBasicDockStyle.ResetDockControlTabOption;
begin

end;

function TCnBasicDockStyle.GetDockBaseControlListCount: Integer;
begin
  Result := FDockBaseControlList.Count;
end;

function TCnBasicDockStyle.GetDockBaseControlLists(
  Index: Integer): TCnDockBaseControl;
begin
  Result := FDockBaseControlList[Index];
end;

function TCnBasicDockStyle.DockServerWindowProc(DockServer: TCnDockServer;
  var Message: TMessage): Boolean;
begin
  { 默认要执行原来的消息过程 }
  Result := False;
end;

function TCnBasicDockStyle.GetDockState(
  DockClient: TCnDockClient): Integer;
begin
  Result := DS_Unknow;
  if (DockClient <> nil) and (DockClient.ParentForm <> nil) then
  begin
    if DockClient.ParentForm.Floating then
      Result := DS_Floating
    else Result := DS_Docking;
  end; 
end;

function TCnBasicDockStyle.CanSetBottomDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetEachOtherDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetEnableDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetLeftDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetRightDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

function TCnBasicDockStyle.CanSetTopDocked(ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := True;
end;

procedure TCnBasicDockStyle.ResetCursor(Source: TCnDragDockObject);
begin
  if (Source.TargetControl = nil) and (Source.Control <> nil) and (Source.Control.Floating) then
    Windows.SetCursor(Screen.Cursors[crDefault])
  else if (Source.TargetControl = nil) and (not GlobalDockClient.CanFloat) then
    Windows.SetCursor(Screen.Cursors[crNo])
  else
    Windows.SetCursor(Screen.Cursors[crDefault]);
end;

procedure TCnBasicDockStyle.HideDockForm(ADockClient: TCnDockClient);
begin
  { 调用窗体上的TCnDockClient的MakeHideEvent函数 }
  if ADockClient <> nil then
  begin
    ADockClient.ParentForm.Visible := False;
    ADockClient.MakeHideEvent;
  end;
end;

procedure TCnBasicDockStyle.ShowDockForm(ADockClient: TCnDockClient);
begin
  { 调用窗体上的TCnDockClient的MakeShowEvent函数 }
  if ADockClient <> nil then
  begin
    ADockClient.ParentForm.Visible := True;
    ADockClient.MakeShowEvent;
  end;
end;

function TCnBasicDockStyle.GetDockFormVisible(ADockClient: TCnDockClient): Boolean;
begin
  Result := True;
  if ADockClient <> nil then
  begin
    if ADockClient.ParentForm.Visible then
    begin
      if ADockClient.ParentForm.HostDockSite <> nil then
      begin
        if ADockClient.ParentForm.HostDockSite is TCnDockPanel then
          Result := ADockClient.ParentForm.HostDockSite.Width * ADockClient.ParentForm.HostDockSite.Height > 0
        else
          Result := GetFormVisible(ADockClient.ParentForm.HostDockSite.Parent);
      end;
    end else Result := False;
  end;
end;

procedure TCnBasicDockStyle.RestoreClient(DockClient: TCnDockClient);
var TmpLastDockSite: TWinControl;
  TmpUnDockLeft, TmpUnDockTop: Integer;
  i: Integer;
  ADockClient: TCnDockClient;
  ADockServer: TCnDockServer;
  ARect: TRect;

  procedure DoFloatParentForm;
  begin
    with DockClient do
    begin
      if (ParentForm.HostDockSite <> nil) then
      begin
        ARect := Bounds(TmpUnDockLeft, TmpUnDockTop, ParentForm.UndockWidth, ParentForm.UndockHeight);
        ParentForm.ManualFloat(ARect);
        if (ParentForm.Left <> ARect.Left) or (ParentForm.Top <> ARect.Top) then
        begin
          ParentForm.Left := ARect.Left;
          ParentForm.Top := ARect.Top;
        end;
      end;
    end;
  end;

begin
  if DockClient = nil then Exit;
  if not DockClient.CanFloat then Exit;
  with DockClient do
  begin
    { 必须是能够符合停靠条件的才行 }
    if not EnableDock then Exit;
    if LastDockSite is TCnDockPanel then
    begin
      with TCnDockPanel(LastDockSite) do
      begin
        { 对于停靠客户 }
        if ((not LeftDock) and (Align = alLeft)) or
          ((not RightDock) and (Align = alRight)) or
          ((not TopDock) and (Align = alTop)) or
          ((not BottomDock) and (Align = alBottom)) then
        begin
          DoFloatParentForm;
          Exit;
        end;

        { 对于停靠服务器 }
        ADockServer := DockServer;
        if ADockServer <> nil then
          if (not ADockServer.EnableDock) or
            ((not ADockServer.LeftDock) and (Align = alLeft)) or
            ((not ADockServer.RightDock) and (Align = alRight)) or
            ((not ADockServer.TopDock) and (Align = alTop)) or
            ((not ADockServer.BottomDock) and (Align = alBottom)) then
        begin
          DoFloatParentForm;
          Exit;
        end;
      end;
    end;

    if ParentForm is TCnConjoinDockHostForm then
    begin
      with TCnConjoinDockHostForm(ParentForm).Panel do
      begin
        for i := DockClientCount - 1 downto 0 do
        begin
          ADockClient := FindDockClient(DockClients[i]);
          if (ADockClient <> nil) and (ADockClient.LastDockSite is TCnDockPanel) then
            ADockClient.RestoreChild;
        end;
      end;
      Exit;
    end;

    { 保存原先的LastDockSite，因为在调用一些函数后会覆盖掉原先的LastDockSite }
    TmpLastDockSite := LastDockSite;
    TmpUnDockLeft := UnDockLeft;
    TmpUnDockTop := UnDockTop;

    { 重新设置DockClient的一些属性 }
    ResetDockClient(DockClient, nil);

    DoFloatParentForm;

    if TmpLastDockSite is TCnDockPanel then
    begin
      with TCnDockPanel(TmpLastDockSite) do
      begin
        if UseDockManager and (CnDockManager <> nil) then
        begin
          if not CnDockManager.HasZoneWithControl(ParentForm) then Exit;
          DisableAlign;
          try
            { 调用ParentForm的Dock函数把ParentForm停靠到LastDockSite中 }
            ParentForm.Dock(TmpLastDockSite, Rect(0,0,0,0));
            { 由于在LastDockSite的CnDockManager接口中已经有一个ParentForm的Zone，
              所以只要把它show出来就可以了 }
            CnDockManager.ShowControl(ParentForm);
            { 下面一行代码是为了防止窗体的标题栏和窗体上面的控件的内容被清空 }
            ParentForm.ActiveControl := nil;
            SetDockSite(ParentForm, False);
            { 使ParentForm获得焦点 }
            if ParentForm.Visible and ParentForm.CanFocus then
              ParentForm.SetFocus;
            ShowDockPanel(True, ParentForm, sdfDockPanel);
          finally
            EnableAlign;
          end;
        end;
      end;
    end;
  end;
end;

{ TCnAdvDockStyle }

function TCnAdvDockStyle.DockClientWindowProc(DockClient: TCnDockClient;
  var Message: TMessage): Boolean;
begin
  if (DockClient <> nil) and (Message.Msg = WM_NCLBUTTONDBLCLK) then
    if DockClient.CanFloat then
      DockClient.RestoreChild;
  Result := inherited DockClientWindowProc(DockClient, Message);
end;

{ TCnDockSplitter }

constructor TCnDockSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSnap := False;
end;

function TCnDockSplitter.FindControl: TControl;
begin
  if DockServer <> nil then
    Result := DockServer.GetDockPanelWithAlign(Align)
  else
    Result := inherited FindControl;
end;

function TCnDockSplitter.GetSplitterIndex: Integer;
begin
  case Align of
    alTop:    Result := 0;
    alBottom: Result := 1;
    alLeft:   Result := 2;
    alRight:  Result := 3;
  else
    Result := -1;
  end;
end;

{ TCnBasicServerOption }

procedure TCnBasicServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnBasicServerOption then
  begin

  end;
  inherited Assign(Source);
end;

constructor TCnBasicServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  FDockStyle := ADockStyle;
end;

destructor TCnBasicServerOption.Destroy;
begin

  inherited Destroy;
end;

procedure TCnBasicServerOption.ResetDockClientOption(
  ADockClient: TCnDockClient);
begin
// 本来这个函数是一个纯虚函数，但是不知道怎么回事编译要出错，只好改成一般的虚函数
end;

procedure TCnBasicServerOption.ResetDockServerOption(
  ADockServer: TCnDockServer);
begin
// 本来这个函数是一个纯虚函数，但是不知道怎么回事编译要出错，只好改成一般的虚函数
end;

{ TCnBasicTabServerOption }

procedure TCnBasicTabServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnBasicTabServerOption then
  begin
    FTabPosition := TCnBasicTabServerOption(Source).FTabPosition;
    FHotTrack := TCnBasicTabServerOption(Source).FHotTrack;
  end;
  inherited Assign(Source);

end;

constructor TCnBasicTabServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  inherited Create(ADockStyle);
  FHotTrack := False;
  FTabPosition := tpTop;
end;

destructor TCnBasicTabServerOption.Destroy;
begin

  inherited Destroy;
end;

procedure TCnBasicTabServerOption.ResetDockClientOption(
  ADockClient: TCnDockClient);
var PageControl: TCnTabPageControl;
begin
  if ADockClient = nil then Exit;
  { 得到这个ADockClient所在的窗体的PageControl }
  PageControl := TCnTabPageControl(TCnTabDockHostForm(ADockClient.ParentForm).PageControl);
  ResetTabPageControl(PageControl);
  if PageControl <> nil then
    PageControl.Invalidate;
end;

procedure TCnBasicTabServerOption.ResetDockControlOption;
var i: Integer;
  ADockClient: TCnDockClient;
begin
  { 循环DockStyle的DockBaseControlList列表，然后把每一个
    TCnDockClient取出来，然后调用各自的函数重新设置它们的选项 }
  for i := 0 to DockStyle.DockBaseControlListCount - 1 do
  begin
    if DockStyle.DockBaseControlLists[i] is TCnDockClient then
    begin
      ADockClient := TCnDockClient(DockStyle.DockBaseControlLists[i]);
      if ADockClient.ParentForm is TCnTabDockHostForm then
      begin
        { 重新设置TCnDockClient的选项 }
        ResetDockClientOption(ADockClient);
      end;
    end;
  end;
end;

procedure TCnBasicTabServerOption.ResetDockServerOption(
  ADockServer: TCnDockServer);
begin
  { do nothing }
end;

procedure TCnBasicTabServerOption.ResetTabPageControl(
  APage: TCnTabPageControl);
begin
  if APage = nil then Exit;
  APage.HotTrack := FHotTrack;
  APage.TabPosition := FTabPosition;
end;

procedure TCnBasicTabServerOption.SetHotTrack(const Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnBasicTabServerOption.SetTabPosition(
  const Value: TTabPosition);
begin
  if FTabPosition <> Value then
  begin
    FTabPosition := Value;
    ResetDockControlOption;
  end;
end;

{ TCnBasicConjoinServerOption }

procedure TCnBasicConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnBasicConjoinServerOption then
  begin
    FGrabbersSize := TCnBasicConjoinServerOption(Source).FGrabbersSize;
    FSplitterWidth := TCnBasicConjoinServerOption(Source).FSplitterWidth;
  end;
  inherited Assign(Source);
end;

constructor TCnBasicConjoinServerOption.Create(
  ADockStyle: TCnBasicDockStyle);
begin
  inherited Create(ADockStyle);
  GrabbersSize := 12;
  SplitterWidth := 4;
end;

destructor TCnBasicConjoinServerOption.Destroy;
begin
  inherited Destroy;
end;

procedure TCnBasicConjoinServerOption.ResetConjoinPanel(
  APanel: TCnConjoinPanel);
begin
  APanel.CnDockManager.GrabberSize := FGrabbersSize;
  APanel.CnDockManager.SplitterWidth := FSplitterWidth;
end;

procedure TCnBasicConjoinServerOption.ResetDockClientOption(
  ADockClient: TCnDockClient);
var
  ConjoinPanel: TCnConjoinPanel;
begin
  if ADockClient = nil then Exit;
  { 得到这个ADockClient所在的窗体的Panel }
  ConjoinPanel := TCnConjoinPanel(TCnConjoinDockHostForm(ADockClient.ParentForm).Panel);
  ResetConjoinPanel(ConjoinPanel);
  ConjoinPanel.Invalidate;
end;

procedure TCnBasicConjoinServerOption.ResetDockControlOption;
var
  i: Integer;
  ADockServer: TCnDockServer;
  ADockClient: TCnDockClient;
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
    end else if DockStyle.DockBaseControlLists[i] is TCnDockClient then
    begin
      ADockClient := TCnDockClient(DockStyle.DockBaseControlLists[i]);
      if ADockClient.ParentForm.HostDockSite is TCnConjoinPanel then
      begin
        { 重新设置TCnDockClient的选项 }
        ADockClient := FindDockClient(ADockClient.ParentForm.HostDockSite.Parent);
        ResetDockClientOption(ADockClient);
      end;
    end;
  end;
end;

procedure TCnBasicConjoinServerOption.ResetDockPanel(APanel: TCnDockPanel);
begin
  APanel.CnDockManager.GrabberSize := FGrabbersSize;
  APanel.CnDockManager.SplitterWidth := FSplitterWidth;
end;

procedure TCnBasicConjoinServerOption.ResetDockServerOption(
  ADockServer: TCnDockServer);
var
  i: Integer;
begin
  // Panel必须不能为空
  if ADockServer = nil then Exit;
  for i := 0 to 3 do
  begin
    if ADockServer.DockPanel[i] = nil then
      break;
    ResetDockPanel(ADockServer.DockPanel[i]);
    ADockServer.DockPanel[i].Invalidate;
  end;
end;

procedure TCnBasicConjoinServerOption.SetGrabbersSize(
  const Value: TGrabbersSize);
begin
  if FGrabbersSize <> Value then
  begin
    FGrabbersSize := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnBasicConjoinServerOption.SetGrabbersSize_WithoutChangeSystemInfo(
  const Value: TGrabbersSize);
begin
  FGrabbersSize := Value;
end;

procedure TCnBasicConjoinServerOption.SetSplitterWidth(
  const Value: TSplitterWidth);
begin
  if FSplitterWidth <> Value then
  begin
    FSplitterWidth := Value;
    ResetDockControlOption;
  end;
end;

procedure TCnBasicConjoinServerOption.SetSplitterWidth_WithoutChangeSystemInfo(
  const Value: TSplitterWidth);
begin
  FSplitterWidth := Value;
end;

{ TCnAdvTabPageControl }

procedure TCnAdvTabPageControl.CMUnDockClient(
  var Message: TCMUnDockClient);
begin
  inherited;
end;

procedure TCnAdvTabPageControl.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited;
end;

function TCnAdvTabPageControl.CustomUnDock(Source: TCnDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

destructor TCnAdvTabPageControl.Destroy;
var DockClient: TCnDockClient;
begin
  DockClient := FindDockClient(Parent);
  if (DockClient <> nil) and (DockClient.LastDockSite is TCnDockPanel) then
  begin
    with TCnDockPanel(DockClient.LastDockSite) do
    begin
      if UseDockManager and (CnDockManager <> nil) then
        CnDockManager.RemoveControl(Self.Parent);
    end;
  end;
  inherited;
end;

procedure TCnAdvTabPageControl.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited;
end;

function TCnAdvTabPageControl.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

{ TCnAdvConjoinPanel }

procedure TCnAdvConjoinPanel.CMUnDockClient(var Message: TCMUnDockClient);
begin
  inherited;

end;

procedure TCnAdvConjoinPanel.CustomDockDrop(Source: TCnDragDockObject; X,
  Y: Integer);
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, Source.TargetControl);
  inherited;
end;

function TCnAdvConjoinPanel.CustomUnDock(Source: TCnDragDockObject;
  NewTarget: TWinControl; Client: TControl): Boolean;
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, NewTarget);
  Result := inherited CustomUnDock(Source, NewTarget, Client)
end;

procedure TCnAdvConjoinPanel.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Source.Control, TControl(Source.DragTarget));
  inherited;
end;

function TCnAdvConjoinPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  // 设置Client上的TCnDockClient控件的LastDockSite
  if not IsLoading then
    ResetDockClient(Client, NewTarget);
  Result := inherited DoUnDock(NewTarget, Client);
end;

{ Initialization and cleanup }

procedure InitDockPresident;
var OSVERSIONINFO: TOSVERSIONINFO;
begin
  { 创建一个TCnDockPresident的实例 }
  if CnGlobalDockPresident <> nil then
    CnGlobalDockPresident.Free;
  CnGlobalDockPresident := TCnDockPresident.Create;
  { 判断当前操作系统的版本, 是WindowsXP还是其他, 以后会用到的 }
  OSVERSIONINFO.dwOSVersionInfoSize := sizeof(TOSVERSIONINFO);
  GetVersionEx(OSVERSIONINFO);
  IsWinXP := (OSVERSIONINFO.dwMajorVersion = 5) and (OSVERSIONINFO.dwMinorVersion = 1);
end;

procedure DoneDockPresident;
begin
  { 释放这个TCnDockPresident的实例 }
  if CnGlobalDockPresident <> nil then
  begin
    CnGlobalDockPresident.Free;
    CnGlobalDockPresident := nil;
  end;
end;

initialization
  InitDockPresident;

finalization
  DoneDockPresident;

end.

