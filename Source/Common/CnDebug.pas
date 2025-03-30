{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnDebug;
{* |<PRE>
================================================================================
* 软件名称：CnDebugger
* 单元名称：CnDebug 调试信息输出接口单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：该单元定义并实现了 CnDebugger 输出信息的接口内容，
*           支持 Win32 和 Win64 以及 Unicode 与非 Unicode
*           部分接口内容引用了 overseer 的 udbg 单元内容
*           注：MAC 下只支持到文件，且不支持时钟周期的计时模式
* 开发平台：PWin2000Pro + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.02.11
*               增加对 FMX 的 Evaluate 支持，需要定义 ENABLE_FMX，待测试
*           2025.02.05
*               增加 REDIRECT_OPDS 编译条件，以控制直接使用 OutputDebugStringA
*           2024.08.30
*               修正 x64 下输出时钟周期不准确的问题，去除无用函数
*           2022.12.03
*               支持 FMX，但在 MACOS 下只支持文件输出的方式
*           2022.08.18
*               用 CnPE 取代 JCL 的行号获取功能，待测试
*           2020.05.08
*               用 CnRTL 取代 JCL 的堆栈捕获功能，留行号获取功能，待测试
*           2019.03.25
*               移植部分功能包括写文件到 MACOS
*           2018.07.29
*               增加遍历全局组件与控件的功能
*           2018.01.31
*               增加记录 Windows 消息的功能
*           2017.04.12
*               默认改为 LOCAL_SESSION，需要更新 CnDebugViewer 至 1.6
*           2016.07.29
*               修正 CPU 周期计时时超界的问题，需要同步更新 CnDebugViewer 至 1.5
*           2015.06.16
*               增加四个记录 Class/Interface 的方法
*           2015.06.03
*               增加两个记录 array of const 的方法
*           2015.05.15
*               修正多线程同时启动 CnDebugViewer 时可能导致丢信息的问题
*           2015.04.13
*               增加两个记录字符串的方法，带十六进制输出，可供 Ansi/Unicode 使用
*           2014.10.03
*               增加两个记录 Exception 的方法
*           2012.10.15
*               修正 tkUString 对 D2009 版本以上的支持
*           2012.05.10
*               超长信息将拆分发送而不是截断
*           2009.12.31
*               不输出至 CnDebugViewer 时也可输出至文件
*           2008.07.16
*               增加部分声明以区分对宽字符的支持。
*           2008.05.01
*               增加部分记录入文件的属性。
*           2007.09.24
*               增加 DUMP_TO_FILE 条件，可同时将信息记录入文件中。
*           2007.01.05
*               增加 ALLDEBUG 条件，等同于 DEBUG 与 SUPPORT_EVALUATE。
*           2006.11.11
*               增加运行期查看对象 RTTI 信息的功能，需要定义 SUPPORT_EVALUATE。
*           2006.10.11
*               增加一消息类型，修改为全局对象。
*           2006.07.16
*               增加了三个消息统计属性。
*           2005.02.27
*               增加了类似于 Overseer 的 JclExcept 记录功能，需要安装 JCL 库。
*               如不安装 JCL 库，则需要从 JCL 库中复制以下文件来参与编译：
*           INC:crossplatform.inc, jcl.inc, jedi.inc, windowsonly.inc
*           PAS:Jcl8087, JclBase, JclConsole, JclDateTime,
*               JclDebug, JclFileUtils, JclHookExcept,
*               JclIniFiles, JclLogic, JclMath, JclPeImage,
*               JclRegistry, JclResources, JclSecurity, JclShell,
*               JclStrings, JclSynch, JclSysInfo, JclSysUtils,
*               JclTD32, JclWideStrings, JclWin32, Snmp;
*           并打开编译选项 Include TD32 debug Info 或生成 MapFile 以获得更多信息
*               (以 JCL 1.94 版为准)
*           2004.12.22 V1.0
*               创建单元,实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// {$DEFINE REDIRECT_OPDS}
// 定义此条件将在 Windows 下将输出直接改成 OutputDebugString，忽略其他所有输出方式
// 注意仍然会被 FIgnoreViewer 变量给拦截而不输出

// {$DEFINE DUMP_TO_FILE}
// 定义此条件可重定向到文件.
// Define this flag to log message to a file.

{$DEFINE LOCAL_SESSION}
// 定义此条件可将发送限制在当前用户会话内，不走全局，
// 自动启动相应的 DebugViewer 时也将通过命令行参数指定非全局。
// Define this flag to use local session, not global.

// {$DEFINE CAPTURE_STACK}
// 定义此条件可启用堆栈抓取与行号获取功能，之前的 USE_JCL 废止。专家包可按需在工程选项中定义

// 如果 FMX 框架下需要用到 FMX 功能，请手工定义 ENABLE_FMX。
// 如果是 MACOS，下文会自动定义 ENABLE_FMX
// {$DEFINE ENABLE_FMX}

{$IFDEF NDEBUG}
  {$UNDEF DEBUG}
  {$UNDEF SUPPORT_EVALUATE}
  {$UNDEF ALLDEBUG}
  {$UNDEF DUMP_TO_FILE}
{$ENDIF}

{$IFDEF ALLDEBUG}
  {$DEFINE DEBUG}
  {$DEFINE SUPPORT_EVALUATE}
{$ENDIF}

{$IFDEF MACOS}
  {$UNDEF CAPTURE_STACK}   // CnRTL Does NOT Support MACOS.
  {$UNDEF SUPPORT_EVALUATE}
  {$DEFINE ENABLE_FMX}     // MAC 下只能支持 FMX
  {$IFNDEF DUMP_TO_FILE}
    {$DEFINE DUMP_TO_FILE} // MAC 下只能支持到文件，干脆直接支持
  {$ENDIF}
{$ENDIF}

{$IFNDEF MSWINDOWS}
  {$UNDEF REDIRECT_OPDS}   // 非 Windows 下不支持 OutputDebugString
{$ENDIF}

uses
  SysUtils, Classes, TypInfo
  {$IFDEF ENABLE_FMX}, System.Types, System.UITypes, System.SyncObjs, System.UIConsts
  {$IFDEF MSWINDOWS}, Winapi.Windows, Winapi.Messages, Vcl.Controls, System.Win.Registry
  {$ELSE}, Posix.Unistd, Posix.Pthread {$ENDIF},
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Types
  {$ELSE}, Windows, Registry, Messages, Controls, Graphics, Forms {$ENDIF}
  {$IFDEF SUPPORT_ENHANCED_RTTI}, Rtti {$ENDIF}
  {$IFDEF CAPTURE_STACK}, CnPE, CnRTL {$ENDIF};

const
  CnMaxTagLength = 8; // 不可改变
  CnMaxMsgLength = 4096;
  CnDebugMagicLength = 8;
  CnDebugMapEnabled = $7F3D92E0; // 定义的一个 Magic 值表示 MapEnable

{$IFDEF LOCAL_SESSION}
  SCnDebugPrefix = 'Local\';
{$ELSE}
  SCnDebugPrefix = 'Global\';
{$ENDIF}
  SCnDebugMapName = SCnDebugPrefix + 'CnDebugMap';
  SCnDebugQueueEventName = SCnDebugPrefix + 'CnDebugQueueEvent';
  SCnDebugQueueMutexName = SCnDebugPrefix + 'CnDebugQueueMutex';
  SCnDebugStartEventName = SCnDebugPrefix + 'CnDebugStartEvent';
  SCnDebugFlushEventName = SCnDebugPrefix + 'CnDebugFlushEvent';

  SCnDefaultDumpFileName = 'CnDebugDump.cdd';

type
  // ===================== 以下结构定义需要和 Viewer 共享 ======================

  // 输出的信息类型
  TCnMsgType = (cmtInformation, cmtWarning, cmtError, cmtSeparator, cmtEnterProc,
    cmtLeaveProc, cmtTimeMarkStart, cmtTimeMarkStop, cmtMemoryDump, cmtException,
    cmtObject, cmtComponent, cmtCustom, cmtSystem, cmtUDPMsg, cmtWatch,
    cmtClearWatch);
  TCnMsgTypes = set of TCnMsgType;

  // 时间戳格式类型
  TCnTimeStampType = (ttNone, ttDateTime, ttTickCount, ttCPUPeriod);

  {$NODEFINE TCnMsgAnnex}
  TCnMsgAnnex = packed record
  {* 放入数据区的每条信息的头描述结构 }
    Level:     Integer;                            // 自定义 Level 数，供用户过滤用
    Indent:    Integer;                            // 缩进数目，由 Enter 和 Leave 控制
    ProcessId: Cardinal;                           // 调用者的进程 ID
    ThreadId:  Cardinal;                           // 调用者的线程 ID
    Tag: array[0..CnMaxTagLength - 1] of AnsiChar; // 自定义 Tag 值，供用户过滤用
    MsgType:   Cardinal;                           // 消息类型
    MsgCPInterval: Int64;                          // 计时结束时的 CPU 周期数
    TimeStampType: Cardinal;                       // 消息输出的时间戳类型
    case Integer of
      1: (MsgDateTime:   TDateTime);               // 消息输出的时间戳值 DateTime
      2: (MsgTickCount:  Cardinal);                // 消息输出的时间戳值 TickCount
      3: (MsgCPUPeriod:  Int64);                   // 消息输出的时间戳值 CPU 周期
  end;

  {$NODEFINE TCnMsgDesc}
  {$NODEFINE PCnMsgDesc}
  TCnMsgDesc = packed record
  {* 放入数据区的每条信息的描述结构，包括一信息头}
    Length: Integer;                               // 总长度，包括信息头
    Annex: TCnMsgAnnex;                            // 一个信息头
    Msg: array[0..CnMaxMsgLength - 1] of AnsiChar; // 需要记录的信息
  end;
  PCnMsgDesc = ^TCnMsgDesc;

  {$NODEFINE TCnMapFilter}
  {$NODEFINE PCnMapFilter}
  TCnMapFilter = packed record
  {* 用内存映射文件传送数据时的内存区头中的过滤器格式}
    NeedRefresh: Cardinal;                         // 非 0 时需要更新
    Enabled: Integer;                              // 非 0 时表示使能
    Level: Integer;                                // 限定的 Level
    Tag: array[0..CnMaxTagLength - 1] of AnsiChar; // 限定的 Tag
    case Integer of
      0: (MsgTypes: TCnMsgTypes);                  // 限定的 MsgTypes
      1: (DummyPlace: Cardinal);
  end;
  PCnMapFilter = ^TCnMapFilter;

  {$NODEFINE TCnMapHeader}
  {$NODEFINE PCnMapHeader}
  TCnMapHeader = packed record
  {* 用内存映射文件传送数据时的内存区头格式}
    MagicName:  array[0..CnDebugMagicLength - 1] of AnsiChar;  // 'CNDEBUG'
    MapEnabled: Cardinal;           // 为一 CnDebugMapEnabled 时，表示区域可用
    MapSize:    Cardinal;           // 整个 Map 的大小，不包括尾保护区
    DataOffset: Integer;            // 数据区相对于头部的偏移量，目前定为 64
    QueueFront: Integer;            // 队列头指针，是相对于数据区的偏移量
    QueueTail:  Integer;            // 队列尾指针，是相对于数据区的偏移量
    Filter: TCnMapFilter;           // Viewer 端设置的过滤器
  end;
  PCnMapHeader = ^TCnMapHeader;

  // ===================== 以上结构定义需要和 Viewer 共享 ======================

{$IFDEF MSWINDOWS}
  TCnDebugCriticalSection = TRTLCriticalSection;
{$ELSE}
  TCnDebugCriticalSection = TCriticalSection;
{$ENDIF}

  TCnFindComponentEvent = procedure(Sender: TObject; AComponent: TComponent;
    var Cancel: Boolean) of object;
  {* 寻找 Component 时的回调}

  TCnFindControlEvent = procedure(Sender: TObject; AControl: TControl;
    var Cancel: Boolean) of object;
  {* 寻找 Control 时的回调}

  TCnAnsiCharSet = set of AnsiChar; // 32 字节大小
{$IFDEF UNICODE}
  {$WARNINGS OFF}
  TCnWideCharSet = set of WideChar; // D2009 以上的 set 支持 WideChar 但实际上是裁剪到 AnsiChar，大小仍然是 32
  {$WARNINGS ON}
{$ENDIF}

  TCnTimeDesc = packed record
    Tag: array[0..CnMaxTagLength - 1] of AnsiChar;
    PassCount: Integer;
    StartTime: Int64;
    AccuTime: Int64;  // 计时的和
  end;
  PCnTimeDesc = ^TCnTimeDesc;

  TCnDebugFilter = class(TObject)
  {* 信息输出的过滤条件}
  private
    FLevel: Integer;
    FTag: string;
    FMsgTypes: TCnMsgTypes;
    FEnabled: Boolean;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
    property MsgTypes: TCnMsgTypes read FMsgTypes write FMsgTypes;
    property Level: Integer read FLevel write FLevel;
    property Tag: string read FTag write FTag;
  end;

  TCnDebugChannel = class;

  TCnDebugger = class(TObject)
  private
    FActive: Boolean;
    FThrdIDList: TList;
    FIndentList: TList;
    FTimes: TList;
    FFilter: TCnDebugFilter;
    FChannel: TCnDebugChannel;
    FCSThrdId: TCnDebugCriticalSection;
    FAutoStart: Boolean;
{$IFNDEF REDIRECT_OPDS}
    FViewerAutoStartCalled: Boolean;
{$ENDIF}
    // 内部变量，控制不朝 Viewer 输出
    FIgnoreViewer: Boolean;
    FExceptFilter: TStringList;
    FExceptTracking: Boolean;
    FPostedMessageCount: Integer;
    FMessageCount: Integer;
    FDumpToFile: Boolean;
    FDumpFileName: string;
    FDumpFile: TFileStream;
    FUseAppend: Boolean;
    FAfterFirstWrite: Boolean;
    FFindAbort: Boolean;
    FComponentFindList: TList;
    FOnFindComponent: TCnFindComponentEvent;
    FControlFindList: TList;
    FOnFindControl: TCnFindControlEvent;
{$IFDEF CAPTURE_STACK}
    FIsInExcption: Boolean;
{$ENDIF}
    procedure CreateChannel;

    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function IntArrayToString(ArrayAddress: Pointer; ElementCount, ElementSize: Integer;
      Sign: Boolean): string;
    function SizeToString(ASize: TSize): string;
    function PointToString(APoint: TPoint): string;
    function RectToString(ARect: TRect): string;
    function BitsToString(ABits: TBits): string;
    function GetExceptTracking: Boolean;
    procedure SetExceptTracking(const Value: Boolean);
    function GetDiscardedMessageCount: Integer;
{$IFDEF MSWINDOWS}
    function VirtualKeyToString(AKey: Word): string;
    function WindowMessageToStr(AMessage: Cardinal): string;
{$ENDIF}
    procedure SetDumpFileName(const Value: string);
    procedure SetDumpToFile(const Value: Boolean);
    function GetAutoStart: Boolean;
    function GetChannel: TCnDebugChannel;
    function GetDumpFileName: string;
    function GetDumpToFile: Boolean;
    function GetFilter: TCnDebugFilter;
    function GetUseAppend: Boolean;
    procedure SetAutoStart(const Value: Boolean);
    procedure SetUseAppend(const Value: Boolean);
    function GetMessageCount: Integer;
    function GetPostedMessageCount: Integer;
{$IFDEF SUPPORT_ENHANCED_RTTI}
    function GetEnumTypeStr<T>: string;
{$ENDIF}
    procedure InternalFindComponent(AComponent: TComponent);
    procedure InternalFindControl(AControl: TControl);
{$IFDEF CAPTURE_STACK}
     procedure ExceptionRecorder(ExceptObj: Exception; ExceptAddr: Pointer;
      IsOSException: Boolean; StackList: TCnStackInfoList);
{$ENDIF}
  protected
    function CheckEnabled: Boolean;
    {* 检测当前输出功能是否使能 }
    function CheckFiltered(const Tag: string; Level: Byte; AType: TCnMsgType): Boolean;
    {* 检测当前输出信息是否被允许输出，True 允许，False 允许 }

    // 处理 Indent
    function GetCurrentIndent(ThrdID: Cardinal): Integer;
    function IncIndent(ThrdID: Cardinal): Integer;
    function DecIndent(ThrdID: Cardinal): Integer;

    // 处理计时
    function IndexOfTime(const ATag: string): PCnTimeDesc;
    function AddTimeDesc(const ATag: string): PCnTimeDesc;

    // 统一处理 Format
    function FormatMsg(const AFormat: string; Args: array of const): string;
    function FormatConstArray(Args: array of const): string;
    function FormatClassString(AClass: TClass): string;
    function FormatInterfaceString(AIntf: IUnknown): string;
    function FormatObjectInterface(AObj: TObject): string;
    function GUIDToString(const GUID: TGUID): string;

    procedure GetCurrentTrace(Strings: TStrings);
    procedure GetTraceFromAddr(StackBaseAddr: Pointer; Strings: TStrings);

    procedure InternalOutputMsg(const AMsg: PAnsiChar; Size: Integer; const ATag: AnsiString;
      ALevel, AIndent: Integer; AType: TCnMsgType; ThreadID: Cardinal; CPUPeriod: Int64);
    procedure InternalOutput(var Data; Size: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartDebugViewer;

    // 利用 CPU 周期计时 == Start ==
    procedure StartTimeMark(const ATag: Integer; const AMsg: string = ''); overload;
    {* 标记此 Tag 的一次计时开始的时刻，不发送内容}
    procedure StopTimeMark(const ATag: Integer; const AMsg: string = ''); overload;
    {* 标记此 Tag 的一次计时结束的时刻，并将本次计时耗时叠加至同 Tag 计时上发出}

    {* 此两函数不使用局部字符串变量，误差相对较小，所以推荐使用}

    // 以下两函数由于使用了 Delphi 字符串，误差较大（几万左右个 CPU 周期）
    procedure StartTimeMark(const ATag: string; const AMsg: string = ''); overload;
    procedure StopTimeMark(const ATag: string; const AMsg: string = ''); overload;
    // 利用 CPU 周期计时 == End ==

    // Log 系列输出函数 == Start ==
    procedure LogMsg(const AMsg: string);
    procedure LogMsgWithTag(const AMsg: string; const ATag: string);
    procedure LogMsgWithLevel(const AMsg: string; ALevel: Integer);
    procedure LogMsgWithType(const AMsg: string; AType: TCnMsgType);
    procedure LogMsgWithTagLevel(const AMsg: string; const ATag: string; ALevel: Integer);
    procedure LogMsgWithLevelType(const AMsg: string; ALevel: Integer; AType: TCnMsgType);
    procedure LogMsgWithTypeTag(const AMsg: string; AType: TCnMsgType; const ATag: string);
    procedure LogFmt(const AFormat: string; Args: array of const);
    procedure LogFmtWithTag(const AFormat: string; Args: array of const; const ATag: string);
    procedure LogFmtWithLevel(const AFormat: string; Args: array of const; ALevel: Integer);
    procedure LogFmtWithType(const AFormat: string; Args: array of const; AType: TCnMsgType);
    procedure LogFull(const AMsg: string; const ATag: string;
      ALevel: Integer; AType: TCnMsgType; CPUPeriod: Int64 = 0);

    procedure LogSeparator;
    procedure LogEnter(const AProcName: string; const ATag: string = '');
    procedure LogLeave(const AProcName: string; const ATag: string = '');

    // 额外辅助的输出函数
    procedure LogMsgWarning(const AMsg: string);
    procedure LogMsgError(const AMsg: string);
    procedure LogErrorFmt(const AFormat: string; Args: array of const);
{$IFDEF MSWINDOWS}
    procedure LogLastError;
{$ENDIF}
    procedure LogAssigned(Value: Pointer; const AMsg: string = '');
    procedure LogBoolean(Value: Boolean; const AMsg: string = '');
    procedure LogColor(Color: TColor; const AMsg: string = '');
    procedure LogFloat(Value: Extended; const AMsg: string = '');
    procedure LogInteger(Value: Integer; const AMsg: string = '');
    procedure LogInt64(Value: Int64; const AMsg: string = '');
{$IFDEF SUPPORT_UINT64}
    procedure LogUInt64(Value: UInt64; const AMsg: string = '');
{$ENDIF}
    procedure LogChar(Value: Char; const AMsg: string = '');
    procedure LogAnsiChar(Value: AnsiChar; const AMsg: string = '');
    procedure LogWideChar(Value: WideChar; const AMsg: string = '');
    procedure LogSet(const ASet; ASetSize: Integer; SetElementTypInfo: PTypeInfo = nil;
      const AMsg: string = '');
    procedure LogCharSet(const ASet: TSysCharSet; const AMsg: string = '');
    procedure LogAnsiCharSet(const ASet: TCnAnsiCharSet; const AMsg: string = '');
{$IFDEF UNICODE}
    procedure LogWideCharSet(const ASet: TCnWideCharSet; const AMsg: string = '');
{$ENDIF}
    procedure LogDateTime(Value: TDateTime; const AMsg: string = '' );
    procedure LogDateTimeFmt(Value: TDateTime; const AFmt: string; const AMsg: string = '' );
    procedure LogPointer(Value: Pointer; const AMsg: string = '');
    procedure LogPoint(Point: TPoint; const AMsg: string = '');
    procedure LogSize(Size: TSize; const AMsg: string = '');
    procedure LogRect(Rect: TRect; const AMsg: string = '');
    procedure LogBits(Bits: TBits; const AMsg: string = '');
    procedure LogGUID(const GUID: TGUID; const AMsg: string = '');
    procedure LogRawString(const Value: string);
    procedure LogRawAnsiString(const Value: AnsiString);
    procedure LogRawWideString(const Value: WideString);
    procedure LogStrings(Strings: TStrings; const AMsg: string = '');
{$IFDEF SUPPORT_ENHANCED_RTTI}
    procedure LogEnumType<T>(const AMsg: string = '');
{$ENDIF}
    procedure LogException(E: Exception; const AMsg: string = '');
    procedure LogMemDump(AMem: Pointer; Size: Integer);
    procedure LogBitmapMemory(ABmp: TBitmap);
{$IFDEF MSWINDOWS}
    procedure LogVirtualKey(AKey: Word);
    procedure LogVirtualKeyWithTag(AKey: Word; const ATag: string);
    procedure LogWindowMessage(AMessage: Cardinal);
    procedure LogWindowMessageWithTag(AMessage: Cardinal; const ATag: string);
{$ENDIF}
    procedure LogObject(AObject: TObject);
    procedure LogObjectWithTag(AObject: TObject; const ATag: string);
    procedure LogCollection(ACollection: TCollection);
    procedure LogCollectionWithTag(ACollection: TCollection; const ATag: string);
    procedure LogComponent(AComponent: TComponent);
    procedure LogComponentWithTag(AComponent: TComponent; const ATag: string);
    procedure LogCurrentStack(const AMsg: string = '');
    procedure LogConstArray(const Arr: array of const; const AMsg: string = '');
    procedure LogIntegerArray(const Arr: array of Integer; const AMsg: string = ''); overload;
    procedure LogIntegerArray(const ArrAddr: Pointer; Count: Integer; const AMsg: string = ''); overload;
    procedure LogCardinalArray(const Arr: array of Cardinal; const AMsg: string = ''); overload;
    procedure LogCardinalArray(const ArrAddr: Pointer; Count: Integer; const AMsg: string = ''); overload;
    procedure LogClass(const AClass: TClass; const AMsg: string = '');
    procedure LogClassByName(const AClassName: string; const AMsg: string = '');
    procedure LogInterface(const AIntf: IUnknown; const AMsg: string = '');
    procedure LogStackFromAddress(Addr: Pointer; const AMsg: string = '');
    // Log 系列输出函数 == End ==

    // Trace 系列输出函数 == Start ==
    procedure TraceMsg(const AMsg: string);
    procedure TraceMsgWithTag(const AMsg: string; const ATag: string);
    procedure TraceMsgWithLevel(const AMsg: string; ALevel: Integer);
    procedure TraceMsgWithType(const AMsg: string; AType: TCnMsgType);
    procedure TraceMsgWithTagLevel(const AMsg: string; const ATag: string; ALevel: Integer);
    procedure TraceMsgWithLevelType(const AMsg: string; ALevel: Integer; AType: TCnMsgType);
    procedure TraceMsgWithTypeTag(const AMsg: string; AType: TCnMsgType; const ATag: string);
    procedure TraceFmt(const AFormat: string; Args: array of const);
    procedure TraceFmtWithTag(const AFormat: string; Args: array of const; const ATag: string);
    procedure TraceFmtWithLevel(const AFormat: string; Args: array of const; ALevel: Integer);
    procedure TraceFmtWithType(const AFormat: string; Args: array of const; AType: TCnMsgType);
    procedure TraceFull(const AMsg: string; const ATag: string;
      ALevel: Integer; AType: TCnMsgType; CPUPeriod: Int64 = 0);

    procedure TraceSeparator;
    procedure TraceEnter(const AProcName: string; const ATag: string = '');
    procedure TraceLeave(const AProcName: string; const ATag: string = '');

    // 额外辅助的输出函数
    procedure TraceMsgWarning(const AMsg: string);
    procedure TraceMsgError(const AMsg: string);
    procedure TraceErrorFmt(const AFormat: string; Args: array of const);
{$IFDEF MSWINDOWS}
    procedure TraceLastError;
{$ENDIF}
    procedure TraceAssigned(Value: Pointer; const AMsg: string = '');
    procedure TraceBoolean(Value: Boolean; const AMsg: string = '');
    procedure TraceColor(Color: TColor; const AMsg: string = '');
    procedure TraceFloat(Value: Extended; const AMsg: string = '');
    procedure TraceInteger(Value: Integer; const AMsg: string = '');
    procedure TraceInt64(Value: Int64; const AMsg: string = '');
{$IFDEF SUPPORT_UINT64}
    procedure TraceUInt64(Value: UInt64; const AMsg: string = '');
{$ENDIF}
    procedure TraceChar(Value: Char; const AMsg: string = '');
    procedure TraceAnsiChar(Value: AnsiChar; const AMsg: string = '');
    procedure TraceWideChar(Value: WideChar; const AMsg: string = '');
    procedure TraceSet(const ASet; ASetSize: Integer; SetElementTypInfo: PTypeInfo = nil;
      const AMsg: string = '');
    procedure TraceCharSet(const ASet: TSysCharSet; const AMsg: string = '');
    procedure TraceAnsiCharSet(const ASet: TCnAnsiCharSet; const AMsg: string = '');
{$IFDEF UNICODE}
    procedure TraceWideCharSet(const ASet: TCnWideCharSet; const AMsg: string = '');
{$ENDIF}
    procedure TraceDateTime(Value: TDateTime; const AMsg: string = '' );
    procedure TraceDateTimeFmt(Value: TDateTime; const AFmt: string; const AMsg: string = '' );
    procedure TracePointer(Value: Pointer; const AMsg: string = '');
    procedure TracePoint(Point: TPoint; const AMsg: string = '');
    procedure TraceSize(Size: TSize; const AMsg: string = '');
    procedure TraceRect(Rect: TRect; const AMsg: string = '');
    procedure TraceBits(Bits: TBits; const AMsg: string = '');
    procedure TraceGUID(const GUID: TGUID; const AMsg: string = '');
    procedure TraceRawString(const Value: string);
    procedure TraceRawAnsiString(const Value: AnsiString);
    procedure TraceRawWideString(const Value: WideString);
    procedure TraceStrings(Strings: TStrings; const AMsg: string = '');
{$IFDEF SUPPORT_ENHANCED_RTTI}
    procedure TraceEnumType<T>(const AMsg: string = '');
{$ENDIF}
    procedure TraceException(E: Exception; const AMsg: string = '');
    procedure TraceMemDump(AMem: Pointer; Size: Integer);
    procedure TraceBitmapMemory(ABmp: TBitmap);
{$IFDEF MSWINDOWS}
    procedure TraceVirtualKey(AKey: Word);
    procedure TraceVirtualKeyWithTag(AKey: Word; const ATag: string);
    procedure TraceWindowMessage(AMessage: Cardinal);
    procedure TraceWindowMessageWithTag(AMessage: Cardinal; const ATag: string);
{$ENDIF}
    procedure TraceObject(AObject: TObject);
    procedure TraceObjectWithTag(AObject: TObject; const ATag: string);
    procedure TraceCollection(ACollection: TCollection);
    procedure TraceCollectionWithTag(ACollection: TCollection; const ATag: string);
    procedure TraceComponent(AComponent: TComponent);
    procedure TraceComponentWithTag(AComponent: TComponent; const ATag: string);
    procedure TraceCurrentStack(const AMsg: string = '');
    procedure TraceConstArray(const Arr: array of const; const AMsg: string = '');
    procedure TraceIntegerArray(const Arr: array of Integer; const AMsg: string = ''); overload;
    procedure TraceIntegerArray(const ArrAddr: Pointer; Count: Integer; const AMsg: string = ''); overload;
    procedure TraceCardinalArray(const Arr: array of Cardinal; const AMsg: string = ''); overload;
    procedure TraceCardinalArray(const ArrAddr: Pointer; Count: Integer; const AMsg: string = ''); overload;
    procedure TraceClass(const AClass: TClass; const AMsg: string = '');
    procedure TraceClassByName(const AClassName: string; const AMsg: string = '');
    procedure TraceInterface(const AIntf: IUnknown; const AMsg: string = '');
    procedure TraceStackFromAddress(Addr: Pointer; const AMsg: string = '');
    // Trace 系列输出函数 == End ==

    // 监视变量函数
    procedure WatchMsg(const AVarName: string; const AValue: string);
    procedure WatchFmt(const AVarName: string; const AFormat: string; Args: array of const);
    procedure WatchClear(const AVarName: string);

    // 异常过滤函数
    procedure AddFilterExceptClass(E: ExceptClass); overload;
    procedure RemoveFilterExceptClass(E: ExceptClass); overload;
    procedure AddFilterExceptClass(const EClassName: string); overload;
    procedure RemoveFilterExceptClass(const EClassName: string); overload;

    // 查看对象函数
    procedure EvaluateObject(AObject: TObject; SyncMode: Boolean = False); overload;
    procedure EvaluateObject(APointer: Pointer; SyncMode: Boolean = False); overload;


    procedure EvaluateControlUnderPos(const ScreenPos: TPoint); {$IFDEF ENABLE_FMX} overload; {$ENDIF}
{$IFDEF ENABLE_FMX}
    procedure EvaluateControlUnderPos(const ScreenPos: TPointF); overload;
{$ENDIF}
    procedure EvaluateInterfaceInstance(const AIntf: IUnknown; SyncMode: Boolean = False);

    // 辅助过程
    function ObjectFromInterface(const AIntf: IUnknown): TObject;

    procedure FindComponent;
    {* 全局范围内发起 Component 遍历，每个组件触发 OnFindComponent 事件，用于查找}

    procedure FindControl;
    {* 全局范围内发起 Control 遍历，每个组件触发 OnFindComponent 事件，用于查找}

    procedure Enable;
    procedure Disable;

    // 其他属性
    property Channel: TCnDebugChannel read GetChannel;
    property Filter: TCnDebugFilter read GetFilter;

    property Active: Boolean read GetActive write SetActive;
    {* 是否使能，也就是是否输出信息}
    property ExceptTracking: Boolean read GetExceptTracking write SetExceptTracking;
    {* 是否捕捉异常}
    property AutoStart: Boolean read GetAutoStart write SetAutoStart;
    {* 是否自动启动 Viewer}

    property DumpToFile: Boolean read GetDumpToFile write SetDumpToFile;
    {* 是否把输出信息同时输出到文件}
    property DumpFileName: string read GetDumpFileName write SetDumpFileName;
    {* 输出的文件名}
    property UseAppend: Boolean read GetUseAppend write SetUseAppend;
    {* 每次运行时，如果文件已存在，是否追加到已有内容后还是重写}

    // 输出消息统计
    property MessageCount: Integer read GetMessageCount;
    {* 调用而输出的拆包消息数。注意一条长消息可能会被拆包拆成多条消息}
    property PostedMessageCount: Integer read GetPostedMessageCount;
    {* 实际输出成功的拆包后的消息数。}
    property DiscardedMessageCount: Integer read GetDiscardedMessageCount;
    {* 未输出的拆包消息数。}

    property OnFindComponent: TCnFindComponentEvent read FOnFindComponent write FOnFindComponent;
    {* 全局遍历 Component 时的回调}
    property OnFindControl: TCnFindControlEvent read FOnFindControl write FOnFindControl;
    {* 全局遍历 Control 时的回调}
  end;

  TCnDebugChannel = class(TObject)
  {* 信息输出 Channel 的抽象类}
  private
    FAutoFlush: Boolean;
    FActive: Boolean;
    procedure SetAutoFlush(const Value: Boolean);
  protected
    procedure SetActive(const Value: Boolean); virtual;
    // 供子类重载以处理 Active 变化
    function CheckReady: Boolean; virtual;
    // 检测是否准备好
    procedure UpdateFlush; virtual;
    // AutoFlush 属性更新时供子类重载以进行处理
  public
    constructor Create(IsAutoFlush: Boolean = True); virtual;
    // 构造函数，参数为是否自动送出并等待接收完成
    procedure StartDebugViewer; virtual;
    // 启动 Debug Viewer 并等待其启动完成
    function CheckFilterChanged: Boolean; virtual;
    // 检测过滤条件是否改变
    procedure RefreshFilter(Filter: TCnDebugFilter); virtual;
    // 过滤条件改变时重新载入
    procedure SendContent(var MsgDesc; Size: Integer); virtual;
    // 发送信息内容
    property Active: Boolean read FActive write SetActive;
    // 是否激活
    property AutoFlush: Boolean read FAutoFlush write SetAutoFlush;
    // 是否自动送出并等接收方接收
  end;

  TCnDebugChannelClass = class of TCnDebugChannel;

{$IFDEF MSWINDOWS}

  TCnMapFileChannel = class(TCnDebugChannel)
  {* 使用内存映射文件来传输数据的 Channel 实现类}
  private
    FMap: THandle;               // 内存映射文件 Handle
    FQueueEvent: THandle;        // 队列写成功事件
    FQueueFlush: THandle;        // 队列一元素被读完成事件
    FMapSize:   Integer;         // 整个 Map 的大小
    FQueueSize: Integer;         // 数据区大小
    FMapHeader: Pointer;         // Map 区指针，也是头指针
    FMsgBase:   Pointer;         // Map 的数据区指针
    FFront:     Integer;         // 队列头指针，也是相对于数据区的偏移量
    FTail:      Integer;         // 队列尾指针，也是相对于数据区的偏移量

    function IsInitedFromHeader: Boolean;  // 检测并载入头信息
    procedure DestroyHandles;
    procedure LoadQueuePtr;
    procedure SaveQueuePtr(SaveFront: Boolean = False);
  protected
    function CheckReady: Boolean; override;
    procedure UpdateFlush; override;
  public
    constructor Create(IsAutoFlush: Boolean = True); override;
    destructor Destroy; override;
    procedure StartDebugViewer; override;
    function CheckFilterChanged: Boolean; override;
    procedure RefreshFilter(Filter: TCnDebugFilter); override;
    procedure SendContent(var MsgDesc; Size: Integer); override;
  end;

{$ENDIF}

function CnDebugger: TCnDebugger;

var
  CnDebugChannelClass: TCnDebugChannelClass = nil;
  // 当前 Channel 的 Class

  CnDebugMagicName: string = 'CNDEBUG';

  CurrentLevel: Byte = 3;
  CurrentTag: string = '';
  CurrentMsgType: TCnMsgType = cmtInformation;
  TimeStampType: TCnTimeStampType = ttDateTime;

implementation

{$IFDEF SUPPORT_EVALUATE}
uses
  CnPropSheetFrm;
{$ENDIF}

const
  SCnCRLF = #13#10;
  SCnTimeMarkStarted = 'Start Time Mark. ';
  SCnTimeMarkStopped = 'Stop Time Mark. ';

  SCnEnterProc = 'Enter: ';
  SCnLeaveProc = 'Leave: ';

  SCnAssigned = 'Assigned: ';
  SCnUnAssigned = 'Unassigned: ';
  SCnDefAssignedMsg = 'a Pointer.';

  SCnBooleanTrue = 'True: ';
  SCnBooleanFalse = 'False: ';
  SCnDefBooleanMsg = 'a Boolean Value.';

  SCnColor = 'Color: ';
  SCnInteger = 'Integer: ';
  SCnInt64 = 'Int64: ';
  SCnUInt64 = 'UInt64: ';
{$IFDEF UNICODE}
  SCnCharFmt = 'Char: ''%s''(%d/$%4.4x)';
{$ELSE}
  SCnCharFmt = 'Char: ''%s''(%d/$%2.2x)';
{$ENDIF}
  SCnAnsiCharFmt = 'AnsiChar: ''%s''(%d/$%2.2x)';
  SCnWideCharFmt = 'WideChar: ''%s''(%d/$%4.4x)';
  SCnDateTime = 'A Date/Time: ';
  SCnPointer = 'Pointer Address: ';
  SCnFloat = 'Float: ';
  SCnPoint = 'Point: ';
  SCnSize = 'Size: ';
  SCnRect = 'Rect: ';
  SCnGUID = 'GUID: ';
  SCnVirtualKeyFmt = 'VirtualKey: %d($%2.2x), %s';
  SCnException = 'Exception:';
  SCnNilComponent = 'Component is nil.';
  SCnObjException = '*** Exception ***';
  SCnUnknownError = 'Unknown Error! ';
  SCnLastErrorFmt = 'Last Error (Code: %d): %s';
  SCnConstArray = 'Array of Const:';
  SCnIntegerArray = 'Array of Int32:';
  SCnCardinalArray = 'Array of UInt32:';
  SCnEmptyArray = '<Empty Array>';
  SCnClass = 'Class:';
  SCnHierarchy = 'Hierarchy:';
  SCnClassFmt = '%s ClassName %s. InstanceSize %d%s%s';
  SCnInterface = 'Interface: ';
  SCnInterfaceFmt = '%s %s';
  SCnStackTraceFromAddress = 'Stack Trace';
  SCnStackTraceFromAddressFmt = '';
  SCnStackTraceNil = 'No Stack Trace.';
  SCnStackTraceNotSupport = 'Stack Trace NOT Support.';
{$IFDEF CPUX64}
  SCnLocationInfoFmt = '(%16.16x) [%-14s | $%16.16x] ';
{$ELSE}
  SCnLocationInfoFmt = '(%8.8x) [%-14s | $%8.8x] ';
{$ENDIF}

  CnDebugWaitingMutexTime = 1000;  // Mutex 的等待时间顶多 1 秒
  CnDebugStartingEventTime = 5000; // 启动 Viewer 的 Event 的等待时间顶多 5 秒
  CnDebugFlushEventTime = 100;     // 写队列后等待读取完成的时间顶多 0.1 秒

{$IFDEF CPUX64}
  CN_HEX_DIGITS = 16;
{$ELSE}
  CN_HEX_DIGITS = 8;
{$ENDIF}

type
{$IFDEF CPUX64}
  TCnNativeInt = NativeInt;
{$ELSE}
  TCnNativeInt = Integer;
{$ENDIF}

{$IFNDEF SUPPORT_INTERFACE_AS_OBJECT}
  PPointer = ^Pointer;
  TObjectFromInterfaceStub = packed record
    Stub: Cardinal;
    case Integer of
      0: (ShortJmp: ShortInt);
      1: (LongJmp:  LongInt)
  end;
  PObjectFromInterfaceStub = ^TObjectFromInterfaceStub;
{$ENDIF}

var
  FCnDebugger: TCnDebugger = nil;
  FCnDebuggerCriticalSection: TCnDebugCriticalSection;
  FStartCriticalSection: TCnDebugCriticalSection; // 用于多线程内控制启动 CnDebugViewer

  FFixedCalling: Int64 = 0;

  FUseLocalSession: Boolean = {$IFDEF LOCAL_SESSION}True{$ELSE}False{$ENDIF};

{$IFDEF CAPTURE_STACK}
  FInProcessCriticalSection: TCnDebugCriticalSection;
  FInProcessModuleList: TCnInProcessModuleList = nil;
{$ENDIF}

procedure CnEnterCriticalSection(Section: TCnDebugCriticalSection);
begin
{$IFDEF MSWINDOWS}
  EnterCriticalSection(Section);
{$ELSE}
  Section.Acquire;
{$ENDIF}
end;

procedure CnLeaveCriticalSection(Section: TCnDebugCriticalSection);
begin
{$IFDEF MSWINDOWS}
  LeaveCriticalSection(Section);
{$ELSE}
  Section.Release;
{$ENDIF}
end;

{$IFNDEF MACOS}

// RDTSC 指令可读出 CPU 时钟周期数放在 EDX:EAX 的 64 位数据中，32 与 64 位下均可用
// 但返回值 64 位下直接用 RAX 返回会漏掉 EDX 的高位，因而需要将 EDX 左移 32 位后拼到 RAX 高 32 位
function GetCPUPeriod: Int64; assembler;
asm
{$IFNDEF CPUX64}
  DB 0FH;
  DB 031H;
{$ELSE}
  RDTSC;
  SHL RDX, 32
  OR  RAX, RDX
{$ENDIF}
end;

{$ENDIF}

procedure FixCallingCPUPeriod;
var
  I: Integer;
  TestDesc: PCnTimeDesc;
begin
  if CnDebugger.Channel <> nil then
    CnDebugger.Channel.Active := False;

  CnDebugger.FIgnoreViewer := True;
  for I := 1 to 1000 do
  begin
    CnDebugger.StartTimeMark('', '');
    CnDebugger.StopTimeMark('', SCnTimeMarkStopped);
  end;
  CnDebugger.FIgnoreViewer := False;

  CnDebugger.FMessageCount := 0;
  CnDebugger.FPostedMessageCount := 0;

  if CnDebugger.Channel <> nil then
    CnDebugger.Channel.Active := True;
  TestDesc := CnDebugger.IndexOfTime('');
  if TestDesc <> nil then
    FFixedCalling := TestDesc^.AccuTime div 1000;
end;

procedure ShowError(const AMsg: string);
begin
  // MessageBox(0, PChar(AMsg), 'Error', MB_OK or MB_ICONWARNING);
end;

function TypeInfoName(TypeInfo: PTypeInfo): string;
begin
  Result := string(TypeInfo^.Name);
end;

// 根据 set 值与 set 的类型获得 set 的字符串，TypInfo 参数必须是枚举的类型，
// 而不能是 set of 后的类型，如无 TypInfo，则返回数值
function GetSetStr(TypInfo: PTypeInfo; Value: Integer): string;
var
  I: Integer;
  S: TIntegerSet;
begin
  if Value = 0 then
  begin
    Result := '[]';
    Exit;
  end;

  Result := '';
  Integer(S) := Value;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';

      if TypInfo = nil then
        Result := Result + IntToStr(I)
      else
      begin
        try
          Result := Result + GetEnumName(TypInfo, I);
        except
          Result := Result + IntToStr(I);
        end;
      end;
    end;
  end;
  Result := '[' + Result + ']';
end;

function GetAnsiCharSetStr(AnsiCharSetAddr: Pointer; SizeInByte: Integer): string;
var
  I, ByteOffset, BitOffset: Integer;
  EleByte, ByteMask: Byte;
begin
  Result := '';
  if SizeInByte <> SizeOf(TCnAnsiCharSet) then
  begin
    Result := '<Error Set>';
    Exit;
  end;

  for I := 0 to SizeInByte * 8 - 1 do
  begin
    ByteOffset := I div 8;
    BitOffset := I mod 8;
    ByteMask := 1 shl BitOffset;

    EleByte := PByte(TCnNativeInt(AnsiCharSetAddr) + ByteOffset)^;
    if (EleByte and ByteMask) <> 0 then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '''' + Chr(I) + '''';
    end;
  end;
  Result := '[' + Result + ']';
end;

function GetClassHierarchyString(AClass: TClass): string;
begin
  Result := '';
  while AClass <> nil do
  begin
    Result := Result + AClass.ClassName;
    AClass := AClass.ClassParent;
    if AClass <> nil then
      Result := Result + ' <- ';
  end;
end;

{$IFDEF CAPTURE_STACK}

function GetLocationInfoStr(const Address: Pointer): string;
var
  Info: TCnModuleDebugInfo;
  MN, UN, PN: string;
  LN, OL, OP: Integer;
begin
  Result := '';
  if FInProcessModuleList = nil then
  begin
    CnEnterCriticalSection(FInProcessCriticalSection);
    try
      if FInProcessModuleList = nil then
        FInProcessModuleList := CreateInProcessAllModulesList;
    finally
      CnLeaveCriticalSection(FInProcessCriticalSection);
    end;
  end;

  Info := FInProcessModuleList.CreateDebugInfoFromAddress(Address);
  if Info = nil then
    Exit;

  // (地址) [模块名| 基址]
  Result := Format(SCnLocationInfoFmt, [TCnNativeInt(Address),
    ExtractFileName(Info.ModuleFile), Info.ModuleHandle]);

  if Info.GetDebugInfoFromAddr(Address, MN, UN, PN, LN, OL, OP) then
  begin
    if PN <> '' then
    begin
      Result := Result + PN;
      if OP > 0 then
        Result := Result + Format(' + $%x', [OP]);
    end;

    if UN <> '' then
    begin
      Result := Result + ' ("' + UN + '"';
      if LN > 0 then
        Result := Result + Format(' #%d', [LN]);
      if OL > 0 then
        Result := Result + Format(' + $%x', [OL]);
      Result := Result + ')';
    end;
  end;
end;

{$ENDIF}

// 移植自 uDbg
procedure AddObjectToStringList(PropOwner: TObject; List: TStrings; Level: Integer);
type
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1; // see Classes.pas
var
  PropIdx: Integer;
  PropertyList: ^TPropList;
  PropertyName: string;
  PropertyTypeName: string;
  PropertyInfo: PPropInfo;
  PropertyType: PTypeInfo;
  PropertyKind: TTypeKind;
  BaseType: PTypeInfo;
  BaseData: PTypeData;
  GetProc: Pointer;
  OrdValue: Integer;
  FloatValue: Extended;
  N: Integer;
  Prefix: string;
  NewLine: string;
  EnumName: string;
  NextObject: TObject;
  FollowObject: Boolean;
begin
  if PropOwner = nil then  if PropOwner.ClassInfo = nil then
    Exit;

  Prefix := StringOfChar(' ', 2 * Level);
  List.Add(Prefix + SCnClass + PropOwner.ClassName);
  List.Add(Prefix + SCnHierarchy + GetClassHierarchyString(PropOwner.ClassType));

  if PropOwner.ClassInfo = nil then
    Exit;

  GetMem(PropertyList, SizeOf(TPropList));
  try
    // Build list of published properties
    FillChar(PropertyList^[0], SizeOf(TPropList), #00);
    GetPropList(PropOwner.ClassInfo, tkAny - [tkArray, tkRecord, tkUnknown,
      tkInterface], @PropertyList^[0]);
    // Process property list
    PropIdx := 0;
    while ((PropIdx < High(PropertyList^)) and (nil <> PropertyList^[PropIdx])) do
    begin
      // Get information about found properties
      PropertyInfo := PropertyList^[PropIdx];
      PropertyType := PropertyInfo^.PropType^;
      PropertyKind := PropertyType^.Kind;
      PropertyName := string(PropertyInfo^.Name);
      PropertyTypeName := string(PropertyType^.Name);

      // Write only property
      GetProc := PropertyInfo^.GetProc;
      if not Assigned(GetProc) then
      begin
        NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = <' +
          TypeInfoName(PropertyType) + '> (can''t be read)';
        List.Add(NewLine);
      end
      else
      begin
        case PropertyKind of
          tkSet:
            begin
              BaseType := GetTypeData(PropertyType)^.CompType^;
              BaseData := GetTypeData(BaseType);
              OrdValue := GetOrdProp(PropOwner, PropertyInfo);
              NewLine := Prefix + '+ ' + PropertyName + ': ' + PropertyTypeName + ' = [' +
                TypeInfoName(BaseType) + ']';
              List.Add(NewLine);
              for N := BaseData^.MinValue to BaseData^.MaxValue do
              begin
                EnumName := GetEnumName(BaseType, N);
                if EnumName = '' then
                  Break;
                NewLine := Prefix + '    ' + EnumName;
                if N in TIntegerSet(OrdValue) then
                  NewLine := NewLine + ' = True'
                else
                  NewLine := NewLine + ' = False';
                List.Add(NewLine);
              end;
            end;
          tkInteger:
            begin
              OrdValue := GetOrdProp(PropOwner, PropertyInfo);
              NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = ' + IntToStr(OrdValue);
              List.Add(NewLine);
            end;
          tkChar:
            begin
              OrdValue := GetOrdProp(PropOwner, PropertyInfo);
              NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = ' + '#$' +
                IntToHex(OrdValue, 2);
              List.Add(NewLine);
            end;
          tkWChar:
            begin
              OrdValue := GetOrdProp(PropOwner, PropertyInfo);
              NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = #$' + IntToHex(OrdValue, 4);
              List.Add(NewLine);
            end;
          tkClass:
            begin
              NextObject := GetObjectProp(PropOwner, PropertyInfo);
              if NextObject = nil then
              begin
                NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = <' +
                  TypeInfoName(PropertyType) + '> (Not Assigned)';
                List.Add(NewLine);
              end
              else
              begin
                NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = <' +
                  TypeInfoName(PropertyType) + '>';
                if NextObject is TComponent then
                begin
                  FollowObject := False;
                  NewLine := NewLine + ': ' + TComponent(NextObject).Name
                end
                else
                begin
                  FollowObject := True;
                  NewLine[Succ(Length(Prefix))] := '*';
                end;
                List.Add(NewLine);
                if FollowObject then
                begin
                  try
                    AddObjectToStringList(NextObject, List, Level + 1);
                  except
                    List.Add(SCnObjException);
                  end;
                end;
              end;
            end;
          tkFloat:
            begin
              FloatValue := GetFloatProp(PropOwner, PropertyInfo);
              NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = ' +
                FormatFloat('n', FloatValue);
              List.Add(NewLine);
            end;
          tkEnumeration:
            begin
              OrdValue := GetOrdProp(PropOwner, PropertyInfo);
              NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = ' +
                GetEnumName(PropertyType, OrdValue);
              List.Add(NewLine);
            end;
          tkString, tkLString, tkWString {$IFNDEF VER130} {$IF RTLVersion > 19.00}, tkUString{$IFEND} {$ENDIF}:
            begin
              NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = ' + '''' +
                GetStrProp(PropOwner, PropertyInfo) + '''';
              List.Add(NewLine);
            end;
          tkVariant:
            begin
              NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = ' +
                GetVariantProp(PropOwner, PropertyInfo);
              List.Add(NewLine);
            end;
          tkMethod:
            begin
              OrdValue := GetOrdProp(PropOwner, PropertyInfo);
              if OrdValue = 0 then
                NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = (nil)'
              else
                NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = (' +
                  GetEnumName(TypeInfo(TMethodKind),
                  Ord(GetTypeData(PropertyType)^.MethodKind)) + ')';
              List.Add(NewLine);
            end;
        else
          begin
            NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName + ' = <' +
              TypeInfoName(PropertyType) + '> ('
              + GetEnumName(TypeInfo(TTypeKind), Ord(PropertyKind)) + ')';
            List.Add(NewLine);
          end;
        end
      end;
      // Next item in the property list
      Inc(PropIdx);
    end;
    NewLine := '';
  finally
    if NewLine <> '' then
      List.Add(NewLine);
    FreeMem(PropertyList);
  end;
end;

procedure AddClassToStringList(PropClass: TClass; List: TStrings; Level: Integer);
type
  TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1; // see Classes.pas
var
  PropIdx: Integer;
  PropertyList: ^TPropList;
  PropertyName: string;
  PropertyInfo: PPropInfo;
  PropertyType: PTypeInfo;
  PropertyTypeName: string;
  Prefix: string;
  NewLine: string;
begin
  Prefix := StringOfChar(' ', 2 * Level);
  List.Add(Prefix + SCnHierarchy + GetClassHierarchyString(PropClass));

  if PropClass.ClassInfo = nil then
    Exit;

  GetMem(PropertyList, SizeOf(TPropList));
  try

    // Build list of published properties
    FillChar(PropertyList^[0], SizeOf(TPropList), #00);
    GetPropList(PropClass.ClassInfo, tkAny - [tkArray, tkRecord, tkUnknown,
      tkInterface], @PropertyList^[0]);

    // Process property list
    PropIdx := 0;
    while ((PropIdx < High(PropertyList^)) and (nil <> PropertyList^[PropIdx])) do
    begin
      // Get information about found properties
      PropertyInfo := PropertyList^[PropIdx];
      PropertyName := string(PropertyInfo^.Name);
      PropertyType := PropertyInfo^.PropType^;
      PropertyTypeName := string(PropertyType^.Name);

      NewLine := Prefix + '  ' + PropertyName + ': ' + PropertyTypeName;
      List.Add(NewLine);

      // Next item in the property list
      Inc(PropIdx);
    end;
  finally
    FreeMem(PropertyList);
  end;
end;

procedure CollectionToStringList(Collection: TCollection;
  AList: TStrings);
var
  I: Integer;
begin
  if (AList = nil) or (Collection = nil) then Exit;

  AList.Add('Collection: $' + IntToHex(TCnNativeInt(Collection), CN_HEX_DIGITS) + ' ' + Collection.ClassName);
  AList.Add('  Count = ' + IntToStr(Collection.Count));
  AddObjectToStringList(Collection, AList, 0);
  for I := 0 to Collection.Count - 1 do
  begin
    AList.Add('');
    AList.Add('  object: ' + Collection.Items[I].ClassName);
    AList.Add('    Index = ' + IntToStr(I));
    AddObjectToStringList(Collection.Items[I], AList, 1);
    AList.Add('  end');
  end;
  AList.Add('end');
end;

function GetBitmapPixelBytesCount(APixelFormat: TPixelFormat): Integer;
begin
{$IFNDEF ENABLE_FMX}
  case APixelFormat of
    pf8bit: Result := 1;
    pf15bit, pf16bit: Result := 2;
    pf24bit: Result := 3;
    pf32bit: Result := 4;
  else
    raise Exception.Create('NOT Suppport');
  end;
{$ELSE}
  Result := PixelFormatBytes[APixelFormat];
{$ENDIF}
end;

{$IFDEF ENABLE_FMX}

type
  TFmxControlHack = class(FMX.Controls.TControl);

function FindFmxControlAtPoint(const ScreenPos: TPointF): FMX.Controls.TControl;
var
  I, J: Integer;
  Form: TCommonCustomForm;
  FormRoot: TFmxObject;

  function FindControlAtPosition(Root: TFmxObject; const ScreenPos: TPointF): FMX.Controls.TControl;
  var
    I: Integer;
    ChildObj: TFmxObject;
    LocalPos: TPointF;
    CurrentControl: FMX.Controls.TControl;
  begin
    Result := nil;

    // 仅处理 FMX TControl 及其子类
    if not (Root is FMX.Controls.TControl) then Exit;
    CurrentControl := FMX.Controls.TControl(Root);

    // 过滤条件
    if not CurrentControl.Visible
      or not CurrentControl.HitTest
      or (CurrentControl.Opacity = 0) then Exit;

    // 坐标转换
    LocalPos := TFmxControlHack(CurrentControl).ScreenToLocal(ScreenPos);

    // 不在内部，退出
    if (LocalPos.X < 0) or (LocalPos.Y < 0) or (LocalPos.X >= CurrentControl.Width)
      or (LocalPos.Y >= CurrentControl.Height) then
      Exit;

    // 在内部，还得判断是否在子对象里，于是逆序遍历子对象（从最上层开始）
    for I := Root.ChildrenCount - 1 downto 0 do
    begin
      ChildObj := Root.Children[I];
      Result := FindControlAtPosition(ChildObj, ScreenPos);
      if Assigned(Result) then Exit;
    end;

    // 如果子对象未命中则返回当前控件
    Result := CurrentControl;
  end;

begin
  Result := nil;

  // 遍历所有窗体
  for I := FMX.Forms.Screen.FormCount - 1 downto 0 do
  begin
    Form := FMX.Forms.Screen.Forms[I];
    if not Form.Visible then
      Continue;

    // 检查是否在窗体客户区内
    if Form.ClientRect.Contains(Form.ScreenToClient(ScreenPos)) then
    begin
      for J := 0 to Form.ChildrenCount - 1 do
      begin
        Result := FindControlAtPosition(Form.Children[J], ScreenPos);
        if Result <> nil then
          Exit;
      end;
    end;
  end;
end;

{$ENDIF}

{$IFDEF MSWINDOWS}

function IsWin64: Boolean;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
var
  Kernel32Handle: THandle;
  IsWow64Process: function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  GetNativeSystemInfo : procedure(var lpSystemInfo: TSystemInfo); stdcall; isWoW64:BOOL; SystemInfo :  TSystemInfo;
begin
  Result := False;
  Kernel32Handle := GetModuleHandle(kernel32);

  if Kernel32Handle = 0 then
    Kernel32Handle := LoadLibrary(kernel32);

  if Kernel32Handle <> 0 then
  begin
    IsWow64Process := GetProcAddress(Kernel32Handle, 'IsWow64Process');
    GetNativeSystemInfo := GetProcAddress(Kernel32Handle, 'GetNativeSystemInfo');

    if Assigned(IsWow64Process) then
    begin
      IsWow64Process(GetCurrentProcess, isWoW64);
      Result := isWoW64 and Assigned(GetNativeSystemInfo);
      if Result then
      begin
        GetNativeSystemInfo(SystemInfo);
        Result := (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64)
          or (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64);
      end;
    end;
  end;
end;

{$ELSE}

function IsWin64: Boolean;
begin
  Result := False;
end;

{$ENDIF}

function CnDebugger: TCnDebugger;
begin
{$IFNDEF NDEBUG}
  if FCnDebugger = nil then
  begin
    CnEnterCriticalSection(FCnDebuggerCriticalSection);
    try
      if FCnDebugger = nil then
        FCnDebugger := TCnDebugger.Create;
    finally
      CnLeaveCriticalSection(FCnDebuggerCriticalSection);
    end;
  end;
  Result := FCnDebugger;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

{ TCnDebugger }

procedure TCnDebugger.AddFilterExceptClass(E: ExceptClass);
begin
  FExceptFilter.Add(E.ClassName);
end;

procedure TCnDebugger.AddFilterExceptClass(const EClassName: string);
begin
  FExceptFilter.Add(EClassName);
end;

function TCnDebugger.AddTimeDesc(const ATag: string): PCnTimeDesc;
var
  ADesc: PCnTimeDesc;
  Len: Integer;
  TTag: AnsiString;
begin
  New(ADesc);
  FillChar(ADesc^, SizeOf(TCnTimeDesc), 0);
  TTag := AnsiString(ATag);
  Len := Length(TTag);
  if Len > CnMaxTagLength then
    Len := CnMaxTagLength;

  Move(PAnsiChar(TTag)^, ADesc^.Tag, Len);
  FTimes.Add(ADesc);
  Result := ADesc;
end;

function TCnDebugger.CheckEnabled: Boolean;
begin
  Result := (Self <> nil) and FActive and (FChannel <> nil) and FChannel.Active;
end;

function TCnDebugger.CheckFiltered(const Tag: string;
  Level: Byte; AType: TCnMsgType): Boolean;
begin
  Result := True;
  if FFilter.Enabled then
  begin
    Result := Level <= FFilter.Level;
    if Result then
    begin
      Result := (FFilter.MsgTypes = []) or (AType in FFilter.MsgTypes);
      if Result then
        Result := (FFilter.Tag = '') or ((UpperCase(Tag) = UpperCase(FFilter.Tag))
          and (Length(Tag) <= CnMaxTagLength));
    end;
  end;
end;

constructor TCnDebugger.Create;
begin
  inherited;
  FAutoStart := True; // 是否有输出时自动启动 Viewer
  FIndentList := TList.Create;
  FThrdIDList := TList.Create;
  FTimes := TList.Create;

  FFilter := TCnDebugFilter.Create;
  FFilter.FLevel := CurrentLevel;

  {$IFDEF CAPTURE_STACK}
  FExceptTracking := True;
  FExceptFilter := TStringList.Create;
  FExceptFilter.Duplicates := dupIgnore;
  {$ENDIF}

  FDumpFileName := SCnDefaultDumpFileName;
{$IFDEF MSWINDOWS}
  InitializeCriticalSection(FCSThrdId);
{$ELSE}
  FCSThrdId := TCnDebugCriticalSection.Create;
{$ENDIF}

  CreateChannel;

{$IFDEF DUMP_TO_FILE}
  DumpToFile := True;
{$ENDIF}

  FActive := True;
end;

procedure TCnDebugger.CreateChannel;
begin
  if CnDebugChannelClass <> nil then
  begin
    FChannel := TCnDebugChannel(CnDebugChannelClass.NewInstance);
    try
      FChannel.Create(True); // 此处控制是否自动 Flush
    except
      FChannel := nil;
    end;
  end;
end;

function TCnDebugger.DecIndent(ThrdID: Cardinal): Integer;
var
  Indent, Index: Integer;
begin
  Index := FThrdIDList.IndexOf(Pointer(ThrdID));
  if Index >= 0 then
  begin
    Indent := Integer(FIndentList.Items[Index]); // Indent 很小，无需在乎 64 位下截断成 32 位
    if Indent > 0 then Dec(Indent);
    FIndentList.Items[Index] := Pointer(Indent);
    Result := Indent;
  end
  else
  begin
    CnEnterCriticalSection(FCSThrdId);
    FThrdIDList.Add(Pointer(ThrdID));
    FIndentList.Add(nil);
    CnLeaveCriticalSection(FCSThrdId);
    Result := 0;
  end;
end;

destructor TCnDebugger.Destroy;
var
  I: Integer;
begin
{$IFDEF MSWINDOWS}
  DeleteCriticalSection(FCSThrdId);
{$ELSE}
  FCSThrdId.Free;
{$ENDIF}

  FChannel.Free;
  FDumpFile.Free;
  FFilter.Free;
  for I := 0 to FTimes.Count - 1 do
    if FTimes[I] <> nil then
      Dispose(FTimes[I]);
  FExceptFilter.Free;
  FTimes.Free;
  FThrdIDList.Free;
  FIndentList.Free;
  inherited;
end;

function TCnDebugger.FormatMsg(const AFormat: string;
  Args: array of const): string;
var
  I: Integer;
begin
  try
    Result := Format(AFormat, Args);
  except
    // Format String Error.
    Result := 'Format Error! Format String: ' + AFormat + '. ';
    if Integer(High(Args)) >= 0 then
    begin
      Result := Result + #13#10'Hex Params:';
      for I := Low(Args) to High(Args) do
        Result := Result + Format(' %8.8x', [Args[I].VInteger]);
    end;
  end;
end;

function TCnDebugger.GetActive: Boolean;
begin
{$IFNDEF NDEBUG}
  Result := FActive;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TCnDebugger.GetCurrentIndent(ThrdID: Cardinal): Integer;
var
  Index: Integer;
begin
  Index := FThrdIDList.IndexOf(Pointer(ThrdID));
  if Index >= 0 then
  begin
    Result := Integer(FIndentList.Items[Index]);
  end
  else
  begin
    CnEnterCriticalSection(FCSThrdId);
    FThrdIDList.Add(Pointer(ThrdID));
    FIndentList.Add(nil);
    CnLeaveCriticalSection(FCSThrdId);
    Result := 0;
  end;
end;

function TCnDebugger.GetExceptTracking: Boolean;
begin
{$IFNDEF NDEBUG}
  Result := FExceptTracking;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TCnDebugger.IncIndent(ThrdID: Cardinal): Integer;
var
  Indent, Index: Integer;
begin
  Index := FThrdIDList.IndexOf(Pointer(ThrdID));
  if Index >= 0 then
  begin
    Indent := Integer(FIndentList.Items[Index]);
    Inc(Indent);
    FIndentList.Items[Index] := Pointer(Indent);
    Result := Indent;
  end
  else
  begin
    CnEnterCriticalSection(FCSThrdId);
    FThrdIDList.Add(Pointer(ThrdID));
    FIndentList.Add(Pointer(1));
    CnLeaveCriticalSection(FCSThrdId);
    Result := 1;
  end;
end;

function TCnDebugger.IndexOfTime(const ATag: string): PCnTimeDesc;
var
  I, Len: Integer;
  TTag: AnsiString;
  TmpTag: array[0..CnMaxTagLength - 1] of AnsiChar;
begin
  Result := nil;
  TTag := AnsiString(ATag);
  FillChar(TmpTag, CnMaxTagLength, 0);
  Len := Length(TTag);
  if Len > CnMaxTagLength then
    Len := CnMaxTagLength;

  Move(PAnsiChar(TTag)^, TmpTag, Len);
  for I := 0 to FTimes.Count - 1 do
  begin
    if FTimes[I] <> nil then
    begin
      if ((TTag = '') and (PCnTimeDesc(FTimes[I])^.Tag[0] = #0))
        or CompareMem(@(PCnTimeDesc(FTimes[I])^.Tag), @TmpTag, CnMaxTagLength) then
      begin
        Result := PCnTimeDesc(FTimes[I]);
        Exit;
      end;
    end
  end;
end;

procedure TCnDebugger.InternalOutput(var Data; Size: Integer);
begin
  if (FChannel = nil) or not FChannel.Active or not FChannel.CheckReady then Exit;
  if Size > 0 then
  begin
    FChannel.SendContent(Data, Size);
{$IFDEF MSWINDOWS}
    InterlockedIncrement(FPostedMessageCount);
{$ELSE}
    TInterlocked.Increment(FPostedMessageCount);
{$ENDIF}
  end;
end;

procedure TCnDebugger.InternalOutputMsg(const AMsg: PAnsiChar; Size: Integer;
  const ATag: AnsiString; ALevel, AIndent: Integer; AType: TCnMsgType;
  ThreadID: Cardinal; CPUPeriod: Int64);
var
  TagLen, MsgLen: Integer;
  MsgDesc: TCnMsgDesc;
  ChkReady, IsFirst: Boolean;
  MsgBufPtr: PAnsiChar;
  MsgBufSize: Integer;

  procedure GenerateMsgDesc(MsgBuf: PAnsiChar; MsgSize: Integer);
  begin
    // 进行具体的组装工作
    MsgLen := MsgSize;
    if MsgLen > CnMaxMsgLength then
      MsgLen := CnMaxMsgLength;
    TagLen := Length(ATag);
    if TagLen > CnMaxTagLength then
      TagLen := CnMaxTagLength;

    FillChar(MsgDesc, SizeOf(MsgDesc), 0);
    MsgDesc.Annex.Level := ALevel;
    MsgDesc.Annex.Indent := AIndent;
{$IFDEF MSWINDOWS}
    MsgDesc.Annex.ProcessId := GetCurrentProcessId;
{$ELSE}
    MsgDesc.Annex.ProcessId := getpid;
{$ENDIF}
    MsgDesc.Annex.ThreadId := ThreadID;
    MsgDesc.Annex.MsgType := Ord(AType);
    MsgDesc.Annex.TimeStampType := Ord(TimeStampType);

    case TimeStampType of
      ttDateTime: MsgDesc.Annex.MsgDateTime := Date + Time;
      ttTickCount: MsgDesc.Annex.MsgTickCount := {$IFNDEF MSWINDOWS}TThread.{$ENDIF}GetTickCount;
{$IFDEF MSWINDOWS}
      ttCPUPeriod: MsgDesc.Annex.MsgCPUPeriod := GetCPUPeriod;
{$ELSE}
      ttCPUPeriod: MsgDesc.Annex.MsgCPUPeriod := 0;
{$ENDIF}
    else
      MsgDesc.Annex.MsgCPUPeriod := 0; // 设为全 0
    end;

    // TimeMarkStop 时所耗 CPU 时钟周期数
    MsgDesc.Annex.MsgCPInterval := CPUPeriod;

    Move(Pointer(ATag)^, MsgDesc.Annex.Tag, TagLen);
    Move(Pointer(MsgBuf)^, MsgDesc.Msg, MsgLen);

    MsgLen := MsgLen + SizeOf(MsgDesc.Annex) + SizeOf(Cardinal);
    MsgDesc.Length := MsgLen;
  end;

begin
{$IFDEF REDIRECT_OPDS}
  if not FIgnoreViewer then
    OutputDebugStringA(AMsg);
{$ELSE}
  CnEnterCriticalSection(FStartCriticalSection);
  try
    if FAutoStart and not FIgnoreViewer and not FViewerAutoStartCalled then
    begin
      StartDebugViewer;
      FViewerAutoStartCalled := True;
    end;
  finally
    CnLeaveCriticalSection(FStartCriticalSection);
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  InterlockedIncrement(FMessageCount);
{$ELSE}
  TInterlocked.Increment(FMessageCount);
{$ENDIF}

{$IFDEF REDIRECT_OPDS}
  Exit;
{$ENDIF}

  if not CheckEnabled and not FDumpToFile then
  begin
    Sleep(0);
    Exit;
  end;

  if FChannel <> nil then
    ChkReady := FChannel.CheckReady
  else
    ChkReady := False;

  if not ChkReady and not FDumpToFile then
  begin
    Sleep(0);
    Exit;
  end;

  MsgBufPtr := AMsg;
  IsFirst := True;
  repeat
    if Size > CnMaxMsgLength then
      MsgBufSize := CnMaxMsgLength
    else
      MsgBufSize := Size;

    GenerateMsgDesc(MsgBufPtr, MsgBufSize);
    Dec(Size, MsgBufSize);
    Inc(MsgBufPtr, MsgBufSize);

    if IsFirst then
      IsFirst := False
    else
    begin
{$IFDEF MSWINDOWS}
      InterlockedIncrement(FMessageCount); // 拆包消息也要计数，但第一条在上头已计了
{$ELSE}
      TInterlocked.Increment(FMessageCount);
{$ENDIF}
    end;

    if ChkReady then
    begin
      if FChannel.CheckFilterChanged then
        FChannel.RefreshFilter(FFilter);

      if CheckFiltered(string(ATag), ALevel, AType) then
        InternalOutput(MsgDesc, MsgLen);
    end;

    // 同时 DumpToFile
    if FDumpToFile and not FIgnoreViewer and (FDumpFile <> nil) then
    begin
      if not FAfterFirstWrite then // 第一回写时需要判断是否重写
      begin
        if FUseAppend then
        begin
{$IFDEF MSWINDOWS}
          FDumpFile.Seek(0, soFromEnd);
{$ELSE}
          FDumpFile.Seek(0, soEnd);
{$ENDIF}
        end
        else
        begin
          FDumpFile.Size := 0;
{$IFDEF MSWINDOWS}
          FDumpFile.Seek(0, soFromBeginning);
{$ELSE}
          FDumpFile.Seek(0, soBeginning);
{$ENDIF}
        end;
        FAfterFirstWrite := True; // 后续写就无需判断了
      end;

      FDumpFile.Write(MsgDesc, MsgLen);
    end;
  until Size <= 0;
end;

procedure TCnDebugger.LogAssigned(Value: Pointer; const AMsg: string);
begin
{$IFDEF DEBUG}
  if Assigned(Value) then
  begin
    if AMsg = '' then
      LogMsg(SCnAssigned + SCnDefAssignedMsg)
    else
      LogMsg(SCnAssigned + AMsg);
  end
  else
  begin
    if AMsg = '' then
      LogMsg(SCnUnAssigned + SCnDefAssignedMsg)
    else
      LogMsg(SCnUnAssigned + AMsg);
  end;
{$ENDIF}
end;

procedure TCnDebugger.LogBoolean(Value: Boolean; const AMsg: string);
begin
{$IFDEF DEBUG}
  if Value then
  begin
    if AMsg = '' then
      LogMsg(SCnBooleanTrue + SCnDefBooleanMsg)
    else
      LogMsg(SCnBooleanTrue + AMsg);
  end
  else
  begin
    if AMsg = '' then
      LogMsg(SCnBooleanFalse + SCnDefBooleanMsg)
    else
      LogMsg(SCnBooleanFalse + AMsg);
  end;
{$ENDIF}
end;

procedure TCnDebugger.LogCollectionWithTag(ACollection: TCollection;
  const ATag: string);
{$IFDEF DEBUG}
var
  List: TStringList;
{$ENDIF}
begin
{$IFDEF DEBUG}
  List := nil;
  try
    List := TStringList.Create;
    try
      CollectionToStringList(ACollection, List);
    except
      List.Add(SCnObjException);
    end;
    LogMsgWithTypeTag(List.Text, cmtObject, ATag);
  finally
    List.Free;
  end;
{$ENDIF}
end;

procedure TCnDebugger.LogCollection(ACollection: TCollection);
begin
{$IFDEF DEBUG}
  LogCollectionWithTag(ACollection, CurrentTag);
{$ENDIF}
end;

procedure TCnDebugger.LogColor(Color: TColor; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnColor + ColorToString(Color))
  else
    LogFmt('%s %s', [AMsg, ColorToString(Color)]);
{$ENDIF}
end;

procedure TCnDebugger.LogComponent(AComponent: TComponent);
begin
{$IFDEF DEBUG}
  LogComponentWithTag(AComponent, CurrentTag);
{$ENDIF}
end;

procedure TCnDebugger.LogComponentWithTag(AComponent: TComponent;
  const ATag: string);
{$IFDEF DEBUG}
var
  InStream, OutStream: TMemoryStream;
  ThrdID: Cardinal;
{$ENDIF}
begin
{$IFDEF DEBUG}
  InStream := nil; OutStream := nil;
  try
    InStream := TMemoryStream.Create;
    OutStream := TMemoryStream.Create;

    if Assigned(AComponent) then
    begin
      InStream.WriteComponent(AComponent);
{$IFDEF MSWINDOWS}
      InStream.Seek(0, soFromBeginning);
{$ELSE}
      InStream.Seek(0, soBeginning);
{$ENDIF}
      ObjectBinaryToText(InStream, OutStream);
      ThrdID := GetCurrentThreadId;
      InternalOutputMsg(PAnsiChar(OutStream.Memory), OutStream.Size, AnsiString(ATag), CurrentLevel,
        GetCurrentIndent(ThrdID), cmtComponent, ThrdID, 0);
    end
    else
      LogMsgWithTypeTag(SCnNilComponent, cmtComponent, ATag);
  finally
    InStream.Free;
    OutStream.Free;
  end;
{$ENDIF}
end;

procedure TCnDebugger.LogEnter(const AProcName, ATag: string);
begin
{$IFDEF DEBUG}
  LogFull(SCnEnterProc + AProcName, ATag, CurrentLevel, cmtEnterProc);
  IncIndent(GetCurrentThreadId);
{$ENDIF}
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

procedure TCnDebugger.LogEnumType<T>(const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg('EnumType: ' + GetEnumTypeStr<T>)
  else
    LogFmt('%s %s', [AMsg, GetEnumTypeStr<T>]);
{$ENDIF}
end;

{$ENDIF}

procedure TCnDebugger.LogException(E: Exception; const AMsg: string);
begin
{$IFDEF DEBUG}
  if not Assigned(E) then
    Exit;

  if AMsg = '' then
    LogFmt('%s %s - %s', [SCnException, E.ClassName, E.Message])
  else
    LogFmt('%s %s - %s', [AMsg, E.ClassName, E.Message]);
{$ENDIF}
end;

procedure TCnDebugger.LogFloat(Value: Extended; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnFloat + FloatToStr(Value))
  else
    LogFmt('%s %s', [AMsg, FloatToStr(Value)]);
{$ENDIF}
end;

procedure TCnDebugger.LogFmt(const AFormat: string; Args: array of const);
begin
{$IFDEF DEBUG}
  LogFull(FormatMsg(AFormat, Args), CurrentTag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogFmtWithLevel(const AFormat: string;
  Args: array of const; ALevel: Integer);
begin
{$IFDEF DEBUG}
  LogFull(FormatMsg(AFormat, Args), CurrentTag, ALevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogFmtWithTag(const AFormat: string;
  Args: array of const; const ATag: string);
begin
{$IFDEF DEBUG}
  LogFull(FormatMsg(AFormat, Args), ATag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogFmtWithType(const AFormat: string;
  Args: array of const; AType: TCnMsgType);
begin
{$IFDEF DEBUG}
  LogFull(FormatMsg(AFormat, Args), CurrentTag, CurrentLevel, AType);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgError(const AMsg: string);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, CurrentTag, CurrentLevel, cmtError);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgWarning(const AMsg: string);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, CurrentTag, CurrentLevel, cmtWarning);
{$ENDIF}
end;

procedure TCnDebugger.LogErrorFmt(const AFormat: string;
  Args: array of const);
begin
{$IFDEF DEBUG}
  LogFull(FormatMsg(AFormat, Args), CurrentTag, CurrentLevel, cmtError);
{$ENDIF}
end;

procedure TCnDebugger.LogFull(const AMsg, ATag: string; ALevel: Integer;
  AType: TCnMsgType; CPUPeriod: Int64 = 0);
{$IFDEF DEBUG}
{$IFNDEF NDEBUG}
var
  ThrdID: Cardinal;
{$IFDEF UNICODE}
  Msg: AnsiString;
{$ENDIF}
{$ENDIF}
{$ENDIF}
begin
{$IFDEF DEBUG}
{$IFNDEF NDEBUG}
  if AMsg = '' then Exit;
  ThrdID := GetCurrentThreadId;
  {$IFDEF UNICODE}
  Msg := AnsiString(AMsg);
  InternalOutputMsg(PAnsiChar(Msg), Length(Msg), AnsiString(ATag),
    ALevel, GetCurrentIndent(ThrdID), AType, ThrdID, CPUPeriod);
  {$ELSE}
  InternalOutputMsg(PAnsiChar(AMsg), Length(AnsiString(AMsg)), AnsiString(ATag),
    ALevel, GetCurrentIndent(ThrdID), AType, ThrdID, CPUPeriod);
  {$ENDIF}
{$ENDIF}
{$ENDIF}
end;

procedure TCnDebugger.LogInteger(Value: Integer; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnInteger + IntToStr(Value))
  else
    LogFmt('%s %d', [AMsg, Value]);
{$ENDIF}
end;

procedure TCnDebugger.LogInt64(Value: Int64; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnInt64 + IntToStr(Value))
  else
    LogFmt('%s %d', [AMsg, Value]);
{$ENDIF}
end;

{$IFDEF SUPPORT_UINT64}

procedure TCnDebugger.LogUInt64(Value: UInt64; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFmt('%s%u', [SCnUInt64, Value])
  else
    LogFmt('%s %u', [AMsg, Value]);
{$ENDIF}
end;

{$ENDIF}

procedure TCnDebugger.LogChar(Value: Char; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFmt(SCnCharFmt, [Value, Ord(Value), Ord(Value)])
  else
  begin
{$IFDEF UNICODE}
    LogFmt('%s ''%s''(%d/$%4.4x)', [AMsg, Value, Ord(Value), Ord(Value)]);
{$ELSE}
    LogFmt('%s ''%s''(%d/$%2.2x)', [AMsg, Value, Ord(Value), Ord(Value)]);
{$ENDIF}
  end;
{$ENDIF}
end;

procedure TCnDebugger.LogAnsiChar(Value: AnsiChar; const AMsg: string = '');
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFmt(SCnAnsiCharFmt, [Value, Ord(Value), Ord(Value)])
  else
    LogFmt('%s ''%s''(%d/$%2.2x)', [AMsg, Value, Ord(Value), Ord(Value)]);
{$ENDIF}
end;

procedure TCnDebugger.LogWideChar(Value: WideChar; const AMsg: string = '');
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFmt(SCnWideCharFmt, [Value, Ord(Value), Ord(Value)])
  else
    LogFmt('%s ''%s''(%d/$%4.4x)', [AMsg, Value, Ord(Value), Ord(Value)]);
{$ENDIF}
end;

procedure TCnDebugger.LogSet(const ASet; ASetSize: Integer;
  SetElementTypInfo: PTypeInfo; const AMsg: string);
{$IFDEF DEBUG}
var
  SetVal: Integer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  if (ASetSize <= 0) or (ASetSize > SizeOf(Integer)) then
  begin
    LogException(EInvalidCast.Create(AMsg));
    Exit;
  end;

  SetVal := 0;
  Move(ASet, SetVal, ASetSize);
  if AMsg = '' then
    LogMsg(GetSetStr(SetElementTypInfo, SetVal))
  else
    LogFmt('%s %s', [AMsg, GetSetStr(SetElementTypInfo, SetVal)]);
{$ENDIF}
end;

procedure TCnDebugger.LogDateTime(Value: TDateTime; const AMsg: string = '' );
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnDateTime + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Value))
  else
    LogMsg(AMsg + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Value));
{$ENDIF}
end;

procedure TCnDebugger.LogDateTimeFmt(Value: TDateTime; const AFmt: string; const AMsg: string = '' );
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnDateTime + FormatDateTime(AFmt, Value))
  else
    LogMsg(AMsg + FormatDateTime(AFmt, Value));
{$ENDIF}
end;

procedure TCnDebugger.LogPointer(Value: Pointer; const AMsg: string = '');
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFmt('%s $%p', [SCnPointer, Value])
  else
    LogFmt('%s $%p', [AMsg, Value]);
{$ENDIF}
end;

procedure TCnDebugger.LogLeave(const AProcName, ATag: string);
begin
{$IFDEF DEBUG}
  DecIndent(GetCurrentThreadId);
  LogFull(SCnLeaveProc + AProcName, ATag, CurrentLevel, cmtLeaveProc);
{$ENDIF}
end;

procedure TCnDebugger.LogMemDump(AMem: Pointer; Size: Integer);
{$IFDEF DEBUG}
var
  ThrdID: Cardinal;
{$ENDIF}
begin
{$IFDEF DEBUG}
  ThrdID := GetCurrentThreadId;
  InternalOutputMsg(PAnsiChar(AMem), Size, AnsiString(CurrentTag), CurrentLevel, GetCurrentIndent(ThrdID),
    cmtMemoryDump, ThrdID, 0);
{$ENDIF}
end;

procedure TCnDebugger.LogBitmapMemory(ABmp: TBitmap);
{$IFDEF DEBUG}
var
  H, B: Integer;
  E: Boolean;
{$IFDEF ENABLE_FMX}
  D: TBitmapData;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF DEBUG}
  {$IFDEF ENABLE_FMX}
  E := (ABmp <> nil) and ABmp.IsEmpty;
  {$ELSE}
  E := (ABmp <> nil) and ABmp.Empty;
  {$ENDIF}

  if (ABmp <> nil) and not E then
  begin
    LogFmt('Bmp Width %d, Height %d.', [ABmp.Width, ABmp.Height]);

    B := GetBitmapPixelBytesCount(ABmp.PixelFormat);
{$IFDEF ENABLE_FMX}
    if ABmp.Map(TMapAccess.Read, D) then
      for H := 0 to ABmp.Height - 1 do
        LogMemDump(D.GetScanline(H), ABmp.Width * B);
{$ELSE}
    for H := 0 to ABmp.Height - 1 do
      LogMemDump(ABmp.ScanLine[H], ABmp.Width * B);
{$ENDIF}
  end;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

procedure TCnDebugger.LogVirtualKey(AKey: Word);
begin
{$IFDEF DEBUG}
  LogVirtualKeyWithTag(AKey, CurrentTag);
{$ENDIF}
end;

procedure TCnDebugger.LogVirtualKeyWithTag(AKey: Word; const ATag: string);
begin
{$IFDEF DEBUG}
  LogFmtWithTag(SCnVirtualKeyFmt, [AKey, AKey, VirtualKeyToString(AKey)], ATag);
{$ENDIF}
end;

procedure TCnDebugger.LogWindowMessage(AMessage: Cardinal);
begin
{$IFDEF DEBUG}
  LogWindowMessageWithTag(AMessage, CurrentTag);
{$ENDIF}
end;

procedure TCnDebugger.LogWindowMessageWithTag(AMessage: Cardinal; const ATag: string);
begin
{$IFDEF DEBUG}
  LogMsgWithTag(WindowMessageToStr(AMessage), ATag);
{$ENDIF}
end;

{$ENDIF}

procedure TCnDebugger.LogMsg(const AMsg: string);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, CurrentTag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgWithLevel(const AMsg: string; ALevel: Integer);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, CurrentTag, ALevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgWithLevelType(const AMsg: string;
  ALevel: Integer; AType: TCnMsgType);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, CurrentTag, ALevel, AType);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgWithTag(const AMsg, ATag: string);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, ATag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgWithTagLevel(const AMsg, ATag: string;
  ALevel: Integer);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, ATag, ALevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgWithType(const AMsg: string;
  AType: TCnMsgType);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, CurrentTag, CurrentLevel, AType);
{$ENDIF}
end;

procedure TCnDebugger.LogMsgWithTypeTag(const AMsg: string;
  AType: TCnMsgType; const ATag: string);
begin
{$IFDEF DEBUG}
  LogFull(AMsg, ATag, CurrentLevel, AType);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

procedure TCnDebugger.LogLastError;
begin
{$IFDEF DEBUG}
  TraceLastError;
{$ENDIF}
end;

{$ENDIF}

procedure TCnDebugger.LogObject(AObject: TObject);
begin
{$IFDEF DEBUG}
  LogObjectWithTag(AObject, CurrentTag);
{$ENDIF}
end;

procedure TCnDebugger.LogObjectWithTag(AObject: TObject;
  const ATag: string);
{$IFDEF DEBUG}
var
  List: TStringList;
  Intfs: string;
{$ENDIF}
begin
{$IFDEF DEBUG}
  if AObject = nil then
  begin
    LogMsgWithTypeTag('Object: nil', cmtObject, ATag);
    Exit;
  end;

  List := nil;
  try
    List := TStringList.Create;
    try
      AddObjectToStringList(AObject, List, 0);
      Intfs := FormatObjectInterface(AObject);
      if Intfs <> '' then
      begin
        List.Add('Supports Interfaces:');
        List.Add(Intfs);
      end;
    except
      List.Add(SCnObjException);
    end;
    LogMsgWithTypeTag('Object: $' + IntToHex(TCnNativeInt(AObject), CN_HEX_DIGITS) + SCnCRLF +
      List.Text, cmtObject, ATag);
  finally
    List.Free;
  end;
{$ENDIF}
end;

procedure TCnDebugger.LogPoint(Point: TPoint; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnPoint + PointToString(Point))
  else
    LogFmt('%s %s', [AMsg, PointToString(Point)]);
{$ENDIF}
end;

procedure TCnDebugger.LogSize(Size: TSize; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnSize + SizeToString(Size))
  else
    LogFmt('%s %s', [AMsg, SizeToString(Size)]);
{$ENDIF}
end;

procedure TCnDebugger.LogRect(Rect: TRect; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnRect + RectToString(Rect))
  else
    LogFmt('%s %s', [AMsg, RectToString(Rect)]);
{$ENDIF}
end;

procedure TCnDebugger.LogBits(Bits: TBits; const AMsg: string = '');
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(BitsToString(Bits))
  else
    LogFmt('%s %s', [AMsg, BitsToString(Bits)]);
{$ENDIF}
end;

procedure TCnDebugger.LogGUID(const GUID: TGUID; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogMsg(SCnGUID + GUIDToString(GUID))
  else
    LogFmt('%s %s', [AMsg, GUIDToString(GUID)]);
{$ENDIF}
end;

procedure TCnDebugger.LogSeparator;
begin
{$IFDEF DEBUG}
  LogFull('-', CurrentTag, CurrentLevel, cmtSeparator);
{$ENDIF}
end;


procedure TCnDebugger.LogRawString(const Value: string);
begin
{$IFDEF DEBUG}
  if Value <> '' then
    LogMemDump(Pointer(Value), Length(Value) * SizeOf(Char));
{$ENDIF}
end;

procedure TCnDebugger.LogRawAnsiString(const Value: AnsiString);
begin
{$IFDEF DEBUG}
  if Value <> '' then
    LogMemDump(Pointer(Value), Length(Value) * SizeOf(AnsiChar));
{$ENDIF}
end;

procedure TCnDebugger.LogRawWideString(const Value: WideString);
begin
{$IFDEF DEBUG}
  if Value <> '' then
    LogMemDump(Pointer(Value), Length(Value) * SizeOf(WideChar));
{$ENDIF}
end;

procedure TCnDebugger.LogStrings(Strings: TStrings; const AMsg: string);
begin
{$IFDEF DEBUG}
  if not Assigned(Strings) then
    Exit;

  if AMsg = '' then
    LogMsg(Strings.Text)
  else
    LogMsg(AMsg + SCnCRLF + Strings.Text);
{$ENDIF}
end;

procedure TCnDebugger.LogCurrentStack(const AMsg: string);
{$IFDEF DEBUG}
{$IFDEF CAPTURE_STACK}
var
  Strings: TStrings;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF DEBUG}
{$IFDEF CAPTURE_STACK}
  Strings := nil;

  try
    Strings := TStringList.Create;
    GetCurrentTrace(Strings);

    LogMsgWithType('Dump Call Stack: ' + AMsg + SCnCRLF + Strings.Text, cmtInformation);
  finally
    Strings.Free;
  end;
{$ENDIF}
{$ENDIF}
end;

procedure TCnDebugger.LogConstArray(const Arr: array of const;
  const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFull(FormatMsg('%s %s', [SCnConstArray, FormatConstArray(Arr)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    LogFull(FormatMsg('%s %s', [AMsg, FormatConstArray(Arr)]), CurrentTag,
      CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogIntegerArray(const Arr: array of Integer; const AMsg: string);
{$IFDEF DEBUG}
var
  P: Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  if Length(Arr) = 0 then
    P := nil
  else
    P := @Arr[0];

  if AMsg = '' then
    LogFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(P, Length(Arr), SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    LogFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(P, Length(Arr), SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogIntegerArray(const ArrAddr: Pointer; Count: Integer;
  const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(ArrAddr, Count, SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    LogFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(ArrAddr, Count, SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogCardinalArray(const Arr: array of Cardinal; const AMsg: string);
{$IFDEF DEBUG}
var
  P: Pointer;
{$ENDIF}
begin
{$IFDEF DEBUG}
  if Length(Arr) = 0 then
    P := nil
  else
    P := @Arr[0];

  if AMsg = '' then
    LogFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(P, Length(Arr), SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    LogFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(P, Length(Arr), SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

procedure TCnDebugger.LogCardinalArray(const ArrAddr: Pointer; Count: Integer;
  const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(ArrAddr, Count, SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    LogFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(ArrAddr, Count, SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
{$ENDIF}
end;

function TCnDebugger.IntArrayToString(ArrayAddress: Pointer;
  ElementCount, ElementSize: Integer; Sign: Boolean): string;
var
  I: Integer;
  PtrInt8: PShortInt;
  PtrUInt8: PByte;
  PtrInt16: PSmallInt;
  PtrUInt16: PWORD;
  PtrInt32: PInteger;
  PtrUInt32: PDWORD;
begin
  if (ArrayAddress = nil) or (ElementCount = 0) or (ElementSize <= 0) then
  begin
    Result := SCnEmptyArray;
    Exit;
  end;

  Result := '';
  case ElementSize of
    1:
      begin
        if Sign then
        begin
          PtrInt8 := PShortInt(ArrayAddress);
          for I := 0 to ElementCount - 1 do
          begin
            if I = 0 then
              Result := Format('%d', [PtrInt8^])
            else
              Result := Result + ',' + Format('%d', [PtrInt8^]);
            Inc(PtrInt8);
          end;
        end
        else
        begin
          PtrUInt8 := PByte(ArrayAddress);
          for I := 0 to ElementCount - 1 do
          begin
            if I = 0 then
              Result := Format('%u', [PtrUInt8^])
            else
              Result := Result + ',' + Format('%u', [PtrUInt8^]);
            Inc(PtrUInt8);
          end;
        end;
      end;
    2:
      begin
        if Sign then
        begin
          PtrInt16 := PSmallInt(ArrayAddress);
          for I := 0 to ElementCount - 1 do
          begin
            if I = 0 then
              Result := Format('%d', [PtrInt16^])
            else
              Result := Result + ',' + Format('%d', [PtrInt16^]);
            Inc(PtrInt16);
          end;
        end
        else
        begin
          PtrUInt16 := PWord(ArrayAddress);
          for I := 0 to ElementCount - 1 do
          begin
            if I = 0 then
              Result := Format('%u', [PtrUInt16^])
            else
              Result := Result + ',' + Format('%u', [PtrUInt16^]);
            Inc(PtrUInt16);
          end;
        end;
      end;
    4:
      begin
        if Sign then
        begin
          PtrInt32 := PInteger(ArrayAddress);
          for I := 0 to ElementCount - 1 do
          begin
            if I = 0 then
              Result := Format('%d', [PtrInt32^])
            else
              Result := Result + ',' + Format('%d', [PtrInt32^]);
            Inc(PtrInt32);
          end;
        end
        else
        begin
          PtrUInt32 := PDWORD(ArrayAddress);
          for I := 0 to ElementCount - 1 do
          begin
            if I = 0 then
              Result := Format('%u', [PtrUInt32^])
            else
              Result := Result + ',' + Format('%u', [PtrUInt32^]);
            Inc(PtrUInt32);
          end;
        end;
      end;
  end;
end;

function TCnDebugger.PointToString(APoint: TPoint): string;
begin
  Result := '(' + IntToStr(APoint.x) + ',' + IntToStr(APoint.y) + ')';
end;

function TCnDebugger.SizeToString(ASize: TSize): string;
begin
  Result := '(cx: ' + IntToStr(ASize.cx) + ', cy: ' + IntToStr(ASize.cy) + ')';
end;

function TCnDebugger.RectToString(ARect: TRect): string;
begin
  Result := '(Left/Top: ' + PointToString(ARect.TopLeft) + ', Right/Bottom: ' +
    PointToString(ARect.BottomRight) + ')';
end;

function TCnDebugger.BitsToString(ABits: TBits): string;
var
  I: Integer;
begin
  if (ABits = nil) or (ABits.Size = 0) then
    Result := 'No Bits.'
  else
  begin
    SetLength(Result, ABits.Size);
    for I := 0 to ABits.Size - 1 do
    begin
      if ABits.Bits[I] then
        Result[I + 1] := '1'
      else
        Result[I + 1] := '0';
    end;
    Result := 'Size: ' + IntToStr(ABits.Size) + '. Bits: ' + Result;
  end;
end;

procedure TCnDebugger.RemoveFilterExceptClass(E: ExceptClass);
var
  I: Integer;
begin
  I := FExceptFilter.IndexOf(E.ClassName);
  if I >= 0 then
    FExceptFilter.Delete(I);
end;

procedure TCnDebugger.RemoveFilterExceptClass(const EClassName: string);
var
  I: Integer;
begin
  I := FExceptFilter.IndexOf(EClassName);
  if I >= 0 then
    FExceptFilter.Delete(I);
end;

procedure TCnDebugger.SetActive(const Value: Boolean);
begin
{$IFNDEF NDEBUG}
  FActive := Value;
{$ENDIF}
end;

procedure TCnDebugger.SetExceptTracking(const Value: Boolean);
begin
{$IFNDEF NDEBUG}
  FExceptTracking := Value;
  {$IFDEF CAPTURE_STACK}
  if FExceptTracking then
    CnHookException
  else
    CnUnHookException;
  {$ENDIF}
{$ENDIF}
end;

procedure TCnDebugger.StartDebugViewer;
begin
  if FChannel <> nil then
    FChannel.StartDebugViewer;
end;

procedure TCnDebugger.StartTimeMark(const ATag, AMsg: string);
{$IFNDEF NDEBUG}
var
  ADesc: PCnTimeDesc;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  // 根据 ATag 找是否存在以前的记录，不存在则新增
  ADesc := IndexOfTime(ATag);
  if ADesc = nil then
    ADesc := AddTimeDesc(ATag);

  if ADesc <> nil then
  begin
//    不发记录，以降低误差，原理不详，惭愧
//    if AMsg <> '' then
//      TraceFull(AMsg, ATag, DefLevel, mtTimeMarkStart)
//    else
//      TraceFull(SCnTimeMarkStarted, ATag, DefLevel, mtTimeMarkStart);

    // 最后记录当时的 CPU 周期
    Inc(ADesc^.PassCount);
{$IFDEF MSWINDOWS}
    ADesc^.StartTime := GetCPUPeriod;
{$ELSE}
    ADesc^.StartTime := 0;
{$ENDIF}
  end;
{$ENDIF}
end;

procedure TCnDebugger.StartTimeMark(const ATag: Integer;
  const AMsg: string);
begin
  StartTimeMark(Copy('#' + IntToStr(ATag), 1, CnMaxTagLength), AMsg);
end;

procedure TCnDebugger.StopTimeMark(const ATag, AMsg: string);
{$IFNDEF NDEBUG}
var
  Period: Int64;
  ADesc: PCnTimeDesc;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  // 马上记录当时的 CPU 周期
{$IFDEF MSWINDOWS}
  Period := GetCPUPeriod;
{$ELSE}
  Period := 0;
{$ENDIF}

  ADesc := IndexOfTime(ATag);
  if ADesc <> nil then
  begin
    // 得到相应的旧记录，相减，并减去误差，叠加上一次的计时，作为记录发出去
    ADesc^.AccuTime := ADesc^.AccuTime + (Period - ADesc^.StartTime - FFixedCalling);

    if AMsg <> '' then
      TraceFull(AMsg, ATag, CurrentLevel, cmtTimeMarkStop, ADesc^.AccuTime)
    else
      TraceFull(SCnTimeMarkStopped, ATag, CurrentLevel, cmtTimeMarkStop, ADesc^.AccuTime);
  end;
{$ENDIF}
end;

procedure TCnDebugger.StopTimeMark(const ATag: Integer;
  const AMsg: string);
begin
  StopTimeMark(Copy('#' + IntToStr(ATag), 1, CnMaxTagLength), AMsg);
end;

procedure TCnDebugger.TraceAssigned(Value: Pointer; const AMsg: string);
begin
  if Assigned(Value) then
  begin
    if AMsg = '' then
      TraceMsg(SCnAssigned + SCnDefAssignedMsg)
    else
      TraceMsg(SCnAssigned + AMsg);
  end
  else
  begin
    if AMsg = '' then
      TraceMsg(SCnUnAssigned + SCnDefAssignedMsg)
    else
      TraceMsg(SCnUnAssigned + AMsg);
  end;
end;

procedure TCnDebugger.TraceBoolean(Value: Boolean;
  const AMsg: string);
begin
  if Value then
  begin
    if AMsg = '' then
      TraceMsg(SCnBooleanTrue + SCnDefBooleanMsg)
    else
      TraceMsg(SCnBooleanTrue + AMsg);
  end
  else
  begin
    if AMsg = '' then
      TraceMsg(SCnBooleanFalse + SCnDefBooleanMsg)
    else
      TraceMsg(SCnBooleanFalse + AMsg);
  end;
end;

procedure TCnDebugger.TraceCollection(ACollection: TCollection);
begin
  TraceCollectionWithTag(ACollection, CurrentTag);
end;

procedure TCnDebugger.TraceCollectionWithTag(ACollection: TCollection;
  const ATag: string);
{$IFNDEF NDEBUG}
var
  List: TStringList;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  List := nil;
  try
    List := TStringList.Create;
    try
      CollectionToStringList(ACollection, List);
    except
      List.Add(SCnObjException);
    end;
    TraceMsgWithTypeTag(List.Text, cmtObject, ATag);
  finally
    List.Free;
  end;
{$ENDIF}
end;

procedure TCnDebugger.TraceColor(Color: TColor; const AMsg: string);
begin
  if AMsg = '' then
    TraceMsg(SCnColor + ColorToString(Color))
  else
    TraceFmt('%s %s', [AMsg, ColorToString(Color)]);
end;

procedure TCnDebugger.TraceComponent(AComponent: TComponent);
begin
  TraceComponentWithTag(AComponent, CurrentTag);
end;

procedure TCnDebugger.TraceComponentWithTag(AComponent: TComponent;
  const ATag: string);
{$IFNDEF NDEBUG}
var
  InStream, OutStream: TMemoryStream;
  ThrdID: Cardinal;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  InStream := nil; OutStream := nil;
  try
    InStream := TMemoryStream.Create;
    OutStream := TMemoryStream.Create;

    if Assigned(AComponent) then
    begin
      InStream.WriteComponent(AComponent);
{$IFDEF MSWINDOWS}
      InStream.Seek(0, soFromBeginning);
{$ELSE}
      InStream.Seek(0, soBeginning);
{$ENDIF}
      ObjectBinaryToText(InStream, OutStream);
      ThrdID := GetCurrentThreadId;
      InternalOutputMsg(PAnsiChar(OutStream.Memory), OutStream.Size, AnsiString(ATag), CurrentLevel,
        GetCurrentIndent(ThrdID), cmtComponent, ThrdID, 0);
    end
    else
      TraceMsgWithTypeTag(SCnNilComponent, cmtComponent, ATag);
  finally
    InStream.Free;
    OutStream.Free;
  end;
{$ENDIF}
end;

procedure TCnDebugger.TraceEnter(const AProcName, ATag: string);
begin
  TraceFull(SCnEnterProc + AProcName, ATag, CurrentLevel, cmtEnterProc);
{$IFNDEF NDEBUG}
  IncIndent(GetCurrentThreadId);
{$ENDIF}
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

procedure TCnDebugger.TraceEnumType<T>(const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    TraceMsg('EnumType: ' + GetEnumTypeStr<T>)
  else
    TraceFmt('%s %s', [AMsg, GetEnumTypeStr<T>]);
{$ENDIF}
end;

{$ENDIF}

procedure TCnDebugger.TraceException(E: Exception; const AMsg: string);
begin
  if not Assigned(E) then
    Exit;

  if AMsg = '' then
    TraceFmt('%s %s - %s', [SCnException, E.ClassName, E.Message])
  else
    TraceFmt('%s %s - %s', [AMsg, E.ClassName, E.Message]);
end;

procedure TCnDebugger.TraceFloat(Value: Extended; const AMsg: string);
begin
  if AMsg = '' then
    TraceMsg(SCnFloat + FloatToStr(Value))
  else
    TraceFmt('%s %s', [AMsg, FloatToStr(Value)]);
end;

procedure TCnDebugger.TraceFmt(const AFormat: string;
  Args: array of const);
begin
  TraceFull(FormatMsg(AFormat, Args), CurrentTag, CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceFmtWithLevel(const AFormat: string;
  Args: array of const; ALevel: Integer);
begin
  TraceFull(FormatMsg(AFormat, Args), CurrentTag, ALevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceFmtWithTag(const AFormat: string;
  Args: array of const; const ATag: string);
begin
  TraceFull(FormatMsg(AFormat, Args), ATag, CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceFmtWithType(const AFormat: string;
  Args: array of const; AType: TCnMsgType);
begin
  TraceFull(FormatMsg(AFormat, Args), CurrentTag, CurrentLevel, AType);
end;

procedure TCnDebugger.TraceFull(const AMsg, ATag: string; ALevel: Integer;
  AType: TCnMsgType; CPUPeriod: Int64 = 0);
{$IFNDEF NDEBUG}
var
  ThrdID: Cardinal;
{$IFDEF UNICODE}
  Msg: AnsiString;
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  if AMsg = '' then Exit;
  ThrdID := GetCurrentThreadId;
  {$IFDEF UNICODE}
  Msg := AnsiString(AMsg);
  InternalOutputMsg(PAnsiChar(Msg), Length(Msg), AnsiString(ATag),
    ALevel, GetCurrentIndent(ThrdID), AType, ThrdID, CPUPeriod);
  {$ELSE}
  InternalOutputMsg(PAnsiChar(AMsg), Length(AMsg), AnsiString(ATag),
    ALevel, GetCurrentIndent(ThrdID), AType, ThrdID, CPUPeriod);
  {$ENDIF}
{$ENDIF}
end;

procedure TCnDebugger.TraceInteger(Value: Integer;
  const AMsg: string);
begin
  if AMsg = '' then
    TraceMsg(SCnInteger + IntToStr(Value))
  else
    TraceFmt('%s %d', [AMsg, Value]);
end;

procedure TCnDebugger.TraceInt64(Value: Int64; const AMsg: string);
begin
  if AMsg = '' then
    TraceMsg(SCnInt64 + IntToStr(Value))
  else
    TraceFmt('%s %d', [AMsg, Value]);
end;

{$IFDEF SUPPORT_UINT64}

procedure TCnDebugger.TraceUInt64(Value: UInt64; const AMsg: string);
begin
  if AMsg = '' then
    LogFmt('%s%u', [SCnUInt64, Value])
  else
    LogFmt('%s %u', [AMsg, Value]);
end;

{$ENDIF}


procedure TCnDebugger.TraceChar(Value: Char; const AMsg: string);
begin
  if AMsg = '' then
    TraceFmt(SCnCharFmt, [Value, Ord(Value), Ord(Value)])
  else
  begin
{$IFDEF UNICODE}
    TraceFmt('%s ''%s''(%d/$%4.4x)', [AMsg, Value, Ord(Value), Ord(Value)]);
{$ELSE}
    TraceFmt('%s ''%s''(%d/$%2.2x)', [AMsg, Value, Ord(Value), Ord(Value)]);
{$ENDIF}
  end;
end;

procedure TCnDebugger.TraceAnsiChar(Value: AnsiChar; const AMsg: string = '');
begin
  if AMsg = '' then
    TraceFmt(SCnAnsiCharFmt, [Value, Ord(Value), Ord(Value)])
  else
    TraceFmt('%s ''%s''(%d/$%2.2x)', [AMsg, Value, Ord(Value), Ord(Value)]);
end;

procedure TCnDebugger.TraceWideChar(Value: WideChar; const AMsg: string = '');
begin
  if AMsg = '' then
    TraceFmt(SCnWideCharFmt, [Value, Ord(Value), Ord(Value)])
  else
    TraceFmt('%s ''%s''(%d/$%4.4x)', [AMsg, Value, Ord(Value), Ord(Value)]);
end;

procedure TCnDebugger.TraceSet(const ASet; ASetSize: Integer;
  SetElementTypInfo: PTypeInfo; const AMsg: string);
var
  SetVal: Integer;
begin
  if (ASetSize <= 0) or (ASetSize > SizeOf(Integer)) then
  begin
    TraceException(EInvalidCast.Create(AMsg));
    Exit;
  end;

  SetVal := 0;
  Move(ASet, SetVal, ASetSize);
  if AMsg = '' then
    TraceMsg(GetSetStr(SetElementTypInfo, SetVal))
  else
    TraceFmt('%s %s', [AMsg, GetSetStr(SetElementTypInfo, SetVal)]);
end;

procedure TCnDebugger.TraceDateTime(Value: TDateTime; const AMsg: string = '' );
begin
  if AMsg = '' then
    TraceMsg(SCnDateTime + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Value))
  else
    TraceMsg(AMsg + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Value));
end;

procedure TCnDebugger.TraceDateTimeFmt(Value: TDateTime; const AFmt: string; const AMsg: string = '' );
begin
  if AMsg = '' then
    TraceMsg(SCnDateTime + FormatDateTime(AFmt, Value))
  else
    TraceMsg(AMsg + FormatDateTime(AFmt, Value));
end;

procedure TCnDebugger.TracePointer(Value: Pointer; const AMsg: string = '');
begin
  if AMsg = '' then
    TraceFmt('%s $%p', [SCnPointer, Value])
  else
    TraceFmt('%s $%p', [AMsg, Value]);
end;

procedure TCnDebugger.TraceLeave(const AProcName, ATag: string);
begin
{$IFNDEF NDEBUG}
  DecIndent(GetCurrentThreadId);
{$ENDIF}
  TraceFull(SCnLeaveProc + AProcName, ATag, CurrentLevel, cmtLeaveProc);
end;

procedure TCnDebugger.TraceMemDump(AMem: Pointer; Size: Integer);
{$IFNDEF NDEBUG}
var
  ThrdID: Cardinal;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  ThrdID := GetCurrentThreadId;
  InternalOutputMsg(PAnsiChar(AMem), Size, AnsiString(CurrentTag), CurrentLevel, GetCurrentIndent(ThrdID),
    cmtMemoryDump, ThrdID, 0);
{$ENDIF}
end;

procedure TCnDebugger.TraceBitmapMemory(ABmp: TBitmap);
{$IFNDEF NDEBUG}
var
  H, B: Integer;
  E: Boolean;
{$IFDEF ENABLE_FMX}
  D: TBitmapData;
{$ENDIF}
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  {$IFDEF ENABLE_FMX}
  E := (ABmp <> nil) and ABmp.IsEmpty;
  {$ELSE}
  E := (ABmp <> nil) and ABmp.Empty;
  {$ENDIF}

  if (ABmp <> nil) and not E then
  begin
    TraceFmt('Bmp Width %d, Height %d.', [ABmp.Width, ABmp.Height]);

    B := GetBitmapPixelBytesCount(ABmp.PixelFormat);
{$IFDEF ENABLE_FMX}
    if ABmp.Map(TMapAccess.Read, D) then
      for H := 0 to ABmp.Height - 1 do
        TraceMemDump(D.GetScanline(H), ABmp.Width * B);
{$ELSE}
    for H := 0 to ABmp.Height - 1 do
      TraceMemDump(ABmp.ScanLine[H], ABmp.Width * B);
{$ENDIF}
  end;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

procedure TCnDebugger.TraceVirtualKey(AKey: Word);
begin
  TraceVirtualKeyWithTag(AKey, CurrentTag);
end;

procedure TCnDebugger.TraceVirtualKeyWithTag(AKey: Word; const ATag: string);
begin
  TraceFmtWithTag(SCnVirtualKeyFmt, [AKey, AKey, VirtualKeyToString(AKey)], ATag);
end;

procedure TCnDebugger.TraceWindowMessage(AMessage: Cardinal);
begin
  TraceWindowMessageWithTag(AMessage, CurrentTag);
end;

procedure TCnDebugger.TraceWindowMessageWithTag(AMessage: Cardinal; const ATag: string);
begin
  TraceMsgWithTag(WindowMessageToStr(AMessage), ATag);
end;

{$ENDIF}

procedure TCnDebugger.TraceMsg(const AMsg: string);
begin
  TraceFull(AMsg, CurrentTag, CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceMsgWithLevel(const AMsg: string;
  ALevel: Integer);
begin
  TraceFull(AMsg, CurrentTag, ALevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceMsgWithLevelType(const AMsg: string;
  ALevel: Integer; AType: TCnMsgType);
begin
  TraceFull(AMsg, CurrentTag, ALevel, AType);
end;

procedure TCnDebugger.TraceMsgWithTag(const AMsg, ATag: string);
begin
  TraceFull(AMsg, ATag, CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceMsgWithTagLevel(const AMsg, ATag: string;
  ALevel: Integer);
begin
  TraceFull(AMsg, ATag, ALevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceMsgWithType(const AMsg: string;
  AType: TCnMsgType);
begin
  TraceFull(AMsg, CurrentTag, CurrentLevel, AType);
end;

procedure TCnDebugger.TraceMsgWithTypeTag(const AMsg: string;
  AType: TCnMsgType; const ATag: string);
begin
  TraceFull(AMsg, ATag, CurrentLevel, AType);
end;

procedure TCnDebugger.TraceObject(AObject: TObject);
begin
  TraceObjectWithTag(AObject, CurrentTag);
end;

procedure TCnDebugger.TraceObjectWithTag(AObject: TObject;
  const ATag: string);
{$IFNDEF NDEBUG}
var
  List: TStringList;
  Intfs: string;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  if AObject = nil then
  begin
    TraceMsgWithTypeTag('Object: nil', cmtObject, ATag);
    Exit;
  end;

  List := nil;
  try
    List := TStringList.Create;
    try
      AddObjectToStringList(AObject, List, 0);
      Intfs := FormatObjectInterface(AObject);
      if Intfs <> '' then
      begin
        List.Add('Supports Interfaces:');
        List.Add(Intfs);
      end;
    except
      List.Add(SCnObjException);
    end;
    TraceMsgWithTypeTag('Object: ' + IntToHex(TCnNativeInt(AObject), CN_HEX_DIGITS) + SCnCRLF +
      List.Text, cmtObject, ATag);
  finally
    List.Free;
  end;
{$ENDIF}
end;

procedure TCnDebugger.TracePoint(Point: TPoint; const AMsg: string);
begin
  if AMsg = '' then
    TraceMsg(SCnPoint + PointToString(Point))
  else
    TraceFmt('%s %s', [AMsg, PointToString(Point)]);
end;

procedure TCnDebugger.TraceSize(Size: TSize; const AMsg: string);
begin
  if AMsg = '' then
    TraceMsg(SCnSize + SizeToString(Size))
  else
    TraceFmt('%s %s', [AMsg, SizeToString(Size)]);
end;

procedure TCnDebugger.TraceRect(Rect: TRect; const AMsg: string);
begin
  if AMsg = '' then
    TraceMsg(SCnRect + RectToString(Rect))
  else
    TraceFmt('%s %s', [AMsg, RectToString(Rect)]);
end;

procedure TCnDebugger.TraceBits(Bits: TBits; const AMsg: string = '');
begin
  if AMsg = '' then
    TraceMsg(BitsToString(Bits))
  else
    TraceFmt('%s %s', [AMsg, BitsToString(Bits)]);
end;

procedure TCnDebugger.TraceGUID(const GUID: TGUID; const AMsg: string);
begin
  if AMsg = '' then
    LogMsg(SCnGUID + GUIDToString(GUID))
  else
    LogFmt('%s %s', [AMsg, GUIDToString(GUID)]);
end;

procedure TCnDebugger.TraceSeparator;
begin
  TraceFull('-', CurrentTag, CurrentLevel, cmtSeparator);
end;

procedure TCnDebugger.TraceRawString(const Value: string);
begin
  if Value <> '' then
    TraceMemDump(Pointer(Value), Length(Value) * SizeOf(Char));
end;

procedure TCnDebugger.TraceRawAnsiString(const Value: AnsiString);
begin
  if Value <> '' then
    TraceMemDump(Pointer(Value), Length(Value) * SizeOf(AnsiChar));
end;

procedure TCnDebugger.TraceRawWideString(const Value: WideString);
begin
  if Value <> '' then
    TraceMemDump(Pointer(Value), Length(Value) * SizeOf(WideChar));
end;

procedure TCnDebugger.TraceStrings(Strings: TStrings; const AMsg: string);
begin
  if not Assigned(Strings) then
    Exit;

  if AMsg = '' then
    TraceMsg(Strings.Text)
  else
    TraceMsg(AMsg + SCnCRLF + Strings.Text);
end;

procedure TCnDebugger.TraceErrorFmt(const AFormat: string;
  Args: array of const);
begin
  TraceFull(FormatMsg(AFormat, Args), CurrentTag, CurrentLevel, cmtError);
end;

procedure TCnDebugger.TraceMsgError(const AMsg: string);
begin
  TraceFull(AMsg, CurrentTag, CurrentLevel, cmtError);
end;

procedure TCnDebugger.TraceMsgWarning(const AMsg: string);
begin
  TraceFull(AMsg, CurrentTag, CurrentLevel, cmtWarning);
end;

{$IFDEF MSWINDOWS}

procedure TCnDebugger.TraceLastError;
var
  ErrNo: Integer;
  Buf: array[0..255] of Char;
begin
  ErrNo := GetLastError;
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNo, $400, Buf, 255, nil);
  if Buf = '' then StrCopy(PChar(@Buf), PChar(SCnUnknownError));
  TraceErrorFmt(SCnLastErrorFmt, [ErrNo, Buf]);
end;

{$ENDIF}

procedure TCnDebugger.TraceCurrentStack(const AMsg: string);
{$IFDEF CAPTURE_STACK}
var
  Strings: TStrings;
{$ENDIF}
begin
{$IFDEF CAPTURE_STACK}
  Strings := nil;

  try
    Strings := TStringList.Create;
    GetCurrentTrace(Strings);

    TraceMsgWithType('Dump Call Stack: ' + AMsg + SCnCRLF + Strings.Text, cmtInformation);
  finally
    Strings.Free;
  end;
{$ENDIF}
end;

procedure TCnDebugger.TraceConstArray(const Arr: array of const;
  const AMsg: string);
begin
  if AMsg = '' then
    TraceFull(FormatMsg('%s %s', [SCnConstArray, FormatConstArray(Arr)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    TraceFull(FormatMsg('%s %s', [AMsg, FormatConstArray(Arr)]), CurrentTag,
      CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceIntegerArray(const Arr: array of Integer; const AMsg: string);
var
  P: Pointer;
begin
  if Length(Arr) = 0 then
    P := nil
  else
    P := @Arr[0];

  if AMsg = '' then
    TraceFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(P, Length(Arr), SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    TraceFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(P, Length(Arr), SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceIntegerArray(const ArrAddr: Pointer; Count: Integer;
  const AMsg: string);
begin
  if AMsg = '' then
    TraceFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(ArrAddr, Count, SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    TraceFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(ArrAddr, Count, SizeOf(Integer), True)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceCardinalArray(const Arr: array of Cardinal; const AMsg: string);
var
  P: Pointer;
begin
  if Length(Arr) = 0 then
    P := nil
  else
    P := @Arr[0];

  if AMsg = '' then
    TraceFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(P, Length(Arr), SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    TraceFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(P, Length(Arr), SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
end;

procedure TCnDebugger.TraceCardinalArray(const ArrAddr: Pointer; Count: Integer;
  const AMsg: string);
begin
  if AMsg = '' then
    TraceFull(FormatMsg('%s %s', [SCnIntegerArray,
      IntArrayToString(ArrAddr, Count, SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType)
  else
    TraceFull(FormatMsg('%s %s', [AMsg,
      IntArrayToString(ArrAddr, Count, SizeOf(Cardinal), False)]),
      CurrentTag, CurrentLevel, CurrentMsgType);
end;

function TCnDebugger.GetDiscardedMessageCount: Integer;
begin
{$IFNDEF NDEBUG}
  Result := FMessageCount - FPostedMessageCount;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure TCnDebugger.EvaluateObject(AObject: TObject; SyncMode: Boolean = False);
begin
{$IFDEF SUPPORT_EVALUATE}
  EvaluatePointer(AObject, nil, nil, SyncMode);
{$ENDIF}
end;

procedure TCnDebugger.EvaluateObject(APointer: Pointer; SyncMode: Boolean = False);
begin
{$IFDEF SUPPORT_EVALUATE}
  EvaluatePointer(APointer, nil, nil, SyncMode);
{$ENDIF}
end;

{$IFDEF ENABLE_FMX}

procedure TCnDebugger.EvaluateControlUnderPos(const ScreenPos: TPointF);
var
  P: TPoint;
begin
  P.X := Round(ScreenPos.X);
  P.Y := Round(ScreenPos.Y);
  EvaluateControlUnderPos(P);
end;

{$ENDIF}

procedure TCnDebugger.EvaluateControlUnderPos(const ScreenPos: TPoint);
{$IFDEF SUPPORT_EVALUATE}
var
  Control: TWinControl;
{$IFDEF ENABLE_FMX}
  ScreenPoint: TPointF; // 鼠标屏幕坐标
  ClientPoint: TPointF; // 转换后的窗体客户区坐标
  Obj: TFmxObject;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF SUPPORT_EVALUATE}
{$IFDEF ENABLE_FMX}
  ScreenPoint.X := ScreenPos.X;
  ScreenPoint.Y := ScreenPos.Y;
  Obj := FindFmxControlAtPoint(ScreenPoint);
  if Obj <> nil then
    EvaluateObject(Obj);
{$ENDIF}
  Control := FindVCLWindow(ScreenPos);
  if Control <> nil then
    EvaluateObject(Control);
{$ENDIF}
end;

// 移植自 A.Bouchez 的实现
function TCnDebugger.ObjectFromInterface(const AIntf: IUnknown): TObject;
begin
  Result := nil;
  if AIntf = nil then
    Exit;

{$IFDEF SUPPORT_INTERFACE_AS_OBJECT}
  Result := AIntf as TObject;
{$ELSE}
  // 只在 32 位下，可用 Integer 转换
  with PObjectFromInterfaceStub(PPointer(PPointer(AIntf)^)^)^ do
  case Stub of
    $04244483: Result := Pointer(Integer(AIntf) + ShortJmp);
    $04244481: Result := Pointer(Integer(AIntf) + LongJmp);
    else       Result := nil;
  end;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

function TCnDebugger.VirtualKeyToString(AKey: Word): string;
begin
  case AKey of
    VK_LBUTTON:      Result := 'VK_LBUTTON';
    VK_RBUTTON:      Result := 'VK_RBUTTON';
    VK_CANCEL:       Result := 'VK_CANCEL';
    VK_MBUTTON:      Result := 'VK_MBUTTON';
    VK_BACK:         Result := 'VK_BACK';
    VK_TAB:          Result := 'VK_TAB';
    VK_CLEAR:        Result := 'VK_CLEAR';
    VK_RETURN:       Result := 'VK_RETURN';
    VK_SHIFT:        Result := 'VK_SHIFT';
    VK_CONTROL:      Result := 'VK_CONTROL';
    VK_MENU:         Result := 'VK_MENU';
    VK_PAUSE:        Result := 'VK_PAUSE';
    VK_CAPITAL:      Result := 'VK_CAPITAL';
    VK_KANA:         Result := 'VK_KANA/VK_HANGUL';
    VK_JUNJA:        Result := 'VK_JUNJA';
    VK_FINAL:        Result := 'VK_FINAL';
    VK_HANJA:        Result := 'VK_HANJA/VK_KANJI';
    VK_CONVERT:      Result := 'VK_CONVERT';
    VK_NONCONVERT:   Result := 'VK_NONCONVERT';
    VK_ACCEPT:       Result := 'VK_ACCEPT';
    VK_MODECHANGE:   Result := 'VK_MODECHANGE';
    VK_ESCAPE:       Result := 'VK_ESCAPE';
    VK_SPACE:        Result := 'VK_SPACE';
    VK_PRIOR:        Result := 'VK_PRIOR';
    VK_NEXT:         Result := 'VK_NEXT';
    VK_END:          Result := 'VK_END';
    VK_HOME:         Result := 'VK_HOME';
    VK_LEFT:         Result := 'VK_LEFT';
    VK_UP:           Result := 'VK_UP';
    VK_RIGHT:        Result := 'VK_RIGHT';
    VK_DOWN:         Result := 'VK_DOWN';
    VK_SELECT:       Result := 'VK_SELECT';
    VK_PRINT:        Result := 'VK_PRINT';
    VK_EXECUTE:      Result := 'VK_EXECUTE';
    VK_SNAPSHOT:     Result := 'VK_SNAPSHOT';
    VK_INSERT:       Result := 'VK_INSERT';
    VK_DELETE:       Result := 'VK_DELETE';
    VK_HELP:         Result := 'VK_HELP';
    Ord('0'):        Result := 'VK_0';
    Ord('1'):        Result := 'VK_1';
    Ord('2'):        Result := 'VK_2';
    Ord('3'):        Result := 'VK_3';
    Ord('4'):        Result := 'VK_4';
    Ord('5'):        Result := 'VK_5';
    Ord('6'):        Result := 'VK_6';
    Ord('7'):        Result := 'VK_7';
    Ord('8'):        Result := 'VK_8';
    Ord('9'):        Result := 'VK_9';
    Ord('A'):        Result := 'VK_A';
    Ord('B'):        Result := 'VK_B';
    Ord('C'):        Result := 'VK_C';
    Ord('D'):        Result := 'VK_D';
    Ord('E'):        Result := 'VK_E';
    Ord('F'):        Result := 'VK_F';
    Ord('G'):        Result := 'VK_G';
    Ord('H'):        Result := 'VK_H';
    Ord('I'):        Result := 'VK_I';
    Ord('J'):        Result := 'VK_J';
    Ord('K'):        Result := 'VK_K';
    Ord('L'):        Result := 'VK_L';
    Ord('M'):        Result := 'VK_M';
    Ord('N'):        Result := 'VK_N';
    Ord('O'):        Result := 'VK_O';
    Ord('P'):        Result := 'VK_P';
    Ord('Q'):        Result := 'VK_Q';
    Ord('R'):        Result := 'VK_R';
    Ord('S'):        Result := 'VK_S';
    Ord('T'):        Result := 'VK_T';
    Ord('U'):        Result := 'VK_U';
    Ord('V'):        Result := 'VK_V';
    Ord('W'):        Result := 'VK_W';
    Ord('X'):        Result := 'VK_X';
    Ord('Y'):        Result := 'VK_Y';
    Ord('Z'):        Result := 'VK_Z';
    VK_LWIN:         Result := 'VK_LWIN';
    VK_RWIN:         Result := 'VK_RWIN';
    VK_APPS:         Result := 'VK_APPS';
    VK_NUMPAD0:      Result := 'VK_NUMPAD0';
    VK_NUMPAD1:      Result := 'VK_NUMPAD1';
    VK_NUMPAD2:      Result := 'VK_NUMPAD2';
    VK_NUMPAD3:      Result := 'VK_NUMPAD3';
    VK_NUMPAD4:      Result := 'VK_NUMPAD4';
    VK_NUMPAD5:      Result := 'VK_NUMPAD5';
    VK_NUMPAD6:      Result := 'VK_NUMPAD6';
    VK_NUMPAD7:      Result := 'VK_NUMPAD7';
    VK_NUMPAD8:      Result := 'VK_NUMPAD8';
    VK_NUMPAD9:      Result := 'VK_NUMPAD9';
    VK_MULTIPLY:     Result := 'VK_MULTIPLY';
    VK_ADD:          Result := 'VK_ADD';
    VK_SEPARATOR:    Result := 'VK_SEPARATOR';
    VK_SUBTRACT:     Result := 'VK_SUBTRACT';
    VK_DECIMAL:      Result := 'VK_DECIMAL';
    VK_DIVIDE:       Result := 'VK_DIVIDE';
    VK_F1:           Result := 'VK_F1';
    VK_F2:           Result := 'VK_F2';
    VK_F3:           Result := 'VK_F3';
    VK_F4:           Result := 'VK_F4';
    VK_F5:           Result := 'VK_F5';
    VK_F6:           Result := 'VK_F6';
    VK_F7:           Result := 'VK_F7';
    VK_F8:           Result := 'VK_F8';
    VK_F9:           Result := 'VK_F9';
    VK_F10:          Result := 'VK_F10';
    VK_F11:          Result := 'VK_F11';
    VK_F12:          Result := 'VK_F12';
    VK_F13:          Result := 'VK_F13';
    VK_F14:          Result := 'VK_F14';
    VK_F15:          Result := 'VK_F15';
    VK_F16:          Result := 'VK_F16';
    VK_F17:          Result := 'VK_F17';
    VK_F18:          Result := 'VK_F18';
    VK_F19:          Result := 'VK_F19';
    VK_F20:          Result := 'VK_F20';
    VK_F21:          Result := 'VK_F21';
    VK_F22:          Result := 'VK_F22';
    VK_F23:          Result := 'VK_F23';
    VK_F24:          Result := 'VK_F24';
    VK_NUMLOCK:      Result := 'VK_NUMLOCK';
    VK_SCROLL:       Result := 'VK_SCROLL';
    VK_LSHIFT:       Result := 'VK_LSHIFT';
    VK_RSHIFT:       Result := 'VK_RSHIFT';
    VK_LCONTROL:     Result := 'VK_LCONTROL';
    VK_RCONTROL:     Result := 'VK_RCONTROL';
    VK_LMENU:        Result := 'VK_LMENU';
    VK_RMENU:        Result := 'VK_RMENU';

    166:             Result := 'VK_BROWSER_BACK';
    167:             Result := 'VK_BROWSER_FORWARD';
    168:             Result := 'VK_BROWSER_REFRESH';
    169:             Result := 'VK_BROWSER_STOP';
    170:             Result := 'VK_BROWSER_SEARCH';
    171:             Result := 'VK_BROWSER_FAVORITES';
    172:             Result := 'VK_BROWSER_HOME';
    173:             Result := 'VK_VOLUME_MUTE';
    174:             Result := 'VK_VOLUME_DOWN';
    175:             Result := 'VK_VOLUME_UP';
    176:             Result := 'VK_MEDIA_NEXT_TRACK';
    177:             Result := 'VK_MEDIA_PREV_TRACK';
    178:             Result := 'VK_MEDIA_STOP';
    179:             Result := 'VK_MEDIA_PLAY_PAUSE';
    180:             Result := 'VK_LAUNCH_MAIL';
    181:             Result := 'VK_LAUNCH_MEDIA_SELECT';
    182:             Result := 'VK_LAUNCH_APP1';
    183:             Result := 'VK_LAUNCH_APP2';

    186:             Result := 'VK_OEM_1';
    187:             Result := 'VK_OEM_PLUS';
    188:             Result := 'VK_OEM_COMMA';
    189:             Result := 'VK_OEM_MINUS';
    190:             Result := 'VK_OEM_PERIOD';
    191:             Result := 'VK_OEM_2';
    192:             Result := 'VK_OEM_3';
    219:             Result := 'VK_OEM_4';
    220:             Result := 'VK_OEM_5';
    221:             Result := 'VK_OEM_6';
    222:             Result := 'VK_OEM_7';
    223:             Result := 'VK_OEM_8';
    226:             Result := 'VK_OEM_102';
    231:             Result := 'VK_PACKET';

    VK_PROCESSKEY:   Result := 'VK_PROCESSKEY';
    VK_ATTN:         Result := 'VK_ATTN';
    VK_CRSEL:        Result := 'VK_CRSEL';
    VK_EXSEL:        Result := 'VK_EXSEL';
    VK_EREOF:        Result := 'VK_EREOF';
    VK_PLAY:         Result := 'VK_PLAY';
    VK_ZOOM:         Result := 'VK_ZOOM';
    VK_NONAME:       Result := 'VK_NONAME';
    VK_PA1:          Result := 'VK_PA1';
    VK_OEM_CLEAR:    Result := 'VK_OEM_CLEAR';
  else
    Result := 'VK_UNKNOWN';
  end;
end;

function TCnDebugger.WindowMessageToStr(AMessage: Cardinal): string;
begin
  case AMessage of  // Windows Messages
    WM_NULL                 : Result := Format('WM_NULL: %d/$%x', [AMessage, AMessage]);
    WM_CREATE               : Result := Format('WM_CREATE: %d/$%x', [AMessage, AMessage]);
    WM_DESTROY              : Result := Format('WM_DESTROY: %d/$%x', [AMessage, AMessage]);
    WM_MOVE                 : Result := Format('WM_MOVE: %d/$%x', [AMessage, AMessage]);
    WM_SIZE                 : Result := Format('WM_SIZE: %d/$%x', [AMessage, AMessage]);
    WM_ACTIVATE             : Result := Format('WM_ACTIVATE: %d/$%x', [AMessage, AMessage]);
    WM_SETFOCUS             : Result := Format('WM_SETFOCUS: %d/$%x', [AMessage, AMessage]);
    WM_KILLFOCUS            : Result := Format('WM_KILLFOCUS: %d/$%x', [AMessage, AMessage]);
    WM_ENABLE               : Result := Format('WM_ENABLE: %d/$%x', [AMessage, AMessage]);
    WM_SETREDRAW            : Result := Format('WM_SETREDRAW: %d/$%x', [AMessage, AMessage]);
    WM_SETTEXT              : Result := Format('WM_SETTEXT: %d/$%x', [AMessage, AMessage]);
    WM_GETTEXT              : Result := Format('WM_GETTEXT: %d/$%x', [AMessage, AMessage]);
    WM_GETTEXTLENGTH        : Result := Format('WM_GETTEXTLENGTH: %d/$%x', [AMessage, AMessage]);
    WM_PAINT                : Result := Format('WM_PAINT: %d/$%x', [AMessage, AMessage]);
    WM_CLOSE                : Result := Format('WM_CLOSE: %d/$%x', [AMessage, AMessage]);
    WM_QUERYENDSESSION      : Result := Format('WM_QUERYENDSESSION: %d/$%x', [AMessage, AMessage]);
    WM_QUIT                 : Result := Format('WM_QUIT: %d/$%x', [AMessage, AMessage]);
    WM_QUERYOPEN            : Result := Format('WM_QUERYOPEN: %d/$%x', [AMessage, AMessage]);
    WM_ERASEBKGND           : Result := Format('WM_ERASEBKGND: %d/$%x', [AMessage, AMessage]);
    WM_SYSCOLORCHANGE       : Result := Format('WM_SYSCOLORCHANGE: %d/$%x', [AMessage, AMessage]);
    WM_ENDSESSION           : Result := Format('WM_ENDSESSION: %d/$%x', [AMessage, AMessage]);
    WM_SYSTEMERROR          : Result := Format('WM_SYSTEMERROR: %d/$%x', [AMessage, AMessage]);
    WM_SHOWWINDOW           : Result := Format('WM_SHOWWINDOW: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLOR             : Result := Format('WM_CTLCOLOR: %d/$%x', [AMessage, AMessage]);
    WM_WININICHANGE         : Result := Format('WM_WININICHANGE/WM_SETTINGCHANGE: %d/$%x', [AMessage, AMessage]);
    WM_DEVMODECHANGE        : Result := Format('WM_DEVMODECHANGE: %d/$%x', [AMessage, AMessage]);
    WM_ACTIVATEAPP          : Result := Format('WM_ACTIVATEAPP: %d/$%x', [AMessage, AMessage]);
    WM_FONTCHANGE           : Result := Format('WM_FONTCHANGE: %d/$%x', [AMessage, AMessage]);
    WM_TIMECHANGE           : Result := Format('WM_TIMECHANGE: %d/$%x', [AMessage, AMessage]);
    WM_CANCELMODE           : Result := Format('WM_CANCELMODE: %d/$%x', [AMessage, AMessage]);
    WM_SETCURSOR            : Result := Format('WM_SETCURSOR: %d/$%x', [AMessage, AMessage]);
    WM_MOUSEACTIVATE        : Result := Format('WM_MOUSEACTIVATE: %d/$%x', [AMessage, AMessage]);
    WM_CHILDACTIVATE        : Result := Format('WM_CHILDACTIVATE: %d/$%x', [AMessage, AMessage]);
    WM_QUEUESYNC            : Result := Format('WM_QUEUESYNC: %d/$%x', [AMessage, AMessage]);
    WM_GETMINMAXINFO        : Result := Format('WM_GETMINMAXINFO: %d/$%x', [AMessage, AMessage]);
    WM_PAINTICON            : Result := Format('WM_PAINTICON: %d/$%x', [AMessage, AMessage]);
    WM_ICONERASEBKGND       : Result := Format('WM_ICONERASEBKGND: %d/$%x', [AMessage, AMessage]);
    WM_NEXTDLGCTL           : Result := Format('WM_NEXTDLGCTL: %d/$%x', [AMessage, AMessage]);
    WM_SPOOLERSTATUS        : Result := Format('WM_SPOOLERSTATUS: %d/$%x', [AMessage, AMessage]);
    WM_DRAWITEM             : Result := Format('WM_DRAWITEM: %d/$%x', [AMessage, AMessage]);
    WM_MEASUREITEM          : Result := Format('WM_MEASUREITEM: %d/$%x', [AMessage, AMessage]);
    WM_DELETEITEM           : Result := Format('WM_DELETEITEM: %d/$%x', [AMessage, AMessage]);
    WM_VKEYTOITEM           : Result := Format('WM_VKEYTOITEM: %d/$%x', [AMessage, AMessage]);
    WM_CHARTOITEM           : Result := Format('WM_CHARTOITEM: %d/$%x', [AMessage, AMessage]);
    WM_SETFONT              : Result := Format('WM_SETFONT: %d/$%x', [AMessage, AMessage]);
    WM_GETFONT              : Result := Format('WM_GETFONT: %d/$%x', [AMessage, AMessage]);
    WM_SETHOTKEY            : Result := Format('WM_SETHOTKEY: %d/$%x', [AMessage, AMessage]);
    WM_GETHOTKEY            : Result := Format('WM_GETHOTKEY: %d/$%x', [AMessage, AMessage]);
    WM_QUERYDRAGICON        : Result := Format('WM_QUERYDRAGICON: %d/$%x', [AMessage, AMessage]);
    WM_COMPAREITEM          : Result := Format('WM_COMPAREITEM: %d/$%x', [AMessage, AMessage]);
    WM_GETOBJECT            : Result := Format('WM_GETOBJECT: %d/$%x', [AMessage, AMessage]);
    WM_COMPACTING           : Result := Format('WM_COMPACTING: %d/$%x', [AMessage, AMessage]);
    WM_COMMNOTIFY           : Result := Format('WM_COMMNOTIFY: %d/$%x', [AMessage, AMessage]);
    WM_WINDOWPOSCHANGING    : Result := Format('WM_WINDOWPOSCHANGING: %d/$%x', [AMessage, AMessage]);
    WM_WINDOWPOSCHANGED     : Result := Format('WM_WINDOWPOSCHANGED: %d/$%x', [AMessage, AMessage]);
    WM_POWER                : Result := Format('WM_POWER: %d/$%x', [AMessage, AMessage]);
    WM_COPYDATA             : Result := Format('WM_COPYDATA: %d/$%x', [AMessage, AMessage]);
    WM_CANCELJOURNAL        : Result := Format('WM_CANCELJOURNAL: %d/$%x', [AMessage, AMessage]);
    WM_NOTIFY               : Result := Format('WM_NOTIFY: %d/$%x', [AMessage, AMessage]);
    WM_INPUTLANGCHANGEREQUEST: Result := Format('WM_INPUTLANGCHANGEREQUEST: %d/$%x', [AMessage, AMessage]);
    WM_INPUTLANGCHANGE      : Result := Format('WM_INPUTLANGCHANGE: %d/$%x', [AMessage, AMessage]);
    WM_TCARD                : Result := Format('WM_TCARD: %d/$%x', [AMessage, AMessage]);
    WM_HELP                 : Result := Format('WM_HELP: %d/$%x', [AMessage, AMessage]);
    WM_USERCHANGED          : Result := Format('WM_USERCHANGED: %d/$%x', [AMessage, AMessage]);
    WM_NOTIFYFORMAT         : Result := Format('WM_NOTIFYFORMAT: %d/$%x', [AMessage, AMessage]);
    WM_CONTEXTMENU          : Result := Format('WM_CONTEXTMENU: %d/$%x', [AMessage, AMessage]);
    WM_STYLECHANGING        : Result := Format('WM_STYLECHANGING: %d/$%x', [AMessage, AMessage]);
    WM_STYLECHANGED         : Result := Format('WM_STYLECHANGED: %d/$%x', [AMessage, AMessage]);
    WM_DISPLAYCHANGE        : Result := Format('WM_DISPLAYCHANGE: %d/$%x', [AMessage, AMessage]);
    WM_GETICON              : Result := Format('WM_GETICON: %d/$%x', [AMessage, AMessage]);
    WM_SETICON              : Result := Format('WM_SETICON: %d/$%x', [AMessage, AMessage]);
    WM_NCCREATE             : Result := Format('WM_NCCREATE: %d/$%x', [AMessage, AMessage]);
    WM_NCDESTROY            : Result := Format('WM_NCDESTROY: %d/$%x', [AMessage, AMessage]);
    WM_NCCALCSIZE           : Result := Format('WM_NCCALCSIZE: %d/$%x', [AMessage, AMessage]);
    WM_NCHITTEST            : Result := Format('WM_NCHITTEST: %d/$%x', [AMessage, AMessage]);
    WM_NCPAINT              : Result := Format('WM_NCPAINT: %d/$%x', [AMessage, AMessage]);
    WM_NCACTIVATE           : Result := Format('WM_NCACTIVATE: %d/$%x', [AMessage, AMessage]);
    WM_GETDLGCODE           : Result := Format('WM_GETDLGCODE: %d/$%x', [AMessage, AMessage]);
    WM_NCMOUSEMOVE          : Result := Format('WM_NCMOUSEMOVE: %d/$%x', [AMessage, AMessage]);
    WM_NCLBUTTONDOWN        : Result := Format('WM_NCLBUTTONDOWN: %d/$%x', [AMessage, AMessage]);
    WM_NCLBUTTONUP          : Result := Format('WM_NCLBUTTONUP: %d/$%x', [AMessage, AMessage]);
    WM_NCLBUTTONDBLCLK      : Result := Format('WM_NCLBUTTONDBLCLK: %d/$%x', [AMessage, AMessage]);
    WM_NCRBUTTONDOWN        : Result := Format('WM_NCRBUTTONDOWN: %d/$%x', [AMessage, AMessage]);
    WM_NCRBUTTONUP          : Result := Format('WM_NCRBUTTONUP: %d/$%x', [AMessage, AMessage]);
    WM_NCRBUTTONDBLCLK      : Result := Format('WM_NCRBUTTONDBLCLK: %d/$%x', [AMessage, AMessage]);
    WM_NCMBUTTONDOWN        : Result := Format('WM_NCMBUTTONDOWN: %d/$%x', [AMessage, AMessage]);
    WM_NCMBUTTONUP          : Result := Format('WM_NCMBUTTONUP: %d/$%x', [AMessage, AMessage]);
    WM_NCMBUTTONDBLCLK      : Result := Format('WM_NCMBUTTONDBLCLK: %d/$%x', [AMessage, AMessage]);
    WM_KEYDOWN              : Result := Format('WM_KEYDOWN: %d/$%x', [AMessage, AMessage]);
    WM_KEYUP                : Result := Format('WM_KEYUP: %d/$%x', [AMessage, AMessage]);
    WM_CHAR                 : Result := Format('WM_CHAR: %d/$%x', [AMessage, AMessage]);
    WM_DEADCHAR             : Result := Format('WM_DEADCHAR: %d/$%x', [AMessage, AMessage]);
    WM_SYSKEYDOWN           : Result := Format('WM_SYSKEYDOWN: %d/$%x', [AMessage, AMessage]);
    WM_SYSKEYUP             : Result := Format('WM_SYSKEYUP: %d/$%x', [AMessage, AMessage]);
    WM_SYSCHAR              : Result := Format('WM_SYSCHAR: %d/$%x', [AMessage, AMessage]);
    WM_SYSDEADCHAR          : Result := Format('WM_SYSDEADCHAR: %d/$%x', [AMessage, AMessage]);
    WM_KEYLAST              : Result := Format('WM_KEYLAST: %d/$%x', [AMessage, AMessage]);
    WM_INITDIALOG           : Result := Format('WM_INITDIALOG: %d/$%x', [AMessage, AMessage]);
    WM_COMMAND              : Result := Format('WM_COMMAND: %d/$%x', [AMessage, AMessage]);
    WM_SYSCOMMAND           : Result := Format('WM_SYSCOMMAND: %d/$%x', [AMessage, AMessage]);
    WM_TIMER                : Result := Format('WM_TIMER: %d/$%x', [AMessage, AMessage]);
    WM_HSCROLL              : Result := Format('WM_HSCROLL: %d/$%x', [AMessage, AMessage]);
    WM_VSCROLL              : Result := Format('WM_VSCROLL: %d/$%x', [AMessage, AMessage]);
    WM_INITMENU             : Result := Format('WM_INITMENU: %d/$%x', [AMessage, AMessage]);
    WM_INITMENUPOPUP        : Result := Format('WM_INITMENUPOPUP: %d/$%x', [AMessage, AMessage]);
    WM_MENUSELECT           : Result := Format('WM_MENUSELECT: %d/$%x', [AMessage, AMessage]);
    WM_MENUCHAR             : Result := Format('WM_MENUCHAR: %d/$%x', [AMessage, AMessage]);
    WM_ENTERIDLE            : Result := Format('WM_ENTERIDLE: %d/$%x', [AMessage, AMessage]);
    WM_MENURBUTTONUP        : Result := Format('WM_MENURBUTTONUP: %d/$%x', [AMessage, AMessage]);
    WM_MENUDRAG             : Result := Format('WM_MENUDRAG: %d/$%x', [AMessage, AMessage]);
    WM_MENUGETOBJECT        : Result := Format('WM_MENUGETOBJECT: %d/$%x', [AMessage, AMessage]);
    WM_UNINITMENUPOPUP      : Result := Format('WM_UNINITMENUPOPUP: %d/$%x', [AMessage, AMessage]);
    WM_MENUCOMMAND          : Result := Format('WM_MENUCOMMAND: %d/$%x', [AMessage, AMessage]);
    WM_CHANGEUISTATE        : Result := Format('WM_CHANGEUISTATE: %d/$%x', [AMessage, AMessage]);
    WM_UPDATEUISTATE        : Result := Format('WM_UPDATEUISTATE: %d/$%x', [AMessage, AMessage]);
    WM_QUERYUISTATE         : Result := Format('WM_QUERYUISTATE: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLORMSGBOX       : Result := Format('WM_CTLCOLORMSGBOX: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLOREDIT         : Result := Format('WM_CTLCOLOREDIT: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLORLISTBOX      : Result := Format('WM_CTLCOLORLISTBOX: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLORBTN          : Result := Format('WM_CTLCOLORBTN: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLORDLG          : Result := Format('WM_CTLCOLORDLG: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLORSCROLLBAR    : Result := Format('WM_CTLCOLORSCROLLBAR: %d/$%x', [AMessage, AMessage]);
    WM_CTLCOLORSTATIC       : Result := Format('WM_CTLCOLORSTATIC: %d/$%x', [AMessage, AMessage]);
    WM_MOUSEMOVE            : Result := Format('WM_MOUSEMOVE: %d/$%x', [AMessage, AMessage]);
    WM_LBUTTONDOWN          : Result := Format('WM_LBUTTONDOWN: %d/$%x', [AMessage, AMessage]);
    WM_LBUTTONUP            : Result := Format('WM_LBUTTONUP: %d/$%x', [AMessage, AMessage]);
    WM_LBUTTONDBLCLK        : Result := Format('WM_LBUTTONDBLCLK: %d/$%x', [AMessage, AMessage]);
    WM_RBUTTONDOWN          : Result := Format('WM_RBUTTONDOWN: %d/$%x', [AMessage, AMessage]);
    WM_RBUTTONUP            : Result := Format('WM_RBUTTONUP: %d/$%x', [AMessage, AMessage]);
    WM_RBUTTONDBLCLK        : Result := Format('WM_RBUTTONDBLCLK: %d/$%x', [AMessage, AMessage]);
    WM_MBUTTONDOWN          : Result := Format('WM_MBUTTONDOWN: %d/$%x', [AMessage, AMessage]);
    WM_MBUTTONUP            : Result := Format('WM_MBUTTONUP: %d/$%x', [AMessage, AMessage]);
    WM_MBUTTONDBLCLK        : Result := Format('WM_MBUTTONDBLCLK: %d/$%x', [AMessage, AMessage]);
    WM_MOUSEWHEEL           : Result := Format('WM_MOUSEWHEEL: %d/$%x', [AMessage, AMessage]);
    WM_PARENTNOTIFY         : Result := Format('WM_PARENTNOTIFY: %d/$%x', [AMessage, AMessage]);
    WM_ENTERMENULOOP        : Result := Format('WM_ENTERMENULOOP: %d/$%x', [AMessage, AMessage]);
    WM_EXITMENULOOP         : Result := Format('WM_EXITMENULOOP: %d/$%x', [AMessage, AMessage]);
    WM_NEXTMENU             : Result := Format('WM_NEXTMENU: %d/$%x', [AMessage, AMessage]);
    WM_SIZING               : Result := Format('WM_SIZING: %d/$%x', [AMessage, AMessage]);
    WM_CAPTURECHANGED       : Result := Format('WM_CAPTURECHANGED: %d/$%x', [AMessage, AMessage]);
    WM_MOVING               : Result := Format('WM_MOVING: %d/$%x', [AMessage, AMessage]);
    WM_POWERBROADCAST       : Result := Format('WM_POWERBROADCAST: %d/$%x', [AMessage, AMessage]);
    WM_DEVICECHANGE         : Result := Format('WM_DEVICECHANGE: %d/$%x', [AMessage, AMessage]);
    WM_IME_STARTCOMPOSITION : Result := Format('WM_IME_STARTCOMPOSITION: %d/$%x', [AMessage, AMessage]);
    WM_IME_ENDCOMPOSITION   : Result := Format('WM_IME_ENDCOMPOSITION: %d/$%x', [AMessage, AMessage]);
    WM_IME_COMPOSITION      : Result := Format('WM_IME_COMPOSITION: %d/$%x', [AMessage, AMessage]);
    WM_IME_SETCONTEXT       : Result := Format('WM_IME_SETCONTEXT: %d/$%x', [AMessage, AMessage]);
    WM_IME_NOTIFY           : Result := Format('WM_IME_NOTIFY: %d/$%x', [AMessage, AMessage]);
    WM_IME_CONTROL          : Result := Format('WM_IME_CONTROL: %d/$%x', [AMessage, AMessage]);
    WM_IME_COMPOSITIONFULL  : Result := Format('WM_IME_COMPOSITIONFULL: %d/$%x', [AMessage, AMessage]);
    WM_IME_SELECT           : Result := Format('WM_IME_SELECT: %d/$%x', [AMessage, AMessage]);
    WM_IME_CHAR             : Result := Format('WM_IME_CHAR: %d/$%x', [AMessage, AMessage]);
    WM_IME_REQUEST          : Result := Format('WM_IME_REQUEST: %d/$%x', [AMessage, AMessage]);
    WM_IME_KEYDOWN          : Result := Format('WM_IME_KEYDOWN: %d/$%x', [AMessage, AMessage]);
    WM_IME_KEYUP            : Result := Format('WM_IME_KEYUP: %d/$%x', [AMessage, AMessage]);
    WM_MDICREATE            : Result := Format('WM_MDICREATE: %d/$%x', [AMessage, AMessage]);
    WM_MDIDESTROY           : Result := Format('WM_MDIDESTROY: %d/$%x', [AMessage, AMessage]);
    WM_MDIACTIVATE          : Result := Format('WM_MDIACTIVATE: %d/$%x', [AMessage, AMessage]);
    WM_MDIRESTORE           : Result := Format('WM_MDIRESTORE: %d/$%x', [AMessage, AMessage]);
    WM_MDINEXT              : Result := Format('WM_MDINEXT: %d/$%x', [AMessage, AMessage]);
    WM_MDIMAXIMIZE          : Result := Format('WM_MDIMAXIMIZE: %d/$%x', [AMessage, AMessage]);
    WM_MDITILE              : Result := Format('WM_MDITILE: %d/$%x', [AMessage, AMessage]);
    WM_MDICASCADE           : Result := Format('WM_MDICASCADE: %d/$%x', [AMessage, AMessage]);
    WM_MDIICONARRANGE       : Result := Format('WM_MDIICONARRANGE: %d/$%x', [AMessage, AMessage]);
    WM_MDIGETACTIVE         : Result := Format('WM_MDIGETACTIVE: %d/$%x', [AMessage, AMessage]);
    WM_MDISETMENU           : Result := Format('WM_MDISETMENU: %d/$%x', [AMessage, AMessage]);
    WM_ENTERSIZEMOVE        : Result := Format('WM_ENTERSIZEMOVE: %d/$%x', [AMessage, AMessage]);
    WM_EXITSIZEMOVE         : Result := Format('WM_EXITSIZEMOVE: %d/$%x', [AMessage, AMessage]);
    WM_DROPFILES            : Result := Format('WM_DROPFILES: %d/$%x', [AMessage, AMessage]);
    WM_MDIREFRESHMENU       : Result := Format('WM_MDIREFRESHMENU: %d/$%x', [AMessage, AMessage]);

    $0238                   : Result := Format('WM_POINTERDEVICECHANGE: %d/$%x', [AMessage, AMessage]);
    $0239                   : Result := Format('WM_POINTERDEVICEINRANGE: %d/$%x', [AMessage, AMessage]);
    $023A                   : Result := Format('WM_POINTERDEVICEOUTOFRANGE: %d/$%x', [AMessage, AMessage]);
    $0240                   : Result := Format('WM_TOUCH: %d/$%x', [AMessage, AMessage]);
    $0241                   : Result := Format('WM_NCPOINTERUPDATE: %d/$%x', [AMessage, AMessage]);
    $0242                   : Result := Format('WM_NCPOINTERDOWN: %d/$%x', [AMessage, AMessage]);
    $0243                   : Result := Format('WM_NCPOINTERUP: %d/$%x', [AMessage, AMessage]);
    $0245                   : Result := Format('WM_POINTERUPDATE: %d/$%x', [AMessage, AMessage]);
    $0246                   : Result := Format('WM_POINTERDOWN: %d/$%x', [AMessage, AMessage]);
    $0247                   : Result := Format('WM_POINTERUP: %d/$%x', [AMessage, AMessage]);
    $0249                   : Result := Format('WM_POINTERENTER: %d/$%x', [AMessage, AMessage]);
    $024A                   : Result := Format('WM_POINTERLEAVE: %d/$%x', [AMessage, AMessage]);
    $024B                   : Result := Format('WM_POINTERACTIVATE: %d/$%x', [AMessage, AMessage]);
    $024C                   : Result := Format('WM_POINTERCAPTURECHANGED: %d/$%x', [AMessage, AMessage]);
    $024D                   : Result := Format('WM_TOUCHHITTESTING: %d/$%x', [AMessage, AMessage]);
    $024E                   : Result := Format('WM_POINTERWHEEL: %d/$%x', [AMessage, AMessage]);
    $024F                   : Result := Format('WM_POINTERHWHEEL: %d/$%x', [AMessage, AMessage]);
    $0250                   : Result := Format('DM_POINTERHITTEST: %d/$%x', [AMessage, AMessage]);
    $0251                   : Result := Format('WM_POINTERROUTEDTO: %d/$%x', [AMessage, AMessage]);
    $0252                   : Result := Format('WM_POINTERROUTEDAWAY: %d/$%x', [AMessage, AMessage]);
    $0253                   : Result := Format('WM_POINTERROUTEDRELEASED: %d/$%x', [AMessage, AMessage]);

    WM_MOUSEHOVER           : Result := Format('WM_MOUSEHOVER: %d/$%x', [AMessage, AMessage]);
    WM_MOUSELEAVE           : Result := Format('WM_MOUSELEAVE: %d/$%x', [AMessage, AMessage]);

    $02A0                   : Result := Format('WM_NCMOUSEHOVER: %d/$%x', [AMessage, AMessage]);
    $02A2                   : Result := Format('WM_NCMOUSELEAVE: %d/$%x', [AMessage, AMessage]);
    $02B1                   : Result := Format('WM_WTSSESSION_CHANGE: %d/$%x', [AMessage, AMessage]);
    $02C0                   : Result := Format('WM_TABLET_FIRST: %d/$%x', [AMessage, AMessage]);
    $02DF                   : Result := Format('WM_TABLET_LAST: %d/$%x', [AMessage, AMessage]);
    $02E0                   : Result := Format('WM_DPICHANGED: %d/$%x', [AMessage, AMessage]);
    $02E2                   : Result := Format('WM_DPICHANGED_BEFOREPARENT: %d/$%x', [AMessage, AMessage]);
    $02E3                   : Result := Format('WM_DPICHANGED_AFTERPARENT: %d/$%x', [AMessage, AMessage]);
    $02E4                   : Result := Format('WM_GETDPISCALEDSIZE: %d/$%x', [AMessage, AMessage]);

    WM_CUT                  : Result := Format('WM_CUT: %d/$%x', [AMessage, AMessage]);
    WM_COPY                 : Result := Format('WM_COPY: %d/$%x', [AMessage, AMessage]);
    WM_PASTE                : Result := Format('WM_PASTE: %d/$%x', [AMessage, AMessage]);
    WM_CLEAR                : Result := Format('WM_CLEAR: %d/$%x', [AMessage, AMessage]);
    WM_UNDO                 : Result := Format('WM_UNDO: %d/$%x', [AMessage, AMessage]);
    WM_RENDERFORMAT         : Result := Format('WM_RENDERFORMAT: %d/$%x', [AMessage, AMessage]);
    WM_RENDERALLFORMATS     : Result := Format('WM_RENDERALLFORMATS: %d/$%x', [AMessage, AMessage]);
    WM_DESTROYCLIPBOARD     : Result := Format('WM_DESTROYCLIPBOARD: %d/$%x', [AMessage, AMessage]);
    WM_DRAWCLIPBOARD        : Result := Format('WM_DRAWCLIPBOARD: %d/$%x', [AMessage, AMessage]);
    WM_PAINTCLIPBOARD       : Result := Format('WM_PAINTCLIPBOARD: %d/$%x', [AMessage, AMessage]);
    WM_VSCROLLCLIPBOARD     : Result := Format('WM_VSCROLLCLIPBOARD: %d/$%x', [AMessage, AMessage]);
    WM_SIZECLIPBOARD        : Result := Format('WM_SIZECLIPBOARD: %d/$%x', [AMessage, AMessage]);
    WM_ASKCBFORMATNAME      : Result := Format('WM_ASKCBFORMATNAME: %d/$%x', [AMessage, AMessage]);
    WM_CHANGECBCHAIN        : Result := Format('WM_CHANGECBCHAIN: %d/$%x', [AMessage, AMessage]);
    WM_HSCROLLCLIPBOARD     : Result := Format('WM_HSCROLLCLIPBOARD: %d/$%x', [AMessage, AMessage]);
    WM_QUERYNEWPALETTE      : Result := Format('WM_QUERYNEWPALETTE: %d/$%x', [AMessage, AMessage]);
    WM_PALETTEISCHANGING    : Result := Format('WM_PALETTEISCHANGING: %d/$%x', [AMessage, AMessage]);
    WM_PALETTECHANGED       : Result := Format('WM_PALETTECHANGED: %d/$%x', [AMessage, AMessage]);
    WM_HOTKEY               : Result := Format('WM_HOTKEY: %d/$%x', [AMessage, AMessage]);
    WM_PRINT                : Result := Format('WM_PRINT: %d/$%x', [AMessage, AMessage]);
    WM_PRINTCLIENT          : Result := Format('WM_PRINTCLIENT: %d/$%x', [AMessage, AMessage]);
    WM_HANDHELDFIRST        : Result := Format('WM_HANDHELDFIRST: %d/$%x', [AMessage, AMessage]);
    WM_HANDHELDLAST         : Result := Format('WM_HANDHELDLAST: %d/$%x', [AMessage, AMessage]);
    WM_PENWINFIRST          : Result := Format('WM_PENWINFIRST: %d/$%x', [AMessage, AMessage]);
    WM_PENWINLAST           : Result := Format('WM_PENWINLAST: %d/$%x', [AMessage, AMessage]);
    WM_COALESCE_FIRST       : Result := Format('WM_COALESCE_FIRST: %d/$%x', [AMessage, AMessage]);
    WM_COALESCE_LAST        : Result := Format('WM_COALESCE_LAST: %d/$%x', [AMessage, AMessage]);
    WM_DDE_INITIATE         : Result := Format('WM_DDE_INITIATE: %d/$%x', [AMessage, AMessage]);
    WM_DDE_TERMINATE        : Result := Format('WM_DDE_TERMINATE: %d/$%x', [AMessage, AMessage]);
    WM_DDE_ADVISE           : Result := Format('WM_DDE_ADVISE: %d/$%x', [AMessage, AMessage]);
    WM_DDE_UNADVISE         : Result := Format('WM_DDE_UNADVISE: %d/$%x', [AMessage, AMessage]);
    WM_DDE_ACK              : Result := Format('WM_DDE_ACK: %d/$%x', [AMessage, AMessage]);
    WM_DDE_DATA             : Result := Format('WM_DDE_DATA: %d/$%x', [AMessage, AMessage]);
    WM_DDE_REQUEST          : Result := Format('WM_DDE_REQUEST: %d/$%x', [AMessage, AMessage]);
    WM_DDE_POKE             : Result := Format('WM_DDE_POKE: %d/$%x', [AMessage, AMessage]);
    WM_DDE_EXECUTE          : Result := Format('WM_DDE_EXECUTE: %d/$%x', [AMessage, AMessage]);
    WM_APP                  : Result := Format('WM_APP: %d/$%x', [AMessage, AMessage]);
    WM_USER                 : Result := Format('WM_USER: %d/$%x', [AMessage, AMessage]);
    // VCL Control Messages
    // CM_BASE                 : Result := Format('CM_BASE: %d/$%x', [AMessage, AMessage]);
    CM_ACTIVATE             : Result := Format('CM_ACTIVATE: %d/$%x', [AMessage, AMessage]);
    CM_DEACTIVATE           : Result := Format('CM_DEACTIVATE: %d/$%x', [AMessage, AMessage]);
    CM_GOTFOCUS             : Result := Format('CM_GOTFOCUS: %d/$%x', [AMessage, AMessage]);
    CM_LOSTFOCUS            : Result := Format('CM_LOSTFOCUS: %d/$%x', [AMessage, AMessage]);
    CM_CANCELMODE           : Result := Format('CM_CANCELMODE: %d/$%x', [AMessage, AMessage]);
    CM_DIALOGKEY            : Result := Format('CM_DIALOGKEY: %d/$%x', [AMessage, AMessage]);
    CM_DIALOGCHAR           : Result := Format('CM_DIALOGCHAR: %d/$%x', [AMessage, AMessage]);
    CM_FOCUSCHANGED         : Result := Format('CM_FOCUSCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_PARENTFONTCHANGED    : Result := Format('CM_PARENTFONTCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_PARENTCOLORCHANGED   : Result := Format('CM_PARENTCOLORCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_HITTEST              : Result := Format('CM_HITTEST: %d/$%x', [AMessage, AMessage]);
    CM_VISIBLECHANGED       : Result := Format('CM_VISIBLECHANGED: %d/$%x', [AMessage, AMessage]);
    CM_ENABLEDCHANGED       : Result := Format('CM_ENABLEDCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_COLORCHANGED         : Result := Format('CM_COLORCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_FONTCHANGED          : Result := Format('CM_FONTCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_CURSORCHANGED        : Result := Format('CM_CURSORCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_CTL3DCHANGED         : Result := Format('CM_CTL3DCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_PARENTCTL3DCHANGED   : Result := Format('CM_PARENTCTL3DCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_TEXTCHANGED          : Result := Format('CM_TEXTCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_MOUSEENTER           : Result := Format('CM_MOUSEENTER: %d/$%x', [AMessage, AMessage]);
    CM_MOUSELEAVE           : Result := Format('CM_MOUSELEAVE: %d/$%x', [AMessage, AMessage]);
    CM_MENUCHANGED          : Result := Format('CM_MENUCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_APPKEYDOWN           : Result := Format('CM_APPKEYDOWN: %d/$%x', [AMessage, AMessage]);
    CM_APPSYSCOMMAND        : Result := Format('CM_APPSYSCOMMAND: %d/$%x', [AMessage, AMessage]);
    CM_BUTTONPRESSED        : Result := Format('CM_BUTTONPRESSED: %d/$%x', [AMessage, AMessage]);
    CM_SHOWINGCHANGED       : Result := Format('CM_SHOWINGCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_ENTER                : Result := Format('CM_ENTER: %d/$%x', [AMessage, AMessage]);
    CM_EXIT                 : Result := Format('CM_EXIT: %d/$%x', [AMessage, AMessage]);
    CM_DESIGNHITTEST        : Result := Format('CM_DESIGNHITTEST: %d/$%x', [AMessage, AMessage]);
    CM_ICONCHANGED          : Result := Format('CM_ICONCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_WANTSPECIALKEY       : Result := Format('CM_WANTSPECIALKEY: %d/$%x', [AMessage, AMessage]);
    CM_INVOKEHELP           : Result := Format('CM_INVOKEHELP: %d/$%x', [AMessage, AMessage]);
    CM_WINDOWHOOK           : Result := Format('CM_WINDOWHOOK: %d/$%x', [AMessage, AMessage]);
    CM_RELEASE              : Result := Format('CM_RELEASE: %d/$%x', [AMessage, AMessage]);
    CM_SHOWHINTCHANGED      : Result := Format('CM_SHOWHINTCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_PARENTSHOWHINTCHANGED: Result := Format('CM_PARENTSHOWHINTCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_SYSCOLORCHANGE       : Result := Format('CM_SYSCOLORCHANGE: %d/$%x', [AMessage, AMessage]);
    CM_WININICHANGE         : Result := Format('CM_WININICHANGE: %d/$%x', [AMessage, AMessage]);
    CM_FONTCHANGE           : Result := Format('CM_FONTCHANGE: %d/$%x', [AMessage, AMessage]);
    CM_TIMECHANGE           : Result := Format('CM_TIMECHANGE: %d/$%x', [AMessage, AMessage]);
    CM_TABSTOPCHANGED       : Result := Format('CM_TABSTOPCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_UIACTIVATE           : Result := Format('CM_UIACTIVATE: %d/$%x', [AMessage, AMessage]);
    CM_UIDEACTIVATE         : Result := Format('CM_UIDEACTIVATE: %d/$%x', [AMessage, AMessage]);
    CM_DOCWINDOWACTIVATE    : Result := Format('CM_DOCWINDOWACTIVATE: %d/$%x', [AMessage, AMessage]);
    CM_CONTROLLISTCHANGE    : Result := Format('CM_CONTROLLISTCHANGE: %d/$%x', [AMessage, AMessage]);
    CM_GETDATALINK          : Result := Format('CM_GETDATALINK: %d/$%x', [AMessage, AMessage]);
    CM_CHILDKEY             : Result := Format('CM_CHILDKEY: %d/$%x', [AMessage, AMessage]);
    CM_DRAG                 : Result := Format('CM_DRAG: %d/$%x', [AMessage, AMessage]);
    CM_HINTSHOW             : Result := Format('CM_HINTSHOW: %d/$%x', [AMessage, AMessage]);
    CM_DIALOGHANDLE         : Result := Format('CM_DIALOGHANDLE: %d/$%x', [AMessage, AMessage]);
    CM_ISTOOLCONTROL        : Result := Format('CM_ISTOOLCONTROL: %d/$%x', [AMessage, AMessage]);
    CM_RECREATEWND          : Result := Format('CM_RECREATEWND: %d/$%x', [AMessage, AMessage]);
    CM_INVALIDATE           : Result := Format('CM_INVALIDATE: %d/$%x', [AMessage, AMessage]);
    CM_SYSFONTCHANGED       : Result := Format('CM_SYSFONTCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_CONTROLCHANGE        : Result := Format('CM_CONTROLCHANGE: %d/$%x', [AMessage, AMessage]);
    CM_CHANGED              : Result := Format('CM_CHANGED: %d/$%x', [AMessage, AMessage]);
    CM_DOCKCLIENT           : Result := Format('CM_DOCKCLIENT: %d/$%x', [AMessage, AMessage]);
    CM_UNDOCKCLIENT         : Result := Format('CM_UNDOCKCLIENT: %d/$%x', [AMessage, AMessage]);
    CM_FLOAT                : Result := Format('CM_FLOAT: %d/$%x', [AMessage, AMessage]);
    CM_BORDERCHANGED        : Result := Format('CM_BORDERCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BIDIMODECHANGED      : Result := Format('CM_BIDIMODECHANGED: %d/$%x', [AMessage, AMessage]);
    CM_PARENTBIDIMODECHANGED: Result := Format('CM_PARENTBIDIMODECHANGED: %d/$%x', [AMessage, AMessage]);
    CM_ALLCHILDRENFLIPPED   : Result := Format('CM_ALLCHILDRENFLIPPED: %d/$%x', [AMessage, AMessage]);
    CM_ACTIONUPDATE         : Result := Format('CM_ACTIONUPDATE: %d/$%x', [AMessage, AMessage]);
    CM_ACTIONEXECUTE        : Result := Format('CM_ACTIONEXECUTE: %d/$%x', [AMessage, AMessage]);
    CM_HINTSHOWPAUSE        : Result := Format('CM_HINTSHOWPAUSE: %d/$%x', [AMessage, AMessage]);
    CM_DOCKNOTIFICATION     : Result := Format('CM_DOCKNOTIFICATION: %d/$%x', [AMessage, AMessage]);
    CM_MOUSEWHEEL           : Result := Format('CM_MOUSEWHEEL: %d/$%x', [AMessage, AMessage]);
    // Add some New Definitions
    CM_BASE + 68            : Result := Format('CM_ISSHORTCUT: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 69            : Result := Format('CM_UPDATEACTIONS: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 70            : Result := Format('CM_INVALIDATEDOCKHOST: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 71            : Result := Format('CM_SETACTIVECONTROL: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 72            : Result := Format('CM_POPUPHWNDDESTROY: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 73            : Result := Format('CM_CREATEPOPUP: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 74            : Result := Format('CM_DESTROYHANDLE: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 75            : Result := Format('CM_MOUSEACTIVATE: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 76            : Result := Format('CM_CONTROLLISTCHANGING: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 77            : Result := Format('CM_BUFFEREDPRINTCLIENT: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 78            : Result := Format('CM_UNTHEMECONTROL: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 79            : Result := Format('CM_DOUBLEBUFFEREDCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 80            : Result := Format('CM_PARENTDOUBLEBUFFEREDCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 81            : Result := Format('CM_STYLECHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 82            : Result := Format('CM_GESTURE: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 83            : Result := Format('CM_CUSTOMGESTURESCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 84            : Result := Format('CM_GESTUREMANAGERCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 85            : Result := Format('CM_STANDARDGESTURESCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 86            : Result := Format('CM_INPUTLANGCHANGE: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 87            : Result := Format('CM_TABLETOPTIONSCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 88            : Result := Format('CM_PARENTTABLETOPTIONSCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 89            : Result := Format('CM_CUSTOMSTYLECHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 90            : Result := Format('CM_SYSFONTSALLCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 91            : Result := Format('CM_PARENTVISIBLECHANGED: %d/$%x',  [AMessage, AMessage]);
    CM_BASE + 92            : Result := Format('CM_SYSCOMMAND: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 93            : Result := Format('CM_REMOTESESSIONSTATUSCHANGED: %d/$%x', [AMessage, AMessage]);
    CM_BASE + 94            : Result := Format('CM_STYLEELEMENTSCHANGED: %d/$%x', [AMessage, AMessage]);
    // VCL Control Notifications
    CN_BASE                 : Result := Format('CN_BASE: %d/$%x', [AMessage, AMessage]);
    CN_CHARTOITEM           : Result := Format('CN_CHARTOITEM: %d/$%x', [AMessage, AMessage]);
    CN_COMMAND              : Result := Format('CN_COMMAND: %d/$%x', [AMessage, AMessage]);
    CN_COMPAREITEM          : Result := Format('CN_COMPAREITEM: %d/$%x', [AMessage, AMessage]);
    CN_CTLCOLORBTN          : Result := Format('CN_CTLCOLORBTN: %d/$%x', [AMessage, AMessage]);
    CN_CTLCOLORDLG          : Result := Format('CN_CTLCOLORDLG: %d/$%x', [AMessage, AMessage]);
    CN_CTLCOLOREDIT         : Result := Format('CN_CTLCOLOREDIT: %d/$%x', [AMessage, AMessage]);
    CN_CTLCOLORLISTBOX      : Result := Format('CN_CTLCOLORLISTBOX: %d/$%x', [AMessage, AMessage]);
    CN_CTLCOLORMSGBOX       : Result := Format('CN_CTLCOLORMSGBOX: %d/$%x', [AMessage, AMessage]);
    CN_CTLCOLORSCROLLBAR    : Result := Format('CN_CTLCOLORSCROLLBAR: %d/$%x', [AMessage, AMessage]);
    CN_CTLCOLORSTATIC       : Result := Format('CN_CTLCOLORSTATIC: %d/$%x', [AMessage, AMessage]);
    CN_DELETEITEM           : Result := Format('CN_DELETEITEM: %d/$%x', [AMessage, AMessage]);
    CN_DRAWITEM             : Result := Format('CN_DRAWITEM: %d/$%x', [AMessage, AMessage]);
    CN_HSCROLL              : Result := Format('CN_HSCROLL: %d/$%x', [AMessage, AMessage]);
    CN_MEASUREITEM          : Result := Format('CN_MEASUREITEM: %d/$%x', [AMessage, AMessage]);
    CN_PARENTNOTIFY         : Result := Format('CN_PARENTNOTIFY: %d/$%x', [AMessage, AMessage]);
    CN_VKEYTOITEM           : Result := Format('CN_VKEYTOITEM: %d/$%x', [AMessage, AMessage]);
    CN_VSCROLL              : Result := Format('CN_VSCROLL: %d/$%x', [AMessage, AMessage]);
    CN_KEYDOWN              : Result := Format('CN_KEYDOWN: %d/$%x', [AMessage, AMessage]);
    CN_KEYUP                : Result := Format('CN_KEYUP: %d/$%x', [AMessage, AMessage]);
    CN_CHAR                 : Result := Format('CN_CHAR: %d/$%x', [AMessage, AMessage]);
    CN_SYSKEYDOWN           : Result := Format('CN_SYSKEYDOWN: %d/$%x', [AMessage, AMessage]);
    CN_SYSCHAR              : Result := Format('CN_SYSCHAR: %d/$%x', [AMessage, AMessage]);
    CN_NOTIFY               : Result := Format('CN_NOTIFY: %d/$%x', [AMessage, AMessage]);
  else
    Result := Format('Unknown Window Message: %d/$%x', [AMessage, AMessage]);
  end
end;

{$ENDIF}

procedure TCnDebugger.SetDumpFileName(const Value: string);
{$IFNDEF NDEBUG}
var
  Mode: Word;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  if FDumpFileName <> Value then
  begin
    FDumpFileName := Value;
    // Dump 时更新文件
    if FDumpToFile then
    begin
      if FDumpFile <> nil then
        FreeAndNil(FDumpFile);
      if FDumpFileName = '' then
        FDumpFileName := SCnDefaultDumpFileName;

      if FileExists(FDumpFileName) then
        Mode := fmOpenWrite
      else
        Mode := fmCreate;

      FDumpFile := TFileStream.Create(FDumpFileName,
        Mode or fmShareDenyWrite);
      FAfterFirstWrite := False; // 重新开另一文件，需要重新判断

      if FUseAppend then   // 追加则定位到结尾
      begin
{$IFDEF MSWINDOWS}
        FDumpFile.Seek(0, soFromEnd);
{$ELSE}
        FDumpFile.Seek(0, soEnd);
{$ENDIF}
      end
      else
      begin
{$IFDEF MSWINDOWS}
        FDumpFile.Seek(0, soFromBeginning); // 移动到开头
{$ELSE}
        FDumpFile.Seek(0, soBeginning);
{$ENDIF}
      end;

    end;
  end;
{$ENDIF}
end;

procedure TCnDebugger.SetDumpToFile(const Value: Boolean);
{$IFNDEF NDEBUG}
var
  Mode: Word;
{$ENDIF}
begin
{$IFNDEF NDEBUG}
  if FDumptoFile <> Value then
  begin
    FDumpToFile := Value;
    if FDumptoFile then
    begin
      if FDumpFileName = '' then
        FDumpFileName := SCnDefaultDumpFileName;

      try
        if FDumpFile <> nil then
          FreeAndNil(FDumpFile);

        if FileExists(FDumpFileName) then
          Mode := fmOpenWrite
        else
          Mode := fmCreate;

        FDumpFile := TFileStream.Create(FDumpFileName,
          Mode or fmShareDenyWrite);
        FAfterFirstWrite := False; // 重新开文件，需要重新判断

        if FUseAppend then // 追加则定位到结尾
        begin
{$IFDEF MSWINDOWS}
          FDumpFile.Seek(0, soFromEnd);
{$ELSE}
          FDumpFile.Seek(0, soEnd);
{$ENDIF}
        end
        else
        begin
{$IFDEF MSWINDOWS}
          FDumpFile.Seek(0, soFromBeginning); // 移动到开头
{$ELSE}
          FDumpFile.Seek(0, soBeginning);
{$ENDIF}
        end;
      except
        ;
      end;
    end
    else
    begin
      FreeAndNil(FDumpFile);
    end;
  end;
{$ENDIF}
end;

function TCnDebugger.GetAutoStart: Boolean;
begin
{$IFNDEF NDEBUG}
  Result := FAutoStart;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TCnDebugger.GetChannel: TCnDebugChannel;
begin
{$IFNDEF NDEBUG}
  Result := FChannel;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TCnDebugger.GetDumpFileName: string;
begin
{$IFNDEF NDEBUG}
  Result := FDumpFileName;
{$ELSE}
  Result := '';
{$ENDIF}
end;

function TCnDebugger.GetDumpToFile: Boolean;
begin
{$IFNDEF NDEBUG}
  Result := FDumpToFile;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TCnDebugger.GetFilter: TCnDebugFilter;
begin
{$IFNDEF NDEBUG}
  Result := FFilter;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TCnDebugger.GetUseAppend: Boolean;
begin
{$IFNDEF NDEBUG}
  Result := FUseAppend;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TCnDebugger.SetAutoStart(const Value: Boolean);
begin
{$IFNDEF NDEBUG}
  FAutoStart := Value;
{$ENDIF}
end;

procedure TCnDebugger.SetUseAppend(const Value: Boolean);
begin
{$IFNDEF NDEBUG}
  FUseAppend := Value;
{$ENDIF}
end;

function TCnDebugger.GetMessageCount: Integer;
begin
{$IFNDEF NDEBUG}
  Result := FMessageCount;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TCnDebugger.GetPostedMessageCount: Integer;
begin
{$IFNDEF NDEBUG}
  Result := FPostedMessageCount;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TCnDebugger.FormatConstArray(Args: array of const): string;
const
  CRLF = #13#10;
var
  I: Integer;
begin
  Result := 'Count ' + IntToStr(High(Args) - Low(Args) + 1) + CRLF;
  for I := Low(Args) to High(Args) do
  begin
    case Args[I].VType of
      vtInteger:
        Result := Result + 'Integer: ' + IntToStr(Args[I].VInteger) + CRLF;
      vtBoolean:
        begin
          if Args[I].VBoolean then
            Result := Result + 'Boolean: ' + 'True' + CRLF
          else
            Result := Result + 'Boolean: ' + 'False' + CRLF;
        end;
      vtChar:
        Result := Result + 'Char: ' + string(Args[I].VChar) + CRLF;
      vtExtended:
        Result := Result + 'Extended: ' + FloatToStr(Args[I].VExtended^) + CRLF;
      vtString:
        Result := Result + 'String: ' + string(PShortString(Args[I].VString)^) + CRLF;
      vtPointer:
        Result := Result + 'Pointer: ' + IntToHex(TCnNativeInt(Args[I].VPointer), CN_HEX_DIGITS) + CRLF;
      vtPChar:
        Result := Result + 'PChar: ' + string(Args[I].VPChar) + CRLF;
      vtObject:
        Result := Result + 'Object: ' + Args[I].VObject.ClassName + IntToHex(TCnNativeInt
          (Args[I].VObject), CN_HEX_DIGITS) + CRLF;
      vtClass:
        Result := Result + 'Class: ' + Args[I].VClass.ClassName + CRLF;
      vtWideChar:
        Result := Result + 'WideChar: ' + Args[I].VWideChar + CRLF;
      vtPWideChar:
        Result := Result + 'PWideChar: ' + Args[I].VPWideChar + CRLF;
      vtAnsiString:
        Result := Result + 'AnsiString: ' + string(AnsiString(PAnsiChar(Args[I].VAnsiString))) + CRLF;
      vtCurrency:
        Result := Result + 'Currency: ' + CurrToStr(Args[I].VCurrency^) + CRLF;
      vtVariant:
        Result := Result + 'Variant: ' + string(Args[I].VVariant^) + CRLF;
      vtInterface:
        Result := Result + 'Interface: ' + IntToHex(TCnNativeInt(Args[I].VInterface), CN_HEX_DIGITS) + CRLF;
      vtWideString:
        Result := Result + 'WideString: ' + WideString(PWideChar(Args[I].VWideString)) + CRLF;
      vtInt64:
        Result := Result + 'Int64: ' + IntToStr(Args[I].VInt64^) + CRLF;
{$IFDEF UNICODE}
      vtUnicodeString:
        Result := Result + 'UnicodeString: ' + string(PWideChar(Args[I].VUnicodeString)) + CRLF;
{$ENDIF}
    end;
  end;
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

function TCnDebugger.GetEnumTypeStr<T>: string;
var
  Rtx: TRttiContext;
  Rt: TRttiType;
  Rot: TRttiOrdinalType;
  I: Integer;
begin
  Result := '';
  Rt := Rtx.GetType(TypeInfo(T));
  if Rt.IsOrdinal then
  begin
    Rot := Rt.AsOrdinal;
    for I := Rot.MinValue to Rot.MaxValue do
    begin
      if Result = '' then
        Result := GetEnumName(TypeInfo(T), I)
      else
        Result := Result + ', ' + GetEnumName(TypeInfo(T), I);
    end;
    Result := '(' + Result + ')';
  end;
end;

{$ENDIF}

procedure TCnDebugger.LogClass(const AClass: TClass; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFmt(SCnClassFmt, [SCnClass, AClass.ClassName, AClass.InstanceSize,
      #13#10, FormatClassString(AClass)])
  else
    LogFmt(SCnClassFmt, [AMsg, AClass.ClassName, AClass.InstanceSize,
      #13#10, FormatClassString(AClass)]);
{$ENDIF}
end;

procedure TCnDebugger.LogClassByName(const AClassName: string; const AMsg: string);
{$IFDEF DEBUG}
var
  AClass: TPersistentClass;
{$ENDIF}
begin
{$IFDEF DEBUG}
  AClass := GetClass(AClassName);
  if AClass <> nil then
    LogClass(AClass, AMsg)
  else
    LogMsgError('No Persistent Class Found for ' + AClassName);
{$ENDIF}
end;

procedure TCnDebugger.LogInterface(const AIntf: IUnknown; const AMsg: string);
begin
{$IFDEF DEBUG}
  if AMsg = '' then
    LogFmt(SCnInterfaceFmt, [SCnInterface, FormatInterfaceString(AIntf)])
  else
    LogFmt(SCnInterfaceFmt, [AMsg, FormatInterfaceString(AIntf)]);
{$ENDIF}
end;

procedure TCnDebugger.TraceClass(const AClass: TClass; const AMsg: string);
begin
  if AMsg = '' then
    TraceFmt(SCnClassFmt, [SCnClass, AClass.ClassName, AClass.InstanceSize,
      #13#10, FormatClassString(AClass)])
  else
    TraceFmt(SCnClassFmt, [AMsg, AClass.ClassName, AClass.InstanceSize,
      #13#10, FormatClassString(AClass)]);
end;

procedure TCnDebugger.TraceClassByName(const AClassName: string; const AMsg: string);
var
  AClass: TPersistentClass;
begin
  AClass := GetClass(AClassName);
  if AClass <> nil then
    TraceClass(AClass, AMsg)
  else
    TraceMsgError('No Persistent Class Found for ' + AClassName);
end;

procedure TCnDebugger.TraceInterface(const AIntf: IUnknown; const AMsg: string);
begin
  if AMsg = '' then
    TraceFmt(SCnInterfaceFmt, [SCnInterface, FormatInterfaceString(AIntf)])
  else
    TraceFmt(SCnInterfaceFmt, [AMsg, FormatInterfaceString(AIntf)]);
end;

function TCnDebugger.FormatClassString(AClass: TClass): string;
var
  List: TStrings;
begin
  List := nil;
  try
    try
      List := TStringList.Create;
      AddClassToStringList(AClass, List, 0);
    except
      List.Add(SCnObjException);
    end;
    Result := List.Text;
  finally
    List.Free;
  end;
end;

function TCnDebugger.FormatInterfaceString(AIntf: IUnknown): string;
var
  Obj: TObject;
  Intfs: string;
  List: TStrings;
begin
  Result := IntToHex(TCnNativeInt(AIntf), CN_HEX_DIGITS);
  if AIntf <> nil then
  begin
    Obj := ObjectFromInterface(AIntf);
    if Obj <> nil then
    begin
      Result := Result + ' ' + SCnCRLF + ' ' + Obj.ClassName + ': ' +
        IntToHex(TCnNativeInt(Obj), CN_HEX_DIGITS);

      List := TStringList.Create;
      try
        AddObjectToStringList(Obj, List, 0);
        Result := Result + SCnCRLF + List.Text;
      finally
        List.Free;
      end;

      Intfs := FormatObjectInterface(Obj);
      if Intfs <> '' then
        Result := Result + ' ' + SCnCRLF + 'Supports Interfaces:' + Intfs;
    end;
  end;
end;

function TCnDebugger.GUIDToString(const GUID: TGUID): string;
begin
  SetLength(Result, 38);
  StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
    GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;

function TCnDebugger.FormatObjectInterface(AObj: TObject): string;
var
  ClassPtr: TClass;
  IntfTable: PInterfaceTable;
  IntfEntry: PInterfaceEntry;
  I: Integer;
begin
  Result := '';
  if AObj = nil then
    Exit;

  ClassPtr := AObj.ClassType;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
    begin
      for I := 0 to IntfTable.EntryCount-1 do
      begin
        IntfEntry := @IntfTable.Entries[I];
        Result := Result + ' ' + SCnCRLF + GUIDToString(IntfEntry^.IID);
        // TODO: If Enhanced RTTI, using IID to Find Actual Interface Type and Parse Methods.
      end;
    end;
    ClassPtr := ClassPtr.ClassParent;
  end;
end;

procedure TCnDebugger.GetCurrentTrace(Strings: TStrings);
{$IFDEF CAPTURE_STACK}
var
  I: Integer;
  List: TCnStackInfoList;
{$ENDIF}
begin
  if Strings = nil then
    Exit;
  Strings.Clear;

{$IFDEF CAPTURE_STACK}
  List := nil;
  try
    List := TCnCurrentStackInfoList.Create;
    if List.Count > 2 then
    begin
      List.Delete(0);  // 删除这里多余的堆栈条目，但可能随编译选项里的 StackFrame 不同有 2 到 3 个
      List.Delete(0);
    end;

    for I := 0 to List.Count - 1 do
      Strings.Add(GetLocationInfoStr(List.Items[I].CallerAddr));
  finally
    List.Free;
  end;
{$ELSE}
  Strings.Add(SCnStackTraceNotSupport);
{$ENDIF}
end;

procedure TCnDebugger.GetTraceFromAddr(StackBaseAddr: Pointer; Strings: TStrings);
{$IFDEF CAPTURE_STACK}
var
  I: Integer;
  List: TCnStackInfoList;
{$ENDIF}
begin
  if Strings = nil then
    Exit;
  Strings.Clear;

  if StackBaseAddr = nil then
  begin
    Strings.Add(SCnStackTraceNil);
    Exit;
  end;

{$IFDEF CAPTURE_STACK}
  List := nil;
  try
    List := TCnManualStackInfoList.Create(StackBaseAddr, nil);
    for I := 0 to List.Count - 1 do
      Strings.Add(GetLocationInfoStr(List.Items[I].CallerAddr));
  finally
    List.Free;
  end;
{$ELSE}
  Strings.Add(SCnStackTraceNotSupport);
{$ENDIF}
end;

procedure TCnDebugger.LogStackFromAddress(Addr: Pointer;
  const AMsg: string);
{$IFDEF DEBUG}
{$IFDEF CAPTURE_STACK}
var
  Strings: TStringList;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF DEBUG}
{$IFDEF CAPTURE_STACK}
  Strings := nil;
  try
    Strings := TStringList.Create;
    Strings.Add(GetLocationInfoStr(Addr));
    GetTraceFromAddr(Addr, Strings);
    LogMsgWithType(Format('Address $%p with Stack: %s', [Addr, AMsg + SCnCRLF + Strings.Text]), cmtInformation);
  finally
    Strings.Free;
  end;
{$ELSE}
  LogPointer(Addr, AMsg);
{$ENDIF}
{$ENDIF}
end;

procedure TCnDebugger.TraceStackFromAddress(Addr: Pointer;
  const AMsg: string);
{$IFDEF CAPTURE_STACK}
var
  Strings: TStringList;
{$ENDIF}
begin
{$IFDEF CAPTURE_STACK}
  Strings := nil;
  try
    Strings := TStringList.Create;
    Strings.Add(GetLocationInfoStr(Addr));
    GetTraceFromAddr(Addr, Strings);
    TraceMsgWithType(Format('Address $%p with Stack: %s', [Addr, AMsg + SCnCRLF + Strings.Text]), cmtInformation);
  finally
    Strings.Free;
  end;
{$ELSE}
  TracePointer(Addr, AMsg);
{$ENDIF}
end;

procedure TCnDebugger.LogAnsiCharSet(const ASet: TCnAnsiCharSet;
  const AMsg: string);
{$IFDEF DEBUG}
var
  SetVal: TCnAnsiCharSet;
{$ENDIF}
begin
{$IFDEF DEBUG}
  SetVal := ASet;
  if AMsg = '' then
    LogMsg(GetAnsiCharSetStr(@SetVal, SizeOf(SetVal)))
  else
    LogFmt('%s %s', [AMsg, GetAnsiCharSetStr(@SetVal, SizeOf(SetVal))]);
{$ENDIF}
end;

procedure TCnDebugger.LogCharSet(const ASet: TSysCharSet; const AMsg: string);
begin
{$IFDEF DEBUG}
  {$IFDEF UNICODE}
  LogWideCharSet(ASet, AMsg);
  {$ELSE}
  LogAnsiCharSet(ASet, AMsg);
  {$ENDIF}
{$ENDIF}
end;

{$IFDEF UNICODE}

procedure TCnDebugger.LogWideCharSet(const ASet: TCnWideCharSet;
  const AMsg: string);
var
  SetVal: TCnWideCharSet;
begin
{$IFDEF DEBUG}
  SetVal := ASet;
  // WideCharSet 被剪裁成 AnsiChar
  if AMsg = '' then
    LogMsg(GetAnsiCharSetStr(@SetVal, SizeOf(SetVal)))
  else
    LogFmt('%s %s', [AMsg, GetAnsiCharSetStr(@SetVal, SizeOf(SetVal))]);
{$ENDIF}
end;

{$ENDIF}

procedure TCnDebugger.TraceAnsiCharSet(const ASet: TCnAnsiCharSet;
  const AMsg: string);
var
  SetVal: TCnAnsiCharSet;
begin
  SetVal := ASet;
  if AMsg = '' then
    TraceMsg(GetAnsiCharSetStr(@SetVal, SizeOf(SetVal)))
  else
    TraceFmt('%s %s', [AMsg, GetAnsiCharSetStr(@SetVal, SizeOf(SetVal))]);
end;

procedure TCnDebugger.TraceCharSet(const ASet: TSysCharSet;
  const AMsg: string);
begin
{$IFDEF UNICODE}
  TraceWideCharSet(ASet, AMsg);
{$ELSE}
  TraceAnsiCharSet(ASet, AMsg);
{$ENDIF}
end;

{$IFDEF UNICODE}

procedure TCnDebugger.TraceWideCharSet(const ASet: TCnWideCharSet;
  const AMsg: string);
var
  SetVal: TCnWideCharSet;
begin
  SetVal := ASet;
  // WideCharSet 被剪裁成 AnsiChar
  if AMsg = '' then
    LogMsg(GetAnsiCharSetStr(@SetVal, SizeOf(SetVal)))
  else
    LogFmt('%s %s', [AMsg, GetAnsiCharSetStr(@SetVal, SizeOf(SetVal))]);
end;

{$ENDIF}

procedure TCnDebugger.EvaluateInterfaceInstance(const AIntf: IUnknown;
  SyncMode: Boolean);
var
  Obj: TObject;
begin
  Obj := ObjectFromInterface(AIntf);
  if Obj <> nil then
    EvaluateObject(Obj, SyncMode);
end;

procedure TCnDebugger.WatchClear(const AVarName: string);
begin
  if AVarName <> '' then
    TraceFull(AVarName, CurrentTag, CurrentLevel, cmtClearWatch);
end;

procedure TCnDebugger.WatchFmt(const AVarName, AFormat: string;
  Args: array of const);
begin
  if AVarName <> '' then
    TraceFull(AVarName + '|' + FormatMsg(AFormat, Args), CurrentTag, CurrentLevel, cmtWatch);
end;

procedure TCnDebugger.WatchMsg(const AVarName, AValue: string);
begin
  if AVarName <> '' then
    TraceFull(AVarName + '|' + AValue, CurrentTag, CurrentLevel, cmtWatch);
end;

procedure TCnDebugger.Enable;
begin
  Active := True;
end;

procedure TCnDebugger.Disable;
begin
  Active := False;
end;

procedure TCnDebugger.FindComponent;
var
  I: Integer;
begin
  if FComponentFindList = nil then
    FComponentFindList := TList.Create
  else
    FComponentFindList.Clear;

  FFindAbort := False;
  InternalFindComponent(Application);
  if FFindAbort then
    Exit;

{$IFDEF ENABLE_FMX}
  for I := 0 to Screen.FormCount - 1 do
  begin
    InternalFindComponent(Screen.Forms[I]);
    if FFindAbort then
      Exit;
  end;
{$ELSE}
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    InternalFindComponent(Screen.CustomForms[I]);
    if FFindAbort then
      Exit;
  end;
{$ENDIF}
end;

procedure TCnDebugger.FindControl;
var
  I: Integer;
{$IFDEF ENABLE_FMX}
  J: Integer;
  F: TCustomForm;
{$ENDIF}
begin
  if FControlFindList = nil then
    FControlFindList := TList.Create
  else
    FControlFindList.Clear;

  FFindAbort := False;
{$IFDEF ENABLE_FMX}
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is TCustomForm then
    begin
      F := Screen.Forms[I] as TCustomForm;
      for J := 0 to F.ChildrenCount - 1 do
      begin
        if F.Children[J] is TControl then
        begin
          InternalFindControl(F.Children[J] as TControl);
          if FFindAbort then
            Exit;
        end;
      end;
    end;
  end;
{$ELSE}
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    InternalFindControl(Screen.CustomForms[I]);
    if FFindAbort then
      Exit;
  end;
{$ENDIF}
end;

procedure TCnDebugger.InternalFindComponent(AComponent: TComponent);
var
  I: Integer;
begin
  if FComponentFindList.IndexOf(AComponent) >= 0 then
    Exit;

  FComponentFindList.Add(AComponent);
  if Assigned(FOnFindComponent) then
  begin
    FOnFindComponent(Self, AComponent, FFindAbort);
    if FFindAbort then
      Exit;
  end;

  for I := 0 to AComponent.ComponentCount - 1 do
    InternalFindComponent(AComponent.Components[I]);
end;

procedure TCnDebugger.InternalFindControl(AControl: TControl);
var
  I: Integer;
begin
  if FControlFindList.IndexOf(AControl) >= 0 then
    Exit;

  FControlFindList.Add(AControl);
  if Assigned(FOnFindControl) then
  begin
    FOnFindControl(Self, AControl, FFindAbort);
    if FFindAbort then
      Exit;
  end;

{$IFDEF ENABLE_FMX}
    for I := 0 to AControl.ControlsCount - 1 do
      InternalFindControl(AControl.Controls[I]);
{$ELSE}
  if AControl is TWinControl then
    for I := 0 to TWinControl(AControl).ControlCount - 1 do
      InternalFindControl(TWinControl(AControl).Controls[I]);
{$ENDIF}
end;

{$IFDEF CAPTURE_STACK}

procedure TCnDebugger.ExceptionRecorder(ExceptObj: Exception; ExceptAddr: Pointer;
  IsOSException: Boolean; StackList: TCnStackInfoList);
var
  I: Integer;
  Strings: TStrings;
begin
  if not FCnDebugger.Active or not FCnDebugger.ExceptTracking then
    Exit;

  if FCnDebugger.FExceptFilter.IndexOf(ExceptObj.ClassName) >= 0 then
    Exit;

  if IsOSException then
    FCnDebugger.TraceMsgWithType('OS Exception: ' + ExceptObj.ClassName + ': '
      + ExceptObj.Message, cmtError)
  else
    FCnDebugger.TraceMsgWithType(ExceptObj.ClassName + ': ' + ExceptObj.Message,
      cmtError);

  if FIsInExcption then
  begin
    FCnDebugger.TraceMsgWithType('!!! Exception Reraised in CnDebug Handler !!!', cmtError);
    Exit;
  end;

  Strings := TStringList.Create;
  FIsInExcption := True;
  try
    for I := 0 to StackList.Count - 1 do
      Strings.Add(GetLocationInfoStr(StackList.Items[I].CallerAddr));
    FCnDebugger.TraceMsgWithType('Exception call stack:' + SCnCRLF +
      Strings.Text, cmtException);
  finally
    FIsInExcption := False;
    Strings.Free;
  end;
end;

{$ENDIF}

{ TCnDebugChannel }

function TCnDebugChannel.CheckFilterChanged: Boolean;
begin
  Result := False;
end;

function TCnDebugChannel.CheckReady: Boolean;
begin
  Result := False;
end;

constructor TCnDebugChannel.Create(IsAutoFlush: Boolean);
begin
  Active := True;
  FAutoFlush := IsAutoFlush;
end;

procedure TCnDebugChannel.RefreshFilter(Filter: TCnDebugFilter);
begin
// Do Nothing
end;

procedure TCnDebugChannel.SendContent(var MsgDesc; Size: Integer);
begin
// Do Nothing
end;

procedure TCnDebugChannel.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TCnDebugChannel.SetAutoFlush(const Value: Boolean);
begin
  if FAutoFlush <> Value then
  begin
    FAutoFlush := Value;
    UpdateFlush;
  end;
end;

procedure TCnDebugChannel.StartDebugViewer;
begin
// Do nothing
end;

procedure TCnDebugChannel.UpdateFlush;
begin
// Do nothing
end;

{$IFDEF MSWINDOWS}

{ TCnMapFileChannel }

function TCnMapFileChannel.CheckFilterChanged: Boolean;
var
  Header: PCnMapHeader;
begin
  Result := False;
  if FMapHeader <> nil then
  begin
    Header := FMapHeader;
    Result := Header^.Filter.NeedRefresh <> 0;
  end;
end;

function TCnMapFileChannel.CheckReady: Boolean;
begin
  Result := (FMap <> 0) and (FMapHeader <> nil) and (FQueueEvent <> 0);
  if not Result then
  begin
    FMap := OpenFileMapping(FILE_MAP_READ or FILE_MAP_WRITE, False, PChar(SCnDebugMapName));
    if FMap <> 0 then
    begin
      FMapHeader := MapViewOfFile(FMap, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
      if FMapHeader <> nil then
      begin
        FQueueEvent := OpenEvent(EVENT_MODIFY_STATE, False, PChar(SCnDebugQueueEventName));
        if (FQueueEvent <> 0) then
        begin
          UpdateFlush;
          Result := IsInitedFromHeader;
        end
        else
          OutputDebugString(PChar('CnDebug: OpenEvent Fail: ' + IntToStr(GetLastError)));
      end
      else
        OutputDebugString(PChar('CnDebug: MapViewOfFile Fail: ' + IntToStr(GetLastError)));
    end
    else
      OutputDebugString(PChar('CnDebug: OpenFileMapping Fail: ' + IntToStr(GetLastError)));
  end
  else // 区域都有效
    Result := PCnMapHeader(FMapHeader)^.MapEnabled = CnDebugMapEnabled;

  if not Result then
    DestroyHandles;
end;

constructor TCnMapFileChannel.Create(IsAutoFlush: Boolean = True);
begin
  inherited;
  UpdateFlush;
end;

destructor TCnMapFileChannel.Destroy;
begin
  DestroyHandles;
  inherited;
end;

procedure TCnMapFileChannel.DestroyHandles;
begin
  if FQueueFlush <> 0 then
  begin
    CloseHandle(FQueueFlush);
    FQueueFlush := 0;
  end;
  if FQueueEvent <> 0 then
  begin
    CloseHandle(FQueueEvent);
    FQueueEvent := 0;
  end;
  if FMapHeader <> nil then
  begin
    UnmapViewOfFile(FMapHeader);
    FMapHeader := nil;
  end;
  if FMap <> 0 then
  begin
    CloseHandle(FMap);
    FMap := 0;
  end;
end;

function TCnMapFileChannel.IsInitedFromHeader: Boolean;
var
  Header: PCnMapHeader;
begin
  Result := False;
  if (FMap <> 0) and (FMapHeader <> nil) then
  begin
    Header := FMapHeader;
    FMsgBase := Pointer(Header^.DataOffset + TCnNativeInt(FMapHeader));
    FMapSize := Header^.MapSize;
    FQueueSize := FMapSize - Header^.DataOffset;
    Result := (Header^.MapEnabled = CnDebugMapEnabled) and
      CompareMem(@(Header^.MagicName), PAnsiChar(AnsiString(CnDebugMagicName)), CnDebugMagicLength);
  end;
end;

procedure TCnMapFileChannel.LoadQueuePtr;
var
  Header: PCnMapHeader;
begin
  if (FMap <> 0) and (FMapHeader <> nil) then
  begin
    Header := FMapHeader;
    FFront := Header^.QueueFront;
    FTail := Header^.QueueTail;
  end;
end;

procedure TCnMapFileChannel.RefreshFilter(Filter: TCnDebugFilter);
var
  Header: PCnMapHeader;
  TagArray: array[0..CnMaxTagLength] of AnsiChar;
begin
  if (Filter <> nil) and (FMap <> 0) and (FMapHeader <> nil) then
  begin
    Header := FMapHeader;
    FillChar(TagArray, CnMaxTagLength + 1, 0);
    CopyMemory(@TagArray, @(Header^.Filter.Tag), CnMaxTagLength);

    Filter.Enabled := Header^.Filter.Enabled <> 0;
    Filter.Level := Header^.Filter.Level;
    Filter.Tag := string(TagArray);
    Filter.MsgTypes := Header^.Filter.MsgTypes;
    Header^.Filter.NeedRefresh := 0;
  end;
end;

procedure TCnMapFileChannel.SaveQueuePtr(SaveFront: Boolean = False);
var
  Header: PCnMapHeader;
begin
  if (FMap <> 0) and (FMapHeader <> nil) then
  begin
    Header := FMapHeader;
    Header^.QueueTail := FTail;
    if SaveFront then
      Header^.QueueFront := FFront;
  end;
end;

procedure TCnMapFileChannel.SendContent(var MsgDesc; Size: Integer);
var
  Mutex: THandle;
  Res: Cardinal;
  MsgLen, RestLen: Integer;
  IsFull: Boolean;
  MsgBuf : array[0..255] of Char;

  function BufferFull: Boolean;
  begin
    if FTail = FFront then      // 空队列
      Result := False
    else if FTail < FFront then // Tail 已经折返，Front 没有
      Result := FTail + Size < FFront
    // 都未折返，Tail 比 FFront 大
    else if FTail + Size < FQueueSize then // 新位置如不产生折返，则未满
      Result := False
    else if (FTail + Size) mod FQueueSize < FFront then // 新位置折返但不超过 Front
      Result := False
    else
      Result := True;
  end;

begin
  if Size > FQueueSize then Exit;
  // 从尾进，头由 Viewer 读出. Tail 一直前进，到尾折返
  // 写完数据后，才置增加的 Tail，Tail 指向下一个空位置
  IsFull := False;
  Mutex := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(SCnDebugQueueMutexName));
  if Mutex <> 0 then
  begin
    Res := WaitForSingleObject(Mutex, CnDebugWaitingMutexTime);
    if (Res = WAIT_TIMEOUT) or (Res = WAIT_FAILED) then // 出错或对方不释放，没法子，撤
    begin
      ShowError('Mutex Obtained Error.');
      CloseHandle(Mutex);
      Exit;
    end;
  end
  else
  begin
    ShowError('Mutex Opened Error.');
    DestroyHandles;
    Exit;  // 无 Mutex 便不写
  end;

  try
    LoadQueuePtr;
    if BufferFull then
    begin
      // 锁定并删队列头元素，直到有足够的空间来容纳本 Size 为止
      IsFull := True;
      repeat
        MsgLen := PInteger(TCnNativeInt(FMsgBase) + FFront)^;
        FFront := (FFront + MsgLen) mod FQueueSize;
      until not BufferFull;
      // 删完毕，进入写步骤 -- 以上可以考虑改成直接清空队列
    end;

    // 先写数据再改指针
    if FTail + Size < FQueueSize then
    begin
      CopyMemory(Pointer(TCnNativeInt(FMsgBase) + FTail), @MsgDesc, Size);
    end
    else
    begin
      RestLen := FQueueSize - FTail;
      if RestLen < SizeOf(Integer) then // 剩余空间不足以容纳信息头的 Length 字段
      begin
        CopyMemory(Pointer(TCnNativeInt(FMsgBase) + FTail), @MsgDesc, SizeOf(Integer));
        // 强行复制，要求队列超出 QueueSize 外的尾部至少有 SizeOf(Integer) 的空余缓冲
        // 可不如此做，但会增加 Viewer 读取长度时的回溯困难
      end
      else
        CopyMemory(Pointer(TCnNativeInt(FMsgBase) + FTail), @MsgDesc, RestLen);

      CopyMemory(FMsgBase, Pointer(TCnNativeInt(@MsgDesc) + RestLen), Size - RestLen);
    end;

    Inc(FTail, Size);
    if FTail >= FQueueSize then
      FTail := FTail mod FQueueSize;

    SaveQueuePtr(IsFull);
    if Mutex <> 0 then
    begin
      ReleaseMutex(Mutex);
      CloseHandle(Mutex);
    end;
    SetEvent(FQueueEvent);
    if AutoFlush and (FQueueFlush <> 0) then
    begin
      Res := WaitForSingleObject(FQueueFlush, CnDebugFlushEventTime);
      if Res = WAIT_FAILED then
      begin
        Res := GetLastError;
        // 处理出错情况, 5 是拒绝访问。
        FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, Res,
          LANG_NEUTRAL or (SUBLANG_DEFAULT shl 10), // Default language
          PChar(@MsgBuf),
          Sizeof(MsgBuf)-1,
          nil);
        ShowError(MsgBuf);
      end;
    end;

  except
    DestroyHandles;
  end;
end;

procedure TCnMapFileChannel.StartDebugViewer;
const
  SCnDebugViewerExeName = 'CnDebugViewer.exe';
  SCnDotExe = '.exe';
  SCn64DotExe = '64.exe';
var
  hStarting: THandle;
  Reg: TRegistry;
  S, Subfix, Subfix64: string;
  Len: Integer;
  ViewerExe: AnsiString;
begin
  ViewerExe := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\CnPack\CnDebug', False) then
      S := Reg.ReadString('CnDebugViewer');
  finally
    Reg.CloseKey;
    Reg.Free;
  end;

  // 设置运行文件名
  if S <> '' then
    ViewerExe := AnsiString(S)
  else
    ViewerExe := SCnDebugViewerExeName;

  // 判断是否支持 64 位
  if IsWin64 then
  begin
    S := LowerCase(string(ViewerExe));
    Len := Length(S);

    if Len > Length(SCnDotExe) + 4 then
    begin
      if (S[1] = '"') and (S[Len] = '"') then
      begin
        Subfix := SCnDotExe + '"';
        Subfix64 := SCn64DotExe + '"';
      end
      else
      begin
        Subfix := SCnDotExe;
        Subfix64 := SCn64DotExe;
      end;

      if Copy(S, Len - Length(Subfix) + 1, MaxInt) = Subfix then // 末尾是 .exe
      begin
        if Copy(S, Len - Length(Subfix64) + 1, MaxInt) <> Subfix64 then // 但末尾不是 64.exe
        begin
          S := string(ViewerExe);
          Insert('64', S, Len - Length(Subfix) + 1);
          if FileExists(S) then
            Insert('64', ViewerExe, Len - Length(Subfix) + 1); // 把 64 插点前面并且判断文件存在不
        end;
      end;
    end;
  end;

  // 加上调用参数
  ViewerExe := ViewerExe + ' -a ';
  if FUseLocalSession then
    ViewerExe := ViewerExe + ' -local ';

  hStarting := CreateEvent(nil, False, False, PChar(SCnDebugStartEventName));
  if 31 < WinExec(PAnsiChar(ViewerExe + AnsiString(IntToStr(GetCurrentProcessId))),
    SW_SHOW) then // 成功创建，等待
  begin
    if hStarting <> 0 then
    begin
      WaitForSingleObject(hStarting, CnDebugStartingEventTime);
      CloseHandle(hStarting);
    end;
  end;
end;

procedure TCnMapFileChannel.UpdateFlush;
begin
  if FAutoFlush then
  begin
    if FQueueFlush = 0 then
      FQueueFlush := CreateEvent(nil, False, False, PChar(SCnDebugFlushEventName));
  end
  else if FQueueFlush <> 0 then
  begin
    CloseHandle(FQueueFlush);
    FQueueFlush := 0;
  end;
end;

{$ENDIF}

initialization
{$IFNDEF NDEBUG}
  {$IFDEF MSWINDOWS}

  {$IFDEF REDIRECT_OPDS}
  CnDebugChannelClass := nil; // 用 OutputDebugString 不创建 Channel
  {$ELSE}
  CnDebugChannelClass := TCnMapFileChannel;
  {$ENDIF}

  InitializeCriticalSection(FStartCriticalSection);
  InitializeCriticalSection(FCnDebuggerCriticalSection);
  {$IFDEF CAPTURE_STACK}
  InitializeCriticalSection(FInProcessCriticalSection);
  {$ENDIF}
  {$ELSE}
  FStartCriticalSection := TCnDebugCriticalSection.Create;
  FCnDebuggerCriticalSection := TCnDebugCriticalSection.Create;
  {$IFDEF CAPTURE_STACK}
  FInProcessCriticalSection := TCnDebugCriticalSection.Create;
  {$ENDIF}
  {$ENDIF}
  FCnDebugger := TCnDebugger.Create;

  {$IFNDEF REDIRECT_OPDS}
  FixCallingCPUPeriod;
  {$ENDIF}

  {$IFDEF CAPTURE_STACK}
  CnSetAdditionalExceptionRecorder(FCnDebugger.ExceptionRecorder);
  CnHookException;
  {$ENDIF}
{$ELSE}
  CnDebugChannelClass := nil; // NDEBUG 环境下不创建 Channel
{$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  {$IFDEF CAPTURE_STACK}
  DeleteCriticalSection(FInProcessCriticalSection);
  {$ENDIF}
  DeleteCriticalSection(FCnDebuggerCriticalSection);
  DeleteCriticalSection(FStartCriticalSection);
  {$ELSE}
  {$IFDEF CAPTURE_STACK}
  FInProcessCriticalSection.Free;
  {$ENDIF}
  FCnDebuggerCriticalSection.Free;
  FStartCriticalSection.Free;
  {$ENDIF}
{$IFDEF CAPTURE_STACK}
  CnUnHookException;
{$ENDIF}
  FreeAndNil(FCnDebugger);

end.

