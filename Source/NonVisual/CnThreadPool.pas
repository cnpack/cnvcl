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

{******************************************************************************
  This component is modified from Aleksej Petrov's threadpool, fixed some
  memory leaks and enhanced the scheduling implementation and fixed some bugs.

*******************************************************************************

    Component for processing request queue in pool of threads

    Copyright (c) 2001, Aleksej Petrov, AKPetrov@pisem.net

     Free for noncommercial use.
  Please contact with author for use in commercial projects.

*******************************************************************************}

unit CnThreadPool;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：线程池实现单元
* 单元作者：Chinbo（Shenloqi）
* 备    注：支持 D5 至最新版但对应例子用了 Indy 因而例子只能在 D7 或以上编译
*
*          设计：
*              实现线程池，首先要抽象出任务，最简单的就是使用一个指
*            向某一结构的指针，不过这样做显然不是一个好的方法，还有
*            一种比较简单的方法就是使用对象；有了抽象的任务之后，就
*            要有一个对任务的管理列表，这可以简单的用 TList 实现，尽管
*            TList 的事件少了一些；然后就要有执行处理任务的线程，这最
*            好要从 TThread 继承；然后要有通知机制，任务量，计算时间的
*            估算，任务平衡机制。
*              最简单的实现是工作线程作完一次任务之后就休眠，管理线
*            程使用一个定时器定期给这些线程进行分配，管理，不过这样
*            效率不高；也可以在增加任务的时候就进行一次分配，不过这
*            样的主动分配对于线程的任务均衡，效率等都不是好的解决方
*            法，而信号量则会比较好的解决这个问题。而线程池中线程的
*            释放则可以简单通过计时器实现。
*              真正的问题是如何估计工作量，线程池的工作强度，然后决
*            定何时需要增加线程，何时需要减少线程:)
*
*          限制：
*              考虑到线程池的使用范围，所以在实现中采用了不少只有在
*            NT 系列才实现的 API，所以该组件在 NT 系列操作系统上会有最好
*            的效果，当然也不支持非 Windows 环境。
*              因为使用了 WaitableTimer，所以在 9x 环境下线程池不会减少
*            线程池中的线程数目，应该是可以通过 SetTimer 替代，不过
*            SetTimer 需要一个消息循环处理 WM_TIMER 消息，开销较大且不
*            容易实现:)
*              另外还有一个替代的方法就是 mmsystem 的 timesetevent 函数,
*            不过这个函数的开销应该比其他的更大
*              不过如果通过 TTimer 来实现的话，应该就可以实现跨平台的
*            组件了...
*
*          内存泄漏：
*              一般情况下该组件不会有内存泄露，然而当线程池被释放时
*            线程池中还有正在工作的线程，且这些线程依赖的外部环境被
*            破坏时，为了线程能够正常退出不得已使用了 TerminateThread
*            函数，而这个函数不会清理线程分配的内存，就可能造成内存
*            泄露，幸运的是这种情形一般发生在应用程序退出时破坏了外
*            部环境所致，所以一旦应用程序退出，操作系统会做相应的清
*            理工作，所以实际上应该不算内存泄露:)
*              即使是使用了 TerminateThread，也不应该会造成程序退出时
*            的种种异常，如 RunTime Error 216
*
*          类及函数：
*
*            TCnCriticalSection -- 临界区的封装
*              在 NT 中实现了 TryEnterCriticalSection，在 SyncObjs.pas 中
*            的 TCriticalSection 没有封装这个函数，TCnCriticalSection
*            封装了它，且直接从 TObject 继承，开销应该小一些:)
*              TryEnter -- TryEnterCriticalSection
*              TryEnterEx -- 9x时候就是Enter，NT就是TryEnter
*
*            TCnTaskDataObject -- 线程池线程处理数据的封装
*              线程池中的线程处理任务所需的数据的基类
*              一般情况下，要实现某种特定的任务就需要实现相应的一个
*            从该类继承的类
*              Duplicate -- 是否与另一个处理数据相同，相同则不处理
*              Info -- 信息，用于调试输出
*
*            TCnPoolingThread -- 线程池中的线程
*              线程池中的线程的基类，一般情况下不需要继承该类就可以
*            实现大部分的操作，但在线程需要一些外部环境时可以继承该
*            类的构造和析构函数，另一种方法是可以在线程池的相关事件
*            中进行这些配置
*              AverageWaitingTime -- 平均等待时间
*              AverageProcessingTime -- 平均处理时间
*              Duplicate -- 是否正在处理相同的任务
*              Info -- 信息，用于调试输出
*              IsDead -- 是否已死
*              IsFinished -- 是否执行完成
*              IsIdle -- 是否空闲
*              NewAverage -- 计算平均值（特殊算法）
*              StillWorking -- 表明线程仍然在运行
*              Execute -- 线程执行函数，一般不需继承
*              Create -- 构造函数
*              Destroy -- 析构函数
*              Terminate -- 结束线程
*
*            TCnThreadPool -- 线程池
*              控件的事件并没有使用同步方式封装，所以这些事件的代码要线程安全才可以
*              HasSpareThread -- 有空闲的线程
*              AverageWaitingTime -- 平均等待时间
*              AverageProcessingTime -- 平均计算时间
*              TaskCount -- 任务数目
*              ThreadCount -- 线程数目
*              CheckTaskEmpty -- 检查任务是否都已经完成
*              GetRequest -- 从队列中获取任务
*              DecreaseThreads -- 减少线程
*              IncreaseThreads -- 增加线程
*              FreeFinishedThreads -- 释放完成的线程
*              KillDeadThreads -- 清除死线程
*              Info -- 信息，用于调试输出
*              OSIsWin9x -- 操作系统是Win9x
*              AddRequest -- 增加任务
*              RemoveRequest -- 从队列中删除任务
*              CreateSpecial -- 创建自定义线程池线程的构造函数
*              AdjustInterval -- 减少线程的时间间隔
*              DeadTaskAsNew -- 将死线程的任务重新加到队列
*              MinAtLeast -- 线程数不少于最小数目
*              ThreadDeadTimeout -- 线程死亡超时
*              ThreadsMinCount -- 最少线程数
*              ThreadsMaxCount -- 最大线程数
*              OnGetInfo -- 获取信息事件
*              OnProcessRequest -- 处理任务事件
*              OnQueueEmpty -- 队列空事件
*              OnThreadInitializing -- 线程初始化事件
*              OnThreadFinalizing -- 线程终止化事件
*
* 开发平台：PWin2000Pro + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串暂不符合本地化处理方式
* 修改记录：2008.4.1
*               找到了当初这个线程池的主要参考实现单元，补上了原作者
*                 Aleksej Petrov 的版权信息。尽管在原来的基础上修正了很多问题也
*                 增强了一些功能，但是整体的思路和大的实现方法还是跟原来的一样，
*                 再次感谢 Aleksej Petrov，也感谢 Leeon 帮我找到了原作者信息
*           2007.7.13
*               修改了 DeadTaskAsNew 会导致无限递归和不生效的 BUG
*               如果需要使用 DeadTaskAsNew 则需要实现 TCnTaskDataObject.Clone 方法
*               增加了一个 TCnThreadPool.AddRequests 方法
*           2004.8.9
*               公开了 TCnPoolingThread.StillWorking
*               简单修正了 TickCount 相减的 BUG
*           2004.3.14
*               修正一些 BUG
*           2003.12.24
*               使用 FTaskCount 和 FIdleThreadCount 加快执行效率
*               修正了 MinAtLeast 不能正确工作的 BUG
*               在 DefaultGetInfo 之中调用 FreeFinishedThreads
*           2003.12.21
*               完成并测试单元
*           2003.12.16
 *              创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF DEBUG}
  {.$UNDEF DEBUG} //是否输出调试信息
{$ENDIF}

uses
  SysUtils, Windows, Classes,
  CnConsts, CnClasses, CnCompConsts;

type
  TCnTaskDataObject = class
  {* 抽象出的线程池里待跑的任务，可判断重复}
  public
    function Clone: TCnTaskDataObject; virtual; abstract;
    {* 供子类实现复制自身用，必须实现}
    function Duplicate(DataObj: TCnTaskDataObject;
      const Processing: Boolean): Boolean; virtual;
    {* 供子类判断重复用，如重复，可免跑}
    function Info: string; virtual;
  end;

  { TCnPoolingThread }

  TCnThreadPool = class;

  TCnThreadState = (ctsInitializing, ctsWaiting,
    ctsGetting,
    ctsProcessing, ctsProcessed,
    ctsTerminating, ctsForReduce);

  TCnPoolingThread = class(TThread)
  private
    hInitFinished: THandle;
    sInitError: string;
{$IFDEF DEBUG}
    procedure Trace(const Str: string);
{$ENDIF}
    procedure ForceTerminate;
  protected
    FAverageWaitingTime: Integer;
    FAverageProcessing: Integer;
    FUWaitingStart: DWORD;
    FUProcessingStart: DWORD;
    FUStillWorking: DWORD;
    FWorkCount: Int64;
    FCurState: TCnThreadState;
    FHThreadTerminated: THandle;
    FPool: TCnThreadPool;
    FProcessingDataObject: TCnTaskDataObject; // 运行时在处理的任务对象，处理完后内部释放

    FCSProcessingDataObject: TObject; // Hide TCnCriticalSection;

    function AverageProcessingTime: DWORD;
    function AverageWaitingTime: DWORD;
    function CloneData: TCnTaskDataObject;
    function Duplicate(DataObj: TCnTaskDataObject): Boolean; virtual;
    function Info: string; virtual;
    function IsDead: Boolean; virtual;
    {* 对状态是否死掉了的判断，以备外部 Kill}
    function IsFinished: Boolean; virtual;
    function IsIdle: Boolean; virtual;
    function NewAverage(OldAvg, NewVal: Integer): Integer; virtual;

    procedure Execute; override;
  public
    constructor Create(aPool: TCnThreadPool); virtual;
    destructor Destroy; override;

    procedure StillWorking;
    procedure Terminate(const Force: Boolean = False);
  end;

  TCnPoolingThreadClass = class of TCnPoolingThread;

  { TCnThreadPool }

  TCnCheckDuplicate = (cdQueue, cdProcessing);
  TCnCheckDuplicates = set of TCnCheckDuplicate;
  {* 添加的任务如重复，要如何处理？分别代表队列中重复则删除，运行期重复则删除}

  TCnThreadPoolGetInfo = procedure(Sender: TCnThreadPool;
    var InfoText: string) of object;
  TCnThreadPoolProcessRequest = procedure(Sender: TCnThreadPool;
    DataObj: TCnTaskDataObject; Thread: TCnPoolingThread) of object;

  TCnPoolEmptyKind = (ekQueueEmpty, ekTaskEmpty);
  TCnQueueEmpty = procedure(Sender: TCnThreadPool;
    EmptyKind: TCnPoolEmptyKind) of object;

  TCnThreadInPoolInitializing = procedure(Sender: TCnThreadPool;
    Thread: TCnPoolingThread) of object;
  TCnThreadInPoolFinalizing = procedure(Sender: TCnThreadPool;
    Thread: TCnPoolingThread) of object;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnThreadPool = class(TCnComponent)
  private
    FCSQueueManagment: TObject; // Hide TCnCriticalSection;
    FCSThreadManagment: TObject; // Hide TCnCriticalSection;

    FQueue: TList;
    FThreads: TList;             // 运行线程对象列表
    FThreadsKilling: TList;      // 死线程对象列表
    FThreadsMinCount, FThreadsMaxCount: Integer;
    FThreadDeadTimeout: DWORD;
    FThreadClass: TCnPoolingThreadClass;
    FAdjustInterval: DWORD;
    FDeadTaskAsNew: Boolean;
    FMinAtLeast: Boolean;
    FIdleThreadCount, FTaskCount: Integer;

    FThreadInitializing: TCnThreadInPoolInitializing;
    FThreadFinalizing: TCnThreadInPoolFinalizing;
    FProcessRequest: TCnThreadPoolProcessRequest;
    FQueueEmpty: TCnQueueEmpty;
    FOnGetInfo: TCnThreadPoolGetInfo;
    FForceTerminate: Boolean;

    procedure SetAdjustInterval(const Value: DWORD);
{$IFDEF DEBUG}
    procedure Trace(const Str: string);
{$ENDIF}
  protected
    FLastGetPoint: Integer;
    FTerminateWaitTime: DWORD;
    FQueuePackCount: Integer;
    FHSemRequestCount: THandle;
    FHTimReduce: THandle;

    function HasSpareThread: Boolean;
    function HasTask: Boolean;
    function FinishedThreadsAreFull: Boolean; virtual;
    procedure CheckTaskEmpty;
    procedure GetRequest(var Request: TCnTaskDataObject);
    {* 从待运行队列中拿一个任务对象，准备运行}
    procedure DecreaseThreads;
    {* 减少一个运行线程}
    procedure IncreaseThreads;
    {* 增加一个运行线程，待接单}
    procedure FreeFinishedThreads;
    procedure KillDeadThreads;

    procedure DoProcessRequest(aDataObj: TCnTaskDataObject;
      aThread: TCnPoolingThread); virtual;
    procedure DoQueueEmpty(EmptyKind: TCnPoolEmptyKind); virtual;
    procedure DoThreadInitializing(aThread: TCnPoolingThread); virtual;
    procedure DoThreadFinalizing(aThread: TCnPoolingThread); virtual;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateSpecial(AOwner: TComponent;
      AClass: TCnPoolingThreadClass);
    destructor Destroy; override;

    function AverageWaitingTime: Integer;
    function AverageProcessingTime: Integer;
    function Info: string;
    function OSIsWin9x: Boolean;
    function TaskCount: Integer;
    function ThreadCount: Integer;
    function ThreadInfo(const I: Integer): string;
    function ThreadKillingCount: Integer;
    function ThreadKillingInfo(const I: Integer): string;
    procedure DefaultGetInfo(Sender: TCnThreadPool; var InfoText: string);

    function AddRequest(DataObject: TCnTaskDataObject;
      CheckDuplicate: TCnCheckDuplicates = [cdQueue]): Boolean;
    {* 外界添加待跑的任务数据，待线程池安排线程跑}
    procedure AddRequests(DataObjects: array of TCnTaskDataObject;
      CheckDuplicate: TCnCheckDuplicates = [cdQueue]);
    procedure RemoveRequest(DataObject: TCnTaskDataObject);

    property TerminateWaitTime: DWORD read FTerminateWaitTime write FTerminateWaitTime;
    property QueuePackCount: Integer read FQueuePackCount write FQueuePackCount;
  published
    property AdjustInterval: DWORD read FAdjustInterval write SetAdjustInterval
      default 10000;
    property DeadTaskAsNew: Boolean read FDeadTaskAsNew write FDeadTaskAsNew
      default True;
    property MinAtLeast: Boolean read FMinAtLeast write FMinAtLeast
      default False;
    property ThreadDeadTimeout: DWORD read FThreadDeadTimeout
      write FThreadDeadTimeout default 0;
    property ThreadsMinCount: Integer read FThreadsMinCount write FThreadsMinCount default 0;
    property ThreadsMaxCount: Integer read FThreadsMaxCount write FThreadsMaxCount default 10;

    property ForceTerminate: Boolean read FForceTerminate write FForceTerminate;
    {* 控制析构时是否强行停止线程}

    property OnGetInfo: TCnThreadPoolGetInfo read FOnGetInfo write FOnGetInfo;
    property OnProcessRequest: TCnThreadPoolProcessRequest read FProcessRequest
      write FProcessRequest;
    {* 线程池中的线程将执行任务时调用，要在此事件中写实际处理的线程代码}
    property OnQueueEmpty: TCnQueueEmpty read FQueueEmpty write FQueueEmpty;
    property OnThreadInitializing: TCnThreadInPoolInitializing
      read FThreadInitializing write FThreadInitializing;
    property OnThreadFinalizing: TCnThreadInPoolFinalizing read FThreadFinalizing
      write FThreadFinalizing;
  end;

{$IFDEF DEBUG}

  TLogWriteProc = procedure(const Str: string; const ID: Integer);

var
  TraceLog: TLogWriteProc = nil;

{$ENDIF}

const
  CCnTHREADSTATE: array[TCnThreadState] of string = (
    'ctsInitializing', 'ctsWaiting',
    'ctsGetting',
    'ctsProcessing', 'ctsProcessed',
    'ctsTerminating', 'ctsForReduce');

implementation

uses
  {$IFDEF DEBUG} CnDebug, {$ENDIF} Math;

type
  TCnCriticalSection = class
  protected
    FSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
    function TryEnterEx: Boolean;
  end;

var
  FOSIsWin9x: Boolean;

{$IFDEF DEBUG}

procedure SimpleTrace(const Str: string; const ID: Integer);
begin
  CnDebugger.LogFmt('$%x:%s', [ID, Str]);
  // OutputDebugString(PChar(IntToStr(ID) + ':' + Str))
end;

{$ENDIF}

function GetTickDiff(const AOldTickCount, ANewTickCount : Cardinal):Cardinal;
begin
  if ANewTickCount >= AOldTickCount then
  begin
    Result := ANewTickCount - AOldTickCount;
  end
  else
  begin
    Result := High(Cardinal) - AOldTickCount + ANewTickCount;
  end;
end;

{ TCnCriticalSection }

constructor TCnCriticalSection.Create;
begin
  inherited;
  InitializeCriticalSection(FSection);
end;

destructor TCnCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited;
end;

procedure TCnCriticalSection.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TCnCriticalSection.Leave;
begin
  LeaveCriticalSection(FSection);
end;

function TCnCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(FSection)
end;

function TCnCriticalSection.TryEnterEx: Boolean;
begin
  if FOSIsWin9x then
  begin
    Enter;
    Result := True;
  end
  else
    Result := TryEnter;
end;

{ TCnTaskDataObject }

function TCnTaskDataObject.Duplicate(DataObj: TCnTaskDataObject;
  const Processing: Boolean): Boolean;
begin
  Result := False;
end;

function TCnTaskDataObject.Info: string;
begin
  Result := IntToHex(Cardinal(Self), 8);
end;

{ TCnPoolingThread }

constructor TCnPoolingThread.Create(aPool: TCnThreadPool);
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Create');
{$ENDIF}

  inherited Create(True);
  FPool := aPool;

  FAverageWaitingTime := 0;
  FAverageProcessing := 0;
  FWorkCount := 0;
  sInitError := '';
  FreeOnTerminate := False;
  hInitFinished := CreateEvent(nil, True, False, nil);
  FHThreadTerminated := CreateEvent(nil, True, False, nil);
  FCSProcessingDataObject := TCnCriticalSection.Create;
  try
    Resume;
    WaitForSingleObject(hInitFinished, INFINITE);
    if sInitError <> '' then
      raise Exception.Create(sInitError);
  finally
    CloseHandle(hInitFinished);
  end;

{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Created OK');
{$ENDIF}
end;

destructor TCnPoolingThread.Destroy;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Destroy');
{$ENDIF}

  FreeAndNil(FProcessingDataObject);
  CloseHandle(FHThreadTerminated);
  FreeAndNil(FCSProcessingDataObject);
  inherited;
end;

function TCnPoolingThread.AverageProcessingTime: DWORD;
begin
  if FCurState in [ctsProcessing] then
    Result := NewAverage(FAverageProcessing, GetTickDiff(FUProcessingStart, GetTickCount))
  else
    Result := FAverageProcessing
end;

function TCnPoolingThread.AverageWaitingTime: DWORD;
begin
  if FCurState in [ctsWaiting, ctsForReduce] then
    Result := NewAverage(FAverageWaitingTime, GetTickDiff(FUWaitingStart, GetTickCount))
  else
    Result := FAverageWaitingTime
end;

function TCnPoolingThread.Duplicate(DataObj: TCnTaskDataObject): Boolean;
begin
  TCnCriticalSection(FCSProcessingDataObject).Enter;
  try
    Result := (FProcessingDataObject <> nil) and
      DataObj.Duplicate(FProcessingDataObject, True);
  finally
    TCnCriticalSection(FCSProcessingDataObject).Leave;
  end
end;

procedure TCnPoolingThread.ForceTerminate;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.ForceTerminate');
{$ENDIF}

  TerminateThread(Handle, 0)
end;

procedure TCnPoolingThread.Execute;
type
  THandleID = (hidRequest, hidReduce, hidTerminate);
var
  WaitedTime: Integer;
  Handles: array[THandleID] of THandle;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Execute');
{$ENDIF}

  FCurState := ctsInitializing;
  try
    FPool.DoThreadInitializing(Self);
  except
    on E: Exception do
      sInitError := E.Message;
  end;
  SetEvent(hInitFinished);

{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Execute: Initialized');
{$ENDIF}

  Handles[hidRequest] := FPool.FHSemRequestCount;
  Handles[hidReduce] := FPool.FHTimReduce;
  Handles[hidTerminate] := FHThreadTerminated;

  FUWaitingStart := GetTickCount;
  FProcessingDataObject := nil;

  while not Terminated do
  begin
    if not (FCurState in [ctsWaiting, ctsForReduce]) then
      InterlockedIncrement(FPool.FIdleThreadCount);

    FCurState := ctsWaiting; // 标记运行线程是等待状态
    case WaitForMultipleObjects(Length(Handles), @Handles, False, INFINITE) of
      WAIT_OBJECT_0 + Ord(hidRequest):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidRequest');
{$ENDIF}

          WaitedTime := GetTickDiff(FUWaitingStart, GetTickCount);
          FAverageWaitingTime := NewAverage(FAverageWaitingTime, WaitedTime);

          if FCurState in [ctsWaiting, ctsForReduce] then
            InterlockedDecrement(FPool.FIdleThreadCount);

          FCurState := ctsGetting; // 标记线程正在准备拿运行任务
          FPool.GetRequest(FProcessingDataObject);
          if FWorkCount < High(Int64) then
            FWorkCount := FWorkCount + 1;
          FUProcessingStart := GetTickCount;
          FUStillWorking := FUProcessingStart;

          FCurState := ctsProcessing; // 标记线程正在运行
          try
{$IFDEF DEBUG}
            Trace('Processing: ' + FProcessingDataObject.Info);
{$ENDIF}

            FPool.DoProcessRequest(FProcessingDataObject, Self)
          except
{$IFDEF DEBUG}
            on E: Exception do
              Trace('OnProcessRequest Exception: ' + E.Message);
{$ENDIF}
          end;

          TCnCriticalSection(FCSProcessingDataObject).Enter;
          try
            FreeAndNil(FProcessingDataObject);
          finally
            TCnCriticalSection(FCSProcessingDataObject).Leave;
          end;
          FAverageProcessing :=
            NewAverage(FAverageProcessing, GetTickDiff(FUProcessingStart, GetTickCount));

          FCurState := ctsProcessed; // 标记线程刚刚任务执行完毕
          FPool.CheckTaskEmpty;
          FUWaitingStart := GetTickCount;
        end;
      WAIT_OBJECT_0 + Ord(hidReduce):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidReduce');
{$ENDIF}

          if not (FCurState in [ctsWaiting, ctsForReduce]) then
            InterlockedIncrement(FPool.FIdleThreadCount);
          FCurState := ctsForReduce;
          FPool.DecreaseThreads;
        end;
      WAIT_OBJECT_0 + Ord(hidTerminate):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidTerminate');
{$ENDIF}

          if FCurState in [ctsWaiting, ctsForReduce] then
            InterlockedDecrement(FPool.FIdleThreadCount);

          FCurState := ctsTerminating; // 标记线程准备结束了
          Break;
        end;
    end;
  end;

  if FCurState in [ctsWaiting, ctsForReduce] then
    InterlockedDecrement(FPool.FIdleThreadCount);
  FCurState := ctsTerminating;
  FPool.DoThreadFinalizing(Self);
end;

function TCnPoolingThread.Info: string;
begin
  Result := 'AverageWaitingTime=' + IntToStr(AverageWaitingTime) + '; ' +
    'AverageProcessingTime=' + IntToStr(AverageProcessingTime) + '; ' +
    'FCurState=' + CCnTHREADSTATE[FCurState] + '; ' +
    'FWorkCount=' + IntToStr(FWorkCount);

  if not FPool.OSIsWin9x then
  begin
    if TCnCriticalSection(FCSProcessingDataObject).TryEnter then
    try
      Result := Result + '; ' + 'FProcessingDataObject = ';
      if FProcessingDataObject = nil then
        Result := Result + 'nil'
      else
        Result := Result + FProcessingDataObject.Info;
    finally
      TCnCriticalSection(FCSProcessingDataObject).Leave;
    end
  end
  else
  begin
    if FProcessingDataObject = nil then
      Result := Result + '; FProcessingDataObject = nil'
    else
      Result := Result + '; FProcessingDataObject <> nil';
  end
end;

function TCnPoolingThread.IsDead: Boolean;
begin
  Result := Terminated or
    ((FPool.ThreadDeadTimeout > 0) and
    (FCurState = ctsProcessing) and
    (GetTickDiff(FUStillWorking, GetTickCount) > FPool.ThreadDeadTimeout));
{$IFDEF DEBUG}
  if Result then
    Trace('Thread is Dead, Info = ' + Info);
{$ENDIF}
end;

function TCnPoolingThread.IsFinished: Boolean;
begin
  Result := WaitForSingleObject(Handle, 0) = WAIT_OBJECT_0
end;

function TCnPoolingThread.IsIdle: Boolean;
begin
  Result := (FCurState in [ctsWaiting, ctsForReduce]) and
    (AverageWaitingTime > 200) and
    (AverageWaitingTime * 2 > AverageProcessingTime)
end;

function TCnPoolingThread.NewAverage(OldAvg, NewVal: Integer): Integer;
begin
  if FWorkCount >= 8 then
    Result := (OldAvg * 7 + NewVal) div 8
  else if FWorkCount > 0 then
    Result := (OldAvg * FWorkCount + NewVal) div FWorkCount
  else
    Result := NewVal
end;

procedure TCnPoolingThread.StillWorking;
begin
  FUStillWorking := GetTickCount;
end;

procedure TCnPoolingThread.Terminate(const Force: Boolean);
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Terminate');
{$ENDIF}

  inherited Terminate;

  if Force then
  begin
    ForceTerminate;
    Free;
  end
  else
    SetEvent(FHThreadTerminated)
end;

{$IFDEF DEBUG}

procedure TCnPoolingThread.Trace(const Str: string);
begin
  TraceLog(Str, ThreadID);
end;
{$ENDIF}

function TCnPoolingThread.CloneData: TCnTaskDataObject;
begin
  TCnCriticalSection(FCSProcessingDataObject).Enter;
  try
    Result := nil;
    if FProcessingDataObject <> nil then
      Result := FProcessingDataObject.Clone;
  finally
    TCnCriticalSection(FCSProcessingDataObject).Leave;
  end;
end;

{ TCnThreadPool }

constructor TCnThreadPool.Create(AOwner: TComponent);
var
  DueTo: Int64;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.Create');
{$ENDIF}

  inherited;

  FCSQueueManagment := TCnCriticalSection.Create;
  FCSThreadManagment := TCnCriticalSection.Create;
  FQueue := TList.Create;
  FThreads := TList.Create;
  FThreadsKilling := TList.Create;
  FThreadsMinCount := 0;
  FThreadsMaxCount := 1;
  FThreadDeadTimeout := 0;
  FThreadClass := TCnPoolingThread;
  FAdjustInterval := 10000;
  FDeadTaskAsNew := True;
  FMinAtLeast := False;
  FLastGetPoint := 0;
  TerminateWaitTime := 10000;
  QueuePackCount := 127;
  FIdleThreadCount := 0;
  FTaskCount := 0;

  FHSemRequestCount := CreateSemaphore(nil, 0, $7FFFFFFF, nil);

  DueTo := -1;
  FHTimReduce := CreateWaitableTimer(nil, False, nil);

  if FHTimReduce = 0 then
    FHTimReduce := CreateEvent(nil, False, False, nil)
  else
    SetWaitableTimer(FHTimReduce, DueTo, FAdjustInterval, nil, nil, False);
end;

constructor TCnThreadPool.CreateSpecial(AOwner: TComponent;
  AClass: TCnPoolingThreadClass);
begin
  Create(AOwner);
  if AClass <> nil then
    FThreadClass := AClass
end;

destructor TCnThreadPool.Destroy;
var
  I, N: Integer;
  Handles: array of THandle;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.Destroy');
{$ENDIF}

  TCnCriticalSection(FCSThreadManagment).Enter;
  try
    SetLength(Handles, FThreads.Count);
    N := 0;

{$IFDEF DEBUG}
    Trace('TCnThreadPool Destroy: To Terminate FThreads.');
{$ENDIF}

    // 针对每个线程，先等两秒结束时间
    for I := 0 to FThreads.Count - 1 do
    begin
      if FThreads[I] <> nil then
      begin
        Handles[N] := TCnPoolingThread(FThreads[I]).Handle;
        TCnPoolingThread(FThreads[I]).Terminate(FForceTerminate);
        // 如 FForceTerminate 为 True，内部会 Free
        Inc(N);
      end;
    end;

{$IFDEF DEBUG}
    Trace('TCnThreadPool Destroy: WaitFor FThreads Terminate.');
{$ENDIF}

    WaitForMultipleObjects(N, @Handles[0], True, TerminateWaitTime);

{$IFDEF DEBUG}
    Trace('TCnThreadPool Destroy: Free FThreads Thread Instances.');
{$ENDIF}

    if not FForceTerminate then
    begin
      // 非强制情况下，再次挨个释放工作线程，不管其是否结束（注意强制情况下已 Free）
      for I := 0 to FThreads.Count - 1 do
      begin
        {if FThreads[i] <> nil then
          TCnPoolingThread(FThreads[i]).Terminate(True)
        else}
        TCnPoolingThread(FThreads[I]).Free;
      end;
    end;

{$IFDEF DEBUG}
    Trace('TCnThreadPool Destroy: Free FThreads List.');
{$ENDIF}

    // 释放线程列表
    FThreads.Free;

{$IFDEF DEBUG}
    Trace('TCnThreadPool Destroy: Free Finished Threads.');
{$ENDIF}

    // 释放完成了的死线程
    FreeFinishedThreads;

{$IFDEF DEBUG}
    Trace('TCnThreadPool Destroy: Free FThreadsKilling Thread Instances.');
{$ENDIF}

    // 释放其他死线程
    for I := 0 to FThreadsKilling.Count - 1 do
    begin
      {if FThreadsKilling[I] <> nil then
        TCnPoolingThread(FThreadsKilling[I]).Terminate(True)
      else}
      TCnPoolingThread(FThreadsKilling[I]).Free;
    end;

{$IFDEF DEBUG}
    Trace('TCnThreadPool Destroy: Free FThreadsKilling List.');
{$ENDIF}
    FThreadsKilling.Free;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end;
  FCSThreadManagment.Free;

  TCnCriticalSection(FCSQueueManagment).Enter;
  try
    for I := FQueue.Count - 1 downto 0 do
      TObject(FQueue[I]).Free;
    FQueue.Free;
  finally
    TCnCriticalSection(FCSQueueManagment).Leave;
  end;
  FCSQueueManagment.Free;

  CloseHandle(FHSemRequestCount);
  CloseHandle(FHTimReduce);

  inherited;
end;

function TCnThreadPool.AddRequest(DataObject: TCnTaskDataObject;
  CheckDuplicate: TCnCheckDuplicates): Boolean;
var
  I: Integer;
begin
{$IFDEF DEBUG}
  Trace('AddRequest:' + DataObject.Info);
{$ENDIF}

  Result := False;

  TCnCriticalSection(FCSQueueManagment).Enter;
  try
    if cdQueue in CheckDuplicate then
    begin
      for I := 0 to FQueue.Count - 1 do
      begin
        if (FQueue[I] <> nil) and
          DataObject.Duplicate(TCnTaskDataObject(FQueue[I]), False) then
        begin
{$IFDEF DEBUG}
          Trace('Duplicate:' + TCnTaskDataObject(FQueue[I]).Info);
{$ENDIF}

          FreeAndNil(DataObject);
          Exit;
        end;
      end;
    end;

    TCnCriticalSection(FCSThreadManagment).Enter;
    try
      IncreaseThreads;

      if cdProcessing in CheckDuplicate then
      begin
        for I := 0 to FThreads.Count - 1 do
        begin
          if TCnPoolingThread(FThreads[I]).Duplicate(DataObject) then
          begin
{$IFDEF DEBUG}
            Trace('Duplicate:' + TCnPoolingThread(FThreads[I]).FProcessingDataObject.Info);
{$ENDIF}
            FreeAndNil(DataObject);
            Exit;
          end;
        end;
      end;
    finally
      TCnCriticalSection(FCSThreadManagment).Leave;
    end;

    FQueue.Add(DataObject);
    Inc(FTaskCount);
    ReleaseSemaphore(FHSemRequestCount, 1, nil);
{$IFDEF DEBUG}
    Trace('ReleaseSemaphore');
{$ENDIF}

    Result := True;
  finally
    TCnCriticalSection(FCSQueueManagment).Leave;
  end;

{$IFDEF DEBUG}
  Trace('Added Request:' + DataObject.Info);
{$ENDIF}
end;

procedure TCnThreadPool.AddRequests(DataObjects: array of TCnTaskDataObject;
  CheckDuplicate: TCnCheckDuplicates);
var
  I: Integer;
begin
  for I := 0 to Length(DataObjects) - 1 do
    AddRequest(DataObjects[I], CheckDuplicate);
end;

procedure TCnThreadPool.CheckTaskEmpty;
var
  I: Integer;
begin
  TCnCriticalSection(FCSQueueManagment).Enter;
  try
    if FLastGetPoint < FQueue.Count then
      Exit;

    TCnCriticalSection(FCSThreadManagment).Enter;
    try
      for I := 0 to FThreads.Count - 1 do
      begin
        if TCnPoolingThread(FThreads[I]).FCurState in [ctsProcessing] then
          Exit;
      end;
    finally
      TCnCriticalSection(FCSThreadManagment).Leave;
    end;

    DoQueueEmpty(ekTaskEmpty);
  finally
    TCnCriticalSection(FCSQueueManagment).Leave;
  end
end;

procedure TCnThreadPool.DecreaseThreads;
var
  I: Integer;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.DecreaseThreads');
{$ENDIF}

  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  try
    KillDeadThreads;
    FreeFinishedThreads;

    for I := FThreads.Count - 1 downto FThreadsMinCount do
    begin
      if TCnPoolingThread(FThreads[I]).IsIdle then
      begin
        TCnPoolingThread(FThreads[I]).Terminate(False);
        FThreadsKilling.Add(FThreads[I]);
        FThreads.Delete(I);
        Break
      end;
    end;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end
end;

procedure TCnThreadPool.DefaultGetInfo(Sender: TCnThreadPool;
  var InfoText: string);
var
  I: Integer;
  sLine: string;
begin
  sLine := StringOfChar('=', 15);
  with Sender do
  begin
    FreeFinishedThreads;
    InfoText := 'MinCount=' + IntToStr(ThreadsMinCount) +
      '; MaxCount=' + IntToStr(ThreadsMaxCount) +
      '; AdjustInterval=' + IntToStr(AdjustInterval) +
      '; DeadTimeOut=' + IntToStr(ThreadDeadTimeout) + #13#10 +
      'ThreadCount=' + IntToStr(ThreadCount) +
      '; KillingCount=' + IntToStr(ThreadKillingCount) +
      '; SpareThreadCount=' + IntToStr(FIdleThreadCount) +
      '; TaskCount=' + IntToStr(TaskCount) + #13#10 +
      'AverageWaitingTime=' + IntToStr(AverageWaitingTime) +
      '; AverageProcessingTime=' + IntToStr(AverageProcessingTime) + #13#10 +
      {sLine + }'Working Threads Info' + sLine;
    for I := 0 to ThreadCount - 1 do
      InfoText := InfoText + #13#10 + ThreadInfo(I);
    InfoText := InfoText + #13#10 + {sLine +} 'Killing Threads Info' + sLine;
    for I := 0 to ThreadKillingCount - 1 do
      InfoText := InfoText + #13#10 + ThreadKillingInfo(I);
  end
end;

procedure TCnThreadPool.DoProcessRequest(aDataObj: TCnTaskDataObject;
  aThread: TCnPoolingThread);
begin
  if Assigned(FProcessRequest) then
    FProcessRequest(Self, aDataObj, aThread);
end;

procedure TCnThreadPool.DoQueueEmpty(EmptyKind: TCnPoolEmptyKind);
begin
  if Assigned(FQueueEmpty) then
    FQueueEmpty(Self, EmptyKind);
end;

procedure TCnThreadPool.DoThreadFinalizing(aThread: TCnPoolingThread);
begin
  if Assigned(FThreadFinalizing) then
    FThreadFinalizing(Self, aThread);
end;

procedure TCnThreadPool.DoThreadInitializing(aThread: TCnPoolingThread);
begin
  if Assigned(FThreadInitializing) then
    FThreadInitializing(Self, aThread);
end;

procedure TCnThreadPool.FreeFinishedThreads;
var
  I: Integer;
begin
  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  try
    for I := FThreadsKilling.Count - 1 downto 0 do
    begin
      if TCnPoolingThread(FThreadsKilling[I]).IsFinished then
      begin
        TCnPoolingThread(FThreadsKilling[I]).Free;
        FThreadsKilling.Delete(I);
      end;
    end;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end;
end;

procedure TCnThreadPool.GetRequest(var Request: TCnTaskDataObject);
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.GetRequest');
{$ENDIF}

  TCnCriticalSection(FCSQueueManagment).Enter;
  try
    while (FLastGetPoint < FQueue.Count) and (FQueue[FLastGetPoint] = nil) do
      Inc(FLastGetPoint);

    if (FQueue.Count > QueuePackCount) and
      (FLastGetPoint >= FQueue.Count * 3 div 4) then
    begin
{$IFDEF DEBUG}
      Trace('FQueue.Pack');
{$ENDIF DEBUG}

      FQueue.Pack;
      FTaskCount := FQueue.Count;
      FLastGetPoint := 0
    end;

    Request := TCnTaskDataObject(FQueue[FLastGetPoint]);
    FQueue[FLastGetPoint] := nil;
    Dec(FTaskCount);
    Inc(FLastGetPoint);

    if (FLastGetPoint = FQueue.Count) then
    begin
      DoQueueEmpty(ekQueueEmpty);
      FQueue.Clear;
      FTaskCount := 0;
      FLastGetPoint := 0;
    end;
  finally
    TCnCriticalSection(FCSQueueManagment).Leave;
  end
end;

function TCnThreadPool.HasSpareThread: Boolean;
begin
  Result := FIdleThreadCount > 0;
end;

function TCnThreadPool.HasTask: Boolean;
begin
  Result := FTaskCount > 0;
end;

function TCnThreadPool.FinishedThreadsAreFull: Boolean;
begin
  TCnCriticalSection(FCSThreadManagment).Enter;
  try
    if FThreadsMaxCount > 0 then
      Result := FThreadsKilling.Count >= FThreadsMaxCount div 2
    else
      Result := FThreadsKilling.Count >= 50;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end
end;

procedure TCnThreadPool.IncreaseThreads;
var
  iAvgWait, iAvgProc: Integer;
  I: Integer;
begin
  TCnCriticalSection(FCSThreadManagment).Enter;
  try
    KillDeadThreads;
    FreeFinishedThreads;

    if FThreads.Count = 0 then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count = 0');
{$ENDIF}

      try
        FThreads.Add(FThreadClass.Create(Self));
      except
{$IFDEF DEBUG}
        on E: Exception do
          Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF}
      end
    end
    else if FMinAtLeast and (FThreads.Count < FThreadsMinCount) then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count < FThreadsMinCount');
{$ENDIF}

      for I := FThreads.Count to FThreadsMinCount - 1 do
      try
        FThreads.Add(FThreadClass.Create(Self));
      except
{$IFDEF DEBUG}
        on E: Exception do
          Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF}
      end
    end
    else if (FThreads.Count < FThreadsMaxCount) and HasTask and not HasSpareThread then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count < FThreadsMaxCount');
{$ENDIF}
      I := TaskCount;
      if I <= 0 then
        Exit;

      iAvgWait := Max(AverageWaitingTime, 1);
      if iAvgWait > 100 then
        Exit;

      iAvgProc := Max(AverageProcessingTime, 2);
{$IFDEF DEBUG}
      Trace(Format(
        'ThreadCount(%D);ThreadsMaxCount(%D);AvgWait(%D);AvgProc(%D);TaskCount(%D);Killing(%D)',
        [FThreads.Count, FThreadsMaxCount, iAvgWait, iAvgProc, I, ThreadKillingCount]));
{$ENDIF}

      //if i * iAvgWait * 2 > iAvgProc * FThreads.Count then
      if ((iAvgProc + iAvgWait) * I > iAvgProc * FThreads.Count) then
      begin
        try
          FThreads.Add(FThreadClass.Create(Self));
        except
{$IFDEF DEBUG}
          on E: Exception do
            Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF}
        end;
      end;
    end;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end
end;

function TCnThreadPool.Info: string;
begin
  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  begin
    try
      if Assigned(FOnGetInfo) then
        FOnGetInfo(Self, Result)
      else
        DefaultGetInfo(Self, Result);
    finally
      TCnCriticalSection(FCSThreadManagment).Leave;
    end;
  end
  else
  begin
    Result := 'Too Busy to Get Info.';
  end;
end;

procedure TCnThreadPool.KillDeadThreads;
var
  I, iLen: Integer;
  LThread: TCnPoolingThread;
  LObjects: array of TCnTaskDataObject;
begin
  if FinishedThreadsAreFull then
    Exit;

  iLen := 0;
  SetLength(LObjects, iLen);
  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  try
    for I := FThreads.Count - 1 downto 0 do
    begin
      LThread := TCnPoolingThread(FThreads[I]);
      if LThread.IsDead then
      begin
        if FDeadTaskAsNew then
        begin
          Inc(iLen);
          SetLength(LObjects, iLen);
          LObjects[iLen - 1] := LThread.CloneData;
        end;

        LThread.Terminate(False);
        FThreadsKilling.Add(LThread);
        FThreads.Delete(I);

//        else
//        try
//          FThreads.Add(FThreadClass.Create(Self));
//        except
//{$IFDEF DEBUG}
//          on E: Exception do
//            Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
//{$ENDIF}
//        end
      end;
    end;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end;
  AddRequests(LObjects, []);
end;

function TCnThreadPool.OSIsWin9x: Boolean;
begin
  Result := FOsIsWin9x;
end;

function TCnThreadPool.AverageProcessingTime: Integer;
var
  I: Integer;
begin
  Result := 0;
  if FThreads.Count > 0 then
  begin
    for I := 0 to FThreads.Count - 1 do
      Inc(Result, TCnPoolingThread(FThreads[I]).AverageProcessingTime);
    Result := Result div FThreads.Count;
  end
  else
    Result := 20;
end;

function TCnThreadPool.AverageWaitingTime: Integer;
var
  I: Integer;
begin
  Result := 0;
  if FThreads.Count > 0 then
  begin
    for I := 0 to FThreads.Count - 1 do
      Inc(Result, TCnPoolingThread(FThreads[I]).AverageWaitingTime);
    Result := Result div FThreads.Count;
  end
  else
    Result := 10;
end;

procedure TCnThreadPool.RemoveRequest(DataObject: TCnTaskDataObject);
begin
  TCnCriticalSection(FCSQueueManagment).Enter;
  try
    FQueue.Remove(DataObject);
    Dec(FTaskCount);
    FreeAndNil(DataObject);
  finally
    TCnCriticalSection(FCSQueueManagment).Leave;
  end
end;

procedure TCnThreadPool.SetAdjustInterval(const Value: DWORD);
var
  DueTo: Int64;
begin
  FAdjustInterval := Value;
  if FHTimReduce <> 0 then
    SetWaitableTimer(FHTimReduce, DueTo, Value, nil, nil, False);
end;

function TCnThreadPool.TaskCount: Integer;
begin
  Result := FTaskCount;
end;

function TCnThreadPool.ThreadCount: Integer;
begin
  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  try
    Result := FThreads.Count;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end
  else
    Result := -1;
end;

function TCnThreadPool.ThreadInfo(const I: Integer): string;
begin
  Result := '';

  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  try
    if I < FThreads.Count then
      Result := TCnPoolingThread(FThreads[I]).Info;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end
end;

function TCnThreadPool.ThreadKillingCount: Integer;
begin
  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  try
    Result := FThreadsKilling.Count;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end
  else
    Result := -1;
end;

function TCnThreadPool.ThreadKillingInfo(const I: Integer): string;
begin
  Result := '';

  if TCnCriticalSection(FCSThreadManagment).TryEnter then
  try
    if I < FThreadsKilling.Count then
      Result := TCnPoolingThread(FThreadsKilling[I]).Info;
  finally
    TCnCriticalSection(FCSThreadManagment).Leave;
  end;
end;

procedure TCnThreadPool.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnThreadPoolName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnThreadPoolComment;
end;

{$IFDEF DEBUG}

procedure TCnThreadPool.Trace(const Str: string);
begin
  TraceLog(Str, 0)
end;

{$ENDIF}

var
  V: TOSVersionInfo;

initialization
  V.dwOSVersionInfoSize := SizeOf(V);
  FOSIsWin9x := GetVersionEx(V) and
    (V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);

{$IFDEF DEBUG}
  TraceLog := SimpleTrace;
{$ENDIF}

end.

