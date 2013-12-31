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

unit CnThreadPool;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：线程池实现单元
* 单元作者：Chinbo（Shenloqi）
* 备    注：
* 开发平台：PWin2000Pro + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串暂不符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.4.1
*               找到了当初这个线程池的主要参考实现单元，补上了原作者
*                 Aleksej Petrov的版权信息。尽管在原来的基础上修正了很多问题也
*                 增强了一些功能，但是整体的思路和大的实现方法还是跟原来的一样，
*                 再次感谢Aleksej Petrov，也感谢Leeon帮我找到了原作者信息
*           2007.7.13
*               修改了DeadTaskAsNew会导致无限递归和不生效的BUG
*               如果需要使用DeadTaskAsNew则需要实现TCnTaskDataObject.Clone方法
*               增加了一个TCnThreadPool.AddRequests方法
*           2004.8.9
*               公开了TCnPoolingThread.StillWorking
*               简单修正了TickCount相减的BUG
*           2004.3.14
*               修正一些BUG            
*           2003.12.24
*               使用FTaskCount和FIdleThreadCount加快执行效率
*               修正了MinAtLeast不能正确工作的BUG
*               在DefaultGetInfo之中调用FreeFinishedThreads
*           2003.12.21
*               完成并测试单元
*           2003.12.16
 *              创建单元，实现功能
================================================================================
|</PRE>}

(********************************************************
  This component is modified from Aleksej Petrov's threadpool, fixed some
  memory leaks and enhanced the scheduling implementation and fixed some bugs.

 {*************************************************************}

 {   Component for processing request queue in pool of threads }

 {   Copyright (c) 2001, Aleksej Petrov, AKPetrov@pisem.net    }

 {    Free for noncommercial use.                              }
 { Please contact with author for use in commercial projects.  }
********************************************************)

(********************************************************
单元说明：
    该单元实现了线程池的功能



设计：
    实现线程池，首先要抽象出任务，最简单的就是使用一个指
  向某一结构的指针，不过这样做显然不是一个好的方法，还有
  一种比较简单的方法就是使用对象；有了抽象的任务之后，就
  要有一个对任务的管理列表，这可以简单的用TList实现，尽管
  TList的事件少了一些；然后就要有执行处理任务的线程，这最
  好要从TThread继承；然后要有通知机制，任务量，计算时间的
  估算，任务平衡机制。
    最简单的实现是工作线程作完一次任务之后就休眠，管理线
  程使用一个定时器定期给这些线程进行分配，管理，不过这样
  效率不高；也可以在增加任务的时候就进行一次分配，不过这
  样的主动分配对于线程的任务均衡，效率等都不是好的解决方
  法，而信号量则会比较好的解决这个问题。而线程池中线程的
  释放则可以简单通过计时器实现。
    真正的问题是如何估计工作量，线程池的工作强度，然后决
  定何时需要增加线程，何时需要减少线程:)

限制：
    考虑到线程池的使用范围，所以在实现中采用了不少只有在
  NT系列才实现的API，所以该组件在NT系列操作系统上会有最好
  的效果，当然也不支持非Windows环境。
    因为使用了WaitableTimer，所以在9x环境下线程池不会减少
  线程池中的线程数目，应该是可以通过SetTimer替代，不过
  SetTimer需要一个消息循环处理WM_TIMER消息，开销较大且不
  容易实现:)
    另外还有一个替代的方法就是mmsystem的timesetevent函数,
  不过这个函数的开销应该比其他的更大
    不过如果通过TTimer来实现的话，应该就可以实现跨平台的
  组件了...

内存泄漏：
    一般情况下该组件不会有内存泄露，然而当线程池被释放时
  线程池中还有正在工作的线程，且这些线程依赖的外部环境被
  破坏时，为了线程能够正常退出不得已使用了TerminateThread
  函数，而这个函数不会清理线程分配的内存，就可能造成内存
  泄露，幸运的是这种情形一般发生在应用程序退出时破坏了外
  部环境所致，所以一旦应用程序退出，操作系统会做相应的清
  理工作，所以实际上应该不算内存泄露:)
    即使是使用了TerminateThread，也不应该会造成程序退出时
  的种种异常，如RunTime Error 216

类及函数：

  TCnCriticalSection -- 临界区的封装
    在NT中实现了TryEnterCriticalSection，在SyncObjs.pas中
  的TCriticalSection没有封装这个函数，TCnCriticalSection
  封装了它，且直接从TObject继承，开销应该小一些:)
    TryEnter -- TryEnterCriticalSection
    TryEnterEx -- 9x时候就是Enter，NT就是TryEnter

  TCnTaskDataObject -- 线程池线程处理数据的封装
    线程池中的线程处理任务所需的数据的基类
    一般情况下，要实现某种特定的任务就需要实现相应的一个
  从该类继承的类
    Duplicate -- 是否与另一个处理数据相同，相同则不处理
    Info -- 信息，用于调试输出

  TCnPoolingThread -- 线程池中的线程
    线程池中的线程的基类，一般情况下不需要继承该类就可以
  实现大部分的操作，但在线程需要一些外部环境时可以继承该
  类的构造和析构函数，另一种方法是可以在线程池的相关事件
  中进行这些配置
    AverageWaitingTime -- 平均等待时间
    AverageProcessingTime -- 平均处理时间
    Duplicate -- 是否正在处理相同的任务
    Info -- 信息，用于调试输出
    IsDead -- 是否已死
    IsFinished -- 是否执行完成
    IsIdle -- 是否空闲
    NewAverage -- 计算平均值（特殊算法）
    StillWorking -- 表明线程仍然在运行
    Execute -- 线程执行函数，一般不需继承
    Create -- 构造函数
    Destroy -- 析构函数
    Terminate -- 结束线程

  TCnThreadPool -- 线程池
    控件的事件并没有使用同步方式封装，所以这些事件的代码
  要线程安全才可以
    HasSpareThread -- 有空闲的线程
    AverageWaitingTime -- 平均等待时间
    AverageProcessingTime -- 平均计算时间
    TaskCount -- 任务数目
    ThreadCount -- 线程数目
    CheckTaskEmpty -- 检查任务是否都已经完成
    GetRequest -- 从队列中获取任务
    DecreaseThreads -- 减少线程
    IncreaseThreads -- 增加线程
    FreeFinishedThreads -- 释放完成的线程
    KillDeadThreads -- 清除死线程
    Info -- 信息，用于调试输出
    OSIsWin9x -- 操作系统是Win9x
    AddRequest -- 增加任务
    RemoveRequest -- 从队列中删除任务
    CreateSpecial -- 创建自定义线程池线程的构造函数
    AdjustInterval -- 减少线程的时间间隔
    DeadTaskAsNew -- 将死线程的任务重新加到队列
    MinAtLeast -- 线程数不少于最小数目
    ThreadDeadTimeout -- 线程死亡超时
    ThreadsMinCount -- 最少线程数
    ThreadsMaxCount -- 最大线程数
    OnGetInfo -- 获取信息事件
    OnProcessRequest -- 处理任务事件
    OnQueueEmpty -- 队列空事件
    OnThreadInitializing -- 线程初始化事件
    OnThreadFinalizing -- 线程终止化事件
********************************************************)

interface

{$I CnPack.inc}

{.$DEFINE DEBUG}//是否输出调试信息

uses
  SysUtils, Windows, Classes,
  CnConsts, CnClasses, CnCompConsts;

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

  TCnTaskDataObject = class
  public
    function Clone: TCnTaskDataObject; virtual; abstract;
    function Duplicate(DataObj: TCnTaskDataObject;
      const Processing: Boolean): Boolean; virtual;
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
{$ENDIF DEBUG}
    procedure ForceTerminate;
  protected
    FAverageWaitingTime: Integer;
    FAverageProcessing: Integer;
    uWaitingStart: DWORD;
    uProcessingStart: DWORD;
    uStillWorking: DWORD;
    FWorkCount: Int64;
    FCurState: TCnThreadState;
    hThreadTerminated: THandle;
    FPool: TCnThreadPool;
    FProcessingDataObject: TCnTaskDataObject;

    csProcessingDataObject: TCnCriticalSection;

    function AverageProcessingTime: DWORD;
    function AverageWaitingTime: DWORD;
    function CloneData: TCnTaskDataObject;
    function Duplicate(DataObj: TCnTaskDataObject): Boolean; virtual;
    function Info: string; virtual;
    function IsDead: Boolean; virtual;
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

  TCheckDuplicate = (cdQueue, cdProcessing);
  TCheckDuplicates = set of TCheckDuplicate;

  TGetInfo = procedure(Sender: TCnThreadPool;
    var InfoText: string) of object;
  TProcessRequest = procedure(Sender: TCnThreadPool;
    aDataObj: TCnTaskDataObject; aThread: TCnPoolingThread) of object;

  TEmptyKind = (ekQueueEmpty, ekTaskEmpty);
  TQueueEmpty = procedure(Sender: TCnThreadPool;
    EmptyKind: TEmptyKind) of object;

  TThreadInPoolInitializing = procedure(Sender: TCnThreadPool;
    aThread: TCnPoolingThread) of object;
  TThreadInPoolFinalizing = procedure(Sender: TCnThreadPool;
    aThread: TCnPoolingThread) of object;

  TCnThreadPool = class(TCnComponent)
  private
    csQueueManagment: TCnCriticalSection;
    csThreadManagment: TCnCriticalSection;

    FQueue: TList;
    FThreads: TList;
    FThreadsKilling: TList;
    FThreadsMinCount, FThreadsMaxCount: Integer;
    FThreadDeadTimeout: DWORD;
    FThreadClass: TCnPoolingThreadClass;
    FAdjustInterval: DWORD;
    FDeadTaskAsNew: Boolean;
    FMinAtLeast: Boolean;
    FIdleThreadCount, FTaskCount: Integer;

    FThreadInitializing: TThreadInPoolInitializing;
    FThreadFinalizing: TThreadInPoolFinalizing;
    FProcessRequest: TProcessRequest;
    FQueueEmpty: TQueueEmpty;
    FGetInfo: TGetInfo;

    procedure SetAdjustInterval(const Value: DWORD);
{$IFDEF DEBUG}
    procedure Trace(const Str: string);
{$ENDIF DEBUG}
  protected
    FLastGetPoint: Integer;
    hSemRequestCount: THandle;
    hTimReduce: THandle;

    function HasSpareThread: Boolean;
    function HasTask: Boolean;
    function FinishedThreadsAreFull: Boolean; virtual;
    procedure CheckTaskEmpty;
    procedure GetRequest(var Request: TCnTaskDataObject);
    procedure DecreaseThreads;
    procedure IncreaseThreads;
    procedure FreeFinishedThreads;
    procedure KillDeadThreads;

    procedure DoProcessRequest(aDataObj: TCnTaskDataObject;
      aThread: TCnPoolingThread); virtual;
    procedure DoQueueEmpty(EmptyKind: TEmptyKind); virtual;
    procedure DoThreadInitializing(aThread: TCnPoolingThread); virtual;
    procedure DoThreadFinalizing(aThread: TCnPoolingThread); virtual;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    uTerminateWaitTime: DWORD;
    QueuePackCount: Integer;

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
    function ThreadInfo(const i: Integer): string;
    function ThreadKillingCount: Integer;
    function ThreadKillingInfo(const i: Integer): string;
    procedure DefaultGetInfo(Sender: TCnThreadPool; var InfoText: string);

    function AddRequest(aDataObject: TCnTaskDataObject;
      CheckDuplicate: TCheckDuplicates = [cdQueue]): Boolean;
    procedure AddRequests(aDataObjects: array of TCnTaskDataObject;
      CheckDuplicate: TCheckDuplicates = [cdQueue]);
    procedure RemoveRequest(aDataObject: TCnTaskDataObject);
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

    property OnGetInfo: TGetInfo read FGetInfo write FGetInfo;
    property OnProcessRequest: TProcessRequest read FProcessRequest
      write FProcessRequest;
    property OnQueueEmpty: TQueueEmpty read FQueueEmpty write FQueueEmpty;
    property OnThreadInitializing: TThreadInPoolInitializing
      read FThreadInitializing write FThreadInitializing;
    property OnThreadFinalizing: TThreadInPoolFinalizing read FThreadFinalizing
      write FThreadFinalizing;
  end;

{$IFDEF DEBUG}
  TLogWriteProc = procedure(const Str: string; const ID: Integer);

var
  TraceLog: TLogWriteProc = nil;
{$ENDIF DEBUG}

const
  CCnTHREADSTATE: array[TCnThreadState] of string = (
    'ctsInitializing', 'ctsWaiting',
    'ctsGetting',
    'ctsProcessing', 'ctsProcessed',
    'ctsTerminating', 'ctsForReduce');

implementation

uses
  Math;

const
  MaxInt64 = High(Int64);

var
  FOSIsWin9x: Boolean;

{$IFDEF DEBUG}

procedure SimpleTrace(const Str: string; const ID: Integer);
begin
  OutputDebugString(PChar(IntToStr(ID) + ':' + Str))
end;
{$ENDIF DEBUG}

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
  InitializeCriticalSection(FSection)
end;

destructor TCnCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection)
end;

procedure TCnCriticalSection.Enter;
begin
  EnterCriticalSection(FSection)
end;

procedure TCnCriticalSection.Leave;
begin
  LeaveCriticalSection(FSection)
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
    Result := True
  end
  else
    Result := TryEnter
end;

{ TCnTaskDataObject }

function TCnTaskDataObject.Duplicate(DataObj: TCnTaskDataObject;
  const Processing: Boolean): Boolean;
begin
  Result := False
end;

function TCnTaskDataObject.Info: string;
begin
  Result := IntToHex(Cardinal(Self), 8)
end;

{ TCnPoolingThread }

constructor TCnPoolingThread.Create(aPool: TCnThreadPool);
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Create');
{$ENDIF DEBUG}

  inherited Create(True);
  FPool := aPool;

  FAverageWaitingTime := 0;
  FAverageProcessing := 0;
  FWorkCount := 0;
  sInitError := '';
  FreeOnTerminate := False;
  hInitFinished := CreateEvent(nil, True, False, nil);
  hThreadTerminated := CreateEvent(nil, True, False, nil);
  csProcessingDataObject := TCnCriticalSection.Create;
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
{$ENDIF DEBUG}
end;

destructor TCnPoolingThread.Destroy;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Destroy');
{$ENDIF DEBUG}

  FreeAndNil(FProcessingDataObject);
  CloseHandle(hThreadTerminated);
  csProcessingDataObject.Free;
  inherited;
end;

function TCnPoolingThread.AverageProcessingTime: DWORD;
begin
  if FCurState in [ctsProcessing] then
    Result := NewAverage(FAverageProcessing, GetTickDiff(uProcessingStart, GetTickCount))
  else
    Result := FAverageProcessing
end;

function TCnPoolingThread.AverageWaitingTime: DWORD;
begin
  if FCurState in [ctsWaiting, ctsForReduce] then
    Result := NewAverage(FAverageWaitingTime, GetTickDiff(uWaitingStart, GetTickCount))
  else
    Result := FAverageWaitingTime
end;

function TCnPoolingThread.Duplicate(DataObj: TCnTaskDataObject): Boolean;
begin
  csProcessingDataObject.Enter;
  try
    Result := (FProcessingDataObject <> nil) and
      DataObj.Duplicate(FProcessingDataObject, True);
  finally
    csProcessingDataObject.Leave
  end
end;

procedure TCnPoolingThread.ForceTerminate;
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.ForceTerminate');
{$ENDIF DEBUG}

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
{$ENDIF DEBUG}

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
{$ENDIF DEBUG}

  Handles[hidRequest] := FPool.hSemRequestCount;
  Handles[hidReduce] := FPool.hTimReduce;
  Handles[hidTerminate] := hThreadTerminated;

  uWaitingStart := GetTickCount;
  FProcessingDataObject := nil;

  while not Terminated do
  begin
    if not (FCurState in [ctsWaiting, ctsForReduce]) then
      InterlockedIncrement(FPool.FIdleThreadCount);
    FCurState := ctsWaiting;
    case WaitForMultipleObjects(Length(Handles), @Handles, False, INFINITE) of
      WAIT_OBJECT_0 + Ord(hidRequest):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidRequest');
{$ENDIF DEBUG}

          WaitedTime := GetTickDiff(uWaitingStart, GetTickCount);
          FAverageWaitingTime := NewAverage(FAverageWaitingTime, WaitedTime);

          if FCurState in [ctsWaiting, ctsForReduce] then
            InterlockedDecrement(FPool.FIdleThreadCount);
          FCurState := ctsGetting;
          FPool.GetRequest(FProcessingDataObject);
          if FWorkCount < MaxInt64 then
            FWorkCount := FWorkCount + 1;
          uProcessingStart := GetTickCount;
          uStillWorking := uProcessingStart;

          FCurState := ctsProcessing;
          try
{$IFDEF DEBUG}
            Trace('Processing: ' + FProcessingDataObject.Info);
{$ENDIF DEBUG}

            FPool.DoProcessRequest(FProcessingDataObject, Self)
          except
{$IFDEF DEBUG}
            on E: Exception do
              Trace('OnProcessRequest Exception: ' + E.Message);
{$ENDIF DEBUG}
          end;

          csProcessingDataObject.Enter;
          try
            FreeAndNil(FProcessingDataObject)
          finally
            csProcessingDataObject.Leave
          end;
          FAverageProcessing :=
            NewAverage(FAverageProcessing, GetTickDiff(uProcessingStart, GetTickCount));

          FCurState := ctsProcessed;
          FPool.CheckTaskEmpty;
          uWaitingStart := GetTickCount;
        end;
      WAIT_OBJECT_0 + Ord(hidReduce):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidReduce');
{$ENDIF DEBUG}

          if not (FCurState in [ctsWaiting, ctsForReduce]) then
            InterlockedIncrement(FPool.FIdleThreadCount);
          FCurState := ctsForReduce;
          FPool.DecreaseThreads;
        end;
      WAIT_OBJECT_0 + Ord(hidTerminate):
        begin
{$IFDEF DEBUG}
          Trace('TCnPoolingThread.Execute: hidTerminate');
{$ENDIF DEBUG}

          if FCurState in [ctsWaiting, ctsForReduce] then
            InterlockedDecrement(FPool.FIdleThreadCount);

          FCurState := ctsTerminating;
          Break
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
    if csProcessingDataObject.TryEnter then
    try
      Result := Result + '; ' + 'FProcessingDataObject=';
      if FProcessingDataObject = nil then
        Result := Result + 'nil'
      else
        Result := Result + FProcessingDataObject.Info
    finally
      csProcessingDataObject.Leave
    end
  end
  else
  begin
    if FProcessingDataObject = nil then
      Result := Result + '; ' + 'FProcessingDataObject=nil'
    else
      Result := Result + '; ' + 'FProcessingDataObject!=nil'
  end
end;

function TCnPoolingThread.IsDead: Boolean;
begin
  Result := Terminated or
    ((FPool.ThreadDeadTimeout > 0) and
    (FCurState = ctsProcessing) and
    (GetTickDiff(uStillWorking, GetTickCount) > FPool.ThreadDeadTimeout));
{$IFDEF DEBUG}
  if Result then
    Trace('Thread is dead, Info = ' + Info);
{$ENDIF DEBUG}
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
  uStillWorking := GetTickCount
end;

procedure TCnPoolingThread.Terminate(const Force: Boolean);
begin
{$IFDEF DEBUG}
  Trace('TCnPoolingThread.Terminate');
{$ENDIF DEBUG}

  inherited Terminate;

  if Force then
  begin
    ForceTerminate;
    Free
  end
  else
    SetEvent(hThreadTerminated)
end;

{$IFDEF DEBUG}

procedure TCnPoolingThread.Trace(const Str: string);
begin
  TraceLog(Str, ThreadID);
end;
{$ENDIF DEBUG}

function TCnPoolingThread.CloneData: TCnTaskDataObject;
begin
  csProcessingDataObject.Enter;
  try
    Result := nil;
    if FProcessingDataObject <> nil then
      Result := FProcessingDataObject.Clone;
  finally
    csProcessingDataObject.Leave;
  end;
end;

{ TCnThreadPool }

constructor TCnThreadPool.Create(AOwner: TComponent);
var
  DueTo: Int64;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.Create');
{$ENDIF DEBUG}

  inherited;

  csQueueManagment := TCnCriticalSection.Create;
  csThreadManagment := TCnCriticalSection.Create;
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
  uTerminateWaitTime := 10000;
  QueuePackCount := 127;
  FIdleThreadCount := 0;
  FTaskCount := 0;

  hSemRequestCount := CreateSemaphore(nil, 0, $7FFFFFFF, nil);

  DueTo := -1;
  hTimReduce := CreateWaitableTimer(nil, False, nil);

  if hTimReduce = 0 then
    hTimReduce := CreateEvent(nil, False, False, nil)
  else
    SetWaitableTimer(hTimReduce, DueTo, FAdjustInterval, nil, nil, False);
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
  i, n: Integer;
  Handles: array of THandle;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.Destroy');
{$ENDIF DEBUG}

  csThreadManagment.Enter;
  try
    SetLength(Handles, FThreads.Count);
    n := 0;
    for i := 0 to FThreads.Count - 1 do
      if FThreads[i] <> nil then
      begin
        Handles[n] := TCnPoolingThread(FThreads[i]).Handle;
        TCnPoolingThread(FThreads[i]).Terminate(False);
        Inc(n);
      end;
    WaitForMultipleObjects(n, @Handles[0], True, uTerminateWaitTime);

    for i := 0 to FThreads.Count - 1 do
    begin
      {if FThreads[i] <> nil then
        TCnPoolingThread(FThreads[i]).Terminate(True)
      else}
      TCnPoolingThread(FThreads[i]).Free;
    end;

    FThreads.Free;

    FreeFinishedThreads;
    for i := 0 to FThreadsKilling.Count - 1 do
    begin
      {if FThreadsKilling[i] <> nil then
        TCnPoolingThread(FThreadsKilling[i]).Terminate(True)
      else}
      TCnPoolingThread(FThreadsKilling[i]).Free;
    end;

    FThreadsKilling.Free;
  finally
    csThreadManagment.Free;
  end;

  csQueueManagment.Enter;
  try
    for i := FQueue.Count - 1 downto 0 do
      TObject(FQueue[i]).Free;
    FQueue.Free;
  finally
    csQueueManagment.Free;
  end;

  CloseHandle(hSemRequestCount);
  CloseHandle(hTimReduce);

  inherited;
end;

function TCnThreadPool.AddRequest(aDataObject: TCnTaskDataObject;
  CheckDuplicate: TCheckDuplicates): Boolean;
var
  i: Integer;
begin
{$IFDEF DEBUG}
  Trace('AddRequest:' + aDataObject.Info);
{$ENDIF DEBUG}

  Result := False;

  csQueueManagment.Enter;
  try
    if cdQueue in CheckDuplicate then
      for i := 0 to FQueue.Count - 1 do
        if (FQueue[i] <> nil) and
          aDataObject.Duplicate(TCnTaskDataObject(FQueue[i]), False) then
        begin
{$IFDEF DEBUG}
          Trace('Duplicate:' + TCnTaskDataObject(FQueue[i]).Info);
{$ENDIF DEBUG}

          FreeAndNil(aDataObject);
          Exit
        end;

    csThreadManagment.Enter;
    try
      IncreaseThreads;

      if cdProcessing in CheckDuplicate then
        for i := 0 to FThreads.Count - 1 do
          if TCnPoolingThread(FThreads[i]).Duplicate(aDataObject) then
          begin
{$IFDEF DEBUG}
            Trace('Duplicate:' + TCnPoolingThread(FThreads[i]).FProcessingDataObject.Info);
{$ENDIF DEBUG}

            FreeAndNil(aDataObject);
            Exit
          end
    finally
      csThreadManagment.Leave;
    end;

    FQueue.Add(aDataObject);
    Inc(FTaskCount);
    ReleaseSemaphore(hSemRequestCount, 1, nil);
{$IFDEF DEBUG}
    Trace('ReleaseSemaphore');
{$ENDIF DEBUG}

    Result := True;

  finally
    csQueueManagment.Leave;
  end;

{$IFDEF DEBUG}
  Trace('Added Request:' + aDataObject.Info);
{$ENDIF DEBUG}
end;

procedure TCnThreadPool.AddRequests(
  aDataObjects: array of TCnTaskDataObject;
  CheckDuplicate: TCheckDuplicates);
var
  i: Integer;
begin
  for i := 0 to Length(aDataObjects) - 1 do
    AddRequest(aDataObjects[i], CheckDuplicate)
end;

procedure TCnThreadPool.CheckTaskEmpty;
var
  i: Integer;
begin
  csQueueManagment.Enter;
  try
    if (FLastGetPoint < FQueue.Count) then
      Exit;

    csThreadManagment.Enter;
    try
      for i := 0 to FThreads.Count - 1 do
        if TCnPoolingThread(FThreads[i]).FCurState in [ctsProcessing] then
          Exit
    finally
      csThreadManagment.Leave
    end;

    DoQueueEmpty(ekTaskEmpty)

  finally
    csQueueManagment.Leave
  end
end;

procedure TCnThreadPool.DecreaseThreads;
var
  i: Integer;
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.DecreaseThreads');
{$ENDIF DEBUG}

  if csThreadManagment.TryEnter then
  try
    KillDeadThreads;
    FreeFinishedThreads;

    for i := FThreads.Count - 1 downto FThreadsMinCount do
      if TCnPoolingThread(FThreads[i]).IsIdle then
      begin
        TCnPoolingThread(FThreads[i]).Terminate(False);
        FThreadsKilling.Add(FThreads[i]);
        FThreads.Delete(i);
        Break
      end
  finally
    csThreadManagment.Leave
  end
end;

procedure TCnThreadPool.DefaultGetInfo(Sender: TCnThreadPool;
  var InfoText: string);
var
  i: Integer;
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
    for i := 0 to ThreadCount - 1 do
      InfoText := InfoText + #13#10 + ThreadInfo(i);
    InfoText := InfoText + #13#10 + {sLine +} 'Killing Threads Info' + sLine;
    for i := 0 to ThreadKillingCount - 1 do
      InfoText := InfoText + #13#10 + ThreadKillingInfo(i)
  end
end;

procedure TCnThreadPool.DoProcessRequest(aDataObj: TCnTaskDataObject;
  aThread: TCnPoolingThread);
begin
  if Assigned(FProcessRequest) then
    FProcessRequest(Self, aDataObj, aThread)
end;

procedure TCnThreadPool.DoQueueEmpty(EmptyKind: TEmptyKind);
begin
  if Assigned(FQueueEmpty) then
    FQueueEmpty(Self, EmptyKind)
end;

procedure TCnThreadPool.DoThreadFinalizing(aThread: TCnPoolingThread);
begin
  if Assigned(FThreadFinalizing) then
    FThreadFinalizing(Self, aThread)
end;

procedure TCnThreadPool.DoThreadInitializing(aThread: TCnPoolingThread);
begin
  if Assigned(FThreadInitializing) then
    FThreadInitializing(Self, aThread)
end;

procedure TCnThreadPool.FreeFinishedThreads;
var
  i: Integer;
begin
  if csThreadManagment.TryEnter then
  try
    for i := FThreadsKilling.Count - 1 downto 0 do
      if TCnPoolingThread(FThreadsKilling[i]).IsFinished then
      begin
        TCnPoolingThread(FThreadsKilling[i]).Free;
        FThreadsKilling.Delete(i)
      end

  finally
    csThreadManagment.Leave
  end
end;

procedure TCnThreadPool.GetRequest(var Request: TCnTaskDataObject);
begin
{$IFDEF DEBUG}
  Trace('TCnThreadPool.GetRequest');
{$ENDIF DEBUG}

  csQueueManagment.Enter;
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
      FLastGetPoint := 0
    end

  finally
    csQueueManagment.Leave
  end
end;

function TCnThreadPool.HasSpareThread: Boolean;
begin
  Result := FIdleThreadCount > 0
end;

function TCnThreadPool.HasTask: Boolean;
begin
  Result := FTaskCount > 0
end;

function TCnThreadPool.FinishedThreadsAreFull: Boolean;
begin
  csThreadManagment.Enter;
  try
    if FThreadsMaxCount > 0 then
      Result := FThreadsKilling.Count >= FThreadsMaxCount div 2
    else
      Result := FThreadsKilling.Count >= 50;
  finally
    csThreadManagment.Leave
  end
end;

procedure TCnThreadPool.IncreaseThreads;
var
  iAvgWait, iAvgProc: Integer;
  i: Integer;
begin
  csThreadManagment.Enter;
  try
    KillDeadThreads;
    FreeFinishedThreads;

    if FThreads.Count = 0 then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count = 0');
{$ENDIF DEBUG}

      try
        FThreads.Add(FThreadClass.Create(Self));
      except
{$IFDEF DEBUG}
        on E: Exception do
          Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF DEBUG}
      end
    end
    else if FMinAtLeast and (FThreads.Count < FThreadsMinCount) then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count < FThreadsMinCount');
{$ENDIF DEBUG}

      for i := FThreads.Count to FThreadsMinCount - 1 do
      try
        FThreads.Add(FThreadClass.Create(Self));
      except
{$IFDEF DEBUG}
        on E: Exception do
          Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF DEBUG}
      end
    end
    else if (FThreads.Count < FThreadsMaxCount) and HasTask and not HasSpareThread then
    begin
{$IFDEF DEBUG}
      Trace('IncreaseThreads: FThreads.Count < FThreadsMaxCount');
{$ENDIF DEBUG}
      i := TaskCount;
      if i <= 0 then
        Exit;

      iAvgWait := Max(AverageWaitingTime, 1);
      if iAvgWait > 100 then
        Exit;

      iAvgProc := Max(AverageProcessingTime, 2);
{$IFDEF DEBUG}
      Trace(Format(
        'ThreadCount(%D);ThreadsMaxCount(%D);AvgWait(%D);AvgProc(%D);TaskCount(%D);Killing(%D)',
        [FThreads.Count, FThreadsMaxCount, iAvgWait, iAvgProc, i, ThreadKillingCount]));
{$ENDIF DEBUG}

      //if i * iAvgWait * 2 > iAvgProc * FThreads.Count then
      if ((iAvgProc + iAvgWait) * i > iAvgProc * FThreads.Count) then
      begin
        try
          FThreads.Add(FThreadClass.Create(Self));
        except
{$IFDEF DEBUG}
          on E: Exception do
            Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
{$ENDIF DEBUG}
        end
      end
    end

  finally
    csThreadManagment.Leave
  end
end;

function TCnThreadPool.Info: string;
begin
  if csThreadManagment.TryEnter then
  begin
    try
      if Assigned(FGetInfo) then
      begin
        FGetInfo(Self, Result)
      end
      else
        DefaultGetInfo(Self, Result)
    finally
      csThreadManagment.Leave
    end;
  end
  else
  begin
    Result := 'Too busy to get info.';
  end;
end;

procedure TCnThreadPool.KillDeadThreads;
var
  i, iLen: Integer;
  LThread: TCnPoolingThread;
  LObjects: array of TCnTaskDataObject;
begin
  if FinishedThreadsAreFull then Exit;

  iLen := 0;
  SetLength(LObjects, iLen);
  if csThreadManagment.TryEnter then
  try
    for i := FThreads.Count - 1 downto 0 do
    begin
      LThread := TCnPoolingThread(FThreads[i]);
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
        FThreads.Delete(i);

//        else
//        try
//          FThreads.Add(FThreadClass.Create(Self));
//        except
//{$IFDEF DEBUG}
//          on E: Exception do
//            Trace('New thread Exception on ' + E.ClassName + ': ' + E.Message)
//{$ENDIF DEBUG}
//        end
      end
    end
  finally
    csThreadManagment.Leave
  end;
  AddRequests(LObjects, []);
end;

function TCnThreadPool.OSIsWin9x: Boolean;
begin
  Result := FOsIsWin9x;
end;

function TCnThreadPool.AverageProcessingTime: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FThreads.Count > 0 then
  begin
    for i := 0 to FThreads.Count - 1 do
      Inc(Result, TCnPoolingThread(FThreads[i]).AverageProcessingTime);
    Result := Result div FThreads.Count
  end
  else
    Result := 20
end;

function TCnThreadPool.AverageWaitingTime: Integer;
var
  i: Integer;
begin
  Result := 0;
  if FThreads.Count > 0 then
  begin
    for i := 0 to FThreads.Count - 1 do
      Inc(Result, TCnPoolingThread(FThreads[i]).AverageWaitingTime);
    Result := Result div FThreads.Count
  end
  else
    Result := 10
end;

procedure TCnThreadPool.RemoveRequest(aDataObject: TCnTaskDataObject);
begin
  csQueueManagment.Enter;
  try
    FQueue.Remove(aDataObject);
    Dec(FTaskCount);
    FreeAndNil(aDataObject)
  finally
    csQueueManagment.Leave
  end
end;

procedure TCnThreadPool.SetAdjustInterval(const Value: DWORD);
var
  DueTo: Int64;
begin
  FAdjustInterval := Value;
  if hTimReduce <> 0 then
    SetWaitableTimer(hTimReduce, DueTo, Value, nil, nil, False)
end;

function TCnThreadPool.TaskCount: Integer;
begin
  Result := FTaskCount;
end;

function TCnThreadPool.ThreadCount: Integer;
begin
  if csThreadManagment.TryEnter then
  try
    Result := FThreads.Count
  finally
    csThreadManagment.Leave
  end
  else
    Result := -1
end;

function TCnThreadPool.ThreadInfo(const i: Integer): string;
begin
  Result := '';

  if csThreadManagment.TryEnter then
  try
    if i < FThreads.Count then
      Result := TCnPoolingThread(FThreads[i]).Info
  finally
    csThreadManagment.Leave
  end
end;

function TCnThreadPool.ThreadKillingCount: Integer;
begin
  if csThreadManagment.TryEnter then
  try
    Result := FThreadsKilling.Count
  finally
    csThreadManagment.Leave
  end
  else
    Result := -1
end;

function TCnThreadPool.ThreadKillingInfo(const i: Integer): string;
begin
  Result := '';

  if csThreadManagment.TryEnter then
  try
    if i < FThreadsKilling.Count then
      Result := TCnPoolingThread(FThreadsKilling[i]).Info
  finally
    csThreadManagment.Leave;
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
{$ENDIF DEBUG}

var
  V: TOSVersionInfo;
initialization
  V.dwOSVersionInfoSize := SizeOf(V);
  FOSIsWin9x := GetVersionEx(V) and
    (V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);
    
{$IFDEF DEBUG}
  TraceLog := SimpleTrace;
{$ENDIF DEBUG}

finalization

end.

