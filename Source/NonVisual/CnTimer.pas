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

unit CnTimer;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：高精度定时器组件TCnTimer单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：- Delphi自带的TTimer使用操作系统以消息方式提供的定时器，在Win9X下
*             定时精度仅为55ms，NT下约10ms。
*           - TCnTimer使用多媒体定时器进行定时控制，精度较高。其使用方式与TTimer
*             完全兼容，并提供了更多的功能。
*           - TCnTimerList定时器列表可以同时产生多个定时器。
*           - 所有定时器使用同一个内部定时器，适合大量使用的场合。
*           - 由于Win32是抢占式多任务操作系统，各个线程轮流享用CPU时间片，如果
*             其它的线程占用大量CPU时间，即使设置最高精度，也不一定能保证精确
*             的定时间隔。
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.12.22 V2.2
*               增加同步事件属性 SyncEvent 并默认为真，使定时器事件可以在主线程中执行
*           2006.12.28 V2.1
*               去掉定时线程，改用多媒体定时器，以减少资源占用并解决在 DLL 中不能
*               使用的问题
*           2002.11.05 V2.0
*               重写全部代码，增加定时器列表，所有定时器使用同一线程定时
*           2002.04.18 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Forms, MMSystem, CnClasses, CnConsts, CnCompConsts,
  CnNativeDecl;

type

//==============================================================================
// 高精度定时器对象
//==============================================================================

{ TCnTimerObject }

  TCnTimerObject = class(TObject)
  private
    FActualFPS: Double;
    FEnabled: Boolean;
    FExecCount: Cardinal;
    FInterval: Cardinal;
    FLastTickCount: Cardinal;
    FOnTimer: TNotifyEvent;
    FRepeatCount: Cardinal;
    FSyncEvent: Boolean;
    function GetFPS: Double;
    procedure SetEnabled(Value: Boolean);
    procedure SetFPS(Value: Double);
    procedure SetInterval(Value: Cardinal);
    procedure SetRepeatCount(Value: Cardinal);
  protected
    procedure Timer; dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    property ActualFPS: Double read FActualFPS;
    property ExecCount: Cardinal read FExecCount;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FPS: Double read GetFPS write SetFPS stored False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property RepeatCount: Cardinal read FRepeatCount write SetRepeatCount default 0;
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent default True;
  end;

//==============================================================================
// 高精度定时器组件
//==============================================================================

{ TCnTimer }

  TCnTimer = class(TCnComponent)
  {* 高精度定时器组件，使用方法类似 TTimer。}
  private
    FTimerObject: TCnTimerObject;
    function GetActualFPS: Double;
    function GetEnabled: Boolean;
    function GetExecCount: Cardinal;
    function GetFPS: Double;
    function GetInterval: Cardinal;
    function GetOnTimer: TNotifyEvent;
    function GetRepeatCount: Cardinal;
    function GetSyncEvent: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetFPS(Value: Double);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure SetRepeatCount(Value: Cardinal);
    procedure SetSyncEvent(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    property ActualFPS: Double read GetActualFPS;
    {* 实际的定时器速率，次每秒}
    property ExecCount: Cardinal read GetExecCount;
    {* 已经执行过的次数}
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    {* 定时器是否启用}
    property FPS: Double read GetFPS write SetFPS stored False;
    {* 定时器速度，次每秒}
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    {* 定时间隔，毫秒}
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    {* 定时事件}
    property RepeatCount: Cardinal read GetRepeatCount write SetRepeatCount default 0;
    {* 定时事件次数，当定时事件发生指定次数后自动关闭。如果为 0 表示不限制}
    property SyncEvent: Boolean read GetSyncEvent write SetSyncEvent default True;
    {* 定时事件是否用同步方式在主线程中产生，如果为 False 则定时事件在多媒体定时器线程中调用。
       定时事件中如果涉及到 VCL 界面操作，应设为 True。}
  end;

//==============================================================================
// 高精度定时器列表集合子项
//==============================================================================

{ TCnTimerItem }

  TCnTimerItem = class(TCollectionItem)
  {* 高精度定时器列表子项，使用方法类似 TTimer。}
  private
    FOnTimer: TNotifyEvent;
    FTimerObject: TCnTimerObject;
    function GetActualFPS: Double;
    function GetEnabled: Boolean;
    function GetExecCount: Cardinal;
    function GetFPS: Double;
    function GetInterval: Cardinal;
    function GetRepeatCount: Cardinal;
    procedure SetEnabled(Value: Boolean);
    procedure SetFPS(Value: Double);
    procedure SetInterval(Value: Cardinal);
    procedure SetRepeatCount(Value: Cardinal);
    function GetSyncEvent: Boolean;
    procedure SetSyncEvent(const Value: Boolean);
  protected
    procedure Timer(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 赋值方法}
    property ActualFPS: Double read GetActualFPS;
    {* 实际的定时器速率，次每秒}
    property ExecCount: Cardinal read GetExecCount;
    {* 已经执行过的次数}
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    {* 定时器是否启用}
    property FPS: Double read GetFPS write SetFPS stored False;
    {* 定时器速度，次每秒}
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    {* 定时间隔，毫秒}
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    {* 定时事件}
    property RepeatCount: Cardinal read GetRepeatCount write SetRepeatCount default 0;
    {* 定时事件次数，当定时事件发生指定次数后自动关闭。如果为 0 表示不限制}
    property SyncEvent: Boolean read GetSyncEvent write SetSyncEvent default True;
    {* 定时事件是否用同步方式在主线程中产生，如果为 False 则定时事件在多媒体定时器线程中调用。
       定时事件中如果涉及到 VCL 界面操作，应设为 True。}
  end;

//==============================================================================
// 高精度定时器列表集合类
//==============================================================================

{ TCnTimerCollection }

  TCnTimerList = class;

  TCnTimerCollection = class(TOwnedCollection)
  {* 高精度定时器列表集合}
  private
    FTimerList: TCnTimerList;
    function GetItems(Index: Integer): TCnTimerItem;
    procedure SetItems(Index: Integer; Value: TCnTimerItem);
  protected
    property TimerList: TCnTimerList read FTimerList;
  public
    constructor Create(AOwner: TPersistent);
    {* 类构造器}
    property Items[Index: Integer]: TCnTimerItem read GetItems write SetItems; default;
    {* 定时器数组属性}
  end;

//==============================================================================
// 高精度定时器列表组件
//==============================================================================

{ TCnTimerList }

  TCnTimerEvent = procedure(Sender: TObject; Index: Integer; var Handled:
    Boolean) of object;
  {* 高精度定时器列表事件。Index 为产生事件的定时器子项序号，Handle 返回是否已处理，
     如果在事件中将 Handle 置为 true，将不产生该定时器子项事件}
    
  TCnTimerList = class(TCnComponent)
  {* 高精度定时器列表组件，可以定义多个定时器。}
  private
    FItems: TCnTimerCollection;
    FOnTimer: TCnTimerEvent;
    procedure SetItems(Value: TCnTimerCollection);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
    function Timer(Index: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
  published
    property Items: TCnTimerCollection read FItems write SetItems;
    {* 定时器列表}
    property OnTimer: TCnTimerEvent read FOnTimer write FOnTimer;
    {* 定时器事件}
  end;

implementation

uses
  Messages;

const
  UM_CNTIMER = WM_USER + 101;

type

//==============================================================================
// 高精度定时器管理器（私有类）
//==============================================================================

{ TCnTimerMgr }

  TCnTimerMgr = class(TObject)
  private
    FTimerList: TThreadList;
    FTimerRes: Integer;
    FTimerID: Integer;
    FHwnd: HWND;
    function InitMMTimer: Boolean;
    procedure FreeMMTimer;
  protected
    procedure ClearTimer;
    procedure DoTimer(Sync: Boolean);
    procedure Timer; virtual;
    procedure WndProc(var Message: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    function AddTimer: TCnTimerObject;
    procedure DeleteTimer(TimerObject: TCnTimerObject);
  end;

//==============================================================================
// 高精度定时器管理器（私有类）
//==============================================================================

{ TCnTimerMgr }

constructor TCnTimerMgr.Create;
begin
  inherited Create;
  FTimerList := TThreadList.Create;
  FHwnd := AllocateHWnd(WndProc);
  InitMMTimer;
end;

destructor TCnTimerMgr.Destroy;
begin
  DeallocateHWnd(FHwnd);
  FreeMMTimer;
  ClearTimer;
  FreeAndNil(FTimerList);
  inherited Destroy;
end;

procedure MMTimerProc(uTimerID, uMessage: UINT; dwUser, dw1, dw2: TCnNativePointer) stdcall;
begin
  TCnTimerMgr(dwUser).Timer;
end;  

function TCnTimerMgr.InitMMTimer: Boolean;
var
  tc: TIMECAPS;
begin
  Result := False;
  if timeGetDevCaps(@tc, SizeOf(TIMECAPS)) = TIMERR_NOERROR then
  begin
    FTimerRes := tc.wPeriodMin;
    if timeBeginPeriod(FTimerRes) = TIMERR_NOERROR then
    begin
      FTimerID := timeSetEvent(tc.wPeriodMin, 0, MMTimerProc, Cardinal(Self),
        TIME_PERIODIC);
      Result := FTimerID <> 0;
    end
    else
      FTimerRes := 0;
  end;
end;

procedure TCnTimerMgr.FreeMMTimer;
begin
  if FTimerID <> 0 then
  begin
    timeKillEvent(FTimerID);
  end;
  
  if FTimerRes <> 0 then
  begin
    timeEndPeriod(FTimerRes);
  end;   
end;

function TCnTimerMgr.AddTimer: TCnTimerObject;
begin
  Result := TCnTimerObject.Create;
  with FTimerList.LockList do
  try
    Add(Result);
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.ClearTimer;
var
  i: Integer;
begin
  with FTimerList.LockList do
  try
    for i := Count - 1 downto 0 do
    begin
      TCnTimerObject(Items[i]).Free;
      Delete(i);
    end;
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.DeleteTimer(TimerObject: TCnTimerObject);
var
  i: Integer;
begin
  with FTimerList.LockList do
  try
    for i := 0 to Count - 1 do
      if Items[i] = TimerObject then
      begin
        TimerObject.Free;
        Delete(i);
        Exit;
      end;
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.DoTimer(Sync: Boolean);
var
  i: Integer;
  CurrTick: Cardinal;
begin
  with FTimerList.LockList do
  try
    CurrTick := timeGetTime;
    for i := 0 to Count - 1 do
      with TCnTimerObject(Items[i]) do
        if Enabled and (FSyncEvent = Sync) and(Interval <> 0) and
          (CurrTick - FLastTickCount >= Interval) and Assigned(FOnTimer) then
        begin
          if CurrTick <> FLastTickCount then
            FActualFPS := 1000 / (CurrTick - FLastTickCount)
          else
            FActualFPS := 0;
          FLastTickCount := CurrTick;
          try
            Timer;
          except
            Application.HandleException(Self);
          end;
        end;
  finally
    FTimerList.UnlockList;
  end;
end;

procedure TCnTimerMgr.Timer;
begin
  DoTimer(False);
  PostMessage(FHwnd, UM_CNTIMER, 0, 0);
end;

procedure TCnTimerMgr.WndProc(var Message: TMessage);
begin
  if Message.Msg = UM_CNTIMER then
  begin
    DoTimer(True);
  end;  
end;

var
  TimerMgr: TCnTimerMgr;

function GetTimerMgr: TCnTimerMgr;
begin
  if TimerMgr = nil then
    TimerMgr := TCnTimerMgr.Create;
  Result := TimerMgr;
end;

//==============================================================================
// 高精度定时器对象
//==============================================================================

{ TCnTimerObject }

constructor TCnTimerObject.Create;
begin
  inherited Create;
  FEnabled := True;
  FExecCount := 0;
  FInterval := 1000;
  FLastTickCount := timeGetTime;
  FRepeatCount := 0;
  FSyncEvent := True;
end;

destructor TCnTimerObject.Destroy;
begin

  inherited;
end;

function TCnTimerObject.GetFPS: Double;
begin
  if Interval = 0 then
    Result := 0
  else
    Result := 1000 / Interval;
end;

procedure TCnTimerObject.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    FExecCount := 0;
    if FEnabled then
    begin
      FLastTickCount := timeGetTime;
    end;
  end;
end;

procedure TCnTimerObject.SetFPS(Value: Double);
begin
  if Value < 0 then
    Exit
  else if Value < 1 / High(Word) then
    Value := 1 / High(Word)
  else if Value > 1000 then
    Value := 1000;
  FInterval := Round(1000 / Value);
end;

procedure TCnTimerObject.SetInterval(Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    FLastTickCount := timeGetTime;
  end;
end;

procedure TCnTimerObject.SetRepeatCount(Value: Cardinal);
begin
  if FRepeatCount <> Value then
  begin
    FRepeatCount := Value;
  end;
end;

procedure TCnTimerObject.Timer;
begin
  Inc(FExecCount);
  if Assigned(FOnTimer) then FOnTimer(Self);
  if (RepeatCount <> 0) and (FExecCount >= RepeatCount) then
  begin
    Enabled := False;
  end;
end;

//==============================================================================
// 高精度定时器组件
//==============================================================================

{ TCnTimer }

constructor TCnTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimerObject := GetTimerMgr.AddTimer;
end;

destructor TCnTimer.Destroy;
begin
  if TimerMgr <> nil then
    TimerMgr.DeleteTimer(FTimerObject);
  inherited Destroy;
end;

function TCnTimer.GetActualFPS: Double;
begin
  Result := FTimerObject.ActualFPS;
end;

procedure TCnTimer.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnTimerName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnTimerComment;
end;

function TCnTimer.GetEnabled: Boolean;
begin
  Result := FTimerObject.Enabled;
end;

function TCnTimer.GetExecCount: Cardinal;
begin
  Result := FTimerObject.ExecCount;
end;

function TCnTimer.GetFPS: Double;
begin
  Result := FTimerObject.FPS;
end;

function TCnTimer.GetInterval: Cardinal;
begin
  Result := FTimerObject.Interval;
end;

function TCnTimer.GetOnTimer: TNotifyEvent;
begin
  Result := FTimerObject.OnTimer;
end;

function TCnTimer.GetRepeatCount: Cardinal;
begin
  Result := FTimerObject.RepeatCount;
end;

function TCnTimer.GetSyncEvent: Boolean;
begin
  Result := FTimerObject.SyncEvent;
end;

procedure TCnTimer.SetEnabled(Value: Boolean);
begin
  FTimerObject.Enabled := Value;
end;

procedure TCnTimer.SetFPS(Value: Double);
begin
  FTimerObject.FPS := Value;
end;

procedure TCnTimer.SetInterval(Value: Cardinal);
begin
  FTimerObject.Interval := Value;
end;

procedure TCnTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FTimerObject.OnTimer := Value;
end;

procedure TCnTimer.SetRepeatCount(Value: Cardinal);
begin
  FTimerObject.RepeatCount := Value;
end;

procedure TCnTimer.SetSyncEvent(const Value: Boolean);
begin
  FTimerObject.SyncEvent := Value;
end;

//==============================================================================
// 高精度定时器列表集合子项
//==============================================================================

{ TCnTimerItem }

constructor TCnTimerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FTimerObject := GetTimerMgr.AddTimer;
  FTimerObject.OnTimer := Timer;
end;

destructor TCnTimerItem.Destroy;
begin
  if TimerMgr <> nil then
    TimerMgr.DeleteTimer(FTimerObject);
  inherited Destroy;
end;

procedure TCnTimerItem.Assign(Source: TPersistent);
begin
  if Source is TCnTimerItem then
  begin
    Enabled := TCnTimerItem(Source).Enabled;
    Interval := TCnTimerItem(Source).Interval;
    RepeatCount := TCnTimerItem(Source).RepeatCount;
  end
  else
    inherited;
end;

function TCnTimerItem.GetActualFPS: Double;
begin
  Result := FTimerObject.ActualFPS;
end;

function TCnTimerItem.GetEnabled: Boolean;
begin
  Result := FTimerObject.Enabled;
end;

function TCnTimerItem.GetExecCount: Cardinal;
begin
  Result := FTimerObject.ExecCount;
end;

function TCnTimerItem.GetFPS: Double;
begin
  Result := FTimerObject.FPS;
end;

function TCnTimerItem.GetInterval: Cardinal;
begin
  Result := FTimerObject.Interval;
end;

function TCnTimerItem.GetRepeatCount: Cardinal;
begin
  Result := FTimerObject.RepeatCount;
end;

function TCnTimerItem.GetSyncEvent: Boolean;
begin
  Result := FTimerObject.SyncEvent;
end;

procedure TCnTimerItem.SetEnabled(Value: Boolean);
begin
  FTimerObject.Enabled := Value;
end;

procedure TCnTimerItem.SetFPS(Value: Double);
begin
  FTimerObject.FPS := Value;
end;

procedure TCnTimerItem.SetInterval(Value: Cardinal);
begin
  FTimerObject.Interval := Value;
end;

procedure TCnTimerItem.SetRepeatCount(Value: Cardinal);
begin
  FTimerObject.RepeatCount := Value;
end;

procedure TCnTimerItem.SetSyncEvent(const Value: Boolean);
begin
  FTimerObject.SyncEvent := Value;
end;

procedure TCnTimerItem.Timer(Sender: TObject);
begin
  if not TCnTimerList(TCnTimerCollection(Collection).GetOwner).Timer(Index) then
    if Assigned(FOnTimer) then
      FOnTimer(Self);
end;

//==============================================================================
// 高精度定时器列表集合类
//==============================================================================

{ TCnTimerCollection }

constructor TCnTimerCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnTimerItem);
  Assert(AOwner is TCnTimerList);
end;

function TCnTimerCollection.GetItems(Index: Integer): TCnTimerItem;
begin
  Result := TCnTimerItem(inherited Items[Index]);
end;

procedure TCnTimerCollection.SetItems(Index: Integer; Value: TCnTimerItem);
begin
  inherited Items[Index] := Value;
end;

//==============================================================================
// 高精度定时器列表组件
//==============================================================================

{ TCnTimerList }

constructor TCnTimerList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TCnTimerCollection.Create(Self);
end;

destructor TCnTimerList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TCnTimerList.GetComponentInfo(var AName, Author, Email, Comment:
  string);
begin
  AName := SCnTimerListName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnTimerListComment;
end;

procedure TCnTimerList.SetItems(Value: TCnTimerCollection);
begin
  FItems.Assign(Value);
end;

function TCnTimerList.Timer(Index: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnTimer) then
    FOnTimer(Self, Index, Result);
end;

initialization

finalization
  if TimerMgr <> nil then
    FreeAndNil(TimerMgr);

end.

