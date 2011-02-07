{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

unit CnCodeDemo;
{* |<PRE>
================================================================================
* 软件名称：CnPack组件包
* 单元名称：组件代码规范示例单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: Delphi单元规范格式.pas,v 1.8 2009/02/25 12:32:57 liuxiao Exp $
* 备    注：- 此单元仅仅只作为 CnPack 的代码规范示例单元，供阅读与对比用，不参与
*             实际的编译与调试。
*           - 本例中的TCnTimer采用单独的线程进行定时控制，精度比TTimer要高，相应
*             地也占用较多的CPU资源。
* 修改记录：2009.02.18 V1.1
*               更改单元说明
*           2002.04.18 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, SysUtils, ExtCtrls, CnClasses, CnConsts, CnCompConsts;

type

//==============================================================================
// 高精度定时器组件定时线程
//==============================================================================

{ TCnTimerThread }

  TCnTimer = class;

  TCnTimerThread = class(TThread)
  private
    FOwner: TCnTimer;
    FInterval: Word;
    FStop: THandle;
  protected
    constructor Create(CreateSuspended: Boolean); virtual;
    procedure Execute; override;
  end;

//==============================================================================
// 高精度定时器组件
//==============================================================================

{ TCnTimer }

  TTimerQuality = (tqHighest, tqHigh, tqLow);
  {* 高精度定时器定时精度类型
   |<PRE>
     tqHighest  - 最高精度，采用高优先级的线程定时
     tqHigh     - 高精度，采用普通优先级的线程定时
     tqLow      - 低精度，内部使用TTimer进行定时
   |</PRE>}

  TCnTimer = class(TCnComponent)
  {* 高精度定时器组件，使用单独的线程进行定时控制，使用方法与TTimer一样，
     仅增加了一个Quality属性控制定时精度}
  private
    FOnTimer: TNotifyEvent;
    FQuality: TTimerQuality;
    FEnabled: Boolean;
    FInterval: Word;
    FTimerThread: TCnTimerThread;
    FTimer: TTimer;
    FLastTick: Cardinal;
    FLastCountTick: Cardinal;
    FActualInterval: Integer;
    FActualRate: Integer;
    FCount: Integer;
    procedure DoTimer;
    procedure OnTimerTimer(Sender: TObject);
    procedure CreateTimer;
    procedure CreateTimerThread;
    procedure FreeTimer;
    procedure FreeTimerThread;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Word);
    procedure SetQuality(const Value: TTimerQuality);
  protected
    function GetAuthor: string; override;
    function GetComment: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ActualInterval: Integer read FActualInterval;
    {* 实际的定时间隔，单位为毫秒}
    property ActualRate: Integer read FActualRate;
    {* 实际的定时速度，单位为次每秒}
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    {* 是否允许定时事件}
    property Interval: Word read FInterval write SetInterval default 1000;
    {* 定时间隔，单位为毫秒}
    property Quality: TTimerQuality read FQuality write SetQuality default tqLow;
    {* 定时精度，如果Interval小于55（Win9X）或10（WinNT），建议设为高精度以上}
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    {* 定时器事件}
  end;

implementation

//==============================================================================
// 高精度定时器组件定时线程
//==============================================================================

{ TCnTimerThread }

// 初始化线程
constructor TCnTimerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FStop := CreateEvent(nil, False, False, nil); // 创建退出用事件
end;

// 线程主体
procedure TCnTimerThread.Execute;
begin
  repeat                      // 等待退出事件置位或 FInterval 毫秒后超时退出
    if WaitForSingleObject(FStop, FInterval) = WAIT_TIMEOUT then
      Synchronize(FOwner.DoTimer); // 同步方式产生定时事件
  until Terminated;
  CloseHandle(FStop);         // 释放事件句柄
end;

{ TCnTimer }

//==============================================================================
// 高精度定时器组件
//==============================================================================

// 组件初始化
constructor TCnTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := False;
  FInterval := 1000;
  FQuality := tqLow;
  FTimer := nil;
  FTimerThread := nil;
  CreateTimer;
end;

// 释放
destructor TCnTimer.Destroy;
begin
  FreeTimer;
  FreeTimerThread;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
// 事件产生
//------------------------------------------------------------------------------

// 产生定时事件
procedure TCnTimer.DoTimer;
var
  Tick: Cardinal;
begin
  Tick := GetTickCount;
  if (FLastTick = 0) and (FLastCountTick = 0) then
  begin
    FLastTick := Tick;
    FLastCountTick := Tick;
  end
  else
  begin
    FActualInterval := Tick - FLastTick;
    FLastTick := Tick;
    if Tick - FLastCountTick >= 1000 then
    begin
      FActualRate := FCount;
      FLastCountTick := Tick;
      FCount := 0;
    end else
      Inc(FCount);
  end;
  begin
    if Assigned(FOnTimer) then
      FOnTimer(Self);
  end;
end;

//------------------------------------------------------------------------------
// 内部定时器创建释放
//------------------------------------------------------------------------------

// 内部Timer事件
procedure TCnTimer.OnTimerTimer(Sender: TObject);
begin
  DoTimer;
end;

// 创建内部Timer定时器（低精度）
procedure TCnTimer.CreateTimer;
begin
  if not Assigned(FTimer) then
  begin
    FTimer := TTimer.Create(Self);
    FTimer.OnTimer := OnTimerTimer;
    FTimer.Interval := FInterval;
    FTimer.Enabled := FEnabled;
  end;
end;

// 创建定时器线程（高精度）
procedure TCnTimer.CreateTimerThread;
begin
  if not Assigned(FTimerThread) then
  begin
    FTimerThread := TCnTimerThread.Create(True);
    FTimerThread.FOwner := Self;
    FTimerThread.FreeOnTerminate := False;
    FTimerThread.Priority := tpNormal;
    FTimerThread.FInterval := FInterval;
    if FEnabled then
    begin
      if FInterval > 0 then
      begin
        SetEvent(FTimerThread.FStop);
        FTimerThread.Resume;
      end;
    end
    else
      FTimerThread.Suspend;
  end;
end;

// 释放内部定时器（低精度）
procedure TCnTimer.FreeTimer;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer := nil;
  end;
end;

// 释放定时器线程（高精度）
procedure TCnTimer.FreeTimerThread;
begin
  if Assigned(FTimerThread) then
  begin
    FTimerThread.Terminate;
    SetEvent(FTimerThread.FStop);
    if FTimerThread.Suspended then FTimerThread.Resume;
    FTimerThread.WaitFor;
    FTimerThread.Free;
    FTimerThread := nil;
  end;
end;

//------------------------------------------------------------------------------
// 属性读写方法
//------------------------------------------------------------------------------

// 设置定时精度
procedure TCnTimer.SetQuality(const Value: TTimerQuality);
begin
  if FQuality <> Value then
  begin
    FQuality := Value;
    case FQuality of
      tqHighest, tqHigh:
        begin
          FreeTimer;
          CreateTimerThread;
          if Value = tqHighest then
            FTimerThread.Priority := tpHigher
          else
            FTimerThread.Priority := tpNormal;
        end;
      tqLow:
        begin
          FreeTimerThread;
          CreateTimer;
        end;
    end;
  end;
end;

// 设置是否允许定时
procedure TCnTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FQuality = tqLow then
      FTimer.Enabled := FEnabled
    else
    begin
      if FEnabled then
      begin
        if FTimerThread.FInterval > 0 then
        begin
          SetEvent(FTimerThread.FStop);
          FTimerThread.Resume;
        end;
      end
      else
        FTimerThread.Suspend;
    end;
  end;
end;

// 设置定时间隔
procedure TCnTimer.SetInterval(Value: Word);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    Enabled := False;
    if FQuality = tqLow then
      FTimer.Interval := FInterval
    else
      FTimerThread.FInterval := FInterval;
    Enabled := True;
  end;
end;

// 取组件作者
function TCnTimer.GetAuthor: string;
begin
  Result := SCnPack_Zjy;
end;

// 取组件注释
function TCnTimer.GetComment: string;
begin
  Result := SCnTimerComment;
end;

end.

