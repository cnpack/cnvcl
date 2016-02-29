{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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

unit CnDHibernateThread; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：线程控件单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备   注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.08.23 V1.8
*          移植到 Delphi2009
*        2006.09.04 V1.0
*          创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, CnDHibernateBase;

type
  TCnNotifyEventParams = procedure(Sender: TObject; params: ICnMap) of object;

  TCnDHibernateThread = class(TComponent)
  private
    FThreadCount: Integer;
    FExclusif: Boolean;
    FRunOnCreate: Boolean;
    FOnbegin: TNotifyEvent;
    FOnExecute: TCnNotifyEventParams;
    FOnFinish: TNotifyEvent;
    FOnFinishAll: TNotifyEvent;
    FFreeOnTerminate: Boolean;
    FAbout: string;
    procedure DoCreate;
    procedure DoTerminate(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    function Execute(p: ICnMap): Thandle;
    function OneThreadIsRunning: Boolean;
    function GetPriority(Thread: Thandle): TThreadPriority;
    procedure SetPriority(Thread: THandle; Priority: TThreadPriority);
    procedure QuitThread(Thread: Thandle);
    procedure Suspend(Thread: Thandle);
    procedure Resume(Thread: Thandle);

    property About: string read FAbout write FAbout;
    property Exclusif: Boolean read FExclusif write FExclusif;
    property RunOnCreate: Boolean read FRunOnCreate write FRunOnCreate;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Onbegin: TNotifyEvent read FOnbegin write FOnBegin;
    property OnExecute: TCnNotifyEventParams read FOnExecute write FOnExecute;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnFinishAll: TNotifyEvent read FOnFinishAll write FOnFinishAll;
  end;

  TCnHideThread = class(TThread)
  private
    FExecuteEvent: TCnNotifyEventParams;
    FParams: ICnMap;
  public
    constructor Create(event: TCnNotifyEventParams; params: ICnMap); virtual;
    procedure Execute; override;
  end;

procedure Synchronize(Method: TNotifyEvent);

procedure SynchronizeParams(Method: TCnNotifyEventParams; p: ICnMap);
  
{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

var
  mtx: THandle;

procedure Synchronize(Method: TNotifyEvent);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil);
  ReleaseMutex(mtx);
end;

procedure SynchronizeParams(Method: TCnNotifyEventParams; p: ICnMap);
begin
  WaitForSingleObject(mtx, INFINITE);
  Method(nil, p);
  ReleaseMutex(mtx);
end;

constructor TCnDHibernateThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount := 0;
  FRunOnCreate := true;
  FExclusif := true;
  FreeOnTerminate := true;
end;

destructor TCnDHibernateThread.Destroy;
begin
  inherited Destroy;
end;

function TCnDHibernateThread.Execute(p: ICnMap): Thandle;
var
  HideThread: TCnHideThread;
begin
  result := 0;
  if Assigned(FOnExecute) then
  begin
    if Exclusif then
      if OneThreadIsRunning then
        exit;
    inc(FThreadCount);
    HideThread := TCnHideThread.Create(FOnExecute, p);
    HideThread.FreeOnTerminate := FFreeOnTerminate;
    HideThread.OnTerminate := DoTerminate;
    DoCreate;
    if FRunOnCreate then
      HideThread.Resume;
    result := HideThread.Handle;   { HideThread.ThreadID }
  end;
end;

function TCnDHibernateThread.GetPriority(Thread: Thandle): TThreadPriority;
begin
  result := tpIdle;
  if Thread <> 0 then
    result := TThreadPriority(GetThreadPriority(thread));
end;

procedure TCnDHibernateThread.SetPriority(Thread: THandle; Priority: TThreadPriority);
begin
  SetThreadPriority(Thread, integer(priority));
end;

procedure TCnDHibernateThread.QuitThread(Thread: Thandle);
begin
  TerminateThread(Thread, 0);
end;

procedure TCnDHibernateThread.Suspend(Thread: Thandle);
begin
  SuspendThread(Thread);
end;

procedure TCnDHibernateThread.Resume(Thread: Thandle);
begin
  ResumeThread(thread);
end;

procedure TCnDHibernateThread.DoCreate;
begin
  if Assigned(FOnBegin) then
    FOnBegin(nil);
end;

procedure TCnDHibernateThread.DoTerminate;
begin
  Dec(FThreadCount);
  if Assigned(FOnFinish) then
    FOnFinish(nil);
  if FThreadCount = 0 then
    if Assigned(FOnFinishAll) then
      FOnFinishAll(nil);
end;

function TCnDHibernateThread.OneThreadIsRunning: Boolean;
begin
  Result := FThreadCount > 0;
end;

constructor TCnHideThread.Create(event: TCnNotifyEventParams; params: ICnMap);
begin
  inherited Create(true);
  FExecuteEvent := event;
  FParams := params;
end;

procedure TCnHideThread.Execute;
begin
  FExecuteEvent(nil, FParams);
end;

initialization
  mtx := CreateMutex(nil, False, 'DHibernateThreadMutex');

finalization
  CloseHandle(mtx); 
  
{$ENDIF SUPPORT_ADO}
end.
