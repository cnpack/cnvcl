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

unit CnWinSvc;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：Windows 服务封装单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 备    注：
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, WinSvc;

type
  TServiceCurrentState = (ssNotInstalled, ssUnknown, ssStopped, ssStartPending,
    ssStopPending, ssRunning, ssContinuePending, ssPausePending, ssPause);

  TCnNTService = class(TObject)
  private
    FComputerName: string;
    FLastErrorCode: DWORD;
    FLastErrorMsg: string;
    FServiceName: string;
    FSCHandle: Integer;
    FServiceHandle: Integer;
    function OpenSCHandle: Boolean;
    procedure CloseSCHandle;
    function OpenSvcHandle: Boolean;
    procedure CloseSvcHandle;
    function GetIsInstalled: Boolean;
    function GetCanStart: Boolean;
    function GetCanStop: Boolean;
    function GetCurrentState: TServiceCurrentState;
  protected
    procedure LogError(const ErrorMsg: string);
    property SCHandle: Integer read FSCHandle;
    property ServiceHandle: Integer read FServiceHandle;
  public
    constructor Create(const AServiceName: string; const AComputerName:
      string = '');
    destructor Destroy; override;
    function Install(const ServiceFileName: string; const DisplayName:
      string = ''): Boolean;
    function Uninstall: Boolean;
    function Start: Boolean;
    function Stop: Boolean;
    function GetStatus(var Value: TServiceStatus): Boolean;
    function SetStatus(var Value: TServiceStatus): Boolean;
    property ComputerName: string read FComputerName;
    property ServiceName: string read FServiceName;
    property LastErrorCode: DWORD read FLastErrorCode;
    property LastErrorMsg: string read FLastErrorMsg;
    property IsInstalled: Boolean read GetIsInstalled;
    property CanStart: Boolean read GetCanStart;
    property CanStop: Boolean read GetCanStop;
    property CurrentState: TServiceCurrentState read GetCurrentState;
  end;

implementation

{ TCnNTService }

procedure TCnNTService.CloseSCHandle;
begin
  CloseServiceHandle(FSCHandle);
end;

procedure TCnNTService.CloseSvcHandle;
begin
  CloseServiceHandle(FServiceHandle);
end;

constructor TCnNTService.Create(const AServiceName: string; const AComputerName: string);
begin
  inherited Create;
  FServiceName := AServiceName;
  FComputerName := AComputerName;
end;

destructor TCnNTService.Destroy;
begin

  inherited;
end;

function TCnNTService.GetCanStart: Boolean;
var
  Status: TServiceStatus;
begin
  if GetStatus(Status) then
    Result := Status.dwCurrentState = SERVICE_STOPPED
  else
    Result := False;
end;

function TCnNTService.GetCanStop: Boolean;
var
  Status: TServiceStatus;
begin
  if GetStatus(Status) then
    Result := Status.dwControlsAccepted and SERVICE_ACCEPT_STOP > 0
  else
    Result := False;
end;

function TCnNTService.GetCurrentState: TServiceCurrentState;
var
  Status: TServiceStatus;
begin
  if not IsInstalled then
    Result := ssNotInstalled
  else if not GetStatus(Status) then
    Result := ssUnknown
  else
  begin
    case Status.dwCurrentState of
      SERVICE_STOPPED: Result := ssStopped;
      SERVICE_START_PENDING: Result := ssStartPending;
      SERVICE_STOP_PENDING: Result := ssStopPending;
      SERVICE_RUNNING: Result := ssRunning;
      SERVICE_CONTINUE_PENDING: Result := ssContinuePending;
      SERVICE_PAUSE_PENDING: Result := ssPausePending;
      SERVICE_PAUSED: Result := ssPause;
    else
      Result := ssUnknown;
    end;
  end;
end;

function TCnNTService.GetIsInstalled: Boolean;
begin
  Result := False;
  if not OpenSCHandle then Exit;
  try
    if not OpenSvcHandle then Exit;

    Result := True;
    
    CloseSvcHandle;
  finally
    CloseSCHandle;
  end;
end;

function TCnNTService.GetStatus(var Value: TServiceStatus): Boolean;
begin
  Result := False;
  if not OpenSCHandle then Exit;
  try
    if not OpenSvcHandle then Exit;
    try
      if not QueryServiceStatus(ServiceHandle, Value) then
        LogError('GetStatus fail')
      else
        Result := True;
    finally
      CloseSvcHandle;
    end;
  finally
    CloseSCHandle;
  end;
end;

function TCnNTService.Install(const ServiceFileName,
  DisplayName: string): Boolean;
var
  SvcHandle: Integer;
begin
  Result := False;
  if not OpenSCHandle then Exit;
  try
    SvcHandle := CreateService(SCHandle, PChar(ServiceName), PChar(DisplayName),
      GENERIC_EXECUTE, SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS,
      SERVICE_AUTO_START, SERVICE_ERROR_IGNORE, PChar(ServiceFileName),
      nil, nil, nil, nil, nil);

    if (SvcHandle = 0) and (GetLastError <> ERROR_SERVICE_EXISTS) then
      LogError('CreateService fail.')
    else
      Result := True;

    CloseServiceHandle(SvcHandle);
  finally
    CloseSCHandle;
  end;
end;

procedure TCnNTService.LogError(const ErrorMsg: string);
begin
  FLastErrorCode := GetLastError;
  FLastErrorMsg := ErrorMsg;
end;

function TCnNTService.OpenSCHandle: Boolean;
begin
  FSCHandle := OpenSCManager(PChar(ComputerName), nil, SC_MANAGER_ALL_ACCESS or
    GENERIC_WRITE or GENERIC_EXECUTE);

  if SCHandle = 0 then
  begin
    LogError('OpenSCManager fail.');
    Result := False;
  end
  else
    Result := True;
end;

function TCnNTService.OpenSvcHandle: Boolean;
begin
  FServiceHandle := OpenService(SCHandle, PChar(ServiceName),
    SERVICE_ALL_ACCESS);

  if ServiceHandle = 0 then
  begin
    LogError('OpenService fail');
    Result := False;
  end
  else
    Result := True;
end;

function TCnNTService.SetStatus(var Value: TServiceStatus): Boolean;
begin
  Result := False;
  if not OpenSCHandle then Exit;
  try
    if not OpenSvcHandle then Exit;
    try
      if not SetServiceStatus(ServiceHandle, Value) then
        LogError('SetStatus fail')
      else
        Result := True;
    finally
      CloseSvcHandle;
    end;
  finally
    CloseSCHandle;
  end;
end;

function TCnNTService.Start: Boolean;
var
  P: PChar;
begin
  Result := False;
  if not OpenSCHandle then Exit;
  try
    if not OpenSvcHandle then Exit;
    try
      if (not StartService(ServiceHandle, 0, P))
        and (GetLastError <> ERROR_SERVICE_ALREADY_RUNNING) then
        LogError('StartService fail')
      else
        Result := True;
    finally
      CloseSvcHandle;
    end;
  finally
    CloseSCHandle;
  end;
end;

function TCnNTService.Stop: Boolean;
var
  SvStatus: TServiceStatus;
begin
  Result := False;
  if not OpenSCHandle then Exit;
  try
    if not OpenSvcHandle then Exit;
    try
      if not ControlService(Servicehandle, SERVICE_CONTROL_STOP, svStatus) then
        LogError('StopService fail')
      else
        Result := True;
    finally
      CloseSvcHandle;
    end;
  finally
    CloseSCHandle;
  end;
end;

function TCnNTService.Uninstall: Boolean;
begin
  Result := False;
  if not OpenSCHandle then Exit;
  try
    if not OpenSvcHandle then Exit;
    try
      if not DeleteService(ServiceHandle) then
        LogError('DeleteService fail')
      else
        Result := True;
    finally
      CloseSvcHandle;
    end;
  finally
    CloseSCHandle;
  end;
end;

end.
