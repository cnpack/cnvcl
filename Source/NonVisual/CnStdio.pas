{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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

unit CnStdio;
{* |<PRE>
================================================================================
* 单元名称：进程标准输入输出错误重定向单元
* 单元作者：CnPack 开发组
* 备    注：封装 Win32/Win64 启动进程并重定向 stdin/stdout/stderr。
*           当前实现为核心类，不含设计期组件。
* 修改记录：2026.05.06 V1.0
*               创建单元。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, CnNative, CnClasses, CnConsts, CnCompConsts;

type
  TCnStdioState = (csIdle, csStarting, csRunning, csExited, csError);
  TCnStdioStreamKind = (cskStdOut, cskStdErr);

  TCnStdioStateEvent = procedure(Sender: TObject; OldState, NewState: TCnStdioState) of object;
  TCnStdioRawDataEvent = procedure(Sender: TObject; StreamKind: TCnStdioStreamKind;
    const Data: TBytes) of object;
  TCnStdioLineEvent = procedure(Sender: TObject; StreamKind: TCnStdioStreamKind;
    const Line: string) of object;
  TCnStdioExitEvent = procedure(Sender: TObject; ExitCode: Cardinal) of object;
  TCnStdioErrorEvent = procedure(Sender: TObject; const Stage: string;
    ErrorCode: Cardinal; const ErrorMessage: string) of object;

  TCnStdioProcess = class(TObject)
  private
    FCriticalSection: TRTLCriticalSection;
    FState: TCnStdioState;
    FApplicationName: string;
    FCommandLine: string;
    FWorkingDirectory: string;
    FCreateNoWindow: Boolean;

    FProcessHandle: THandle;
    FThreadHandle: THandle;
    FProcessId: Cardinal;
    FExitCode: Cardinal;
    FExitCodeReady: Boolean;
    FStartedTick: Cardinal;
    FProcessExitedEvent: THandle;
    FReadDoneEvent: THandle;
    FReaderCount: Integer;
    FExitEventFired: Boolean;

    FStdInWriteHandle: THandle;
    FStdOutReadHandle: THandle;
    FStdErrReadHandle: THandle;

    FStdOutBuffer: TStringList;
    FStdErrBuffer: TStringList;
    FStdOutRemain: string;
    FStdErrRemain: string;

    FOnStateChanged: TCnStdioStateEvent;
    FOnRawData: TCnStdioRawDataEvent;
    FOnLine: TCnStdioLineEvent;
    FOnExited: TCnStdioExitEvent;
    FOnError: TCnStdioErrorEvent;

    FStdOutThread: TThread;
    FStdErrThread: TThread;
    FWaitThread: TThread;

    procedure SetState(const Value: TCnStdioState);
    procedure DoError(const Stage: string; ErrorCode: Cardinal);
    procedure DoRawData(StreamKind: TCnStdioStreamKind; const Data: TBytes);
    procedure DoLine(StreamKind: TCnStdioStreamKind; const Line: string);
    procedure DoExited;

    procedure ResetRuntimeState;
    procedure CloseRuntimeHandles;
    procedure ClosePipeHandles;
    procedure InternalCloseHandle(var AHandle: THandle);
    function BuildCreateFlags: Cardinal;

    procedure InternalHandleData(StreamKind: TCnStdioStreamKind; const Data: TBytes);
    procedure InternalSplitLines(StreamKind: TCnStdioStreamKind; const Chunk: string);
    procedure InternalFlushRemain(StreamKind: TCnStdioStreamKind);
    procedure InternalReaderDone;
    procedure InternalProcessExited;
    function InternalStartThreads: Boolean;
    function InternalBuildChildEnvironment: Pointer;
    function InternalBuildCurrentDirectory: PChar;
    procedure InternalFreeThreads;

    function GetRunning: Boolean;
    function GetRunningTime: Cardinal;
    function GetStdOutText: string;
    function GetStdErrText: string;

  public
    constructor Create;
    destructor Destroy; override;

    function Start: Boolean;
    procedure CloseInput;
    function WriteBytes(const Data: TBytes): Boolean;
    function WriteString(const S: string): Boolean;
    function WriteLine(const S: string): Boolean;
    function WaitFor(Timeout: Cardinal): Boolean;
    procedure Terminate;
    procedure ClearOutput;

    property ApplicationName: string read FApplicationName write FApplicationName;
    property CommandLine: string read FCommandLine write FCommandLine;
    property WorkingDirectory: string read FWorkingDirectory write FWorkingDirectory;
    property CreateNoWindow: Boolean read FCreateNoWindow write FCreateNoWindow;

    property State: TCnStdioState read FState;
    property ProcessHandle: THandle read FProcessHandle;
    property ProcessId: Cardinal read FProcessId;
    property ExitCode: Cardinal read FExitCode;
    property ExitCodeReady: Boolean read FExitCodeReady;
    property Running: Boolean read GetRunning;
    property RunningTime: Cardinal read GetRunningTime;
    property StdOutText: string read GetStdOutText;
    property StdErrText: string read GetStdErrText;

    property OnStateChanged: TCnStdioStateEvent read FOnStateChanged write FOnStateChanged;
    property OnRawData: TCnStdioRawDataEvent read FOnRawData write FOnRawData;
    property OnLine: TCnStdioLineEvent read FOnLine write FOnLine;
    property OnExited: TCnStdioExitEvent read FOnExited write FOnExited;
    property OnError: TCnStdioErrorEvent read FOnError write FOnError;
  end;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnStdioProcessor = class(TCnComponent)
  private
    FProcess: TCnStdioProcess;
    FActive: Boolean;
    FOnStateChanged: TCnStdioStateEvent;
    FOnRawData: TCnStdioRawDataEvent;
    FOnLine: TCnStdioLineEvent;
    FOnExited: TCnStdioExitEvent;
    FOnError: TCnStdioErrorEvent;

    function GetApplicationName: string;
    procedure SetApplicationName(const Value: string);
    function GetCommandLine: string;
    procedure SetCommandLine(const Value: string);
    function GetWorkingDirectory: string;
    procedure SetWorkingDirectory(const Value: string);
    function GetCreateNoWindow: Boolean;
    procedure SetCreateNoWindow(const Value: Boolean);
    function GetState: TCnStdioState;
    function GetProcessId: Cardinal;
    function GetExitCode: Cardinal;
    function GetExitCodeReady: Boolean;
    function GetRunningTime: Cardinal;
    function GetStdOutText: string;
    function GetStdErrText: string;

    procedure ProcessStateChanged(Sender: TObject; OldState, NewState: TCnStdioState);
    procedure ProcessRawData(Sender: TObject; StreamKind: TCnStdioStreamKind; const Data: TBytes);
    procedure ProcessLine(Sender: TObject; StreamKind: TCnStdioStreamKind; const Line: string);
    procedure ProcessExited(Sender: TObject; ExitCode: Cardinal);
    procedure ProcessError(Sender: TObject; const Stage: string; ErrorCode: Cardinal;
      const ErrorMessage: string);

    procedure SetActive(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Start: Boolean;
    procedure Stop;
    function WaitFor(Timeout: Cardinal): Boolean;
    procedure CloseInput;
    function WriteBytes(const Data: TBytes): Boolean;
    function WriteString(const S: string): Boolean;
    function WriteLine(const S: string): Boolean;
    procedure ClearOutput;

    property Process: TCnStdioProcess read FProcess;
    property State: TCnStdioState read GetState;
    property ProcessId: Cardinal read GetProcessId;
    property ExitCode: Cardinal read GetExitCode;
    property ExitCodeReady: Boolean read GetExitCodeReady;
    property RunningTime: Cardinal read GetRunningTime;
    property StdOutText: string read GetStdOutText;
    property StdErrText: string read GetStdErrText;
  published
    property Active: Boolean read FActive write SetActive default False;
    property ApplicationName: string read GetApplicationName write SetApplicationName;
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property WorkingDirectory: string read GetWorkingDirectory write SetWorkingDirectory;
    property CreateNoWindow: Boolean read GetCreateNoWindow write SetCreateNoWindow default True;

    property OnStateChanged: TCnStdioStateEvent read FOnStateChanged write FOnStateChanged;
    property OnRawData: TCnStdioRawDataEvent read FOnRawData write FOnRawData;
    property OnLine: TCnStdioLineEvent read FOnLine write FOnLine;
    property OnExited: TCnStdioExitEvent read FOnExited write FOnExited;
    property OnError: TCnStdioErrorEvent read FOnError write FOnError;
  end;

implementation

const
  CN_STDIO_WAIT_INFINITE = Cardinal($FFFFFFFF);

  CN_STDIO_DEFAULT_READ_BUFFER_SIZE = 1024;

  CN_STDIO_WRITE_FLUSH_TIMEOUT = 3000;

  SCN_STDIO_STAGE_CREATE_PIPE = 'CreatePipe';

  SCN_STDIO_STAGE_CREATE_PROCESS = 'CreateProcess';

  SCN_STDIO_STAGE_WRITE_STDIN = 'WriteStdIn';

  SCN_STDIO_STAGE_WAIT_PROCESS = 'WaitProcess';

  SCN_STDIO_CRLF = #13#10;

type
  TCnStdioReadThread = class(TThread)
  private
    FOwner: TCnStdioProcess;
    FReadHandle: THandle;
    FStreamKind: TCnStdioStreamKind;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TCnStdioProcess; AReadHandle: THandle;
      AStreamKind: TCnStdioStreamKind);
  end;

  TCnStdioWaitThread = class(TThread)
  private
    FOwner: TCnStdioProcess;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TCnStdioProcess);
  end;

{ TCnStdioReadThread }

constructor TCnStdioReadThread.Create(AOwner: TCnStdioProcess;
  AReadHandle: THandle; AStreamKind: TCnStdioStreamKind);
begin
  FOwner := AOwner;
  FReadHandle := AReadHandle;
  FStreamKind := AStreamKind;
  inherited Create(False);
  FreeOnTerminate := False;
end;

procedure TCnStdioReadThread.Execute;
var
  Buf: array[0..CN_STDIO_DEFAULT_READ_BUFFER_SIZE - 1] of Byte;
  ReadCount: Cardinal;
  B: TBytes;
begin
  while not Terminated do
  begin
    ReadCount := 0;
    if not ReadFile(FReadHandle, Buf[0], CN_STDIO_DEFAULT_READ_BUFFER_SIZE, ReadCount, nil) then
      Break;

    if ReadCount = 0 then
      Break;

    SetLength(B, Integer(ReadCount));
    Move(Buf[0], B[0], Integer(ReadCount));
    FOwner.InternalHandleData(FStreamKind, B);
  end;

  FOwner.InternalReaderDone;
end;

{ TCnStdioWaitThread }

constructor TCnStdioWaitThread.Create(AOwner: TCnStdioProcess);
begin
  FOwner := AOwner;
  inherited Create(False);
  FreeOnTerminate := False;
end;

procedure TCnStdioWaitThread.Execute;
begin
  if FOwner.FProcessHandle <> 0 then
  begin
    WaitForSingleObject(FOwner.FProcessHandle, INFINITE);
    FOwner.InternalProcessExited;
  end;
end;

{ TCnStdioProcess }

constructor TCnStdioProcess.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
  FState := csIdle;
  FCreateNoWindow := True;
  FProcessHandle := 0;
  FThreadHandle := 0;
  FStdInWriteHandle := 0;
  FStdOutReadHandle := 0;
  FStdErrReadHandle := 0;
  FProcessExitedEvent := CreateEvent(nil, True, False, nil);
  FReadDoneEvent := CreateEvent(nil, True, False, nil);
  FStdOutBuffer := TStringList.Create;
  FStdErrBuffer := TStringList.Create;
end;

destructor TCnStdioProcess.Destroy;
begin
  Terminate;
  WaitFor(2000);
  CloseRuntimeHandles;
  InternalFreeThreads;
  CloseHandle(FProcessExitedEvent);
  CloseHandle(FReadDoneEvent);
  FStdOutBuffer.Free;
  FStdErrBuffer.Free;
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

procedure TCnStdioProcess.InternalFreeThreads;
begin
  if FStdOutThread <> nil then
  begin
    FStdOutThread.Terminate;
    FStdOutThread.WaitFor;
    FreeAndNil(FStdOutThread);
  end;

  if FStdErrThread <> nil then
  begin
    FStdErrThread.Terminate;
    FStdErrThread.WaitFor;
    FreeAndNil(FStdErrThread);
  end;

  if FWaitThread <> nil then
  begin
    FWaitThread.Terminate;
    FWaitThread.WaitFor;
    FreeAndNil(FWaitThread);
  end;
end;

procedure TCnStdioProcess.InternalCloseHandle(var AHandle: THandle);
begin
  if AHandle <> 0 then
  begin
    CloseHandle(AHandle);
    AHandle := 0;
  end;
end;

procedure TCnStdioProcess.CloseRuntimeHandles;
begin
  ClosePipeHandles;
  InternalCloseHandle(FThreadHandle);
  InternalCloseHandle(FProcessHandle);
end;

procedure TCnStdioProcess.ClosePipeHandles;
begin
  InternalCloseHandle(FStdInWriteHandle);
  InternalCloseHandle(FStdOutReadHandle);
  InternalCloseHandle(FStdErrReadHandle);
end;

procedure TCnStdioProcess.ResetRuntimeState;
begin
  ResetEvent(FProcessExitedEvent);
  ResetEvent(FReadDoneEvent);
  FProcessId := 0;
  FExitCode := 0;
  FExitCodeReady := False;
  FExitEventFired := False;
  FStdOutRemain := '';
  FStdErrRemain := '';
  FReaderCount := 0;
  ClearOutput;
end;

function TCnStdioProcess.BuildCreateFlags: Cardinal;
begin
  Result := NORMAL_PRIORITY_CLASS;
  if FCreateNoWindow then
    Result := Result or CREATE_NO_WINDOW;
end;

function TCnStdioProcess.InternalBuildChildEnvironment: Pointer;
begin
  Result := nil;
end;

function TCnStdioProcess.InternalBuildCurrentDirectory: PChar;
begin
  if FWorkingDirectory <> '' then
    Result := PChar(FWorkingDirectory)
  else
    Result := nil;
end;

procedure TCnStdioProcess.SetState(const Value: TCnStdioState);
var
  OldState: TCnStdioState;
begin
  OldState := FState;
  if OldState = Value then
    Exit;

  FState := Value;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self, OldState, Value);
end;

procedure TCnStdioProcess.DoError(const Stage: string; ErrorCode: Cardinal);
begin
  if Assigned(FOnError) then
    FOnError(Self, Stage, ErrorCode, SysErrorMessage(Integer(ErrorCode)));
end;

procedure TCnStdioProcess.DoRawData(StreamKind: TCnStdioStreamKind;
  const Data: TBytes);
begin
  if Assigned(FOnRawData) then
    FOnRawData(Self, StreamKind, Data);
end;

procedure TCnStdioProcess.DoLine(StreamKind: TCnStdioStreamKind;
  const Line: string);
begin
  if StreamKind = cskStdOut then
    FStdOutBuffer.Add(Line)
  else
    FStdErrBuffer.Add(Line);

  if Assigned(FOnLine) then
    FOnLine(Self, StreamKind, Line);
end;

procedure TCnStdioProcess.DoExited;
begin
  if FExitEventFired then
    Exit;

  FExitEventFired := True;
  if Assigned(FOnExited) then
    FOnExited(Self, FExitCode);
end;

procedure TCnStdioProcess.InternalHandleData(StreamKind: TCnStdioStreamKind;
  const Data: TBytes);
var
  S: string;
begin
  EnterCriticalSection(FCriticalSection);
  try
    DoRawData(StreamKind, Data);
    if Length(Data) > 0 then
    begin
      SetLength(S, Length(Data));
      Move(Data[0], S[1], Length(Data));
      InternalSplitLines(StreamKind, S);
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TCnStdioProcess.InternalSplitLines(StreamKind: TCnStdioStreamKind;
  const Chunk: string);
var
  P: Integer;
  Line: string;
  S: string;
begin
  if StreamKind = cskStdOut then
    S := FStdOutRemain + Chunk
  else
    S := FStdErrRemain + Chunk;

  P := Pos(SCN_STDIO_CRLF, S);
  while P > 0 do
  begin
    Line := Copy(S, 1, P - 1);
    DoLine(StreamKind, Line);
    Delete(S, 1, P + Length(SCN_STDIO_CRLF) - 1);
    P := Pos(SCN_STDIO_CRLF, S);
  end;

  if StreamKind = cskStdOut then
    FStdOutRemain := S
  else
    FStdErrRemain := S;
end;

procedure TCnStdioProcess.InternalFlushRemain(StreamKind: TCnStdioStreamKind);
begin
  if StreamKind = cskStdOut then
  begin
    if FStdOutRemain <> '' then
    begin
      DoLine(StreamKind, FStdOutRemain);
      FStdOutRemain := '';
    end;
  end
  else
  begin
    if FStdErrRemain <> '' then
    begin
      DoLine(StreamKind, FStdErrRemain);
      FStdErrRemain := '';
    end;
  end;
end;

procedure TCnStdioProcess.InternalReaderDone;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if FReaderCount > 0 then
      Dec(FReaderCount);

    if FReaderCount = 0 then
    begin
      InternalFlushRemain(cskStdOut);
      InternalFlushRemain(cskStdErr);
      SetEvent(FReadDoneEvent);
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TCnStdioProcess.InternalProcessExited;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if not FExitCodeReady then
    begin
      GetExitCodeProcess(FProcessHandle, FExitCode);
      FExitCodeReady := True;
      SetState(csExited);
      SetEvent(FProcessExitedEvent);
      DoExited;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TCnStdioProcess.InternalStartThreads: Boolean;
begin
  FReaderCount := 2;

  FStdOutThread := TCnStdioReadThread.Create(Self, FStdOutReadHandle, cskStdOut);
  FStdErrThread := TCnStdioReadThread.Create(Self, FStdErrReadHandle, cskStdErr);
  FWaitThread := TCnStdioWaitThread.Create(Self);

  Result := True;
end;

function TCnStdioProcess.Start: Boolean;
var
  SA: TSecurityAttributes;
  StdInReadHandle: THandle;
  StdInWriteHandle: THandle;
  StdOutReadHandle: THandle;
  StdOutWriteHandle: THandle;
  StdErrReadHandle: THandle;
  StdErrWriteHandle: THandle;
  SI: TStartupInfo;
  PI: TProcessInformation;
  Cmd: string;
  AppNamePtr: PChar;
begin
  Result := False;
  EnterCriticalSection(FCriticalSection);
  try
    if FState = csRunning then
    begin
      Result := True;
      Exit;
    end;

    ResetRuntimeState;
    CloseRuntimeHandles;
    InternalFreeThreads;
    SetState(csStarting);

    FillChar(SA, SizeOf(SA), 0);
    SA.nLength := SizeOf(SA);
    SA.bInheritHandle := True;
    SA.lpSecurityDescriptor := nil;

    StdInReadHandle := 0;
    StdInWriteHandle := 0;
    StdOutReadHandle := 0;
    StdOutWriteHandle := 0;
    StdErrReadHandle := 0;
    StdErrWriteHandle := 0;

    if not CreatePipe(StdOutReadHandle, StdOutWriteHandle, @SA, 0) then
    begin
      DoError(SCN_STDIO_STAGE_CREATE_PIPE, GetLastError);
      SetState(csError);
      Exit;
    end;

    if not CreatePipe(StdErrReadHandle, StdErrWriteHandle, @SA, 0) then
    begin
      DoError(SCN_STDIO_STAGE_CREATE_PIPE, GetLastError);
      CloseHandle(StdOutReadHandle);
      CloseHandle(StdOutWriteHandle);
      SetState(csError);
      Exit;
    end;

    if not CreatePipe(StdInReadHandle, StdInWriteHandle, @SA, 0) then
    begin
      DoError(SCN_STDIO_STAGE_CREATE_PIPE, GetLastError);
      CloseHandle(StdOutReadHandle);
      CloseHandle(StdOutWriteHandle);
      CloseHandle(StdErrReadHandle);
      CloseHandle(StdErrWriteHandle);
      SetState(csError);
      Exit;
    end;

    SetHandleInformation(StdOutReadHandle, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(StdErrReadHandle, HANDLE_FLAG_INHERIT, 0);
    SetHandleInformation(StdInWriteHandle, HANDLE_FLAG_INHERIT, 0);

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := StdInReadHandle;
    SI.hStdOutput := StdOutWriteHandle;
    SI.hStdError := StdErrWriteHandle;

    FillChar(PI, SizeOf(PI), 0);
    Cmd := FCommandLine;
    if Cmd = '' then
      Cmd := FApplicationName;

    if FApplicationName <> '' then
      AppNamePtr := PChar(FApplicationName)
    else
      AppNamePtr := nil;

    if not CreateProcess(AppNamePtr, PChar(Cmd), nil, nil, True,
      BuildCreateFlags, InternalBuildChildEnvironment, InternalBuildCurrentDirectory,
      SI, PI) then
    begin
      DoError(SCN_STDIO_STAGE_CREATE_PROCESS, GetLastError);
      CloseHandle(StdOutReadHandle);
      CloseHandle(StdOutWriteHandle);
      CloseHandle(StdErrReadHandle);
      CloseHandle(StdErrWriteHandle);
      CloseHandle(StdInReadHandle);
      CloseHandle(StdInWriteHandle);
      SetState(csError);
      Exit;
    end;

    CloseHandle(StdInReadHandle);
    CloseHandle(StdOutWriteHandle);
    CloseHandle(StdErrWriteHandle);

    FStdInWriteHandle := StdInWriteHandle;
    FStdOutReadHandle := StdOutReadHandle;
    FStdErrReadHandle := StdErrReadHandle;
    FProcessHandle := PI.hProcess;
    FThreadHandle := PI.hThread;
    FProcessId := PI.dwProcessId;
    FStartedTick := GetTickCount;

    if not InternalStartThreads then
    begin
      DoError(SCN_STDIO_STAGE_CREATE_PROCESS, ERROR_INVALID_DATA);
      SetState(csError);
      Exit;
    end;

    SetState(csRunning);
    Result := True;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TCnStdioProcess.CloseInput;
begin
  EnterCriticalSection(FCriticalSection);
  try
    InternalCloseHandle(FStdInWriteHandle);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TCnStdioProcess.WriteBytes(const Data: TBytes): Boolean;
var
  Written: Cardinal;
  Need: Cardinal;
begin
  Result := False;
  EnterCriticalSection(FCriticalSection);
  try
    if FStdInWriteHandle = 0 then
    begin
      DoError(SCN_STDIO_STAGE_WRITE_STDIN, ERROR_INVALID_HANDLE);
      Exit;
    end;

    Need := Cardinal(Length(Data));
    if Need = 0 then
    begin
      Result := True;
      Exit;
    end;

    Written := 0;
    if not WriteFile(FStdInWriteHandle, Data[0], Need, Written, nil) then
    begin
      DoError(SCN_STDIO_STAGE_WRITE_STDIN, GetLastError);
      Exit;
    end;

    Result := Written = Need;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

function TCnStdioProcess.WriteString(const S: string): Boolean;
var
  B: TBytes;
begin
  if Length(S) = 0 then
  begin
    Result := True;
    Exit;
  end;

  SetLength(B, Length(S));
  Move(S[1], B[0], Length(S));
  Result := WriteBytes(B);
end;

function TCnStdioProcess.WriteLine(const S: string): Boolean;
begin
  Result := WriteString(S + SCN_STDIO_CRLF);
end;

function TCnStdioProcess.WaitFor(Timeout: Cardinal): Boolean;
var
  WaitRes: Cardinal;
  TickBegin: Cardinal;
  NeedWait: Cardinal;
  Used: Cardinal;
begin
  Result := False;
  TickBegin := GetTickCount;

  WaitRes := WaitForSingleObject(FProcessExitedEvent, Timeout);
  if WaitRes <> WAIT_OBJECT_0 then
  begin
    if WaitRes = WAIT_TIMEOUT then
      Exit;

    DoError(SCN_STDIO_STAGE_WAIT_PROCESS, GetLastError);
    Exit;
  end;

  if Timeout = CN_STDIO_WAIT_INFINITE then
    NeedWait := CN_STDIO_WAIT_INFINITE
  else
  begin
    Used := GetTickCount - TickBegin;
    if Used >= Timeout then
      NeedWait := 0
    else
      NeedWait := Timeout - Used;
  end;

  WaitRes := WaitForSingleObject(FReadDoneEvent, NeedWait);
  if (WaitRes <> WAIT_OBJECT_0) and (WaitRes <> WAIT_TIMEOUT) then
    DoError(SCN_STDIO_STAGE_WAIT_PROCESS, GetLastError);

  Result := True;
end;

procedure TCnStdioProcess.Terminate;
begin
  EnterCriticalSection(FCriticalSection);
  try
    if (FProcessHandle <> 0) and (FState = csRunning) then
      Windows.TerminateProcess(FProcessHandle, 1);
    CloseInput;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TCnStdioProcess.ClearOutput;
begin
  FStdOutBuffer.Clear;
  FStdErrBuffer.Clear;
end;

function TCnStdioProcess.GetRunning: Boolean;
begin
  Result := FState = csRunning;
end;

function TCnStdioProcess.GetRunningTime: Cardinal;
begin
  if FState in [csRunning, csExited] then
    Result := GetTickCount - FStartedTick
  else
    Result := 0;
end;

function TCnStdioProcess.GetStdOutText: string;
begin
  Result := FStdOutBuffer.Text;
end;

function TCnStdioProcess.GetStdErrText: string;
begin
  Result := FStdErrBuffer.Text;
end;

{ TCnStdioProcessComponent }

constructor TCnStdioProcessor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProcess := TCnStdioProcess.Create;
  FProcess.OnStateChanged := ProcessStateChanged;
  FProcess.OnRawData := ProcessRawData;
  FProcess.OnLine := ProcessLine;
  FProcess.OnExited := ProcessExited;
  FProcess.OnError := ProcessError;
  FActive := False;
end;

destructor TCnStdioProcessor.Destroy;
begin
  Stop;
  FProcess.Free;
  inherited Destroy;
end;

procedure TCnStdioProcessor.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnStdioProcessorName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnStdioProcessorComment;
end;

procedure TCnStdioProcessor.Loaded;
begin
  inherited Loaded;
  if FActive then
    SetActive(True);
end;

function TCnStdioProcessor.GetApplicationName: string;
begin
  Result := FProcess.ApplicationName;
end;

procedure TCnStdioProcessor.SetApplicationName(const Value: string);
begin
  FProcess.ApplicationName := Value;
end;

function TCnStdioProcessor.GetCommandLine: string;
begin
  Result := FProcess.CommandLine;
end;

procedure TCnStdioProcessor.SetCommandLine(const Value: string);
begin
  FProcess.CommandLine := Value;
end;

function TCnStdioProcessor.GetWorkingDirectory: string;
begin
  Result := FProcess.WorkingDirectory;
end;

procedure TCnStdioProcessor.SetWorkingDirectory(const Value: string);
begin
  FProcess.WorkingDirectory := Value;
end;

function TCnStdioProcessor.GetCreateNoWindow: Boolean;
begin
  Result := FProcess.CreateNoWindow;
end;

procedure TCnStdioProcessor.SetCreateNoWindow(const Value: Boolean);
begin
  FProcess.CreateNoWindow := Value;
end;

function TCnStdioProcessor.GetState: TCnStdioState;
begin
  Result := FProcess.State;
end;

function TCnStdioProcessor.GetProcessId: Cardinal;
begin
  Result := FProcess.ProcessId;
end;

function TCnStdioProcessor.GetExitCode: Cardinal;
begin
  Result := FProcess.ExitCode;
end;

function TCnStdioProcessor.GetExitCodeReady: Boolean;
begin
  Result := FProcess.ExitCodeReady;
end;

function TCnStdioProcessor.GetRunningTime: Cardinal;
begin
  Result := FProcess.RunningTime;
end;

function TCnStdioProcessor.GetStdOutText: string;
begin
  Result := FProcess.StdOutText;
end;

function TCnStdioProcessor.GetStdErrText: string;
begin
  Result := FProcess.StdErrText;
end;

procedure TCnStdioProcessor.ProcessStateChanged(Sender: TObject; OldState,
  NewState: TCnStdioState);
begin
  FActive := NewState = csRunning;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self, OldState, NewState);
end;

procedure TCnStdioProcessor.ProcessRawData(Sender: TObject;
  StreamKind: TCnStdioStreamKind; const Data: TBytes);
begin
  if Assigned(FOnRawData) then
    FOnRawData(Self, StreamKind, Data);
end;

procedure TCnStdioProcessor.ProcessLine(Sender: TObject;
  StreamKind: TCnStdioStreamKind; const Line: string);
begin
  if Assigned(FOnLine) then
    FOnLine(Self, StreamKind, Line);
end;

procedure TCnStdioProcessor.ProcessExited(Sender: TObject;
  ExitCode: Cardinal);
begin
  FActive := False;
  if Assigned(FOnExited) then
    FOnExited(Self, ExitCode);
end;

procedure TCnStdioProcessor.ProcessError(Sender: TObject;
  const Stage: string; ErrorCode: Cardinal; const ErrorMessage: string);
begin
  if Assigned(FOnError) then
    FOnError(Self, Stage, ErrorCode, ErrorMessage);
end;

procedure TCnStdioProcessor.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    Exit;

  FActive := Value;
  if csDesigning in ComponentState then
    Exit;

  if csLoading in ComponentState then
    Exit;

  if FActive then
    FActive := FProcess.Start
  else
  begin
    FProcess.Terminate;
    FProcess.WaitFor(2000);
    FActive := False;
  end;
end;

function TCnStdioProcessor.Start: Boolean;
begin
  Result := FProcess.Start;
end;

procedure TCnStdioProcessor.Stop;
begin
  FProcess.Terminate;
  FProcess.WaitFor(2000);
  FActive := False;
end;

function TCnStdioProcessor.WaitFor(Timeout: Cardinal): Boolean;
begin
  Result := FProcess.WaitFor(Timeout);
end;

procedure TCnStdioProcessor.CloseInput;
begin
  FProcess.CloseInput;
end;

function TCnStdioProcessor.WriteBytes(const Data: TBytes): Boolean;
begin
  Result := FProcess.WriteBytes(Data);
end;

function TCnStdioProcessor.WriteString(const S: string): Boolean;
begin
  Result := FProcess.WriteString(S);
end;

function TCnStdioProcessor.WriteLine(const S: string): Boolean;
begin
  Result := FProcess.WriteLine(S);
end;

procedure TCnStdioProcessor.ClearOutput;
begin
  FProcess.ClearOutput;
end;

end.
