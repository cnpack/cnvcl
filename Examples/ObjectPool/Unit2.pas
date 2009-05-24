unit Unit2;

interface

uses
  SysUtils, Classes, Windows;

procedure CreateTerminateThread(const iWaitTime: Integer);

implementation

procedure TerminateApplication(ID: THandle = 0);
var  hh: HWND;begin  if ID = 0 then    ID := GetCurrentProcessID;  hh := OpenProcess(PROCESS_ALL_ACCESS, True, ID);  try    TerminateProcess(hh, 0);  finally    CLoseHandle(hh)  end;end;type  TTerminateProcessThread = class(TThread)  private    FWaitTime: Integer;  protected    procedure Execute; override;  public    constructor Create(const aWaitTime: Integer); virtual;  end;{ TTerminateProcessThread }constructor TTerminateProcessThread.Create(const aWaitTime: Integer);begin  FWaitTime := aWaitTime;  inherited Create(False);end;procedure TTerminateProcessThread.Execute;begin  Sleep(FWaitTime);  TerminateApplication();end;procedure CreateTerminateThread(const iWaitTime: Integer);begin  TTerminateProcessThread.Create(iWaitTime);end;

end.
 