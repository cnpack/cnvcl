unit UnitThread;

interface

uses
  Classes, Windows, SysUtils;

type
  TSendThread = class(TThread)
  private
    FCount: Integer;
  protected
    procedure Execute; override;
  end;

implementation

uses
  CnDebug, UnitOutput;

{ TSendThread }

procedure TSendThread.Execute;
begin
  { Place thread code here }
  while not Terminated do
  begin
    Sleep(1000);
    Inc(FCount);
    CnDebugger.WatchMsg('Count', IntToStr(FCount));
    if FormSend.rgMethod.ItemIndex = 1 then
      CnDebugger.TraceMsg(FormSend.edtMsg.Text)
    else
      CnDebugger.LogMsg(FormSend.edtMsg.Text);
  end;
end;

end.
