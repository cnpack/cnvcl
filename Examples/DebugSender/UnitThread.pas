unit UnitThread;

interface

uses
  Classes, Windows;

type
  TSendThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

implementation

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TSendThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

uses
  CnDebug, UnitOutput;

{ TSendThread }

procedure TSendThread.Execute;
begin
  { Place thread code here }
  while not Terminated do
  begin
    Sleep(1000);
    if FormSend.rgMethod.ItemIndex = 1 then
      CnDebugger.TraceMsg(FormSend.edtMsg.Text)
    else
      CnDebugger.LogMsg(FormSend.edtMsg.Text);
  end;
end;

end.
