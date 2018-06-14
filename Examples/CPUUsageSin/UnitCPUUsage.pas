unit UnitCPUUsage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TSinThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TFormSin = class(TForm)
    btnStart: TButton;
    procedure btnStartClick(Sender: TObject);
  private
    FThread: TSinThread;
  public
    { Public declarations }
  end;

var
  FormSin: TFormSin;

implementation

{$R *.DFM}

{ TSinThread }

procedure TSinThread.Execute;
const
  COUNT = 200;
  INTERVAL = 400;
var
  Busy, Idle: array[0..COUNT - 1] of Cardinal;
  I, H: Integer;
  Start: DWORD;
  R: Double;
begin
  H := INTERVAL div 2;
  R := 0;
  for I := 0 to COUNT - 1 do
  begin
    Busy[I] := H + Trunc(Sin(R * 3.1415926) * H);
    Idle[I] := INTERVAL - Busy[I];
    R := R + 0.01;
  end;

  I := 0;
  while not Terminated do
  begin
    I := I mod COUNT;
    Start := GetTickCount;
    while GetTickCount - Start <= Busy[I] do
      ;

    Sleep(Idle[I]);
    Inc(I);
  end;
end;

procedure TFormSin.btnStartClick(Sender: TObject);
begin
  if FThread = nil then
  begin
    FThread := TSinThread.Create(True);
    FThread.Resume;
    btnStart.Caption := 'Stop';
  end
  else
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread := nil;
    btnStart.Caption := 'Start'
  end;
end;

end.
