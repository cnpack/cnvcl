program HardwareBreakDemo;

uses
  Forms,
  uHardwareBreakDemo in 'uHardwareBreakDemo.pas' {Form1},
  CnHardwareBreakpoint in '..\..\..\Source\NonVisual\CnHardwareBreakpoint.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
