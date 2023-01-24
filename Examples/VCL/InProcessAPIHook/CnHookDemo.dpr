program CnHookDemo;

uses
  Forms,
  uCnHookDemo in 'uCnHookDemo.pas' {Form1},
  CnInProcessAPIHook in '..\..\..\Source\NonVisual\CnInProcessAPIHook.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
