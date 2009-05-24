program AntiCheater;

uses
  Forms,
  AntiCheaterTest in 'AntiCheaterTest.pas' {Form1},
  CnAntiCheater in '..\..\Source\Common\CnAntiCheater.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
