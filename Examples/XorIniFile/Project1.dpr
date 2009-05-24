program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CnIni in '..\..\Source\Common\CnIni.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
