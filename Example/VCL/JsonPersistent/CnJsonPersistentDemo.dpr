program CnJsonPersistentDemo;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CnJsonPersistent in '..\..\..\Source\Common\CnJsonPersistent.pas',
  JsonDataObjects in '..\..\..\Source\ThirdParty\JsonDataObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
