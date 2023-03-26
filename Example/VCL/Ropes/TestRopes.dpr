program TestRopes;

uses
  Forms,
  TestRopeUnit in 'TestRopeUnit.pas' {TestRopeForm},
  CnStrings in '..\..\..\Source\Common\CnStrings.pas',
  CnRopes in '..\..\..\Source\Common\CnRopes.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTestRopeForm, TestRopeForm);
  Application.Run;
end.
