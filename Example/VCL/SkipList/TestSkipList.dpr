program TestSkipList;

uses
  Forms,
  TestSkipListUnit in 'TestSkipListUnit.pas' {SkipListTestForm},
  CnSkipList in '..\..\..\Source\Common\CnSkipList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSkipListTestForm, SkipListTestForm);
  Application.Run;
end.
