program TestDancingLinks;

uses
  Forms,
  TestDancingLinksUnit in 'TestDancingLinksUnit.pas' {DancingLinksForm},
  CnDancingLinks in '..\..\Source\Common\CnDancingLinks.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDancingLinksForm, DancingLinksForm);
  Application.Run;
end.
