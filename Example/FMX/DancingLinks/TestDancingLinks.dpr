program TestDancingLinks;

uses
  System.StartUpCopy,
  FMX.Forms,
  TestDancingLinksUnit in 'TestDancingLinksUnit.pas' {DancingLinksForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDancingLinksForm, DancingLinksForm);
  Application.Run;
end.
