program CnTreeTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  CnTreeTestUnit in 'CnTreeTestUnit.pas' {CnTreeTestForm},
  CnTree in '..\..\Source\Common\CnTree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCnTreeTestForm, CnTreeTestForm);
  Application.Run;
end.
