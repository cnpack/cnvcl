program Graph;

uses
  Forms,
  GraphUnit in 'GraphUnit.pas' {FormGraph},
  CnGraph in '..\..\Source\Common\CnGraph.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormGraph, FormGraph);
  Application.Run;
end.
