program TabSet;

uses
  Forms,
  TabSetUnit in 'TabSetUnit.pas' {FormTabSet},
  CnTabSet in '..\..\Source\Graphics\CnTabSet.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTabSet, FormTabSet);
  Application.Run;
end.
