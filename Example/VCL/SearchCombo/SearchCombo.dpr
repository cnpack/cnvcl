program SearchCombo;

uses
  Forms,
  UnitSearchCombo in 'UnitSearchCombo.pas' {FormSearchCombo},
  CnSearchCombo in '..\..\..\Source\Graphic\CnSearchCombo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSearchCombo, FormSearchCombo);
  Application.Run;
end.
