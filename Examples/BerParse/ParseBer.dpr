program ParseBer;

uses
  Forms,
  UnitBer in 'UnitBer.pas' {FormParseBer},
  CnBerUtils in '..\..\Source\Common\CnBerUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormParseBer, FormParseBer);
  Application.Run;
end.
