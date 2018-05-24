program ParseBer;

uses
  Forms,
  UnitBer in 'UnitBer.pas' {FormParseBer},
  CnBerParser in '..\..\Source\Common\CnBerParser.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormParseBer, FormParseBer);
  Application.Run;
end.
