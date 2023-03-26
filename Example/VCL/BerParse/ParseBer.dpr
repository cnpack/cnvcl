program ParseBer;

uses
  Forms,
  UnitBer in 'UnitBer.pas' {FormParseBer},
  CnBerUtils in '..\..\..\Source\Crypto\CnBerUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormParseBer, FormParseBer);
  Application.Run;
end.
