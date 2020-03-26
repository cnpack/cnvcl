program RSA;

uses
  Forms,
  UnitRSA in 'UnitRSA.pas' {FormRSA},
  CnRSA in '..\..\Source\Common\CnRSA.pas',
  CnPemUtils in '..\..\Source\Common\CnPemUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRSA, FormRSA);
  Application.Run;
end.
