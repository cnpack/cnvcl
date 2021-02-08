program RSA;

uses
  Forms,
  UnitRSA in 'UnitRSA.pas' {FormRSA},
  CnRSA in '..\..\Source\Crypto\CnRSA.pas',
  CnPemUtils in '..\..\Source\Crypto\CnPemUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRSA, FormRSA);
  Application.Run;
end.
