program RSA;

uses
  Forms,
  UnitRSA in 'UnitRSA.pas' {FormRSA},
  CnRSA in '..\..\Source\CnRSA.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRSA, FormRSA);
  Application.Run;
end.
