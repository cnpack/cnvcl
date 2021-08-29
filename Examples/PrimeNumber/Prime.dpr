program Prime;

uses
  Forms,
  UnitPrime in 'UnitPrime.pas' {FormPrime},
  CnPrimeNumber in '..\..\Source\Crypto\CnPrimeNumber.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrime, FormPrime);
  Application.Run;
end.
