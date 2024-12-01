program Prime;

uses
  Forms,
  UnitPrime in 'UnitPrime.pas' {FormPrime},
  CnPrime in '..\..\..\Source\Crypto\CnPrime.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrime, FormPrime);
  Application.Run;
end.
