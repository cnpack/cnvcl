program Prime;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitPrime in 'UnitPrime.pas' {FormPrime},
  CnPrimeNumber in '..\..\..\Source\Crypto\CnPrimeNumber.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrime, FormPrime);
  Application.Run;
end.
