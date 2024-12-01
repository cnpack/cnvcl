program Prime;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitPrime in 'UnitPrime.pas' {FormPrime},
  CnPrime in '..\..\..\Source\Crypto\CnPrime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrime, FormPrime);
  Application.Run;
end.
