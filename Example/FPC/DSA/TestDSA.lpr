program TestDSA;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitDSA in 'UnitDSA.pas' {FormDSA},
  CnDSA in '..\..\..\Source\Crypto\CnDSA.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDSA, FormDSA);
  Application.Run;
end.
