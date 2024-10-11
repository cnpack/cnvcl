program TestDSA;

uses
  Forms,
  UnitDSA in 'UnitDSA.pas' {FormDSA},
  CnDSA in '..\..\..\Source\Crypto\CnDSA.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormDSA, FormDSA);
  Application.Run;
end.
