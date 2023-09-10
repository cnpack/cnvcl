program TestLattice;

uses
  Forms,
  UnitLattice in 'UnitLattice.pas' {FormLattice},
  CnVector in '..\..\..\Source\Crypto\CnVector.pas',
  CnLattice in '..\..\..\Source\Crypto\CnLattice.pas',
  CnBits in '..\..\..\Source\Crypto\CnBits.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormLattice, FormLattice);
  Application.Run;
end.
