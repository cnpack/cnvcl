program Matrix;

uses
  Forms,
  UnitMatrix in 'UnitMatrix.pas' {FormMatrix},
  CnMatrix in '..\..\..\Source\Common\CnMatrix.pas',
  CnFEC in '..\..\..\Source\Crypto\CnFEC.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMatrix, FormMatrix);
  Application.Run;
end.
