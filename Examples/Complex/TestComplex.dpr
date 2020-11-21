program TestComplex;

uses
  Forms,
  UnitComplex in 'UnitComplex.pas' {FormComplex},
  CnComplex in '..\..\Source\Common\CnComplex.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormComplex, FormComplex);
  Application.Run;
end.
