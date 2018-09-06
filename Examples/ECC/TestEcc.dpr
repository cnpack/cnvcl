program TestEcc;

uses
  Forms,
  UnitEcc in 'UnitEcc.pas' {FormEcc},
  CnECC in '..\..\Source\Common\CnECC.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormEcc, FormEcc);
  Application.Run;
end.
