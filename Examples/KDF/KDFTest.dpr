program KDFTest;

uses
  Forms,
  UnitkKeyDF in 'UnitkKeyDF.pas' {FormKDF},
  CnKeyDerivation in '..\..\Source\Common\CnKeyDerivation.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormKDF, FormKDF);
  Application.Run;
end.
