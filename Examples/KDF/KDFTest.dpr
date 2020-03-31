program KDFTest;

uses
  Forms,
  UnitkKeyDF in 'UnitkKeyDF.pas' {FormKDF},
  CnKDF in '..\..\Source\Common\CnKDF.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormKDF, FormKDF);
  Application.Run;
end.
