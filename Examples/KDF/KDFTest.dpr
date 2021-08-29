program KDFTest;

uses
  Forms,
  UnitkKeyDF in 'UnitkKeyDF.pas' {FormKDF},
  CnKDF in '..\..\Source\Crypto\CnKDF.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormKDF, FormKDF);
  Application.Run;
end.
