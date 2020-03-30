program KDFTest;

uses
  Forms,
  UnitkKeyDF in 'UnitkKeyDF.pas' {Form1},
  CnKeyDerivation in '..\..\Source\Common\CnKeyDerivation.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
