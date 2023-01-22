program KDFTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitkKeyDF in 'UnitkKeyDF.pas' {FormKDF};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormKDF, FormKDF);
  Application.Run;
end.
