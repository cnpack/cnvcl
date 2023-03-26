program SecretSharing;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitSecretSharing in 'UnitSecretSharing.pas' {FormSecretSharing};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSecretSharing, FormSecretSharing);
  Application.Run;
end.
