program OneTimePassword;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitOneTimePassword in 'UnitOneTimePassword.pas' {FormOneTimePassword};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOneTimePassword, FormOneTimePassword);
  Application.Run;
end.
