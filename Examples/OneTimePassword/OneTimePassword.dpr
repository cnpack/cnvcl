program OneTimePassword;

uses
  Forms,
  UnitOneTimePassword in 'UnitOneTimePassword.pas' {FormOneTimePassword},
  CnOneTimePassword in '..\..\Source\Crypto\CnOneTimePassword.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormOneTimePassword, FormOneTimePassword);
  Application.Run;
end.
