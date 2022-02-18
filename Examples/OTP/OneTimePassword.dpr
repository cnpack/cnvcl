program OneTimePassword;

uses
  Forms,
  UnitOneTimePassword in 'UnitOneTimePassword.pas' {FormOneTimePassword},
  CnOTP in '..\..\Source\Crypto\CnOTP.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormOneTimePassword, FormOneTimePassword);
  Application.Run;
end.
