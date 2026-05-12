program OneTimePassword;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitOneTimePassword in 'UnitOneTimePassword.pas' {FormOneTimePassword},
  CnOTP in '..\..\..\Source\Crypto\CnOTP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOneTimePassword, FormOneTimePassword);
  Application.Run;
end.
