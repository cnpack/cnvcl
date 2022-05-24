program SecretSharing;

uses
  Forms,
  UnitSecretSharing in 'UnitSecretSharing.pas' {FormSecretSharing},
  CnSecretSharing in '..\..\Source\Crypto\CnSecretSharing.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSecretSharing, FormSecretSharing);
  Application.Run;
end.
