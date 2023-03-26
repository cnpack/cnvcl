program Test25519;

uses
  Forms,
  Unit25519 in 'Unit25519.pas' {Form25519},
  Cn25519 in '..\..\..\Source\Crypto\Cn25519.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm25519, Form25519);
  Application.Run;
end.
