program Test25519;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit25519 in 'Unit25519.pas' {Form25519},
  Cn25519 in '..\..\..\Source\Crypto\Cn25519.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm25519, Form25519);
  Application.Run;
end.
