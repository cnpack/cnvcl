program Test25519;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit25519 in 'Unit25519.pas' {Form25519};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm25519, Form25519);
  Application.Run;
end.
