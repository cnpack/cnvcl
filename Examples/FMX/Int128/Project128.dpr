program Project128;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit128 in 'Unit128.pas' {Form128};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm128, Form128);
  Application.Run;
end.
