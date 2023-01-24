program Project128;

uses
  Forms,
  Unit128 in 'Unit128.pas' {Form128},
  CnInt128 in '..\..\..\Source\Crypto\CnInt128.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm128, Form128);
  Application.Run;
end.
