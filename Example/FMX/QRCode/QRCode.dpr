program QRCode;

uses
  FMX.Forms,
  UnitQRCode in 'UnitQRCode.pas' {FormQRTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormQRTest, FormQRTest);
  Application.Run;
end.