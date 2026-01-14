program GenQRCode;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitGenQRCode in 'UnitGenQRCode.pas' {FormQRTest},
  CnQRCode in '..\..\..\Source\Crypto\CnQRCode.pas',
  CnQRImage in '..\..\..\Source\Graphic\CnQRImage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormQRTest, FormQRTest);
  Application.Run;
end.
