program BigNumTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {FormBigNumber},
  CnBigNumber in '..\..\Source\Crypto\CnBigNumber.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigNumber, FormBigNumber);
  Application.Run;
end.
