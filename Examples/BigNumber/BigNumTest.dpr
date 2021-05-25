program BigNumTest;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {FormBigNumber},
  CnBigNumber in '..\..\Source\Crypto\CnBigNumber.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigNumber, FormBigNumber);
  Application.Run;
end.
