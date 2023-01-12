program BigNumTest;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {FormBigNumber},
  CnBigNumber in '..\..\Source\Crypto\CnBigNumber.pas',
  UnitBatchTest in 'UnitBatchTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigNumber, FormBigNumber);
  Application.Run;
end.
