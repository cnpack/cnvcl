program BigNumTest;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {FormBigNumber},
  CnBigNumber in '..\..\Source\Common\CnBigNumber.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigNumber, FormBigNumber);
  Application.Run;
end.
