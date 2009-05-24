program LEDSample;

uses
  Forms,
  UFrmMain in 'UFrmMain.pas' {FrmMain},
  CnLED in '..\..\Source\Graphics\CnLED.pas',
  CnAutoOption in '..\..\Source\Graphics\CnAutoOption.pas',
  CnAOTreeView in '..\..\Source\Graphics\CnAOTreeView.pas',
  CnSpin in '..\..\Source\Graphics\CnSpin.pas',
  CnGraphConsts in '..\..\Source\Graphics\CnGraphConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
