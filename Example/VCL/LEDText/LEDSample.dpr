program LEDSample;

uses
  Forms,
  UFrmMain in 'UFrmMain.pas' {FrmMain},
  CnLED in '..\..\..\Source\Graphic\CnLED.pas',
  CnAutoOption in '..\..\..\Source\Graphic\CnAutoOption.pas',
  CnAOTreeView in '..\..\..\Source\Graphic\CnAOTreeView.pas',
  CnSpin in '..\..\..\Source\Graphic\CnSpin.pas',
  CnGraphConsts in '..\..\..\Source\Graphic\CnGraphConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
