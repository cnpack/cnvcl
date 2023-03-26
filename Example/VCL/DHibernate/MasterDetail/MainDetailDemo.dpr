program MainDetailDemo;

uses
  Forms,
  frmMain in 'frmMain.pas' {FormMainDetailDemo},
  PODO_DETAILTABLE in 'PODO\PODO_DETAILTABLE.pas',
  PODO_IDG in 'PODO\PODO_IDG.pas',
  PODO_MAINTABLE in 'PODO\PODO_MAINTABLE.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainDetailDemo, FormMainDetailDemo);
  Application.Run;
end.
