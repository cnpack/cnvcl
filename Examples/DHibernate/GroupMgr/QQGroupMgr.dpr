program QQGroupMgr;

uses
  Forms,
  frmMain in 'frmMain.pas' {FormMain},
  PODO_WARNINGS in 'PODO\PODO_WARNINGS.pas',
  PODO_CONSTANTS in 'PODO\PODO_CONSTANTS.pas',
  PODO_MEMBERS in 'PODO\PODO_MEMBERS.pas',
  PODO_RESEARCHS in 'PODO\PODO_RESEARCHS.pas',
  frmAddOrEdit in 'frmAddOrEdit.pas' {FormAddOrEdit},
  frmAddWarning in 'frmAddWarning.pas' {FormAddWarning},
  frmAddResearch in 'frmAddResearch.pas' {FormAddReseach},
  frmViewWarning in 'frmViewWarning.pas' {FormViewWarning},
  frmViewResearch in 'frmViewResearch.pas' {FormViewResearch};

{$R *.res}

begin
  Application.Initialize;
  // Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
