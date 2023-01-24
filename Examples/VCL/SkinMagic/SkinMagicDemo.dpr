program SkinMagicDemo;

uses
  Forms,
  CnSkinMagic_Sample in 'CnSkinMagic_Sample.pas',
  MainFrm in 'MainFrm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
