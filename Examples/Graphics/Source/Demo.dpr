program Demo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'CnPack图像处理库演示程序 V0.11Alpha';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
