program Mdiapp;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Childwin in 'ChildWin.pas' {MDIChild};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
