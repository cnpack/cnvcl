program Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {FrmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
