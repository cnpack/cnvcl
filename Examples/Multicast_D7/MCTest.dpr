program MCTest;

uses
  Forms,
  FMainUnit in 'FMainUnit.pas' {frmMain},
  CnMulticastEvent in '..\..\Source\Common\CnMulticastEvent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
