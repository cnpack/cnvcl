program Demo;

uses
  Forms,
  Main in 'Main.pas' {FrmMain},
  CnLinkedList in '..\..\Source\Common\CnLinkedList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
