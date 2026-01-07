program VirtualControlDemo;

uses
  Forms,
  UnitVirtualControl in 'UnitVirtualControl.pas' {frmMain},
  CnVirtualControl in '..\..\..\Source\Graphic\CnVirtualControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

