program CnPingDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uCnPingDemo in 'uCnPingDemo.pas' {frmCnPingDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCnPingDemo, frmCnPingDemo);
  Application.Run;
end.
