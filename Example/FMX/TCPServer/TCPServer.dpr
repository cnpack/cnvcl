program TCPServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitTCPServer in 'UnitTCPServer.pas' {FormTCPServer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCPServer, FormTCPServer);
  Application.Run;
end.
