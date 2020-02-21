program TCPServer;

uses
  Forms,
  UnitTCPServer in 'UnitTCPServer.pas' {FormTCPServer},
  CnThreadingTCPServer in '..\..\Source\NetComm\CnThreadingTCPServer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCPServer, FormTCPServer);
  Application.Run;
end.
