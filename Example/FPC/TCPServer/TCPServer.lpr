program TCPServer;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitTCPServer in 'UnitTCPServer.pas' {FormTCPServer},
  CnThreadingTCPServer in '..\..\..\Source\NetComm\CnThreadingTCPServer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCPServer, FormTCPServer);
  Application.Run;
end.
