program TCPClient;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitTCPClient in 'UnitTCPClient.pas' {FormTCPClient},
  CnTCPClient in '..\..\..\Source\NetComm\CnTCPClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCPClient, FormTCPClient);
  Application.Run;
end.
