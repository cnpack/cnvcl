program TCPClient;

uses
  Forms,
  UnitTCPClient in 'UnitTCPClient.pas' {FormTCPClient},
  CnTCPClient in '..\..\Source\NetComm\CnTCPClient.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCPClient, FormTCPClient);
  Application.Run;
end.
