program TCPClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitTCPClient in 'UnitTCPClient.pas' {FormTCPClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTCPClient, FormTCPClient);
  Application.Run;
end.
