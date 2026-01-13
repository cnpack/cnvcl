program TLSServer;

uses
  Forms,
  UnitTLSServer in 'UnitTLSServer.pas' {FormTLSServer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTLSServer, FormTLSServer);
  Application.Run;
end.

