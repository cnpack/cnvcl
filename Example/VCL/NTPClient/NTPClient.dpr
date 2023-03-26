program NTPClient;

uses
  Forms,
  UnitNTPClient in 'UnitNTPClient.pas' {FormNTP};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormNTP, FormNTP);
  Application.Run;
end.
