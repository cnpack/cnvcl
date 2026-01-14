program MDNS;

uses
  Forms,
  UnitMDNS in 'UnitMDNS.pas' {FormMDNS};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMDNS, FormMDNS);
  Application.Run;
end.
