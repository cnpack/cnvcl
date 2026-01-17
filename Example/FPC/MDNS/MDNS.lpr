program MDNS;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitMDNS in 'UnitMDNS.pas' {FormMDNS};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMDNS, FormMDNS);
  Application.Run;
end.
