program DNS;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitDNS in 'UnitDNS.pas' {FormDNS};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDNS, FormDNS);
  Application.Run;
end.
