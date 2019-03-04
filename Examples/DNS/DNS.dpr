program DNS;

uses
  Forms,
  UnitDNS in 'UnitDNS.pas' {FormDNS},
  CnDNS in '..\..\Source\NetComm\CnDNS.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormDNS, FormDNS);
  Application.Run;
end.
