program DNS;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitDNS in 'UnitDNS.pas' {FormDNS},
  CnDNS in '..\..\..\Source\NetComm\CnDNS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDNS, FormDNS);
  Application.Run;
end.
