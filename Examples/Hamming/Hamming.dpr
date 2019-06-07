program Hamming;

uses
  Forms,
  UnitHamming in 'UnitHamming.pas' {FormHamming},
  CnFEC in '..\..\Source\Common\CnFEC.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormHamming, FormHamming);
  Application.Run;
end.
