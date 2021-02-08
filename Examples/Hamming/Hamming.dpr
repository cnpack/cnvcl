program Hamming;

uses
  Forms,
  UnitHamming in 'UnitHamming.pas' {FormHamming},
  CnFEC in '..\..\Source\Crypto\CnFEC.pas',
  CnMatrix in '..\..\Source\Common\CnMatrix.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormHamming, FormHamming);
  Application.Run;
end.
