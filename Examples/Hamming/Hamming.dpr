program Hamming;

uses
  Forms,
  UnitHamming in 'UnitHamming.pas' {FormHamming},
  CnHamming in '..\..\Source\Common\CnHamming.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormHamming, FormHamming);
  Application.Run;
end.
