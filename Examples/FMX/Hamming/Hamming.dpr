program Hamming;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitHamming in 'UnitHamming.pas' {FormHamming};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHamming, FormHamming);
  Application.Run;
end.
