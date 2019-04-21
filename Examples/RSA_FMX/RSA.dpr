program RSA;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitRSA in 'UnitRSA.pas' {FormRSA};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRSA, FormRSA);
  Application.Run;
end.
