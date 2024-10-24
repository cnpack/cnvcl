program TestDSA;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitDSA in 'UnitDSA.pas' {FormDSA};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDSA, FormDSA);
  Application.Run;
end.
