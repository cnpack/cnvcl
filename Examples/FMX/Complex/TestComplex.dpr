program TestComplex;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitComplex in 'UnitComplex.pas' {FormComplex};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormComplex, FormComplex);
  Application.Run;
end.
