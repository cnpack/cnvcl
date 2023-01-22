program TestDFT;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitDFT in 'UnitDFT.pas' {FormDFT};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDFT, FormDFT);
  Application.Run;
end.
