program BigDecimal;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitDecimal in 'UnitDecimal.pas' {FormBigDecimal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigDecimal, FormBigDecimal);
  Application.Run;
end.
