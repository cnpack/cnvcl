program ParseBer;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitBer in 'UnitBer.pas' {FormParseBer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormParseBer, FormParseBer);
  Application.Run;
end.
