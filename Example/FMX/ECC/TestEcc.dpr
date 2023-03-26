program TestEcc;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitEcc in 'UnitEcc.pas' {FormEcc};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEcc, FormEcc);
  Application.Run;
end.
