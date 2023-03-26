program BigRational;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitBigRational in 'UnitBigRational.pas' {FormRational};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRational, FormRational);
  Application.Run;
end.
