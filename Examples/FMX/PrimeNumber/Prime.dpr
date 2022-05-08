program Prime;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitPrime in 'UnitPrime.pas' {FormPrime};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPrime, FormPrime);
  Application.Run;
end.
