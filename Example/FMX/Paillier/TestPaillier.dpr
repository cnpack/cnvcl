program TestPaillier;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitPaillier in 'UnitPaillier.pas' {FormPaillier};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPaillier, FormPaillier);
  Application.Run;
end.
