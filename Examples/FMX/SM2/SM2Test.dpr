program SM2Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitSM2 in 'UnitSM2.pas' {FormSM2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSM2, FormSM2);
  Application.Run;
end.
