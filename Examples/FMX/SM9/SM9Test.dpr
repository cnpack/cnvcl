program SM9Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitSM9 in 'UnitSM9.pas' {FormSM9};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSM9, FormSM9);
  Application.Run;
end.
