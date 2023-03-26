program OutPut;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitOutput in 'UnitOutput.pas' {FormSend},
  UnitThread in 'UnitThread.pas',
  CnSampleComponent in 'CnSampleComponent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSend, FormSend);
  Application.Run;
end.
