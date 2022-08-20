program OutPut;

uses
  Forms,
  UnitOutput in 'UnitOutput.pas' {FormSend},
  CnDebug in '..\..\Source\Common\CnDebug.pas',
  UnitThread in 'UnitThread.pas',
  CnRTL in '..\..\Source\Common\CnRTL.pas',
  CnSampleComponent in '..\..\..\cnwizards\Source\Examples\CnSampleComponent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSend, FormSend);
  Application.Run;
end.
