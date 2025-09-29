program OutPut;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitOutput in 'UnitOutput.pas' {FormSend},
  CnDebug in '..\..\..\Source\Common\CnDebug.pas',
  UnitThread in 'UnitThread.pas',
  CnSampleComponent in '..\..\..\..\cnwizards\Source\Example\CnSampleComponent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSend, FormSend);
  Application.Run;
end.
