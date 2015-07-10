program EventHookTest;

uses
  Forms,
  EventHookUnit in 'EventHookUnit.pas' {FormEventHook},
  CnEventHook in '..\..\Source\Common\CnEventHook.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEventHook, FormEventHook);
  Application.Run;
end.
