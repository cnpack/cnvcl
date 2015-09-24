program EventBus;

uses
  Forms,
  UnitEventBus in 'UnitEventBus.pas' {EventBusForm},
  CnEventBus in '..\..\Source\Common\CnEventBus.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TEventBusForm, EventBusForm);
  Application.Run;
end.
