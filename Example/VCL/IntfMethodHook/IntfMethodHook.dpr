program IntfMethodHook;

uses
  Forms,
  UnitIntfHook in 'UnitIntfHook.pas' {IntfHookForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIntfHookForm, IntfHookForm);
  Application.Run;
end.
