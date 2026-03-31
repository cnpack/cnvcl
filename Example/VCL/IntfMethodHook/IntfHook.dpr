program IntfHook;

uses
  Forms,
  UnitIntfHookMain in 'UnitIntfHookMain.pas' {IntfHookForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIntfHookForm, IntfHookForm);
  Application.Run;
end.
