program IntfHook;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitIntfHookMain in 'UnitIntfHookMain.pas' {IntfHookForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIntfHookForm, IntfHookForm);
  Application.Run;
end.
