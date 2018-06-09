program OverrideIntf;

uses
  Forms,
  UnitOverride in 'UnitOverride.pas' {FormTest},
  UnitOrigIntf in 'UnitOrigIntf.pas',
  UnitOrigProvider in 'UnitOrigProvider.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTest, FormTest);
  Application.Run;
end.
