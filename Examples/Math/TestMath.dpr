program TestMath;

uses
  Forms,
  UnitMath in 'UnitMath.pas' {FormMath},
  CnMath in '..\..\Source\Common\CnMath.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMath, FormMath);
  Application.Run;
end.
