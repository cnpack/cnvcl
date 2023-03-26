program TestMath;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMath in 'UnitMath.pas' {FormMath};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMath, FormMath);
  Application.Run;
end.
