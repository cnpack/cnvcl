program GDIPlusTest;

uses
  Forms,
  UnitGDIPlusTest in 'UnitGDIPlusTest.pas' {FormGDIPlus},
  CnGraphUtils in '..\..\..\Source\Common\CnGraphUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormGDIPlus, FormGDIPlus);
  Application.Run;
end.
