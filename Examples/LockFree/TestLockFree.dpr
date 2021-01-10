program TestLockFree;

uses
  Forms,
  UnitLockFree in 'UnitLockFree.pas' {FormLockFree},
  CnLockFree in '..\..\Source\Common\CnLockFree.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormLockFree, FormLockFree);
  Application.Run;
end.
