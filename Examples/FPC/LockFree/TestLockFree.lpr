program TestLockFree;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitLockFree in 'UnitLockFree.pas' {FormLockFree},
  CnLockFree in '..\..\Source\Common\CnLockFree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormLockFree, FormLockFree);
  Application.Run;
end.
