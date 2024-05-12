program BigRational;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitBigRational in 'UnitBigRational.pas' {FormRational},
  CnBigRational in '..\..\..\Source\Common\CnBigRational.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRational, FormRational);
  Application.Run;
end.
