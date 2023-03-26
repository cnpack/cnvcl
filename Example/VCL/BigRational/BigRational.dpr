program BigRational;

uses
  Forms,
  UnitBigRational in 'UnitBigRational.pas' {FormRational},
  CnBigRational in '..\..\..\Source\Common\CnBigRational.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRational, FormRational);
  Application.Run;
end.
