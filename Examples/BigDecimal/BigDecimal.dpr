program BigDecimal;

uses
  Forms,
  UnitDecimal in 'UnitDecimal.pas' {FormBigDecimal},
  CnBigDecimal in '..\..\Source\Common\CnBigDecimal.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigDecimal, FormBigDecimal);
  Application.Run;
end.
