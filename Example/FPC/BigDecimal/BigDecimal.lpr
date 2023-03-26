program BigDecimal;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitDecimal in 'UnitDecimal.pas' {FormBigDecimal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigDecimal, FormBigDecimal);
  Application.Run;
end.
