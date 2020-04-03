program SM2Test;

uses
  Forms,
  UnitSM2 in 'UnitSM2.pas' {FormSM2},
  CnSM2 in '..\..\Source\Common\CnSM2.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSM2, FormSM2);
  Application.Run;
end.
