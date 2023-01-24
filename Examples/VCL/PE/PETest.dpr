program PETest;

uses
  Forms,
  UnitPE in 'UnitPE.pas' {FormPE},
  CnPE in '..\..\..\Source\Common\CnPE.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPE, FormPE);
  Application.Run;
end.
