program TestOTS;

uses
  Forms,
  UnitOTS in 'UnitOTS.pas' {FormOTS},
  CnOTS in '..\..\..\Source\Crypto\CnOTS.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormOTS, FormOTS);
  Application.Run;
end.
