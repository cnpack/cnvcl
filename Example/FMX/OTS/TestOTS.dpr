program TestOTS;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitOTS in 'UnitOTS.pas' {FormOTS},
  CnOTS in '..\..\..\Source\Crypto\CnOTS.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormOTS, FormOTS);
  Application.Run;
end.
