program TestZip;

uses
  Forms,
  UnitZip in 'UnitZip.pas' {FormZip},
  CnZip in '..\..\..\Source\Common\CnZip.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormZip, FormZip);
  Application.Run;
end.
