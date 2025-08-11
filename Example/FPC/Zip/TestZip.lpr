program TestZip;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  UnitZip in 'UnitZip.pas' {FormZip},
  CnZip in '..\..\..\Source\Common\CnZip.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormZip, FormZip);
  Application.Run;
end.
