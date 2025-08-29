program SearchCombo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  UnitSearchCombo in 'UnitSearchCombo.pas' {FormSearchCombo},
  CnSearchCombo in '..\..\..\Source\Graphic\CnSearchCombo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSearchCombo, FormSearchCombo);
  Application.Run;
end.
