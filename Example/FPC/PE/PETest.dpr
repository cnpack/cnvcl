program PETest;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  UnitPE in 'UnitPE.pas' {FormPE},
  CnPE in '..\..\..\Source\Common\CnPE.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPE, FormPE);
  Application.Run;
end.
