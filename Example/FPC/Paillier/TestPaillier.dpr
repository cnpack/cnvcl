program TestPaillier;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  UnitPaillier in 'UnitPaillier.pas' {FormPaillier},
  CnPaillier in '..\..\..\Source\Crypto\CnPaillier.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPaillier, FormPaillier);
  Application.Run;
end.
