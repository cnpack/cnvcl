program MLDSADemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  MLDSAMainFrm in 'MLDSAMainFrm.pas' {MLDSAMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMLDSAMainForm, MLDSAMainForm);
  Application.Run;
end.