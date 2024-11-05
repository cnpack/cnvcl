program SM2Test;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitSM2 in 'UnitSM2.pas' {FormSM2},
  CnSM2 in '..\..\..\Source\Crypto\CnSM2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSM2, FormSM2);
  Application.Run;
end.
