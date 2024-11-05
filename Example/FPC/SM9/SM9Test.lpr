program SM9Test;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitSM9 in 'UnitSM9.pas' {FormSM9},
  CnSM9 in '..\..\..\Source\Crypto\CnSM9.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSM9, FormSM9);
  Application.Run;
end.
