program RSA;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitRSA in 'UnitRSA.pas' {FormRSA};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRSA, FormRSA);
  Application.Run;
end.
