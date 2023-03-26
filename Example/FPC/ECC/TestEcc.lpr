program TestEcc;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitEcc in 'UnitEcc.pas' {FormEcc};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEcc, FormEcc);
  Application.Run;
end.
