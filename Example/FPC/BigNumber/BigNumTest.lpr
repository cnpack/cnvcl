program BigNumTest;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitMain in 'UnitMain.pas' {FormBigNumber};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigNumber, FormBigNumber);
  Application.Run;
end.
