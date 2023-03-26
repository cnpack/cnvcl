program Project1;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {FormFloat},
  CnFloat in '..\..\Source\Common\CnFloat.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFloat, FormFloat);
  Application.Run;
end.
