program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormFloat},
  CnFloat in '..\..\..\Source\Common\CnFloat.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFloat, FormFloat);
  Application.Run;
end.
