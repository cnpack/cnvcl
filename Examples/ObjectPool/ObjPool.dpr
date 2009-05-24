{.$MINSTACKSIZE $00100000}
{.$MAXSTACKSIZE $01000000}
program ObjPool;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
