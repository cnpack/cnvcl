program AutoOption;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CnAutoOption in '..\..\..\Source\Graphic\CnAutoOption.pas',
  CnAOTreeView in '..\..\..\Source\Graphic\CnAOTreeView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
