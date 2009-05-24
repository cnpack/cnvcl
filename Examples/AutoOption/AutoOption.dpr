program AutoOption;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CnAutoOption in '..\..\Source\Graphics\CnAutoOption.pas',
  CnAOTreeView in '..\..\Source\Graphics\CnAOTreeView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
