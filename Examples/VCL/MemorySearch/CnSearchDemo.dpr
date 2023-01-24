program CnSearchDemo;

uses
  Forms,
  SearchMainFrm in 'SearchMainFrm.pas' {Form1},
  CnMemorySearch in '..\..\..\Source\NonVisual\CnMemorySearch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
