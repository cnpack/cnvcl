program HashTest;

uses
  Forms,
  fMainUnit in 'fMainUnit.pas' {Form3},
  CnHashTable in '..\..\Source\Common\CnHashTable.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
