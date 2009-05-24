program MemPoolTest;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  CnIocpSimpleMemPool in '..\..\Source\NetComm\CnIocpSimpleMemPool.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
