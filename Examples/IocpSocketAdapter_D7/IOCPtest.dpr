program IOCPtest;

uses
  Forms,
  FrmMain in 'FrmMain.pas' {Form1},
  CnIocpSocketAdapter in '..\..\Source\NetComm\CnIocpSocketAdapter.pas',
  CnIocpSimpleMemPool in '..\..\Source\NetComm\CnIocpSimpleMemPool.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
