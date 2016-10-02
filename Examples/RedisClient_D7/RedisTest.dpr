program RedisTest;

uses
  Forms,
  RedisTestMainFrm in 'RedisTestMainFrm.pas' {RedisTestFrm},
  CnRedisClient in '..\..\Source\NetComm\CnRedisClient.pas';

{$R *.res}

begin
  Application.Initialize;
  // Application.MainFormOnTaskbar := True;
  Application.CreateForm(TRedisTestFrm, RedisTestFrm);
  Application.Run;
end.
