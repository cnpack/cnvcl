program RedisClient;

uses
  Forms,
  uMainFrm in 'uMainFrm.pas' {MainFrm},
  CnRedisClient in '..\..\..\Source\NetComm\CnRedisClient.pas';

{$R *.res}

begin
  Application.Initialize;
  // Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
