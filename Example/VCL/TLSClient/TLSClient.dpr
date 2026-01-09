program TLSClient;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
