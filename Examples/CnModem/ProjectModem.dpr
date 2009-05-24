program ProjectModem;

uses
  Forms,
  uFrmModem in 'uFrmModem.pas' {FrmModem};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmModem, FrmModem);
  Application.Run;
end.
