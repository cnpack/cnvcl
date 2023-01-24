program CpuID;

uses
  Forms,
  uCpuIDMain in 'uCpuIDMain.pas' {FrmCPUIDs};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmCPUIDs, FrmCPUIDs);
  Application.Run;
end.
