program SLHDSADemo;

uses
  Forms,
  SLHDSAMainFrm in 'SLHDSAMainFrm.pas' {SLHDSAMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SLH-DSA Demo';
  Application.CreateForm(TSLHDSAMainForm, SLHDSAMainForm);
  Application.Run;
end.
