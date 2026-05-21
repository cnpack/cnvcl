program SLHDSADemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  SLHDSAMainFrm in 'SLHDSAMainFrm.pas' {SLHDSAMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSLHDSAMainForm, SLHDSAMainForm);
  Application.Run;
end.
