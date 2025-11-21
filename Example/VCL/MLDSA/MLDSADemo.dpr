program MLDSADemo;

uses
  Forms,
  MLDSAMainFrm in 'MLDSAMainFrm.pas' {MLDSAMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MLDSA Demo';
  Application.CreateForm(TMLDSAMainForm, MLDSAMainForm);
  Application.Run;
end.