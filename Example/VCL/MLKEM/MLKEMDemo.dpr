program MLKEMDemo;

uses
  Forms,
  MLKEMMainFrm in 'MLKEMMainFrm.pas' {MLKEMMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MLKEM Demo';
  Application.CreateForm(TMLKEMMainForm, MLKEMMainForm);
  Application.Run;
end.