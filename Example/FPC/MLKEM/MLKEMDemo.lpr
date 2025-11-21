program MLKEMDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  MLKEMMainFrm in 'MLKEMMainFrm.pas' {MLKEMMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMLKEMMainForm, MLKEMMainForm);
  Application.Run;
end.