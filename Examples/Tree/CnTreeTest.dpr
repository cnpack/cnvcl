program CnTreeTest;

uses
  Forms,
  CnTreeTestUnit in 'CnTreeTestUnit.pas' {CnTreeTestForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCnTreeTestForm, CnTreeTestForm);
  Application.Run;
end.
