program Target;

uses
  Forms,
  TargetMainFrm in 'TargetMainFrm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
