program SpinEdit;

uses
  Forms,
  SpinEditUnit in 'SpinEditUnit.pas' {FormSpin},
  CnSpin in '..\..\..\Source\Graphic\CnSpin.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSpin, FormSpin);
  Application.Run;
end.
