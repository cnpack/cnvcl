program CPUSin;

uses
  Forms,
  UnitCPUUsage in 'UnitCPUUsage.pas' {FormSin};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSin, FormSin);
  Application.Run;
end.
