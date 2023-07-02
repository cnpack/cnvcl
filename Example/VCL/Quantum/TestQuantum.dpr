program TestQuantum;

uses
  Forms,
  UnitQuantum in 'UnitQuantum.pas' {FormQuantum},
  CnQuantum in '..\..\..\Source\Common\CnQuantum.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormQuantum, FormQuantum);
  Application.Run;
end.
