program Calculator;

uses
  Forms,
  UnitCalc in 'UnitCalc.pas' {FormCalc};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCalc, FormCalc);
  Application.Run;
end.
