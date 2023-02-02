program Calculator;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitCalc in 'UnitCalc.pas' {FormCalc};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCalc, FormCalc);
  Application.Run;
end.
