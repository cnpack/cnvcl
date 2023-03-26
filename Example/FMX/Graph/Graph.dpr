program Graph;

uses
  System.StartUpCopy,
  FMX.Forms,
  GraphUnit in 'GraphUnit.pas' {FormGraph};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormGraph, FormGraph);
  Application.Run;
end.
