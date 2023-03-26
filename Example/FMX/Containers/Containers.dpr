program Containers;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitContainer in 'UnitContainer.pas' {FormContainers};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormContainers, FormContainers);
  Application.Run;
end.
