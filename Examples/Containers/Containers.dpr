program Containers;

uses
  Forms,
  UnitContainer in 'UnitContainer.pas' {FormContainers},
  CnQueue in '..\..\Source\Common\CnQueue.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormContainers, FormContainers);
  Application.Run;
end.
