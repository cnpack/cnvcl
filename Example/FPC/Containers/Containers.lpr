program Containers;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitContainer in 'UnitContainer.pas' {FormContainers},
  CnContainers in '..\..\..\Source\Common\CnContainers.pas',
  CnHashMap in '..\..\..\Source\Common\CnHashMap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormContainers, FormContainers);
  Application.Run;
end.
