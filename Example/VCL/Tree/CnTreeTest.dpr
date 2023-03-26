program CnTreeTest;

uses
  Forms,
  CnTreeTestUnit in 'CnTreeTestUnit.pas' {CnTreeTestForm},
  UnitBinaryTree in 'UnitBinaryTree.pas' {FormBinaryTree},
  CnTree in '..\..\..\Source\Common\CnTree.pas',
  CnTreeClasses in '..\..\..\Source\Common\CnTreeClasses.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCnTreeTestForm, CnTreeTestForm);
  Application.Run;
end.
