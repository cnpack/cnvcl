program CnTreeTest;

uses
  Forms,
  CnTreeTestUnit in 'CnTreeTestUnit.pas' {CnTreeTestForm},
  UnitBinaryTree in 'UnitBinaryTree.pas' {FormBinaryTree},
  CnTree in '..\..\Source\Common\CnTree.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCnTreeTestForm, CnTreeTestForm);
  Application.Run;
end.
