program CnTreeTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  CnTreeTestUnit in 'CnTreeTestUnit.pas' {CnTreeTestForm},
  UnitBinaryTree in 'UnitBinaryTree.pas' {FormBinaryTree};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCnTreeTestForm, CnTreeTestForm);
  Application.CreateForm(TFormBinaryTree, FormBinaryTree);
  Application.Run;
end.
