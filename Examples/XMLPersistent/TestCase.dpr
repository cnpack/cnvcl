program TestCase;

uses
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  w_XMLPersistentTestCase in 'w_XMLPersistentTestCase.pas',
  w_PivotUnit in 'w_PivotUnit.pas',
  w_StreamUnitTest in 'w_StreamUnitTest.pas',
  w_frmTestcase in 'w_frmTestcase.pas' {frmTest},
  w_PersistentClassSample in 'w_PersistentClassSample.pas',
  CnXMLPersistent in '..\..\Source\Common\CnXMLPersistent.pas',
  CnDynObjBuilder in '..\..\Source\Common\CnDynObjBuilder.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

