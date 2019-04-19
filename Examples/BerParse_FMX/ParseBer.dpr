program ParseBer;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitBer in 'UnitBer.pas' {FormParseBer},
  CnBerUtils in '..\..\Source\Common\CnBerUtils.pas',
  CnTree in '..\..\Source\Common\CnTree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormParseBer, FormParseBer);
  Application.Run;
end.
