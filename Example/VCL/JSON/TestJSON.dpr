program TestJSON;

uses
  Forms,
  UnitJSON in 'UnitJSON.pas' {FormJSON},
  CnJSON in '..\..\..\Source\Common\CnJSON.pas',
  CnSampleComponent in '..\..\..\..\cnwizards\Source\Example\CnSampleComponent.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormJSON, FormJSON);
  Application.Run;
end.
