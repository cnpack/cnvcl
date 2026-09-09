program TestMarkDown;

uses
  Forms,
  UnitMarkDown in 'UnitMarkDown.pas' {FormMarkDown},
  CnMarkDown in '..\..\..\Source\Common\CnMarkDown.pas',
  CnMarkDownView in '..\..\..\Source\Graphic\CnMarkDownView.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMarkDown, FormMarkDown);
  Application.Run;
end.
