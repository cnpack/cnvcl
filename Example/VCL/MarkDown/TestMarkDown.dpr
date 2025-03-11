program TestMarkDown;

uses
  Forms,
  UnitMarkDown in 'UnitMarkDown.pas' {FormMarkDown},
  CnMarkDown in '..\..\..\Source\Common\CnMarkDown.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMarkDown, FormMarkDown);
  Application.Run;
end.
