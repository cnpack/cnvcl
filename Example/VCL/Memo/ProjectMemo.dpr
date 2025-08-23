program ProjectMemo;

uses
  Forms,
  UnitMemo in 'UnitMemo.pas' {CnMemoForm},
  CnMemo in '..\..\..\Source\Graphic\CnMemo.pas',
  CnTextControl in '..\..\..\Source\Graphic\CnTextControl.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCnMemoForm, CnMemoForm);
  Application.Run;
end.
