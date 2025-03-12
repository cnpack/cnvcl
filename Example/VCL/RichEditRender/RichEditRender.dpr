program RichEditRender;

uses
  Forms,
  UnitRender in 'UnitRender.pas' {FormRender},
  CnRichEdit in '..\..\..\Source\Graphics\CnRichEdit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRender, FormRender);
  Application.Run;
end.
