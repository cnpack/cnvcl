program TestLang;

uses
  Forms,
  UnitLang in 'UnitLang.pas' {FormLang},
  CnLangUtils in '..\..\Source\MultiLang\CnLangUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormLang, FormLang);
  Application.Run;
end.
