program RtlUtils;

uses
  Forms,
  UnitRtlUtils in 'UnitRtlUtils.pas' {FormRtlUtils},
  CnRtlUtils in '..\..\Source\Common\CnRtlUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormRtlUtils, FormRtlUtils);
  Application.Run;
end.
