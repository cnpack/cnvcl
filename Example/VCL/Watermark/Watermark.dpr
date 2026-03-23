program Watermark;

uses
  Forms,
  UnitWatermark in 'UnitWatermark.pas' {FormWatermark},
  CnWatermark in '..\..\..\Source\Graphic\CnWatermark.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormWatermark, FormWatermark);
  Application.Run;
end.
