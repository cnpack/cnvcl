program Watermark;

uses
  Forms,
  UnitWatermark in 'UnitWatermark.pas' {FormWatermark};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormWatermark, FormWatermark);
  Application.Run;
end.
