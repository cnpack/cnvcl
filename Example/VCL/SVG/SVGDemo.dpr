program SVGDemo;

uses
  Forms,
  UnitSVGDemo in 'UnitSVGDemo.pas' {FormSVGDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSVGDemo, FormSVGDemo);
  Application.Run;
end.
