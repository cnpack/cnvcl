program SVGDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitSVGDemo in 'UnitSVGDemo.pas' {FormSVGDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSVGDemo, FormSVGDemo);
  Application.Run;
end.
