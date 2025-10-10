program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormMandelbrot},
  CnMandelbrotImage in '..\..\..\Source\Graphic\CnMandelbrotImage.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMandelbrot, FormMandelbrot);
  Application.Run;
end.
