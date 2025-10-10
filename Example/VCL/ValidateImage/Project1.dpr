program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CnSpin in '..\..\..\Source\Graphic\CnSpin.pas',
  CnValidateImage in '..\..\..\Source\Graphic\CnValidateImage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
