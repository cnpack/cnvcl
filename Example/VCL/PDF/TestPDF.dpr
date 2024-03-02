program TestPDF;

uses
  Forms,
  UnitPdf in 'UnitPdf.pas' {FormPDF},
  CnPDF in '..\..\..\Source\Common\CnPDF.pas',
  CnPDFCrypt in '..\..\..\Source\Crypto\CnPDFCrypt.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPDF, FormPDF);
  Application.Run;
end.
