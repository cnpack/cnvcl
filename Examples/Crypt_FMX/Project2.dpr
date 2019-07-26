program Project2;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {FormCrypt},
  CnMD5 in '..\..\Source\Common\CnMD5.pas',
  CnDES in '..\..\Source\Common\CnDES.pas',
  CnAES in '..\..\Source\Common\CnAES.pas',
  CnCRC32 in '..\..\Source\Common\CnCRC32.pas',
  CnBase64 in '..\..\Source\Common\CnBase64.pas',
  CnTEA in '..\..\Source\Common\CnTEA.pas',
  CnZUC in '..\..\Source\Common\CnZUC.pas',
  CnSM3 in '..\..\Source\Common\CnSM3.pas',
  CnSM4 in '..\..\Source\Common\CnSM4.pas',
  CnSHA1 in '..\..\Source\Common\CnSHA1.pas',
  CnSHA2 in '..\..\Source\Common\CnSHA2.pas',
  CnSHA3 in '..\..\Source\Common\CnSHA3.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCrypt, FormCrypt);
  Application.Run;
end.
