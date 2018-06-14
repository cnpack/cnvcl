program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormCrypt},
  CnMD5 in '..\..\Source\Common\CnMD5.pas',
  CnBase64 in '..\..\Source\Common\CnBase64.pas',
  CnCRC32 in '..\..\Source\Common\CnCRC32.pas',
  CnDES in '..\..\Source\Common\CnDES.pas',
  CnSHA1 in '..\..\Source\Common\CnSHA1.pas',
  CnSM3 in '..\..\Source\Common\CnSM3.pas',
  CnSM4 in '..\..\Source\Common\CnSM4.pas',
  CnAES in '..\..\Source\Common\CnAES.pas',
  CnSHA2 in '..\..\Source\Common\CnSHA2.pas',
  CnZUC in '..\..\Source\Common\CnZUC.pas',
  CnSHA3 in '..\..\Source\Common\CnSHA3.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCrypt, FormCrypt);
  Application.Run;
end.
