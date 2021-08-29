program Project2;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {FormCrypt},
  CnMD5 in '..\..\Source\Crypto\CnMD5.pas',
  CnDES in '..\..\Source\Crypto\CnDES.pas',
  CnAES in '..\..\Source\Crypto\CnAES.pas',
  CnCRC32 in '..\..\Source\Crypto\CnCRC32.pas',
  CnBase64 in '..\..\Source\Crypto\CnBase64.pas',
  CnTEA in '..\..\Source\Crypto\CnTEA.pas',
  CnZUC in '..\..\Source\Crypto\CnZUC.pas',
  CnSM3 in '..\..\Source\Crypto\CnSM3.pas',
  CnSM4 in '..\..\Source\Crypto\CnSM4.pas',
  CnSHA1 in '..\..\Source\Crypto\CnSHA1.pas',
  CnSHA2 in '..\..\Source\Crypto\CnSHA2.pas',
  CnSHA3 in '..\..\Source\Crypto\CnSHA3.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCrypt, FormCrypt);
  Application.Run;
end.
