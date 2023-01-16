program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FormCrypt},
  CnMD5 in '..\..\Source\Crypto\CnMD5.pas',
  CnBase64 in '..\..\Source\Crypto\CnBase64.pas',
  CnCRC32 in '..\..\Source\Crypto\CnCRC32.pas',
  CnDES in '..\..\Source\Crypto\CnDES.pas',
  CnSHA1 in '..\..\Source\Crypto\CnSHA1.pas',
  CnSM3 in '..\..\Source\Crypto\CnSM3.pas',
  CnSM4 in '..\..\Source\Crypto\CnSM4.pas',
  CnAES in '..\..\Source\Crypto\CnAES.pas',
  CnSHA2 in '..\..\Source\Crypto\CnSHA2.pas',
  CnZUC in '..\..\Source\Crypto\CnZUC.pas',
  CnSHA3 in '..\..\Source\Crypto\CnSHA3.pas',
  CnTEA in '..\..\Source\Crypto\CnTEA.pas',
  CnPoly1305 in '..\..\Source\Crypto\CnPoly1305.pas',
  CnChaCha20 in '..\..\Source\Crypto\CnChaCha20.pas',
  CnAEAD in '..\..\Source\Crypto\CnAEAD.pas',
  CnFNV in '..\..\Source\Crypto\CnFNV.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCrypt, FormCrypt);
  Application.Run;
end.
