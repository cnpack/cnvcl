program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CnMD5 in '..\..\Source\Common\CnMD5.pas',
  CnBase64 in '..\..\Source\Common\CnBase64.pas',
  CnCRC32 in '..\..\Source\Common\CnCRC32.pas',
  CnDES in '..\..\Source\Common\CnDES.pas',
  CnSHA1 in '..\..\Source\Common\CnSHA1.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
