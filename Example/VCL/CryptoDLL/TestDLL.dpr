program TestDLL;


{$APPTYPE CONSOLE}

// Please prepare Crypto.dll compiled from cncrypto\Package\Crypto.dpr

uses
  ExportTestUnit in '..\CryptoExport\ExportTestUnit.pas',
  CnCryptoIntf in '..\..\..\Source\Crypto\CnCryptoIntf.pas';

begin
  RunAll;
  Readln;
end.


