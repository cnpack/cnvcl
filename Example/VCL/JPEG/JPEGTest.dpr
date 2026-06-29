program JPEGTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  CnJPEGTestUnit in 'CnJPEGTestUnit.pas';

begin
  RunAllTests;
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
