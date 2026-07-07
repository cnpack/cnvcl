program GIFTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  CnGIFTestUnit in 'CnGIFTestUnit.pas';

begin
  RunAllTests;
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
