program GIFTest;

{$APPTYPE CONSOLE}

uses
  SysUtils, Interfaces,
  CnGIFTestUnit in '..\..\VCL\GIF\CnGIFTestUnit.pas';

begin
  RunAllTests;
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
