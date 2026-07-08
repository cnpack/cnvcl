program JPEGTest;

{$APPTYPE CONSOLE}

uses
  SysUtils, Interfaces,
  CnJPEGTestUnit in '..\..\VCL\JPEG\CnJPEGTestUnit.pas';

begin
  RunAllTests;
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
