program CnXMLTestRunner;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  CnXMLTest in '..\..\VCL\XML\CnXMLTest.pas';

begin
  try
    RunXMLTests;
    Readln;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
