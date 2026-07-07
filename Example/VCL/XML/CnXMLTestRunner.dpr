program CnXMLBugfixTestRunner;

{$IFDEF FPC}
{$MODE DELPHI}
{$ELSE}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  CnXMLTest in 'CnXMLTest.pas';

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
