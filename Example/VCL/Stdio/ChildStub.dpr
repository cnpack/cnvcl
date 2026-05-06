program ChildStub;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils;

const
  SCN_MODE_STDOUT = 'stdout';

  SCN_MODE_STDERR = 'stderr';

  SCN_MODE_MIXED = 'mixed';

  SCN_MODE_ECHO = 'echo';

  SCN_MODE_BIG = 'big';

  SCN_MODE_SLEEP = 'sleep';

procedure WriteErr(const S: string);
var
  H: THandle;
  OutS: string;
  Written: Cardinal;
begin
  H := GetStdHandle(STD_ERROR_HANDLE);
  if H = INVALID_HANDLE_VALUE then
    Exit;

  OutS := S + #13#10;
  Written := 0;
  WriteFile(H, OutS[1], Length(OutS), Written, nil);
end;

function ReadArg(Index: Integer; const Def: string): string;
begin
  if ParamCount >= Index then
    Result := ParamStr(Index)
  else
    Result := Def;
end;

function ReadIntArg(Index: Integer; Def: Integer): Integer;
begin
  Result := Def;
  if ParamCount >= Index then
  begin
    try
      Result := StrToInt(ParamStr(Index));
    except
      Result := Def;
    end;
  end;
end;

procedure RunStdOut;
begin
  Writeln('OUT:LINE1');
  Writeln('OUT:LINE2');
end;

procedure RunStdErr;
begin
  WriteErr('ERR:LINE1');
  WriteErr('ERR:LINE2');
end;

procedure RunMixed;
begin
  Writeln('OUT:MIXED1');
  WriteErr('ERR:MIXED1');
  Writeln('OUT:MIXED2');
  WriteErr('ERR:MIXED2');
end;

procedure RunEcho;
var
  S: string;
begin
  Writeln('OUT:ECHO_READY');
  while not Eof(Input) do
  begin
    Readln(S);
    Writeln('OUT:ECHO:' + S);
    if S = 'quit' then
      Break;
  end;
  Writeln('OUT:ECHO_DONE');
end;

procedure RunBig;
var
  I: Integer;
begin
  for I := 1 to 1500 do
  begin
    Writeln('OUT:BIG:' + IntToStr(I));
    if (I mod 5) = 0 then
      WriteErr('ERR:BIG:' + IntToStr(I));
  end;
end;

procedure RunSleep;
var
  WaitMs: Integer;
begin
  WaitMs := ReadIntArg(3, 15000);
  Writeln('OUT:SLEEP_BEGIN');
  Sleep(WaitMs);
  Writeln('OUT:SLEEP_END');
end;

var
  Mode: string;
  ExitCode: Integer;
begin
  Mode := LowerCase(ReadArg(1, SCN_MODE_STDOUT));
  ExitCode := ReadIntArg(2, 0);

  if Mode = SCN_MODE_STDOUT then
    RunStdOut
  else
  if Mode = SCN_MODE_STDERR then
    RunStdErr
  else
  if Mode = SCN_MODE_MIXED then
    RunMixed
  else
  if Mode = SCN_MODE_ECHO then
    RunEcho
  else
  if Mode = SCN_MODE_BIG then
    RunBig
  else
  if Mode = SCN_MODE_SLEEP then
    RunSleep
  else
  begin
    Writeln('OUT:UNKNOWN_MODE=' + Mode);
    ExitCode := 2;
  end;

  Halt(ExitCode);
end.
