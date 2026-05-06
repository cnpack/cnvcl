program StdioTests;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes,
  CnStdio in '..\..\..\Source\NonVisual\CnStdio.pas';

const
  CN_BIG_EXPECT_STDOUT_LINES = 1500;

  CN_BIG_EXPECT_STDERR_LINES = 300;

  CN_STRESS_CONCURRENT_COUNT = 8;

  CN_STRESS_ROUND_COUNT = 10;

  CN_STRESS_WAIT_TIMEOUT = 20000;

type
  TCnStdioTestRunner = class(TObject)
  private
    FStdOutLines: TStringList;
    FStdErrLines: TStringList;
    FFailCount: Integer;
    FPassCount: Integer;
    FStartTick: Cardinal;
    FStressProcTotal: Integer;
    FStressProcSuccess: Integer;
    FStressStdOutLines: Integer;
    FStressStdErrLines: Integer;
    FStressElapsed: Cardinal;

    procedure OnLine(Sender: TObject; StreamKind: TCnStdioStreamKind; const Line: string);
    procedure OnError(Sender: TObject; const Stage: string; ErrorCode: Cardinal;
      const ErrorMessage: string);
    procedure AssertTrue(const CaseName: string; Cond: Boolean);
    procedure AssertContains(const CaseName, WholeText, PartText: string);
    function ChildPath: string;
    function BuildCmd(const Mode: string; ExitCode: Integer; ExtraArg: Integer): string;
    procedure ClearCaptured;
    function CountLines(const S: string): Integer;
    procedure PrintSummary;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunAll;
    procedure TestStdOutCapture;
    procedure TestStdErrCapture;
    procedure TestMixedAndExitCode;
    procedure TestEchoInput;
    procedure TestTerminate;
    procedure TestBigDataLoad;
    procedure TestHighConcurrency;
  end;

{ TCnStdioTestRunner }

constructor TCnStdioTestRunner.Create;
begin
  inherited Create;
  FStdOutLines := TStringList.Create;
  FStdErrLines := TStringList.Create;
  FStartTick := GetTickCount;
end;

destructor TCnStdioTestRunner.Destroy;
begin
  FStdOutLines.Free;
  FStdErrLines.Free;
  inherited Destroy;
end;

procedure TCnStdioTestRunner.OnLine(Sender: TObject;
  StreamKind: TCnStdioStreamKind; const Line: string);
begin
  if StreamKind = cskStdOut then
    FStdOutLines.Add(Line)
  else
    FStdErrLines.Add(Line);
end;

procedure TCnStdioTestRunner.OnError(Sender: TObject; const Stage: string;
  ErrorCode: Cardinal; const ErrorMessage: string);
begin
  Writeln('ERROR: ', Stage, ' Code=', ErrorCode, ' Msg=', ErrorMessage);
end;

procedure TCnStdioTestRunner.AssertTrue(const CaseName: string; Cond: Boolean);
begin
  if Cond then
  begin
    Inc(FPassCount);
    Writeln('[PASS] ', CaseName);
  end
  else
  begin
    Inc(FFailCount);
    Writeln('[FAIL] ', CaseName);
  end;
end;

procedure TCnStdioTestRunner.AssertContains(const CaseName, WholeText,
  PartText: string);
begin
  AssertTrue(CaseName, Pos(PartText, WholeText) > 0);
end;

function TCnStdioTestRunner.ChildPath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'ChildStub.exe';
end;

function TCnStdioTestRunner.BuildCmd(const Mode: string; ExitCode: Integer;
  ExtraArg: Integer): string;
begin
  if ExtraArg >= 0 then
    Result := '"' + ChildPath + '" ' + Mode + ' ' + IntToStr(ExitCode) + ' ' + IntToStr(ExtraArg)
  else
    Result := '"' + ChildPath + '" ' + Mode + ' ' + IntToStr(ExitCode);
end;

procedure TCnStdioTestRunner.ClearCaptured;
begin
  FStdOutLines.Clear;
  FStdErrLines.Clear;
end;

function TCnStdioTestRunner.CountLines(const S: string): Integer;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := S;
    Result := SL.Count;
  finally
    SL.Free;
  end;
end;

procedure TCnStdioTestRunner.TestStdOutCapture;
var
  P: TCnStdioProcess;
begin
  ClearCaptured;
  P := TCnStdioProcess.Create;
  try
    P.OnLine := OnLine;
    P.OnError := OnError;
    P.CommandLine := BuildCmd('stdout', 0, -1);
    AssertTrue('Start stdout mode', P.Start);
    AssertTrue('Wait stdout mode', P.WaitFor(5000));
    AssertContains('stdout has line1', P.StdOutText, 'OUT:LINE1');
    AssertContains('stdout has line2', P.StdOutText, 'OUT:LINE2');
    AssertTrue('stderr should be empty in stdout mode', Pos('ERR:', P.StdErrText) = 0);
  finally
    P.Free;
  end;
end;

procedure TCnStdioTestRunner.TestStdErrCapture;
var
  P: TCnStdioProcess;
begin
  ClearCaptured;
  P := TCnStdioProcess.Create;
  try
    P.OnLine := OnLine;
    P.OnError := OnError;
    P.CommandLine := BuildCmd('stderr', 0, -1);
    AssertTrue('Start stderr mode', P.Start);
    AssertTrue('Wait stderr mode', P.WaitFor(5000));
    AssertContains('stderr has line1', P.StdErrText, 'ERR:LINE1');
    AssertContains('stderr has line2', P.StdErrText, 'ERR:LINE2');
  finally
    P.Free;
  end;
end;

procedure TCnStdioTestRunner.TestMixedAndExitCode;
var
  P: TCnStdioProcess;
begin
  ClearCaptured;
  P := TCnStdioProcess.Create;
  try
    P.OnLine := OnLine;
    P.OnError := OnError;
    P.CommandLine := BuildCmd('mixed', 7, -1);
    AssertTrue('Start mixed mode', P.Start);
    AssertTrue('Wait mixed mode', P.WaitFor(5000));
    AssertContains('mixed stdout line', P.StdOutText, 'OUT:MIXED1');
    AssertContains('mixed stderr line', P.StdErrText, 'ERR:MIXED1');
    AssertTrue('mixed exit code should be 7', P.ExitCodeReady and (P.ExitCode = 7));
  finally
    P.Free;
  end;
end;

procedure TCnStdioTestRunner.TestEchoInput;
var
  P: TCnStdioProcess;
begin
  ClearCaptured;
  P := TCnStdioProcess.Create;
  try
    P.OnLine := OnLine;
    P.OnError := OnError;
    P.CommandLine := BuildCmd('echo', 0, -1);
    AssertTrue('Start echo mode', P.Start);
    Sleep(100);
    AssertTrue('Write line alpha', P.WriteLine('alpha'));
    AssertTrue('Write line quit', P.WriteLine('quit'));
    P.CloseInput;
    AssertTrue('Wait echo mode', P.WaitFor(5000));
    AssertContains('echo alpha result', P.StdOutText, 'OUT:ECHO:alpha');
    AssertContains('echo quit result', P.StdOutText, 'OUT:ECHO:quit');
    AssertContains('echo done result', P.StdOutText, 'OUT:ECHO_DONE');
  finally
    P.Free;
  end;
end;

procedure TCnStdioTestRunner.TestTerminate;
var
  P: TCnStdioProcess;
begin
  ClearCaptured;
  P := TCnStdioProcess.Create;
  try
    P.OnLine := OnLine;
    P.OnError := OnError;
    P.CommandLine := BuildCmd('sleep', 0, 15000);
    AssertTrue('Start sleep mode', P.Start);
    Sleep(200);
    P.Terminate;
    AssertTrue('Wait terminate mode', P.WaitFor(5000));
    AssertTrue('Terminate exit code non-zero', P.ExitCodeReady and (P.ExitCode <> 0));
  finally
    P.Free;
  end;
end;

procedure TCnStdioTestRunner.TestBigDataLoad;
var
  P: TCnStdioProcess;
  OutLines: Integer;
  ErrLines: Integer;
  T0: Cardinal;
  Elapsed: Cardinal;
begin
  ClearCaptured;
  P := TCnStdioProcess.Create;
  try
    P.OnError := OnError;
    P.CommandLine := BuildCmd('big', 0, -1);
    T0 := GetTickCount;
    AssertTrue('Start big mode', P.Start);
    AssertTrue('Wait big mode', P.WaitFor(CN_STRESS_WAIT_TIMEOUT));
    Elapsed := GetTickCount - T0;

    OutLines := CountLines(P.StdOutText);
    ErrLines := CountLines(P.StdErrText);

    AssertTrue('big stdout lines >= 1500', OutLines >= CN_BIG_EXPECT_STDOUT_LINES);
    AssertTrue('big stderr lines >= 300', ErrLines >= CN_BIG_EXPECT_STDERR_LINES);
    AssertTrue('big exit code is 0', P.ExitCodeReady and (P.ExitCode = 0));

    Writeln('[STAT] BIG elapsed ms: ', Elapsed);
    Writeln('[STAT] BIG stdout lines: ', OutLines);
    Writeln('[STAT] BIG stderr lines: ', ErrLines);
  finally
    P.Free;
  end;
end;

procedure TCnStdioTestRunner.TestHighConcurrency;
var
  RoundIdx: Integer;
  ProcIdx: Integer;
  PArr: array of TCnStdioProcess;
  OK: Boolean;
  T0: Cardinal;
  Elapsed: Cardinal;
  OutLines: Integer;
  ErrLines: Integer;
begin
  T0 := GetTickCount;
  for RoundIdx := 1 to CN_STRESS_ROUND_COUNT do
  begin
    SetLength(PArr, CN_STRESS_CONCURRENT_COUNT);
    try
      for ProcIdx := 0 to CN_STRESS_CONCURRENT_COUNT - 1 do
      begin
        PArr[ProcIdx] := TCnStdioProcess.Create;
        PArr[ProcIdx].OnError := OnError;
        PArr[ProcIdx].CommandLine := BuildCmd('mixed', 0, -1);
      end;

      for ProcIdx := 0 to CN_STRESS_CONCURRENT_COUNT - 1 do
      begin
        Inc(FStressProcTotal);
        OK := PArr[ProcIdx].Start;
        AssertTrue('concurrent start round ' + IntToStr(RoundIdx) + ' idx ' + IntToStr(ProcIdx), OK);
      end;

      for ProcIdx := 0 to CN_STRESS_CONCURRENT_COUNT - 1 do
      begin
        OK := PArr[ProcIdx].WaitFor(CN_STRESS_WAIT_TIMEOUT);
        AssertTrue('concurrent wait round ' + IntToStr(RoundIdx) + ' idx ' + IntToStr(ProcIdx), OK);
        if OK then
        begin
          Inc(FStressProcSuccess);
          OutLines := CountLines(PArr[ProcIdx].StdOutText);
          ErrLines := CountLines(PArr[ProcIdx].StdErrText);
          Inc(FStressStdOutLines, OutLines);
          Inc(FStressStdErrLines, ErrLines);
          AssertContains('concurrent stdout content round ' + IntToStr(RoundIdx) +
            ' idx ' + IntToStr(ProcIdx), PArr[ProcIdx].StdOutText, 'OUT:MIXED1');
          AssertContains('concurrent stderr content round ' + IntToStr(RoundIdx) +
            ' idx ' + IntToStr(ProcIdx), PArr[ProcIdx].StdErrText, 'ERR:MIXED1');
        end;
      end;
    finally
      for ProcIdx := 0 to Length(PArr) - 1 do
        PArr[ProcIdx].Free;
      SetLength(PArr, 0);
    end;
  end;

  Elapsed := GetTickCount - T0;
  Inc(FStressElapsed, Elapsed);

  Writeln('[STAT] CONCURRENT rounds: ', CN_STRESS_ROUND_COUNT);
  Writeln('[STAT] CONCURRENT process per round: ', CN_STRESS_CONCURRENT_COUNT);
  Writeln('[STAT] CONCURRENT total process: ', FStressProcTotal);
  Writeln('[STAT] CONCURRENT success process: ', FStressProcSuccess);
  Writeln('[STAT] CONCURRENT total stdout lines: ', FStressStdOutLines);
  Writeln('[STAT] CONCURRENT total stderr lines: ', FStressStdErrLines);
  Writeln('[STAT] CONCURRENT elapsed ms: ', Elapsed);
end;

procedure TCnStdioTestRunner.PrintSummary;
var
  Total: Integer;
  PassRate100: Integer;
  TotalElapsed: Cardinal;
begin
  Total := FPassCount + FFailCount;
  if Total > 0 then
    PassRate100 := (FPassCount * 10000) div Total
  else
    PassRate100 := 0;

  TotalElapsed := GetTickCount - FStartTick;

  Writeln('== Summary ==');
  Writeln('Total Assertions: ', Total);
  Writeln('PASS: ', FPassCount);
  Writeln('FAIL: ', FFailCount);
  Writeln('Pass Rate: ', PassRate100 div 100, '.', PassRate100 mod 100, '%');
  Writeln('Stress Process Total: ', FStressProcTotal);
  Writeln('Stress Process Success: ', FStressProcSuccess);
  Writeln('Stress StdOut Lines: ', FStressStdOutLines);
  Writeln('Stress StdErr Lines: ', FStressStdErrLines);
  Writeln('Stress Elapsed(ms): ', FStressElapsed);
  Writeln('All Elapsed(ms): ', TotalElapsed);
end;

procedure TCnStdioTestRunner.RunAll;
begin
  Writeln('== CnStdio Tests Begin ==');
  AssertTrue('ChildStub exists', FileExists(ChildPath));
  if not FileExists(ChildPath) then
    Exit;

  TestStdOutCapture;
  TestStdErrCapture;
  TestMixedAndExitCode;
  TestEchoInput;
  TestTerminate;
  TestBigDataLoad;
  TestHighConcurrency;

  Writeln('== CnStdio Tests End ==');
  PrintSummary;
end;

var
  Runner: TCnStdioTestRunner;
begin
  Runner := TCnStdioTestRunner.Create;
  try
    Runner.RunAll;
  finally
    Runner.Free;
  end;
end.

