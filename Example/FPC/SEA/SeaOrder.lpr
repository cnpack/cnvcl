program SeaOrder;

{$MODE Delphi}
{$H+}
{$APPTYPE CONSOLE}

// ==================================================================
// CnSEA Elliptic Curve Point Count Tool (Command-Line)
//
// Computes #E(F_p) for y^2 = x^3 + Ax + B mod P using the
// Schoof-Elkies-Atkin (SEA) algorithm.
//
// Usage:
//   ./SeaOrder A B P       -- compute order for given curve
//   ./SeaOrder             -- run built-in 48/64/72/96-bit tests
//
//   A, B, P are string parameters. Decimal if all digits,
//   hex if prefixed with 0x or contains non-digit characters.
//
// Modular polynomials are loaded from CnMP_*.txt files in the
// executable's directory, avoiding expensive on-the-fly computation.
// If a file is missing, it falls back to generation.
//
// Compile (macOS FPC):
//   fpc -Mdelphi -Scghi -O2 \
//     -Fu../../../Source/Common -Fu../../../Source/Crypto \
//     -Fi../../../Source/Common -Fi../../../Source/Crypto \
//     -XR/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk \
//     -FE. SeaOrder.lpr
// ==================================================================

uses
  SysUtils, Classes, Contnrs,
  CnNative, CnBigNumber, CnPrime, CnPolynomial, CnECC, CnSEA;

const
  MSecsPerDay = 86400000;

// ------------------------------------------------------------------
// Parse a string into a TCnBigNumber.
//   "0x..." or "0X..."  -> hex (prefix stripped)
//   all digits [0-9]    -> decimal
//   otherwise           -> hex
// ------------------------------------------------------------------
procedure ParseBigNumber(BN: TCnBigNumber; const S: string);
var
  I: Integer;
  AllDigits: Boolean;
begin
  if (Length(S) >= 2) and ((Copy(S, 1, 2) = '0x') or (Copy(S, 1, 2) = '0X')) then
    BN.SetHex(Copy(S, 3, MaxInt))
  else
  begin
    AllDigits := True;
    for I := 1 to Length(S) do
      if not (S[I] in ['0'..'9']) then
      begin
        AllDigits := False;
        Break;
      end;
    if AllDigits then
      BN.SetDec(S)
    else
      BN.SetHex(S);
  end;
end;

// ------------------------------------------------------------------
// Return the directory where CnMP_*.txt files reside.
// Tries the executable directory first, then the current directory.
// ------------------------------------------------------------------
function GetModPolyDir: string;
begin
  Result := ExtractFilePath(ParamStr(0));
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetCurrentDir + PathDelim;
end;

// ------------------------------------------------------------------
// Load (or generate) all modular polynomials needed for SEA on E/F_p.
//
// Iterates primes from CN_PRIME_NUMBERS_SQRT_UINT32, accumulating
// their product until it exceeds 4*sqrt(p) (Hasse bound).  For each
// prime L >= 3, loads CnMP_<L>.txt from disk; if the file is missing
// or fails to parse, falls back to CnGenerateClassicalModularPolynomial.
//
// Returns a TObjectList of TCnBigNumberBiPolynomial, one per L >= 3,
// in the same order CnSeaPointCount expects.
// ------------------------------------------------------------------
function LoadModPolys(P: TCnBigNumber; out ModPolys: TObjectList): Boolean;
var
  QMax, QMul, BQ: TCnBigNumber;
  I, L, J, SafetyBits: Integer;
  PhiL: TCnBigNumberBiPolynomial;
  SL: TStringList;
  FileName, Dir: string;
  T1, T2: TDateTime;
  LoadedCount, GeneratedCount: Integer;
begin
  Result := False;
  ModPolys := nil;
  QMax := nil; QMul := nil; BQ := nil; SL := nil;
  try
    QMax := TCnBigNumber.Create;
    QMul := TCnBigNumber.Create;
    BQ := TCnBigNumber.Create;
    SL := TStringList.Create;
    ModPolys := TObjectList.Create(True);
    Dir := GetModPolyDir;
    LoadedCount := 0;
    GeneratedCount := 0;

    // Determine the set of primes needed (same logic as CnSeaPointCount).
    // Extended threshold: product > 4*sqrt(p) * 2^SafetyBits to ensure strong Atkin
    // filtering, reducing false positives in the combine phase.
    // SafetyBits via CnSeaSafetyBits (piecewise: 0 for <=112-bit, 40 for 128-bit, etc.)
    if not BigNumberSqrt(QMax, P) then
    begin
      Result := False;
      Exit;
    end;
    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);
    SafetyBits := CnSeaSafetyBits(BigNumberGetBitsCount(P));
    for J := 1 to SafetyBits do
      BigNumberShiftLeftOne(QMax, QMax);
    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    while (BigNumberCompare(QMul, QMax) <= 0) and
          (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      L := CN_PRIME_NUMBERS_SQRT_UINT32[I];
      // Guard: stop at L=199 (max pre-computed CnMP file).
      // Beyond this, on-the-fly generation is extremely slow.
      if L > 199 then Break;
      BigNumberSetWord(BQ, L);
      if BigNumberCompare(BQ, P) <> 0 then   // skip L = P
      begin
        BigNumberMulWord(QMul, L);
        if L >= 3 then
        begin
          PhiL := TCnBigNumberBiPolynomial.Create;
          FileName := Dir + 'CnMP_' + IntToStr(L) + '.txt';
          if FileExists(FileName) then
          begin
            SL.LoadFromFile(FileName);
            if LoadModularPolynomialCoefficientsFromText(PhiL, SL) then
            begin
              ModPolys.Add(PhiL);
              Inc(LoadedCount);
              WriteLn(Format('    Loaded Phi_%d from file', [L]));
              Flush(Output);
            end
            else
            begin
              PhiL.Free;
              // Parse failed — regenerate
              T1 := Now;
              PhiL := TCnBigNumberBiPolynomial.Create;
              if CnGenerateClassicalModularPolynomial(PhiL, L) then
              begin
                ModPolys.Add(PhiL);
                Inc(GeneratedCount);
                T2 := Now;
                WriteLn(Format('    [Fallback] Generated Phi_%d (%d ms, parse failed)',
                  [L, Round((T2 - T1) * MSecsPerDay)]));
              end
              else
                PhiL.Free;
            end;
          end
          else
          begin
            // File not found — regenerate
            WriteLn(Format('    [Generating] Phi_%d (file not found, may take a while...)', [L]));
            Flush(Output);
            T1 := Now;
            if CnGenerateClassicalModularPolynomial(PhiL, L) then
            begin
              ModPolys.Add(PhiL);
              Inc(GeneratedCount);
              T2 := Now;
              WriteLn(Format('    [Fallback] Generated Phi_%d (%d ms, file not found)',
                [L, Round((T2 - T1) * MSecsPerDay)]));
            end
            else
              PhiL.Free;
          end;
        end;
      end;
      Inc(I);
    end;

    WriteLn(Format('  Modular polynomials: %d loaded from files, %d generated (L up to %d)',
      [LoadedCount, GeneratedCount, L]));
    WriteLn(Format('  Max prime L = %d, SafetyBits = %d', [L, SafetyBits]));
    Flush(Output);
    Result := True;
  finally
    SL.Free;
    BQ.Free;
    QMul.Free;
    QMax.Free;
  end;
end;

// ------------------------------------------------------------------
// Run SEA point counting on y^2 = x^3 + Ax + B over F_p.
// Prints parameters, loads modular polynomials, runs CnSeaPointCount,
// and independently verifies the result with [#E]P = O.
// ------------------------------------------------------------------
procedure RunSEA(const AStr, BStr, PStr: string; const TestName: string;
  IsHex: Boolean);
var
  A, B, P, SeaResult, Tmp: TCnBigNumber;
  T1, T2: TDateTime;
  SeaMs, LoadMs: Int64;
  ModPolys: TObjectList;
  Ecc: TCnEcc;
  Point: TCnEccPoint;
  XVal: TCnBigNumber;
  XInt: Int64;
  Found: Boolean;
  MaxL: Integer;
begin
  A := nil; B := nil; P := nil; SeaResult := nil; Tmp := nil;
  ModPolys := nil; Ecc := nil; Point := nil; XVal := nil;
  try
    A := TCnBigNumber.Create;
    B := TCnBigNumber.Create;
    P := TCnBigNumber.Create;
    SeaResult := TCnBigNumber.Create;
    Tmp := TCnBigNumber.Create;

    // Parse parameters
    if IsHex then
    begin
      A.SetHex(AStr);
      B.SetHex(BStr);
      P.SetHex(PStr);
    end
    else
    begin
      ParseBigNumber(A, AStr);
      ParseBigNumber(B, BStr);
      ParseBigNumber(P, PStr);
    end;

    // Validate
    if P.IsZero or P.IsNegative then
    begin
      WriteLn('  ERROR: P must be a positive prime.');
      Exit;
    end;

    // Reduce A and B mod P (SEA expects them in [0, P-1])
    BigNumberMod(Tmp, A, P);  BigNumberCopy(A, Tmp);
    BigNumberMod(Tmp, B, P);  BigNumberCopy(B, Tmp);

    WriteLn;
    WriteLn(Format('--- %s ---', [TestName]));
    WriteLn(Format('  E: y^2 = x^3 + %s*x + %s  over  F_p', [A.ToDec, B.ToDec]));
    WriteLn(Format('  p = %s', [P.ToDec]));
    WriteLn(Format('    (0x%s, %d bits)', [P.ToHex, P.GetBitsCount]));
    Flush(Output);

    // Show max required prime L
    MaxL := CnSeaMaxRequiredPrimeL(P);
    if MaxL > 0 then
      WriteLn(Format('  Max required prime L = %d', [MaxL]));
    Flush(Output);

    // Load modular polynomials from CnMP_*.txt files
    T1 := Now;
    if not LoadModPolys(P, ModPolys) then
    begin
      WriteLn('  ERROR: Failed to prepare modular polynomials.');
      Exit;
    end;
    T2 := Now;
    LoadMs := Round((T2 - T1) * MSecsPerDay);
    WriteLn(Format('  Modular polynomial preparation: %d ms', [LoadMs]));
    Flush(Output);

    // Run SEA
    T1 := Now;
    try
      if CnSeaPointCount(SeaResult, A, B, P, ModPolys) then
      begin
        T2 := Now;
        SeaMs := Round((T2 - T1) * MSecsPerDay);
        WriteLn(Format('  SEA result:   #E = %s  (%d ms)', [SeaResult.ToDec, SeaMs]));
        WriteLn(Format('           (0x%s)', [SeaResult.ToHex]));
        Flush(Output);

        // Independent verification: find a point P on E, check [#E]P = O
        WriteLn('  Verification ([#E]P = O):');
        Flush(Output);
        Ecc := TCnEcc.Create(A.ToHex, B.ToHex, P.ToHex, '0', '0',
          SeaResult.ToHex, 1);
        Point := TCnEccPoint.Create;
        XVal := TCnBigNumber.Create;
        Found := False;
        XInt := 1;
        while XInt <= 10000 do
        begin
          XVal.SetInt64(XInt);
          if Ecc.PlainToPoint(XVal, Point) then
          begin
            Found := True;
            Break;
          end;
          Inc(XInt);
        end;

        if Found then
        begin
          WriteLn(Format('    Found point P = (%s, %s)',
            [Point.X.ToDec, Point.Y.ToDec]));
          Ecc.MultiplePoint(SeaResult, Point);
          if Point.IsZero then
            WriteLn('    VERIFIED! [#E]P = O (point at infinity)')
          else
            WriteLn(Format('    FAILED! [#E]P = (%s, %s)',
              [Point.X.ToDec, Point.Y.ToDec]));
        end
        else
          WriteLn('    Could not find a point on the curve for verification');
      end
      else
      begin
        T2 := Now;
        SeaMs := Round((T2 - T1) * MSecsPerDay);
        WriteLn(Format('  SEA FAILED  (%d ms)', [SeaMs]));
      end;
    except
      on E: Exception do
      begin
        T2 := Now;
        SeaMs := Round((T2 - T1) * MSecsPerDay);
        WriteLn(Format('  SEA EXCEPTION (%d ms): %s', [SeaMs, E.Message]));
      end;
    end;
  finally
    XVal.Free;
    Point.Free;
    Ecc.Free;
    ModPolys.Free;
    Tmp.Free;
    SeaResult.Free;
    P.Free;
    B.Free;
    A.Free;
  end;
  Flush(Output);
end;

// ------------------------------------------------------------------
// Main
// ------------------------------------------------------------------
var
  AStr, BStr, PStr: string;
begin
  WriteLn('CnSEA Elliptic Curve Point Count Tool');
  WriteLn('======================================');
  Flush(Output);

  if ParamCount >= 3 then
  begin
    AStr := ParamStr(1);
    BStr := ParamStr(2);
    PStr := ParamStr(3);
    WriteLn(Format('Parameters: A=%s  B=%s  P=%s', [AStr, BStr, PStr]));
    WriteLn('(decimal if all digits, hex if 0x-prefixed or non-digit chars)');
    RunSEA(AStr, BStr, PStr, 'Custom Curve', False);
  end
  else if (ParamCount = 1) and (LowerCase(ParamStr(1)) = '112bit') then
  begin
    // secp112r1 parameters
    // p = DB7C2ABF62E35E668076BEAD208B
    // a = DB7C2ABF62E35E668076BEAD2088
    // b = 659EF8BA043916EEDE8911702B22
    WriteLn('Running secp112r1 test...');
    RunSEA('DB7C2ABF62E35E668076BEAD2088',
           '659EF8BA043916EEDE8911702B22',
           'DB7C2ABF62E35E668076BEAD208B',
           '112-bit secp112r1', True);
  end
  else if (ParamCount = 1) and (LowerCase(ParamStr(1)) = '128bit') then
  begin
    // secp128r1 parameters
    // p = FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF
    // a = FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFC
    // b = E87579C11079F43DD824993C2CEE5ED3
    WriteLn('Running secp128r1 test...');
    WriteLn('NOTE: This may take a long time. Use -dSEA_DEBUG compilation');
    WriteLn('      flag to see per-L timing output.');
    RunSEA('FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFC',
           'E87579C11079F43DD824993C2CEE5ED3',
           'FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF',
           '128-bit secp128r1', True);
  end
  else if (ParamCount = 1) and (LowerCase(ParamStr(1)) = '160bit') then
  begin
    // secp160r1 parameters (SECG SEC 2)
    // p = FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF
    // a = FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFC
    // b = 1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45
    WriteLn('Running secp160r1 test...');
    WriteLn('NOTE: This may take a long time. Use -dSEA_DEBUG compilation');
    WriteLn('      flag to see per-L timing output.');
    RunSEA('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFC',
           '1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45',
           'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF',
           '160-bit secp160r1', True);
  end
  else if (ParamCount = 1) and (LowerCase(ParamStr(1)) = '192bit') then
  begin
    // secp192r1 parameters (NIST P-192 / SEC 2)
    // p = FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF
    // a = FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC
    // b = 64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1
    WriteLn('Running secp192r1 test...');
    WriteLn('NOTE: This may take a long time. Use -dSEA_DEBUG compilation');
    WriteLn('      flag to see per-L timing output.');
    RunSEA('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC',
           '64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1',
           'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF',
           '192-bit secp192r1', True);
  end
  else
  begin
    WriteLn('No parameters. Running built-in test cases (48/64/72/96/112/128-bit).');
    WriteLn('Usage: SeaOrder <A> <B> <P>');
    WriteLn('  A, B: Weierstrass coefficients (decimal or hex string)');
    WriteLn('  P:    field prime (decimal or hex string)');
    WriteLn('  Special: SeaOrder 112bit / 128bit / 160bit / 192bit  for secp curves');
    WriteLn;

    // 48-bit CM curve: p = 16777213^2 + 38^2, a=1, b=0
    RunSEA('1', '0', 'FFFFFA0005AD',
      '48-bit CM: y^2 = x^3 + x, p = 16777213^2 + 38^2', True);

    // 64-bit CM curve: p = 3037000503^2 + 88^2, a=1, b=0
    RunSEA('1', '0', '8000000446C99411',
      '64-bit CM: y^2 = x^3 + x, p = 3037000503^2 + 88^2', True);

    // 72-bit CM curve: p = 55000000000^2 + 21^2, a=1, b=0
    RunSEA('1', '0', 'A3FC4EE0DCF4A401B9',
      '72-bit CM: y^2 = x^3 + x, p = 55000000000^2 + 21^2', True);

    // 96-bit CM curve: p = 200000000000000^2 + 3^2, a=1, b=0
    RunSEA('1', '0', '813F3978F894098440000009',
      '96-bit CM: y^2 = x^3 + x, p = 200000000000000^2 + 3^2', True);
  end;

  WriteLn;
  WriteLn('Done.');
end.
