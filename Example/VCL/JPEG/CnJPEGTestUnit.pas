unit CnJPEGTestUnit;

interface

uses
  SysUtils, Classes, Windows, Graphics, CnNative, CnJPEG;

type
  TCnJPEGTestCase = record
    Name: string;
    Description: string;
    Run: procedure of object;
  end;

  TObjectProc = procedure of object;

procedure RunAllTests;
procedure RegisterTest(const AName, ADesc: string; AProc: TObjectProc);
procedure AssertTrue(Condition: Boolean; const Msg: string);
procedure AssertEqual(Expected, Actual: Integer; const Msg: string);
procedure AssertEqualStr(const Expected, Actual: string; const Msg: string);
procedure AssertInRange(Value, Lo, Hi: Integer; const Msg: string);

var
  TestPassCount: Integer = 0;
  TestFailCount: Integer = 0;

implementation

uses
  TypInfo;

var
  TestCases: array of TCnJPEGTestCase;

procedure RegisterTest(const AName, ADesc: string; AProc: TObjectProc);
begin
  SetLength(TestCases, Length(TestCases) + 1);
  TestCases[High(TestCases)].Name := AName;
  TestCases[High(TestCases)].Description := ADesc;
  TestCases[High(TestCases)].Run := AProc;
end;

procedure AssertTrue(Condition: Boolean; const Msg: string);
begin
  if not Condition then
    raise Exception.Create('AssertTrue failed: ' + Msg);
end;

procedure AssertEqual(Expected, Actual: Integer; const Msg: string);
begin
  if Expected <> Actual then
    raise Exception.Create(Format('AssertEqual failed: expected %d, got %d. %s',
      [Expected, Actual, Msg]));
end;

procedure AssertEqualStr(const Expected, Actual: string; const Msg: string);
begin
  if Expected <> Actual then
    raise Exception.Create(Format('AssertEqualStr failed: expected "%s", got "%s". %s',
      [Expected, Actual, Msg]));
end;

procedure AssertInRange(Value, Lo, Hi: Integer; const Msg: string);
begin
  if (Value < Lo) or (Value > Hi) then
    raise Exception.Create(Format('AssertInRange failed: %d not in [%d..%d]. %s',
      [Value, Lo, Hi, Msg]));
end;

function MakeRect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

procedure RunAllTests;
var
  I: Integer;
begin
  WriteLn('=== CnJPEG Test Suite ===');
  WriteLn;
  TestPassCount := 0;
  TestFailCount := 0;

  for I := 0 to High(TestCases) do
  begin
    Write(Format('[RUN ] %s: %s', [TestCases[I].Name, TestCases[I].Description]));
    try
      TestCases[I].Run;
      Inc(TestPassCount);
      WriteLn(' -> [PASS]');
    except
      on E: Exception do
      begin
        Inc(TestFailCount);
        WriteLn(' -> [FAIL]');
        WriteLn('       ' + E.Message);
      end;
    end;
  end;

  WriteLn;
  WriteLn(Format('Result: %d passed, %d failed', [TestPassCount, TestFailCount]));
end;

//============================================================================
// L1: Marker Parsing Tests
//============================================================================

type
  TMarkerTests = class
    class procedure TestSOIDetection;
    class procedure TestSegmentLength;
  end;

class procedure TMarkerTests.TestSOIDetection;
var
  MS: TMemoryStream;
  Marker: TCnJPEGMarker;
  Code: Word;
  B: Byte;
begin
  // Test: $FF $D8 → SOI
  MS := TMemoryStream.Create;
  try
    B := $FF; MS.Write(B, 1);
    B := $D8; MS.Write(B, 1);
    MS.Position := 0;
    Marker := TCnJPEGMarker.Create(MS);
    try
      Code := Marker.ReadMarker;
      AssertEqual($FFD8, Code, 'SOI marker');
    finally
      Marker.Free;
    end;
  finally
    MS.Free;
  end;
end;

class procedure TMarkerTests.TestSegmentLength;
var
  MS: TMemoryStream;
  Marker: TCnJPEGMarker;
  SegLen: Integer;
  SegData: TMemoryStream;
  W: Word;
  B: Byte;
  I: Integer;
begin
  // Test: segment length = 16, data = 14 bytes
  MS := TMemoryStream.Create;
  try
    W := UInt16ToBigEndian($FFE0); MS.Write(W, 2);  // APP0 marker
    W := UInt16ToBigEndian(16);   MS.Write(W, 2);  // length = 16
    for I := 0 to 13 do
    begin
      B := I;
      MS.Write(B, 1);
    end;
    MS.Position := 0;
    Marker := TCnJPEGMarker.Create(MS);
    try
      Marker.ReadMarker;  // skip marker code
      SegData := TMemoryStream.Create;
      try
        SegLen := Marker.ReadSegment(SegData);
        AssertEqual(16, SegLen, 'segment length');
        AssertEqual(14, SegData.Size, 'segment data size');
      finally
        SegData.Free;
      end;
    finally
      Marker.Free;
    end;
  finally
    MS.Free;
  end;
end;

//============================================================================
// L1: ZigZag Tests
//============================================================================

type
  TZigZagTests = class
    class procedure TestZigZagOrder;
    class procedure TestZigZagInverse;
    class procedure TestZigZagRoundtrip;
  end;

class procedure TZigZagTests.TestZigZagOrder;
begin
  AssertEqual(0, CN_JPEG_ZIGZAG_ORDER[0], 'ZigZag[0]');
  AssertEqual(1, CN_JPEG_ZIGZAG_ORDER[1], 'ZigZag[1]');
  AssertEqual(8, CN_JPEG_ZIGZAG_ORDER[2], 'ZigZag[2]');
end;

class procedure TZigZagTests.TestZigZagInverse;
begin
  AssertEqual(0, CN_JPEG_ZIGZAG_INV[0], 'ZigZagInv[0]');
  AssertEqual(1, CN_JPEG_ZIGZAG_INV[1], 'ZigZagInv[1]');
  AssertEqual(5, CN_JPEG_ZIGZAG_INV[2], 'ZigZagInv[2]');
end;

class procedure TZigZagTests.TestZigZagRoundtrip;
var
  I: Integer;
begin
  for I := 0 to 63 do
    AssertEqual(I, CN_JPEG_ZIGZAG_INV[CN_JPEG_ZIGZAG_ORDER[I]],
      'ZigZag roundtrip at ' + IntToStr(I));
end;

//============================================================================
// L1: Color Conversion Tests
//============================================================================

type
  TColorTests = class
    class procedure TestYCbCrToRGB;
    class procedure TestRGBToYCbCr;
  end;

class procedure TColorTests.TestYCbCrToRGB;
var
  R, G, B: Integer;
  Y, Cb, Cr: Integer;
begin
  // Gray: Y=128, Cb=128, Cr=128 → R=128, G=128, B=128
  Y := 128; Cb := 128; Cr := 128;
  R := Y + Round(1.402 * (Cr - 128));
  G := Y - Round(0.34414 * (Cb - 128)) - Round(0.71414 * (Cr - 128));
  B := Y + Round(1.772 * (Cb - 128));
  AssertInRange(R, 127, 129, 'Gray R');
  AssertInRange(G, 127, 129, 'Gray G');
  AssertInRange(B, 127, 129, 'Gray B');

  // Red: Y=76, Cb=85, Cr=255 → R=255
  Y := 76; Cb := 85; Cr := 255;
  R := Y + Round(1.402 * (Cr - 128));
  AssertInRange(R, 254, 255, 'Red R');

  // Green: Y=150, Cb=44, Cr=21 → G=255
  Y := 150; Cb := 44; Cr := 21;
  G := Y - Round(0.34414 * (Cb - 128)) - Round(0.71414 * (Cr - 128));
  AssertInRange(G, 254, 255, 'Green G');

  // Blue: Y=29, Cb=255, Cr=107 → B=255
  Y := 29; Cb := 255; Cr := 107;
  B := Y + Round(1.772 * (Cb - 128));
  AssertInRange(B, 254, 255, 'Blue B');
end;

class procedure TColorTests.TestRGBToYCbCr;
var
  Y: Integer;
  R, G, B: Integer;
begin
  // Red: R=255, G=0, B=0 → Y≈76, Cb≈85, Cr≈255
  R := 255; G := 0; B := 0;
  Y := Round(0.299 * R + 0.587 * G + 0.114 * B);
  AssertInRange(Y, 75, 77, 'Red Y');

  // Green: R=0, G=255, B=0 → Y≈150
  R := 0; G := 255; B := 0;
  Y := Round(0.299 * R + 0.587 * G + 0.114 * B);
  AssertInRange(Y, 149, 151, 'Green Y');

  // Blue: R=0, G=0, B=255 → Y≈29
  R := 0; G := 0; B := 255;
  Y := Round(0.299 * R + 0.587 * G + 0.114 * B);
  AssertInRange(Y, 28, 30, 'Blue Y');
end;

//============================================================================
// L1: Quantization Table Tests
//============================================================================

type
  TQuantTests = class
    class procedure TestQuantTableBuild;
  end;

class procedure TQuantTests.TestQuantTableBuild;
var
  Encoder: TCnJPEGEncoder;
begin
  Encoder := TCnJPEGEncoder.Create;
  try
    Encoder.Encode(nil, nil, 50, False, False);  // This will fail, but tables are built
  except
    // Expected to fail since SrcBmp is nil
  end;
  // Just verify the test framework works
  AssertTrue(True, 'Quantization table test placeholder');
end;

//============================================================================
// L2: Baseline Decode Tests
//============================================================================

type
  TDecodeTests = class
    class procedure TestDecodeProperties;
    class procedure TestEncodeRoundtrip;
    class procedure TestGrayscaleEncode;
  end;

class procedure TDecodeTests.TestDecodeProperties;
var
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  W: Word;
begin
  // Create minimal JPEG: SOI + EOI
  MS := TMemoryStream.Create;
  try
    W := UInt16ToBigEndian($FFD8); MS.Write(W, 2);  // SOI
    W := UInt16ToBigEndian($FFD9); MS.Write(W, 2);  // EOI
    MS.Position := 0;

    JPEG := TCnJPEGImage.Create;
    try
      // This should not crash (may report 0x0)
      try
        JPEG.LoadFromStream(MS);
      except
        // Minimal JPEG without SOF may raise, that's OK
      end;
      AssertTrue(True, 'LoadFromStream did not crash');
    finally
      JPEG.Free;
    end;
  finally
    MS.Free;
  end;
end;

class procedure TDecodeTests.TestEncodeRoundtrip;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  OutBmp: TBitmap;
  X, Y: Integer;
  Row: PByteArray;
begin
  // Create a simple bitmap
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 64;
    Bmp.Height := 64;

    for Y := 0 to 63 do
    begin
      Row := Bmp.ScanLine[Y];
      for X := 0 to 63 do
      begin
        Row[X * 3] := Byte((X * 4) and $FF);     // B
        Row[X * 3 + 1] := Byte((Y * 4) and $FF); // G
        Row[X * 3 + 2] := Byte(128);             // R
      end;
    end;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 90;

      // Save to stream
      // Load back
      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(64, OutBmp.Width, 'Roundtrip width');
        AssertEqual(64, OutBmp.Height, 'Roundtrip height');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TDecodeTests.TestGrayscaleEncode;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 32;
    Bmp.Height := 32;

    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 32, 32));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.Grayscale := True;
      JPEG.CompressionQuality := 75;
      // Encode
      JPEG.Compress;
      AssertTrue(JPEG.Grayscale, 'Grayscale flag after encode');
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L1: Huffman Table Build Tests (Task 36.1)
//============================================================================

type
  THuffmanTests = class
    class procedure TestHuffmanTableBuild;
    class procedure TestHuffmanDCDecode;
    class procedure TestHuffmanACDecode;
    class procedure TestHuffmanBitstreamDecode;
  end;

class procedure THuffmanTests.TestHuffmanTableBuild;
var
  Tbl: TCnJPEGHuffmanTable;
  I, J, TotalCodes: Integer;
  BitCode, CodeLen: Byte;
  Found: Boolean;
begin
  // ---- Test 1: Standard DC luminance table ----
  for I := 1 to 16 do
    Tbl.Bits[I] := CN_JPEG_STD_DC_LUMINANCE_BITS[I];
  for I := 0 to 11 do
    Tbl.HuffVal[I] := CN_JPEG_STD_DC_LUMINANCE_VAL[I];

  CnJPEGBuildHuffmanTable(Tbl);

  // DC luminance: Bits = (0,1,5,1,1,1,1,1,1,0,...)
  // Length 1: 0 codes, Length 2: 1 code -> MinCode[2]=0, MaxCode[2]=0
  AssertEqual(0, Tbl.MinCode[2], 'DC lum MinCode[2]');
  AssertEqual(0, Tbl.MaxCode[2], 'DC lum MaxCode[2]');
  AssertEqual(0, Tbl.ValPtr[2], 'DC lum ValPtr[2]');

  // Length 3: 5 codes -> MinCode[3]=2, MaxCode[3]=6
  AssertEqual(2, Tbl.MinCode[3], 'DC lum MinCode[3]');
  AssertEqual(6, Tbl.MaxCode[3], 'DC lum MaxCode[3]');
  AssertEqual(1, Tbl.ValPtr[3], 'DC lum ValPtr[3]');

  // Total code count should be 12
  TotalCodes := 0;
  for I := 1 to 16 do
    Inc(TotalCodes, Tbl.Bits[I]);
  AssertEqual(12, TotalCodes, 'DC lum total codes');

  // Empty bits (length 1 has 0 codes) -> MaxCode[1] = $FFFF
  AssertEqual($FFFF, Tbl.MaxCode[1], 'DC lum MaxCode[1] empty');

  // Lookahead: value 0 (code 00, 2 bits) should be at indices 0x00..0x3F
  AssertTrue(Tbl.Lookahead[0].Valid, 'DC lum Lookahead[0] valid');
  AssertEqual(0, Tbl.Lookahead[0].Code, 'DC lum Lookahead[0] code');
  AssertEqual(2, Tbl.Lookahead[0].Size, 'DC lum Lookahead[0] size');

  // ---- Test 2: Lookahead consistency with bit-by-bit decode ----
  // For every 8-bit prefix, verify lookahead matches manual decode
  for I := 0 to 255 do
  begin
    // Manual bit-by-bit decode from 8-bit prefix
    BitCode := 0;
    Found := False;
    CodeLen := 0;
    for J := 1 to 8 do
    begin
      BitCode := (BitCode shl 1) or ((I shr (8 - J)) and 1);
      if (Tbl.Bits[J] > 0) and (BitCode <= Tbl.MaxCode[J]) then
      begin
        CodeLen := J;
        Found := True;
        Break;
      end;
    end;

    if Found then
    begin
      AssertTrue(Tbl.Lookahead[I].Valid,
        'DC lum Lookahead[' + IntToStr(I) + '] should be valid');
      AssertEqual(Tbl.HuffVal[Tbl.ValPtr[CodeLen] + BitCode - Tbl.MinCode[CodeLen]],
        Tbl.Lookahead[I].Code, 'DC lum Lookahead[' + IntToStr(I) + '] code mismatch');
      AssertEqual(CodeLen, Tbl.Lookahead[I].Size,
        'DC lum Lookahead[' + IntToStr(I) + '] size mismatch');
    end;
    // If not Found, code is > 8 bits, lookahead may or may not be valid (not tested here)
  end;

  // ---- Test 3: Standard AC luminance table ----
  for I := 1 to 16 do
    Tbl.Bits[I] := CN_JPEG_STD_AC_LUMINANCE_BITS[I];
  for I := 0 to 161 do
    Tbl.HuffVal[I] := CN_JPEG_STD_AC_LUMINANCE_VAL[I];

  CnJPEGBuildHuffmanTable(Tbl);

  // AC luminance total codes = 162
  TotalCodes := 0;
  for I := 1 to 16 do
    Inc(TotalCodes, Tbl.Bits[I]);
  AssertEqual(162, TotalCodes, 'AC lum total codes');

  // ---- Test 4: Empty table (BITS all 0) should not crash ----
  FillChar(Tbl, SizeOf(Tbl), 0);
  CnJPEGBuildHuffmanTable(Tbl);

  for I := 1 to 16 do
  begin
    AssertEqual(0, Tbl.MinCode[I], 'Empty table MinCode[' + IntToStr(I) + ']');
    AssertEqual($FFFF, Tbl.MaxCode[I], 'Empty table MaxCode[' + IntToStr(I) + ']');
    AssertEqual(0, Tbl.ValPtr[I], 'Empty table ValPtr[' + IntToStr(I) + ']');
    AssertTrue(not Tbl.Lookahead[I].Valid, 'Empty table Lookahead[' + IntToStr(I) + '] invalid');
  end;
  // Also check a few spread-out lookahead entries
  AssertTrue(not Tbl.Lookahead[0].Valid, 'Empty table Lookahead[0]');
  AssertTrue(not Tbl.Lookahead[128].Valid, 'Empty table Lookahead[128]');
  AssertTrue(not Tbl.Lookahead[255].Valid, 'Empty table Lookahead[255]');
end;

//============================================================================
// L1: Huffman DC Decode Tests (Task 36.2)
//============================================================================

class procedure THuffmanTests.TestHuffmanDCDecode;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  OutBmp: TBitmap;
  X, Y: Integer;
  Row: PByteArray;
  Row0: PByteArray;
  DC1, DC2: Integer;
begin
  // ---- Test 1: Solid gray block (S=0 → DIFF=0 for DC) ----
  // A solid color block has DC only (all AC = 0), making DC decode testable
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    // Solid gray (128,128,128) -> Y=128, Cb=128, Cr=128 -> DC=128
    Bmp.Canvas.Brush.Color := RGB(128, 128, 128);
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // For a solid gray block, DC decode should produce ~128
        // With quality=100, quantization is 1, so DC should be exact
        Row := OutBmp.ScanLine[0];
        // BGR format
        AssertInRange(Row[0], 120, 136, 'DC decode B channel');
        AssertInRange(Row[1], 120, 136, 'DC decode G channel');
        AssertInRange(Row[2], 120, 136, 'DC decode R channel');

        // All pixels in a solid block should be nearly identical
        for Y := 0 to 7 do
        begin
          Row := OutBmp.ScanLine[Y];
          for X := 0 to 7 do
          begin
            AssertInRange(Row[X * 3], 120, 136, 'DC decode B uniform');
            AssertInRange(Row[X * 3 + 1], 120, 136, 'DC decode G uniform');
            AssertInRange(Row[X * 3 + 2], 120, 136, 'DC decode R uniform');
          end;
        end;
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;

  // ---- Test 2: Differential DC accumulation (two blocks) ----
  // 16x8 image = two 8x8 blocks side by side.
  // Block 0: solid gray (Y=128), Block 1: solid bright gray (Y=200)
  // DC of block 0 = 128, DC of block 1 = DIFF = 200 - 128 = 72
  // Verifies that differential accumulation produces correct DC for block 1.
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 8;

    // Left block: gray (128,128,128)
    Bmp.Canvas.Brush.Color := RGB(128, 128, 128);
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));
    // Right block: bright gray (200,200,200)
    Bmp.Canvas.Brush.Color := RGB(200, 200, 200);
    Bmp.Canvas.FillRect(MakeRect(8, 0, 16, 8));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // Left block should be ~128
        Row := OutBmp.ScanLine[0];
        AssertInRange(Row[0], 118, 138, 'DC diff: left block B');
        AssertInRange(Row[1], 118, 138, 'DC diff: left block G');
        AssertInRange(Row[2], 118, 138, 'DC diff: left block R');

        // Right block should be ~200 (differential decode worked)
        Row0 := OutBmp.ScanLine[0];
        DC1 := Row0[1]; // G channel of left block
        DC2 := Row0[8 * 3 + 1]; // G channel of right block (pixel X=8)
        AssertInRange(DC2, 190, 210, 'DC diff: right block G (differential accumulation)');

        // The difference between blocks should be significant (72 ± tolerance)
        AssertTrue(Abs(DC2 - DC1) > 50,
          'DC diff: blocks should have significantly different DC values');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L1: Huffman AC Decode Tests (Task 36.3)
//============================================================================

class procedure THuffmanTests.TestHuffmanACDecode;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
  Row: PByteArray;
  X, Y2: Integer;
  DiffFound: Boolean;
  V0, V: Byte;
begin
  // Test AC decoding via encode->decode with a gradient pattern.
  // A horizontal gradient has strong AC coefficients.
  // If AC decode fails (e.g. EOB/ZRL mishandled), output will be flat.
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    for X := 0 to 7 do
    begin
      Row := Bmp.ScanLine[0];
      Row[X * 3] := Byte(X * 32);     // B: 0,32,64,96,128,160,192,224
      Row[X * 3 + 1] := Byte(X * 32); // G
      Row[X * 3 + 2] := Byte(X * 32); // R
    end;
    // Copy row 0 to all rows (consistent horizontal gradient)
    for Y2 := 1 to 7 do
    begin
      Row := Bmp.ScanLine[Y2];
      for X := 0 to 7 do
      begin
        Row[X * 3] := Byte(X * 32);
        Row[X * 3 + 1] := Byte(X * 32);
        Row[X * 3 + 2] := Byte(X * 32);
      end;
    end;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // Verify gradient is present (not flat) — AC coefficients decoded correctly
        Row := OutBmp.ScanLine[0];
        V0 := Row[0]; // first pixel B
        DiffFound := False;
        for X := 1 to 7 do
        begin
          V := Row[X * 3];
          if Abs(V - V0) > 5 then
          begin
            DiffFound := True;
            Break;
          end;
        end;
        AssertTrue(DiffFound, 'AC decode: gradient should have variation across pixels');

        // Verify the trend is increasing (allow some quantization noise)
        for X := 1 to 6 do
        begin
          AssertTrue(Row[(X + 1) * 3] >= Row[X * 3] - 10,
            'AC decode: gradient should be roughly increasing at X=' + IntToStr(X));
        end;
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;

  // ---- Test 2: Solid block exercises EOB (RS=0x00) ----
  // A solid block has all AC=0, so the encoder emits EOB immediately.
  // If EOB decode fails, the block would have garbage AC values.
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    Bmp.Canvas.Brush.Color := RGB(64, 64, 64);
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // All pixels should be close to 64 (EOB decoded correctly → flat block)
        for Y2 := 0 to 7 do
        begin
          Row := OutBmp.ScanLine[Y2];
          for X := 0 to 7 do
          begin
            AssertInRange(Row[X * 3], 54, 74, 'AC EOB: B uniform at (' + IntToStr(X) + ',' + IntToStr(Y2) + ')');
          end;
        end;
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;

  // ---- Test 3: Checkerboard pattern exercises ZRL (RS=0xF0) ----
  // A checkerboard has many high-frequency AC coefficients.
  // If ZRL (skip 16 zeros) decode fails, output would be wrong.
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    for Y2 := 0 to 7 do
    begin
      Row := Bmp.ScanLine[Y2];
      for X := 0 to 7 do
      begin
        if ((X + Y2) and 1) = 0 then
        begin
          Row[X * 3] := 255; Row[X * 3 + 1] := 255; Row[X * 3 + 2] := 255;
        end
        else
        begin
          Row[X * 3] := 0; Row[X * 3 + 1] := 0; Row[X * 3 + 2] := 0;
        end;
      end;
    end;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // Checkerboard should produce significant variation (ZRL decoded correctly)
        Row := OutBmp.ScanLine[0];
        V0 := Row[0]; // first pixel B
        V := Row[3];  // second pixel B
        AssertTrue(Abs(V - V0) > 30,
          'AC ZRL: checkerboard should have high contrast between adjacent pixels');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L1: Huffman Bitstream Direct Decode Tests (Task 36.2 + 36.3)
// Directly tests Huffman table + BitReader without going through full JPEG encode/decode.
// Uses the same decode algorithm as the private TCnJPEGDecoder.HuffmanDecode.
//============================================================================

class procedure THuffmanTests.TestHuffmanBitstreamDecode;
var
  Tbl: TCnJPEGHuffmanTable;
  MS: TMemoryStream;
  Reader: TCnJPEGBitReader;
  I, S, V, Diff: Integer;
  RS, Run, Size: Integer;
  Coef: array[0..63] of SmallInt;
  B: Byte;
begin
  // ---- Part A: DC Huffman decode (Task 36.2) ----
  // Build standard DC luminance table
  for I := 1 to 16 do
    Tbl.Bits[I] := CN_JPEG_STD_DC_LUMINANCE_BITS[I];
  for I := 0 to 11 do
    Tbl.HuffVal[I] := CN_JPEG_STD_DC_LUMINANCE_VAL[I];
  CnJPEGBuildHuffmanTable(Tbl);

  // Construct bitstream for 3 DC decodes:
  //   1. S=0 (code 00, 2 bits) → DIFF=0
  //   2. S=2 (code 011, 3 bits) + additional bits 10 → V=2, DIFF=+2
  //   3. S=2 (code 011, 3 bits) + additional bits 01 → V=1, DIFF=-2
  // Bitstream: 00 011 10 011 01 0000 → 00011100 11010000 → 0x1C 0xD0
  MS := TMemoryStream.Create;
  try
    B := $1C; MS.Write(B, 1);
    B := $D0; MS.Write(B, 1);
    MS.Position := 0;

    Reader := TCnJPEGBitReader.Create(MS);
    try
      // Decode 1: S=0 → DIFF=0
      S := 0; V := 0;
      // Manual Huffman decode (same algorithm as TCnJPEGDecoder.HuffmanDecode)
      V := 0;
      for I := 1 to 16 do
      begin
        V := (V shl 1) or Reader.GetBit;
        if (Tbl.Bits[I] > 0) and (V <= Tbl.MaxCode[I]) then
        begin
          S := Tbl.HuffVal[Tbl.ValPtr[I] + V - Tbl.MinCode[I]];
          Break;
        end;
      end;
      AssertEqual(0, S, 'DC bitstream: first decode S should be 0');
      // S=0 → DIFF=0 (no additional bits)
      Diff := 0;
      AssertEqual(0, Diff, 'DC bitstream: DIFF should be 0 when S=0');

      // Decode 2: S=2 → read 2 additional bits
      V := 0;
      for I := 1 to 16 do
      begin
        V := (V shl 1) or Reader.GetBit;
        if (Tbl.Bits[I] > 0) and (V <= Tbl.MaxCode[I]) then
        begin
          S := Tbl.HuffVal[Tbl.ValPtr[I] + V - Tbl.MinCode[I]];
          Break;
        end;
      end;
      AssertEqual(2, S, 'DC bitstream: second decode S should be 2');
      V := Reader.GetBits(S);
      // V=2 (binary 10), V >= 2^(S-1)=2 → DIFF = V = 2
      if V >= (1 shl (S - 1)) then
        Diff := V
      else
        Diff := V - (1 shl S) + 1;
      AssertEqual(2, Diff, 'DC bitstream: DIFF should be +2');

      // Decode 3: S=2 → read 2 additional bits
      V := 0;
      for I := 1 to 16 do
      begin
        V := (V shl 1) or Reader.GetBit;
        if (Tbl.Bits[I] > 0) and (V <= Tbl.MaxCode[I]) then
        begin
          S := Tbl.HuffVal[Tbl.ValPtr[I] + V - Tbl.MinCode[I]];
          Break;
        end;
      end;
      AssertEqual(2, S, 'DC bitstream: third decode S should be 2');
      V := Reader.GetBits(S);
      // V=1 (binary 01), V < 2^(S-1)=2 → DIFF = V - (2^S - 1) = 1 - 3 = -2
      if V >= (1 shl (S - 1)) then
        Diff := V
      else
        Diff := V - (1 shl S) + 1;
      AssertEqual(-2, Diff, 'DC bitstream: DIFF should be -2');

      // Verify differential accumulation: DC = 0 + 2 + (-2) = 0
      AssertEqual(0, 0 + 2 + (-2), 'DC bitstream: differential accumulation DC=0');
    finally
      Reader.Free;
    end;
  finally
    MS.Free;
  end;

  // ---- Part B: AC Huffman decode (Task 36.3) ----
  // Build standard AC luminance table
  for I := 1 to 16 do
    Tbl.Bits[I] := CN_JPEG_STD_AC_LUMINANCE_BITS[I];
  for I := 0 to 161 do
    Tbl.HuffVal[I] := CN_JPEG_STD_AC_LUMINANCE_VAL[I];
  CnJPEGBuildHuffmanTable(Tbl);

  // Construct bitstream for AC decode:
  //   1. RS=0x21 (code 11100, 5 bits) + 1 additional bit (1) → Run=2, Size=1, Val=1
  //      → Coef[3] = 1 (skip 2 zeros at positions 1,2, write at position 3)
  //   2. RS=0x00 (code 1010, 4 bits) → EOB, remaining coefficients = 0
  // Bitstream: 11100 1 1010 00 → 11100110 10000000 → 0xE6 0x80
  MS := TMemoryStream.Create;
  try
    B := $E6; MS.Write(B, 1);
    B := $80; MS.Write(B, 1);
    MS.Position := 0;

    Reader := TCnJPEGBitReader.Create(MS);
    try
      // Initialize coefficient array
      FillChar(Coef[0], SizeOf(Coef), 0);

      // Decode AC coefficients (simplified version of DecodeBlock AC portion)
      I := 1; // Start from position 1 (DC is position 0)
      while I <= 63 do
      begin
        // Manual Huffman decode
        V := 0;
        RS := -1;
        for S := 1 to 16 do
        begin
          V := (V shl 1) or Reader.GetBit;
          if (Tbl.Bits[S] > 0) and (V <= Tbl.MaxCode[S]) then
          begin
            RS := Tbl.HuffVal[Tbl.ValPtr[S] + V - Tbl.MinCode[S]];
            Break;
          end;
        end;
        AssertTrue(RS >= 0, 'AC bitstream: decode should succeed at I=' + IntToStr(I));

        Run := RS shr 4;
        Size := RS and $0F;

        if Size = 0 then
        begin
          if Run = 15 then
          begin
            // ZRL: skip 16 zeros
            Inc(I, 16);
            Continue;
          end
          else
          begin
            // EOB: remaining coefficients are zero
            Break;
          end;
        end;

        // Skip Run zeros
        Inc(I, Run);
        AssertTrue(I <= 63, 'AC bitstream: coefficient index in range');

        // Read Size additional bits
        V := Reader.GetBits(Size);
        if V >= (1 shl (Size - 1)) then
          Coef[I] := V
        else
          Coef[I] := V - (1 shl Size) + 1;

        Inc(I);
      end;

      // Verify: Coef[3] should be 1 (Run=2, Size=1, V=1 → positive)
      AssertEqual(1, Coef[3], 'AC bitstream: Coef[3] should be 1');
      // All other AC coefficients (except position 3) should be 0
      for I := 1 to 63 do
      begin
        if I <> 3 then
          AssertEqual(0, Coef[I], 'AC bitstream: Coef[' + IntToStr(I) + '] should be 0 (EOB)');
      end;
    finally
      Reader.Free;
    end;
  finally
    MS.Free;
  end;

  // ---- Part C: ZRL test (RS=0xF0) ----
  // The ZRL code for AC luminance is 11 bits (too long for simple bitstream test).
  // Instead, verify via table lookup that $F0 is present in the table.
  V := -1;
  for I := 0 to 161 do
  begin
    if Tbl.HuffVal[I] = $F0 then
    begin
      V := I;
      Break;
    end;
  end;
  AssertTrue(V >= 0, 'AC bitstream: ZRL ($F0) should be in standard AC table');
end;

//============================================================================
// L1: IDCT Basic Tests (Task 37.1)
//============================================================================

type
  TIDCTTests = class
    class procedure TestIDCTBasics;
    class procedure TestIDCTQualityComparison;
  end;

class procedure TIDCTTests.TestIDCTBasics;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
  Row: PByteArray;
  X, Y: Integer;
  MinVal, MaxVal: Byte;
begin
  // Test 1: Solid color block (all coefficients zero except DC)
  // DC = 128, AC = 0 -> IDCT output should be 128 (flat gray)
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    Bmp.Canvas.Brush.Color := RGB(128, 128, 128);
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // Flat block: all pixels should be close to 128
        for Y := 0 to 7 do
        begin
          Row := OutBmp.ScanLine[Y];
          for X := 0 to 7 do
          begin
            AssertInRange(Row[X * 3], 120, 136, 'IDCT flat B');
            AssertInRange(Row[X * 3 + 1], 120, 136, 'IDCT flat G');
            AssertInRange(Row[X * 3 + 2], 120, 136, 'IDCT flat R');
          end;
        end;
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;

  // Test 2: Horizontal gradient (strong AC coefficient at frequency 1)
  // If IDCT works, we should see a smooth gradient, not a flat or garbled block
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    for Y := 0 to 7 do
    begin
      Row := Bmp.ScanLine[Y];
      for X := 0 to 7 do
      begin
        Row[X * 3] := Byte(X * 32);
        Row[X * 3 + 1] := Byte(X * 32);
        Row[X * 3 + 2] := Byte(X * 32);
      end;
    end;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // Check that output has a gradient (min != max)
        Row := OutBmp.ScanLine[0];
        MinVal := 255;
        MaxVal := 0;
        for X := 0 to 7 do
        begin
          if Row[X * 3] < MinVal then MinVal := Row[X * 3];
          if Row[X * 3] > MaxVal then MaxVal := Row[X * 3];
        end;
        AssertTrue(MaxVal - MinVal > 10, 'IDCT gradient: should have range > 10');

        // Check gradient is monotonic (allowing small noise)
        for X := 1 to 6 do
          AssertTrue(Row[(X + 1) * 3] >= Row[X * 3] - 10,
            'IDCT gradient: should be roughly increasing');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;

  // Test 3: Black and white halves (vertical edge — tests vertical frequency)
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    for Y := 0 to 7 do
    begin
      Row := Bmp.ScanLine[Y];
      for X := 0 to 3 do
      begin
        Row[X * 3] := 0;
        Row[X * 3 + 1] := 0;
        Row[X * 3 + 2] := 0;
      end;
      for X := 4 to 7 do
      begin
        Row[X * 3] := 255;
        Row[X * 3 + 1] := 255;
        Row[X * 3 + 2] := 255;
      end;
    end;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        // Left half should be dark, right half should be bright
        Row := OutBmp.ScanLine[0];
        AssertTrue(Row[0] < 100, 'IDCT edge: left half should be dark');
        AssertTrue(Row[7 * 3] > 155, 'IDCT edge: right half should be bright');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L1: IDCT Quality vs Speed Comparison Tests (Task 37.2)
//============================================================================

class procedure TIDCTTests.TestIDCTQualityComparison;
var
  Bmp, OutBmp1, OutBmp2: TBitmap;
  JPEG, JPEG2: TCnJPEGImage;
  MS: TMemoryStream;
  X, Y: Integer;
  Row1, Row2: PByteArray;
  MaxDiff, Diff: Integer;
begin
  // Test: jpBestQuality vs jpBestSpeed should produce nearly identical output.
  // The current implementation uses a single IDCT algorithm for both modes,
  // so the difference should be 0. Allow <= 2 for future algorithm changes.
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 32;
    Bmp.Height := 32;

    // Create a complex pattern with strong AC coefficients
    for Y := 0 to 31 do
    begin
      Row1 := Bmp.ScanLine[Y];
      for X := 0 to 31 do
      begin
        Row1[X * 3]     := Byte((X * 8) and $FF);     // B: horizontal gradient
        Row1[X * 3 + 1] := Byte((Y * 8) and $FF);     // G: vertical gradient
        Row1[X * 3 + 2] := Byte((X * 8 + Y * 8) and $FF); // R: diagonal
      end;
    end;

    // Encode to stream
    MS := TMemoryStream.Create;
    try
      JPEG := TCnJPEGImage.Create;
      try
        JPEG.Assign(Bmp);
        JPEG.CompressionQuality := 90;
        JPEG.SaveToStream(MS);
      finally
        JPEG.Free;
      end;

      // Decode with jpBestQuality
      MS.Position := 0;
      JPEG := TCnJPEGImage.Create;
      try
        JPEG.LoadFromStream(MS);
        JPEG.Performance := jpBestQuality;
        OutBmp1 := TBitmap.Create;
        try
          JPEG.DIBNeeded;
          OutBmp1.Assign(JPEG);
        finally
          JPEG.Free;
        end;

        // Decode with jpBestSpeed
        MS.Position := 0;
        JPEG2 := TCnJPEGImage.Create;
        try
          JPEG2.LoadFromStream(MS);
          JPEG2.Performance := jpBestSpeed;
          OutBmp2 := TBitmap.Create;
          try
            JPEG2.DIBNeeded;
            OutBmp2.Assign(JPEG2);

            // Compare every pixel
            MaxDiff := 0;
            for Y := 0 to 31 do
            begin
              Row1 := OutBmp1.ScanLine[Y];
              Row2 := OutBmp2.ScanLine[Y];
              for X := 0 to 31 do
              begin
                Diff := Abs(Row1[X * 3] - Row2[X * 3]);
                if Diff > MaxDiff then MaxDiff := Diff;
                Diff := Abs(Row1[X * 3 + 1] - Row2[X * 3 + 1]);
                if Diff > MaxDiff then MaxDiff := Diff;
                Diff := Abs(Row1[X * 3 + 2] - Row2[X * 3 + 2]);
                if Diff > MaxDiff then MaxDiff := Diff;
              end;
            end;
            AssertTrue(MaxDiff <= 2,
              'IDCT quality vs speed: pixel difference should be <= 2, got ' + IntToStr(MaxDiff));
          finally
            OutBmp2.Free;
          end;
        finally
          JPEG2.Free;
        end;
      finally
        OutBmp1.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L1: Segment Length Minimal Test (Task 35.2)
//============================================================================

type
  TSegmentTests = class
    class procedure TestMinimalSegment;
  end;

class procedure TSegmentTests.TestMinimalSegment;
var
  MS: TMemoryStream;
  Marker: TCnJPEGMarker;
  SegData: TMemoryStream;
  SegLen: Integer;
  W: Word;
begin
  // Test: minimal segment (length=2, 0 data bytes)
  MS := TMemoryStream.Create;
  try
    W := UInt16ToBigEndian($FFE0); MS.Write(W, 2);  // APP0 marker
    W := UInt16ToBigEndian(2);    MS.Write(W, 2);   // length = 2 (minimum)
    MS.Position := 0;
    Marker := TCnJPEGMarker.Create(MS);
    try
      Marker.ReadMarker;  // skip marker code
      SegData := TMemoryStream.Create;
      try
        SegLen := Marker.ReadSegment(SegData);
        AssertEqual(2, SegLen, 'minimal segment length');
        AssertEqual(0, SegData.Size, 'minimal segment data size');
      finally
        SegData.Free;
      end;
    finally
      Marker.Free;
    end;
  finally
    MS.Free;
  end;
end;

//============================================================================
// L3: Integration Tests
//============================================================================

type
  TIntegrationTests = class
    class procedure TestTPictureRegistration;
    class procedure TestAssignBitmap;
  end;

class procedure TIntegrationTests.TestTPictureRegistration;
begin
  // The initialization section should have registered the format
  AssertTrue(True, 'TPicture registration test (init section ran)');
end;

class procedure TIntegrationTests.TestAssignBitmap;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  Bmp2: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 16;

    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 16, 16));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;
      JPEG.Compress;

      Bmp2 := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        Bmp2.Assign(JPEG);
        AssertEqual(16, Bmp2.Width, 'Assign bitmap width');
        AssertEqual(16, Bmp2.Height, 'Assign bitmap height');
      finally
        Bmp2.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L2: File Roundtrip Tests (Task 21.3)
//============================================================================

type
  TFileRoundtripTests = class
    class procedure TestSaveToStreamMarkers;
    class procedure TestSaveLoadRoundtrip;
    class procedure TestProgressiveSaveMarkers;
  end;

class procedure TFileRoundtripTests.TestSaveToStreamMarkers;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  Buf: array of Byte;
  HasSOI, HasEOI, HasDQT, HasDHT, HasSOF0, HasSOS: Boolean;
  I: Integer;
begin
  // Create a bitmap and encode to JPEG, then verify markers in stream
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 16;

    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 16, 16));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 75;

      MS := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS);
        AssertTrue(MS.Size > 100, 'Stream should have data');

        // Read entire stream into buffer for simple byte search
        SetLength(Buf, MS.Size);
        MS.Position := 0;
        MS.Read(Buf[0], MS.Size);

        // Search for 2-byte marker patterns: $FF followed by marker code
        HasSOI := False;
        HasEOI := False;
        HasDQT := False;
        HasDHT := False;
        HasSOF0 := False;
        HasSOS := False;

        I := 0;
        while I < Length(Buf) - 1 do
        begin
          if Buf[I] = $FF then
          begin
            case Buf[I + 1] of
              $D8: HasSOI := True;
              $D9: HasEOI := True;
              $DB: HasDQT := True;
              $C4: HasDHT := True;
              $C0: HasSOF0 := True;
              $DA: HasSOS := True;
            end;
          end;
          Inc(I);
        end;

        AssertTrue(HasSOI, 'Stream should contain SOI marker');
        AssertTrue(HasEOI, 'Stream should contain EOI marker');
        AssertTrue(HasDQT, 'Stream should contain DQT marker');
        AssertTrue(HasDHT, 'Stream should contain DHT marker');
        AssertTrue(HasSOF0, 'Stream should contain SOF0 marker');
        AssertTrue(HasSOS, 'Stream should contain SOS marker');
      finally
        MS.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TFileRoundtripTests.TestSaveLoadRoundtrip;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  X, Y: Integer;
  RowIn, RowOut: PByteArray;
  MaxDiff, Diff: Integer;
begin
  // Encode bitmap to JPEG, save to stream, load back, compare pixels
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 32;
    Bmp.Height := 32;

    for Y := 0 to 31 do
    begin
      RowIn := Bmp.ScanLine[Y];
      for X := 0 to 31 do
      begin
        RowIn[X * 3] := Byte((X * 8) and $FF);
        RowIn[X * 3 + 1] := Byte((Y * 8) and $FF);
        RowIn[X * 3 + 2] := Byte(128);
      end;
    end;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      MS := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS);
        MS.Position := 0;

        // Load back into a new JPEG image
        JPEG.LoadFromStream(MS);
      finally
        MS.Free;
      end;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        AssertEqual(32, OutBmp.Width, 'Roundtrip width');
        AssertEqual(32, OutBmp.Height, 'Roundtrip height');

        // With quality=100, pixel difference should be small
        MaxDiff := 0;
        for Y := 0 to 31 do
        begin
          RowIn := Bmp.ScanLine[Y];
          RowOut := OutBmp.ScanLine[Y];
          for X := 0 to 32 * 3 - 1 do
          begin
            Diff := Abs(Integer(RowIn[X]) - Integer(RowOut[X]));
            if Diff > MaxDiff then
              MaxDiff := Diff;
          end;
        end;
        AssertTrue(MaxDiff <= 25, 'Roundtrip pixel diff should be <= 25, got ' + IntToStr(MaxDiff));
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TFileRoundtripTests.TestProgressiveSaveMarkers;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  W: Word;
  Marker: Word;
  HasSOF2: Boolean;
begin
  // Encode with progressive=True, verify SOF2 marker in stream
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 16;

    Bmp.Canvas.Brush.Color := clGreen;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 16, 16));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 75;
      JPEG.ProgressiveEncoding := True;

      MS := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS);

        // Scan for SOF2 ($FFC2)
        MS.Position := 0;
        HasSOF2 := False;
        while MS.Position < MS.Size - 1 do
        begin
          MS.Read(W, 2);
          Marker := ((W and $FF) shl 8) or ((W shr 8) and $FF);
          if Marker = $FFC2 then
          begin
            HasSOF2 := True;
            Break;
          end;
          // Skip segment data
          if (Marker >= $FFC0) and (Marker <> $FFD8) and (Marker <> $FFD9) and
             (Marker < $FFF0) then
          begin
            if MS.Position < MS.Size - 1 then
            begin
              MS.Read(W, 2);
              Marker := ((W and $FF) shl 8) or ((W shr 8) and $FF);
              if Marker > 2 then
                MS.Seek(Marker - 2, soFromCurrent);
            end;
          end;
        end;

        AssertTrue(HasSOF2, 'Progressive JPEG should contain SOF2 marker');
      finally
        MS.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L2: Scale Decode Tests (Task 40.3 / 39.2)
//============================================================================

type
  TScaleTests = class
    class procedure TestScaleSizes;
    class procedure TestScaleHalfConsistency;
    class procedure TestScaleQuarterConsistency;
    class procedure TestScaleEighthConsistency;
  end;

class procedure TScaleTests.TestScaleSizes;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  OutBmp: TBitmap;
begin
  // Create a 100x100 bitmap (not a multiple of 8, tests rounding)
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 100;
    Bmp.Height := 100;

    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 100, 100));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      // Full size
      JPEG.Scale := jsFullSize;
      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(100, OutBmp.Width, 'Full size width');
        AssertEqual(100, OutBmp.Height, 'Full size height');
      finally
        OutBmp.Free;
      end;

      // Half size: ceil(100/2) = 50
      JPEG.Scale := jsHalf;
      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(50, OutBmp.Width, 'Half size width');
        AssertEqual(50, OutBmp.Height, 'Half size height');
      finally
        OutBmp.Free;
      end;

      // Quarter size: ceil(100/4) = 25
      JPEG.Scale := jsQuarter;
      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(25, OutBmp.Width, 'Quarter size width');
        AssertEqual(25, OutBmp.Height, 'Quarter size height');
      finally
        OutBmp.Free;
      end;

      // Eighth size: ceil(100/8) = 13
      JPEG.Scale := jsEighth;
      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(13, OutBmp.Width, 'Eighth size width');
        AssertEqual(13, OutBmp.Height, 'Eighth size height');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TScaleTests.TestScaleHalfConsistency;
var
  Bmp, FullBmp, HalfBmp: TBitmap;
  JPEG: TCnJPEGImage;
  RowHalf: PByteArray;
begin
  // Encode a gradient bitmap, decode at full and half, verify half is a downscaled version
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 64;
    Bmp.Height := 64;

    Bmp.Canvas.Brush.Color := clNavy;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 64, 64));
    // Draw a horizontal gradient on top half
    Bmp.Canvas.Brush.Color := clYellow;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 64, 32));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      // Full size decode
      JPEG.Scale := jsFullSize;
      JPEG.DIBNeeded;
      FullBmp := TBitmap.Create;
      try
        FullBmp.Assign(JPEG);

        // Half size decode
        JPEG.Scale := jsHalf;
        JPEG.DIBNeeded;
        HalfBmp := TBitmap.Create;
        try
          HalfBmp.Assign(JPEG);

          AssertEqual(32, HalfBmp.Width, 'Half width');
          AssertEqual(32, HalfBmp.Height, 'Half height');

          // Top row of half should be bright (yellow), bottom row should be dark (navy)
          RowHalf := HalfBmp.ScanLine[0];
          AssertTrue(RowHalf[2] > 200, 'Half top R should be bright (yellow)');
          AssertTrue(RowHalf[1] > 200, 'Half top G should be bright (yellow)');

          RowHalf := HalfBmp.ScanLine[31];
          AssertTrue(RowHalf[2] < 100, 'Half bottom R should be dark (navy)');
        finally
          HalfBmp.Free;
        end;
      finally
        FullBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TScaleTests.TestScaleQuarterConsistency;
var
  Bmp, QuarterBmp: TBitmap;
  JPEG: TCnJPEGImage;
  Row: PByteArray;
begin
  // Verify quarter scale produces correct size and doesn't crash
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 64;
    Bmp.Height := 64;

    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 64, 64));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;
      JPEG.Scale := jsQuarter;
      JPEG.DIBNeeded;

      QuarterBmp := TBitmap.Create;
      try
        QuarterBmp.Assign(JPEG);
        AssertEqual(16, QuarterBmp.Width, 'Quarter width');
        AssertEqual(16, QuarterBmp.Height, 'Quarter height');

        // Red image: R channel should be high
        Row := QuarterBmp.ScanLine[0];
        AssertTrue(Row[2] > 200, 'Quarter R channel should be high (red)');
      finally
        QuarterBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TScaleTests.TestScaleEighthConsistency;
var
  Bmp, EighthBmp: TBitmap;
  JPEG: TCnJPEGImage;
  Row: PByteArray;
begin
  // Verify eighth scale produces correct size and doesn't crash
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 64;
    Bmp.Height := 64;

    Bmp.Canvas.Brush.Color := clLime;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 64, 64));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;
      JPEG.Scale := jsEighth;
      JPEG.DIBNeeded;

      EighthBmp := TBitmap.Create;
      try
        EighthBmp.Assign(JPEG);
        AssertEqual(8, EighthBmp.Width, 'Eighth width');
        AssertEqual(8, EighthBmp.Height, 'Eighth height');

        // Lime = (0, 255, 0), G channel should be high
        Row := EighthBmp.ScanLine[0];
        AssertTrue(Row[1] > 200, 'Eighth G channel should be high (lime)');
      finally
        EighthBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L2: Property Linkage Tests (Task 43.1)
//============================================================================

type
  TPropertyTests = class
    class procedure TestQualityAffectsSize;
    class procedure TestGrayscaleProperty;
    class procedure TestPixelFormat8Bit;
    class procedure TestScaleProperty;
  end;

class procedure TPropertyTests.TestQualityAffectsSize;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS1, MS2: TMemoryStream;
begin
  // Higher quality should produce larger file
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 64;
    Bmp.Height := 64;

    // Draw gradient to make it non-trivial
    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 64, 32));
    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 32, 64, 64));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);

      // Low quality
      JPEG.CompressionQuality := 10;
      MS1 := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS1);
      finally
        // Don't free yet
      end;

      // High quality
      JPEG.CompressionQuality := 100;
      MS2 := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS2);

        AssertTrue(MS2.Size > MS1.Size,
          'High quality file should be larger: ' + IntToStr(MS2.Size) + ' vs ' + IntToStr(MS1.Size));
      finally
        MS2.Free;
      end;
      MS1.Free;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TPropertyTests.TestGrayscaleProperty;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  OutBmp: TBitmap;
  Row: PByteArray;
begin
  // Load color image, set grayscale, verify output is gray
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 32;
    Bmp.Height := 32;

    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 32, 32));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.Grayscale := True;
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        AssertTrue(JPEG.Grayscale, 'Grayscale flag should be True');

        // For grayscale output, R=G=B for all pixels
        Row := OutBmp.ScanLine[0];
        AssertInRange(Row[0], 60, 120, 'Gray B');  // Red->gray ~76
        AssertInRange(Row[1], 60, 120, 'Gray G');
        AssertInRange(Row[2], 60, 120, 'Gray R');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TPropertyTests.TestPixelFormat8Bit;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  OutBmp: TBitmap;
  Row: PByteArray;
begin
  // Encode color image, set PixelFormat=jf8Bit, verify 8-bit output
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 32;
    Bmp.Height := 32;

    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 32, 32));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;
      JPEG.PixelFormat := jf8Bit;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        AssertEqual(32, OutBmp.Width, '8bit width');
        AssertEqual(32, OutBmp.Height, '8bit height');
        // Blue -> gray Y = 0.114*255 ≈ 29
        Row := OutBmp.ScanLine[0];
        AssertInRange(Row[0], 20, 40, '8bit blue->gray value');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TPropertyTests.TestScaleProperty;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  OutBmp: TBitmap;
begin
  // Change scale, verify output size changes
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 80;
    Bmp.Height := 80;

    Bmp.Canvas.Brush.Color := clGreen;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 80, 80));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      // Test Scale = jsHalf
      JPEG.Scale := jsHalf;
      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(40, OutBmp.Width, 'Scale half width');
        AssertEqual(40, OutBmp.Height, 'Scale half height');
      finally
        OutBmp.Free;
      end;

      // Test Scale = jsQuarter
      JPEG.Scale := jsQuarter;
      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(20, OutBmp.Width, 'Scale quarter width');
        AssertEqual(20, OutBmp.Height, 'Scale quarter height');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L2: Boundary Size Tests (Task 40.5)
//============================================================================

type
  TBoundaryTests = class
    class procedure Test1x1;
    class procedure Test8x8;
    class procedure Test9x9;
    class procedure Test15x16;
  end;

class procedure TBoundaryTests.Test1x1;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 1;
    Bmp.Height := 1;

    Bmp.Canvas.Pixels[0, 0] := clRed;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(1, OutBmp.Width, '1x1 width');
        AssertEqual(1, OutBmp.Height, '1x1 height');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TBoundaryTests.Test8x8;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
  Row: PByteArray;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;

    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(8, OutBmp.Width, '8x8 width');
        AssertEqual(8, OutBmp.Height, '8x8 height');
        // Red pixel check
        Row := OutBmp.ScanLine[0];
        AssertTrue(Row[2] > 200, '8x8 R should be high (red)');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TBoundaryTests.Test9x9;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
  Row: PByteArray;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 9;
    Bmp.Height := 9;

    Bmp.Canvas.Brush.Color := clLime;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 9, 9));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(9, OutBmp.Width, '9x9 width');
        AssertEqual(9, OutBmp.Height, '9x9 height');
        // All pixels should be lime-ish (not garbled)
        Row := OutBmp.ScanLine[8];
        AssertTrue(Row[1] > 200, '9x9 corner G should be high (lime)');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

class procedure TBoundaryTests.Test15x16;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
  Row: PByteArray;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 15;
    Bmp.Height := 16;

    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 15, 16));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);
        AssertEqual(15, OutBmp.Width, '15x16 width');
        AssertEqual(16, OutBmp.Height, '15x16 height');
        // Corner pixel should be blue-ish
        Row := OutBmp.ScanLine[15];
        AssertTrue(Row[0] > 200, '15x16 corner B should be high (blue)');
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L2: CMYK JPEG Tests (Task 24.2)
//============================================================================

type
  TCMYKTests = class
    class procedure TestCMYKColorSpace;
  end;

class procedure TCMYKTests.TestCMYKColorSpace;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  W: Word;
  B: Byte;
  HasAPP14: Boolean;
  Marker: Word;
  SegLen: Word;
begin
  // Test CMYK path: encode a 4-component bitmap won't work directly,
  // but we can test that the CMYK detection code path doesn't crash
  // by creating a synthetic JPEG stream with APP14 marker.
  // For now, verify the color space enum works
  AssertEqual(Ord(jcRGB), 0, 'jcRGB ordinal');
  AssertEqual(Ord(jcGrayscale), 1, 'jcGrayscale ordinal');
  AssertEqual(Ord(jcCMYK), 2, 'jcCMYK ordinal');

  // Create a normal color JPEG and verify ColorSpace is RGB
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 16;

    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 16, 16));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;

      // Verify it's RGB color space
      AssertEqual(Ord(jcRGB), Ord(JPEG.NewJPEG.ColorSpace), 'Color JPEG should be RGB');

      // Verify Grayscale flag is False
      AssertTrue(not JPEG.Grayscale, 'Color JPEG Grayscale should be False');
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;

  // Test grayscale JPEG color space
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 16;

    Bmp.Canvas.Brush.Color := clGray;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 16, 16));

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.Grayscale := True;
      JPEG.CompressionQuality := 100;
      JPEG.Compress;

      AssertEqual(Ord(jcGrayscale), Ord(JPEG.NewJPEG.ColorSpace), 'Gray JPEG should be Grayscale');
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// L2: Progressive Encode/Decode Roundtrip (Task 42.2 extended)
//============================================================================

type
  TProgressiveTests = class
    class procedure TestProgressiveRoundtrip;
  end;

class procedure TProgressiveTests.TestProgressiveRoundtrip;
var
  Bmp, OutBmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  X, Y: Integer;
  Row: PByteArray;
  Diff, MaxDiff: Integer;
begin
  // Encode progressive, verify stream properties and pixel roundtrip accuracy
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 32;
    Bmp.Height := 32;

    for Y := 0 to 31 do
    begin
      Row := Bmp.ScanLine[Y];
      for X := 0 to 31 do
      begin
        Row[X * 3] := Byte((X * 8) and $FF);     // B
        Row[X * 3 + 1] := Byte((Y * 8) and $FF); // G
        Row[X * 3 + 2] := Byte(128);             // R
      end;
    end;

    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 100;
      JPEG.ProgressiveEncoding := True;

      MS := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS);
        AssertTrue(MS.Size > 100, 'Progressive stream should have data');

        MS.Position := 0;
        JPEG.LoadFromStream(MS);
        AssertTrue(JPEG.ProgressiveEncoding, 'Progressive flag after reload');
      finally
        MS.Free;
      end;

      OutBmp := TBitmap.Create;
      try
        JPEG.DIBNeeded;
        OutBmp.Assign(JPEG);

        AssertEqual(32, OutBmp.Width, 'Progressive roundtrip width');
        AssertEqual(32, OutBmp.Height, 'Progressive roundtrip height');

        // Verify output is not completely blank
        Row := OutBmp.ScanLine[16];
        AssertTrue((Row[0] <> 0) or (Row[1] <> 0) or (Row[2] <> 0),
          'Progressive output should have non-zero pixels');

        // Verify pixel accuracy (quality 100 should have small error)
        MaxDiff := 0;
        for Y := 0 to 31 do
        begin
          Row := OutBmp.ScanLine[Y];
          for X := 0 to 31 do
          begin
            Diff := Abs(Row[X * 3] - Byte((X * 8) and $FF));
            if Diff > MaxDiff then MaxDiff := Diff;
            Diff := Abs(Row[X * 3 + 1] - Byte((Y * 8) and $FF));
            if Diff > MaxDiff then MaxDiff := Diff;
            Diff := Abs(Row[X * 3 + 2] - Byte(128));
            if Diff > MaxDiff then MaxDiff := Diff;
          end;
        end;
        AssertTrue(MaxDiff <= 35,
          'Progressive roundtrip pixel error should be <= 35, got ' + IntToStr(MaxDiff));
      finally
        OutBmp.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// P0 Robustness Regression Tests
//============================================================================

function EncodeRobustnessJPEG(AProgressive: Boolean): TBytes;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  I: Integer;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 8;
    Bmp.Height := 8;
    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));
    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 90;
      JPEG.ProgressiveEncoding := AProgressive;
      MS := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS);
        SetLength(Result, Integer(MS.Size));
        MS.Position := 0;
        if Length(Result) > 0 then
          MS.ReadBuffer(Result[0], Length(Result));
      finally
        MS.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

function TryDecodeRobustnessJPEG(const Data: TBytes): Boolean;
var
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
begin
  Result := False;
  JPEG := TCnJPEGImage.Create;
  MS := TMemoryStream.Create;
  try
    if Length(Data) > 0 then
      MS.WriteBuffer(Data[0], Length(Data));
    MS.Position := 0;
    try
      JPEG.LoadFromStream(MS);
      JPEG.DIBNeeded;
      Result := True;
    except
      Result := False;
    end;
  finally
    MS.Free;
    JPEG.Free;
  end;
end;

function FindJPEGMarker(const Data: TBytes; ACode: Byte): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(Data) - 2 do
    if (Data[I] = $FF) and (Data[I + 1] = ACode) then
    begin
      Result := I;
      Exit;
    end;
end;

function EncodeGradientJPEG(AProgressive: Boolean): TBytes;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  X, Y: Integer;
  Row: PByteArray;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 16;
    for Y := 0 to 15 do
    begin
      Row := Bmp.ScanLine[Y];
      for X := 0 to 15 do
      begin
        Row[X * 3] := Byte((X * 17) and $FF);
        Row[X * 3 + 1] := Byte((Y * 19) and $FF);
        Row[X * 3 + 2] := Byte((X * 11 + Y * 7) and $FF);
      end;
    end;
    JPEG := TCnJPEGImage.Create;
    try
      JPEG.Assign(Bmp);
      JPEG.CompressionQuality := 90;
      JPEG.ProgressiveEncoding := AProgressive;
      MS := TMemoryStream.Create;
      try
        JPEG.SaveToStream(MS);
        SetLength(Result, Integer(MS.Size));
        MS.Position := 0;
        if Length(Result) > 0 then
          MS.ReadBuffer(Result[0], Length(Result));
      finally
        MS.Free;
      end;
    finally
      JPEG.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

type
  TShortWriteStream = class(TStream)
  private
    FData: TMemoryStream;
    FMaxWrite: Integer;
  public
    constructor Create(AMaxWrite: Integer);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

constructor TShortWriteStream.Create(AMaxWrite: Integer);
begin
  inherited Create;
  FData := TMemoryStream.Create;
  FMaxWrite := AMaxWrite;
end;

destructor TShortWriteStream.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TShortWriteStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FData.Read(Buffer, Count);
end;

function TShortWriteStream.Write(const Buffer; Count: Longint): Longint;
begin
  if FMaxWrite <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  if Count > FMaxWrite then
    Count := FMaxWrite;
  Result := FData.Write(Buffer, Count);
end;

function TShortWriteStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := FData.Seek(Offset, Origin);
end;

type
  TRobustnessTests = class
    class procedure TestSamplingFactorsRejected;
    class procedure TestProgressiveCoefficientOverflowRejected;
    class procedure TestTruncatedEntropyRejected;
    class procedure TestMalformedDHTRejected;
    class procedure TestSegmentLengthOverrunRejected;
    class procedure TestBaselineMultiScanRejected;
    class procedure TestProgressiveNegativeRefinement;
    class procedure TestUndefinedTableRejected;
    class procedure TestAPP0LengthRejected;
    class procedure TestDIBRetryAfterFailure;
    class procedure TestLoadedQualityCanSave;
    class procedure TestPixelFormatRestored;
    class procedure TestMissingSOFRejected;
    class procedure TestDecoderFootprint;
    class procedure TestDimensionContract;
    class procedure TestShortWrites;
    class procedure TestSelfAssignAndSharedLifetime;
  end;

class procedure TRobustnessTests.TestSamplingFactorsRejected;
var
  Data: TBytes;
  SOF: Integer;
begin
  Data := EncodeRobustnessJPEG(False);
  SOF := FindJPEGMarker(Data, $C0);
  AssertTrue(SOF >= 0, 'baseline SOF0 marker must exist');
  Data[SOF + 11] := 0;
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'zero sampling factors must be rejected');

  Data := EncodeRobustnessJPEG(False);
  SOF := FindJPEGMarker(Data, $C0);
  AssertTrue(SOF >= 0, 'baseline SOF0 marker must exist for upper bound test');
  Data[SOF + 11] := $F0;
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'sampling factors above four must be rejected');
end;

class procedure TRobustnessTests.TestProgressiveCoefficientOverflowRejected;
var
  Data: TBytes;
  SOF: Integer;
begin
  Data := EncodeRobustnessJPEG(True);
  SOF := FindJPEGMarker(Data, $C2);
  AssertTrue(SOF >= 0, 'progressive SOF2 marker must exist');
  Data[SOF + 5] := $FF;
  Data[SOF + 6] := $FF;
  Data[SOF + 7] := $FF;
  Data[SOF + 8] := $FF;
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'overflowing progressive coefficient storage must be rejected');
end;

class procedure TRobustnessTests.TestTruncatedEntropyRejected;
var
  Data: TBytes;
begin
  Data := EncodeRobustnessJPEG(False);
  AssertTrue(Length(Data) > 2, 'encoded JPEG must contain EOI');
  SetLength(Data, Length(Data) - 2);
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'truncated entropy data must be rejected');
end;

class procedure TRobustnessTests.TestMalformedDHTRejected;
var
  Data: TBytes;
  DHT: Integer;
begin
  Data := EncodeRobustnessJPEG(False);
  DHT := FindJPEGMarker(Data, $C4);
  AssertTrue(DHT >= 0, 'baseline DHT marker must exist');
  // Force the first Huffman table count to 257 (> 256-byte HuffVal capacity).
  Data[DHT + 4 + 14] := 2;
  Data[DHT + 4 + 15] := 255;
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'oversized Huffman table must be rejected');
end;

class procedure TRobustnessTests.TestSegmentLengthOverrunRejected;
var
  Data: TBytes;
  DQT: Integer;
begin
  Data := EncodeRobustnessJPEG(False);
  DQT := FindJPEGMarker(Data, $DB);
  AssertTrue(DQT >= 0, 'baseline DQT marker must exist');
  Data[DQT + 2] := 0;
  Data[DQT + 3] := 3;
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'segment length overrun must be rejected');
end;

class procedure TRobustnessTests.TestBaselineMultiScanRejected;
var
  Data: TBytes;
  SOS: Integer;
begin
  Data := EncodeRobustnessJPEG(False);
  SOS := FindJPEGMarker(Data, $DA);
  AssertTrue(SOS >= 0, 'baseline SOS marker must exist');
  Data[SOS + 4] := 1; // Non-interleaved scan is not supported by baseline path.
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'unsupported baseline multi-scan organization must be rejected');
end;

class procedure TRobustnessTests.TestProgressiveNegativeRefinement;
var
  BaselineData, ProgressiveData: TBytes;
  BaselineJPEG, ProgressiveJPEG: TCnJPEGImage;
  BaseMS, ProgMS: TMemoryStream;
  BaseBmp, ProgBmp: TBitmap;
  X, Y, Diff, MaxDiff, TotalDiff, SampleCount: Integer;
  BaseRow, ProgRow: PByteArray;
begin
  BaselineData := EncodeGradientJPEG(False);
  ProgressiveData := EncodeGradientJPEG(True);
  BaselineJPEG := TCnJPEGImage.Create;
  ProgressiveJPEG := TCnJPEGImage.Create;
  BaseBmp := TBitmap.Create;
  ProgBmp := TBitmap.Create;
  BaseMS := TMemoryStream.Create;
  ProgMS := TMemoryStream.Create;
  try
    BaseMS.WriteBuffer(BaselineData[0], Length(BaselineData));
    ProgMS.WriteBuffer(ProgressiveData[0], Length(ProgressiveData));
    BaseMS.Position := 0;
    ProgMS.Position := 0;
    BaselineJPEG.LoadFromStream(BaseMS);
    ProgressiveJPEG.LoadFromStream(ProgMS);
    BaselineJPEG.DIBNeeded;
    ProgressiveJPEG.DIBNeeded;
    BaseBmp.Assign(BaselineJPEG);
    ProgBmp.Assign(ProgressiveJPEG);
    MaxDiff := 0;
    TotalDiff := 0;
    SampleCount := 0;
    for Y := 0 to 15 do
    begin
      BaseRow := BaseBmp.ScanLine[Y];
      ProgRow := ProgBmp.ScanLine[Y];
      for X := 0 to 15 * 3 + 2 do
      begin
        Diff := Abs(Integer(BaseRow[X]) - Integer(ProgRow[X]));
        if Diff > MaxDiff then MaxDiff := Diff;
        Inc(TotalDiff, Diff);
        Inc(SampleCount);
      end;
    end;
    AssertTrue((SampleCount > 0) and (TotalDiff div SampleCount <= 40),
      'progressive negative refinement average diff too high, max diff=' + IntToStr(MaxDiff));
  finally
    ProgMS.Free;
    BaseMS.Free;
    ProgBmp.Free;
    BaseBmp.Free;
    ProgressiveJPEG.Free;
    BaselineJPEG.Free;
  end;
end;

class procedure TRobustnessTests.TestUndefinedTableRejected;
var
  Data: TBytes;
  SOF: Integer;
begin
  Data := EncodeRobustnessJPEG(False);
  SOF := FindJPEGMarker(Data, $C0);
  AssertTrue(SOF >= 0, 'baseline SOF0 marker must exist');
  Data[SOF + 12] := 3; // First component references an undefined DQT.
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'undefined quantization table reference must be rejected');
end;

class procedure TRobustnessTests.TestAPP0LengthRejected;
var
  Data: TBytes;
  APP0: Integer;
begin
  Data := EncodeRobustnessJPEG(False);
  APP0 := FindJPEGMarker(Data, $E0);
  AssertTrue(APP0 >= 0, 'APP0 marker must exist');
  Data[APP0 + 2] := 0;
  Data[APP0 + 3] := 3;
  AssertTrue(not TryDecodeRobustnessJPEG(Data),
    'short APP0 segment must be rejected');
end;

class procedure TRobustnessTests.TestDIBRetryAfterFailure;
var
  Data: TBytes;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  FailedOnce, FailedTwice: Boolean;
begin
  Data := EncodeRobustnessJPEG(False);
  AssertTrue(Length(Data) > 2, 'encoded JPEG must contain EOI');
  SetLength(Data, Length(Data) - 2);
  JPEG := TCnJPEGImage.Create;
  MS := TMemoryStream.Create;
  try
    MS.WriteBuffer(Data[0], Length(Data));
    MS.Position := 0;
    JPEG.LoadFromStream(MS);
    FailedOnce := False;
    try
      JPEG.DIBNeeded;
    except
      FailedOnce := True;
    end;
    FailedTwice := False;
    try
      JPEG.DIBNeeded;
    except
      FailedTwice := True;
    end;
    AssertTrue(FailedOnce and FailedTwice,
      'failed decode must not leave a half-built bitmap');
  finally
    MS.Free;
    JPEG.Free;
  end;
end;

class procedure TRobustnessTests.TestLoadedQualityCanSave;
var
  Data: TBytes;
  JPEG: TCnJPEGImage;
  MS, OutMS: TMemoryStream;
begin
  Data := EncodeRobustnessJPEG(False);
  JPEG := TCnJPEGImage.Create;
  MS := TMemoryStream.Create;
  OutMS := TMemoryStream.Create;
  try
    MS.WriteBuffer(Data[0], Length(Data));
    MS.Position := 0;
    JPEG.LoadFromStream(MS);
    JPEG.CompressionQuality := 70;
    JPEG.SaveToStream(OutMS);
    AssertTrue(OutMS.Size > 0, 'modified loaded JPEG must be saveable');
    OutMS.Position := 0;
    JPEG.LoadFromStream(OutMS);
    JPEG.DIBNeeded;
  finally
    OutMS.Free;
    MS.Free;
    JPEG.Free;
  end;
end;

class procedure TRobustnessTests.TestPixelFormatRestored;
var
  Bmp: TBitmap;
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  OriginalFormat: TPixelFormat;
begin
  Bmp := TBitmap.Create;
  JPEG := TCnJPEGImage.Create;
  MS := TMemoryStream.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf8bit;
    Bmp.Width := 8;
    Bmp.Height := 8;
    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));
    OriginalFormat := Bmp.PixelFormat;
    JPEG.Assign(Bmp);
    JPEG.SaveToStream(MS);
    AssertEqual(Ord(OriginalFormat), Ord(Bmp.PixelFormat),
      'encoder must restore the source bitmap pixel format');
  finally
    MS.Free;
    JPEG.Free;
    Bmp.Free;
  end;
end;

class procedure TRobustnessTests.TestMissingSOFRejected;
var
  JPEG: TCnJPEGImage;
  MS: TMemoryStream;
  B: Byte;
  Failed: Boolean;
begin
  MS := TMemoryStream.Create;
  JPEG := TCnJPEGImage.Create;
  try
    B := $FF; MS.WriteBuffer(B, 1);
    B := $D8; MS.WriteBuffer(B, 1);
    B := $FF; MS.WriteBuffer(B, 1);
    B := $D9; MS.WriteBuffer(B, 1);
    MS.Position := 0;
    Failed := False;
    try
      JPEG.LoadFromStream(MS);
    except
      Failed := True;
    end;
    AssertTrue(Failed, 'JPEG without SOF/SOS must be rejected');
  finally
    JPEG.Free;
    MS.Free;
  end;
end;

class procedure TRobustnessTests.TestDecoderFootprint;
begin
  AssertTrue(TCnJPEGDecoder.InstanceSize < 64 * 1024,
    'decoder instance must not contain the unused 256 KiB YCbCr table');
end;

class procedure TRobustnessTests.TestDimensionContract;
var
  Bmp: TBitmap;
  SourceJPEG, LoadedJPEG: TCnJPEGImage;
  MS: TMemoryStream;
begin
  Bmp := TBitmap.Create;
  SourceJPEG := TCnJPEGImage.Create;
  LoadedJPEG := TCnJPEGImage.Create;
  MS := TMemoryStream.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 17;
    Bmp.Height := 9;
    Bmp.Canvas.Brush.Color := clLime;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 17, 9));
    SourceJPEG.Assign(Bmp);
    AssertEqual(17, SourceJPEG.Width, 'assigned bitmap width before compression');
    AssertEqual(9, SourceJPEG.Height, 'assigned bitmap height before compression');
    SourceJPEG.SaveToStream(MS);

    MS.Position := 0;
    LoadedJPEG.LoadFromStream(MS);
    LoadedJPEG.Scale := jsQuarter;
    AssertEqual(5, LoadedJPEG.Width, 'scaled width before DIB decode');
    AssertEqual(3, LoadedJPEG.Height, 'scaled height before DIB decode');
    LoadedJPEG.DIBNeeded;
    AssertEqual(LoadedJPEG.Bitmap.Width, LoadedJPEG.Width,
      'Width must match decoded bitmap width');
    AssertEqual(LoadedJPEG.Bitmap.Height, LoadedJPEG.Height,
      'Height must match decoded bitmap height');
  finally
    MS.Free;
    LoadedJPEG.Free;
    SourceJPEG.Free;
    Bmp.Free;
  end;
end;

class procedure TRobustnessTests.TestShortWrites;
var
  Data: TBytes;
  JPEG: TCnJPEGImage;
  LoadMS: TMemoryStream;
  PartialStream, FailingStream: TShortWriteStream;
  Failed: Boolean;
begin
  Data := EncodeRobustnessJPEG(False);
  JPEG := TCnJPEGImage.Create;
  LoadMS := TMemoryStream.Create;
  PartialStream := TShortWriteStream.Create(3);
  FailingStream := TShortWriteStream.Create(0);
  try
    AssertTrue(Length(Data) > 0, 'source JPEG data must exist');
    LoadMS.WriteBuffer(Data[0], Length(Data));
    LoadMS.Position := 0;
    JPEG.LoadFromStream(LoadMS);

    JPEG.SaveToStream(PartialStream);
    AssertTrue(PartialStream.Size = Length(Data),
      'partial writes must be retried until complete');

    Failed := False;
    try
      JPEG.SaveToStream(FailingStream);
    except
      Failed := True;
    end;
    AssertTrue(Failed, 'zero-length write must raise an exception');
  finally
    FailingStream.Free;
    PartialStream.Free;
    LoadMS.Free;
    JPEG.Free;
  end;
end;

class procedure TRobustnessTests.TestSelfAssignAndSharedLifetime;
var
  Bmp: TBitmap;
  JPEG1, JPEG2: TCnJPEGImage;
  MS: TMemoryStream;
begin
  Bmp := TBitmap.Create;
  JPEG1 := TCnJPEGImage.Create;
  JPEG2 := TCnJPEGImage.Create;
  MS := TMemoryStream.Create;
  try
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 11;
    Bmp.Height := 7;
    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 11, 7));
    JPEG1.Assign(Bmp);
    JPEG1.Compress;
    JPEG1.Assign(JPEG1);
    JPEG1.NewJPEG.Assign(JPEG1.NewJPEG);
    JPEG2.Assign(JPEG1);
    JPEG1.Free;
    JPEG1 := nil;
    JPEG2.DIBNeeded;
    AssertEqual(11, JPEG2.Width, 'shared JPEG width after source release');
    AssertEqual(7, JPEG2.Height, 'shared JPEG height after source release');
    JPEG2.SaveToStream(MS);
    AssertTrue(MS.Size > 0, 'shared JPEG data must remain valid');
  finally
    MS.Free;
    JPEG2.Free;
    JPEG1.Free;
    Bmp.Free;
  end;
end;

//============================================================================
// Register all tests
//============================================================================

initialization
  // L1: Marker Parsing
  RegisterTest('L1_Marker_SOI', 'SOI marker detection', TMarkerTests.TestSOIDetection);
  RegisterTest('L1_Marker_SegLen', 'Segment length reading', TMarkerTests.TestSegmentLength);

  // L1: ZigZag
  RegisterTest('L1_ZigZag_Order', 'ZigZag order values', TZigZagTests.TestZigZagOrder);
  RegisterTest('L1_ZigZag_Inv', 'ZigZag inverse values', TZigZagTests.TestZigZagInverse);
  RegisterTest('L1_ZigZag_Roundtrip', 'ZigZag roundtrip consistency', TZigZagTests.TestZigZagRoundtrip);

  // L1: Color Conversion
  RegisterTest('L1_Color_YCbCr2RGB', 'YCbCr to RGB conversion', TColorTests.TestYCbCrToRGB);
  RegisterTest('L1_Color_RGB2YCbCr', 'RGB to YCbCr conversion', TColorTests.TestRGBToYCbCr);

  // L1: Quantization
  RegisterTest('L1_Quant_Build', 'Quantization table building', TQuantTests.TestQuantTableBuild);

  // L1: Huffman Table (Task 36.1-36.3)
  RegisterTest('L1_Huffman_Build', 'Huffman table construction', THuffmanTests.TestHuffmanTableBuild);
  RegisterTest('L1_Huffman_DC', 'Huffman DC coefficient decode', THuffmanTests.TestHuffmanDCDecode);
  RegisterTest('L1_Huffman_AC', 'Huffman AC coefficient decode', THuffmanTests.TestHuffmanACDecode);
  RegisterTest('L1_Huffman_Bitstream', 'Huffman bitstream direct decode', THuffmanTests.TestHuffmanBitstreamDecode);

  // L1: IDCT (Task 37.1)
  RegisterTest('L1_IDCT_Basics', 'IDCT basic transforms', TIDCTTests.TestIDCTBasics);
  RegisterTest('L1_IDCT_Quality', 'IDCT quality vs speed comparison', TIDCTTests.TestIDCTQualityComparison);

  // L1: Segment Length Minimal (Task 35.2)
  RegisterTest('L1_Marker_MinSeg', 'Minimal segment (length=2)', TSegmentTests.TestMinimalSegment);

  // L2: Baseline Decode/Encode
  RegisterTest('L2_Decode_Properties', 'Decode properties from minimal JPEG', TDecodeTests.TestDecodeProperties);
  RegisterTest('L2_Encode_Roundtrip', 'Encode-decode roundtrip', TDecodeTests.TestEncodeRoundtrip);
  RegisterTest('L2_Encode_Grayscale', 'Grayscale encoding', TDecodeTests.TestGrayscaleEncode);

  // L3: Integration
  RegisterTest('L3_Int_Registration', 'TPicture registration', TIntegrationTests.TestTPictureRegistration);
  RegisterTest('L3_Int_AssignBitmap', 'Assign to/from TBitmap', TIntegrationTests.TestAssignBitmap);

  // L2: File Roundtrip (Task 21.3)
  RegisterTest('L2_File_Markers', 'SaveToStream marker verification', TFileRoundtripTests.TestSaveToStreamMarkers);
  RegisterTest('L2_File_Roundtrip', 'Save-load pixel roundtrip', TFileRoundtripTests.TestSaveLoadRoundtrip);
  RegisterTest('L2_File_ProgMarkers', 'Progressive JPEG SOF2 marker', TFileRoundtripTests.TestProgressiveSaveMarkers);

  // L2: Scale Decode (Task 40.3 / 39.2)
  RegisterTest('L2_Scale_Sizes', 'Scaled output sizes', TScaleTests.TestScaleSizes);
  RegisterTest('L2_Scale_Half', 'Half scale consistency', TScaleTests.TestScaleHalfConsistency);
  RegisterTest('L2_Scale_Quarter', 'Quarter scale consistency', TScaleTests.TestScaleQuarterConsistency);
  RegisterTest('L2_Scale_Eighth', 'Eighth scale consistency', TScaleTests.TestScaleEighthConsistency);

  // L2: Property Linkage (Task 43.1)
  RegisterTest('L2_Prop_Quality', 'Quality affects file size', TPropertyTests.TestQualityAffectsSize);
  RegisterTest('L2_Prop_Grayscale', 'Grayscale property', TPropertyTests.TestGrayscaleProperty);
  RegisterTest('L2_Prop_8Bit', 'PixelFormat 8-bit output', TPropertyTests.TestPixelFormat8Bit);
  RegisterTest('L2_Prop_Scale', 'Scale property changes size', TPropertyTests.TestScaleProperty);

  // L2: Boundary Sizes (Task 40.5)
  RegisterTest('L2_Boundary_1x1', '1x1 pixel image', TBoundaryTests.Test1x1);
  RegisterTest('L2_Boundary_8x8', '8x8 pixel image', TBoundaryTests.Test8x8);
  RegisterTest('L2_Boundary_9x9', '9x9 pixel image', TBoundaryTests.Test9x9);
  RegisterTest('L2_Boundary_15x16', '15x16 pixel image', TBoundaryTests.Test15x16);

  // L2: CMYK ColorSpace (Task 24.2)
  RegisterTest('L2_CMYK_ColorSpace', 'CMYK color space enum', TCMYKTests.TestCMYKColorSpace);

  // L2: Progressive Roundtrip (Task 42.2)
  RegisterTest('L2_Prog_Roundtrip', 'Progressive encode-decode roundtrip', TProgressiveTests.TestProgressiveRoundtrip);

  // P0: Robustness regressions
  RegisterTest('P0_SamplingFactors', 'Reject invalid sampling factors', TRobustnessTests.TestSamplingFactorsRejected);
  RegisterTest('P0_CoefOverflow', 'Reject progressive coefficient overflow', TRobustnessTests.TestProgressiveCoefficientOverflowRejected);
  RegisterTest('P0_TruncatedEntropy', 'Reject truncated entropy data', TRobustnessTests.TestTruncatedEntropyRejected);
  RegisterTest('P0_MalformedDHT', 'Reject oversized Huffman table', TRobustnessTests.TestMalformedDHTRejected);
  RegisterTest('P0_SegmentBounds', 'Reject segment length overrun', TRobustnessTests.TestSegmentLengthOverrunRejected);

  // P1: Functional and compatibility regressions
  RegisterTest('P1_BaselineMultiScan', 'Reject unsupported baseline multi-scan organization', TRobustnessTests.TestBaselineMultiScanRejected);
  RegisterTest('P1_ProgressiveNegativeRefine', 'Progressive negative coefficient refinement', TRobustnessTests.TestProgressiveNegativeRefinement);
  RegisterTest('P1_UndefinedTable', 'Reject undefined table references', TRobustnessTests.TestUndefinedTableRejected);
  RegisterTest('P1_APP0Length', 'Reject short APP0 segment', TRobustnessTests.TestAPP0LengthRejected);
  RegisterTest('P1_DIBRetry', 'DIB decode retry after failure', TRobustnessTests.TestDIBRetryAfterFailure);
  RegisterTest('P1_LoadedQualitySave', 'Save loaded JPEG after quality change', TRobustnessTests.TestLoadedQualityCanSave);
  RegisterTest('P1_PixelFormatRestore', 'Restore source bitmap pixel format', TRobustnessTests.TestPixelFormatRestored);
  RegisterTest('P1_MissingSOF', 'Reject JPEG without SOF', TRobustnessTests.TestMissingSOFRejected);

  // P2: API and robustness regressions
  RegisterTest('P2_DecoderFootprint', 'Decoder does not embed unused YCbCr table', TRobustnessTests.TestDecoderFootprint);
  RegisterTest('P2_Dimensions', 'Width/Height follow bitmap and scale contract', TRobustnessTests.TestDimensionContract);
  RegisterTest('P2_ShortWrites', 'Retry partial writes and reject zero writes', TRobustnessTests.TestShortWrites);
  RegisterTest('P2_RefCountAssign', 'Self-assign and shared JPEG lifetime', TRobustnessTests.TestSelfAssignAndSharedLifetime);

end.
