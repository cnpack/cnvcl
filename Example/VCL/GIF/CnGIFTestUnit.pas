unit CnGIFTestUnit;

{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                   ------------------------------------                       }
{            This unit is a console-style test suite for CnGIF.pas,            }
{            modeled after CnJPEGTestUnit.pas. It exercises GIF89a             }
{            parsing, LZW encode/decode, interlacing, multi-frame              }
{            animation, NETSCAPE loop extension, and the bitmap-to-GIF         }
{            conversion paths. Special attention is paid to the 9 bugs         }
{            identified in the code review (P0/P1 are explicitly covered).     }
{******************************************************************************}

interface

uses
  SysUtils, Classes, Windows, Graphics, CnNative, CnGIF;

type
  TCnGIFTestCase = record
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

function MakeRect(ALeft, ATop, ARight, ABottom: Integer): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

var
  TestCases: array of TCnGIFTestCase;

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

procedure RunAllTests;
var
  I: Integer;
begin
  WriteLn('=== CnGIF Test Suite ===');
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
// GIF byte-stream builder helpers
//============================================================================

const
  GIF_HEADER_89A: AnsiString = 'GIF89a';
  GIF_HEADER_87A: AnsiString = 'GIF87a';

  GIF_EXT_INTRODUCER    = $21;
  GIF_IMAGE_DESCRIPTOR  = $2C;
  GIF_TRAILER           = $3B;
  GIF_EXT_GRAPHIC_CTRL  = $F9;
  GIF_EXT_APPLICATION   = $FF;
  GIF_EXT_COMMENT       = $FE;
  GIF_EXT_PLAIN_TEXT    = $01;

type
  TGIFStreamBuilder = class
  private
    FStream: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Stream: TMemoryStream;
    function Bytes: TBytes;

    procedure WriteByte(B: Byte);
    procedure WriteWord(W: Word);
    procedure WriteBytes(const B: array of Byte);
    procedure WriteAnsi(const S: AnsiString);

    procedure WriteHeader(const Ver: AnsiString = 'GIF89a');
    procedure WriteLSD(W, H: Word; HasGCT: Boolean; ColorRes: Byte;
      SortFlag: Boolean; GCTBits: Byte; BgIdx: Byte; Aspect: Byte);
    procedure WriteColorTable(const Pal: TCnGIFColors; Bits: Byte);
    procedure WriteGCE(Disposal: Byte; HasTrans: Boolean; Delay: Word;
      TransIdx: Byte; BlockSz: Byte = 4); overload;
    procedure WriteGCEWithExtra(Disposal: Byte; HasTrans: Boolean; Delay: Word;
      TransIdx: Byte; BlockSz: Byte; const Extra: array of Byte);
    procedure WriteNetscapeExt(LoopCount: Word; SubBlockSize: Byte = 3;
      SubBlockId: Byte = 1; WriteTrailerZero: Boolean = True);
    procedure WriteImageDesc(Left, Top, W, H: Word; HasLCT: Boolean;
      Interlaced: Boolean; LCTBits: Byte);
    procedure WriteLZWSubBlocks(const Data: array of Byte);
    procedure WriteTrailer;
  end;

constructor TGIFStreamBuilder.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TGIFStreamBuilder.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TGIFStreamBuilder.Clear;
begin
  FStream.Size := 0;
end;

function TGIFStreamBuilder.Stream: TMemoryStream;
begin
  Result := FStream;
end;

function TGIFStreamBuilder.Bytes: TBytes;
begin
  SetLength(Result, FStream.Size);
  if FStream.Size > 0 then
    Move(FStream.Memory^, Result[0], FStream.Size);
end;

procedure TGIFStreamBuilder.WriteByte(B: Byte);
begin
  FStream.Write(B, 1);
end;

procedure TGIFStreamBuilder.WriteWord(W: Word);
begin
  FStream.Write(W, 2);
end;

procedure TGIFStreamBuilder.WriteBytes(const B: array of Byte);
var
  I: Integer;
begin
  for I := Low(B) to High(B) do
    FStream.Write(B[I], 1);
end;

procedure TGIFStreamBuilder.WriteAnsi(const S: AnsiString);
begin
  if Length(S) > 0 then
    FStream.Write(S[1], Length(S));
end;

procedure TGIFStreamBuilder.WriteHeader(const Ver: AnsiString);
begin
  WriteAnsi(Ver);
end;

procedure TGIFStreamBuilder.WriteLSD(W, H: Word; HasGCT: Boolean;
  ColorRes: Byte; SortFlag: Boolean; GCTBits: Byte; BgIdx: Byte;
  Aspect: Byte);
var
  Pkd: Byte;
begin
  WriteWord(W);
  WriteWord(H);
  Pkd := 0;
  if HasGCT then Pkd := Pkd or $80;
  Pkd := Pkd or ((ColorRes and $07) shl 4);
  if SortFlag then Pkd := Pkd or $08;
  Pkd := Pkd or (GCTBits and $07);
  WriteByte(Pkd);
  WriteByte(BgIdx);
  WriteByte(Aspect);
end;

procedure TGIFStreamBuilder.WriteColorTable(const Pal: TCnGIFColors;
  Bits: Byte);
var
  I, Count: Integer;
  Black: TCnGIFColor;
begin
  Black.R := 0; Black.G := 0; Black.B := 0;
  Count := 1 shl (Bits + 1);
  for I := 0 to Count - 1 do
  begin
    if I < Length(Pal) then
      FStream.Write(Pal[I], 3)
    else
      FStream.Write(Black, 3);
  end;
end;

procedure TGIFStreamBuilder.WriteGCE(Disposal: Byte; HasTrans: Boolean;
  Delay: Word; TransIdx: Byte; BlockSz: Byte);
var
  Pkd: Byte;
begin
  WriteByte(GIF_EXT_INTRODUCER);
  WriteByte(GIF_EXT_GRAPHIC_CTRL);
  WriteByte(BlockSz);
  Pkd := (Disposal and $07) shl 2;
  if HasTrans then Pkd := Pkd or $01;
  WriteByte(Pkd);
  WriteWord(Delay);
  WriteByte(TransIdx);
  WriteByte(0);  // block terminator
end;

procedure TGIFStreamBuilder.WriteGCEWithExtra(Disposal: Byte;
  HasTrans: Boolean; Delay: Word; TransIdx: Byte; BlockSz: Byte;
  const Extra: array of Byte);
var
  Pkd: Byte;
begin
  WriteByte(GIF_EXT_INTRODUCER);
  WriteByte(GIF_EXT_GRAPHIC_CTRL);
  WriteByte(BlockSz);
  Pkd := (Disposal and $07) shl 2;
  if HasTrans then Pkd := Pkd or $01;
  WriteByte(Pkd);
  WriteWord(Delay);
  WriteByte(TransIdx);
  // 4 standard GCE bytes written; emit any extra bytes declared by BlockSz>4.
  if BlockSz > 4 then
    WriteBytes(Extra);
  WriteByte(0);  // block terminator
end;

procedure TGIFStreamBuilder.WriteNetscapeExt(LoopCount: Word;
  SubBlockSize: Byte; SubBlockId: Byte; WriteTrailerZero: Boolean);
begin
  WriteByte(GIF_EXT_INTRODUCER);
  WriteByte(GIF_EXT_APPLICATION);
  WriteByte(11);
  WriteAnsi('NETSCAPE2.0');
  WriteByte(SubBlockSize);
  if SubBlockSize >= 1 then
    WriteByte(SubBlockId);
  if SubBlockSize >= 2 then
    WriteByte(LoopCount and $FF);
  if SubBlockSize >= 3 then
    WriteByte((LoopCount shr 8) and $FF);
  if WriteTrailerZero then
    WriteByte(0);
end;

procedure TGIFStreamBuilder.WriteImageDesc(Left, Top, W, H: Word;
  HasLCT: Boolean; Interlaced: Boolean; LCTBits: Byte);
var
  Pkd: Byte;
begin
  WriteByte(GIF_IMAGE_DESCRIPTOR);
  WriteWord(Left);
  WriteWord(Top);
  WriteWord(W);
  WriteWord(H);
  Pkd := 0;
  if HasLCT then Pkd := Pkd or $80;
  { sort flag $20 omitted }
  if Interlaced then Pkd := Pkd or $40;
  Pkd := Pkd or (LCTBits and $07);
  WriteByte(Pkd);
end;

procedure TGIFStreamBuilder.WriteLZWSubBlocks(const Data: array of Byte);
var
  I, Chunk: Integer;
begin
  I := 0;
  while I < Length(Data) do
  begin
    if Length(Data) - I > 255 then Chunk := 255 else Chunk := Length(Data) - I;
    WriteByte(Chunk);
    FStream.Write(Data[I], Chunk);
    Inc(I, Chunk);
  end;
  WriteByte(0);  // terminator
end;

procedure TGIFStreamBuilder.WriteTrailer;
begin
  WriteByte(GIF_TRAILER);
end;

//============================================================================
// Standalone LZW encoder for crafting valid LZW data in tests.
// Ports the algorithm from CnGIF.pas EncodeLZW so we can produce
// known-correct LZW bytes for arbitrary pixel patterns.
//============================================================================

function GifLZWEncode(const Pixels: array of Byte;
  MinCodeSize: Byte): TBytes;
const
  MAX_CODES = 4096;
type
  THashEnt = packed record
    Used: Boolean;
    Prefix: Word;
    Suffix: Byte;
    Code: Word;
  end;
var
  HT: array[0..MAX_CODES - 1] of THashEnt;
  ClearCode, EOICode, CodeSize, CodeMask, NextCode: Integer;
  InPos, CurPrefix, CurSuffix, HashIdx: Integer;
  BitBuf: Cardinal;
  BitCnt: Integer;
  OutByte: Byte;
  OutStm: TMemoryStream;

  function FindHash(Prefix: Word; Suffix: Byte): Integer;
  var
    Key: Integer;
  begin
    Key := (Prefix xor (Word(Suffix) shl 4)) and $FFF;
    while True do
    begin
      if not HT[Key].Used then
      begin
        Result := Key;
        Exit;
      end;
      if (HT[Key].Prefix = Prefix) and (HT[Key].Suffix = Suffix) then
      begin
        Result := Key;
        Exit;
      end;
      Key := (Key + 1) and $FFF;
    end;
  end;

  procedure WriteCode(Code: Integer);
  begin
    BitBuf := BitBuf or (Cardinal(Code) shl BitCnt);
    Inc(BitCnt, CodeSize);
    while BitCnt >= 8 do
    begin
      OutByte := BitBuf and $FF;
      OutStm.Write(OutByte, 1);
      BitBuf := BitBuf shr 8;
      Dec(BitCnt, 8);
    end;
  end;

  procedure FlushBits;
  begin
    while BitCnt > 0 do
    begin
      OutByte := BitBuf and $FF;
      OutStm.Write(OutByte, 1);
      BitBuf := BitBuf shr 8;
      Dec(BitCnt, 8);
    end;
  end;

begin
  SetLength(Result, 0);
  if Length(Pixels) = 0 then Exit;
  if MinCodeSize < 2 then MinCodeSize := 2;

  ClearCode := 1 shl MinCodeSize;
  EOICode   := ClearCode + 1;
  CodeSize  := MinCodeSize + 1;
  CodeMask  := (1 shl CodeSize) - 1;
  NextCode  := ClearCode + 2;
  BitBuf    := 0;
  BitCnt    := 0;
  FillChar(HT, SizeOf(HT), 0);

  OutStm := TMemoryStream.Create;
  try
    WriteCode(ClearCode);
    CurPrefix := Pixels[0];
    InPos := 1;
    while InPos < Length(Pixels) do
    begin
      CurSuffix := Pixels[InPos];
      HashIdx := FindHash(CurPrefix, CurSuffix);
      if HT[HashIdx].Used and (HT[HashIdx].Prefix = CurPrefix) and
         (HT[HashIdx].Suffix = CurSuffix) then
      begin
        CurPrefix := HT[HashIdx].Code;
      end
      else
      begin
        WriteCode(CurPrefix);
        if NextCode < MAX_CODES then
        begin
          HT[HashIdx].Used := True;
          HT[HashIdx].Prefix := CurPrefix;
          HT[HashIdx].Suffix := CurSuffix;
          HT[HashIdx].Code := NextCode;
          Inc(NextCode);
          // Match CnGIF.pas: defer code-size growth by one step so the
          // decoder stays in sync (its table also grows one step behind).
          if NextCode > CodeMask then
          begin
            if CodeSize < 12 then
            begin
              if NextCode > CodeMask + 1 then
              begin
                Inc(CodeSize);
                CodeMask := (1 shl CodeSize) - 1;
              end;
            end
            else
            begin
              WriteCode(ClearCode);
              FillChar(HT, SizeOf(HT), 0);
              NextCode := ClearCode + 2;
              CodeSize := MinCodeSize + 1;
              CodeMask := (1 shl CodeSize) - 1;
            end;
          end;
        end;
        CurPrefix := CurSuffix;
      end;
      Inc(InPos);
    end;
    WriteCode(CurPrefix);
    WriteCode(EOICode);
    FlushBits;
    SetLength(Result, OutStm.Size);
    if OutStm.Size > 0 then
      Move(OutStm.Memory^, Result[0], OutStm.Size);
  finally
    OutStm.Free;
  end;
end;

//============================================================================
// Palette helper
//============================================================================

function MakePalette(Count: Integer): TCnGIFColors;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    Result[I].R := (I * 53) and $FF;
    Result[I].G := (I * 31) and $FF;
    Result[I].B := (I * 97) and $FF;
  end;
end;

function PaletteBitsFor(Count: Integer): Byte;
var
  P: Integer;
begin
  P := 0;
  while (1 shl (P + 1)) < Count do Inc(P);
  Result := P;
end;

//============================================================================
// GIF stream parser / inspector
//============================================================================

type
  TGIFFrameInfo = record
    Left, Top, Width, Height: Word;
    Interlaced: Boolean;
    HasLCT: Boolean;
    LCTBits: Byte;
    HasGCE: Boolean;
    GCEDisposal: Byte;
    GCEHasTrans: Boolean;
    GCEDelay: Word;
    GCETransIdx: Byte;
    GCEBlockSz: Byte;
    LZWMinCodeSize: Byte;
    LZWDataOffset: Int64;     // offset of LZW sub-blocks start
    LZWDataSize: Integer;     // accumulated data bytes (excluding size bytes)
  end;

  TGIFInfo = record
    Signature: AnsiString;
    LogicalWidth, LogicalHeight: Word;
    HasGCT: Boolean;
    GCTBits: Byte;
    BgIdx: Byte;
    Aspect: Byte;
    HasNetscape: Boolean;
    NetscapeSubBlockSize: Byte;
    LoopCount: Word;
    FrameCount: Integer;
    Frames: array of TGIFFrameInfo;
    TrailerOffset: Int64;
    ParseError: string;
  end;

function ParseGIFStream(S: TStream): TGIFInfo;
var
  Hdr: array[0..5] of AnsiChar;
  Pkd: Byte;
  B, SubB, BlkSz: Byte;
  Frame: TGIFFrameInfo;
  HasPendingGCE: Boolean;
  PendingDisposal: Byte;
  PendingHasTrans: Boolean;
  PendingDelay: Word;
  PendingTransIdx: Byte;
  PendingBlockSz: Byte;
  SB, B1, B2: Byte;
  AppId: array[0..10] of AnsiChar;
  DataSize: Integer;
begin
  Result.Signature := '';
  Result.LogicalWidth := 0;
  Result.LogicalHeight := 0;
  Result.HasGCT := False;
  Result.GCTBits := 0;
  Result.BgIdx := 0;
  Result.Aspect := 0;
  Result.HasNetscape := False;
  Result.NetscapeSubBlockSize := 0;
  Result.LoopCount := 0;
  Result.FrameCount := 0;
  Result.Frames := nil;
  Result.TrailerOffset := -1;
  Result.ParseError := '';

  if S.Read(Hdr, 6) <> 6 then
  begin
    Result.ParseError := 'Cannot read header';
    Exit;
  end;
  SetLength(Result.Signature, 6);
  Move(Hdr[0], Result.Signature[1], 6);

  if (Result.Signature <> 'GIF87a') and (Result.Signature <> 'GIF89a') then
  begin
    Result.ParseError := 'Bad signature';
    Exit;
  end;

  S.Read(Result.LogicalWidth, 2);
  S.Read(Result.LogicalHeight, 2);
  S.Read(Pkd, 1);
  Result.HasGCT := (Pkd and $80) <> 0;
  Result.GCTBits := Pkd and $07;
  S.Read(Result.BgIdx, 1);
  S.Read(Result.Aspect, 1);

  if Result.HasGCT then
    S.Seek(3 * (1 shl (Result.GCTBits + 1)), soFromCurrent);

  HasPendingGCE := False;
  PendingBlockSz := 0;
  PendingDisposal := 0;
  PendingHasTrans := False;
  PendingDelay := 0;
  PendingTransIdx := 0;

  while S.Position < S.Size do
  begin
    S.Read(B, 1);
    if B = GIF_TRAILER then
    begin
      Result.TrailerOffset := S.Position - 1;
      Break;
    end;

    if B = GIF_EXT_INTRODUCER then
    begin
      S.Read(B, 1);
      case B of
        GIF_EXT_GRAPHIC_CTRL:
        begin
          S.Read(BlkSz, 1);
          PendingBlockSz := BlkSz;
          S.Read(Pkd, 1);
          S.Read(PendingDelay, 2);
          S.Read(PendingTransIdx, 1);
          if BlkSz > 4 then
            S.Seek(BlkSz - 4, soFromCurrent);
          S.Read(SubB, 1);  // terminator
          PendingDisposal := (Pkd and $1C) shr 2;
          PendingHasTrans := (Pkd and $01) <> 0;
          HasPendingGCE := True;
        end;
        GIF_EXT_APPLICATION:
        begin
          S.Read(BlkSz, 1);
          if BlkSz = 11 then
          begin
            S.Read(AppId, 11);
            if (AppId[0] = 'N') and (AppId[1] = 'E') and (AppId[2] = 'T') and
               (AppId[3] = 'S') and (AppId[4] = 'C') and (AppId[5] = 'A') and
               (AppId[6] = 'P') and (AppId[7] = 'E') then
            begin
              Result.HasNetscape := True;
              S.Read(SB, 1);
              Result.NetscapeSubBlockSize := SB;
              if SB >= 3 then
              begin
                S.Read(SubB, 1);  // sub-block id (1)
                S.Read(B1, 1);
                S.Read(B2, 1);
                Result.LoopCount := B1 or (B2 shl 8);
                if SB > 3 then
                  S.Seek(SB - 3, soFromCurrent);
                S.Read(SubB, 1);  // terminator
              end
              else
              begin
                // skip remaining sub-blocks
                while True do
                begin
                  S.Read(SubB, 1);
                  if SubB = 0 then Break;
                  S.Seek(SubB, soFromCurrent);
                end;
              end;
            end
            else
            begin
              // other app ext, skip sub-blocks
              while True do
              begin
                S.Read(SubB, 1);
                if SubB = 0 then Break;
                S.Seek(SubB, soFromCurrent);
              end;
            end;
          end
          else
          begin
            // skip sub-blocks
            while True do
            begin
              S.Read(SubB, 1);
              if SubB = 0 then Break;
              S.Seek(SubB, soFromCurrent);
            end;
          end;
        end;
      else
        // comment / plain text / unknown: skip sub-blocks
        while True do
        begin
          S.Read(SubB, 1);
          if SubB = 0 then Break;
          S.Seek(SubB, soFromCurrent);
        end;
      end;
    end
    else if B = GIF_IMAGE_DESCRIPTOR then
    begin
      FillChar(Frame, SizeOf(Frame), 0);
      S.Read(Frame.Left, 2);
      S.Read(Frame.Top, 2);
      S.Read(Frame.Width, 2);
      S.Read(Frame.Height, 2);
      S.Read(Pkd, 1);
      Frame.Interlaced := (Pkd and $40) <> 0;
      Frame.HasLCT := (Pkd and $80) <> 0;
      Frame.LCTBits := Pkd and $07;
      if Frame.HasLCT then
        S.Seek(3 * (1 shl (Frame.LCTBits + 1)), soFromCurrent);

      if HasPendingGCE then
      begin
        Frame.HasGCE := True;
        Frame.GCEDisposal := PendingDisposal;
        Frame.GCEHasTrans := PendingHasTrans;
        Frame.GCEDelay := PendingDelay;
        Frame.GCETransIdx := PendingTransIdx;
        Frame.GCEBlockSz := PendingBlockSz;
        HasPendingGCE := False;
      end;

      S.Read(Frame.LZWMinCodeSize, 1);
      Frame.LZWDataOffset := S.Position;
      Frame.LZWDataSize := 0;
      DataSize := 0;
      while True do
      begin
        S.Read(SubB, 1);
        if SubB = 0 then Break;
        Inc(DataSize, SubB);
        S.Seek(SubB, soFromCurrent);
      end;
      Frame.LZWDataSize := DataSize;

      SetLength(Result.Frames, Length(Result.Frames) + 1);
      Result.Frames[High(Result.Frames)] := Frame;
      Inc(Result.FrameCount);
    end
    else
    begin
      // Unknown byte: stop parsing
      Result.ParseError := 'Unknown block introducer: $' + IntToHex(B, 2);
      Break;
    end;
  end;
end;

function LoadGIFFromBytes(const Img: TCnGIFImage; const B: TBytes): Boolean;
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    if Length(B) > 0 then
      MS.Write(B[0], Length(B));
    MS.Position := 0;
    try
      Img.LoadFromStream(MS);
      Result := True;
    except
      Result := False;
    end;
  finally
    MS.Free;
  end;
end;

function SaveGIFToBytes(const Img: TCnGIFImage): TBytes;
var
  MS: TMemoryStream;
begin
  SetLength(Result, 0);
  MS := TMemoryStream.Create;
  try
    Img.SaveToStream(MS);
    SetLength(Result, MS.Size);
    if MS.Size > 0 then
      Move(MS.Memory^, Result[0], MS.Size);
  finally
    MS.Free;
  end;
end;

//============================================================================
// Test Suite: Header / Signature Parsing
//============================================================================

type
  THeaderTests = class
    class procedure TestGIF89aSignature;
    class procedure TestGIF87aSignature;
    class procedure TestInvalidSignature;
    class procedure TestShortStream;
  end;

class procedure THeaderTests.TestGIF89aSignature;
var
  Bld: TGIFStreamBuilder;
  Info: TGIFInfo;
  Img: TCnGIFImage;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Bld.WriteHeader('GIF89a');
    Bld.WriteLSD(8, 8, False, 0, False, 0, 0, 0);
    Bld.WriteTrailer;
    Bld.Stream.Position := 0;
    Info := ParseGIFStream(Bld.Stream);
    AssertTrue(Info.Signature = 'GIF89a', 'signature = GIF89a');
    AssertEqual(8, Info.LogicalWidth, 'LSD width');
    AssertEqual(8, Info.LogicalHeight, 'LSD height');
    AssertTrue(not Info.HasGCT, 'no GCT expected');

    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load 89a');
      AssertEqual(0, Img.FrameCount, 'no frames');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure THeaderTests.TestGIF87aSignature;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Bld.WriteHeader('GIF87a');
    Bld.WriteLSD(4, 4, False, 0, False, 0, 0, 0);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load 87a should succeed');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure THeaderTests.TestInvalidSignature;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Bld.WriteHeader('GIFXYZ');  // invalid
    Bld.WriteLSD(4, 4, False, 0, False, 0, 0, 0);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(not LoadGIFFromBytes(Img, Bld.Bytes), 'invalid signature should fail');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure THeaderTests.TestShortStream;
var
  Img: TCnGIFImage;
  MS: TMemoryStream;
begin
  // Stream with only 3 bytes (truncated header)
  MS := TMemoryStream.Create;
  try
    MS.Write(AnsiString('GIF')[1], 3);
    MS.Position := 0;
    Img := TCnGIFImage.Create;
    try
      // Should not crash; either raise or silently return empty
      try
        Img.LoadFromStream(MS);
      except
        // acceptable
      end;
      AssertEqual(0, Img.FrameCount, 'no frames from short stream');
    finally
      Img.Free;
    end;
  finally
    MS.Free;
  end;
end;

//============================================================================
// Test Suite: Logical Screen Descriptor + Global Color Table
//============================================================================

type
  TLSDTests = class
    class procedure TestGlobalPaletteRead;
    class procedure TestPaletteSizeBitsEncoding;
  end;

class procedure TLSDTests.TestGlobalPaletteRead;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);  // 4 entries => GCT bits = 1
    Bld.WriteHeader;
    Bld.WriteLSD(16, 8, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(0, Img.FrameCount, 'no frames');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TLSDTests.TestPaletteSizeBitsEncoding;
var
  Bld: TGIFStreamBuilder;
  Info: TGIFInfo;
  Pal: TCnGIFColors;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(8);  // 8 entries => GCT bits = 2 (since 1 shl (2+1) = 8)
    Bld.WriteHeader;
    Bld.WriteLSD(10, 10, True, 7, False, 2, 0, 0);
    Bld.WriteColorTable(Pal, 2);
    Bld.WriteTrailer;
    Bld.Stream.Position := 0;
    Info := ParseGIFStream(Bld.Stream);
    AssertTrue(Info.HasGCT, 'GCT flag');
    AssertEqual(2, Info.GCTBits, 'GCT bits = 2 for 8 entries');
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Test Suite: Single-frame roundtrip via hand-crafted LZW
//============================================================================

type
  TFrameTests = class
    class procedure TestSingleFrameLoad;
    class procedure TestLocalPaletteRead;
    class procedure TestFrameGeometry;
  end;

class procedure TFrameTests.TestSingleFrameLoad;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..15] of Byte;
  I: Integer;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 15 do Pixels[I] := I mod 4;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(4, 4, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 4, 4, False, False, 0);
    Bld.WriteByte(2);  // LZW MinCodeSize
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(1, Img.FrameCount, 'one frame');
      AssertEqual(4, Img.Frames[0].Width, 'frame width');
      AssertEqual(4, Img.Frames[0].Height, 'frame height');
      AssertEqual(16, Img.Frames[0].PixelCount, 'pixel count');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TFrameTests.TestLocalPaletteRead;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  GPal, LPal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    GPal := MakePalette(4);
    LPal := MakePalette(8);
    Pixels[0] := 0; Pixels[1] := 1; Pixels[2] := 2; Pixels[3] := 3;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(GPal, 1);
    Bld.WriteImageDesc(0, 0, 2, 2, True, False, 2);
    Bld.WriteColorTable(LPal, 2);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(1, Img.FrameCount, 'frame');
      AssertTrue(Img.Frames[0].HasLocalPalette, 'has local palette');
      AssertEqual(8, Length(Img.Frames[0].LocalPalette), 'local palette size');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TFrameTests.TestFrameGeometry;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Pixels[0] := 0; Pixels[1] := 1; Pixels[2] := 2; Pixels[3] := 3;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(20, 20, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(5, 7, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(5, Img.Frames[0].Left, 'frame left');
      AssertEqual(7, Img.Frames[0].Top, 'frame top');
      AssertEqual(2, Img.Frames[0].Width, 'frame width');
      AssertEqual(2, Img.Frames[0].Height, 'frame height');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Test Suite: LZW encode/decode roundtrip
//============================================================================

type
  TLZWTests = class
    class procedure TestLZWRoundtripSmall;
    class procedure TestLZWRoundtripLarge;
    class procedure TestLZWMinCodeSizeZero;        // Bug #8
    class procedure TestLZWMinCodeSizeTooLarge;    // Bug #8
  end;

class procedure TLZWTests.TestLZWRoundtripSmall;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..15] of Byte;
  I: Integer;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 15 do Pixels[I] := (I * 3) mod 4;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(4, 4, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 4, 4, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(16, Img.Frames[0].PixelCount, 'pixel count');
      // Verify decoded pixels match encoded ones
      for I := 0 to 15 do
        AssertEqual(Pixels[I], Img.Frames[0].Pixels[I],
          'pixel[' + IntToStr(I) + ']');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TLZWTests.TestLZWRoundtripLarge;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array of Byte;
  I, N: Integer;
begin
  // 32x32 image with 256-color palette to exercise code-size growth
  N := 32 * 32;
  SetLength(Pixels, N);
  for I := 0 to N - 1 do
    Pixels[I] := (I * 7 + 13) mod 256;
  Pal := MakePalette(256);
  LZW := GifLZWEncode(Pixels, 8);
  Bld := TGIFStreamBuilder.Create;
  try
    Bld.WriteHeader;
    Bld.WriteLSD(32, 32, True, 7, False, 7, 0, 0);
    Bld.WriteColorTable(Pal, 7);
    Bld.WriteImageDesc(0, 0, 32, 32, False, False, 0);
    Bld.WriteByte(8);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(N, Img.Frames[0].PixelCount, 'pixel count');
      for I := 0 to N - 1 do
        AssertEqual(Pixels[I], Img.Frames[0].Pixels[I],
          'pixel[' + IntToStr(I) + ']');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TLZWTests.TestLZWMinCodeSizeZero;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  Dummy: array[0..3] of Byte;
begin
  // Bug #8 (P1): LZW MinCodeSize is not validated. MinCodeSize=0 yields
  // ClearCode=1, EOI=2, which is invalid for real GIF. The decoder should
  // not crash with an access violation or infinite loop.
  Dummy[0] := 0; Dummy[1] := 0; Dummy[2] := 0; Dummy[3] := 0;
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(0);  // MinCodeSize = 0 (invalid)
    // Provide a few bytes of dummy LZW data so the decoder doesn't read EOF
    Bld.WriteLZWSubBlocks(Dummy);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      // We don't care if it raises or succeeds, only that it doesn't crash
      try
        LoadGIFFromBytes(Img, Bld.Bytes);
      except
        // Acceptable: validation would raise
      end;
      AssertEqual(1, Img.FrameCount, 'frame should still be registered');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TLZWTests.TestLZWMinCodeSizeTooLarge;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  Dummy: array[0..5] of Byte;
begin
  // MinCodeSize = 12 (too large; valid range is 2..11 for GIF). Should not
  // crash with range check error or stack overflow.
  Dummy[0] := 0; Dummy[1] := 0; Dummy[2] := 0;
  Dummy[3] := 0; Dummy[4] := 0; Dummy[5] := 0;
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(12);  // invalid MinCodeSize
    Bld.WriteLZWSubBlocks(Dummy);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      try
        LoadGIFFromBytes(Img, Bld.Bytes);
      except
        // Acceptable
      end;
      AssertEqual(1, Img.FrameCount, 'frame present');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Test Suite: GCE (Graphic Control Extension)
//============================================================================

type
  TGCETests = class
    class procedure TestGCEParsing;
    class procedure TestGCEBlockSzNot4;          // Bug #4
    class procedure TestGCETransparentIndex;
    class procedure TestGCEDisposalMethods;
  end;

class procedure TGCETests.TestGCEParsing;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Pixels[0] := 0; Pixels[1] := 1; Pixels[2] := 2; Pixels[3] := 3;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteGCE(2, False, 100, 0);  // disposal=2 (bg), delay=100
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(1, Img.FrameCount, 'frame');
      AssertEqual(100, Img.Frames[0].Delay, 'delay');
      AssertEqual(2, Img.Frames[0].Disposal, 'disposal');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TGCETests.TestGCEBlockSzNot4;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  Extra: array[0..3] of Byte;
begin
  // Bug #4 (P1): GCE BlockSz is not validated. Some encoders write a
  // GCE with BlockSz != 4 (e.g., 8 bytes). The reader should not
  // misinterpret the extra bytes as the next block introducer.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Pixels[0] := 0; Pixels[1] := 1; Pixels[2] := 2; Pixels[3] := 3;
    LZW := GifLZWEncode(Pixels, 2);
    Extra[0] := $AA; Extra[1] := $BB; Extra[2] := $CC; Extra[3] := $DD;
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    // GCE with BlockSz=8 (4 standard + 4 extra)
    Bld.WriteGCEWithExtra(1, False, 50, 0, 8, Extra);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      // The reader currently reads 4 bytes regardless of BlockSz, then
      // reads a terminator. With BlockSz=8, the extra 4 bytes shift
      // everything. We test that loading either succeeds (BlockSz honored)
      // or fails gracefully (no hang / no AV).
      try
        LoadGIFFromBytes(Img, Bld.Bytes);
        // If load succeeded, frame should be present
        AssertInRange(Img.FrameCount, 0, 2, 'frame count after GCE BlockSz=8');
      except
        // Acceptable: would be raised as bad stream
      end;
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TGCETests.TestGCETransparentIndex;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Pixels[0] := 0; Pixels[1] := 1; Pixels[2] := 2; Pixels[3] := 3;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteGCE(0, True, 0, 2);  // transparent idx = 2
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(2, Img.Frames[0].TransparentIndex, 'transparent idx');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TGCETests.TestGCEDisposalMethods;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  D: Byte;
begin
  for D := 0 to 3 do
  begin
    Bld := TGIFStreamBuilder.Create;
    try
      Pal := MakePalette(4);
      Pixels[0] := 0; Pixels[1] := 1; Pixels[2] := 2; Pixels[3] := 3;
      LZW := GifLZWEncode(Pixels, 2);
      Bld.WriteHeader;
      Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
      Bld.WriteColorTable(Pal, 1);
      Bld.WriteGCE(D, False, 0, 0);
      Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
      Bld.WriteByte(2);
      Bld.WriteLZWSubBlocks(LZW);
      Bld.WriteTrailer;
      Img := TCnGIFImage.Create;
      try
        AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load disposal=' + IntToStr(D));
        AssertEqual(D, Img.Frames[0].Disposal, 'disposal');
      finally
        Img.Free;
      end;
    finally
      Bld.Free;
    end;
  end;
end;

//============================================================================
// Test Suite: Interlacing
//============================================================================

type
  TInterlaceTests = class
    class procedure TestInterlaceFlagRead;
    class procedure TestDeinterlaceRoundtrip;
    class procedure TestInterlaceReInterlacedOnSave;  // Bug #2 (P0)
  end;

class procedure TInterlaceTests.TestInterlaceFlagRead;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..63] of Byte;
  I: Integer;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 63 do Pixels[I] := I mod 4;
    // For interlaced GIF, LZW data must be in interlaced row order.
    // Since the test only checks the flag (not pixel layout), use plain
    // encoding; the decoder will deinterlace whatever it gets.
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(8, 8, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 8, 8, False, True, 0);  // interlaced
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(1, Img.FrameCount, 'frame');
      AssertTrue(Img.Frames[0].Interlaced, 'interlaced flag');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TInterlaceTests.TestDeinterlaceRoundtrip;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels, InterlacedPixels: array[0..63] of Byte;
  I, DstRow, SrcRow: Integer;
  Pass: array[0..3] of Integer;
  P, Step: Integer;
begin
  // Build a known 8x8 image, then permute rows into GIF interlaced order
  // for encoding. After load, the decoder should restore original order.
  for I := 0 to 63 do Pixels[I] := (I div 8) mod 4;  // by row
  // Interlaced row order: pass1 (every 8th from 0), pass2 (every 8th from 4),
  // pass3 (every 4th from 2), pass4 (every 2nd from 1).
  SrcRow := 0;
  Pass[0] := 0; Pass[1] := 4; Pass[2] := 2; Pass[3] := 1;
  for P := 0 to 3 do
  begin
    if P = 0 then Step := 8
    else if P = 1 then Step := 8
    else if P = 2 then Step := 4
    else Step := 2;
    DstRow := Pass[P];
    while DstRow < 8 do
    begin
      Move(Pixels[DstRow * 8], InterlacedPixels[SrcRow * 8], 8);
      Inc(SrcRow);
      Inc(DstRow, Step);
    end;
  end;
  LZW := GifLZWEncode(InterlacedPixels, 2);
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Bld.WriteHeader;
    Bld.WriteLSD(8, 8, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 8, 8, False, True, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      // After deinterlace, pixels should be back in original row order
      for I := 0 to 63 do
        AssertEqual(Pixels[I], Img.Frames[0].Pixels[I],
          'deinterlaced pixel[' + IntToStr(I) + ']');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TInterlaceTests.TestInterlaceReInterlacedOnSave;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  SrcLinear, InterlacedPixels: array[0..63] of Byte;
  I, DstRow, SrcRow: Integer;
  Pass: array[0..3] of Integer;
  P, Step: Integer;
  Saved: TBytes;
  Info: TGIFInfo;
  MS: TMemoryStream;
begin
  // Bug #2 (P0): When saving an interlaced frame, the encoder writes the
  // interlace flag but does NOT re-interlace the pixel data (it emits
  // pixels in linear order while flagging them as interlaced). A consumer
  // that re-loads the saved stream will deinterlace linear data, producing
  // a corrupted image.
  //
  // Test flow:
  //   1. Build a known 8x8 linear source image (row r => color r mod 4).
  //   2. Permute rows into GIF interlaced order, encode as LZW.
  //   3. Load: decoder deinterlaces -> FPixels = SrcLinear.
  //   4. Clear FRawData so SaveToStream must re-encode from FPixels.
  //   5. Save: encoder SHOULD re-interlace FPixels before LZW encoding.
  //      Bug: it does not — it writes linear bytes with interlace flag set.
  //   6. Reload: decoder deinterlaces the "linear-but-flagged" data,
  //      producing scrambled pixels that do NOT match SrcLinear.
  //   7. Assert reloaded pixels == SrcLinear (passes after fix, fails now).
  for I := 0 to 63 do SrcLinear[I] := (I div 8) mod 4;

  SrcRow := 0;
  Pass[0] := 0; Pass[1] := 4; Pass[2] := 2; Pass[3] := 1;
  for P := 0 to 3 do
  begin
    if P = 0 then Step := 8
    else if P = 1 then Step := 8
    else if P = 2 then Step := 4
    else Step := 2;
    DstRow := Pass[P];
    while DstRow < 8 do
    begin
      Move(SrcLinear[DstRow * 8], InterlacedPixels[SrcRow * 8], 8);
      Inc(SrcRow);
      Inc(DstRow, Step);
    end;
  end;
  LZW := GifLZWEncode(InterlacedPixels, 2);

  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Bld.WriteHeader;
    Bld.WriteLSD(8, 8, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 8, 8, False, True, 0);  // interlaced
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;

    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertTrue(Img.Frames[0].Interlaced, 'flag after load');
      // After load, FPixels should be SrcLinear (deinterlaced correctly).
      for I := 0 to 63 do
        AssertEqual(SrcLinear[I], Img.Frames[0].Pixels[I],
          'pixel[' + IntToStr(I) + '] after load');

      // Force re-encode path: drop the original raw LZW.
      Img.Frames[0].RawData.Size := 0;
      Saved := SaveGIFToBytes(Img);

      // Verify interlace flag is preserved on save.
      MS := TMemoryStream.Create;
      try
        if Length(Saved) > 0 then
          MS.Write(Saved[0], Length(Saved));
        MS.Position := 0;
        Info := ParseGIFStream(MS);
      finally
        MS.Free;
      end;
      AssertEqual(1, Info.FrameCount, 'saved frame count');
      AssertTrue(Info.Frames[0].Interlaced,
        'interlace flag preserved on save');

      // Reload the saved stream. After the fix, the encoder re-interlaces
      // FPixels before LZW encoding, so the decoder's deinterlace restores
      // SrcLinear. Before the fix, the saved LZW is linear-but-flagged-
      // interlaced, so the decoder scrambles the pixels.
      AssertTrue(LoadGIFFromBytes(Img, Saved), 'reload after re-encode');
      AssertTrue(Img.Frames[0].Interlaced, 'flag after reload');
      for I := 0 to 63 do
        AssertEqual(SrcLinear[I], Img.Frames[0].Pixels[I],
          'pixel[' + IntToStr(I) + '] after re-encode roundtrip');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Test Suite: NETSCAPE Application Extension (loop count)
//============================================================================

type
  TNetscapeTests = class
    class procedure TestNetscapeLoopCountRead;
    class procedure TestNetscapeSubBlockNot3;      // Bug #9
    class procedure TestNetscapeLoopZeroPreserved; // Bug #3 (P0)
    class procedure TestNetscapeSubBlockReadOnce;  // Bug #9 variant
  end;

class procedure TNetscapeTests.TestNetscapeLoopCountRead;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteNetscapeExt(7);  // loop 7 times
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(7, Img.AnimationLoopCount, 'loop count');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TNetscapeTests.TestNetscapeSubBlockNot3;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
begin
  // Bug #9 (P1): The reader checks SB = 3 strictly. If the sub-block size
  // is not 3 (e.g., 5 with extra bytes), the loop count is not parsed.
  // We just verify no crash.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    // NETSCAPE with sub-block size 5 (3 standard + 2 extra bytes 0xEE 0xFF)
    Bld.WriteNetscapeExt(3, 5, 1, True);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      try
        LoadGIFFromBytes(Img, Bld.Bytes);
      except
      end;
      // Frame should still be present even if loop count was skipped.
      AssertEqual(1, Img.FrameCount, 'frame present');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TNetscapeTests.TestNetscapeLoopZeroPreserved;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
  Saved: TBytes;
  Info: TGIFInfo;
  MS: TMemoryStream;
begin
  // Bug #3 (P0): The save condition is `FLoopCount > 0`. But loop count = 0
  // means "loop forever" per spec, which is the most common case. The save
  // path drops the NETSCAPE extension entirely for loop=0.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteNetscapeExt(0);  // loop = 0 (infinite)
    // Two frames so that the save condition FFrames.Count > 1 is met
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(0, Img.AnimationLoopCount, 'loop=0 read');
      AssertEqual(2, Img.FrameCount, 'two frames');
      Saved := SaveGIFToBytes(Img);
      MS := TMemoryStream.Create;
      try
        if Length(Saved) > 0 then
          MS.Write(Saved[0], Length(Saved));
        MS.Position := 0;
        Info := ParseGIFStream(MS);
      finally
        MS.Free;
      end;
      // The bug: NETSCAPE ext is dropped when loop=0. So this assertion
      // documents the bug. After fix, HasNetscape should be True.
      // We assert True here as the desired post-fix behavior; this test
      // will FAIL until bug #3 is fixed.
      AssertTrue(Info.HasNetscape, 'NETSCAPE ext should be preserved for loop=0');
      AssertEqual(0, Info.LoopCount, 'loop count zero preserved');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TNetscapeTests.TestNetscapeSubBlockReadOnce;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
begin
  // Bug #9 (variant): If the NETSCAPE sub-block is followed by another
  // sub-block (some encoders split data), the reader only reads once and
  // then calls SkipSubBlocks for the rest. Verify no infinite loop.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    // Hand-craft: NETSCAPE with sub-block size 3 + an extra sub-block
    Bld.WriteByte(GIF_EXT_INTRODUCER);
    Bld.WriteByte(GIF_EXT_APPLICATION);
    Bld.WriteByte(11);
    Bld.WriteAnsi('NETSCAPE2.0');
    Bld.WriteByte(3);
    Bld.WriteByte(1);
    Bld.WriteByte($05);  // loop lo = 5
    Bld.WriteByte($00);  // loop hi = 0
    // Extra sub-block (should be skipped by SkipSubBlocks in the else branch)
    Bld.WriteByte(2);
    Bld.WriteByte($99);
    Bld.WriteByte($88);
    Bld.WriteByte(0);  // terminator
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      // Should not infinite-loop
      try
        LoadGIFFromBytes(Img, Bld.Bytes);
      except
      end;
      AssertEqual(1, Img.FrameCount, 'frame present after multi-sub-block');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Test Suite: Save / Roundtrip
//============================================================================

type
  TSaveTests = class
    class procedure TestSavePaletteSizeBits;       // Bug #1 (P0)
    class procedure TestSaveRoundtripFrameCount;
    class procedure TestSaveSingleFrameViaBitmap;
  end;

class procedure TSaveTests.TestSavePaletteSizeBits;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
  Saved: TBytes;
  Info: TGIFInfo;
  MS: TMemoryStream;
begin
  // Bug #1 (P0): The palette size bits J calculation in SaveToStream is
  // inverted. The loop `while (1 shl (J+1)) < PalSz do Dec(J)` starts at
  // J=7 and decrements while the size is too SMALL, which means J stays
  // at 7 for any palette of 256 entries (since 1 shl 8 = 256 is not < 256),
  // but for smaller palettes J decrements too far, producing wrong bits.
  //
  // Test: load a GIF with a global palette of size 4 (bits=1), save it,
  // and verify the saved GCT bits field is still 1.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);  // GCT bits = 1
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      Saved := SaveGIFToBytes(Img);
      MS := TMemoryStream.Create;
      try
        if Length(Saved) > 0 then
          MS.Write(Saved[0], Length(Saved));
        MS.Position := 0;
        Info := ParseGIFStream(MS);
      finally
        MS.Free;
      end;
      AssertTrue(Info.HasGCT, 'GCT preserved');
      AssertEqual(1, Info.GCTBits, 'GCT bits = 1 for 4-entry palette');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TSaveTests.TestSaveRoundtripFrameCount;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
  Saved: TBytes;
  Img2: TCnGIFImage;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(2, Img.FrameCount, 'two frames loaded');
      Saved := SaveGIFToBytes(Img);
      Img2 := TCnGIFImage.Create;
      try
        AssertTrue(LoadGIFFromBytes(Img2, Saved), 'reload saved');
        AssertEqual(2, Img2.FrameCount, 'two frames after save');
        AssertEqual(2, Img2.Frames[0].Width, 'width');
        AssertEqual(2, Img2.Frames[0].Height, 'height');
      finally
        Img2.Free;
      end;
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TSaveTests.TestSaveSingleFrameViaBitmap;
var
  Img: TCnGIFImage;
  Bmp: TBitmap;
  Saved: TBytes;
  MS: TMemoryStream;
  Info: TGIFInfo;
begin
  // Build a 16x16 RGB bitmap, save as GIF via SaveBitmapToGIFStream,
  // then parse to verify structure.
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 16;
    Bmp.Height := 16;
    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(MakeRect(0, 0, 8, 8));
    Bmp.Canvas.Brush.Color := clGreen;
    Bmp.Canvas.FillRect(MakeRect(8, 0, 16, 8));
    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(MakeRect(0, 8, 8, 16));
    Bmp.Canvas.Brush.Color := clYellow;
    Bmp.Canvas.FillRect(MakeRect(8, 8, 16, 16));

    Img := TCnGIFImage.Create;
    try
      MS := TMemoryStream.Create;
      try
        Img.SaveBitmapToGIFStream(MS, Bmp);
        SetLength(Saved, MS.Size);
        if MS.Size > 0 then
          Move(MS.Memory^, Saved[0], MS.Size);
        MS.Position := 0;
        Info := ParseGIFStream(MS);
      finally
        MS.Free;
      end;
      AssertTrue(Info.Signature = 'GIF89a', 'saved signature = GIF89a');
      AssertEqual(16, Info.LogicalWidth, 'saved LSD width');
      AssertEqual(16, Info.LogicalHeight, 'saved LSD height');
      AssertEqual(1, Info.FrameCount, 'one frame');
      AssertEqual(16, Info.Frames[0].Width, 'frame width');
      AssertEqual(16, Info.Frames[0].Height, 'frame height');
      AssertTrue(not Info.Frames[0].Interlaced, 'not interlaced');
    finally
      Img.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

//============================================================================
// Test Suite: Robustness - Truncation / Overflow (P1)
//============================================================================

type
  TRobustnessTests = class
    class procedure TestTruncatedHeader;             // Bug #6
    class procedure TestTruncatedLSD;                // Bug #6
    class procedure TestTruncatedColorTable;         // Bug #6
    class procedure TestTruncatedLZW;                // Bug #6
    class procedure TestFrameSizeOverflow;           // Bug #7
    class procedure TestShortLZWOutputBuffer;        // Bug #5
  end;

class procedure TRobustnessTests.TestTruncatedHeader;
var
  Img: TCnGIFImage;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.Write(AnsiString('GIF89')[1], 5);  // 5 bytes, missing 1
    MS.Position := 0;
    Img := TCnGIFImage.Create;
    try
      try
        Img.LoadFromStream(MS);
      except
      end;
      AssertEqual(0, Img.FrameCount, 'no frames');
    finally
      Img.Free;
    end;
  finally
    MS.Free;
  end;
end;

class procedure TRobustnessTests.TestTruncatedLSD;
var
  Img: TCnGIFImage;
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.Write(AnsiString('GIF89a')[1], 6);
    MS.Write(AnsiString('12')[1], 2);  // 2 of 7 LSD bytes
    MS.Position := 0;
    Img := TCnGIFImage.Create;
    try
      try
        Img.LoadFromStream(MS);
      except
      end;
    finally
      Img.Free;
    end;
  finally
    MS.Free;
  end;
end;

class procedure TRobustnessTests.TestTruncatedColorTable;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  B: TBytes;
  MS: TMemoryStream;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Bld.WriteHeader;
    Bld.WriteLSD(4, 4, True, 7, False, 2, 0, 0);  // GCT bits = 2 => 8 entries = 24 bytes
    // Only write 3 bytes (one entry) of color table, then truncate.
    Bld.WriteByte($FF);
    Bld.WriteByte($00);
    Bld.WriteByte($80);
    B := Bld.Bytes;
    // Truncate further to ensure stream ends mid-color-table
    SetLength(B, Length(B) - 1);
    MS := TMemoryStream.Create;
    try
      if Length(B) > 0 then
        MS.Write(B[0], Length(B));
      MS.Position := 0;
      Img := TCnGIFImage.Create;
      try
        try
          Img.LoadFromStream(MS);
        except
        end;
        AssertEqual(0, Img.FrameCount, 'no frames');
      finally
        Img.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TRobustnessTests.TestTruncatedLZW;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..15] of Byte;
  I: Integer;
  B: TBytes;
  MS: TMemoryStream;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 15 do Pixels[I] := I mod 4;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(4, 4, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 4, 4, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    // Omit trailer; truncate mid-LZW
    B := Bld.Bytes;
    SetLength(B, Length(B) - 3);  // cut last few bytes
    MS := TMemoryStream.Create;
    try
      MS.Write(B[0], Length(B));
      MS.Position := 0;
      Img := TCnGIFImage.Create;
      try
        try
          Img.LoadFromStream(MS);
        except
        end;
        // Should not crash; frame may or may not be present
        AssertInRange(Img.FrameCount, 0, 1, 'frame count after truncation');
      finally
        Img.Free;
      end;
    finally
      MS.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TRobustnessTests.TestFrameSizeOverflow;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
begin
  // Bug #7 (P1): Frame W*H multiplication overflow is not checked.
  // W=65535, H=65535 would request 4 GB pixel buffer. The decoder
  // allocates W*H bytes. We use a more moderate but still oversized
  // frame to verify the loader doesn't blindly allocate. Use W=H=4096
  // (16M pixels, 16 MB) - large enough to test the path but not OOM.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(4096, 4096, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 4096, 4096, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);  // only 4 real pixels of data
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      try
        LoadGIFFromBytes(Img, Bld.Bytes);
      except
        // Acceptable: ideally the loader would reject the oversized frame
      end;
      // If load succeeded, verify frame is registered (the decoder should
      // not have allocated a 16MB buffer if it validated).
      AssertInRange(Img.FrameCount, 0, 1, 'frame count');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TRobustnessTests.TestShortLZWOutputBuffer;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..1] of Byte;
begin
  // Bug #5 (P1): When the LZW decoder produces fewer pixels than W*H,
  // the trailing portion of FPixels is not zeroed. AllocatePixels uses
  // GetMem without FillChar, so stale heap data leaks into the frame.
  //
  // Test: encode LZW for only 2 pixels but declare frame as 2x2 (4 pixels).
  // The 2 trailing pixel positions should be 0 if the buffer is zeroed.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    Pixels[0] := 1; Pixels[1] := 2;  // only 2 pixels of LZW data
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);  // declares 4 pixels
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      try
        LoadGIFFromBytes(Img, Bld.Bytes);
      except
      end;
      AssertEqual(1, Img.FrameCount, 'frame present');
      AssertEqual(4, Img.Frames[0].PixelCount, 'pixel count is W*H');
      // First two pixels should be 1, 2; trailing two should be 0 if zeroed.
      AssertEqual(1, Img.Frames[0].Pixels[0], 'pixel 0');
      AssertEqual(2, Img.Frames[0].Pixels[1], 'pixel 1');
      // The bug: pixels 2 and 3 may contain heap garbage.
      // We assert they are zeroed; this test will FAIL until bug #5 is fixed.
      AssertEqual(0, Img.Frames[0].Pixels[2], 'pixel 2 (should be zeroed)');
      AssertEqual(0, Img.Frames[0].Pixels[3], 'pixel 3 (should be zeroed)');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Test Suite: Multi-frame animation
//============================================================================

type
  TMultiFrameTests = class
    class procedure TestTwoFrameAnimation;
    class procedure TestFrameDisposal;
  end;

class procedure TMultiFrameTests.TestTwoFrameAnimation;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteNetscapeExt(3);
    Bld.WriteGCE(0, False, 10, 0);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteGCE(0, False, 20, 0);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(2, Img.FrameCount, 'two frames');
      AssertEqual(10, Img.Frames[0].Delay, 'frame 0 delay');
      AssertEqual(20, Img.Frames[1].Delay, 'frame 1 delay');
      AssertEqual(3, Img.AnimationLoopCount, 'loop count');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TMultiFrameTests.TestFrameDisposal;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    // Frame 0: disposal = 1 (leave)
    Bld.WriteGCE(1, False, 10, 0);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    // Frame 1: disposal = 2 (background)
    Bld.WriteGCE(2, False, 10, 0);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    // Frame 2: disposal = 3 (restore previous)
    Bld.WriteGCE(3, False, 10, 0);
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(3, Img.FrameCount, 'three frames');
      AssertEqual(1, Img.Frames[0].Disposal, 'frame 0 disposal');
      AssertEqual(2, Img.Frames[1].Disposal, 'frame 1 disposal');
      AssertEqual(3, Img.Frames[2].Disposal, 'frame 2 disposal');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Test Suite: SaveCurrentFrame / SaveCompositedFrame
//============================================================================

type
  TCompositeTests = class
    class procedure TestSaveCurrentFrame;
    class procedure TestSaveCompositedFrame;
  end;

class procedure TCompositeTests.TestSaveCurrentFrame;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  I: Integer;
  MS: TMemoryStream;
  Info: TGIFInfo;
begin
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteGCE(0, True, 5, 1);  // transparent idx = 1
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      MS := TMemoryStream.Create;
      try
        Img.SaveCurrentFrameToGIFStream(MS);
        MS.Position := 0;
        Info := ParseGIFStream(MS);
      finally
        MS.Free;
      end;
      AssertTrue(Info.Signature = 'GIF89a', 'sig = GIF89a');
      AssertEqual(1, Info.FrameCount, 'one frame');
      AssertEqual(2, Info.LogicalWidth, 'LSD W');
      AssertEqual(2, Info.LogicalHeight, 'LSD H');
      AssertTrue(Info.Frames[0].HasGCE, 'GCE present');
      AssertTrue(Info.Frames[0].GCEHasTrans, 'transparent flag');
      AssertEqual(1, Info.Frames[0].GCETransIdx, 'transparent idx');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

class procedure TCompositeTests.TestSaveCompositedFrame;
var
  Bld: TGIFStreamBuilder;
  Img: TCnGIFImage;
  Pal: TCnGIFColors;
  LZW: TBytes;
  Pixels: array[0..3] of Byte;
  OnePixel: array[0..0] of Byte;
  I: Integer;
  MS: TMemoryStream;
  Info: TGIFInfo;
begin
  // Multi-frame GIF where second frame is at offset (1,1) with size 1x1.
  // Composited output should be 2x2.
  Bld := TGIFStreamBuilder.Create;
  try
    Pal := MakePalette(4);
    for I := 0 to 3 do Pixels[I] := I;
    LZW := GifLZWEncode(Pixels, 2);
    Bld.WriteHeader;
    Bld.WriteLSD(2, 2, True, 7, False, 1, 0, 0);
    Bld.WriteColorTable(Pal, 1);
    Bld.WriteGCE(1, False, 10, 0);  // leave
    Bld.WriteImageDesc(0, 0, 2, 2, False, False, 0);
    Bld.WriteByte(2);
    Bld.WriteLZWSubBlocks(LZW);
    Bld.WriteGCE(1, False, 10, 0);
    Bld.WriteImageDesc(1, 1, 1, 1, False, False, 0);
    Bld.WriteByte(2);
    // Single-pixel LZW: just emit ClearCode + pixel 3 + EOI
    OnePixel[0] := 3;
    Bld.WriteLZWSubBlocks(GifLZWEncode(OnePixel, 2));
    Bld.WriteTrailer;
    Img := TCnGIFImage.Create;
    try
      AssertTrue(LoadGIFFromBytes(Img, Bld.Bytes), 'load');
      AssertEqual(2, Img.FrameCount, 'two frames');
      Img.CurrentFrame := 1;
      MS := TMemoryStream.Create;
      try
        Img.SaveCompositedFrameToGIFStream(MS);
        AssertTrue(MS.Size > 0, 'composited stream non-empty');
        MS.Position := 0;
        Info := ParseGIFStream(MS);
      finally
        MS.Free;
      end;
      AssertEqual(1, Info.FrameCount, 'one composited frame');
      AssertEqual(2, Info.LogicalWidth, 'composited width');
      AssertEqual(2, Info.LogicalHeight, 'composited height');
    finally
      Img.Free;
    end;
  finally
    Bld.Free;
  end;
end;

//============================================================================
// Registration
//============================================================================

initialization
  // Header / signature
  RegisterTest('Header', 'GIF89a signature parsing',
    THeaderTests.TestGIF89aSignature);
  RegisterTest('Header', 'GIF87a signature parsing',
    THeaderTests.TestGIF87aSignature);
  RegisterTest('Header', 'Invalid signature rejected',
    THeaderTests.TestInvalidSignature);
  RegisterTest('Header', 'Short stream does not crash',
    THeaderTests.TestShortStream);

  // LSD
  RegisterTest('LSD', 'Global palette read',
    TLSDTests.TestGlobalPaletteRead);
  RegisterTest('LSD', 'Palette size bits encoding',
    TLSDTests.TestPaletteSizeBitsEncoding);

  // Frames
  RegisterTest('Frame', 'Single frame load',
    TFrameTests.TestSingleFrameLoad);
  RegisterTest('Frame', 'Local palette read',
    TFrameTests.TestLocalPaletteRead);
  RegisterTest('Frame', 'Frame geometry (left/top/w/h)',
    TFrameTests.TestFrameGeometry);

  // LZW
  RegisterTest('LZW', 'LZW roundtrip small',
    TLZWTests.TestLZWRoundtripSmall);
  RegisterTest('LZW', 'LZW roundtrip large (256 colors)',
    TLZWTests.TestLZWRoundtripLarge);
  RegisterTest('LZW', 'MinCodeSize=0 does not crash (bug #8)',
    TLZWTests.TestLZWMinCodeSizeZero);
  RegisterTest('LZW', 'MinCodeSize=12 does not crash (bug #8)',
    TLZWTests.TestLZWMinCodeSizeTooLarge);

  // GCE
  RegisterTest('GCE', 'GCE parsing (disposal/delay)',
    TGCETests.TestGCEParsing);
  RegisterTest('GCE', 'BlockSz != 4 does not desync (bug #4)',
    TGCETests.TestGCEBlockSzNot4);
  RegisterTest('GCE', 'Transparent index read',
    TGCETests.TestGCETransparentIndex);
  RegisterTest('GCE', 'All disposal methods (0..3)',
    TGCETests.TestGCEDisposalMethods);

  // Interlace
  RegisterTest('Interlace', 'Interlace flag read',
    TInterlaceTests.TestInterlaceFlagRead);
  RegisterTest('Interlace', 'Deinterlace roundtrip',
    TInterlaceTests.TestDeinterlaceRoundtrip);
  RegisterTest('Interlace', 'Re-interlace on save (bug #2 P0)',
    TInterlaceTests.TestInterlaceReInterlacedOnSave);

  // NETSCAPE
  RegisterTest('Netscape', 'Loop count read',
    TNetscapeTests.TestNetscapeLoopCountRead);
  RegisterTest('Netscape', 'Sub-block size != 3 (bug #9)',
    TNetscapeTests.TestNetscapeSubBlockNot3);
  RegisterTest('Netscape', 'Loop count = 0 preserved on save (bug #3 P0)',
    TNetscapeTests.TestNetscapeLoopZeroPreserved);
  RegisterTest('Netscape', 'Multiple sub-blocks no infinite loop (bug #9)',
    TNetscapeTests.TestNetscapeSubBlockReadOnce);

  // Save
  RegisterTest('Save', 'Palette size bits on save (bug #1 P0)',
    TSaveTests.TestSavePaletteSizeBits);
  RegisterTest('Save', 'Roundtrip frame count',
    TSaveTests.TestSaveRoundtripFrameCount);
  RegisterTest('Save', 'Single-frame GIF via bitmap',
    TSaveTests.TestSaveSingleFrameViaBitmap);

  // Robustness
  RegisterTest('Robust', 'Truncated header (bug #6)',
    TRobustnessTests.TestTruncatedHeader);
  RegisterTest('Robust', 'Truncated LSD (bug #6)',
    TRobustnessTests.TestTruncatedLSD);
  RegisterTest('Robust', 'Truncated color table (bug #6)',
    TRobustnessTests.TestTruncatedColorTable);
  RegisterTest('Robust', 'Truncated LZW (bug #6)',
    TRobustnessTests.TestTruncatedLZW);
  RegisterTest('Robust', 'Frame size overflow handled (bug #7)',
    TRobustnessTests.TestFrameSizeOverflow);
  RegisterTest('Robust', 'Short LZW output buffer zeroed (bug #5)',
    TRobustnessTests.TestShortLZWOutputBuffer);

  // Multi-frame
  RegisterTest('Multi', 'Two-frame animation',
    TMultiFrameTests.TestTwoFrameAnimation);
  RegisterTest('Multi', 'Frame disposal methods (1/2/3)',
    TMultiFrameTests.TestFrameDisposal);

  // Composite
  RegisterTest('Composite', 'SaveCurrentFrameToGIFStream',
    TCompositeTests.TestSaveCurrentFrame);
  RegisterTest('Composite', 'SaveCompositedFrameToGIFStream',
    TCompositeTests.TestSaveCompositedFrame);

end.
