{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGIF;
{* |<PRE>
===============================================================================
* 单元名称：CnGIF
* 单元说明：GIF89a 图像格式解析与写入单元
*           支持 GIF87a/GIF89a 的读取、多帧动画、LZW 解压缩
*           支持 GIF89a 的写入（LZW 压缩）
*           通过 TGraphic 派生融入 Delphi TPicture 体系
* 开发平台：PWin98SE + Delphi 5.0
* 兼容平台：Delphi 5~最新、FPC
* 兼容系统：Windows / MacOS (FPC)
* 修改记录：2026.06.25 V1.0
*               创建单元
===============================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Graphics, Contnrs, CnNative;

type
  PCnGIFColor = ^TCnGIFColor;
  TCnGIFColor = packed record
    R, G, B: Byte;
  end;

  TCnGIFColors = array of TCnGIFColor;

//============================================================================
// TCnGIFFrame
//============================================================================

  TCnGIFFrame = class
  private
    FLeft: Word;
    FTop: Word;
    FWidth: Word;
    FHeight: Word;
    FInterlaced: Boolean;
    FLocalPalette: TCnGIFColors;
    FHasLocalPalette: Boolean;
    FTransparentIndex: Integer;
    FDelay: Word;
    FDisposal: Byte;
    FUserInput: Boolean;
    FPixels: PByteArray;
    FPixelCount: Integer;
    FRawData: TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AllocatePixels(Count: Integer);
    property Left: Word read FLeft write FLeft;
    property Top: Word read FTop write FTop;
    property Width: Word read FWidth write FWidth;
    property Height: Word read FHeight write FHeight;
    property Interlaced: Boolean read FInterlaced write FInterlaced;
    property TransparentIndex: Integer read FTransparentIndex write FTransparentIndex;
    property Delay: Word read FDelay write FDelay;
    property Disposal: Byte read FDisposal write FDisposal;
    property UserInput: Boolean read FUserInput write FUserInput;
    property Pixels: PByteArray read FPixels;
    property PixelCount: Integer read FPixelCount;
    property RawData: TMemoryStream read FRawData;
    property HasLocalPalette: Boolean read FHasLocalPalette write FHasLocalPalette;
    property LocalPalette: TCnGIFColors read FLocalPalette write FLocalPalette;
  end;

  //============================================================================
  // TCnGIFImage
  //============================================================================

  TCnGIFImage = class(TGraphic)
  private
    // GIF 文件头
    FHeader: array[0..5] of AnsiChar;
    FLogicalScreenWidth: Word;
    FLogicalScreenHeight: Word;
    FColorResolution: Byte;
    FBackgroundColorIndex: Byte;
    FPixelAspectRatio: Byte;
    FGlobalPalette: TCnGIFColors;
    FHasGlobalPalette: Boolean;
    FSortFlag: Boolean;

    // 帧
    FFrames: TObjectList;
    FCurrentFrame: Integer;
    FLoopCount: Integer;
    FHasNetscape: Boolean;

    // 渲染缓存
    FCompositeBuf: PByteArray;
    FCompWidth: Integer;
    FCompHeight: Integer;
    FRenderedFrame: Integer;
    FDIB: HBITMAP;
    FDIBBits: Pointer;
    FDIBW: Integer;
    FDIBH: Integer;

    // 待定 GCE（GCE 在 Image Descriptor 之前）
    FPendingDelay: Word;
    FPendingDisposal: Byte;
    FPendingTransparent: Integer;
    FPendingUserInput: Boolean;
    FHasPendingGCE: Boolean;

    // 缓存管理
    procedure FreeComposite;
    procedure EnsureComposite(W, H: Integer);
    procedure FreeDIB;
    procedure EnsureDIB(W, H: Integer);
    procedure EnsureRendered(FrameIdx: Integer);

    // 流辅助
    procedure ReadExact(Stream: TStream; var Buffer; Count: Integer);
    function  ReadByte(Stream: TStream): Byte;
    function  ReadWord(Stream: TStream): Word;
    procedure WriteByte(Stream: TStream; B: Byte);
    procedure WriteWord(Stream: TStream; W: Word);

    // 解析
    procedure ReadColorTable(Stream: TStream;
      var Palette: TCnGIFColors; Count: Integer);
    procedure ReadSubBlocks(Stream: TStream; Data: TStream);
    procedure SkipSubBlocks(Stream: TStream);
    procedure ReadGraphicCtrlExt(Stream: TStream);
    procedure ReadAppExt(Stream: TStream);
    procedure ReadPlainTextExt(Stream: TStream);

    // 合成
    procedure CompositeFrames(LastFrame: Integer);
    function  AdvanceCompositeFrame(FrameIdx: Integer): Boolean;

    // 写入
    procedure WriteColorTable(Stream: TStream;
      const Palette: TCnGIFColors; Count: Integer);
    procedure EmitSubBlocks(Stream: TStream; Data: Pointer; Size: Integer);

    // LZW
    procedure DecodeLZW(InData: PByte; InSize: Integer; OutStm: TStream;
      MinCodeSize: Integer; PixelCount: Integer);
    procedure EncodeLZW(InData: PByteArray; InSize: Integer; OutStm: TStream;
      MinCodeSize: Integer);

    // 隔行扫描
    procedure Deinterlace(Frame: TCnGIFFrame);
    procedure InterlacePixels(Frame: TCnGIFFrame; Dst: PByteArray);

    procedure SetCurrentFrame(Value: Integer);
    procedure SetAnimationLoopCount(Value: Integer);
    function  GetFrameCount: Integer;
    function  GetFrame(Index: Integer): TCnGIFFrame;

    // 单帧 GIF 保存辅助
    procedure QuantizeBitmap(Src: TBitmap; var Palette: TCnGIFColors;
      Indices: PByteArray);
    procedure QuantizeComposite(var Palette: TCnGIFColors; Indices: PByteArray);
    procedure WriteSingleFrameGIF(Stream: TStream; W, H: Integer;
      const Palette: TCnGIFColors; Indices: PByteArray);

  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveBitmapToGIFStream(Stream: TStream; Src: TBitmap);
    procedure SaveBitmapToGIFFile(const FileName: string; Src: TBitmap);
    procedure SaveCurrentFrameToGIFStream(Stream: TStream);
    procedure SaveCurrentFrameToGIFFile(const FileName: string);
    procedure SaveCompositedFrameToGIFStream(Stream: TStream);
    procedure SaveCompositedFrameToGIFFile(const FileName: string);
    procedure Clear; {$IFDEF FPC} override; {$ENDIF}
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); {$IFNDEF FPC} override; {$ENDIF}
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); {$IFNDEF FPC} override; {$ENDIF}

    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    property FrameCount: Integer read GetFrameCount;
    property Frames[Index: Integer]: TCnGIFFrame read GetFrame;
    property AnimationLoopCount: Integer read FLoopCount write SetAnimationLoopCount;
    property HasAnimationLoop: Boolean read FHasNetscape write FHasNetscape;
  end;

// 注册
procedure RegisterCnGIF;

// 注销
procedure UnregisterCnGIF;

implementation

resourcestring
  SCnGIFInvalidImageSize = 'Invalid GIF Image Size';
  SCnGIFInvalidDataSize = 'Invalid GIF Data Size';
  SCnGIFUnexpectedEndOfStream = 'Unexpected End of GIF Stream';
  SCnGIFInvalidSignature = 'Invalid GIF Signature';
  SCnGIFInvalidLZWMinCodeSize = 'Invalid GIF LZW Minimum Code Size';
  SCnGIFInvalidPaletteSize = 'Invalid GIF Palette Size';

const
  GIF87a: array[0..5] of AnsiChar = 'GIF87a';
  GIF89a: array[0..5] of AnsiChar = 'GIF89a';

  // 块标识
  GIF_EXT_INTRODUCER    = $21;
  GIF_IMAGE_DESCRIPTOR  = $2C;
  GIF_TRAILER           = $3B;

  // 扩展标签
  GIF_EXT_GRAPHIC_CTRL  = $F9;
  GIF_EXT_COMMENT       = $FE;
  GIF_EXT_PLAIN_TEXT    = $01;
  GIF_EXT_APPLICATION   = $FF;

  // 销毁方式
  GIF_DISPOSAL_UNSPEC   = 0;
  GIF_DISPOSAL_LEAVE    = 1;
  GIF_DISPOSAL_BG       = 2;
  GIF_DISPOSAL_PREV     = 3;

  // 最大 LZW 码
  GIF_MAX_CODES = 4096;

type
  PCnGIFQuad = ^TCnGIFQuad;
  TCnGIFQuad = packed record
    B, G, R, A: Byte;
  end;

  PQuadArray = ^TQuadArray;
  TQuadArray = array[0..0] of TCnGIFQuad;

  TDecEntry = packed record
    Prefix: Word;
    Suffix: Byte;
  end;

  PCnQuantHashEntry = ^TCnQuantHashEntry;
  TCnQuantHashEntry = packed record
    Used: Boolean;
    B, G, R: Byte;
    Count: Cardinal;
    PalIdx: Byte;
  end;

  TCnQuantColor = packed record
    B, G, R: Byte;
    Count: Cardinal;
  end;

  TCnQuantBucket = record
    StartIdx: Integer;
    Num: Integer;
    TotalCount: Cardinal;
    RngB, RngG, RngR: Integer;
    SplitChan: Integer;
  end;

{$R-}

function GIFSafeMultiply(A, B: Integer): Integer;
begin
  if (A < 0) or (B < 0) or
     ((A <> 0) and (B > MaxInt div A)) then
    raise Exception.Create(SCnGIFInvalidImageSize);
  Result := A * B;
end;

function GIFPixelCount(W, H: Integer): Integer;
begin
  Result := GIFSafeMultiply(W, H);
end;

function GIFQuadBufferSize(W, H: Integer): Integer;
begin
  Result := GIFSafeMultiply(GIFPixelCount(W, H), 4);
end;

procedure GIFValidateLZWMinCodeSize(MinCodeSize: Integer);
begin
  if (MinCodeSize < 2) or (MinCodeSize > 8) then
    raise Exception.Create(SCnGIFInvalidLZWMinCodeSize);
end;

function GIFColorHash(Key: Cardinal): Cardinal;
begin
  Result := (Key xor (Key shr 11) xor (Key shr 22)) * 2654435761;
end;

function GIFPaletteTableSize(PaletteSize: Integer): Integer;
begin
  if PaletteSize > 256 then
    raise Exception.Create(SCnGIFInvalidPaletteSize);
  Result := 2;
  while Result < PaletteSize do
    Result := Result shl 1;
end;

//==============================================================================
// TCnGIFFrame
//==============================================================================

constructor TCnGIFFrame.Create;
begin
  inherited;
  FTransparentIndex := -1;
  FUserInput := False;
  FRawData := TMemoryStream.Create;
end;

destructor TCnGIFFrame.Destroy;
begin
  FRawData.Free;
  if FPixels <> nil then
    FreeMem(FPixels);
  inherited;
end;

procedure TCnGIFFrame.AllocatePixels(Count: Integer);
begin
  if Count < 0 then
    raise Exception.Create(SCnGIFInvalidImageSize);
  if FPixels <> nil then
    FreeMem(FPixels);
  FPixelCount := Count;
  if Count > 0 then
  begin
    GetMem(FPixels, Count);
    FillChar(FPixels^, Count, 0);
  end
  else
    FPixels := nil;
end;

//==============================================================================
// TCnGIFImage - 构造 / 析构 / Clear
//==============================================================================

constructor TCnGIFImage.Create;
begin
  inherited;
  FFrames := TObjectList.Create(True);
  FCurrentFrame := 0;
  FRenderedFrame := -1;
  FDIB := 0;
  FDIBBits := nil;
  FCompositeBuf := nil;
  FCompWidth := 0;
  FCompHeight := 0;
  FPendingUserInput := False;
  FHasPendingGCE := False;
end;

destructor TCnGIFImage.Destroy;
begin
  Clear;
  FFrames.Free;
  FreeComposite;
  FreeDIB;
  inherited;
end;

procedure TCnGIFImage.Clear;
begin
  FFrames.Clear;
  FHasGlobalPalette := False;
  SetLength(FGlobalPalette, 0);
  FLogicalScreenWidth := 0;
  FLogicalScreenHeight := 0;
  FCurrentFrame := 0;
  FRenderedFrame := -1;
  FLoopCount := 0;
  FHasNetscape := False;
  FPendingUserInput := False;
  FHasPendingGCE := False;
  FreeComposite;
  FreeDIB;
end;

//==============================================================================
// 缓存
//==============================================================================

procedure TCnGIFImage.FreeComposite;
begin
  if FCompositeBuf <> nil then
  begin
    FreeMem(FCompositeBuf);
    FCompositeBuf := nil;
  end;
  FCompWidth := 0;
  FCompHeight := 0;
end;

procedure TCnGIFImage.EnsureComposite(W, H: Integer);
var
  Sz: Integer;
begin
  Sz := GIFQuadBufferSize(W, H);
  if (FCompWidth >= W) and (FCompHeight >= H) then
  begin
    FillChar(FCompositeBuf^, Sz, 0);
    Exit;
  end;
  FreeComposite;
  FCompWidth := W;
  FCompHeight := H;
  GetMem(FCompositeBuf, Sz);
  FillChar(FCompositeBuf^, Sz, 0);
end;

procedure TCnGIFImage.FreeDIB;
begin
  if FDIB <> 0 then
  begin
    DeleteObject(FDIB);
    FDIB := 0;
  end;
  FDIBBits := nil;
  FDIBW := 0;
  FDIBH := 0;
end;

procedure TCnGIFImage.EnsureDIB(W, H: Integer);
var
  BMI: TBitmapInfo;
  DC: HDC;
begin
  if (FDIB <> 0) and (FDIBW = W) and (FDIBH = H) then
    Exit;
  FreeDIB;

  FillChar(BMI, SizeOf(BMI), 0);
  BMI.bmiHeader.biSize := SizeOf(BMI.bmiHeader);
  BMI.bmiHeader.biWidth := W;
  BMI.bmiHeader.biHeight := -H;  // top-down
  BMI.bmiHeader.biPlanes := 1;
  BMI.bmiHeader.biBitCount := 32;
  BMI.bmiHeader.biCompression := BI_RGB;

  DC := GetDC(0);
  try
    FDIB := CreateDIBSection(DC, BMI, DIB_RGB_COLORS, FDIBBits, 0, 0);
  finally
    ReleaseDC(0, DC);
  end;
  FDIBW := W;
  FDIBH := H;
end;

//==============================================================================
// 流辅助
//==============================================================================

procedure TCnGIFImage.ReadExact(Stream: TStream; var Buffer; Count: Integer);
var
  P: PByte;
  N: Integer;
begin
  if Count < 0 then
    raise Exception.Create(SCnGIFInvalidDataSize);
  P := @Buffer;
  while Count > 0 do
  begin
    N := Stream.Read(P^, Count);
    if N <= 0 then
      raise Exception.Create(SCnGIFUnexpectedEndOfStream);
    Inc(P, N);
    Dec(Count, N);
  end;
end;

function TCnGIFImage.ReadByte(Stream: TStream): Byte;
begin
  ReadExact(Stream, Result, 1);
end;

function TCnGIFImage.ReadWord(Stream: TStream): Word;
begin
  ReadExact(Stream, Result, 2);
end;

procedure TCnGIFImage.WriteByte(Stream: TStream; B: Byte);
begin
  Stream.Write(B, 1);
end;

procedure TCnGIFImage.WriteWord(Stream: TStream; W: Word);
begin
  Stream.Write(W, 2);
end;

//==============================================================================
// 解析
//==============================================================================

procedure TCnGIFImage.ReadColorTable(Stream: TStream;
  var Palette: TCnGIFColors; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ReadExact(Stream, Palette[I], 3);
end;

procedure TCnGIFImage.ReadSubBlocks(Stream: TStream; Data: TStream);
var
  Sz: Byte;
  Buf: array[0..254] of Byte;
begin
  while True do
  begin
    Sz := ReadByte(Stream);
    if Sz = 0 then
      Break;
    ReadExact(Stream, Buf, Sz);
    Data.WriteBuffer(Buf, Sz);
  end;
end;

procedure TCnGIFImage.SkipSubBlocks(Stream: TStream);
var
  Sz: Byte;
  Buf: array[0..254] of Byte;
begin
  while True do
  begin
    Sz := ReadByte(Stream);
    if Sz = 0 then
      Break;
    ReadExact(Stream, Buf, Sz);
  end;
end;

procedure TCnGIFImage.ReadGraphicCtrlExt(Stream: TStream);
var
  BlockSz: Byte;
  Pkd: Byte;
  Delay: Word;
  TransIdx: Byte;
begin
  BlockSz := ReadByte(Stream);  // 应为 4
  Pkd := ReadByte(Stream);
  Delay := ReadWord(Stream);
  TransIdx := ReadByte(Stream);
  ReadByte(Stream);  // 终结符

  // 存储为待定状态，将在下一个 Image Descriptor 时应用
  FHasPendingGCE := True;
  FPendingDelay := Delay;
  FPendingDisposal := (Pkd and $1C) shr 2;
  FPendingUserInput := (Pkd and $02) <> 0;
  if (Pkd and $01) <> 0 then
    FPendingTransparent := TransIdx
  else
    FPendingTransparent := -1;
end;

procedure TCnGIFImage.ReadAppExt(Stream: TStream);
var
  BlockSz: Byte;
  AppId: array[0..10] of AnsiChar;
  SB: Byte;
  SubBuf: array[0..254] of Byte;
  B1, B2: Byte;
  IsNetscape: Boolean;
begin
  BlockSz := ReadByte(Stream);
  if BlockSz <> 11 then
  begin
    if BlockSz > 0 then
      ReadExact(Stream, SubBuf, BlockSz);
    SkipSubBlocks(Stream);
    Exit;
  end;
  ReadExact(Stream, AppId, 11);

  IsNetscape := (AppId[0] = 'N') and (AppId[1] = 'E') and (AppId[2] = 'T') and
     (AppId[3] = 'S') and (AppId[4] = 'C') and (AppId[5] = 'A') and
     (AppId[6] = 'P') and (AppId[7] = 'E');

  while True do
  begin
    SB := ReadByte(Stream);
    if SB = 0 then
      Break;
    ReadExact(Stream, SubBuf, SB);
    if IsNetscape and (SB = 3) and (SubBuf[0] = 1) then
    begin
      B1 := SubBuf[1];
      B2 := SubBuf[2];
      FLoopCount := B1 or (B2 shl 8);
      FHasNetscape := True;
    end;
  end;
end;

procedure TCnGIFImage.ReadPlainTextExt(Stream: TStream);
var
  BlockSz: Byte;
  Buf: array[0..254] of Byte;
begin
  // Plain Text Extension has a fixed-size application block followed by
  // data sub-blocks.  It is a graphic-rendering block, so any pending GCE
  // applies to it and must not leak to the next image descriptor.
  BlockSz := ReadByte(Stream);
  if BlockSz > 0 then
    ReadExact(Stream, Buf, BlockSz);
  SkipSubBlocks(Stream);
  FHasPendingGCE := False;
end;

//==============================================================================
// LoadFromStream
//==============================================================================

procedure TCnGIFImage.LoadFromStream(Stream: TStream);
var
  B: Byte;
  Frame: TCnGIFFrame;
  LZWCodeSize: Byte;
  PalSize: Integer;
  Pkd: Byte;
  Temp: TMemoryStream;
  PixelCount: Integer;
begin
  Clear;

  // Header
  ReadExact(Stream, FHeader, 6);
  if (FHeader <> GIF87a) and (FHeader <> GIF89a) then
    raise Exception.Create(SCnGIFInvalidSignature);

  // Logical Screen Descriptor
  FLogicalScreenWidth := ReadWord(Stream);
  FLogicalScreenHeight := ReadWord(Stream);
  Pkd := ReadByte(Stream);
  FHasGlobalPalette := (Pkd and $80) <> 0;
  FColorResolution   := (Pkd and $70) shr 4;
  FSortFlag          := (Pkd and $08) <> 0;

  FBackgroundColorIndex := ReadByte(Stream);
  FPixelAspectRatio     := ReadByte(Stream);

  // Global Color Table
  if FHasGlobalPalette then
  begin
    PalSize := 1 shl ((Pkd and $07) + 1);
    SetLength(FGlobalPalette, PalSize);
    ReadColorTable(Stream, FGlobalPalette, PalSize);
  end;

  // 逐块遍历
  while Stream.Position < Stream.Size do
  begin
    B := ReadByte(Stream);
    case B of
      GIF_EXT_INTRODUCER:
        begin
          B := ReadByte(Stream);
          case B of
            GIF_EXT_GRAPHIC_CTRL: ReadGraphicCtrlExt(Stream);
            GIF_EXT_APPLICATION:  ReadAppExt(Stream);
            GIF_EXT_COMMENT:      SkipSubBlocks(Stream);
            GIF_EXT_PLAIN_TEXT:   ReadPlainTextExt(Stream);
          else
            SkipSubBlocks(Stream);
          end;
        end;

      GIF_IMAGE_DESCRIPTOR:
        begin
          Frame := TCnGIFFrame.Create;
          try

            // 应用待定 GCE
            if FHasPendingGCE then
            begin
              Frame.FDelay := FPendingDelay;
              Frame.FDisposal := FPendingDisposal;
              Frame.FTransparentIndex := FPendingTransparent;
              Frame.FUserInput := FPendingUserInput;
              FHasPendingGCE := False;
              FPendingUserInput := False;
            end;

            Frame.FLeft   := ReadWord(Stream);
            Frame.FTop    := ReadWord(Stream);
            Frame.FWidth  := ReadWord(Stream);
            Frame.FHeight := ReadWord(Stream);

            B := ReadByte(Stream);
            Frame.FInterlaced := (B and $40) <> 0;
            if (B and $80) <> 0 then
            begin
              Frame.FHasLocalPalette := True;
              PalSize := 1 shl ((B and $07) + 1);
              SetLength(Frame.FLocalPalette, PalSize);
              ReadColorTable(Stream, Frame.FLocalPalette, PalSize);
            end;

            LZWCodeSize := ReadByte(Stream);
            GIFValidateLZWMinCodeSize(LZWCodeSize);

            // 读取 LZW 子块数据
            Frame.FRawData.Size := 0;
            ReadSubBlocks(Stream, Frame.FRawData);
            if Frame.FRawData.Size > MaxInt then
              raise Exception.Create(SCnGIFInvalidDataSize);

            PixelCount := GIFPixelCount(Frame.FWidth, Frame.FHeight);

            // LZW 解码
            Temp := TMemoryStream.Create;
            try
              DecodeLZW(Frame.FRawData.Memory, Frame.FRawData.Size,
                        Temp, LZWCodeSize, PixelCount);

              Frame.AllocatePixels(PixelCount);
              if Temp.Size > 0 then
                if Temp.Size < PixelCount then
                  Move(Temp.Memory^, Frame.FPixels^, Temp.Size)
                else
                  Move(Temp.Memory^, Frame.FPixels^, PixelCount);

              if Frame.FInterlaced then
                Deinterlace(Frame);
            finally
              Temp.Free;
            end;

            FFrames.Add(Frame);
            Frame := nil;
          finally
            Frame.Free;
          end;
        end;

      GIF_TRAILER:
        Break;
    end;
  end;

  if FFrames.Count > 0 then
  begin
    FCurrentFrame := 0;
    FRenderedFrame := -1;
  end;
end;

//==============================================================================
// LZW 解码
//==============================================================================

procedure TCnGIFImage.DecodeLZW(InData: PByte; InSize: Integer;
  OutStm: TStream; MinCodeSize: Integer; PixelCount: Integer);
var
  Table: array[0..GIF_MAX_CODES - 1] of TDecEntry;
  ClearCode: Integer;
  EOICode: Integer;
  CodeSize: Integer;
  CodeMask: Integer;
  NextCode: Integer;
  OldCode: Integer;
  Code: Integer;
  InPos: Integer;
  BitBuf: Cardinal;
  BitCnt: Integer;
  OutCnt: Integer;
  Stack: array[0..GIF_MAX_CODES] of Byte;
  SP: Integer;
  I: Integer;
  FirstChar: Byte;

  function GetCode: Integer;
  var
    IDP: PByteArray;
  begin
    IDP := PByteArray(InData);
    while BitCnt < CodeSize do
    begin
      if InPos < InSize then
      begin
        BitBuf := BitBuf or (Cardinal(IDP[InPos]) shl BitCnt);
        Inc(InPos);
      end;
      Inc(BitCnt, 8);
    end;
    Result := BitBuf and ((1 shl CodeSize) - 1);
    BitBuf := BitBuf shr CodeSize;
    Dec(BitCnt, CodeSize);
  end;

begin
  GIFValidateLZWMinCodeSize(MinCodeSize);
  if PixelCount <= 0 then
    Exit;

  ClearCode := 1 shl MinCodeSize;
  EOICode   := ClearCode + 1;
  CodeSize  := MinCodeSize + 1;
  CodeMask  := (1 shl CodeSize) - 1;
  NextCode  := ClearCode + 2;
  InPos     := 0;
  BitBuf    := 0;
  BitCnt    := 0;
  OutCnt    := 0;
  FirstChar := 0;

  FillChar(Table, SizeOf(Table), 0);
  for I := 0 to 255 do
  begin
    Table[I].Prefix := 0;
    Table[I].Suffix := Byte(I);
  end;

  // 读第一个码
  Code := GetCode;
  if Code = EOICode then Exit;
  while Code = ClearCode do
    Code := GetCode;
  if Code >= NextCode then
    Code := 0;

  OldCode := Code;
  FirstChar := Table[Code].Suffix;
  OutStm.Write(FirstChar, 1);
  Inc(OutCnt);

  while OutCnt < PixelCount do
  begin
    Code := GetCode;

    if Code = EOICode then
      Break;
    if Code = ClearCode then
    begin
      CodeSize := MinCodeSize + 1;
      CodeMask := (1 shl CodeSize) - 1;
      NextCode := ClearCode + 2;
      FillChar(Table, SizeOf(Table), 0);
      for I := 0 to 255 do
      begin
        Table[I].Prefix := 0;
        Table[I].Suffix := Byte(I);
      end;

      Code := GetCode;
      if Code = EOICode then Break;
      while Code = ClearCode do
        Code := GetCode;
      if Code >= NextCode then
        Code := 0;

      OldCode := Code;
      FirstChar := Table[Code].Suffix;
      OutStm.Write(FirstChar, 1);
      Inc(OutCnt);
      Continue;
    end;

    if Code > NextCode then
      Code := NextCode;

    SP := 0;
    if Code = NextCode then
    begin
      Stack[SP] := FirstChar;  Inc(SP);
      I := OldCode;
    end
    else
      I := Code;

    while I > ClearCode + 1 do
    begin
      if SP >= GIF_MAX_CODES then Break;
      Stack[SP] := Table[I].Suffix;  Inc(SP);
      I := Table[I].Prefix;
    end;
    Stack[SP] := Byte(I);  Inc(SP);
    FirstChar := Byte(I);

    // 输出栈
    while SP > 0 do
    begin
      Dec(SP);
      OutStm.Write(Stack[SP], 1);
      Inc(OutCnt);
      if OutCnt >= PixelCount then Break;
    end;
    if OutCnt >= PixelCount then Break;

    // 添加新的表项
    if NextCode < GIF_MAX_CODES then
    begin
      Table[NextCode].Prefix := OldCode;
      Table[NextCode].Suffix := FirstChar;
      Inc(NextCode);

      if (NextCode > CodeMask) and (CodeSize < 12) then
      begin
        Inc(CodeSize);
        CodeMask := (1 shl CodeSize) - 1;
      end;
    end;

    OldCode := Code;
  end;
end;

//==============================================================================
// 隔行扫描解码
//==============================================================================

procedure TCnGIFImage.Deinterlace(Frame: TCnGIFFrame);
var
  Src: PByteArray;
  Dst: PByteArray;
  RowSize: Integer;
  H: Integer;
  SrcRow: Integer;
  DstRow: Integer;
  PixelCount: Integer;
begin
  H := Frame.FHeight;
  RowSize := Frame.FWidth;
  if (H <= 1) or (RowSize <= 0) then
    Exit;

  PixelCount := GIFPixelCount(RowSize, H);
  GetMem(Src, PixelCount);
  try
    Move(Frame.FPixels^, Src^, PixelCount);
    Dst := Frame.FPixels;
    SrcRow := 0;

    // Pass 1: rows 0, 8, 16, ...
    DstRow := 0;
    while DstRow < H do
    begin
      Move(Src^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
      Inc(SrcRow);
      Inc(DstRow, 8);
    end;

    // Pass 2: rows 4, 12, 20, ...
    DstRow := 4;
    while DstRow < H do
    begin
      Move(Src^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
      Inc(SrcRow);
      Inc(DstRow, 8);
    end;

    // Pass 3: rows 2, 6, 10, 14, ...
    DstRow := 2;
    while DstRow < H do
    begin
      Move(Src^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
      Inc(SrcRow);
      Inc(DstRow, 4);
    end;

    // Pass 4: rows 1, 3, 5, 7, ...
    DstRow := 1;
    while DstRow < H do
    begin
      Move(Src^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
      Inc(SrcRow);
      Inc(DstRow, 2);
    end;
  finally
    FreeMem(Src);
  end;
end;

procedure TCnGIFImage.InterlacePixels(Frame: TCnGIFFrame; Dst: PByteArray);
var
  RowSize: Integer;
  H: Integer;
  SrcRow: Integer;
  DstRow: Integer;
begin
  H := Frame.FHeight;
  RowSize := Frame.FWidth;
  if (H <= 1) or (RowSize <= 0) then
    Exit;

  DstRow := 0;
  // Pass 1: linear rows 0, 8, 16, ...
  SrcRow := 0;
  while SrcRow < H do
  begin
    Move(Frame.FPixels^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
    Inc(DstRow);
    Inc(SrcRow, 8);
  end;
  // Pass 2: linear rows 4, 12, 20, ...
  SrcRow := 4;
  while SrcRow < H do
  begin
    Move(Frame.FPixels^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
    Inc(DstRow);
    Inc(SrcRow, 8);
  end;
  // Pass 3: linear rows 2, 6, 10, 14, ...
  SrcRow := 2;
  while SrcRow < H do
  begin
    Move(Frame.FPixels^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
    Inc(DstRow);
    Inc(SrcRow, 4);
  end;
  // Pass 4: linear rows 1, 3, 5, 7, ...
  SrcRow := 1;
  while SrcRow < H do
  begin
    Move(Frame.FPixels^[SrcRow * RowSize], Dst^[DstRow * RowSize], RowSize);
    Inc(DstRow);
    Inc(SrcRow, 2);
  end;
end;

//==============================================================================
// 帧合成
//==============================================================================

procedure TCnGIFImage.CompositeFrames(LastFrame: Integer);
var
  K, X, Y, I: Integer;
  Frame: TCnGIFFrame;
  Pal: TCnGIFColors;
  PalLen: Integer;
  Q: PQuadArray;
  Pix: PByteArray;
  BufW: Integer;
  SavedArea: PByteArray;
  SavedW, SavedH, SavedSize, ClearWidth: Integer;
  BgColor: TCnGIFColor;
begin
  if (FLogicalScreenWidth <= 0) or (FLogicalScreenHeight <= 0) then
    Exit;

  EnsureComposite(FLogicalScreenWidth, FLogicalScreenHeight);
  BufW := FCompWidth;

  // 清为背景色
  if FHasGlobalPalette and (FBackgroundColorIndex < Length(FGlobalPalette)) then
  begin
    BgColor := FGlobalPalette[FBackgroundColorIndex];
    for Y := 0 to FLogicalScreenHeight - 1 do
    begin
      Q := Pointer(TCnNativeInt(FCompositeBuf) + Y * BufW * 4);
      for X := 0 to FLogicalScreenWidth - 1 do
      begin
        Q^[X].B := BgColor.B;
        Q^[X].G := BgColor.G;
        Q^[X].R := BgColor.R;
        Q^[X].A := 255;
      end;
    end;
  end
  else
  begin
    BgColor.R := 0;
    BgColor.G := 0;
    BgColor.B := 0;
    for Y := 0 to FLogicalScreenHeight - 1 do
    begin
      Q := Pointer(TCnNativeInt(FCompositeBuf) + Y * BufW * 4);
      for X := 0 to FLogicalScreenWidth - 1 do
      begin
        Q^[X].B := 0;
        Q^[X].G := 0;
        Q^[X].R := 0;
        Q^[X].A := 255;
      end;
    end;
  end;

  SavedArea := nil;
  SavedW := 0; SavedH := 0;

  for K := 0 to LastFrame do
  begin
    if K >= FFrames.Count then
      Break;
    Frame := TCnGIFFrame(FFrames[K]);

    if Frame.FHasLocalPalette then
    begin
      Pal := Frame.FLocalPalette;
      PalLen := Length(Pal);
    end
    else
    begin
      Pal := FGlobalPalette;
      PalLen := Length(Pal);
    end;

    if PalLen = 0 then
      Continue;

    // 应用前一帧的销毁方式
    if K > 0 then
    begin
      if TCnGIFFrame(FFrames[K - 1]).FDisposal = GIF_DISPOSAL_BG then
      begin
        // 用背景色填充前一帧区域
        for Y := TCnGIFFrame(FFrames[K - 1]).FTop to
          TCnGIFFrame(FFrames[K - 1]).FTop + TCnGIFFrame(FFrames[K - 1]).FHeight - 1 do
        begin
          if Y >= FLogicalScreenHeight then
            Break;
          if TCnGIFFrame(FFrames[K - 1]).FLeft < FLogicalScreenWidth then
          begin
            ClearWidth := TCnGIFFrame(FFrames[K - 1]).FWidth;
            if TCnGIFFrame(FFrames[K - 1]).FLeft + ClearWidth > FLogicalScreenWidth then
              ClearWidth := FLogicalScreenWidth - TCnGIFFrame(FFrames[K - 1]).FLeft;
            Q := Pointer(TCnNativeInt(FCompositeBuf) +
              Y * BufW * 4 + TCnGIFFrame(FFrames[K - 1]).FLeft * 4);
            for X := 0 to ClearWidth - 1 do
            begin
              Q^[X].B := BgColor.B;
              Q^[X].G := BgColor.G;
              Q^[X].R := BgColor.R;
              Q^[X].A := 255;
            end;
          end;
        end;
      end
      else if TCnGIFFrame(FFrames[K - 1]).FDisposal = GIF_DISPOSAL_PREV then
      begin
        // 恢复保存的区域
        if SavedArea <> nil then
        begin
          for Y := 0 to SavedH - 1 do
          begin
            if (TCnGIFFrame(FFrames[K - 1]).FTop + Y >= FLogicalScreenHeight) then Break;
            if TCnGIFFrame(FFrames[K - 1]).FLeft < FLogicalScreenWidth then
            begin
              ClearWidth := SavedW;
              if TCnGIFFrame(FFrames[K - 1]).FLeft + ClearWidth > FLogicalScreenWidth then
                ClearWidth := FLogicalScreenWidth - TCnGIFFrame(FFrames[K - 1]).FLeft;
              Move(SavedArea^[Y * SavedW * 4],
                   FCompositeBuf^[(TCnGIFFrame(FFrames[K - 1]).FTop + Y) * BufW * 4 +
                     TCnGIFFrame(FFrames[K - 1]).FLeft * 4],
                   ClearWidth * 4);
            end;
          end;
        end;
      end;
    end;

    // 为 gdPrevious 保存当前帧区域
    if Frame.FDisposal = GIF_DISPOSAL_PREV then
    begin
      SavedW := Frame.FWidth;
      SavedH := Frame.FHeight;
      SavedSize := GIFQuadBufferSize(SavedW, SavedH);
      if SavedArea <> nil then FreeMem(SavedArea);
      GetMem(SavedArea, SavedSize);
      for Y := 0 to SavedH - 1 do
      begin
        if (Frame.FTop + Y >= FLogicalScreenHeight) then Break;
        // 简化：保存当前帧区域到 SavedArea
        if (Frame.FTop + Y < FLogicalScreenHeight) and
           (Frame.FLeft < FLogicalScreenWidth) then
        begin
          ClearWidth := SavedW;
          if Frame.FLeft + ClearWidth > FLogicalScreenWidth then
            ClearWidth := FLogicalScreenWidth - Frame.FLeft;
          Move(FCompositeBuf^[(Frame.FTop + Y) * BufW * 4 + Frame.FLeft * 4],
               SavedArea^[Y * SavedW * 4], ClearWidth * 4);
        end;
      end;
    end;

    // 绘制当前帧
    for Y := 0 to Frame.FHeight - 1 do
    begin
      if (Frame.FTop + Y) >= FLogicalScreenHeight then Break;
      Q := Pointer(TCnNativeInt(FCompositeBuf) + (Frame.FTop + Y) * BufW * 4 + Frame.FLeft * 4);
      Pix := @Frame.FPixels[Y * Frame.FWidth];

      for X := 0 to Frame.FWidth - 1 do
      begin
        if (Frame.FLeft + X) >= FLogicalScreenWidth then Break;

        if (Frame.FTransparentIndex < 0) or
           (Integer(Pix[X]) <> Frame.FTransparentIndex) then
        begin
          I := Integer(Pix[X]);
          if I >= PalLen then
            I := 0;
          if I < PalLen then
          begin
            Q^[X].B := Pal[I].B;
            Q^[X].G := Pal[I].G;
            Q^[X].R := Pal[I].R;
            Q^[X].A := 255;
          end;
        end;
      end;
    end;
  end;

  if SavedArea <> nil then
    FreeMem(SavedArea);
end;

//==============================================================================
// 渲染
//==============================================================================

function TCnGIFImage.AdvanceCompositeFrame(FrameIdx: Integer): Boolean;
var
  PrevFrame, Frame: TCnGIFFrame;
  Pal: TCnGIFColors;
  PalLen: Integer;
  Q: PQuadArray;
  Pix: PByteArray;
  X, Y, I, ClearWidth: Integer;
  BgColor: TCnGIFColor;
begin
  Result := False;
  if (FrameIdx <= 0) or (FRenderedFrame < 0) or
     (FrameIdx <> FRenderedFrame + 1) or
     (FrameIdx >= FFrames.Count) or
     (FCompositeBuf = nil) or
     (FCompWidth < FLogicalScreenWidth) or
     (FCompHeight < FLogicalScreenHeight) then
    Exit;

  PrevFrame := TCnGIFFrame(FFrames[FrameIdx - 1]);
  // gdPrevious requires a snapshot of the composited area.  The full
  // compositor already maintains that state, so use it for this uncommon
  // disposal mode instead of attempting an incomplete incremental restore.
  if PrevFrame.FDisposal = GIF_DISPOSAL_PREV then
    Exit;

  BgColor.R := 0;
  BgColor.G := 0;
  BgColor.B := 0;
  if FHasGlobalPalette and (FBackgroundColorIndex < Length(FGlobalPalette)) then
    BgColor := FGlobalPalette[FBackgroundColorIndex];

  if PrevFrame.FDisposal = GIF_DISPOSAL_BG then
  begin
    if PrevFrame.FLeft < FLogicalScreenWidth then
    begin
      ClearWidth := PrevFrame.FWidth;
      if PrevFrame.FLeft + ClearWidth > FLogicalScreenWidth then
        ClearWidth := FLogicalScreenWidth - PrevFrame.FLeft;
      for Y := PrevFrame.FTop to PrevFrame.FTop + PrevFrame.FHeight - 1 do
      begin
        if Y >= FLogicalScreenHeight then
          Break;
        Q := Pointer(TCnNativeInt(FCompositeBuf) +
          Y * FCompWidth * 4 + PrevFrame.FLeft * 4);
        for X := 0 to ClearWidth - 1 do
        begin
          Q^[X].B := BgColor.B;
          Q^[X].G := BgColor.G;
          Q^[X].R := BgColor.R;
          Q^[X].A := 255;
        end;
      end;
    end;
  end;

  Frame := TCnGIFFrame(FFrames[FrameIdx]);
  if Frame.FHasLocalPalette then
  begin
    Pal := Frame.FLocalPalette;
    PalLen := Length(Pal);
  end
  else
  begin
    Pal := FGlobalPalette;
    PalLen := Length(Pal);
  end;
  if (PalLen = 0) or (Frame.FLeft >= FLogicalScreenWidth) or
     (Frame.FTop >= FLogicalScreenHeight) then
    begin
      Result := True;
      Exit;
    end;

  for Y := 0 to Frame.FHeight - 1 do
  begin
    if Frame.FTop + Y >= FLogicalScreenHeight then
      Break;
    Q := Pointer(TCnNativeInt(FCompositeBuf) +
      (Frame.FTop + Y) * FCompWidth * 4 + Frame.FLeft * 4);
    Pix := @Frame.FPixels[Y * Frame.FWidth];
    for X := 0 to Frame.FWidth - 1 do
    begin
      if Frame.FLeft + X >= FLogicalScreenWidth then
        Break;
      if (Frame.FTransparentIndex < 0) or
         (Integer(Pix[X]) <> Frame.FTransparentIndex) then
      begin
        I := Integer(Pix[X]);
        if I >= PalLen then
          I := 0;
        Q^[X].B := Pal[I].B;
        Q^[X].G := Pal[I].G;
        Q^[X].R := Pal[I].R;
        Q^[X].A := 255;
      end;
    end;
  end;
  Result := True;
end;

procedure TCnGIFImage.EnsureRendered(FrameIdx: Integer);
var
  BufSize: Integer;
begin
  if (FrameIdx < 0) or (FrameIdx >= FFrames.Count) then
    Exit;
  if (FRenderedFrame = FrameIdx) and (FDIB <> 0) then
    Exit;

  if not AdvanceCompositeFrame(FrameIdx) then
    CompositeFrames(FrameIdx);

  if (FCompositeBuf = nil) or (FCompWidth <= 0) or (FCompHeight <= 0) then
    Exit;

  EnsureDIB(FCompWidth, FCompHeight);
  if (FDIB = 0) or (FDIBBits = nil) then Exit;

  BufSize := GIFQuadBufferSize(FCompWidth, FCompHeight);
  Move(FCompositeBuf^, FDIBBits^, BufSize);

  FRenderedFrame := FrameIdx;
end;

//==============================================================================
// TGraphic 覆盖
//==============================================================================

function TCnGIFImage.GetEmpty: Boolean;
begin
  Result := FFrames.Count = 0;
end;

function TCnGIFImage.GetHeight: Integer;
begin
  Result := FLogicalScreenHeight;
end;

function TCnGIFImage.GetWidth: Integer;
begin
  Result := FLogicalScreenWidth;
end;

function TCnGIFImage.GetTransparent: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FFrames.Count - 1 do
    if TCnGIFFrame(FFrames[I]).FTransparentIndex >= 0 then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TCnGIFImage.SetTransparent(Value: Boolean);
begin
  // GIF 透明性是固有属性
end;

procedure TCnGIFImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  MemDC: HDC;
  OldBmp: HBITMAP;
begin
  if GetEmpty then
    Exit;
  EnsureRendered(FCurrentFrame);
  if FDIB = 0 then
    Exit;

  MemDC := CreateCompatibleDC(ACanvas.Handle);
  try
    OldBmp := SelectObject(MemDC, FDIB);
    StretchBlt(ACanvas.Handle, Rect.Left, Rect.Top,
      Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
      MemDC, 0, 0, FCompWidth, FCompHeight, SRCCOPY);
    SelectObject(MemDC, OldBmp);
  finally
    DeleteDC(MemDC);
  end;
end;

procedure TCnGIFImage.SetHeight(Value: Integer);
begin

end;

procedure TCnGIFImage.SetWidth(Value: Integer);
begin

end;

procedure TCnGIFImage.AssignTo(Dest: TPersistent);
var
  Stm: TMemoryStream;
  MemDC: HDC;
  OldBmp: HBITMAP;
begin
  if Dest is TCnGIFImage then
  begin
    // 通过流复制
    Stm := TMemoryStream.Create;
    try
      SaveToStream(Stm);
      Stm.Position := 0;
      TCnGIFImage(Dest).LoadFromStream(Stm);
    finally
      Stm.Free;
    end;
  end
  else if Dest is TBitmap then
  begin
    if GetEmpty then
    begin
      TBitmap(Dest).Width := 0;
      TBitmap(Dest).Height := 0;
    end
    else
    begin
      EnsureRendered(FCurrentFrame);
      if FCompWidth > 0 then
      begin
        TBitmap(Dest).HandleType := bmDIB;
        TBitmap(Dest).PixelFormat := pf32Bit;
        TBitmap(Dest).Width := FCompWidth;
        TBitmap(Dest).Height := FCompHeight;

        MemDC := CreateCompatibleDC(0);
        try
          OldBmp := SelectObject(MemDC, FDIB);
          StretchBlt(TBitmap(Dest).Canvas.Handle, 0, 0,
            FCompWidth, FCompHeight,
            MemDC, 0, 0, FCompWidth, FCompHeight, SRCCOPY);
          SelectObject(MemDC, OldBmp);
        finally
          DeleteDC(MemDC);
        end;

        TBitmap(Dest).Transparent := GetTransparent;
      end;
    end;
  end
  else
    inherited;
end;

procedure TCnGIFImage.SaveToStream(Stream: TStream);
var
  I, J: Integer;
  Frame: TCnGIFFrame;
  MinCodeSize: Byte;
  Pkd: Byte;
  PalSz: Integer;
  SrcStm: TMemoryStream;
  InterBuf: PByteArray;
  PixelCount: Integer;
begin
  if FFrames.Count = 0 then
    Exit;

  // Header
  Stream.Write(GIF89a, 6);

  // Logical Screen Descriptor
  WriteWord(Stream, FLogicalScreenWidth);
  WriteWord(Stream, FLogicalScreenHeight);

  Pkd := 0;
  if FHasGlobalPalette then
  begin
    Pkd := Pkd or $80;
    Pkd := Pkd or ((FColorResolution and $07) shl 4);
    if FSortFlag then Pkd := Pkd or $08;
    PalSz := GIFPaletteTableSize(Length(FGlobalPalette));
    J := 0;
    while (1 shl (J + 1)) < PalSz do Inc(J);
    Pkd := Pkd or (J and $07);
  end;
  WriteByte(Stream, Pkd);
  WriteByte(Stream, FBackgroundColorIndex);
  WriteByte(Stream, FPixelAspectRatio);

  // Global Color Table
  if FHasGlobalPalette then
    WriteColorTable(Stream, FGlobalPalette, PalSz);

  // NETSCAPE 2.0 Application Extension (循环播放)
  if (FFrames.Count > 1) and FHasNetscape then
  begin
    WriteByte(Stream, GIF_EXT_INTRODUCER);
    WriteByte(Stream, GIF_EXT_APPLICATION);
    WriteByte(Stream, 11);
    Stream.Write(PAnsiChar('NETSCAPE2.0')^, 11);
    WriteByte(Stream, 3);
    WriteByte(Stream, 1);
    WriteWord(Stream, FLoopCount);
    WriteByte(Stream, 0);
  end;

  // ?
  for I := 0 to FFrames.Count - 1 do
  begin
    Frame := TCnGIFFrame(FFrames[I]);
    PixelCount := GIFPixelCount(Frame.FWidth, Frame.FHeight);

    // Graphic Control Extension (需要时)
    if (Frame.FDelay > 0) or (Frame.FTransparentIndex >= 0) or
       (Frame.FDisposal > 0) or Frame.FUserInput then
    begin
      WriteByte(Stream, GIF_EXT_INTRODUCER);
      WriteByte(Stream, GIF_EXT_GRAPHIC_CTRL);
      WriteByte(Stream, 4);  // 块大小
      Pkd := (Frame.FDisposal and $07) shl 2;
      if Frame.FUserInput then
        Pkd := Pkd or $02;
      if Frame.FTransparentIndex >= 0 then
        Pkd := Pkd or $01;
      WriteByte(Stream, Pkd);
      WriteWord(Stream, Frame.FDelay);
      if Frame.FTransparentIndex >= 0 then
        WriteByte(Stream, Frame.FTransparentIndex)
      else
        WriteByte(Stream, 0);
      WriteByte(Stream, 0);  // 终结符
    end;

    // Image Descriptor
    WriteByte(Stream, GIF_IMAGE_DESCRIPTOR);
    WriteWord(Stream, Frame.FLeft);
    WriteWord(Stream, Frame.FTop);
    WriteWord(Stream, Frame.FWidth);
    WriteWord(Stream, Frame.FHeight);
    Pkd := 0;
    if Frame.FInterlaced then Pkd := Pkd or $40;
    if Frame.FHasLocalPalette then
    begin
      Pkd := Pkd or $80;
      PalSz := GIFPaletteTableSize(Length(Frame.FLocalPalette));
      J := 0;
      while (1 shl (J + 1)) < PalSz do Inc(J);
      Pkd := Pkd or (J and $07);
    end;
    WriteByte(Stream, Pkd);

    // Local Color Table
    if Frame.FHasLocalPalette then
      WriteColorTable(Stream, Frame.FLocalPalette, PalSz);

    // LZW 最小码长
    if Frame.FHasLocalPalette then
      PalSz := GIFPaletteTableSize(Length(Frame.FLocalPalette))
    else if FHasGlobalPalette then
      PalSz := GIFPaletteTableSize(Length(FGlobalPalette))
    else
      PalSz := 256;

    if PalSz <= 2     then MinCodeSize := 2
    else if PalSz <= 4  then MinCodeSize := 3
    else if PalSz <= 8  then MinCodeSize := 4
    else if PalSz <= 16 then MinCodeSize := 5
    else if PalSz <= 32 then MinCodeSize := 6
    else if PalSz <= 64 then MinCodeSize := 7
    else                    MinCodeSize := 8;

    WriteByte(Stream, MinCodeSize);

    // 始终从当前像素重新编码，保证描述符、隔行标志、色表和码长一致。
    SrcStm := TMemoryStream.Create;
    try
      if Frame.FInterlaced then
      begin
        GetMem(InterBuf, PixelCount);
        try
          InterlacePixels(Frame, InterBuf);
          EncodeLZW(InterBuf, PixelCount,
                    SrcStm, MinCodeSize);
        finally
          FreeMem(InterBuf);
        end;
      end
      else
        EncodeLZW(Frame.FPixels, PixelCount,
                  SrcStm, MinCodeSize);
      EmitSubBlocks(Stream, SrcStm.Memory, SrcStm.Size);
    finally
      SrcStm.Free;
    end;
  end;

  // Trailer
  WriteByte(Stream, GIF_TRAILER);
end;

procedure TCnGIFImage.WriteColorTable(Stream: TStream;
  const Palette: TCnGIFColors; Count: Integer);
var
  I: Integer;
  Black: TCnGIFColor;
begin
  if (Count < 0) or (Count > 256) then
    raise Exception.Create(SCnGIFInvalidPaletteSize);
  Black.R := 0;
  Black.G := 0;
  Black.B := 0;
  for I := 0 to Count - 1 do
    if I < Length(Palette) then
      Stream.Write(Palette[I], 3)
    else
      Stream.Write(Black, 3);
end;

procedure TCnGIFImage.EmitSubBlocks(Stream: TStream; Data: Pointer;
  Size: Integer);
var
  P: PByte;
  Remain: Integer;
  Chunk: Byte;
begin
  P := Data;
  Remain := Size;
  while Remain > 0 do
  begin
    if Remain > 255 then Chunk := 255 else Chunk := Remain;
    WriteByte(Stream, Chunk);
    Stream.Write(P^, Chunk);
    Inc(P, Chunk);
    Dec(Remain, Chunk);
  end;
  WriteByte(Stream, 0);
end;

//==============================================================================
// LZW 编码
//==============================================================================

type
  THashEnt = packed record
    Used: Boolean;
    Prefix: Word;
    Suffix: Byte;
    Code: Word;
  end;

  PHashTab = ^THashTab;
  THashTab = array[0..GIF_MAX_CODES - 1] of THashEnt;

procedure TCnGIFImage.EncodeLZW(InData: PByteArray; InSize: Integer;
  OutStm: TStream; MinCodeSize: Integer);
var
  HT: THashTab;
  ClearCode: Integer;
  EOICode: Integer;
  CodeSize: Integer;
  CodeMask: Integer;
  NextCode: Integer;
  InPos: Integer;
  CurPrefix: Integer;
  CurSuffix: Integer;
  HashIdx: Integer;
  BitBuf: Cardinal;
  BitCnt: Integer;

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
      WriteByte(OutStm, BitBuf and $FF);
      BitBuf := BitBuf shr 8;
      Dec(BitCnt, 8);
    end;
  end;

  procedure FlushBits;
  begin
    while BitCnt > 0 do
    begin
      WriteByte(OutStm, BitBuf and $FF);
      BitBuf := BitBuf shr 8;
      Dec(BitCnt, 8);
    end;
  end;

  procedure InitTable;
  begin
    FillChar(HT, SizeOf(HT), 0);
  end;

begin
  GIFValidateLZWMinCodeSize(MinCodeSize);
  if InSize <= 0 then
    Exit;

  ClearCode := 1 shl MinCodeSize;
  EOICode   := ClearCode + 1;
  CodeSize  := MinCodeSize + 1;
  CodeMask  := (1 shl CodeSize) - 1;
  NextCode  := ClearCode + 2;
  BitBuf    := 0;
  BitCnt    := 0;

  InitTable;

  WriteCode(ClearCode);

  CurPrefix := InData[0];
  InPos := 1;

  while InPos < InSize do
  begin
    CurSuffix := InData[InPos];
    HashIdx := FindHash(CurPrefix, CurSuffix);

    if HT[HashIdx].Used and
       (HT[HashIdx].Prefix = CurPrefix) and
       (HT[HashIdx].Suffix = CurSuffix) then
    begin
      CurPrefix := HT[HashIdx].Code;
    end
    else
    begin
      WriteCode(CurPrefix);

      if NextCode < GIF_MAX_CODES then
      begin
        HT[HashIdx].Used   := True;
        HT[HashIdx].Prefix := CurPrefix;
        HT[HashIdx].Suffix := CurSuffix;
        HT[HashIdx].Code   := NextCode;
        Inc(NextCode);

        if NextCode > CodeMask then
        begin
          if CodeSize < 12 then
          begin
            // 解码器比编码器少一个字典表项（首个码不添加表项），
            // 因此编码器需延迟一个表项再增加码长，以保持编解码同步。
            if NextCode > CodeMask + 1 then
            begin
              Inc(CodeSize);
              CodeMask := (1 shl CodeSize) - 1;
            end;
          end
          else
          begin
            // 表满 -> ClearCode
            WriteCode(ClearCode);
            InitTable;
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

  // 输出最后一个前缀
  WriteCode(CurPrefix);
  WriteCode(EOICode);
  FlushBits;
end;

//==============================================================================
// 单帧 GIF 保存（位图 -> GIF）
//==============================================================================

procedure TCnGIFImage.QuantizeBitmap(Src: TBitmap; var Palette: TCnGIFColors;
  Indices: PByteArray);
const
  QHASH_SIZE = 1 shl 16;
  QHASH_MASK = (1 shl 16) - 1;
  QHASH_MAX_ITEMS = QHASH_SIZE div 2;
  QMAX = 256;
var
  W, H, X, Y, I, K: Integer;
  Row: PByteArray;
  Bv, Gv, Rv: Byte;
  Key, HIdx: Cardinal;
  Hash: array of TCnQuantHashEntry;
  UniqueCount: Integer;
  Colors: array of TCnQuantColor;
  Buckets: array of TCnQuantBucket;
  BestIdx, BestRange, Range: Integer;
  Mid, Chan: Integer;
  NewBkt: TCnQuantBucket;
  PalLen: Integer;
  TC: Cardinal;
  SB, SG, SR: Int64;
  BestDist, BestI, Dist: Integer;
  dB, dG, dR: Integer;
  PalCount: Integer;

  procedure ComputeBox(var Bkt: TCnQuantBucket);
  var
    MnB, MxB, MnG, MxG, MnR, MxR: Byte;
    II: Integer;
  begin
    if Bkt.Num <= 0 then
    begin
      Bkt.RngB := 0; Bkt.RngG := 0; Bkt.RngR := 0; Bkt.SplitChan := 0;
      Exit;
    end;
    MnB := 255; MxB := 0; MnG := 255; MxG := 0; MnR := 255; MxR := 0;
    for II := Bkt.StartIdx to Bkt.StartIdx + Bkt.Num - 1 do
    begin
      if Colors[II].B < MnB then MnB := Colors[II].B;
      if Colors[II].B > MxB then MxB := Colors[II].B;
      if Colors[II].G < MnG then MnG := Colors[II].G;
      if Colors[II].G > MxG then MxG := Colors[II].G;
      if Colors[II].R < MnR then MnR := Colors[II].R;
      if Colors[II].R > MxR then MxR := Colors[II].R;
    end;
    Bkt.RngB := MxB - MnB;
    Bkt.RngG := MxG - MnG;
    Bkt.RngR := MxR - MnR;
    if (Bkt.RngB >= Bkt.RngG) and (Bkt.RngB >= Bkt.RngR) then
      Bkt.SplitChan := 0
    else if Bkt.RngG >= Bkt.RngR then
      Bkt.SplitChan := 1
    else
      Bkt.SplitChan := 2;
  end;

  function ChanVal(const C: TCnQuantColor; Ch: Integer): Byte;
  begin
    case Ch of
      0: Result := C.B;
      1: Result := C.G;
    else
      Result := C.R;
    end;
  end;

  procedure QSort(Lo, Hi: Integer; Ch: Integer);
  var
    II, JJ: Integer;
    Pivot: Byte;
    Tmp: TCnQuantColor;
  begin
    if Lo >= Hi then Exit;
    Pivot := ChanVal(Colors[(Lo + Hi) div 2], Ch);
    II := Lo; JJ := Hi;
    while II <= JJ do
    begin
      while ChanVal(Colors[II], Ch) < Pivot do Inc(II);
      while ChanVal(Colors[JJ], Ch) > Pivot do Dec(JJ);
      if II <= JJ then
      begin
        Tmp := Colors[II]; Colors[II] := Colors[JJ]; Colors[JJ] := Tmp;
        Inc(II); Dec(JJ);
      end;
    end;
    if Lo < JJ then QSort(Lo, JJ, Ch);
    if II < Hi then QSort(II, Hi, Ch);
  end;

begin
  W := Src.Width;
  H := Src.Height;
  if (W <= 0) or (H <= 0) then
  begin
    SetLength(Palette, 0);
    Exit;
  end;

  // 收集唯一颜色及其出现次数
  SetLength(Hash, QHASH_SIZE);
  FillChar(Hash[0], SizeOf(TCnQuantHashEntry) * QHASH_SIZE, 0);
  UniqueCount := 0;

  for Y := 0 to H - 1 do
  begin
    Row := Src.ScanLine[Y];
    for X := 0 to W - 1 do
    begin
      Bv := Row^[X * 3];
      Gv := Row^[X * 3 + 1];
      Rv := Row^[X * 3 + 2];
      Key := (Cardinal(Bv) shl 16) or (Cardinal(Gv) shl 8) or Cardinal(Rv);
      HIdx := GIFColorHash(Key) and QHASH_MASK;
      while Hash[HIdx].Used do
      begin
        if (Hash[HIdx].B = Bv) and (Hash[HIdx].G = Gv) and (Hash[HIdx].R = Rv) then
        begin
          Inc(Hash[HIdx].Count);
          Break;
        end;
        HIdx := (HIdx + 1) and QHASH_MASK;
      end;
      if not Hash[HIdx].Used then
      begin
        // Keep open-addressing load at or below 50%.  The old near-full
        // table made high-color photographs spend most of their time in
        // linear probing; unrecorded colors are still mapped against the
        // resulting 256-color palette in the final pass.
        if UniqueCount < QHASH_MAX_ITEMS then
        begin
          Hash[HIdx].Used := True;
          Hash[HIdx].B := Bv;
          Hash[HIdx].G := Gv;
          Hash[HIdx].R := Rv;
          Hash[HIdx].Count := 1;
          Inc(UniqueCount);
        end;
      end;
    end;
  end;

  // 拷贝到线性颜色表
  SetLength(Colors, UniqueCount);
  K := 0;
  for I := 0 to QHASH_SIZE - 1 do
    if Hash[I].Used then
    begin
      Colors[K].B := Hash[I].B;
      Colors[K].G := Hash[I].G;
      Colors[K].R := Hash[I].R;
      Colors[K].Count := Hash[I].Count;
      Inc(K);
      if K >= UniqueCount then Break;
    end;

  if UniqueCount <= QMAX then
  begin
    // 唯一色不超过 256，直接作为调色板
    PalCount := UniqueCount;
    if PalCount < 2 then PalCount := 2;
    SetLength(Palette, PalCount);
    for I := 0 to UniqueCount - 1 do
    begin
      Palette[I].B := Colors[I].B;
      Palette[I].G := Colors[I].G;
      Palette[I].R := Colors[I].R;
    end;
    for I := UniqueCount to PalCount - 1 do
    begin
      Palette[I].B := 0;
      Palette[I].G := 0;
      Palette[I].R := 0;
    end;

    // 记录每个颜色在调色板中的索引
    for I := 0 to UniqueCount - 1 do
    begin
      Key := (Cardinal(Colors[I].B) shl 16) or (Cardinal(Colors[I].G) shl 8) or Cardinal(Colors[I].R);
      HIdx := GIFColorHash(Key) and QHASH_MASK;
      while Hash[HIdx].Used do
      begin
        if (Hash[HIdx].B = Colors[I].B) and (Hash[HIdx].G = Colors[I].G) and (Hash[HIdx].R = Colors[I].R) then
        begin
          Hash[HIdx].PalIdx := I;
          Break;
        end;
        HIdx := (HIdx + 1) and QHASH_MASK;
      end;
    end;

    // 生成索引图
    for Y := 0 to H - 1 do
    begin
      Row := Src.ScanLine[Y];
      for X := 0 to W - 1 do
      begin
        Bv := Row^[X * 3];
        Gv := Row^[X * 3 + 1];
        Rv := Row^[X * 3 + 2];
        Key := (Cardinal(Bv) shl 16) or (Cardinal(Gv) shl 8) or Cardinal(Rv);
        HIdx := GIFColorHash(Key) and QHASH_MASK;
        while Hash[HIdx].Used do
        begin
          if (Hash[HIdx].B = Bv) and (Hash[HIdx].G = Gv) and (Hash[HIdx].R = Rv) then
          begin
            Indices^[Y * W + X] := Hash[HIdx].PalIdx;
            Break;
          end;
          HIdx := (HIdx + 1) and QHASH_MASK;
        end;
        if not Hash[HIdx].Used then
          Indices^[Y * W + X] := 0;
      end;
    end;
  end
  else
  begin
    // 中位切分法量化到 256 色
    SetLength(Buckets, 1);
    Buckets[0].StartIdx := 0;
    Buckets[0].Num := UniqueCount;
    Buckets[0].TotalCount := 0;
    for I := 0 to UniqueCount - 1 do
      Inc(Buckets[0].TotalCount, Colors[I].Count);
    ComputeBox(Buckets[0]);

    while Length(Buckets) < QMAX do
    begin
      BestIdx := -1;
      BestRange := 0;
      for I := 0 to High(Buckets) do
      begin
        if Buckets[I].Num > 1 then
        begin
          Range := Buckets[I].RngB;
          if Buckets[I].RngG > Range then Range := Buckets[I].RngG;
          if Buckets[I].RngR > Range then Range := Buckets[I].RngR;
          if Range > BestRange then
          begin
            BestRange := Range;
            BestIdx := I;
          end;
        end;
      end;
      if BestIdx < 0 then Break;

      Chan := Buckets[BestIdx].SplitChan;
      QSort(Buckets[BestIdx].StartIdx,
            Buckets[BestIdx].StartIdx + Buckets[BestIdx].Num - 1, Chan);

      Mid := Buckets[BestIdx].StartIdx + Buckets[BestIdx].Num div 2;

      NewBkt.StartIdx := Mid;
      NewBkt.Num := Buckets[BestIdx].StartIdx + Buckets[BestIdx].Num - Mid;
      NewBkt.TotalCount := 0;
      for I := NewBkt.StartIdx to NewBkt.StartIdx + NewBkt.Num - 1 do
        Inc(NewBkt.TotalCount, Colors[I].Count);
      ComputeBox(NewBkt);

      Buckets[BestIdx].Num := Mid - Buckets[BestIdx].StartIdx;
      Buckets[BestIdx].TotalCount := 0;
      for I := Buckets[BestIdx].StartIdx to Buckets[BestIdx].StartIdx + Buckets[BestIdx].Num - 1 do
        Inc(Buckets[BestIdx].TotalCount, Colors[I].Count);
      ComputeBox(Buckets[BestIdx]);

      SetLength(Buckets, Length(Buckets) + 1);
      Buckets[High(Buckets)] := NewBkt;
    end;

    // 每个桶取加权平均色作为调色板项
    SetLength(Palette, Length(Buckets));
    for I := 0 to High(Buckets) do
    begin
      TC := 0;
      SB := 0; SG := 0; SR := 0;
      for K := Buckets[I].StartIdx to Buckets[I].StartIdx + Buckets[I].Num - 1 do
      begin
        Inc(SB, Int64(Colors[K].B) * Colors[K].Count);
        Inc(SG, Int64(Colors[K].G) * Colors[K].Count);
        Inc(SR, Int64(Colors[K].R) * Colors[K].Count);
        Inc(TC, Colors[K].Count);
      end;
      if TC > 0 then
      begin
        Palette[I].B := SB div TC;
        Palette[I].G := SG div TC;
        Palette[I].R := SR div TC;
      end
      else
      begin
        Palette[I].B := 0;
        Palette[I].G := 0;
        Palette[I].R := 0;
      end;
    end;

    // 最近邻映射生成索引图
    PalLen := Length(Palette);
    for Y := 0 to H - 1 do
    begin
      Row := Src.ScanLine[Y];
      for X := 0 to W - 1 do
      begin
        Bv := Row^[X * 3];
        Gv := Row^[X * 3 + 1];
        Rv := Row^[X * 3 + 2];
        BestDist := MaxInt;
        BestI := 0;
        for I := 0 to PalLen - 1 do
        begin
          dB := Bv - Palette[I].B;
          dG := Gv - Palette[I].G;
          dR := Rv - Palette[I].R;
          Dist := dB * dB + dG * dG + dR * dR;
          if Dist < BestDist then
          begin
            BestDist := Dist;
            BestI := I;
            if BestDist = 0 then Break;
          end;
        end;
        Indices^[Y * W + X] := BestI;
      end;
    end;
  end;
end;

procedure TCnGIFImage.QuantizeComposite(var Palette: TCnGIFColors;
  Indices: PByteArray);
const
  QHASH_SIZE = 1 shl 16;
  QHASH_MASK = (1 shl 16) - 1;
var
  W, H, X, Y, I: Integer;
  Q: PQuadArray;
  Bv, Gv, Rv: Byte;
  Key, HIdx: Cardinal;
  Hash: array of TCnQuantHashEntry;
  UniqueCount, PalCount: Integer;
  Found: Boolean;
  BestI, BestDist, Dist, dB, dG, dR: Integer;
begin
  W := FCompWidth;
  H := FCompHeight;
  if (W <= 0) or (H <= 0) or (FCompositeBuf = nil) then
  begin
    SetLength(Palette, 0);
    Exit;
  end;

  SetLength(Hash, QHASH_SIZE);
  FillChar(Hash[0], SizeOf(TCnQuantHashEntry) * QHASH_SIZE, 0);
  UniqueCount := 0;

  // 第一遍：收集唯一色并分配调色板索引
  for Y := 0 to H - 1 do
  begin
    Q := Pointer(TCnNativeInt(FCompositeBuf) + Y * W * 4);
    for X := 0 to W - 1 do
    begin
      Bv := Q^[X].B;
      Gv := Q^[X].G;
      Rv := Q^[X].R;
      Key := (Cardinal(Bv) shl 16) or (Cardinal(Gv) shl 8) or Cardinal(Rv);
      HIdx := GIFColorHash(Key) and QHASH_MASK;
      while Hash[HIdx].Used do
      begin
        if (Hash[HIdx].B = Bv) and (Hash[HIdx].G = Gv) and (Hash[HIdx].R = Rv) then
          Break;
        HIdx := (HIdx + 1) and QHASH_MASK;
      end;
      if not Hash[HIdx].Used then
      begin
        Hash[HIdx].Used := True;
        Hash[HIdx].B := Bv;
        Hash[HIdx].G := Gv;
        Hash[HIdx].R := Rv;
        Hash[HIdx].Count := 1;
        Hash[HIdx].PalIdx := UniqueCount;
        Inc(UniqueCount);
        if UniqueCount >= 256 then Break;
      end
      else
        Inc(Hash[HIdx].Count);
    end;
    if UniqueCount >= 256 then Break;
  end;

  PalCount := UniqueCount;
  if PalCount < 2 then PalCount := 2;
  SetLength(Palette, PalCount);

  // 从哈希表填充调色板
  for I := 0 to QHASH_SIZE - 1 do
    if Hash[I].Used then
    begin
      Palette[Hash[I].PalIdx].B := Hash[I].B;
      Palette[Hash[I].PalIdx].G := Hash[I].G;
      Palette[Hash[I].PalIdx].R := Hash[I].R;
    end;
  for I := UniqueCount to PalCount - 1 do
  begin
    Palette[I].B := 0;
    Palette[I].G := 0;
    Palette[I].R := 0;
  end;

  // 第二遍：生成索引图
  for Y := 0 to H - 1 do
  begin
    Q := Pointer(TCnNativeInt(FCompositeBuf) + Y * W * 4);
    for X := 0 to W - 1 do
    begin
      Bv := Q^[X].B;
      Gv := Q^[X].G;
      Rv := Q^[X].R;
      Key := (Cardinal(Bv) shl 16) or (Cardinal(Gv) shl 8) or Cardinal(Rv);
      HIdx := GIFColorHash(Key) and QHASH_MASK;
      Found := False;
      while Hash[HIdx].Used do
      begin
        if (Hash[HIdx].B = Bv) and (Hash[HIdx].G = Gv) and (Hash[HIdx].R = Rv) then
        begin
          Found := True;
          Break;
        end;
        HIdx := (HIdx + 1) and QHASH_MASK;
      end;
      if Found then
        Indices^[Y * W + X] := Hash[HIdx].PalIdx
      else
      begin
        BestI := 0;
        BestDist := MaxInt;
        for I := 0 to PalCount - 1 do
        begin
          dB := Integer(Bv) - Palette[I].B;
          dG := Integer(Gv) - Palette[I].G;
          dR := Integer(Rv) - Palette[I].R;
          Dist := dB * dB + dG * dG + dR * dR;
          if Dist < BestDist then
          begin
            BestDist := Dist;
            BestI := I;
            if Dist = 0 then Break;
          end;
        end;
        Indices^[Y * W + X] := BestI;
      end;
    end;
  end;
end;

procedure TCnGIFImage.WriteSingleFrameGIF(Stream: TStream; W, H: Integer;
  const Palette: TCnGIFColors; Indices: PByteArray);
var
  Pkd: Byte;
  PalSz, P, I, MinCodeSize, PixelCount: Integer;
  EncStm: TMemoryStream;
begin
  PixelCount := GIFPixelCount(W, H);
  PalSz := GIFPaletteTableSize(Length(Palette));

  // 计算调色板尺寸（2 的幂，范围 2..256）
  P := 0;
  while (1 shl P) < PalSz do Inc(P);

  // Header
  Stream.Write(GIF89a, 6);

  // Logical Screen Descriptor
  WriteWord(Stream, W);
  WriteWord(Stream, H);
  Pkd := $80 or $70 or ((P - 1) and $07);  // 全局色表、色分辨率 7、无排序
  WriteByte(Stream, Pkd);
  WriteByte(Stream, 0);  // 背景色索引
  WriteByte(Stream, 0);  // 像素宽高比

  // Global Color Table（共 2^P 项，不足补 0）
  for I := 0 to (1 shl P) - 1 do
  begin
    if I < Length(Palette) then
      Stream.Write(Palette[I], 3)
    else
    begin
      WriteByte(Stream, 0);
      WriteByte(Stream, 0);
      WriteByte(Stream, 0);
    end;
  end;

  // Image Descriptor
  WriteByte(Stream, GIF_IMAGE_DESCRIPTOR);
  WriteWord(Stream, 0);  // Left
  WriteWord(Stream, 0);  // Top
  WriteWord(Stream, W);
  WriteWord(Stream, H);
  WriteByte(Stream, 0);  // 无局部色表、无隔行

  // LZW 最小码长
  PalSz := 1 shl P;
  if PalSz <= 2 then MinCodeSize := 2
  else if PalSz <= 4 then MinCodeSize := 3
  else if PalSz <= 8 then MinCodeSize := 4
  else if PalSz <= 16 then MinCodeSize := 5
  else if PalSz <= 32 then MinCodeSize := 6
  else if PalSz <= 64 then MinCodeSize := 7
  else MinCodeSize := 8;

  WriteByte(Stream, MinCodeSize);

  // LZW 编码并以子块写入
  EncStm := TMemoryStream.Create;
  try
    EncodeLZW(Indices, PixelCount, EncStm, MinCodeSize);
    EmitSubBlocks(Stream, EncStm.Memory, EncStm.Size);
  finally
    EncStm.Free;
  end;

  // Trailer
  WriteByte(Stream, GIF_TRAILER);
end;

procedure TCnGIFImage.SaveBitmapToGIFStream(Stream: TStream; Src: TBitmap);
var
  Bmp: TBitmap;
  W, H: Integer;
  Palette: TCnGIFColors;
  IdxBuf: PByteArray;
  PixelCount: Integer;
begin
  if (Src = nil) or Src.Empty then
    Exit;

  Bmp := TBitmap.Create;
  try
  {$IFDEF FPC}
    // LCL: HandleType/PixelFormat setters recreate the bitmap handle and
    // discard existing pixel data, so we must set format BEFORE copying.
    Bmp.PixelFormat := pf24bit;
    Bmp.HandleType := bmDIB;
    W := Src.Width;
    H := Src.Height;
    Bmp.Width := W;
    Bmp.Height := H;
    Bmp.Canvas.Draw(0, 0, Src);
  {$ELSE}
    Bmp.Assign(Src);
    Bmp.HandleType := bmDIB;
    Bmp.PixelFormat := pf24bit;
    W := Bmp.Width;
    H := Bmp.Height;
  {$ENDIF}
    if (W <= 0) or (H <= 0) then
      Exit;

    PixelCount := GIFPixelCount(W, H);
    GetMem(IdxBuf, PixelCount);
    try
      QuantizeBitmap(Bmp, Palette, IdxBuf);
      if Length(Palette) = 0 then
        Exit;
      WriteSingleFrameGIF(Stream, W, H, Palette, IdxBuf);
    finally
      FreeMem(IdxBuf);
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TCnGIFImage.SaveBitmapToGIFFile(const FileName: string; Src: TBitmap);
var
  Stm: TFileStream;
begin
  Stm := TFileStream.Create(FileName, fmCreate);
  try
    SaveBitmapToGIFStream(Stm, Src);
  finally
    Stm.Free;
  end;
end;

procedure TCnGIFImage.SaveCurrentFrameToGIFStream(Stream: TStream);
var
  Frame: TCnGIFFrame;
  W, H, PalSz, P, I, MinCodeSize: Integer;
  Palette: TCnGIFColors;
  Pkd: Byte;
  EncStm: TMemoryStream;
  PixelCount: Integer;
begin
  if GetEmpty then
    Exit;
  if (FCurrentFrame < 0) or (FCurrentFrame >= FFrames.Count) then
    Exit;

  Frame := TCnGIFFrame(FFrames[FCurrentFrame]);
  W := Frame.FWidth;
  H := Frame.FHeight;
  if (W <= 0) or (H <= 0) then
    Exit;
  PixelCount := GIFPixelCount(W, H);

  // 使用帧的原始调色板（局部优先，否则全局）
  if Frame.FHasLocalPalette then
    Palette := Frame.FLocalPalette
  else
    Palette := FGlobalPalette;
  PalSz := Length(Palette);
  if PalSz = 0 then
    Exit;

  // 计算调色板尺寸（2 的幂，范围 2..256）
  PalSz := GIFPaletteTableSize(PalSz);
  P := 0;
  while (1 shl P) < PalSz do
    Inc(P);

  // Header
  Stream.Write(GIF89a, 6);

  // Logical Screen Descriptor（用帧尺寸作为逻辑屏幕尺寸）
  WriteWord(Stream, W);
  WriteWord(Stream, H);
  Pkd := $80 or $70 or ((P - 1) and $07);
  WriteByte(Stream, Pkd);
  WriteByte(Stream, 0);  // 背景色索引
  WriteByte(Stream, 0);  // 像素宽高比

  // Global Color Table
  for I := 0 to (1 shl P) - 1 do
  begin
    if I < Length(Palette) then
      Stream.Write(Palette[I], 3)
    else
    begin
      WriteByte(Stream, 0);
      WriteByte(Stream, 0);
      WriteByte(Stream, 0);
    end;
  end;

  // Graphic Control Extension（如有透明色或延迟）
  if (Frame.FTransparentIndex >= 0) or (Frame.FDelay > 0) or
     Frame.FUserInput then
  begin
    WriteByte(Stream, GIF_EXT_INTRODUCER);
    WriteByte(Stream, GIF_EXT_GRAPHIC_CTRL);
    WriteByte(Stream, 4);
    Pkd := (Frame.FDisposal and $07) shl 2;
    if Frame.FUserInput then
      Pkd := Pkd or $02;
    if Frame.FTransparentIndex >= 0 then
      Pkd := Pkd or $01;
    WriteByte(Stream, Pkd);
    WriteWord(Stream, Frame.FDelay);
    if Frame.FTransparentIndex >= 0 then
      WriteByte(Stream, Frame.FTransparentIndex)
    else
      WriteByte(Stream, 0);
    WriteByte(Stream, 0);
  end;

  // Image Descriptor
  WriteByte(Stream, GIF_IMAGE_DESCRIPTOR);
  WriteWord(Stream, 0);
  WriteWord(Stream, 0);
  WriteWord(Stream, W);
  WriteWord(Stream, H);
  WriteByte(Stream, 0);  // 无局部色表、无隔行

  // LZW 最小码长
  PalSz := 1 shl P;
  if PalSz <= 2 then MinCodeSize := 2
  else if PalSz <= 4 then MinCodeSize := 3
  else if PalSz <= 8 then MinCodeSize := 4
  else if PalSz <= 16 then MinCodeSize := 5
  else if PalSz <= 32 then MinCodeSize := 6
  else if PalSz <= 64 then MinCodeSize := 7
  else MinCodeSize := 8;

  WriteByte(Stream, MinCodeSize);

  // 单帧描述符为非隔行，必须按当前线性像素重新编码。
  EncStm := TMemoryStream.Create;
  try
    EncodeLZW(Frame.FPixels, PixelCount, EncStm, MinCodeSize);
    EmitSubBlocks(Stream, EncStm.Memory, EncStm.Size);
  finally
    EncStm.Free;
  end;

  // Trailer
  WriteByte(Stream, GIF_TRAILER);
end;

procedure TCnGIFImage.SaveCurrentFrameToGIFFile(const FileName: string);
var
  Stm: TFileStream;
begin
  Stm := TFileStream.Create(FileName, fmCreate);
  try
    SaveCurrentFrameToGIFStream(Stm);
  finally
    Stm.Free;
  end;
end;

procedure TCnGIFImage.SaveCompositedFrameToGIFStream(Stream: TStream);
var
  W, H: Integer;
  Palette: TCnGIFColors;
  IdxBuf: PByteArray;
  PixelCount: Integer;
begin
  if GetEmpty then
    Exit;
  if (FCurrentFrame < 0) or (FCurrentFrame >= FFrames.Count) then
    Exit;

  // Composite all frames up to current frame into FCompositeBuf
  EnsureRendered(FCurrentFrame);
  if (FCompositeBuf = nil) or (FCompWidth <= 0) or (FCompHeight <= 0) then
    Exit;

  W := FCompWidth;
  H := FCompHeight;
  PixelCount := GIFPixelCount(W, H);

  GetMem(IdxBuf, PixelCount);
  try
    QuantizeComposite(Palette, IdxBuf);
    if Length(Palette) = 0 then
      Exit;
    WriteSingleFrameGIF(Stream, W, H, Palette, IdxBuf);
  finally
    FreeMem(IdxBuf);
  end;
end;

procedure TCnGIFImage.SaveCompositedFrameToGIFFile(const FileName: string);
var
  Stm: TFileStream;
begin
  Stm := TFileStream.Create(FileName, fmCreate);
  try
    SaveCompositedFrameToGIFStream(Stm);
  finally
    Stm.Free;
  end;
end;

procedure TCnGIFImage.SetCurrentFrame(Value: Integer);
begin
  if (Value >= 0) and (Value < FFrames.Count) and (Value <> FCurrentFrame) then
  begin
    FCurrentFrame := Value;
    if (FRenderedFrame < 0) or (Value <> FRenderedFrame + 1) then
      FRenderedFrame := -1;
  end;
end;

procedure TCnGIFImage.SetAnimationLoopCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > 65535 then
    Value := 65535;
  FLoopCount := Value;
  FHasNetscape := True;
end;

function TCnGIFImage.GetFrameCount: Integer;
begin
  Result := FFrames.Count;
end;

function TCnGIFImage.GetFrame(Index: Integer): TCnGIFFrame;
begin
  Result := TCnGIFFrame(FFrames[Index]);
end;

procedure TCnGIFImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromClipboardFormat(AFormat {$IFNDEF FPC}, AData, APalette {$ENDIF});
    Assign(Bmp);
  finally
    Bmp.Free;
  end;
  Changed(Self);
end;

procedure TCnGIFImage.SaveToClipboardFormat(var Format: Word;
  var Data: THandle; var APalette: HPALETTE);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    AssignTo(Bmp);
    Bmp.SaveToClipboardFormat(Format {$IFNDEF FPC}, Data, APalette {$ENDIF});
  finally
    Bmp.Free;
  end;
end;

procedure RegisterCnGIF;
begin
  TPicture.RegisterFileFormat('gif', 'GIF Image', TCnGIFImage);
end;

procedure UnregisterCnGIF;
begin
  TPicture.UnregisterGraphicClass(TCnGIFImage);
end;

{
initialization
  RegisterCnGIF;

finalization
  UnregisterCnGIF;
}

end.
