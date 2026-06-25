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

const
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
  PCnGIFColor = ^TCnGIFColor;
  TCnGIFColor = packed record
    R, G, B: Byte;
  end;

  TCnGIFColors = array of TCnGIFColor;

  TCnGIFDisposal = (gdUnspecified, gdLeave, gdBackground, gdPrevious);

  PCnGIFPixels = ^TCnGIFPixels;
  TCnGIFPixels = array[0..0] of Byte;

  PCnGIFQuad = ^TCnGIFQuad;
  TCnGIFQuad = packed record
    B, G, R, A: Byte;
  end;

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

    // 渲染缓存
    FCompositeBuf: PByteArray;
    FCompWidth: Integer;
    FCompHeight: Integer;
    FRenderedFrame: Integer;
    FDIB: HBITMAP;
    FDIBW: Integer;
    FDIBH: Integer;

    // 待定 GCE（GCE 在 Image Descriptor 之前）
    FPendingDelay: Word;
    FPendingDisposal: Byte;
    FPendingTransparent: Integer;
    FHasPendingGCE: Boolean;

    // 缓存管理
    procedure FreeComposite;
    procedure EnsureComposite(W, H: Integer);
    procedure FreeDIB;
    procedure EnsureDIB(W, H: Integer);
    procedure EnsureRendered(FrameIdx: Integer);

    // 流辅助
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

    // 合成
    procedure CompositeFrames(LastFrame: Integer);

    // 写入
    procedure WriteColorTable(Stream: TStream;
      const Palette: TCnGIFColors; Count: Integer);
    procedure EmitSubBlocks(Stream: TStream; Data: Pointer; Size: Integer);

    // LZW
    procedure DecodeLZW(InData: PByte; InSize: Integer; OutStm: TStream;
      MinCodeSize: Integer; PixelCount: Integer);
    procedure EncodeLZW(InData: PByteArray; InSize: Integer; OutStm: TStream;
      MinCodeSize: Integer);

    // 帧访问
    procedure SetCurrentFrame(Value: Integer);
    function  GetFrameCount: Integer;
    function  GetFrame(Index: Integer): TCnGIFFrame;

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
    procedure Clear;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    property FrameCount: Integer read GetFrameCount;
    property Frames[Index: Integer]: TCnGIFFrame read GetFrame;
    property AnimationLoopCount: Integer read FLoopCount;
  end;

// 注册
procedure RegisterCnGIF;

// 注销
procedure UnregisterCnGIF;

implementation

const
  GIF87a: array[0..5] of AnsiChar = 'GIF87a';
  GIF89a: array[0..5] of AnsiChar = 'GIF89a';

type
  TDecEntry = packed record
    Prefix: Word;
    Suffix: Byte;
  end;

{$R-}

//==============================================================================
// TCnGIFFrame
//==============================================================================

constructor TCnGIFFrame.Create;
begin
  inherited;
  FTransparentIndex := -1;
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
  if FPixels <> nil then
    FreeMem(FPixels);
  FPixelCount := Count;
  if Count > 0 then
    GetMem(FPixels, Count)
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
  FCompositeBuf := nil;
  FCompWidth := 0;
  FCompHeight := 0;
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
  FHasPendingGCE := False;
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
  Sz := W * H * 4;
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
  FDIBW := 0;
  FDIBH := 0;
end;

procedure TCnGIFImage.EnsureDIB(W, H: Integer);
var
  BMI: TBitmapInfo;
  DC: HDC;
  Bits: Pointer;
begin
  if (FDIB <> 0) and (FDIBW >= W) and (FDIBH >= H) then
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
    FDIB := CreateDIBSection(DC, BMI, DIB_RGB_COLORS, Bits, 0, 0);
  finally
    ReleaseDC(0, DC);
  end;
  FDIBW := W;
  FDIBH := H;
end;

//==============================================================================
// 流辅助
//==============================================================================

function TCnGIFImage.ReadByte(Stream: TStream): Byte;
begin
  Stream.Read(Result, 1);
end;

function TCnGIFImage.ReadWord(Stream: TStream): Word;
begin
  Stream.Read(Result, 2);
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
    Stream.Read(Palette[I], 3);
end;

procedure TCnGIFImage.ReadSubBlocks(Stream: TStream; Data: TStream);
var
  Sz: Byte;
begin
  while True do
  begin
    Sz := ReadByte(Stream);
    if Sz = 0 then
      Break;
    Data.CopyFrom(Stream, Sz);
  end;
end;

procedure TCnGIFImage.SkipSubBlocks(Stream: TStream);
var
  Sz: Byte;
begin
  while True do
  begin
    Sz := ReadByte(Stream);
    if Sz = 0 then
      Break;
    Stream.Seek(Sz, soFromCurrent);
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
  B1, B2: Byte;
begin
  BlockSz := ReadByte(Stream);
  if BlockSz <> 11 then
  begin
    SkipSubBlocks(Stream);
    Exit;
  end;
  Stream.Read(AppId, 11);

  // NETSCAPE 2.0
  if (AppId[0] = 'N') and (AppId[1] = 'E') and (AppId[2] = 'T') and
     (AppId[3] = 'S') and (AppId[4] = 'C') and (AppId[5] = 'A') and
     (AppId[6] = 'P') and (AppId[7] = 'E') then
  begin
    SB := ReadByte(Stream);
    if SB = 3 then
    begin
      ReadByte(Stream); // sub-block ID (1)
      B1 := ReadByte(Stream);
      B2 := ReadByte(Stream);
      FLoopCount := B1 or (B2 shl 8);
      ReadByte(Stream); // terminators
    end
    else
      SkipSubBlocks(Stream);
  end
  else
    SkipSubBlocks(Stream);
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
begin
  Clear;

  // Header
  Stream.Read(FHeader, 6);
  if (FHeader <> GIF87a) and (FHeader <> GIF89a) then
    raise Exception.Create('Invalid GIF Signature');

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
            GIF_EXT_PLAIN_TEXT:   SkipSubBlocks(Stream);
          else
            SkipSubBlocks(Stream);
          end;
        end;

      GIF_IMAGE_DESCRIPTOR:
        begin
          Frame := TCnGIFFrame.Create;

          // 应用待定 GCE
          if FHasPendingGCE then
          begin
            Frame.FDelay := FPendingDelay;
            Frame.FDisposal := FPendingDisposal;
            Frame.FTransparentIndex := FPendingTransparent;
            FHasPendingGCE := False;
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

          // 读取 LZW 子块数据
          Frame.FRawData.Size := 0;
          ReadSubBlocks(Stream, Frame.FRawData);

  // LZW 解码
  Temp := TMemoryStream.Create;
  try
    DecodeLZW(Frame.FRawData.Memory, Frame.FRawData.Size,
              Temp, LZWCodeSize, Frame.FWidth * Frame.FHeight);

    Frame.AllocatePixels(Frame.FWidth * Frame.FHeight);
    if Temp.Size > 0 then
      if Temp.Size < Frame.FWidth * Frame.FHeight then
        Move(Temp.Memory^, Frame.FPixels^, Temp.Size)
      else
        Move(Temp.Memory^, Frame.FPixels^, Frame.FWidth * Frame.FHeight);
  finally
    Temp.Free;
  end;

          FFrames.Add(Frame);
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

  FillChar(Table, SizeOf(Table), 0);
  for I := 0 to 255 do
  begin
    Table[I].Prefix := 0;
    Table[I].Suffix := Byte(I);
  end;

  // 读第一个码
  Code := GetCode;
  if Code = EOICode then Exit;
  if Code = ClearCode then
    Code := GetCode;

  OldCode := Code;
  OutStm.Write(Table[Code].Suffix, 1);
  Inc(OutCnt);

  while OutCnt < PixelCount do
  begin
    Code := GetCode;

    if Code = ClearCode then
    begin
      CodeSize := MinCodeSize + 1;
      CodeMask := (1 shl CodeSize) - 1;
      NextCode := ClearCode + 2;
      OldCode  := -1;
      Continue;
    end;

    if Code = EOICode then
      Break;

    if Code = NextCode then
    begin
      // KWI 特例
      SP := 0;
      Stack[SP] := Table[OldCode].Suffix;  Inc(SP);
      I := OldCode;
      while I > 255 do
      begin
        Stack[SP] := Table[I].Suffix;  Inc(SP);
        I := Table[I].Prefix;
      end;
      Stack[SP] := Byte(I);  Inc(SP);
    end
    else
    begin
      SP := 0;
      I := Code;
      while I > 255 do
      begin
        Stack[SP] := Table[I].Suffix;  Inc(SP);
        I := Table[I].Prefix;
      end;
      Stack[SP] := Byte(I);  Inc(SP);
    end;

    // 输出栈
    while SP > 0 do
    begin
      Dec(SP);
      OutStm.Write(Stack[SP], 1);
      Inc(OutCnt);
      if OutCnt >= PixelCount then Break;
    end;
    if OutCnt >= PixelCount then Break;

    // 添加新条目到表
    if NextCode < GIF_MAX_CODES then
    begin
      if (Code = NextCode) and (OldCode >= 0) then
      begin
        Table[NextCode].Prefix := OldCode;
        Table[NextCode].Suffix := Table[OldCode].Suffix;
      end
      else if OldCode >= 0 then
      begin
        Table[NextCode].Prefix := OldCode;
        I := Code;
        while I > 255 do
          I := Table[I].Prefix;
        Table[NextCode].Suffix := Byte(I);
      end;
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
// 帧合成
//==============================================================================

type
  PQuadArray = ^TQuadArray;
  TQuadArray = array[0..0] of TCnGIFQuad;

procedure TCnGIFImage.CompositeFrames(LastFrame: Integer);
var
  K, X, Y: Integer;
  Frame: TCnGIFFrame;
  Pal: TCnGIFColors;
  PalLen: Integer;
  Q: PQuadArray;
  Pix: PByteArray;
  BufW: Integer;
  SavedArea: PByteArray;
  SavedW, SavedH: Integer;
begin
  if (FLogicalScreenWidth <= 0) or (FLogicalScreenHeight <= 0) then
    Exit;

  EnsureComposite(FLogicalScreenWidth, FLogicalScreenHeight);
  BufW := FCompWidth;

  // 清为背景色
  FillChar(FCompositeBuf^, BufW * FLogicalScreenHeight * 4, 0);
  if FHasGlobalPalette and (FBackgroundColorIndex < Length(FGlobalPalette)) then
  begin
    for Y := 0 to FLogicalScreenHeight - 1 do
    begin
      Q := Pointer(TCnNativeInt(FCompositeBuf) + Y * BufW * 4);
      for X := 0 to FLogicalScreenWidth - 1 do
      begin
        Q^[X].B := FGlobalPalette[FBackgroundColorIndex].B;
        Q^[X].G := FGlobalPalette[FBackgroundColorIndex].G;
        Q^[X].R := FGlobalPalette[FBackgroundColorIndex].R;
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
          FillChar(FCompositeBuf^[Y * BufW * 4 +
            TCnGIFFrame(FFrames[K - 1]).FLeft * 4],
            TCnGIFFrame(FFrames[K - 1]).FWidth * 4, 0);
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
            Move(SavedArea^[Y * SavedW * 4],
                 FCompositeBuf^[(TCnGIFFrame(FFrames[K - 1]).FTop + Y) * BufW * 4 +
                   TCnGIFFrame(FFrames[K - 1]).FLeft * 4],
                 SavedW * 4);
          end;
        end;
      end;
    end;

    // 为 gdPrevious 保存当前帧区域
    if Frame.FDisposal = GIF_DISPOSAL_PREV then
    begin
      SavedW := Frame.FWidth;
      SavedH := Frame.FHeight;
      if SavedArea <> nil then FreeMem(SavedArea);
      GetMem(SavedArea, SavedW * SavedH * 4);
      for Y := 0 to SavedH - 1 do
      begin
        if (Frame.FTop + Y >= FLogicalScreenHeight) then Break;
        // 简化：保存当前帧区域到 SavedArea
        if (Frame.FTop + Y < FLogicalScreenHeight) and
           (Frame.FLeft < FLogicalScreenWidth) then
          Move(FCompositeBuf^[(Frame.FTop + Y) * BufW * 4 + Frame.FLeft * 4],
               SavedArea^[Y * SavedW * 4], SavedW * 4);
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
          if Integer(Pix[X]) < PalLen then
          begin
            Q^[X].B := Pal[Pix[X]].B;
            Q^[X].G := Pal[Pix[X]].G;
            Q^[X].R := Pal[Pix[X]].R;
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

procedure TCnGIFImage.EnsureRendered(FrameIdx: Integer);
var
  BMI: TBitmapInfo;
  DC: HDC;
begin
  if (FrameIdx < 0) or (FrameIdx >= FFrames.Count) then
    Exit;
  if (FRenderedFrame = FrameIdx) and (FDIB <> 0) then
    Exit;

  CompositeFrames(FrameIdx);

  if (FCompositeBuf = nil) or (FCompWidth <= 0) or (FCompHeight <= 0) then
    Exit;

  EnsureDIB(FCompWidth, FCompHeight);
  if FDIB = 0 then Exit;

  FillChar(BMI, SizeOf(BMI), 0);
  BMI.bmiHeader.biSize := SizeOf(BMI.bmiHeader);
  BMI.bmiHeader.biWidth  := FCompWidth;
  BMI.bmiHeader.biHeight := -FCompHeight;
  BMI.bmiHeader.biPlanes := 1;
  BMI.bmiHeader.biBitCount := 32;
  BMI.bmiHeader.biCompression := BI_RGB;

  DC := GetDC(0);
  try
    SetDIBits(DC, FDIB, 0, FCompHeight, FCompositeBuf, BMI, DIB_RGB_COLORS);
  finally
    ReleaseDC(0, DC);
  end;

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
    PalSz := Length(FGlobalPalette);
    J := 7;
    while (1 shl (J + 1)) < PalSz do Dec(J);
    Pkd := Pkd or (J and $07);
  end;
  WriteByte(Stream, Pkd);
  WriteByte(Stream, FBackgroundColorIndex);
  WriteByte(Stream, FPixelAspectRatio);

  // Global Color Table
  if FHasGlobalPalette then
    WriteColorTable(Stream, FGlobalPalette, Length(FGlobalPalette));

  // 帧
  for I := 0 to FFrames.Count - 1 do
  begin
    Frame := TCnGIFFrame(FFrames[I]);

    // Graphic Control Extension (需要时)
    if (Frame.FDelay > 0) or (Frame.FTransparentIndex >= 0) or
       (Frame.FDisposal > 0) then
    begin
      WriteByte(Stream, GIF_EXT_INTRODUCER);
      WriteByte(Stream, GIF_EXT_GRAPHIC_CTRL);
      WriteByte(Stream, 4);  // 块大小
      Pkd := (Frame.FDisposal and $07) shl 2;
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
      PalSz := Length(Frame.FLocalPalette);
      J := 7;
      while (1 shl (J + 1)) < PalSz do Dec(J);
      Pkd := Pkd or (J and $07);
    end;
    WriteByte(Stream, Pkd);

    // Local Color Table
    if Frame.FHasLocalPalette then
      WriteColorTable(Stream, Frame.FLocalPalette, Length(Frame.FLocalPalette));

    // LZW 最小码长
    if Frame.FHasLocalPalette then
      PalSz := Length(Frame.FLocalPalette)
    else if FHasGlobalPalette then
      PalSz := Length(FGlobalPalette)
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

    // 写入 LZW 数据（原始数据优先，否则重新编码）
    if Frame.FRawData.Size > 0 then
    begin
      EmitSubBlocks(Stream, Frame.FRawData.Memory, Frame.FRawData.Size);
    end
    else
    begin
      SrcStm := TMemoryStream.Create;
      try
        EncodeLZW(Frame.FPixels, Frame.FWidth * Frame.FHeight,
                  SrcStm, MinCodeSize);
        EmitSubBlocks(Stream, SrcStm.Memory, SrcStm.Size);
      finally
        SrcStm.Free;
      end;
    end;
  end;

  // Trailer
  WriteByte(Stream, GIF_TRAILER);
end;

procedure TCnGIFImage.WriteColorTable(Stream: TStream;
  const Palette: TCnGIFColors; Count: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Stream.Write(Palette[I], 3);
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
  var
    I: Integer;
  begin
    FillChar(HT, SizeOf(HT), 0);
    for I := 0 to 255 do
    begin
      HT[I].Used := True;
      HT[I].Prefix := 0;
      HT[I].Suffix := Byte(I);
      HT[I].Code := I;
    end;
  end;

begin
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
            Inc(CodeSize);
            CodeMask := (1 shl CodeSize) - 1;
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

procedure TCnGIFImage.SetCurrentFrame(Value: Integer);
begin
  if (Value >= 0) and (Value < FFrames.Count) and (Value <> FCurrentFrame) then
  begin
    FCurrentFrame := Value;
    FRenderedFrame := -1;
  end;
end;

function TCnGIFImage.GetFrameCount: Integer;
begin
  Result := FFrames.Count;
end;

function TCnGIFImage.GetFrame(Index: Integer): TCnGIFFrame;
begin
  Result := TCnGIFFrame(FFrames[Index]);
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
}

end.
