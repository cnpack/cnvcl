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

unit CnJPEG;
{* |<PRE>
===============================================================================
* 单元名称：CnJPEG
* 单元说明：JPEG (JFIF) 图像格式读写单元
*           纯 Pascal 实现 JPEG 编解码，不依赖外部 DLL
*           支持 Baseline/Progressive JPEG 的读取和写入
*           通过 TGraphic 继承接入 Delphi TPicture 体系
* 开发平台：PWin98SE + Delphi 5.0
* 兼容平台：Delphi 5~最新版、FPC
* 操作系统：Windows / MacOS (FPC)
* 修改记录：2026.06.29 V1.0
*               创建单元
===============================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Graphics, CnNative;

const
  // 标准 JPEG ZigZag 扫描顺序
  CN_JPEG_ZIGZAG_ORDER: array[0..63] of Byte = (
     0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63
  );

  // 逆序表：ZigZagInv[natural_pos] = zigzag_pos
  CN_JPEG_ZIGZAG_INV: array[0..63] of Byte = (
     0,  1,  5,  6, 14, 15, 27, 28,
     2,  4,  7, 13, 16, 26, 29, 42,
     3,  8, 12, 17, 25, 30, 41, 43,
     9, 11, 18, 24, 31, 40, 44, 53,
    10, 19, 23, 32, 39, 45, 52, 54,
    20, 22, 33, 38, 46, 51, 55, 60,
    21, 34, 37, 47, 50, 56, 59, 61,
    35, 36, 48, 49, 57, 58, 62, 63
  );

type
  ECnJPEGException = class(Exception);

  TCnJPEGColorSpace = (jcRGB, jcGrayscale, jcCMYK);
  {* JPEG 颜色空间
  |<BR>  jcRGB       — RGB 彩色
  |<BR>  jcGrayscale — 灰度
  |<BR>  jcCMYK      — CMYK（Adobe APP14 标识）}

  TCnJPEGPerformance = (jpBestQuality, jpBestSpeed);
  {* 解码性能/质量权衡
  |<BR>  jpBestQuality — 使用精确 IDCT
  |<BR>  jpBestSpeed   — 使用快速 IDCT}

  TCnJPEGScale = (jsFullSize, jsHalf, jsQuarter, jsEighth);
  {* 解码缩放比例
  |<BR>  jsFullSize — 原始尺寸
  |<BR>  jsHalf     — 1/2 尺寸
  |<BR>  jsQuarter  — 1/4 尺寸
  |<BR>  jsEighth   — 1/8 尺寸}

  TCnJPEGPixelFormat = (jf24Bit, jf8Bit);
  {* 输出像素格式
  |<BR>  jf24Bit — 24 位真彩色
  |<BR>  jf8Bit  — 8 位灰度（仅 Grayscale 模式）}

  TCnJPEGQualityRange = 1..100;
  {* JPEG 压缩质量范围，1 最低，100 最高}

  //============================================================================
  // 辅助类型
  //============================================================================

  TCnJPEGHuffmanLookahead = record
    Valid: Boolean;
    Code: Byte;
    Size: Byte;
  end;

  TCnJPEGHuffmanTable = record
  {* Huffman 解码表}
    Bits: array[1..16] of Byte;      // 每个码长的码字数量
    HuffVal: array[0..255] of Byte;  // Huffman 值表
    MinCode: array[1..16] of Word;   // 运行时生成的最小码字
    MaxCode: array[1..16] of Word;   // 运行时生成的最大码字
    ValPtr: array[1..16] of Integer; // 运行时生成的值表指针
    Lookahead: array[0..255] of TCnJPEGHuffmanLookahead; // 8 位前缀快速查找
  end;
  PCnJPEGHuffmanTable = ^TCnJPEGHuffmanTable;

  TCnJPEGComponentInfo = record
  {* JPEG 分量信息（从 SOF 解析）}
    ID: Byte;
    HSampFactor: Byte;   // 水平采样因子
    VSampFactor: Byte;   // 垂直采样因子
    QuantTableID: Byte;  // 量化表 ID
    DCTableID: Byte;     // DC Huffman 表 ID
    ACTableID: Byte;     // AC Huffman 表 ID
  end;

  //============================================================================
  // TCnJPEGBitReader — 位读取器
  //============================================================================

  TCnJPEGBitReader = class
  {* JPEG 熵编码数据的位读取器}
  private
    FStream: TStream;
    FBitBuf: Cardinal;
    FBitCnt: Integer;
    FMarker: Byte;       // 读出的 RST marker 类型
    FEOF: Boolean;
    FHitMarker: Word;    // 遇到的非 RST marker 代码（0=无）
  public
    constructor Create(AStream: TStream);
    function GetBits(Count: Integer): Integer;
    function GetBit: Integer;
    procedure ResetDC;
    procedure AlignToByte;
    procedure ClearMarker;
    function IsEOF: Boolean;
    function HitMarker: Word;
  end;

  //============================================================================
  // TCnJPEGBitWriter — 位写入器
  //============================================================================

  TCnJPEGBitWriter = class
  {* JPEG 熵编码数据的位写入器}
  private
    FStream: TStream;
    FBitBuf: Cardinal;
    FBitCnt: Integer;
  public
    constructor Create(AStream: TStream);
    procedure PutBits(Code: Cardinal; Size: Integer);
    procedure PutBit(Bit: Integer);
    procedure Flush;
  end;

  //============================================================================
  // TCnJPEGMarker — Marker 读取器
  //============================================================================

  TCnJPEGMarker = class
  {* JPEG 文件 marker 解析器，顺序读取流中的标记段}
  private
    FStream: TStream;
    function ReadByte: Byte;
    function ReadWord: Word;
    procedure ReadBuf(var Buf; Count: Integer);
    procedure SkipBytes(Count: Integer);
  public
    constructor Create(AStream: TStream);
    function ReadMarker: Word;           // 读取下一个 marker（0xFF + code）
    function ReadSegment(Data: TStream): Integer; // 读取 marker 段数据，返回段长度
    function Position: Int64;
    procedure Seek(Pos: Int64);
  end;

  //============================================================================
  // TCnJPEGDecoder — 解码器（内部类）
  //============================================================================

  TCnJPEGDecoder = class
  {* JPEG 解码器，将 JPEG 流解码为 TBitmap}
  private
    FScale: TCnJPEGScale;
    FPerformance: TCnJPEGPerformance;
    FSmoothing: Boolean;
    FProgressiveDisplay: Boolean;

    // 帧参数
    FWidth: Integer;
    FHeight: Integer;
    FBitDepth: Integer;
    FNumComponents: Integer;
    FColorSpace: TCnJPEGColorSpace;
    FProgressive: Boolean;

    // 分量参数
    FComponents: array[0..3] of TCnJPEGComponentInfo;

    // MCU 参数
    FMCUWidth: Integer;
    FMCUHeight: Integer;
    FMCUsPerRow: Integer;
    FMCUsPerCol: Integer;
    FBlocksInMCU: Integer;
    FMaxH: Byte;
    FMaxV: Byte;
    FMCUBlockComp: array[0..9] of Integer;

    // 扫描分量信息
    FScanCompCount: Integer;
    FScanCompIdx: array[0..3] of Integer;
    FScanDCTable: array[0..3] of Byte;
    FScanACTable: array[0..3] of Byte;

    // 渐进式扫描参数
    FScanSs: Byte;       // 频谱选择起始
    FScanSe: Byte;       // 频谱选择结束
    FScanAh: Byte;       // 前一次逼近精度
    FScanAl: Byte;       // 本次逼近精度

    // 量化表
    FQuantTables: array[0..3] of array[0..63] of Word;

    // Huffman 表
    FDCTables: array[0..3] of TCnJPEGHuffmanTable;
    FACTables: array[0..3] of TCnJPEGHuffmanTable;

    // 重启间隔
    FRestartInterval: Integer;

    // 位读取器
    FBitReader: TCnJPEGBitReader;

    // DC 预测值
    FPrevDC: array[0..3] of Integer;

    // 渐进式系数存储
    FCoefBlocks: array of array of SmallInt;
    FCoefReady: array of array of Boolean;
    FCompBlocksPerRow: array[0..3] of Integer;
    FCompBlocksPerCol: array[0..3] of Integer;

    // Adobe CMYK 转换标记
    FAdobeTransform: Byte;

    // YCbCr→RGB 查找表
    FYCbCrTable: array[0..255, 0..255] of Cardinal;  // [Cb, Cr] → packed RGB
    FYCbCrTableInit: Boolean;

    procedure InitYCbCrTable;

    procedure ParseDQT(Marker: TCnJPEGMarker; SegLen: Integer);
    procedure ParseDHT(Marker: TCnJPEGMarker; SegLen: Integer);
    procedure ParseSOF(Marker: TCnJPEGMarker; SegLen: Integer; MarkerCode: Word);
    procedure ParseSOS(Marker: TCnJPEGMarker; SegLen: Integer);
    procedure ParseDRI(Marker: TCnJPEGMarker; SegLen: Integer);
    procedure ParseAPP0(Marker: TCnJPEGMarker; SegLen: Integer);
    procedure ParseAPP14(Marker: TCnJPEGMarker; SegLen: Integer);
    procedure CalculateMCUParams;
    function HuffmanDecode(var Table: TCnJPEGHuffmanTable): Byte;
    procedure DecodeScanBaseline(OutBmp: TBitmap);
    procedure DecodeScanProgressive(OutBmp: TBitmap; Stream: TStream; Marker: TCnJPEGMarker);
    procedure DecodeMCU(MCUX, MCUY: Integer; OutBmp: TBitmap);
    procedure DecodeBlock(CompIdx: Integer; var Coef: array of SmallInt);
    procedure DequantizeAndIDCT(var Coef: array of SmallInt;
      QuantID: Integer; var OutPixels: array of Byte;
      CompIdx: Integer; AScale: TCnJPEGScale);
    procedure UpsampleAndConvert(OutBmp: TBitmap);
    procedure ApplySmoothing(OutBmp: TBitmap);
    function ScaledOutWidth: Integer;
    function ScaledOutHeight: Integer;
    // 渐进式专用方法
    procedure AllocCoefBlocks;
    function GetCoefBlockPtr(CompIdx, BlockX, BlockY: Integer): PSmallInt;
    procedure DecodeDCScanProgressive;
    procedure DecodeACScanProgressive;
    procedure FinalizeProgressive(OutBmp: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Decode(Stream: TStream; OutBmp: TBitmap;
      AScale: TCnJPEGScale; APerformance: TCnJPEGPerformance;
      ASmoothing: Boolean; AProgressiveDisplay: Boolean);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property ColorSpace: TCnJPEGColorSpace read FColorSpace;
    property Progressive: Boolean read FProgressive;
    property NumComponents: Integer read FNumComponents;
  end;

  //============================================================================
  // TCnJPEGEncoder — 编码器（内部类）
  //============================================================================

  TCnJPEGEncoder = class
  {* JPEG 编码器，将 TBitmap 编码为 JPEG 流}
  private
    FQuality: TCnJPEGQualityRange;
    FGrayscale: Boolean;
    FProgressive: Boolean;
    FStream: TStream;

    FWidth: Integer;
    FHeight: Integer;
    FNumComponents: Integer;

    FHSampFactor: array[0..3] of Byte;
    FVSampFactor: array[0..3] of Byte;

    FQuantTables: array[0..3] of array[0..63] of Word;
    FDCTables: array[0..3] of TCnJPEGHuffmanTable;
    FACTables: array[0..3] of TCnJPEGHuffmanTable;

    FBitWriter: TCnJPEGBitWriter;

    // Huffman 编码查找表
    FDCEncCode: array[0..1, 0..255] of Cardinal;
    FDCEncSize: array[0..1, 0..255] of Byte;
    FACEncCode: array[0..1, 0..255] of Cardinal;
    FACEncSize: array[0..1, 0..255] of Byte;

    // DC 预测值
    FPrevDC: array[0..3] of Integer;

    // MCU 参数
    FMaxH: Byte;
    FMaxV: Byte;
    FMCUWidth: Integer;
    FMCUHeight: Integer;
    FMCUsPerRow: Integer;
    FMCUsPerCol: Integer;

    // 渐进式编码：预计算的 DCT 系数存储
    FCoefBlocks: array[0..3] of array of SmallInt;
    FCompBlocksPerRow: array[0..3] of Integer;
    FCompBlocksPerCol: array[0..3] of Integer;

    procedure BuildQuantTables(Quality: Integer);
    procedure BuildStandardHuffmanTables;
    procedure BuildEncLookup(const Bits: array of Byte;
      const HuffVal: array of Byte; NumVals: Integer;
      var EncCode: array of Cardinal; var EncSize: array of Byte);
    procedure WriteWord(W: Word);
    procedure WriteSOI;
    procedure WriteAPP0;
    procedure WriteDQT;
    procedure WriteSOF0;
    procedure WriteSOF2;
    procedure WriteDHT;
    procedure WriteSOS;
    procedure WriteSOSProgressive(Ss, Se, Ah, Al: Byte; CompList: array of Integer;
      NumComps: Integer);
    procedure WriteEOI;
    procedure EncodeMCU(SrcBmp: TBitmap; MCUX, MCUY: Integer);
    procedure EncodeBlock(Pixels: array of Byte; CompIdx: Integer;
      QuantID: Integer; var PrevDC: Integer);
    // 渐进式编码辅助
    procedure ComputeAllCoefBlocks(SrcBmp: TBitmap);
    function GetEncCoefBlockPtr(CompIdx, BlockX, BlockY: Integer): PSmallInt;
    procedure EncodeProgressive(SrcBmp: TBitmap);
    procedure EncodeDCFirstScan(CompList: array of Integer; NumComps: Integer; Al: Byte);
    procedure EncodeDCRefineScan(CompList: array of Integer; NumComps: Integer; Ah, Al: Byte);
    procedure EncodeACFirstScan(CompIdx: Integer; Ss, Se, Al: Byte);
    procedure EncodeACRefineScan(CompIdx: Integer; Ss, Se, Ah, Al: Byte);
    procedure WriteSOSForScan(Ss, Se, Ah, Al: Byte; CompIdx: Integer; IsAC: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Encode(SrcBmp: TBitmap; AStream: TStream;
      AQuality: TCnJPEGQualityRange; AGrayscale: Boolean;
      AProgressive: Boolean);
  end;

  //============================================================================
  // TCnJPEGData — 内部数据类
  //============================================================================

  TCnJPEGData = class(TPersistent)
  {* 持有原始 JPEG 压缩数据的内部类，支持引用计数}
  private
    FData: TMemoryStream;
    FWidth: Integer;
    FHeight: Integer;
    FGrayscale: Boolean;
    FProgressive: Boolean;
    FColorSpace: TCnJPEGColorSpace;
    FBitDepth: Integer;
    FRefCount: Integer;
    procedure ParseHeader;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure Assign(Source: TPersistent); override;
    procedure AddRef;
    procedure Release;
    property Data: TMemoryStream read FData;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Grayscale: Boolean read FGrayscale;
    property Progressive: Boolean read FProgressive;
    property ColorSpace: TCnJPEGColorSpace read FColorSpace;
    property BitDepth: Integer read FBitDepth;
  end;

  //============================================================================
  // TCnJPEGImage — 主类
  //============================================================================

  TCnJPEGImage = class(TGraphic)
  {* JPEG (JFIF) 图像类，纯 Pascal 实现 JPEG 编解码。
  |<BR> 继承自 TGraphic，可通过 TPicture.LoadFromFile 直接加载 .jpg/.jpeg 文件。
  |<BR> 支持读取和写入 Baseline/Progressive JPEG。}
  private
    FData: TCnJPEGData;
    FBitmap: TBitmap;
    FModified: Boolean;

    FCompressionQuality: TCnJPEGQualityRange;
    FGrayscale: Boolean;
    FPixelFormat: TCnJPEGPixelFormat;
    FPerformance: TCnJPEGPerformance;
    FProgressiveDisplay: Boolean;
    FProgressiveEncoding: Boolean;
    FScale: TCnJPEGScale;
    FSmoothing: Boolean;
    FTransparentColor: TColor;

    procedure FreeBitmap;
    procedure CreateBitmap;
    procedure NewJPEGData;
    function GetBitmap: TBitmap;
    function GetJPEGData: TCnJPEGData;
    procedure SetCompressionQuality(Value: TCnJPEGQualityRange);
    procedure SetGrayscale(Value: Boolean);
    procedure SetPixelFormat(Value: TCnJPEGPixelFormat);
    procedure SetPerformance(Value: TCnJPEGPerformance);
    procedure SetProgressiveDisplay(Value: Boolean);
    procedure SetProgressiveEncoding(Value: Boolean);
    procedure SetScale(Value: TCnJPEGScale);
    procedure SetSmoothing(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure Assign(Source: TPersistent); override;

    procedure Compress;
    procedure DIBNeeded;
    procedure JPEGNeeded;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); {$IFNDEF FPC} override; {$ENDIF}
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); {$IFNDEF FPC} override; {$ENDIF}

    procedure Clear; {$IFDEF FPC} override; {$ENDIF}

    property CompressionQuality: TCnJPEGQualityRange
      read FCompressionQuality write SetCompressionQuality;
    {* 压缩质量 (1-100)，影响编码时的量化表}

    property Grayscale: Boolean
      read FGrayscale write SetGrayscale;
    {* 是否以灰度模式编解码}

    property PixelFormat: TCnJPEGPixelFormat
      read FPixelFormat write SetPixelFormat;
    {* 输出位图像素格式}

    property Performance: TCnJPEGPerformance
      read FPerformance write SetPerformance;
    {* 解码性能模式}

    property ProgressiveDisplay: Boolean
      read FProgressiveDisplay write SetProgressiveDisplay;
    {* 渐进式显示（解码时逐次逼近）}

    property ProgressiveEncoding: Boolean
      read FProgressiveEncoding write SetProgressiveEncoding;
    {* 编码为渐进式 JPEG}

    property Scale: TCnJPEGScale
      read FScale write SetScale;
    {* 解码缩放比例}

    property Smoothing: Boolean
      read FSmoothing write SetSmoothing;
    {* 解码后块边界平滑}

    property TransparentColor: TColor
      read FTransparentColor write SetTransparentColor;
    {* 透明色（用于 Draw 时的透明处理）}

    property Bitmap: TBitmap read GetBitmap;
    {* 解码后的位图（只读访问，触发解码）}

    property NewBitmap: TBitmap read FBitmap;
    {* 内部位图对象引用（用于直接操作）}

    property NewJPEG: TCnJPEGData read FData;
    {* 内部 JPEG 数据对象引用}
  end;

// 注册/注销
procedure RegisterCnJPEG;
{* 向 TPicture 注册 TCnJPEGImage，关联 .jpg/.jpeg 扩展名}

procedure UnregisterCnJPEG;
{* 从 TPicture 注销 TCnJPEGImage}

implementation

resourcestring
  SCnErrorUnexpectedEndOfJpegData = 'Unexpected End of JPEG Data';
  SCnErrorInvalidMarkerInMarkerContext = 'Invalid Marker: FF00 in Marker Context';
  SCnErrorInvalidSegmentLength = 'Invalid Segment Length';
  SCnErrorInvalidQuantizationTableId = 'Invalid Quantization Table ID';
  SCnErrorInvalidHuffmanTableId = 'Invalid Huffman Table ID';
  SCnErrorUnsupportedSamplePrecision = 'Unsupported Sample Precision: ';
  SCnErrorTooManyComponents = 'Too Many Components: ';
  SCnErrorTooManyScanComponents = 'Too Many Scan Components';
  SCnErrorInvalidJpegMissingSoi = 'Invalid JPEG: Missing SOI';
  SCnErrorNoJpegDataToDecode = 'No JPEG Data to Decode';
  SCnErrorNoImageData = 'No Image Data';
  SCnErrorCannotAllocateClipboardMemory = 'Cannot Allocate Clipboard Memory';

//============================================================================
// 常量定义
//============================================================================

const
  // JPEG Marker 常量
  CN_JPEG_SOI   = $FFD8;  // Start of Image
  CN_JPEG_EOI   = $FFD9;  // End of Image
  CN_JPEG_SOS   = $FFDA;  // Start of Scan
  CN_JPEG_DQT   = $FFDB;  // Define Quantization Table
  CN_JPEG_DHT   = $FFC4;  // Define Huffman Table
  CN_JPEG_DRI   = $FFDD;  // Define Restart Interval
  CN_JPEG_SOF0  = $FFC0;  // Baseline DCT
  CN_JPEG_SOF1  = $FFC1;  // Extended DCT
  CN_JPEG_SOF2  = $FFC2;  // Progressive DCT
  CN_JPEG_APP0  = $FFE0;  // JFIF
  CN_JPEG_APP1  = $FFE1;  // EXIF
  CN_JPEG_APP14 = $FFEE;  // Adobe
  CN_JPEG_COM   = $FFFE;  // Comment
  CN_JPEG_RST0  = $FFD0;  // Restart Marker 0
  CN_JPEG_RST7  = $FFD7;  // Restart Marker 7
  CN_JPEG_TEM   = $FF01;  // Temporary marker

  CN_JPEG_MAX_COMPONENTS = 4;
  CN_JPEG_BLOCK_SIZE = 64;  // 8×8

  // IDCT 余弦表（定点，缩放因子 2^15 = 32768）
  // idct_cos[u][x] = round(C(u) * cos((2x+1)*u*PI/16) * 32768)
  // C(0) = 1/sqrt(2), C(u) = 1 for u > 0
  CN_JPEG_IDCT_COS: array[0..7, 0..7] of Integer = (
    ( 23170,  23170,  23170,  23170,  23170,  23170,  23170,  23170),
    ( 32138,  27245,  18204,   6392,  -6392, -18204, -27245, -32138),
    ( 30274,  12539, -12539, -30274, -30274, -12539,  12539,  30274),
    ( 27245,  -6392, -32138, -18204,  18204,  32138,   6392, -27245),
    ( 23170, -23170, -23170,  23170,  23170, -23170, -23170,  23170),
    ( 18204, -32138,   6392,  27245, -27245,  -6392,  32138, -18204),
    ( 12539, -30274,  30274, -12539, -12539,  30274, -30274,  12539),
    (  6392, -18204,  27245, -32138,  32138, -27245,  18204,  -6392)
  );

  // 标准亮度量化表（自然顺序）
  CN_JPEG_STD_LUMINANCE_QUANT: array[0..63] of Byte = (
    16, 11, 10, 16, 24, 40, 51, 61,
    12, 12, 14, 19, 26, 58, 60, 55,
    14, 13, 16, 24, 40, 57, 69, 56,
    14, 17, 22, 29, 51, 87, 80, 62,
    18, 22, 37, 56, 68,109,103, 77,
    24, 35, 55, 64, 81,104,113, 92,
    49, 64, 78, 87,103,121,120,101,
    72, 92, 95, 98,112,100,103, 99
  );

  // 标准色度量化表（自然顺序）
  CN_JPEG_STD_CHROMINANCE_QUANT: array[0..63] of Byte = (
    17, 18, 24, 47, 99, 99, 99, 99,
    18, 21, 26, 66, 99, 99, 99, 99,
    24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99
  );

  // 标准 DC 亮度 Huffman 表 (JPEG T.81 Annex K, Table K.3)
  CN_JPEG_STD_DC_LUMINANCE_BITS: array[1..16] of Byte = (
    0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0
  );
  CN_JPEG_STD_DC_LUMINANCE_VAL: array[0..11] of Byte = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
  );

  // 标准 DC 色度 Huffman 表 (Table K.4)
  CN_JPEG_STD_DC_CHROMINANCE_BITS: array[1..16] of Byte = (
    0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
  );
  CN_JPEG_STD_DC_CHROMINANCE_VAL: array[0..11] of Byte = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
  );

  // 标准 AC 亮度 Huffman 表 (Table K.5)
  CN_JPEG_STD_AC_LUMINANCE_BITS: array[1..16] of Byte = (
    0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, $7D
  );
  CN_JPEG_STD_AC_LUMINANCE_VAL: array[0..161] of Byte = (
    $01, $02, $03, $00, $04, $11, $05, $12,
    $21, $31, $41, $06, $13, $51, $61, $07,
    $22, $71, $14, $32, $81, $91, $A1, $08,
    $23, $42, $B1, $C1, $15, $52, $D1, $F0,
    $24, $33, $62, $72, $82, $09, $0A, $16,
    $17, $18, $19, $1A, $25, $26, $27, $28,
    $29, $2A, $34, $35, $36, $37, $38, $39,
    $3A, $43, $44, $45, $46, $47, $48, $49,
    $4A, $53, $54, $55, $56, $57, $58, $59,
    $5A, $63, $64, $65, $66, $67, $68, $69,
    $6A, $73, $74, $75, $76, $77, $78, $79,
    $7A, $83, $84, $85, $86, $87, $88, $89,
    $8A, $92, $93, $94, $95, $96, $97, $98,
    $99, $9A, $A2, $A3, $A4, $A5, $A6, $A7,
    $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6,
    $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5,
    $C6, $C7, $C8, $C9, $CA, $D2, $D3, $D4,
    $D5, $D6, $D7, $D8, $D9, $DA, $E1, $E2,
    $E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA,
    $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8,
    $F9, $FA
  );

  // 标准 AC 色度 Huffman 表 (Table K.6)
  CN_JPEG_STD_AC_CHROMINANCE_BITS: array[1..16] of Byte = (
    0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, $77
  );
  CN_JPEG_STD_AC_CHROMINANCE_VAL: array[0..161] of Byte = (
    $00, $01, $02, $11, $03, $04, $21, $31,
    $05, $12, $41, $51, $06, $13, $22, $61,
    $07, $71, $32, $81, $14, $42, $91, $A1,
    $08, $23, $33, $B1, $C1, $15, $52, $D1,
    $F0, $24, $34, $62, $72, $82, $09, $0A,
    $16, $17, $18, $19, $1A, $25, $26, $27,
    $28, $29, $2A, $35, $36, $37, $38, $39,
    $3A, $43, $44, $45, $46, $47, $48, $49,
    $4A, $53, $54, $55, $56, $57, $58, $59,
    $5A, $63, $64, $65, $66, $67, $68, $69,
    $6A, $73, $74, $75, $76, $77, $78, $79,
    $7A, $83, $84, $85, $86, $87, $88, $89,
    $8A, $92, $93, $94, $95, $96, $97, $98,
    $99, $9A, $A2, $A3, $A4, $A5, $A6, $A7,
    $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6,
    $B7, $B8, $B9, $BA, $C2, $C3, $C4, $C5,
    $C6, $C7, $C8, $C9, $CA, $D2, $D3, $D4,
    $D5, $D6, $D7, $D8, $D9, $DA, $E1, $E2,
    $E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA,
    $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8,
    $F9, $FA
  );

var
  CN_JPEG_CLIPBOARD_FORMAT: Word = 0;

//============================================================================
// 辅助函数
//============================================================================

function CnJPEGYCbCrToRGB(Y, Cb, Cr: Integer): TColor;
var
  R, G, B: Integer;
begin
  R := Y + Round(1.402 * (Cr - 128));
  G := Y - Round(0.34414 * (Cb - 128)) - Round(0.71414 * (Cr - 128));
  B := Y + Round(1.772 * (Cb - 128));
  if R < 0 then R := 0;
  if R > 255 then R := 255;
  if G < 0 then G := 0;
  if G > 255 then G := 255;
  if B < 0 then B := 0;
  if B > 255 then B := 255;
  Result := RGB(R, G, B);
end;

procedure CnJPEGRGBToYCbCr(R, G, B: Integer; var Y, Cb, Cr: Integer);
begin
  Y  := Round(0.299 * R + 0.587 * G + 0.114 * B);
  Cb := Round(-0.16874 * R - 0.33126 * G + 0.5 * B) + 128;
  Cr := Round(0.5 * R - 0.41869 * G - 0.08131 * B) + 128;
  if Y < 0 then Y := 0;
  if Y > 255 then Y := 255;
  if Cb < 0 then Cb := 0;
  if Cb > 255 then Cb := 255;
  if Cr < 0 then Cr := 0;
  if Cr > 255 then Cr := 255;
end;

// YCbCr→RGB 查找表（消除解码循环中的浮点运算）
var
  CnJPEGCrToR: array[0..255] of Integer;
  CnJPEGCbToG: array[0..255] of Integer;
  CnJPEGCrToG: array[0..255] of Integer;
  CnJPEGCbToB: array[0..255] of Integer;
  CnJPEGYCbCrInit: Boolean = False;

procedure CnJPEGInitYCbCrTables;
var
  I: Integer;
begin
  if CnJPEGYCbCrInit then Exit;
  for I := 0 to 255 do
  begin
    CnJPEGCrToR[I] := Round(1.402 * (I - 128));
    CnJPEGCbToG[I] := -Round(0.34414 * (I - 128));
    CnJPEGCrToG[I] := -Round(0.71414 * (I - 128));
    CnJPEGCbToB[I] := Round(1.772 * (I - 128));
  end;
  CnJPEGYCbCrInit := True;
end;

procedure CnJPEGBuildHuffmanTable(var Table: TCnJPEGHuffmanTable);
var
  I, J, K: Integer;
  Code: Integer;
  LookBits: Integer;
  LookSize: Integer;
begin
  // 生成 MinCode/MaxCode/ValPtr 数组
  K := 0;
  Code := 0;
  for I := 1 to 16 do
  begin
    Table.ValPtr[I] := K;
    if Table.Bits[I] > 0 then
    begin
      Table.MinCode[I] := Code;
      Inc(Code, Table.Bits[I]);
      Table.MaxCode[I] := Code - 1;
      Inc(K, Table.Bits[I]);
    end
    else
    begin
      Table.MinCode[I] := 0;
      Table.MaxCode[I] := $FFFF; // -1 表示该码长无码字
    end;
    Code := Code shl 1;
  end;

  // 生成 8 位 Lookahead 快速查找表
  for I := 0 to 255 do
  begin
    Table.Lookahead[I].Valid := False;
    Table.Lookahead[I].Code := 0;
    Table.Lookahead[I].Size := 0;
  end;

  Code := 0;
  K := 0;
  for I := 1 to 16 do
  begin
    for J := 1 to Table.Bits[I] do
    begin
      if I <= 8 then
      begin
        // 对所有以当前码字为前缀的 8 位组合填入
        LookBits := Code shl (8 - I);
        for LookSize := 0 to (1 shl (8 - I)) - 1 do
        begin
          Table.Lookahead[LookBits or LookSize].Valid := True;
          Table.Lookahead[LookBits or LookSize].Code := Table.HuffVal[K];
          Table.Lookahead[LookBits or LookSize].Size := I;
        end;
      end;
      Inc(K);
      Inc(Code);
    end;
    Code := Code shl 1;
  end;
end;

//============================================================================
// TCnJPEGBitReader
//============================================================================

constructor TCnJPEGBitReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FBitBuf := 0;
  FBitCnt := 0;
  FMarker := 0;
  FEOF := False;
  FHitMarker := 0;
end;

function TCnJPEGBitReader.GetBit: Integer;
begin
  Result := GetBits(1);
end;

function TCnJPEGBitReader.GetBits(Count: Integer): Integer;
var
  B: Byte;
begin
  while FBitCnt < Count do
  begin
    if FStream.Read(B, 1) <> 1 then
    begin
      FEOF := True;
      // 用零填充剩余位
      Result := Integer(FBitBuf) shr (32 - Count);
      Exit;
    end;

    if B = $FF then
    begin
      // 读取下一字节
      if FStream.Read(B, 1) <> 1 then
      begin
        FEOF := True;
        Result := Integer(FBitBuf) shr (32 - Count);
        Exit;
      end;

      if B = $00 then
      begin
        // FF 00 → 数据中的 FF
        B := $FF;
      end
      else if (B >= $D0) and (B <= $D7) then
      begin
        // 重启标记 RST0-RST7：记录并重置位缓冲
        FMarker := B;
        FBitBuf := 0;
        FBitCnt := 0;
        Continue;  // 继续读取新重启间隔的数据
      end
      else if B = $D9 then
      begin
        // EOI
        FHitMarker := $FFD9;
        FEOF := True;
        Result := Integer(FBitBuf) shr (32 - Count);
        Exit;
      end
      else
      begin
        // 其他标记（SOS/DHT/DRI 等），记录并当作 EOF 处理
        FHitMarker := $FF00 or B;
        FEOF := True;
        Result := Integer(FBitBuf) shr (32 - Count);
        Exit;
      end;
    end;

    // 将字节加入位缓冲（高位在前）
    FBitBuf := (FBitBuf shl 8) or B;
    Inc(FBitCnt, 8);
  end;

  // 从高位提取 Count 位
  Result := Integer(FBitBuf) shr (32 - Count);
  // 清除已读位
  FBitBuf := FBitBuf shl Count;
  Dec(FBitCnt, Count);
end;

procedure TCnJPEGBitReader.ResetDC;
begin
  FBitBuf := 0;
  FBitCnt := 0;
  FMarker := 0;
end;

procedure TCnJPEGBitReader.AlignToByte;
begin
  FBitBuf := 0;
  FBitCnt := 0;
end;

procedure TCnJPEGBitReader.ClearMarker;
begin
  FMarker := 0;
end;

function TCnJPEGBitReader.IsEOF: Boolean;
begin
  Result := FEOF;
end;

function TCnJPEGBitReader.HitMarker: Word;
begin
  Result := FHitMarker;
end;

//============================================================================
// TCnJPEGBitWriter
//============================================================================

constructor TCnJPEGBitWriter.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FBitBuf := 0;
  FBitCnt := 0;
end;

procedure TCnJPEGBitWriter.PutBit(Bit: Integer);
begin
  PutBits(Cardinal(Bit), 1);
end;

procedure TCnJPEGBitWriter.PutBits(Code: Cardinal; Size: Integer);
var
  B: Byte;
begin
  while Size > 0 do
  begin
    Dec(Size);
    FBitBuf := (FBitBuf shl 1) or ((Code shr Size) and 1);
    Inc(FBitCnt);
    if FBitCnt = 8 then
    begin
      B := Byte(FBitBuf);
      FStream.Write(B, 1);
      if B = $FF then
      begin
        B := $00;
        FStream.Write(B, 1);
      end;
      FBitBuf := 0;
      FBitCnt := 0;
    end
  end;
end;

procedure TCnJPEGBitWriter.Flush;
var
  B: Byte;
begin
  if FBitCnt > 0 then
  begin
    // 用 1 填充剩余位
    FBitBuf := FBitBuf shl (8 - FBitCnt);
    FBitBuf := FBitBuf or ((1 shl (8 - FBitCnt)) - 1);
    B := Byte(FBitBuf);
    FStream.Write(B, 1);
    if B = $FF then
    begin
      B := $00;
      FStream.Write(B, 1);
    end;
    FBitBuf := 0;
    FBitCnt := 0;
  end;
end;

//============================================================================
// TCnJPEGMarker
//============================================================================

constructor TCnJPEGMarker.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TCnJPEGMarker.ReadByte: Byte;
begin
  if FStream.Read(Result, 1) <> 1 then
    raise ECnJPEGException.Create(SCnErrorUnexpectedEndOfJpegData);
end;

function TCnJPEGMarker.ReadWord: Word;
var
  Hi, Lo: Byte;
begin
  Hi := ReadByte;
  Lo := ReadByte;
  Result := (Hi shl 8) or Lo;
end;

procedure TCnJPEGMarker.ReadBuf(var Buf; Count: Integer);
var
  P: PByte;
  Remaining: Integer;
  Read: Integer;
begin
  P := @Buf;
  Remaining := Count;
  while Remaining > 0 do
  begin
    Read := FStream.Read(P^, Remaining);
    if Read <= 0 then
      raise ECnJPEGException.Create(SCnErrorUnexpectedEndOfJpegData);
    Inc(P, Read);
    Dec(Remaining, Read);
  end;
end;

procedure TCnJPEGMarker.SkipBytes(Count: Integer);
begin
  FStream.Seek(Count, soFromCurrent);
end;

function TCnJPEGMarker.ReadMarker: Word;
var
  B: Byte;
begin
  // 跳过非 FF 字节，找到 marker 前缀
  repeat
    B := ReadByte;
  until B = $FF;

  // 跳过填充 FF
  repeat
    B := ReadByte;
  until B <> $FF;

  // B=00 是字节对齐填充（不应出现在 marker 扫描中），当作错误处理
  if B = $00 then
    raise ECnJPEGException.Create(SCnErrorInvalidMarkerInMarkerContext);

  Result := $FF00 or B;
end;

function TCnJPEGMarker.ReadSegment(Data: TStream): Integer;
var
  SegLen: Word;
  Buf: array[0..4095] of Byte;
  Remaining: Integer;
  Read: Integer;
begin
  SegLen := ReadWord;
  Result := SegLen;
  if SegLen < 2 then
    raise ECnJPEGException.Create(SCnErrorInvalidSegmentLength);
  Dec(SegLen, 2); // 减去长度字段自身

  Remaining := SegLen;
  while Remaining > 0 do
  begin
    if Remaining > SizeOf(Buf) then
      Read := SizeOf(Buf)
    else
      Read := Remaining;
    ReadBuf(Buf, Read);
    Data.Write(Buf, Read);
    Dec(Remaining, Read);
  end;
end;

function TCnJPEGMarker.Position: Int64;
begin
  Result := FStream.Position;
end;

procedure TCnJPEGMarker.Seek(Pos: Int64);
begin
  FStream.Position := Pos;
end;

//============================================================================
// TCnJPEGDecoder
//============================================================================

constructor TCnJPEGDecoder.Create;
begin
inherited Create;
FScale := jsFullSize;
FPerformance := jpBestQuality;
FSmoothing := True;
FProgressiveDisplay := False;
FWidth := 0;
FHeight := 0;
FBitDepth := 8;
FNumComponents := 3;
FColorSpace := jcRGB;
FProgressive := False;
FRestartInterval := 0;
FBitReader := nil;
FAdobeTransform := 0;
FYCbCrTableInit := False;
end;

destructor TCnJPEGDecoder.Destroy;
begin
  if FBitReader <> nil then
    FBitReader.Free;
  inherited Destroy;
end;

procedure TCnJPEGDecoder.ParseDQT(Marker: TCnJPEGMarker; SegLen: Integer);
var
  Remaining: Integer;
  Prec: Byte;
  TableID: Byte;
  I: Integer;
  B: Byte;
begin
  Remaining := SegLen - 2;
  while Remaining > 0 do
  begin
    B := Marker.ReadByte;
    Dec(Remaining);
    Prec := B shr 4;       // 精度：0=8bit, 1=16bit
    TableID := B and $0F;  // 表 ID (0-3)
    if TableID > 3 then
      raise ECnJPEGException.Create(SCnErrorInvalidQuantizationTableId);

    for I := 0 to 63 do
    begin
      if Prec = 0 then
      begin
        FQuantTables[TableID, CN_JPEG_ZIGZAG_ORDER[I]] := Marker.ReadByte;
        Dec(Remaining);
      end
      else
      begin
        FQuantTables[TableID, CN_JPEG_ZIGZAG_ORDER[I]] := Marker.ReadWord;
        Dec(Remaining, 2);
      end;
    end;
  end;
end;

procedure TCnJPEGDecoder.ParseDHT(Marker: TCnJPEGMarker; SegLen: Integer);
var
  Remaining: Integer;
  Tc, Th: Byte;
  B: Byte;
  I: Integer;
  Count: Integer;
  Table: PCnJPEGHuffmanTable;
begin
  Remaining := SegLen - 2;
  while Remaining > 0 do
  begin
    B := Marker.ReadByte;
    Dec(Remaining);
    Tc := B shr 4;        // 表类别：0=DC, 1=AC
    Th := B and $0F;      // 表 ID (0-3)
    if Th > 3 then
      raise ECnJPEGException.Create(SCnErrorInvalidHuffmanTableId);

    if Tc = 0 then
      Table := @FDCTables[Th]
    else
      Table := @FACTables[Th];

    // 读取 16 字节 BITS
    for I := 1 to 16 do
    begin
      Table.Bits[I] := Marker.ReadByte;
      Dec(Remaining);
    end;

    // 计算总码字数
    Count := 0;
    for I := 1 to 16 do
      Inc(Count, Table.Bits[I]);

    // 读取 HUFFVAL
    for I := 0 to Count - 1 do
    begin
      Table.HuffVal[I] := Marker.ReadByte;
      Dec(Remaining);
    end;

    // 构建查找表
    CnJPEGBuildHuffmanTable(Table^);
  end;
end;

procedure TCnJPEGDecoder.ParseSOF(Marker: TCnJPEGMarker; SegLen: Integer;
  MarkerCode: Word);
var
  Precision: Byte;
  Nf: Byte;
  I: Integer;
  B: Byte;
  C: TCnJPEGComponentInfo;
begin
  Precision := Marker.ReadByte;
  if Precision <> 8 then
    raise ECnJPEGException.Create(SCnErrorUnsupportedSamplePrecision + IntToStr(Precision));

  FHeight := Marker.ReadWord;
  FWidth := Marker.ReadWord;
  Nf := Marker.ReadByte;
  FNumComponents := Nf;

  if Nf > CN_JPEG_MAX_COMPONENTS then
    raise ECnJPEGException.Create(SCnErrorTooManyComponents + IntToStr(Nf));

  for I := 0 to Nf - 1 do
  begin
    C.ID := Marker.ReadByte;
    B := Marker.ReadByte;
    C.HSampFactor := B shr 4;
    C.VSampFactor := B and $0F;
    C.QuantTableID := Marker.ReadByte;
    C.DCTableID := 0;
    C.ACTableID := 0;
    FComponents[I] := C;
  end;

  // 设置色彩空间
  if Nf = 1 then
  begin
    FColorSpace := jcGrayscale;
  end
  else if Nf = 3 then
  begin
    FColorSpace := jcRGB;
  end
  else if Nf = 4 then
  begin
    FColorSpace := jcCMYK;
  end;

  // 设置渐进标志
  if MarkerCode = CN_JPEG_SOF2 then
    FProgressive := True
  else
    FProgressive := False;
end;

procedure TCnJPEGDecoder.ParseSOS(Marker: TCnJPEGMarker; SegLen: Integer);
var
  Ns: Byte;
  I, J: Integer;
  Cs, B: Byte;
begin
  Ns := Marker.ReadByte;  // 扫描分量数
  FScanCompCount := Ns;
  if Ns > CN_JPEG_MAX_COMPONENTS then
    raise ECnJPEGException.Create(SCnErrorTooManyScanComponents);

  for I := 0 to Ns - 1 do
  begin
    Cs := Marker.ReadByte;  // 分量选择器
    B := Marker.ReadByte;   // DC/AC 表 ID

    // 查找分量索引
    FScanCompIdx[I] := -1;
    for J := 0 to FNumComponents - 1 do
    begin
      if FComponents[J].ID = Cs then
      begin
        FScanCompIdx[I] := J;
        FComponents[J].DCTableID := B shr 4;
        FComponents[J].ACTableID := B and $0F;
        Break;
      end;
    end;
  end;

  FScanSs := Marker.ReadByte;  // Ss（频谱选择起始）
  FScanSe := Marker.ReadByte;  // Se（频谱选择结束）
  B := Marker.ReadByte;        // Ah/Al（逐次逼近参数）
  FScanAh := B shr 4;
  FScanAl := B and $0F;
end;

procedure TCnJPEGDecoder.ParseDRI(Marker: TCnJPEGMarker; SegLen: Integer);
begin
  FRestartInterval := Marker.ReadWord;
end;

procedure TCnJPEGDecoder.ParseAPP0(Marker: TCnJPEGMarker; SegLen: Integer);
var
  Ident: array[0..4] of AnsiChar;
  Version: Word;
  Units: Byte;
  XDensity, YDensity: Word;
  ThumbW, ThumbH: Byte;
begin
  if SegLen < 14 then
  begin
    Marker.SkipBytes(SegLen - 2);
    Exit;
  end;

  Marker.ReadBuf(Ident[0], 5);
  if (Ident[0] = 'J') and (Ident[1] = 'F') and (Ident[2] = 'I') and
     (Ident[3] = 'F') and (Ident[4] = #0) then
  begin
    Version := Marker.ReadWord;
    Units := Marker.ReadByte;
    XDensity := Marker.ReadWord;
    YDensity := Marker.ReadWord;
    ThumbW := Marker.ReadByte;
    ThumbH := Marker.ReadByte;
    // 跳过缩略图数据
    if (ThumbW > 0) and (ThumbH > 0) then
      Marker.SkipBytes(ThumbW * ThumbH * 3);
  end
  else
  begin
    // 非 JFIF APP0，跳过剩余数据
    Marker.SkipBytes(SegLen - 2 - 5);
  end;
end;

procedure TCnJPEGDecoder.ParseAPP14(Marker: TCnJPEGMarker; SegLen: Integer);
var
  Ident: array[0..4] of AnsiChar;
  Version: Word;
  Transform: Byte;
begin
  if SegLen < 12 then
  begin
    Marker.SkipBytes(SegLen - 2);
    Exit;
  end;

  Marker.ReadBuf(Ident[0], 5);
  if (Ident[0] = 'A') and (Ident[1] = 'd') and (Ident[2] = 'o') and
    (Ident[3] = 'b') and (Ident[4] = 'e') then
  begin
    Version := Marker.ReadWord;
    Marker.ReadWord; // Flags0
    Marker.ReadWord; // flags
    Transform := Marker.ReadByte;
    FAdobeTransform := Transform;
    // Transform: 0=Unknown(CMYK), 1=YCbCr, 2=YCC(YCCK)
    if FNumComponents = 4 then
      FColorSpace := jcCMYK;
  end
  else
  begin
    Marker.SkipBytes(SegLen - 2 - 5);
  end;
end;

procedure TCnJPEGDecoder.CalculateMCUParams;
var
  I, K: Integer;
  MaxH, MaxV: Byte;
begin
  MaxH := 1;
  MaxV := 1;
  for I := 0 to FNumComponents - 1 do
  begin
    if FComponents[I].HSampFactor > MaxH then
      MaxH := FComponents[I].HSampFactor;
    if FComponents[I].VSampFactor > MaxV then
      MaxV := FComponents[I].VSampFactor;
  end;
  FMaxH := MaxH;
  FMaxV := MaxV;

  FMCUWidth := 8 * MaxH;
  FMCUHeight := 8 * MaxV;
  FMCUsPerRow := (FWidth + FMCUWidth - 1) div FMCUWidth;
  FMCUsPerCol := (FHeight + FMCUHeight - 1) div FMCUHeight;

  // 构建 MCU 块布局
  K := 0;
  for I := 0 to FNumComponents - 1 do
    Inc(K, FComponents[I].HSampFactor * FComponents[I].VSampFactor);
  FBlocksInMCU := K;
end;

function TCnJPEGDecoder.HuffmanDecode(var Table: TCnJPEGHuffmanTable): Byte;
var
  L: Integer;
  Code: Integer;
begin
  Code := 0;
  for L := 1 to 16 do
  begin
    Code := (Code shl 1) or FBitReader.GetBit;
    if (Table.Bits[L] > 0) and (Code <= Table.MaxCode[L]) then
    begin
      Result := Table.HuffVal[Table.ValPtr[L] + Code - Table.MinCode[L]];
      Exit;
    end;
  end;
  Result := 0;  // 不应到达此处
end;

function TCnJPEGDecoder.ScaledOutWidth: Integer;
begin
  case FScale of
    jsHalf:    Result := (FWidth + 1) div 2;
    jsQuarter: Result := (FWidth + 3) div 4;
    jsEighth:  Result := (FWidth + 7) div 8;
  else
    Result := FWidth;
  end;
end;

function TCnJPEGDecoder.ScaledOutHeight: Integer;
begin
  case FScale of
    jsHalf:    Result := (FHeight + 1) div 2;
    jsQuarter: Result := (FHeight + 3) div 4;
    jsEighth:  Result := (FHeight + 7) div 8;
  else
    Result := FHeight;
  end;
end;

procedure TCnJPEGDecoder.DecodeBlock(CompIdx: Integer;
  var Coef: array of SmallInt);
var
  K: Integer;
  S: Byte;
  RS: Byte;
  Run, Size: Byte;
  Diff, Val: Integer;
  DCTable, ACTable: PCnJPEGHuffmanTable;
begin
  // 清零系数
  FillChar(Coef[0], 128, 0);

  DCTable := @FDCTables[FComponents[CompIdx].DCTableID];
  ACTable := @FACTables[FComponents[CompIdx].ACTableID];

  // 解码 DC 系数
  S := HuffmanDecode(DCTable^);
  if S = 0 then
    Diff := 0
  else
  begin
    Diff := FBitReader.GetBits(S);
    if Diff < (1 shl (S - 1)) then
      Diff := Diff - (1 shl S) + 1;
  end;
  Inc(FPrevDC[CompIdx], Diff);
  Coef[0] := SmallInt(FPrevDC[CompIdx]);

  // 解码 AC 系数
  K := 1;
  while K < 64 do
  begin
    RS := HuffmanDecode(ACTable^);
    Run := RS shr 4;
    Size := RS and $0F;

    if Size = 0 then
    begin
      if Run = 0 then
        Break  // EOB
      else if Run = 15 then
        Inc(K, 16)  // ZRL
      else
        Break;  // 无效，当作 EOB
      Continue;
    end;

    Inc(K, Run);
    if K >= 64 then Break;

    Val := FBitReader.GetBits(Size);
    if Val < (1 shl (Size - 1)) then
      Val := Val - (1 shl Size) + 1;

    Coef[CN_JPEG_ZIGZAG_ORDER[K]] := SmallInt(Val);
    Inc(K);
  end;
end;

procedure TCnJPEGDecoder.DequantizeAndIDCT(var Coef: array of SmallInt;
  QuantID: Integer; var OutPixels: array of Byte;
  CompIdx: Integer; AScale: TCnJPEGScale);
var
  I, X, Y, U, V: Integer;
  DQ: array[0..63] of Integer;
  Tmp: array[0..63] of Integer;
  Sum: Int64;
begin
  // 反量化（Coef 和 QuantTable 均为自然顺序）
  for I := 0 to 63 do
    DQ[I] := Coef[I] * FQuantTables[QuantID, I];

  // 行变换: Tmp[x*8+v] = sum_u cos[u][x] * DQ[v*8+u], 然后 >> 16
  for V := 0 to 7 do
  begin
    for X := 0 to 7 do
    begin
      Sum := 0;
      for U := 0 to 7 do
        Inc(Sum, Int64(CN_JPEG_IDCT_COS[U, X]) * DQ[V * 8 + U]);
      Tmp[X * 8 + V] := Integer(Sum shr 16);
    end;
  end;

  // 列变换: Out[y*8+x] = (sum_v cos[v][y] * Tmp[x*8+v]) >> 16, +128, clamp
  for X := 0 to 7 do
  begin
    for Y := 0 to 7 do
    begin
      Sum := 0;
      for V := 0 to 7 do
        Inc(Sum, Int64(CN_JPEG_IDCT_COS[V, Y]) * Tmp[X * 8 + V]);
      Sum := Sum shr 16;
      Inc(Sum, 128);
      if Sum < 0 then Sum := 0;
      if Sum > 255 then Sum := 255;
      OutPixels[Y * 8 + X] := Byte(Sum);
    end;
  end;
end;

procedure TCnJPEGDecoder.DecodeMCU(MCUX, MCUY: Integer; OutBmp: TBitmap);
var
  CompIdx, BX, BY, PX, PY: Integer;
  Coef: array[0..63] of SmallInt;
  Pixels: array[0..63] of Byte;
  // 分量像素缓冲（最大 16×16 = 256 字节）
  CompBuf: array[0..3, 0..255] of Byte;
  CompW, CompH: array[0..3] of Integer;
  OutX, OutY: Integer;
  MaxPX, MaxPY: Integer;
  YVal, CbVal, CrVal, AVal: Byte;
  CbX, CbY: Integer;
  R, G, B: Integer;
  RowPtr: PByteArray;
  ImgX, ImgY: Integer;
begin
  FillChar(CompBuf[0, 0], SizeOf(CompBuf), 0);

  // 解码每个分量的每个块
  for CompIdx := 0 to FNumComponents - 1 do
  begin
    CompW[CompIdx] := 8 * FComponents[CompIdx].HSampFactor;
    CompH[CompIdx] := 8 * FComponents[CompIdx].VSampFactor;

    for BY := 0 to FComponents[CompIdx].VSampFactor - 1 do
      for BX := 0 to FComponents[CompIdx].HSampFactor - 1 do
      begin
        DecodeBlock(CompIdx, Coef);
        DequantizeAndIDCT(Coef, FComponents[CompIdx].QuantTableID,
          Pixels, CompIdx, FScale);

        // 将 8×8 像素复制到分量缓冲
        for PY := 0 to 7 do
          for PX := 0 to 7 do
            CompBuf[CompIdx, (BY * 8 + PY) * CompW[CompIdx] + (BX * 8 + PX)] :=
              Pixels[PY * 8 + PX];
      end;
  end;

  // 上采样 + 色彩转换 + 写入位图
  MaxPX := FMCUWidth;
  MaxPY := FMCUHeight;
  if MCUX = FMCUsPerRow - 1 then
    MaxPX := FWidth - MCUX * FMCUWidth;
  if MCUY = FMCUsPerCol - 1 then
    MaxPY := FHeight - MCUY * FMCUHeight;

  OutX := MCUX * FMCUWidth;
  OutY := MCUY * FMCUHeight;

  for PY := 0 to MaxPY - 1 do
  begin
    ImgY := OutY + PY;
    if ImgY >= FHeight then Break;
    if ImgY >= OutBmp.Height then Break;

    RowPtr := OutBmp.ScanLine[ImgY];

    for PX := 0 to MaxPX - 1 do
    begin
      ImgX := OutX + PX;
      if ImgX >= FWidth then Break;
      if ImgX >= OutBmp.Width then Break;

      if FNumComponents = 1 then
      begin
        // 灰度
        YVal := CompBuf[0, PY * CompW[0] + PX];
        RowPtr[ImgX * 3]     := YVal;  // B
        RowPtr[ImgX * 3 + 1] := YVal;  // G
        RowPtr[ImgX * 3 + 2] := YVal;  // R
      end
      else if FNumComponents = 3 then
      begin
        // YCbCr → RGB
        YVal := CompBuf[0, PY * CompW[0] + PX];
        CbX := (PX * FComponents[1].HSampFactor) div FMaxH;
        CbY := (PY * FComponents[1].VSampFactor) div FMaxV;
        CbVal := CompBuf[1, CbY * CompW[1] + CbX];
        CrVal := CompBuf[2, CbY * CompW[2] + CbX];

        R := YVal + Round(1.402 * (CrVal - 128));
        G := YVal - Round(0.34414 * (CbVal - 128)) -
             Round(0.71414 * (CrVal - 128));
        B := YVal + Round(1.772 * (CbVal - 128));
        if R < 0 then R := 0;
        if R > 255 then R := 255;
        if G < 0 then G := 0;
        if G > 255 then G := 255;
        if B < 0 then B := 0;
        if B > 255 then B := 255;

        RowPtr[ImgX * 3]     := Byte(B);  // B
        RowPtr[ImgX * 3 + 1] := Byte(G);  // G
        RowPtr[ImgX * 3 + 2] := Byte(R);  // R
      end
      else if FNumComponents = 4 then
      begin
        // CMYK → RGB (Adobe 公式)
        YVal := CompBuf[0, PY * CompW[0] + PX];
        CbVal := CompBuf[1, PY * CompW[1] + PX];
        CrVal := CompBuf[2, PY * CompW[2] + PX];
        AVal := CompBuf[3, PY * CompW[3] + PX];  // K
        if FAdobeTransform = 2 then
        begin
          // YCCK: 先 YCbCr→RGB，再乘以 K
          R := YVal + Round(1.402 * (CrVal - 128));
          G := YVal - Round(0.34414 * (CbVal - 128)) -
               Round(0.71414 * (CrVal - 128));
          B := YVal + Round(1.772 * (CbVal - 128));
          R := (R * AVal) div 255;
          G := (G * AVal) div 255;
          B := (B * AVal) div 255;
        end
        else
        begin
          // Inverted CMYK (transform=0 或无 APP14)
          R := (255 - YVal) * AVal div 255;
          G := (255 - CbVal) * AVal div 255;
          B := (255 - CrVal) * AVal div 255;
        end;
        if R < 0 then R := 0;
        if R > 255 then R := 255;
        if G < 0 then G := 0;
        if G > 255 then G := 255;
        if B < 0 then B := 0;
        if B > 255 then B := 255;
        RowPtr[ImgX * 3]     := Byte(B);
        RowPtr[ImgX * 3 + 1] := Byte(G);
        RowPtr[ImgX * 3 + 2] := Byte(R);
      end;
    end;
  end;
end;

procedure TCnJPEGDecoder.DecodeScanBaseline(OutBmp: TBitmap);
var
  MCUX, MCUY: Integer;
  MCUCount: Integer;
  TotalMCUs: Integer;
  I: Integer;
begin
  MCUCount := 0;
  TotalMCUs := FMCUsPerRow * FMCUsPerCol;

  for MCUY := 0 to FMCUsPerCol - 1 do
  begin
    for MCUX := 0 to FMCUsPerRow - 1 do
    begin
      DecodeMCU(MCUX, MCUY, OutBmp);
      Inc(MCUCount);

      // 重启间隔处理
      if (FRestartInterval > 0) and
         (MCUCount mod FRestartInterval = 0) and
         (MCUCount < TotalMCUs) then
      begin
        // 对齐到字节边界（丢弃填充位）
        FBitReader.AlignToByte;
        // 重置 DC 预测值
        for I := 0 to FNumComponents - 1 do
          FPrevDC[I] := 0;
        // 清除 RST 标记
        FBitReader.ClearMarker;
      end;
    end;
  end;
end;

procedure TCnJPEGDecoder.AllocCoefBlocks;
var
  I, TotalBlocks: Integer;
begin
  // 先初始化一维数组，再清空每个二维子数组，确保从干净状态开始
  SetLength(FCoefBlocks, CN_JPEG_MAX_COMPONENTS);
  for I := 0 to CN_JPEG_MAX_COMPONENTS - 1 do
  begin
    FCompBlocksPerRow[I] := 0;
    FCompBlocksPerCol[I] := 0;
    SetLength(FCoefBlocks[I], 0);
  end;

  for I := 0 to FNumComponents - 1 do
  begin
    FCompBlocksPerRow[I] := FMCUsPerRow * FComponents[I].HSampFactor;
    FCompBlocksPerCol[I] := FMCUsPerCol * FComponents[I].VSampFactor;
    TotalBlocks := FCompBlocksPerRow[I] * FCompBlocksPerCol[I];
    SetLength(FCoefBlocks[I], TotalBlocks * 64);
    FillChar(FCoefBlocks[I][0], TotalBlocks * 64 * SizeOf(SmallInt), 0);
  end;
end;

function TCnJPEGDecoder.GetCoefBlockPtr(CompIdx, BlockX, BlockY: Integer): PSmallInt;
var
  BlockIdx: Integer;
begin
  BlockIdx := BlockY * FCompBlocksPerRow[CompIdx] + BlockX;
  Result := @FCoefBlocks[CompIdx][BlockIdx * 64];
end;

procedure TCnJPEGDecoder.DecodeDCScanProgressive;
var
  MCUX, MCUY: Integer;
  CompIdx, ScanIdx: Integer;
  BX, BY: Integer;
  S: Byte;
  Diff, Val: Integer;
  CoefPtr: PSmallInt;
  DCTable: PCnJPEGHuffmanTable;
  MCUCount, TotalMCUs: Integer;
begin
  MCUCount := 0;
  TotalMCUs := FMCUsPerRow * FMCUsPerCol;

  for MCUY := 0 to FMCUsPerCol - 1 do
  begin
    for MCUX := 0 to FMCUsPerRow - 1 do
    begin
      for ScanIdx := 0 to FScanCompCount - 1 do
      begin
        CompIdx := FScanCompIdx[ScanIdx];
        if CompIdx < 0 then Continue;
        DCTable := @FDCTables[FComponents[CompIdx].DCTableID];

        for BY := 0 to FComponents[CompIdx].VSampFactor - 1 do
          for BX := 0 to FComponents[CompIdx].HSampFactor - 1 do
          begin
            CoefPtr := GetCoefBlockPtr(CompIdx,
              MCUX * FComponents[CompIdx].HSampFactor + BX,
              MCUY * FComponents[CompIdx].VSampFactor + BY);

            if FScanAh = 0 then
            begin
              // 首次扫描：Huffman 解码 DC 差分值
              S := HuffmanDecode(DCTable^);
              if S = 0 then
                Diff := 0
              else
              begin
                Diff := FBitReader.GetBits(S);
                if Diff < (1 shl (S - 1)) then
                  Diff := Diff - (1 shl S) + 1;
              end;
              Inc(FPrevDC[CompIdx], Diff);
              CoefPtr^ := SmallInt(FPrevDC[CompIdx] shl FScanAl);
            end
            else
            begin
              // 精化扫描：读取 1 位修正 DC 系数
              Val := FBitReader.GetBit;
              if Val = 1 then
                CoefPtr^ := CoefPtr^ or SmallInt(1 shl FScanAl);
            end;
          end;
      end;

      Inc(MCUCount);
      if (FRestartInterval > 0) and
         (MCUCount mod FRestartInterval = 0) and
         (MCUCount < TotalMCUs) then
      begin
        FBitReader.AlignToByte;
        for ScanIdx := 0 to FNumComponents - 1 do
          FPrevDC[ScanIdx] := 0;
        FBitReader.ClearMarker;
      end;
    end;
  end;
end;

procedure TCnJPEGDecoder.DecodeACScanProgressive;
var
  BlockX, BlockY: Integer;
  CompIdx: Integer;
  K: Integer;
  RS: Byte;
  Run, Size: Byte;
  Val: Integer;
  CoefPtr: PSmallIntArray;
  ACTable: PCnJPEGHuffmanTable;
  BlocksPerRow, BlocksPerCol: Integer;
  BlockCount, TotalBlocks: Integer;
  EOBRun: Integer;
begin
  CompIdx := FScanCompIdx[0];  // AC 扫描为非交错（单分量）
  if CompIdx < 0 then Exit;
  ACTable := @FACTables[FComponents[CompIdx].ACTableID];

  BlocksPerRow := FCompBlocksPerRow[CompIdx];
  BlocksPerCol := FCompBlocksPerCol[CompIdx];
  TotalBlocks := BlocksPerRow * BlocksPerCol;

  BlockCount := 0;
  EOBRun := 0;

  for BlockY := 0 to BlocksPerCol - 1 do
  begin
    for BlockX := 0 to BlocksPerRow - 1 do
    begin
      CoefPtr := PSmallIntArray(GetCoefBlockPtr(CompIdx, BlockX, BlockY));

      if FScanAh = 0 then
      begin
        // 首次 AC 扫描
        if EOBRun > 0 then
          Dec(EOBRun)  // EOB 游程跳过当前块
        else
        begin
          K := FScanSs;
          while K <= FScanSe do
          begin
            RS := HuffmanDecode(ACTable^);
            Run := RS shr 4;
            Size := RS and $0F;

            if Size = 0 then
            begin
              if Run = 0 then
              begin
                // EOB：计算 EOB 游程
                EOBRun := 1;
                // 尝试读取更多位确定 EOB 游程长度
                while True do
                begin
                  Val := FBitReader.GetBit;
                  if Val = 1 then
                  begin
                    EOBRun := (EOBRun shl 1) or 1;
                    if EOBRun >= 32768 then Break;
                  end
                  else
                  begin
                    EOBRun := (EOBRun shl 1);
                    if EOBRun >= 1 then
                    begin
                      EOBRun := EOBRun + FBitReader.GetBits(1);
                      Break;
                    end;
                  end;
                end;
                Dec(EOBRun);  // 当前块已算 1 个
                Break;
              end
              else if Run = 15 then
                Inc(K, 16)  // ZRL
              else
                Break;
              Continue;
            end;

            Inc(K, Run);
            if K > FScanSe then Break;

            Val := FBitReader.GetBits(Size);
            if Val < (1 shl (Size - 1)) then
              Val := Val - (1 shl Size) + 1;

            CoefPtr[CN_JPEG_ZIGZAG_ORDER[K]] := SmallInt(Val shl FScanAl);
            Inc(K);
          end;
        end;
      end
      else
      begin
        // AC 精化扫描
        if EOBRun > 0 then
          Dec(EOBRun)
        else
        begin
          K := FScanSs;
          while K <= FScanSe do
          begin
            RS := HuffmanDecode(ACTable^);
            Run := RS shr 4;
            Size := RS and $0F;

            if Size = 0 then
            begin
              if Run = 0 then
              begin
                // EOB 游程
                EOBRun := 1;
                while True do
                begin
                  Val := FBitReader.GetBit;
                  if Val = 1 then
                  begin
                    EOBRun := (EOBRun shl 1) or 1;
                    if EOBRun >= 32768 then Break;
                  end
                  else
                  begin
                    EOBRun := (EOBRun shl 1);
                    if EOBRun >= 1 then
                    begin
                      EOBRun := EOBRun + FBitReader.GetBits(1);
                      Break;
                    end;
                  end;
                end;
                Dec(EOBRun);
                Break;
              end
              else if Run = 15 then
              begin
                // ZRL：跳过 16 个零系数
                Inc(K, 16);
              end
              else
                Break;
              Continue;
            end;

            // Size=1：跳过 Run 个零，精化下一个系数
            Inc(K, Run);
            if K > FScanSe then Break;

            Val := FBitReader.GetBit;
            if Val = 1 then
            begin
              if CoefPtr[CN_JPEG_ZIGZAG_ORDER[K]] = 0 then
              begin
                // 新出现的非零系数：正值 + (1<<Al)
                CoefPtr[CN_JPEG_ZIGZAG_ORDER[K]] := SmallInt(1 shl FScanAl);
              end
              else
              begin
                // 已有非零系数：设置第 Al 位
                CoefPtr[CN_JPEG_ZIGZAG_ORDER[K]] :=
                  CoefPtr[CN_JPEG_ZIGZAG_ORDER[K]] or SmallInt(1 shl FScanAl);
              end;
            end;
            Inc(K);
          end;
        end;
      end;

      Inc(BlockCount);
      if (FRestartInterval > 0) and
         (BlockCount mod FRestartInterval = 0) and
         (BlockCount < TotalBlocks) then
      begin
        FBitReader.AlignToByte;
        FBitReader.ClearMarker;
        EOBRun := 0;
      end;
    end;
  end;
end;

procedure TCnJPEGDecoder.FinalizeProgressive(OutBmp: TBitmap);
var
  CompIdx, BX, BY, PX, PY: Integer;
  BlockX, BlockY: Integer;
  Coef: array[0..63] of SmallInt;
  Pixels: array[0..63] of Byte;
  CompBuf: array[0..3, 0..255] of Byte;
  CompW, CompH: array[0..3] of Integer;
  MCUX, MCUY: Integer;
  OutX, OutY: Integer;
  MaxPX, MaxPY: Integer;
  YVal, CbVal, CrVal: Byte;
  CbX, CbY: Integer;
  R, G, B: Integer;
  RowPtr: PByteArray;
  ImgX, ImgY: Integer;
  Val: Byte;
begin
  // 对每个 MCU 执行反量化 + IDCT + 色彩转换
  for MCUY := 0 to FMCUsPerCol - 1 do
  begin
    for MCUX := 0 to FMCUsPerRow - 1 do
    begin
      FillChar(CompBuf[0, 0], SizeOf(CompBuf), 0);

      for CompIdx := 0 to FNumComponents - 1 do
      begin
        CompW[CompIdx] := 8 * FComponents[CompIdx].HSampFactor;
        CompH[CompIdx] := 8 * FComponents[CompIdx].VSampFactor;

        for BY := 0 to FComponents[CompIdx].VSampFactor - 1 do
          for BX := 0 to FComponents[CompIdx].HSampFactor - 1 do
          begin
            BlockX := MCUX * FComponents[CompIdx].HSampFactor + BX;
            BlockY := MCUY * FComponents[CompIdx].VSampFactor + BY;

            // 从系数块存储中取出系数
            Move(GetCoefBlockPtr(CompIdx, BlockX, BlockY)^,
              Coef[0], 128);

            DequantizeAndIDCT(Coef, FComponents[CompIdx].QuantTableID,
              Pixels, CompIdx, FScale);

            for PY := 0 to 7 do
              for PX := 0 to 7 do
                CompBuf[CompIdx, (BY * 8 + PY) * CompW[CompIdx] + (BX * 8 + PX)] :=
                  Pixels[PY * 8 + PX];
          end;
      end;

      // 上采样 + 色彩转换 + 写入位图（与 DecodeMCU 相同逻辑）
      MaxPX := FMCUWidth;
      MaxPY := FMCUHeight;
      if MCUX = FMCUsPerRow - 1 then
        MaxPX := FWidth - MCUX * FMCUWidth;
      if MCUY = FMCUsPerCol - 1 then
        MaxPY := FHeight - MCUY * FMCUHeight;

      OutX := MCUX * FMCUWidth;
      OutY := MCUY * FMCUHeight;

      for PY := 0 to MaxPY - 1 do
      begin
        ImgY := OutY + PY;
        if ImgY >= FHeight then Break;
        if ImgY >= OutBmp.Height then Break;

        RowPtr := OutBmp.ScanLine[ImgY];

        for PX := 0 to MaxPX - 1 do
        begin
          ImgX := OutX + PX;
          if ImgX >= FWidth then Break;
          if ImgX >= OutBmp.Width then Break;

          if FNumComponents = 1 then
          begin
            YVal := CompBuf[0, PY * CompW[0] + PX];
            RowPtr[ImgX * 3]     := YVal;
            RowPtr[ImgX * 3 + 1] := YVal;
            RowPtr[ImgX * 3 + 2] := YVal;
          end
          else if FNumComponents = 3 then
          begin
            YVal := CompBuf[0, PY * CompW[0] + PX];
            CbX := (PX * FComponents[1].HSampFactor) div FMaxH;
            CbY := (PY * FComponents[1].VSampFactor) div FMaxV;
            CbVal := CompBuf[1, CbY * CompW[1] + CbX];
            CrVal := CompBuf[2, CbY * CompW[2] + CbX];

            R := YVal + Round(1.402 * (CrVal - 128));
            G := YVal - Round(0.34414 * (CbVal - 128)) -
                 Round(0.71414 * (CrVal - 128));
            B := YVal + Round(1.772 * (CbVal - 128));
            if R < 0 then R := 0;
            if R > 255 then R := 255;
            if G < 0 then G := 0;
            if G > 255 then G := 255;
            if B < 0 then B := 0;
            if B > 255 then B := 255;

            RowPtr[ImgX * 3]     := Byte(B);
            RowPtr[ImgX * 3 + 1] := Byte(G);
            RowPtr[ImgX * 3 + 2] := Byte(R);
          end
          else if FNumComponents = 4 then
          begin
            // CMYK → RGB (Adobe 公式)
            YVal := CompBuf[0, PY * CompW[0] + PX];
            CbVal := CompBuf[1, PY * CompW[1] + PX];
            CrVal := CompBuf[2, PY * CompW[2] + PX];
            Val := CompBuf[3, PY * CompW[3] + PX];  // K
            if FAdobeTransform = 2 then
            begin
              // YCCK: 先 YCbCr→RGB，再乘以 K
              R := YVal + Round(1.402 * (CrVal - 128));
              G := YVal - Round(0.34414 * (CbVal - 128)) -
                   Round(0.71414 * (CrVal - 128));
              B := YVal + Round(1.772 * (CbVal - 128));
              R := (R * Val) div 255;
              G := (G * Val) div 255;
              B := (B * Val) div 255;
            end
            else
            begin
              // Inverted CMYK (transform=0 或无 APP14)
              R := (255 - YVal) * Val div 255;
              G := (255 - CbVal) * Val div 255;
              B := (255 - CrVal) * Val div 255;
            end;
            if R < 0 then R := 0;
            if R > 255 then R := 255;
            if G < 0 then G := 0;
            if G > 255 then G := 255;
            if B < 0 then B := 0;
            if B > 255 then B := 255;
            RowPtr[ImgX * 3]     := Byte(B);
            RowPtr[ImgX * 3 + 1] := Byte(G);
            RowPtr[ImgX * 3 + 2] := Byte(R);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCnJPEGDecoder.DecodeScanProgressive(OutBmp: TBitmap;
  Stream: TStream; Marker: TCnJPEGMarker);
var
  Code: Word;
  SegLen: Word;
  SegEnd: Int64;
  HitMarker: Word;
begin
  // 预分配系数块存储
  AllocCoefBlocks;

  // 渐进式多扫描循环（首个 SOS 已在 Decode 中解析）
  repeat
    // 初始化 DC 预测值
    FillChar(FPrevDC[0], SizeOf(FPrevDC), 0);

    // 创建位读取器
    if FBitReader <> nil then
      FBitReader.Free;
    FBitReader := TCnJPEGBitReader.Create(Stream);

    // 解码当前扫描
    if FScanSs = 0 then
      DecodeDCScanProgressive
    else
      DecodeACScanProgressive;

    // 释放位读取器
    HitMarker := FBitReader.HitMarker;
    if not FBitReader.IsEOF then
      FBitReader.AlignToByte;
    FBitReader.Free;
    FBitReader := nil;

    // 确定下一个 marker
    if HitMarker <> 0 then
    begin
      // BitReader 遇到了 marker，流位置在 marker 代码之后
      // 回退 2 字节重新读取 marker
      Stream.Seek(-2, soFromCurrent);
      Code := Marker.ReadMarker;
    end
    else
    begin
      // 从流中读取下一个 marker
      Code := Marker.ReadMarker;
    end;

    if Code = CN_JPEG_EOI then Break;

    // 处理扫描间的 marker
    SegLen := Marker.ReadWord;
    SegEnd := Marker.Position + SegLen - 2;
    while True do
    begin
      if Code = CN_JPEG_SOS then
      begin
        ParseSOS(Marker, SegLen);
        Break;  // 继续下一次扫描
      end
      else if Code = CN_JPEG_DHT then
      begin
        ParseDHT(Marker, SegLen);
      end
      else if Code = CN_JPEG_DRI then
      begin
        ParseDRI(Marker, SegLen);
      end
      else
      begin
        // 跳过未知段
        if SegLen >= 2 then
          Marker.SkipBytes(SegLen - 2);
      end;

      if Marker.Position < SegEnd then
        Marker.Seek(SegEnd);

      Code := Marker.ReadMarker;
      if Code = CN_JPEG_EOI then Break;
      SegLen := Marker.ReadWord;
      SegEnd := Marker.Position + SegLen - 2;
    end;

    if Code = CN_JPEG_EOI then Break;
  until False;

  // 所有扫描完成：反量化 + IDCT + 色彩转换
  FinalizeProgressive(OutBmp);
end;

procedure TCnJPEGDecoder.UpsampleAndConvert(OutBmp: TBitmap);
begin
  // 已内联在 DecodeMCU 中
end;

procedure TCnJPEGDecoder.InitYCbCrTable;
var
  Cb, Cr: Integer;
  R, G, B: Integer;
begin
  for Cb := 0 to 255 do
    for Cr := 0 to 255 do
    begin
      R := Round(1.402 * (Cr - 128));
      G := -Round(0.34414 * (Cb - 128)) - Round(0.71414 * (Cr - 128));
      B := Round(1.772 * (Cb - 128));
      FYCbCrTable[Cb, Cr] := Cardinal((B and $FF) or
        ((G and $FF) shl 8) or ((R and $FF) shl 16));
    end;
  FYCbCrTableInit := True;
end;

procedure TCnJPEGDecoder.ApplySmoothing(OutBmp: TBitmap);
var
  X, Y: Integer;
  Row0, Row1, Row2: PByteArray;
  BmpW, BmpH: Integer;
  R, G, B: Integer;
  TmpBmp: TBitmap;
  DstRow: PByteArray;
begin
  if not FSmoothing then Exit;
  if FScale <> jsFullSize then Exit;

  BmpW := OutBmp.Width;
  BmpH := OutBmp.Height;
  if (BmpW < 3) or (BmpH < 3) then Exit;

  TmpBmp := TBitmap.Create;
  try
    TmpBmp.PixelFormat := pf24bit;
    TmpBmp.Width := BmpW;
    TmpBmp.Height := BmpH;
    TmpBmp.Canvas.Draw(0, 0, OutBmp);

    for Y := 1 to BmpH - 2 do
    begin
      Row0 := TmpBmp.ScanLine[Y - 1];
      Row1 := TmpBmp.ScanLine[Y];
      Row2 := TmpBmp.ScanLine[Y + 1];
      DstRow := OutBmp.ScanLine[Y];

      for X := 1 to BmpW - 2 do
      begin
        // 简单 3x3 平均滤波（仅在块边界处）
        if ((X mod 8 = 0) or (Y mod 8 = 0)) then
        begin
          B := (Row0[(X-1)*3] + Row0[X*3] + Row0[(X+1)*3] +
                Row1[(X-1)*3] + Row1[X*3] + Row1[(X+1)*3] +
                Row2[(X-1)*3] + Row2[X*3] + Row2[(X+1)*3]) div 9;
          G := (Row0[(X-1)*3+1] + Row0[X*3+1] + Row0[(X+1)*3+1] +
                Row1[(X-1)*3+1] + Row1[X*3+1] + Row1[(X+1)*3+1] +
                Row2[(X-1)*3+1] + Row2[X*3+1] + Row2[(X+1)*3+1]) div 9;
          R := (Row0[(X-1)*3+2] + Row0[X*3+2] + Row0[(X+1)*3+2] +
                Row1[(X-1)*3+2] + Row1[X*3+2] + Row1[(X+1)*3+2] +
                Row2[(X-1)*3+2] + Row2[X*3+2] + Row2[(X+1)*3+2]) div 9;
          DstRow[X*3] := Byte(B);
          DstRow[X*3+1] := Byte(G);
          DstRow[X*3+2] := Byte(R);
        end;
      end;
    end;
  finally
    TmpBmp.Free;
  end;
end;

procedure TCnJPEGDecoder.Decode(Stream: TStream; OutBmp: TBitmap;
  AScale: TCnJPEGScale; APerformance: TCnJPEGPerformance;
  ASmoothing: Boolean; AProgressiveDisplay: Boolean);
var
  Marker: TCnJPEGMarker;
  Code: Word;
  SegLen: Word;
  SegEnd: Int64;
  I: Integer;
begin
  FScale := AScale;
  FPerformance := APerformance;
  FSmoothing := ASmoothing;
  FProgressiveDisplay := AProgressiveDisplay;
  FRestartInterval := 0;

  Stream.Position := 0;
  Marker := TCnJPEGMarker.Create(Stream);
  try
    // 1. 读取 SOI
    Code := Marker.ReadMarker;
    if Code <> CN_JPEG_SOI then
      raise ECnJPEGException.Create(SCnErrorInvalidJpegMissingSoi);

    // 2. 循环读取 marker
    repeat
      Code := Marker.ReadMarker;

      // 无段 marker
      if Code = CN_JPEG_EOI then
        Break;
      if Code = CN_JPEG_TEM then
        Continue;
      if (Code >= CN_JPEG_RST0) and (Code <= CN_JPEG_RST7) then
        Continue;

      // 读取段长度
      SegLen := Marker.ReadWord;
      SegEnd := Marker.Position + SegLen - 2;

      case Code of
        CN_JPEG_APP0:
          ParseAPP0(Marker, SegLen);
        CN_JPEG_APP14:
          ParseAPP14(Marker, SegLen);
        CN_JPEG_DQT:
          ParseDQT(Marker, SegLen);
        CN_JPEG_DHT:
          ParseDHT(Marker, SegLen);
        CN_JPEG_DRI:
          ParseDRI(Marker, SegLen);
        CN_JPEG_SOF0, CN_JPEG_SOF1:
          ParseSOF(Marker, SegLen, Code);
        CN_JPEG_SOF2:
          ParseSOF(Marker, SegLen, Code);
        CN_JPEG_SOS:
          begin
            ParseSOS(Marker, SegLen);
            Break;
          end;
      else
        begin
          // 跳过未知段
          if SegLen >= 2 then
            Marker.SkipBytes(SegLen - 2);
        end;
      end;

      // Safety net: skip any unconsumed segment data
      if Marker.Position < SegEnd then
        Marker.Seek(SegEnd);
    until False;

    // 3. 计算 MCU 参数
    CalculateMCUParams;

    // 4. 设置输出位图
    OutBmp.PixelFormat := pf24bit;
    OutBmp.Width := ScaledOutWidth;
    OutBmp.Height := ScaledOutHeight;

    // 5. 初始化 DC 预测值
    for I := 0 to 3 do
      FPrevDC[I] := 0;

    // 初始化 YCbCr 查找表
    if not FYCbCrTableInit then
      InitYCbCrTable;

    // 6. 解码扫描数据
    if FProgressive then
      DecodeScanProgressive(OutBmp, Stream, Marker)
    else
    begin
      // Baseline：创建位读取器并解码
      FBitReader := TCnJPEGBitReader.Create(Stream);
      try
        DecodeScanBaseline(OutBmp);
      finally
        FBitReader.Free;
        FBitReader := nil;
      end;
    end;

    // 7. 平滑处理
    ApplySmoothing(OutBmp);
  finally
    Marker.Free;
  end;
end;

//============================================================================
// TCnJPEGEncoder
//============================================================================

constructor TCnJPEGEncoder.Create;
begin
  inherited Create;
  FQuality := 75;
  FGrayscale := False;
  FProgressive := False;
  FStream := nil;
  FBitWriter := nil;
end;

destructor TCnJPEGEncoder.Destroy;
begin
  if FBitWriter <> nil then
    FBitWriter.Free;
  inherited Destroy;
end;

procedure TCnJPEGEncoder.BuildQuantTables(Quality: Integer);
var
  I: Integer;
  Scale: Double;
  Q: Integer;
begin
  if Quality < 1 then Quality := 1;
  if Quality > 100 then Quality := 100;
  if Quality < 50 then
    Scale := 5000.0 / Quality
  else
    Scale := 200.0 - Quality * 2.0;

  // 亮度量化表（自然顺序存储）
  for I := 0 to 63 do
  begin
    Q := Round(CN_JPEG_STD_LUMINANCE_QUANT[I] * Scale / 100.0);
    if Q < 1 then Q := 1;
    if Q > 255 then Q := 255;
    FQuantTables[0, CN_JPEG_ZIGZAG_ORDER[I]] := Q;
  end;

  // 色度量化表
  for I := 0 to 63 do
  begin
    Q := Round(CN_JPEG_STD_CHROMINANCE_QUANT[I] * Scale / 100.0);
    if Q < 1 then Q := 1;
    if Q > 255 then Q := 255;
    FQuantTables[1, CN_JPEG_ZIGZAG_ORDER[I]] := Q;
  end;
end;

procedure TCnJPEGEncoder.BuildEncLookup(const Bits: array of Byte;
  const HuffVal: array of Byte; NumVals: Integer;
  var EncCode: array of Cardinal; var EncSize: array of Byte);
var
  I, K: Integer;
  Code: Cardinal;
  L: Integer;
begin
  for I := 0 to 255 do
  begin
    EncCode[I] := 0;
    EncSize[I] := 0;
  end;

  Code := 0;
  K := 0;
  for L := 1 to 16 do
  begin
    for I := 1 to Bits[L - 1] do  // 开放数组 0 基，原数组 1 基
    begin
      EncCode[HuffVal[K]] := Code;
      EncSize[HuffVal[K]] := L;
      Inc(Code);
      Inc(K);
      if K >= NumVals then Break;
    end;
    if K >= NumVals then Break;
    Code := Code shl 1;
  end;
end;

procedure TCnJPEGEncoder.BuildStandardHuffmanTables;
var
  I: Integer;
begin
  // 复制标准 BITS/HUFFVAL 到解码表结构
  for I := 1 to 16 do
  begin
    FDCTables[0].Bits[I] := CN_JPEG_STD_DC_LUMINANCE_BITS[I];
    FDCTables[1].Bits[I] := CN_JPEG_STD_DC_CHROMINANCE_BITS[I];
    FACTables[0].Bits[I] := CN_JPEG_STD_AC_LUMINANCE_BITS[I];
    FACTables[1].Bits[I] := CN_JPEG_STD_AC_CHROMINANCE_BITS[I];
  end;
  for I := 0 to 11 do
  begin
    FDCTables[0].HuffVal[I] := CN_JPEG_STD_DC_LUMINANCE_VAL[I];
    FDCTables[1].HuffVal[I] := CN_JPEG_STD_DC_CHROMINANCE_VAL[I];
  end;
  for I := 0 to 161 do
  begin
    FACTables[0].HuffVal[I] := CN_JPEG_STD_AC_LUMINANCE_VAL[I];
    FACTables[1].HuffVal[I] := CN_JPEG_STD_AC_CHROMINANCE_VAL[I];
  end;

  // 构建编码查找表
  BuildEncLookup(CN_JPEG_STD_DC_LUMINANCE_BITS,
    CN_JPEG_STD_DC_LUMINANCE_VAL, 12,
    FDCEncCode[0], FDCEncSize[0]);
  BuildEncLookup(CN_JPEG_STD_DC_CHROMINANCE_BITS,
    CN_JPEG_STD_DC_CHROMINANCE_VAL, 12,
    FDCEncCode[1], FDCEncSize[1]);
  BuildEncLookup(CN_JPEG_STD_AC_LUMINANCE_BITS,
    CN_JPEG_STD_AC_LUMINANCE_VAL, 162,
    FACEncCode[0], FACEncSize[0]);
  BuildEncLookup(CN_JPEG_STD_AC_CHROMINANCE_BITS,
    CN_JPEG_STD_AC_CHROMINANCE_VAL, 162,
    FACEncCode[1], FACEncSize[1]);
end;

procedure TCnJPEGEncoder.WriteWord(W: Word);
begin
  W := UInt16ToBigEndian(W);
  FStream.Write(W, 2);
end;

procedure TCnJPEGEncoder.WriteSOI;
var
  W: Word;
begin
  W := CN_JPEG_SOI;
  WriteWord(W);
end;

procedure TCnJPEGEncoder.WriteAPP0;
var
  W: Word;
  B: Byte;
  Ident: array[0..4] of AnsiChar;
begin
  W := CN_JPEG_APP0;
  WriteWord(W);
  W := 16;  // 段长度
  WriteWord(W);
  Ident := 'JFIF';
  FStream.Write(Ident[0], 5);  // "JFIF\0"
  W := $0101;  // 版本 1.1
  WriteWord(W);
  B := 0;  // 密度单位（无单位）
  FStream.Write(B, 1);
  W := 1;  // X 密度
  WriteWord(W);
  W := 1;  // Y 密度
  WriteWord(W);
  B := 0;  // 缩略图宽度
  FStream.Write(B, 1);
  B := 0;  // 缩略图高度
  FStream.Write(B, 1);
end;

procedure TCnJPEGEncoder.WriteDQT;
var
  W: Word;
  B: Byte;
  I, T: Integer;
begin
  for T := 0 to 1 do
  begin
    if FGrayscale and (T = 1) then Break;  // 灰度模式只写亮度表
    W := CN_JPEG_DQT;
    WriteWord(W);
    W := 2 + 1 + 64;  // 段长度
    WriteWord(W);
    B := T;  // 精度=0 (8bit), 表 ID=T
    FStream.Write(B, 1);
    // 按 ZigZag 顺序写入量化表
    for I := 0 to 63 do
    begin
      B := Byte(FQuantTables[T, CN_JPEG_ZIGZAG_ORDER[I]]);
      FStream.Write(B, 1);
    end;
  end;
end;

procedure TCnJPEGEncoder.WriteSOF0;
var
  W: Word;
  B: Byte;
begin
  W := CN_JPEG_SOF0;
  WriteWord(W);
  if FGrayscale then
    W := 2 + 1 + 2 + 2 + 1 + 1 * 3  // 1 分量
  else
    W := 2 + 1 + 2 + 2 + 1 + 3 * 3;  // 3 分量
  WriteWord(W);
  B := 8;  // 样本精度
  FStream.Write(B, 1);
  W := FHeight;
  WriteWord(W);
  W := FWidth;
  WriteWord(W);
  if FGrayscale then
  begin
    B := 1;  // 分量数
    FStream.Write(B, 1);
    B := 1;  // 分量 ID
    FStream.Write(B, 1);
    B := $11;  // H=1, V=1
    FStream.Write(B, 1);
    B := 0;  // 量化表 ID
    FStream.Write(B, 1);
  end
  else
  begin
    B := 3;  // 分量数
    FStream.Write(B, 1);
    // Y
    B := 1; FStream.Write(B, 1);  // ID
    B := $22; FStream.Write(B, 1);  // H=2, V=2
    B := 0; FStream.Write(B, 1);  // 量化表 0
    // Cb
    B := 2; FStream.Write(B, 1);
    B := $11; FStream.Write(B, 1);  // H=1, V=1
    B := 1; FStream.Write(B, 1);  // 量化表 1
    // Cr
    B := 3; FStream.Write(B, 1);
    B := $11; FStream.Write(B, 1);
    B := 1; FStream.Write(B, 1);
  end;
end;

procedure TCnJPEGEncoder.WriteSOF2;
var
  W: Word;
  B: Byte;
begin
  W := CN_JPEG_SOF2;
  WriteWord(W);
  if FGrayscale then
    W := 2 + 1 + 2 + 2 + 1 + 1 * 3  // 1 分量
  else
    W := 2 + 1 + 2 + 2 + 1 + 3 * 3;  // 3 分量
  WriteWord(W);
  B := 8;  // 样本精度
  FStream.Write(B, 1);
  W := FHeight;
  WriteWord(W);
  W := FWidth;
  WriteWord(W);
  if FGrayscale then
  begin
    B := 1;  // 分量数
    FStream.Write(B, 1);
    B := 1;  // 分量 ID
    FStream.Write(B, 1);
    B := $11;  // H=1, V=1
    FStream.Write(B, 1);
    B := 0;  // 量化表 ID
    FStream.Write(B, 1);
  end
  else
  begin
    B := 3;  // 分量数
    FStream.Write(B, 1);
    // Y
    B := 1; FStream.Write(B, 1);  // ID
    B := $22; FStream.Write(B, 1);  // H=2, V=2
    B := 0; FStream.Write(B, 1);  // 量化表 0
    // Cb
    B := 2; FStream.Write(B, 1);
    B := $11; FStream.Write(B, 1);  // H=1, V=1
    B := 1; FStream.Write(B, 1);  // 量化表 1
    // Cr
    B := 3; FStream.Write(B, 1);
    B := $11; FStream.Write(B, 1);
    B := 1; FStream.Write(B, 1);
  end;
end;

procedure TCnJPEGEncoder.WriteDHT;
var
  W: Word;
  B: Byte;
  I, T: Integer;
  Total: Integer;
begin
  for T := 0 to 1 do
  begin
    // DC 表
    Total := 0;
    for I := 1 to 16 do
      Inc(Total, FDCTables[T].Bits[I]);
    W := CN_JPEG_DHT;
    WriteWord(W);
    W := 2 + 1 + 16 + Total;
    WriteWord(W);
    B := T;  // DC 表, ID=T
    FStream.Write(B, 1);
    for I := 1 to 16 do
    begin
      B := FDCTables[T].Bits[I];
      FStream.Write(B, 1);
    end;
    for I := 0 to Total - 1 do
    begin
      B := FDCTables[T].HuffVal[I];
      FStream.Write(B, 1);
    end;

    // AC 表
    Total := 0;
    for I := 1 to 16 do
      Inc(Total, FACTables[T].Bits[I]);
    W := CN_JPEG_DHT;
    WriteWord(W);
    W := 2 + 1 + 16 + Total;
    WriteWord(W);
    B := $10 or T;  // AC 表, ID=T
    FStream.Write(B, 1);
    for I := 1 to 16 do
    begin
      B := FACTables[T].Bits[I];
      FStream.Write(B, 1);
    end;
    for I := 0 to Total - 1 do
    begin
      B := FACTables[T].HuffVal[I];
      FStream.Write(B, 1);
    end;

    if FGrayscale and (T = 0) then Break;  // 灰度只需一套表
  end;
end;

procedure TCnJPEGEncoder.WriteSOS;
var
  W: Word;
  B: Byte;
begin
  W := CN_JPEG_SOS;
  WriteWord(W);
  if FGrayscale then
    W := 2 + 1 + 1 * 2 + 3
  else
    W := 2 + 1 + 3 * 2 + 3;
  WriteWord(W);

  if FGrayscale then
  begin
    B := 1; FStream.Write(B, 1);  // 分量数
    B := 1; FStream.Write(B, 1);  // 分量 ID
    B := $00; FStream.Write(B, 1);  // DC=0, AC=0
  end
  else
  begin
    B := 3; FStream.Write(B, 1);  // 分量数
    B := 1; FStream.Write(B, 1); B := $00; FStream.Write(B, 1);  // Y: DC=0, AC=0
    B := 2; FStream.Write(B, 1); B := $11; FStream.Write(B, 1);  // Cb: DC=1, AC=1
    B := 3; FStream.Write(B, 1); B := $11; FStream.Write(B, 1);  // Cr: DC=1, AC=1
  end;

  B := 0; FStream.Write(B, 1);  // Ss
  B := 63; FStream.Write(B, 1);  // Se
  B := 0; FStream.Write(B, 1);  // Ah=0, Al=0
end;

procedure TCnJPEGEncoder.WriteEOI;
var
  W: Word;
begin
  W := CN_JPEG_EOI;
  WriteWord(W);
end;

procedure TCnJPEGEncoder.WriteSOSForScan(Ss, Se, Ah, Al: Byte;
  CompIdx: Integer; IsAC: Boolean);
var
  W: Word;
  B, DCTbl, ACTbl: Byte;
begin
  W := CN_JPEG_SOS;
  WriteWord(W);
  W := 2 + 1 + 1 * 2 + 3;
  WriteWord(W);
  B := 1;
  FStream.Write(B, 1);
  B := CompIdx + 1;
  FStream.Write(B, 1);
  DCTbl := CompIdx;
  if DCTbl > 1 then DCTbl := 1;
  ACTbl := DCTbl;
  B := (DCTbl shl 4) or ACTbl;
  FStream.Write(B, 1);
  B := Ss; FStream.Write(B, 1);
  B := Se; FStream.Write(B, 1);
  B := (Ah shl 4) or Al; FStream.Write(B, 1);
end;

procedure TCnJPEGEncoder.WriteSOSProgressive(Ss, Se, Ah, Al: Byte;
  CompList: array of Integer; NumComps: Integer);
var
  W: Word;
  B: Byte;
  I, CompIdx: Integer;
  DCTbl, ACTbl: Byte;
begin
  W := CN_JPEG_SOS;
  WriteWord(W);
  W := 2 + 1 + NumComps * 2 + 3;
  WriteWord(W);
  B := NumComps;
  FStream.Write(B, 1);
  for I := 0 to NumComps - 1 do
  begin
    CompIdx := CompList[I];
    B := CompIdx + 1;
    FStream.Write(B, 1);
    DCTbl := CompIdx;
    if DCTbl > 1 then DCTbl := 1;
    ACTbl := DCTbl;
    B := (DCTbl shl 4) or ACTbl;
    FStream.Write(B, 1);
  end;
  B := Ss; FStream.Write(B, 1);
  B := Se; FStream.Write(B, 1);
  B := (Ah shl 4) or Al; FStream.Write(B, 1);
end;

function TCnJPEGEncoder.GetEncCoefBlockPtr(CompIdx, BlockX,
  BlockY: Integer): PSmallInt;
var
  BlockIdx: Integer;
begin
  BlockIdx := BlockY * FCompBlocksPerRow[CompIdx] + BlockX;
  Result := @FCoefBlocks[CompIdx][BlockIdx * 64];
end;

procedure TCnJPEGEncoder.EncodeBlock(Pixels: array of Byte; CompIdx: Integer;
  QuantID: Integer; var PrevDC: Integer);
var
  I, X, Y, U, V: Integer;
  F: array[0..63] of Integer;
  Tmp: array[0..63] of Integer;
  QCoef: array[0..63] of Integer;
  ZigCoef: array[0..63] of Integer;
  Sum: Int64;
  DCVal, Diff, S, RS: Integer;
  Run, Size, K: Integer;
  Val, A: Integer;
  TblIdx: Integer;
begin
  // 1. 电平偏移
  for I := 0 to 63 do
    F[I] := Pixels[I] - 128;

  // 2. FDCT（行变换 + 列变换）
  for Y := 0 to 7 do
    for U := 0 to 7 do
    begin
      Sum := 0;
      for X := 0 to 7 do
        Inc(Sum, Int64(CN_JPEG_IDCT_COS[U, X]) * F[X + Y * 8]);
      Tmp[U + Y * 8] := Integer(Sum shr 16);
    end;
  for U := 0 to 7 do
    for V := 0 to 7 do
    begin
      Sum := 0;
      for Y := 0 to 7 do
        Inc(Sum, Int64(CN_JPEG_IDCT_COS[V, Y]) * Tmp[U + Y * 8]);
      QCoef[V * 8 + U] := Integer(Sum shr 16);
    end;

  // 3. 量化
  for I := 0 to 63 do
  begin
    if QCoef[I] >= 0 then
      QCoef[I] := (QCoef[I] + FQuantTables[QuantID, I] div 2) div
        FQuantTables[QuantID, I]
    else
      QCoef[I] := -((-QCoef[I] + FQuantTables[QuantID, I] div 2) div
        FQuantTables[QuantID, I]);
  end;

  // 4. ZigZag 重排
  for I := 0 to 63 do
    ZigCoef[I] := QCoef[CN_JPEG_ZIGZAG_ORDER[I]];

  // 5. Huffman 编码
  TblIdx := 0;
  if CompIdx > 0 then TblIdx := 1;

  // DC 编码（差分）
  DCVal := ZigCoef[0];
  Diff := DCVal - PrevDC;
  PrevDC := DCVal;

  A := Abs(Diff);
  S := 0;
  while A > 0 do
  begin
    Inc(S);
    A := A shr 1;
  end;

  // 写 DC Huffman 码
  FBitWriter.PutBits(FDCEncCode[TblIdx, S], FDCEncSize[TblIdx, S]);
  // 写 DC 附加位
  if S > 0 then
  begin
    if Diff < 0 then
      Val := Diff + (1 shl S) - 1
    else
      Val := Diff;
    FBitWriter.PutBits(Cardinal(Val), S);
  end;

  // AC 编码
  K := 1;
  Run := 0;
  while K < 64 do
  begin
    if ZigCoef[K] = 0 then
    begin
      Inc(Run);
      Inc(K);
      Continue;
    end;

    // 输出零游程
    while Run >= 16 do
    begin
      // ZRL
      FBitWriter.PutBits(FACEncCode[TblIdx, $F0], FACEncSize[TblIdx, $F0]);
      Dec(Run, 16);
    end;

    // 计算系数的 category
    A := Abs(ZigCoef[K]);
    Size := 0;
    while A > 0 do
    begin
      Inc(Size);
      A := A shr 1;
    end;

    RS := (Run shl 4) or Size;
    FBitWriter.PutBits(FACEncCode[TblIdx, RS], FACEncSize[TblIdx, RS]);

    // 写系数附加位
    if ZigCoef[K] < 0 then
      Val := ZigCoef[K] + (1 shl Size) - 1
    else
      Val := ZigCoef[K];
    FBitWriter.PutBits(Cardinal(Val), Size);

    Run := 0;
    Inc(K);
  end;

  // 如果剩余都是零，写 EOB
  if Run > 0 then
    FBitWriter.PutBits(FACEncCode[TblIdx, $00], FACEncSize[TblIdx, $00]);
end;

procedure TCnJPEGEncoder.EncodeMCU(SrcBmp: TBitmap; MCUX, MCUY: Integer);
var
  Pixels: array[0..63] of Byte;
  BX, BY, PX, PY: Integer;
  R, G, B: Byte;
  ImgX, ImgY: Integer;
  RowPtr: PByteArray;
  YVal, CbVal, CrVal: Integer;
  YBuf: array[0..15, 0..15] of Byte;
  CbBuf, CrBuf: array[0..7, 0..7] of Byte;
  CbX, CbY: Integer;
begin
  // 读取 MCU 像素到缓冲区
  FillChar(YBuf, SizeOf(YBuf), 128);
  FillChar(CbBuf, SizeOf(CbBuf), 128);
  FillChar(CrBuf, SizeOf(CrBuf), 128);

  for PY := 0 to FMCUHeight - 1 do
  begin
    ImgY := MCUY * FMCUHeight + PY;
    if ImgY >= SrcBmp.Height then ImgY := SrcBmp.Height - 1;
    if ImgY < 0 then Continue;
    RowPtr := SrcBmp.ScanLine[ImgY];

    for PX := 0 to FMCUWidth - 1 do
    begin
      ImgX := MCUX * FMCUWidth + PX;
      if ImgX >= SrcBmp.Width then ImgX := SrcBmp.Width - 1;
      if ImgX < 0 then Continue;

      B := RowPtr[ImgX * 3];
      G := RowPtr[ImgX * 3 + 1];
      R := RowPtr[ImgX * 3 + 2];

      if FGrayscale then
      begin
        YBuf[PY, PX] := Byte(Round(0.299 * R + 0.587 * G + 0.114 * B));
      end
      else
      begin
        YVal := Round(0.299 * R + 0.587 * G + 0.114 * B);
        CbVal := Round(-0.16874 * R - 0.33126 * G + 0.5 * B) + 128;
        CrVal := Round(0.5 * R - 0.41869 * G - 0.08131 * B) + 128;
        if YVal < 0 then YVal := 0;
        if YVal > 255 then YVal := 255;
        if CbVal < 0 then CbVal := 0;
        if CbVal > 255 then CbVal := 255;
        if CrVal < 0 then CrVal := 0;
        if CrVal > 255 then CrVal := 255;
        YBuf[PY, PX] := Byte(YVal);
        // 4:2:0 下采样
        CbX := PX div 2;
        CbY := PY div 2;
        CbBuf[CbY, CbX] := Byte(CbVal);
        CrBuf[CbY, CbX] := Byte(CrVal);
      end;
    end;
  end;

  // 编码 Y 分量
  for BY := 0 to FMaxV - 1 do
    for BX := 0 to FMaxH - 1 do
    begin
      for PY := 0 to 7 do
        for PX := 0 to 7 do
          Pixels[PY * 8 + PX] := YBuf[BY * 8 + PY, BX * 8 + PX];
      EncodeBlock(Pixels, 0, 0, FPrevDC[0]);
    end;

  // 编码 Cb/Cr 分量
  if not FGrayscale then
  begin
    for PY := 0 to 7 do
      for PX := 0 to 7 do
        Pixels[PY * 8 + PX] := CbBuf[PY, PX];
    EncodeBlock(Pixels, 1, 1, FPrevDC[1]);

    for PY := 0 to 7 do
      for PX := 0 to 7 do
        Pixels[PY * 8 + PX] := CrBuf[PY, PX];
    EncodeBlock(Pixels, 2, 1, FPrevDC[2]);
  end;
end;

procedure TCnJPEGEncoder.Encode(SrcBmp: TBitmap; AStream: TStream;
  AQuality: TCnJPEGQualityRange; AGrayscale: Boolean;
  AProgressive: Boolean);
var
  I: Integer;
  MCUX, MCUY: Integer;
  SrcPF: TPixelFormat;
begin
  FQuality := AQuality;
  FGrayscale := AGrayscale;
  FProgressive := AProgressive;
  FStream := AStream;
  FWidth := SrcBmp.Width;
  FHeight := SrcBmp.Height;

  if FGrayscale then
  begin
    FNumComponents := 1;
    FMaxH := 1;
    FMaxV := 1;
    FHSampFactor[0] := 1;
    FVSampFactor[0] := 1;
  end
  else
  begin
    FNumComponents := 3;
    FMaxH := 2;
    FMaxV := 2;
    FHSampFactor[0] := 2; FVSampFactor[0] := 2;
    FHSampFactor[1] := 1; FVSampFactor[1] := 1;
    FHSampFactor[2] := 1; FVSampFactor[2] := 1;
  end;

  FMCUWidth := 8 * FMaxH;
  FMCUHeight := 8 * FMaxV;
  FMCUsPerRow := (FWidth + FMCUWidth - 1) div FMCUWidth;
  FMCUsPerCol := (FHeight + FMCUHeight - 1) div FMCUHeight;

  // 确保源位图是 24 位
  SrcPF := SrcBmp.PixelFormat;
  if SrcPF <> pf24bit then
    SrcBmp.PixelFormat := pf24bit;

  // 构建表
  BuildQuantTables(FQuality);
  BuildStandardHuffmanTables;

  // 初始化 DC 预测值
  for I := 0 to 3 do
    FPrevDC[I] := 0;

  if FProgressive then
  begin
    EncodeProgressive(SrcBmp);
  end
  else
  begin
    // 写入 JPEG 文件头
    WriteSOI;
    WriteAPP0;
    WriteDQT;
    WriteSOF0;
    WriteDHT;
    WriteSOS;

    // 创建位写入器
    FBitWriter := TCnJPEGBitWriter.Create(FStream);
    try
      // 编码所有 MCU
      for MCUY := 0 to FMCUsPerCol - 1 do
      begin
        for MCUX := 0 to FMCUsPerRow - 1 do
        begin
          EncodeMCU(SrcBmp, MCUX, MCUY);
        end;
      end;

      // 刷新位写入器
      FBitWriter.Flush;
    finally
      FBitWriter.Free;
      FBitWriter := nil;
    end;

    // 写入 EOI
    WriteEOI;
  end;

  // 恢复源位图格式
  if SrcPF <> pf24bit then
    SrcBmp.PixelFormat := SrcPF;
end;

procedure TCnJPEGEncoder.ComputeAllCoefBlocks(SrcBmp: TBitmap);
var
  MCUX, MCUY, BX, BY, PX, PY: Integer;
  R, G, B: Byte;
  ImgX, ImgY: Integer;
  RowPtr: PByteArray;
  YVal, CbVal, CrVal: Integer;
  YBuf: array[0..15, 0..15] of Byte;
  CbBuf, CrBuf: array[0..7, 0..7] of Byte;
  CbX, CbY: Integer;
  I, X, Y, U, V: Integer;
  F: array[0..63] of Integer;
  Tmp: array[0..63] of Integer;
  QCoef: array[0..63] of Integer;
  Sum: Int64;
  CoefPtr: PSmallIntArray;
  CompIdx, TotalBlocks: Integer;
begin
  for CompIdx := 0 to FNumComponents - 1 do
  begin
    FCompBlocksPerRow[CompIdx] := FMCUsPerRow * FHSampFactor[CompIdx];
    FCompBlocksPerCol[CompIdx] := FMCUsPerCol * FVSampFactor[CompIdx];
    TotalBlocks := FCompBlocksPerRow[CompIdx] * FCompBlocksPerCol[CompIdx];
    SetLength(FCoefBlocks[CompIdx], TotalBlocks * 64);
    FillChar(FCoefBlocks[CompIdx][0], TotalBlocks * 64 * SizeOf(SmallInt), 0);
  end;

  for MCUY := 0 to FMCUsPerCol - 1 do
  begin
    for MCUX := 0 to FMCUsPerRow - 1 do
    begin
      FillChar(YBuf, SizeOf(YBuf), 128);
      FillChar(CbBuf, SizeOf(CbBuf), 128);
      FillChar(CrBuf, SizeOf(CrBuf), 128);

      for PY := 0 to FMCUHeight - 1 do
      begin
        ImgY := MCUY * FMCUHeight + PY;
        if ImgY >= SrcBmp.Height then ImgY := SrcBmp.Height - 1;
        if ImgY < 0 then Continue;
        RowPtr := SrcBmp.ScanLine[ImgY];
        for PX := 0 to FMCUWidth - 1 do
        begin
          ImgX := MCUX * FMCUWidth + PX;
          if ImgX >= SrcBmp.Width then ImgX := SrcBmp.Width - 1;
          if ImgX < 0 then Continue;
          B := RowPtr[ImgX * 3];
          G := RowPtr[ImgX * 3 + 1];
          R := RowPtr[ImgX * 3 + 2];
          if FGrayscale then
            YBuf[PY, PX] := Byte(Round(0.299 * R + 0.587 * G + 0.114 * B))
          else
          begin
            YVal := Round(0.299 * R + 0.587 * G + 0.114 * B);
            CbVal := Round(-0.16874 * R - 0.33126 * G + 0.5 * B) + 128;
            CrVal := Round(0.5 * R - 0.41869 * G - 0.08131 * B) + 128;
            if YVal < 0 then YVal := 0;
            if YVal > 255 then YVal := 255;
            if CbVal < 0 then CbVal := 0;
            if CbVal > 255 then CbVal := 255;
            if CrVal < 0 then CrVal := 0;
            if CrVal > 255 then CrVal := 255;
            YBuf[PY, PX] := Byte(YVal);
            CbX := PX div 2;
            CbY := PY div 2;
            CbBuf[CbY, CbX] := Byte(CbVal);
            CrBuf[CbY, CbX] := Byte(CrVal);
          end;
        end;
      end;

      // 计算每个分量的 DCT 系数
      for BY := 0 to FMaxV - 1 do
        for BX := 0 to FMaxH - 1 do
        begin
          // FDCT for Y block
          for PY := 0 to 7 do
            for PX := 0 to 7 do
              F[PY * 8 + PX] := YBuf[BY * 8 + PY, BX * 8 + PX] - 128;
          // 行变换
          for Y := 0 to 7 do
            for U := 0 to 7 do
            begin
              Sum := 0;
              for X := 0 to 7 do
                Inc(Sum, Int64(CN_JPEG_IDCT_COS[U, X]) * F[X + Y * 8]);
              Tmp[U + Y * 8] := Integer(Sum shr 16);
            end;
          // 列变换
          for U := 0 to 7 do
            for V := 0 to 7 do
            begin
              Sum := 0;
              for Y := 0 to 7 do
                Inc(Sum, Int64(CN_JPEG_IDCT_COS[V, Y]) * Tmp[U + Y * 8]);
              QCoef[V * 8 + U] := Integer(Sum shr 16);
            end;
          // 量化
          for I := 0 to 63 do
          begin
            if QCoef[I] >= 0 then
              QCoef[I] := (QCoef[I] + FQuantTables[0, I] div 2) div FQuantTables[0, I]
            else
              QCoef[I] := -((-QCoef[I] + FQuantTables[0, I] div 2) div FQuantTables[0, I]);
          end;
          // 存储
          CoefPtr := PSmallIntArray(GetEncCoefBlockPtr(0,
            MCUX * FHSampFactor[0] + BX, MCUY * FVSampFactor[0] + BY));
          for I := 0 to 63 do
            CoefPtr[I] := SmallInt(QCoef[I]);
        end;

      if not FGrayscale then
      begin
        for CompIdx := 1 to 2 do
        begin
          // FDCT for Cb/Cr block
          for PY := 0 to 7 do
            for PX := 0 to 7 do
            begin
              if CompIdx = 1 then
                F[PY * 8 + PX] := CbBuf[PY, PX] - 128
              else
                F[PY * 8 + PX] := CrBuf[PY, PX] - 128;
            end;
          for Y := 0 to 7 do
            for U := 0 to 7 do
            begin
              Sum := 0;
              for X := 0 to 7 do
                Inc(Sum, Int64(CN_JPEG_IDCT_COS[U, X]) * F[X + Y * 8]);
              Tmp[U + Y * 8] := Integer(Sum shr 16);
            end;
          for U := 0 to 7 do
            for V := 0 to 7 do
            begin
              Sum := 0;
              for Y := 0 to 7 do
                Inc(Sum, Int64(CN_JPEG_IDCT_COS[V, Y]) * Tmp[U + Y * 8]);
              QCoef[V * 8 + U] := Integer(Sum shr 16);
            end;
          for I := 0 to 63 do
          begin
            if QCoef[I] >= 0 then
              QCoef[I] := (QCoef[I] + FQuantTables[1, I] div 2) div FQuantTables[1, I]
            else
              QCoef[I] := -((-QCoef[I] + FQuantTables[1, I] div 2) div FQuantTables[1, I]);
          end;
          CoefPtr := PSmallIntArray(GetEncCoefBlockPtr(CompIdx, MCUX, MCUY));
          for I := 0 to 63 do
            CoefPtr[I] := SmallInt(QCoef[I]);
        end;
      end;
    end;
  end;
end;

procedure TCnJPEGEncoder.EncodeDCFirstScan(CompList: array of Integer;
  NumComps: Integer; Al: Byte);
var
  MCUX, MCUY, ScanIdx, CompIdx, BX, BY: Integer;
  CoefPtr: PSmallIntArray;
  DCVal, Diff, S, A, Val: Integer;
  TblIdx: Integer;
begin
  for ScanIdx := 0 to 3 do
    FPrevDC[ScanIdx] := 0;

  for MCUY := 0 to FMCUsPerCol - 1 do
  begin
    for MCUX := 0 to FMCUsPerRow - 1 do
    begin
      for ScanIdx := 0 to NumComps - 1 do
      begin
        CompIdx := CompList[ScanIdx];
        TblIdx := CompIdx;
        if TblIdx > 1 then TblIdx := 1;
        for BY := 0 to FVSampFactor[CompIdx] - 1 do
          for BX := 0 to FHSampFactor[CompIdx] - 1 do
          begin
            CoefPtr := PSmallIntArray(GetEncCoefBlockPtr(CompIdx,
              MCUX * FHSampFactor[CompIdx] + BX,
              MCUY * FVSampFactor[CompIdx] + BY));
            DCVal := CoefPtr[0] shr Al;
            Diff := DCVal - FPrevDC[CompIdx];
            FPrevDC[CompIdx] := DCVal;
            A := Abs(Diff);
            S := 0;
            while A > 0 do
            begin
              Inc(S);
              A := A shr 1;
            end;
            FBitWriter.PutBits(FDCEncCode[TblIdx, S], FDCEncSize[TblIdx, S]);
            if S > 0 then
            begin
              if Diff < 0 then
                Val := Diff + (1 shl S) - 1
              else
                Val := Diff;
              FBitWriter.PutBits(Cardinal(Val), S);
            end;
          end;
      end;
    end;
  end;
  FBitWriter.Flush;
end;

procedure TCnJPEGEncoder.EncodeDCRefineScan(CompList: array of Integer;
  NumComps: Integer; Ah, Al: Byte);
var
  MCUX, MCUY, ScanIdx, CompIdx, BX, BY: Integer;
  CoefPtr: PSmallIntArray;
  Bit: Integer;
begin
  for MCUY := 0 to FMCUsPerCol - 1 do
  begin
    for MCUX := 0 to FMCUsPerRow - 1 do
    begin
      for ScanIdx := 0 to NumComps - 1 do
      begin
        CompIdx := CompList[ScanIdx];
        for BY := 0 to FVSampFactor[CompIdx] - 1 do
          for BX := 0 to FHSampFactor[CompIdx] - 1 do
          begin
            CoefPtr := PSmallIntArray(GetEncCoefBlockPtr(CompIdx,
              MCUX * FHSampFactor[CompIdx] + BX,
              MCUY * FVSampFactor[CompIdx] + BY));
            Bit := (CoefPtr[0] shr Al) and 1;
            FBitWriter.PutBit(Bit);
          end;
      end;
    end;
  end;
  FBitWriter.Flush;
end;

procedure TCnJPEGEncoder.EncodeACFirstScan(CompIdx: Integer;
  Ss, Se, Al: Byte);
var
  BlockX, BlockY, K: Integer;
  CoefPtr: PSmallIntArray;
  ZigCoef: array[0..63] of SmallInt;
  Run, Size, RS, Val, A: Integer;
  TblIdx: Integer;
  EOBRun: Integer;
  BlocksPerRow, BlocksPerCol: Integer;
begin
  FPrevDC[CompIdx] := 0;
  TblIdx := CompIdx;
  if TblIdx > 1 then TblIdx := 1;

  BlocksPerRow := FCompBlocksPerRow[CompIdx];
  BlocksPerCol := FCompBlocksPerCol[CompIdx];
  EOBRun := 0;

  for BlockY := 0 to BlocksPerCol - 1 do
  begin
    for BlockX := 0 to BlocksPerRow - 1 do
    begin
      CoefPtr := PSmallIntArray(GetEncCoefBlockPtr(CompIdx, BlockX, BlockY));

      // ZigZag 重排 Ss..Se 范围
      for K := 0 to 63 do
        ZigCoef[K] := 0;
      for K := Ss to Se do
        ZigCoef[K] := CoefPtr[CN_JPEG_ZIGZAG_ORDER[K]];

      if EOBRun > 0 then
      begin
        Dec(EOBRun);
        Continue;
      end;

      K := Ss;
      Run := 0;
      while K <= Se do
      begin
        if ZigCoef[K] = 0 then
        begin
          Inc(Run);
          Inc(K);
          Continue;
        end;

        while Run >= 16 do
        begin
          FBitWriter.PutBits(FACEncCode[TblIdx, $F0], FACEncSize[TblIdx, $F0]);
          Dec(Run, 16);
        end;

        Val := ZigCoef[K] shr Al;
        A := Abs(Val);
        Size := 0;
        while A > 0 do
        begin
          Inc(Size);
          A := A shr 1;
        end;

        RS := (Run shl 4) or Size;
        FBitWriter.PutBits(FACEncCode[TblIdx, RS], FACEncSize[TblIdx, RS]);

        if Val < 0 then
          Val := Val + (1 shl Size) - 1;
        FBitWriter.PutBits(Cardinal(Val), Size);

        Run := 0;
        Inc(K);
      end;

      if Run > 0 then
      begin
        // EOB run
        Inc(EOBRun);
        // 写 EOBn 编码
        if EOBRun = 1 then
          FBitWriter.PutBits(FACEncCode[TblIdx, $00], FACEncSize[TblIdx, $00])
        else
        begin
          // EOBn: 写入 ZRL + (EOBRun-1) 的二进制
          K := 0;
          A := EOBRun - 1;
          while A > 0 do
          begin
            Inc(K);
            A := A shr 1;
          end;
          RS := (K shl 4);  // Size=K, Run=0 → EOBn
          if RS <= $0F then
          begin
            FBitWriter.PutBits(FACEncCode[TblIdx, RS], FACEncSize[TblIdx, RS]);
            Val := EOBRun - 1 - (1 shl (K - 1));
            FBitWriter.PutBits(Cardinal(Val), K);
          end
          else
            FBitWriter.PutBits(FACEncCode[TblIdx, $00], FACEncSize[TblIdx, $00]);
          Dec(EOBRun);  // 当前块已算
        end;
      end;
    end;
  end;
  FBitWriter.Flush;
end;

procedure TCnJPEGEncoder.EncodeACRefineScan(CompIdx: Integer;
  Ss, Se, Ah, Al: Byte);
var
  BlockX, BlockY, K: Integer;
  CoefPtr: PSmallIntArray;
  ZigCoef: array[0..63] of SmallInt;
  Run, RS, Val: Integer;
  TblIdx: Integer;
  EOBRun: Integer;
  BlocksPerRow, BlocksPerCol: Integer;
  CoefVal: SmallInt;
  Sign: Integer;
begin
  TblIdx := CompIdx;
  if TblIdx > 1 then TblIdx := 1;

  BlocksPerRow := FCompBlocksPerRow[CompIdx];
  BlocksPerCol := FCompBlocksPerCol[CompIdx];
  EOBRun := 0;

  for BlockY := 0 to BlocksPerCol - 1 do
  begin
    for BlockX := 0 to BlocksPerRow - 1 do
    begin
      CoefPtr := PSmallIntArray(GetEncCoefBlockPtr(CompIdx, BlockX, BlockY));
      for K := 0 to 63 do
        ZigCoef[K] := 0;
      for K := Ss to Se do
        ZigCoef[K] := CoefPtr[CN_JPEG_ZIGZAG_ORDER[K]];

      if EOBRun > 0 then
      begin
        Dec(EOBRun);
        // 在 EOB run 中仍需输出非零系数的修正位
        K := Ss;
        while K <= Se do
        begin
          if ZigCoef[K] <> 0 then
          begin
            Val := (ZigCoef[K] shr Al) and 1;
            if Val = 1 then
              FBitWriter.PutBit(1);
          end;
          Inc(K);
        end;
        Continue;
      end;

      K := Ss;
      Run := 0;
      while K <= Se do
      begin
        CoefVal := ZigCoef[K];

        // 检查是否是已校正的系数（在新位之前已非零）
        if Abs(CoefVal) >= (1 shl (Al + 1)) then
        begin
          // 之前已非零：输出修正位
          Val := (CoefVal shr Al) and 1;
          if Val = 1 then
            FBitWriter.PutBit(1);
          Inc(K);
          Continue;
        end;

        // 新出现的非零系数
        if CoefVal = 0 then
        begin
          Inc(Run);
          Inc(K);
          Continue;
        end;

        // 输出零游程
        while Run >= 16 do
        begin
          FBitWriter.PutBits(FACEncCode[TblIdx, $F0], FACEncSize[TblIdx, $F0]);
          Dec(Run, 16);
          // ZRL 期间修正位也要输出
        end;

        // 新非零系数：RS=Run<<4|1, 附加 1 位
        RS := (Run shl 4) or 1;
        FBitWriter.PutBits(FACEncCode[TblIdx, RS], FACEncSize[TblIdx, RS]);
        Sign := (CoefVal shr Al) and 1;
        FBitWriter.PutBit(Sign);
        Run := 0;
        Inc(K);
      end;

      if Run > 0 then
      begin
        Inc(EOBRun);
        if EOBRun >= 32767 then
        begin
          FBitWriter.PutBits(FACEncCode[TblIdx, $00], FACEncSize[TblIdx, $00]);
          EOBRun := 0;
        end;
      end;
    end;
  end;
  FBitWriter.Flush;
end;

procedure TCnJPEGEncoder.EncodeProgressive(SrcBmp: TBitmap);
var
  CompList: array[0..3] of Integer;
  NumComps, I: Integer;
  SrcPF: TPixelFormat;
begin
  SrcPF := SrcBmp.PixelFormat;
  if SrcPF <> pf24bit then
    SrcBmp.PixelFormat := pf24bit;

  // 1. 预计算所有 DCT 系数
  ComputeAllCoefBlocks(SrcBmp);

  // 2. 写入文件头
  WriteSOI;
  WriteAPP0;
  WriteDQT;
  WriteSOF2;
  WriteDHT;

  // 3. DC 首次扫描（所有分量交错，Al=1）
  NumComps := FNumComponents;
  for I := 0 to NumComps - 1 do
    CompList[I] := I;
  WriteSOSProgressive(0, 0, 0, 1, CompList, NumComps);
  FBitWriter := TCnJPEGBitWriter.Create(FStream);
  try
    EncodeDCFirstScan(CompList, NumComps, 1);
  finally
    FBitWriter.Free;
    FBitWriter := nil;
  end;

  // 4. DC 精化扫描（Ah=1, Al=0）
  WriteSOSProgressive(0, 0, 1, 0, CompList, NumComps);
  FBitWriter := TCnJPEGBitWriter.Create(FStream);
  try
    EncodeDCRefineScan(CompList, NumComps, 1, 0);
  finally
    FBitWriter.Free;
    FBitWriter := nil;
  end;

  // 5. AC 首次扫描 + 精化扫描，每个分量
  for I := 0 to NumComps - 1 do
  begin
    // AC 首次扫描 (Ss=1, Se=63, Ah=0, Al=1)
    WriteSOSForScan(1, 63, 0, 1, I, True);
    FBitWriter := TCnJPEGBitWriter.Create(FStream);
    try
      EncodeACFirstScan(I, 1, 63, 1);
    finally
      FBitWriter.Free;
      FBitWriter := nil;
    end;

    // AC 精化扫描 (Ss=1, Se=63, Ah=1, Al=0)
    WriteSOSForScan(1, 63, 1, 0, I, True);
    FBitWriter := TCnJPEGBitWriter.Create(FStream);
    try
      EncodeACRefineScan(I, 1, 63, 1, 0);
    finally
      FBitWriter.Free;
      FBitWriter := nil;
    end;
  end;

  // 6. 写入 EOI
  WriteEOI;

  // 恢复源位图格式
  if SrcPF <> pf24bit then
    SrcBmp.PixelFormat := SrcPF;
end;

//============================================================================
// TCnJPEGData
//============================================================================

constructor TCnJPEGData.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create;
  FWidth := 0;
  FHeight := 0;
  FGrayscale := False;
  FProgressive := False;
  FColorSpace := jcRGB;
  FBitDepth := 8;
  FRefCount := 1;
end;

destructor TCnJPEGData.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TCnJPEGData.LoadFromStream(Stream: TStream);
begin
  FData.Clear;
  FData.CopyFrom(Stream, 0);
  FData.Position := 0;
  ParseHeader;
end;

procedure TCnJPEGData.SaveToStream(Stream: TStream);
begin
  FData.Position := 0;
  Stream.CopyFrom(FData, 0);
end;

procedure TCnJPEGData.Assign(Source: TPersistent);
begin
  if Source is TCnJPEGData then
  begin
    FData.Clear;
    FData.CopyFrom(TCnJPEGData(Source).FData, 0);
    FWidth := TCnJPEGData(Source).FWidth;
    FHeight := TCnJPEGData(Source).FHeight;
    FGrayscale := TCnJPEGData(Source).FGrayscale;
    FProgressive := TCnJPEGData(Source).FProgressive;
    FColorSpace := TCnJPEGData(Source).FColorSpace;
    FBitDepth := TCnJPEGData(Source).FBitDepth;
  end
  else
    inherited Assign(Source);
end;

procedure TCnJPEGData.AddRef;
begin
  Inc(FRefCount);
end;

procedure TCnJPEGData.Release;
begin
  Dec(FRefCount);
  if FRefCount <= 0 then
    Free;
end;

procedure TCnJPEGData.ParseHeader;
var
  Marker: TCnJPEGMarker;
  Code: Word;
  SegLen: Word;
  Precision: Byte;
  Nf: Byte;
  I: Integer;
  B: Byte;
begin
  FWidth := 0;
  FHeight := 0;
  FGrayscale := False;
  FProgressive := False;
  FColorSpace := jcRGB;
  FBitDepth := 8;

  FData.Position := 0;
  Marker := TCnJPEGMarker.Create(FData);
  try
    // 读取 SOI
    Code := Marker.ReadMarker;
    if Code <> CN_JPEG_SOI then
      raise ECnJPEGException.Create(SCnErrorInvalidJpegMissingSoi);

    // 循环读取 marker
    repeat
      Code := Marker.ReadMarker;

      // 无段 marker
      if Code = CN_JPEG_EOI then
        Break;
      if Code = CN_JPEG_TEM then
        Continue;
      if (Code >= CN_JPEG_RST0) and (Code <= CN_JPEG_RST7) then
        Continue;

      // 读取段长度
      SegLen := Marker.ReadWord;

      // SOF marker — 解析帧头
      if (Code = CN_JPEG_SOF0) or (Code = CN_JPEG_SOF1) or
         (Code = CN_JPEG_SOF2) then
      begin
        Precision := Marker.ReadByte;
        FHeight := Marker.ReadWord;
        FWidth := Marker.ReadWord;
        Nf := Marker.ReadByte;
        FBitDepth := Precision;

        // 跳过分量信息
        for I := 0 to Nf - 1 do
        begin
          Marker.ReadByte; // ID
          B := Marker.ReadByte; // 采样因子
          Marker.ReadByte; // 量化表 ID
        end;

        // 设置色彩空间和灰度标志
        if Nf = 1 then
        begin
          FGrayscale := True;
          FColorSpace := jcGrayscale;
        end
        else if Nf = 4 then
        begin
          FGrayscale := False;
          FColorSpace := jcCMYK;
        end
        else
        begin
          FGrayscale := False;
          FColorSpace := jcRGB;
        end;

        // 设置渐进标志
        if Code = CN_JPEG_SOF2 then
          FProgressive := True
        else
          FProgressive := False;
      end
      else if Code = CN_JPEG_SOS then
      begin
        // SOS — 停止解析
        Break;
      end
      else
      begin
        // 跳过未知段
        if SegLen >= 2 then
          Marker.SkipBytes(SegLen - 2);
      end;
    until False;

  finally
    Marker.Free;
  end;
end;

//============================================================================
// TCnJPEGImage
//============================================================================

constructor TCnJPEGImage.Create;
begin
  inherited Create;
  FData := nil;
  FBitmap := nil;
  FModified := False;
  FCompressionQuality := 100;
  FGrayscale := False;
  FPixelFormat := jf24Bit;
  FPerformance := jpBestQuality;
  FProgressiveDisplay := False;
  FProgressiveEncoding := False;
  FScale := jsFullSize;
  FSmoothing := True;
  FTransparentColor := clDefault;
end;

destructor TCnJPEGImage.Destroy;
begin
  FreeBitmap;
  if FData <> nil then
  begin
    FData.Release;
    FData := nil;
  end;
  inherited Destroy;
end;

procedure TCnJPEGImage.FreeBitmap;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TCnJPEGImage.CreateBitmap;
begin
  FreeBitmap;
  FBitmap := TBitmap.Create;
end;

procedure TCnJPEGImage.NewJPEGData;
begin
  if FData <> nil then
    FData.Release;
  FData := TCnJPEGData.Create;
end;

function TCnJPEGImage.GetBitmap: TBitmap;
begin
  DIBNeeded;
  Result := FBitmap;
end;

function TCnJPEGImage.GetJPEGData: TCnJPEGData;
begin
  JPEGNeeded;
  Result := FData;
end;

procedure TCnJPEGImage.SetCompressionQuality(Value: TCnJPEGQualityRange);
begin
  if FCompressionQuality <> Value then
  begin
    FCompressionQuality := Value;
    FModified := True;
  end;
end;

procedure TCnJPEGImage.SetGrayscale(Value: Boolean);
begin
  if FGrayscale <> Value then
  begin
    FGrayscale := Value;
    if Value then
      FPixelFormat := jf8Bit;
    FreeBitmap;
    FModified := True;
  end;
end;

procedure TCnJPEGImage.SetPixelFormat(Value: TCnJPEGPixelFormat);
begin
  if FPixelFormat <> Value then
  begin
    FPixelFormat := Value;
    FreeBitmap;
  end;
end;

procedure TCnJPEGImage.SetPerformance(Value: TCnJPEGPerformance);
begin
  if FPerformance <> Value then
  begin
    FPerformance := Value;
    FreeBitmap;
  end;
end;

procedure TCnJPEGImage.SetProgressiveDisplay(Value: Boolean);
begin
  FProgressiveDisplay := Value;
end;

procedure TCnJPEGImage.SetProgressiveEncoding(Value: Boolean);
begin
  if FProgressiveEncoding <> Value then
  begin
    FProgressiveEncoding := Value;
    FModified := True;
  end;
end;

procedure TCnJPEGImage.SetScale(Value: TCnJPEGScale);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    FreeBitmap;
  end;
end;

procedure TCnJPEGImage.SetSmoothing(Value: Boolean);
begin
  if FSmoothing <> Value then
  begin
    FSmoothing := Value;
    FreeBitmap;
  end;
end;

procedure TCnJPEGImage.SetTransparentColor(Value: TColor);
begin
  FTransparentColor := Value;
end;

function TCnJPEGImage.GetEmpty: Boolean;
begin
  Result := (FData = nil) and (FBitmap = nil);
end;

function TCnJPEGImage.GetHeight: Integer;
begin
  if FData <> nil then
    Result := FData.Height
  else if FBitmap <> nil then
    Result := FBitmap.Height
  else
    Result := 0;
end;

function TCnJPEGImage.GetWidth: Integer;
begin
  if FData <> nil then
    Result := FData.Width
  else if FBitmap <> nil then
    Result := FBitmap.Width
  else
    Result := 0;
end;

function TCnJPEGImage.GetTransparent: Boolean;
begin
  Result := FTransparentColor <> clDefault;
end;

procedure TCnJPEGImage.SetTransparent(Value: Boolean);
begin
  if Value then
  begin
    if FTransparentColor = clDefault then
      FTransparentColor := clWhite;
  end
  else
    FTransparentColor := clDefault;
end;

procedure TCnJPEGImage.SetHeight(Value: Integer);
begin
  // 不允许直接修改尺寸
end;

procedure TCnJPEGImage.SetWidth(Value: Integer);
begin
  // 不允许直接修改尺寸
end;

procedure TCnJPEGImage.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
  begin
    DIBNeeded;
    if FBitmap <> nil then
      Dest.Assign(FBitmap)
    else
    begin
      TBitmap(Dest).Width := 0;
      TBitmap(Dest).Height := 0;
    end;
  end
  else if Dest is TCnJPEGImage then
    TCnJPEGImage(Dest).Assign(Self)
  else
    inherited AssignTo(Dest);
end;

procedure TCnJPEGImage.LoadFromStream(Stream: TStream);
begin
  NewJPEGData;
  FData.LoadFromStream(Stream);
  FreeBitmap;
  FGrayscale := FData.Grayscale;
  FProgressiveEncoding := FData.Progressive;
  FModified := False;
  Changed(Self);
end;

procedure TCnJPEGImage.SaveToStream(Stream: TStream);
begin
  JPEGNeeded;
  if FModified then
    Compress;
  if FData <> nil then
    FData.SaveToStream(Stream);
end;

procedure TCnJPEGImage.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnJPEGImage.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnJPEGImage.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if GetEmpty then Exit;
  DIBNeeded;
  if FBitmap = nil then Exit;
  StretchBlt(ACanvas.Handle,
    Rect.Left, Rect.Top,
    Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
    FBitmap.Canvas.Handle,
    0, 0, FBitmap.Width, FBitmap.Height,
    SRCCOPY);
end;

procedure TCnJPEGImage.Assign(Source: TPersistent);
begin
  if Source = nil then
    Clear
  else if Source is TCnJPEGImage then
  begin
    if FData <> nil then
      FData.Release;
    FData := TCnJPEGImage(Source).FData;
    if FData <> nil then
      FData.AddRef;
    FreeBitmap;
    FCompressionQuality := TCnJPEGImage(Source).FCompressionQuality;
    FGrayscale := TCnJPEGImage(Source).FGrayscale;
    FPixelFormat := TCnJPEGImage(Source).FPixelFormat;
    FPerformance := TCnJPEGImage(Source).FPerformance;
    FProgressiveDisplay := TCnJPEGImage(Source).FProgressiveDisplay;
    FProgressiveEncoding := TCnJPEGImage(Source).FProgressiveEncoding;
    FScale := TCnJPEGImage(Source).FScale;
    FSmoothing := TCnJPEGImage(Source).FSmoothing;
    FTransparentColor := TCnJPEGImage(Source).FTransparentColor;
    FModified := False;
    Changed(Self);
  end
  else if Source is TBitmap then
  begin
    NewJPEGData;
    CreateBitmap;
    FBitmap.Assign(Source);
    FModified := True;
    Changed(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TCnJPEGImage.Compress;
var
  Encoder: TCnJPEGEncoder;
begin
  if FBitmap = nil then Exit;
  if FData = nil then
    FData := TCnJPEGData.Create
  else
    FData.Data.Clear;
  Encoder := TCnJPEGEncoder.Create;
  try
    Encoder.Encode(FBitmap, FData.Data, FCompressionQuality,
      FGrayscale, FProgressiveEncoding);
    FData.ParseHeader;
  finally
    Encoder.Free;
  end;
  FModified := False;
end;

procedure TCnJPEGImage.DIBNeeded;
var
  Decoder: TCnJPEGDecoder;
begin
  if FBitmap <> nil then Exit;
  if FData = nil then
    raise ECnJPEGException.Create(SCnErrorNoJpegDataToDecode);

  CreateBitmap;
  Decoder := TCnJPEGDecoder.Create;
  try
    Decoder.Decode(FData.Data, FBitmap, FScale, FPerformance,
      FSmoothing, FProgressiveDisplay);
  finally
    Decoder.Free;
  end;
end;

procedure TCnJPEGImage.JPEGNeeded;
begin
  if FData = nil then
  begin
    if FBitmap = nil then
      raise ECnJPEGException.Create(SCnErrorNoImageData);
    Compress;
  end;
end;

procedure TCnJPEGImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
{$IFNDEF FPC}
var
  Ptr: Pointer;
  Size: Cardinal;
  Stream: TMemoryStream;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if (AData = 0) or (CN_JPEG_CLIPBOARD_FORMAT = 0) then Exit;
  if AFormat <> CN_JPEG_CLIPBOARD_FORMAT then Exit;

  Ptr := GlobalLock(AData);
  if Ptr = nil then Exit;
  try
    Size := GlobalSize(AData);
    Stream := TMemoryStream.Create;
    try
      Stream.Write(Ptr^, Size);
      Stream.Position := 0;
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    GlobalUnlock(AData);
  end;
  {$ENDIF}
end;

procedure TCnJPEGImage.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
{$IFNDEF FPC}
var
  Stream: TMemoryStream;
  Ptr: Pointer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  Format := CN_JPEG_CLIPBOARD_FORMAT;
  Data := 0;
  APalette := 0;

  if GetEmpty then Exit;

  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    Data := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Stream.Size);
    if Data = 0 then
      raise ECnJPEGException.Create(SCnErrorCannotAllocateClipboardMemory);
    Ptr := GlobalLock(Data);
    if Ptr = nil then
    begin
      GlobalFree(Data);
      Data := 0;
      Exit;
    end;
    try
      System.Move(Stream.Memory^, Ptr^, Stream.Size);
    finally
      GlobalUnlock(Data);
    end;
  finally
    Stream.Free;
  end;
  {$ENDIF}
end;

procedure TCnJPEGImage.Clear;
begin
  FreeBitmap;
  if FData <> nil then
  begin
    FData.Release;
    FData := nil;
  end;
  FModified := False;
  Changed(Self);
end;

//============================================================================
// DFM 流支持
//============================================================================

procedure TCnJPEGImage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, True);
end;

procedure TCnJPEGImage.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TCnJPEGImage.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

//============================================================================
// 注册/注销
//============================================================================

procedure RegisterCnJPEG;
begin
  CN_JPEG_CLIPBOARD_FORMAT := RegisterClipboardFormat('JPEG');
  TPicture.RegisterFileFormat('jpg', 'JPEG Image', TCnJPEGImage);
  TPicture.RegisterFileFormat('jpeg', 'JPEG Image', TCnJPEGImage);
end;

procedure UnregisterCnJPEG;
begin
  TPicture.UnregisterGraphicClass(TCnJPEGImage);
end;

initialization
  RegisterCnJPEG;

finalization
  UnregisterCnJPEG;

end.
