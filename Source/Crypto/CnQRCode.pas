{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnQRCode;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：二维码生成单元
* 单元作者：CnPack 开发组
* 备    注：本单元实现了二维码编码与解码功能，可配合 CnQRCodeImage 控件实现绘制。
*
*           本单元不操作 VCL/FMX 中实际的 TBitmap，而采用了中间层 TCnQRGrayImage
*
*           二维码编码内部位操作使用 CnBits，但大部分是 MSB First 模式，
*           也即符合阅读习惯的高位在前，和 CnBits 里大部分底位在前不同。
*           阅读代码时需注意。
*
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.05.13 V1.1
*               增加二维码识别功能
*           2026.01.13 V1.0
*               创建单元，在 AI 帮助下实现编码并能扫描成功
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnBits, CnNative;

type
  ECnQRCodeException = class(Exception);
  {* 二维码相关异常}

  TCnErrorRecoveryLevel = (erlL, erlM, erlQ, erlH);
  {* 二维码纠错等级，分别代表 7%、15%、25%、30%}

  TCnQRCodeVersion = 1..40;
  {* 二维码版本}

  TCnQRData = array of array of Byte;
  {* 二维矩阵数据：[列, 行] 也就是 [X(Left), Y(Top)]。左上角为 [0, 0]}

  TCnQREncodeMode = (emNumeric, emAlphaNumeric, emByte, emWideChar);
  {* 二维码内容模式}

  TCnQRGFPoly = record
  {* 二维码中使用的 Reed-Solomon 多项式结构}
    Num: Integer;
    {* 多项式次数}
    Coeff: array of Byte;
    {* 多项式系数}
  end;

  TCnQRWideCharMode = (cqwUtf8, cqwAnsi);
  {* 宽字符的编码模式，默认 Utf8。后者直接转 Ansi，如是汉字则是 GB18030 编码}

  // 以下类型用于二维码解码
  TCnQRFormatInfo = packed record
  {* QR码格式信息，包含纠错级别和掩码类型（0..7）}
    ErrorLevel: TCnErrorRecoveryLevel;
    MaskType: Integer;
  end;

  TCnQRFinderPattern = packed record
  {* 寻像图案记录，包含中心坐标、估算模块尺寸和确认计数}
    X: Double;
    Y: Double;
    EstimatedModuleSize: Double;
    ConfirmedCount: Integer;
  end;

  TCnQRAlignmentPattern = packed record
  {* 对齐图案记录，包含中心坐标和估算模块尺寸}
    X: Double;
    Y: Double;
    EstimatedModuleSize: Double;
  end;

  TCnQRPerspectiveTransform = packed record
  {* 3x3 透视变换矩阵（单精度浮点数），用于四边形到四边形映射}
    a11, a12, a13: Single;
    a21, a22, a23: Single;
    a31, a32, a33: Single;
  end;

  TCnQRDecodeMode = (
  {* 解码模式枚举，对应 QR 码规范中 4 位模式指示符的各种编码模式}
    qrmTerminator,
    qrmNumeric,
    qrmAlphaNumeric,
    qrmStructuredAppend,
    qrmByte,
    qrmECI,
    qrmKanji,
    qrmFNC1First,
    qrmFNC1Second,
    qrmHanzi
  );

  TDataBlockArray = array of TBytes;
  {* 数据块数组，每个元素为一个数据块的字节序列}

  TCnQRPointF = packed record
  {* 浮点坐标点，用于透视变换中的坐标表示}
    X: Double;
    Y: Double;
  end;

  TCnQRDecoder = class
  {* 二维码解码器类，负责从 TCnQRData 矩阵解码出文本内容}
  private
    FQRData: TCnQRData;
    FQRSize: Integer;
    FFormatInfo: TCnQRFormatInfo;
    FQRVersion: TCnQRCodeVersion;
    FIsMirrored: Boolean;
    FErrorMessage: string;

    // 格式信息解码
    function ReadFormatInformation: Boolean;
    function DecodeFormatInfo(MaskedInfo1, MaskedInfo2: Integer): Boolean;

    // 版本信息解码
    function ReadVersion: Boolean;
    function ExtractVersionBits(AStartCol, AStartRow: Integer): Integer;
    function DecodeVersion(VersionBits1, VersionBits2: Integer): Boolean;

    // 去掩码
    function GetMaskPattern(X, Y, MaskType: Integer): Boolean;
    function IsFunctionArea(X, Y: Integer): Boolean;
    procedure UnmaskMatrix;

    // 码字读取
    function ReadCodewords: TBytes;

    // 数据块划分
    procedure SplitDataBlocks(const RawCodewords: TBytes;
      var Blocks: array of TBytes; var BlockCount: Integer;
      var ECCPerBlock: Integer);

    // RS 纠错
    function RSDecodeBlock(var Data: TBytes; ECCount: Integer): Boolean;
    function GFMul(A, B: Integer): Integer;
    function GFDiv(A, B: Integer): Integer;
    function GFExp(N: Integer): Integer;
    function GFLog(N: Integer): Integer;
    function GFInv(N: Integer): Integer;

    // 比特流解析
    function DecodeDataStream(const DataBytes: TBytes): string;
    function ReadBits(var BitPos: Integer; NumBits: Integer;
      const DataBytes: TBytes): Integer;
    function GetCharCountBits(AMode: TCnQRDecodeMode): Integer;
    function DecodeNumericSegment(var BitPos: Integer; ACount: Integer;
      const DataBytes: TBytes): string;
    function DecodeAlphaNumericSegment(var BitPos: Integer; ACount: Integer;
      const DataBytes: TBytes): string;
    function DecodeByteSegment(var BitPos: Integer; ACount: Integer;
      const DataBytes: TBytes): string;
    function DecodeKanjiSegment(var BitPos: Integer; ACount: Integer;
      const DataBytes: TBytes): string;
    function DecodeHanziSegment(var BitPos: Integer; ACount: Integer;
      const DataBytes: TBytes): string;

    // 镜像处理
    procedure MirrorMatrix;

  public
    constructor Create;
    function DecodeMatrix(const AQRData: TCnQRData): string;

    property IsMirrored: Boolean read FIsMirrored;
    {* 是否通过镜像重试解码成功}
    property QRVersion: TCnQRCodeVersion read FQRVersion;
    {* 解码得到的二维码版本号}
    property FormatInfo: TCnQRFormatInfo read FFormatInfo;
    {* 解码得到的格式信息（纠错级别和掩码类型）}
    property ErrorMessage: string read FErrorMessage;
    {* 最后一次解码失败的错误信息}
  end;

  TCnQREncoder = class
  {* 二维码编码实现类}
  private
    FText: string;
    FRawText: AnsiString;
    FQRData: TCnQRData;
    FQRSize: Integer;

    FQRErrorRecoveryLevel: TCnErrorRecoveryLevel;  // 纠错级别 2 位
    FMaskType: Integer;                            // 蒙版类型 3 位
    FBitsFormatInfo: TCnBitBuilder;                // 15 位，由上面俩加上 10 位 BCH 纠错码组成

    FQRVersion: TCnQRCodeVersion;                  // 版本号 6 位
    FBitsVersionInfo: TCnBitBuilder;               // 18 位，由上面俩加上 12 位 BCH 纠错码组成

    FDataBits: TCnBitBuilder;
    FFinalBits: TCnBitBuilder;
    FQRWideCharMode: TCnQRWideCharMode;

    procedure SetQRErrorRecoveryLevel(const Value: TCnErrorRecoveryLevel);
    procedure SetQRVersion(const Value: TCnQRCodeVersion);
    procedure SetText(const Value: string);
    function GetQRSize: Integer;

    procedure UpdateRawText;
    procedure UpdateFormatInfoBits;
    procedure UpdateVersionInfoBits;
    procedure PaintRect(Left, Top, Right, Bottom: Integer; Solid: Boolean = False);
    procedure SetQRWideCharMode(const Value: TCnQRWideCharMode);
    procedure ClearData;

    // 绘制位置探测图形
    procedure PaintPositionDetectionPattern;

    // 绘制对齐图形
    procedure PaintAlignmentPattern;

    // 绘制时序图形
    procedure PaintTimingPattern;

    // 绘制格式信息
    procedure PaintFormatInformation;

    // 绘制版本信息
    procedure PaintVersionInformation;

    // 计算并应用最佳 Mask 掩码
    procedure PaintMaskCoding;

    // 数据编码相关方法
    function AnalyzeRawText: TCnQREncodeMode;
    function EncodeNumeric(const AText: AnsiString): TCnBitBuilder;
    function EncodeAlphaNumeric(const AText: AnsiString): TCnBitBuilder;
    function EncodeByte(const AText: AnsiString): TCnBitBuilder;
    function GetCharCountBits(Mode: TCnQREncodeMode; Version: TCnQRCodeVersion): Integer;
    {* 根据编码模式和版本获取字符计数指示符的位长度}
    procedure EncodeText;

    function PolyMult(A, B: TCnQRGFPoly): TCnQRGFPoly;
    {* 多项式乘法运算}
    function PolyMod(Dividend, Divisor: TCnQRGFPoly): TCnQRGFPoly;
    {* 多项式除法运算}
    function GetGeneratorPoly(Num: Integer): TCnQRGFPoly;
    {* 获取指定次数的生成多项式，用于 Reed-Solomon 编码}

    // 数据放置和掩码
    procedure PlaceDataBits;
    procedure ApplyMask(MaskType: Integer);
    {* 应用指定的掩码类型到数据}
    function EvaluateMask(MaskType: Integer): Integer;
    {* 评估指定掩码类型的优劣}
    function GetMaskPattern(X, Y, MaskType: Integer): Boolean;
    {* 获取指定位置的掩码值}

    // BCH 编码
    function BCHEncode15(Data: Integer): Integer;
    function BCHEncode18(Data: Integer): Integer;
    function GetFormatBits(ErrorLevel: TCnErrorRecoveryLevel; MaskType: Integer): Integer;
    {* 获取格式信息位}
    function GetVersionBits(Version: TCnQRCodeVersion): Integer;
    {* 获取版本信息位}

    // 版本相关参数查询
    function GetTotalCodewords(Version: TCnQRCodeVersion): Integer;
    function GetECCodewords(Version: TCnQRCodeVersion; ErrorLevel:
      TCnErrorRecoveryLevel): Integer;
    function GetOptimalVersion(const AText: AnsiString; ErrorLevel:
      TCnErrorRecoveryLevel): TCnQRCodeVersion;

    // 功能区域判断
    function IsFunctionArea(X, Y: Integer): Boolean;

    function AddEccAndInterleave(Data: TCnBitBuilder): TCnBitBuilder;
    {* 添加纠错码并对数据进行交织}
    function ComputeBlockECC(const DataBytes: TBytes; ECCodewords: Integer): TBytes;
    {* 计算数据块的纠错码}
    function BitBuilderToBytes(B: TCnBitBuilder; ByteCount: Integer): TBytes;
    {* 将位构建器转换为字节数组}

    procedure PaintData;
    {* 绘制数据到二维码矩阵}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    // 调试相关输出
    function GetDataCodewordsBytes: TBytes;
    {* 获取数据码字的字节数组}
    function GetAllCodewordsBytes: TBytes;
    {* 获取所有码字（含纠错码）的字节数组}
    function GetFormatBitsValue: Integer;
    {* 获取格式信息的位值}
    function GetVersionBitsValue: Integer;
    {* 获取版本信息的位值}

    function DumpMatrix: string;
    {* 导出二维码矩阵为字符串（应用掩码后）}
    function DumpMatrixUnmasked: string;
    {* 导出二维码矩阵为字符串（应用掩码前）}
    function DumpFunctionArea: string;
    {* 导出功能区域为字符串}
    function DumpFormatRowInfo: string;
    {* 导出格式行信息为字符串}
    function DumpFormatColInfo: string;
    {* 导出格式列信息为字符串}
    function DumpAlignmentCenters: string;
    {* 导出对齐图形中心坐标为字符串}
    function DumpFormatRowIndices: string;
    {* 导出格式行索引为字符串}
    function DumpFormatColIndices: string;
    {* 导出格式列索引为字符串}

    // 对外公开的属性
    property QRVersion: TCnQRCodeVersion read FQRVersion write SetQRVersion;
    {* 二维码尺寸版本}
    property QRErrorRecoveryLevel: TCnErrorRecoveryLevel read
      FQRErrorRecoveryLevel write SetQRErrorRecoveryLevel;
    {* 二维码纠错等级}
    property Text: string read FText write SetText;
    {* 指定的文本供生成二维码}
    property QRWideCharMode: TCnQRWideCharMode read FQRWideCharMode write SetQRWideCharMode;
    {* 宽字符编码模式}
    property QRSize: Integer read GetQRSize;
    {* 生成的二维码的边长格子数}

    property QRData: TCnQRData read FQRData;
    {* 生成的二维码数据，是一个维度为 QRSize 的二维数组，每个元素 1 字节，为 1 代表黑格，否则白格}
    property MaskType: Integer read FMaskType;
    {* 掩码类型，有八种}
  end;

// ============================== 解码全局函数 =================================

// 阶段一：矩阵解码
function CnQRDecodeFromMatrix(const AQRData: TCnQRData): string;
{* 从 TCnQRData 矩阵解码出文本。成功返回解码文本，失败抛出 ECnQRCodeException 异常}

// 阶段二：图像检测
function CnQRBinarize(const AGrayImage: TCnQRData): TCnQRData;
{* 将灰度图像二值化输出 0/1 矩阵。输入输出均为 TCnQRData，尺寸由数组维度确定}

function CnQRFindFinderPatterns(const ABinarized: TCnQRData;
  out TopLeft, TopRight, BottomLeft: TCnQRFinderPattern): Boolean;
{* 在二值矩阵中定位三个寻像图案。成功返回 True 并填充三个图案坐标}

function CnQRFindAlignmentPattern(const ABinarized: TCnQRData;
  AWidth, AHeight: Integer; const TopLeft, TopRight,
  BottomLeft: TCnQRFinderPattern;
  out AlignmentPattern: TCnQRAlignmentPattern): Boolean;
{* 在二值矩阵中定位对齐图案。版本<=1 或未找到时返回 False}

function CnQRCalcPerspectiveTransform(const SrcPoints: array of TCnQRPointF;
  const DstPoints: array of TCnQRPointF): TCnQRPerspectiveTransform;
{* 根据源四边形 4 点和目标四边形 4 点计算 3x3 透视变换矩阵（Wolberg 算法）}

function CnQRTransformPoint(const Transform: TCnQRPerspectiveTransform;
  X, Y: Single): TCnQRPointF;
{* 对单点执行透视变换}

function CnQRSampleGrid(const ABinarized: TCnQRData;
  const Transform: TCnQRPerspectiveTransform;
  ADimension: Integer): TCnQRData;
{* 根据透视变换对二值矩阵进行网格采样，输出 ADimension x ADimension 的规范化矩阵}

function CnQRCalcModuleSize(const TopLeft, TopRight,
  BottomLeft: TCnQRFinderPattern): Double;
{* 估算模块尺寸（像素）}

function CnQRCalcDimension(const TopLeft, TopRight,
  BottomLeft: TCnQRFinderPattern; ModuleSize: Double): Integer;
{* 估算二维码矩阵维度}

// 阶段三：端到端
function CnQRDecodeFromGrayImage(const AGrayImage: TCnQRData): string;
{* 从灰度图像端到端解码二维码文本。内部串联二值化→检测→采样→矩阵解码}

implementation

uses
  CnWideStrings;

resourcestring
  SCnErrorQRRowY8FirstCount = 'Row y=8: First=%d, Count=%d, Size=%d, Threshold(Size-8)=%d';
  SCnErrorQRColX8FirstCount = 'Col x=8: First=%d, Count=%d, Size=%d, Threshold(Size-8)=%d';
  SCnErrorQRMatrixSizeTooSmall = 'Matrix Size Too Small';
  SCnErrorQRFormatInformationDecodeFailed = 'Format Information Decode Failed';
  SCnErrorQRVersionInformationDecodeFailed = 'Version Information Decode Failed';
  SCnErrorQRChecksumErrorInBlock = 'Checksum Error in Block %d';
  SCnErrorQRQrDecodeFailed = 'QR Decode Failed';
  SCnErrorQRImageTooSmallMin21X21 = 'Image Too Small (Min 21x21)';
  SCnErrorQRNoFinderPatternsFound = 'No Finder Patterns Found';
  SCnErrorQRModuleSizeTooSmall = 'Module Size Too Small';

type
  TCn2BytesArray = array[0..1] of Byte;

  TCn3BytesArray = array[0..2] of Byte;

  TCn4BytesArray = array[0..3] of Byte;

  TCn5BytesArray = array[0..4] of Byte;

  TCn6BytesArray = array[0..5] of Byte;

  TCn7BytesArray = array[0..6] of Byte;

  PCn2BytesArray = ^TCn2BytesArray;

  PCn3BytesArray = ^TCn3BytesArray;

  PCn4BytesArray = ^TCn4BytesArray;

  PCn5BytesArray = ^TCn5BytesArray;

  PCn6BytesArray = ^TCn6BytesArray;

  PCn7BytesArray = ^TCn7BytesArray;

resourcestring
  SCnErrorQRCodeDataTooLong = 'Data too long for QR Code Version 40';

const
  CN_QRCODE_FORMATINFO_LENGTH = 15;

  CN_QRCODE_VERSIONINFO_LENGTH = 18;

  CN_MASK_FORMATINFO: array[0..CN_QRCODE_FORMATINFO_LENGTH - 1] of Byte =
    (1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0);

  // 对齐块在不同版本下的坐标位置
  CN_ALIGNMENT_PATTERN_COORDINATES_V2: array[0..1] of Byte =
    (6, 18);
  CN_ALIGNMENT_PATTERN_COORDINATES_V3: array[0..1] of Byte =
    (6, 22);
  CN_ALIGNMENT_PATTERN_COORDINATES_V4: array[0..1] of Byte =
    (6, 26);
  CN_ALIGNMENT_PATTERN_COORDINATES_V5: array[0..1] of Byte =
    (6, 30);
  CN_ALIGNMENT_PATTERN_COORDINATES_V6: array[0..1] of Byte =
    (6, 34);
  CN_ALIGNMENT_PATTERN_COORDINATES_V7: array[0..2] of Byte =
    (6, 22, 38);
  CN_ALIGNMENT_PATTERN_COORDINATES_V8: array[0..2] of Byte =
    (6, 24, 42);
  CN_ALIGNMENT_PATTERN_COORDINATES_V9: array[0..2] of Byte =
    (6, 26, 46);
  CN_ALIGNMENT_PATTERN_COORDINATES_V10: array[0..2] of Byte =
    (6, 28, 50);
  CN_ALIGNMENT_PATTERN_COORDINATES_V11: array[0..2] of Byte =
    (6, 30, 54);
  CN_ALIGNMENT_PATTERN_COORDINATES_V12: array[0..2] of Byte =
    (6, 32, 58);
  CN_ALIGNMENT_PATTERN_COORDINATES_V13: array[0..2] of Byte =
    (6, 34, 62);
  CN_ALIGNMENT_PATTERN_COORDINATES_V14: array[0..3] of Byte =
    (6, 26, 46, 66);
  CN_ALIGNMENT_PATTERN_COORDINATES_V15: array[0..3] of Byte =
    (6, 26, 48, 70);
  CN_ALIGNMENT_PATTERN_COORDINATES_V16: array[0..3] of Byte =
    (6, 26, 50, 74);
  CN_ALIGNMENT_PATTERN_COORDINATES_V17: array[0..3] of Byte =
    (6, 30, 54, 78);
  CN_ALIGNMENT_PATTERN_COORDINATES_V18: array[0..3] of Byte =
    (6, 30, 56, 82);
  CN_ALIGNMENT_PATTERN_COORDINATES_V19: array[0..3] of Byte =
    (6, 30, 58, 86);
  CN_ALIGNMENT_PATTERN_COORDINATES_V20: array[0..3] of Byte =
    (6, 34, 62, 90);
  CN_ALIGNMENT_PATTERN_COORDINATES_V21: array[0..4] of Byte =
    (6, 28, 50, 72, 94);
  CN_ALIGNMENT_PATTERN_COORDINATES_V22: array[0..4] of Byte =
    (6, 26, 50, 74, 98);
  CN_ALIGNMENT_PATTERN_COORDINATES_V23: array[0..4] of Byte =
    (6, 30, 54, 78, 102);
  CN_ALIGNMENT_PATTERN_COORDINATES_V24: array[0..4] of Byte =
    (6, 28, 54, 80, 106);
  CN_ALIGNMENT_PATTERN_COORDINATES_V25: array[0..4] of Byte =
    (6, 32, 58, 84, 110);
  CN_ALIGNMENT_PATTERN_COORDINATES_V26: array[0..4] of Byte =
    (6, 30, 58, 86, 114);
  CN_ALIGNMENT_PATTERN_COORDINATES_V27: array[0..4] of Byte =
    (6, 34, 62, 90, 118);
  CN_ALIGNMENT_PATTERN_COORDINATES_V28: array[0..5] of Byte =
    (6, 26, 50, 74, 98, 122);
  CN_ALIGNMENT_PATTERN_COORDINATES_V29: array[0..5] of Byte =
    (6, 30, 54, 78, 102, 126);
  CN_ALIGNMENT_PATTERN_COORDINATES_V30: array[0..5] of Byte =
    (6, 26, 52, 78, 104, 130);
  CN_ALIGNMENT_PATTERN_COORDINATES_V31: array[0..5] of Byte =
    (6, 30, 56, 82, 108, 134);
  CN_ALIGNMENT_PATTERN_COORDINATES_V32: array[0..5] of Byte =
    (6, 34, 60, 86, 112, 138);
  CN_ALIGNMENT_PATTERN_COORDINATES_V33: array[0..5] of Byte =
    (6, 30, 58, 86, 114, 142);
  CN_ALIGNMENT_PATTERN_COORDINATES_V34: array[0..5] of Byte =
    (6, 34, 62, 90, 118, 146);
  CN_ALIGNMENT_PATTERN_COORDINATES_V35: array[0..6] of Byte =
    (6, 30, 54, 78, 102, 126, 150);
  CN_ALIGNMENT_PATTERN_COORDINATES_V36: array[0..6] of Byte =
    (6, 24, 50, 76, 102, 128, 154);
  CN_ALIGNMENT_PATTERN_COORDINATES_V37: array[0..6] of Byte =
    (6, 28, 54, 80, 106, 132, 158);
  CN_ALIGNMENT_PATTERN_COORDINATES_V38: array[0..6] of Byte =
    (6, 32, 58, 84, 110, 136, 162);
  CN_ALIGNMENT_PATTERN_COORDINATES_V39: array[0..6] of Byte =
    (6, 26, 54, 82, 110, 138, 166);
  CN_ALIGNMENT_PATTERN_COORDINATES_V40: array[0..6] of Byte =
    (6, 30, 58, 86, 114, 142, 170);

  CN_ALIGNMENT_PATTERN_2ARRAY: array[2..6] of PCn2BytesArray = (
    @CN_ALIGNMENT_PATTERN_COORDINATES_V2,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V3,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V4,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V5,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V6
  );

  CN_ALIGNMENT_PATTERN_3ARRAY: array[7..13] of PCn3BytesArray = (
    @CN_ALIGNMENT_PATTERN_COORDINATES_V7,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V8,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V9,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V10,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V11,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V12,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V13
  );

  CN_ALIGNMENT_PATTERN_4ARRAY: array[14..20] of PCn4BytesArray = (
    @CN_ALIGNMENT_PATTERN_COORDINATES_V14,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V15,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V16,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V17,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V18,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V19,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V20
  );

  CN_ALIGNMENT_PATTERN_5ARRAY: array[21..27] of PCn5BytesArray = (
    @CN_ALIGNMENT_PATTERN_COORDINATES_V21,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V22,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V23,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V24,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V25,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V26,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V27
  );

  CN_ALIGNMENT_PATTERN_6ARRAY: array[28..34] of PCn6BytesArray = (
    @CN_ALIGNMENT_PATTERN_COORDINATES_V28,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V29,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V30,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V31,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V32,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V33,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V34
  );

  CN_ALIGNMENT_PATTERN_7ARRAY: array[35..40] of PCn7BytesArray = (
    @CN_ALIGNMENT_PATTERN_COORDINATES_V35,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V36,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V37,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V38,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V39,
    @CN_ALIGNMENT_PATTERN_COORDINATES_V40
  );

const
  CN_QRCODE_CHARSET_NUMERIC =['0'..'9'];
  CN_QRCODE_CHARSET_ALPHANUMERIC =['0'..'9', 'A'.. 'Z', ' ', '$', '%',
    '+', '-', '.', '/', ':'];
  CN_QRCODE_CHARSET_BYTE =[ #0.. #255] - CN_QRCODE_CHARSET_ALPHANUMERIC;

  // 纠错等级对应值 (L=01, M=00, Q=11, H=10)
  CN_ERROR_LEVEL_BITS: array[TCnErrorRecoveryLevel] of Integer = (1, 0, 3, 2);

  // 字母数字模式编码表
  CN_ALPHANUMERIC_ENCODING: array[0..44] of Integer = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,            // '0'-'9'
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,  // 'A'-'J'
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,  // 'K'-'T'
    30, 31, 32, 33, 34, 35, 36, 37, 38, 39,  // 'U'-'Z'
    40, 41, 42, 43, 44                       // ' ', '$', '%', '*', '+', '-', '.', '/', ':'
  );

  // 字符计数指示符的位长
  CN_CHAR_COUNT_BITS: array[1..40, 0..2] of Integer =(
    // Version 1-9
    (10, 9, 8), (10, 9, 8), (10, 9, 8), (10, 9, 8), (10, 9, 8),
    (10, 9, 8), (10, 9, 8), (10, 9, 8), (10, 9, 8),
    // Version 10-26
    (12, 11, 16), (12, 11, 16), (12, 11, 16), (12, 11, 16), (12, 11, 16),
    (12, 11, 16), (12, 11, 16), (12, 11, 16), (12, 11, 16), (12, 11, 16),
    (12, 11, 16), (12, 11, 16), (12, 11, 16), (12, 11, 16), (12, 11, 16),
    (12, 11, 16), (12, 11, 16),
    // Version 27-40
    (14, 13, 16), (14, 13, 16), (14, 13, 16), (14, 13, 16), (14, 13, 16),
    (14, 13, 16), (14, 13, 16), (14, 13, 16), (14, 13, 16), (14, 13, 16),
    (14, 13, 16), (14, 13, 16), (14, 13, 16), (14, 13, 16)
  );

  // 数据码字总数
  CN_TOTAL_CODEWORDS: array[1..40] of Integer = (
    26, 44, 70, 100, 134, 172, 196, 242, 292, 346,
    404, 466, 532, 581, 655, 733, 815, 901, 991, 1085,
    1156, 1258, 1364, 1474, 1588, 1706, 1828, 1921, 2051, 2185,
    2323, 2465, 2611, 2761, 2876, 3034, 3196, 3362, 3532, 3706
  );

  // 纠错码字数量 [版本, 纠错等级]
  CN_EC_CODEWORDS: array[1..40, 0..3] of Integer =(
    // Version 1
    (7, 10, 13, 17),
    // Version 2
    (10, 16, 22, 28),
    // Version 3
    (15, 26, 36, 44),
    // Version 4
    (20, 36, 48, 64),
    // Version 5
    (26, 48, 64, 80),
    // Version 6
    (36, 64, 72, 96),
    // Version 7
    (40, 72, 88, 112),
    // Version 8
    (48, 88, 110, 136),
    // Version 9
    (60, 110, 132, 168),
    // Version 10
    (72, 130, 156, 192),
    // Version 11
    (80, 150, 192, 224),
    // Version 12
    (96, 176, 224, 264),
    // Version 13
    (104, 198, 260, 308),
    // Version 14
    (120, 216, 288, 352),
    // Version 15
    (132, 240, 320, 384),
    // Version 16
    (144, 280, 352, 432),
    // Version 17
    (168, 308, 384, 480),
    // Version 18
    (180, 338, 416, 512),
    // Version 19
    (196, 364, 444, 576),
    // Version 20
    (224, 416, 476, 640),
    // Version 21
    (224, 442, 504, 672),
    // Version 22
    (252, 476, 560, 704),
    // Version 23
    (270, 504, 588, 768),
    // Version 24
    (300, 560, 644, 848),
    // Version 25
    (312, 588, 676, 904),
    // Version 26
    (336, 644, 724, 980),
    // Version 27
    (360, 672, 792, 1056),
    // Version 28
    (390, 720, 840, 1120),
    // Version 29
    (420, 750, 882, 1200),
    // Version 30
    (450, 816, 936, 1260),
    // Version 31
    (480, 900, 984, 1440),
    // Version 32
    (510, 960, 1050, 1530),
    // Version 33
    (540, 1008, 1116, 1620),
    // Version 34
    (570, 1050, 1188, 1740),
    // Version 35
    (570, 1116, 1248, 1860),
    // Version 36
    (600, 1188, 1284, 1980),
    // Version 37
    (630, 1212, 1428, 2016),
    // Version 38
    (660, 1278, 1452, 2100),
    // Version 39
    (720, 1332, 1518, 2232),
    // Version 40
    (750, 1410, 1590, 2364)
  );

  CN_ECC_CODEWORDS_PER_BLOCK: array[1..40, 0..3] of Integer = (
    (7, 10, 13, 17),
    (10, 16, 22, 28),
    (15, 26, 18, 22),
    (20, 18, 26, 16),
    (26, 24, 18, 22),
    (18, 16, 24, 28),
    (20, 18, 18, 26),
    (24, 22, 22, 26),
    (30, 22, 20, 24),
    (18, 26, 28, 28),
    (20, 30, 24, 24),
    (24, 22, 26, 28),
    (26, 22, 24, 22),
    (30, 24, 20, 24),
    (22, 24, 30, 24),
    (24, 28, 24, 30),
    (28, 28, 28, 28),
    (30, 26, 28, 28),
    (28, 26, 26, 26),
    (28, 26, 30, 28),
    (28, 26, 28, 30),
    (28, 28, 30, 24),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (26, 28, 30, 30),
    (28, 28, 28, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30),
    (30, 28, 30, 30)
  );

  CN_NUM_ERROR_CORRECTION_BLOCKS: array[1..40, 0..3] of Integer = (
    (1, 1, 1, 1),
    (1, 1, 1, 1),
    (1, 1, 2, 2),
    (1, 2, 2, 4),
    (1, 2, 4, 4),
    (2, 4, 4, 4),
    (2, 4, 6, 5),
    (2, 4, 6, 6),
    (2, 5, 8, 8),
    (4, 5, 8, 8),
    (4, 5, 8, 11),
    (4, 8, 10, 11),
    (4, 9, 12, 16),
    (4, 9, 16, 16),
    (6, 10, 12, 18),
    (6, 10, 17, 16),
    (6, 11, 16, 19),
    (6, 13, 18, 21),
    (7, 14, 21, 25),
    (8, 16, 20, 25),
    (8, 17, 23, 25),
    (9, 17, 23, 34),
    (9, 18, 25, 30),
    (10, 20, 27, 32),
    (12, 21, 29, 35),
    (12, 23, 34, 37),
    (12, 25, 34, 40),
    (13, 26, 35, 42),
    (14, 28, 38, 45),
    (15, 29, 40, 48),
    (16, 31, 43, 51),
    (17, 33, 45, 54),
    (18, 35, 48, 57),
    (19, 37, 51, 60),
    (19, 38, 53, 63),
    (20, 40, 56, 66),
    (21, 43, 59, 70),
    (22, 45, 62, 74),
    (24, 47, 65, 77),
    (25, 49, 68, 81)
  );

  // 对数表和指数表 (用于Reed-Solomon编码 GF(2^8))
  CN_LOG_TABLE: array[0..255] of Integer = (
    -1, 0, 1, 25, 2, 50, 26, 198, 3, 223, 51, 238, 27, 104, 199, 75,
    4, 100, 224, 14, 52, 141, 239, 129, 28, 193, 105, 248, 200, 8, 76, 113,
    5, 138, 101, 47, 225, 36, 15, 33, 53, 147, 142, 218, 240, 18, 130, 69,
    29, 181, 194, 125, 106, 39, 249, 185, 201, 154, 9, 120, 77, 228, 114, 166,
    6, 191, 139, 98, 102, 221, 48, 253, 226, 152, 37, 179, 16, 145, 34, 136,
    54, 208, 148, 206, 143, 150, 219, 189, 241, 210, 19, 92, 131, 56, 70, 64,
    30, 66, 182, 163, 195, 72, 126, 110, 107, 58, 40, 84, 250, 133, 186, 61,
    202, 94, 155, 159, 10, 21, 121, 43, 78, 212, 229, 172, 115, 243, 167, 87,
    7, 112, 192, 247, 140, 128, 99, 13, 103, 74, 222, 237, 49, 197, 254, 24,
    227, 165, 153, 119, 38, 184, 180, 124, 17, 68, 146, 217, 35, 32, 137, 46,
    55, 63, 209, 91, 149, 188, 207, 205, 144, 135, 151, 178, 220, 252, 190, 97,
    242, 86, 211, 171, 20, 42, 93, 158, 132, 60, 57, 83, 71, 109, 65, 162,
    31, 45, 67, 216, 183, 123, 164, 118, 196, 23, 73, 236, 127, 12, 111, 246,
    108, 161, 59, 82, 41, 157, 85, 170, 251, 96, 134, 177, 187, 204, 62, 90,
    203, 89, 95, 176, 156, 169, 160, 81, 11, 245, 22, 235, 122, 117, 44, 215,
    79, 174, 213, 233, 230, 231, 173, 232, 116, 214, 244, 234, 168, 80, 88, 175
  );

  CN_EXP_TABLE: array[0..303] of Integer = (
    1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135, 19, 38,
    76, 152, 45, 90, 180, 117, 234, 201, 143, 3, 6, 12, 24, 48, 96, 192,
    157, 39, 78, 156, 37, 74, 148, 53, 106, 212, 181, 119, 238, 193, 159, 35,
    70, 140, 5, 10, 20, 40, 80, 160, 93, 186, 105, 210, 185, 111, 222, 161,
    95, 190, 97, 194, 153, 47, 94, 188, 101, 202, 137, 15, 30, 60, 120, 240,
    253, 231, 211, 187, 107, 214, 177, 127, 254, 225, 223, 163, 91, 182, 113,
    226, 217, 175, 67, 134, 17, 34, 68, 136, 13, 26, 52, 104, 208, 189, 103,
    206, 129, 31, 62, 124, 248, 237, 199, 147, 59, 118, 236, 197, 151, 51, 102,
    204, 133, 23, 46, 92, 184, 109, 218, 169, 79, 158, 33, 66, 132, 21, 42,
    84, 168, 77, 154, 41, 82, 164, 85, 170, 73, 146, 57, 114, 228, 213, 183,
    115, 230, 209, 191, 99, 198, 145, 63, 126, 252, 229, 215, 179, 123, 246,
    241, 255, 227, 219, 171, 75, 150, 49, 98, 196, 149, 55, 110, 220, 165,
    87, 174, 65, 130, 25, 50, 100, 200, 141, 7, 14, 28, 56, 112, 224, 221,
    167, 83, 166, 81, 162, 89, 178, 121, 242, 249, 239, 195, 155, 43, 86, 172,
    69, 138, 9, 18, 36, 72, 144, 61, 122, 244, 245, 247, 243, 251, 235, 203,
    139, 11, 22, 44, 88, 176, 125, 250, 233, 207, 131, 27, 54, 108, 216, 173,
    71, 142, 1, 2, 4, 8, 16, 32, 64, 128, 29, 58, 116, 232, 205, 135, 19, 38,
    76, 152, 45, 90, 180, 117, 234, 201, 143, 3, 6, 12, 24, 48, 96, 192, 157,
    39, 78, 156, 37, 74, 148, 53, 106, 212, 181, 119, 238, 193, 159, 35, 70
  );

  // 格式信息解码查找表：已异或掩码 0x5412 的 BCH 码字 -> 格式信息值
  // 原始数据 bits[4..3]=纠错级别(01=L,00=M,11=Q,10=H), bits[2..0]=掩码类型
  CN_FORMAT_INFO_DECODE_LOOKUP: array[0..31, 0..1] of Integer = (
    ($5412, $00), ($5125, $01), ($5E7C, $02), ($5B4B, $03),
    ($45F9, $04), ($40CE, $05), ($4F97, $06), ($4AA0, $07),
    ($77C4, $08), ($72F3, $09), ($7DAA, $0A), ($789D, $0B),
    ($662F, $0C), ($6318, $0D), ($6C41, $0E), ($6976, $0F),
    ($1689, $10), ($13BE, $11), ($1CE7, $12), ($19D0, $13),
    ($0762, $14), ($0255, $15), ($0D0C, $16), ($083B, $17),
    ($355F, $18), ($3068, $19), ($3F31, $1A), ($3A06, $1B),
    ($24B4, $1C), ($2183, $1D), ($2EDA, $1E), ($2BED, $1F));
  {* 格式信息解码查找表，用于汉明距离匹配。第 0 列为已异或掩码的 15 位 BCH 码字，
     第 1 列为原始 5 位数据（高 2 位=纠错级别，低 3 位=掩码类型）}

  // 版本信息解码查找表（索引 0 对应版本 7，按顺序到版本 40）
  CN_VERSION_DECODE_INFO: array[0..33] of Integer = (
    $07C94, $085BC, $09A99, $0A4D3, $0BBF6,
    $0C762, $0D847, $0E60D, $0F928, $10B78,
    $1145D, $12A17, $13532, $149A6, $15683,
    $168C9, $177EC, $18EC4, $191E1, $1AFAB,
    $1B08E, $1CC1A, $1D33F, $1ED75, $1F250,
    $209D5, $216F0, $228BA, $2379F, $24B0B,
    $2542E, $26A64, $27541, $28C69);
  {* 版本信息解码查找表，用于汉明距离匹配。
     索引 0 对应版本 7，索引 33 对应版本 40。
     值为 18 位 BCH 编码后的版本信息码字。}

function GetQRSizeFromVersion(Version: TCnQRCodeVersion): Integer;
begin
  Result := Version * 4 + 17;
end;

{ TCnQREncoder }

procedure TCnQREncoder.ClearData;
var
  I, J: Integer;
begin
  for I := 0 to QRSize - 1 do
  begin
    for J := 0 to QRSize - 1 do
      FQRData[I, J] := 0;
  end;
end;

constructor TCnQREncoder.Create;
begin
  inherited;
  FText := 'CnPack Sample QR Code.';
  UpdateRawText;

  FQRErrorRecoveryLevel := erlM;
  FMaskType := 0;
  FQRVersion := GetOptimalVersion(FRawText, FQRErrorRecoveryLevel);
  FQRSize := GetQRSizeFromVersion(FQRVersion);

  // 确保矩阵在操作前已正确调整大小
  SetLength(FQRData, FQRSize, FQRSize);

  FBitsFormatInfo := TCnBitBuilder.Create;
  FBitsFormatInfo.BitLength := CN_QRCODE_FORMATINFO_LENGTH;

  FBitsVersionInfo := TCnBitBuilder.Create;
  FBitsVersionInfo.BitLength := CN_QRCODE_VERSIONINFO_LENGTH;

  FDataBits := TCnBitBuilder.Create;
  FFinalBits := TCnBitBuilder.Create;

  UpdateFormatInfoBits;
  UpdateVersionInfoBits;

  // 初始绘制
  PaintData;
end;

destructor TCnQREncoder.Destroy;
begin
  FBitsVersionInfo.Free;
  FBitsFormatInfo.Free;
  FDataBits.Free;
  FFinalBits.Free;
  SetLength(FQRData, 0);
  inherited;
end;

function TCnQREncoder.GetQRSize: Integer;
begin
  Result := FQRSize;
end;

procedure TCnQREncoder.PaintAlignmentPattern;
var
  I, J: Integer;
  Arr2Ptr: PCn2BytesArray;
  Arr3Ptr: PCn3BytesArray;
  Arr4Ptr: PCn4BytesArray;
  Arr5Ptr: PCn5BytesArray;
  Arr6Ptr: PCn6BytesArray;
  Arr7Ptr: PCn7BytesArray;

  procedure PaintAlignment(Left, Top: Integer);
  begin
    PaintRect(Left, Top, Left + 4, Top + 4);
    FQRData[Left + 2, Top + 2] := 1;
  end;

begin
  case FQRVersion of
    2..6:
      begin
        Arr2Ptr := CN_ALIGNMENT_PATTERN_2ARRAY[FQRVersion];
        for I := Low(Arr2Ptr^) to High(Arr2Ptr^) do
          for J := Low(Arr2Ptr^) to High(Arr2Ptr^) do
            if ((I <> Low(Arr2Ptr^)) or (J <> Low(Arr2Ptr^))) and
              ((I <> Low(Arr2Ptr^)) or (J <> High(Arr2Ptr^))) and
              ((I <> High(Arr2Ptr^)) or (J <> Low(Arr2Ptr^))) then
              PaintAlignment(Arr2Ptr^[I] - 2, Arr2Ptr[J] - 2);
      end;
    7..13:
      begin
        Arr3Ptr := CN_ALIGNMENT_PATTERN_3ARRAY[FQRVersion];
        for I := Low(Arr3Ptr^) to High(Arr3Ptr^) do
          for J := Low(Arr3Ptr^) to High(Arr3Ptr^) do
            if ((I <> Low(Arr3Ptr^)) or (J <> Low(Arr3Ptr^))) and
              ((I <> Low(Arr3Ptr^)) or (J <> High(Arr3Ptr^))) and
              ((I <> High(Arr3Ptr^)) or (J <> Low(Arr3Ptr^))) then
              PaintAlignment(Arr3Ptr^[I] - 2, Arr3Ptr[J] - 2);
      end;
    14..20:
      begin
        Arr4Ptr := CN_ALIGNMENT_PATTERN_4ARRAY[FQRVersion];
        for I := Low(Arr4Ptr^) to High(Arr4Ptr^) do
          for J := Low(Arr4Ptr^) to High(Arr4Ptr^) do
            if ((I <> Low(Arr4Ptr^)) or (J <> Low(Arr4Ptr^))) and
              ((I <> Low(Arr4Ptr^)) or (J <> High(Arr4Ptr^))) and
              ((I <> High(Arr4Ptr^)) or (J <> Low(Arr4Ptr^))) then
              PaintAlignment(Arr4Ptr^[I] - 2, Arr4Ptr[J] - 2);
      end;
    21..27:
      begin
        Arr5Ptr := CN_ALIGNMENT_PATTERN_5ARRAY[FQRVersion];
        for I := Low(Arr5Ptr^) to High(Arr5Ptr^) do
          for J := Low(Arr5Ptr^) to High(Arr5Ptr^) do
            if ((I <> Low(Arr5Ptr^)) or (J <> Low(Arr5Ptr^))) and
              ((I <> Low(Arr5Ptr^)) or (J <> High(Arr5Ptr^))) and
              ((I <> High(Arr5Ptr^)) or (J <> Low(Arr5Ptr^))) then
              PaintAlignment(Arr5Ptr^[I] - 2, Arr5Ptr[J] - 2);
      end;
    28..34:
      begin
        Arr6Ptr := CN_ALIGNMENT_PATTERN_6ARRAY[FQRVersion];
        for I := Low(Arr6Ptr^) to High(Arr6Ptr^) do
          for J := Low(Arr6Ptr^) to High(Arr6Ptr^) do
            if ((I <> Low(Arr6Ptr^)) or (J <> Low(Arr6Ptr^))) and
              ((I <> Low(Arr6Ptr^)) or (J <> High(Arr6Ptr^))) and
              ((I <> High(Arr6Ptr^)) or (J <> Low(Arr6Ptr^))) then
              PaintAlignment(Arr6Ptr^[I] - 2, Arr6Ptr[J] - 2);
      end;
    35..40:
      begin
        Arr7Ptr := CN_ALIGNMENT_PATTERN_7ARRAY[FQRVersion];
        for I := Low(Arr7Ptr^) to High(Arr7Ptr^) do
          for J := Low(Arr7Ptr^) to High(Arr7Ptr^) do
            if ((I <> Low(Arr7Ptr^)) or (J <> Low(Arr7Ptr^))) and
              ((I <> Low(Arr7Ptr^)) or (J <> High(Arr7Ptr^))) and
              ((I <> High(Arr7Ptr^)) or (J <> Low(Arr7Ptr^))) then
              PaintAlignment(Arr7Ptr^[I] - 2, Arr7Ptr[J] - 2);
      end;
  end;
end;

procedure TCnQREncoder.PaintData;
begin
  ClearData;
  EncodeText;
  PaintTimingPattern;
  PaintPositionDetectionPattern;
  PaintAlignmentPattern;
  PlaceDataBits;
  PaintMaskCoding;
end;

procedure TCnQREncoder.PaintFormatInformation;
begin
  // Bit 0 是 MSB (Bit 14)，Bit 14 是 LSB (Bit 0)
  // 标准中格式信息存放顺序：14..0
  // 位置：(8,0)..(8,5), (8,7), (8,8), (7,8)..(0,8)
  FQRData[8, 0] := Byte(FBitsFormatInfo.Bit[0]);
  FQRData[8, 1] := Byte(FBitsFormatInfo.Bit[1]);
  FQRData[8, 2] := Byte(FBitsFormatInfo.Bit[2]);
  FQRData[8, 3] := Byte(FBitsFormatInfo.Bit[3]);
  FQRData[8, 4] := Byte(FBitsFormatInfo.Bit[4]);
  FQRData[8, 5] := Byte(FBitsFormatInfo.Bit[5]);
  FQRData[8, 7] := Byte(FBitsFormatInfo.Bit[6]);
  FQRData[8, 8] := Byte(FBitsFormatInfo.Bit[7]);
  FQRData[7, 8] := Byte(FBitsFormatInfo.Bit[8]);
  FQRData[5, 8] := Byte(FBitsFormatInfo.Bit[9]);
  FQRData[4, 8] := Byte(FBitsFormatInfo.Bit[10]);
  FQRData[3, 8] := Byte(FBitsFormatInfo.Bit[11]);
  FQRData[2, 8] := Byte(FBitsFormatInfo.Bit[12]);
  FQRData[1, 8] := Byte(FBitsFormatInfo.Bit[13]);
  FQRData[0, 8] := Byte(FBitsFormatInfo.Bit[14]);

  FQRData[FQRSize - 1, 8] := Byte(FBitsFormatInfo.Bit[0]);
  FQRData[FQRSize - 2, 8] := Byte(FBitsFormatInfo.Bit[1]);
  FQRData[FQRSize - 3, 8] := Byte(FBitsFormatInfo.Bit[2]);
  FQRData[FQRSize - 4, 8] := Byte(FBitsFormatInfo.Bit[3]);
  FQRData[FQRSize - 5, 8] := Byte(FBitsFormatInfo.Bit[4]);
  FQRData[FQRSize - 6, 8] := Byte(FBitsFormatInfo.Bit[5]);
  FQRData[FQRSize - 7, 8] := Byte(FBitsFormatInfo.Bit[6]);
  FQRData[FQRSize - 8, 8] := Byte(FBitsFormatInfo.Bit[7]);
  FQRData[8, FQRSize - 8] := 1; // DarkModule
  FQRData[8, FQRSize - 7] := Byte(FBitsFormatInfo.Bit[8]);
  FQRData[8, FQRSize - 6] := Byte(FBitsFormatInfo.Bit[9]);
  FQRData[8, FQRSize - 5] := Byte(FBitsFormatInfo.Bit[10]);
  FQRData[8, FQRSize - 4] := Byte(FBitsFormatInfo.Bit[11]);
  FQRData[8, FQRSize - 3] := Byte(FBitsFormatInfo.Bit[12]);
  FQRData[8, FQRSize - 2] := Byte(FBitsFormatInfo.Bit[13]);
  FQRData[8, FQRSize - 1] := Byte(FBitsFormatInfo.Bit[14]);
end;

procedure TCnQREncoder.PaintPositionDetectionPattern;

  procedure PaintPositionDetection(Left, Top: Integer);
  begin
    PaintRect(Left, Top, Left + 6, Top + 6);
    PaintRect(Left + 2, Top + 2, Left + 4, Top + 4, True);
  end;

begin
  PaintPositionDetection(0, 0);
  PaintPositionDetection(0, FQRSize - 7);
  PaintPositionDetection(FQRSize - 7, 0);
end;

procedure TCnQREncoder.PaintRect(Left, Top, Right, Bottom: Integer; Solid: Boolean);
var
  I, J: Integer;
begin
  if Solid then
  begin
    for I := Left to Right do
    begin
      for J := Top to Bottom do
        FQRData[I, J] := 1;
    end;
  end
  else
  begin
    for I := Left to Right do
    begin
      FQRData[I, Top] := 1;
      FQRData[I, Bottom] := 1;
    end;
    for J := Top to Bottom do
    begin
      FQRData[Left, J] := 1;
      FQRData[Right, J] := 1;
    end;
  end;
end;

procedure TCnQREncoder.PaintTimingPattern;
var
  I: Integer;
begin
  for I := 0 to FQRSize - 1 do
  begin
    FQRData[6, I] := Byte((I and 1) = 0);
    FQRData[I, 6] := Byte((I and 1) = 0);
  end;
end;

procedure TCnQREncoder.PaintVersionInformation;
var
  I, A, B: Integer;
begin
  if FQRVersion < 7 then
    Exit;

  for I := 0 to 17 do
  begin
    A := FQRSize - 11 + (I mod 3);
    B := I div 3;
    FQRData[A, B] := Byte(FBitsVersionInfo.Bit[I]);
    FQRData[B, A] := Byte(FBitsVersionInfo.Bit[I]);
  end;
end;

procedure TCnQREncoder.SetQRErrorRecoveryLevel(const Value: TCnErrorRecoveryLevel);
begin
  if FQRErrorRecoveryLevel <> Value then
  begin
    FQRErrorRecoveryLevel := Value;
    FQRVersion := GetOptimalVersion(FRawText, FQRErrorRecoveryLevel);
    FQRSize := GetQRSizeFromVersion(FQRVersion);
    SetLength(FQRData, FQRSize, FQRSize);
    UpdateFormatInfoBits;
    UpdateVersionInfoBits;
    PaintData;
  end;
end;

procedure TCnQREncoder.SetQRVersion(const Value: TCnQRCodeVersion);
begin
  if Value <> FQRVersion then
  begin
    FQRVersion := Value;
    FQRSize := GetQRSizeFromVersion(FQRVersion);
    SetLength(FQRData, FQRSize, FQRSize);
    UpdateFormatInfoBits;
    UpdateVersionInfoBits;
    PaintData;
  end;
end;

procedure TCnQREncoder.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    UpdateRawText;
    FQRVersion := GetOptimalVersion(FRawText, FQRErrorRecoveryLevel);
    FQRSize := GetQRSizeFromVersion(FQRVersion);
    SetLength(FQRData, FQRSize, FQRSize);
    UpdateFormatInfoBits;
    UpdateVersionInfoBits;
    PaintData;
  end;
end;

procedure TCnQREncoder.SetQRWideCharMode(const Value: TCnQRWideCharMode);
begin
  if FQRWideCharMode <> Value then
  begin
    FQRWideCharMode := Value;
    UpdateRawText;
    FQRVersion := GetOptimalVersion(FRawText, FQRErrorRecoveryLevel);
    FQRSize := GetQRSizeFromVersion(FQRVersion);
    SetLength(FQRData, FQRSize, FQRSize);
    UpdateFormatInfoBits;
    UpdateVersionInfoBits;
    PaintData;
  end;
end;

// 数据编码方法
function TCnQREncoder.AnalyzeRawText: TCnQREncodeMode;
var
  I: Integer;
  HasAlphaNumeric, HasByte: Boolean;
begin
  HasAlphaNumeric := False;
  HasByte := False;

  for I := 1 to Length(FRawText) do
  begin
    if FRawText[I] in CN_QRCODE_CHARSET_NUMERIC then
      Continue
    else if FRawText[I] in CN_QRCODE_CHARSET_ALPHANUMERIC then
      HasAlphaNumeric := True
    else
    begin
      HasByte := True;
      Break;
    end;
  end;

  if HasByte then
    Result := emByte
  else if HasAlphaNumeric then
    Result := emAlphaNumeric
  else
    Result := emNumeric;
end;

function TCnQREncoder.GetCharCountBits(Mode: TCnQREncodeMode; Version:
  TCnQRCodeVersion): Integer;
begin
  case Mode of
    emNumeric:
      Result := CN_CHAR_COUNT_BITS[Version, 0];
    emAlphaNumeric:
      Result := CN_CHAR_COUNT_BITS[Version, 1];
    emByte:
      Result := CN_CHAR_COUNT_BITS[Version, 2];
  else
    Result := 8;
  end;
end;

function TCnQREncoder.EncodeNumeric(const AText: AnsiString): TCnBitBuilder;
var
  I, Groups, Remainder, Value, Pos: Integer;
  ModeBits: Integer;
begin
  Result := TCnBitBuilder.Create;

  // 模式指示符 (4 bits)
  Result.BitLength := 4;
  Result.Bit[0] := False; // 0001 for numeric
  Result.Bit[1] := False;
  Result.Bit[2] := False;
  Result.Bit[3] := True;

  // 字符计数位
  ModeBits := GetCharCountBits(emNumeric, FQRVersion);
  Result.BitLength := Result.BitLength + ModeBits;
  for I := 0 to ModeBits - 1 do
    Result.Bit[4 + I] := (Length(AText) shr (ModeBits - 1 - I)) and 1 = 1;

  Pos := 4 + ModeBits;

  // 每 3 位一组编码为 10 位
  Groups := Length(AText) div 3;
  Remainder := Length(AText) mod 3;

  for I := 1 to Groups do
  begin
    Value := (Ord(AText[(I - 1) * 3 + 1]) - Ord('0')) * 100 +
      (Ord(AText[(I - 1) * 3 + 2]) - Ord('0')) * 10 +
      (Ord(AText[(I - 1) * 3 + 3]) - Ord('0'));

    Result.BitLength := Pos + 10;
    Result.Bit[Pos] := (Value and $200) <> 0;
    Result.Bit[Pos + 1] := (Value and $100) <> 0;
    Result.Bit[Pos + 2] := (Value and $080) <> 0;
    Result.Bit[Pos + 3] := (Value and $040) <> 0;
    Result.Bit[Pos + 4] := (Value and $020) <> 0;
    Result.Bit[Pos + 5] := (Value and $010) <> 0;
    Result.Bit[Pos + 6] := (Value and $008) <> 0;
    Result.Bit[Pos + 7] := (Value and $004) <> 0;
    Result.Bit[Pos + 8] := (Value and $002) <> 0;
    Result.Bit[Pos + 9] := (Value and $001) <> 0;
    Inc(Pos, 10);
  end;

  // 处理剩余字符
  if Remainder = 1 then
  begin
    Value := Ord(AText[Length(AText)]) - Ord('0');
    Result.BitLength := Pos + 4;
    Result.Bit[Pos] := (Value and $8) <> 0;
    Result.Bit[Pos + 1] := (Value and $4) <> 0;
    Result.Bit[Pos + 2] := (Value and $2) <> 0;
    Result.Bit[Pos + 3] := (Value and $1) <> 0;
  end
  else if Remainder = 2 then
  begin
    Value := (Ord(AText[Length(AText) - 1]) - Ord('0')) * 10 +
      (Ord(AText[Length(AText)]) - Ord('0'));
    Result.BitLength := Pos + 7;
    Result.Bit[Pos] := (Value and $40) <> 0;
    Result.Bit[Pos + 1] := (Value and $20) <> 0;
    Result.Bit[Pos + 2] := (Value and $10) <> 0;
    Result.Bit[Pos + 3] := (Value and $08) <> 0;
    Result.Bit[Pos + 4] := (Value and $04) <> 0;
    Result.Bit[Pos + 5] := (Value and $02) <> 0;
    Result.Bit[Pos + 6] := (Value and $01) <> 0;
  end;
end;

function TCnQREncoder.EncodeAlphaNumeric(const AText: AnsiString): TCnBitBuilder;
var
  I, Groups, Remainder, Value, Value1, Value2, Pos: Integer;
  ModeBits: Integer;
  CharIndex: Integer;
begin
  Result := TCnBitBuilder.Create;

  // 模式指示符 (4 bits)
  Result.BitLength := 4;
  Result.Bit[0] := False; // 0010 for alphanumeric
  Result.Bit[1] := False;
  Result.Bit[2] := True;
  Result.Bit[3] := False;

  // 字符计数位
  ModeBits := GetCharCountBits(emAlphaNumeric, FQRVersion);
  Result.BitLength := Result.BitLength + ModeBits;
  for I := 0 to ModeBits - 1 do
    Result.Bit[4 + I] := (Length(AText) shr (ModeBits - 1 - I)) and 1 = 1;

  Pos := 4 + ModeBits;

  // 每两个字符一组编码为 11 位
  Groups := Length(AText) div 2;
  Remainder := Length(AText) mod 2;

  for I := 1 to Groups do
  begin
    // 获取字符索引值
    case AText[(I - 1) * 2 + 1] of
      '0'..'9':
        CharIndex := Ord(AText[(I - 1) * 2 + 1]) - Ord('0');
      'A'..'Z':
        CharIndex := Ord(AText[(I - 1) * 2 + 1]) - Ord('A') + 10;
      ' ':
        CharIndex := 36;
      '$':
        CharIndex := 37;
      '%':
        CharIndex := 38;
      '*':
        CharIndex := 39;
      '+':
        CharIndex := 40;
      '-':
        CharIndex := 41;
      '.':
        CharIndex := 42;
      '/':
        CharIndex := 43;
      ':':
        CharIndex := 44;
    else
      CharIndex := 0;
    end;
    Value1 := CharIndex;

    case AText[(I - 1) * 2 + 2] of
      '0'..'9':
        CharIndex := Ord(AText[(I - 1) * 2 + 2]) - Ord('0');
      'A'..'Z':
        CharIndex := Ord(AText[(I - 1) * 2 + 2]) - Ord('A') + 10;
      ' ':
        CharIndex := 36;
      '$':
        CharIndex := 37;
      '%':
        CharIndex := 38;
      '*':
        CharIndex := 39;
      '+':
        CharIndex := 40;
      '-':
        CharIndex := 41;
      '.':
        CharIndex := 42;
      '/':
        CharIndex := 43;
      ':':
        CharIndex := 44;
    else
      CharIndex := 0;
    end;
    Value2 := CharIndex;

    Value := Value1 * 45 + Value2;

    Result.BitLength := Pos + 11;
    Result.Bit[Pos] := (Value and $400) <> 0;
    Result.Bit[Pos + 1] := (Value and $200) <> 0;
    Result.Bit[Pos + 2] := (Value and $100) <> 0;
    Result.Bit[Pos + 3] := (Value and $080) <> 0;
    Result.Bit[Pos + 4] := (Value and $040) <> 0;
    Result.Bit[Pos + 5] := (Value and $020) <> 0;
    Result.Bit[Pos + 6] := (Value and $010) <> 0;
    Result.Bit[Pos + 7] := (Value and $008) <> 0;
    Result.Bit[Pos + 8] := (Value and $004) <> 0;
    Result.Bit[Pos + 9] := (Value and $002) <> 0;
    Result.Bit[Pos + 10] := (Value and $001) <> 0;
    Inc(Pos, 11);
  end;

  // 处理剩余字符
  if Remainder = 1 then
  begin
    case AText[Length(AText)] of
      '0'..'9':
        CharIndex := Ord(AText[Length(AText)]) - Ord('0');
      'A'..'Z':
        CharIndex := Ord(AText[Length(AText)]) - Ord('A') + 10;
      ' ':
        CharIndex := 36;
      '$':
        CharIndex := 37;
      '%':
        CharIndex := 38;
      '*':
        CharIndex := 39;
      '+':
        CharIndex := 40;
      '-':
        CharIndex := 41;
      '.':
        CharIndex := 42;
      '/':
        CharIndex := 43;
      ':':
        CharIndex := 44;
    else
      CharIndex := 0;
    end;

    Result.BitLength := Pos + 6;
    Result.Bit[Pos] := (CharIndex and $20) <> 0;
    Result.Bit[Pos + 1] := (CharIndex and $10) <> 0;
    Result.Bit[Pos + 2] := (CharIndex and $08) <> 0;
    Result.Bit[Pos + 3] := (CharIndex and $04) <> 0;
    Result.Bit[Pos + 4] := (CharIndex and $02) <> 0;
    Result.Bit[Pos + 5] := (CharIndex and $01) <> 0;
  end;
end;

function TCnQREncoder.EncodeByte(const AText: AnsiString): TCnBitBuilder;
var
  I, Pos, ModeBits: Integer;
begin
  Result := TCnBitBuilder.Create;

  // 模式指示符 (4 bits)
  Result.BitLength := 4;
  Result.Bit[0] := False; // 0100 for byte
  Result.Bit[1] := True;
  Result.Bit[2] := False;
  Result.Bit[3] := False;

  // 字符计数位
  ModeBits := GetCharCountBits(emByte, FQRVersion);
  Result.BitLength := Result.BitLength + ModeBits;
  for I := 0 to ModeBits - 1 do
    Result.Bit[4 + I] := (Length(AText) shr (ModeBits - 1 - I)) and 1 = 1;

  Pos := 4 + ModeBits;

  // 每个字节 8 位
  for I := 1 to Length(AText) do
  begin
    Result.BitLength := Pos + 8;
    Result.Bit[Pos] := (Ord(AText[I]) and $80) <> 0;
    Result.Bit[Pos + 1] := (Ord(AText[I]) and $40) <> 0;
    Result.Bit[Pos + 2] := (Ord(AText[I]) and $20) <> 0;
    Result.Bit[Pos + 3] := (Ord(AText[I]) and $10) <> 0;
    Result.Bit[Pos + 4] := (Ord(AText[I]) and $08) <> 0;
    Result.Bit[Pos + 5] := (Ord(AText[I]) and $04) <> 0;
    Result.Bit[Pos + 6] := (Ord(AText[I]) and $02) <> 0;
    Result.Bit[Pos + 7] := (Ord(AText[I]) and $01) <> 0;
    Inc(Pos, 8);
  end;
end;

procedure TCnQREncoder.EncodeText;
var
  Mode: TCnQREncodeMode;
  DataBits: TCnBitBuilder;
  TotalBits, RemainderBits, I, DataCount: Integer;
  NewVersion: TCnQRCodeVersion;
begin
  Mode := AnalyzeRawText;

  case Mode of
    emNumeric:
      DataBits := EncodeNumeric(FRawText);
    emAlphaNumeric:
      DataBits := EncodeAlphaNumeric(FRawText);
    emByte:
      DataBits := EncodeByte(FRawText);
  else
    DataBits := EncodeByte(FRawText);
  end;

  FDataBits.Assign(DataBits);
  DataBits.Free;

  // 计算总可用位数（数据码字 * 8）
  DataCount := GetTotalCodewords(FQRVersion) - GetECCodewords(FQRVersion,
    FQRErrorRecoveryLevel);
  TotalBits := DataCount * 8;

  // 如果数据太长，需要增加版本
  if FDataBits.BitLength > TotalBits then
  begin
    NewVersion := GetOptimalVersion(FRawText, FQRErrorRecoveryLevel);

    if NewVersion > 40 then
      raise ECnQRCodeException.Create(SCnErrorQRCodeDataTooLong);

    FQRVersion := NewVersion;
    FQRSize := GetQRSizeFromVersion(FQRVersion);
    SetLength(FQRData, FQRSize, FQRSize);
    EncodeText; // 重新编码
    Exit;
  end;

  // 添加终止符（最多 4 个 0）
  RemainderBits := TotalBits - FDataBits.BitLength;
  if RemainderBits > 4 then
    RemainderBits := 4;
  for I := 1 to RemainderBits do
    FDataBits.AppendBit(False);

  // 调整到字节边界（8 的倍数）
  RemainderBits := FDataBits.BitLength mod 8;
  if RemainderBits <> 0 then
  begin
    for I := 1 to (8 - RemainderBits) do
      FDataBits.AppendBit(False);
  end;

  // 填充剩余字节（使用 236 和 17 交替）
  while FDataBits.BitLength < TotalBits do
  begin
    FDataBits.AppendByteMSBFirst($EC);
    if FDataBits.BitLength < TotalBits then
      FDataBits.AppendByteMSBFirst($11);
  end;

  // 确保位长度正确
  if FDataBits.BitLength > TotalBits then
    FDataBits.BitLength := TotalBits;

  // 生成最终数据（包括纠错码）
  FFinalBits.Free;
  FFinalBits := AddEccAndInterleave(FDataBits);
end;

function TCnQREncoder.GetGeneratorPoly(Num: Integer): TCnQRGFPoly;
var
  I: Integer;
  SecondPoly, TempPoly: TCnQRGFPoly;
begin
  Result.Num := 0;
  SetLength(Result.Coeff, 1);
  Result.Coeff[0] := 1;

  for I := 0 to Num - 1 do
  begin
    // 乘法多项式 (x - 2^i)
    SetLength(SecondPoly.Coeff, 2);
    SecondPoly.Num := 1;
    SecondPoly.Coeff[0] := 1;
    SecondPoly.Coeff[1] := CN_EXP_TABLE[I];

    // 多项式乘法
    TempPoly := PolyMult(Result, SecondPoly);
    Result := TempPoly;
  end;
end;

function TCnQREncoder.PolyMult(A, B: TCnQRGFPoly): TCnQRGFPoly;
var
  I, J, ValA, ValB: Integer;
begin
  Result.Num := A.Num + B.Num;
  SetLength(Result.Coeff, Result.Num + 1);
  for I := 0 to Result.Num do
    Result.Coeff[I] := 0;

  for I := 0 to A.Num do
  begin
    if A.Coeff[I] = 0 then
      Continue;
    ValA := CN_LOG_TABLE[A.Coeff[I]];

    for J := 0 to B.Num do
    begin
      if B.Coeff[J] = 0 then
        Continue;
      ValB := CN_LOG_TABLE[B.Coeff[J]];

      Result.Coeff[I + J] := Result.Coeff[I + J] xor
        CN_EXP_TABLE[(ValA + ValB) mod $FF];
    end;
  end;
end;

function TCnQREncoder.PolyMod(Dividend, Divisor: TCnQRGFPoly): TCnQRGFPoly;
var
  I, J, LeadTerm, Term: Integer;
begin
  Result := Dividend;

  I := 0;
  while (I < Length(Result.Coeff)) and (Result.Coeff[I] = 0) do
    Inc(I);
  Result.Num := Length(Result.Coeff) - 1 - I;
  if Result.Num < 0 then
    Result.Num := 0;

  while (Result.Num >= Divisor.Num) do
  begin
    I := 0;
    while (I < Length(Result.Coeff)) and (Result.Coeff[I] = 0) do
      Inc(I);
    if I >= Length(Result.Coeff) then
      Break;

    Result.Num := Length(Result.Coeff) - 1 - I;
    if Result.Num < Divisor.Num then
      Break;

    LeadTerm := CN_LOG_TABLE[Result.Coeff[I]];

    for J := 0 to Divisor.Num do
    begin
      if Divisor.Coeff[J] <> 0 then
      begin
        Term := CN_LOG_TABLE[Divisor.Coeff[J]] + LeadTerm;
        Result.Coeff[I + J] := Result.Coeff[I + J] xor CN_EXP_TABLE[Term mod 255];
      end;
    end;
  end;
end;

// 数据布局
procedure TCnQREncoder.PlaceDataBits;
var
  BitIndex, Right, Vert, J, X, Y: Integer;
  Upward: Boolean;
begin
  BitIndex := 0;
  Right := FQRSize - 1;
  while Right >= 1 do
  begin
    if Right = 6 then
      Right := 5;
    Upward := ((Right + 1) and 2) = 0;

    for Vert := 0 to FQRSize - 1 do
    begin
      for J := 0 to 1 do
      begin
        X := Right - J;
        if Upward then
          Y := FQRSize - 1 - Vert
        else
          Y := Vert;

        if not IsFunctionArea(X, Y) then
        begin
          if BitIndex < FFinalBits.BitLength then
          begin
            FQRData[X, Y] := Byte(Ord(FFinalBits.Bit[BitIndex]));
            Inc(BitIndex);
          end;
        end;
      end;
    end;
    Dec(Right, 2);
  end;
end;

function TCnQREncoder.IsFunctionArea(X, Y: Integer): Boolean;
var
  I, J: Integer;
  AlignCoords: array of Integer;
  AlignCount: Integer;
  Arr2Ptr: PCn2BytesArray;
  Arr3Ptr: PCn3BytesArray;
  Arr4Ptr: PCn4BytesArray;
  Arr5Ptr: PCn5BytesArray;
  Arr6Ptr: PCn6BytesArray;
  Arr7Ptr: PCn7BytesArray;
begin
  Result := False;

  // 边界检查
  if (X < 0) or (X >= FQRSize) or (Y < 0) or (Y >= FQRSize) then
  begin
    Result := True; // 越界视为功能区域
    Exit;
  end;

  // 位置探测图形区域（三个角）
  // 左上角 (0..7, 0..7)
  if (X <= 7) and (Y <= 7) then
  begin
    Result := True;
    Exit;
  end;

  // 右上角 (Size-8..Size-1, 0..7)
  if (X >= FQRSize - 8) and (Y <= 7) then
  begin
    Result := True;
    Exit;
  end;

  // 左下角 (0..7, Size-8..Size-1)
  if (X <= 7) and (Y >= FQRSize - 8) then
  begin
    Result := True;
    Exit;
  end;

  // 时序图案 (第6行和第6列)
  if (X = 6) or (Y = 6) then
  begin
    Result := True;
    Exit;
  end;

  // 格式信息区域
  // 左上角的格式信息（第8行，0-8列，跳过第6列）
  if (Y = 8) and (X <= 8) and (X <> 6) then
  begin
    Result := True;
    Exit;
  end;

  // 左上角的格式信息（第8列，0-8行，跳过第6行）
  if (X = 8) and (Y <= 8) and (Y <> 6) then
  begin
    Result := True;
    Exit;
  end;

  // 右上角的格式信息（第8行，右侧区域）
  if (Y = 8) and (X >= FQRSize - 8) then
  begin
    Result := True;
    Exit;
  end;

  // 左下角的格式信息（第8列，底部区域）
  if (X = 8) and (Y >= FQRSize - 8) then
  begin
    Result := True;
    Exit;
  end;

  // 版本信息区域（版本7以上）
  if FQRVersion >= 7 then
  begin
    // 右上角版本信息区域
    if (X >= FQRSize - 11) and (X <= FQRSize - 9) and (Y >= 0) and (Y <= 5) then
    begin
      Result := True;
      Exit;
    end;

    // 左下角版本信息区域
    if (Y >= FQRSize - 11) and (Y <= FQRSize - 9) and (X >= 0) and (X <= 5) then
    begin
      Result := True;
      Exit;
    end;
  end;

  // 对齐图案检查
  // 获取对齐图案坐标
  case FQRVersion of
    2..6:
      begin
        Arr2Ptr := CN_ALIGNMENT_PATTERN_2ARRAY[FQRVersion];
        SetLength(AlignCoords, 2);
        AlignCoords[0] := Arr2Ptr^[0];
        AlignCoords[1] := Arr2Ptr^[1];
        AlignCount := 2;
      end;
    7..13:
      begin
        Arr3Ptr := CN_ALIGNMENT_PATTERN_3ARRAY[FQRVersion];
        SetLength(AlignCoords, 3);
        AlignCoords[0] := Arr3Ptr^[0];
        AlignCoords[1] := Arr3Ptr^[1];
        AlignCoords[2] := Arr3Ptr^[2];
        AlignCount := 3;
      end;
    14..20:
      begin
        Arr4Ptr := CN_ALIGNMENT_PATTERN_4ARRAY[FQRVersion];
        SetLength(AlignCoords, 4);
        AlignCoords[0] := Arr4Ptr^[0];
        AlignCoords[1] := Arr4Ptr^[1];
        AlignCoords[2] := Arr4Ptr^[2];
        AlignCoords[3] := Arr4Ptr^[3];
        AlignCount := 4;
      end;
    21..27:
      begin
        Arr5Ptr := CN_ALIGNMENT_PATTERN_5ARRAY[FQRVersion];
        SetLength(AlignCoords, 5);
        AlignCoords[0] := Arr5Ptr^[0];
        AlignCoords[1] := Arr5Ptr^[1];
        AlignCoords[2] := Arr5Ptr^[2];
        AlignCoords[3] := Arr5Ptr^[3];
        AlignCoords[4] := Arr5Ptr^[4];
        AlignCount := 5;
      end;
    28..34:
      begin
        Arr6Ptr := CN_ALIGNMENT_PATTERN_6ARRAY[FQRVersion];
        SetLength(AlignCoords, 6);
        AlignCoords[0] := Arr6Ptr^[0];
        AlignCoords[1] := Arr6Ptr^[1];
        AlignCoords[2] := Arr6Ptr^[2];
        AlignCoords[3] := Arr6Ptr^[3];
        AlignCoords[4] := Arr6Ptr^[4];
        AlignCoords[5] := Arr6Ptr^[5];
        AlignCount := 6;
      end;
    35..40:
      begin
        Arr7Ptr := CN_ALIGNMENT_PATTERN_7ARRAY[FQRVersion];
        SetLength(AlignCoords, 7);
        AlignCoords[0] := Arr7Ptr^[0];
        AlignCoords[1] := Arr7Ptr^[1];
        AlignCoords[2] := Arr7Ptr^[2];
        AlignCoords[3] := Arr7Ptr^[3];
        AlignCoords[4] := Arr7Ptr^[4];
        AlignCoords[5] := Arr7Ptr^[5];
        AlignCoords[6] := Arr7Ptr^[6];
        AlignCount := 7;
      end;
  else
    // 版本1没有对齐图案
    AlignCount := 0;
  end;

  // 检查是否在对齐图案区域内
  for I := 0 to AlignCount - 1 do
  begin
    for J := 0 to AlignCount - 1 do
    begin
      if ((AlignCoords[I] = 6) and (AlignCoords[J] = 6)) or
        ((AlignCoords[I] = 6) and (AlignCoords[J] = FQRSize - 7)) or
        ((AlignCoords[I] = FQRSize - 7) and (AlignCoords[J] = 6)) then
        Continue;

      // 检查是否在对齐图案的5x5区域内
      if (X >= AlignCoords[I] - 2) and (X <= AlignCoords[I] + 2) and
        (Y >= AlignCoords[J] - 2) and (Y <= AlignCoords[J] + 2) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;


// 掩码处理
function TCnQREncoder.GetMaskPattern(X, Y, MaskType: Integer): Boolean;
begin
  case MaskType of
    0:
      Result := (X + Y) mod 2 = 0;
    1:
      Result := Y mod 2 = 0;
    2:
      Result := X mod 3 = 0;
    3:
      Result := (X + Y) mod 3 = 0;
    4:
      Result := ((Y div 2) + (X div 3)) mod 2 = 0;
    5:
      Result := ((X * Y) mod 2) + ((X * Y) mod 3) = 0;
    6:
      Result := (((X * Y) mod 2) + ((X * Y) mod 3)) mod 2 = 0;
    7:
      Result := (((X + Y) mod 2) + ((X * Y) mod 3)) mod 2 = 0;
  else
    Result := False;
  end;
end;

procedure TCnQREncoder.ApplyMask(MaskType: Integer);
var
  X, Y: Integer;
begin
  for X := 0 to FQRSize - 1 do
  begin
    for Y := 0 to FQRSize - 1 do
    begin
      if not IsFunctionArea(X, Y) then
      begin
        if GetMaskPattern(X, Y, MaskType) then
          FQRData[X, Y] := 1 - FQRData[X, Y];
      end;
    end;
  end;
end;

function TCnQREncoder.EvaluateMask(MaskType: Integer): Integer;
var
  TempData: TCnQRData;
  X, Y: Integer;
  Penalty1, Penalty2, Penalty3, Penalty4: Integer;
  RunLength, LastModule: Integer;
  DarkCount: Integer;
  LeftSafe, RightSafe: Boolean;
begin
  // 复制当前矩阵并应用指定掩码到临时矩阵
  SetLength(TempData, FQRSize, FQRSize);
  for X := 0 to FQRSize - 1 do
    for Y := 0 to FQRSize - 1 do
      TempData[X, Y] := FQRData[X, Y];

  for X := 0 to FQRSize - 1 do
    for Y := 0 to FQRSize - 1 do
      if not IsFunctionArea(X, Y) and GetMaskPattern(X, Y, MaskType) then
        TempData[X, Y] := 1 - TempData[X, Y];

  Penalty1 := 0;
  Penalty2 := 0;
  Penalty3 := 0;

  // 行相邻同色
  for Y := 0 to FQRSize - 1 do
  begin
    RunLength := 1;
    LastModule := TempData[0, Y];
    for X := 1 to FQRSize - 1 do
    begin
      if TempData[X, Y] = LastModule then
        Inc(RunLength)
      else
      begin
        if RunLength >= 5 then
          Inc(Penalty1, RunLength - 2);
        RunLength := 1;
        LastModule := TempData[X, Y];
      end;
    end;
    if RunLength >= 5 then
      Inc(Penalty1, RunLength - 2);
  end;

  // 检查列
  for X := 0 to FQRSize - 1 do
  begin
    RunLength := 1;
    LastModule := TempData[X, 0];
    for Y := 1 to FQRSize - 1 do
    begin
      if TempData[X, Y] = LastModule then
        Inc(RunLength)
      else
      begin
        if RunLength >= 5 then
          Inc(Penalty1, RunLength - 2);
        RunLength := 1;
        LastModule := TempData[X, Y];
      end;
    end;
    if RunLength >= 5 then
      Inc(Penalty1, RunLength - 2);
  end;

  // 2x2 同色块
  for Y := 0 to FQRSize - 2 do
  begin
    for X := 0 to FQRSize - 2 do
    begin
      if (TempData[X, Y] = TempData[X + 1, Y]) and
        (TempData[X, Y] = TempData[X, Y + 1]) and
        (TempData[X, Y] = TempData[X + 1, Y + 1]) then
        Inc(Penalty2, 3);
    end;
  end;

  // 查找类图案（行）
  for Y := 0 to FQRSize - 1 do
  begin
    for X := 0 to FQRSize - 7 do
    begin
      if (TempData[X, Y] = 1) and (TempData[X + 1, Y] = 0) and (TempData[X + 2,
        Y] = 1) and
        (TempData[X + 3, Y] = 1) and (TempData[X + 4, Y] = 1) and (TempData[X + 5, Y] = 0) and
        (TempData[X + 6, Y] = 1) then
      begin
        LeftSafe := (X < 4) or ((TempData[X - 1, Y] = 0) and (TempData[X - 2, Y] = 0) and
          (TempData[X - 3, Y] = 0) and (TempData[X - 4, Y] = 0));
        RightSafe := (X + 10 >= FQRSize) or ((TempData[X + 7, Y] = 0) and (TempData
          [X + 8, Y] = 0) and
          (TempData[X + 9, Y] = 0) and (TempData[X + 10, Y] = 0));
        if LeftSafe or RightSafe then
          Inc(Penalty3, 40);
      end;
    end;
  end;

  // 查找类图案（列）
  for X := 0 to FQRSize - 1 do
  begin
    for Y := 0 to FQRSize - 7 do
    begin
      if (TempData[X, Y] = 1) and (TempData[X, Y + 1] = 0) and (TempData[X, Y +
        2] = 1) and
        (TempData[X, Y + 3] = 1) and (TempData[X, Y + 4] = 1) and (TempData[X, Y + 5] = 0) and
        (TempData[X, Y + 6] = 1) then
      begin
        LeftSafe := (Y < 4) or ((TempData[X, Y - 1] = 0) and (TempData[X, Y - 2] = 0) and
          (TempData[X, Y - 3] = 0) and (TempData[X, Y - 4] = 0));
        RightSafe := (Y + 10 >= FQRSize) or ((TempData[X, Y + 7] = 0) and (TempData
          [X, Y + 8] = 0) and
          (TempData[X, Y + 9] = 0) and (TempData[X, Y + 10] = 0));
        if LeftSafe or RightSafe then
          Inc(Penalty3, 40);
      end;
    end;
  end;

  // 黑模块比例
  DarkCount := 0;
  for X := 0 to FQRSize - 1 do
  begin
    for Y := 0 to FQRSize - 1 do
    begin
      if TempData[X, Y] = 1 then
        Inc(DarkCount);
    end;
  end;

  Penalty4 := Abs((DarkCount * 100 div (FQRSize * FQRSize)) - 50) div 5 * 10;
  Result := Penalty1 + Penalty2 + Penalty3 + Penalty4;
  SetLength(TempData, 0);
end;

procedure TCnQREncoder.PaintMaskCoding;
var
  BestMask, MaskType, BestPenalty, CurrentPenalty: Integer;
  X, Y: Integer;
  QRDataBackup: TCnQRData;
begin
  SetLength(QRDataBackup, FQRSize, FQRSize);

  BestMask := 0;
  BestPenalty := MaxInt;

  // 尝试所有 8 种掩码
  for MaskType := 0 to 7 do
  begin
    // 备份数据
    for X := 0 to FQRSize - 1 do
      for Y := 0 to FQRSize - 1 do
        QRDataBackup[X, Y] := FQRData[X, Y];

    // 应用掩码
    ApplyMask(MaskType);

    // 计算惩罚分
    CurrentPenalty := EvaluateMask(MaskType);

    if CurrentPenalty < BestPenalty then
    begin
      BestPenalty := CurrentPenalty;
      BestMask := MaskType;
    end;

    // 恢复数据
    for X := 0 to FQRSize - 1 do
      for Y := 0 to FQRSize - 1 do
        FQRData[X, Y] := QRDataBackup[X, Y];
  end;

  // 应用最佳掩码
  FMaskType := BestMask;
  ApplyMask(FMaskType);

  // 更新格式信息和版本信息
  UpdateFormatInfoBits;
  UpdateVersionInfoBits;

  PaintFormatInformation;
  PaintVersionInformation;

  SetLength(QRDataBackup, 0);
end;

// BCH 编码
function TCnQREncoder.BCHEncode15(Data: Integer): Integer;
var
  I: Integer;
  Generator, Remainder: Integer;
begin
  // 生成器多项式 x^10 + x^8 + x^5 + x^4 + x^2 + x + 1 = 10100110111 (binary) = 0x537
  Generator := $537;
  // 数据左移10位
  Remainder := Data shl 10;

  // 计算余数
  for I := 14 downto 10 do
  begin
    if (Remainder shr I) and 1 = 1 then
      Remainder := Remainder xor (Generator shl (I - 10));
  end;

  // 结果是 (Data << 10) | Remainder
  Result := (Data shl 10) or Remainder;
end;

function TCnQREncoder.BCHEncode18(Data: Integer): Integer;
var
  I: Integer;
  Generator, Remainder: Integer;
begin
  // 生成多项式 x^12 + x^11 + x^10 + x^9 + x^8 + x^5 + x^2 + 1 = 1111100100101 = 0x1F25
  Generator := $1F25;
  // 数据左移12位
  Remainder := Data shl 12;

  for I := 17 downto 12 do
  begin
    if (Remainder shr I) and 1 = 1 then
      Remainder := Remainder xor (Generator shl (I - 12));
  end;

  // 结果是 (Data << 12) | Remainder
  Result := (Data shl 12) or Remainder;
end;

function TCnQREncoder.GetFormatBits(ErrorLevel: TCnErrorRecoveryLevel; MaskType:
  Integer): Integer;
var
  FormatData: Integer;
begin
  // 格式: 2 位错误纠正等级 + 3 位掩码模式
  FormatData := (CN_ERROR_LEVEL_BITS[ErrorLevel] shl 3) or (MaskType and $07);
  // BCH 编码并异或 Mask Pattern (101010000010010 = 0x5412)
  Result := BCHEncode15(FormatData) xor $5412;
end;

function TCnQREncoder.GetVersionBits(Version: TCnQRCodeVersion): Integer;
begin
  // 版本 7 以下不需要版本信息
  if Version < 7 then
  begin
    Result := 0;
    Exit;
  end;

  Result := BCHEncode18(Version);
end;

function TCnQREncoder.GetTotalCodewords(Version: TCnQRCodeVersion): Integer;
begin
  Result := CN_TOTAL_CODEWORDS[Version];
end;

function TCnQREncoder.GetECCodewords(Version: TCnQRCodeVersion; ErrorLevel:
  TCnErrorRecoveryLevel): Integer;
begin
  Result := CN_EC_CODEWORDS[Version, Ord(ErrorLevel)];
end;

function TCnQREncoder.GetOptimalVersion(const AText: AnsiString; ErrorLevel:
  TCnErrorRecoveryLevel): TCnQRCodeVersion;
var
  Version: TCnQRCodeVersion;
  Mode: TCnQREncodeMode;
  RequiredBits, AvailableBits, ModeBits, DataBits: Integer;
begin
  Mode := AnalyzeRawText;
  Result := 40; // 默认最大版本

  for Version := 1 to 40 do
  begin
    // 字符计数位长度随版本变化
    ModeBits := 4 + GetCharCountBits(Mode, Version);

    case Mode of
      emNumeric:
        begin
          DataBits := (Length(AText) div 3) * 10;
          if Length(AText) mod 3 = 1 then
            DataBits := DataBits + 4
          else if Length(AText) mod 3 = 2 then
            DataBits := DataBits + 7;
        end;
      emAlphaNumeric:
        begin
          DataBits := (Length(AText) div 2) * 11;
          if Length(AText) mod 2 = 1 then
            DataBits := DataBits + 6;
        end;
      emByte:
        DataBits := Length(AText) * 8;
    else
      DataBits := Length(AText) * 8;
    end;

    RequiredBits := ModeBits + DataBits;
    AvailableBits := (GetTotalCodewords(Version) - GetECCodewords(Version,
      ErrorLevel)) * 8;

    if RequiredBits <= AvailableBits then
    begin
      Result := Version;
      Exit;
    end;
  end;
end;

procedure TCnQREncoder.UpdateFormatInfoBits;
var
  FormatBits, I: Integer;
begin
  FormatBits := GetFormatBits(FQRErrorRecoveryLevel, FMaskType);
  for I := 0 to 14 do
    FBitsFormatInfo.Bit[I] := (FormatBits shr I) and 1 = 1;
end;

procedure TCnQREncoder.UpdateVersionInfoBits;
var
  VersionBits, I: Integer;
begin
  if FQRVersion < 7 then
  begin
    for I := 0 to 17 do
      FBitsVersionInfo.Bit[I] := False;
    Exit;
  end;

  VersionBits := GetVersionBits(FQRVersion);
  for I := 0 to 17 do
    FBitsVersionInfo.Bit[I] := (VersionBits shr I) and 1 = 1;
end;

function TCnQREncoder.ComputeBlockECC(const DataBytes: TBytes; ECCodewords:
  Integer): TBytes;
var
  I: Integer;
  MessagePoly, GeneratorPoly, DataPoly: TCnQRGFPoly;
begin
  SetLength(MessagePoly.Coeff, Length(DataBytes) + ECCodewords);
  MessagePoly.Num := Length(DataBytes) + ECCodewords - 1;

  for I := 0 to High(DataBytes) do
    MessagePoly.Coeff[I] := DataBytes[I];
  for I := Length(DataBytes) to High(MessagePoly.Coeff) do
    MessagePoly.Coeff[I] := 0;

  GeneratorPoly := GetGeneratorPoly(ECCodewords);
  DataPoly := PolyMod(MessagePoly, GeneratorPoly);
  SetLength(Result, ECCodewords);
  for I := 0 to ECCodewords - 1 do
    Result[I] := DataPoly.Coeff[Length(DataPoly.Coeff) - ECCodewords + I];
end;

function TCnQREncoder.AddEccAndInterleave(Data: TCnBitBuilder): TCnBitBuilder;
var
  I, J, K: Integer;
  DataBytes: TBytes;
  RawCodewords, NumBlocks, BlockEccLen, NumShortBlocks, ShortBlockLen: Integer;
  Blocks: array of array of Byte;
  DatLen, BlockLen: Integer;
  DatSlice: TBytes;
  EccSlice: TBytes;
  ResultBytes: TBytes;
begin
  SetLength(DataBytes, Data.BitLength div 8);
  for I := 0 to High(DataBytes) do
  begin
    DataBytes[I] := 0;
    for J := 0 to 7 do
    begin
      if Data.Bit[I * 8 + J] then
        DataBytes[I] := DataBytes[I] or (1 shl (7 - J));
    end;
  end;

  RawCodewords := GetTotalCodewords(FQRVersion);
  NumBlocks := CN_NUM_ERROR_CORRECTION_BLOCKS[FQRVersion, Ord(FQRErrorRecoveryLevel)];
  BlockEccLen := CN_ECC_CODEWORDS_PER_BLOCK[FQRVersion, Ord(FQRErrorRecoveryLevel)];
  NumShortBlocks := NumBlocks - (RawCodewords mod NumBlocks);
  ShortBlockLen := RawCodewords div NumBlocks;
  SetLength(Blocks, NumBlocks);
  K := 0;

  for I := 0 to NumBlocks - 1 do
  begin
    DatLen := ShortBlockLen - BlockEccLen + (Ord(I >= NumShortBlocks));
    SetLength(DatSlice, DatLen);
    for J := 0 to DatLen - 1 do
      DatSlice[J] := DataBytes[K + J];
    Inc(K, DatLen);
    BlockLen := ShortBlockLen + 1;
    SetLength(Blocks[I], BlockLen);
    for J := 0 to DatLen - 1 do
      Blocks[I][J] := DatSlice[J];
    EccSlice := ComputeBlockECC(DatSlice, BlockEccLen);
    for J := 0 to BlockEccLen - 1 do
      Blocks[I][BlockLen - BlockEccLen + J] := EccSlice[J];
  end;

  SetLength(ResultBytes, RawCodewords);
  K := 0;
  for I := 0 to Length(Blocks[0]) - 1 do
  begin
    for J := 0 to NumBlocks - 1 do
    begin
      if not ((I = ShortBlockLen - BlockEccLen) and (J < NumShortBlocks)) then
      begin
        ResultBytes[K] := Blocks[J][I];
        Inc(K);
      end;
    end;
  end;

  Result := TCnBitBuilder.Create;
  Result.BitLength := RawCodewords * 8;
  for I := 0 to RawCodewords - 1 do
  begin
    for J := 0 to 7 do
      Result.Bit[I * 8 + J] := ((ResultBytes[I] shr (7 - J)) and 1) = 1;
  end;
end;

function TCnQREncoder.BitBuilderToBytes(B: TCnBitBuilder; ByteCount: Integer): TBytes;
var
  I, J: Integer;
begin
  SetLength(Result, ByteCount);
  for I := 0 to ByteCount - 1 do
  begin
    Result[I] := 0;
    for J := 0 to 7 do
    begin
      if B.Bit[I * 8 + J] then
        Result[I] := Result[I] or (1 shl (7 - J));
    end;
  end;
end;

function TCnQREncoder.GetDataCodewordsBytes: TBytes;
var
  DataCount: Integer;
begin
  DataCount := GetTotalCodewords(FQRVersion) - GetECCodewords(FQRVersion,
    FQRErrorRecoveryLevel);
  Result := BitBuilderToBytes(FDataBits, DataCount);
end;

function TCnQREncoder.GetAllCodewordsBytes: TBytes;
var
  RawCount: Integer;
begin
  RawCount := GetTotalCodewords(FQRVersion);
  Result := BitBuilderToBytes(FFinalBits, RawCount);
end;

function TCnQREncoder.GetFormatBitsValue: Integer;
begin
  Result := GetFormatBits(FQRErrorRecoveryLevel, FMaskType);
end;

function TCnQREncoder.GetVersionBitsValue: Integer;
begin
  Result := GetVersionBits(FQRVersion);
end;

function TCnQREncoder.DumpMatrix: string;
var
  X, Y: Integer;
  S: string;
begin
  SetLength(Result, 0);
  for Y := 0 to FQRSize - 1 do
  begin
    S := '';
    SetLength(S, FQRSize);
    for X := 1 to FQRSize do
    begin
      if FQRData[X - 1, Y] <> 0 then
        S[X] := '1'
      else
        S[X] := '0';
    end;
    Result := Result + S + #13#10;
  end;
end;

function TCnQREncoder.DumpMatrixUnmasked: string;
var
  X, Y: Integer;
  S: string;
  V: Byte;
begin
  SetLength(Result, 0);
  for Y := 0 to FQRSize - 1 do
  begin
    S := '';
    SetLength(S, FQRSize);
    for X := 0 to FQRSize - 1 do
    begin
      V := FQRData[X, Y];
      if (not IsFunctionArea(X, Y)) and GetMaskPattern(X, Y, FMaskType) then
        V := 1 - V;
      if V <> 0 then
        S[X + 1] := '1'
      else
        S[X + 1] := '0';
    end;
    Result := Result + S + #13#10;
  end;
end;

function TCnQREncoder.DumpFunctionArea: string;
var
  X, Y: Integer;
  S: string;
begin
  SetLength(Result, 0);
  for Y := 0 to FQRSize - 1 do
  begin
    S := '';
    SetLength(S, FQRSize);
    for X := 0 to FQRSize - 1 do
    begin
      if IsFunctionArea(X, Y) then
        S[X + 1] := 'F'
      else
        S[X + 1] := '.';
    end;
    Result := Result + S + #13#10;
  end;
end;

function TCnQREncoder.DumpFormatRowInfo: string;
var
  X: Integer;
  First, Count: Integer;
begin
  First := -1;
  Count := 0;
  for X := 0 to FQRSize - 1 do
  begin
    if IsFunctionArea(X, 8) then
    begin
      if First < 0 then
        First := X;
      Inc(Count);
    end;
  end;
  Result := Format(SCnErrorQRRowY8FirstCount,
    [First, Count, FQRSize, FQRSize - 8]);
end;

function TCnQREncoder.DumpFormatColInfo: string;
var
  Y: Integer;
  First, Count: Integer;
begin
  First := -1;
  Count := 0;
  for Y := 0 to FQRSize - 1 do
  begin
    if IsFunctionArea(8, Y) then
    begin
      if First < 0 then
        First := Y;
      Inc(Count);
    end;
  end;
  Result := Format(SCnErrorQRColX8FirstCount,
    [First, Count, FQRSize, FQRSize - 8]);
end;

function TCnQREncoder.DumpAlignmentCenters: string;
var
  ArrPtr: Pointer;
  I: Integer;
begin
  Result := '';
  case FQRVersion of
    2..6:
      begin
        ArrPtr := CN_ALIGNMENT_PATTERN_2ARRAY[FQRVersion];
        for I := 0 to 1 do
          Result := Result + Format(' %d', [PCn2BytesArray(ArrPtr)^[I]]);
      end;
    7..13:
      begin
        ArrPtr := CN_ALIGNMENT_PATTERN_3ARRAY[FQRVersion];
        for I := 0 to 2 do
          Result := Result + Format(' %d', [PCn3BytesArray(ArrPtr)^[I]]);
      end;
    14..20:
      begin
        ArrPtr := CN_ALIGNMENT_PATTERN_4ARRAY[FQRVersion];
        for I := 0 to 3 do
          Result := Result + Format(' %d', [PCn4BytesArray(ArrPtr)^[I]]);
      end;
    21..27:
      begin
        ArrPtr := CN_ALIGNMENT_PATTERN_5ARRAY[FQRVersion];
        for I := 0 to 4 do
          Result := Result + Format(' %d', [PCn5BytesArray(ArrPtr)^[I]]);
      end;
    28..34:
      begin
        ArrPtr := CN_ALIGNMENT_PATTERN_6ARRAY[FQRVersion];
        for I := 0 to 5 do
          Result := Result + Format(' %d', [PCn6BytesArray(ArrPtr)^[I]]);
      end;
    35..40:
      begin
        ArrPtr := CN_ALIGNMENT_PATTERN_7ARRAY[FQRVersion];
        for I := 0 to 6 do
          Result := Result + Format(' %d', [PCn7BytesArray(ArrPtr)^[I]]);
      end;
  end;
  Result := Trim(Result);
end;

function TCnQREncoder.DumpFormatRowIndices: string;
var
  X: Integer;
  First: Boolean;
begin
  Result := '';
  First := True;
  for X := 0 to FQRSize - 1 do
  begin
    if IsFunctionArea(X, 8) then
    begin
      if not First then
        Result := Result + ' ';
      Result := Result + IntToStr(X);
      First := False;
    end;
  end;
end;

function TCnQREncoder.DumpFormatColIndices: string;
var
  Y: Integer;
  First: Boolean;
begin
  Result := '';
  First := True;
  for Y := 0 to FQRSize - 1 do
  begin
    if IsFunctionArea(8, Y) then
    begin
      if not First then
        Result := Result + ' ';
      Result := Result + IntToStr(Y);
      First := False;
    end;
  end;
end;

procedure TCnQREncoder.UpdateRawText;
begin
  if FQRWideCharMode = cqwUtf8 then
  begin
{$IFDEF UNICODE}
    FRawText := CnUtf8EncodeWideString(FText)
{$ELSE}
    FRawText := CnAnsiToUtf8(FText);
{$ENDIF}
  end
  else
    FRawText := AnsiString(FText);
end;

{ ---- 二维码解码函数 ---- }

function CnQRHammingDistance(A, B, ABitCount: Integer): Integer;
var
  XorVal: Integer;
  I: Integer;
begin
  XorVal := A xor B;
  Result := 0;
  for I := 0 to ABitCount - 1 do
  begin
    if (XorVal and (1 shl I)) <> 0 then
      Inc(Result);
  end;
end;
{* 计算两个整数低 ABitCount 位的汉明距离（异或后 1 的个数）}

{ TCnQRDecoder }

// 构造函数，初始化内部变量
constructor TCnQRDecoder.Create;
begin
  inherited;
  FQRSize := 0;
  FQRVersion := 1;
  FIsMirrored := False;
  FErrorMessage := '';
end;

// GF(2^8) 乘法：LOG[A] + LOG[B] mod 255 后查 EXP 表，0 元素特殊处理
function TCnQRDecoder.GFMul(A, B: Integer): Integer;
begin
  if (A = 0) or (B = 0) then
    Result := 0
  else
    Result := CN_EXP_TABLE[(CN_LOG_TABLE[A] + CN_LOG_TABLE[B]) mod 255];
end;

// GF(2^8) 对数：查 CN_LOG_TABLE[N]
function TCnQRDecoder.GFLog(N: Integer): Integer;
begin
  Result := CN_LOG_TABLE[N];
end;

// GF(2^8) 乘法逆元：CN_EXP_TABLE[255 - CN_LOG_TABLE[N]]
function TCnQRDecoder.GFInv(N: Integer): Integer;
begin
  Result := CN_EXP_TABLE[255 - CN_LOG_TABLE[N]];
end;

// GF(2^8) 除法：LOG[A] - LOG[B] + 255 mod 255 后查 EXP 表
function TCnQRDecoder.GFDiv(A, B: Integer): Integer;
begin
  if (A = 0) or (B = 0) then
    Result := 0
  else
    Result := CN_EXP_TABLE[(CN_LOG_TABLE[A] - CN_LOG_TABLE[B] + 255) mod 255];
end;

// GF(2^8) 指数：查 CN_EXP_TABLE[N mod 255]
function TCnQRDecoder.GFExp(N: Integer): Integer;
begin
  Result := CN_EXP_TABLE[N mod 255];
end;

// 对两份掩码后的 15 位格式信息进行汉明距离匹配解码
function TCnQRDecoder.DecodeFormatInfo(MaskedInfo1, MaskedInfo2: Integer): Boolean;
var
  I, Dist1, Dist2, BestDist1, BestDist2, BestIdx1, BestIdx2: Integer;
  RawVal1: Integer;
begin
  BestDist1 := MaxInt;
  BestDist2 := MaxInt;
  BestIdx1 := 0;
  BestIdx2 := 0;
  for I := 0 to 31 do
  begin
    Dist1 := CnQRHammingDistance(MaskedInfo1, CN_FORMAT_INFO_DECODE_LOOKUP[I, 0], 15);
    Dist2 := CnQRHammingDistance(MaskedInfo2, CN_FORMAT_INFO_DECODE_LOOKUP[I, 0], 15);
    if Dist1 < BestDist1 then
    begin
      BestDist1 := Dist1;
      BestIdx1 := I;
    end;
    if Dist2 < BestDist2 then
    begin
      BestDist2 := Dist2;
      BestIdx2 := I;
    end;
  end;

  // 两份中选最小距离的
  if BestDist1 <= BestDist2 then
  begin
    if BestDist1 > 3 then
    begin
      Result := False;
      Exit;
    end;
    RawVal1 := CN_FORMAT_INFO_DECODE_LOOKUP[BestIdx1, 1];
  end
  else
  begin
    if BestDist2 > 3 then
    begin
      Result := False;
      Exit;
    end;
    RawVal1 := CN_FORMAT_INFO_DECODE_LOOKUP[BestIdx2, 1];
  end;

  // 解析纠错级别和掩码类型
  case (RawVal1 shr 3) and $03 of
    1: FFormatInfo.ErrorLevel := erlL;
    0: FFormatInfo.ErrorLevel := erlM;
    3: FFormatInfo.ErrorLevel := erlQ;
    2: FFormatInfo.ErrorLevel := erlH;
  else
    FFormatInfo.ErrorLevel := erlM;
  end;
  FFormatInfo.MaskType := RawVal1 and $07;
  Result := True;
end;

// 从矩阵中读取两份格式信息并解码
function TCnQRDecoder.ReadFormatInformation: Boolean;
var
  MaskedInfo1, MaskedInfo2: Integer;
  I: Integer;
begin
  // OutputDebugString(PChar(Format('FI: Size=%d IsMirrored=%d', [FQRSize, Ord(FIsMirrored)])));
  // 读取第一份格式信息（15 位），位顺序对照 zxing FormatInformation.java：
  // 水平方向：行 8，列 0..5 → bits 0-5
  // 列 7 行 8 → bit 6，列 8 行 8 → bit 7
  // 垂直方向：列 8，行 7 → bit 8
  // 列 8，行 5..0（逆序）→ bits 9-14
  MaskedInfo1 := 0;
  for I := 0 to 5 do
    MaskedInfo1 := (MaskedInfo1 shl 1) or FQRData[I, 8];          // bits 0-5: (0..5, 8)
  MaskedInfo1 := (MaskedInfo1 shl 1) or FQRData[7, 8];            // bit 6: (7, 8)
  MaskedInfo1 := (MaskedInfo1 shl 1) or FQRData[8, 8];            // bit 7: (8, 8)
  MaskedInfo1 := (MaskedInfo1 shl 1) or FQRData[8, 7];            // bit 8: (8, 7)
  for I := 5 downto 0 do
    MaskedInfo1 := (MaskedInfo1 shl 1) or FQRData[8, I];          // bits 9-14: (8, 5..0)

  // 读取第二份格式信息（15 位）：
  // 垂直方向：列 8，行 Size-1..Size-7 → bits 0-6
  // 水平方向：行 8，列 Size-8..Size-1 → bits 7-14
  MaskedInfo2 := 0;
  for I := 0 to 6 do
    MaskedInfo2 := (MaskedInfo2 shl 1) or FQRData[8, FQRSize - 1 - I];  // bits 0-6: (8, Size-1..Size-7)
  for I := 0 to 7 do
    MaskedInfo2 := (MaskedInfo2 shl 1) or FQRData[FQRSize - 8 + I, 8];  // bits 7-14: (Size-8..Size-1, 8)

  Result := DecodeFormatInfo(MaskedInfo1, MaskedInfo2);
  // OutputDebugString(PChar(Format('FI: M1=%d M2=%d Result=%d EL=%d Mask=%d',
  //  [MaskedInfo1, MaskedInfo2, Ord(Result), Ord(FFormatInfo.ErrorLevel), FFormatInfo.MaskType])));
end;

// 从指定位置提取 18 位版本信息位序列，按标准存储布局组合 3 列 x 6 行
function TCnQRDecoder.ExtractVersionBits(AStartCol, AStartRow: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 17 do
  begin
    Result := (Result shl 1) or
      FQRData[AStartCol + (I mod 3), AStartRow + (I div 3)];
  end;
end;

// 对两份 18 位版本信息进行汉明距离匹配解码
function TCnQRDecoder.DecodeVersion(VersionBits1, VersionBits2: Integer): Boolean;
var
  I, Dist1, Dist2, BestDist1, BestDist2, BestIdx1, BestIdx2: Integer;
begin
  BestDist1 := MaxInt;
  BestDist2 := MaxInt;
  BestIdx1 := 0;
  BestIdx2 := 0;
  for I := 0 to 33 do
  begin
    Dist1 := CnQRHammingDistance(VersionBits1, CN_VERSION_DECODE_INFO[I], 18);
    Dist2 := CnQRHammingDistance(VersionBits2, CN_VERSION_DECODE_INFO[I], 18);
    if Dist1 < BestDist1 then
    begin
      BestDist1 := Dist1;
      BestIdx1 := I;
    end;
    if Dist2 < BestDist2 then
    begin
      BestDist2 := Dist2;
      BestIdx2 := I;
    end;
  end;

  // 选最小距离的匹配结果
  if BestDist1 <= BestDist2 then
  begin
    if BestDist1 > 3 then
    begin
      Result := False;
      Exit;
    end;
    FQRVersion := BestIdx1 + 7;
  end
  else
  begin
    if BestDist2 > 3 then
    begin
      Result := False;
      Exit;
    end;
    FQRVersion := BestIdx2 + 7;
  end;

  // 验证版本号对应的矩阵尺寸
  if FQRSize <> FQRVersion * 4 + 17 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

// 读取并解码版本信息
function TCnQRDecoder.ReadVersion: Boolean;
var
  VersionBits1, VersionBits2: Integer;
begin
  // OutputDebugString(PChar(Format('VER: Size=%d', [FQRSize])));
  // 版本 <= 6 由尺寸直接推算
  if FQRSize <= 41 then
  begin
    FQRVersion := (FQRSize - 17) div 4;
    if (FQRVersion >= 1) and (FQRVersion <= 40) then
      Result := True
    else
      Result := False;
    Exit;
  end;

  // 版本 >= 7，从两份位置读取 18 位版本信息
  VersionBits1 := ExtractVersionBits(FQRSize - 11, 0);
  VersionBits2 := ExtractVersionBits(0, FQRSize - 11);
  Result := DecodeVersion(VersionBits1, VersionBits2);
end;

// 获取指定位置在指定掩码类型下的掩码值，与编码器中的逻辑完全一致
function TCnQRDecoder.GetMaskPattern(X, Y, MaskType: Integer): Boolean;
begin
  case MaskType of
    0:
      Result := (X + Y) mod 2 = 0;
    1:
      Result := Y mod 2 = 0;
    2:
      Result := X mod 3 = 0;
    3:
      Result := (X + Y) mod 3 = 0;
    4:
      Result := ((Y div 2) + (X div 3)) mod 2 = 0;
    5:
      Result := ((X * Y) mod 2) + ((X * Y) mod 3) = 0;
    6:
      Result := (((X * Y) mod 2) + ((X * Y) mod 3)) mod 2 = 0;
    7:
      Result := (((X + Y) mod 2) + ((X * Y) mod 3)) mod 2 = 0;
  else
    Result := False;
  end;
end;

// 判断指定位置是否属于功能区域（寻像图案/时序图案/格式信息/版本信息/对齐图案）
function TCnQRDecoder.IsFunctionArea(X, Y: Integer): Boolean;
var
  I, J, AlignCount: Integer;
  Arr2Ptr: PCn2BytesArray;
  Arr3Ptr: PCn3BytesArray;
  Arr4Ptr: PCn4BytesArray;
  Arr5Ptr: PCn5BytesArray;
  Arr6Ptr: PCn6BytesArray;
  Arr7Ptr: PCn7BytesArray;
  AlignCoords: array of Integer;
begin
  Result := True;

  // 边界检查
  if (X < 0) or (X >= FQRSize) or (Y < 0) or (Y >= FQRSize) then
    Exit;

  // 左上角位置探测图形 7x7
  if (X <= 7) and (Y <= 7) then
    Exit;

  // 右上角位置探测图形 7x7
  if (X >= FQRSize - 8) and (Y <= 7) then
    Exit;

  // 左下角位置探测图形 7x7
  if (X <= 7) and (Y >= FQRSize - 8) then
    Exit;

  // 时序图案：第6行和第6列
  if (X = 6) or (Y = 6) then
    Exit;

  // 格式信息区域
  if (Y = 8) and ((X <= 5) or (X = 7) or (X = 8)) then
    Exit;
  if (X = 8) and ((Y <= 5) or (Y = 7) or (Y = 8)) then
    Exit;
  if (Y = 8) and (X >= FQRSize - 8) then
    Exit;
  if (X = 8) and (Y >= FQRSize - 8) then
    Exit;

  // 版本信息区域（版本 7 及以上）
  if FQRVersion >= 7 then
  begin
    if (X >= FQRSize - 11) and (X <= FQRSize - 9) and (Y >= 0) and (Y <= 5) then
      Exit;
    if (X >= 0) and (X <= 5) and (Y >= FQRSize - 11) and (Y <= FQRSize - 9) then
      Exit;
  end;

  // 对齐图案区域
  AlignCount := 0;
  case FQRVersion of
    2..6:
      begin
        Arr2Ptr := CN_ALIGNMENT_PATTERN_2ARRAY[FQRVersion];
        SetLength(AlignCoords, 2);
        AlignCoords[0] := Arr2Ptr^[0];
        AlignCoords[1] := Arr2Ptr^[1];
        AlignCount := 2;
      end;
    7..13:
      begin
        Arr3Ptr := CN_ALIGNMENT_PATTERN_3ARRAY[FQRVersion];
        SetLength(AlignCoords, 3);
        AlignCoords[0] := Arr3Ptr^[0];
        AlignCoords[1] := Arr3Ptr^[1];
        AlignCoords[2] := Arr3Ptr^[2];
        AlignCount := 3;
      end;
    14..20:
      begin
        Arr4Ptr := CN_ALIGNMENT_PATTERN_4ARRAY[FQRVersion];
        SetLength(AlignCoords, 4);
        AlignCoords[0] := Arr4Ptr^[0];
        AlignCoords[1] := Arr4Ptr^[1];
        AlignCoords[2] := Arr4Ptr^[2];
        AlignCoords[3] := Arr4Ptr^[3];
        AlignCount := 4;
      end;
    21..27:
      begin
        Arr5Ptr := CN_ALIGNMENT_PATTERN_5ARRAY[FQRVersion];
        SetLength(AlignCoords, 5);
        AlignCoords[0] := Arr5Ptr^[0];
        AlignCoords[1] := Arr5Ptr^[1];
        AlignCoords[2] := Arr5Ptr^[2];
        AlignCoords[3] := Arr5Ptr^[3];
        AlignCoords[4] := Arr5Ptr^[4];
        AlignCount := 5;
      end;
    28..34:
      begin
        Arr6Ptr := CN_ALIGNMENT_PATTERN_6ARRAY[FQRVersion];
        SetLength(AlignCoords, 6);
        AlignCoords[0] := Arr6Ptr^[0];
        AlignCoords[1] := Arr6Ptr^[1];
        AlignCoords[2] := Arr6Ptr^[2];
        AlignCoords[3] := Arr6Ptr^[3];
        AlignCoords[4] := Arr6Ptr^[4];
        AlignCoords[5] := Arr6Ptr^[5];
        AlignCount := 6;
      end;
    35..40:
      begin
        Arr7Ptr := CN_ALIGNMENT_PATTERN_7ARRAY[FQRVersion];
        SetLength(AlignCoords, 7);
        AlignCoords[0] := Arr7Ptr^[0];
        AlignCoords[1] := Arr7Ptr^[1];
        AlignCoords[2] := Arr7Ptr^[2];
        AlignCoords[3] := Arr7Ptr^[3];
        AlignCoords[4] := Arr7Ptr^[4];
        AlignCoords[5] := Arr7Ptr^[5];
        AlignCoords[6] := Arr7Ptr^[6];
        AlignCount := 7;
      end;
  end;

  // 检查是否在对齐图案区域内（5x5区域）
  for I := 0 to AlignCount - 1 do
  begin
    for J := 0 to AlignCount - 1 do
    begin
      // 排除与寻像图案重叠的位置
      if ((AlignCoords[I] = 6) and (AlignCoords[J] = 6)) or
        ((AlignCoords[I] = 6) and (AlignCoords[J] = FQRSize - 7)) or
        ((AlignCoords[I] = FQRSize - 7) and (AlignCoords[J] = 6)) then
        Continue;

      if (X >= AlignCoords[I] - 2) and (X <= AlignCoords[I] + 2) and
        (Y >= AlignCoords[J] - 2) and (Y <= AlignCoords[J] + 2) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  Result := False;
end;

// 对非功能区域按 8 种掩码规则之一执行位翻转（XOR），还原原始数据
procedure TCnQRDecoder.UnmaskMatrix;
var
  X, Y, MaskType: Integer;
begin
  MaskType := FFormatInfo.MaskType;
  for X := 0 to FQRSize - 1 do
  begin
    for Y := 0 to FQRSize - 1 do
    begin
      if not IsFunctionArea(X, Y) then
      begin
        if GetMaskPattern(X, Y, MaskType) then
          FQRData[X, Y] := 1 - FQRData[X, Y];
      end;
    end;
  end;
end;

// 按 Z 字形路径从矩阵中读取码字字节序列
function TCnQRDecoder.ReadCodewords: TBytes;
var
  Right, Vert, J, X, Y, CodeCount, MaxCodewords: Integer;
  Upward: Boolean;
  CurrentByte, BitsInCurrentByte: Integer;
begin
  MaxCodewords := CN_TOTAL_CODEWORDS[FQRVersion];
  SetLength(Result, MaxCodewords);
  BitsInCurrentByte := 0;
  CurrentByte := 0;
  CodeCount := 0;
  Right := FQRSize - 1;

  while (Right >= 1) and (CodeCount < MaxCodewords) do
  begin
    if Right = 6 then
      Right := 5;  // 跳过时序图案列

    Upward := ((Right + 1) and 2) = 0;

    for Vert := 0 to FQRSize - 1 do
    begin
      for J := 0 to 1 do
      begin
        X := Right - J;
        if Upward then
          Y := FQRSize - 1 - Vert
        else
          Y := Vert;

        if not IsFunctionArea(X, Y) then
        begin
          CurrentByte := (CurrentByte shl 1) or FQRData[X, Y];
          Inc(BitsInCurrentByte);
          if BitsInCurrentByte = 8 then
          begin
            if CodeCount < MaxCodewords then
              Result[CodeCount] := CurrentByte;
            Inc(CodeCount);
            CurrentByte := 0;
            BitsInCurrentByte := 0;
          end;
        end;
      end;
    end;
    Dec(Right, 2);
  end;
end;

// 将交错的码字序列按版本和纠错级别解交织划分为数据块
procedure TCnQRDecoder.SplitDataBlocks(const RawCodewords: TBytes;
  var Blocks: array of TBytes; var BlockCount: Integer;
  var ECCPerBlock: Integer);
var
  TotalCodewords, NumBlocks, ShortBlockLen, LongBlockLen: Integer;
  NumLongBlocks, NumShortBlocks, ShortDataSize, LongDataSize: Integer;
  I, J, K, BlockLen: Integer;
begin
  TotalCodewords := CN_TOTAL_CODEWORDS[FQRVersion];
  NumBlocks := CN_NUM_ERROR_CORRECTION_BLOCKS[FQRVersion, Ord(FFormatInfo.ErrorLevel)];
  ECCPerBlock := CN_ECC_CODEWORDS_PER_BLOCK[FQRVersion, Ord(FFormatInfo.ErrorLevel)];
  BlockCount := NumBlocks;

  // 计算短块和长块的数量与长度
  ShortBlockLen := TotalCodewords div NumBlocks;
  LongBlockLen := ShortBlockLen + 1;
  NumLongBlocks := TotalCodewords mod NumBlocks;
  NumShortBlocks := NumBlocks - NumLongBlocks;
  ShortDataSize := ShortBlockLen - ECCPerBlock;
  LongDataSize := LongBlockLen - ECCPerBlock;

  // 初始化各块为空
  for I := 0 to NumBlocks - 1 do
    SetLength(Blocks[I], 0);

  K := 0;

  // 数据部分：按列优先分配所有块的数据码字，长块多一个数据字节
  for I := 0 to LongDataSize - 1 do
  begin
    for J := 0 to NumBlocks - 1 do
    begin
      // 短块没有对应的数据位置时跳过
      if (I >= ShortDataSize) and (J < NumShortBlocks) then
        Continue;
      if K < Length(RawCodewords) then
      begin
        BlockLen := Length(Blocks[J]);
        SetLength(Blocks[J], BlockLen + 1);
        Blocks[J][BlockLen] := RawCodewords[K];
        Inc(K);
      end;
    end;
  end;

  // 纠错码部分：按列优先分配所有块的纠错码
  for I := 0 to ECCPerBlock - 1 do
  begin
    for J := 0 to NumBlocks - 1 do
    begin
      if K < Length(RawCodewords) then
      begin
        BlockLen := Length(Blocks[J]);
        SetLength(Blocks[J], BlockLen + 1);
        Blocks[J][BlockLen] := RawCodewords[K];
        Inc(K);
      end;
    end;
  end;
end;

// 对数据块执行 GF(2^8) Reed-Solomon 纠错（Euclidean 算法 + Chien 搜索 + Forney 公式）
function TCnQRDecoder.RSDecodeBlock(var Data: TBytes; ECCount: Integer): Boolean;
var
  TwoS, I, J, K, N, ErrCount: Integer;
  Syndrome: array of Integer;
  ErrorLocations: array of Integer;
  ErrorValues: array of Integer;
  // Euclidean 算法多项式
  PolyR0, PolyR1, PolyR2: array of Integer;
  PolyT0, PolyT1, PolyT2: array of Integer;
  DegR0, DegR1, DegR2: Integer;
  DegT0, DegT1, DegT2: Integer;
  TempVal, TempDeg: Integer;
  // Forney 用
  Sigma, Omega: array of Integer;
  DegSigma, DegOmega, SigmaPrimeVal: Integer;
  // Chien 搜索
  ErrorFound, AllSyndromeZero: Boolean;
begin
  TwoS := ECCount;
  N := Length(Data);

  // Step 1: 计算伴随式 Syndromes（对照 zxing 反向存储）
  // S(x) = S[0] + S[1]x + ... + S[2t-1]x^(2t-1)
  // 其中 S[0] = R(alpha^(2t-1)), S[2t-1] = R(alpha^0)
  SetLength(Syndrome, TwoS);
  AllSyndromeZero := True;
  for I := 0 to TwoS - 1 do
  begin
    TempVal := Data[0];
    for J := 1 to N - 1 do
      TempVal := GFMul(TempVal, GFExp(I)) xor Data[J];
    Syndrome[I] := TempVal;
    if TempVal <> 0 then
      AllSyndromeZero := False;
  end;

//  OutputDebugString(PChar(Format('RS: N=%d t=%d S=%d,%d,%d,%d,%d,%d,%d,%d,%d,%d AllZero=%d',
//    [N, TwoS div 2, Syndrome[0], Syndrome[1], Syndrome[2], Syndrome[3],
//     Syndrome[4], Syndrome[5], Syndrome[6], Syndrome[7], Syndrome[8], Syndrome[9],
//     Ord(AllSyndromeZero)])));

  // 所有伴随式为 0，无错误
  if AllSyndromeZero then
  begin
    Result := True;
    Exit;
  end;

  // Step 2: Berlekamp-Massey 算法求解错误定位多项式 sigma(x)
  // C(x)=1, B(x)=1, L=0, m=1, b=1
  SetLength(Sigma, 1); Sigma[0] := 1; DegSigma := 0;
  SetLength(PolyT0, 1); PolyT0[0] := 1; DegT1 := 0;  // B(x)
  DegR0 := 0;  // L
  DegR1 := 1;  // m
  DegT0 := 1;  // b

  for I := 0 to TwoS - 1 do
  begin
    // 计算差值 d
    TempVal := Syndrome[I];
    for J := 1 to I do
      if J <= DegSigma then
        TempVal := TempVal xor GFMul(Sigma[J], Syndrome[I - J]);

    if TempVal = 0 then
      Inc(DegR1)
    else
    begin
      // 保存旧 C(x) 到 PolyR2
      SetLength(PolyR2, DegSigma + 1);
      for J := 0 to DegSigma do PolyR2[J] := Sigma[J];

      // T(x) = C(x) - d/b * x^m * B(x)
      TempDeg := GFMul(TempVal, GFInv(DegT0));  // d/b
      // 扩展 Sigma 数组以容纳新项
      if DegSigma < DegR1 + DegT1 then
        SetLength(Sigma, DegR1 + DegT1 + 1);
      for J := DegSigma + 1 to Length(Sigma) - 1 do
        Sigma[J] := 0;
      for J := 0 to DegT1 do
        Sigma[J + DegR1] := Sigma[J + DegR1] xor GFMul(TempDeg, PolyT0[J]);
      // 更新次数
      DegSigma := Length(Sigma) - 1;
      while (DegSigma >= 0) and (Sigma[DegSigma] = 0) do Dec(DegSigma);

      if 2 * DegR0 <= I then
      begin
        // B(x) = 旧 C(x), b = d, m = 1, L = i+1-L
        DegT1 := DegR0;
        SetLength(PolyT0, DegT1 + 1);
        for J := 0 to DegT1 do PolyT0[J] := PolyR2[J];
        DegT0 := TempVal;  // b = d (not d/b)
        DegR0 := I + 1 - DegR0;
        DegR1 := 1;
      end
      else
        Inc(DegR1);
    end;
  end;

  // 归一化 sigma(0) = 1
  if (DegSigma >= 0) and (Sigma[0] <> 0) then
    for I := 0 to DegSigma do
      Sigma[I] := GFDiv(Sigma[I], Sigma[0]);

//  OutputDebugString(PChar(Format('RS: BM sigma deg=%d c0=%d c1=%d c2=%d',
//    [DegSigma, Sigma[0], Sigma[1], Sigma[2]])));

  // omega(x) = S(x)*sigma(x) mod x^(2t)
  SetLength(Omega, TwoS);
  FillChar(Omega[0], TwoS * SizeOf(Integer), 0);
  for I := 0 to TwoS - 1 do
    for J := 0 to I do
      if J <= DegSigma then
        Omega[I] := Omega[I] xor GFMul(Syndrome[I - J], Sigma[J]);
  DegOmega := TwoS - 1;
  while (DegOmega >= 0) and (Omega[DegOmega] = 0) do
    Dec(DegOmega);

  // Step 3: Chien 搜索找错误位置
  ErrCount := 0;
  SetLength(ErrorLocations, N);
  for I := 0 to N - 1 do
  begin
    // 计算 sigma(alpha^(-i)) = sigma(alpha^(255-i))
    TempVal := 0;
    for J := 0 to DegSigma do
      TempVal := TempVal xor GFMul(Sigma[J], GFExp((255 - I) * J mod 255));
    if TempVal = 0 then
    begin
      ErrorLocations[ErrCount] := I;
      Inc(ErrCount);
    end;
//    if I < 3 then
//      OutputDebugString(PChar(Format('RS: chien i=%d val=%d', [I, TempVal])));
  end;

  // 检查错误数量是否在纠错能力范围内
  if ErrCount > TwoS div 2 then
  begin
    Result := False;
    Exit;
  end;

  // Step 4: Forney 公式计算错误值
  SetLength(ErrorValues, ErrCount);
  ErrorFound := True;
  for I := 0 to ErrCount - 1 do
  begin
    K := ErrorLocations[I];
    // 计算 omega(xi^(-1))
    TempVal := 0;
    for J := 0 to DegOmega do
      TempVal := TempVal xor GFMul(Omega[J], GFExp((255 - K) * J mod 255));

    // 计算 sigma'(xi^(-1)) - 形式导数：偶数次项忽略，奇次项系数不变
    SigmaPrimeVal := 0;
    for J := 0 to DegSigma do
    begin
      if (J mod 2 = 1) then  // 奇次项
        SigmaPrimeVal := SigmaPrimeVal xor
          GFMul(Sigma[J], GFExp((255 - K) * (J - 1) mod 255));
    end;

    if SigmaPrimeVal <> 0 then
      ErrorValues[I] := GFMul(GFDiv(TempVal, SigmaPrimeVal), GFExp(ErrorLocations[I]))
    else
    begin
      ErrorFound := False;
      Break;
    end;
  end;

  if not ErrorFound then
  begin
    Result := False;
    Exit;
  end;

//  OutputDebugString(PChar(Format('RS: fix %d err at [%d,%d,%d]',
//    [ErrCount, ErrorLocations[0], ErrorLocations[1], ErrorLocations[2]])));

  // Step 5: 修复错误
  for I := 0 to ErrCount - 1 do
  begin
    K := ErrorLocations[I];
      if (K >= 0) and (K < N) then
        Data[N - 1 - K] := Data[N - 1 - K] xor ErrorValues[I];
  end;

  Result := True;
end;

// 从数据字节流的 BitPos 位置读取 NumBits 位（大端序），返回整数值
function TCnQRDecoder.ReadBits(var BitPos: Integer; NumBits: Integer;
  const DataBytes: TBytes): Integer;
var
  I, ByteIndex, BitOffset: Integer;
begin
  Result := 0;
  for I := 0 to NumBits - 1 do
  begin
    ByteIndex := BitPos div 8;
    BitOffset := 7 - (BitPos mod 8);
    if (ByteIndex >= 0) and (ByteIndex < Length(DataBytes)) then
      Result := (Result shl 1) or ((DataBytes[ByteIndex] shr BitOffset) and 1)
    else
      Result := Result shl 1;
    Inc(BitPos);
  end;
end;

// 获取指定模式在 TCnQRDecoder 版本下的字符计数位长度
function TCnQRDecoder.GetCharCountBits(AMode: TCnQRDecodeMode): Integer;
begin
  case AMode of
    qrmNumeric:
      begin
        if FQRVersion <= 9 then Result := 10
        else if FQRVersion <= 26 then Result := 12
        else Result := 14;
      end;
    qrmAlphaNumeric:
      begin
        if FQRVersion <= 9 then Result := 9
        else if FQRVersion <= 26 then Result := 11
        else Result := 13;
      end;
    qrmByte:
      begin
        if FQRVersion <= 9 then Result := 8
        else Result := 16;
      end;
    qrmKanji, qrmHanzi:
      begin
        if FQRVersion <= 9 then Result := 8
        else if FQRVersion <= 26 then Result := 10
        else Result := 12;
      end;
  else
    Result := 0;
  end;
end;

// 解码 Numeric 模式段
function TCnQRDecoder.DecodeNumericSegment(var BitPos: Integer; ACount: Integer;
  const DataBytes: TBytes): string;
var
  I, Value: Integer;
  ThreeDigits: array[0..2] of Char;
begin
  Result := '';
  I := 0;
  while I < ACount do
  begin
    if ACount - I >= 3 then
    begin
      // 10 位读 3 位数字
      Value := ReadBits(BitPos, 10, DataBytes);
      ThreeDigits[0] := Char(Ord('0') + (Value div 100));
      ThreeDigits[1] := Char(Ord('0') + ((Value div 10) mod 10));
      ThreeDigits[2] := Char(Ord('0') + (Value mod 10));
      Result := Result + ThreeDigits[0] + ThreeDigits[1] + ThreeDigits[2];
      Inc(I, 3);
    end
    else if ACount - I >= 2 then
    begin
      // 7 位读 2 位数字
      Value := ReadBits(BitPos, 7, DataBytes);
      Result := Result + Char(Ord('0') + (Value div 10)) +
        Char(Ord('0') + (Value mod 10));
      Inc(I, 2);
    end
    else
    begin
      // 4 位读 1 位数字
      Value := ReadBits(BitPos, 4, DataBytes);
      Result := Result + Char(Ord('0') + Value);
      Inc(I, 1);
    end;
  end;
end;

// 解码 AlphaNumeric 模式段
function TCnQRDecoder.DecodeAlphaNumericSegment(var BitPos: Integer;
  ACount: Integer; const DataBytes: TBytes): string;
const
  ALPHA_TABLE: array[0..44] of Char =
    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';
var
  I, Value: Integer;
begin
  Result := '';
  I := 0;
  while I < ACount do
  begin
    if ACount - I >= 2 then
    begin
      // 11 位编码 2 个字符
      Value := ReadBits(BitPos, 11, DataBytes);
      Result := Result + ALPHA_TABLE[Value div 45] + ALPHA_TABLE[Value mod 45];
      Inc(I, 2);
    end
    else
    begin
      // 6 位编码 1 个字符
      Value := ReadBits(BitPos, 6, DataBytes);
      if Value < 45 then
        Result := Result + ALPHA_TABLE[Value];
      Inc(I, 1);
    end;
  end;
end;

// 解码 Byte 模式段，优先 UTF-8 解码，回退到 ISO-8859-1
function TCnQRDecoder.DecodeByteSegment(var BitPos: Integer; ACount: Integer;
  const DataBytes: TBytes): string;
var
  I: Integer;
  ByteBuf: array of Byte;
  Utf8Str: AnsiString;
  WideResult: WideString;
begin
  // 读取原始字节
  SetLength(ByteBuf, ACount);
  for I := 0 to ACount - 1 do
    ByteBuf[I] := ReadBits(BitPos, 8, DataBytes);

  // 将字节组装为 AnsiString（实际内容是 UTF-8 编码）
  SetLength(Utf8Str, ACount);
  for I := 0 to ACount - 1 do
    Utf8Str[I + 1] := AnsiChar(ByteBuf[I]);

  // 尝试 UTF-8 → WideString 解码
  WideResult := CnUtf8DecodeToWideString(Utf8Str);
  if WideResult <> '' then
    Result := WideResult  // WideString → AnsiString（当前代码页转换，如 CP936 保留汉字）
  else
  begin
    // 回退：逐字节当作 ISO-8859-1
    Result := '';
    for I := 0 to ACount - 1 do
      Result := Result + Char(ByteBuf[I]);
  end;
end;

// 解码 Kanji 模式段，按 Shift-JIS 编码
function TCnQRDecoder.DecodeKanjiSegment(var BitPos: Integer; ACount: Integer;
  const DataBytes: TBytes): string;
var
  I, Value, Assembled: Integer;
begin
  Result := '';
  for I := 0 to ACount - 1 do
  begin
    Value := ReadBits(BitPos, 13, DataBytes);
    Assembled := ((Value div $0C0) shl 8) or (Value mod $0C0);
    if Assembled < $1F00 then
      Assembled := Assembled + $8140
    else
      Assembled := Assembled + $C140;
    // Shift-JIS 转 Ansi（在 Delphi 5 中需要系统代码页支持）
    // 此处简化为输出 Shift-JIS 编码值占位
    Result := Result + '?';
  end;
end;

// 解码 Hanzi 模式段，按 GB2312 编码
function TCnQRDecoder.DecodeHanziSegment(var BitPos: Integer; ACount: Integer;
  const DataBytes: TBytes): string;
var
  I, Value, Assembled: Integer;
begin
  Result := '';
  for I := 0 to ACount - 1 do
  begin
    Value := ReadBits(BitPos, 13, DataBytes);
    Assembled := ((Value div $060) shl 8) or (Value mod $060);
    if Assembled < $0A00 then
      Assembled := Assembled + $0A1A1
    else
      Assembled := Assembled + $0A6A1;
    // GB2312 转 Ansi（在 Delphi 5 中需要系统代码页支持）
    Result := Result + '?';
  end;
end;

// 解析数据字节流，按编码模式段循环解码并返回完整文本
function TCnQRDecoder.DecodeDataStream(const DataBytes: TBytes): string;
var
  BitPos, ModeVal, CharCount, TotalBits: Integer;
begin
  Result := '';
  BitPos := 0;
  TotalBits := Length(DataBytes) * 8;

  while BitPos < TotalBits - 4 do
  begin
    // 读取 4 位模式指示符
    ModeVal := ReadBits(BitPos, 4, DataBytes);

    case ModeVal of
      0:
        Break;  // TERMINATOR
      1:
        begin
          CharCount := ReadBits(BitPos, GetCharCountBits(qrmNumeric), DataBytes);
          Result := Result + DecodeNumericSegment(BitPos, CharCount, DataBytes);
        end;
      2:
        begin
          CharCount := ReadBits(BitPos, GetCharCountBits(qrmAlphaNumeric), DataBytes);
          Result := Result + DecodeAlphaNumericSegment(BitPos, CharCount, DataBytes);
        end;
      4:
        begin
          CharCount := ReadBits(BitPos, GetCharCountBits(qrmByte), DataBytes);
          Result := Result + DecodeByteSegment(BitPos, CharCount, DataBytes);
        end;
      7:
        begin
          // ECI 模式：读取 ECI 指定值（7/8/16 位可变），跳过后续 Byte 段
          // 简化处理：跳过 ECI 头
          if BitPos < TotalBits then
          begin
            if ReadBits(BitPos, 1, DataBytes) = 0 then
              ReadBits(BitPos, 7, DataBytes)  // 7 位 ECI
            else if ReadBits(BitPos, 1, DataBytes) = 0 then
              ReadBits(BitPos, 14, DataBytes) // 14 位 ECI（两段）
            else
              ReadBits(BitPos, 21, DataBytes); // 21 位 ECI
          end;
        end;
      8:
        begin
          CharCount := ReadBits(BitPos, GetCharCountBits(qrmKanji), DataBytes);
          Result := Result + DecodeKanjiSegment(BitPos, CharCount, DataBytes);
        end;
      5, 9:
        begin
          // FNC1 第一/第二位置：跳过，后续有独立模式指示符
        end;
      13:
        begin
          ReadBits(BitPos, 4, DataBytes);  // 跳过子集指示符
          CharCount := ReadBits(BitPos, GetCharCountBits(qrmHanzi), DataBytes);
          Result := Result + DecodeHanziSegment(BitPos, CharCount, DataBytes);
        end;
      3:
        begin
          ReadBits(BitPos, 16, DataBytes);
        end;
    else
      begin
        // 未知模式：回退 4 位，尝试用 Byte 模式重新解析
        Dec(BitPos, 4);
        CharCount := ReadBits(BitPos, GetCharCountBits(qrmByte), DataBytes);
        Result := Result + DecodeByteSegment(BitPos, CharCount, DataBytes);
      end;
    end;
  end;
end;

// 将当前矩阵沿主对角线翻转（QRData[X, Y] 与 QRData[Y, X] 互换）
procedure TCnQRDecoder.MirrorMatrix;
var
  X, Y: Integer;
  Temp: Byte;
begin
  for X := 0 to FQRSize - 1 do
  begin
    for Y := X + 1 to FQRSize - 1 do
    begin
      Temp := FQRData[X, Y];
      FQRData[X, Y] := FQRData[Y, X];
      FQRData[Y, X] := Temp;
    end;
  end;
end;

// 主解码入口。从 TCnQRData 矩阵解码出文本字符串。
// 自动尝试镜像重试：正常解码失败后镜像翻转矩阵重新解码
function TCnQRDecoder.DecodeMatrix(const AQRData: TCnQRData): string;
var
  Attempt: Integer;
  I, BlockCount, ECCPerBlock, TotalDataBytes, DataByteIdx: Integer;
  Blocks: array of TBytes;
  AllDataBytes, Codewords: TBytes;
  LastErrorMessage: string;
begin
  Result := '';
  LastErrorMessage := '';
  FQRData := AQRData;
  FQRSize := Length(FQRData);
  FIsMirrored := False;

  if FQRSize < 21 then
    raise ECnQRCodeException.Create(SCnErrorQRMatrixSizeTooSmall);

  for Attempt := 0 to 1 do
  begin
    try
      // Step 1: 格式信息
      if not ReadFormatInformation then
        raise ECnQRCodeException.Create(SCnErrorQRFormatInformationDecodeFailed);

      // Step 2: 版本信息
      if not ReadVersion then
        raise ECnQRCodeException.Create(SCnErrorQRVersionInformationDecodeFailed);

      // Step 3: 去掩码
      UnmaskMatrix;

      // Step 4: 读取码字
      Codewords := ReadCodewords;

      // Step 5: 数据块划分
      SetLength(Blocks, 40);  // 最大 40 块
      BlockCount := 0;
      ECCPerBlock := 0;
      SplitDataBlocks(Codewords, Blocks, BlockCount, ECCPerBlock);

      // Step 6: RS 纠错
      for I := 0 to BlockCount - 1 do
      begin
        if not RSDecodeBlock(Blocks[I], ECCPerBlock) then
          raise ECnQRCodeException.CreateFmt(SCnErrorQRChecksumErrorInBlock, [I]);
      end;

      // Step 7: 合并数据部分
      TotalDataBytes := 0;
      for I := 0 to BlockCount - 1 do
        Inc(TotalDataBytes, Length(Blocks[I]) - ECCPerBlock);

      SetLength(AllDataBytes, TotalDataBytes);
      DataByteIdx := 0;
      for I := 0 to BlockCount - 1 do
      begin
        Move(Blocks[I][0], AllDataBytes[DataByteIdx],
          Length(Blocks[I]) - ECCPerBlock);
        Inc(DataByteIdx, Length(Blocks[I]) - ECCPerBlock);
      end;

      // Step 8: 比特流解析
      Result := DecodeDataStream(AllDataBytes);
      if Result <> '' then
        Exit;

    except
      on E: ECnQRCodeException do
      begin
        LastErrorMessage := E.Message;
        if Attempt = 0 then
        begin
          // 镜像重试：沿主对角线翻转矩阵
          MirrorMatrix;
          FIsMirrored := True;
          // 重置为镜像后的数据继续解码
        end
        else
        begin
          // 两次都失败，抛出异常
          FErrorMessage := LastErrorMessage;
          raise ECnQRCodeException.Create(LastErrorMessage);
        end;
      end;
    end;
  end;

  if Result = '' then
  begin
    FErrorMessage := Format(
      'QR decode failed: V%d Sz%d EL%d M%d CW0=%d CW1=%d DB0=%d DB1=%d',
      [FQRVersion, FQRSize, Ord(FFormatInfo.ErrorLevel), FFormatInfo.MaskType,
       Codewords[0] and $FF, Codewords[1] and $FF,
       AllDataBytes[0] and $FF, AllDataBytes[1] and $FF]);
    raise ECnQRCodeException.Create(FErrorMessage);
  end;
end;

function CnQRDecodeFromMatrix(const AQRData: TCnQRData): string;
var
  Decoder: TCnQRDecoder;
begin
  if Length(AQRData) < 21 then
    raise ECnQRCodeException.Create(SCnErrorQRMatrixSizeTooSmall);

  Decoder := TCnQRDecoder.Create;
  try
    Result := Decoder.DecodeMatrix(AQRData);
  finally
    Decoder.Free;
  end;
end;

{ ---- 阶段二辅助函数 ---- }

// 计算两个寻像图案中心的欧几里得距离
function CnQRDistance(const P1, P2: TCnQRFinderPattern): Double;
begin
  Result := Sqrt((P1.X - P2.X) * (P1.X - P2.X) + (P1.Y - P2.Y) * (P1.Y - P2.Y));
end;

// 计算两个浮点坐标点的欧几里得距离
function CnQRDistancePointF(const P1, P2: TCnQRPointF): Double;
begin
  Result := Sqrt((P1.X - P2.X) * (P1.X - P2.X) + (P1.Y - P2.Y) * (P1.Y - P2.Y));
end;

// 将灰度图像二值化输出 0/1 矩阵
function CnQRBinarize(const AGrayImage: TCnQRData): TCnQRData;
var
  X, Y, AWidth, AHeight, BlockCols, BlockRows, BlockX, BlockY: Integer;
  StartX, StartY, EndX, EndY: Integer;
  Histogram: array[0..255] of Integer;
  I, MinVal, MaxVal, TotalPixels: Integer;
  SumDark, SumLight, CountDark, CountLight: Integer;
  Threshold: Integer;
begin
  AWidth := Length(AGrayImage);
  AHeight := Length(AGrayImage[0]);
  SetLength(Result, AWidth, AHeight);

  // 8x8 子块划分
  BlockCols := (AWidth + 7) div 8;
  BlockRows := (AHeight + 7) div 8;

  for BlockY := 0 to BlockRows - 1 do
  begin
    StartY := BlockY * 8;
    EndY := StartY + 7;
    if EndY >= AHeight then
      EndY := AHeight - 1;

    for BlockX := 0 to BlockCols - 1 do
    begin
      StartX := BlockX * 8;
      EndX := StartX + 7;
      if EndX >= AWidth then
        EndX := AWidth - 1;

      FillChar(Histogram, SizeOf(Histogram), 0);
      MinVal := 255;
      MaxVal := 0;
      for Y := StartY to EndY do
        for X := StartX to EndX do
        begin
          I := AGrayImage[X, Y];
          Inc(Histogram[I]);
          if I < MinVal then MinVal := I;
          if I > MaxVal then MaxVal := I;
        end;

      if MaxVal - MinVal < 24 then
        Threshold := 128
      else
      begin
        TotalPixels := (EndX - StartX + 1) * (EndY - StartY + 1);
        SumDark := 0; CountDark := 0;
        for I := 0 to 255 do
        begin
          Inc(CountDark, Histogram[I]);
          Inc(SumDark, I * Histogram[I]);
          if CountDark >= TotalPixels * 5 div 100 then
          begin
            if CountDark > 0 then SumDark := SumDark div CountDark
            else SumDark := 0;
            Break;
          end;
        end;
        SumLight := 0; CountLight := 0;
        for I := 255 downto 0 do
        begin
          Inc(CountLight, Histogram[I]);
          Inc(SumLight, I * Histogram[I]);
          if CountLight >= TotalPixels * 5 div 100 then
          begin
            if CountLight > 0 then SumLight := SumLight div CountLight
            else SumLight := 255;
            Break;
          end;
        end;
        Threshold := (SumDark + SumLight) div 2;
      end;

      for Y := StartY to EndY do
        for X := StartX to EndX do
        begin
          if AGrayImage[X, Y] < Threshold then
            Result[X, Y] := 1
          else
            Result[X, Y] := 0;
        end;
    end;
  end;
end;

// 验证五段黑白黑白黑比例是否为 1:1:3:1:1
function CnQRFoundPatternCross(const stateCount: array of Integer): Boolean;
var
  TotalModuleSize, I: Integer;
begin
  TotalModuleSize := 0;
  for I := 0 to 4 do
    Inc(TotalModuleSize, stateCount[I]);

  if TotalModuleSize < 7 then
  begin
    Result := False;
    Exit;
  end;

  TotalModuleSize := TotalModuleSize div 7;
  Result := (Abs(stateCount[0] - TotalModuleSize) < TotalModuleSize) and
    (Abs(stateCount[1] - TotalModuleSize) < TotalModuleSize) and
    (Abs(stateCount[2] - 3 * TotalModuleSize) < 3 * TotalModuleSize) and
    (Abs(stateCount[3] - TotalModuleSize) < TotalModuleSize) and
    (Abs(stateCount[4] - TotalModuleSize) < TotalModuleSize);
end;

// 在候选中心列坐标处沿垂直方向扫描并验证 1:1:3:1:1 比例，返回精确行坐标
function CnQRCrossCheckVertical(const ABinarized: TCnQRData;
  AWidth, AHeight, StartX, StartY, CenterY, MaxCount: Integer;
  const stateCount: array of Integer): Double;
var
  Y, TopY: Integer;
  LocalStateCount: array[0..4] of Integer;
begin
  // 垂直交叉验证：
  // 向上: state[2]+=中心黑, [1]=白, [0]=黑
  // 向下: state[2]+=中心黑, [3]=白, [4]=黑
  FillChar(LocalStateCount, SizeOf(LocalStateCount), 0);

  Y := StartY;
  // 向上
  while (Y >= 0) and (ABinarized[StartX, Y] = 1) and (LocalStateCount[2] < MaxCount) do
  begin
    Inc(LocalStateCount[2]);
    Dec(Y);
  end;
  if Y < 0 then begin Result := -1.0; Exit; end;
  while (Y >= 0) and (ABinarized[StartX, Y] = 0) and (LocalStateCount[1] < MaxCount) do
  begin
    Inc(LocalStateCount[1]);
    Dec(Y);
  end;
  if Y < 0 then begin Result := -1.0; Exit; end;
  while (Y >= 0) and (ABinarized[StartX, Y] = 1) and (LocalStateCount[0] < MaxCount) do
  begin
    Inc(LocalStateCount[0]);
    Dec(Y);
  end;
  // TopY = 最顶段黑[0]的首个像素行坐标
  TopY := Y + 1;

  // 向下
  Y := StartY + 1;
  while (Y < AHeight) and (ABinarized[StartX, Y] = 1) and (LocalStateCount[2] < MaxCount) do
  begin
    Inc(LocalStateCount[2]);
    Inc(Y);
  end;
  while (Y < AHeight) and (ABinarized[StartX, Y] = 0) and (LocalStateCount[3] < MaxCount) do
  begin
    Inc(LocalStateCount[3]);
    Inc(Y);
  end;
  while (Y < AHeight) and (ABinarized[StartX, Y] = 1) and (LocalStateCount[4] < MaxCount) do
  begin
    Inc(LocalStateCount[4]);
    Inc(Y);
  end;

  // 验证 1:1:3:1:1 比例
  if not CnQRFoundPatternCross(LocalStateCount) then
  begin
    Result := -1.0;
    Exit;
  end;

  // 中心 = TopY + state[0] + state[1] + state[2]/2
  Result := TopY + LocalStateCount[0] + LocalStateCount[1] + LocalStateCount[2] / 2.0;
end;

// 在二值矩阵中定位三个寻像图案
function CnQRFindFinderPatterns(const ABinarized: TCnQRData;
  out TopLeft, TopRight, BottomLeft: TCnQRFinderPattern): Boolean;
const
  MIN_SKIP = 3;
  MAX_CANDIDATES = 25;
var
  Candidates: array[0..24] of TCnQRFinderPattern;
  CandidateCount, AWidth, AHeight: Integer;
  Y, X, I, J, K: Integer;
  CurrentState: Integer;
  stateCount: array[0..4] of Integer;
  CenterX, CenterY: Double;
  ModuleSize: Double;
  Found: Boolean;
  TotalPixelCount: Integer;
  BestDist, Dist, MaxDist: Double;
  BestI, BestJ, BestK: Integer;
  Patterns: array[0..2] of TCnQRFinderPattern;
  D01, D02, D12: Double;
  A, B, C: Double;
  Score, BestScore: Double;
  PointOrder: array[0..2] of Integer;
begin
  CandidateCount := 0;
  Y := 0;
  AWidth := Length(ABinarized);
  AHeight := Length(ABinarized[0]);

  while Y < AHeight do
  begin
    // 状态机（CurrentState）:
    // 0=等待第一个黑色, 1=计数第一段黑(state[0]), 2=计数白(state[1]),
    // 3=计数中心黑(state[2]=3模块), 4=计数白(state[3]), 5=计数最后一段黑(state[4])
    // 6=状态5遇到白色→检查完整模式[0..4] 1:1:3:1:1
    FillChar(stateCount, SizeOf(stateCount), 0);
    CurrentState := 0;
    X := 0;

    while X < AWidth do
    begin
      case CurrentState of
        0:
          begin
            if ABinarized[X, Y] = 1 then  // 遇到第一个黑色
            begin
              stateCount[0] := 1;
              CurrentState := 1;
            end;
          end;
        1:
          begin  // 计数第一段黑
            if ABinarized[X, Y] = 1 then
              Inc(stateCount[0])
            else  // 遇到白色，转入状态2
            begin
              stateCount[1] := 1;
              CurrentState := 2;
            end;
          end;
        2:
          begin  // 计数白色段
            if ABinarized[X, Y] = 0 then
              Inc(stateCount[1])
            else  // 遇到黑色，转入状态3
            begin
              stateCount[2] := 1;
              CurrentState := 3;
            end;
          end;
        3:
          begin  // 计数中心黑色段(3模块)
            if ABinarized[X, Y] = 1 then
              Inc(stateCount[2])
            else  // 遇到白色，转入状态4
            begin
              stateCount[3] := 1;
              CurrentState := 4;
            end;
          end;
        4:
          begin  // 计数白色段
            if ABinarized[X, Y] = 0 then
              Inc(stateCount[3])
            else  // 遇到黑色，转入状态5
            begin
              stateCount[4] := 1;
              CurrentState := 5;
            end;
          end;
        5:
          begin  // 计数最后一段黑
            if ABinarized[X, Y] = 1 then
              Inc(stateCount[4])
            else  // 遇到白色，模式完成！检查 1:1:3:1:1
            begin
              if CnQRFoundPatternCross(stateCount) then
              begin
                TotalPixelCount := 0;
                for I := 0 to 4 do
                  Inc(TotalPixelCount, stateCount[I]);
                ModuleSize := TotalPixelCount / 7.0;
                CenterX := X - stateCount[4] - stateCount[3] - stateCount[2] / 2.0;
                CenterY := Y;

                // 垂直交叉验证
                CenterY := CnQRCrossCheckVertical(ABinarized, AWidth, AHeight,
                  Round(CenterX), Round(CenterY), Round(CenterY), stateCount[2] * 2,
                  stateCount);
                if CenterY >= 0 then
                begin
                  Found := False;
                  for I := 0 to CandidateCount - 1 do
                  begin
                    Dist := Sqrt((CenterX - Candidates[I].X) * (CenterX - Candidates[I].X) +
                      (CenterY - Candidates[I].Y) * (CenterY - Candidates[I].Y));
                    if Dist < Candidates[I].EstimatedModuleSize * 2 then
                    begin
                      Candidates[I].X := (Candidates[I].X * Candidates[I].ConfirmedCount + CenterX) /
                        (Candidates[I].ConfirmedCount + 1);
                      Candidates[I].Y := (Candidates[I].Y * Candidates[I].ConfirmedCount + CenterY) /
                        (Candidates[I].ConfirmedCount + 1);
                      Candidates[I].EstimatedModuleSize :=
                        (Candidates[I].EstimatedModuleSize * Candidates[I].ConfirmedCount + ModuleSize) /
                        (Candidates[I].ConfirmedCount + 1);
                      Inc(Candidates[I].ConfirmedCount);
                      Found := True;
                      Break;
                    end;
                  end;

                  if not Found and (CandidateCount < MAX_CANDIDATES) then
                  begin
                    Candidates[CandidateCount].X := CenterX;
                    Candidates[CandidateCount].Y := CenterY;
                    Candidates[CandidateCount].EstimatedModuleSize := ModuleSize;
                    Candidates[CandidateCount].ConfirmedCount := 1;
                    Inc(CandidateCount);
                  end;
                end;
              end;

              // 重置：保留最后两段继续搜索
              stateCount[0] := stateCount[2];
              stateCount[1] := stateCount[3];
              stateCount[2] := stateCount[4];
              stateCount[3] := 1;   // 当前白色段
              stateCount[4] := 0;
              CurrentState := 4;    // 回到计数白色
            end;
          end;
      end;
      Inc(X);
    end;

    // 动态行跳跃
    if CandidateCount > 2 then
    begin
      ModuleSize := 0;
      for I := 0 to CandidateCount - 1 do
        ModuleSize := ModuleSize + Candidates[I].EstimatedModuleSize;
      ModuleSize := ModuleSize / CandidateCount;
      Y := Y + Round(ModuleSize);
    end
    else
      Inc(Y, MIN_SKIP);
  end;

  // 筛选三个最佳图案
  if CandidateCount < 3 then
  begin
    Result := False;
    Exit;
  end;

  BestScore := 1E30;
  BestI := -1;
  BestJ := -1;
  BestK := -1;

  for I := 0 to CandidateCount - 3 do
    for J := I + 1 to CandidateCount - 2 do
      for K := J + 1 to CandidateCount - 1 do
      begin
        // 计算三边距离
        D01 := Sqrt((Candidates[I].X - Candidates[J].X) * (Candidates[I].X - Candidates[J].X) +
          (Candidates[I].Y - Candidates[J].Y) * (Candidates[I].Y - Candidates[J].Y));
        D02 := Sqrt((Candidates[I].X - Candidates[K].X) * (Candidates[I].X - Candidates[K].X) +
          (Candidates[I].Y - Candidates[K].Y) * (Candidates[I].Y - Candidates[K].Y));
        D12 := Sqrt((Candidates[J].X - Candidates[K].X) * (Candidates[J].X - Candidates[K].X) +
          (Candidates[J].Y - Candidates[K].Y) * (Candidates[J].Y - Candidates[K].Y));

        // 排序列 a ≤ b ≤ c
        if (D01 <= D02) and (D01 <= D12) then
        begin
          A := D01;
          if D02 <= D12 then begin B := D02; C := D12; end
          else begin B := D12; C := D02; end;
        end
        else if (D02 <= D01) and (D02 <= D12) then
        begin
          A := D02;
          if D01 <= D12 then begin B := D01; C := D12; end
          else begin B := D12; C := D01; end;
        end
        else
        begin
          A := D12;
          if D01 <= D02 then begin B := D01; C := D02; end
          else begin B := D02; C := D01; end;
        end;

        // 评估等腰直角三角形程度
        Score := Abs(C * C - 2 * B * B) + Abs(C * C - 2 * A * A);
        if Score < BestScore then
        begin
          BestScore := Score;
          BestI := I;
          BestJ := J;
          BestK := K;
        end;
      end;

  if (BestI < 0) or (BestJ < 0) or (BestK < 0) then
  begin
    Result := False;
    Exit;
  end;

  Patterns[0] := Candidates[BestI];
  Patterns[1] := Candidates[BestJ];
  Patterns[2] := Candidates[BestK];

  // 确认三个点的模块尺寸相差不超过 40%
  ModuleSize := (Patterns[0].EstimatedModuleSize + Patterns[1].EstimatedModuleSize +
    Patterns[2].EstimatedModuleSize) / 3.0;
  MaxDist := ModuleSize * 0.4;
  if (Abs(Patterns[0].EstimatedModuleSize - ModuleSize) > MaxDist) or
     (Abs(Patterns[1].EstimatedModuleSize - ModuleSize) > MaxDist) or
     (Abs(Patterns[2].EstimatedModuleSize - ModuleSize) > MaxDist) then
  begin
    Result := False;
    Exit;
  end;

  // 排序：找出左上、右上、左下
  // 最长边对面的点为左上角
  D01 := Sqrt((Patterns[0].X - Patterns[1].X) * (Patterns[0].X - Patterns[1].X) +
    (Patterns[0].Y - Patterns[1].Y) * (Patterns[0].Y - Patterns[1].Y));
  D02 := Sqrt((Patterns[0].X - Patterns[2].X) * (Patterns[0].X - Patterns[2].X) +
    (Patterns[0].Y - Patterns[2].Y) * (Patterns[0].Y - Patterns[2].Y));
  D12 := Sqrt((Patterns[1].X - Patterns[2].X) * (Patterns[1].X - Patterns[2].X) +
    (Patterns[1].Y - Patterns[2].Y) * (Patterns[1].Y - Patterns[2].Y));

  // 最长边对面的点为左上角
  if (D01 >= D02) and (D01 >= D12) then
  begin
    // D01 最长，Points[2] 为左上角
    PointOrder[0] := 2;  // 左上
    // 叉积判断其余两点
    if ((Patterns[1].X - Patterns[2].X) * (Patterns[0].Y - Patterns[2].Y) -
        (Patterns[1].Y - Patterns[2].Y) * (Patterns[0].X - Patterns[2].X)) > 0 then
    begin
      PointOrder[1] := 1;  // 右上
      PointOrder[2] := 0;  // 左下
    end
    else
    begin
      PointOrder[1] := 0;  // 右上
      PointOrder[2] := 1;  // 左下
    end;
  end
  else if (D02 >= D01) and (D02 >= D12) then
  begin
    // D02 最长，Points[1] 为左上角
    PointOrder[0] := 1;
    if ((Patterns[2].X - Patterns[1].X) * (Patterns[0].Y - Patterns[1].Y) -
        (Patterns[2].Y - Patterns[1].Y) * (Patterns[0].X - Patterns[1].X)) > 0 then
    begin
      PointOrder[1] := 2;
      PointOrder[2] := 0;
    end
    else
    begin
      PointOrder[1] := 0;
      PointOrder[2] := 2;
    end;
  end
  else
  begin
    // D12 最长，Points[0] 为左上角
    PointOrder[0] := 0;
    if ((Patterns[1].X - Patterns[0].X) * (Patterns[2].Y - Patterns[0].Y) -
        (Patterns[1].Y - Patterns[0].Y) * (Patterns[2].X - Patterns[0].X)) > 0 then
    begin
      PointOrder[1] := 1;
      PointOrder[2] := 2;
    end
    else
    begin
      PointOrder[1] := 2;
      PointOrder[2] := 1;
    end;
  end;

  TopLeft := Patterns[PointOrder[0]];
  TopRight := Patterns[PointOrder[1]];
  BottomLeft := Patterns[PointOrder[2]];
  Result := True;
end;

// 在二值矩阵中定位对齐图案
function CnQRFindAlignmentPattern(const ABinarized: TCnQRData;
  AWidth, AHeight: Integer; const TopLeft, TopRight,
  BottomLeft: TCnQRFinderPattern;
  out AlignmentPattern: TCnQRAlignmentPattern): Boolean;
var
  EstimatedVersion: Integer;
  ModuleSize: Double;
  BottomRightX, BottomRightY: Double;
  StartX, StartY, SearchRadius: Integer;
  X, Y: Integer;
  stateCount: array[0..2] of Integer;
  CurrentState: Integer;
  MaxCount: Integer;
  CenterX, CenterY: Double;
  Confirmed: Boolean;
begin
  Result := False;

  // 估算版本号
  ModuleSize := (CnQRDistance(TopLeft, TopRight) + CnQRDistance(TopLeft, BottomLeft)) / 28.0;
  EstimatedVersion := Round((CnQRDistance(TopLeft, TopRight) / ModuleSize + 7 - 17) / 4);

  if EstimatedVersion <= 1 then
    Exit;

  // 估算右下角位置
  BottomRightX := TopRight.X - TopLeft.X + BottomLeft.X;
  BottomRightY := TopRight.Y - TopLeft.Y + BottomLeft.Y;

  // 从右下角向中心偏移，搜索对齐图案
  StartX := Round(BottomRightX - ModuleSize * 3);
  StartY := Round(BottomRightY - ModuleSize * 3);
  SearchRadius := Round(ModuleSize * 4);

  if SearchRadius < 3 then
    SearchRadius := 3;

  // 尝试不同搜索半径（最多扩大到 16 倍模块尺寸）
  while SearchRadius < Round(ModuleSize * 16) do
  begin
    CenterX := -1;
    CenterY := -1;

    // 水平搜索 1:1:1 比例
    for Y := StartY to StartY + SearchRadius * 2 do
    begin
      if (Y < 0) or (Y >= AHeight) then
        Continue;

      FillChar(stateCount, SizeOf(stateCount), 0);
      CurrentState := 0;

      for X := StartX to StartX + SearchRadius * 2 do
      begin
        if (X < 0) or (X >= AWidth) then
          Continue;

        if ABinarized[X, Y] = 1 then  // 黑色
        begin
          if (CurrentState and 1) = 1 then
            Inc(stateCount[CurrentState])
          else if CurrentState = 2 then
          begin
            // 检查 1:1:1 比例
            MaxCount := stateCount[0];
            if stateCount[1] > MaxCount then MaxCount := stateCount[1];
            if stateCount[2] > MaxCount then MaxCount := stateCount[2];
            if (MaxCount > 0) and
               (Abs(stateCount[0] - stateCount[1]) * 2 < MaxCount) and
               (Abs(stateCount[1] - stateCount[2]) * 2 < MaxCount) then
            begin
              CenterX := X - stateCount[2] - stateCount[1] / 2.0;
              CenterY := Y;
              Break;
            end;
            stateCount[0] := stateCount[2];
            stateCount[1] := 1;
            stateCount[2] := 0;
            CurrentState := 1;
          end
          else
          begin
            Inc(CurrentState);
            stateCount[CurrentState] := 1;
          end;
        end
        else  // 白色
        begin
          if (CurrentState and 1) = 0 then
            Inc(stateCount[CurrentState])
          else
          begin
            Inc(CurrentState);
            stateCount[CurrentState] := 1;
          end;
        end;
      end;

      if CenterX >= 0 then
        Break;
    end;

    if CenterX >= 0 then
    begin
      // 垂直交叉验证
      Confirmed := False;
      FillChar(stateCount, SizeOf(stateCount), 0);
      CurrentState := 0;
      for Y := Round(CenterY - SearchRadius) to Round(CenterY + SearchRadius) do
      begin
        if (Y < 0) or (Y >= AHeight) then
          Continue;
        X := Round(CenterX);
        if (X < 0) or (X >= AWidth) then
          Continue;

        if ABinarized[X, Y] = 1 then
        begin
          if (CurrentState and 1) = 1 then
            Inc(stateCount[CurrentState])
          else if CurrentState = 2 then
          begin
            MaxCount := stateCount[0];
            if stateCount[1] > MaxCount then MaxCount := stateCount[1];
            if stateCount[2] > MaxCount then MaxCount := stateCount[2];
            if (MaxCount > 0) and
               (Abs(stateCount[0] - stateCount[1]) * 2 < MaxCount) and
               (Abs(stateCount[1] - stateCount[2]) * 2 < MaxCount) then
            begin
              CenterY := Y - stateCount[2] - stateCount[1] / 2.0;
              Confirmed := True;
              Break;
            end;
            stateCount[0] := stateCount[2];
            stateCount[1] := 1;
            stateCount[2] := 0;
            CurrentState := 1;
          end
          else
          begin
            Inc(CurrentState);
            stateCount[CurrentState] := 1;
          end;
        end
        else
        begin
          if (CurrentState and 1) = 0 then
            Inc(stateCount[CurrentState])
          else
          begin
            Inc(CurrentState);
            stateCount[CurrentState] := 1;
          end;
        end;
      end;

      if Confirmed then
      begin
        AlignmentPattern.X := CenterX;
        AlignmentPattern.Y := CenterY;
        AlignmentPattern.EstimatedModuleSize := ModuleSize;
        Result := True;
        Exit;
      end;
    end;

    // 扩大搜索半径
    SearchRadius := SearchRadius * 2;
    StartX := Round(BottomRightX - SearchRadius);
    StartY := Round(BottomRightY - SearchRadius);
  end;
end;

// 根据源四边形 4 点和目标四边形 4 点计算透视变换矩阵（对照 zxing 精确实现）
function CnQRCalcPerspectiveTransform(const SrcPoints: array of TCnQRPointF;
  const DstPoints: array of TCnQRPointF): TCnQRPerspectiveTransform;

  // squareToQuadrilateral: (0,0)-(1,0)-(1,1)-(0,1) → 目标四边形
  procedure SquareToQuad(const Pts: array of TCnQRPointF;
    out T: TCnQRPerspectiveTransform);
  var
    dx1, dy1, dx2, dy2, dx3, dy3: Single;
    denom, a13, a23: Single;
  begin
    dx3 := Pts[0].X - Pts[1].X + Pts[2].X - Pts[3].X;
    dy3 := Pts[0].Y - Pts[1].Y + Pts[2].Y - Pts[3].Y;
    if (Abs(dx3) < 1E-10) and (Abs(dy3) < 1E-10) then
    begin
      // 仿射
      T.a11 := Pts[1].X - Pts[0].X;
      T.a12 := Pts[3].X - Pts[0].X;
      T.a13 := Pts[0].X;
      T.a21 := Pts[1].Y - Pts[0].Y;
      T.a22 := Pts[3].Y - Pts[0].Y;
      T.a23 := Pts[0].Y;
      T.a31 := 0;
      T.a32 := 0;
      T.a33 := 1;
    end
    else
    begin
      dx1 := Pts[1].X - Pts[2].X;
      dx2 := Pts[3].X - Pts[2].X;
      dy1 := Pts[1].Y - Pts[2].Y;
      dy2 := Pts[3].Y - Pts[2].Y;
      denom := dx1 * dy2 - dx2 * dy1;
      if Abs(denom) < 1E-10 then
      begin
        // 退化为仿射
        T.a11 := Pts[1].X - Pts[0].X;
        T.a12 := Pts[3].X - Pts[0].X;
        T.a13 := Pts[0].X;
        T.a21 := Pts[1].Y - Pts[0].Y;
        T.a22 := Pts[3].Y - Pts[0].Y;
        T.a23 := Pts[0].Y;
        T.a31 := 0;
        T.a32 := 0;
        T.a33 := 1;
      end
      else
      begin
        a13 := (dx3 * dy2 - dx2 * dy3) / denom;
        a23 := (dx1 * dy3 - dx3 * dy1) / denom;
        T.a11 := Pts[1].X - Pts[0].X + a13 * Pts[1].X;
        T.a12 := Pts[3].X - Pts[0].X + a23 * Pts[3].X;
        T.a13 := Pts[0].X;
        T.a21 := Pts[1].Y - Pts[0].Y + a13 * Pts[1].Y;
        T.a22 := Pts[3].Y - Pts[0].Y + a23 * Pts[3].Y;
        T.a23 := Pts[0].Y;
        T.a31 := a13;
        T.a32 := a23;
        T.a33 := 1;
      end;
    end;
  end;

  // buildAdjoint: 行主序(row-major)伴随矩阵 = 余子式矩阵的转置
  procedure Adjoint(const T: TCnQRPerspectiveTransform;
    out A: TCnQRPerspectiveTransform);
  begin
    // C[i][j] = (-1)^(i+j) * det(去掉行i列j后的子矩阵)
    // adj = C^T
    A.a11 := T.a22 * T.a33 - T.a23 * T.a32;   // C00
    A.a12 := T.a13 * T.a32 - T.a12 * T.a33;   // C10 → adj[0][1]
    A.a13 := T.a12 * T.a23 - T.a13 * T.a22;   // C20 → adj[0][2]
    A.a21 := T.a23 * T.a31 - T.a21 * T.a33;   // C01 → adj[1][0]
    A.a22 := T.a11 * T.a33 - T.a13 * T.a31;   // C11
    A.a23 := T.a13 * T.a21 - T.a11 * T.a23;   // C21 → adj[1][2]
    A.a31 := T.a21 * T.a32 - T.a22 * T.a31;   // C02 → adj[2][0]
    A.a32 := T.a12 * T.a31 - T.a11 * T.a32;   // C12 → adj[2][1]
    A.a33 := T.a11 * T.a22 - T.a12 * T.a21;   // C22
  end;

  // times: 矩阵乘法 this * other
  function Multiply(const A, B: TCnQRPerspectiveTransform): TCnQRPerspectiveTransform;
  begin
    Result.a11 := A.a11 * B.a11 + A.a12 * B.a21 + A.a13 * B.a31;
    Result.a12 := A.a11 * B.a12 + A.a12 * B.a22 + A.a13 * B.a32;
    Result.a13 := A.a11 * B.a13 + A.a12 * B.a23 + A.a13 * B.a33;
    Result.a21 := A.a21 * B.a11 + A.a22 * B.a21 + A.a23 * B.a31;
    Result.a22 := A.a21 * B.a12 + A.a22 * B.a22 + A.a23 * B.a32;
    Result.a23 := A.a21 * B.a13 + A.a22 * B.a23 + A.a23 * B.a33;
    Result.a31 := A.a31 * B.a11 + A.a32 * B.a21 + A.a33 * B.a31;
    Result.a32 := A.a31 * B.a12 + A.a32 * B.a22 + A.a33 * B.a32;
    Result.a33 := A.a31 * B.a13 + A.a32 * B.a23 + A.a33 * B.a33;
  end;

var
  SrcToSquare, SquareToDst: TCnQRPerspectiveTransform;
begin
  FillChar(SquareToDst, SizeOf(SquareToDst), 0);
  SquareToQuad(SrcPoints, SquareToDst);
  Adjoint(SquareToDst, SrcToSquare);

  FillChar(SquareToDst, SizeOf(SquareToDst), 0);
  SquareToQuad(DstPoints, SquareToDst);

  Result := Multiply(SquareToDst, SrcToSquare);
end;

// 对单点执行透视变换
function CnQRTransformPoint(const Transform: TCnQRPerspectiveTransform;
  X, Y: Single): TCnQRPointF;
var
  Denominator: Single;
begin
  Denominator := Transform.a31 * X + Transform.a32 * Y + Transform.a33;
  if Abs(Denominator) < 1E-10 then
    Denominator := 1E-10;
  Result.X := (Transform.a11 * X + Transform.a12 * Y + Transform.a13) / Denominator;
  Result.Y := (Transform.a21 * X + Transform.a22 * Y + Transform.a23) / Denominator;
end;

// 根据透视变换对二值矩阵进行网格采样
function CnQRSampleGrid(const ABinarized: TCnQRData;
  const Transform: TCnQRPerspectiveTransform;
  ADimension: Integer): TCnQRData;
var
  AWidth, AHeight: Integer;
  Col, Row: Integer;
  SrcPoint: TCnQRPointF;
  PixelX, PixelY: Integer;
begin
  AWidth := Length(ABinarized);
  AHeight := Length(ABinarized[0]);
  SetLength(Result, ADimension, ADimension);

  // Transform 已为 dst→src 映射，直接应用获取源图像坐标
  for Row := 0 to ADimension - 1 do
  begin
    for Col := 0 to ADimension - 1 do
    begin
      SrcPoint := CnQRTransformPoint(Transform, Col + 0.5, Row + 0.5);
      PixelX := Round(SrcPoint.X);
      PixelY := Round(SrcPoint.Y);

      if (PixelX >= 0) and (PixelX < AWidth) and
         (PixelY >= 0) and (PixelY < AHeight) then
        Result[Col, Row] := ABinarized[PixelX, PixelY]
      else
        Result[Col, Row] := 0;
    end;
  end;
end;

// 估算模块尺寸（像素）
function CnQRCalcModuleSize(const TopLeft, TopRight,
  BottomLeft: TCnQRFinderPattern): Double;
begin
  Result := (CnQRDistance(TopLeft, TopRight) / 14.0 +
    CnQRDistance(TopLeft, BottomLeft) / 14.0) / 2.0;
end;

// 估算二维码矩阵维度
function CnQRCalcDimension(const TopLeft, TopRight,
  BottomLeft: TCnQRFinderPattern; ModuleSize: Double): Integer;
var
  Dim1, Dim2: Integer;
begin
  Dim1 := Round(CnQRDistance(TopLeft, TopRight) / ModuleSize + 7);
  Dim2 := Round(CnQRDistance(TopLeft, BottomLeft) / ModuleSize + 7);
  Result := (Dim1 + Dim2) div 2;
  // 调整为满足 (Dim - 1) mod 4 = 0 的合法值
  Result := ((Result - 1) div 4) * 4 + 1;
  if Result < 21 then
    Result := 21;
end;

// 从灰度图像端到端解码二维码文本。内部串联二值化→检测→采样→矩阵解码
function CnQRDecodeFromGrayImage(const AGrayImage: TCnQRData): string;
var
  Binarized: TCnQRData;
  Width, Height, Dimension: Integer;
  TopLeft, TopRight, BottomLeft: TCnQRFinderPattern;
  AlignmentPattern: TCnQRAlignmentPattern;
  ModuleSize: Double;
  SrcPoints: array[0..3] of TCnQRPointF;
  DstPoints: array[0..3] of TCnQRPointF;
  Transform: TCnQRPerspectiveTransform;
  QRData: TCnQRData;
  HasAlignment: Boolean;
  I, DimCand: Integer;
  ModCand1, ModCand2, MinFrac: Double;
begin
  Width := Length(AGrayImage);
  Height := Length(AGrayImage[0]);
  if (Width < 21) or (Height < 21) then
    raise ECnQRCodeException.Create(SCnErrorQRImageTooSmallMin21X21);

  // Step 1: 二值化
  Binarized := CnQRBinarize(AGrayImage);

  try
    // Step 2: 寻像图案定位
    if not CnQRFindFinderPatterns(Binarized, TopLeft, TopRight, BottomLeft) then
      raise ECnQRCodeException.Create(SCnErrorQRNoFinderPatternsFound);

    // Step 3: 枚举合法维度(21,25,29,33,37)找最佳匹配
    // 中心间距(像素) / (维度-7) = 模块尺寸(像素)
    ModuleSize := CnQRDistance(TopLeft, TopRight) / 14.0;
    Dimension := 21;
    MinFrac := 999;
    for I := 0 to 4 do
    begin
      DimCand := 21 + I * 4;
      ModCand1 := CnQRDistance(TopLeft, TopRight) / (DimCand - 7);
      ModCand2 := CnQRDistance(TopLeft, BottomLeft) / (DimCand - 7);
      if Abs(Frac(ModCand1)) + Abs(Frac(ModCand2)) < MinFrac then
      begin
        MinFrac := Abs(Frac(ModCand1)) + Abs(Frac(ModCand2));
        ModuleSize := (ModCand1 + ModCand2) / 2.0;
        Dimension := DimCand;
      end;
    end;
    if ModuleSize < 1.0 then
      raise ECnQRCodeException.Create(SCnErrorQRModuleSizeTooSmall);

    // Step 4: 对齐图案定位
    HasAlignment := CnQRFindAlignmentPattern(Binarized, Width, Height,
      TopLeft, TopRight, BottomLeft, AlignmentPattern);

    // Step 5: 构建源四点
    SrcPoints[0].X := TopLeft.X;
    SrcPoints[0].Y := TopLeft.Y;
    SrcPoints[1].X := TopRight.X;
    SrcPoints[1].Y := TopRight.Y;
    SrcPoints[2].X := BottomLeft.X;
    SrcPoints[2].Y := BottomLeft.Y;
    if HasAlignment then
    begin
      SrcPoints[3].X := AlignmentPattern.X;
      SrcPoints[3].Y := AlignmentPattern.Y;
    end
    else
    begin
      // 无对齐图案时估算右下角
      SrcPoints[3].X := TopRight.X - TopLeft.X + BottomLeft.X;
      SrcPoints[3].Y := TopRight.Y - TopLeft.Y + BottomLeft.Y;
    end;

    // 目标四点：寻像图案中心在模块(3.5, 3.5)，TR在(Dim-3.5, 3.5)
    DstPoints[0].X := 3.5;
    DstPoints[0].Y := 3.5;
    DstPoints[1].X := Dimension - 3.5;
    DstPoints[1].Y := 3.5;
    DstPoints[2].X := 3.5;
    DstPoints[2].Y := Dimension - 3.5;
    DstPoints[3].X := Dimension - 3.5;
    DstPoints[3].Y := Dimension - 3.5;

    // Step 6: 透视变换
    Transform := CnQRCalcPerspectiveTransform(DstPoints, SrcPoints);

    // Step 7: 网格采样
    QRData := CnQRSampleGrid(Binarized, Transform, Dimension);

    // Step 8: 矩阵解码
    Result := CnQRDecodeFromMatrix(QRData);
  finally
    SetLength(Binarized, 0);
    SetLength(QRData, 0);
  end;
end;

end.

