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
* 备    注：本单元实现了二维码编码功能，可配合 CnQRImage 实现绘制。

*           二维码编码内部位操作使用 CnBits，但大部分是 MSB First 模式，
*           也即符合阅读习惯的高位在前，和 CnBits 里大部分底位在前不同。
*           阅读代码时需注意。
*
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.01.13 V1.0
*               创建单元，在 AI 帮助下实现编码并能扫描成功
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls, CnBits, CnNative;

type
  TCnErrorRecoveryLevel = (erlL, erlM, erlQ, erlH);
  {* 二维码纠错等级，分别代表 7%、15%、25%、30%}

  TCnQRCodeVersion = 1..40;
  {* 二维码版本}

  TCnQRData = array of array of Byte;
  {* 二维矩阵数据：[列, 行] 也就是 [X(Left), Y(Top)]。左上角为 [0, 0]}

  TCnEncodeMode = (emNumeric, emAlphaNumeric, emByte, emWideChar);
  {* 二维码内容模式}

  TCnQRGFPoly = record
  {* 二维码中使用的 Reed-Solomon 多项式结构}
    Num: Integer;
    Coeff: array of Byte;
  end;

  TCnQRWideCharMode = (cqwUtf8, cqwAnsi);
  {* 宽字符的编码模式，默认 Utf8。后者直接转 Ansi，如是汉字则是 GB18030 编码}

  TCnQRCodeInfo = record
  {* 二维码基本信息}
    Mode: TCnEncodeMode;
    CharCount: Integer;
    DataBits: TCnBitBuilder;
    ECCodewords: Integer;
    DataCodewords: Integer;
    TotalCodewords: Integer;
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
  protected
    procedure ClearData;
    procedure PaintPositionDetectionPattern;
    {* 绘制位置探测图形}
    procedure PaintAlignmentPattern;
    {* 绘制对齐图形}
    procedure PaintTimingPattern;
    {* 绘制时序图形}
    procedure PaintFormatInformation;
    {* 绘制格式信息}
    procedure PaintVersionInformation;
    {* 绘制版本信息}
    procedure PaintMaskCoding;
    {* 计算并应用最佳 Mask 掩码}

    // 数据编码相关方法
    function AnalyzeRawText: TCnEncodeMode;
    function EncodeNumeric(const AText: AnsiString): TCnBitBuilder;
    function EncodeAlphaNumeric(const AText: AnsiString): TCnBitBuilder;
    function EncodeByte(const AText: AnsiString): TCnBitBuilder;
    function GetCharCountBits(Mode: TCnEncodeMode; Version: TCnQRCodeVersion): Integer;
    procedure EncodeText;

    // Reed-Solomon 纠错码生成
    function GenerateECCodewords(Data: TCnBitBuilder; ECCodewords: Integer):
      TCnBitBuilder;
    function PolyMult(A, B: TCnQRGFPoly): TCnQRGFPoly;
    function PolyMod(Dividend, Divisor: TCnQRGFPoly): TCnQRGFPoly;
    function GetGeneratorPoly(Num: Integer): TCnQRGFPoly;

    // 数据放置和掩码
    procedure PlaceDataBits;
    procedure ApplyMask(MaskType: Integer);
    function EvaluateMask(MaskType: Integer): Integer;
    function GetMaskPattern(X, Y, MaskType: Integer): Boolean;

    // BCH 编码
    function BCHEncode15(Data: Integer): Integer;
    function BCHEncode18(Data: Integer): Integer;
    function GetFormatBits(ErrorLevel: TCnErrorRecoveryLevel; MaskType: Integer): Integer;
    function GetVersionBits(Version: TCnQRCodeVersion): Integer;

    // 版本相关参数查询
    function GetTotalCodewords(Version: TCnQRCodeVersion): Integer;
    function GetECCodewords(Version: TCnQRCodeVersion; ErrorLevel:
      TCnErrorRecoveryLevel): Integer;
    function GetOptimalVersion(const AText: AnsiString; ErrorLevel:
      TCnErrorRecoveryLevel): TCnQRCodeVersion;

    // 功能区域判断
    function IsFunctionArea(X, Y: Integer): Boolean;

    function AddEccAndInterleave(Data: TCnBitBuilder): TCnBitBuilder;
    function ComputeBlockECC(const DataBytes: TBytes; ECCodewords: Integer): TBytes;
    function BitBuilderToBytes(B: TCnBitBuilder; ByteCount: Integer): TBytes;

    procedure PaintData;
    function EvaluateMaskPenalty(MaskType: Integer): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // 调试相关输出
    function GetDataCodewordsBytes: TBytes;
    function GetAllCodewordsBytes: TBytes;
    function GetFormatBitsValue: Integer;
    function GetVersionBitsValue: Integer;
    function DumpMatrix: string;
    function DumpMatrixUnmasked: string;
    function DumpFunctionArea: string;
    function DumpFormatRowInfo: string;
    function DumpFormatColInfo: string;
    function DumpAlignmentCenters: string;
    function DumpFormatRowIndices: string;
    function DumpFormatColIndices: string;

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
    {* 生成的二维码数据}
    property MaskType: Integer read FMaskType;
    {* 掩码类型}
  end;

implementation

uses
  CnWideStrings;

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
function TCnQREncoder.AnalyzeRawText: TCnEncodeMode;
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

function TCnQREncoder.GetCharCountBits(Mode: TCnEncodeMode; Version:
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
  Mode: TCnEncodeMode;
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
      raise Exception.Create('Data too long for QR Code Version 40');

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

// Reed-Solomon 纠错码
function TCnQREncoder.GenerateECCodewords(Data: TCnBitBuilder; ECCodewords:
  Integer): TCnBitBuilder;
var
  I, J: Integer;
  DataPoly, GeneratorPoly, MessagePoly: TCnQRGFPoly;
  DataBytes: TBytes;
begin
  Result := TCnBitBuilder.Create;
  SetLength(DataBytes, Data.BitLength div 8);

  // 将位转换为字节
  for I := 0 to High(DataBytes) do
  begin
    DataBytes[I] := 0;
    for J := 0 to 7 do
    begin
      if Data.Bit[I * 8 + J] then
        DataBytes[I] := DataBytes[I] or (1 shl (7 - J));
    end;
  end;

  // 创建消息多项式
  SetLength(MessagePoly.Coeff, Length(DataBytes) + ECCodewords);
  MessagePoly.Num := Length(DataBytes) + ECCodewords - 1;
  for I := 0 to High(DataBytes) do
    MessagePoly.Coeff[I] := DataBytes[I];
  for I := Length(DataBytes) to High(MessagePoly.Coeff) do
    MessagePoly.Coeff[I] := 0;

  // 获取生成器多项式
  GeneratorPoly := GetGeneratorPoly(ECCodewords);

  // 计算纠错码
  DataPoly := PolyMod(MessagePoly, GeneratorPoly);

  // 转换为位流
  Result.BitLength := ECCodewords * 8;
  for I := 0 to ECCodewords - 1 do
  begin
    for J := 0 to 7 do
    begin
      Result.Bit[I * 8 + J] :=
        (DataPoly.Coeff[Length(DataPoly.Coeff) - ECCodewords + I] and (1 shl (7 - J))) <> 0;
    end;
  end;
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
  Mode: TCnEncodeMode;
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

function TCnQREncoder.EvaluateMaskPenalty(MaskType: Integer): Integer;
begin
  Result := EvaluateMask(MaskType);
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
  Result := Format('Row y=8: first=%d, count=%d, size=%d, threshold(size-8)=%d',
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
  Result := Format('Col x=8: first=%d, count=%d, size=%d, threshold(size-8)=%d',
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

end.

