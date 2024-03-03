{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnPDF;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：PDF 简易解析生成单元
* 单元作者：刘啸
* 备    注：简单的 PDF 格式处理单元
*           解析：先线性进行词法分析，再解析出多个对象，再将对象整理成树
*           生成：先构造固定的对象树，补充内容后写入流
*
*           封装了 CnJpegFilesToPDF 过程，将多个 JPEG 文件拼成一个 PDF 输出
*           也实现了 TCnImagesToPDFCreator 以在输出 JPEG 的 PDF 时支持页面边距等设置
*
*           文件尾的 Trailer 的 Root 指向 Catalog 对象，大体的树结构如下：
*
*           Catalog -> Pages -> Page1 -> Resource
*                   |        |      | -> Content
*                   |        |      | -> Thunbnail Image
*                   |        |      | -> Annoation
*                   |        -> Page2 ...
*                   |
*                   -> Outline Hierarchy -> Outline Entry
*                   |                  | -> Outline Entry
*                   |
*                   -> Artical Threads -> Thread
*                   |                | -> Thread
*                   -> Named Destination
*                   -> Interactive Form
*
*           压缩测试：
*               外部 PDF，2007 或以下解，2009 或以上解，目前看起来均正常
*               2007 或以下生成的 PDF，外部 Reader 打开正常，2007 或以下解正常，2009 或以上解正常
*               2009 或以上生成的 PDF，外部 Reader 打开正常，2007 或以下解正常，2009 或以上解正常
*
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.03.03 V1.4
*               PDF 文件的载入与保存初步支持权限密码与用户密码，内部使用 128RC4/128AES 加密，其他途径暂不支持
*           2024.02.22 V1.3
*               实现 TCnImagesToPDFCreator 以在输出 JPEG 的 PDF 时支持页面边距等设置
*               增加从 PDF 中抽取 JPEG 文件的方法
*           2024.02.012 V1.3
*               TCnPDFDocument 能够初步分析并输出逻辑结构
*               实现 CnJpegFilesToPDF 过程，将多个 JPEG 文件拼成一个 PDF 输出
*           2024.02.10 V1.2
*               基本完成四个部分的对象与结构分析，待组织逻辑结构
*           2024.02.06 V1.1
*               基本完成词法分析，待组织语法树
*           2024.01.28 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, TypInfo, jpeg, CnNative, CnStrings, CnPDFCrypt;

const
  CN_PDF_PERMISSION_PRINT       = 1 shl 2;   // 常规打印
  CN_PDF_PERMISSION_MODIFY      = 1 shl 3;   // 修改内容
  CN_PDF_PERMISSION_COPY        = 1 shl 4;   // 复制内容
  CN_PDF_PERMISSION_ANNOTATIONS = 1 shl 5;   // 修改标记
  CN_PDF_PERMISSION_INTERACTIVE = 1 shl 8;   // 填写表单
  CN_PDF_PERMISSION_EXTRACT     = 1 shl 9;   // 抽取元素
  CN_PDF_PERMISSION_ASSEMBLE    = 1 shl 10;  // 重编文档
  CN_PDF_PERMISSION_PRINTHI     = 1 shl 11;  // 精细打印

type
  ECnPDFException = class(Exception);
  {* PDF 异常}

  ECnPDFEofException = class(Exception);
  {* 解析 PDF 时碰到内容尾}

//==============================================================================
// 以下是 PDF 文件中各种对象类的声明及继承关系
//
//  TCnPDFObject 派生出
//    简单：TCnPDFNumberObject、TCnPDFNameObject、TCnPDFBooleanObject、
//          TCnPDFNullObject、TCnPDFStringObject、TCnPDFReferenceObject
//    复合：TCnPDFArrayObject，线性包含多个 TCnPDFObject
//          TCnPDFDictionaryObject，包含多个 TCnPDFNameObject 与 TCnPDFObject 对
//          TCnPDFStreamObject，包含一个 TCnPDFDictionaryObject 与一片二进制数据
//
//==============================================================================

  TCnPDFXRefType = (xrtNormal, xrtDeleted, xrtFree);
  {* 对象的交叉引用类型：自由无引用、正常引用、已删除}

  TCnPDFObject = class(TPersistent)
  {* PDF 文件中的对象基类}
  private
    FID: Cardinal;
    FGeneration: Cardinal;
    FXRefType: TCnPDFXRefType;
    FOffset: Integer;
    FParent: TCnPDFObject;
  protected
    function CheckWriteObjectStart(Stream: TStream): Cardinal;
    function CheckWriteObjectEnd(Stream: TStream): Cardinal;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ELSE} virtual; {$ENDIF}
    {* 输出成单行字符串}
    procedure ToStrings(Strings: TStrings; Indent: Integer = 0); virtual;
    {* 输出成多行字符串，默认添加单行。实际主要用于 Array 或 Dictionary 等子类}

    function WriteToStream(Stream: TStream): Cardinal; virtual; abstract;

    function Clone: TCnPDFObject;
    {* 创建一个新对象并复制内容}

    property ID: Cardinal read FID write FID;
    {* 对象 ID，如为 0，写入时不写前后缀}
    property Generation: Cardinal read FGeneration write FGeneration;
    {* 对象的代数，一般为 0}
    property XRefType: TCnPDFXRefType read FXRefType write FXRefType;
    {* 对象交叉引用类型，一般为 normal}
    property Offset: Integer read FOffset write FOffset;
    {* 内容中的偏移量，解析而来}

    property Parent: TCnPDFObject read FParent write FParent;
    {* 父节点的引用。譬如数组元素的数组，以及字典的名称、值的字典，顶层对象可为 nil}
  end;

  TCnPDFObjectClass = class of TCnPDFObject;

  TCnPDFSimpleObject = class(TCnPDFObject)
  {* 简单的 PDF 文件对象基类，有一段简单内容，可按格式输出}
  private

  protected
    FContent: TBytes;
  public
    constructor Create(const AContent: AnsiString); reintroduce; overload;
    {* 从一简单内容创建对象}
    constructor Create(const Data: TBytes); reintroduce; overload;
    {* 从一简单内容创建对象}

    procedure Assign(Source: TPersistent); override;
    {* 赋值方法}

    function ToString: string; override;

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 简单对象，默认照原样输出}

    property Content: TBytes read FContent write FContent;
    {* 不包括包装格式前后缀的具体内容}
  end;

  TCnPDFNumberObject = class(TCnPDFSimpleObject)
  {* PDF 文件中的数字对象类}
  public
    constructor Create(Num: Integer); reintroduce; overload;
    constructor Create(Num: Int64); reintroduce; overload;
    constructor Create(Num: Extended); reintroduce; overload;

    function AsInteger: Integer;
    function AsFloat: Extended;

    procedure SetInteger(Value: Integer);
    procedure SetFloat(Value: Extended);
  end;

  TCnPDFNameObject = class(TCnPDFSimpleObject)
  private
    function GetName: AnsiString;
  {* PDF 文件中的名字对象类}
  public
    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 输出斜杠加名字}

    property Name: AnsiString read GetName;
  end;

  TCnPDFBooleanObject = class(TCnPDFSimpleObject)
  {* PDF 文件中的布尔对象类}
  public
    constructor Create(IsTrue: Boolean); reintroduce;
  end;

  TCnPDFNullObject = class(TCnPDFSimpleObject)
  {* PDF 文件中的空对象类}
  public
    constructor Create; reintroduce;
  end;

  TCnPDFStringObject = class(TCnPDFSimpleObject)
  {* PDF 文件中的字符串对象类}
  private
    FIsHex: Boolean;
  public
    constructor Create(const AnsiStr: AnsiString); overload;
{$IFDEF COMPILER5}
    constructor CreateW(const WideStr: WideString); // D5 不让 overload
{$ELSE}
    constructor Create(const WideStr: WideString); overload;
{$ENDIF}
{$IFDEF UNICODE}
    constructor Create(const UnicodeStr: string); overload;
{$ENDIF}

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 输出一对小括号加上其内的字符串}

    function AsString: string;
    {* 转换为 string}
    property IsHex: Boolean read FIsHex write FIsHex;
    {* 内容是否是十六进制输出}

    procedure AddEscape;
    {* 增加转义，注意不要重复调用}
    procedure RemoveEscape;
    {* 去除转义}
  end;

  TCnPDFReferenceObject = class(TCnPDFSimpleObject)
  {* PDF 文件中的引用对象类}
  private
    FReference: TCnPDFObject;
    procedure SetReference(const Value: TCnPDFObject);
  public
    constructor Create(Obj: TCnPDFObject); reintroduce;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    {* 赋值方法}

    function ToString: string; override;

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 输出数字 数字 R}

    function IsReference(Obj: TCnPDFObject): Boolean;
    {* 判断自己是否是指定外部对象的引用，通过比较 ID Generation 等参数判断}

    property Reference: TCnPDFObject read FReference write SetReference;
    {* 引用的对象}
  end;

  TCnPDFDictionaryObject = class;

  TCnPDFDictPair = class(TPersistent)
  {* PDF 文件中的字典对象类中的名字对象对，持有名字与值两个对象，不具有 Parent 机制}
  private
    FName: TCnPDFNameObject;
    FValue: TCnPDFObject;
    FDictionary: TCnPDFDictionaryObject;
    procedure SetValue(const Value: TCnPDFObject);
  public
    constructor Create(const Name: string; Dict: TCnPDFDictionaryObject); virtual;
    {* 构造函数，会创建名称对象并设置其 Parent 为指定字典对象}
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    {* 赋值方法}

    procedure ChangeToArray;
    {* 当 Value 是简单对象或 nil 时，转换 Value 成数组对象，并将旧 Value 设为其第一个元素}

    function WriteToStream(Stream: TStream): Cardinal;
    {* 输出名字 值}

    property Dictionary: TCnPDFDictionaryObject read FDictionary;
    {* 所属字典的引用}
    property Name: TCnPDFNameObject read FName;
    {* 名字对象}
    property Value: TCnPDFObject read FValue write SetValue;
    {* 值对象，可由外界设置，自身析构时释放。注意平时 SetValue 时不会释放旧值}
  end;

  TCnPDFArrayObject = class(TCnPDFObject)
  {* PDF 文件中的数组对象类，持有数组内的元素对象}
  private
    FElements: TObjectList;
    function GetItem(Index: Integer): TCnPDFObject;
    procedure SetItem(Index: Integer; const Value: TCnPDFObject);
    function GetCount: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    {* 赋值方法}

    procedure Clear;
    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 输出[及每个对象及]}

    function ToString: string; override;
    procedure ToStrings(Strings: TStrings; Indent: Integer = 0); override;

    procedure AddObject(Obj: TCnPDFObject);
    {* 添加一个对象，外部请勿释放此对象}
    procedure AddNumber(Value: Integer); overload;
    procedure AddNumber(Value: Int64); overload;
    procedure AddNumber(Value: Extended); overload;
    procedure AddNull;
    procedure AddTrue;
    procedure AddFalse;
    procedure AddObjectRef(Obj: TCnPDFObject);
    procedure AddAnsiString(const Value: AnsiString);
    procedure AddWideString(const Value: WideString);
{$IFDEF UNICODE}
    procedure AddUnicodeString(const Value: string);
{$ENDIF}

    function HasObjectRef(Obj: TCnPDFObject): Boolean;
    {* 是否存在一对象的引用}

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCnPDFObject read GetItem write SetItem;
    {* 序号引用其元素}
  end;

  TCnPDFDictionaryObject = class(TCnPDFObject)
  {* PDF 文件中的字典对象类，持有内部 Pair}
  private
    FPairs: TObjectList;
    function GetValue(const Name: string): TCnPDFObject;
    procedure SetValue(const Name: string; const Value: TCnPDFObject);
    function GetCount: Integer;
    function GetPair(Index: Integer): TCnPDFDictPair;
  protected
    function IndexOfName(const Name: string): Integer;
    procedure AddPair(APair: TCnPDFDictPair);
    {* 添加一个外部创建的 Pair，注意 Pair 的 Dictionary 需提前设置好属于本实例}

    function WriteDictionary(Stream: TStream): Cardinal;

    property Pairs[Index: Integer]: TCnPDFDictPair read GetPair;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    {* 赋值方法}

    procedure Clear;
    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 输出<<及每个Pair及>>}

    function ToString: string; override;
    procedure ToStrings(Strings: TStrings; Indent: Integer = 0); override;

    function AddName(const Name: string): TCnPDFDictPair; overload;
    {* 添加一个名称，值由外界赋值，赋值后外部请勿释放此对象}
    function AddName(const Name1, Name2: string): TCnPDFDictPair; overload;
    {* 添加两个名称分别作为名称与值}

    function AddArray(const Name: string): TCnPDFArrayObject;
    {* 添加一个命名的空数组，注意返回的是数组对象本身}
    function AddDictionary(const Name: string): TCnPDFDictionaryObject;
    {* 添加一个命名的空字典，注意返回的是字典对象本身}

    function AddNumber(const Name: string; Value: Integer): TCnPDFDictPair; overload;
    function AddNumber(const Name: string; Value: Int64): TCnPDFDictPair; overload;
    function AddNumber(const Name: string; Value: Extended): TCnPDFDictPair; overload;
    function AddNull(const Name: string): TCnPDFDictPair;
    function AddTrue(const Name: string): TCnPDFDictPair;
    function AddFalse(const Name: string): TCnPDFDictPair;
    function AddObjectRef(const Name: string; Obj: TCnPDFObject): TCnPDFDictPair;
    function AddString(const Name: string; const Value: string): TCnPDFDictPair;
    function AddAnsiString(const Name: string; const Value: AnsiString): TCnPDFDictPair;
    function AddWideString(const Name: string; const Value: WideString): TCnPDFDictPair;
{$IFDEF UNICODE}
    function AddUnicodeString(const Name: string; const Value: string): TCnPDFDictPair;
{$ENDIF}

    procedure DeleteName(const Name: string);
    {* 删除指定名字及对应 Value 并释放}
    function HasName(const Name: string): Boolean;
    {* 是否有指定名称存在}
    procedure GetNames(Names: TStrings);
    {* 将所有名字塞 Names 里}
    function GetType: string;
    {* 封装的常用的获取名称是 'Type' 的名字的字符串值}
    property Count: Integer read GetCount;
    {* 字典内的元素数量}
    property Values[const Name: string]: TCnPDFObject read GetValue write SetValue; default;
    {* 根据名字引用对象}
  end;

  TCnPDFStreamObject = class(TCnPDFDictionaryObject)
  {* PDF 文件中的流对象类，据说包含一字典一流}
  private
    FStream: TBytes;
    FSupportCompress: Boolean;
  protected
    procedure SyncLength;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetJpegImage(const JpegFileName: string);
    {* 将一 JPEG 格式的文件放入本对象}
    procedure SetJpegStream(JpegStream: TStream);
    {* 将一 JPEG 格式的流放入本对象，注意流的 Position 须是 0}

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 输出 stream 及流及 endstream}

    procedure ExtractStream(OutStream: TStream);

    procedure SetStrings(Strings: TStrings);
    {* 将指定 Strings 中的内容赋值给流}

{$IFNDEF NO_ZLIB}
    procedure Compress;
    {* 将 FStream 明文内容压缩成标准 Zip 格式重新放入 FStream，请勿对其他已知编码的内容调用此压缩方法
      注意似乎无论 Delphi 版本高低也就是无论是否定义 SUPPORT_ZLIB_WINDOWBITS
      压缩出的流都能被 Acrobat Reader 解析从而正确显示内容}

    procedure Uncompress;
    {* 将 FStream 中的标准 Zip 内容解压缩成明文重新放入 FStream
      注意如果 Delphi 版本过低，内部解压时可能会出异常，暂无好办法}
{$ENDIF}

    function ToString: string; override;
    procedure ToStrings(Strings: TStrings; Indent: Integer = 0); override;

    property SupportCompress: Boolean read FSupportCompress write FSupportCompress;
    {* 是否支持压缩。创建时由外界指定，解析 PDF 时根据字典内容的 Filter 是否 FlateDecode 指定}
    property Stream: TBytes read FStream write FStream;
    {* 包含的原始流内容，刚从 PDF 中解析出来时可能是压缩的}
  end;

  TCnPDFObjectManager = class(TObjectList)
  {* PDFDocument 类内部使用的管理每个独立对象的总类}
  private
    FMaxID: Cardinal;
    function GetItem(Index: Integer): TCnPDFObject;
    procedure SetItem(Index: Integer; const Value: TCnPDFObject);
  public
    constructor Create;

    procedure CalcMaxID;
    {* 批量读入对象后，遍历统计出其最大 ID}
    procedure CopyTo(DestObjects: TObjectList);
    {* 把内容的引用都复制到另一列表中}

    function AddRaw(AObject: TCnPDFObject): Integer;
    {* 增加一外部对象供管理，内部不处理 ID，用于解析}

    function GetObjectByIDGeneration(ObjID: Cardinal;
      ObjGeneration: Cardinal = 0): TCnPDFObject;
    {* 根据 ID 和代数查找对象}

    function Add(AObject: TCnPDFObject): Integer; reintroduce;
    {* 增加一外部对象供管理，内部会重设其 ID}

    property Items[Index: Integer]: TCnPDFObject read GetItem write SetItem; default;
    {* 持有的 PDF 对象条目}
    property MaxID: Cardinal read FMaxID;
    {* 对象中的最大 ID}
  end;

  TCnPDFPartBase = class
  public
    function WriteToStream(Stream: TStream): Cardinal; virtual; abstract;
    procedure DumpToStrings(Strings: TStrings; Verbose: Boolean = False;
      Indent: Integer = 0); virtual; abstract;
    {* 输出信息，Verbose 指示详细与否，内容少时可不处理。
      Indent 是多行信息在 Verbose 为 True 时的缩进}
  end;

  TCnPDFHeader = class(TCnPDFPartBase)
  {* PDF 文件头的解析与生成}
  private
    FVersion: string;
    FComment: string;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 将内容输出至流}
    procedure DumpToStrings(Strings: TStrings; Verbose: Boolean = False; Indent: Integer = 0); override;
    {* 输出概要总结信息供调试}

    property Version: string read FVersion write FVersion;
    {* 字符串形式的版本号，如 1.7 等}
    property Comment: string read FComment write FComment;
    {* 一段单行注释，用一些复杂字符}
  end;

  TCnPDFXRefItem = class(TCollectionItem)
  {* PDF 文件里的交叉引用表的条目，多个条目属于一个段}
  private
    FObjectGeneration: Cardinal;
    FObjectXRefType: TCnPDFXRefType;
    FObjectOffset: Cardinal;
  public
    property ObjectGeneration: Cardinal read FObjectGeneration write FObjectGeneration;
    {* 对象代数}
    property ObjectXRefType: TCnPDFXRefType read FObjectXRefType write FObjectXRefType;
    {* 对象引用类型}
    property ObjectOffset: Cardinal read FObjectOffset write FObjectOffset;
    {* 对象在文件中的偏移量}
  end;

  TCnPDFXRefCollection = class(TCollection)
  {* PDF 文件里的交叉引用表中的一个段的解析与生成，包含多个条目}
  private
    FObjectIndex: Cardinal;
    function GetItem(Index: Integer): TCnPDFXRefItem;
    procedure SetItem(Index: Integer; const Value: TCnPDFXRefItem);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function WriteToStream(Stream: TStream): Cardinal;
    {* 将内容输出至流}

    function Add: TCnPDFXRefItem;
    {* 添加一个空交叉引用条目}

    property ObjectIndex: Cardinal read FObjectIndex write FObjectIndex;
    {* 本段内的对象起始编号}
    property Items[Index: Integer]: TCnPDFXRefItem read GetItem write SetItem;
    {* 本段的连续对象数}
  end;

  TCnPDFXRefTable = class(TCnPDFPartBase)
  {* PDF 文件中的交叉引用表的解析与生成，包括一个或多个段}
  private
    FSegments: TObjectList;
    function GetSegmenet(Index: Integer): TCnPDFXRefCollection;
    function GetSegmentCount: Integer;
    procedure SetSegment(Index: Integer;
      const Value: TCnPDFXRefCollection);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 将内容输出至流}
     procedure DumpToStrings(Strings: TStrings; Verbose: Boolean = False; Indent: Integer = 0); override;
    {* 输出概要总结信息供调试}

    function FindByObject(Obj: TCnPDFObject): TCnPDFXRefItem;
    {* 在各段以及各表中搜索指定 Obj 的 ID 与 Generation 对应的引用条目}

    function AddSegment: TCnPDFXRefCollection;
    {* 增加一个空段}

    property SegmentCount: Integer read GetSegmentCount;
    {* 交叉引用表中的段数}
    property Segments[Index: Integer]: TCnPDFXRefCollection read GetSegmenet write SetSegment;
    {* 交叉引用表中的每一段}
  end;

  TCnPDFTrailer = class(TCnPDFPartBase)
  {* PDF 文件尾的解析与生成}
  private
    FDictionary: TCnPDFDictionaryObject;
    FXRefStart: Cardinal;
    FComment: string;
  protected
    procedure GenerateID;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 将内容输出至流}
     procedure DumpToStrings(Strings: TStrings; Verbose: Boolean = False; Indent: Integer = 0); override;
    {* 输出概要总结信息供调试}

    property Dictionary: TCnPDFDictionaryObject read FDictionary;
    {* 文件尾的字典，包括 Size、Root、Info 等关键信息}

    property XRefStart: Cardinal read FXRefStart write FXRefStart;
    {* 交叉引用表的起始字节偏移量，可以指向 xref 原始表，也可用是一个 类型是 XRef 的 Object，里头有流式内容}
    property Comment: string read FComment write FComment;
    {* 最后一块注释}
  end;

  TCnPDFBody = class(TCnPDFPartBase)
  {* PDF 内容组织类}
  private
    FObjects: TCnPDFObjectManager;     // 所有对象都在这里管辖，其余都是引用
    FPages: TCnPDFDictionaryObject;    // 页面树对象
    FCatalog: TCnPDFDictionaryObject;  // 根目录对象，供 Trailer 中引用
    FInfo: TCnPDFDictionaryObject;     // 信息对象，供 Trailer 中引用
    FEncrypt: TCnPDFDictionaryObject;  // 加密对象，供 Trailer 中引用
    FXRefTable: TCnPDFXRefTable;       // 交叉引用表的引用
    function GetPage(Index: Integer): TCnPDFDictionaryObject;
    function GetPageCount: Integer;
    function GetContent(Index: Integer): TCnPDFStreamObject;
    function GetContentCount: Integer;
    function GetResource(Index: Integer): TCnPDFDictionaryObject;
    function GetResourceCount: Integer;
  protected
    FPageList: TObjectList;            // 页面对象列表
    FResourceList: TObjectList;        // 页面对象的资源列表，先都塞一块，一般是 Dictionary
    FContentList: TObjectList;         // 页面对象的内容列表，先都塞一块，一般是 Stream

    procedure SyncPages;
    {* 将页面内容引用赋值给 Pages 的 Kids}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SortObjectRefs(PDFObjects: TObjectList);
    {* 将外部新索引的对象表按对象编号排序，注意自身的原始顺序不变}

    procedure CreateResources;
    procedure CreateEncrypt;

    function WriteToStream(Stream: TStream): Cardinal; override;
    {* 将内容输出至流}
     procedure DumpToStrings(Strings: TStrings; Verbose: Boolean = False; Indent: Integer = 0); override;
    {* 输出概要总结信息供调试}

    procedure AddObject(Obj: TCnPDFObject);
    {* 让外界添加创建好的对象并交给本类管理，内部会替该对象生成有效 ID}
    property Objects: TCnPDFObjectManager read FObjects;
    {* 所有对象供访问}

    property XRefTable: TCnPDFXRefTable read FXRefTable write FXRefTable;
    {* 交叉引用表的引用，供写入各个对象的偏移等}

    // 以下并非都必须
    property Info: TCnPDFDictionaryObject read FInfo write FInfo;
    {* 信息对象，类型为字典}

    property Catalog: TCnPDFDictionaryObject read FCatalog write FCatalog;
    {* 根对象，类型为字典，其 /Pages 指向 Pages 对象}

    property Encrypt: TCnPDFDictionaryObject read FEncrypt write FEncrypt;
    {* 加密对象}

    property Pages: TCnPDFDictionaryObject read FPages write FPages;
    {* 页面列表，类型为字典，其 /Kids 指向各个页面}
    property PageCount: Integer read GetPageCount;
    {* 页面对象数量}
    property Page[Index: Integer]: TCnPDFDictionaryObject read GetPage;
    {* 多个页面对象，类型为字典，有 MediaBox（定义纸张大小）、Resources（字体资源等）、
      Parent（指向页面列表父节点），Contents（页面内容操作符）}

    property ContentCount: Integer read GetContentCount;
    {* 内容对象数量，暂不区分页面}
    property Content[Index: Integer]: TCnPDFStreamObject read GetContent;
    {* 多个内容对象，类型为字典或流}

    property ResourceCount: Integer read GetResourceCount;
    {* 资源对象数量，暂不区分页面}
    property Resource[Index: Integer]: TCnPDFDictionaryObject read GetResource;
    {* 多个资源对象，类型为字典}

    function AddPage: TCnPDFDictionaryObject;
    {* 增加一空页面并返回该页面}
    function AddResource(Page: TCnPDFDictionaryObject): TCnPDFDictionaryObject;
    {* 给某页增加一个 Resource，Page 的 /Resources 指向或包括此对象}
    function AddContent(Page: TCnPDFDictionaryObject): TCnPDFStreamObject;
    {* 给某页增加一个 Content，Page 的 /Contents 指向或包括此对象}

    procedure AddRawPage(APage: TCnPDFDictionaryObject);
    {* 增加一外部指定页面作为引用}
    procedure AddRawContent(AContent: TCnPDFStreamObject);
    {* 增加一外部指定内容作为引用}
    procedure AddRawResource(AResource: TCnPDFDictionaryObject);
    {* 增加一外部指定内容作为引用}
  end;

//==============================================================================
//
// 以下是 PDF 文件的结构，包含四个类
//
//==============================================================================

  TCnPDFParser = class;

  TCnPDFDocument = class
  private
    FHeader: TCnPDFHeader;
    FBody: TCnPDFBody;
    FXRefTable: TCnPDFXRefTable;
    FTrailer: TCnPDFTrailer;
    FEncrypted: Boolean;
    FDecrypted: Boolean;
    FPermission: Cardinal;
    FEncryptionMethod: TCnPDFEncryptionMethod;
    function FromReference(Ref: TCnPDFReferenceObject): TCnPDFObject;
    procedure GetCryptIDGen(Obj: TCnPDFObject; out AID, AGen: Cardinal);
    function GetNeedPassword: Boolean;
  protected
    procedure ReadTrailer(P: TCnPDFParser);
    procedure ReadTrailerStartXRef(P: TCnPDFParser);
    procedure ReadXRef(P: TCnPDFParser);

    procedure XRefDictToXRefTable(Dict: TCnPDFDictionaryObject);
    procedure ArrangeObjects;
    {* 读入所有对象后从 Root 等处重新整理}
    procedure UncompressObjects;
    {* 判断 Stream 内容并尽量解压缩}

    procedure SyncTrailer;

    function CheckUserPassword(const APass: AnsiString; out Key: TBytes): Boolean;
    {* 检查用户输入的密码是否是用户密码，是则返回 True，并 Key 里返回密钥}
    function CheckOwnerPassword(const APass: AnsiString; out Key: TBytes): Boolean;
    {* 检查用户输入的密码是否是权限密码，是则返回 True，并 Key 里返回密钥}

    procedure DecryptObject(Cryptor: TCnPDFDataCryptor; Obj: TCnPDFObject; Key: TBytes);
    procedure DecryptString(Cryptor: TCnPDFDataCryptor; Str: TCnPDFStringObject; Key: TBytes);
    procedure DecryptStream(Cryptor: TCnPDFDataCryptor; Stream: TCnPDFStreamObject; Key: TBytes);

    procedure EncryptObject(Cryptor: TCnPDFDataCryptor; Obj: TCnPDFObject; Key: TBytes);
    procedure EncryptString(Cryptor: TCnPDFDataCryptor; Str: TCnPDFStringObject; Key: TBytes);
    procedure EncryptStream(Cryptor: TCnPDFDataCryptor; Stream: TCnPDFStreamObject; Key: TBytes);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure DumpToStrings(Strings: TStrings);
    // 从 Parse 中读入内容，并让 P 跳出至内容的下一个 Token
    procedure ReadDictionary(P: TCnPDFParser; Dict: TCnPDFDictionaryObject);
    {* 读入一个字典，P 须指向 <<，运行后跳出 >>}
    procedure ReadArray(P: TCnPDFParser; AnArray: TCnPDFArrayObject);
    {* 读入一个数组，P 须指向 [，运行后跳出 ]}
    procedure ReadNumber(P: TCnPDFParser; Num: TCnPDFNumberObject; OverCRLF: Boolean = True);
    {* 读入一个数字，P 须指向 pttNumber，运行后跳出该 pttNumber
      该方法新增 OverCRLF 参数因交叉引用表中需要}
    procedure ReadReference(P: TCnPDFParser; Ref: TCnPDFReferenceObject);
    {* 读入一个引用，P 须指向 pttNumber pttNumber pttR，运行后跳出该 pttR}
    procedure ReadName(P: TCnPDFParser; Name: TCnPDFNameObject);
    {* 读入一个名称，P 须指向 pttName，运行后跳出该 pttName}
    procedure ReadString(P: TCnPDFParser; Str: TCnPDFStringObject);
    {* 读入一个字符串，P 须指向 (，运行后跳出 ) }
    procedure ReadHexString(P: TCnPDFParser; Str: TCnPDFStringObject);
    {* 读入一个字符串，P 须指向 <，运行后跳出 > }
    procedure ReadStream(P: TCnPDFParser; Stream: TCnPDFStreamObject);
    {* 读入一个流内容，P 须指向 stream 关键字，运行后跳出 endstream}

    function ReadObject(P: TCnPDFParser): TCnPDFObject;
    {* 读一个完整的间接对象，并设置至 Manager 中返回}
    function ReadObjectInner(P: TCnPDFParser): TCnPDFObject;
    {* 读间接对象内的部分或其他直接对象}

    procedure Encrypt(const OwnerPass, UserPass: Ansistring);
    {* 用权限密码与用户密码对 PDF 文件进行加密，并生成或重写 Encrypt 对象，一般在写入前调用}
    procedure Decrypt(const APass: Ansistring);
    {* 用密码对 PDF 文件进行解密，使用内部的 Encrypt 对象，一般在读入成功后调用}

    property Encrypted: Boolean read FEncrypted write FEncrypted;
    {* 是否加密}
    property NeedPassword: Boolean read GetNeedPassword;
    {* 是否需要密码，打开文件后调用，加密且空密码验证不通过时返回 True}
    property Decrypted: Boolean read FDecrypted;
    {* 是否解密成功，值仅在 Encrypted 为 True 时有效}
    property Permission: Cardinal read FPermission write FPermission;
    {* 允许的权限集合}
    property EncryptionMethod: TCnPDFEncryptionMethod read FEncryptionMethod write FEncryptionMethod;
    {* 支持的加密模式}

    property Header: TCnPDFHeader read FHeader;
    property Body: TCnPDFBody read FBody;
    property XRefTable: TCnPDFXRefTable read FXRefTable;
    property Trailer: TCnPDFTrailer read FTrailer;
  end;

//==============================================================================
//
// 以下是 PDF 文件的词法和语法解析，暂未实现
//
//==============================================================================

  TCnPDFTokenType = (pttUnknown, pttComment, pttBlank, pttLineBreak, pttNumber,
    pttNull, pttTrue, pttFalse, pttObj, pttEndObj, pttStream, pttEnd, pttR,
    pttN, pttD, pttF, pttXref, pttStartxref, pttTrailer,
    pttName, pttStringBegin, pttString, pttStringEnd,
    pttHexStringBegin, pttHexString, pttHexStringEnd, pttArrayBegin, pttArrayEnd,
    pttDictionaryBegin, pttDictionaryEnd, pttStreamData, pttEndStream);
  {* PDF 文件内容中的符号类型，对应%、空格、回车换行、数字、
    null、true、false、obj、stream、end、R、xref、startxref、trailer
    /、(、)、<、>、[、]、<<、>>、流内容、endstream}

  TCnPDFParserBookmark = packed record
  {* 记录 Parser 状态以回溯}
    Run: Integer;
    TokenPos: Integer;
    TokenID: TCnPDFTokenType;
    PrevNonBlankID: TCnPDFTokenType;
    StringLen: Integer;
  end;

  TCnPDFParser = class
  {* PDF 内容解析器}
  private
    FRun: Integer;
    FTokenPos: Integer;
    FTokenID: TCnPDFTokenType;
    FPrevNonBlankID: TCnPDFTokenType;
    FStringLen: Integer; // 当前字符串的字符长度

    FOrigin: PAnsiChar;
    FByteLength: Integer;
    FProcTable: array[#0..#255] of procedure of object;

    procedure KeywordProc;               // obj stream end null true false 等固定标识符
    procedure NameBeginProc;             // /
    procedure StringBeginProc;           // (
    procedure StringEndProc;             // )
    procedure ArrayBeginProc;            // [
    procedure ArrayEndProc;              // ]
    procedure LessThanProc;              // <<
    procedure GreaterThanProc;           // >>
    procedure CommentProc;               // %
    procedure NumberProc;                // 数字+-
    procedure BlankProc;                 // 空格 Tab 等
    procedure CRLFProc;                  // 回车或换行或回车换行
    procedure UnknownProc;               // 未知

    procedure StringProc;                // 手工调用的字符串处理
    procedure HexStringProc;             // 手工调用的十六进制字符串处理
    procedure StreamDataProc;            // 手工调用的流内容处理

    function GetToken: AnsiString;
    procedure SetRunPos(const Value: Integer);
    function GetTokenLength: Integer;
  protected
    procedure Error(const Msg: string);
    function TokenEqualStr(Org: PAnsiChar; const Str: AnsiString): Boolean;
    procedure MakeMethodTable;
    procedure StepRun; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SetOrigin(const PDFBuf: PAnsiChar; PDFByteSize: Integer);

    procedure LoadFromBookmark(var Bookmark: TCnPDFParserBookmark);
    procedure SaveToBookmark(var Bookmark: TCnPDFParserBookmark);

    procedure Next;
    {* 跳至下一个 Token 并确定 TokenID}
    procedure NextNoJunk;
    {* 跳至下一个非 Null 以及非空格 Token 并确定 TokenID}
    procedure NextNoJunkNoCRLF;
    {* 跳至下一个非 Null 以及非空格以及非回车换行 Token 并确定 TokenID}

    property Origin: PAnsiChar read FOrigin;
    {* 待解析的 PDF 内容}
    property RunPos: Integer read FRun write SetRunPos;
    {* 当前处理位置相对于 FOrigin 的线性偏移量，单位为字节数，0 开始}
    property TokenID: TCnPDFTokenType read FTokenID;
    {* 当前 Token 类型}
    property Token: AnsiString read GetToken;
    {* 当前 Token 的字符串内容，暂不解析}
    property TokenLength: Integer read GetTokenLength;
    {* 当前 Token 的字节长度}
  end;

  TCnImagesToPDFCreator = class
  {* 用来把一批 JPEG 文件转换成一个 PDF 的工具类}
  private
    FFiles: TStringList;
    FLeftMargin: Integer;
    FTopMargin: Integer;
    FRightMargin: Integer;
    FPageHeight: Integer;
    FBottomMargin: Integer;
    FPageWidth: Integer;
    FCreator: string;
    FTitle: string;
    FComments: string;
    FKeywords: string;
    FCompany: string;
    FAuthor: string;
    FSubject: string;
    FProducer: string;
    FCreationDate: TDateTime;
    FOwnerPassword: AnsiString;
    FUserPassword: AnsiString;
    FEncrypt: Boolean;
    FPermission: Cardinal;
    FEncryptionMethod: TCnPDFEncryptionMethod;
    procedure SetBottomMargin(const Value: Integer);
    procedure SetLeftMargin(const Value: Integer);
    procedure SetPageHeight(const Value: Integer);
    procedure SetPageWidth(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);
    procedure SetTopMargin(const Value: Integer);
  protected
    procedure ValidatePage;
    procedure CalcImageSize(var ImageWidth, ImageHeight: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddJpegFile(const JpegFile: string);
    {* 增加一个 JPEG 文件}
    procedure AddJpegFiles(const JpegFiles: TStrings);
    {* 增加多个 JPEG 文件}
    procedure RemoveJpegFile(const JpegFile: string);
    {* 从实例中删除一个之前增加的 JPEG 文件，注意不会删除文件本身}
    procedure Clear;
    {* 清除所有增加的 JPEG 文件}

    procedure SaveToPDF(const PDFFile: string);
    {* 将添加的 JPEG 文件存成指定文件名的 PDF}

    property PageHeight: Integer read FPageHeight write SetPageHeight;
    {* 以点为单位的页面高度}
    property PageWidth: Integer read FPageWidth write SetPageWidth;
    {* 以点为单位的页面宽度}
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin;
    {* 以点为单位的页面内容左边距}
    property RightMargin: Integer read FRightMargin write SetRightMargin;
    {* 以点为单位的页面内容右边距}
    property TopMargin: Integer read FTopMargin write SetTopMargin;
    {* 以点为单位的页面内容上边距}
    property BottomMargin: Integer read FBottomMargin write SetBottomMargin;
    {* 以点为单位的页面内容下边距}

    property Author: string read FAuthor write FAuthor;
    {* 作者}
    property Producer: string read FProducer write FProducer;
    {* 生产者}
    property Creator: string read FCreator write FCreator;
    {* 创作者}
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    {* 创作日期}
    property Title: string read FTitle write FTitle;
    {* 文档标题}
    property Subject: string read FSubject write FSubject;
    {* 文档主题}
    property Keywords: string read FKeywords write FKeywords;
    {* 文档关键字}
    property Company: string read FCompany write FCompany;
    {* 文档所属公司}
    property Comments: string read FComments write FComments;
    {* 其他注释}

    // 加密相关，支持 128RC4
    property Encrypt: Boolean read FEncrypt write FEncrypt;
    {* 由外界指定是否加密}
    property OwnerPassword: AnsiString read FOwnerPassword write FOwnerPassword;
    {* 权限控制密码}
    property UserPassword: AnsiString read FUserPassword write FUserPassword;
    {* 用户打开密码}
    property Permission: Cardinal read FPermission write FPermission;
    {* 允许的权限集合}
    property EncryptionMethod: TCnPDFEncryptionMethod read FEncryptionMethod write FEncryptionMethod;
    {* 支持的加密模式}
  end;

function CnLoadPDFFile(const FileName: string): TCnPDFDocument;
{* 解析一个 PDF 文件，返回一个新建的 PDFDocument 对象}

procedure CnSavePDFFile(PDF: TCnPDFDocument; const FileName: string);
{* 将一个 PDFDocument 对象保存成 PDF 文件}

procedure CnJpegFilesToPDF(JpegFiles: TStrings; const FileName: string);
{* 将一批 JPG 文件拼成一个 PDF 文件，输出至指定文件名
  PDF 页面内采用竖向的标准 A4 纸张尺寸并采用标准左右上下边距值}

procedure CnExtractJpegFilesFromPDF(const FileName, OutDirName: string);
{* 打开指定 PDF 文件并将其内部的 JPG 图片解压至指定目录}

implementation

uses
  {$IFNDEF NO_ZLIB} CnZip, {$ENDIF} CnRandom, CnMD5, CnRC4;

const
  CN_PDF_A4PT_WIDTH = 612;        // A4 页面的默认宽度
  CN_PDF_A4PT_HEIGHT = 792;       // A4 页面的默认高度
  CN_PDF_A4PT_MARGIN_LEFT = 90;   // A4 页面的默认左边距
  CN_PDF_A4PT_MARGIN_RIGHT = 90;  // A4 页面的默认右边距
  CN_PDF_A4PT_MARGIN_TOP = 72;    // A4 页面的默认上边距
  CN_PDF_A4PT_MARGIN_BOTTOM = 72; // A4 页面的默认下边距

  IDLENGTH = 16;                  // PDF 文件的 ID 的长度
  INDENTDELTA = 4;                // PDF 内容输出时的默认缩进

  SPACE: AnsiChar = ' ';                       // 空格
  CRLF: array[0..1] of AnsiChar = (#13, #10);  // 回车换行

  CRLFS: set of AnsiChar = [#13, #10];
  // PDF 规范中的空白字符中的回车换行
  WHITESPACES: set of AnsiChar = [#0, #9, #12, #32];
  // PDF 规范中除了回车换行之外的空白字符
  DELIMETERS: set of AnsiChar = ['(', ')', '<', '>', '[', ']', '{', '}', '%'];
  // PDF 规范中的分隔字符

  // 以下是 PDF 文中的固定字符串
  PDFHEADER: AnsiString = '%PDF-';
  OBJFMT: AnsiString = '%d %d obj';
  ENDOBJ: AnsiString = 'endobj';
  XREF: AnsiString = 'xref';
  BEGINSTREAM: AnsiString = 'stream';
  ENDSTREAM: AnsiString = 'endstream';

  TRAILER: AnsiString = 'trailer';
  STARTXREF: AnsiString = 'startxref';
  EOF: AnsiString = '%%EOF';

resourcestring
  SCnErrorPDFImageFileNotFound = 'Image File NOT Found';
  SCnErrorPDFNoFile = 'NO Image Files';
  SCnErrorPDFPageSize = 'Invalid Page Size';
  SCnErrorPDFPageMargin = 'Invalid Page Margin';
  SCnErrorPDFPageSizeMargin = 'Invalid Page Size and Margin';
  SCnErrorPDFEncryptParams = 'Invalid Encrypt Params';
  SCnErrorPDFEncryptPassword = 'Invalid Password';
  SCnErrorPDFEncryptNOTSupport = 'Encrypt Method NOT Support';
  SCnErrorPDFEncryptNOTSupportFmt = 'Encrypt Filter %s NOT Support';
  SCnErrorPDFEscapeCharNOTSupportFmt = 'Escape Char NOT Support %d';

function WriteSpace(Stream: TStream): Cardinal;
begin
  Result := Stream.Write(SPACE, SizeOf(SPACE));
end;

function WriteCRLF(Stream: TStream): Cardinal;
begin
  Result := Stream.Write(CRLF[0], SizeOf(CRLF));
end;

function WriteLine(Stream: TStream; const Str: AnsiString): Cardinal;
begin
  if Length(Str) > 0 then
    Result := Stream.Write(Str[1], Length(Str))
  else
    Result := 0;
  Inc(Result, WriteCRLF(Stream));
end;

function WriteString(Stream: TStream; const Str: AnsiString): Cardinal;
begin
  if Length(Str) > 0 then
    Result := Stream.Write(Str[1], Length(Str))
  else
    Result := 0;
end;

function WriteBytes(Stream: TStream; const Data: TBytes): Cardinal;
begin
  if Length(Data) > 0 then
    Result := Stream.Write(Data[0], Length(Data))
  else
    Result := 0;
end;

function XRefTokenToType(XRefToken: TCnPDFTokenType): TCnPDFXRefType;
begin
  case XRefToken of
    pttN: Result := xrtNormal;
    pttD: Result := xrtDeleted;
    pttF: Result := xrtFree;
  else
    Result := xrtNormal;
  end;
end;

function XRefTypeToString(XRefType: TCnPDFXRefType): AnsiString;
begin
  case XRefType of
    xrtFree: Result := 'f';
    xrtNormal: Result := 'n';
    xrtDeleted: Result := 'd';
  else
    Result := 'n';
  end;
end;

procedure ParseError(P: TCnPDFParser; const Msg: string);
begin
  raise ECnPDFException.CreateFmt('PDF Parse Error at %d: %s', [P.RunPos, Msg]);
end;

procedure CheckExpectedToken(P: TCnPDFParser; ExpectedToken: TCnPDFTokenType);
begin
  if P.TokenID <> ExpectedToken then
    ParseError(P, Format('Expect Token %s but Meet %s',
      [GetEnumName(TypeInfo(TCnPDFTokenType), Ord(ExpectedToken)),
      GetEnumName(TypeInfo(TCnPDFTokenType), Ord(P.TokenID))]));
end;

function TrimToName(const SlashName: string): string;
begin
  Result := SlashName;
  if SlashName <> '' then
  begin
    if Result[1] = '/' then
    begin
      Delete(Result, 1, 1);
      Result := Trim(Result);
    end;
  end;
end;

{ TCnPDFTrailer }

constructor TCnPDFTrailer.Create;
begin
  inherited;
  FDictionary := TCnPDFDictionaryObject.Create;
end;

destructor TCnPDFTrailer.Destroy;
begin
  FDictionary.Free;
  inherited;
end;

procedure TCnPDFTrailer.DumpToStrings(Strings: TStrings; Verbose: Boolean;
  Indent: Integer);
var
  I: Integer;
  V: TCnPDFObject;
  N: TStringList;
begin
  Strings.Add('Trailer');
  Strings.Add('Dictionary:');
  N := TStringList.Create;
  try
    FDictionary.GetNames(N);
    for I := 0 to N.Count - 1 do
    begin
      V := FDictionary.Values[N[I]];
      if V = nil then
        N[I] := N[I] + ': nil'
      else
        N[I] := N[I] + ': ' + V.ToString;
    end;

    Strings.AddStrings(N);
  finally
    N.Free;
  end;
  Strings.Add('XRefStart ' + IntToStr(FXRefStart));
  Strings.Add(FComment);
end;

procedure TCnPDFTrailer.GenerateID;
var
  Arr: TCnPDFArrayObject;
  V: TCnPDFStringObject;
  S: AnsiString;
begin
  Arr := TCnPDFArrayObject.Create;
  SetLength(S, IDLENGTH);

  CnRandomFillBytes(@S[1], IDLENGTH);
  V := TCnPDFStringObject.Create(S);
  V.IsHex := True;
  Arr.AddObject(V);

  CnRandomFillBytes(@S[1], IDLENGTH);
  V := TCnPDFStringObject.Create(S);
  V.IsHex := True;
  Arr.AddObject(V);

  FDictionary.Values['ID'] := Arr;
end;

function TCnPDFTrailer.WriteToStream(Stream: TStream): Cardinal;
begin
  Result := 0;
  Inc(Result, WriteLine(Stream, TRAILER));
  Inc(Result, FDictionary.WriteToStream(Stream));
  Inc(Result, WriteLine(Stream, STARTXREF));
  Inc(Result, WriteLine(Stream, IntToStr(FXRefStart)));
  Inc(Result, WriteLine(Stream, EOF));
end;

{ TCnPDFXRefCollection }

function TCnPDFXRefCollection.Add: TCnPDFXRefItem;
begin
  Result := TCnPDFXRefItem(inherited Add);
end;

constructor TCnPDFXRefCollection.Create;
begin
  inherited Create(TCnPDFXRefItem);
end;

destructor TCnPDFXRefCollection.Destroy;
begin

  inherited;
end;

function TCnPDFXRefCollection.GetItem(Index: Integer): TCnPDFXRefItem;
begin
  Result := TCnPDFXRefItem(inherited GetItem(Index));
end;

procedure TCnPDFXRefCollection.SetItem(Index: Integer;
  const Value: TCnPDFXRefItem);
begin
  inherited SetItem(Index, Value);
end;

function TCnPDFXRefCollection.WriteToStream(Stream: TStream): Cardinal;
var
  I: Integer;
begin
  Result := WriteLine(Stream, Format('%d %d', [FObjectIndex, Count]));
  for I := 0 to Count - 1 do
    Inc(Result, WriteLine(Stream, Format('%10.10d %5.5d %s', [Items[I].ObjectOffset,
      Items[I].ObjectGeneration, XRefTypeToString(Items[I].ObjectXRefType)])));
end;

{ TCnPDFXRefTable }

function TCnPDFXRefTable.AddSegment: TCnPDFXRefCollection;
begin
  Result := TCnPDFXRefCollection.Create;
  FSegments.Add(Result);
end;

procedure TCnPDFXRefTable.Clear;
begin
  FSegments.Clear;
end;

constructor TCnPDFXRefTable.Create;
begin
  inherited;
  FSegments := TObjectList.Create(True);
end;

destructor TCnPDFXRefTable.Destroy;
begin
  FSegments.Free;
  inherited;
end;

procedure TCnPDFXRefTable.DumpToStrings(Strings: TStrings; Verbose: Boolean;
  Indent: Integer);
var
  I, J: Integer;
  Seg: TCnPDFXRefCollection;
begin
  Strings.Add('XRefTable');
  for I := 0 to SegmentCount - 1 do
  begin
    Seg := Segments[I];
    Strings.Add(Format('%d %d', [Seg.ObjectIndex, Seg.Count]));
    for J := 0 to Seg.Count - 1 do
      Strings.Add(Format('%10.10d %5.5d %s', [Seg.Items[J].ObjectOffset,
        Seg.Items[J].ObjectGeneration, XRefTypeToString(Seg.Items[J].ObjectXRefType)]));
  end;
end;

function TCnPDFXRefTable.FindByObject(Obj: TCnPDFObject): TCnPDFXRefItem;
var
  I, J: Integer;
  Seg: TCnPDFXRefCollection;
begin
  for I := 0 to SegmentCount - 1 do
  begin
    Seg := Segments[I];
    for J := 0 to Seg.Count - 1 do
    begin
      if (Seg.ObjectIndex + Cardinal(J) = Obj.ID) and (Seg.Items[J].ObjectGeneration = Obj.Generation) then
      begin
        Result := Seg.Items[J];
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TCnPDFXRefTable.GetSegmenet(Index: Integer): TCnPDFXRefCollection;
begin
  Result := TCnPDFXRefCollection(FSegments[Index]);
end;

function TCnPDFXRefTable.GetSegmentCount: Integer;
begin
  Result := FSegments.Count;
end;

procedure TCnPDFXRefTable.SetSegment(Index: Integer;
  const Value: TCnPDFXRefCollection);
begin
  FSegments[Index] := Value;
end;

function TCnPDFXRefTable.WriteToStream(Stream: TStream): Cardinal;
var
  I: Integer;
  Seg: TCnPDFXRefCollection;
  Item: TCnPDFXRefItem;
begin
  Result := WriteLine(Stream, XREF);

  // 最头上的一个如果是 1 开始，插入一个 0 的
  if GetSegmentCount > 0 then
  begin
    Seg := Segments[0];
    if Seg.ObjectIndex = 1 then
    begin
      Seg.ObjectIndex := 0;

      Item := TCnPDFXRefItem(Seg.Insert(0));
      Item.ObjectGeneration := 65535;
      Item.ObjectOffset := 0;
      Item.ObjectXRefType := xrtFree;
    end;
  end;

  for I := 0 to SegmentCount - 1 do
    Inc(Result, Segments[I].WriteToStream(Stream));
end;

{ TCnPDFParser }

procedure TCnPDFParser.ArrayBeginProc;
begin
  StepRun;
  FTokenID := pttArrayBegin;
end;

procedure TCnPDFParser.ArrayEndProc;
begin
  StepRun;
  FTokenID := pttArrayEnd;
end;

procedure TCnPDFParser.BlankProc;
begin
  repeat
    StepRun;
  until not (FOrigin[FRun] in WHITESPACES);
  FTokenID := pttBlank;
end;

procedure TCnPDFParser.CommentProc;
begin
  repeat
    StepRun;
  until (FOrigin[FRun] in [#13, #10]);
  FTokenID := pttComment;
end;

constructor TCnPDFParser.Create;
begin
  inherited;
  MakeMethodTable;
end;

procedure TCnPDFParser.CRLFProc;
begin
  repeat
    StepRun;
  until not (FOrigin[FRun] in [#13, #10]);
  FTokenID := pttLineBreak;
end;

destructor TCnPDFParser.Destroy;
begin

  inherited;
end;

procedure TCnPDFParser.LessThanProc;
begin
  StepRun;
  if FOrigin[FRun] = '<' then
  begin
    StepRun;
    FTokenID := pttDictionaryBegin;
  end
  else
    FTokenID := pttHexStringBegin;
  // Error('Dictionary Begin Corrupt');
end;

procedure TCnPDFParser.GreaterThanProc;
begin
  StepRun;
  if FOrigin[FRun] = '>' then
  begin
    StepRun;
    FTokenID := pttDictionaryEnd;
  end
  else
    FTokenID := pttHexStringEnd;
  // Error('Dictionary End Corrupt');
end;

procedure TCnPDFParser.Error(const Msg: string);
begin
  raise ECnPDFException.CreateFmt('PDF Token Parse Error at %d: %s', [FRun, Msg]);
end;

function TCnPDFParser.GetToken: AnsiString;
var
  Len: Cardinal;
  OutStr: AnsiString;
begin
  Len := FRun - FTokenPos;                         // 两个偏移量之差，单位为字符数
  SetString(OutStr, (FOrigin + FTokenPos), Len);   // 以指定内存地址与长度构造字符串
  Result := OutStr;
end;

function TCnPDFParser.GetTokenLength: Integer;
begin
  Result := FRun - FTokenPos;
end;

procedure TCnPDFParser.KeywordProc;
begin
  FStringLen := 0;
  repeat
    StepRun;
    Inc(FStringLen);
  until not (FOrigin[FRun] in ['a'..'z', 'A'..'Z']); // 找到小写字母组合的标识符尾巴

  FTokenID := pttUnknown; // 先这么设
  // 比较 endstream endobj stream false null true obj end

  if FStringLen = 9 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'endstream') then
      FTokenID := pttEndStream
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'startxref') then
      FTokenID := pttStartxref
  end
  else if FStringLen = 7 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'trailer') then
      FTokenID := pttTrailer
  end
  else if FStringLen = 6 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'stream') then
      FTokenID := pttStream
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'endobj') then
      FTokenID := pttEndObj
  end
  else if FStringLen = 5 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'false') then
      FTokenID := pttFalse
  end
  else if FStringLen = 4 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'true') then
      FTokenID := pttTrue
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'null') then
      FTokenID := pttNull
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'xref') then
      FTokenID := pttXref;
  end
  else if FStringLen = 3 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'obj') then
      FTokenID := pttObj
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'end') then
      FTokenID := pttEnd;
  end
  else if FStringLen = 1 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'R') then
      FTokenID := pttR
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'n') then
      FTokenID := pttN
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'd') then
      FTokenID := pttD
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'f') then
      FTokenID := pttF
  end;
end;

procedure TCnPDFParser.MakeMethodTable;
var
  I: AnsiChar;
begin
  for I := #0 to #255 do
  begin
    case I of
      '%':
        FProcTable[I] := CommentProc;
      #9, #32:
        FProcTable[I] := BlankProc;
      #10, #13:
        FProcTable[I] := CRLFProc;
      '(':
        FProcTable[I] := StringBeginProc;
      ')':
        FProcTable[I] := StringEndProc;
      '0'..'9', '+', '-':
        FProcTable[I] := NumberProc;
      '[':
        FProcTable[I] := ArrayBeginProc;
      ']':
        FProcTable[I] := ArrayEndProc;
      '<':
        FProcTable[I] := LessThanProc;
      '>':
        FProcTable[I] := GreaterThanProc;
      '/':
        FProcTable[I] := NameBeginProc;
      'f', 'n', 't', 'o', 's', 'e', 'x', 'R':
        FProcTable[I] := KeywordProc;
    else
      FProcTable[I] := UnknownProc;
    end;
  end;
end;

procedure TCnPDFParser.NameBeginProc;
begin
  repeat
    StepRun;
  until FOrigin[FRun] in CRLFS + WHITESPACES + DELIMETERS + ['/'];
  FTokenID := pttName;
end;

procedure TCnPDFParser.Next;
var
  OldId: TCnPDFTokenType;
begin
  FTokenPos := FRun;
  OldId := FTokenID;

  if (FTokenID = pttStringBegin) and (FOrigin[FRun] <> ')') then
    StringProc
  else if (FTokenID = pttHexStringBegin) and (FOrigin[FRun] <> '>') then
    HexStringProc
  else if (FTokenID = pttLineBreak) and (FPrevNonBlankID = pttStream) then
    StreamDataProc
  else
    FProcTable[FOrigin[FRun]];

  if not (FTokenID in [pttBlank, pttComment]) then // 保留一个非空回溯
    FPrevNonBlankID := OldId;
end;

procedure TCnPDFParser.NextNoJunk;
begin
  repeat
    Next;
  until not (FTokenID in [pttBlank]);
end;

procedure TCnPDFParser.NumberProc;
begin
  repeat
    StepRun;
  until not (FOrigin[FRun] in ['0'..'9', '.']); // 负号不能再出现了，也不能出现 e 这种科学计数法
  FTokenID := pttNumber;
end;

procedure TCnPDFParser.SetOrigin(const PDFBuf: PAnsiChar; PDFByteSize: Integer);
begin
  FOrigin := PDFBuf;
  FRun := 0;
  FByteLength := PDFByteSize;

  // 重新初始化
  FTokenPos := 0;
  FTokenID := pttUnknown;
  FPrevNonBlankID := pttUnknown;
  FStringLen := 0;

  Next;
end;

procedure TCnPDFParser.SetRunPos(const Value: Integer);
begin
  FRun := Value;
  Next;
end;

procedure TCnPDFParser.StepRun;
begin
  Inc(FRun);
  if FRun >= FByteLength then
    raise ECnPDFEofException.Create('PDF EOF');
end;

procedure TCnPDFParser.StreamDataProc;
var
  I, OldRun: Integer;
  Es: AnsiString;
begin
  // 开始流内容，到回车换行后判断后方是否 endstream
  SetLength(Es, 9);
  repeat
    StepRun;

    if FOrigin[FRun] in [#13, #10] then
    begin
      repeat
        StepRun;
      until not (FOrigin[FRun] in [#13, #10]);

      // 往前跳八个并判断是否 endstream 关键字，无论是否成功都跳回来
      OldRun := FRun; // 记录原始位置
      for I := 1 to 9 do
      begin
        Es[I] := FOrigin[FRun];
        StepRun;
      end;
      FRun := OldRun; // 回来

      if Es = 'endstream' then // 只有碰到 endstream 才跳出
        Break;
    end;
  until False;

  // 注意 endstream 前面可能有多余的回车换行，需要根据 Length 字段值修正
  FTokenID := pttStreamData;
end;

procedure TCnPDFParser.StringBeginProc;
begin
  StepRun;
  FTokenID := pttStringBegin;
end;

procedure TCnPDFParser.StringEndProc;
begin
  StepRun;
  FTokenID := pttStringEnd;
end;

procedure TCnPDFParser.StringProc;
var
  C: Integer;
begin
  // TODO: 判断头俩字节是否是 UTF16，是则俩字节俩字节读直到单个碰到 ) 否则单个读直到读到 )
  C := 0;
  repeat
    StepRun;
    if FOrigin[FRun - 1] = '\' then
      StepRun
    else if FOrigin[FRun - 1] = '(' then
      Inc(C)
    else if FOrigin[FRun - 1] = ')' then
      Dec(C);
  until (FOrigin[FRun] = ')') and (C = 0);
  FTokenID := pttString;
end;

function TCnPDFParser.TokenEqualStr(Org: PAnsiChar; const Str: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Length(Str) - 1 do
  begin
    if Org[I] <> Str[I + 1] then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

procedure TCnPDFParser.UnknownProc;
begin
  StepRun;
  FTokenID := pttUnknown;
end;

procedure TCnPDFParser.HexStringProc;
begin
  repeat
    StepRun;
  until not (FOrigin[FRun] in ['0'..'9', 'a'..'f', 'A'..'F'] + CRLFS + WHITESPACES);
  FTokenID := pttHexString;
end;

procedure TCnPDFParser.NextNoJunkNoCRLF;
begin
  repeat
    Next;
  until not (FTokenID in [pttBlank, pttLineBreak, pttComment]);
end;

procedure TCnPDFParser.LoadFromBookmark(var Bookmark: TCnPDFParserBookmark);
begin
  FRun := Bookmark.Run;
  FTokenPos := Bookmark.TokenPos;
  FTokenID := Bookmark.TokenID;
  FPrevNonBlankID := Bookmark.PrevNonBlankID;
  FStringLen := Bookmark.StringLen;
end;

procedure TCnPDFParser.SaveToBookmark(var Bookmark: TCnPDFParserBookmark);
begin
  Bookmark.Run := FRun;
  Bookmark.TokenPos := FTokenPos;
  Bookmark.TokenID := FTokenID;
  Bookmark.PrevNonBlankID := FPrevNonBlankID;
  Bookmark.StringLen := FStringLen;
end;

{ TCnPDFHeader }

constructor TCnPDFHeader.Create;
begin
  inherited;
  FVersion := '1.7';
  FComment := '中国CnPack开发组';
end;

destructor TCnPDFHeader.Destroy;
begin

  inherited;
end;

procedure TCnPDFHeader.DumpToStrings(Strings: TStrings; Verbose: Boolean;
  Indent: Integer);
begin
  Strings.Add('PDF Version ' + FVersion);
  Strings.Add('PDF First Comment ' + FComment);
end;

function TCnPDFHeader.WriteToStream(Stream: TStream): Cardinal;
begin
  Result := WriteLine(Stream, '%PDF-' + FVersion);
  Inc(Result, WriteLine(Stream, '%' + FComment));
end;

{ TCnPDFObject }

function TCnPDFObject.CheckWriteObjectEnd(Stream: TStream): Cardinal;
begin
  if ID > 0 then
    Result := WriteLine(Stream, ENDOBJ)
  else
    Result := 0;
end;

function TCnPDFObject.CheckWriteObjectStart(Stream: TStream): Cardinal;
begin
  if ID > 0 then
    Result := WriteLine(Stream, Format(OBJFMT, [ID, Generation]))
  else
    Result := 0;
end;

function TCnPDFObject.Clone: TCnPDFObject;
var
  Clz: TCnPDFObjectClass;
begin
  if Self = nil then
  begin
    Result := nil;
    Exit;
  end;

  Clz := TCnPDFObjectClass(ClassType);
  try
    Result := TCnPDFObject(Clz.NewInstance);
    Result.Create;

    Result.Assign(Self);
  except
    Result := nil;
  end;
end;

constructor TCnPDFObject.Create;
begin
  inherited;

end;

destructor TCnPDFObject.Destroy;
begin

  inherited;
end;

function TCnPDFObject.ToString: string;
begin
  raise ECnPDFException.Create('NO ToString for Base PDF Object');
end;

procedure TCnPDFObject.ToStrings(Strings: TStrings; Indent: Integer);
begin
  if Strings <> nil then
  begin
    if Indent = 0 then
      Strings.Add(ToString)
    else
      Strings.Add(StringOfChar(' ', Indent) + ToString);
  end;
end;

{ TCnPDFDocument }

constructor TCnPDFDocument.Create;
begin
  inherited;
  FHeader := TCnPDFHeader.Create;
  FBody := TCnPDFBody.Create;
  FXRefTable := TCnPDFXRefTable.Create;
  FTrailer := TCnPDFTrailer.Create;

  FBody.XRefTable := FXRefTable;
//  FTrailer.Dictionary.AddObjectRef('Root', FBody.Catalog);
//  FTrailer.Dictionary.AddObjectRef('Info', FBody.Info);
end;

destructor TCnPDFDocument.Destroy;
begin
  FTrailer.Free;
  FXRefTable.Free;
  FBody.Free;
  FHeader.Free;
  inherited;
end;

procedure TCnPDFDocument.LoadFromFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TCnPDFDocument.LoadFromStream(Stream: TStream);
var
  P: TCnPDFParser;
  M: TMemoryStream;
  S: AnsiString;
  X: PAnsiChar;
  Obj: TCnPDFObject;
begin
  P := nil;
  M := nil;

  try
    P := TCnPDFParser.Create;
    M := TMemoryStream.Create;
    M.LoadFromStream(Stream);
    P.SetOrigin(M.Memory, M.Size);

    try
      if P.TokenID <> pttComment then
        ParseError(P, 'NO PDF File Header!');

      // 处理第一个 Comment
      S := P.Token;
      if (Length(S) < 6) or (Pos(PDFHEADER, S) <> 1) then
        ParseError(P, 'PDF File Header Corrupt');

      Delete(S, 1, Length(PDFHEADER));
      FHeader.Version := S;

      // 如果有则处理第二个 Comment
      P.NextNoJunk;
      if P.TokenID = pttLineBreak then
        P.NextNoJunk;
      if P.TokenID = pttComment then
      begin
        FHeader.Comment := P.Token;
        P.NextNoJunkNoCRLF;
      end;

      // 后面处理对象列表等
      while True do
      begin
        case P.TokenID of
          pttXref:
            begin
              // 读交叉引用表
              ReadXRef(P);
            end;
          pttTrailer:
            begin
              // 读尾巴
              ReadTrailer(P);
            end;
          pttNumber:
            begin
              // 数字、数字、obj 这种
              ReadObject(P);
            end;
          pttStartXRef:
            begin
              // 某些情况会出现单独的 startxref，读了再说
              ReadTrailerStartXRef(P);
            end;
        else
          P.NextNoJunk;
        end;
      end;
    except
      on E: ECnPDFEofException do // PDF 解析完毕的异常吞掉，正常往下走
      begin
        ;
      end;
    end;

    // 如果没读到 xref 关键字指示的交叉应用表，则从 startxref 处再读新类型的
    if FXRefTable.SegmentCount = 0 then
    begin
      if FTrailer.XRefStart > 0 then
      begin
        X := M.Memory;
        Inc(X, FTrailer.XRefStart);

        P.SetOrigin(X, M.Size - Integer(FTrailer.XRefStart));
        if P.TokenID = pttNumber then
        begin
          Obj := ReadObject(P);
          if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) and
            ((Obj as TCnPDFDictionaryObject).GetType = 'XRef') then
          begin
            XRefDictToXRefTable(Obj as TCnPDFDictionaryObject);

            // TODO: 如果有 Prev，要一路读过去合并之
          end;
        end;
      end;
    end;
  finally
    M.Free;
    P.Free;
  end;

  // 从 Trailer 里的字段整理内容
  ArrangeObjects;

  // 未加密或解密了的情况下解开压缩 Content 的内容，否则放到解密后再解压
  if not FEncrypted or FDecrypted then
    UncompressObjects;
end;

procedure TCnPDFDocument.ReadArray(P: TCnPDFParser;
  AnArray: TCnPDFArrayObject);
var
  Obj: TCnPDFObject;
begin
  P.NextNoJunkNoCRLF;
  if P.TokenID = pttArrayEnd then
  begin
    AnArray.Clear;
    P.NextNoJunkNoCRLF;
    Exit;
  end;

  while P.TokenID <> pttArrayEnd do
  begin
    Obj := ReadObjectInner(P);
    AnArray.AddObject(Obj);
  end;
  P.NextNoJunkNoCRLF;
end;

procedure TCnPDFDocument.ReadDictionary(P: TCnPDFParser;
  Dict: TCnPDFDictionaryObject);
var
  N: TCnPDFNameObject;
  V: TCnPDFObject;
  Pair: TCnPDFDictPair;
begin
  P.NextNoJunkNoCRLF;
  if P.TokenID = pttDictionaryEnd then
  begin
    Dict.Clear;
    P.NextNoJunkNoCRLF;
    Exit;
  end;

  N := TCnPDFNameObject.Create;
  try
    while P.TokenID <> pttDictionaryEnd do
    begin
      CheckExpectedToken(P, pttName);
      ReadName(P, N);
      Pair := Dict.AddName(N.Name);

      V := ReadObjectInner(P);
      Pair.Value := V;
    end;

    P.NextNoJunkNoCRLF;
    // 跳出后可能是 stream
  finally
    N.Free;
  end;
end;

procedure TCnPDFDocument.ReadName(P: TCnPDFParser; Name: TCnPDFNameObject);
begin
  Name.Content := AnsiToBytes(TrimToName(P.Token));
  P.NextNoJunkNoCRLF;
end;

procedure TCnPDFDocument.ReadNumber(P: TCnPDFParser; Num: TCnPDFNumberObject;
  OverCRLF: Boolean);
var
  S: AnsiString;
  R, E: Integer;
  F: Extended;
begin
  S := P.Token;

  if OverCRLF then
    P.NextNoJunkNoCRLF
  else
    P.NextNoJunk;

  Val(S, R, E);
  if E = 0 then
    Num.SetInteger(R)
  else
  begin
    Val(S, F, E);
    if E = 0 then
      Num.SetFloat(F)
    else
      ParseError(P, 'PDF Number Format Error');
  end;
end;

function TCnPDFDocument.ReadObject(P: TCnPDFParser): TCnPDFObject;
var
  Num: TCnPDFNumberObject;
  ID, G: Cardinal;
  Ofst: Integer;
begin
  // 读 数字 数字 obj
  Num := TCnPDFNumberObject.Create;
  try
    CheckExpectedToken(P, pttNumber);
    Ofst := P.RunPos - P.TokenLength;
    ReadNumber(P, Num); // 内部会步进
    ID := Num.AsInteger;

    CheckExpectedToken(P, pttNumber);
    ReadNumber(P, Num);
    G := Num.AsInteger;

    CheckExpectedToken(P, pttObj);
    P.NextNoJunkNoCRLF;

    Result := ReadObjectInner(P);
    Result.ID := ID;
    Result.Generation := G;
    Result.Offset := Ofst;

    CheckExpectedToken(P, pttEndObj);
    P.NextNoJunkNoCRLF;

    FBody.Objects.AddRaw(Result);
  finally
    Num.Free;
  end;
end;

function TCnPDFDocument.ReadObjectInner(P: TCnPDFParser): TCnPDFObject;
var
  Stream: TCnPDFStreamObject;
  Bookmark: TCnPDFParserBookmark;
  IsR: Boolean;
begin
  Result := nil;
  case P.TokenID of
    pttDictionaryBegin:
      begin
        // 不一定是 Dict，可能是 Stream
        Result := TCnPDFDictionaryObject.Create;
        ReadDictionary(P, Result as TCnPDFDictionaryObject);

        // 如果发现是 Stream，则把 Result 改成 TCnPDFStreamObject
        if P.TokenID = pttStream then
        begin
          P.NextNoJunkNoCRLF;
          CheckExpectedToken(P, pttStreamData);

          Stream := TCnPDFStreamObject.Create;
          Stream.Assign(Result);
          ReadStream(P, Stream);

          Result.Free;
          Result := Stream;                         // 读完过了 endstream
        end;
      end;
    pttArrayBegin:
      begin
        Result := TCnPDFArrayObject.Create;
        ReadArray(P, Result as TCnPDFArrayObject); // 读完过了 ]
      end;
    pttStringBegin:
      begin
        Result := TCnPDFStringObject.Create;
        ReadString(P, Result as TCnPDFStringObject); // 读完过了 )
      end;
    pttHexStringBegin:
      begin
        Result := TCnPDFStringObject.Create;
        ReadHexString(P, Result as TCnPDFStringObject); // 读完过了 >
      end;
    pttNumber:
      begin
        // 要区分 数字、数字 R 这种引用
        IsR := False;
        P.SaveToBookmark(Bookmark);

        if P.TokenID = pttNumber then
        begin
          P.NextNoJunk;
          if P.TokenID = pttNumber then
          begin
            P.NextNoJunk;
            if P.TokenID = pttR then
              IsR := True;
          end;
        end;
        P.LoadFromBookmark(Bookmark);

        if IsR then
        begin
          Result := TCnPDFReferenceObject.Create(nil);
          ReadReference(P, Result as TCnPDFReferenceObject);
        end
        else
        begin
          Result := TCnPDFNumberObject.Create;
          ReadNumber(P, Result as TCnPDFNumberObject);
        end;
      end;
    pttName:
      begin
        Result := TCnPDFNameObject.Create; // 去掉斜杠
        ReadName(P, Result as TCnPDFNameObject);
      end;
    pttNull:
      begin
        Result := TCnPDFNullObject.Create;
        P.NextNoJunkNoCRLF;
      end;
    pttTrue:
      begin
        Result := TCnPDFBooleanObject.Create(True);
        P.NextNoJunkNoCRLF;
      end;
    pttFalse:
      begin
        Result := TCnPDFBooleanObject.Create(False);
        P.NextNoJunkNoCRLF;
      end;
  end;
end;

procedure TCnPDFDocument.ReadStream(P: TCnPDFParser; Stream: TCnPDFStreamObject);
var
  V: TCnPDFObject;
  L: Integer;
begin
  if P.TokenID = pttStreamData then
  begin
    SetLength(Stream.FStream, P.TokenLength);
    if P.TokenLength > 0 then
      Move(P.Token[1], Stream.Stream[0], P.TokenLength);
  end;
  P.NextNoJunk;

  V := Stream.Values['Length'];
  if (V <> nil) and (V is TCnPDFNumberObject) then
  begin
    L := (V as TCnPDFNumberObject).AsInteger;
    if L < Length(Stream.Stream) then
      SetLength(Stream.FStream, L);
  end;

  CheckExpectedToken(P, pttEndStream);
  P.NextNoJunkNoCRLF;
end;

procedure TCnPDFDocument.ReadHexString(P: TCnPDFParser; Str: TCnPDFStringObject);
begin
  P.NextNoJunk;
  if P.TokenID = pttHexStringEnd then
  begin
    SetLength(Str.FContent, 0);
    P.NextNoJunkNoCRLF;
    Exit;
  end;

  CheckExpectedToken(P, pttHexString);
  Str.Content := HexToBytes(P.Token);
  Str.IsHex := True;
  P.NextNoJunk;

  CheckExpectedToken(P, pttHexStringEnd);
  P.NextNoJunkNoCRLF;
end;

procedure TCnPDFDocument.ReadString(P: TCnPDFParser;
  Str: TCnPDFStringObject);
begin
  P.NextNoJunk;
  if P.TokenID = pttStringEnd then
  begin
    SetLength(Str.FContent, 0);
    P.NextNoJunkNoCRLF;
    Exit;
  end;

  CheckExpectedToken(P, pttString);

  Str.Content := AnsiToBytes(P.Token);
  Str.RemoveEscape;
  P.NextNoJunk;

  CheckExpectedToken(P, pttStringEnd);
  P.NextNoJunkNoCRLF;
end;

procedure TCnPDFDocument.ReadTrailerStartXRef(P: TCnPDFParser);
var
  Num: TCnPDFNumberObject;
begin
  P.NextNoJunkNoCRLF;
  CheckExpectedToken(P, pttNumber);
  Num := TCnPDFNumberObject.Create;
  try
    ReadNumber(P, Num, False);
    FTrailer.XRefStart := Num.AsInteger;
  finally
    Num.Free;
  end;

  if P.TokenID = pttLineBreak then
    P.NextNoJunk;
  CheckExpectedToken(P, pttComment); // %%EOF
  FTrailer.Comment := P.Token;
end;

procedure TCnPDFDocument.ReadTrailer(P: TCnPDFParser);
begin
  // 读字典、及 startxref 的内容
  CheckExpectedToken(P, pttTrailer);
  P.NextNoJunkNoCRLF;
  CheckExpectedToken(P, pttDictionaryBegin);
  ReadDictionary(P, FTrailer.Dictionary);

  CheckExpectedToken(P, pttStartXRef);
  ReadTrailerStartXRef(P);
end;

procedure TCnPDFDocument.ReadXRef(P: TCnPDFParser);
var
  Num: TCnPDFNumberObject;
  Seg: TCnPDFXRefCollection;
  Item: TCnPDFXRefItem;
  C1, C2: Cardinal;
begin
  P.NextNoJunkNoCRLF;
  Num := TCnPDFNumberObject.Create;
  try
    Seg := nil;
    while P.TokenID = pttNumber do
    begin
      // 先要俩 Number
      CheckExpectedToken(P, pttNumber);
      ReadNumber(P, Num, False);
      C1 := Num.AsInteger;

      CheckExpectedToken(P, pttNumber);
      ReadNumber(P, Num, False); // 注意不要越过回车换行
      C2 := Num.AsInteger;

      if P.TokenID in [pttN,pttF, pttD] then
      begin
        // 如果是俩 Number 后 f n d 再回车，则是段内新条目
        if Seg <> nil then
        begin
          Item := Seg.Add;
          Item.ObjectOffset := C1;
          Item.ObjectGeneration := C2;
          Item.ObjectXRefType := XRefTokenToType(P.TokenID);
        end;
        P.NextNoJunkNoCRLF; // 要跳过换行
      end
      else if P.TokenID = pttLineBreak then
      begin
        // 如果是俩 Number 后回车，则是新段
        Seg := FXRefTable.AddSegment;
        Seg.ObjectIndex := C1;
        // 先不记录当前段的个数和后文比对

        P.NextNoJunk;       // 已经碰到换行了
      end;
    end;
  finally
    Num.Free;
  end;
end;

procedure TCnPDFDocument.SaveToFile(const FileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TCnPDFDocument.SaveToStream(Stream: TStream);
begin
  SyncTrailer;
  FBody.SyncPages;

  FHeader.WriteToStream(Stream);
  FBody.WriteToStream(Stream);

  FTrailer.XRefStart := Stream.Position;
  FXRefTable.WriteToStream(Stream);

  FTrailer.Dictionary.Values['Size'] := TCnPDFNumberObject.Create(FBody.Objects.MaxID + 1);
  FTrailer.WriteToStream(Stream);
end;

procedure TCnPDFDocument.ReadReference(P: TCnPDFParser;
  Ref: TCnPDFReferenceObject);
var
  Num: TCnPDFNumberObject;
  C1, C2: Cardinal;
begin
  CheckExpectedToken(P, pttNumber);

  Num := TCnPDFNumberObject.Create;
  try
    ReadNumber(P, Num);
    C1 := Num.AsInteger;

    CheckExpectedToken(P, pttNumber);
    ReadNumber(P, Num);
    C2 := Num.AsInteger;

    CheckExpectedToken(P, pttR);
    P.NextNoJunkNoCRLF;

    Ref.ID := C1;
    Ref.Generation := C2;
  finally
    Num.Free;
  end;
end;

procedure TCnPDFDocument.ArrangeObjects;
var
  I: Integer;
  Obj: TCnPDFObject;
  Arr: TCnPDFArrayObject;
  Page: TCnPDFDictionaryObject;
begin
  FBody.Objects.CalcMaxID;
  if FTrailer = nil then
    Exit;

  // 读加密对象，可以不存在
  Obj := FTrailer.Dictionary.Values['Encrypt'];
  if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
  begin
    FEncrypted := True;
    FDecrypted := False;
    Obj := FromReference(Obj as TCnPDFReferenceObject);
    if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) then
      FBody.Encrypt := Obj as TCnPDFDictionaryObject;
  end
  else
  begin
    FEncrypted := False;
    FDecrypted := True;
  end;

  // 找 Info 对象
  Obj := FTrailer.Dictionary.Values['Info'];
  if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
  begin
    Obj := FromReference(Obj as TCnPDFReferenceObject);
    if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) then
      FBody.Info := Obj as TCnPDFDictionaryObject;
  end;

  // 找 Catalog 对象
  Obj := FTrailer.Dictionary.Values['Root'];
  if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
  begin
    Obj := FromReference(Obj as TCnPDFReferenceObject);
    if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) then
    begin
      FBody.Catalog := Obj as TCnPDFDictionaryObject;
      if FBody.Catalog.GetType <> 'Catalog' then
        raise ECnPDFException.Create('Catalog Type Error');
    end;
  end;

  // 找 Pages 对象
  if FBody.Catalog <> nil then
  begin
    Obj := FBody.Catalog.Values['Pages'];
    if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
    begin
      Obj := FromReference(Obj as TCnPDFReferenceObject);
      if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) then
      begin
        FBody.Pages := Obj as TCnPDFDictionaryObject;
        if FBody.Pages.GetType <> 'Pages' then
          raise ECnPDFException.Create('Pages Type Error');
      end;
    end;
  end;

  // 找各个 Page
  if FBody.Pages <> nil then
  begin
    // 找 Page 数组
    Obj := FBody.Pages.Values['Kids'];
    if (Obj <> nil) and (Obj is TCnPDFArrayObject) then
    begin
      Arr := Obj as TCnPDFArrayObject;
      if Arr.Count > 0 then
      begin
        for I := 0 to Arr.Count - 1 do
        begin
          Obj := Arr.Items[I];
          if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
          begin
            Obj := FromReference(Obj as TCnPDFReferenceObject);
            if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) then
              FBody.AddRawPage(Obj as TCnPDFDictionaryObject);
          end;
        end;
      end;
    end
    else if Obj <> nil then
      raise ECnPDFException.CreateFmt('Error Object Type %s for Kids', [Obj.ClassName]);
  end;

  // 给每个 Page 找 Content 和 Resource 等
  for I := 0 to FBody.PageCount - 1 do
  begin
    Page := FBody.Page[I];

    // 找 Contents，可能是字典或数组
    Obj := Page.Values['Contents'];
    if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
    begin
      Obj := FromReference(Obj as TCnPDFReferenceObject);
      if (Obj <> nil) and (Obj is TCnPDFStreamObject) then
        FBody.AddRawContent(Obj as TCnPDFStreamObject);
    end
    else if (Obj <> nil) and (Obj is TCnPDFArrayObject) then
    begin
      Arr := Obj as TCnPDFArrayObject;
      if Arr.Count > 0 then
      begin
        Obj := Arr.Items[0];
        if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
        begin
          Obj := FromReference(Obj as TCnPDFReferenceObject);
          if (Obj <> nil) and (Obj is TCnPDFStreamObject) then
            FBody.AddRawContent(Obj as TCnPDFStreamObject);
        end;
      end
    end
    else if Obj <> nil then
      raise ECnPDFException.CreateFmt('Error Object Type %s for Contents', [Obj.ClassName]);

    // 找 Resources，可以不是引用对象而是直接字典
    Obj := Page.Values['Resources'];
    if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) then
      FBody.AddRawResource(Obj as TCnPDFDictionaryObject)
    else if (Obj <> nil) and (Obj is TCnPDFReferenceObject) then
    begin
      Obj := FromReference(Obj as TCnPDFReferenceObject);
      if (Obj <> nil) and (Obj is TCnPDFDictionaryObject) then
        FBody.AddRawResource(Obj as TCnPDFDictionaryObject);
    end
    else if Obj <> nil then
      raise ECnPDFException.CreateFmt('Error Object Type %s for Resources', [Obj.ClassName]);
  end;
end;

function TCnPDFDocument.FromReference(Ref: TCnPDFReferenceObject): TCnPDFObject;
begin
  Result := nil;
  if Ref <> nil then
    Result := FBody.Objects.GetObjectByIDGeneration(Ref.ID, Ref.Generation);
end;

procedure TCnPDFDocument.XRefDictToXRefTable(Dict: TCnPDFDictionaryObject);
begin
  if Dict = nil then
    Exit;

  if FTrailer.Dictionary.Count = 0 then // 先把 Info 等内容塞过去
    FTrailer.Dictionary.Assign(Dict);
end;

procedure TCnPDFDocument.SyncTrailer;
begin
  if FTrailer.Dictionary.Values['Info'] = nil then
    FTrailer.Dictionary.Values['Info'] := TCnPDFReferenceObject.Create(FBody.Info);
  if FTrailer.Dictionary.Values['Root'] = nil then
    FTrailer.Dictionary.Values['Root'] := TCnPDFReferenceObject.Create(FBody.Catalog);
end;

procedure TCnPDFDocument.UncompressObjects;
{$IFNDEF NO_ZLIB}
var
  I: Integer;
{$ENDIF}
begin
{$IFNDEF NO_ZLIB}
  for I := 0 to FBody.Objects.Count - 1 do
  begin
    if FBody.Objects[I] is TCnPDFStreamObject then
      (FBody.Objects[I] as TCnPDFStreamObject).Uncompress;
  end;
{$ENDIF}
end;

procedure TCnPDFDocument.DumpToStrings(Strings: TStrings);
var
  I: Integer;
begin
  // 输出文件内的原始内容
  FHeader.DumpToStrings(Strings);
  FBody.DumpToStrings(Strings, True);
  FXRefTable.DumpToStrings(Strings);
  FTrailer.DumpToStrings(Strings);

  Strings.Add('');
  Strings.Add('==============================');
  Strings.Add('');

  // 输出 Info、Catalog、Pages 等分析后的对象的内容
  if FEncrypted then
  begin
    Strings.Add('Encrypted');
    if FDecrypted then
      Strings.Add('Succesfully Decrypted')
    else
      Strings.Add('NOT Decrypted');

    if FBody.Encrypt <> nil then
      FBody.Encrypt.ToStrings(Strings);
  end
  else
    Strings.Add('NOT Encrypted');

  Strings.Add('--- Info ---') ;
  if FBody.Info <> nil then
    FBody.Info.ToStrings(Strings);

  Strings.Add('--- Catalog ---') ;
  if FBody.Catalog <> nil then
    FBody.Catalog.ToStrings(Strings);

  Strings.Add('--- Pages ---') ;
  if FBody.Pages <> nil then
    FBody.Pages.ToStrings(Strings);

  Strings.Add('--- Page List ---') ;
  for I := 0 to FBody.PageCount - 1 do
    FBody.Page[I].ToStrings(Strings);

  Strings.Add('--- Content List ---') ;
  for I := 0 to FBody.ContentCount - 1 do
    FBody.Content[I].ToStrings(Strings);

  Strings.Add('--- Resource List ---') ;
  for I := 0 to FBody.ResourceCount - 1 do
    FBody.Resource[I].ToStrings(Strings);
end;

procedure TCnPDFDocument.Decrypt(const APass: Ansistring);
var
  I, Ver, Rev, KBL: Integer;
  Key: TBytes;
  Cryptor: TCnPDFDataCryptor;
  V: TCnPDFObject;
  CFM: string;
begin
  if not FEncrypted or FDecrypted or (FBody.Encrypt = nil) then
    Exit;

  // 只支持标准处理器
  V := FBody.Encrypt.Values['Filter'];
  if V is TCnPDFNameObject then
  begin
    if (V as TCnPDFNameObject).Name <> 'Standard' then
      raise ECnPDFCryptException.CreateFmt(SCnErrorPDFEncryptNOTSupportFmt,
        [string((V as TCnPDFNameObject).Name)]);
  end
  else
    raise ECnPDFCryptException.CreateFmt(SCnErrorPDFEncryptNOTSupportFmt, ['Unknown']);

  // 尝试用户密码及空密码，注意这一步和 V 值无关
  if not CheckUserPassword(APass, Key) then
    if not CheckUserPassword('', Key) then
      if not CheckOwnerPassword(APass, Key) then
        raise ECnPDFException.Create(SCnErrorPDFEncryptPassword);

  Rev := 0;
  V := FBody.Encrypt.Values['R'];
  if V is TCnPDFNumberObject then
    Rev := (V as TCnPDFNumberObject).AsInteger;

  KBL := 0;
  V := FBody.Encrypt.Values['Length'];
  if V is TCnPDFNumberObject then
    KBL := (V as TCnPDFNumberObject).AsInteger;

  Ver := 0;
  V := FBody.Encrypt.Values['V'];
  if V is TCnPDFNumberObject then
    Ver := (V as TCnPDFNumberObject).AsInteger;

  CFM := '';
  V := FBody.Encrypt.Values['CF'];
  if V is TCnPDFDictionaryObject then
  begin
    V := (V as TCnPDFDictionaryObject).Values['StdCF'];
    if V is TCnPDFDictionaryObject then
    begin
      V := (V as TCnPDFDictionaryObject).Values['CFM'];
      if V is TCnPDFNameObject then
        CFM := string((V as TCnPDFNameObject).Name);
    end;
  end;

  // 读入后解密的场合，从文件内容各参数里获取加密算法种类
  FEncryptionMethod := CnPDFFindEncryptionMethod(Ver, Rev, KBL, CFM);
  if FEncryptionMethod = cpemNotSupport then
    raise ECnPDFException.Create(SCnErrorPDFEncryptNOTSupport);

  // 得到解密密钥 Key，准备解密所有的 String 与 Stream
  Cryptor := TCnPDFDataCryptor.Create(FEncryptionMethod, Key, KBL);
  try
    for I := 0 to FBody.Objects.Count - 1 do
      DecryptObject(Cryptor, FBody.Objects[I], Key);
  finally
    Cryptor.Free;
  end;
  FDecrypted := True;

  UncompressObjects; // 解密成功后，顺便解压
end;

procedure TCnPDFDocument.Encrypt(const OwnerPass, UserPass: Ansistring);
var
  I, Ver, Rev, KBL: Integer;
  ID, O, U, Key: TBytes;
  V: TCnPDFObject;
  D: TCnPDFDictionaryObject;
  Cryptor: TCnPDFDataCryptor;
begin
  // 用 V4 R4 结合 CF 的 CTM V2 计算密码并加密，再设置 Encryt 字段
  Ver := 4;
  Rev := 4;
  KBL := 128;

  if FEncryptionMethod = cpem40RC4 then
  begin
    Ver := 1;
    Rev := 2;
    KBL := 0;
  end
  else if FEncryptionMethod = cpem128RC4 then
  begin
    Ver := 4;
    Rev := 4;
    KBL := 128;
  end
  else if FEncryptionMethod = cpem128AES then
  begin
    Ver := 4;
    Rev := 4;
    KBL := 128;
  end;

  ID := nil;
  V := FTrailer.Dictionary.Values['ID'];
  if V is TCnPDFArrayObject then
  begin
    if (V as TCnPDFArrayObject).Count > 1 then
    begin
      V := (V as TCnPDFArrayObject).Items[0];
      if V is TCnPDFStringObject then
        ID := (V as TCnPDFStringObject).Content;  // 引用
    end;
  end;

  O := CnPDFCalcOwnerCipher(OwnerPass, UserPass, Ver, Rev, KBL);
  U := CnPDFCalcUserCipher(UserPass, Ver, Rev, O, FPermission, ID, KBL);

  Key := CnPDFCalcEncryptKey(UserPass, Ver, Rev, O, FPermission, ID, KBL);

  // 得到加密密钥 Key，准备加密所有的 String 与 Stream
  Cryptor := TCnPDFDataCryptor.Create(FEncryptionMethod, Key, KBL);
  try
    for I := 0 to FBody.Objects.Count - 1 do
      if FBody.Objects[I] <> FBody.Encrypt then // 不要加密 Encrypt 对象（如果存在的话）
        EncryptObject(Cryptor, FBody.Objects[I], Key);
  finally
    Cryptor.Free;
  end;

  // 确保创建并组装 Encrypt 字典对象
  FBody.CreateEncrypt;
  FBody.Encrypt.Clear;

  D := FBody.Encrypt.AddDictionary('CF');
  D := D.AddDictionary('StdCF');
  D.AddName('AuthEvent', 'DocOpen');
  if FEncryptionMethod = cpem128RC4 then
    D.AddName('CFM', 'V2')
  else if FEncryptionMethod = cpem128AES then
    D.AddName('CFM', 'AESV2');
  D.AddNumber('Length', KBL);

  FBody.Encrypt.AddTrue('EncryptMetadata');
  FBody.Encrypt.AddName('Filter', 'Standard');
  FBody.Encrypt.AddNumber('Length', KBL);
  FBody.Encrypt.AddNumber('P', Integer(FPermission));
  FBody.Encrypt.AddNumber('V', Ver);
  FBody.Encrypt.AddNumber('R', Rev);
  FBody.Encrypt.AddName('StmF', 'StdCF');
  FBody.Encrypt.AddName('StrF', 'StdCF');

  // 两行密文可能有转义，内部已处理
  FBody.Encrypt.AddAnsiString('O', BytesToAnsi(O));
  FBody.Encrypt.AddAnsiString('U', BytesToAnsi(U));

  FTrailer.Dictionary.Values['Encrypt'] := TCnPDFReferenceObject.Create(FBody.Encrypt);

  FEncrypted := True;
  FDecrypted := False;
end;

function TCnPDFDocument.CheckUserPassword(const APass: AnsiString;
  out Key: TBytes): Boolean;
var
  OC, UC, ID: TBytes;
  Per: Cardinal;
  Ver, Rev, KBL: Integer;
  V: TCnPDFObject;
begin
  Result := False;
  Key := nil;

  if not FEncrypted or FDecrypted or (FBody.Encrypt = nil) then
    Exit;

  OC := nil;
  UC := nil;
  ID := nil;
  Per := 0;
  Ver := 0;
  Rev := 0;
  KBL := 0;

  // 从 Encrypt 字典中读出内容，与用户输入的密码运算，再和 U 比对
  V := FBody.Encrypt.Values['P'];
  if V is TCnPDFNumberObject then
    Per := Cardinal((V as TCnPDFNumberObject).AsInteger);

  V := FBody.Encrypt.Values['V'];
  if V is TCnPDFNumberObject then
    Ver := (V as TCnPDFNumberObject).AsInteger;

  V := FBody.Encrypt.Values['R'];
  if V is TCnPDFNumberObject then
    Rev := (V as TCnPDFNumberObject).AsInteger;

  V := FBody.Encrypt.Values['Length'];
  if V is TCnPDFNumberObject then
    KBL := (V as TCnPDFNumberObject).AsInteger;

  V := FBody.Encrypt.Values['O'];
  if V is TCnPDFStringObject then
    OC := (V as TCnPDFStringObject).Content;   // 引用

  V := FBody.Encrypt.Values['U'];
  if V is TCnPDFStringObject then
    UC := (V as TCnPDFStringObject).Content;   // 引用

  V := FTrailer.Dictionary.Values['ID'];
  if V is TCnPDFArrayObject then
  begin
    if (V as TCnPDFArrayObject).Count > 1 then
    begin
      V := (V as TCnPDFArrayObject).Items[0];
      if V is TCnPDFStringObject then
        ID := (V as TCnPDFStringObject).Content;  // 引用
    end;
  end;

  Key := CnPDFCheckUserPassword(APass, Ver, Rev, OC, UC, Per, ID, KBL);
  Result := Length(Key) > 0;
end;

function TCnPDFDocument.CheckOwnerPassword(const APass: AnsiString;
  out Key: TBytes): Boolean;
var
  OC, UC, ID: TBytes;
  Per: Cardinal;
  Ver, Rev, KBL: Integer;
  V: TCnPDFObject;
begin
  Result := False;
  Key := nil;

  if not FEncrypted or FDecrypted or (FBody.Encrypt = nil) then
    Exit;

  OC := nil;
  UC := nil;
  ID := nil;
  Per := 0;
  Ver := 0;
  Rev := 0;
  KBL := 0;

  // 从 Encrypt 字典中读出内容，与用户输入的密码运算，再和 O 比对
  V := FBody.Encrypt.Values['P'];
  if V is TCnPDFNumberObject then
    Per := Cardinal((V as TCnPDFNumberObject).AsInteger);

  V := FBody.Encrypt.Values['V'];
  if V is TCnPDFNumberObject then
    Ver := (V as TCnPDFNumberObject).AsInteger;

  V := FBody.Encrypt.Values['R'];
  if V is TCnPDFNumberObject then
    Rev := (V as TCnPDFNumberObject).AsInteger;

  V := FBody.Encrypt.Values['Length'];
  if V is TCnPDFNumberObject then
    KBL := (V as TCnPDFNumberObject).AsInteger;

  V := FBody.Encrypt.Values['O'];
  if V is TCnPDFStringObject then
    OC := (V as TCnPDFStringObject).Content;   // 引用

  V := FBody.Encrypt.Values['U'];
  if V is TCnPDFStringObject then
    UC := (V as TCnPDFStringObject).Content;   // 引用

  V := FTrailer.Dictionary.Values['ID'];
  if V is TCnPDFArrayObject then
  begin
    if (V as TCnPDFArrayObject).Count > 1 then
    begin
      V := (V as TCnPDFArrayObject).Items[0];
      if V is TCnPDFStringObject then
        ID := (V as TCnPDFStringObject).Content;  // 引用
    end;
  end;

  Key := CnPDFCheckOwnerPassword(APass, Ver, Rev, OC, UC, Per, ID, KBL);
  Result := Length(Key) > 0;
end;

procedure TCnPDFDocument.DecryptObject(Cryptor: TCnPDFDataCryptor;
  Obj: TCnPDFObject; Key: TBytes);
var
  I: Integer;
begin
  if (Obj = nil) or (Obj = FBody.Encrypt) then // 加密对象不解密，也不包括 Trailer 内的字典
    Exit;

  if Obj is TCnPDFStringObject then
    DecryptString(Cryptor, (Obj as TCnPDFStringObject), Key)
  else if Obj is TCnPDFStreamObject then
    DecryptStream(Cryptor, (Obj as TCnPDFStreamObject), Key)
  else if Obj is TCnPDFArrayObject then
  begin
    for I := 0 to (Obj as TCnPDFArrayObject).Count - 1 do
      DecryptObject(Cryptor, (Obj as TCnPDFArrayObject).Items[I], Key);
  end
  else if Obj is TCnPDFDictionaryObject then
  begin
    for I := 0 to (Obj as TCnPDFDictionaryObject).Count - 1 do
      DecryptObject(Cryptor, (Obj as TCnPDFDictionaryObject).Pairs[I].Value, Key);
  end;
end;

procedure TCnPDFDocument.DecryptStream(Cryptor: TCnPDFDataCryptor;
  Stream: TCnPDFStreamObject; Key: TBytes);
var
  ID, Gen: Cardinal;
begin
  GetCryptIDGen(Stream, ID, Gen);
  Cryptor.Decrypt(Stream.FStream, ID, Gen);
end;

procedure TCnPDFDocument.DecryptString(Cryptor: TCnPDFDataCryptor;
  Str: TCnPDFStringObject; Key: TBytes);
var
  ID, Gen: Cardinal;
begin
  GetCryptIDGen(Str, ID, Gen);
  Cryptor.Decrypt(Str.FContent, ID, Gen);
end;

procedure TCnPDFDocument.EncryptObject(Cryptor: TCnPDFDataCryptor;
  Obj: TCnPDFObject; Key: TBytes);
var
  I: Integer;
begin
  if (Obj = nil) or (Obj = FBody.Encrypt) then // 加密对象不加密，也不包括 Trailer 内的字典
    Exit;

  if Obj is TCnPDFStringObject then
    EncryptString(Cryptor, (Obj as TCnPDFStringObject), Key)
  else if Obj is TCnPDFStreamObject then
    EncryptStream(Cryptor, (Obj as TCnPDFStreamObject), Key)
  else if Obj is TCnPDFArrayObject then
  begin
    for I := 0 to (Obj as TCnPDFArrayObject).Count - 1 do
      EncryptObject(Cryptor, (Obj as TCnPDFArrayObject).Items[I], Key);
  end
  else if Obj is TCnPDFDictionaryObject then
  begin
    for I := 0 to (Obj as TCnPDFDictionaryObject).Count - 1 do
      EncryptObject(Cryptor, (Obj as TCnPDFDictionaryObject).Pairs[I].Value, Key);
  end;
end;

procedure TCnPDFDocument.EncryptStream(Cryptor: TCnPDFDataCryptor;
  Stream: TCnPDFStreamObject; Key: TBytes);
var
  ID, Gen: Cardinal;
begin
  GetCryptIDGen(Stream, ID, Gen);
  Cryptor.Encrypt(Stream.FStream, ID, Gen);
end;

procedure TCnPDFDocument.EncryptString(Cryptor: TCnPDFDataCryptor;
  Str: TCnPDFStringObject; Key: TBytes);
var
  ID, Gen: Cardinal;
begin
  GetCryptIDGen(Str, ID, Gen);
  Str.RemoveEscape;
  Cryptor.Encrypt(Str.FContent, ID, Gen); // 加密前要去除转义，以原始内容加密，
  Str.AddEscape; // 并针对密文进行转义
end;

procedure TCnPDFDocument.GetCryptIDGen(Obj: TCnPDFObject; out AID,
  AGen: Cardinal);
begin
  AID := Obj.ID;
  AGen := Obj.Generation;

  if (AID = 0) and (Obj.Parent <> nil) then
  begin
    AID := Obj.Parent.ID;
    AGen := Obj.Parent.Generation;
  end;
end;

function TCnPDFDocument.GetNeedPassword: Boolean;
var
  Key: TBytes;
begin
  Result := FEncrypted and not CheckUserPassword('', Key);
end;

{ TCnPDFDictPair }

procedure TCnPDFDictPair.Assign(Source: TPersistent);
begin
  if Source is TCnPDFDictPair then
  begin
    FName.Assign((Source as TCnPDFDictPair).Name); // Name 对象总是固定持有

    FreeAndNil(FValue);
    Value := (Source as TCnPDFDictPair).Value.Clone; // 内部会设 Parent
  end
  else
    inherited;
end;

procedure TCnPDFDictPair.ChangeToArray;
var
  Arr: TCnPDFArrayObject;
begin
  if not (FValue is TCnPDFArrayObject) then
  begin
    Arr := TCnPDFArrayObject.Create;
    if FValue <> nil then
      Arr.AddObject(FValue);

    Value := Arr; // 会保留 FValue 对象，且 Arr 的 Parent 设为所属的字典
  end;
end;

constructor TCnPDFDictPair.Create(const Name: string; Dict: TCnPDFDictionaryObject);
begin
  inherited Create;
  FName := TCnPDFNameObject.Create(Name);
  FDictionary := Dict;
  FName.Parent := FDictionary;
end;

destructor TCnPDFDictPair.Destroy;
begin
  FName.Free;
  FValue.Free; // 如果外界没设置，则为 nil，不影响
  inherited;
end;

procedure TCnPDFDictPair.SetValue(const Value: TCnPDFObject);
begin
  if FValue <> Value then // 注意不释放原有 Value
  begin
    FValue := Value;
    FValue.Parent := FDictionary;
  end;
end;

function TCnPDFDictPair.WriteToStream(Stream: TStream): Cardinal;
begin
  Result := WriteString(Stream, CRLF);
  Inc(Result, FName.WriteToStream(Stream));
  Inc(Result, WriteSpace(Stream));
  if FValue <> nil then
    Inc(Result, FValue.WriteToStream(Stream));
end;

{ TCnPDFNameObject }

function TCnPDFNameObject.GetName: AnsiString;
begin
  Result := BytesToAnsi(FContent);
end;

function TCnPDFNameObject.WriteToStream(Stream: TStream): Cardinal;
begin
  Result := WriteString(Stream, '/' + BytesToAnsi(Content));
end;

{ TCnPDFDictionaryObject }

function TCnPDFDictionaryObject.AddAnsiString(const Name: string;
  const Value: AnsiString): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFStringObject.Create(Value);
end;

function TCnPDFDictionaryObject.AddArray(const Name: string): TCnPDFArrayObject;
var
  Pair: TCnPDFDictPair;
begin
  Pair := AddName(Name);
  Result := TCnPDFArrayObject.Create;
  Pair.Value := Result;
end;

function TCnPDFDictionaryObject.AddDictionary(const Name: string): TCnPDFDictionaryObject;
var
  Pair: TCnPDFDictPair;
begin
  Pair := AddName(Name);
  Result := TCnPDFDictionaryObject.Create;
  Pair.Value := Result;
end;

function TCnPDFDictionaryObject.AddFalse(const Name: string): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFBooleanObject.Create(False);
end;

function TCnPDFDictionaryObject.AddName(const Name: string): TCnPDFDictPair;
begin
  Result := TCnPDFDictPair.Create(Name, Self);
  AddPair(Result);
end;

function TCnPDFDictionaryObject.AddName(const Name1,
  Name2: string): TCnPDFDictPair;
begin
  Result := TCnPDFDictPair.Create(Name1, Self);
  Result.Value := TCnPDFNameObject.Create(Name2);
  AddPair(Result);
end;

function TCnPDFDictionaryObject.AddNull(const Name: string): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFNullObject.Create;
end;

function TCnPDFDictionaryObject.AddNumber(const Name: string;
  Value: Int64): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFNumberObject.Create(Value);
end;

function TCnPDFDictionaryObject.AddNumber(const Name: string;
  Value: Integer): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFNumberObject.Create(Value);
end;

function TCnPDFDictionaryObject.AddNumber(const Name: string;
  Value: Extended): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFNumberObject.Create(Value);
end;

function TCnPDFDictionaryObject.AddObjectRef(const Name: string;
  Obj: TCnPDFObject): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFReferenceObject.Create(Obj);
end;

procedure TCnPDFDictionaryObject.AddPair(APair: TCnPDFDictPair);
begin
  FPairs.Add(APair);
end;

function TCnPDFDictionaryObject.AddString(const Name,
  Value: string): TCnPDFDictPair;
begin
{$IFDEF UNICODE}
  Result := AddUnicodeString(Name, Value);
{$ELSE}
  Result := AddAnsiString(Name, Value);
{$ENDIF}
end;

function TCnPDFDictionaryObject.AddTrue(const Name: string): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFBooleanObject.Create(True);
end;

{$IFDEF UNICODE}

function TCnPDFDictionaryObject.AddUnicodeString(const Name,
  Value: string): TCnPDFDictPair;
begin
  Result := AddName(Name);
  Result.Value := TCnPDFStringObject.Create(Value);
end;

{$ENDIF}

function TCnPDFDictionaryObject.AddWideString(const Name: string;
  const Value: WideString): TCnPDFDictPair;
begin
  Result := AddName(Name);
{$IFDEF COMPILER5}
  Result.Value := TCnPDFStringObject.CreateW(Value);
{$ELSE}
  Result.Value := TCnPDFStringObject.Create(Value);
{$ENDIF}
end;

procedure TCnPDFDictionaryObject.Assign(Source: TPersistent);
var
  I: Integer;
  Dict: TCnPDFDictionaryObject;
  Pair: TCnPDFDictPair;
begin
  if Source is TCnPDFDictionaryObject then
  begin
    Clear;

    Dict := Source as TCnPDFDictionaryObject;
    for I := 0 to Dict.Count - 1 do
    begin
      Pair := TCnPDFDictPair.Create('', Self);
      Pair.Assign(Dict.Pairs[I]);
      AddPair(Pair);
    end;
  end
  else
    inherited;
end;

procedure TCnPDFDictionaryObject.Clear;
begin
  FPairs.Clear;
end;

constructor TCnPDFDictionaryObject.Create;
begin
  inherited;
  FPairs := TObjectList.Create(True);
end;

procedure TCnPDFDictionaryObject.DeleteName(const Name: string);
var
  I: Integer;
  Pair: TCnPDFDictPair;
begin
  for I := FPairs.Count - 1 downto 0 do
  begin
    Pair := TCnPDFDictPair(FPairs[I]);
    if (Pair <> nil) and (Pair.Name.Name = Name) then
      FPairs.Delete(I);
  end;
end;

destructor TCnPDFDictionaryObject.Destroy;
begin
  FPairs.Free;
  inherited;
end;

function TCnPDFDictionaryObject.GetCount: Integer;
begin
  Result := FPairs.Count;
end;

procedure TCnPDFDictionaryObject.GetNames(Names: TStrings);
var
  I: Integer;
begin
  Names.Clear;
  for I := 0 to FPairs.Count - 1 do
    Names.Add(Pairs[I].Name.Name);
end;

function TCnPDFDictionaryObject.GetPair(Index: Integer): TCnPDFDictPair;
begin
  Result := TCnPDFDictPair(FPairs[Index]);
end;

function TCnPDFDictionaryObject.GetType: string;
var
  V: TCnPDFObject;
begin
  V := Values['Type'];
  if (V <> nil) and (V is TCnPDFNameObject) then
    Result := (V as TCnPDFNameObject).Name
  else
    Result := '';
end;

function TCnPDFDictionaryObject.GetValue(const Name: string): TCnPDFObject;
var
  Idx: Integer;
begin
  Idx := IndexOfName(Name);
  if Idx >= 0 then
    Result := TCnPDFDictPair(FPairs[Idx]).Value
  else
    Result := nil;
end;

function TCnPDFDictionaryObject.HasName(const Name: string): Boolean;
begin
  Result := IndexOfName(Name) >= 0;
end;

function TCnPDFDictionaryObject.IndexOfName(const Name: string): Integer;
var
  I: Integer;
  Pair: TCnPDFDictPair;
  S: string;
begin
  for I := 0 to FPairs.Count - 1 do
  begin
    Pair := TCnPDFDictPair(FPairs[I]);
    S := string(BytesToAnsi(Pair.Name.Content));

    if S = Name then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TCnPDFDictionaryObject.SetValue(const Name: string;
  const Value: TCnPDFObject);
var
  Idx: Integer;
  Pair: TCnPDFDictPair;
begin
  Idx := IndexOfName(Name);
  if Idx >= 0 then
  begin
    if TCnPDFDictPair(FPairs[Idx]).Value <> nil then
      TCnPDFDictPair(FPairs[Idx]).Value.Free;
    TCnPDFDictPair(FPairs[Idx]).Value := Value;
  end
  else
  begin
    Pair := AddName(Name);
    Pair.Value := Value;
  end;
end;

function TCnPDFDictionaryObject.ToString: string;
begin
  Result := Format('<<...Count %d...>>', [Count]);
end;

procedure TCnPDFDictionaryObject.ToStrings(Strings: TStrings;
  Indent: Integer);
var
  I: Integer;
  S1, S2: string;
  Pair: TCnPDFDictPair;
begin
  S1 := StringOfChar(' ', Indent);
  Strings.Add(S1 + '<<');
  if Count > 0 then
  begin
    S2 := StringOfChar(' ', Indent + INDENTDELTA);
    for I := 0 to Count - 1 do
    begin
      Pair := Pairs[I];
      if Pair.Value is TCnPDFDictionaryObject then
      begin
        Strings.Add(S2 + Pair.Name.Name + ': <<...Count ' + IntToStr((Pair.Value as TCnPDFDictionaryObject).Count) + '...>>');
        (Pair.Value as TCnPDFDictionaryObject).ToStrings(Strings, Indent + INDENTDELTA);
      end
      else if Pair.Value is TCnPDFArrayObject then
      begin
        Strings.Add(S2 + Pair.Name.Name + ': [...Count ' + IntToStr((Pair.Value as TCnPDFArrayObject).Count) + '...]');
        (Pair.Value as TCnPDFArrayObject).ToStrings(Strings, Indent + INDENTDELTA);
      end
      else if Pair.Value <> nil then
        Strings.Add(S2 + Pair.Name.Name + ': ' + Pair.Value.ToString)
      else
        Strings.Add(S2 + Pair.Name.Name);
    end;
  end;
  Strings.Add(S1 + '>>');
end;

function TCnPDFDictionaryObject.WriteDictionary(Stream: TStream): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  Inc(Result, CheckWriteObjectStart(Stream));
  if FPairs.Count <= 0 then
  begin
    Inc(Result, WriteString(Stream, '<<>>'));
    Inc(Result, WriteCRLF(Stream));
    Exit;
  end;

  Inc(Result, WriteString(Stream, '<<'));
  for I := 0 to FPairs.Count - 1 do
  begin
    Inc(Result, (FPairs[I] as TCnPDFDictPair).WriteToStream(Stream));
    // Inc(Result, WriteCRLF(Stream));
  end;
  Inc(Result, WriteCRLF(Stream));
  Inc(Result, WriteLine(Stream, '>>'));
end;

function TCnPDFDictionaryObject.WriteToStream(Stream: TStream): Cardinal;
begin
  Result := WriteDictionary(Stream);
  Inc(Result, CheckWriteObjectEnd(Stream));
end;

{ TCnPDFArrayObject }

procedure TCnPDFArrayObject.AddAnsiString(const Value: AnsiString);
begin
  AddObject(TCnPDFStringObject.Create(Value));
end;

procedure TCnPDFArrayObject.AddFalse;
begin
  AddObject(TCnPDFBooleanObject.Create(False));
end;

procedure TCnPDFArrayObject.AddNull;
begin
  AddObject(TCnPDFNullObject.Create);
end;

procedure TCnPDFArrayObject.AddNumber(Value: Extended);
begin
  AddObject(TCnPDFNumberObject.Create(Value));
end;

procedure TCnPDFArrayObject.AddNumber(Value: Integer);
begin
  AddObject(TCnPDFNumberObject.Create(Value));
end;

procedure TCnPDFArrayObject.AddNumber(Value: Int64);
begin
  AddObject(TCnPDFNumberObject.Create(Value));
end;

procedure TCnPDFArrayObject.AddObject(Obj: TCnPDFObject);
begin
  FElements.Add(Obj);
  Obj.Parent := Self;
end;

procedure TCnPDFArrayObject.AddObjectRef(Obj: TCnPDFObject);
begin
  AddObject(TCnPDFReferenceObject.Create(Obj));
end;

procedure TCnPDFArrayObject.AddTrue;
begin
  AddObject(TCnPDFBooleanObject.Create(True));
end;

{$IFDEF UNICODE}

procedure TCnPDFArrayObject.AddUnicodeString(const Value: string);
begin
  AddObject(TCnPDFStringObject.Create(Value));
end;

{$ENDIF}

procedure TCnPDFArrayObject.AddWideString(const Value: WideString);
begin
  AddObject(TCnPDFStringObject.Create(Value));
end;

procedure TCnPDFArrayObject.Assign(Source: TPersistent);
var
  I: Integer;
  Obj: TCnPDFObject;
  Arr: TCnPDFArrayObject;
begin
  if Source is TCnPDFArrayObject then
  begin
    Clear;

    Arr := Source as TCnPDFArrayObject;
    for I := 0 to Arr.Count - 1 do
    begin
      Obj := Arr.Items[I];
      if Obj <> nil then
        Obj := Obj.Clone;

      AddObject(Obj);
    end;
  end
  else
    inherited;
end;

procedure TCnPDFArrayObject.Clear;
begin
  FElements.Clear;
end;

constructor TCnPDFArrayObject.Create;
begin
  inherited;
  FElements := TObjectList.Create(True);
end;

destructor TCnPDFArrayObject.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TCnPDFArrayObject.GetCount: Integer;
begin
  Result := FElements.Count;
end;

function TCnPDFArrayObject.GetItem(Index: Integer): TCnPDFObject;
begin
  Result := TCnPDFObject(FElements[Index]);
end;

function TCnPDFArrayObject.HasObjectRef(Obj: TCnPDFObject): Boolean;
var
  I: Integer;
  Ref: TCnPDFReferenceObject;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[I] is TCnPDFReferenceObject then
    begin
      Ref := Items[I] as TCnPDFReferenceObject;
      if Ref.IsReference(Obj) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

procedure TCnPDFArrayObject.SetItem(Index: Integer;
  const Value: TCnPDFObject);
begin
  FElements[Index] := Value;
end;

function TCnPDFArrayObject.ToString: string;
begin
  Result := Format('[...Count %d...]', [Count]);
end;

procedure TCnPDFArrayObject.ToStrings(Strings: TStrings; Indent: Integer);
var
  I: Integer;
  S1, S2: string;
  V: TCnPDFObject;
begin
  S1 := StringOfChar(' ', Indent);
  Strings.Add(S1 + '[');
  if Count > 0 then
  begin
    S2 := StringOfChar(' ', Indent + INDENTDELTA);
    for I := 0 to Count - 1 do
    begin
      V := Items[I];
      if V <> nil then
        V.ToStrings(Strings, Indent + INDENTDELTA);
    end;
  end;
  Strings.Add(S1 + ']');
end;

function TCnPDFArrayObject.WriteToStream(Stream: TStream): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  Inc(Result, CheckWriteObjectStart(Stream));
  Inc(Result, WriteString(Stream, '['));
  for I := 0 to FElements.Count - 1 do
  begin
    Inc(Result, (FElements[I] as TCnPDFObject).WriteToStream(Stream));
    if I < FElements.Count - 1 then
      Inc(Result, WriteSpace(Stream));
  end;
  Inc(Result, WriteString(Stream, ']'));
  Inc(Result, CheckWriteObjectEnd(Stream));
end;

{ TCnPDFSimpleObject }

constructor TCnPDFSimpleObject.Create(const AContent: AnsiString);
begin
  inherited Create;
  FContent := AnsiToBytes(AContent);
end;

procedure TCnPDFSimpleObject.Assign(Source: TPersistent);
begin
  if Source is TCnPDFSimpleObject then
  begin
    SetLength(FContent, Length((Source as TCnPDFSimpleObject).Content));
    if Length(FContent) > 0 then
      Move((Source as TCnPDFSimpleObject).Content[0], FContent[0], Length(FContent));
  end
  else
    inherited;
end;

constructor TCnPDFSimpleObject.Create(const Data: TBytes);
begin
  inherited Create;
  if Length(Data) > 0 then
    FContent := NewBytesFromMemory(@Data[0], Length(Data));
end;

function TCnPDFSimpleObject.WriteToStream(Stream: TStream): Cardinal;
begin
  Result := 0;
  Inc(Result, CheckWriteObjectStart(Stream));
  Inc(Result, WriteBytes(Stream, Content));
  Inc(Result, CheckWriteObjectEnd(Stream));
end;

function TCnPDFSimpleObject.ToString: string;
begin
  Result := BytesToString(FContent);
end;

{ TCnPDFReferenceObject }

procedure TCnPDFReferenceObject.Assign(Source: TPersistent);
begin
  if Source is TCnPDFReferenceObject then
  begin
    FReference := (Source as TCnPDFReferenceObject).Reference;
  end
  else
    inherited;
end;

constructor TCnPDFReferenceObject.Create(Obj: TCnPDFObject);
begin
  inherited Create('');
  Reference := Obj;
end;

destructor TCnPDFReferenceObject.Destroy;
begin

  inherited;
end;

function TCnPDFReferenceObject.IsReference(Obj: TCnPDFObject): Boolean;
begin
  Result := False;
  if Obj <> nil then
  begin
    if (FID = Obj.ID) and (FGeneration = Obj.Generation) then
      Result := True;
  end;
end;

procedure TCnPDFReferenceObject.SetReference(const Value: TCnPDFObject);
begin
  FReference := Value;
  if FReference = nil then
  begin
    FID := 0;
    FGeneration := 0;
  end
  else
  begin
    FID := FReference.ID;
    FGeneration := FReference.Generation;
  end;
end;

function TCnPDFReferenceObject.ToString: string;
begin
  Result := Format('%d %d R', [ID, Generation]);
end;

function TCnPDFReferenceObject.WriteToStream(Stream: TStream): Cardinal;
begin
  Result := 0;

  Inc(Result, WriteString(Stream, IntToStr(FID)));
  Inc(Result, WriteSpace(Stream));
  Inc(Result, WriteString(Stream, IntToStr(FGeneration)));
  Inc(Result, WriteSpace(Stream));
  Inc(Result, WriteString(Stream, 'R'));
end;

{ TCnPDFBooleanObject }

constructor TCnPDFBooleanObject.Create(IsTrue: Boolean);
begin
  if IsTrue then
    inherited Create('true')
  else
    inherited Create('false');
end;

{ TCnPDFNullObject }

constructor TCnPDFNullObject.Create;
begin
  inherited Create('null');
end;

{ TCnPDFStringObject }

constructor TCnPDFStringObject.Create(const AnsiStr: AnsiString);
begin
  inherited Create(AnsiStr);
  AddEscape;
end;

{$IFDEF COMPILER5}

constructor TCnPDFStringObject.CreateW(const WideStr: WideString);
begin
  if Length(WideStr) > 0 then
  begin
    SetLength(FContent, Length(WideStr) * SizeOf(WideChar) + 2);
    Move(SCN_BOM_UTF16_LE[0], FContent[0], SizeOf(SCN_BOM_UTF16_LE));
    Move(WideStr[1], FContent[2], Length(WideStr) * SizeOf(WideChar));
  end;
end;

{$ELSE}

constructor TCnPDFStringObject.Create(const WideStr: WideString);
begin
  if Length(WideStr) > 0 then
  begin
    SetLength(FContent, Length(WideStr) * SizeOf(WideChar) + 2);
    Move(SCN_BOM_UTF16_LE[0], FContent[0], SizeOf(SCN_BOM_UTF16_LE));
    Move(WideStr[1], FContent[2], Length(WideStr) * SizeOf(WideChar));
  end;
end;

{$ENDIF}

{$IFDEF UNICODE}

constructor TCnPDFStringObject.Create(const UnicodeStr: string);
begin
  if Length(UnicodeStr) > 0 then
  begin
    SetLength(FContent, Length(UnicodeStr) * SizeOf(WideChar) + 2);
    Move(SCN_BOM_UTF16_LE[0], FContent[0], SizeOf(SCN_BOM_UTF16_LE));
    Move(UnicodeStr[1], FContent[2], Length(UnicodeStr) * SizeOf(WideChar));
  end;
end;

{$ENDIF}

function TCnPDFStringObject.WriteToStream(Stream: TStream): Cardinal;
var
  S: string;
begin
  Result := 0;
  if FIsHex then
  begin
    Inc(Result, WriteString(Stream, '<'));
    S := BytesToHex(Content);
    Inc(Result, WriteString(Stream, S));
    Inc(Result, WriteString(Stream, '>'));
  end
  else
  begin
    Inc(Result, WriteString(Stream, '('));
    Inc(Result, WriteBytes(Stream, Content));
    Inc(Result, WriteString(Stream, ')'));
  end;
end;

procedure TCnPDFStringObject.AddEscape;
var
  I: Integer;
  SB: TCnStringBuilder;
begin
  if Length(FContent) = 0 then
    Exit;

  SB := TCnStringBuilder.Create(True);
  try
    for I := 0 to Length(FContent) - 1 do
    begin
      case FContent[I] of
        8:  SB.Append('\b');
        9:  SB.Append('\t');
        10: SB.Append('\n');
        12: SB.Append('\f');
        13: SB.Append('\r');
        40: SB.Append('\(');
        41: SB.Append('\)');
        92: SB.Append('\\');
      else
        SB.AppendAnsiChar(AnsiChar(FContent[I]));
      end;
    end;
    FContent := AnsiToBytes(SB.ToAnsiString);
  finally
    SB.Free;
  end;
end;

procedure TCnPDFStringObject.RemoveEscape;
var
  I: Integer;
  SB: TCnStringBuilder;
begin
  if Length(FContent) = 0 then
    Exit;

  I := 0;
  SB := TCnStringBuilder.Create(True);
  try
    while I < Length(FContent) do
    begin
      if FContent[I] = 92 then // 碰到 \ 后面转义
      begin
        Inc(I);
        if I = Length(FContent) then // \ 是最后一个字符，先输出再说
        begin
          SB.AppendAnsiChar(AnsiChar(FContent[I - 1]));
          Break;
        end
        else
        begin
          case FContent[I] of // 被转义的字符 btnfr()\
            98:  SB.AppendAnsiChar(AnsiChar(8));
            116: SB.AppendAnsiChar(AnsiChar(9));
            110: SB.AppendAnsiChar(AnsiChar(10));
            102: SB.AppendAnsiChar(AnsiChar(12));
            114: SB.AppendAnsiChar(AnsiChar(13));
            40:  SB.AppendAnsiChar(AnsiChar(40));
            41:  SB.AppendAnsiChar(AnsiChar(41));
            92:  SB.AppendAnsiChar(AnsiChar(92));
          else
            raise ECnPDFException.CreateFmt(SCnErrorPDFEscapeCharNOTSupportFmt, [FContent[I]]);
          end;
        end;
      end
      else
        SB.AppendAnsiChar(AnsiChar(FContent[I]));

      Inc(I);
    end;
    FContent := AnsiToBytes(SB.ToAnsiString);
  finally
    SB.Free;
  end;
end;

function TCnPDFStringObject.AsString: string;
begin
  Result := string(BytesToAnsi(FContent));
end;

{ TCnPDFStreamObject }

constructor TCnPDFStreamObject.Create;
begin
  inherited;

end;

destructor TCnPDFStreamObject.Destroy;
begin
  SetLength(FStream, 0);
  inherited;
end;

procedure TCnPDFStreamObject.ExtractStream(OutStream: TStream);
begin
  // TODO: 解压
end;

procedure TCnPDFStreamObject.SetJpegStream(JpegStream: TStream);
var
  S: Int64;
  J: TJPEGImage;
begin
  Clear;

  AddName('Type', 'XObject');
  AddName('Subtype', 'Image');
  AddNumber('BitsPerComponent', 8);
  AddName('ColorSpace', 'DeviceRGB');
  AddName('Filter', 'DCTDecode');

  J := TJPEGImage.Create;
  try
    J.LoadFromStream(JpegStream);
    AddNumber('Height', J.Height);
    AddNumber('Width', J.Width);
  finally
    J.Free;
  end;

  S := JpegStream.Size;
  JpegStream.Position := 0;
  AddNumber('Length', S);
  SetLength(FStream, S);

  JpegStream.Read(FStream[0], S);
end;

procedure TCnPDFStreamObject.SetJpegImage(const JpegFileName: string);
var
  F: TFileStream;
begin
  if FileExists(JpegFileName) then
  begin
    F := TFileStream.Create(JpegFileName, fmOpenRead or fmShareDenyWrite);
    try
      SetJpegStream(F);
    finally
      F.Free;
    end;
  end;
end;

{$IFNDEF NO_ZLIB}

procedure TCnPDFStreamObject.Compress;
var
  InS, OutS: TMemoryStream;
begin
  // 如果外界没指定支持压缩则不做
  if not FSupportCompress or (Length(FStream) <= 0) then
    Exit;

  Ins := nil;
  OutS := nil;

  try
    InS := TMemoryStream.Create;
    BytesToStream(FStream, InS);
    OutS := TMemoryStream.Create;

    CnZipCompressStream(InS, OutS);
    FStream := StreamToBytes(OutS);

    Values['Filter'] := TCnPDFNameObject.Create('FlateDecode');
  finally
    OutS.Free;
    InS.Free;
  end;
end;

procedure TCnPDFStreamObject.Uncompress;
var
  InS, OutS: TMemoryStream;
  V: TCnPDFObject;
begin
  if Length(FStream) <= 0 then
    Exit;

  // 不是标准 Zip 压缩则不做
  V := Values['Filter'];
  if (V <> nil) and (V is TCnPDFNameObject) then
  begin
    if (V as TCnPDFNameObject).Name = 'FlateDecode' then
      FSupportCompress := True
    else
      Exit;
  end
  else
    Exit;

  Ins := nil;
  OutS := nil;

  try
    InS := TMemoryStream.Create;
    BytesToStream(FStream, InS);
    OutS := TMemoryStream.Create;

    try
      InS.Position := 0;
      CnZipUncompressStream(InS, OutS);
      FStream := StreamToBytes(OutS);

      DeleteName('Filter'); // 解了就不需要这个 Filter 标记了
    except
      ;
    end;
  finally
    OutS.Free;
    InS.Free;
  end;
end;

{$ENDIF}

procedure TCnPDFStreamObject.SetStrings(Strings: TStrings);
var
  S: AnsiString;
begin
  S := Strings.Text;
  SetLength(FStream, Length(S));
  if Length(FStream) > 0 then
    Move(S[1], FStream[0], Length(FStream));
end;

procedure TCnPDFStreamObject.SyncLength;
begin
  Values['Length'] := TCnPDFNumberObject.Create(Length(FStream));
end;

function TCnPDFStreamObject.ToString: string;
var
  V: TCnPDFObject;
  S: string;
begin
  V := Values['Length'];
  S := '';
  if (V <> nil) and (V is TCnPDFNumberObject) then
    S := ' Length: ' + IntToStr((V as TCnPDFNumberObject).AsInteger);
  Result := inherited ToString + S + ' Stream Size: ' + IntToStr(Length(FStream));
end;

procedure TCnPDFStreamObject.ToStrings(Strings: TStrings; Indent: Integer);
begin
  Strings.Add(ToString);
  inherited ToStrings(Strings, Indent);
end;

function TCnPDFStreamObject.WriteToStream(Stream: TStream): Cardinal;
begin
  SyncLength;

  Result := WriteDictionary(Stream);
  Inc(Result, WriteLine(Stream, BEGINSTREAM));
  if Length(FStream) > 0 then
    Inc(Result, Stream.Write(FStream[0], Length(FStream)));

  Inc(Result, WriteCRLF(Stream));
  Inc(Result, WriteLine(Stream, ENDSTREAM));
  Inc(Result, CheckWriteObjectEnd(Stream));
end;

{ TCnPDFBody }

function TCnPDFBody.AddContent(Page: TCnPDFDictionaryObject): TCnPDFStreamObject;
begin
  Result := TCnPDFStreamObject.Create;
  FObjects.Add(Result);
  Page['Contents'] := TCnPDFReferenceObject.Create(Result);
end;

procedure TCnPDFBody.AddObject(Obj: TCnPDFObject);
begin
  FObjects.Add(Obj);
end;

function TCnPDFBody.AddPage: TCnPDFDictionaryObject;
begin
  Result := TCnPDFDictionaryObject.Create;
  Result['Parent'] := TCnPDFReferenceObject.Create(FPages);
  Result['Resources'] := TCnPDFDictionaryObject.Create;

  FObjects.Add(Result);
  FPageList.Add(Result);
end;

procedure TCnPDFBody.AddRawContent(AContent: TCnPDFStreamObject);
begin
  FContentList.Add(AContent);
end;

procedure TCnPDFBody.AddRawPage(APage: TCnPDFDictionaryObject);
begin
  FPageList.Add(APage);
end;

procedure TCnPDFBody.AddRawResource(AResource: TCnPDFDictionaryObject);
begin
  FResourceList.Add(AResource);
end;

function TCnPDFBody.AddResource(Page: TCnPDFDictionaryObject): TCnPDFDictionaryObject;
begin
  Result := TCnPDFDictionaryObject.Create;
  FObjects.Add(Result);
  Page['Resources'] := TCnPDFReferenceObject.Create(Result);
end;

constructor TCnPDFBody.Create;
begin
  inherited;
  FObjects := TCnPDFObjectManager.Create;

  FPageList := TObjectList.Create(False);
  FContentList := TObjectList.Create(False);
  FResourceList := TObjectList.Create(False);
end;

procedure TCnPDFBody.CreateEncrypt;
begin
  if FEncrypt = nil then
  begin
    FEncrypt := TCnPDFDictionaryObject.Create;
    FObjects.Add(FEncrypt);
  end;
end;

procedure TCnPDFBody.CreateResources;
begin
  if FInfo = nil then
  begin
    FInfo := TCnPDFDictionaryObject.Create;
    FObjects.Add(FInfo);
  end;

  if FCatalog = nil then
  begin
    FCatalog := TCnPDFDictionaryObject.Create;
    FCatalog.AddName('Type', 'Catalog');
    FObjects.Add(FCatalog);
  end;

  if FPages = nil then
  begin
    FPages := TCnPDFDictionaryObject.Create;
    FPages.AddName('Type', 'Pages');
    FObjects.Add(FPages);
  end;

  FPages.AddArray('Kids');
  FCatalog.AddObjectRef('Pages', FPages);
end;

destructor TCnPDFBody.Destroy;
begin
  FResourceList.Free;
  FContentList.Free;
  FPageList.Free;

  FObjects.Free;
  inherited;
end;

procedure TCnPDFBody.DumpToStrings(Strings: TStrings; Verbose: Boolean;
  Indent: Integer);
var
  I: Integer;
  Obj, V: TCnPDFObject;
  S, Ext: string;
begin
  Strings.Add('Body');
  Strings.Add('PDF Object Count: ' + IntToStr(FObjects.Count));
  for I := 0 to FObjects.Count - 1 do
  begin
    Obj := FObjects[I];
    S := Obj.ClassName;
    S := StringReplace(S, 'TCnPDF', '', [rfReplaceAll]);
    S := StringReplace(S, 'Object', '', [rfReplaceAll]);

    if Obj is TCnPDFArrayObject then
      Ext := (Obj as TCnPDFArrayObject).ToString
    else if Obj is TCnPDFDictionaryObject then
    begin
      Ext := (Obj as TCnPDFDictionaryObject).ToString;
      V := (Obj as TCnPDFDictionaryObject).Values['Type'];
      if (V <> nil) and (V is TCnPDFNameObject) then
        Ext := Ext + ' Type: ' + V.ToString;
    end;

    Strings.Add(Format('#%d ID %d Gen %d %s Offset %d. %s',
      [I + 1, Obj.ID, Obj.Generation, S, Obj.Offset, Ext]));

    // 补充输出 Array 和 Dictionary 的详情
    if Verbose then
    begin
      if Obj is TCnPDFDictionaryObject then
        (Obj as TCnPDFDictionaryObject).ToStrings(Strings, INDENTDELTA)
      else if Obj is TCnPDFArrayObject then
        (Obj as TCnPDFArrayObject).ToStrings(Strings, INDENTDELTA);
    end;
  end;
end;

function TCnPDFBody.GetContent(Index: Integer): TCnPDFStreamObject;
begin
  Result := TCnPDFStreamObject(FContentList[Index]);
end;

function TCnPDFBody.GetContentCount: Integer;
begin
  Result := FContentList.Count;
end;

function TCnPDFBody.GetPage(Index: Integer): TCnPDFDictionaryObject;
begin
  Result := TCnPDFDictionaryObject(FPageList[Index]);
end;

function TCnPDFBody.GetPageCount: Integer;
begin
  Result := FPageList.Count;
end;

function PDFObjectCompare(Item1, Item2: Pointer): Integer;
var
  P1, P2: TCnPDFObject;
begin
  P1 := TCnPDFObject(Item1);
  P2 := TCnPDFObject(Item2);
  Result := P1.ID - P2.ID;
end;

function TCnPDFBody.GetResource(Index: Integer): TCnPDFDictionaryObject;
begin
  Result := TCnPDFDictionaryObject(FResourceList[Index]);
end;

function TCnPDFBody.GetResourceCount: Integer;
begin
  Result := FResourceList.Count;
end;

procedure TCnPDFBody.SortObjectRefs(PDFObjects: TObjectList);
begin
  PDFObjects.Sort(PDFObjectCompare);
end;

procedure TCnPDFBody.SyncPages;
var
  I: Integer;
  Arr: TCnPDFArrayObject;
begin
  if Pages <> nil then
  begin
    FPages['Count'] := TCnPDFNumberObject.Create(FPageList.Count);
    Arr := FPages['Kids'] as TCnPDFArrayObject;

    for I := 0 to FPageList.Count - 1 do
    begin
      if not Arr.HasObjectRef(FPageList[I] as TCnPDFObject) then // 防止页面重复
        Arr.AddObjectRef(FPageList[I] as TCnPDFObject);
    end;
  end;
end;

function TCnPDFBody.WriteToStream(Stream: TStream): Cardinal;
var
  I: Integer;
  OldID: Int64;
  Obj: TCnPDFObject;
  Collection: TCnPDFXRefCollection;
  Item: TCnPDFXRefItem;
  Refs: TObjectList;
begin
  FXRefTable.Clear;
  Refs := TObjectList.Create(False);
  try
    FObjects.CopyTo(Refs);
    SortObjectRefs(Refs); // 根据复制出的列表排序

    Result := 0;
    OldID := -1;
    Collection := nil;

    for I := 0 to Refs.Count - 1 do
    begin
      Obj := TCnPDFObject(Refs[I]);
      if Obj.ID > OldID + 1 then
      begin
        // 新起 Segment，起点 Index 为该 Obj.ID
        Collection := FXRefTable.AddSegment;
        Collection.ObjectIndex := Obj.ID;
      end
      else if Obj.ID = OldID + 1 then
      begin
        // 属于本 Segment
      end;

      // 用旧 Collection 或新 Collection 新建 Item
      Item := Collection.Add;
      Item.ObjectGeneration := Obj.Generation;
      Item.ObjectXRefType := Obj.XRefType;
      Item.ObjectOffset := Stream.Position;

      // 更新 ID
      OldID := Obj.ID;
    end;

    // 原始的内容则仍按顺序写，写时更新对应对象的偏移量
    for I := 0 to FObjects.Count - 1 do
    begin
      Obj := FObjects[I];

      Item := FXRefTable.FindByObject(Obj);
      if Item <> nil then
        Item.ObjectOffset := Stream.Position;

      Inc(Result, Obj.WriteToStream(Stream));
    end;
  finally
    Refs.Free;
  end;
end;

{ TCnPDFObjectManger }

function TCnPDFObjectManager.Add(AObject: TCnPDFObject): Integer;
begin
  Result := inherited Add(AObject);
  Inc(FMaxID);
  AObject.ID := FMaxID;
end;

function TCnPDFObjectManager.AddRaw(AObject: TCnPDFObject): Integer;
begin
  Result := inherited Add(AObject);
end;

procedure TCnPDFObjectManager.CalcMaxID;
var
  I: Integer;
  Obj: TCnPDFObject;
begin
  FMaxID := 0;
  for I := 0 to Count - 1 do
  begin
    Obj := Items[I];
    if Obj.ID > FMaxID then
      FMaxID := Obj.ID;
  end;
end;

procedure TCnPDFObjectManager.CopyTo(DestObjects: TObjectList);
var
  I: Integer;
begin
  DestObjects.Clear;
  for I := 0 to Count - 1 do
    DestObjects.Add(Items[I]);
end;

constructor TCnPDFObjectManager.Create;
begin
  inherited Create(True);
end;

function TCnPDFObjectManager.GetItem(Index: Integer): TCnPDFObject;
begin
  Result := TCnPDFObject(inherited GetItem(Index));
end;

function TCnPDFObjectManager.GetObjectByIDGeneration(ObjID,
  ObjGeneration: Cardinal): TCnPDFObject;
var
  I: Integer;
  Obj: TCnPDFObject;
begin
  for I := 0 to Count - 1 do
  begin
    Obj := Items[I];
    if (Obj.ID = ObjID) and (Obj.Generation = ObjGeneration) then
    begin
      Result := Obj;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TCnPDFObjectManager.SetItem(Index: Integer;
  const Value: TCnPDFObject);
begin
  inherited SetItem(Index, Value);
  Inc(FMaxID);
  Value.ID := FMaxID;
end;

{ TCnPDFNumberObject }

constructor TCnPDFNumberObject.Create(Num: Integer);
begin
  inherited Create(AnsiString(IntToStr(Num)));
end;

constructor TCnPDFNumberObject.Create(Num: Int64);
begin
  inherited Create(AnsiString(IntToStr(Num)));
end;

function TCnPDFNumberObject.AsFloat: Extended;
var
  S: string;
begin
  S := BytesToAnsi(FContent);
  Result := StrToFloat(S);
end;

function TCnPDFNumberObject.AsInteger: Integer;
var
  S: string;
begin
  S := BytesToAnsi(FContent);
  Result := StrToInt(S);
end;

constructor TCnPDFNumberObject.Create(Num: Extended);
begin
  inherited Create(AnsiString(FloatToStr(Num)));
end;

procedure TCnPDFNumberObject.SetFloat(Value: Extended);
begin
  FContent := AnsiToBytes(FloatToStr(Value));
end;

procedure TCnPDFNumberObject.SetInteger(Value: Integer);
begin
  FContent := AnsiToBytes(IntToStr(Value));
end;

function CnLoadPDFFile(const FileName: string): TCnPDFDocument;
begin
  Result := TCnPDFDocument.Create;
  try
    Result.LoadFromFile(FileName);
  except
    Result.Free;
    Result := nil;
  end;
end;

procedure CnSavePDFFile(PDF: TCnPDFDocument; const FileName: string);
begin
  if PDF <> nil then
    PDF.SaveToFile(FileName);
end;

procedure CnJpegFilesToPDF(JpegFiles: TStrings; const FileName: string);
var
  Creator: TCnImagesToPDFCreator;
begin
  Creator := TCnImagesToPDFCreator.Create;
  try
    Creator.AddJpegFiles(JpegFiles);
    Creator.SaveToPDF(FileName);
  finally
    Creator.Free;
  end;
end;

procedure CnExtractJpegFilesFromPDF(const FileName, OutDirName: string);
var
  I: Integer;
  PDF: TCnPDFDocument;
  Stm: TCnPDFStreamObject;
  F: TFileStream;
begin
  PDF := TCnPDFDocument.Create;
  try
    PDF.LoadFromFile(FileName);
    for I := 0 to PDF.Body.Objects.Count - 1 do
    begin
      if PDF.Body.Objects[I] is TCnPDFStreamObject then
      begin
        Stm := PDF.Body.Objects[I] as TCnPDFStreamObject;
        if (Stm.Values['Filter'] is TCnPDFNameObject) and
          ((Stm.Values['Filter'] as TCnPDFNameObject).Name = 'DCTDecode') then
        begin
          F := TFileStream.Create(IncludeTrailingBackslash(OutDirName)
            + IntToStr(I) + '.jpg', fmCreate);
          try
            BytesToStream(Stm.Stream, F);
          finally
            F.Free;
          end
        end;
      end;
    end;
  finally
    PDF.Free;
  end;
end;

{ TCnImagePDFCreator }

procedure TCnImagesToPDFCreator.AddJpegFile(const JpegFile: string);
begin
  if FileExists(JpegFile) then
    FFiles.Add(JpegFile)
  else
    raise ECnPDFException.Create(SCnErrorPDFImageFileNotFound);
end;

procedure TCnImagesToPDFCreator.AddJpegFiles(const JpegFiles: TStrings);
var
  I: Integer;
begin
  for I := 0 to JpegFiles.Count - 1 do
    AddJpegFile(JpegFiles[I]);
end;

procedure TCnImagesToPDFCreator.CalcImageSize(var ImageWidth, ImageHeight: Integer);
var
  W, H: Extended;
begin
  // 页面宽 612，高 792，左右边距默认 90，上下边距默认 72
  // 因而中间内容区的宽为：页面宽 - 左边距 - 右边距 = 612 - 90 - 90 = 432
  // 高为：页面高 - 上边距 - 下边距 = 792 - 72 - 72 = 648
  // 图像如有一边超出该长度则要等比例缩，缩完再看另一边是否超长，是则再缩

  W := FPageWidth - FLeftMargin - FRightMargin;     // 默认 432
  H := FPageHeight - FTopMargin - FBottomMargin;    // 默认 648

  if ImageWidth > W then
  begin
    ImageHeight := Round(ImageHeight * W / ImageWidth);
    ImageWidth := Round(W);
  end;

  if ImageHeight > H then
  begin
    ImageWidth := Round(ImageWidth * H / ImageHeight);
    ImageHeight := Round(H);
  end;
end;

procedure TCnImagesToPDFCreator.Clear;
begin
  FFiles.Clear;
end;

constructor TCnImagesToPDFCreator.Create;
begin
  inherited;
  FFiles := TStringList.Create;

  FPageHeight := CN_PDF_A4PT_HEIGHT;
  FPageWidth := CN_PDF_A4PT_WIDTH;
  FLeftMargin := CN_PDF_A4PT_MARGIN_LEFT;
  FRightMargin := CN_PDF_A4PT_MARGIN_RIGHT;
  FTopMargin := CN_PDF_A4PT_MARGIN_TOP;
  FBottomMargin := CN_PDF_A4PT_MARGIN_BOTTOM;

  FAuthor := 'CnPack CnVCL';
  FProducer := 'CnPack Team';
  FCreator := 'CnPack Images PDF Creator';
  FCreationDate := Now;

  FTitle := '文档标题';
  FKeywords := '文档关键字,图像关键字';
  FSubject := '文档主题';
  FCompany := 'CnPack开发组';
  FComments := '本PDF文档由CnPack开发组CnVCL项目中的PDF解析库生成';
end;

destructor TCnImagesToPDFCreator.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TCnImagesToPDFCreator.RemoveJpegFile(const JpegFile: string);
var
  Idx: Integer;
begin
  Idx := FFiles.IndexOf(JpegFile);
  if Idx >= 0 then
    FFiles.Delete(Idx);
end;

procedure TCnImagesToPDFCreator.SaveToPDF(const PDFFile: string);
var
  I, W, H: Integer;
  PDF: TCnPDFDocument;
  Page: TCnPDFDictionaryObject;
  Box: TCnPDFArrayObject;
  Stream: TCnPDFStreamObject;
  Resource: TCnPDFDictionaryObject;
  ExtGState, ResDict: TCnPDFDictionaryObject;
  Content: TCnPDFStreamObject;
  ContData: TStringList;
begin
  ValidatePage;
  if FFiles.Count <= 0 then
    raise ECnPDFException.Create(SCnErrorPDFNoFile);

  PDF := nil;
  ContData := nil;

  try
    PDF := TCnPDFDocument.Create;
    ContData := TStringList.Create;

    PDF.Body.CreateResources;

    PDF.Body.Info.AddWideString('Author', FAuthor);
    PDF.Body.Info.AddWideString('Producer', FProducer);
    PDF.Body.Info.AddWideString('Creator', FCreator);
    PDF.Body.Info.AddAnsiString('CreationDate', 'D:' + FormatDateTime('yyyyMMddhhmmss', FCreationDate) + '+8''00''');

    PDF.Body.Info.AddWideString('Title', FTitle);
    PDF.Body.Info.AddWideString('Subject', FSubject);
    PDF.Body.Info.AddWideString('Keywords', FKeywords);
    PDF.Body.Info.AddWideString('Company', FCompany);
    PDF.Body.Info.AddWideString('Comments', FComments);

    // 添加 ExtGState 供多个页面共用
    ExtGState := TCnPDFDictionaryObject.Create;
    ExtGState.AddName('Type', 'ExtGState');
    ExtGState.AddFalse('AIS');
    ExtGState.AddName('BM', 'Normal');
    ExtGState.AddNumber('CA', 1);
    ExtGState.AddNumber('ca', 1);
    PDF.Body.AddObject(ExtGState);

    for I := 0 to FFiles.Count - 1 do
    begin
      // 新加一页
      Page := PDF.Body.AddPage;

      // 设置纸张大小，默认 A4。单位 Points 也就是 1/72 英寸，612 792 换算过来是 210 297 mm
      Box := Page.AddArray('MediaBox');
      Box.AddNumber(0);
      Box.AddNumber(0);
      Box.AddNumber(FPageWidth);
      Box.AddNumber(FPageHeight);

      // 添加图像内容
      Stream := TCnPDFStreamObject.Create;
      Stream.SetJpegImage(FFiles[I]);
      PDF.Body.AddObject(Stream);

      // 添加引用此图像的资源
      Resource := PDF.Body.AddResource(Page);

      // ExtGState 和 Stream 的 ID 要作为名字
      ResDict := Resource.AddDictionary('ExtGState');
      ResDict.AddObjectRef('GS' + IntToStr(ExtGState.ID), ExtGState);
      ResDict := Resource.AddDictionary('XObject');
      ResDict.AddObjectRef('IM' + IntToStr(Stream.ID), Stream);

      // 添加页面布局内容
      Content := PDF.Body.AddContent(Page);

      // 计算页面布局里的图像大小及位置并绘制，
      ContData.Clear;
      ContData.Add('q');

      W := TCnPDFNumberObject(Stream.Values['Width']).AsInteger;
      H := TCnPDFNumberObject(Stream.Values['Height']).AsInteger;
      CalcImageSize(W, H);

      ContData.Add(Format('1 0 0 1 %d %d cm', [FLeftMargin,
        FPageHeight - FTopMargin - H]));
      ContData.Add(Format('%d 0 0 %d 0 0 cm', [W, H]));

      ContData.Add('/IM' + IntToStr(Stream.ID) + ' Do');
      ContData.Add('Q');
      Content.SetStrings(ContData);

      Content.SupportCompress := True;
{$IFNDEF NO_ZLIB}
      Content.Compress; // 使用 Deflate 压缩，低版本 Delphi 下似乎也兼容 Acrobat Reader 等阅读软件
{$ENDIF}
    end;

    PDF.Trailer.GenerateID;

    if FEncrypt and (FEncryptionMethod <> cpemNotSupport) then
    begin
      PDF.Permission := FPermission;
      PDF.EncryptionMethod := FEncryptionMethod;
      PDF.Encrypt(FOwnerPassword, FUserPassword);
    end;
    PDF.SaveToFile(PDFFile);
  finally
    ContData.Free;
    PDF.Free;
  end;
end;

procedure TCnImagesToPDFCreator.SetBottomMargin(const Value: Integer);
begin
  FBottomMargin := Value;
end;

procedure TCnImagesToPDFCreator.SetLeftMargin(const Value: Integer);
begin
  FLeftMargin := Value;
end;

procedure TCnImagesToPDFCreator.SetPageHeight(const Value: Integer);
begin
  FPageHeight := Value;
end;

procedure TCnImagesToPDFCreator.SetPageWidth(const Value: Integer);
begin
  FPageWidth := Value;
end;

procedure TCnImagesToPDFCreator.SetRightMargin(const Value: Integer);
begin
  FRightMargin := Value;
end;

procedure TCnImagesToPDFCreator.SetTopMargin(const Value: Integer);
begin
  FTopMargin := Value;
end;

procedure TCnImagesToPDFCreator.ValidatePage;
begin
  // 检查页面尺寸是否合格
  if (FPageWidth <= 0) or (FPageHeight <= 0) then
    raise ECnPDFException.Create(SCnErrorPDFPageSize);

  // 检查页边距是否合格
  if (FLeftMargin < 0) or (FRightMargin < 0) or (FTopMargin < 0) or (FBottomMargin < 0) then
    raise ECnPDFException.Create(SCnErrorPDFPageMargin);

  // 检查页边距是否太大弄没了内容区
  if ((FLeftMargin + FRightMargin) >= FPageWidth) or ((FTopMargin + FBottomMargin) > FPageHeight) then
    raise ECnPDFException.Create(SCnErrorPDFPageSizeMargin);
end;

end.
