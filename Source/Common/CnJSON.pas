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

unit CnJSON;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：JSON 解析与组装单元，适用于 DXE6 以下无 JSON 解析库的场合
* 单元作者：CnPack 开发组
* 备    注：适合 UTF8 无注释格式，根据 RFC 7159 来处理
*           注意未经严格全面测试，不适合替代 System.JSON
*           仅在无 System.JSON 的低版本中充当 JSON 解析与组装用
*
*           一段 JSON 是一个 JSONObject，包含一个或多个 Key Value 对，
*           Key 是双引号字符串，Value 则可以是普通值、JSONObject 或 JSONArray，
*           JSONArray 是一排 JSONValue
*
*           JSONValue 设有 Count 及 [Integer] 非缺省索引作为是 JSONArray 时的子项，
*           设有 Count 及 Values[String] 缺省索引作为是 JSONObject 时的子项，String 参数为 Key
*           这样可以写 Obj['animals']['dog'].Values[0]['age'].AsInteger 的级联形式来访问
*
*           解析：
*              调用函数 CnJSONParse，传入 UTF8 格式的 JSONString，返回 JSONObject 对象
*
*           组装：
*              创建 TCnJSONObject 后调用其 AddPair 函数
*              需要数组时创建 TCnJSONArray 后调用其 AddValue 函数
*              根 TCnJSONObject 调用 ToJSON 方法能生成 UTF8 格式的 JSON 字符串
*
*           TCnJSONReader.LoadFromJSON 与 TCnJSONWriter.SaveToJSON 能够将对象的属性流化至 JSON 字符串中
*
* 开发平台：PWin7 + Delphi 7
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.06.14 V1.8
*                 增加解析 [ 开头和 ] 结尾的字符串为 JSONArray 的方法
*                 增加将一批 JSON 对象或值转换为 [ 开头和 ] 结尾的字符串的方法
*           2025.03.02 V1.7
*                 经 DeepSeek 检查修正几处小问题
*           2025.03.01 V1.6
*                 增加将字符串解析成多个 JSONObject 的过程并增加步进机制用于解析不完整的字符串
*                 并修正对字符串一开始就出现 \" 时的处理错误
*           2024.07.30 V1.5
*                 实现 Value 的 Clone 方法、数组的 AddValues 方法，合并 JSONObject 的过程
*           2024.06.30 V1.4
*                 浮点数与字符串转换不受某些逗号的系统区域设置的影响，均以点号小数点为准
*           2024.02.04 V1.3
*                 JSONObject 的 Key Value 对数量超过阈值时，内部用哈希表进行加速
*           2024.02.03 V1.2
*                 加入全局函数并修复空数组的问题
*           2024.01.11 V1.1
*                 加入级联默认属性，加入 Reader 和 Writer
*           2023.09.15 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, {$IFNDEF COMPILER5} Variants, {$ENDIF} Contnrs, TypInfo,
  SysConst, CnNative, CnStrings, CnHashTable;

type
  ECnJSONException = class(Exception);
  {* JSON 解析相关异常}

  TCnJSONTokenType = (jttObjectBegin, jttObjectEnd, jttArrayBegin, jttArrayEnd,
    jttNameValueSep, jttElementSep, jttNumber, jttString, jttNull, jttTrue,
    jttFalse, jttBlank, jttTerminated, jttUnknown);
  {* JSON 中的符号类型，对应左大括号、右大括号、左中括号、右中括号、分号、逗号、
    数字（包括整数和浮点）、双引号字符串、null、true、false、空格回车、#0、未知等}

  TCnJSONParser = class
  {* UTF8 格式的无注释的 JSON 字符串解析器}
  private
    FRun: Integer;
    FTokenPos: Integer;
    FOrigin: PAnsiChar;
    FStringLen: Integer; // 当前字符串的字符长度
    FProcTable: array[#0..#255] of procedure of object;
    FTokenID: TCnJSONTokenType;

    procedure KeywordProc;               // null true false 仨标识符
    procedure ObjectBeginProc;           // {
    procedure ObjectEndProc;             // }
    procedure ArrayBeginProc;            // [
    procedure ArrayEndProc;              // ]
    procedure NameValueSepProc;          // :
    procedure ArrayElementSepProc;       // ,
    procedure StringProc;                // 双引号
    procedure NumberProc;                // 数字
    procedure BlankProc;                 // 空格 Tab 回车等
    procedure TerminateProc;             // #0
    procedure UnknownProc;               // 未知
    function GetToken: AnsiString;
    procedure SetOrigin(const Value: PAnsiChar);
    procedure SetRunPos(const Value: Integer);
    function GetTokenLength: Integer;
  protected
    function TokenEqualStr(Org: PAnsiChar; const Str: AnsiString): Boolean;
    procedure MakeMethodTable;
    procedure StepRun; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    procedure StepBOM;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Next;
    {* 跳至下一个 Token 并确定 TokenID}
    procedure NextNoJunk;
    {* 跳至下一个非 Null 以及非空格 Token 并确定 TokenID}

    property Origin: PAnsiChar read FOrigin write SetOrigin;
    {* 待解析的 UTF8 格式的 JSON 字符串内容}
    property RunPos: Integer read FRun write SetRunPos;
    {* 当前处理位置相对于 FOrigin 的线性偏移量，单位为字节数，0 开始}
    property TokenID: TCnJSONTokenType read FTokenID;
    {* 当前 Token 类型}
    property Token: AnsiString read GetToken;
    {* 当前 Token 的 UTF8 字符串，暂不解析内容}
    property TokenLength: Integer read GetTokenLength;
    {* 当前 Token 的字节长度}
  end;

  TCnJSONString = class;

  TCnJSONPair = class;

  TCnJSONBase = class(TPersistent)
  {* JSON 中的各元素的基类}
  private
    FParent: TCnJSONBase;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; virtual;
    {* 供解析 JSON 时各元素拼装用，一般不需要让用户调用}
  public

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; virtual; abstract;
    property Parent: TCnJSONBase read FParent write FParent;
  end;

  TCnJSONValue = class(TCnJSONBase)
  {* 代表 JSON 中的值的类}
  private
    FContent: AnsiString;
    // 解析时存储 JSON 中解析出的 UTF8 原始内容，组装时存 UTF8 的 JSON 字符串内容
    procedure SetContent(const Value: AnsiString);
  protected
    FUpdated: Boolean;

    function GetName(Index: Integer): TCnJSONString; virtual;
    function GetValueByName(const Name: string): TCnJSONValue; virtual;

    function GetCount: Integer; virtual;
    function GetValue(Index: Integer): TCnJSONValue; virtual;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 赋值函数}

    function Clone: TCnJSONValue; virtual; abstract;
    {* 复制一份自身，包括自身持有的所有对象}

    class function FromString(const Value: string): TCnJSONValue;
    {* 从一字符串创建实例}
    class function FromInt(const Value: Integer): TCnJSONValue;
    {* 从一整数创建实例}
    class function FromFloat(const Value: Extended): TCnJSONValue;
    {* 从一浮点数创建实例}

    // 以下方法组装用
    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;

    // 以下方法解析时判断类型用
    function IsObject: Boolean; virtual;
    function IsArray: Boolean; virtual;
    function IsString: Boolean; virtual;
    function IsNumber: Boolean; virtual;
    function IsNull: Boolean; virtual;
    function IsTrue: Boolean; virtual;
    function IsFalse: Boolean; virtual;

    // 以下方法解析时取值用
    function AsString: string; virtual;
    function AsInteger: Integer; virtual;
    function AsInt64: Int64; virtual;
    function AsFloat: Extended; virtual;
    function AsBoolean: Boolean; virtual;

    property Content: AnsiString read FContent write SetContent;
    {* 普通值类型时代表原始 UTF8 格式的字符串内容}

    // 是 Object 或 Array 时提供相关模拟的属性，由子类实现
    property Names[Index: Integer]: TCnJSONString read GetName;
    {* 名称对象索引}
    property ValueByName[const Name: string]: TCnJSONValue read GetValueByName; default;
    {* 根据名称获取值的实例}

    property Count: Integer read GetCount;
    {* 如果是数组，表示数组里的元素数量}

    property Values[Index: Integer]: TCnJSONValue read GetValue;
    {* 值对象索引，如果是数组则是数组元素，注意值可能是 TCnJSONValue 的不同子类实例}
  end;

  TCnJSONArray = class;

{
  object = begin-object [ member *( value-separator member ) ]
           end-object

  member = string name-separator value
}
  TCnJSONObject = class(TCnJSONValue)
  {* 代表 JSON 中的对象值的类，也是 JSON 顶层类。
    管理下面挂的所有 Pair，从而间接管理所有下属的对象实例}
  private
    FPairs: TObjectList;
    FMap: TCnHashTable;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* 供内部解析时添加 Pair}
    function GetName(Index: Integer): TCnJSONString; override;
    function GetValueByName(const Name: string): TCnJSONValue; override;
    procedure SetValueByName(const Name: string; const Value: TCnJSONValue);
    function GetCount: Integer; override;
    function GetValue(Index: Integer): TCnJSONValue; override;
    procedure SetValue(Index: Integer; const Value: TCnJSONValue);
  public
    constructor Create; override;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 赋值函数}

    function Clone: TCnJSONValue; override;
    {* 复制一份对象自身，包括自身持有的所有对象}

    procedure Clear;
    {* 清除所有内容}

    procedure Sort(Recursive: Boolean = True; CompareProc: TListSortCompare = nil);
    {* 排序}

    // 以下方法组装用
    function AddPair(const Name: string; Value: TCnJSONValue): TCnJSONPair; overload;
    function AddPair(const Name: string; const Value: string): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Integer): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Int64): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Extended): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Boolean): TCnJSONPair; overload;
    function AddPair(const Name: string): TCnJSONPair; overload;

    function AddArray(const Name: string): TCnJSONArray;
    {* 添加一个命名的空数组并返回该数组对象}

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* 生成 UTF8 格式的 JSON 字符串，Indent 表示起始缩进}

    // 以下方法解析用
    function IsObject: Boolean; override;
    {* 返回是否是 JSONObject}

    class function FromJSON(const JsonStr: AnsiString): TCnJSONObject;
    {* 解析 UTF8 格式的 JSON 字符串，返回新对象}

    procedure GetNames(OutNames: TStrings);
    {* 将 Name Value 对的 Name 放入 OutNames 列表}
    property Count: Integer read GetCount;
    {* 有多少个 Name Value 对}

    property Names[Index: Integer]: TCnJSONString read GetName;
    {* 名称对象索引}
    property Values[Index: Integer]: TCnJSONValue read GetValue write SetValue;
    {* 值对象索引，读方法可返回索引对应的值对象，注意值可能是 TCnJSONValue 的不同子类实例；
      写方法可根据索引设置并管理其值对象，原有值对象如有则将释放}
    property ValueByName[const Name: string]: TCnJSONValue read GetValueByName write SetValueByName; default;
    {* 读方法可根据名称获取值对象，无该名称则返回 nil；
      写方法可根据名称设置并管理其值对象，原有值对象如有则释放；无该名称则抛出异常}
  end;

  TCnJSONValueClass = class of TCnJSONValue;

{
  string = quotation-mark *char quotation-mark
}
  TCnJSONString = class(TCnJSONValue)
  {* 代表 JSON 中的字符串值的类，解析时 Content 是包含前后双引号在内的未处理转义的内容}
  private
    FValue: string;
    // 与 Content 同步的 string 格式内容，同时支持 Unicode 与 Ansi
    procedure SetValue(const Value: string);

    function JsonFormatToString(const Str: AnsiString): string;
    {* 把 JSON 中的内容解析转义后返回，包括去引号、解释转义等}
    function StringToJsonFormat(const Str: string): AnsiString;
    {* 把字符串加上双引号与转义后返回为 JSON 格式，内部会做 UTF8 转换
      Str 为 string 类型，同时支持 Unicode 与 Ansi 下的 string}
  public
    class function FromString(const Value: string): TCnJSONString; reintroduce;
    {* 从一字符串创建实例}

    function Clone: TCnJSONValue; override;
    {* 复制一份相同的字符串对象}

    function IsString: Boolean; override;
    function AsString: string; override;
    {* 根据 Content 值更新 Value 并返回}

    property Value: string read FValue write SetValue;
    {* 组装时供外界写入值，内部同步更新 Content
      同时支持 Unicode 与 Ansi 下的 string}
  end;

  TCnJSONNumber = class(TCnJSONValue)
  {* 代表 JSON 中的数字值的类}
  private

  public
    class function FromInt(Value: Int64): TCnJSONNumber; reintroduce;
    {* 从整数值创建实例}
    class function FromFloat(Value: Extended): TCnJSONNumber; reintroduce;
    {* 从浮点数值创建实例}

    function IsNumber: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* 复制一份相同的数字对象}

    class function FloatToJsonFormat(Value: Extended): AnsiString;
  end;

  TCnJSONNull = class(TCnJSONValue)
  {* 代表 JSON 中的空值类}
  private

  public
    constructor Create; override;
    function IsNull: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* 复制一份 Null 对象}
  end;

  TCnJSONTrue = class(TCnJSONValue)
  {* 代表 JSON 中的真值的类}
  private

  public
    constructor Create; override;
    function IsTrue: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* 复制一份 True 对象}
  end;

  TCnJSONFalse = class(TCnJSONValue)
  {* 代表 JSON 中的假值的类}
  private

  public
    constructor Create; override;
    function IsFalse: Boolean; override;

    function Clone: TCnJSONValue; override;
    {* 复制一份 False 对象}
  end;

{
  array = begin-array [ value *( value-separator value ) ] end-array
}
  TCnJSONArray = class(TCnJSONValue)
  {* 代表 JSON 中的数组类}
  private
    FValues: TObjectList;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* 内部添加 Value 作为数组元素}
    function GetCount: Integer; override;
    function GetValue(Index: Integer): TCnJSONValue; override;
  public
    constructor Create; override;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 赋值函数}

    function Clone: TCnJSONValue; override;
    {* 复制一份相同的数组对象，包括数组里各个元素}

    procedure Clear;
    {* 清除所有内容}

    // 外部组装用
    function AddValue(Value: TCnJSONValue): TCnJSONArray; overload;
    {* 增加一值对象到数组并持有，返回数组本身}
    function AddValue(const Value: string): TCnJSONArray; overload;
    {* 增加一字符串到数组，返回数组本身}
    function AddValue(Value: Int64): TCnJSONArray; overload;
    {* 增加一整数到数组，返回数组本身}
    function AddValue(Value: Extended): TCnJSONArray; overload;
    {* 增加一浮点数到数组，返回数组本身}
    function AddValue(Value: Boolean): TCnJSONArray; overload;
    {* 增加一 Boolean 值到数组，返回数组本身}
    function AddValue: TCnJSONArray; overload;
    {* 增加一空值到数组，返回数组本身}

    function AddValues(Values: array of const): TCnJSONArray;
    {* 增加一堆内容至数组，返回数组本身。如果内容中有 TCnJSONValue 实例，则持有之}

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* 生成 UTF8 格式的 JSON 字符串}

    property Count: Integer read GetCount;
    {* 数组里的元素数量}
    property Values[Index: Integer]: TCnJSONValue read GetValue; default;
    {* 数组里的元素}
  end;

  TCnJSONPair = class(TCnJSONBase)
  {* 代表 JSON 中 Object 内的 Name 和 Value 的组合类，并管理 Name 和 Value 对象}
  private
    FName: TCnJSONString;
    FValue: TCnJSONValue;
    procedure SetValue(const Value: TCnJSONValue);
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* 设置 AChild 作为其 Value}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 赋值函数}

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* 生成 UTF8 格式的 JSON 字符串}

    property Name: TCnJSONString read FName;
    {* 键名，自动创建并持有，自身负责释放}
    property Value: TCnJSONValue read FValue write SetValue;
    {* 值，不自动创建，外部设置其引用，自身负责释放}
  end;

  TCnJSONReader = class
  {* 从 JSON 中读入对象的辅助工具类，可不创建实例而直接调用 class 各方法}
  private
    // 以下系列，返回 False 表示 Name 在 JSON 中不存在，True 存在，Value 返回值
    class function ReadStringValue(Obj: TCnJSONObject; const Name: string; out Value: string): Boolean;
    class function ReadIntegerValue(Obj: TCnJSONObject; const Name: string; out Value: Integer): Boolean;
    class function ReadFloatValue(Obj: TCnJSONObject; const Name: string; out Value: Extended): Boolean;
    class function ReadBooleanValue(Obj: TCnJSONObject; const Name: string; out Value: Boolean): Boolean;
    class function ReadInt64Value(Obj: TCnJSONObject; const Name: string; out Value: Int64): Boolean;

    class procedure ReadProperty(Instance: TPersistent; PropInfo: PPropInfo; Obj: TCnJSONObject);
  public
    class procedure Read(Instance: TPersistent; Obj: TCnJSONObject);
    {* 遍历 Instance 的各属性名并从 JSONObject 中读入对应属性值}

    class function FileToJSONObject(const FileName: string): TCnJSONObject;
    {* 从 JSON 文件中创建 JSON 对象}
    class function FileToJSON(const FileName: string): AnsiString;
    {* 从 JSON 文件中载入 JSON 字符串}
    class procedure LoadFromFile(Instance: TPersistent; const FileName: string);
    {* 遍历 Instance 的各属性名并从 JSON 文件中读入对应属性值}
    class procedure LoadFromJSON(Instance: TPersistent; const JSON: AnsiString);
    {* 遍历 Instance 的各属性名并从 JSON 字符串中读入对应属性值}
  end;

  TCnJSONWriter = class
  {* 将对象写入 JSON 的辅助工具类，可不创建实例而直接调用 class 各方法}
  private
    class procedure WriteStringValue(Obj: TCnJSONObject; const Name, Value: string);
    class procedure WriteIntegerValue(Obj: TCnJSONObject; const Name: string; Value: Integer);
    class procedure WriteFloatValue(Obj: TCnJSONObject; const Name: string; Value: Extended);
    class procedure WriteBooleanValue(Obj: TCnJSONObject; const Name: string; Value: Boolean);
    class procedure WriteInt64Value(Obj: TCnJSONObject; const Name: string; Value: Int64);
    class procedure WriteNullValue(Obj: TCnJSONObject; const Name: string);

    class procedure WriteProperty(Instance: TPersistent; PropInfo: PPropInfo; Obj: TCnJSONObject);
  public
    class procedure Write(Instance: TPersistent; Obj: TCnJSONObject);
    {* 将 Instance 的各属性赋值给 JSONObject}

    class procedure JSONObjectToFile(Obj: TCnJSONObject; const FileName: string;
       UseFormat: Boolean = True; Indent: Integer = 0; Utf8Bom: Boolean = True);
    {* 将 JSON 对象写入文件，UseFormat 控制是否带缩进格式，Indent 表示缩进空格数，
      Utf8Bom 控制是否写 Utf8 的 BOM 头}
    class procedure JSONToFile(const JSON: AnsiString; const FileName: string;
      Utf8Bom: Boolean = True);
    {* 将 JSON 字符串写入文件，Utf8Bom 控制是否写 Utf8 的 BOM 头}
    class procedure SaveToFile(Instance: TPersistent; const FileName: string;
      Utf8Bom: Boolean = True);
    {* 将 Instance 的各属性写入 JSON 文件，Utf8Bom 控制是否写 Utf8 的 BOM 头}
    class function SaveToJSON(Instance: TPersistent; UseFormat: Boolean = True): AnsiString;
    {* 将 Instance 的各属性写入 JSON 字符串，UseFormat 控制是否带缩进格式}
  end;

function CnJSONConstruct(Obj: TCnJSONObject; UseFormat: Boolean = True;
  Indent: Integer = 0): AnsiString; overload;
{* 将单个 JSON 对象转为 UTF8 格式的 JSON 字符串}

function CnJSONConstruct(Objects: TObjectList; UseFormat: Boolean = True;
  Indent: Integer = 0): AnsiString; overload;
{* 将列表中的多个 JSON 对象或值转为 UTF8 格式的 JSON 字符串，结果以 [ 开头 ] 结尾，
  Objects 列表中的对象须为 TCnJSONValue 或其子类}

function CnJSONParse(const JsonStr: AnsiString): TCnJSONObject; overload;
{* 解析 UTF8 格式的 JSON 字符串为单个 JSON 对象，需要外部释放}

function CnJSONParse(JsonStr: PAnsiChar; Objects: TObjectList): Integer; overload;
{* 解析 UTF8 格式的 JSON 字符串为多个 JSON 对象，每个对象加入 Objects 列表中，需要外部释放
  返回值表示解析出完整的 JSON 对象后，该字符串步进了多少字节。用于不完整的 JSON 字符串持续解析。
  字符指针 JsonStr + 返回值（或者说字符串下标 string(JsonStr)[返回值+1]）就是下一个待解析的起点，
  也就是上一个成功解析的右大括号的后一处。具体来说就是：
  字符串如果以右大括号结尾，字符指针 JsonStr + 返回值会指向它结尾的 #0，
  字符串尾部如果是大括号加一些空格或回车换行，JsonStr + 返回值会指向第一个空格或换行，
  字符串尾部如果是不完整的 JSON 字符串，JsonStr + 返回值就指向不完整的开头。}

procedure CnJSONMergeObject(FromObj: TCnJSONObject; ToObj: TCnJSONObject;
  Replace: Boolean = False);
{* 将 FromObj 这个 JSONObject 的键值对合并至 ToObj 这个 JSONObject
  名字不存在的键值对将复制后插入；名字存在的，同为数组则直接拼接（元素不会判重）、同为对象则合并，
  其他情况，Replace 为 False 则啥都不做，Replace 为 True，则复制后替换}

function CnJSONParseToArray(const JsonStr: AnsiString): TCnJSONArray;
{* 解析 UTF8 格式的 JSON 字符串为一个数组，用于处理 [ 开头及 ] 结尾的字符串，
  如果字符串不是 [ 开头及 ] 结尾，返回 nil}

implementation

{$IFNDEF UNICODE}
uses
  CnWideStrings;
{$ENDIF}

const
  CN_BLANK_CHARSET: set of AnsiChar = [#9, #10, #13, #32]; // RFC 规范中只允许这几个作为空白符
  CN_INDENT_DELTA = 4; // 输出时的缩进空格
  CRLF = #13#10;

  CN_NAME_HASH_THRESHOLD = 128;
  {* JSONObject 的 Key Value 对数量超过这个阈值时，内部用哈希表进行加速}

var
  DummyTermStep: Integer;

resourcestring
  SCnErrorJSONTokenFmt = 'JSON Token %s Expected at Offset %d';
  SCnErrorJSONValueFmt = 'JSON Value Error %s at Offset %d';
  SCnErrorJSONPair = 'JSON Pair Value Conflict';
  SCnErrorJSONTypeMismatch = 'JSON Value Type Mismatch';
  SCnErrorJSONNameNotExistsFmt = 'JSON Name %s NOT Exist.';
  SCnErrorJSONStringParse = 'JSON String Parse Error';
  SCnErrorJSONValueTypeNotImplementedFmt = 'NOT Implemented for this JSON Value Type %s';
  SCnErrorJSONArrayConstsTypeFmt = 'JSON Const Type NOT Support %d';
  SCnErrorJSONArrayTrailingComma = 'JSON Trailing Comma Error in Array';

{$IFDEF SUPPORT_FORMAT_SETTINGS}

var
  JSONFormatSettings: TFormatSettings; // 控制 JSON 中的浮点数小数点为 . 号，不受一些 , 的 OS 语言地区影响

{$ENDIF}

function JSONDateTimeToStr(Value: TDateTime): string;
begin
  if Trunc(Value) = 0 then
    Result := FormatDateTime('''hh:mm:ss.zzz''', Value)
  else if Frac(Value) = 0 then
    Result := FormatDateTime('''yyyy-mm-dd''', Value)
  else
    Result := FormatDateTime('''yyyy-mm-dd hh:mm:ss.zzz''', Value);
end;

// 注意，每个 JSONParseXXXX 函数执行完后，P 的 TokenID 总指向这个元素后紧邻的非空元素

function JSONParseValue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONValue; forward;

function JSONParseObject(P: TCnJSONParser; Current: TCnJSONBase; out TermStep: Integer): TCnJSONObject; forward;

procedure JSONCheckToken(P: TCnJSONParser; ExpectedToken: TCnJSONTokenType);
begin
  if P.TokenID <> ExpectedToken then
    raise ECnJSONException.CreateFmt(SCnErrorJSONTokenFmt,
      [GetEnumName(TypeInfo(TCnJSONTokenType), Ord(ExpectedToken)), P.RunPos]);
end;

// 解析器遇到字符串时调用，Current 是外部的父对象
function JSONParseString(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONString;
begin
  Result := TCnJSONString.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// 解析器遇到数字时调用，Current 是外部的父对象
function JSONParseNumber(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONNumber;
begin
  Result := TCnJSONNumber.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// 解析器遇到 null 时调用，Current 是外部的父对象
function JSONParseNull(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONNull;
begin
  Result := TCnJSONNull.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// 解析器遇到 true 时调用，Current 是外部的父对象
function JSONParseTrue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONTrue;
begin
  Result := TCnJSONTrue.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// 解析器遇到 false 时调用，Current 是外部的父对象
function JSONParseFalse(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONFalse;
begin
  Result := TCnJSONFalse.Create;
  Result.Content := P.Token;
  Current.AddChild(Result);
  P.NextNoJunk;
end;

// 解析器遇到数组开始符号 [ 时调用，Current 是外部的父对象
function JSONParseArray(P: TCnJSONParser; Current: TCnJSONBase; out TermStep: Integer): TCnJSONArray;
begin
  Result := TCnJSONArray.Create;
  P.NextNoJunk;

  try
    while not (P.TokenID in [jttTerminated, jttArrayEnd]) do
    begin
      JSONParseValue(P, Result);
      if P.TokenID = jttElementSep then
      begin
        P.NextNoJunk;
        if P.TokenID = jttArrayEnd then // 最后一个元素后不允许逗号
          raise ECnJSONException.Create(SCnErrorJSONArrayTrailingComma);

        Continue;
      end
      else
        Break;
    end;

    JSONCheckToken(P, jttArrayEnd);
    TermStep := P.RunPos;
  except
    // 如果解析出了异常，就要释放之前创建的 Result 这个对象，否则会出内存泄漏
    FreeAndNil(Result);

    raise;
  end;

  // 最后再把有效的 Result 挂上去
  if (Current <> nil) and (Result <> nil) then
    Current.AddChild(Result);

  P.NextNoJunk;
end;

function JSONParseValue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONValue;
begin
  case P.TokenID of
    jttObjectBegin:
      Result := JSONParseObject(P, Current, DummyTermStep);
    jttString:
      Result := JSONParseString(P, Current);
    jttNumber:
      Result := JSONParseNumber(P, Current);
    jttArrayBegin:
      Result := JSONParseArray(P, Current, DummyTermStep);
    jttNull:
      Result := JSONParseNull(P, Current);
    jttTrue:
      Result := JSONParseTrue(P, Current);
    jttFalse:
      Result := JSONParseFalse(P, Current);
  else
    raise ECnJSONException.CreateFmt(SCnErrorJSONValueFmt,
      [GetEnumName(TypeInfo(TCnJSONTokenType), Ord(P.TokenID)), P.RunPos]);
  end;
end;

// 解析器遇到 { 时调用，要求 Current 是外部创建的 JSONObject 对象或 nil
function JSONParseObject(P: TCnJSONParser; Current: TCnJSONBase; out TermStep: Integer): TCnJSONObject;
var
  Pair: TCnJSONPair;
begin
  Result := TCnJSONObject.Create;
  P.NextNoJunk;

  try
    // { 后也可以直接一个 } 表示空对象
    while (P.TokenID <> jttTerminated) and (P.TokenID <> jttObjectEnd) do
    begin
      // 必须一个 String
      JSONCheckToken(P, jttString);

      Pair := TCnJSONPair.Create;
      Result.AddChild(Pair);
      Pair.Name.Content := P.Token;            // 设置 Pair 自有的 Name 的内容

      // 必须一个冒号
      P.NextNoJunk;
      JSONCheckToken(P, jttNameValueSep);

      P.NextNoJunk;
      JSONParseValue(P, Pair);
      // 必须一个 Value

      if P.TokenID = jttElementSep then        // 有逗号分隔，说明有下一对 Key Value 对
      begin
        P.NextNoJunk;
        Continue;
      end
      else
        Break;
    end;

    JSONCheckToken(P, jttObjectEnd);
    TermStep := P.RunPos;
  except
    // 如果解析出了异常，就要释放之前创建的 Result 这个对象，否则会出内存泄漏
    FreeAndNil(Result);

    raise;
  end;

  // 最后再把有效的 Result 挂上去
  if (Current <> nil) and (Result <> nil) then
    Current.AddChild(Result);
  P.NextNoJunk;
end;

function CnJSONParseToArray(const JsonStr: AnsiString): TCnJSONArray;
var
  P: TCnJSONParser;
begin
  Result := nil;
  P := TCnJSONParser.Create;
  try
    P.SetOrigin(PAnsiChar(JsonStr));

    if P.TokenID in [jttBlank, jttUnknown] then
      P.NextNoJunk;

    if P.TokenID = jttArrayBegin then
      Result := JSONParseArray(P, nil, DummyTermStep);
  finally
    P.Free;
  end;
end;

function CnJSONParse(const JsonStr: AnsiString): TCnJSONObject;
var
  P: TCnJSONParser;
begin
  Result := nil;
  P := TCnJSONParser.Create;
  try
    P.SetOrigin(PAnsiChar(JsonStr));

    while P.TokenID <> jttTerminated do
    begin
      if P.TokenID = jttObjectBegin then
      begin
        Result := JSONParseObject(P, nil, DummyTermStep);
        Exit;
      end;

      P.NextNoJunk;
    end;
  finally
    P.Free;
  end;
end;

function CnJSONParse(JsonStr: PAnsiChar; Objects: TObjectList): Integer;
var
  P: TCnJSONParser;
  Obj: TCnJSONObject;
  Step: Integer;
begin
  Result := 0;
  P := TCnJSONParser.Create;
  try
    P.SetOrigin(JsonStr);
    while P.TokenID <> jttTerminated do
    begin
      // 这个循环有两个退出点，一个是碰不上 {，一个是碰上 { 后解析出错
      if P.TokenID = jttObjectBegin then
      begin
        try
          Obj := JSONParseObject(P, nil, Step);
        except
          Obj := nil;
        end;

        if Obj <> nil then
        begin
          Objects.Add(Obj);
          Result := Step;
        end
        else
          Exit;
      end;

      P.NextNoJunk;
    end;
  finally
    P.Free;
  end;
end;

function CnJSONConstruct(Obj: TCnJSONObject; UseFormat: Boolean;
  Indent: Integer): AnsiString;
begin
  if Obj <> nil then
    Result := Obj.ToJSON(UseFormat, Indent)
  else
    Result := '';
end;

function CnJSONConstruct(Objects: TObjectList; UseFormat: Boolean;
  Indent: Integer): AnsiString;
var
  I: Integer;
begin
  Result := '[';
  if Objects.Count > 0 then
  begin
    for I := 0 to Objects.Count - 1 do
    begin
      if Objects[I] is TCnJSONValue then
      begin
        if I = 0 then
          Result := Result + TCnJSONValue(Objects[I]).ToJSON(UseFormat, Indent)
        else
          Result := Result + ', ' + TCnJSONValue(Objects[I]).ToJSON(UseFormat, Indent);
      end;
    end;
  end;
  Result := Result + ']';
end;

procedure CnJSONMergeObject(FromObj: TCnJSONObject; ToObj: TCnJSONObject;
  Replace: Boolean);
var
  I, J: Integer;
  V, D: TCnJSONValue;
  N: string;
begin
  if (FromObj = nil) or (ToObj = nil) then
    Exit;

  if FromObj.Count = 0 then
    Exit;

  // 不存在的键值对将复制后插入，存在的，则简单类型进行覆盖，数组类型拼接，对象类型合并
  for I := 0 to FromObj.Count - 1 do
  begin
    N := FromObj.Names[I].AsString;
    if N = '' then
      Continue;

    D := ToObj.ValueByName[N];
    V := FromObj.Values[I];
    // N 是名称，V 是 From 的值，D 是 To 的值

    if D = nil then
    begin
      // 如果 FromObj 里的这个 Name 在 ToObj 里不存在，则复制后插入 ToObj
      ToObj.AddPair(N, V.Clone);
    end
    else
    begin
      // 同名的值存在，都是数组则拼接，都是对象则合并
      if (V is TCnJSONArray) and (D is TCnJSONArray) then
      begin
        for J := 0 to TCnJSONArray(V).Count - 1 do
          TCnJSONArray(D).AddValue(TCnJSONArray(V)[J].Clone);
      end
      else if (V is TCnJSONObject) and (D is TCnJSONObject) then
      begin
        CnJSONMergeObject(TCnJSONObject(V), TCnJSONObject(D), Replace);
      end
      else // 都不是，则按需替换，哪怕类型不对也强行替换
      begin
        if Replace then
          ToObj.ValueByName[N] := V.Clone;
      end;
    end;
  end;
end;

{ TCnJSONParser }

procedure TCnJSONParser.ArrayBeginProc;
begin
  StepRun;
  FTokenID := jttArrayBegin;
end;

procedure TCnJSONParser.ArrayElementSepProc;
begin
  StepRun;
  FTokenID := jttElementSep;
end;

procedure TCnJSONParser.ArrayEndProc;
begin
  StepRun;
  FTokenID := jttArrayEnd;
end;

procedure TCnJSONParser.BlankProc;
begin
  repeat
    StepRun;
  until not (FOrigin[FRun] in CN_BLANK_CHARSET);
  FTokenID := jttBlank;
end;

constructor TCnJSONParser.Create;
begin
  inherited Create;
  MakeMethodTable;
end;

destructor TCnJSONParser.Destroy;
begin

  inherited;
end;

function TCnJSONParser.GetToken: AnsiString;
var
  Len: Cardinal;
  OutStr: AnsiString;
begin
  Len := FRun - FTokenPos;                         // 两个偏移量之差，单位为字符数
  SetString(OutStr, (FOrigin + FTokenPos), Len);   // 以指定内存地址与长度构造字符串
  Result := OutStr;
end;

function TCnJSONParser.GetTokenLength: Integer;
begin
  Result := FRun - FTokenPos;
end;

procedure TCnJSONParser.KeywordProc;
begin
  FStringLen := 0;
  repeat
    StepRun;
    Inc(FStringLen);
  until not (FOrigin[FRun] in ['a'..'z']); // 找到小写字母组合的标识符尾巴

  FTokenID := jttUnknown; // 先这么设
  if (FStringLen = 5) and TokenEqualStr(FOrigin + FRun - FStringLen, 'false') then
    FTokenID := jttFalse
  else if FStringLen = 4 then
  begin
    if TokenEqualStr(FOrigin + FRun - FStringLen, 'true') then
      FTokenID := jttTrue
    else if TokenEqualStr(FOrigin + FRun - FStringLen, 'null') then
      FTokenID := jttNull;
  end;
end;

procedure TCnJSONParser.MakeMethodTable;
var
  I: AnsiChar;
begin
  for I := #0 to #255 do
  begin
    case I of
      #0:
        FProcTable[I] := TerminateProc;
      #9, #10, #13, #32:
        FProcTable[I] := BlankProc;
      '"':
        FProcTable[I] := StringProc;
      '0'..'9', '+', '-':
        FProcTable[I] := NumberProc;
      '{':
        FProcTable[I] := ObjectBeginProc;
      '}':
        FProcTable[I] := ObjectEndProc;
      '[':
        FProcTable[I] := ArrayBeginProc;
      ']':
        FProcTable[I] := ArrayEndProc;
      ':':
        FProcTable[I] := NameValueSepProc;
      ',':
        FProcTable[I] := ArrayElementSepProc;
      'f', 'n', 't':
        FProcTable[I] := KeywordProc;
    else
      FProcTable[I] := UnknownProc;
    end;
  end;
end;

procedure TCnJSONParser.NameValueSepProc;
begin
  StepRun;
  FTokenID := jttNameValueSep;
end;

procedure TCnJSONParser.Next;
begin
  FTokenPos := FRun;
  FProcTable[FOrigin[FRun]];
end;

procedure TCnJSONParser.NextNoJunk;
begin
  repeat
    Next;
  until not (FTokenID in [jttBlank]);
end;

procedure TCnJSONParser.NumberProc;
begin
  FTokenID := jttNumber;

  // 可选正负号
  if FOrigin[FRun] in ['-', '+'] then
    StepRun;

  // 整数部分
  while FOrigin[FRun] in ['0'..'9'] do
    StepRun;

  // 可能有小数部分
  if FOrigin[FRun] = '.' then
  begin
    StepRun;
    while FOrigin[FRun] in ['0'..'9'] do
      StepRun;
  end;

  // 指数
  if FOrigin[FRun] in ['e', 'E'] then
  begin
    StepRun;
    if FOrigin[FRun] in ['-', '+'] then
      StepRun;
    while FOrigin[FRun] in ['0'..'9'] do
      StepRun;
  end;
end;

procedure TCnJSONParser.ObjectBeginProc;
begin
  StepRun;
  FTokenID := jttObjectBegin;
end;

procedure TCnJSONParser.ObjectEndProc;
begin
  StepRun;
  FTokenID := jttObjectEnd;
end;

procedure TCnJSONParser.SetOrigin(const Value: PAnsiChar);
begin
  FOrigin := Value;
  FRun := 0;
  StepBOM;
  Next;
end;

procedure TCnJSONParser.SetRunPos(const Value: Integer);
begin
  FRun := Value;
  Next;
end;

procedure TCnJSONParser.StepBOM;
begin
  if (FOrigin[FRun] <> #239) or (FOrigin[FRun + 1] = #0) then
    Exit;
  if (FOrigin[FRun + 1] <> #187) or (FOrigin[FRun + 2] = #0) then
    Exit;
  if FOrigin[FRun + 2] <> #191 then
    Exit;

  Inc(FRun, 3);
end;

procedure TCnJSONParser.StepRun;
begin
  Inc(FRun);
end;

procedure TCnJSONParser.StringProc;
begin
  StepRun;
  FTokenID := jttString;
  // 要处理 UTF8 字符串，也要处理转义字符如 \ 后的 " \ / b f n r t u 直到结束的真正 " 为止
  while (FOrigin[FRun] <> '"') and (FOrigin[FRun] <> #0) do
  begin
    if FOrigin[FRun] = '\' then // 有转义号
      StepRun; // 指向转义号后面一个字符，如果该字符是双引号，会被下面越过，不参与循环结束判断

    StepRun; // 越过转义号后一字符指向转义号后第二个字符，可能是引号，可能是 u 后的四个数字字母，其他正常字符
  end;

  if FOrigin[FRun] <> #0 then
    StepRun;
end;

procedure TCnJSONParser.TerminateProc;
begin
  FTokenID := jttTerminated;
end;

function TCnJSONParser.TokenEqualStr(Org: PAnsiChar; const Str: AnsiString): Boolean;
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

procedure TCnJSONParser.UnknownProc;
begin
  StepRun;
  FTokenID := jttUnknown;
end;

{ TCnJSONObject }

function TCnJSONObject.AddArray(const Name: string): TCnJSONArray;
begin
  Result := TCnJSONArray.Create;
  AddPair(Name, Result);
end;

function TCnJSONObject.AddChild(AChild: TCnJSONBase): TCnJSONBase;
var
  I: Integer;
begin
  if AChild is TCnJSONPair then
  begin
    FPairs.Add(AChild);
    AChild.Parent := Self;
    Result := AChild;

    // 如果之前已经超了 THRESHOLD，则有了 Map，无论如何都加 Map
    if (FMap <> nil) or (FPairs.Count > CN_NAME_HASH_THRESHOLD) then
    begin
      if FMap = nil then
      begin
        // 如果是首次超，则创建 Map 并将之前的都加进来
        FMap := TCnHashTable.Create;
        for I := 0 to FPairs.Count - 1 do
          FMap.Add(TCnJSONPair(FPairs[I]).Name.AsString, FPairs[I]);
      end;

      FMap.Add(TCnJSONPair(AChild).Name.AsString, Pointer(AChild)); // 暂未判重
    end;
  end
  else
    Result := nil;
end;

function TCnJSONObject.AddPair(const Name: string; Value: Integer): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := AnsiString(IntToStr(Value));
  Result := AddPair(Name, V);
end;

function TCnJSONObject.AddPair(const Name, Value: string): TCnJSONPair;
var
  V: TCnJSONString;
begin
  V := TCnJSONString.Create;
  V.Value := Value;
  Result := AddPair(Name, V);
end;

function TCnJSONObject.AddPair(const Name: string; Value: TCnJSONValue): TCnJSONPair;
begin
  Result := TCnJSONPair.Create;
  AddChild(Result);
  Result.Name.Value := Name;
  Result.Value := Value;
end;

function TCnJSONObject.AddPair(const Name: string): TCnJSONPair;
begin
  Result := AddPair(Name, TCnJSONNull.Create);
end;

function TCnJSONObject.AddPair(const Name: string; Value: Boolean): TCnJSONPair;
begin
  if Value then
    Result := AddPair(Name, TCnJSONTrue.Create)
  else
    Result := AddPair(Name, TCnJSONFalse.Create);
end;

function TCnJSONObject.AddPair(const Name: string; Value: Extended): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := TCnJSONNumber.FloatToJsonFormat(Value);
  Result := AddPair(Name, V);
end;

function TCnJSONObject.AddPair(const Name: string; Value: Int64): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := AnsiString(IntToStr(Value));
  Result := AddPair(Name, V);
end;

procedure TCnJSONObject.Assign(Source: TPersistent);
var
  I: Integer;
  JObj: TCnJSONObject;
  Pair: TCnJSONPair;
begin
  if Source is TCnJSONObject then
  begin
    JObj := Source as TCnJSONObject;
    FPairs.Clear;

    for I := 0 to JObj.Count - 1 do
    begin
      Pair := TCnJSONPair.Create;
      Pair.Assign(TCnJSONPair(JObj.FPairs[I]));
      AddChild(Pair);
    end;
  end
  else
    inherited;
end;

procedure TCnJSONObject.Clear;
begin
  FPairs.Clear;
end;

function TCnJSONObject.Clone: TCnJSONValue;
begin
  Result := TCnJSONObject.Create;
  Result.Assign(Self);
end;

constructor TCnJSONObject.Create;
begin
  inherited;
  FPairs := TObjectList.Create(True);
end;

destructor TCnJSONObject.Destroy;
begin
  FMap.Free;
  FPairs.Free;
  inherited;
end;

class function TCnJSONObject.FromJSON(const JsonStr: AnsiString): TCnJSONObject;
begin
  Result := CnJSONParse(JsonStr);
end;

function TCnJSONObject.GetCount: Integer;
begin
  Result := FPairs.Count;
end;

function TCnJSONObject.GetName(Index: Integer): TCnJSONString;
begin
  Result := (FPairs[Index] as TCnJSONPair).Name;
end;

procedure TCnJSONObject.GetNames(OutNames: TStrings);
var
  I: Integer;
begin
  if OutNames <> nil then
  begin
    OutNames.Clear;
    for I := 0 to Count - 1 do
      OutNames.Add((FPairs[I] as TCnJSONPair).Name.AsString);
  end;
end;

function TCnJSONObject.GetValue(Index: Integer): TCnJSONValue;
begin
  Result := (FPairs[Index] as TCnJSONPair).Value;
end;

function TCnJSONObject.GetValueByName(const Name: string): TCnJSONValue;
var
  I: Integer;
  P: Pointer;
begin
  if FMap = nil then
  begin
    for I := 0 to FPairs.Count - 1 do
    begin
      if TCnJSONPair(FPairs[I]).Name.AsString = Name then
      begin
        Result := TCnJSONPair(FPairs[I]).Value;
        Exit;
      end;
    end
  end
  else // 用散列来加速
  begin
    P := FMap.GetValues(Name);
    if P <> nil then
    begin
      Result := TCnJSONPair(P).Value;
      Exit;
    end;
  end;
  Result := nil;
end;

function TCnJSONObject.IsObject: Boolean;
begin
  Result := True;
end;

procedure TCnJSONObject.SetValue(Index: Integer; const Value: TCnJSONValue);
begin
  (FPairs[Index] as TCnJSONPair).Value := Value;
end;

procedure TCnJSONObject.SetValueByName(const Name: string; const Value: TCnJSONValue);
var
  I: Integer;
  P: Pointer;
begin
  if FMap = nil then
  begin
    for I := 0 to FPairs.Count - 1 do
    begin
      if TCnJSONPair(FPairs[I]).Name.AsString = Name then
      begin
        TCnJSONPair(FPairs[I]).Value := Value;
        Exit;
      end;
    end
  end
  else // 用散列来加速
  begin
    P := FMap.GetValues(Name);
    if P <> nil then
    begin
      TCnJSONPair(P).Value := Value;
      Exit;
    end;
  end;
  raise ECnJSONException.CreateFmt(SCnErrorJSONNameNotExistsFmt, [Name]);
end;

function ComparePair(Item1, Item2: Pointer): Integer;
var
  P1, P2: TCnJSONPair;
begin
  if (Item1 = nil) and (Item2 = nil) then
    Result := 0
  else if Item1 = nil then
    Result := -1
  else if Item2 = nil then
    Result := 1
  else
  begin
    P1 := TCnJSONPair(Item1);
    P2 := TCnJSONPair(Item2);
    Result := CompareStr(P1.Name.AsString, P2.Name.AsString);
  end;
end;

procedure TCnJSONObject.Sort(Recursive: Boolean;
  CompareProc: TListSortCompare);
var
  I, J: Integer;
  Arr: TCnJSONArray;
begin
  if not Assigned(CompareProc) then
    CompareProc := ComparePair;
  FPairs.Sort(ComparePair);

  if Recursive then // 下属子 Object 也排序
  begin
    for I := 0 to Count - 1 do
    begin
      if Values[I] is TCnJSONObject then
        (Values[I] as TCnJSONObject).Sort(Recursive, CompareProc)
      else if Values[I] is TCnJSONArray then
      begin
        Arr := Values[I] as TCnJSONArray;
        for J := 0 to Arr.Count - 1 do
        begin
          if Arr.Values[J] is TCnJSONObject then
            (Arr.Values[J] as TCnJSONObject).Sort(Recursive, CompareProc);
        end;
      end;
    end;
  end;
end;

function TCnJSONObject.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
var
  I: Integer;
  Bld: TCnStringBuilder;
begin
  if Indent < 0 then
    Indent := 0;

  Bld := TCnStringBuilder.Create(True);
  try
    if UseFormat then
      Bld.Append('{' + CRLF)
    else
      Bld.AppendAnsiChar('{');

    for I := 0 to Count - 1 do
    begin
      if UseFormat then
        Bld.Append(StringOfChar(' ', Indent + CN_INDENT_DELTA));

{$IFDEF UNICODE}
      Bld.AppendAnsi(Names[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
      // 要显式走 Ansi，因为内容可能是 UTF8，不能额外进行 string 转换
{$ELSE}
      Bld.Append(Names[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ENDIF}

      Bld.AppendAnsiChar(':');
      if UseFormat then
        Bld.AppendAnsiChar(' ');

{$IFDEF UNICODE}
      Bld.AppendAnsi(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
      // 要显式走 Ansi，因为内容可能是 UTF8，不能额外进行 string 转换
{$ELSE}
      Bld.Append(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ENDIF}

      if I <> Count - 1 then
      begin
        Bld.AppendAnsiChar(',');
        if UseFormat then
          Bld.Append(CRLF);
      end;
    end;

    if UseFormat then
      Bld.Append(CRLF + StringOfChar(' ', Indent) + '}')
    else
      Bld.AppendAnsiChar('}');

    Result := Bld.ToAnsiString;
  finally
    Bld.Free;
  end;
end;

{ TCnJSONValue }

function TCnJSONValue.AsBoolean: Boolean;
begin
  if IsTrue then
    Result := True
  else if IsFalse then
    Result := False
  else
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);
end;

function TCnJSONValue.AsFloat: Extended;
{$IFNDEF SUPPORT_FORMAT_SETTINGS}
var
  E: Integer;
{$ENDIF}
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

{$IFDEF SUPPORT_FORMAT_SETTINGS}
  Result := StrToFloat(string(FContent), JSONFormatSettings);
{$ELSE}
  // D 5 6 没有 TFormatSettings
  Val(string(FContent), Result, E);
  if E <> 0 then
    raise EConvertError.CreateFmt(SInvalidFloat, [FContent]);
{$ENDIF}
end;

function TCnJSONValue.AsInt64: Int64;
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

  Result := StrToInt64(string(FContent));
end;

function TCnJSONValue.AsInteger: Integer;
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

  Result := StrToInt(string(FContent));
end;

procedure TCnJSONValue.Assign(Source: TPersistent);
begin
  if Source is TCnJSONValue then
  begin
    Content := (Source as TCnJSONValue).Content;
  end
  else
    inherited;
end;

function TCnJSONValue.AsString: string;
begin
  Result := string(FContent); // 基类返回原始内容
end;

constructor TCnJSONValue.Create;
begin

end;

destructor TCnJSONValue.Destroy;
begin

  inherited;
end;

class function TCnJSONValue.FromFloat(const Value: Extended): TCnJSONValue;
begin
  Result := TCnJSONNumber.FromFloat(Value);
end;

class function TCnJSONValue.FromInt(const Value: Integer): TCnJSONValue;
begin
  Result := TCnJSONNumber.FromInt(Value);
end;

class function TCnJSONValue.FromString(const Value: string): TCnJSONValue;
begin
  Result := TCnJSONString.FromString(Value);
end;

function TCnJSONValue.GetCount: Integer;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.GetName(Index: Integer): TCnJSONString;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.GetValue(Index: Integer): TCnJSONValue;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.GetValueByName(const Name: string): TCnJSONValue;
begin
  raise ECnJSONException.CreateFmt(SCnErrorJSONValueTypeNotImplementedFmt, [ClassName]);
end;

function TCnJSONValue.IsArray: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsFalse: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsNull: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsNumber: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsObject: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsString: Boolean;
begin
  Result := False;
end;

function TCnJSONValue.IsTrue: Boolean;
begin
  Result := False;
end;

procedure TCnJSONValue.SetContent(const Value: AnsiString);
begin
  FContent := Value;
  FUpdated := True;
end;

function TCnJSONValue.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
begin
  // FContent 是 UTF8 格式
  Result := FContent;
end;

{ TCnJSONArray }

function TCnJSONArray.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  if AChild is TCnJSONValue then
  begin
    FValues.Add(AChild);
    AChild.Parent := Self;
    Result := AChild;
  end
  else
    Result := nil;
end;

function TCnJSONArray.AddValue(const Value: string): TCnJSONArray;
var
  V: TCnJSONString;
begin
  V := TCnJSONString.Create;
  V.Value := Value;
  Result := AddValue(V);
end;

function TCnJSONArray.AddValue(Value: TCnJSONValue): TCnJSONArray;
begin
  if Value <> nil then
    FValues.Add(Value);
  Result := Self;
end;

function TCnJSONArray.AddValue(Value: Int64): TCnJSONArray;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := AnsiString(IntToStr(Value));
  Result := AddValue(V);
end;

function TCnJSONArray.AddValue(Value: Boolean): TCnJSONArray;
begin
  if Value then
    Result := AddValue(TCnJSONTrue.Create)
  else
    Result := AddValue(TCnJSONFalse.Create)
end;

function TCnJSONArray.AddValue(Value: Extended): TCnJSONArray;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := TCnJSONNumber.FloatToJsonFormat(Value);
  Result := AddValue(V);
end;

function TCnJSONArray.AddValue: TCnJSONArray;
begin
  Result := AddValue(TCnJSONNull.Create);
end;

function TCnJSONArray.AddValues(Values: array of const): TCnJSONArray;
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
  begin
    case Values[I].VType of
      vtInteger:
        begin
          AddValue(Values[I].VInteger);
        end;
      vtInt64:
        begin
          AddValue(Values[I].VInt64^);
        end;
      vtExtended:
        begin
          AddValue(Values[I].VExtended^);
        end;
      vtBoolean:
        begin
          AddValue(Values[I].VBoolean);
        end;
      vtObject:
        begin
          if Values[I].VObject = nil then
            AddValue
          else if Values[I].VObject is TCnJSONValue then
            AddValue(Values[I].VObject as TCnJSONValue);
        end;
      vtPointer:
        begin
          if Values[I].VPointer = nil then // 指针类型只支持 nil 为 null
            AddValue
          else
            raise ECnJSONException.CreateFmt(SCnErrorJSONArrayConstsTypeFmt, [Values[I].VType]);
        end;
      vtString:
        begin
          AddValue(string(Values[I].VString^));
        end;
      vtAnsiString:
        begin
          AddValue(string(PAnsiChar(Values[I].VAnsiString)));
        end;
      vtWideString:
        begin
          AddValue(string(PWideChar(Values[I].VWideString)));
        end;
      vtChar:
        begin
          AddValue(string(Values[I].VChar));  // 注意不随编译器变化，只是 AnsiChar
        end;
      vtWideChar:
        begin
          AddValue(string(Values[I].VWideChar));
        end;
      vtPChar:
        begin
          AddValue(string(Values[I].VPChar)); // 注意不随编译器变化，只是 PAnsiChar
        end;
      vtPWideChar:
        begin
          AddValue(string(Values[I].VPWideChar));
        end;
{$IFDEF UNICODE}
      vtUnicodeString:
        begin
          AddValue(string(Values[I].VUnicodeString));
        end;
{$ENDIF}
    else
      raise ECnJSONException.CreateFmt(SCnErrorJSONArrayConstsTypeFmt, [Values[I].VType]);
    end;
  end;
  Result := Self;
end;

procedure TCnJSONArray.Assign(Source: TPersistent);
var
  I: Integer;
  Clz: TCnJSONValueClass;
  V: TCnJSONValue;
  Arr: TCnJSONArray;
begin
  if Source is TCnJSONArray then
  begin
    Arr := Source as TCnJSONArray;

    FValues.Clear;
    for I := 0 to Arr.Count - 1 do
    begin
      Clz := TCnJSONValueClass(Arr.Values[I].ClassType);
      V := TCnJSONValue(Clz.NewInstance);
      V.Create;
      V.Assign(Arr.Values[I]);

      AddValue(V);
    end;
  end
  else
    inherited;
end;

procedure TCnJSONArray.Clear;
begin
  FValues.Clear;
end;

function TCnJSONArray.Clone: TCnJSONValue;
begin
  Result := TCnJSONArray.Create;
  Result.Assign(Self);
end;

constructor TCnJSONArray.Create;
begin
  inherited;
  FValues := TObjectList.Create(True);
end;

destructor TCnJSONArray.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TCnJSONArray.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TCnJSONArray.GetValue(Index: Integer): TCnJSONValue;
begin
  Result := TCnJSONValue(FValues[Index]);
end;

function TCnJSONArray.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
var
  Bld: TCnStringBuilder;
  I: Integer;
begin
  Bld := TCnStringBuilder.Create(True);
  try
    Bld.AppendAnsiChar('[');
    if UseFormat then
      Bld.Append(CRLF + StringOfChar(' ', Indent + CN_INDENT_DELTA));

    for I := 0 to Count - 1 do
    begin
{$IFDEF UNICODE}
      Bld.AppendAnsi(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ELSE}
      Bld.Append(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
{$ENDIF}
      if I <> Count - 1 then
      begin
        Bld.AppendAnsiChar(',');
        if UseFormat then
          Bld.AppendAnsiChar(' ');
      end;
    end;

    if UseFormat then
    begin
      Bld.Append(CRLF);
      Bld.Append(StringOfChar(' ', Indent) + ']');
    end
    else
      Bld.AppendAnsiChar(']');

    Result := Bld.ToAnsiString;
  finally
    Bld.Free;
  end;
end;

{ TCnJSONPair }

function TCnJSONPair.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  if FValue <> nil then
    raise ECnJSONException.Create(SCnErrorJSONPair);

  if AChild is TCnJSONValue then
  begin
    FValue := AChild as TCnJSONValue;
    AChild.Parent := Self;
    Result := AChild;
  end
  else
    Result := nil;
end;

procedure TCnJSONPair.Assign(Source: TPersistent);
var
  Clz: TCnJSONValueClass;
  Pair: TCnJSONPair;
begin
  if Source is TCnJSONPair then
  begin
    Pair := Source as TCnJSONPair;
    FName.Assign(Pair.Name);

    if Pair.Value <> nil then
    begin
      Clz := TCnJSONValueClass(Pair.Value.ClassType);
      FValue := TCnJSONValue(Clz.NewInstance);
      FValue.Create;
      FValue.Assign(Pair.Value);
    end;
  end
  else
    inherited;
end;

constructor TCnJSONPair.Create;
begin
  inherited;
  FName := TCnJSONString.Create;
  // FValue 类型不一，不先创建
end;

destructor TCnJSONPair.Destroy;
begin
  FValue.Free;
  FName.Free;
  inherited;
end;

procedure TCnJSONPair.SetValue(const Value: TCnJSONValue);
begin
  if FValue <> nil then // 如果已经有了 FValue 则释放掉
    FreeAndNil(FValue);

  FValue := Value;
end;

function TCnJSONPair.ToJSON(UseFormat: Boolean; Indent: Integer): AnsiString;
begin
  // 不做，不应调用到这儿
  Result := '';
end;

{ TCnJSONBase }

function TCnJSONBase.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  Result := AChild;
  AChild.Parent := Self;
end;

{ TCnJSONString }

function TCnJSONString.AsString: string;
begin
  if FUpdated then
  begin
    FValue := JsonFormatToString(Content);
    FUpdated := False;
  end;
  Result := FValue;
end;

function TCnJSONString.Clone: TCnJSONValue;
begin
  Result := TCnJSONString.Create;
  Result.Assign(Self);
end;

class function TCnJSONString.FromString(const Value: string): TCnJSONString;
begin
  Result := TCnJSONString.Create;
  Result.SetValue(Value);
end;

function TCnJSONString.IsString: Boolean;
begin
  Result := True;
end;

function TCnJSONString.JsonFormatToString(const Str: AnsiString): string;
var
  Bld: TCnStringBuilder;
  P: PWideChar;
  U: Integer;
{$IFDEF UNICODE}
  WS: string;
{$ELSE}
  WS: WideString;
{$ENDIF}
  B0, B1, B2, B3: Byte;

  procedure CheckHex(B: Byte);
  begin
    if not (AnsiChar(B) in ['0'..'9', 'A'..'F', 'a'..'f']) then
      raise ECnJSONException.Create(SCnErrorJSONStringParse);
  end;

  function HexToDec(const Value: Byte): Integer;
  begin
    if Value > Ord('9') then
    begin
      if Value > Ord('F') then
        Result := Value - Ord('a') + 10
      else
        Result := Value - Ord('A') + 10;
    end
    else
      Result := Value - Ord('0');
  end;

begin
  Result := '';
  if Length(Str) = 0 then
    Exit;

  // Unicode 环境下使用系统的转换，否则使用 CnWideStrings 里的转换
{$IFDEF UNICODE}
  WS := UTF8ToUnicodeString(Str);
{$ELSE}
  WS := CnUtf8DecodeToWideString(Str);
{$ENDIF}

  if Length(WS) = 0 then
    raise ECnJSONException.Create(SCnErrorJSONStringParse); // UTF8 解码失败

  P := @WS[1];
  if P^ <> '"' then
    raise ECnJSONException.Create(SCnErrorJSONStringParse);

  Bld := TCnStringBuilder.Create(False);  // 处理双字节字符串得用 Wide 模式
  try
    Inc(P);
    while (P^ <> '"') and (P^ <> #0) do
    begin
      if P^ = '\' then
      begin
        Inc(P);
        case P^ of
          '\': Bld.AppendWideChar('\');
          '/': Bld.AppendWideChar('/');
          '"': Bld.AppendWideChar('"');
          'b': Bld.AppendWideChar(#$08);
          't': Bld.AppendWideChar(#$09);
          'n': Bld.AppendWideChar(#$0A);
          'f': Bld.AppendWideChar(#$0C);
          'r': Bld.AppendWideChar(#$0D);
          'u': // u 后面四个十六进制字符如 FFFF，不支持更长的
            begin
              Inc(P);
              B3 := Ord(P^);
              CheckHex(B3);

              Inc(P);
              B2 := Ord(P^);
              CheckHex(B2);

              Inc(P);
              B1 := Ord(P^);
              CheckHex(B1);

              Inc(P);
              B0 := Ord(P^);
              CheckHex(B0);

              U := (HexToDec(B3) shl 12) or (HexToDec(B2) shl 8) or (HexToDec(B1) shl 4) or HexToDec(B0);
              Bld.AppendWideChar(WideChar(U));
            end;
        else
          raise ECnJSONException.Create(SCnErrorJSONStringParse);
        end;
      end
      else
        Bld.AppendWideChar(P^);
      Inc(P);
    end;

{$IFDEF UNICODE}
    Result := Bld.ToString;
    // Unicode 版本下使用 Wide 版本，直接输出 string
{$ELSE}
    Result := AnsiString(Bld.ToWideString);
    // 非 Unicode 下强行使用 Wide 版本时只支持输出 WideString，外部转换成 AnsiString
{$ENDIF}
  finally
    Bld.Free;
  end;
end;

procedure TCnJSONString.SetValue(const Value: string);
begin
  FValue := Value;
  Content := StringToJsonFormat(Value);
  FUpdated := False; // 由 Value 发起的对 Content 的更新，Content 无需逆向去更新 FValue
end;

function TCnJSONString.StringToJsonFormat(const Str: string): AnsiString;
var
  Bld: TCnStringBuilder;
  P: PChar;
begin
  // 加引号以及转义编码再 UTF8 转换
  Bld := TCnStringBuilder.Create;
  try
    Bld.AppendChar('"');
    if Length(Str) > 0 then
    begin
      P := @Str[1];
      while P^ <> #0 do
      begin
        case P^ of // 注意生成时不进行 / 的转义
          '\':
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('\');
            end;
          '"':
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('"');
            end;
          #$08:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('b');
            end;
          #$09:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('t');
            end;
          #$0A:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('n');
            end;
          #$0C:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('f');
            end;
          #$0D:
            begin
              Bld.AppendChar('\');
              Bld.AppendChar('r');
            end;
        else
          Bld.AppendChar(P^);
        end;
        Inc(P);
      end;
    end;

    Bld.AppendChar('"');

{$IFDEF UNICODE}
    Result := UTF8Encode(Bld.ToString);
    // Unicode 环境下 StringBuilder 内部使用 Wide 模式，返回 UnicodeString，直接编码成 UTF8
{$ELSE}
    // 非 Unicode 环境下 StringBuilder 内部使用 Ansi 模式，返回 AnsiString（内部可能有双字节字符）
    // 转成 WideString 后编码成 UTF8
    Result := CnUtf8EncodeWideString(WideString(Bld.ToString));
{$ENDIF}
  finally
    Bld.Free;
  end;
end;

{ TCnJSONNumber }

function TCnJSONNumber.Clone: TCnJSONValue;
begin
  Result := TCnJSONNumber.Create;
  Result.Assign(Self);
end;

class function TCnJSONNumber.FloatToJsonFormat(Value: Extended): AnsiString;
begin
{$IFDEF SUPPORT_FORMAT_SETTINGS}
  Result := AnsiString(FloatToStr(Value, JSONFormatSettings));
{$ELSE}
  Result := AnsiString(FloatToStr(Value));
  // D 5 6 下不支持 TFormatSettings，可能因为区域设置将小数点整成了逗号，替换回来
  Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
  // TODO: 如何区分因某些区域设置使 FloatToStr 出现逗号小数点和逗号千位分隔符？
{$ENDIF}
end;

class function TCnJSONNumber.FromFloat(Value: Extended): TCnJSONNumber;
begin
  Result := TCnJSONNumber.Create;
  Result.SetContent(FloatToStr(Value));
end;

class function TCnJSONNumber.FromInt(Value: Int64): TCnJSONNumber;
begin
  Result := TCnJSONNumber.Create;
  Result.SetContent(IntToStr(Value));
end;

function TCnJSONNumber.IsNumber: Boolean;
begin
  Result := True;
end;

{ TCnJSONNull }

function TCnJSONNull.Clone: TCnJSONValue;
begin
  Result := TCnJSONNull.Create;
end;

constructor TCnJSONNull.Create;
begin
  inherited;
  FContent := 'null';
end;

function TCnJSONNull.IsNull: Boolean;
begin
  Result := True;
end;

{ TCnJSONTrue }

function TCnJSONTrue.Clone: TCnJSONValue;
begin
  Result := TCnJSONTrue.Create;
end;

constructor TCnJSONTrue.Create;
begin
  inherited;
  FContent := 'true';
end;

function TCnJSONTrue.IsTrue: Boolean;
begin
  Result := True;
end;

{ TCnJSONFalse }

function TCnJSONFalse.Clone: TCnJSONValue;
begin
  Result := TCnJSONFalse.Create;
end;

constructor TCnJSONFalse.Create;
begin
  inherited;
  FContent := 'false';
end;

function TCnJSONFalse.IsFalse: Boolean;
begin
  Result := True;
end;

{ TCnJSONReader }

class function TCnJSONReader.FileToJSON(const FileName: string): AnsiString;
var
  F: TFileStream;
begin
  // 如有 UTF8Bom 则原始读入，无则直接读入，不处理 UTF16 格式也就碰到 UTF 16 会出错
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if F.Size > 0 then
    begin
      SetLength(Result, F.Size);
      F.Read(Result[1], F.Size);

      // 去掉 UTF8 的 BOM 头
      if Length(Result) > SizeOf(SCN_BOM_UTF8) then
      begin
        if CompareMem(@Result[1], @SCN_BOM_UTF8[0], SizeOf(SCN_BOM_UTF8)) then
          Delete(Result, 1, SizeOf(SCN_BOM_UTF8));
      end;
    end;
  finally
    F.Free;
  end;
end;

class function TCnJSONReader.FileToJSONObject(const FileName: string): TCnJSONObject;
var
  JSON: AnsiString;
begin
  JSON := FileToJSON(FileName);
  Result := CnJSONParse(JSON);
end;

class procedure TCnJSONReader.LoadFromFile(Instance: TPersistent;
  const FileName: string);
var
  S: AnsiString;
begin
  S := FileToJSON(FileName);
  LoadFromJSON(Instance, S);
end;

class procedure TCnJSONReader.LoadFromJSON(Instance: TPersistent;
  const JSON: AnsiString);
var
  Obj: TCnJSONObject;
  Reader: TCnJSONReader;
begin
  Obj := nil;
  Reader := nil;

  try
    Obj := CnJSONParse(JSON);
    Reader := TCnJSONReader.Create;

    Reader.Read(Instance, Obj)
  finally
    Reader.Free;
    Obj.Free;
  end;
end;

class procedure TCnJSONReader.Read(Instance: TPersistent; Obj: TCnJSONObject);
var
  PropCount: Integer;
  PropList: PPropList;
  I: Integer;
  PropInfo: PPropInfo;
  Value: TCnJSONValue;
  Arr: TCnJSONArray;
begin
  PropCount := GetTypeData(Instance.ClassInfo)^.PropCount;
  if PropCount > 0 then
  begin
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      GetPropInfos(Instance.ClassInfo, PropList);
      for I := 0 to PropCount - 1 do
      begin
        PropInfo := PropList^[I];
        if PropInfo = nil then
          Break;

        ReadProperty(Instance, PropInfo, Obj);
      end;
    finally
      FreeMem(PropList, PropCount * SizeOf(Pointer));
    end;
  end;

  if Instance is TCollection then
  begin
    Value := Obj.ValueByName['Items'];
    if (Value <> nil) and (Value is TCnJSONArray) then
    begin
      Arr := Value as TCnJSONArray;
      (Instance as TCollection).Clear;

      for I := 0 to Arr.Count - 1 do
      begin
        Value := Arr.Values[I];
        if Value is TCnJSONObject then
          Read((Instance as TCollection).Add, Value as TCnJSONObject);
      end;
    end;
  end;
end;

class function TCnJSONReader.ReadBooleanValue(Obj: TCnJSONObject;
  const Name: string; out Value: Boolean): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONFalse then
    begin
      Value := False;
      Result := True;
    end
    else if V is TCnJSONTrue then
    begin
      Value := True;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadFloatValue(Obj: TCnJSONObject;
  const Name: string; out Value: Extended): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONNumber then
    begin
      Value := V.AsFloat;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadInt64Value(Obj: TCnJSONObject;
  const Name: string; out Value: Int64): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONNumber then
    begin
      Value := V.AsInt64;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadIntegerValue(Obj: TCnJSONObject;
  const Name: string; out Value: Integer): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONNumber then
    begin
      Value := V.AsInteger;
      Result := True;
    end;
  end;
end;

class function TCnJSONReader.ReadStringValue(Obj: TCnJSONObject;
  const Name: string; out Value: string): Boolean;
var
  V: TCnJSONValue;
begin
  Result := False;
  V := Obj.ValueByName[Name];
  if V <> nil then
  begin
    if V is TCnJSONString then
    begin
      Value := V.AsString;
      Result := True;
    end;
  end;
end;

class procedure TCnJSONReader.ReadProperty(Instance: TPersistent;
  PropInfo: PPropInfo; Obj: TCnJSONObject);
var
  PropType: PTypeInfo;

  procedure ReadStrProp;
  var
    Value: string;
  begin
    if ReadStringValue(Obj, string(PropInfo^.Name), Value) then
      SetStrProp(Instance, PropInfo, Value);
  end;

  procedure ReadInt64Prop;
  var
    Value: Int64;
  begin
    if ReadInt64Value(Obj, string(PropInfo^.Name), Value) then
      SetInt64Prop(Instance, PropInfo, Value);
  end;

  procedure ReadFloatProp;
  var
    Value: Extended;
  begin
    if ReadFloatValue(Obj, string(PropInfo^.Name), Value) then
      SetFloatProp(Instance, PropInfo, Value);
  end;

  procedure ReadOrdProp;
  var
    VI: Integer;
    VS: string;
    VB: Boolean;
  begin
    case PropType^.Kind of
      tkInteger:
        begin
          if ReadIntegerValue(Obj, string(PropInfo^.Name), VI) then
            SetOrdProp(Instance, string(PropInfo^.Name), VI);
        end;
      tkChar:
        begin
          if ReadStringValue(Obj, string(PropInfo^.Name), VS) then
            if Length(VS) > 0 then
              SetOrdProp(Instance, string(PropInfo^.Name), Ord(VS[1]));
        end;
      tkSet:
        begin
          if ReadStringValue(Obj, string(PropInfo^.Name), VS) then
            SetSetProp(Instance, string(PropInfo^.Name), VS);
        end;
      tkEnumeration:
        begin
          if PropType = TypeInfo(Boolean) then
          begin
            if ReadBooleanValue(Obj, string(PropInfo^.Name), VB) then
              SetOrdProp(Instance, string(PropInfo^.Name), Ord(VB));
          end
          else
          begin
            if ReadStringValue(Obj, string(PropInfo^.Name), VS) then
              SetEnumProp(Instance, string(PropInfo^.Name), VS);
          end;
        end;
    end;
  end;

  procedure ReadObjectProp;
  var
    Value: TCnJSONValue;
    Sub: TObject;
  begin
    Value := Obj.ValueByName[string(PropInfo^.Name)];
    if Value <> nil then
    begin
      if Value is TCnJSONNull then
        SetObjectProp(Instance, string(PropInfo^.Name), nil)
      else if Value is TCnJSONObject then
      begin
        Sub := GetObjectProp(Instance, string(PropInfo^.Name));
        if Sub <> nil then
          Read(TPersistent(Sub), Value as TCnJSONObject);
      end;
    end;
  end;

begin
  if PropInfo^.SetProc <> nil then // 只要可写
  begin
{$IFDEF FPC}
    PropType := PropInfo^.PropType;
{$ELSE}
    PropType := PropInfo^.PropType^;
{$ENDIF}
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        ReadOrdProp;
      tkString, tkLString, tkWString {$IFDEF UNICODE}, tkUString {$ENDIF} {$IFDEF FPC}, tkAString {$ENDIF}:
        ReadStrProp;
      tkFloat:
        ReadFloatProp; // 时间日期暂时不额外处理，内部都用浮点先整
      tkInt64:
        ReadInt64Prop;
      tkClass:
        ReadObjectProp;
    end;
  end;
end;

{ TCnJSONWriter }

class procedure TCnJSONWriter.JSONObjectToFile(Obj: TCnJSONObject;
  const FileName: string; UseFormat: Boolean; Indent: Integer; Utf8Bom: Boolean);
var
  JSON: AnsiString;
begin
  JSON := CnJSONConstruct(Obj, UseFormat, Indent);
  JSONToFile(JSON, FileName, Utf8Bom);
end;

class procedure TCnJSONWriter.JSONToFile(const JSON: AnsiString;
  const FileName: string; Utf8Bom: Boolean);
var
  F: TFileStream;
begin
  // UTF8 格式的 AnsiString，写 BOM 头与内容到文件
  F := TFileStream.Create(FileName, fmCreate);
  try
    if Utf8Bom then
      F.Write(SCN_BOM_UTF8[0], SizeOf(SCN_BOM_UTF8));

    if Length(JSON) > 0 then
      F.Write(JSON[1], Length(JSON));
  finally
    F.Free;
  end;
end;

class procedure TCnJSONWriter.SaveToFile(Instance: TPersistent;
  const FileName: string; Utf8Bom: Boolean);
var
  JSON: AnsiString;
begin
  JSON := SaveToJSON(Instance);
  JSONToFile(JSON, FileName, Utf8Bom);
end;

class function TCnJSONWriter.SaveToJSON(Instance: TPersistent;
  UseFormat: Boolean): AnsiString;
var
  Obj: TCnJSONObject;
begin
  Obj := nil;

  try
    Obj := TCnJSONObject.Create;
    TCnJSONWriter.Write(Instance, Obj);
    Result := Obj.ToJSON(UseFormat);
  finally
    Obj.Free;
  end;
end;

class procedure TCnJSONWriter.Write(Instance: TPersistent; Obj: TCnJSONObject);
var
  PropCount: Integer;
  PropList: PPropList;
  I: Integer;
  PropInfo: PPropInfo;
  Arr: TCnJSONArray;
  Sub: TCnJSONObject;
begin
  PropCount := GetTypeData(Instance.ClassInfo)^.PropCount;
  if PropCount = 0 then
    Exit;

  GetMem(PropList, PropCount * SizeOf(Pointer));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    for I := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[I];
      if PropInfo = nil then
        Break;

      if IsStoredProp(Instance, PropInfo) then
        WriteProperty(Instance, PropInfo, Obj)
    end;
  finally
    FreeMem(PropList, PropCount * SizeOf(Pointer));
  end;

  if Instance is TCollection then
  begin
    Arr := TCnJSONArray.Create;
    Obj.AddPair('Items', Arr);

    for I := 0 to (Instance as TCollection).Count - 1 do
    begin
      Sub := TCnJSONObject.Create;
      Arr.AddChild(Sub);
      Write((Instance as TCollection).Items[I], Sub);
    end;
  end;
end;

class procedure TCnJSONWriter.WriteProperty(Instance: TPersistent;
  PropInfo: PPropInfo; Obj: TCnJSONObject);
var
  PropType: PTypeInfo;

  procedure WriteStrProp;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    WriteStringValue(Obj, string(PropInfo^.Name), Value);
  end;

  procedure WriteOrdProp;
  var
    Value: Longint;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    if Value <> PPropInfo(PropInfo)^.Default then
    begin
      case PropType^.Kind of
        tkInteger:
          WriteIntegerValue(Obj, string(PropInfo^.Name), Value);
        tkChar:
          WriteStringValue(Obj, string(PropInfo^.Name), Chr(Value));
        tkSet:
          WriteStringValue(Obj, string(PropInfo^.Name), GetSetProp(Instance, PPropInfo(PropInfo), True));
        tkEnumeration:
          begin
            if PropType = TypeInfo(Boolean) then
              WriteBooleanValue(Obj, string(PropInfo^.Name), Value <> 0)
            else
              WriteStringValue(Obj, string(PropInfo^.Name), GetEnumName(PropType, Value));
          end;
      end;
    end;
  end;

  procedure WriteFloatProp;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    WriteFloatValue(Obj, string(PropInfo^.Name), Value);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    WriteInt64Value(Obj, string(PropInfo^.Name), Value);
  end;

  procedure WriteObjectProp;
  var
    Value: TObject;
    SubObj: TCnJSONObject;
  begin
    Value := GetObjectProp(Instance, PropInfo);
    if Value <> nil then
    begin
      if Value is TComponent then
      begin
        WriteStringValue(Obj, string(PropInfo^.Name), (Value as TComponent).Name);
      end
      else if Value is TPersistent then
      begin
        SubObj := TCnJSONObject.Create;
        Obj.AddPair(string(PropInfo^.Name), SubObj);
        Write(TPersistent(Value), SubObj);
      end;
    end
    else
      WriteNullValue(Obj, string(PropInfo^.Name));
  end;

begin
  if PropInfo^.GetProc <> nil then // 只要可读
  begin
{$IFDEF FPC}
    PropType := PropInfo^.PropType;
{$ELSE}
    PropType := PropInfo^.PropType^;
{$ENDIF}
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        WriteOrdProp;
      tkString, tkLString, tkWString {$IFDEF UNICODE}, tkUString {$ENDIF} {$IFDEF FPC}, tkAString {$ENDIF}:
        WriteStrProp;
      tkFloat:
        WriteFloatProp; // 时间日期暂时不额外处理，内部都用浮点先整
      tkInt64:
        WriteInt64Prop;
      tkClass:
        WriteObjectProp;
    end;
  end;
end;

class procedure TCnJSONWriter.WriteStringValue(Obj: TCnJSONObject; const Name,
  Value: string);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteBooleanValue(Obj: TCnJSONObject;
  const Name: string; Value: Boolean);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteFloatValue(Obj: TCnJSONObject;
  const Name: string; Value: Extended);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteInt64Value(Obj: TCnJSONObject;
  const Name: string; Value: Int64);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteIntegerValue(Obj: TCnJSONObject;
  const Name: string; Value: Integer);
begin
  Obj.AddPair(Name, Value);
end;

class procedure TCnJSONWriter.WriteNullValue(Obj: TCnJSONObject;
  const Name: string);
begin
  Obj.AddPair(Name);
end;

{$IFDEF SUPPORT_FORMAT_SETTINGS}

initialization
  JSONFormatSettings.DecimalSeparator := '.';

{$ENDIF}
end.
