{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnJSON;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：JSON 解析与组装单元，适用于 DXE6 以下无 JSON 解析库的场合
* 单元作者：CnPack 开发组 Liu Xiao
* 备    注：适合 UTF8 无注释格式，根据 RFC 7159 来处理
*           注意未经严格全面测试，不适合替代 System.JSON
*           仅在无 System.JSON 的低版本中充当 JSON 解析与组装用
*
*           解析：
*              调用函数 CnJSONParse，传入 UTF8 格式的 JSONString，返回 JSONObject 对象
*
*           组装：
*              创建 TCnJSONObject 后调用其 AddPair 函数
*              需要数组时创建 TCnJSONArray 后调用其 AddValue 函数
*              根 TCnJSONObject 调用 ToJSON 方法能生成 UTF8 格式的 JSON 字符串
*
* 开发平台：PWinXP + Delphi 7
* 兼容测试：PWinXP/7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2023.09.15 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Contnrs, TypInfo, CnStrings;

type
  ECnJSONException = class(Exception);
  {* JSON 解析相关异常}

  TCnJSONTokenType = (jttObjectBegin, jttObjectEnd, jttArrayBegin, jttArrayEnd,
    jttNameValueSep, jttElementSep, jttNumber, jttString, jttNull, jttTrue,
    jttFalse, jttBlank, jttTerminated, jttUnknown);
  {* JSON 中的符号类型，对应左大括号、右大括号、左中括号、右中括号、分号、逗号、
    数字、双引号字符串、null、true、false、空格回车、#0、未知等}

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
    procedure ArrayBeginProc;            // []
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
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 赋值函数}

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
  end;

{
  object = begin-object [ member *( value-separator member ) ]
           end-object

  member = string name-separator value
}
  TCnJSONObject = class(TCnJSONValue)
  {* 代表 JSON 中的对象值的类，也是 JSON 顶层类}
  private
    FPairs: TObjectList;
    function GetCount: Integer;
    function GetName(Index: Integer): TCnJSONString;
    function GetValue(Index: Integer): TCnJSONValue;
    function GetValueByName(const Name: string): TCnJSONValue;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* 供内部解析时添加 Pair}
  public
    constructor Create; override;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 赋值函数}

    procedure Clear;
    {* 清除所有内容}

    // 以下方法组装用
    function AddPair(const Name: string; Value: TCnJSONValue): TCnJSONPair; overload;
    function AddPair(const Name: string; const Value: string): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Integer): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Int64): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Extended): TCnJSONPair; overload;
    function AddPair(const Name: string; Value: Boolean): TCnJSONPair; overload;
    function AddPair(const Name: string): TCnJSONPair; overload;

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* 生成 UTF8 格式的 JSON 字符串}

    // 以下方法解析用
    function IsObject: Boolean; override;

    class function FromJSON(const JsonStr: AnsiString): TCnJSONObject;
    {* 解析 UTF8 格式的 JSON 字符串，返回新对象}

    procedure GetNames(OutNames: TStrings);
    {* 将 Name Value 对的 Name 放入 OutNames 列表}
    property Count: Integer read GetCount;
    {* 有多少个 Name Value 对}

    property Names[Index: Integer]: TCnJSONString read GetName;
    {* 名称对象索引}
    property Values[Index: Integer]: TCnJSONValue read GetValue;
    {* 值对象索引，注意值可能是 TCnJSONValue 的不同子类实例}
    property ValueByName[const Name: string]: TCnJSONValue read GetValueByName; default;
    {* 根据名称获取值的实例}
  end;

  TCnJSONValueClass = class of TCnJSONValue;

{
  string = quotation-mark *char quotation-mark
}
  TCnJSONString = class(TCnJSONValue)
  {* 代表 JSON 中的字符串值的类}
  private
    FValue: string;
    // 与 Content 同步的 string 格式内容
    procedure SetValue(const Value: string);

    function JsonFormatToString(const Str: AnsiString): string;
    {* 把 JSON 中的内容解析转义后返回，包括去引号、解释转义等}
    function StringToJsonFormat(const Str: string): AnsiString;
    {* 把字符串加上双引号与转义后返回为 JSON 格式，内部会做 UTF8 转换}
  public
    function IsString: Boolean; override;
    function AsString: string; override;
    {* 根据 Content 值更新 Value 并返回}

    property Value: string read FValue write SetValue;
    {* 组装时供外界写入值，内部同步更新 Content}
  end;

  TCnJSONNumber = class(TCnJSONValue)
  {* 代表 JSON 中的数字值的类}
  private

  public
    function IsNumber: Boolean; override;
  end;

  TCnJSONNull = class(TCnJSONValue)
  {* 代表 JSON 中的空值类}
  private

  public
    constructor Create; override;
    function IsNull: Boolean; override;
  end;

  TCnJSONTrue = class(TCnJSONValue)
  {* 代表 JSON 中的真值的类}
  private

  public
    constructor Create; override;
    function IsTrue: Boolean; override;
  end;

  TCnJSONFalse = class(TCnJSONValue)
  {* 代表 JSON 中的假值的类}
  private

  public
    constructor Create; override;
    function IsFalse: Boolean; override;
  end;

{
  array = begin-array [ value *( value-separator value ) ] end-array
}
  TCnJSONArray = class(TCnJSONValue)
  {* 代表 JSON 中的数组类}
  private
    FValues: TObjectList;
    function GetCount: Integer;
    function GetValues(Index: Integer): TCnJSONValue;
  protected
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    {* 内部添加 Value 作为数组元素}
  public
    constructor Create; override;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 赋值函数}

    procedure Clear;
    {* 清除所有内容}

    // 外部组装用
    function AddValue(Value: TCnJSONValue): TCnJSONArray; overload;
    function AddValue(const Value: string): TCnJSONArray; overload;
    function AddValue(Value: Integer): TCnJSONArray; overload;
    function AddValue(Value: Extended): TCnJSONArray; overload;
    function AddValue(Value: Boolean): TCnJSONArray; overload;
    function AddValue: TCnJSONArray; overload;

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): AnsiString; override;
    {* 生成 UTF8 格式的 JSON 字符串}

    property Count: Integer read GetCount;
    {* 数组里的元素数量}
    property Values[Index: Integer]: TCnJSONValue read GetValues;
    {* 数组里的元素}
  end;

  TCnJSONPair = class(TCnJSONBase)
  {* 代表 JSON 中 Object 内的 Name 和 Value 的组合类}
  private
    FName: TCnJSONString;
    FValue: TCnJSONValue;
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
    property Value: TCnJSONValue read FValue write FValue;
    {* 值，不自动创建，外部设置其引用，自身负责释放}
  end;

function CnJSONParse(const JsonStr: AnsiString): TCnJSONObject;
{* 解析 UTF8 格式的 JSON 字符串为 JSON 对象}

implementation

{$IFNDEF UNICODE}
uses
  CnWideStrings;
{$ENDIF}

const
  CN_BLANK_CHARSET: set of AnsiChar = [#9, #10, #13, #32]; // RFC 规范中只允许这几个作为空白符
  CN_INDENT_DELTA = 4; // 输出时的缩进空格
  CRLF = #13#10;

resourcestring
  SCnErrorJSONTokenFmt = 'JSON Token %s Expected at Offset %d';
  SCnErrorJSONValueFmt = 'JSON Value Error %s at Offset %d';
  SCnErrorJSONPair = 'JSON Pair Value Conflict';
  SCnErrorJSONTypeMismatch = 'JSON Value Type Mismatch';
  SCnErrorJSONStringParse = 'JSON String Parse Error';

// 注意，每个 JSONParseXXXX 函数执行完后，P 的 TokenID 总指向这个元素后紧邻的非空元素

function JSONParseValue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONValue; forward;

function JSONParseObject(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONObject; forward;

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
function JSONParseArray(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONArray;
begin
  Result := TCnJSONArray.Create;
  P.NextNoJunk;

  Current.AddChild(Result);
  while P.TokenID <> jttTerminated do
  begin
    JSONParseValue(P, Result);
    if P.TokenID = jttElementSep then
    begin
      P.NextNoJunk;
      Continue;
    end
    else
      Break;
  end;

  JSONCheckToken(P, jttArrayEnd);
  P.NextNoJunk;
end;

function JSONParseValue(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONValue;
begin
  case P.TokenID of
    jttObjectBegin:
      Result := JSONParseObject(P, Current);
    jttString:
      Result := JSONParseString(P, Current);
    jttNumber:
      Result := JSONParseNumber(P, Current);
    jttArrayBegin:
      Result := JSONParseArray(P, Current);
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

// 解析器遇到 { 时调用，要求 Current 是外部创建的 JSONObject 对象
function JSONParseObject(P: TCnJSONParser; Current: TCnJSONBase): TCnJSONObject;
var
  Pair: TCnJSONPair;
begin
  Result := TCnJSONObject.Create;
  P.NextNoJunk;
  if Current <> nil then
    Current.AddChild(Result);

  while P.TokenID <> jttTerminated do
  begin
    // 必须一个 String
    JSONCheckToken(P, jttString);

    Pair := TCnJSONPair.Create;
    Pair.Name.Content := P.Token;            // 设置　Pair 自有的 Name 的内容
    Result.AddChild(Pair);

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
  P.NextNoJunk;
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
        Result := JSONParseObject(P, nil);
        Exit;
      end;

      P.NextNoJunk;
    end;
  finally
    P.Free;
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
  repeat
    StepRun;
  until not (FOrigin[FRun] in ['0'..'9', '.', 'e', 'E']); // 负号不能再出现了，能出现 e 这种科学计数法
  FTokenID := jttNumber;
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
  // 要处理 UTF8 字符串，也要处理转义字符如 \ 后的 " \ / b f n r t u 直到结束的 " 为止
  while FOrigin[FRun] <> '"' do
  begin
    StepRun;
    if FOrigin[FRun] = '\' then
    begin
      StepRun;
      if FOrigin[FRun] = '"' then   // \" 特殊处理以避免判断结束错误，但要注意 UTF8 的后续字符可能出现引号
        StepRun;
    end;
  end;
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

function TCnJSONObject.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  if AChild is TCnJSONPair then
  begin
    FPairs.Add(AChild);
    AChild.Parent := Self;
    Result := AChild;
  end
  else
    Result := nil;
end;

function TCnJSONObject.AddPair(const Name: string; Value: Integer): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := IntToStr(Value);
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
  Result.Name.Content := Name;
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
  V.Content := FloatToStr(Value);
  Result := AddPair(Name, V);
end;

function TCnJSONObject.AddPair(const Name: string; Value: Int64): TCnJSONPair;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := IntToStr(Value);
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
      FPairs.Add(Pair);
    end;
  end
  else
    inherited;
end;

procedure TCnJSONObject.Clear;
begin
  FPairs.Clear;
end;

constructor TCnJSONObject.Create;
begin
  inherited;
  FPairs := TObjectList.Create(True);
end;

destructor TCnJSONObject.Destroy;
begin
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
begin
  for I := 0 to FPairs.Count - 1 do
  begin
    if TCnJSONPair(FPairs[I]).Name.AsString = Name then
    begin
      Result := TCnJSONPair(FPairs[I]).Value;
      Exit;
    end;
  end;
  Result := nil;
end;

function TCnJSONObject.IsObject: Boolean;
begin
  Result := True;
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
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

  Result := StrToFloat(FContent);
end;

function TCnJSONValue.AsInt64: Int64;
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

  Result := StrToInt64(FContent);
end;

function TCnJSONValue.AsInteger: Integer;
begin
  if not IsNumber then
    raise ECnJSONException.Create(SCnErrorJSONTypeMismatch);

  Result := StrToInt(FContent);
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
  Result := FContent; // 基类返回原始内容
end;

constructor TCnJSONValue.Create;
begin

end;

destructor TCnJSONValue.Destroy;
begin

  inherited;
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

function TCnJSONArray.AddValue(Value: Integer): TCnJSONArray;
var
  V: TCnJSONNumber;
begin
  V := TCnJSONNumber.Create;
  V.Content := IntToStr(Value);
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
  V.Content := FloatToStr(Value);
  Result := AddValue(V);
end;

function TCnJSONArray.AddValue: TCnJSONArray;
begin
  Result := AddValue(TCnJSONNull.Create);
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

function TCnJSONArray.GetValues(Index: Integer): TCnJSONValue;
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
          '"': Bld.AppendWideChar('"');
          'b': Bld.AppendWideChar(#$08);
          't': Bld.AppendWideChar(#$09);
          'n': Bld.AppendWideChar(#$0A);
          'f': Bld.AppendWideChar(#$0C);
          'r': Bld.AppendWideChar(#$0D);
          'u':
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
        case P^ of
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

function TCnJSONNumber.IsNumber: Boolean;
begin
  Result := True;
end;

{ TCnJSONNull }

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

constructor TCnJSONFalse.Create;
begin
  inherited;
  FContent := 'false';
end;

function TCnJSONFalse.IsFalse: Boolean;
begin
  Result := True;
end;

end.
