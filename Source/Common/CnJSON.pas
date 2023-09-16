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
    数字、双引号字符串、null、true、false、空格回车、#0等}

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
    destructor Destroy; override;

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

  TCnJSONBase = class
  private
    FParent: TCnJSONBase;
  public
    function AddChild(AChild: TCnJSONBase): TCnJSONBase; virtual;
    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): string; virtual; abstract;
    property Parent: TCnJSONBase read FParent write FParent;
  end;

  TCnJSONValue = class(TCnJSONBase)
  private
    FContent: AnsiString;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): string; override;

    function IsObject: Boolean; virtual;
    function IsArray: Boolean; virtual;
    function IsString: Boolean; virtual;
    function IsNumber: Boolean; virtual;
    function IsNull: Boolean; virtual;
    function IsTrue: Boolean; virtual;
    function IsFalse: Boolean; virtual;

    property Content: AnsiString read FContent write FContent;
  end;

{
  object = begin-object [ member *( value-separator member ) ]
           end-object

  member = string name-separator value
}
  TCnJSONObject = class(TCnJSONValue)
  private
    FPairs: TObjectList;
    function GetCount: Integer;
    function GetName(Index: Integer): TCnJSONString;
    function GetValue(Index: Integer): TCnJSONValue;
  public
    constructor Create; override;
    destructor Destroy; override;

    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override; // 添加 Pair
    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): string; override;

    function IsObject: Boolean; override;

    property Count: Integer read GetCount;
    {* 有多少个 Name Value 对}
    property Names[Index: Integer]: TCnJSONString read GetName;
    property Values[Index: Integer]: TCnJSONValue read GetValue;
  end;

{
  string = quotation-mark *char quotation-mark
}
  TCnJSONString = class(TCnJSONValue)
  private

  public
    function IsString: Boolean; override;
  end;

  TCnJSONNumber = class(TCnJSONValue)
  private

  public
    function IsNumber: Boolean; override;
  end;

  TCnJSONNull = class(TCnJSONValue)
  private

  public
    function IsNull: Boolean; override;
  end;

  TCnJSONTrue = class(TCnJSONValue)
  private

  public
    function IsTrue: Boolean; override;
  end;

  TCnJSONFalse = class(TCnJSONValue)
  private

  public
    function IsFalse: Boolean; override;
  end;

{
  array = begin-array [ value *( value-separator value ) ] end-array
}
  TCnJSONArray = class(TCnJSONValue)
  private
    FValues: TObjectList;
    function GetCount: Integer;
    function GetValues(Index: Integer): TCnJSONValue;
  public
    constructor Create; override;
    destructor Destroy; override;

    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    // 添加 Value 作为数组元素

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): string; override;

    property Count: Integer read GetCount;
    property Values[Index: Integer]: TCnJSONValue read GetValues;
  end;

  TCnJSONPair = class(TCnJSONBase)
  private
    FName: TCnJSONString;
    FValue: TCnJSONValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddChild(AChild: TCnJSONBase): TCnJSONBase; override;
    // 设置 Value 作为 Value

    function ToJSON(UseFormat: Boolean = True; Indent: Integer = 0): string; override;

    property Name: TCnJSONString read FName;
    {* 键名，持有}
    property Value: TCnJSONValue read FValue;
    {* 值，外部设置其引用，负责释放}
  end;

function CnJSONParse(const JsonStr: AnsiString): TCnJSONObject;
{* 解析 UTF8 格式的 JSON 字符串为 JSON 对象}

implementation

const
  CN_BLANK_CHARSET: set of AnsiChar = [#9, #10, #13, #32]; // RFC 规范中只允许这几个作为空白符
  CN_INDENT_DELTA = 4; // 输出时的缩进空格
  CRLF = #13#10;

resourcestring
  SCnErrorJSONTokenFmt = 'JSON Token %s Expected at Offset %d';
  SCnErrorJSONValueFmt = 'JSON Value Error %s at Offset %d';
  SCnErrorJSONPair = 'JSON Pair Value Conflict';

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
    Result := AChild;
  end
  else
    Result := nil;
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

function TCnJSONObject.GetCount: Integer;
begin
  Result := FPairs.Count;
end;

function TCnJSONObject.GetName(Index: Integer): TCnJSONString;
begin
  Result := (FPairs[Index] as TCnJSONPair).Name;
end;

function TCnJSONObject.GetValue(Index: Integer): TCnJSONValue;
begin
  Result := (FPairs[Index] as TCnJSONPair).Value;
end;

function TCnJSONObject.IsObject: Boolean;
begin
  Result := True;
end;

function TCnJSONObject.ToJSON(UseFormat: Boolean; Indent: Integer): string;
var
  I: Integer;
  Bld: TCnStringBuilder;
begin
  if Indent < 0 then
    Indent := 0;

  Bld := TCnStringBuilder.Create;
  try
    if UseFormat then
      Bld.Append('{' + CRLF)
    else
      Bld.AppendChar('{');

    for I := 0 to Count - 1 do
    begin
      if UseFormat then
        Bld.Append(StringOfChar(' ', Indent + CN_INDENT_DELTA));

      Bld.Append(Names[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
      Bld.AppendChar(':');
      if UseFormat then
        Bld.AppendChar(' ');
      Bld.Append(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));

      if I <> Count - 1 then
      begin
        Bld.AppendChar(',');
        if UseFormat then
          Bld.Append(CRLF);
      end;
    end;

    if UseFormat then
      Bld.Append(CRLF + StringOfChar(' ', Indent) + '}')
    else
      Bld.AppendChar('}');

    Result := Bld.ToString;
  finally
    Bld.Free;
  end;
end;

{ TCnJSONValue }

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

function TCnJSONValue.ToJSON(UseFormat: Boolean; Indent: Integer): string;
begin
  Result := FContent;
end;

{ TCnJSONArray }

function TCnJSONArray.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  if AChild is TCnJSONValue then
  begin
    FValues.Add(AChild);
    Result := AChild;
  end
  else
    Result := nil;
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

function TCnJSONArray.ToJSON(UseFormat: Boolean; Indent: Integer): string;
var
  Bld: TCnStringBuilder;
  I: Integer;
begin
  Bld := TCnStringBuilder.Create;
  try
    Bld.AppendChar('[');
    if UseFormat then
      Bld.Append(CRLF + StringOfChar(' ', Indent + CN_INDENT_DELTA));

    for I := 0 to Count - 1 do
    begin
      Bld.Append(Values[I].ToJSON(UseFormat, Indent + CN_INDENT_DELTA));
      if I <> Count - 1 then
      begin
        Bld.AppendChar(',');
        if UseFormat then
          Bld.AppendChar(' ');
      end;
    end;

    if UseFormat then
    begin
      Bld.Append(CRLF);
      Bld.Append(StringOfChar(' ', Indent) + ']');
    end
    else
      Bld.AppendChar(']');
    Result := Bld.ToString;
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
    Result := AChild;
  end
  else
    Result := nil;
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

function TCnJSONPair.ToJSON(UseFormat: Boolean; Indent: Integer): string;
begin
  // 不做，不应调用到这儿
end;

{ TCnJSONBase }

function TCnJSONBase.AddChild(AChild: TCnJSONBase): TCnJSONBase;
begin
  Result := AChild;
end;

{ TCnJSONString }

function TCnJSONString.IsString: Boolean;
begin
  Result := True;
end;

{ TCnJSONNumber }

function TCnJSONNumber.IsNumber: Boolean;
begin
  Result := True;
end;

{ TCnJSONNull }

function TCnJSONNull.IsNull: Boolean;
begin
  Result := True;
end;

{ TCnJSONTrue }

function TCnJSONTrue.IsTrue: Boolean;
begin
  Result := True;
end;

{ TCnJSONFalse }

function TCnJSONFalse.IsFalse: Boolean;
begin
  Result := True;
end;

end.
