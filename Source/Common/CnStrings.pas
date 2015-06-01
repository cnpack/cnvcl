{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2015 CnPack 开发组                       }
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

unit CnStrings;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：CnStrings 实现单元，包括 AnsiStringList 以及一个快速子串搜索算法
* 单元作者：CnPack 开发组 刘啸 (liuxiao@cnpack.org)
* 开发平台：PWinXPPro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* 备　　注：AnsiStringList 移植自 Delphi 7 的 StringList
* 单元标识：$Id$
* 最后更新：2015.06.01
*               增加快速搜索子串算法 FastPosition
*           2013.03.04
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Windows;

type
  TCnAnsiStrings = class;
  
  ICnStringsAdapter = interface
    ['{E32A5BD7-9A80-4DDE-83D7-2EE050BF476A}']
    procedure ReferenceStrings(S: TCnAnsiStrings);
    procedure ReleaseStrings;
  end;

  TCnAnsiStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator);

  TCnAnsiStrings = class(TPersistent)
  private
    FDefined: TCnAnsiStringsDefined;
    FDelimiter: AnsiChar;
    FQuoteChar: AnsiChar;
    FNameValueSeparator: AnsiChar;
    FUpdateCount: Integer;
    FAdapter: ICnStringsAdapter;
    function GetCommaText: AnsiString;
    function GetDelimitedText: AnsiString;
    function GetName(Index: Integer): AnsiString;
    function GetValue(const Name: AnsiString): AnsiString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: AnsiString);
    procedure SetDelimitedText(const Value: AnsiString);
    procedure SetStringsAdapter(const Value: ICnStringsAdapter);
    procedure SetValue(const Name, Value: AnsiString);
    procedure WriteData(Writer: TWriter);
    function GetDelimiter: AnsiChar;
    procedure SetDelimiter(const Value: AnsiChar);
    function GetQuoteChar: AnsiChar;
    procedure SetQuoteChar(const Value: AnsiChar);
    function GetNameValueSeparator: AnsiChar;
    procedure SetNameValueSeparator(const Value: AnsiChar);
    function GetValueFromIndex(Index: Integer): AnsiString;
    procedure SetValueFromIndex(Index: Integer; const Value: AnsiString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: AnsiString; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: AnsiString): AnsiString;
    function Get(Index: Integer): AnsiString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: AnsiString; virtual;
    procedure Put(Index: Integer; const S: AnsiString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: AnsiString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: AnsiString): Integer; virtual;
  public
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; virtual;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; virtual;
    procedure Append(const S: AnsiString);
    procedure AddStrings(Strings: TCnAnsiStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TCnAnsiStrings): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PAnsiChar; virtual;
    function IndexOf(const S: AnsiString): Integer; virtual;
    function IndexOfName(const Name: AnsiString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: AnsiString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: AnsiString;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: AnsiString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: AnsiString); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PAnsiChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: AnsiString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: AnsiChar read GetDelimiter write SetDelimiter;
    property DelimitedText: AnsiString read GetDelimitedText write SetDelimitedText;
    property Names[Index: Integer]: AnsiString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: AnsiChar read GetQuoteChar write SetQuoteChar;
    property Values[const Name: AnsiString]: AnsiString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: AnsiString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: AnsiChar read GetNameValueSeparator write SetNameValueSeparator;
    property Strings[Index: Integer]: AnsiString read Get write Put; default;
    property Text: AnsiString read GetTextStr write SetTextStr;
    property StringsAdapter: ICnStringsAdapter read FAdapter write SetStringsAdapter;
  end;

  TCnAnsiStringList = class;

  PCnAnsiStringItem = ^TCnAnsiStringItem;
  TCnAnsiStringItem = record
    FString: AnsiString;
    FObject: TObject;
  end;

  PCnAnsiStringItemList = ^TCnAnsiStringItemList;
  TCnAnsiStringItemList = array[0..MaxListSize] of TCnAnsiStringItem;
  TCnAnsiStringListSortCompare = function(List: TCnAnsiStringList; Index1, Index2: Integer): Integer;

  TCnAnsiStringList = class(TCnAnsiStrings)
  private
    FList: PCnAnsiStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TCnAnsiStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): AnsiString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: AnsiString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: AnsiString): Integer; override;
    procedure InsertItem(Index: Integer; const S: AnsiString; AObject: TObject); virtual;
  public
    destructor Destroy; override;
    function Add(const S: AnsiString): Integer; override;
    function AddObject(const S: AnsiString; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: AnsiString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: AnsiString): Integer; override;
    procedure Insert(Index: Integer; const S: AnsiString); override;
    procedure InsertObject(Index: Integer; const S: AnsiString;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TCnAnsiStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  PPCnAnsiHashItem = ^PCnAnsiHashItem;
  PCnAnsiHashItem = ^TCnAnsiHashItem;
  TCnAnsiHashItem = record
    Next: PCnAnsiHashItem;
    Key: AnsiString;
    Value: Integer;
  end;

  TCnAnsiStringHash = class
  private
    Buckets: array of PCnAnsiHashItem;
  protected
    function Find(const Key: AnsiString): PPCnAnsiHashItem;
    function HashOf(const Key: AnsiString): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: AnsiString; Value: Integer);
    procedure Clear;
    procedure Remove(const Key: AnsiString);
    function Modify(const Key: AnsiString; Value: Integer): Boolean;
    function ValueOf(const Key: AnsiString): Integer;
  end;

  TCnHashedAnsiStringList = class(TCnAnsiStringList)
  private
    FValueHash: TCnAnsiStringHash;
    FNameHash: TCnAnsiStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    destructor Destroy; override;
    function IndexOf(const S: AnsiString): Integer; override;
    function IndexOfName(const Name: AnsiString): Integer; override;
  end;

function FastPosition(const Str, Pattern: PChar; FromIndex: Integer = 0): Integer;
{* 快速搜索子串，返回 Pattern 在 Str 中的第一次出现的索引号，无则返回 -1}

implementation

const
  sLineBreak = #13#10;

resourcestring
  SDuplicateString = 'AnsiString list does not allow duplicates';
  SListIndexError = 'AnsiString List index out of bounds (%d)';
  SSortedListError = 'Operation not allowed on sorted AnsiString list';

// 快速搜索子串，返回 Pattern 在 Str 中的第一次出现的索引号，无则返回 -1
function FastPosition(const Str, Pattern: PChar; FromIndex: Integer): Integer;
var
  C: Char;
  I, L, X, Y, PLen, SLen: Integer;
  BCS: array[0..255] of Integer;
begin
  Result := -1;
  if (Str = nil) or (Pattern = nil) then
    Exit;

  PLen := StrLen(Pattern);
  if PLen = 0 then
    Exit;
  SLen := StrLen(Str);

  // 如果是单字符搜索模式
  if PLen = 1 then
  begin
    for I := FromIndex to SLen - 1 do
    begin
      if Str[I] = Pattern[0] then
      begin
        Result := I;
        Exit;
      end;
    end;
    Exit;
  end;

  // 填充快速跃进表
  for I := Low(BCS) to High(BCS) do
    BCS[I] := PLen;

  for I := 0 to PLen - 2 do
  begin
    C := Pattern[I];
    L := Ord(C) and $FF;
    if PLen - I - 1 < BCS[L] then
      BCS[L] := PLen - I - 1;
  end;

  // 再进行搜索
  I := FromIndex + PLen - 1;
  while I < SLen do
  begin
    X := I;
    Y := PLen - 1;
    while True do
    begin
      if Pattern[Y] <> Str[X] then
      begin
        Inc(I, BCS[Ord(Str[X]) and $FF]);
        Break;
      end;

      if Y = 0 then
      begin
        Result := X;
        Exit;
      end;

      Dec(X);
      Dec(Y);
    end;
  end;
end;

{ TCnAnsiStrings }

destructor TCnAnsiStrings.Destroy;
begin
  StringsAdapter := nil;
  inherited Destroy;
end;

function TCnAnsiStrings.Add(const S: AnsiString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TCnAnsiStrings.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TCnAnsiStrings.Append(const S: AnsiString);
begin
  Add(S);
end;

procedure TCnAnsiStrings.AddStrings(Strings: TCnAnsiStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TCnAnsiStrings.Assign(Source: TPersistent);
begin
  if Source is TCnAnsiStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TCnAnsiStrings(Source).FDefined;
      FNameValueSeparator := TCnAnsiStrings(Source).FNameValueSeparator;
      FQuoteChar := TCnAnsiStrings(Source).FQuoteChar;
      FDelimiter := TCnAnsiStrings(Source).FDelimiter;
      AddStrings(TCnAnsiStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCnAnsiStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TCnAnsiStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TCnAnsiStrings then
        Result := not Equals(TCnAnsiStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TCnAnsiStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TCnAnsiStrings.Equals(Strings: TCnAnsiStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TCnAnsiStrings.Error(const Msg: AnsiString; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(string(Msg), [Data]) at ReturnAddr;
end;

procedure TCnAnsiStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(AnsiString(LoadResString(Msg)), Data);
end;

procedure TCnAnsiStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TCnAnsiStrings.ExtractName(const S: AnsiString): AnsiString;
var
  P: Integer;
begin
  Result := S;
  P := AnsiPos(string(NameValueSeparator), string(S));
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TCnAnsiStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

function TCnAnsiStrings.GetCommaText: AnsiString;
var
  LOldDefined: TCnAnsiStringsDefined;
  LOldDelimiter: AnsiChar;
  LOldQuoteChar: AnsiChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

function TCnAnsiStrings.GetDelimitedText: AnsiString;
var
  S: AnsiString;
  P: PAnsiChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PAnsiChar(S);
      while not (P^ in [#0..' ', QuoteChar, Delimiter]) do
      {$IFDEF MSWINDOWS}
        P := CharNextA(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      if (P^ <> #0) then S := AnsiString(AnsiQuotedStr(string(S), Char(QuoteChar)));
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TCnAnsiStrings.GetName(Index: Integer): AnsiString;
begin
  Result := ExtractName(Get(Index));
end;

function TCnAnsiStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TCnAnsiStrings.GetText: PAnsiChar;
begin
  Result := StrNew(PAnsiChar(GetTextStr));
end;

function TCnAnsiStrings.GetTextStr: AnsiString;
var
  I, L, Size, Count: Integer;
  P: PAnsiChar;
  S, LB: AnsiString;
begin
  Count := GetCount;
  Size := 0;
  LB := sLineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

function TCnAnsiStrings.GetValue(const Name: AnsiString): AnsiString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TCnAnsiStrings.IndexOf(const S: AnsiString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TCnAnsiStrings.IndexOfName(const Name: AnsiString): Integer;
var
  P: Integer;
  S: AnsiString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos(string(NameValueSeparator), string(S));
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TCnAnsiStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TCnAnsiStrings.InsertObject(Index: Integer; const S: AnsiString;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TCnAnsiStrings.LoadFromFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(string(FileName), fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnAnsiStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TCnAnsiStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: AnsiString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCnAnsiStrings.Put(Index: Integer; const S: AnsiString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TCnAnsiStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TCnAnsiStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do Add(AnsiString(Reader.ReadString));
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TCnAnsiStrings.SaveToFile(const FileName: AnsiString);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(string(FileName), fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnAnsiStrings.SaveToStream(Stream: TStream);
var
  S: AnsiString;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TCnAnsiStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

procedure TCnAnsiStrings.SetCommaText(const Value: AnsiString);
begin
  Delimiter := ',';
  QuoteChar := '"';
  SetDelimitedText(Value);
end;

procedure TCnAnsiStrings.SetStringsAdapter(const Value: ICnStringsAdapter);
begin
  if FAdapter <> nil then FAdapter.ReleaseStrings;
  FAdapter := Value;
  if FAdapter <> nil then FAdapter.ReferenceStrings(Self);
end;

procedure TCnAnsiStrings.SetText(Text: PAnsiChar);
begin
  SetTextStr(Text);
end;

procedure TCnAnsiStrings.SetTextStr(const Value: AnsiString);
var
  P, Start: PAnsiChar;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TCnAnsiStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TCnAnsiStrings.SetValue(const Name, Value: AnsiString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TCnAnsiStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(string(Get(I)));
  Writer.WriteListEnd;
end;

procedure TCnAnsiStrings.SetDelimitedText(const Value: AnsiString);
var
  P, P1: PAnsiChar;
  S: AnsiString;
begin
  BeginUpdate;
  try
    Clear;
    P := PAnsiChar(Value);
    while P^ in [#1..' '] do
    {$IFDEF MSWINDOWS}
      P := CharNextA(P);
    {$ELSE}
      Inc(P);
    {$ENDIF}
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while (P^ > ' ') and (P^ <> Delimiter) do
        {$IFDEF MSWINDOWS}
          P := CharNextA(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}
        SetString(S, P1, P - P1);
      end;
      Add(S);
      while P^ in [#1..' '] do
      {$IFDEF MSWINDOWS}
        P := CharNextA(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      if P^ = Delimiter then
      begin
        P1 := P;
        {$IFDEF MSWINDOWS}
        if CharNextA(P1)^ = #0 then
        {$ELSE}
        Inc(P1);
        if P1^ = #0 then
        {$ENDIF}
          Add('');
        repeat
          {$IFDEF MSWINDOWS}
          P := CharNextA(P);
          {$ELSE}
          Inc(P);
          {$ENDIF}
        until not (P^ in [#1..' ']);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TCnAnsiStrings.GetDelimiter: AnsiChar;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TCnAnsiStrings.GetQuoteChar: AnsiChar;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

procedure TCnAnsiStrings.SetDelimiter(const Value: AnsiChar);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TCnAnsiStrings.SetQuoteChar(const Value: AnsiChar);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

function TCnAnsiStrings.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  Result := AnsiCompareText(string(S1), string(S2));
end;

function TCnAnsiStrings.GetNameValueSeparator: AnsiChar;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

procedure TCnAnsiStrings.SetNameValueSeparator(const Value: AnsiChar);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

function TCnAnsiStrings.GetValueFromIndex(Index: Integer): AnsiString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TCnAnsiStrings.SetValueFromIndex(Index: Integer; const Value: AnsiString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{ TCnAnsiStringList }

destructor TCnAnsiStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TCnAnsiStringList.Add(const S: AnsiString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TCnAnsiStringList.AddObject(const S: AnsiString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TCnAnsiStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCnAnsiStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TCnAnsiStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TCnAnsiStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TCnAnsiStringItem));
  Changed;
end;

procedure TCnAnsiStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TCnAnsiStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TCnAnsiStringList.Find(const S: AnsiString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TCnAnsiStringList.Get(Index: Integer): AnsiString;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TCnAnsiStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TCnAnsiStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TCnAnsiStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TCnAnsiStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnAnsiStringList.IndexOf(const S: AnsiString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TCnAnsiStringList.Insert(Index: Integer; const S: AnsiString);
begin
  InsertObject(Index, S, nil);
end;

procedure TCnAnsiStringList.InsertObject(Index: Integer; const S: AnsiString;
  AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TCnAnsiStringList.InsertItem(Index: Integer; const S: AnsiString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TCnAnsiStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TCnAnsiStringList.Put(Index: Integer; const S: AnsiString);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TCnAnsiStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TCnAnsiStringList.QuickSort(L, R: Integer; SCompare: TCnAnsiStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TCnAnsiStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TCnAnsiStringItem));
  FCapacity := NewCapacity;
end;

procedure TCnAnsiStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TCnAnsiStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompareStrings(List: TCnAnsiStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
                                List.FList^[Index2].FString);
end;

procedure TCnAnsiStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TCnAnsiStringList.CustomSort(Compare: TCnAnsiStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TCnAnsiStringList.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(string(S1), string(S2))
  else
    Result := AnsiCompareText(string(S1), string(S2));
end;

procedure TCnAnsiStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

{ TCnAnsiStringHash }

procedure TCnAnsiStringHash.Add(const Key: AnsiString; Value: Integer);
var
  Hash: Integer;
  Bucket: PCnAnsiHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TCnAnsiStringHash.Clear;
var
  I: Integer;
  P, N: PCnAnsiHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

constructor TCnAnsiStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TCnAnsiStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCnAnsiStringHash.Find(const Key: AnsiString): PPCnAnsiHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TCnAnsiStringHash.HashOf(const Key: AnsiString): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(Key[I]);
end;

function TCnAnsiStringHash.Modify(const Key: AnsiString; Value: Integer): Boolean;
var
  P: PCnAnsiHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TCnAnsiStringHash.Remove(const Key: AnsiString);
var
  P: PCnAnsiHashItem;
  Prev: PPCnAnsiHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TCnAnsiStringHash.ValueOf(const Key: AnsiString): Integer;
var
  P: PCnAnsiHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;

{ TCnHashedAnsiStringList }

procedure TCnHashedAnsiStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

destructor TCnHashedAnsiStringList.Destroy;
begin
  FValueHash.Free;
  FNameHash.Free;
  inherited Destroy;
end;

function TCnHashedAnsiStringList.IndexOf(const S: AnsiString): Integer;
begin
  UpdateValueHash;
  if not CaseSensitive then
    Result :=  FValueHash.ValueOf(AnsiString(AnsiUpperCase(string(S))))
  else
    Result :=  FValueHash.ValueOf(S);
end;

function TCnHashedAnsiStringList.IndexOfName(const Name: AnsiString): Integer;
begin
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(AnsiString(AnsiUpperCase(string(Name))))
  else
    Result := FNameHash.ValueOf(Name);
end;

procedure TCnHashedAnsiStringList.UpdateNameHash;
var
  I: Integer;
  P: Integer;
  Key: AnsiString;
begin
  if FNameHashValid then Exit;
  
  if FNameHash = nil then
    FNameHash := TCnAnsiStringHash.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
  begin
    Key := Get(I);
    P := AnsiPos('=', string(Key));
    if P <> 0 then
    begin
      if not CaseSensitive then
        Key := AnsiString(AnsiUpperCase(string(Copy(Key, 1, P - 1))))
      else
        Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure TCnHashedAnsiStringList.UpdateValueHash;
var
  I: Integer;
begin
  if FValueHashValid then Exit;
  
  if FValueHash = nil then
    FValueHash := TCnAnsiStringHash.Create
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    if not CaseSensitive then
      FValueHash.Add(AnsiString(AnsiUpperCase(string(Self[I]))), I)
    else
      FValueHash.Add(Self[I], I);
  FValueHashValid := True;
end;

end.
