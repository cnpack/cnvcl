{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2012 CnPack 开发组                       }
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

unit CnDHibernateUtils;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate基础库
* 单元名称：工具函数库
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

{$WARN SYMBOL_PLATFORM OFF}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Registry, Classes, SysUtils, DB, DBTables, IniFiles, Forms, Messages,
  Controls, Dialogs, Consts, DBConsts, CnDHibernateConsts, CnDHibernatePodoList,
  DBClient, TypInfo, ComObj, ActiveX, CnDHibernateCalc,
  xmldom, XMLIntf, msxmldom, XMLDoc, ComCtrls, CnDHibernateBase, ADODB;

type

  { TLocateObject }
  TCnLocateObject = class(TObject)
  private
    FDataSet: TDataSet;
    FLookupField: TField;
    FLookupValue: string;
    FLookupExact, FCaseSensitive: Boolean;
    FBookmark: TBookmark;
    FIndexSwitch: Boolean;
    procedure SetDataSet(Value: TDataSet);
  protected
    function MatchesLookup(Field: TField): Boolean;
    procedure CheckFieldType(Field: TField); virtual;
    procedure ActiveChanged; virtual;
    function LocateFilter: Boolean; virtual;
    function LocateKey: Boolean; virtual;
    function LocateFull: Boolean; virtual;
    function UseKey: Boolean; virtual;
    function FilterApplicable: Boolean; virtual;
    property LookupField: TField read FLookupField;
    property LookupValue: string read FLookupValue;
    property LookupExact: Boolean read FLookupExact;
    property CaseSensitive: Boolean read FCaseSensitive;
    property Bookmark: TBookmark read FBookmark write FBookmark;
  public
    function Locate(const KeyField, KeyValue: string; Exact,
      CaseSensitive: Boolean): Boolean;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property IndexSwitch: Boolean read FIndexSwitch write FIndexSwitch;
  end;

type
  TCnCreateLocateObject = function: TCnLocateObject;

const
  CreateLocateObject: TCnCreateLocateObject = nil;
function CreateLocate(DataSet: TDataSet): TCnLocateObject;

{ Utility routines }

function IsDataSetEmpty(DataSet: TDataSet): Boolean;
procedure RefreshQuery(Query: TDataSet);
function DataSetSortedSearch(DataSet: TDataSet; const Value,
  FieldName: string; CaseInsensitive: Boolean): Boolean;
function DataSetSectionName(DataSet: TDataSet): string;
procedure InternalSaveFields(DataSet: TDataSet; IniFile: TObject;
  const Section: string);
procedure InternalRestoreFields(DataSet: TDataSet; IniFile: TObject;
  const Section: string; RestoreVisible: Boolean);
function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
procedure SaveFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile);
procedure RestoreFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile;
  RestoreVisible: Boolean);
procedure SaveFields(DataSet: TDataSet; IniFile: TIniFile);
procedure RestoreFields(DataSet: TDataSet; IniFile: TIniFile;
  RestoreVisible: Boolean);
procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
function ConfirmDelete: Boolean;
procedure ConfirmDataSetCancel(DataSet: TDataSet);
procedure CheckRequiredField(Field: TField);
procedure CheckRequiredFields(const Fields: array of TField);

{ SQL expressions }

function DateToSQL(Value: TDateTime): string;
function FormatSQLDateRange(Date1, Date2: TDateTime;
  const FieldName: string): string;
function FormatSQLDateRangeEx(Date1, Date2: TDateTime;
  const FieldName: string): string;
function FormatSQLNumericRange(const FieldName: string;
  LowValue, HighValue, LowEmpty, HighEmpty: Double; Inclusive: Boolean): string;
function StrMaskSQL(const Value: string): string;
function FormatSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;
function FormatAnsiSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;

{ convert utils }
function XMLStreamToPodoList(TableName: string; XMLStream: TStream): TCnPodoList;

procedure XMLDomToTree(XMLNode: IXMLNode; TreeNode: TTreeNode; BaseNodes:
  TTreeNodes);
procedure XMLDomToTreeNoValue(XMLNode: IXMLNode; TreeNode: TTreeNode; BaseNodes:
  TTreeNodes);

{ GUID }
function GenerateGUID: string;

{ Calc Expression }
function CalcExpress(expr: string; args: array of Extended): Variant;

function StringMapToHashMap(map: TStringList): ICnMap;
function GuidRW(str: string; g: string; id: integer): string;
procedure SetGUID(index: Integer; str: string; id: Integer);
function GetGUID(index: Integer; id: Integer): string;
function GetExpressValue(Expr: string; ds: TDataSet): Variant;
function GetFormulaValue(hql: string; ds: TDataSet): Variant;

const
  ServerDateFmt     : string[50] = DH_DATE_FMT_STD_16;

const
  ftBlobTypes       = [ftBlob..ftTypedBinary];

type
  TCnEventRec = class
  public
    field: string;
    expr: string;
    value: Variant;
  end;

var
  WaitCount         : Integer = 0;
  SaveCursor        : TCursor = crDefault;
  Guids             : array of array of string;

procedure _DBError(const Msg: string);

{ general functions }
function ResStr(const Ident: string): string;
function Max(A, B: Longint): Longint;
function Min(A, B: Longint): Longint;
function AllocMemo(Size: Longint): Pointer;
function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
procedure FreeMemo(var fpBlock: Pointer);
procedure StartWait;
procedure StopWait;
procedure SwitchToWindow(Wnd: HWnd; Restore: Boolean);
function NormalDir(const DirName: string): string;
function ExcelEventToMap(eve: TStringList): TCnPodoList;
function ExcelEventExpressions(expr: string; excel: Variant; row: Integer; map:
  ICnMap; ds: TDataSet): Variant;
function SplitString(Source, Deli: string): TStringList;
function ExcelConvert(expr: string; excel: Variant; row: Integer; map: ICnMap; ds:
  TDataSet): Variant;
function GetXmlFieldConvertArray(expr: string): TStringList;
function GetXmlExpress(Expr: string): Variant;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

uses
  CnDHibernateAppUtils, CnDHibernateStringUtils, CnDHibernateDateUtils;

function GetXmlExpress(Expr: string): Variant;
var
  i                 : Integer;
  upExp             : string;
begin
  upExp := UpperCase(Expr);
  for i := 1 to Length(upExp) do
  begin
    if (upExp[i] >= 'A') and (upExp[i] <= 'Z') then
    begin
      Expr := Copy(Expr, 2, Length(Expr) - 2);
      expr := StringReplace(expr, '+', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
      expr := StringReplace(expr, '-', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
      expr := StringReplace(expr, '*', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
      expr := StringReplace(expr, '/', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
      Result := Expr;
      Exit;
    end;
  end;
  with TCnDHibernateCalc.Create do
  begin
    Formula := Expr;
    try
      Result := calc([]);
    except
      Result := Expr;
    end;
    Free;
  end;
end;

function GetXmlFieldConvertArray(expr: string): TStringList;
var
  sl                : TStringList;
  i                 : Integer;
begin
  sl := SplitString(expr, 'X-');
  for i := sl.Count - 1 downto 0 do
  begin
    if Pos(']', sl[i]) > 0 then
      sl[i] := LeftStr(sl[i], Pos(']', sl[i]) - 1);
    if Pos('[', sl[i]) > 0 then
      sl.Delete(i);
  end;
  Result := sl;
end;

function ExcelConvert(expr: string; excel: Variant; row: Integer; map: ICnMap; ds:
  TDataSet): Variant;
var
  sl                : TStringList;
  vl                : TStringList;
  i, j              : Integer;
  nv                : Variant;
  nd                : string;
begin
  sl := GetXmlFieldConvertArray(expr);
  vl := TStringList.Create;
  j := 1;
  for i := 0 to sl.Count - 1 do
  begin
    application.ProcessMessages;
    Nd := excel.Cells[1, j].Value;
    while nd <> emptystr do
    begin
      if nd = sl[i] then
      begin
        nv := excel.Cells[row, j];
        vl.Add(nv);
      end;
      Inc(j);
      nd := excel.cells[row, j];
    end;
  end;
  for i := 0 to sl.Count - 1 do
  begin
    expr := StringReplace(expr, '[X-' + sl[i] + ']', vl[i], [rfReplaceAll,
      rfIgnoreCase]);
  end;
  try
    Result := GetXmlExpress(expr);
  except
    expr := Copy(expr, 2, Length(expr) - 2); // 去掉括号
    expr := StringReplace(expr, '+', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
    expr := StringReplace(expr, '-', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
    expr := StringReplace(expr, '*', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
    expr := StringReplace(expr, '/', EmptyStr, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function SplitString(Source, Deli: string): TStringList;
var
  EndOfCurrentString: byte;
  StringList        : TStringList;
begin
  StringList := TStringList.Create;
  while Pos(Deli, Source) > 0 do
  begin
    EndOfCurrentString := Pos(Deli, Source);
    StringList.add(Copy(Source, 1, EndOfCurrentString - 1));
    Source := Copy(Source, EndOfCurrentString + length(Deli), length(Source) -
      EndOfCurrentString);
  end;
  Result := StringList;
  StringList.Add(source);
end;

function ExcelEventExpressions(expr: string; excel: Variant; row: Integer; map:
  ICnMap; ds: TDataSet): Variant;
var
  sl                : TStringList;
  pl                : TCnPodoList;
  nd                : string;
  i, j              : Integer;
  nv                : Variant;
  fn                : string;
  ex                : string;
begin
  //
  Result := EmptyStr;
  sl := SplitString(expr, ';');
  pl := ExcelEventToMap(sl);
  j := 1;
  for i := 0 to pl.Count - 1 do
  begin
    Application.ProcessMessages;
    nd := excel.Cells[1, j].Value;
    while nd <> EmptyStr do
    begin
      fn := TCnEventRec(pl.Objects[i]).field;
      if { 3 }  Pos('X-', fn) > 0 then
      begin
        fn := RightStr(fn, Length(fn) - 2);
        if { 2 }  nd = fn then
        begin
          // found node!
          nv := excel.Cells[row, j].Value;
          ex := TCnEventRec(pl.Objects[i]).expr;
          if Pos('''', ex) > 0 then
            ex := Copy(ex, 2, Length(ex) - 2);
          if { 1 }  nv = ex then
          begin
            Result := TCnEventRec(pl.Objects[i]).value;
            if Pos('''', Result) > 0 then
              Result := Copy(result, 2, Length(Result) - 2);
            Exit;
          end;                          { if 1 }
        end;                            { if 2 }
      end                               { if 3 }
      else
      begin
        // db field
        nv := TADOTable(ds).fieldByName(fn).Value;
        ex := TCnEventRec(pl.Objects[i]).expr;
        if Pos('''', ex) > 0 then
          ex := Copy(ex, 2, Length(ex) - 2);
        if nv = ex then
        begin
          Result := TCnEventRec(pl.Objects[i]).value;
          if Pos('''', Result) > 0 then
            Result := Copy(result, 2, Length(Result) - 2);
          Exit;
        end;
      end;
      Inc(j);
      nd := excel.cells[row, j];
    end;                                { while }
  end;                                  { for }
end;

function ExcelEventToMap(eve: TStringList): TCnPodoList;
var
  i                 : Integer;
  f, e, v           : string;
  er                : TCnEventRec;
begin
  Result := TCnPodoList.Create;
  for i := 0 to eve.Count - 1 do
  begin
    // if([Field]=expr):Value;
    //    4     10
    f := Copy(eve[i], Pos('[', eve[i]) + 1, Pos(']', eve[i]) - pos('[', eve[i])
      - 1);
    e := Copy(eve[i], Pos('=', eve[i]) + 1, Pos(')', eve[i]) - pos('=', eve[i])
      - 1);
    v := Copy(eve[i], Pos(':', eve[i]) + 1, Length(eve[i]) - pos(':', eve[i]));
    if f = emptyStr then Continue;
    er := TCnEventRec.Create;
    er.field := f;
    er.expr := e;
    er.value := v;
    Result.Add(er);
  end;
end;

function GetExpressValue(Expr: string; ds: TDataSet): Variant;
var
  i                 : integer;
  tmp               : string;
begin
  tmp := Expr;

  Result := 0;
  // replace variants to fields value
  for i := 0 to ds.FieldCount - 1 do
  begin
    try
      Expr := StringReplace(Expr, '[' + ds.Fields[i].FieldName + ']',
        ds.Fields[i].Value,
        [rfReplaceAll, rfIgnoreCase]);
    except

    end;
  end;
  with TCnDHibernateCalc.Create do
  begin
    Formula := Expr;
    try
      Result := calc([]);
    except
      Result := tmp;
    end;
    Free;
  end;
end;

function GetFormulaValue(hql: string; ds: TDataSet): Variant;
var
  qry               : TADOQuery;
  i                 : Integer;
begin
  qry := TADOQuery.Create(nil);
  qry.Connection := TADOTable(ds).Connection;
  for i := 0 to ds.FieldCount - 1 do
  begin
    try
      hql := StringReplace(hql, ds.Fields[i].FieldName, ds.Fields[i].Value,
        [rfREplaceAll, rfIgnoreCase]);
    except
    end;
  end;
  qry.SQL.Text := hql;
  qry.Open;
  if qry.RecordCount > 0 then
    Result := qry.Fields[0].Value
  else
    Result := EmptyStr;
  qry.Close;
  qry.Free;
end;

procedure SetGUID(index: Integer; str: string; id: Integer);
begin
  Guids[index, id] := str;
end;

function GetGUID(index: Integer; id: Integer): string;
begin
  Result := guids[index, id];
end;

function GuidRW(str: string; g: string; id: integer): string;
var
  wPos, rPos        : Integer;
  idx               : Integer;
begin
  // w = write
  wPos := Pos('w', str);
  // r = read
  rPos := Pos('r', str);
  // write guid
  if wPos > 0 then
  begin
    idx := StrToInt(trim(Copy(str, wPos + 1, Length(str) - wpos - 2)));
    SetGUID(idx, g, id);
    Result := 'w';                      // 表示已写入
    Exit;
  end;
  if rPos > 0 then
  begin
    // writelog('Reading GUID');
    idx := StrToInt(trim(Copy(str, rPos + 1, Length(str) - rPos - 2)));
    Result := GetGUID(idx, id);         // 返回 GUID
  end;
end;

function StringMapToHashMap(map: TStringList): ICnMap;
var
  i                 : Integer;
  s, n, v           : string;
  p                 : Integer;
begin
  Result := TCnDHHashMap.Create;
  for i := 0 to map.Count - 1 do
  begin
    s := map[i];
    p := Pos('=', s);
    n := Copy(s, 1, p - 1);
    v := map.Values[n];
    Result.put(n, v);
  end;
end;

procedure XMLDomToTree(XMLNode: IXMLNode; TreeNode: TTreeNode; BaseNodes:
  TTreeNodes);
var
  NewTreeNode       : TTreeNode;
  NodeText          : string;
  AttrNode          : IXMLNode;
  i                 : Integer;
begin
  if not (XMLNode.NodeType = ntElement) then Exit;
  NodeText := XMLNode.NodeName;
  if XMLNode.IsTextElement then
    NodeText := NodeText + '=' + XMLNode.NodeValue;
  NewTreeNode := BaseNodes.AddChild(TreeNode, NodeText);
  for i := 0 to XMLNode.AttributeNodes.Count - 1 do
  begin
    AttrNode := XMLNode.AttributeNodes.Nodes[i];
    BaseNodes.AddChild(NewTreeNode, '[' + AttrNode.NodeName + '="' +
      AttrNode.Text + '"]');
  end;
  if XMLNode.HasChildNodes then
    for i := 0 to XMLNode.ChildNodes.Count - 1 do
      XMLDomToTree(XMLNode.ChildNodes.Nodes[i], NewTreeNode, baseNodes);
end;

procedure XMLDomToTreeNoValue(XMLNode: IXMLNode; TreeNode: TTreeNode; BaseNodes:
  TTreeNodes);
var
  NewTreeNode       : TTreeNode;
  NodeText          : string;
  AttrNode          : IXMLNode;
  i                 : Integer;
begin
  if not (XMLNode.NodeType = ntElement) then Exit;
  NodeText := XMLNode.NodeName;
  //  if XMLNode.IsTextElement then
  //    NodeText := NodeText + '=' + XMLNode.NodeValue;
  NewTreeNode := BaseNodes.AddChild(TreeNode, NodeText);
  for i := 0 to XMLNode.AttributeNodes.Count - 1 do
  begin
    AttrNode := XMLNode.AttributeNodes.Nodes[i];
    BaseNodes.AddChild(NewTreeNode, '[' + AttrNode.NodeName { + '="' +
      AttrNode.Text }+ ']');
  end;
  if XMLNode.HasChildNodes then
    for i := 0 to XMLNode.ChildNodes.Count - 1 do
      XMLDomToTreeNoValue(XMLNode.ChildNodes.Nodes[i], NewTreeNode, baseNodes);
end;

function CalcExpress(expr: string; args: array of Extended): Variant;
begin
  with TCnDHibernateCalc.Create do
  begin
    Formula := expr;
    try
      Result := calc(args);
    except
      Result := EmptyStr;
    end;
    Free;
  end;
end;

function GenerateGUID: string;
var
  id                : tguid;
begin
  if CoCreateGuid(id) = s_ok then
    result := guidtostring(id);
end;

{ Utility routines }

procedure _DBError(const Msg: string);
begin
  DatabaseError(Msg);
end;

function ConfirmDelete: Boolean;
begin
  Screen.Cursor := crDefault;
  Result := MessageDlg(ResStr(SDeleteRecordQuestion), mtConfirmation,
    [mbYes, mbNo], 0) = mrYes;
end;

procedure ConfirmDataSetCancel(DataSet: TDataSet);
begin
  if DataSet.State in [dsEdit, dsInsert] then
  begin
    DataSet.UpdateRecord;
    if DataSet.Modified then
    begin
      case MessageDlg(LoadStr(DH_CONFIRM_SAVE), mtConfirmation, mbYesNoCancel, 0)
        of
        mrYes: DataSet.Post;
        mrNo: DataSet.Cancel;
        else
          SysUtils.Abort;
      end;
    end
    else
      DataSet.Cancel;
  end;
end;

function SetToBookmark(ADataSet: TDataSet; ABookmark: TBookmark): Boolean;
begin
  Result := False;
  with ADataSet do
    if Active and (ABookmark <> nil) and not (Bof and Eof) and
      BookmarkValid(ABookmark) then
    try
      ADataSet.GotoBookmark(ABookmark);
      Result := True;
    except
    end;
end;

{ Refresh Query procedure }

procedure RefreshQuery(Query: TDataSet);
var
  BookMk            : TBookmark;
begin
  with Query do
  begin
    DisableControls;
    try
      if Active then
        BookMk := GetBookmark
      else
        BookMk := nil;
      try
        Close;
        Open;
        SetToBookmark(Query, BookMk);
      finally
        if BookMk <> nil then
          FreeBookmark(BookMk);
      end;
    finally
      EnableControls;
    end;
  end;
end;

{ TCnLocateObject }

procedure TCnLocateObject.SetDataSet(Value: TDataSet);
begin
  ActiveChanged;
  FDataSet := Value;
end;

function TCnLocateObject.LocateFull: Boolean;
begin
  Result := False;
  with DataSet do
  begin
    First;
    while not EOF do
    begin
      if MatchesLookup(FLookupField) then
      begin
        Result := True;
        Break;
      end;
      Next;
    end;
  end;
end;

function TCnLocateObject.LocateKey: Boolean;
begin
  Result := False;
end;

function TCnLocateObject.FilterApplicable: Boolean;
begin
  Result := FLookupField.FieldKind in [fkData, fkInternalCalc];
end;

function TCnLocateObject.LocateFilter: Boolean;
var
  SaveCursor        : TCursor;
  Options           : TLocateOptions;
  Value             : Variant;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Options := [];
    if not FCaseSensitive then
      Include(Options, loCaseInsensitive);
    if not FLookupExact then
      Include(Options, loPartialKey);
    if (FLookupValue = '') then
      VarClear(Value)
    else
      Value := FLookupValue;
    Result := DataSet.Locate(FLookupField.FieldName, Value, Options);
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TCnLocateObject.CheckFieldType(Field: TField);
begin
end;

function TCnLocateObject.Locate(const KeyField, KeyValue: string;
  Exact, CaseSensitive: Boolean): Boolean;
var
  LookupKey         : TField;
begin
  if DataSet = nil then
  begin
    Result := False;
    Exit;
  end;
  DataSet.CheckBrowseMode;
  LookupKey := DataSet.FieldByName(KeyField);
  DataSet.CursorPosChanged;
  FLookupField := LookupKey;
  FLookupValue := KeyValue;
  FLookupExact := Exact;
  FCaseSensitive := CaseSensitive;
  if FLookupField.DataType <> ftString then
  begin
    FCaseSensitive := True;
    try
      CheckFieldType(FLookupField);
    except
      Result := False;
      Exit;
    end;
  end;
  FBookmark := DataSet.GetBookmark;
  try
    DataSet.DisableControls;
    try
      Result := MatchesLookup(FLookupField);
      if not Result then
      begin
        if UseKey then
          Result := LocateKey
        else
        begin
          if FilterApplicable then
            Result := LocateFilter
          else
            Result := LocateFull;
        end;
        if not Result then
          SetToBookmark(DataSet, FBookmark);
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    FLookupValue := EmptyStr;
    FLookupField := nil;
    DataSet.FreeBookmark(FBookmark);
    FBookmark := nil;
  end;
end;

function TCnLocateObject.UseKey: Boolean;
begin
  Result := False;
end;

procedure TCnLocateObject.ActiveChanged;
begin
end;

function TCnLocateObject.MatchesLookup(Field: TField): Boolean;
var
  Temp              : string;
begin
  Temp := Field.AsString;
  if not FLookupExact then
    SetLength(Temp, Min(Length(FLookupValue), Length(Temp)));
  if FCaseSensitive then
    Result := AnsiCompareStr(Temp, FLookupValue) = 0
  else
    Result := AnsiCompareText(Temp, FLookupValue) = 0;
end;

function CreateLocate(DataSet: TDataSet): TCnLocateObject;
begin
  if Assigned(CreateLocateObject) then
    Result := CreateLocateObject
  else
    Result := TCnLocateObject.Create;
  if (Result <> nil) and (DataSet <> nil) then
    Result.DataSet := DataSet;
end;

{ DataSet locate routines }

function DataSetLocateThrough(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  FieldCount        : Integer;
  Fields            : TList;
  Bookmark          : {$IFDEF DELPHI2009_UP} TBookmark {$ELSE} string {$ENDIF};

  function CompareField(Field: TField; Value: Variant): Boolean;
  var
    S               : string;
  begin
    if Field.DataType = ftString then
    begin
      S := Field.AsString;
      if (loPartialKey in Options) then
        Delete(S, Length(Value) + 1, MaxInt);
      if (loCaseInsensitive in Options) then
        Result := AnsiCompareText(S, Value) = 0
      else
        Result := AnsiCompareStr(S, Value) = 0;
    end
    else
      Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I               : Integer;
  begin
    if FieldCount = 1 then
      Result := CompareField(TField(Fields.First), KeyValues)
    else
    begin
      Result := True;
      for I := 0 to FieldCount - 1 do
        Result := Result and CompareField(TField(Fields[I]), KeyValues[I]);
    end;
  end;

begin
  Result := False;
  with DataSet do
  begin
    CheckBrowseMode;
    if BOF and EOF then
      Exit;
  end;
  Fields := TList.Create;
  try
    DataSet.GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Result := CompareRecord;
    if Result then
      Exit;
    DataSet.DisableControls;
    try
      Bookmark := DataSet.Bookmark;
      try
        with DataSet do
        begin
          First;
          while not EOF do
          begin
            Result := CompareRecord;
            if Result then
              Break;
            Next;
          end;
        end;
      finally
        if not Result and
          DataSet.BookmarkValid({$IFNDEF DELPHI2009_UP}PChar {$ENDIF} (Bookmark)) then
          DataSet.Bookmark := Bookmark;
      end;
    finally
      DataSet.EnableControls;
    end;
  finally
    Fields.Free;
  end;
end;

{ DataSetSortedSearch. Navigate on sorted DataSet routine. }

function DataSetSortedSearch(DataSet: TDataSet; const Value,
  FieldName: string; CaseInsensitive: Boolean): Boolean;
var
  L, H, I           : Longint;
  CurrentPos        : Longint;
  CurrentValue      : string;
  BookMk            : TBookmark;
  Field             : TField;

  function UpStr(const Value: string): string;
  begin
    if CaseInsensitive then
      Result := AnsiUpperCase(Value)
    else
      Result := Value;
  end;

  function GetCurrentStr: string;
  begin
    Result := Field.AsString;
    if Length(Result) > Length(Value) then
      SetLength(Result, Length(Value));
    Result := UpStr(Result);
  end;

begin
  Result := False;
  if DataSet = nil then
    Exit;
  Field := DataSet.FindField(FieldName);
  if Field = nil then
    Exit;
  if Field.DataType = ftString then
  begin
    DataSet.DisableControls;
    BookMk := DataSet.GetBookmark;
    try
      L := 0;
      DataSet.First;
      CurrentPos := 0;
      H := DataSet.RecordCount - 1;
      if Value <> '' then
      begin
        while L <= H do
        begin
          I := (L + H) shr 1;
          if I <> CurrentPos then
            DataSet.MoveBy(I - CurrentPos);
          CurrentPos := I;
          CurrentValue := GetCurrentStr;
          if (UpStr(Value) > CurrentValue) then
            L := I + 1
          else
          begin
            H := I - 1;
            if (UpStr(Value) = CurrentValue) then
              Result := True;
          end;
        end;                            { while }
        if Result then
        begin
          if (L <> CurrentPos) then
            DataSet.MoveBy(L - CurrentPos);
          while (L < DataSet.RecordCount) and
            (UpStr(Value) <> GetCurrentStr) do
          begin
            Inc(L);
            DataSet.MoveBy(1);
          end;
        end;
      end
      else
        Result := True;
      if not Result then
        SetToBookmark(DataSet, BookMk);
    finally
      DataSet.FreeBookmark(BookMk);
      DataSet.EnableControls;
    end;
  end
  else
    DatabaseErrorFmt(SFieldTypeMismatch, [Field.DisplayName]);
end;

{ Save and restore DataSet Fields layout }

function DataSetSectionName(DataSet: TDataSet): string;
begin
  with DataSet do
    if (Owner <> nil) and (Owner is TCustomForm) then
      Result := GetDefaultSection(Owner as TCustomForm)
    else
      Result := Name;
end;

function CheckSection(DataSet: TDataSet; const Section: string): string;
begin
  Result := Section;
  if Result = '' then
    Result := DataSetSectionName(DataSet);
end;

procedure InternalSaveFields(DataSet: TDataSet; IniFile: TObject;
  const Section: string);
var
  I                 : Integer;
begin
  with DataSet do
  begin
    for I := 0 to FieldCount - 1 do
    begin
      IniWriteString(IniFile, CheckSection(DataSet, Section),
        Name + Fields[I].FieldName,
        Format('%d,%d,%d', [Fields[I].Index, Fields[I].DisplayWidth,
        Integer(Fields[I].Visible)]));
    end;
  end;
end;

procedure InternalRestoreFields(DataSet: TDataSet; IniFile: TObject;
  const Section: string; RestoreVisible: Boolean);
type
  TFieldInfo = record
    Field: TField;
    EndIndex: Integer;
  end;
  PFieldArray = ^TFieldArray;
  TFieldArray = array[0..(65528 div SizeOf(TFieldInfo)) - 1] of TFieldInfo;
const
  Delims            = [' ', ','];
var
  I, J              : Integer;
  S                 : string;
  FieldArray        : PFieldArray;
begin
  with DataSet do
  begin
    FieldArray := AllocMemo(FieldCount * SizeOf(TFieldInfo));
    try
      for I := 0 to FieldCount - 1 do
      begin
        S := IniReadString(IniFile, CheckSection(DataSet, Section),
          Name + Fields[I].FieldName, EmptyStr);
        FieldArray^[I].Field := Fields[I];
        FieldArray^[I].EndIndex := Fields[I].Index;
        if S <> '' then
        begin
          FieldArray^[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
            FieldArray^[I].EndIndex);
          Fields[I].DisplayWidth := StrToIntDef(ExtractWord(2, S, Delims),
            Fields[I].DisplayWidth);
          if RestoreVisible then
            Fields[I].Visible := Boolean(StrToIntDef(ExtractWord(3, S, Delims),
              Integer(Fields[I].Visible)));
        end;
      end;
      for I := 0 to FieldCount - 1 do
      begin
        for J := 0 to FieldCount - 1 do
        begin
          if FieldArray^[J].EndIndex = I then
          begin
            FieldArray^[J].Field.Index := FieldArray^[J].EndIndex;
            Break;
          end;
        end;
      end;
    finally
      FreeMemo(Pointer(FieldArray));
    end;
  end;
end;

procedure SaveFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile);
begin
  InternalSaveFields(DataSet, IniFile, DataSetSectionName(DataSet));
end;

procedure RestoreFieldsReg(DataSet: TDataSet; IniFile: TRegIniFile;
  RestoreVisible: Boolean);
begin
  InternalRestoreFields(DataSet, IniFile, DataSetSectionName(DataSet),
    RestoreVisible);
end;

procedure SaveFields(DataSet: TDataSet; IniFile: TIniFile);
begin
  InternalSaveFields(DataSet, IniFile, DataSetSectionName(DataSet));
end;

procedure RestoreFields(DataSet: TDataSet; IniFile: TIniFile;
  RestoreVisible: Boolean);
begin
  InternalRestoreFields(DataSet, IniFile, DataSetSectionName(DataSet),
    RestoreVisible);
end;

function IsDataSetEmpty(DataSet: TDataSet): Boolean;
begin
  with DataSet do
    Result := (not Active) or (Eof and Bof);
end;

{ SQL expressions }

function DateToSQL(Value: TDateTime): string;
begin
  Result := IntToStr(Trunc(Value));
end;

function FormatSQLDateRange(Date1, Date2: TDateTime;
  const FieldName: string): string;
begin
  Result := DH_TRUE_EXPRESS;
  if (Date1 = Date2) and (Date1 <> NullDate) then
  begin
    Result := Format('%s = %s', [FieldName, FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt),
        Date1)]);
  end
  else
    if (Date1 <> NullDate) or (Date2 <> NullDate) then
    begin
      if Date1 = NullDate then
        Result := Format('%s < %s', [FieldName,
          FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), IncDay(Date2, 1))])
      else
        if Date2 = NullDate then
          Result := Format('%s > %s', [FieldName,
            FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), IncDay(Date1, -1))])
        else
          Result := Format('(%s < %s) AND (%s > %s)',
            [FieldName, FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), IncDay(Date2, 1)),
            FieldName, FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), IncDay(Date1, -1))]);
    end;
end;

function FormatSQLDateRangeEx(Date1, Date2: TDateTime;
  const FieldName: string): string;
begin
  Result := DH_TRUE_EXPRESS;
  if (Date1 <> NullDate) or (Date2 <> NullDate) then
  begin
    if Date1 = NullDate then
      Result := Format('%s < %s', [FieldName,
        FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), IncDay(Date2, 1))])
    else
      if Date2 = NullDate then
        Result := Format('%s >= %s', [FieldName,
          FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), Date1)])
      else
        Result := Format('(%s < %s) AND (%s >= %s)',
          [FieldName, FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), IncDay(Date2, 1)),
          FieldName, FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), Date1)]);
  end;
end;

function FormatSQLNumericRange(const FieldName: string;
  LowValue, HighValue, LowEmpty, HighEmpty: Double; Inclusive: Boolean): string;
const
  Operators         : array[Boolean, 1..2] of string[2] = (('>', '<'), ('>=',
    '<='));
begin
  Result := DH_TRUE_EXPRESS;
  if (LowValue = HighValue) and (LowValue <> LowEmpty) then
  begin
    Result := Format('%s = %g', [FieldName, LowValue]);
  end
  else
    if (LowValue <> LowEmpty) or (HighValue <> HighEmpty) then
    begin
      if LowValue = LowEmpty then
        Result := Format('%s %s %g', [FieldName, Operators[Inclusive, 2],
          HighValue])
      else
        if HighValue = HighEmpty then
          Result := Format('%s %s %g', [FieldName, Operators[Inclusive, 1],
            LowValue])
        else
        begin
          Result := Format('(%s %s %g) AND (%s %s %g)',
            [FieldName, Operators[Inclusive, 2], HighValue,
            FieldName, Operators[Inclusive, 1], LowValue]);
        end;
    end;
end;

function StrMaskSQL(const Value: string): string;
begin
  if (Pos('*', Value) = 0) and (Pos('?', Value) = 0) and (Value <> '') then
    Result := '*' + Value + '*'
  else
    Result := Value;
end;

function FormatSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;
var
  EmptyValue        : Boolean;
  FieldValue        : string;
  DateValue         : TDateTime;
  LogicOperator     : string;
begin
  FieldValue := '';
  DateValue := NullDate;
  Exact := Exact or not (FieldType in
    [ftString, ftDate, ftTime, ftDateTime]);
  if FieldType in [ftDate, ftTime, ftDateTime] then
  begin
    DateValue := StrToDateDef(Value, NullDate);
    EmptyValue := (DateValue = NullDate);
    FieldValue := FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), DateValue);
  end
  else
  begin
    FieldValue := Value;
    EmptyValue := FieldValue = '';
    if not (Exact or EmptyValue) then
      FieldValue := ReplaceStr(ReplaceStr(StrMaskSQL(FieldValue),
        '*', '%'), '?', '_');
    if FieldType = ftString then
      FieldValue := '''' + FieldValue + '''';
  end;
  LogicOperator := Operator;
  if LogicOperator = '' then
  begin
    if Exact then
      LogicOperator := '='
    else
    begin
      if FieldType = ftString then
        LogicOperator := 'LIKE'
      else
        LogicOperator := '>=';
    end;
  end;
  if EmptyValue then
    Result := DH_TRUE_EXPRESS
  else
    if (FieldType = ftDateTime) and Exact then
    begin
      DateValue := IncDay(DateValue, 1);
      Result := Format('(%s >= %s) and (%s < %s)', [FieldName, FieldValue,
        FieldName, FormatDateTime({$IFDEF DELPHI12_UP}String{$ENDIF}(ServerDateFmt), DateValue)]);
    end
    else
      Result := Format('%s %s %s', [FieldName, LogicOperator, FieldValue]);
end;

function FormatAnsiSQLCondition(const FieldName, Operator, Value: string;
  FieldType: TFieldType; Exact: Boolean): string;
var
  S, Esc            : string;
begin
  Esc := '';
  if not Exact and (FieldType = ftString) then
  begin
    S := ReplaceStr(ReplaceStr(ReplaceStr(Value, '/', '//'),
      '_', '/_'), '%', '/%');
    if S <> Value then
      Esc := ' ESCAPE''/''';
  end
  else
    S := Value;
  Result := FormatSQLCondition(FieldName, Operator, S, FieldType, Exact) + Esc;
end;

procedure CheckRequiredField(Field: TField);
begin
  with Field do
    if not ReadOnly and not Calculated and IsNull then
    begin
      FocusControl;
      DatabaseErrorFmt(SFieldRequired, [DisplayName]);
    end;
end;

procedure CheckRequiredFields(const Fields: array of TField);
var
  I                 : Integer;
begin
  for I := Low(Fields) to High(Fields) do
    CheckRequiredField(Fields[I]);
end;

procedure AssignRecord(Source, Dest: TDataSet; ByName: Boolean);
var
  I                 : Integer;
  F, FSrc           : TField;
begin
  if not (Dest.State in dsEditModes) then
    _DBError(SNotEditing);
  if ByName then
  begin
    for I := 0 to Source.FieldCount - 1 do
    begin
      F := Dest.FindField(Source.Fields[I].FieldName);
      if F <> nil then
      begin
        F.Value := Source.Fields[I].Value;
      end;
    end;
  end
  else
  begin
    for I := 0 to Min(Source.FieldDefs.Count - 1, Dest.FieldDefs.Count - 1) do
    begin
      F := Dest.FindField(Dest.FieldDefs[I].Name);
      FSrc := Source.FindField(Source.FieldDefs[I].Name);
      if (F <> nil) and (FSrc <> nil) then
      begin
        F.Value := FSrc.Value;
      end;
    end;
  end;
end;

function ResStr(const Ident: string): string;
begin
  Result := Ident;
end;

function Max(A, B: Longint): Longint;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: Longint): Longint;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function AllocMemo(Size: Longint): Pointer;
begin
  if Size > 0 then
    Result := GlobalAllocPtr(HeapAllocFlags or GMEM_ZEROINIT, Size)
  else
    Result := nil;
end;

function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
begin
  Result := GlobalReallocPtr(fpBlock, Size,
    HeapAllocFlags or GMEM_ZEROINIT);
end;

procedure FreeMemo(var fpBlock: Pointer);
begin
  if fpBlock <> nil then
  begin
    GlobalFreePtr(fpBlock);
    fpBlock := nil;
  end;
end;

procedure StartWait;
begin
  if WaitCount = 0 then
  begin
    SaveCursor := Screen.Cursor;
    // Screen.Cursor := WaitCursor;
  end;
  Inc(WaitCount);
end;

procedure StopWait;
begin
  if WaitCount > 0 then
  begin
    Dec(WaitCount);
    if WaitCount = 0 then
      Screen.Cursor := SaveCursor;
  end;
end;

procedure SwitchToWindow(Wnd: HWnd; Restore: Boolean);
begin
  if IsWindowEnabled(Wnd) then
  begin
    SetForegroundWindow(Wnd);
    if Restore and IsWindowVisible(Wnd) then
    begin
      if not IsZoomed(Wnd) then
        SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
      SetFocus(Wnd);
    end;
  end;
end;

function NormalDir(const DirName: string): string;
begin
  Result := DirName;
  if (Result <> '') and
    not ({$IFDEF DELPHI12_UP}CharInSet(AnsiLastChar(Result)^,  [':', '\']){$ELSE}AnsiLastChar(Result)^ in [':', '\']{$ENDIF}) then
  begin
    if (Length(Result) = 1) and ({$IFDEF DELPHI12_UP}CharInSet(UpCase(Result[1]), ['A'..'Z']){$ELSE}UpCase(Result[1]) in ['A'..'Z']{$ENDIF}) then
      Result := Result + ':\'
    else
      Result := Result + '\';
  end;
end;

/// <summary>
/// convert xml dom to podoList object
/// </summary>
/// <param name="TableName">table name</param>
/// <param name="XMLStream">stream contains dom</param>
/// <returns>podo list</returns>
function XMLStreamToPodoList(TableName: string; XMLStream: TStream): TCnPodoList;
var
  DC                : TClientDataSet;
  clazz             : TClass;
  i, j              : Integer;
  obj               : TObject;
  Classtypeinfo     : PTypeInfo;
  classDataInfo     : PTypeData;
  tk                : TypInfo.TTypeKind;
  Pplst             : PPropList;
begin
  // create and open xml datas
  DC := TClientDataSet.Create(nil);
  DC.LoadFromStream(XMLStream);
  DC.Open;
  // create result
  Result := TCnPodoList.Create;
  // create podo object
  clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
  Classtypeinfo := clazz.ClassInfo;
  classDataInfo := GetTypeData(Classtypeinfo);
  for i := 1 to dc.RecordCount do
  begin
    dc.RecNo := i;
    obj := clazz.NewInstance;
    if classDataInfo.PropCount <> 0 then
    begin
      GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
      try
        GetPropInfos(Classtypeinfo, Pplst);
        for j := 0 to classDataInfo.PropCount - 1 do
        begin
          if (RightStr({$IFDEF DELPHI12_UP}String{$ENDIF}(Pplst[j]^.Name), 8) = '_FORMULA') or
            (RightStr({$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name), 4) = '_SQL') then
            Continue;
          tk := Pplst[j]^.PropType^.Kind;
          if tk <> tkMethod then
          begin
            // set the string properties
            if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF} then
            begin
              SetStrProp((obj as clazz), {$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name),
                dc.FieldByName({$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name)).AsString);
            end;
            // set the integer properties
            if tk = tkInteger then
            begin
              try
                SetInt64Prop((obj as clazz), {$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name),
                  DC.FieldByName({$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name)).AsInteger);
              except
                SetInt64Prop((obj as clazz), {$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name), 0);
              end;
            end;
            // set the float properties
            if tk = tkFloat then
            begin
              try
                SetFloatProp((obj as clazz), {$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name),
                  DC.FieldByName({$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name)).AsFloat);
              except
                SetFloatProp((obj as clazz), {$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name), 0);
              end;
            end;
            // set the variant properties
            if tk = tkVariant then
            begin
              SetVariantProp((obj as clazz), {$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name),
                DC.FieldByName({$IFDEF DELPHI12_UP}String{$ENDIF}(pplst[j]^.Name)).Value);
            end;
          end;
        end;
      finally
        FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
      end;
    end;
    // ad to result
    Result.Add(obj as clazz);
  end;
  DC.Close;
  DC.Free;
end;

initialization
  SetLength(guids, 32, 32);

{$ENDIF SUPPORT_ADO}
end.

