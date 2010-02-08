{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2010 CnPack 开发组                       }
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

unit CnDHibernateMemData;  
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：内存表控件单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.08.23 V1.8
*               Delphi2009 无法支持此单元
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$WARN SYMBOL_DEPRECATED OFF}
{$M+}

{$IFDEF SUPPORT_ADO}

{$IFNDEF DELPHI2009_UP}

uses
  Windows, SysUtils, Classes, Controls, DB, CnDHibernateConsts,
  CnDHibernateUtils;

type
  TCnMemBlobData = string;

  TCnMemBlobArray = array[0..0] of TCnMemBlobData;

  PCnMemBlobArray = ^TCnMemBlobArray;

  TCnMemoryRecord = class;

  TCnLoadMode = (lmCopy, lmAppend);

  TCompareRecords = function(Item1, Item2: TCnMemoryRecord): Integer of object; 

{ TCnDHibernateMemData }

  TCnDHibernateMemData = class(TDataSet)
  private
    FRecordPos: Integer;
    FRecordSize: Integer;
    FBookmarkOfs: Integer;
    FBlobOfs: Integer;
    FRecBufSize: Integer;
    FOffsets: PWordArray;
    FLastID: Integer;
    FAutoInc: Longint;
    FActive: Boolean;
    FRecords: TList;
    FIndexList: TList;
    FCaseInsensitiveSort: Boolean;
    FDescendingSort: Boolean;
    FAbout: string;
    function AddRecord: TCnMemoryRecord;
    function InsertRecord(Index: Integer): TCnMemoryRecord;
    function FindRecordID(ID: Integer): TCnMemoryRecord;
    procedure CreateIndexList(const FieldNames: string);
    procedure FreeIndexList;
    procedure QuickSort(L, R: Integer; Compare: TCompareRecords);
    procedure Sort;
    function CalcRecordSize: Integer;
    function FindFieldData(Buffer: Pointer; Field: TField): Pointer;
    function GeTCnMemoryRecord(Index: Integer): TCnMemoryRecord;
    function GetCapacity: Integer;
    function RecordFilter: Boolean;
    procedure SetCapacity(Value: Integer);
    procedure ClearRecords;
    procedure InitBufferPointers(GetProps: Boolean);
  protected
    procedure AssignMemoryRecord(Rec: TCnMemoryRecord; Buffer: PChar);
    function GetActiveRecBuf(var RecBuf: PChar): Boolean; virtual;
    procedure InitFieldDefsFromFields;
    procedure RecordToBuffer(Rec: TCnMemoryRecord; Buffer: PChar);
    procedure SeTCnMemoryRecordData(Buffer: PChar; Pos: Integer); virtual;
    procedure SetAutoIncFields(Buffer: PChar); virtual;
    function CompareRecords(Item1, Item2: TCnMemoryRecord): Integer; virtual;
    function GetBlobData(Field: TField; Buffer: PChar): TCnMemBlobData;
    procedure SetBlobData(Field: TField; Buffer: PChar; Value: TCnMemBlobData);
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision, Decimals: Integer): Boolean;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure CloseBlob(Field: TField); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetIsIndexField(Field: TField): Boolean; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InitRecord(Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    property Records[Index: Integer]: TCnMemoryRecord read GeTCnMemoryRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetCurrentRecord(Buffer: PChar): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    procedure SortOnFields(const FieldNames: string;
      CaseInsensitive: Boolean = True; Descending: Boolean = False);
    procedure EmptyTable;
    procedure CopyStructure(Source: TDataSet);
    function LoadFromDataSet(Source: TDataSet; RecordCount: Integer;
      Mode: TCnLoadMode): Integer;
    function SaveToDataSet(Dest: TDataSet; RecordCount: Integer): Integer;
  published
    property About: string read FAbout write FAbout;
    property Capacity: Integer read GetCapacity write SetCapacity default 0;
    property Active;
    property AutoCalcFields;
    property Filtered;
    property FieldDefs;
    property ObjectView default False;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end; 

  { TCnMemBlobStream }

  TCnMemBlobStream = class(TStream)
  private
    FField: TBlobField;
    FDataSet: TCnDHibernateMemData;
    FBuffer: PChar;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: Longint;
    FCached: Boolean;
    function GetBlobSize: Longint;
    function GetBlobFromRecord(Field: TField): TCnMemBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end; 

  { TCnMemoryRecord }

  TCnMemoryRecord = class(TPersistent)
  private
    FMemoryData: TCnDHibernateMemData;
    FID: Integer;
    FData: Pointer;
    FBlobs: Pointer;
    function GetIndex: Integer;
    procedure SetMemoryData(Value: TCnDHibernateMemData; UpdateParent: Boolean);
  protected
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(MemoryData: TCnDHibernateMemData); virtual;
    constructor CreateEx(MemoryData: TCnDHibernateMemData; UpdateParent: Boolean); virtual;
    destructor Destroy; override;
    property MemoryData: TCnDHibernateMemData read FMemoryData;
    property ID: Integer read FID write FID;
    property Index: Integer read GetIndex write SetIndex;
    property Data: Pointer read FData;
  end; 

{$ENDIF DELPHI2009_UP}

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$IFNDEF DELPHI2009_UP}

uses
  ComObj, Variants, Forms, DbConsts;

const
  ftBlobTypes =[ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftOraBlob, ftOraClob];
  ftSupported =[ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat, ftCurrency, ftDate, ftTime, ftDateTime, ftAutoInc, ftBCD, ftBytes, ftVarBytes, ftADT, ftFixedChar, ftWideString, ftLargeint, ftVariant, ftGuid] + ftBlobTypes;
  fkStoredFields =[fkData];
  GuidSize = 38;  

{ Utility routines }

function CompareFields(Data1, Data2: Pointer; FieldType: TFieldType; CaseInsensitive: Boolean): Integer;
begin
  Result := 0;
  case FieldType of
    ftString:
      if CaseInsensitive then
        Result := AnsiCompareText(PChar(Data1), PChar(Data2))
      else
        Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
    ftSmallint:
      if SmallInt(Data1^) > SmallInt(Data2^) then
        Result := 1
      else if SmallInt(Data1^) < SmallInt(Data2^) then
        Result := -1;
    ftInteger, ftDate, ftTime, ftAutoInc:
      if Longint(Data1^) > Longint(Data2^) then
        Result := 1
      else if Longint(Data1^) < Longint(Data2^) then
        Result := -1;
    ftWord:
      if Word(Data1^) > Word(Data2^) then
        Result := 1
      else if Word(Data1^) < Word(Data2^) then
        Result := -1;
    ftBoolean:
      if WordBool(Data1^) and not WordBool(Data2^) then
        Result := 1
      else if not WordBool(Data1^) and WordBool(Data2^) then
        Result := -1;
    ftFloat, ftCurrency:
      if Double(Data1^) > Double(Data2^) then
        Result := 1
      else if Double(Data1^) < Double(Data2^) then
        Result := -1;
    ftDateTime:
      if TDateTime(Data1^) > TDateTime(Data2^) then
        Result := 1
      else if TDateTime(Data1^) < TDateTime(Data2^) then
        Result := -1;
    ftFixedChar:
      if CaseInsensitive then
        Result := AnsiCompareText(PChar(Data1), PChar(Data2))
      else
        Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
    ftWideString:
      if CaseInsensitive then
        Result := AnsiCompareText(WideCharToString(PWideChar(Data1)), WideCharToString(PWideChar(Data2)))
      else
        Result := AnsiCompareStr(WideCharToString(PWideChar(Data1)), WideCharToString(PWideChar(Data2)));
    ftLargeint:
      if Int64(Data1^) > Int64(Data2^) then
        Result := 1
      else if Int64(Data1^) < Int64(Data2^) then
        Result := -1;
    ftVariant:
      Result := 0;
    ftGuid:
      Result := AnsiCompareText(PChar(Data1), PChar(Data2));
  end;
end;

function CalcFieldLen(FieldType: TFieldType; Size: Word): Word;
begin
  if not (FieldType in ftSupported) then
    Result := 0
  else if (FieldType in ftBlobTypes) then
    Result := SizeOf(Longint)
  else
  begin
    Result := Size;
    case FieldType of
      ftString:
        Inc(Result);
      ftSmallint:
        Result := SizeOf(SmallInt);
      ftInteger:
        Result := SizeOf(Longint);
      ftWord:
        Result := SizeOf(Word);
      ftBoolean:
        Result := SizeOf(WordBool);
      ftFloat:
        Result := SizeOf(Double);
      ftCurrency:
        Result := SizeOf(Double);
      ftBCD:
        Result := 34;
      ftDate, ftTime:
        Result := SizeOf(Longint);
      ftDateTime:
        Result := SizeOf(TDateTime);
      ftBytes:
        Result := Size;
      ftVarBytes:
        Result := Size + 2;
      ftAutoInc:
        Result := SizeOf(Longint);
      ftADT:
        Result := 0;
      ftFixedChar:
        Inc(Result);
      ftWideString:
        Result := (Result + 1) * 2;
      ftLargeint:
        Result := SizeOf(Int64);
      ftVariant:
        Result := SizeOf(Variant);
      ftGuid:
        Result := GuidSize + 1;
    end;
  end;
end;

procedure CalcDataSize(FieldDef: TFieldDef; var DataSize: Integer);
var
  I: Integer;
begin
  with FieldDef do
  begin
    if (DataType in ftSupported - ftBlobTypes) then
      Inc(DataSize, CalcFieldLen(DataType, Size) + 1);
    for I := 0 to ChildDefs.Count - 1 do
      CalcDataSize(ChildDefs[I], DataSize);
  end;
end;

procedure Error(const Msg: string);
begin
  DatabaseError(Msg);
end;

procedure ErrorFmt(const Msg: string; const Args: array of const);
begin
  DatabaseErrorFmt(Msg, Args);
end;

type
  TCnBookmarkData = Integer;

  PCnMemBookmarkInfo = ^TCnMemBookmarkInfo;

  TCnMemBookmarkInfo = record
    BookmarkData: TCnBookmarkData;
    BookmarkFlag: TBookmarkFlag;
  end; 

{ TCnMemoryRecord }

constructor TCnMemoryRecord.Create(MemoryData: TCnDHibernateMemData);
begin
  CreateEx(MemoryData, True);
end;

constructor TCnMemoryRecord.CreateEx(MemoryData: TCnDHibernateMemData; UpdateParent: Boolean);
begin
  inherited Create;
  SetMemoryData(MemoryData, UpdateParent);
end;

destructor TCnMemoryRecord.Destroy;
begin
  SetMemoryData(nil, True);
  inherited Destroy;
end;

function TCnMemoryRecord.GetIndex: Integer;
begin
  if FMemoryData <> nil then
    Result := FMemoryData.FRecords.IndexOf(Self)
  else
    Result := -1;
end;

procedure TCnMemoryRecord.SetMemoryData(Value: TCnDHibernateMemData; UpdateParent: Boolean);
var
  I: Integer;
  DataSize: Integer;
begin
  if FMemoryData <> Value then
  begin
    if FMemoryData <> nil then
    begin
      FMemoryData.FRecords.Remove(Self);
      if FMemoryData.BlobFieldCount > 0 then
        Finalize(PCnMemBlobArray(FBlobs)[0], FMemoryData.BlobFieldCount);
      ReallocMem(FBlobs, 0);
      ReallocMem(FData, 0);
      FMemoryData := nil;
    end;
    if Value <> nil then
    begin
      if UpdateParent then
      begin
        Value.FRecords.Add(Self);
        Inc(Value.FLastID);
        FID := Value.FLastID;
      end;
      FMemoryData := Value;
      if Value.BlobFieldCount > 0 then
      begin
        ReallocMem(FBlobs, Value.BlobFieldCount * SizeOf(Pointer));
        Initialize(PCnMemBlobArray(FBlobs)[0], Value.BlobFieldCount);
      end;
      DataSize := 0;
      for I := 0 to Value.FieldDefs.Count - 1 do
        CalcDataSize(Value.FieldDefs[I], DataSize);
      ReallocMem(FData, DataSize);
    end;
  end;
end;

procedure TCnMemoryRecord.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FMemoryData.FRecords.Move(CurIndex, Value);
end; 

{ TCnDHibernateMemData }

constructor TCnDHibernateMemData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordPos := -1;
  FLastID := Low(Integer);
  FAutoInc := 1;
  FRecords := TList.Create;
end;

destructor TCnDHibernateMemData.Destroy;
begin
  inherited Destroy;
  FreeIndexList;
  ClearRecords;
  FRecords.Free;
  ReallocMem(FOffsets, 0);
end;

{ Records Management }

function TCnDHibernateMemData.GetCapacity: Integer;
begin
  if FRecords <> nil then
    Result := FRecords.Capacity
  else
    Result := 0;
end;

procedure TCnDHibernateMemData.SetCapacity(Value: Integer);
begin
  if FRecords <> nil then
    FRecords.Capacity := Value;
end;

function TCnDHibernateMemData.AddRecord: TCnMemoryRecord;
begin
  Result := TCnMemoryRecord.Create(Self);
end;

function TCnDHibernateMemData.FindRecordID(ID: Integer): TCnMemoryRecord;
var
  I: Integer;
begin
  for I := 0 to FRecords.Count - 1 do
  begin
    Result := TCnMemoryRecord(FRecords[I]);
    if Result.ID = ID then
      Exit;
  end;
  Result := nil;
end;

function TCnDHibernateMemData.InsertRecord(Index: Integer): TCnMemoryRecord;
begin
  Result := AddRecord;
  Result.Index := Index;
end;

function TCnDHibernateMemData.GeTCnMemoryRecord(Index: Integer): TCnMemoryRecord;
begin
  Result := TCnMemoryRecord(FRecords[Index]);
end; 

{ Field Management }

function TCnDHibernateMemData.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
  Move(BCD^, Curr, SizeOf(Currency));
  Result := True;
end;

function TCnDHibernateMemData.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision, Decimals: Integer): Boolean;
begin
  Move(Curr, BCD^, SizeOf(Currency));
  Result := True;
end;

procedure TCnDHibernateMemData.InitFieldDefsFromFields;
var
  I: Integer;
  Offset: Word;
begin
  if FieldDefs.Count = 0 then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      with Fields[I] do
        if (FieldKind in fkStoredFields) and not (DataType in ftSupported) then
          ErrorFmt(SUnknownFieldType, [DisplayName]);
    end;
    FreeIndexList;
  end;
  Offset := 0;
  inherited InitFieldDefsFromFields; 
  { Calculate fields offsets }
  ReallocMem(FOffsets, FieldDefList.Count * SizeOf(Word));
  for I := 0 to FieldDefList.Count - 1 do
  begin
    FOffsets^[I] := Offset;
    with FieldDefList[I] do
    begin
      if (DataType in ftSupported - ftBlobTypes) then
        Inc(Offset, CalcFieldLen(DataType, Size) + 1);
    end;
  end;
end;

function TCnDHibernateMemData.FindFieldData(Buffer: Pointer; Field: TField): Pointer;
var
  Index: Integer;
begin
  Index := FieldDefList.IndexOf(Field.FullName);
  if (Index >= 0) and (Buffer <> nil) and (FieldDefList[Index].DataType in ftSupported - ftBlobTypes) then
    Result := (PChar(Buffer) + FOffsets[Index])
  else
    Result := nil;
end; 

{ Buffer Manipulation }

function TCnDHibernateMemData.CalcRecordSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FieldDefs.Count - 1 do
    CalcDataSize(FieldDefs[I], Result);
end;

procedure TCnDHibernateMemData.InitBufferPointers(GetProps: Boolean);
begin
  if GetProps then
    FRecordSize := CalcRecordSize;
  FBookmarkOfs := FRecordSize + CalcFieldsSize;
  FBlobOfs := FBookmarkOfs + SizeOf(TCnMemBookmarkInfo);
  FRecBufSize := FBlobOfs + BlobFieldCount * SizeOf(Pointer);
end;

procedure TCnDHibernateMemData.ClearRecords;
begin
  while FRecords.Count > 0 do
    TObject(FRecords.Last).Free;
  FLastID := Low(Integer);
  FRecordPos := -1;
end;

function TCnDHibernateMemData.AllocRecordBuffer: PChar;
begin
  Result := PChar(StrAlloc(FRecBufSize));
  if BlobFieldCount > 0 then
    Initialize(PCnMemBlobArray(Result + FBlobOfs)[0], BlobFieldCount);
end;

procedure TCnDHibernateMemData.FreeRecordBuffer(var Buffer: PChar);
begin
  if BlobFieldCount > 0 then
    Finalize(PCnMemBlobArray(Buffer + FBlobOfs)[0], BlobFieldCount);
  StrDispose(PAnsiChar(Buffer));
  Buffer := nil;
end;

procedure TCnDHibernateMemData.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

procedure TCnDHibernateMemData.InternalInitRecord(Buffer: PChar);
var
  I: Integer;
begin
  FillChar(Buffer^, FBlobOfs, 0);
  for I := 0 to BlobFieldCount - 1 do
    PCnMemBlobArray(Buffer + FBlobOfs)[I] := EmptyStr;
end;

procedure TCnDHibernateMemData.InitRecord(Buffer: PChar);
begin
  inherited InitRecord(Buffer);
  with PCnMemBookmarkInfo(Buffer + FBookmarkOfs)^do
  begin
    BookmarkData := Low(Integer);
    BookmarkFlag := bfInserted;
  end;
end;

function TCnDHibernateMemData.GetCurrentRecord(Buffer: PChar): Boolean;
begin
  Result := False;
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      Move(Records[FRecordPos].Data^, Buffer^, FRecordSize);
      Result := True;
    end;
  end;
end;

procedure TCnDHibernateMemData.RecordToBuffer(Rec: TCnMemoryRecord; Buffer: PChar);
var
  I: Integer;
begin
  Move(Rec.Data^, Buffer^, FRecordSize);
  with PCnMemBookmarkInfo(Buffer + FBookmarkOfs)^do
  begin
    BookmarkData := Rec.ID;
    BookmarkFlag := bfCurrent;
  end;
  for I := 0 to BlobFieldCount - 1 do
    try
      PCnMemBlobArray(Buffer + FBlobOfs)[I] := PCnMemBlobArray(Rec.FBlobs)[I];
    except

    end;
  GetCalcFields(PChar(Buffer));
end;

function TCnDHibernateMemData.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
begin
  Result := grOk;
  Accept := True;
  case GetMode of
    gmPrior:
      if FRecordPos <= 0 then
      begin
        Result := grBOF;
        FRecordPos := -1;
      end
      else
      begin
        repeat
          Dec(FRecordPos);
          if Filtered then
            Accept := RecordFilter;
        until Accept or (FRecordPos < 0);
        if not Accept then
        begin
          Result := grBOF;
          FRecordPos := -1;
        end;
      end;
    gmCurrent:
      if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
        Result := grError
      else if Filtered then
      begin
        if not RecordFilter then
          Result := grError;
      end;
    gmNext:
      if FRecordPos >= RecordCount - 1 then
        Result := grEOF
      else
      begin
        repeat
          Inc(FRecordPos);
          if Filtered then
            Accept := RecordFilter;
        until Accept or (FRecordPos > RecordCount - 1);
        if not Accept then
        begin
          Result := grEOF;
          FRecordPos := RecordCount - 1;
        end;
      end;
  end;
  if Result = grOk then
    RecordToBuffer(Records[FRecordPos], PChar(Buffer))
  else if (Result = grError) and DoCheck then
    Error(DH_MEM_NO_RECORDS);
end;

function TCnDHibernateMemData.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TCnDHibernateMemData.GetActiveRecBuf(var RecBuf: PChar): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := PChar(ActiveBuffer);
    dsEdit, dsInsert:
      RecBuf := PChar(ActiveBuffer);
    dsCalcFields:
      RecBuf := PChar(CalcBuffer);
    dsFilter:
      RecBuf := PChar(TempBuffer);
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TCnDHibernateMemData.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf, Data: PChar;
  VarData: Variant;
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then
    Exit;
  if Field.FieldNo > 0 then
  begin
    Data := FindFieldData(RecBuf, Field);
    if Data <> nil then
    begin
      Result := Boolean(Data[0]);
      Inc(Data);
      if Field.DataType in [ftString, ftFixedChar, ftWideString, ftGuid] then
        Result := Result and (StrLen(Data) > 0);
      if Result and (Buffer <> nil) then
        if Field.DataType = ftVariant then
        begin
          VarData := PVariant(Data)^;
          PVariant(Buffer)^ := VarData;
        end
        else
          Move(Data^, Buffer^, CalcFieldLen(Field.DataType, Field.Size));
    end;
  end
  else
  begin
    if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Inc(RecBuf, FRecordSize + Field.Offset);
      Result := Boolean(RecBuf[0]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
    end;
  end;
end;

procedure TCnDHibernateMemData.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf, Data: PChar;
  VarData: Variant;
begin
  if not (State in dsWriteModes) then
    Error(SNotEditing);
  GetActiveRecBuf(RecBuf);
  with Field do
  begin
    if FieldNo > 0 then
    begin
      if State in [dsCalcFields, dsFilter] then
        Error(SNotEditing);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        ErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then
      begin
        Data := FindFieldData(RecBuf, Field);
        if Data <> nil then
        begin
          if DataType = ftVariant then
          begin
            if Buffer <> nil then
              VarData := PVariant(Buffer)^
            else
              VarData := EmptyParam;
            Data[0] := Char(LongBool(Buffer) and not (VarIsNull(VarData) or VarIsEmpty(VarData)));
            if Boolean(Data[0]) then
            begin
              Inc(Data);
              PVariant(Data)^ := VarData;
            end
            else
              FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end
          else
          begin
            Data[0] := Char(LongBool(Buffer));
            Inc(Data);
            if LongBool(Buffer) then
              Move(Buffer^, Data^, CalcFieldLen(DataType, Size))
            else
              FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end;
        end;
      end;
    end
    else                                {fkCalculated, fkLookup}
    begin
      Inc(RecBuf, FRecordSize + Offset);
      RecBuf[0] := Char(LongBool(Buffer));
      if Boolean(RecBuf[0]) then
        Move(Buffer^, RecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
  end;
end; 

{ Filter }

procedure TCnDHibernateMemData.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      inherited SetFiltered(Value);
      First;
    end;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TCnDHibernateMemData.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then
  begin
    CheckBrowseMode;
    inherited SetOnFilterRecord(Value);
    if Filtered then
      First;
  end
  else
    inherited SetOnFilterRecord(Value);
end;

function TCnDHibernateMemData.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
  begin
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      SaveState := SetTempState(dsFilter);
      try
        RecordToBuffer(Records[FRecordPos], PChar(TempBuffer));
        OnFilterRecord(Self, Result);
      except
        Application.HandleException(Self);
      end;
      RestoreState(SaveState);
    end
    else
      Result := False;
  end;
end; 

{ Blobs }

function TCnDHibernateMemData.GetBlobData(Field: TField; Buffer: PChar): TCnMemBlobData;
begin
  Result := PCnMemBlobArray(Buffer + FBlobOfs)[Field.Offset];
end;

procedure TCnDHibernateMemData.SetBlobData(Field: TField; Buffer: PChar; Value: TCnMemBlobData);
begin
  if (Buffer = PChar(ActiveBuffer)) then
  begin
    if State = dsFilter then
      Error(SNotEditing);
    PCnMemBlobArray(Buffer + FBlobOfs)[Field.Offset] := Value;
  end;
end;

procedure TCnDHibernateMemData.CloseBlob(Field: TField);
begin
  if (FRecordPos >= 0) and (FRecordPos < FRecords.Count) and (State = dsEdit) then
    PCnMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] := PCnMemBlobArray(Records[FRecordPos].FBlobs)[Field.Offset]
  else
    PCnMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] := EmptyStr;
end;

function TCnDHibernateMemData.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TCnMemBlobStream.Create(Field as TBlobField, Mode);
end; 

{ Bookmarks }

function TCnDHibernateMemData.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := FActive and (TCnBookmarkData(Bookmark^) > Low(Integer)) and (TCnBookmarkData(Bookmark^) <= FLastID);
end;

function TCnDHibernateMemData.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  if (Bookmark1 = nil) and (Bookmark2 = nil) then
    Result := 0
  else if (Bookmark1 <> nil) and (Bookmark2 = nil) then
    Result := 1
  else if (Bookmark1 = nil) and (Bookmark2 <> nil) then
    Result := -1
  else if TCnBookmarkData(Bookmark1^) > TCnBookmarkData(Bookmark2^) then
    Result := 1
  else if TCnBookmarkData(Bookmark1^) < TCnBookmarkData(Bookmark2^) then
    Result := -1
  else
    Result := 0;
end;

procedure TCnDHibernateMemData.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(PCnMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, Data^, SizeOf(TCnBookmarkData));
end;

procedure TCnDHibernateMemData.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Data^, PCnMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, SizeOf(TCnBookmarkData));
end;

function TCnDHibernateMemData.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PCnMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag;
end;

procedure TCnDHibernateMemData.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PCnMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag := Value;
end;

procedure TCnDHibernateMemData.InternalGotoBookmark(Bookmark: TBookmark);
var
  Rec: TCnMemoryRecord;
  SavePos: Integer;
  Accept: Boolean;
begin
  Rec := FindRecordID(TCnBookmarkData(Bookmark^));
  if Rec <> nil then
  begin
    Accept := True;
    SavePos := FRecordPos;
    try
      FRecordPos := Rec.Index;
      if Filtered then
        Accept := RecordFilter;
    finally
      if not Accept then
        FRecordPos := SavePos;
    end;
  end;
end; 

{ Navigation }

procedure TCnDHibernateMemData.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PCnMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData);
end;

procedure TCnDHibernateMemData.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TCnDHibernateMemData.InternalLast;
begin
  FRecordPos := FRecords.Count;
end; 

{ Data Manipulation }

procedure TCnDHibernateMemData.AssignMemoryRecord(Rec: TCnMemoryRecord; Buffer: PChar);
var
  I: Integer;
begin
  Move(Buffer^, Rec.Data^, FRecordSize);
  for I := 0 to BlobFieldCount - 1 do
    PCnMemBlobArray(Rec.FBlobs)[I] := PCnMemBlobArray(Buffer + FBlobOfs)[I];
end;

procedure TCnDHibernateMemData.SeTCnMemoryRecordData(Buffer: PChar; Pos: Integer);
var
  Rec: TCnMemoryRecord;
begin
  if State = dsFilter then
    Error(SNotEditing);
  Rec := Records[Pos];
  AssignMemoryRecord(Rec, Buffer);
end;

procedure TCnDHibernateMemData.SetAutoIncFields(Buffer: PChar);
var
  I, Count: Integer;
  Data: PChar;
begin
  Count := 0;
  for I := 0 to FieldCount - 1 do
    if (Fields[I].FieldKind in fkStoredFields) and (Fields[I].DataType = ftAutoInc) then
    begin
      Data := FindFieldData(Buffer, Fields[I]);
      if Data <> nil then
      begin
        Data[0] := Char(True);
        Inc(Data);
        Move(FAutoInc, Data^, SizeOf(Longint));
        Inc(Count);
      end;
    end;
  if Count > 0 then
    Inc(FAutoInc);
end;

procedure TCnDHibernateMemData.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos: Integer;
  Rec: TCnMemoryRecord;
begin
  if Append then
  begin
    Rec := AddRecord;
    FRecordPos := FRecords.Count - 1;
  end
  else
  begin
    if FRecordPos = -1 then
      RecPos := 0
    else
      RecPos := FRecordPos;
    Rec := InsertRecord(RecPos);
    FRecordPos := RecPos;
  end;
  SetAutoIncFields(Buffer);
  SeTCnMemoryRecordData(Buffer, Rec.Index);
end;

procedure TCnDHibernateMemData.InternalDelete;
var
  Accept: Boolean;
begin
  Records[FRecordPos].Free;
  if FRecordPos >= FRecords.Count then
    Dec(FRecordPos);
  Accept := True;
  repeat
    if Filtered then
      Accept := RecordFilter;
    if not Accept then
      Dec(FRecordPos);
  until Accept or (FRecordPos < 0);
  if FRecords.Count = 0 then
    FLastID := Low(Integer);
end;

procedure TCnDHibernateMemData.InternalPost;
var
  RecPos: Integer;
begin
  if State = dsEdit then
    SeTCnMemoryRecordData(PChar(ActiveBuffer), FRecordPos)
  else
  begin
    if State in [dsInsert] then
      SetAutoIncFields(PChar(ActiveBuffer));
    if FRecordPos >= FRecords.Count then
    begin
      SeTCnMemoryRecordData(PChar(ActiveBuffer), AddRecord.Index);
      FRecordPos := FRecords.Count - 1;
    end
    else
    begin
      if FRecordPos = -1 then
        RecPos := 0
      else
        RecPos := FRecordPos;
      SeTCnMemoryRecordData(PChar(ActiveBuffer), InsertRecord(RecPos).Index);
      FRecordPos := RecPos;
    end;
  end;
end;

procedure TCnDHibernateMemData.OpenCursor(InfoQuery: Boolean);
begin
  if not InfoQuery then
  begin
    if FieldCount > 0 then
      FieldDefs.Clear;
    InitFieldDefsFromFields;
  end;
  FActive := True;
  inherited OpenCursor(InfoQuery);
end;

procedure TCnDHibernateMemData.InternalOpen;
begin
  BookmarkSize := SizeOf(TCnBookmarkData);
  if DefaultFields then
    CreateFields;
  BindFields(True);
  InitBufferPointers(True);
  InternalFirst;
end;

procedure TCnDHibernateMemData.InternalClose;
begin
  ClearRecords;
  FAutoInc := 1;
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  FreeIndexList;
  FActive := False;
end;

procedure TCnDHibernateMemData.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TCnDHibernateMemData.InternalInitFieldDefs;
begin

end;

function TCnDHibernateMemData.IsCursorOpen: Boolean;
begin
  Result := FActive;
end; 

{ Informational }

function TCnDHibernateMemData.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

function TCnDHibernateMemData.GetRecNo: Integer;
begin
  CheckActive;
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FRecordPos + 1;
end;

procedure TCnDHibernateMemData.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value <= FRecords.Count) then
  begin
    FRecordPos := Value - 1;
    Resync([]);
  end;
end;

function TCnDHibernateMemData.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

function TCnDHibernateMemData.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end; 

{ Table Manipulation }

procedure TCnDHibernateMemData.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    ClearRecords;
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end;
end;

procedure TCnDHibernateMemData.CopyStructure(Source: TDataSet);

  procedure CheckDataTypes(FieldDefs: TFieldDefs);
  var
    I: Integer;
  begin
    for I := FieldDefs.Count - 1 downto 0 do
    begin
      if not (FieldDefs.Items[I].DataType in ftSupported) then
        FieldDefs.Items[I].Free
      else
        CheckDataTypes(FieldDefs[I].ChildDefs);
    end;
  end;

var
  I: Integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do
    Fields[I].Free;
  if (Source = nil) then
    Exit;
  Source.FieldDefs.Update;
  FieldDefs := Source.FieldDefs;
  CheckDataTypes(FieldDefs);
  CreateFields;
end;

function TCnDHibernateMemData.LoadFromDataSet(Source: TDataSet; RecordCount: Integer; Mode: TCnLoadMode): Integer;
var
  SourceActive: Boolean;
  MovedCount: Integer;
begin
  Result := 0;
  if Source = Self then
    Exit;
  SourceActive := Source.Active;
  Source.DisableControls;
  try
    DisableControls;
    try
      Filtered := False;
      with Source do
      begin
        Open;
        CheckBrowseMode;
        UpdateCursorPos;
      end;
      if Mode = lmCopy then
      begin
        Close;
        CopyStructure(Source);
      end;
      FreeIndexList;
      if not Active then
        Open;
      CheckBrowseMode;
      if RecordCount > 0 then
        MovedCount := RecordCount
      else
      begin
        Source.First;
        MovedCount := MaxInt;
      end;
      try
        while not Source.EOF do
        begin
          Append;
          AssignRecord(Source, Self, True);
          Post;
          Inc(Result);
          if Result >= MovedCount then
            Break;
          Source.Next;
        end;
      finally
        First;
      end;
    finally
      EnableControls;
    end;
  finally
    if not SourceActive then
      Source.Close;
    Source.EnableControls;
  end;
end;

function TCnDHibernateMemData.SaveToDataSet(Dest: TDataSet; RecordCount: Integer): Integer;
var
  MovedCount: Integer;
begin
  Result := 0;
  if Dest = Self then
    Exit;
  CheckBrowseMode;
  UpdateCursorPos;
  Dest.DisableControls;
  try
    DisableControls;
    try
      if not Dest.Active then
        Dest.Open
      else
        Dest.CheckBrowseMode;
      if RecordCount > 0 then
        MovedCount := RecordCount
      else
      begin
        First;
        MovedCount := MaxInt;
      end;
      try
        while not EOF do
        begin
          Dest.Append;
          AssignRecord(Self, Dest, True);
          Dest.Post;
          Inc(Result);
          if Result >= MovedCount then
            Break;
          Next;
        end;
      finally
        Dest.First;
      end;
    finally
      EnableControls;
    end;
  finally
    Dest.EnableControls;
  end;
end; 

{ Index Related }

procedure TCnDHibernateMemData.SortOnFields(const FieldNames: string; CaseInsensitive: Boolean = True; Descending: Boolean = False);
begin
  CreateIndexList(FieldNames);
  FCaseInsensitiveSort := CaseInsensitive;
  FDescendingSort := Descending;
  try
    Sort;
  except
    FreeIndexList;
    raise;
  end;
end;

procedure TCnDHibernateMemData.Sort;
var
  Pos: TBookmarkStr;
begin
  if Active and (FRecords <> nil) and (FRecords.Count > 0) then
  begin
    Pos := Bookmark;
    try
      QuickSort(0, FRecords.Count - 1, CompareRecords);
      SetBufListSize(0);
      InitBufferPointers(False);
      try
        SetBufListSize(BufferCount + 1);
      except
        SetState(dsInactive);
        CloseCursor;
        raise;
      end;
    finally
      Bookmark := Pos;
    end;
    Resync([]);
  end;
end;

procedure TCnDHibernateMemData.QuickSort(L, R: Integer; Compare: TCompareRecords);
var
  I, J: Integer;
  P: TCnMemoryRecord;
begin
  repeat
    I := L;
    J := R;
    P := Records[(L + R) shr 1];
    repeat
      while Compare(Records[I], P) < 0 do
        Inc(I);
      while Compare(Records[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        FRecords.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

function TCnDHibernateMemData.CompareRecords(Item1, Item2: TCnMemoryRecord): Integer;
var
  Data1, Data2: PChar;
  F: TField;
  I: Integer;
begin
  Result := 0;
  if FIndexList <> nil then
  begin
    for I := 0 to FIndexList.Count - 1 do
    begin
      F := TField(FIndexList[I]);
      Data1 := FindFieldData(Item1.Data, F);
      if Data1 <> nil then
      begin
        Data2 := FindFieldData(Item2.Data, F);
        if Data2 <> nil then
        begin
          if Boolean(Data1[0]) and Boolean(Data2[0]) then
          begin
            Inc(Data1);
            Inc(Data2);
            Result := CompareFields(Data1, Data2, F.DataType, FCaseInsensitiveSort);
          end
          else if Boolean(Data1[0]) then
            Result := 1
          else if Boolean(Data2[0]) then
            Result := -1;
          if FDescendingSort then
            Result := -Result;
        end;
      end;
      if Result <> 0 then
        Exit;
    end;
  end;
  if (Result = 0) then
  begin
    if Item1.ID > Item2.ID then
      Result := 1
    else if Item1.ID < Item2.ID then
      Result := -1;
    if FDescendingSort then
      Result := -Result;
  end;
end;

function TCnDHibernateMemData.GetIsIndexField(Field: TField): Boolean;
begin
  if FIndexList <> nil then
    Result := FIndexList.IndexOf(Field) >= 0
  else
    Result := False;
end;

procedure TCnDHibernateMemData.CreateIndexList(const FieldNames: string);
var
  Pos: Integer;
  F: TField;
begin
  if FIndexList = nil then
    FIndexList := TList.Create
  else
    FIndexList.Clear;
  Pos := 1;
  while Pos <= Length(FieldNames) do
  begin
    F := FieldByName(ExtractFieldName(FieldNames, Pos));
    if (F.FieldKind = fkData) and (F.DataType in ftSupported - ftBlobTypes) then
      FIndexList.Add(F)
    else
      ErrorFmt(SFieldTypeMismatch, [F.DisplayName]);
  end;
end;

procedure TCnDHibernateMemData.FreeIndexList;
begin
  FIndexList.Free;
  FIndexList := nil;
end; 

{ TCnMemBlobStream }

constructor TCnMemBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TCnDHibernateMemData;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
  if not FField.Modified and (Mode <> bmRead) then
  begin
    if FField.ReadOnly then
      ErrorFmt(SFieldReadOnly, [FField.DisplayName]);
    if not (FDataSet.State in [dsEdit, dsInsert]) then
      Error(SNotEditing);
    FCached := True;
  end
  else
    FCached := (FBuffer = PChar(FDataSet.ActiveBuffer));
  FOpened := True;
  if Mode = bmWrite then
    Truncate;
end;

destructor TCnMemBlobStream.Destroy;
begin
  if FOpened and FModified then
    FField.Modified := True;
  if FModified then
    try
      FDataSet.DataEvent(deFieldChange, Longint(FField));
    except
      Application.HandleException(Self);
    end;
end;

function TCnMemBlobStream.GetBlobFromRecord(Field: TField): TCnMemBlobData;
var
  Rec: TCnMemoryRecord;
  Pos: Integer;
begin
  Result := '';
  Pos := FDataSet.FRecordPos;
  if (Pos < 0) and (FDataSet.RecordCount > 0) then
    Pos := 0
  else if Pos >= FDataSet.RecordCount then
    Pos := FDataSet.RecordCount - 1;
  if (Pos >= 0) and (Pos < FDataSet.RecordCount) then
  begin
    Rec := FDataSet.Records[Pos];
    if Rec <> nil then
      Result := PCnMemBlobArray(Rec.FBlobs)[FField.Offset];
  end;
end;

function TCnMemBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if FOpened then
  begin
    if Count > Size - FPosition then
      Result := Size - FPosition
    else
      Result := Count;
    if Result > 0 then
    begin
      if FCached then
      begin
        Move(PChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer, Result);
        Inc(FPosition, Result);
      end
      else
      begin
        Move(PChar(GetBlobFromRecord(FField))[FPosition], Buffer, Result);
        Inc(FPosition, Result);
      end;
    end;
  end;
end;

function TCnMemBlobStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp: TCnMemBlobData;
begin
  Result := 0;
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    Temp := FDataSet.GetBlobData(FField, FBuffer);
    if Length(Temp) < FPosition + Count then
      SetLength(Temp, FPosition + Count);
    Move(Buffer, PChar(Temp)[FPosition], Count);
    FDataSet.SetBlobData(FField, FBuffer, Temp);
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
  end;
end;

function TCnMemBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0:
      FPosition := Offset;
    1:
      Inc(FPosition, Offset);
    2:
      FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TCnMemBlobStream.Truncate;
begin
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    FDataSet.SetBlobData(FField, FBuffer, '');
    FModified := True;
  end;
end;

function TCnMemBlobStream.GetBlobSize: Longint;
begin
  Result := 0;
  if FOpened then
    if FCached then
      Result := Length(FDataSet.GetBlobData(FField, FBuffer))
    else
      Result := Length(GetBlobFromRecord(FField));
end; 

{$ENDIF DELPHI2009_UP}

{$ENDIF SUPPORT_ADO}
end.

