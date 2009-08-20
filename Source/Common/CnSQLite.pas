{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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
{            以下为原单元作者的版权声明                                        }
{******************************************************************************}
{ july 22. 2003                                                                }
{ taken from the sqlite.pas unit from next listed autors                       }
{ adjusted by R.M. Tegel (rene@dubaron.com)                                    }
{ to match my ideas about easy querying                                        }
{ partly based on ideas i had when developing TMyDB.pas library for MySQL      }
{ (http://kylix.dubaron.com)                                                   }
{ future version might combine sqlite/mysql functionality                      }
{                                                                              }
{ also adjusted to handle multi-threaded access to same DB                     }
{                                                                              }
{ simple class interface for SQLite.                                           }
{ Hacked in by Ben Hochstrasser (bhoc@surfeu.ch)                               }
{ Thanks to Roger Reghin (RReghin@scelectric.ca) for his idea to ValueList.    }
{******************************************************************************}
{                                                                              }
{            下载地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnSQLite;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SQLite数据库的Delphi封装单元
* 单元作者：见以上原单元作者的版权声明
* 备    注：CnPack将会以SQLite数据库为主要的内部数据库内核，
*           但要注意该单元需要Sqlite.dll的支持。
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串暂时不符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2003.11.17 V1.1 by 何清(QSoft)
*               取消TLiteDB中创建数据库的默认扩展名，原为.lit，现在的扩展名可由
*               调用者自己设定。
*           2003.11.17 V1.0 by 何清(QSoft)
*               上传单元。
================================================================================
|</PRE>}

{* 该宏用户决定该单元是否支持GUI，如果定义该宏，TLiteDB类将从 TComponent 继承，
   并增加 QueryComboBox 和 QueryStringGrid 两个GUI相关方法的支持。}
//{$DEFINE WITH_GUI}

interface

{$I CnPack.inc}

uses Windows, Classes, SysUtils, SyncObjs
{$IFDEF WITH_GUI}, Forms, StdCtrls, Grids{$ENDIF};

type
  TSQLiteExecCallback = function(Sender: TObject; Columns: Integer; ColumnValues: Pointer; ColumnNames: Pointer): integer of object; cdecl;
  TSQLiteBusyCallback = function(Sender: TObject; ObjectName: PChar; BusyCount: integer): integer of object; cdecl;
  TOnData = procedure(Sender: TObject; Columns: Integer; ColumnNames, ColumnValues: string) of object;
  TOnBusy = procedure(Sender: TObject; ObjectName: string; BusyCount: integer; var Cancel: Boolean) of object;
  TOnQueryComplete = procedure(Sender: TObject) of object;
  
  TSQLite = class(TObject)
  private
    fSQLite: Pointer;
    fMsg: string;
    fIsOpen: Boolean;
    fBusy: Boolean;
    fError: Integer;
    fVersion: string;
    fEncoding: string;
    fTable: TStrings;
    fLstName: TStringList;
    fLstVal: TStringList;
    fOnData: TOnData;
    fOnBusy: TOnBusy;
    fOnQueryComplete: TOnQueryComplete;
    fBusyTimeout: integer;
    fPMsg: PChar;
    fChangeCount: integer;
    procedure SetBusyTimeout(Timeout: integer);
  public
    constructor Create(DBFileName: string);
    destructor Destroy; override;
    function Query(Sql: string; Table: TStrings = nil): boolean;
    function ErrorMessage(ErrNo: Integer): string;
    function IsComplete(Sql: string): boolean;
    function LastInsertRow: integer;
    function Cancel: boolean;
    function DatabaseDetails(Table: TStrings): boolean;
    property LastErrorMessage: string read fMsg;
    property LastError: Integer read fError;
    property Version: string read fVersion;
    property Encoding: string read fEncoding;
    property OnData: TOnData read fOnData write fOnData;
    property OnBusy: TOnBusy read fOnBusy write fOnBusy;
    property OnQueryComplete: TOnQueryComplete read fOnQueryComplete write fOnQueryComplete;
    property BusyTimeout: Integer read fBusyTimeout write SetBusyTimeout;
    property ChangeCount: Integer read fChangeCount;
  end;
  
function Pas2SQLStr(const PasString: string): string;
function SQL2PasStr(const SQLString: string): string;
function QuoteStr(const s: string; QuoteChar: Char = #39): string;
function UnQuoteStr(const s: string; QuoteChar: Char = #39): string;
procedure ValueList(const ColumnNames, ColumnValues: string; NameValuePairs: TStrings);

type
  TResultCell = class(TObject)
  private
    FValue: string;
    FIsNull: Boolean;
    function GetInteger: Integer;
    function GetFloat: Extended;
    function GetBoolean: Boolean;
  public
    property IsNull: Boolean read FIsNull;
    property AsString: string read FValue;
    property AsInteger: Integer read GetInteger;
    property AsBoolean: Boolean read GetBoolean;
    property AsFloat: Extended read GetFloat;
//    property AsVariant:Variant read FValue;
  end;

  TResultRow = class(TStringList)
  private
    FNulls: TList;
    FResultCell: TResultCell;
    FFields: TStrings;
    FNameValue: TStrings;
    function GetString(i: Integer): string;
    function GetResultCell(i: Integer): TResultCell;
    function GetIsNull(i: Integer): Boolean;
    function GetByField(Value: string): TResultCell;
    function GetAsNameValue: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    //override Strings property to customize behavior
    property Columns[Index: Integer]: string read GetString; default;
    property Format[Index: Integer]: TResultCell read GetResultCell;
    property IsNull[i: Integer]: Boolean read GetIsNull;
    property ByField[Value: string]: TResultCell read GetByField;
    property Fields: TStrings read FFields;
    property AsNameValue: TStrings read GetAsNameValue;
  end;

  TBaseInfo = class(TObject)
    CS: TCriticalSection;
    ReferenceCount: Integer;
    FHandle: Pointer;
    constructor Create;
    destructor Destroy; override;
  end;

  TOnFetchRow = procedure(Sender: TObject; Row: TResultRow) of object;
  
  TLiteDB = class({$IFDEF WITH_GUI}TComponent{$ELSE}TObject{$ENDIF})
  private
    FThreaded: Boolean;
    FRowCount: Integer;
    FColCount: Integer;
    FLastInsertID: Integer;
    FRowsAffected: Integer;
    FRowList: TList;
    FFields: TStringList;
    FDataBase: string;
    FNilRow: TResultRow;
    FSQL: string;
    FBaseInfo: TBaseInfo;
    FMaxQuerySize: Integer;
    FQuerySize: Integer;
    FOnFetchRow: TOnFetchRow;
    FOnQueryComplete: TNotifyEvent;
    FCallbackOnly: Boolean;
    FLastError: Integer;
  protected
  public
    FVersion: string;
    FEncoding: string;
    function Query(SQL: string): Boolean;
    function QueryOne(SQL: string): string;
    procedure DoQuery(SQL: string);
    //comment this out if not supported by compiler:
    function FormatQuery(Value: string; const Args: array of const): Boolean;
    function GetField(I: Integer): string;
    procedure Use(Database: string);
    function GetOneResult: string;
    function GetResult(I: Integer): TResultRow;
    procedure Lock;
    procedure Unlock;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetErrorMessage: string;
    function QueryStrings(SQL: string; Strings: TStrings): Boolean;
{$IFDEF WITH_GUI}
    function QueryComboBox(SQL: string; Combobox: TComboBox; AddEmpty: Boolean = False): Boolean;
    function QueryStringGrid(SQL: string; Grid: TStringGrid): Boolean;
{$ENDIF}
    constructor Create({$IFDEF WITH_GUI}AOwner: TComponent; {$ENDIF}DataBase: string = '');
    destructor Destroy; override;
    property Results[Index: Integer]: TResultRow read GetResult; default;
    property Fields[Index: Integer]: string read GetField;
    property LastError: Integer read FLastError;
    property ErrorMessage: string read GetErrorMessage;

    property DataBase: string read FDataBase write Use;
    property SQL: string read FSQL write DoQuery;
    property Result: string read GetOneResult;
    property Threaded: Boolean read FThreaded write FThreaded; //Use to fill remaining time with processmessages
    property RowsAffected: Integer read FRowsAffected;
    property RowCount: Integer read FRowCount;
    property LastInsertID: Integer read FLastInsertID;
    property MaxQuerySize: Integer read FMaxQuerySize write FMaxQuerySize;
    property CallBackOnly: Boolean read FCallBackOnly write FCallBackOnly;
    property OnFetchRow: TOnFetchRow read FOnFetchRow write FOnFetchRow;
    property OnQueryComplete: TNotifyEvent read FOnQueryComplete write FOnQueryComplete;
    {
    property ResultAsHTMLTable
    etc etc
    }
  end;


implementation

var
  DataBases: TStringList; //filled with DB names and appropiate critical sections
  CSConnect: TCriticalSection;

const
  SQLITE_OK = 0; // Successful result - 成功
  SQLITE_ERROR = 1; // SQL error or missing database - SQL语句错误或缺少数据库
  SQLITE_INTERNAL = 2; // An internal logic error in SQLite - SQLite内部逻辑错误
  SQLITE_PERM = 3; // Access permission denied - 访问许可被拒绝
  SQLITE_ABORT = 4; // Callback routine requested an abort - 发生一个请求回滚事务的异常
  SQLITE_BUSY = 5; // The database file is locked - 数据库文件被锁定
  SQLITE_LOCKED = 6; // A table in the database is locked - 数据库中的一个表被锁定
  SQLITE_NOMEM = 7; // A malloc() failed - malloc()分配内存失败
  SQLITE_READONLY = 8; // Attempt to write a readonly database - 试图对一个只读的数据库进行写操作
  SQLITE_INTERRUPT = 9; // Operation terminated by sqlite_interrupt() - sqlite_interrupt()操作中止
  SQLITE_IOERR = 10; // Some kind of disk I/O error occurred - 发生一些磁盘 I/O 错误
  SQLITE_CORRUPT = 11; // The database disk image is malformed - 不可识别的数据库磁盘映像
  SQLITE_NOTFOUND = 12; // (Internal Only) Table or record not found - （仅在内部的）未找到表或记录
  SQLITE_FULL = 13; // Insertion failed because database is full - 插入记录失败，因为数据库已满
  SQLITE_CANTOPEN = 14; // Unable to open the database file - 不能打开数据库文件
  SQLITE_PROTOCOL = 15; // Database lock protocol error - 数据库锁协议错误
  SQLITE_EMPTY = 16; // (Internal Only) Database table is empty - （仅在内部的）数据库的表是空的
  SQLITE_SCHEMA = 17; // The database schema changed - 数据库计划改变
  SQLITE_TOOBIG = 18; // Too much data for one row of a table - 表的行数据太多
  SQLITE_CONSTRAINT = 19; // Abort due to constraint violation - 由于违反约束条件导致异常中断
  SQLITE_MISMATCH = 20; // Data type mismatch - 数据类型不匹配
  SQLITEDLL: PChar = 'sqlite.dll';
  SngQuote: Char = #39;
  Crlf: string = #13#10;

var
  SQLite_Open: function(dbname: PChar; mode: Integer; var ErrMsg: PChar): Pointer; cdecl;
  SQLite_Close: procedure(db: Pointer); cdecl;
  SQLite_Exec: function(db: Pointer; SQLStatement: PChar; CallbackPtr: Pointer; Sender: TObject; var ErrMsg: PChar): integer; cdecl;
  SQLite_Version: function(): PChar; cdecl;
  SQLite_Encoding: function(): PChar; cdecl;
  SQLite_ErrorString: function(ErrNo: Integer): PChar; cdecl;
  SQLite_GetTable: function(db: Pointer; SQLStatement: PChar; var ResultPtr: Pointer; var RowCount: Cardinal; var ColCount: Cardinal; var ErrMsg: PChar): integer; cdecl;
  SQLite_FreeTable: procedure(Table: PChar); cdecl;
  SQLite_FreeMem: procedure(P: PChar); cdecl;
  SQLite_Complete: function(P: PChar): boolean; cdecl;
  SQLite_LastInsertRow: function(db: Pointer): integer; cdecl;
  SQLite_Cancel: procedure(db: Pointer); cdecl;
  SQLite_BusyHandler: procedure(db: Pointer; CallbackPtr: Pointer; Sender: TObject); cdecl;
  SQLite_BusyTimeout: procedure(db: Pointer; TimeOut: integer); cdecl;
  SQLite_Changes: function(db: Pointer): integer; cdecl;
  LibsLoaded: Boolean;
  DLLHandle: THandle;
  MsgNoError: string;

function QuoteStr(const s: string; QuoteChar: Char = #39): string;
begin
  Result := Concat(QuoteChar, s, QuoteChar);
end;

function UnQuoteStr(const s: string; QuoteChar: Char = #39): string;
begin
  Result := s;
  if length(Result) > 1 then
  begin
    if Result[1] = QuoteChar then
      Delete(Result, 1, 1);
    if Result[Length(Result)] = QuoteChar then
      Delete(Result, Length(Result), 1);
  end;
end;

function Pas2SQLStr(const PasString: string): string;
var
  n: integer;
begin
  Result := SQL2PasStr(PasString);
  n := Length(Result);
  while n > 0 do
  begin
    if Result[n] = SngQuote then
      Insert(SngQuote, Result, n);
    dec(n);
  end;
  Result := QuoteStr(Result);
end;

function SQL2PasStr(const SQLString: string): string;
const
  DblSngQuote: string = #39#39;
var
  p: integer;
begin
  Result := SQLString;
  p := pos(DblSngQuote, Result);
  while p > 0 do
  begin
    Delete(Result, p, 1);
    p := pos(DblSngQuote, Result);
  end;
  Result := UnQuoteStr(Result);
  Result := UnQuoteStr(Result, '"');
end;

procedure ValueList(const ColumnNames, ColumnValues: string; NameValuePairs: TStrings);
var
  n: integer;
  lstName, lstValue: TStringList;
begin
  if NameValuePairs <> nil then
  begin
    lstName := TStringList.Create;
    lstValue := TStringList.Create;
    lstName.CommaText := ColumnNames;
    lstValue.CommaText := ColumnValues;
    NameValuePairs.Clear;
    if lstName.Count = LstValue.Count then
      if lstName.Count > 0 then
        for n := 0 to lstName.Count - 1 do
          NameValuePairs.Append(Concat(lstName.Strings[n], '=', lstValue.Strings[n]));
    lstValue.Free;
    lstName.Free;
  end;
end;

function LoadLibs: Boolean;
begin
  Result := False;
  DLLHandle := LoadLibrary(SQLITEDLL);
  if DLLHandle <> 0 then
  begin
    @SQLite_Open := GetProcAddress(DLLHandle, 'sqlite_open');
    if not Assigned(@SQLite_Open) then exit;
    @SQLite_Close := GetProcAddress(DLLHandle, 'sqlite_close');
    if not Assigned(@SQLite_Close) then exit;
    @SQLite_Exec := GetProcAddress(DLLHandle, 'sqlite_exec');
    if not Assigned(@SQLite_Exec) then exit;
    @SQLite_Version := GetProcAddress(DLLHandle, 'sqlite_libversion');
    if not Assigned(@SQLite_Version) then exit;
    @SQLite_Encoding := GetProcAddress(DLLHandle, 'sqlite_libencoding');
    if not Assigned(@SQLite_Encoding) then exit;
    @SQLite_ErrorString := GetProcAddress(DLLHandle, 'sqlite_error_string');
    if not Assigned(@SQLite_ErrorString) then exit;
    @SQLite_GetTable := GetProcAddress(DLLHandle, 'sqlite_get_table');
    if not Assigned(@SQLite_GetTable) then exit;
    @SQLite_FreeTable := GetProcAddress(DLLHandle, 'sqlite_free_table');
    if not Assigned(@SQLite_FreeTable) then exit;
    @SQLite_FreeMem := GetProcAddress(DLLHandle, 'sqlite_freemem');
    if not Assigned(@SQLite_FreeMem) then exit;
    @SQLite_Complete := GetProcAddress(DLLHandle, 'sqlite_complete');
    if not Assigned(@SQLite_Complete) then exit;
    @SQLite_LastInsertRow := GetProcAddress(DLLHandle, 'sqlite_last_insert_rowid');
    if not Assigned(@SQLite_LastInsertRow) then exit;
    @SQLite_Cancel := GetProcAddress(DLLHandle, 'sqlite_interrupt');
    if not Assigned(@SQLite_Cancel) then exit;
    @SQLite_BusyTimeout := GetProcAddress(DLLHandle, 'sqlite_busy_timeout');
    if not Assigned(@SQLite_BusyTimeout) then exit;
    @SQLite_BusyHandler := GetProcAddress(DLLHandle, 'sqlite_busy_handler');
    if not Assigned(@SQLite_BusyHandler) then exit;
    @SQLite_Changes := GetProcAddress(DLLHandle, 'sqlite_changes');
    if not Assigned(@SQLite_Changes) then exit;
    Result := True;
  end;
end;

function SystemErrorMsg(ErrNo: Integer = -1): string;
var
  size: Integer;
  MsgLen: Integer;
begin
  size := 256;
  SetLength(Result, size);
  if ErrNo = -1 then
    ErrNo := GetLastError;
  MsgLen := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNo, 0, PChar(Result), size, nil);
  if MsgLen = 0 then
  begin
    Result := 'ERROR';
  end;
  SetLength(Result, StrLen(PChar(Result)));
end;

function BusyCallback(Sender: TObject; ObjectName: PChar; BusyCount: integer): integer; cdecl;
var
  sObjName: string;
  bCancel: Boolean;
begin
  Result := -1;
  with Sender as TSQLite do
  begin
    if Assigned(fOnBusy) then
    begin
      bCancel := False;
      sObjName := ObjectName;
      fOnBusy(Sender, sObjName, BusyCount, bCancel);
      if bCancel then
        Result := 0;
    end;
  end;
end;

function ExecCallback(Sender: TObject; Columns: Integer; ColumnValues: Pointer; ColumnNames: Pointer): integer; cdecl;
var
  PVal, PName: ^PChar;
  n: integer;
  sVal, sName: string;
begin
  Result := 0;
  with Sender as TSQLite do
  begin
    if (Assigned(fOnData) or Assigned(fTable)) then
    begin
      fLstName.Clear;
      fLstVal.Clear;
      if Columns > 0 then
      begin
        PName := ColumnNames;
        PVal := ColumnValues;
        for n := 0 to Columns - 1 do
        begin
          fLstName.Append(PName^);
          fLstVal.Append(PVal^);
          inc(PName);
          inc(PVal);
        end;
      end;
      sVal := fLstVal.CommaText;
      sName := fLstName.CommaText;
      if Assigned(fOnData) then
        fOnData(Sender, Columns, sName, sVal);
      if Assigned(fTable) then
      begin
        if fTable.Count = 0 then
          fTable.Append(sName);
        fTable.Append(sVal);
      end;
    end;
  end;
end;

constructor TSQLite.Create(DBFileName: string);
var
  fPMsg: PChar;
begin
  inherited Create;
  fError := SQLITE_ERROR;
  fIsOpen := False;
  fLstName := TStringList.Create;
  fLstVal := TStringList.Create;
  fOnData := nil;
  fOnBusy := nil;
  fOnQueryComplete := nil;
  fChangeCount := 0;
  if LibsLoaded then
  begin
    fSQLite := SQLite_Open(PChar(DBFileName), 1, fPMsg);
    SQLite_FreeMem(fPMsg);
    if fSQLite <> nil then
    begin
      fVersion := SQLite_Version;
      fEncoding := SQLite_Encoding;
      fIsOpen := True;
      fError := SQLITE_OK;
    end;
  end;
  fMsg := ErrorMessage(fError);
end;

destructor TSQLite.Destroy;
begin
  if fIsOpen then
    SQLite_Close(fSQLite);
  fIsOpen := False;
  fLstName.Free;
  fLstVal.Free;
  fSQLite := nil;
  fOnData := nil;
  fOnBusy := nil;
  fOnQueryComplete := nil;
  fLstName := nil;
  fLstVal := nil;
  inherited Destroy;
end;

function TSQLite.Query(Sql: string; Table: TStrings = nil): boolean;
//var
//  fPMsg: PChar;
begin
  fError := SQLITE_ERROR;
  if fIsOpen then
  begin
    fPMsg := nil;
    fBusy := True;
    fTable := Table;
    if fTable <> nil then
      fTable.Clear;
    fError := SQLite_Exec(fSQLite, PChar(Sql), @ExecCallback, Self, fPMsg);
    SQLite_FreeMem(fPMsg);
    fChangeCount := SQLite_Changes(fSQLite);
    fTable := nil;
    fBusy := False;
    if Assigned(fOnQueryComplete) then
      fOnQueryComplete(Self);
  end;
  fMsg := ErrorMessage(fError);
  Result := (fError <> SQLITE_OK);
end;

function TSQLite.Cancel: boolean;
begin
  Result := False;
  if fBusy and fIsOpen then
  begin
    SQLite_Cancel(fSQLite);
    fBusy := false;
    Result := True;
  end;
end;

procedure TSQLite.SetBusyTimeout(Timeout: Integer);
begin
  fBusyTimeout := Timeout;
  if fIsOpen then
  begin
    SQLite_BusyTimeout(fSQLite, fBusyTimeout);
    if fBusyTimeout > 0 then
      SQLite_BusyHandler(fSQLite, @BusyCallback, Self)
    else
      SQLite_BusyHandler(fSQLite, nil, nil);
  end;
end;

function TSQLite.LastInsertRow: integer;
begin
  if fIsOpen then
    Result := SQLite_LastInsertRow(fSQLite)
  else
    Result := -1;
end;

function TSQLite.ErrorMessage(ErrNo: Integer): string;
begin
  if LibsLoaded then
  begin
    if ErrNo = 0 then
      Result := MsgNoError
    else
      Result := SQLite_ErrorString(ErrNo);
  end else
    MessageBox(GetActiveWindow(), 'Library "sqlite.dll" not found.', 'Error loading DLL', MB_OK or MB_ICONHAND or MB_SETFOREGROUND);
end;

function TSQLite.IsComplete(Sql: string): boolean;
begin
  Result := SQLite_Complete(PChar(Sql));
end;

function TSQLite.DatabaseDetails(Table: TStrings): boolean;
begin
  Result := Query('SELECT * FROM SQLITE_MASTER;', Table);
end;

//*******************************//

function TResultCell.GetInteger;
begin
  Result := StrToIntDef(FValue, 0);
end;

function TResultCell.GetFloat;
begin
  try
    Result := StrToFloat(FValue);
  except
    Result := 0;
  end;
end;

function TResultCell.GetBoolean;
begin
  Result := (not FIsNull) and
    (((FValue <> '0') and (lowercase(FValue) <> 'false')) or
    (lowercase(FValue) = 'true'));
end;

constructor TResultRow.Create;
begin
  inherited;
  FResultCell := TResultCell.Create;
  FNulls := TList.Create;
  FNameValue := TStringList.Create;
end;

destructor TResultRow.Destroy;
begin
  FResultCell.Free;
  FNulls.Free;
  FNameValue.Free;
  inherited;
end;

function TResultRow.GetResultCell;
begin
  with FResultCell do
  begin
    if (i >= 0) and (i < count) then
    begin
      FValue := Strings[i];
      FIsNull := Integer(FNulls[i]) <> 0;
    end
    else
    begin
      FValue := '';
      FIsNull := True;
    end;
  end;
  Result := FResultCell;
end;

function TResultRow.GetString;
begin
  if (i >= 0) and (i < Count) then
    Result := Strings[i]
  else
    Result := '';
end;

function TResultRow.GetByField;
begin
  Result := GetResultCell(FFields.IndexOf(Value));
end;

function TResultRow.GetIsNull;
begin
  if (i >= 0) and (i < FNulls.Count) then
    Result := Integer(FNulls[i]) = 0
  else
    Result := True;
end;

function TResultRow.GetAsNameValue;
var i: Integer;
begin
  Result := FNameValue;
  Result.Clear;
  if FFields.Count <> Count then
    exit; //this will be an empty set (nilrow)
  for i := 0 to Count - 1 do
    Result.Add(FFields[i] + '=' + Strings[i])
end;

constructor TBaseInfo.Create;
begin
  inherited Create;
  CS := TCriticalSection.Create;
end;

destructor TBaseInfo.Destroy;
begin
  CS.Free;
  inherited Destroy;
end;

function QueryCallback(Sender: TObject; Columns: Integer; ColumnValues: Pointer; ColumnNames: Pointer): integer; cdecl;
var S: TResultRow;
  FieldName, Value: ^PChar;
  i: Integer;
begin
  //nice, we got a row. get it.
  with Sender as TLiteDB do
  begin
    inc(FRowCount);
    if FCallBackOnly then
      i := 1
    else
      i := FRowCount;
    if i <= FRowList.Count then
    begin
      S := TResultRow(FRowList[i - 1]);
      S.Clear;
      S.FNulls.Clear;
    end
    else
    begin
      S := TResultRow.Create;
      S.FFields := FFields; //copy pointer to ffields array
      FRowList.Add(S);
    end;
    if Columns > 0 then
    begin
      FieldName := ColumnNames;
      Value := ColumnValues;
      for i := 0 to Columns - 1 do
      begin
        S.Add(Value^);
        S.FNulls.Add(Pointer(Integer(Value^ <> nil)));
        inc(FQuerySize, length(string(Value^)));
        inc(Value);
      end;
      if FFields.Count = 0 then //do only once per query
        for i := 0 to Columns - 1 do
        begin
          FFields.Add(FieldName^);
          inc(FieldName);
        end;
      if Assigned(FOnFetchRow) then
      try
        FOnFetchRow(Sender, S);
      except end;
    end;
    QueryCallBack := Integer((not FCallBackOnly) and (FQuerySize > FMaxQuerySize)); //return 0 if ok for next row
  end;
end;

function QueryBusyCallback(Sender: TObject; ObjectName: PChar; BusyCount: integer): integer; cdecl;
begin
  with TLiteDB(Sender) do
  begin
    if FThreaded then
      sleep(1)
    else
    begin
{$IFDEF WITH_GUI}
      Application.ProcessMessages;
{$ENDIF}
      sleep(0);
    end;
  end;
  Result := -1;
end;


constructor TLiteDB.Create;
begin
  inherited Create{$IFDEF WITH_GUI}(AOwner){$ENDIF};
  if DataBase <> '' then
    Use(DataBase);
  FRowList := TList.Create;
  FFields := TStringList.Create;
  FNilRow := TResultRow.Create;
  FNilRow.FFields := FFields;
  FThreaded := True; //assume operation in thread by default
                     //client could set it off when in main thread
                     //to optimize for busy calls
  FMaxQuerySize := 16 * 1024 * 1024; //16MB
end;

destructor TLiteDB.Destroy;
var i: Integer;
begin
  for i := 0 to FRowList.Count - 1 do
    TResultRow(FRowList[i]).Free;
  FFields.Free;
  FRowList.Free;
  FNilRow.Free;
  inherited;
end;

function TLiteDB.Query;
var P: PChar;
  i: Integer;
begin
  Result := False;
  if not LibsLoaded or
    (FBaseInfo = nil) then
    exit;
  FFields.Clear;
  FRowCount := 0;
  FBaseInfo.CS.Enter;
  SQLite_BusyTimeout(FBaseInfo.FHandle, 2);
  SQLite_BusyHandler(FBaseInfo.FHandle, @QueryBusyCallback, Self);
  FQuerySize := 0;
  if SQL <> '' then //calling with empty string causes a result cleanup.
  begin
    FLastError := SQLite_exec(FBaseInfo.FHandle, PChar(SQL), @QueryCallBack, Self, P);
    SQLite_freemem(P);
  end;
  i := FRowList.Count - 1;
  if not FCallBackOnly then //we need to clean up
  begin
    while i >= FRowCount do
    begin
      TResultRow(FRowList[i]).Free;
      FRowList.Delete(i);
      dec(i);
    end;
  end;
  FRowsAffected := SQLite_Changes(FBaseInfo.FHandle);
  //we need to do this here (multi-threaded!):
  FLastInsertID := SQLite_lastinsertrow(FBaseInfo.FHandle);
  FBaseInfo.CS.Leave;
  FColCount := FFields.Count;
  Result := (FLastError = sqlite_ok);
  if Assigned(FOnQueryComplete) then
  try
    FOnQueryComplete(Self);
  except end;
end;

procedure TLiteDB.DoQuery; //procedure needed for SQL property
begin
  Query(SQL);
end;

function TLiteDB.GetOneResult;
begin
  if (FRowCount >= 1) and (FColCount >= 1) then
    Result := Results[0][0]
  else
    Result := '';
end;

function TLiteDB.QueryOne;
begin
  if Query(SQL) then
    Result := GetOneResult
  else
    Result := '';
end;

function TLiteDB.FormatQuery;

var i, j: Integer;
  c: char;
  sql: string;
begin
  //we could call the format function,
  //but we need to escape non-numerical values anyhow
  //open arrays are fun since they involve some compiler magic :)
  Result := False;
  sql := '';
//  Query (''); //empty result set
  //to-do: this function fails if some string contains a '%'.. fix it.
  for i := 0 to high(Args) do
  begin

    j := pos('%', Value);
    if j < length(Value) then
      c := upcase(Value[j + 1])
    else
      c := #0; //exit;
    sql := sql + copy(Value, 1, j - 1);
    Value := copy(Value, j + 2, maxint);

    with Args[i] do
      case VType of
        vtBoolean:
          begin
            if c <> 'B' then
              exit; //illegal format
            sql := sql + IntToStr(Integer(VBoolean));
          end;
        vtInteger:
          begin
            if c <> 'D' then
              exit; //illegal format
            sql := sql + IntToStr(VInteger);
          end;
        vtString:
          begin
            if c <> 'S' then
              exit; //illegal format
            sql := sql + '''' + string(VString^) + '''';
          end;
        vtChar:
          begin
            if c <> 'S' then
              exit; //illegal format
            sql := sql + '''' + {$IFDEF DELPHI12_UP}String{$ENDIF}(VChar) + '''';
          end;
        vtExtended:
          begin
            if c <> 'F' then
              exit; //illegal format
            sql := sql + '''' + FloatToStr(Extended(VExtended^));
          end;
        vtInt64:
          begin
            if c <> 'I' then
              exit; //illegal format
            sql := sql + '''' + IntToStr(VInt64^);
          end;
        vtAnsiString:
          begin
            if c <> 'S' then
              exit; //illegal format
            sql := sql + '''' + string(VAnsiString) + '''';
          end;
        vtVariant:
          begin
            if c <> 'V' then
              exit; //illegal format
            sql := sql + '''' + string(VVariant^) + '''';
          end;
        vtCurrency:
          begin
            if c <> 'C' then
              exit; //illegal format
            sql := sql + '''' + CurrToStr(VCurrency^) + '''';
          end;
      else
        exit;
      end; //case
  end;
  sql := sql + Value;
  Result := Query(sql);
{
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  I: Integer;
  R,s:String;
begin
  R := '';
  query('');
  for I := 0 to High(Args) do
    begin
      r:='';
      s:=s+r;
      with Args[I] do
        case VType of
//          vtBoolean:    r := r + BoolChars[VBoolean];
          vtInteger:    r := r + IntToStr(VInteger);

          vtChar:       begin
                          if r<>'' then
                            ;
                          r := r + VChar;
                        end;
          vtExtended:   r := r + FloatToStr(VExtended^);

          vtString:     r := r + VString^;
//          vtPChar:      r := r + VPChar;
          vtObject:     r := r + VObject.ClassName;
  //        vtClass:      r := r + VClass.ClassName;
  //        vtAnsiString: r := r + string(VAnsiString);
          vtCurrency:   r := r + CurrToStr(VCurrency^);
          vtVariant:    r := r + string(VVariant^);
          vtInt64:      r := r + IntToStr(VInt64^);
        else
          r:=r+'';
      end;
    end;
  Result := R<>'';
  query(r);
}
end;

function TLiteDB.GetField;
begin
  if (i >= 0) and (i < FFields.Count) then
    Result := FFields[i]
  else
    Result := '';
end;

procedure TLiteDB.Use;
var B: TBaseInfo;
  i: Integer;
  P: PChar;
begin
  //use this database
  if not LibsLoaded then
    exit; //just do nothing
  CSConnect.Enter;
  if FDataBase <> '' then //unregister
  begin
    i := DataBases.IndexOf(FDataBase);
    if i >= 0 then //should always be!
      with TBaseInfo(DataBases.Objects[i]) do
      begin
        dec(ReferenceCount);
        if ReferenceCount = 0 then
        begin
                //close handle
          SQLite_close(FHandle);
          Free;
          DataBases.Delete(i);
        end;
      end;
  end;
  if DataBases.IndexOf(DataBase) < 0 then
  begin
    B := TBaseInfo.Create;
    DataBases.AddObject(DataBase, B);
    inc(B.ReferenceCount);
      // Modified by QSoft, 2003-11-17
    B.FHandle := SQLite_open(PChar(DataBase {+'.lit'}), 1, P);
    if Assigned(B.FHandle) then
    begin
      FVersion := SQLite_version;
      FEncoding := SQLite_encoding;
    end;
    FBaseInfo := B;
  end
  else
    FBaseInfo := TBaseInfo(DataBases.Objects[DataBases.IndexOf(DataBase)]);
  FDataBase := DataBase;
  CSConnect.Leave;
end;

function TLiteDB.GetResult;
begin
  if (i >= 0) and (i < FRowList.Count) then
    Result := TResultRow(FRowList[i])
  else
    Result := FNilRow; //give back a valid pointer to an empty row
end;

function TLiteDB.GetErrorMessage;
begin
  if Assigned(FBaseInfo) then
    Result := sqlite_errorstring(FLastError);
end;


procedure TLiteDB.Lock;
begin
  if Assigned(FBaseInfo) then
    FBaseInfo.CS.Enter;
end;

procedure TLiteDB.UnLock;
begin
  if Assigned(FBaseInfo) then
    FBaseInfo.CS.Leave;
end;

procedure TLiteDB.StartTransaction;
begin
  if Assigned(FBaseInfo) then
  begin
    Lock;
    Query('BEGIN'); //start transaction
  end;
end;

procedure TLiteDB.Commit;
begin
  if Assigned(FBaseInfo) then
  begin
    Query('COMMIT');
    Unlock;
  end;
end;

procedure TLiteDB.Rollback;
begin
  if Assigned(FBaseInfo) then
  begin
    Query('ROLLBACK');
    Unlock;
  end;
end;

function TLiteDB.QueryStrings;
var i: Integer;
begin
  Strings.Clear;
  Result := Query(SQL);
  if Result then
  begin
    for i := 0 to FRowCount - 1 do
      Strings.Add(Results[i][0]);
  end;
end;

{$IFDEF WITH_GUI}

function TLiteDB.QueryComboBox;
var S: string;
begin
  S := ComboBox.Text;
  Result := QueryStrings(SQL, ComboBox.Items);
  if Result then
  begin
      //if ComboBox.IndexOf(S)>=0 then
      //  ComboBox.ItemIndex := ComboBox.IndexOf(S);
      //if style=dropdownlist then this may not have effect
    ComboBox.Text := S;
  end;
  //Add an empty item {not always wanted}
  if AddEmpty then
    ComboBox.Items.Add('');
end;

function TLiteDB.QueryStringGrid;
var i, j: Integer;
begin
  Result := Query(SQL);
  Grid.RowCount := FRowCount + 1;
  Grid.ColCount := FFields.Count + 1;
  for i := 0 to FFields.Count - 1 do
  begin
    Grid.Cells[i + 1, 0] := FFields[i];
    Grid.ColWidths[i + 1] := Grid.Canvas.TextWidth(FFields[i]) + 6;
  end;
  Grid.ColWidths[0] := 6;
  for i := 0 to FRowCount - 1 do
  begin
    Grid.Cells[0, i + 1] := IntToStr(i + 1);
    if Grid.ColWidths[0] < (Grid.Canvas.TextWidth(IntToStr(i + 1)) + 6) then
      Grid.ColWidths[0] := Grid.Canvas.TextWidth(IntToStr(i + 1)) + 6;
    for j := 0 to FFields.Count - 1 do
    begin
      Grid.Cells[1 + j, i + 1] := Results[i][j];
      if Grid.ColWidths[j + 1] < (Grid.Canvas.TextWidth(Results[i][j]) + 6) then
        Grid.ColWidths[j + 1] := Grid.Canvas.TextWidth(Results[i][j]) + 6;
    end;
  end;
end;
{$ENDIF}

procedure InitSQLite;
begin
  LibsLoaded := LoadLibs;
  MsgNoError := SystemErrorMsg(0);
  DataBases := TStringList.Create;
  CSConnect := TCriticalSection.Create;
end;

procedure FreeSQLite;
var
  i: Integer;
begin
  if DLLHandle <> 0 then
    FreeLibrary(DLLHandle);
  for i := 0 to DataBases.Count - 1 do
    TBaseInfo(DataBases.Objects[i]).Free;
  DataBases.Free;
  CSConnect.Free;
end;

initialization
  InitSQLite;

finalization
  FreeSQLite;

end.

