{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnDHibernateBase; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准类库
* 单元名称：DHibernate基础类/接口单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Classes, SysUtils, CnDHibernateConsts, TypInfo, DB, ADODB, StrUtils, Controls, Windows;

type
  TCnNoMappingException = Exception;

  TCnNoConnectionException = Exception;

  TCnNoTableException = Exception;

  TCnNoFileException = Exception;

  TCnNoExcelException = Exception;

  TCnNoSheetNameException = Exception;

  TCnNoSQLException = Exception; 

  { string buffer }
  TCnStringBuffer = class(TStringList)
  public
    constructor Create(Str: string); reintroduce;
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING}override;{$ENDIF}
  end; 

  { Map table }
  TCnDHHashMapTable = record
    hashName: string;
    hashValue: Variant;
  end;

  PCnDHHashMapTable = ^TCnDHHashMapTable; 

  { Map interface, must be implements by TCnDHHashMap }
  ICnMap = interface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure put(hashName: string; hashValue: Variant); stdcall;
    function get(hashName: string): Variant; stdcall;
    function remove(hashName: string): Boolean; stdcall; // add 1.6
    function getTable(index: Integer): PCnDHHashMapTable; stdcall;
    procedure clear; stdcall;           // add 1.6
    function size: Integer; stdcall;
  end; 

  { HashMap }
  TCnDHHashMap = class(TObject, ICnMap)
  private
    FTable: array of TCnDHHashMapTable;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure put(hashName: string; hashValue: Variant); stdcall;
    function remove(hashName: string): Boolean; stdcall; // add 1.6
    function get(hashName: string): Variant; stdcall;
    function getTable(index: Integer): PCnDHHashMapTable; stdcall;
    function size: Integer; stdcall;
    procedure clear; stdcall;           // add 1.6
    constructor Create;
    destructor Destroy; override;
  end; 

  { Dhibernate base class, for PODO }
  TCnDHibernateBase = class(TPersistent)
  end;

function getPodoProperties(TableName: string; data: TObject): ICnMap;

function getSearchEvent(param: ICnMap): string;

function boolToYN(b: Boolean): string;

procedure setAllNumber(component: TWinControl);

procedure setReadOnly(component: TWinControl);

function isNumber(str: string): boolean;

function isZero(str: string): Boolean;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TIntfClass }

procedure TCnDHHashMap.clear;
begin
  SetLength(FTable, 0);
end;

constructor TCnDHHashMap.Create;
begin
  SetLength(FTable, 0);
end;

destructor TCnDHHashMap.Destroy;
begin
  SetLength(FTable, 0);
  inherited;
end;

function TCnDHHashMap.get(hashName: string): Variant;
var
  i: Integer;
begin
  Result := DH_NULL_VAR;
  for i := 0 to Length(FTable) - 1 do
  begin
    if FTable[i].hashName = hashName then
    begin
      Result := FTable[i].hashValue;
      break;
    end;
  end;
end;

function TCnDHHashMap.getTable(index: Integer): PCnDHHashMapTable;
begin
  { if the index was overflow }
  if (Length(FTable) <= index) or (index < 0) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @FTable[index];
end;

procedure TCnDHHashMap.put(hashName: string; hashValue: Variant);
var
  i: Integer;
  len: Integer;
begin
  len := Length(FTable);
  for i := 0 to len - 1 do
  begin
    if FTable[i].hashName = hashName then
    begin
      FTable[i].hashValue := hashValue;
      Exit;
    end;
  end;
  SetLength(FTable, len + 1);
  FTable[len].hashName := hashName;
  FTable[len].hashValue := hashValue;
end;

function TCnDHHashMap.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCnDHHashMap.remove(hashName: string): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to Length(FTable) - 1 do
  begin
    if FTable[i].hashName = hashName then
    begin
      // found!
      for j := i to Length(FTable) - 2 do
      begin
        FTable[j].hashName := FTable[j + 1].hashName;
        FTable[j].hashValue := FTable[j + 1].hashValue;
      end;
      SetLength(FTable, Length(FTable) - 1);
      Result := True;
      Break;
    end;
  end;
end;

function TCnDHHashMap.size: Integer;
begin
  Result := Length(FTable);
end;

function TCnDHHashMap._AddRef: Integer;
begin
  Result := -1;
end;

function TCnDHHashMap._Release: Integer;
begin
  Result := -1;
end; 

{ TCnStringBuffer }

constructor TCnStringBuffer.Create(Str: string);
begin
  inherited Create;
  Self.Text := Str;
end;

function TCnStringBuffer.ToString: string;
begin
  Result := Self.Text;
end;

function getPodoProperties(TableName: string; data: TObject): ICnMap;
var
  clazz: TClass;
  obj: TObject;
  Pplst: PPropList;
  Classtypeinfo: PTypeInfo;
  classDataInfo: PTypeData;
  i: Integer;
  tk: TTypeKind;
  map: ICnMap; 
  i64 : Int64;
begin
  map := TCnDHHashMap.Create;
  clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
  obj := data;
  Classtypeinfo := clazz.ClassInfo;
  classDataInfo := GetTypeData(Classtypeinfo);
  if classDataInfo.PropCount <> 0 then
  begin
    GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    try
      GetPropInfos(Classtypeinfo, Pplst);
      for i := 0 to classDataInfo.PropCount - 1 do
      begin
        if (RightStr(pplst[i]^.Name, 8) = '_FORMULA') or (RightStr(pplst[i]^.Name, 4) = '_SQL') then
          Continue;
        tk := Pplst[i]^.PropType^.Kind;
        if tk <> tkMethod then
        begin
          // set the string properties
          if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF}  then
          begin
            map.put({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), GetStrProp((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name)));
          end; 
          // set the integer properties
          if tk = tkInteger then
          begin
            try
              i64 := GetInt64Prop((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name));
              map.put({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), i64);
            except
              map.put({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), 0);
            end;
          end; 
          // set the float properties
          if tk = tkFloat then
          begin
            try
              map.put({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), GetFloatProp((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name)));
            except
              map.put({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), 0);
            end;
          end; 
          // set the variant properties
          if tk = tkVariant then
          begin
            map.put({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), GetVariantProp((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name)));
          end;
        end;
      end;
    finally
      FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    end;
  end;
  Result := map;
end;

function getSearchEvent(param: ICnMap): string;
var
  i: Integer;
  evt: string;
  mName: string;
  mValue: Variant;
begin
  evt := EmptyStr;
  for i := 0 to param.size - 1 do
  begin
    mName := param.getTable(i).hashName;
    mValue := param.getTable(i).hashValue; 
    // 不对 formula 进行拼装
    if (RightStr(mName, 8) = '_FORMULA') or (RightStr(mName, 4) = '_SQL') then
      Continue;
    try
      if (mValue = EmptyStr) then
        Continue;
    except
      // 不处理异常

    end;
    if isZero(string(mValue)) then
      Continue;
    evt := evt + Format(DH_SEARCH_FILTER, [mName, mValue]);
  end;
  Result := evt;
end;

function boolToYN(b: Boolean): string;
begin
  if b then
    Result := 'Y'
  else
    Result := 'N';
end;

procedure setAllNumber(component: TWinControl);
var
  w: Cardinal;
begin
  w := GetWindowLong(component.Handle, GWL_STYLE);
  SetWindowLong(component.Handle, GWL_STYLE, w or ES_NUMBER);
end;

procedure setReadOnly(component: TWinControl);
var
  w: Cardinal;
begin
  w := GetWindowLong(component.Handle, GWL_STYLE);
  SetWindowLong(component.Handle, GWL_STYLE, w or ES_READONLY);
end;

function isNumber(str: string): boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(str) do
  begin
    if (str[i] < '0') or (str[i] > '9') then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function isZero(str: string): Boolean;
var
  i: integer;
begin
  if not isNumber(str) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for i := 1 to Length(str) do
  begin
    if str[i] <> '0' then
    begin
      Result := False;
      Break;
    end;
  end;
end; 

{$ENDIF SUPPORT_ADO}
end.
