{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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

unit CnDHibernateSubQueryAdv; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate扩展控件库
* 单元名称：高级Sub-Query控件单元
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

interface

{$I CnPack.inc}

{$M+}
{$WARNINGS OFF}

{$IFDEF SUPPORT_ADO}

uses
  SysUtils, Classes, DB, ADODB, CnDHibernateMemData, CnDHibernateClasses,
  CnDHibernatePodoList, CnDHibernateBase, TypInfo, CnDHibernateConsts, StrUtils, CnDHibernateSubQuery, CnDHibernateSet, DBClient;

type
  TCnDHibernateSubQueryAdvance = class(TClientDataSet)
  private
    FDHQuery: TCnDHibernateSubQuery;
    FRowsPerPage: integer;
    FCurrentPage: integer;
    FConnection: TAdoConnection;
    FActive: boolean;
    FTableName: string;
    FuseFormula: boolean;
    FPKName: string;
    FAbout: string;
    FMainTableName: string;
    FSubTableName: string;
    FSubTableRefField: string;
    FSubTablePKName: string;
    FMainTablePK: string;
    FMainTablePKValue: Variant;
    procedure SetRowsPerPage(const Value: integer);
    procedure SetCurrentPage(const Value: integer);
    function GetPagecount: integer;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    procedure SetConnection(const Value: TAdoConnection);
    procedure SetActive(const Value: boolean);
    procedure SetMainTableName(const Value: string);
    procedure SetMainTablePK(const Value: string);
    procedure SetMainTablePKValue(const Value: Variant);
    procedure SetSubTableName(const Value: string);
    procedure SetSubTablePKName(const Value: string);
    procedure SetSubTableRefField(const Value: string);
  protected
    procedure GetFields; 
    { 获取 formula 字段 }
    procedure GetFormulaFields;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure PriorPage;
    procedure NextPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Open;
    procedure Close;
    procedure Refresh;
    procedure find(hql: string; param: ICnMap);
    function get(TableName: string; pkName: string; pkValue: Variant): TObject;
    function saveDetail(items: ICnSet): Boolean;
    function updateDetail(items: ICnSet): Boolean;
    function deleteDetail(items: ICnSet): Boolean;
    function saveOrUpdateDetail(items: ICnSet): Boolean; 
    //    { save }
    //    function saveData(TableName: string; data: TObject): Boolean;
    //    { update }
    //    function updateData(TableName: string; data: TObject; pkName: string): Boolean;
    //    { delete }
    //    function deleteData(TableName: string; data: TObject): Boolean;
    //    { save or update }
    //    function saveOrUpdateData(TableName: string; data: TObject; pkName: string): Boolean;
    //    { save all }
    //    function saveAllData(TableName: string; dataList: TPodoList): Boolean;
    //    { update all }
    //    function updateAllData(TableName: string; dataList: TPodoList; pkName: string): Boolean;
    //    { delete all }
    //    function deleteAllData(TableName: string; dataList: TPodoList): Boolean;
    //    { save or update all }
    //    function saveOrUpdateAllData(TableName: string; dataList: TPodoList; pkName: string): Boolean;
  published
    property About: string read FAbout write FAbout;
    property Active: boolean read FActive write SetActive;
    property Connection: TAdoConnection read FConnection write SetConnection;
    property SQL: TStrings read GetSQL write SetSQL;
    property PageCount: integer read GetPagecount;
    property CurrentPage: integer read FCurrentPage write SetCurrentPage;
    property RowsPerPage: integer read FRowsPerPage write SetRowsPerPage default 10; 
    { table name for formula }
    property TableName: string read FTableName write FTableName; 
    { whether use formula or not? }
    property useFormula: boolean read FuseFormula write FuseFormula; 
    { pk name }
    property PKName: string read FPKName write FPKName;
    property Query: TCnDHibernateSubQuery read FDHQuery; 

    { inherited in TDHibernateSubQuery }
    property MainTableName: string read FMainTableName write SetMainTableName;
    property MainTablePK: string read FMainTablePK write SetMainTablePK;
    property MainTablePKValue: Variant read FMainTablePKValue write SetMainTablePKValue;
    property SubTableRefField: string read FSubTableRefField write SetSubTableRefField;
    property SubTableName: string read FSubTableName write SetSubTableName;
    property SubTablePKName: string read FSubTablePKName write SetSubTablePKName;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateSubQueryAdvance }

procedure TCnDHibernateSubQueryAdvance.Close;
begin
  inherited Close;
  FDHQuery.Close;
  FActive := false;
end;

constructor TCnDHibernateSubQueryAdvance.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FConnection := nil;
  FActive := false;
  FDHQuery := TCnDHibernateSubQuery.Create(nil);
  RowsPerPage := 10;
end; 
//
//function TCnDHibernateSubQueryAdvance.deleteAllData(TableName: string;
//  dataList: TPodoList): Boolean;
//begin
//  Result := FDHQuery.deleteAllData(TableName, dataList);
//end;
//
//function TCnDHibernateSubQueryAdvance.deleteData(TableName: string;
//  data: TObject): Boolean;
//begin
//  Result := FDHQuery.deleteData(TableName, data);
//end;

function TCnDHibernateSubQueryAdvance.deleteDetail(items: ICnSet): Boolean;
begin
  Result := FDHQuery.deleteDetail(items);
end;

destructor TCnDHibernateSubQueryAdvance.Destroy;
begin
  FDHQuery.Connection := nil;
  FDHQuery.Free;
  FConnection := nil;
  inherited Destroy;
end;

procedure TCnDHibernateSubQueryAdvance.find(hql: string; param: ICnMap);
var
  i: Integer;
begin
  Self.Close;
  FDHQuery.SQL.Text := hql;
  for i := 0 to param.size - 1 do
  begin
    FDHQuery.Parameters.ParamValues[param.getTable(i)^.hashName] := param.getTable(i)^.hashValue;
  end;
  Self.Open;
end;

procedure TCnDHibernateSubQueryAdvance.FirstPage;
begin
  if not FActive then
    Exit;
  CurrentPage := 1;
end;

function TCnDHibernateSubQueryAdvance.get(TableName, pkName: string; pkValue: Variant): TObject;
begin
  Result := FDHQuery.get(TableName, pkName, pkValue);
end;

procedure TCnDHibernateSubQueryAdvance.GetFields;
var
  i: integer;
  ft: TFieldType;
begin
  FieldDefs.Clear;
  for i := 0 to FDHQuery.FieldCount - 1 do
  begin
    with FDHQuery.Fields[i] do
    begin
      ft := DataType;
      if ft = ftWideString then
        ft := ftString; 
      { if ft = ftWideMemo then
        ft := ftMemo; }
      FieldDefs.Add(FieldName, ft, Size, Required);
    end;
  end;
  GetFormulaFields;
  CreateDataSet;
end;

procedure TCnDHibernateSubQueryAdvance.GetFormulaFields;
var
  clazz: TClass;
  Pplst: PPropList;
  Classtypeinfo: PTypeInfo;
  classDataInfo: PTypeData;
  i: Integer;
  tk: TTypeKind;
  ppName: string;
begin
  // todo : get formula fields
  clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
  Classtypeinfo := clazz.ClassInfo;
  classDataInfo := GetTypeData(Classtypeinfo);
  if classDataInfo.PropCount <> 0 then
  begin
    GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    try
      GetPropInfos(Classtypeinfo, Pplst);
      for i := 0 to classDataInfo.PropCount - 1 do
      begin
        ppName := pplst[i]^.Name; 
        // check whether formula attr
        if RightStr(ppName, 8) = '_FORMULA' then
        begin
          tk := Pplst[i]^.PropType^.Kind;
          if tk <> tkMethod then
          begin
            if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF} then
              FieldDefs.Add(ppName, ftString, 255, False);
            if tk = tkInteger then
              FieldDefs.Add(ppName, ftInteger, 8, False);
            if tk = tkFloat then
              FieldDefs.Add(ppName, ftFloat, 32, False);
            if tk = tkVariant then
              FieldDefs.Add(ppName, ftVariant, 255, False);
          end;
        end;
      end;
    finally
      FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    end;
  end;
end;

function TCnDHibernateSubQueryAdvance.GetPagecount: integer;
begin
  Result := 0;
  if not FActive then
    Exit;
  Result := Trunc(FDHQuery.RecordCount / FRowsPerPage);
  if Result * FRowsPerPage <> FDHQuery.RecordCount then
    Result := Result + 1;
end;

function TCnDHibernateSubQueryAdvance.GetSQL: TStrings;
begin
  Result := TStrings(FDHQuery.SQL);
end;

procedure TCnDHibernateSubQueryAdvance.LastPage;
begin
  if not FActive then
    Exit;
  CurrentPage := PageCount;
end;

procedure TCnDHibernateSubQueryAdvance.NextPage;
begin
  if not FActive then
    Exit;
  if CurrentPage < PageCount then
    CurrentPage := CurrentPage + 1;
end;

procedure TCnDHibernateSubQueryAdvance.Open;
begin
  try
    FDHQuery.Open;
  except
    FActive := false;
    Exit;
  end;
  FActive := True;
  GetFields;
  inherited Open;
  CurrentPage := 1;
  First;
end;

procedure TCnDHibernateSubQueryAdvance.PriorPage;
begin
  if not FActive then
    Exit;
  if CurrentPage > 1 then
    CurrentPage := CurrentPage - 1;
end;

procedure TCnDHibernateSubQueryAdvance.Refresh;
var
  pg: Integer;
begin
  pg := CurrentPage;
  Close;
  Open;
  CurrentPage := pg;
end; 

//function TCnDHibernateSubQueryAdvance.saveAllData(TableName: string;
//  dataList: TPodoList): Boolean;
//begin
//  Result := FDHQuery.saveAllData(TableName, dataList);
//end;
//
//function TCnDHibernateSubQueryAdvance.saveData(TableName: string;
//  data: TObject): Boolean;
//begin
//  Result := FDHQuery.saveData(TableName, data);
//end;
//
//function TCnDHibernateSubQueryAdvance.saveOrUpdateAllData(TableName: string;
//  dataList: TPodoList; pkName: string): Boolean;
//begin
//  Result := FDHQuery.saveOrUpdateAllData(TableName, dataList, pkName);
//end;
//
//function TCnDHibernateSubQueryAdvance.saveOrUpdateData(TableName: string;
//  data: TObject; pkName: string): Boolean;
//begin
//  Result := FDHQuery.saveOrUpdateData(TableName, data, pkName);
//end;

function TCnDHibernateSubQueryAdvance.saveDetail(items: ICnSet): Boolean;
begin
  Result := FDHQuery.saveDetail(items);
end;

function TCnDHibernateSubQueryAdvance.saveOrUpdateDetail(items: ICnSet): Boolean;
begin
  Result := FDHQuery.saveOrUpdateDetail(items);
end;

procedure TCnDHibernateSubQueryAdvance.SetActive(const Value: boolean);
begin
  if (FConnection = nil) or (FDHQuery.SQL.Text = '') then
    Exit;
  FActive := Value;
  if FActive then
    Open
  else
    Close;
end;

procedure TCnDHibernateSubQueryAdvance.SetConnection(const Value: TAdoConnection);
begin
  Close;
  FConnection := Value;
  FDHQuery.Connection := FConnection;
end;

procedure TCnDHibernateSubQueryAdvance.SetCurrentPage(const Value: integer);
var
  i, j, k: integer;
  start: integer;
  obj: TObject;
  clazz: TClass;
  Pplst: PPropList;
  Classtypeinfo: PTypeInfo;
  classDataInfo: PTypeData;
  ppName: string;
  tk: TTypeKind;
begin
  if not FActive then
    Exit;
  if (Value <= 0) or (Value > PageCount) then
    Exit;
  FCurrentPage := Value; 
  // 清空记录
  for i := 1 to RecordCount do
    Delete; 
  // 取记录
  start := (FCurrentPage - 1) * FRowsPerPage + 1;
  for i := start to start + FRowsPerPage - 1 do
  begin
    FDHQuery.RecNo := i;
    if FDHQuery.RecNo <> i then
      Break;
    clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
    obj := FDHQuery.get(TableName, PKName, FDHQuery.FieldByName(PKName).Value);
    Append; 
    // 常规记录
    for j := 0 to FDHQuery.FieldCount - 1 do
      Fields[j].Value := FDHQuery.Fields[j].Value; 
    // formula 记录
    if useFormula then
    begin
      // todo : add formula records
      Classtypeinfo := clazz.ClassInfo;
      classDataInfo := GetTypeData(Classtypeinfo);
      if classDataInfo.PropCount <> 0 then
      begin
        GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
        try
          GetPropInfos(Classtypeinfo, Pplst);
          for k := 0 to classDataInfo.PropCount - 1 do
          begin
            ppName := pplst[k]^.Name; 
            // check whether formula attr
            if RightStr(ppName, 8) = '_FORMULA' then
            begin
              tk := Pplst[k]^.PropType^.Kind;
              if tk <> tkMethod then
              begin
                if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF} then
                  FieldByName(ppName).AsString := GetStrProp((obj as clazz), ppName);
                if tk = tkInteger then
                  FieldByName(ppName).AsInteger := GetInt64Prop((obj as clazz), ppName);
                if tk = tkFloat then
                  FieldByName(ppName).AsFloat := GetFloatProp((obj as clazz), ppName);
                if tk = tkVariant then
                  FieldByName(ppName).AsVariant := GetVariantProp((obj as clazz), ppName);
              end;
            end;
          end;
        finally
          FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
        end;
      end;
    end;
    Post;
  end;
end;

procedure TCnDHibernateSubQueryAdvance.SetMainTableName(const Value: string);
begin
  FMainTableName := Value;
  FDHQuery.MainTableName := Value;
end;

procedure TCnDHibernateSubQueryAdvance.SetMainTablePK(const Value: string);
begin
  FMainTablePK := Value;
  FDHQuery.MainTablePK := Value;
end;

procedure TCnDHibernateSubQueryAdvance.SetMainTablePKValue(const Value: Variant);
begin
  Close;
  FMainTablePKValue := Value;
  FDHQuery.MainTablePKValue := Value;
  Open;
end;

procedure TCnDHibernateSubQueryAdvance.SetRowsPerPage(const Value: integer);
begin
  Close;
  if Value <= 0 then
    Exit;
  FRowsPerPage := Value;
end;

procedure TCnDHibernateSubQueryAdvance.SetSQL(const Value: TStrings);
begin
  Close;
  FDHQuery.SQL.Assign(Value);
end; 

//function TCnDHibernateSubQueryAdvance.updateAllData(TableName: string;
//  dataList: TPodoList; pkName: string): Boolean;
//begin
//  Result := FDHQuery.updateAllData(TableName, dataList, pkName);
//end;
//
//function TCnDHibernateSubQueryAdvance.updateData(TableName: string;
//  data: TObject; pkName: string): Boolean;
//begin
//  Result := FDHQuery.updateData(TableName, data, pkName);
//end;

procedure TCnDHibernateSubQueryAdvance.SetSubTableName(const Value: string);
begin
  FSubTableName := Value;
  FDHQuery.SubTableName := Value;
end;

procedure TCnDHibernateSubQueryAdvance.SetSubTablePKName(const Value: string);
begin
  FSubTablePKName := Value;
  FDHQuery.SubTablePKName := Value;
end;

procedure TCnDHibernateSubQueryAdvance.SetSubTableRefField(const Value: string);
begin
  FSubTableRefField := Value;
  FDHQuery.SubTableRefField := Value;
end;

function TCnDHibernateSubQueryAdvance.updateDetail(items: ICnSet): Boolean;
begin
  Result := FDHQuery.updateDetail(items);
end; 

{$ENDIF SUPPORT_ADO}
end.
