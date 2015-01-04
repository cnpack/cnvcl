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

unit CnADOUpdateSQL;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：ADO多表更新单元
* 单元作者：小夏
* 备    注：支持TClientDataSet连接ADO的多表更新类
* 开发平台：PWin2000Pro + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串暂不符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.04.21 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Classes, SysUtils, Math, ADODB, DB, Provider, DBClient, Dialogs
  {$IFDEF COMPILER6_UP}, Variants{$ENDIF}, CnConsts, CnClasses, CnDBConsts;

type
  TCnConnectionType = (ctConnection, ctDataSet, ctProvider);
  {* 通过哪种数据集连接}

  TCnADOUpdateSQL = class(TCnComponent)
  private
    FConnection: TADOConnection;
    FConnectionType: TCnConnectionType;
    FDataSet: TDataSet;
    FProvider: TCustomProvider;
    FQueries: array[TUpdateKind] of TADOQuery;
    FSQLText: array[TUpdateKind] of TStrings;
    function GetConnection: TADOConnection;
    function GetConnectionType: TCnConnectionType;
    function GetProvider: TCustomProvider;
    function GetQuery(UpdateKind: TUpdateKind): TADOQuery;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetConnection(Value: TADOConnection);
    procedure SetConnectionType(Value: TCnConnectionType);
    procedure SetProvider(Value: TCustomProvider);
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
    procedure UpdateRecord(Sender: TObject; SourceDS: TDataSet;
      DeltaDS: {$IFDEF COMPILER6_UP} TCustomClientDataSet {$ELSE} TClientDataSet {$ENDIF};
      UpdateKind: TUpdateKind; var Applied: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function GetSQL(UpdateKind: TUpdateKind): TStrings;
    function GetDataSet: TDataSet;
    procedure SetDataSet(ADataSet: TDataSet);
    procedure SQLChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind);
    procedure ExecSQL(UpdateKind: TUpdateKind); virtual;
    //procedure SetParams(UpdateKind: TUpdateKind; DeltaDS: TDataSet); virtual;
    procedure SetParams(UpdateKind: TUpdateKind; DeltaDS: TDataSet); virtual;
    property Query[UpdateKind: TUpdateKind]: TADOQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property Connection: TADOConnection read GetConnection write SetConnection;
    property ConnectionType: TCnConnectionType read GetConnectionType write SetConnectionType default ctConnection;
    property DataSet: TDataSet read GetDataSet write SetDataSet;
    property Provider: TCustomProvider read GetProvider write SetProvider;
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnADOUpdateSQL }

constructor TCnADOUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited Create(AOwner);
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;

  //关联BeforeUpdateRecord事件
  if Assigned(FProvider) and (FProvider is TDataSetProvider) then
    TDataSetProvider(FProvider).BeforeUpdateRecord := UpdateRecord;
end;

destructor TCnADOUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind].Free;
    //FQueries[UpdateKind].Free; // 由于Create(Self)，所以在这里没必要手动释放
  end;
  inherited Destroy;
end;

procedure TCnADOUpdateSQL.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnADOUpdateSqlName;
  Author := SCnPack_XiaoXia;
  Email := SCnPack_XiaoXiaEmail;
  Comment := SCnADOUpdateSqlComment;
end;          

procedure TCnADOUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
begin
  with Query[UpdateKind] do
  begin
    Prepared := True;
    ExecSQL;
    if RowsAffected <> 1 then
      raise Exception.Create('Update Failed.');
  end;
end;

function TCnADOUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TADOQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TADOQuery.Create(Self);
    case FConnectionType of
      ctConnection:
        if (FConnection is TADOConnection) then
          FQueries[UpdateKind].Connection := FConnection; 
      ctDataSet:
        if (FDataSet is TCustomADODataSet) then
          FQueries[UpdateKind].Connection := TCustomADODataSet(FDataSet).Connection;
      ctProvider:
        if (FProvider is TDataSetProvider) then
          FQueries[UpdateKind].Connection := TCustomADODataSet(TDataSetProvider(FProvider).DataSet).Connection;
    end;
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]); //先取得Connection，再赋SQL.Text值，才能取得Parameters.Count
  end;
  Result := FQueries[UpdateKind];
end;

function TCnADOUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TCnADOUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

function TCnADOUpdateSQL.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TCnADOUpdateSQL.SetDataSet(ADataSet: TDataSet);
begin
  inherited;
  FDataSet := ADataSet;
end;

procedure TCnADOUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TCnADOUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

function TCnADOUpdateSQL.GetConnection: TADOConnection;
begin
  Result := FConnection;
end;

procedure TCnADOUpdateSQL.SetConnection(Value: TADOConnection);
begin
  FConnection := Value;
end;

function TCnADOUpdateSQL.GetProvider: TCustomProvider;
begin
  Result := FProvider;
end;

procedure TCnADOUpdateSQL.SetProvider(Value: TCustomProvider);
begin
  FProvider := Value;
end;

function TCnADOUpdateSQL.GetConnectionType: TCnConnectionType;
begin
  Result := FConnectionType;
end;

procedure TCnADOUpdateSQL.SetConnectionType(Value: TCnConnectionType);
begin
  FConnectionType := Value;
end;

procedure TCnADOUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
      begin
        FQueries[UpdateKind].Parameters.Clear;
        FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      end;
      Break;
    end;
  end;
end;

{
procedure TCnADOUpdateSQL.SetParams(UpdateKind: TUpdateKind;
  DeltaDS: TDataSet);
var
  I: Integer;
  Old: Boolean;
  Param: TParam;
  PName: string;
  Field: TField;
  Value: Variant;
begin
  case FConnectionType of
    ctDataSet:  if not Assigned(FDataSet)  then Exit;
    ctProvider: if not Assigned(FProvider) then Exit;
  end;
  
  with Query[UpdateKind] do
  begin
    for I := 0 to Parameters.Count - 1 do
    begin
      Param := TParams(Parameters.Items[I]);
      PName := Param.Name;
      Old := CompareText(Copy(PName, 1, 4), 'OLD_') = 0;
      if Old then System.Delete(PName, 1, 4);
      Field := DeltaDS.FindField(PName);
      if not Assigned(Field) then Continue;
      if Old then
        Param.AssignFieldValue(Field, Field.OldValue)
      else begin
        Value := Field.NewValue;
        if VarIsClear(Value) then Value := Field.OldValue;
        Param.AssignFieldValue(Field, Value);
      end;
    end;
  end;
end;
}

procedure TCnADOUpdateSQL.SetParams(UpdateKind: TUpdateKind;
  DeltaDS: TDataSet);
var
  i: Integer;
  bOld: Boolean;               
  sFieldName: AnsiString;      //字段名（如：CustomerName)
  sFieldNameParam: AnsiString; //字段参数名(如：:CustomerName)
  sSQLText: AnsiString;
  nField: TField;
  nValue: Variant;
begin
  case FConnectionType of
    ctConnection: if not Assigned(FConnection) then Exit;
    ctDataSet: if not Assigned(FDataSet) then Exit;
    ctProvider: if not Assigned(FProvider) then Exit;
  end;

  with GetQuery(UpdateKind) do
  begin
    SQL.Text := FSQLText[UpdateKind].Text;
    sSQLText := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(SQL.Text);
    for i := 0 to Parameters.Count - 1 do
    begin
      sFieldName := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(Parameters.Items[i].Name);
      bOld := CompareText({$IFDEF DELPHI12_UP}String{$ENDIF}(Copy(sFieldName, 1, 4)), 'OLD_') = 0;
      if bOld then System.Delete(sFieldName, 1, 4);

      nField := DeltaDS.FindField({$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldName));
      if not Assigned(nField) then Continue;

      if bOld then //字段参数名中找到OLD_，不更新该字段，即取原值OldValue
      begin
        sFieldNameParam := ':OLD_' + sFieldName;
        nValue := nField.OldValue;
      end
      else begin  //字段参数名中找不到OLD_，更新该字段，即取新值NewValue
        sFieldNameParam := ':' + sFieldName;
        nValue := nField.NewValue;
        if VarIsEmpty(nValue) then nValue := nField.OldValue;
      end;

      //数据类型判断，处理后赋值（取字段参数名，替换字段名为相应的Value值）
      if not(Parameters.Items[i].DataType in [ftUnknown, ftAutoInc, ftParadoxOle..ftDBaseOle, ftADT..ftIDispatch]) then
      begin
        case Parameters.Items[i].DataType of
          //布尔型
          ftBoolean:
          begin
            if not VarIsNull(nValue) then
              nValue := nValue <> 0
            else
              sSQLText :=
                {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), 'NULL', [rfReplaceAll]));
          end;

          //字符型
          ftString, ftWideString:
          begin
            if not VarIsNull(nValue) then
              sSQLText :=
                {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), QuotedStr(VarToStr(nValue)), [rfReplaceAll]))
            else
              sSQLText :=
                {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), 'NULL', [rfReplaceAll]));
          end;

          //数字型
          ftSmallint, ftInteger, ftBytes, ftLargeint, ftBCD, ftFloat, ftCurrency:
          begin
            if not VarIsNull(nValue) then
              sSQLText := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), VarToStr(nValue), [rfReplaceAll]))
            else
              sSQLText := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), 'NULL', [rfReplaceAll]));
          end;

          //日期型
          ftDate, ftTime, ftDateTime{$IFDEF COMPILER6_UP},  ftTimeStamp {$ENDIF}:
          begin
            if not VarIsNull(nValue) then
              sSQLText := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), QuotedStr(VarToStr(nValue)), [rfReplaceAll]))
            else
              sSQLText := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), 'NULL', [rfReplaceAll]));
          end;

          else begin
            if not VarIsNull(nValue) then
              sSQLText := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), VarToStr(nValue), [rfReplaceAll]))
            else
              sSQLText := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(
                StringReplace({$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText),
                {$IFDEF DELPHI12_UP}String{$ENDIF}(sFieldNameParam), 'NULL', [rfReplaceAll]));
          end;
        end;
      end;
    end;

    SQL.Text := {$IFDEF DELPHI12_UP}String{$ENDIF}(sSQLText);
  end;
end;

procedure TCnADOUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  inherited;
  SetParams(UpdateKind, FDataSet);
  ExecSQL(UpdateKind);
end;

procedure TCnADOUpdateSQL.UpdateRecord(Sender: TObject; SourceDS: TDataSet;
  DeltaDS: {$IFDEF COMPILER6_UP} TCustomClientDataSet {$ELSE} TClientDataSet {$ENDIF}; UpdateKind: TUpdateKind;
  var Applied: Boolean);
begin
  SetParams(UpdateKind, DeltaDS);
  ExecSQL(UpdateKind);
  Applied := True;
end;

{$ENDIF SUPPORT_ADO}

end.
