{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnRunSqlUnit;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：查询分析器组件查询运行线程单元
* 单元作者：不得闲 (appleak46@yahoo.com.cn)
* 备    注：
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.11.24 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses 
  Windows, Messages, Classes, SysUtils, ADODb, DB, OleDb, ActiveX,
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF}
  CnDBConsts;

const
  CN_MSG_EXECUTING = WM_USER + 100;
  CN_MSG_EXEFAIL = WM_USER + 101;
  CN_MSG_CLOSEQUERYTOOL = WM_USER + 102;
  
type
  TRunThread = class(TThread)
  private
    FConnection: TADOConnection;
    FIsStop: Boolean;
    FIsParse: Boolean;
    FMsgHandle: THandle;
    FSqlList: TList;
    FMsgList: TList;
    FSql: string;
    FDBProvider: string;
    FOldTime: Integer;
    FNewTime: Integer;
    FRunSucced: Boolean;
    FUseTime: Integer;
    FRecordList: TList;
    procedure SetConnection(const Value: TADOConnection);
    procedure SetIsStop(const Value: Boolean);
    procedure SetIsParse(const Value: Boolean);
    procedure SetSql(const Value: string);
    Function ExecuteSql(sql: string): Boolean;
  protected
    procedure Execute;override;
  public
    property DBProvider: string read FDBProvider write FDBProvider;
    Constructor Create(CreateSuspended: Boolean;MsgHandle: THandle);
    Destructor Destroy;override;
    property RunSucced: Boolean read FRunSucced;
    property Connection: TADOConnection read FConnection write SetConnection;
    property Sql: string read FSql write SetSql;
    property IsStop: Boolean read FIsStop write SetIsStop;
    property IsParse: Boolean read FIsParse write SetIsParse;
    property RecordList: TList read FRecordList;
    property MsgList: TList read FMsgList;
  end;

procedure GetSqlStrings(List: TList; SqlText: string);

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

procedure GetSqlStrings(List: TList; SqlText: string);
var
  Strings: TStringList;
  i: Integer;
  tempstr: string;
  SqlRec: PChar;
begin
  Strings := TStringList.Create;
  Strings.Text := SqlText;
  for i := 0 to Strings.Count - 1 do
  begin
    if uppercase(trim(Strings[i])) <> 'GO' then
      tempstr := tempstr + strings[i]+#13#10
    else
    begin
      sqlRec := StrNew(PChar(tempstr));
      tempstr := '';
      List.Add(SqlRec);
    end;
  end;
  sqlRec := StrNew(PChar(tempstr));
  List.Add(SqlRec);
  FreeAndNil(Strings);
end;

{ TRunThread }

constructor TRunThread.Create(CreateSuspended: Boolean;MsgHandle: THandle);
begin
  inherited Create(CreateSuspended);
  FMsgHandle := MsgHandle;
  FSQlList := TList.Create;
  FMsgList := TList.Create;
  FRecordList := TList.Create;
end;

destructor TRunThread.Destroy;
var
  i: Integer;
  tempstr: PChar;
begin
  FreeAndNil(FRecordList);
  for i := 0 to FMsgList.Count - 1 do
  begin
    tempstr := FMsgList.Items[i];
    StrDispose(tempstr);
  end;
  for i := 0 to FSqlList.Count - 1 do
  begin
    tempstr := FSqlList.Items[i];
    strDispose(tempstr);
  end;
  FreeAndNil(FMsgList);
  FreeAndNil(FSqlList);
  inherited;
end;

procedure TRunThread.Execute;
var
  i: Integer;
  CurrSql: PChar;
  ExeFlag: Boolean;
  msg: string;
  Fold, FNew: Integer;
begin
  inherited;
  FRunSucced := False;
  FUseTime := 0;
  FOld := GetTickCount;
  if FSqlList.Count > 0 then
  begin
    ExeFlag := True;
    SendMessage(FMsgHandle, CN_MSG_EXECUTING, 0, 0);//执行开始
    CoInitialize(Nil);
    try
      FConnection.Open;
    except
      on E: Exception do
      begin
        if not FIsStop then
        begin
        //MessageBox(FMsgHandle,PChar(E.Message),ErrMsg,16);
          msg := E.Message;
          FMsgList.Add(strNew(PChar(msg)));
          ExeFlag := false;
        end;
      end;
    end;

    CoUninitialize;
    FNew := GetTickCount;
    FUseTime := FNew - FOld;
    if ExeFlag then
      for i := 0 to FSqlList.Count - 1 do
      begin
        CurrSql := FSqlList.Items[i];
        if not ExecuteSql(CurrSql) then
        begin
          ExeFlag := false;
          Break;
        end;
      end;

    msg := SCnUsedTime + #13#10#13#10 + inttostr(FUseTime);
    FMsgList.Add(strNew(PChar(msg)));
    if not ExeFlag then
      SendMessage(FMsgHandle,CN_MSG_EXEFAIL,Integer(self),0)
    else
      FRunSucced := true;
  end;
end;

function TRunThread.ExecuteSql(sql: string): Boolean;
var
  ExeRecord: _RecordSet;
  msg: string;
  RecordAffected: Integer;
  Affected: OleVariant;
  FCommand: TADOCommand;
  Query: TADOQuery;
  tempCount: Integer;
  Ds: TDataSource;
  Flag: Boolean;
begin
  if DBProvider <> 'SQLOLEDB.1' then
  begin
    Result := False;
    FMsgList.Add(strNew(PChar(Format(SCnVerWar, [DBProvider]))));
    Exit;
  end;
  {if FIsStop then
  begin
    Result := false;
    exit;
  end;}
  Result := true;
  CoInitialize(Nil);
  tempcount := -1;
  FCommand := nil;
  ExeRecord := nil;
  try
    if FIsParse then
      FConnection.Execute('SET PARSEONLY ON')
    else
      FConnection.Execute('SET PARSEONLY OFF');
    FCommand := TADOCommand.Create(nil);
    FCommand.Connection := FConnection;
    FCommand.ParamCheck := false;
    FCommand.CommandText := sql;
    FConnection.BeginTrans;//开始事务
    try
      FOldTime := GetTickCount;
      ExeRecord := FCommand.Execute(RecordAffected,EmptyParam);
      FConnection.CommitTrans;//提交事务
      if FIsStop then
      begin
        Result := false;
        FConnection.RollbackTrans;
        exit;
      end;
      FNewTime := GetTickCount;
      FUseTime := FUseTime + (FNewTime - FOldTime);
      Affected := RecordAffected;
      if not FIsParse then
      begin
        if Affected <> -1 then
        begin
          msg := Format(SCnAffectMsg, [Integer(Affected)])+ #13#10#13#10;
          FMsgList.Add(strNew(PChar(msg)));
        end
        else
        begin
          try
            TempCount := ExeRecord.RecordCount;
          except
            tempcount := -1;
          end;
          if TempCount = -1 then
          begin
            msg := SCnExeSucced+#13#10#13#10;
            FMsgList.Add(strNew(PChar(msg)));
          end;
        end;
        Flag := True;

        while (ExeRecord <> nil) do
        begin
          if Affected = -1 then
          begin
            if tempCount <> -1 then
            begin
              msg := Format(SCnAffectMsg,[ExeRecord.RecordCount])+ #13#10#13#10;
              FMsgList.Add(strNew(PChar(msg)));
              Query := TADOquery.Create(nil);
              Query.Connection := FConnection;
              Query.Recordset := ExeRecord;
              Ds := TDataSource.Create(nil);
              Ds.DataSet := Query;
              FRecordList.Add(Ds);
            end
            else if not Flag then
            begin
              msg := SCnExeSucced + #13#10#13#10;
              FMsgList.Add(strNew(PChar(msg)));
            end;
          end;
          ExeRecord := ExeRecord.NextRecordset(Affected);
          Flag := False;
          if ExeRecord <> nil then
          try
            TempCount := ExeRecord.RecordCount;
          except
            Tempcount := -1;
          end;
        end;
      end
      else
      begin
        msg := SCnExeSucced + #13#10#13#10;
        FMsgList.Add(StrNew(PChar(msg)));
      end;
    except
      on E: Exception do
      begin
        FNewTime := GetTickCount;
        FUseTime := FUseTime + (FNewTime - FOldTime);
        msg := E.Message + #13#10#13#10;
        FMsgList.Add(strNew(PChar(msg)));
        FConnection.RollbackTrans;
        Result := False;
      end;
    end;
    FreeAndNil(Fcommand);
    ExeRecord := nil;
  finally
    CoUninitialize;
  end;
end;

procedure TRunThread.SetConnection(const Value: TADOConnection);
begin
  if FConnection <> Value then
  begin
    FConnection := Value;
  end;
end;

procedure TRunThread.SetIsParse(const Value: Boolean);
begin
  FIsParse := Value;
end;

procedure TRunThread.SetIsStop(const Value: Boolean);
begin
  FIsStop := Value;
  //if FConnection.Connected then
  //  FConnection.RollbackTrans;// 回滚事务
end;

procedure TRunThread.SetSql(const Value: string);
var
  i: Integer;
  pstr: PChar;
begin
  if FSql <> Value then
  begin
    FSql := Value;
    for i := 0 to FSqlList.Count - 1 do
    begin
      pstr := FSqlList.Items[i];
      strDispose(pstr);
    end;
    FSqlList.Clear;
    GetSqlStrings(FSqlList,Value);
  end;
end;

{$ENDIF SUPPORT_ADO}
end.
