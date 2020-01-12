{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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

unit CnDHibernateBatchSQL; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：批量执行SQL控件单元
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
  Classes, SysUtils, DB, ADODB;

type
  TCnOnException = procedure(Sender: TObject; E: Exception) of object;

  TCnOnFinishOne = procedure(Sender: TObject; FinishedSQL: string) of object;

  TCnDHibernateBatchSQL = class(TComponent)
  private
    FConnection: TADOConnection;
    FBatchSQL: TStringList;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FOnException: TCnOnException;
    FOnFinishOne: TCnOnFinishOne;
    FAbout: string;
    function GetBatchSQL: TStrings;
    procedure SetBatchSQL(const Value: TStrings);
  protected
    FSQLList: TStringList;
    procedure ExtractBatchSQLToSQLList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property About: string read FAbout write FAbout;
    property Connection: TADOConnection read FConnection write FConnection;
    property BatchSQL: TStrings read GetBatchSQL write SetBatchSQL;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
    property OnException: TCnOnException read FOnException write FOnException;
    property OnFinishOne: TCnOnFinishOne read FOnFinishOne write FOnFinishOne;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateBatchSQL }

constructor TCnDHibernateBatchSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FBatchSQL := TStringList.Create;
  FSQLList := TStringList.Create;
end;

destructor TCnDHibernateBatchSQL.Destroy;
begin
  FConnection := nil;
  FBatchSQL.Free;
  FSQLList.Free;
  inherited Destroy;
end;

procedure TCnDHibernateBatchSQL.Execute;
var
  i: Integer;
  hql: string;
begin
  if FConnection = nil then
    raise Exception.Create('No ADOConnection found!');
  if not FConnection.Connected then
    FConnection.Open();
  if Assigned(BeforeExecute) then
    BeforeExecute(Self);
  ExtractBatchSQLToSQLList; 
  //
  with TADOQuery.Create(nil) do
  begin
    // todo: exceute the sql
    for i := 0 to FSQLList.Count - 1 do
    begin
      hql := FSQLList[i]; 
      // check whether select, insert, update, delete included.
      if Pos('select', hql) > 0 then
      begin
        // do select
        Close;
        SQL.Text := hql;
        try
          Open;
          if Assigned(OnFinishOne) then
            OnFinishOne(self, hql);
        except
          on E: Exception do
            if Assigned(OnException) then
              OnException(Self, E);
        end;
      end
      else if (Pos('insert', hql) > 0) or (pos('update', hql) > 0) or (Pos('delete', hql) > 0) then
      begin
          // do insert, update, delete
        Close;
        sql.Text := hql;
        try
          ExecSQL;
          if Assigned(OnFinishOne) then
            OnFinishOne(self, hql);
        except
          on E: Exception do
            if Assigned(OnException) then
              OnException(Self, E);
        end;
      end
      else
      begin
          // not matched! exception throws.
        if Assigned(OnException) then
          OnException(self, exception.Create('SQL not contains select, insert, update or delete.'));
      end;
    end;
    close;
    Free;
  end;
  if Assigned(AfterExecute) then
    AfterExecute(Self);
end;

procedure TCnDHibernateBatchSQL.ExtractBatchSQLToSQLList;
var
  str: string;
begin
  FSQLList.Clear;
  str := FBatchSQL.Text; 
  // remove the CR_LF
  str := StringReplace(str, #13#10, EmptyStr, [rfReplaceAll, rfIgnoreCase]); 
  // split the string to list
  FSQLList.Delimiter := '|';
  FSQLList.DelimitedText := str;
end;

function TCnDHibernateBatchSQL.GetBatchSQL: TStrings;
begin
  Result := FBatchSQL;
end;

procedure TCnDHibernateBatchSQL.SetBatchSQL(const Value: TStrings);
begin
  FBatchSQL.Assign(Value);
end; 

{$ENDIF SUPPORT_ADO}
end.
