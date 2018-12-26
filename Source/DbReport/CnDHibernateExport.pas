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

unit CnDHibernateExport; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：数据导出控件单元
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
  Windows, Messages, SysUtils, Classes, DB, ADODB, ComObj, CnDHibernateBase,
  Variants;

type
  TCnOnExport = procedure of object;

  TCnDHibernateExport = class(tcomponent)
  private
    FSQL: string;
    FConnection: TAdoConnection;
    FExportColumn: boolean;
    FFileName: string;
    FSheetName: string;
    FAdoQuery: TADOQuery;
    FOnExport: TCnOnExport;
    FAbout: string; 
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Export: Integer;
  published
    property About: string read FAbout write FAbout;
    property Connection: TAdoConnection read FConnection write FConnection;
    property SQL: string read FSQL write FSQL; 
    { whether export column name or not }
    property ExportColumn: boolean read FExportColumn write FExportColumn default True;
    property FileName: string read FFileName write FFileName;
    property SheetName: string read FSheetName write FSheetName;
    property OnExport: TCnOnExport read FOnExport write FOnExport;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateExport }

constructor TCnDHibernateExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FExportColumn := True;
end;

destructor TCnDHibernateExport.Destroy;
begin
  FConnection := nil;
  inherited Destroy;
end;

function TCnDHibernateExport.Export: Integer;
var
  i: Integer;
  excel: OleVariant;
  currentRow: Integer;
begin
  Result := 0; 
  // check event
  if FFileName = EmptyStr then
    raise TCnNoFileException.Create('file not found!');
  if FSheetName = EmptyStr then
    raise TCnNoSheetNameException.Create('sheet not found!');
  if FConnection = nil then
    raise TCnNoConnectionException.Create('data connection not found!');
  if FSQL = EmptyStr then
    raise TCnNoSQLException.Create('T-SQL not found!');
  try
    excel := CreateOleObject('excel.application');
    excel.WorkBooks.Open(fFileName);
  except
    raise TCnNoExcelException.Create('Excel not installed!');
    Exit;
  end;
  excel.WorkSheets[FSheetName].Activate; 
  
  // create ds
  FAdoQuery := TADOQuery.Create(nil);
  FAdoQuery.Connection := FConnection;
  FAdoQuery.SQL.Text := FSQL;
  FAdoQuery.Open;
  currentRow := 1;
  if FExportColumn then
  begin
    // export column name
    for i := 0 to FAdoQuery.FieldCount - 1 do
    begin
      excel.Cells[currentRow, i + 1].Value := FAdoQuery.Fields[i].FieldName;
    end;
    Inc(currentRow);
  end; 
  // export data
  FAdoQuery.First;
  while not FAdoQuery.Eof do
  begin
    for i := 0 to FAdoQuery.FieldCount - 1 do
    begin
      excel.cells[currentRow, i + 1].value := FAdoQuery.Fields[i].Value;
    end;
    Inc(Result);
    if Assigned(OnExport) then
      OnExport;
    Inc(currentRow);
    FAdoQuery.Next;
  end;
  FAdoQuery.Close;
  FAdoQuery.Free; 
  // excel.WorkBooks.Close(SaveChanges:=True);
  excel.save;
  excel.quit;
  excel := Unassigned;
end; 

{$ENDIF SUPPORT_ADO}
end.
