{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDHibernateSubQuery;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准控件库
* 单元名称：主从表子查询控件单元
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
  Windows, Messages, SysUtils, Classes, DB, ADODB, CnDHibernateClasses,
  CnDHibernateBase, CnDHibernateSet, CnDHibernatePodoList, CnDHibernateConsts;

type
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnDHibernateSubQuery = class(TCnDHibernateQuery)
  private
    FMainTableName: string;
    FMainTablePK: string;
    FMainTablePKValue: Variant;
    FSubTableRefField: string;
    FSubTableName: string;
    FSubTablePKName: string;
    FAbout: string;
    procedure SetMainTablePKValue(const Value: Variant);
    procedure SetSubTableRefField(const Value: string);
  protected
    procedure findSubDetail;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function saveDetail(items: ICnSet): Boolean;
    function updateDetail(items: ICnSet): Boolean;
    function deleteDetail(items: ICnSet): Boolean;
    function saveOrUpdateDetail(items: ICnSet): Boolean;
  published
    property About: string read FAbout write FAbout;
    property MainTableName: string read FMainTableName write FMainTableName;
    property MainTablePK: string read FMainTablePK write FMainTablePK;
    property MainTablePKValue: Variant read FMainTablePKValue write
      SetMainTablePKValue;
    property SubTableRefField: string read FSubTableRefField write
      SetSubTableRefField;
    property SubTableName: string read FSubTableName write FSubTableName;
    property SubTablePKName: string read FSubTablePKName write FSubTablePKName;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateSubQuery }

constructor TCnDHibernateSubQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

function TCnDHibernateSubQuery.deleteDetail(items: ICnSet): Boolean;
var
  i                 : Integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not deleteData(FSubTableName, podos.Objects[i]) then
      Result := False;
  end;
end;

destructor TCnDHibernateSubQuery.Destroy;
begin

  inherited Destroy;
end;

procedure TCnDHibernateSubQuery.findSubDetail;
var
  hql               : TCnStringBuffer;
  param             : ICnMap;
begin
  if (FMainTableName = EmptyStr) or (FMainTablePK = EmptyStr) or
    (FMainTablePKValue = EmptyStr) or (FSubTableRefField = EmptyStr)
    or (FSubTableName = EmptyStr) then
    Exit;
  hql := TCnStringBuffer.Create(Format(DH_GET_RECORD, [FSubTableName,
    FSubTableRefField, FSubTableRefField]));
  param := TCnDHHashMap.Create;
  param.put(FSubTableRefField, FMainTablePKValue);
  Self.find(hql.toString, param);
end;

function TCnDHibernateSubQuery.saveDetail(items: ICnSet): Boolean;
var
  i                 : Integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not saveData(FSubTableName, podos.Objects[i]) then
      Result := False;
  end;
end;

function TCnDHibernateSubQuery.saveOrUpdateDetail(items: ICnSet): Boolean;
var
  i                 : Integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not saveOrUpdateData(FSubTableName, podos.Objects[i], FSubTablePKName)
      then
      Result := False;
  end;
end;

procedure TCnDHibernateSubQuery.SetMainTablePKValue(const Value: Variant);
begin
  // check whether main table has name and primary key
  if (FMainTableName = EmptyStr) or (FMainTablePK = EmptyStr) then
  begin
    FMainTablePKValue := EmptyStr;
    Exit;
  end;
  FMainTablePKValue := Value;
  if FSubTableRefField <> EmptyStr then
    findSubDetail;
end;

procedure TCnDHibernateSubQuery.SetSubTableRefField(const Value: string);
begin
  FSubTableRefField := Value;
  if (FMainTableName <> EmptyStr) and (FMainTablePK <> EmptyStr) and
    (FMainTablePKValue <> EmptyStr) then
    findSubDetail;
end;

function TCnDHibernateSubQuery.updateDetail(items: ICnSet): Boolean;
var
  i                 : integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not updateData(FSubTableName, podos.Objects[i], FSubTablePKName) then
      Result := False;
  end;
end;

{$ENDIF SUPPORT_ADO}
end.
