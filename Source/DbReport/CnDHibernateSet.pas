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

unit CnDHibernateSet; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准类库
* 单元名称：Set类单元
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

{$IFDEF SUPPORT_ADO}

uses
  Classes, SysUtils, CnDHibernatePodoList;

type
  ICnSet = interface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function add(item: TObject): Boolean; stdcall;
    function addAll(items: TCnPodoList): Boolean; stdcall;
    function remove(item: TObject): Boolean; stdcall;
    function removeAll(items: TCnPodoList): Boolean; stdcall;
    procedure clear; stdcall;
    function size: Integer; stdcall;
    function toArray: TCnPodoList; stdcall;
  end;

  TCnHashSet = class(TObject, ICnSet)
  private
    FObjectList: array of TObject;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function add(item: TObject): Boolean; stdcall;
    function addAll(items: TCnPodoList): Boolean; stdcall;
    function remove(item: TObject): Boolean; stdcall;
    function removeAll(items: TCnPodoList): Boolean; stdcall;
    procedure clear; stdcall;
    function size: Integer; stdcall;
    function toArray: TCnPodoList; stdcall;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnHashSet }

function TCnHashSet._AddRef: Integer;
begin
  Result := -1;
end;

function TCnHashSet._Release: Integer;
begin
  Result := -1;
end;

function TCnHashSet.add(item: TObject): Boolean;
var
  i: Integer;
  len: Integer;
begin
  len := Length(FObjectList);
  for i := 0 to len - 1 do
  begin
    if FObjectList[i] = item then
    begin
      // already exists
      Result := False;
      Exit;
    end;
  end; 
  // add
  SetLength(FObjectList, len + 1);
  FObjectList[len] := item;
  Result := True;
end;

function TCnHashSet.addAll(items: TCnPodoList): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to items.Count - 1 do
  begin
    if not add(items.Objects[i]) then
      Result := False;
  end;
end;

procedure TCnHashSet.clear;
begin
  SetLength(FObjectList, 0);
end;

function TCnHashSet.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCnHashSet.remove(item: TObject): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to Length(FObjectList) - 1 do
  begin
    if FObjectList[i] = item then
    begin
      for j := i to Length(FObjectList) - 2 do
        FObjectList[j] := FObjectList[j + 1];
      SetLength(FObjectList, Length(FObjectList) - 1);
      Result := True;
      Break;
    end;
  end;
end;

function TCnHashSet.removeAll(items: TCnPodoList): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to items.Count - 1 do
  begin
    if not remove(items.Objects[i]) then
      Result := False;
  end;
end;

function TCnHashSet.size: Integer;
begin
  Result := Length(FObjectList);
end;

function TCnHashSet.toArray: TCnPodoList;
var
  i: Integer;
begin
  Result := TCnPodoList.Create;
  for i := 0 to Length(FObjectList) - 1 do
    Result.Add(FObjectList[i]);
end;

constructor TCnHashSet.Create;
begin
  SetLength(FObjectList, 0);
end;

destructor TCnHashSet.Destroy;
begin
  SetLength(FObjectList, 0);
  inherited Destroy;
end; 

{$ENDIF SUPPORT_ADO}
end.
