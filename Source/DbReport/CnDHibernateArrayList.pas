{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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

unit CnDHibernateArrayList; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准类库
* 单元名称：数组列表类单元
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

const
  // 异常字符串
  OUT_OF_ARRAYLIST_INDEX_EXCEPTION_STRING = '超出对象列表下标';

type

  // 异常声明
  // 此异常在判断 index 时使用
  // 如果 index 范围超出，则抛出此异常
  TCnOutOfArrayListIndexException = Exception;

  ICnList = interface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure trimToSize; stdcall;
    procedure ensureCapacity(minCapacity: integer); stdcall;
    function size: Integer; stdcall;
    function isEmpty: Boolean; stdcall;
    function contains(elem: TObject): Boolean; stdcall;
    function indexOf(elem: TObject): integer; stdcall;
    function lastIndexOf(elem: TObject): integer; stdcall;
    // function clone: TObject; stdcall;
    function toArray: TCnPodoList; overload; stdcall;
    // function toArray(a: TPodoList): TPodoList; overload; stdcall;
    function _get(index: integer): TObject; stdcall;
    function _set(index: integer; element: TObject): TObject; stdcall;
    procedure add(o: TObject); overload; stdcall;
    procedure add(index: integer; element: TObject); overload; stdcall;
    function remove(index: integer): TObject; overload; stdcall;
    function remove(o: TObject): Boolean; overload; stdcall;
    procedure fastRemove(index: integer); stdcall;
    procedure clear; stdcall;
    procedure addAll(c: TCollection); overload; stdcall;
    procedure addAll(index: integer; c: TCollection); overload; stdcall;
  end;

  TCnArrayList = class(TObject, ICnList)
  private
    FSize: integer;
    elementData: TCnPodoList;
    procedure fastRemove(index: integer); stdcall;
  protected
    procedure RangeCheck(index: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure trimToSize; stdcall;
    procedure ensureCapacity(minCapacity: integer); stdcall;
    function size: Integer; stdcall;
    function isEmpty: Boolean; stdcall;
    function contains(elem: TObject): Boolean; stdcall;
    function indexOf(elem: TObject): integer; stdcall;
    function lastIndexOf(elem: TObject): integer; stdcall;
    // function clone: TObject; stdcall;
    function toArray: TCnPodoList; stdcall;
    function _get(index: integer): TObject; stdcall;
    function _set(index: integer; element: TObject): TObject; stdcall;
    procedure add(o: TObject); overload; stdcall;
    procedure add(index: integer; element: TObject); overload; stdcall;
    function remove(index: integer): TObject; overload; stdcall;
    function remove(o: TObject): Boolean; overload; stdcall;
    procedure clear; stdcall;
    procedure addAll(c: TCollection); overload; stdcall;
    procedure addAll(index: integer; c: TCollection); overload; stdcall;
    //procedure TCnArrayList.removeRange(fromIndex:integer;toIndex:integer);
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

constructor TCnArrayList.Create;
begin

end;

destructor TCnArrayList.Destroy;
begin
  inherited Destroy;
end;

function TCnArrayList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCnArrayList._AddRef: Integer;
begin
  Result := -1;
end;

function TCnArrayList._Release: Integer;
begin
  Result := -1;
end; 

(******************************************************************************)
/// .registed
/// <summary>
///  修正元素数量
/// </summary>
procedure TCnArrayList.trimToSize;
var
  oldCapacity: integer; 
  // oldData           : TPodoList;
  i: integer;
begin
  // modCount := modCount + 1;
  oldCapacity := elementData.Count;
  if (Fsize < oldCapacity) then
  begin
    if Fsize <> 0 then
    begin
      for i := 0 to(oldCapacity - Fsize - 1) do
        elementData.Remove(oldCapacity - i);
    end
    else
      elementData.Clear;
  end
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// ??
/// </summary>
/// <param name="minCapacity">??</param>
procedure TCnArrayList.ensureCapacity(minCapacity: integer);
var
  oldCapacity: integer;
  newCapacity: integer;
  i: integer;
begin
  // modCount := modCount + 1;
  oldCapacity := elementData.Count;
  if minCapacity > oldCapacity then
  begin
    newCapacity := (oldCapacity * 3) div 2 + 1;
    if newCapacity < minCapacity then
      newCapacity := minCapacity;
    for i := 0 to(newCapacity - oldCapacity - 1) do
    begin
      elementData.Add(nil);
    end;
  end;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 返回list的数量
/// </summary>
/// <returns>数量</returns>
function TCnArrayList.size: Integer;
begin
  result := Fsize;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 是否为空
/// </summary>
/// <returns>boolean</returns>
function TCnArrayList.isEmpty: Boolean;
begin
  result := FSize = 0;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 检查元素是否在list中
/// </summary>
/// <param name="elem">元素</param>
/// <returns>boolean</returns>
function TCnArrayList.contains(elem: TObject): Boolean;
begin
  result := indexOf(elem) > 0;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 返回元素在list的序号,按第一次出显为准
/// </summary>
/// <param name="elem">元素</param>
/// <returns>序号</returns>
function TCnArrayList.indexOf(elem: TObject): integer;
begin
  Result := -1;
  if elem <> nil then
  begin
    result := elementData.IndexOf(elem);
  end;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 返回元素在list的序号,从后往前数
/// </summary>
/// <param name="elem">元素</param>
/// <returns>序号</returns>
function TCnArrayList.lastIndexOf(elem: TObject): integer;
begin
  Result := -1;
  if elem <> nil then
  begin
    result := elementData.lastIndexOf(elem);
  end;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 返回list
/// </summary>
/// <returns>元素列表</returns>
function TCnArrayList.toArray: TCnPodoList;
begin
  result := elementData;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 获取指定的元素
/// </summary>
/// <param name="index">序号</param>
/// <returns>元素</returns>
function TCnArrayList._get(index: integer): TObject;
begin
  RangeCheck(index);
  result := elementData.Objects[index];
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 更改指定元素
/// </summary>
/// <param name="index">序号</param>
/// <param name="element">元素</param>
/// <returns>old元素</returns>
function TCnArrayList._set(index: integer; element: TObject): TObject;
var
  oldValue: TObject;
begin
  RangeCheck(index);
  oldValue := elementData.Objects[index];
  elementData.Objects[index] := element;
  result := oldValue;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 增加元素
/// </summary>
/// <param name="o">元素</param>
/// <returns>old元素</returns>
procedure TCnArrayList.add(o: TObject);
begin
  elementData.Add(o);
  Fsize := Fsize + 1;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 插入元素
/// </summary>
/// <param name="index">序号</param>
/// <param name="element">元素</param>
procedure TCnArrayList.add(index: integer; element: TObject);
begin
  elementData.Insert(index, element);
  Fsize := Fsize + 1;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 删除元素
/// </summary>
/// <param name="index">序号</param>
/// <returns>old元素</returns>
function TCnArrayList.remove(index: integer): TObject;
var
  oldValue: TObject;
begin
  RangeCheck(index);
  oldValue := elementData.Objects[index];
  elementData.Remove(index);
  Fsize := Fsize - 1; 
  // modCount := modCount + 1;
  result := oldValue;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 删除元素
/// </summary>
/// <param name="index">序号</param>
/// <returns>old元素</returns>
function TCnArrayList.remove(o: TObject): boolean;
begin
  result := elementData.Remove(o);
  Fsize := Fsize - 1; 
  // modCount := modCount + 1;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 删除元素,无返回值
/// </summary>
/// <param name="index">序号</param>
procedure TCnArrayList.fastRemove(index: integer);
begin
  elementData.Remove(index);
  Fsize := Fsize - 1; 
  // modCount := modCount + 1;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 清空
/// </summary>
procedure TCnArrayList.clear;
begin
  elementData.Clear;
  Fsize := 0; 
  // modCount := modCount + 1;
end; 

(******************************************************************************)
procedure TCnArrayList.addAll(c: TCollection);
var
  i: integer;
begin
  for i := 0 to c.Count - 1 do
  begin
    elementData.Add(TObject(c.Items[i]));
  end;
end;

procedure TCnArrayList.addAll(index: integer; c: TCollection);
var
  i: integer;
begin
  for i := 0 to c.Count - 1 do
  begin
    elementData.Insert(index, TObject(c.Items[i]));
  end;
end; 

(******************************************************************************)
/// .registed
/// <summary>
/// 检查序号的有效范围
/// </summary>
/// <param name="index">序号</param>
procedure TCnArrayList.RangeCheck(index: integer);
begin
  if (index < 0) or (index > elementData.Count - 1) then
  begin
    raise TCnOutOfArrayListIndexException.Create(OUT_OF_ARRAYLIST_INDEX_EXCEPTION_STRING);
    exit;
  end;
end; 

{$ENDIF SUPPORT_ADO}
end.
