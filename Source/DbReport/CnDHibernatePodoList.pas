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

unit CnDHibernatePodoList;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate标准类库
* 单元名称：PODO 类/基类单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
*
*      使用示例：
*
*      objList := TRaObjList.Create;
*      obj := 'obj';
*      obj.add('id', TObject(obj));
*      showMessage(string(obj.getObjectByName('id')));
*
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
  Classes, SysUtils;

const
  // 异常字符串
  OUT_OF_OBJECT_INDEX_EXCEPTION_STRING = '超出对象列表下标';
  NAME_NOT_FOUND_EXCEPTION_STRING = '对象 id 不存在';

  (******************************************************************************)

type
  // 异常声明
  // 此异常在判断 index 时使用
  // 如果 index 范围超出，则抛出此异常
  TCnOutOfObjectIndexException = Exception;

  // 此异常在判断对象 id 时使用
  // 如果对象没有 id，则抛出此异常
  TCnNameNotFoundException = Exception;

  (******************************************************************************)

    // 对象记录声明
    // 此记录是 hash 表的一个结点
    // 允许通过 名称 查找 对象
    // 名称可以不唯一
    // 但是跟据名称查找对象时
    // 如果名称有重复，那么只会取出第一个匹配的
    // 如果名称为空，只能通过 index 取出对象
  TCnObjectRec = record
    // 名称
    id: string;
    // 对象
    obj: TObject;
  end;                                   { TCnObjectRec }

  (******************************************************************************)

    // 对象列表类声明
    // 该对象用于保存对象列表
    // 并且可以取出对象

  TCnPodoList = class
  private

    // 对象列表
    // 此列表使用 TCnObjectRec 记录对象
    // 形成 hash 表的结构
    // 实际操作时，使用的是名值对
    objList: array of TCnObjectRec;

    // 获取指定序号的对象
    // 绑定到 Objects 属性
    function GetObject(index: integer): TObject;
    procedure SetObject(index: integer; obj: TObject);
    // 获取 id 列表
    // 绑定到 IdList 属性
    function getIdList: TStringList;
    function GetCount: integer;
  protected
  public
    // 清空列表
    procedure Clear;
    // 添加对象
    procedure Add(obj: TObject); overload;
    // 添加对象，带有 id
    procedure Add(Id: string; obj: TObject); overload;
    // 在 index 处插入对象
    procedure Insert(index: Integer; obj: TObject); overload;
    // 在 index 处插入对象，带有 id
    procedure Insert(index: Integer; id: string; obj: TObject); overload;
    // 移除 index 处的对象
    procedure Remove(index: Integer); overload;
    // 移除 obj 的 对象
    function Remove(obj: TObject): boolean; overload;
    // 获取第一个对象的序号
    function IndexOf(obj: TObject): integer;
    // 获取最后对象的序号
    function LastIndexOf(obj: TObject): integer;
    // 跟据名称获取对象
    function getObjectByName(ObjName: string): TObject;
    // 移动
    procedure move(oldIndex, newIndex: integer);
  public
    // 对象属性
    property Objects[index: integer]: TObject read GetObject write SetObject;
    // 名称列表
    property IdList: TStringList read getIdList;
    // 对象数
    property Count: integer read GetCount;
  end;                                   

  (******************************************************************************)

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnPodoList }

/// <summary>
/// 添加对象到列表
/// </summary>
/// <param name="obj">对象</param>
procedure TCnPodoList.Add(obj: TObject);
var
  id: string;
begin
  // 置 id 为空
  id := EmptyStr;
  // 调用带 2 个参数的添加方法
  Add(id, obj);
end;                                     { Add }

/// <summary>
/// 添加对象到列表，带有 id
/// </summary>
/// <param name="Id">标识</param>
/// <param name="obj">对象</param>

procedure TCnPodoList.Add(Id: string; obj: TObject);
var
  Len: Integer;
begin
  // 获取列表长度
  Len := Length(objList);
  // 设置长度 + 1
  SetLength(objList, Len + 1);
  // 为 hash 表赋值
  objList[Len].id := Id;
  objList[Len].obj := obj;
end;                                     { Add }

(******************************************************************************)

/// <summary>
/// 清空对象列表
/// </summary>

procedure TCnPodoList.Clear;
begin
  // 将所有的对象置空
  // 内部数组置零
  SetLength(objList, 0);
end;                                     { Clear }

function TCnPodoList.GetCount: integer;
begin
  Result := Length(objList);
end;                                     { GetCount }

(******************************************************************************)

/// <summary>
/// 获取 id 列表
/// </summary>
/// <returns>列表</returns>

function TCnPodoList.getIdList: TStringList;
var
  i: Integer;
begin
  // 创建 id 列表实例
  Result := TStringList.Create;
  // 将对象列表中的对象 id 依次添加到 id 列表中
  for i := 0 to Length(objList) - 1 do
    Result.Add(objlist[i].id);
end;                                     { getIdList }

(******************************************************************************)

/// <summary>
/// 根据名称获取对象
/// </summary>
/// <param name="ObjName">对象名称</param>
/// <returns>对象</returns>

function TCnPodoList.getObjectByName(ObjName: string): TObject;
var
  i: Integer;
begin
  // 判断名称是否存在
  if ObjName = EmptyStr then
  begin
    // 如果不存在则抛出异常
    raise TCnNameNotFoundException.Create(NAME_NOT_FOUND_EXCEPTION_STRING);
    Exit;
  end;                                   { if }

  // 初始化返回对象
  Result := nil;
  for i := 0 to Length(objList) - 1 do
  begin
    // 如果对象的 id 与传入的名称相同
    if objList[i].id = ObjName then
    begin
      // 返回此对象并结束循环
      Result := objlist[i].obj;
      Break;
    end;                                 { if }
  end;                                   { for }
end;                                     { getObjectByName }

(******************************************************************************)

/// <summary>
/// 获取指定序号的对象
/// </summary>
/// <param name="index">序号</param>
/// <returns>对象</returns>

function TCnPodoList.GetObject(index: integer): TObject;
begin
  // 判断序号是否超出下标
  if (index < 0) or (index > Length(objList)) then
  begin
    // 若超出，则抛出异常
    raise TCnOutOfObjectIndexException.Create(OUT_OF_OBJECT_INDEX_EXCEPTION_STRING);
    Exit;
  end;                                   { if }
  // 返回对象
  Result := objlist[index].obj;
end;
                                  { GetObject }
(******************************************************************************)

/// <summary>
/// 更改指定序号的对象
/// </summary>
/// <param name="index">序号</param>
/// <param name="obj">对象</param>
// 新增 SetObject by 天晓 1.6 版

procedure TCnPodoList.SetObject(index: integer; obj: TObject);
begin
  // 判断序号是否超出下标
  if (index < 0) or (index > Length(objList)) then
  begin
    // 若超出，则抛出异常
    raise TCnOutOfObjectIndexException.Create(OUT_OF_OBJECT_INDEX_EXCEPTION_STRING);
    Exit;
  end;                                   { if }
  // 返回对象
  objlist[index].obj := obj;
end;
                                  { GetObject }
(******************************************************************************)

/// <summary>
/// 获取指定的对象序号
/// </summary>
/// <param name="obj">对象</param>
/// <returns>序号</returns>

function TCnPodoList.IndexOf(obj: TObject): integer;
var
  i: Integer;
begin
  // 初始化返回值为 -1
  // 此返回值表示未找到相关的对象
  Result := -1;
  for i := 0 to Length(objList) - 1 do
  begin
    // 如果对象列表中有此对象
    if objList[i].obj = obj then
    begin
      // 返回对象的序号并结束循环
      Result := i;
      Break;
    end;                                 { if }
  end;                                   { for }
end;                                     { IndexOf }


(******************************************************************************)

/// <summary>
/// 获取最后一个对象序号
/// </summary>
/// <param name="obj">对象</param>
/// <returns>序号</returns>
// 新增 LastIndexOf  by 天晓 1.6 版

function TCnPodoList.LastIndexOf(obj: TObject): integer;
var
  i: Integer;
begin
  // 初始化返回值为 -1
  // 此返回值表示未找到相关的对象
  Result := -1;
  for i := Length(objList) - 1 downto 0 do
  begin
    // 如果对象列表中有此对象
    if objList[i].obj = obj then
    begin
      // 返回对象的序号并结束循环
      Result := i;
      Break;
    end;                                 { if }
  end;                                   { for }
end;

(******************************************************************************)

/// <summary>
/// 在 index 处插入对象，带有 id
/// </summary>
/// <param name="index">序号</param>
/// <param name="id">标识</param>
/// <param name="obj">对象</param>
procedure TCnPodoList.Insert(index: Integer; id: string; obj: TObject);
var
  i: Integer;
  len: Integer;
begin
  // 如果序号小于零，则在零处插入
  if index < 0 then
    index := 0;

  // 如果序号大于列表长度
  // 则在列表最后添加
  if index > Length(objList) - 1 then
  begin
    Add(id, obj);
    Exit;
  end;                                   { if }

  // 序列长度 + 1
  len := Length(objList);
  SetLength(objList, len + 1);

  // 依次向后顺移
  for i := len downto index + 1 do
  begin
    objList[i].id := objList[i - 1].id;
    objList[i].obj := objList[i - 1].obj;
  end;                                   { for }

  // 将对象插入到 index 处
  objList[index].id := id;
  objList[index].obj := obj;
end;                                     { Insert }

/// <summary>
/// 插入对象到序号处
/// </summary>
/// <param name="index">序号</param>
/// <param name="obj">对象</param>

procedure TCnPodoList.Insert(index: Integer; obj: TObject);
var
  id: string;
begin
  // 将 id 置空
  id := EmptyStr;
  // 调用带 3 个参数的插入方法
  Insert(index, id, obj);
end;

(******************************************************************************)

/// <summary>
/// 按索引移除对象
/// </summary>
/// <param name="index">序号</param>
procedure TCnPodoList.Remove(index: Integer);
var
  i: Integer;
begin
  // 判断序号是否超出下标
  if (index < 0) or (index > Length(objList)) then
  begin
    // 若超出，则抛出异常
    raise TCnOutOfObjectIndexException.Create(OUT_OF_OBJECT_INDEX_EXCEPTION_STRING);
    Exit;
  end;                                   { if }

  // 移除对象
  // 后面的对象依次向上移
  for i := index to Length(objList) - 2 do
  begin
    objList[i].id := objlist[i + 1].id;
    objList[i].obj := objlist[i + 1].obj;
  end;                                   { for }

  // 将最后一个对象去掉
  SetLength(objList, Length(objList) - 1);
end;

(******************************************************************************)

/// <summary>
/// 按objcet移除对象
/// </summary>
/// <param name="index">对象</param>
// 新增 Remove by 天晓 1.6 版
function TCnPodoList.Remove(obj: TObject): boolean;
var
  index: Integer;
begin
  index := indexof(obj);
  if index < 0 then
    result := false
  else
  begin
    Remove(index);
    result := true;
  end;
end;

procedure TCnPodoList.move(oldIndex, newIndex: integer);
var
  obj: TObject;
begin
  // get object at old index
  obj := Objects[oldIndex];
  // remove old obj
  Remove(oldIndex);
  // insert the object at the new index
  Insert(newIndex, obj);
end;

{$ENDIF SUPPORT_ADO}
end.
