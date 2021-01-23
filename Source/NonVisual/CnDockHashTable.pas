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

unit CnDockHashTable;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：用于停靠的HashTable单元 
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.07.13 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Controls;

const DefaultHashSize = 20;

type
  { 散列的节点类 }
  TCnDockClientHashNode = class
  private
    FKeyName: string;                 //名字项
    FKeyData: Pointer;                //数据
    FPrevNode,                        //上一个指针
    FNextNode: TCnDockClientHashNode; //下一个指针
    FListIndex: Integer;              //属于哪个桶
  public
    property KeyName: string read FKeyName write FKeyName;
    property KeyData: Pointer read FKeyData write FKeyData;
    property PrevNode: TCnDockClientHashNode
        read FPrevNode write FPrevNode;
    property NextNode: TCnDockClientHashNode
        read FNextNode write FNextNode;
    property ListIndex: Integer read FListIndex write FListIndex;
  end;

  { 散列类 }
  TCnDockControlHashTable = class
  private
    FCurrentSize,            //当前桶数
    FTableSize: Integer;     //最大桶数
    FEntryList: TList;       //桶队列
    FRiseException: Boolean; //当查找到相同的名字时是否触发异常
    procedure SetTableSize(const Value: Integer);     //为散列表分配空间
  protected
    function HashProc(Name: string): Integer; virtual;//散列函数，计算Name的初始桶号
    procedure DeleteListIndex(Index: Integer);        //删除索引为Index的桶
    function CreateKeyNode(KeyName: string; KeyData: Pointer;
      ListIndex: Integer): TCnDockClientHashNode;     //创建散列节点
    function CompareKey(Key1, Key2: string): Integer; //比较两个节点
  public
    constructor Create(Size: Integer = DefaultHashSize; RiseExcept: Boolean = True); virtual;
    destructor Destroy; override;
    procedure CreateDictionary(Size: Integer); virtual;//创建字典
    function IsIn(Name: string): Boolean; virtual;     //判断Name是否在字典中
    function FindNode(Name: string): TCnDockClientHashNode; virtual;//查找节点
    function Find(Name: string): Pointer; virtual;     //查找
    function Insert(Name: string; Data: Pointer): Integer; virtual;//插入
    procedure Remove(Name: string); virtual;           //删除
    procedure MakeEmpty;                               //置散列表为空
    property CurrentSize: Integer read FCurrentSize;
    property TableSize: Integer read FTableSize write SetTableSize;
  end;

implementation

uses
  CnDockGlobal, SysUtils;

{ TCnDockControlHashTable }

function TCnDockControlHashTable.CompareKey(Key1, Key2: string): Integer;
begin
  Result := AnsiStrComp(PChar(Key1), PChar(Key2));
end;

constructor TCnDockControlHashTable.Create(Size: Integer; RiseExcept: Boolean);
begin
  { 首先创建桶 }
  CreateDictionary(Size);
  FRiseException := RiseExcept;
end;

procedure TCnDockControlHashTable.CreateDictionary(Size: Integer);
begin
  FEntryList := TList.Create;
  FEntryList.Count := Size;
  FTableSize := Size;
end;

function TCnDockControlHashTable.CreateKeyNode(KeyName: string;
  KeyData: Pointer; ListIndex: Integer): TCnDockClientHashNode;
begin
  Result := TCnDockClientHashNode.Create;
  Result.KeyName := KeyName;
  Result.KeyData := KeyData;
  Result.ListIndex := ListIndex;
end;

procedure TCnDockControlHashTable.DeleteListIndex(Index: Integer);
var Node, NextNode: TCnDockClientHashNode;
begin
  Node := FEntryList[Index];
  while Node <> nil do
  begin
    NextNode := Node.NextNode;
    Node.Free;
    Node := NextNode;
  end;
  FEntryList.Delete(Index);
end;

destructor TCnDockControlHashTable.Destroy;
begin
  MakeEmpty;
  FEntryList.Free;
  inherited;
end;

function TCnDockControlHashTable.Find(Name: string): Pointer;
var Node: TCnDockClientHashNode;
begin
  Node := FindNode(Name);
  if Node <> nil then
    Result := Node.KeyData
  else Result := nil;
end;

function TCnDockControlHashTable.FindNode(
  Name: string): TCnDockClientHashNode;
var Value: Integer;
  ListIndex: Integer;
begin
  ListIndex := HashProc(Name);
  Assert((ListIndex >= 0) and (ListIndex < FTableSize), gs_CnTableIndexError);
  Result := FEntryList[ListIndex];
  if Result = nil then Exit;
  repeat
    Value := CompareKey(Name, Result.FKeyName);
    if Value = 0 then Exit;
    Result := Result.FNextNode;
  until Result = nil;
end;

function TCnDockControlHashTable.HashProc(Name: string): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Name) do
    Inc(Result, Ord(Name[i]));
  Result := Result mod FTableSize;
end;

function TCnDockControlHashTable.Insert(Name: string;
  Data: Pointer): Integer;
var Index: Integer;
  Value: Integer;
  Node, ParentNode: TCnDockClientHashNode;
begin
  Result := -1;
  Index := HashProc(Name);
  Assert((Index >= 0) and (Index < FTableSize), gs_CnTableIndexError);
  { 首先查找在桶里面是否有数据 }
  if FEntryList[Index] = nil then
    FEntryList[Index] := CreateKeyNode(Name, Data, Index)
  else
  begin
    Node := FEntryList[Index];
    repeat
      { 判断在散列里面是否有相同的键值 }
      Value := CompareKey(Name, Node.FKeyName);
      { 触发异常 }
      if FRiseException then
        Assert(Value <> 0, gs_CnNodeExistedError)
      else if Value = 0 then
        Exit;
      ParentNode := Node;
      Node := Node.FNextNode;
    until Node = nil;
    { 创建节点 }
    Node := CreateKeyNode(Name, Data, Index);
    Node.FPrevNode := ParentNode;
    ParentNode.NextNode := Node;
  end;
  Result := Index;
end;

function TCnDockControlHashTable.IsIn(Name: string): Boolean;
begin
  Result := FindNode(Name) <> nil;
end;

procedure TCnDockControlHashTable.MakeEmpty;
var i: Integer;
begin
  for i := FEntryList.Count - 1 downto 0 do
    DeleteListIndex(i);
end;

procedure TCnDockControlHashTable.Remove(Name: string);
var Node: TCnDockClientHashNode;
begin
  Node := FindNode(Name);
  if Node <> nil then
  begin
    if Node.FPrevNode <> nil then
      Node.FPrevNode.FNextNode := Node.FNextNode
    else FEntryList[Node.ListIndex] := Node.FNextNode;
    if Node.FNextNode <> nil then
      Node.FNextNode.FPrevNode := Node.FPrevNode;
    Node.Free;
  end;
end;

procedure TCnDockControlHashTable.SetTableSize(const Value: Integer);
begin
  FEntryList.Count := Value;
end;

end.
