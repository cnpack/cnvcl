{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnLinkedList;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：双向链表的List实现单元
* 单元作者：巴哈姆特
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 备    注：2008.05.23
*               移植单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, SysUtils;

type
  PCnLinkedNode = ^TCnLinkedNode;
  TCnLinkedNode = packed record
  {* 双向链表节点结构}
    Previous: PCnLinkedNode; // 上一节点
    Code: Pointer;           // 本节点的内容
    Next: PCnLinkedNode;     // 下一节点
  end;

  ICnLinkedListIterator = interface
    ['{0380614D-F455-4FDA-8862-6E1505C0C5D4}']
  {* 双向链表迭代器接口
     使用方法举例：

     var
       Iterator: ICnLinkedListIterator;
       List: TCnLinkedList;
     begin
       ...

       Iterator := List.CreateIterator;
       while not Iterator.Eof do
       begin
         Iterator.GetItem;
         Iterator.Next;
       end;
       
       // Iterator 无需释放，自动释放。
     end;
  }
    function Bof: Boolean;
    {* 是否超过了链表头}
    function Eof: Boolean;
    {* 是否超过了链表尾}
    procedure First;
    {* 到链表开头}
    procedure Last;
    {* 到链表末尾}
    procedure Previous;
    {* 到链表当前位置的上一个}
    procedure Next;
    {* 到链表当前位置的下一个}
    function GetCurrentItem: Pointer;
    {* 获得链表当前位置的值}
  end;

  TCnLinkedList = class(TObject)
  private
    FFirst, FLast, FNode: PCnLinkedNode;
    FCount, FIndex: Integer;
    FList: TList;
    FAutoClear: Boolean;

    FLock: TRTLCriticalSection;

    function GetItem(const Index: Integer): PCnLinkedNode;
    function GetList: TList;
  protected
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    procedure SetCount(const NewCount: Integer);

    function GetFirst: PCnLinkedNode;  // 返回首节点
    function GetLast: PCnLinkedNode;   // 返回尾节点
    function GetBefore: PCnLinkedNode; // 返回前一次查找的节点
    function GetMiddle(const Index: Integer): PCnLinkedNode; // 返回中间节点

    function GetPrevious(Move: Boolean = False): PCnLinkedNode; // 返回上一个节点
    function GetNext(Move: Boolean = False): PCnLinkedNode;     // 返回下一个节点

    function AddFirst(const Item: Pointer): Boolean; // 添加首节点
    function AddLast(const Item: Pointer): Boolean;  // 添加尾节点
    function AddMiddle(const Index: Integer; const Item: Pointer): Boolean; // 添加中间节点

    function DeleteFirst: Boolean; // 删除首节点
    function DeleteLast: Boolean;  // 删除尾节点
    function DeleteMiddle(const Index: Integer): Boolean; // 删除中间节点
    function DeleteLastNode: Boolean; // 删除最后一个节点
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Item: Pointer): Integer;
    {* 添加一条目到链表尾}
    function Delete(const Index: Integer): Integer;
    {* 删除指定索引处的条目}

    function First: Pointer;
    {* 获得链表头}
    function Last: Pointer;
    {* 获得链表尾}

    procedure Lock;
    {* 加锁, 供多线程中操作使用}
    procedure UnLock;
    {* 解锁, 供多线程中操作使用}

    // 以下两个移动方法会影响到内部当前指针，多线程访问时需要加锁操作。

    function Previous: Pointer;
    {* 将链表内部的当前指针移动到上一个, 如超出头, 则当前指针为 nil}
    function Next: Pointer;
    {* 将链表内部的当前指针移动到下一个, 如超出尾, 则当前指针为 nil}
    
    // 以上几个移动方法会影响到内部当前指针，多线程访问时需要加锁操作。

    function IndexOf(const Item: Pointer): Integer;
    {* 查找一条目的索引号}
    function Insert(Index: Integer; Item: Pointer): Integer;
    {* 在指定位置插入一条目}
    function Clear: Integer;
    {* 清除全部条目, 返回原有条目数量}

    procedure Exchange(Index1, Index2: Integer);
    {* 交换两个条目位置}
    function Extract(Item: Pointer): Pointer;
    {* 抽取一节点}
    procedure Move(CurIndex, NewIndex: Integer);
    {* 移动节点}
    function Remove(Item: Pointer): Integer;
    {* 删除一节点}
    procedure Pack;
    {* 压缩}
    procedure Assign(AList: TCnLinkedList);
    {* 从另一双向链表复制}

    function CreateIterator: ICnLinkedListIterator;
    {* 返回一链表迭代器接口，可供外界遍历。此遍历器线程访问安全}

    property Items[Index: Integer]: Pointer read Get write Put; default;
    {* 以索引号直接访问条目, 数据量大时效率不高}
    property Count: Integer read FCount write SetCount;
    {* 条目数量}
    property List: TList read GetList;
    {* 返回一个正常的 TList, 包含链表所有内容}
    property AutoClear: Boolean read FAutoClear write FAutoClear;
    {* 是否在删除节点时自动Dispose节点内容}
  end;

implementation

const
  SListIndexError = 'List Out of Bounds (%d).';
  SListCountError = 'Invalid List Count (%d).';

type
  TCnLinkedListIterator = class(TInterfacedObject, ICnLinkedListIterator)
  {* 双向链表迭代器实现类，封装了通常的遍历操作}
  private
    FList: TCnLinkedList;
    FBof: Boolean;
    FEof: Boolean;
    FCurrent: PCnLinkedNode;
  public
    constructor Create(AList: TCnLinkedList);

    function Bof: Boolean;
    function Eof: Boolean;
    procedure First;
    procedure Last;
    procedure Previous;
    procedure Next;
    function GetCurrentItem: Pointer;
  end;

{ TCnLinkedList }

function TCnLinkedList.Add(const Item: Pointer): Integer;
begin
  Result := FCount;
  if not AddLast(Item) then
    Result := -1;
end;

function TCnLinkedList.AddFirst(const Item: Pointer): Boolean;
var
  AItem: PCnLinkedNode;
begin
  try
    AItem := New(PCnLinkedNode);
    AItem.Previous := nil;
    AItem.Code := Item;
    AItem.Next := FFirst;

    if FFirst = nil then //如果是添加第一个节点
      FLast := AItem
    else
      FFirst.Previous := AItem;

    FFirst := AItem;

    if FIndex <> -1 then
      Inc(FIndex);

    Inc(FCount);
    if Item <> nil then
      Notify(Item, lnAdded);
    Result := True;
  except
    Result := False;
  end;
end;

function TCnLinkedList.AddLast(const Item: Pointer): Boolean;
var
  AItem: PCnLinkedNode;
begin
  try
    AItem := New(PCnLinkedNode);
    AItem.Previous := FLast;
    AItem.Code := Item;
    AItem.Next := nil;

    if FLast = nil then //如果是添加第一个节点
      FFirst := AItem
    else
      FLast.Next := AItem;

    FLast := AItem;

    Inc(FCount);
    if Item <> nil then
      Notify(Item, lnAdded);
    Result := True;
  except
    Result := False;
  end;
end;

function TCnLinkedList.AddMiddle(const Index: Integer; const Item: Pointer): Boolean;
var
  Item_P, Item_N, AItem: PCnLinkedNode;
begin
  Result := False;
  try
    if (Index <= 0) or (Index >= FCount - 1) then
      Exit;

    Item_N := GetItem(Index); //当前节点
    Item_P := GetPrevious();

    AItem := New(PCnLinkedNode);
    AItem.Previous := Item_P;
    AItem.Code := Item;
    AItem.Next := Item_N;

    Item_P.Next := AItem;
    Item_N.Previous := AItem;

    //if (FIndex <= Index) and (FIndex <> -1) then
    Inc(FIndex);

    Inc(FCount);
    if Item <> nil then
      Notify(Item, lnAdded);

    Result := True;
  except
  end;
end;

procedure TCnLinkedList.Assign(AList: TCnLinkedList);
var
  I: Integer;
begin
  Clear;
  if AList.Count = 0 then
    Exit;

  Add(AList.Items[0]);
  for I := 0 to AList.Count - 2 do
    Add(AList.Next);
end;

function TCnLinkedList.Clear: Integer;
begin
  Result := FCount;
  SetCount(0);
  FFirst := nil;
  FLast := nil;
end;

constructor TCnLinkedList.Create;
begin
  inherited Create();
  InitializeCriticalSection(FLock);

  FFirst := nil;
  FLast := nil;
  FNode := nil;
  FIndex := -1;
  FCount := 0;
  FAutoClear := False;
  FList := TList.Create;
end;

function TCnLinkedList.CreateIterator: ICnLinkedListIterator;
begin
  Result := TCnLinkedListIterator.Create(Self);
end;

function TCnLinkedList.Delete(const Index: Integer): Integer;
begin
  Result := -1;
  if (Index < 0) or (Index >= FCount) then
    Exit;

  if FCount > 1 then
  begin
    if Index = 0 then //删除首节点
    begin
      DeleteFirst();
      Result := Index;
    end
    else
      if Index = FCount - 1 then //删除尾节点
      begin
        DeleteLast();
        Result := Index;
      end
      else
        if DeleteMiddle(Index) then
          Result := Index;
  end
  else //如果是删除最后一个节点
  begin
    DeleteLastNode();
    Result := 0;
  end;
end;

function TCnLinkedList.DeleteFirst: Boolean;
var
  Item: PCnLinkedNode;
begin
  Result := False;

  if FFirst = nil then
    Exit;

  Item := FFirst;
  FFirst := FFirst.Next;
  FFirst.Previous := nil;

  if FIndex = 0 then
    FNode := FFirst
  else
    if FIndex <> -1 then
      Dec(FIndex);

  Dec(FCount);
  if Item.Code <> nil then
  begin
    Notify(Item.Code, lnDeleted);
    if FAutoClear then
      Dispose(Item.Code);
  end;
  Dispose(Item);

  Result := True;
end;

function TCnLinkedList.DeleteLast: Boolean;
var
  Item: PCnLinkedNode;
begin
  Result := False;

  if FLast = nil then
    Exit;

  Item := FLast;
  FLast := FLast.Previous;
  FLast.Next := nil;

  if FIndex = FCount - 1 then
  begin
    Dec(FIndex);
    FNode := FLast;
  end;

  Dec(FCount);
  if Item.Code <> nil then
  begin
    Notify(Item.Code, lnDeleted);
    if FAutoClear then
      Dispose(Item.Code);
  end;
  Dispose(Item);

  Result := True;
end;

function TCnLinkedList.DeleteLastNode: Boolean;
var
  Item: PCnLinkedNode;
begin
  Result := False;
  if FCount > 1 then
    Exit;

  Item := FFirst;

  FFirst := nil;
  FLast := nil;
  FNode := nil;
  FIndex := -1;

  Dec(FCount);
  if Item.Code <> nil then
  begin
    Notify(Item.Code, lnDeleted);
    if FAutoClear then
      Dispose(Item.Code);
  end;
  Dispose(Item);

  Result := True;
end;

function TCnLinkedList.DeleteMiddle(const Index: Integer): Boolean;
var
  Item_P, Item_N, Item: PCnLinkedNode;
begin
  Result := False;

  if (Index <= 0) or (Index >= FCount - 1) then
    Exit;

  Item := GetItem(Index); //当前节点
  Item_P := GetPrevious(); //上一节点
  Item_N := GetNext(); //下一节点

  Item_P.Next := Item_N;
  Item_N.Previous := Item_P;

  FNode := Item_N;
{
  if FNode = Item then //如果查询用节点为当前要删除的节点
    FNode := Item_N
  else
    if FIndex > Index then //如果删除查询节点前的节点
      Dec(FIndex);
}
  Dec(FCount);
  if Item.Code <> nil then
  begin
    Notify(Item.Code, lnDeleted);
    if FAutoClear then
      Dispose(Item.Code);
  end;

  DisPose(Item);
  Result := True;
end;

destructor TCnLinkedList.Destroy;
begin
  Lock();
  try
    if Assigned(FList) then
      FreeAndNil(FList);

    Clear;
    FIndex := -1;
    FNode := nil;
    FFirst := nil;
    FLast := nil;
  finally
    UnLock();
    DeleteCriticalSection(FLock);
  end;
  inherited Destroy();
end;

procedure TCnLinkedList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    raise Exception.Create(Format(SListIndexError, [Index1]));
  if (Index2 < 0) or (Index2 >= FCount) then
    raise Exception.Create(Format(SListIndexError, [Index2]));

  Item := Get(Index1);
  Put(Index1, Get(Index2));
  Put(Index2, Item);
end;

function TCnLinkedList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
  AAutoClear: Boolean;
begin
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    AAutoClear := FAutoClear;
    FAutoClear := False;
    Delete(I);
    FAutoClear := AAutoClear;
    Notify(Result, lnExtracted);
  end
  else
    Result := nil;
end;

function TCnLinkedList.First: Pointer;
begin
  Result := FFirst.Code;
end;

function TCnLinkedList.Get(Index: Integer): Pointer;
var
  Item: PCnLinkedNode;
begin
  Item := GetItem(Index);
  if Item <> nil then
    Result := Item.Code
  else
    Result := nil;
end;

function TCnLinkedList.GetBefore: PCnLinkedNode;
begin
  Result := FNode;
end;

function TCnLinkedList.GetFirst: PCnLinkedNode;
begin
  Result := FFirst;
  if FFirst = nil then
    Exit;

  FIndex := 0;
  FNode := FFirst;
end;

function TCnLinkedList.GetItem(const Index: Integer): PCnLinkedNode;
begin
  Result := nil;
  if (Index < 0) or (Index >= FCount) then
    Exit;

  if Index = 0 then //查找首节点
    Result := GetFirst
  else
    if Index = FCount - 1 then //查找尾节点
      Result := GetLast
    else
      if Index = FIndex - 1 then //如果本次查找在上次查找的前一节点
        Result := GetPrevious(True)
      else
        if Index = FIndex + 1 then //如果本次查找在上次查找的后一节点
          Result := GetNext(True)
        else
          if Index = FIndex then //如果本次查找位置和上次查找相同
            Result := GetBefore
          else
            Result := GetMiddle(Index);
end;

function TCnLinkedList.GetLast: PCnLinkedNode;
begin
  Result := FLast;
  if FLast = nil then
    Exit;

  FNode := FLast;
  FIndex := FCount - 1;
end;

function TCnLinkedList.GetList: TList;
var
  Index: Integer;
begin
  FList.Clear;
  if FCount <> 0 then
  begin
    FList.Add(Get(0));
    for Index := 0 to FCount - 2 do
      FList.Add(Next());
  end;

  Result := FList;
end;

function TCnLinkedList.GetMiddle(const Index: Integer): PCnLinkedNode;
var
  I, IFirst, ILast, ICode: Integer;
  PFirst, PLast: PCnLinkedNode;
begin
  if FIndex = -1 then //如果是第一次查找
  begin
    FIndex := 0;
    FNode := FFirst;
  end
  else //如果上次查找节点位置比最后节点大
  begin
    FIndex := FCount - 1;
    FNode := FLast;
  end;

  if Index < FIndex then //如果本次查找节点在上次查找节点之前
  begin
    IFirst := 0; //循环变量起始值
    ILast := FIndex; //循环变量终止值
    PFirst := FFirst; //循环查找起始节点
    PLast := FNode; //循环查找终止节点
  end
  else
  begin
    IFirst := FIndex; //循环变量起始值
    ILast := FCount - 1; //循环变量终止值
    PFirst := FNode; //循环查找起始节点
    PLast := FLast; //循环查找终止节点
  end;
  ICode := (ILast - IFirst) div 2; //计算中间值

  if Index < ICode then //如果查找序号比中间值小就从起始位置开始查找
  begin
    Result := PFirst;
    I := IFirst;
    while I <> Index do
    begin
      Result := Result.Next;
      Inc(I);
    end;
  end
  else //如果查找序号比中间值小就从终止位置开始查找
  begin
    Result := PLast;
    I := ILast;
    while I <> Index do
    begin
      Result := Result.Previous;
      Dec(I);
    end;
  end;

  FNode := Result;
  FIndex := Index;
end;

function TCnLinkedList.GetNext(Move: Boolean): PCnLinkedNode;
begin
  Result := nil;
  if FNode = nil then
    Exit;

  Result := FNode.Next;
  if Move then
  begin
    Inc(FIndex);
    FNode := FNode.Next;
  end;
end;

function TCnLinkedList.GetPrevious(Move: Boolean): PCnLinkedNode;
begin
  Result := nil;
  if FNode.Previous = nil then
    Exit;

  Result := FNode.Previous;
  if Move then
  begin
    Dec(FIndex);
    FNode := FNode.Previous;
  end;
end;

function TCnLinkedList.IndexOf(const Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (Item <> Get(Result)) do
    Inc(Result);

  if Result = FCount then
    Result := -1;
end;

function TCnLinkedList.Insert(Index: Integer; Item: Pointer): Integer;
var
  Flag: Boolean;
begin
  Result := -1;
  if Index < 0 then
    Exit;

  if Index = 0 then
    Flag := AddFirst(Item)
  else
    if Index >= FCount - 1 then
      Flag := AddLast(Item)
    else
      Flag := AddMiddle(Index, Item);

  if Flag then
    Result := Index;
end;

function TCnLinkedList.Last: Pointer;
begin
  Result := FLast.Code;
end;

procedure TCnLinkedList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TCnLinkedList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
  AAutoClear: Boolean;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      raise Exception.Create(Format(SListIndexError, [NewIndex]));

    Item := Get(CurIndex);
    AAutoClear := FAutoClear;
    FAutoClear := False;
    Delete(CurIndex);
    FAutoClear := AAutoClear;
    Insert(NewIndex, Item);
  end;
end;

function TCnLinkedList.Next: Pointer;
begin
  FNode := FNode.Next;
  Inc(FIndex);
  Result := FNode.Code;
end;

procedure TCnLinkedList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

procedure TCnLinkedList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Get(I) = nil then
      Delete(I);
end;

function TCnLinkedList.Previous: Pointer;
begin
  FNode := FNode.Previous;
  Dec(FIndex);
  Result := FNode.Code;
end;

procedure TCnLinkedList.Put(Index: Integer; Item: Pointer);
var
  Code: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.Create(Format(SListIndexError, [Index]));

  Code := Get(Index);
  if Item <> Code then
  begin
    GetItem(Index).Code := Item;
    if Code <> nil then
      Notify(Code, lnDeleted);
    if Item <> nil then
      Notify(Item, lnAdded);
  end;
end;

function TCnLinkedList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnLinkedList.SetCount(const NewCount: Integer);
var
  I: Integer;
begin
  if NewCount < 0 then
    raise Exception.Create(Format(SListCountError, [NewCount]));

  if NewCount > FCount then
    for I := 0 to NewCount - FCount do
      Add(nil)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure TCnLinkedList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

{ TCnLinkedListIterator }

function TCnLinkedListIterator.Bof: Boolean;
begin
  Result := FBof;
end;

constructor TCnLinkedListIterator.Create(AList: TCnLinkedList);
begin
  inherited Create();

  FList := AList;
  if FList.Count = 0 then
  begin
    FBof := True;
    FEof := True;
  end
  else
    First;
end;

function TCnLinkedListIterator.Eof: Boolean;
begin
  Result := FEof;
end;

procedure TCnLinkedListIterator.First;
begin
  FCurrent := FList.FFirst;
  if FCurrent = nil then
    FBof := True;
end;

function TCnLinkedListIterator.GetCurrentItem: Pointer;
begin
  if FCurrent <> nil then
    Result := FCurrent^.Code
  else
    Result := nil;
end;

procedure TCnLinkedListIterator.Last;
begin
  FCurrent := FList.FLast;
  if FCurrent = nil then
    FEof := True;
end;

procedure TCnLinkedListIterator.Next;
begin
  if FEof then
    Exit;

  FCurrent := FCurrent^.Next;
  FBof := False;
  if FCurrent = nil then
    FEof := True;
end;

procedure TCnLinkedListIterator.Previous;
begin
  if FBof then
    Exit;

  FCurrent := FCurrent^.Previous;
  FEof := False;
  if FCurrent = nil then
    FBof := True;
end;

end.
