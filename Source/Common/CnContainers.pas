{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnContainers;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：链表队列实现
* 单元作者：小峰
* 备    注：简单的链表队列类，从尾Push，从头Pop，参数可以是对象（被转换成指针）。
*           操作时内部有互斥机制，无需在外部通过临界区互斥。操作例子：
*           声明：
*           var
*             Q: TCnQueue;
*
*           创建：
*             Q := TCnQueue.Create;
*            
*           使用：
*
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject.Create;
*             Q.Push(Data); // 放入队列尾
*           end;
*            
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject(Q.Pop); // 从队列头中取出
*             TmpObj.Free;
*           end;
*
*           释放：
*             Q.Free;
* 开发平台：PWinXP + Delphi 7
* 兼容测试：PWin2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2023.08.21 V1.4
*               增加扩展精度浮点数列表
*           2020.11.05 V1.3
*               将大数池基类抽取至此处
*           2017.01.17 V1.2
*               加入 TCnObjectRingBuffer 循环缓冲区实现
*           2016.12.02 V1.1
*               加入 TCnObjectStack 实现，允许 Clear 等方法
*           2008.04.30 V1.0
*               小峰从原始代码移植而来。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, SyncObjs
  {$IFDEF POSIX}, System.Generics.Collections {$ENDIF};

{$DEFINE MULTI_THREAD} // 数学对象池支持多线程，性能略有下降，如不需要，注释此行即可

type
  TCnQueue = class(TObject)
  {* 指针队列实现类}
  private
    FMultiThread: Boolean;
    FHead: TObject;
    FTail: TObject;
    FSize: Integer;
    FLock: TCriticalSection;
    procedure FreeNode(Value: TObject);
    function GetSize: Integer;
  public
    constructor Create(MultiThread: Boolean = False);
    destructor Destroy; override;
    procedure Push(Data: Pointer);
    function Pop: Pointer;
    property Size: Integer read GetSize;
  end;

  TCnObjectStack = class(TObject)
  {* 对象栈实现类}
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function IsEmpty: Boolean;
    procedure Clear;

    procedure Push(AObject: TObject);
    function Pop: TObject;
    function Peek: TObject;
  end;

  ECnRingBufferFullException = class(Exception);
  {* 循环队列缓冲区满时触发的异常}

  ECnRingBufferEmptyException = class(Exception);
  {* 循环队列缓冲区空时触发的异常}

  TCnObjectRingBuffer = class(TObject)
  {* 循环队列缓冲区}
  private
    FFullOverwrite: Boolean;
    FMultiThread: Boolean;
    FSize: Integer;
    FList: TList;
    FLock: TCriticalSection;
    // Idx 可以理解为始终指向相邻位置中间的缝，编号从第 0 到第 Size - 1 ( 第 Size 也即等于第 0 )
    // 有元素的情况下，FrontIdx 高后始终是元素，前可能是空，或绕回来的尾巴
    //                 BackIdx 的低前始终是元素，后可能是空，或绕回来的头
    // 无元素的情况下，FrontIdx 和 BackIdx 相等
    FFrontIdx: Integer;
    FBackIdx: Integer;
    FCount: Integer;
    function GetCount: Integer;
  public
    constructor Create(ASize: Integer; AFullOverwrite: Boolean = False;
      AMultiThread: Boolean = False);
    {* 构造函数，ASize 是缓冲区容量；AFullOverwrite 是否允许缓冲区满后再塞东西时
      覆盖以前的数据，AMultiThread 是否需要多线程互斥}
    destructor Destroy; override;
    {* 析构函数}

    procedure PushToFront(AObject: TObject);
    {* 从循环队列缓冲区前方推入一个 Object，前方是指内部存储索引低的一端，如满且不允许覆盖则抛异常}
    function PopFromBack: TObject;
    {* 从循环队列缓冲区后方弹出一个 Object，后方是指内部存储索引高的一端，无可弹则抛异常}

    procedure PushToBack(AObject: TObject);
    {* 从循环队列缓冲区后方推入一个 Object，后方是指内部存储索引高的一端，如满且不允许覆盖则抛异常}
    function PopFromFront: TObject;
    {* 从循环队列缓冲区前方弹出一个 Object，前方是指内部存储索引低的一端，无可弹则抛异常}

    procedure Dump(List: TList; out FrontIdx: Integer; out BackIdx: Integer);
    {* 把全部内容导出至一 TList，以及指针位置}

    property FullOverwrite: Boolean read FFullOverwrite;
    {* 该循环队列缓冲区满时是否允许覆盖旧数据}
    property MultiThread: Boolean read FMultiThread;
    {* 该循环队列缓冲区是否需要支持多线程并发访问，为 True 时内部有临界区处理}
    property Size: Integer read FSize;
    {* 从循环队列缓冲区的尺寸}
    property Count: Integer read GetCount;
    {* 从循环队列缓冲区内的有效元素数量}
  end;

  TCnMathObjectPool = class(TObjectList)
  {* 数学对象池实现类，允许使用到数学对象池的地方自行继承并创建池}
  private
{$IFDEF MULTI_THREAD}
    FCriticalSection: TCriticalSection;
{$ENDIF}
    procedure Enter; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    procedure Leave; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  protected
    function CreateObject: TObject; virtual; abstract;
    {* 子类必须重载的创建具体对象的方法}
  public
    constructor Create; reintroduce;

    destructor Destroy; override;

    function Obtain: TObject;
    procedure Recycle(Num: TObject);
  end;

  TCnIntegerList = class(TList)
  {* 整数列表，利用 32 位 Pointer 或 64 位 Pointer 的低 32 位存 Integer}
  private
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const Value: Integer);
  public
    function Add(Item: Integer): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: Integer); reintroduce;
    property Items[Index: Integer]: Integer read Get write Put; default;
  end;

  PInt64List = ^TInt64List;
  TInt64List = array[0..MaxListSize - 1] of Int64;

  TCnInt64List = class(TObject)
  {* 64 位整数列表}
  private
    FList: PInt64List;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Int64;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Int64);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Int64): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure DeleteLow(ACount: Integer);
    {* 新增方法，删除 ACount 个最低端元素，如果 Count 不够则删除 Count 个}
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TCnInt64List;
    function First: Int64;
    function IndexOf(Item: Int64): Integer;
    procedure Insert(Index: Integer; Item: Int64);
    procedure InsertBatch(Index: Integer; ACount: Integer);
    {* 新增方法，在某位置批量插入全 0 值 ACount 个}
    function Last: Int64;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Int64): Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Int64 read Get write Put; default;
    property List: PInt64List read FList;
  end;

  PExtendedList = ^TExtendedList;
  TExtendedList = array[0..MaxListSize - 1] of Extended;

  TCnExtendedList = class(TObject)
  {* 扩展精度浮点数列表，注意不同平台下元素长度可能不一样}
  private
    FList: PExtendedList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Extended;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Extended);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Extended): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure DeleteLow(ACount: Integer);
    {* 新增方法，删除 ACount 个最低端元素，如果 Count 不够则删除 Count 个}
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TCnExtendedList;
    function First: Extended;
    function IndexOf(Item: Extended): Integer;
    procedure Insert(Index: Integer; Item: Extended);
    procedure InsertBatch(Index: Integer; ACount: Integer);
    {* 新增方法，在某位置批量插入全 0 值 ACount 个}
    function Last: Extended;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Extended): Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Extended read Get write Put; default;
    property List: PExtendedList read FList;
  end;

  PRefObjectList = ^TRefObjectList;
  TRefObjectList = array[0..MaxListSize - 1] of TObject;

  TCnRefObjectList = class(TObject)
  {* 对象引用列表，类似于 TObjectList 但不 Own 对象}
  private
    FList: PRefObjectList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): TObject;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: TObject);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: TObject): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure DeleteLow(ACount: Integer);
    {* 新增方法，删除 ACount 个最低端元素，如果 Count 不够则删除 Count 个}
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TCnRefObjectList;
    function First: TObject;
    function IndexOf(Item: TObject): Integer;
    procedure Insert(Index: Integer; Item: TObject);
    procedure InsertBatch(Index: Integer; ACount: Integer);
    {* 新增方法，在某位置批量插入全 0 值 ACount 个}
    function Last: TObject;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TObject): Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: TObject read Get write Put; default;
    property List: PRefObjectList read FList;
  end;

{$IFDEF POSIX}

  // MACOS/LINUX 等平台下的 TList 没有 IgnoreDuplicated 功能，需要手工去重
  TCnInternalList<T> = class(TList<T>)
  public
    procedure RemoveDuplictedElements;
  end;

{$ENDIF}

procedure CnIntegerListCopy(Dst, Src: TCnIntegerList);
{* 复制 TCnIntegerList}

procedure CnInt64ListCopy(Dst, Src: TCnInt64List);
{* 复制 TCnInt64List}

procedure CnRefObjectListCopy(Dst, Src: TCnRefObjectList);
{* 复制 TCnRefObjectList}

implementation

uses
  CnNative;

resourcestring
  SCnInt64ListError = 'Int64 List Error. %d';
  SCnExtendedListError = 'Float List Error. %d';
  SCnRefObjectListError = 'Reference Object List Error. %d';
  SCnEmptyPopFromBackError = 'Ring Buffer Empty. Can NOT Pop From Back.';
  SCnEmptyPopFromFrontError = 'Ring Buffer Empty. Can NOT Pop From Front.';
  SCnFullPushToBackError = 'Ring Buffer Full. Can NOT Push To Back.';
  SCnFullPushToFrontError = 'Ring Buffer Full. Can NOT Push To Front.';

type
  TCnNode = class
  private
    FNext: TCnNode;
    FData: Pointer;
  public
    property Next: TCnNode read FNext write FNext;
    property Data: Pointer read FData write FData;
  end;

{ TCnQueue }

procedure TCnQueue.FreeNode(Value: TObject);
var
  Tmp: TCnNode;
begin
  Tmp := TCnNode(Value).Next;
  TCnNode(Value).Free;
  if Tmp = nil then
    Exit;
  FreeNode(Tmp);
end;

constructor TCnQueue.Create(MultiThread: Boolean);
begin
  FMultiThread := MultiThread;
  FHead := nil;
  FTail := nil;
  FSize := 0;
  if FMultiThread then
    FLock := TCriticalSection.Create;
end;

destructor TCnQueue.Destroy;
begin
  if FHead <> nil then
    FreeNode(FHead);
  if FMultiThread then
    FLock.Free;
  inherited;
end;

function TCnQueue.Pop: Pointer;
var
  Tmp: TCnNode;
begin
  if FMultiThread then
    FLock.Enter;

  try
    Result := nil;
    if FHead = nil then
      Exit;

    Result := TCnNode(FHead).Data;
    Tmp := TCnNode(FHead).Next;
    TCnNode(FHead).Free;
    FHead := Tmp;
    
    if Tmp = nil then
      FTail := nil;
    FSize := FSize - 1;
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

procedure TCnQueue.Push(Data: Pointer);
var
  Tmp: TCnNode;
begin
  if FMultiThread then
    FLock.Enter;

  try
    if Data = nil then Exit;
    Tmp := TCnNode.Create;
    Tmp.Data := Data;
    Tmp.Next := nil;
    
    if FTail = nil then
    begin
      FTail := Tmp;
      FHead := Tmp;
    end
    else
    begin
      TCnNode(FTail).Next := Tmp;
      FTail := Tmp
    end;
    
    FSize := FSize + 1;
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

function TCnQueue.GetSize: Integer;
begin
  Result := FSize;
end;

{ TCnObjectStack }

procedure TCnObjectStack.Clear;
begin
  FList.Clear;
end;

function TCnObjectStack.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCnObjectStack.Create;
begin
  FList := TList.Create;
end;

destructor TCnObjectStack.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCnObjectStack.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TCnObjectStack.Peek: TObject;
begin
  Result := TObject(FList[FList.Count - 1]);
end;

function TCnObjectStack.Pop: TObject;
begin
  Result := TObject(FList[FList.Count - 1]);
  FList.Delete(FList.Count - 1);
end;

procedure TCnObjectStack.Push(AObject: TObject);
begin
  FList.Add(AObject);
end;

{ TCnRingBuffer }

constructor TCnObjectRingBuffer.Create(ASize: Integer; AFullOverwrite,
  AMultiThread: Boolean);
begin
  Assert(ASize > 0);

  FSize := ASize;
  FFullOverwrite := AFullOverwrite;
  FMultiThread := AMultiThread;

  FList := TList.Create;
  FList.Count := FSize;

  if FMultiThread then
    FLock := TCriticalSection.Create;
end;

destructor TCnObjectRingBuffer.Destroy;
begin
  if FMultiThread then
    FLock.Free;
  FList.Free;
  inherited;
end;

procedure TCnObjectRingBuffer.Dump(List: TList; out FrontIdx: Integer;
  out BackIdx: Integer);
var
  I: Integer;
begin
  FrontIdx := FFrontIdx;
  BackIdx := FBackIdx;
  if List <> nil then
  begin
    List.Clear;
    for I := 0 to FList.Count - 1 do
      List.Add(FList[I]);
  end;
end;

function TCnObjectRingBuffer.GetCount: Integer;
begin
  Result := FCount;
end;

{$HINTS OFF}

function TCnObjectRingBuffer.PopFromBack: TObject;
begin
  Result := nil;  // 不加则低版本 Delphi 有警告，加则高版本 Delphi 有警告
  if FMultiThread then
    FLock.Enter;

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create(SCnEmptyPopFromBackError);

    Dec(FBackIdx);
    if FBackIdx < 0 then
      FBackIdx := FSize - 1;
    Result := TObject(FList[FBackIdx]);
    FList[FBackIdx] := nil;
    Dec(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

function TCnObjectRingBuffer.PopFromFront: TObject;
begin
  Result := nil; // 不加则低版本 Delphi 有警告，加则高版本 Delphi 有警告
  if FMultiThread then
    FLock.Enter;

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create(SCnEmptyPopFromFrontError);

    Result := TObject(FList[FFrontIdx]);
    FList[FFrontIdx] := nil;

    Inc(FFrontIdx);
    if FFrontIdx >= FSize then
      FFrontIdx := 0;
    Dec(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

{$HINTS ON}

procedure TCnObjectRingBuffer.PushToBack(AObject: TObject);
begin
  if FMultiThread then
    FLock.Enter;

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create(SCnFullPushToBackError);

    FList[FBackIdx] := AObject;
    Inc(FBackIdx);
    if FBackIdx >= FSize then
      FBackIdx := 0;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

procedure TCnObjectRingBuffer.PushToFront(AObject: TObject);
begin
  if FMultiThread then
    FLock.Enter;

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create(SCnFullPushToFrontError);

    Dec(FFrontIdx);
    if FFrontIdx < 0 then
      FFrontIdx := FSize - 1;
    FList[FFrontIdx] := AObject;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

{ TCnMathObjectPool }

constructor TCnMathObjectPool.Create;
begin
  inherited Create(False);
{$IFDEF MULTI_THREAD}
  FCriticalSection := TCriticalSection.Create;
{$ENDIF}
end;

destructor TCnMathObjectPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;

{$IFDEF MULTI_THREAD}
  FCriticalSection.Free;
{$ENDIF}
  inherited;
end;

procedure TCnMathObjectPool.Enter;
begin
{$IFDEF MULTI_THREAD}
  FCriticalSection.Enter;
{$ENDIF}
end;

procedure TCnMathObjectPool.Leave;
begin
{$IFDEF MULTI_THREAD}
  FCriticalSection.Leave;
{$ENDIF}
end;

function TCnMathObjectPool.Obtain: TObject;
begin
  Enter;
  try
    if Count = 0 then
      Result := CreateObject
    else
    begin
      Result := TObject(Items[Count - 1]);
      Delete(Count - 1);
    end;
  finally
    Leave;
  end;
end;

procedure TCnMathObjectPool.Recycle(Num: TObject);
begin
  if Num <> nil then
  begin
    Enter;
    try
      Add(Num);
    finally
      Leave;
    end;
  end;
end;

{ TCnIntegerList }

function TCnIntegerList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(IntegerToPointer(Item));
end;

function TCnIntegerList.Get(Index: Integer): Integer;
begin
  Result := PointerToInteger(inherited Get(Index));
end;

procedure TCnIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, IntegerToPointer(Item));
end;

procedure TCnIntegerList.Put(Index: Integer; const Value: Integer);
begin
  inherited Put(Index, IntegerToPointer(Value));
end;

{ TCnInt64List }

destructor TCnInt64List.Destroy;
begin
  Clear;
end;

function TCnInt64List.Add(Item: Int64): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnInt64List.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnInt64List.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Int64));
end;

procedure TCnInt64List.DeleteLow(ACount: Integer);
begin
  if ACount > 0 then
  begin
    if ACount >= FCount then
      Clear
    else
    begin
      Dec(FCount, ACount);

      // 从 0 删除到 ACount - 1，也就是把 ACount 到 Count - 1 处的 Move 到 0
      System.Move(FList^[ACount], FList^[0],
        FCount * SizeOf(Int64));
    end;
  end;
end;

class procedure TCnInt64List.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

procedure TCnInt64List.Exchange(Index1, Index2: Integer);
var
  Item: Int64;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SCnInt64ListError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SCnInt64ListError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnInt64List.Expand: TCnInt64List;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnInt64List.First: Int64;
begin
  Result := Get(0);
end;

function TCnInt64List.Get(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);
  Result := FList^[Index];
end;

procedure TCnInt64List.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnInt64List.IndexOf(Item: Int64): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnInt64List.Insert(Index: Integer; Item: Int64);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SCnInt64ListError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Int64));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TCnInt64List.InsertBatch(Index, ACount: Integer);
begin
  if ACount <= 0 then
    Exit;

  if (Index < 0) or (Index > FCount) then
    Error(SCnInt64ListError, Index);
  SetCapacity(FCount + ACount); // 容量扩充至至少 FCount + ACount，FCount 没变

  System.Move(FList^[Index], FList^[Index + ACount],
    (FCount - Index) * SizeOf(Int64));
  System.FillChar(FList^[Index], ACount * SizeOf(Int64), 0);
  FCount := FCount + ACount;
end;

function TCnInt64List.Last: Int64;
begin
  Result := Get(FCount - 1);
end;

procedure TCnInt64List.Move(CurIndex, NewIndex: Integer);
var
  Item: Int64;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SCnInt64ListError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnInt64List.Put(Index: Integer; Item: Int64);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);

  FList^[Index] := Item;
end;

function TCnInt64List.Remove(Item: Int64): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnInt64List.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SCnInt64ListError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Int64));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnInt64List.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SCnInt64ListError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Int64), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

{ TCnExtendedList }

destructor TCnExtendedList.Destroy;
begin
  Clear;
end;

function TCnExtendedList.Add(Item: Extended): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnExtendedList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnExtendedList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnExtendedListError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Extended));
end;

procedure TCnExtendedList.DeleteLow(ACount: Integer);
begin
  if ACount > 0 then
  begin
    if ACount >= FCount then
      Clear
    else
    begin
      Dec(FCount, ACount);

      // 从 0 删除到 ACount - 1，也就是把 ACount 到 Count - 1 处的 Move 到 0
      System.Move(FList^[ACount], FList^[0],
        FCount * SizeOf(Extended));
    end;
  end;
end;

class procedure TCnExtendedList.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

procedure TCnExtendedList.Exchange(Index1, Index2: Integer);
var
  Item: Extended;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SCnExtendedListError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SCnExtendedListError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnExtendedList.Expand: TCnExtendedList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnExtendedList.First: Extended;
begin
  Result := Get(0);
end;

function TCnExtendedList.Get(Index: Integer): Extended;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnExtendedListError, Index);
  Result := FList^[Index];
end;

procedure TCnExtendedList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnExtendedList.IndexOf(Item: Extended): Integer;
begin
  Result := 0;
  while (Result < FCount) and (Abs(FList^[Result] - Item) < 0.00001) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnExtendedList.Insert(Index: Integer; Item: Extended);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SCnExtendedListError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Extended));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TCnExtendedList.InsertBatch(Index, ACount: Integer);
begin
  if ACount <= 0 then
    Exit;

  if (Index < 0) or (Index > FCount) then
    Error(SCnExtendedListError, Index);
  SetCapacity(FCount + ACount); // 容量扩充至至少 FCount + ACount，FCount 没变

  System.Move(FList^[Index], FList^[Index + ACount],
    (FCount - Index) * SizeOf(Extended));
  System.FillChar(FList^[Index], ACount * SizeOf(Extended), 0);
  FCount := FCount + ACount;
end;

function TCnExtendedList.Last: Extended;
begin
  Result := Get(FCount - 1);
end;

procedure TCnExtendedList.Move(CurIndex, NewIndex: Integer);
var
  Item: Extended;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SCnExtendedListError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnExtendedList.Put(Index: Integer; Item: Extended);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnExtendedListError, Index);

  FList^[Index] := Item;
end;

function TCnExtendedList.Remove(Item: Extended): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnExtendedList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SCnExtendedListError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Extended));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnExtendedList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SCnExtendedListError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Extended), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

{ TCnRefObjectList }

destructor TCnRefObjectList.Destroy;
begin
  Clear;
end;

function TCnRefObjectList.Add(Item: TObject): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnRefObjectList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnRefObjectList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnRefObjectListError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TObject));
end;

procedure TCnRefObjectList.DeleteLow(ACount: Integer);
begin
  if ACount > 0 then
  begin
    if ACount >= FCount then
      Clear
    else
    begin
      Dec(FCount, ACount);

      // 从 0 删除到 ACount - 1，也就是把 ACount 到 Count - 1 处的 Move 到 0
      System.Move(FList^[ACount], FList^[0],
        FCount * SizeOf(TObject));
    end;
  end;
end;

class procedure TCnRefObjectList.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

procedure TCnRefObjectList.Exchange(Index1, Index2: Integer);
var
  Item: TObject;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SCnRefObjectListError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SCnRefObjectListError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnRefObjectList.Expand: TCnRefObjectList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnRefObjectList.First: TObject;
begin
  Result := Get(0);
end;

function TCnRefObjectList.Get(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnRefObjectListError, Index);
  Result := FList^[Index];
end;

procedure TCnRefObjectList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnRefObjectList.IndexOf(Item: TObject): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnRefObjectList.Insert(Index: Integer; Item: TObject);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SCnRefObjectListError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TObject));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TCnRefObjectList.InsertBatch(Index, ACount: Integer);
begin
  if ACount <= 0 then
    Exit;

  if (Index < 0) or (Index > FCount) then
    Error(SCnRefObjectListError, Index);
  SetCapacity(FCount + ACount); // 容量扩充至至少 FCount + ACount，FCount 没变

  System.Move(FList^[Index], FList^[Index + ACount],
    (FCount - Index) * SizeOf(TObject));
  System.FillChar(FList^[Index], ACount * SizeOf(TObject), 0);
  FCount := FCount + ACount;
end;

function TCnRefObjectList.Last: TObject;
begin
  Result := Get(FCount - 1);
end;

procedure TCnRefObjectList.Move(CurIndex, NewIndex: Integer);
var
  Item: TObject;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SCnRefObjectListError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnRefObjectList.Put(Index: Integer; Item: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnRefObjectListError, Index);

  FList^[Index] := Item;
end;

function TCnRefObjectList.Remove(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnRefObjectList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SCnRefObjectListError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TObject));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnRefObjectList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SCnRefObjectListError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TObject), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure CnIntegerListCopy(Dst, Src: TCnIntegerList);
begin
  if (Src <> nil) and (Dst <> nil) and (Src <> Dst) then
  begin
    Dst.Count := Src.Count;
    if Src.Count > 0 then
    begin
{$IFDEF LIST_NEW_POINTER}
      Move(Src.List[0], Dst.List[0], Src.Count * SizeOf(Integer));
{$ELSE}
      Move(Src.List^, Dst.List^, Src.Count * SizeOf(Integer));
{$ENDIF}
    end;
  end;
end;

procedure CnInt64ListCopy(Dst, Src: TCnInt64List);
begin
  if (Src <> nil) and (Dst <> nil) and (Src <> Dst) then
  begin
    Dst.Count := Src.Count;
    if Src.Count > 0 then
      Move(Src.List^, Dst.List^, Src.Count * SizeOf(Int64));
  end;
end;

procedure CnRefObjectListCopy(Dst, Src: TCnRefObjectList);
begin
  if (Src <> nil) and (Dst <> nil) and (Src <> Dst) then
  begin
    Dst.Count := Src.Count;
    if Src.Count > 0 then
      Move(Src.List^, Dst.List^, Src.Count * SizeOf(TObject));
  end;
end;

{$IFDEF POSIX}

{ TCnInternalList<T> }

procedure TCnInternalList<T>.RemoveDuplictedElements;
var
  I, J: Integer;
  V: NativeInt;
  Dup: Boolean;
begin
  for I := Count - 1 downto 0 do
  begin
    V := ItemValue(Items[I]);
    Dup := False;
    for J := 0 to I - 1 do
    begin
      if V = ItemValue(Items[J]) then
      begin
        Dup := True;
        Break;
      end;
    end;

    if Dup then
      Delete(I);
  end;
end;

{$ENDIF}

end.
