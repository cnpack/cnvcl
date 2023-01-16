{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnLockFree;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：涉及到无锁机制的一些原子操作封装以及无锁数据结构的实现
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：封装了 CnAtomicCompareAndSet 的 CAS 实现，适应 32 位和 64 位
*           并基于此实现了自旋锁与无锁有序链表、单读单写的无锁循环队列
*           无锁有序链表参考了 Timothy L. Harris 的论文：
*             《A Pragmatic Implementation of Non-Blocking Linked-Lists》
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/ 10.3，包括 Win32/64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2021.04.02 V1.2
*               不支持 Atomic 的 C++Buider 平台上有四个函数会抛出异常
*           2021.01.22 V1.1
*               实现单线程读单线程写的无锁循环队列
*           2021.01.10 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, {$IFDEF MSWINDOWS} Windows, {$ENDIF} Classes, CnNative;

const
  CN_RING_QUEUE_DEFAULT_CAPACITY = 16;

type
{$IFDEF CPUX64}
  TCnSpinLockRecord = NativeInt;
{$ELSE}
  TCnSpinLockRecord = Integer;
{$ENDIF}
  {* 自旋锁，值为 1 时表示有别人锁了它，0 表示空闲}

  TCnLockFreeNodeKeyCompare = function(Key1, Key2: TObject): Integer;
  {* 用来比较 Key 的方法类型，返回 -1、0、1}

  PCnLockFreeLinkedNode = ^TCnLockFreeLinkedNode;

  TCnLockFreeLinkedNode = packed record
  {* 无锁单链表节点}
    Key: TObject;
    Value: TObject;
    Next: PCnLockFreeLinkedNode; // 该指针低位为 1 表示该节点待删除
  end;

  TCnLockFreeNodeTravelEvent = procedure(Sender: TObject; Node: PCnLockFreeLinkedNode) of object;

  TCnLockFreeLinkedList = class
  {* 无锁有序单链表实现}
  private
    FCompare: TCnLockFreeNodeKeyCompare;
    FGuardHead: PCnLockFreeLinkedNode; // 固定的头节点指针
    FGuardTail: PCnLockFreeLinkedNode; // 固定的尾节点指针
    FHiddenHead, FHiddenTail: TCnLockFreeLinkedNode;
    FOnTravelNode: TCnLockFreeNodeTravelEvent;
    function GetLastNode: PCnLockFreeLinkedNode; // 获得链表中 FGuadTail 之前的最后一个活指针
    function GetLast2Nodes(out P1, P2: PCnLockFreeLinkedNode): Boolean;
    // 获得链表中 FGuadTail 之前的最后两个指针，P1.Next 指向 P2，P2.Next 指向 FTail

    function CompareKey(Key1, Key2: TObject): Integer;
    function IsNodePointerMarked(Node: PCnLockFreeLinkedNode): Boolean;  // 节点指针用最低位存储一 Mark 标记
    function GetMarkedNodePointer(Node: PCnLockFreeLinkedNode): PCnLockFreeLinkedNode;   // 将一个节点指针加上 Mark 标记
    function ExtractRealNodePointer(Node: PCnLockFreeLinkedNode): PCnLockFreeLinkedNode; // 返回一个节点指针的实际值无论有无加上 Mark 标记
    function GetNextNode(Node: PCnLockFreeLinkedNode): PCnLockFreeLinkedNode; // 返回一个节点的后续真实节点，去除 Mark 标记的
    procedure InternalSearch(Key: TObject; var LeftNode, RightNode: PCnLockFreeLinkedNode);
    {* 内部搜索方法，返回针对特定 Key 的左右相邻未标记节点，其中左节点的 Key < Key，右节点的 Key >= Key}
  protected
    function CreateNode: PCnLockFreeLinkedNode;
    procedure FreeNode(Node: PCnLockFreeLinkedNode);
    procedure DoTravelNode(Node: PCnLockFreeLinkedNode); virtual;
  public
    constructor Create(KeyCompare: TCnLockFreeNodeKeyCompare = nil);
    destructor Destroy; override;

    function GetCount: Integer;
    {* 遍历获取有多少个节点，不包括隐藏节点}
    procedure Clear;
    {* 全部清空，该方法不支持多线程}
    procedure Travel;
    {* 从头遍历，针对每个节点调用 OnTravelNode 事件，不支持多线程}

    procedure Append(Key, Value: TObject);
    {* 在链表尾部直接添加新节点，调用者需自行保证 Key 递增，否则搜索会出错}
    function RemoveTail(out Key, Value: TObject): Boolean;
    {* 删除链表尾节点，并把删除的尾节点的 Key 和 Value 返回。如无尾节点，返回 False}

    function Insert(Key, Value: TObject): Boolean;
    {* 在链表中根据 Key 查找位置并插入并返回 True，如果 Key 已经存在则返回 False}
    function HasKey(Key: TObject; out Value: TObject): Boolean;
    {* 在链表中搜索指定 Key 是否存在，如存在则返回 True 并将对应 Value 返回}
    function Delete(Key: TObject): Boolean;
    {* 在链表中删除指定 Key 匹配的节点，返回是否找到}

    property OnTravelNode: TCnLockFreeNodeTravelEvent read FOnTravelNode write FOnTravelNode;
    {* 遍历时触发的事件}
  end;

  TCnLockFreeLinkedStack = class(TCnLockFreeLinkedList)
  {* 以无锁链表为基础实现的无锁单链表堆栈}
  public
    procedure Push(Key, Value: TObject);
    {* 入栈}
    function Pop(out Key, Value: TObject): Boolean;
    {* 出栈，如栈空则返回 False}
  end;

  TCnLockFreeSingleRingQueueNode = packed record
  {* 队列节点}
    Key: TObject;
    Value: TObject;
  end;

  TCnLockFreeSingleRingQueue = class
  {* 只支持一线程读一线程写的无锁循环队列}
  private
    FSize: Integer;
    FHead: Cardinal; // 如队列不空，Head 始终指向有效节点。Head 后一个节点始终无效，无论队列满或空
    FTail: Cardinal; // 无论队列满或空，Tail 始终指向无效节点。如队列不空，Tail 后一个节点始终有效
    FNodes: array of TCnLockFreeSingleRingQueueNode;
    function GetCount: Integer;
    function GetIndex(Seq: Cardinal): Integer;
  protected

  public
    constructor Create(ASize: Integer = CN_RING_QUEUE_DEFAULT_CAPACITY);
    destructor Destroy; override;

    function Enqueue(Key, Value: TObject): Boolean;
    {* 入队列头，返回是否入成功，如队列满则返回 False}
    function Dequeue(out Key, Value: TObject): Boolean;
    {* 出队列尾，返回是否出成功，如队列空则返回 False}

    function IsEmpty: Boolean;
    {* 返回是否空，也就是头指针和尾指针相等}
    function IsFull: Boolean;
    {* 返回是否满，也就是头指针比尾指针差一}
    property Count: Integer read GetCount;
    {* 队列中有几个有效元素}
    property Size: Integer read FSize;
    {* 队列的大小}
  end;

//------------------------------------------------------------------------------
// 原子操作封装
//------------------------------------------------------------------------------

function CnAtomicIncrement32(var Addend: Integer): Integer;
{* 原子操作令一 32 位值增 1，返回增加后的值}

function CnAtomicDecrement32(var Addend: Integer): Integer;
{* 原子操作令一 32 位值减 1，返回减少后的值}

function CnAtomicExchange32(var Target: Integer; Value: Integer): Integer;
{* 原子操作令俩 32 位值交换，返回 Targe 的原始值}

function CnAtomicExchangeAdd32(var Addend: LongInt; Value: LongInt): Longint;
{* 原子操作令 32 位值 Addend := Addend + Value，返回 Addend 原始值}

// 以下 4 个 64 位函数在不支持 Atomic 的低版本 Delphi 或 C++Buider 平台上会使用 API
// 如果是 32 位 XP 系统或更老的，则运行期不支持，会抛出异常

function CnAtomicIncrement64(var Addend: Int64): Int64;
{* 原子操作令一 64 位值增 1，返回增加后的值}

function CnAtomicDecrement64(var Addend: Int64): Int64;
{* 原子操作令一 64 位值减 1，返回减少后的值}

function CnAtomicExchange64(var Target: Int64; Value: Int64): Int64;
{* 原子操作令俩 64 位值交换，返回 Targe 的原始值}

function CnAtomicExchangeAdd64(var Addend: Int64; Value: Int64): Int64;
{* 原子操作令 64 位值 Addend := Addend + Value，返回 Addend 原始值}

// 以上 4 个 64 位函数有部分情况不支持

function CnAtomicCompareExchange(var Target: Pointer; NewValue: Pointer; Comperand: Pointer): Pointer;
{* 原子操作比较 Target 与 Comperand 俩值，相等时则将 NewValue 赋值给 Target，返回旧的 Target 值
  32 位下支持 32 位值，64 位下支持 64 位值}

function CnAtomicCompareAndSet(var Target: Pointer; NewValue: Pointer; Comperand: Pointer): Boolean;
{* 原子操作执行以下代码，比较 Target 与 Comperand 俩值，相等时则将 NewValue 赋值给 Target，
  32 位下支持 32 位值，64 位下支持 64 位值，未发生赋值操作时返回 False，赋值时返回 True
  注意 NewValue 不要等于 Target，否则无法区分是否执行了赋值操作，因为无论是否赋值都一样
  if Comperand = Target then
  begin
    Target := NewValue;
    Result := True;
  end
  else
    Result := False;
}

//------------------------------------------------------------------------------
// 自旋锁
//------------------------------------------------------------------------------

procedure CnInitSpinLockRecord(var Critical: TCnSpinLockRecord);
{* 初始化一个自旋锁，其实就是赋值为 0，无需释放}

procedure CnSpinLockEnter(var Critical: TCnSpinLockRecord);
{* 进入自旋锁}

procedure CnSpinLockLeave(var Critical: TCnSpinLockRecord);
{* 离开自旋锁}

implementation

{$IFDEF MSWINDOWS}
const
  kernel32  = 'kernel32.dll';
{$ELSE}
const // MACOS 和 Linux 都用这个，TODO: 不确定 Mac 上行不
  kernel32  = 'libwine.borland.so';
{$ENDIF}

// 注意 32 位 XP 上没有，需要动态获得

type
  TInterlockedCompareExchange64 = function (var Destination: Int64;
    Exchange: Int64; Comparand: Int64): Int64 stdcall;

var
  InterlockedCompareExchange64: TInterlockedCompareExchange64 = nil;

resourcestring
  SCnNotImplemented = 'NOT Implemented!';

//function InterlockedCompareExchange64(var Destination: Int64; Exchange: Int64;
//  Comparand: Int64): Int64 stdcall; external kernel32 name 'InterlockedCompareExchange64';

function CnAtomicIncrement32(var Addend: Integer): Integer;
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicIncrement(Addend);
{$ELSE}
  Result := InterlockedIncrement(Addend);
{$ENDIF}
end;

function CnAtomicDecrement32(var Addend: Integer): Integer;
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicDecrement(Addend);
{$ELSE}
  Result := InterlockedDecrement(Addend);
{$ENDIF}
end;

function CnAtomicExchange32(var Target: Integer; Value: Integer): Integer;
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicExchange(Target, Value);
{$ELSE}
  Result := InterlockedExchange(Target, Value);
{$ENDIF}
end;

function CnAtomicExchangeAdd32(var Addend: LongInt; Value: LongInt): LongInt;
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicIncrement(Addend, Value) - Value;
{$ELSE}
  {$IFDEF WIN64}
  Result := InterlockedExchangeAdd(Addend, Value);
  {$ELSE}
  Result := InterlockedExchangeAdd(@Addend, Value);
  {$ENDIF}
{$ENDIF}
end;

function CnAtomicIncrement64(var Addend: Int64): Int64;
{$IFNDEF SUPPORT_ATOMIC}
var
  Tmp: Int64;
{$ENDIF}
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicIncrement(Addend);
{$ELSE}
  if not Assigned(InterlockedCompareExchange64) then
    raise Exception.Create(SCnNotImplemented);
  repeat
    Tmp := Addend;
    Result := InterlockedCompareExchange64(Addend, Tmp + 1, Tmp);
  until Result = Tmp;
  Inc(Result);
{$ENDIF}
end;

function CnAtomicDecrement64(var Addend: Int64): Int64;
{$IFNDEF SUPPORT_ATOMIC}
var
  Tmp: Int64;
{$ENDIF}
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicDecrement(Addend);
{$ELSE}
  if not Assigned(InterlockedCompareExchange64) then
    raise Exception.Create(SCnNotImplemented);
  repeat
    Tmp := Addend;
    Result := InterlockedCompareExchange64(Addend, Tmp - 1, Tmp);
  until Result = Tmp;
  Dec(Result);
{$ENDIF}
end;

function CnAtomicExchange64(var Target: Int64; Value: Int64): Int64;
{$IFNDEF SUPPORT_ATOMIC}
var
  Tmp: Int64;
{$ENDIF}
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicExchange(Target, Value);
{$ELSE}
  if not Assigned(InterlockedCompareExchange64) then
    raise Exception.Create(SCnNotImplemented);
  repeat
    Tmp := Target;
    Result := InterlockedCompareExchange64(Target, Value, Tmp);
  until Result = Tmp;
{$ENDIF}
end;

function CnAtomicExchangeAdd64(var Addend: Int64; Value: Int64): Int64;
var
  Tmp: Int64;
begin
  repeat
{$IFDEF SUPPORT_ATOMIC}
    Tmp := Addend;
    Result := AtomicCmpExchange(Addend, Addend + Value, Tmp);
{$ELSE}
    if not Assigned(InterlockedCompareExchange64) then
      raise Exception.Create(SCnNotImplemented);
    Tmp := Addend;
    Result := InterlockedCompareExchange64(Addend, Addend + Value, Tmp);
{$ENDIF}
  until Result = Tmp;
end;

function CnAtomicCompareExchange(var Target: Pointer; NewValue: Pointer; Comperand: Pointer): Pointer;
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicCmpExchange(Target, NewValue, Comperand);
{$ELSE}
  {$IFDEF BDS}
    // XE2 下 Win64 时会出错，必须改用 64 位版本，XE3 后的版本声明调整一致了
    {$IFDEF DELPHIXE2}
      {$IFDEF WIN64}
       Result := Pointer(InterlockedCompareExchange64(Int64(Target), Int64(NewValue), Int64(Comperand)));
      {$ELSE}
       Result := Pointer(InterlockedCompareExchange(Integer(Target), Integer(NewValue), Integer(Comperand)));
      {$ENDIF}
    {$ELSE}
       Result := Pointer(InterlockedCompareExchange(Integer(Target), Integer(NewValue), Integer(Comperand)));
    {$ENDIF}
  {$ELSE}
  {$IFDEF FPC}
  Result := Pointer(InterlockedCompareExchange(LongInt(Target), LongInt(NewValue), LongInt(Comperand)));
  {$ELSE}// D567 下的 InterlockedCompareExchange 被声明为 Pointer
  Result := InterlockedCompareExchange(Target, NewValue, Comperand);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

{$IFDEF SUPPORT_ATOMIC}

function CnAtomicCompareAndSet(var Target: Pointer; NewValue: Pointer;
  Comperand: Pointer): Boolean;
begin
  AtomicCmpExchange(Target, NewValue, Comperand, Result);
end;

{$ELSE}

{$IFDEF WIN64}

// XE2 的 Win64 下没有 Atomic 系列函数
function CnAtomicCompareAndSet(var Target: Pointer; NewValue: Pointer;
  Comperand: Pointer): Boolean; assembler;
asm
  // API 里的 InterlockedCompareExchange 不会返回是否成功，不得不用汇编代替
  MOV  RAX,  R8
  LOCK CMPXCHG [RCX], RDX
  SETZ AL
  AND RAX, $FF
end;

{$ELSE}

{$IFDEF FPC}

// TODO: FPC 下面的实现
function CnAtomicCompareAndSet(var Target: Pointer; NewValue: Pointer;
  Comperand: Pointer): Boolean;
begin
  raise Exception.Create(SCnNotImplemented);
end;

{$ELSE}

// XE2 或以下版本的 Win32 实现
function CnAtomicCompareAndSet(var Target: Pointer; NewValue: Pointer;
  Comperand: Pointer): Boolean; assembler;
asm
  // API 里的 InterlockedCompareExchange 不会返回是否成功，不得不用汇编代替
  // 其中 @Target 是 EAX, NewValue 是 EDX，Comperand 是 ECX，
  // 要做一次 ECX 与 EAX 的互换才能调用 LOCK CMPXCHG [ECX], EDX，结果返回在 AL 中
  XCHG  EAX, ECX
  LOCK CMPXCHG [ECX], EDX
  SETZ AL
  AND EAX, $FF
end;

{$ENDIF}

{$ENDIF}

{$ENDIF}

procedure CnInitSpinLockRecord(var Critical: TCnSpinLockRecord);
begin
  Critical := 0;
end;

procedure CnSpinLockEnter(var Critical: TCnSpinLockRecord);
begin
  repeat
    while Critical <> 0 do
      ;  // 此处如果改成 Sleep(0) 就会有线程切换开销，就不是自旋锁了
  until CnAtomicCompareAndSet(Pointer(Critical), Pointer(1), Pointer(0));
end;

procedure CnSpinLockLeave(var Critical: TCnSpinLockRecord);
begin
  while not CnAtomicCompareAndSet(Pointer(Critical), Pointer(0), Pointer(1)) do
    Sleep(0);
end;

{ TCnLockFreeLinkedList }

function DefaultKeyCompare(Key1, Key2: TObject): Integer;
var
  K1, K2: TCnNativeInt;
begin
  K1 := TCnNativeInt(Key1);
  K2 := TCnNativeInt(Key2);

  if K1 > K2 then
    Result := 1
  else if K1 < K2 then
    Result := -1
  else
    Result := 0;
end;

procedure TCnLockFreeLinkedList.Append(Key, Value: TObject);
var
  Node, P: PCnLockFreeLinkedNode;
begin
  Node := CreateNode;
  Node^.Key := Key;
  Node^.Value := Value;
  Node^.Next := FGuardTail;

  // 原子操作，先摸到尾巴 Tail，判断 Tail 的 Next 是否是 FGuardTail，是则将 Tail 的 Next 设为 NewNode
  // 如果其他线程修改了 Tail，导致这里取到的 Tail 不是尾巴，那么 Tail 的 Next 就不为 nil，就得重试
  // 注意这里的尾巴是指不包括 FGuardTail 内的最后一个节点，尾巴的 Next 应该是 FGuardTail
  repeat
    P := GetLastNode;
  until CnAtomicCompareAndSet(Pointer(P^.Next), Pointer(Node), FGuardTail);
end;

procedure TCnLockFreeLinkedList.Clear;
var
  P, N: PCnLockFreeLinkedNode;
begin
  P := GetNextNode(FGuardHead);
  while (P <> nil) and (P <> FGuardTail) do
  begin
    N := P;
    P := GetNextNode(P);
    FreeNode(N);
  end;
  FGuardHead := @FHiddenHead;
  FGuardTail := @FHiddenTail;
end;

function TCnLockFreeLinkedList.CompareKey(Key1, Key2: TObject): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(Key1, Key2)
  else
    Result := DefaultKeyCompare(Key1, Key2);
end;

constructor TCnLockFreeLinkedList.Create(KeyCompare: TCnLockFreeNodeKeyCompare);
begin
  inherited Create;
  FCompare := KeyCompare;

  FHiddenTail.Key := nil;
  FHiddenTail.Value := nil;
  FHiddenTail.Next := nil;

  FHiddenHead.Key := nil;
  FHiddenHead.Value := nil;
  FHiddenHead.Next := @FHiddenTail;

  FGuardHead := @FHiddenHead;
  FGuardTail := @FHiddenTail;
end;

function TCnLockFreeLinkedList.CreateNode: PCnLockFreeLinkedNode;
begin
  New(Result);
  Result^.Next := nil;
end;

function TCnLockFreeLinkedList.Delete(Key: TObject): Boolean;
var
  R, RN, L: PCnLockFreeLinkedNode;
begin
  Result := False;
  RN := nil;

  while True do
  begin
    InternalSearch(Key, L, R);
    if (R = FGuardTail) or (CompareKey(R^.Key, Key) <> 0) then
      Exit;

    // R 符合要求，要删掉 R，先把 R 的 Next 标记为待删除
    RN := R^.Next;
    if not IsNodePointerMarked(RN) then
      if CnAtomicCompareAndSet(Pointer(R^.Next), GetMarkedNodePointer(RN), RN) then
        Break;
  end;

  // 再把 L 的 Next 挂到 R 的 Next
  if not CnAtomicCompareAndSet(Pointer(L^.Next), RN, R) then
    InternalSearch(R^.Key, L, R); // 然后删 R
  Result := True;
end;

destructor TCnLockFreeLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TCnLockFreeLinkedList.ExtractRealNodePointer(
  Node: PCnLockFreeLinkedNode): PCnLockFreeLinkedNode;
begin
  Result := PCnLockFreeLinkedNode(TCnNativeUInt(Node) and TCnNativeUInt(not 1));
end;

procedure TCnLockFreeLinkedList.FreeNode(Node: PCnLockFreeLinkedNode);
begin
  if Node <> nil then
    Dispose(Node);
end;

function TCnLockFreeLinkedList.GetCount: Integer;
var
  P: PCnLockFreeLinkedNode;
begin
  Result := 0;
  P := GetNextNode(FGuardHead);
  while (P <> nil) and (P <> FGuardTail) do
  begin
    Inc(Result);
    P := GetNextNode(P);
  end;
end;

function TCnLockFreeLinkedList.GetLastNode: PCnLockFreeLinkedNode;
begin
  Result := FGuardHead;
  while (Result^.Next <> nil) and (Result^.Next <> FGuardTail) do
    Result := Result^.Next;
end;

function TCnLockFreeLinkedList.GetNextNode(
  Node: PCnLockFreeLinkedNode): PCnLockFreeLinkedNode;
begin
  Result := ExtractRealNodePointer(Node^.Next);
end;

function TCnLockFreeLinkedList.HasKey(Key: TObject; out Value: TObject): Boolean;
var
  L, R: PCnLockFreeLinkedNode;
begin
  InternalSearch(Key, L, R);
  if (R = FGuardTail) or (R^.Key <> Key) then
  begin
    Value := nil;
    Result := False;
  end
  else
  begin
    Value := R^.Value;
    Result := True;
  end;
end;

procedure TCnLockFreeLinkedList.InternalSearch(Key: TObject; var LeftNode,
  RightNode: PCnLockFreeLinkedNode);
var
  T, TN, L: PCnLockFreeLinkedNode;
begin
  L := nil;
  while True do
  begin
    T := FGuardHead;
    TN := T^.Next;

    // 搜索节点，初步得到左右节点
    repeat
      if not IsNodePointerMarked(TN) then
      begin
        LeftNode := T;
        L := TN;
      end;

      T := ExtractRealNodePointer(TN);
      if T = FGuardTail then
        Break;

      TN := T^.Next;
    until (not IsNodePointerMarked(TN)) and (CompareKey(T^.Key, Key) >= 0);
    RightNode := T;

    // 检查 LeftNode 和 RightNode 是否相邻，其中 L 是 LeftNode 的下一个节点，
    if L = RightNode then
    begin
      // 如果右节点的下个节点被标记了，要重来
      if (RightNode <> FGuardTail) and IsNodePointerMarked(RightNode^.Next) then
        Continue
      else
      begin
        Exit;
      end;
    end;

    // 到这里的话说明 L 和 RightNode 不一样，中间有标记过的节点，于是删掉标记过的节点 L，让 LeftNode 的 Next 指向 Right
    if CnAtomicCompareAndSet(Pointer(LeftNode^.Next), RightNode, L) then
    begin
      FreeNode(L);
      if (RightNode <> FGuardTail) and IsNodePointerMarked(RightNode^.Next) then
        Continue
      else
      begin
        Exit;
      end;
    end;
  end;
end;

function TCnLockFreeLinkedList.IsNodePointerMarked(
  Node: PCnLockFreeLinkedNode): Boolean;
begin
  Result := (TCnNativeUInt(Node) and 1) <> 0;
end;

function TCnLockFreeLinkedList.GetMarkedNodePointer(
  Node: PCnLockFreeLinkedNode): PCnLockFreeLinkedNode;
begin
  Result := PCnLockFreeLinkedNode(TCnNativeUInt(Node) or 1);
end;

function TCnLockFreeLinkedList.Insert(Key, Value: TObject): Boolean;
var
  L, R, N: PCnLockFreeLinkedNode;
begin
  Result := False;
  N := nil;

  while True do
  begin
    InternalSearch(Key, L, R);
    if (R <> FGuardTail) and (CompareKey(R^.Key, Key) = 0) then
      Exit; // Key 已存在

    FreeNode(N);
    N := CreateNode;
    N^.Next := R;
    N^.Key := Key;
    N^.Value := Value;

    if CnAtomicCompareAndSet(Pointer(L^.Next), N, R) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TCnLockFreeLinkedList.Travel;
var
  P: PCnLockFreeLinkedNode;
begin
  P := GetNextNode(FGuardHead);
  while (P <> nil) and (P <> FGuardTail) do
  begin
    DoTravelNode(P);
    P := GetNextNode(P);
  end;
end;

procedure TCnLockFreeLinkedList.DoTravelNode(Node: PCnLockFreeLinkedNode);
begin
  if Assigned(FOnTravelNode) then
    FOnTravelNode(Self, Node);
end;

function TCnLockFreeLinkedList.RemoveTail(out Key, Value: TObject): Boolean;
var
  P1, P2, RN: PCnLockFreeLinkedNode;
begin
  Result := False;
  RN := nil;

  while True do
  begin
    if not GetLast2Nodes(P1, P2) then
      Exit;

    RN := P2^.Next;
    if not IsNodePointerMarked(RN) then
      if CnAtomicCompareAndSet(Pointer(P2^.Next), GetMarkedNodePointer(RN), RN) then
        Break;
  end;

  Key := P2^.Key;
  Value := P2^.Value;

  if not CnAtomicCompareAndSet(Pointer(P1^.Next), RN, P2) then
    InternalSearch(P2^.Key, P1, P2);
  Result := True;
end;

function TCnLockFreeLinkedList.GetLast2Nodes(out P1,
  P2: PCnLockFreeLinkedNode): Boolean;
var
  T, TN, L: PCnLockFreeLinkedNode;
begin
  Result := False;
  if FGuardHead^.Next = FGuardTail then
    Exit;

  L := nil;
  while True do
  begin
    T := FGuardHead;
    P1 := T;
    TN := T^.Next;

    // 搜索节点，初步得到左右节点
    repeat
      if not IsNodePointerMarked(TN) then
      begin
        P1 := T;
        L := TN;
      end;

      T := ExtractRealNodePointer(TN);
      if T^.Next = FGuardTail then
        Break;

      TN := T^.Next;
    until False;
    P2 := T;

    // 检查 LeftNode 和 RightNode 是否相邻
    if L = P2 then
    begin
      // 如果右节点的下个节点被标记了，要重来
      if (P2 <> FGuardTail) and IsNodePointerMarked(P2^.Next) then
        Continue
      else
      begin
        Result := True;
        Exit;
      end;
    end;

    // 删掉标记过的节点
    if CnAtomicCompareAndSet(Pointer(P1^.Next), P2, L) then
    begin
      if (P2 <> FGuardTail) and IsNodePointerMarked(P2^.Next) then
        Continue
      else
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

{ TCnLockFreeSingleRingQueue }

constructor TCnLockFreeSingleRingQueue.Create(ASize: Integer);
begin
  if ASize <= 0 then
    ASize := CN_RING_QUEUE_DEFAULT_CAPACITY;

  FSize := GetUInt32PowerOf2GreaterEqual(ASize);
  if FSize <= 1 then
    FSize := CN_RING_QUEUE_DEFAULT_CAPACITY;

  SetLength(FNodes, FSize);
  FHead := 0;
  FTail := 0;
end;

function TCnLockFreeSingleRingQueue.Dequeue(out Key, Value: TObject): Boolean;
var
  Idx: Integer;
begin
  // 先出队列再改 Tail
  Result := False;
  if not IsEmpty then
  begin
    Idx := GetIndex(FTail + 1);
    Key := FNodes[Idx].Key;
    Value := FNodes[Idx].Value;

    Inc(FTail);
    Result := True;
  end;
end;

destructor TCnLockFreeSingleRingQueue.Destroy;
begin
  SetLength(FNodes, 0);
  inherited;
end;

function TCnLockFreeSingleRingQueue.Enqueue(Key, Value: TObject): Boolean;
var
  Idx: Integer;
begin
  // 先进队列再改 Head
  Result := False;
  if not IsFull then
  begin
    Idx := GetIndex(FHead + 1);
    FNodes[Idx].Key := Key;
    FNodes[Idx].Value := Value;

    Inc(FHead);
    Result := True;
  end;
end;

function TCnLockFreeSingleRingQueue.GetCount: Integer;
begin
  Result := FHead - FTail;
end;

function TCnLockFreeSingleRingQueue.GetIndex(Seq: Cardinal): Integer;
begin
  Result := Seq and (FSize - 1);
end;

function TCnLockFreeSingleRingQueue.IsEmpty: Boolean;
begin
  Result := (GetIndex(FHead) = GetIndex(FTail));
end;

function TCnLockFreeSingleRingQueue.IsFull: Boolean;
begin
  Result := (GetIndex(FHead) = GetIndex(FTail - 1));
end;

{ TCnLockFreeLinkedStack }

function TCnLockFreeLinkedStack.Pop(out Key, Value: TObject): Boolean;
begin
  Result := RemoveTail(Key, Value);
end;

procedure TCnLockFreeLinkedStack.Push(Key, Value: TObject);
begin
  Append(Key, Value);
end;

initialization
  InterlockedCompareExchange64 := GetProcAddress(GetModuleHandle(kernel32), 'InterlockedCompareExchange64');

end.
