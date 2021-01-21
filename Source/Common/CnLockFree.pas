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

unit CnLockFree;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：涉及到无锁机制的一些原子操作封装以及无锁数据结构的实现
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：封装了 CnAtomicCompareAndSet 的 CAS 实现，适应 32 位和 64 位
*           并基于此实现了自旋锁与无所有序链表
*           无锁有序链表参考了 Timothy L. Harris 的论文：
*             《A Pragmatic Implementation of Non-Blocking Linked-Lists》
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/ 10.3，包括 Win32/64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2021.01.10 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, {$IFDEF MSWINDOWS} Windows, {$ENDIF} Classes, CnNativeDecl;

type
{$IFDEF WIN64}
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
    Next: PCnLockFreeLinkedNode;
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
    function Insert(Key, Value: TObject): Boolean;
    {* 在链表中根据 Key 查找位置并插入并返回 True，如果 Key 已经存在则返回 False}
    function HasKey(Key: TObject; out Value: TObject): Boolean;
    {* 在链表中搜索指定 Key 是否存在，如存在则返回 True 并将对应 Value 返回}
    function Delete(Key: TObject): Boolean;
    {* 在链表中删除指定 Key 匹配的节点，返回是否找到}

    property OnTravelNode: TCnLockFreeNodeTravelEvent read FOnTravelNode write FOnTravelNode;
    {* 遍历时触发的事件}
  end;

//------------------------------------------------------------------------------
// 原子操作封装
//------------------------------------------------------------------------------

function CnAtomicIncrement32(var Addend: Integer): Integer;
{* 原子操作令一 32 位值增 1}

function CnAtomicDecrement32(var Addend: Integer): Integer;
{* 原子操作令一 32 位值减 1}

function CnAtomicExchange32(var Target: Integer; Value: Integer): Integer;
{* 原子操作令俩 32 位值交换}

function CnAtomicExchangeAdd32(var Addend: LongInt; Value: LongInt): Longint;
{* 原子操作令 32 位值 Addend := Addend + Value，返回 Addend 原始值}

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

function CnAtomicIncrement32(var Addend: Integer): Integer;
begin
{$IFDEF SUPPORT_ATOMIC}
  AtomicIncrement(Addend);
{$ELSE}
  Result := InterlockedIncrement(Addend);
{$ENDIF}
end;

function CnAtomicDecrement32(var Addend: Integer): Integer;
begin
{$IFDEF SUPPORT_ATOMIC}
  AtomicDecrement(Addend);
{$ELSE}
  Result := InterlockedDecrement(Addend);
{$ENDIF}
end;

function CnAtomicExchange32(var Target: Integer; Value: Integer): Integer;
begin
{$IFDEF SUPPORT_ATOMIC}
  AtomicExchange(Target, Value);
{$ELSE}
  Result := InterlockedExchange(Target, Value);
{$ENDIF}
end;

function CnAtomicExchangeAdd32(var Addend: LongInt; Value: LongInt): LongInt;
begin
{$IFDEF WIN64}
  Result := InterlockedExchangeAdd(Addend, Value);
{$ELSE}
  Result := InterlockedExchangeAdd(@Addend, Value);
{$ENDIF}
end;

function CnAtomicCompareExchange(var Target: Pointer; NewValue: Pointer; Comperand: Pointer): Pointer;
begin
{$IFDEF SUPPORT_ATOMIC}
  Result := AtomicCmpExchange(Target, NewValue, Comperand);
{$ELSE}
  Result := InterlockedCompareExchange(Target, NewValue, Comperand);
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

    RN := R^.Next;
    if not IsNodePointerMarked(RN) then
      if CnAtomicCompareAndSet(Pointer(R^.Next), GetMarkedNodePointer(RN), RN) then
        Break;
  end;

  if not CnAtomicCompareAndSet(Pointer(L^.Next), RN, R) then
    InternalSearch(R^.Key, L, R);
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

    // 检查 LeftNode 和 RightNode 是否相邻
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

    // 删掉标记过的节点
    if CnAtomicCompareAndSet(Pointer(LeftNode^.Next), RightNode, L) then
    begin
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

end.
