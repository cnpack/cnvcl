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

unit CnSkipList;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：跳跃链表 SkipList 实现单元
* 单元作者：刘啸
* 备    注：参考维基百科以及 ftp://ftp.cs.umd.edu/pub/skipLists/
*           单向跳跃链表的搜索时间复杂度优于单向链表，接近树
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2015.05.22 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Contnrs;

const
  CN_SKIPLIST_MAX_LEVEL = 16; // 0 ~ 15

type
  ECnSkipListLevelError = Exception;
  ECnSkipListCompareError = Exception;
  ECnSkipListRandomError = Exception;

  TCnSkipListNodeList = class;

  TCnSkipListNode = class
  {* 一个存储数据节点的结构，包括一排竖的指针}
  private
    FLevel: Integer;
    FForwards: TCnSkipListNodeList;
    FData: Pointer;
    FText: string;
    FOnDestroy: TNotifyEvent;
  protected
    procedure DoDestroy; virtual;
  public
    constructor Create(ALevel: Integer);
    destructor Destroy; override;

    property Forwards: TCnSkipListNodeList read FForwards;
    {* 竖排指针，0 代表最下面的最细化的一层}
    property Level: Integer read FLevel write FLevel;
    {* 此 Node 拥有的有效层次，从 0 到 Level}
    
    property Data: Pointer read FData write FData;
    property Text: string read FText write FText;

    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    {* 释放的回调事件，供外界释放 Data 等}
  end;

  TCnSkipListValueCompareFunc = function (const Value: Pointer; const Node: TCnSkipListNode): Integer;
  {* 用于比较待搜索内容与节点间大小的 Function，>=< 分别返回 1、0、-1，
     有俩特殊逻辑已封装在 SkipList 中实现：Head 节点要小于任何值，任何值小于 nil 节点}

  TCnSkipListNodeList = class(TObjectList)
  {* 一个节点结构中的竖排指针}
  private
    function GetItem(Index: Integer): TCnSkipListNode;
    procedure SetItem(Index: Integer; const Value: TCnSkipListNode);
  public
    constructor Create; 
    destructor Destroy; override;

    function Add(ANode: TCnSkipListNode): Integer;
    property Items[Index: Integer]: TCnSkipListNode read GetItem write SetItem; default;
  end;

  TCnLevelProbability = (lpDot5, lpDot25, lpDot125); // 升 Level 的概率

  TCnSkipList = class
  {* 跳跃链表 SkipList 的实现类}
  private
    FRandomBits: DWORD;
    FAllowDuplicates: Boolean;
    FCompareFunc: TCnSkipListValueCompareFunc;
    FMaxLevel: Integer;
    FHead: TCnSkipListNode;
    FLevelProbability: TCnLevelProbability;
    function GetMaxLevel: Integer;
    function GetCount: Integer;

    function ValueGreaterThanNode(const Value: Pointer; const Node: TCnSkipListNode): Boolean;
    function ValueEqualsNode(const Value: Pointer; const Node: TCnSkipListNode): Boolean;
    function ValueLessThanNode(const Value: Pointer; const Node: TCnSkipListNode): Boolean;

    function GenerateLevel: Integer;
  public
    constructor Create(CompareFunc: TCnSkipListValueCompareFunc; ALevelProbability:
      TCnLevelProbability = lpDot5; AllowDuplicates: Boolean = False);
    {* 构造函数。
       CompareFunc：使用者必须提供一比较函数，供跳跃链表回调，来判断所查找的值与节点的大小关系
       ALevelProbability：升层的概率，默认为 0.5
       AllowDuplicates：是否允许插入重复值，是否相等由 CompareFunc 返回的值决定
    }
    destructor Destroy; override;
    {* 析构函数，会释放全部节点，节点的 OnDestroy 事件可供释放其附加内容}

    function Delete(AValue: Pointer): Boolean;
    {* 查找并删除一值，返回是否删除成功}
    function Search(AValue: Pointer): TCnSkipListNode;
    {* 查找一值，返回找到的节点，无则返回 nil}
    function Insert(AValue: Pointer): TCnSkipListNode;
    {* 插入一值，返回插入的节点，外部必须使用此节点存储 AValue，插入失败返回 nil}

    property AllowDuplicates: Boolean read FAllowDuplicates;
    {* 是否允许插入重复节点}
    property MaxLevel: Integer read GetMaxLevel;
    {* 本表里所有节点的最大层次，各层次从 0 到 MaxLevel}
    property Head: TCnSkipListNode read FHead;
    {* 头节点，并包含最深层次的竖排指针}
    property Count: Integer read GetCount;
    {* 节点数，通过遍历得到}
    property LevelProbability: TCnLevelProbability read FLevelProbability;
    {* 升 Level 的概率，只允许几个固定值}
  end;

implementation

const
  CN_BITS_IN_RANDOM = 31;
  PROV_RSA_FULL = 1;

function CryptAcquireContext(phProv: PULONG; pszContainer: PAnsiChar;
  pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI32 name 'CryptAcquireContextA';

function CryptReleaseContext(hProv: ULONG; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI32 name 'CryptReleaseContext';

function CryptGenRandom(hProv: ULONG; dwLen: DWORD; pbBuffer: PAnsiChar): BOOL;
  stdcall; external ADVAPI32 name 'CryptGenRandom';

// 使用 Windows API 实现随机填充 32 bit
function Rand32: DWORD;
var
  HProv: Cardinal;
  Res: DWORD;
begin
  HProv := 0;
  if not CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, 0) then
    raise ECnSkipListRandomError.Create('SkipList Error Random.');

  if HProv <> 0 then
  begin
    try
      CryptGenRandom(HProv, SizeOf(DWORD), @Res);
    finally
      CryptReleaseContext(HProv, 0);
    end;
  end
  else
    raise ECnSkipListRandomError.Create('SkipList Error Random.');
  
  Result := Res;
end;

{ TCnSkipListNodeList }

function TCnSkipListNodeList.Add(ANode: TCnSkipListNode): Integer;
begin
  Result := inherited Add(ANode);
end;

constructor TCnSkipListNodeList.Create;
begin
  inherited Create(False);
end;

destructor TCnSkipListNodeList.Destroy;
begin
  inherited;

end;

function TCnSkipListNodeList.GetItem(Index: Integer): TCnSkipListNode;
begin
  Result := TCnSkipListNode(inherited GetItem(Index));
end;

procedure TCnSkipListNodeList.SetItem(Index: Integer;
  const Value: TCnSkipListNode);
begin
  inherited SetItem(Index, Value);
end;

{ TCnSkipListNode }

constructor TCnSkipListNode.Create(ALevel: Integer);
var
  I: Integer;
begin
  if (ALevel < 0) or (ALevel >= CN_SKIPLIST_MAX_LEVEL) then
    raise ECnSkipListLevelError.Create('SkipList Error Level.');

  FLevel := ALevel;
  FForwards := TCnSkipListNodeList.Create;

  for I := 0 to CN_SKIPLIST_MAX_LEVEL - 1 do
    FForwards.Add(nil);
end;

destructor TCnSkipListNode.Destroy;
begin
  DoDestroy;
  FForwards.Free;
  inherited;
end;

procedure TCnSkipListNode.DoDestroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
end;

{ TCnSkipList }

constructor TCnSkipList.Create(CompareFunc: TCnSkipListValueCompareFunc;
  ALevelProbability: TCnLevelProbability; AllowDuplicates: Boolean);
begin
  inherited Create;
  if not Assigned(CompareFunc) then
    raise ECnSkipListCompareError.Create('SkipList No Compare Function.');

  FLevelProbability := ALevelProbability;
  FAllowDuplicates := AllowDuplicates;
  FCompareFunc := CompareFunc;

  FHead := TCnSkipListNode.Create(0); // 0 ~ CN_SKIPLIST_MAX_LEVEL
end;

function TCnSkipList.Delete(AValue: Pointer): Boolean;
var
  K, M: Integer;
  P, PreNode, Q: TCnSkipListNode;
  Prevs: array[0..CN_SKIPLIST_MAX_LEVEL - 1] of TCnSkipListNode;
begin
  Result := False;

  PreNode := FHead;
  K := FMaxLevel;
  M := FMaxLevel;
  FillChar(Prevs, SizeOf(Prevs), 0);

  repeat
    Q := PreNode.Forwards[K];
    while ValueGreaterThanNode(AValue, Q) and (Q <> nil) do
    begin
      PreNode := Q;
      Q := PreNode.Forwards[K];
    end;
    Prevs[K] := PreNode;
    Dec(K);
  until K < 0; // 等同于 Search，但记录了中间链

  if ValueEqualsNode(AValue, Q) then // 找到了要删除的
  begin
    K := 0;
    P := Prevs[K];
    while (K <= M) and (P.Forwards[K] = Q) do
    begin
      P.Forwards[K] := Q.Forwards[K];
      Inc(K);
      P := Prevs[K];
    end;

    Q.Free; // 释放被删节点

    while (FHead.Forwards[M] = nil) and (M > 0) do
      Dec(M);
    FMaxLevel := M; // 更新最深层次
    Result := True;
  end;
end;

destructor TCnSkipList.Destroy;
var
  P, Q: TCnSkipListNode;
begin
  P := FHead;
  repeat
    Q := P.Forwards[0];
    P.Free;
    P := Q;
  until P = nil;
  // 遍历最底层 Node 并释放

  inherited;
end;

function TCnSkipList.GenerateLevel: Integer;
var
  C, B: DWORD;
begin
  Result := 0;
  C := 1;
  case FLevelProbability of
    lpDot5: C := 1;        // 1
    lpDot25: C := 3;       // 11
    lpDot125: C := 7;      // 111
  end;

  repeat
    FRandomBits := Rand32;
    B := FRandomBits and C;
    if B = 0 then    // 增一层的概率是 FLevelProbability
      Inc(Result);
  until B <> 0;

  if Result >= CN_SKIPLIST_MAX_LEVEL then
    Result := CN_SKIPLIST_MAX_LEVEL - 1;
end;

function TCnSkipList.GetCount: Integer;
var
  P: TCnSkipListNode;
begin
  P := FHead;
  Result := 0;
  while P <> nil do
  begin
    Inc(Result);
    P := P.Forwards[0];
  end;
end;

function TCnSkipList.GetMaxLevel: Integer;
begin
  Result := FMaxLevel;
end;

function TCnSkipList.Insert(AValue: Pointer): TCnSkipListNode;
var
  K: Integer;
  P, Q, PreNode, NewNode: TCnSkipListNode;
  Prevs: array[0..CN_SKIPLIST_MAX_LEVEL - 1] of TCnSkipListNode;
begin
  Result := nil;

  PreNode := FHead;
  K := FMaxLevel;

  FillChar(Prevs, SizeOf(Prevs), 0);
  repeat
    Q := PreNode.Forwards[K];
    while ValueGreaterThanNode(AValue, Q) and (Q <> nil) do
    begin
      PreNode := Q;
      Q := PreNode.Forwards[K];
    end;
    Prevs[K] := PreNode;
    Dec(K);
  until K < 0; // 等同于 Search，但也记录了中间链

  if (not FAllowDuplicates) and ValueEqualsNode(AValue, Q) then
    Exit;
  // 新节点要插 P 后面，暂未考虑插 FHead 前面的情况
  
  K := GenerateLevel;
  if K > FMaxLevel then
  begin
    Inc(FMaxLevel);
    K := FMaxLevel;
    Prevs[K] := FHead;

    // FHead 的 Level 需要涨
    if FHead.Level < K then
      FHead.Level := K;
  end;  // Update 是待插入节点的左边一排各个节点，需要按层次指向新增节点

  NewNode := TCnSkipListNode.Create(K);
  Result := NewNode;
  repeat
    P := Prevs[K];                        // 取一个第 K 层的左边节点
    NewNode.Forwards[K] := P.Forwards[K];  // 第 K 层新节点指向原有左节点的下一个
    P.Forwards[K] := NewNode;              // 左节点第 K 层指向新增节点
    Dec(K);
  until K < 0;
end;

function TCnSkipList.Search(AValue: Pointer): TCnSkipListNode;
var
  I: Integer;
  P, Q: TCnSkipListNode;
begin
  Result := nil;
  P := FHead;
  I := FMaxLevel;

  repeat
    Q := P.Forwards[I];
    while ValueGreaterThanNode(AValue, Q) and (Q <> nil) do
    begin
      P := Q;
      Q := P.Forwards[I];
    end;
    Dec(I);
  until I < 0;

  if not ValueEqualsNode(AValue, Q) then
    Exit;

  Result := Q;
end;

function TCnSkipList.ValueEqualsNode(const Value: Pointer;
  const Node: TCnSkipListNode): Boolean;
begin
  if Node = FHead then
    Result := False
  else if Node = nil then
    Result := False
  else
    Result := FCompareFunc(Value, Node) = 0;
end;

function TCnSkipList.ValueGreaterThanNode(const Value: Pointer;
  const Node: TCnSkipListNode): Boolean;
begin
  if Node = FHead then     // 任何子节点永远大于 Head
    Result := True
  else if Node = nil then  // 任何子节点永远小于 nil
    Result := False
  else
    Result := FCompareFunc(Value, Node) > 0;
end;

function TCnSkipList.ValueLessThanNode(const Value: Pointer;
  const Node: TCnSkipListNode): Boolean;
begin
  if Node = FHead then     // Head 永远小于任何子节点
    Result := False
  else if Node = nil then  // nil 永远大于任何节点
    Result := True
  else
    Result := FCompareFunc(Value, Node) < 0;
end;

end.
