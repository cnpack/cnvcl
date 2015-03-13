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

unit CnTree;
{* |<PRE>
================================================================================
* 软件名称：CnPack 公共单元
* 单元名称：实现单根无序树的类单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元为 TCnTree 和 TCnLeaf 的单根无序树的实现单元。
*           类似于 TTreeNodes 和 TTreeNode 的关系，支持深度和广度优先遍历，
*           支持按深度优先的顺序以索引值的形式直接访问各个节点。
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2005.05.08 V1.3 by Alan
*               修正 LoadFromTreeView 方法调用 Clear 方法未考虑 RootLeaf 参数的错误
*           2004.11.02 V1.2
*               加入流化的接口
*           2004.09.04 V1.1
*               加入和 TreeView 交互的功能
*           2004.05.29 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, ComCtrls;

type
  ECnTreeException = class(Exception);

  TCnTree = class;

  TCnLeaf = class(TPersistent)
  private
    FData: Integer;
    FList: TList;
    FParent: TCnLeaf;
    FText: string;
    FTree: TCnTree;
  protected
    function GetAbsoluteIndex: Integer;
    function GetAllCount: Integer;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetIndex: Integer;
    function GetItems(Index: Integer): TCnLeaf;
    function GetLevel: Integer;
      
    procedure DoDepthFirstTravel;
    procedure DoWidthFirstTravel;
    function SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
    {* 将某节点赋值为第 Index 个子节点，返回原节点值 }
  public
    constructor Create(ATree: TCnTree); virtual;
    {* 构造方法，需要一 Tree 做容器 }
    destructor Destroy; override;
    {* 析构方法 }
    function AddChild(ALeaf: TCnLeaf): TCnLeaf;
    {* 添加一指定的子节点作为最后的直属子节点 }
    function AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
    {* 添加一指定的子节点作为第一直属子节点 }
    function InsertChild(ALeaf: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* 在指定的子索引后增加一子节点 }
    procedure Clear;
    {* 清除所有直属子节点，子节点以下也会被删除释放  }
    procedure DeleteChild(AIndex: Integer);
    {* 删除一直属子节点，子节点以下会被删除释放 }
    procedure Delete;
    {* 删除本身和子节点 }
    function ExtractChild(AIndex: Integer): TCnLeaf; overload;
    {* 剥离出一直属子节点，子节点以下不会被删除释放 }
    function ExtractChild(ALeaf: TCnLeaf): TCnLeaf; overload;
    {* 剥离出指定的一子节点，子节点以下不会被删除释放 }

    // 获得其他节点的方法
    function GetFirstChild: TCnLeaf;
    {* 获得第一个直属子节点 }
    function GetLastChild: TCnLeaf;
    {* 获得最后一个直属子节点 }
    function GetNext: TCnLeaf;
    {* 获得第一子节点，如无，则返回同级节点的后一个节点，如无，则返回 nil }
    function GetNextChild(Value: TCnLeaf): TCnLeaf;
    {* 获得某子节点的后一同级节点，无则返回 nil }
    function GetNextSibling: TCnLeaf;
    {* 获得同级的后一子节点，无则返回 nil }
    function GetPrev: TCnLeaf;
    {* 获得同级节点的前一个节点，如无，则返回父节点，如无，则返回 nil }
    function GetPrevChild(Value: TCnLeaf): TCnLeaf;
    {* 获得某一子节点的前一同级节点，无则返回 nil }
    function GetPrevSibling: TCnLeaf;
    {* 获得同级的前一子节点，无则返回 nil }
    function GetAbsoluteItems(AAbsoluteIndex: Integer): TCnLeaf;
    {* 根据深度优先的遍历顺序获得第 n 个子节点，类似于 TreeNodes 中的机制 }

    function HasAsParent(Value: TCnLeaf): Boolean;
    {* 指定的节点是否是本节点的上级或更上级 }
    function IndexOf(ALeaf: TCnLeaf): Integer;
    {* 在直属子节点中查找是否有某一节点并返回其索引 }
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    {* 在整棵树中的索引值 }
    property AllCount: Integer read GetAllCount;
    {* 所有子孙节点数目 }
    property Count: Integer read GetCount;
    {* 直属子节点数目 }
    property HasChildren: Boolean read GetHasChildren;
    {* 是否有子节点 }
    property Index: Integer read GetIndex;
    {* 本叶节点在父节点列表中的顺序索引，从 0 开始。无父则为 -1 }
    property Items[Index: Integer]: TCnLeaf read GetItems; default;
    {* 直属叶节点数组 }

    property Level: Integer read GetLevel;
    {* 本节点层数，树顶节点 Level 为 1 }
    property Parent: TCnLeaf read FParent;
    {* 父节点，不可写 }
    property Tree: TCnTree read FTree;
    {* 所属树，一个叶必须属于一棵树 }
  published
    property Data: Integer read FData write FData;
    {* 用以保存一整数的属性，类似于 Tag }
    property Text: string read FText write FText;
    {* 用以保存一字符串的属性 }
  end;

  ICnTreeFiler = interface(IUnknown)
  {* 用来流化树的接口 }
    ['{E81A9CE0-2D1D-11D9-BA1C-5254AB35836A}']
    procedure LoadFromFile(Instance: TPersistent; const FileName: string);
    procedure SaveToFile(Instance: TPersistent; const FileName: string);
  end;

  TCnLeafClass = class of TCnLeaf;

  TCnTreeNodeEvent = procedure(ALeaf: TCnLeaf; ATreeNode: TTreeNode;
    var Valid: Boolean) of object;

  TCnTree = class(TPersistent)
  private
    FLeafClass: TCnLeafClass;
    FBatchUpdating: Boolean;
    FLeaves: TObjectList;
    FRoot: TCnLeaf;
    FOnWidthFirstTravelLeaf: TNotifyEvent;
    FOnDepthFirstTravelLeaf: TNotifyEvent;
    FOnSaveANode: TCnTreeNodeEvent;
    FOnLoadANode: TCnTreeNodeEvent;
  protected
    function GetRoot: TCnLeaf;
    function GetItems(AbsoluteIndex: Integer): TCnLeaf;
    function GetCount: Integer;
    function GetRegisteredCount: Integer;

    function CreateLeaf(ATree: TCnTree): TCnLeaf; virtual;
    procedure DoDepthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
    procedure DoWidthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
    function DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
    function DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;

    procedure RegisterLeaf(ALeaf: TCnLeaf);
    {* 仅供叶节点调用，在树中登记此叶节点 }
    procedure UnRegisterLeaf(ALeaf: TCnLeaf);
    {* 仅供叶节点调用，取消此叶节点的登记 }

    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* 从一 TreeNode 节点载入其子节点，供递归调用 }
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* 将节点本身以及子节点写入一 TreeNode，供递归调用 }
  public
    constructor Create; overload;
    {* 构造方法 }
    constructor Create(LeafClass: TCnLeafClass); overload;
    {* 另一构造方法}
    destructor Destroy; override;
    {* 析构方法 }
    procedure DepthFirstTravel;
    {* 进行深度优先遍历 }
    procedure WidthFirstTravel;
    {* 进行广度优先遍历 }
    function ExtractLeaf(ALeaf: TCnLeaf): TCnLeaf;
    {* 从树中剥离一叶节点并返回它 }
    procedure Clear;
    {* 清除并释放所有叶节点，批量序次释放，不进行树遍历，不进行通知更新 }

    // 各种添加方法
    function AddChildFirst(AParent: TCnLeaf): TCnLeaf;
    {* 给指定的节点增加一首子节点 }
    function AddChild(AParent: TCnLeaf): TCnLeaf;
    {* 给指定的节点增加一尾子节点 }
    function InsertChild(AParent: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* 给指定的节点增加一指定位置的子节点 }
    function AddFirst(ASibing: TCnLeaf): TCnLeaf;
    {* 给指定的节点增加一同级的最前节点 }
    function Add(ASibing: TCnLeaf): TCnLeaf;
    {* 给指定的节点增加一同级的最后节点 }

    procedure ExchangeWithChild(Leaf1, Leaf2: TCnLeaf); overload;
    {* 交换俩节点位置，带子节点们一起交换 }
    procedure ExchangeWithChild(AbsoluteIndex1, AbsoluteIndex2: Integer); overload;
    {* 交换俩节点位置，带子节点们一起交换 }
    procedure Exchange(Leaf1, Leaf2: TCnLeaf); overload;
    {* 单纯交换俩节点位置 }
    procedure Exchange(AbsoluteIndex1, AbsoluteIndex2: Integer); overload;
    {* 单纯根据索引交换俩节点位置 }

    // 和 TreeView 的交互方法，注意 Root 不参与交互
    procedure LoadFromTreeView(ATreeView: TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil);
    {* 从一 TreeView 读入节点内容。RootNode 的子节点被读入成 RootLeaf 所指明的
    节点的子节点，RootNode 为 nil 表示载入全部 TreeNodes，RootLeaf 为 nil 表示
    载入的为 Tree.Root 的直属节点，也就是所有节点}
    procedure SaveToTreeView(ATreeView: TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil);
    {* 将节点内容写入一 TreeView。 RootLeaf 的子节点被写入成 RootNode 所指明的
    节点的子节点，RootLeaf 为 nil 表示写入 Root 的所有子节点，其实也就是所有节
    点，RootNode 为 nil 表示写入的将成为 TreeView 的根 TreeNodes}

    // 流化方法
    procedure LoadFromFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* 从文件中载入树节点，由提供接口的对象实现 }
    procedure SaveToFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* 将树节点保存至文件，由提供接口的对象实现 }

    property BatchUpdating: Boolean read FBatchUpdating write FBatchUpdating;
    {* 是否在批量更新，为 True 时叶节点释放时不通知 Tree }
    property Root: TCnLeaf read GetRoot;
    {* 根节点，总是存在 }
    property Items[AbsoluteIndex: Integer]: TCnLeaf read GetItems;
    {* 根据深度优先的遍历顺序获得第 n 个子节点，类似于 TreeNodes 中的机制，0 代表 Root }
    property Count: Integer read GetCount;
    {* 返回树中所有节点的数目，包括 Root }
    property RegisteredCount: Integer read GetRegisteredCount;
    {* 返回树中所有注册过的子节点的数目 }
  published
    property OnDepthFirstTravelLeaf: TNotifyEvent read FOnDepthFirstTravelLeaf write FOnDepthFirstTravelLeaf;
    {* 深度优先遍历时遍历到一个叶节点时的触发事件，Sender 是此节点 }
    property OnWidthFirstTravelLeaf: TNotifyEvent read FOnWidthFirstTravelLeaf write FOnWidthFirstTravelLeaf;
    {* 广度优先遍历时遍历到一个叶节点时的触发事件，Sender 是此节点 }
    property OnLoadANode: TCnTreeNodeEvent read FOnLoadANode write FOnLoadANode;
    {* 从 TreeView 中载入节点时针对每一个节点的触发事件 }
    property OnSaveANode: TCnTreeNodeEvent read FOnSaveANode write FOnSaveANode;
    {* 将节点存入 TreeView 时针对每一个节点的触发事件 }
  end;

implementation

//==============================================================================
// TCnLeaf
//==============================================================================

constructor TCnLeaf.Create(ATree: TCnTree);
begin
  inherited Create;
  Assert(ATree <> nil);
  FList := TList.Create;
  FTree := ATree;
  ATree.RegisterLeaf(Self);
end;

destructor TCnLeaf.Destroy;
var
  I: Integer;
begin
  if not FTree.BatchUpdating then
  begin
    for I := FList.Count - 1 downto 0 do
      DeleteChild(I);
    FTree.UnregisterLeaf(Self);
  end;
  FreeAndNil(FList);
  inherited;
end;

function TCnLeaf.AddChild(ALeaf: TCnLeaf): TCnLeaf;
begin
  Assert(ALeaf.Tree = Self.FTree);
  Result := ALeaf;
  FList.Add(Result);
  Result.FParent := Self;
end;

function TCnLeaf.AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
begin
  Assert(ALeaf.Tree = Self.FTree);
  Result := ALeaf;
  FList.Insert(0, Result);
  Result.FParent := Self;
end;

procedure TCnLeaf.Clear;
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
  begin
    TCnLeaf(FList.Items[I]).Free;
    FList.Delete(I);
  end;
end;

procedure TCnLeaf.DeleteChild(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    TCnLeaf(FList.Items[AIndex]).Free;
    FList.Delete(AIndex);
  end;
end;

procedure TCnLeaf.Delete;
begin
  if FParent <> nil then
    FParent.DeleteChild(Index)
  else
    raise ECnTreeException.Create('Root can NOT be deleted.');
end;

function TCnLeaf.ExtractChild(ALeaf: TCnLeaf): TCnLeaf;
var
  AIndex: Integer;
begin
  if ALeaf.HasAsParent(Self) then
  begin
    AIndex := ALeaf.Index;
    Result := ALeaf.Parent.Items[AIndex];
    ALeaf.Parent.FList.Delete(AIndex);
  end
  else
    Result := nil;
end;

function TCnLeaf.ExtractChild(AIndex: Integer): TCnLeaf; 
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    Result := TCnLeaf(Items[AIndex]);
    Result.FParent := nil;
    FList.Delete(AIndex);
  end;
end;

procedure TCnLeaf.DoDepthFirstTravel;
var
  I: Integer;
begin
  if FTree <> nil then
    FTree.DoDepthFirstTravelLeaf(Self);
  for I := 0 to FList.Count - 1 do
    Items[I].DoDepthFirstTravel;
end;

procedure TCnLeaf.DoWidthFirstTravel;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FTree.DoWidthFirstTravelLeaf(TCnLeaf(Items[I]));
  for I := 0 to FList.Count - 1 do
    Items[I].DoWidthFirstTravel;
end;

function TCnLeaf.GetAbsoluteIndex: Integer;
begin
  if FParent <> nil then
    Result := Self.Index + FParent.AbsoluteIndex + 1
  else
    Result := 0;
end;

function TCnLeaf.GetAbsoluteItems(AAbsoluteIndex: Integer): TCnLeaf;
var
  I, ACount, IndexCount: Integer;
begin
  Result := nil;
  if AAbsoluteIndex < 0 then
    Exit
  else
  begin
    IndexCount := 0;
    for I := 0 to Count - 1 do
    begin
      if IndexCount = AAbsoluteIndex then
      begin
        Result := Items[I];
        Exit;
      end;

      ACount := Items[I].AllCount + 1;
      if IndexCount + ACount > AAbsoluteIndex then
      begin
        Result := Items[I].GetAbsoluteItems(AAbsoluteIndex - IndexCount - 1);
        Exit;
      end
      else
        Inc(IndexCount, ACount);
    end;
  end;
end;

function TCnLeaf.GetAllCount: Integer;
var
  I: Integer;
begin
  Result := Count;
  for I := 0 to Self.Count - 1 do
    Result := Result + Self.Items[I].AllCount;
end;

function TCnLeaf.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCnLeaf.GetFirstChild: TCnLeaf;
begin
  if HasChildren then
    Result := TCnLeaf(FList.Items[0])
  else
    Result := nil;
end;

function TCnLeaf.GetHasChildren: Boolean;
begin
  Result := FList.Count > 0;
end;

function TCnLeaf.GetIndex: Integer;
begin
  if FParent <> nil then
    Result := FParent.IndexOf(Self)
  else
    Result := -1;
end;

function TCnLeaf.GetItems(Index: Integer): TCnLeaf;
begin
  Result := TCnLeaf(FList.Items[Index]);
end;

function TCnLeaf.GetLastChild: TCnLeaf;
begin
  if HasChildren then
    Result := TCnLeaf(FList.Items[Count - 1])
  else
    Result := nil;
end;

function TCnLeaf.GetLevel: Integer;
begin
  if FParent = nil then
    Result := 1
  else
    Result := FParent.Level + 1;
end;

function TCnLeaf.HasAsParent(Value: TCnLeaf): Boolean;
var
  AParent: TCnLeaf;
begin
  Result := False;
  if Value.Tree <> Self.Tree then
    Exit;
    
  AParent := FParent;
  while AParent <> nil do
  begin
    if AParent = Value then
    begin
      Result := True;
      Exit;
    end
    else
      AParent := AParent.Parent;
  end;
end;

function TCnLeaf.IndexOf(ALeaf: TCnLeaf): Integer;
begin
  Result := FList.IndexOf(ALeaf);
end;

function TCnLeaf.GetNext: TCnLeaf;
begin
  Result := GetFirstChild;
  if Result = nil then
    Result := GetNextSibling;
end;

function TCnLeaf.GetNextChild(Value: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if Value.Parent = Self then
    if Value.Index < Self.Count - 1 then
      Result := Items[Value.Index + 1];
end;

function TCnLeaf.GetNextSibling: TCnLeaf;
begin
  Result := nil;
  if Parent <> nil then
    if Index < Parent.Count - 1 then
      Result := Parent.Items[Index + 1];
end;

function TCnLeaf.GetPrev: TCnLeaf;
begin
  Result := GetPrevSibling;
  if Result = nil then
    Result := Parent;
end;

function TCnLeaf.GetPrevChild(Value: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if Value.Parent = Self then
    if Value.Index > 0 then
      Result := Items[Value.Index - 1];
end;

function TCnLeaf.GetPrevSibling: TCnLeaf;
begin
  Result := nil;
  if Parent <> nil then
    if Index > 0 then
      Result := Parent.Items[Index - 1];
end;

function TCnLeaf.SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
begin
  if (ALeaf <> nil) and (ALeaf.Tree = Self.FTree) and
    (Index >= 0) and (Index < Count) then
  begin
    Result := FList.Items[Index];
    FList.Items[Index] := ALeaf;
    ALeaf.FParent := Self;
  end
  else
    Result := nil;
end;

//==============================================================================
// TCnTree
//==============================================================================

constructor TCnTree.Create;
begin
  inherited;
  FLeaves := TObjectList.Create(True);
  if FLeafClass = nil then
    FLeafClass := TCnLeaf;
  FRoot := CreateLeaf(Self);
end;

constructor TCnTree.Create(LeafClass: TCnLeafClass);
begin
  FLeafClass := LeafClass;
  Create;
end;

destructor TCnTree.Destroy;
begin
  FBatchUpdating := True;
  FLeaves.Free;
  inherited;
end;

procedure TCnTree.DepthFirstTravel;
begin
  FRoot.DoDepthFirstTravel;
end;

function TCnTree.CreateLeaf(ATree: TCnTree): TCnLeaf;
begin
  try
    Result := TCnLeaf(FLeafClass.NewInstance);
    Result.Create(ATree);
  except
    Result := nil;
  end;
end;

procedure TCnTree.DoDepthFirstTravelLeaf(ALeaf: TCnLeaf);
begin
  if Assigned(FOnDepthFirstTravelLeaf) then
    FOnDepthFirstTravelLeaf(ALeaf);
end;

procedure TCnTree.DoWidthFirstTravelLeaf(ALeaf: TCnLeaf);
begin
  if Assigned(FOnWidthFirstTravelLeaf) then
    FOnWidthFirstTravelLeaf(ALeaf);
end;

function TCnTree.GetRoot: TCnLeaf;
begin
  Result := FRoot;
end;

procedure TCnTree.RegisterLeaf(ALeaf: TCnLeaf);
begin
  if FLeaves.IndexOf(ALeaf) < 0 then
    FLeaves.Add(ALeaf);
end;

procedure TCnTree.WidthFirstTravel;
begin
  DoWidthFirstTravelLeaf(FRoot);
  FRoot.DoWidthFirstTravel;
end;

procedure TCnTree.UnRegisterLeaf(ALeaf: TCnLeaf);
begin
  FLeaves.Extract(ALeaf);
end;

procedure TCnTree.Clear;
begin
  FBatchUpdating := True;
  try
    FLeaves.Clear;
    // FRoot 已经由 Fleaves 释放，无须再次释放.
    FRoot := CreateLeaf(Self);
  finally
    FBatchUpdating := False;
  end;
end;

function TCnTree.ExtractLeaf(ALeaf: TCnLeaf): TCnLeaf;
begin
  Result := nil;
  if ALeaf.Tree = Self then
  begin
    Self.UnRegisterLeaf(ALeaf);
    if ALeaf.Parent <> nil then
      Result := ALeaf.Parent.ExtractChild(ALeaf.Index);
  end;
end;

function TCnLeaf.InsertChild(ALeaf: TCnLeaf; AIndex: Integer): TCnLeaf;
begin
  if (ALeaf <> nil) and (AIndex >= 0) and (AIndex <= Count) then
  begin
    Result := ALeaf;
    FList.Insert(AIndex, ALeaf);
    ALeaf.FParent := Self;
  end
  else
    Result := nil;
end;

function TCnTree.AddChild(AParent: TCnLeaf): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    AParent.AddChild(Result);
  end
  else
    Result := nil;
end;

function TCnTree.AddChildFirst(AParent: TCnLeaf): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    AParent.AddChildFirst(Result);
  end
  else
    Result := nil;
end;

function TCnTree.InsertChild(AParent: TCnLeaf; AIndex: Integer): TCnLeaf;
begin
  if AParent.Tree = Self then
  begin
    Result := CreateLeaf(Self);
    if AParent.InsertChild(Result, AIndex) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TCnTree.AddFirst(ASibing: TCnLeaf): TCnLeaf;
begin
  if (ASibing <> nil) and (ASibing.Tree = Self) and (ASibing.Parent <> nil) then
  begin
    Result := CreateLeaf(Self);
    if ASibing.Parent.AddChildFirst(Result) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

function TCnTree.Add(ASibing: TCnLeaf): TCnLeaf;
begin
  if (ASibing <> nil) and (ASibing.Tree = Self) and (ASibing.Parent <> nil) then
  begin
    Result := CreateLeaf(Self);
    if ASibing.Parent.AddChild(Result) = nil then
    begin
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

procedure TCnTree.Exchange(AbsoluteIndex1, AbsoluteIndex2: Integer); 
begin
  Exchange(Items[AbsoluteIndex1], Items[AbsoluteIndex2]);
end;

procedure TCnTree.ExchangeWithChild(AbsoluteIndex1,
  AbsoluteIndex2: Integer);
begin
  ExchangeWithChild(Items[AbsoluteIndex1], Items[AbsoluteIndex2]);
end;

procedure TCnTree.ExchangeWithChild(Leaf1, Leaf2: TCnLeaf); 
var
  Parent2: TCnLeaf;
  Index2: Integer;
begin
  if (Leaf1 <> nil) and (Leaf2 <> nil) and (Leaf1 <> Leaf2)
    and (Leaf1.Tree = Self) and (Leaf2.Tree = Self) then
  begin
    if Leaf1.HasAsParent(Leaf2) or Leaf2.HasAsParent(Leaf1) then
      Exit; // 为父子关系的不允许交换
    Parent2 := Leaf2.Parent;
    Index2 := Leaf2.Index;

    Leaf1.Parent.SetChild(Leaf2, Leaf1.Index);
    Parent2.SetChild(Leaf1, Index2);
  end;
end;

procedure TCnTree.Exchange(Leaf1, Leaf2: TCnLeaf); 
var
  Parent2: TCnLeaf;
  I, Index2: Integer;
  AList: TList;
begin
  if (Leaf1 <> nil) and (Leaf2 <> nil) and (Leaf1 <> Leaf2)
    and (Leaf1.Tree = Self) and (Leaf2.Tree = Self) then
  begin
    // 自身交换父节点和子节点列表，父节点交换两引用
    Parent2 := Leaf2.Parent;
    Index2 := Leaf2.Index;

    AList := nil;
    try
      AList := TList.Create;
      for I := 0 to Leaf1.Count - 1 do
        AList.Add(Leaf1.Items[I]);

      Leaf1.FList.Clear;
      for I := 0 to Leaf2.Count - 1 do
        Leaf1.FList.Add(Leaf2.Items[I]);
      for I := 0 to AList.Count - 1 do
        Leaf2.FList.Add(AList.Items[I]);
    finally
      AList.Free;
    end;

    if Leaf1.Parent <> nil then
      Leaf1.Parent.SetChild(Leaf2, Leaf1.Index)
    else
      Leaf2.FParent := nil;
    if Parent2 <> nil then
      Parent2.SetChild(Leaf1, Index2)
    else
      Leaf1.FParent := nil;

    // 顺便判断根节点
    if FRoot = Leaf1 then
      FRoot := Leaf2
    else if FRoot = Leaf2 then
      FRoot := Leaf1;
  end;
end;

function TCnTree.GetItems(AbsoluteIndex: Integer): TCnLeaf;
begin
  if AbsoluteIndex < 0 then
    Result := nil
  else if AbsoluteIndex = 0 then
    Result := FRoot
  else
    Result := FRoot.GetAbsoluteItems(AbsoluteIndex - 1);
end;

function TCnTree.GetCount: Integer;
begin
  Result := FRoot.AllCount + 1;
end;

function TCnTree.GetRegisteredCount: Integer;
begin
  Result := FLeaves.Count;
end;

procedure TCnTree.LoadFromTreeView(ATreeView: TTreeView; RootNode: TTreeNode;
  RootLeaf: TCnLeaf);
var
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;
      
    if ATreeView.Items.Count > 0 then
    begin
      if RootNode = nil then
        ANode := ATreeView.Items[0]
      else
        ANode := RootNode;
      // 第一个节点
      if RootLeaf = nil then
        RootLeaf := FRoot;

      ALeaf := Self.AddChild(RootLeaf);
      LoadFromATreeNode(ALeaf, ANode);
      if RootNode <> nil then Exit;
      // 声明了 RootNode 时以 RootNode 为根，所以不处理 RootNode 的同层节点

      ANode := ANode.GetNextSibling; // 遍历此层的其他后继节点
      while ANode <> nil do
      begin
        ALeaf := Self.AddChild(RootLeaf);
        LoadFromATreeNode(ALeaf, ANode);
        ANode := ANode.GetNextSibling;
      end;
    end;
  end;
end;

procedure TCnTree.SaveToTreeView(ATreeView: TTreeView; RootNode: TTreeNode;
  RootLeaf: TCnLeaf);
var
  I: Integer;
  ANode: TTreeNode;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootNode <> nil) and (RootNode.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    ATreeView.Items.BeginUpdate;
    try
      if RootNode <> nil then
        RootNode.DeleteChildren
      else
        ATreeView.Items.Clear;

      if RootLeaf = nil then
        RootLeaf := Self.FRoot;
      if RootLeaf.Count > 0 then
      begin
        ANode := RootNode;
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf 的子节点，RootLeaf 不参与交互
          ANode := ATreeView.Items.Add(ANode, '');
          SaveToATreeNode(ALeaf, ANode);
        end;
      end;
    finally
      ATreeView.Items.EndUpdate;
    end;
  end;
end;

procedure TCnTree.LoadFromFile(Filer: ICnTreeFiler;
  const FileName: string);
begin
  if Filer <> nil then
    Filer.LoadFromFile(Self, FileName);
end;

procedure TCnTree.SaveToFile(Filer: ICnTreeFiler; const FileName: string);
begin
  if Filer <> nil then
    Filer.SaveToFile(Self, FileName);
end;

procedure TCnTree.LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
var
  I: Integer;
  Leaf: TCnLeaf;
begin
  if (ANode <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeNode(ALeaf, ANode) then
    begin
      for I := 0 to ANode.Count - 1 do
      begin
        Leaf := Self.AddChild(ALeaf);
        LoadFromATreeNode(Leaf, ANode.Item[I]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnTree.SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
var
  I: Integer;
  Node: TTreeNode;
begin
  if (ANode <> nil) and (ALeaf <> nil) and (ANode.TreeView is TTreeView) then
  begin
    if DoSaveToATreeNode(ALeaf, ANode) then
    begin
      for I := 0 to ALeaf.Count - 1 do
      begin
        Node := (ANode.TreeView as TTreeView).Items.AddChild(ANode, '');
        SaveToATreeNode(ALeaf.Items[I], Node);
      end;
    end
    else
    begin
      ANode.Delete;
    end;
  end;
end;

function TCnTree.DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnLoadANode) then
    FOnLoadANode(ALeaf, ANode, Result)
  else
  begin
    ALeaf.Text := ANode.Text;
    ALeaf.Data := Integer(ANode.Data);
  end;
end;

function TCnTree.DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean;
begin
  Result := True;
  if Assigned(FOnSaveANode) then
  begin
    FOnSaveANode(ALeaf, ANode, Result);
  end
  else
  begin
    ANode.Text := ALeaf.Text;
    ANode.Data := Pointer(ALeaf.Data);
  end;
end;

end.
