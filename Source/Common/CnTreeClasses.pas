{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnTreeClasses;
{* |<PRE>
================================================================================
* 软件名称：CnPack 公共单元
* 单元名称：实现二叉树、二叉排序树、红黑树、字典搜索树的类单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：该单元为基于 TCnTree 和 TCnLeaf 的子类
*           二叉树 TCnBinaryTree/Leaf、字典搜索树 TCnTrieTree/Leaf。
*           TCnTree/Leaf 类似于 TTreeNodes/Node 的关系，支持深度和广度优先遍历，
*           支持按深度优先的顺序以索引值的形式直接访问各个节点。
*           红黑树尚未完全实现，不可用。
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6 + 10.3.1
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.09.10 V1.0 by LiuXiao
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// 如需要启用和 TreeView 交互的功能，需在工程选项中定义 ENABLE_UIINTERACT

// 用 ENABLE_FMX 来控制 FMX 环境下是否支持 FMX
{$IFNDEF ENABLE_FMX}
  {$UNDEF SUPPORT_FMX}
{$ENDIF}
  
uses
  SysUtils, Classes, Contnrs {$IFDEF MSWINDOWS}, ComCtrls {$ENDIF}
  {$IFDEF SUPPORT_FMX}, FMX.TreeView {$ENDIF}, Math, CnTree;
  // If ComCtrls not found, please add 'Vcl' to 'Unit Scope Names' in Project Options.

type
//==============================================================================
// 二叉树的实现
//==============================================================================

  ECnBinaryTreeException = class(Exception);

  TCnBinaryTree = class;

  TCnBinaryLeaf = class(TCnLeaf)
  {* 二叉树节点子类，有左右子节点的封装}
  private
    function GetLeftLeaf: TCnBinaryLeaf;
    function GetRightLeaf: TCnBinaryLeaf;
    procedure SetLeftLeaf(const Value: TCnBinaryLeaf);
    procedure SetRightLeaf(const Value: TCnBinaryLeaf);
    function GetTree: TCnBinaryTree;
    function GetParent: TCnBinaryLeaf;
    procedure SetParent(const Value: TCnBinaryLeaf);
  protected
    function GetSubTreeHeight: Integer; override;

    procedure DoPreOrderTravel;
    procedure DoInOrderTravel;
    procedure DoPostOrderTravel;

    function GetMostLeftLeaf: TCnBinaryLeaf;
    {* 递归获取左子树的最左深度子节点}
    function GetMostRightLeaf: TCnBinaryLeaf;
    {* 递归获取右子树的最右深度子节点}
  public
    constructor Create(ATree: TCnTree); override;
    function IsBalance: Boolean;
    {* 以此节点为根节点的子二叉树是否是平衡二叉树}

    function AddLeftChild: TCnBinaryLeaf;
    {* 增加左子节点，如已存在则返回 nil}
    function AddRightChild: TCnBinaryLeaf;
    {* 增加右子节点，如已存在则返回 nil}
    procedure DeleteLeftChild;
    {* 删除左子节点，也就是置 nil}
    procedure DeleteRightChild;
    {* 删除右子节点，也就是置 nil}

    function GetMostRightLeafFromLeft: TCnBinaryLeaf;
    {* 获取左子树中最靠右的节点，也即中序遍历的前驱节点}
    function GetMostLeftLeafFromRight: TCnBinaryLeaf;
    {* 获取右子树中最靠左的节点，也即中序遍历的后驱节点}
    function GetBrotherLeaf: TCnBinaryLeaf;
    {* 获取兄弟节点，也就是父节点的另一子节点}
    function GetUncleLeaf: TCnBinaryLeaf;
    {* 获取叔伯节点，也就是父节点的父节点的另一子节点}
    function GetGrandLeaf: TCnBinaryLeaf;
    {* 获取父节点的父节点}

    property Parent: TCnBinaryLeaf read GetParent write SetParent;
    {* 父节点}
    property LeftLeaf: TCnBinaryLeaf read GetLeftLeaf write SetLeftLeaf;
    {* 左子节点，使用第 0 个子节点，无则返回 nil，设置时注意原有节点不会释放，需要自行处理}
    property RightLeaf: TCnBinaryLeaf read GetRightLeaf write SetRightLeaf;
    {* 右子节点，使用第 1 个子节点，无则返回 nil，设置时注意原有节点不会释放，需要自行处理}

    property Tree: TCnBinaryTree read GetTree;
    {* 所属树，一个叶必须属于一棵树}
  end;

  TCnBinaryLeafClass = class of TCnBinaryLeaf;

  TCnBinaryTree = class(TCnTree)
  {* 二叉树实现类}
  private
    FOnPostOrderTravelLeaf: TNotifyEvent;
    FOnInOrderTravelLeaf: TNotifyEvent;
    FOnPreOrderTravelLeaf: TNotifyEvent;
    procedure SetRoot(const Value: TCnBinaryLeaf);
  protected
    function GetHeight: Integer; override;  
    function DefaultLeafClass: TCnLeafClass; override;
    procedure ValidateComingLeaf(AParent, AChild: TCnLeaf); override;

    function GetRoot: TCnBinaryLeaf;
    function GetCount: Integer;

    procedure DoPreOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoInOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoPostOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;

    procedure ReplaceLeaf(ALeaf, AChild: TCnBinaryLeaf);
    {* 用 AChild 取代 ALeaf 所在的节点，ALeaf 剥离}

{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); override;
    {* 从一 VCL 的 TreeNode 节点载入其子节点，供递归调用，较基类增加了俩子节点的限制 }
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); override;
    {* 将节点本身以及子节点写入一 VCL 的 TreeNode，供递归调用 }
  {$ENDIF}

  {$IFDEF SUPPORT_FMX}
    procedure LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); override;
    {* 从一 FMX 的 TreeViewItem 节点载入其子节点，供递归调用，较基类增加了俩子节点的限制 }
    procedure SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); override;
    {* 将节点本身以及子节点写入一 FMX 的 TreeViewItem，供递归调用 }
  {$ENDIF}
{$ENDIF}
  public
    constructor Create; overload;
    {* 构造方法 }
    constructor Create(LeafClass: TCnBinaryLeafClass); overload;
    {* 另一构造方法}

    function AddLeftChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
    {* 替指定节点增加左子节点，如已存在则返回 nil}
    function AddRightChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
    {* 替指定节点增加右子节点，如已存在则返回 nil}
    procedure DeleteLeftChild(AParent: TCnBinaryLeaf);
    {* 删除指定节点的左子节点，也就是置 nil，但原有子节点不会释放}
    procedure DeleteRightChild(AParent: TCnBinaryLeaf);
    {* 删除指定节点的右子节点，也就是置 nil，但原有子节点不会释放}

{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    // 和 TreeView 的交互方法，注意 Root 不参与交互
    procedure LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* 从一 VCL 的 TreeView 读入节点内容。RootNode 的子节点被读入成 RootLeaf 所指明的
    节点的子节点，RootNode 为 nil 表示从根扫描全部 TreeNodes，RootLeaf 为 nil 表示
    载入的为 Tree.Root 的直属节点，也就是所有节点。
    对于任意一个 TreeNode，其第一个子节点作为左子树，第二个作为右子树，超过二个的忽略}
    procedure SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* 将节点内容写入一 VCL 的 TreeView。 RootLeaf 的子节点被写入成 RootNode 所指明的
    节点的子节点，RootLeaf 为 nil 表示写入 Root 的所有子节点，其实也就是所有节
    点，RootNode 为 nil 表示写入的将成为 TreeView 的根 TreeNodes}
  {$ENDIF}

  {$IFDEF SUPPORT_FMX}
    procedure LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* 从一 FMX 的 TreeView 读入节点内容。RootNode 的子节点被读入成 RootLeaf 所指明的
    节点的子节点，RootItem 为 nil 表示从根扫描全部 TreeNodes，RootLeaf 为 nil 表示
    载入的为 Tree.Root 的直属节点，也就是所有节点。
    对于任意一个 TreeNode，其第一个子节点作为左子树，第二个作为右子树，超过二个的忽略}
    procedure SaveToTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnBinaryLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* 将节点内容写入一 FMX 的 TreeView。 RootLeaf 的子节点被写入成 RootItem 所指明的
    节点的子节点，RootLeaf 为 nil 表示写入 Root 的所有子节点，其实也就是所有节
    点，RootNode 为 nil 表示写入的将成为 TreeView 的根 TreeNodes}
  {$ENDIF}
{$ENDIF}

    function IsFull: Boolean;
    {* 是否是满二叉树，所有底层叶节点均全满并且层次相同}
    function IsComplete: Boolean;
    {* 是否是完全二叉树}
    function IsBalance: Boolean;
    {* 是否是平衡二叉树}

    procedure PreOrderTravel;
    {* 先根次序遍历，中左右}
    procedure InOrderTravel;
    {* 中根次序遍历，左中右}
    procedure PostOrderTravel;
    {* 后根次序遍历，左右中}

    property Root: TCnBinaryLeaf read GetRoot write SetRoot;
    {* 根节点，总是存在，但二叉树里允许外部设置}
    property Count: Integer read GetCount;
    {* 返回树中所有节点的数目，包括 Root }
    property Height: Integer read GetHeight;
    {* 树高度，只有根节点时为 1}

    property OnPreOrderTravelLeaf: TNotifyEvent read FOnPreOrderTravelLeaf
      write FOnPreOrderTravelLeaf;
    {* 先根次序遍历时触发的事件}
    property OnInOrderTravelLeaf: TNotifyEvent read FOnInOrderTravelLeaf
      write FOnInOrderTravelLeaf;
    {* 中根次序遍历时触发的事件}
    property OnPostOrderTravelLeaf: TNotifyEvent read FOnPostOrderTravelLeaf
      write FOnPostOrderTravelLeaf;
    {* 后根次序遍历时触发的事件}
  end;

//==============================================================================
// 排序二叉树的实现
//==============================================================================

  TCnBinarySortCompareEvent = function (Value: Integer; Leaf: TCnBinaryLeaf): Integer of object;
  {* 排序二叉树的比较事件}
  TCnBinarySortSetLeafEvent = procedure (Leaf: TCnBinaryLeaf; Value: Integer) of object;
  {* 排序二叉树的设置子节点事件}

  TCnBinarySortTree = class(TCnBinaryTree)
  {* 排序二叉树的实现}
  private
    FRootInserted: Boolean;
    FOnCompare: TCnBinarySortCompareEvent;
    FOnSetLeaf: TCnBinarySortSetLeafEvent;
    procedure CheckCompareSetLeaf;
    function InternalSearch(Leaf: TCnBinaryLeaf; Value: Integer): TCnBinaryLeaf;
    function DefaultOnCompare(Value: Integer; Leaf: TCnBinaryLeaf): Integer;
    procedure DefaultOnSetLeaf(Leaf: TCnBinaryLeaf; Value: Integer);
  protected
    function IsEmpty: Boolean;
    function InternalInsert(Leaf: TCnBinaryLeaf; Value: Integer): TCnBinaryLeaf; virtual;
    function InternalDelete(Leaf: TCnBinaryLeaf; Value: Integer): Boolean; virtual;
  public
    constructor Create; overload;
    {* 构造方法 }
    constructor Create(LeafClass: TCnBinaryLeafClass); overload;
    {* 另一构造方法}

    procedure Clear; override;
    {* 清除所有子节点包括 Root}

    function Insert(Value: Integer): TCnBinaryLeaf; virtual;
    {* 插入指定值的节点，插入时默认 Value 会放到 Leaf 的 Data 属性中，返回该节点}
    function Search(Value: Integer): TCnBinaryLeaf;
    {* 查找指定值的节点，未找到则返回 nil}
    function Delete(Value: Integer): Boolean; virtual;
    {* 删除指定值，返回删除成功与否}
    property OnSetLeaf: TCnBinarySortSetLeafEvent read FOnSetLeaf write FOnSetLeaf;
    {* 插入节点时将值赋值给节点时触发，默认为设置 Text 与 Data}
    property OnCompare: TCnBinarySortCompareEvent read FOnCompare write FOnCompare;
    {* 查找时比较触发，默认为 Value 与 Data 比较大小}
  end;

//==============================================================================
// 红黑树的实现
//==============================================================================

  TCnRedBlackTree = class;

  TCnRedBlackLeaf = class(TCnBinaryLeaf)
  {* 红黑树节点子类，继承于普通二叉树节点}
  private
    FIsRed: Boolean;
    function GetLeftLeaf: TCnRedBlackLeaf;
    function GetRightLeaf: TCnRedBlackLeaf;
    procedure SetLeftLeaf(const Value: TCnRedBlackLeaf);
    procedure SetRightLeaf(const Value: TCnRedBlackLeaf);
    function GetTree: TCnRedBlackTree;
    function GetParent: TCnRedBlackLeaf;
    procedure SetParent(const Value: TCnRedBlackLeaf);
  protected
    function GetMostLeftLeaf: TCnRedBlackLeaf;
    {* 递归获取左子树的最左深度子节点}
    function GetMostRightLeaf: TCnRedBlackLeaf;
    {* 递归获取右子树的最右深度子节点}
  public
    constructor Create(ATree: TCnTree); override;

    function AddLeftChild: TCnRedBlackLeaf;
    {* 增加左子节点，如已存在则返回 nil}
    function AddRightChild: TCnRedBlackLeaf;
    {* 增加右子节点，如已存在则返回 nil}

    function GetMostRightLeafFromLeft: TCnRedBlackLeaf;
    {* 获取左子树中最靠右的节点，也即中序遍历的前驱节点}
    function GetMostLeftLeafFromRight: TCnRedBlackLeaf;
    {* 获取右子树中最靠左的节点，也即中序遍历的后驱节点}
    function GetBrotherLeaf: TCnRedBlackLeaf;
    {* 获取兄弟节点，也就是父节点的另一子节点}
    function GetUncleLeaf: TCnRedBlackLeaf;
    {* 获取叔伯节点，也就是父节点的父节点的另一子节点}
    function GetGrandLeaf: TCnRedBlackLeaf;
    {* 获取父节点的父节点}

    property Parent: TCnRedBlackLeaf read GetParent write SetParent;
    {* 父节点}
    property LeftLeaf: TCnRedBlackLeaf read GetLeftLeaf write SetLeftLeaf;
    {* 左子节点，使用第 0 个子节点，无则返回 nil，设置时注意原有节点不会释放，需要自行处理}
    property RightLeaf: TCnRedBlackLeaf read GetRightLeaf write SetRightLeaf;
    {* 右子节点，使用第 1 个子节点，无则返回 nil，设置时注意原有节点不会释放，需要自行处理}

    property IsRed: Boolean read FIsRed write FIsRed;
    {* 子节点颜色是红还是黑}
    property Tree: TCnRedBlackTree read GetTree;
    {* 所属树，一个叶必须属于一棵树}
  end;

  TCnRedBlackLeafClass = class of TCnRedBlackLeaf;

  TCnRedBlackTree = class(TCnBinarySortTree)
  {* 红黑树的实现类}
  private
    procedure SetRoot(const Value: TCnRedBlackLeaf);
  protected
    function DefaultLeafClass: TCnLeafClass; override;
    function GetRoot: TCnRedBlackLeaf;
    procedure RotateLeft(ALeaf: TCnRedBlackLeaf);
    {* 对一个节点及其右子节点实施左旋}
    procedure RotateRight(ALeaf: TCnRedBlackLeaf);
    {* 对一个节点及其左子节点实施左旋}

    procedure InsertRepair(ALeaf: TCnRedBlackLeaf);
    {* 插入后的修正}
    procedure InsertRepairCase3(ALeaf: TCnRedBlackLeaf);
    procedure InsertRepairCase4(ALeaf: TCnRedBlackLeaf);
    procedure InsertRepairCase4Step2(ALeaf: TCnRedBlackLeaf);

    procedure DeleteOneChildLeaf(ALeaf: TCnRedBlackLeaf);
    {* 删除一个只带一个子节点的节点}
    procedure DeleteCase1(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase2(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase3(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase4(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase5(ALeaf: TCnRedBlackLeaf);
    procedure DeleteCase6(ALeaf: TCnRedBlackLeaf);
  public
    constructor Create; overload;
    {* 构造方法 }
    constructor Create(LeafClass: TCnRedBlackLeafClass); overload;
    {* 另一构造方法}

    function Insert(Value: Integer): TCnRedBlackLeaf; reintroduce;
    {* 插入一个节点，返回插入的节点，内部自动做好着色与旋转等操作}
    function Delete(Value: Integer): Boolean; reintroduce;
    {* 删除指定值，返回删除成功与否，内部自动做好着色与旋转等操作}

    property Root: TCnRedBlackLeaf read GetRoot write SetRoot;
    {* 根节点，总是存在}
  end;

//==============================================================================
// 字典树的实现
//==============================================================================

  TCnTrieTree = class;

  TCnTrieLeaf = class(TCnLeaf)
  {* 字典树的树叶类，使用 Data 存储前缀字符}
  private
    FCharacter: Char;
    function GetCharacter: Char;
    procedure SetCharacter(const Value: Char);
    function GetTree: TCnTrieTree;
  protected
    function GetItems(Index: Integer): TCnTrieLeaf;
    procedure SetItems(Index: Integer; const Value: TCnTrieLeaf);

    function DoInsertChar(P: PChar): TCnTrieLeaf;
    function DoSearchChar(P: PChar): TCnTrieLeaf;
  public
    property Character: Char read GetCharacter write SetCharacter;
    property Items[Index: Integer]: TCnTrieLeaf read GetItems write SetItems; default;
    {* 转换了类型的直属叶节点数组 }
    property Tree: TCnTrieTree read GetTree;
    {* 转换了类型的所属树，一个叶必须属于一棵树 }
  end;

  TCnTrieTree = class(TCnTree)
  {* 字典树实现类}
  private
    FCaseSensitive: Boolean;
    FOnlyChar: Boolean;
    FAnsiFastMode: Boolean;
    function ConvertCharWithCase(C: Char): Char; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 根据大小写设置，返回原字符或大写字母（如果是小写字母）}
  protected
    function GetRoot: TCnTrieLeaf;
    function DefaultLeafClass: TCnLeafClass; override;
    function CreateTrieLeaf: TCnTrieLeaf;
  public
    constructor Create(ACaseSensitive: Boolean = True;
      AOnlyChar: Boolean = False; AnAnsiFastMode: Boolean = False); reintroduce; virtual;
    {* 字典树构造函数，参数指定：
      ACaseSensitive：是否区分大小写
      AOnlyChar：节点上是否只存储字母，如为否，则节点的 Text 属性会存储字符串值
      AnAnsiFastMode：是否使用 Ansi 快速模式，在 Unicode 编译环境下此属性无效
        如为 True，则节点将提前创建 256 个空白子节点供快速搜索，否则线性增序存储}

    function InsertString(const Str: string): TCnTrieLeaf;
    {* 插入字符串，返回插入的叶节点供外界设置内容，如果已存在则返回 nil}
    function SearchString(const Str: string): TCnTrieLeaf;
    {* 查找字符串，返回查找到的叶节点，如果未找到则返回 nil}
    function StringExists(const Str: string): Boolean;
    {* 查找字符串，返回是否存在}

    property Root: TCnTrieLeaf read GetRoot;
    {* 根节点 }
    property OnlyChar: Boolean read FOnlyChar;
    {* 是否只存储字母}
    property CaseSensitive: Boolean read FCaseSensitive;
    {* 是否区分大小写。注意，如果不区分大小写且 OnlyChar 为 False，
       查找到的节点其存储的字符串内容可能不符合存入时的大小写情况}
    property AnsiFastMode: Boolean read FAnsiFastMode;
    {* Ansi 模式下是否预先创建 255 个子节点供快速定位，否则使用线性搜索}
  end;

implementation

//==============================================================================
// TCnBinaryTree
//==============================================================================

constructor TCnBinaryTree.Create;
begin
  inherited;

end;

function TCnBinaryTree.AddLeftChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
begin
  if (AParent.Tree = Self) and (AParent.LeftLeaf = nil) then
  begin
    Result := TCnBinaryLeaf(CreateLeaf(Self));
    AParent.LeftLeaf := Result;
  end
  else
    Result := nil;
end;

function TCnBinaryTree.AddRightChild(AParent: TCnBinaryLeaf): TCnBinaryLeaf;
begin
  if (AParent.Tree = Self) and (AParent.RightLeaf = nil) then
  begin
    Result := TCnBinaryLeaf(CreateLeaf(Self));
    AParent.RightLeaf := Result;
  end
  else
    Result := nil;
end;

constructor TCnBinaryTree.Create(LeafClass: TCnBinaryLeafClass);
begin
  inherited Create(LeafClass);
end;

function TCnBinaryTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnBinaryLeaf;
end;

function TCnBinaryTree.IsBalance: Boolean;
begin
  if Root = nil then
    Result := True
  else
    Result := Root.IsBalance;
end;

function TCnBinaryTree.IsComplete: Boolean;
var
  Queue: TQueue;
  Node: TCnBinaryLeaf;
begin
  Result := True;
  Queue := TQueue.Create;
  try
    Queue.Push(Root);
    Node := TCnBinaryLeaf(Queue.Pop);
    while Node <> nil do
    begin
      Queue.Push(Node.LeftLeaf);
      Queue.Push(Node.RightLeaf);
      Node := TCnBinaryLeaf(Queue.Pop);
    end;

    // 进行广度优先遍历，第一次碰到 Node 是 nil 时，它上一层的节点的后续子节点已经都进了队列
    // 此时找队列中的非 nil 点，如果有，说明非完全

    if Queue.Count = 0 then // 如果所有遍历的节点都不是 nil，说明可能是满二叉树？
      Exit;

    // 此时碰到 nil 了，找队列里的剩余节点
    while Queue.Count > 0 do
    begin
      Node := TCnBinaryLeaf(Queue.Pop);
      if Node <> nil then // 如果还有，则不是完全二叉树
      begin
        Result := False;
        Exit;
      end;
    end;
  finally
    Queue.Free;
  end;
end;

function TCnBinaryTree.IsFull: Boolean;
var
  Deep: Integer;
begin
  Deep := MaxLevel + 1;
  Result := Count = Power(2, Deep - 1);
end;

procedure TCnBinaryTree.ValidateComingLeaf(AParent, AChild: TCnLeaf);
begin
  if AParent.Count >= 2 then
    raise ECnBinaryTreeException.Create('Binary TreeNode Can Only Contains 2 Child.');
end;

procedure TCnBinaryTree.DeleteLeftChild(AParent: TCnBinaryLeaf);
begin
  if AParent.Tree = Self then
  begin
    if AParent.LeftLeaf <> nil then
      AParent.LeftLeaf.Parent := nil;
    AParent.LeftLeaf := nil;
  end;
end;

procedure TCnBinaryTree.DeleteRightChild(AParent: TCnBinaryLeaf);
begin
  if AParent.Tree = Self then
  begin
    if AParent.RightLeaf <> nil then
      AParent.RightLeaf.Parent := nil;
    AParent.RightLeaf := nil;
  end;
end;

procedure TCnBinaryTree.DoInOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnInOrderTravelLeaf) then
    FOnInOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.DoPostOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnPostOrderTravelLeaf) then
    FOnPostOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.DoPreOrderTravelLeaf(ALeaf: TCnBinaryLeaf);
begin
  if Assigned(FOnPreOrderTravelLeaf) then
    FOnPreOrderTravelLeaf(ALeaf);
end;

procedure TCnBinaryTree.InOrderTravel;
begin
  Root.DoInOrderTravel;
end;

procedure TCnBinaryTree.PostOrderTravel;
begin
  Root.DoPostOrderTravel;
end;

procedure TCnBinaryTree.PreOrderTravel;
begin
  Root.DoPreOrderTravel;
end;

function TCnBinaryTree.GetRoot: TCnBinaryLeaf;
begin
  Result := TCnBinaryLeaf(inherited GetRoot);
end;

procedure TCnBinaryTree.ReplaceLeaf(ALeaf, AChild: TCnBinaryLeaf);
begin
  AChild.Parent := ALeaf.Parent;
  if ALeaf = ALeaf.Parent.LeftLeaf then
    ALeaf.Parent.LeftLeaf := AChild
  else
    ALeaf.Parent.RightLeaf := AChild;
end;


{$IFDEF ENABLE_UIINTERACT}
{$IFDEF MSWINDOWS}

procedure TCnBinaryTree.LoadFromATreeNode(ALeaf: TCnLeaf;
  ANode: TTreeNode);
var
  Leaf: TCnLeaf;
begin
  if (ANode <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeNode(ALeaf, ANode) then
    begin
      if ANode.Count > 0 then
      begin
        Leaf := AddLeftChild(ALeaf as TCnBinaryLeaf);
{$IFDEF FPC}
        LoadFromATreeNode(Leaf, ANode.Items[0]);
{$ELSE}
        LoadFromATreeNode(Leaf, ANode.Item[0]);
{$ENDIF}
      end;
      if ANode.Count > 1 then
      begin
        Leaf := AddRightChild(ALeaf as TCnBinaryLeaf);
{$IFDEF FPC}
        LoadFromATreeNode(Leaf, ANode.Items[1]);
{$ELSE}
        LoadFromATreeNode(Leaf, ANode.Item[1]);
{$ENDIF}
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnBinaryTree.LoadFromTreeView(ATreeView: ComCtrls.TTreeView;
  RootNode: TTreeNode; RootLeaf: TCnBinaryLeaf);
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
        RootLeaf := Root;

      ALeaf := AddLeftChild(RootLeaf);
      LoadFromATreeNode(ALeaf, ANode);
      if RootNode <> nil then Exit;
      // 声明了 RootNode 时以 RootNode 为根，所以不处理 RootNode 的同层节点

      ANode := ANode.GetNextSibling; // 此层如有下一个后继节点，做右子树
      if ANode <> nil then
      begin
        ALeaf := AddRightChild(RootLeaf);
        LoadFromATreeNode(ALeaf, ANode);
      end;
    end;
  end;
end;

procedure TCnBinaryTree.SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode);
begin
  inherited SaveToATreeNode(ALeaf, ANode);
end;

procedure TCnBinaryTree.SaveToTreeView(ATreeView: ComCtrls.TTreeView;
  RootNode: TTreeNode; RootLeaf: TCnBinaryLeaf);
begin
  inherited SaveToTreeView(ATreeView, RootNode, RootLeaf);
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnBinaryTree.LoadFromATreeViewItem(ALeaf: TCnLeaf;
  AItem: TTreeViewItem);
var
  Leaf: TCnLeaf;
begin
  if (AItem <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeViewItem(ALeaf, AItem) then
    begin
      if AItem.Count > 0 then
      begin
        Leaf := AddLeftChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[0]);
      end;
      if AItem.Count > 1 then
      begin
        Leaf := AddRightChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[1]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnBinaryTree.LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnBinaryLeaf);
var
  AItem: TTreeViewItem;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootItem <> nil) and (RootItem.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    if RootLeaf = nil then
      Self.Clear
    else
      RootLeaf.Clear;

    if ATreeView.GlobalCount > 0 then
    begin
      if RootItem = nil then
        AItem := ATreeView.Items[0]
      else
        AItem := RootItem;
      // 第一个节点
      if RootLeaf = nil then
        RootLeaf := Root;

      ALeaf := AddLeftChild(RootLeaf);
      LoadFromATreeViewItem(ALeaf, AItem);
      if RootItem <> nil then Exit;
      // 声明了 RootNode 时以 RootNode 为根，所以不处理 RootNode 的同层节点

      AItem := GetNextSiblingItem(AItem); // 此层如有下一个后继节点，做右子树
      if AItem <> nil then
      begin
        ALeaf := AddRightChild(RootLeaf);
        LoadFromATreeViewItem(ALeaf, AItem);
      end;
    end;
  end;
end;

procedure TCnBinaryTree.SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem);
begin
  inherited SaveToATreeViewItem(ALeaf, AItem);
end;

procedure TCnBinaryTree.SaveToTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnBinaryLeaf);
begin
  inherited SaveToTreeView(ATreeView, RootItem, RootLeaf);
end;

{$ENDIF}
{$ENDIF}

function TCnBinaryTree.GetCount: Integer;
begin
  Result := Root.AllNonNilCount + 1;
end;

function TCnBinaryTree.GetHeight: Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := Root.SubTreeHeight + 1;
end;

//==============================================================================
// TCnBinaryLeaf
//==============================================================================

function TCnBinaryLeaf.AddLeftChild: TCnBinaryLeaf;
begin
  Result := Tree.AddLeftChild(Self);
end;

function TCnBinaryLeaf.AddRightChild: TCnBinaryLeaf;
begin
  Result := Tree.AddRightChild(Self);
end;

constructor TCnBinaryLeaf.Create(ATree: TCnTree);
begin
  if not (ATree is TCnBinaryTree) then
    raise ECnTreeException.Create('Must be Binary Tree.');

  inherited;
  FList.Add(nil);  // 左子节点
  FList.Add(nil);  // 右子节点
end;

procedure TCnBinaryLeaf.DeleteLeftChild;
begin
  Tree.DeleteLeftChild(Self);
end;

procedure TCnBinaryLeaf.DeleteRightChild;
begin
  Tree.DeleteRightChild(Self);
end;

procedure TCnBinaryLeaf.DoInOrderTravel;
begin
  if LeftLeaf <> nil then
    LeftLeaf.DoInOrderTravel;
  Tree.DoInOrderTravelLeaf(Self);
  if RightLeaf <> nil then
    RightLeaf.DoInOrderTravel;
end;

procedure TCnBinaryLeaf.DoPostOrderTravel;
begin
  if LeftLeaf <> nil then
    LeftLeaf.DoPostOrderTravel;
  if RightLeaf <> nil then
    RightLeaf.DoPostOrderTravel;
  Tree.DoPostOrderTravelLeaf(Self);
end;

procedure TCnBinaryLeaf.DoPreOrderTravel;
begin
  Tree.DoPreOrderTravelLeaf(Self);
  if LeftLeaf <> nil then
    LeftLeaf.DoPreOrderTravel;
  if RightLeaf <> nil then
    RightLeaf.DoPreOrderTravel;
end;

function TCnBinaryLeaf.GetBrotherLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Parent <> nil then
  begin
    if Parent.LeftLeaf = Self then
      Result := Parent.RightLeaf
    else if Parent.RightLeaf = Self then
      Result := Parent.LeftLeaf;
  end;
end;

function TCnBinaryLeaf.GetGrandLeaf: TCnBinaryLeaf;
begin
  if GetParent <> nil then
    Result := GetParent.GetParent
  else
    Result := nil;
end;

function TCnBinaryLeaf.GetLeftLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Count > 0 then
    Result := TCnBinaryLeaf(Items[0]);
end;

function TCnBinaryLeaf.GetMostLeftLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if LeftLeaf = nil then
    Exit;

  if LeftLeaf.LeftLeaf = nil then
    Result := LeftLeaf
  else
    Result := LeftLeaf.GetMostLeftLeaf;
end;

function TCnBinaryLeaf.GetMostLeftLeafFromRight: TCnBinaryLeaf;
begin
  if RightLeaf = nil then
    Result := nil
  else if RightLeaf.LeftLeaf = nil then
    Result := RightLeaf
  else
    Result := RightLeaf.GetMostLeftLeaf;
end;

function TCnBinaryLeaf.GetMostRightLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if RightLeaf = nil then
    Exit;

  if RightLeaf.RightLeaf = nil then
    Result := RightLeaf
  else
    Result := RightLeaf.GetMostRightLeaf;
end;

function TCnBinaryLeaf.GetMostRightLeafFromLeft: TCnBinaryLeaf;
begin
  if LeftLeaf = nil then
    Result := nil
  else if LeftLeaf.RightLeaf = nil then
    Result := LeftLeaf
  else
    Result := LeftLeaf.GetMostRightLeaf;
end;

function TCnBinaryLeaf.GetParent: TCnBinaryLeaf;
begin
  Result := TCnBinaryLeaf(inherited GetParent);
end;

function TCnBinaryLeaf.GetRightLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Count > 1 then
    Result := TCnBinaryLeaf(Items[1]);
end;

function TCnBinaryLeaf.GetSubTreeHeight: Integer;
var
  L, R: Integer;
begin
  Result := 0;
  if Self = nil then
    Exit;

  if (LeftLeaf = nil) and (RightLeaf = nil) then
    Result := 0
  else
  begin
    if LeftLeaf = nil then
      L := 0
    else
      L := LeftLeaf.SubTreeHeight;
    if RightLeaf = nil then
      R := 0
    else
      R := RightLeaf.SubTreeHeight;

    Result := Max(L, R) + 1;
  end;
end;

function TCnBinaryLeaf.GetTree: TCnBinaryTree;
begin
  Result := TCnBinaryTree(inherited GetTree);
end;

function TCnBinaryLeaf.GetUncleLeaf: TCnBinaryLeaf;
begin
  if Parent <> nil then
    Result := Parent.GetBrotherLeaf
  else
    Result := nil;
end;

function TCnBinaryLeaf.IsBalance: Boolean;
var
  L, R: Integer;
  LB, RB: Boolean;
begin
  L := 0;
  R := 0;
  LB := True;
  RB := True;

  if LeftLeaf <> nil then
  begin
    L := LeftLeaf.SubTreeHeight;
    LB := LeftLeaf.IsBalance;
  end;
  if RightLeaf <> nil then
  begin
    R := RightLeaf.SubTreeHeight;
    RB := RightLeaf.IsBalance;
  end;

  Result := LB and RB and ((L - R) <= 1) and ((L - R) >= -1);
end;

procedure TCnBinaryLeaf.SetLeftLeaf(const Value: TCnBinaryLeaf);
begin
  if Value <> nil then
    Assert(Value.Tree = Self.FTree);

  if Value = Self then
    raise ECnTreeException.Create('Left Leaf can NOT be Self.');

//  if (Value <> Items[0]) and (Items[0] <> nil) then
//  begin
//    // 旧节点要不要释放？如果旧节点是叶子节点，直接释放没问题，其余情况不释放
//    if ((Items[0] as TCnBinaryLeaf).LeftLeaf = nil) and
//      ((Items[0] as TCnBinaryLeaf).RightLeaf = nil) then
//      Items[0].Free; // 释放旧节点
//  end;

  Items[0] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

procedure TCnBinaryLeaf.SetParent(const Value: TCnBinaryLeaf);
begin
  if Value <> FParent then
    FParent := Value;
end;

procedure TCnBinaryLeaf.SetRightLeaf(const Value: TCnBinaryLeaf);
begin
  if Value <> nil then
    Assert(Value.Tree = Self.FTree);

  if Value = Self then
    raise ECnTreeException.Create('Right Leaf can NOT be Self.');

//  if (Value <> Items[1]) and (Items[1] <> nil) then
//  begin
//    // 旧节点要不要释放？如果旧节点是叶子节点，直接释放没问题，其余情况不释放
//    if ((Items[1] as TCnBinaryLeaf).LeftLeaf = nil) and
//      ((Items[1] as TCnBinaryLeaf).RightLeaf = nil) then
//      Items[1].Free; // 释放旧节点
//  end;

  Items[1] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

//==============================================================================
// TCnTrieLeaf
//==============================================================================

function TCnTrieLeaf.DoInsertChar(P: PChar): TCnTrieLeaf;
var
  C, CaseC: Char;
  I, Idx, Gt: Integer;
  Leaf: TCnTrieLeaf;
begin
  Result := nil;
  if (P = nil) or (P^ = #0) then
    Exit;

  C := P^;
  CaseC := Tree.ConvertCharWithCase(C);

  if Tree.AnsiFastMode then
  begin
    I := Ord(CaseC);
    if Items[I] = nil then // 无此字母对应子节点，直接创建
    begin
      Leaf := Tree.CreateTrieLeaf;
      Leaf.Character := CaseC;
      Items[I] := Leaf;

      if not Tree.OnlyChar then
        Leaf.Text := Leaf.Parent.Text + C;

      Inc(P);
      if P^ = #0 then
        Result := Leaf
      else
        Result := Leaf.DoInsertChar(P);
    end
    else
    begin
      Inc(P);
      if P^ = #0 then // 结束，字符串已经存在
        Result := nil
      else
        Result := Items[I].DoInsertChar(P);
    end;
    Exit;
  end;

  if Count = 0 then // 无子节点，直接创建
  begin
    Leaf := Tree.CreateTrieLeaf;
    Leaf.Character := CaseC;
    AddChild(Leaf);
    if not Tree.OnlyChar then
      Leaf.Text := Leaf.Parent.Text + C;

    Inc(P);
    if P^ = #0 then
      Result := Leaf
    else
      Result := Leaf.DoInsertChar(P);
    Exit;
  end;

  Idx := -1;
  Gt := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Character = CaseC then
    begin
      Idx := I;
      Break;
    end
    else if Items[I].Character > CaseC then
    begin
      Gt := I;
      Break;
    end;
  end;

  if Idx >= 0 then // 找到有这个字符的节点
  begin
    Inc(P);
    if P^ = #0 then // 结束，字符串已经存在
      Result := nil
    else
      Result := Items[Idx].DoInsertChar(P);
  end
  else // 没这个字符的节点，要创建
  begin
    Leaf := Tree.CreateTrieLeaf;
    Leaf.Character := CaseC; // 如果大小写不敏感，则 Character 中存储大写字母

    if Gt = -1 then  // 没有比这字符大的节点，添加在最后
      AddChild(Leaf)
    else
      InsertChild(Leaf, Gt); // 否则插在第一个比这个字符大的节点的前面

    if not Tree.OnlyChar then
      Leaf.Text := Leaf.Parent.Text + C; // 真实文字保持原样

    Inc(P);
    if P^ = #0 then
      Result := Leaf
    else
      Result := Leaf.DoInsertChar(P);
  end;
end;

function TCnTrieLeaf.DoSearchChar(P: PChar): TCnTrieLeaf;
var
  CaseC: Char;
  I: Integer;
begin
  Result := nil;
  if (P = nil) or (P^ = #0) then
    Exit;

  CaseC := Tree.ConvertCharWithCase(P^);
  if Tree.AnsiFastMode then
  begin
    I := Ord(CaseC);
    if Items[I] <> nil then
    begin
      Inc(P);
      if P^ = #0 then
        Result := Items[I]
      else
        Result := Items[I].DoSearchChar(P);
    end;
  end
  else
  begin
    for I := 0 to Count - 1 do
    begin
      if Items[I].Character = CaseC then
      begin
        Inc(P);
        if P^ = #0 then
          Result := Items[I]
        else
          Result := Items[I].DoSearchChar(P);
      end;
    end;
  end;
end;

function TCnTrieLeaf.GetCharacter: Char;
begin
  Result := FCharacter;
end;

function TCnTrieLeaf.GetItems(Index: Integer): TCnTrieLeaf;
begin
  Result := TCnTrieLeaf(inherited GetItems(Index));
end;

function TCnTrieLeaf.GetTree: TCnTrieTree;
begin
  Result := TCnTrieTree(inherited GetTree);
end;

procedure TCnTrieLeaf.SetCharacter(const Value: Char);
begin
  FCharacter := Value;
end;

procedure TCnTrieLeaf.SetItems(Index: Integer; const Value: TCnTrieLeaf);
begin
  inherited SetItems(Index, Value);
end;

//==============================================================================
// TCnTrieTree 字典搜索树
//==============================================================================

function TCnTrieTree.ConvertCharWithCase(C: Char): Char;
begin
  Result := C;
  if not FCaseSensitive and
    {$IFDEF COMPILER12_UP}(Ord(C) <= $FF) and{$ENDIF}
    (AnsiChar(C) in ['a'..'z']) then
    Dec(Result, 32);
end;

constructor TCnTrieTree.Create(ACaseSensitive: Boolean; AOnlyChar: Boolean;
  AnAnsiFastMode: Boolean);
var
  I: Char;
begin
  inherited Create(TCnTrieLeaf);
  FCaseSensitive := ACaseSensitive;
  FOnlyChar := AOnlyChar;

{$IFDEF UNICODE}
  FAnsiFastMode := False;
{$ELSE}
  FAnsiFastMode := AnAnsiFastMode;
{$ENDIF}
  if FAnsiFastMode then
    for I := Low(Char) to High(Char) do // 预先加入 256 个空子树
      Root.FList.Add(nil);
end;

function TCnTrieTree.CreateTrieLeaf: TCnTrieLeaf;
var
  I: Char;
begin
  Result := TCnTrieLeaf(CreateLeaf(Self));
  if FAnsiFastMode then
    for I := Low(Char) to High(Char) do // 预先加入 256 个空子树
      Result.FList.Add(nil);
end;

function TCnTrieTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnTrieLeaf;
end;

function TCnTrieTree.GetRoot: TCnTrieLeaf;
begin
  Result := TCnTrieLeaf(inherited GetRoot);
end;

function TCnTrieTree.InsertString(const Str: string): TCnTrieLeaf;
begin
  Result := Root.DoInsertChar(PChar(Str));
end;

function TCnTrieTree.SearchString(const Str: string): TCnTrieLeaf;
begin
  Result := Root.DoSearchChar(PChar(Str));
end;

function TCnTrieTree.StringExists(const Str: string): Boolean;
begin
  Result := (SearchString(Str) <> nil);
end;

{ TCnBinarySortTree }

constructor TCnBinarySortTree.Create;
begin
  inherited;
  FOnCompare := DefaultOnCompare;
  FOnSetLeaf := DefaultOnSetLeaf;
end;

procedure TCnBinarySortTree.CheckCompareSetLeaf;
begin
  if not Assigned(FOnCompare) then
    raise ECnTreeException.Create('No OnCompare Event, can NOT Continue.');
  if not Assigned(FOnSetLeaf) then
    raise ECnTreeException.Create('No OnSetLeaf Event, can NOT Continue.');
end;

constructor TCnBinarySortTree.Create(LeafClass: TCnBinaryLeafClass);
begin
  inherited Create(LeafClass);
end;

function TCnBinarySortTree.DefaultOnCompare(Value: Integer;
  Leaf: TCnBinaryLeaf): Integer;
begin
  Result := Value - Leaf.Data;
end;

function TCnBinarySortTree.Insert(Value: Integer): TCnBinaryLeaf;
begin
  CheckCompareSetLeaf;
  if not FRootInserted then
  begin
    FOnSetLeaf(Root, Value);
    FRootInserted := True;
    Result := Root;
  end
  else
    Result := InternalInsert(Root, Value);
end;

function TCnBinarySortTree.InternalInsert(Leaf: TCnBinaryLeaf;
  Value: Integer): TCnBinaryLeaf;
var
  Cmp: Integer;
begin
  Cmp := FOnCompare(Value, Leaf);
  if Cmp = 0 then
    raise ECnTreeException.CreateFmt('Value %d already Exists.', [Value])
  else if Cmp < 0 then // Value < Leaf
  begin
    if Leaf.LeftLeaf = nil then
    begin
      Result := Leaf.AddLeftChild;
      FOnSetLeaf(Result, Value);
    end
    else
      Result := InternalInsert(Leaf.LeftLeaf, Value);
  end
  else // Value > Leaf
  begin
    if Leaf.RightLeaf = nil then
    begin
      Result := Leaf.AddRightChild;
      FOnSetLeaf(Result, Value);
    end
    else
      Result := InternalInsert(Leaf.RightLeaf, Value);
  end;
end;

function TCnBinarySortTree.InternalSearch(Leaf: TCnBinaryLeaf;
  Value: Integer): TCnBinaryLeaf;
var
  Cmp: Integer;
begin
  if Leaf = nil then
  begin
    Result := nil;
    Exit;
  end;

  Cmp := FOnCompare(Value, Leaf);
  if Cmp = 0 then
    Result := Leaf
  else if Cmp < 0 then // Value < Leaf
    Result := InternalSearch(Leaf.LeftLeaf, Value)
  else // Value > Leaf
    Result := InternalSearch(Leaf.RightLeaf, Value);
end;

function TCnBinarySortTree.IsEmpty: Boolean;
begin
  Result := FRootInserted or (GetCount > 1);
end;

function TCnBinarySortTree.Search(Value: Integer): TCnBinaryLeaf;
begin
  CheckCompareSetLeaf;
  if FRootInserted then
    Result := InternalSearch(Root, Value)
  else
    Result := nil;
end;

procedure TCnBinarySortTree.DefaultOnSetLeaf(Leaf: TCnBinaryLeaf;
  Value: Integer);
begin
  Leaf.Text := IntToStr(Value);
  Leaf.Data := Value;
end;

function TCnBinarySortTree.Delete(Value: Integer): Boolean;
begin
  CheckCompareSetLeaf;
  if FRootInserted then
  begin
    if (GetCount = 1) and (FOnCompare(Value, Root) = 0) then
    begin
      Result := True;
      FRootInserted := False; // Root 节点不删，只设一个未初始化标记
    end
    else
      Result := InternalDelete(Root, Value);
  end
  else
    Result := False;
end;

function TCnBinarySortTree.InternalDelete(Leaf: TCnBinaryLeaf;
  Value: Integer): Boolean;
var
  Cmp: Integer;
  AParent, ALeaf: TCnBinaryLeaf;
  IsParent: Boolean;
begin
  Cmp := FOnCompare(Value, Leaf);
  if Cmp = 0 then
  begin
    Result := True;
    AParent := Leaf.Parent;
    // 找到了，要删 Leaf，但要注意其左右子树的存在状况
    if (Leaf.LeftLeaf = nil) and (Leaf.RightLeaf = nil) then // 叶节点直接删
    begin
      if AParent <> nil then
      begin
        if AParent.LeftLeaf = Leaf then
          AParent.DeleteLeftChild
        else if AParent.RightLeaf = Leaf then
          AParent.DeleteRightChild;
      end
      else
      begin
        // Leaf 是 Root 且只有 Root，删（可能代码进不了此处）
        FRootInserted := False;
      end;
    end
    else if Leaf.LeftLeaf = nil then // Left 空，将右子树代替自身，自己取下右子树并释放自己
    begin
      if AParent <> nil then
      begin
        if AParent.LeftLeaf = Leaf then
          AParent.LeftLeaf := Leaf.RightLeaf
        else if AParent.RightLeaf = Leaf then
          AParent.RightLeaf := Leaf.RightLeaf;
      end
      else
      begin
        // Leaf 是 Root，右子树设为 Root
        Root := Leaf.RightLeaf;
      end;

      Leaf.RightLeaf := nil;
      Leaf.Free;
    end
    else if Leaf.RightLeaf = nil then // Right 空，将左子树代替自身，自己取下左子树并释放自己
    begin
      if AParent <> nil then
      begin
        if AParent.LeftLeaf = Leaf then
          AParent.LeftLeaf := Leaf.LeftLeaf
        else if AParent.RightLeaf = Leaf then
          AParent.RightLeaf := Leaf.LeftLeaf;
      end
      else
      begin
        // Leaf 是 Root，左子树设为 Root
        Root := Leaf.LeftLeaf;
      end;

      Leaf.LeftLeaf := nil;
      Leaf.Free;
    end
    else // 左右都不空，处理起来比较麻烦
    begin
      ALeaf := Leaf.GetMostRightLeafFromLeft;
      if ALeaf <> nil then
      begin
        IsParent := ALeaf.Parent = Leaf;

        // 找到 Leaf 的中序遍历的前驱节点，取代 Leaf，注意前驱节点不可能有右子树，且不是 Leaf 的右节点
        if ALeaf.LeftLeaf <> nil then
        begin
          if ALeaf.Parent.LeftLeaf = ALeaf then
          begin
            // 前驱节点要删，前驱节点是其父的左子树时，把前驱节点的左子树挂到前驱节点的父节点左上
            ALeaf.Parent.LeftLeaf := ALeaf.LeftLeaf;
            ALeaf.LeftLeaf := nil;
          end
          else if ALeaf.Parent.RightLeaf = ALeaf then
          begin
            // 前驱节点要删，前驱节点是其父的右子树时，把前驱节点的左子树挂到前驱节点的父节点右上
            ALeaf.Parent.RightLeaf := ALeaf.LeftLeaf;
            ALeaf.LeftLeaf := nil;
          end;
        end;

        // 剥离 ALeaf 前驱节点后替换 Leaf 所在的位置
        if AParent <> nil then
        begin
          if AParent.LeftLeaf = Leaf then
            AParent.LeftLeaf := ALeaf
          else if AParent.RightLeaf = Leaf then
            AParent.RightLeaf := ALeaf;
        end
        else
        begin
          // 原有 Leaf 是 Root，设为 ALeaf
          Root := ALeaf;
        end;

        // 把原来 Leaf 的子树挂到 ALeaf 下，如果 ALeaf 不是 Leaf 的左子树的话
        if not IsParent then
          ALeaf.LeftLeaf := Leaf.LeftLeaf;
        ALeaf.RightLeaf := Leaf.RightLeaf;

        // 释放此被删除的 Leaf 节点
        Leaf.LeftLeaf := nil;
        Leaf.RightLeaf := nil;
        Leaf.Free;
      end
      else // 用后驱节点替换 Leaf
      begin
        ALeaf := Leaf.GetMostLeftLeafFromRight;
        if ALeaf <> nil then
        begin
          IsParent := ALeaf.Parent = Leaf;

          // 找到 Leaf 的中序遍历的后驱节点，取代 Leaf，注意后驱节点不可能有左子树，且不是 Leaf 的左节点
          if ALeaf.RightLeaf <> nil then
          begin
            if ALeaf.Parent.LeftLeaf = ALeaf then
            begin
              // 后驱节点要删，后驱节点是其父的左子树时，把后驱节点的右子树挂到前驱节点的父节点左上
              ALeaf.Parent.LeftLeaf := ALeaf.RightLeaf;
              ALeaf.RightLeaf := nil;
            end
            else if ALeaf.Parent.RightLeaf = ALeaf then
            begin
              // 后驱节点要删，后驱节点是其父的右子树时，把后驱节点的右子树挂到前驱节点的父节点右上
              ALeaf.Parent.RightLeaf := ALeaf.RightLeaf;
              ALeaf.RightLeaf := nil;
            end;
          end;

          // 剥离 ALeaf 前驱节点后替换 Leaf 所在的位置
          if AParent <> nil then
          begin
            if AParent.LeftLeaf = Leaf then
              AParent.LeftLeaf := ALeaf
            else if AParent.RightLeaf = Leaf then
              AParent.RightLeaf := ALeaf;
          end
          else
          begin
            // 原有 Leaf 是 Root，设为 ALeaf
            Root := ALeaf;
          end;

          // 把原来 Leaf 的子树挂到 ALeaf 下，如果 ALeaf 不是 Leaf 的直接子树的话
          ALeaf.LeftLeaf := Leaf.LeftLeaf;
          if not IsParent then
            ALeaf.RightLeaf := Leaf.RightLeaf;

          // 释放此被删除的 Leaf 节点
          Leaf.LeftLeaf := nil;
          Leaf.RightLeaf := nil;
          Leaf.Free;
        end
      end;
    end;
  end
  else if Cmp < 0 then
  begin
    if Leaf.LeftLeaf = nil then
      Result := False // 找不到
    else
      Result := InternalDelete(Leaf.LeftLeaf, Value);
  end
  else // Value > Leaf
  begin
    if Leaf.RightLeaf = nil then // 找不到
      Result := False
    else
      Result := InternalDelete(Leaf.RightLeaf, Value);
  end;
end;

procedure TCnBinarySortTree.Clear;
begin
  inherited;
  FRootInserted := False;
end;

procedure TCnBinaryTree.SetRoot(const Value: TCnBinaryLeaf);
begin
  if Value <> FRoot then
  begin
    FRoot := Value;
    Root.Parent := nil;
  end;
end;

{ TCnRedBlackLeaf }

function TCnRedBlackLeaf.AddLeftChild: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited AddLeftChild);
end;

function TCnRedBlackLeaf.AddRightChild: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited AddRightChild);
end;

constructor TCnRedBlackLeaf.Create(ATree: TCnTree);
begin
  if not (ATree is TCnRedBlackTree) then
    raise ECnTreeException.Create('Must be RedBlack Tree.');

  inherited;
end;

function TCnRedBlackLeaf.GetBrotherLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetBrotherLeaf);
end;

function TCnRedBlackLeaf.GetGrandLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetGrandLeaf);
end;

function TCnRedBlackLeaf.GetLeftLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetLeftLeaf);
end;

function TCnRedBlackLeaf.GetMostLeftLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostLeftLeaf);
end;

function TCnRedBlackLeaf.GetMostLeftLeafFromRight: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostLeftLeafFromRight);
end;

function TCnRedBlackLeaf.GetMostRightLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostRightLeaf);
end;

function TCnRedBlackLeaf.GetMostRightLeafFromLeft: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetMostRightLeafFromLeft);
end;

function TCnRedBlackLeaf.GetParent: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetParent);
end;

function TCnRedBlackLeaf.GetRightLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetRightLeaf);
end;

function TCnRedBlackLeaf.GetTree: TCnRedBlackTree;
begin
  Result := TCnRedBlackTree(inherited GetTree);
end;

function TCnRedBlackLeaf.GetUncleLeaf: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetUncleLeaf);
end;

procedure TCnRedBlackLeaf.SetLeftLeaf(const Value: TCnRedBlackLeaf);
begin
  inherited SetLeftLeaf(Value);
end;

procedure TCnRedBlackLeaf.SetParent(const Value: TCnRedBlackLeaf);
begin
  inherited SetParent(Value);
end;

procedure TCnRedBlackLeaf.SetRightLeaf(const Value: TCnRedBlackLeaf);
begin
  inherited SetRightLeaf(Value);
end;

{ TCnRedBlackTree }

constructor TCnRedBlackTree.Create;
begin
  inherited;

end;

constructor TCnRedBlackTree.Create(LeafClass: TCnRedBlackLeafClass);
begin
  inherited Create(LeafClass);
end;

function TCnRedBlackTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnRedBlackLeaf;
end;

function TCnRedBlackTree.Delete(Value: Integer): Boolean;
var
  ALeaf: TCnRedBlackLeaf;
begin
  ALeaf := TCnRedBlackLeaf(Search(Value));
  Result := ALeaf <> nil;
  if Result then
    DeleteOneChildLeaf(ALeaf); // Error
end;

procedure TCnRedBlackTree.DeleteCase1(ALeaf: TCnRedBlackLeaf);
begin
  if ALeaf.Parent <> nil then
    DeleteCase2(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase2(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if Brother.IsRed then
  begin
    ALeaf.Parent.IsRed := True;
    Brother.IsRed := False;
    if ALeaf = ALeaf.Parent.LeftLeaf then
      RotateLeft(ALeaf.Parent)
    else
      RotateRight(ALeaf.Parent)
  end;

  DeleteCase3(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase3(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if not ALeaf.Parent.IsRed and not Brother.IsRed and not Brother.LeftLeaf.IsRed
    and not Brother.RightLeaf.IsRed then
  begin
    Brother.IsRed := True;
    DeleteCase1(ALeaf.Parent);
  end
  else
    DeleteCase4(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase4(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if ALeaf.Parent.IsRed and not Brother.IsRed and not Brother.LeftLeaf.IsRed
    and not Brother.RightLeaf.IsRed then
  begin
    Brother.IsRed := True;
    ALeaf.Parent.IsRed := False;
  end
  else
    DeleteCase5(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase5(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  if not Brother.IsRed then
  begin
    if (ALeaf = ALeaf.Parent.LeftLeaf) and not Brother.RightLeaf.IsRed
      and Brother.LeftLeaf.IsRed then
    begin
      Brother.IsRed := True;
      Brother.LeftLeaf.IsRed := False;
      RotateRight(Brother);
    end
    else if (ALeaf = ALeaf.Parent.RightLeaf) and not Brother.LeftLeaf.IsRed
      and Brother.RightLeaf.IsRed then
    begin
      Brother.IsRed := True;
      Brother.RightLeaf.IsRed := False;
      RotateLeft(Brother);
    end;
  end;

  DeleteCase6(ALeaf);
end;

procedure TCnRedBlackTree.DeleteCase6(ALeaf: TCnRedBlackLeaf);
var
  Brother: TCnRedBlackLeaf;
begin
  Brother := ALeaf.GetBrotherLeaf;
  Brother.IsRed := ALeaf.Parent.IsRed;
  ALeaf.Parent.IsRed := False;

  if ALeaf = ALeaf.Parent.LeftLeaf then
  begin
    Brother.RightLeaf.IsRed := False;
    RotateLeft(ALeaf.Parent);
  end
  else
  begin
    Brother.LeftLeaf.IsRed := False;
    RotateRight(ALeaf.Parent);
  end;
end;

procedure TCnRedBlackTree.DeleteOneChildLeaf(ALeaf: TCnRedBlackLeaf);
var
  Child: TCnRedBlackLeaf;
begin
  Child := ALeaf.LeftLeaf;
  if Child = nil then
    Child := ALeaf.RightLeaf;

  if Child = nil then
    raise ECnTreeException.Create('ALeaf has NO Child.');

  ReplaceLeaf(ALeaf, Child);
  if not ALeaf.IsRed then
  begin
    if Child.IsRed then
      Child.IsRed := False
    else
      DeleteCase1(Child);
  end;

  ALeaf.Free; // ALeaf 已剥离，可以直接删除
end;

function TCnRedBlackTree.GetRoot: TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited GetRoot);
end;

function TCnRedBlackTree.Insert(Value: Integer): TCnRedBlackLeaf;
begin
  Result := TCnRedBlackLeaf(inherited Insert(Value));
  if Result <> nil then
  begin
    Result.IsRed := True;
    InsertRepair(Result);
  end;
end;

procedure TCnRedBlackTree.InsertRepair(ALeaf: TCnRedBlackLeaf);
begin
  if (ALeaf.Parent = nil) and (Root = ALeaf) then
    ALeaf.IsRed := False    // 根节点直接染黑
  else if not ALeaf.Parent.IsRed then
    ALeaf.IsRed := True
  else if (ALeaf.GetUncleLeaf <> nil) and ALeaf.GetUncleLeaf.IsRed then
    InsertRepairCase3(ALeaf)
  else
    InsertRepairCase4(ALeaf);
end;

procedure TCnRedBlackTree.InsertRepairCase3(ALeaf: TCnRedBlackLeaf);
begin
  ALeaf.Parent.IsRed := False;
  ALeaf.GetUncleLeaf.IsRed := False;
  if ALeaf.GetGrandLeaf <> nil then
  begin
    ALeaf.GetGrandLeaf.IsRed := True;
    InsertRepair(ALeaf.GetGrandLeaf);
  end;
end;

procedure TCnRedBlackTree.InsertRepairCase4(ALeaf: TCnRedBlackLeaf);
var
  P, G: TCnRedBlackLeaf;
begin
  P := ALeaf.Parent;
  G := ALeaf.GetGrandLeaf;

  if (ALeaf = P.RightLeaf) and (P = G.GetLeftLeaf) then
  begin
    RotateLeft(P);
    ALeaf := ALeaf.LeftLeaf;
  end
  else if (ALeaf = P.LeftLeaf) and (P = G.GetRightLeaf) then
  begin
    RotateRight(P);
    ALeaf := ALeaf.RightLeaf;
  end;
  InsertRepairCase4Step2(ALeaf);
end;

procedure TCnRedBlackTree.InsertRepairCase4Step2(ALeaf: TCnRedBlackLeaf);
var
  P, G: TCnRedBlackLeaf;
begin
  P := ALeaf.Parent;
  G := ALeaf.GetGrandLeaf;

  if ALeaf = P.LeftLeaf then
    RotateRight(G)
  else
    RotateLeft(G);

  P.IsRed := False;
  G.IsRed := True;
end;

procedure TCnRedBlackTree.RotateLeft(ALeaf: TCnRedBlackLeaf);
var
  Right: TCnRedBlackLeaf;
begin
  // ALeaf 的右子节点取代自己，ALeaf 变成左子节点，原右子节点的左子节点变成 ALeaf 的右子节点
  if ALeaf = nil then
    Exit;
  if ALeaf.RightLeaf = nil then
    Exit;

  Right := ALeaf.RightLeaf;
  ALeaf.RightLeaf := Right.LeftLeaf;
  if ALeaf.Parent <> nil then
  begin
    if ALeaf.Parent.LeftLeaf <> ALeaf then
      raise ECnTreeException.Create('Rotate Left Failed');

    ALeaf.Parent.LeftLeaf := Right;
  end
  else if Root = ALeaf then // 如果 ALeaf 是根节点，要重设根节点
    Root := Right;
  Right.LeftLeaf := ALeaf;
end;

procedure TCnRedBlackTree.RotateRight(ALeaf: TCnRedBlackLeaf);
var
  Left: TCnRedBlackLeaf;
begin
  // ALeaf 的左子节点取代自己，ALeaf 变成右子节点，原左子节点的右子节点变成 ALeaf 的左子节点
  if ALeaf = nil then
    Exit;
  if ALeaf.LeftLeaf = nil then
    Exit;

  Left := ALeaf.LeftLeaf;
  ALeaf.LeftLeaf := Left.RightLeaf;
  if ALeaf.Parent <> nil then
  begin
    if ALeaf.Parent.RightLeaf <> ALeaf then
      raise ECnTreeException.Create('Rotate Right Failed');

    ALeaf.Parent.RightLeaf := Left;
  end
  else if Root = ALeaf then // 如果 ALeaf 是根节点，要重设根节点
    Root := Left;
  Left.RightLeaf := ALeaf;
end;

procedure TCnRedBlackTree.SetRoot(const Value: TCnRedBlackLeaf);
begin
  inherited SetRoot(Value);
  if Value <> nil then
    Value.IsRed := False; // 根节点黑色
end;

end.
