{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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
* 单元名称：实现单根无序树、二叉树、字典搜索树的类单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元为 TCnTree 和 TCnLeaf 的单根无序树的实现单元，以及其子类
*           二叉树 TCnBinaryTree/Leaf、字典搜索树 TCnTrieTree/Leaf。
*           TCnTree/Leaf 类似于 TTreeNodes/Node 的关系，支持深度和广度优先遍历，
*           支持按深度优先的顺序以索引值的形式直接访问各个节点。
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6 + 10.3.1
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.04.17 V1.8 by LiuXiao
*               支持 Win32/Win64/MacOS，支持 VCL 与 FMX 下的 TreeView 交互。
*           2015.05.30 V1.7 by LiuXiao
*               字典树加入 Ansi 快速查找模式。
*           2015.05.22 V1.6 by LiuXiao
*               加入字典树的实现。
*           2015.05.03 V1.5 by LiuXiao
*               加入二叉树的实现。
*           2015.03.16 V1.4 by LiuXiao
*               修正广度优先遍历的错误，将 Root 的 Level 改成 0。
*           2005.05.08 V1.3 by Alan
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
  SysUtils, Classes, Contnrs {$IFDEF MSWINDOWS}, ComCtrls {$ENDIF}
  {$IFDEF SUPPORT_FMX}, FMX.TreeView {$ENDIF}, Math;
  // If ComCtrls not found, please add 'Vcl' to 'Unit Scope Names' in Project Options.

type

//==============================================================================
// 树的基类，单根有序树的实现
//==============================================================================

  ECnTreeException = class(Exception);

  TCnTree = class;

  TCnLeaf = class(TPersistent)
  {* 树叶基类}
  private
    FData: Integer;
    FList: TList;
    FParent: TCnLeaf;
    FText: string;
    FTree: TCnTree;
    FObj: TObject;
    function GetTree: TCnTree;
    function GetAllNonNilCount: Integer;
    function GetSubTreeHeight: Integer; virtual;
  protected
    function GetAbsoluteIndex: Integer;
    function GetAllCount: Integer;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetIndex: Integer;
    function GetItems(Index: Integer): TCnLeaf;
    procedure SetItems(Index: Integer; const Value: TCnLeaf);
    function GetLevel: Integer;

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDepthFirstTravel;
    procedure DoWidthFirstTravel;
    function SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
    {* 将某节点赋值为第 Index 个子节点，返回原节点值 }
    property AllNonNilCount: Integer read GetAllNonNilCount;
    {* 所有非 nil 的子孙节点数目 }
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
    procedure Clear; virtual;
    {* 清除所有直属子节点，子节点以下也会被删除释放  }
    procedure DeleteChild(AIndex: Integer); virtual;
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
    function GetAbsoluteIndexFromParent(IndirectParentLeaf: TCnLeaf): Integer;
    {* 获得 Leaf 在 IndirectParentLeaf 下深度优先的遍历顺序的索引，0 开始，
      如果 IndirectParentLeaf 不是间接或直接 Parent 则返回 -1}
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
    property Items[Index: Integer]: TCnLeaf read GetItems write SetItems; default;
    {* 直属叶节点数组 }
    property SubTreeHeight: Integer read GetSubTreeHeight;
    {* 此节点下属子树的最大高度，无子节点时为 0}

    property Level: Integer read GetLevel;
    {* 本节点层数，Root 节点 Level 为 0 }
    property Parent: TCnLeaf read FParent;
    {* 父节点，不可写 }
    property Tree: TCnTree read GetTree;
    {* 所属树，一个叶必须属于一棵树 }
  published
    property Obj: TObject read FObj write FObj;
    {* 用以保存一对象引用}
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

{$IFDEF MSWINDOWS}
  TCnTreeNodeEvent = procedure (ALeaf: TCnLeaf; ATreeNode: TTreeNode;
    var Valid: Boolean) of object;
{$ENDIF}

{$IFDEF SUPPORT_FMX}
  TCnTreeViewItemEvent = procedure (ALeaf: TCnLeaf; ATreeItem: TTreeViewItem;
    var Valid: Boolean) of object;
{$ENDIF}

  TCnTree = class(TPersistent)
  {* 单根有序树实现类}
  private
    FLeafClass: TCnLeafClass;
    FBatchUpdating: Boolean;
    FLeaves: TObjectList;
    FRoot: TCnLeaf;
    FOnWidthFirstTravelLeaf: TNotifyEvent;
    FOnDepthFirstTravelLeaf: TNotifyEvent;
{$IFDEF MSWINDOWS}
    FOnSaveANode: TCnTreeNodeEvent;
    FOnLoadANode: TCnTreeNodeEvent;
{$ENDIF}
{$IFDEF SUPPORT_FMX}
    FOnSaveAItem: TCnTreeViewItemEvent;
    FOnLoadAItem: TCnTreeViewItemEvent;
{$ENDIF}
    function GetMaxLevel: Integer;
    function GetHeight: Integer; virtual;
    procedure AssignLeafAndChildren(Source, DestLeaf: TCnLeaf; DestTree: TCnTree);
  protected
    function DefaultLeafClass: TCnLeafClass; virtual;

    function GetRoot: TCnLeaf;
    function GetItems(AbsoluteIndex: Integer): TCnLeaf;
    function GetCount: Integer;
    function GetRegisteredCount: Integer;

    procedure AssignTo(Dest: TPersistent); override;

    function CreateLeaf(ATree: TCnTree): TCnLeaf; virtual;
    procedure DoDepthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
    procedure DoWidthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
{$IFDEF MSWINDOWS}
    function DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
    function DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    function DoLoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
    function DoSaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
{$ENDIF}

    procedure ValidateComingLeaf(AParent, AChild: TCnLeaf); virtual;
    {* 当某节点需要插入一个子节点时被调用，供树的子类根据条件抛出异常来拦截控制}

    procedure RegisterLeaf(ALeaf: TCnLeaf);
    {* 仅供叶节点调用，在树中登记此叶节点 }
    procedure UnRegisterLeaf(ALeaf: TCnLeaf);
    {* 仅供叶节点调用，取消此叶节点的登记 }

{$IFDEF MSWINDOWS}
    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* 从一 TreeNode 节点载入其子节点，供递归调用 }
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* 将节点本身以及子节点写入一 TreeNode，供递归调用 }
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); virtual;
    {* 从一 TreeNode 节点载入其子节点，供递归调用 }
    procedure SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); virtual;
    {* 将节点本身以及子节点写入一 TreeNode，供递归调用 }
{$ENDIF}
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

{$IFDEF MSWINDOWS}
    // 和 TreeView 的交互方法，注意 Root 不参与交互
    procedure LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* 从一 VCL 的 TreeView 读入节点内容。RootNode 的子节点被读入成 RootLeaf 所指明的
    节点的子节点，RootNode 为 nil 表示载入全部 TreeNodes，RootLeaf 为 nil 表示
    载入的为 Tree.Root 的直属节点，也就是所有节点}
    procedure SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* 将节点内容写入一 VCL 的 TreeView。 RootLeaf 的子节点被写入成 RootNode 所指明的
    节点的子节点，RootLeaf 为 nil 表示写入 Root 的所有子节点，其实也就是所有节
    点，RootNode 为 nil 表示写入的将成为 TreeView 的根 TreeNodes}
{$ENDIF}

{$IFDEF SUPPORT_FMX}
    procedure LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* 从一 FMX 的 TreeView 读入节点内容。RootItem 的子节点被读入成 RootLeaf 所指明的
    节点的子节点，RootItem 为 nil 表示载入全部 TreeNodes，RootLeaf 为 nil 表示
    载入的为 Tree.Root 的直属节点，也就是所有节点}
    procedure SaveToTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* 将节点内容写入一 FMX 的 TreeView。 RootLeaf 的子节点被写入成 RootItem 所指明的
    节点的子节点，RootLeaf 为 nil 表示写入 Root 的所有子节点，其实也就是所有节
    点，RootItem 为 nil 表示写入的将成为 TreeView 的根 TreeNodes}
{$ENDIF}

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
    property MaxLevel: Integer read GetMaxLevel;
    {* 返回树中最深层节点的层数，Root 为 0}
    property Height: Integer read GetHeight;
    {* 树高度，只有 Root 时为 1}
    property RegisteredCount: Integer read GetRegisteredCount;
    {* 返回树中所有注册过的子节点的数目 }
  published
    property OnDepthFirstTravelLeaf: TNotifyEvent read FOnDepthFirstTravelLeaf write FOnDepthFirstTravelLeaf;
    {* 深度优先遍历时遍历到一个叶节点时的触发事件，Sender 是此节点 }
    property OnWidthFirstTravelLeaf: TNotifyEvent read FOnWidthFirstTravelLeaf write FOnWidthFirstTravelLeaf;
    {* 广度优先遍历时遍历到一个叶节点时的触发事件，Sender 是此节点 }
{$IFDEF MSWINDOWS}
    property OnLoadANode: TCnTreeNodeEvent read FOnLoadANode write FOnLoadANode;
    {* 从 VCL 的 TreeView 中载入节点时针对每一个节点的触发事件 }
    property OnSaveANode: TCnTreeNodeEvent read FOnSaveANode write FOnSaveANode;
    {* 将节点存入 VCL 的 TreeView 时针对每一个节点的触发事件 }
{$ENDIF}
{$IFDEF SUPPORT_FMX}
    property OnLoadAItem: TCnTreeViewItemEvent read FOnLoadAItem write FOnLoadAItem;
    {* 从 VCL 的 TreeView 中载入节点时针对每一个节点的触发事件 }
    property OnSaveAItem: TCnTreeViewItemEvent read FOnSaveAItem write FOnSaveAItem;
    {* 将节点存入 VCL 的 TreeView 时针对每一个节点的触发事件 }
{$ENDIF}
  end;

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
    function GetSubTreeHeight: Integer; override;
    function GetTree: TCnBinaryTree;
  protected
    procedure DoPreOrderTravel;
    procedure DoInOrderTravel;
    procedure DoPostOrderTravel;
  public
    constructor Create(ATree: TCnTree); override;
    function IsBalance: Boolean;
    {* 以此节点为根节点的子二叉树是否是平衡二叉树}
    property LeftLeaf: TCnBinaryLeaf read GetLeftLeaf write SetLeftLeaf;
    {* 左子节点，使用第 0 个子节点，无则返回 nil}
    property RightLeaf: TCnBinaryLeaf read GetRightLeaf write SetRightLeaf;
    {* 右子节点，使用第 1 个子节点，无则返回 nil}

    property Tree: TCnBinaryTree read GetTree;
    {* 所属树，一个叶必须属于一棵树 }
  end;

  TCnBinaryLeafClass = class of TCnBinaryLeaf;

  TCnBinaryTree = class(TCnTree)
  {* 二叉树实现类}
  private
    FOnPostOrderTravelLeaf: TNotifyEvent;
    FOnInOrderTravelLeaf: TNotifyEvent;
    FOnPreOrderTravelLeaf: TNotifyEvent;
    function GetHeight: Integer; override;
  protected
    function DefaultLeafClass: TCnLeafClass; override;
    procedure ValidateComingLeaf(AParent, AChild: TCnLeaf); override;

    function GetRoot: TCnBinaryLeaf;
    function GetCount: Integer;

    procedure DoPreOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoInOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;
    procedure DoPostOrderTravelLeaf(ALeaf: TCnBinaryLeaf); virtual;

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
    {* 删除指定节点的左子节点，也就是置 nil}
    procedure DeleteRightChild(AParent: TCnBinaryLeaf);
    {* 删除指定节点的右子节点，也就是置 nil}

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

    property Root: TCnBinaryLeaf read GetRoot;
    {* 根节点，总是存在 }
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


{$IFDEF SUPPORT_FMX}

function GetNextSiblingItem(Item: TTreeViewItem): TTreeViewItem;
var
  P: TTreeViewItem;
  T: TCustomTreeView;
begin
  Result := nil;
  P := Item.ParentItem;
  if P <> nil then
  begin
    if (Item.Index >= 0) and (Item.Index < P.Count - 1) then
      Result := P.ItemByIndex(Item.Index + 1);
    Exit;
  end;

  T := Item.TreeView;
  if T <> nil then
  begin
    if (Item.Index >= 0) and (Item.Index < T.Count - 1) then
      Result := T.Items[Item.Index + 1];
  end;
end;

{$ENDIF}

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
  FTree.ValidateComingLeaf(Self, ALeaf);
  Result := ALeaf;
  FList.Add(Result);
  Result.FParent := Self;
end;

function TCnLeaf.AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
begin
  Assert(ALeaf.Tree = Self.FTree);
  FTree.ValidateComingLeaf(Self, ALeaf);
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
  Queue: TQueue;
  I: Integer;
  Node: TCnLeaf;
begin
  // 广度优先遍历并非子节点的递归，而是看成一体地使用队列
  if FTree <> nil then
    FTree.DoWidthFirstTravelLeaf(Self);
  Queue := TQueue.Create;
  try
    for I := 0 to FList.Count - 1 do
      Queue.Push(Items[I]);

    while Queue.Count > 0 do
    begin
      Node := TCnLeaf(Queue.Pop);
      if FTree <> nil then
        FTree.DoWidthFirstTravelLeaf(Node);

      if Node.Count > 0 then
        for I := 0 to Node.Count - 1 do
          Queue.Push(Node.Items[I]);
    end;
  finally
    Queue.Free;
  end;
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

      if Items[I] <> nil then
        ACount := Items[I].AllCount + 1
      else
        ACount := 1;

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
    if Items[I] <> nil then
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
    Result := 0
  else
    Result := FParent.Level + 1;
end;

function TCnLeaf.GetAllNonNilCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Self.Count - 1 do
    if Items[I] <> nil then
      Result := Result + Self.Items[I].AllNonNilCount + 1;
end;

function TCnLeaf.GetSubTreeHeight: Integer;
var
  I, MaxChildHeight: Integer;
begin
  Result := 0;
  if not HasChildren then
    Exit;

  MaxChildHeight := 0;
  for I := 0 to FList.Count - 1 do
  begin
    if FList[I] <> nil then
    begin
      if MaxChildHeight = 0 then // 有不为 nil 的子节点，深度至少 1
        MaxChildHeight := 1;

      if TCnLeaf(FList[I]).SubTreeHeight > MaxChildHeight then
        MaxChildHeight := TCnLeaf(FList[I]).SubTreeHeight;
    end;
  end;
  Result := MaxChildHeight + 1;
end;

function TCnLeaf.GetTree: TCnTree;
begin
  Result := FTree;
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

procedure TCnLeaf.SetItems(Index: Integer; const Value: TCnLeaf);
begin
  if (Index >= 0) and (Index < Count) then
  begin
    FList.Items[Index] := Value;
    Value.FParent := Self;
  end;
end;

procedure TCnLeaf.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnLeaf then
  begin
    TCnLeaf(Dest).Data := FData;
    TCnLeaf(Dest).Text := FText;
    TCnLeaf(Dest).Obj := FObj;
  end
  else
    inherited;
end;

function TCnLeaf.GetAbsoluteIndexFromParent(IndirectParentLeaf: TCnLeaf): Integer;
var
  I, Idx: Integer;
begin
  Result := -1;
  if FParent = IndirectParentLeaf then
  begin
    Idx := Index;
    Result := 0;
    for I := 0 to Idx - 1 do
      Result := Result + IndirectParentLeaf.Items[I].AllCount + 1;
  end
  else if HasAsParent(IndirectParentLeaf) then
  begin
    Result := FParent.GetAbsoluteIndexFromParent(IndirectParentLeaf)
      + GetAbsoluteIndexFromParent(FParent) + 1;
  end;
end;

//==============================================================================
// TCnTree
//==============================================================================

constructor TCnTree.Create;
begin
  inherited;
  FLeaves := TObjectList.Create(True);
  if FLeafClass = nil then
    FLeafClass := DefaultLeafClass;
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

function TCnTree.GetMaxLevel: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (Items[I] <> nil) and (Items[I].Level > Result) then
      Result := Items[I].Level;
end;

function TCnTree.GetRegisteredCount: Integer;
begin
  Result := FLeaves.Count;
end;

{$IFDEF MSWINDOWS}

procedure TCnTree.LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode;
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

procedure TCnTree.SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode;
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
        RootLeaf := FRoot;
      if RootLeaf.Count > 0 then
      begin
        ANode := RootNode;
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf 的子节点，RootLeaf 不参与交互
          if ALeaf = nil then
            Continue;
          ANode := ATreeView.Items.Add(ANode, '');
          SaveToATreeNode(ALeaf, ANode);
        end;
      end;
    finally
      ATreeView.Items.EndUpdate;
    end;
  end;
end;

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnTree.LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnLeaf);
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
        RootLeaf := FRoot;

      ALeaf := Self.AddChild(RootLeaf);
      LoadFromATreeViewItem(ALeaf, AItem);
      if RootItem <> nil then Exit;
      // 声明了 RootNode 时以 RootNode 为根，所以不处理 RootNode 的同层节点

      AItem := GetNextSiblingItem(AItem); // 遍历此层的其他后继节点
      while AItem <> nil do
      begin
        ALeaf := Self.AddChild(RootLeaf);
        LoadFromATreeViewItem(ALeaf, AItem);
        AItem := GetNextSiblingItem(AItem);
      end;
    end;
  end;
end;

procedure TCnTree.SaveToTreeView(ATreeView: FMX.TreeView.TTreeView;
  RootItem: TTreeViewItem; RootLeaf: TCnLeaf);
var
  I: Integer;
  AItem: TTreeViewItem;
  ALeaf: TCnLeaf;
begin
  if (RootLeaf <> nil) and (RootLeaf.Tree <> Self) then Exit;
  if (RootItem <> nil) and (RootItem.TreeView <> ATreeView) then Exit;

  if ATreeView <> nil then
  begin
    ATreeView.BeginUpdate;
    try
      if RootItem <> nil then
        RootItem.DeleteChildren
      else
        ATreeView.Clear;

      if RootLeaf = nil then
        RootLeaf := FRoot;
      if RootLeaf.Count > 0 then
      begin
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf 的子节点，RootLeaf 不参与交互
          if ALeaf = nil then
            Continue;

          AItem := TTreeViewItem.Create(ATreeView);
          if RootItem = nil then
            AItem.Parent := ATreeView
          else
            AItem.Parent := RootItem;

          SaveToATreeViewItem(ALeaf, AItem);
        end;
      end;
    finally
      ATreeView.EndUpdate;
    end;
  end;
end;

{$ENDIF}

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

{$IFDEF MSWINDOWS}

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
  if (ANode <> nil) and (ALeaf <> nil) and (ANode.TreeView is ComCtrls.TTreeView) then
  begin
    if DoSaveToATreeNode(ALeaf, ANode) then
    begin
      for I := 0 to ALeaf.Count - 1 do
      begin
        if ALeaf.Items[I] = nil then
          Continue;
        Node := (ANode.TreeView as ComCtrls.TTreeView).Items.AddChild(ANode, '');
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

{$ENDIF}

{$IFDEF SUPPORT_FMX}

procedure TCnTree.LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem);
var
  I: Integer;
  Leaf: TCnLeaf;
begin
  if (AItem <> nil) and (ALeaf <> nil) then
  begin
    if DoLoadFromATreeViewItem(ALeaf, AItem) then
    begin
      for I := 0 to AItem.Count - 1 do
      begin
        Leaf := AddChild(ALeaf);
        LoadFromATreeViewItem(Leaf, AItem.Items[I]);
      end;
    end
    else
    begin
      ALeaf.Delete;
    end;
  end;
end;

procedure TCnTree.SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem);
var
  I: Integer;
  Item: TTreeViewItem;
begin
  if (AItem <> nil) and (ALeaf <> nil) and (AItem.TreeView is TTreeView) then
  begin
    if DoSaveToATreeViewItem(ALeaf, AItem) then
    begin
      for I := 0 to ALeaf.Count - 1 do
      begin
        if ALeaf.Items[I] = nil then
          Continue;

        Item := TTreeViewItem.Create(AItem.TreeView);
        Item.Parent := AItem;
        SaveToATreeViewItem(ALeaf.Items[I], Item);
      end;
    end
    else
    begin
      AItem.Free;
    end;
  end;
end;

function TCnTree.DoLoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
begin
  Result := True;
  if Assigned(FOnLoadAItem) then
    FOnLoadAItem(ALeaf, AItem, Result)
  else
  begin
    ALeaf.Text := AItem.Text;
    try
      ALeaf.Data := AItem.Data.AsInteger;
    except
      ALeaf.Data := 0;
    end;
  end;
end;

function TCnTree.DoSaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
begin
  Result := True;
  if Assigned(FOnSaveAItem) then
  begin
    FOnSaveAItem(ALeaf, AItem, Result);
  end
  else
  begin
    AItem.Text := ALeaf.Text;
    AItem.Tag := ALeaf.Data; // Data 会影响 Text，改用 Tag
  end;
end;

{$ENDIF}

procedure TCnTree.ValidateComingLeaf(AParent, AChild: TCnLeaf);
begin

end;

procedure TCnTree.AssignLeafAndChildren(Source, DestLeaf: TCnLeaf; DestTree: TCnTree);
var
  I: Integer;
  Leaf: TCnLeaf;
begin
  if (Source <> nil) and (DestLeaf <> nil) and (DestTree <> nil) then
  begin
    DestLeaf.Assign(Source);
    DestLeaf.Clear;
    for I := 0 to Source.Count - 1 do
    begin
      Leaf := DestTree.CreateLeaf(DestTree);
      DestLeaf.AddChild(Leaf);
      AssignLeafAndChildren(Source.Items[I], Leaf, DestTree);
    end;
  end;
end;

procedure TCnTree.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnTree then
  begin
    TCnTree(Dest).Clear;
    // 完全克隆树节点的结构
    AssignLeafAndChildren(FRoot, TCnTree(Dest).Root, TCnTree(Dest));
  end
  else
    inherited;
end;

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

function TCnTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnLeaf;
end;

procedure TCnBinaryTree.DeleteLeftChild(AParent: TCnBinaryLeaf);
begin
  if (AParent.Tree = Self) then
    AParent.LeftLeaf := nil;
end;

procedure TCnBinaryTree.DeleteRightChild(AParent: TCnBinaryLeaf);
begin
  if (AParent.Tree = Self) then
    AParent.RightLeaf := nil;
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
        LoadFromATreeNode(Leaf, ANode.Item[0]);
      end;
      if ANode.Count > 1 then
      begin
        Leaf := AddRightChild(ALeaf as TCnBinaryLeaf);
        LoadFromATreeNode(Leaf, ANode.Item[1]);
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

constructor TCnBinaryLeaf.Create(ATree: TCnTree);
begin
  inherited;
  FList.Add(nil);  // 左子节点
  FList.Add(nil);  // 右子节点
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

function TCnBinaryLeaf.GetLeftLeaf: TCnBinaryLeaf;
begin
  Result := nil;
  if Count > 0 then
    Result := TCnBinaryLeaf(Items[0]);
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

  if (Value <> Items[0]) and (Items[0] <> nil) then
    Items[0].Delete;

  Items[0] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

procedure TCnBinaryLeaf.SetRightLeaf(const Value: TCnBinaryLeaf);
begin
  if Value <> nil then
    Assert(Value.Tree = Self.FTree);

  if (Value <> Items[1]) and (Items[1] <> nil) then
    Items[1].Delete;

  Items[1] := Value;
  if Value <> nil then
    Value.FParent := Self;
end;

function TCnTree.GetHeight: Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := Root.SubTreeHeight + 1;
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

end.
