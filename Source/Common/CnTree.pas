{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTree;
{* |<PRE>
================================================================================
* ������ƣ�CnPack ������Ԫ
* ��Ԫ���ƣ�ʵ�ֵ����������Ļ��൥Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������ԪΪ TCnTree �� TCnLeaf �ĵ�����������ʵ�ֵ�Ԫ��
*           TCnTree/Leaf ������ TTreeNodes/Node �Ĺ�ϵ��֧����Ⱥ͹�����ȱ�����
*           ֧�ְ�������ȵ�˳��������ֵ����ʽֱ�ӷ��ʸ����ڵ㡣
*
*           ���⣬Ĭ�ϲ�֧���� TreeView �ؼ��Ľ�������Ҫ֧�֣��붨�� ENABLE_UIINTERACT
*           ���������� ComCtrls�������ÿ����ǵ��´� FMX ����Ҳ������ Vcl ǰ׺��Ԫ��
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6 + 10.3.1
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.09.10 V2.0 by LiuXiao
*               ������������֮����������ȡ�� CnTreeClasses��
*           2019.07.14 V1.9 by LiuXiao
*               ʵ��������������������浽 TreeView ʱָ���� Node �����⡣
*           2019.04.17 V1.8 by LiuXiao
*               ֧�� Win32/Win64/MacOS��֧�� VCL �� FMX �µ� TreeView ������
*           2015.05.30 V1.7 by LiuXiao
*               �ֵ������� Ansi ���ٲ���ģʽ��
*           2015.05.22 V1.6 by LiuXiao
*               �����ֵ�����ʵ�֡�
*           2015.05.03 V1.5 by LiuXiao
*               �����������ʵ�֡�
*           2015.03.16 V1.4 by LiuXiao
*               ����������ȱ����Ĵ��󣬽� Root �� Level �ĳ� 0��
*           2005.05.08 V1.3 by Alan
*               ���� LoadFromTreeView �������� Clear ����δ���� RootLeaf �����Ĵ���
*           2004.11.02 V1.2
*               ���������Ľӿ�
*           2004.09.04 V1.1
*               ����� TreeView �����Ĺ���
*           2004.05.29 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{.$DEFINE ENABLE_UIINTERACT}
// ��������������� Tree ���ݽṹ�� TreeView �Ƚ���ؼ�����
// Ĭ�ϲ����壬��üӽ��ܿ����Ӳ���Ҫ������

// �� ENABLE_FMX ������֧�� FMX �ı�������ͷ�Ƿ�ʹ�� FMX��Ĭ�ϲ�ʹ�ã��Ա����������Ķ������̫��
{$IFNDEF ENABLE_FMX}
  {$UNDEF SUPPORT_FMX}
{$ENDIF}

uses
  SysUtils, Classes, Contnrs {$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}, ComCtrls {$ENDIF} // ��� Windows �±�������Ҳ����õ�Ԫ�����ڱ���ѡ����� Vcl ǰ׺
  {$IFDEF SUPPORT_FMX}, FMX.TreeView {$ENDIF}
  // If ComCtrls not found, please add 'Vcl' to 'Unit Scope Names' in Project Options.
  {$ENDIF};

type

//==============================================================================
// ���Ļ��࣬������������ʵ��
//==============================================================================

  ECnTreeException = class(Exception);
  {* ������쳣}

  TCnTree = class;

  TCnLeaf = class(TPersistent)
  {* ��Ҷ����}
  private
    FData: Integer;
    FText: string;
    FObj: TObject;
  protected
    FList: TList;
    FTree: TCnTree;
    FParent: TCnLeaf;
    function GetTree: TCnTree;
    function GetAllNonNilCount: Integer;
    function GetParent: TCnLeaf;
    function GetAbsoluteIndex: Integer;
    function GetAllCount: Integer;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetIndex: Integer;
    function GetItems(AIndex: Integer): TCnLeaf;
    procedure SetItems(AIndex: Integer; const Value: TCnLeaf);
    function GetLevel: Integer;
    function GetSubTreeHeight: Integer; virtual;

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoDepthFirstTravel(PreOrder: Boolean = True; Reverse: Boolean = False);
    procedure DoWidthFirstTravel(Reverse: Boolean = False);
    function SetChild(ALeaf: TCnLeaf; Index: Integer): TCnLeaf;
    {* ��ĳ�ڵ㸳ֵΪ�� Index ���ӽڵ㣬����ԭ�ڵ�ֵ}
    property AllNonNilCount: Integer read GetAllNonNilCount;
    {* ���з� nil ������ڵ���Ŀ}
  public
    constructor Create(ATree: TCnTree); virtual;
    {* ���췽������Ҫһ Tree ������}
    destructor Destroy; override;
    {* ��������}

    function AddChild(ALeaf: TCnLeaf): TCnLeaf;
    {* ���һָ�����ӽڵ���Ϊ����ֱ���ӽڵ�}
    function AddChildFirst(ALeaf: TCnLeaf): TCnLeaf;
    {* ���һָ�����ӽڵ���Ϊ��һֱ���ӽڵ�}
    function InsertChild(ALeaf: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* ��ָ����������������һ�ӽڵ�}
    procedure Clear; virtual;
    {* �������ֱ���ӽڵ㣬�ӽڵ�����Ҳ�ᱻɾ���ͷ�}
    procedure DeleteChild(AIndex: Integer); virtual;
    {* ɾ��һֱ���ӽڵ㣬�ӽڵ����»ᱻɾ���ͷ�}
    procedure Delete;
    {* ɾ��������ӽڵ�}
    function ExtractChild(AIndex: Integer): TCnLeaf; overload;
    {* �����һֱ���ӽڵ㣬�ӽڵ����²��ᱻɾ���ͷ�}
    function ExtractChild(ALeaf: TCnLeaf): TCnLeaf; overload;
    {* �����ָ����һ�ӽڵ㣬�ӽڵ����²��ᱻɾ���ͷ�}

    // ��������ڵ�ķ���
    function GetFirstChild: TCnLeaf;
    {* ��õ�һ��ֱ���ӽڵ�}
    function GetLastChild: TCnLeaf;
    {* ������һ��ֱ���ӽڵ�}
    function GetNext: TCnLeaf;
    {* ��õ�һ�ӽڵ㣬���ޣ��򷵻�ͬ���ڵ�ĺ�һ���ڵ㣬���ޣ��򷵻� nil }
    function GetNextChild(Value: TCnLeaf): TCnLeaf;
    {* ���ĳ�ӽڵ�ĺ�һͬ���ڵ㣬���򷵻� nil}
    function GetNextSibling: TCnLeaf;
    {* ���ͬ���ĺ�һ�ӽڵ㣬���򷵻� nil}
    function GetPrev: TCnLeaf;
    {* ���ͬ���ڵ��ǰһ���ڵ㣬���ޣ��򷵻ظ��ڵ㣬���ޣ��򷵻� nil}
    function GetPrevChild(Value: TCnLeaf): TCnLeaf;
    {* ���ĳһ�ӽڵ��ǰһͬ���ڵ㣬���򷵻� nil}
    function GetPrevSibling: TCnLeaf;
    {* ���ͬ����ǰһ�ӽڵ㣬���򷵻� nil}
    function GetAbsoluteItems(AAbsoluteIndex: Integer): TCnLeaf;
    {* ����������ȵı���˳���õ� n ���ӽڵ㣬������ TreeNodes �еĻ���}
    function GetAbsoluteIndexFromParent(IndirectParentLeaf: TCnLeaf): Integer;
    {* ��� Leaf �� IndirectParentLeaf ��������ȵı���˳���������0 ��ʼ��
      ��� IndirectParentLeaf ���Ǽ�ӻ�ֱ�� Parent �򷵻� -1}
    function HasAsParent(Value: TCnLeaf): Boolean;
    {* ָ���Ľڵ��Ƿ��Ǳ��ڵ���ϼ�����ϼ�}
    function IndexOf(ALeaf: TCnLeaf): Integer;
    {* ��ֱ���ӽڵ��в����Ƿ���ĳһ�ڵ㲢����������}
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    {* ���������е�����ֵ}
    property AllCount: Integer read GetAllCount;
    {* ��������ڵ���Ŀ}
    property Count: Integer read GetCount;
    {* ֱ���ӽڵ���Ŀ}
    property HasChildren: Boolean read GetHasChildren;
    {* �Ƿ����ӽڵ�}
    property Index: Integer read GetIndex;
    {* ��Ҷ�ڵ��ڸ��ڵ��б��е�˳���������� 0 ��ʼ���޸���Ϊ -1}
    property Items[AIndex: Integer]: TCnLeaf read GetItems write SetItems; default;
    {* ֱ��Ҷ�ڵ�����}
    property SubTreeHeight: Integer read GetSubTreeHeight;
    {* �˽ڵ��������������߶ȣ����ӽڵ�ʱΪ 0}

    property Level: Integer read GetLevel;
    {* ���ڵ������Root �ڵ� Level Ϊ 0}
    property Parent: TCnLeaf read GetParent;
    {* ���ڵ㣬����д}
    property Tree: TCnTree read GetTree;
    {* ��������һ��Ҷ��������һ����}
  published
    property Obj: TObject read FObj write FObj;
    {* ���Ա���һ��������}
    property Data: Integer read FData write FData;
    {* ���Ա���һ���������ԣ������� Tag}
    property Text: string read FText write FText;
    {* ���Ա���һ�ַ���������}
  end;

  ICnTreeFiler = interface(IUnknown)
  {* �����������Ľӿ� }
    ['{E81A9CE0-2D1D-11D9-BA1C-5254AB35836A}']
    procedure LoadFromFile(Instance: TPersistent; const FileName: string);
    procedure SaveToFile(Instance: TPersistent; const FileName: string);
  end;

  TCnLeafClass = class of TCnLeaf;

{$IFDEF ENABLE_UIINTERACT}
{$IFDEF MSWINDOWS}
  TCnTreeNodeEvent = procedure (ALeaf: TCnLeaf; ATreeNode: TTreeNode;
    var Valid: Boolean) of object;
{$ENDIF}

{$IFDEF SUPPORT_FMX}
  TCnTreeViewItemEvent = procedure (ALeaf: TCnLeaf; ATreeItem: TTreeViewItem;
    var Valid: Boolean) of object;
{$ENDIF}
{$ENDIF}

  TCnTree = class(TPersistent)
  {* ����������ʵ����}
  private
    FLeafClass: TCnLeafClass;
    FBatchUpdating: Boolean;
    FLeaves: TObjectList;

    FOnWidthFirstTravelLeaf: TNotifyEvent;
    FOnDepthFirstTravelLeaf: TNotifyEvent;
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    FOnSaveANode: TCnTreeNodeEvent;
    FOnLoadANode: TCnTreeNodeEvent;
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    FOnSaveAItem: TCnTreeViewItemEvent;
    FOnLoadAItem: TCnTreeViewItemEvent;
  {$ENDIF}
{$ENDIF}
    function GetMaxLevel: Integer;
    procedure AssignLeafAndChildren(Source: TCnLeaf; DestLeaf: TCnLeaf; DestTree: TCnTree);
  protected
    FRoot: TCnLeaf;  
    function DefaultLeafClass: TCnLeafClass; virtual;

    function GetRoot: TCnLeaf;
    function GetItems(AbsoluteIndex: Integer): TCnLeaf;
    function GetCount: Integer;
    function GetRegisteredCount: Integer;
    function GetHeight: Integer; virtual;

    procedure AssignTo(Dest: TPersistent); override;

    function CreateLeaf(ATree: TCnTree): TCnLeaf; virtual;
    procedure DoDepthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
    procedure DoWidthFirstTravelLeaf(ALeaf: TCnLeaf); virtual;
{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    function DoLoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
    function DoSaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode): Boolean; virtual;
  {$ENDIF}

  {$IFDEF SUPPORT_FMX}
    function DoLoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
    function DoSaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem): Boolean;
  {$ENDIF}
{$ENDIF}

    procedure ValidateComingLeaf(AParent, AChild: TCnLeaf); virtual;
    {* ��ĳ�ڵ���Ҫ����һ���ӽڵ�ʱ�����ã�������������������׳��쳣�����ؿ���}

    procedure RegisterLeaf(ALeaf: TCnLeaf);
    {* ����Ҷ�ڵ���ã������еǼǴ�Ҷ�ڵ�}
    procedure UnRegisterLeaf(ALeaf: TCnLeaf);
    {* ����Ҷ�ڵ���ã�ȡ����Ҷ�ڵ�ĵǼ�}

{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    procedure LoadFromATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* ��һ TreeNode �ڵ��������ӽڵ㣬���ݹ����}
    procedure SaveToATreeNode(ALeaf: TCnLeaf; ANode: TTreeNode); virtual;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ TreeNode�����ݹ����}
  {$ENDIF}

  {$IFDEF SUPPORT_FMX}
    procedure LoadFromATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); virtual;
    {* ��һ TreeNode �ڵ��������ӽڵ㣬���ݹ����}
    procedure SaveToATreeViewItem(ALeaf: TCnLeaf; AItem: TTreeViewItem); virtual;
    {* ���ڵ㱾���Լ��ӽڵ�д��һ TreeNode�����ݹ����}
  {$ENDIF}
{$ENDIF}
  public
    constructor Create; overload;
    {* ���췽��}
    constructor Create(LeafClass: TCnLeafClass); overload;
    {* ��һ���췽��}
    destructor Destroy; override;
    {* ��������}
    procedure DepthFirstTravel(PreOrder: Boolean = True; Reverse: Boolean = False);
    {* ����������ȱ���������ǰ���ȱ��ڵ���ӽڵ㣩�ͺ������ӽڵ�󱾽ڵ㣩���֣�ע�ⲻ�Ƕ��������û������
       Reverse ���Ʊ���ͬ���ӽڵ��˳��False ��ʾ�� 0 ��ʼ�����ң�True ��ʾ�����һ����ǰ��}
    procedure WidthFirstTravel(Reverse: Boolean = False);
    {* ���й�����ȱ�����Reverse ���Ʊ���ͬ���ӽڵ��˳��False ��ʾ�� 0 ��ʼ�����ң�True ��ʾ�����һ����ǰ��}
    function ExtractLeaf(ALeaf: TCnLeaf): TCnLeaf;
    {* �����а���һҶ�ڵ㲢������}
    procedure Clear; virtual;
    {* ������ͷ�����Ҷ�ڵ㣬��������ͷţ���������������������֪ͨ���� }

    // ������ӷ���
    function AddChildFirst(AParent: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һ���ӽڵ�}
    function AddChild(AParent: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һβ�ӽڵ�}
    function InsertChild(AParent: TCnLeaf; AIndex: Integer): TCnLeaf;
    {* ��ָ���Ľڵ�����һָ��λ�õ��ӽڵ�}
    function AddFirst(ASibing: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һͬ������ǰ�ڵ�}
    function Add(ASibing: TCnLeaf): TCnLeaf;
    {* ��ָ���Ľڵ�����һͬ�������ڵ�}

    procedure ExchangeWithChild(Leaf1: TCnLeaf; Leaf2: TCnLeaf); overload;
    {* �������ڵ�λ�ã����ӽڵ���һ�𽻻�}
    procedure ExchangeWithChild(AbsoluteIndex1: Integer; AbsoluteIndex2: Integer); overload;
    {* �������ڵ�λ�ã����ӽڵ���һ�𽻻�}
    procedure Exchange(Leaf1: TCnLeaf; Leaf2: TCnLeaf); overload;
    {* �����������ڵ�λ��}
    procedure Exchange(AbsoluteIndex1: Integer; AbsoluteIndex2: Integer); overload;
    {* �������������������ڵ�λ��}

{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    // �� TreeView �Ľ���������ע�� Root �����뽻��
    procedure LoadFromTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ��һ VCL �� TreeView ����ڵ����ݡ�RootNode ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootNode Ϊ nil ��ʾ����ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ�}
    procedure SaveToTreeView(ATreeView: ComCtrls.TTreeView; RootNode: TTreeNode = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF SUPPORT_FMX} overload; {$ENDIF}
    {* ���ڵ�����д��һ VCL �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootNode ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootNode Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
  {$ENDIF}

  {$IFDEF SUPPORT_FMX}
    procedure LoadFromTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ��һ FMX �� TreeView ����ڵ����ݡ�RootItem ���ӽڵ㱻����� RootLeaf ��ָ����
    �ڵ���ӽڵ㣬RootItem Ϊ nil ��ʾ����ȫ�� TreeNodes��RootLeaf Ϊ nil ��ʾ
    �����Ϊ Tree.Root ��ֱ���ڵ㣬Ҳ�������нڵ�}
    procedure SaveToTreeView(ATreeView: FMX.TreeView.TTreeView; RootItem: TTreeViewItem = nil;
      RootLeaf: TCnLeaf = nil); {$IFDEF MSWINDOWS} overload; {$ENDIF}
    {* ���ڵ�����д��һ FMX �� TreeView�� RootLeaf ���ӽڵ㱻д��� RootItem ��ָ����
    �ڵ���ӽڵ㣬RootLeaf Ϊ nil ��ʾд�� Root �������ӽڵ㣬��ʵҲ�������н�
    �㣬RootItem Ϊ nil ��ʾд��Ľ���Ϊ TreeView �ĸ� TreeNodes}
  {$ENDIF}
{$ENDIF}

    // ��������
    procedure LoadFromFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* ���ļ����������ڵ㣬���ṩ�ӿڵĶ���ʵ��}
    procedure SaveToFile(Filer: ICnTreeFiler; const FileName: string); virtual;
    {* �����ڵ㱣�����ļ������ṩ�ӿڵĶ���ʵ��}

    property BatchUpdating: Boolean read FBatchUpdating write FBatchUpdating;
    {* �Ƿ����������£�Ϊ True ʱҶ�ڵ��ͷ�ʱ��֪ͨ Tree}
    property Root: TCnLeaf read GetRoot;
    {* ���ڵ㣬���Ǵ���}
    property Items[AbsoluteIndex: Integer]: TCnLeaf read GetItems;
    {* ����ǰ��������ȵı���˳���õ� n ���ӽڵ㣬������ TreeNodes �еĻ��ƣ�0 ���� Root}
    property Count: Integer read GetCount;
    {* �����������нڵ����Ŀ������ Root}
    property MaxLevel: Integer read GetMaxLevel;
    {* �������������ڵ�Ĳ�����Root Ϊ 0}
    property Height: Integer read GetHeight;
    {* ���߶ȣ�ֻ�� Root ʱΪ 1}
    property RegisteredCount: Integer read GetRegisteredCount;
    {* ������������ע������ӽڵ����Ŀ}
  published
    property OnDepthFirstTravelLeaf: TNotifyEvent read FOnDepthFirstTravelLeaf write FOnDepthFirstTravelLeaf;
    {* ������ȱ���ʱ������һ��Ҷ�ڵ�ʱ�Ĵ����¼���Sender �Ǵ˽ڵ�}
    property OnWidthFirstTravelLeaf: TNotifyEvent read FOnWidthFirstTravelLeaf write FOnWidthFirstTravelLeaf;
    {* ������ȱ���ʱ������һ��Ҷ�ڵ�ʱ�Ĵ����¼���Sender �Ǵ˽ڵ�}

{$IFDEF ENABLE_UIINTERACT}
  {$IFDEF MSWINDOWS}
    property OnLoadANode: TCnTreeNodeEvent read FOnLoadANode write FOnLoadANode;
    {* �� VCL �� TreeView ������ڵ�ʱ���ÿһ���ڵ�Ĵ����¼�}
    property OnSaveANode: TCnTreeNodeEvent read FOnSaveANode write FOnSaveANode;
    {* ���ڵ���� VCL �� TreeView ʱ���ÿһ���ڵ�Ĵ����¼�}
  {$ENDIF}
  {$IFDEF SUPPORT_FMX}
    property OnLoadAItem: TCnTreeViewItemEvent read FOnLoadAItem write FOnLoadAItem;
    {* �� VCL �� TreeView ������ڵ�ʱ���ÿһ���ڵ�Ĵ����¼�}
    property OnSaveAItem: TCnTreeViewItemEvent read FOnSaveAItem write FOnSaveAItem;
    {* ���ڵ���� VCL �� TreeView ʱ���ÿһ���ڵ�Ĵ����¼�}
  {$ENDIF}
{$ENDIF}
  end;

{$IFDEF ENABLE_UIINTERACT}
{$IFDEF SUPPORT_FMX}

function GetNextSiblingItem(Item: TTreeViewItem): TTreeViewItem;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF ENABLE_UIINTERACT}
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

procedure TCnLeaf.DoDepthFirstTravel(PreOrder: Boolean; Reverse: Boolean);
var
  I: Integer;
begin
  if PreOrder then // ǰ���ȱ��ڵ����ӽڵ�
  begin
    if FTree <> nil then
      FTree.DoDepthFirstTravelLeaf(Self);

    if Reverse then
    begin
      for I := FList.Count - 1 downto 0 do
        Items[I].DoDepthFirstTravel(PreOrder, Reverse);
    end
    else
    begin
      for I := 0 to FList.Count - 1 do
        Items[I].DoDepthFirstTravel(PreOrder, Reverse);
    end;
  end
  else // �������ӽڵ��ٱ��ڵ�
  begin
    if Reverse then
    begin
      for I := FList.Count - 1 downto 0 do
        Items[I].DoDepthFirstTravel(PreOrder, Reverse);
    end
    else
    begin
      for I := 0 to FList.Count - 1 do
        Items[I].DoDepthFirstTravel(PreOrder, Reverse);
    end;

    if FTree <> nil then
      FTree.DoDepthFirstTravelLeaf(Self);
  end;
end;

procedure TCnLeaf.DoWidthFirstTravel(Reverse: Boolean);
var
  Queue: TQueue;
  I: Integer;
  Node: TCnLeaf;
begin
  // ������ȱ��������ӽڵ�ĵݹ飬���ǿ���һ���ʹ�ö���
  if FTree <> nil then
    FTree.DoWidthFirstTravelLeaf(Self);
  Queue := TQueue.Create;
  try
    if Reverse then
    begin
      for I := FList.Count - 1 downto 0 do
        Queue.Push(Items[I]);
    end
    else
    begin
      for I := 0 to FList.Count - 1 do
        Queue.Push(Items[I]);
    end;

    while Queue.Count > 0 do
    begin
      Node := TCnLeaf(Queue.Pop);
      if FTree <> nil then
        FTree.DoWidthFirstTravelLeaf(Node);

      if Node.Count > 0 then
      begin
        if Reverse then
        begin
          for I := Node.Count - 1 downto 0 do
            Queue.Push(Node.Items[I]);
        end
        else
        begin
          for I := 0 to Node.Count - 1 do
            Queue.Push(Node.Items[I]);
        end;
      end;
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

function TCnLeaf.GetItems(AIndex: Integer): TCnLeaf;
begin
  Result := TCnLeaf(FList.Items[AIndex]);
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
      if MaxChildHeight = 0 then // �в�Ϊ nil ���ӽڵ㣬������� 1
        MaxChildHeight := 1;

      if TCnLeaf(FList[I]).SubTreeHeight > MaxChildHeight then
        MaxChildHeight := TCnLeaf(FList[I]).SubTreeHeight;
    end;
  end;
  Result := MaxChildHeight + 1;
end;

function TCnLeaf.GetParent: TCnLeaf;
begin
  Result := FParent;
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
    Result := TCnLeaf(FList.Items[Index]);
    FList.Items[Index] := ALeaf;
    ALeaf.FParent := Self;
  end
  else
    Result := nil;
end;

procedure TCnLeaf.SetItems(AIndex: Integer; const Value: TCnLeaf);
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    FList.Items[AIndex] := Value;
    if Value <> nil then
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

procedure TCnTree.DepthFirstTravel(PreOrder: Boolean; Reverse: Boolean);
begin
  FRoot.DoDepthFirstTravel(PreOrder, Reverse);
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

procedure TCnTree.WidthFirstTravel(Reverse: Boolean);
begin
  FRoot.DoWidthFirstTravel(Reverse);
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
    // FRoot �Ѿ��� Fleaves �ͷţ������ٴ��ͷ�.
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
      Exit; // Ϊ���ӹ�ϵ�Ĳ�������
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
    // ���������ڵ���ӽڵ��б����ڵ㽻��������
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

    // ˳���жϸ��ڵ�
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

{$IFDEF ENABLE_UIINTERACT}
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
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := FRoot;

      ALeaf := Self.AddChild(RootLeaf);
      LoadFromATreeNode(ALeaf, ANode);
      if RootNode <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      ANode := ANode.GetNextSibling; // �����˲��������̽ڵ�
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

      // ��ָ���� RootNode���� RootLeaf д�� RootNode������ RootLeaf �����뽻��
      if RootNode <> nil then
        DoSaveToATreeNode(RootLeaf, RootNode);

      ANode := RootNode;
      if ANode = nil then // ˳������
      begin
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf ���ӽڵ�
          if ALeaf = nil then
            Continue;
          ANode := ATreeView.Items.Add(ANode, '');
          SaveToATreeNode(ALeaf, ANode);
        end;
      end
      else // �и��ڵ㣬���Ϊ���ڵ���ӽڵ�
      begin
        for I := 0 to RootLeaf.Count - 1 do
        begin
          ALeaf := RootLeaf.Items[I]; // RootLeaf ���ӽڵ�
          if ALeaf = nil then
            Continue;
          ANode := ATreeView.Items.AddChild(RootNode, '');
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
      // ��һ���ڵ�
      if RootLeaf = nil then
        RootLeaf := FRoot;

      ALeaf := Self.AddChild(RootLeaf);
      LoadFromATreeViewItem(ALeaf, AItem);
      if RootItem <> nil then Exit;
      // ������ RootNode ʱ�� RootNode Ϊ�������Բ����� RootNode ��ͬ��ڵ�

      AItem := GetNextSiblingItem(AItem); // �����˲��������̽ڵ�
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

      // ��ָ���� RootItem���� RootLeaf д�� RootItem������ RootLeaf �����뽻��
      if RootItem <> nil then
        DoSaveToATreeViewItem(RootLeaf, RootItem);

      for I := 0 to RootLeaf.Count - 1 do
      begin
        ALeaf := RootLeaf.Items[I]; // RootLeaf ���ӽڵ�
        if ALeaf = nil then
          Continue;

        AItem := TTreeViewItem.Create(ATreeView);
        if RootItem = nil then
          AItem.Parent := ATreeView
        else
          AItem.Parent := RootItem;

        SaveToATreeViewItem(ALeaf, AItem);
      end;
    finally
      ATreeView.EndUpdate;
    end;
  end;
end;

{$ENDIF}
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

{$IFDEF ENABLE_UIINTERACT}
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
{$IFDEF FPC}
        LoadFromATreeNode(Leaf, ANode.Items[I]);
{$ELSE}
        LoadFromATreeNode(Leaf, ANode.Item[I]);
{$ENDIF}
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
    AItem.Tag := ALeaf.Data; // Data ��Ӱ�� Text������ Tag
  end;
end;

{$ENDIF}
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
    // ��ȫ��¡���ڵ�Ľṹ
    AssignLeafAndChildren(FRoot, TCnTree(Dest).Root, TCnTree(Dest));
  end
  else
    inherited;
end;

function TCnTree.DefaultLeafClass: TCnLeafClass;
begin
  Result := TCnLeaf;
end;

function TCnTree.GetHeight: Integer;
begin
  if Root = nil then
    Result := 0
  else
    Result := Root.SubTreeHeight + 1;
end;

end.
