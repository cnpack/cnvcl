{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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

unit CnCheckTreeView;
{* |<PRE>
================================================================================
* 软件名称：界面组件包
* 单元名称：带检查框的 TreeView 控件单元
* 单元作者：周劲羽 (zjy@cnpack.org)
*           刘啸（liuxiao@cnpack.org)
* 备    注：使用 TreeNode 的 StateIndex 来保存检查框设置，用户请勿手动更改该值
            使用 OverlayIndex 来保存 Enable 设置，当 CanDisableNode 属性为 True
            时，用户请勿手动更改该值，另外 OnCustomDrawItem 事件用来绘制 Node 在
            Disable 时的效果，用户可重写此事件进行其他风格的绘制。
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.04.10 V1.2
*               修改SyncParentNode、SyncChildNode函数，
*               使其在有 CheckBox 的时候才会同步节点的状态 by Jackson.He
*               增加一方法 HideCheckBox 用来隐藏某节点的复选框
*           2007.11.07 V1.1
*               加入 NodeEnabled 的功能，总体用属性 CanDisableNode 来控制
*           2003.03.28 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Commctrl, StdCtrls;
  
type

{ TCnCheckTreeView }

  TCnStateChangeEvent = procedure (Sender: TObject; Node: TTreeNode;
    OldState, NewState: TCheckBoxState) of object;

  TCnCheckTreeView = class(TTreeView)
  private
    { Private declarations }
    FStateImages: TImageList;
    FUpdateCount: Integer;
    FOnStateChange: TCnStateChangeEvent;
    FOnNodeEnabledChange: TTVChangedEvent;
    FCanDisableNode: Boolean;
    function GetCheckBoxState(Node: TTreeNode): TCheckBoxState;
    procedure SetCheckBoxState(Node: TTreeNode;
      const Value: TCheckBoxState);
    function GetChecked(Node: TTreeNode): Boolean;
    procedure SetChecked(Node: TTreeNode; const Value: Boolean);
    function GetNodeEnabled(Node: TTreeNode): Boolean;
    procedure SetNodeEnabled(Node: TTreeNode; const Value: Boolean);
    procedure SetCanDisableNode(const Value: Boolean);
  protected
    { Protected declarations }
    procedure WndProc(var message: TMessage); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    function CanChange(Node: TTreeNode): Boolean; override;
    procedure DoCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure DoNodeEnabledChange(Node: TTreeNode); virtual;
    function ToggleCheckedByPos(x, y: Integer): Boolean;
    function ToggleChecked(Node: TTreeNode): Boolean;
    procedure SyncChildNode(Node: TTreeNode);
    procedure SyncParentNode(Node: TTreeNode);
    procedure SetUpdateState(Updating: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SyncNodes;
    procedure SelectAll;
    procedure SelectNone;
    procedure SelectInvert;
    procedure HideCheckBox(Node: TTreeNode);

    property CheckBoxState[Node: TTreeNode]: TCheckBoxState read GetCheckBoxState
      write SetCheckBoxState;
    property Checked[Node: TTreeNode]: Boolean read GetChecked write SetChecked;
    property NodeEnabled[Node: TTreeNode]: Boolean read GetNodeEnabled write SetNodeEnabled;
  published
    property CanDisableNode: Boolean read FCanDisableNode write SetCanDisableNode;
    property OnStateChange: TCnStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnNodeEnabledChange: TTVChangedEvent read FOnNodeEnabledChange write FOnNodeEnabledChange;
  end;

implementation

{$R CnCheckTreeView.res}

{ TCnCheckTreeView }

constructor TCnCheckTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FStateImages := TImageList.CreateSize(16, 16);
  FStateImages.Handle := ImageList_LoadBitmap(hInstance, 'CNTREEVIEWSTATE',
    16, 0, clFuchsia);
  StateImages := FStateImages;
  OnCustomDrawItem := DoCustomDrawItem;
end;

destructor TCnCheckTreeView.Destroy;
begin
  FStateImages.Free;
  inherited;
end;

procedure TCnCheckTreeView.Loaded;
begin
  inherited Loaded;
  SyncNodes;
end;

{-------------------------------------------------------------------------------
  过程名:    TCnCheckTreeView.GetCheckBoxState
  作者:      jackson.he
  日期:      2008.04.11
  参数:      Node: TTreeNode
  功能:      返回节点的check状态，注意对没有check框的节点的状态返回为unchecked
  返回值:    TCheckBoxState
-------------------------------------------------------------------------------}
function TCnCheckTreeView.GetCheckBoxState(Node: TTreeNode): TCheckBoxState;
begin
  if Node.StateIndex in [1..3] then
    Result := TCheckBoxState(Node.StateIndex - 1)
  else
    Result := cbUnchecked;
end;

procedure TCnCheckTreeView.SetCheckBoxState(Node: TTreeNode;
  const Value: TCheckBoxState);
var
  OldState: TCheckBoxState;
begin
  // 不允许手工设置部分选择状态
  if (Node.StateIndex - 1 <> Ord(Value)) and (Value <> cbGrayed) then
  begin
    OldState := TCheckBoxState(Node.StateIndex - 1);
    Node.StateIndex := Ord(Value) + 1;
    if FUpdateCount = 0 then
    begin
      SyncChildNode(Node);
      SyncParentNode(Node);
      if Assigned(FOnStateChange) then
        FOnStateChange(Self, Node, OldState, Value);
    end;      
  end;
end;

function TCnCheckTreeView.GetChecked(Node: TTreeNode): Boolean;
begin
  Result := GetCheckBoxState(Node) = cbChecked;
end;

procedure TCnCheckTreeView.SetChecked(Node: TTreeNode;
  const Value: Boolean);
begin
  if Value then
    SetCheckBoxState(Node, cbChecked)
  else
    SetCheckBoxState(Node, cbUnchecked);
end;

function TCnCheckTreeView.ToggleChecked(Node: TTreeNode): Boolean;
begin
  if Assigned(Node) then
  begin
    SetChecked(Node, not GetChecked(Node));
    Result := True;
  end
  else
    Result := False;
end;

function TCnCheckTreeView.ToggleCheckedByPos(x, y: Integer): Boolean;
var
  Node: TTreeNode;
begin
  Result := False;
  if htOnStateIcon in GetHitTestInfoAt(x, y) then
  begin
    Node := GetNodeAt(x, y);
    if not FCanDisableNode or GetNodeEnabled(Node) then // Enable 的才允许改变
      Result := ToggleChecked(Node);
  end;
end;

procedure TCnCheckTreeView.KeyPress(var Key: Char);
begin
  if (Key = ' ') and Assigned(Selected) then
    ToggleChecked(Selected)
  else
    inherited;
end;

procedure TCnCheckTreeView.SyncNodes;
var
  i: Integer;

  function DoSyncNodeState(Node: TTreeNode): TCheckBoxState;
  var
    SelCount, UnSelCount, Count: Integer;
    ChildNode: TTreeNode;
  begin
    ChildNode := Node.GetFirstChild;
    if ChildNode <> nil then
    begin
      SelCount := 0;
      UnSelCount := 0;
      Count := 0;
      while ChildNode <> nil do
      begin
        if DoSyncNodeState(ChildNode) = cbChecked then
          Inc(SelCount)
        else if DoSyncNodeState(ChildNode) = cbUnchecked then
          Inc(UnSelCount);
        Inc(Count);
        ChildNode := Node.GetNextChild(ChildNode);
      end;

      if SelCount = Count then
      begin
        if Node.StateIndex <> Ord(cbChecked) + 1 then
          Node.StateIndex := Ord(cbChecked) + 1;
      end
      else if UnSelCount = Count then
      begin
        if Node.StateIndex <> Ord(cbUnchecked) + 1 then
          Node.StateIndex := Ord(cbUnchecked) + 1;
      end
      else if Node.StateIndex <> Ord(cbGrayed) + 1 then
      begin
        Node.StateIndex := Ord(cbGrayed) + 1;
      end;
    end
    else if not (Node.StateIndex in [1..3]) then
    begin
      Node.StateIndex := Ord(cbUnchecked) + 1;
    end;

    Result := GetCheckBoxState(Node);
  end;
begin
  for i := 0 to Items.Count - 1 do
    DoSyncNodeState(Items[i]);
end;

// 同步子节点状态，子节点拷贝父节点状态
procedure TCnCheckTreeView.SyncChildNode(Node: TTreeNode);
var
  i: Integer;
  ParentNode: TTreeNode;
begin
  if Node.StateIndex <> Ord(cbGrayed) + 1 then
    for i := 0 to Node.Count - 1 do
    begin
      // 同步的子节点Node.Item[i]必须是有check框类型的节点
      if TCheckBoxState(Node.Item[i].StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed] then
      begin
        ParentNode := Node;
        if not (TCheckBoxState(Node.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
        begin
          // 如果当前节点Node的状态是没有check框的，则必须往上找有check框的父节点的状态
          while (ParentNode <> Items.Item[0]) and
                (not (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed])) do
          begin
            ParentNode := ParentNode.Parent;
          end;
          Node.Item[i].StateIndex := ParentNode.StateIndex;
        end else
        begin
          // 如果当前节点Node的状态有check框的，直接同步
          Node.Item[i].StateIndex := Node.StateIndex;
        end;
      end;
      SyncChildNode(Node.Item[i]);
    end;
end;

// 同步父节点状态, 父节点拷贝子节点状态
procedure TCnCheckTreeView.SyncParentNode(Node: TTreeNode);
var
  SelCount, UnSelCount, Count: Integer;
  ParentNode, ChildNode: TTreeNode;

  SubSelCount, SubUnSelCount, SubCount: Integer;
  // 如果当前节点Node是没有check的，查找当前节点中有Check框的选择的和没有选择的节点数量
  procedure CheckNode(Node: TTreeNode);
  var
    i: Integer;
  begin
    case TCheckBoxState(Node.StateIndex - 1) of
      cbUnchecked:
        begin
          Inc(SubUnSelCount);
          Inc(SubCount);
        end;
      cbChecked:
        begin
          Inc(SubSelCount);
          Inc(SubCount);
        end;
      cbGrayed:
        begin
          Inc(SubCount);
        end;  
    end;

    if Node.HasChildren then
    begin
      for i:= 0 to Node.Count - 1 do
      begin
        CheckNode(Node.Item[i]);
      end;  
    end;
  end;  
begin
  ParentNode := Node.Parent;
  if ParentNode <> nil then
  begin
    SelCount := 0;
    UnSelCount := 0;
    Count := 0;
    ChildNode := ParentNode.GetFirstChild;
    while ChildNode <> nil do
    begin
      // 检查check状态不能用 GetCheckBoxState了。。
      if TCheckBoxState(ChildNode.StateIndex - 1) = cbChecked then
      begin
        Inc(SelCount);
      // 检查check状态不能用 GetCheckBoxState了。。
      end else if TCheckBoxState(ChildNode.StateIndex - 1) = cbUnchecked then
      begin
        Inc(UnSelCount);
      end else if not (TCheckBoxState(ChildNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
      begin
        // 如果ChildNode节点是没有check框的，
        // 则需要看有check节点的总数以及有check框没有选中的总数以及选中的总数状态
        SubUnSelCount := 0;
        SubSelCount := 0;
        SubCount := 0;
        CheckNode(ChildNode);
        if SubUnSelCount = SubCount then
        begin
          Inc(UnSelCount);
        end;

        if SubSelCount = SubCount then
        begin
          Inc(SelCount);
        end;  
      end;
      Inc(Count);
      ChildNode := ParentNode.GetNextChild(ChildNode);
    end;

    if SelCount = Count then
    begin
      if (ParentNode.StateIndex <> Ord(cbChecked) + 1) and
         (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
      begin
        ParentNode.StateIndex := Ord(cbChecked) + 1;
      end;
    end else if UnSelCount = Count then
    begin
      if (ParentNode.StateIndex <> Ord(cbUnchecked) + 1) and
         (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
      begin
        ParentNode.StateIndex := Ord(cbUnchecked) + 1;
      end;
    end else if (ParentNode.StateIndex <> Ord(cbGrayed) + 1) and
            (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
    begin
      ParentNode.StateIndex := Ord(cbGrayed) + 1;
    end;   
    SyncParentNode(ParentNode);
  end;
end;

procedure TCnCheckTreeView.WndProc(var message: TMessage);
begin
  case message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        if ToggleCheckedByPos(TWMLButtonDown(message).XPos, TWMLButtonDown(message).YPos)
          then
          Exit;
      end;
    WM_LBUTTONUP, WM_RBUTTONUP:
      begin
        if htOnStateIcon in GetHitTestInfoAt(TWMLButtonDown(message).XPos,
          TWMLButtonDown(message).YPos) then
          Exit;
      end;
  end;
  inherited WndProc(message);
end;

procedure TCnCheckTreeView.BeginUpdate;
begin
  Items.BeginUpdate;
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TCnCheckTreeView.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
  Items.EndUpdate;
end;

procedure TCnCheckTreeView.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    SyncNodes;
end;

procedure TCnCheckTreeView.SelectAll;
var
  Node: TTreeNode;
begin
  Items.BeginUpdate;
  try
    Node := Items.GetFirstNode;
    while Node <> nil do
    begin
      Node.StateIndex := Ord(cbChecked) + 1;
      Node := Node.GetNext;
    end;
  finally
    Items.EndUpdate;
  end;          
end;

procedure TCnCheckTreeView.SelectNone;
var
  Node: TTreeNode;
begin
  Items.BeginUpdate;
  try
    Node := Items.GetFirstNode;
    while Node <> nil do
    begin
      Node.StateIndex := Ord(cbUnchecked) + 1;
      Node := Node.GetNext;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TCnCheckTreeView.SelectInvert;
var
  Node: TTreeNode;
begin
  Items.BeginUpdate;
  try
    Node := Items.GetFirstNode;
    while Node <> nil do
    begin
      if Node.StateIndex = Ord(cbUnchecked) + 1 then
        Node.StateIndex := Ord(cbChecked) + 1
      else if Node.StateIndex = Ord(cbChecked) + 1 then
        Node.StateIndex := Ord(cbUnchecked) + 1;
      Node := Node.GetNext;
    end;
  finally
    Items.EndUpdate;
  end;          
end;

procedure TCnCheckTreeView.HideCheckBox(Node: TTreeNode);
begin
  if Node <> nil then
    Node.StateIndex := 0;
end;

function TCnCheckTreeView.GetNodeEnabled(Node: TTreeNode): Boolean;
begin
  if not FCanDisableNode then
    Result := True
  else
    Result := Node.OverlayIndex >= -1;
end;

procedure TCnCheckTreeView.SetNodeEnabled(Node: TTreeNode;
  const Value: Boolean);
begin
  if not FCanDisableNode then Exit;

  if Value then // Enable
  begin
    if not GetNodeEnabled(Node) then
    begin
      Node.OverlayIndex := -1;
      if FUpdateCount = 0 then
      begin
        DoNodeEnabledChange(Node);
        Invalidate;
      end;
    end;
  end
  else // Disable
  begin
    if GetNodeEnabled(Node) then
    begin
      Node.OverlayIndex := -2;
      if FUpdateCount = 0 then
      begin
        if Selected = Node then
          Selected := nil;
        DoNodeEnabledChange(Node);
      end;
    end;
  end;
end;

procedure TCnCheckTreeView.DoNodeEnabledChange(Node: TTreeNode);
begin
  if Assigned(FOnNodeEnabledChange) then
    FOnNodeEnabledChange(Self, Node);
end;

function TCnCheckTreeView.CanChange(Node: TTreeNode): Boolean;
begin
  Result := True;
  if Node = nil then
    Exit;
    
  if FCanDisableNode and not GetNodeEnabled(Node) then
    Result := False
  else
    Result := inherited CanChange(Node);
end;

procedure TCnCheckTreeView.DoCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if FCanDisableNode and not GetNodeEnabled(Node) then
  begin
    Canvas.Font.Color := clGray;
    Canvas.Brush.Color := clBtnFace;
  end;
  
  DefaultDraw := True;
end;

procedure TCnCheckTreeView.SetCanDisableNode(const Value: Boolean);
var
  I: Integer;
begin
  if FCanDisableNode <> Value then
  begin
    FCanDisableNode := Value;
    if not Value then // 禁止使用 NodeEnable 属性时，恢复所有 Enabled 的 Node
    begin
      BeginUpdate;
      try
        for I := 0 to Items.Count - 1 do
          NodeEnabled[Items[I]] := True;
      finally
        EndUpdate;
      end;
      Invalidate;
    end;
  end;
end;

end.

