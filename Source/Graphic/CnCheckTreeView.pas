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

unit CnCheckTreeView;
{* |<PRE>
================================================================================
* ������ƣ����������
* ��Ԫ���ƣ�������� TreeView �ؼ���Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
*           CnPack �����飨master@cnpack.org)
* ��    ע��ʹ�� TreeNode �� StateIndex ������������ã��û������ֶ����ĸ�ֵ
*           ʹ�� OverlayIndex ������ Enable ���ã��� CanDisableNode ����Ϊ True
*           ʱ���û������ֶ����ĸ�ֵ������ OnCustomDrawItem �¼��������� Node ��
*           Disable ʱ��Ч�����û�����д���¼������������Ļ��ơ�
*           ע��Delphi 12 �¶� Check ״̬�ĳ�ʼ�������� Form �� Create �¼�������
*           ���� Show �¼��������������Ĺ�ѡ״̬��ʾȫ�Ƿ�ʵ���ڲ�������ȷ�ġ�
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.09.03 V1.3
*               ֧�� FPC
*           2008.04.10 V1.2
*               �޸� SyncParentNode��SyncChildNode ������
*               ʹ������ CheckBox ��ʱ��Ż�ͬ���ڵ��״̬ by Jackson.He
*               ����һ���� HideCheckBox ��������ĳ�ڵ�ĸ�ѡ��
*           2007.11.07 V1.1
*               ���� NodeEnabled �Ĺ��ܣ����������� CanDisableNode ������
*           2003.03.28 V1.0
*               ������Ԫ
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

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnCheckTreeView = class(TTreeView)
  private
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
{$IFDEF FPC}
var
  Bmp: TBitmap;
{$ENDIF}
begin
  inherited;
  FStateImages := TImageList.CreateSize(16, 16);

{$IFDEF FPC}
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromResourceName(hInstance, 'CNTREEVIEWSTATE');
    Bmp.TransparentColor:= clFuchsia;
    Bmp.Transparent := True;
    FStateImages.AddSliced(Bmp, Bmp.Width div FStateImages.Width, 1);
  finally
    Bmp.Free;
  end;
{$ELSE}
  FStateImages.Handle := ImageList_LoadBitmap(hInstance, 'CNTREEVIEWSTATE',
    16, 0, clFuchsia);
{$ENDIF}

  StateImages := FStateImages;

{$IFDEF DELPHI120_ATHENS_UP}
  CheckBoxes := True;
{$ENDIF}
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
  ������:    TCnCheckTreeView.GetCheckBoxState
  ����:      jackson.he
  ����:      2008.04.11
  ����:      Node: TTreeNode
  ����:      ���ؽڵ�� Check ״̬��ע���û�� Check ��Ľڵ��״̬����Ϊ unChecked
  ����ֵ:    TCheckBoxState
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
  // �������ֹ����ò���ѡ��״̬
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
    if not FCanDisableNode or GetNodeEnabled(Node) then // Enable �Ĳ�����ı�
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
  I: Integer;

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
      Node.StateIndex := Ord(cbUnchecked) + 1;

    Result := GetCheckBoxState(Node);
  end;

begin
  for I := 0 to Items.Count - 1 do
    DoSyncNodeState(Items[I]);
end;

// ͬ���ӽڵ�״̬���ӽڵ㿽�����ڵ�״̬
procedure TCnCheckTreeView.SyncChildNode(Node: TTreeNode);
var
  I: Integer;
  ParentNode: TTreeNode;
begin
  if Node.StateIndex <> Ord(cbGrayed) + 1 then
  begin
    for I := 0 to Node.Count - 1 do
    begin
      // ͬ�����ӽڵ� Node[I] �������� Check �����͵Ľڵ�
      if TCheckBoxState(Node[I].StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed] then
      begin
        ParentNode := Node;
        if not (TCheckBoxState(Node.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
        begin
          // �����ǰ�ڵ� Node ��״̬��û�� Check ��ģ�������������� Check ��ĸ��ڵ��״̬
          while (ParentNode <> Items.Item[0]) and
            (not (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed])) do
            ParentNode := ParentNode.Parent;

          Node[I].StateIndex := ParentNode.StateIndex;
        end
        else
          Node[I].StateIndex := Node.StateIndex; // �����ǰ�ڵ� Node ��״̬�� Check ��ģ�ֱ��ͬ��
      end;
      SyncChildNode(Node[I]);
    end;
  end;
end;

// ͬ�����ڵ�״̬, ���ڵ㿽���ӽڵ�״̬
procedure TCnCheckTreeView.SyncParentNode(Node: TTreeNode);
var
  SelCount, UnSelCount, Count: Integer;
  ParentNode, ChildNode: TTreeNode;
  SubSelCount, SubUnSelCount, SubCount: Integer;

  // �����ǰ�ڵ� Node ��û�� Check �ģ����ҵ�ǰ�ڵ����� Check ���ѡ��ĺ�û��ѡ��Ľڵ�����
  procedure CheckNode(Node: TTreeNode);
  var
    I: Integer;
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
      for I := 0 to Node.Count - 1 do
        CheckNode(Node[I]);
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
      // ��� Check ״̬������ GetCheckBoxState �ˡ�
      if TCheckBoxState(ChildNode.StateIndex - 1) = cbChecked then
        Inc(SelCount)  // ��� Check ״̬������ GetCheckBoxState �ˡ�
      else if TCheckBoxState(ChildNode.StateIndex - 1) = cbUnchecked then
        Inc(UnSelCount)
      else if not (TCheckBoxState(ChildNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
      begin
        // ��� ChildNode �ڵ���û�� Check ��ģ�
        // ����Ҫ���� Check �ڵ�������Լ��� Check ��û��ѡ�е������Լ�ѡ�е�����״̬
        SubUnSelCount := 0;
        SubSelCount := 0;
        SubCount := 0;
        CheckNode(ChildNode);
        if SubUnSelCount = SubCount then
          Inc(UnSelCount);

        if SubSelCount = SubCount then
          Inc(SelCount);
      end;

      Inc(Count);
      ChildNode := ParentNode.GetNextChild(ChildNode);
    end;

    if SelCount = Count then
    begin
      if (ParentNode.StateIndex <> Ord(cbChecked) + 1) and
        (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
        ParentNode.StateIndex := Ord(cbChecked) + 1;
    end
    else if UnSelCount = Count then
    begin
      if (ParentNode.StateIndex <> Ord(cbUnchecked) + 1) and
        (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
        ParentNode.StateIndex := Ord(cbUnchecked) + 1;
    end
    else if (ParentNode.StateIndex <> Ord(cbGrayed) + 1) and
      (TCheckBoxState(ParentNode.StateIndex - 1) in [cbUnchecked, cbChecked, cbGrayed]) then
      ParentNode.StateIndex := Ord(cbGrayed) + 1;

    SyncParentNode(ParentNode);
  end;
end;

procedure TCnCheckTreeView.WndProc(var message: TMessage);
begin
  case message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        if ToggleCheckedByPos(TWMLButtonDown(message).XPos,
          TWMLButtonDown(message).YPos) then
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
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TCnCheckTreeView.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
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

    if not Value then // ��ֹʹ�� NodeEnable ����ʱ���ָ����� Enabled �� Node
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

