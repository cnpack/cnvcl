unit UnitVirtualControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnVirtualControl;

type
  TfrmMain = class(TForm)
  private
    CnVirtualControl1: TCnVirtualControlList;
  published
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    procedure CnVirtualControl1ItemClick(Sender: TObject; Index: Integer);
    procedure CnVirtualControl1Action(Sender: TObject; Index: Integer);
    procedure MeasureItem(Sender: TObject; Index: Integer; var Height: Integer);
    procedure GetItemType(Sender: TObject; Index: Integer; var ItemType: Integer);
    procedure BuildTemplate(Sender: TObject; ItemType: Integer; Container: TWinControl);
    procedure BindItem(Sender: TObject; Index: Integer; ItemType: Integer; Container: TWinControl; Selected: Boolean);
    procedure ActionButtonClick(Sender: TObject);
    procedure LabelClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CnVirtualControl1 := TCnVirtualControlList.Create(Self);
  CnVirtualControl1.Parent := Self;
  CnVirtualControl1.Align := alClient;
  CnVirtualControl1.TabStop := True;
  CnVirtualControl1.UseItemControls := True;
  CnVirtualControl1.ItemHeight := 28;
  CnVirtualControl1.ItemSpacing := 3;
  CnVirtualControl1.OnMeasureItem := MeasureItem;
  CnVirtualControl1.OnGetItemType := GetItemType;
  CnVirtualControl1.OnBuildItemTemplate := BuildTemplate;
  CnVirtualControl1.OnBindItem := BindItem;
  CnVirtualControl1.OnItemClick := CnVirtualControl1ItemClick;
  CnVirtualControl1.OnItemAction := CnVirtualControl1Action;
  CnVirtualControl1.ItemCount := 3000;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  CnVirtualControl1 := nil;
end;

procedure TfrmMain.CnVirtualControl1ItemClick(Sender: TObject; Index: Integer);
begin
  Caption := Format('VirtualControl Demo - Selected: %d', [Index]);
end;

procedure TfrmMain.CnVirtualControl1Action(Sender: TObject; Index: Integer);
begin
  Caption := Format('Action on item %d', [Index]);
end;
procedure TfrmMain.MeasureItem(Sender: TObject; Index: Integer; var Height: Integer);
begin
  // Height based on item type
  case (Index mod 3) of
    0: Height := 28;
    1: Height := 36;
    2: Height := 60;
  else
    Height := 28;
  end;
end;

procedure TfrmMain.GetItemType(Sender: TObject; Index: Integer; var ItemType: Integer);
begin
  ItemType := Index mod 3;
end;

procedure TfrmMain.BuildTemplate(Sender: TObject; ItemType: Integer; Container: TWinControl);
var
  L: TLabel;
  B: TButton;
  M: TMemo;
begin
  case ItemType of
    0:
      begin
        L := TLabel.Create(Container);
        L.Parent := Container;
        L.Left := 8;
        L.Top := 6;
        L.Cursor := crHandPoint;
        L.OnClick := LabelClick;
      end;
    1:
      begin
        L := TLabel.Create(Container);
        L.Parent := Container;
        L.Left := 8;
        L.Top := 8;
        B := TButton.Create(Container);
        B.Parent := Container;
        B.Caption := 'Action';
        B.Width := 60;
        B.Height := 24;
        B.Top := 6;
        B.Left := Container.Width - B.Width - 8;
        B.Anchors := [akTop, akRight];
        B.OnClick := ActionButtonClick;
      end;
    2:
      begin
        L := TLabel.Create(Container);
        L.Parent := Container;
        L.Left := 8;
        L.Top := 6;
        M := TMemo.Create(Container);
        M.Parent := Container;
        M.Left := 8;
        M.Top := 24;
        M.Width := Container.Width - 16;
        M.Height := 32;
        M.ScrollBars := ssVertical;
        M.ReadOnly := True;
        M.WordWrap := True;
        M.Anchors := [akLeft, akTop, akRight];
      end;
  end;
end;

procedure TfrmMain.BindItem(Sender: TObject; Index: Integer; ItemType: Integer; Container: TWinControl; Selected: Boolean);
var
  I: Integer;
begin
  Container.Tag := Index;
  for I := 0 to Container.ControlCount - 1 do
    if Container.Controls[I] is TLabel then
      TLabel(Container.Controls[I]).Caption := Format('Item %d', [Index]);
  for I := 0 to Container.ControlCount - 1 do
    if Container.Controls[I] is TMemo then
      TMemo(Container.Controls[I]).Text := Format('Notes for item %d'#13#10'This is a demo memo.', [Index]);
  if Selected then
    TPanel(Container).Color := clHighlight
  else
  begin
    case ItemType of
      0: TPanel(Container).Color := clWindow;
      1: TPanel(Container).Color := RGB(230, 240, 255);
      2: TPanel(Container).Color := RGB(230, 255, 230);
    else
      TPanel(Container).Color := clWindow;
    end;
  end;
end;

procedure TfrmMain.ActionButtonClick(Sender: TObject);
var
  P: TWinControl;
  Idx: Integer;
begin
  if Sender is TButton then
  begin
    P := TButton(Sender).Parent;
    Idx := P.Tag;
    CnVirtualControl1.ItemAction(Idx);
  end;
end;

procedure TfrmMain.LabelClick(Sender: TObject);
var
  P: TWinControl;
  Idx: Integer;
begin
  if Sender is TLabel then
  begin
    P := TLabel(Sender).Parent;
    Idx := P.Tag;
    CnVirtualControl1.SelectedIndex := Idx;
  end;
end;

end.
