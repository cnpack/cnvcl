unit TestDancingLinksUnit;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Grid, CnDancingLinks, FMX.StdCtrls, Contnrs, FMX.Edit, FMX.Types, System.Types, System.UITypes,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Controls.Presentation,
  FMX.EditBox, FMX.SpinBox;

type
  TDancingLinksForm = class(TForm)
    lblRow: TLabel;
    lblCol: TLabel;
    Grid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    btnCreate: TButton;
    stat1: TStatusBar;
    btnExpandRow: TButton;
    btnExpandCol: TButton;
    btnDump: TButton;
    spnbxRow: TSpinBox;
    spnbxCol: TSpinBox;
    lblStat: TLabel;
    lblStat2: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single);
    procedure GridSelectCell(Sender: TObject; const ACol: Integer; const ARow: Integer; var CanSelect: Boolean);
    procedure btnCreateClick(Sender: TObject);
    procedure btnExpandRowClick(Sender: TObject);
    procedure btnExpandColClick(Sender: TObject);
    procedure btnDumpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCross: TCnDancingLinks;
    FRowStack: TStack;
    FColStack: TStack;
    procedure UpdateMatrixToGrid;
    procedure UpdateStatusBar;
    procedure TravelNode(Sender: TObject);
    procedure ShowMyMessage(const Str: string);
  public
    { Public declarations }
  end;

var
  DancingLinksForm: TDancingLinksForm;

implementation

{$R *.fmx}

uses
  CnFmxUtils;

procedure TDancingLinksForm.btnCreateClick(Sender: TObject);
begin
  if FCross <> nil then
    FreeAndNil(FCross);
  FCross := TCnDancingLinks.Create(Trunc(spnbxCol.Value), Trunc(spnbxRow.Value));
  FCross.OnTravelNode := TravelNode;

  FRowStack := TStack.Create;
  FColStack := TStack.Create;

  UpdateMatrixToGrid;
end;

procedure TDancingLinksForm.UpdateMatrixToGrid;
var
  I, J: Integer;
begin
  CnFmxSetStringGridColumnCount(Grid, FCross.ColCount + 1);
  // Grid.ColumnCount := FCross.ColCount + 1;
  Grid.RowCount := FCross.RowCount + 1;

  for I := 0 to Grid.RowCount - 1 do
    for J := 0 to Grid.ColumnCount - 1 do
      Grid.Cells[J, I] := '';

  for I := 0 to Grid.RowCount - 1 do
    Grid.Cells[0, I] := IntToStr(I - 1);
  for I := 0 to Grid.ColumnCount - 1 do
    Grid.Cells[I, 0] := IntToStr(I - 1);
  Grid.Cells[0, 0] := '';

  FCross.TravelByRow;

  UpdateStatusBar;
end;

procedure TDancingLinksForm.FormCreate(Sender: TObject);
var
  S: TSize;
begin
  S.cx := 1;
  S.cy := 1;
  Grid.FixedSize := S;
end;

procedure TDancingLinksForm.FormDestroy(Sender: TObject);
var
  P: TObject;
begin
  while FRowStack.Count > 0 do
  begin
    P := TObject(FRowStack.Pop);
    P.Free;
  end;

  while FColStack.Count > 0 do
  begin
    P := TObject(FColStack.Pop);
    P.Free;
  end;

  FRowStack.Free;
  FColStack.Free;
  FCross.Free;
end;

procedure TDancingLinksForm.TravelNode(Sender: TObject);
var
  Col, Row: Integer;
  RowHead, ColHead: TCnCrossLinkedNode;
begin
  Col := (Sender as TCnCrossLinkedNode).Column;
  Row := (Sender as TCnCrossLinkedNode).Row;

  RowHead := FCross.RowHead[Row];
  ColHead := FCross.ColumnHead[Col];

  if (Sender = RowHead) and (Sender = ColHead) then
    Grid.Cells[Col + 1, Row + 1] := '<^ '
  else if Sender = RowHead then
    Grid.Cells[Col + 1, Row + 1] := '< '
  else if Sender = ColHead then
    Grid.Cells[Col + 1, Row + 1] := '^ '
  else
    Grid.Cells[Col + 1, Row + 1] := '';
  Grid.Cells[Col + 1, Row + 1] := Grid.Cells[Col + 1, Row + 1] + IntToStr(Col) + ':' + IntToStr(Row);
end;

procedure TDancingLinksForm.UpdateStatusBar;
var
  Node: TCnCrossLinkedNode;
begin
  if (Grid.Col <= 0) or (Grid.Row <= 0) then
    Exit;

  Node := FCross.Cells[Grid.Col - 1, Grid.Row - 1];
  if Node = nil then
    lblStat.Text := Format('No Node Exist at %d:%d', [Grid.Col - 1, Grid.Row - 1])
  else
  begin
    lblStat.Text := Format('Current %d:%d. Left %d:%d, Right %d:%d, Up %d:%d, Down %d:%d',
      [Node.Column, Node.Row, Node.Left.Column, Node.Left.Row, Node.Right.Column,
      Node.Right.Row, Node.Up.Column, Node.Up.Row, Node.Down.Column, Node.Down.Row])
  end;
  lblStat2.Text := Format('Nodes Count %d', [FCross.Count]);
end;

procedure TDancingLinksForm.GridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Node: TCnCrossLinkedNode;
  ACol, ARow: Integer;
begin
  if FCross = nil then
    Exit;

//  if Button = mbLeft then // 左键
//  begin
//    Grid.MouseToCell(X, Y, ACol, ARow);
//    if (ACol < Grid.FixedSize.cx) or (ARow < Grid.FixedSize.cy) then
//    begin
//      // 点击在 Fixed 区，删行列
//      if (ACol = 0) and (ARow = 0) then
//        Exit;
//
//      if ACol = 0 then
//      begin
//        // 删行
//        Node := FCross.ExtractRow(ARow - 1);
//        if Node <> nil then
//        begin
//          FRowStack.Push(Node);
//          ShowMyMessage('Row Extracted.');
//        end
//        else
//          ShowMyMessage('No Row to Extract');
//      end
//      else if ARow = 0 then
//      begin
//        // 删列
//        Node := FCross.ExtractColumn(ACol - 1);
//        if Node <> nil then
//        begin
//          FColStack.Push(Node);
//          ShowMyMessage('Column Extracted.');
//        end
//        else
//          ShowMyMessage('No Column to Extract');
//      end;
//      UpdateMatrixToGrid;
//      Exit;
//    end;
//
//    // 添加节点
//    Node := FCross.InsertNode(Grid.Col - 1, Grid.Row - 1);
//    if Node = nil then
//      ShowMyMessage('Insert Failed. Already Exists.')
//    else
//    begin
//      ShowMyMessage('Insert OK.');
//    end;
//    UpdateMatrixToGrid;
//  end
//  else if Button = mbRight then  // 删除
//  begin
//    Grid.MouseToCell(X, Y, ACol, ARow);
//    if (ACol < Grid.FixedSize.cx) or (ARow < Grid.FixedSize.cy) then
//    begin
//      // 点击在 Fixed 区，弹回行列
//      if (ACol = 0) and (ARow = 0) then
//        Exit;
//
//      if ACol = 0 then
//      begin
//        // 恢复行
//        if FRowStack.Count > 0 then
//          FCross.RestoreRow(TCnCrossLinkedNode(FRowStack.Pop))
//        else
//          ShowMyMessage('No Row to Restore.');
//      end
//      else if ARow = 0 then
//      begin
//        // 恢复列
//        if FColStack.Count > 0 then
//          FCross.RestoreColumn(TCnCrossLinkedNode(FColStack.Pop))
//        else
//          ShowMyMessage('No Column to Restore.');
//      end;
//      UpdateMatrixToGrid;
//      Exit;
//    end;
//
//    // 删除节点
//    Node := FCross.ExtractNode(Grid.Col - 1, Grid.Row - 1);
//    if Node = nil then
//      ShowMyMessage('Remove Node Failed. Not Exists.')
//    else
//    begin
//      ShowMyMessage(Format('Removed Node %d:%d', [Node.Column, Node.Row]));
//      UpdateMatrixToGrid;
//    end;
//  end;
end;

procedure TDancingLinksForm.GridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  ACol, ARow: Integer;
begin
//  if Button = mbRight then
//  begin
//    Grid.MouseToCell(X, Y, ACol, ARow);
//    if (ACol < Grid.FixedSize.cx) or (ARow < Grid.FixedSize.cy) then
//      Exit;
//    Grid.Col := ACol;
//    Grid.Row := ARow;
//  end;
end;

procedure TDancingLinksForm.ShowMyMessage(const Str: string);
begin
  lblStat2.Text := Str;
end;

procedure TDancingLinksForm.btnExpandRowClick(Sender: TObject);
begin
  if FCross <> nil then
    FCross.ExpandRow;
  UpdateMatrixToGrid;
end;

procedure TDancingLinksForm.btnExpandColClick(Sender: TObject);
begin
  if FCross <> nil then
    FCross.ExpandCol;
  UpdateMatrixToGrid;
end;

procedure TDancingLinksForm.GridSelectCell(Sender: TObject; const ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  UpdateStatusBar;
end;

procedure TDancingLinksForm.btnDumpClick(Sender: TObject);
var
  List: TStrings;
  I: Integer;
  P, Head: TCnCrossLinkedNode;
begin
  if FCross = nil then
    Exit;

  List := TStringList.Create;
  try
    for I := 0 to FCross.ColCount - 1 do
    begin
      Head := FCross.ColumnHead[I];
      if Head = nil then
        Continue;

      P := Head;
      repeat
        List.Add(Format('Col %d: Element %d:%d. Left %d:%d, Right %d:%d, Up %d:%d, Down %d:%d',
          [I, P.Column, P.Row, P.Left.Column, P.Left.Row, P.Right.Column,
          P.Right.Row, P.Up.Column, P.Up.Row, P.Down.Column, P.Down.Row]));
        P := P.Down;
      until P = Head;
    end;

    List.Add('');
    for I := 0 to FCross.RowCount - 1 do
    begin
      Head := FCross.RowHead[I];
      if Head = nil then
        Continue;

      P := Head;
      repeat
        List.Add(Format('Row %d: Element %d:%d. Left %d:%d, Right %d:%d, Up %d:%d, Down %d:%d',
          [I, P.Column, P.Row, P.Left.Column, P.Left.Row, P.Right.Column,
          P.Right.Row, P.Up.Column, P.Up.Row, P.Down.Column, P.Down.Row]));
        P := P.Right;
      until P = Head;
    end;

    ShowMessage(List.Text);
  finally
    List.Free;
  end;
end;

end.
