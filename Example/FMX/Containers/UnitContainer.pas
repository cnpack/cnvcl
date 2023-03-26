unit UnitContainer;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  CnContainers, FMX.Grid, FMX.StdCtrls, FMX.ExtCtrls, CnHashMap, FMX.Types, System.Types, System.UITypes,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormContainers = class(TForm)
    lblCount: TLabel;
    lblRingBuffer: TLabel;
    lblHashMapCount: TLabel;
    lblHashMapCapacity: TLabel;
    StringGrid: TStringGrid;
    btnPushFront: TButton;
    btnPushBack: TButton;
    btnPopFront: TButton;
    btnPopBack: TButton;
    sgMap: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    StringColumn9: TStringColumn;
    StringColumn10: TStringColumn;
    StringColumn11: TStringColumn;
    StringColumn12: TStringColumn;
    StringColumn13: TStringColumn;
    StringColumn14: TStringColumn;
    StringColumn15: TStringColumn;
    StringColumn16: TStringColumn;
    StringColumn17: TStringColumn;
    StringColumn18: TStringColumn;
    StringColumn19: TStringColumn;
    StringColumn20: TStringColumn;
    StringColumn21: TStringColumn;
    StringColumn22: TStringColumn;
    StringColumn23: TStringColumn;
    StringColumn24: TStringColumn;
    StringColumn25: TStringColumn;
    StringColumn26: TStringColumn;
    StringColumn27: TStringColumn;
    StringColumn28: TStringColumn;
    StringColumn29: TStringColumn;
    StringColumn30: TStringColumn;
    StringColumn31: TStringColumn;
    StringColumn32: TStringColumn;
    btnHashMapAdd: TButton;
    btnHashMapDel: TButton;
    btnHashMapClear: TButton;
    btnHashMapAddInt64: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPushFrontClick(Sender: TObject);
    procedure btnPushBackClick(Sender: TObject);
    procedure btnPopFrontClick(Sender: TObject);
    procedure btnPopBackClick(Sender: TObject);
    procedure btnHashMapAddClick(Sender: TObject);
    procedure btnHashMapDelClick(Sender: TObject);
    procedure btnHashMapClearClick(Sender: TObject);
    procedure btnHashMapAddInt64Click(Sender: TObject);
  private
    FRing: TCnObjectRingBuffer;
    FHashMap: TCnHashMap;
    function RandObj: TObject;
  public
    procedure DumpRing;
    procedure DumpMap;
  end;

var
  FormContainers: TFormContainers;

implementation

{$R *.fmx}

uses
  CnFmxUtils;

procedure TFormContainers.DumpRing;
var
  List: TList;
  F, B, I: Integer;
begin
  CnFmxSetStringGridColumnCount(StringGrid, FRing.Size, 24);
  // StringGrid.ColumnCount := FRing.Size;

  List := TList.Create;

  try
    FRing.Dump(List, F, B);
    for I := 0 to List.Count - 1 do
    begin
      if (F = B) and (I = F) then
        StringGrid.Cells[I, 0] := '*' + IntToStr(Integer(List[I])) + '*'
      else if I = F then
        StringGrid.Cells[I, 0] := '*' + IntToStr(Integer(List[I]))
      else if I = B then
        StringGrid.Cells[I, 0] := IntToStr(Integer(List[I])) + '*'
      else
        StringGrid.Cells[I, 0] := IntToStr(Integer(List[I]));
    end;
    lblCount.Text := Format('Count: %d.', [FRing.Count]);
  finally
    List.Free;
  end;
end;

procedure TFormContainers.FormCreate(Sender: TObject);
begin
  FRing := TCnObjectRingBuffer.Create(10, True, False);
  Randomize;
  DumpRing;

  FHashMap := TCnHashMap.Create;
  DumpMap;
end;

procedure TFormContainers.FormDestroy(Sender: TObject);
begin
  FHashMap.Free;
  FRing.Free;
end;

procedure TFormContainers.btnPushFrontClick(Sender: TObject);
begin
  FRing.PushToFront(RandObj);
  DumpRing;
end;

function TFormContainers.RandObj: TObject;
begin
  Result := TObject(Trunc(Random * 1000));
end;

procedure TFormContainers.btnPushBackClick(Sender: TObject);
begin
  FRing.PushToBack(RandObj);
  DumpRing;
end;

procedure TFormContainers.btnPopBackClick(Sender: TObject);
begin
  ShowMessage(IntToStr(Integer(FRing.PopFromBack)));
  DumpRing;
end;

procedure TFormContainers.btnPopFrontClick(Sender: TObject);
begin
  ShowMessage(IntToStr(Integer(FRing.PopFromFront)));
  DumpRing;
end;

procedure TFormContainers.DumpMap;
var
  I, J, Idx, OldIdx, C, TC: Integer;
  It: ICnHashMapIterator;
  Node: TCnHashNode;
begin
  lblHashMapCapacity.Text := 'Capacity: ' + IntToStr(FHashMap.Capacity);
  CnFmxSetStringGridColumnCount(sgMap, FHashMap.Capacity, 24);
  // sgMap.ColumnCount := FHashMap.Capacity;

  sgMap.RowCount := 2;
  // sgMap.FixedSize.cy := 1;

  for I := 0 to sgMap.ColumnCount - 1 do
  begin
    sgMap.Cells[I, 0] := '0';
    for J := 1 to sgMap.RowCount - 1 do
      sgMap.Cells[I, J] := '';
  end;

  OldIdx := -1;
  C := 0;
  TC := 0;

  It := FHashMap.CreateIterator;
  while not It.Eof do
  begin
    Idx := It.CurrentIndex;
    if OldIdx <> Idx then
    begin
      // 新槽数量恢复为 1
      C := 1;
      OldIdx := Idx;
    end
    else
    begin
      Inc(C); // 老槽，数量加 1
    end;

    sgMap.Cells[Idx, 0] := IntToStr(C);
    if sgMap.RowCount < C + 1 then
      sgMap.RowCount := C + 1;

    Node := It.CurrentNode;
    Inc(TC);
    sgMap.Cells[Idx, C] := IntToStr(Integer(Node.Key64));

    // 当前槽的尾巴清空
    for I := C + 1 to sgMap.RowCount - 1 do
      sgMap.Cells[Idx, I] := '';

    It.Next;
  end;

  lblHashMapCount.Text := 'Size: ' + IntToStr(FHashMap.Size) + ' Iterator Count: ' + IntToStr(TC);

  It := nil;
end;

procedure TFormContainers.btnHashMapAddClick(Sender: TObject);
var
  M: Integer;
begin
  M := Random(1000);
  if M = 0 then
    M := 1;
  FHashMap.Add(M, 0);
  DumpMap;
end;

procedure TFormContainers.btnHashMapDelClick(Sender: TObject);
var
  M, C: Integer;
begin
  C := 0;
  repeat
    Inc(C);
    M := Random(1000);
    if M = 0 then
      M := 1;
  until FHashMap.HasKey(M) or (C > 20);

  if C > 20 then
    Exit;

  FHashMap.Remove(M);
  DumpMap;
end;

procedure TFormContainers.btnHashMapClearClick(Sender: TObject);
begin
  FHashMap.Clear;
  DumpMap;
end;

procedure TFormContainers.btnHashMapAddInt64Click(Sender: TObject);
var
  M: Int64;
begin
  Int64Rec(M).Lo := Random(1000);
  Int64Rec(M).Hi := Random(1000);
  if M = 0 then
    M := 1;
  FHashMap.Add(M, M + 1);
  DumpMap;
end;

end.

