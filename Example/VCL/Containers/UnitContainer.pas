unit UnitContainer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnContainers, Grids, StdCtrls, ExtCtrls, CnHashMap;

type
  TFormContainers = class(TForm)
    StringGrid: TStringGrid;
    btnPushFront: TButton;
    btnPushBack: TButton;
    btnPopFront: TButton;
    btnPopBack: TButton;
    lblCount: TLabel;
    lblRingBuffer: TLabel;
    bvl1: TBevel;
    sgMap: TStringGrid;
    bvl2: TBevel;
    btnHashMapAdd: TButton;
    lblHashMapCount: TLabel;
    lblHashMapCapacity: TLabel;
    btnHashMapDel: TButton;
    btnHashMapClear: TButton;
    btnHashMapAddInt64: TButton;
    btnIntList: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPushFrontClick(Sender: TObject);
    procedure btnPushBackClick(Sender: TObject);
    procedure btnPopBackClick(Sender: TObject);
    procedure btnPopFrontClick(Sender: TObject);
    procedure btnHashMapAddClick(Sender: TObject);
    procedure btnHashMapDelClick(Sender: TObject);
    procedure btnHashMapClearClick(Sender: TObject);
    procedure btnHashMapAddInt64Click(Sender: TObject);
    procedure btnIntListClick(Sender: TObject);
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

{$R *.DFM}

procedure TFormContainers.DumpRing;
var
  List: TList;
  F, B, I: Integer;
begin
  if StringGrid.ColCount <> FRing.Size then
    StringGrid.ColCount := FRing.Size;

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
    lblCount.Caption := Format('Count: %d.', [FRing.Count]);
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
  lblHashMapCapacity.Caption := 'Capacity: ' + IntToStr(FHashMap.Capacity);
  if sgMap.ColCount <> FHashMap.Capacity then
    sgMap.ColCount := FHashMap.Capacity;

  sgMap.RowCount := 2;
  sgMap.FixedRows := 1;

  for I := 0 to sgMap.ColCount - 1 do
  begin
    sgMap.Cells[I, 0] := '0';
    for J := 1 to 100 do
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

  lblHashMapCount.Caption := 'Size: ' + IntToStr(FHashMap.Size) + ' Iterator Count: ' + IntToStr(TC);

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

procedure TFormContainers.btnIntListClick(Sender: TObject);
var
  EL: TCnExtendedList;
  L1: TCnIntegerList;
  L2: TCnUInt32List;
  L3: TCnInt64List;
  L4: TCnUInt64List;
begin
  EL := TCnExtendedList.Create;
  L1 := TCnIntegerList.Create;
  L2 := TCnUInt32List.Create;
  L3 := TCnInt64List.Create;
  L4 := TCnUInt64List.Create;

  EL.Add(0);
  EL.Add(-3.14);
  EL.Add(-3.14);
  EL.Add(0.00001);
  EL.Add(3.4e5);
  EL.Add(-10000);
  EL.FloatSort;
  ShowMessage(EL.ToString);

  L1.Add(0);
  L1.Add(14303434);
  L1.Add(-193984);
  L1.Add(100);
  L1.Add(-90099);
  L1.IntSort;
  ShowMessage(L1.ToString);

  L2.Add(0);
  L2.Add(14303434);
  L2.Add(193984);
  L2.Add(100);
  L2.Add(100);
  L2.Add(90099);
  L2.IntSort;
  ShowMessage(L2.ToString);

  L3.Add(0);
  L3.Add(14303434333);
  L3.Add(-1939843);
  L3.Add(10033);
  L3.Add(-90099333);
  L3.IntSort;
  ShowMessage(L3.ToString);

  L4.Add(0);
  L4.Add(143034344444);
  L4.Add(1939844444);
  L4.Add(100);
  L4.Add($FFFFFFFFFFFFFFFF);
  L4.Add(90099777777);
  L4.IntSort;
  ShowMessage(L4.ToString);

  L4.Free;
  L3.Free;
  L2.Free;
  L1.Free;
  EL.Free;
end;

end.

