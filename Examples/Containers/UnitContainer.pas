unit UnitContainer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnContainers, Grids, StdCtrls;

type
  TFormContainers = class(TForm)
    StringGrid: TStringGrid;
    btnPushFront: TButton;
    btnPushBack: TButton;
    btnPopFront: TButton;
    btnPopBack: TButton;
    lblCount: TLabel;
    lblRingBuffer: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPushFrontClick(Sender: TObject);
    procedure btnPushBackClick(Sender: TObject);
    procedure btnPopBackClick(Sender: TObject);
    procedure btnPopFrontClick(Sender: TObject);
  private
    FRing: TCnObjectRingBuffer;
    function RandObj: TObject;
  public
    procedure DumpRing;
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
end;

procedure TFormContainers.FormDestroy(Sender: TObject);
begin
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

end.

