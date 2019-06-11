unit UnitMatrix;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, CnMatrix, Buttons, ComCtrls;

type
  TFormMatrix = class(TForm)
    StringGrid1: TStringGrid;
    StaticText1: TStaticText;
    StringGrid2: TStringGrid;
    btnEqual: TSpeedButton;
    StringGridR: TStringGrid;
    udRow1: TUpDown;
    udRow2: TUpDown;
    udCol1: TUpDown;
    udCol2: TUpDown;
    btnTranspose: TButton;
    btnTrace: TButton;
    btnSetE: TButton;
    btnSetZero: TButton;
    btnDeteminant: TButton;
    btnDump: TButton;
    btnMinor: TButton;
    btnAdjoint: TButton;
    btnInverse: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEqualClick(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure udRow1Click(Sender: TObject; Button: TUDBtnType);
    procedure udRow2Click(Sender: TObject; Button: TUDBtnType);
    procedure udCol1Click(Sender: TObject; Button: TUDBtnType);
    procedure udCol2Click(Sender: TObject; Button: TUDBtnType);
    procedure btnTransposeClick(Sender: TObject);
    procedure btnTraceClick(Sender: TObject);
    procedure btnSetEClick(Sender: TObject);
    procedure btnSetZeroClick(Sender: TObject);
    procedure btnDeteminantClick(Sender: TObject);
    procedure btnDumpClick(Sender: TObject);
    procedure btnMinorClick(Sender: TObject);
    procedure btnAdjointClick(Sender: TObject);
    procedure btnInverseClick(Sender: TObject);
  private
    FM1, FM2, FMR: TCnIntMatrix;
  public
    procedure RandMatrix(Matrix: TCnIntMatrix);
    procedure StringGridToMatrix(Grid: TStringGrid; Matrix: TCnIntMatrix);
    procedure MatrixToStringGrid(Matrix: TCnIntMatrix; Grid: TStringGrid);
  end;

var
  FormMatrix: TFormMatrix;

implementation

{$R *.DFM}

procedure TFormMatrix.FormCreate(Sender: TObject);
begin
  FM1 := TCnIntMatrix.Create(StringGrid1.RowCount, StringGrid1.ColCount);
  FM2 := TCnIntMatrix.Create(StringGrid2.RowCount, StringGrid2.ColCount);
  FMR := TCnIntMatrix.Create(1, 1);

  Randomize;
  RandMatrix(FM1);
  RandMatrix(FM2);
  MatrixToStringGrid(FM1, StringGrid1);
  MatrixToStringGrid(FM2, StringGrid2);

  udRow1.Position := StringGrid1.RowCount;
  udRow2.Position := StringGrid2.RowCount;
  udCol1.Position := StringGrid1.ColCount;
  udCol2.Position := StringGrid2.ColCount;
end;

procedure TFormMatrix.FormDestroy(Sender: TObject);
begin
  FM1.Free;
  FM2.Free;
  FMR.Free;
end;

procedure TFormMatrix.MatrixToStringGrid(Matrix: TCnIntMatrix;
  Grid: TStringGrid);
var
  I, J: Integer;
begin
  if (Matrix <> nil) and (Grid <> nil) then
  begin
    Grid.RowCount := Matrix.RowCount;
    Grid.ColCount := Matrix.ColCount;
    for I := 0 to Matrix.RowCount - 1 do
      for J := 0 to Matrix.ColCount - 1 do
        Grid.Cells[J, I] := IntToStr(Matrix.Value[I, J]);
  end;
end;

procedure TFormMatrix.RandMatrix(Matrix: TCnIntMatrix);
var
  I, J: Integer;
begin
  if Matrix <> nil then
    for I := 0 to Matrix.RowCount - 1 do
      for J := 0 to Matrix.ColCount - 1 do
        Matrix.Value[I, J] := Trunc(Random(256));
end;

procedure TFormMatrix.StringGridToMatrix(Grid: TStringGrid;
  Matrix: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix <> nil) and (Grid <> nil) then
  begin
    Matrix.RowCount := Grid.RowCount;
    Matrix.ColCount := Grid.ColCount;
    for I := 0 to Grid.RowCount - 1 do
      for J := 0 to Grid.ColCount - 1 do
        Matrix.Value[I, J] := StrToIntDef(Grid.Cells[J, I], 0);
  end;
end;

procedure TFormMatrix.btnEqualClick(Sender: TObject);
begin
  CnMatrixMul(FM1, FM2, FMR);
  MatrixToStringGrid(FMR, StringGridR);
end;

procedure TFormMatrix.StaticText1Click(Sender: TObject);
begin
  RandMatrix(FM1);
  RandMatrix(FM2);
  MatrixToStringGrid(FM1, StringGrid1);
  MatrixToStringGrid(FM2, StringGrid2);
end;

procedure TFormMatrix.udRow1Click(Sender: TObject; Button: TUDBtnType);
begin
  FM1.RowCount := udRow1.Position;
  StaticText1Click(StaticText1);
end;

procedure TFormMatrix.udRow2Click(Sender: TObject; Button: TUDBtnType);
begin
  FM2.RowCount := udRow2.Position;
  StaticText1Click(StaticText1);
end;

procedure TFormMatrix.udCol1Click(Sender: TObject; Button: TUDBtnType);
begin
  FM1.ColCount := udCol1.Position;
  StaticText1Click(StaticText1);
end;

procedure TFormMatrix.udCol2Click(Sender: TObject; Button: TUDBtnType);
begin
  FM2.ColCount := udCol2.Position;
  StaticText1Click(StaticText1);
end;

procedure TFormMatrix.btnTransposeClick(Sender: TObject);
begin
  CnMatrixTranspose(FM1, FMR);
  MatrixToStringGrid(FMR, StringGridR);
end;

procedure TFormMatrix.btnTraceClick(Sender: TObject);
begin
  ShowMessage(IntToStr(FM1.Trace));
end;

procedure TFormMatrix.btnSetEClick(Sender: TObject);
begin
  FM1.SetE(FM1.RowCount);
  MatrixToStringGrid(FM1, StringGrid1);
end;

procedure TFormMatrix.btnSetZeroClick(Sender: TObject);
begin
  FM1.SetZero;
  MatrixToStringGrid(FM1, StringGrid1);
end;

procedure TFormMatrix.btnDeteminantClick(Sender: TObject);
begin
  StringGridToMatrix(StringGrid1, FM1);
  ShowMessage(IntToStr(FM1.Determinant));
end;

procedure TFormMatrix.btnDumpClick(Sender: TObject);
var
  List: TStrings;
begin
  List := TStringList.Create;
  FM1.DumpToStrings(List);
  ShowMessage(List.Text);
  List.Free;
end;

procedure TFormMatrix.btnMinorClick(Sender: TObject);
var
  M: TCnIntMatrix;
begin
  M := TCnIntMatrix.Create(1, 1);
  CnMatrixMinor(FM1, StringGrid1.Row, StringGrid1.Col, M);
  MatrixToStringGrid(M, StringGridR);
  M.Free;
end;

procedure TFormMatrix.btnAdjointClick(Sender: TObject);
begin
  StringGridToMatrix(StringGrid1, FM1);
  CnMatrixAdjoint(FM1, FM2);
  MatrixToStringGrid(FM2, StringGrid2);
end;

procedure TFormMatrix.btnInverseClick(Sender: TObject);
begin
  StringGridToMatrix(StringGrid1, FM1);
  CnMatrixInverse(FM1, FM2);
  MatrixToStringGrid(FM2, StringGrid2);
end;

end.
