unit UnitMatrix;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, CnMatrix, CnFEC, Buttons, ComCtrls;

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
    grpInt: TGroupBox;
    btnTranspose: TButton;
    btnTrace: TButton;
    btnSetE: TButton;
    btnSetZero: TButton;
    btnDeteminant: TButton;
    btnDump: TButton;
    btnMinor: TButton;
    btnAdjoint: TButton;
    grpRational: TGroupBox;
    btnRTranspose: TButton;
    btnRTrace: TButton;
    btnRSetE: TButton;
    btnRSetZero: TButton;
    btnRDump: TButton;
    btnRDeter: TButton;
    btnRMinor: TButton;
    btnRAdj: TButton;
    btnREqu: TSpeedButton;
    btnInverse: TButton;
    btnRational: TButton;
    btnRCalc2: TButton;
    btnEqualG: TSpeedButton;
    grpGalios: TGroupBox;
    btnGTranspose: TButton;
    btnGTrace: TButton;
    btnGSetE: TButton;
    btnGSetZero: TButton;
    btnGDeterminant: TButton;
    btnGDump: TButton;
    btnGMinor: TButton;
    btnGAdj: TButton;
    btnGInverse: TButton;
    btnDeleteRow: TButton;
    btnDeleteCol: TButton;
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
    procedure btnRTransposeClick(Sender: TObject);
    procedure btnRTraceClick(Sender: TObject);
    procedure btnRSetEClick(Sender: TObject);
    procedure btnRSetZeroClick(Sender: TObject);
    procedure btnRDumpClick(Sender: TObject);
    procedure btnRDeterClick(Sender: TObject);
    procedure btnRMinorClick(Sender: TObject);
    procedure btnRAdjClick(Sender: TObject);
    procedure btnREquClick(Sender: TObject);
    procedure btnRationalClick(Sender: TObject);
    procedure btnRCalc2Click(Sender: TObject);
    procedure btnEqualGClick(Sender: TObject);
    procedure btnGTransposeClick(Sender: TObject);
    procedure btnGTraceClick(Sender: TObject);
    procedure btnGSetEClick(Sender: TObject);
    procedure btnGSetZeroClick(Sender: TObject);
    procedure btnGDeterminantClick(Sender: TObject);
    procedure btnGDumpClick(Sender: TObject);
    procedure btnGMinorClick(Sender: TObject);
    procedure btnGAdjClick(Sender: TObject);
    procedure btnGInverseClick(Sender: TObject);
    procedure btnDeleteRowClick(Sender: TObject);
    procedure btnDeleteColClick(Sender: TObject);
  private
    FM1, FM2, FMR: TCnIntMatrix;
    FR1, FR2, FRR: TCnRationalMatrix;
    FG1, FG2, FGR: TCnGalois2Power8Matrix;
  public
    procedure RandMatrix(Matrix: TCnIntMatrix);
    procedure StringGridToMatrix(Grid: TStringGrid; Matrix: TCnIntMatrix); overload;
    procedure StringGridToMatrix(Grid: TStringGrid; Matrix: TCnRationalMatrix); overload;
    procedure MatrixToStringGrid(Matrix: TCnIntMatrix; Grid: TStringGrid); overload;
    procedure MatrixToStringGrid(Matrix: TCnRationalMatrix; Grid: TStringGrid); overload;
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

  FR1 := TCnRationalMatrix.Create(StringGrid1.RowCount, StringGrid1.ColCount);
  FR2 := TCnRationalMatrix.Create(StringGrid1.RowCount, StringGrid1.ColCount);
  FRR := TCnRationalMatrix.Create(1, 1);

  FG1 := TCnGalois2Power8Matrix.Create(StringGrid1.RowCount, StringGrid1.ColCount);
  FG2 := TCnGalois2Power8Matrix.Create(StringGrid1.RowCount, StringGrid1.ColCount);
  FGR := TCnGalois2Power8Matrix.Create(1, 1);

  Randomize;
  RandMatrix(FM1);
  RandMatrix(FM2);
  CnIntToRationalMatrix(FM1, FR1);
  CnIntToRationalMatrix(FM2, FR2);

  FG1.Assign(FM1);
  FG2.Assign(FM2);

  MatrixToStringGrid(FM1, StringGrid1);
  MatrixToStringGrid(FM2, StringGrid2);
  MatrixToStringGrid(FR1, StringGrid1);
  MatrixToStringGrid(FR2, StringGrid2);

  udRow1.Position := StringGrid1.RowCount;
  udRow2.Position := StringGrid2.RowCount;
  udCol1.Position := StringGrid1.ColCount;
  udCol2.Position := StringGrid2.ColCount;
end;

procedure TFormMatrix.FormDestroy(Sender: TObject);
begin
  FRR.Free;
  FR1.Free;
  FR2.Free;
  FM1.Free;
  FM2.Free;
  FMR.Free;
  FG1.Free;
  FG2.Free;
  FGR.Free;
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
  CnIntToRationalMatrix(FM1, FR1);
  CnIntToRationalMatrix(FM2, FR2);
  FG1.Assign(FM1);
  FG2.Assign(FM2);
  MatrixToStringGrid(FM1, StringGrid1);
  MatrixToStringGrid(FM2, StringGrid2);
end;

procedure TFormMatrix.udRow1Click(Sender: TObject; Button: TUDBtnType);
begin
  FM1.RowCount := udRow1.Position;
  FR1.RowCount := udRow1.Position;
  StaticText1Click(StaticText1);
end;

procedure TFormMatrix.udRow2Click(Sender: TObject; Button: TUDBtnType);
begin
  FM2.RowCount := udRow2.Position;
  FR2.RowCount := udRow1.Position;
  StaticText1Click(StaticText1);
end;

procedure TFormMatrix.udCol1Click(Sender: TObject; Button: TUDBtnType);
begin
  FM1.ColCount := udCol1.Position;
  FR1.ColCount := udCol1.Position;
  StaticText1Click(StaticText1);
end;

procedure TFormMatrix.udCol2Click(Sender: TObject; Button: TUDBtnType);
begin
  FM2.ColCount := udCol2.Position;
  FR2.ColCount := udCol2.Position;
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
  StringGridToMatrix(StringGrid1, FR1);
  CnMatrixInverse(FR1, FR2);
  MatrixToStringGrid(FR2, StringGrid2);
end;

procedure TFormMatrix.MatrixToStringGrid(Matrix: TCnRationalMatrix;
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
        Grid.Cells[J, I] := Matrix.Value[I, J].ToString;
  end;
end;

procedure TFormMatrix.StringGridToMatrix(Grid: TStringGrid;
  Matrix: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Matrix <> nil) and (Grid <> nil) then
  begin
    Matrix.RowCount := Grid.RowCount;
    Matrix.ColCount := Grid.ColCount;
    for I := 0 to Grid.RowCount - 1 do
      for J := 0 to Grid.ColCount - 1 do
        Matrix.Value[I, J].SetString(Grid.Cells[J, I]);
  end;
end;

procedure TFormMatrix.btnRTransposeClick(Sender: TObject);
begin
  CnMatrixTranspose(FR1, FRR);
  MatrixToStringGrid(FRR, StringGridR);
end;

procedure TFormMatrix.btnRTraceClick(Sender: TObject);
var
  T: TCnRationalNumber;
begin
  T := TCnRationalNumber.Create;
  FR1.Trace(T);
  ShowMessage(T.ToString);
  T.Free;
end;

procedure TFormMatrix.btnRSetEClick(Sender: TObject);
begin
  FR1.SetE(FR1.RowCount);
  MatrixToStringGrid(FR1, StringGrid1);
end;

procedure TFormMatrix.btnRSetZeroClick(Sender: TObject);
begin
  FR1.SetZero;
  MatrixToStringGrid(FR1, StringGrid1);
end;

procedure TFormMatrix.btnRDumpClick(Sender: TObject);
var
  List: TStrings;
begin
  List := TStringList.Create;
  FR1.DumpToStrings(List);
  ShowMessage(List.Text);
  List.Free;
end;

procedure TFormMatrix.btnRDeterClick(Sender: TObject);
var
  D: TCnRationalNumber;
begin
  StringGridToMatrix(StringGrid1, FR1);
  D := TCnRationalNumber.Create;
  FR1.Determinant(D);
  ShowMessage(D.ToString);
  D.Free;
end;

procedure TFormMatrix.btnRMinorClick(Sender: TObject);
var
  M: TCnRationalMatrix;
begin
  M := TCnRationalMatrix.Create(1, 1);
  CnMatrixMinor(FR1, StringGrid1.Row, StringGrid1.Col, M);
  MatrixToStringGrid(M, StringGridR);
  M.Free;
end;

procedure TFormMatrix.btnRAdjClick(Sender: TObject);
begin
  StringGridToMatrix(StringGrid1, FR1);
  CnMatrixAdjoint(FR1, FR2);
  MatrixToStringGrid(FR2, StringGrid2);
end;

procedure TFormMatrix.btnREquClick(Sender: TObject);
begin
  CnMatrixMul(FR1, FR2, FRR);
  MatrixToStringGrid(FRR, StringGridR);
end;

procedure TFormMatrix.btnRationalClick(Sender: TObject);
var
  X, Y, S: TCnRationalNumber;
begin
  X := TCnRationalNumber.Create;
  Y := TCnRationalNumber.Create;
  S := TCnRationalNumber.Create;

  S.SetZero;
  X.SetIntValue(92);
  Y.SetIntValue(2931698);
  X.Mul(Y);                   // X = 269716216
  S.Add(X);

  X.SetIntValue(251);
  Y.SetIntValue(-655370309);
  X.Mul(Y);                   // X = -164497947559
  S.Add(X);

  X.SetIntValue(16);
  Y.SetIntValue(83764351);
  X.Mul(Y);                   // X = 1340229616
  S.Add(X);

  X.SetIntValue(118);
  Y.SetIntValue(239587282);
  X.Mul(Y);                   // X = 28271299276
  S.Add(X);

  X.SetIntValue(82);
  Y.SetIntValue(125825447);
  X.Mul(Y);                   // X = 10317686654
  S.Add(X);

  ShowMessage(S.ToString); // Should be -124299015797
  X.Free;
  Y.Free;
  S.Free;
end;

procedure TFormMatrix.btnRCalc2Click(Sender: TObject);
var
  X, Y, S: TCnRationalNumber;
begin
  X := TCnRationalNumber.Create;
  Y := TCnRationalNumber.Create;
  S := TCnRationalNumber.Create;

  S.SetZero;
  X.SetIntValue(92);
  Y.SetValue(2931698, -124299015797);
  X.Mul(Y);                   // X = 269716216/-124299015797
  S.Add(X);

  X.SetIntValue(251);
  Y.SetValue(-655370309, -124299015797);
  X.Mul(Y);                   // X = -164497947559/-124299015797
  S.Add(X);

  X.SetIntValue(16);
  Y.SetValue(83764351, -124299015797);
  X.Mul(Y);                   // X = 1340229616/-124299015797
  S.Add(X);

  X.SetIntValue(118);
  Y.SetValue(239587282, -124299015797);
  X.Mul(Y);                   // X = 28271299276/-124299015797
  S.Add(X);

  X.SetIntValue(82);
  Y.SetValue(125825447, -124299015797);
  X.Mul(Y);                   // X = 10317686654/-124299015797
  S.Add(X);

  ShowMessage(S.ToString); // Should be 1
  X.Free;
  Y.Free;
  S.Free;
end;

procedure TFormMatrix.btnEqualGClick(Sender: TObject);
begin
  CnMatrixMul(FG1, FG2, FGR);
  MatrixToStringGrid(FGR, StringGridR);
end;

procedure TFormMatrix.btnGTransposeClick(Sender: TObject);
begin
  CnMatrixTranspose(FG1, FGR);
  MatrixToStringGrid(FGR, StringGridR);
end;

procedure TFormMatrix.btnGTraceClick(Sender: TObject);
begin
  ShowMessage(IntToStr(FG1.Trace));
end;

procedure TFormMatrix.btnGSetEClick(Sender: TObject);
begin
  FG1.SetE(FG1.RowCount);
  MatrixToStringGrid(FG1, StringGrid1);
end;

procedure TFormMatrix.btnGSetZeroClick(Sender: TObject);
begin
  FG1.SetZero;
  MatrixToStringGrid(FG1, StringGrid1);
end;

procedure TFormMatrix.btnGDeterminantClick(Sender: TObject);
begin
  StringGridToMatrix(StringGrid1, FG1);
  ShowMessage(IntToStr(FG1.Determinant));
end;

procedure TFormMatrix.btnGDumpClick(Sender: TObject);
var
  List: TStrings;
begin
  List := TStringList.Create;
  FM1.DumpToStrings(List);
  ShowMessage(List.Text);
  List.Free;
end;

procedure TFormMatrix.btnGMinorClick(Sender: TObject);
var
  M: TCnGalois2Power8Matrix;
begin
  M := TCnGalois2Power8Matrix.Create(1, 1);
  CnMatrixMinor(FG1, StringGrid1.Row, StringGrid1.Col, M);
  MatrixToStringGrid(M, StringGridR);
  M.Free;
end;

procedure TFormMatrix.btnGAdjClick(Sender: TObject);
begin
  StringGridToMatrix(StringGrid1, FG1);
  CnMatrixAdjoint(FG1, FG2);
  MatrixToStringGrid(FG2, StringGrid2);
end;

procedure TFormMatrix.btnGInverseClick(Sender: TObject);
begin
  StringGridToMatrix(StringGrid1, FG1);
  CnMatrixInverse(FG1, FG2);
  MatrixToStringGrid(FG2, StringGrid2);
end;

procedure TFormMatrix.btnDeleteRowClick(Sender: TObject);
begin
  FM1.DeleteRow(StringGrid1.Row);
  MatrixToStringGrid(FM1, StringGrid1);
end;

procedure TFormMatrix.btnDeleteColClick(Sender: TObject);
begin
  FM1.DeleteCol(StringGrid1.Col);
  MatrixToStringGrid(FM1, StringGrid1);
end;

end.
