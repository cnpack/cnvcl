unit UnitHamming;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnDebug, StdCtrls, CnMatrix;

type
  TFormHamming = class(TForm)
    btnHamming: TButton;
    btnRSTest: TButton;
    procedure btnHammingClick(Sender: TObject);
    procedure btnRSTestClick(Sender: TObject);
  private
    procedure Vandermonde(V: TCnRationalMatrix; M, N: Integer);
  public
    { Public declarations }
  end;

var
  FormHamming: TFormHamming;

implementation

uses
  CnFEC;

{$R *.DFM}

procedure TFormHamming.btnHammingClick(Sender: TObject);
var
  InBits, OutBits: TBits;
  I: Integer;
begin
  InBits := TBits.Create;
  InBits.Size := 128;
  for I := 0 to InBits.Size - 1 do
    InBits.Bits[I] := I mod 2 = 0;
  CnDebugger.TraceBits(InBits);
  OutBits := TBits.Create;
  CnCalcHammingCode(InBits, OutBits, 8);
  CnDebugger.TraceBits(OutBits);
  OutBits.Bits[35] := not OutBits.Bits[35];
  CnVerifyHammingCode(OutBits, InBits, 8);

  CnDebugger.TraceBits(InBits);

  InBits.Free;
  OutBits.Free;
end;

procedure TFormHamming.btnRSTestClick(Sender: TObject);
const
  M = 8;  // 总数据
  N = 5;  // 原始数据
  ARR: array[0..N - 1] of Integer = (5, 6, 10, 34, 9);
var
  V, D, R, DI: TCnRationalMatrix;
  L: TStringList;
  I: Integer;
begin
  V := TCnRationalMatrix.Create(1, 1);
  Vandermonde(V, M, N);
  L := TStringList.Create;
  V.DumpToStrings(L);
  ShowMessage(L.Text);

  D := TCnRationalMatrix.Create(N, 1);
  R := TCnRationalMatrix.Create(1, 1);
  for I := 0 to N - 1 do
    D.Value[I, 0].SetIntValue(ARR[I]);

  CnMatrixMul(V, D, R);
  R.DumpToStrings(L);
  ShowMessage(L.Text);

  // R 是原始数据加校验数据，删去 3 个
  V.DeleteRow(5);
  V.DeleteRow(3);
  V.DeleteRow(1);
  V.DumpToStrings(L);
  ShowMessage(L.Text);

  R.DeleteRow(5);
  R.DeleteRow(3);
  R.DeleteRow(1);
  R.DumpToStrings(L);
  ShowMessage(L.Text);

  // 求逆矩阵并乘以结果
  DI := TCnRationalMatrix.Create(1, 1);
  CnMatrixInverse(V, DI);
  DI.DumpToStrings(L);
  ShowMessage(L.Text);

  CnMatrixMul(DI, R, V);
  V.DumpToStrings(L);
  ShowMessage(L.Text);  // 还原回 ARR 中的五个值

  DI.Free;
  D.Free;
  R.Free;
  L.Free;
  V.Free;
end;

procedure TFormHamming.Vandermonde(V: TCnRationalMatrix; M, N: Integer);
var
  I, J: Integer;
  Arr: array of Int64;
begin
  if (M < 0) or (N < 0) then
    Exit;

  if M < N then
    Exit;

  if V = nil then
    Exit;

  V.RowCount := M;
  V.ColCount := N;

  for I := 0 to N - 1 do
    for J := 0 to N - 1 do
      if I = J then
        V.Value[I, J].SetOne
      else
        V.Value[I, J].SetZero;

  for J := 0 to N - 1 do
    V.Value[N, J].SetOne;

  SetLength(Arr, N);
  for I := 0 to N - 1 do
    Arr[I] := I + 1;

  for I := N + 1 to M - 1 do
  begin
    for J := 0 to N - 1 do
    begin
      V.Value[I, J].SetIntValue(Arr[J]);
      Arr[J] := Arr[J] * Arr[J];
    end;
  end;
end;

end.
