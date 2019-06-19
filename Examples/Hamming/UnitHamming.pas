unit UnitHamming;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnDebug, StdCtrls, CnMatrix;

type
  TFormHamming = class(TForm)
    btnHamming: TButton;
    btnRSTest: TButton;
    btnFiniteField2N: TButton;
    btnGenerate2Power8UsingX: TButton;
    btnCalc2Power8: TButton;
    procedure btnHammingClick(Sender: TObject);
    procedure btnRSTestClick(Sender: TObject);
    procedure btnFiniteField2NClick(Sender: TObject);
    procedure btnGenerate2Power8UsingXClick(Sender: TObject);
    procedure btnCalc2Power8Click(Sender: TObject);
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

procedure TFormHamming.btnFiniteField2NClick(Sender: TObject);
var
  I, J: Integer;
  XN, NX: array[0..255] of Integer;
begin
  XN[0] := 1;
  for I := 1 to 255 do
  begin
    J := (XN[I - 1] shl 1) xor XN[I - 1];  // 用生成元 x + 1 的幂来遍历并生成所有元素，要配合下面的本原多项式使用
    if (J and $100) <> 0 then
      J := J xor $11B;                     // 其对应本原多项式是 x8+x4+x3+x+1，也就是 1 0001 1011
    XN[I] := J;
  end;

  NX[0] := 0;
  NX[1] := 0;
  for I := 1 to 254 do
    NX[XN[I]] := I;

  ShowMessage(IntToStr(XN[0]) + IntToStr(NX[0]));
end;

procedure TFormHamming.btnGenerate2Power8UsingXClick(Sender: TObject);
var
  I, J: Integer;
  XN, NX: array[0..255] of Integer;
begin
  XN[0] := 1;
  for I := 1 to 254 do
  begin
    J := XN[I - 1] shl 1;  // 用生成元 x 的幂来遍历并生成所有元素，要配合下面的本原多项式使用
    if (J and $100) <> 0 then
      J := J xor $12D;     // 其对应本原多项式是 x8+x5+x3+x2+1，也就是1 0010 1101
    XN[I] := J;
  end;
  XN[255] := 0;

  NX[0] := 255;
  NX[1] := 0;
  for I := 1 to 254 do
    NX[XN[I]] := I;

{      1,   2,   4,   8,  16,  32,  64, 128,  45,  90, 180,  69, 138,  57, 114, 228,
     229, 231, 227, 235, 251, 219, 155,  27,  54, 108, 216, 157,  23,  46,  92, 184,
      93, 186,  89, 178,  73, 146,   9,  18,  36,  72, 144,  13,  26,  52, 104, 208,
     141,  55, 110, 220, 149,   7,  14,  28,  56, 112, 224, 237, 247, 195, 171, 123,
     246, 193, 175, 115, 230, 225, 239, 243, 203, 187,  91, 182,  65, 130,  41,  82,
     164, 101, 202, 185,  95, 190,  81, 162, 105, 210, 137,  63, 126, 252, 213, 135,
      35,  70, 140,  53, 106, 212, 133,  39,  78, 156,  21,  42,  84, 168, 125, 250,
     217, 159,  19,  38,  76, 152,  29,  58, 116, 232, 253, 215, 131,  43,  86, 172,
     117, 234, 249, 223, 147,  11,  22,  44,  88, 176,  77, 154,  25,  50, 100, 200,
     189,  87, 174, 113, 226, 233, 255, 211, 139,  59, 118, 236, 245, 199, 163, 107,
     214, 129,  47,  94, 188,  85, 170, 121, 242, 201, 191,  83, 166,  97, 194, 169,
     127, 254, 209, 143,  51, 102, 204, 181,  71, 142,  49,  98, 196, 165, 103, 206,
     177,  79, 158,  17,  34,  68, 136,  61, 122, 244, 197, 167,  99, 198, 161, 111,
     222, 145,  15,  30,  60, 120, 240, 205, 183,  67, 134,  33,  66, 132,  37,  74,
     148,   5,  10,  20,  40,  80, 160, 109, 218, 153,  31,  62, 124, 248, 221, 151,
       3,   6,  12,  24,  48,  96, 192, 173, 119, 238, 241, 207, 179,  75, 150,   0}
  ShowMessage(IntToStr(XN[0]) + IntToStr(NX[0]));
   { 255,   0,   1, 240,   2, 225, 241,  53,   3,  38, 226, 133, 242,  43,  54, 210,
       4, 195,  39, 114, 227, 106, 134,  28, 243, 140,  44,  23,  55, 118, 211, 234,
       5, 219, 196,  96,  40, 222, 115, 103, 228,  78, 107, 125, 135,   8,  29, 162,
     244, 186, 141, 180,  45,  99,  24,  49,  56,  13, 119, 153, 212, 199, 235,  91,
       6,  76, 220, 217, 197,  11,  97, 184,  41,  36, 223, 253, 116, 138, 104, 193,
     229,  86,  79, 171, 108, 165, 126, 145, 136,  34,   9,  74,  30,  32, 163,  84,
     245, 173, 187, 204, 142,  81, 181, 190,  46,  88, 100, 159,  25, 231,  50, 207,
      57, 147,  14,  67, 120, 128, 154, 248, 213, 167, 200,  63, 236, 110,  92, 176,
       7, 161,  77, 124, 221, 102, 218,  95, 198,  90,  12, 152,  98,  48, 185, 179,
      42, 209,  37, 132, 224,  52, 254, 239, 117, 233, 139,  22, 105,  27, 194, 113,
     230, 206,  87, 158,  80, 189, 172, 203, 109, 175, 166,  62, 127, 247, 146,  66,
     137, 192,  35, 252,  10, 183,  75, 216,  31,  83,  33,  73, 164, 144,  85, 170,
     246,  65, 174,  61, 188, 202, 205, 157, 143, 169,  82,  72, 182, 215, 191, 251,
      47, 178,  89, 151, 101,  94, 160, 123,  26, 112, 232,  21,  51, 238, 208, 131,
      58,  69, 148,  18,  15,  16,  68,  17, 121, 149, 129,  19, 155,  59, 249,  70,
     214, 250, 168,  71, 201, 156,  64,  60, 237, 130, 111,  20,  93, 122, 177, 150 }
end;

procedure TFormHamming.btnCalc2Power8Click(Sender: TObject);
begin
  ShowMessage(IntToStr(CnGalois2Power8Rule.Add(66, 67)));
  ShowMessage(IntToStr(CnGalois2Power8Rule.Subtract(66, 67)));
  ShowMessage(IntToStr(CnGalois2Power8Rule.Multiply(66, 67)));
  ShowMessage(IntToStr(CnGalois2Power8Rule.Divide(66, 67)));
end;

end.
