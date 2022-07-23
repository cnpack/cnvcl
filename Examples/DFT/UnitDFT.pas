unit UnitDFT;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CnComplex,
  StdCtrls, CnDFT, CnNative, ComCtrls, CnMatrix;

type
  TFormDFT = class(TForm)
    pgc1: TPageControl;
    tsDFTNTT: TTabSheet;
    grpDFT: TGroupBox;
    btnDFTButterFly: TButton;
    edtDftButterFly: TEdit;
    btnTwiddleFactors: TButton;
    edtTwiddleFactors: TEdit;
    btnTestDFT: TButton;
    edtFFT: TEdit;
    edtIFFT: TEdit;
    grpNTT: TGroupBox;
    btnNTTButterFly: TButton;
    edtNttButterFly: TEdit;
    btnNttFactors: TButton;
    edtNttFactors: TEdit;
    btnTestNtt: TButton;
    edtNTT: TEdit;
    edtINTT: TEdit;
    tsDCT: TTabSheet;
    grpDCT: TGroupBox;
    btnDCT: TButton;
    btnIDCT: TButton;
    edtDCT: TEdit;
    edtIDCT: TEdit;
    grpDCT2: TGroupBox;
    btnDCT2: TButton;
    mmoDCT2: TMemo;
    procedure btnDFTButterFlyClick(Sender: TObject);
    procedure btnTwiddleFactorsClick(Sender: TObject);
    procedure btnTestDFTClick(Sender: TObject);
    procedure btnNTTButterFlyClick(Sender: TObject);
    procedure btnNttFactorsClick(Sender: TObject);
    procedure btnTestNttClick(Sender: TObject);
    procedure btnDCTClick(Sender: TObject);
    procedure btnIDCTClick(Sender: TObject);
    procedure btnDCT2Click(Sender: TObject);
  private

  public

  end;

var
  FormDFT: TFormDFT;

implementation

{$R *.DFM}

const
  Pi = 3.1415926535897932384626;

procedure TFormDFT.btnDFTButterFlyClick(Sender: TObject);
var
  I: Integer;
  S: string;
  CA: array[0..7] of TCnComplexNumber;
begin
  for I := Low(CA) to High(CA) do
    ComplexNumberSetValue(CA[I], I, 0);

  ButterflyChangeComplex(@CA, 8);

  for I := Low(CA) to High(CA) do
    S := S + ComplexNumberToString(CA[I]) + ' ; ';
  edtDftButterFly.Text := S;
end;

procedure InitTwiddleFactors(Data: PCnComplexArray; Len: Integer);
var
  I: Integer;
begin
  for I := 0 to Len - 1 do
  begin
    Data^[I].R := Cos(2.0 * Pi * I / Len);
    Data^[I].I := Sin(2.0 * Pi * I / Len);
  end;
end;

procedure TFormDFT.btnTwiddleFactorsClick(Sender: TObject);
const
  LEN = 8;
var
  I: Integer;
  S: string;
  CA: array[0..LEN - 1] of TCnComplexNumber;
begin
  InitTwiddleFactors(@CA, LEN);
  for I := Low(CA) to High(CA) do
    S := S + ComplexNumberToString(CA[I]) + ' ; ';
  edtTwiddleFactors.Text := S;
end;

procedure TFormDFT.btnTestDFTClick(Sender: TObject);
const
  LEN = 8;
var
  I: Integer;
  S: string;
  CA: array[0..LEN - 1] of TCnComplexNumber;
begin
  for I := Low(CA) to High(CA) do
  begin
    CA[I].R := I * 2 + 3;
    CA[I].I := 0;
  end;

  CnFFT(@CA, LEN);
  S := '';
  for I := Low(CA) to High(CA) do
    S := S + ComplexNumberToString(CA[I]) + ' ; ';
  edtFFT.Text := S;

  CnIFFT(@CA, LEN);
  S := '';
  for I := Low(CA) to High(CA) do
    S := S + ComplexNumberToString(CA[I]) + ' ; ';
  edtIFFT.Text := S;
end;

procedure TFormDFT.btnNTTButterFlyClick(Sender: TObject);
var
  I: Integer;
  S: string;
  IA: array[0..7] of Int64;
begin
  for I := Low(IA) to High(IA) do
    IA[I] := I;

  ButterflyChangeInt64(@IA, 8);

  for I := Low(IA) to High(IA) do
    S := S + IntToStr(IA[I]) + ' ; ';
  edtNttButterFly.Text := S;
end;

procedure InitNttFactors(Data: PInt64Array; Len: Integer);
const
  CN_NR = 1 shl 22;     // 2 的 23 次方的一半，最多只能处理次数为 CN_NR 的多项式
  CN_G = 3;             // 下面素数的原根是 3
  CN_G_INV = 332748118; // 该原根对该素数的逆元为 332748118
  CN_P = 998244353;     // 选取素数为 998244353 = 2^23*119 + 1
var
  I: Integer;
begin
  // 计算原根与其各次方
  for I := 0 to Len - 1 do
  begin
    // Data^[I] := Cos(2.0 * Pi * I / Len);
    // Data^[I] := Sin(2.0 * Pi * I / Len);
  end;
end;

procedure TFormDFT.btnNttFactorsClick(Sender: TObject);
const
  LEN = 8;
var
  I: Integer;
  S: string;
  IA: array[0..LEN - 1] of Int64;
begin
  InitNttFactors(@IA, LEN);

  for I := Low(IA) to High(IA) do
    S := S + IntToStr(IA[I]) + ' ; ';
  edtNttFactors.Text := S;
end;

procedure TFormDFT.btnTestNttClick(Sender: TObject);
const
  LEN = 8;
var
  I: Integer;
  S: string;
  IA: array[0..LEN - 1] of Int64;
begin
  for I := Low(IA) to High(IA) do
    IA[I] := I * 2 + 3;

  CnNTT(@IA, LEN);
  S := '';
  for I := Low(IA) to High(IA) do
    S := S + IntToStr(IA[I]) + ' ; ';
  edtNTT.Text := S;

  CnINTT(@IA, LEN);
  S := '';
  for I := Low(IA) to High(IA) do
    S := S + IntToStr(IA[I]) + ' ; ';
  edtINTT.Text := S;
end;

procedure TFormDFT.btnDCTClick(Sender: TObject);
var
  X, U: array[0..7] of Extended;
  I: Integer;
  S: string;
begin
  for I := Low(X) to High(X) do
    X[I] := 5 * I;

  CnDCT(@X[0], @U[0], 8);

  S := '';
  for I := Low(U) to High(U) do
  begin
    if I = High(U) then
      S := S + FloatToStr(U[I])
    else
      S := S + FloatToStr(U[I]) + ','
  end;

  edtDCT.Text := S;
  CnIDCT(@U[0], @X[0], 8);
end;

procedure TFormDFT.btnIDCTClick(Sender: TObject);
var
  X, U: array[0..7] of Extended;
  I: Integer;
  S: string;
begin
  for I := Low(X) to High(X) do
    X[I] := 5 * I;

  CnDCT(@X[0], @U[0], 8);
  CnIDCT(@U[0], @X[0], 8);

  S := '';
  for I := Low(X) to High(X) do
  begin
    if I = High(X) then
      S := S + IntToStr(Round(X[I]))
    else
      S := S + IntToStr(Round(X[I])) + ','
  end;

  edtIDCT.Text := S;
end;

procedure TFormDFT.btnDCT2Click(Sender: TObject);
var
  D, R: TCnFloatMatrix;
  L: TStrings;
begin
  D := TCnFloatMatrix.Create(4, 4);
  R := TCnFloatMatrix.Create(4, 4);

  D[0, 0] := 61; D[0, 1] := 19; D[0, 2] := 50; D[0, 3] := 20;
  D[1, 0] := 82; D[1, 1] := 26; D[1, 2] := 61; D[1, 3] := 45;
  D[2, 0] := 89; D[2, 1] := 90; D[2, 2] := 82; D[2, 3] := 43;
  D[3, 0] := 93; D[3, 1] := 59; D[3, 2] := 53; D[3, 3] := 97;

  L := TStringList.Create;
  D.DumpToStrings(L);
  mmoDCT2.Lines.AddStrings(L);
  mmoDCT2.Lines.Add('');

  CnDCT2(D, R);
  R.DumpToStrings(L);
  mmoDCT2.Lines.AddStrings(L);
  mmoDCT2.Lines.Add('');

  CnIDCT2(R, D);
  D.DumpToStrings(L);
  mmoDCT2.Lines.AddStrings(L);

  L.Free;
  R.Free;
  D.Free;
end;

end.
