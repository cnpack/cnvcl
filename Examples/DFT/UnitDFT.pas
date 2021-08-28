unit UnitDFT;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CnComplex,
  StdCtrls, CnDFT, CnNativeDecl;

type
  TFormDFT = class(TForm)
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
    procedure btnDFTButterFlyClick(Sender: TObject);
    procedure btnTwiddleFactorsClick(Sender: TObject);
    procedure btnTestDFTClick(Sender: TObject);
    procedure btnNTTButterFlyClick(Sender: TObject);
    procedure btnNttFactorsClick(Sender: TObject);
    procedure btnTestNttClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
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

end.
