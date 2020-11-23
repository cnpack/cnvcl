unit UnitDFT;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CnComplex,
  StdCtrls, CnDFT;

type
  TFormDFT = class(TForm)
    btnDFTButterFly: TButton;
    edtButterFly: TEdit;
    btnTwiddleFactors: TButton;
    edtTwiddleFactors: TEdit;
    btnTestDFT: TButton;
    edtFFT: TEdit;
    edtIFFT: TEdit;
    procedure btnDFTButterFlyClick(Sender: TObject);
    procedure btnTwiddleFactorsClick(Sender: TObject);
    procedure btnTestDFTClick(Sender: TObject);
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

  ButterflyChange(@CA, 8);

  for I := Low(CA) to High(CA) do
    S := S + ComplexNumberToString(CA[I]) + ' ; ';
  edtButterFly.Text := S;
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

end.
