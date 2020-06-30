unit UnitDecimal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnBigDecimal, StdCtrls, ComCtrls;

type
  TFormBigDecimal = class(TForm)
    pgc1: TPageControl;
    tsBigDecimal: TTabSheet;
    grpBigDecimal: TGroupBox;
    lblDecimal: TLabel;
    edtBigDecimal1: TEdit;
    btnSetAndGet: TButton;
    edtBigDecimal2: TEdit;
    btnRandCmp: TButton;
    btnBigDecimalAdd: TButton;
    btnBigDecimalSub: TButton;
    btnBigDecimalMul: TButton;
    btnBigDecimalDivide: TButton;
    edtBigDecimalResult: TEdit;
    btnSetFloat: TButton;
    edtFloat: TEdit;
    btnRoundToScale: TButton;
    edtRoundDigits: TEdit;
    mmoRound: TMemo;
    btnGetDigits: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSetAndGetClick(Sender: TObject);
    procedure btnRandCmpClick(Sender: TObject);
    procedure btnBigDecimalAddClick(Sender: TObject);
    procedure btnBigDecimalSubClick(Sender: TObject);
    procedure btnBigDecimalMulClick(Sender: TObject);
    procedure btnBigDecimalDivideClick(Sender: TObject);
    procedure btnSetFloatClick(Sender: TObject);
    procedure btnRoundToScaleClick(Sender: TObject);
    procedure btnGetDigitsClick(Sender: TObject);
  private
    FBD1: TCnBigDecimal;
    FBD2: TCnBigDecimal;
    FBD3: TCnBigDecimal;
  public
    { Public declarations }
  end;

var
  FormBigDecimal: TFormBigDecimal;

implementation

{$R *.DFM}

procedure TFormBigDecimal.FormCreate(Sender: TObject);
begin
  FBD1 := TCnBigDecimal.Create;
  FBD2 := TCnBigDecimal.Create;
  FBD3 := TCnBigDecimal.Create;
end;

procedure TFormBigDecimal.FormDestroy(Sender: TObject);
begin
  FBD1.Free;
  FBD2.Free;
  FBD3.Free;
end;

procedure TFormBigDecimal.btnSetAndGetClick(Sender: TObject);
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  edtBigDecimal2.Text := BigDecimalToString(FBD1);
end;

procedure TFormBigDecimal.btnRandCmpClick(Sender: TObject);
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  BigDecimalSetDec(edtBigDecimal2.Text, FBD2);
  ShowMessage(IntToStr(BigDecimalCompare(FBD1, FBD2)));
end;

procedure TFormBigDecimal.btnBigDecimalAddClick(Sender: TObject);
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  BigDecimalSetDec(edtBigDecimal2.Text, FBD2);
  if BigDecimalAdd(FBD3, FBD1, FBD2) then
    edtBigDecimalResult.Text := BigDecimalToString(FBD3);
end;

procedure TFormBigDecimal.btnBigDecimalSubClick(Sender: TObject);
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  BigDecimalSetDec(edtBigDecimal2.Text, FBD2);
  if BigDecimalSub(FBD3, FBD1, FBD2) then
    edtBigDecimalResult.Text := BigDecimalToString(FBD3);
end;

procedure TFormBigDecimal.btnBigDecimalMulClick(Sender: TObject);
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  BigDecimalSetDec(edtBigDecimal2.Text, FBD2);
  if BigDecimalMul(FBD3, FBD1, FBD2) then
    edtBigDecimalResult.Text := BigDecimalToString(FBD3);
end;

procedure TFormBigDecimal.btnBigDecimalDivideClick(Sender: TObject);
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  BigDecimalSetDec(edtBigDecimal2.Text, FBD2);
  if BigDecimalDiv(FBD3, FBD1, FBD2) then
    edtBigDecimalResult.Text := BigDecimalToString(FBD3);
end;

procedure TFormBigDecimal.btnSetFloatClick(Sender: TObject);
var
  S: Single;
  D: Double;
  E: Extended;
begin
  S := StrToFloat(edtFloat.Text);
  D := StrToFloat(edtFloat.Text);
  E := StrToFloat(edtFloat.Text);
  if BigDecimalSetSingle(S, FBD1) then
    edtBigDecimal1.Text := BigDecimalToString(FBD1);
  if BigDecimalSetDouble(D, FBD2) then
    edtBigDecimal2.Text := BigDecimalToString(FBD2);
  if BigDecimalSetExtended(E, FBD3) then
    edtBigDecimalResult.Text := BigDecimalToString(FBD3);
end;

procedure TFormBigDecimal.btnRoundToScaleClick(Sender: TObject);
var
  Dig: Integer;
  Mode: TCnBigDecimalRoundMode;
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  Dig := StrToInt(edtRoundDigits.Text);
  mmoRound.Lines.Clear;
  for Mode := Low(TCnBigDecimalRoundMode) to High(TCnBigDecimalRoundMode) do
  begin
    BigDecimalRoundToScale(FBD3, FBD1, Dig, Mode);
    mmoRound.Lines.Add(BigDecimalToString(FBD3));
  end;
end;

procedure TFormBigDecimal.btnGetDigitsClick(Sender: TObject);
var
  P, I, D: Integer;
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  P := BigDecimalGetPrecision(FBD1);
  BigDecimalGetIntDecimalCount(FBD1, I, D);
  ShowMessage(Format('Precision %d. Int Count %d. Decimal Count %d.', [P, I, D]));
end;

end.
