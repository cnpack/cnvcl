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
    chkMulDivPrecision: TCheckBox;
    edtMulDivRoundDigits: TEdit;
    btnGetHighScale: TButton;
    tsBigBinary: TTabSheet;
    grpBigBinary: TGroupBox;
    lblBigBinary: TLabel;
    edtBigBinary1: TEdit;
    edtBigBinary2: TEdit;
    btnBigBinarySetGet: TButton;
    btnBigBinarySetFloat: TButton;
    btnBigBinaryCompare: TButton;
    edtBigBinaryFloat: TEdit;
    btnBigBinaryAdd: TButton;
    btnBigBinarySub: TButton;
    btnBigBinaryMul: TButton;
    btnBigBinaryDiv: TButton;
    chkBigBinaryPrecision: TCheckBox;
    edtBBMulDivRoundDigits: TEdit;
    edtBigBinaryResult: TEdit;
    btnBigBinaryToFloat: TButton;
    btnBigDecimalToFloat: TButton;
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
    procedure btnGetHighScaleClick(Sender: TObject);
    procedure btnBigBinarySetGetClick(Sender: TObject);
    procedure btnBigBinaryAddClick(Sender: TObject);
    procedure btnBigBinarySubClick(Sender: TObject);
    procedure btnBigBinaryMulClick(Sender: TObject);
    procedure btnBigBinaryDivClick(Sender: TObject);
    procedure btnBigBinaryCompareClick(Sender: TObject);
    procedure btnBigBinarySetFloatClick(Sender: TObject);
    procedure btnBigBinaryToFloatClick(Sender: TObject);
    procedure btnBigDecimalToFloatClick(Sender: TObject);
  private
    FBD1: TCnBigDecimal;
    FBD2: TCnBigDecimal;
    FBD3: TCnBigDecimal;
    FBB1: TCnBigBinary;
    FBB2: TCnBigBinary;
    FBB3: TCnBigBinary;
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

  FBB1 := TCnBigBinary.Create;
  FBB2 := TCnBigBinary.Create;
  FBB3 := TCnBigBinary.Create;
end;

procedure TFormBigDecimal.FormDestroy(Sender: TObject);
begin
  FBB1.Free;
  FBB2.Free;
  FBB3.Free;

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
var
  D: Integer;
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  BigDecimalSetDec(edtBigDecimal2.Text, FBD2);
  D := 0;
  if chkMulDivPrecision.Checked then
    D := StrToInt(edtMulDivRoundDigits.Text);
  if BigDecimalMul(FBD3, FBD1, FBD2, D) then
    edtBigDecimalResult.Text := BigDecimalToString(FBD3);
end;

procedure TFormBigDecimal.btnBigDecimalDivideClick(Sender: TObject);
var
  D: Integer;
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  BigDecimalSetDec(edtBigDecimal2.Text, FBD2);
  D := 0;
  if chkMulDivPrecision.Checked then
    D := StrToInt(edtMulDivRoundDigits.Text);
  if BigDecimalDiv(FBD3, FBD1, FBD2, D) then
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
  Mode: TCnBigRoundMode;
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  Dig := StrToInt(edtRoundDigits.Text);
  mmoRound.Lines.Clear;
  for Mode := Low(TCnBigRoundMode) to High(TCnBigRoundMode) do
  begin
    BigDecimalChangeToScale(FBD3, FBD1, Dig, Mode);
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

procedure TFormBigDecimal.btnGetHighScaleClick(Sender: TObject);
begin
  BigDecimalSetDec(edtBigDecimal1.Text, FBD1);
  ShowMessage(IntToStr(BigDecimalGetHighScale(FBD1)));
end;

procedure TFormBigDecimal.btnBigBinarySetGetClick(Sender: TObject);
begin
  BigBinarySetDec(edtBigBinary1.Text, FBB1);
  edtBigBinary2.Text := BigBinaryToString(FBB1);
end;

procedure TFormBigDecimal.btnBigBinaryAddClick(Sender: TObject);
begin
  BigBinarySetDec(edtBigBinary1.Text, FBB1);
  BigBinarySetDec(edtBigBinary2.Text, FBB2);
  if BigBinaryAdd(FBB3, FBB1, FBB2) then
    edtBigBinaryResult.Text := BigBinaryToString(FBB3);
end;

procedure TFormBigDecimal.btnBigBinarySubClick(Sender: TObject);
begin
  BigBinarySetDec(edtBigBinary1.Text, FBB1);
  BigBinarySetDec(edtBigBinary2.Text, FBB2);
  if BigBinarySub(FBB3, FBB1, FBB2) then
    edtBigBinaryResult.Text := BigBinaryToString(FBB3);
end;

procedure TFormBigDecimal.btnBigBinaryMulClick(Sender: TObject);
var
  D: Integer;
begin
  BigBinarySetDec(edtBigBinary1.Text, FBB1);
  BigBinarySetDec(edtBigBinary2.Text, FBB2);
  D := 0;
  if chkBigBinaryPrecision.Checked then
    D := StrToInt(edtBBMulDivRoundDigits.Text);
  if BigBinaryMul(FBB3, FBB1, FBB2, D) then
    edtBigBinaryResult.Text := BigBinaryToString(FBB3);
end;

procedure TFormBigDecimal.btnBigBinaryDivClick(Sender: TObject);
var
  D: Integer;
begin
  BigBinarySetDec(edtBigBinary1.Text, FBB1);
  BigBinarySetDec(edtBigBinary2.Text, FBB2);
  D := 0;
  if chkBigBinaryPrecision.Checked then
    D := StrToInt(edtBBMulDivRoundDigits.Text);
  if BigBinaryDiv(FBB3, FBB1, FBB2, D) then
    edtBigBinaryResult.Text := BigBinaryToString(FBB3);
end;

procedure TFormBigDecimal.btnBigBinaryCompareClick(Sender: TObject);
begin
  BigBinarySetDec(edtBigBinary1.Text, FBB1);
  BigBinarySetDec(edtBigBinary2.Text, FBB2);
  ShowMessage(IntToStr(BigBinaryCompare(FBB1, FBB2)));
end;

procedure TFormBigDecimal.btnBigBinarySetFloatClick(Sender: TObject);
var
  S: Single;
  D: Double;
  E: Extended;
begin
  S := StrToFloat(edtBigBinaryFloat.Text);
  D := StrToFloat(edtBigBinaryFloat.Text);
  E := StrToFloat(edtBigBinaryFloat.Text);
  if BigBinarySetSingle(S, FBB1) then
    edtBigBinary1.Text := BigBinaryToString(FBB1);
  if BigBinarySetDouble(D, FBB2) then
    edtBigBinary2.Text := BigBinaryToString(FBB2);
  if BigBinarySetExtended(E, FBB3) then
    edtBigBinaryResult.Text := BigBinaryToString(FBB3);
end;

procedure TFormBigDecimal.btnBigBinaryToFloatClick(Sender: TObject);
var
  S: Single;
  D: Double;
  E: Extended;
begin
  S := StrToFloat(edtBigBinaryFloat.Text);
  D := StrToFloat(edtBigBinaryFloat.Text);
  E := StrToFloat(edtBigBinaryFloat.Text);
  if BigBinarySetSingle(S, FBB1) then
    edtBigBinary1.Text := FloatToStr(BigBinaryToSingle(FBB1));
  if BigBinarySetDouble(D, FBB2) then
    edtBigBinary2.Text := FloatToStr(BigBinaryToDouble(FBB2));
  if BigBinarySetExtended(E, FBB3) then
    edtBigBinaryResult.Text := FloatToStr(BigBinaryToExtended(FBB3));
end;

procedure TFormBigDecimal.btnBigDecimalToFloatClick(Sender: TObject);
var
  S: Single;
  D: Double;
  E: Extended;
begin
  S := StrToFloat(edtFloat.Text);
  D := StrToFloat(edtFloat.Text);
  E := StrToFloat(edtFloat.Text);
  if BigDecimalSetSingle(S, FBD1) then
    edtBigDecimal1.Text := FloatToStr(BigDecimalToSingle(FBD1));
  if BigDecimalSetDouble(D, FBD2) then
    edtBigDecimal2.Text := FloatToStr(BigDecimalToDouble(FBD2));
  if BigDecimalSetExtended(E, FBD3) then
    edtBigDecimalResult.Text := FloatToStr(BigDecimalToExtended(FBD3));
end;

end.
