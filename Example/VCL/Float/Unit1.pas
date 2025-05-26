unit Unit1;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnFloat, CnNative, StdCtrls, ExtCtrls;

type
  TFormFloat = class(TForm)
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button1: TButton;
    Label1: TLabel;
    rdoBin: TRadioButton;
    rdoOct: TRadioButton;
    rdoHex: TRadioButton;
    btnExtract: TButton;
    btnUInt64ToFloat: TButton;
    btnFloatToUInt64: TButton;
    bvl1: TBevel;
    edtFloat: TEdit;
    rgFloat: TRadioGroup;
    edtFloatHex: TEdit;
    edtFloatBack: TEdit;
    chkNeg: TCheckBox;
    lblExp: TLabel;
    edtExp: TEdit;
    lblManti: TLabel;
    edtManti: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnUInt64ToFloatClick(Sender: TObject);
    procedure btnFloatToUInt64Click(Sender: TObject);
    procedure edtFloatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormFloat: TFormFloat;

implementation

{$R *.dfm}

procedure TFormFloat.Button1Click(Sender: TObject);
begin
{$IFDEF WIN32}
{$IFDEF COMPILER7_UP}
  if rdoBin.Checked then
    ShowMessage(FloatDecimalToBinExtended(StrToFloat(Edit1.Text), CheckBox1.Checked,
      CheckBox2.Checked))
  else if rdoOct.Checked then
    ShowMessage(FloatDecimalToOctExtended(StrToFloat(Edit1.Text), CheckBox1.Checked,
      CheckBox2.Checked))
  else
    ShowMessage(FloatDecimalToHexExtended(StrToFloat(Edit1.Text), CheckBox1.Checked,
      CheckBox2.Checked));
{$ENDIF}
{$ENDIF}
end;

procedure TFormFloat.btnExtractClick(Sender: TObject);
var
  SignNeg1, SignNeg2, SignNeg3: Boolean;
  Exponent1, Exponent2, Exponent3: Integer;
  Mantissa1: Cardinal;
  Mantissa2: TUInt64;
  Mantissa3: TUInt64;
  A1, A2: Single;
  B1, B2: Double;
  C1, C2: Extended;
begin
  A1 := -0.33;            // C3 F5 A8 BE 是内存中的内容，处理时字节序要倒过来BEA8F5C3
  B1 := 1003.2354545;     // 91 09 F8 35 E2 59 8F 40，处理时同样倒过来
  C1 := -88843453452.091981100001; // 97 60 BC 60 20 DC 7B A5 23 C0

  ExtractFloatSingle(A1, SignNeg1, Exponent1, Mantissa1);
  ExtractFloatDouble(B1, SignNeg2, Exponent2, Mantissa2);
  ExtractFloatExtended(C1, SignNeg3, Exponent3, Mantissa3);

  CombineFloatSingle(SignNeg1, Exponent1, Mantissa1, A2);
  CombineFloatDouble(SignNeg2, Exponent2, Mantissa2, B2);
  CombineFloatExtended(SignNeg3, Exponent3, Mantissa3, C2);

  if (A1 = A2) and (B1 = B2) and (C1 = C2) then
    ShowMessage('Float Extract and Combine OK.');
end;

procedure TFormFloat.btnUInt64ToFloatClick(Sender: TObject);
var
  U: TUInt64;
  S: Single;
  D: Double;
  E: Extended;
begin
{$IFNDEF SUPPORT_UINT64}
  U := -234567869758674564;
  ShowMessage(UInt64ToStr(U));
{$ELSE}
  U := 4234567869758674564;
{$ENDIF}

  S := UInt64ToSingle(U);
  ShowMessage(FloatToStr(S));

  D := UInt64ToDouble(U);
  ShowMessage(FloatToStr(D));

  E := UInt64ToExtended(U);
  ShowMessage(FloatToStr(E));
end;

procedure TFormFloat.btnFloatToUInt64Click(Sender: TObject);
var
  A1: Single;
  B1: Double;
  C1, D1: Extended;
  U: TUInt64;
begin
  A1 := 0.33;
  B1 := 1003.2354545;
  C1 := 88843453452.091981100001;
  D1 := 1.5e19;

  U := SingleToUInt64(A1);
  ShowMessage(UInt64ToStr(U));
  U := DoubleToUInt64(B1);
  ShowMessage(UInt64ToStr(U));
  U := ExtendedToUInt64(C1);
  ShowMessage(UInt64ToStr(U));
  U := ExtendedToUInt64(D1);
  ShowMessage(UInt64ToStr(U));
end;

procedure TFormFloat.edtFloatChange(Sender: TObject);
var
  F1: Single;
  F2: Double;
  F3: Extended;
  Sign: Boolean;
  Exp: Integer;
  Mant: Cardinal;
  Mant64: TUInt64;

  function HexTrimZero(N: TUInt64): string;
  begin
    Result := UInt64ToHex(N);
    while (Length(Result) > 0) and (Result[1] = '0') do
      Delete(Result, 1, 1);
  end;

begin
  edtFloatHex.Text := '';
  if Trim(edtFloat.Text) = '' then
    Exit;

  case rgFloat.ItemIndex of
    0:
      begin
        F1 := StrToFloat(edtFloat.Text);
        edtFloatHex.Text := DataToHex(@F1, SizeOf(F1));
        edtFloatBack.Text := FloatToStr(F1);
        ExtractFloatSingle(F1, Sign, Exp, Mant);
        edtManti.Text := HexTrimZero(Mant);
      end;
    1:
      begin
        F2 := StrToFloat(edtFloat.Text);
        edtFloatHex.Text := DataToHex(@F2, SizeOf(F2));
        edtFloatBack.Text := FloatToStr(F2);
        ExtractFloatDouble(F2, Sign, Exp, Mant64);
        edtManti.Text := HexTrimZero(Mant64);
      end;
    2:
      begin
        F3 := StrToFloat(edtFloat.Text);
        edtFloatHex.Text := DataToHex(@F3, SizeOf(F3));
        edtFloatBack.Text := ExtendedToStr(F3);
        ExtractFloatExtended(F3, Sign, Exp, Mant64);
        edtManti.Text := HexTrimZero(Mant64);
      end;
  end;
  chkNeg.Checked := Sign;
  edtExp.Text := IntToStr(Exp);
end;

procedure TFormFloat.FormCreate(Sender: TObject);
begin
  edtFloat.OnChange(edtFloat);
end;

end.
