unit Unit1;

interface

{$I CnPack.inc}

uses
  {$IFDEF MSWINDOWS}Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, CnFloat, CnNative, FMX.StdCtrls, FMX.Edit, FMX.Types, System.Types, System.UITypes,
  FMX.Controls.Presentation;

type
  TFormFloat = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button1: TButton;
    rdoBin: TRadioButton;
    rdoOct: TRadioButton;
    rdoHex: TRadioButton;
    btnExtract: TButton;
    btnUInt64ToFloat: TButton;
    btnFloatToUInt64: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnUInt64ToFloatClick(Sender: TObject);
    procedure btnFloatToUInt64Click(Sender: TObject);
  private

  public

  end;

var
  FormFloat: TFormFloat;

implementation

{$R *.fmx}

procedure TFormFloat.Button1Click(Sender: TObject);
begin
// FPC、Windows 64/Linux 64 以及 Delphi 5、6 不支持以下三个函数
{$IFDEF WIN32}
{$IFDEF COMPILER7_UP}
  if rdoBin.IsChecked then
    ShowMessage(FloatDecimalToBinExtended(StrToFloat(Edit1.Text), CheckBox1.IsChecked,
      CheckBox2.IsChecked))
  else if rdoOct.IsChecked then
    ShowMessage(FloatDecimalToOctExtended(StrToFloat(Edit1.Text), CheckBox1.IsChecked,
      CheckBox2.IsChecked))
  else
    ShowMessage(FloatDecimalToHexExtended(StrToFloat(Edit1.Text), CheckBox1.IsChecked,
      CheckBox2.IsChecked));
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
{$IFDEF EXTENDED_SIZE_16}
  D1, D2: Extended;
{$ENDIF}
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

{$IFDEF EXTENDED_SIZE_16}
  D1 := -10;
  // Delphi 不支持 IEEE 的 16 字节完整浮点，虽然能拆装拼凑，但中间结果都是错的
  ExtractFloatQuadruple(D1, SignNeg3, Exponent3, Mantissa3, Mantissa2);
  CombineFloatQuadruple(SignNeg3, Exponent3, Mantissa3, Mantissa2, D2);
{$ENDIF}

  if (A1 = A2) and (B1 = B2) and (C1 = C2) {$IFDEF EXTENDED_SIZE_16} and (D1 = D2) {$ENDIF} then
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

end.
