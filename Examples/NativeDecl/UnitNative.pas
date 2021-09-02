unit UnitNative;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnNativeDecl, ExtCtrls, Buttons;

type
  TFormNative = class(TForm)
    btnUInt64Div: TButton;
    btnUInt64Mod: TButton;
    mmoRes: TMemo;
    bvl1: TBevel;
    btnStrUInt64: TButton;
    btnMul32: TButton;
    btnHighLowBits: TButton;
    btnInt64MulMod: TButton;
    btnUInt64Add: TButton;
    btnUInt64Mul: TButton;
    btnGetGT2Power: TButton;
    btnGetGT2Power64: TButton;
    edtPower: TEdit;
    btnPower: TSpeedButton;
    edtExponent: TEdit;
    btnRoot: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btnUInt64DivClick(Sender: TObject);
    procedure btnUInt64ModClick(Sender: TObject);
    procedure btnStrUInt64Click(Sender: TObject);
    procedure btnMul32Click(Sender: TObject);
    procedure btnHighLowBitsClick(Sender: TObject);
    procedure btnInt64MulModClick(Sender: TObject);
    procedure btnUInt64AddClick(Sender: TObject);
    procedure btnUInt64MulClick(Sender: TObject);
    procedure btnGetGT2PowerClick(Sender: TObject);
    procedure btnGetGT2Power64Click(Sender: TObject);
    procedure btnPowerClick(Sender: TObject);
    procedure btnRootClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNative: TFormNative;

implementation

{$R *.DFM}

const
  A0: TUInt64 = 0;
  MIN_UINT64: TUInt64 = 0;
  MAX_UINT64: TUInt64 = $FFFFFFFFFFFFFFFF; // 18446744073709551615;
  MIN_INT64: TUInt64 = $8000000000000000;  // -9223372036854775808;
  MAX_INT64: TUInt64 = $7FFFFFFFFFFFFFFF; // 9223372036854775807;

var
  A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8: TUInt64;

procedure TFormNative.FormCreate(Sender: TObject);
begin
  // UInt64 的 div mod 各包括四类测试用例（从 Int64 的角度来看）：A+B+、A-B-、A+B-、A-B+

  A1 := TUInt64(4227372036857772807);
  A2 := TUInt64(-2227372036857772807); // 16219372036851778809
  A3 := TUInt64(97372037857779845);
  A4 := TUInt64(-97372037857779845);   // 18349372035851771771
  A5 := $22222222FFFFFFFF;
  A6 := $FFFFFFFF22222222;
  A7 := $FEFEFEFEFEFEFEFE;
  A8 := $FEFEFEFEFEFEFEFE;
  B1 := TUInt64(84560395435344);
  B2 := TUInt64(-684560395435342);     // 18446059513314116274
  B3 := TUInt64(-784560395435344);     // 18445959513314116272
  B4 := TUInt64(64560395435344);
  B5 := $1111111111111111;
  B6 := $1111111111111111;
  B7 := $0000000033333333;
  B8 := $3333333300000000;
end;

procedure TFormNative.btnUInt64DivClick(Sender: TObject);
begin
  mmoRes.Clear;
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Div(A0, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Div(A1, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' div ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(UInt64Div(A2, B2)));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' div ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(UInt64Div(A3, B3)));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' div ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(UInt64Div(A4, B4)));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' div ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(UInt64Div(A5, B5)));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' div ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(UInt64Div(A6, B6)));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' div ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(UInt64Div(A7, B7)));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' div ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(UInt64Div(A8, B8)));
{$IFDEF SUPPORT_UINT64}
  mmoRes.Lines.Add('');
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A0 div B1));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A1 div B1));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' div ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(A2 div B2));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' div ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(A3 div B3));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' div ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(A4 div B4));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' div ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(A5 div B5));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' div ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(A6 div B6));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' div ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(A7 div B7));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' div ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(A8 div B8));
{$ENDIF}
end;

procedure TFormNative.btnUInt64ModClick(Sender: TObject);
begin
  mmoRes.Clear;
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Mod(A0, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Mod(A1, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' mod ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(UInt64Mod(A2, B2)));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' mod ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(UInt64Mod(A3, B3)));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' mod ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(UInt64Mod(A4, B4)));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' mod ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(UInt64Mod(A5, B5)));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' mod ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(UInt64Mod(A6, B6)));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' mod ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(UInt64Mod(A7, B7)));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' mod ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(UInt64Mod(A8, B8)));
{$IFDEF SUPPORT_UINT64}
  mmoRes.Lines.Add('');
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A0 mod B1));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A1 mod B1));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' mod ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(A2 mod B2));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' mod ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(A3 mod B3));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' mod ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(A4 mod B4));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' mod ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(A5 mod B5));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' mod ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(A6 mod B6));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' mod ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(A7 mod B7));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' mod ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(A8 mod B8));
{$ENDIF}
end;

procedure TFormNative.btnStrUInt64Click(Sender: TObject);
var
  A: TUInt64;
  S: string;
begin
  S := '10977225559701242671';
  A := StrToUInt64(S);
  mmoRes.Lines.Clear;
  mmoRes.Lines.Add(S + ' Converted to TUInt64.');
  mmoRes.Lines.Add(UInt64ToStr(A));
end;

procedure TFormNative.btnMul32Click(Sender: TObject);
var
  A, B: Cardinal;
  C: Int64;
{$IFDEF SUPPORT_UINT64}
  D: UInt64;
{$ENDIF}
begin
  A := $FFFFFFFF;
  B := $FFFFFFFE;
  C := UInt64Mul(A, B);
  mmoRes.Clear;
  mmoRes.Lines.Add(UInt64ToStr(C));
{$IFDEF SUPPORT_UINT64}
  D := UInt64(A) * B;
  mmoRes.Lines.Add(UInt64ToStr(D));
{$ENDIF}

end;

procedure TFormNative.btnHighLowBitsClick(Sender: TObject);
var
  H, L: Integer;
  T1: Cardinal;
  T2: TUInt64;               //    28
begin                        //    *                          1
  T1 := 349583946;           // 00010100110101100011101001001010
  T2 := 102849293434583456;  // 0000000101101101011001001110001100110000100100010111100110100000
                             //        *                                                  5
                             //        56

//  T1 := $FFFFFFFF;
//  T2 := $FFFFFFFFFFFFFFFC;
  H := GetUInt32HighBits(T1);
  L := GetUInt32LowBits(T1);
  mmoRes.Lines.Add(Format('32: High %d Low %d.', [H, L]));

  H := GetUInt64HighBits(T2);
  L := GetUInt64LowBits(T2);
  mmoRes.Lines.Add(Format('64: High %d Low %d.', [H, L]));
end;

procedure TFormNative.btnInt64MulModClick(Sender: TObject);
var
  A, B: Int64;
begin
  A := 3567798656;
  B := 3352796231;
  B := Int64NonNegativeMulMod(A, B, 4294967291);
  // B := UInt64NonNegativeMulMod(A, B, 4294967291);
  mmoRes.Lines.Add(IntToStr(B)); // 要等于 1 才对
end;

procedure TFormNative.btnUInt64AddClick(Sender: TObject);
var
  A, B, L, H: TUInt64;
begin
  A := MAX_TUINT64;
  B := MAX_TUINT64;

  UInt64AddUInt64(A, B, L, H);
  ShowMessage(UInt64ToHex(L));
  ShowMessage(UInt64ToHex(H));
end;

procedure TFormNative.btnUInt64MulClick(Sender: TObject);
var
  A, B, L, H: TUInt64;
begin
  A := $FFFFFFFFFFFFFF3C;
  B := $FFFFFFFFFFFFFF2E;

  UInt64MulUInt64(A, B, L, H);
  ShowMessage(UInt64ToHex(L));
  ShowMessage(UInt64ToHex(H));
end;

procedure TFormNative.btnGetGT2PowerClick(Sender: TObject);
var
  N, R: Cardinal;
begin
  N := Trunc(Random(MaxInt) * 2);
  R := GetUInt32PowerOf2GreaterEqual(N);
  ShowMessage(UInt64ToStr(N) + ' < ' + UInt64ToStr(R));
end;

procedure TFormNative.btnGetGT2Power64Click(Sender: TObject);
var
  N, R: TUInt64;
begin
  N := Trunc(Random(MaxInt) * 2);
  N := UInt64Mul(N, N);
  R := GetUInt64PowerOf2GreaterEqual(N);
  ShowMessage(UInt64ToStr(N) + ' < ' + UInt64ToStr(R));
end;

procedure TFormNative.btnPowerClick(Sender: TObject);
begin
  mmoRes.Lines.Text := IntToStr(Int64NonNegativPower(StrToInt64(edtPower.Text), StrToInt64(edtExponent.Text)));
end;

procedure TFormNative.btnRootClick(Sender: TObject);
var
  M: Int64;
begin
  M := Int64NonNegativeRoot(StrToInt64(edtPower.Text), StrToInt(edtExponent.Text));
  mmoRes.Lines.Text := IntToStr(M) + ' ' + IntToStr(Int64NonNegativPower(M, StrToInt(edtExponent.Text)));

end;

end.

