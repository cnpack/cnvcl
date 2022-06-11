unit Unit128;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, CnInt128, CnNativeDecl;

type
  TForm128 = class(TForm)
    btnSample1: TButton;
    grpInt128: TGroupBox;
    lblHex128A: TLabel;
    lblHex128B: TLabel;
    bvl1: TBevel;
    btn128Add: TSpeedButton;
    btn128Sub: TSpeedButton;
    btn128Mul: TSpeedButton;
    btn128Div: TSpeedButton;
    btn128Shl: TSpeedButton;
    btn128Shr: TSpeedButton;
    bvl2: TBevel;
    lblHex128r: TLabel;
    edt128A: TEdit;
    edt128B: TEdit;
    edt128R: TEdit;
    grpUInt128: TGroupBox;
    lblHexU128A: TLabel;
    edtU128A: TEdit;
    btnU128Shl: TSpeedButton;
    btnU128Shr: TSpeedButton;
    btnU128Add: TSpeedButton;
    btnU128Mul: TSpeedButton;
    btnU128Div: TSpeedButton;
    btnU128Sub: TSpeedButton;
    edtU128B: TEdit;
    lblHexU128B: TLabel;
    bvl4: TBevel;
    bvl5: TBevel;
    edtU128R: TEdit;
    lblHexU128R: TLabel;
    btnSample2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSample1Click(Sender: TObject);
    procedure btn128ShlClick(Sender: TObject);
    procedure btn128ShrClick(Sender: TObject);
    procedure btnU128ShlClick(Sender: TObject);
    procedure btnU128ShrClick(Sender: TObject);
    procedure btnSample2Click(Sender: TObject);
  private
    FInt128A, FInt128B, FInt128R: TCnInt128;
    FUInt128A, FUInt128B, FUInt128R: TCnUInt128;
  public

  end;

var
  Form128: TForm128;

implementation

{$R *.DFM}

procedure TForm128.FormCreate(Sender: TObject);
begin
  FInt128A.Lo64 := 123;
  FInt128B.Hi64 := 456;

  FUInt128A.Lo64 := 7890;
  FUInt128B.Hi64 := $100;

  FInt128R.Lo64 := $F9;
  FInt128R.Hi64 := 1;

  FUInt128R.Lo64 := $3238F9;
  FUInt128R.Hi64 := 10000000;

  SetWindowLong(edt128A.Handle, GWL_STYLE, GetWindowLong(edt128A.Handle, GWL_STYLE) or ES_RIGHT);
  SetWindowLong(edt128B.Handle, GWL_STYLE, GetWindowLong(edt128B.Handle, GWL_STYLE) or ES_RIGHT);
  SetWindowLong(edt128R.Handle, GWL_STYLE, GetWindowLong(edt128R.Handle, GWL_STYLE) or ES_RIGHT);

  SetWindowLong(edtU128A.Handle, GWL_STYLE, GetWindowLong(edtU128A.Handle, GWL_STYLE) or ES_RIGHT);
  SetWindowLong(edtU128B.Handle, GWL_STYLE, GetWindowLong(edtU128B.Handle, GWL_STYLE) or ES_RIGHT);
  SetWindowLong(edtU128R.Handle, GWL_STYLE, GetWindowLong(edtU128R.Handle, GWL_STYLE) or ES_RIGHT);
end;

procedure TForm128.btnSample1Click(Sender: TObject);
var
  O: TCnUInt128;
begin
  // 有符号无进位加
  Int128Set(FInt128A, 1);
  Int128Set(FInt128B, 2);
  Int128Add(FInt128R, FInt128A, FInt128B);
  ShowMessage(Int128ToHex(FInt128R)); // 3

  // 无符号无进位加
  UInt128Set(FUInt128A, 1);
  UInt128Set(FUInt128B, 2);
  UInt128Add(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R)); // 3

  // 有符号无借位减
  Int128Set(FInt128A, 3);
  Int128Set(FInt128B, 2);
  Int128Sub(FInt128R, FInt128A, FInt128B);
  ShowMessage(Int128ToHex(FInt128R)); // 1

  // 无符号无借位减
  UInt128Set(FUInt128A, 3);
  UInt128Set(FUInt128B, 2);
  UInt128Sub(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R)); // 1

  // 有符号内部有进位加
  Int128Set(FInt128A, $A000000011111111);
  Int128Set(FInt128B, $D000000022222222);
  Int128Add(FInt128R, FInt128A, FInt128B);
  ShowMessage(Int128ToHex(FInt128R)); // 17000000033333333

  // 无符号内部有进位加
  UInt128Set(FUInt128A, $B000000033333333);
  UInt128Set(FUInt128B, $D000000055555555);
  UInt128Add(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R)); // 18000000088888888

  // 有符号内部有借位减
  Int128Set(FInt128A, $A000000000000000, 1);
  Int128Set(FInt128B, $B000000000000000);
  Int128Sub(FInt128R, FInt128A, FInt128B);
  ShowMessage(Int128ToHex(FInt128R)); // F000000000000000

  // 无符号内部有借位减
  UInt128Set(FUInt128A, $A000000000000000, 4);
  UInt128Set(FUInt128B, $C000000000000000);
  UInt128Sub(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R)); // 3E000000000000000

//  UInt64AddUInt64($FFFFFFFFFFFFFFFF, $FFFFFFFFFFFFFFFF, FUInt128R.Lo64, FUInt128R.Hi64);
//  ShowMessage(UInt64ToHex(FUInt128R.Hi64)); // 3E000000000000000

  // 无符号外部有借位减（整体不够减得负号
  UInt128Set(FUInt128A, $1000000000000000);
  UInt128Set(FUInt128B, $C000000000000000, 2);
  UInt128Sub(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R));

  // 无符号都不溢出乘
  UInt128Set(FUInt128A, 2);
  UInt128Set(FUInt128B, 3);
  UInt128Mul(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R)); // 6

  // 无符号内部单个溢出乘
  UInt128Set(FUInt128A, 2);
  UInt128Set(FUInt128B, $F000000011111111);
  UInt128Mul(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R)); // 1E000000022222222

  // 无符号内部俩溢出乘
  UInt128Set(FUInt128A, $F000000011111111);
  UInt128Set(FUInt128B, $F000000022222222);
  UInt128Mul(FUInt128R, FUInt128A, FUInt128B);
  ShowMessage(UInt128ToHex(FUInt128R)); // E10000002FFFFFFFD2468ACF0ECA8642

  // 无符号外部溢出乘
  UInt128Set(FUInt128A, $F000000011111111, 1);
  UInt128Set(FUInt128B, $F000000022222222, 1);
  try
    UInt128Mul(FUInt128R, FUInt128A, FUInt128B);
    ShowMessage(UInt128ToHex(FUInt128R)); // 抛出异常
  except
    ;
  end;

  // 无符号外部溢出乘
  UInt128Set(FUInt128A, $F000000011111111, 1);
  UInt128Set(FUInt128B, $F000000022222222, 1);
  UInt128Mul(FUInt128R, FUInt128A, FUInt128B, @O);
  ShowMessage(UInt128ToHex(O) + UInt128ToHex(FUInt128R)); // 03C100000063333332D2468ACF0ECA8642
end;

procedure TForm128.btn128ShlClick(Sender: TObject);
begin
  Int128ShiftLeft(FInt128R, 1);
  edt128R.Text := Int128ToHex(FInt128R);
end;

procedure TForm128.btn128ShrClick(Sender: TObject);
begin
  Int128ShiftRight(FInt128R, 1);
  edt128R.Text := Int128ToHex(FInt128R);
end;

procedure TForm128.btnU128ShlClick(Sender: TObject);
begin
  UInt128ShiftLeft(FUInt128R, 1);
  edtU128R.Text := UInt128ToHex(FUInt128R);
end;

procedure TForm128.btnU128ShrClick(Sender: TObject);
begin
  UInt128ShiftRight(FUInt128R, 1);
  edtU128R.Text := UInt128ToHex(FUInt128R);
end;

procedure TForm128.btnSample2Click(Sender: TObject);
begin
  // 正负号混合运算
  // 有符号正负加
  Int128Set(FInt128A, 3);
  Int128Set(FInt128B, -2);
  ShowMessage(Int128ToHex(FInt128B)); // -2
  Int128Add(FInt128R, FInt128A, FInt128B);
  ShowMessage(Int128ToHex(FInt128R)); // 1

  // 正直接加负 Int64 值
  Int128Set(FInt128A, 3);
  Int128Add(FInt128R, FInt128A, -2);
  ShowMessage(Int128ToHex(FInt128R)); // 1

  // 负负相加
  Int128Set(FInt128A, -33);
  Int128Set(FInt128B, -2);
  Int128Add(FInt128R, FInt128A, FInt128B);
  ShowMessage(Int128ToHex(FInt128R)); // -$23

  // 负直接加负 Int64 值
  Int128Set(FInt128A, -13);
  Int128Add(FInt128R, FInt128A, -2);
  ShowMessage(Int128ToHex(FInt128R)); // -$F

  // 负负相减
  Int128Set(FInt128A, -33);
  Int128Set(FInt128B, -20);
  Int128Sub(FInt128R, FInt128A, FInt128B);
  ShowMessage(Int128ToHex(FInt128R)); // -$D

  // 负直接加负 Int64 值
  Int128Set(FInt128A, -13);
  Int128Sub(FInt128R, FInt128A, 20);
  ShowMessage(Int128ToHex(FInt128R)); // -$21
end;

end.
