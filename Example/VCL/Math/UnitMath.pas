unit UnitMath;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnMath, CnBigDecimal, StdCtrls, ComCtrls;

type
  TFormMath = class(TForm)
    edtValue: TEdit;
    btnInt64Sqrt: TButton;
    btnSqrt: TButton;
    btnLogN: TButton;
    btnLog2: TButton;
    btnLog10: TButton;
    edtResult: TEdit;
    btnFloatPi: TButton;
    btnGaussLegendrePi: TButton;
    lblRound: TLabel;
    edtGLRound: TEdit;
    udGL: TUpDown;
    mmoRes: TMemo;
    btnXGEuler: TButton;
    btnFloatToHex: TButton;
    edtFloat: TEdit;
    btnTestContFrac: TButton;
    procedure btnInt64SqrtClick(Sender: TObject);
    procedure btnSqrtClick(Sender: TObject);
    procedure btnLogNClick(Sender: TObject);
    procedure btnLog2Click(Sender: TObject);
    procedure btnLog10Click(Sender: TObject);
    procedure btnFloatPiClick(Sender: TObject);
    procedure btnGaussLegendrePiClick(Sender: TObject);
    procedure btnXGEulerClick(Sender: TObject);
    procedure btnFloatToHexClick(Sender: TObject);
    procedure btnTestContFracClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMath: TFormMath;

implementation

{$R *.DFM}

procedure TFormMath.btnInt64SqrtClick(Sender: TObject);
begin
  edtResult.Text := FloatToStr(Int64Sqrt(StrToInt64(edtValue.Text)));
end;

procedure TFormMath.btnSqrtClick(Sender: TObject);
begin
  edtResult.Text := FloatToStr(FloatSqrt(StrToFloat(edtValue.Text)));
end;

procedure TFormMath.btnLogNClick(Sender: TObject);
begin
  edtResult.Text := FloatToStr(FloatLogN(StrToFloat(edtValue.Text)));
end;

procedure TFormMath.btnLog2Click(Sender: TObject);
begin
  edtResult.Text := FloatToStr(FloatLog2(StrToFloat(edtValue.Text)));
end;

procedure TFormMath.btnLog10Click(Sender: TObject);
begin
  edtResult.Text := FloatToStr(FloatLog10(StrToFloat(edtValue.Text)));
end;

procedure TFormMath.btnFloatPiClick(Sender: TObject);
begin
  edtResult.Text := FloatGaussLegendrePi;
end;

procedure TFormMath.btnGaussLegendrePiClick(Sender: TObject);
begin
  mmoRes.Lines.Text := GaussLegendrePi(udGL.Position);
end;

procedure TFormMath.btnXGEulerClick(Sender: TObject);
begin
  mmoRes.Lines.Text := XavierGourdonEuler(1000);
end;

procedure TFormMath.btnFloatToHexClick(Sender: TObject);
var
  X: Extended;
  S: string;
begin
  X := StrToFloat(edtFloat.Text);
  S := FloatToHex(X);
  ShowMessage(S);

  X := HexToFloat(S);
  edtFloat.Text := FloatToStr(X);
end;

procedure TFormMath.btnTestContFracClick(Sender: TObject);
const
  TERM = 150;
var
  A, B: TInt64s;
  I: Integer;
  P: Extended;
begin
  // 创建数组，大小为 Terms + 1（加一是因为需要包含 A[0] 和 B[0]，虽然 A[0] 不使用）
  SetLength(A, TERM + 1);
  SetLength(B, TERM + 1);

  // 初始化数组
  // B[0] = 3（连分数的整数部分）
  B[0] := 3;

  // 填充 A 和 B 数组
  for I := 0 to TERM do
  begin
    if I = 0 then
    begin
      // A[0] 不使用，但需要分配空间
      A[0] := 0;
    end
    else
    begin
      // 分子：奇数的平方 (12, 32, 52, ...)
      A[I] := Int64(2*I - 1) * Int64(2*I - 1);
    end;

    if I = 0 then
      Continue; // B[0] 已经设置

    // 分母：从 B[1] 开始都是6
    B[I] := 6;
  end;

  // 使用连分数计算 π的近似值
  P := Int64ContinuedFraction(A, B);
  ShowMessage(FloatToStr(P));
end;

end.
