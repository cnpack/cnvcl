unit UnitPolynomial;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnPolynomial, StdCtrls, ComCtrls, ExtCtrls;

type
  TFormPolynomial = class(TForm)
    pgcPoly: TPageControl;
    tsIntegerPolynomial: TTabSheet;
    grpIntegerPolynomial: TGroupBox;
    btnIPCreate: TButton;
    edtIP1: TEdit;
    bvl1: TBevel;
    mmoIP1: TMemo;
    mmoIP2: TMemo;
    btnIP1Random: TButton;
    btnIP2Random: TButton;
    lblDeg1: TLabel;
    lblDeg2: TLabel;
    edtIPDeg1: TEdit;
    edtIPDeg2: TEdit;
    btnIPAdd: TButton;
    btnIPSub: TButton;
    btnIPMul: TButton;
    btnIPDiv: TButton;
    lblIPEqual: TLabel;
    edtIP3: TEdit;
    btnTestExample1: TButton;
    btnTestExample2: TButton;
    bvl2: TBevel;
    btnTestExample3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnIPCreateClick(Sender: TObject);
    procedure btnIP1RandomClick(Sender: TObject);
    procedure btnIP2RandomClick(Sender: TObject);
    procedure btnIPAddClick(Sender: TObject);
    procedure btnIPSubClick(Sender: TObject);
    procedure btnIPMulClick(Sender: TObject);
    procedure btnIPDivClick(Sender: TObject);
    procedure btnTestExample1Click(Sender: TObject);
    procedure btnTestExample2Click(Sender: TObject);
    procedure btnTestExample3Click(Sender: TObject);
  private
    FIP1: TCnIntegerPolynomial;
    FIP2: TCnIntegerPolynomial;
    FIP3: TCnIntegerPolynomial;
  public
    { Public declarations }
  end;

var
  FormPolynomial: TFormPolynomial;

implementation

{$R *.DFM}

procedure TFormPolynomial.FormCreate(Sender: TObject);
begin
  FIP1 := TCnIntegerPolynomial.Create;
  FIP2 := TCnIntegerPolynomial.Create;
  FIP3 := TCnIntegerPolynomial.Create;
end;

procedure TFormPolynomial.FormDestroy(Sender: TObject);
begin
  FIP1.Free;
  FIP2.Free;
  FIP3.Free;
end;

procedure TFormPolynomial.btnIPCreateClick(Sender: TObject);
var
  IP: TCnIntegerPolynomial;
begin
  IP := TCnIntegerPolynomial.Create([23,4,-45,6,-78,23,34,1,0,-34,4]);
  edtIP1.Text := IP.ToString;
  IP.Free;
end;

procedure TFormPolynomial.btnIP1RandomClick(Sender: TObject);
var
  I, D: Integer;
begin
  D := StrToIntDef(edtIPDeg1.Text, 10);
  FIP1.Clear;
  Randomize;
  for I := 0 to D do
    FIP1.Add(Random(256) - 128);
  mmoIP1.Lines.Text := FIP1.ToString;
end;

procedure TFormPolynomial.btnIP2RandomClick(Sender: TObject);
var
  I, D: Integer;
begin
  D := StrToIntDef(edtIPDeg2.Text, 10);
  FIP2.Clear;
  Randomize;
  for I := 0 to D do
    FIP2.Add(Random(256) - 128);
  mmoIP2.Lines.Text := FIP2.ToString;
end;

procedure TFormPolynomial.btnIPAddClick(Sender: TObject);
begin
  if IntegerPolynomialAdd(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnIPSubClick(Sender: TObject);
begin
  if IntegerPolynomialSub(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnIPMulClick(Sender: TObject);
begin
  if IntegerPolynomialMul(FIP3, FIP1, FIP2) then
    edtIP3.Text := FIP3.ToString;
end;

procedure TFormPolynomial.btnIPDivClick(Sender: TObject);
var
  R: TCnIntegerPolynomial;
begin
  R := TCnIntegerPolynomial.Create;

  // 测试代码
//  FIP1.SetCoefficents([1, 2, 3]);
//  FIP2.SetCoefficents([2, 1]);
//  if IntegerPolynomialDiv(FIP3, R, FIP1, FIP2) then
//  begin
//    edtIP3.Text := FIP3.ToString;          // 3X - 4
//    ShowMessage('Remain: ' + R.ToString);  // 9
//  end;
  // 测试代码

  if FIP2[FIP2.MaxDegree] <> 1 then
  begin
    ShowMessage('Divisor MaxDegree only Support 1, change to 1');
    FIP2[FIP2.MaxDegree] := 1;
    mmoIP2.Lines.Text := FIP2.ToString;
  end;

  if IntegerPolynomialDiv(FIP3, R, FIP1, FIP2) then
  begin
    edtIP3.Text := FIP3.ToString;
    ShowMessage('Remain: ' + R.ToString);
  end;

  // 验算 FIP3 * FIP2 + R
  IntegerPolynomialMul(FIP3, FIP3, FIP2);
  IntegerPolynomialAdd(FIP3, FIP3, R);
  ShowMessage(FIP3.ToString);
  if mmoIP1.Lines.Text = FIP3.ToString then
    ShowMessage('Equal Verified OK.');
  R.Free;
end;

procedure TFormPolynomial.btnTestExample1Click(Sender: TObject);
var
  X, Y, P: TCnIntegerPolynomial;
begin
{
  用例一：
  构造一个有限域的二阶扩域 67*67，并指定其本原多项式是 u^2 + 1 = 0，
  然后在上面构造一条椭圆曲线 y^2 = x^3 + 4x + 3，选一个点 2u + 16, 30u + 39
  验证这个点在该椭圆曲线上。（注意 n 阶扩域上的椭圆曲线上的点的坐标是一对 n 次多项式）

  该俩用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 2.2.5

  具体实现就是计算(Y^2 - X^3 - A*X - B) mod Primtive，然后每个系数运算时都要 mod p
  这里 A = 4，B = 3。
  二阶扩域上，p 是素数 67，本原多项式是 u^2 + 1
}

  X := TCnIntegerPolynomial.Create([16, 2]);
  Y := TCnIntegerPolynomial.Create([39, 30]);
  P := TCnIntegerPolynomial.Create([1, 0, 1]);
  try
    IntegerPolynomialGaloisMul(Y, Y, Y, 67, P); // Y^2 得到 62X + 18

    IntegerPolynomialMulWord(X, 4);
    IntegerPolynomialSub(Y, Y, X);
    IntegerPolynomialSubWord(Y, 3);             // Y 减去了 A*X - B，得到 54X + 18
    IntegerPolynomialNonNegativeModWord(Y, 67);

    X.SetCoefficents([16, 2]);
    IntegerPolynomialGaloisPower(X, X, 3, 67, P);  // 得到 54X + 18


    IntegerPolynomialSub(Y, Y, X);
    IntegerPolynomialMod(Y, Y, P);    // 算出 0
    ShowMessage(Y.ToString);
  finally
    P.Free;
    Y.Free;
    X.Free;
  end;
end;

procedure TFormPolynomial.btnTestExample2Click(Sender: TObject);
var
  X, Y, P: TCnIntegerPolynomial;
begin
{
  用例二：
  构造一个有限域的二阶扩域 7691*7691，并指定其本原多项式是 u^2 + 1 = 0，
  然后在上面构造一条椭圆曲线 y^2=x^3+1 mod 7691，选一个点 633u + 6145, 7372u + 109
  验证这个点在该椭圆曲线上。

  该俩用例来源于 Craig Costello 的《Pairings for beginners》中的 Example 4.0.1

  具体实现就是计算(Y^2 - X^3 - A*X - B) mod Primtive，然后每个系数运算时都要 mod p
  这里 A = 0，B = 1
  二阶扩域上，p 是素数 67，本原多项式是 u^2 + 1
}

  X := TCnIntegerPolynomial.Create([6145, 633]);
  Y := TCnIntegerPolynomial.Create([109, 7372]);
  P := TCnIntegerPolynomial.Create([1, 0, 1]);
  try
    IntegerPolynomialGaloisMul(Y, Y, Y, 7691, P);

    IntegerPolynomialSubWord(Y, 1);
    IntegerPolynomialNonNegativeModWord(Y, 7691);

    X.SetCoefficents([6145, 633]);
    IntegerPolynomialGaloisPower(X, X, 3, 7691, P);

    IntegerPolynomialSub(Y, Y, X);
    IntegerPolynomialMod(Y, Y, P);    // 算出 0
    ShowMessage(Y.ToString);
  finally
    P.Free;
    Y.Free;
    X.Free;
  end;
end;

procedure TFormPolynomial.btnTestExample3Click(Sender: TObject);
var
  X, P: TCnIntegerPolynomial;
begin
{
  用例三：
  构造一个有限域的二阶扩域 67*67，并指定其本原多项式是 u^2 + 1 = 0，
  验证：(2u + 16)^67 = 65u + 16, (30u + 39)^67 = 37u + 39
}

  X := TCnIntegerPolynomial.Create([16, 2]);
  P := TCnIntegerPolynomial.Create([1, 0, 1]);
  try
    IntegerPolynomialGaloisPower(X, X, 67, 67, P);
    ShowMessage(X.ToString);

    X.SetCoefficents([39, 30]);
    IntegerPolynomialGaloisPower(X, X, 67, 67, P);
    ShowMessage(X.ToString);
  finally
    X.Free;
    P.Free;
  end;   
end;

end.
