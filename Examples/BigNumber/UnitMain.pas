unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnBigNumber, Spin;

type
  TFormBigNumber = class(TForm)
    lblNumber1: TLabel;
    mmoNum1: TMemo;
    lblNum2: TLabel;
    mmoNum2: TMemo;
    btnGen1: TButton;
    btnGen2: TButton;
    mmoResult: TMemo;
    btnDup: TButton;
    btnSwap: TButton;
    btnCompare: TButton;
    btnInverseNeg1: TButton;
    btnInverseNeg2: TButton;
    cbbDigits: TComboBox;
    lblBytes: TLabel;
    btnUAdd: TButton;
    btnUsub: TButton;
    btnSignedAdd: TButton;
    btnSignedSub: TButton;
    btnShiftRightOne: TButton;
    btnShiftleftOne: TButton;
    lblShift: TLabel;
    seShift: TSpinEdit;
    btnShiftLeft: TButton;
    btnShiftRight: TButton;
    btnSqr: TButton;
    procedure btnGen1Click(Sender: TObject);
    procedure btnGen2Click(Sender: TObject);
    procedure btnDupClick(Sender: TObject);
    procedure btnSwapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnInverseNeg1Click(Sender: TObject);
    procedure btnInverseNeg2Click(Sender: TObject);
    procedure btnUAddClick(Sender: TObject);
    procedure btnUsubClick(Sender: TObject);
    procedure btnSignedAddClick(Sender: TObject);
    procedure btnSignedSubClick(Sender: TObject);
    procedure btnShiftleftOneClick(Sender: TObject);
    procedure btnShiftRightOneClick(Sender: TObject);
    procedure btnShiftRightClick(Sender: TObject);
    procedure btnShiftLeftClick(Sender: TObject);
    procedure btnSqrClick(Sender: TObject);
  private
    procedure CalcRandomLength;
    procedure ShowNumbers;
    procedure ShowResult(var Res: TCnBigNumber);
  public
    { Public declarations }
  end;

var
  FormBigNumber: TFormBigNumber;

implementation

{$R *.DFM}

const
  DEFAULT_RANDOM_LENGTH: Integer = 4096;

var
  Num1: TCnBigNumber;
  Num2: TCnBigNumber;
  RandomLength: Integer = 4096;

procedure TFormBigNumber.FormCreate(Sender: TObject);
begin
  BigNumberClear(Num1);
  BigNumberClear(Num2);
  ShowNumbers;
end;

procedure TFormBigNumber.btnGen1Click(Sender: TObject);
begin
  BigNumberClear(Num1);
  CalcRandomLength;
  if BigNumberRandBytes(Num1, RandomLength) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnGen2Click(Sender: TObject);
begin
  BigNumberClear(Num2);
  CalcRandomLength;
  if BigNumberRandBytes(Num2, RandomLength) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnDupClick(Sender: TObject);
begin
  if BigNumberCopy(Num2, Num1) <> nil then
    ShowNumbers;
end;

procedure TFormBigNumber.btnSwapClick(Sender: TObject);
begin
  BigNumberSwap(Num1, Num2);
  ShowNumbers
end;


procedure TFormBigNumber.btnCompareClick(Sender: TObject);
var
  Res: Integer;
begin
  Res := BigNumberCompare(Num1, Num2);
  if Res = 1 then
    ShowMessage('Num1 > Num2')
  else if Res = 0 then
    ShowMessage('Num1 = Num2')
  else if Res = -1 then
    ShowMessage('Num1 < Num2');
end;

procedure TFormBigNumber.btnInverseNeg1Click(Sender: TObject);
begin
  BigNumberSetNegative(Num1, not BigNumberIsNegative(Num1));
  ShowNumbers;
end;

procedure TFormBigNumber.ShowNumbers;
begin
  mmoNum1.Text := BigNumberToHex(Num1);
  mmoNum2.Text := BigNumberToHex(Num2);
end;

procedure TFormBigNumber.btnInverseNeg2Click(Sender: TObject);
begin
  BigNumberSetNegative(Num2, not BigNumberIsNegative(Num2));
  ShowNumbers;
end;

procedure TFormBigNumber.CalcRandomLength;
begin
  RandomLength := StrToIntDef(cbbDigits.Text, 1024);
end;

procedure TFormBigNumber.btnUAddClick(Sender: TObject);
var
  Res: PCnBigNumber;
begin
  Res := BigNumerNew;
  if BigNumberUnsignedAdd(Res^, Num1, Num2) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.ShowResult(var Res: TCnBigNumber);
begin
  mmoResult.Text := BigNumberToHex(Res);
end;

procedure TFormBigNumber.btnUsubClick(Sender: TObject);
var
  Res: PCnBigNumber;
begin
  Res := BigNumerNew;
  if BigNumberUnsignedSub(Res^, Num1, Num2) then
    ShowResult(Res^)
  else
    ShowMessage('Num1 < Num2');
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnSignedAddClick(Sender: TObject);
var
  Res: PCnBigNumber;
begin
  Res := BigNumerNew;
  if BigNumberAdd(Res^, Num1, Num2) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnSignedSubClick(Sender: TObject);
var
  Res: PCnBigNumber;
begin
  Res := BigNumerNew;
  if BigNumberSub(Res^, Num1, Num2) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftleftOneClick(Sender: TObject);
var
  Res: PCnBigNumber;
begin
  Res := BigNumerNew;
  if BigNumberShiftLeftOne(Res^, Num1) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftRightOneClick(Sender: TObject);
var
  Res: PCnBigNumber;
begin
  Res := BigNumerNew;
  if BigNumberShiftRightOne(Res^, Num1) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftRightClick(Sender: TObject);
var
  N: Integer;
  Res: PCnBigNumber;
begin
  N := seShift.Value;
  Res := BigNumerNew;
  if BigNumberShiftLeft(Res^, Num1, N) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftLeftClick(Sender: TObject);
var
  N: Integer;
  Res: PCnBigNumber;
begin
  N := seShift.Value;
  Res := BigNumerNew;
  if BigNumberShiftRight(Res^, Num1, N) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnSqrClick(Sender: TObject);
var
  Res: PCnBigNumber;
begin
  Res := BigNumerNew;
  if BigNumberSqr(Res^, Num1) then
    ShowResult(Res^);
  BigNumberFree(Res);
end;

end.
