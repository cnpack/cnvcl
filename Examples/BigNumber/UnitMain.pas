unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnBigNumber, Spin, ExtCtrls;

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
    btnShiftRight: TButton;
    btnShiftLeft: TButton;
    btnSqr: TButton;
    btnMul: TButton;
    btnDiv: TButton;
    btnMod: TButton;
    btnExp: TButton;
    seExponent: TSpinEdit;
    pnlDisplay: TPanel;
    rbHex: TRadioButton;
    rbDec: TRadioButton;
    btnGcd: TButton;
    btnGenWord: TButton;
    edtWord: TEdit;
    btnWordAdd: TButton;
    btnSubWord: TButton;
    btnMulWord: TButton;
    btnDivWord: TButton;
    btnModWord: TButton;
    btnVerifyDiv: TButton;
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
    procedure btnShiftLeftClick(Sender: TObject);
    procedure btnShiftRightClick(Sender: TObject);
    procedure btnSqrClick(Sender: TObject);
    procedure btnMulClick(Sender: TObject);
    procedure btnDivClick(Sender: TObject);
    procedure btnModClick(Sender: TObject);
    procedure btnExpClick(Sender: TObject);
    procedure rbDecClick(Sender: TObject);
    procedure btnGcdClick(Sender: TObject);
    procedure btnGenWordClick(Sender: TObject);
    procedure btnWordAddClick(Sender: TObject);
    procedure btnSubWordClick(Sender: TObject);
    procedure btnMulWordClick(Sender: TObject);
    procedure btnDivWordClick(Sender: TObject);
    procedure btnModWordClick(Sender: TObject);
    procedure btnVerifyDivClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure CalcRandomLength;
    procedure ShowNumbers;
    procedure CheckNumber(const Num: TCnBigNumber);
    procedure CheckStringAndNumber(S: string; const Num: TCnBigNumber);
    procedure ShowResult(const Res: TCnBigNumber);
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
  Num1: TCnBigNumber = nil;
  Num2: TCnBigNumber = nil;
  AWord: DWORD;
  RandomLength: Integer = 4096;

procedure TFormBigNumber.FormCreate(Sender: TObject);
begin
  Num1 := BigNumberNew;
  Num2 := BigNumberNew;
  BigNumberClear(Num1);
  BigNumberClear(Num2);
  ShowNumbers;
end;

procedure TFormBigNumber.btnGen1Click(Sender: TObject);
begin
  BigNumberClear(Num1);
  CalcRandomLength;
  if BigNumberRandBytes(Num1, RandomLength) then
  begin
    ShowNumbers;
    CheckNumber(Num1);
    CheckStringAndNumber(mmoNum1.Lines.Text, Num1);
  end;
end;

procedure TFormBigNumber.btnGen2Click(Sender: TObject);
begin
  BigNumberClear(Num2);
  CalcRandomLength;
  if BigNumberRandBytes(Num2, RandomLength) then
  begin
    ShowNumbers;
    CheckNumber(Num2);
    CheckStringAndNumber(mmoNum2.Lines.Text, Num2);
  end;
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
  if rbHex.Checked then
  begin
    mmoNum1.Text := BigNumberToHex(Num1);
    mmoNum2.Text := BigNumberToHex(Num2);
    edtWord.Text := IntToHex(AWord, 8);
  end
  else
  begin
    mmoNum1.Text := BigNumberToDec(Num1);
    mmoNum2.Text := BigNumberToDec(Num2);
    edtWord.Text := IntToStr(AWord);
  end;
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
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberUnsignedAdd(Res, Num1, Num2) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.ShowResult(const Res: TCnBigNumber);
begin
  if rbHex.Checked then
    mmoResult.Text := BigNumberToHex(Res)
  else
    mmoResult.Text := BigNumberToDec(Res);
end;

procedure TFormBigNumber.btnUsubClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberUnsignedSub(Res, Num1, Num2) then
    ShowResult(Res)
  else
    ShowMessage('Num1 < Num2');
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnSignedAddClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberAdd(Res, Num1, Num2) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnSignedSubClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberSub(Res, Num1, Num2) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftleftOneClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberShiftLeftOne(Res, Num1) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftRightOneClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberShiftRightOne(Res, Num1) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftLeftClick(Sender: TObject);
var
  N: Integer;
  Res: TCnBigNumber;
begin
  N := seShift.Value;
  Res := BigNumberNew;
  if BigNumberShiftLeft(Res, Num1, N) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftRightClick(Sender: TObject);
var
  N: Integer;
  Res: TCnBigNumber;
begin
  N := seShift.Value;
  Res := BigNumberNew;
  if BigNumberShiftRight(Res, Num1, N) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnSqrClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberSqr(Res, Num1) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnMulClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberMul(Res, Num1, Num2) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnDivClick(Sender: TObject);
var
  Res, Rem: TCnBigNumber;
begin
  Res := BigNumberNew;
  Rem := BigNumberNew;
  if BigNumberDiv(Res, Rem, Num1, Num2) then
  begin
    ShowResult(Res);
    ShowMessage(BigNumberToHex(Rem));
  end;
  BigNumberFree(Rem);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnModClick(Sender: TObject);
var
  Rem: TCnBigNumber;
begin
  Rem := BigNumberNew;
  if BigNumberMod(Rem, Num1, Num2) then
    ShowResult(Rem);
  BigNumberFree(Rem);
end;

procedure TFormBigNumber.btnExpClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  BigNumberSetWord(Num2, seExponent.Value);
  if BigNumberExp(Res, Num1, Num2) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.rbDecClick(Sender: TObject);
begin
  ShowNumbers;
end;

procedure TFormBigNumber.btnGcdClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberGcd(Res, Num1, Num2) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnGenWordClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberRandBytes(Res, SizeOf(DWORD)) then
  begin
    AWord := Res.D^;
    ShowNumbers;
  end;
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnWordAddClick(Sender: TObject);
begin
  if BigNumberAddWord(Num1, AWord) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnSubWordClick(Sender: TObject);
begin
  if BigNumberSubWord(Num1, AWord) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnMulWordClick(Sender: TObject);
begin
  if BigNumberMulWord(Num1, AWord) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnDivWordClick(Sender: TObject);
var
  Rem: DWORD;
begin
  Rem := BigNumberDivWord(Num1, AWord);
  ShowNumbers;
  if rbHex.Checked then
    ShowMessage(IntToHex(Rem, 8))
  else
    ShowMessage(IntToStr(Rem));
end;

procedure TFormBigNumber.btnModWordClick(Sender: TObject);
var
  Rem: DWORD;
begin
  Rem := BigNumberModWord(Num1, AWord);
  ShowNumbers;  
  if rbHex.Checked then
    ShowMessage(IntToHex(Rem, 8))
  else
    ShowMessage(IntToStr(Rem));
end;

procedure TFormBigNumber.CheckNumber(const Num: TCnBigNumber);
var
  Bin: AnsiString;
  N: TCnBigNumber;
  Len: Integer;
begin
  SetLength(Bin, 65536); // 假设足够长，大于 Num 的 BytesCount
  Len := BigNumberToBinary(Num, @Bin[1]);
  N := BigNumberFromBinary(@Bin[1], Len);
  if BigNumberCompare(Num, N) <> 0 then
    ShowMessage('Error');
  BigNumberFree(N);
end;

procedure TFormBigNumber.btnVerifyDivClick(Sender: TObject);
var
  Rem: DWORD;
begin
  Rem := BigNumberDivWord(Num1, AWord);
  if BigNumberMulWord(Num1, AWord) then
    if BigNumberAddWord(Num1, Rem) then
      ShowNumbers;
end;

procedure TFormBigNumber.CheckStringAndNumber(S: string;
  const Num: TCnBigNumber);
var
  N: TCnBigNumber;
begin
  if rbHex.Checked then
  begin
    N := BigNumberFromHex(S);
    if BigNumberCompare(Num, N) <> 0 then
      ShowMessage('Error');
  end
  else
  begin
    N := BigNumberFromDec(S);
    if BigNumberCompare(Num, N) <> 0 then
      ShowMessage('Error');
  end;
  BigNumberFree(N);
end;

procedure TFormBigNumber.FormDestroy(Sender: TObject);
begin
  BigNumberFree(Num1);
  BigNumberFree(Num2);
end;

end.
