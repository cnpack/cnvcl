unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Controls, FMX.Dialogs, FMX.Edit, FMX.Forms, FMX.Graphics, FMX.ListBox, FMX.Memo, FMX.StdCtrls, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation, CnBigNumber;

type
  TFormBigNumber = class(TForm)
    lblNumber1: TLabel;
    lblNum2: TLabel;
    lblBytes: TLabel;
    lblShift: TLabel;
    mmoNum1: TMemo;
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
    btnUAdd: TButton;
    btnUsub: TButton;
    btnSignedAdd: TButton;
    btnSignedSub: TButton;
    btnShiftRightOne: TButton;
    btnShiftleftOne: TButton;
    btnShiftRight: TButton;
    btnShiftLeft: TButton;
    btnSqr: TButton;
    btnMul: TButton;
    btnDiv: TButton;
    btnMod: TButton;
    btnExp: TButton;
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
    btnMultipleMod: TButton;
    btnPowerMod: TButton;
    btnIsPrime: TButton;
    btnGenPrime: TButton;
    btnJudgeInt: TButton;
    btnRandRange: TButton;
    btnEnterNum1: TButton;
    btnEnterNum2: TButton;
    btnMInverse: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGen1Click(Sender: TObject);
    procedure btnGen2Click(Sender: TObject);
    procedure btnDupClick(Sender: TObject);
    procedure btnSwapClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnInverseNeg1Click(Sender: TObject);
    procedure btnInverseNeg2Click(Sender: TObject);
    procedure btnUAddClick(Sender: TObject);
    procedure btnUsubClick(Sender: TObject);
    procedure btnSignedAddClick(Sender: TObject);
    procedure btnSignedSubClick(Sender: TObject);
    procedure btnShiftRightOneClick(Sender: TObject);
    procedure btnShiftleftOneClick(Sender: TObject);
    procedure btnShiftRightClick(Sender: TObject);
    procedure btnShiftLeftClick(Sender: TObject);
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
    procedure btnMultipleModClick(Sender: TObject);
    procedure btnPowerModClick(Sender: TObject);
    procedure btnIsPrimeClick(Sender: TObject);
    procedure btnGenPrimeClick(Sender: TObject);
    procedure btnJudgeIntClick(Sender: TObject);
    procedure btnRandRangeClick(Sender: TObject);
    procedure btnEnterNum1Click(Sender: TObject);
    procedure btnEnterNum2Click(Sender: TObject);
    procedure btnMInverseClick(Sender: TObject);
  private
    procedure CalcRandomLength;
    procedure ShowNumbers;
    procedure CheckNumber(const Num: TCnBigNumber);
    procedure CheckStringAndNumber(S: string; const Num: TCnBigNumber);
    procedure ShowResult(const Res: TCnBigNumber);
  public

  end;

var
  FormBigNumber: TFormBigNumber;

implementation

{$R *.fmx}

const
  DEFAULT_RANDOM_LENGTH: Integer = 4096;

var
  Num1: TCnBigNumber = nil;
  Num2: TCnBigNumber = nil;
  Num3: TCnBigNumber = nil;
  AWord: DWORD;
  RandomLength: Integer = 4096;

procedure TFormBigNumber.FormCreate(Sender: TObject);
begin
  Num1 := BigNumberNew;
  Num2 := BigNumberNew;
  Num3 := BigNumberNew;
  BigNumberClear(Num1);
  BigNumberClear(Num2);
  BigNumberClear(Num3);
  ShowNumbers;
end;

procedure TFormBigNumber.FormDestroy(Sender: TObject);
begin
  BigNumberFree(Num1);
  BigNumberFree(Num2);
  BigNumberFree(Num3);
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

procedure TFormBigNumber.btnInverseNeg2Click(Sender: TObject);
begin
  BigNumberSetNegative(Num2, not BigNumberIsNegative(Num2));
  ShowNumbers;
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

procedure TFormBigNumber.btnShiftRightOneClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberShiftRightOne(Res, Num1) then
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

procedure TFormBigNumber.btnShiftRightClick(Sender: TObject);
var
  N: Integer;
  Res: TCnBigNumber;
begin
  N := 5;
  Res := BigNumberNew;
  if BigNumberShiftRight(Res, Num1, N) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnShiftLeftClick(Sender: TObject);
var
  N: Integer;
  Res: TCnBigNumber;
begin
  N := 5;
  Res := BigNumberNew;
  if BigNumberShiftLeft(Res, Num1, N) then
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
    if rbHex.IsChecked then
      ShowMessage(BigNumberToHex(Rem))
    else
      ShowMessage(BigNumberToDec(Rem));
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
  BigNumberSetWord(Num2, 5);
  if BigNumberExp(Res, Num1, Num2) then
    ShowResult(Res);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.rbDecClick(Sender: TObject);
begin
  ShowNumbers;
end;

procedure TFormBigNumber.ShowNumbers;
begin
  if rbHex.IsChecked then
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

procedure TFormBigNumber.ShowResult(const Res: TCnBigNumber);
begin
  if rbHex.IsChecked then
    mmoResult.Text := BigNumberToHex(Res)
  else
    mmoResult.Text := BigNumberToDec(Res);
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

procedure TFormBigNumber.CalcRandomLength;
begin
  RandomLength := StrToIntDef(cbbDigits.Items[cbbDigits.ItemIndex], 1024);
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

procedure TFormBigNumber.CheckStringAndNumber(S: string;
  const Num: TCnBigNumber);
var
  N: TCnBigNumber;
begin
  if rbHex.IsChecked then
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
  if rbHex.IsChecked then
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
  if rbHex.IsChecked then
    ShowMessage(IntToHex(Rem, 8))
  else
    ShowMessage(IntToStr(Rem));
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

procedure TFormBigNumber.btnMultipleModClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Num3.SetWord(AWord);
  Res := BigNumberNew;
  if BigNumberMulMod(Res, Num1, Num2, Num3) then
    ShowMessage('BigNumberMultipleMod ' + Res.ToDec);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnPowerModClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Num3.SetWord(AWord);
  Res := BigNumberNew;
  if BigNumberMontgomeryPowerMod(Res, Num1, Num3, Num2) then
    ShowMessage('BigNumberMontgomeryPowerMod ' + Res.ToDec);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnIsPrimeClick(Sender: TObject);
var
  S: string;
  B: TCnBigNumber;
begin
  if not InputQuery('Hint', 'Enter a Dec Number. If Cancel, will Use Num1.', S) then
  begin
    if BigNumberIsProbablyPrime(Num1) then
      ShowMessage('Num1 is Prime, maybe.')
    else
      ShowMessage('Num1 is NOT Prime.');
  end
  else
  begin
    B := TCnBigNumber.FromDec(S);
    ShowMessage(B.ToDec);
    if BigNumberIsProbablyPrime(B) then
      ShowMessage('Enter is Prime, maybe.')
    else
      ShowMessage('Enter is NOT Prime.');
    B.Free;
  end;
end;

procedure TFormBigNumber.btnGenPrimeClick(Sender: TObject);
begin
  BigNumberClear(Num1);
  CalcRandomLength;
  if BigNumberGeneratePrime(Num1, RandomLength, 60) then
  begin
    ShowNumbers;
    if BigNumberIsProbablyPrime(Num1) then
      ShowMessage('Generated is Prime Maybe.')
    else
      ShowMessage('Generated is NOT Prime.');

    CheckNumber(Num1);
    CheckStringAndNumber(mmoNum1.Lines.Text, Num1);
  end;
end;

procedure TFormBigNumber.btnJudgeIntClick(Sender: TObject);
var
  S: string;
  N: TCnBigNumber;
  B1, B2, B3, B4: Boolean;
begin
  if InputQuery('Hint', 'Enter a Dec Number.', S) then
  begin
    N := TCnBigNumber.FromDec(S);
    B1 := BigNumberIsInt32(N);
    B2 := BigNumberIsUInt32(N);
    B3 := BigNumberIsInt64(N);
    B4 := BigNumberIsUInt64(N);
    ShowMessage(Format('Int32 %s. UInt32 %s. Int64 %s. UInt64 %s.', [BoolToStr(B1, True),
      BoolToStr(B2, True), BoolToStr(B3, True), BoolToStr(B4, True)]));
    N.Free;
  end;
end;

procedure TFormBigNumber.btnRandRangeClick(Sender: TObject);
begin
  if BigNumberRandRange(Num2, Num1) then
  begin
    ShowNumbers;
    CheckNumber(Num1);
    CheckStringAndNumber(mmoNum1.Lines.Text, Num1);
  end;
end;

procedure TFormBigNumber.btnEnterNum1Click(Sender: TObject);
var
  S: string;
begin
  if rbHex.IsChecked then
  begin
    if InputQuery('Hint', 'Enter a Hex Number.', S) then
    begin
      Num1.SetHex(S);
      ShowNumbers;
      CheckNumber(Num1);
      CheckStringAndNumber(mmoNum1.Lines.Text, Num1);
    end;
  end
  else
  begin
    if InputQuery('Hint', 'Enter a Dec Number.', S) then
    begin
      Num1.SetDec(S);
      ShowNumbers;
      CheckNumber(Num1);
      CheckStringAndNumber(mmoNum1.Lines.Text, Num1);
    end;
  end;
end;

procedure TFormBigNumber.btnEnterNum2Click(Sender: TObject);
var
  S: string;
begin
  if rbHex.IsChecked then
  begin
    if InputQuery('Hint', 'Enter a Hex Number.', S) then
    begin
      Num2.SetHex(S);
      ShowNumbers;
      CheckNumber(Num2);
      CheckStringAndNumber(mmoNum2.Lines.Text, Num2);
    end;
  end
  else
  begin
    if InputQuery('Hint', 'Enter a Dec Number.', S) then
    begin
      Num2.SetDec(S);
      ShowNumbers;
      CheckNumber(Num2);
      CheckStringAndNumber(mmoNum2.Lines.Text, Num2);
    end;
  end;
end;

procedure TFormBigNumber.btnMInverseClick(Sender: TObject);
var
  Res, R: TCnBigNumber;
begin
  Res := BigNumberNew;
  // 如何保证这俩互质？
  BigNumberModularInverse(Res, Num1, Num2);
  ShowResult(Res);

  // 验证 Res * Num1 mod Num2 = 1
  R := BigNumberNew;
  BigNumberMulMod(R, Res, Num1, Num2);
  if R.IsOne then
    ShowMessage('Modular Inverse Check OK')
  else
    ShowMessage('Modular Inverse Check Error: ' + R.ToDec);
  BigNumberFree(R);
  BigNumberFree(Res);
end;

end.
