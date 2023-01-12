unit UnitMain;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnBigNumber, Spin, ExtCtrls, CnCommon, ComCtrls, CnNative;

type
  TFormBigNumber = class(TForm)
    pgc1: TPageControl;
    tsBigNumber: TTabSheet;
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
    btnMultipleMod: TButton;
    btnPowerMod: TButton;
    btnIsPrime: TButton;
    btnGenPrime: TButton;
    btnJudgeInt: TButton;
    btnRandRange: TButton;
    btnEnterNum1: TButton;
    btnEnterNum2: TButton;
    btnMInverse: TButton;
    btnSetUInt64: TButton;
    btnPowerModCompare: TButton;
    btnMulModCompare: TButton;
    btnCheckPrime: TButton;
    btnIntPower: TButton;
    seIntPower: TSpinEdit;
    lblIntPower: TLabel;
    btnGetTenCount: TButton;
    btnCheckPrime2: TButton;
    btnBNCRT: TButton;
    btnBNSqrt: TButton;
    btnBNNextPrime: TButton;
    btnBNMulKaratsuba: TButton;
    btnRoot: TButton;
    btnIsPerfectPower: TButton;
    btnComNum: TButton;
    tsSparseBigNumberList: TTabSheet;
    btnSBNLTest1: TButton;
    edtSBNL: TEdit;
    mmoSBNL: TMemo;
    chkSparseUseSubMerge: TCheckBox;
    edtSparseList2: TEdit;
    btnSparseMerge: TButton;
    mmoSBNL2: TMemo;
    mmoSBNL3: TMemo;
    btnBNAKS: TButton;
    btnFloatToBigNumber: TButton;
    btnBigNumberToFloat: TButton;
    btnBNEuler: TButton;
    btnMulDivFloat: TButton;
    btnNegModInv: TButton;
    btnMontReduct: TButton;
    btnMontReduct1: TButton;
    btnMontReductTime: TButton;
    btnSquareRootMod: TButton;
    btnBNRawDump: TButton;
    btnBNDebugDump: TButton;
    btnBNFieldModInv: TButton;
    btnBNNaf: TButton;
    btnBNHashMap: TButton;
    btnBSGS: TButton;
    btnBase64: TButton;
    btnBatch: TButton;
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
    procedure btnMultipleModClick(Sender: TObject);
    procedure btnPowerModClick(Sender: TObject);
    procedure btnIsPrimeClick(Sender: TObject);
    procedure btnGenPrimeClick(Sender: TObject);
    procedure btnJudgeIntClick(Sender: TObject);
    procedure btnRandRangeClick(Sender: TObject);
    procedure btnEnterNum1Click(Sender: TObject);
    procedure btnEnterNum2Click(Sender: TObject);
    procedure btnMInverseClick(Sender: TObject);
    procedure btnSetUInt64Click(Sender: TObject);
    procedure btnPowerModCompareClick(Sender: TObject);
    procedure btnMulModCompareClick(Sender: TObject);
    procedure btnCheckPrimeClick(Sender: TObject);
    procedure btnIntPowerClick(Sender: TObject);
    procedure btnGetTenCountClick(Sender: TObject);
    procedure btnCheckPrime2Click(Sender: TObject);
    procedure btnBNCRTClick(Sender: TObject);
    procedure btnBNSqrtClick(Sender: TObject);
    procedure btnBNNextPrimeClick(Sender: TObject);
    procedure btnBNMulKaratsubaClick(Sender: TObject);
    procedure btnRootClick(Sender: TObject);
    procedure btnIsPerfectPowerClick(Sender: TObject);
    procedure btnComNumClick(Sender: TObject);
    procedure btnSBNLTest1Click(Sender: TObject);
    procedure btnSparseMergeClick(Sender: TObject);
    procedure btnBNAKSClick(Sender: TObject);
    procedure btnFloatToBigNumberClick(Sender: TObject);
    procedure btnBigNumberToFloatClick(Sender: TObject);
    procedure btnBNEulerClick(Sender: TObject);
    procedure btnMulDivFloatClick(Sender: TObject);
    procedure btnNegModInvClick(Sender: TObject);
    procedure btnMontReductClick(Sender: TObject);
    procedure btnMontReduct1Click(Sender: TObject);
    procedure btnMontReductTimeClick(Sender: TObject);
    procedure btnSquareRootModClick(Sender: TObject);
    procedure btnBNDebugDumpClick(Sender: TObject);
    procedure btnBNRawDumpClick(Sender: TObject);
    procedure btnBNFieldModInvClick(Sender: TObject);
    procedure btnBNNafClick(Sender: TObject);
    procedure btnBNHashMapClick(Sender: TObject);
    procedure btnBSGSClick(Sender: TObject);
    procedure btnBase64Click(Sender: TObject);
    procedure btnBatchClick(Sender: TObject);
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

uses
  UnitBatchTest;

const
  DEFAULT_RANDOM_LENGTH: Integer = 4096;

  N1 = // 2048 Bit
    '10B4735367B504B96174DAB4CD751E243AB761A0660C6118254EF415B6A73CC1830A3BFE2D166B5E' +
    'C31E42EE9EBBE6C9AE004EF8455079936AE39E88759F43B558E37DEBE3AF9448F4DE60E4D764460B' +
    '2BB1B7094AECC3046E8DC75FC2A2E34169867B41E25C64635CE3BAB69832A903AF47BC5F4F53B440' +
    'D8D21CD107840A0CB390D247D3BE19B1545F0670076EB166F4F0E4D76A9A1F7433869AACC2DE2CC3' +
    'AC6FE0A7B3F214C11359E5B7BFA605798D53854F4A7B8701567D140B770D01AF3C4CC28FF1A8E811' +
    '89890162643F2096CC76FF4A01A631A1B8BB4DC5BE1E5917C07AA1FA5349B4CEB2E9686E714F4FB2' +
    '31E935447EFFB439D27A3D58E1F8B679';

  N2 = // 2048 Bit
    '15E8353D4D33A8E16470A519CDA654077C83BD5C2EEFBF2D464BE664A34C9CEF35831436DC69C9B1' +
    '68EE1234BD6362A4C60354187B3EA624A24235EE91F162303C5ECBE4E495AE04BB5B3E81E641E515' +
    'E412F8DEF708F846410E717A35378B16834D9E8126657E8E4FB4CCD1A2386409F78999ED92CFCBA5' +
    'BEFDECD87CB52153C7115D4CE8ED01A31329F39766CB02528E9ACFC3882402250DAE0432A9061B49' +
    '63B4244E682764D19FD402C2F12367F702DE5162B930A9A1B530CBC252E48996E56E465C1CC8EB49' +
    'B74B2B387174D80B2E6590A65B524ACB';

  A_PRIME = // 2048 Bit 的素数，注意是十进制
    '28252724380732286542591368370721704854150567604113175217077230843313544784816297' +
    '37154388727440821537525353375729392255766404959663812014610316796349121129776513' +
    '27272491611042479583686782869413881255975514947460569547999740088522909749703803' +
    '51287414612821746133951800507866851457121498244347974584470558739999517605903128' +
    '68088011489999576946848850884874889391392158103921544850473635333145979604220556' +
    '05401618479250679842675845375864948088035482940234773751862824386358717810005061' +
    '39092721796571316352307346681153270426171068325601771167674032696377131534880878' +
    '406140033967436856195325073675138237048294121429135749497';

var
  Num1: TCnBigNumber = nil;
  Num2: TCnBigNumber = nil;
  Num3: TCnBigNumber = nil;
  SpareList1: TCnSparseBigNumberList = nil;
  SpareList2: TCnSparseBigNumberList = nil;
  SpareList3: TCnSparseBigNumberList = nil;
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

  SpareList1 := TCnSparseBigNumberList.Create;
  SpareList2 := TCnSparseBigNumberList.Create;
  SpareList3 := TCnSparseBigNumberList.Create;
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
    if rbHex.Checked then
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
  AWord := StrToInt(edtWord.Text);
  if BigNumberAddWord(Num1, AWord) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnSubWordClick(Sender: TObject);
begin
  AWord := StrToInt(edtWord.Text);
  if BigNumberSubWord(Num1, AWord) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnMulWordClick(Sender: TObject);
begin
  AWord := StrToInt(edtWord.Text);
  if BigNumberMulWord(Num1, AWord) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnDivWordClick(Sender: TObject);
var
  Rem: DWORD;
begin
  AWord := StrToInt(edtWord.Text);
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
  AWord := StrToInt(edtWord.Text);
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
  AWord := StrToInt(edtWord.Text);
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
  SpareList1.Free;
  SpareList2.Free;
  SpareList3.Free;

  BigNumberFree(Num1);
  BigNumberFree(Num2);
  BigNumberFree(Num3);
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
  if BigNumberPowerMod(Res, Num1, Num3, Num2) then
    ShowMessage('BigNumberPowerMod ' + Res.ToDec);
  if BigNumberMontgomeryPowerMod(Res, Num1, Num3, Num2) then
    ShowMessage('BigNumberPowerMod ' + Res.ToDec);
//  BigNumberCopy(Num2, Res);
//  ShowNumbers;
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnIsPrimeClick(Sender: TObject);
var
  S: string;
  B: TCnBigNumber;
begin
  if not CnInputQuery('Hint', 'Enter a Dec Number. If Cancel, will Use Num1.', S) then
  begin
    if BigNumberIsProbablyPrime(Num1) then
      ShowMessage('Num1 is Prime, maybe.')
    else
      ShowMessage('Num1 is NOT Prime.');
  end
  else
  begin
    B := TCnBigNumber.FromDec(S);
    MessageBox(Handle, PChar(B.ToDec), 'To Check', MB_OK);
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
  if CnInputQuery('Hint', 'Enter a Dec Number.', S) then
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
  if rbHex.Checked then
  begin
    if CnInputQuery('Hint', 'Enter a Hex Number.', S) then
    begin
      Num1.SetHex(S);
      ShowNumbers;
      CheckNumber(Num1);
      CheckStringAndNumber(mmoNum1.Lines.Text, Num1);
    end;
  end
  else
  begin
    if CnInputQuery('Hint', 'Enter a Dec Number.', S) then
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
  if rbHex.Checked then
  begin
    if CnInputQuery('Hint', 'Enter a Hex Number.', S) then
    begin
      Num2.SetHex(S);
      ShowNumbers;
      CheckNumber(Num2);
      CheckStringAndNumber(mmoNum2.Lines.Text, Num2);
    end;
  end
  else
  begin
    if CnInputQuery('Hint', 'Enter a Dec Number.', S) then
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

procedure TFormBigNumber.btnSetUInt64Click(Sender: TObject);
var
  I: Int64;
{$IFDEF SUPPORT_UINT64}
  U: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  U := 12345678900987654321;
  Num1.SetUInt64(U);
  ShowMessage(Num1.ToDec);
  ShowMessage(Format('%u', [Num1.GetUInt64]));
{$ENDIF}
  I := -1234567890987654321;
  Num1.SetInt64(I);
  ShowMessage(Num1.ToDec);
  ShowMessage(IntToStr(Num1.GetInt64));
end;

procedure TFormBigNumber.btnPowerModCompareClick(Sender: TObject);
var
  R, A, B, C: TCnBigNumber;
  I: Integer;
  T1, T2: Cardinal;
begin
  R := TCnBigNumber.Create;
  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  C := TCnBigNumber.Create;

  A.SetDec('10926328966621102888783897267380978171428912483236649140828461455449403718107172349285202724270203158' +
    '05909106990085035046584624778982050630243316098543499294811' +
    '9283579687896291347722073968959855784428052254098891891430648811124969261567848711676522442536268189458421498886245121488307690767638788097157642973084026754820409016' +
    '191637899383762061252595114284993937744366545900318366814966858837552224818274440060649653884332652964198092242717538444823224409505377454471443802886989820795601604571095103301411032999260138166956982194246286502558110700932266723' +
    '474955077801813323617693980110961526430560379713095196479985');
  B.SetDec('34240932876523563834934854726162173283495496872616126318493495');
  C.SetDec('15575420744931366419710570205233760876215102977476052404283164924326802297839585083500021566567882084' +
    '77470109419843820631188309753930725724873800163281678200106' +
    '7737712144748515403323200664612438794490141974524086072335047264180125117076257639828802596826756767050858763222544155925991787713570867390360297598461732436601655205' +
    '715030503008149826851879663736112391510133285948979899043574839934736403237345250591459328567949556277745246326114316877320270131878504750555383083784129172406937229993107665106777698229304957100770658485651755733157282457972650319' +
    '300058715351813427200113310977605056839185172045391687814960');

  T1 := GetTickCount;
  for I := 1 to 1 do
    BigNumberPowerMod(R, A, B, C);
  T1 := GetTickCount - T1;

  ShowMessage(R.ToHex);

  T2 := GetTickCount;
  for I := 1 to 1 do
    BigNumberMontgomeryPowerMod(R, A, B, C);
  T2 := GetTickCount - T2;

  ShowMessage(R.ToDec);
  ShowMessage(IntToStr(T1) + ' : ' + IntToStr(T2));

  C.Free;
  B.Free;
  A.Free;
  R.Free;
end;

procedure TFormBigNumber.btnMulModCompareClick(Sender: TObject);
var
  R, A, B, C: TCnBigNumber;
  I: Integer;
  T1: Cardinal;
begin
  R := TCnBigNumber.Create;
  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  C := TCnBigNumber.Create;

  A.SetDec('10926328966621102888783897267380978171428912483236649140828461455449403718107172349285202724270203158' +
    '05909106990085035046584624778982050630243316098543499294811' +
    '9283579687896291347722073968959855784428052254098891891430648811124969261567848711676522442536268189458421498886245121488307690767638788097157642973084026754820409016' +
    '191637899383762061252595114284993937744366545900318366814966858837552224818274440060649653884332652964198092242717538444823224409505377454471443802886989820795601604571095103301411032999260138166956982194246286502558110700932266723' +
    '474955077801813323617693980110961526430560379713095196479985');
  B.SetDec('34240932876523563834934854726162173283495496872616126318493495');
  C.SetDec('15575420744931366419710570205233760876215102977476052404283164924326802297839585083500021566567882084' +
    '77470109419843820631188309753930725724873800163281678200106' +
    '7737712144748515403323200664612438794490141974524086072335047264180125117076257639828802596826756767050858763222544155925991787713570867390360297598461732436601655205' +
    '715030503008149826851879663736112391510133285948979899043574839934736403237345250591459328567949556277745246326114316877320270131878504750555383083784129172406937229993107665106777698229304957100770658485651755733157282457972650319' +
    '300058715351813427200113310977605056839185172045391687814960');

  T1 := GetTickCount;
  for I := 1 to 100 do
    BigNumberMulMod(R, A, B, C);
  T1 := GetTickCount - T1;

  ShowMessage(R.ToDec);
  ShowMessage(IntToStr(T1));

  C.Free;
  B.Free;
  A.Free;
  R.Free;
end;

procedure TFormBigNumber.btnCheckPrimeClick(Sender: TObject);
var
  R: TCnBigNumber;
  I: Integer;
  T1: Cardinal;
begin
  R := TCnBigNumber.Create;
  R.SetHex('9A674C0F2098EDEAB7B2D07B080273D24AA394AD5457CB72E3093C010E79C7DF1D6808D54F77A0B08E77EA56281C00508B361FE8C47C51458E527' +
    'BEA7BD6430263431D968E27248D85743699ED190D025B3BDE1E0DA6CBC14FFE7F2965C0D953F5CB6E1D9047DCF1F064620B0E295852009F7A5A543E9B1B74' +
    '6871BC144844026DF2DEF6AAB6B0E17761CEFFD8616DDA0341A38A1A006384EE2176F2157AF350E73A1FACBC509A71EA2D69A61B67CD9D449058EADBB2C50' +
    '93B67292A5CD520CC033147789944B94B02B8E7114C8CCD4888ED79813C592F0BBAA0590C032FFFEC6B7D1CB7C1642DC9EFEBDFE6041B9545E73F61DDE75C01489F70F51CCBC1EF69');

  T1 := GetTickCount;
  for I := 1 to 1 do
    BigNumberIsProbablyPrime(R, 5);
  T1 := GetTickCount - T1;
  ShowMessage(IntToStr(T1));
  R.Free;
end;

procedure TFormBigNumber.btnIntPowerClick(Sender: TObject);
begin
  if BigNumberPower(Num2, Num1, seIntPower.Value) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnGetTenCountClick(Sender: TObject);
begin
  ShowMessage(IntToStr(BigNumberGetTenPrecision(Num1)));
end;

procedure TFormBigNumber.btnCheckPrime2Click(Sender: TObject);
var
  R: TCnBigNumber;
  T1: Cardinal;
  S: string;
  B: Boolean;
begin
  R := TCnBigNumber.Create;

  SetLength(S, 6400);
  FillChar(S[1], Length(S), Ord('9'));
  S[48] := '8';
  R.SetDec(S);
  T1 := GetTickCount;
  B := BigNumberIsProbablyPrime(R);    // 2 次得400多秒，默认50次得万把秒，三四个小时
  T1 := GetTickCount - T1;

  if B then
    ShowMessage('Prime')
  else
    ShowMessage('NOT Prime');

  ShowMessage(IntToStr(T1));
  R.Free;
end;

procedure TFormBigNumber.btnBNCRTClick(Sender: TObject);
var
  R, F: TCnBigNumberList;
  M: TCnBigNumber;
begin
  // 有物不知其数，三三数之剩二，五五数之剩三，七七数之剩二。问物几何？
  R := TCnBigNumberList.Create;
  F := TCnBigNumberList.Create;

  F.Add.SetDec('3');
  F.Add.SetDec('5');
  F.Add.SetDec('7');

  R.Add.SetDec('2');
  R.Add.SetDec('3');
  R.Add.SetDec('2');

  M := TCnBigNumber.Create;
  if BigNumberChineseRemainderTheorem(M, R, F) then
    ShowMessage(M.ToDec); // 23

  M.Free;
  F.Free;
  R.Free;
end;

procedure TFormBigNumber.btnBNSqrtClick(Sender: TObject);
var
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberSqrt(Res, Num1) then
    ShowResult(Res);
  BigNumberFree(Res);
  if BigNumberSqrt(Num2, Num2) then
    ShowNumbers;
end;

procedure TFormBigNumber.btnBNNextPrimeClick(Sender: TObject);
begin
  BigNumberNextPrime(Num2, Num1);
  ShowNumbers;
end;

procedure TFormBigNumber.btnBNMulKaratsubaClick(Sender: TObject);
var
  I: Integer;
  Tick: Cardinal;
  Res: TCnBigNumber;
begin
  Res := BigNumberNew;
  if BigNumberMulKaratsuba(Res, Num1, Num2) then
    ShowResult(Res);

  Tick := GetTickCount;
  for I := 0 to 1000 do
    BigNumberMul(Res, Num1, Num2);
  ShowMessage(IntToStr(GetTickCount - Tick));

  Tick := GetTickCount;
  for I := 0 to 1000 do
    BigNumberMulKaratsuba(Res, Num1, Num2);
  ShowMessage(IntToStr(GetTickCount - Tick));

  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnRootClick(Sender: TObject);
var
  R: TCnBigNumber;
begin
  if BigNumberRoot(Num2, Num1, seIntPower.Value) then
    ShowNumbers;

  // 验证 Num2 的 Power 小于等于 Num1，Num2 + 1 的 Power 大于 Num1
  R := TCnBigNumber.Create;

  BigNumberPower(R, Num2, seIntPower.Value);
  if BigNumberCompare(R, Num1) <= 0 then
    ShowMessage('Verify 1 OK');

  BigNumberCopy(R, Num2);
  BigNumberAddWord(R, 1);
  BigNumberPower(R, R, seIntPower.Value);

  if BigNumberCompare(R, Num1) > 0 then
    ShowMessage('Verify 2 OK');

  R.Free;
end;

procedure TFormBigNumber.btnIsPerfectPowerClick(Sender: TObject);
begin
  if BigNumberIsPerfectPower(Num1) then
    ShowMessage('Num 1 is Perfect Power.')
  else
    ShowMessage('Num 1 is NOT Perfect Power.');
end;

procedure TFormBigNumber.btnComNumClick(Sender: TObject);
var
  I: Integer;
  List: TCnBigNumberList;
  P: TCnBigNumber;
begin
  List := TCnBigNumberList.Create;
  try
    BigNumberFillCombinatorialNumbers(List, 1000);
    for I := 0 to List.Count - 1 do
      if List[I] <> nil then
        mmoResult.Lines.Add(Format('%4.4d  -  ', [I]) + List[I].ToDec)
      else
        mmoResult.Lines.Add(Format('%4.4d  -  !!!', [I]));

    mmoResult.Lines.Add('');

    P := TCnBigNumber.FromDec('31');
    BigNumberFillCombinatorialNumbersMod(List, 29, P);
    for I := 0 to List.Count - 1 do
      if List[I] <> nil then
        mmoResult.Lines.Add(Format('%4.4d  -  ', [I]) + List[I].ToDec)
      else
        mmoResult.Lines.Add(Format('%4.4d  -  !!!', [I]));
    P.Free;
  finally
    List.Free;
  end;
end;

procedure TFormBigNumber.btnSBNLTest1Click(Sender: TObject);
var
  S: array of Int64;
begin
  SetLength(S, 9);
  S[0] := 2;
  S[1] := -1;
  S[2] := 3;
  S[3] := 0;
  S[4] := 9;
  S[5] := 0;
  S[6] := 10;
  S[8] := 16;
  SpareList1.SetValues(S);
  SpareList1.Compact;
  mmoSBNL.Lines.Text := SpareList1.ToString;

  SpareList1.SafeValue[10].SetWord(666);
  mmoSBNL.Lines.Text := SpareList1.ToString;
end;

procedure TFormBigNumber.btnSparseMergeClick(Sender: TObject);
begin
  SpareList1.AssignTo(SpareList2);

  // SpareList2.SafeValue[3].SetInt64(18);
  // SpareList2.SafeValue[7].SetInt64(-64);
  SpareList2.SafeValue[9].SetInt64(-60);
//  SpareList2.SafeValue[6].SetInt64(-10);
  mmoSBNL2.Lines.Text := SpareList2.ToString;
  SparseBigNumberListMerge(SpareList3, SpareList1, SpareList2,
    not chkSparseUseSubMerge.Checked);
  mmoSBNL3.Lines.Text := SpareList3.ToString;
end;

procedure TFormBigNumber.btnBNAKSClick(Sender: TObject);
var
  P: TCnBigNumber;
  S: string;
begin
  S := '39779';
  if InputQuery('Hint', 'Enter an Integer Value', S) then
  begin
    P := TCnBigNumber.FromDec(S);
    if BigNumberAKSIsPrime(P) then
      ShowMessage(S + ' Is a Prime')
    else
      ShowMessage('NOT Prime');
    P.Free;
  end;
end;

procedure TFormBigNumber.btnFloatToBigNumberClick(Sender: TObject);
var
  P: Extended;
  S: string;
begin
  S := '120.3';
  if InputQuery('Hint', 'Enter a Float Value', S) then
  begin
    P := StrToFloat(S);
    if BigNumberSetFloat(P, Num1) then
      ShowNumbers;
  end;
end;

procedure TFormBigNumber.btnBigNumberToFloatClick(Sender: TObject);
begin
  ShowMessage(FloatToStr(BigNumberGetFloat(Num1)));
end;

procedure TFormBigNumber.btnBNEulerClick(Sender: TObject);
begin
  BigNumberEuler(Num1, Num2);
  ShowNumbers;
end;

procedure TFormBigNumber.btnMulDivFloatClick(Sender: TObject);
begin
  BigNumberMulFloat(Num2, Num1, Pi);
  BigNumberDivFloat(Num1, Num2, Pi);
  ShowNumbers;
end;

procedure TFormBigNumber.btnNegModInvClick(Sender: TObject);
var
  Res, R: TCnBigNumber;
begin
  Res := BigNumberNew;
  // 如何保证这俩互质？
  BigNumberNegativeModularInverse(Res, Num1, Num2);
  ShowResult(Res);

  // 验证 Res * Num1 mod Num2 = -1
  R := BigNumberNew;
  BigNumberMulMod(R, Res, Num1, Num2);
  if not R.IsNegative then
    BigNumberSub(R, R, Num2);
  if R.IsNegOne then
    ShowMessage('Negative Modular Inverse Check OK')
  else
    ShowMessage('Negative Modular Inverse Check Error: ' + R.ToDec);

  BigNumberFree(R);
  BigNumberFree(Res);
end;

procedure TFormBigNumber.btnMontReductClick(Sender: TObject);
var
  R, N, R2ModN, NNegInv: TCnBigNumber;
begin
  // 测试蒙哥马利约简与蒙哥马利模乘，先用小的数

  N := TCnBigNumber.FromDec('53');
  R := TCnBigNumber.Create;
  R.SetBit(N.GetBitsCount); // 64

  R2ModN := TCnBigNumber.Create;
  NNegInv := TCnBigNumber.Create;

  BigNumberNegativeModularInverse(NNegInv, N, R);

  BigNumberMulMod(R2ModN, R, R, N);

  Num1.SetWord(18);
  Num2.SetWord(48);

  if BigNumberMontgomeryMulMod(Num3, Num1, Num2, R, R2ModN, N, NNegInv) then
  begin
    ShowNumbers;
    ShowResult(Num3); // 18 * 48 mod 53 = 16
  end;

  NNegInv.Free;
  R2ModN.Free;
  R.Free;
  N.Free;
end;

procedure TFormBigNumber.btnMontReduct1Click(Sender: TObject);
var
  R, N, R2ModN, NNegInv: TCnBigNumber;
begin
  // 测试蒙哥马利约简与蒙哥马利模乘，R 用大的素数

  N := TCnBigNumber.FromDec('12550844042077551148644259270604032741055959212145823469311684782060253184499937617056889721462511207045689755920646150143846052328485880703118997163734347');
  //N := TCnBigNumber.FromDec('65537');  // 比较小没问题
  //N := TCnBigNumber.FromDec('15456967440729873067'); // 太大出问题了
  //N := TCnBigNumber.FromDec('831365771'); // 比较小也没问题

  R := TCnBigNumber.Create;
  R.SetBit(N.GetBitsCount);
  ShowMessage('R: ' + R.ToHex);
  ShowMessage('R Bits: ' + IntToStr(R.GetBitsCount));

  R2ModN := TCnBigNumber.Create;
  NNegInv := TCnBigNumber.Create;

  BigNumberNegativeModularInverse(NNegInv, N, R);
  ShowMessage('-N^-1 :' + NNegInv.ToDec);

  BigNumberMulMod(R2ModN, R, R, N);
  ShowMessage('R^2 mod N: ' + R2ModN.ToDec);

  // Test 1, a single T
  Num1.SetWord(864);
  if BigNumberMontgomeryReduction(Num3, Num1, R, N, NNegInv) then
  begin
    BigNumberMulMod(Num3, Num3, R, N);
    ShowNumbers;
    ShowResult(Num3);
    if Num3.IsWord(864) then
      ShowMessage('Test 1 OK')
    else
      ShowMessage('Test 1 Fail');
  end;

  // Test 2，18 * R^2 mod N 去 Mont，得到 18 * R mod N
  Num1.SetWord(18);
  BigNumberMul(Num1, Num1, R2ModN);
  if BigNumberMontgomeryReduction(Num3, Num1, R, N, NNegInv) then
  begin
    // Num3 得到 18 * R mod N，再用 R2 直接计算验证
    Num2.SetWord(18);
    BigNumberMul(Num2, Num2, R);
    BigNumberNonNegativeMod(Num2, Num2, N);

    if BigNumberEqual(Num3, Num2) then
      ShowMessage('Test 2 OK')
    else
      ShowMessage('Test 2 Fail');
  end;

  Num1.SetDec('50844042077551148644259270604032741055959212145623469311684742060253184499937617056839721462511207045689755925646150143846752328485882703118997163734349');
  Num2.SetDec('48234598764323456789098765431234567890');
  if BigNumberMontgomeryMulMod(Num3, Num1, Num2, R, R2ModN, N, NNegInv) then
  begin
    ShowNumbers;
    ShowResult(Num3);

    BigNumberDirectMulMod(R, Num1, Num2, N);
    if BigNumberEqual(R, Num3) then
      ShowMessage('Test 3 OK')
    else
      ShowMessage('Test 3 Fail');
  end;

  NNegInv.Free;
  R2ModN.Free;
  R.Free;
  N.Free;
end;

procedure TFormBigNumber.btnMontReductTimeClick(Sender: TObject);
var
  I: Integer;
  T1, T2: Cardinal;
  R, N, R2ModN, NNegInv: TCnBigNumber;
begin
  N := TCnBigNumber.FromDec(A_PRIME);
  Num1.SetHex(N1);
  Num2.SetHex(N2);

  R := TCnBigNumber.Create;
  R.SetBit(N.GetBitsCount);

  R2ModN := TCnBigNumber.Create;
  NNegInv := TCnBigNumber.Create;

  BigNumberNegativeModularInverse(NNegInv, N, R);

  BigNumberMulMod(R2ModN, R, R, N);

//  if BigNumberMontgomeryMulMod(Num3, Num1, Num2, R, R2ModN, N, NNegInv) then
//  begin
//    ShowNumbers;
//    ShowResult(Num3);
//
//    BigNumberDirectMulMod(R, Num1, Num2, N);
//    if BigNumberEqual(R, Num3) then
//      ShowMessage('Test OK')
//    else
//      ShowMessage('Test Fail');
//  end;

  T1 := GetTickCount;
  for I := 1 to 2000 do
    BigNumberMontgomeryMulMod(Num3, Num1, Num2, R, R2ModN, N, NNegInv);
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 1 to 2000 do
    BigNumberDirectMulMod(Num3, Num1, Num2, N);
  T2 := GetTickCount - T2;

  ShowMessage(Format('Mont %d, Direct %d', [T1, T2]));

  NNegInv.Free;
  R2ModN.Free;
  R.Free;
  N.Free;
end;

procedure TFormBigNumber.btnSquareRootModClick(Sender: TObject);

  function TestSquareRootMod: Boolean;
  begin
    Result := False;
    if BigNumberSquareRootModPrime(Num1, Num2, Num3) then
    begin
      ShowNumbers;
      ShowResult(Num1);

      BigNumberDirectMulMod(Num1, Num1, Num1, Num3);
      Result := BigNumberEqual(Num1, Num2);
    end;
  end;

begin
  Num3.SetWord(65537);
  Num2.SetWord(2);
  if TestSquareRootMod then ShowMessage('OK 1'); // 8u1 的用 Lucas 没问题
  Num3.SetWord(65537);
  Num2.SetWord(200);
  if TestSquareRootMod then ShowMessage('OK 2'); // 8u1 的用 Lucas 没问题

  Num3.SetWord(2147483647);
  Num2.SetWord(202421);
  if TestSquareRootMod then ShowMessage('OK 3'); // 4u3 的没问题，但 202420 似乎就无解
  Num3.SetWord(4294967291);
  Num2.SetWord(202421);
  if TestSquareRootMod then ShowMessage('OK 4'); // 4u3 的没问题

  Num3.SetDec('13');
  Num2.SetWord(3);
  if TestSquareRootMod then ShowMessage('OK 5'); // 8u5 的没问题 9^2 mod 13 = 3
end;

procedure TFormBigNumber.btnBNDebugDumpClick(Sender: TObject);
begin
  ShowMessage(Num1.DebugDump);
end;

procedure TFormBigNumber.btnBNRawDumpClick(Sender: TObject);
var
  L: Integer;
  B: array of Byte;
begin
  L := BigNumberRawDump(Num1);
  if L > 0 then
  begin
    SetLength(B, L);
    BigNumberRawDump(Num1, @B[0]);
    ShowMessage(DataToHex(@B[0], Length(B), True));
    SetLength(B, 0);
  end;
end;

procedure TFormBigNumber.btnBNFieldModInvClick(Sender: TObject);
var
  I: Integer;
  T1, T2: Cardinal;
begin
  Num1.SetDec(A_PRIME);
//  if BigNumberIsProbablyPrime(Num1) then
//    ShowMessage('Is a Prime');

  Num2.SetHex(N1);
  BigNumberModularInverse(Num3, Num2, Num1); // 辗转相除法求模逆元
  ShowMessage(Num3.ToHex());
  BigNumberPrimeModularInverse(Num3, Num2, Num1); // 费马小定理法求素数模逆元
  ShowResult(Num3);

  BigNumberDirectMulMod(Num3, Num2, Num3, Num1);
  if Num3.IsOne then
    ShowMessage('OK');

  T1 := GetTickCount;
  for I := 1 to 10 do
    BigNumberPrimeModularInverse(Num3, Num2, Num1);
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 1 to 10 do
    BigNumberPrimeModularInverse(Num3, Num2, Num1);
  T2 := GetTickCount - T2;

  ShowMessage(Format('Modular Inverse: Prime %d, Normal %d', [T1, T2]));
end;

procedure TFormBigNumber.btnBNNafClick(Sender: TObject);
var
  I: Integer;
  Naf: TShortInts;
  S: string;

  function ShortIntsToStr(SI: TShortInts): string;
  var
    J: Integer;
  begin
    Result := '';
    for J := High(SI) downto Low(SI) do
    begin
      if J = Low(SI) then
        Result := Result + IntToStr(SI[J])
      else
        Result := Result + IntToStr(SI[J]) + ', '
    end;
  end;

begin
  for I := 1 to 7 do // 测试数据 987654321
  begin
    Naf := BigNumberNonAdjanceFormWidth(Num1, I);
    // ShowMessage(IntToStr(Length(Naf)));
    if Length(Naf) > 0 then
    begin
      S := ShortIntsToStr(Naf);
      ShowMessage(IntToStr(I) + '(i-0): ' + S);
    end;
  end;
end;

procedure TFormBigNumber.btnBNHashMapClick(Sender: TObject);
var
  I: Integer;
  Map: TCnBigNumberHashMap;
  N: TCnBigNumber;
begin
  Map := TCnBigNumberHashMap.Create(True, False);

  for I := 0 to 10 do
  begin
    N := TCnBigNumber.Create;
    N.SetWord(I * 100);
    Map.Add(N, N);
  end;

  ShowMessage(IntToStr(Map.Size));

  N := TCnBigNumber.Create;
  N.SetWord(200);
  if Map.HasKey(N) then
    ShowMessage('Has ' + N.ToString);

  N.Free;
  Map.Free;
end;

procedure TFormBigNumber.btnBSGSClick(Sender: TObject);
var
  A, B, M, R, T: TCnBigNumber;
begin
  A := TCnBigNumber.FromFloat(8723);
  B := TCnBigNumber.FromFloat(3623);
  M := TCnBigNumber.FromFloat(65537);

  R := TCnBigNumber.Create;

  if BigNumberBigStepGiantStep(R, A, B, M) then
    ShowMessage(R.ToDec);

  T := TCnBigNumber.Create;
  BigNumberPowerMod(T, A, R, M);
  if BigNumberEqual(T, B) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');

  T.Free;
  R.Free;
  M.Free;
  B.Free;
  A.Free;
end;

procedure TFormBigNumber.btnBase64Click(Sender: TObject);
var
  S: string;
begin
  S := Num1.ToBase64;
  ShowMessage(S);
  Num2.SetBase64(S);
  ShowNumbers;
end;

procedure TFormBigNumber.btnBatchClick(Sender: TObject);
begin
  Assert(TestBigNumberGetBitsCount, 'TestBigNumberGetBitsCount');
  Assert(TestBigNumberShiftLeft, 'TestBigNumberShiftLeft');
  Assert(TestBigNumberDivWord, 'TestBigNumberDivWord');
  Assert(TestBigNumberMulWord, 'TestBigNumberMulWord');
  Assert(TestBigNumberHex, 'TestBigNumberHex');
  Assert(TestBigNumberDec, 'TestBigNumberDec');
  Assert(TestBigNumberUnsignedAdd, 'TestBigNumberUnsignedAdd');
  Assert(TestBigNumberPowerMod, 'TestBigNumberPowerMod');
  Assert(TestBigNumberDiv, 'TestBigNumberDiv');
end;

end.
