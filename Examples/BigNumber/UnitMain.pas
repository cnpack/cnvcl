unit UnitMain;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnBigNumber, Spin, ExtCtrls, CnCommon, ComCtrls;

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
  ShowMessage(Num1.GetUInt64.ToString);
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

end.
