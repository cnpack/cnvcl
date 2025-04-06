unit UnitPrime;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, CnNative, CnBigNumber;

type
  TFormPrime = class(TForm)
    pgc1: TPageControl;
    tsGenPrime: TTabSheet;
    btnGen: TButton;
    edtMax: TEdit;
    mmoResult: TMemo;
    tsIsPrime: TTabSheet;
    lblCheck: TLabel;
    edtToPrime: TEdit;
    btnIsPrime: TButton;
    chkQuickGen: TCheckBox;
    lblInt64: TLabel;
    edtInt64: TEdit;
    btnInt64IsPrime: TButton;
    btnCarmichael: TButton;
    mmoCar: TMemo;
    btnGen64: TButton;
    tsMontgomery: TTabSheet;
    edtMonA: TEdit;
    lbl1: TLabel;
    edtMonB: TEdit;
    lblMonMod: TLabel;
    edtMonC: TEdit;
    btnMon: TButton;
    edtMonRes: TEdit;
    btnMonPowerMod64: TButton;
    btn2: TSpeedButton;
    btn65537: TSpeedButton;
    bvl1: TBevel;
    edtMulModA: TEdit;
    lblMul: TLabel;
    edtMulModB: TEdit;
    lblMulMod: TLabel;
    edtMulModC: TEdit;
    edtMulModRes: TEdit;
    btnMulMod: TButton;
    btnMulMod64: TButton;
    bvl2: TBevel;
    edtAddModA: TEdit;
    lblAddMod: TLabel;
    edtAddModB: TEdit;
    edtAddModC: TEdit;
    lbl3: TLabel;
    btnAddMod: TButton;
    btnAddMod64: TButton;
    edtAddModRes: TEdit;
    tsDH: TTabSheet;
    lblInt64DHP: TLabel;
    edtDHPrime: TEdit;
    lblInt64DHRoot: TLabel;
    edtDHRoot: TEdit;
    btnGenInt64DH: TButton;
    btnGenInt32Prime: TButton;
    btnGenInt32DH: TButton;
    lblDHA: TLabel;
    lblXA: TLabel;
    edtDHXa: TEdit;
    edtDHXb: TEdit;
    lblXb: TLabel;
    lblB: TLabel;
    btnCalcXA: TButton;
    btnCalcYb: TButton;
    edtDHYa: TEdit;
    edtDHYb: TEdit;
    btnDHACKey: TButton;
    btnDHBCK: TButton;
    edtAKey: TEdit;
    edtBKey: TEdit;
    btnDHIsRoot32: TButton;
    btnDHIsPrimitiveRoot64: TButton;
    btnDHRand: TButton;
    tsCRT: TTabSheet;
    btnCRTTest: TButton;
    btnCheckPrime: TButton;
    btnInt64BSGS: TButton;
    edtPower: TEdit;
    btnIsPerfectPower: TButton;
    btnInt64AKS: TButton;
    btnCombinatorialNumber: TButton;
    btnComNumMod: TButton;
    chkRaw: TCheckBox;
    btnMoreAKS: TButton;
    bvl3: TBevel;
    btnDHCheck: TButton;
    btnGenDH2: TButton;
    btnGenDH3: TButton;
    btnDHCheck2: TButton;
    bvl21: TBevel;
    btnMontReduct: TButton;
    btnMontMulMod: TButton;
    btnMontMulModTime: TButton;
    bvl4: TBevel;
    edtShor: TEdit;
    btnShor: TButton;
    procedure btnGenClick(Sender: TObject);
    procedure btnIsPrimeClick(Sender: TObject);
    procedure btnInt64IsPrimeClick(Sender: TObject);
    procedure btnCarmichaelClick(Sender: TObject);
    procedure btnGen64Click(Sender: TObject);
    procedure btnMonClick(Sender: TObject);
    procedure btnMonPowerMod64Click(Sender: TObject);
    procedure btn65537Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btnMulMod64Click(Sender: TObject);
    procedure btnMulModClick(Sender: TObject);
    procedure btnAddModClick(Sender: TObject);
    procedure btnAddMod64Click(Sender: TObject);
    procedure btnGenInt64DHClick(Sender: TObject);
    procedure btnGenInt32PrimeClick(Sender: TObject);
    procedure btnGenInt32DHClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCalcXAClick(Sender: TObject);
    procedure btnCalcYbClick(Sender: TObject);
    procedure btnDHACKeyClick(Sender: TObject);
    procedure btnDHBCKClick(Sender: TObject);
    procedure btnDHIsRoot32Click(Sender: TObject);
    procedure btnDHIsPrimitiveRoot64Click(Sender: TObject);
    procedure btnDHRandClick(Sender: TObject);
    procedure btnCRTTestClick(Sender: TObject);
    procedure btnCheckPrimeClick(Sender: TObject);
    procedure btnInt64BSGSClick(Sender: TObject);
    procedure btnIsPerfectPowerClick(Sender: TObject);
    procedure btnInt64AKSClick(Sender: TObject);
    procedure btnCombinatorialNumberClick(Sender: TObject);
    procedure btnComNumModClick(Sender: TObject);
    procedure btnMoreAKSClick(Sender: TObject);
    procedure btnGenDH2Click(Sender: TObject);
    procedure btnGenDH3Click(Sender: TObject);
    procedure btnMontReductClick(Sender: TObject);
    procedure btnMontMulModClick(Sender: TObject);
    procedure btnMontMulModTimeClick(Sender: TObject);
    procedure btnShorClick(Sender: TObject);
  private

  public

  end;

var
  FormPrime: TFormPrime;

implementation

uses
  CnPrime, CnPolynomial, CnContainers;

{$R *.DFM}

{$I Carmichael.inc}

function IsPrime(N: Cardinal): Boolean;
var
  I: Cardinal;
  Sq: Cardinal;
begin
  Result := False;
  if N < 2 then
    Exit;
  if N = 2 then
  begin
    Result := True;
    Exit;
  end;

  if N mod 2 = 0 then
    Exit;

  Sq := Trunc(Sqrt(N));
  I := 3;
  while I <= Sq do
  begin
    if N mod I = 0 then
      Exit;
    Inc(I, 2);
  end;
  Result := True;
end;

procedure TFormPrime.btnGenClick(Sender: TObject);
var
  M, I: Cardinal;
  F: TFileStream;
  L: TStrings;
  T, C: DWORD;
  S: AnsiString;
begin
  M := StrToInt64(edtMax.Text);
  mmoResult.Clear;
  if chkQuickGen.Checked then
  begin
    T := GetTickCount;
    C := 0;
    F := TFileStream.Create('C:\primes.txt', fmCreate);
    L := TStringList.Create;
    for I := 2 to M do
    begin
      if CnUInt32IsPrime(I) then
      begin
        Inc(C);
        L.Add(IntToStr(I) + ',');
      end;
      if I mod 10000000 = 0 then
      begin
        mmoResult.Lines[0] := FloatToStr(I / M) + ' - ' + IntToStr(GetTickCount - T) + ' - ' + IntToStr(C);
        S := L.Text;
        F.Write(S[1], Length(S));
        L.Clear;
        Application.ProcessMessages;
      end;
    end;
    S := L.Text;
    F.Write(S[1], Length(S));
    mmoResult.Lines.Add('C:\primes.txt Done. ' + ' - ' + IntToStr(GetTickCount - T) + 'ms. - Count: ' + IntToStr(C));
    F.Free;
    L.Free;
  end
  else
    for I := 2 to M do
      if IsPrime(I) then
        mmoResult.Lines.Add(IntToStr(I) + ',');
end;

procedure TFormPrime.btnIsPrimeClick(Sender: TObject);
var
  N, Root: Cardinal;
  F: TCnUInt32List;
  S: string;
  I: Integer;
begin
  N := Cardinal(StrToInt64(edtToPrime.Text));
  if chkRaw.Checked then
  begin
    Root := Trunc(Sqrt(N));
    for I := 2 to Root do  // 时间复杂度为 O(根号n)
    begin
      if N mod Cardinal(I) = 0 then
      begin
        ShowMessage('Not Prime Number. One Factor is: ' + IntToStr(I));
        Exit;
      end;
    end;
    ShowMessage('Is Prime Number.');
  end
  else
  begin
    if CnUInt32IsPrime(N) then
      ShowMessage('Is Prime Number.')
    else
    begin
      F := TCnUInt32List.Create;
      CnUInt32FindFactors(N, F);
      S := #13#10#13#10;
      for I := 0 to F.Count - 1 do
        S := S + ' ' + IntToStr(F[I]);
      F.Free;
      N := CnEulerUInt32(N);
      S := S + #13#10 + 'Euler: ' + IntToStr(N);
      ShowMessage('Not Prime Number. Factors are:' + S);
    end;
  end;
end;

procedure TFormPrime.btnInt64IsPrimeClick(Sender: TObject);
var
  N: TUInt64;
  R: Extended;
  Root: Cardinal;
  F: TCnUInt64List;
  S: string;
  I: Integer;
begin
  N := StrToUInt64(edtInt64.Text);
  if chkRaw.Checked then
  begin
    R := N;
    Root := Trunc(Sqrt(R));
    for I := 2 to Root do  // 时间复杂度为 O(根号n)
    begin
      if N mod I = 0 then
      begin
        ShowMessage('Not Prime Number. One Factor is: ' + IntToStr(I));
        Exit;
      end;
    end;
    ShowMessage('Int64 Is Prime Number.');
  end
  else
  begin
    if CnInt64IsPrime(N) then
      ShowMessage('Is Prime Number.')
    else
    begin
      F := TCnUInt64List.Create;
      CnInt64FindFactors(N, F);
      S := #13#10#13#10;
      for I := 0 to F.Count - 1 do
        S := S + ' ' + UInt64ToStr(F[I]);
      F.Free;
      N := CnEulerInt64(N);
      S := S + #13#10 + 'Euler: ' + UInt64ToStr(N);
      ShowMessage('Not Prime Number. Factors are:' + S);
    end;
  end;
end;

procedure TFormPrime.btnCarmichaelClick(Sender: TObject);
var
  I: Integer;
begin
  mmoCar.Clear;
  for I := Low(CN_CARMICHAEL_NUMBERS_GT_UINT32) to High(CN_CARMICHAEL_NUMBERS_GT_UINT32) do
    if CnInt64IsPrime(CN_CARMICHAEL_NUMBERS_GT_UINT32[I]) then
      mmoCar.Lines.Add(IntToStr(CN_CARMICHAEL_NUMBERS_GT_UINT32[I]));
end;

procedure TFormPrime.btnGen64Click(Sender: TObject);
var
  U: TUInt64;
begin
  U := CnGenerateInt64Prime;
  MessageBox(Handle, PChar(UInt64ToStr(U) + ' ($' + IntToHex(U, 2) + ')'), 'Prime', MB_OK);
end;

procedure TFormPrime.btnMonClick(Sender: TObject);
var
  A, B, C, R: TUInt64;
begin
  A := StrToUInt64((edtMonA.Text));
  B := StrToUInt64((edtMonB.Text));
  C := StrToUInt64((edtMonC.Text));
  R := MontgomeryPowerMod(A, B, C);
  edtMonRes.Text := UInt64ToStr(R);
end;

procedure TFormPrime.btnMonPowerMod64Click(Sender: TObject);
{$IFDEF SUPPORT_UINT64}
var
  A, B, C, R: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  A := StrToUInt64((edtMonA.Text));
  B := StrToUInt64((edtMonB.Text));
  C := StrToUInt64((edtMonC.Text));
  R := MontgomeryPowerMod64(A, B, C);
  edtMonRes.Text := UInt64ToStr(R);
{$ENDIF}
end;

procedure TFormPrime.btn65537Click(Sender: TObject);
begin
  edtMonB.Text := '65537';
end;

procedure TFormPrime.btn2Click(Sender: TObject);
begin
  edtMonA.Text := edtMonRes.Text;
end;

procedure TFormPrime.btnMulMod64Click(Sender: TObject);
{$IFDEF SUPPORT_UINT64}
var
  A, B, C, R: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  A := StrToUInt64((edtMulModA.Text));
  B := StrToUInt64((edtMulModB.Text));
  C := StrToUInt64((edtMulModC.Text));
  R := MultipleMod64(A, B, C);
  edtMulModRes.Text := UInt64ToStr(R);
{$ENDIF}
end;

procedure TFormPrime.btnMulModClick(Sender: TObject);
var
  A, B, C, R: TUInt64;
begin
  A := StrToUInt64((edtMulModA.Text));
  B := StrToUInt64((edtMulModB.Text));
  C := StrToUInt64((edtMulModC.Text));
  R := MultipleMod(A, B, C);
  edtMulModRes.Text := UInt64ToStr(R);
end;

procedure TFormPrime.btnAddModClick(Sender: TObject);
var
  A, B, C, R: TUInt64;
begin
  A := StrToUInt64((edtAddModA.Text));
  B := StrToUInt64((edtAddModB.Text));
  C := StrToUInt64((edtAddModC.Text));
  R := AddMod(A, B, C);
  edtAddModRes.Text := UInt64ToStr(R);
end;

procedure TFormPrime.btnAddMod64Click(Sender: TObject);
{$IFDEF SUPPORT_UINT64}
var
  A, B, C, R: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  A := StrToUInt64((edtAddModA.Text));
  B := StrToUInt64((edtAddModB.Text));
  C := StrToUInt64((edtAddModC.Text));
  R := AddMod64(A, B, C);
  edtAddModRes.Text := UInt64ToStr(R);
{$ENDIF}
end;

procedure TFormPrime.btnGenInt64DHClick(Sender: TObject);
var
  P, R: TUInt64;
begin
  CnGenerateInt64DiffieHellmanPrimeMaxRoot(P, R);
  edtDHPrime.Text := UInt64ToStr(P);
  edtDHRoot.Text := UInt64ToStr(R);
end;

procedure TFormPrime.btnGenInt32PrimeClick(Sender: TObject);
var
  U: Cardinal;
begin
  U := CnGenerateUInt32Prime();
  MessageBox(Handle, PChar(IntToStr(U) + ' ($' + IntToHex(U, 2) + ')'), 'Prime', MB_OK);
end;

procedure TFormPrime.btnGenInt32DHClick(Sender: TObject);
var
  P, R: Cardinal;
begin
  CnGenerateUInt32DiffieHellmanPrimeMaxRoot(P, R);
  edtDHPrime.Text := UInt64ToStr(P);
  edtDHRoot.Text := UInt64ToStr(R);
end;

procedure TFormPrime.FormCreate(Sender: TObject);
var
  K: TCnBigNumberBiPolynomial;
  P: TCnInt64Polynomial;
  M: TCnInt64BiPolynomial;
  B: TCnBigNumber;
begin
  pgc1.ActivePageIndex := 0;

  // 为了编译进来
  K := TCnBigNumberBiPolynomial.Create;
  K.ToString;
  K.Free;

  M := TCnInt64BiPolynomial.Create;
  M.ToString;
  M.Free;

  B := TCnBigNumber.Create;
  B.ToString;
  B.Free;

  P := TCnInt64Polynomial.Create;
  P.ToString;
  P.Free;
end;

procedure TFormPrime.btnCalcXAClick(Sender: TObject);
var
  Xa, Ya, Prime, Root: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);
  Xa := StrToUInt64(edtDHXa.Text);
  Ya := MontgomeryPowerMod(Root, Xa, Prime);
  edtDHYa.Text := UInt64ToStr(Ya);
end;

procedure TFormPrime.btnCalcYbClick(Sender: TObject);
var
  Xb, Yb, Prime, Root: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);
  Xb := StrToUInt64(edtDHXb.Text);
  Yb := MontgomeryPowerMod(Root, Xb, Prime);
  edtDHYb.Text := UInt64ToStr(Yb);
end;

procedure TFormPrime.btnDHACKeyClick(Sender: TObject);
var
  Xa, Yb, Prime, Key: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Xa := StrToUInt64(edtDHXa.Text);
  Yb := StrToUInt64(edtDHYb.Text);
  Key := MontgomeryPowerMod(Yb, Xa, Prime);
  edtAKey.Text := UInt64ToStr(Key);
end;

procedure TFormPrime.btnDHBCKClick(Sender: TObject);
var
  Xb, Ya, Prime, Key: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Xb := StrToUInt64(edtDHXb.Text);
  Ya := StrToUInt64(edtDHYa.Text);
  Key := MontgomeryPowerMod(Ya, Xb, Prime);
  edtBKey.Text := UInt64ToStr(Key);
end;

procedure TFormPrime.btnDHIsRoot32Click(Sender: TObject);
var
  Prime, Root: Cardinal;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);

  if CnIsUInt32PrimitiveRoot(Prime, Root) then
    ShowMessage('Is Primitive Root.')
  else
    ShowMessage('NOT Primitive Root.');
end;

procedure TFormPrime.btnDHIsPrimitiveRoot64Click(Sender: TObject);
var
  Prime, Root: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  Root := StrToUInt64(edtDHRoot.Text);

  if CnIsInt64PrimitiveRoot(Prime, Root) then
    ShowMessage('Is Primitive Root.')
  else
    ShowMessage('NOT Primitive Root.');
end;

procedure TFormPrime.btnDHRandClick(Sender: TObject);
var
  Prime: TUInt64;
begin
  Prime := StrToUInt64(edtDHPrime.Text);
  edtDHXa.Text := UInt64ToStr(Trunc(Random * Prime));
  edtDHXb.Text := UInt64ToStr(Trunc(Random * Prime));
end;

procedure TFormPrime.btnCRTTestClick(Sender: TObject);
var
  R, F: array of TUInt64;
  C: TUInt64;
  Rl, Fl: TCnInt64List;
begin
  SetLength(R, 3);
  SetLength(F, 3);

  // 有物不知其数，三三数之剩二，五五数之剩三，七七数之剩二。问物几何？
  F[0] := 3; F[1] := 5; F[2] := 7;
  R[0] := 2; R[1] := 3; R[2] := 2;
  C := ChineseRemainderTheoremInt64(R, F);
  ShowMessage(IntToStr(C));

  Rl := TCnInt64List.Create;
  Fl := TCnInt64List.Create;

  Fl.Add(3); Fl.Add(5); Fl.Add(7);
  Rl.Add(-1); Rl.Add(-2); Rl.Add(-5);
  C := ChineseRemainderTheoremInt64(Rl, Fl);
  ShowMessage(IntToStr(C));
end;

procedure TFormPrime.btnCheckPrimeClick(Sender: TObject);
var
  I: Cardinal;
  K: Int64;
  M: TUInt64;
begin
  I := MaxInt - 1;
  while not CnUInt32IsPrime(I) do   // < Int32
    Dec(I);
  ShowMessage(IntToStr(I));
  I := Cardinal(MaxInt) + 1;
  while not CnUInt32IsPrime(I) do  // > Int32
    Inc(I);
  ShowMessage(IntToStr(I));

  I := $FFFFFFFF;
  while not CnUInt32IsPrime(I) do  // < UInt32
    Dec(I);
  ShowMessage(IntToStr(I));

  K := $FFFFFFFF;
  while not CnInt64IsPrime(K) do  // > UInt32
    Inc(K);
  ShowMessage(IntToStr(K));

  K := CN_MAX_SIGNED_INT64_IN_TUINT64;  // < Int64
  while not CnInt64IsPrime(K) do
    Dec(K);
  ShowMessage(IntToStr(K));

  M := CN_MAX_SIGNED_INT64_IN_TUINT64;  // > Int64
  while not CnInt64IsPrime(M) do
    Inc(M);
  ShowMessage(UInt64ToStr(M));

  M := CN_MAX_TUINT64;
  while not CnInt64IsPrime(M) do     // < UInt64
    Dec(M);
  ShowMessage(UInt64ToStr(M));

  M := UInt64Sqrt(CN_MAX_SIGNED_INT64_IN_TUINT64);
  while not CnInt64IsPrime(M) do     // < Sqrt Int64
    Dec(M);
  ShowMessage(UInt64ToStr(M));

  M := UInt64Sqrt(CN_MAX_SIGNED_INT64_IN_TUINT64) + 1;
  while not CnInt64IsPrime(M) do     // > Sqrt Int64
    Inc(M);
  ShowMessage(UInt64ToStr(M));

  // 比 Sqrt(2 * Max UInt64) 小的，也就是 MaxUInt32 * 1.4142135
  M := Trunc(CN_MAX_UINT32 * 1.4142135);
  while not CnInt64IsPrime(M) do     // < Sqrt 2 * UInt64
    Dec(M);
  ShowMessage(UInt64ToStr(M));

  // 比 Sqrt(2 * Max UInt64) 大的，也就是 MaxUInt32 * 1.4142136
  M := Trunc(CN_MAX_UINT32 * 1.4142136);
  while not CnInt64IsPrime(M) do     // > Sqrt 2 * UInt64
    Inc(M);
  ShowMessage(UInt64ToStr(M));
end;

procedure TFormPrime.btnInt64BSGSClick(Sender: TObject);
var
  A, B, M, R: Int64;
begin
  A := 8723;
  B := 3623;
  M := 65537;

  R := CnInt64BigStepGiantStep(8723, 3623, 65537);
  ShowMessage(IntToStr(R));

  if MontgomeryPowerMod(A, R, M) = B then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormPrime.btnIsPerfectPowerClick(Sender: TObject);
var
  N: Int64;
begin
  N := StrToInt64(edtPower.Text);
  if CnInt64IsPerfectPower(N) then
    ShowMessage('Is Perfect Power')
  else
    ShowMessage('Not Perfect Power');
end;

procedure TFormPrime.btnInt64AKSClick(Sender: TObject);
var
  P: Int64;
  S: string;
begin
  S := '39779';
  if InputQuery('Hint', 'Enter an Integer Value', S) then
  begin
    P := StrToInt64(S);
    if CnInt64AKSIsPrime(P) then
      ShowMessage(S + ' Is a Prime')
    else
      ShowMessage('NOT Prime');
  end;
end;

procedure TFormPrime.btnCombinatorialNumberClick(Sender: TObject);
var
  I: Integer;
  List: TCnInt64List;
  UList: TCnUInt64List;
begin
  List := TCnInt64List.Create;
  UList := TCnUInt64List.Create;
  try
    CnInt64FillCombinatorialNumbers(List, 61);
    for I := 0 to List.Count - 1 do
      mmoResult.Lines.Add(Format('%3.3d  -  ', [I]) +  IntToStr(List[I]));

    mmoResult.Lines.Add('');

    CnUInt64FillCombinatorialNumbers(UList, 62);
    for I := 0 to UList.Count - 1 do
      mmoResult.Lines.Add(Format('%3.3d  -  ', [I]) +  UInt64ToStr(UList[I]));

    pgc1.ActivePageIndex := 0;
  finally
    UList.Free;
    List.Free;
  end;
end;

procedure TFormPrime.btnComNumModClick(Sender: TObject);
var
  I: Integer;
  List: TCnInt64List;
  UList: TCnUInt64List;
begin
  List := TCnInt64List.Create;
  UList := TCnUInt64List.Create;
  try
    CnInt64FillCombinatorialNumbersMod(List, 29, 31);
    for I := 0 to List.Count - 1 do
      mmoResult.Lines.Add(Format('%3.3d  -  ', [I]) +  IntToStr(List[I]));

    mmoResult.Lines.Add('');

    CnUInt64FillCombinatorialNumbersMod(UList, 29, 31);
    for I := 0 to UList.Count - 1 do
      mmoResult.Lines.Add(Format('%3.3d  -  ', [I]) +  UInt64ToStr(UList[I]));

    pgc1.ActivePageIndex := 0;
  finally
    UList.Free;
    List.Free;
  end;
end;

procedure TFormPrime.btnMoreAKSClick(Sender: TObject);
var
  I: Integer;
begin
  for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
  begin
    if CnInt64AKSIsPrime(CN_PRIME_NUMBERS_SQRT_UINT32[I]) then
    begin
      mmoCar.Lines.Add(IntToStr(I) + '  -  ' + IntToStr(CN_PRIME_NUMBERS_SQRT_UINT32[I]));
      Application.ProcessMessages;
    end
    else
      ShowMessage(IntToStr(CN_PRIME_NUMBERS_SQRT_UINT32[I]));
  end;
end;

procedure TFormPrime.btnGenDH2Click(Sender: TObject);
var
  P, G: Cardinal;
begin
  if CnGenerateUInt32DiffieHellmanPrimeMinRoot(P, G) then
  begin
    ShowMessage(Format('%u, %d', [P, G]));
    if CnIsUInt32PrimitiveRoot(P, G) then
      ShowMessage('Check OK')
    else
      ShowMessage('Check Fail');
  end;
end;

procedure TFormPrime.btnGenDH3Click(Sender: TObject);
var
  P, G: TUInt64;
begin
  if CnGenerateInt64DiffieHellmanPrimeMinRoot(P, G) then
  begin
    ShowMessage(Format('%s, %d', [UInt64ToStr(P), G]));
    edtDHPrime.Text := UInt64ToStr(P);
    edtDHRoot.Text := UInt64ToStr(G);

    if CnIsInt64PrimitiveRoot(P, G) then
      ShowMessage('Check OK')
    else
      ShowMessage('Check Fail');
  end;
end;

procedure TFormPrime.btnMontReductClick(Sender: TObject);
var
  NNegInv, N, R, RInv, T, Res: TUInt64;
begin
  // N = 53, R = 64  T = 864
  N := 53;
  R := 64;
  RInv := CnInt64ModularInverse(N, R);
  ShowMessage(UInt64ToStr(RInv)); // 29
  T := 864;
  NNegInv := CnInt64NegativeModularInverse(N, R);
  ShowMessage(UInt64ToStr(NNegInv)); // 35

  Res := CnInt64MontgomeryReduction(T, 6, N, NNegInv); // 2^6 = 64
  // 快速计算 864 * 29 mod 53 = 25056 mod 53 = 40
  ShowMessage(UInt64ToStr(Res));
end;

procedure TFormPrime.btnMontMulModClick(Sender: TObject);
var
  NNegInv, N, R, R2ModN, RInv, T, A, B: TUInt64;
begin
  // N = 53, R = 64  T = 18 * 48
  N := 53;
  R := 64;
  RInv := CnInt64ModularInverse(N, R);
  ShowMessage(UInt64ToStr(RInv)); // 29

  NNegInv := CnInt64NegativeModularInverse(N, R);
  ShowMessage(UInt64ToStr(NNegInv)); // 35

  R2ModN := UInt64NonNegativeMulMod(R, R, N);

  A := 18;
  B := 48;
  T := CnInt64MontgomeryMulMod(A, B, 6, R2ModN, N, NNegInv);
  ShowMessage(UInt64ToStr(T)); // 18 * 48 mod 53 = 16
end;

procedure TFormPrime.btnMontMulModTimeClick(Sender: TObject);
var
  I: Integer;
  T1, T2: Cardinal;
  NNegInv, N, R, RExp, R2ModN, RInv, T, A, B: TUInt64;
begin
  N := 3294619793;
  RExp := (GetUInt32HighBits(N) + 1);
  R := TUInt64(1) shl RExp;
  ShowMessage(UInt64ToHex(R)); // $1 00000000  EExp 为 32

  RInv := CnInt64ModularInverse(N, R);
  ShowMessage(UInt64ToStr(RInv)); // 1181800561

  NNegInv := CnInt64NegativeModularInverse(N, R);
  ShowMessage(UInt64ToStr(NNegInv)); // 3113166735

  R2ModN := UInt64NonNegativeMulMod(R, R, N);
  ShowMessage(UInt64ToStr(R2ModN));  // 1707957645

  A := 1737946195;
  B := 2974637459;
  ShowMessage(UInt64ToStr(MultipleMod(A, B, N))); // 3148701007
  ShowMessage(UInt64ToStr(CnInt64MontgomeryMulMod(A, B, RExp, R2ModN, N, NNegInv))); // 3148701007

  T := 0;
  T1 := GetTickCount;
  for I := 1 to 100000 do
    T := T + CnInt64MontgomeryMulMod(A, B, RExp, R2ModN, N, NNegInv);
  T1 := GetTickCount - T1;

  T := 0;
  T2 := GetTickCount;
  for I := 1 to 100000 do
    T := T + MultipleMod(A, B, N);
  T2 := GetTickCount - T2;

  ShowMessage(Format('Mont %d, Direct %d', [T1, T2])); // 有一定提速作用
end;

procedure TFormPrime.btnShorClick(Sender: TObject);
var
  N, F: TUInt64;
begin
  N := StrToUInt64(edtShor.Text);
  F := CnUInt64Shor(N, 7);

  if F > 1 then
    ShowMessage('OK ' + UInt64ToStr(F))
  else
    ShowMessage('Fail. Prime or Base Error');
end;

end.

