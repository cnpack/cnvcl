unit UnitRSA;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, CnBigNumber, CnRSA, CnNativeDecl, CnPrimeNumber,
  ImgList, Buttons, CnCommon, CnPemUtils;

type
  TFormRSA = class(TForm)
    pgc1: TPageControl;
    tsInt64RSA: TTabSheet;
    grpKeys: TGroupBox;
    lblPrime1: TLabel;
    lblPrime2: TLabel;
    lblPrivProduct: TLabel;
    lblPrivExp: TLabel;
    lblPubProduct: TLabel;
    lblPubExp: TLabel;
    edtPrime1: TEdit;
    edtPrime2: TEdit;
    edtPrivProduct: TEdit;
    edtPrivExp: TEdit;
    edtPubProduct: TEdit;
    edtPubExp: TEdit;
    btnGenerateRSA: TButton;
    grpCrypt: TGroupBox;
    lblData: TLabel;
    lblRes: TLabel;
    lblDataBack: TLabel;
    edtData: TEdit;
    edtRes: TEdit;
    edtDataBack: TEdit;
    btnRSAEn: TButton;
    btnRSADe: TButton;
    tsRSA: TTabSheet;
    grpBNKeys: TGroupBox;
    lblBNP1: TLabel;
    edtBNPrime1: TEdit;
    lblBNP2: TLabel;
    edtBNPrime2: TEdit;
    lblBNPrivProduct: TLabel;
    lblBNPrivExp: TLabel;
    lblBNPubProduct: TLabel;
    lblBNPubExp: TLabel;
    btnBNGen: TButton;
    edtBNPrivExp: TEdit;
    edtBNPubExp: TEdit;
    mmoBNPrivProduct: TMemo;
    mmoBNPubProduct: TMemo;
    cbbBits: TComboBox;
    tsEuclid: TTabSheet;
    grpEuclidean: TGroupBox;
    lblEqual: TLabel;
    lblA: TLabel;
    edtA: TEdit;
    lblB: TLabel;
    edtB: TEdit;
    lblX: TLabel;
    edtX: TEdit;
    lblY: TLabel;
    edtY: TEdit;
    btnInt64Euc: TButton;
    btnBNGcd: TButton;
    lblX0: TLabel;
    edtXP: TEdit;
    lblXP: TLabel;
    btnBNLoadKeys: TButton;
    btnBNSaveKeys: TButton;
    btnBNLoadPub: TButton;
    btnSavePub: TButton;
    dlgOpenPEM: TOpenDialog;
    dlgSavePEM: TSaveDialog;
    lblSaveFormat: TLabel;
    cbbSaveFormat: TComboBox;
    Bevel1: TBevel;
    btnSendR: TButton;
    btnBNSendR: TButton;
    tsModInverse: TTabSheet;
    grpModInverse: TGroupBox;
    lbl1: TLabel;
    lblInverse: TLabel;
    lbl2: TLabel;
    lblMA: TLabel;
    edtMa: TEdit;
    lblMB: TLabel;
    edtMb: TEdit;
    btnMInt64MI: TButton;
    lblMX: TLabel;
    edtMX: TEdit;
    lblMY: TLabel;
    edtMY: TEdit;
    lblMX0: TLabel;
    edtMXP: TEdit;
    lblMX2: TLabel;
    lblPY: TLabel;
    edtPY: TEdit;
    btnPQ: TButton;
    lblModulusBits: TLabel;
    bvl1: TBevel;
    lblMBits: TLabel;
    cbbMBits: TComboBox;
    btnGenByM: TButton;
    lblInt64MBits: TLabel;
    chkPureUInt64: TCheckBox;
    chkN64: TCheckBox;
    pgc2: TPageControl;
    tsData: TTabSheet;
    tsFile: TTabSheet;
    ilCrypt: TImageList;
    btnBNRSAEn: TButton;
    edtBNData: TEdit;
    edtBNRes: TEdit;
    edtBNDataBack: TEdit;
    btnBNRSADe: TButton;
    lblBNResult: TLabel;
    lblBNInteger: TLabel;
    lblBNDecrypt: TLabel;
    edtFile1: TEdit;
    lblFile1: TLabel;
    lblFile2: TLabel;
    edtFile2: TEdit;
    btnBrowse1: TButton;
    btnBrowse2: TButton;
    btnPrivCrypt: TBitBtn;
    btnPubCrypt: TButton;
    btnDePrivate: TButton;
    btnDePub: TBitBtn;
    dlgOpenFile: TOpenDialog;
    dlgSaveFile: TSaveDialog;
    tsSign: TTabSheet;
    lblSignFile: TLabel;
    edtSignFile: TEdit;
    btnSignBrowse: TButton;
    btnSignPrivate: TButton;
    lblSignature: TLabel;
    edtSigFile: TEdit;
    btnSignatureBrowse: TButton;
    btnPrivVerify: TButton;
    btnPubVerify: TButton;
    lblSigMethod: TLabel;
    cbbSig: TComboBox;
    btnMInt32MI: TButton;
    btnUInt32MI: TButton;
    btnInt64MI: TButton;
    tsDiffieHellman: TTabSheet;
    grpFactors: TGroupBox;
    lblFactorNumber: TLabel;
    edtDHNumber: TEdit;
    btnFindFactors: TButton;
    lblInt64DHP: TLabel;
    edtDHPrime: TEdit;
    edtDHRoot: TEdit;
    btnGenInt64DH: TButton;
    lblDHRoot: TLabel;
    edtDHXa: TEdit;
    lblDHA: TLabel;
    lblXA: TLabel;
    lblDHB: TLabel;
    lblXb: TLabel;
    edtDHXb: TEdit;
    btnCalcYb: TButton;
    btnCalcXA: TButton;
    edtDHYa: TEdit;
    edtDHYb: TEdit;
    btnDHBCK: TButton;
    btnDHACKey: TButton;
    edtAKey: TEdit;
    edtBKey: TEdit;
    lblDHBits: TLabel;
    cbbDHBits: TComboBox;
    btnDHRand: TButton;
    lblSqrt: TLabel;
    edtFastSqrt: TEdit;
    btnFastSqrt: TButton;
    lblSaveCrypt: TLabel;
    cbbSaveCrypt: TComboBox;
    bvlLoadPEM: TBevel;
    lblKeyHash: TLabel;
    cbbLoadKeyHash: TComboBox;
    chkOAEP: TCheckBox;
    procedure btnGenerateRSAClick(Sender: TObject);
    procedure btnRSAEnClick(Sender: TObject);
    procedure btnRSADeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBNGenClick(Sender: TObject);
    procedure btnInt64EucClick(Sender: TObject);
    procedure btnBNGcdClick(Sender: TObject);
    procedure btnBNRSAEnClick(Sender: TObject);
    procedure btnBNRSADeClick(Sender: TObject);
    procedure btnBNLoadPubClick(Sender: TObject);
    procedure btnBNLoadKeysClick(Sender: TObject);
    procedure btnSavePubClick(Sender: TObject);
    procedure btnBNSaveKeysClick(Sender: TObject);
    procedure btnSendRClick(Sender: TObject);
    procedure btnBNSendRClick(Sender: TObject);
    procedure btnMInt64MIClick(Sender: TObject);
    procedure btnPQClick(Sender: TObject);
    procedure btnGenByMClick(Sender: TObject);
    procedure edtBNChange(Sender: TObject);
    procedure mmoBNChange(Sender: TObject);
    procedure btnBrowse1Click(Sender: TObject);
    procedure btnBrowse2Click(Sender: TObject);
    procedure btnPrivCryptClick(Sender: TObject);
    procedure btnPubCryptClick(Sender: TObject);
    procedure btnDePrivateClick(Sender: TObject);
    procedure btnDePubClick(Sender: TObject);
    procedure btnSignBrowseClick(Sender: TObject);
    procedure btnSignatureBrowseClick(Sender: TObject);
    procedure btnSignPrivateClick(Sender: TObject);
    procedure btnPrivVerifyClick(Sender: TObject);
    procedure btnPubVerifyClick(Sender: TObject);
    procedure btnMInt32MIClick(Sender: TObject);
    procedure btnUInt32MIClick(Sender: TObject);
    procedure btnInt64MIClick(Sender: TObject);
    procedure btnFindFactorsClick(Sender: TObject);
    procedure btnGenInt64DHClick(Sender: TObject);
    procedure btnCalcXAClick(Sender: TObject);
    procedure btnCalcYbClick(Sender: TObject);
    procedure btnDHACKeyClick(Sender: TObject);
    procedure btnDHBCKClick(Sender: TObject);
    procedure btnDHRandClick(Sender: TObject);
    procedure btnFastSqrtClick(Sender: TObject);
  private
    FPrivKeyProduct, FPrivKeyExponent, FPubKeyProduct, FPubKeyExponent, FR: TUInt64;
    FBNR: TCnBigNumber;
    FPrivateKey: TCnRSAPrivateKey;
    FPublicKey: TCnRSAPublicKey;
    procedure CalcR;
    function CalcBitLength(const DecStr: string): Integer;
    function CalcLengthHint(Edit: TEdit): string; overload;
    function CalcLengthHint(Memo: TMemo): string; overload;
  public

  end;

var
  FormRSA: TFormRSA;

implementation

{$R *.DFM}

procedure TFormRSA.btnGenerateRSAClick(Sender: TObject);
var
  Prime1, Prime2: Cardinal;

  function GetInt64BitCount(A: TUInt64): Integer;
  var
    I: Integer;
  begin
    I := 0;
    while (A shr I) <> 0 do
      Inc(I);

    Result := I;
  end;

begin
  if CnInt64RSAGenerateKeys(Prime1, Prime2, FPrivKeyProduct, FPrivKeyExponent,
    FPubKeyProduct, FPubKeyExponent, chkN64.Checked) then
  begin
    edtPrime1.Text := IntToStr(Prime1);
    edtPrime2.Text := IntToStr(Prime2);
    edtPrivProduct.Text := Format('%u', [FPrivKeyProduct]);
    edtPrivExp.Text := Format('%u', [FPrivKeyExponent]);
    edtPubProduct.Text := Format('%u', [FPubKeyProduct]);
    edtPubExp.Text := IntToStr(FPubKeyExponent);

    FR := TUInt64(Prime1 - 1 ) * TUInt64(Prime2 - 1);
    lblInt64MBits.Caption := 'n Bits: ' + IntToStr(GetInt64BitCount(FPrivKeyProduct));
  end;
end;


procedure TFormRSA.btnRSAEnClick(Sender: TObject);
var
  Res, Data: TUInt64;
begin
  Data := StrToUInt64(edtData.Text);
  if UInt64Compare(Data, FPrivKeyProduct) >= 0 then
  begin
    ShowMessage('Data Greater than Private Keys (Product *). Can NOT Encrypt.');
    Exit;
  end;

  if CnInt64RSAEncrypt(Data, FPrivKeyProduct, FPrivKeyExponent, Res) then
    edtRes.Text := Format('%u', [Res]);
end;

procedure TFormRSA.btnRSADeClick(Sender: TObject);
var
  Res, Data: TUInt64;
begin
  Res := StrToUInt64(edtRes.Text);
  if CnInt64RSADecrypt(Res, FPubKeyProduct, FPubKeyExponent, Data) then
    edtDataBack.Text := Format('%u', [Data]);
end;

procedure TFormRSA.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  pgc1.ActivePageIndex := 0;
  cbbBits.ItemIndex := cbbBits.Items.Count - 1;
  cbbMBits.ItemIndex := 2;
  cbbLoadKeyHash.ItemIndex := 0;
  cbbSaveFormat.ItemIndex := 0;
  cbbSaveCrypt.ItemIndex := 0;
  cbbSig.ItemIndex := 0;

  FPrivateKey := TCnRSAPrivateKey.Create;
  FPublicKey := TCnRSAPublicKey.Create;
  FBNR := TCnBigNumber.Create;

{$IFDEF SUPPORT_UINT64}
  chkPureUInt64.Enabled := True;
{$ENDIF}
end;

procedure TFormRSA.FormDestroy(Sender: TObject);
begin
  FBNR.Free;
  FPublicKey.Free;
  FPrivateKey.Free;
end;

procedure TFormRSA.btnBNGenClick(Sender: TObject);
begin
  if CnRSAGenerateKeysByPrimeBits(StrToIntDef(cbbBits.Text, 256), FPrivateKey, FPublicKey) then
  begin
    edtBNPrime1.Text := FPrivateKey.PrimeKey1.ToDec;
    edtBNPrime2.Text := FPrivateKey.PrimeKey2.ToDec;
    mmoBNPrivProduct.Text := FPrivateKey.PrivKeyProduct.ToDec;
    edtBNPrivExp.Text := FPrivateKey.PrivKeyExponent.ToDec;
    mmoBNPubProduct.Text := FPublicKey.PubKeyProduct.ToDec;
    edtBNPubExp.Text := FPublicKey.PubKeyExponent.ToDec;
    lblModulusBits.Caption := 'n Bits: ' + IntToStr(FPublicKey.BitsCount);
  end;
end;

procedure TFormRSA.btnInt64EucClick(Sender: TObject);
var
  A, B, X, Y: TUInt64;
begin
  A := StrToInt64(edtA.Text);
  B := StrToInt64(edtB.Text);
  X := 0;
  Y := 0;
  CnInt64ExtendedEuclideanGcd(A, B, X, Y);
  edtX.Text := IntToStr(X);
  edtY.Text := IntToStr(Y);

  if X < 0 then
  begin
    lblX0.Caption := 'X < 0. Add B to X.';
    edtXP.Text := IntToStr(X + B);
  end
  else
  begin
    lblX0.Caption := 'X > 0. OK.';
    edtXP.Text := IntToStr(X);
  end;
end;

procedure TFormRSA.btnBNGcdClick(Sender: TObject);
var
  A, B, X, Y, R: TCnBigNumber;
begin
  A := TCnBigNumber.FromDec(edtA.Text);
  B := TCnBigNumber.FromDec(edtB.Text);
//  CnDebugger.LogMsg(A.DebugDump);
//  CnDebugger.LogMsg(B.DebugDump);
  X := BigNumberNew;
  Y := BigNumberNew;
  R := BigNumberNew;

  BigNumberExtendedEuclideanGcd(A, B, X, Y);
  edtX.Text := X.ToDec;
  edtY.Text := Y.ToDec;

  if BigNumberIsNegative(X) then
  begin
    lblX0.Caption := 'BN X < 0. Add B to X.';
    // B.SetDec(edtB.Text);
    BigNumberAdd(X, X, B);
    edtXP.Text := X.ToDec;
  end
  else
  begin
    lblX0.Caption := 'BN X > 0. OK.';
    edtXP.Text := X.ToDec;
  end;

  BigNumberFree(R);
  BigNumberFree(Y);
  BigNumberFree(X);
  BigNumberFree(B);
  BigNumberFree(A);
end;

procedure TFormRSA.btnBNRSAEnClick(Sender: TObject);
var
  Data, Res: TCnBigNumber;
begin
  Data := TCnBigNumber.FromDec(edtBNData.Text);
  if BigNumberCompare(Data, FPrivateKey.PrivKeyProduct) >= 0 then
  begin
    ShowMessage('Data Greater than Private Keys (Product *). Can NOT Encrypt.');
    BigNumberFree(Data);
    Exit;
  end;

  Res := BigNumberNew;
  if CnRSAEncrypt(Data, FPrivateKey, Res) then
    edtBNRes.Text := Res.ToDec;

  BigNumberFree(Res);
  BigNumberFree(Data);
end;

procedure TFormRSA.btnBNRSADeClick(Sender: TObject);
var
  Data, Res: TCnBigNumber;
begin
  if Trim(edtBNRes.Text) = '' then
    Exit;

  Data := BigNumberNew;
  Res := TCnBigNumber.FromDec(edtBNRes.Text);

  if CnRSADecrypt(Res, FPublicKey, Data) then
    edtBNDataBack.Text := Data.ToDec;

  BigNumberFree(Res);
  BigNumberFree(Data);
end;

procedure TFormRSA.btnBNLoadPubClick(Sender: TObject);
begin
  if dlgOpenPEM.Execute then
  begin
    if CnRSALoadPublicKeyFromPem(dlgOpenPEM.FileName, FPublicKey) then
    begin
      mmoBNPubProduct.Text := FPublicKey.PubKeyProduct.ToDec;
      edtBNPubExp.Text := FPublicKey.PubKeyExponent.ToDec;
      lblModulusBits.Caption := 'n Bits: ' + IntToStr(FPublicKey.BitsCount);
    end;
  end;
end;

procedure TFormRSA.btnBNLoadKeysClick(Sender: TObject);
var
  X, Y: TCnBigNumber;
  Password: string;
begin
  if dlgOpenPEM.Execute then
  begin
    Password := CnInputBox('Password', 'Enter Password here if the PEM has Password', '');
    if CnRSALoadKeysFromPem(dlgOpenPEM.FileName, FPrivateKey, FPublicKey,
      TCnKeyHashMethod(cbbLoadKeyHash.ItemIndex), Password) then
    begin
      edtBNPrime1.Text := FPrivateKey.PrimeKey1.ToDec;
      edtBNPrime2.Text := FPrivateKey.PrimeKey2.ToDec;
      mmoBNPrivProduct.Text := FPrivateKey.PrivKeyProduct.ToDec;
      edtBNPrivExp.Text := FPrivateKey.PrivKeyExponent.ToDec;
      mmoBNPubProduct.Text := FPublicKey.PubKeyProduct.ToDec;
      edtBNPubExp.Text := FPublicKey.PubKeyExponent.ToDec;
      lblModulusBits.Caption := 'n Bits: ' + IntToStr(FPublicKey.BitsCount);

      X := BigNumberNew;
      Y := BigNumberNew;
      // CnDebugger.LogMsg(FPrivateKey.PrimeKey2.DebugDump);
      // CnDebugger.LogMsg(FPrivateKey.PrimeKey1.DebugDump);
      BigNumberExtendedEuclideanGcd(FPrivateKey.PrimeKey2, FPrivateKey.PrimeKey1, X, Y);
      edtX.Text := X.ToDec;
      edtY.Text := Y.ToDec;
      BigNumberFree(Y);
      BigNumberFree(X);
    end;
  end;
end;

procedure TFormRSA.btnSavePubClick(Sender: TObject);
begin
  if dlgSavePEM.Execute then
  begin
    if CnRSASavePublicKeyToPem(dlgSavePEM.FileName, FPublicKey,
      TCnRSAKeyType(cbbSaveFormat.ItemIndex)) then
      ShowMessage('Saved to ' + dlgSavePEM.FileName);
  end;
end;

procedure TFormRSA.btnBNSaveKeysClick(Sender: TObject);
var
  Password: string;
begin
  if dlgSavePEM.Execute then
  begin
    if cbbSaveCrypt.ItemIndex > 0 then
      Password := CnInputBox('Password', 'Enter Password here for Encryption', '');

    if CnRSASaveKeysToPem(dlgSavePEM.FileName, FPrivateKey, FPublicKey,
      TCnRSAKeyType(cbbSaveFormat.ItemIndex), TCnKeyEncryptMethod(cbbSaveCrypt.ItemIndex),
      TCnKeyHashMethod(cbbLoadKeyHash.ItemIndex), Password) then
      ShowMessage('Saved to ' + dlgSavePEM.FileName);
  end;
end;

procedure TFormRSA.btnSendRClick(Sender: TObject);
begin
  edtB.Text := IntToStr(FR);
  pgc1.ActivePageIndex := 2;
  edtB.SetFocus;
end;

procedure TFormRSA.btnBNSendRClick(Sender: TObject);
begin
  CalcR;
  edtB.Text := FBNR.ToDec;
  pgc1.ActivePageIndex := 2;
  edtB.SetFocus;
end;

procedure TFormRSA.CalcR;
var
  P1, P2, One: TCnBigNumber;
begin
  P1 := TCnBigNumber.Create;
  P2 := TCnBigNumber.Create;
  One := TCnBigNumber.Create;
  One.SetOne;

  BigNumberSub(P1, FPrivateKey.PrimeKey1, One);
  BigNumberSub(P2, FPrivateKey.PrimeKey2, One);
  BigNumberMul(FBNR, P1, P2);

  One.Free;
  P2.Free;
  P1.Free;
end;

procedure TFormRSA.btnMInt64MIClick(Sender: TObject);
var
  A, B, X, Y: TUInt64;
begin
  A := StrToInt64(edtMA.Text);
  B := StrToInt64(edtMB.Text);
  X := 0;
  Y := 0;
  CnInt64ExtendedEuclideanGcd2(A, B, X, Y);
  if X < 0 then
  begin
    lblMX0.Caption := 'X < 0. Add B to X.';
    edtMXP.Text := IntToStr(X + B);
  end
  else
  begin
    lblMX0.Caption := 'X > 0. OK.';
    edtMXP.Text := IntToStr(X);
  end;
  edtPY.Text := IntToStr(-Y);

  edtMX.Text := IntToStr(X);
  edtMY.Text := IntToStr(Y);
end;

procedure TFormRSA.btnPQClick(Sender: TObject);
begin
  edtA.Text := FPrivateKey.PrimeKey2.ToDec;
  edtB.Text := FPrivateKey.PrimeKey1.ToDec;
  pgc1.ActivePageIndex := 2;
end;

procedure TFormRSA.btnGenByMClick(Sender: TObject);
begin
  if CnRSAGenerateKeys(StrToIntDef(cbbMBits.Text, 1024), FPrivateKey, FPublicKey) then
  begin
    edtBNPrime1.Text := FPrivateKey.PrimeKey1.ToDec;
    edtBNPrime2.Text := FPrivateKey.PrimeKey2.ToDec;
    mmoBNPrivProduct.Text := FPrivateKey.PrivKeyProduct.ToDec;
    edtBNPrivExp.Text := FPrivateKey.PrivKeyExponent.ToDec;
    mmoBNPubProduct.Text := FPublicKey.PubKeyProduct.ToDec;
    edtBNPubExp.Text := FPublicKey.PubKeyExponent.ToDec;
    lblModulusBits.Caption := 'n Bits: ' + IntToStr(FPublicKey.BitsCount);
  end;
end;

function TFormRSA.CalcLengthHint(Edit: TEdit): string;
begin
  if Edit.Text = '' then
    Result := '<No Digit>'
  else
    Result := 'Decimal Length: ' + IntToStr(Length(Edit.Text)) + ', Bit Length: '
      + IntToStr(CalcBitLength(Edit.Text));
end;

procedure TFormRSA.edtBNChange(Sender: TObject);
begin
  (Sender as TEdit).Hint := CalcLengthHint(Sender as TEdit);
end;

function TFormRSA.CalcLengthHint(Memo: TMemo): string;
begin
  if Memo.Lines.Text = '' then
    Result := '<No Digit>'
  else
    Result := 'Decimal Length: ' + IntToStr(Length(Memo.Lines.Text))
      + ', Bit Length: ' + IntToStr(CalcBitLength(Memo.Lines.Text));
end;

procedure TFormRSA.mmoBNChange(Sender: TObject);
begin
  (Sender as TMemo).Hint := CalcLengthHint(Sender as TMemo);
end;

function TFormRSA.CalcBitLength(const DecStr: string): Integer;
var
  N: TCnBigNumber;
begin
  N := TCnBigNumber.FromDec(DecStr);
  Result := N.GetBitsCount;
  N.Free;
end;

procedure TFormRSA.btnBrowse1Click(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    edtFile1.Text := dlgOpenFile.FileName;  
end;

procedure TFormRSA.btnBrowse2Click(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    edtFile2.Text := dlgOpenFile.FileName;
end;

procedure TFormRSA.btnPrivCryptClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
  begin
    if CnRSAEncryptFile(edtFile1.Text, dlgSaveFile.FileName, FPrivateKey) then
    begin
      ShowMessage('RSA Private Key Encrypt File Success.');
      if Trim(edtFile2.Text) = '' then
        edtFile2.Text := dlgSaveFile.FileName;
    end;
  end;
end;

procedure TFormRSA.btnPubCryptClick(Sender: TObject);
var
  R: Boolean;
begin
  if dlgSaveFile.Execute then
  begin
    if chkOAEP.Checked then
      R := CnRSAEncryptFile(edtFile1.Text, dlgSaveFile.FileName, FPublicKey, cpmOAEP)
    else
      R := CnRSAEncryptFile(edtFile1.Text, dlgSaveFile.FileName, FPublicKey);

    if R then
    begin
      ShowMessage('RSA Public Key Encrypt File Success.');
      if Trim(edtFile2.Text) = '' then
        edtFile2.Text := dlgSaveFile.FileName;
    end;
  end;
end;

procedure TFormRSA.btnDePrivateClick(Sender: TObject);
var
  R: Boolean;
begin
  if dlgSaveFile.Execute then
  begin
    if chkOAEP.Checked then
      R := CnRSADecryptFile(edtFile2.Text, dlgSaveFile.FileName, FPrivateKey, cpmOAEP)
    else
      R := CnRSADecryptFile(edtFile2.Text, dlgSaveFile.FileName, FPrivateKey);
    if R then
      ShowMessage('RSA Private Key Decrypt File Success.');
  end;
end;

procedure TFormRSA.btnDePubClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
  begin
    if CnRSADecryptFile(edtFile2.Text, dlgSaveFile.FileName, FPublicKey) then
      ShowMessage('RSA Public Key Decrypt File Success.');
  end;
end;

procedure TFormRSA.btnSignBrowseClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    edtSignFile.Text := dlgOpenFile.FileName;
end;

procedure TFormRSA.btnSignatureBrowseClick(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    edtSigFile.Text := dlgOpenFile.FileName;
end;

procedure TFormRSA.btnSignPrivateClick(Sender: TObject);
begin
  if dlgSaveFile.Execute then
  begin
    if CnRSASignFile(edtSignFile.Text, dlgSaveFile.FileName, FPrivateKey,
      TCnRSASignDigestType(cbbSig.ItemIndex)) then
    begin
      ShowMessage('RSA Private Key Sign File Success.');
      edtSigFile.Text := dlgSaveFile.FileName;
    end;
  end;
end;

procedure TFormRSA.btnPrivVerifyClick(Sender: TObject);
begin
//  if CnRSAVerifyFile(edtSignFile.Text, edtSigFile.Text, FPrivateKey,
//    TCnRSASignDigestType(cbbSig.ItemIndex)) then
//    ShowMessage('RSA Private Key Verify Success.')
//  else
//    ShowMessage('RSA Private Key Verify Fail.');
end;

procedure TFormRSA.btnPubVerifyClick(Sender: TObject);
begin
  if CnRSAVerifyFile(edtSignFile.Text, edtSigFile.Text, FPublicKey,
    TCnRSASignDigestType(cbbSig.ItemIndex)) then
    ShowMessage('RSA Public Key Verify Success.')
  else
    ShowMessage('RSA Public Key Verify Fail.');
end;

procedure TFormRSA.btnMInt32MIClick(Sender: TObject);
var
  A, B, X, Y: Cardinal;
begin
  A := StrToInt64(edtMA.Text);
  B := StrToInt64(edtMB.Text);
  X := 0;
  Y := 0;
  CnUInt32ExtendedEuclideanGcd2(A, B, X, Y);
  if UInt32IsNegative(X) then
  begin
    lblMX0.Caption := 'X < 0. Add B to X.';
    edtMXP.Text := IntToStr(X + B);
  end
  else
  begin
    lblMX0.Caption := 'X > 0. OK.';
    edtMXP.Text := IntToStr(X);
  end;
  edtPY.Text := IntToStr(-Y);

  edtMX.Text := IntToStr(X);
  edtMY.Text := IntToStr(Y);
end;

procedure TFormRSA.btnUInt32MIClick(Sender: TObject);
var
  A, B, X: Cardinal;
begin
  A := StrToInt64(edtMA.Text);
  B := StrToInt64(edtMB.Text);
  X := CnUInt32ModularInverse(A, B);
  ShowMessage(IntToStr(X));
end;

procedure TFormRSA.btnInt64MIClick(Sender: TObject);
var
  A, B, X: TUInt64;
begin
  A := StrToInt64(edtMA.Text);
  B := StrToInt64(edtMB.Text);
  X := CnInt64ModularInverse(A, B);
  ShowMessage(UInt64ToStr(X));
end;

procedure TFormRSA.btnFindFactorsClick(Sender: TObject);
var
  Num: TCnBigNumber;
  List: TCnBigNumberList;

  function BigNumberListToString: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to List.Count - 1 do
      Result := Result + ' ' + List[I].ToDec;
  end;

begin
  Num := TCnBigNumber.FromDec(edtDHNumber.Text);
  List := TCnBigNumberList.Create;
  BigNumberFindFactors(Num, List);
  ShowMessage(IntToStr(List.Count) + ' Factors:' + #13#10 + BigNumberListToString);
  List.Free;
  Num.Free;
end;

procedure TFormRSA.btnGenInt64DHClick(Sender: TObject);
var
  Prime, Root: TCnBigNumber;
begin
  Prime := TCnBigNumber.Create;
  Root := TCnBigNumber.Create;

  if CnDiffieHellmanGeneratePrimeRootByBitsCount(StrToIntDef(cbbDHBits.Text, 64), Prime, Root) then
  begin
    edtDHPrime.Text := Prime.ToDec;
    edtDHRoot.Text := Root.ToDec;
  end
  else
    ShowMessage('DH Generation Error.');

  Prime.Free;
  Root.Free;
end;

procedure TFormRSA.btnCalcXAClick(Sender: TObject);
var
  Prime, Root, SelfPrivateKey, OutPublicKey: TCnBigNumber;
begin
  Prime := BigNumberFromDec(edtDHPrime.Text);
  Root := BigNumberFromDec(edtDHRoot.Text);
  SelfPrivateKey := BigNumberFromDec(edtDHXa.Text);
  OutPublicKey := BigNumberNew;

  if CnDiffieHellmanGenerateOutKey(Prime, Root, SelfPrivateKey, OutPublicKey) then
    edtDHYa.Text := OutPublicKey.ToDec;

  Prime.Free;
  Root.Free;
  SelfPrivateKey.Free;
  OutPublicKey.Free;
end;

procedure TFormRSA.btnCalcYbClick(Sender: TObject);
var
  Prime, Root, SelfPrivateKey, OutPublicKey: TCnBigNumber;
begin
  Prime := BigNumberFromDec(edtDHPrime.Text);
  Root := BigNumberFromDec(edtDHRoot.Text);
  SelfPrivateKey := BigNumberFromDec(edtDHXb.Text);
  OutPublicKey := BigNumberNew;

  if CnDiffieHellmanGenerateOutKey(Prime, Root, SelfPrivateKey, OutPublicKey) then
    edtDHYb.Text := OutPublicKey.ToDec;

  Prime.Free;
  Root.Free;
  SelfPrivateKey.Free;
  OutPublicKey.Free;
end;

procedure TFormRSA.btnDHACKeyClick(Sender: TObject);
var
  Prime, SelfPrivateKey, OtherPublicKey, SecretKey: TCnBigNumber;
begin
  Prime := BigNumberFromDec(edtDHPrime.Text);
  SelfPrivateKey := BigNumberFromDec(edtDHXa.Text);
  OtherPublicKey := BigNumberFromDec(edtDHYb.Text);
  SecretKey := BigNumberNew;

  if CnDiffieHellmanComputeKey(Prime, SelfPrivateKey, OtherPublicKey, SecretKey) then
    edtAKey.Text := SecretKey.ToDec;

  Prime.Free;
  SelfPrivateKey.Free;
  OtherPublicKey.Free;
  SecretKey.Free;
end;

procedure TFormRSA.btnDHBCKClick(Sender: TObject);
var
  Prime, SelfPrivateKey, OtherPublicKey, SecretKey: TCnBigNumber;
begin
  Prime := BigNumberFromDec(edtDHPrime.Text);
  SelfPrivateKey := BigNumberFromDec(edtDHXb.Text);
  OtherPublicKey := BigNumberFromDec(edtDHYa.Text);
  SecretKey := BigNumberNew;

  if CnDiffieHellmanComputeKey(Prime, SelfPrivateKey, OtherPublicKey, SecretKey) then
    edtBKey.Text := SecretKey.ToDec;

  Prime.Free;
  SelfPrivateKey.Free;
  OtherPublicKey.Free;
  SecretKey.Free;
end;

procedure TFormRSA.btnDHRandClick(Sender: TObject);
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  BigNumberRandBits(R, StrToIntDef(cbbDHBits.Text, 64));
  edtDHXa.Text := R.ToDec;
  BigNumberRandBits(R, StrToIntDef(cbbDHBits.Text, 64));
  edtDHXb.Text := R.ToDec;
  R.Free;
end;

procedure TFormRSA.btnFastSqrtClick(Sender: TObject);
var
  N: LongWord;
  T: Int64;
begin
  N := StrToUInt64(edtFastSqrt.Text);
  if IntToStr(N) = edtFastSqrt.Text then
    ShowMessage('Integer Sqrt of ' + UInt64ToStr(N) + ' is ' + UInt64ToStr(FastSqrt(N)))
  else
  begin
    T := StrToInt64(edtFastSqrt.Text);
    ShowMessage('Integer Sqrt of Int64 ' + UInt64ToStr(T) + ' is ' + UInt64ToStr(FastSqrt64(T)))
  end;
end;

end.
