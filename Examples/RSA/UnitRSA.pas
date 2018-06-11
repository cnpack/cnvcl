unit UnitRSA;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, CnBigNumber, CnRSA, CnNativeDecl;

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
    grpBNCrypt: TGroupBox;
    lblBNInteger: TLabel;
    lblBNResult: TLabel;
    lblBNDecrypt: TLabel;
    edtBNData: TEdit;
    edtBNRes: TEdit;
    edtBNDataBack: TEdit;
    btnBNRSAEn: TButton;
    btnBNRSADe: TButton;
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
  cbbSaveFormat.ItemIndex := 0;

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
  Int64ExtendedEuclideanGcd(A, B, X, Y);
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
begin
  if dlgOpenPEM.Execute then
  begin
    if CnRSALoadKeysFromPem(dlgOpenPEM.FileName, FPrivateKey, FPublicKey) then
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
begin
  if dlgSavePEM.Execute then
  begin
    if CnRSASaveKeysToPem(dlgSavePEM.FileName, FPrivateKey, FPublicKey,
      TCnRSAKeyType(cbbSaveFormat.ItemIndex)) then
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
  Int64ExtendedEuclideanGcd2(A, B, X, Y);
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

end.
