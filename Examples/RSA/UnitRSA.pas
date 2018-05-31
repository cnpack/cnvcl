unit UnitRSA;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnBigNumber, CnRSA, ExtCtrls;

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
    lblBits: TLabel;
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
  private
    FPrivKeyProduct, FPrivKeyExponent, FPubKeyProduct, FPubKeyExponent, FR: Int64;
    FBNR: TCnBigNumber;
    FPrivateKey: TCnRSAPrivateKey;
    FPublicKey: TCnRSAPublicKey;
    procedure CalcR;
  public

  end;

var
  FormRSA: TFormRSA;

implementation

{$R *.DFM}

procedure TFormRSA.btnGenerateRSAClick(Sender: TObject);
var
  Prime1, Prime2: Integer;
begin
  if CnInt64RSAGenerateKeys(Prime1, Prime2, FPrivKeyProduct, FPrivKeyExponent,
    FPubKeyProduct, FPubKeyExponent) then
  begin
    edtPrime1.Text := IntToStr(Prime1);
    edtPrime2.Text := IntToStr(Prime2);
    edtPrivProduct.Text := Format('%d', [FPrivKeyProduct]);
    edtPrivExp.Text := Format('%d', [FPrivKeyExponent]);
    edtPubProduct.Text := Format('%d', [FPubKeyProduct]);
    edtPubExp.Text := IntToStr(FPubKeyExponent);

    FR := Int64(Prime1 - 1 ) * Int64(Prime2 - 1); 
  end;
end;


procedure TFormRSA.btnRSAEnClick(Sender: TObject);
var
  Res, Data: Int64;
begin
  Data := StrToInt64(edtData.Text);
  if Data >= FPrivKeyProduct then
  begin
    ShowMessage('Data Greater than Private Keys (Product *). Can NOT Encrypt.');
    Exit;
  end;

  if CnInt64RSAEncrypt(Data, FPrivKeyProduct, FPrivKeyExponent, Res) then
    edtRes.Text := Format('%d', [Res]);
end;

procedure TFormRSA.btnRSADeClick(Sender: TObject);
var
  Res, Data: Int64;
begin
  Res := StrToInt64(edtRes.Text);
  if CnInt64RSADecrypt(Res, FPubKeyProduct, FPubKeyExponent, Data) then
    edtDataBack.Text := Format('%d', [Data]);
end;

procedure TFormRSA.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  pgc1.ActivePageIndex := 0;
  cbbBits.ItemIndex := cbbBits.Items.Count - 1;
  cbbSaveFormat.ItemIndex := 0;

  FPrivateKey := TCnRSAPrivateKey.Create;
  FPublicKey := TCnRSAPublicKey.Create;
  FBNR := TCnBigNumber.Create;
end;

procedure TFormRSA.FormDestroy(Sender: TObject);
begin
  FBNR.Free;
  FPublicKey.Free;
  FPrivateKey.Free;
end;

procedure TFormRSA.btnBNGenClick(Sender: TObject);
begin
  if CnRSAGenerateKeys(StrToIntDef(cbbBits.Text, 256), FPrivateKey, FPublicKey) then
  begin
    edtBNPrime1.Text := FPrivateKey.PrimeKey1.ToDec;
    edtBNPrime2.Text := FPrivateKey.PrimeKey2.ToDec;
    mmoBNPrivProduct.Text := FPrivateKey.PrivKeyProduct.ToDec;
    edtBNPrivExp.Text := FPrivateKey.PrivKeyExponent.ToDec;
    mmoBNPubProduct.Text := FPublicKey.PubKeyProduct.ToDec;
    edtBNPubExp.Text := FPublicKey.PubKeyExponent.ToDec;
  end;
end;

procedure TFormRSA.btnInt64EucClick(Sender: TObject);
var
  A, B, X, Y: Int64;
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
  X := BigNumberNew;
  Y := BigNumberNew;
  R := BigNumberNew;

  BigNumberExtendedEuclideanGcd(A, B, X, Y, R);
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
    end;
  end;
end;

procedure TFormRSA.btnBNLoadKeysClick(Sender: TObject);
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

end.
