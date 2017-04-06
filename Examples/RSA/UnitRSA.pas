unit UnitRSA;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnBigNumber;

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
  private
    FPrivKeyProduct, FPrivKeyExponent, FPubKeyProduct, FPubKeyExponent: Int64;
    FBNPrime1, FBNPrime2: TCnBigNumber;
    FBNPrivKeyProduct, FBNPrivKeyExponent, FBNPubKeyProduct, FBNPubKeyExponent: TCnBigNumber;
  public
    { Public declarations }
  end;

var
  FormRSA: TFormRSA;

implementation

uses
  CnRSA;

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

  FBNPrivKeyProduct := TCnBigNumber.Create;
  FBNPrivKeyExponent := TCnBigNumber.Create;
  FBNPubKeyProduct := TCnBigNumber.Create;
  FBNPubKeyExponent := TCnBigNumber.Create;
  FBNPrime1 := TCnBigNumber.Create;
  FBNPrime2 := TCnBigNumber.Create;
end;

procedure TFormRSA.FormDestroy(Sender: TObject);
begin
  FBNPrivKeyProduct.Free;
  FBNPrivKeyExponent.Free;
  FBNPubKeyProduct.Free;
  FBNPubKeyExponent.Free;
  FBNPrime1.Free;
  FBNPrime2.Free;
end;

procedure TFormRSA.btnBNGenClick(Sender: TObject);
begin
  if CnRSAGenerateKeys(StrToIntDef(cbbBits.Text, 256), FBNPrime1, FBNPrime2,
    FBNPrivKeyProduct, FBNPrivKeyExponent, FBNPubKeyProduct, FBNPubKeyExponent) then
  begin
    edtBNPrime1.Text := FBNPrime1.ToDec;
    edtBNPrime2.Text := FBNPrime2.ToDec;
    mmoBNPrivProduct.Text := FBNPrivKeyProduct.ToDec;
    edtBNPrivExp.Text := FBNPrivKeyExponent.ToDec;
    mmoBNPubProduct.Text := FBNPubKeyProduct.ToDec;
    edtBNPubExp.Text := FBNPubKeyExponent.ToDec;
  end;
end;

procedure TFormRSA.btnInt64EucClick(Sender: TObject);
//var
//  A, B, X, Y: Int64;
begin
//  A := StrToInt64(edtA.Text);
//  B := StrToInt64(edtB.Text);
//  X := 0;
//  Y := 0;
//  Int64ExtendedEuclideanGcd(A, B, X, Y);
//  edtX.Text := IntToStr(X);
//  edtY.Text := IntToStr(Y);
//
//  if X < 0 then
//  begin
//    lblX0.Caption := 'X < 0. Add B to X.';
//    edtXP.Text := IntToStr(X + B);
//  end
//  else
//  begin
//    lblX0.Caption := 'X > 0. OK.';
//    edtXP.Text := IntToStr(X);
//  end;
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
  if BigNumberCompare(Data, FBNPrivKeyProduct) >= 0 then
  begin
    ShowMessage('Data Greater than Private Keys (Product *). Can NOT Encrypt.');
    BigNumberFree(Data);
    Exit;
  end;

  Res := BigNumberNew;
  if CnRSAEncrypt(Data, FBNPrivKeyProduct, FBNPrivKeyExponent, Res) then
    edtBNRes.Text := Res.ToDec;

  BigNumberFree(Res);
  BigNumberFree(Data);
end;

procedure TFormRSA.btnBNRSADeClick(Sender: TObject);
var
  Data, Res: TCnBigNumber;
begin
  Data := BigNumberNew;
  Res := TCnBigNumber.FromDec(edtBNRes.Text);

  if CnRSADecrypt(Res, FBNPubKeyProduct, FBNPubKeyExponent, Data) then
    edtBNDataBack.Text := Data.ToDec;

  BigNumberFree(Res);
  BigNumberFree(Data);
end;

end.
