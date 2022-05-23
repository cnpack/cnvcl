unit UnitPaillier;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, CnBigNumber, CnPaillier;

type
  TFormPaillier = class(TForm)
    pgcPaillier: TPageControl;
    tsInt64Paillier: TTabSheet;
    grpInt64Paillier: TGroupBox;
    btnInt64PaillierSample: TButton;
    btnGenerateKey: TButton;
    lblInt64Public: TLabel;
    edtInt64PublicN: TEdit;
    lblInt64PublicG: TLabel;
    edtInt64PublicG: TEdit;
    lblInt64PrivateP: TLabel;
    edtInt64PrivateP: TEdit;
    lblInt64PrivateQ: TLabel;
    edtInt64PrivateQ: TEdit;
    lblInt64PrivateLambda: TLabel;
    edtInt64PrivateLambda: TEdit;
    lblInt64PrivateMu: TLabel;
    edtInt64PrivateMu: TEdit;
    lblInt64Data: TLabel;
    edtInt64Data: TEdit;
    btnInt64Encrypt: TButton;
    bvl1: TBevel;
    edtInt64PublicN2: TEdit;
    lblInt64PublicN2: TLabel;
    tsBigNumberPaillier: TTabSheet;
    grpBNPaillier: TGroupBox;
    lblBNPublic: TLabel;
    lblBNPublicG: TLabel;
    lblBNPrivateP: TLabel;
    lblBNPrivateQ: TLabel;
    lblBNPrivateLambda: TLabel;
    lblBNPrivateMu: TLabel;
    lblBNData: TLabel;
    bvl11: TBevel;
    lbBNPublicN2: TLabel;
    btnBNPaillierSample: TButton;
    btnBNGenerateKey: TButton;
    edtBNPublicN: TEdit;
    edtBNPublicG: TEdit;
    edtBNPrivateP: TEdit;
    edtBNPrivateQ: TEdit;
    edtBNPrivateLambda: TEdit;
    edtBNPrivateMu: TEdit;
    edtBNData: TEdit;
    btnBNEncrypt: TButton;
    edtBNPublicN2: TEdit;
    bvl2: TBevel;
    lblInt64Data1: TLabel;
    edtInt64Data1: TEdit;
    lblInt64Data2: TLabel;
    edtInt64Data2: TEdit;
    lblInt64Data3: TLabel;
    edtInt64Data3: TEdit;
    edtInt64Enc1: TEdit;
    lblInt64Enc1: TLabel;
    lblInt64Enc2: TLabel;
    edtInt64Enc2: TEdit;
    lblInt64Enc3: TLabel;
    edtInt64Enc3: TEdit;
    btnChecknt64AddHomo: TButton;
    btnInt64PaillierSample2: TButton;
    btnInt64PaillierSample3: TButton;
    mmoBNResult: TMemo;
    btnInt64PaillierSample4: TButton;
    btnInt64PaillierSample5: TButton;
    bvl111: TBevel;
    edtBNData1: TEdit;
    lblBNData1: TLabel;
    lblBNEnc1: TLabel;
    edtBNEnc1: TEdit;
    lblBNEnc2: TLabel;
    lblBNData2: TLabel;
    edtBNData2: TEdit;
    edtBNEnc2: TEdit;
    lblBNData3: TLabel;
    lblBNEnc3: TLabel;
    edtBNEnc3: TEdit;
    edtBNData3: TEdit;
    btnCheckBNAddHomo: TButton;
    procedure btnInt64PaillierSampleClick(Sender: TObject);
    procedure btnGenerateKeyClick(Sender: TObject);
    procedure btnInt64EncryptClick(Sender: TObject);
    procedure edtInt64PublicNChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtBNPublicNChange(Sender: TObject);
    procedure btnBNGenerateKeyClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBNPaillierSampleClick(Sender: TObject);
    procedure btnBNEncryptClick(Sender: TObject);
    procedure btnChecknt64AddHomoClick(Sender: TObject);
    procedure btnInt64PaillierSample2Click(Sender: TObject);
    procedure btnInt64PaillierSample3Click(Sender: TObject);
    procedure btnInt64PaillierSample4Click(Sender: TObject);
    procedure btnInt64PaillierSample5Click(Sender: TObject);
    procedure btnCheckBNAddHomoClick(Sender: TObject);
  private
    FPrivKey: TCnPaillierPrivateKey;
    FPubKey: TCnPaillierPublicKey;
    procedure ShowInt64PaillierKeys(var PrivateKey: TCnInt64PaillierPrivateKey;
      var PublicKey: TCnInt64PaillierPublicKey);
    procedure PutInt64PaillierKeys(var PrivateKey: TCnInt64PaillierPrivateKey;
      var PublicKey: TCnInt64PaillierPublicKey);
    procedure ShowBNPaillierKeys(PrivateKey: TCnPaillierPrivateKey;
      PublicKey: TCnPaillierPublicKey);
    procedure PutBNPaillierKeys(PrivateKey: TCnPaillierPrivateKey;
      PublicKey: TCnPaillierPublicKey);
  public

  end;

var
  FormPaillier: TFormPaillier;

implementation

uses
  CnPrimeNumber, CnNativeDecl;

{$R *.DFM}

procedure TFormPaillier.btnInt64PaillierSampleClick(Sender: TObject);
var
  PrivKey: TCnInt64PaillierPrivateKey;
  PubKey: TCnInt64PaillierPublicKey;
  M, R, T1, T2, N2, K, Enc: Int64;
begin
  PrivKey.P := 7;
  PrivKey.Q := 11;
  PrivKey.Lambda := 30; // 6 和 10 的最小公倍数

  PubKey.N := PrivKey.P * PrivKey.Q; // 77，N^2 为 5929
  PubKey.G := 5652;  // 要验证 5652^2310 mod 5929 是否等于 1，确定等于
                     // IntToStr( MontgomeryPowerMod(5652, 2310, 5929));

  // 如果 G 用 N + 1 为 78，会咋样？先要求阶，再确定 G 针对 N^2 的阶能否被 N 整除，
  // 先求 78 的多少次方 mod 5929 为 1，得到 9 次方
  // Mu 就应该是 Lambda 对 N 的逆元 18

  // 计算 K = ((G^Lambda mod N^2) - 1) div N ，实际上因为 G 的特性，能整除
  // 5652^30 mod 5929 = 3928
  // (3928 - 1) / 77 = 51 得到 K
  // 再计算 K 对 N 的逆元，得到 74
  // PrivKey.Mu := 74;
  K := MontgomeryPowerMod(PubKey.G, PrivKey.Lambda, PubKey.N * PubKey.N) - 1;
  K := K div PubKey.N;
  PrivKey.Mu := CnInt64ModularInverse2(K, PubKey.N); // 得到 74

  ShowInt64PaillierKeys(PrivKey, PubKey);

  // 公钥加密的密文计算过程：密文 = G^明文 * 随机r^N mod N^2

  M := 42; // 明文要求比 N 小，换其他值都行
  R := 23; // 随机数也要求比 N 小，但 23 之外居然很多数不行，大概是 P 或 Q 的倍数都不行

  N2 := PubKey.N * PubKey.N;

  T1 := MontgomeryPowerMod(PubKey.G, M, N2);
  T2 := MontgomeryPowerMod(R, PubKey.N, N2);
  Enc := Int64NonNegativeMulMod(T1, T2, N2);
  ShowMessage(IntToStr(M) + ' Encrypt To ' + IntToStr(Enc)); // 得到密文 4624（如果 R 是 23 的话）

  // 私钥解密的明文还原过程：明文 = (((密文^Lambda mod N^2) - 1)/N) * Mu) mod N
  T1 := MontgomeryPowerMod(Enc, PrivKey.Lambda, PubKey.N * PubKey.N);
  T1 := (T1 - 1) div PubKey.N; // 如果这里不整除，说明出了问题

  R := Int64NonNegativeMulMod(T1, PrivKey.Mu, PubKey.N);
  ShowMessage('Decrypt: ' + IntToStr(R)); // 得到明文 42
end;

procedure TFormPaillier.btnGenerateKeyClick(Sender: TObject);
var
  PrivKey: TCnInt64PaillierPrivateKey;
  PubKey: TCnInt64PaillierPublicKey;
begin
  if CnGenerateInt64PaillierKeys(PrivKey, PubKey) then
    ShowInt64PaillierKeys(PrivKey, PubKey);
end;

procedure TFormPaillier.PutInt64PaillierKeys(
  var PrivateKey: TCnInt64PaillierPrivateKey;
  var PublicKey: TCnInt64PaillierPublicKey);
begin
  PublicKey.N := StrToInt64(edtInt64PublicN.Text);
  PublicKey.G := StrToInt64(edtInt64PublicG.Text);

  PrivateKey.P := StrToInt64(edtInt64PrivateP.Text);
  PrivateKey.Q := StrToInt64(edtInt64PrivateQ.Text);
  PrivateKey.Lambda := StrToInt64(edtInt64PrivateLambda.Text);
  PrivateKey.Mu := StrToInt64(edtInt64PrivateMu.Text);
end;

procedure TFormPaillier.ShowInt64PaillierKeys(
  var PrivateKey: TCnInt64PaillierPrivateKey;
  var PublicKey: TCnInt64PaillierPublicKey);
begin
  edtInt64PublicN.Text := IntToStr(PublicKey.N);
  edtInt64PublicG.Text := IntToStr(PublicKey.G);

  edtInt64PrivateP.Text := IntToStr(PrivateKey.P);
  edtInt64PrivateQ.Text := IntToStr(PrivateKey.Q);
  edtInt64PrivateLambda.Text := IntToStr(PrivateKey.Lambda);
  edtInt64PrivateMu.Text := IntToStr(PrivateKey.Mu);
end;

procedure TFormPaillier.btnInt64EncryptClick(Sender: TObject);
var
  PrivKey: TCnInt64PaillierPrivateKey;
  PubKey: TCnInt64PaillierPublicKey;
  Data, Enc: Int64;
begin
  PutInt64PaillierKeys(PrivKey, PubKey);

  Data := StrToInt64(edtInt64Data.Text);
  if CnInt64PaillierEncrypt(PubKey, Data, Enc) then
  begin
    ShowMessage('Encrypt: ' + UInt64ToStr(Enc));
    if CnInt64PaillierDecrypt(PrivKey, PubKey, Enc, Data) then
      ShowMessage('Decrypt: ' + UInt64ToStr(Data));
  end;
end;

procedure TFormPaillier.edtInt64PublicNChange(Sender: TObject);
var
  A: Int64;
  T: TUInt64;
begin
  A := StrToInt64(edtInt64PublicN.Text);
  T := UInt64Mul(A, A);
  edtInt64PublicN2.Text := UInt64ToStr(T);
end;

procedure TFormPaillier.FormCreate(Sender: TObject);
begin
  edtInt64PublicN.OnChange(edtInt64PublicN);
  edtBNPublicN.OnChange(edtBNPublicN);

  FPrivKey := TCnPaillierPrivateKey.Create;
  FPubKey := TCnPaillierPublicKey.Create;
end;

procedure TFormPaillier.PutBNPaillierKeys(
  PrivateKey: TCnPaillierPrivateKey; PublicKey: TCnPaillierPublicKey);
begin
  PublicKey.N.SetDec(edtBNPublicN.Text);
  PublicKey.G.SetDec(edtBNPublicG.Text);
  BigNumberMul(PublicKey.N2, PublicKey.N, PublicKey.N);

  PrivateKey.P.SetDec(edtBNPrivateP.Text);
  PrivateKey.Q.SetDec(edtBNPrivateQ.Text);
  PrivateKey.Lambda.SetDec(edtBNPrivateLambda.Text);
  PrivateKey.Mu.SetDec(edtBNPrivateMu.Text);
end;

procedure TFormPaillier.ShowBNPaillierKeys(
  PrivateKey: TCnPaillierPrivateKey; PublicKey: TCnPaillierPublicKey);
begin
  edtBNPublicN.Text := PublicKey.N.ToDec;
  edtBNPublicG.Text := PublicKey.G.ToDec;

  edtBNPrivateP.Text := PrivateKey.P.ToDec;
  edtBNPrivateQ.Text := PrivateKey.Q.ToDec;
  edtBNPrivateLambda.Text := PrivateKey.Lambda.ToDec;
  edtBNPrivateMu.Text := PrivateKey.Mu.ToDec;
end;

procedure TFormPaillier.edtBNPublicNChange(Sender: TObject);
var
  A: TCnBigNumber;
begin
  A := TCnBigNumber.Create;
  try
    A.SetDec(edtBNPublicN.Text);
    BigNumberMul(A, A, A);
    edtBNPublicN2.Text := A.ToDec;
  finally
    A.Free;
  end;
end;

procedure TFormPaillier.btnBNGenerateKeyClick(Sender: TObject);
begin
  if CnGeneratePaillierKeys(FPrivKey, FPubKey, 256) then
    ShowBNPaillierKeys(FPrivKey, FPubKey);
end;

procedure TFormPaillier.FormDestroy(Sender: TObject);
begin
  FPrivKey.Free;
  FPubKey.Free;
end;

procedure TFormPaillier.btnBNPaillierSampleClick(Sender: TObject);
var
  Data, EnData: TCnBigNumber;
begin
  FPrivKey.P.SetWord(7);
  FPrivKey.Q.SetWord(11);
  FPrivKey.Lambda.SetWord(30);

  BigNumberMul(FPubKey.N, FPrivKey.P, FPrivKey.Q);
  BigNumberMul(FPubKey.N2, FPubKey.N, FPubKey.N);

  FPubKey.G.SetWord(5652);

  // BigNumberModularInverse(FPrivKey.Mu, )
  FPrivKey.Mu.SetWord(74);

  Data := TCnBigNumber.Create;
  Data.SetWord(42);  // 对于 R 随机取 21 仍然解错误
  EnData := TCnBigNumber.Create;

  if CnPaillierEncrypt(FPubKey, Data, EnData) then
  begin
    ShowMessage(Data.ToDec + ' Encrypt to: ' + EnData.ToDec);
    if CnPaillierDecrypt(FPrivKey, FPubKey, EnData, Data) then
      ShowMessage('Decrypt to: ' + Data.ToDec);
  end;

  EnData.Free;
  Data.Free;
end;

procedure TFormPaillier.btnBNEncryptClick(Sender: TObject);
var
  Data, EnData: TCnBigNumber;
begin
  PutBNPaillierKeys(FPrivKey, FPubKey);

  Data := TCnBigNumber.Create;
  Data.SetDec(edtBNData.Text);
  EnData := TCnBigNumber.Create;

  if CnPaillierEncrypt(FPubKey, Data, EnData) then
  begin
    mmoBNResult.Lines.Clear;
    mmoBNResult.Lines.Add(EnData.ToDec);
    // ShowMessage(Data.ToDec + ' Encrypt to: ' + EnData.ToDec);
    if CnPaillierDecrypt(FPrivKey, FPubKey, EnData, Data) then
      ShowMessage('Decrypt to: ' + Data.ToDec);
  end;

  EnData.Free;
  Data.Free;
end;

procedure TFormPaillier.btnChecknt64AddHomoClick(Sender: TObject);
var
  Data1, Data2, Enc1, Enc2, Enc3: Int64;
  PrivKey: TCnInt64PaillierPrivateKey;
  PubKey: TCnInt64PaillierPublicKey;
begin
  PutInt64PaillierKeys(PrivKey, PubKey);

  Data1 := StrToInt64(edtInt64Data1.Text);
  Data2 := StrToInt64(edtInt64Data2.Text);
  if CnInt64PaillierEncrypt(PubKey, Data1, Enc1) then
    edtInt64Enc1.Text := UInt64ToStr(Enc1);

  if CnInt64PaillierEncrypt(PubKey, Data2, Enc2) then
    edtInt64Enc2.Text := UInt64ToStr(Enc2);

  edtInt64Data3.Text := UInt64ToStr(CnInt64PaillierAddPlain(Data1, Data2, PubKey));

  Enc3 := CnInt64PaillierAddCipher(Enc1, Enc2, PubKey);
  edtInt64Enc3.Text := UInt64ToStr(Enc3);

  if CnInt64PaillierDecrypt(PrivKey, PubKey, Enc3, Data1) then
    ShowMessage('Decrypt Enc1*Enc2 to: ' + UInt64ToStr(Data1));
end;

procedure TFormPaillier.btnInt64PaillierSample2Click(Sender: TObject);
var
  PrivKey: TCnInt64PaillierPrivateKey;
  PubKey: TCnInt64PaillierPublicKey;
  I: Integer;
  N2, Data, En, NewData, T1, T2: Int64;
begin
  PrivKey.P := 7;
  PrivKey.Q := 11;
  PrivKey.Lambda := 30; // (7 - 1) * (11 - 1) 但不能用 60

  PubKey.N := 7 * 11;
  PubKey.G := PubKey.N + 1; // 78

//  K := MontgomeryPowerMod(PubKey.G, PrivKey.Lambda, PubKey.N * PubKey.N) - 1;
//  K := K div PubKey.N;
//  PrivKey.Mu := CnInt64ModularInverse2(K, PubKey.N); // K 算到 30 等于 Lambda，也得到 18
//  if PrivKey.Mu <> 0 then

  PrivKey.Mu := CnInt64ModularInverse2(PrivKey.Lambda, PubKey.N); // 30 对 77 的模逆元 18

  Data := 42;
  N2 := PubKey.N * PubKey.N;
  for I := 1 to PubKey.N - 1 do
  begin
    // 加密
{$IFDEF SUPPORT_UINT64}
    T1 := MontgomeryPowerMod(PubKey.G, UInt64(Data), N2);
{$ELSE}
    T1 := MontgomeryPowerMod(PubKey.G, Data, N2);
{$ENDIF}
    T2 := MontgomeryPowerMod(I, PubKey.N, N2);
    En := UInt64NonNegativeMulMod(T1, T2, N2); // 得到密文

    // 解密
{$IFDEF SUPPORT_UINT64}
    T1 := MontgomeryPowerMod(UInt64(En), PrivKey.Lambda, N2);
{$ELSE}
    T1 := MontgomeryPowerMod(En, PrivKey.Lambda, N2);
{$ENDIF}

    T1 := UInt64Div(T1 - 1, PubKey.N); // 这里按 G 的设定，能整除
    NewData := Int64NonNegativeMulMod(T1, PrivKey.Mu, PubKey.N);

    if NewData <> Data then
      ShowMessage(Format('#%d: %d -> %d -> %d', [I, Data, En, NewData]));
  end;
end;

procedure TFormPaillier.btnInt64PaillierSample3Click(Sender: TObject);
var
  PrivKey: TCnInt64PaillierPrivateKey;
  PubKey: TCnInt64PaillierPublicKey;
  I: Integer;
  N2, Data, En, NewData, T1, T2{, K}: Int64;
begin
  PrivKey.P := 3;
  PrivKey.Q := 5;
  PrivKey.Lambda := 4;

  PubKey.N := 3 * 5;
  PubKey.G := PubKey.N + 1; // 15

//  K := MontgomeryPowerMod(PubKey.G, PrivKey.Lambda, PubKey.N * PubKey.N) - 1;
//  K := K div PubKey.N;
//  PrivKey.Mu := CnInt64ModularInverse2(K, PubKey.N); // K 算到 4 等于 Lambda，也得到 4
//  if PrivKey.Mu <> 0 then

  PrivKey.Mu := CnInt64ModularInverse2(PrivKey.Lambda, PubKey.N); // 4 对 15 的模逆元 4

  Data := 9;
  N2 := PubKey.N * PubKey.N;
  for I := 1 to PubKey.N - 1 do
  begin
    // 加密
{$IFDEF SUPPORT_UINT64}
    T1 := MontgomeryPowerMod(PubKey.G, UInt64(Data), N2);
{$ELSE}
    T1 := MontgomeryPowerMod(PubKey.G, Data, N2);
{$ENDIF}
    T2 := MontgomeryPowerMod(I, PubKey.N, N2);
    En := UInt64NonNegativeMulMod(T1, T2, N2); // 得到密文

    // 解密
{$IFDEF SUPPORT_UINT64}
    T1 := MontgomeryPowerMod(UInt64(En), PrivKey.Lambda, N2);
{$ELSE}
    T1 := MontgomeryPowerMod(En, PrivKey.Lambda, N2);
{$ENDIF}

    if UInt64Mod(T1 - 1, PubKey.N) <> 0 then // 随机数 I 和 N 不互质时会出错
    begin
      ShowMessage(Format('Error #%d: %d -> %d', [I, Data, En]));
      Continue;
    end;
     
    T1 := UInt64Div(T1 - 1, PubKey.N); // 这里按 G 的设定，能整除
    NewData := Int64NonNegativeMulMod(T1, PrivKey.Mu, PubKey.N);

    if NewData <> Data then
      ShowMessage(Format('#%d: %d -> %d -> %d', [I, Data, En, NewData]));
  end;
end;

procedure TFormPaillier.btnInt64PaillierSample4Click(Sender: TObject);
var
  Data1, Data2, Data3, Enc1, Enc2, Enc3, Dec3: Int64;
  PrivKey: TCnInt64PaillierPrivateKey;
  PubKey: TCnInt64PaillierPublicKey;
  I: Integer;
begin
  PutInt64PaillierKeys(PrivKey, PubKey);

  Data1 := StrToInt64(edtInt64Data1.Text);
  Data2 := StrToInt64(edtInt64Data2.Text);

  for I := 1 to 1000 do
  begin
    if CnInt64PaillierEncrypt(PubKey, Data1, Enc1, I) then
      edtInt64Enc1.Text := UInt64ToStr(Enc1);

    if CnInt64PaillierEncrypt(PubKey, Data2, Enc2, I) then
      edtInt64Enc2.Text := UInt64ToStr(Enc2);

    Data3 := CnInt64PaillierAddPlain(Data1, Data2, PubKey);
    edtInt64Data3.Text := UInt64ToStr(Data3);

    Enc3 := CnInt64PaillierAddCipher(Enc1, Enc2, PubKey);
    edtInt64Enc3.Text := UInt64ToStr(Enc3);

    if CnInt64PaillierDecrypt(PrivKey, PubKey, Enc3, Dec3) then
    begin
      if Dec3 <> Data3 then
      begin
        ShowMessage(IntToStr(I) + 'Decrypt Enc1*Enc2 to: ' + UInt64ToStr(Dec3));
        Exit;
      end;
    end;
  end;
end;

procedure TFormPaillier.btnInt64PaillierSample5Click(Sender: TObject);
var
  Enc1, Enc2, Enc3, Dec3: Int64;
  Prk: TCnInt64PaillierPrivateKey;
  Puk: TCnInt64PaillierPublicKey;
begin
  Prk.P := 61723;
  Prk.Q := 62053;
  Prk.Lambda := 638328924;
  Prk.Mu := 1352223169;

  Puk.N := 3830097319;
  Puk.G := 3830097320;

  CnInt64PaillierEncrypt(Puk, 23, Enc1, 3);
  CnInt64PaillierEncrypt(Puk, 74, Enc2, 3);
  Enc3 := CnInt64PaillierAddCipher(Enc1, Enc2, Puk);

  CnInt64PaillierDecrypt(Prk, Puk, Enc3, Dec3);
  if Dec3 <> CnInt64PaillierAddPlain(23, 74, Puk) then
    ShowMessage('Error')
  else
    ShowMessage('OK');
end;

procedure TFormPaillier.btnCheckBNAddHomoClick(Sender: TObject);
var
  Data1, Data2, Data3, Enc1, Enc2, Enc3: TCnBigNumber;
begin
  PutBNPaillierKeys(FPrivKey, FPubKey);

  Data1 := TCnBigNumber.Create;
  Data2 := TCnBigNumber.Create;
  Data3 := TCnBigNumber.Create;
  Enc1 := TCnBigNumber.Create;
  Enc2 := TCnBigNumber.Create;
  Enc3 := TCnBigNumber.Create;

  Data1.SetDec(edtBNData1.Text);
  Data2.SetDec(edtBNData2.Text);
  Data3.SetDec(edtBNData3.Text);

  if CnPaillierEncrypt(FPubKey, Data1, Enc1) then
    edtBNEnc1.Text := Enc1.ToDec;
  if CnPaillierEncrypt(FPubKey, Data2, Enc2) then
    edtBNEnc2.Text := Enc2.ToDec;

  if CnPaillierAddPlain(Data3, Data1, Data2, FPubKey) then
    edtBNData3.Text := Data3.ToDec;

  if CnPaillierAddCipher(Enc3, Enc1, Enc2, FPubKey) then
    edtBNEnc3.Text := Enc3.ToDec;


  if CnPaillierDecrypt(FPrivKey, FPubKey, Enc3, Data1) then
  begin
    if BigNumberEqual(Data1, Data3) then
      ShowMessage('OK')
    else
      ShowMessage('Fail');
  end;

  Enc3.Free;
  Enc2.Free;
  Enc1.Free;
  Data3.Free;
  Data2.Free;
  Data1.Free;
end;

end.
