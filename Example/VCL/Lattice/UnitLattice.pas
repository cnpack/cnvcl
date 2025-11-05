unit UnitLattice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TypInfo, CnLattice, ComCtrls;

type
  TFormLattice = class(TForm)
    pgcLattice: TPageControl;
    tsNTRU: TTabSheet;
    tsBasic: TTabSheet;
    btnInt64GaussianReduceBasis: TButton;
    btnGenNTRUKeys: TButton;
    btnPolynomialNTRU: TButton;
    btnSimpleTest2: TButton;
    btnSimpleTest: TButton;
    grpNTRU: TGroupBox;
    lblNTRUType: TLabel;
    lblPrivateKey: TLabel;
    lblPublicKey: TLabel;
    lblNTRUMessage: TLabel;
    lblNTRUPolynomial: TLabel;
    lblNTRUEnc: TLabel;
    lblNTRUDec: TLabel;
    lblNTRUMessageDec: TLabel;
    cbbNTRUType: TComboBox;
    mmoNTRUPrivateKeyF: TMemo;
    mmoNTRUPublicKey: TMemo;
    edtNTRUMessage: TEdit;
    mmoNTRUPolynomial: TMemo;
    mmoNTRUEnc: TMemo;
    btnNTRUEncrypt: TButton;
    mmoNTRUDec: TMemo;
    edtNTRUMessageDec: TEdit;
    btnNTRUGenerateKeys: TButton;
    mmoNTRUPrivateKeyG: TMemo;
    btnNTRUEncryptBytes: TButton;
    tsMLKEM: TTabSheet;
    grpMLKEM: TGroupBox;
    btnCompressTest: TButton;
    mmoCompress: TMemo;
    btnDeCompressTest: TButton;
    procedure btnSimpleTestClick(Sender: TObject);
    procedure btnInt64GaussianReduceBasisClick(Sender: TObject);
    procedure btnSimpleTest2Click(Sender: TObject);
    procedure btnPolynomialNTRUClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGenNTRUKeysClick(Sender: TObject);
    procedure cbbNTRUTypeChange(Sender: TObject);
    procedure btnNTRUGenerateKeysClick(Sender: TObject);
    procedure btnNTRUEncryptClick(Sender: TObject);
    procedure btnNTRUEncryptBytesClick(Sender: TObject);
    procedure btnCompressTestClick(Sender: TObject);
    procedure btnDeCompressTestClick(Sender: TObject);
  private
    FPriv: TCnNTRUPrivateKey;
    FPub: TCnNTRUPublicKey;
  public
    { Public declarations }
  end;

var
  FormLattice: TFormLattice;

implementation

{$R *.DFM}

uses
  CnBigNumber, CnVector, CnPolynomial, CnNative;

procedure TFormLattice.btnSimpleTestClick(Sender: TObject);
var
  Q, H, F, G: TCnBigNumber;
  M, R, E: TCnBigNumber;
  A, B: TCnBigNumber;
  X, Y, V1, V2: TCnBigNumberVector;
begin
  // q 是大整数这里选个素数
  // 选 f 略小于 q 一半的平方根
  // 选 g 大于 q 四分之一的平方根小于 q 一半的平方根
  // 计算 h = g/f 也就是 g 乘以 f 对于 q 的模拟元
  // 私钥 f g
  // 公钥 h q

  Q := TCnBigNumber.Create;
  H := TCnBigNumber.Create;
  F := TCnBigNumber.Create;
  G := TCnBigNumber.Create;

  M := TCnBigNumber.Create;
  R := TCnBigNumber.Create;
  E := TCnBigNumber.Create;

  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;

  Q.SetHex('FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFF');
  // F 要小于 Q/2 的平方根 B504F3339F5BEAEA45EDB90ABDFFDE01 这里取 AA3A7C074DEF52E20A184147D7586D17
  // G 要大于 Q/4 的平方根 7FFFFFFFBFFFFFFFEFFFFFFFF7FFFFFF 并小于 F 这里取 961E643A19DC196A359774162C793BB1

  F.SetHex('AA3A7C074DEF52E20A184147D7586D17');
  G.SetHex('961E643A19DC196A359774162C793BB1');
  BigNumberModularInverse(H, F, Q);
  BigNumberDirectMulMod(H, H, G, Q);
  // H 得到 1144FB9BEC274A81064DA6D5243258E7F5B997DE76CF45BEFAF0D97FDCCD8751

  // 取明文 M 要小于 Q/4 的平方根取 7BBBBBBBBBBBBBBBBBBBBBBBB7BBBBBB
  // 取随机整数 R 要小于 Q/2 的平方根取 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  // 计算 (R * H + M) mod Q 得到密文
  R.SetHex('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA');
  M.SetHex('7BBBBBBBBBBBBBBBBBBBBBBBB7BBBBBB');
  BigNumberDirectMulMod(E, R, H, Q);
  BigNumberAddMod(E, E, M, Q);

  // E 密文得到 B2EB4167BD48A4914086FE1D29F08E29FB8F8ED3FADC38146C41C8B2947AE56B
  ShowMessage(E.ToHex());

  // 然后解密
  // 先计算 A = F * E mod Q
  // 再计算 A / F mod G 也就是 A 乘以 F 对 G 的模逆元
  BigNumberDirectMulMod(A, F, E, Q);
  BigNumberModularInverse(B, F, G);
  BigNumberDirectMulMod(B, A, B, G);

  ShowMessage(B.ToHex());
  if BigNumberEqual(B, M) then
    ShowMessage('Encryption/Decryption OK');

  // 下面考虑攻击方法
  // 等价于用 (1, H) 和 (0, Q) 两个向量线性组合使其结果 (F', G') 足够短
  // 这个 (F', G') 的作用就等同于 (F, G) 能够用来解密
  // 因此问题简化为二维格上的 SVP 最短向量问题
  // 可直接使用高斯格基约减算法
  // 该算法基于 Gram_Schmidt 正交化，为了满足格的整系数条件要做系数四舍五入的处理

  X := TCnBigNumberVector.Create(2);
  Y := TCnBigNumberVector.Create(2);
  V1 := TCnBigNumberVector.Create(2);
  V2 := TCnBigNumberVector.Create(2);

  X[0].SetWord(1);
  BigNumberCopy(X[1], H);
  Y[0].SetZero;
  BigNumberCopy(Y[1], Q);

  BigNumberGaussianLatticeReduction(V1, V2, X, Y);

  ShowMessage(V1.ToString); // V1 的两个分量据说就是 F' 和 G'
  ShowMessage(V2.ToString);

  // 然后重新解密
  // 先计算 A = F' * E mod Q
  // 再计算 A / F' mod G' 也就是 A 乘以 F' 对 G' 的模逆元
  BigNumberDirectMulMod(A, V1[0], E, Q);
  BigNumberModularInverse(B, V1[0], V1[1]);
  BigNumberDirectMulMod(B, A, B, V1[1]);

  ShowMessage(B.ToHex());
  if BigNumberEqual(B, M) then
    ShowMessage('Attack OK');

  V2.Free;
  V1.Free;
  Y.Free;
  X.Free;

  B.Free;
  A.Free;

  E.Free;
  R.Free;
  M.Free;

  G.Free;
  F.Free;
  H.Free;
  Q.Free;
end;

procedure TFormLattice.btnInt64GaussianReduceBasisClick(Sender: TObject);
var
  V1, V2, X, Y: TCnInt64Vector;
begin
  // (90, 123) 和 (56, 76) 要约减得到 (-6, 3) 和 (-2, -7)
  X := TCnInt64Vector.Create(2);
  Y := TCnInt64Vector.Create(2);
  V1 := TCnInt64Vector.Create(2);
  V2 := TCnInt64Vector.Create(2);

  X[0] := 90;
  X[1] := 123;
  Y[0] := 56;
  Y[1] := 76;

  Int64GaussianLatticeReduction(V1, V2, X, Y);
  ShowMessage(V1.ToString);
  ShowMessage(V2.ToString);

  V2.Free;
  V1.Free;
  Y.Free;
  X.Free;
end;

procedure TFormLattice.btnSimpleTest2Click(Sender: TObject);
var
  F, G, Fp, Fq, Ring, H: TCnInt64Polynomial;
  M, R: TCnInt64Polynomial;
  E: TCnInt64Polynomial;
begin
  // NTRU 多项式例子公开参数 N = 11, P = 3, Q = 32
  // 多项式最高次数 N - 1
  // 随机生成多项式 F 和 G 作为私钥，参数均为 -1 或 0 或 1
  F := TCnInt64Polynomial.Create([-1, 1, 1, 0, -1, 0, 1, 0, 0, 1, -1]);
  // -1+x+x^2-x^4+x^6+x^9-x^10

  G := TCnInt64Polynomial.Create([-1, 0, 1, 1, 0, 1, 0, 0, -1, 0, -1]);
  // -1+x^2+x^3+x^5-x^8-x^10

  Fp := TCnInt64Polynomial.Create();
  Fq := TCnInt64Polynomial.Create();

  Ring := TCnInt64Polynomial.Create();
  Ring.MaxDegree := 11;
  Ring[11] := 1;
  Ring[0] := -1;  // 多项式环为 x^n - 1

  // 求 F 针对 3 与 x^11 - 1 的模逆多项式
  Int64PolynomialGaloisModularInverse(Fp, F, Ring, 3);
  ShowMessage(Fp.ToString);  // 2X^9+X^8+2X^7+X^5+2X^4+2X^3+2X+1

  // 求 F 针对 32 与 x^11 - 1 的模逆多项式
  Int64PolynomialGaloisPrimePowerModularInverse(Fq, F, Ring, 2, 5);
  ShowMessage(Fq.ToString); // 30X^10+18X^9+20X^8+22X^7+16X^6+15X^5+4X^4+16X^3+6X^2+9X+5

  // 计算 H = P * Fq * G mod 32 作为公钥
  H := TCnInt64Polynomial.Create;
  Int64PolynomialGaloisMul(H, Fq, G, 32, Ring);
  Int64PolynomialGaloisMulWord(H, 3, 32);
  ShowMessage(H.ToString);  // 16X^10+19X^9+12X^8+19X^7+15X^6+24X^5+12X^4+20X^3+22X^2+25X+8

  // 加密方的明文和随机数
  M := TCnInt64Polynomial.Create([-1, 0, 0, 1, -1, 0, 0, 0, -1, 1, 1]);  // 明文 X^10+X^9-X^8-X^4+X^3-1
  R := TCnInt64Polynomial.Create([-1, 0, 1, 1, 1, -1, 0, -1]);           // 随机数 -X^7-X^5+X^4+X^3+X^2-1

  E := TCnInt64Polynomial.Create;

  // 加密的密文是在 Ring 上计算 e = r * h + m mod 32
  Int64PolynomialGaloisMul(E, R, H, 32, Ring);
  Int64PolynomialGaloisAdd(E, E, M, 32, Ring);
  ShowMessage(E.ToString); // 19X^10+6X^9+25X^8+7X^7+30X^6+16X^5+14X^4+24X^3+26X^2+11X+14

  // 解密在 Ring 上 f * e mod 32 再 mod 3 再乘以 Fp mod 3
  Int64PolynomialGaloisMul(G, F, E, 32, Ring);

  Int64PolynomialCentralize(G, 32); // 系数降到 -15 到 16 得到 -7X^10-3X^9+5X^8+7X^7+6X^6+7X^5+10X^4-11X^3-10X^2-7X+3

  Int64PolynomialNonNegativeModWord(G, 3);
  Int64PolynomialGaloisMul(G, G, Fp, 3, Ring);
  Int64PolynomialCentralize(G, 3);  // 系数降到 -1 到 1 得到 -7X^10-3X^9+5X^8+7X^7+6X^6+7X^5+10X^4-11X^3-10X^2-7X+3

  ShowMessage(G.ToString);       // 等于明文 X^10+X^9-X^8-X^4+X^3-1 通过!
  ShowMessage('Encryption/Decryption OK');

  // TODO: 下面考虑攻击方法

  E.Free;
  R.Free;
  M.Free;
  H.Free;
  Ring.Free;
  Fq.Free;
  Fp.Free;
  G.Free;
  F.Free;
end;

procedure TFormLattice.btnPolynomialNTRUClick(Sender: TObject);
var
  NTRU: TCnNTRU;
  M, En, De: TCnInt64Polynomial;
begin
  if FPriv.F.IsZero then
  begin
    ShowMessage('Invalid Keys');
    Exit;
  end;

  NTRU := TCnNTRU.Create(cnptHPS2048509);
  // NTRU.GenerateKeys(FPriv, FPub);

  M := TCnInt64Polynomial.Create([1,0,1,1,0,0,0,0,1,-1,-1,-1,-1,-1,-1,0,1,-1]);
  En := TCnInt64Polynomial.Create;
  De := TCnInt64Polynomial.Create;

  NTRU.Encrypt(FPub, M, En);
  ShowMessage(En.ToString);
  NTRU.Decrypt(FPriv, En, De);
  ShowMessage(De.ToString);

  if Int64PolynomialEqual(M, De) then
    ShowMessage('Encryption/Decryption OK');

  De.Free;
  En.Free;
  M.Free;
  NTRU.Free;
end;

procedure TFormLattice.FormCreate(Sender: TObject);
var
  Pt: TCnNTRUParamType;
begin
  FPriv := TCnNTRUPrivateKey.Create;
  FPub := TCnNTRUPublicKey.Create;

  for Pt := Low(TCnNTRUParamType) to High(TCnNTRUParamType) do
    cbbNTRUType.Items.Add(GetEnumName(TypeInfo(TCnNTRUParamType), Ord(Pt)));
end;

procedure TFormLattice.FormDestroy(Sender: TObject);
begin
  FPub.Free;
  FPriv.Free;
end;

procedure TFormLattice.btnGenNTRUKeysClick(Sender: TObject);
var
  NTRU: TCnNTRU;
begin
  NTRU := TCnNTRU.Create(cnptHPS2048509);
  NTRU.GenerateKeys(FPriv, FPub);
  ShowMessage('Keys Generate OK.');
  ShowMessage(FPriv.F.ToString);
  ShowMessage(FPriv.G.ToString);
  ShowMessage(FPub.H.ToString);
  NTRU.Free;
end;

procedure TFormLattice.cbbNTRUTypeChange(Sender: TObject);
begin
  mmoNTRUPrivateKeyF.Lines.Clear;
  mmoNTRUPrivateKeyG.Lines.Clear;
  mmoNTRUPublicKey.Lines.Clear;
end;

procedure TFormLattice.btnNTRUGenerateKeysClick(Sender: TObject);
var
  NTRU: TCnNTRU;
begin
  NTRU := TCnNTRU.Create(TCNNTRUParamType(cbbNTRUType.ItemIndex));
  NTRU.GenerateKeys(FPriv, FPub);
  mmoNTRUPrivateKeyF.Lines.Text := FPriv.F.ToString;
  mmoNTRUPrivateKeyG.Lines.Text := FPriv.G.ToString;
  mmoNTRUPublicKey.Lines.Text := FPub.H.ToString;
  NTRU.Free;
end;

procedure TFormLattice.btnNTRUEncryptClick(Sender: TObject);
var
  NTRU: TCnNTRU;
  B: TBytes;
  Pl, En, De: TCnInt64Polynomial;
  L: Integer;
begin
  NTRU := TCnNTRU.Create(TCNNTRUParamType(cbbNTRUType.ItemIndex));
  B := AnsiToBytes(edtNTRUMessage.Text);

  Pl := TCnInt64Polynomial.Create;
  NTRUDataToInt64Polynomial(Pl, @B[0], Length(B), NTRU.N, NTRU.Prime);
  mmoNTRUPolynomial.Lines.Text := Pl.ToString; // 内容原文转换成多项式

  En := TCnInt64Polynomial.Create;
  NTRU.Encrypt(FPub, Pl, En);
  mmoNTRUEnc.Lines.Text := En.ToString;        // 原文多项式加密后的多项式结果

  De := TCnInt64Polynomial.Create;
  NTRU.Decrypt(FPriv, En, De);                 // 注意这里没有转换成密文数据，而是直接使用密文多项式进行解密

  mmoNTRUDec.Lines.Text := De.ToString;        // 密文多项式解密后的多项式结果

  L := NTRUInt64PolynomialToData(De, NTRU.N, NTRU.Prime, nil);
  if L > 0 then
  begin
    SetLength(B, L);
    NTRUInt64PolynomialToData(De, NTRU.N, NTRU.Prime, @B[0]);
    edtNTRUMessageDec.Text := BytesToAnsi(B);
  end;

  Pl.Free;
  En.Free;
  De.Free;
  NTRU.Free;
end;

procedure TFormLattice.btnNTRUEncryptBytesClick(Sender: TObject);
var
  NTRU: TCnNTRU;
  Pl, En, De: TBytes;
begin
  NTRU := TCnNTRU.Create(TCNNTRUParamType(cbbNTRUType.ItemIndex));
  Pl := AnsiToBytes(edtNTRUMessage.Text);

  En := NTRU.EncryptBytes(FPub, Pl);
  mmoNTRUEnc.Lines.Text := BytesToHex(En);

  De := NTRU.DecryptBytes(FPriv, En);
  edtNTRUMessageDec.Text := BytesToAnsi(De);

  NTRU.Free;
end;

procedure TFormLattice.btnCompressTestClick(Sender: TObject);
var
  I: Integer;
  V, R: Word;
begin
  mmoCompress.Lines.Clear;
  for I := 0 to CN_MLKEM_PRIME - 1 do
  begin
    V := TCnMLKEM.Compress(Word(I), 11);
    R := TCnMLKEM.Decompress(V, 11);
    if I <> R then
    begin
      if Abs(I - R) > 1 then
        mmoCompress.Lines.Add(Format('*** %d -> %d -> %d', [I, V, R]))
      else
        mmoCompress.Lines.Add(Format('* %d -> %d -> %d', [I, V, R]));
    end
    else
      mmoCompress.Lines.Add(Format('  %d -> %d -> %d', [I, V, R]));
  end;
end;

procedure TFormLattice.btnDeCompressTestClick(Sender: TObject);
var
  I: Integer;
  V, R: Word;
begin
  // 2^d 以下应该都相等
  mmoCompress.Lines.Clear;
  for I := 0 to CN_MLKEM_PRIME - 1 do
  begin
    V := TCnMLKEM.Decompress(Word(I), 11);
    R := TCnMLKEM.Compress(V, 11);
    if I <> R then
      mmoCompress.Lines.Add(Format('*** %d -> %d -> %d', [I, V, R]))
    else
      mmoCompress.Lines.Add(Format('%d -> %d -> %d', [I, V, R]));
  end;
end;

end.
