unit UnitLattice;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TypInfo, CnLattice, ComCtrls, CnNative, CnMLKEM, CnMLDSA;

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
    mmoMLKEM: TMemo;
    btnDeCompressTest: TButton;
    btnMLKEMSamplePolyCBD: TButton;
    edtSamleEta: TEdit;
    edtMLKEMD: TEdit;
    btnMLKEMSampleNtt: TButton;
    btnMLKEMKeyGen: TButton;
    mmoMLKEMKeys: TMemo;
    chkMLKEMUsePre: TCheckBox;
    lblMLKEMMsg: TLabel;
    edtMLKEMMsg: TEdit;
    btnMLKEMEncrypt: TButton;
    btnMLKEMDecrypt: TButton;
    mmoMLKEMCipher: TMemo;
    btnMLKEM2Ntt: TButton;
    btnMLKEMDotProduct: TButton;
    btnMLKEMEncap: TButton;
    cbbMLKEMType: TComboBox;
    tsMLDSA: TTabSheet;
    grpMLDSA: TGroupBox;
    btnMLDSAKeyGen: TButton;
    chkMLDSAUsePre: TCheckBox;
    cbbMLDSAType: TComboBox;
    mmoMLDSAKeys: TMemo;
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
    procedure btnMLKEMSamplePolyCBDClick(Sender: TObject);
    procedure btnMLKEMSampleNttClick(Sender: TObject);
    procedure btnMLKEMKeyGenClick(Sender: TObject);
    procedure btnMLKEMEncryptClick(Sender: TObject);
    procedure btnMLKEMDecryptClick(Sender: TObject);
    procedure btnMLKEM2NttClick(Sender: TObject);
    procedure btnMLKEMDotProductClick(Sender: TObject);
    procedure btnMLKEMEncapClick(Sender: TObject);
    procedure btnMLDSAKeyGenClick(Sender: TObject);
  private
    FPriv: TCnNTRUPrivateKey;
    FPub: TCnNTRUPublicKey;
    FMLKEMEn: TCnMLKEMEncapsulationKey;
    FMLKEMDe: TCnMLKEMDecapsulationKey;
    FMLKEM: TCnMLKEM;
    FMLKEMEnBytes, FMLKEMDeBytes: TBytes;
    FMLDSA: TCnMLDSA;
    FMLDSAPub: TCnMLDSAPublicKey;
    FMLDSAPriv: TCnMLDSAPrivateKey;
    FMLDSAPubBytes, FMLDSAPrivBytes: TBytes;
  public
    { Public declarations }
  end;

var
  FormLattice: TFormLattice;

implementation

{$R *.DFM}

uses
  CnBigNumber, CnVector, CnPolynomial, CnRandom, CnSHA3, CnBits;

const
  MLKEM_EK =
    'ADB031A018722F19C25829150A8B94297C9519173A2C908AEBF76F86C9724A0354300C6AB58A90C7' +
    'B18A2E828B5FD2CDEACA9810EC00FD8CBB22AA6AE641B5824055A91B27C1F73518953913FA2037D2' +
    '95F42B7E34AABE90D72A2453B215D886EBE28779D76F4830B01A73291B5A7186454A91D665B9E6C0' +
    '4FE808F76AC25E298B97195999F866DBFABD571B59549787F5E316A0C65F3E9132089B219E11712D' +
    '245AF955ACBFA36D37BB2DFD570ECF90941BD369E8023B2FF65F04D5408FA84531604A5EACCF3D62' +
    '221429A1D6B1BE5FEB04EBA0BB698B6B6AE4CF72317D96E99B0A840AB55C275B9979BB231926D714' +
    '0FC9B8C8124B87532E854113532A809C714D492746BEA7C30AA869DD780B77D0BADBBA9D9FE6A097' +
    '80B1EE7076F5E3CAF5F470D004CB53098BFA340248835EC782B34D612C53701259D2583360935D8B' +
    '5F25051BF196214D1CC6B33A8A47F41A65B5A143DB3651885214307B0D938F9D253672A4587EBC6D' +
    '9743A3AFB35A3098149B27C3EA0BAAC5942D9A92856EB0447D1709ED70A673AA1FEBA5398A821FA2' +
    '1B215FD78866C29B1E75C50815CC3F093B7CA70717A10CCF8C1880072230163008AA675A17759937' + 
    'CA672A56BBA772C527A5F9F12D53267419559A7A1A0A3003B64D34ABC8F4294210B140F517BEE86F' +
    '393BB577E6A41EB64A4EF633C8BA941314528B67A24F4829E99C96BF92022307A763A9086F7C5973' +
    '40C8FBBB0689DC1D28800BAD710422A9334A406B203101A6C97A3D00AC6C095735E22CBC60B3A076' +
    '0C8B0268FB5BB3D7D0543BAB599FD326608264EA477EE8F8C178898FE7387F15A6B40B13B069D3B5' +
    '74967F8487309495420F20B23456CF6D010AB19CABA9B27CAE79B09D7084D1863359B40F20555645' +
    '6AAD5F92A5A36B49B737866901CF6FB47B59A9C5783C2C5FD95087DA417F7410708290B619B70185' +
    '2B051272AE0428263C28CB025E1C1103B2718A0DDA5FC30394600879AF509B62C57B88A018FEDB09' +
    '642872591582ABB4AC1408A5750902589406E2479D3BC5AD8016610D7BA18D984EBD951393FC2B83' +
    '446D474AC8E2A634F66F181976B8C14F6078DE84113D8E728FAC101A23C86300D795CAE9875436CC';

  MLKEM_DK =
    'C1C9276205D0DAD6447DC94A5E2C36E754499B42595E75BFBFC15B8AA2615204A33EA62DA190398B' +
    '341384C1BDD7B9BC547A976594BFA20C20BA9754302A0FF17616C83A3997C9CBF6F7031D9A5977BC' +
    '58C1058FE15C3101981578B48B09DB07B035C220753C0766992C035FFF0836B3120B06D40200C648' + 
    '4DEA3618491FC446A2C95B94332512AEC61A8453B2251794A0929859E03925A31B0DABC9E9F88E30' + 
    'B2C364F9858CDA1D7A525F913B51BCDA41BB8635F9202253EA4EC1B7882E541A9A1411B660342122' + 
    '09D7F7B08B960832395280628742728B3BB0037AE07FABBC7CB89289DC9996B5894E203C5F24E77E' + 
    'F33B77AF513D3203B32F7258D663BA48A49B0249CC837AA35966C4CFB5774E96AB689613B6565C17' + 
    'DB0273268CFF2A2F391A018FD972BE165C1FA3A6B6587830EAB75D6B4FD3750EF7820727E9CC5D38' + 
    '69219A690FA58A966C80715AB23D7B042111733B8A3AD70674415277BF0C70F225B7BD775E639CB8' +
    '8FC06005CB135F752E02C66C0A197AFF56544EE13A0F8C2119C4B410168F16D779CD7590B77CADC1' + 
    '8269B1AB84F1C562FAA94342279BBFFA58961032F794562D288D2F9683F00C06CD5A7B9230027299' + 
    '47814811397487C4972579604288695DE054576F872501F02D98399A4369B636AAA84C04A9C87B1B' + 
    '3A3A7DE79B6ECBD49AAB705652CBABA6A1C91153277726203784BBDAE16703FC77F5AB273AB7376A' +
    '033A15F3CB19872AA113633291416A85A5ADF9BC164BA9E32C991D85CFFE9B391EC06055B7BAA5A5' + 
    '9053546EC6DA129896BE628820089B31A1ECB6AC91C5184428D4C582FA9A4841A6B66D500657A456' + 
    'D378BF6EBA41F9EB57CBE8227DE06751FB9AE058981A105604095E0F0943E79C715ABA8ADE8A2250' + 
    'D600BED71B42E7CCC676909CA78EF9823B60C14B4FE565C63237E9E10EE3761D5B677F9AD809E3E1' +
    '3301C601E1F36B5860C3828B9E7634C3ECC53541CBAA5460AC94270C530C617E9866097C20BFF5B8' + 
    '79F642BAD352A2E6254CA4184C1022DD8297CF2CBCDC6292FAF5BB9051226D489D8A5979F5C810DC' + 
    '9819210B309E3A71ADB031A018722F19C25829150A8B94297C9519173A2C908AEBF76F86C9724A03' + 
    '54300C6AB58A90C7B18A2E828B5FD2CDEACA9810EC00FD8CBB22AA6AE641B5824055A91B27C1F735' + 
    '18953913FA2037D295F42B7E34AABE90D72A2453B215D886EBE28779D76F4830B01A73291B5A7186' + 
    '454A91D665B9E6C04FE808F76AC25E298B97195999F866DBFABD571B59549787F5E316A0C65F3E91' + 
    '32089B219E11712D245AF955ACBFA36D37BB2DFD570ECF90941BD369E8023B2FF65F04D5408FA845' +
    '31604A5EACCF3D62221429A1D6B1BE5FEB04EBA0BB698B6B6AE4CF72317D96E99B0A840AB55C275B' +
    '9979BB231926D7140FC9B8C8124B87532E854113532A809C714D492746BEA7C30AA869DD780B77D0' + 
    'BADBBA9D9FE6A09780B1EE7076F5E3CAF5F470D004CB53098BFA340248835EC782B34D612C537012' + 
    '59D2583360935D8B5F25051BF196214D1CC6B33A8A47F41A65B5A143DB3651885214307B0D938F9D' + 
    '253672A4587EBC6D9743A3AFB35A3098149B27C3EA0BAAC5942D9A92856EB0447D1709ED70A673AA' +
    '1FEBA5398A821FA21B215FD78866C29B1E75C50815CC3F093B7CA70717A10CCF8C18800722301630' +
    '08AA675A17759937CA672A56BBA772C527A5F9F12D53267419559A7A1A0A3003B64D34ABC8F42942' + 
    '10B140F517BEE86F393BB577E6A41EB64A4EF633C8BA941314528B67A24F4829E99C96BF92022307' +
    'A763A9086F7C597340C8FBBB0689DC1D28800BAD710422A9334A406B203101A6C97A3D00AC6C0957' +
    '35E22CBC60B3A0760C8B0268FB5BB3D7D0543BAB599FD326608264EA477EE8F8C178898FE7387F15' + 
    'A6B40B13B069D3B574967F8487309495420F20B23456CF6D010AB19CABA9B27CAE79B09D7084D186' +
    '3359B40F205556456AAD5F92A5A36B49B737866901CF6FB47B59A9C5783C2C5FD95087DA417F7410' +
    '708290B619B701852B051272AE0428263C28CB025E1C1103B2718A0DDA5FC30394600879AF509B62' + 
    'C57B88A018FEDB09642872591582ABB4AC1408A5750902589406E2479D3BC5AD8016610D7BA18D98' +
    '4EBD951393FC2B83446D474AC8E2A634F66F181976B8C14F6078DE84113D8E728FAC101A23C86300' +
    'D795CAE9875436CC17073137259AE58DA42855C429CB69DE93930C35FA796F80C774DC812AD0B9F6' +
    'E2FC8C99012BB954AB3FDCBD6F6522CF7A0A59C511B330C6B72C0493BA70AA3C';


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
  cbbMLKEMType.ItemIndex := 0;
  cbbMLDSAType.ItemIndex := 0;

  FPriv := TCnNTRUPrivateKey.Create;
  FPub := TCnNTRUPublicKey.Create;

  for Pt := Low(TCnNTRUParamType) to High(TCnNTRUParamType) do
    cbbNTRUType.Items.Add(GetEnumName(TypeInfo(TCnNTRUParamType), Ord(Pt)));
end;

procedure TFormLattice.FormDestroy(Sender: TObject);
begin
  FMLKEMEn.Free;
  FMLKEMDe.Free;
  FMLKEM.Free;

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

type
  // Barrett Reduction 所需的参数结构体
  TMLKEMBarrettReduce = packed record
    MU: Cardinal;    // Floror(2^k / q) 的近似值 (通常四舍五入)
    K: Integer;      // 幂次 k，用于计算 2^k
    HalfQ: Word;     // q/2 的上取整或下取整，用于中心化约减
    D: Integer;      // 此表项对应的压缩参数 d (如 du 或 dv)
  end;

const
  // ML-KEM Barrett Reduction 查找表，包含不同参数集和操作（压缩、解压）所需的约减参数
  MLKEM_BARRETT_TABLE: array[0..4] of TMLKEMBarrettReduce = (
    (MU: 80635;   K: 28; HalfQ: 1665; D: 1),     // round(2^28/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  1 is mlkem512 du
    (MU: 1290167; K: 32; HalfQ: 1665; D: 10),    // round(2^32/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  10 is mlkem768 du
    (MU: 80635;   K: 28; HalfQ: 1665; D: 4),     // round(2^28/MLKEM_Q), ?, Ceil(MLKEM_Q/2),  4 is mlkem768 dv
    (MU: 40318;   K: 27; HalfQ: 1664; D: 5),     // round(2^27/MLKEM_Q), ?, Floor(MLKEM_Q/2), 5 is mlkem1024 dv
    (MU: 645084;  K: 31; HalfQ: 1664; D: 11)     // round(2^31/MLKEM_Q), ?, Floor(MLKEM_Q/2), 11 is mlkem1024 du
  );

  // FIPS 203 Appendix A 的 NTT 预计算值
  ZETA_NTT: array[0..127] of Word = (
    1, 1729, 2580, 3289, 2642, 630, 1897, 848,
    1062, 1919, 193, 797, 2786, 3260, 569, 1746,
    296, 2447, 1339, 1476, 3046, 56, 2240, 1333,
    1426, 2094, 535, 2882, 2393, 2879, 1974, 821,
    289, 331, 3253, 1756, 1197, 2304, 2277, 2055,
    650, 1977, 2513, 632, 2865, 33, 1320, 1915,
    2319, 1435, 807, 452, 1438, 2868, 1534, 2402,
    2647, 2617, 1481, 648, 2474, 3110, 1227, 910,
    17, 2761, 583, 2649, 1637, 723, 2288, 1100,
    1409, 2662, 3281, 233, 756, 2156, 3015, 3050,
    1703, 1651, 2789, 1789, 1847, 952, 1461, 2687,
    939, 2308, 2437, 2388, 733, 2337, 268, 641,
    1584, 2298, 2037, 3220, 375, 2549, 2090, 1645,
    1063, 319, 2773, 757, 2099, 561, 2466, 2594,
    2804, 1092, 403, 1026, 1143, 2150, 2775, 886,
    1722, 1212, 1874, 1029, 2110, 2935, 885, 2154
  );

  ZETA_BASE_CASE: array[0..127] of Word = (
    17, 3312, 2761, 568, 583, 2746, 2649, 680,
    1637, 1692, 723, 2606, 2288, 1041, 1100, 2229,
    1409, 1920, 2662, 667, 3281, 48, 233, 3096,
    756, 2573, 2156, 1173, 3015, 314, 3050, 279,
    1703, 1626, 1651, 1678, 2789, 540, 1789, 1540,
    1847, 1482, 952, 2377, 1461, 1868, 2687, 642,
    939, 2390, 2308, 1021, 2437, 892, 2388, 941,
    733, 2596, 2337, 992, 268, 3061, 641, 2688,
    1584, 1745, 2298, 1031, 2037, 1292, 3220, 109, 
    375, 2954, 2549, 780, 2090, 1239, 1645, 1684,
    1063, 2266, 319, 3010, 2773, 556, 757, 2572,
    2099, 1230, 561, 2768, 2466, 863, 2594, 735,
    2804, 525, 1092, 2237, 403, 2926, 1026, 2303,
    1143, 2186, 2150, 1179, 2775, 554, 886, 2443,
    1722, 1607, 1212, 2117, 1874, 1455, 1029,2300,
    2110, 1219, 2935, 394, 885, 2444, 2154, 1175
  );

function DivMlKemQ(X: Word; B, HQ, BS: Integer; BM: TUInt64): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  R: TUInt64;
begin
  R := (TUInt64(X) shl B) + TUInt64(HQ);
  R := UInt64Mul(R, BM);
  R := R shr BS;
  Result := Word(R and ((1 shl B) - 1));
end;

// 模素数加减乘法
function ModAdd(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A + B) mod CN_MLKEM_PRIME;
end;

function ModSub(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if A >= B then
    Result := A - B
  else
    Result := CN_MLKEM_PRIME + A - B;

  Result := Result mod CN_MLKEM_PRIME;
end;

function ModMul(A, B: Word): Word; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Word(Cardinal(A) * Cardinal(B) mod CN_MLKEM_PRIME);
end;

// 将一个 X 的系数值压缩到 D 位并返回
function Compress(X, D: Word): Word;
var
  V, T: Word;
  I: Integer;
begin
  V := 0;
  T := (X + CN_MLKEM_PRIME) mod CN_MLKEM_PRIME;

  for I := Low(MLKEM_BARRETT_TABLE) to High(MLKEM_BARRETT_TABLE) do
  begin
    if D = MLKEM_BARRETT_TABLE[I].D then
    begin
      V := DivMlKemQ(T, MLKEM_BARRETT_TABLE[I].D, MLKEM_BARRETT_TABLE[I].HalfQ,
        MLKEM_BARRETT_TABLE[I].K, MLKEM_BARRETT_TABLE[I].MU);
      Break;
    end;
  end;

  Result := V;
end;

// 将一个压缩后的系数值解压并返回
function Decompress(X: Word; D: Word): Word;
var
  P: Cardinal;
begin
  P := Cardinal(X) * CN_MLKEM_PRIME;

  Result := Word((P shr D) +                // 商（主干部分）
    ((P and ((1 shl D) - 1)) shr (D - 1))); // 四舍五入的进位项
end;

// 根据真随机数组生成 256 个采样的多项式系数供 NTT 变换用，RandBytes 的长度至少要 34 字节
function SampleNTT(const RandBytes: TBytes): TWords;
var
  Ctx: TCnSHA3Context;
  C: TBytes;
  D1, D2: Integer;
  J: Integer;
begin
  if Length(RandBytes) < CN_MLKEM_KEY_SIZE + 2 then
    raise Exception.Create('Invalid SampleNTT Random Bytes');

  SetLength(Result, CN_MLKEM_POLY_SIZE);

  SHAKE128Init(Ctx, 0);
  SHAKE128Absorb(Ctx, PAnsiChar(@RandBytes[0]), Length(RandBytes));

  J := 0;
  while J < CN_MLKEM_POLY_SIZE do
  begin
    C := SHAKE128Squeeze(Ctx, 3);

    // 从 3 字节中提取两个 12 位数值
    D1 := C[0] + 256 * (C[1] and $0F);       // 使用 C[1] 的低 4 位
    D2 := (C[1] shr 4) + 16 * C[2];          // 使用 C[1] 的高 4 位

    // 检查第一个值是否有效
    if D1 < CN_MLKEM_PRIME then
    begin
      Result[J] := D1;
      Inc(J);

      // 如果已经收集够 256 个值，提前退出
      if J >= CN_MLKEM_POLY_SIZE then
        Break;
    end;

    // 检查第二个值是否有效
    if (D2 < CN_MLKEM_PRIME) and (J < CN_MLKEM_POLY_SIZE) then
    begin
      Result[J] := D2;
      Inc(J);
    end;
  end;
end;

// 根据真随机数组生成 256 个采样的多项式系数，RandBytes 的长度至少要 64 * Eta 字节
function SamplePolyCBD(const RandBytes: TBytes; Eta: Integer): TWords;
var
  I, J, X, Y: Integer;
  Bits: TCnBitBuilder;

  function BitToInt(Bit: Boolean): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  begin
    if Bit then
      Result := 1
    else
      Result := 0;
  end;

begin
  if Length(RandBytes) < 64 * Eta then
    raise Exception.Create('Invalid Random Length');

  SetLength(Result, CN_MLKEM_POLY_SIZE);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendBytes(RandBytes);

    for I := 0 to CN_MLKEM_POLY_SIZE - 1 do
    begin
      X := 0;
      Y := 0;

      for J := 0 to Eta - 1 do
        X := X + BitToInt(Bits[2 * I * Eta + J]);
      for J := 0 to Eta - 1 do
        Y := Y + BitToInt(Bits[2 * I * Eta + Eta + J]);

      if X >= Y then
        Result[I] := X - Y
      else
        Result[I] := CN_MLKEM_PRIME + X - Y;
    end;
  finally
    Bits.Free;
  end;
end;

procedure TFormLattice.btnCompressTestClick(Sender: TObject);
var
  I, D: Integer;
  V, R: Word;
begin
  D := StrToIntDef(edtMLKEMD.Text, 11);
  mmoMLKEM.Lines.Clear;
  for I := 0 to CN_MLKEM_PRIME - 1 do
  begin
    V := Compress(Word(I), D);
    R := Decompress(V, D);
    if I <> R then
    begin
      if Abs(I - R) > 1 then
        mmoMLKEM.Lines.Add(Format('*** %d -> %d -> %d', [I, V, R]))
      else
        mmoMLKEM.Lines.Add(Format('* %d -> %d -> %d', [I, V, R]));
    end
    else
      mmoMLKEM.Lines.Add(Format('  %d -> %d -> %d', [I, V, R]));
  end;
end;

procedure TFormLattice.btnDeCompressTestClick(Sender: TObject);
var
  I, D: Integer;
  V, R: Word;
begin
  // 2^d 以下应该都相等
  D := StrToIntDef(edtMLKEMD.Text, 11);
  mmoMLKEM.Lines.Clear;
  for I := 0 to CN_MLKEM_PRIME - 1 do
  begin
    V := Decompress(Word(I), D);
    R := Compress(V, D);
    if I <> R then
      mmoMLKEM.Lines.Add(Format('*** %d -> %d -> %d', [I, V, R]))
    else
      mmoMLKEM.Lines.Add(Format('%d -> %d -> %d', [I, V, R]));
  end;
end;

procedure TFormLattice.btnMLKEMSamplePolyCBDClick(Sender: TObject);
var
  B: TBytes;
  I, Eta: Integer;
  W: TWords;
begin
  B := CnRandomBytes(256);
  Eta := StrToIntDef(edtSamleEta.Text, 2);

  W := SamplePolyCBD(B, Eta);
  mmoMLKEM.Lines.Clear;
  for I := Low(W) to High(W) do
    mmoMLKEM.Lines.Add(IntToStr(W[I]));
end;

procedure TFormLattice.btnMLKEMSampleNttClick(Sender: TObject);
var
  I: Integer;
  B: TBytes;
  W: TWords;
begin
  B := CnRandomBytes(34);
  W := SampleNTT(B);

  mmoMLKEM.Lines.Clear;
  for I := Low(W) to High(W) do
    mmoMLKEM.Lines.Add(IntToStr(W[I]));
end;

procedure TFormLattice.btnMLKEMKeyGenClick(Sender: TObject);
begin
  FreeAndNil(FMLKEMEn);
  FreeAndNil(FMLKEMDe);
  FreeAndNil(FMLKEM);

  FMLKEMEn := TCnMLKEMEncapsulationKey.Create;
  FMLKEMDe := TCnMLKEMDecapsulationKey.Create;
  FMLKEM := TCnMLKEM.Create(TCnMLKEMType(cbbMLKEMType.ItemIndex));

  if chkMLKEMUsePre.Checked then
  begin
    case FMLKEM.MLKEMType of
      cmkt512:
        FMLKEM.GenerateKeys(FMLKEMEn, FMLKEMDe, 'BBA3C0F5DF044CDF4D9CAA53CA15FDE26F34EB3541555CFC54CA9C31B964D0C8',
          '0A64FDD51A8D91B3166C4958A94EFC3166A4F5DF680980B878DB8371B7624C96');
      cmkt768:
        FMLKEM.GenerateKeys(FMLKEMEn, FMLKEMDe, '6DBB99AE6889AF01DA387D7D99BD4E91BACB11A6051B14AECD4C96F30CD9F9D9',
          '360557CADDFCF5FEE7C0DE6A363F095757588C35A3FD11C58677AB5E8797C2B8');
      cmkt1024:
        FMLKEM.GenerateKeys(FMLKEMEn, FMLKEMDe, '2B5330C4F23BFDFD5C31F050BA3B38235324BF032372FC12D04DD08920F0BD59',
          '0A064D6C06CEAB73E59CFCA9FF6402255A326AEF1E9CB678BF36929DAFE29A58');
    end;
  end
  else
    FMLKEM.GenerateKeys(FMLKEMEn, FMLKEMDe);

  FMLKEMEnBytes := FMLKEM.SaveEncapKeyToBytes(FMLKEMEn);
  FMLKEMDeBytes := FMLKEM.SaveKeysToBytes(FMLKEMDe, FMLKEMEn);

  mmoMLKEMKeys.Lines.Clear;
  mmoMLKEMKeys.Lines.Add(BytesToHex(FMLKEMEnBytes));
  mmoMLKEMKeys.Lines.Add(BytesToHex(FMLKEMDeBytes));

  FMLKEM.LoadKeyFromBytes(FMLKEMEnBytes, FMLKEMEn);
  FMLKEM.LoadKeysFromBytes(FMLKEMDeBytes, FMLKEMDe, FMLKEMEn);
end;

procedure TFormLattice.btnMLKEMEncryptClick(Sender: TObject);
var
  M, C: TBytes;
begin
  if FMLKEM = nil then
  begin
    FMLKEM := TCnMLKEM.Create(cmkt512);
    FMLKEMEnBytes := HexToBytes(MLKEM_EK);
    FMLKEMDeBytes := HexToBytes(MLKEM_DK);
  end;

  FMLKEM.LoadKeyFromBytes(FMLKEMEnBytes, FMLKEMEn);
  FMLKEM.LoadKeysFromBytes(FMLKEMDeBytes, FMLKEMDe, FMLKEMEn);

  M := AnsiToBytes(edtMLKEMMsg.Text);
  if chkMLKEMUsePre.Checked then
    C := FMLKEM.MLKEMEncrypt(FMLKEMEnBytes, M, 'BBA3C0F5DF044CDF4D9CAA53CA15FDE26F34EB3541555CFC54CA9C31B964D0C8')
  else
    C := FMLKEM.MLKEMEncrypt(FMLKEMEnBytes, M, '');
  mmoMLKEMCipher.Lines.Clear;
  mmoMLKEMCipher.Lines.Add(BytesToHex(C));
end;

procedure TFormLattice.btnMLKEMDecryptClick(Sender: TObject);
var
  M, C: TBytes;
  S: string;
begin
  if FMLKEM = nil then
  begin
    ShowMessage('Please Gen Key First');
    Exit;
  end;

  FMLKEM.LoadKeyFromBytes(FMLKEMEnBytes, FMLKEMEn);
  FMLKEM.LoadKeysFromBytes(FMLKEMDeBytes, FMLKEMDe, FMLKEMEn);

  S := mmoMLKEMCipher.Lines.Text;
  S := StringReplace(S, #13, '', [rfReplaceAll]);
  S := StringReplace(S, #10, '', [rfReplaceAll]);

  C := HexToBytes(S);
  M := FMLKEM.MLKEMDecrypt(FMLKEMDeBytes, C);
  ShowMessage(BytesToAnsi(M));
end;

procedure TFormLattice.btnMLKEM2NttClick(Sender: TObject);
var
  Poly, PolyNTT, PolyRec: TCnMLKEMPolynomial;
  I: Integer;
  B: Boolean;
begin
  // 创建一个测试多项式
  for I := 0 to CN_MLKEM_POLY_SIZE - 1 do
    Poly[I] := I * 5 mod CN_MLKEM_PRIME;

  MLKEMPolynomialToNTT(PolyNTT, Poly);
  MLKEMPolynomialToINTT(PolyRec, PolyNTT);
  
  // 检查是否恢复原始多项式
  B := True;
  for I := 0 to CN_MLKEM_POLY_SIZE - 1 do
  begin
    if Poly[I] <> PolyRec[I] then
    begin
      B := False;
      Break;
    end;
  end;

  if B then
    ShowMessage('1 Poly Test OK')
  else
    ShowMessage('1 Poly Test Fail');
end;

procedure TFormLattice.btnMLKEMDotProductClick(Sender: TObject);
const
  M_D = 255; // 两个如果都是 255 次多项式就曾经有问题，后来修复了
var
  R, P1, P2, P: TCnInt64Polynomial;
  M1, M2, M: TCnMLKEMPolynomial;
begin
  P := TCnInt64Polynomial.Create;
  P1 := TCnInt64Polynomial.Create;
  P2 := TCnInt64Polynomial.Create;
  R := TCnInt64Polynomial.Create;

  R.MaxDegree := 256;
  R[256] := 1;
  R[0] := 1;

  P1.MaxDegree := M_D;
  P2.MaxDegree := M_D;
  P1[0] := 1;
  P1[M_D] := 65;
  P2[0] := 34431;
  P2[M_D] := 1;

  Int64PolynomialMul(P, P1, P2);
  Int64PolynomialNonNegativeModWord(P, 3329);
  Int64PolynomialMod(P, P, R);
  Int64PolynomialNonNegativeModWord(P, 3329);

  mmoMLKEM.Lines.Add(P.ToString);

  Int64PolynomialToMLKEMPolynomial(P1, M1);
  Int64PolynomialToMLKEMPolynomial(P2, M2);
  MLKEMPolynomialToNTT(M1, M1);
  MLKEMPolynomialToNTT(M2, M2);
  MLKEMPolynomialMul(M, M1, M2);
  MLKEMPolynomialToINTT(M, M);

  MLKEMPolynomialToInt64Polynomial(M, P);
  mmoMLKEM.Lines.Add(P.ToString);

  // x^255+1 的 NTT
  M1[0] := 1; M[255] := 1;
  MLKEMPolynomialToNTT(M1, M1);

  // 其正确结果 2X^255+3328X^254+1 的 NTT
  M2[0] := 1; M2[254] := 3328; M2[255] := 2;
  MLKEMPolynomialToNTT(M2, M2);

  // 我们的 NTT 计算的结果的 NTT

  R.Free;
  P2.Free;
  P1.Free;
  P.Free;
end;

procedure TFormLattice.btnMLKEMEncapClick(Sender: TObject);
var
  M, S, C, S1: TBytes;
begin
  if FMLKEM = nil then
  begin
    FMLKEM := TCnMLKEM.Create(cmkt512);
    FMLKEMEn := TCnMLKEMEncapsulationKey.Create;
    FMLKEMDe := TCnMLKEMDecapsulationKey.Create;

    FMLKEMEnBytes := HexToBytes(MLKEM_EK);
    FMLKEMDeBytes := HexToBytes(MLKEM_DK);
  end;

  FMLKEM.LoadKeyFromBytes(FMLKEMEnBytes, FMLKEMEn);
  FMLKEM.LoadKeysFromBytes(FMLKEMDeBytes, FMLKEMDe, FMLKEMEn);

  M := HexToBytes('E8D6BAC09B25469BEE582A7DEE9BD21890CD3F0AF4D7E19E30B3E24E657C149C');
  FMLKEM.MLKEMEncaps(FMLKEMEnBytes, M, S, C);

  if FMLKEM.MLKEMType = cmkt512 then
  begin
    if BytesToHex(S) = '6C832560DFE97BECADFBB340EE31AE868A73120806EED02839518E5627D32968' then
      ShowMessage('Encap OK')
    else
      ShowMessage('Encap Fail');
  end;

  S1 := FMLKEM.MLKEMDecaps(FMLKEMDeBytes, C);
  if CompareBytes(S1, S) then
    ShowMessage('Decap Test OK. Share Key: ' + BytesToHex(S))
  else
    ShowMessage('Decap Test Fail');
end;

procedure TFormLattice.btnMLDSAKeyGenClick(Sender: TObject);
begin
  FreeAndNil(FMLDSAPub);
  FreeAndNil(FMLDSAPriv);
  FreeAndNil(FMLDSA);

  FMLDSAPub := TCnMLDSAPublicKey.Create;
  FMLDSAPriv := TCnMLDSAPrivateKey.Create;
  FMLDSA := TCnMLDSA.Create(TCnMLDSAType(cbbMLDSAType.ItemIndex));

//  if chkMLDSAUsePre.Checked then
//  begin
//    case FMLDSA.MLDSAType of
//      cmkt512:
//        FMLDSA.GenerateKeys(FMLDSAPub, FMLDSAPriv, 'BBA3C0F5DF044CDF4D9CAA53CA15FDE26F34EB3541555CFC54CA9C31B964D0C8',
//          '0A64FDD51A8D91B3166C4958A94EFC3166A4F5DF680980B878DB8371B7624C96');
//      cmkt768:
//        FMLDSA.GenerateKeys(FMLDSAPub, FMLDSAPriv, '6DBB99AE6889AF01DA387D7D99BD4E91BACB11A6051B14AECD4C96F30CD9F9D9',
//          '360557CADDFCF5FEE7C0DE6A363F095757588C35A3FD11C58677AB5E8797C2B8');
//      cmkt1024:
//        FMLDSA.GenerateKeys(FMLDSAPub, FMLDSAPriv, '2B5330C4F23BFDFD5C31F050BA3B38235324BF032372FC12D04DD08920F0BD59',
//          '0A064D6C06CEAB73E59CFCA9FF6402255A326AEF1E9CB678BF36929DAFE29A58');
//    end;
//  end
//  else
    FMLDSA.GenerateKeys(FMLDSAPriv, FMLDSAPub);

  FMLDSAPubBytes := FMLDSA.SavePublicKeyToBytes(FMLDSAPub);
  FMLDSAPrivBytes := FMLDSA.SavePrivateKeyToBytes(FMLDSAPriv);

  mmoMLDSAKeys.Lines.Clear;
  mmoMLDSAKeys.Lines.Add(BytesToHex(FMLDSAPubBytes));
  mmoMLDSAKeys.Lines.Add(BytesToHex(FMLDSAPrivBytes));

  FMLDSA.LoadPublicKeyFromBytes(FMLDSAPub, FMLDSAPubBytes);
  FMLDSA.LoadPrivateKeyFromBytes(FMLDSAPriv, FMLDSAPrivBytes);
end;

end.
