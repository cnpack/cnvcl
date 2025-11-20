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
    btnMLDSALoadKey: TButton;
    btnMLDSATestNTT: TButton;
    btnMLDSATestSign: TButton;
    btnMLDSATestSign2: TButton;
    btnMLDSATestVerify: TButton;
    btnMLDSATestSignVer: TButton;
    btnMLDSATestSign65: TButton;
    btnMLDSATest65KeyGen: TButton;
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
    procedure btnMLDSALoadKeyClick(Sender: TObject);
    procedure btnMLDSATestNTTClick(Sender: TObject);
    procedure btnMLDSATestSignClick(Sender: TObject);
    procedure btnMLDSATestSign2Click(Sender: TObject);
    procedure btnMLDSATestVerifyClick(Sender: TObject);
    procedure btnMLDSATestSignVerClick(Sender: TObject);
    procedure btnMLDSATestSign65Click(Sender: TObject);
    procedure btnMLDSATest65KeyGenClick(Sender: TObject);
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

  if chkMLDSAUsePre.Checked then
  begin
    case FMLDSA.MLDSAType of
      cmdt44:
        FMLDSA.GenerateKeys(FMLDSAPriv, FMLDSAPub, '885B7DF7CF6695F30AA3F1BC6A3840B8CA3101734118AE619166838AA3EFDBCD');
      cmdt65:
        FMLDSA.GenerateKeys(FMLDSAPriv, FMLDSAPub, '72C3C5E0CC9F332F49D0FC0FD6399DA75645A3E33DBF56F1E96897662D0A9B37');
      cmdt87:
        FMLDSA.GenerateKeys(FMLDSAPriv, FMLDSAPub, '6DDC6B90E85615F0B14B4404DF3980684561530D0836B13E83E3D0FCB6BAE3A7');
    end;
  end
  else
    FMLDSA.GenerateKeys(FMLDSAPriv, FMLDSAPub);

  FMLDSAPubBytes := FMLDSA.SavePublicKeyToBytes(FMLDSAPub);
  FMLDSAPrivBytes := FMLDSA.SavePrivateKeyToBytes(FMLDSAPriv);

  mmoMLDSAKeys.Lines.Clear;
  mmoMLDSAKeys.Lines.Add(BytesToHex(FMLDSAPubBytes));
  mmoMLDSAKeys.Lines.Add(BytesToHex(FMLDSAPrivBytes));

  FMLDSA.LoadPublicKeyFromBytes(FMLDSAPub, FMLDSAPubBytes);
  FMLDSA.LoadPrivateKeyFromBytes(FMLDSAPriv, FMLDSAPrivBytes);
end;

procedure TFormLattice.btnMLDSALoadKeyClick(Sender: TObject);
const
  MLDSA_PK =
    'B845FA2881407A59183071629B08223128116014FB58FF6BB4C8C9FE19CF5B0BD77B16648A344FFE' +
    '486BC3E3CB5FAB9ABC4CC2F1C34901692BEC5D290D815A6CDF7E9710A3388247A7E0371615507A57' +
    '2C9835E6737BF30B92A796FFF3A10A730C7B550924EB1FB6D56195F02DE6D3746F9F330BEBE990C9' +
    '0C4D676AD415F4268D2D6B548A8BCDF27FDD467E6749C0F87B71E85C2797694772BBA88D4F1AC06C' +
    '7C0E91786472CD76353708D6BBC5C28E9DB891C3940E879052D30C8FD10965CBB8EE1BD79B060D37' +
    'FB839098552AABDD3A57AB1C6A82B0911D1CF148654AA5613B07014B21E4A1182B4A5501671D112F' +
    '5975FB0C8A2AC45D575DC42F48977FF37FFF421DB27C45E79F8A9472007023DF0B64205CD9F57C02' +
    'CE9D1F61F2AE24F7139F5641984EE8DF783B9EA43E997C6E19D09E062AFCA56E4F76AAAB8F66600F' +
    'C78F6AB4F6785690D185816EE35A939458B60324EEFC60E64B11FA0D20317ACB6CB29AA03C775F15' +
    '1672952689FA4F8F838329CB9E6DC9945B6C7ADE4E7B663578F87D3935F2A1522097AD5042A0D990' +
    'A628510B6103CB242CD8A3AFC1A5ADA52331F4DF461BC1DA51D1D224094E7ABED3D87D98F0D81708' +
    '4780EE80370F397631ECB75D4264B6B5E2E66C0586B5FB743516399165837A0FDFF7C6134F033BFA' +
    '69C1B2416965C6E578592F40E258CB6DFB29FB8E0F54355B6E24A65F67ABAE3193D007115CC0B9FF' +
    '94CB911A93B1A76C0E7662F5E2B20139E0159ED929CB932D4895F89A02E55C59DF2DBB8F6E5DD7D5' +
    'B1F3CEC37B4A9166B381C5440E23E67368CDE0A29D59AA05A3C9BE24A4DC8DD75BE30E82BC635D36' +
    'AAC66DE880C6701A987D7E05F0F2FF287828BEC30595089D8AB9AA390ED719CAA6E576CDBBE9B184' +
    'A322E5E2DABB69C23CC696D54FC32FF57001B6B64E2A837F3062D85AEB50B3510F7EDFC34DF38E08' +
    '3D4D9B94FFAB0DE15D73D9AF30B9F31CC4F41C9C24F2D618B2A7C3C4BDFB745D52D3EB54589C8BDA' +
    '8AC05DAD14EC744505575A0988EEC651C1715439FDFB29923380A43C1A66A86C982A841F11820A6A' +
    '0E1E2F2FFF5108ECAE51A6AABC9B949226D228FF84C4E5E5D63114D80359C4931E612DCED1838B7D' +
    '066AC9182CECFA223A21A4C8E155AEFA780373BCC15098AEE40C033AF22F8E7C67A0D2526DA7475E' +
    '830308C04AED9D32BCCC72E719EE70A8D13F09AC11E26EA237D5CC8F98B5AE0E54F933BD0507942E' +
    'D900D056FD32F8E6E81777912FD482746029B71CCE3BA69B8FC2D03EB441027C387BC2F95031A0AE' +
    '7052215EB24B9EA8FB0A961B0F80BFA80D0D6257C1C22B508C5D31B97FCDFE1D1766E8A9C8771932' +
    'DD598ADB7E717743F45FC571F21E4A516249F81D747F15329790F0F70A0B8E461A4EDF50504AF03F' +
    '30DDF8A8818E38761E1681D6DDEF0B1DD326B2EC228CE48570F285B49D29D7C2EF37866D5446DF82' +
    'B8E43B34CB248962A21A9A3946159740F8AEE8E6A16A4EB2B42D143FE2612E05EF4B5E646D813248' +
    '444556A2A8BF92CE10BADECB6B8A40B080DD42D53346FEFCC4B9B40B1E4998991EC753C95AA2F2A5' +
    '06F311E710B0F1D36C1DCA6644EE6D1D4AE9CEA5666EF4B3E888DBDBB95A77ECFE1E8B477DE7CB07' +
    '639D682D53020EC14EA6C7DD7E715389D10938429FAB8A068A1466A4CD891359F8074E0F5A142ADD' +
    '731B87878D985E4FA6ECB3B73D298553418273E9503AA84092C080E5F2902F90F5C59944D24CA027' +
    '1D11D0D6734606D039550A37FCA2B735850E63F540F2F06B79144B5C4ED2C700BB51C33D265B3D03' +
    '7389C99EFD597642D829DB1EB58643CFCD07F4DEC60B8F727D97BD7C4B59BDA1';
  MLDSA_SK =
    'B845FA2881407A59183071629B08223128116014FB58FF6BB4C8C9FE19CF5B0B28965F58D99EE0DE' +
    '7BFB7840F59F65414289E259E05E8A18D47EC06D7900284AD7A0AE404B93B0498945ABD07638920F' +
    'C7F8C21282031EECEB0CC48D71FB0AD1CE84B9B3DDD1B3A2A40A15D5BFE0BE23FC4757137C6C8FEB' +
    '7F8055677BA4AF2814242A9AA0108A4046981800042130A2340A99140E01A931E1300A1A13695948' +
    '6601C111140451244206E4C848A1B80008B2490121901224500B03306112025C144912B30049424D' +
    '14470E93342023898803238CA1C081CC0411184545C1C20862465210300D52B870A3B004DCA20D4A' +
    '386511214A01B48C43104809461253068161348618A00920878CA0168682208E41188E52A2902101' +
    '4652184521A42902132ED0B8248816710AB245818480942670DC126C1BC16C401624623600899610' +
    '8B124E1B3721639225E1C88CA346285A32111C856110B9295990910493651CC40063B810C1184A52' +
    '900C1CB20D2380090A4021D4188043A64D0835060C98650CB4890C8661D804601C0789D09231D808' +
    '1100881119294E48B661D8962DD2B04121A1051226860349301A246D99483001200C5CC284012890' +
    '0A298850340E1804640AB409D1968413196DD91031133689808025E300294A06304448109B429008' +
    '858902A20D13461110120EDC380C4C468842C004131490C1462248B82D48C80D64346C0234810913' +
    '6842A4211B26511C302A6328688B18609C922C22C9000031058AC08C894430D0142189424C4C1064' +
    '00B64DD2A8246306308BB4289B246EC34262A0C00C0C2050E112444A360AD122320C36014836804C' +
    '00400C16090C9569A19829110602CC982D08C811498445D20290014942C1062E42C000A2902D9810' +
    '711A054008058608B2651B36441109402107100B3306D298858148088034305C3844DC266ECA9470' +
    '04394A442841E1464898862D113621221509530826213088DA94804A80601425321295641B02120A' +
    '92241A4422230624CC12818922600036608A32820CC368A1A04D5B286A110744589851C42831A406' +
    '4EC0C831CAB46810B669132488840462A3C421E0920523303050B6105942285C3005102286CC260E' +
    '2226450003928A242C9C024E40B20400148C4A086E4BA6444CC84924464D4946821AA4690A316E21' +
    '408C22964949044618B72C12B864D1C851C3366E5C2008E3328513198564C2019A4822111300A4B6' +
    '4960C06D1944628C086119B761021570EA732640984F68143D81AB95763698DCD02625A980994C19' +
    'B7D47626409370F87E45D39B76B6762F7827B2779BA44046F54228A7B1BAC9D8E751AC2DD2485215' +
    '64F81D028AD671DB6A3A5BA3A9463597A519AE0C1DA435FEF7C421CDE7F8483782C4334993192923' +
    '796DDC3E5803F6C86CF2B37E39BBCB53775E5E9D2BDC65E84395B01AF95C4398503A7E63A8D72F7E' +
    '9BC58D7655CB86BAFA0B679D1D791F09703F6E4B333A51DFB14B36EAC9A3582F795AFCECC2A67A42' +
    'C7BC43B447B73EC8B73C9164B2D9EF4F767D607E975738E1D871FE61B7BB9570516959B9E7D81554' +
    '55FC91590D84BD430921568B718241CCD09C989DDF75F529C4F15818969216EC0FBABEC64F184832' +
    'BFE428BE44849C42F13CA84A5CE6CA628044CB9DA3FD9CB5EBC776BA0E12683B98253B213B286593' +
    '779EACC489C985BEB734A7506791BE1A6E6E03B557CFEF2069091B846CDBBB72D36A89FD5C45C7C7' +
    'FBFAED956EC9D8F3025225D6FEF811B1CC3C6705F056388BBFBBA1FEBF4D072281DC792ADA048216' +
    'D60D0D6044150909B70A4753336F4D5D3FDB8BACA62946DB28332EC778A76F96577BAF868BF89677' +
    '3C200637495098B0F7749835915348D37641EC74D5A78E565D8100627468B0B5D05C2937618A98E5' +
    'A12476533CE9D699E0F4F23B3127EE1F8D3CDE3851A94BEB8D606D836D47A9F090581B660FF8EC31' +
    'E3EF4E45DB530DD88A44D48F84B6BD3CED53BE935EFBCE9930DD83BD004F086A064B3260B7211BA0' +
    '547CB29E56603B922597B31A30A5FFCBD8A7AD2F3F05372DCEA89048C683164410E56A6E5B7EC2EA' +
    '6E3B0176DB1EBF9258C97C297E752A202B1B4B02DA47235873CC3E4A376B655779AFFFC9AEF2E30B' +
    '62BE022AEFF5E34B468507C921A00F1110B2CC1BC99B2EE295A31EAC141AB4D87EDC11D7FEE22A07' +
    'D69B7008C107EBE47DD2F215D074868C8B929A3DF6614B5E95CC8A52F96C57E1697CEE332F45B198' +
    '0E6CA372574BDB51825654E6FF31D8E755CFC15B602F46A0570D3BD0D65E43BDE8F792E7D526B3D3' +
    '62FE33638B3870E1C29E1030D3DBA43C3D0DB6E5F501EA3441B709E54FCA1FD3E8654B1A9301AD15' +
    '1A36C8FA1513D0229DE6E94D02A97B9D6F2EE8078A5D1228E783B8FDAC46102A19600C531276B447' +
    'F3900EF8F723FC3B60D7BBBBDA9F949E4460B2F1103315ACE25B3497CA4B4674A6052304609A16E9' +
    '0D064B0F6A9EAE8D8E4BE38E3B781F9173654E876CE6CC00578171FD9CFDBA5704AF2E60CF84FD02' +
    'EBEF19AED140287D195023000568E8D908EDEADC76BCA3E7FE8FE1F4B201C2A15603C025A61917AE' +
    '664B4294C57030AC988D7967364A8BB5857E98218C2943E8CF1CB8C719DACCFAE8F42AA7DDE9B413' +
    '0170068A9CC86B0552D1DD0A48F63DDB8F8A51B3C0E10F3174A87D18F96533CF0C30C0BE33D37C25' +
    'CD3EBF8F5F10E66464AC2FB8260BA8B5BD443ABC6AE0D5F89DAA42BC71B2A9BC2D10140B117E32F4' +
    '1353FA1ECEC9DBCCB621350806C4E4EBFC966AA7122D85532728EF26AA40E55E2E1739790FFA157E' +
    'FA4E2A68DA1EA4C9E3273CDE11DB7DA47220123C40D50B73B8460CD93CB449783EAD8A58F6FF2722' +
    '75A4F6C0437DAF3DF1D8D9617743D9302E73B715AA7CAA139CDD07EDAD2DE7B32B3F11E3C2B6A96D' +
    '4630632C6D1DE21473A287D42B52D210CD4EF808A998148817BB5083BDAB3A3354B084C5773970B1' +
    '5DA56F755E1F25354EFEFF89FDF4E307998D623CED32C7B9E942B6816A87A233867E2BEB84582FFF' +
    '61AF612BE6E521AB3005E17D471A5C67A0A927211802CA4A0C2FEDC01A12AF5E8A6E8FF7BD08883A' +
    'F1D35C2F8570091D9E5FCD9D720644F0A45042443D2FAC309FA7A15B2DBBA1517EA19407B17CAF7E' +
    '9EA81FBCE42F35A0568B2F70B279E12DA48858F0E0FF0B5CB3A3012308AB83DB627F5439E3A424D8' +
    '3F9E0B7750F2CBD9CA4AFB9BA77765A1EA590CE9E37BC7EABE1EF099765CE59EA74C2776ECCBCEB8' +
    '95997F577733C609B9E853CF03010C5B9CC4E819C041ABF11A9B3866502BF6AC0354E6CE1060D515' +
    '5E6E5A1AC4FADF73080B20569317F9E118B97C78D7273300E329373E5B64A4181C4AC7B8933F82AD' +
    'E505D52CDDCEBA373A54D072BC04A8D83273586615ADDC4D6F258BFD75AE94EFD120F20B721A1CAC' +
    '33FF3446E554D6CFE266B05E4CD44DE77D263A9D347F53B282A7BCB1511DD405A67AC05E7233F5EB' +
    'B40D38F953AC903F90E96F17A6357EB59ED7900FFD82F54C3043D37F7774A0F4DEFAF65887EB53FE' +
    '156FB26794982B1249BAD27C89C136097AA2268347FC9C5E5BE79E23C923215353A850E63748E766';

  S_SK =
    'B845FA2881407A59183071629B08223128116014FB58FF6BB4C8C9FE19CF5B0B28965F58D99EE0DE' +
    '7BFB7840F59F65414289E259E05E8A18D47EC06D7900284AD7A0AE404B93B0498945ABD07638920F' +
    'C7F8C21282031EECEB0CC48D71FB0AD1CE84B9B3DDD1B3A2A40A15D5BFE0BE23FC4757137C6C8FEB' +
    '7F8055677BA4AF2814242A9AA0108A4046981800042130A2340A99140E01A931E1300A1A13695948' +
    '6601C111140451244206E4C848A1B80008B2490121901224500B03306112025C144912B30049424D' +
    '14470E93342023898803238CA1C081CC0411184545C1C20862465210300D52B870A3B004DCA20D4A' +
    '386511214A01B48C43104809461253068161348618A00920878CA0168682208E41188E52A2902101' +
    '4652184521A42902132ED0B8248816710AB245818480942670DC126C1BC16C401624623600899610' +
    '8B124E1B3721639225E1C88CA346285A32111C856110B9295990910493651CC40063B810C1184A52' +
    '900C1CB20D2380090A4021D4188043A64D0835060C98650CB4890C8661D804601C0789D09231D808' +
    '1100881119294E48B661D8962DD2B04121A1051226860349301A246D99483001200C5CC284012890' +
    '0A298850340E1804640AB409D1968413196DD91031133689808025E300294A06304448109B429008' +
    '858902A20D13461110120EDC380C4C468842C004131490C1462248B82D48C80D64346C0234810913' +
    '6842A4211B26511C302A6328688B18609C922C22C9000031058AC08C894430D0142189424C4C1064' +
    '00B64DD2A8246306308BB4289B246EC34262A0C00C0C2050E112444A360AD122320C36014836804C' +
    '00400C16090C9569A19829110602CC982D08C811498445D20290014942C1062E42C000A2902D9810' +
    '711A054008058608B2651B36441109402107100B3306D298858148088034305C3844DC266ECA9470' +
    '04394A442841E1464898862D113621221509530826213088DA94804A80601425321295641B02120A' +
    '92241A4422230624CC12818922600036608A32820CC368A1A04D5B286A110744589851C42831A406' +
    '4EC0C831CAB46810B669132488840462A3C421E0920523303050B6105942285C3005102286CC260E' +
    '2226450003928A242C9C024E40B20400148C4A086E4BA6444CC84924464D4946821AA4690A316E21' +
    '408C22964949044618B72C12B864D1C851C3366E5C2008E3328513198564C2019A4822111300A4B6' +
    '4960C06D1944628C086119B761021570EA732640984F68143D81AB95763698DCD02625A980994C19' +
    'B7D47626409370F87E45D39B76B6762F7827B2779BA44046F54228A7B1BAC9D8E751AC2DD2485215' +
    '64F81D028AD671DB6A3A5BA3A9463597A519AE0C1DA435FEF7C421CDE7F8483782C4334993192923' +
    '796DDC3E5803F6C86CF2B37E39BBCB53775E5E9D2BDC65E84395B01AF95C4398503A7E63A8D72F7E' +
    '9BC58D7655CB86BAFA0B679D1D791F09703F6E4B333A51DFB14B36EAC9A3582F795AFCECC2A67A42' +
    'C7BC43B447B73EC8B73C9164B2D9EF4F767D607E975738E1D871FE61B7BB9570516959B9E7D81554' +
    '55FC91590D84BD430921568B718241CCD09C989DDF75F529C4F15818969216EC0FBABEC64F184832' +
    'BFE428BE44849C42F13CA84A5CE6CA628044CB9DA3FD9CB5EBC776BA0E12683B98253B213B286593' +
    '779EACC489C985BEB734A7506791BE1A6E6E03B557CFEF2069091B846CDBBB72D36A89FD5C45C7C7' +
    'FBFAED956EC9D8F3025225D6FEF811B1CC3C6705F056388BBFBBA1FEBF4D072281DC792ADA048216' +
    'D60D0D6044150909B70A4753336F4D5D3FDB8BACA62946DB28332EC778A76F96577BAF868BF89677' +
    '3C200637495098B0F7749835915348D37641EC74D5A78E565D8100627468B0B5D05C2937618A98E5' +
    'A12476533CE9D699E0F4F23B3127EE1F8D3CDE3851A94BEB8D606D836D47A9F090581B660FF8EC31' +
    'E3EF4E45DB530DD88A44D48F84B6BD3CED53BE935EFBCE9930DD83BD004F086A064B3260B7211BA0' +
    '547CB29E56603B922597B31A30A5FFCBD8A7AD2F3F05372DCEA89048C683164410E56A6E5B7EC2EA' +
    '6E3B0176DB1EBF9258C97C297E752A202B1B4B02DA47235873CC3E4A376B655779AFFFC9AEF2E30B' +
    '62BE022AEFF5E34B468507C921A00F1110B2CC1BC99B2EE295A31EAC141AB4D87EDC11D7FEE22A07' +
    'D69B7008C107EBE47DD2F215D074868C8B929A3DF6614B5E95CC8A52F96C57E1697CEE332F45B198' +
    '0E6CA372574BDB51825654E6FF31D8E755CFC15B602F46A0570D3BD0D65E43BDE8F792E7D526B3D3' +
    '62FE33638B3870E1C29E1030D3DBA43C3D0DB6E5F501EA3441B709E54FCA1FD3E8654B1A9301AD15' +
    '1A36C8FA1513D0229DE6E94D02A97B9D6F2EE8078A5D1228E783B8FDAC46102A19600C531276B447' +
    'F3900EF8F723FC3B60D7BBBBDA9F949E4460B2F1103315ACE25B3497CA4B4674A6052304609A16E9' +
    '0D064B0F6A9EAE8D8E4BE38E3B781F9173654E876CE6CC00578171FD9CFDBA5704AF2E60CF84FD02' +
    'EBEF19AED140287D195023000568E8D908EDEADC76BCA3E7FE8FE1F4B201C2A15603C025A61917AE' +
    '664B4294C57030AC988D7967364A8BB5857E98218C2943E8CF1CB8C719DACCFAE8F42AA7DDE9B413' +
    '0170068A9CC86B0552D1DD0A48F63DDB8F8A51B3C0E10F3174A87D18F96533CF0C30C0BE33D37C25' +
    'CD3EBF8F5F10E66464AC2FB8260BA8B5BD443ABC6AE0D5F89DAA42BC71B2A9BC2D10140B117E32F4' +
    '1353FA1ECEC9DBCCB621350806C4E4EBFC966AA7122D85532728EF26AA40E55E2E1739790FFA157E' +
    'FA4E2A68DA1EA4C9E3273CDE11DB7DA47220123C40D50B73B8460CD93CB449783EAD8A58F6FF2722' +
    '75A4F6C0437DAF3DF1D8D9617743D9302E73B715AA7CAA139CDD07EDAD2DE7B32B3F11E3C2B6A96D' +
    '4630632C6D1DE21473A287D42B52D210CD4EF808A998148817BB5083BDAB3A3354B084C5773970B1' +
    '5DA56F755E1F25354EFEFF89FDF4E307998D623CED32C7B9E942B6816A87A233867E2BEB84582FFF' +
    '61AF612BE6E521AB3005E17D471A5C67A0A927211802CA4A0C2FEDC01A12AF5E8A6E8FF7BD08883A' +
    'F1D35C2F8570091D9E5FCD9D720644F0A45042443D2FAC309FA7A15B2DBBA1517EA19407B17CAF7E' +
    '9EA81FBCE42F35A0568B2F70B279E12DA48858F0E0FF0B5CB3A3012308AB83DB627F5439E3A424D8' +
    '3F9E0B7750F2CBD9CA4AFB9BA77765A1EA590CE9E37BC7EABE1EF099765CE59EA74C2776ECCBCEB8' +
    '95997F577733C609B9E853CF03010C5B9CC4E819C041ABF11A9B3866502BF6AC0354E6CE1060D515' +
    '5E6E5A1AC4FADF73080B20569317F9E118B97C78D7273300E329373E5B64A4181C4AC7B8933F82AD' +
    'E505D52CDDCEBA373A54D072BC04A8D83273586615ADDC4D6F258BFD75AE94EFD120F20B721A1CAC' +
    '33FF3446E554D6CFE266B05E4CD44DE77D263A9D347F53B282A7BCB1511DD405A67AC05E7233F5EB' +
    'B40D38F953AC903F90E96F17A6357EB59ED7900FFD82F54C3043D37F7774A0F4DEFAF65887EB53FE' +
    '156FB26794982B1249BAD27C89C136097AA2268347FC9C5E5BE79E23C923215353A850E63748E766';
  S_PK =
    'B845FA2881407A59183071629B08223128116014FB58FF6BB4C8C9FE19CF5B0BD77B16648A344FFE' +
    '486BC3E3CB5FAB9ABC4CC2F1C34901692BEC5D290D815A6CDF7E9710A3388247A7E0371615507A57' +
    '2C9835E6737BF30B92A796FFF3A10A730C7B550924EB1FB6D56195F02DE6D3746F9F330BEBE990C9' +
    '0C4D676AD415F4268D2D6B548A8BCDF27FDD467E6749C0F87B71E85C2797694772BBA88D4F1AC06C' +
    '7C0E91786472CD76353708D6BBC5C28E9DB891C3940E879052D30C8FD10965CBB8EE1BD79B060D37' +
    'FB839098552AABDD3A57AB1C6A82B0911D1CF148654AA5613B07014B21E4A1182B4A5501671D112F' +
    '5975FB0C8A2AC45D575DC42F48977FF37FFF421DB27C45E79F8A9472007023DF0B64205CD9F57C02' +
    'CE9D1F61F2AE24F7139F5641984EE8DF783B9EA43E997C6E19D09E062AFCA56E4F76AAAB8F66600F' +
    'C78F6AB4F6785690D185816EE35A939458B60324EEFC60E64B11FA0D20317ACB6CB29AA03C775F15' +
    '1672952689FA4F8F838329CB9E6DC9945B6C7ADE4E7B663578F87D3935F2A1522097AD5042A0D990' +
    'A628510B6103CB242CD8A3AFC1A5ADA52331F4DF461BC1DA51D1D224094E7ABED3D87D98F0D81708' +
    '4780EE80370F397631ECB75D4264B6B5E2E66C0586B5FB743516399165837A0FDFF7C6134F033BFA' +
    '69C1B2416965C6E578592F40E258CB6DFB29FB8E0F54355B6E24A65F67ABAE3193D007115CC0B9FF' +
    '94CB911A93B1A76C0E7662F5E2B20139E0159ED929CB932D4895F89A02E55C59DF2DBB8F6E5DD7D5' +
    'B1F3CEC37B4A9166B381C5440E23E67368CDE0A29D59AA05A3C9BE24A4DC8DD75BE30E82BC635D36' +
    'AAC66DE880C6701A987D7E05F0F2FF287828BEC30595089D8AB9AA390ED719CAA6E576CDBBE9B184' +
    'A322E5E2DABB69C23CC696D54FC32FF57001B6B64E2A837F3062D85AEB50B3510F7EDFC34DF38E08' +
    '3D4D9B94FFAB0DE15D73D9AF30B9F31CC4F41C9C24F2D618B2A7C3C4BDFB745D52D3EB54589C8BDA' +
    '8AC05DAD14EC744505575A0988EEC651C1715439FDFB29923380A43C1A66A86C982A841F11820A6A' +
    '0E1E2F2FFF5108ECAE51A6AABC9B949226D228FF84C4E5E5D63114D80359C4931E612DCED1838B7D' +
    '066AC9182CECFA223A21A4C8E155AEFA780373BCC15098AEE40C033AF22F8E7C67A0D2526DA7475E' +
    '830308C04AED9D32BCCC72E719EE70A8D13F09AC11E26EA237D5CC8F98B5AE0E54F933BD0507942E' +
    'D900D056FD32F8E6E81777912FD482746029B71CCE3BA69B8FC2D03EB441027C387BC2F95031A0AE' +
    '7052215EB24B9EA8FB0A961B0F80BFA80D0D6257C1C22B508C5D31B97FCDFE1D1766E8A9C8771932' +
    'DD598ADB7E717743F45FC571F21E4A516249F81D747F15329790F0F70A0B8E461A4EDF50504AF03F' +
    '30DDF8A8818E38761E1681D6DDEF0B1DD326B2EC228CE48570F285B49D29D7C2EF37866D5446DF82' +
    'B8E43B34CB248962A21A9A3946159740F8AEE8E6A16A4EB2B42D143FE2612E05EF4B5E646D813248' +
    '444556A2A8BF92CE10BADECB6B8A40B080DD42D53346FEFCC4B9B40B1E4998991EC753C95AA2F2A5' +
    '06F311E710B0F1D36C1DCA6644EE6D1D4AE9CEA5666EF4B3E888DBDBB95A77ECFE1E8B477DE7CB07' +
    '639D682D53020EC14EA6C7DD7E715389D10938429FAB8A068A1466A4CD891359F8074E0F5A142ADD' +
    '731B87878D985E4FA6ECB3B73D298553418273E9503AA84092C080E5F2902F90F5C59944D24CA027' +
    '1D11D0D6734606D039550A37FCA2B735850E63F540F2F06B79144B5C4ED2C700BB51C33D265B3D03' +
    '7389C99EFD597642D829DB1EB58643CFCD07F4DEC60B8F727D97BD7C4B59BDA1';
var
  PK, SK, B: TBytes;
  S, P: string;
begin
  FreeAndNil(FMLDSAPub);
  FreeAndNil(FMLDSAPriv);
  FreeAndNil(FMLDSA);

  FMLDSAPub := TCnMLDSAPublicKey.Create;
  FMLDSAPriv := TCnMLDSAPrivateKey.Create;
  FMLDSA := TCnMLDSA.Create(TCnMLDSAType(cbbMLDSAType.ItemIndex));

  PK := HexToBytes(MLDSA_PK);
  SK := HexToBytes(MLDSA_SK);
  FMLDSA.LoadPublicKeyFromBytes(FMLDSAPub, PK);
  FMLDSA.LoadPrivateKeyFromBytes(FMLDSAPriv, SK);

  B := FMLDSA.SavePublicKeyToBytes(FMLDSAPub);
  if CompareBytes(PK, B) then
    ShowMessage('Load/Save PublicKey OK');

  B := FMLDSA.SavePrivateKeyToBytes(FMLDSAPriv);
  if CompareBytes(SK, B) then
    ShowMessage('Load/Save PrivateKey OK');
end;

procedure TFormLattice.btnMLDSATestNTTClick(Sender: TObject);
var
  Original, AfterNTT, AfterINTT: TCnMLDSAPolynomial;
  I: Integer;
begin
  // 创建一个测试多项式
  for I := 0 to 255 do
    Original[I] := I * 1000 mod CN_MLDSA_PRIME;
  
  // NTT变换
  MLDSAPolynomialToNTT(AfterNTT, Original);
  // 逆NTT变换
  MLDSAPolynomialToINTT(AfterINTT, AfterNTT);
  
  // 检查是否一致
  for I := 0 to 255 do
  begin
    if AfterINTT[I] <> Original[I] then
    begin
      ShowMessage(Format('NTT inconsistency at index %d: %d vs %d',
        [I, Original[I], AfterINTT[I]]));
      Break;
    end;
  end;
end;

procedure TFormLattice.btnMLDSATestSignClick(Sender: TObject);
const
  S_PK =
'60FC3030C9CC67C44C0EFD47C60D02296D0F73B25AE1B5F506EB9521D59A6A50AF94D2FB6C3F81D8' + 
'C4F6D0E737828747FD25BCAFBCC448E3146B9897FFFB3055D30604C55B633D1F67D49B11C103792D' + 
'FACC1195154F5C770D50329F52449B7C0D67881740A723BD3EF7B419B74F823F843A09C123DAB65C' + 
'EA4FFE95D2A14F991586232030A476D6B0948A7855DC551104BD97545BD68E5124013E31F9EB3604' + 
'AD2E2F137EF7826C62D6B0855D5C920DE6163C0A76AD5274CCD52FCC872E647C1E33D4E72B258020' + 
'88F8167C68A238A7113BD6CB425D30F5567488B9F798506334839FDA32424E091A6E22E3B011C48D' + 
'BA40421F88CBBD681F1AC5489CC3AC9F7E352B1A5EC7065FE3ABAD7B29894BDEA3CA06ECB229B3F3' + 
'52A8FCD69459C92C13260D31DA3347CFA97B8069B5F4006D9DC40876FD6D6B33C90A07827D77FD42' + 
'ECEE7802BBEB38925EDC8C24DCCF464F6AF5E84387732D61F02F9F24062E62A5C3176585CB6333D0' + 
'13A7B03073690FBF5FA5943CE65E106C664C82CD0338AD069672035FE9FDFA666F11B9C68B04BE2D' + 
'4ADB531BB81F6DDB817F5F5E52C0D61CC0ECC4F6468DC67185368511D87C614E331C83066BB89ABF' + 
'763F284C4BDFCC8697F12ACFD44AB613472ADD8F2510D5F664CC0EB6D9F8EA7D1106D3346938C70E' + 
'5513F8041F5543C749C6C3966513ED346780568F99E232E75B7E9C3489B5B1022AF85DFB44D4957C' + 
'C436A181F22A0AE03FF222DE0C54C727EACB9F590C78E21393EAA12E2FFAB6A0EE346CCF9B236DB9' + 
'E7110305A150CB2B82A639F3E3F8956E0BF29691DB7C11BA94D7483E000B78D039DB58A873108BBA' + 
'46A981E5F4E3B4915D4BA10391C30D8CACE5DC11764C3F47B14475085B90E91B00C79D37FBA31A1E' + 
'1E1D9B8300D2C30F52EBC8FB050ACAC3DB7A14A05ABB86FAFCC9164915DC7F0E790AE8447ACE8A9D' + 
'380E5905A914D0135612A69AA8F108F3220103F072F362B9243B216F76E1E0ABF9C9A054C00404D5' + 
'9F8F804A07484C7FA8EF71588DE20B7F0469BAB32EA0BE3C72A1607C6051F4B0B40BE9A1CC28B86B' + 
'087319EC1C8223B983DA3C201F3953004E51DF3096D87517B03EC831DAFBB84345A2EED2AAB12FFF' + 
'190384EC74420099978557AAE0A3DF5554BF8DB4CF3F3FCB4C337E12F9F756E9CFE681DF345ECF90' + 
'DFF5301A80ED8869A5BB70E533576A3C8D48BB9F75718ED10ABC6E18654FFC44380AB4CEBBE02BF7' + 
'EE6654BAD63AC5A3C580B68E5859137AD8BFB5C57A040C23F67864DBA78A77F5A17DB4A7BCC175CB' + 
'C624E7E6CFA497ECE4914C9159E11B067229AE8A38EA468FC043E229D343D143CB2BB38A51CFE492' + 
'C2D506D9B57CDDB347A7FE0FE95225456FC5FEB799B50C895737A6BEB581AD0404CDB9D8FB77AE8C' + 
'39C1D9364018576A750E254B35462D34D4647AE5E608DE6C899A8A38A462AFAD6317A52666382C8F' + 
'7770E3A945B14A9763A7859E564974B442AE6CE5C22BCF5CEB78A4B79DD94DE17D0C31AC58905B68' + 
'50D884281EFFC86A1D64FC72540BF92BA88B1FFB78B19514505DA0ED886F72738AAEAE5F7740E8D7' + 
'F760A95F22161BF66FF64CF21F9DB33E048807EC3B988D2CB5DA7E4A974636CC44F97129379B457B' + 
'976EDEDEF82338219E16F66425F690371AEE0C7330FE041EAAEA1F2F58C205178115E68DC1F7B1DD' + 
'D2E884C3A8772D29E6A70AEE30241D69DFBE4D64C00C31FFB7DB9B618F412AAD7662AB8124FCCDD1' + 
'AAB273683AC778B992BED45A8A34F4EF08FF14749B26A57CA14F1771D1C5038217EA7909050706C0' + 
'A084DE280CD734D90EEC8B56A579FA720C72CA98F778C0369F501FCEDDA11277';

  S_SK =
'60FC3030C9CC67C44C0EFD47C60D02296D0F73B25AE1B5F506EB9521D59A6A500717CB32F9BD049A' + 
'77B2C5511B777B7422B6FE1EDE80985951F3F22BDCA5F6AD720842C17F42017DAB38E431FAA7878E' + 
'936C1F23FA61CF1EB7CCDCBDF816F8628C60983C78A70D512BA37EA469CD9E7D0C53F6F0B9196614' + 
'223FB31B429A6A3B03232C4C00111CC48543468A13090AE0988104316DCA34248090812301201425' + 
'229B402E92B6911481259846860380248208520AB929A32245111586020648A398401A190E90C829' + 
'1341111AC36514806484484D5A8031881050DA1412CB180A22C82DD3C8404A828094C44992288E58' + 
'984919038062C4081CC810102549D320620904451B03261491609116485CA64C51422ACA10861121' + 
'821B210A53B810490451D4147280802C0A252E214065A12806C3927001410002416E80A088844690' + 
'0B45929C84695C300511076DD0328D010321090026C198444214720A01281B836941A6311042729B' + 
'120824852D8A144108C31008A5850CB510023351D130241A811014902D40960DCC82204318851949' + 
'52D0A6054A384111260EE2285024472063820822A72D4C324159460D4AC42D90224614B164192052' + 
'92040611C3650098091B460E4BC6301A014C6020081137044444706424001A3602D9A4251093810C' + 
'B930E0022900868C0CC3451428425B302C03B98581C6296346491C1600CBB071C1228AE4A4915144' + 
'8A934291C9002D1240118B22011B2044020044D0B6205008065A90455B042209C000828851548248' + 
'5B0081DC9001C8964C181050DA247120223052A04461960C528025D8B888CAC841D8086D12314D49' + 
'A6800BC150800685A48851123442410832D802620028845A8269613462E1B64823490C5C26411C08' + 
'90104662A2048902A70903154E00B500D1048C5AC26C1B456C89300A24C505912028C8B000C19648' + 
'1A396C80B849820069DB406850420502440CA1844119400550068901176A09A724A4C21060149003' + 
'083100450ED8422E102430522066D2946D40124113C164403064A09620184905D2122A1A476ECB16' + 
'4EE30841E3A07118B00419A4809A826109B96842206A41186400148E802405C480888CB40940A860' + 
'5840458C0212DCC824D4B82912C985404220E4C4302082118B882853C824CA06050C072DC8A22C09' + 
'38008C2049E08050530022CA94491BA40911984923179152042903456E0AA8241A322CD0144C10B6' + 
'308C0222201172DAB820CA84811492448A3BA3C98A607F8DADC136AB4891DB9E86BD5E76773B777C' + 
'3114BAB40C5CACA95E840C73BB3E7B1FE16E8512DFB5A72491CA15FDE8BFBFB1F72A73EB11D3FD6C' + 
'887D3F9E7122CED067F0AFC4527233B11318527FB236C3704FA5FC16301BB750B6DD79F84865CCD1' + 
'EF8F1180510D4B56947E3D993AD9200093610E5814F926906C84F9D22787503A5886C78952DDECFD' + 
'3D7893258C44A59D899420F12A9F405B897A935F423D7041778340A4F41F07935129F4E8B1505192' + 
'4148EAB0548A880FBA223F75031E33706F04896A600EA02BFC12D97AC3BCD626B3DED6F3B2BA46F4' + 
'74DFC1266C4C1C915763C1BB4D71D3374C01CC5E09407F8C77F575BC39B258718C4D85CA42C92A6B' + 
'3DEE6C7D52D0985B1EECD83AC67C42B2A71710193D95C84EAE2D2CD5DC9A3C9FD3ED22140D8AEE53' + 
'C69DB30653AF6D14301C8E8721AE002AA67F8CEC7C9CDA6050B4CFDDE3D5D15EB8B28711F4B38762' + 
'A6FB688BA952299AC83374EB6D03553F243EB3A537C5DEB2C099CC3B5EDF798031D9DD131F956301' + 
'5FC4D2ED25FE74907AB61E6163795D45A465E9CACDB9ED896750F5A03A77E85FFAB280F22525BC8D' + 
'C9A29175650D26517938EC3A4707AEDDAAF6C67CDD499B16927EE3B4B435E5FFC29ADBD6FB4F7E73' + 
'B62058BB5EED6EDF9F7FEDF1495193A6A76C8D475EF5B6605915B07F40E7C10EDD60AFD71384809C' + 
'BBF94577C6D5E51F8395BB2277A886188680892801B5D718FC4DF915D3778872E44C42BA7E0D6E2D' + 
'7628105C9630976C50DCD9FC0239F2256F06656E9F44BF8C993AB9EDDDFE61F0C0BD4CC7E7FF3EA3' + 
'EF0863EBF22B5B5FC0696000BA2A1874C8CDF4D901D831537158BF0E747FC365E83F490640EEB309' + 
'C1079CACB4D9B4D7FA46321CAFB2D13D101E988D52847854C09934659B62FA1F7230DC94161060B2' + 
'42C07C1DA0B1F1AC9FBAA2BA6276729DD0660C057CBB1544AFF5FBA74B0EA7AF375BD67BC469727B' + 
'A9D35BDC2233E87E405EDE5C97B1204BFBADCBEE037EE6C6C550243248522B2AB1F195059C0E81AB' + 
'D02AB579C7BC9400691D715288B22FABA998CC02F7640B32F9836069A888C13A9676C8E1B252779D' + 
'0B4DE6E4F2D68CBDCCBC15E5345188094756B1CCD4CE0D10F11EE89E3D7377F5C96445BE8676D9FE' + 
'7CDE9CE497D128F1BA42F40C38C1E0931497BA8B28EAFEA41A5CE203D19E1F44BE11DBB0B2876051' + 
'72690F0988B7427F9083B9D583FAEE3487F6B75F2CED99DE2E67F8D9987EEEF91E51828FBC312843' + 
'2BAD5BD9B3FF385858B65CCF1C79905BEB655E46DD9210F87C7D43FDE100356984DBD55956632665' + 
'864380EDE7E1CF4450DD24A463BE1BA934878C6872F60E4AB96ACC44B157A728F0EC32FC6A7F0F1C' + 
'4348911BB715AC36F88D68555CBCED2B4B1EDC339DC6FD36BCAF72FFD808038146249964FF8C5F18' + 
'23C32A0EC44F3F97192573BC79D7DF1A473E8D945D8F4D72465307A01FB6B165729CF56FACB03F11' + 
'062B1A6CBD9B300BD0F4FB6A54CE360F52C378E6E150D8BBE3ECB7DEA65457CD3121914283481D38' + 
'0DE6B527744EAE2BF45FB09ED5FDB7C0F7CD45427540E674306E7B4303CF0825588F606EDD795C31' + 
'211259860F2913E05061E9E8D78B15DA97A5492532B4EDEBF63C901F620AAF9ECC9F2BE5B5E2F028' + 
'64ADF1B6A2663E9D654F20ED0FE14BDA106336AC76682E84D66ADC6826176240C3BA7A8D0895ADD5' + 
'5706069B378304FB6A45AD9A0354C4D0DFA13A2F97244AF6C1F03B91A6DF0DCD938BC2DEBC275ADF' + 
'1B9D706672C02E12B1D2FB129BA4D58FF2F869AB929071E5BC4BC420E2B346F56424FABA5C80E141' + 
'8CDE6542F74297D62F34D0E84FC8C6297BD95B1B5AB5AEB3113B6B3A27BB9866656BE9572A864A14' + 
'B2DF2185AFEF09C70EBA43046E4536CF7938BA7E45BED3A580D5AE98074F254D3E1DDB15EB705D8A' + 
'3482316BAF5A02038D4186EA9FF40AD35D3CA955F5720932DBEF69016773C5AF8BE1D7278BFBC58D' + 
'4D500FCB6AD9D7ACC9C6EB0F33CDD0D0F49D40505D9020C0332756D35CC5AEB9C873447C90C25B22' + 
'0A3FC1888557692B9F33D1A113C56DBBFB018691B96961B73826F58DB0C411F548395DD3F5BDBE77' + 
'7BBA14152C6910F07E2577B08247645D4BEDF0A13A87F7855CBA4DF7D8687AC0DD1241EBF3B3EA93' + 
'98E7CFD4CCF1A237AA03D840030206FA7DE3FB35A4CAB4A2A38288F710A16FB0E24B95851A18F11A' + 
'1C65DECA9A66B9C3B9BF1863436E494ECC82E5A04209F00FB03E0B6B094013ABA55EB3FA9B325521' + 
'C50DB1844479006EF65B088ED1AD9CA62F4A8A03D3D3EE041D3C44D9B2D15216CF8D0DCE2A92C6BF';

  S_CTX =
'9B13BF381D9DC5DB51F7DF2520E41BDBD60CDE6C2F175FD92837226843D04FB25D2084BD3DDFF2C5' +
'3AF872D7E3A1CC863973F060B3A4391FD0FC180F6300D4C3CEC2C6ED6B554EC66D921168372FD641' + 
'47856818931D74240DEF128BA65F584B60A906BAE2130CA28B847E28C8460E';

  S_MSG =
'968DCBC54EB2CF5CD36F04BB004EB1700E24915CE0885BDA2C5818C1A7E69414AA1968943A5C7A6A' + 
'E152A2F130A246067162B304590607FF210621C81CB0AE7661345D465522D2C229EA62E8887DE14E' + 
'213471565CF6490860BDB70D1EA355433A2A6E11F0683BE3B80FFBA20E67BDD914406C71F4E9A0D0' + 
'464EAF91068BD2248D71504556F58E8EA6663FFCC78F570F4EBE98F0C8E2B48A6FC5077FF7E249F8' + 
'0CC23265760C97AD303454A97310964DCEA6735083DEE6BFD6C651EEA2BFC3FB0FB30C5DDD25B5C2' + 
'0E1CCEAEA28B51C1B4AA3EBADD59AB191F108A2CCFA6C9FFE3F635C5D24AF80C6051A92FF706CF61' + 
'F321946CBFA6F24478DEA7D5311614B84CC6B472ED44E24816FD60D09E24F7C7E6BC42E35E16C697' + 
'68220AB4920E0AC1214754EFCDB61E3604F282853E5A6783933AE43CD736FFA0B582280C7BE73A99' + 
'839E2A05A78B02CBF91A195EB67B1A912F859CCDA88D5F4C4DFC7AD08118501E50390158EB960830' + 
'088DCA36824D9D3052A925CB058BD384F6169CB0BB0B58C3509CAB54AC6C3A5450A2597B115663E9' + 
'AC36DBDADCA1BAFB8DADC426EB59D26209DB8DE6364A672D83016CC0876131BB3D87098E6D629FDE' + 
'82E29E1399BE55BD62E1DC4312A170B6C4483C7B7F3DDE2CC15EE540163543E61F9F87A7002B75C0' + 
'7CECFD658DF31A23C720C3B048BF7E0A899F3F0F7ED2A9604669362CC438D787F8853BFD4767CB00' + 
'6E6412C095895056CC4BE0D7B36E45B7E945CD58F38887DAE0583A99154A556A76F27DC48A92A613' + 
'C22AB54B13B7E0321E51489DD142F3EB289D523B9D12808297F48F94A0630831E39F9AE674B07733' + 
'94D7AB96CDED5E06871E2893C489B35C102AFB8BEA750AC9E7158A30487A0A503017C5506E91AD92' + 
'7F7909AB2DF0476ECFE2607E00A84A59281A277A80DD8090C955830BF7823E71A9FC3335CF33E30F' + 
'8F0A0DC80BD3A6B323BDD3AD02568FB2AFD8DE11FFD779DDAA6B52A65C6D17FCC72AD10F902F69A7' + 
'6683E2705FE9DDFCA38FA3E62FAA39611A9EF7747A0C37052E3296DF52324252BE0DBC3838937BA2' +
'E0CCB39373452F5D092442D28228B422BB1A04DA8BBDC85722162B5E0843855C707A9C2E2FF22032' +
'0D219D1B896469EA6BC47BBDEFEB1C7DFBE28594773C7EA740CC37EC121FE737D8F4C6A83AC439A8' +
'ABA29A87EAC528A866AF4E162AF926D4C9D8E0C30D158D064B65821894832C4AF9B08D9E10487F20' +
'8F45019F25329D5B0C5B2B670F32583841ABD069E6B8ED674F7448C754332FF53FEFF6664DCEB11B' +
'CF69D082EB7CBE4177E1D1403EBE1086AB4E6ED74E6CC4DDDD17E3CD74D01A13';

  S_SIG =
'0476B8DA70153726E115877BC45607CE80DE0B02C499D254CA45E095D627928E0E9C38A9BBDCF4D9' +
'FBFB1869BB4D4003CB70C46F5B225A6CDEEFCDF96611941A676FA0AFB6C6F9B39C0083948FC90D99' + 
'BDA6FADD9F508D213BD7D0BAFCD399E20927E7E334B006A52139865C18727EE0CF726578CDF82EFF' + 
'C8659D2CC6E68EEF3EA037F9F6B411BD88BAE3FA4C28B3DCE901F73F681F9E5F106C843F20DB42FB' + 
'7DC9D9A4CD7AD5BE7C04425FD5EEB53B98241EEFA30A63455B10C84591C3AAB938C4B5EFB8A12D09' + 
'805EA5710DA1E20DBF0670F57997482F95CBECAC3C4AE9A6E2602CD46950D2F4339C7CDDE3085F3D' + 
'38BB0C43B2C5B767C1DEEC2A6E3199730EEDCC6B2DF10C036C2E7AB399D893B3675F236085183E4E' + 
'3A859533FA7BFF97DBD12710ABE4C813479AEC23AF042F6C6ACCB0592D73E2A56CB8A61F1977F29A' + 
'93F05B592D7E7AEE8911B2D93645C3A43E6AED6F6FFD84DC49E5DE468C480DDB493E9C8E3DD92E18' + 
'8E39E637F93377F4F0AA2FD6D30BD1DF4DFF3AA02849D20509C3F6C6F23085A792FEA8D4F3ED254E' + 
'BEB10CF1A2C34206ABF508866CEC9953FBFDA7B9B6BDEB08B5309F68B757BE8ACC22D22E1976DE4C' + 
'8A97FA6200DAB292E7935C7CC6900B3F1ED8B8990F9F2F18FB23F5FD2ACDED0994D681AE2416D263' + 
'5159622EBE009137C9C8ACADD41563E850527EF4DD5D0E12866732B13EECED11FB8AC3CB4B657A19' + 
'80EB8C0EE3A9FA92BA5C82618EC8CF5B2D6514B1C3386A27A6DC0F2D9AA7AFDD4D93296C772D0C91' + 
'91E26B02DB137DF2ABA473E93C04001EEFF610BE56FCC8DB9E4D717BF2F1ECC3FF9A663F95331882' + 
'F7176590AFDA22BC697D23CDBE1FC6DC960930220CA063ADBEDCCEA4D6388E4BF05FE343F29F5526' + 
'AE7DD35252DD2BB67AE0FF9A509D85075A7B8A073DD5D6A73707AC22AC7488A03F04DC6F3334B808' + 
'BAEE98CA1993CB4BBC21E7CFC5406B2730E6BB31D1E2D2134564C098DAFD158B140670472440D340' + 
'8948CE3457B2343CF9359B3949BDF148FDB3BC4915063FCE99DDD9467756C8F883D83C8CB3575506' + 
'8B1F9ED99DBBD52842D277CAA5F55E88F4DA8763B4C1D733FD0F81999D858B98BDC2EE8DF9E8F46F' + 
'7A0C73915AE1C8FABF707335457FC61937993DC99877AB4DF122A8723DAF4B88B1DA92E398FC7FED' + 
'2084ABF5AD370538001239BF46058DE89A7B90717627A0EAE7C62EA1832EC065A694849AD992B0C2' + 
'7B310A55D6E950F6BC470E5818FC0194F0B1552A9211D9A8EF5C940D1719AC315EA925ECCD107674' + 
'1A2A1A3333E0021FB5B16F65F58181BA64904F6550945FCBB855D6AEF12BC1E3C9DAC6D6E3E5F3B5' + 
'CAF2D0F27DAFAA97C1426BC3A740B17653DC809F2C0CFBD6409E6995FE905198CDBFE8987B45E683' + 
'9B72365FF16E465DCF5E139F02A9538722DAC7E0DD1CBDD462442FD1200928CE291F651617D40597' + 
'8A8A946FE37821C9563D2DD24ACC282CC9FCF7736E228436586F352D856B9009BDC33A834A626B58' + 
'0F8A91003A12C2D30E5B7D49883EA3594DA819CEE2D39D8C3CF691E42E6554B4562FEAA300BE3950' + 
'C8264E3F431DA17381B11936972C165021285608412836085194AC26A5C5868DD2BB9BFD7B561E74' + 
'54AB63DAC73C5B1B727B8E1DEA479E47ABA6BA3FE5FAB099CD3E856D0654F01E72FB65C5E85EA48E' + 
'92B824F550AA0B9329007FF6543C0A5160AFD2C37F3D7975972127610466303E5B8E73CE4E973875' + 
'DD68411B2DBAA0CB235903BD5C95ABF58562564E37B76BE22CC205EA48652720F1B82C433762C30D' + 
'6115A6A958C5207B0F2F4D1B6FE34E487D88E8C815BFFF1C43442284F346525863198F4B7B3E0B6E' + 
'3326829FD1357DED656DEBDE0BD590E737337C839C2C956A863E614FD6B4AA68FEC99F2F34BCCE58' + 
'5FB152F7A56FA96FAD64544097E7716E3EB376141A43E577F8A2E886DB868B19521054E340EA2D65' + 
'D63F9CAB102D68C37962ED1460F3A4FAB561210F742424F7E63BBE06C3D0501BB8EF30708376142E' + 
'A5FDE75D0DA78E757C865AC24EB5F0A863DB8D681CE2AA1A645756687F323046E5DB2FDF9363EA80' + 
'9133711A4509FAFEDD4A8020EAFD0214677132B1C604927BFF5FD5BDB57AB8F04D0097476C4B1CB5' + 
'5B2ED0B19607BD95A7F71BD59D9370E4ABFA72AA383ACDB30B9668388F6F9244325E9A6D6F772C54' + 
'7A8D9B36A486F6832C547B2729CE6564B532B232170FB2B452E3C7CEC81BD37D0B5DD44FA05EBE35' + 
'F03C5EF46143AD2DEBB8EA0E856CDBDF43C97398819F7E49303632D96AC7F8CE3BE4217307CA2850' + 
'4EAF1DF7F2A162240B3DE7D85BABF5B6CFE607DAA439E9773D596A81C60624AE1460A9E41A2843F8' + 
'BDBC5B7A181A4C59982CC2CB58BA5C9447A4B6E58C75A0B41A6D2A1365F6B44E9271AF423A3888F3' + 
'A1613DF6E84A7EF05BE50D018A517BB7B312A04D18F6980B281A582B2D78F3FBD89205862171DDA8' + 
'98EA906A0D6B08D33E9B843E75F593E524796E321B4FE6F3659FB001A895D7039B62D54C3A999FEA' + 
'798A4245F29F6E992DE1B0CB882C1C5ADFE5FDAFB57CC6EE52371A76F65F632C6D0A76992749268F' + 
'0A59B4951AC1AF7D1CBE3E484161B5940BE065952CCEF341D88FC25AA906FF11C64595CC56EDB029' + 
'3948EE1898041F26FFB9BB03ACD71207156EBC072B1E0F54E7E68E153FA8036FAAE2BB0D1904552A' + 
'96CE167C00E2A5123F76FB762B4A2E66E81C35CD667A442B36A09C2A29812E3D7E99D8501E425969' + 
'0FAFA138AEF08902E3AEE8F57CDCB8F39B20894149EB8E72F919880EFA746C466C01D64A04F1F337' + 
'9C8EB95A02C5E5180104BF6222FBDF8110AC0EC316F89DC224FFA6973F37DA97DB05D94E503D08DD' + 
'318C30186BDA2686F46F5B1851CE792217F63E03C6EAD5241365E41A8854E633A43C2671C3650F62' + 
'874ED76395FC0C627830290BECED03447FA999FB93BD75B0DECD963E3076AAB489D041B44E9F4F4C' + 
'10AD80FC34B7F3FB89665E3989737750D1E5317B07765D00B75185C253C26DFA8FACB0040B656F44' + 
'9F35B3E84A4949AFF414F3F559357A913C0168E8E6D7930A2E9C6B5C9A7803270D32DC85EF4DEE3B' + 
'D7E90DFCC51690138B752A9BC2A8CB37A9BE25DCFA16C409519D53FB571CAC6BDB0A1969B6A2F9FA' + 
'951DAE001401EF3072D3C2BAAB1DAA6487275D5150639652DAF7CFBDAF62F6E4F5BAA4A239094847' + 
'138102DCEF4F9CE84AEF72AC7EBD6E8D24E8A5D3327FB8F343F14CE0D9CB66CF295E13483C7034EC' + 
'8A7C7439A9918CC1EB1ADD16C416E326172B3E6A6F7785888B989A9DA7ADAEB4C0D2FE323B54565F' + 
'88A4D10B1220283335555C7D83A5B2B4BDBEF2181D3F434D5055577E868C9DB8CBE5000000000000' + 
'00000000000000000000000000000000131B2B3A';
var
  PK, SK, MSG, Sig: TBytes;
  Ctx: AnsiString;
  S: string;
begin
  FreeAndNil(FMLDSAPub);
  FreeAndNil(FMLDSAPriv);
  FreeAndNil(FMLDSA);

  FMLDSAPub := TCnMLDSAPublicKey.Create;
  FMLDSAPriv := TCnMLDSAPrivateKey.Create;
  FMLDSA := TCnMLDSA.Create(cmdt44);

  PK := HexToBytes(S_PK);
  SK := HexToBytes(S_SK);
  FMLDSA.LoadPublicKeyFromBytes(FMLDSAPub, PK);
  FMLDSA.LoadPrivateKeyFromBytes(FMLDSAPriv, SK);

  MSG := HexToBytes(S_MSG);
  Ctx := HexToAnsiStr(S_CTX);

  Sig := FMLDSA.SignBytes(FMLDSAPriv, MSG, Ctx, cmhtNone);

  S := BytesToHex(Sig);
  mmoMLDSAKeys.Lines.Clear;
  mmoMLDSAKeys.Lines.Add(S);

  Sig := HexToBytes(S_SIG);
  if FMLDSA.VerifyBytes(FMLDSAPub, Sig, Msg, Ctx) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormLattice.btnMLDSATestSign2Click(Sender: TObject);
const
  S_SK =
'5AE5192442A0894AC775D84419BB7EDE9E8143A6E21CD709A47B58545EAEC7F59EAD08050900CBCF' +
'4093F9327CAD67B7600D9F8425084B6905BB6550F426F58B27B47C434BC194D466D83C14BF5ABEA2' + 
'49E05287EC80D1B56BD0824E4EB84CE1CD4CCD0DBD1BDE59106AE10516E1150E841B03E759DFDD93' + 
'0F6C7BAC857530451C438E128029E436491980441BA56913074A8A8831E084641A152C40B865C9A2' + 
'2100800C13828C1C080A84C850221688241100D220085302695B1881524812C832881CA970D91445' + 
'A122221226449106800413504BA86DC126321C312288486280A64463C261C220220A210653C89109' + 
'19925308200988640A3100CB9868434050C2A06C1830482138640CA80D03339254C44D42B29014B9' + 
'500400220BB38D8C269010B62DCC481261980522A03050A489DA2225A120859BC804801292529061' + 
'93985182B470584481D2806448C669C8B65164B6219C964143262D1294641887111414500A166D64' + 
'364EC282095C980502202683C48113A20559B02D10090DC084519CA6215C8885E1088990200E1112' + 
'6D182129D422421C2811214611582829482830A1400EA4400A428040D0882504C72111B20C8A1481' + 
'23002C53B688943411E0448140044A03A4111B042C6330715204285114864B8004E3B649E4B831D1' + 
'3264D814094B468881424A01430C41388008478A20378A01A8080831228CB285CB880D0A13499C86' + 
'0191848D04A72192A80181248959964D0805202341511A36081012842407300A1820D39881E0164A' + 
'0AA669A2462044240C54084D1B208C923869588428D1144E63920022C94DC9320800497111A28909' + 
'B384234591C22452D8C220C810610106669B062959B24C11B38941149083886108C488D1462682B0' + 
'0D14B8685AB65080409119B38DE2A44D1A0161981489A196610181514106019B0804C3C461A18230' + 
'10A32D21876098103190425252145293A241534692212332A1828D40286ED3288AA38689D1048E1C' + 
'936910970DD4125184C891D4246A194609E33426D0A485481605618489124126C9B6281C2111A386' + 
'0543C05158386121828C1835645AB68C04892C03422EA148065280910146852219205B3292E14404' + 
'04A908C9C884E4928C5342400C181010064A1BC07142887010284D2241602393905A88455A186E61' + 
'8445400884139084D4406964140288006A63380E18362E98160C04A7894B964D04844051B0841945' + 
'0612912411B410E186895A00318C129097956ABE59E75ECDE4E7482F0BB70D6502309163C9467EA5' + 
'1E7B467C114BA884CBDF37291400166F7E2450D27C5A20FC1130192F1B482740712446605F49EA14' + 
'E3C35515D05EFB062339E69349C7D416830F426282A4C0C0BF875B0F8D98BCA9B55BC1E199C9482A' + 
'8D5CFEC68166F9545F54F3B0BF14B2117DDC6356966BE0DD3CD42FA47EEC9FE4058A0679F3EF50F7' + 
'306EDF0DBC58D2020702F3C1F0B23A70C4B9023B43E7EC670EABB06869A3AA3D315E9606F0216BAF' + 
'E585E0006BB6656E1C146E2396A44D3B545A1D722B6BEDD33B40433B7A9A0B91DB43A0AE280F57DA' + 
'FF4EC8E534885C436A6745FDF857A093F9067BA95229CF52D0C8DF29C52736EFD4FA98FC4C71F6A9' + 
'4983F8F008F29178BB53CBDD56A045408151FBFDE28941F7D7446D5C2B2CAB3C418E04C851D230A4' + 
'9F288713F3E7221CEE86FEB67B469B076871A7CB576D423B5CE20FDDCDBC6F3BFE841C494E6D6050' + 
'BE92AED653F7B387CBD662AFB7B01401609D66EE5739BC0E9188EF3F61B23F6402A8C60D6C272EB2' + 
'7E3C36ED15A7D95E1BB15C8DF6774170CCBF50A2628F1E80F3007BE33D0C5D86C61BD927BA3EADCF' + 
'000C5B4A1BBC0889C42183A1FDFFDA0FDEC7A2CB5148953B3C428F1153E802B0E60F39EC4CF4108D' + 
'E315B8870E80790E6B67799499A768A60F0E70EE861369F0B0110498B206B01AEC9C56CC335221E6' + 
'75F0DB77B6A2F9BE317407442F152D32E2FA57054C2F05757079D29BF85077E571973C5410743C67' + 
'9AEC39B51163A9B3AF4049647FA008AC5E304F82AF0A0F28E5CE3040C8D9AD6799BD4E70B7CA23F9' + 
'65FE96539EBE04ABAD5DF057FE26437FE44289ECF68807FD2C8900A0D3C5B3910BAAD7AF92BE09B8' + 
'3D0E16CC617607623DD8A2A8ABAE82C48C1B383C13DFFBC62CFC3F45B7A0150E799868D55AB5A9F6' + 
'7EA0D4D35D9D2EB0317768E1F67FFB8DCDC9A6FF9FAF58BC260DE56A0165900F4C54DF209CC148CB' + 
'93469C59CE5D0576BD2CD40BB0D0A9EB21A41EADEE0039D603FC2917366B7F238E75E431ED7DE076' + 
'E9E72701DB873D86F73A4A99A82538FDF5611FC42BA4D899BBEE81EC55E997A697A9591AFCC2425A' + 
'570BD026CF0F3DA4999D0CE7C2238037D8077B3E13290DF420A41771548FF2C2D89416FE77648123' + 
'6B3AE22DD2A21EF8EDFD5E2CA526A796B568AC47B3CD4FFB0743771E647138AFD6ED35AB242B0252' + 
'DD19862AC4DC3310D905FF68DB2F53038D176D22D350401857794DC2E2C1443696E65D7886759A5E' + 
'D653F14D9F804FE55E45DB4090C0E166C01F1FA8AA939B8D83CB3B1B65ADF4B31656CB8D04F21452' +
'BF2B6B36BC18A8EFA7A0E4AB67479844DE5EF231C8E8168BCC252411E67EB3F6598176F4D6739DA6' +
'9AB952CA12B7A951B2333A991477CA51E1183D2769CCF90FA87EA2C62DF0145990C787AE739D5802' +
'E9362E5EDF969DC71FE7D2542A4DD1EBF12A08240BAB3FB4EC2126DD0C0AC740EBA06CCA2AC6ECA5' +
'7E4094ED4C818965105037D1EF2E739DC2D58F2554C40AF69ABBD483A896654C5A26668691A6A8C0' + 
'05ADCA4EE51BAA82DB5D7495E29FBDE6DADC67929A60FA95267A1AB764A1A41B3CA0C3FE03FB5A84' + 
'00130E6ACE93197D3DF50FA789F16BFBE6B292FCC2E9C03C792473A70AFE13935DF76C2E5A2FAB15' + 
'4B5CC5723E5611BB363B621EEF74343CE19AEED62BF771DCAAE501977CD573A412F15FC8D294A512' +
'B7B412F462298713889F6F9E7DC35A0541A2A306CFB9AECFB74C0F94817AB81C804A0508718269E5' +
'691A24A2CFAEE8D0BAA69BB30FE04C45881485B2D67AD09D5EBB43A3508AA45BA2D54CCE4E7FD7B6' + 
'79E735A4D78E8DB1BB1583F1C91B0330D2AD7A80C7D2BC60DB220287413E9B4DF7F68E8A8541C1F1' + 
'F85D15BDD51C6902DBA86A67B2E4CD86B92E7BF58724AC5A53219216B78BD1B50CFBE9B468A667FC' +
'6C4F35A610E7AECDA8A12DAB0C5A658005879E2608EA6549999DE0C396E2E0E19895E77A68356EA2' + 
'9BD2A1F24BE4552E679DFF66EFF39235183E26DF4D23CDD284BE2D9D706A301CCF33D0C85DAE62EC' + 
'CBACC1503246B853603F68DB14C2E0B972BE2AAE6856BEEEF83DE2227AA876E5DC62636E2E95BCE7' +
'9597E01A5A3FEAAFACC205AB09FFF7641BCE3ACEBDC6E6AC2C95276148543F2AA1A4E949DE5F35F4' +
'DC1048824389CB90910D47C7ECEF591BAF68D6A456B79278F356A6C34AEBC370EAFB722F07910337' +
'08C24A3CCFAEDCA515C6DC1B83B85251A2F665B3777E1805FC8F98F9789944139C60BB8AB0BFA34D' +
'3783599707BD44CC99D2E4ED71F31EF7AE6B8FBAAA4A5377DF40F8D6A4C1F720159E57718C78FE40';
  S_MSG =
'DA5FBC7F78116BC8537E8E522DFBB6F68710FC36AE5AF0ACE5CFA3BA4FEBF6C86D966A44C2CF53CC' + 
'C4FF5B9CC4E6403CFF3C23B4F292AAC01E35A21AB11F0707726E88048DD05887448B870741FDCDFB' + 
'4451E1216FA2F89D90D872B67B4BA546C8CBF504A46FC02036FB5B287BC82DB9E2D835802DDE3075' + 
'C7B703ADF4FAE4F98840670964A1ABC61105C3B99C78609605E7F17CD262BE67F7E7A2C6ECC519EC' + 
'DE8D5B76C21FE5C15859FC94382608A15C7E656AD8E0042CA649EB8EEB59B8E266C244591B265B67' + 
'2D4654C5FF28296707815C647DD11AB3148AEDE24E96D7ABD6C479C0C367B00E4648041841401520' + 
'63153EC3982987618F7D9BD9812DF3B95DDBC47D1F96C752C01B742255946461509EF7A7A67DA012' + 
'3D670845BE07725D9C1E9F419B4B9133EBA36BC90DE45666C40EA664D93A16948F848CDB9688E116' + 
'B00B0A4F03B26BB836DFBA93EA6247550FD983371E360F04C9F2793EB769A00BF4049E94F225EE03' + 
'5676E983FC9CB01D15EEB621CC0D4B840EE777ECE351EC66C4B28A36FAF02C42B24804E8126E3852' + 
'735C2A6F6E201662DC18E125210C78197372B4787A8163ACF4B758921AB23ECC4AB8F3C1DB354905' + 
'0E26F9C35594DCC1C27AE5999DB695A19681FFD9CD4416E0C79E3457C0B5144F3993E945F6006EA0' + 
'F541B1C3F473D78F4FED4011E07D46F8DAF7998BBD4A9F5161050D9982DFF28C72E15D158A630B89' + 
'72E8E2B70611C299BE4BD3405350D2FEB84C93E97FE4DFBDCA5BCC9B6708D7E542D7101BD8D5589B' + 
'AAEC5649E1C9844C37C5DB0556BBB6B2EB577FB65909B1EBFA7F1590D53D98FC9975BC06F7AC581F' + 
'E22066BACCA0375AE09DBADC8940E98A38B15FC190523FA6D0CF326D7495B5CC948AC994181EE0DF' + 
'CAB11722E1B46AD6FD2065442D1FA06ADF21FD9187B55320F554B929BC795F367AFEF11A5CE4128A' + 
'E32E6C1662D013A91456AD292DFB8B4C161EA7A4D5B43AF63BE7BFC5EEAB9D1E3198F441AD70DF9D' + 
'6BDEAEAFBD0A293F1A6DF07339A4C34C7375CB6096EB06EC035777C22344912DCCF3D0F8DAED1D0B' + 
'046118A22886299DA5782DC77487CA5FD0D3CFC724769D658131FA415672996CE8E8BED83E057E3A' + 
'FE26703012DFED31C732D770FFC7FA55C59BC3FD68C34D00738FC9E3DBECCF4B695B99BBD6A00827' + 
'52E3A95150AFFE793D8F7FF19BAEFF7744688445E7B561294371E1909035929D654606EED6BD011D' + 
'2CB0512B1A0987827BB07F14E0E8F0207519DF24BA69AB788EFAB21F1BD69BCB14AB44AED8FEF80F' + 
'1F63685C6A85100DA1671405F8D6454FCD7D5FB6656959AD68D3E1896C6EB4967615332993D4207D' + 
'B6B0F5A6E33EF4B2494DCDBA368A052FB8CD48A9E215D30C8C9D37DDAA6B4F3080744E6861B9889E' + 
'23E9D964F75C5BD5E1DEA98EEEA8E66C60DA73ED1E39115EE0274F2747858D5D644C09B5EFC978D4' + 
'496A007DAC35F2F68B1F9A3B43A638F38F663E6124740234119D6E8DBAF933ED0FD5A02FEFFF7AE3' + 
'235C82D90B2F8557DDDC9102FAAF8181E6EE3E9BCEC78E4C61F73FBDA23D4382E5050A0E151E3F51' + 
'BC22347F9B1A19D74649558ACFF8B21AF69EFFF78BA3E83CEB9EA36D4799272E10F56D3C151209C9' + 
'9105A133B665EA51E4A245D37E7A0F84F5222E3A0401E67BFC1BAB6269CA4A87D3CF062CFFEEE0B5' + 
'EE5FBFBE1029E93F46FAE68A91CFBCBD5CF8B78F122AF22A11E77095DEAEBC96F1613911E02237F7' + 
'95AE717301499936B440E07B32DDB09008276C9A5A314889E8B9ACDF32D6695AE2931CC29ED1DAFD' + 
'B61B708B52F9888DA67ECED25217F1A5FC1EE7D482917C7C7E44CA7F0508FD3BC94BEC116A630ADB' + 
'08DDD882A221DC5D2C60A2E9F2F849B3E8B5DF150EF753C35251850D0B4980164F8C07339E035D5B' + 
'2EEC4BEEFB885E261E401BDBA408869951075DEEED3C7EE7452AB39856D240E954FC8A4BEB01A0F7' + 
'9187BB72DD2925E63B3DE9E16CDDC041BC3BE47F318DA6DC6A9AD8710B18883367E374DA58D97F00' + 
'B1E75AC8D4658F04295223C8C4F61B2CF7104492DC7A0C6267F45B5A4A2D258239455A31D265BDBA' + 
'C9FA6E4A1812043C9112ACD4240625F08B901263EC456D2A5877BC9223A6C483673BE5CAB8C32516' + 
'AD5B956688A01374AA48789D53AE79E7FB109632969E45684804DAD54FAB1E94A19C2A9B53BB9079' + 
'F484C64A924C9C940FE5A3ECF29C739E3826311E42B310CF75A47B932BEF104CB27FF35D51E3FED9' + 
'D724D90A17982F2897E2FD09E555977CD668E67A34216637380B8720A593FFC4175BB27E293C652C' + 
'2DD8C506F4D1F551542EDBB31EFB3C1BD5DE6315280BB04BFA53CDC2D38C385C04E9BDF389745ED3' + 
'94147063CE3639ABCAA429C68475F9D466321EEBDE9392A2A2A6EAF53FDE20803B76B0903DDC6C81' + 
'874A81AC147FCFB9FDA970BA5D87E4C346DDB83B60A718882E16F256A0F13969606C43D84AFD193D' + 
'7BA3CFAB875646401547458F36DD53C2182B36B84DE8F8A50E62921A5B011319C555766E46EFECEB' + 
'239297DFBFABEF3AE370CF48A08BC073B1E16A8C840F2604F75F39B3F0F4847A445A15F0B946D423' + 
'210BD32A09A9EDF4CCEAA6E90AD1379E5928B66BD3A02C5E2AEAB56E8987B342EBFD9D3C70EED346' + 
'5D78F12AF4423319B3106AD8BDBD9AA5736E6CC15C7AEE0DC78A1FAB83015CF880DE6249734153F6' + 
'058F567D648AECF70D1BAB8397879617FD6FB06DA1AF6713E3CE584B214742587A5D35C6DFD33AFD' + 
'AAF00AFE00E1CB516C955DDA9E0A5FB78DEF914B532F556D7E72E9717BA49A2D0F9EAFB5F526565C' + 
'58E0FFC1B55138C8DE7A9D6CC06FC9927F6D04440512163B58A236F57B093E80F3CE0F4CCE282ABD' + 
'CCE284057C4DB6314E05EE0A32B5F20C481F8BA4ADF943CA1B0277119DA1BBC4EA1D24CF7E3DB4C7' + 
'59E98753EFCABE2FB5B1FA593F2AE2B7ECEA96B8E0E5B3FFEEE97DD6C50CFE8602613FAC2D711749' + 
'84C2EB59E532A35CF4FB0BFD60B8CF7CA3E7D5C0EEAD822648533D3CDE72C787DAA412FC70674E95' + 
'CE2F5BF58D3646B4881D4CB6DD96659BB1A937B24795B435A103C03CEB6AC85E8FFE9C495082A878' + 
'54392BEA530D61A7F60B3431455DB85414E6C365F9721ED2DD4F7C8A37B88131F5B2FFB5FC4A40BD' + 
'B12EACE61AA956F8724C99DA98DAC18F6D33DB5F4A189454C736';
var
  PK, SK, MSG, Sig: TBytes;
  Ctx: AnsiString;
  S: string;
begin
  FreeAndNil(FMLDSAPub);
  FreeAndNil(FMLDSAPriv);
  FreeAndNil(FMLDSA);

  FMLDSAPub := TCnMLDSAPublicKey.Create;
  FMLDSAPriv := TCnMLDSAPrivateKey.Create;
  FMLDSA := TCnMLDSA.Create(cmdt44);

  // PK := HexToBytes(S_PK);
  SK := HexToBytes(S_SK);
  // FMLDSA.LoadPublicKeyFromBytes(FMLDSAPub, PK);
  FMLDSA.LoadPrivateKeyFromBytes(FMLDSAPriv, SK);

  MSG := HexToBytes(S_MSG);
  Sig := FMLDSA.SignBytes(FMLDSAPriv, MSG, '', cmhtNone, '0000000000000000000000000000000000000000000000000000000000000000');
  S := BytesToHex(Sig);
  mmoMLDSAKeys.Lines.Clear;
  mmoMLDSAKeys.Lines.Add(S);
end;

procedure TFormLattice.btnMLDSATestVerifyClick(Sender: TObject);
const
  S_PK =
'5B003CBFAF3E5166A85F8A45B9C1A4533FF216FB226CFEB83A81A20EE6E97E540FE2E3C6E44262A8' +
'C344330126E881551371383EA34EA2ADEDAD1185908B34905B09FC1E1304BD96225F36056C1B2099' +
'C624E770227D1E7CC310EC1D24A8F034FD91CD01FFDE608FAFAE157C6589DBD5F63DC8F57E857844' + 
'AAA44E0B644E5F6BD684239D145F3D45A8454BE4BB588AEF4245DB3A0BB949322987B9C40A7DBD37' +
'A4526363FDE5EC3778C1F72E85230187A9E7B35028C3EE5CF8AEBE8748C45D50E8E22A81E70494C1' +
'1C276375A8AD230411DB26C8100F07C471D69095575F09A1EEBCEC4C9C0E050B84DF0D4F95E558B9' +
'21DDFC8F26B2067680998E9C99488EFC128D5BF927ABA361FB5E9CE7CE32C0524EE88FBA0F3BE5A1' + 
'A7C55AE6C518AD4C7A33C05E2956CFC8DF6EB2A81FD1EDDA40F67AECFD715E4E3DC042AA939ADD3D' +
'275AE72CA7EB85F0F0B38884D0D7DE81CFC8487411F8A84247B82B2BC28F76BBA2D80BB7009697A7' +
'FE729DC123D11E695E60C024FD31D5F94F5C3CA6B76A13B7537FE6402DDF86EA6D8D77718D03B325' +
'05444ABF4BE76C01FD43CC86CDD5736E469DAF3B9A239DE67FD4C4D99319B0BA690C424DA2F3E688' +
'31E5CDBA77B49F20D138A3CAED4EAEC8968F33169C3A2A7A28B39B875A6C18F5A3A7F49E6AAD46D4' +
'75C99FA980BCA322FC69633B576B30E1E98F771412A1267F0C82653562F755BF39DB930902358705' + 
'98E405BE0F8A58F033F19BBDF126639FB85D6030CCBF4666384943E7F4DF69EA767982D82147D8F5' + 
'2955EBB3D3A8D90664AE9B9610CE8A8F66454B2C518BE42853BA93C434C8D9E072726BBC8EACE2A2' + 
'18F6DA2639737541FEB2016E3478E3443442381A4AF18004C49198D2A87154DC6D8975BBE20C22DB' + 
'DC1B95DEB7EB74E61E5AFF999833B746221FA13BC442F9F25C6035861B7A5E75615CA6749716E8CE' + 
'F56CAFEAF04CCF6B824FBE295C55C2D796AAA7992C49C4903D362AB50AFABC4AEAF6644106ED2F74' + 
'9CAE8C970D4D49B79A98ABF6CC3933563E499C07B52B80F963DE9A5C54FDC1EC4898B7713630D0C6' + 
'F4CBC84DC245EFCF821FE382E0FA855AD32E3B70D38772341C0C3000D2595F749E26C5EA692A0BD4' + 
'2251A5A9C5653B995EB6B83A2FB8635C0EC0E29F9760A4684591D0CEF71D46F529204BC56087EDAD' + 
'2D37939040F834610B9BD3B616C87E43B49E8B38D21B9E8B936B4DCD90D73C12F3F6F66A111738B1' +
'6A2EAEEF8DF66C61E78D29AAA2D1378DA0039ECD56CE7231A5ECE7EC2ACAC469A42B9E323AC420D8' + 
'6A100DCFB78B1716A0365DA724EE0EDC9C4E1115E3BE71F4FF0794A10AB766D49E11EC1DB5568166' +
'27DD54BFCC22CBAFB4F2A2568192638B9789E3020A2802A4B90F9DDAA36F650546D41600B2F33A86' +
'B1B10E802EF9B7C4DD1975970A930B0CB383E3916728CE1E2C698DB19AA027CFB620D88840C51DF1' +
'7657BEFD3240510D95A0B6C480FDC1A1346E00C195CFFA6BA7822625F9A47CA029C29858F07AC0A8' +
'6F4A1D0792635F82C15D5DFB19CCC415A2B9556AD67B1806E8AF681827B1C465D8646B481A00B7AB' +
'68C7CAFC623656D2FDD9431EAE0171B96885140196EB7F1ED76BE72AB001CDE7CBDB220DA5EECFE8' +
'D341EACBE12878011CF85681AE8F4BF0A9E8C40087FF51692207C57E0BD3C1F45E90F22B5D139008' +
'CCCDF10795EF0858C5DC3F6FD9F78858DFCA5D81510A9682EB45D0E094DEED0454101B0B28EAD2BF' +
'B7CF1230994E03BC98CE986F4CE7720577DA9C8406BAEFCB3F2A8B9EAC2D1A5C598A076E52BCAF29' +
'51CF1D4339757F5C6C522C3BC4B93C71E47B639A6FB8FFADB117C191B4B9D0D9';
  S_MSG =
'DBAEDE95F7793725C9DB980AE6544EB2E2C4FC165C28A12B6EE675764F020C01C048BD0DC8064612' +
'E4B6858FB6871F71D104ECC4AA0FB27B9B79D1D95EF34E1072743826CA9E4AC0F1DC608D75695F1D' + 
'39B5BC2B52758ABC11FE8BFCDAB36DA01B713B1434B9FA141ABA354EF1C50220757425B486682DF6' + 
'4FD3C584DFE147180657C15E6E21A9888219BDAEE8FD883A41177A6F6537F4DBE6809A0334D54582' + 
'325C80119B6D4B37D45CDFCE93683FEFDD684F180119B88558D4737FCF1815063A06C0D0CC2F653D' + 
'A98C272883B71BF463AC57A104F02C1944999E3788DB99F3F26D752F8D286049D0FBEFCA4BF5E1E5' + 
'765FD0E3DDB9B72550A725DD96F2E017CC99937812D037FE476C613541DE88498A2CB72DB2120EA3' + 
'232629709F551C4134372E58BD6EDF8366FC5F00DB38DF6E281962CB5C68FDA2CC4EBC135D438AE8' + 
'4E908E6DAFF39AA1A7E09785F8375D3E9950041679E86DEDC7398798EE624067A7D31E313A509E16' + 
'BC25564DBD96F7FA811A6B5128819CB35396FD2BFDE8200EC146192AB727516FAD0FB85613B1C922' + 
'203B4CC0617E076BDEFEA2A178DA9CFADC2044A89FF9034C23201F11D3C8B3EB98BADCB3E767812D' + 
'8F71733885B6B6E13BBDE5811CA2DA120D8529FE5EBB21910E25ED49364F8E17EBD49901C0F23504' + 
'9258C97BD24186E5BC3ABE0D1FE6C448739760CC586BE39DBBF9043FAC6DCD5AAD1F5CCEA91994E7' + 
'5125F24DE6CC0495C2017EE37D35263294D1DCA2903A571D3511A1E38E575B0C1469E0B02ADA0EBE' + 
'331CE290DB49F353C1C7ACFBD25C715D7B8154310B1042D73CC78145752A93B07BE7D1125F8B122A' +
'38849CE7AD7B69D0729822333DF209EDE90783CE95039E856002834F09BE1F41C213012B9569AB2F' + 
'0AB29FFB084BE293B387B823F62E14F0F38DE03E4DE40F5A753C71A00DEC36750855A1771A06FCBD' + 
'8B8448C67F08806812B72FD7C56EE3FCE1EBE2E2DABAA9196A2FCD9B4D479D553229D7C69B359ED5' + 
'3BD7132A2129130953F5EC0753703C202649F6D218776E6FB023A1188ACE6FDC49FD56BAD40D7936' + 
'ED945FF0C5403F24377EABB1A3D97ADC8916EB8BF67B7C8DD0A48F8C3E62BF1A12A009FE4A3B3C6D' + 
'7FDB87F64FA200285C6DE922BEE5C5CD28C0CEA9ECD6740C5966EE76C948195B626830725AE7D048' + 
'955339A095DEEEF7C9DCDE9EE169B2FF233AD7213959231E74BF2132CC60566AB84806910894A0BD' +
'2ADFC562DBB4F64722280FFF3DCDAB54D5F96826DB5AF6BEF32068343A5F22F55FEA30A417C76B62' + 
'0BADA6A44B09228136516CBA30E70ABD4CDAA603A0FB5EC5E1268E47665D5AE9FF70468A3D19283A' + 
'5276ECA45A847FDE13E3446F1F17CF057E581E071FBA06AC4321880B820C4E01329EED052A67ABA6' + 
'32B73896D0BE7C6DAFB5BC674CA11FAE0F1AF7A69CAE1C43A81733186902192F06ED2C73864D68B0' + 
'584076353DFD8FA10F0556F8652C04520EF5712EBE2E4B9B4E62E308DB848D58106B1EB82FFA1DD6' + 
'DF689B1C92C62C237200A38BAD10F5E622C9026329D48C0BEAE51BAE803DB0FCD68FAB0E0C1E00C8' + 
'F0990D09D44DA36DE0C5B8BFF16D18E1AFCC465EDC575C9381334103B0098D209141F870E51D8081' + 
'3220A6408B4287C12228BC4403846E2A687A0DB1390BA6CBA6F0A16ADCE5E2EA6BF3FCEF7DE0DA09' + 
'15638916AA437661F278A2652DC20FE96F84C8FB76612FA8B11D6E2FF0EACA6F1B1A4F680B453747' +
'1C24CD878EA34D725E6CB37A75F97DB8A4AB052518D798B0A8EC06A7094ECC524251CD06F5FAED94' + 
'39299B0E988371E0AF0BBE7E97B1B601068BA3950E9F000D50CDADE4018A160A96ED4DB2FE500ADD' + 
'EF749E1056FE8C6C9CA82E8D0FD9FA7F5EFAE2F196475BA73D4031E5BAECB0A83575D203D8CB9D6F' + 
'49DB3B6FFFFA0864FEF2847BC36C3DAA3F19987B54784B84BCB8D5982FA6BB7145FA5BB9726813E2' + 
'4647D70C23401FB7FD4E1CA26D7E43E5FBA1E27FADBAC64D1B8004C201C7D29336DA4BAC7A18A850' +
'E42FFDA6E7C586B94021E3FC989864E2A40EF0233CFA7DAFCCCFA519FA6D8690D6DFA6948BD88D90' + 
'4BB9E6E9B9949A65F13D8F32F910C40A5410F8D71BCBF7B71716A73271D8355132776E6C56748CEE' +
'9ED6226FE340C704D1FBA5F58204AE56673C148B2C6C7D94020BE854D49F0E3FC5CCBA6981CFE318' + 
'0521469E42FA3B458922F9BE4E23EC93324FD73BBCAB4C43C70E8FEA1D232A92FCCF4D7BEBA195D0' +
'24C67A66E93F68618FAF32C943DB3622FBD22CF777084F54B638C774D6D3D8C91763C20693A92712' + 
'456D42DF9D2FEAF0ADB1A6D9B4D500AB899A1AD2213A7008AA21E2FD7D00879FBBF765EBDC7526B8' +
'FA2D13BE83FD2717945670B5D73E96445A948AE028165CB3A73D3F2541238CF40C1B6EB26F33FC8F' + 
'69D22BB899CEBCBB739BFD073EFC6993E221BA2EDEEBF35922FABE93B254C438A12A22E0BCF74D14' + 
'9043BDFECBCBB6EB1A55D928150F601EFC4DF082322B83C8395555135CA1936D690CF3ACFDC19E51' + 
'64280905E3F3C5F62155184714EB9F61EEEFCCE338BC0217045AB2910F4E9DA330283CF93DB2D0B4' + 
'EC2D81877266785BBB52AB0F81E0A06A7C5B736C1F58D234094F74DF7EE550C03404E9A192EE6316' + 
'3C079DF3C5ECA1214F20A2BC0683C66F22AF3F1AF532F5FD828EF3F2F9FF1724FC2BDEB2E7F706B2' +
'A219A2188D828D57255300CF6E29A7B992BED0D3BC532719B475A08D759CA7A888E0895683027D9A' +
'3AD3FDDA5F021E5193844382B1A963945AA49BCE0CF4231133C585807E0CA86D4779564801C05F2E' +
'474DF49A9C0AE7000D65B35B0896916CF18D584B24FC41A29AC67103A240025D316BBA272ADF68F0' +
'6CFB19012F846DF8F5AA45EFC76B89FE08EC9CF0DFC7CDC63A392B83ABDD51DE93728996C9981205' +
'2E83DA07DD3026A15E4186A19D2A7CA6232C9F54C886423B7ED35F9E8A97B804F401AAE8B3B7DDAD' + 
'F5D6851C4C4DD4FBC15D55520837123C11499A6F6B4C024F457C3D9AF50D7D82E31F433101834930' +
'C183DF8427922887CC36D9DFEC570C204E77E8C54119EA5D9210F82100E1C1E846F763397ED5DB39' +
'B7C086F3730617D91413CBEDB2AADC91C95B7997D45C8977D8D17E9BF86B97E3E388BBBFCD126421' +
'6C3BA773030FF49ACC1E79B2BFDF73C890E68A1EC42A638D2E5F57';
  S_SIG =
'E98901A3F79293983D935DCF3A4DC9BA8966F70CB2991E6E1E5942643D37A1523FA43A15CC894A81' +
'285C4BD0E5063267D317BD1EA3E3A2F0AEA6BFAADF074926F5E522140F00CA40BE0C60D492C7BF42' + 
'0EEA18F2C922A0570FBEFBF07CC3D084793D4D07F9FAADF386C6D1C24AC98F153313FD1C5957D02C' + 
'884B21CED24B3BC5148F9F837EE94E9FD03371342353D627FB43FEF9472C832A1DCFAD355DABD974' + 
'AC185C2FF7A7CEED63124BA86696F095D419810E381FA62B66C12D2B75E8CF59B0CB76E9D0546D71' + 
'D9BF154D0D056603666212ABE8910E60C08DF738EF0EE8BB794B35C6E47E7BFEC734A64519099720' + 
'93BC17E080518053EDE94905978F397DACABE24A6C4194F0305C0F6839F8D9993A6F6948B9EF3984' + 
'A90B1C3B2E9991449D1B1BAC5DD54A2134608851662F6F25E8ED9BCC65D199A770F20D2A28C35BC5' + 
'6500E4BBF0823615FCD07452B8A558F8146081861DCA6A6EE9E24C688DC7367BEA1E00181D8F0FCF' + 
'ECD8D46A607C08CCADC32499BB73A9C664CF07817C7D496CBB428AF3A06ABB3C4A140C681CF8A6B7' + 
'216A64AC4BD3505E4AC5C08C076C9F34F7DF9157BA1141628837B0F3BE1064D3F4908FF0DFBD8342' + 
'6C90ACA12D8FB9C9F9955B5959EF6DAEC31140CE117D9448C813F2A940D54DEBF844E28CEC621CEE' + 
'34A9F336C0294572CD03AE5EA6A9B7199C6FCD9E20BE47214FC0F7E0125A6D8B8C397CA9D497671B' + 
'C41CA08CCC153AEE53ED7F52D678E2C1873D70ECA96AB6A43ACF1786BB018057F98A4311751CE6C3' + 
'D8B0C4601A370AC792B081CAE7581AC836F0C0BD3F11D2A78EBC5E268F8D19355F3B4995B57736C0' + 
'42F1B7B90EA55899011DE84A2CD0F6BD440D51DB2A6BFD93E5ABE620BD0CF9452ED0C5F4EEE9B724' + 
'B06FE1981A16CD14D554859ED4A1B205E4527B66085587769ADFEFC289D07B09CABECDB95CE52587' + 
'B7BE95D6D5068C05AAC009CDDC7AB550CD49C66EF55A86EA04765C46E74CC3ECB02650797966A6F1' + 
'25DB780C3C507C2E2B4EEB148B243673FB5FD2865533CEDA9938821221A01A8585EF76086E429A80' + 
'A99AE31A2A6D4541AA3393FAF0A092B3744D8C742239C18F7AA3FA258FEE01736D446785BB9C0086' + 
'B6BEB05AE13BFE5E5C462A87C35CC5731218EF2AC902B4505A593457F553F8F4DA1533B2B144DB83' + 
'963663C3EC991C6F834DD9C75B21E40D0060646E323468C1D44F6071B6135CA41EADD17F4C36FF21' + 
'C5DEA01775FB519597D4F9DDDAB30CD63DB37E8F5C0620EDCBCF2C84595C8DE1E10EA2D8D7A8A2B7' + 
'D45BF84C0784392E0C7FC74FC8787C118782E57802DE412A1E99D002C2793AA614DAA5685C5D4253' + 
'FADF0A630D1150DAA8D8D54F4EE0F6081E59434C6664A933167C61262FE7A16E3CFC18BF9AC42C49' + 
'C5E0F9928F2DEAE020F5E105BF247E4432F4263E6BE8ACE9D8D8269B215E31782DC83B0805538BD7' + 
'5503C2DB6E90E99741D120951598493176499DFABDE262A16F28EA1FD183485478BD4E906DDA0FA0' + 
'37A7061BB28C7031BA4DE21CE4B1F736B1DCB12ECBB755A1DBD11469A924F417383E0B0AE22739C8' + 
'888477FBE24336C2551040B3F044B02F28D4D5C3CB4852871DDA6584303882AAD3DEC7D6436E0089' + 
'F2E2E4F6E2B5FF628DC565CAAD8775C1DB24CE7FEF3FCD524821CD58E43443D1669F3E4DEB508C7A' + 
'2EAF0AE8742C06C537520EFFC894EE1F58EA2D335E9B8986783CFD4E3687D966A03BE25A8DA8F856' + 
'5FDCA33EF9F5E8444F0966F73952354A985B61C2A4520F7758721FE9E667EA36A746F999B6E19067' + 
'47555A361CAB9603A68795B065CEAFE7625F3EC8CCD9589C9E4800AE2ABCA461734BEDF46582E282' + 
'0F129E4030D6A2E967BE349BA20DDBC5DD04EF8006351EB39AAB145E0E5FF648491C016AB7CA26E5' + 
'789BB184DA0824F699AF597ED6A252C7AAE5F60B0119F74C439AF414545B2C1299DC546EC0C66589' + 
'AD70C2DB8EB517E3CE2485C0F13D96E699B0C7C3AE607619637FE5732A31DBDE4565A894478A00DF' + 
'E78BF559E0E9BB65125B1CB5F1A1A221C9C8BCFA3D294646AA9E21F4EDA1825880FCB7E1A4B7F425' + 
'22442697A677ED9F3777F8CB8BE61DA12BB182C3148E9FA395E95311C0C374698C462407FE98E188' + 
'FAB09057099915732CFE16DD35909F912A42E2C5EF0059139DF8AE6D85FAB29B33973E40EA0C592B' + 
'066859611BDE45C96F2397FC0FB5F07A2E8F2177FC2C8EA2E3F357B5617892B071F6366488D0FA27' + 
'FDA31D674997BBE9B0234459EEE7E5FB606B405448DF0322782B55D5DD83459B55F6041E62D47E2E' + 
'A4389A2A29F2C4FC27CCB0AB9907952C9678650C57C0B909EFA205CA27F25C13FCA3E572E484F0F7' + 
'EC947CD0955C1020FFC6F1612BDA856C8E540E6A41EA9D0A84C7E9DC6255BAF32C3D135A299F0035' + 
'4463C3E1CC0B3EDCC309F8F046BD329A19F9B747828807B958FFBB7D6F05D77B3D354948CFA9215E' + 
'A79DD62E0EA95BFED33F9BAB1B92623AE9DBD4512B4A4F6B8C08A70CDFFB6A91609309752707C04D' + 
'183651191B1C3964D6AE450334E0BFF52F7E1B64273955056E944D47041CAEF51385F01F0ECA4973' + 
'BF2A56FFD8ABD679BA6983161B7EAB18758A5BD62C61693BA3C724395F5FD69D94E242C6F174FB26' + 
'E7BF0B089BDF92FA03C4C1E663C99F2E22A23ABC3810B0EEB6953B1EAF7F2207616400CF79E530AF' + 
'203873BCC0DDDA656BCAE9FBED9B20F870877002DCB79FFCD2CA91266FC8F8EABEBDBF71F7032EDE' + 
'F77C3FDBBA35161A45C22F1248106A4AD3B2FF1132A24891FBD3E79DFD2E0F44F4CD5EFD5D07BAA5' + 
'ED8E13A406FC9E813460FAAA196E4DFAAAB4A8F686A59ED6753DE8A7DFF287BF24BFD60FD7A84A43' + 
'1F80D84062F4157A4B0712B89A3743C80FC0B5D8BA8D4A6A6D113C029F5579CAF30A9E458BEF1903' + 
'A989753237CBC69109DC435121FDDD10892547F41EF4DA88F35CACD6F5199E9973682440ED4AAA0F' + 
'98106CF86B30766B31C55DA18EA8C0FB67B0C937FF4727CD8FE157F6E96E73138F1BF394E7AEA816' + 
'2CBFDD548F3F4633CBE99CF91A9364D4C93E6ED58167670DAED4358D517D9CD0C9B5F771EB2F9198' + 
'F91D833CD92ACBE244C6888F9577BC8CB4B514E89C03DF1AF38CD355426A21EDA4EEDB8791B41BB1' + 
'3ACA2A5B6637B9985C1B19AC3D5004B177B8DB3844D3A7D0573CC2F4237A712D2D4F42D3429C13E1' + 
'89913EFD4DC4B382B8E14873C8FB7D3B885F208DAABDF186601DAFE85E02D2B47DA258B5D554C2AC' + 
'FF9D12C2260AC96D5E150C98A226E212151A49788186899AB4BEC6F707203D48526F75848C9AA0A6' + 
'ABB2D1E200181D2C5758606873777C85CDD2DBDEE8EDFF09162527435861626AA1A4A7A9BFD5F900' + 
'000000000000000000000000000000000C1C2F3F';
var
  PK, SK, MSG, Sig: TBytes;
  Ctx: AnsiString;
  S: string;
begin
  FreeAndNil(FMLDSAPub);
  FreeAndNil(FMLDSAPriv);
  FreeAndNil(FMLDSA);

  FMLDSAPub := TCnMLDSAPublicKey.Create;
  FMLDSAPriv := TCnMLDSAPrivateKey.Create;
  FMLDSA := TCnMLDSA.Create(cmdt44);

  PK := HexToBytes(S_PK);
  // SK := HexToBytes(S_SK);
  FMLDSA.LoadPublicKeyFromBytes(FMLDSAPub, PK);
  // FMLDSA.LoadPrivateKeyFromBytes(FMLDSAPriv, SK);

  MSG := HexToBytes(S_MSG);
  Sig := HexToBytes(S_SIG);

  if FMLDSA.VerifyBytes(FMLDSAPub, MSG, Sig, '', cmhtNone) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormLattice.btnMLDSATestSignVerClick(Sender: TObject);
const
  S_CTX: AnsiString = 'CnPack';
  S_MSG = 'E5AF86E7A081E7AE97E6B395E5BA93436E5061636BE5BC80E58F91E7BB84';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK, Sig, Msg: TBytes;
  I: Integer;
  Res: Boolean;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(TCnMLDSAType(cbbMLDSAType.ItemIndex));
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    M.GenerateKeys(Priv, Pub, '821229E3226BB7A7774BFC3C3593DA9C4DE76255374CC4B13F9F62860E0C3E07');

    SK := M.SavePrivateKeyToBytes(Priv);
    PK := M.SavePublicKeyToBytes(Pub);

    Msg := HexToBytes(S_MSG);
    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtNone);

    Res := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtNone);
    if Res then
      mmoMLDSAKeys.Lines.Add('Message OK');

    M.LoadPrivateKeyFromBytes(Priv, SK);
    M.LoadPublicKeyFromBytes(Pub, PK);

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA256);
    Res := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA256);
    if Res then
      mmoMLDSAKeys.Lines.Add('SHA256 OK');

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA512);
    Res := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA512);
    if Res then
      mmoMLDSAKeys.Lines.Add('SHA512 OK');

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHAKE128);
    Res := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHAKE128);
    if Res then
      mmoMLDSAKeys.Lines.Add('SHAKE128 OK');

    Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSM3);
    Res := M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSM3);
    if Res then
      mmoMLDSAKeys.Lines.Add('SM3 OK');

    for I := 0 to 1 do
    begin
      Msg := CnRandomBytes(330);
      Sig := M.SignBytes(Priv, Msg, S_CTX, cmhtSHA256);

      if not M.VerifyBytes(Pub, Msg, Sig, S_CTX, cmhtSHA256) then
      begin
        ShowMessage('Verify Fail');
        Exit;
      end
      else
        mmoMLDSAKeys.Lines.Add(IntToStr(I) + ' OK');
    end;
  finally
    Pub.Free;
    Priv.Free;
  end;
end;

procedure TFormLattice.btnMLDSATestSign65Click(Sender: TObject);
const
  S_MSG =
    '4D3C4D952A1DAC151736AE9D0AD81CD37F7C492539FCC916A4B2251309E06CED54D7145868D145BC' +
    '8D16F8B364E5D6026E113BEC4824F6BFAE3C9F6C8B888EC6B8254CE59E9E8E158EB04077841243EE' +
    '3A89D25C0B30B4A64B723697330AA87663B20EA4513189558466D264C79088415EFA09DE9B64AB01' +
    'B640B288D1D5562ED744E80CAF0B6E2C29FFA1C321129EBC70CAA73DCA7DCE899571260B2E7DA1B4' +
    '7CEC5BB00FD66AF8B89BB84392897BDD1A5EC7186670DB81604E0B949B5AB1D19522A3F0828D0E77' +
    'B300A63EDCB74C48F9A01CA91816FBEE011A5B0145C6A0B4EFD5A3008A91E0B1AC96DD8CB9410D56' +
    'A952E74B7FB684A0B166E5F0DF9AD100EE1C9E3B7B851B4CBF337A3328DB13977451E992C4A0B454' +
    'BAC5E3A2D0E64D106DC47C216E0C0AC11B0F7C48589E573051133D01F29579C7E659F8FB8E6539B0' +
    'F618D053F4323AD0237A58949F689458ACC109BEB7C81952A879A10BF8BC01CE057E936B62017E22' +
    '6DD66AB2470852C000A618B9B130050F6B047746D068FE7D3CC1277E4FF4400F372D50A85F469B4C' +
    '7A35AA7FE88DD923ACEF5FFF47A95F971EFEA6B65D33E65F20173C5BA5A320DD16CAAB59AA7A7BB9' +
    '9956EB5ED54EB93E89C887A30D6D384345E21291B8E9C6CDCBE2671F810A058B858220146A2161B7' +
    '412CE2DB817CC8C99528195F567A92E198330AAA418A5FAB754E27221A2165EC4D50B391BB8AB2BE' +
    'CCAA449CA1D4D3D5C17E28573048452EE4120693505515CF8D1E2F8984D363464DB786E9B8B7BDED' +
    'FA2301A21F3C469F013E27742405CFD0D15E70BE0560E8B18C82327861A8DF64200A51E69D2791D4' +
    '1E1E12BF371664270AFBBE1C84FF19D11D4C233ECDB8033BC79BC7967C410C13E8E3F9340F848510' +
    '760DABCCAB6EF7BB87398B1421E5804F6D41ABC09E6542D72B87115F1281450C66AF3BF4FDC21CB9' +
    'B8C27D0FC6DDFA73732A877CB7D7C09B8E9F4603D9865E4AF91C58E9EF6FD9A32C91EEAAEF170BF4' +
    'F44BF586D789EF4571DEBE6FFF0C43E842617FA50FA6EAC8C7F70EB680BE717688188A430484F338' +
    '8F6219A70EA38BF3C6DBBC7FF3A749D8C8E51A1365AE372988D248F9FC0F2B67E896CBC86D2AED35' +
    'F7AD8D4E93C21DA09E0204C7DB92FAFA7119A63BD0CC00A029CE7DB93DF34C33ECF673D48CE3F409' +
    '48DE86CA5467CC159960B329F560B20E5C8AC37AC4C8960B49B455075489E7217D6BC53AEE7C19B3' +
    'FDE9FFF667EAE89AA6CE8C415E807B348A14598329A32D9E508C3409DF18EB426BDBFDDD88AEB3A9' +
    'BCB9D84C2388AD79AABFA3445F80A9A68EF74096E34516CB5316DF599E5836F123271CF729472952' +
    'E06C817A67C219382F22C4B0A16F7B7DDE9ECF4AF6927BBF4424F43FCEB1A48F1F2CBD98E7593464' +
    'EBDE45D604B6AF95E32AE728D921B390F712ADC280257F4420741C0D29E0E6B98EC1708569381BC8' +
    '45420DB0C1BDC9A23DAEC209378A01703B9C16C10FC29763A52FEE64F0A728198513361CD981DAC7' +
    '39937FC472CCCF666E61DA3D90C3656F0BBDE6B942D8A0496C9BC703F7424BF00848800AAD519347' +
    'A4747676C81088109A2B67848EE0A08BD1409E774434EFFE754BDECAFF2328EDC30FDB85B45488DE' +
    '0A5D9052397F9DDB0F13865DCA85A36F6B43B84ED044E5F40BB960E56DD0CD6EC08C37B017FEEDBA' +
    '4A7F20A174E1146C441DB99A2BF34BB8D8FCAEB874B2313AF2E20B01D38B56B42451047EAFF59223' +
    '6CB241857754F6E2DA3E4FAD070FD195B641567375B6F2744FE16996D7A73D095F5CFFDD50A96508' +
    'FBECF802625E5F168DA08C8D5198BB2702F1CBDEA8221BCFC301195B717A0D174EEC9BB96E7C49E1' +
    '5CD2A26B933E0C6BB11D702F1A21D33BE48039E89E38C2A5E96F55A557359BBC8E191719EC5E5669' +
    'B2F168912C9177B320A5762474CB2303F54E78F4DBB914EA354F8C9A3E81342A10ACF5FD4A3D8CD6' +
    'F85A204E165D59E4FD12D5E5E900AC94DB984DD8A5A72372D8B5D8958EC7F9E64CEA4E560A8D9525' +
    'CB24D3B2F36ED7D46F897229A776E7E422838D4E2B551B28D0127F2B7644F35FBC35BEF9D856EF44' +
    '7F699151F73AA70CEEA55F6193060762B28FA150A83344A2E23B9E5E1B103805DE85C520EACCFD67' +
    'A5C390334B036028D4B696AE3D6F6DD80304F7CF915F564409AE65AB7504D8FED434966AA4D4DB3A' +
    '3C90B160332FB8E6A33A392122E3C02F27C38E37B8B217958912D0AB987D232B65D7FDF3F28CD507' +
    'D53F5CAACE75224AA3F63A4D4570627561AA146A32645C89BA18CD4C7D2AC0D9123C89AA7D4AEE9C' +
    'F811DC877D7D0321AB5C7EAE86F2EBD61AAE681EB53EEA73D0D7532937AAF8F8A23E0AE64FCBAF78' +
    '2169422ADB6CE8F3F003CD2EE9EFC37E5EA1A8902D3D1246A89736C6C7C9A46FE2D2DB3BEC00E691' +
    '66ABD213A6D30494244FEE28050BFAE566DCC91C9528883011F215995DC92E577421C9645478FD3C' +
    '3D7D8FE06BD1824AA20B4A67CF9E8C2A69C031E4E1FCBCBB6170D60838C61AF3731C7FD4DE4D6277' +
    'FD6652B63318CF093F7DF46D800368A71333BA657171ADCE4D360E249B32F49F93E38C22ECF79C89' +
    '647564B7F83E26A59D403B239A25F7629CEC50EDE1EFB58D9017ED44CE34A3136518FA009C8BEC05' +
    '7BA054C794EA0F49F3C581FD91009835946742B806D115D4E2DD109E1797F7DB5575CF233980B2D0' +
    '72D247E704EF40A6DD6651369C67B576FA7DE4C794E469EB2DB8C0E424D07CA01753671116FDCB9B' +
    '37C0517686D85111B8E12A530C25F0D01ECB20E259CCCDB0A081ACA5CD06E9FE50D79B9A7412E02F' +
    '41B53CE3555E986833D253532CA5D27A4F0C72FB698CADE01B36CF94C5A05907AB3AC631455B4AE9' +
    '1803F2A3A83E9EE23EB0538840E1C9408F73205D7930AF7CAA7B4EEC2D47A71B99C07B09FB4BA1EC' +
    'C036BAAF5230D600550CEBFCAF49A19D63B3B50016CC8077D97C27376E2AAA370BB3CA213968D242' +
    'C11AC0A341';
  S_SK =
    'D26EE59A89F67C98B20F890B03422C8027B776FC305BEF422CDED403AA705DA5DC380AD909E3C13A' +
    '76F7F59BDD27A843F5F9A1B8E5B7B524EC445A916B7A44A15167716BC609B3EC0C653B210C6569BA' +
    '2F8233E7F6E404B92EB21F39729FEBC06A644087C75FA7DA3DDC5B85A89F55C9F87A15014E27AB32' +
    '5EBEE0FE472B2F562058448603825340188288664414765756856412036480876655851836255165' +
    '55420058871751602717711246115635711801273601127806468005021161768110481565564313' +
    '80408615622477525336508572667670254212877701521765638422146616701854171256077565' +
    '33053567883520673465437273384264714252814150385332026561288516713001177555134523' +
    '07351315370854301872372157587303156081110321112258736404438264705244308745330557' +
    '35845516047300453165044242772430375327364354554615124108154741243307824331134370' +
    '37235185813885732348830025241382031188181067127262750051644858467034354231803160' +
    '86652647174338637385102777331112452328027048826136833604027623520567678561007443' +
    '30011802641776217247041410056768423675527675026503880702365431353050020174633258' +
    '67621376867004312242037778684262346075013758076061186436433431828544871180673757' +
    '17216763686234631445727470521225615865012624304163885146650645168324156757125027' +
    '63176118137833536728642640525817846376704674434640538555860741636531231160765578' +
    '30326141711180364263331826242367478648076563538232465770015765476015361372456481' +
    '67881548355703441616635365661535127864121615460226475308628856173321321610874562' +
    '55350678472373538538646305637023507323682268078360636557516737242348137027378732' +
    '17062747566405158327553175244176435271317827767330236265632262843444146648141807' +
    '51873017473606630568447604505822478245472545717704487614714872060204431705604477' +
    '51262275520875411164411453355505707067401375675661302860283461350752870564321823' +
    '64563783373178816012647627527210643346750274661645473810664666543245758255208826' +
    '82277558861126808652842067242706502602822135413806761150412241623142132062854561' +
    '46200528246352705422206708423246383470302256330312064070061302824818613586226780' +
    '75145170348088718870286513766033570534713568482324601237202366236048681570881173' +
    '31708032612544633466027487881011822577840213584603838688167482874577534023608684' +
    '71755117806087860580701360737114686181011145211825285172754666411644272571866662' +
    '77455657061280633463378371258505684448715213474712362663501616743060832514154480' +
    '31242478881077307231468143328543380706263487802348512856658522881606460085147711' +
    '84766026578721408534481264011671368670154401888250860602537543640048570478886834' +
    '45675837403868112564208678771660777124321362883310623811784503765223161535201537' +
    '26880635206125157517874821078632831518110002578434628677277256811181641133083303' +
    '10847404188582131517723703441574026287773553025215566368362105036381238206270354' +
    '16360720378881306514377274531642557775043466253742217176663553410214616423000181' +
    '61635551232553767782843483040236111880701245700154018276321853454261144802277261' +
    '07201665633614706250085010708611155106700522786665481255156668041404261230737031' +
    '68340012065602317066686416153178574253053134027687580310663468125022141878120167' +
    '74121074448086641371280564240151566227543442570306101083513862873685631407576010' +
    '245281712830883456301434255351012FADA607976F6837B0DBB68127C5D2CCA1E3BFE9CE0441DA' +
    'A2FC7F0145F96BFA473054009EFB11184E4718B045758DAB9E41CC03A8A581F86F05C9D76F3BBEFC' +
    'EE4A67DCD2CE920CD15B7241D7BF641B462A72E287C3DB90AFF454ED1C8B0794AC1055BC6B4BE816' +
    '69FE7154ED8FAB43BB21E2D787FED316B81CE03DA5019A0948BF4F19CE09D317736CF91FDDD4B42B' +
    '29B9AA0C87D58C81D2A74F22F44AFEFDF6DD0BB2689BC4E54C42DE56F5FC6796941631931525D7B5' +
    '1FED90894F88B20BC819BE6F19D080EFC03E23CB034D06EAA41D552CCB07B42AE79486825EBEAEE2' +
    '2CE193D5B9174E20491E23F376E3EA9731981FDEE6E0C837A07C9A6575A038522C8F36FF0E481625' +
    'E1D693C69E2B4B4910B35CFCD7AFF76301F15E580C5F2E4118CB9754D90ADC4140E4AA8DA02FF750' +
    '2514652F07D14B0AFDD6DE58EB896CF51E562E0B7972E85D334EE77E09E7882EF080275786B91E1B' +
    'DB949E94BCFBE45333D0CFD6305746A24B4B2AF2311F964151D4F89D12D1D88A6149351CB3294926' +
    'A3B05051FA1F613F4854A1727E3FC195F4F5F99D7A6F6766A250839C090B4D81E71C242A58DEBE5D' +
    'DCA556B97DE9790DB89ED15CEDCF7EBC6D44DC61881BE060F905CBB77025694FB382A66A2809A759' +
    'AE66DE102CF059E9339C26112F76C50B39EF61ADF393B92B69BD88F6AC5E7EED1FA1A8BA7201696D' +
    'BECC8FB9AFC1946C744D92F3ED30F8834C398F42C53720510D16817A8074A2F12D27FF98CFCFFDFF' +
    '7D94FF793993991FD44C799D8BAA5E0BCC789D3F3EF467104BDBF2092509E530F920DA1F663A5E58' +
    'AECA50EC6F1DC82986D37EE6C195D64E2D4960BEFB09D3EEAADBF9873340A5B86D29DE380DFC41A9' +
    '9D7350D9A5055706E3C2037245DF35B9DF14B810772C3A40887CA02CF5CD42993E360852219A9A38' +
    '4647CE912D95DFF0E80706A2696296C7290214CEC72EEB016D34E03FBF5E65F4795E714B35DE183F' +
    'EE6CC6C35E349289CC8DBB37AE9F968CF0BC3654D3B82175B0C5AFFD9B1D421B7DCE8015AFD77520' +
    'A983E1C1E9EB0708118FA4BBEE072157E7CD8CE9381091D3F1D4FC3980E5F8C95D54F4E6BB7AF368' +
    'AF6BC9B332B80D7F8E76C2E203049C3380C5E0F5299DD6E57CD59AED1CCDC998F2FAE72C29A0C1D4' +
    '025C34E506AF77760228DC3B8B680AC8A2FAD784EA6B86EF34F1842E66F46FB0F68F0101425D1634' +
    '1825DD6605DF6310E7C9811F4CB81EC3C7AE7CC6F528C782C6D33D6883E895B3A06E3D3EA84B03E3' +
    '603394B44A1A3C289871A59269F4B80F4C7CF404A4B99A529A574BE46AA6B52B4E544FB7022C9090' +
    '4BE1C767FB8DB744B9C69FBC503C234F6618794F71AD8C95E5087F54B4F82154454C0E7386F21EEC' +
    'C0DDBAF454382A973BE4ECEFE846E0C21B60B56448E6E5EDDBAE084132A1397F16314047144ACADD' +
    '6CC342447DB6A65365747A63C404AE623BC3EF1703C18B14FDA178AB7392E9EB6D2C9045EEFC8A73' +
    '68DBCBA7F09CC88D6EA54505BFB82373819FB990F0F64C6FBE421E35CEBD9B79CE77BDF3C8946C58' +
    'C6071757284E5B53B56DFEDB2EEF928DC9AE02BA21A2B5E284E285C9A6CB05710E2634CB3627C52E' +
    '577F7E5CA7475607A7D2CCF1452F9BA703D322F69A47F16BD9851C7528D89B59C03B426AEBB3B372' +
    '328CA9102F624E901DC8A315524B941706ECFE0AA8AEB0AF8EC33D2E1CA203B489AE23C03D880B83' +
    'D738CC277EB6FF1C7DD31886C639C6C658396F60920D8A0A4E58278D63E5B393295FE8887E8F9F6A' +
    '64C10CA241F699B99D72D13722E42D4615D9F3D0545E320D38EF809076F4216428C29D130F2F8305' +
    '2B9D0BA4A8A1872A242BDC59722281DDECE5407CBBDDBDB0B8C04E9385AD9660D252DD28AA881166' +
    '694207BB04F9A24FA92216ACAF264C9F1CC200A5E2CBD2694C91238A8972496097B3BD3785345EE7' +
    '171A9EB96619722C64571DE6327CD0345B788871D7D5D0D84EF2A47DDE6680B7743AD61B164E558B' +
    '672E4CC4B46D6E6C236C2133965C8B3C7D0F240AB0EDEC78E8EAB4C80040A037F0080FA3B15ACC3E' +
    'EA34208065EB6E6E21DA65601454E6DB037C08B051A1942DAB90C92B0BDC8C572C52F0AEF84B85F0' +
    '444A6C15E84801448DE8147804F796A1A05341E75DF015FEEA7E191C461D3D3D7B2D74A9C4C7E18E' +
    'AFE0966C26731F0A960461DE0387A917A6BBD645C4668030642D70DE30D9954556CDACDE00F82219' +
    'E7A3EECC8354FC5A4388195CE568016C7C63661559EF65E2FF09F1C98A6668C23AE4863B04287A3B' +
    'B6210B3F5689A3E7978517B95B0D06A831F28A3FF55B6053E38535B2AA97FD66B42980CDAF9E5DB6' +
    'F1212BC23CC740BB7DDF8CD40391C0190F62A0A2A287DCC2AB8FA65C71675619C451EC07F3D67EE8' +
    '1A6370EA2144D82CE63CAEB4FD76E6FE4979FBC149BC54A9236D4CF23CD5C09E83DE5BD888F2EB03' +
    '008BB9FBA0CD5CB79DDB62E2F9F8F2E11B2210DDD11590E21214015085A3CD8DD1665AD81D28D552' +
    'B214D91550FFB3010102098E6F7649A5E06057998E25B4F789B2FF356299C589B1CE03D4AADDC917' +
    '7B47974FBE205516B3A9E88F0B3955F133C29C1EDB94969E89B2DA7B64EE423A51672521CB4E1008' +
    'D0AFCF68D6172A174F0ECABB5FB7648444032E6443CF9C7FA4ED4E1728427C2804699A143EDF11CD' +
    '6CF9F5AFF6341A69DE7B7710C223C799861242C0D68841F36D9200122381EBF94D22258B38375C0D' +
    '25DC1E3EEE00E312433CF8032F46EA0A0BCA36C679F5FFC82898372D631FA7368595ED7CB6571E8C' +
    '00C16AFA4F4791E63813141D800111C1EDCEFD8872BEA41B2DD159676F23BF85C7DE07A30D2459EB' +
    '16B24BDAD322C177B174BA83EAEB352B2C0D8140B33D50B0C9F2DDD39F305F9AB4565D3ADF35550F' +
    '023F346900C59903CB972579395840D6A89169DA96D7012A821632E66821591FD3731EB068CBAF5F' +
    '18589E7541CB4AEB0C4859020D0A3B99887D12E78F690073D29A242682D4677E2F904DFD7266E051' +
    '4F6E460AD2F1FC5CE875D106432C5F91D6246D80AE5C7D8C4E4EE122F5FC58380391ED536CA4F073' +
    'E2B55F5ED19751479CB3A71E72E8F330E086981169140A8AD069A49B1FD4EB950F9FB9084D1E702B' +
    '957D081709FC510D31CE2EB1E42AB474D73A793231EC274EE399257DEC4B9D0704265A77E745F4CD' +
    '8F9E4909FB9299304CE6AC7FA95C3F47B210DA388EE748ABD41CDEB3CBB187A40BB3C3F4150ADA34' +
    '72DDB398DF8D8D5865EF27F54CBD4164DB0465DF7443E1D7782A2403387EF0118E8EB10283A39072' +
    'C36A46E724165F49ED170A8FCC00FADF037EBFC652810697CD7108A56989491BA2232BED54BA167D' +
    'B9D526FAD348B7787AE71557C59B38D1054CBE9BCB45E17BB146ADFC04F6816334D3D815BF73FBEC' +
    'F1C9D437D3A47A7862C286884B638DD7FB64BE01B7981D7CC82A90938EBDD9664DEBAF0E03A59536' +
    '8B89338AB076BD22AD579D04F1C7EA075D220F8CCDC0B9E372F5194AFE834D28';
var
  PK, SK, MSG, Sig: TBytes;
  Ctx: AnsiString;
  S: string;
begin
  FreeAndNil(FMLDSAPub);
  FreeAndNil(FMLDSAPriv);
  FreeAndNil(FMLDSA);

  FMLDSAPub := TCnMLDSAPublicKey.Create;
  FMLDSAPriv := TCnMLDSAPrivateKey.Create;
  FMLDSA := TCnMLDSA.Create(cmdt65);

  // PK := HexToBytes(S_PK);
  SK := HexToBytes(S_SK);
  // FMLDSA.LoadPublicKeyFromBytes(FMLDSAPub, PK);
  FMLDSA.LoadPrivateKeyFromBytes(FMLDSAPriv, SK);

  MSG := HexToBytes(S_MSG);
  Sig := FMLDSA.SignBytes(FMLDSAPriv, MSG);

  S := BytesToHex(Sig);
  mmoMLDSAKeys.Lines.Clear;
  mmoMLDSAKeys.Lines.Add(S);
end;

procedure TFormLattice.btnMLDSATest65KeyGenClick(Sender: TObject);
const
  S_SK =
    '43AD6560D3BB684667A559EE6EC7C816020E5B65671F270F2353A8C912B6C26B3DAC8325D5208FBB' +
    'A385BE4D9C77611961D796200072F900727D715515AFE3A3839104C526A5806257D54D22CA1035F1' +
    '7DCA06D2CECBCFAB310C994F7D58E62CD8B9A5CAE7FEF2775FB612246459A94DEB8BE511BAD4C007' +
    'C5607FE1C9CDB7C91871042734805760071827016058751083551404015714520370521746384075' +
    '61232566011558506027142575452430021032256102148680101016217778047720268341572613' +
    '78285714503441714757160073732356454870013823556823652414027414467352117628737408' +
    '22880881412810353573148554078386048742028384216832477015164257364347170070476826' +
    '42523476171210235348518663656457444724377120164183683438543210752815327213436862' +
    '37227178234275545600726724632063482165470350156412367821112008871272133015375501' +
    '24661416832623134008275127301110438274170284760336777540767006607015458542733644' +
    '77527653031481033526727053663502446271203330764885318152610381162634273374774115' +
    '27372436312303640105630174273246816510212641412477524607721300642371627800401363' +
    '55137636372534687431201613276233550463451322058364785885052601380733632615061100' +
    '46525885585486470783338728527025371757228367720826786632442648888664567641215343' +
    '44424088315124321437108104736487517350118228336624244787454758840387161255671842' +
    '08768860650416622351137275746336756553777453220673618100250174607842037110877466' +
    '57367767870635317826728708677118636774557228730464268435746880724165042145653361' +
    '34206816440212168446314053402810218615206277587064842382778073506378438575728063' +
    '63145528876076540884216485115253516510151067614034047278055348333071554060501056' +
    '77751254475831305621337501545847223755453130146855043504633775825214046505831782' +
    '68185780510230484347714884333033718604346122076820516810072742355046510511050364' +
    '85727048777861140347683323582177270861101745674724017721068503801314751007050330' +
    '68137545145731165715511552416705816565437664874846745602571867462001877773648817' +
    '20453658314312756473640018266428267342685700018774244018117438545651832682404883' +
    '47571103622607068764060037426823556464876211866704285442566432428611512475760138' +
    '18486858637104702643412736346843582065646835415016716855362122176312878427117031' +
    '46253503861323702635710641403168165420138152652710726756872661205666788778252318' +
    '80065251181231633822285122458443015318633364000784236458252384030804427720857853' +
    '02804880023810815632470002070583446286620075101543532564262255420231471047745076' +
    '82666885626813008117821757115307444846404057433872085553236700784108754776448157' +
    '40410477021318650866587571047373875644673507143062246681336181716752823720657328' +
    '62882748028156275776088240167187265530483683611254358755481744280472180622102828' +
    '84001702540737560173176405776471080717638445652451150365406628342207516552873422' +
    '21621752356237133878057213237585028728713738001774675108141608126487445165350021' +
    '32422305484115354484258445332215383756671144327484627347377021066832526228313864' +
    '06021017072061511441845036768158021388323027514722271175615420362623266154313381' +
    '65116758685720162285157116377643231431243852733776584261054544005228457187605483' +
    '01842253570007421821308315311248062732561802128461130725500206157265486367318662' +
    '1261024440273457334313817453457750444B7169D30417F278FC99324BEDA10F1E123855584D14' +
    'CB6C067B8E8D699C0F991FBD7BB1791F7B30DC418483BB316CF9613E5BD127EA122632DB2F204D78' +
    'DF5278020117CDA39DD5A0EFAF087C794387A7EF66B8420B966B2326A802B34A3E92F71BED842371' +
    'FDAE6B0D61D5C72DAA85AF99FB4C97B14E2E4BC4BB11D3B3434ADA822CD786F43DA8026FF857FA82' +
    'D2C32715DE8DF951855EAA8644B2BDE0CD5E8F8B40624DE06C6BA60C4E50308A757C1B7B24B635DE' +
    '0CFE2EFA64F14AEE1DB19A8A834C2C5B9C45CFE2D1842DB27B471E70D176735E05D4279270CB78D4' +
    '60834CDA4A25E344E5C5E1A03A224C3857E4912F944AE618B1F8387E8B2475C9A20D59694060BDCF' +
    '3D83ECEA1A1445EE1A33F9C2ED52C39642BCD39C1D4B09062D121D028B220491A6B3C66B81442C5A' +
    'C20B1BD3A047F1E57330C0FEAC6FE5CF725FC76DD3A84EC4FA9C9164F2061494AA89B0367E4A24B0' +
    '8FE656397E0924EC373898F2AA25FC62A9E271A0DA015D4E4FF2189654594D574B8F8884DA9991A6' +
    'F54A2F13DF15F8B0F67D7491207539488505D591964B4A1F22650E881E74C84255E81D016F5A99AE' +
    '559686098997CCD1AD102BE86AE4A1CCED1EEB057CF89DC103CC4026E4B244640055AE44B08A85C3' +
    'C3A0617998A17A533ECA6904677D351E1C10575032FE493577AAFBDF32EE4C8784A1DAEA2F04EDE5' +
    'E6ED69F6CBE75D1830EBF74A3F93C13A199895A4C5528BBA9A3DDC3BE87EAD4E2FD9AA951A9F5749' +
    '502CF89FCB57F47E95B479372BFCD80D0203C9680D94B43671C12B35C511A6CCC24A284B6563C6F0' +
    '833FA5E82261831C4E6D3740135200CF6D10342A808AE47EB0CFBBE2E9912B7EC2E53E69C3873E21' +
    'D52B3993E080B7926E95E167EA3EB9D3E52B802B03BBE35CAB65C938ACA9DC7C282A0FE2E0EAFC9E' +
    '27A5945B2EF7279145DD5A4F4DD63C30CC9728CB0849D5BD35D43D604B42EC4D76C0120A38FF97A4' +
    '95C38D60F573FFFDC704367618B919791A6806A2B9DA834C668097911568C554A821307E5CC522B6' +
    'E607193C1CB7952E221957F2684B4700B5AFBFCAD7852735AE040D63ECF483FA2ED1BC358A274E80' +
    '695AD706B47BDF7B8BEC73DD99187B8DB51E6CF0D04BCE594BB7EAB045C628B45C195005BFFF6F43' +
    'FC61C2D617FFFA5C06B195B7284F15D009800856149930309E693060381F6958CDDE8EABE65B87FF' +
    'FC6623D239EA72278E69AD8C505B16073C8A035F2BB9458FA38A6EC27B7D30E9F40B82F657B21B4C' +
    'AE98FCC7E366EB20199BEE79BDDEDB3629A43FDB2ABF59CD2E37CCF15C59234DF85B15A1C6440F3F' +
    '66DAF010E6BB7A3A0C1DE62CBBA224BCEF2C9233702B92B124A29FE54E45CA2C7BC371D52D9A9E11' +
    'AD0C84771A0181378AC896F17398E6583B84A3E495E0AEDA09875EF4B676AB813C8C9DDE120203BF' +
    'DDFD9B6412F5A10A49109921185D4DA5D89E455B036654F6108FDF36701A36538EA04AF2855C8D2A' +
    '6E7810A45BCD4F8940320C8C1CD179070307906E016815E9F37D3A32943E164DEADB8C1A393EDEE5' +
    'ACEAC9C7153B55805FEF3A5EE27234EF040B8F269FB96D3659044C8CAC05659F3E9F603C3C165EA9' +
    '749AA1C0A7BE268E747B57B01521CFBDCC63B3AFDC45CF5EC82431C6CF18A98357B1E1FE184818E8' +
    '896674E867314626AA5A50B3333CFC3B1FA1E9B8BCB45C181A6EEA07FC14566695171D3EAF5A60E9' +
    '2A2ED2FBC13E34C699AC1CA57AD278471601892E9674E2763ED697F80EEA52B06A747AA4B0FD2DEB' +
    '55D565CC13C7AEBC2805BA2777E19D884A0A618B08D6BBB4AFFBE8A1C1CF8DDE4258B11E50904F48' +
    '8EF7A73FD0E0C141F493BCD0C648F23DD4B721ED5CF1041EFD1AF46446418EFBEF514AF9714F7127' +
    'E98B6EBF17755ED6C0EAEACD61400E07F93E885748E248578784B42660AF8936CD565AFF9DC16647' +
    'C014000039E3107B978FF28B3CA0E6C8D552925D9B3B7F9190363089C4BD709CE8DD0AE6AF6DECD6' +
    'D5975539EBFC011975D268654E0A7BBE49AAB559B01D9F64CA7E858BC8F78AFBF9E7B360F0A039A3' +
    '3ADA0AD70677C6C90AACCBAEB5DD384CE28126B2ED11306662819BC51E71E1EF93CC989244FCC0DF' +
    '266D90FF57F09FB4EA2ADE80AF9C9A1A67437F6FCCCBBC9181F4BEDC7B456AD592AE4ADA35AEB613' +
    'B50D46EB3DEEC8F9A5F36F1C92E900DFE55B212EEE6FC963E0C149B9AF83E256EA3021AF7BBA9AFF' +
    '46E2617BEF211987F1C0BB6F63737BDE142C96C6D284421A3E49B141D3125580391AC5EEAAF7D3FF' +
    '1A61CC15AAD4F64F712D0A26F57A869E28BBD44A056000819D9F8A42FAF0046517B3B8DC43F04B2F' +
    '438F147462ABAE1CBBBCDE086CA358C3596C34BD237509625AC7033CDC40EF5EDBC43E0ACB41CDFB' +
    '813761200837E47F9780F5FD5ADBBF03065DE78FCE30DB1C0FDA6C64BC13C98CDBBD518A73CE4469' +
    '67F077A5D965739C8664438EC770C14DB0CAED34E751BB3AFEA13B53E92E3CDEEB1AC151C628A44A' +
    '6F93D50C10FE5CE305EA9CDAF9920F7AABA65A65BA7038CD4EDFD633B15B8EE96595F5BA90C3E736' +
    '7C6FD8AE7356B7C870B3DF7CAD77A57A189472EEBC1634059928F0E44F8A113D8328001834AFCF64' +
    'D589FBBFF7FA6409E4BD0A08D96B751549B2DFD94D7784BE7943CDDCC41149EED29C003CF2F3678C' +
    '8258B3061AE592B5C46BAF4DCF3DED7DFCF0DF4D9CE1A37954356A7FD794E4AFB8A30EE0779CC9A2' +
    '8E72F0671D5E928E99E302AD7FC8864472004F782EA17249B803A134D998C8D0A52EBE3AF26C20AD' +
    '3B789C324456FDC1D8B432DD92A3A7CA8AC6F0EAD3F714C410D462FBAEFBE9138227172D58672A12' +
    '203228F3141BAA173EA2BA78B6152BF472FAA9A8F83643F19659AFCB7709F82EA91779B746DC8B45' +
    '22424222F71C9579E6D1050386A4762BAA1A38CC4A379F0744704CDB8DAF847A686DA42A26FA7CDE' +
    '58800E0701EC7B7980A10F7464A4BD6710988919AF04E0174B058793E655ECE6F31F41EE01C6C9DD' +
    '3D6BDB9C26F51A0013B7E2ABF343986ABD9617DDBE0D9B760E2238C4AE3CA620C515AA2CCD5C5BD6' +
    'D0BA2061CCDE43DCA83834F110FD123569CEA234656904C2F02F12FDEB8996BCE10792D7E447BCBA' +
    '1A91900EE7A1DDDC3FF4E05AE59A267BF78550C79847DFCF6960EF3D908A4D28FECA3800682581CE' +
    '766A64097A5BE69A6DD376637625C8F9E10DCC951056BCC843E16BBEB7C137E2D83680F96AAF8808' +
    'EE24FB6898742380ACA5A23C6EEC15C0AB7D6B4D32678005CC5E0F042146ABBB52D13EE3C7D33CE7' +
    '8BB55D8FCDB4F9023AACBF04D3658C6A81DA63E11C1F53FCB7F684A66994C55F7D024EACFAC70896' +
    '6A665658BA599D8D781DCF39F69A36278B6CBF500CCBA29211CAF2605FEE01AD47C3FD875C8DD889' +
    'F0C699C6C88B22FDAC5E5DA14ED1F6FD475D74B647BB23F2C0F66F091CCD70EE089CED548CF11CB6' +
    'A11D237C8CB4CB549B8BF9904F781865F17897F05B40B8B8C9BCFA4DD2BE7D23';
  S_PK =
    '43AD6560D3BB684667A559EE6EC7C816020E5B65671F270F2353A8C912B6C26B0DB0C2CF42DC747B' +
    '10AA3EBDD573B300EEA46C4200B210094F9512119A6BB837242762B2CE94C2467278500EE7B139BE' +
    'D906676663355B813A9AD9D3DB70F7AF2D785040BFD51208BD3D2CFB09EAF7CEDF77D1B59DA75F77' +
    '28F120C11898D9EC2CB22C73EB8F9436FF60524B56EE6B413030EB7DD10774261452CD8C5ADE75D1' +
    '967628078CDA77E2B1AFB83B9F07F6939D37FF54D5E10ED17FF8A3C21546A89F514576AE780DE876' +
    '1C4F2EA28828C69E38C730ACAA4CC8DC7DF63BA4C1525510FAE2C8E1B01812358BC5DFC01E955294' +
    'A5DFDD1CFF0519E20B8F74FE18854D80C86051AA5CC2FC1DB078BC785BF4BAD6832B8C269156509B' +
    '332038B4C3719DC49814FC6B6AD5360E945AFFF4D4AC235F56C7F7A9A872B518C1F0D48184DA0EB3' +
    '18F74EB84C4F324A2BD03704D2E2A59F64A8854C7AEFB2D3530E20C8AE8A487E6CBEDA645BD86A5A' +
    '83E77A6A22888ED8E43A7F4804C2DE187F1ACBA3CF55CF99412A7A59CF77A4A977724A72686FDF7F' +
    'C64492A5CB75921AD014EB727EDA1DFA7BD7ACE52FE292322F0BE0B004DCE44BEAA20FF06A7691DC' +
    '36405361F9240DDF2FD1A5EC422ED639505AB8E137B971D5729B11E84C040247424A51DDDBDBC43A' +
    'F261D038B0CD70D5BF44252A3786A26AF3FCD4EC100E5CDDE019F17BE6A64F820C3F622F78D4F56A' +
    '984122D6FA2D438D548DD87B9095F1FF02437854E2419A0316C33EAFFA0161737E476A9E707CC40E' +
    '78686D6A043DDE962B319BE2BF9F7A1EFF9EDEFD1B4CD07131494C084083BF76181E3EB139992931' +
    '4473A75E199AC9D5444DB0CEC07E625EC70C6864093961950987FB1E96DCB7E001209865D66D829C' +
    'D2E2B240818CACE003C9CC74DCE5151C65E59AC1EF6D495B0C717B4412C70B50CF44F44E648788F4' +
    '6BAF6F8AF3361F0E4B6119EDF6374DA596453169B935E1A3B875A6C1B9FE384AF961860514E8CF29' +
    '1D8650D7530DB42A46790649B5D8134AAEC33A41F0AB4296AE26203291F1C2BB5276AC305269778E' +
    '7F2A4BAC15B5A31A6B6B76342596D39C7FD3D1C518689372EBD20B667BE5EE2ED11BC107A7600EDA' +
    '1BE7A5DC05BB9F16D2B8BB1C7D8D10050207530BFFDAAE7B11E0615726F2E99CE99D6CA6048F9D61' +
    'B14F7265473EC2D02989772B3D7E212AA68D89374C6CAF7AB160C6C5E09502049C3D03738D700457' +
    'F706341DDEAFC6CA739ECFB4F193EA6B385B035EEA0F7BFD61FA776AF32AED6366E6C0642D1A0175' +
    '9FA6BDD295F7D18CA6DA1D48563EEE403F2F8BCB6A60326C481F12F8180B2B8117ADE61C7E29F525' +
    '4207C5D4657B82BE4EBA436752EC7DA0627FCED830C15F10FB8D3CD90B4505FA325B54D954C5B630' +
    '1DA72B262B226EAB2E4EE88226CC606B97736260ECB6D8F74A0440AFD5D751A90873FFF00C8D3E9C' +
    'A0975F303F7AC263B8FF496C6C8FD22E8EF7B587BAB50A7DAD99BD55D3B7968584F1FB21255EE22D' +
    'B56AF6034F3F13E659161A57CA8C9F2E87CA96BD7100FCEF8F74A8C6A1C92E2EFF74E2F5FAE512C0' +
    '9D26E0F3985D882401EFC54727BBC0F4E1110771A106898692D0C5A6997CA742846FA4D49E8ABDF1' +
    '23D92743E9949BEB6E46B9655EE698C23D74991C96067DEF06EACB981AD4A7A5ED91EACF05D374C7' +
    '4C443F3FBAD363B2450A1A47AAF2954D36E53B06345139138D38B941298982EEA84400C4DCC38F51' +
    '27951906EE3E40F75A5DFE09FCE9BB0143A5D5ADC3C402F23A75A423AB98392CA3A4D5D23D3BCD56' +
    'FF22C9612A5D2C223C7079958CD05175AA74DDD21B42051CDBDC14048CB43CB2F6535E2CA9F5B870' +
    '52F633976F4795CBA69D39F2481CFB9D210C9B0E9EFD941AC875A9A6C3E839EC54F55585721DE418' +
    '15DDFC05E8A58C97E2FA52984135AAB0931094FF8400CAB043C2A5E63C2942B7D36988C4ED9B73C1' +
    '1D913E758ADF94291A42743E4FB04C271ED5807EA03271EA6656CF967AB2595588B55F82AF2D07AD' +
    'FFCCF859ABA70B1707B722DA1FF393CC5BBCC02014C0D4500655577946DB5F95EF1E7657DC98402E' +
    '5CB048DCB372C9277FDA4D8F3A30C953822474CEEDA670D5E680029259260D91F8737CF7572651FB' +
    '28A7DF46F671679BDD507696B021C2C7F4300F3098FF9460582DB58E122C585185BCD091E7ACCB60' +
    '8F7E0C3558627484529A662C0528D419248B6565D32ECC78F7891DB5BB1984CDE89C1AF25F092720' +
    '5E734A7DFEB9AEE94F23F2FD11FB53EC768F6B8268E00E4054CD12EDE4832B07A254A4E2241854E8' +
    'FF2AE1E1B248F9EB1C77581CA2A2EF2D4C9171177A1E040F9D4AD8D0D0C6CD14FCD13B233794E517' +
    '04B6890C56BCE1B8CD1C9EAE6D59ACD91EB67B3A618D65F0F94E5458271E14DC6F6530AD0EE8B2B2' +
    'F0CEC14612E563338E241602B997EC4E62C83942C7F18DAD6841B1348CAB99A78F598FE78A20205D' +
    '88D826D2E163F6B628B266C187B427F253000E4EF99FEC0494A97D9B42E37EE613767D2651FB7CB2' +
    'B9E99578CE2D78B9C9777C954DBD1D7BE8B568F88AB42DDFD293BE28747103B052AD81D8F6254E42' +
    '6802516500111ADF0A8F27AE7C55D3D5DB86278FAF58B68A26D12B2801AC28EDA87AA5D692EDA9BE' +
    '08F7CC3E78517299A3FD9CE2A0A893E12D71062AE2514C465D399F165E4D2F71D1913D8B95396681' +
    '486432B090F0CCE86AA84B661FF22D4A56035E821A1CE30F33AFEB6C7B8FA9CE';
var
  M: TCnMLDSA;
  Priv: TCnMLDSAPrivateKey;
  Pub: TCnMLDSAPublicKey;
  SK, PK, SK1, PK1: TBytes;
  DI: Integer;
begin
  M := nil;
  Priv := nil;
  Pub := nil;

  try
    M := TCnMLDSA.Create(cmdt65);
    Priv := TCnMLDSAPrivateKey.Create;
    Pub := TCnMLDSAPublicKey.Create;

    // 测生成
    M.GenerateKeys(Priv, Pub, '1BD67DC782B2958E189E315C040DD1F64C8AB232A6A170E1A7A52C33F10851B1');

    SK := HexToBytes(S_SK);
    PK := HexToBytes(S_PK);
    SK1 := M.SavePrivateKeyToBytes(Priv);
    PK1 := M.SavePublicKeyToBytes(Pub);

    if not CompareBytesWithDiffIndex(SK, SK1, DI) then
      ShowMessage('SK Diff at ' + IntToStr(DI));
    if not CompareBytesWithDiffIndex(PK, PK1, DI) then
      ShowMessage('PK Diff at ' + IntToStr(DI));
  finally
    Pub.Free;
    Priv.Free;
    M.Free;
  end;
end;

end.
