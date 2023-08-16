unit Unit25519;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Clipbrd, CnBigNumber, CnECC, Cn25519, ExtCtrls, CnNative;

type
  TForm25519 = class(TForm)
    pgc25519: TPageControl;
    ts25519: TTabSheet;
    grp25519: TGroupBox;
    btnCurve25519G: TButton;
    btnEd25519G: TButton;
    btnCurve25519GAdd: TButton;
    btnEd25519GAdd: TButton;
    btnCurve25519GSub: TButton;
    btnEd25519GSub: TButton;
    btnCurve25519GMul: TButton;
    btnEd25519GMul: TButton;
    btnEd25519ExtendedAdd: TButton;
    btnEd25519ExtendedMul: TButton;
    btnEd25519GenKey: TButton;
    bvl1: TBevel;
    btnEd25519SignSample: TButton;
    btnEd25519PointData: TButton;
    btnCurve25519DHKeyExchange: TButton;
    btnCalcSqrt: TButton;
    btn25519PointConvert: TButton;
    btnCurv25519MontLadderDouble: TButton;
    btnCurv25519MontLadderAdd: TButton;
    btnCurv25519MontLadderMul: TButton;
    bvl11: TBevel;
    btnBigNumberToField: TButton;
    btnField64Mul: TButton;
    btnField64MulTime: TButton;
    btnCurv25519MontLadderField64Double: TButton;
    btnCurv25519MontLadderField64Add: TButton;
    btnCurv25519MontLadderField64Mul: TButton;
    btnField64Sub: TButton;
    btnField64Reduce: TButton;
    btnEd25519ExtendedField64Add: TButton;
    btnEd25519ExtendedField64Mul: TButton;
    ts25519Sign: TTabSheet;
    grpEd25519Sign: TGroupBox;
    lblEd25519Priv: TLabel;
    lblEd25519Pub: TLabel;
    edtEd25519Priv: TEdit;
    edtEd25519Pub: TEdit;
    btnEd25519Gen: TButton;
    lblEd25519Msg: TLabel;
    edtEd25519Message: TEdit;
    btnEd25519Sign: TButton;
    edtEd25519Sig: TEdit;
    lblEd25519Sig: TLabel;
    btnEd25519Verify: TButton;
    btn25519SignTime: TButton;
    btn25519VerifyTime: TButton;
    btnEd25519SignFile: TButton;
    btnEd25519VerifyFile: TButton;
    dlgOpen1: TOpenDialog;
    dlgSave1: TSaveDialog;
    btnEd25519LoadKeys: TButton;
    btnEd25519SaveKeys: TButton;
    btn25519Field64Power2k: TButton;
    btn25519Field64PowerPMinus2: TButton;
    ts448Basic: TTabSheet;
    grp448Basic: TGroupBox;
    btn448CheckMap: TButton;
    btnCurve25519Test: TButton;
    btnCurve448Test: TButton;
    btnConvert448Point: TButton;
    btnCurve448GOn: TButton;
    btnEd448GOn: TButton;
    btnEd448PlainToPoint: TButton;
    btnAnother448GOn: TButton;
    btnConvertAnother448Point: TButton;
    btnCurve448DHKeyExchange: TButton;
    btnEd448CalcKey: TButton;
    btnEd448GAdd: TButton;
    btnEd448GMul: TButton;
    btnEd448SignSample: TButton;
    bvl2: TBevel;
    lblEd448Priv: TLabel;
    lblEd448Pub: TLabel;
    lblEd448Msg: TLabel;
    lblEd448Sig: TLabel;
    edtEd448Sig: TEdit;
    edtEd448Message: TEdit;
    edtEd448Pub: TEdit;
    edtEd448Priv: TEdit;
    btnEd448Gen: TButton;
    btnEd448LoadKeys: TButton;
    btnEd448SaveKeys: TButton;
    btnEd448Sign: TButton;
    btnEd448Verify: TButton;
    btn448SignTime: TButton;
    btnEd448SignFile: TButton;
    btnEd448VerifyFile: TButton;
    btn448VerifyTime: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCurve25519GClick(Sender: TObject);
    procedure btnEd25519GClick(Sender: TObject);
    procedure btnCurve25519GAddClick(Sender: TObject);
    procedure btnEd25519GAddClick(Sender: TObject);
    procedure btnCurve25519GSubClick(Sender: TObject);
    procedure btnEd25519GSubClick(Sender: TObject);
    procedure btnCurve25519GMulClick(Sender: TObject);
    procedure btnEd25519GMulClick(Sender: TObject);
    procedure btnEd25519ExtendedAddClick(Sender: TObject);
    procedure btnEd25519ExtendedMulClick(Sender: TObject);
    procedure btnEd25519GenKeyClick(Sender: TObject);
    procedure btnEd25519SignSampleClick(Sender: TObject);
    procedure btnEd25519PointDataClick(Sender: TObject);
    procedure btnCurve25519DHKeyExchangeClick(Sender: TObject);
    procedure btnCalcSqrtClick(Sender: TObject);
    procedure btn25519PointConvertClick(Sender: TObject);
    procedure btnCurv25519MontLadderDoubleClick(Sender: TObject);
    procedure btnCurv25519MontLadderAddClick(Sender: TObject);
    procedure btnCurv25519MontLadderMulClick(Sender: TObject);
    procedure btnBigNumberToFieldClick(Sender: TObject);
    procedure btnField64MulClick(Sender: TObject);
    procedure btnField64MulTimeClick(Sender: TObject);
    procedure btnCurv25519MontLadderField64DoubleClick(Sender: TObject);
    procedure btnCurv25519MontLadderField64AddClick(Sender: TObject);
    procedure btnField64SubClick(Sender: TObject);
    procedure btnField64ReduceClick(Sender: TObject);
    procedure btnCurv25519MontLadderField64MulClick(Sender: TObject);
    procedure btnEd25519ExtendedField64AddClick(Sender: TObject);
    procedure btnEd25519ExtendedField64MulClick(Sender: TObject);
    procedure btnEd25519GenClick(Sender: TObject);
    procedure btnEd25519SignClick(Sender: TObject);
    procedure btnEd25519VerifyClick(Sender: TObject);
    procedure btn25519SignTimeClick(Sender: TObject);
    procedure btn25519VerifyTimeClick(Sender: TObject);
    procedure btnEd25519SignFileClick(Sender: TObject);
    procedure btnEd25519VerifyFileClick(Sender: TObject);
    procedure btn25519Field64Power2kClick(Sender: TObject);
    procedure btn25519Field64PowerPMinus2Click(Sender: TObject);
    procedure btnEd25519LoadKeysClick(Sender: TObject);
    procedure btnEd25519SaveKeysClick(Sender: TObject);
    procedure btn448CheckMapClick(Sender: TObject);
    procedure btnCurve25519TestClick(Sender: TObject);
    procedure btnCurve448TestClick(Sender: TObject);
    procedure btnConvert448PointClick(Sender: TObject);
    procedure btnCurve448GOnClick(Sender: TObject);
    procedure btnEd448GOnClick(Sender: TObject);
    procedure btnEd448PlainToPointClick(Sender: TObject);
    procedure btnAnother448GOnClick(Sender: TObject);
    procedure btnConvertAnother448PointClick(Sender: TObject);
    procedure btnCurve448DHKeyExchangeClick(Sender: TObject);
    procedure btnEd448CalcKeyClick(Sender: TObject);
    procedure btnEd448GAddClick(Sender: TObject);
    procedure btnEd448GMulClick(Sender: TObject);
    procedure btnEd448SignSampleClick(Sender: TObject);
    procedure btnEd448GenClick(Sender: TObject);
    procedure btnEd448LoadKeysClick(Sender: TObject);
    procedure btnEd448SaveKeysClick(Sender: TObject);
    procedure btnEd448SignFileClick(Sender: TObject);
    procedure btnEd448SignClick(Sender: TObject);
    procedure btnEd448VerifyClick(Sender: TObject);
    procedure btnEd448VerifyFileClick(Sender: TObject);
    procedure btn448SignTimeClick(Sender: TObject);
    procedure btn448VerifyTimeClick(Sender: TObject);
  private
    FCurve25519: TCnCurve25519;
    FEd25519: TCnEd25519;
    F25519PrivKey: TCnEccPrivateKey;
    F25519PubKey: TCnEccPublicKey;
    F25519SigData: TCnEd25519SignatureData;
    FCurve448: TCnCurve448;
    FEd448: TCnEd448;
    F448PrivKey: TCnEccPrivateKey;
    F448PubKey: TCnEccPublicKey;
  public

  end;

var
  Form25519: TForm25519;

implementation

{$R *.lfm}

const
  SCN_25519_PRIME = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED';
  // 2^255 - 19

  SCN_25519_COFACTOR = 8;
  // 余因子均为 8，也就是椭圆曲线总点数是 G 点阶数的八倍

  SCN_25519_ORDER = '1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED';
  // 基点阶数均为 2^252 + 27742317777372353535851937790883648493

  // 25519 扭曲爱德华曲线参数
  SCN_25519_EDWARDS_A = '-01';
  // -1

  SCN_25519_EDWARDS_D = '52036CEE2B6FFE738CC740797779E89800700A4D4141D8AB75EB4DCA135978A3';
  // -121655/121656，也就是 121656 * D mod P = P - 121655 算得 D =
  // 37095705934669439343138083508754565189542113879843219016388785533085940283555

  SCN_25519_EDWARDS_GX = '216936D3CD6E53FEC0A4E231FDD6DC5C692CC7609525A7B2C9562D608F25D51A';
  // 15112221349535400772501151409588531511454012693041857206046113283949847762202

  SCN_25519_EDWARDS_GY = '6666666666666666666666666666666666666666666666666666666666666658';
  // 46316835694926478169428394003475163141307993866256225615783033603165251855960

  SCN_448_PRIME = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';

  SCN_448_ORDER = '3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7CCA23E9C44EDB49AED63690216CC2728DC58F552378C292AB5844F3';
  // 基点阶数为 2^446 - 13818066809895115352007386748515426880336692474882178609894547503885

  SCN_448_MONT_GU = '05';

  SCN_448_MONT_GV = '7D235D1295F5B1F66C98AB6E58326FCECBAE5D34F55545D060F75DC28DF3F6EDB8027E2346430D211312C4B150677AF76FD7223D457B5B1A';

  SCN_448_EDWARDS_A = '01';

  SCN_448_EDWARDS_D = '-98A9';

  SCN_448_EDWARDS_GX = '4F1970C66BED0DED221D15A622BF36DA9E146570470F1767EA6DE324A3D3A46412AE1AF72AB66511433B80E18B00938E2626A82BC70CC05E';

  SCN_448_EDWARDS_GY = '693F46716EB6BC248876203756C9C7624BEA73736CA3984087789C1E05A0C2D73AD3FF1CE67C39C4FDBD132C4ED7C8AD9808795BF230FA14';

  SCN_448_SQRT_156324 = 'BA4D3A0829B6112F8812E51BA0BB2ABEBC1CB08EB48E556936BA50FDD2E7D68AF8CB32160522425B3F990812ABBE635AD37A21E17551B193';
  // 提前算好的 sqrt(156324)，供点坐标转换计算

procedure TForm25519.FormCreate(Sender: TObject);
begin
  FCurve25519 := TCnCurve25519.Create;
  FEd25519 := TCnEd25519.Create;
  FCurve448 := TCnCurve448.Create;
  FEd448 := TCnEd448.Create;
  F25519PrivKey := TCnEccPrivateKey.Create;
  F25519PubKey := TCnEccPublicKey.Create;
  F448PrivKey := TCnEccPrivateKey.Create;
  F448PubKey := TCnEccPublicKey.Create;
end;

procedure TForm25519.FormDestroy(Sender: TObject);
begin
  F448PubKey.Free;
  F448PrivKey.Free;
  F25519PubKey.Free;
  F25519PrivKey.Free;
  FEd448.Free;
  FCurve448.Free;
  FEd25519.Free;
  FCurve25519.Free;
end;

procedure TForm25519.btnCurve25519GClick(Sender: TObject);
begin
  if FCurve25519.IsPointOnCurve(FCurve25519.Generator) then
    ShowMessage('Curve 25519 Generator Point is on this Curve');
end;

procedure TForm25519.btnEd25519GClick(Sender: TObject);
var
  Data: TCnEd25519Data;
  P: TCnEccPoint;
begin
  if FEd25519.IsPointOnCurve(FEd25519.Generator) then
    ShowMessage('Ed 25519 Generator Point is on this Curve');

  CnEd25519PointToData(FEd25519.Generator, Data);
  P := TCnEccPoint.Create;
  FEd25519.PlainToPoint(Data, P);
  if CnEccPointsEqual(P, FEd25519.Generator) then
    ShowMessage('Ed25519 G Plain To Point is G');

  P.Free;
end;

procedure TForm25519.btnCurve25519GAddClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);

  FCurve25519.PointAddPoint(P, Q, P);
  // P 是 2*G
  if FCurve25519.IsPointOnCurve(P) then
    ShowMessage('Curve 25519 G + G is on this Curve');

  FCurve25519.PointAddPoint(P, Q, P);
  // P 是 3*G
  if FCurve25519.IsPointOnCurve(P) then
    ShowMessage('Curve 25519 G + 2*G is on this Curve');

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointInverse(Q);

  FCurve25519.PointAddPoint(P, Q, P);
  if P.IsZero then
    ShowMessage('Curve 25519 G + -G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519GAddClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);

  FEd25519.PointAddPoint(P, Q, P);
  // P 是 2*G
  if FEd25519.IsPointOnCurve(P) then
    ShowMessage('Curve 25519 G + G is on this Curve');

  FEd25519.PointAddPoint(P, Q, P);
  // P 是 3*G
  if FEd25519.IsPointOnCurve(P) then
    ShowMessage('Ed 25519 G + 2*G is on this Curve');

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);
  FEd25519.PointInverse(Q);

  FEd25519.PointAddPoint(P, Q, P);
  if FEd25519.IsNeutualPoint(P) then
    ShowMessage('Ed 25519 G + -G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurve25519GSubClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);

  FCurve25519.PointAddPoint(P, Q, P);
  FCurve25519.PointAddPoint(P, Q, P);

  // P 是 3*G，Q 是 G
  FCurve25519.PointSubPoint(P, Q, Q); // Q 是 2*G
  if FCurve25519.IsPointOnCurve(Q) then
    ShowMessage('Curve 25519 3*G - G is on this Curve');

  FCurve25519.PointSubPoint(P, P, Q); // Q 是无限远点
  if Q.IsZero then
    ShowMessage('Curve 25519 G - G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519GSubClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);

  FEd25519.PointAddPoint(P, Q, P);
  FEd25519.PointAddPoint(P, Q, P);

  // P 是 3*G，Q 是 G
  FEd25519.PointSubPoint(P, Q, Q); // Q 是 2*G
  if FEd25519.IsPointOnCurve(Q) then
    ShowMessage('Curve 25519 3*G - G is on this Curve');

  FEd25519.PointSubPoint(P, P, Q); // Q 是无限远点
  if FEd25519.IsNeutualPoint(Q) then
    ShowMessage('Curve 25519 G - G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurve25519GMulClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FCurve25519.Generator);

  FCurve25519.MultiplePoint(456768997823, P);
  if FCurve25519.IsPointOnCurve(P) then
    ShowMessage('Curve 25519 Random * G is on this Curve');

  P.Free;
end;

procedure TForm25519.btnEd25519GMulClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FEd25519.Generator);

  FEd25519.MultiplePoint(8099261456373487672, P);
  if FEd25519.IsPointOnCurve(P) then
    ShowMessage('Ed 25519 Random * G is on this Curve');

  P.Free;
end;

procedure TForm25519.btnEd25519ExtendedAddClick(Sender: TObject);
var
  P, Q, S: TCnEccPoint;
  P4, Q4, S4: TCnEcc4Point;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;
  S := TCnEccPoint.Create;

  P4 := TCnEcc4Point.Create;
  Q4 := TCnEcc4Point.Create;
  S4 := TCnEcc4Point.Create;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);
//  FEd25519.SetNeutualPoint(P);
//  FEd25519.SetNeutualPoint(Q);
  // ============ 相同点的点加 ================
  // P 是 G , Q 是 G
  CnEccPointToEcc4Point(P4, P, FEd25519.FiniteFieldSize);
  CnEccPointToEcc4Point(Q4, Q, FEd25519.FiniteFieldSize);

  FEd25519.ExtendedPointAddPoint(P4, Q4, S4);
  // ShowMessage(S4.ToString);
  FEd25519.PointAddPoint(P, Q, S);

  // 验证 S 和 S4 是否相等
  CnEcc4PointToEccPoint(P, S4, FEd25519.FiniteFieldSize);
  //ShowMessage(P.ToString);
  //ShowMessage(S.ToString);
  if CnEccPointsEqual(P, S) then
    ShowMessage('Extended Add G+G OK');

  // ============ 不同点的点加 ================
  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);
  //FEd25519.SetNeutualPoint(Q);
  FEd25519.PointAddPoint(P, Q, P);

  // P 是 2*G , Q 是 G
  CnEccPointToEcc4Point(P4, P, FEd25519.FiniteFieldSize);
  CnEccPointToEcc4Point(Q4, Q, FEd25519.FiniteFieldSize);

  FEd25519.ExtendedPointAddPoint(P4, Q4, S4);
  FEd25519.PointAddPoint(P, Q, S);
  // ShowMessage(S.ToString);

  // 验证 S 和 S4 是否相等
  CnEcc4PointToEccPoint(P, S4, FEd25519.FiniteFieldSize);
  // ShowMessage(S4.ToString);
  if CnEccPointsEqual(P, S) then
    ShowMessage('Extended Add G+2G OK');

  S4.Free;
  Q4.Free;
  P4.Free;
  S.Free;
  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519ExtendedMulClick(Sender: TObject);
const
  M = -8099261456373487672;
var
  P, Q: TCnEccPoint;
  P4: TCnEcc4Point;
  T1, T2: Cardinal;
  I: Integer;
begin
  P := TCnEccPoint.Create;
  P.Assign(FEd25519.Generator);

  P4 := TCnEcc4Point.Create;
  CnEccPointToEcc4Point(P4, P, FEd25519.FiniteFieldSize);

  FEd25519.MultiplePoint(M, P);
  FEd25519.ExtendedMultiplePoint(M, P4);

  if FEd25519.IsExtendedPointOnCurve(P4) then
    ShowMessage('Ed 25519 Extended Random * G is on this Curve');

  Q := TCnEccPoint.Create;
  CnEcc4PointToEccPoint(Q, P4, FEd25519.FiniteFieldSize);

  if CnEccPointsEqual(P, Q) then
    ShowMessage('Ed 25519 Mul/ExtendedMul Equal OK');

  T1 := GetTickCount;
  for I := 1 to 1000 do
    FEd25519.MultiplePoint(M, P); // 已经改造成 Extended 的了
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 1 to 1000 do
    FEd25519.ExtendedMultiplePoint(M, P4);
  T2 := GetTickCount - T2;

  ShowMessage(Format('Normal %d, Extended %d', [T1, T2])); // Extended 比常规的快十倍以上！
  CnEcc4PointToEccPoint(Q, P4, FEd25519.FiniteFieldSize);
  if CnEccPointsEqual(P, Q) then
    ShowMessage('Ed 25519 1000 Mul/ExtendedMul Equal OK');

  Q.Free;
  P4.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519GenKeyClick(Sender: TObject);
var
  Data: TCnEd25519Data;
begin
  FEd25519.GenerateKeys(TCnEd25519PrivateKey(F25519PrivKey), TCnEd25519PublicKey(F25519PubKey));
  FEd25519.PointToPlain(F25519PubKey, Data);
  ShowMessage(F25519PubKey.ToString);
end;

procedure TForm25519.btnEd25519SignSampleClick(Sender: TObject);
var
  B: Byte;
  Sig, ASig: TCnEd25519Signature;
begin
  B := $72;
  Sig := TCnEd25519Signature.Create;
  if CnEd25519SignData(@B, 1, TCnEd25519PrivateKey(F25519PrivKey), TCnEd25519PublicKey(F25519PubKey), Sig) then
  begin
    ShowMessage('Sign OK');
    Sig.SaveToData(F25519SigData);

    ASig := TCnEd25519Signature.Create;
    ASig.LoadFromData(F25519SigData);

    // 比较 Sig 和 ASig 是否相同
    if CnEccPointsEqual(Sig.R, ASig.R) and BigNumberEqual(Sig.S, ASig.S) then
      ShowMessage('Sig Save/Load OK');

    if CnEd25519VerifyData(@B, 1, Sig, TCnEd25519PublicKey(F25519PubKey)) then
      ShowMessage('Verify OK')
    else
      ShowMessage('Verify Fail. Maybe Key needs to be Re-Generated?');
    ASig.Free;
  end;
  Sig.Free;
end;

procedure TForm25519.btnEd25519PointDataClick(Sender: TObject);
var
  Data: TCnEd25519Data;
  P, Q: TCnEccPoint;
  I, K: Integer;
begin
  FEd25519.PointToPlain(FEd25519.Generator, Data);

  P := TCnEccPoint.Create;
  FEd25519.PlainToPoint(Data, P);

  if CnEccPointsEqual(P, FEd25519.Generator) then
    ShowMessage('OK for G');

  Q := TCnEccPoint.Create;
  for I := 1 to 1000 do
  begin
    P.Assign(FEd25519.Generator);
    K := Random(65536);
    FEd25519.MultiplePoint(K, P);

    FEd25519.PointToPlain(P, Data);
    FEd25519.PlainToPoint(Data, Q);

    if not CnEccPointsEqual(P, Q) then
    begin
      ShowMessage('Fail. ' + IntToStr(K));
      Exit;
    end;
  end;
  ShowMessage('OK');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurve25519DHKeyExchangeClick(Sender: TObject);
var
  Priv1, Priv2: TCnCurve25519PrivateKey;
  Pub1, Pub2: TCnEccPublicKey;
  Key1, Key2, Key1O, Key2O: TCnEccPoint;
  D: TCnCurve25519Data;
begin
  Priv1 := nil;
  Priv2 := nil;
  Pub1 := nil;
  Pub2 := nil;
  Key1 := nil;
  Key2 := nil;
  Key1O := nil;
  Key2O := nil;

  try
    Priv1 := TCnCurve25519PrivateKey.Create;
    Priv2 := TCnCurve25519PrivateKey.Create;
    Pub1 := TCnEccPublicKey.Create;
    Pub2 := TCnEccPublicKey.Create;
    Key1 := TCnEccPoint.Create;
    Key2 := TCnEccPoint.Create;
    Key1O := TCnEccPoint.Create;
    Key2O := TCnEccPoint.Create;

    HexToData('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a', @D[0]);
    Priv1.LoadFromData(D);
    HexToData('5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb', @D[0]);
    Priv2.LoadFromData(D);

    CnCurve25519KeyExchangeStep1(Priv1, Key1); // 第一方调用，产生 Key 1
    CnCurve25519KeyExchangeStep1(Priv2, Key2); // 另一方调用，产生 Key 2

    // Key2 给一，Key1 给另一方

    CnCurve25519KeyExchangeStep2(Priv1, Key2, Key1O); // 第一方调用，产生公有 Key 1O
    CnCurve25519KeyExchangeStep2(Priv2, Key1, Key2O); // 第一方调用，产生公有 Key 2O

    if CnEccPointsEqual(Key1O, Key2O) then
      ShowMessage('Key Exchange OK '+ Key1O.ToString);

    // RFC 中的 Secret K 是 Key1O 的 X 坐标再倒过来
  finally
    Key2O.Free;
    Key1O.Free;
    Key2.Free;
    Key1.Free;
    Pub2.Free;
    Pub1.Free;
    Priv2.Free;
    Priv1.Free;
  end;
end;

procedure TForm25519.btnCalcSqrtClick(Sender: TObject);
var
  Prime, P, R, Inv: TCnBigNumber;
begin
  // 检查 Curve25519 曲线 By^2 = x^3 + Ax^2 + x 与 Ed25519 曲线 au^2 + v^2 = 1 + du^2v^2
  // 的参数符合关系 B = 4 /(a-d) （没法验证）  A = 2(a+d)/(a-d) 已验证

  Prime := TCnBigNumber.Create;
  Prime.SetHex('7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED');

  P := TCnBigNumber.Create;
  BigNumberSubMod(P, FEd25519.CoefficientA, FEd25519.CoefficientD, Prime);
  Inv := TCnBigNumber.Create;
  BigNumberModularInverse(Inv, P, Prime);
  BigNumberMulWord(Inv, 4);
  BigNumberNonNegativeMod(P, Inv, Prime);
  ShowMessage(P.ToDec);

  BigNumberDirectMulMod(P, P, FCurve25519.CoefficientB, Prime);
  if P.IsWord(4) then
    ShowMessage('OK')
  else
    ShowMessage('B = 4 /(a-d) Fail');

  R := TCnBigNumber.Create;
  BigNumberSubMod(R, FEd25519.CoefficientA, FEd25519.CoefficientD, Prime);
  BigNumberModularInverse(Inv, R, Prime);

  BigNumberAddMod(P, FEd25519.CoefficientA, FEd25519.CoefficientD, Prime);
  BigNumberAddMod(P, P, P, Prime);

  BigNumberDirectMulMod(P, P, Inv, Prime);
  if BigNumberEqual(P, FCurve25519.CoefficientA) then
    ShowMessage('OK: A = 2(a+d)/(a-d) = ' + P.ToDec); // 486662

  Inv.Free;
  R.Free;
  P.Free;
  Prime.Free;
end;

procedure TForm25519.btn25519PointConvertClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := nil;
  Q := nil;

  try
    P := TCnEccPoint.Create;
    Q := TCnEccPoint.Create;

    P.Assign(FCurve25519.Generator);
    CnCurve25519PointToEd25519Point(Q, P);
    ShowMessage(Q.ToString);
    if FEd25519.IsPointOnCurve(Q) then
      ShowMessage('Converted Ed25519 Point is on Curve');

    P.Assign(FEd25519.Generator);
    CnEd25519PointToCurve25519Point(Q, P);
    ShowMessage(Q.ToString);

    if FCurve25519.IsPointOnCurve(Q) then
      ShowMessage('Converted Curve25519 Point is on Curve');
  finally
    Q.Free;
    P.Free;
  end;
end;

procedure TForm25519.btnCurv25519MontLadderDoubleClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
  T: TCnBigNumber;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToXAffinePoint(P, P); // 转换为射影点
  FCurve25519.XAffinePointToPoint(P, P); // 再转换回来
  ShowMessage(P.ToString); // 和 G 相等

  FCurve25519.PointToXAffinePoint(P, FCurve25519.Generator);
  ShowMessage(P.ToString); // P 得到射影 G 点
  FCurve25519.MontgomeryLadderPointXDouble(P, P);
  ShowMessage(P.ToString); // P 得到射影 2*G 点
  FCurve25519.XAffinePointToPoint(P, P);
  ShowMessage(P.ToString); // P 得到普通 2*G 点

  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointAddPoint(Q, Q, Q);
  ShowMessage(Q.ToString);

  // P 是 X Y 形式的 2*G，Q 是普通形式的 2*G，判断其 X 是否相等
  if BigNumberEqual(P.X, Q.X) then
  begin
    if BigNumberEqual(P.Y, Q.Y) then
      ShowMessage('Montgomery Ladder Double OK. X, Y Both Equals.')
    else
    begin
      T := TCnBigNumber.Create;
      BigNumberAdd(T, P.Y, Q.Y);
      if BigNumberEqual(T, FCurve25519.FiniteFieldSize) then
        ShowMessage('Montgomery Ladder Double OK. X Equals. Y +-');
      T.Free;
    end;
  end;

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurv25519MontLadderAddClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
  T: TCnBigNumber;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  Q.SetZero;
  FCurve25519.PointToXAffinePoint(P, P);
  ShowMessage(P.ToString);                // 9, 1
  FCurve25519.PointToXAffinePoint(Q, Q);  // Q 0
  ShowMessage(Q.ToString);                // 0, 1
  FCurve25519.MontgomeryLadderPointXAdd(P, P, Q, P); // 理论上 P 要得到自身
  ShowMessage(P.ToString); // 结果不是 9, 1
  FCurve25519.XAffinePointToPoint(P, P);
  ShowMessage(P.ToString); // 转换回来才是 9, xxxxxxx 等于 FCurve25519.Generator

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToXAffinePoint(P, P); // P 转换为射影点 G
  FCurve25519.MontgomeryLadderPointXDouble(Q, P); // Q 射影 2*G

  FCurve25519.MontgomeryLadderPointXAdd(P, Q, P, P); // P 得到射影 3*G

  FCurve25519.XAffinePointToPoint(P, P);
  ShowMessage(P.ToString); // P 得到普通 3*G 点

  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointAddPoint(Q, Q, Q);
  FCurve25519.PointAddPoint(Q, FCurve25519.Generator, Q); // Q 直接加出普通 3*G 点
  ShowMessage(Q.ToString);

  // P 是 X Y 形式的 3*G，Q 是普通形式的 3*G，判断其 X 是否相等
  if BigNumberEqual(P.X, Q.X) then
  begin
    if BigNumberEqual(P.Y, Q.Y) then
      ShowMessage('Montgomery Ladder Add OK. X, Y Both Equals.')
    else
    begin
      T := TCnBigNumber.Create;
      BigNumberAdd(T, P.Y, Q.Y);
      if BigNumberEqual(T, FCurve25519.FiniteFieldSize) then
        ShowMessage('Montgomery Ladder Add OK. X Equals. Y +-');
      T.Free;
    end;
  end;

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurv25519MontLadderMulClick(Sender: TObject);
const
  MUL_COUNT = 9876577987898;
var
  P, Q: TCnEccPoint;
  T: TCnBigNumber;
  T1, T2: Cardinal;
  I: Integer;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToXAffinePoint(P, P); // P 转换为射影点 G
  FCurve25519.MontgomeryLadderMultiplePoint(MUL_COUNT, P); // P 自乘
  FCurve25519.XAffinePointToPoint(P, P); // nG 转换为普通点

  Q.Assign(FCurve25519.Generator);
  FCurve25519.MultiplePoint(MUL_COUNT, Q);

  if BigNumberEqual(P.X, Q.X) then
  begin
    if BigNumberEqual(P.Y, Q.Y) then
      ShowMessage('Montgomery Ladder Mul OK. X, Y Both Equals.')
    else
    begin
      T := TCnBigNumber.Create;
      BigNumberAdd(T, P.Y, Q.Y);
      if BigNumberEqual(T, FCurve25519.FiniteFieldSize) then
        ShowMessage('Montgomery Ladder Mul OK. X Equals. Y +-');
      T.Free;
    end;
  end;

  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointToXAffinePoint(Q, Q); // Q 转换为射影点 G
  T1 := GetTickCount;
  for I := 1 to 1000 do
  begin
    P.Assign(Q);
    FCurve25519.MontgomeryLadderMultiplePoint(MUL_COUNT, P); // P 自乘，蒙哥马利阶梯法比常规的快十倍以上！
  end;
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 1 to 1000 do
  begin
    P.Assign(FCurve25519.Generator);
    FCurve25519.MultiplePoint(MUL_COUNT, P); // P 自乘，也已经改成蒙哥马利阶梯法了
  end;
  T2 := GetTickCount - T2;

  ShowMessage(Format('Curve 25519 1000 Mul %d, MontgomeryLadder %d', [T2, T1]));

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnBigNumberToFieldClick(Sender: TObject);
var
  B, C: TCnBigNumber;
  L: TCnBigNumberList;
  D: TCn25519Field64;
begin
  // B := TCnBigNumber.FromHex('8888888877777777666666665555555544444444333333332222222211111111');
  B := TCnBigNumber.FromHex('7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEC');
  Cn25519BigNumberToField64(D, B);

  ShowMessage(UInt64ToHex(D[0]));
  ShowMessage(UInt64ToHex(D[1]));
  ShowMessage(UInt64ToHex(D[2]));
  ShowMessage(UInt64ToHex(D[3]));
  ShowMessage(UInt64ToHex(D[4]));

  L := TCnBigNumberList.Create;
  L.Add.SetInt64(D[0]);
  L.Add.SetInt64(D[1]);
  L.Add.SetInt64(D[2]);
  L.Add.SetInt64(D[3]);
  L.Add.SetInt64(D[4]);

  L[1].ShiftLeft(51);
  L[2].ShiftLeft(102);
  L[3].ShiftLeft(153);
  L[4].ShiftLeft(204);

  C := TCnBigNumber.Create;
  L.SumTo(C);
  ShowMessage(C.ToHex);

  Cn25519Field64ToBigNumber(C, D);
  ShowMessage(C.ToHex);

  C.Free;
  L.Free;
  B.Free;
end;

procedure TForm25519.btnField64MulClick(Sender: TObject);
var
  A, B, C: TCnBigNumber;
  FA, FB, FC: TCn25519Field64;
begin
  A := TCnBigNumber.FromHex('11111111222222223333333344444444555555556666666677777777');
  B := TCnBigNumber.FromHex('66666666555555554444444433333333222222221111111100000000');

//  A := TCnBigNumber.FromHex('F00000000000000000000000');
//  B := TCnBigNumber.FromHex('F00000000000000000000000'); // 测试数据

//  A := TCnBigNumber.FromHex('1000000000000000000000000'); // 多个 0 就出错了，乘积变成全 0 了，该问题已修复
//  B := TCnBigNumber.FromHex('100000000000000000000000'); 

  Cn25519BigNumberToField64(FA, A);
  Cn25519BigNumberToField64(FB, B);

  Cn25519Field64Mul(FC, FA, FB);

  C := TCnBigNumber.Create;
  Cn25519Field64ToBigNumber(C, FC);
  ShowMessage(C.ToHex());  // 这里和下面要相等

  BigNumberDirectMulMod(C, A, B, FEd25519.FiniteFieldSize);
  ShowMessage(C.ToHex());  // 这里和上面要相等

  C.Free;
  B.Free;
  A.Free;
end;

procedure TForm25519.btnField64MulTimeClick(Sender: TObject);
var
  I: Integer;
  A, B, C: TCnBigNumber;
  FA, FB, FC: TCn25519Field64;
  T1, T2: Cardinal;
begin
  A := TCnBigNumber.FromHex('11111111222222223333333344444444555555556666666677777777');
  B := TCnBigNumber.FromHex('66666666555555554444444433333333222222221111111100000000');

  Cn25519BigNumberToField64(FA, A);
  Cn25519BigNumberToField64(FB, B);

  T1 := GetTickCount;
  for I := 1 to 50000 do
    Cn25519Field64Mul(FC, FA, FB);
  T1 := GetTickCount - T1;

  C := TCnBigNumber.Create;
  Cn25519Field64ToBigNumber(C, FC);

  T2 := GetTickCount;
  for I := 1 to 50000 do
    BigNumberDirectMulMod(C, A, B, FEd25519.FiniteFieldSize);
  T2 := GetTickCount - T2;

  ShowMessage(Format('Field %d. DirectMulMod %d', [T1, T2])); // 前者稍微快一点点

  C.Free;
  B.Free;
  A.Free;
end;

procedure TForm25519.btnCurv25519MontLadderField64DoubleClick(
  Sender: TObject);
var
  P, Q: TCnEccPoint;
  PF: TCn25519Field64EccPoint;
  T: TCnBigNumber;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToField64XAffinePoint(PF, P); // 转换为多项式点
  FCurve25519.Field64XAffinePointToPoint(P, PF); // 再转换回来
  ShowMessage(P.ToString); // 和 G 相等

  FCurve25519.PointToField64XAffinePoint(PF, FCurve25519.Generator); // PF 得到射影 G 点
  FCurve25519.MontgomeryLadderField64PointXDouble(PF, PF); // P 得到射影 2*G 点
  FCurve25519.Field64XAffinePointToPoint(P, PF);
  ShowMessage(P.ToString); // P 得到普通 2*G 点

  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointAddPoint(Q, Q, Q);
  ShowMessage(Q.ToString);

  // P 是 X Y 形式的 2*G，Q 是普通形式的 2*G，判断其 X 是否相等
  if BigNumberEqual(P.X, Q.X) then
  begin
    if BigNumberEqual(P.Y, Q.Y) then
      ShowMessage('Montgomery Ladder Field64 Double OK. X, Y Both Equals.')
    else
    begin
      T := TCnBigNumber.Create;
      BigNumberAdd(T, P.Y, Q.Y);
      if BigNumberEqual(T, FCurve25519.FiniteFieldSize) then
        ShowMessage('Montgomery Ladder Field64 Double OK. X Equals. Y +-');
      T.Free;
    end;
  end;

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnCurv25519MontLadderField64AddClick(
  Sender: TObject);
var
  P, Q: TCnEccPoint;
  PF, QF: TCn25519Field64EccPoint;
  T: TCnBigNumber;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToField64XAffinePoint(PF, P); // PF 转换为多项式射影点 G
  FCurve25519.MontgomeryLadderField64PointXDouble(QF, PF); // QF 多项式射影 2*G
  FCurve25519.MontgomeryLadderField64PointXAdd(PF, QF, PF, PF); // PF 得到多项式射影 3*G
  FCurve25519.Field64XAffinePointToPoint(P, PF);
  ShowMessage(P.ToString); // P 得到普通 3*G 点，似乎算得不对

  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointAddPoint(Q, Q, Q);
  FCurve25519.PointAddPoint(Q, FCurve25519.Generator, Q); // Q 直接加出普通 3*G 点
  ShowMessage(Q.ToString);

  // P 是 X Y 形式的 3*G，Q 是普通形式的 3*G，判断其 X 是否相等
  if BigNumberEqual(P.X, Q.X) then
  begin
    if BigNumberEqual(P.Y, Q.Y) then
      ShowMessage('Montgomery Ladder Add OK. X, Y Both Equals.')
    else
    begin
      T := TCnBigNumber.Create;
      BigNumberAdd(T, P.Y, Q.Y);
      if BigNumberEqual(T, FCurve25519.FiniteFieldSize) then
        ShowMessage('Montgomery Ladder Add OK. X Equals. Y +-');
      T.Free;
    end;
  end;

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnField64SubClick(Sender: TObject);
var
  A, B, C: TCnBigNumber;
  FA, FB, FC: TCn25519Field64;
begin
  A := TCnBigNumber.FromHex('64');
  B := TCnBigNumber.FromHex('40');

  Cn25519BigNumberToField64(FA, A);
  Cn25519BigNumberToField64(FB, B);

  Cn25519Field64Sub(FC, FA, FB);

  C := TCnBigNumber.Create;
  Cn25519Field64ToBigNumber(C, FC);
  ShowMessage(C.ToHex());  // 这里和下面要相等

  C.Free;
  B.Free;
  A.Free;
end;

procedure TForm25519.btnField64ReduceClick(Sender: TObject);
var
  FA: TCn25519Field64;
  R: TCnBigNumber;
begin
  FA[0] := 45;
  FA[1] := 2251799813685248;
  FA[2] := 2251799813685247;
  FA[3] := 2251799813685247;
  FA[4] := 2251799813685247;

  R := TCnBigNumber.Create;
  Cn25519Field64ToBigNumber(R, FA);
  ShowMessage(R.ToHex); // 得到 40

  Cn25519Field64Reduce(FA);
  ShowMessage(Cn25519Field64ToHex(FA));
  Cn25519Field64ToBigNumber(R, FA);
  ShowMessage(R.ToHex); // 不知道得到了个啥

  R.Free;
end;

procedure TForm25519.btnCurv25519MontLadderField64MulClick(
  Sender: TObject);
const
  MUL_COUNT = 9823454363465987;
var
  P, Q: TCnEccPoint;
  PF, QF: TCn25519Field64EccPoint;
  T: TCnBigNumber;
  T1, T2: Cardinal;
  I: Integer;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToField64XAffinePoint(PF, P); // P 转换为多项式点 G
  FCurve25519.MontgomeryLadderField64MultiplePoint(MUL_COUNT, PF); // P 自乘
  FCurve25519.Field64XAffinePointToPoint(P, PF); // nG 转换为普通点

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToXAffinePoint(P, P); // P 转换为射影点 G
  FCurve25519.MontgomeryLadderMultiplePoint(MUL_COUNT, P); // P 自乘
  FCurve25519.XAffinePointToPoint(P, P); // nG 转换为普通点

  Q.Assign(FCurve25519.Generator);
  FCurve25519.MultiplePoint(MUL_COUNT, Q);

  if BigNumberEqual(P.X, Q.X) then
  begin
    if BigNumberEqual(P.Y, Q.Y) then
      ShowMessage('Montgomery Ladder Mul OK. X, Y Both Equals.')
    else
    begin
      T := TCnBigNumber.Create;
      BigNumberAdd(T, P.Y, Q.Y);
      if BigNumberEqual(T, FCurve25519.FiniteFieldSize) then
        ShowMessage('Montgomery Ladder Mul OK. X Equals. Y +-');
      T.Free;
    end;
  end;

  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointToXAffinePoint(Q, Q); // Q 转换为射影点 G
  T1 := GetTickCount;
  for I := 1 to 5000 do
  begin
    P.Assign(Q);
    FCurve25519.MontgomeryLadderMultiplePoint(MUL_COUNT, P); // P 自乘，蒙哥马利阶梯法
  end;
  T1 := GetTickCount - T1;

  P.Assign(FCurve25519.Generator);
  FCurve25519.PointToField64XAffinePoint(QF, P);
  T2 := GetTickCount;
  for I := 1 to 5000 do
  begin
    Cn25519Field64EccPointCopy(PF, QF);
    FCurve25519.MontgomeryLadderField64MultiplePoint(MUL_COUNT, PF); // PF 多项式拆项法自乘，又快了近一倍！
  end;
  T2 := GetTickCount - T2;

  ShowMessage(Format('Curve 25519 5000 Field Mul %d, MontgomeryLadder %d', [T2, T1]));

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519ExtendedField64AddClick(Sender: TObject);
var
  P, Q, S: TCnEccPoint;
  P4, Q4, S4: TCn25519Field64Ecc4Point;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;
  S := TCnEccPoint.Create;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);

  // ============ 相同点的点加 ================
  CnEccPointToField64Ecc4Point(P4, P);
  CnEccPointToField64Ecc4Point(Q4, Q);

  FEd25519.ExtendedField64PointAddPoint(P4, Q4, S4);
  FEd25519.PointAddPoint(P, Q, S);

  // 验证 S 和 S4 是否相等
  CnField64Ecc4PointToEccPoint(P, S4);
  if CnEccPointsEqual(P, S) then
    ShowMessage('Extended Add G+G OK');

  // ============ 不同点的点加 ================
  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);
  FEd25519.PointAddPoint(P, Q, P);

  // P 是 2*G , Q 是 G
  CnEccPointToField64Ecc4Point(P4, P);
  CnEccPointToField64Ecc4Point(Q4, Q);

  FEd25519.ExtendedField64PointAddPoint(P4, Q4, S4);
  FEd25519.PointAddPoint(P, Q, S);

  // 验证 S 和 S4 是否相等
  CnField64Ecc4PointToEccPoint(P, S4);
  if CnEccPointsEqual(P, S) then
    ShowMessage('Extended Add G+2G OK');

  S.Free;
  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd25519ExtendedField64MulClick(Sender: TObject);
const
  M = 809926145687654876;
var
  P, Q: TCnEccPoint;
  P4: TCn25519Field64Ecc4Point;
  T1, T2: Cardinal;
  I: Integer;
  Ed: TCnTwistedEdwardsCurve;
begin
  Ed := TCnTwistedEdwardsCurve.Create;
  Ed.Load(SCN_25519_EDWARDS_A, SCN_25519_EDWARDS_D, SCN_25519_PRIME, SCN_25519_EDWARDS_GX,
    SCN_25519_EDWARDS_GY, SCN_25519_ORDER, 8);

  P := TCnEccPoint.Create;
  P.Assign(FEd25519.Generator);

  CnEccPointToField64Ecc4Point(P4, P);

  FEd25519.MultiplePoint(M, P);
  FEd25519.ExtendedField64MultiplePoint(M, P4);

  if FEd25519.IsExtendedField64PointOnCurve(P4) then
    ShowMessage('Ed 25519 Extended Random * G is on this Curve');

  Q := TCnEccPoint.Create;
  CnField64Ecc4PointToEccPoint(Q, P4);

  if CnEccPointsEqual(P, Q) then
    ShowMessage('Ed 25519 Mul/ExtendedMul Equal OK');

  T1 := GetTickCount;
  for I := 1 to 1000 do
    Ed.MultiplePoint(M, P); // 走原始的点加
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 1 to 1000 do
    FEd25519.ExtendedField64MultiplePoint(M, P4);
  T2 := GetTickCount - T2;

  ShowMessage(Format('Normal %d, Field64 Extended %d', [T1, T2])); // Field64 Extended 比 Extended 的还要快一倍以上！
  CnField64Ecc4PointToEccPoint(Q, P4);
  if CnEccPointsEqual(P, Q) then
    ShowMessage('Ed 25519 1000 Mul/ Field64 Extended Mul Equal OK');

  Q.Free;
  P.Free;
  Ed.Free;
end;

procedure TForm25519.btnEd25519GenClick(Sender: TObject);
var
  D: TCnEd25519Data;
begin
  if FEd25519.GenerateKeys(TCnEd25519PrivateKey(F25519PrivKey), TCnEd25519PublicKey(F25519PubKey)) then
  begin
    CnEd25519BigNumberToData(F25519PrivKey, D);
    edtEd25519Priv.Text := DataToHex(@D[0], SizeOf(D));
    CnEd25519PointToData(F25519PubKey, D);
    edtEd25519Pub.Text := DataToHex(@D[0], SizeOf(D));
  end;
end;

procedure TForm25519.btnEd25519SignClick(Sender: TObject);
var
  Priv: TCnEd25519PrivateKey;
  Pub: TCnEd25519PublicKey;
  Data: TCnEd25519Data;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
  S: AnsiString;
begin
  Priv := TCnEd25519PrivateKey.Create;
  Pub := TCnEd25519PublicKey.Create;

  HexToData(edtEd25519Pub.Text, @Data[0]);
  Pub.LoadFromData(Data);

  HexToData(edtEd25519Priv.Text, @Data[0]);
  Priv.LoadFromData(Data);

  Sig := TCnEd25519Signature.Create;

  S := edtEd25519Message.Text;
  if CnEd25519SignData(@S[1], Length(S), Priv, Pub, Sig) then
  begin
    Sig.SaveToData(SigData);
    edtEd25519Sig.Text := DataToHex(@SigData[0], SizeOf(SigData));
    ShowMessage('Sign OK');
  end;

  Sig.Free;
  Pub.Free;
  Priv.Free;
end;

procedure TForm25519.btnEd25519VerifyClick(Sender: TObject);
var
  Pub: TCnEd25519PublicKey;
  Data: TCnEd25519Data;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
  S: AnsiString;
begin
  Pub := TCnEd25519PublicKey.Create;

  HexToData(edtEd25519Pub.Text, @Data[0]);
  Pub.LoadFromData(Data);

  Sig := TCnEd25519Signature.Create;
  HexToData(edtEd25519Sig.Text, @SigData[0]);
  Sig.LoadFromData(SigData);

  S := edtEd25519Message.Text;
  if CnEd25519VerifyData(@S[1], Length(S), Sig, Pub) then
    ShowMessage('Verify OK');

  Sig.Free;
  Pub.Free;
end;

procedure TForm25519.btn25519SignTimeClick(Sender: TObject);
var
  Priv: TCnEd25519PrivateKey;
  Pub: TCnEd25519PublicKey;
  Data: TCnEd25519Data;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
  S: AnsiString;
  T: Cardinal;
  I: Integer;
begin
  Priv := TCnEd25519PrivateKey.Create;
  Pub := TCnEd25519PublicKey.Create;

  HexToData(edtEd25519Pub.Text, @Data[0]);
  Pub.LoadFromData(Data);

  HexToData(edtEd25519Priv.Text, @Data[0]);
  Priv.LoadFromData(Data);

  Sig := TCnEd25519Signature.Create;

  S := edtEd25519Message.Text;
  T := GetTickCount;
  for I := 1 to 1000 do
  begin
    if CnEd25519SignData(@S[1], Length(S), Priv, Pub, Sig) then
      Sig.SaveToData(SigData);
  end;
  T := GetTickCount - T;
  ShowMessage('Sign OK ' + IntToStr(T));
  // 32 位：7 秒签一千次，一次 7 毫秒
  // 64 位：2.5 秒签一千次，一次 2.5 毫秒

  Sig.Free;
  Pub.Free;
  Priv.Free;
end;

procedure TForm25519.btn25519VerifyTimeClick(Sender: TObject);
var
  Pub: TCnEd25519PublicKey;
  PubData: TCnEd25519Data;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
  S: AnsiString;
  T: Cardinal;
  I: Integer;
begin
  Pub := TCnEd25519PublicKey.Create;

  HexToData(edtEd25519Pub.Text, @PubData[0]);
  Pub.LoadFromData(PubData);

  Sig := TCnEd25519Signature.Create;
  HexToData(edtEd25519Sig.Text, @SigData[0]);
  Sig.LoadFromData(SigData);

  S := edtEd25519Message.Text;
  T := GetTickCount;
  for I := 1 to 1000 do
  begin
    if CnEd25519VerifyData(@S[1], Length(S), Sig, Pub) then
      ;
  end;
  T := GetTickCount - T;
  ShowMessage('Verify OK ' + IntToStr(T));
  // 32 位：15 秒验证一千次，一次 15 毫秒
  // 64 位：4.5 秒验证一千次，一次 4.5 毫秒

  Sig.Free;
  Pub.Free;
end;

procedure TForm25519.btnEd25519SignFileClick(Sender: TObject);
var
  Ed: TCnEd25519;
  Priv: TCnEd25519PrivateKey;
  Pub: TCnEd25519PublicKey;
  Data: TCnEd25519Data;
  SigStream: TMemoryStream;
begin
  dlgOpen1.Title := 'Open a File to Sign';
  if dlgOpen1.Execute then
  begin
    Ed := TCnEd25519.Create;
    Priv := TCnEd25519PrivateKey.Create;
    Pub := TCnEd25519PublicKey.Create;

    HexToData(edtEd25519Pub.Text, @Data[0]);
    Pub.LoadFromData(Data);

    HexToData(edtEd25519Priv.Text, @Data[0]);
    Priv.LoadFromData(Data);

    SigStream := TMemoryStream.Create;
    if CnEd25519SignFile(dlgOpen1.FileName, Priv, Pub, SigStream, Ed) then
    begin
      dlgSave1.Title := 'Save Signature to a File';
      dlgSave1.FileName := 'Sig.bin';
      if dlgSave1.Execute then
      begin
        SigStream.SaveToFile(dlgSave1.FileName);
        ShowMessage('Sign OK. Signature Saved to ' + dlgSave1.FileName);
      end;
    end;

    SigStream.Free;
    Pub.Free;
    Priv.Free;
    Ed.Free;
  end;
end;

procedure TForm25519.btnEd25519VerifyFileClick(Sender: TObject);
var
  Ed: TCnEd25519;
  Pub: TCnEd25519PublicKey;
  PubData: TCnEd25519Data;
  SigStream: TMemoryStream;
begin
  dlgOpen1.Title := 'Open a File to Verify';
  dlgSave1.Title := 'Open a Signature File';
  if dlgOpen1.Execute and dlgSave1.Execute then
  begin
    Ed := TCnEd25519.Create;
    Pub := TCnEd25519PublicKey.Create;

    HexToData(edtEd25519Pub.Text, @PubData[0]);
    Pub.LoadFromData(PubData);

    SigStream := TMemoryStream.Create;
    SigStream.LoadFromFile(dlgSave1.FileName);
    SigStream.Position := 0;

    if CnEd25519VerifyFile(dlgOpen1.FileName, SigStream, Pub, Ed) then
      ShowMessage('Verify OK')
    else
      ShowMessage('Verify Fail');

    SigStream.Free;
    Pub.Free;
    Ed.Free;
  end;
end;

procedure TForm25519.btn25519Field64Power2kClick(Sender: TObject);
const
  K = 17;
  DATA = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0FFFFFFFFFFFFFFFFFFFFFFEC';
var
  B, C, P: TCnBigNumber;
  L: Integer;
  D: TCn25519Field64;
begin
  B := TCnBigNumber.FromHex(DATA);
  Cn25519BigNumberToField64(D, B);
  Cn25519Field64Power2K(D, D, K);

  C := TCnBigNumber.Create;
  Cn25519Field64ToBigNumber(C, D);
  ShowMessage(C.ToHex());

  L := 1 shl K;
  P := TCnBigNumber.FromHex(SCN_25519_PRIME);
  BigNumberPowerWordMod(B, B, L, P);

  ShowMessage(B.ToHex());  // 以上俩 B 和 C 相等

  C.SetWord(K);
  B.SetHex(DATA);
  BigNumberPowerMod(B, B, C, P);
  ShowMessage(B.ToHex());          // 大数算出正确值

  B.SetHex(DATA);
  Cn25519BigNumberToField64(D, B);
  Cn25519Field64Power(D, D, K);
  Cn25519Field64ToBigNumber(C, D);
  ShowMessage(C.ToHex());          // 以下仨 B 和 C 相等

  B.SetHex(DATA);
  Cn25519BigNumberToField64(D, B);
  P.SetWord(K);
  Cn25519Field64Power(D, D, P);
  Cn25519Field64ToBigNumber(C, D);
  ShowMessage(C.ToHex());

  P.Free;
  C.Free;
  B.Free;
end;

procedure TForm25519.btn25519Field64PowerPMinus2Click(Sender: TObject);
const
  DATA = '345678909876543456fe1234567098';
var
  B, C, P: TCnBigNumber;
  D: TCn25519Field64;
begin
  P := TCnBigNumber.FromHex(SCN_25519_PRIME);
  B := TCnBigNumber.Create;
  BigNumberCopy(B, P);
  B.SubWord(2);

  C := TCnBigNumber.Create;
  C.SetHex(DATA);
  BigNumberPowerMod(C, C, B, P);  // 计算 C 的 p-2 次方，两者相等
  ShowMessage(C.ToHex());

  C.SetHex(DATA);
  Cn25519BigNumberToField64(D, C);
  Cn25519Field64Power(D, D, B);    // 计算 C 的 p-2 次方
  Cn25519Field64ToBigNumber(C, D);
  ShowMessage(C.ToHex());

  B.SetHex(DATA);
  BigNumberDirectMulMod(C, B, C, P);
  ShowMessage(C.ToHex());          // 得到 1

  P.Free;
  C.Free;
  B.Free;
end;

procedure TForm25519.btnEd25519LoadKeysClick(Sender: TObject);
var
  Data: TCnEd25519Data;
begin
  HexToData(edtEd25519Priv.Text, @Data[0]);
  TCnEd25519PrivateKey(F25519PrivKey).LoadFromData(Data);
  HexToData(edtEd25519Pub.Text, @Data[0]);
  TCnEd25519PublicKey(F25519PubKey).LoadFromData(Data);
end;

procedure TForm25519.btnEd25519SaveKeysClick(Sender: TObject);
var
  Data: TCnEd25519Data;
begin
  TCnEd25519PrivateKey(F25519PrivKey).SaveToData(Data);
  edtEd25519Priv.Text := DataToHex(@Data[0], SizeOf(Data));
  TCnEd25519PublicKey(F25519PubKey).SaveToData(Data);
  edtEd25519Pub.Text := DataToHex(@Data[0], SizeOf(Data));
end;

procedure TForm25519.btn448CheckMapClick(Sender: TObject);
var
  P, U, V, X, Y: TCnBigNumber;
  T1, T2: TCnBigNumber;
begin
// 照理应满足
//  (u, v) = (y^2/x^2, (2 - x^2 - y^2)*y/x^3)
//  (x, y) = (4*v*(u^2 - 1)/(u^4 - 2*u^2 + 4*v^2 + 1),
//            -(u^5 - 2*u^3 - 4*u*v^2 + u)/
//             (u^5 - 2*u^2*v^2 - 2*u^3 - 2*v^2 + u))

  P := TCnBigNumber.FromHex(SCN_448_PRIME);
  U := TCnBigNumber.FromHex(SCN_448_MONT_GU);
  V := TCnBigNumber.FromHex(SCN_448_MONT_GV);
  X := TCnBigNumber.FromHex(SCN_448_EDWARDS_GX);
  Y := TCnBigNumber.FromHex(SCN_448_EDWARDS_GY);

  T1 := TCnBigNumber.Create;
  T2 := TCnBigNumber.Create;

  BigNumberDirectMulMod(T1, X, X, P);
  BigNumberPrimeModularInverse(T1, T1, P);  // T1 得到 1 / x^2
  BigNumberDirectMulMod(T2, Y, Y, P);
  BigNumberDirectMulMod(T2, T1, T2, P);     // T1 得到 y^2 / x^2

  if BigNumberEqual(U, T2) then
    ShowMessage('U Mapping OK');

  BigNumberDirectMulMod(T1, X, X, P);
  BigNumberDirectMulMod(T2, Y, Y, P);
  BigNumberAddMod(T2, T1, T2, P);           // T2 得到 x^2 + y^2 并释放 T1
  BigNumberSubWord(T2, 2);                  // T2 得到 x^2 + y^2 - 2
  BigNumberSubMod(T2, CnBigNumberZero, T2, P);  // 0 - T2 得到 2 - x^2 - y^2
  BigNumberDirectMulMod(T2, T2, Y, P);          // T2 得到分母 (2 - x^2 - y^2)*y

  BigNumberDirectMulMod(T1, X, X, P);
  BigNumberDirectMulMod(T1, T1, X, P);
  BigNumberPrimeModularInverse(T1, T1, P);  // T1 得到 1 / x^3

  BigNumberDirectMulMod(T2, T1, T2, P);     // T2 得到 (2 - x^2 - y^2)*y/x^3
  if BigNumberEqual(V, T2) then
    ShowMessage('V Mapping OK');

  // X OK 和 Y OK 因计算太复杂就不验证了想必一定是对的

  T2.Free;
  T1.Free;

  Y.Free;
  X.Free;
  V.Free;
  U.Free;
  P.Free;
end;

procedure TForm25519.btnCurve25519TestClick(Sender: TObject);
var
  Curve: TCnCurve25519;
  K: TCnBigNumber;
  P: TCnEccPoint;
  D: TCnCurve25519Data;
begin
// a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4 * e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c
// 要 = c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552 后两者均为 u

  HexToData('A546E36BF0527C9D3B16154B82465EDD62144C0AC1FC5A18506A2244BA449AC4', @D[0]);
  K := TCnBigNumber.Create;
  CnCurve25519DataToBigNumber(D, K);
  CnProcess25519ScalarNumber(K);

  P := TCnEccPoint.Create;
  HexToData('E6DB6867583030DB3594C1A424B15F7C726624EC26B3353B10A903A6D0AB1C4C', @D[0]);
  CnCurve25519DataToBigNumber(D, P.X);

  Curve := TCnCurve25519.Create;
  Curve.MultiplePoint(K, P);

  ShowMessage(P.X.ToHex); // 倒过来即得到 C3DA55379DE9C6908E94EA4DF28D084F32ECCF03491C71F754B4075577A28552

  Curve.Free;
  P.Free;
  K.Free;
end;

procedure TForm25519.btnCurve448TestClick(Sender: TObject);
var
  Curve: TCnCurve448;
  K: TCnBigNumber;
  P: TCnEccPoint;
  D: TCnCurve448Data;
begin
// 3d262fddf9ec8e88495266fea19a34d28882acef045104d0d1aae121700a779c984c24f8cdd78fbff44943eba368f54b29259a4f1c600ad3
// * 06fce640fa3487bfda5f6cf2d5263f8aad88334cbd07437f020f08f9814dc031ddbdc38c19c6da2583fa5429db94ada18aa7a7fb4ef8a086
// 要 = ce3e4ff95a60dc6697da1db1d85e6afbdf79b50a2412d7546d5f239fe14fbaadeb445fc66a01b0779d98223961111e21766282f73dd96b6f
// 后两者均为 u

  HexToData('3D262FDDF9EC8E88495266FEA19A34D28882ACEF045104D0D1AAE121700A779C984C24F8CDD78FBFF44943EBA368F54B29259A4F1C600AD3', @D[0]);
  ReverseMemory(@D[0], SizeOf(TCnCurve448Data));
  K := TCnBigNumber.FromBinary(@D[0], SizeOf(TCnCurve448Data));
  CnProcessCurve448ScalarNumber(K);

  P := TCnEccPoint.Create;
  HexToData('06FCE640FA3487BFDA5F6CF2D5263F8AAD88334CBD07437F020F08F9814DC031DDBDC38C19C6DA2583FA5429DB94ADA18AA7A7FB4EF8A086', @D[0]);
  ReverseMemory(@D[0], SizeOf(TCnCurve448Data));
  P.X.SetBinary(@D[0], SizeOf(TCnCurve448Data));

  Curve := TCnCurve448.Create;
  Curve.MultiplePoint(K, P);

  ShowMessage(P.X.ToHex); // 倒过来即得到 CE3E4FF95A60DC6697DA1DB1D85E6AFBDF79B50A2412D7546D5F239FE14FBAADEB445FC66A01B0779D98223961111E21766282F73DD96B6F

  Curve.Free;
  P.Free;
  K.Free;
end;

procedure TForm25519.btnConvert448PointClick(Sender: TObject);
const
  SCN_448_MONT_GU = '05';
  SCN_448_MONT_GV = '7D235D1295F5B1F66C98AB6E58326FCECBAE5D34F55545D060F75DC28DF3F6EDB8027E2346430D211312C4B150677AF76FD7223D457B5B1A';
  SCN_448_EDWARDS_GX = '4F1970C66BED0DED221D15A622BF36DA9E146570470F1767EA6DE324A3D3A46412AE1AF72AB66511433B80E18B00938E2626A82BC70CC05E';
  SCN_448_EDWARDS_GY = '693F46716EB6BC248876203756C9C7624BEA73736CA3984087789C1E05A0C2D73AD3FF1CE67C39C4FDBD132C4ED7C8AD9808795BF230FA14';
var
  PM, PE: TCnEccPoint;

  procedure SetPoints;
  begin
    PM.X.SetHex(SCN_448_MONT_GU);
    PM.Y.SetHex(SCN_448_MONT_GV);
    PE.X.SetHex(SCN_448_EDWARDS_GX);
    PE.Y.SetHex(SCN_448_EDWARDS_GY);
  end;

begin
  PM := TCnEccPoint.Create;
  PE := TCnEccPoint.Create;

  SetPoints;
  CnEd448PointToCurve448Point(PE, PE);
  if CnEccPointsEqual(PE, PM) then
    ShowMessage('Ed 448 to Curve 448 OK');

  SetPoints;
  CnCurve448PointToEd448Point(PM, PM);
  if CnEccPointsEqual(PE, PM) then
    ShowMessage('Curve 448 to Ed 448 OK');

  PE.Free;
  PM.Free;
end;

procedure TForm25519.btnCurve448GOnClick(Sender: TObject);
begin
  if FCurve448.IsPointOnCurve(FCurve448.Generator) then
    ShowMessage('Curve 448 Generator Point is on this Curve');
end;

procedure TForm25519.btnEd448GOnClick(Sender: TObject);
begin
  if FEd448.IsPointOnCurve(FEd448.Generator) then
    ShowMessage('Ed 448 Generator Point is on this Curve');
end;

procedure TForm25519.btnEd448PlainToPointClick(Sender: TObject);
var
  Data: TCnEd448Data;
  P: TCnEccPoint;
begin
  CnEd448PointToData(FEd448.Generator, Data);
  P := TCnEccPoint.Create;
  FEd448.PlainToPoint(Data, P);
  if FEd448.IsPointOnCurve(P) then
    ShowMessage('Point is on Curve.');
  if CnEccPointsEqual(P, FEd448.Generator) then
    ShowMessage('G Plain To Point is G');
  P.Free;
end;

procedure TForm25519.btnAnother448GOnClick(Sender: TObject);
var
  Ed: TCnEd448;
begin
  Ed := TCnEd448.Create;
  Ed.CoefficientD.SetDec('611975850744529176160423220965553317543219696871016626328968936415087860042636474891785599283666020414768678979989378147065462815545017');
  Ed.Generator.X.SetDec('345397493039729516374008604150537410266655260075183290216406970281645695073672344430481787759340633221708391583424041788924124567700732');
  Ed.Generator.Y.SetDec('363419362147803445274661903944002267176820680343659030140745099590306164083365386343198191849338272965044442230921818680526749009182718');

//  Ed.CoefficientD.SetDec('-39081');
//  Ed.Generator.X.SetDec('224580040295924300187604334099896036246789641632564134246125461686950415467406032909029192869357953282578032075146446173674602635247710');
//  Ed.Generator.Y.SetDec('298819210078481492676017930443930673437544040154080242095928241372331506189835876003536878655418784733982303233503462500531545062832660');

  if Ed.IsPointOnCurve(Ed.Generator) then
    ShowMessage('New Point is on Curve.');

  Ed.Free;
end;

// =============================================================================
//
//          Curve448 的 u v 和 RFC 上另一款 Ed448 的 x y 的双向映射关系为：
//
//         (u, v) = ((y-1)/(y+1), sqrt(156324)*u/x)
//         (x, y) = (sqrt(156324)*u/v, (1+u)/(1-u))
//
// =============================================================================

procedure MyCurve448PointToEd448Point(DestPoint, SourcePoint: TCnEccPoint);
var
  T1, T2, S, P, TX: TCnBigNumber;
begin
  // x = s * u / v
  // y =  (1+u)/(1-u)

  T1 := nil;
  T2 := nil;
  S := nil;
  P := nil;
  TX := nil;

  try
    T1 := TCnBigNumber.Create;
    T2 := TCnBigNumber.Create;
    S := TCnBigNumber.Create;
    P := TCnBigNumber.Create;
    TX := TCnBigNumber.Create;

    P.SetHex(SCN_448_PRIME);
    T1.SetHex(SCN_448_SQRT_156324);
    BigNumberDirectMulMod(T1, T1, SourcePoint.X, P);        // T1 得到 s * u
    BigNumberModularInverse(T2, SourcePoint.Y, P);          // T2 得到 1 / v

    BigNumberDirectMulMod(TX, T1, T2, P);                   // TX 暂存 x 并释放 T1 和 T2

    BigNumberCopy(T1, SourcePoint.X);
    BigNumberCopy(T2, SourcePoint.X);
    BigNumberAddMod(T1, T1, CnBigNumberOne, P);             // T1 得到 1+u
    BigNumberSubMod(T2, CnBigNumberOne, T2, P);             // T2 得到 1-u
    BigNumberModularInverse(T2, T2, P);                     // T2 得到 1/(1-u)

    BigNumberDirectMulMod(DestPoint.Y, T1, T2, P);
    BigNumberCopy(DestPoint.X, TX);
  finally
    TX.Free;
    P.Free;
    S.Free;
    T2.Free;
    T1.Free;
  end;
end;

procedure MyEd448PointToCurve448Point(DestPoint, SourcePoint: TCnEccPoint);
var
  T1, T2, S, P, TX: TCnBigNumber;
begin
  // u = (y-1)/(y+1)
  // v = s * u / x

  T1 := nil;
  T2 := nil;
  S := nil;
  P := nil;
  TX := nil;

  try
    T1 := TCnBigNumber.Create;
    T2 := TCnBigNumber.Create;
    S := TCnBigNumber.Create;
    P := TCnBigNumber.Create;
    TX := TCnBigNumber.Create;

    P.SetHex(SCN_448_PRIME);

    BigNumberCopy(T1, SourcePoint.Y);
    BigNumberCopy(T2, SourcePoint.Y);
    BigNumberAddMod(T1, T1, CnBigNumberOne, P);             // T1 得到 y+1
    BigNumberModularInverse(T1, T1, P);                     // T1 得到 1/(y+1)
    BigNumberSubMod(T2, T2, CnBigNumberOne, P);             // T2 得到 y-1

    BigNumberDirectMulMod(TX, T1, T2, P);                   // TX 暂存 u 并释放 T1 和 T2

    T1.SetHex(SCN_448_SQRT_156324);
    BigNumberDirectMulMod(T1, T1, TX, P);                   // T1 得到 s * u 注意 u 是上面计算的
    BigNumberModularInverse(T2, SourcePoint.X, P);          // T2 得到 1 / x

    BigNumberDirectMulMod(DestPoint.Y, T1, T2, P);          // 算出 v
    BigNumberCopy(DestPoint.X, TX);                         // 塞进 u
  finally
    TX.Free;
    P.Free;
    S.Free;
    T2.Free;
    T1.Free;
  end;
end;

procedure TForm25519.btnConvertAnother448PointClick(Sender: TObject);
const
  SCN_448_MONT_GU = '05';
  SCN_448_MONT_GV = '355293926785568175264127502063783334808976399387714271831880898435169088786967410002932673765864550910142774147268105838985595290606362';
  SCN_448_EDWARDS_GX = '345397493039729516374008604150537410266655260075183290216406970281645695073672344430481787759340633221708391583424041788924124567700732';
  SCN_448_EDWARDS_GY = '363419362147803445274661903944002267176820680343659030140745099590306164083365386343198191849338272965044442230921818680526749009182718';
var
  PM, PE, P: TCnEccPoint;

  procedure SetPoints;
  begin
    PM.X.SetDec(SCN_448_MONT_GU);
    PM.Y.SetDec(SCN_448_MONT_GV);
    PE.X.SetDec(SCN_448_EDWARDS_GX);
    PE.Y.SetDec(SCN_448_EDWARDS_GY);
  end;

begin
  PM := TCnEccPoint.Create;
  PE := TCnEccPoint.Create;
  P := TCnEccPoint.Create;

  SetPoints;
  MyEd448PointToCurve448Point(P, PE);
  if CnEccPointsEqual(P, PM) then
    ShowMessage('My Ed 448 to Curve 448 OK');

  SetPoints;
  MyCurve448PointToEd448Point(P, PM);
  if CnEccPointsEqual(P, PE) then
    ShowMessage('My Curve 448 to Ed 448 OK');

  // Curve448 与 RFC 中另一个 Ed 对应曲线的坐标转换均验证通不过
  P.Free;
  PE.Free;
  PM.Free;
end;

procedure TForm25519.btnCurve448DHKeyExchangeClick(Sender: TObject);
var
  Priv1, Priv2: TCnCurve448PrivateKey;
  Pub1, Pub2: TCnEccPublicKey;
  Key1, Key2, Key1O, Key2O: TCnEccPoint;
  D: TCnCurve448Data;
begin
  Priv1 := nil;
  Priv2 := nil;
  Pub1 := nil;
  Pub2 := nil;
  Key1 := nil;
  Key2 := nil;
  Key1O := nil;
  Key2O := nil;

  try
    Priv1 := TCnCurve448PrivateKey.Create;
    Priv2 := TCnCurve448PrivateKey.Create;
    Pub1 := TCnEccPublicKey.Create;
    Pub2 := TCnEccPublicKey.Create;
    Key1 := TCnEccPoint.Create;
    Key2 := TCnEccPoint.Create;
    Key1O := TCnEccPoint.Create;
    Key2O := TCnEccPoint.Create;

    // 俩 Private Key 来源于 RFC 7748
    HexToData('9a8f4925d1519f5775cf46b04b5800d4ee9ee8bae8bc5565d498c28dd9c9baf574a9419744897391006382a6f127ab1d9ac2d8c0a598726b', @D[0]);
    Priv1.LoadFromData(D);
    HexToData('1c306a7ac2a0e2e0990b294470cba339e6453772b075811d8fad0d1d6927c120bb5ee8972b0d3e21374c9c921b09d1b0366f10b65173992d', @D[0]);
    Priv2.LoadFromData(D);

    CnCurve448KeyExchangeStep1(Priv1, Key1); // 第一方调用，产生 Key 1
    CnCurve448KeyExchangeStep1(Priv2, Key2); // 另一方调用，产生 Key 2

    // Key2 给一，Key1 给另一方

    CnCurve448KeyExchangeStep2(Priv1, Key2, Key1O); // 第一方调用，产生公有 Key 1O
    CnCurve448KeyExchangeStep2(Priv2, Key1, Key2O); // 第一方调用，产生公有 Key 2O

    if CnEccPointsEqual(Key1O, Key2O) then
      ShowMessage('Key Exchange OK '+ Key1O.ToString);
  finally
    Key2O.Free;
    Key1O.Free;
    Key2.Free;
    Key1.Free;
    Pub2.Free;
    Pub1.Free;
    Priv2.Free;
    Priv1.Free;
  end;
end;

procedure TForm25519.btnEd448CalcKeyClick(Sender: TObject);
var
  S, K: TCnBigNumber;
  D: TCnEd448Data;
  Ed: TCnEd448;
  Pub: TCnEd448PublicKey;
begin
  // RFC 8032 中的 Test Vector
  // SECRET KEY: 258cdd4ada32ed9c9ff54e63756ae582fb8fab2ac721f2c8e676a72768513d939f63dddb55609133f29adf86ec9929dccb52c1c5fd2ff7e21b
  // PUBLIC KEY: 3ba16da0c6f2cc1f30187740756f5e798d6bc5fc015d7c63cc9510ee3fd44adc24d8e968b6e46e6f94d19b945361726bd75e149ef09817f580

  S := TCnBigNumber.Create;
  K := TCnBigNumber.Create;
  Ed := TCnEd448.Create;
  Pub := TCnEd448PublicKey.Create;

  HexToData('258CDD4ADA32ED9C9FF54E63756AE582FB8FAB2AC721F2C8E676A72768513D939F63DDDB55609133F29ADF86EC9929DCCB52C1C5FD2FF7E21B', @D[0]);
  CnEd448DataToBigNumber(D, S);
  CnCalcKeysFromEd448PrivateKey(S, CN_448_EDWARDS_BLOCK_BYTESIZE, K, nil);

  Pub.Assign(Ed.Generator);
  Ed.MultiplePoint(K, Pub);

  Pub.SaveToData(D);
  if DataToHex(@D[0], SizeOf(TCnEd448Data)) = '3BA16DA0C6F2CC1F30187740756F5E798D6BC5FC015D7C63CC9510EE3FD44ADC24D8E968B6E46E6F94D19B945361726BD75E149EF09817F580' then
    ShowMessage('Key Calc OK');

  Pub.Free;
  Ed.Free;
  K.Free;
  S.Free;
end;

procedure TForm25519.btnEd448GAddClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FEd448.Generator);
  Q.Assign(FEd448.Generator);

  FEd448.PointAddPoint(P, Q, P);
  // P 是 2*G
  if FEd448.IsPointOnCurve(P) then
    ShowMessage('Curve 448 G + G is on this Curve');

  FEd448.PointAddPoint(P, Q, P);
  // P 是 3*G
  if FEd448.IsPointOnCurve(P) then
    ShowMessage('Ed 448 G + 2*G is on this Curve');

  P.Assign(FEd448.Generator);
  Q.Assign(FEd448.Generator);
  FEd448.PointInverse(Q);

  FEd448.PointAddPoint(P, Q, P);
  if FEd448.IsNeutualPoint(P) then
    ShowMessage('Ed 448 G + -G is Zero');

  Q.Free;
  P.Free;
end;

procedure TForm25519.btnEd448GMulClick(Sender: TObject);
var
  Ed: TCnTwistedEdwardsCurve;
  Prime: TCnBigNumber;
  P: TCnEccPoint;
  P3, P32: TCnEcc3Point;
begin
  Prime := TCnBigNumber.FromHex(SCN_448_PRIME);

  P := TCnEccPoint.Create;
  P.Assign(FEd448.Generator);
  P3 := TCnEcc3Point.Create;

  CnEccPointToEcc3Point(P, P3);
  P.X.SetZero;
  P.Y.SetZero;
  FEd448.AffinePointAddPoint(P3, P3, P3);
  if FEd448.IsAffinePointOnCurve(P3) then
    ShowMessage('Ed 448 Affine G + G P3 is on this Curve');

  CnAffinePointToEccPoint(P3, P, Prime);
  if FEd448.IsPointOnCurve(P) then
    ShowMessage('Ed 448 Affine G + G P is on this Curve');

  P32 := TCnEcc3Point.Create;
  CnEccPointToEcc3Point(FEd448.Generator, P32);
  FEd448.AffinePointAddPoint(P3, P32, P32);
  if FEd448.IsAffinePointOnCurve(P32) then
    ShowMessage('Ed 448 Affine G + G + G P32 is on this Curve');

  FEd448.SetNeutualAffinePoint(P32);                // 中性点
  CnEccPointToEcc3Point(FEd448.Generator, P3);      // G 点
  FEd448.AffinePointAddPoint(P32, P3, P3);          // 简单相加
  if FEd448.IsAffinePointOnCurve(P3) then
    ShowMessage('Ed 448 Affine 0 + G P3 is on this Curve');

  P.Assign(FEd448.Generator);
  CnEccPointToEcc3Point(P, P3);
  FEd448.AffineMultiplePoint(2, P3);
  if FEd448.IsAffinePointOnCurve(P3) then
    ShowMessage('Ed 448 Affine 2*G P3 is on this Curve');

  // 拿原始的扭曲爱德华曲线来算二倍点的原始坐标
  Ed := TCnTwistedEdwardsCurve.Create;
  Ed.Load(SCN_448_EDWARDS_A, SCN_448_EDWARDS_D, SCN_448_PRIME, SCN_448_EDWARDS_GX,
    SCN_448_EDWARDS_GY, SCN_448_ORDER, 4);
  P.Assign(FEd448.Generator);
  Ed.MultiplePoint(2, P);
  if Ed.IsPointOnCurve(P) then
    ShowMessage('TwistedEdwardsCurve 2 * G is on this Curve');

  Ed.Free;
  P32.Free;
  P3.Free;
  P.Free;
  Prime.Free;
end;

procedure TForm25519.btnEd448SignSampleClick(Sender: TObject);
var
  Ed: TCnEd448;
  Data: TCnEd448Data;
  PrivKey: TCnEd448PrivateKey;
  PubKey: TCnEd448PublicKey;
  SigData: TCnEd448SignatureData;
  Sig: TCnEd448Signature;
  B: Byte;
  R: Boolean;
begin
  // RFC 8032 中的 Test Vector
  // Secret Key: c4eab05d357007c632f3dbb48489924d552b08fe0c353a0d4a1f00acda2c463afbea67c5e8d2877c5e3bc397a659949ef8021e954e0a12274e
  // Public Key: 43ba28f430cdff456ae531545f7ecd0ac834a55d9358c0372bfa0c6c6798c0866aea01eb00742802b8438ea4cb82169c235160627b4c3a9480
  // Message 1 Byte: 03
  // User Context: nil

  Ed := TCnEd448.Create;
  PrivKey := TCnEd448PrivateKey.Create;
  PubKey := TCnEd448PublicKey.Create;
  Sig := TCnEd448Signature.Create;

  try
    HexToData('C4EAB05D357007C632F3DBB48489924D552B08FE0C353A0D4A1F00ACDA2C463AFBEA67C5E8D2877C5E3BC397A659949EF8021E954E0A12274E', @Data[0]);
    PrivKey.LoadFromData(Data);
    HexToData('43BA28F430CDFF456AE531545F7ECD0AC834A55D9358C0372BFA0C6C6798C0866AEA01EB00742802B8438EA4CB82169C235160627B4C3A9480', @Data[0]);
    PubKey.LoadFromData(Data);

    B := $03;
    R := CnEd448SignData(@B, 1, PrivKey, PubKey, Sig); // 无 UserContext
    if not R then Exit;

    Sig.SaveToData(SigData);
    R := DataToHex(@SigData, SizeOf(SigData)) = '26B8F91727BD62897AF15E41EB43C377EFB9C610D48F2335CB0BD0087810F4352541B143C4B981B7E18F62DE8CCDF633FC1BF037AB7CD779805E0DBCC0AAE1CBCEE1AFB2E027DF36BC04DCECBF154336C19F0AF7E0A6472905E799F1953D2A0FF3348AB21AA4ADAFD1D234441CF807C03A00';
    if not R then Exit;

    ShowMessage('Sign OK');
    R := CnEd448VerifyData(@B, 1, Sig, PubKey);
    if R then
      ShowMessage('Verify OK');
  finally
    Sig.Free;
    PubKey.Free;
    PrivKey.Free;
    Ed.Free;
  end;
end;

procedure TForm25519.btnEd448GenClick(Sender: TObject);
var
  D: TCnEd448Data;
begin
  if FEd448.GenerateKeys(TCnEd448PrivateKey(F448PrivKey), TCnEd448PublicKey(F448PubKey)) then
  begin
    CnEd448BigNumberToData(F448PrivKey, D);
    edtEd448Priv.Text := DataToHex(@D[0], SizeOf(D));
    CnEd448PointToData(F448PubKey, D);
    edtEd448Pub.Text := DataToHex(@D[0], SizeOf(D));
  end;
end;

procedure TForm25519.btnEd448LoadKeysClick(Sender: TObject);
var
  Data: TCnEd448Data;
begin
  HexToData(edtEd448Priv.Text, @Data[0]);
  TCnEd448PrivateKey(F448PrivKey).LoadFromData(Data);
  HexToData(edtEd448Pub.Text, @Data[0]);
  TCnEd448PublicKey(F448PubKey).LoadFromData(Data);
end;

procedure TForm25519.btnEd448SaveKeysClick(Sender: TObject);
var
  Data: TCnEd448Data;
begin
  TCnEd448PrivateKey(F448PrivKey).SaveToData(Data);
  edtEd448Priv.Text := DataToHex(@Data[0], SizeOf(Data));
  TCnEd448PublicKey(F448PubKey).SaveToData(Data);
  edtEd448Pub.Text := DataToHex(@Data[0], SizeOf(Data));
end;

procedure TForm25519.btnEd448SignFileClick(Sender: TObject);
var
  Ed: TCnEd448;
  Priv: TCnEd448PrivateKey;
  Pub: TCnEd448PublicKey;
  Data: TCnEd448Data;
  SigStream: TMemoryStream;
begin
  dlgOpen1.Title := 'Open a File to Sign';
  if dlgOpen1.Execute then
  begin
    Ed := TCnEd448.Create;
    Priv := TCnEd448PrivateKey.Create;
    Pub := TCnEd448PublicKey.Create;

    HexToData(edtEd448Pub.Text, @Data[0]);
    Pub.LoadFromData(Data);

    HexToData(edtEd448Priv.Text, @Data[0]);
    Priv.LoadFromData(Data);

    SigStream := TMemoryStream.Create;
    if CnEd448SignFile(dlgOpen1.FileName, Priv, Pub, SigStream, nil, Ed) then
    begin
      dlgSave1.Title := 'Save Signature to a File';
      dlgSave1.FileName := 'Sig.bin';
      if dlgSave1.Execute then
      begin
        SigStream.SaveToFile(dlgSave1.FileName);
        ShowMessage('Sign OK. Signature Saved to ' + dlgSave1.FileName);
      end;
    end;

    SigStream.Free;
    Pub.Free;
    Priv.Free;
    Ed.Free;
  end;
end;

procedure TForm25519.btnEd448SignClick(Sender: TObject);
var
  Priv: TCnEd448PrivateKey;
  Pub: TCnEd448PublicKey;
  Data: TCnEd448Data;
  Sig: TCnEd448Signature;
  SigData: TCnEd448SignatureData;
  S: AnsiString;
begin
  Priv := TCnEd448PrivateKey.Create;
  Pub := TCnEd448PublicKey.Create;

  HexToData(edtEd448Pub.Text, @Data[0]);
  Pub.LoadFromData(Data);

  HexToData(edtEd448Priv.Text, @Data[0]);
  Priv.LoadFromData(Data);

  Sig := TCnEd448Signature.Create;

  S := edtEd448Message.Text;
  if CnEd448SignData(@S[1], Length(S), Priv, Pub, Sig) then
  begin
    Sig.SaveToData(SigData);
    edtEd448Sig.Text := DataToHex(@SigData[0], SizeOf(SigData));
    ShowMessage('Sign OK');
  end;

  Sig.Free;
  Pub.Free;
  Priv.Free;
end;

procedure TForm25519.btnEd448VerifyClick(Sender: TObject);
var
  Pub: TCnEd448PublicKey;
  Data: TCnEd448Data;
  Sig: TCnEd448Signature;
  SigData: TCnEd448SignatureData;
  S: AnsiString;
begin
  Pub := TCnEd448PublicKey.Create;

  HexToData(edtEd448Pub.Text, @Data[0]);
  Pub.LoadFromData(Data);

  Sig := TCnEd448Signature.Create;
  HexToData(edtEd448Sig.Text, @SigData[0]);
  Sig.LoadFromData(SigData);

  S := edtEd448Message.Text;
  if CnEd448VerifyData(@S[1], Length(S), Sig, Pub) then
    ShowMessage('Verify OK');

  Sig.Free;
  Pub.Free;
end;

procedure TForm25519.btnEd448VerifyFileClick(Sender: TObject);
var
  Ed: TCnEd448;
  Pub: TCnEd448PublicKey;
  PubData: TCnEd448Data;
  SigStream: TMemoryStream;
begin
  dlgOpen1.Title := 'Open a File to Verify';
  dlgSave1.Title := 'Open a Signature File';
  if dlgOpen1.Execute and dlgSave1.Execute then
  begin
    Ed := TCnEd448.Create;
    Pub := TCnEd448PublicKey.Create;

    HexToData(edtEd448Pub.Text, @PubData[0]);
    Pub.LoadFromData(PubData);

    SigStream := TMemoryStream.Create;
    SigStream.LoadFromFile(dlgSave1.FileName);
    SigStream.Position := 0;

    if CnEd448VerifyFile(dlgOpen1.FileName, SigStream, Pub, nil, Ed) then
      ShowMessage('Verify OK')
    else
      ShowMessage('Verify Fail');

    SigStream.Free;
    Pub.Free;
    Ed.Free;
  end;
end;

procedure TForm25519.btn448SignTimeClick(Sender: TObject);
var
  Priv: TCnEd448PrivateKey;
  Pub: TCnEd448PublicKey;
  Data: TCnEd448Data;
  Sig: TCnEd448Signature;
  SigData: TCnEd448SignatureData;
  S: AnsiString;
  T: Cardinal;
  I: Integer;
begin
  Priv := TCnEd448PrivateKey.Create;
  Pub := TCnEd448PublicKey.Create;

  HexToData(edtEd448Pub.Text, @Data[0]);
  Pub.LoadFromData(Data);

  HexToData(edtEd448Priv.Text, @Data[0]);
  Priv.LoadFromData(Data);

  Sig := TCnEd448Signature.Create;

  S := edtEd448Message.Text;
  T := GetTickCount;
  for I := 1 to 100 do
  begin
    if CnEd448SignData(@S[1], Length(S), Priv, Pub, Sig) then
      Sig.SaveToData(SigData);
  end;
  T := GetTickCount - T;
  ShowMessage('Sign OK ' + IntToStr(T));
  // 32 位：31 秒签一百次，一次三百毫秒
  // 64 位：24 秒签一百次，一次二百五毫秒

  Sig.Free;
  Pub.Free;
  Priv.Free;
end;

procedure TForm25519.btn448VerifyTimeClick(Sender: TObject);
var
  Pub: TCnEd448PublicKey;
  PubData: TCnEd448Data;
  Sig: TCnEd448Signature;
  SigData: TCnEd448SignatureData;
  S: AnsiString;
  T: Cardinal;
  I: Integer;
begin
  Pub := TCnEd448PublicKey.Create;

  HexToData(edtEd448Pub.Text, @PubData[0]);
  Pub.LoadFromData(PubData);

  Sig := TCnEd448Signature.Create;
  HexToData(edtEd448Sig.Text, @SigData[0]);
  Sig.LoadFromData(SigData);

  S := edtEd448Message.Text;
  T := GetTickCount;
  for I := 1 to 100 do
  begin
    if CnEd448VerifyData(@S[1], Length(S), Sig, Pub) then
      ;
  end;
  T := GetTickCount - T;
  ShowMessage('Verify OK ' + IntToStr(T));
  // 32 位：52 秒验证一百次，一次五百毫秒
  // 64 位：50 秒验证一千次，一次五百毫秒

  Sig.Free;
  Pub.Free;
end;

end.
