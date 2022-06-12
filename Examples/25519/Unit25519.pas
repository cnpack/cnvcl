unit Unit25519;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Clipbrd, CnBigNumber, CnECC, Cn25519, ExtCtrls, CnNativeDecl;

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
  private
    FCurve25519: TCnCurve25519;
    FEd25519: TCnEd25519;
    FPrivKey: TCnEccPrivateKey;
    FPubKey: TCnEccPublicKey;
    FSigData: TCnEd25519SignatureData;
  public

  end;

var
  Form25519: TForm25519;

implementation

{$R *.DFM}

procedure TForm25519.FormCreate(Sender: TObject);
begin
  FCurve25519 := TCnCurve25519.Create;
  FEd25519 := TCnEd25519.Create;
  FPrivKey := TCnEccPrivateKey.Create;
  FPubKey := TCnEccPublicKey.Create;
end;

procedure TForm25519.FormDestroy(Sender: TObject);
begin
  FPubKey.Free;
  FPrivKey.Free;
  FEd25519.Free;
  FCurve25519.Free;
end;

procedure TForm25519.btnCurve25519GClick(Sender: TObject);
begin
  if FCurve25519.IsPointOnCurve(FCurve25519.Generator) then
    ShowMessage('Curve 25519 Generator Point is on this Curve');
end;

procedure TForm25519.btnEd25519GClick(Sender: TObject);
begin
  if FEd25519.IsPointOnCurve(FEd25519.Generator) then
    ShowMessage('Ed 25519 Generator Point is on this Curve');
end;

procedure TForm25519.btnCurve25519GAddClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);

  if FCurve25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 2*G
    if FCurve25519.IsPointOnCurve(P) then
      ShowMessage('Curve 25519 G + G is on this Curve');
  end;

  if FCurve25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 3*G
    if FCurve25519.IsPointOnCurve(P) then
      ShowMessage('Curve 25519 G + 2*G is on this Curve');
  end;

  P.Assign(FCurve25519.Generator);
  Q.Assign(FCurve25519.Generator);
  FCurve25519.PointInverse(Q);

  if FCurve25519.PointAddPoint(P, Q, P) then
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

  if FEd25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 2*G
    if FEd25519.IsPointOnCurve(P) then
      ShowMessage('Curve 25519 G + G is on this Curve');
  end;

  if FEd25519.PointAddPoint(P, Q, P) then
  begin
    // P 是 3*G
    if FEd25519.IsPointOnCurve(P) then
      ShowMessage('Ed 25519 G + 2*G is on this Curve');
  end;

  P.Assign(FEd25519.Generator);
  Q.Assign(FEd25519.Generator);
  FEd25519.PointInverse(Q);

  if FEd25519.PointAddPoint(P, Q, P) then
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
  if FCurve25519.PointSubPoint(P, Q, Q) then // Q 是 2*G
    if FCurve25519.IsPointOnCurve(Q) then
      ShowMessage('Curve 25519 3*G - G is on this Curve');

  if FCurve25519.PointSubPoint(P, P, Q) then // Q 是无限远点
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
  if FEd25519.PointSubPoint(P, Q, Q) then // Q 是 2*G
    if FEd25519.IsPointOnCurve(Q) then
      ShowMessage('Curve 25519 3*G - G is on this Curve');

  if FEd25519.PointSubPoint(P, P, Q) then // Q 是无限远点
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
  FEd25519.GenerateKeys(FPrivKey, FPubKey);
  FEd25519.PointToPlain(FPubKey, Data);
  ShowMessage(FPubKey.ToString);
end;

procedure TForm25519.btnEd25519SignSampleClick(Sender: TObject);
var
  B: Byte;
  Sig, ASig: TCnEd25519Signature;
begin
  B := $72;
  Sig := TCnEd25519Signature.Create;
  if CnEd25519SignData(@B, 1, FPrivKey, FPubKey, Sig) then
  begin
    ShowMessage('Sign OK');
    Sig.SaveToData(FSigData);

    ASig := TCnEd25519Signature.Create;
    ASig.LoadFromData(FSigData);

    // 比较 Sig 和 ASig 是否相同
    if CnEccPointsEqual(Sig.R, ASig.R) and BigNumberEqual(Sig.S, ASig.S) then
      ShowMessage('Sig Save/Load OK');

    if CnEd25519VerifyData(@B, 1, Sig, FPubKey) then
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
  Priv1, Priv2: TCnEccPrivateKey;
  Pub1, Pub2: TCnEccPublicKey;
  Key1, Key2, Key1O, Key2O: TCnEccPoint;
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
    Priv1 := TCnEccPrivateKey.Create;
    Priv2 := TCnEccPrivateKey.Create;
    Pub1 := TCnEccPublicKey.Create;
    Pub2 := TCnEccPublicKey.Create;
    Key1 := TCnEccPoint.Create;
    Key2 := TCnEccPoint.Create;
    Key1O := TCnEccPoint.Create;
    Key2O := TCnEccPoint.Create;

    Priv1.SetHex('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a');
    Priv2.SetHex('5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb');

    CnCurve25519KeyExchangeStep1(Priv1, Key1); // 第一方调用，产生 Key 1
    CnCurve25519KeyExchangeStep1(Priv2, Key2); // 另一方调用，产生 Key 2

    // Key2 给一，Key1 给另一方

    CnCurve25519KeyExchangeStep2(Priv1, Key2, Key1O); // 第一方调用，产生公有 Key 1O
    CnCurve25519KeyExchangeStep2(Priv2, Key1, Key2O); // 第一方调用，产生公有 Key 2O

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
    if CnCurve25519PointToEd25519Point(Q, P) then
    begin
      ShowMessage(Q.ToString);
      if FEd25519.IsPointOnCurve(Q) then
        ShowMessage('Converted Ed25519 Point is on Curve');
    end;

    P.Assign(FEd25519.Generator);
    if CnEd25519PointToCurve25519Point(Q, P) then
    begin
      ShowMessage(Q.ToString);
      if FCurve25519.IsPointOnCurve(Q) then
        ShowMessage('Converted Curve25519 Point is on Curve');
    end;
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

end.
