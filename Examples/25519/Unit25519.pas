unit Unit25519;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnECC, Cn25519, ExtCtrls;

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
  private
    FCurve25519: TCnCurve25519;
    FEd25519: TCnEd25519;
    FPrivKey: TCnEccPrivateKey;
    FPubKey: TCnEccPublicKey;
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
  CnEccPointToEcc4Point(P, P4, FEd25519.FiniteFieldSize);
  CnEccPointToEcc4Point(Q, Q4, FEd25519.FiniteFieldSize);

  FEd25519.ExtendedPointAddPoint(P4, Q4, S4);
  // ShowMessage(S4.ToString);
  FEd25519.PointAddPoint(P, Q, S);

  // 验证 S 和 S4 是否相等
  CnEcc4PointToEccPoint(S4, P, FEd25519.FiniteFieldSize);
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
  CnEccPointToEcc4Point(P, P4, FEd25519.FiniteFieldSize);
  CnEccPointToEcc4Point(Q, Q4, FEd25519.FiniteFieldSize);

  FEd25519.ExtendedPointAddPoint(P4, Q4, S4);
  FEd25519.PointAddPoint(P, Q, S);
  // ShowMessage(S.ToString);

  // 验证 S 和 S4 是否相等
  CnEcc4PointToEccPoint(S4, P, FEd25519.FiniteFieldSize);
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
  CnEccPointToEcc4Point(P, P4, FEd25519.FiniteFieldSize);

  FEd25519.MultiplePoint(M, P);
  FEd25519.ExtendedMultiplePoint(M, P4);

  if FEd25519.IsExtendedPointOnCurve(P4) then
    ShowMessage('Ed 25519 Extended Random * G is on this Curve');

  Q := TCnEccPoint.Create;
  CnEcc4PointToEccPoint(P4, Q, FEd25519.FiniteFieldSize);

  if CnEccPointsEqual(P, Q) then
    ShowMessage('Ed 25519 Mul/ExtendedMul Equal OK');

  T1 := GetTickCount;
  for I := 1 to 1000 do
    FEd25519.MultiplePoint(M, P);
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 1 to 1000 do
    FEd25519.ExtendedMultiplePoint(M, P4);
  T2 := GetTickCount - T2;

  ShowMessage(Format('Normal %d, Extended %d', [T1, T2])); // Extended 快十倍以上！
  CnEcc4PointToEccPoint(P4, Q, FEd25519.FiniteFieldSize);
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
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
begin
  B := $72;
  Sig := TCnEd25519Signature.Create;
  if CnEd25519SignData(@B, 1, FPrivKey, FPubKey, Sig) then
  begin
    ShowMessage('Sign OK');
    Sig.ToArray(SigData);
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

end.
