unit UnitEcc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnECC, ExtCtrls, Buttons, TeEngine, Series, TeeProcs,
  Chart, TypInfo, CnPrimeNumber, CnBigNumber, CnNativeDecl, CnCommon, CnPemUtils;

type
  TFormEcc = class(TForm)
    pgc1: TPageControl;
    tsSimpleECC: TTabSheet;
    grpSimpleECC: TGroupBox;
    btnTest1: TButton;
    btnTest0: TButton;
    btnTestOn: TButton;
    btnTestInverse: TButton;
    btnTest2P: TButton;
    btnTestMul: TButton;
    Bevel1: TBevel;
    lblPX: TLabel;
    lblPY: TLabel;
    edtPX: TEdit;
    edtPY: TEdit;
    lblAdd: TLabel;
    edtQX: TEdit;
    lblQY: TLabel;
    edtQY: TEdit;
    lblAddResult: TLabel;
    btnEqual: TButton;
    lblMPX: TLabel;
    edtMPX: TEdit;
    lblMPY: TLabel;
    edtMPY: TEdit;
    lblMul: TLabel;
    btnMEqual: TButton;
    edtMK: TEdit;
    lblMResult: TLabel;
    btnPOn: TSpeedButton;
    btnMPOn: TSpeedButton;
    btnQOn: TSpeedButton;
    chtE2311: TChart;
    pntsrsSeries1: TPointSeries;
    btnNewKey: TButton;
    lblPrivateKey: TLabel;
    edtPrivateKey: TEdit;
    edtPublicKey: TEdit;
    lblPublicKey: TLabel;
    lblData: TLabel;
    edtData: TEdit;
    btnEncrypt: TButton;
    edtEncrypted: TEdit;
    btnDecrypt: TButton;
    edtDecrypted: TEdit;
    btnBatchVerify: TButton;
    tsInt64ECC: TTabSheet;
    Bevel2: TBevel;
    lblECDH: TLabel;
    lblDHA: TLabel;
    lblDHB: TLabel;
    lblXb: TLabel;
    lblXA: TLabel;
    edtDHXa: TEdit;
    edtDHXb: TEdit;
    btnCalcYb: TButton;
    btnCalcXA: TButton;
    edtDHYa: TEdit;
    edtDHYb: TEdit;
    btnDHBCK: TButton;
    btnDHACKey: TButton;
    edtAKey: TEdit;
    edtBKey: TEdit;
    grpGenEcc: TGroupBox;
    btnGenEcc: TButton;
    lblEccY2: TLabel;
    edtEccA: TEdit;
    edtEccB: TEdit;
    edtEccP: TEdit;
    lblEccG: TLabel;
    edtEccGX: TEdit;
    edtEccGY: TEdit;
    lblEccOrder: TLabel;
    edtEccOrder: TEdit;
    btnCalcNG: TButton;
    Bevel3: TBevel;
    mmoGenECCPoints: TMemo;
    chtEccInt64: TChart;
    pntsrsSeries2: TPointSeries;
    btnLeRanDe: TButton;
    tsECC: TTabSheet;
    grpBNEcc: TGroupBox;
    lblBNEqu: TLabel;
    edtBNEccA: TEdit;
    edtBNEccB: TEdit;
    edtBNEccP: TEdit;
    lblBNGX: TLabel;
    edtBNEccGX: TEdit;
    edtBNEccGY: TEdit;
    lblBNGY: TLabel;
    lblBNEccOrder: TLabel;
    edtBNEccOrder: TEdit;
    Bevel4: TBevel;
    btnBNEccInverseG: TButton;
    edtBNEccResult: TEdit;
    btnBNEccInverseAdd: TButton;
    btnBNEccGx2: TButton;
    btnBNEccG2SubG: TButton;
    btnBNEccGAddG: TButton;
    btnBNEccGSubG: TButton;
    lblBNEccB: TLabel;
    lblBNEccMod: TLabel;
    btnBNEccNG: TButton;
    btnBNEcc4G: TButton;
    btnBNEccNewKey: TButton;
    lblBNEccPrivateKey: TLabel;
    edtBNEccPublicKey: TEdit;
    edtBNEccPrivateKey: TEdit;
    lblBNEccPublicKey: TLabel;
    lblBNEccDataPoint: TLabel;
    edtBNEccDataPoint: TEdit;
    btnBNEccCrypt: TButton;
    tsWrapData: TTabSheet;
    grpWrap: TGroupBox;
    lblWrapData: TLabel;
    edtWrapData: TEdit;
    btnWrapData: TButton;
    edtWrapPoint: TEdit;
    bvl1: TBevel;
    lblBNECDH: TLabel;
    lblBNECDHA: TLabel;
    lblBNECDHB: TLabel;
    lblBNECDHXb: TLabel;
    lblBNECDHXa: TLabel;
    edtBNECDHXa: TEdit;
    edtBNECDHXb: TEdit;
    btnBNECDHYb: TButton;
    btnBNECDHYa: TButton;
    edtBNECDHA: TEdit;
    edtBNECDHB: TEdit;
    btnBNECDHBkey: TButton;
    btnBNECDHAKey: TButton;
    edtBNECDHResA: TEdit;
    edtBNECDHResB: TEdit;
    btnTestECDH: TButton;
    btnBNEccWrapRange: TButton;
    btnBNGXtoPoint: TButton;
    btnInt64GXtoPt: TButton;
    tsLucas: TTabSheet;
    grpLucas: TGroupBox;
    lblLucasX: TLabel;
    edtLucasX: TEdit;
    lblLucasY: TLabel;
    edtLucasY: TEdit;
    edtLucasP: TEdit;
    lblLucasP: TLabel;
    btnLucasRecur: TButton;
    mmoLucasRes: TMemo;
    mmoLucasMod: TMemo;
    btnLucasMod: TButton;
    grpLegendre: TGroupBox;
    btnCalcLegendre: TButton;
    mmoLegendre: TMemo;
    mmoLegendreRes1: TMemo;
    mmoLegendreRes2: TMemo;
    tsTonelliShanks: TTabSheet;
    grpTonelliShanks: TGroupBox;
    lblTSX: TLabel;
    edtTSX: TEdit;
    lblTSP: TLabel;
    edtTSP: TEdit;
    btnTSInt64: TButton;
    btnBNTS: TButton;
    mmoTSData: TMemo;
    btnRandomTS: TButton;
    chkLucasMod: TCheckBox;
    tsSquareRoot: TTabSheet;
    grpSquareRoot: TGroupBox;
    lblSRY: TLabel;
    edtSRY: TEdit;
    lblSREqual: TLabel;
    edtSRX: TEdit;
    lblSRMod: TLabel;
    edtSRP: TEdit;
    btnSRLucas: TButton;
    btnSRCompare: TButton;
    btnBNLucasMod: TButton;
    dlgOpen1: TOpenDialog;
    tsPem: TTabSheet;
    grpEccKeys: TGroupBox;
    btnLoad: TButton;
    dlgSave1: TSaveDialog;
    lblCurveType: TLabel;
    lblCurveTypeText: TLabel;
    edtKeyEccA: TEdit;
    edtKeyEccB: TEdit;
    edtKeyEccP: TEdit;
    edtKeyEccGX: TEdit;
    edtKeyEccGY: TEdit;
    edtKeyEccOrder: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    bvlKey: TBevel;
    lblKeyPrivate: TLabel;
    lblKeyPublic: TLabel;
    edtKeyPrivate: TEdit;
    edtKeyPublic: TEdit;
    btnKeyCheckPublic: TButton;
    btnSaveKey: TButton;
    procedure btnTest1Click(Sender: TObject);
    procedure btnTest0Click(Sender: TObject);
    procedure btnTestOnClick(Sender: TObject);
    procedure btnTestInverseClick(Sender: TObject);
    procedure btnTest2PClick(Sender: TObject);
    procedure btnTestMulClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEqualClick(Sender: TObject);
    procedure btnMEqualClick(Sender: TObject);
    procedure btnPOnClick(Sender: TObject);
    procedure btnQOnClick(Sender: TObject);
    procedure btnMPOnClick(Sender: TObject);
    procedure btnNewKeyClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnBatchVerifyClick(Sender: TObject);
    procedure btnCalcXAClick(Sender: TObject);
    procedure btnCalcYbClick(Sender: TObject);
    procedure btnDHACKeyClick(Sender: TObject);
    procedure btnDHBCKClick(Sender: TObject);
    procedure btnGenEccClick(Sender: TObject);
    procedure btnCalcNGClick(Sender: TObject);
    procedure btnLeRanDeClick(Sender: TObject);
    procedure btnBNEccInverseGClick(Sender: TObject);
    procedure btnBNEccInverseAddClick(Sender: TObject);
    procedure btnBNEccGx2Click(Sender: TObject);
    procedure btnBNEccG2SubGClick(Sender: TObject);
    procedure btnBNEccGAddGClick(Sender: TObject);
    procedure btnBNEccGSubGClick(Sender: TObject);
    procedure btnBNEccNGClick(Sender: TObject);
    procedure btnBNEcc4GClick(Sender: TObject);
    procedure btnBNEccNewKeyClick(Sender: TObject);
    procedure lblBNEccDataPointClick(Sender: TObject);
    procedure btnBNEccCryptClick(Sender: TObject);
    procedure btnWrapDataClick(Sender: TObject);
    procedure btnBNECDHYaClick(Sender: TObject);
    procedure btnBNECDHYbClick(Sender: TObject);
    procedure btnBNECDHAKeyClick(Sender: TObject);
    procedure btnBNECDHBkeyClick(Sender: TObject);
    procedure btnTestECDHClick(Sender: TObject);
    procedure btnBNEccWrapRangeClick(Sender: TObject);
    procedure btnBNGXtoPointClick(Sender: TObject);
    procedure btnInt64GXtoPtClick(Sender: TObject);
    procedure btnLucasRecurClick(Sender: TObject);
    procedure btnLucasModClick(Sender: TObject);
    procedure btnCalcLegendreClick(Sender: TObject);
    procedure btnTSInt64Click(Sender: TObject);
    procedure btnBNTSClick(Sender: TObject);
    procedure btnSRLucasClick(Sender: TObject);
    procedure btnSRCompareClick(Sender: TObject);
    procedure btnBNLucasModClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnKeyCheckPublicClick(Sender: TObject);
    procedure btnSaveKeyClick(Sender: TObject);
  private
    FEcc64E2311: TCnInt64Ecc;
    FEcc64E2311Points: array[0..23] of array [0..23] of Boolean;
    FEcc64PrivateKey: TCnInt64PrivateKey;
    FEcc64PublicKey: TCnInt64PublicKey;
    FEcc64Enc1, FEcc64Enc2: TCnInt64EccPoint;

    FBNEcc: TCnEcc;
    FBNEccPrivateKey: TCnEccPrivateKey;
    FBNEccPublicKey: TCnEccPublicKey;
    FBNEccDataPoint: TCnEccPoint;
    FBNECDHPrivKey1, FBNECDHPrivKey2: TCnEccPrivateKey;
    FBNECDHPubKey1, FBNECDHPubKey2: TCnEccPublicKey;
    procedure CalcE2311Points;
    procedure UpdateE2311Chart;
    procedure ShowBnEcc;
    procedure CalcLucas(X, Y, U_2, V_2, U_1, V_1: Int64; var U, V: Int64);
  public
    { Public declarations }
  end;

var
  FormEcc: TFormEcc;

implementation

{$R *.DFM}

const
  CN_ECC_PLAIN_DATA_BITS_GAP = 24;

var
  FPrivateKey: TCnEccPrivateKey;
  FPublicKey: TCnEccPublicKey;
  FCurveType: TCnEccCurveType;
  FKeyEcc: TCnEcc;

procedure TFormEcc.btnTest1Click(Sender: TObject);
var
  P, Q: TCnInt64EccPoint;
begin
  P.X := 3; P.Y := 10;
  Q.X := 9; Q.Y := 7;

  FEcc64E2311.PointAddPoint(P, Q, P);
  ShowMessage(Format('3,10 + 9,7 = %d,%d',[P.X, P.Y]));
end;

procedure TFormEcc.btnTest0Click(Sender: TObject);
var
  P, Q: TCnInt64EccPoint;
begin
  P.X := 0; P.Y := 0;
  Q.X := StrToInt(edtPX.Text);
  Q.Y := StrToInt(edtPY.Text);

  FEcc64E2311.PointAddPoint(P, Q, P);
  ShowMessage(Format('0,0 + P = %d,%d',[P.X, P.Y]));
end;

procedure TFormEcc.btnTestOnClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  if FEcc64E2311.IsPointOnCurve(P) then
    ShowMessage('P is On Curve')
  else
    ShowMessage('P is NOT On Curve');
end;

procedure TFormEcc.btnTestInverseClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  FEcc64E2311.PointInverse(P);
  ShowMessage('P Inverse to ' + IntToStr(P.X) + ',' + IntToStr(P.Y));
end;

procedure TFormEcc.btnTest2PClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  FEcc64E2311.MultiplePoint(2, P);
  ShowMessage('P Multiple 2 is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));
end;

procedure TFormEcc.btnTestMulClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
  I: Integer;
  List: TStringList;
begin
  List := TStringList.Create;

  for I := 0 to 30 do
  begin
    P.X := StrToInt(edtPX.Text);
    P.Y := StrToInt(edtPY.Text);
    FEcc64E2311.MultiplePoint(I, P);

    List.Add(IntToStr(I) + '*: ' + IntToStr(P.X) + ',' + IntToStr(P.Y));
    if FEcc64E2311.IsPointOnCurve(P) then
      List[List.Count - 1] := List[List.Count - 1] + ' On Curve.'
    else
      List[List.Count - 1] := List[List.Count - 1] + ' NOT On Curve.'
  end;

  ShowMessage('P Multiple is '#13#10#13#10 + List.Text);
  List.Free;
end;

procedure TFormEcc.FormCreate(Sender: TObject);
begin
  pgc1.ActivePageIndex := 0;

  FEcc64E2311 := TCnInt64Ecc.Create(1, 1, 23, 5, 4, 7);
  // 9,7 为基点，28 是该曲线的阶，选其素因数 7 作为基点的阶，基点为 5,4
  CalcE2311Points;
  UpdateE2311Chart;

  FBNEcc := TCnEcc.Create(ctSecp224r1);
  FBNEccPrivateKey := TCnEccPrivateKey.Create;
  FBNEccPublicKey := TCnEccPublicKey.Create;
  FBNEccDataPoint := TCnEccPoint.Create;

  FBNECDHPrivKey1 := TCnEccPrivateKey.Create;
  FBNECDHPrivKey2 := TCnEccPrivateKey.Create;
  FBNECDHPubKey1 := TCnEccPublicKey.Create;
  FBNECDHPubKey2 := TCnEccPublicKey.Create;
  ShowBnEcc;
  edtWrapData.Text := edtBNEccGX.Text;

  lblBNEccDataPoint.OnClick(lblBNEccDataPoint);

  FPrivateKey := TCnEccPrivateKey.Create;
  FPublicKey := TCnEccPublicKey.Create;
  FKeyEcc := TCnEcc.Create;
end;

procedure TFormEcc.FormDestroy(Sender: TObject);
begin
  FKeyEcc.Free;
  FPrivateKey.Free;
  FPublicKey.Free;

  FBNECDHPrivKey1.Free;
  FBNECDHPrivKey2.Free;
  FBNECDHPubKey1.Free;
  FBNECDHPubKey2.Free;
  FBNEccDataPoint.Free;
  FBNEccPrivateKey.Free;
  FBNEccPublicKey.Free;
  FBNEcc.Free;
  FEcc64E2311.Free;
end;

procedure TFormEcc.btnEqualClick(Sender: TObject);
var
  P, Q: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);
  Q.X := StrToInt(edtQX.Text);
  Q.Y := StrToInt(edtQY.Text);
  FEcc64E2311.PointAddPoint(P, Q, P);
  lblAddResult.Caption := IntToStr(P.X) + ',' + IntToStr(P.Y);
end;

procedure TFormEcc.btnMEqualClick(Sender: TObject);
var
  K: Integer;
  P: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtMPX.Text);
  P.Y := StrToInt(edtMPY.Text);
  K := StrToInt(edtMK.Text);
  FEcc64E2311.MultiplePoint(K, P);
  lblMResult.Caption := IntToStr(P.X) + ',' + IntToStr(P.Y);
end;

procedure TFormEcc.btnPOnClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);
  if FEcc64E2311.IsPointOnCurve(P) then
    ShowMessage(Format('%d,%d is On Curve.', [P.X, P.Y]));
end;

procedure TFormEcc.btnQOnClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtQX.Text);
  P.Y := StrToInt(edtQY.Text);
  if FEcc64E2311.IsPointOnCurve(P) then
    ShowMessage(Format('%d,%d is On Curve.', [P.X, P.Y]));
end;

procedure TFormEcc.btnMPOnClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  P.X := StrToInt(edtMPX.Text);
  P.Y := StrToInt(edtMPY.Text);
  if FEcc64E2311.IsPointOnCurve(P) then
    ShowMessage(Format('%d,%d is On Curve.', [P.X, P.Y]));
end;

procedure TFormEcc.CalcE2311Points;
var
  I, J: Integer;
  P: TCnInt64EccPoint;
begin
  for I := 0 to 22 do
  begin
    for J := 0 to 22 do
    begin
      P.X := I;
      P.Y := J;
      FEcc64E2311Points[I][J] := FEcc64E2311.IsPointOnCurve(P);
    end;
  end;
end;

procedure TFormEcc.UpdateE2311Chart;
var
  I, J: Integer;
begin
  for I := 0 to 23 do
  begin
    for J := 0 to 23 do
    begin
      if FEcc64E2311Points[I][J] then
        chtE2311.SeriesList.Series[0].AddXY(I, J);
    end;
  end;

end;

procedure TFormEcc.btnNewKeyClick(Sender: TObject);
begin
  FEcc64E2311.GenerateKeys(FEcc64PrivateKey, FEcc64PublicKey);
  edtPrivateKey.Text := IntToStr(FEcc64PrivateKey);
  edtPublicKey.Text := IntToStr(FEcc64PublicKey.X) + ',' + IntToStr(FEcc64PublicKey.Y);
end;

procedure TFormEcc.btnEncryptClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  if FEcc64E2311.PlainToPoint(StrToInt(edtData.Text), P) then
  begin
    ShowMessage(edtData.Text + ' Convert to ' + CnInt64EccPointToString(P));
    FEcc64E2311.Encrypt(P, FEcc64PublicKey, FEcc64Enc1, FEcc64Enc2);
    edtEncrypted.Text := CnInt64EccPointToString(FEcc64Enc1) + '  ' + CnInt64EccPointToString(FEcc64Enc2);
  end
  else
    ShowMessage('Can NOT Convert Data to Point.');
end;

procedure TFormEcc.btnDecryptClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
begin
  FEcc64E2311.Decrypt(FEcc64Enc1, FEcc64Enc2, FEcc64PrivateKey, P);
  ShowMessage('Decrypt to ' + CnInt64EccPointToString(P));
  edtDecrypted.Text := IntToStr(P.X);
end;

procedure TFormEcc.btnBatchVerifyClick(Sender: TObject);
var
  I: Integer;
  P, Q: TCnInt64EccPoint;
begin
  for I := 1 to 27 do
  begin
    FEcc64E2311.PlainToPoint(StrToInt(edtData.Text), P);
    FEcc64E2311.Encrypt(P, FEcc64PublicKey, FEcc64Enc1, FEcc64Enc2, I);
    FEcc64E2311.Decrypt(FEcc64Enc1, FEcc64Enc2, FEcc64PrivateKey, Q);
    if (P.X <> Q.X) or (P.Y <> Q.Y) then
      ShowMessage('Error: ' + IntToStr(I) + ' ' + CnInt64EccPointToString(Q));
  end;
end;

var
  FAOutPub: TCnInt64PublicKey;
  FBOutPub: TCnInt64PublicKey;

procedure TFormEcc.btnCalcXAClick(Sender: TObject);
var
  InPriv: TCnInt64PrivateKey;
begin
  InPriv := StrToInt(edtDHXa.Text);
  CnInt64EccDiffieHellmanGenerateOutKey(FEcc64E2311, InPriv, FAOutPub);
  edtDHYa.Text := CnInt64EccPointToString(FAOutPub);
end;

procedure TFormEcc.btnCalcYbClick(Sender: TObject);
var
  InPriv: TCnInt64PrivateKey;
begin
  InPriv := StrToInt(edtDHXb.Text);
  CnInt64EccDiffieHellmanGenerateOutKey(FEcc64E2311, InPriv, FBOutPub);
  edtDHYb.Text := CnInt64EccPointToString(FBOutPub);
end;

procedure TFormEcc.btnDHACKeyClick(Sender: TObject);
var
  InPriv: TCnInt64PrivateKey;
  Sec: TCnInt64PublicKey;
begin
  InPriv := StrToInt(edtDHXa.Text);
  CnInt64EccDiffieHellmanComputeKey(FEcc64E2311, InPriv, FBOutPub, Sec);
  edtAKey.Text := CnInt64EccPointToString(Sec);
end;

procedure TFormEcc.btnDHBCKClick(Sender: TObject);
var
  InPriv: TCnInt64PrivateKey;
  Sec: TCnInt64PublicKey;
begin
  InPriv := StrToInt(edtDHXb.Text);
  CnInt64EccDiffieHellmanComputeKey(FEcc64E2311, InPriv, FAOutPub, Sec);
  edtBKey.Text := CnInt64EccPointToString(Sec);
end;

procedure TFormEcc.btnGenEccClick(Sender: TObject);
var
  P, A, B, X, Y, N: Int64;
begin
  if CnInt64EccGenerateParams(P, A, B, X, Y, N) then
  begin
    edtEccA.Text := IntToStr(A);
    edtEccB.Text := IntToStr(B);
    edtEccGX.Text := IntToStr(X);
    edtEccGY.Text := IntToStr(Y);
    edtEccP.Text := IntToStr(P);
    edtEccOrder.Text := IntToStr(N);
  end;
end;

procedure TFormEcc.btnCalcNGClick(Sender: TObject);
var
  P, A, B, X, Y, N, X1, Y1: Int64;
  Ecc: TCnInt64Ecc;
  I: Integer;
  Q: TCnInt64EccPoint;
  List: TStrings;
begin
  A := StrToInt(edtEccA.Text);
  B := StrToInt(edtEccB.Text);
  X := StrToInt(edtEccGX.Text);
  Y := StrToInt(edtEccGY.Text);
  P := StrToInt(edtEccP.Text);
  N := StrToInt(edtEccOrder.Text);

  Ecc := TCnInt64Ecc.Create(A, B, P, X, Y, N);
  mmoGenECCPoints.Lines.Clear;
  chtEccInt64.SeriesList[0].Clear;
  chtEccInt64.BottomAxis.Maximum := P - 1;
  chtEccInt64.LeftAxis.Maximum := P - 1;

  List := TStringList.Create;
  for I := 0 to N + 1 do
  begin
    Q.X := X;
    Q.Y := Y;
    Ecc.MultiplePoint(I, Q);

    if (Q.X = 0) and (Q.Y = 0) then
      List.Add(Format('***%d: (%d,%d)***', [I, Q.X, Q.Y]))
    else
      List.Add(Format('%d: (%d,%d)', [I, Q.X, Q.Y]));
    chtEccInt64.SeriesList[0].AddXY(Q.X, Q.Y);

    X1 := Q.X;
    Y1 := Q.Y;
    // 顺便解 Q 点上的 X Y 方程验证
    if not Ecc.PlainToPoint(X1, Q) then
    begin
      ShowMessage(Format('Error %d: X: %d', [I, X1]));
      Exit;
    end
    else if (Q.Y <> Y1) and (Q.Y <> Ecc.FiniteFieldSize - Y1) then
    begin
      ShowMessage(Format('Error %d: X: %d. Y %d <> %d', [I, X1, Y1, Q.Y]));
      Exit;
    end;
  end;
  mmoGenECCPoints.Lines.Assign(List);
  List.Free;
  Ecc.Free;
end;

// 计算勒让德符号 ( A / P) 的值
function CalcLegendre(A, P: Int64): Integer;
begin
  // 三种情况：P 能整除 A 时返回 0，不能整除时，如果 A 是完全平方数就返回 1，否则返回 -1
  if A mod P = 0 then
    Result := 0
  else if MontgomeryPowerMod(A, (P - 1) shr 1, P) = 1 then // 欧拉判别法
    Result := 1
  else
    Result := -1;
end;

procedure TFormEcc.btnLeRanDeClick(Sender: TObject);
var
  I: Integer;
begin
  I := 1297;
  CalcLegendre(I * I * I + 225, 21893);
  CalcLegendre(Int64(I) * Int64(I) * Int64(I) + 225, 21893);
end;

procedure TFormEcc.ShowBnEcc;
begin
  edtBNEccA.Text := FBNEcc.CoefficientA.ToDec;
  edtBNEccB.Text := FBNEcc.CoefficientB.ToDec;
  edtBNEccP.Text := FBNEcc.FiniteFieldSize.ToDec;
  edtBNEccGX.Text := FBNEcc.Generator.X.ToDec;
  edtBNEccGY.Text := FBNEcc.Generator.Y.ToDec;
  edtBNEccOrder.Text := FBNEcc.Order.ToDec;

  edtBNEccA.Hint := FBNEcc.CoefficientA.ToHex;
  edtBNEccB.Hint := FBNEcc.CoefficientB.ToHex;
  edtBNEccP.Hint := FBNEcc.FiniteFieldSize.ToHex;
  edtBNEccGX.Hint := FBNEcc.Generator.X.ToHex;
  edtBNEccGY.Hint := FBNEcc.Generator.Y.ToHex;
  edtBNEccOrder.Hint := FBNEcc.Order.ToHex;

  if FBNEcc.IsPointOnCurve(FBNEcc.Generator) then
  begin
    lblBNGX.Hint := 'G is on Curve.';
    lblBNGY.Hint := 'G is on Curve.';
  end;
end;

procedure TFormEcc.btnBNEccInverseGClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  FBNEcc.PointInverse(P);
  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
end;

procedure TFormEcc.btnBNEccInverseAddClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  FBNEcc.PointInverse(P);
  FBNEcc.PointAddPoint(FBNEcc.Generator, P, P);
  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
end;

procedure TFormEcc.btnBNEccGx2Click(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  FBNEcc.PointAddPoint(FBNEcc.Generator, P, P);
  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
end;

procedure TFormEcc.btnBNEccG2SubGClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  FBNEcc.PointAddPoint(FBNEcc.Generator, P, P);
  FBNEcc.PointSubPoint(P, FBNEcc.Generator, P);
  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
end;

procedure TFormEcc.btnBNEccGAddGClick(Sender: TObject);
var
  P: TCnEccPoint;
  K: TCnBigNumber;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  K := TCnBigNumber.Create;
  K.SetDec('2');
  FBNEcc.MultiplePoint(K, P);
  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
  K.Free;
end;

procedure TFormEcc.btnBNEccGSubGClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  FBNEcc.PointSubPoint(P, FBNEcc.Generator, P);
  edtBNEccResult.Text := CnEccPointToString(P);
  P.Free;
end;

procedure TFormEcc.btnBNEccNGClick(Sender: TObject);
var
  P: TCnEccPoint;
  K: TCnBigNumber;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  K := TCnBigNumber.Create;
  K.SetDec(edtBNEccOrder.Text);
  FBNEcc.MultiplePoint(K, P);
  edtBNEccResult.Text := CnEccPointToString(P);
  P.Free;
  K.Free;
end;

procedure TFormEcc.btnBNEcc4GClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
  K: TCnBigNumber;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  Q.Assign(FBNEcc.Generator);

  K := TCnBigNumber.Create;
  K.SetDec('2');
  FBNEcc.MultiplePoint(K, P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error * 2');
  FBNEcc.MultiplePoint(K, P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error * 4');
  edtBNEccResult.Text := CnEccPointToString(P);

  FBNEcc.PointAddPoint(Q, FBNEcc.Generator, Q);
  if not FBNEcc.IsPointOnCurve(Q) then
    ShowMessage('Error G + G');
  FBNEcc.PointAddPoint(Q, FBNEcc.Generator, Q); // 前两个换位置就会出错？
  if not FBNEcc.IsPointOnCurve(Q) then
    ShowMessage('Error G + G + G');
  FBNEcc.PointAddPoint(Q, FBNEcc.Generator, Q);
  if not FBNEcc.IsPointOnCurve(Q) then
    ShowMessage('Error G + G + G + G');

  if CnEccPointsEqual(P, Q) then
    ShowMessage('Equal');
  P.Free;
  K.Free;
end;

procedure TFormEcc.btnBNEccNewKeyClick(Sender: TObject);
begin
  FBNEcc.GenerateKeys(FBNEccPrivateKey, FBNEccPublicKey);
  edtBNEccPrivateKey.Text := FBNEccPrivateKey.ToDec;
  edtBNEccPublicKey.Text := CnEccPointToString(FBNEccPublicKey);
end;

procedure TFormEcc.lblBNEccDataPointClick(Sender: TObject);
var
  I: Integer;
  K: TCnBigNumber;
begin
  Randomize;
  I := Trunc(Random * 65537) + 1;
  K := TCnBigNumber.Create;
  K.SetWord(I);
  FBNEccDataPoint.Assign(FBNEcc.Generator);
  FBNEcc.MultiplePoint(K, FBNEccDataPoint);
  K.Free;
  edtBNEccDataPoint.Text := CnEccPointToString(FBNEccDataPoint);
end;

procedure TFormEcc.btnBNEccCryptClick(Sender: TObject);
var
  Data1, Data2, Plain: TCnEccPoint;
begin
  Data1 := TCnEccPoint.Create;
  Data2 := TCnEccPoint.Create;
  Plain := TCnEccPoint.Create;
  FBNEcc.Encrypt(FBNEccDataPoint, FBNEccPublicKey, Data1, Data2);
  ShowMessage('Encrypted to' + #13#10 + CnEccPointToString(Data1) + #13#10 + CnEccPointToString(Data2));

  FBNEcc.Decrypt(Data1, Data2, FBNEccPrivateKey, Plain);
  ShowMessage('Decrypted back to' + #13#10 + CnEccPointToString(Plain));

  Plain.Free;
  Data1.Free;
  Data2.Free;
end;

procedure TFormEcc.btnWrapDataClick(Sender: TObject);
var
  P: TCnBigNumber;
  Pt: TCnEccPoint;
begin
  P := TCnBigNumber.Create;
  Pt := TCnEccPoint.Create;
  P.SetDec(edtWrapData.Text);
//  P.SetDec('21');
//  Ecc := TCnEcc.Create('0C', 'C7', '49', '15', '15', '3D');

  if FBNEcc.PlainToPoint(P, Pt) then
  begin
    edtWrapPoint.Text := Pt.Y.ToDec;
    P.SetZero;
    FBNEcc.PointToPlain(Pt, P);
    edtWrapData.Text := P.ToDec;
  end
  else
    ShowMessage('Can NOT Convert Data to Point');

  P.Free;
  Pt.Free;
end;

procedure TFormEcc.btnBNECDHYaClick(Sender: TObject);
begin
  FBNECDHPrivKey1.SetDec(edtBNECDHXa.Text);
  CnEccDiffieHellmanGenerateOutKey(FBNEcc, FBNECDHPrivKey1, FBNECDHPubKey1);
  edtBNECDHA.Text := CnEccPointToString(FBNECDHPubKey1);
end;

procedure TFormEcc.btnBNECDHYbClick(Sender: TObject);
begin
  FBNECDHPrivKey2.SetDec(edtBNECDHXb.Text);
  CnEccDiffieHellmanGenerateOutKey(FBNEcc, FBNECDHPrivKey2, FBNECDHPubKey2);
  edtBNECDHB.Text := CnEccPointToString(FBNECDHPubKey2);
end;

procedure TFormEcc.btnBNECDHAKeyClick(Sender: TObject);
var
  Sec: TCnEccPublicKey;
begin
  Sec := TCnEccPublicKey.Create;
  CnEccDiffieHellmanComputeKey(FBNEcc, FBNECDHPrivKey1, FBNECDHPubKey2, Sec);
  edtBNECDHResA.Text := CnEccPointToString(Sec);
  Sec.Free;
end;

procedure TFormEcc.btnBNECDHBkeyClick(Sender: TObject);
var
  Sec: TCnEccPublicKey;
begin
  Sec := TCnEccPublicKey.Create;
  CnEccDiffieHellmanComputeKey(FBNEcc, FBNECDHPrivKey2, FBNECDHPubKey1, Sec);
  edtBNECDHResB.Text := CnEccPointToString(Sec);
  Sec.Free;
end;

procedure TFormEcc.btnTestECDHClick(Sender: TObject);
var
  Priv1, Priv2: TCnEccPrivateKey;
  Pub1, Pub2: TCnEccPublicKey;
  Sec: TCnEccPoint;
begin
  Priv1 := TCnEccPrivateKey.FromHex('E32868331FA8EF0138DE0DE85478346AEC5E3912B6029AE71691C384237A3EEB');
  Priv2 := TCnEccPrivateKey.FromHex('CEF147652AA90162E1FFF9CF07F2605EA05529CA215A04350A98ECC24AA34342');

  Pub1 := TCnEccPublicKey.Create;
  Pub2 := TCnEccPublicKey.Create;

  CnEccDiffieHellmanGenerateOutKey(FBNEcc, Priv1, Pub1);
  CnEccDiffieHellmanGenerateOutKey(FBNEcc, Priv2, Pub2);

  ShowMessage('Pub1 is:' + #13#10 + Pub1.X.ToHex + #13#10 + Pub1.Y.ToHex);
  // 86B1AA5120F079594348C67647679E7AC4C365B2C01330DB782B0BA611C1D677, 5F4376A23EED633657A90F385BA21068ED7E29859A7FAB09E953CC5B3E89BEBA

  ShowMessage('Pub2 is:' + #13#10 + Pub2.X.ToHex + #13#10 + Pub2.Y.ToHex);
  // 4034127647BB7FDAB7F1526C7D10BE8B28174E2BBA35B06FFD8A26FC2C20134A, 9E773199EDC1EA792B150270EA3317689286C9FE239DD5B9C5CFD9E81B4B632

  Sec := TCnEccPoint.Create;
  CnEccDiffieHellmanComputeKey(FBNEcc, Priv1, Pub2, Sec);
  ShowMessage('A Compute Key is:' + #13#10 + Sec.X.ToHex + #13#10 + Sec.Y.ToHex);
  CnEccDiffieHellmanComputeKey(FBNEcc, Priv2, Pub1, Sec);
  ShowMessage('B Compute Key is:' + #13#10 + Sec.X.ToHex + #13#10 + Sec.Y.ToHex);
  // 3E2FFBC3AA8A2836C1689E55CD169BA638B58A3A18803FCF7DE153525B28C3CD, 43CA148C92AF58EBDB525542488A4FE6397809200FE8C61B41A105449507083

  Sec.Free;
  Pub1.Free;
  Pub2.Free;
  Priv1.Free;
  Priv2.Free;
end;

procedure TFormEcc.btnBNEccWrapRangeClick(Sender: TObject);
var
  P, Q: TCnBigNumber;
  Pt: TCnEccPoint;
begin
  P := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  Pt := TCnEccPoint.Create;
  P.SetDec(edtWrapData.Text);
  BigNumberCopy(Q, P);

  // 将 P 左移 24 位腾出 2^24 个空间来搜索有无解
  if P.GetBitsCount + CN_ECC_PLAIN_DATA_BITS_GAP - 1 > FBNEcc.FiniteFieldSize.GetBitsCount then
    raise ECnEccException.Create('Data Too Large.');

  BigNumberShiftLeft(P, P, CN_ECC_PLAIN_DATA_BITS_GAP);
  BigNumberAddWord(Q, 1);
  BigNumberShiftLeft(Q, Q, CN_ECC_PLAIN_DATA_BITS_GAP);

  repeat
    // 对于 Pt.X 所属范围内的每一个 X，求 Pt.Y
    if FBNEcc.PlainToPoint(P, Pt) then
    begin
      edtWrapData.Text := Pt.X.ToDec;
      edtWrapPoint.Text := Pt.Y.ToDec;
      Exit;
    end;
    BigNumberAddWord(P, 1);
  until BigNumberCompare(P, Q) >= 0;

  ShowMessage('Can NOT Convert Range to Point.');
  Pt.Free;
  P.Free;
  Q.Free;
end;

procedure TFormEcc.btnBNGXtoPointClick(Sender: TObject);
var
  P: TCnBigNumber;
  Pt: TCnEccPoint;
  BnEcc: TCnEcc;
begin
  P := TCnBigNumber.Create;
  Pt := TCnEccPoint.Create;
  P.SetDec(edtEccGX.Text);
  BnEcc := TCnEcc.Create('4', 'B6', '295', '1', 'D6', '2A1');

  if BnEcc.PlainToPoint(P, Pt) then
  begin
    ShowMessage('Convert to ' + CnEccPointToString(Pt));
    P.SetZero;
    BnEcc.PointToPlain(Pt, P);
    ShowMessage('Convert back ' + P.ToDec);
  end
  else
    ShowMessage('Can NOT Convert Data to Point');

  BnEcc.Free;
  P.Free;
  Pt.Free;
end;

procedure TFormEcc.btnInt64GXtoPtClick(Sender: TObject);
var
//  I: Integer;
  P, A, B, X, Y, N: Int64;
  Ecc: TCnInt64Ecc;
  Pt: TCnInt64EccPoint;
begin
  A := StrToInt(edtEccA.Text);
  B := StrToInt(edtEccB.Text);
  X := StrToInt(edtEccGX.Text);
  Y := StrToInt(edtEccGY.Text);
  P := StrToInt(edtEccP.Text);
  N := StrToInt(edtEccOrder.Text);

  Ecc := TCnInt64Ecc.Create(A, B, P, X, Y, N);
  // 4/182/661/1/214/673  是 8u5 型的测试通过
  // 15/194/64403/41589/5579 是 4u3 型的测试通过
  // 12/199/73/21/21/61 是 8u1 型的也基本测试通过

//P := GetTickCount;
//for I := 0 to 100000 do     // 一万次要 453/469 毫秒，十万次要 4484 毫秒
//  Ecc.PlainToPoint(X, Pt);
//ShowMessage(IntToStr(GetTickCount - P));

  if Ecc.PlainToPoint(X, Pt) then
  begin
    ShowMessage('Convert to ' + CnInt64EccPointToString(Pt));
    if Ecc.IsPointOnCurve(Pt) then
      ShowMessage('Convert on Curve.');
  end
  else
    ShowMessage('Can NOT Convert Data to Point');

  Ecc.Free;
end;

procedure TFormEcc.btnLucasRecurClick(Sender: TObject);
var
  X, Y, P, U, V, U_2, V_2, U_1, V_1: Int64;
  I: Integer;
begin
  X := StrToInt(edtLucasX.Text);
  Y := StrToInt(edtLucasY.Text);
  P := StrToInt(edtLucasP.Text);

  mmoLucasRes.Lines.Clear;
  mmoLucasRes.Lines.Add(Format('%d: %d, %d', [0, 0, 2]));
  mmoLucasRes.Lines.Add(Format('%d: %d, %d', [1, 1, X]));
  U_2 := 0;
  V_2 := 2;
  U_1 := 1;
  V_1 := X;

  for I := 2 to 100 do
  begin
    CalcLucas(X, Y, U_2, V_2, U_1, V_1, U, V);
    U_2 := U_1;
    V_2 := V_1;

    U_1 := U;
    V_1 := V;
    if chkLucasMod.Checked then
      mmoLucasRes.Lines.Add(Format('%d: %d, %d', [I, U mod P, V mod P]))
    else
      mmoLucasRes.Lines.Add(Format('%d: %d, %d', [I, U, V]));
  end;
end;

// 按定义递归计算 Lucas 序列，照理慢但准确
procedure TFormEcc.CalcLucas(X, Y, U_2, V_2, U_1, V_1: Int64; var U, V: Int64);
begin
  U := X * U_1 - Y * U_2;
  V := X * V_1 - Y * V_2;
end;

// 支持 A、B 为负数的乘积取模，但 C 仍要求正数否则结果不靠谱
function Int64MultipleMod(A, B, C: Int64): Int64;
begin
  if (A > 0) and (B > 0) then
    Result := MultipleMod(A, B, C)
  else if (A < 0) and (B < 0) then
    Result := MultipleMod(-A, -B, C)
  else if (A > 0) and (B < 0) then
    Result := C - MultipleMod(A, -B, C)
  else if (A < 0) and (B > 0) then
    Result := C - MultipleMod(-A, B, C)
  else
    Result := 0;
end;

// 计算 Lucas 序列以及模的两种不同实现函数，但结果对不上，弃用。
procedure CalcLucasSequenceBad(X, Y, K, P: Int64; out U, V: Int64);
var
  I: Integer;
  U_2, V_2, U_1, V_1: Int64;
begin
  if K < 0 then
    raise ECnEccException.Create('Invalid K for Lucas Sequence');

  if K = 0 then
  begin
    U := 0;
    V := 2;
    Exit;
  end
  else if K = 1 then
  begin
    U := 1;
    V := X;
    Exit;
  end;

  U_2 := 0;
  V_2 := 2;
  U_1 := 1;
  V_1 := X;

  for I := 2 to K do
  begin
    U := X * U_1 - Y * U_2;
    V := X * V_1 - Y * V_2;

    U_2 := U_1;
    V_2 := V_1;

    U_1 := U;
    V_1 := V;
  end;
end;

// P1363 上的 Lucas 计算，虽然和 SM2 里的说明几乎全都对不上号，但目前结果看起来还靠谱
// V0 = 2, V1 = X, and Vk = X * Vk-1 - Y * Vk-2   for k >= 2
// V 返回 Vk mod N，Q 返回 Y ^ (K div 2) mod N
procedure CalcLucasSequenceMod(X, Y, K, N: Int64; out Q, V: Int64);
var
  C, I: Integer;
  V0, V1, Q0, Q1: Int64;
begin
  if K < 0 then
    raise ECnEccException.Create('Invalid K for Lucas Sequence');

  // V0 = 2, V1 = P, and Vk = P * Vk-1 - Q * Vk-2   for k >= 2

  if K = 0 then
  begin
    Q := 1;
    V := 2;
    Exit;
  end
  else if K = 1 then
  begin
    Q := 1;
    V := X;
    Exit;
  end;

  V0 := 2;
  V1 := X;
  Q0 := 1;
  Q1 := 1;

  C := GetUInt64HighBits(K);
  for I := C downto 0 do
  begin
    Q0 := Int64MultipleMod(Q0, Q1, N);
    if GetUInt64BitSet(K, I) then
    begin
      Q1 := Int64MultipleMod(Q0, Y, N);
      V0 := Int64Mod(Int64MultipleMod(V0, V1, N) - Int64MultipleMod(X, Q0, N), N);
      V1 := Int64Mod(Int64MultipleMod(V1, V1, N) - Int64MultipleMod(2, Q1, N), N);
    end
    else
    begin
      Q1 := Q0;
      V1 := Int64Mod(Int64MultipleMod(V0, V1, N) - Int64MultipleMod(X, Q0, N), N);
      V0 := Int64Mod(Int64MultipleMod(V0, V0, N) - Int64MultipleMod(2, Q0, N), N);
    end;
  end;
  Q := Q0;
  V := V0;
end;

// 另一种 Lucas 计算，V 对得上号但 U 不靠谱
procedure CalcLucasSequence2(X, Y, K, P: Int64; out U, V: Int64);
var
  C, I: Integer;
  V0, V1, Q0, Q1: Int64;
begin
  if K < 0 then
    raise ECnEccException.Create('Invalid K for Lucas Sequence');

  if K = 0 then
  begin
    U := 0;
    V := 2;
    Exit;
  end
  else if K = 1 then
  begin
    U := 1;
    V := X;
    Exit;
  end;

  V0 := 2;
  V1 := X;
  Q0 := 1;
  Q1 := 1;

  C := GetUInt64HighBits(K);
  for I := C downto 0 do
  begin
    Q0 := Q0 * Q1;
    if GetUInt64BitSet(K, I) then
    begin
      Q1 := Q0 * Y;
      V0 := V0 * V1 - X * Q0;
      V1 := V1 * V1 - 2 * Q1;
    end
    else
    begin
      Q1 := Q0;
      V1 := V0 * V1 - X * Q0;
      V0 := V0 * V0 - 2 * Q0;
    end;
  end;
  U := Q0;
  V := V0;
end;

procedure CalcLucasSequenceMod_SM2(X, Y, K, P: Int64; out U, V: Int64);
var
  C, I: Integer;
  D, UT, VT: Int64;
begin
  if K < 0 then
    raise ECnEccException.Create('Invalid K for Lucas Sequence');

  if K = 0 then
  begin
    U := 0 mod P;
    V := 2 mod P;
    Exit;
  end
  else if K = 1 then
  begin
    U := 1 mod P;
    V := X mod P;
    Exit;
  end;

  D := X * X - 4 * Y;

  U := 1;
  V := X;

  C := GetUInt64HighBits(K);
  for I := C - 1 downto 0 do
  begin
    UT := Int64MultipleMod(U, V, P);
    VT := ((V * V + D * U * U) div 2) mod P;

    U := UT;
    V := VT;
    if GetUInt64BitSet(K, I) then
    begin
      UT := ((X * U + V) div 2) mod P;
      VT := ((X * V + D * U) div 2) mod P;
      U := UT;
      V := VT;
    end;
  end;

  if U < 0 then
    U := U + P;
  if V < 0 then
    V := V + P;
end;

procedure TFormEcc.btnLucasModClick(Sender: TObject);
var
  I: Integer;
  X, Y, U, V, P: Int64;
begin
  mmoLucasMod.Lines.Clear;
  X := StrToInt(edtLucasX.Text);
  Y := StrToInt(edtLucasY.Text);
  P := StrToInt(edtLucasP.Text);

  for I := 0 to 100 do
  begin
    if chkLucasMod.Checked then
    begin
      CalcLucasSequenceMod(X, Y, I, P, U, V);
      mmoLucasMod.Lines.Add(Format('%d: %d, %d', [I, U, V]));
    end
    else
    begin
      CalcLucasSequence2(X, Y, I, P, U, V);
      mmoLucasMod.Lines.Add(Format('%d: %d, %d', [I, U, V]));
    end;
  end;
end;

// 计算 X, Y 的第 K 的 Lucas 序列 mod p 的值，不太对劲
procedure BigNumberCalcLucasSequenceMod(X, Y, U, V, K, P: TCnBigNumber);
var
  I: Integer;
  D, T, U1, V1: TCnBigNumber;
begin
  D := nil;
  T := nil;
  U1 := nil;
  V1 := nil;

  if K.IsNegative or K.IsZero or P.IsNegative or P.IsZero then
    raise ECnEccException.Create('Invalid K or P for Lucas Sequance');

  try
    D := TCnBigNumber.Create;
    T := TCnBigNumber.Create;
    U1 := TCnBigNumber.Create;
    V1 := TCnBigNumber.Create;

    BigNumberMul(D, X, X);   // D: X^2
    BigNumberCopy(T, Y);
    BigNumberMulWord(T, 4);  // T: 4 * Y
    BigNumberSub(D, D, T);   // D = X^2 - 4 * Y

    U1.SetOne;
    BigNumberCopy(V1, X);

    if not BigNumberIsBitSet(K, K.GetBitsCount - 1) then
      raise ECnEccException.Create('Invalid K Bits for Lucas Sequance');

    for I := K.GetBitsCount - 2 downto 0 do
    begin
      // 用本轮的初始值 U1、V1 计算下一轮的 U、V，并赋值回 U1、V1
      BigNumberMulMod(U, U1, V1, P);   // U = (U*V) mod p 计算 U 时不能改变 U1 因为 计算 V 时还要用到

      BigNumberMul(V1, V1, V1);        // V = ((V^2 +D*U^2)/2) mod p  // 计算 V 时随便改变 U1、V1
      BigNumberMul(U1, U1, U1);
      BigNumberMul(U1, U1, D);
      BigNumberAdd(V1, U1, V1);
      BigNumberShiftRight(V1, V1, 1);
      BigNumberMod(V, V1, P);

      BigNumberCopy(U1, U);
      BigNumberCopy(V1, V);
      if BigNumberIsBitSet(K, I) then
      begin
        // 用 U1、V1 算 U、V，并赋值回 U1、V1
        BigNumberMul(U, U1, X);   // U = ((X * U +V)/2) mod p
        BigNumberAdd(U, U, V1);
        BigNumberShiftRight(U, U, 1);
        BigNumberMod(U, U, P);    // 计算 U 时不能改变 U1 因为 计算 V 时还要用到

        BigNumberMul(V1, V1, X);        // V = ((X*V + D*U)/2) mod p
        BigNumberMul(U1, U1, D);
        BigNumberAdd(V1, U1, V1);
        BigNumberShiftRight(V1, V1, 1); // 计算 V 时随便改变 U1、V1
        BigNumberMod(V, V1, P);

        BigNumberCopy(U1, U);
        BigNumberCopy(V1, V);
      end;
    end;
  finally
    D.Free;
    T.Free;
    U1.Free;
    V1.Free;
  end;
end;

procedure TFormEcc.btnCalcLegendreClick(Sender: TObject);
var
  List: TCnBigNumberList;
  P, T: TCnBigNumber;
  I, R: Integer;
begin
  P := TCnBigNumber.FromDec(edtBNEccP.Text);
  // P.SetDec('23');
  List := TCnBigNumberList.Create;

  mmoLegendre.Lines.Clear;
  for I := 0 to 20 do
  begin
    T := TCnBigNumber.Create;
    BigNumberRandRange(T, P);
    // T.SetWord(I + 1);
    List.Add(T);
    mmoLegendre.Lines.Add(T.ToDec);
  end;

  mmoLegendreRes1.Lines.Clear;
  for I := 0 to List.Count - 1 do
  begin
    R := BigNumberLegendre(List[I], P);
    mmoLegendreRes1.Lines.Add(IntToStr(R));
  end;

  mmoLegendreRes2.Lines.Clear;
  for I := 0 to List.Count - 1 do
  begin
    R := BigNumberLegendre2(List[I], P);
    mmoLegendreRes2.Lines.Add(IntToStr(R));
  end;

  P.Free;
  List.Free;
end;

type
  TCnInt64EccHack = class(TCnInt64Ecc);

procedure TFormEcc.btnTSInt64Click(Sender: TObject);
var
  X, P, R: Int64;
begin
  X := StrToInt64(edtTSX.Text);
  P := StrToInt64(edtTSP.Text);
  if not CnInt64IsPrime(P) then
  begin
    ShowMessage('NOT Prime');
    Exit;
  end;
  if CnInt64Legendre(X, P) <> 1 then
  begin
    ShowMessage('Legendre NOT 1, No Result');
    Exit;
  end;

  if TCnInt64EccHack(FEcc64E2311).TonelliShanks(X, P, R) then
    ShowMessage(IntToStr(R));
end;

procedure TFormEcc.btnBNTSClick(Sender: TObject);
var
  R, X, P: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  X := TCnBigNumber.Create;
  P := TCnBigNumber.Create;

  try
    X.SetDec(edtTSX.Text);
    P.SetDec(edtTSP.Text);
    if not BigNumberIsProbablyPrime(P) then
    begin
      ShowMessage('NOT Prime');
      Exit;
    end;
    if BigNumberLegendre(X, P) <> 1 then
    begin
      ShowMessage('Legendre NOT 1, No Result');
      Exit;
    end;

    if BigNumberTonelliShanks(R, X, P) then
      ShowMessage(R.ToDec);
  finally
    R.Free;
    X.Free;
    P.Free;
  end
end;

// 封装的对于 P 为 8*u+1 的奇素数，用 Lucas 方法求其模平方根
function SquareRootModPrimeLucas(X, P: Int64; out Y: Int64): Boolean;
var
  G, Z, U, V: Int64;

  function RandomInt64LessThan(HighValue: Int64): Int64;
  var
    Hi, Lo: Cardinal;
  begin
    Randomize;
    Hi := Trunc(Random * High(Integer) - 1) + 1;   // Int64 最高位不能是 1，避免负数
    Randomize;
    Lo := Trunc(Random * High(Cardinal) - 1) + 1;
    Result := (Int64(Hi) shl 32) + Lo;
    Result := Result mod HighValue;
  end;

begin
  Result := False;
  G := X;
  while True do
  begin
    // 随机取 X
    X := RandomInt64LessThan(P);

    // 再计算 Lucas 序列中的 V，其下标 K 为 (P+1)/2
    CalcLucasSequenceMod(X, G, (P + 1) shr 1, P, U, V);

    // V 偶则直接右移 1 再 mod P，V 奇则加 P 再右移 1
    if (V and 1) = 0 then
      Z := (V shr 1) mod P
    else
      Z := (V + P) shr 1;
    // Z := (V div 2) mod P;

    if Int64MultipleMod(Z, Z, P) = G then
    begin
      Y := Z;
      Result := True;
      Exit;
    end
    else if (U > 1) and (U < P - 1) then
      Break;
  end;
end;

procedure TFormEcc.btnSRLucasClick(Sender: TObject);
var
  R, P, U, X, Y, Z: Int64;
  PrimeType: TCnEccPrimeType;
begin
  P := StrToInt64(edtSRP.Text);
  X := StrToInt64(edtSRX.Text);
  edtSRY.Text := '';

  if CnInt64Legendre(X, P) <> 1 then
  begin
    ShowMessage('NO Answer');
    Exit;
  end;

  R := P mod 4;
  if R = 3 then
  begin
    PrimeType := pt4U3;
    U := P div 4;
  end
  else
  begin
    R := P mod 8;
    if R = 1 then
    begin
      PrimeType := pt8U1;
      U := P div 8;
    end
    else if R = 5 then
    begin
      PrimeType := pt8U5;
      U := P div 8;
    end
    else
      raise ECnEccException.Create('Invalid Finite Field Size.');
  end;

  case PrimeType of
  pt4U3:  // 参考自《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节
    begin
      Y := MontgomeryPowerMod(X, U + 1, P);   // 55, 103 得 63
      Z := Int64MultipleMod(Y, Y, P);
      if Z = X then
      begin
        edtSRY.Text := IntToStr(Y);
      end;
    end;
  pt8U5:  // 参考自《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节
    begin
      Z := MontgomeryPowerMod(X, 2 * U + 1, P);
      if Z = 1 then
      begin
        Y := MontgomeryPowerMod(X, U + 1, P);
        edtSRY.Text := IntToStr(Y);
      end
      else
      begin
        Z := P - Z;
        if Z = 1 then
        begin
          // y = (2g * (4g)^u) mod p = (2g mod p * (4^u * g^u) mod p) mod p
          Y := (Int64MultipleMod(X, 2, P) *
            MontgomeryPowerMod(4, U, P) *
            MontgomeryPowerMod(X, U, P)) mod P;
          edtSRY.Text := IntToStr(Y);
        end;
      end;
    end;
  pt8U1: // 参考自 wikipedia 上的 Tonelli Shanks 二次剩余求解算法
    begin
      // 《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节 Lucas 序列计算出来的结果实在不对
      // 换成 IEEE P1363 中说的 Lucas 序列
      if SquareRootModPrimeLucas(X, P, Y) then
      begin
        edtSRY.Text := IntToStr(Y);
        Exit;
      end;
    end;
  end;

  if edtSRY.Text = '' then
    ShowMessage('NO Result')
  else
  begin
    Y := StrToInt64(edtSRY.Text);
    P := StrToInt64(edtSRP.Text);
    X := StrToInt64(edtSRX.Text);
    if Int64MultipleMod(Y, Y, P) = X then
      ShowMessage('Check OK');
  end;
end;

procedure TFormEcc.btnSRCompareClick(Sender: TObject);
const
  COUNT = 100000;
var
  T1, T2: DWORD;
  I: Integer;
  X, P, Y: Int64;
begin
  X := 55;
  P := 73;    // 必须是 8u+1 型

  T1 := GetTickCount;
  for I := 0 to COUNT - 1 do
  begin
    SquareRootModPrimeLucas(X, P, Y);                    // 1.2 秒
  end;
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 0 to COUNT - 1 do
  begin
    TCnInt64EccHack(nil).TonelliShanks(X, P, Y);         // 0.7 秒
  end;
  T2 := GetTickCount - T2;

  ShowMessage(IntToStr(T1) + ' ' + IntToStr(T2));
end;

procedure TFormEcc.btnBNLucasModClick(Sender: TObject);
var
  X, Y, K, N, Q, V: TCnBigNumber;
  I: Integer;
begin
  X := TCnBigNumber.Create;
  Y := TCnBigNumber.Create;
  K := TCnBigNumber.Create;
  N := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  V := TCnBigNumber.Create;

  X.SetDec(edtLucasX.Text);
  Y.SetDec(edtLucasY.Text);
  N.SetDec(edtLucasP.Text);

  mmoLucasRes.Lines.Clear;
  for I := 0 to 100 do
  begin
    K.SetWord(I);
    if not BigNumberLucasSequenceMod(X, Y, K, N, Q, V) then
      Exit;

    mmoLucasRes.Lines.Add(Format('%d: %s, %s', [I, Q.ToDec, V.ToDec]));
  end;

  X.Free;
  Y.Free;
  K.Free;
  N.Free;
  Q.Free;
  V.Free;
end;

procedure TFormEcc.btnLoadClick(Sender: TObject);
var
  S: string;
begin
  if dlgOpen1.Execute then
  begin
    S := CnInputBox('Password', 'Enter Password if the PEM is Encrypted.', '');

    if CnEccLoadKeysFromPem(dlgOpen1.FileName, FPrivateKey, FPublicKey, FCurveType, ckhMd5, S) then
    begin
      lblCurveTypeText.Caption := GetEnumName(TypeInfo(TCnEccCurveType), Ord(FCurveType));
      FKeyEcc.Load(FCurveType);

      edtKeyEccA.Text := FKeyEcc.CoefficientA.ToDec;
      edtKeyEccB.Text := FKeyEcc.CoefficientB.ToDec;
      edtKeyEccP.Text := FKeyEcc.FiniteFieldSize.ToDec;
      edtKeyEccGX.Text := FKeyEcc.Generator.X.ToDec;
      edtKeyEccGY.Text := FKeyEcc.Generator.Y.ToDec;
      edtKeyEccOrder.Text := FKeyEcc.Order.ToDec;

      edtKeyEccA.Hint := FKeyEcc.CoefficientA.ToHex;
      edtKeyEccB.Hint := FKeyEcc.CoefficientB.ToHex;
      edtKeyEccP.Hint := FKeyEcc.FiniteFieldSize.ToHex;
      edtKeyEccGX.Hint := FKeyEcc.Generator.X.ToHex;
      edtKeyEccGY.Hint := FKeyEcc.Generator.Y.ToHex;
      edtKeyEccOrder.Hint := FKeyEcc.Order.ToHex;

      edtKeyPrivate.Text := FPrivateKey.ToDec;
      edtKeyPublic.Text := CnEccPointToString(FPublicKey);
    end;
//     CnEccLoadPublicKeyFromPem(dlgOpen1.FileName, PublicKey, CurveType, ckhMd5, Password);
//    if dlgSave1.Execute then
//      CnEccSaveKeysToPem(dlgSave1.FileName, FPrivateKey, FPublicKey, CurveType);
//       CnEccSavePublicKeyToPem(dlgSave1.FileName, PublicKey, CurveType);
  end;
end;

procedure TFormEcc.btnKeyCheckPublicClick(Sender: TObject);
var
  P: TCnEccPoint;
begin
  // 根据 X 求 Y 并验证
  P := TCnEccPoint.Create;
  if FKeyEcc.PlainToPoint(FPublicKey.X, P) then
  begin
    if FPublicKey.Y.IsZero then
      BigNumberCopy(FPublicKey.Y, P.Y)
    else if BigNumberCompare(FPublicKey.Y, P.Y) = 0 then
      ShowMessage('Public Key Verify OK.');
  end;
  P.Free;

  if FKeyEcc.IsPointOnCurve(FPublicKey) then
    ShowMessage('PublicKey is On Curve.');
end;

procedure TFormEcc.btnSaveKeyClick(Sender: TObject);
begin
  if dlgSave1.Execute then
    if CnEccSaveKeysToPem(dlgSave1.FileName, FPrivateKey, FPublicKey, FCurveType) then
      ShowMessage('Private Key Save OK.');
// CnEccSavePublicKeyToPem(dlgSave1.FileName, PublicKey, CurveType);
end;

end.
