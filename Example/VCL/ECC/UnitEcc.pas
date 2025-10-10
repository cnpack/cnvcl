unit UnitEcc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnECC, ExtCtrls, Buttons, TeEngine, Series, TeeProcs,
  Chart, TypInfo, CnPrime, CnBigNumber, CnNative, CnPemUtils, CnPolynomial;

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
    bvlSig: TBevel;
    edtKeyData: TEdit;
    lblKeyData: TLabel;
    btnKeySign: TButton;
    edtKeySign: TEdit;
    lblKeySign: TLabel;
    btnKeyVerify: TButton;
    lblKeyHash: TLabel;
    cbbKeyHash: TComboBox;
    btnKeyGenerate: TButton;
    btnKeyLoadSig: TButton;
    btnBNUpdate: TButton;
    btnBNEccCalc: TButton;
    cbbInt64EccPreset: TComboBox;
    btnEccTestAdd: TButton;
    btnHassenTest: TButton;
    btnHassenTest2: TButton;
    btnInt64SchoofTest: TButton;
    btnEccSchoof: TButton;
    btnSimpleAttack: TButton;
    btnTestCRT: TButton;
    btnInt64EccCountOrder: TButton;
    btnInt64CountOrder1: TButton;
    btnInt64CountEccPoints3: TButton;
    mmoBNEccPoints: TMemo;
    cbbCurveTypes: TComboBox;
    btnInt64Affine: TButton;
    btnTestJacobian: TButton;
    btnBNEccAffineTest: TButton;
    btnBNJacobianTest: TButton;
    bvl2: TBevel;
    rbBNAddNormal: TRadioButton;
    rbBNAddAffine: TRadioButton;
    rbBNAddJacobian: TRadioButton;
    btnMulTime: TButton;
    btnRecoverPubKey: TButton;
    btnEccFastSchoof: TButton;
    btnEccSchoof2: TButton;
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
    procedure btnKeySignClick(Sender: TObject);
    procedure btnKeyVerifyClick(Sender: TObject);
    procedure btnKeyGenerateClick(Sender: TObject);
    procedure btnKeyLoadSigClick(Sender: TObject);
    procedure btnSimpleAttackClick(Sender: TObject);
    procedure btnTestCRTClick(Sender: TObject);
    procedure btnBNUpdateClick(Sender: TObject);
    procedure btnBNEccCalcClick(Sender: TObject);
    procedure cbbInt64EccPresetChange(Sender: TObject);
    procedure btnEccTestAddClick(Sender: TObject);
    procedure btnHassenTestClick(Sender: TObject);
    procedure btnHassenTest2Click(Sender: TObject);
    procedure btnInt64SchoofTestClick(Sender: TObject);
    procedure btnEccSchoofClick(Sender: TObject);
    procedure btnInt64EccCountOrderClick(Sender: TObject);
    procedure btnInt64CountOrder1Click(Sender: TObject);
    procedure btnInt64CountEccPoints3Click(Sender: TObject);
    procedure cbbCurveTypesChange(Sender: TObject);
    procedure btnInt64AffineClick(Sender: TObject);
    procedure btnTestJacobianClick(Sender: TObject);
    procedure btnBNEccAffineTestClick(Sender: TObject);
    procedure btnBNJacobianTestClick(Sender: TObject);
    procedure btnMulTimeClick(Sender: TObject);
    procedure btnRecoverPubKeyClick(Sender: TObject);
    procedure btnEccFastSchoofClick(Sender: TObject);
    procedure btnEccSchoof2Click(Sender: TObject);
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
    procedure ShowMsg(Data: Int64); overload;
    procedure ShowMsg(const Data: string); overload;
    procedure CalcLucas(X, Y, U_2, V_2, U_1, V_1: Int64; var U, V: Int64);

    procedure CallUseless; // ���ñ�Ҫ�ĺ�����ֹ�� Link ʱ����
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
  P3, Q3: TCnInt64Ecc3Point;
begin
  P.X := 3; P.Y := 10;
  Q.X := 9; Q.Y := 7;
  CnInt64EccPointToEcc3Point(P, P3);
  CnInt64EccPointToEcc3Point(Q, Q3);

  FEcc64E2311.PointAddPoint(P, Q, P);
  ShowMessage(Format('3,10 + 9,7 = %d,%d',[P.X, P.Y]));

  FEcc64E2311.AffinePointAddPoint(P3, Q3, P3);
  CnInt64AffinePointToEccPoint(P3, Q, FEcc64E2311.FiniteFieldSize);
  ShowMessage('Affine Sum: ' + Format('%d,%d',[Q.X, Q.Y]));

  P.X := 3; P.Y := 10;
  Q.X := 9; Q.Y := 7;
  CnInt64EccPointToEcc3Point(P, P3);
  CnInt64EccPointToEcc3Point(Q, Q3);

  FEcc64E2311.JacobianPointAddPoint(P3, Q3, P3);
  CnInt64JacobianPointToEccPoint(P3, Q, FEcc64E2311.FiniteFieldSize);
  ShowMessage('Jacobian Sum: ' + Format('%d,%d',[Q.X, Q.Y]));
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
  P3: TCnInt64Ecc3Point;
begin
  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  // ��ͨ���� P + P
  CnInt64EccPointToEcc3Point(P, P3);
  FEcc64E2311.MultiplePoint(2, P);
  ShowMessage('P Multiple 2 is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));

  // ����������� P + P
  FEcc64E2311.AffinePointAddPoint(P3, P3, P3);
  CnInt64AffinePointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage('P + P using Affine is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));

  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  // �ſɱ�������� P + P
  CnInt64EccPointToEcc3Point(P, P3);
  FEcc64E2311.JacobianPointAddPoint(P3, P3, P3);
  CnInt64JacobianPointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage('P + P using Jacobian is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));

  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  // ����������� P * 2
  CnInt64EccPointToEcc3Point(P, P3);
  FEcc64E2311.AffineMultiplePoint(2, P3);
  CnInt64AffinePointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage('P * 2  using Affine is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));

  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  // �ſɱ�������� P * 2
  CnInt64EccPointToEcc3Point(P, P3);
  FEcc64E2311.JacobianMultiplePoint(2, P3);
  CnInt64JacobianPointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage('P * 2  using Jacobian is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));

  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  // ��ͨ������� P * 3
  FEcc64E2311.MultiplePoint(3, P);
  ShowMessage('P * 3 is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));

  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  // ����������� P * 3
  CnInt64EccPointToEcc3Point(P, P3);
  FEcc64E2311.AffineMultiplePoint(3, P3);
  CnInt64AffinePointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage('P * 3  using Affine is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));

  P.X := StrToInt(edtPX.Text);
  P.Y := StrToInt(edtPY.Text);

  // �ſɱ�������� P * 2
  CnInt64EccPointToEcc3Point(P, P3);
  FEcc64E2311.JacobianMultiplePoint(3, P3);
  CnInt64JacobianPointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage('P * 3  using Jacobian is ' + IntToStr(P.X) + ',' + IntToStr(P.Y));
end;

procedure TFormEcc.btnTestMulClick(Sender: TObject);
var
  P: TCnInt64EccPoint;
  P3: TCnInt64Ecc3Point;
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

  List.Clear;
  for I := 0 to 30 do
  begin
    P.X := StrToInt(edtPX.Text);
    P.Y := StrToInt(edtPY.Text);

    CnInt64EccPointToEcc3Point(P, P3);
    FEcc64E2311.AffineMultiplePoint(I, P3);
    CnInt64AffinePointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);

    List.Add(IntToStr(I) + '*: ' + IntToStr(P.X) + ',' + IntToStr(P.Y));
    if FEcc64E2311.IsPointOnCurve(P) then
      List[List.Count - 1] := List[List.Count - 1] + ' On Curve.'
    else
    begin
      // ShowMessage(List[List.Count - 1] + ' NOT On Curve.');
      List[List.Count - 1] := List[List.Count - 1] + ' NOT On Curve.'
    end;
  end;

  ShowMessage('P Affine Multiple is '#13#10#13#10 + List.Text);

  List.Clear;
  for I := 0 to 30 do
  begin
    P.X := StrToInt(edtPX.Text);
    P.Y := StrToInt(edtPY.Text);

    CnInt64EccPointToEcc3Point(P, P3);
    FEcc64E2311.JacobianMultiplePoint(I, P3);
    CnInt64JacobianPointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);

    List.Add(IntToStr(I) + '*: ' + IntToStr(P.X) + ',' + IntToStr(P.Y));
    if FEcc64E2311.IsPointOnCurve(P) then
      List[List.Count - 1] := List[List.Count - 1] + ' On Curve.'
    else
    begin
      // ShowMessage(List[List.Count - 1] + ' NOT On Curve.');
      List[List.Count - 1] := List[List.Count - 1] + ' NOT On Curve.'
    end;
  end;

  ShowMessage('P Jacobian Multiple is '#13#10#13#10 + List.Text);

  List.Free;
end;

procedure TFormEcc.FormCreate(Sender: TObject);
var
  C: TCnEccCurveType;
begin
  pgc1.ActivePageIndex := 0;

  FEcc64E2311 := TCnInt64Ecc.Create(1, 1, 23, 5, 4, 7);
  // 9,7 Ϊ���㣬28 �Ǹ����ߵĽף�ѡ�������� 7 ��Ϊ����Ľף�����Ϊ 5,4
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
  cbbKeyHash.ItemIndex := 0;

  cbbCurveTypes.Items.Clear;
  for C := Low(TCnEccCurveType) to High(TCnEccCurveType) do
    cbbCurveTypes.Items.Add(GetEnumName(TypeInfo(TCnEccCurveType), Ord(C)));

  CallUseless;
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
  try
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

      if Q.X <> 0 then // ע�� X Ϊ 0 ʱ PlainToPoint ֱ�����������
      begin
        // ˳��� Q ���ϵ� X Y ������֤
        if not Ecc.PlainToPoint(X1, Q) then // �󲻳� X ָ���� Y
        begin
          mmoGenECCPoints.Lines.Assign(List);
          ShowMessage(Format('Error %d: X: %d', [I, X1]));
          // Exit;
        end // �������Ҫ�ж��Ƿ��֮ǰ�����һ��,
        else if (Q.Y <> Y1) and (Q.Y <> Ecc.FiniteFieldSize - Y1) then
        begin
          mmoGenECCPoints.Lines.Assign(List);
          ShowMessage(Format('Error %d: X: %d. Y %d <> %d', [I, X1, Y1, Q.Y]));
          // Exit;
        end;
      end;
    end;
    mmoGenECCPoints.Lines.Assign(List);
  finally
    List.Free;
    Ecc.Free;
  end;
end;

// �������õ·��� ( A / P) ��ֵ
function CalcLegendre(A, P: Int64): Integer;
begin
  // ���������P ������ A ʱ���� 0����������ʱ����� A ����ȫƽ�����ͷ��� 1�����򷵻� -1
  if A mod P = 0 then
    Result := 0
  else if MontgomeryPowerMod(A, (P - 1) shr 1, P) = 1 then // ŷ���б�
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
  P3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  if rbBNAddNormal.Checked then
    FBNEcc.PointInverse(P)
  else
  begin
    P3 := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(P, P3);
    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffinePointInverse(P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianPointInverse(P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;
    P3.Free;
  end;

  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
end;

procedure TFormEcc.btnBNEccInverseAddClick(Sender: TObject);
var
  P: TCnEccPoint;
  P3, Q3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  if rbBNAddNormal.Checked then
  begin
    FBNEcc.PointInverse(P);
    FBNEcc.PointAddPoint(FBNEcc.Generator, P, P);
  end
  else
  begin
    P3 := TCnEcc3Point.Create;
    Q3 := TCnEcc3Point.Create;

    CnEccPointToEcc3Point(P, P3);
    CnEccPointToEcc3Point(P, Q3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffinePointInverse(P3);
      FBNEcc.AffinePointAddPoint(P3, Q3, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianPointInverse(P3);
      FBNEcc.JacobianPointAddPoint(P3, Q3, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    Q3.Free;
    P3.Free;
  end;

  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('NOT on Curve');
  P.Free;
end;

procedure TFormEcc.btnBNEccGx2Click(Sender: TObject);
var
  P: TCnEccPoint;
  P3, Q3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  if rbBNAddNormal.Checked then
    FBNEcc.PointAddPoint(FBNEcc.Generator, P, P)
  else
  begin
    P3 := TCnEcc3Point.Create;
    Q3 := TCnEcc3Point.Create;

    CnEccPointToEcc3Point(P, P3);
    CnEccPointToEcc3Point(P, Q3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffinePointAddPoint(P3, Q3, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianPointAddPoint(P3, Q3, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    Q3.Free;
    P3.Free;
  end;

  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
end;

procedure TFormEcc.btnBNEccG2SubGClick(Sender: TObject);
var
  P: TCnEccPoint;
  P3, Q3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  if rbBNAddNormal.Checked then
  begin
    FBNEcc.PointAddPoint(FBNEcc.Generator, P, P);
    FBNEcc.PointSubPoint(P, FBNEcc.Generator, P);
  end
  else
  begin
    P3 := TCnEcc3Point.Create;
    Q3 := TCnEcc3Point.Create;

    CnEccPointToEcc3Point(P, P3);
    CnEccPointToEcc3Point(P, Q3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffinePointAddPoint(P3, Q3, P3);
      FBNEcc.AffinePointSubPoint(P3, Q3, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianPointAddPoint(P3, Q3, P3);
      FBNEcc.JacobianPointSubPoint(P3, Q3, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    Q3.Free;
    P3.Free;
  end;

  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
end;

procedure TFormEcc.btnBNEccGAddGClick(Sender: TObject);
var
  P: TCnEccPoint;
  K: TCnBigNumber;
  P3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  K := TCnBigNumber.Create;
  K.SetDec('2');
  if rbBNAddNormal.Checked then
    FBNEcc.MultiplePoint(K, P)
  else
  begin
    P3 := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(P, P3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffineMultiplePoint(K, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianMultiplePoint(K, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    P3.Free;
  end;

  edtBNEccResult.Text := CnEccPointToString(P);
  if not FBNEcc.IsPointOnCurve(P) then
    ShowMessage('Error');
  P.Free;
  K.Free;
end;

procedure TFormEcc.btnBNEccGSubGClick(Sender: TObject);
var
  P: TCnEccPoint;
  P3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  if rbBNAddNormal.Checked then
    FBNEcc.PointSubPoint(P, FBNEcc.Generator, P)
  else
  begin
    P3 := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(P, P3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffinePointSubPoint(P3, P3, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianPointSubPoint(P3, P3, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    P3.Free;
  end;
  edtBNEccResult.Text := CnEccPointToString(P);
  P.Free;
end;

procedure TFormEcc.btnBNEccNGClick(Sender: TObject);
var
  P: TCnEccPoint;
  K: TCnBigNumber;
  P3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  K := TCnBigNumber.Create;
  K.SetDec(edtBNEccOrder.Text);
  if rbBNAddNormal.Checked then
    FBNEcc.MultiplePoint(K, P)
  else
  begin
    P3 := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(P, P3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffineMultiplePoint(K, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianMultiplePoint(K, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    P3.Free;
  end;
  edtBNEccResult.Text := CnEccPointToString(P);
  P.Free;
  K.Free;
end;

procedure TFormEcc.btnBNEcc4GClick(Sender: TObject);
var
  P, Q: TCnEccPoint;
  K: TCnBigNumber;
  P3, Q3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  Q.Assign(FBNEcc.Generator);

  K := TCnBigNumber.Create;
  K.SetDec('2');

  if rbBNAddNormal.Checked then
  begin
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
    FBNEcc.PointAddPoint(Q, FBNEcc.Generator, Q); // ǰ������λ�þͻ����
    if not FBNEcc.IsPointOnCurve(Q) then
      ShowMessage('Error G + G + G');
    FBNEcc.PointAddPoint(Q, FBNEcc.Generator, Q);
    if not FBNEcc.IsPointOnCurve(Q) then
      ShowMessage('Error G + G + G + G');

    if CnEccPointsEqual(P, Q) then
      ShowMessage('Equal');
  end
  else
  begin
    P3 := TCnEcc3Point.Create;
    Q3 := TCnEcc3Point.Create;

    CnEccPointToEcc3Point(P, P3);
    CnEccPointToEcc3Point(P, Q3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffineMultiplePoint(K, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(P) then
        ShowMessage('Affine Error * 2');

      FBNEcc.AffineMultiplePoint(K, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(P) then
        ShowMessage('Affine Error * 4');

      edtBNEccResult.Text := CnEccPointToString(P);

      CnEccPointToEcc3Point(FBNEcc.Generator, P3); // P3 �ǹ̶��� G
      FBNEcc.AffinePointAddPoint(Q3, P3, Q3);
      CnAffinePointToEccPoint(Q3, Q, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(Q) then
        ShowMessage('Error Affine G + G');
      FBNEcc.AffinePointAddPoint(Q3, P3, Q3);
      CnAffinePointToEccPoint(Q3, Q, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(Q) then
        ShowMessage('Error Affine G + G + G');
      FBNEcc.AffinePointAddPoint(Q3, P3, Q3);
      CnAffinePointToEccPoint(Q3, Q, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(Q) then
        ShowMessage('Error Affine G + G + G + G');

      if CnEccPointsEqual(P, Q) then
        ShowMessage('Equal');
    end
    else
    begin
      FBNEcc.JacobianMultiplePoint(K, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(P) then
        ShowMessage('Affine Error * 2');

      FBNEcc.JacobianMultiplePoint(K, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(P) then
        ShowMessage('Affine Error * 4');

      edtBNEccResult.Text := CnEccPointToString(P);

      CnEccPointToEcc3Point(FBNEcc.Generator, P3); // P3 �ǹ̶��� G
      FBNEcc.JacobianPointAddPoint(Q3, P3, Q3);
      CnJacobianPointToEccPoint(Q3, Q, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(Q) then
        ShowMessage('Error Affine G + G');
      FBNEcc.JacobianPointAddPoint(Q3, P3, Q3);
      CnJacobianPointToEccPoint(Q3, Q, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(Q) then
        ShowMessage('Error Affine G + G + G');
      FBNEcc.JacobianPointAddPoint(Q3, P3, Q3);
      CnJacobianPointToEccPoint(Q3, Q, FBNEcc.FiniteFieldSize);
      if not FBNEcc.IsPointOnCurve(Q) then
        ShowMessage('Error Affine G + G + G + G');

      if CnEccPointsEqual(P, Q) then
        ShowMessage('Equal');
    end;

    Q3.Free;
    P3.Free;
  end;
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
  Sec: TCnEccPublicKey;
begin
  Priv1 := TCnEccPrivateKey(TCnBigNumber.FromHex('E32868331FA8EF0138DE0DE85478346AEC5E3912B6029AE71691C384237A3EEB'));
  Priv2 := TCnEccPrivateKey(TCnBigNumber.FromHex('CEF147652AA90162E1FFF9CF07F2605EA05529CA215A04350A98ECC24AA34342'));

  Pub1 := TCnEccPublicKey.Create;
  Pub2 := TCnEccPublicKey.Create;

  CnEccDiffieHellmanGenerateOutKey(FBNEcc, Priv1, Pub1);
  CnEccDiffieHellmanGenerateOutKey(FBNEcc, Priv2, Pub2);

  ShowMessage('Pub1 is:' + #13#10 + Pub1.X.ToHex + #13#10 + Pub1.Y.ToHex);
  // 86B1AA5120F079594348C67647679E7AC4C365B2C01330DB782B0BA611C1D677, 5F4376A23EED633657A90F385BA21068ED7E29859A7FAB09E953CC5B3E89BEBA

  ShowMessage('Pub2 is:' + #13#10 + Pub2.X.ToHex + #13#10 + Pub2.Y.ToHex);
  // 4034127647BB7FDAB7F1526C7D10BE8B28174E2BBA35B06FFD8A26FC2C20134A, 9E773199EDC1EA792B150270EA3317689286C9FE239DD5B9C5CFD9E81B4B632

  Sec := TCnEccPublicKey.Create;
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

  // �� P ���� 24 λ�ڳ� 2^24 ���ռ����������޽�
  if P.GetBitsCount + CN_ECC_PLAIN_DATA_BITS_GAP - 1 > FBNEcc.FiniteFieldSize.GetBitsCount then
    raise ECnEccException.Create('Data Too Large.');

  BigNumberShiftLeft(P, P, CN_ECC_PLAIN_DATA_BITS_GAP);
  BigNumberAddWord(Q, 1);
  BigNumberShiftLeft(Q, Q, CN_ECC_PLAIN_DATA_BITS_GAP);

  repeat
    // ���� Pt.X ������Χ�ڵ�ÿһ�� X���� Pt.Y
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
  // 4/182/661/1/214/673  �� 8u5 �͵Ĳ���ͨ��
  // 15/194/64403/41589/5579 �� 4u3 �͵Ĳ���ͨ��
  // 12/199/73/21/21/61 �� 8u1 �͵�Ҳ��������ͨ��

//P := GetTickCount;
//for I := 0 to 100000 do     // һ���Ҫ 453/469 ���룬ʮ���Ҫ 4484 ����
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

// ������ݹ���� Lucas ���У���������׼ȷ
procedure TFormEcc.CalcLucas(X, Y, U_2, V_2, U_1, V_1: Int64; var U, V: Int64);
begin
  U := X * U_1 - Y * U_2;
  V := X * V_1 - Y * V_2;
end;

// ֧�� A��B Ϊ�����ĳ˻�ȡģ���� C ��Ҫ������������������
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

// ���� Lucas �����Լ�ģ�����ֲ�ͬʵ�ֺ�����������Բ��ϣ����á�
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

// P1363 �ϵ� Lucas ���㣬��Ȼ�� SM2 ���˵������ȫ���Բ��Ϻţ���Ŀǰ���������������
// V0 = 2, V1 = X, and Vk = X * Vk-1 - Y * Vk-2   for k >= 2
// V ���� Vk mod N��Q ���� Y ^ (K div 2) mod N
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

// ��һ�� Lucas ���㣬V �Ե��Ϻŵ� U ������
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

// ���� X, Y �ĵ� K �� Lucas ���� mod p ��ֵ����̫�Ծ�
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
      // �ñ��ֵĳ�ʼֵ U1��V1 ������һ�ֵ� U��V������ֵ�� U1��V1
      BigNumberMulMod(U, U1, V1, P);   // U = (U*V) mod p ���� U ʱ���ܸı� U1 ��Ϊ ���� V ʱ��Ҫ�õ�

      BigNumberMul(V1, V1, V1);        // V = ((V^2 +D*U^2)/2) mod p  // ���� V ʱ���ı� U1��V1
      BigNumberMul(U1, U1, U1);
      BigNumberMul(U1, U1, D);
      BigNumberAdd(V1, U1, V1);
      BigNumberShiftRight(V1, V1, 1);
      BigNumberMod(V, V1, P);

      BigNumberCopy(U1, U);
      BigNumberCopy(V1, V);
      if BigNumberIsBitSet(K, I) then
      begin
        // �� U1��V1 �� U��V������ֵ�� U1��V1
        BigNumberMul(U, U1, X);   // U = ((X * U +V)/2) mod p
        BigNumberAdd(U, U, V1);
        BigNumberShiftRight(U, U, 1);
        BigNumberMod(U, U, P);    // ���� U ʱ���ܸı� U1 ��Ϊ ���� V ʱ��Ҫ�õ�

        BigNumberMul(V1, V1, X);        // V = ((X*V + D*U)/2) mod p
        BigNumberMul(U1, U1, D);
        BigNumberAdd(V1, U1, V1);
        BigNumberShiftRight(V1, V1, 1); // ���� V ʱ���ı� U1��V1
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

// ��װ�Ķ��� P Ϊ 8*u+1 ������������ Lucas ��������ģƽ����
function SquareRootModPrimeLucas(X, P: Int64; out Y: Int64): Boolean;
var
  G, Z, U, V: Int64;

  function RandomInt64LessThan(HighValue: Int64): Int64;
  var
    Hi, Lo: Cardinal;
  begin
    Randomize;
    Hi := Trunc(Random * High(Integer) - 1) + 1;   // Int64 ���λ������ 1�����⸺��
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
    // ���ȡ X
    X := RandomInt64LessThan(P);

    // �ټ��� Lucas �����е� V�����±� K Ϊ (P+1)/2
    CalcLucasSequenceMod(X, G, (P + 1) shr 1, P, U, V);

    // V ż��ֱ������ 1 �� mod P��V ����� P ������ 1
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
  X, Y, P: Int64;
begin
  P := StrToInt64(edtSRP.Text);
  X := StrToInt64(edtSRX.Text);
  edtSRY.Text := '';

  edtSRY.Text := IntToStr(CnInt64SquareRoot(X, P));
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
  P := 73;    // ������ 8u+1 ��

  T1 := GetTickCount;
  for I := 0 to COUNT - 1 do
  begin
    SquareRootModPrimeLucas(X, P, Y);                    // 1.2 ��
  end;
  T1 := GetTickCount - T1;

  T2 := GetTickCount;
  for I := 0 to COUNT - 1 do
  begin
    TCnInt64EccHack(nil).TonelliShanks(X, P, Y);         // 0.7 ��
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
    if not BigNumberLucasVSequenceMod(X, Y, K, N, Q, V) then
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
    S := InputBox('Password', 'Enter Password if the PEM is Encrypted.', '');

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
  // ���� X �� Y ����֤
  P := TCnEccPoint.Create;
  if FKeyEcc.PlainToPoint(FPublicKey.X, P) then
  begin
    if FPublicKey.Y.IsZero then
      BigNumberCopy(FPublicKey.Y, P.Y)
    else if BigNumberCompare(FPublicKey.Y, P.Y) = 0 then
      ShowMessage('Public Key Verify OK.');
  end;

  if FKeyEcc.IsPointOnCurve(FPublicKey) then
    ShowMessage('PublicKey is On Curve.');
  P.Assign(FPublicKey);
  FKeyEcc.MultiplePoint(FKeyEcc.Order, P);
  if P.IsZero then
    ShowMessage('PublicKey Point * Order = O');

  P.Free;
end;

procedure TFormEcc.btnSaveKeyClick(Sender: TObject);
begin
  if dlgSave1.Execute then
    if CnEccSaveKeysToPem(dlgSave1.FileName, FPrivateKey, FPublicKey, FCurveType) then
      ShowMessage('Private Key Save OK.');
// CnEccSavePublicKeyToPem(dlgSave1.FileName, PublicKey, CurveType);
end;

procedure TFormEcc.btnKeySignClick(Sender: TObject);
var
  InStream, OutStream: TMemoryStream;
  S: AnsiString;
begin
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  S := edtKeyData.Text; //  'abc';
  InStream.Write(S[1], Length(S));

  // TEST
  // FKeyEcc.Load(ctRfc4754ECDSAExample256);
  // FPrivateKey.SetHex('DC51D3866A15BACDE33D96F992FCA99DA7E6EF0934E7097559C27F1614C88A7F');
  // FPublicKey.X.SetHex('2442A5CC0ECD015FA3CA31DC8E2BBC70BF42D60CBCA20085E0822CB04235E970');
  // FPublicKey.Y.SetHex('6FC98BD7E50211A4A27102FA3549DF79EBCB4BF246B80945CDDFE7D509BBFD7D');
  // TEST

  if CnEccSignStream(InStream, OutStream, FKeyEcc, FPrivateKey,
    TCnEccSignDigestType(cbbKeyHash.ItemIndex)) then
  begin
    if dlgSave1.Execute then
      OutStream.SaveToFile(dlgSave1.FileName);

    SetLength(S, OutStream.Size);
    OutStream.Position := 0;
    OutStream.Read(S[1], OutStream.Size);
    edtKeySign.Text := DataToHex(@S[1], Length(S));
  end;

  InStream.Free;
  OutStream.Free;
  SetLength(S, 0);
end;

procedure TFormEcc.btnKeyVerifyClick(Sender: TObject);
var
  InStream, SignStream: TMemoryStream;
  S: AnsiString;
  B: TBytes;
begin
  InStream := TMemoryStream.Create;
  SignStream := TMemoryStream.Create;
  S := edtKeyData.Text; // 'abc'
  InStream.Write(S[1], Length(S));

  B := HexToBytes(edtKeySign.Text);
  SignStream.Write(B[0], Length(B));
  SignStream.Position := 0;

  if CnEccVerifyStream(InStream, SignStream, FKeyEcc, FPublicKey,
    TCnEccSignDigestType(cbbKeyHash.ItemIndex)) then
    ShowMessage('Verify OK.')
  else
    ShowMessage('Verify Fail.');

  InStream.Free;
  SignStream.Free;
  SetLength(S, 0);
  SetLength(B, 0);
end;

procedure TFormEcc.btnKeyGenerateClick(Sender: TObject);
begin
  FKeyEcc.GenerateKeys(FPrivateKey, FPublicKey);
  edtKeyPrivate.Text := FPrivateKey.ToDec;
  edtKeyPublic.Text := CnEccPointToString(FPublicKey);
end;

procedure TFormEcc.btnKeyLoadSigClick(Sender: TObject);
var
  Stream: TMemoryStream;
  S: AnsiString;
begin
  if dlgOpen1.Execute then
  begin
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(dlgOpen1.FileName);

    SetLength(S, Stream.Size);
    Stream.Position := 0;
    Stream.Read(S[1], Stream.Size);
    edtKeySign.Text := DataToHex(@S[1], Length(S));

    Stream.Free;
  end;
end;

procedure TFormEcc.btnSimpleAttackClick(Sender: TObject);
var
  I, J: Integer;
  Ecc: TCnInt64Ecc;
  Gi, Q, Qi, Sum: TCnInt64EccPoint;
  Factors, Remains: array of TUInt64;
begin
{
  ������E/F1021 �ϵ���Բ����: y^2 = x^3 + 905x + 100����Ҳ���ǵ�����Ϊ 966
  ���㣺1006, 416���е㣨612, 827������������� G �� k ����� k ֵ

  966 = 2 * 3 * 7 * 23��Ҫ�� k ֵ��ֻ��Ҫ�����Ⱥ�ں� k ͬ��ļ���ֵ��
  Ȼ�����й�ʣ�ඨ���� k��
  ����� 2��3��7��23 ����Ⱥ��ͬ���ֵ�أ�

  �����ÿһ�������� i������ G �� 966/i ���� Gi��ͬʱҲ�� Q �� 966/i ���� Qi��
  �ٴ� 1 �� i - 1 �������� Qi �ļ������� Gi����������� ki
}

  SetLength(Factors, 4);
  SetLength(Remains, 4);
  Factors[0] := 2; Factors[1] := 3; Factors[2] := 7; Factors[3] := 23;
  Ecc := TCnInt64Ecc.Create(905, 100, 1021, 1006, 416, 966);
  // G ���������û�ṩ��������ע�͵��Ĵ���������� 966��˵����������е� h = 1

//  for I := 1 to 1000 do
//  begin
//    GT := Ecc.Generator;
//    Ecc.MultiplePoint(I, GT);
//    if (GT.X = 0) and (GT.Y = 0) then
//      ShowMessage(IntToStr(I));
//  end;

  for I := Low(Factors) to High(Factors) do
  begin
    Gi := Ecc.Generator;                                                   //  G483    G322      G138      G42
    Ecc.MultiplePoint(966 div Factors[I], Gi);  // �õ� 966/i ���� Gi���ֱ��� (174,0) (147,933) (906,201) (890,665)

    Q.X := 612; // G687 ��Ҫ���
    Q.Y := 827;
                                                                   // ����ֵ   1        0          1       20
    Qi := Q;
    Ecc.MultiplePoint(966 div Factors[I], Qi); // �õ� 966/i ���� Qi���ֱ��� (174,0)   (0,0)    (906,201) (68,281)
                                                                          //  G483/Q483 G0/Q322  G138/Q138 G840/Q42
    ShowMessage(Format('G%d: %s. Q%d: %s',[I, CnInt64EccPointToString(Gi), I, CnInt64EccPointToString(Qi)]));

    if (Qi.X = 0) and (Qi.Y = 0) then
    begin
      // Gi �� 0 ������ 0
      ShowMessage(Format('Found k 0 for factor %d', [Factors[I]]));
      Remains[I] := 0;
    end
    else
    begin
      Sum := Gi;
      // ѭ���� Qi �� Gi �ļ�����, ����ۼ��� Sum �У�Ҳ������ 42*X mod 1021 = 840 ���� X = 20
      for J := 1 to Factors[I] do
      begin
        if (Qi.X = Sum.X) and (Qi.Y = Sum.Y) then
        begin
          ShowMessage(Format('Found k %d for factor %d', [J, Factors[I]]));
          Remains[I] := J;
        end;
        Ecc.PointAddPoint(Gi, Sum, Sum);
      end;
    end;
  end;

  // ��ø����������й�ʣ�ඨ����� 687
  ShowMessage(IntToStr(ChineseRemainderTheoremInt64(Remains, Factors)));
  Ecc.Free;
end;

procedure TFormEcc.btnTestCRTClick(Sender: TObject);
var
  R, F: array of TUInt64;
  C: TUInt64;
begin
  SetLength(R, 4);
  SetLength(F, 4);

  // �й�ʣ�ඨ�����׵������ӽ�С����Բ���ߣ�������Դ��
  // Craig Costello �ġ�Pairings for beginners���е� Example 2.2.2
  F[0] := 2; F[1] := 3; F[2] := 7; F[3] := 23;
  R[0] := 1; R[1] := 0; R[2] := 1; R[3] := 20;
  C := ChineseRemainderTheoremInt64(R, F);
  ShowMessage(IntToStr(C)); // �õ� 687
end;

procedure TFormEcc.btnBNUpdateClick(Sender: TObject);
begin
//  FBNEcc.Load(IntToHex(StrToInt(edtBNEccA.Text), 8),
//    IntToHex(StrToInt(edtBNEccB.Text), 8),
//    IntToHex(StrToInt(edtBNEccP.Text), 8),
//    IntToHex(StrToInt(edtBNEccGX.Text), 8),
//    IntToHex(StrToInt(edtBNEccGY.Text), 8),
//    IntToHex(StrToInt(edtBNEccOrder.Text), 8));

  FBNEcc.CoefficientA.SetDec(edtBNEccA.Text);
  FBNEcc.CoefficientB.SetDec(edtBNEccB.Text);
  FBNEcc.FiniteFieldSize.SetDec(edtBNEccP.Text);
  FBNEcc.Generator.X.SetDec(edtBNEccGX.Text);
  FBNEcc.Generator.Y.SetDec(edtBNEccGY.Text);
  FBNEcc.Order.SetDec(edtBNEccOrder.Text);
end;

procedure TFormEcc.btnBNEccCalcClick(Sender: TObject);
var
  P: TCnEccPoint;
  K: TCnBigNumber;
  P3: TCnEcc3Point;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  K := TCnBigNumber.Create;
  K.SetDec(InputBox('Enter', 'Enter a Multiple Count', '10'));
  if rbBNAddNormal.Checked then
    FBNEcc.MultiplePoint(K, P)
  else
  begin
    P3 := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(P, P3);

    if rbBNAddAffine.Checked then
    begin
      FBNEcc.AffineMultiplePoint(K, P3);
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      FBNEcc.JacobianMultiplePoint(K, P3);
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    P3.Free;
  end;
  edtBNEccResult.Text := CnEccPointToString(P);
  P.Free;
  K.Free;
end;

procedure TFormEcc.cbbInt64EccPresetChange(Sender: TObject);
begin
  case cbbInt64EccPreset.ItemIndex of
    0:
    begin
      edtEccA.Text := '12';
      edtEccB.Text := '199';
      edtEccP.Text := '73';
      edtEccGX.Text := '21';
      edtEccGY.Text := '21';
      edtEccOrder.Text := '61';
    end;
    1:
    begin
      edtEccA.Text := '905';
      edtEccB.Text := '100';
      edtEccP.Text := '1021';
      edtEccGX.Text := '1006';
      edtEccGY.Text := '416';
      edtEccOrder.Text := '966';
    end;
  end;
end;

procedure TFormEcc.btnEccTestAddClick(Sender: TObject);
var
  P, A, B, X, Y, N, X1, Y1: Int64;
  Ecc: TCnInt64Ecc;
  Q: TCnInt64EccPoint;
begin
  A := StrToInt(edtEccA.Text);
  B := StrToInt(edtEccB.Text);
  X := StrToInt(edtEccGX.Text);
  Y := StrToInt(edtEccGY.Text);
  P := StrToInt(edtEccP.Text);
  N := StrToInt(edtEccOrder.Text);

  Ecc := TCnInt64Ecc.Create(A, B, P, X, Y, N);
  mmoGenECCPoints.Lines.Clear;
  chtEccInt64.BottomAxis.Maximum := P - 1;
  chtEccInt64.LeftAxis.Maximum := P - 1;

  Q.X := X;
  Q.Y := Y;
  Ecc.MultiplePoint(StrToInt(InputBox('Enter', 'Enter a Multiple Count', '23')), Q);

  if (Q.X = 0) and (Q.Y = 0) then
    mmoGenECCPoints.Lines.Add(Format('*** (%d,%d)***', [Q.X, Q.Y]))
  else
    mmoGenECCPoints.Lines.Add(Format('(%d,%d)', [Q.X, Q.Y]));

  if Ecc.IsPointOnCurve(Q) then
    ShowMessage('Point is on Curve.')
  else
    ShowMessage('Point is NOT on Curve.');

  X1 := Q.X;
  Y1 := Q.Y;

  // ˳��� Q ���ϵ� X Y ������֤
  if X1 <> 0 then // ��� X1 �� 0��PlainToPoint ��ֱ�ӷ������
  begin
    if not Ecc.PlainToPoint(X1, Q) then
    begin
      ShowMessage(Format('Error: X: %d', [X1]));
    end
    else if (Q.Y <> Y1) and (Q.Y <> Ecc.FiniteFieldSize - Y1) then
    begin
      ShowMessage(Format('Error: X: %d. Y %d <> %d', [X1, Y1, Q.Y]));
    end;
  end;
  Ecc.Free;
end;

procedure TFormEcc.btnHassenTestClick(Sender: TObject);
var
  Ecc: TCnInt64Ecc;
  D1, D2, D3: TCnInt64EccPoint;
  I: Integer;
begin
  // ������������Բ�����ϵ�һ���������� P(x, y)
  // ��֤���Ƿ���� ��^2 - t��+ q = 0
  // Ҳ��������� (x^(q^2), y^(q^2) - t * (x^q, y^q) + q * (x, y) = 0 �е� t
  // �� F1021 �϶���� y2 = x3 + 905 + 100  P(1006, 416) ��Ϊ 966��������ҲΪ 966����ô t Ӧ���� 1021 + 1 - 966 = 56

  Ecc := TCnInt64Ecc.Create(905, 100, 1021, 1006, 416, 966);
  D1 := Ecc.Generator;
  D1.X := MontgomeryPowerMod(D1.X, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D1.Y := MontgomeryPowerMod(D1.Y, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D2 := Ecc.Generator;
  Ecc.MultiplePoint(Ecc.FiniteFieldSize, D2);

  Ecc.PointAddPoint(D1, D2, D1);  // D1 �õ� (x^(q^2), y^(q^2) + q * (x, y) Ҫ��֤������ t * (x^q, y^q)

  D2 := Ecc.Generator;
  D2.X := MontgomeryPowerMod(D2.X, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D2.Y := MontgomeryPowerMod(D2.Y, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D3 := D2;
  // D2 �õ� (x^q, y^q) �� D3 �ۼ���֤���ٵ��� D1
  for I := 1 to Ecc.FiniteFieldSize - 1 do // ʵ���ϲ���Ҫ��ô��
  begin
    if CnInt64EccPointsEqual(D3, D1) then
    begin
      ShowMessage(IntToStr(I));  // �õ� 56 ���� 1021 + 1 - 966 = 56
      Break;
    end;
    Ecc.PointAddPoint(D3, D2, D3);
  end;

  Ecc.Free;
end;

procedure TFormEcc.btnHassenTest2Click(Sender: TObject);
var
  Ecc: TCnInt64Ecc;
  D1, D2, D3: TCnInt64EccPoint;
  I: Integer;
begin
  // ������������Բ�����ϵ�һ���������� P(x, y)
  // ��֤���Ƿ���� ��^2 - t��+ q = 0
  // Ҳ��������� (x^(q^2), y^(q^2) - t * (x^q, y^q) + q * (x, y) = 0 �е� t
  // �� F73 �϶���� y2 = x3 + 12x + 199  P(21, 21) ��Ϊ 61��������ҲΪ 61����ô t Ӧ���� 73 + 1 - 61 = 13

  Ecc := TCnInt64Ecc.Create(12, 199, 73, 21, 21, 61);
  D1 := Ecc.Generator;
  D1.X := MontgomeryPowerMod(D1.X, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D1.Y := MontgomeryPowerMod(D1.Y, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D2 := Ecc.Generator;
  Ecc.MultiplePoint(Ecc.FiniteFieldSize, D2);

  Ecc.PointAddPoint(D1, D2, D1);  // D1 �õ� (x^(q^2), y^(q^2) + q * (x, y) Ҫ��֤������ t * (x^q, y^q)

  D2 := Ecc.Generator;
  D2.X := MontgomeryPowerMod(D2.X, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D2.Y := MontgomeryPowerMod(D2.Y, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D3 := D2;
  // D2 �õ� (x^q, y^q) �� D3 �ۼ���֤���ٵ��� D1
  for I := 1 to Ecc.FiniteFieldSize - 1 do // ʵ���ϲ���Ҫ��ô��
  begin
    if CnInt64EccPointsEqual(D3, D1) then
    begin
      ShowMessage(IntToStr(I));  // �õ� 13 ���� 73 + 1 - 13 = 61
      Break;
    end;
    Ecc.PointAddPoint(D3, D2, D3);
  end;

  Ecc.Free;
end;

procedure TFormEcc.ShowMsg(Data: Int64);
var
  S: string;
begin
  S := IntToStr(Data) + ' - ' + TimeToStr(Now);
  mmoGenECCPoints.Lines.Add(S);
  Application.ProcessMessages;
end;

procedure TFormEcc.ShowMsg(const Data: string);
begin
  mmoBNEccPoints.Lines.Add(Data + ' - ' + TimeToStr(Now));
  Application.ProcessMessages;
end;

procedure TFormEcc.btnInt64SchoofTestClick(Sender: TObject);
begin
  // Schoof �㷨����
  ShowMsg(CnInt64EccSchoof(2, 1, 13));   // 8
  ShowMsg(CnInt64EccSchoof(46, 74, 97)); // 80
  ShowMsg(CnInt64EccSchoof(31, -12, 97)); // 112
  ShowMsg(CnInt64EccSchoof(2, 1, 19));    // 27
  ShowMsg(CnInt64EccSchoof(4, 2, 23)); // 21

  ShowMsg(CnInt64EccSchoof(71, 602, 32003)); // 32021
  ShowMsg(CnInt64EccSchoof(7, 1, 48299)); // 47988
  ShowMsg(CnInt64EccSchoof(7, 1, 58657)); // 58971
  ShowMsg(CnInt64EccSchoof(7, 1, 64007)); // 63585
  ShowMsg(CnInt64EccSchoof(7, 1, 65173)); // 65462
  ShowMsg(CnInt64EccSchoof(7, 1, 65423)); // 65340
  ShowMsg(CnInt64EccSchoof(7, 1, 65521)); // 65772
  ShowMsg(CnInt64EccSchoof(7, 1, 65537)); // 65751
  ShowMsg(CnInt64EccSchoof(7, 1, 98993)); // 99279  Singular online �Ϻ˶�ͨ��

  ShowMsg(CnInt64EccSchoof(7, 1, 2147483629)); // 30 �����ң�2147464597   ���� Singular online �����޷�����˶�
  ShowMsg(CnInt64EccSchoof(7, 1, 2147483659)); // 30 �����ң�2147476793

  // < Max Int64 ��ƽ�����ٲ��
  ShowMsg(CnInt64EccSchoof(7, 1, 3037000493)); // 50 �����ң�3036927405

  // < Max UInt32 ���Ի���ͨ����Q ƽ�������� Int64����û���� UInt64
  ShowMsg(CnInt64EccSchoof(7, 1, 4294967291)); // ���������ң�4294994984

  // �ո� > Max UInt32 ���Ի���ͨ����Q ƽ�������� UInt64����û���� 2 * Max UInt64
  ShowMsg(CnInt64EccSchoof(7, 1, 4294967311)); // ���������ң�4295222567

  // < Sqrt(2 * Max UInt64) ���Ի���ͨ����Q ƽ���ӽ� 2 * Max UInt64
  ShowMsg(CnInt64EccSchoof(7, 1, 6074000687)); // �ķ������ң�6074024457

  // > Sqrt(2 * Max UInt64) ����ͨ����Q ƽ������ 2 * Max UInt64
  ShowMsg(CnInt64EccSchoof(7, 1, 6074001169)); // ���� 6074123004 ���ܺʹ����滥��ӡ֤��
end;

procedure TFormEcc.btnEccSchoofClick(Sender: TObject);
var
  A, B, Q, R: TCnBigNumber;
begin
  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  R := TCnBigNumber.Create;

  A.SetWord(2);
  B.SetWord(1);
  Q.SetWord(13);

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 8���ɹ��� T2 = 0��T3 �ĺ͵��� 0 ���� 0��T5 = 1

  A.SetWord(7);
  B.SetWord(1);
  Q.SetWord(65537);

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 65751���ɹ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('2147483629');

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 2147464597���ɹ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetWord(3037000493);

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 3036927405���ɹ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('4294967291');

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 4294994984������������Ĳ��ԣ�Ȼ�����淢��������������ˣ��Ͷ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('6074000687');

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 6074024457������������Ĳ��ԣ�Ȼ�����淢��������������ˣ��Ͷ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('6074001169');

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ�  6074123004���޴��ж϶Է�ֻ��˵���ٱ� Int64 �濿�ף����ϼ�������һ������

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('9223372036854775783');

  if CnEccSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec); // ����һ����Сʱ���õ� 9223372037391309723���޴��ж϶Է�

  R.Free;
  Q.Free;
  B.Free;
  A.Free;
end;

// Ӳ��˫�ر�����ף�����
function InternalInt64CountEccPoints1(A, B, Q: Int64): Int64;
var
  I, J: Int64;
  Ecc64: TCnInt64Ecc;
  P: TCnInt64EccPoint;
begin
  Result := 1;
  Ecc64 := TCnInt64Ecc.Create(A, B, Q, 0, 0, Q);

  I := 0;
  while I < Q do
  begin
    J := 0;
    while J < Q do
    begin
      P.X := I;
      P.Y := J;
      if Ecc64.IsPointOnCurve(P) then
      begin
        Inc(Result);
        if P.Y > 0 then
        begin
          P.Y := Q - P.Y;
          if Ecc64.IsPointOnCurve(P) then
            Inc(Result);
        end;

        // ��� X �Ѿ������ˣ�ÿ�� X �����ж������� Y��
        Break;
      end;
      J := J + 1;
    end;
    // Break ���ˣ�������һ�� X ��ѭ��
    I := I + 1;
  end;
end;

// Ӳ�Ե��ر�����ף�Ҳ����
function InternalInt64CountEccPoints2(A, B, Q: Int64): Int64;
var
  I: Int64;
  Ecc64: TCnInt64Ecc;
  P: TCnInt64EccPoint;
begin
  Result := 1;
  Ecc64 := TCnInt64Ecc.Create(A, B, Q, 0, 0, Q);

  I := 0;
  while I < Q do
  begin
    if Ecc64.PlainToPoint(I, P) then
      Result := Result + 2;
    I := I + 1;
  end;
end;

// Ӳ�Ե������õ·�����ף�����
function InternalInt64CountEccPoints3(A, B, Q: Int64): Int64;
var
  I: Int64;
begin
  Result := 1 + Q;

  I := 0;
  while I < Q do
  begin
    Result := Result + CnInt64Legendre(I * I * I + A * I + B, Q);
    I := I + 1;
  end;
end;

procedure TFormEcc.btnInt64EccCountOrderClick(Sender: TObject);
begin
  // ����һ�󣬻������ܲ��꣬������
  ShowMsg(InternalInt64CountEccPoints1(2, 1, 13));
  ShowMsg(InternalInt64CountEccPoints1(46, 74, 97)); // 80
  ShowMsg(InternalInt64CountEccPoints1(31, -12, 97)); // 112
  ShowMsg(InternalInt64CountEccPoints1(2, 1, 19));    // 27
  ShowMsg(InternalInt64CountEccPoints1(4, 2, 23)); // 21

  ShowMsg(InternalInt64CountEccPoints1(71, 602, 32003)); // 32021  ���� 26 ����
  ShowMsg(InternalInt64CountEccPoints1(7, 1, 48299)); // 47988     ���� 38 ����
  ShowMsg(InternalInt64CountEccPoints1(7, 1, 58657)); // 58971     ���� 101 ����
end;

procedure TFormEcc.btnInt64CountOrder1Click(Sender: TObject);
begin
  // ������������ܣ�����Ҳ�ܲ���
  ShowMsg(InternalInt64CountEccPoints2(2, 1, 13));
  ShowMsg(InternalInt64CountEccPoints2(46, 74, 97)); // 80
  ShowMsg(InternalInt64CountEccPoints2(31, -12, 97)); // 112
  ShowMsg(InternalInt64CountEccPoints2(2, 1, 19));    // 27
  ShowMsg(InternalInt64CountEccPoints2(4, 2, 23)); // 21

  ShowMsg(InternalInt64CountEccPoints2(71, 602, 32003)); // 32021 һ��͸㶨
  ShowMsg(InternalInt64CountEccPoints2(7, 1, 48299)); // 47988    һ��Ҳ�㶨
  ShowMsg(InternalInt64CountEccPoints2(7, 1, 58657)); // 58971    30 ��
end;

procedure TFormEcc.btnInt64CountEccPoints3Click(Sender: TObject);
begin
  // ������ĸ������ܵ��
  ShowMsg(InternalInt64CountEccPoints3(2, 1, 13));
  ShowMsg(InternalInt64CountEccPoints3(46, 74, 97)); // 80
  ShowMsg(InternalInt64CountEccPoints3(31, -12, 97)); // 112
  ShowMsg(InternalInt64CountEccPoints3(2, 1, 19));    // 27
  ShowMsg(InternalInt64CountEccPoints3(4, 2, 23)); // 21

  ShowMsg(InternalInt64CountEccPoints3(71, 602, 32003)); // 32021
  ShowMsg(InternalInt64CountEccPoints3(7, 1, 48299)); // 47988
  ShowMsg(InternalInt64CountEccPoints3(7, 1, 58657)); // 58971

  ShowMsg(InternalInt64CountEccPoints3(7, 1, 98993)); // 99279     ��һ��㶨
end;

procedure TFormEcc.CallUseless;
var
  TIP: TCnInt64Polynomial;
  TIRP: TCnInt64RationalPolynomial;
  TRP: TCnBigNumberPolynomial;
  TBRP: TCnBigNumberRationalPolynomial;
begin
  TIP := TCnInt64Polynomial.Create;
  TIRP := TCnInt64RationalPolynomial.Create;
  TRP := TCnBigNumberPolynomial.Create;
  TBRP := TCnBigNumberRationalPolynomial.Create;

  TIP.ToString;
  TIRP.ToString;
  TBRP.ToString;
  TBRP.ToString;

  TIP.Free;
  TIRP.Free;
  TRP.Free;
  TBRP.Free;
end;

procedure TFormEcc.cbbCurveTypesChange(Sender: TObject);
begin
  if cbbCurveTypes.ItemIndex > 0 then
  begin
    FCurveType := TCnEccCurveType(cbbCurveTypes.ItemIndex);
    FKeyEcc.Load(FCurveType);
    lblCurveTypeText.Caption := GetEnumName(TypeInfo(TCnEccCurveType), cbbCurveTypes.ItemIndex);

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
  end;
end;

procedure TFormEcc.btnInt64AffineClick(Sender: TObject);
var
  P3, Q3, K3: TCnInt64Ecc3Point;
  P, Q: TCnInt64EccPoint;
begin
  P3.X := 14;
  P3.Y := 19;
  P3.Z := 17;

  Q3.X := 6;
  Q3.Y := 18;
  Q3.Z := 4;

  CnInt64AffinePointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  CnInt64AffinePointToEccPoint(Q3, Q, FEcc64E2311.FiniteFieldSize);
  ShowMessage(Format('P %d,%d. Q %d,%d.', [P.X, P.Y, Q.X, Q.Y]));

  FEcc64E2311.AffinePointAddPoint(P3, Q3, K3);
  ShowMessage(Format('Affine Sum: %d,%d,%d', [K3.X, K3.Y, K3.Z]));
  CnInt64AffinePointToEccPoint(K3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage(Format('Affine Sum to P %d,%d.', [P.X, P.Y]));

  P3.X := 14;
  P3.Y := 19;
  P3.Z := 17;

  Q3.X := 6;
  Q3.Y := 19;
  Q3.Z := 1;

  CnInt64AffinePointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  CnInt64AffinePointToEccPoint(Q3, Q, FEcc64E2311.FiniteFieldSize);
  ShowMessage(Format('P %d,%d. Q %d,%d.', [P.X, P.Y, Q.X, Q.Y]));

  FEcc64E2311.AffinePointAddPoint(P3, Q3, P3);
end;

procedure TFormEcc.btnTestJacobianClick(Sender: TObject);
var
  P3, Q3, K3: TCnInt64Ecc3Point;
  P, Q: TCnInt64EccPoint;
begin         
  P3.X := 6;
  P3.Y := 19;
  P3.Z := 1;
  FEcc64E2311.JacobianMultiplePoint(2, P3);  // 4 19 15

  P3.X := 4;
  P3.Y := 19;
  P3.Z := 15;

  Q3.X := 4;
  Q3.Y := 19;
  Q3.Z := 15;

  CnInt64JacobianPointToEccPoint(P3, P, FEcc64E2311.FiniteFieldSize);
  CnInt64JacobianPointToEccPoint(Q3, Q, FEcc64E2311.FiniteFieldSize);
  ShowMessage(Format('P %d,%d. Q %d,%d.', [P.X, P.Y, Q.X, Q.Y]));

  FEcc64E2311.JacobianPointAddPoint(P3, Q3, K3);

  CnInt64JacobianPointToEccPoint(K3, P, FEcc64E2311.FiniteFieldSize);
  ShowMessage(Format('Jacobian Sum to P %d,%d.', [P.X, P.Y]));
end;

procedure TFormEcc.btnBNEccAffineTestClick(Sender: TObject);
var
  P3, Q3, K3: TCnEcc3Point;
  P, Q, K: TCnEccPoint;
  BNEcc: TCnEcc;
begin
  P3 := TCnEcc3Point.Create;
  Q3 := TCnEcc3Point.Create;
  K3 := TCnEcc3Point.Create;

  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;
  K := TCnEccPoint.Create;

  BNEcc := TCnEcc.Create('1', '1', '17', '5', '4', '7'); // ʮ�����ƣ�תΪ 23

  P.X.SetWord(6);
  P.Y.SetWord(19);

  Q.X.SetWord(9);
  Q.Y.SetWord(7);

  CnEccPointToEcc3Point(P, P3);
  CnEccPointToEcc3Point(Q, Q3);

  BNEcc.AffinePointAddPoint(P3, Q3, K3);
  ShowMessage(CnEcc3PointToString(K3));
  CnAffinePointToEccPoint(K3, P, BNEcc.FiniteFieldSize);
  ShowMessage(CnEccPointToString(P));

  BNEcc.Free;

  K.Free;
  Q.Free;
  P.Free;

  K3.Free;
  Q3.Free;
  P3.Free;
end;

procedure TFormEcc.btnBNJacobianTestClick(Sender: TObject);
var
  P3, Q3, K3: TCnEcc3Point;
  P, Q, K: TCnEccPoint;
  BNEcc: TCnEcc;
begin
  P3 := TCnEcc3Point.Create;
  Q3 := TCnEcc3Point.Create;
  K3 := TCnEcc3Point.Create;

  P := TCnEccPoint.Create;
  Q := TCnEccPoint.Create;
  K := TCnEccPoint.Create;

  BNEcc := TCnEcc.Create('1', '1', '17', '5', '4', '7'); // ʮ�����ƣ�תΪ 23

  P.X.SetWord(6);
  P.Y.SetWord(19);

  Q.X.SetWord(9);
  Q.Y.SetWord(7);

  CnEccPointToEcc3Point(P, P3);
  CnEccPointToEcc3Point(Q, Q3);

  BNEcc.JacobianPointAddPoint(P3, Q3, K3);
  ShowMessage(CnEcc3PointToString(K3));
  CnJacobianPointToEccPoint(K3, P, BNEcc.FiniteFieldSize);
  ShowMessage(CnEccPointToString(P));

  BNEcc.Free;

  K.Free;
  Q.Free;
  P.Free;

  K3.Free;
  Q3.Free;
  P3.Free;
end;

procedure TFormEcc.btnMulTimeClick(Sender: TObject);
const
  COUNT = 100;
var
  P: TCnEccPoint;
  K: TCnBigNumber;
  P3: TCnEcc3Point;
  T: Cardinal;
  I: Integer;
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  K := TCnBigNumber.Create;
  K.SetDec('1234567890ABCDEFFEDCBA0987654321');
  if rbBNAddNormal.Checked then
  begin
    T := GetTickCount;
    for I := 1 to COUNT do
      FBNEcc.NormalMultiplePoint(K, P);
    T := GetTickCount - T;
  end
  else
  begin
    P3 := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(P, P3);

    // ����/�ſɱ������ĳ˷������ٶ���߽�ʮ��
    if rbBNAddAffine.Checked then
    begin
      T := GetTickCount;
      for I := 1 to COUNT do
        FBNEcc.AffineMultiplePoint(K, P3);
      T := GetTickCount - T;
      CnAffinePointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end
    else
    begin
      T := GetTickCount;
      for I := 1 to COUNT do
        FBNEcc.JacobianMultiplePoint(K, P3);
      T := GetTickCount - T;
      CnJacobianPointToEccPoint(P3, P, FBNEcc.FiniteFieldSize);
    end;

    P3.Free;
  end;
  edtBNEccResult.Text := CnEccPointToString(P);
  ShowMessage(IntToStr(T));
  P.Free;
  K.Free;
end;

procedure TFormEcc.btnRecoverPubKeyClick(Sender: TObject);
var
  InStream, SignStream: TMemoryStream;
  S: AnsiString;
  B: TBytes;
  Pub1, Pub2: TCnEccPublicKey;
begin
  // ��ǩ����ԭ��Կ
  InStream := TMemoryStream.Create;
  SignStream := TMemoryStream.Create;
  S := edtKeyData.Text; // 'abc'
  InStream.Write(S[1], Length(S));

  B := HexToBytes(edtKeySign.Text);
  SignStream.Write(B[0], Length(B));
  SignStream.Position := 0;

  Pub1 := TCnEccPublicKey.Create;
  Pub2 := TCnEccPublicKey.Create;

  if CnEccRecoverPublicKeyFromStream(InStream, SignStream, FKeyEcc, Pub1, Pub2,
    TCnEccSignDigestType(cbbKeyHash.ItemIndex)) then
  begin
    if (Pub1.ToHex = FPublicKey.ToHex) or (Pub2.ToHex = FPublicKey.ToHex) then
      ShowMessage('Restore OK.')
    else
      ShowMessage('Restore None.');
  end
  else
    ShowMessage('Restore Fail.');

  Pub2.Free;
  Pub1.Free;
  InStream.Free;
  SignStream.Free;
  SetLength(S, 0);
  SetLength(B, 0);
end;

procedure TFormEcc.btnEccFastSchoofClick(Sender: TObject);
var
  A, B, Q, R: TCnBigNumber;
begin
  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  R := TCnBigNumber.Create;

  A.SetWord(46);
  B.SetWord(74);
  Q.SetWord(97);

  // ��������
  // 2 �� 0
  // 3 δ֪
  // 5 �� 3
  // 7 �� 4

  if CnEccFastSchoof(R, A, B, Q) then
    ShowMsg(R.ToDec);

  R.Free;
  Q.Free;
  B.Free;
  A.Free;
end;

procedure TFormEcc.btnEccSchoof2Click(Sender: TObject);
var
  A, B, Q, R: TCnBigNumber;
begin
  A := TCnBigNumber.Create;
  B := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  R := TCnBigNumber.Create;

  A.SetWord(2);
  B.SetWord(1);
  Q.SetWord(13);

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 8���ɹ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetWord(65537);

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 65751���ɹ���


  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('2147483629');

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 2147464597���ɹ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetWord(3037000493);

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 3036927405���ɹ���

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('4294967291');

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 4294994984

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('6074000687');

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ� 6074024457

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('6074001169');

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // �õ�  6074123004

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('13446163232037310043');

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // ����һ����Сʱ���õ� 13446163228212048345���޴��ж϶Է�

  A.SetWord(7);
  B.SetWord(1);
  Q.SetHex('01000000000000000D');

  if CnEccSchoof2(R, A, B, Q) then
    ShowMsg(R.ToDec); // ��������Сʱ���õ� 18446744066071115814���޴��ж϶Է�

  R.Free;
  Q.Free;
  B.Free;
  A.Free;
end;

end.
