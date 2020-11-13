unit UnitEcc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnECC, ExtCtrls, Buttons, TeEngine, Series, TeeProcs,
  Chart, TypInfo, CnPrimeNumber, CnBigNumber, CnNativeDecl, CnCommon, CnPemUtils, CnPolynomial;

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
    procedure ShowMsg(Data: Int64);
    procedure CalcLucas(X, Y, U_2, V_2, U_1, V_1: Int64; var U, V: Int64);

    procedure CallUseless; // 调用必要的函数防止被 Link 时忽略
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

function HexToInt(Hex: AnsiString): Integer;
var
  I, Res: Integer;
  ch: AnsiChar;
begin
  Res := 0;
  for I := 0 to Length(Hex) - 1 do
  begin
    ch := Hex[I + 1];
    if (ch >= '0') and (ch <= '9') then
      Res := Res * 16 + Ord(ch) - Ord('0')
    else if (ch >= 'A') and (ch <= 'F') then
      Res := Res * 16 + Ord(ch) - Ord('A') + 10
    else if (ch >= 'a') and (ch <= 'f') then
      Res := Res * 16 + Ord(ch) - Ord('a') + 10
    else raise Exception.Create('Error: not a Hex String');
  end;
  Result := Res;
end;

function MyStrToHex(Buffer: PAnsiChar; Length: Integer): AnsiString;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  for I := 0 to Length - 1 do
  begin
    B := PByte(Integer(Buffer) + I)^;
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}
      (Digits[(B shr 4) and $0F] + Digits[B and $0F]);
  end;
end;

function MyHexToStr(Hex: string): AnsiString;
var
  S: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Hex) div 2 - 1 do
  begin
    S := Copy(Hex, I * 2 + 1, 2);
    Result := Result + AnsiChar(HexToInt(S));
  end;
end;

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
  cbbKeyHash.ItemIndex := 0;

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

      if Q.X <> 0 then // 注意 X 为 0 时 PlainToPoint 直接用上零点了
      begin
        // 顺便解 Q 点上的 X Y 方程验证
        if not Ecc.PlainToPoint(X1, Q) then // 求不出 X 指定的 Y
        begin
          mmoGenECCPoints.Lines.Assign(List);
          ShowMessage(Format('Error %d: X: %d', [I, X1]));
          // Exit;
        end // 求出来了要判断是否和之前计算的一样,
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
    edtKeySign.Text := MyStrToHex(@S[1], Length(S));
  end;

  InStream.Free;
  OutStream.Free;
  SetLength(S, 0);
end;

procedure TFormEcc.btnKeyVerifyClick(Sender: TObject);
var
  InStream, SignStream: TMemoryStream;
  S: AnsiString;
begin
  InStream := TMemoryStream.Create;
  SignStream := TMemoryStream.Create;
  S := edtKeyData.Text; // 'abc'
  InStream.Write(S[1], Length(S));

  S := MyHexToStr(edtKeySign.Text);
  SignStream.Write(S[1], Length(S));
  SignStream.Position := 0;

  if CnEccVerifyStream(InStream, SignStream, FKeyEcc, FPublicKey,
    TCnEccSignDigestType(cbbKeyHash.ItemIndex)) then
    ShowMessage('Verify OK.')
  else
    ShowMessage('Verify Fail.');

  InStream.Free;
  SignStream.Free;
  SetLength(S, 0);
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
    edtKeySign.Text := MyStrToHex(@S[1], Length(S));

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
  用例：E/F1021 上的椭圆曲线: y^2 = x^3 + 905x + 100，阶也就是点总数为 966
  基点：1006, 416，有点（612, 827），求这个点是 G 的 k 倍点的 k 值

  966 = 2 * 3 * 7 * 23，要求 k 值，只需要求各子群内和 k 同余的几个值，
  然后用中国剩余定理求 k。
  如何求 2、3、7、23 等子群里同余的值呢？

  先针对每一个素因子 i，先求 G 的 966/i 倍点 Gi，同时也求 Q 的 966/i 倍点 Qi，
  再从 1 到 i - 1 遍历，求 Qi 的几倍点是 Gi，这个几就是 ki
}

  SetLength(Factors, 4);
  SetLength(Remains, 4);
  Factors[0] := 2; Factors[1] := 3; Factors[2] := 7; Factors[3] := 23;
  Ecc := TCnInt64Ecc.Create(905, 100, 1021, 1006, 416, 966);
  // G 点阶用例中没提供，用下面注释掉的代码求得正是 966。说明这个曲线中的 h = 1

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
    Ecc.MultiplePoint(966 div Factors[I], Gi);  // 得到 966/i 倍点 Gi，分别是 (174,0) (147,933) (906,201) (890,665)

    Q.X := 612; // G687 是要求的
    Q.Y := 827;
                                                                   // 倍点值   1        0          1       20
    Qi := Q;
    Ecc.MultiplePoint(966 div Factors[I], Qi); // 得到 966/i 倍点 Qi，分别是 (174,0)   (0,0)    (906,201) (68,281)
                                                                          //  G483/Q483 G0/Q322  G138/Q138 G840/Q42
    ShowMessage(Format('G%d: %s. Q%d: %s',[I, CnInt64EccPointToString(Gi), I, CnInt64EccPointToString(Qi)]));

    if (Qi.X = 0) and (Qi.Y = 0) then
    begin
      // Gi 的 0 倍点是 0
      ShowMessage(Format('Found k 0 for factor %d', [Factors[I]]));
      Remains[I] := 0;
    end
    else
    begin
      Sum := Gi;
      // 循环求 Qi 是 Gi 的几倍点, 结果累加在 Sum 中，也就是求 42*X mod 1021 = 840 照理 X = 20
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

  // 求得各余数后用中国剩余定理求得 687
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

  // 中国剩余定理攻击阶的素因子较小的椭圆曲线，用例来源于
  // Craig Costello 的《Pairings for beginners》中的 Example 2.2.2
  F[0] := 2; F[1] := 3; F[2] := 7; F[3] := 23;
  R[0] := 1; R[1] := 0; R[2] := 1; R[3] := 20;
  C := ChineseRemainderTheoremInt64(R, F);
  ShowMessage(IntToStr(C)); // 得到 687
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
begin
  P := TCnEccPoint.Create;
  P.Assign(FBNEcc.Generator);
  K := TCnBigNumber.Create;
  K.SetDec(CnInputBox('Enter', 'Enter a Multiple Count', '10'));
  FBNEcc.MultiplePoint(K, P);
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
  Ecc.MultiplePoint(StrToInt(CnInputBox('Enter', 'Enter a Multiple Count', '23')), Q);

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

  // 顺便解 Q 点上的 X Y 方程验证
  if X1 <> 0 then // 如果 X1 是 0，PlainToPoint 会直接返回零点
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
  // 用例：给定椭圆曲线上的一个点比如基点 P(x, y)
  // 验证其是否符合 π^2 - tπ+ q = 0
  // 也就是穷举求 (x^(q^2), y^(q^2) - t * (x^q, y^q) + q * (x, y) = 0 中的 t
  // 如 F1021 上定义的 y2 = x3 + 905 + 100  P(1006, 416) 阶为 966，点总数也为 966，那么 t 应该是 1021 + 1 - 966 = 56

  Ecc := TCnInt64Ecc.Create(905, 100, 1021, 1006, 416, 966);
  D1 := Ecc.Generator;
  D1.X := MontgomeryPowerMod(D1.X, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D1.Y := MontgomeryPowerMod(D1.Y, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D2 := Ecc.Generator;
  Ecc.MultiplePoint(Ecc.FiniteFieldSize, D2);

  Ecc.PointAddPoint(D1, D2, D1);  // D1 得到 (x^(q^2), y^(q^2) + q * (x, y) 要验证它等于 t * (x^q, y^q)

  D2 := Ecc.Generator;
  D2.X := MontgomeryPowerMod(D2.X, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D2.Y := MontgomeryPowerMod(D2.Y, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D3 := D2;
  // D2 得到 (x^q, y^q) 用 D3 累加验证多少等于 D1
  for I := 1 to Ecc.FiniteFieldSize - 1 do // 实际上不需要这么大
  begin
    if CnInt64EccPointsEqual(D3, D1) then
    begin
      ShowMessage(IntToStr(I));  // 得到 56 符合 1021 + 1 - 966 = 56
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
  // 用例：给定椭圆曲线上的一个点比如基点 P(x, y)
  // 验证其是否符合 π^2 - tπ+ q = 0
  // 也就是穷举求 (x^(q^2), y^(q^2) - t * (x^q, y^q) + q * (x, y) = 0 中的 t
  // 如 F73 上定义的 y2 = x3 + 12x + 199  P(21, 21) 阶为 61，点总数也为 61，那么 t 应该是 73 + 1 - 61 = 13

  Ecc := TCnInt64Ecc.Create(12, 199, 73, 21, 21, 61);
  D1 := Ecc.Generator;
  D1.X := MontgomeryPowerMod(D1.X, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D1.Y := MontgomeryPowerMod(D1.Y, Ecc.FiniteFieldSize * Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D2 := Ecc.Generator;
  Ecc.MultiplePoint(Ecc.FiniteFieldSize, D2);

  Ecc.PointAddPoint(D1, D2, D1);  // D1 得到 (x^(q^2), y^(q^2) + q * (x, y) 要验证它等于 t * (x^q, y^q)

  D2 := Ecc.Generator;
  D2.X := MontgomeryPowerMod(D2.X, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);
  D2.Y := MontgomeryPowerMod(D2.Y, Ecc.FiniteFieldSize, Ecc.FiniteFieldSize);

  D3 := D2;
  // D2 得到 (x^q, y^q) 用 D3 累加验证多少等于 D1
  for I := 1 to Ecc.FiniteFieldSize - 1 do // 实际上不需要这么大
  begin
    if CnInt64EccPointsEqual(D3, D1) then
    begin
      ShowMessage(IntToStr(I));  // 得到 13 符合 73 + 1 - 13 = 61
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
end;

procedure TFormEcc.btnInt64SchoofTestClick(Sender: TObject);
begin
  // Schoof 算法测试
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
  ShowMsg(CnInt64EccSchoof(7, 1, 98993)); // 99279  Singular online 上核对通过

  ShowMsg(CnInt64EccSchoof(7, 1, 2147483629)); // 30 秒左右，2147464597   以下 Singular online 上已无法计算核对
  ShowMsg(CnInt64EccSchoof(7, 1, 2147483659)); // 30 秒左右，2147476793

  // < Max Int64 的平方根再测测
  ShowMsg(CnInt64EccSchoof(7, 1, 3037000493)); // 50 秒左右，3036927405

  // < Max UInt32 测试基本通过，Q 平方超过了 Int64，但没超过 UInt64
  ShowMsg(CnInt64EccSchoof(7, 1, 4294967291)); // 两分钟左右，4294994984

  // 刚刚 > Max UInt32 测试基本通过，Q 平方超过了 UInt64，但没超过 2 * Max UInt64
  ShowMsg(CnInt64EccSchoof(7, 1, 4294967311)); // 三分钟左右，4295222567

  // < Sqrt(2 * Max UInt64) 测试基本通过，Q 平方接近 2 * Max UInt64
  ShowMsg(CnInt64EccSchoof(7, 1, 6074000687)); // 四分钟左右，6074024457

  // > Sqrt(2 * Max UInt64) 测试有问题，Q 平方超过 2 * Max UInt64
  // ShowMsg(CnInt64EccSchoof(7, 1, 6074001169)); // 返回 6074001170 是错的

  // Q 平方超过 2 * Max UInt64 的目前还在想办法处理，因为计算 (Q*Q) shr 1 会出问题
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
    ShowMessage(R.ToDec); // 得到 8，成功！

  A.SetWord(7);
  B.SetWord(1);
  Q.SetWord(65537);

  if CnEccSchoof(R, A, B, Q) then
    ShowMessage(R.ToDec); // 得到 85751，成功！

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('2147483629');

  if CnEccSchoof(R, A, B, Q) then
    ShowMessage(R.ToDec); // 得到 2147464597，成功！

  A.SetWord(7);
  B.SetWord(1);
  Q.SetWord(3037000493);

  if CnEccSchoof(R, A, B, Q) then
    ShowMessage(R.ToDec); // 得到 3036927405，成功！

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('4294967291');

  if CnEccSchoof(R, A, B, Q) then
    ShowMessage(R.ToDec); // 得到 4294994984，本来和上面的不对，然后上面发现有溢出，修正了，就对了

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('6074000687');

  if CnEccSchoof(R, A, B, Q) then
    ShowMessage(R.ToDec); // 得到 6074024457，本来和上面的不对，然后上面发现有溢出，修正了，就对了

  A.SetWord(7);
  B.SetWord(1);
  Q.SetDec('6074001169');

  if CnEccSchoof(R, A, B, Q) then
    ShowMessage(R.ToDec); // 得到  6074123004，无从判断对否，只能说至少比 Int64 版靠谱

  R.Free;
  Q.Free;
  B.Free;
  A.Free;
end;

// 硬性双重遍历求阶，极慢
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

        // 这个 X 已经查完了，每个 X 不会有多于两个 Y。
        Break;
      end;
      J := J + 1;
    end;
    // Break 到此，进行下一个 X 的循环
    I := I + 1;
  end;
end;

// 硬性单重遍历求阶，也不快
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

// 硬性单重勒让德符号求阶，快点儿
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
  // 素数一大，基本上跑不完，不能跑
  ShowMsg(InternalInt64CountEccPoints1(2, 1, 13));
  ShowMsg(InternalInt64CountEccPoints1(46, 74, 97)); // 80
  ShowMsg(InternalInt64CountEccPoints1(31, -12, 97)); // 112
  ShowMsg(InternalInt64CountEccPoints1(2, 1, 19));    // 27
  ShowMsg(InternalInt64CountEccPoints1(4, 2, 23)); // 21

  ShowMsg(InternalInt64CountEccPoints1(71, 602, 32003)); // 32021  跑了 26 分钟
  ShowMsg(InternalInt64CountEccPoints1(7, 1, 48299)); // 47988     跑了 38 分钟
  ShowMsg(InternalInt64CountEccPoints1(7, 1, 58657)); // 58971     跑了 101 分钟
end;

procedure TFormEcc.btnInt64CountOrder1Click(Sender: TObject);
begin
  // 比上面的容易跑，但是也跑不完
  ShowMsg(InternalInt64CountEccPoints2(2, 1, 13));
  ShowMsg(InternalInt64CountEccPoints2(46, 74, 97)); // 80
  ShowMsg(InternalInt64CountEccPoints2(31, -12, 97)); // 112
  ShowMsg(InternalInt64CountEccPoints2(2, 1, 19));    // 27
  ShowMsg(InternalInt64CountEccPoints2(4, 2, 23)); // 21

  ShowMsg(InternalInt64CountEccPoints2(71, 602, 32003)); // 32021 一秒就搞定
  ShowMsg(InternalInt64CountEccPoints2(7, 1, 48299)); // 47988    一秒也搞定
  ShowMsg(InternalInt64CountEccPoints2(7, 1, 58657)); // 58971    30 秒
end;

procedure TFormEcc.btnInt64CountEccPoints3Click(Sender: TObject);
begin
  // 比上面的更容易跑点儿
  ShowMsg(InternalInt64CountEccPoints3(2, 1, 13));
  ShowMsg(InternalInt64CountEccPoints3(46, 74, 97)); // 80
  ShowMsg(InternalInt64CountEccPoints3(31, -12, 97)); // 112
  ShowMsg(InternalInt64CountEccPoints3(2, 1, 19));    // 27
  ShowMsg(InternalInt64CountEccPoints3(4, 2, 23)); // 21

  ShowMsg(InternalInt64CountEccPoints3(71, 602, 32003)); // 32021
  ShowMsg(InternalInt64CountEccPoints3(7, 1, 48299)); // 47988
  ShowMsg(InternalInt64CountEccPoints3(7, 1, 58657)); // 58971

  ShowMsg(InternalInt64CountEccPoints3(7, 1, 98993)); // 99279     都一秒搞定
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

end.
