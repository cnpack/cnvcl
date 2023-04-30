unit UnitSM9;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnBigNumber, FMX.ExtCtrls, CnSM9, CnECC, CnSM3, FMX.Edit, FMX.Memo, FMX.TabControl, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormSM9 = class(TForm)
    pgcSM9: TTabControl;
    tsFP2: TTabItem;
    grpFP2: TGroupBox;
    btnTestFP2: TButton;
    mmoFP2: TMemo;
    tsFP4: TTabItem;
    grpFP4: TGroupBox;
    btnTestFp4: TButton;
    mmoFP4: TMemo;
    tsFP12: TTabItem;
    grpFP12: TGroupBox;
    btnTestFP12: TButton;
    mmoFP12: TMemo;
    tsAffinePoint: TTabItem;
    grpAP: TGroupBox;
    btnAP: TButton;
    mmoAP: TMemo;
    btnFP2PointMul: TButton;
    tsRate: TTabItem;
    grpRate: TGroupBox;
    btnRateTest: TButton;
    mmoRate: TMemo;
    btnRateTime: TButton;
    tsSM9Hash: TTabItem;
    grpSM9Hash: TGroupBox;
    btnTestHash: TButton;
    btnTestHash2: TButton;
    tsSM9Sign: TTabItem;
    grpSM9Sign: TGroupBox;
    lblUserID: TLabel;
    lbl1: TLabel;
    btnSM9GenMaster: TButton;
    btnSM9GenUser: TButton;
    edtSigUserId: TEdit;
    mmoSig: TMemo;
    edtSignData: TEdit;
    btnSM9Sign: TButton;
    btnSM9VerifyData: TButton;
    btnSM9Sample: TButton;
    tsSM9KeyEnc: TTabItem;
    grpKeyEnc: TGroupBox;
    lblKeyLength: TLabel;
    lbl2: TLabel;
    edtKeyEncLength: TEdit;
    btnSM9KeyEncSend: TButton;
    mmoKeyEnc: TMemo;
    btnKeyEncGenMaster: TButton;
    btnKeyEncGenUser: TButton;
    edtDestUser: TEdit;
    btnTestKeyEnc: TButton;
    btnSM9KeyEncRecv: TButton;
    btnTestEnc: TButton;
    mmoEnc: TMemo;
    tsSM9KeyExchange: TTabItem;
    grpKeyExchange: TGroupBox;
    btnKeyExchangeTest: TButton;
    mmoKeyExchange: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestFP2Click(Sender: TObject);
    procedure btnTestFp4Click(Sender: TObject);
    procedure btnTestFP12Click(Sender: TObject);
    procedure btnAPClick(Sender: TObject);
    procedure btnFP2PointMulClick(Sender: TObject);
    procedure btnRateTestClick(Sender: TObject);
    procedure btnRateTimeClick(Sender: TObject);
    procedure btnTestHashClick(Sender: TObject);
    procedure btnTestHash2Click(Sender: TObject);
    procedure btnSM9GenMasterClick(Sender: TObject);
    procedure btnSM9GenUserClick(Sender: TObject);
    procedure btnSM9SignClick(Sender: TObject);
    procedure btnSM9VerifyDataClick(Sender: TObject);
    procedure btnSM9SampleClick(Sender: TObject);
    procedure btnSM9KeyEncSendClick(Sender: TObject);
    procedure btnKeyEncGenMasterClick(Sender: TObject);
    procedure btnKeyEncGenUserClick(Sender: TObject);
    procedure btnTestKeyEncClick(Sender: TObject);
    procedure btnSM9KeyEncRecvClick(Sender: TObject);
    procedure btnTestEncClick(Sender: TObject);
    procedure btnKeyExchangeTestClick(Sender: TObject);
  private
    FP: TCnBigNumber;
    FP21: TCnFP2;
    FP22: TCnFP2;
    FP23: TCnFP2;
    FP41: TCnFP4;
    FP42: TCnFP4;
    FP43: TCnFP4;
    FP121: TCnFP12;
    FP122: TCnFP12;
    FP123: TCnFP12;
    FAP1: TCnFP2AffinePoint;
    FAP2: TCnFP2AffinePoint;
    FAP3: TCnFP2AffinePoint;

    FSigMasterKey: TCnSM9SignatureMasterKey;
    FSigUserKey: TCnSM9SignatureUserPrivateKey;
    FSig: TCnSM9Signature;
    FKeyEncMasterKey: TCnSM9EncryptionMasterKey;
    FKeyEncUserKey: TCnSM9EncryptionUserPrivateKey;
    FKeyExchangeMasterKey: TCnSM9KeyExchangeMasterKey;
    FKeyExchangeUserA, FKeyExchangeUserB: TCnSM9KeyExchangeUserPrivateKey;
    FKeyEnc: TCnSM9KeyEncapsulation;
  public
    { Public declarations }
  end;

var
  FormSM9: TFormSM9;

implementation

{$R *.fmx}

uses
  CnCommon, CnNative;

const
  SM9_PRIME_HEX = 'B640000002A3A6F1D603AB4FF58EC74521F2934B1A7AEEDBE56F9B27E351457D';

procedure TFormSM9.FormCreate(Sender: TObject);
begin
  FP := TCnBigNumber.Create;
  FP.SetHex(SM9_PRIME_HEX);

  FP21 := TCnFP2.Create;
  FP22 := TCnFP2.Create;
  FP23 := TCnFP2.Create;

  FP41 := TCnFP4.Create;
  FP42 := TCnFP4.Create;
  FP43 := TCnFP4.Create;

  FP121 := TCnFP12.Create;
  FP122 := TCnFP12.Create;
  FP123 := TCnFP12.Create;

  FAP1 := TCnFP2AffinePoint.Create;
  FAP2 := TCnFP2AffinePoint.Create;
  FAP3 := TCnFP2AffinePoint.Create;

  FSigMasterKey := TCnSM9SignatureMasterKey.Create;
  FSigUserKey := TCnSM9SignatureUserPrivateKey.Create;
  FSig := TCnSM9Signature.Create;

  FKeyEncMasterKey := TCnSM9EncryptionMasterKey.Create;
  FKeyEncUserKey := TCnSM9EncryptionUserPrivateKey.Create;

  FKeyExchangeMasterKey := TCnSM9KeyExchangeMasterKey.Create;
  FKeyExchangeUserA := TCnSM9KeyExchangeUserPrivateKey.Create;
  FKeyExchangeUserB := TCnSM9KeyExchangeUserPrivateKey.Create;

  FKeyEnc := TCnSM9KeyEncapsulation.Create;
end;

procedure TFormSM9.FormDestroy(Sender: TObject);
begin
  FKeyEnc.Free;
  FKeyExchangeUserB.Free;
  FKeyExchangeUserA.Free;
  FKeyExchangeMasterKey.Free;

  FKeyEncUserKey.Free;
  FKeyEncMasterKey.Free;

  FSig.Free;
  FSigUserKey.Free;
  FSigMasterKey.Free;

  FAP3.Free;
  FAP2.Free;
  FAP1.Free;

  FP123.Free;
  FP122.Free;
  FP121.Free;

  FP43.Free;
  FP42.Free;
  FP41.Free;

  FP23.Free;
  FP22.Free;
  FP21.Free;

  FP.Free;
end;

procedure TFormSM9.btnTestFP2Click(Sender: TObject);

  procedure SetVars;
  begin
    FP21.SetHex('5F25CE2083FC970A6B9FDCD819FB1966D300AF2AFD58D480C59E02B320852183',
      '9ACDDFEF770BCDCE452D72461F9D1482A8EFF7662E1D591C70A7CE35F2F5710C');
    FP22.SetHex('7114F0B7F50EBB85C124558F76F10BD277F71C27384DEB67F229E582BEFDE3EE',
      'AA2714A30D7B8AE08B987FAE8818881FB1952A1F53CDA30A35C72841B174D7D');
  end;

begin
  mmoFP2.Lines.Clear;
  SetVars;

  mmoFP2.Lines.Add('Add:');
  FP2Add(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Add Self:');
  SetVars;
  FP2Add(FP21, FP21, FP21, FP);
  mmoFP2.Lines.Add(FP21.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Mul 3:');
  SetVars;
  FP2Mul3(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Sub:');
  SetVars;
  FP2Sub(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Negate:');
  SetVars;
  FP2Negate(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Mul:');
  SetVars;
  FP2Mul(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('MulU:');
  SetVars;
  FP2MulU(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Mul Self:');
  SetVars;
  FP2Mul(FP21, FP21, FP21, FP);
  mmoFP2.Lines.Add(FP21.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('MulU Self:');
  SetVars;
  FP2MulU(FP23, FP21, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Inv:');
  SetVars;
  FP2Inverse(FP21, FP21, FP);
  mmoFP2.Lines.Add(FP21.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Div:');
  SetVars;
  FP2Div(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('SetOne Inv:');
  SetVars;
  FP21.SetOne;
  FP2Inverse(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('SetU Inv:');
  SetVars;
  FP21.SetU;
  FP2Inverse(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');
end;

procedure TFormSM9.btnTestFp4Click(Sender: TObject);

  procedure SetVars;
  begin
    FP41.SetHex('BEC057C34CEC656C05F236D9399CD00C64319632885D200F964E4591DD7CA77',
      '55A10432B9095A12C106019C97FA1ED2A484D84BBB750BCF6A378C3F85BA9D09',
      '9EB75C7B34E0259A59385602BD2210B844E6B9F6396443EED06DBD701B48A26C',
      '76F63F8FB8272B173EAF93CB79E57444C816EF099B3FB11057977D1F3F50EB8');
    FP42.SetHex('1DD8569E8B7D7A53A362334330FF5B4E3BEEB180466CF7D268C157FF724C2DE7',
      '48619106BCF6F34107318044223FA5AE3EC74573829F9873E4F06B41D0210762',
      '79FDCB2D33F115EF5405C62B509BE15ADC14CC82ABBE6F89978ED0DE987377C6',
      '71A8D1FD3D68CD689B9ED04872690C41858D98065B2535E70D1A6A8F2547F07E');
  end;

begin
  mmoFP4.Lines.Clear;
  SetVars;

  mmoFP4.Lines.Add('Add:');
  FP4Add(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Add Self:');
  SetVars;
  FP4Add(FP43, FP41, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Sub:');
  SetVars;
  FP4Sub(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Negate:');
  SetVars;
  FP4Negate(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Mul:');
  SetVars;
  FP4Mul(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('MulV:');
  SetVars;
  FP4MulV(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Mul Self:');
  SetVars;
  FP4Mul(FP43, FP41, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('MulV Self:');
  SetVars;
  FP4MulV(FP43, FP41, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Inv:');
  SetVars;
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('SetOne Inv:');
  SetVars;
  FP41.SetOne;
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('SetU Inv:');
  SetVars;
  FP41.SetU;
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('SetV Inv:');
  SetVars;
  FP41.SetV;
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');
end;

procedure TFormSM9.btnTestFP12Click(Sender: TObject);
var
  E: TCnBigNumber;

  procedure SetVars;
  begin
    FP121.SetHex('3A4B2FDF33CFE01AAB98D17AEFC8D38B0508061C3117685839BD0DFDEB5783A8',
      '88A9043BDC9ABB43D241E7F62B0182D2C9F8DE39D77D154A57E126D871E7BCC0',
      'CD2A13D8E31BC262757BE16F34FAB3632BFDF4C5BE36E86799037305A73210F',
      'C407DE563B8393C590E35B4DF002BC9C79E3098558412A7D48BD62CA8723F3E',
      '66CBDEC2300EEBF35B0AB8637A93E0174A4182957B853B227C2A1612ADBAC39D',
      '481431CD7D6B54175B2B95E0036821ED9D757E383AE1A8D4B4EE95614271D328',
      '4B1D12F271AA058193ADC626FA8DD7C510678CD9F6A330C69652DEAF6287948D',
      'AB075F5760464947BDB5A644A1292776D5B6CFD735DF54C3B4F1948F2CC1AC7',
      'B16B0B0BD0C14D693F2BCB13C738669ED806E67B7B18D6F0BB62A2E8D94AEFF6',
      '7CA6249C1B6D5793ACA0549FFAF1DEBE372A3C72129599AFEAE445865D0F53CD',
      '16390AD0D7DD96377A198A2C0736278A453E39F006275B64D2027EA1570EAC51',
      'AB12E62FCDA3B9E62074960A8B036F564B6D45BAB4B183BE000827A3183F2878');

    FP122.SetHex('7D09B50545B09312F786B5E0486DE52AA79578B18961EDE71E8E2E0B8A3AEBB8',
      '280070ECB08554A8BB799271EB3214F2B582C69858E2771AEC594D72CD067F66',
      '18EF945E265CC1A1B77A2C60DB66A97B5365F939BD0DCF1CCE578A822CE2FD74',
      '9B0690C98E2C054AFC4DDB3CF9E6E45F2ED8DBBF1EDFD8EAE8950454DD2F5033',
      '540A59F0DB96DBA0C8EFC44D50DC3B55B0B1A421BA8964B76759F5FA2DB4604F',
      '2A17506AEB7CE73497DB53143DEDDA0429D1430453EE17D743A7D1425C19B79E',
      '1CA32FFEF87F1AEDA06046CA9D345445424F00300DAF0FD66D37B2620572DB62',
      '18C11CC3B61D709D7ED976E3DA5BA630BC49D17BDA470C3AACE50D4DB3E8AE5D',
      '4B91AC95D741011137C66858CA98DBBCD5744B77C51894E1FDA0C5B80959CFD3',
      '1D8E5CAE7D4463EE8F37B73037066455B284CCA92CA0255EEF8B0733B1D7B7F',
      '839EA9892563527B6E653CD9BA665D6284C7696D5AEDC884E469CB90E352A91C',
      'AD699CB305B6D98AAAC6FDB684E59C0D194DB0213214A7DF4BBED0240545A520');
  end;

begin
  mmoFP12.Lines.Clear;
  SetVars;

  mmoFP12.Lines.Add('Add:');
  SetVars;
  FP12Add(FP123, FP121, FP122, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Add Self:');
  SetVars;
  FP12Add(FP123, FP121, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Sub:');
  SetVars;
  FP12Sub(FP123, FP121, FP122, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Negate:');
  SetVars;
  FP12Negate(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Mul:');
  SetVars;
  FP12Mul(FP123, FP121, FP122, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Mul Self:');
  SetVars;
  FP12Mul(FP123, FP121, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Mul Self 3:');
  SetVars;
  FP12Mul3(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Inv:');
  SetVars;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetOne Inv:');
  SetVars;
  FP121.SetOne;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetU Inv:');
  SetVars;
  FP121.SetU;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetV Inv:');
  SetVars;
  FP121.SetV;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetW Inv:');
  SetVars;
  FP121.SetW;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetW^2 Inv:');
  SetVars;
  FP121.SetWSqr;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Power:');
  FP121.SetHex('4E378FB5561CD0668F906B731AC58FEE25738EDF09CADC7A29C0ABC0177AEA6D',
    '28B3404A61908F5D6198815C99AF1990C8AF38655930058C28C21BB539CE0000',
    '38BFFE40A22D529A0C66124B2C308DAC9229912656F62B4FACFCED408E02380F',
    'A01F2C8BEE81769609462C69C96AA923FD863E209D3CE26DD889B55E2E3873DB',
    '67E0E0C2EED7A6993DCE28FE9AA2EF56834307860839677F96685F2B44D0911F',
    '5A1AE172102EFD95DF7338DBC577C66D8D6C15E0A0158C7507228EFB078F42A6',
    '1604A3FCFA9783E667CE9FCB1062C2A5C6685C316DDA62DE0548BAA6BA30038B',
    '93634F44FA13AF76169F3CC8FBEA880ADAFF8475D5FD28A75DEB83C44362B439',
    'B3129A75D31D17194675A1BC56947920898FBF390A5BF5D931CE6CBB3340F66D',
    '4C744E69C4A2E1C8ED72F796D151A17CE2325B943260FC460B9F73CB57C9014B',
    '84B87422330D7936EABA1109FA5A7A7181EE16F2438B0AEB2F38FD5F7554E57A',
    'AAB9F06A4EEBA4323A7833DB202E4E35639D93FA3305AF73F0F071D7D284FCFB');

  E := TCnBigNumber.FromHex('033C8616B06704813203DFD00965022ED15975C662337AED648835DC4B1CBE');
  FP12Power(FP123, FP121, E, FP);
  E.Free;
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');
end;

procedure TFormSM9.btnAPClick(Sender: TObject);
var
  K: TCnBigNumber;

  procedure SetVars;
  begin
    FAP1.SetCoordinatesHex('3722755292130B08D2AAB97FD34EC120EE265948D19C17ABF9B7213BAF82D65B',
      '85AEF3D078640C98597B6027B441A01FF1DD2C190F5E93C454806C11D8806141',
      'A7CF28D519BE3DA65F3170153D278FF247EFBA98A71A08116215BBA5C999A7C7',
      '17509B092E845C1266BA0D262CBEE6ED0736A96FA347C8BD856DC76B84EBEB96');

    FAP2.SetCoordinatesHex('2A74F8561B91993205EB512576AD56221EA5963F3DA078240D55594FB051EA86',
      '513F149AB53E94BB3A0367C61FF87670E025DB30C57F84594E4BA4D7B3C656CF',
      '8E3D9EC4E63D5B9F83081FB97B715430C8BFC6F1A1321A89627B9A4E8961C7BD',
      '776DE41DB0511B8976D69C982DD4757D641487C68D13CBEE7069396C20CD3459');
  end;

begin
  mmoAP.Lines.Clear;
  K := TCnBigNumber.Create;
  SetVars;

  mmoAP.Lines.Add('Self:');
  mmoAP.Lines.Add(FAP1.ToString);
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Neg:');
  SetVars;
  FP2AffinePointNegate(FAP3, FAP1, FP);  // 说明 P 是对的
  mmoAP.Lines.Add(FAP3.ToString);
  mmoAP.Lines.Add('');

  if FP2AffinePointIsOnCurve(FAP1, FP) then
    mmoAP.Lines.Add('Is ON Curve')
  else
    mmoAP.Lines.Add('NOT On Curve');
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Double:');
  SetVars;
  FP2AffinePointDouble(FAP1, FAP1, FP);
  mmoAP.Lines.Add(FAP1.ToString);  // 2 倍的 FAP1
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Add:');
  SetVars;
  FP2AffinePointDouble(FAP3, FAP1, FP);
  FP2AffinePointAdd(FAP3, FAP1, FAP3, FP);
  mmoAP.Lines.Add(FAP3.ToString); // 3 倍的 FAP1
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Sub:');
  FP2AffinePointSub(FAP3, FAP3, FAP1, FP);
  mmoAP.Lines.Add(FAP3.ToString);
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Mul 10:');
  SetVars;

  K.SetWord(10);
  FP2AffinePointMul(FAP3, FAP1, K, FP);
  mmoAP.Lines.Add(FAP3.ToString);
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Mul K:');
  SetVars;
  K.SetHex('0130E78459D78545CB54C587E02CF480CE0B66340F319F348A1D5B1F2DC5F4');
  FP2AffinePointMul(FAP1, FAP1, K, FP);
  mmoAP.Lines.Add(FAP1.ToString);
  mmoAP.Lines.Add('');

  K.Free;
end;

procedure TFormSM9.btnRateTestClick(Sender: TObject);
var
  Pubs: TCnFP2AffinePoint;
  F: TCnFP12;
  E: TCnBigNumber;
begin
  F := TCnFP12.Create;

  Pubs := TCnFP2AffinePoint.Create;
  Pubs.SetCoordinatesHex('29DBA116152D1F786CE843ED24A3B573414D2177386A92DD8F14D65696EA5E32',
    '9F64080B3084F733E48AFF4B41B565011CE0711C5E392CFB0AB1B6791B94C408',
    '41E00A53DDA532DA1A7CE027B7A46F741006E85F5CDFF0730E75C05FB4E3216D',
    '69850938ABEA0112B57329F447E3A0CBAD3E2FDB1A77F335E89E1408D0EF1C25');

  SM9RatePairing(F, Pubs, nil);
  mmoRate.Lines.Clear;
  mmoRate.Lines.Add('R-ate:');
  mmoRate.Lines.Add(F.ToString);

  E := TCnBigNumber.FromHex('033C8616B06704813203DFD00965022ED15975C662337AED648835DC4B1CBE');

  FP12Power(F, F, E, FP);
  mmoRate.Lines.Add('');
  mmoRate.Lines.Add('Power:');
  mmoRate.Lines.Add(F.ToString);

  E.Free;
  Pubs.Free;
  F.Free;
end;

procedure TFormSM9.btnFP2PointMulClick(Sender: TObject);
var
  E: TCnBigNumber;
  P: TCnFP2Point;
  PA: TCnFP2AffinePoint;
begin
  E := TCnBigNumber.FromHex('607CD1361FBEA46FF5F89A0BA0C6D2462D080452AD2EA22FAF9FB48CAB47ECBD');
  P := TCnFP2Point.Create;
  PA := TCnFP2AffinePoint.Create;

  P.X.SetHex(CN_SM9_G2_P2X0, CN_SM9_G2_P2X1);
  P.Y.SetHex(CN_SM9_G2_P2Y0, CN_SM9_G2_P2Y1);

  FP2PointToFP2AffinePoint(PA, P);
  FP2AffinePointMul(PA, PA, E, FP);
  FP2AffinePointToFP2Point(P, PA, FP);

  mmoAP.Lines.Add('G2 Mul:');
  mmoAP.Lines.Add(P.ToString);

  PA.Free;
  P.Free;
  E.Free;
end;

procedure TFormSM9.btnTestHashClick(Sender: TObject);
var
  S: AnsiString;
  R: TCnBigNumber;
  SM9: TCnSM9;
begin
  S := 'Alice' + #1;

  R := TCnBigNumber.Create;
  SM9 := TCnSM9.Create;
  if CnSM9Hash1(R, @S[1], Length(S), SM9.Order) then
    ShowMessage(R.ToHex);

  SM9.Free;
  R.Free;
end;

procedure TFormSM9.btnTestHash2Click(Sender: TObject);
begin
  // No Sample?
end;

procedure TFormSM9.btnSM9GenMasterClick(Sender: TObject);
begin
  CnSM9KGCGenerateSignatureMasterKey(FSigMasterKey);
  mmoSig.Lines.Clear;
  mmoSig.Lines.Add('Master Private Key:');
  mmoSig.Lines.Add(FSigMasterKey.PrivateKey.ToString);
  mmoSig.Lines.Add('Master Public Key:');
  mmoSig.Lines.Add(FSigMasterKey.PublicKey.ToString);
end;

procedure TFormSM9.btnSM9GenUserClick(Sender: TObject);
begin
  CnSM9KGCGenerateSignatureUserKey(FSigMasterKey.PrivateKey, edtSigUserId.Text, FSigUserKey);
  mmoSig.Lines.Add('User Private Key:');
  mmoSig.Lines.Add(FSigUserKey.ToString);
end;

procedure TFormSM9.btnSM9SignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := edtSignData.Text;

  if CnSM9UserSignData(FSigMasterKey.PublicKey, FSigUserKey, @S[1], Length(S), FSig) then
  begin
    mmoSig.Lines.Add('Signature:');
    mmoSig.Lines.Add(FSig.ToString);
  end;
end;

procedure TFormSM9.btnSM9VerifyDataClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := edtSignData.Text;

  if CnSM9UserVerifyData(edtSigUserId.Text, @S[1], Length(S), FSig, FSigMasterKey.PublicKey) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormSM9.btnSM9SampleClick(Sender: TObject);
var
  AP: TCnFP2AffinePoint;
  SM9: TCnSM9;
  User, S: AnsiString;
begin
  mmoSig.Lines.Clear;
  SM9 := TCnSM9.Create;

  // 生成示例 Master Key
  FSigMasterKey.PrivateKey.SetHex('0130E78459D78545CB54C587E02CF480CE0B66340F319F348A1D5B1F2DC5F4');
  AP := TCnFP2AffinePoint.Create;
  FP2PointToFP2AffinePoint(AP, SM9.Generator2);

  FP2AffinePointMul(AP, AP, FSigMasterKey.PrivateKey, SM9.FiniteFieldSize);
  FP2AffinePointToFP2Point(FSigMasterKey.PublicKey, AP, SM9.FiniteFieldSize);

  // 打印 Master Key
  mmoSig.Lines.Add('Master Private Key:');
  mmoSig.Lines.Add(FSigMasterKey.PrivateKey.ToString);
  mmoSig.Lines.Add('Master Public Key:');
  mmoSig.Lines.Add(FSigMasterKey.PublicKey.ToString);

  // 生成示例 User Key
  User := 'Alice';
  CnSM9KGCGenerateSignatureUserKey(FSigMasterKey.PrivateKey, User, FSigUserKey);

  // 打印 User Key
  mmoSig.Lines.Add('User Private Key:');
  mmoSig.Lines.Add(FSigUserKey.ToString);

  S := 'Chinese IBS standard';

  // 签名，注意这里要通过验证得在 CnSM9UserSignData 中将随机数设为 033C8616B06704813203DFD00965022ED15975C662337AED648835DC4B1CBE
  if CnSM9UserSignData(FSigMasterKey.PublicKey, FSigUserKey, @S[1], Length(S), FSig) then
  begin
    mmoSig.Lines.Add('Signature:');
    mmoSig.Lines.Add(FSig.ToString);
  end;

  // 验证
  if CnSM9UserVerifyData(User, @S[1], Length(S), FSig, FSigMasterKey.PublicKey) then
    mmoSig.Lines.Add('Verify OK')
  else
    mmoSig.Lines.Add('Verify Failed');

  AP.Free;
  SM9.Free;
end;

procedure TFormSM9.btnKeyEncGenUserClick(Sender: TObject);
begin
  CnSM9KGCGenerateEncryptionMasterKey(FKeyEncMasterKey);
  mmoKeyEnc.Lines.Clear;
  mmoKeyEnc.Lines.Add('Master Private Key:');
  mmoKeyEnc.Lines.Add(FKeyEncMasterKey.PrivateKey.ToString);
  mmoKeyEnc.Lines.Add('Master Public Key:');
  mmoKeyEnc.Lines.Add(FKeyEncMasterKey.PublicKey.ToString);
end;

procedure TFormSM9.btnKeyEncGenMasterClick(Sender: TObject);
begin
  CnSM9KGCGenerateEncryptionUserKey(FKeyEncMasterKey.PrivateKey, edtDestUser.Text, FKeyEncUserKey);
  mmoKeyEnc.Lines.Add('User Private Key:');
  mmoKeyEnc.Lines.Add(FKeyEncUserKey.ToString);
end;

procedure TFormSM9.btnSM9KeyEncSendClick(Sender: TObject);
var
  KL: Integer;
begin
  KL := StrToInt(edtKeyEncLength.Text);
  if CnSM9UserSendKeyEncapsulation(edtDestUser.Text, KL, FKeyEncMasterKey.PublicKey, FKeyEnc) then
  begin
    mmoKeyEnc.Lines.Add('Key Encapsulation to Send:');
    mmoKeyEnc.Lines.Add(FKeyEnc.ToString);
  end;
end;

procedure TFormSM9.btnTestKeyEncClick(Sender: TObject);
var
  SM9: TCnSM9;
  User: AnsiString;
  S: TBytes;
begin
  mmoKeyEnc.Lines.Clear;
  SM9 := TCnSM9.Create;

  // 生成示例 Master Key
  FKeyEncMasterKey.PrivateKey.SetHex('01EDEE3778F441F8DEA3D9FA0ACC4E07EE36C93F9A08618AF4AD85CEDE1C22');
  FKeyEncMasterKey.PublicKey.X.SetHex('787ED7B8A51F3AB84E0A66003F32DA5C720B17ECA7137D39ABC66E3C80A892FF');
  FKeyEncMasterKey.PublicKey.Y.SetHex('769DE61791E5ADC4B9FF85A31354900B202871279A8C49DC3F220F644C57A7B1');

  // 打印 Master Key
  mmoKeyEnc.Lines.Clear;
  mmoKeyEnc.Lines.Add('Master Private Key:');
  mmoKeyEnc.Lines.Add(FKeyEncMasterKey.PrivateKey.ToString);
  mmoKeyEnc.Lines.Add('Master Public Key:');
  mmoKeyEnc.Lines.Add(FKeyEncMasterKey.PublicKey.ToString);

  // 生成示例 User Key
  User := 'Bob';
  CnSM9KGCGenerateEncryptionUserKey(FKeyEncMasterKey.PrivateKey, User, FKeyEncUserKey);

  // 打印 User Key
  mmoKeyEnc.Lines.Add('User Private Key:');
  mmoKeyEnc.Lines.Add(FKeyEncUserKey.ToString);

  // 注意这里要通过验证得在 CnSM9SendKeyEncapsulation 中将随机数设为 74015F8489C01EF4270456F9E6475BFB602BDE7F33FD482AB4E3684A6722
  if CnSM9UserSendKeyEncapsulation(User, 32, FKeyEncMasterKey.PublicKey, FKeyEnc) then
  begin
    mmoKeyEnc.Lines.Add('Key Encapsulation to Send:');
    mmoKeyEnc.Lines.Add(FKeyEnc.ToString);
  end;

  if CnSM9UserReceiveKeyEncapsulation(User, FKeyEncUserKey, 32, FKeyEnc.Code, S) then
  begin
    mmoKeyEnc.Lines.Add('Key Encapsulation Get:');
    mmoKeyEnc.Lines.Add(BytesToHex(S));
  end;

  SM9.Free;
end;

procedure TFormSM9.btnSM9KeyEncRecvClick(Sender: TObject);
var
  KL: Integer;
  S: TBytes;
begin
  KL := StrToInt(edtKeyEncLength.Text);
  if CnSM9UserReceiveKeyEncapsulation(edtDestUser.Text, FKeyEncUserKey, KL, FKeyEnc.Code, S) then
  begin
    mmoKeyEnc.Lines.Add('Key Encapsulation Get:');
    mmoKeyEnc.Lines.Add(BytesToHex(S));
  end;
end;

procedure TFormSM9.btnTestEncClick(Sender: TObject);
var
  SM9: TCnSM9;
  User, S: AnsiString;
  EnStream, DeStream: TMemoryStream;
begin
  mmoEnc.Lines.Clear;
  SM9 := TCnSM9.Create;

  // 生成示例 Master Key
  FKeyEncMasterKey.PrivateKey.SetHex('01EDEE3778F441F8DEA3D9FA0ACC4E07EE36C93F9A08618AF4AD85CEDE1C22');
  FKeyEncMasterKey.PublicKey.X.SetHex('787ED7B8A51F3AB84E0A66003F32DA5C720B17ECA7137D39ABC66E3C80A892FF');
  FKeyEncMasterKey.PublicKey.Y.SetHex('769DE61791E5ADC4B9FF85A31354900B202871279A8C49DC3F220F644C57A7B1');

  // 打印 Master Key
  mmoEnc.Lines.Clear;
  mmoEnc.Lines.Add('Master Private Key:');
  mmoEnc.Lines.Add(FKeyEncMasterKey.PrivateKey.ToString);
  mmoEnc.Lines.Add('Master Public Key:');
  mmoEnc.Lines.Add(FKeyEncMasterKey.PublicKey.ToString);

  // 生成示例 User Key
  User := 'Bob';
  CnSM9KGCGenerateEncryptionUserKey(FKeyEncMasterKey.PrivateKey, User, FKeyEncUserKey);

  // 打印 User Key
  mmoEnc.Lines.Add('User Private Key:');
  mmoEnc.Lines.Add(FKeyEncUserKey.ToString);

  S := 'Chinese IBE standard';
  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  if CnSM9UserEncryptData(User, FKeyEncMasterKey.PublicKey, @S[1], Length(S), 16, 32, EnStream) then
  begin
    mmoEnc.Lines.Add('SM9 with SM4 Encryption:');
    mmoEnc.Lines.Add(StreamToHex(EnStream));
  end;

  if CnSM9UserDecryptData(User, FKeyEncUserKey, EnStream.Memory, EnStream.Size, 32, DeStream) then
  begin
    mmoEnc.Lines.Add('SM9 with SM4 Decryption:');
    SetLength(S, DeStream.Size);
    DeStream.Position := 0;
    DeStream.Read(S[1], DeStream.Size);
    mmoEnc.Lines.Add(S);
  end;

  EnStream.Clear;
  DeStream.Clear;
  if CnSM9UserEncryptData(User, FKeyEncMasterKey.PublicKey, @S[1], Length(S), 16, 32, EnStream, semKDF) then
  begin
    mmoEnc.Lines.Add('SM9 with XOR Encryption:');
    mmoEnc.Lines.Add(StreamToHex(EnStream));
  end;

  if CnSM9UserDecryptData(User, FKeyEncUserKey, EnStream.Memory, EnStream.Size, 32, DeStream, semKDF) then
  begin
    mmoEnc.Lines.Add('SM9 with SM4 Decryption:');
    SetLength(S, DeStream.Size);
    DeStream.Position := 0;
    DeStream.Read(S[1], DeStream.Size);
    mmoEnc.Lines.Add(S);
  end;

  EnStream.Free;
  DeStream.Free;
  SM9.Free;
end;

procedure TFormSM9.btnKeyExchangeTestClick(Sender: TObject);
const
  KLEN = 16;
var
  AUser, BUser: AnsiString;
  SM9: TCnSM9;
  RA, RB: TCnEccPoint;
  RandA, RandB: TCnBigNumber;
  BG1, BG2, BG3: TCnFP12;
  KeyA, KeyB: TBytes;
  SB, SA: TCnSM3Digest;
begin
  mmoKeyExchange.Lines.Clear;
  SM9 := TCnSM9.Create;

  // 生成示例 Master Key
  FKeyExchangeMasterKey.PrivateKey.SetHex('02E65B0762D042F51F0D23542B13ED8CFA2E9A0E7206361E013A283905E31F');
  FKeyExchangeMasterKey.PublicKey.X.SetHex('9174542668E8F14AB273C0945C3690C66E5DD09678B86F734C4350567ED06283');
  FKeyExchangeMasterKey.PublicKey.Y.SetHex('54E598C6BF749A3DACC9FFFEDD9DB6866C50457CFC7AA2A4AD65C3168FF74210');

  // 打印 Master Key
  mmoKeyExchange.Lines.Clear;
  mmoKeyExchange.Lines.Add('Master Private Key:');
  mmoKeyExchange.Lines.Add(FKeyExchangeMasterKey.PrivateKey.ToString);
  mmoKeyExchange.Lines.Add('Master Public Key:');
  mmoKeyExchange.Lines.Add(FKeyExchangeMasterKey.PublicKey.ToString);

  // 生成示例 User Key
  AUser := 'Alice';
  BUser := 'Bob';
  CnSM9KGCGenerateKeyExchangeUserKey(FKeyExchangeMasterKey.PrivateKey, AUser, FKeyExchangeUserA);
  CnSM9KGCGenerateKeyExchangeUserKey(FKeyExchangeMasterKey.PrivateKey, BUser, FKeyExchangeUserB);

  // 打印 A User Key
  mmoKeyExchange.Lines.Add('A User Private Key:');
  mmoKeyExchange.Lines.Add(FKeyExchangeUserA.ToString);

  // 打印 B User Key
  mmoKeyExchange.Lines.Add('B User Private Key:');
  mmoKeyExchange.Lines.Add(FKeyExchangeUserB.ToString);

  RA := nil;
  RandA := nil;
  RB := nil;
  RandB := nil;

  BG1 := nil;
  BG2 := nil;
  BG3 := nil;

  try
    // 第一步，A 调用
    RA := TCnEccPoint.Create;
    RandA := TCnBigNumber.Create;

    if CnSM9UserKeyExchangeAStep1(BUser, KLEN, FKeyExchangeMasterKey.PublicKey, RA, RandA) then
    begin
      mmoKeyExchange.Lines.Add('A User Step 1: RA & RandA');
      mmoKeyExchange.Lines.Add(RA.ToString);
      mmoKeyExchange.Lines.Add(RandA.ToHex);
    end;

    // 第二步，B 调用，使用了第一步里传来的 RA
    RB := TCnEccPoint.Create;
    BG1 := TCnFP12.Create;
    BG2 := TCnFP12.Create;
    BG3 := TCnFP12.Create;
    if CnSM9UserKeyExchangeBStep1(AUser, BUser, KLEN, FKeyExchangeMasterKey.PublicKey,
      FKeyExchangeUserB, RA, RB, KeyB, SB, BG1, BG2, BG3) then
    begin
      mmoKeyExchange.Lines.Add('B User Step 1: RB & SB & Key!');
      mmoKeyExchange.Lines.Add(RB.ToString);
      mmoKeyExchange.Lines.Add(DataToHex(@SB[0], SizeOf(TCnSM3Digest)));

      mmoKeyExchange.Lines.Add('BG1:');
      mmoKeyExchange.Lines.Add(BG1.ToString);
      mmoKeyExchange.Lines.Add('BG2:');
      mmoKeyExchange.Lines.Add(BG2.ToString);
      mmoKeyExchange.Lines.Add('BG3:');
      mmoKeyExchange.Lines.Add(BG3.ToString);

      mmoKeyExchange.Lines.Add('B Key:');
      mmoKeyExchange.Lines.Add(DataToHex(@KeyB[0], Length(KeyB)));
    end;

    // 第三步，A 调用，使用了第二步里传过来的 RB 和 SB 以及第一步自身的 RandA
    if CnSM9UserKeyExchangeAStep2(AUser, BUser, KLEN, FKeyExchangeMasterKey.PublicKey,
      FKeyExchangeUserA, RandA, RA, RB, SB, KeyA, SA) then
    begin
      mmoKeyExchange.Lines.Add('A User Step 2: Key! & SA');
      mmoKeyExchange.Lines.Add('A Key:');
      mmoKeyExchange.Lines.Add(DataToHex(@KeyA[0], Length(KeyA)));
      mmoKeyExchange.Lines.Add(DataToHex(@SA[0], SizeOf(TCnSM3Digest)));
    end;

    // 第四步，B 调用，使用了第一步里传过来的 RA 以及第二步自身的 BG1、BG2、BG3、RB 以及第三步里传过来的 SA
    if CnSM9UserKeyExchangeBStep2(AUser, BUser, RA, RB, SA, BG1, BG2, BG3) then
    begin
      mmoKeyExchange.Lines.Add('B User Step 2: Check SA OK');
    end;

    if CompareBytes(KeyA, KeyB) then
      mmoKeyExchange.Lines.Add('KeyA = KeyB. Exchange OK.');
  finally
    BG3.Free;
    BG2.Free;
    BG1.Free;
    RandB.Free;
    RB.Free;
    RandA.Free;
    RA.Free;
    SM9.Free;
  end;
end;

procedure TFormSM9.btnRateTimeClick(Sender: TObject);
var
  Pubs: TCnFP2AffinePoint;
  F: TCnFP12;
  I: Integer;
  T: Cardinal;
begin
  F := TCnFP12.Create;

  Pubs := TCnFP2AffinePoint.Create;
  Pubs.SetCoordinatesHex('29DBA116152D1F786CE843ED24A3B573414D2177386A92DD8F14D65696EA5E32',
    '9F64080B3084F733E48AFF4B41B565011CE0711C5E392CFB0AB1B6791B94C408',
    '41E00A53DDA532DA1A7CE027B7A46F741006E85F5CDFF0730E75C05FB4E3216D',
    '69850938ABEA0112B57329F447E3A0CBAD3E2FDB1A77F335E89E1408D0EF1C25');

  T := CnGetTickCount;
  for I := 1 to 10 do // 10 次要 6 秒多钟
    SM9RatePairing(F, Pubs, nil);
  T := CnGetTickCount - T;
  ShowMessage(IntToStr(T));

  Pubs.Free;
  F.Free;
end;

end.
