unit UnitSM9;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnBigNumber, CnSM9;

type
  TFormSM9 = class(TForm)
    pgcSM9: TPageControl;
    tsFP2: TTabSheet;
    grpFP2: TGroupBox;
    btnTestFP2: TButton;
    mmoFP2: TMemo;
    tsFP4: TTabSheet;
    grpFP4: TGroupBox;
    btnTestFp4: TButton;
    mmoFP4: TMemo;
    tsFP12: TTabSheet;
    grpFP12: TGroupBox;
    btnTestFP12: TButton;
    mmoFP12: TMemo;
    tsAffinePoint: TTabSheet;
    grpAP: TGroupBox;
    btnAP: TButton;
    mmoAP: TMemo;
    tsRate: TTabSheet;
    grpRate: TGroupBox;
    btnRateTest: TButton;
    mmoRate: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestFP2Click(Sender: TObject);
    procedure btnTestFp4Click(Sender: TObject);
    procedure btnTestFP12Click(Sender: TObject);
    procedure btnAPClick(Sender: TObject);
    procedure btnRateTestClick(Sender: TObject);
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
    FAP1: TCnAffinePoint;
    FAP2: TCnAffinePoint;
    FAP3: TCnAffinePoint;
  public
    { Public declarations }
  end;

var
  FormSM9: TFormSM9;

implementation

{$R *.DFM}

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

  FAP1 := TCnAffinePoint.Create;
  FAP2 := TCnAffinePoint.Create;
  FAP3 := TCnAffinePoint.Create;
end;

procedure TFormSM9.FormDestroy(Sender: TObject);
begin
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

  mmoFP12.Lines.Add('Power:');
  SetVars;
  E := TCnBigNumber.FromHex('033C8616B06704813203DFD00965022ED15975C662337AED648835DC4B1CBE');
  FP12Power(FP123, FP121, E, FP);
  E.Free;
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
  AffinePointNegate(FAP3, FAP1, FP);  // 说明 P 是对的
  mmoAP.Lines.Add(FAP3.ToString);
  mmoAP.Lines.Add('');

  if AffinePointIsOnCurve(FAP1, FP) then
    mmoAP.Lines.Add('Is ON Curve')
  else
    mmoAP.Lines.Add('NOT On Curve');
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Double:');
  SetVars;
  AffinePointDouble(FAP1, FAP1, FP);
  mmoAP.Lines.Add(FAP1.ToString);  // 2 倍的 FAP1
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Add:');
  SetVars;
  AffinePointDouble(FAP3, FAP1, FP);
  AffinePointAdd(FAP3, FAP1, FAP3, FP);
  mmoAP.Lines.Add(FAP3.ToString); // 3 倍的 FAP1
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Sub:');
  AffinePointSub(FAP3, FAP3, FAP1, FP);
  mmoAP.Lines.Add(FAP3.ToString);
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Mul 10:');
  SetVars;

  K.SetWord(10);
  AffinePointMul(FAP3, FAP1, K, FP);
  mmoAP.Lines.Add(FAP3.ToString);
  mmoAP.Lines.Add('');

  mmoAP.Lines.Add('Mul K:');
  SetVars;
  K.SetHex('0130E78459D78545CB54C587E02CF480CE0B66340F319F348A1D5B1F2DC5F4');
  AffinePointMul(FAP1, FAP1, K, FP);
  mmoAP.Lines.Add(FAP1.ToString);
  mmoAP.Lines.Add('');

  K.Free;
end;

procedure TFormSM9.btnRateTestClick(Sender: TObject);
var
  Pubs: TCnAffinePoint;
  F: TCnFP12;
begin
  F := TCnFP12.Create;

  Pubs := TCnAffinePoint.Create;
  Pubs.SetCoordinatesHex('29DBA116152D1F786CE843ED24A3B573414D2177386A92DD8F14D65696EA5E32',
    '9F64080B3084F733E48AFF4B41B565011CE0711C5E392CFB0AB1B6791B94C408',
    '41E00A53DDA532DA1A7CE027B7A46F741006E85F5CDFF0730E75C05FB4E3216D',
    '69850938ABEA0112B57329F447E3A0CBAD3E2FDB1A77F335E89E1408D0EF1C25');

  if SM9RatePairing(F, Pubs, nil) then
  begin
    mmoRate.Lines.Clear;
    mmoRate.Lines.Add(F.ToString);
  end;

  Pubs.Free;
  F.Free;
end;

end.
