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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTestFP2Click(Sender: TObject);
    procedure btnTestFp4Click(Sender: TObject);
    procedure btnTestFP12Click(Sender: TObject);
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
end;

procedure TFormSM9.FormDestroy(Sender: TObject);
begin
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
begin
  mmoFP2.Lines.Clear;

  FP21.SetHex('5F25CE2083FC970A6B9FDCD819FB1966D300AF2AFD58D480C59E02B320852183',
    '9ACDDFEF770BCDCE452D72461F9D1482A8EFF7662E1D591C70A7CE35F2F5710C');
  FP22.SetHex('7114F0B7F50EBB85C124558F76F10BD277F71C27384DEB67F229E582BEFDE3EE',
    'AA2714A30D7B8AE08B987FAE8818881FB1952A1F53CDA30A35C72841B174D7D');

  mmoFP2.Lines.Add('Add:');
  FP2Add(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Add Self:');
  FP2Add(FP23, FP21, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Mul 3:');
  FP2Mul3(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Sub:');
  FP2Sub(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Negate:');
  FP2Negate(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Mul:');
  FP2Mul(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('MulU:');
  FP2MulU(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Mul Self:');
  FP2Mul(FP23, FP21, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('MulU Self:');
  FP2MulU(FP23, FP21, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Inv:');
  FP2Inverse(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('Div:');
  FP2Div(FP23, FP21, FP22, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('SetOne Inv:');
  FP21.SetOne;
  FP2Inverse(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');

  mmoFP2.Lines.Add('SetU Inv:');
  FP21.SetU;
  FP2Inverse(FP23, FP21, FP);
  mmoFP2.Lines.Add(FP23.ToString);
  mmoFP2.Lines.Add('');
end;

procedure TFormSM9.btnTestFp4Click(Sender: TObject);
begin
  mmoFP4.Lines.Clear;

  FP41.SetHex('BEC057C34CEC656C05F236D9399CD00C64319632885D200F964E4591DD7CA77',
    '55A10432B9095A12C106019C97FA1ED2A484D84BBB750BCF6A378C3F85BA9D09',
    '9EB75C7B34E0259A59385602BD2210B844E6B9F6396443EED06DBD701B48A26C',
    '76F63F8FB8272B173EAF93CB79E57444C816EF099B3FB11057977D1F3F50EB8');
  FP42.SetHex('1DD8569E8B7D7A53A362334330FF5B4E3BEEB180466CF7D268C157FF724C2DE7',
    '48619106BCF6F34107318044223FA5AE3EC74573829F9873E4F06B41D0210762',
    '79FDCB2D33F115EF5405C62B509BE15ADC14CC82ABBE6F89978ED0DE987377C6',
    '71A8D1FD3D68CD689B9ED04872690C41858D98065B2535E70D1A6A8F2547F07E');

  mmoFP4.Lines.Add('Add:');
  FP4Add(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Add Self:');
  FP4Add(FP43, FP41, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Sub:');
  FP4Sub(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Negate:');
  FP4Negate(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Mul:');
  FP4Mul(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('MulV:');
  FP4MulV(FP43, FP41, FP42, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Mul Self:');
  FP4Mul(FP43, FP41, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('MulV Self:');
  FP4MulV(FP43, FP41, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('Inv:');
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('SetOne Inv:');
  FP41.SetOne;
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('SetU Inv:');
  FP41.SetU;
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');

  mmoFP4.Lines.Add('SetV Inv:');
  FP41.SetV;
  FP4Inverse(FP43, FP41, FP);
  mmoFP4.Lines.Add(FP43.ToString);
  mmoFP4.Lines.Add('');
end;

procedure TFormSM9.btnTestFP12Click(Sender: TObject);
var
  E: TCnBigNumber;
begin
  mmoFP12.Lines.Clear;

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

  mmoFP12.Lines.Add('Add:');
  FP12Add(FP123, FP121, FP122, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Add Self:');
  FP12Add(FP123, FP121, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Sub:');
  FP12Sub(FP123, FP121, FP122, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Negate:');
  FP12Negate(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Mul:');
  FP12Mul(FP123, FP121, FP122, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Mul Self:');
  FP12Mul(FP123, FP121, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Mul Self 3:');
  FP12Mul3(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Power:');
  E := TCnBigNumber.FromHex('033C8616B06704813203DFD00965022ED15975C662337AED648835DC4B1CBE');
  FP12Power(FP123, FP121, E, FP);
  E.Free;
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('Inv:');
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetOne Inv:');
  FP121.SetOne;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetU Inv:');
  FP121.SetU;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetV Inv:');
  FP121.SetV;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetW Inv:');
  FP121.SetW;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');

  mmoFP12.Lines.Add('SetW^2 Inv:');
  FP121.SetWSqr;
  FP12Inverse(FP123, FP121, FP);
  mmoFP12.Lines.Add(FP123.ToString);
  mmoFP12.Lines.Add('');
end;

end.
