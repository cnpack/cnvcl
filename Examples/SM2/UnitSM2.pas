unit UnitSM2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CnSM2, CnECC, StdCtrls, CnSM3;

type
  TFormSM2 = class(TForm)
    pgcSm2: TPageControl;
    tsEncDec: TTabSheet;
    tsSignVerify: TTabSheet;
    grpSm2Enc: TGroupBox;
    btnSm2Example1: TButton;
    grpSm2SignVerify: TGroupBox;
    btnSm2SignVerify: TButton;
    tsKeyExchange: TTabSheet;
    grpSM2KeyExchange: TGroupBox;
    btnSM2KeyExchange: TButton;
    procedure btnSm2Example1Click(Sender: TObject);
    procedure btnSm2SignVerifyClick(Sender: TObject);
    procedure btnSM2KeyExchangeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSM2: TFormSM2;

implementation

{$R *.DFM}

const
  MSG1: AnsiString = 'encryption standard';
  MSG2: AnsiString = 'message digest';

  USER_A: AnsiString = 'ALICE123@YAHOO.COM';
  USER_B: AnsiString = 'BILL456@YAHOO.COM';

procedure TestFp192CryptExample;
var
  S: AnsiString;
  Sm2: TCnSM2;
  PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey;
  EnStream, DeStream: TMemoryStream;
begin
  Sm2 := TCnSM2.Create(ctSM2Example192);
  PrivateKey := TCnEccPrivateKey.Create;
  PublicKey := TCnEccPublicKey.Create;

  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  PublicKey.X.SetHex('79F0A9547AC6D100531508B30D30A56536BCFC8149F4AF4A');
  PublicKey.Y.SetHex('AE38F2D8890838DF9C19935A65A8BCC8994BC7924672F912');
  PrivateKey.SetHex('58892B807074F53FBF67288A1DFAA1AC313455FE60355AFD');

  // 里头的随机数 K 要 384F3035 3073AEEC E7A16543 30A96204 D37982A3 E15B2CB5
  if CnSM2EncryptData(@MSG1[1], Length(MSG1), EnStream, PublicKey, Sm2) then
  begin
    ShowMessage('Encrypt OK');
    if CnSM2DecryptData(EnStream.Memory, EnStream.Size, DeStream, PrivateKey, Sm2) then
    begin
      SetLength(S, DeStream.Size);
      DeStream.Position := 0;
      DeStream.Read(S[1], DeStream.Size);
      ShowMessage('Decrypt OK: ' + S);
    end;
  end;

  PrivateKey.Free;
  PublicKey.Free;
  EnStream.Free;
  DeStream.Free;
  Sm2.Free;
end;

procedure TestFp256SignExample;
var
  Sm2: TCnSM2;
  PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey;
  Sig: TCnSM2Signature;
begin
  Sm2 := TCnSM2.Create(ctSM2Example256);
  PrivateKey := TCnEccPrivateKey.Create;
  PublicKey := TCnEccPublicKey.Create;
  Sig := TCnSM2Signature.Create;

  PublicKey.X.SetHex('0AE4C7798AA0F119471BEE11825BE46202BB79E2A5844495E97C04FF4DF2548A');
  PublicKey.Y.SetHex('7C0240F88F1CD4E16352A73C17B7F16F07353E53A176D684A9FE0C6BB798E857');
  PrivateKey.SetHex('128B2FA8BD433C6C068C8D803DFF79792A519A55171B1B650C23661D15897263');

  // 里头的随机数 K 要 6CB28D99385C175C94F94E934817663FC176D925DD72B727260DBAAE1FB2F96F
  if CnSM2SignData(USER_A, @MSG2[1], Length(MSG2), Sig, PrivateKey, PublicKey, Sm2) then
  begin
    ShowMessage('Sig OK: ' + Sig.X.ToHex + ', ' + Sig.Y.ToHex);
    if CnSM2VerifyData(USER_A, @MSG2[1], Length(MSG2), Sig, PublicKey, Sm2) then
      ShowMessage('Verify OK.');
  end;

  Sig.Free;
  PrivateKey.Free;
  PublicKey.Free;
  Sm2.Free;
end;

procedure TestSm2KeyExchangeExample;
const
  KEY_LENGTH = 128 div 8;
var
  Sm2: TCnSM2;
  APrivateKey, BPrivateKey: TCnEccPrivateKey;
  APublicKey, BPublicKey: TCnEccPublicKey;
  OutRA, OutRB: TCnEccPoint;
  KA, KB: AnsiString;
  OpSA, OpSB: TSM3Digest;
begin
  Sm2 := TCnSM2.Create(ctSM2Example256);
  APrivateKey := TCnEccPrivateKey.Create;
  APublicKey := TCnEccPublicKey.Create;
  BPrivateKey := TCnEccPrivateKey.Create;
  BPublicKey := TCnEccPublicKey.Create;

  APrivateKey.SetHex('6FCBA2EF9AE0AB902BC3BDE3FF915D44BA4CC78F88E2F8E7F8996D3B8CCEEDEE');
  APublicKey.X.SetHex('3099093BF3C137D8FCBBCDF4A2AE50F3B0F216C3122D79425FE03A45DBFE1655');
  APublicKey.Y.SetHex('3DF79E8DAC1CF0ECBAA2F2B49D51A4B387F2EFAF482339086A27A8E05BAED98B');

  BPrivateKey.SetHex('5E35D7D3F3C54DBAC72E61819E730B019A84208CA3A35E4C2E353DFCCB2A3B53');
  BPublicKey.X.SetHex('245493D446C38D8CC0F118374690E7DF633A8A4BFB3329B5ECE604B2B4F37F43');
  BPublicKey.Y.SetHex('53C0869F4B9E17773DE68FEC45E14904E0DEA45BF6CECF9918C85EA047C60A4C');

  OutRA := TCnEccPoint.Create;
  OutRB := TCnEccPoint.Create;

  if not CnSM2KeyExchangeAStep1(USER_A, USER_B, KEY_LENGTH, APrivateKey, APublicKey,
    BPublicKey, OutRA, Sm2) then
    Exit;

  if not CnSM2KeyExchangeBStep1(USER_A, USER_B, KEY_LENGTH, BPrivateKey,
    APublicKey, BPublicKey, OutRA, KB, OutRB, OpSB, Sm2) then
    Exit;

  if not CnSM2KeyExchangeAStep2(USER_A, USER_B, KEY_LENGTH, APrivateKey, APublicKey,
    BPublicKey, OutRB, KB, OpSA, Sm2) then
    Exit;

  if CnSM2KeyExchangeBStep2(USER_A, USER_B, KEY_LENGTH, BPrivateKey, APublicKey,
    BPublicKey, OpSA, Sm2) then
    ShowMessage('Key Exchange OK: ' + KA + ' ' + KB);

  OutRA.Free;
  OutRB.Free;

  APublicKey.Free;
  APrivateKey.Free;
  BPublicKey.Free;
  BPrivateKey.Free;
  Sm2.Free;
end;

procedure TFormSM2.btnSm2Example1Click(Sender: TObject);
begin
  TestFp192CryptExample;
end;

procedure TFormSM2.btnSm2SignVerifyClick(Sender: TObject);
begin
  TestFp256SignExample;
end;

procedure TFormSM2.btnSM2KeyExchangeClick(Sender: TObject);
begin
  TestSm2KeyExchangeExample;
end;

end.
