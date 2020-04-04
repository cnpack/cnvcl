unit UnitSM2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CnSM2, CnECC, StdCtrls;

type
  TFormSM2 = class(TForm)
    pgcSm2: TPageControl;
    tsEncDec: TTabSheet;
    tsSignVerify: TTabSheet;
    grpSm2Enc: TGroupBox;
    btnSm2Example1: TButton;
    grpSm2SignVerify: TGroupBox;
    btnSm2SignVerify: TButton;
    procedure btnSm2Example1Click(Sender: TObject);
    procedure btnSm2SignVerifyClick(Sender: TObject);
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
  if CnSM2SignData('ALICE123@YAHOO.COM', @MSG2[1], Length(MSG2), Sig, PrivateKey, PublicKey, Sm2) then
  begin
    ShowMessage('Sig OK: ' + Sig.X.ToHex + ', ' + Sig.Y.ToHex);
    if CnSM2VerifyData('ALICE123@YAHOO.COM', @MSG2[1], Length(MSG2), Sig, PublicKey, Sm2) then
      ShowMessage('Verify OK.');
  end;

  Sig.Free;
  PrivateKey.Free;
  PublicKey.Free;
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

end.
