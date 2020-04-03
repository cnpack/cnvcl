unit UnitSM2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CnSM2, CnECC;

type
  TFormSM2 = class(TForm)
    pgcSm2: TPageControl;
    tsEncDec: TTabSheet;
    tsSignVerify: TTabSheet;
    procedure FormCreate(Sender: TObject);
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
  MSG: AnsiString = 'encryption standard';

procedure TestFp192Example;
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
  if CnSM2EncryptData(@MSG[1], Length(MSG), EnStream, PublicKey, Sm2) then
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
  Sm2.Free;
end;

procedure TFormSM2.FormCreate(Sender: TObject);
begin
  TestFp192Example;
end;

end.
