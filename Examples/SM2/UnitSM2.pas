unit UnitSM2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, CnSM2, CnECC, CnSM3, CnBigNumber, CnBase64, CnNative;

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
    bvl1: TBevel;
    lblSM2PublicKey: TLabel;
    lblSM2PrivateKey: TLabel;
    edtSM2PublicKey: TEdit;
    edtSM2PrivateKey: TEdit;
    bvl2: TBevel;
    lblSM2Text: TLabel;
    edtSM2Text: TEdit;
    btnSM2Encrypt: TButton;
    btnGenerateKey: TButton;
    mmoSM2Result: TMemo;
    btnSM2Decrypt: TButton;
    bvl3: TBevel;
    bvl4: TBevel;
    lblUserId: TLabel;
    edtSM2UserId: TEdit;
    lblSM2FileSign: TLabel;
    edtSM2FileSign: TEdit;
    btnSignBrowse: TButton;
    dlgOpen1: TOpenDialog;
    mmoSignResult: TMemo;
    btnSM2Verify: TButton;
    btnSM2SignFile: TButton;
    bvl5: TBevel;
    btnSignFile: TButton;
    btnVerifyFile: TButton;
    bvl6: TBevel;
    lblAId: TLabel;
    edtSM2AUserId: TEdit;
    lblBUserId: TLabel;
    edtSM2BUserId: TEdit;
    edtSM2BPrivateKey: TEdit;
    edtSM2BPublicKey: TEdit;
    lblBSM2PublicKey: TLabel;
    lblBSm2PrivateKey: TLabel;
    lbl1: TLabel;
    btnSM2ABKeyExchange: TButton;
    btnLoadSM2Key: TButton;
    btnLoadSM2BKey: TButton;
    chkPrefixByte: TCheckBox;
    btnVerifySm2Key: TButton;
    tsSchnorr: TTabSheet;
    grpSchnorr: TGroupBox;
    btnSchnorrProve: TButton;
    lblSchnorrProveCheckR: TLabel;
    edtSchnorrProveCheckR: TEdit;
    lblSchnorrProveCheckZ: TLabel;
    edtSchnorrProveCheckZ: TEdit;
    btnSchnorrCheck: TButton;
    btnSM2EncryptFile: TButton;
    btnSM2DecryptFile: TButton;
    dlgSave1: TSaveDialog;
    tsCollaborative: TTabSheet;
    grpCollaborative: TGroupBox;
    lblSM2PrivateKeyA: TLabel;
    lblSM2PrivateKeyB: TLabel;
    edtSM2PrivateKeyA: TEdit;
    edtSM2PrivateKeyB: TEdit;
    lblSM2PublicKeyAB: TLabel;
    edtSM2PublicKeyAB: TEdit;
    btnSM2CollaborativeGen: TButton;
    lblCollId: TLabel;
    edtSM2CollUserId: TEdit;
    lblSM2CollFileSign: TLabel;
    edtSM2CollFileSign: TEdit;
    btnCollSignBrowse: TButton;
    btnSM2CollSignFile: TButton;
    btnSM2CollVerify: TButton;
    mmoSM2CollResult: TMemo;
    bvl7: TBevel;
    bvl8: TBevel;
    lblSM2CollText: TLabel;
    chkCollPrefixByte: TCheckBox;
    edtSM2CollText: TEdit;
    btnSM2CollEncrypt: TButton;
    btnSM2CollDecrypt: TButton;
    rbCollC1C2C3: TRadioButton;
    rbCollC1C3C2: TRadioButton;
    rbC1C3C2: TRadioButton;
    rbC1C2C3: TRadioButton;
    btnSm2SignTime: TButton;
    btnSM2VerifyTime: TButton;
    tsCollaborative3: TTabSheet;
    tsTest: TTabSheet;
    btnSM2CreateMatrix: TButton;
    grpCollaborative3: TGroupBox;
    lblSM2Private3KeyA: TLabel;
    lblSM2Private3KeyB: TLabel;
    lblSM2PublicKeyABC: TLabel;
    lblColl3Id1: TLabel;
    lblSM2Coll3FileSign: TLabel;
    bvl9: TBevel;
    bvl10: TBevel;
    lblSM2Coll3Text: TLabel;
    edtSM2Private3KeyA: TEdit;
    edtSM2Private3KeyB: TEdit;
    edtSM2PublicKeyABC: TEdit;
    btnSM2Collaborative3Gen: TButton;
    edtSM2Coll3UserId: TEdit;
    edtSM2Coll3FileSign: TEdit;
    btnColl3SignBrowse: TButton;
    btnSM2Coll3SignFile: TButton;
    btnSM2Coll3Verify: TButton;
    mmoSM2Coll3Result: TMemo;
    chkCollPrefixByte3: TCheckBox;
    edtSM2Coll3Text: TEdit;
    btnSM2Coll3Encrypt: TButton;
    btnSM2Coll3Decrypt: TButton;
    rbColl3C1C2C3: TRadioButton;
    rbColl3C1C3C2: TRadioButton;
    edtSM2Private3KeyC: TEdit;
    lblSM2Private3KeyC: TLabel;
    chkEncDecTBytes: TCheckBox;
    chkSignTBytes: TCheckBox;
    btnCalcPubFromPriv: TButton;
    chkSignBase64: TCheckBox;
    procedure btnSm2Example1Click(Sender: TObject);
    procedure btnSm2SignVerifyClick(Sender: TObject);
    procedure btnSM2KeyExchangeClick(Sender: TObject);
    procedure btnSM2EncryptClick(Sender: TObject);
    procedure btnGenerateKeyClick(Sender: TObject);
    procedure btnSM2DecryptClick(Sender: TObject);
    procedure btnSignBrowseClick(Sender: TObject);
    procedure btnSM2SignFileClick(Sender: TObject);
    procedure btnSM2VerifyClick(Sender: TObject);
    procedure btnSignFileClick(Sender: TObject);
    procedure btnVerifyFileClick(Sender: TObject);
    procedure btnSM2ABKeyExchangeClick(Sender: TObject);
    procedure btnLoadSM2KeyClick(Sender: TObject);
    procedure btnLoadSM2BKeyClick(Sender: TObject);
    procedure btnVerifySm2KeyClick(Sender: TObject);
    procedure btnSchnorrProveClick(Sender: TObject);
    procedure btnSchnorrCheckClick(Sender: TObject);
    procedure btnSM2EncryptFileClick(Sender: TObject);
    procedure btnSM2DecryptFileClick(Sender: TObject);
    procedure btnSM2CollaborativeGenClick(Sender: TObject);
    procedure btnCollSignBrowseClick(Sender: TObject);
    procedure btnSM2CollVerifyClick(Sender: TObject);
    procedure btnSM2CollSignFileClick(Sender: TObject);
    procedure btnSM2CollEncryptFileClick(Sender: TObject);
    procedure btnSM2CollEncryptClick(Sender: TObject);
    procedure btnSM2CollDecryptClick(Sender: TObject);
    procedure btnSm2SignTimeClick(Sender: TObject);
    procedure btnSM2VerifyTimeClick(Sender: TObject);
    procedure btnSM2CreateMatrixClick(Sender: TObject);
    procedure btnColl3SignBrowseClick(Sender: TObject);
    procedure btnSM2Collaborative3GenClick(Sender: TObject);
    procedure btnSM2Coll3SignFileClick(Sender: TObject);
    procedure btnSM2Coll3VerifyClick(Sender: TObject);
    procedure btnSM2Coll3EncryptClick(Sender: TObject);
    procedure btnSM2Coll3DecryptClick(Sender: TObject);
    procedure btnCalcPubFromPrivClick(Sender: TObject);
  private
    function CheckPublicKeyStr(Edit: TEdit): Boolean;
    function CheckPrivateKeyStr(Edit: TEdit): Boolean;
  public

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

function HexToInt(const Hex: AnsiString): Integer;
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
    else
      raise Exception.Create('Error: not a Hex String');
  end;
  Result := Res;
end;

function MyStreamFromHex(const Hex: string; Stream: TStream): Integer;
var
  S: string;
  I: Integer;
  C: AnsiChar;
begin
  Result := 0;
  for I := 0 to Length(Hex) div 2 - 1 do
  begin
    S := Copy(Hex, I * 2 + 1, 2);
    C := AnsiChar(HexToInt(S));
    Stream.Write(C, 1);
    Inc(Result);
  end;
end;

procedure TestFp192CryptExample;
var
  S: AnsiString;
  SM2: TCnSM2;
  PrivateKey: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
  EnStream, DeStream: TMemoryStream;
begin
  SM2 := TCnSM2.Create(ctSM2Example192);
  PrivateKey := TCnSM2PrivateKey.Create;
  PublicKey := TCnSM2PublicKey.Create;

  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  PublicKey.X.SetHex('79F0A9547AC6D100531508B30D30A56536BCFC8149F4AF4A');
  PublicKey.Y.SetHex('AE38F2D8890838DF9C19935A65A8BCC8994BC7924672F912');
  PrivateKey.SetHex('58892B807074F53FBF67288A1DFAA1AC313455FE60355AFD');

  // 里头的随机数 K 要 384F3035 3073AEEC E7A16543 30A96204 D37982A3 E15B2CB5
  if CnSM2EncryptData(@MSG1[1], Length(MSG1), EnStream, PublicKey, SM2) then
  begin
    ShowMessage('Encrypt OK');
    if CnSM2DecryptData(EnStream.Memory, EnStream.Size, DeStream, PrivateKey, SM2) then
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
  SM2.Free;
end;

procedure TestFp256SignExample;
var
  SM2: TCnSM2;
  PrivateKey: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
  Sig: TCnSM2Signature;
begin
  SM2 := TCnSM2.Create(ctSM2Example256); // 注意这里不是标准 SM2 曲线参数，内部不能用预计算的加速算法
  PrivateKey := TCnSM2PrivateKey.Create;
  PublicKey := TCnSM2PublicKey.Create;
  Sig := TCnSM2Signature.Create;

  PublicKey.X.SetHex('0AE4C7798AA0F119471BEE11825BE46202BB79E2A5844495E97C04FF4DF2548A');
  PublicKey.Y.SetHex('7C0240F88F1CD4E16352A73C17B7F16F07353E53A176D684A9FE0C6BB798E857');
  PrivateKey.SetHex('128B2FA8BD433C6C068C8D803DFF79792A519A55171B1B650C23661D15897263');

  // 里头的随机数 K 要 6CB28D99385C175C94F94E934817663FC176D925DD72B727260DBAAE1FB2F96F
  if CnSM2SignData(USER_A, @MSG2[1], Length(MSG2), Sig, PrivateKey, PublicKey, SM2) then
  begin
    ShowMessage('Sig OK: ' + Sig.R.ToHex + ', ' + Sig.S.ToHex);
    if CnSM2VerifyData(USER_A, @MSG2[1], Length(MSG2), Sig, PublicKey, SM2) then
      ShowMessage('Verify OK.');
  end;

  Sig.Free;
  PrivateKey.Free;
  PublicKey.Free;
  SM2.Free;
end;

procedure TestSm2KeyExchangeExample;
const
  KEY_LENGTH = 128 div 8;
var
  SM2: TCnSM2;
  APrivateKey, BPrivateKey: TCnSM2PrivateKey;
  APublicKey, BPublicKey: TCnSM2PublicKey;
  RandA, RandB: TCnBigNumber;
  OutRA, OutRB: TCnEccPoint;
  KA, KB: AnsiString;
  OpSA, OpSB, OpS2: TSM3Digest;
begin
  SM2 := TCnSM2.Create(ctSM2Example256);
  APrivateKey := TCnSM2PrivateKey.Create;
  APublicKey := TCnSM2PublicKey.Create;
  BPrivateKey := TCnSM2PrivateKey.Create;
  BPublicKey := TCnSM2PublicKey.Create;
  RandA := TCnBigNumber.Create;
  RandB := TCnBigNumber.Create;

  APrivateKey.SetHex('6FCBA2EF9AE0AB902BC3BDE3FF915D44BA4CC78F88E2F8E7F8996D3B8CCEEDEE');
  APublicKey.X.SetHex('3099093BF3C137D8FCBBCDF4A2AE50F3B0F216C3122D79425FE03A45DBFE1655');
  APublicKey.Y.SetHex('3DF79E8DAC1CF0ECBAA2F2B49D51A4B387F2EFAF482339086A27A8E05BAED98B');

  BPrivateKey.SetHex('5E35D7D3F3C54DBAC72E61819E730B019A84208CA3A35E4C2E353DFCCB2A3B53');
  BPublicKey.X.SetHex('245493D446C38D8CC0F118374690E7DF633A8A4BFB3329B5ECE604B2B4F37F43');
  BPublicKey.Y.SetHex('53C0869F4B9E17773DE68FEC45E14904E0DEA45BF6CECF9918C85EA047C60A4C');

  OutRA := TCnEccPoint.Create;
  OutRB := TCnEccPoint.Create;

  try
    if not CnSM2KeyExchangeAStep1(USER_A, USER_B, KEY_LENGTH, APrivateKey, APublicKey,
      BPublicKey, RandA, OutRA, SM2) then
      Exit;

    if not CnSM2KeyExchangeBStep1(USER_A, USER_B, KEY_LENGTH, BPrivateKey,
      APublicKey, BPublicKey, OutRA, KB, OutRB, OpSB, OpS2, SM2) then
      Exit;

    if not CnSM2KeyExchangeAStep2(USER_A, USER_B, KEY_LENGTH, APrivateKey, APublicKey,
      BPublicKey, OutRA, OutRB, RandA, KA, OpSB, OpSA, SM2) then
      Exit;

    if CnSM2KeyExchangeBStep2(USER_A, USER_B, KEY_LENGTH, BPrivateKey, APublicKey,
      BPublicKey, OpSA, OpS2, SM2) then
      ShowMessage('Key Exchange OK: ' + DataToHex(PAnsiChar(KA), Length(KA)) + ' : '
        + DataToHex(PAnsiChar(KB), Length(KB)));

  finally
    OutRA.Free;
    OutRB.Free;
    RandA.Free;
    RandB.Free;

    APublicKey.Free;
    APrivateKey.Free;
    BPublicKey.Free;
    BPrivateKey.Free;
    SM2.Free;
  end;
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

procedure TFormSM2.btnSM2EncryptClick(Sender: TObject);
var
  T: AnsiString;
  B, R: TBytes;
  SM2: TCnSM2;
  PublicKey: TCnSM2PublicKey;
  EnStream: TMemoryStream;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) then
    Exit;

  if Length(edtSM2Text.Text) = 0 then
  begin
    ShowMessage('Please Enter some Text');
    Exit;
  end;

  SM2 := TCnSM2.Create(ctSM2);
  PublicKey := TCnSM2PublicKey.Create;

  EnStream := TMemoryStream.Create;

  PublicKey.SetHex(edtSM2PublicKey.Text);

  T := AnsiString(edtSM2Text.Text);
  if rbC1C3C2.Checked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

  if chkEncDecTBytes.Checked then
  begin
    SetLength(B, Length(T));
    Move(T[1], B[0], Length(T));

    R := CnSM2EncryptData(B, PublicKey, SM2, ST, chkPrefixByte.Checked);
    if Length(R) > 0 then
    begin
      ShowMessage('Encrypt OK');
      mmoSM2Result.Lines.Text := BytesToHex(R);
    end;
  end
  else
  begin
    if CnSM2EncryptData(@T[1], Length(T), EnStream, PublicKey, SM2, ST, chkPrefixByte.Checked) then
    begin
      ShowMessage('Encrypt OK');
      mmoSM2Result.Lines.Text := DataToHex(PAnsiChar(EnStream.Memory), EnStream.Size);
    end;
  end;

  PublicKey.Free;
  EnStream.Free;
  SM2.Free;
end;

procedure TFormSM2.btnGenerateKeyClick(Sender: TObject);
var
  SM2: TCnSM2;
  PrivateKey: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
begin
  SM2 := TCnSM2.Create(ctSM2);
  PrivateKey := TCnSM2PrivateKey.Create;
  PublicKey := TCnSM2PublicKey.Create;

  SM2.GenerateKeys(PrivateKey, PublicKey);

  // ToHex 如果数值较小，长度可能不够，导致后面解析不了，需要调整为固定尺寸
  edtSM2PublicKey.Text := '04' + PublicKey.X.ToHex(CN_SM2_FINITEFIELD_BYTESIZE)
    + PublicKey.Y.ToHex(CN_SM2_FINITEFIELD_BYTESIZE);
  edtSM2PrivateKey.Text := PrivateKey.ToHex(CN_SM2_FINITEFIELD_BYTESIZE);

  PrivateKey.Free;
  PublicKey.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2DecryptClick(Sender: TObject);
var
  S: AnsiString;
  B, R: TBytes;
  SM2: TCnSM2;
  PrivateKey: TCnSM2PrivateKey;
  EnStream, DeStream: TMemoryStream;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPrivateKeyStr(edtSM2PrivateKey) then
    Exit;

  if Length(Trim(mmoSM2Result.Lines.Text)) < 2 then
  begin
    ShowMessage('SM2 Decrypted Hex Invalid.');
    Exit;
  end;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKey := TCnSM2PrivateKey.Create;

  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  PrivateKey.SetHex(edtSM2PrivateKey.Text);

  MyStreamFromHex(Trim(mmoSM2Result.Lines.Text), EnStream);

  if rbC1C3C2.Checked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

  if chkEncDecTBytes.Checked then
  begin
    SetLength(B, EnStream.Size);
    Move(EnStream.Memory^, B[0], EnStream.Size);

    R := CnSM2DecryptData(B, PrivateKey, SM2, ST);
    if Length(R) > 0 then
    begin
      SetLength(S, Length(R));
      Move(R[0], S[1], Length(R));
      ShowMessage('Decrypt OK: ' + S);
      edtSM2Text.Text := S;
    end
    else
      ShowMessage('Decrypt Failed');
  end
  else
  begin
    if CnSM2DecryptData(EnStream.Memory, EnStream.Size, DeStream, PrivateKey, SM2, ST) then
    begin
      SetLength(S, DeStream.Size);
      DeStream.Position := 0;
      DeStream.Read(S[1], DeStream.Size);
      ShowMessage('Decrypt OK: ' + S);
      edtSM2Text.Text := S;
    end
    else
      ShowMessage('Decrypt Failed');
  end;

  PrivateKey.Free;
  EnStream.Free;
  DeStream.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSignBrowseClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    edtSM2FileSign.Text := dlgOpen1.FileName;
end;

procedure TFormSM2.btnSM2SignFileClick(Sender: TObject);
var
  SM2: TCnSM2;
  B: TBytes;
  PrivateKey: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) or not CheckPrivateKeyStr(edtSM2PrivateKey) then
    Exit;

  if not FileExists(edtSM2FileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKey := TCnSM2PrivateKey.Create;
  PrivateKey.SetHex(edtSM2PrivateKey.Text);

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKey.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2FileSign.Text);

  SignRes := TCnSM2Signature.Create;

  if chkSignTBytes.Checked then
  begin
    SetLength(B, FileStream.Size);
    Move(FileStream.Memory^, B[0], FileStream.Size);

    if CnSM2SignData(edtSM2UserId.Text, B, SignRes, PrivateKey, PublicKey, SM2) then
    begin
      if chkSignBase64.Checked then
        mmoSignResult.Lines.Text := SignRes.ToBase64(SM2.BytesCount)
      else
        mmoSignResult.Lines.Text := SignRes.ToHex(SM2.BytesCount);
    end
    else
      ShowMessage('Sign File Failed.');
  end
  else
  begin
    if CnSM2SignData(edtSM2UserId.Text, FileStream.Memory, FileStream.Size, SignRes,
      PrivateKey, PublicKey, SM2) then
    begin
      if chkSignBase64.Checked then
        mmoSignResult.Lines.Text := SignRes.ToBase64(SM2.BytesCount)
      else
        mmoSignResult.Lines.Text := SignRes.ToHex(SM2.BytesCount);
    end
    else
      ShowMessage('Sign File Failed.');
  end;

  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  PrivateKey.Free;
  SM2.Free;
end;

function TFormSM2.CheckPrivateKeyStr(Edit: TEdit): Boolean;
var
  B: TBytes;
begin
  Result := True;
  if IsHexString(Edit.Text) then
  begin
    if Length(Edit.Text) <> 64 then
    begin
      ShowMessage('SM2 Private Key Hex Invalid. Hex Should be 64 Length.');
      Result := False;
      Exit;
    end;
  end
  else // 当作 Base64
  begin
    if Base64Decode(Edit.Text, B) <> BASE64_OK then
    begin
      ShowMessage('SM2 Private Key Base64 Invalid.');
      Result := False;
      Exit;
    end;

    if (Length(B) = 33) and (B[0] = 0) then
    begin
      Move(B[1], B[0], 32);
      SetLength(B, 32);
    end;

    if Length(B) <> 32 then
    begin
      ShowMessage('SM2 Private Key Base64 Invalid. Should be 64 Length.');
      Result := False;
      Exit;
    end;

    Edit.Text := BytesToHex(B);
  end;
end;

function TFormSM2.CheckPublicKeyStr(Edit: TEdit): Boolean;
var
  B: TBytes;
begin
  Result := True;
  if IsHexString(Edit.Text) then
  begin
    if Length(Edit.Text) <> 128 + 2 then
    begin
      ShowMessage('SM2 Public Key Hex Invalid. Hex Should be 128 Length.');
      Result := False;
      Exit;
    end;

    if Copy(Edit.Text, 1, 2) <> '04' then
    begin
      ShowMessage('SM2 Public Key Hex Head Invalid. Only 04 Supported.');
      Result := False;
      Exit;
    end;
  end
  else // 当作 Base64
  begin
    if Base64Decode(Edit.Text, B) <> BASE64_OK then
    begin
      ShowMessage('SM2 Public Key Base64 Invalid.');
      Result := False;
      Exit;
    end;

    if Length(B) <> 64 then
    begin
      ShowMessage('SM2 Public Key Base64 Length Invalid. Should be 64 Length.');
      Result := False;
      Exit;
    end;

    if B[0] <> 4 then
    begin
      ShowMessage('SM2 Public Key Base64 Head Invalid. Only 04 Supported.');
      Result := False;
      Exit;
    end;

    Edit.Text := BytesToHex(B);
  end;
end;

procedure TFormSM2.btnSM2VerifyClick(Sender: TObject);
var
  SM2: TCnSM2;
  B: TBytes;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) then
    Exit;

  if not FileExists(edtSM2FileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKey.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2FileSign.Text);

  SignRes := TCnSM2Signature.Create;
  if IsHexString(mmoSignResult.Lines.Text) then
    SignRes.SetHex(mmoSignResult.Lines.Text)
  else
  begin
    if not SignRes.SetBase64(mmoSignResult.Lines.Text) then
      ShowMessage('Invalid Base64 Signature.');
  end;

  if chkSignTBytes.Checked then
  begin
    SetLength(B, FileStream.Size);
    Move(FileStream.Memory^, B[0], FileStream.Size);

    if CnSM2VerifyData(edtSM2UserId.Text, B, SignRes, PublicKey, SM2) then
    begin
      ShowMessage('Verify File OK.');
    end
    else
      ShowMessage('Verify File Failed.');
  end
  else
  begin
    if CnSM2VerifyData(edtSM2UserId.Text, FileStream.Memory, FileStream.Size, SignRes,
      PublicKey, SM2) then
    begin
      ShowMessage('Verify File OK.');
    end
    else
      ShowMessage('Verify File Failed.');
  end;

  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSignFileClick(Sender: TObject);
var
  PrivateKey: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) or not CheckPrivateKeyStr(edtSM2PrivateKey) then
    Exit;

  PrivateKey := TCnSM2PrivateKey.Create;
  PrivateKey.SetHex(edtSM2PrivateKey.Text);

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKey.Text);

  mmoSignResult.Lines.Text := CnSM2SignFile(edtSM2UserId.Text, edtSM2FileSign.Text, PrivateKey, PublicKey);

  PrivateKey.Free;
  PublicKey.Free;
end;

procedure TFormSM2.btnVerifyFileClick(Sender: TObject);
var
  PublicKey: TCnSM2PublicKey;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) then
    Exit;

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKey.Text);

  if CnSM2VerifyFile(edtSM2UserId.Text, edtSM2FileSign.Text, mmoSignResult.Lines.Text, PublicKey) then
    ShowMessage('Verify File OK.')
  else
    ShowMessage('Verify File Failed.');

  PublicKey.Free;
end;

procedure TFormSM2.btnSM2ABKeyExchangeClick(Sender: TObject);
const
  KEY_LENGTH = 128 div 8;
var
  SM2: TCnSM2;
  APrivateKey, BPrivateKey: TCnSM2PrivateKey;
  APublicKey, BPublicKey: TCnSM2PublicKey;
  RandA, RandB: TCnBigNumber;
  OutRA, OutRB: TCnEccPoint;
  KA, KB: AnsiString;
  OpSA, OpSB, OpS2: TSM3Digest;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) or not CheckPublicKeyStr(edtSM2BPublicKey) then
    Exit;

  if not CheckPrivateKeyStr(edtSM2PrivateKey) or not CheckPrivateKeyStr(edtSM2BPrivateKey) then
    Exit;

  SM2 := TCnSM2.Create;
  APrivateKey := TCnSM2PrivateKey.Create;
  APublicKey := TCnSM2PublicKey.Create;
  BPrivateKey := TCnSM2PrivateKey.Create;
  BPublicKey := TCnSM2PublicKey.Create;

  RandA := TCnBigNumber.Create;
  RandB := TCnBigNumber.Create;
  OutRA := TCnEccPoint.Create;
  OutRB := TCnEccPoint.Create;

  APrivateKey.SetHex(edtSM2PrivateKey.Text);
  APublicKey.SetHex(edtSM2PublicKey.Text);
  BPrivateKey.SetHex(edtSM2BPrivateKey.Text);
  BPublicKey.SetHex(edtSM2BPublicKey.Text);

  try
    // Step1
    if not CnSM2KeyExchangeAStep1(edtSM2AUserId.Text, edtSM2BUserId.Text, KEY_LENGTH,
      APrivateKey, APublicKey, BPublicKey, RandA, OutRA, SM2) then
      Exit;

    ShowMessage('A Send RA to B: ' + OutRA.ToHex);

    // Step2
    if not CnSM2KeyExchangeBStep1(edtSM2AUserId.Text, edtSM2BUserId.Text, KEY_LENGTH,
      BPrivateKey, APublicKey, BPublicKey, OutRA, KB, OutRB, OpSB, OpS2, SM2) then
      Exit;

    ShowMessage('B Get KeyB [' + DataToHex(PAnsiChar(KB), Length(KB)) + '] and Send RB to A: ' + OutRB.ToHex);

    // Step3
    if not CnSM2KeyExchangeAStep2(edtSM2AUserId.Text, edtSM2BUserId.Text, KEY_LENGTH,
      APrivateKey, APublicKey, BPublicKey, OutRA, OutRB, RandA, KA, OpSB, OpSA, SM2) then
      Exit;

    ShowMessage('A Get KeyA [' +  DataToHex(PAnsiChar(KA), Length(KA)) + '] and Send OpSA to A: ' + SM3Print(OpSA));

    // Step4
    if not CnSM2KeyExchangeBStep2(edtSM2AUserId.Text, edtSM2BUserId.Text, KEY_LENGTH,
      BPrivateKey, APublicKey, BPublicKey, OpSA, OpS2, SM2) then
      Exit;

    ShowMessage('B Optionally Check OpSA OK');

    if KA = KB then
      ShowMessage('Key Exchange OK: [' + DataToHex(PAnsiChar(KA), Length(KA)) + '] : ['
        + DataToHex(PAnsiChar(KB), Length(KB)) + ']');
  finally
    OutRA.Free;
    OutRB.Free;
    RandA.Free;
    RandB.Free;

    APublicKey.Free;
    APrivateKey.Free;
    BPublicKey.Free;
    BPrivateKey.Free;
    SM2.Free;
  end;
end;

procedure TFormSM2.btnLoadSM2KeyClick(Sender: TObject);
var
  Priv: TCnSM2PrivateKey;
  Pub: TCnSM2PublicKey;
  CurveType: TCnEccCurveType;
begin
  if dlgOpen1.Execute then
  begin
    Priv := TCnSM2PrivateKey.Create;
    Pub := TCnSM2PublicKey.Create;

    if CnEccLoadKeysFromPem(dlgOpen1.FileName, Priv, Pub, CurveType) then
    begin
      if CurveType <> ctSM2 then
      begin
        ShowMessage('NOT SM2 Key');
        Exit;
      end;

      edtSM2PublicKey.Text := Pub.ToHex;
      edtSM2PrivateKey.Text := Priv.ToHex;
    end
    else
      ShowMessage('Load SM2 Key Failed.');

    Priv.Free;
    Pub.Free;
  end;
end;

procedure TFormSM2.btnLoadSM2BKeyClick(Sender: TObject);
var
  Priv: TCnSM2PrivateKey;
  Pub: TCnSM2PublicKey;
  CurveType: TCnEccCurveType;
begin
  if dlgOpen1.Execute then
  begin
    Priv := TCnSM2PrivateKey.Create;
    Pub := TCnSM2PublicKey.Create;

    if CnEccLoadKeysFromPem(dlgOpen1.FileName, Priv, Pub, CurveType) then
    begin
      if CurveType <> ctSM2 then
      begin
        ShowMessage('NOT SM2 Key');
        Exit;
      end;

      edtSM2BPublicKey.Text := Pub.ToHex;
      edtSM2BPrivateKey.Text := Priv.ToHex;
    end
    else
      ShowMessage('Load SM2 Key Failed.');

    Priv.Free;
    Pub.Free;
  end;
end;

procedure TFormSM2.btnVerifySm2KeyClick(Sender: TObject);
var
  SM2: TCnSM2;
  PrivKey: TCnSM2PrivateKey;
  PubKey: TCnSM2PublicKey;
begin
  SM2 := TCnSM2.Create;
  PrivKey := TCnSM2PrivateKey.Create;
  PubKey := TCnSM2PublicKey.Create;

  PrivKey.SetHex(edtSM2PrivateKey.Text);

  PubKey.Assign(SM2.Generator);
  SM2.MultiplePoint(PrivKey, PubKey);

  if UpperCase(PubKey.ToHex) = UpperCase(edtSM2PublicKey.Text) then
    ShowMessage('Verify Keys OK')
  else
    ShowMessage('Verify Keys Fail');

  SM2.Free;
  PubKey.Free;
  PrivKey.Free;
end;

procedure TFormSM2.btnSchnorrProveClick(Sender: TObject);
var
  PrivKey: TCnSM2PrivateKey;
  PubKey: TCnSM2PublicKey;
  R: TCnEccPoint;
  Z: TCnBigNumber;
begin
  PrivKey := TCnSM2PrivateKey.Create;
  PubKey := TCnSM2PublicKey.Create;

  PrivKey.SetHex(edtSM2PrivateKey.Text);
  PubKey.SetHex(edtSM2PublicKey.Text);

  R := TCnEccPoint.Create;
  Z := TCnBigNumber.Create;

  if CnSM2SchnorrProve(PrivKey, PubKey, R, Z) then
  begin
    edtSchnorrProveCheckR.Text := R.ToHex;
    edtSchnorrProveCheckZ.Text := Z.ToHex;
  end;

  Z.Free;
  R.Free;
  PubKey.Free;
  PrivKey.Free;
end;

procedure TFormSM2.btnSchnorrCheckClick(Sender: TObject);
var
  PubKey: TCnSM2PublicKey;
  R: TCnEccPoint;
  Z: TCnBigNumber;
begin
  PubKey := TCnSM2PublicKey.Create;

  PubKey.SetHex(edtSM2PublicKey.Text);

  R := TCnEccPoint.Create;
  R.SetHex(edtSchnorrProveCheckR.Text);

  Z := TCnBigNumber.Create;
  Z.SetHex(edtSchnorrProveCheckZ.Text);

  if CnSM2SchnorrCheck(PubKey, R, Z) then
    ShowMessage('Schnorr Check OK. You have the Private Key!')
  else
    ShowMessage('Schnorr Check Fail.');

  Z.Free;
  R.Free;
  PubKey.Free;
end;

procedure TFormSM2.btnSM2EncryptFileClick(Sender: TObject);
var
  PublicKey: TCnSM2PublicKey;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) then
    Exit;

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKey.Text);

  if dlgOpen1.Execute then
  begin
    if dlgSave1.Execute then
    begin
      if rbC1C3C2.Checked then
        ST := cstC1C3C2
      else
        ST := cstC1C2C3;

      if CnSM2EncryptFile(dlgOpen1.FileName, dlgSave1.FileName, PublicKey, nil, ST) then
        ShowMessage('File Encrypted: ' + dlgSave1.FileName)
      else
        ShowMessage('File Encrypt Fail.');
    end;
  end;
  PublicKey.Free;
end;

procedure TFormSM2.btnSM2DecryptFileClick(Sender: TObject);
var
  PrivateKey: TCnSM2PrivateKey;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPrivateKeyStr(edtSM2PrivateKey) then
    Exit;

  PrivateKey := TCnSM2PrivateKey.Create;
  PrivateKey.SetHex(edtSM2PrivateKey.Text);

  if dlgOpen1.Execute then
  begin
    if dlgSave1.Execute then
    begin
      if rbC1C3C2.Checked then
        ST := cstC1C3C2
      else
        ST := cstC1C2C3;

      if CnSM2DecryptFile(dlgOpen1.FileName, dlgSave1.FileName, PrivateKey, nil, ST) then
        ShowMessage('File Decrypted: ' + dlgSave1.FileName)
      else
        ShowMessage('File Decrypt Fail.');
    end;
  end;
  PrivateKey.Free;
end;

procedure TFormSM2.btnSM2CollaborativeGenClick(Sender: TObject);
var
  PrivKeyA, PrivKeyB: TCnSM2CollaborativePrivateKey;
  PubKey: TCnSM2CollaborativePublicKey;
  PrivKey: TCnSM2PrivateKey;
  P, R: TCnEccPoint;
  SM2: TCnSM2;
  Z: TCnBigNumber;
begin
  PrivKeyA := nil;
  PrivKeyB := nil;
  PubKey := nil;
  PrivKey := nil;
  P := nil;
  R := nil;
  Z := nil;
  SM2 := nil;

  try
    PrivKeyA := TCnSM2CollaborativePrivateKey.Create;
    PrivKeyB := TCnSM2CollaborativePrivateKey.Create;
    PubKey := TCnSM2CollaborativePublicKey.Create;

    P := TCnEccPoint.Create;
    if CnSM2CollaborativeGenerateKeyAStep1(PrivKeyA, P) then
    begin
      if CnSM2CollaborativeGenerateKeyBStep1(PrivKeyB, P, PubKey) then
      begin
        edtSM2PublicKeyAB.Text := PubKey.ToHex;
        edtSM2PrivateKeyA.Text := PrivKeyA.ToHex;
        edtSM2PrivateKeyB.Text := PrivKeyB.ToHex;

        // 验证 PrivateKeyA * PrivateKeyB - 1
        PrivKey := TCnSM2PrivateKey.Create;
        SM2 := TCnSM2.Create;
        BigNumberDirectMulMod(PrivKey, PrivKeyA, PrivKeyB, SM2.Order);
        BigNumberSubWord(PrivKey, 1);

        R := TCnEccPoint.Create;
        Z := TCnBigNumber.Create;
        CnSM2SchnorrProve(PrivKey, PubKey, R, Z, SM2);

        if CnSM2SchnorrCheck(PubKey, R, Z, SM2) then
          ShowMessage('2 Keys Check OK');
      end;
    end;
  finally
    SM2.Free;
    Z.Free;
    R.Free;
    P.Free;
    PrivKey.Free;
    PubKey.Free;
    PrivKeyB.Free;
    PrivKeyA.Free;
  end;
end;

procedure TFormSM2.btnCollSignBrowseClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    edtSM2CollFileSign.Text := dlgOpen1.FileName;
end;

procedure TFormSM2.btnSM2CollVerifyClick(Sender: TObject);
var
  SM2: TCnSM2;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
begin
  if not CheckPublicKeyStr(edtSM2PublicKeyAB) then
    Exit;

  if not FileExists(edtSM2CollFileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKeyAB.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2CollFileSign.Text);

  SignRes := TCnSM2Signature.Create;
  SignRes.SetHex(mmoSM2CollResult.Lines.Text);

  if CnSM2VerifyData(edtSM2CollUserId.Text, FileStream.Memory, FileStream.Size, SignRes,
    PublicKey, SM2) then
  begin
    ShowMessage('Verify File OK.');
  end
  else
    ShowMessage('Verify File Failed.');

  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2CollSignFileClick(Sender: TObject);
var
  SM2: TCnSM2;
  PrivateKeyA, PrivateKeyB: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
  E, K, R, S1, S2: TCnBigNumber;
  Q: TCnEccPoint;
begin
  if not CheckPublicKeyStr(edtSM2PublicKeyAB) or not CheckPrivateKeyStr(edtSM2PrivateKeyA)
    or not CheckPrivateKeyStr(edtSM2PrivateKeyB) then
    Exit;

  if not FileExists(edtSM2CollFileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKeyA := TCnSM2PrivateKey.Create;
  PrivateKeyA.SetHex(edtSM2PrivateKeyA.Text);
  PrivateKeyB := TCnSM2PrivateKey.Create;
  PrivateKeyB.SetHex(edtSM2PrivateKeyB.Text);

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKeyAB.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2CollFileSign.Text);

  SignRes := TCnSM2Signature.Create;

  E := TCnBigNumber.Create;
  K := TCnBigNumber.Create;
  R := TCnBigNumber.Create;
  S1 := TCnBigNumber.Create;
  S2 := TCnBigNumber.Create;
  Q := TCnEccPoint.Create;

  if CnSM2CollaborativeSignAStep1(edtSM2CollUserId.Text, FileStream.Memory, FileStream.Size,
    E, Q, K, PrivateKeyA, PublicKey, SM2) then
  begin
    if CnSM2CollaborativeSignBStep1(E, Q, R, S1, S2, PrivateKeyB, SM2) then
    begin
      if CnSM2CollaborativeSignAStep2(K, R, S1, S2, SignRes, PrivateKeyA, SM2) then
      begin
        mmoSM2CollResult.Lines.Text := SignRes.ToHex;
        ShowMessage('A B Collabrative Sign OK.');
      end
      else
        ShowMessage('Sign A Step 3 Failed.');
    end
    else
      ShowMessage('Sign B Step 2 Failed.');
  end
  else
    ShowMessage('Sign A Step 1 Failed.');

  Q.Free;
  S2.Free;
  S1.Free;
  R.Free;
  K.Free;
  E.Free;

  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  PrivateKeyB.Free;
  PrivateKeyA.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2CollEncryptFileClick(Sender: TObject);
var
  PublicKey: TCnSM2PublicKey;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPublicKeyStr(edtSM2PublicKeyAB) then
    Exit;

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKeyAB.Text);

  if dlgOpen1.Execute then
  begin
    if dlgSave1.Execute then
    begin
      if rbC1C3C2.Checked then
        ST := cstC1C3C2
      else
        ST := cstC1C2C3;

      if CnSM2EncryptFile(dlgOpen1.FileName, dlgSave1.FileName, PublicKey, nil, ST) then
        ShowMessage('File Encrypted: ' + dlgSave1.FileName)
      else
        ShowMessage('File Encrypt Fail.');
    end;
  end;
  PublicKey.Free;
end;

procedure TFormSM2.btnSM2CollEncryptClick(Sender: TObject);
var
  T: AnsiString;
  SM2: TCnSM2;
  PublicKey: TCnSM2PublicKey;
  EnStream: TMemoryStream;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPublicKeyStr(edtSM2PublicKeyAB) then
    Exit;

  if Length(edtSM2CollText.Text) = 0 then
  begin
    ShowMessage('Please Enter some Text');
    Exit;
  end;

  SM2 := TCnSM2.Create(ctSM2);
  PublicKey := TCnSM2PublicKey.Create;

  EnStream := TMemoryStream.Create;

  PublicKey.SetHex(edtSM2PublicKeyAB.Text);

  T := AnsiString(edtSM2CollText.Text);
  if rbC1C3C2.Checked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

  if CnSM2EncryptData(@T[1], Length(T), EnStream, PublicKey, SM2, ST, chkCollPrefixByte.Checked) then
  begin
    mmoSM2CollResult.Lines.Text := DataToHex(PAnsiChar(EnStream.Memory), EnStream.Size);
    ShowMessage('Encrypt OK');
  end;

  PublicKey.Free;
  EnStream.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2CollDecryptClick(Sender: TObject);
var
  S: AnsiString;
  SM2: TCnSM2;
  PrivateKeyA, PrivateKeyB: TCnSM2CollaborativePrivateKey;
  EnStream, DeStream: TMemoryStream;
  ST: TCnSM2CryptSequenceType;
  T: TCnEccPoint;
begin
  if not CheckPrivateKeyStr(edtSM2PrivateKeyA) or not CheckPrivateKeyStr(edtSM2PrivateKeyB) then
    Exit;

  if Length(Trim(mmoSM2CollResult.Lines.Text)) < 2 then
  begin
    ShowMessage('SM2 Decrypted Hex Invalid.');
    Exit;
  end;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKeyA := TCnSM2CollaborativePrivateKey.Create;
  PrivateKeyB := TCnSM2CollaborativePrivateKey.Create;

  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  PrivateKeyA.SetHex(edtSM2PrivateKeyA.Text);
  PrivateKeyB.SetHex(edtSM2PrivateKeyB.Text);

  T := TCnEccPoint.Create;

  MyStreamFromHex(Trim(mmoSM2CollResult.Lines.Text), EnStream);

  if rbCollC1C3C2.Checked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

  if CnSM2CollaborativeDecryptAStep1(EnStream.Memory, EnStream.Size, T, PrivateKeyA, SM2) then
  begin
    if CnSM2CollaborativeDecryptBStep1(T, T, PrivateKeyB) then
    begin
      if CnSM2CollaborativeDecryptAStep2(EnStream.Memory, EnStream.Size, T, DeStream, PrivateKeyA, SM2, ST) then
      begin
        SetLength(S, DeStream.Size);
        DeStream.Position := 0;
        DeStream.Read(S[1], DeStream.Size);
        ShowMessage('Decrypt OK: ' + S);
        edtSM2CollText.Text := S;
      end
      else
        ShowMessage('Decrypt A Step 3 Failed.');
    end
    else
      ShowMessage('Decrypt B Step 2 Failed.');
  end
  else
    ShowMessage('Decrypt A Step 1 Failed.');

  T.Free;
  PrivateKeyB.Free;
  PrivateKeyA.Free;
  EnStream.Free;
  DeStream.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSm2SignTimeClick(Sender: TObject);
var
  SM2: TCnSM2;
  PrivateKey: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
  I: Integer;
  T: Cardinal;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) or not CheckPrivateKeyStr(edtSM2PrivateKey) then
    Exit;

  if not FileExists(edtSM2FileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKey := TCnSM2PrivateKey.Create;
  PrivateKey.SetHex(edtSM2PrivateKey.Text);

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKey.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2FileSign.Text);

  SignRes := TCnSM2Signature.Create;

  T := GetTickCount;
  for I := 1 to 1000 do
    CnSM2SignData(edtSM2UserId.Text, FileStream.Memory, FileStream.Size, SignRes,
      PrivateKey, PublicKey, SM2);
  T := GetTickCount - T;

  mmoSignResult.Lines.Text := SignRes.ToHex;
  ShowMessage('Sign 1000 Time: ' + IntToStr(T));  // 常规仿射坐标计算下，签名一千次要二十秒，一次 20 毫秒
                                                  // 改成预计算 2^K 点后，6.5 秒，一次 6.5 毫秒
                                                  // 改成预计算 2^4 的固定基后，4 秒，一次 4 毫秒
  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  PrivateKey.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2VerifyTimeClick(Sender: TObject);
var
  SM2: TCnSM2;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
  I: Integer;
  T: Cardinal;
begin
  if not CheckPublicKeyStr(edtSM2PublicKey) then
    Exit;

  if not FileExists(edtSM2FileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKey.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2FileSign.Text);

  SignRes := TCnSM2Signature.Create;
  SignRes.SetHex(mmoSignResult.Lines.Text);

  T := GetTickCount;
  for I := 1 to 1000 do
    CnSM2VerifyData(edtSM2UserId.Text, FileStream.Memory, FileStream.Size, SignRes,
      PublicKey, SM2);
  T := GetTickCount - T;

  ShowMessage('Verify 1000 Time: ' + IntToStr(T));  // 常规仿射坐标计算下，验证一千次要三十五秒，一次 35 毫秒
                                                    // 改成预计算 2^K 点后，24 秒，一次 24 毫秒
                                                    // 改成预计算 2^4 的固定基后，21 秒，一次 21 毫秒（因为验签里有非 G 点标量乘）
                                                    // 又改成 NAF 以加速公钥点乘后，19 秒，一次 19 毫秒
  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2CreateMatrixClick(Sender: TObject);
var
  SM2: TCnSM2;
  Matrix: TCnEcc3Matrix;
  MWidth, MRows, MCols: Integer;
  R, C, I: Integer;
  P: TCnEcc3Point;
  Q: TCnEccPoint;
  T: Cardinal;
  K, Mask, S: TCnBigNumber;
begin
  SM2 := TCnSM2.Create;
  P := TCnEcc3Point.Create;

  MWidth := 4; // 如果改成 2，则下面 AffinePointAddPoint 会少加两次

  MRows := 256 div MWidth;
  MCols := 1 shl MWidth;

  Matrix := TCnEcc3Matrix.Create(MRows, MCols);

  T := GetTickCount;

  // 第 0 Row 的第 0 到 15 Col 分别是  Col * G
  CnEccPointToEcc3Point(SM2.Generator, P); // P 拿到射影 G
  Matrix.ValueObject[0, 0].SetZero;

  for C := 0 to MCols - 2 do
    SM2.AffinePointAddPoint(Matrix.ValueObject[0, C], P, Matrix.ValueObject[0, C + 1]);

  for R := 1 to MRows - 1 do
  begin
    for C := 0 to MCols - 1 do
    begin
      SM2.AffinePointAddPoint(Matrix.ValueObject[R - 1, C], Matrix.ValueObject[R - 1, C], Matrix.ValueObject[R, C]);
      for I := 1 to MWidth - 1 do
        SM2.AffinePointAddPoint(Matrix.ValueObject[R, C], Matrix.ValueObject[R, C], Matrix.ValueObject[R, C]);
        // 自加二次 = 乘以 4，自加四次 = 乘以 16
    end;
  end;

  T := GetTickCount - T; // Width 是 4 时耗时 200 毫秒左右，是 2 时 50 毫秒左右

  ShowMessage('Create Matrix Time: ' + IntToStr(T));

  // 验证 [Row, Col] 的值是 Col * (2^Width)^Row，其中 Row 是第 Row 片，Col 是该片值（片宽 Width）
  K := TCnBigNumber.Create;
  for R := 0 to MRows - 1 do
  begin
    for C := 0 to MCols - 1 do
    begin
      K.SetOne;
      K.ShiftLeft(MWidth * R);
      K.MulWord(C);

      CnEccPointToEcc3Point(SM2.Generator, P); // P 拿到射影 G
      SM2.AffineMultiplePoint(K, P);

      if not CnAffineEcc3PointEqual(P, Matrix.ValueObject[R, C], SM2.FiniteFieldSize) then
      begin
        ShowMessage(Format('NOT Equal Row %d, Col %d', [R, C]));

        Matrix.Free;
        P.Free;
        SM2.Free;
        Exit; // 没释放但是先不管
      end;
    end;
  end;
  ShowMessage('Matrix Verify OK');

  Q := TCnEccPoint.Create;

  // 根据 Matrix 求值
  K.SetHex('367ABD41981255BFACE42567BEC23456789872BFFFEACD3295');
  // K.SetHex('3');
  CnEccPointToEcc3Point(SM2.Generator, P); // P 拿到射影 G
  SM2.AffineMultiplePoint(K, P);
  CnAffinePointToEccPoint(P, Q, SM2.FiniteFieldSize);
  ShowMessage(Q.ToString); // 应该等于下面

  Mask := TCnBigNumber.FromHex('F');
  S := TCnBigNumber.Create;
  R := 0;
  P.SetZero;
  while not K.IsZero do
  begin
    BigNumberAnd(S, K, Mask); // 留下最低四位
    C := S.GetWord;

    SM2.AffinePointAddPoint(P, Matrix[R, C], P);
    BigNumberShiftRight(K, K, 4);
    Inc(R);
  end;

  CnAffinePointToEccPoint(P, Q, SM2.FiniteFieldSize);
  ShowMessage(Q.ToString);   // 应该等于上面

  K.Free;
  S.Free;

  Matrix.Free; // 创建完毕后占大约 200k 内存
  Q.Free;
  P.Free;
  SM2.Free;
end;

procedure TFormSM2.btnColl3SignBrowseClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    edtSM2Coll3FileSign.Text := dlgOpen1.FileName;
end;

procedure TFormSM2.btnSM2Collaborative3GenClick(Sender: TObject);
var
  PrivKeyA, PrivKeyB, PrivKeyC: TCnSM2CollaborativePrivateKey;
  PubKey: TCnSM2CollaborativePublicKey;
  PrivKey: TCnSM2PrivateKey;
  P, R: TCnEccPoint;
  SM2: TCnSM2;
  Z: TCnBigNumber;
begin
  PrivKeyA := nil;
  PrivKeyB := nil;
  PrivKeyC := nil;
  PubKey := nil;
  PrivKey := nil;
  P := nil;
  R := nil;
  Z := nil;
  SM2 := nil;

  try
    PrivKeyA := TCnSM2CollaborativePrivateKey.Create;
    PrivKeyB := TCnSM2CollaborativePrivateKey.Create;
    PrivKeyC := TCnSM2CollaborativePrivateKey.Create;
    PubKey := TCnSM2CollaborativePublicKey.Create;

    P := TCnEccPoint.Create;
    if CnSM2Collaborative3GenerateKeyAStep1(PrivKeyA, P) then
    begin
      if CnSM2Collaborative3GenerateKeyBStep1(PrivKeyB, P, P) then
      begin
        if CnSM2Collaborative3GenerateKeyCStep1(PrivKeyC, P, PubKey) then
        begin
          edtSM2PublicKeyABC.Text := PubKey.ToHex;
          edtSM2Private3KeyA.Text := PrivKeyA.ToHex;
          edtSM2Private3KeyB.Text := PrivKeyB.ToHex;
          edtSM2Private3KeyC.Text := PrivKeyC.ToHex;

          // 验证 PrivateKeyA * PrivateKeyB * PrivateKeyC - 1
          PrivKey := TCnSM2PrivateKey.Create;
          SM2 := TCnSM2.Create;
          BigNumberDirectMulMod(PrivKey, PrivKeyA, PrivKeyB, SM2.Order);
          BigNumberDirectMulMod(PrivKey, PrivKey, PrivKeyC, SM2.Order);
          BigNumberSubWord(PrivKey, 1);

          R := TCnEccPoint.Create;
          Z := TCnBigNumber.Create;
          CnSM2SchnorrProve(PrivKey, PubKey, R, Z, SM2);

          if CnSM2SchnorrCheck(PubKey, R, Z, SM2) then
            ShowMessage('3 Keys Check OK');
        end;
      end;
    end;
  finally
    SM2.Free;
    Z.Free;
    R.Free;
    P.Free;
    PrivKey.Free;
    PubKey.Free;
    PrivKeyC.Free;
    PrivKeyB.Free;
    PrivKeyA.Free;
  end;
end;

procedure TFormSM2.btnSM2Coll3SignFileClick(Sender: TObject);
var
  SM2: TCnSM2;
  PrivateKeyA, PrivateKeyB, PrivateKeyC: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
  E, KA, KB, R, S1, S2: TCnBigNumber;
  Q: TCnEccPoint;
begin
  if not CheckPublicKeyStr(edtSM2PublicKeyABC) or not CheckPrivateKeyStr(edtSM2Private3KeyA)
    or not CheckPrivateKeyStr(edtSM2Private3KeyB) or not CheckPrivateKeyStr(edtSM2Private3KeyC) then
    Exit;

  if not FileExists(edtSM2Coll3FileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKeyA := TCnSM2PrivateKey.Create;
  PrivateKeyA.SetHex(edtSM2Private3KeyA.Text);
  PrivateKeyB := TCnSM2PrivateKey.Create;
  PrivateKeyB.SetHex(edtSM2Private3KeyB.Text);
  PrivateKeyC := TCnSM2PrivateKey.Create;
  PrivateKeyC.SetHex(edtSM2Private3KeyC.Text);

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKeyABC.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2Coll3FileSign.Text);

  SignRes := TCnSM2Signature.Create;

  E := TCnBigNumber.Create;
  KA := TCnBigNumber.Create;
  KB := TCnBigNumber.Create;
  R := TCnBigNumber.Create;
  S1 := TCnBigNumber.Create;
  S2 := TCnBigNumber.Create;
  Q := TCnEccPoint.Create;

  try
    if not CnSM2Collaborative3SignAStep1(edtSM2Coll3UserId.Text, FileStream.Memory, FileStream.Size,
      E, Q, KA, PrivateKeyA, PublicKey, SM2) then
    begin
      ShowMessage('A 3Sign 1 Fail.');
      Exit;
    end;

    if not CnSM2Collaborative3SignBStep1(E, Q, Q, KB, PrivateKeyB, SM2) then
    begin
      ShowMessage('B 3Sign 1 Fail.');
      Exit;
    end;

    if not CnSM2Collaborative3SignCStep1(E, Q, R, S1, S2, PrivateKeyC, SM2) then
    begin
      ShowMessage('C 3Sign 1 Fail.');
      Exit;
    end;

    if not CnSM2Collaborative3SignBStep2(KB, R, S1, S2, S1, S2, PrivateKeyB, SM2) then
    begin
      ShowMessage('B 3Sign 2 Fail.');
      Exit;
    end;

    if not CnSM2Collaborative3SignAStep2(KA, R, S1, S2, SignRes, PrivateKeyA, SM2) then
    begin
      ShowMessage('A 3Sign 2 Fail.');
      Exit;
    end;

    mmoSM2Coll3Result.Lines.Text := SignRes.ToHex;
    ShowMessage('A B C Collabrative Sign OK.');
  finally
    Q.Free;
    S1.Free;
    R.Free;
    KB.Free;
    KA.Free;
    E.Free;

    SignRes.Free;
    FileStream.Free;
    PublicKey.Free;
    PrivateKeyB.Free;
    PrivateKeyA.Free;
    SM2.Free;
  end;
end;

procedure TFormSM2.btnSM2Coll3VerifyClick(Sender: TObject);
var
  SM2: TCnSM2;
  PublicKey: TCnSM2PublicKey;
  FileStream: TMemoryStream;
  SignRes: TCnSM2Signature;
begin
  if not CheckPublicKeyStr(edtSM2PublicKeyABC) then
    Exit;

  if not FileExists(edtSM2Coll3FileSign.Text) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.SetHex(edtSM2PublicKeyABC.Text);

  FileStream := TMemoryStream.Create;
  FileStream.LoadFromFile(edtSM2Coll3FileSign.Text);

  SignRes := TCnSM2Signature.Create;
  SignRes.SetHex(mmoSM2Coll3Result.Lines.Text);

  if CnSM2VerifyData(edtSM2Coll3UserId.Text, FileStream.Memory, FileStream.Size, SignRes,
    PublicKey, SM2) then
  begin
    ShowMessage('Verify File OK.');
  end
  else
    ShowMessage('Verify File Failed.');

  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2Coll3EncryptClick(Sender: TObject);
var
  T: AnsiString;
  SM2: TCnSM2;
  PublicKey: TCnSM2PublicKey;
  EnStream: TMemoryStream;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPublicKeyStr(edtSM2PublicKeyABC) then
    Exit;

  if Length(edtSM2Coll3Text.Text) = 0 then
  begin
    ShowMessage('Please Enter some Text');
    Exit;
  end;

  SM2 := TCnSM2.Create(ctSM2);
  PublicKey := TCnSM2PublicKey.Create;

  EnStream := TMemoryStream.Create;

  PublicKey.SetHex(edtSM2PublicKeyABC.Text);

  T := AnsiString(edtSM2Coll3Text.Text);
  if rbColl3C1C3C2.Checked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

  if CnSM2EncryptData(@T[1], Length(T), EnStream, PublicKey, SM2, ST, chkCollPrefixByte.Checked) then
  begin
    mmoSM2Coll3Result.Lines.Text := DataToHex(PAnsiChar(EnStream.Memory), EnStream.Size);
    ShowMessage('Encrypt OK');
  end;

  PublicKey.Free;
  EnStream.Free;
  SM2.Free;
end;

procedure TFormSM2.btnSM2Coll3DecryptClick(Sender: TObject);
var
  S: AnsiString;
  SM2: TCnSM2;
  PrivateKeyA, PrivateKeyB, PrivateKeyC: TCnSM2CollaborativePrivateKey;
  EnStream, DeStream: TMemoryStream;
  ST: TCnSM2CryptSequenceType;
  T: TCnEccPoint;
begin
  if not CheckPrivateKeyStr(edtSM2Private3KeyA) or not CheckPrivateKeyStr(edtSM2Private3KeyB)
    or not CheckPrivateKeyStr(edtSM2Private3KeyC) then
    Exit;

  if Length(Trim(mmoSM2Coll3Result.Lines.Text)) < 2 then
  begin
    ShowMessage('SM2 Decrypted Hex Invalid.');
    Exit;
  end;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKeyA := TCnSM2CollaborativePrivateKey.Create;
  PrivateKeyB := TCnSM2CollaborativePrivateKey.Create;
  PrivateKeyC := TCnSM2CollaborativePrivateKey.Create;

  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  PrivateKeyA.SetHex(edtSM2Private3KeyA.Text);
  PrivateKeyB.SetHex(edtSM2Private3KeyB.Text);
  PrivateKeyC.SetHex(edtSM2Private3KeyC.Text);

  T := TCnEccPoint.Create;

  MyStreamFromHex(Trim(mmoSM2Coll3Result.Lines.Text), EnStream);

  if rbCollC1C3C2.Checked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

  if CnSM2Collaborative3DecryptAStep1(EnStream.Memory, EnStream.Size, T, PrivateKeyA, SM2) then
  begin
    if CnSM2Collaborative3DecryptBStep1(T, T, PrivateKeyB) then
    begin
      if CnSM2Collaborative3DecryptCStep1(T, T, PrivateKeyC) then
      begin
        if CnSM2Collaborative3DecryptAStep2(EnStream.Memory, EnStream.Size, T, DeStream, PrivateKeyA, SM2, ST) then
        begin
          SetLength(S, DeStream.Size);
          DeStream.Position := 0;
          DeStream.Read(S[1], DeStream.Size);
          ShowMessage('Decrypt OK: ' + S);
          edtSM2CollText.Text := S;
        end
        else
          ShowMessage('3Decrypt A Step 4 Failed.');
      end
      else
        ShowMessage('3Decrypt C Step 3 Failed.');
    end
    else
      ShowMessage('3Decrypt B Step 2 Failed.');
  end
  else
    ShowMessage('3Decrypt A Step 1 Failed.');

  T.Free;
  PrivateKeyB.Free;
  PrivateKeyA.Free;
  EnStream.Free;
  DeStream.Free;
  SM2.Free;
end;

procedure TFormSM2.btnCalcPubFromPrivClick(Sender: TObject);
var
  SM2: TCnSM2;
  PrivateKey: TCnSM2PrivateKey;
  PublicKey: TCnSM2PublicKey;
begin
  if not CheckPrivateKeyStr(edtSM2PrivateKey) then
    Exit;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKey := TCnSM2PrivateKey.Create;
  PrivateKey.SetHex(edtSM2PrivateKey.Text);

  PublicKey := TCnSM2PublicKey.Create;
  PublicKey.Assign(SM2.Generator);
  SM2.MultiplePoint(PrivateKey, PublicKey);

  edtSM2PublicKey.Text := PublicKey.ToHex(SM2.BytesCount);
  PublicKey.Free;
  PrivateKey.Free;
  SM2.Free;
end;

end.
