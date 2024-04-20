unit UnitSM2;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  CnSM2, CnECC, FMX.StdCtrls, CnSM3, CnBigNumber, FMX.ExtCtrls, FMX.Edit, FMX.Memo, FMX.TabControl, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormSM2 = class(TForm)
    lblSM2PublicKey: TLabel;
    lblSM2PrivateKey: TLabel;
    pgcSm2: TTabControl;
    tsEncDec: TTabItem;
    grpSm2Enc: TGroupBox;
    lblSM2Text: TLabel;
    btnSm2Example1: TButton;
    edtSM2Text: TEdit;
    btnSM2Encrypt: TButton;
    mmoSM2Results: TMemo;
    btnSM2Decrypt: TButton;
    chkPrefixByte: TCheckBox;
    grpSeqType: TGroupBox;
    rbC1C3C2: TRadioButton;
    rbC1C2C3: TRadioButton;
    tsSignVerify: TTabItem;
    grpSm2SignVerify: TGroupBox;
    lblUserId: TLabel;
    lblSM2FileSign: TLabel;
    btnSm2SignVerify: TButton;
    edtSM2UserId: TEdit;
    edtSM2FileSign: TEdit;
    btnSignBrowse: TButton;
    mmoSignResult: TMemo;
    btnSM2Verify: TButton;
    btnSM2SignFile: TButton;
    btnSignFile: TButton;
    btnVerifyFile: TButton;
    tsKeyExchange: TTabItem;
    grpSM2KeyExchange: TGroupBox;
    lblAId: TLabel;
    lblBUserId: TLabel;
    lblBSM2PublicKey: TLabel;
    lblBSm2PrivateKey: TLabel;
    lbl1: TLabel;
    btnSM2KeyExchange: TButton;
    edtSM2AUserId: TEdit;
    edtSM2BUserId: TEdit;
    edtSM2BPrivateKey: TEdit;
    edtSM2BPublicKey: TEdit;
    btnSM2ABKeyExchange: TButton;
    btnLoadSM2BKey: TButton;
    edtSM2PublicKey: TEdit;
    edtSM2PrivateKey: TEdit;
    btnGenerateKey: TButton;
    btnLoadSM2Key: TButton;
    btnVerifySm2Key: TButton;
    dlgOpen1: TOpenDialog;
    procedure btnSm2Example1Click(Sender: TObject);
    procedure btnSM2EncryptClick(Sender: TObject);
    procedure btnSM2DecryptClick(Sender: TObject);
    procedure btnSm2SignVerifyClick(Sender: TObject);
    procedure btnSignBrowseClick(Sender: TObject);
    procedure btnSM2VerifyClick(Sender: TObject);
    procedure btnSM2SignFileClick(Sender: TObject);
    procedure btnSignFileClick(Sender: TObject);
    procedure btnVerifyFileClick(Sender: TObject);
    procedure btnSM2KeyExchangeClick(Sender: TObject);
    procedure btnSM2ABKeyExchangeClick(Sender: TObject);
    procedure btnLoadSM2BKeyClick(Sender: TObject);
    procedure btnGenerateKeyClick(Sender: TObject);
    procedure btnLoadSM2KeyClick(Sender: TObject);
    procedure btnVerifySm2KeyClick(Sender: TObject);
  private
    function CheckPublicKeyStr(Edit: TEdit): Boolean;
    function CheckPrivateKeyStr(Edit: TEdit): Boolean;
  public
    { Public declarations }
  end;

var
  FormSM2: TFormSM2;

implementation

{$R *.fmx}

const
  MSG1: AnsiString = 'encryption standard';
  MSG2: AnsiString = 'message digest';

  USER_A: AnsiString = 'ALICE123@YAHOO.COM';
  USER_B: AnsiString = 'BILL456@YAHOO.COM';

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
  SM2 := TCnSM2.Create(ctSM2Example256);
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
  KA, KB: TBytes;
  OpSA, OpSB, OpS2: TCnSM3Digest;
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
      ShowMessage('Key Exchange OK: ' + MyStrToHex(PAnsiChar(KA), Length(KA)) + ' : '
        + MyStrToHex(PAnsiChar(KB), Length(KB)));

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
  if rbC1C3C2.IsChecked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

  if CnSM2EncryptData(@T[1], Length(T), EnStream, PublicKey, SM2, ST, chkPrefixByte.IsChecked) then
  begin
    ShowMessage('Encrypt OK');
    mmoSM2Results.Lines.Text := MyStrToHex(PAnsiChar(EnStream.Memory), EnStream.Size);
  end;

  PublicKey.Free;
  EnStream.Free;
  SM2.Free;
end;

procedure TFormSM2.btnGenerateKeyClick(Sender: TObject);
var
  SM2: TCnSM2;
  PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey;
begin
  SM2 := TCnSM2.Create(ctSM2);
  PrivateKey := TCnEccPrivateKey.Create;
  PublicKey := TCnEccPublicKey.Create;

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
  SM2: TCnSM2;
  PrivateKey: TCnEccPrivateKey;
  EnStream, DeStream: TMemoryStream;
  ST: TCnSM2CryptSequenceType;
begin
  if not CheckPrivateKeyStr(edtSM2PrivateKey) then
    Exit;

  if Length(Trim(mmoSM2Results.Lines.Text)) < 2 then
  begin
    ShowMessage('SM2 Decrypted Hex Invalid.');
    Exit;
  end;

  SM2 := TCnSM2.Create(ctSM2);
  PrivateKey := TCnEccPrivateKey.Create;

  EnStream := TMemoryStream.Create;
  DeStream := TMemoryStream.Create;

  PrivateKey.SetHex(edtSM2PrivateKey.Text);

  MyStreamFromHex(Trim(mmoSM2Results.Lines.Text), EnStream);

  if rbC1C3C2.IsChecked then
    ST := cstC1C3C2
  else
    ST := cstC1C2C3;

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

  if CnSM2SignData(edtSM2UserId.Text, FileStream.Memory, FileStream.Size, SignRes,
    PrivateKey, PublicKey, SM2) then
  begin
    mmoSignResult.Lines.Text := SignRes.ToHex;
  end
  else
    ShowMessage('Sign File Failed.');

  SignRes.Free;
  FileStream.Free;
  PublicKey.Free;
  PrivateKey.Free;
  SM2.Free;
end;

function TFormSM2.CheckPrivateKeyStr(Edit: TEdit): Boolean;
begin
  Result := True;
  if Length(Edit.Text) <> 64 then
  begin
    ShowMessage('SM2 Private Key Hex Invalid. Hex Should be 64 Length.');
    Result := False;
    Exit;
  end;
end;

function TFormSM2.CheckPublicKeyStr(Edit: TEdit): Boolean;
begin
  Result := True;
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
end;

procedure TFormSM2.btnSM2VerifyClick(Sender: TObject);
var
  SM2: TCnSM2;
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
  SignRes.SetHex(mmoSignResult.Lines.Text);

  if CnSM2VerifyData(edtSM2UserId.Text, FileStream.Memory, FileStream.Size, SignRes,
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
  KA, KB: TBytes;
  OpSA, OpSB, OpS2: TCnSM3Digest;
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

    ShowMessage('B Get KeyB [' + MyStrToHex(PAnsiChar(KB), Length(KB)) + '] and Send RB to A: ' + OutRB.ToHex);

    // Step3
    if not CnSM2KeyExchangeAStep2(edtSM2AUserId.Text, edtSM2BUserId.Text, KEY_LENGTH,
      APrivateKey, APublicKey, BPublicKey, OutRA, OutRB, RandA, KA, OpSB, OpSA, SM2) then
      Exit;

    ShowMessage('A Get KeyA [' +  MyStrToHex(PAnsiChar(KA), Length(KA)) + '] and Send OpSA to A: ' + SM3Print(OpSA));

    // Step4
    if not CnSM2KeyExchangeBStep2(edtSM2AUserId.Text, edtSM2BUserId.Text, KEY_LENGTH,
      BPrivateKey, APublicKey, BPublicKey, OpSA, OpS2, SM2) then
      Exit;

    ShowMessage('B Optionally Check OpSA OK');

    if KA = KB then
      ShowMessage('Key Exchange OK: [' + MyStrToHex(PAnsiChar(KA), Length(KA)) + '] : ['
        + MyStrToHex(PAnsiChar(KB), Length(KB)) + ']');
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
  Priv: TCnEccPrivateKey;
  Pub: TCnEccPublicKey;
  CurveType: TCnEccCurveType;
begin
  if dlgOpen1.Execute then
  begin
    Priv := TCnEccPrivateKey.Create;
    Pub := TCnEccPublicKey.Create;

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
  Priv: TCnEccPrivateKey;
  Pub: TCnEccPublicKey;
  CurveType: TCnEccCurveType;
begin
  if dlgOpen1.Execute then
  begin
    Priv := TCnEccPrivateKey.Create;
    Pub := TCnEccPublicKey.Create;

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

end.
