unit SLHDSAMainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnSLHDSA, CnNative;

type
  TSLHDSAMainForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    cbParamSet: TComboBox;
    btnGenerateKeys: TButton;
    chkRandomize: TCheckBox;
    MemoPublicKey: TMemo;
    Label3: TLabel;
    MemoPrivateKey: TMemo;
    Label4: TLabel;
    lblKeyInfo: TLabel;
    Panel2: TPanel;
    Label2: TLabel;
    edtMessage: TMemo;
    btnSign: TButton;
    Label6: TLabel;
    edtSignature: TMemo;
    btnVerify: TButton;
    Label5: TLabel;
    cbPrehashType: TComboBox;
    chkPrehash: TCheckBox;
    procedure btnGenerateKeysClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkPrehashClick(Sender: TObject);
  private
    FPublicKey: TCnSlhPublicKey;
    FSecretKey: TCnSlhSecretKey;
    FHasKeys: Boolean;
    procedure ClearKeyData;
    procedure UpdateStatus(const Msg: string);
    function GetParamSet: TCnSlhParamSet;
    function GetPrehashID: TCnSlhPrehashID;
    function GetPrehashName(PrehashID: TCnSlhPrehashID): string;
  public

  end;

var
  SLHDSAMainForm: TSLHDSAMainForm;

implementation

{$R *.DFM}

procedure TSLHDSAMainForm.FormCreate(Sender: TObject);
begin
  cbParamSet.Items.Add('SLH-DSA-SHA2-128s');
  cbParamSet.Items.Add('SLH-DSA-SHA2-128f');
  cbParamSet.Items.Add('SLH-DSA-SHA2-192s');
  cbParamSet.Items.Add('SLH-DSA-SHA2-192f');
  cbParamSet.Items.Add('SLH-DSA-SHA2-256s');
  cbParamSet.Items.Add('SLH-DSA-SHA2-256f');
  cbParamSet.Items.Add('SLH-DSA-SHAKE-128s');
  cbParamSet.Items.Add('SLH-DSA-SHAKE-128f');
  cbParamSet.Items.Add('SLH-DSA-SHAKE-192s');
  cbParamSet.Items.Add('SLH-DSA-SHAKE-192f');
  cbParamSet.Items.Add('SLH-DSA-SHAKE-256s');
  cbParamSet.Items.Add('SLH-DSA-SHAKE-256f');
  cbParamSet.ItemIndex := 6;

  cbPrehashType.Items.Add('SHA-224');
  cbPrehashType.Items.Add('SHA-256');
  cbPrehashType.Items.Add('SHA-384');
  cbPrehashType.Items.Add('SHA-512');
  cbPrehashType.Items.Add('SHA-512/224');
  cbPrehashType.Items.Add('SHA-512/256');
  cbPrehashType.Items.Add('SHA3-224');
  cbPrehashType.Items.Add('SHA3-256');
  cbPrehashType.Items.Add('SHA3-384');
  cbPrehashType.Items.Add('SHA3-512');
  cbPrehashType.Items.Add('SHAKE128');
  cbPrehashType.Items.Add('SHAKE256');
  cbPrehashType.Items.Add('SM3');
  cbPrehashType.ItemIndex := 1;

  ClearKeyData;
  UpdateStatus('请选择SLH-DSA参数集，然后点击"生成公私钥"按钮');
end;

procedure TSLHDSAMainForm.ClearKeyData;
begin
  FHasKeys := False;
  FreeAndNil(FPublicKey);
  FreeAndNil(FSecretKey);
  MemoPublicKey.Clear;
  MemoPrivateKey.Clear;
  edtSignature.Clear;
  lblKeyInfo.Caption := '';
end;

procedure TSLHDSAMainForm.UpdateStatus(const Msg: string);
begin
  Caption := 'SLH-DSA Demo - ' + Msg;
end;

function TSLHDSAMainForm.GetParamSet: TCnSlhParamSet;
begin
  Result := TCnSlhParamSet(cbParamSet.ItemIndex);
end;

function TSLHDSAMainForm.GetPrehashID: TCnSlhPrehashID;
begin
  case cbPrehashType.ItemIndex of
    0: Result := shiSHA2_224;
    1: Result := shiSHA2_256;
    2: Result := shiSHA2_384;
    3: Result := shiSHA2_512;
    4: Result := shiSHA2_512_224;
    5: Result := shiSHA2_512_256;
    6: Result := shiSHA3_224;
    7: Result := shiSHA3_256;
    8: Result := shiSHA3_384;
    9: Result := shiSHA3_512;
    10: Result := shiSHAKE128;
    11: Result := shiSHAKE256;
    12: Result := shiSM3;
  else
    Result := shiSHA2_256;
  end;
end;

function TSLHDSAMainForm.GetPrehashName(PrehashID: TCnSlhPrehashID): string;
begin
  case PrehashID of
    shiSHA2_224: Result := 'SHA-224';
    shiSHA2_256: Result := 'SHA-256';
    shiSHA2_384: Result := 'SHA-384';
    shiSHA2_512: Result := 'SHA-512';
    shiSHA2_512_224: Result := 'SHA-512/224';
    shiSHA2_512_256: Result := 'SHA-512/256';
    shiSHA3_224: Result := 'SHA3-224';
    shiSHA3_256: Result := 'SHA3-256';
    shiSHA3_384: Result := 'SHA3-384';
    shiSHA3_512: Result := 'SHA3-512';
    shiSHAKE128: Result := 'SHAKE128';
    shiSHAKE256: Result := 'SHAKE256';
    shiSM3: Result := 'SM3';
  else
    Result := 'Unknown';
  end;
end;

procedure TSLHDSAMainForm.FormDestroy(Sender: TObject);
begin
  ClearKeyData;
end;

procedure TSLHDSAMainForm.chkPrehashClick(Sender: TObject);
begin
  cbPrehashType.Enabled := chkPrehash.Checked;
end;

procedure TSLHDSAMainForm.btnGenerateKeysClick(Sender: TObject);
var
  Ctx: TCnSLHDSA;
begin
  try
    ClearKeyData;
    FPublicKey := TCnSlhPublicKey.Create;
    FSecretKey := TCnSlhSecretKey.Create;
    Ctx := TCnSLHDSA.Create(GetParamSet);
    try
      Ctx.GenerateKeys(FPublicKey, FSecretKey);
      FHasKeys := True;

      MemoPublicKey.Text := BytesToHex(Ctx.PublicKeyToBytes(FPublicKey));
      MemoPrivateKey.Text := BytesToHex(Ctx.SecretKeyToBytes(FSecretKey));
      lblKeyInfo.Caption := Format('PK: %d bytes, SK: %d bytes, Sig: %d bytes',
        [Ctx.PublicKeySize, Ctx.SecretKeySize, Ctx.SignatureSize]);

      UpdateStatus(Format('密钥生成成功，参数集：%s', [SlhParamSetName(GetParamSet)]));
    finally
      Ctx.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('生成密钥失败：' + E.Message);
      UpdateStatus('密钥生成失败');
    end;
  end;
end;

procedure TSLHDSAMainForm.btnSignClick(Sender: TObject);
var
  Ctx: TCnSLHDSA;
  MsgBytes, SigBytes: TBytes;
  Randomize: Boolean;
  PrehashID: TCnSlhPrehashID;
begin
  try
    if not FHasKeys then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;

    if Trim(edtMessage.Text) = '' then
    begin
      ShowMessage('请输入需要签名的消息');
      Exit;
    end;

    Ctx := TCnSLHDSA.Create(GetParamSet);
    try
      MsgBytes := AnsiToBytes(Trim(edtMessage.Text));
      Randomize := chkRandomize.Checked;

      if chkPrehash.Checked then
      begin
        PrehashID := GetPrehashID;
        SigBytes := Ctx.SignPreHash(MsgBytes, FSecretKey, PrehashID, Randomize);
        edtSignature.Text := BytesToHex(SigBytes);
        UpdateStatus(Format('预哈希签名成功，哈希类型：%s', [GetPrehashName(PrehashID)]));
      end
      else
      begin
        SigBytes := Ctx.SignBytes(FSecretKey, MsgBytes, Randomize);
        edtSignature.Text := BytesToHex(SigBytes);
        UpdateStatus('签名成功（标准模式）');
      end;
    finally
      Ctx.Free;
    end;
  except
    on E: Exception do
    begin
      ClearKeyData;
      ShowMessage('签名失败：' + E.Message);
      UpdateStatus('签名失败');
    end;
  end;
end;

procedure TSLHDSAMainForm.btnVerifyClick(Sender: TObject);
var
  Ctx: TCnSLHDSA;
  MsgBytes, SigBytes: TBytes;
  IsValid: Boolean;
  PrehashID: TCnSlhPrehashID;
begin
  try
    if not FHasKeys then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;

    if Trim(edtMessage.Text) = '' then
    begin
      ShowMessage('请输入需要验证签名的消息');
      Exit;
    end;

    if Trim(edtSignature.Text) = '' then
    begin
      ShowMessage('请先进行签名操作');
      Exit;
    end;

    Ctx := TCnSLHDSA.Create(GetParamSet);
    try
      MsgBytes := AnsiToBytes(Trim(edtMessage.Text));
      SigBytes := HexToBytes(Trim(edtSignature.Text));

      if chkPrehash.Checked then
      begin
        PrehashID := GetPrehashID;
        IsValid := Ctx.VerifyPreHash(MsgBytes, SigBytes, FPublicKey, PrehashID);
      end
      else
      begin
        IsValid := Ctx.VerifyBytes(FPublicKey, MsgBytes, SigBytes);
      end;

      if IsValid then
      begin
        ShowMessage('签名验证成功！');
        UpdateStatus('签名验证成功');
      end
      else
      begin
        ShowMessage('签名验证失败！');
        UpdateStatus('签名验证失败');
      end;
    finally
      Ctx.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('验证签名失败：' + E.Message);
      UpdateStatus('验证签名失败');
    end;
  end;
end;

end.
