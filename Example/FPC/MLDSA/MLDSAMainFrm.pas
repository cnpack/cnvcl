unit MLDSAMainFrm;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, CnMLDSA, CnNative;

type
  TMLDSAMainForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    cbMLDSAType: TComboBox;
    btnGenerateKeys: TButton;
    Label2: TLabel;
    edtRandomSeed: TEdit;
    MemoPublicKey: TMemo;
    Label3: TLabel;
    MemoPrivateKey: TMemo;
    Label4: TLabel;
    Panel2: TPanel;
    Label6: TLabel;
    edtMessage: TMemo;
    btnSign: TButton;
    Label7: TLabel;
    edtSignature: TMemo;
    btnVerify: TButton;
    Label5: TLabel;
    cbHashType: TComboBox;
    procedure btnGenerateKeysClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnVerifyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPublicKey: TBytes;
    FPrivateKey: TBytes;
    procedure ClearKeyData;
    procedure UpdateStatus(const Msg: string);
    function GetMLDSAType: TCnMLDSAType;
    function GetHashType: TCnMLDSAHashType;
    function GetHashTypeName(HashType: TCnMLDSAHashType): string;
  public
    { Public declarations }
  end;

var
  MLDSAMainForm: TMLDSAMainForm;

implementation

{$R *.lfm}

procedure TMLDSAMainForm.FormCreate(Sender: TObject);
begin
  // 初始化MLDSA类型下拉框
  cbMLDSAType.Items.Add('MLDSA-44');
  cbMLDSAType.Items.Add('MLDSA-65');
  cbMLDSAType.Items.Add('MLDSA-87');
  cbMLDSAType.ItemIndex := 0;

  // 初始化哈希类型下拉框
  cbHashType.Items.Add('None');
  cbHashType.Items.Add('SHA-224');
  cbHashType.Items.Add('SHA-256');
  cbHashType.Items.Add('SHA-384');
  cbHashType.Items.Add('SHA-512');
  cbHashType.Items.Add('SHA-512/224');
  cbHashType.Items.Add('SHA-512/256');
  cbHashType.Items.Add('SHA3-224');
  cbHashType.Items.Add('SHA3-256');
  cbHashType.Items.Add('SHA3-384');
  cbHashType.Items.Add('SHA3-512');
  cbHashType.Items.Add('SHAKE128');
  cbHashType.Items.Add('SHAKE256');
  cbHashType.Items.Add('SM3');
  cbHashType.ItemIndex := 0;

  ClearKeyData;
  UpdateStatus('请先选择MLDSA类型和哈希类型，然后点击"生成公私钥"按钮');
end;

procedure TMLDSAMainForm.ClearKeyData;
begin
  SetLength(FPublicKey, 0);
  SetLength(FPrivateKey, 0);
  MemoPublicKey.Clear;
  MemoPrivateKey.Clear;
  edtMessage.Clear;
  edtSignature.Clear;
end;

procedure TMLDSAMainForm.UpdateStatus(const Msg: string);
begin
  Caption := 'MLDSA Demo - ' + Msg;
end;

function TMLDSAMainForm.GetMLDSAType: TCnMLDSAType;
begin
  case cbMLDSAType.ItemIndex of
    0: Result := cmdt44;
    1: Result := cmdt65;
    2: Result := cmdt87;
  else
    Result := cmdt44;
  end;
end;

function TMLDSAMainForm.GetHashType: TCnMLDSAHashType;
begin
  case cbHashType.ItemIndex of
    0: Result := cmhtNone;
    1: Result := cmhtSHA224;
    2: Result := cmhtSHA256;
    3: Result := cmhtSHA384;
    4: Result := cmhtSHA512;
    5: Result := cmhtSHA512_224;
    6: Result := cmhtSHA512_256;
    7: Result := cmhtSHA3_224;
    8: Result := cmhtSHA3_256;
    9: Result := cmhtSHA3_384;
    10: Result := cmhtSHA3_512;
    11: Result := cmhtSHAKE128;
    12: Result := cmhtSHAKE256;
    13: Result := cmhtSM3;
  else
    Result := cmhtNone;
  end;
end;

function TMLDSAMainForm.GetHashTypeName(HashType: TCnMLDSAHashType): string;
begin
  case HashType of
    cmhtNone: Result := 'None';
    cmhtSHA224: Result := 'SHA-224';
    cmhtSHA256: Result := 'SHA-256';
    cmhtSHA384: Result := 'SHA-384';
    cmhtSHA512: Result := 'SHA-512';
    cmhtSHA512_224: Result := 'SHA-512/224';
    cmhtSHA512_256: Result := 'SHA-512/256';
    cmhtSHA3_224: Result := 'SHA3-224';
    cmhtSHA3_256: Result := 'SHA3-256';
    cmhtSHA3_384: Result := 'SHA3-384';
    cmhtSHA3_512: Result := 'SHA3-512';
    cmhtSHAKE128: Result := 'SHAKE128';
    cmhtSHAKE256: Result := 'SHAKE256';
    cmhtSM3: Result := 'SM3';
  else
    Result := 'Unknown';
  end;
end;

procedure TMLDSAMainForm.btnGenerateKeysClick(Sender: TObject);
var
  MLDSA: TCnMLDSA;
  PrivateKey: TCnMLDSAPrivateKey;
  PublicKey: TCnMLDSAPublicKey;
  RandHex: string;
begin
  try
    ClearKeyData;
    
    // 创建MLDSA实例
    MLDSA := TCnMLDSA.Create(GetMLDSAType);
    try
      PrivateKey := TCnMLDSAPrivateKey.Create;
      PublicKey := TCnMLDSAPublicKey.Create;
      try
        RandHex := Trim(edtRandomSeed.Text);
        
        // 生成密钥对
        if RandHex = '' then
        begin
          MLDSA.GenerateKeys(PrivateKey, PublicKey);
          UpdateStatus('密钥生成成功（使用随机种子）');
        end
        else
        begin
          if Length(RandHex) <> 64 then
          begin
            ShowMessage('随机数种子必须是64字符的十六进制字符串');
            Exit;
          end;
          MLDSA.GenerateKeys(PrivateKey, PublicKey, RandHex);
          UpdateStatus('密钥生成成功（使用指定种子）');
        end;
        
        // 保存密钥
        FPublicKey := MLDSA.SavePublicKeyToBytes(PublicKey);
        FPrivateKey := MLDSA.SavePrivateKeyToBytes(PrivateKey);
        
        // 显示密钥
        MemoPublicKey.Text := BytesToHex(FPublicKey);
        MemoPrivateKey.Text := BytesToHex(FPrivateKey);
        
      finally
        PrivateKey.Free;
        PublicKey.Free;
      end;
      
    finally
      MLDSA.Free;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('生成密钥失败：' + E.Message);
      UpdateStatus('密钥生成失败');
    end;
  end;
end;

procedure TMLDSAMainForm.btnSignClick(Sender: TObject);
var
  MLDSA: TCnMLDSA;
  PrivateKey: TCnMLDSAPrivateKey;
  MessageBytes, SignatureBytes: TBytes;
begin
  try
    if Length(FPrivateKey) = 0 then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;
    
    if Trim(edtMessage.Text) = '' then
    begin
      ShowMessage('请输入要签名的内容');
      Exit;
    end;
    
    // 创建MLDSA实例
    MLDSA := TCnMLDSA.Create(GetMLDSAType);
    try
      PrivateKey := TCnMLDSAPrivateKey.Create;
      try
        // 加载私钥
        MLDSA.LoadPrivateKeyFromBytes(PrivateKey, FPrivateKey);
        
        // 获取消息字节
        MessageBytes := AnsiToBytes(Trim(edtMessage.Text));
        
        // 签名
        SignatureBytes := MLDSA.SignBytes(PrivateKey, MessageBytes, '', GetHashType);
        
        // 显示签名结果
        edtSignature.Text := BytesToHex(SignatureBytes);
        UpdateStatus('签名成功（使用' + GetHashTypeName(GetHashType) + '）');
        
      finally
        PrivateKey.Free;
      end;
      
    finally
      MLDSA.Free;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('签名失败：' + E.Message);
      UpdateStatus('签名失败');
    end;
  end;
end;

procedure TMLDSAMainForm.btnVerifyClick(Sender: TObject);
var
  MLDSA: TCnMLDSA;
  PublicKey: TCnMLDSAPublicKey;
  MessageBytes, SignatureBytes: TBytes;
  IsValid: Boolean;
begin
  try
    if Length(FPublicKey) = 0 then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;
    
    if Trim(edtMessage.Text) = '' then
    begin
      ShowMessage('请输入要验证签名的内容');
      Exit;
    end;
    
    if Trim(edtSignature.Text) = '' then
    begin
      ShowMessage('请先进行签名操作');
      Exit;
    end;
    
    // 创建MLDSA实例
    MLDSA := TCnMLDSA.Create(GetMLDSAType);
    try
      PublicKey := TCnMLDSAPublicKey.Create;
      try
        // 加载公钥
        MLDSA.LoadPublicKeyFromBytes(PublicKey, FPublicKey);
        
        // 获取消息字节
        MessageBytes := AnsiToBytes(Trim(edtMessage.Text));

        // 获取签名字节
        SignatureBytes := HexToBytes(Trim(edtSignature.Text));
        
        // 验证签名
        IsValid := MLDSA.VerifyBytes(PublicKey, MessageBytes, SignatureBytes, '', GetHashType);
        
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
        PublicKey.Free;
      end;
      
    finally
      MLDSA.Free;
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
