unit MLKEMMainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Math, CnNative;

type
  TMLKEMMainForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    cbMLKEMType: TComboBox;
    btnGenerateKeys: TButton;
    Label2: TLabel;
    edtRandomSeed: TEdit;
    MemoPublicKey: TMemo;
    Label3: TLabel;
    MemoPrivateKey: TMemo;
    Label4: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label5: TLabel;
    edtPlainMessage: TEdit;
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    MemoEncrypted: TMemo;
    MemoDecrypted: TMemo;
    Label6: TLabel;
    Label7: TLabel;
    btnEncaps: TButton;
    btnDecaps: TButton;
    MemoSharedKey1: TMemo;
    MemoCipherText: TMemo;
    MemoSharedKey2: TMemo;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure btnGenerateKeysClick(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnEncapsClick(Sender: TObject);
    procedure btnDecapsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FEnKey: TBytes;
    FDeKey: TBytes;
    procedure ClearKeyData;
    procedure UpdateStatus(const Msg: string);
  public
    { Public declarations }
  end;

var
  MLKEMMainForm: TMLKEMMainForm;

implementation

{$R *.lfm}

uses
  CnMLKEM;

procedure TMLKEMMainForm.FormCreate(Sender: TObject);
begin
  cbMLKEMType.Items.Add('MLKEM-512');
  cbMLKEMType.Items.Add('MLKEM-768');
  cbMLKEMType.Items.Add('MLKEM-1024');
  cbMLKEMType.ItemIndex := 0;
  ClearKeyData;
  UpdateStatus('请先选择MLKEM类型，然后点击"生成公私钥"按钮');
end;

procedure TMLKEMMainForm.ClearKeyData;
begin
  SetLength(FEnKey, 0);
  SetLength(FDeKey, 0);
  MemoPublicKey.Clear;
  MemoPrivateKey.Clear;
  MemoEncrypted.Clear;
  MemoDecrypted.Clear;
  MemoSharedKey1.Clear;
  MemoSharedKey2.Clear;
  MemoCipherText.Clear;
  edtPlainMessage.Text := '';
end;

procedure TMLKEMMainForm.UpdateStatus(const Msg: string);
begin
  Caption := 'MLKEM Demo - ' + Msg;
end;

procedure TMLKEMMainForm.btnGenerateKeysClick(Sender: TObject);
var
  MLKEM: TCnMLKEM;
  MLKEMType: TCnMLKEMType;
  RandHex: string;
begin
  try
    ClearKeyData;
    
    // 获取选择的MLKEM类型
    case cbMLKEMType.ItemIndex of
      0: MLKEMType := cmkt512;
      1: MLKEMType := cmkt768;
      2: MLKEMType := cmkt1024;
    else
      MLKEMType := cmkt512;
    end;
    
    // 创建MLKEM实例
    MLKEM := TCnMLKEM.Create(MLKEMType);
    try
      RandHex := Trim(edtRandomSeed.Text);
      
      // 生成密钥对
      if RandHex = '' then
      begin
        MLKEM.GenerateKeys(FEnKey, FDeKey);
        UpdateStatus('密钥生成成功（使用随机种子）');
      end
      else
      begin
        if Length(RandHex) <> 64 then
        begin
          ShowMessage('随机数种子必须是64字符的十六进制字符串');
          Exit;
        end;
        MLKEM.GenerateKeys(FEnKey, FDeKey, RandHex);
        UpdateStatus('密钥生成成功（使用指定种子）');
      end;
      
      // 显示密钥
      MemoPublicKey.Text := BytesToHex(FEnKey);
      MemoPrivateKey.Text := BytesToHex(FDeKey);
      
    finally
      MLKEM.Free;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('生成密钥失败：' + E.Message);
      UpdateStatus('密钥生成失败');
    end;
  end;
end;

procedure TMLKEMMainForm.btnEncryptClick(Sender: TObject);
var
  MLKEM: TCnMLKEM;
  MLKEMType: TCnMLKEMType;
  PlainBytes, CipherBytes: TBytes;
  PlainText: string;
begin
  try
    if Length(FEnKey) = 0 then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;
    
    // 获取选择的MLKEM类型
    case cbMLKEMType.ItemIndex of
      0: MLKEMType := cmkt512;
      1: MLKEMType := cmkt768;
      2: MLKEMType := cmkt1024;
    else
      MLKEMType := cmkt512;
    end;
    
    // 获取明文（转换为32字节）
    PlainText := Trim(edtPlainMessage.Text);
    if PlainText = '' then
      PlainText := 'MLKEM Test Message';
      
    PlainBytes := AnsiToBytes(StringOfChar(#0, 32)); // 32字节的零
    Move(PlainText[1], PlainBytes[0], Min(Length(PlainText), 32));
    
    // 创建MLKEM实例并加密
    MLKEM := TCnMLKEM.Create(MLKEMType);
    try
      CipherBytes := MLKEM.MLKEMEncrypt(FEnKey, PlainBytes);
      MemoEncrypted.Text := BytesToHex(CipherBytes);
      UpdateStatus('加密成功');
    finally
      MLKEM.Free;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('加密失败：' + E.Message);
      UpdateStatus('加密失败');
    end;
  end;
end;

procedure TMLKEMMainForm.btnDecryptClick(Sender: TObject);
var
  MLKEM: TCnMLKEM;
  MLKEMType: TCnMLKEMType;
  CipherBytes, PlainBytes: TBytes;
begin
  try
    if Length(FDeKey) = 0 then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;
    
    if MemoEncrypted.Text = '' then
    begin
      ShowMessage('请先加密数据');
      Exit;
    end;
    
    // 获取选择的MLKEM类型
    case cbMLKEMType.ItemIndex of
      0: MLKEMType := cmkt512;
      1: MLKEMType := cmkt768;
      2: MLKEMType := cmkt1024;
    else
      MLKEMType := cmkt512;
    end;
    
    // 获取密文
    CipherBytes := HexToBytes(MemoEncrypted.Text);
    
    // 创建MLKEM实例并解密
    MLKEM := TCnMLKEM.Create(MLKEMType);
    try
      PlainBytes := MLKEM.MLKEMDecrypt(FDeKey, CipherBytes);
      
      // 显示解密结果（去除尾部零字符）
      MemoDecrypted.Text := PChar(@PlainBytes[0]);
      UpdateStatus('解密成功');
    finally
      MLKEM.Free;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('解密失败：' + E.Message);
      UpdateStatus('解密失败');
    end;
  end;
end;

procedure TMLKEMMainForm.btnEncapsClick(Sender: TObject);
var
  MLKEM: TCnMLKEM;
  MLKEMType: TCnMLKEMType;
  MsgBytes, ShareKey, CipherText: TBytes;
begin
  try
    if Length(FEnKey) = 0 then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;
    
    // 获取选择的MLKEM类型
    case cbMLKEMType.ItemIndex of
      0: MLKEMType := cmkt512;
      1: MLKEMType := cmkt768;
      2: MLKEMType := cmkt1024;
    else
      MLKEMType := cmkt512;
    end;
    
    // 创建随机消息（32字节）
    SetLength(MsgBytes, 32);
    FillChar(MsgBytes[0], 32, 0);
    MsgBytes[0] := $12; // 示例数据
    
    // 创建MLKEM实例并封装
    MLKEM := TCnMLKEM.Create(MLKEMType);
    try
      MLKEM.MLKEMEncaps(FEnKey, MsgBytes, ShareKey, CipherText);
      MemoSharedKey1.Text := BytesToHex(ShareKey);
      MemoCipherText.Text := BytesToHex(CipherText);
      UpdateStatus('密钥封装成功');
    finally
      MLKEM.Free;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('密钥封装失败：' + E.Message);
      UpdateStatus('密钥封装失败');
    end;
  end;
end;

procedure TMLKEMMainForm.btnDecapsClick(Sender: TObject);
var
  MLKEM: TCnMLKEM;
  MLKEMType: TCnMLKEMType;
  CipherBytes, ShareKey: TBytes;
begin
  try
    if Length(FDeKey) = 0 then
    begin
      ShowMessage('请先生成公私钥');
      Exit;
    end;
    
    if MemoCipherText.Text = '' then
    begin
      ShowMessage('请先执行密钥封装');
      Exit;
    end;
    
    // 获取选择的MLKEM类型
    case cbMLKEMType.ItemIndex of
      0: MLKEMType := cmkt512;
      1: MLKEMType := cmkt768;
      2: MLKEMType := cmkt1024;
    else
      MLKEMType := cmkt512;
    end;
    
    // 获取密文
    CipherBytes := HexToBytes(MemoCipherText.Text);
    
    // 创建MLKEM实例并解封
    MLKEM := TCnMLKEM.Create(MLKEMType);
    try
      ShareKey := MLKEM.MLKEMDecaps(FDeKey, CipherBytes);
      MemoSharedKey2.Text := BytesToHex(ShareKey);
      UpdateStatus('密钥解封成功');
      
      // 检查两次生成的共享密钥是否相同
      if (MemoSharedKey1.Text <> '') and (MemoSharedKey1.Text = MemoSharedKey2.Text) then
        ShowMessage('共享密钥验证成功！')
      else if MemoSharedKey1.Text <> '' then
        ShowMessage('警告：共享密钥不匹配！');
    finally
      MLKEM.Free;
    end;
    
  except
    on E: Exception do
    begin
      ShowMessage('密钥解封失败：' + E.Message);
      UpdateStatus('密钥解封失败');
    end;
  end;
end;

end.
