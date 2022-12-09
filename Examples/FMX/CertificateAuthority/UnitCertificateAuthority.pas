unit UnitCertificateAuthority;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnECC, CnRSA, CnCertificateAuthority, FMX.ComboEdit, FMX.Edit, FMX.Memo, FMX.TabControl, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormCA = class(TForm)
    pgc1: TTabControl;
    tsRequest: TTabItem;
    grpGenRequest: TGroupBox;
    lblKey: TLabel;
    lblContryName: TLabel;
    lblStateOrProvinceName: TLabel;
    lblLocalityName: TLabel;
    lblOrgName: TLabel;
    lblOrgUnitName: TLabel;
    lblCommonName: TLabel;
    lblEmail: TLabel;
    lblHash: TLabel;
    edtRSAECCKey: TEdit;
    btnBrowseKey: TButton;
    edtContryName: TEdit;
    edtStateOrProvinceName: TEdit;
    edtLocalityName: TEdit;
    edtOrgName: TEdit;
    edtOrgUnitName: TEdit;
    edtCommonName: TEdit;
    edtEmail: TEdit;
    cbbHash: TComboEdit;
    btnGenerateCSR: TButton;
    btnSelfSign: TButton;
    grpParse: TGroupBox;
    lblCSR: TLabel;
    edtCSR: TEdit;
    btnBrowseCSR: TButton;
    mmoCSRParse: TMemo;
    btnParseCSR: TButton;
    btnVerifyCSR: TButton;
    tsSign: TTabItem;
    grpSign: TGroupBox;
    lblSignCSR: TLabel;
    lblRoot: TLabel;
    lblRootCrt: TLabel;
    edtSignCSR: TEdit;
    btnSignCSRBrowse: TButton;
    edtSignKey: TEdit;
    btnSignKeyBrowse: TButton;
    btnSign: TButton;
    edtRootCRT: TEdit;
    btnRootCRTBrowse: TButton;
    grpParseCER: TGroupBox;
    lblCRT: TLabel;
    edtCRT: TEdit;
    btnBrowseCRT: TButton;
    mmoCRT: TMemo;
    btnParseCRT: TButton;
    btnVerifySelfSignedCRT: TButton;
    btnVerifyCRT: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseKeyClick(Sender: TObject);
    procedure btnGenerateCSRClick(Sender: TObject);
    procedure btnSelfSignClick(Sender: TObject);
    procedure btnBrowseCSRClick(Sender: TObject);
    procedure btnParseCSRClick(Sender: TObject);
    procedure btnVerifyCSRClick(Sender: TObject);
    procedure btnSignCSRBrowseClick(Sender: TObject);
    procedure btnSignKeyBrowseClick(Sender: TObject);
    procedure btnSignClick(Sender: TObject);
    procedure btnRootCRTBrowseClick(Sender: TObject);
    procedure btnBrowseCRTClick(Sender: TObject);
    procedure btnParseCRTClick(Sender: TObject);
    procedure btnVerifySelfSignedCRTClick(Sender: TObject);
    procedure btnVerifyCRTClick(Sender: TObject);
  private
    FClientRsaPriv: TCnRSAPrivateKey;
    FClientRsaPub: TCnRSAPublicKey;
    FServerRsaPriv: TCnRSAPrivateKey;
    FServerRsaPub: TCnRSAPublicKey;
    FClientEccPriv: TCnEccPrivateKey;
    FClientEccPub: TCnEccPublicKey;
    FClientCurveType: TCnEccCurveType;
    FServerEccPriv: TCnEccPrivateKey;
    FServerEccPub: TCnEccPublicKey;
    FServerCurveType: TCnEccCurveType;
  public
    { Public declarations }
  end;

var
  FormCA: TFormCA;

implementation

{$R *.fmx}

function PrintHex(const Buf: Pointer; Len: Integer): string;
var
  I: Integer;
  P: PByteArray;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  P := PByteArray(Buf);
  for I := 0 to Len - 1 do
  begin
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(P[I] shr 4) and $0F] +
              Digits[P[I] and $0F]);
  end;
end;

procedure TFormCA.FormCreate(Sender: TObject);
begin
  cbbHash.ItemIndex := 1;
  FClientRsaPriv := TCnRSAPrivateKey.Create;
  FClientRsaPub := TCnRSAPublicKey.Create;
  FServerRsaPriv := TCnRSAPrivateKey.Create;
  FServerRsaPub := TCnRSAPublicKey.Create;
  FClientEccPriv := TCnEccPrivateKey.Create;
  FClientEccPub := TCnEccPublicKey.Create;
  FServerEccPriv := TCnEccPrivateKey.Create;
  FServerEccPub := TCnEccPublicKey.Create;
end;

procedure TFormCA.btnBrowseCSRClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnBrowseKeyClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRSAECCKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCSRClick(Sender: TObject);
var
  CSR: TCnCertificateRequest;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  CSR := TCnCertificateRequest.Create;
  if CnCALoadCertificateSignRequestFromFile(edtCSR.Text, CSR) then
  begin
    mmoCSRParse.Lines.Clear;
    mmoCSRParse.Lines.Add(CSR.ToString);

    if CSR.IsRSA and (CSR.SignValue <> nil) and (CSR.SignLength > 0) and (CSR.RSAPublicKey.BitsCount > 128) then
    begin
      SetLength(OutBuf, CSR.RSAPublicKey.BitsCount div 8);
      if CnRSADecryptRawData(CSR.SignValue, CSR.SignLength, @OutBuf[0], OutLen, CSR.RSAPublicKey) then
      begin
        mmoCSRParse.Lines.Add('');
        mmoCSRParse.Lines.Add('--------');
        mmoCSRParse.Lines.Add('Digest after RSA Decryption:');
        mmoCSRParse.Lines.Add(PrintHex(@OutBuf[0], OutLen));
      end;
    end;
  end
  else
    ShowMessage('Parse CSR Failed.');
  CSR.Free;
end;

function ConvertItemIndexToCASignType(ItemIndex: Integer; IsRSA: Boolean): TCnCASignType;
begin
  if IsRSA then
    Result := TCnCASignType(ItemIndex)
  else
    Result := TCnCASignType(ItemIndex + 3);
end;

procedure TFormCA.btnGenerateCSRClick(Sender: TObject);
begin
  if FileExists(edtRSAECCKey.Text) then
  begin
    if CnRSALoadKeysFromPem(edtRSAECCKey.Text, FClientRsaPriv, FClientRsaPub) then
    begin
      if dlgSave.Execute then
      begin
        if CnCANewCertificateSignRequest(FClientRsaPriv, FClientRsaPub, dlgSave.FileName, edtContryName.Text,
          edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text, edtOrgUnitName.Text,
          edtCommonName.Text, edtEmail.Text, ConvertItemIndexToCASignType(cbbHash.ItemIndex, True)) then
          ShowMessage('Generate RSA CSR File Success.')
        else
          ShowMessage('Generate RSA CSR File Fail.');
      end;
    end
    else if CnEccLoadKeysFromPem(edtRSAECCKey.Text, FClientEccPriv, FClientEccPub, FClientCurveType) then
    begin
      if dlgSave.Execute then
      begin
        if CnCANewCertificateSignRequest(FClientEccPriv, FClientEccPub, FClientCurveType,
          dlgSave.FileName, edtContryName.Text, edtStateOrProvinceName.Text,
          edtLocalityName.Text, edtOrgName.Text, edtOrgUnitName.Text, edtCommonName.Text,
          edtEmail.Text, ConvertItemIndexToCASignType(cbbHash.ItemIndex, False)) then
          ShowMessage('Generate ECC CSR File Success.')
        else
          ShowMessage('Generate ECC CSR File Fail.');
      end;
    end;
  end
  else
    ShowMessage('Invalid RSA or ECC Keys');
end;

procedure TFormCA.FormDestroy(Sender: TObject);
begin
  FClientEccPriv.Free;
  FClientEccPub.Free;
  FServerEccPriv.Free;
  FServerEccPub.Free;

  FServerRsaPub.Free;
  FServerRsaPriv.Free;
  FClientRsaPub.Free;
  FClientRsaPriv.Free;
end;

procedure TFormCA.btnBrowseCRTClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCRT.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCRTClick(Sender: TObject);
var
  CRT: TCnCertificate;
begin
  CRT := TCnCertificate.Create;
  if not CnCALoadCertificateFromFile(edtCRT.Text, CRT) then
    ShowMessage('Parse CRT File Failed.')
  else
  begin
    mmoCRT.Lines.Clear;
    mmoCRT.Lines.Add(CRT.ToString);
  end;
  CRT.Free;
end;

procedure TFormCA.btnVerifyCSRClick(Sender: TObject);
begin
  if CnCAVerifyCertificateSignRequestFile(edtCSR.Text) then
    ShowMessage('CSR Verify OK.')
  else
    ShowMessage('CSR Verify Fail.');
end;

procedure TFormCA.btnSelfSignClick(Sender: TObject);
begin
  if FileExists(edtRSAECCKey.Text) then
  begin
    if CnRSALoadKeysFromPem(edtRSAECCKey.Text, FClientRsaPriv, FClientRsaPub) then
    begin
      if dlgSave.Execute then
      begin
        if CnCANewSelfSignedCertificate(FClientRsaPriv, FClientRsaPub, dlgSave.FileName, edtContryName.Text,
          edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text,
          edtOrgUnitName.Text, edtCommonName.Text, edtEmail.Text, '1234567890987654321',
          Now - 1, Now + 365, ConvertItemIndexToCASignType(cbbHash.ItemIndex, True)) then
          ShowMessage('Self-Signed RSA CRT File OK.')
        else
          ShowMessage('Self-Signed RSA CRT File Fail.');
      end;
    end
    else
    begin
      if CnEccLoadKeysFromPem(edtRSAECCKey.Text, FClientEccPriv, FClientEccPub, FClientCurveType) then
      begin
        if dlgSave.Execute then
        begin
          if CnCANewSelfSignedCertificate(FClientEccPriv, FClientEccPub, FClientCurveType, dlgSave.FileName,
            edtContryName.Text, edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text,
            edtOrgUnitName.Text, edtCommonName.Text, edtEmail.Text, '1234567890987654321',
            Now - 1, Now + 365, ConvertItemIndexToCASignType(cbbHash.ItemIndex, False)) then
            ShowMessage('Self-Signed ECC CRT File OK.')
          else
            ShowMessage('Self-Signed ECC CRT File Fail.');
        end;
      end;
    end;
  end;
end;

procedure TFormCA.btnVerifySelfSignedCRTClick(Sender: TObject);
begin
  if CnCAVerifySelfSignedCertificateFile(edtCRT.Text) then
    ShowMessage('Self-Signed CRT Verify OK.')
  else
    ShowMessage('Self-Signed CRT Verify Fail.');
end;

procedure TFormCA.btnSignClick(Sender: TObject);
begin
  if FileExists(edtSignCSR.Text) and FileExists(edtRootCRT.Text) and FileExists(edtSignKey.Text) then
  begin
    if CnRSALoadKeysFromPem(edtSignKey.Text, FServerRsaPriv, FServerRsaPub) then
    begin
      if dlgSave.Execute then
      begin
        if CnCASignCertificate(FServerRsaPriv, edtRootCRT.Text, edtSignCSR.Text, dlgSave.FileName,
          '1234567890987654321', Now - 1, Now + 365, TCnCASignType(cbbHash.ItemIndex)) then
          ShowMessage('Sign CRT File OK.')
        else
          ShowMessage('Sign CRT File Fail.');
      end;
    end
    else if CnEccLoadKeysFromPem(edtSignKey.Text, FServerEccPriv, FServerEccPub, FServerCurveType) then
    begin
      if dlgSave.Execute then
      begin
        if CnCASignCertificate(FServerEccPriv, FServerCurveType, edtRootCRT.Text, edtSignCSR.Text, dlgSave.FileName,
          '1234567890987654321', Now - 1, Now + 365, TCnCASignType(cbbHash.ItemIndex + 3)) then
          ShowMessage('Sign CRT File OK.')
        else
          ShowMessage('Sign CRT File Fail.');
      end;
    end;
  end;
end;

procedure TFormCA.btnSignCSRBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtSignCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnSignKeyBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtSignKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnRootCRTBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRootCRT.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnVerifyCRTClick(Sender: TObject);
var
  ParentCRT: TCnCertificate;
  RSAPub: TCnRSAPublicKey;
  EccPub: TCnEccPublicKey;
  CurveType: TCnEccCurveType;
begin
  if FileExists(edtCRT.Text) and dlgOpen.Execute then
  begin
    ParentCRT := nil;
    RSAPub := nil;
    EccPub := nil;

    try
      // 读 Parent CRT 的被签发者信息、或 ECC/RSA 的父公钥
      ParentCRT := TCnCertificate.Create;
      RSAPub := TCnRSAPublicKey.Create;
      EccPub := TCnEccPublicKey.Create;

      if CnCALoadCertificateFromFile(dlgOpen.FileName, ParentCRT) then  // 是父证书
      begin
        if ParentCRT.BasicCertificate.SubjectIsRSA then
        begin
          if CnCAVerifyCertificateFile(edtCRT.Text, ParentCRT.BasicCertificate.SubjectRSAPublicKey) then
            ShowMessage('Verify CRT using RSA Parent CRT OK.')
          else
            ShowMessage('Verify CRT using RSA Parent CRT Fail.')
        end
        else
        begin
          if CnCAVerifyCertificateFile(edtCRT.Text, ParentCRT.BasicCertificate.SubjectEccPublicKey,
            ParentCRT.BasicCertificate.SubjectEccCurveType) then
            ShowMessage('Verify CRT using Ecc Parent CRT OK.')
          else
            ShowMessage('Verify CRT using Ecc Parent CRT Fail.');
        end;
      end
      else if CnRSALoadPublicKeyFromPem(dlgOpen.FileName, RSAPub) or  
        CnRSALoadKeysFromPem(dlgOpen.FileName, nil ,RSAPub) then        // 是父 RSA 公钥
      begin
        if CnCAVerifyCertificateFile(edtCRT.Text, RSAPub) then
            ShowMessage('Verify CRT using RSA Parent Public Key OK.')
          else
            ShowMessage('Verify CRT using RSA Parent Public Key Fail.');
      end
      else if CnEccLoadKeysFromPem(dlgOpen.FileName, nil, EccPub, CurveType) or
        CnEccLoadPublicKeyFromPem(dlgOpen.FileName, EccPub, CurveType) then  // 是父 ECC 公钥
      begin
        if CnCAVerifyCertificateFile(edtCRT.Text, EccPub, CurveType) then
            ShowMessage('Verify CRT using ECC Parent Public Key OK.')
          else
            ShowMessage('Verify CRT using ECC Parent Public Key Fail.');
      end;
    finally
      EccPub.Free;
      RSAPub.Free;
      ParentCRT.Free;
    end;
  end;
end;

end.
