unit UnitCertificateAuthority;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CnRSA, CnCertificateAuthority;

type
  TFormCA = class(TForm)
    pgc1: TPageControl;
    tsRequest: TTabSheet;
    grpGenRequest: TGroupBox;
    lblKey: TLabel;
    edtRSAKey: TEdit;
    btnBrowseKey: TButton;
    lblContryName: TLabel;
    edtContryName: TEdit;
    lblStateOrProvinceName: TLabel;
    edtStateOrProvinceName: TEdit;
    lblLocalityName: TLabel;
    edtLocalityName: TEdit;
    lblOrgName: TLabel;
    edtOrgName: TEdit;
    lblOrgUnitName: TLabel;
    edtOrgUnitName: TEdit;
    lblCommonName: TLabel;
    edtCommonName: TEdit;
    edtEmail: TEdit;
    lblEmail: TLabel;
    lblHash: TLabel;
    cbbHash: TComboBox;
    btnGenerateCSR: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    grpParse: TGroupBox;
    lblCSR: TLabel;
    edtCSR: TEdit;
    btnBrowseCSR: TButton;
    mmoCSRParse: TMemo;
    tsSign: TTabSheet;
    grpSign: TGroupBox;
    lblSignCSR: TLabel;
    edtSignCSR: TEdit;
    btnSignCSRBrowse: TButton;
    lblRoot: TLabel;
    edtSignKey: TEdit;
    btnSignKeyBrowse: TButton;
    btnSign: TButton;
    grpParseCER: TGroupBox;
    lblCRT: TLabel;
    edtCRT: TEdit;
    btnBrowseCRT: TButton;
    mmoCRT: TMemo;
    lblRootCrt: TLabel;
    edtRootCRT: TEdit;
    btnRootCRTBrowse: TButton;
    btnSelfSign: TButton;
    btnParseCSR: TButton;
    btnParseCRT: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseCSRClick(Sender: TObject);
    procedure btnBrowseKeyClick(Sender: TObject);
    procedure btnParseCSRClick(Sender: TObject);
    procedure btnGenerateCSRClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseCRTClick(Sender: TObject);
    procedure btnParseCRTClick(Sender: TObject);
  private
    FCPriv: TCnRSAPrivateKey;
    FCPub: TCnRSAPublicKey;
  public
    { Public declarations }
  end;

var
  FormCA: TFormCA;

implementation

{$R *.DFM}

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
  FCPriv := TCnRSAPrivateKey.Create;
  FCPub := TCnRSAPublicKey.Create;
end;

procedure TFormCA.btnBrowseCSRClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnBrowseKeyClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRSAKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCSRClick(Sender: TObject);
var
  CSR: TCnRSACertificateRequest;
  OutBuf: array of Byte;
  OutLen: Integer;
begin
  CSR := TCnRSACertificateRequest.Create;
  if CnCALoadCertificateSignRequestFromFile(edtCSR.Text, CSR) then
  begin
    mmoCSRParse.Clear;
    mmoCSRParse.Lines.Add(CSR.ToString);

    if (CSR.SignValue <> nil) and (CSR.SignLength > 0) and (CSR.PublicKey.BitsCount > 128) then
    begin
      SetLength(OutBuf, CSR.PublicKey.BitsCount div 8);
      if CnRSADecryptRawData(CSR.SignValue, CSR.SignLength, @OutBuf[0], OutLen, CSR.PublicKey) then
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

procedure TFormCA.btnGenerateCSRClick(Sender: TObject);
begin
  if FileExists(edtRSAKey.Text) and CnRSALoadKeysFromPem(edtRSAKey.Text, FCPriv, FCPub) then
  begin
    if dlgSave.Execute then
    begin
      if CnCANewCertificateSignRequest(FCPriv, FCPub, dlgSave.FileName, edtContryName.Text,
        edtStateOrProvinceName.Text, edtLocalityName.Text, edtOrgName.Text,
        edtOrgUnitName.Text, edtCommonName.Text, edtEmail.Text, TCnCASignType(cbbHash.ItemIndex)) then
        ShowMessage('Generate CSR File Success.')
      else
        ShowMessage('Generate CSR File Fail.');
    end;
  end
  else
    ShowMessage('Invalid RSA Keys');
end;

procedure TFormCA.FormDestroy(Sender: TObject);
begin
  FCPub.Free;
  FCPriv.Free;
end;

procedure TFormCA.btnBrowseCRTClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCRT.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCRTClick(Sender: TObject);
var
  CRT: TCnRSACertificate;
begin
  CRT := TCnRSACertificate.Create;
  if not CnCALoadCertificateFromFile(edtCRT.Text, CRT) then
    ShowMessage('Parse CRT File Failed.');
  mmoCRT.Clear;
  mmoCRT.Lines.Add(CRT.ToString);
  CRT.Free;
end;

end.
