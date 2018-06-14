unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Clipbrd;

type
  TFormCrypt = class(TForm)
    PageControl1: TPageControl;
    tsDES: TTabSheet;
    tsMD5: TTabSheet;
    grpdES: TGroupBox;
    lbl1: TLabel;
    lblKey: TLabel;
    lblCode: TLabel;
    lblOrigin: TLabel;
    edtDesFrom: TEdit;
    edtKey: TEdit;
    btnDesCrypt: TButton;
    edtCode: TEdit;
    btnDesDecrypt: TButton;
    edtOrigin: TEdit;
    grpMd5: TGroupBox;
    lblfROM: TLabel;
    edtMD5: TEdit;
    btnMd5: TButton;
    pnlMd5: TPanel;
    tsBase64: TTabSheet;
    GroupBox1: TGroupBox;
    lbl2: TLabel;
    edtBase64from: TEdit;
    Button1: TButton;
    edtBase64Result: TEdit;
    lbl3: TLabel;
    Button2: TButton;
    edtBase64Decode: TEdit;
    lbl4: TLabel;
    tsCRC32: TTabSheet;
    grpCRC32: TGroupBox;
    lblCRC: TLabel;
    edtCRC32: TEdit;
    btnCRC32: TButton;
    pnlCRC32: TPanel;
    btnMd5File: TButton;
    OpenDialog1: TOpenDialog;
    btnFileCRC32: TButton;
    tsCRC64: TTabSheet;
    grp1: TGroupBox;
    lbl5: TLabel;
    edtCRC64: TEdit;
    btnCRC64: TButton;
    pnlCRC64: TPanel;
    btnFileCRC64: TButton;
    tsSha1: TTabSheet;
    grpSha1: TGroupBox;
    lblSha1: TLabel;
    edtSha1: TEdit;
    btnSha1: TButton;
    pnlSha1: TPanel;
    btnFileSha1: TButton;
    tsSM3: TTabSheet;
    grpSM3: TGroupBox;
    lblSM3: TLabel;
    edtSM3: TEdit;
    btnSM3: TButton;
    btnFileSM3: TButton;
    lblSm3Result: TLabel;
    tsSM4: TTabSheet;
    grpSM4: TGroupBox;
    lblSm4: TLabel;
    edtSm4: TEdit;
    btnSm4: TButton;
    edtSm4Key: TEdit;
    lblSm4Key: TLabel;
    edtSm4Dec: TEdit;
    btnSm4Dec: TButton;
    lblSm4Dec: TLabel;
    edtSm4Code: TEdit;
    lblSm4Code: TLabel;
    rbSm4Ecb: TRadioButton;
    rbSm4CBC: TRadioButton;
    tsAES: TTabSheet;
    grpAes: TGroupBox;
    lblAesFrom: TLabel;
    lblAesKey: TLabel;
    lblAesOrigin: TLabel;
    lblAesCode: TLabel;
    edtAes: TEdit;
    btnAesEncrypt: TButton;
    edtAesKey: TEdit;
    edtAesDecrypt: TEdit;
    btnAesDecrypt: TButton;
    edtAesResult: TEdit;
    rbAesecb: TRadioButton;
    rbAescbc: TRadioButton;
    cbbAesKeyBitType: TComboBox;
    lblKeyBit: TLabel;
    tsSHA256: TTabSheet;
    grpSHA256: TGroupBox;
    lblSHA256: TLabel;
    edtSHA256: TEdit;
    btnSHA256: TButton;
    pnlSHA256: TPanel;
    btnFileSHA256: TButton;
    tsSHA224: TTabSheet;
    grpSHA224: TGroupBox;
    lblSHA224: TLabel;
    edtSHA224: TEdit;
    btnSHA224: TButton;
    pnlSHA224: TPanel;
    btnSHA224File: TButton;
    tsSHA512: TTabSheet;
    grpSHA512: TGroupBox;
    lblSHA512: TLabel;
    edtSHA512: TEdit;
    btnSHA512: TButton;
    btnSHA512File: TButton;
    lblSHA512Result: TLabel;
    tsSHA384: TTabSheet;
    grpSHA384: TGroupBox;
    lblSHA384: TLabel;
    lblSHA384Result: TLabel;
    edtSHA384: TEdit;
    btnSHA384: TButton;
    btnSHA384File: TButton;
    tsSHA3_224: TTabSheet;
    grpSHA3_224: TGroupBox;
    lblSHA3_224: TLabel;
    edtSHA3_224: TEdit;
    btnSHA3_224: TButton;
    pnlSHA3_224: TPanel;
    btnSHA3_224File: TButton;
    tsSHA3_256: TTabSheet;
    grpSHA3_256: TGroupBox;
    lblSHA3_256: TLabel;
    edtSHA3_256: TEdit;
    btnSHA3_256: TButton;
    pnlSHA3_256: TPanel;
    btnFileSHA3_256: TButton;
    tsSHA3_384: TTabSheet;
    grpSHA3_384: TGroupBox;
    lblSHA3_384: TLabel;
    lblSHA3_384Result: TLabel;
    edtSHA3_384: TEdit;
    btnSHA3_384: TButton;
    btnSHA3_384File: TButton;
    tsSHA3_512: TTabSheet;
    grpSHA3_512: TGroupBox;
    lblSHA3_512: TLabel;
    edtSHA3_512: TEdit;
    btnSHA3_512: TButton;
    btnSHA3_512File: TButton;
    lblSHA3_512Result: TLabel;
    tsZUC: TTabSheet;
    grpZuc: TGroupBox;
    lblZuc1: TLabel;
    btnZUC1: TButton;
    btnZUCEIA31: TButton;
    btnZUC2: TButton;
    btnZUC3: TButton;
    btnZUC4: TButton;
    btnZUCEIA32: TButton;
    btnZUCEEA31: TButton;
    lblSHA224HmacKey: TLabel;
    edtSHA224HmacKey: TEdit;
    btnSHA224Hmac: TButton;
    lblSHA256HmacKey: TLabel;
    edtSHA256HmacKey: TEdit;
    btnSHA256Hmac: TButton;
    lblSHA384HmacKey: TLabel;
    edtSHA384HmacKey: TEdit;
    btnSHA384Hmac: TButton;
    lblSHA512HmacKey: TLabel;
    edtSHA512HmacKey: TEdit;
    btnSHA512Hmac: TButton;
    lblSHA3_224HmacKey: TLabel;
    edtSHA3_224HmacKey: TEdit;
    btnSHA3_224Hmac: TButton;
    lblSHA3_256HmacKey: TLabel;
    edtSHA3_256HmacKey: TEdit;
    btnSHA3_256Hmac: TButton;
    lblSHA3_384HmacKey: TLabel;
    edtSHA3_384HmacKey: TEdit;
    btnSHA3_384Hmac: TButton;
    lblSHA3_512HmacKey: TLabel;
    edtSHA3_512HmacKey: TEdit;
    btnSHA3_512Hmac: TButton;
    edtSHA1HMacKey: TEdit;
    btnSHA1Hmac: TButton;
    lblSHA1HmacKey: TLabel;
    lblMD5HmacKey: TLabel;
    edtMD5HmacKey: TEdit;
    btnMD5Hmac: TButton;
    lblSM3HmacKey: TLabel;
    edtSM3HMacKey: TEdit;
    btnSM3Hmac: TButton;
    lblCRC32HmacKey: TLabel;
    edtCRC32HmacKey: TEdit;
    btnCRC32Hmac: TButton;
    btnCRC64Hmac: TButton;
    edtCRC64HmacKey: TEdit;
    lblCRC64HmacKey: TLabel;
    btnUMd5: TButton;
    btnUSHA1: TButton;
    btnUSM3: TButton;
    btnUSHA224: TButton;
    btnUSHA256: TButton;
    btnUSHA384: TButton;
    btnUSHA512: TButton;
    btnUSHA3_224: TButton;
    btnUSHA3_256: TButton;
    btnUSHA3_384: TButton;
    btnUSHA3_512: TButton;
    btnBase64File: TButton;
    procedure btnMd5Click(Sender: TObject);
    procedure btnDesCryptClick(Sender: TObject);
    procedure btnDesDecryptClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnCRC32Click(Sender: TObject);
    procedure btnMd5FileClick(Sender: TObject);
    procedure btnFileCRC32Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCRC64Click(Sender: TObject);
    procedure btnFileCRC64Click(Sender: TObject);
    procedure btnSha1Click(Sender: TObject);
    procedure btnFileSha1Click(Sender: TObject);
    procedure btnSM3Click(Sender: TObject);
    procedure btnFileSM3Click(Sender: TObject);
    procedure btnSm4Click(Sender: TObject);
    procedure btnSm4DecClick(Sender: TObject);
    procedure btnAesEncryptClick(Sender: TObject);
    procedure btnAesDecryptClick(Sender: TObject);
    procedure ResultDblClick(Sender: TObject);
    procedure btnSHA256Click(Sender: TObject);
    procedure btnFileSHA256Click(Sender: TObject);
    procedure btnSHA224Click(Sender: TObject);
    procedure btnSHA224FileClick(Sender: TObject);
    procedure btnSHA512Click(Sender: TObject);
    procedure btnSHA512FileClick(Sender: TObject);
    procedure btnSHA384Click(Sender: TObject);
    procedure btnSHA384FileClick(Sender: TObject);
    procedure btnZUC1Click(Sender: TObject);
    procedure btnZUCEIA31Click(Sender: TObject);
    procedure btnZUC2Click(Sender: TObject);
    procedure btnZUC3Click(Sender: TObject);
    procedure btnZUC4Click(Sender: TObject);
    procedure btnZUCEIA32Click(Sender: TObject);
    procedure btnZUCEEA31Click(Sender: TObject);
    procedure btnSHA256HmacClick(Sender: TObject);
    procedure btnSHA224HmacClick(Sender: TObject);
    procedure btnSHA384HmacClick(Sender: TObject);
    procedure btnSHA512HmacClick(Sender: TObject);
    procedure btnSHA1HmacClick(Sender: TObject);
    procedure btnMD5HmacClick(Sender: TObject);
    procedure btnSM3HmacClick(Sender: TObject);
    procedure btnCRC32HmacClick(Sender: TObject);
    procedure btnCRC64HmacClick(Sender: TObject);
    procedure btnSHA3_256Click(Sender: TObject);
    procedure btnFileSHA3_256Click(Sender: TObject);
    procedure btnSHA3_224Click(Sender: TObject);
    procedure btnSHA3_224FileClick(Sender: TObject);
    procedure btnSHA3_512Click(Sender: TObject);
    procedure btnSHA3_512FileClick(Sender: TObject);
    procedure btnSHA3_384Click(Sender: TObject);
    procedure btnSHA3_384FileClick(Sender: TObject);
    procedure btnSHA3_256HmacClick(Sender: TObject);
    procedure btnSHA3_224HmacClick(Sender: TObject);
    procedure btnSHA3_384HmacClick(Sender: TObject);
    procedure btnSHA3_512HmacClick(Sender: TObject);
    procedure btnUMd5Click(Sender: TObject);
    procedure btnUSHA1Click(Sender: TObject);
    procedure btnUSM3Click(Sender: TObject);
    procedure btnUSHA224Click(Sender: TObject);
    procedure btnUSHA256Click(Sender: TObject);
    procedure btnUSHA384Click(Sender: TObject);
    procedure btnUSHA512Click(Sender: TObject);
    procedure btnUSHA3_224Click(Sender: TObject);
    procedure btnUSHA3_256Click(Sender: TObject);
    procedure btnUSHA3_384Click(Sender: TObject);
    procedure btnUSHA3_512Click(Sender: TObject);
    procedure btnBase64FileClick(Sender: TObject);
  private
    { Private declarations }
    function ToHex(Buffer: PAnsiChar; Length: Integer): AnsiString;
    function HexToStr(Hex: string): AnsiString;
  public
    { Public declarations }
  end;

var
  FormCrypt: TFormCrypt;

implementation

uses
  CnMD5, CnDES, CnBase64, CnCRC32, CnSHA1, CnSM3, CnSM4, CnAES, CnSHA2, CnZUC,
  CnSHA3;

{$R *.DFM}

procedure TFormCrypt.btnMd5Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlMd5.Caption := MD5Print(MD5StringA(AnsiString(edtMD5.Text)));
{$ELSE}
  pnlMd5.Caption := MD5Print(MD5String(edtMD5.Text));
{$ENDIF}
end;

procedure TFormCrypt.btnDesCryptClick(Sender: TObject);
begin
  edtCode.Text := DESEncryptStrToHex(edtDesFrom.Text, edtKey.Text);
end;

procedure TFormCrypt.btnDesDecryptClick(Sender: TObject);
begin
  edtOrigin.Text := DESDecryptStrFromHex(edtCode.Text, edtKey.Text);
end;

procedure TFormCrypt.Button1Click(Sender: TObject);
var
  S: string;
begin
  Base64Encode(edtBase64from.Text, S);
  edtBase64Result.Text := S;
end;

procedure TFormCrypt.Button2Click(Sender: TObject);
var
  S: AnsiString;
begin
  Base64Decode(edtBase64Result.Text, S);
  edtbase64Decode.Text := S;
end;

procedure TFormCrypt.btnCRC32Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlCRC32.Caption := IntToHex(StrCRC32A(0, AnsiString(edtCRC32.Text)), 2);
{$ELSE}
  pnlCRC32.Caption := IntToHex(StrCRC32(0, edtCRC32.Text), 2);
{$ENDIF}
end;

procedure TFormCrypt.btnMd5FileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlMd5.Caption := MD5Print(MD5File(OpenDialog1.FileName));
end;

procedure TFormCrypt.btnFileCRC32Click(Sender: TObject);
var
  Crc: DWORD;
begin
  Crc := 0;
  if OpenDialog1.Execute then
    if FileCRC32(OpenDialog1.FileName, Crc) then
      pnlCRC32.Caption := IntToHex(Crc, 2);
end;

procedure TFormCrypt.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  cbbAesKeyBitType.ItemIndex := 0;
  Application.Title := Caption;
end;

procedure TFormCrypt.btnCRC64Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlCRC64.Caption := IntToHex(StrCRC64A(0, AnsiString(edtCRC64.Text)), 2);
{$ELSE}
  pnlCRC64.Caption := IntToHex(StrCRC64(0, edtCRC64.Text), 2);
{$ENDIF}
end;

procedure TFormCrypt.btnFileCRC64Click(Sender: TObject);
var
  Crc: Int64;
begin
  Crc := 0;
  if OpenDialog1.Execute then
    if FileCRC64(OpenDialog1.FileName, Crc) then
      pnlCRC64.Caption := IntToHex(Crc, 2);
end;

procedure TFormCrypt.btnSha1Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlSha1.Caption := SHA1Print(SHA1StringA(AnsiString(edtSha1.Text)));
{$ELSE}
  pnlSha1.Caption := SHA1Print(SHA1String(edtSha1.Text));
{$ENDIF}
end;

procedure TFormCrypt.btnFileSha1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlSha1.Caption := SHA1Print(SHA1File(OpenDialog1.FileName));
end;

procedure TFormCrypt.btnSM3Click(Sender: TObject);
var
  S: string;
begin
{$IFDEF UNICODE}
  S := SM3Print(SM3StringA(AnsiString(edtSm3.Text)));
{$ELSE}
  S := SM3Print(SM3String(edtSm3.Text));
{$ENDIF}
  Insert(#13#10, S, 33);
  lblSm3Result.Caption := S;
end;

procedure TFormCrypt.btnFileSM3Click(Sender: TObject);
var
  S: string;
begin
  if OpenDialog1.Execute then
  begin
    S := SM3Print(SM3File(OpenDialog1.FileName));
    Insert(#13#10, S, 33);
    lblSm3Result.Caption := S;
  end;
end;

//var
//  Sm4Input: array[0..15] of Byte = (
//    $01, $23, $45, $67, $89, $AB, $CD, $EF,
//    $FE, $DC, $BA, $98, $76, $54, $32, $10
//  );
//  Sm4Key: array[0..15] of Byte = (
//    $01, $23, $45, $67, $89, $AB, $CD, $EF,
//    $FE, $DC, $BA, $98, $76, $54, $32, $10
//  );

var
  Sm4Iv: array[0..15] of Byte = (
    $01, $23, $45, $67, $89, $AB, $CD, $EF,
    $FE, $DC, $BA, $98, $76, $54, $32, $10
  );

  AesIv: TAESBuffer = (
    $01, $23, $45, $67, $89, $AB, $CD, $EF,
    $FE, $DC, $BA, $98, $76, $54, $32, $10
  );

procedure TFormCrypt.btnSm4Click(Sender: TObject);
var
  Output: AnsiString;
  Len: Integer;
  TmpSm4Iv: array[0..15] of Byte;
//  Ctx: TSM4Context;
begin
//  SM4SetKeyEnc(Ctx, @(Sm4Key[0]));
//  SM4CryptEcb(Ctx, SM4_ENCRYPT, 16, @(Sm4Input[0]), @(Output[0]));
//
//  lblSm4Text.Caption := ToHex(@(Output[0]), SizeOf(Output));
//
//  SM4SetKeyDec(Ctx, @(Sm4Key[0]));
//  SM4CryptEcb(Ctx, SM4_DECRYPT, 16, @(Output[0]), @(Output[0]));
//
//  lblSm4Text.Caption := ToHex(@(Output[0]), SizeOf(Output));

  Len := Length(edtSm4.Text);
  if Len < 16 then
    Len := 16
  else
    Len := (((Len - 1) div 16) + 1) * 16;
  SetLength(Output, Len);
  ZeroMemory(@(Output[1]), Len);

  if rbSm4Ecb.Checked then
    SM4CryptEcbStr(SM4_ENCRYPT, edtSm4Key.Text, edtSm4.Text, @(Output[1]))
  else
  begin
    CopyMemory(@(TmpSm4Iv[0]), @(Sm4Iv[0]), SizeOf(Sm4Iv));
    SM4CryptCbcStr(SM4_ENCRYPT, edtSm4Key.Text, PAnsiChar(@(TmpSm4Iv[0])), edtSm4.Text, @(Output[1]));
  end;
  edtSm4Code.Text := ToHex(@(Output[1]), Length(Output));
end;

function TFormCrypt.ToHex(Buffer: PAnsiChar; Length: Integer): AnsiString;
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

procedure TFormCrypt.btnSm4DecClick(Sender: TObject);
var
  S: AnsiString;
  Output: AnsiString;
  Len: Integer;
  TmpSm4Iv: array[0..15] of Byte;
begin
  S := AnsiString(HexToStr(edtSm4Code.Text));
  Len := Length(S);
  if Len < 16 then
    Len := 16
  else
    Len := (((Len - 1) div 16) + 1) * 16;
  SetLength(Output, Len);
  ZeroMemory(@(Output[1]), Len);

  if rbSm4Ecb.Checked then
    SM4CryptEcbStr(SM4_DECRYPT, edtSm4Key.Text, S, @(Output[1]))
  else
  begin
    CopyMemory(@(TmpSm4Iv[0]), @(Sm4Iv[0]), SizeOf(Sm4Iv));
    SM4CryptCbcStr(SM4_DECRYPT, edtSm4Key.Text, PAnsiChar(@(TmpSm4Iv[0])), S, @(Output[1]));
  end;
  edtSm4Dec.Text := Output;
end;

function HexToInt(Hex: AnsiString): Integer;
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
    else raise Exception.Create('Error: not a Hex String');
  end;
  Result := Res;
end;

function TFormCrypt.HexToStr(Hex: string): AnsiString;
var
  S: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Hex) div 2 - 1 do
  begin
    S := Copy(Hex, I * 2 + 1, 2);
    Result := Result + AnsiChar(HexToInt(S));
  end;
end;

procedure TFormCrypt.btnAesEncryptClick(Sender: TObject);
var
  TmpAesIv: TAESBuffer;
begin
  if rbAesecb.Checked then
  begin
    case cbbAesKeyBitType.ItemIndex of
      0:                       
        edtAesResult.Text := AESEncryptEcbStrToHex(edtAes.Text, edtAesKey.Text, kbt128);
      1:
        edtAesResult.Text := AESEncryptEcbStrToHex(edtAes.Text, edtAesKey.Text, kbt192);
      2:
        edtAesResult.Text := AESEncryptEcbStrToHex(edtAes.Text, edtAesKey.Text, kbt256);
    end;
  end
  else
  begin
    CopyMemory(@TmpAesIv, @AesIv, SizeOf(TmpAesIv));
    case cbbAesKeyBitType.ItemIndex of
      0:
        edtAesResult.Text := AESEncryptCbcStrToHex(edtAes.Text, edtAesKey.Text, TmpAesIv, kbt128);
      1:
        edtAesResult.Text := AESEncryptCbcStrToHex(edtAes.Text, edtAesKey.Text, TmpAesIv, kbt192);
      2:
        edtAesResult.Text := AESEncryptCbcStrToHex(edtAes.Text, edtAesKey.Text, TmpAesIv, kbt256);
    end;
  end;
end;

procedure TFormCrypt.btnAesDecryptClick(Sender: TObject);
var
  TmpAesIv: TAESBuffer;
begin
  if rbAesecb.Checked then
  begin
    case cbbAesKeyBitType.ItemIndex of
      0:
        edtAesDecrypt.Text := AESDecryptEcbStrFromHex(edtAesResult.Text, edtAesKey.Text, kbt128);
      1:
        edtAesDecrypt.Text := AESDecryptEcbStrFromHex(edtAesResult.Text, edtAesKey.Text, kbt192);
      2:
        edtAesDecrypt.Text := AESDecryptEcbStrFromHex(edtAesResult.Text, edtAesKey.Text, kbt256);
    end;
  end
  else
  begin
    CopyMemory(@TmpAesIv, @AesIv, SizeOf(TmpAesIv));
    case cbbAesKeyBitType.ItemIndex of
      0:
        edtAesDecrypt.Text := AESDecryptCbcStrFromHex(edtAesResult.Text, edtAesKey.Text, TmpAesIv, kbt128);
      1:
        edtAesDecrypt.Text := AESDecryptCbcStrFromHex(edtAesResult.Text, edtAesKey.Text, TmpAesIv, kbt192);
      2:
        edtAesDecrypt.Text := AESDecryptCbcStrFromHex(edtAesResult.Text, edtAesKey.Text, TmpAesIv, kbt256);
    end;
  end;
end;

procedure TFormCrypt.ResultDblClick(Sender: TObject);
begin
  if Sender is TPanel then
    Clipboard.AsText := (Sender as TPanel).Caption
  else if Sender is TLabel then
    Clipboard.AsText := (Sender as TLabel).Caption;
end;

procedure TFormCrypt.btnSHA256Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlSha256.Caption := SHA256Print(SHA256StringA(AnsiString(edtSha256.Text)));
{$ELSE}
  pnlSha256.Caption := SHA256Print(SHA256String(edtSha256.Text));
{$ENDIF}
end;

procedure TFormCrypt.btnFileSHA256Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlSha256.Caption := SHA256Print(SHA256File(OpenDialog1.FileName));
end;

procedure TFormCrypt.btnSHA224Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlSha224.Caption := SHA224Print(SHA224StringA(AnsiString(edtSha224.Text)));
{$ELSE}
  pnlSha224.Caption := SHA224Print(SHA224String(edtSha224.Text));
{$ENDIF}
end;

procedure TFormCrypt.btnSHA224FileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlSha224.Caption := SHA224Print(SHA224File(OpenDialog1.FileName));
end;

procedure TFormCrypt.btnSHA512Click(Sender: TObject);
var
  S: string;
begin
{$IFDEF UNICODE}
  S := SHA512Print(SHA512StringA(AnsiString(edtSha512.Text)));
{$ELSE}
  S := SHA512Print(SHA512String(edtSha512.Text));
{$ENDIF}
  Insert(#13#10, S, 65);
  lblSHA512Result.Caption := S;
end;

procedure TFormCrypt.btnSHA512FileClick(Sender: TObject);
var
  S: string;
begin
  if OpenDialog1.Execute then
  begin
    S := SHA512Print(SHA512File(OpenDialog1.FileName));
    Insert(#13#10, S, 65);
    lblSHA512Result.Caption := S;
  end;
end;

procedure TFormCrypt.btnSHA384Click(Sender: TObject);
var
  S: string;
begin
{$IFDEF UNICODE}
  S := SHA384Print(SHA384StringA(AnsiString(edtSha384.Text)));
{$ELSE}
  S := SHA384Print(SHA384String(edtSha384.Text));
{$ENDIF}
  Insert(#13#10, S, 49);
  lblSHA384Result.Caption := S;
end;

procedure TFormCrypt.btnSHA384FileClick(Sender: TObject);
var
  S: string;
begin
  if OpenDialog1.Execute then
  begin
    S := SHA384Print(SHA384File(OpenDialog1.FileName));
    Insert(#13#10, S, 49);
    lblSHA384Result.Caption := S;
  end;
end;

//输入:
// 密钥 k:      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
// 初始向量 iv: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
//输出:
// z1: 27bede74
// z2: 018082da
procedure TFormCrypt.btnZUC1Click(Sender: TObject);
var
  Key, IV: array[0..15] of Byte;
  KeyStream: array[0..1] of DWORD;
  List: TStringList;
  I: Integer;
begin
  FillChar(Key[0], SizeOf(Key), 0);
  FillChar(IV[0], SizeOf(IV), 0);
  ZUC(PByte(@Key[0]), PByte(@IV[0]), PDWORD(@KeyStream[0]), 2);

  List := TStringList.Create;
  for I := Low(KeyStream) to High(KeyStream) do
    List.Add('$' + IntToHex(KeyStream[I], 2));

  ShowMessage(List.Text);
  List.Free;
end;

//输入:
//  密钥 k:      ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
//  初始向量 iv: ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff ff
//输出:
//  z1: 0657cfa0
//  z2: 7096398b
procedure TFormCrypt.btnZUC2Click(Sender: TObject);
var
  Key, IV: array[0..15] of Byte;
  KeyStream: array[0..1] of DWORD;
  List: TStringList;
  I: Integer;
begin
  FillChar(Key[0], SizeOf(Key), $FF);
  FillChar(IV[0], SizeOf(IV), $FF);
  ZUC(PByte(@Key[0]), PByte(@IV[0]), PDWORD(@KeyStream[0]), 2);

  List := TStringList.Create;
  for I := Low(KeyStream) to High(KeyStream) do
    List.Add('$' + IntToHex(KeyStream[I], 2));

  ShowMessage(List.Text);
  List.Free;
end;

//输入:
//  密钥 k:      3d 4c 4b e9 6a 82 fd ae b5 8f 64 1d b1 7b 45 5b
//  初始向量 iv: 84 31 9a a8 de 69 15 ca 1f 6b da 6b fb d8 c7 66
//输出:
//  z1: 14f1c272
//  z2: 3279c419
procedure TFormCrypt.btnZUC3Click(Sender: TObject);
const
  Key: array[0..15] of Byte = ($3d, $4c, $4b, $e9, $6a, $82, $fd, $ae, $b5, $8f,
    $64, $1d, $b1, $7b, $45, $5b);
  IV: array[0..15] of Byte = ($84, $31, $9a, $a8, $de, $69, $15, $ca, $1f, $6b,
    $da, $6b, $fb, $d8, $c7, $66);
var
  KeyStream: array[0..1] of DWORD;
  List: TStringList;
  I: Integer;
begin
  ZUC(PByte(@Key[0]), PByte(@IV[0]), PDWORD(@KeyStream[0]), 2);

  List := TStringList.Create;
  for I := Low(KeyStream) to High(KeyStream) do
    List.Add('$' + IntToHex(KeyStream[I], 2));

  ShowMessage(List.Text);
  List.Free;
end;

//输入:
// Key: 4d 32 0b fa d4 c2 85 bf d6 b8 bd 00 f3 9d 8b 41
// IV: 52 95 9d ab a0 bf 17 6e ce 2d c3 15 04 9e b5 74
//输出:
// z1: ed4400e7
// z2: 0633e5c5
//……
// Z2000: 7a574cdb
procedure TFormCrypt.btnZUC4Click(Sender: TObject);
const
  Key: array[0..15] of Byte = ($4d, $32, $0b, $fa, $d4, $c2, $85, $bf, $d6, $b8,
    $bd, $00, $f3, $9d, $8b, $41);
  IV: array[0..15] of Byte = ($52, $95, $9d, $ab, $a0, $bf, $17, $6e, $ce, $2d,
    $c3, $15, $04, $9e, $b5, $74);
var
  KeyStream: array[0..1999] of DWORD;
  List: TStringList;
begin
  ZUC(PByte(@Key[0]), PByte(@IV[0]), PDWORD(@KeyStream[0]), 2000);

  List := TStringList.Create;
  List.Add('$' + IntToHex(KeyStream[0], 2));
  List.Add('$' + IntToHex(KeyStream[1], 2));
  List.Add('...');
  List.Add('$' + IntToHex(KeyStream[1999], 2));

  ShowMessage(List.Text);
  List.Free;
end;

//Test Set 1
//Key = (hex) 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
//Count = (hex) 0
//Bearer = (hex) 0
//Direction = (hex) 0
//Length = 1 bits
//Message:
//(hex) 00000000
//MAC: (hex) c8a9595e
procedure TFormCrypt.btnZUCEIA31Click(Sender: TObject);
var
  Key: array[0..15] of Byte;
  Msg: Byte;
  Mac: DWORD;
begin
  FillChar(Key[0], SizeOf(Key), 0);
  Msg := 0;
  ZUCEIA3(PByte(@Key[0]), 0, 0, 0, @Msg, 1, @Mac);
  ShowMessage('$' + IntToHex(Mac, 2));
end;

//Test Set 2
//Key = 47 05 41 25 56 1e b2 dd a9 40 59 da 05 09 78 50
//Count = 561eb2dd
//Bearer = 14
//Direction = 0
//Length = 90 bits
//Message:
//00000000 00000000 00000000
//MAC: 6719a088
procedure TFormCrypt.btnZUCEIA32Click(Sender: TObject);
const
  Key: array[0..15] of Byte = ($47, $05, $41, $25, $56, $1e, $b2, $dd, $a9, $40,
    $59, $da, $05, $09, $78, $50);
var
  Msg: array[0..20] of Byte;  // Enough for 90 bits
  Mac: DWORD;
begin
  // FIXME: NOT Ready now.
  FillChar(Msg[0], SizeOf(Msg), 0);
  ZUCEIA3(PByte(@Key[0]), $561eb2dd, $14, 0, @(Msg[0]), 90, @Mac);
  ShowMessage('$' + IntToHex(Mac, 2));
end;

//Key = (hex) 17 3d 14 ba 50 03 73 1d 7a 60 04 94 70 f0 0a 29
//Count = (hex) 66035492
//Bearer = (hex) f
//Direction = (hex) 0
//Length = 193 bits
//Plaintext:
//(hex) 6cf65340 735552ab 0c9752fa 6f9025fe 0bd675d9 005875b2 00000000
//Ciphertext:
//(hex) a6c85fc6 6afb8533 aafc2518 dfe78494 0ee1e4b0 30238cc8 00000000
procedure TFormCrypt.btnZUCEEA31Click(Sender: TObject);
const
  Key: array[0..15] of Byte = ($17, $3d, $14, $ba, $50, $03, $73, $1d, $7a, $60,
    $04, $94, $70, $f0, $0a, $29);
  Plain: array[0..6] of DWORD = ($6cf65340, $735552ab, $0c9752fa, $6f9025fe,
    $0bd675d9, $005875b2, 0);
var
  Cipher: array[0..6] of DWORD;
  List: TStringList;
  I: Integer;
begin
  FillChar(Cipher[0], SizeOf(Cipher), 0);
  ZUCEEA3(PByte(@Key[0]), $66035492, $F, 0, PByte(@Plain[0]), 193, PByte(@Cipher[0]));

  List := TStringList.Create;
  for I := Low(Cipher) to High(Cipher) do
    List.Add('$' + IntToHex(Cipher[I], 2));
  ShowMessage(List.Text);
  List.Free;
end;

procedure TFormCrypt.btnSHA256HmacClick(Sender: TObject);
var
  Output: TSHA256Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA256HmacKey.Text);
  S := AnsiString(edtSHA256.Text);
  SHA256Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  pnlSHA256.Caption := SHA256Print(Output);
end;

procedure TFormCrypt.btnSHA224HmacClick(Sender: TObject);
var
  Output: TSHA224Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA224HmacKey.Text);
  S := AnsiString(edtSHA224.Text);
  SHA224Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  pnlSHA224.Caption := SHA224Print(Output);
end;

procedure TFormCrypt.btnSHA384HmacClick(Sender: TObject);
var
  Output: TSHA384Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA384HmacKey.Text);
  S := AnsiString(edtSHA384.Text);
  SHA384Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  S := SHA384Print(Output);
  Insert(#13#10, S, 49);
  lblSHA384Result.Caption := S;
end;

procedure TFormCrypt.btnSHA512HmacClick(Sender: TObject);
var
  Output: TSHA512Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA512HmacKey.Text);
  S := AnsiString(edtSHA512.Text);
  SHA512Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  S := SHA512Print(Output);
  Insert(#13#10, S, 65);
  lblSHA512Result.Caption := S;
end;

procedure TFormCrypt.btnSHA1HmacClick(Sender: TObject);
var
  Output: TSHA1Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA1HmacKey.Text);
  S := AnsiString(edtSHA1.Text);
  SHA1Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  pnlSHA1.Caption := SHA1Print(Output);
end;

procedure TFormCrypt.btnMD5HmacClick(Sender: TObject);
var
  Output: TMD5Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtMD5HmacKey.Text);
  S := AnsiString(edtMD5.Text);
  MD5Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  pnlMD5.Caption := MD5Print(Output);
end;

procedure TFormCrypt.btnSM3HmacClick(Sender: TObject);
var
  Output: TSM3Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSM3HmacKey.Text);
  S := AnsiString(edtSM3.Text);
  SM3Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  lblSm3Result.Caption := SM3Print(Output);
end;

procedure TFormCrypt.btnCRC32HmacClick(Sender: TObject);
var
  Output: DWORD;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtCRC32HmacKey.Text);
  S := AnsiString(edtCRC32.Text);
  Output := CRC32Hmac(@Key[1], Length(Key), @S[1], Length(S));
  pnlCRC32.Caption := IntToHex(Output, 2);
end;

procedure TFormCrypt.btnCRC64HmacClick(Sender: TObject);
var
  Output: Int64;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtCRC64HmacKey.Text);
  S := AnsiString(edtCRC64.Text);
  Output := CRC64Hmac(@Key[1], Length(Key), @S[1], Length(S));
  pnlCRC64.Caption := IntToHex(Output, 2);
end;

procedure TFormCrypt.btnFileSHA3_256Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlSha3_256.Caption := SHA3_256Print(SHA3_256File(OpenDialog1.FileName));
end;

procedure TFormCrypt.btnSHA3_224Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlSHA3_224.Caption := SHA3_224Print(SHA3_224StringA(AnsiString(edtSha3_224.Text)));
{$ELSE}
  pnlSHA3_224.Caption := SHA3_224Print(SHA3_224String(edtSha3_224.Text));
{$ENDIF}
end;

procedure TFormCrypt.btnSHA3_224FileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlSha3_224.Caption := SHA3_224Print(SHA3_224File(OpenDialog1.FileName));
end;

procedure TFormCrypt.btnSHA3_224HmacClick(Sender: TObject);
var
  Output: TSHA3_224Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA3_224HmacKey.Text);
  S := AnsiString(edtSHA3_224.Text);
  SHA3_224Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  pnlSHA3_224.Caption := SHA3_224Print(Output);
end;

procedure TFormCrypt.btnSHA3_256Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlSHA3_256.Caption := SHA3_256Print(SHA3_256StringA(AnsiString(edtSha3_256.Text)));
{$ELSE}
  pnlSHA3_256.Caption := SHA3_256Print(SHA3_256String(edtSha3_256.Text));
{$ENDIF}
end;

procedure TFormCrypt.btnSHA3_256HmacClick(Sender: TObject);
var
  Output: TSHA3_256Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA3_256HmacKey.Text);
  S := AnsiString(edtSHA3_256.Text);
  SHA3_256Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  pnlSHA3_256.Caption := SHA3_256Print(Output);
end;

procedure TFormCrypt.btnSHA3_384Click(Sender: TObject);
var
  S: string;
begin
{$IFDEF UNICODE}
  S := SHA3_384Print(SHA3_384StringA(AnsiString(edtSha3_384.Text)));
{$ELSE}
  S := SHA3_384Print(SHA3_384String(edtSha3_384.Text));
{$ENDIF}
  Insert(#13#10, S, 49);
  lblSHA3_384Result.Caption := S;
end;

procedure TFormCrypt.btnSHA3_384FileClick(Sender: TObject);
var
  S: string;
begin
  if OpenDialog1.Execute then
  begin
    S := SHA3_384Print(SHA3_384File(OpenDialog1.FileName));
    Insert(#13#10, S, 49);
    lblSHA3_384Result.Caption := S;
  end;
end;

procedure TFormCrypt.btnSHA3_384HmacClick(Sender: TObject);
var
  Output: TSHA3_384Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA3_384HmacKey.Text);
  S := AnsiString(edtSHA3_384.Text);
  SHA3_384Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  S := SHA3_384Print(Output);
  Insert(#13#10, S, 49);
  lblSHA3_384Result.Caption := S;
end;

procedure TFormCrypt.btnSHA3_512Click(Sender: TObject);
var
  S: string;
begin
{$IFDEF UNICODE}
  S := SHA3_512Print(SHA3_512StringA(AnsiString(edtSha3_512.Text)));
{$ELSE}
  S := SHA3_512Print(SHA3_512String(edtSha3_512.Text));
{$ENDIF}
  Insert(#13#10, S, 65);
  lblSHA3_512Result.Caption := S;
end;

procedure TFormCrypt.btnSHA3_512FileClick(Sender: TObject);
var
  S: string;
begin
  if OpenDialog1.Execute then
  begin
    S := SHA3_512Print(SHA3_512File(OpenDialog1.FileName));
    Insert(#13#10, S, 65);
    lblSHA3_512Result.Caption := S;
  end;
end;

procedure TFormCrypt.btnSHA3_512HmacClick(Sender: TObject);
var
  Output: TSHA3_512Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSHA3_512HmacKey.Text);
  S := AnsiString(edtSHA3_512.Text);
  SHA3_512Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  S := SHA3_512Print(Output);
  Insert(#13#10, S, 65);
  lblSHA3_512Result.Caption := S;
end;

procedure TFormCrypt.btnUMd5Click(Sender: TObject);
begin
  pnlMd5.Caption := MD5Print(MD5UnicodeString(edtMD5.Text));
end;

procedure TFormCrypt.btnUSHA1Click(Sender: TObject);
begin
  pnlSha1.Caption := SHA1Print(SHA1UnicodeString(edtSha1.Text));
end;

procedure TFormCrypt.btnUSM3Click(Sender: TObject);
var
  S: string;
begin
  S := SM3Print(SM3UnicodeString(edtSm3.Text));
  Insert(#13#10, S, 33);
  lblSm3Result.Caption := S;
end;

procedure TFormCrypt.btnUSHA224Click(Sender: TObject);
begin
  pnlSha224.Caption := SHA224Print(SHA224UnicodeString(edtSha224.Text));
end;

procedure TFormCrypt.btnUSHA256Click(Sender: TObject);
begin
  pnlSha256.Caption := SHA256Print(SHA256UnicodeString(edtSha256.Text));
end;

procedure TFormCrypt.btnUSHA384Click(Sender: TObject);
var
  S: string;
begin
  S := SHA384Print(SHA384UnicodeString(edtSha384.Text));
  Insert(#13#10, S, 49);
  lblSHA384Result.Caption := S;
end;

procedure TFormCrypt.btnUSHA512Click(Sender: TObject);
var
  S: string;
begin
  S := SHA512Print(SHA512UnicodeString(edtSha512.Text));
  Insert(#13#10, S, 65);
  lblSHA512Result.Caption := S;
end;

procedure TFormCrypt.btnUSHA3_224Click(Sender: TObject);
begin
  pnlSHA3_224.Caption := SHA3_224Print(SHA3_224UnicodeString(edtSha3_224.Text));
end;

procedure TFormCrypt.btnUSHA3_256Click(Sender: TObject);
begin
  pnlSHA3_256.Caption := SHA3_256Print(SHA3_256UnicodeString(edtSha3_256.Text));
end;

procedure TFormCrypt.btnUSHA3_384Click(Sender: TObject);
var
  S: string;
begin
  S := SHA3_384Print(SHA3_384UnicodeString(edtSha3_384.Text));
  Insert(#13#10, S, 49);
  lblSHA3_384Result.Caption := S;
end;

procedure TFormCrypt.btnUSHA3_512Click(Sender: TObject);
var
  S: string;
begin
  S := SHA3_512Print(SHA3_512UnicodeString(edtSha3_512.Text));
  Insert(#13#10, S, 65);
  lblSHA3_512Result.Caption := S;
end;

procedure TFormCrypt.btnBase64FileClick(Sender: TObject);
var
  M: TFileStream;
  S: string;
begin
  if OpenDialog1.Execute then
  begin
    M := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    if Base64Encode(M, S) = Base64_OK then
      edtBase64Result.Text := S;
    M.Free;
  end;
end;

end.
