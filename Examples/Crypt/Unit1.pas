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
    edtFrom: TEdit;
    btnMd5: TButton;
    pnlMd5: TPanel;
    tsBase64: TTabSheet;
    GroupBox1: TGroupBox;
    lbl2: TLabel;
    edtBase64from: TEdit;
    Button1: TButton;
    edt3: TEdit;
    lbl3: TLabel;
    Button2: TButton;
    edt4: TEdit;
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
  CnMD5, CnDES, CnBase64, CnCRC32, CnSHA1, CnSM3, CnSM4, CnAES, CnSHA2;

{$R *.DFM}

procedure TFormCrypt.btnMd5Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlMd5.Caption := MD5Print(MD5StringA(AnsiString(edtFrom.Text)));
{$ELSE}
  pnlMd5.Caption := MD5Print(MD5String(edtFrom.Text));
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
  edt3.Text := S;
end;

procedure TFormCrypt.Button2Click(Sender: TObject);
var
  S: AnsiString;
begin
  Base64Decode(edt3.Text, S);
  edt4.Text := S;
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
  S := SM3Print(SM3(PAnsiChar(AnsiString(edtSm3.Text)),
    Length(AnsiString(edtSm3.Text))));
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
  Insert(#13#10, S, 65);
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
    lblSHA512Result.Caption := S;
  end;
end;

end.
