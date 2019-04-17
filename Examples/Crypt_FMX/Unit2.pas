unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Controls, FMX.Dialogs, FMX.Edit, FMX.Forms, FMX.Graphics, FMX.ListBox, FMX.StdCtrls, FMX.TabControl, FMX.Types,
  FMX.Controls.Presentation, CnZUC;

type
  TFormCrypt = class(TForm)
    PageControl1: TTabControl;
    tsDES: TTabItem;
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
    tsMD5: TTabItem;
    grpMd5: TGroupBox;
    lblfROM: TLabel;
    lblMD5HmacKey: TLabel;
    edtMD5: TEdit;
    btnMd5: TButton;
    pnlMd5: TPanel;
    btnMd5File: TButton;
    edtMD5HmacKey: TEdit;
    btnMD5Hmac: TButton;
    btnUMd5: TButton;
    tsBase64: TTabItem;
    GroupBox1: TGroupBox;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    edtBase64from: TEdit;
    Button1: TButton;
    edtBase64Result: TEdit;
    btnBase64Decode: TButton;
    edtBase64Decode: TEdit;
    btnBase64File: TButton;
    btnDeBase64File: TButton;
    tsCRC32: TTabItem;
    grpCRC32: TGroupBox;
    lblCRC: TLabel;
    lblCRC32HmacKey: TLabel;
    edtCRC32: TEdit;
    btnCRC32: TButton;
    pnlCRC32: TPanel;
    btnFileCRC32: TButton;
    edtCRC32HmacKey: TEdit;
    btnCRC32Hmac: TButton;
    tsCRC64: TTabItem;
    grp1: TGroupBox;
    lbl5: TLabel;
    lblCRC64HmacKey: TLabel;
    edtCRC64: TEdit;
    btnCRC64: TButton;
    pnlCRC64: TPanel;
    btnFileCRC64: TButton;
    btnCRC64Hmac: TButton;
    edtCRC64HmacKey: TEdit;
    tsSha1: TTabItem;
    grpSha1: TGroupBox;
    lblSha1: TLabel;
    lblSHA1HmacKey: TLabel;
    edtSha1: TEdit;
    btnSha1: TButton;
    pnlSha1: TPanel;
    btnFileSha1: TButton;
    edtSHA1HMacKey: TEdit;
    btnSHA1Hmac: TButton;
    btnUSHA1: TButton;
    tsSM3: TTabItem;
    grpSM3: TGroupBox;
    lblSM3: TLabel;
    lblSm3Result: TLabel;
    lblSM3HmacKey: TLabel;
    edtSM3: TEdit;
    btnSM3: TButton;
    btnFileSM3: TButton;
    edtSM3HMacKey: TEdit;
    btnSM3Hmac: TButton;
    btnUSM3: TButton;
    tsSM4: TTabItem;
    grpSM4: TGroupBox;
    lblSm4: TLabel;
    lblSm4Key: TLabel;
    lblSm4Dec: TLabel;
    lblSm4Code: TLabel;
    edtSm4: TEdit;
    btnSm4: TButton;
    edtSm4Key: TEdit;
    edtSm4Dec: TEdit;
    btnSm4Dec: TButton;
    edtSm4Code: TEdit;
    rbSm4Ecb: TRadioButton;
    rbSm4CBC: TRadioButton;
    tsAES: TTabItem;
    grpAes: TGroupBox;
    lblAesFrom: TLabel;
    lblAesKey: TLabel;
    lblAesOrigin: TLabel;
    lblAesCode: TLabel;
    lblKeyBit: TLabel;
    edtAes: TEdit;
    btnAesEncrypt: TButton;
    edtAesKey: TEdit;
    edtAesDecrypt: TEdit;
    btnAesDecrypt: TButton;
    edtAesResult: TEdit;
    rbAesecb: TRadioButton;
    rbAescbc: TRadioButton;
    cbbAesKeyBitType: TComboBox;
    tsSHA224: TTabItem;
    grpSHA224: TGroupBox;
    lblSHA224: TLabel;
    lblSHA224HmacKey: TLabel;
    edtSHA224: TEdit;
    btnSHA224: TButton;
    pnlSHA224: TPanel;
    btnSHA224File: TButton;
    edtSHA224HmacKey: TEdit;
    btnSHA224Hmac: TButton;
    btnUSHA224: TButton;
    tsSHA256: TTabItem;
    grpSHA256: TGroupBox;
    lblSHA256: TLabel;
    lblSHA256HmacKey: TLabel;
    edtSHA256: TEdit;
    btnSHA256: TButton;
    pnlSHA256: TPanel;
    btnFileSHA256: TButton;
    edtSHA256HmacKey: TEdit;
    btnSHA256Hmac: TButton;
    btnUSHA256: TButton;
    tsSHA384: TTabItem;
    grpSHA384: TGroupBox;
    lblSHA384: TLabel;
    lblSHA384Result: TLabel;
    lblSHA384HmacKey: TLabel;
    edtSHA384: TEdit;
    btnSHA384: TButton;
    btnSHA384File: TButton;
    edtSHA384HmacKey: TEdit;
    btnSHA384Hmac: TButton;
    btnUSHA384: TButton;
    tsSHA512: TTabItem;
    grpSHA512: TGroupBox;
    lblSHA512: TLabel;
    lblSHA512Result: TLabel;
    lblSHA512HmacKey: TLabel;
    edtSHA512: TEdit;
    btnSHA512: TButton;
    btnSHA512File: TButton;
    edtSHA512HmacKey: TEdit;
    btnSHA512Hmac: TButton;
    btnUSHA512: TButton;
    tsSHA3_224: TTabItem;
    grpSHA3_224: TGroupBox;
    lblSHA3_224: TLabel;
    lblSHA3_224HmacKey: TLabel;
    edtSHA3_224: TEdit;
    btnSHA3_224: TButton;
    pnlSHA3_224: TPanel;
    btnSHA3_224File: TButton;
    edtSHA3_224HmacKey: TEdit;
    btnSHA3_224Hmac: TButton;
    btnUSHA3_224: TButton;
    tsSHA3_256: TTabItem;
    grpSHA3_256: TGroupBox;
    lblSHA3_256: TLabel;
    lblSHA3_256HmacKey: TLabel;
    edtSHA3_256: TEdit;
    btnSHA3_256: TButton;
    pnlSHA3_256: TPanel;
    btnFileSHA3_256: TButton;
    edtSHA3_256HmacKey: TEdit;
    btnSHA3_256Hmac: TButton;
    btnUSHA3_256: TButton;
    tsSHA3_384: TTabItem;
    grpSHA3_384: TGroupBox;
    lblSHA3_384: TLabel;
    lblSHA3_384Result: TLabel;
    lblSHA3_384HmacKey: TLabel;
    edtSHA3_384: TEdit;
    btnSHA3_384: TButton;
    btnSHA3_384File: TButton;
    edtSHA3_384HmacKey: TEdit;
    btnSHA3_384Hmac: TButton;
    btnUSHA3_384: TButton;
    tsSHA3_512: TTabItem;
    grpSHA3_512: TGroupBox;
    lblSHA3_512: TLabel;
    lblSHA3_512Result: TLabel;
    lblSHA3_512HmacKey: TLabel;
    edtSHA3_512: TEdit;
    btnSHA3_512: TButton;
    btnSHA3_512File: TButton;
    edtSHA3_512HmacKey: TEdit;
    btnSHA3_512Hmac: TButton;
    btnUSHA3_512: TButton;
    tsZUC: TTabItem;
    grpZuc: TGroupBox;
    lblZuc1: TLabel;
    btnZUC1: TButton;
    btnZUCEIA31: TButton;
    btnZUC2: TButton;
    btnZUC3: TButton;
    btnZUC4: TButton;
    btnZUCEIA32: TButton;
    btnZUCEEA31: TButton;
    tsTEA: TTabItem;
    grpTea: TGroupBox;
    lblTeaKey1: TLabel;
    lblTeaKey2: TLabel;
    lblTeaKey3: TLabel;
    lblTeaKey4: TLabel;
    lblTeaData: TLabel;
    edtTeaKey1: TEdit;
    edtTeaKey2: TEdit;
    edtTeaKey3: TEdit;
    edtTeaKey4: TEdit;
    edtTeaData1: TEdit;
    edtTeaData2: TEdit;
    btnTeaEnc: TButton;
    edtTeaEnc1: TEdit;
    edtTeaEnc2: TEdit;
    btnTeaDec: TButton;
    btnXTeaEnc: TButton;
    edtXTeaEnc1: TEdit;
    edtXTeaEnc2: TEdit;
    btnXTeaDec: TButton;
    btnXXTeaEnc: TButton;
    edtXXTeaEnc1: TEdit;
    edtXXTeaEnc2: TEdit;
    btnXXTeaDec: TButton;
    OpenDialog1: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnDesCryptClick(Sender: TObject);
    procedure btnDesDecryptClick(Sender: TObject);
    procedure btnMd5Click(Sender: TObject);
    procedure ResultDblClick(Sender: TObject);
    procedure btnMd5FileClick(Sender: TObject);
    procedure btnMD5HmacClick(Sender: TObject);
    procedure btnUMd5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnBase64DecodeClick(Sender: TObject);
    procedure btnBase64FileClick(Sender: TObject);
    procedure btnDeBase64FileClick(Sender: TObject);
    procedure btnCRC32Click(Sender: TObject);
    procedure btnFileCRC32Click(Sender: TObject);
    procedure btnCRC32HmacClick(Sender: TObject);
    procedure btnCRC64Click(Sender: TObject);
    procedure btnFileCRC64Click(Sender: TObject);
    procedure btnSha1Click(Sender: TObject);
    procedure btnFileSha1Click(Sender: TObject);
    procedure btnSHA1HmacClick(Sender: TObject);
    procedure btnUSHA1Click(Sender: TObject);
    procedure btnSM3Click(Sender: TObject);
    procedure btnFileSM3Click(Sender: TObject);
    procedure btnSM3HmacClick(Sender: TObject);
    procedure btnUSM3Click(Sender: TObject);
    procedure btnSm4Click(Sender: TObject);
    procedure btnSm4DecClick(Sender: TObject);
    procedure btnAesEncryptClick(Sender: TObject);
    procedure btnAesDecryptClick(Sender: TObject);
    procedure btnSHA224Click(Sender: TObject);
    procedure btnSHA224FileClick(Sender: TObject);
    procedure btnSHA224HmacClick(Sender: TObject);
    procedure btnUSHA224Click(Sender: TObject);
    procedure btnSHA256Click(Sender: TObject);
    procedure btnFileSHA256Click(Sender: TObject);
    procedure btnSHA256HmacClick(Sender: TObject);
    procedure btnUSHA256Click(Sender: TObject);
    procedure btnSHA384Click(Sender: TObject);
    procedure btnSHA384FileClick(Sender: TObject);
    procedure btnSHA384HmacClick(Sender: TObject);
    procedure btnUSHA384Click(Sender: TObject);
    procedure btnSHA512Click(Sender: TObject);
    procedure btnSHA512FileClick(Sender: TObject);
    procedure btnSHA512HmacClick(Sender: TObject);
    procedure btnUSHA512Click(Sender: TObject);
    procedure btnSHA3_224Click(Sender: TObject);
    procedure btnSHA3_224FileClick(Sender: TObject);
    procedure btnSHA3_224HmacClick(Sender: TObject);
    procedure btnUSHA3_224Click(Sender: TObject);
    procedure btnSHA3_256Click(Sender: TObject);
    procedure btnFileSHA3_256Click(Sender: TObject);
    procedure btnSHA3_256HmacClick(Sender: TObject);
    procedure btnUSHA3_256Click(Sender: TObject);
    procedure btnSHA3_384Click(Sender: TObject);
    procedure btnSHA3_384FileClick(Sender: TObject);
    procedure btnSHA3_384HmacClick(Sender: TObject);
    procedure btnUSHA3_384Click(Sender: TObject);
    procedure btnSHA3_512Click(Sender: TObject);
    procedure btnSHA3_512FileClick(Sender: TObject);
    procedure btnSHA3_512HmacClick(Sender: TObject);
    procedure btnUSHA3_512Click(Sender: TObject);
    procedure btnZUC1Click(Sender: TObject);
    procedure btnZUCEIA31Click(Sender: TObject);
    procedure btnZUC2Click(Sender: TObject);
    procedure btnZUC3Click(Sender: TObject);
    procedure btnZUC4Click(Sender: TObject);
    procedure btnZUCEIA32Click(Sender: TObject);
    procedure btnZUCEEA31Click(Sender: TObject);
    procedure btnTeaEncClick(Sender: TObject);
    procedure btnTeaDecClick(Sender: TObject);
    procedure btnXTeaEncClick(Sender: TObject);
    procedure btnXTeaDecClick(Sender: TObject);
    procedure btnXXTeaEncClick(Sender: TObject);
    procedure btnXXTeaDecClick(Sender: TObject);
    procedure btnCRC64HmacClick(Sender: TObject);
  private
    procedure InitTeaKeyData;
    function ToHex(Buffer: PAnsiChar; Length: Integer): AnsiString;
    function HexToStr(Hex: string): AnsiString;
  public

  end;

var
  FormCrypt: TFormCrypt;

implementation

uses
  CnMD5, CnDES, CnAES, CnCRC32, CnBase64, CnTEA, CnSM3, CnSM4;

{$R *.fmx}

var
  Sm4Iv: array[0..15] of Byte = (
    $01, $23, $45, $67, $89, $AB, $CD, $EF,
    $FE, $DC, $BA, $98, $76, $54, $32, $10
  );

  AesIv: TAESBuffer = (
    $01, $23, $45, $67, $89, $AB, $CD, $EF,
    $FE, $DC, $BA, $98, $76, $54, $32, $10
  );

  TeaKey: TCnTeaKey;
  TeaData: TCnTeaData;
  TeaEnc: TCnTeaData;

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

procedure TFormCrypt.FormCreate(Sender: TObject);
begin
  // To Implement.
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

procedure TFormCrypt.InitTeaKeyData;
begin
  TeaKey[0] := HexToInt(edtTeaKey1.Text);
  TeaKey[1] := HexToInt(edtTeaKey2.Text);
  TeaKey[2] := HexToInt(edtTeaKey3.Text);
  TeaKey[3] := HexToInt(edtTeaKey4.Text);
  TeaData[0] := HexToInt(edtTeaData1.Text);
  TeaData[1] := HexToInt(edtTeaData2.Text);
end;

procedure TFormCrypt.btnDesCryptClick(Sender: TObject);
begin
  edtCode.Text := DESEncryptStrToHex(edtDesFrom.Text, edtKey.Text);
end;

procedure TFormCrypt.btnDesDecryptClick(Sender: TObject);
begin
  edtOrigin.Text := DESDecryptStrFromHex(edtCode.Text, edtKey.Text);
end;

procedure TFormCrypt.btnMd5Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.ResultDblClick(Sender: TObject);
begin
  // To Implement.
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

procedure TFormCrypt.btnMd5FileClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnMD5HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUMd5Click(Sender: TObject);
begin
  // To Implement.
  ShowMessage(MD5Print(MD5StringW(edtMD5.Text)));
end;

procedure TFormCrypt.Button1Click(Sender: TObject);
var
  S: string;
begin
  Base64Encode(edtBase64from.Text, S);
  edtBase64Result.Text := S;
end;

procedure TFormCrypt.btnBase64DecodeClick(Sender: TObject);
var
  S: AnsiString;
begin
  Base64Decode(edtBase64Result.Text, S);
  edtbase64Decode.Text := S;
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

procedure TFormCrypt.btnDeBase64FileClick(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  if dlgSave.Execute then
  begin
    Stream := TMemoryStream.Create;
    Base64Decode(edtBase64Result.Text, Stream, False);
    Stream.SaveToFile(dlgSave.FileName);
  end;
end;

procedure TFormCrypt.btnCRC32Click(Sender: TObject);
begin
  ShowMessage(IntToHex(StrCRC32A(0, AnsiString(edtCRC32.Text)), 2));
end;

procedure TFormCrypt.btnFileCRC32Click(Sender: TObject);
var
  Crc: LongWord;
begin
  Crc := 0;
  if OpenDialog1.Execute then
    if FileCRC32(OpenDialog1.FileName, Crc) then
      ShowMessage(IntToHex(Crc, 2));
end;

procedure TFormCrypt.btnCRC32HmacClick(Sender: TObject);
var
  Output: LongWord;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtCRC32HmacKey.Text);
  S := AnsiString(edtCRC32.Text);
  Output := CRC32Hmac(@Key[1], Length(Key), @S[1], Length(S));
  ShowMessage(IntToHex(Output, 2));
end;

procedure TFormCrypt.btnCRC64Click(Sender: TObject);
begin
  ShowMessage(IntToHex(StrCRC64A(0, AnsiString(edtCRC64.Text)), 2));
end;

procedure TFormCrypt.btnCRC64HmacClick(Sender: TObject);
var
  Output: Int64;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtCRC64HmacKey.Text);
  S := AnsiString(edtCRC64.Text);
  Output := CRC64Hmac(@Key[1], Length(Key), @S[1], Length(S));
  ShowMessage(IntToHex(Output, 2));
end;

procedure TFormCrypt.btnFileCRC64Click(Sender: TObject);
var
  Crc: Int64;
begin
  Crc := 0;
  if OpenDialog1.Execute then
    if FileCRC64(OpenDialog1.FileName, Crc) then
      ShowMessage(IntToHex(Crc, 2));
end;

procedure TFormCrypt.btnSha1Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnFileSha1Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA1HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA1Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSM3Click(Sender: TObject);
begin
  ShowMessage(SM3Print(SM3StringA(AnsiString(edtSm3.Text))));
end;

procedure TFormCrypt.btnFileSM3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    ShowMessage(SM3Print(SM3File(OpenDialog1.FileName)));
end;

procedure TFormCrypt.btnSM3HmacClick(Sender: TObject);
var
  Output: TSM3Digest;
  S, Key: AnsiString;
begin
  Key := AnsiString(edtSM3HmacKey.Text);
  S := AnsiString(edtSM3.Text);
  SM3Hmac(@Key[1], Length(Key), @S[1], Length(S), Output);
  ShowMessage(SM3Print(Output));
end;

procedure TFormCrypt.btnUSM3Click(Sender: TObject);
begin
  ShowMessage(SM3Print(SM3UnicodeString(edtSm3.Text)));
end;

procedure TFormCrypt.btnSm4Click(Sender: TObject);
var
  Output: AnsiString;
  Len: Integer;
  TmpSm4Iv: array[0..15] of Byte;
begin
  Len := Length(edtSm4.Text);
  if Len < 16 then
    Len := 16
  else
    Len := (((Len - 1) div 16) + 1) * 16;
  SetLength(Output, Len);
  FillChar(Output[1], Len, 0);

  if rbSm4Ecb.IsChecked then
    SM4CryptEcbStr(SM4_ENCRYPT, edtSm4Key.Text, edtSm4.Text, @(Output[1]))
  else
  begin
    Move(Sm4Iv[0], TmpSm4Iv[0], SizeOf(Sm4Iv));
    SM4CryptCbcStr(SM4_ENCRYPT, edtSm4Key.Text, PAnsiChar(@(TmpSm4Iv[0])), edtSm4.Text, @(Output[1]));
  end;
  edtSm4Code.Text := ToHex(@(Output[1]), Length(Output));
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
  FillChar(Output[1], Len, 0);

  if rbSm4Ecb.IsChecked then
    SM4CryptEcbStr(SM4_DECRYPT, edtSm4Key.Text, S, @(Output[1]))
  else
  begin
    Move(Sm4Iv[0], TmpSm4Iv[0], SizeOf(Sm4Iv));
    SM4CryptCbcStr(SM4_DECRYPT, edtSm4Key.Text, PAnsiChar(@(TmpSm4Iv[0])), S, @(Output[1]));
  end;
  edtSm4Dec.Text := Output;
end;

procedure TFormCrypt.btnAesEncryptClick(Sender: TObject);
var
  TmpAesIv: TAESBuffer;
begin
  if rbAesecb.IsChecked then
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
    Move(AesIv, TmpAesIv, SizeOf(TmpAesIv));
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
  if rbAesecb.IsChecked then
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
    Move(AesIv, TmpAesIv, SizeOf(TmpAesIv));
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

procedure TFormCrypt.btnSHA224Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA224FileClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA224HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA224Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA256Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnFileSHA256Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA256HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA256Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA384Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA384FileClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA384HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA384Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA512Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA512FileClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA512HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA512Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_224Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_224FileClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_224HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA3_224Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_256Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnFileSHA3_256Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_256HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA3_256Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_384Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_384FileClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_384HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA3_384Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_512Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_512FileClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnSHA3_512HmacClick(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnUSHA3_512Click(Sender: TObject);
begin
  // To Implement.
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
  KeyStream: array[0..1] of LongWord;
  List: TStringList;
  I: Integer;
begin
  FillChar(Key[0], SizeOf(Key), 0);
  FillChar(IV[0], SizeOf(IV), 0);
  ZUC(PByte(@Key[0]), PByte(@IV[0]), PLongWord(@KeyStream[0]), 2);

  List := TStringList.Create;
  for I := Low(KeyStream) to High(KeyStream) do
    List.Add('$' + IntToHex(KeyStream[I], 2));

  ShowMessage(List.Text);
  List.Free;
end;

procedure TFormCrypt.btnZUCEIA31Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnZUC2Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnZUC3Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnZUC4Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnZUCEIA32Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnZUCEEA31Click(Sender: TObject);
begin
  // To Implement.
end;

procedure TFormCrypt.btnTeaEncClick(Sender: TObject);
begin
  InitTeaKeyData;
  CnTeaEncrypt(TeaKey, TeaData);
  edtTeaEnc1.Text := IntToHex(TeaData[0], 2);
  edtTeaEnc2.Text := IntToHex(TeaData[1], 2);
end;

procedure TFormCrypt.btnTeaDecClick(Sender: TObject);
begin
  InitTeaKeyData;
  TeaEnc[0] := HexToInt(edtTeaEnc1.Text);
  TeaEnc[1] := HexToInt(edtTeaEnc2.Text);
  CnTeaDecrypt(TeaKey, TeaEnc);
  edtTeaEnc1.Text := IntToHex(TeaEnc[0], 2);
  edtTeaEnc2.Text := IntToHex(TeaEnc[1], 2);
end;

procedure TFormCrypt.btnXTeaEncClick(Sender: TObject);
begin
  InitTeaKeyData;
  CnXTeaEncrypt(TeaKey, TeaData);
  edtXTeaEnc1.Text := IntToHex(TeaData[0], 2);
  edtXTeaEnc2.Text := IntToHex(TeaData[1], 2);
end;

procedure TFormCrypt.btnXTeaDecClick(Sender: TObject);
begin
  InitTeaKeyData;
  TeaEnc[0] := HexToInt(edtXTeaEnc1.Text);
  TeaEnc[1] := HexToInt(edtXTeaEnc2.Text);
  CnXTeaDecrypt(TeaKey, TeaEnc);
  edtXTeaEnc1.Text := IntToHex(TeaEnc[0], 2);
  edtXTeaEnc2.Text := IntToHex(TeaEnc[1], 2);
end;

procedure TFormCrypt.btnXXTeaEncClick(Sender: TObject);
begin
  InitTeaKeyData;
  CnXXTeaEncrypt(TeaKey, @TeaData[0], 2);
  edtXXTeaEnc1.Text := IntToHex(TeaData[0], 2);
  edtXXTeaEnc2.Text := IntToHex(TeaData[1], 2);
end;

procedure TFormCrypt.btnXXTeaDecClick(Sender: TObject);
begin
  InitTeaKeyData;
  TeaEnc[0] := HexToInt(edtXXTeaEnc1.Text);
  TeaEnc[1] := HexToInt(edtXXTeaEnc2.Text);
  CnXXTeaDecrypt(TeaKey, @TeaEnc[0], 2);
  edtXXTeaEnc1.Text := IntToHex(TeaEnc[0], 2);
  edtXXTeaEnc2.Text := IntToHex(TeaEnc[1], 2);
end;

end.
