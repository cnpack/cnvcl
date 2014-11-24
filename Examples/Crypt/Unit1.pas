unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
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
  private
    { Private declarations }
    function ToHex(Buffer: PAnsiChar; Length: Integer): AnsiString;
    function HexToStr(Hex: string): AnsiString;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  CnMD5, CnDES, CnBase64, CnCRC32, CnSHA1, CnSM3, CnSM4;

{$R *.DFM}

procedure TForm1.btnMd5Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlMd5.Caption := MD5Print(MD5StringA(AnsiString(edtFrom.Text)));
{$ELSE}
  pnlMd5.Caption := MD5Print(MD5String(edtFrom.Text));
{$ENDIF}
end;

procedure TForm1.btnDesCryptClick(Sender: TObject);
begin
  edtCode.Text := DESEncryptStrToHex(edtDesFrom.Text, edtKey.Text);
end;

procedure TForm1.btnDesDecryptClick(Sender: TObject);
begin
  edtOrigin.Text := DESDecryptStrFromHex(edtCode.Text, edtKey.Text);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  S: string;
begin
  Base64Encode(edtBase64from.Text, S);
  edt3.Text := S;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S: AnsiString;
begin
  Base64Decode(edt3.Text, S);
  edt4.Text := S;
end;

procedure TForm1.btnCRC32Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlCRC32.Caption := IntToHex(StrCRC32A(0, AnsiString(edtCRC32.Text)), 2);
{$ELSE}
  pnlCRC32.Caption := IntToHex(StrCRC32(0, edtCRC32.Text), 2);
{$ENDIF}
end;

procedure TForm1.btnMd5FileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlMd5.Caption := MD5Print(MD5File(OpenDialog1.FileName));
end;

procedure TForm1.btnFileCRC32Click(Sender: TObject);
var
  Crc: DWORD;
begin
  Crc := 0;
  if OpenDialog1.Execute then
    if FileCRC32(OpenDialog1.FileName, Crc) then
      pnlCRC32.Caption := IntToHex(Crc, 2);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.btnCRC64Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlCRC64.Caption := IntToHex(StrCRC64A(0, AnsiString(edtCRC64.Text)), 2);
{$ELSE}
  pnlCRC64.Caption := IntToHex(StrCRC64(0, edtCRC64.Text), 2);
{$ENDIF}
end;

procedure TForm1.btnFileCRC64Click(Sender: TObject);
var
  Crc: Int64;
begin
  Crc := 0;
  if OpenDialog1.Execute then
    if FileCRC64(OpenDialog1.FileName, Crc) then
      pnlCRC64.Caption := IntToHex(Crc, 2);
end;

procedure TForm1.btnSha1Click(Sender: TObject);
begin
{$IFDEF UNICODE}
  pnlSha1.Caption := SHA1Print(SHA1StringA(AnsiString(edtSha1.Text)));
{$ELSE}
  pnlSha1.Caption := SHA1Print(SHA1String(edtSha1.Text));
{$ENDIF}
end;

procedure TForm1.btnFileSha1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    pnlSha1.Caption := SHA1Print(SHA1File(OpenDialog1.FileName));
end;

procedure TForm1.btnSM3Click(Sender: TObject);
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

procedure TForm1.btnFileSM3Click(Sender: TObject);
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

procedure TForm1.btnSm4Click(Sender: TObject);
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

function TForm1.ToHex(Buffer: PAnsiChar; Length: Integer): AnsiString;
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
    Result := Result + {$IFDEF DELPHI12_UP}string{$ENDIF}
      (Digits[(B shr 4) and $0F] + Digits[B and $0F]);
  end;
end;

procedure TForm1.btnSm4DecClick(Sender: TObject);
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

function TForm1.HexToStr(Hex: string): AnsiString;
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

end.
