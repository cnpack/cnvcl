unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    grpdES: TGroupBox;
    lbl1: TLabel;
    lblKey: TLabel;
    lblCode: TLabel;
    lblOrigin: TLabel;
    edt1: TEdit;
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
    ts3: TTabSheet;
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
    ts64: TTabSheet;
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  CnMD5, CnDES, CnBase64, CnCRC32, CnSHA1, CnSM3;

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
  edtCode.Text := DESEncryptStrToHex(edt1.Text, edtKey.Text);
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
  S := SM3Print(SM3(AnsiString(edtSm3.Text)));
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

end.
