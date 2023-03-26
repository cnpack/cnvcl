unit UnitkKeyDF;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnKDF, FMX.ComboEdit, FMX.Edit, FMX.Types,
  FMX.Controls.Presentation;

type
  TFormKDF = class(TForm)
    grpKeyDerivation: TGroupBox;
    lblPass: TLabel;
    lblSalt: TLabel;
    lblKeyHash: TLabel;
    lblNeedLength: TLabel;
    lblBytes: TLabel;
    lblCount: TLabel;
    lblPBKDF1Hash: TLabel;
    lblPBKDF2Hash: TLabel;
    lblPBKDF2KeyLen: TLabel;
    lblSM2KeyLength: TLabel;
    edtPass: TEdit;
    edtSalt: TEdit;
    cbbLoadKeyHash: TComboEdit;
    edtNeedLength: TEdit;
    btnGetKeyToHex: TButton;
    edtGetKeyToHex: TEdit;
    btnPBKDF1: TButton;
    btnPBKDF2: TButton;
    edtCount: TEdit;
    edtPBKDF1: TEdit;
    edtPBKDF2: TEdit;
    cbbPBKDF1Hash: TComboEdit;
    cbbPBKDF2Hash: TComboEdit;
    edtPBKDF2KeyLength: TEdit;
    btnSM2KDF: TButton;
    edtSM2KDF: TEdit;
    edtSM2KeyLength: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnGetKeyToHexClick(Sender: TObject);
    procedure btnPBKDF1Click(Sender: TObject);
    procedure btnPBKDF2Click(Sender: TObject);
    procedure btnSM2KDFClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormKDF: TFormKDF;

implementation

{$R *.fmx}

function StrToHex(Value: PAnsiChar; Len: Integer): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Len - 1 do
    Result := Result + IntToHex(Ord(Value[I]), 2);
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

function HexToStr(Value: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    if ((I mod 2) = 1) then
      Result := Result + AnsiChar(HexToInt(Copy(Value, I, 2)));
  end;
end;

procedure TFormKDF.btnGetKeyToHexClick(Sender: TObject);
var
  Len: Integer;
  Salt, Keys: AnsiString;
begin
  Len := StrToInt(edtNeedLength.Text);
  if Len <= 0 then
    Exit;

  Salt := HexToStr(edtSalt.Text);
  SetLength(Keys, Len);
  if CnGetDeriveKey(edtPass.Text, Salt, @Keys[1], Len, TCnKeyDeriveHash(cbbLoadKeyHash.ItemIndex)) then
  begin
    edtGetKeyToHex.Text := StrToHex(@Keys[1], Len);
    ShowMessage('Get Key OK.');
  end
  else
    ShowMessage('Get Key Failed');
end;

procedure TFormKDF.FormCreate(Sender: TObject);
begin
  cbbLoadKeyHash.ItemIndex := 0;
  cbbPBKDF1Hash.ItemIndex := 0;
  cbbPBKDF2Hash.ItemIndex := 0;
end;

procedure TFormKDF.btnPBKDF1Click(Sender: TObject);
var
  Pass, Salt, S: AnsiString;
  C: Integer;
begin
  Pass := edtPass.Text;
  Salt := HexToStr(edtSalt.Text);
  C := StrToInt(edtCount.Text);

  if cbbPBKDF1Hash.ItemIndex = 0 then
    S := CnPBKDF1(Pass, Salt, C, 16, cpdfMd5)
  else if cbbPBKDF1Hash.ItemIndex = 1 then
    S := CnPBKDF1(Pass, Salt, C, 20, cpdfSha1);

  edtPBKDF1.Text := StrToHex(PAnsiChar(S), Length(S));
end;

procedure TFormKDF.btnPBKDF2Click(Sender: TObject);
var
  Pass, Salt, S: AnsiString;
  C, K: Integer;
begin
  Pass := edtPass.Text;
  Salt := HexToStr(edtSalt.Text);
  C := StrToInt(edtCount.Text);
  K := StrToInt(edtPBKDF2KeyLength.Text);

  if cbbPBKDF2Hash.ItemIndex = 0 then
    S := CnPBKDF2(Pass, Salt, C, K, cpdfSha1Hmac)
  else if cbbPBKDF2Hash.ItemIndex = 1 then
    S := CnPBKDF2(Pass, Salt, C, K, cpdfSha256Hmac);

  edtPBKDF2.Text := StrToHex(PAnsiChar(S), Length(S));
end;

procedure TFormKDF.btnSM2KDFClick(Sender: TObject);
var
  Pass, S: AnsiString;
  K: Integer;
begin
  // Pass := HexToStr('57E7B63623FAE5F08CDA468E872A20AFA03DED41BF1403770E040DC83AF31A67991F2B01EBF9EFD8881F0A0493000603');
  // K := 19;

  Pass := edtPass.Text;
  K := StrToInt(edtPBKDF2KeyLength.Text);

  S := CnSM2KDF(Pass, K);

  edtSM2KDF.Text := StrToHex(PAnsiChar(S), Length(S));
  // 046B04A9ADF53B389B9E2AAFB47D90F4D08978
end;

end.
