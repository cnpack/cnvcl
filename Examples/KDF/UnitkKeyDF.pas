unit UnitkKeyDF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnKeyDerivation;

type
  TFormKDF = class(TForm)
    grpKeyDerivation: TGroupBox;
    lblPass: TLabel;
    edtPass: TEdit;
    lblSalt: TLabel;
    edtSalt: TEdit;
    lblKeyHash: TLabel;
    cbbLoadKeyHash: TComboBox;
    lblNeedLength: TLabel;
    edtNeedLength: TEdit;
    lblBytes: TLabel;
    btnGetKeyToHex: TButton;
    edtGetKeyToHex: TEdit;
    procedure btnGetKeyToHexClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormKDF: TFormKDF;

implementation

{$R *.DFM}

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
end;

end.
