unit UnitOneTimePassword;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormOneTimePassword = class(TForm)
    btnGenerate: TButton;
    procedure btnGenerateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOneTimePassword: TFormOneTimePassword;

implementation

uses
  CnOneTimePassword;

{$R *.DFM}

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

procedure TFormOneTimePassword.btnGenerateClick(Sender: TObject);
var
  T: TCnDynamicToken;
  K, Q: AnsiString;
begin
  T := TCnDynamicToken.Create;

  K := HexToStr('1234567890abcdef1234567890abcdef');
  T.SetSeedKey(@K[1], Length(K));

  Q := '5678';
  T.SetChallengeCode(@Q[1], Length(Q));

  T.SetCounter(1234);

  T.Period := 1;

  T.PasswordType := copSM4;

  ShowMessage(T.OneTimePassword);

  T.Free;
end;

end.
