unit UnitLang;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormLang = class(TForm)
    mmoLangs: TMemo;
    edtText: TEdit;
    lblUtf16: TLabel;
    btnConvert: TButton;
    mmoConvert: TMemo;
    lblHex: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure edtTextChange(Sender: TObject);
  private
    function PtrToHex(Data: PAnsiChar; Size: Integer): AnsiString;
  public
    { Public declarations }
  end;

var
  FormLang: TFormLang;

implementation

uses
  CnLangUtils;

{$R *.DFM}

procedure TFormLang.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  edtText.OnChange(edtText);
  for I := 0 to CnLanguages.Count - 1 do
  begin
    mmoLangs.Lines.Add(Format('$%4.4x %4.4d  %s - %s', [CnLanguages.LocaleID[I],
      CnLanguages.CodePage[I], CnLanguages.Ext[I], CnLanguages.Name[I]]));
  end;
end;

procedure TFormLang.btnConvertClick(Sender: TObject);
var
  I, Len: Integer;
  HS: AnsiString;
  S: AnsiString;
  T: WideString;
begin
  mmoConvert.Lines.Clear;
  T := edtText.Text;
  if T = '' then
    Exit;

  for I := 0 to CnLanguages.Count - 1 do
  begin
    // 针对每个 CodePage 转出 T 对应的内容
    Len := WideCharToMultiByte(CnLanguages.CodePage[I], 0, PWideChar(T), -1, nil, 0, nil, nil);
    if Len <= 0 then
      Continue;

    SetLength(S, Len);
    WideCharToMultiByte(CnLanguages.CodePage[I], 0, PWideChar(T), -1, @S[1], Len, nil, nil);

    HS := PtrToHex(PAnsiChar(S), Length(S));
    mmoConvert.Lines.Add(Format('$%4.4x %4.4d  %s - %s', [CnLanguages.LocaleID[I],
      CnLanguages.CodePage[I], CnLanguages.Ext[I], HS, S]));
  end;

end;

function TFormLang.PtrToHex(Data: PAnsiChar; Size: Integer): AnsiString;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  I: Integer;
  B: Byte;
  Buffer: PAnsiChar;
begin
  Result := '';
  if Size = 0 then
    Exit;

  Buffer := Data;
  for I := 0 to Size - 1 do
  begin
    B := PByte(Integer(Buffer) + I)^;
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}
      (Digits[(B shr 4) and $0F] + Digits[B and $0F]);
  end;

end;

procedure TFormLang.edtTextChange(Sender: TObject);
var
  S: WideString;
begin
  S := edtText.Text;
  if S <> '' then
    lblHex.Caption := string(PtrToHex(PAnsiChar(S), Length(S) * SizeOf(WideChar)))
  else
    lblHex.Caption := '';
end;

end.
