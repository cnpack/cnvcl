unit Unit18030;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnStrings, CnWideStrings, CnGB18030;

type
  TFormGB18030 = class(TForm)
    btnCodePointFromUtf161: TButton;
    btnCodePointFromUtf162: TButton;
    btnUtf16CharLength: TButton;
    btnUtf8CharLength: TButton;
    btnUtf8Encode: TButton;
    btnCodePointUtf16: TButton;
    btnCodePointUtf162: TButton;
    btnUtf8Decode: TButton;
    btnGenUtf16: TButton;
    dlgSave1: TSaveDialog;
    btnCodePointUtf163: TButton;
    btnGB18030ToUtf16: TButton;
    btn18030CodePoint1: TButton;
    btn18030CodePoint2: TButton;
    btn18030CodePoint3: TButton;
    btnCodePoint180301: TButton;
    btnCodePoint180302: TButton;
    btnCodePoint180303: TButton;
    btnMultiUtf16ToGB18130: TButton;
    btnMultiGB18131ToUtf16: TButton;
    btnGenGB18030Page: TButton;
    btnGenUtf16Page: TButton;
    procedure btnCodePointFromUtf161Click(Sender: TObject);
    procedure btnCodePointFromUtf162Click(Sender: TObject);
    procedure btnUtf16CharLengthClick(Sender: TObject);
    procedure btnUtf8CharLengthClick(Sender: TObject);
    procedure btnUtf8EncodeClick(Sender: TObject);
    procedure btnCodePointUtf16Click(Sender: TObject);
    procedure btnCodePointUtf162Click(Sender: TObject);
    procedure btnUtf8DecodeClick(Sender: TObject);
    procedure btnGenUtf16Click(Sender: TObject);
    procedure btnCodePointUtf163Click(Sender: TObject);
    procedure btnGB18030ToUtf16Click(Sender: TObject);
    procedure btn18030CodePoint1Click(Sender: TObject);
    procedure btn18030CodePoint2Click(Sender: TObject);
    procedure btn18030CodePoint3Click(Sender: TObject);
    procedure btnCodePoint180301Click(Sender: TObject);
    procedure btnCodePoint180302Click(Sender: TObject);
    procedure btnCodePoint180303Click(Sender: TObject);
    procedure btnMultiUtf16ToGB18130Click(Sender: TObject);
    procedure btnMultiGB18131ToUtf16Click(Sender: TObject);
    procedure btnGenGB18030PageClick(Sender: TObject);
    procedure btnGenUtf16PageClick(Sender: TObject);
  private
    procedure GenUtf16Page(Page: Byte; Content: TCnWideStringList);
    function CodePointUtf16ToGB18130(UCP: TCnCodePoint): TCnCodePoint;
    function CodePointGB18130ToUtf16(GBCP: TCnCodePoint): TCnCodePoint;
    procedure Gen2GB18030ToUtf16Page(FromH, FromL, ToH, ToL: Byte; Content: TCnWideStringList; H2: Word = 0);
    procedure Gen2Utf16ToGB18030Page(FromH, FromL, ToH, ToL: Byte; Content: TCnAnsiStringList; H2: Word = 0);
  public

  end;

var
  FormGB18030: TFormGB18030;

implementation

uses
  CnNative;

{$R *.DFM}

const
  FACE: array[0..3] of Byte = ($3D, $D8, $02, $DE);      // 笑哭了的表情符的 UTF16-LE 表示
  FACE_UTF8: array[0..3] of Byte = ($F0, $9F, $98, $82); // 笑哭了的表情符的 UTF8-MB4 表示

  CP_GB18030 = 54936;

procedure TFormGB18030.btnCodePointFromUtf161Click(Sender: TObject);
var
  A: AnsiString;
  S: WideString;
  C: Cardinal;
begin
  A := '吃饭'; // 内存中是 B3D4B7B9，GB18030内码也是 B3D4 和 B7B9符合阅读顺序
  S := '吃饭'; // 内存中是 03546D99，但 Unicode 内码却是 5403 和 996D，有反置
  C := GetCodePointFromUtf16Char(PWideChar(S));
  ShowMessage(IntToHex(C, 2));
end;

procedure TFormGB18030.btnCodePointFromUtf162Click(Sender: TObject);
var
  S: WideString;
  C: Cardinal;
begin
  SetLength(S, 2);
  Move(FACE[0], S[1], 4);

  C := GetCodePointFromUtf16Char(PWideChar(S));
  ShowMessage(IntToHex(C, 2)); // 应得到 $1F602
end;

procedure TFormGB18030.btnUtf16CharLengthClick(Sender: TObject);
var
  S: WideString;
  C: Cardinal;
begin
  SetLength(S, 2);
  Move(FACE[0], S[1], 4);

  C := GetCharLengthFromUtf16(PWideChar(S));
  ShowMessage(IntToStr(C)); // 应得到 1
end;

procedure TFormGB18030.btnUtf8CharLengthClick(Sender: TObject);
var
  S: AnsiString;
  C: Cardinal;
begin
  SetLength(S, 4);
  Move(FACE_UTF8[0], S[1], 4);

  C := GetCharLengthFromUtf8(PAnsiChar(S));
  ShowMessage(IntToStr(C)); // 应得到 1
end;

procedure TFormGB18030.btnUtf8EncodeClick(Sender: TObject);
var
  S: WideString;
  R: AnsiString;
begin
  SetLength(S, 2);
  Move(FACE[0], S[1], 4);

  R := CnUtf8EncodeWideString(S);
  if R <> '' then
    ShowMessage(DataToHex(@R[1], Length(R)))  // F09F9882
  else
    ShowMessage('Error');
end;

procedure TFormGB18030.btnCodePointUtf16Click(Sender: TObject);
var
  S: WideString;
  C: Cardinal;
begin
  C := $5403;  // 吃的 Unicode 码点
  SetLength(S, 1);
  GetUtf16CharFromCodePoint(C, @S[1]);
  ShowMessage(S);
end;

procedure TFormGB18030.btnCodePointUtf162Click(Sender: TObject);
var
  S: WideString;
  C: Cardinal;
begin
  C := $1F602;  // 笑哭了的表情符的 Unicode 码点
  SetLength(S, 2);
  GetUtf16CharFromCodePoint(C, @S[1]);
  ShowMessage(DataToHex(@S[1], Length(S) * SizeOf(WideChar))); // $3DD802DE
end;

procedure TFormGB18030.btnUtf8DecodeClick(Sender: TObject);
var
  S: AnsiString;
  R: WideString;
begin
  SetLength(S, 4);
  Move(FACE_UTF8[0], S[1], 4);

  R := CnUtf8DecodeToWideString(S);
  if R <> '' then
    ShowMessage(DataToHex(@R[1], Length(R) * SizeOf(WideChar)))  // 3DD802DE
  else
    ShowMessage('Error');
end;

procedure TFormGB18030.btnGenUtf16Click(Sender: TObject);
var
  I: Integer;
  WS: TCnWideStringList;
begin
  // 0000 ~ FFFF，一行 0~F，一页 0~F，共 255 页
  WS := TCnWideStringList.Create;
  Screen.Cursor := crHourGlass;

  try
    for I := 0 to 255 do
    begin
      WS.Add('');
      GenUtf16Page(I, WS);
    end;

    dlgSave1.FileName := 'UTF16.txt';
    if dlgSave1.Execute then
      WS.SaveToFile(dlgSave1.FileName);
  finally
    Screen.Cursor := crDefault;
    WS.Free;
  end;
end;

procedure TFormGB18030.GenUtf16Page(Page: Byte; Content: TCnWideStringList);
var
  R, C: Byte;
  S, T: WideString;
begin
  S := '    ';
  for C := 0 to $F do
    S := S + ' ' + IntToHex(C, 2);
  Content.Add(S);

  SetLength(T, 1);
  for R := 0 to $F do
  begin
    S := IntToHex(Page, 2) + IntToHex(16 * R, 2);
    for C := 0 to $F do
    begin
      GetUtf16CharFromCodePoint(Page * 256 + R * 16 + C, @T[1]);
      S := S + ' ' + T;
    end;
    Content.Add(S);
  end;
end;

procedure TFormGB18030.btnCodePointUtf163Click(Sender: TObject);
var
  S: WideString;
  C: Cardinal;
begin
  C := $20BB7;  // 上土下口 的 Unicode 码点
  SetLength(S, 2);
  GetUtf16CharFromCodePoint(C, @S[1]);
  ShowMessage(DataToHex(@S[1], Length(S) * SizeOf(WideChar))); // $42D8B7DF
end;

procedure TFormGB18030.btnGB18030ToUtf16Click(Sender: TObject);
var
  S: AnsiString;
  W: WideString;
  C: Integer;
begin
  S := '吃饭';

  C := MultiByteToWideChar(CP_GB18030, 0, @S[1], Length(S), nil, 0);
  if C > 0 then
  begin
    SetLength(W, C);
    C := MultiByteToWideChar(CP_GB18030, 0, @S[1], Length(S), @W[1], Length(W));

    if C > 0 then
    begin
      ShowMessage(W);

      C := WideCharToMultiByte(CP_GB18030, 0, @W[1], Length(W), nil, 0, nil, nil);
      if C > 0 then
      begin
        SetLength(S, C);

        C := WideCharToMultiByte(CP_GB18030, 0, @W[1], Length(W), @S[1], Length(S), nil, nil);
        if C > 0 then
          ShowMessage(S);
      end;
    end;
  end;
end;

procedure TFormGB18030.btn18030CodePoint1Click(Sender: TObject);
var
  S: AnsiString;
  C: TCnCodePoint;
begin
  S := 'A';
  C := GetCodePointFromGB18030Char(@S[1]);
  ShowMessage(IntToHex(C, 2));              // 41
end;

procedure TFormGB18030.btn18030CodePoint2Click(Sender: TObject);
var
  S: AnsiString;
  C: TCnCodePoint;
begin
  S := '吃';
  C := GetCodePointFromGB18030Char(@S[1]);
  ShowMessage(IntToHex(C, 2));              // B3D4
end;

procedure TFormGB18030.btn18030CodePoint3Click(Sender: TObject);
var
  S: AnsiString;
  C: TCnCodePoint;
begin
  SetLength(S, 4);
  S[1] := #$82;   // 左女右敖
  S[2] := #$30;
  S[3] := #$C2;
  S[4] := #$30;

  C := GetCodePointFromGB18030Char(@S[1]);
  ShowMessage(IntToHex(C, 2));              // 8230C230
end;

procedure TFormGB18030.btnCodePoint180301Click(Sender: TObject);
var
  S: AnsiString;
  C: Cardinal;
begin
  C := $42;  // B 的 GB18030 码点
  SetLength(S, 1);
  GetGB18030CharsFromCodePoint(C, @S[1]);
  ShowMessage(S);
end;

procedure TFormGB18030.btnCodePoint180302Click(Sender: TObject);
var
  S: AnsiString;
  C: Cardinal;
begin
  C := $B3D4;  // 吃的 GB18030 码点
  SetLength(S, 2);
  GetGB18030CharsFromCodePoint(C, @S[1]);
  ShowMessage(S);
end;

procedure TFormGB18030.btnCodePoint180303Click(Sender: TObject);
var
  S: AnsiString;
  C: Cardinal;
begin
  C := $8139EF34;  // 一个大叉 GB18030 码点
  SetLength(S, 4);
  GetGB18030CharsFromCodePoint(C, @S[1]);   // 内存中得到 8139EF34
  ShowMessage(S);
end;

function TFormGB18030.CodePointGB18130ToUtf16(GBCP: TCnCodePoint): TCnCodePoint;
var
  S: AnsiString;
  W: WideString;
  C, T: Integer;
begin
  Result := CN_INVALID_CODEPOINT;

  SetLength(S, 4); // 最多 4
  C := GetGB18030CharsFromCodePoint(GBCP, @S[1]);  // S 是该 GB18030 字符，C 是其字节长度

  T := MultiByteToWideChar(CP_GB18030, 0, @S[1], C, nil, 0);
  if T > 0 then
  begin
    SetLength(W, T);
    T := MultiByteToWideChar(CP_GB18030, 0, @S[1], C, @W[1], Length(W));
    if T > 0 then
      Result := GetCodePointFromUtf16Char(@W[1]);
  end;
end;

function TFormGB18030.CodePointUtf16ToGB18130(UCP: TCnCodePoint): TCnCodePoint;
var
  S: AnsiString;
  W: WideString;
  C, T: Integer;
begin
  Result := CN_INVALID_CODEPOINT;

  SetLength(W, 2); // 最多 2 个宽字符也就是四字节
  C := GetUtf16CharFromCodePoint(UCP, @W[1]);  // W 是该 Utf16 字符，C 是其宽字符长度

  T := WideCharToMultiByte(CP_GB18030, 0, @W[1], C, nil, 0, nil, nil);
  if T > 0 then
  begin
    SetLength(S, T);

    T := WideCharToMultiByte(CP_GB18030, 0, @W[1], C, @S[1], Length(S), nil, nil);
    if T > 0 then
      Result := GetCodePointFromGB18030Char(@S[1]);
  end;
end;

procedure TFormGB18030.btnMultiUtf16ToGB18130Click(Sender: TObject);
var
  S: WideString;
  A, T: AnsiString;
  I: Integer;
  C: TCnCodePoint;
begin
  S := '吃饭一半少，睡觉三周多';
  A := '';
  for I := 1 to Length(S) do
  begin
    C := GetCodePointFromUtf16Char(@S[I]);   // Utf16 值
    if C = CN_INVALID_CODEPOINT then
      Exit;

    C := CodePointUtf16ToGB18130(C);         // 转成 GB18030
    if C = CN_INVALID_CODEPOINT then
      Exit;

    SetLength(T, 4);
    C := GetGB18030CharsFromCodePoint(C, @T[1]);
    if C > 0 then
      SetLength(T, C);

    A := A + T;
  end;
  ShowMessage(A);
end;

procedure TFormGB18030.btnMultiGB18131ToUtf16Click(Sender: TObject);
var
  S, T: WideString;
  A: AnsiString;
  I: Integer;
  C: TCnCodePoint;
begin
  A := '吃饭一半少，睡觉三周多';
  S := '';
  I := 1;

  while I <= Length(A) do
  begin
    C := GetCodePointFromGB18030Char(@A[I]);   // GB18030 值
    Inc(I, 2);

    if C = CN_INVALID_CODEPOINT then
      Exit;

    C := CodePointGB18130ToUtf16(C);           // 转成 Utf16
    if C = CN_INVALID_CODEPOINT then
      Exit;

    SetLength(T, 1);
    GetUtf16CharFromCodePoint(C, @T[1]);

    S := S + T;
  end;
  ShowMessage(S);
end;

procedure TFormGB18030.Gen2GB18030ToUtf16Page(FromH, FromL, ToH, ToL: Byte;
  Content: TCnWideStringList; H2: Word);
var
  H, L, T: Integer;
  GBCP, UCP: TCnCodePoint;
  S, C: WideString;
begin
  for H := FromH to ToH do
  begin
    for L := FromL to ToL do
    begin
      GBCP := ((H shl 8) or L) + (H2 shl 16);
      UCP := CodePointGB18130ToUtf16(GBCP);
      T := GetUtf16CharFromCodePoint(UCP, nil);
      SetLength(C, T);
      GetUtf16CharFromCodePoint(UCP, @C[1]);

      S := IntToHex(GBCP, 2) + ' = ' + IntToHex(UCP, 2) + '  ' + C;
      Content.Add(S);
    end;
  end;
end;

procedure TFormGB18030.btnGenGB18030PageClick(Sender: TObject);
var
  WS: TCnWideStringList;
begin
  WS := TCnWideStringList.Create;
// 双字节：A1A9~A1FE                     1 区
//         A840~A97E, A880~A9A0          5 区
//         B0A1~F7FE                     2 区汉字
//         8140~A07E, 8180~A0FE          3 区汉字
//         AA40~FE7E, AA80~FEA0          4 区汉字
//         AAA1~AFFE                     用户 1 区
//         F8A1~FEFE                     用户 2 区
//         A140~A77E, A180~A7A0          用户 3 区

  Gen2GB18030ToUtf16Page($A1, $A9, $A1, $FE, WS);
  WS.Add('');
  Gen2GB18030ToUtf16Page($A8, $40, $A9, $7E, WS);
  Gen2GB18030ToUtf16Page($A8, $80, $A9, $A0, WS);
  WS.Add('');
  Gen2GB18030ToUtf16Page($B0, $A1, $F7, $FE, WS);
  WS.Add('');
  Gen2GB18030ToUtf16Page($81, $40, $A0, $7E, WS);
  Gen2GB18030ToUtf16Page($81, $80, $A0, $FE, WS);
  WS.Add('');
  Gen2GB18030ToUtf16Page($AA, $40, $FE, $7E, WS);
  Gen2GB18030ToUtf16Page($AA, $80, $FE, $A0, WS);
  WS.Add(''); // 双字节用户区
  Gen2GB18030ToUtf16Page($AA, $A1, $AF, $FE, WS);
  WS.Add('');
  Gen2GB18030ToUtf16Page($F8, $A1, $FE, $FE, WS);
  WS.Add('');
  Gen2GB18030ToUtf16Page($A1, $40, $A7, $7E, WS);
  Gen2GB18030ToUtf16Page($A1, $80, $A7, $A0, WS);

  dlgSave1.FileName := 'GB18030_UTF16.txt';
  if dlgSave1.Execute then
    WS.SaveToFile(dlgSave1.FileName);
  WS.Free;
end;

procedure TFormGB18030.btnGenUtf16PageClick(Sender: TObject);
var
  SL: TCnAnsiStringList;
begin
  SL := TCnAnsiStringList.Create;

  Gen2Utf16ToGB18030Page(0, 0, $FF, $FF, SL);
  // Gen2Utf16ToGB18030Page($54, $03, $54, $03, SL);
  dlgSave1.FileName := 'UTF16_GB18030.txt';
  if dlgSave1.Execute then
    SL.SaveToFile(dlgSave1.FileName);
  SL.Free;
end;

procedure TFormGB18030.Gen2Utf16ToGB18030Page(FromH, FromL, ToH, ToL: Byte;
  Content: TCnAnsiStringList; H2: Word);
var
  H, L, T: Integer;
  GBCP, UCP: TCnCodePoint;
  S, C: AnsiString;
begin
  for H := FromH to ToH do
  begin
    for L := FromL to ToL do
    begin
      UCP := ((H shl 8) or L) + (H2 shl 16);
      GBCP := CodePointUtf16ToGB18130(UCP);
      if GBCP <> CN_INVALID_CODEPOINT then
      begin
        T := GetGB18030CharsFromCodePoint(GBCP, nil);
        SetLength(C, T);
        GetGB18030CharsFromCodePoint(GBCP, @C[1]);

        S := IntToHex(UCP, 2) + ' = ' + IntToHex(GBCP, 2) + '  ' + C;
      end
      else
        S := IntToHex(UCP, 2) + ' = ';

      Content.Add(S);
    end;
  end;
end;

end.
