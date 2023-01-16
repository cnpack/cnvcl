{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDHibernateStringUtils;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate基础库
* 单元名称：字符串函数库
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  SysUtils, Windows;

type
  TCnCharSet = TSysCharSet;

function StrToOem(const AnsiStr: string): string;
function OemToAnsiStr(const OemStr: string): string;
function IsEmptyStr(const S: string; const EmptyChars: TCnCharSet): Boolean;
function ReplaceStr(const S, Srch, Replace: string): string;
function DelSpace(const S: string): string;
function DelChars(const S: string; Chr: Char): string;
function DelBSpace(const S: string): string;
function DelESpace(const S: string): string;
function DelRSpace(const S: string): string;
function DelSpace1(const S: string): string;
function Tab2Space(const S: string; Numb: Byte): string;
function NPos(const C: string; S: string; N: Integer): Integer;
function MakeStr(C: Char; N: Integer): string;
function MS(C: Char; N: Integer): string;
function AddChar(C: Char; const S: string; N: Integer): string;
function AddCharR(C: Char; const S: string; N: Integer): string;
function LeftStr(const S: string; N: Integer): string;
function RightStr(const S: string; N: Integer): string;
function CenterStr(const S: string; Len: Integer): string;
function CompStr(const S1, S2: string): Integer;
function CompText(const S1, S2: string): Integer;
function Copy2Symb(const S: string; Symb: Char): string;
function Copy2SymbDel(var S: string; Symb: Char): string;
function Copy2Space(const S: string): string;
function Copy2SpaceDel(var S: string): string;
function AnsiProperCase(const S: string; const WordDelims: TCnCharSet): string;
function WordCount(const S: string; const WordDelims: TCnCharSet): Integer;
function WordPosition(const N: Integer; const S: string;
  const WordDelims: TCnCharSet): Integer;
function ExtractWord(N: Integer; const S: string;
  const WordDelims: TCnCharSet): string;
function ExtractWordPos(N: Integer; const S: string;
  const WordDelims: TCnCharSet; var Pos: Integer): string;
function ExtractDelimited(N: Integer; const S: string;
  const Delims: TCnCharSet): string;
function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TCnCharSet): string;
function IsWordPresent(const W, S: string; const WordDelims: TCnCharSet): Boolean;
function QuotedString(const S: string; Quote: Char): string;
function ExtractQuotedString(const S: string; Quote: Char): string;
function FindPart(const HelpWilds, InputStr: string): Integer;
function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;
function XorString(const Key, Src: ShortString): ShortString;
function XorEncode(const Key, Source: string): string;
function XorDecode(const Key, Source: string): string;
function GetCmdLineArg(const Switch: string; SwitchChars: TCnCharSet): string;
function Numb2USA(const S: string): string;
function Dec2Hex(N: Longint; A: Byte): string;
function D2H(N: Longint; A: Byte): string;
function Hex2Dec(const S: string): Longint;
function H2D(const S: string): Longint;
function Dec2Numb(N: Longint; A, B: Byte): string;
function Numb2Dec(S: string; B: Byte): Longint;
function IntToBin(Value: Longint; Digits, Spaces: Integer): string;
function IntToRoman(Value: Longint): string;
function RomanToInt(const S: string): Longint;

const
  CRLF              = #13#10;
  DigitChars        = ['0'..'9'];
  Brackets          = ['(', ')', '[', ']', '{', '}'];
  StdWordDelims     = [#0..' ', ',', '.', ';', '/', '\', ':', '''', '"', '`'] +
    Brackets;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

function StrToOem(const AnsiStr: string): string;
begin
  SetLength(Result, Length(AnsiStr));
  if Length(Result) > 0 then
    CharToOemBuff({$IFDEF UNICODE} PWideChar{$ELSE}PAnsiChar{$ENDIF}(AnsiStr), PAnsiChar({$IFDEF UNICODE}AnsiString{$ENDIF}(Result)), Length(Result));
end;

function OemToAnsiStr(const OemStr: string): string;
begin
  SetLength(Result, Length(OemStr));
  if Length(Result) > 0 then
    OemToCharBuff(PAnsiChar({$IFDEF UNICODE}AnsiString{$ENDIF}(OemStr)), {$IFDEF UNICODE} PWideChar{$ELSE}PAnsiChar{$ENDIF}(Result), Length(Result));
end;

function IsEmptyStr(const S: string; const EmptyChars: TCnCharSet): Boolean;
var
  I, SLen           : Integer;
begin
  SLen := Length(S);
  I := 1;
  while I <= SLen do
  begin
    if not ({$IFDEF UNICODE}CharInSet(S[I], EmptyChars){$ELSE}S[I] in EmptyChars{$ENDIF}) then
    begin
      Result := False;
      Exit;
    end
    else
      Inc(I);
  end;
  Result := True;
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I                 : Integer;
  Source            : string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then
    begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else
      Result := Result + Source;
  until I <= 0;
end;

function DelSpace(const S: string): string;
begin
  Result := DelChars(S, ' ');
end;

function DelChars(const S: string; Chr: Char): string;
var
  I                 : Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
  begin
    if Result[I] = Chr then
      Delete(Result, I, 1);
  end;
end;

function DelBSpace(const S: string): string;
var
  I, L              : Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do
    Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function DelESpace(const S: string): string;
var
  I                 : Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function DelRSpace(const S: string): string;
begin
  Result := DelBSpace(DelESpace(S));
end;

function DelSpace1(const S: string): string;
var
  I                 : Integer;
begin
  Result := S;
  for I := Length(Result) downto 2 do
  begin
    if (Result[I] = ' ') and (Result[I - 1] = ' ') then
      Delete(Result, I, 1);
  end;
end;

function Tab2Space(const S: string; Numb: Byte): string;
var
  I                 : Integer;
begin
  I := 1;
  Result := S;
  while I <= Length(Result) do
  begin
    if Result[I] = Chr(9) then
    begin
      Delete(Result, I, 1);
      Insert(MakeStr(' ', Numb), Result, I);
      Inc(I, Numb);
    end
    else
      Inc(I);
  end;
end;

function MakeStr(C: Char; N: Integer): string;
begin
  if N < 1 then
    Result := ''
  else
  begin
    SetLength(Result, N);
    FillChar(Result[1], Length(Result), C);
  end;
end;

function MS(C: Char; N: Integer): string;
begin
  Result := MakeStr(C, N);
end;

function NPos(const C: string; S: string; N: Integer): Integer;
var
  I, P, K           : Integer;
begin
  Result := 0;
  K := 0;
  for I := 1 to N do
  begin
    P := Pos(C, S);
    Inc(K, P);
    if (I = N) and (P > 0) then
    begin
      Result := K;
      Exit;
    end;
    if P > 0 then
      Delete(S, 1, P)
    else
      Exit;
  end;
end;

function AddChar(C: Char; const S: string; N: Integer): string;
begin
  if Length(S) < N then
    Result := MakeStr(C, N - Length(S)) + S
  else
    Result := S;
end;

function AddCharR(C: Char; const S: string; N: Integer): string;
begin
  if Length(S) < N then
    Result := S + MakeStr(C, N - Length(S))
  else
    Result := S;
end;

function LeftStr(const S: string; N: Integer): string;
begin
  Result := AddCharR(' ', S, N);
end;

function RightStr(const S: string; N: Integer): string;
begin
  Result := AddChar(' ', S, N);
end;

function CompStr(const S1, S2: string): Integer;
begin
  Result := CompareString(GetThreadLocale, SORT_STRINGSORT, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) - 2;
end;

function CompText(const S1, S2: string): Integer;
begin
  Result := CompareString(GetThreadLocale, SORT_STRINGSORT or NORM_IGNORECASE,
    PChar(S1), Length(S1), PChar(S2), Length(S2)) - 2;
end;

function Copy2Symb(const S: string; Symb: Char): string;
var
  P                 : Integer;
begin
  P := Pos(Symb, S);
  if P = 0 then
    P := Length(S) + 1;
  Result := Copy(S, 1, P - 1);
end;

function Copy2SymbDel(var S: string; Symb: Char): string;
begin
  Result := Copy2Symb(S, Symb);
  S := DelBSpace(Copy(S, Length(Result) + 1, Length(S)));
end;

function Copy2Space(const S: string): string;
begin
  Result := Copy2Symb(S, ' ');
end;

function Copy2SpaceDel(var S: string): string;
begin
  Result := Copy2SymbDel(S, ' ');
end;

function AnsiProperCase(const S: string; const WordDelims: TCnCharSet): string;
var
  SLen, I           : Cardinal;
begin
  Result := AnsiLowerCase(S);
  I := 1;
  SLen := Length(Result);
  while I <= SLen do
  begin
    while (I <= SLen) and ({$IFDEF UNICODE}CharInSet(Result[I], WordDelims){$ELSE}Result[I] in WordDelims{$ENDIF}) do
      Inc(I);
    if I <= SLen then
      Result[I] := AnsiUpperCase(Result[I])[1];
    while (I <= SLen) and not ({$IFDEF UNICODE}CharInSet(Result[I], WordDelims){$ELSE}Result[I] in WordDelims{$ENDIF}) do
      Inc(I);
  end;
end;

function WordCount(const S: string; const WordDelims: TCnCharSet): Integer;
var
  SLen, I           : Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do
  begin
    while (I <= SLen) and ({$IFDEF UNICODE}CharInSet(S[I], WordDelims){$ELSE}S[I] in WordDelims{$ENDIF}) do
      Inc(I);
    if I <= SLen then
      Inc(Result);
    while (I <= SLen) and not ({$IFDEF UNICODE}CharInSet(S[I], WordDelims){$ELSE}S[I] in WordDelims{$ENDIF}) do
      Inc(I);
  end;
end;

function WordPosition(const N: Integer; const S: string;
  const WordDelims: TCnCharSet): Integer;
var
  Count, I          : Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
  begin

    while (I <= Length(S)) and ({$IFDEF UNICODE}CharInSet(S[I], WordDelims){$ELSE}S[I] in WordDelims{$ENDIF}) do
      Inc(I);

    if I <= Length(S) then
      Inc(Count);

    if Count <> N then
      while (I <= Length(S)) and not ({$IFDEF UNICODE}CharInSet(S[I], WordDelims){$ELSE}S[I] in WordDelims{$ENDIF}) do
        Inc(I)
    else
      Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string;
  const WordDelims: TCnCharSet): string;
var
  I                 : Integer;
  Len               : Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then

    while (I <= Length(S)) and not ({$IFDEF UNICODE}CharInSet(S[I], WordDelims){$ELSE}S[I] in WordDelims{$ENDIF}) do
    begin

      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

function ExtractWordPos(N: Integer; const S: string;
  const WordDelims: TCnCharSet; var Pos: Integer): string;
var
  I, Len            : Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  Pos := I;
  if I <> 0 then

    while (I <= Length(S)) and not ({$IFDEF UNICODE}CharInSet(S[I], WordDelims){$ELSE}S[I] in WordDelims{$ENDIF}) do
    begin

      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

function ExtractDelimited(N: Integer; const S: string;
  const Delims: TCnCharSet): string;
var
  CurWord           : Integer;
  I, Len, SLen      : Integer;
begin
  CurWord := 0;
  I := 1;
  Len := 0;
  SLen := Length(S);
  SetLength(Result, 0);
  while (I <= SLen) and (CurWord <> N) do
  begin
    if {$IFDEF UNICODE}CharInSet(S[I], Delims){$ELSE}S[I] in Delims{$ENDIF} then
      Inc(CurWord)
    else
    begin
      if CurWord = N - 1 then
      begin
        Inc(Len);
        SetLength(Result, Len);
        Result[Len] := S[I];
      end;
    end;
    Inc(I);
  end;
end;

function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TCnCharSet): string;
var
  I                 : Integer;
begin
  I := Pos;
  while (I <= Length(S)) and not ({$IFDEF UNICODE}CharInSet(S[I], Delims){$ELSE}S[I] in Delims{$ENDIF}) do
    Inc(I);
  Result := Copy(S, Pos, I - Pos);
  if (I <= Length(S)) and ({$IFDEF UNICODE}CharInSet(S[I], Delims){$ELSE}S[I] in Delims{$ENDIF}) then
    Inc(I);
  Pos := I;
end;

function IsWordPresent(const W, S: string; const WordDelims: TCnCharSet): Boolean;
var
  Count, I          : Integer;
begin
  Result := False;
  Count := WordCount(S, WordDelims);
  for I := 1 to Count do
    if ExtractWord(I, S, WordDelims) = W then
    begin
      Result := True;
      Exit;
    end;
end;

function QuotedString(const S: string; Quote: Char): string;
var
  I                 : Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = Quote then
      Insert(Quote, Result, I);
  Result := Quote + Result + Quote;
end;

function ExtractQuotedString(const S: string; Quote: Char): string;
var
  I                 : Integer;
begin
  Result := S;
  I := Length(Result);
  if (I > 0) and (Result[1] = Quote) and
    (Result[I] = Quote) then
  begin
    Delete(Result, I, 1);
    Delete(Result, 1, 1);
    for I := Length(Result) downto 2 do
    begin
      if (Result[I] = Quote) and (Result[I - 1] = Quote) then
        Delete(Result, I, 1);
    end;
  end;
end;

function Numb2USA(const S: string): string;
var
  I, NA             : Integer;
begin
  I := Length(S);
  Result := S;
  NA := 0;
  while (I > 0) do
  begin
    if ((Length(Result) - I + 1 - NA) mod 3 = 0) and (I <> 1) then
    begin
      Insert(',', Result, I);
      Inc(NA);
    end;
    Dec(I);
  end;
end;

function CenterStr(const S: string; Len: Integer): string;
begin
  if Length(S) < Len then
  begin
    Result := MakeStr(' ', (Len div 2) - (Length(S) div 2)) + S;
    Result := Result + MakeStr(' ', Len - Length(Result));
  end
  else
    Result := S;
end;

function Dec2Hex(N: LongInt; A: Byte): string;
begin
  Result := IntToHex(N, A);
end;

function D2H(N: LongInt; A: Byte): string;
begin
  Result := IntToHex(N, A);
end;

function Hex2Dec(const S: string): Longint;
var
  HexStr            : string;
begin
  if Pos('$', S) = 0 then
    HexStr := '$' + S
  else
    HexStr := S;
  Result := StrToIntDef(HexStr, 0);
end;

function H2D(const S: string): Longint;
begin
  Result := Hex2Dec(S);
end;

function Dec2Numb(N: Longint; A, B: Byte): string;
var
  C                 : Integer;
  Number            : Cardinal;
begin
  if N = 0 then
    Result := '0'
  else
  begin
    Number := Cardinal(N);
    Result := '';
    while Number > 0 do
    begin
      C := Number mod B;
      if C > 9 then
        C := C + 55
      else
        C := C + 48;
      Result := Chr(C) + Result;
      Number := Number div B;
    end;
  end;
  if Result <> '' then
    Result := AddChar('0', Result, A);
end;

function Numb2Dec(S: string; B: Byte): Longint;
var
  I, P              : Longint;
begin
  I := Length(S);
  Result := 0;
  S := UpperCase(S);
  P := 1;
  while (I >= 1) do
  begin
    if S[I] > '@' then
      Result := Result + (Ord(S[I]) - 55) * P
    else
      Result := Result + (Ord(S[I]) - 48) * P;
    Dec(I);
    P := P * B;
  end;
end;

function RomanToInt(const S: string): Longint;
const
  RomanChars        = ['C', 'D', 'I', 'L', 'M', 'V', 'X'];
  RomanValues       : array['C'..'X'] of Word =
    (100, 500, 0, 0, 0, 0, 1, 0, 0, 50, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 10);
var
  Index, Next       : Char;
  I                 : Integer;
  Negative          : Boolean;
begin
  Result := 0;
  I := 0;
  Negative := (Length(S) > 0) and (S[1] = '-');
  if Negative then
    Inc(I);
  while (I < Length(S)) do
  begin
    Inc(I);
    Index := UpCase(S[I]);
    if {$IFDEF UNICODE}CharInSet(Index, RomanChars){$ELSE}Index in RomanChars{$ENDIF} then
    begin
      if Succ(I) <= Length(S) then
        Next := UpCase(S[I + 1])
      else
        Next := #0;
      if ({$IFDEF UNICODE}CharInSet(Next, RomanChars){$ELSE}Next in RomanChars{$ENDIF}) and (RomanValues[Index] < RomanValues[Next]) then
      begin
        Inc(Result, RomanValues[Next]);
        Dec(Result, RomanValues[Index]);
        Inc(I);
      end
      else
        Inc(Result, RomanValues[Index]);
    end
    else
    begin
      Result := 0;
      Exit;
    end;
  end;
  if Negative then
    Result := -Result;
end;

function IntToRoman(Value: Longint): string;
label
  A500, A400, A100, A90, A50, A40, A10, A9, A5, A4, A1;
begin
  Result := '';
  while Value >= 1000 do
  begin
    Dec(Value, 1000);
    Result := Result + 'M';
  end;
  if Value < 900 then
    goto A500
  else
  begin
    Dec(Value, 900);
    Result := Result + 'CM';
  end;
  goto A90;
  A400:
  if Value < 400 then
    goto A100
  else
  begin
    Dec(Value, 400);
    Result := Result + 'CD';
  end;
  goto A90;
  A500:
  if Value < 500 then
    goto A400
  else
  begin
    Dec(Value, 500);
    Result := Result + 'D';
  end;
  A100:
  while Value >= 100 do
  begin
    Dec(Value, 100);
    Result := Result + 'C';
  end;
  A90:
  if Value < 90 then
    goto A50
  else
  begin
    Dec(Value, 90);
    Result := Result + 'XC';
  end;
  goto A9;
  A40:
  if Value < 40 then
    goto A10
  else
  begin
    Dec(Value, 40);
    Result := Result + 'XL';
  end;
  goto A9;
  A50:
  if Value < 50 then
    goto A40
  else
  begin
    Dec(Value, 50);
    Result := Result + 'L';
  end;
  A10:
  while Value >= 10 do
  begin
    Dec(Value, 10);
    Result := Result + 'X';
  end;
  A9:
  if Value < 9 then
    goto A5
  else
  begin
    Result := Result + 'IX';
  end;
  Exit;
  A4:
  if Value < 4 then
    goto A1
  else
  begin
    Result := Result + 'IV';
  end;
  Exit;
  A5:
  if Value < 5 then
    goto A4
  else
  begin
    Dec(Value, 5);
    Result := Result + 'V';
  end;
  goto A1;
  A1:
  while Value >= 1 do
  begin
    Dec(Value);
    Result := Result + 'I';
  end;
end;

function IntToBin(Value: Longint; Digits, Spaces: Integer): string;
begin
  Result := '';
  if Digits > 32 then
    Digits := 32;
  while Digits > 0 do
  begin
    if (Digits mod Spaces) = 0 then
      Result := Result + ' ';
    Dec(Digits);
    Result := Result + IntToStr((Value shr Digits) and 1);
  end;
end;

function FindPart(const HelpWilds, InputStr: string): Integer;
var
  I, J              : Integer;
  Diff              : Integer;
begin
  I := Pos('?', HelpWilds);
  if I = 0 then
  begin

    Result := Pos(HelpWilds, InputStr);
    Exit;
  end;

  Diff := Length(InputStr) - Length(HelpWilds);
  if Diff < 0 then
  begin
    Result := 0;
    Exit;
  end;

  for I := 0 to Diff do
  begin
    for J := 1 to Length(HelpWilds) do
    begin
      if (InputStr[I + J] = HelpWilds[J]) or
        (HelpWilds[J] = '?') then
      begin
        if J = Length(HelpWilds) then
        begin
          Result := I + 1;
          Exit;
        end;
      end
      else
        Break;
    end;
  end;
  Result := 0;
end;

function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;

  function SearchNext(var Wilds: string): Integer;

  begin
    Result := Pos('*', Wilds);
    if Result > 0 then
      Wilds := Copy(Wilds, 1, Result - 1);
  end;

var
  CWild, CInputWord : Integer;
  I, LenHelpWilds   : Integer;
  MaxInputWord, MaxWilds: Integer;
  HelpWilds         : string;
begin
  if Wilds = InputStr then
  begin
    Result := True;
    Exit;
  end;
  repeat
    I := Pos('**', Wilds);
    if I > 0 then
      Wilds := Copy(Wilds, 1, I - 1) + '*' + Copy(Wilds, I + 2, MaxInt);
  until I = 0;
  if Wilds = '*' then
  begin
    Result := True;
    Exit;
  end;
  MaxInputWord := Length(InputStr);
  MaxWilds := Length(Wilds);
  if IgnoreCase then
  begin
    InputStr := AnsiUpperCase(InputStr);
    Wilds := AnsiUpperCase(Wilds);
  end;
  if (MaxWilds = 0) or (MaxInputWord = 0) then
  begin
    Result := False;
    Exit;
  end;
  CInputWord := 1;
  CWild := 1;
  Result := True;
  repeat
    if InputStr[CInputWord] = Wilds[CWild] then
    begin

      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '?' then
    begin

      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '*' then
    begin
      HelpWilds := Copy(Wilds, CWild + 1, MaxWilds);
      I := SearchNext(HelpWilds);
      LenHelpWilds := Length(HelpWilds);
      if I = 0 then
      begin

        if HelpWilds = '' then
          Exit;

        for I := 0 to LenHelpWilds - 1 do
        begin
          if (HelpWilds[LenHelpWilds - I] <> InputStr[MaxInputWord - I]) and
            (HelpWilds[LenHelpWilds - I] <> '?') then
          begin
            Result := False;
            Exit;
          end;
        end;
        Exit;
      end;

      Inc(CWild, 1 + LenHelpWilds);
      I := FindPart(HelpWilds, Copy(InputStr, CInputWord, MaxInt));
      if I = 0 then
      begin
        Result := False;
        Exit;
      end;
      CInputWord := I + LenHelpWilds;
      Continue;
    end;
    Result := False;
    Exit;
  until (CInputWord > MaxInputWord) or (CWild > MaxWilds);

  if CInputWord <= MaxInputWord then
    Result := False;
  if (CWild <= MaxWilds) and (Wilds[MaxWilds] <> '*') then
    Result := False;
end;

function XorString(const Key, Src: ShortString): ShortString;
var
  I                 : Integer;
begin
  Result := Src;
  if Length(Key) > 0 then
    for I := 1 to Length(Src) do
      Result[I] := AnsiChar(Chr(Byte(Key[1 + ((I - 1) mod Length(Key))]) xor
        Ord(Src[I])));
end;

function XorEncode(const Key, Source: string): string;
var
  I                 : Integer;
  C                 : Byte;
begin
  Result := '';
  for I := 1 to Length(Source) do
  begin
    if Length(Key) > 0 then
      C := Byte(Key[1 + ((I - 1) mod Length(Key))]) xor Byte(Source[I])
    else
      C := Byte(Source[I]);
    Result := Result + AnsiLowerCase(IntToHex(C, 2));
  end;
end;

function XorDecode(const Key, Source: string): string;
var
  I                 : Integer;
  C                 : Char;
begin
  Result := '';
  for I := 0 to Length(Source) div 2 - 1 do
  begin
    C := Chr(StrToIntDef('$' + Copy(Source, (I * 2) + 1, 2), Ord(' ')));
    if Length(Key) > 0 then
      C := Chr(Byte(Key[1 + (I mod Length(Key))]) xor Byte(C));
    Result := Result + C;
  end;
end;

function GetCmdLineArg(const Switch: string; SwitchChars: TCnCharSet): string;
var
  I                 : Integer;
  S                 : string;
begin
  I := 1;
  while I <= ParamCount do
  begin
    S := ParamStr(I);
    if (SwitchChars = []) or (({$IFDEF UNICODE}CharInSet(S[1], SwitchChars){$ELSE}S[1] in SwitchChars{$ENDIF}) and (Length(S) > 1)) then
    begin
      if (AnsiCompareText(Copy(S, 2, MaxInt), Switch) = 0) then
      begin
        Inc(I);
        if I <= ParamCount then
        begin
          Result := ParamStr(I);
          Exit;
        end;
      end;
    end;
    Inc(I);
  end;
  Result := '';
end;

{$ENDIF SUPPORT_ADO}
end.
