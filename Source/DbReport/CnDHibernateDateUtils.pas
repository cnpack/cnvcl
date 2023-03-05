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

unit CnDHibernateDateUtils; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate基础库
* 单元名称：日期函数库
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2012.09.18 By Shenloqi
*               移植到 Delphi XE3
*           2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  SysUtils, Windows, Consts, SysConst;

function CurrentYear: Word;

function IsLeapYear(AYear: Integer): Boolean;

function DaysPerMonth(AYear, AMonth: Integer): Integer;

function FirstDayOfPrevMonth: TDateTime;

function LastDayOfPrevMonth: TDateTime;

function FirstDayOfNextMonth: TDateTime;

function ExtractDay(ADate: TDateTime): Word;

function ExtractMonth(ADate: TDateTime): Word;

function ExtractYear(ADate: TDateTime): Word;

function IncDate(ADate: TDateTime; Days, Months, Years: Integer): TDateTime;

function IncDay(ADate: TDateTime; Delta: Integer): TDateTime;

function IncMonth(ADate: TDateTime; Delta: Integer): TDateTime;

function IncYear(ADate: TDateTime; Delta: Integer): TDateTime;

function ValidDate(ADate: TDateTime): Boolean;

procedure DateDiff(Date1, Date2: TDateTime; var Days, Months, Years: Word);

function MonthsBetween(Date1, Date2: TDateTime): Double;

function DaysInPeriod(Date1, Date2: TDateTime): Longint;

function DaysBetween(Date1, Date2: TDateTime): Longint;

function IncTime(ATime: TDateTime; Hours, Minutes, Seconds, MSecs: Integer): TDateTime;

function IncHour(ATime: TDateTime; Delta: Integer): TDateTime;

function IncMinute(ATime: TDateTime; Delta: Integer): TDateTime;

function IncSecond(ATime: TDateTime; Delta: Integer): TDateTime;

function IncMSec(ATime: TDateTime; Delta: Integer): TDateTime;

function CutTime(ADate: TDateTime): TDateTime;

type
  TCnDateOrder = (doMDY, doDMY, doYMD);

  TCnDayOfWeekName = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

  TCnDaysOfWeek = set of TCnDayOfWeekName;

function GetCnDateOrder(const DateFormat: string): TCnDateOrder;

function MonthFromName(const S: string; MaxLen: Byte): Byte;

function StrToDateDef(const S: string; Default: TDateTime): TDateTime;

function StrToDateFmt(const DateFormat, S: string): TDateTime;

function StrToDateFmtDef(const DateFormat, S: string; Default: TDateTime): TDateTime;

function DefDateFormat(FourDigitYear: Boolean): string;

function DefDateMask(BlanksChar: Char; FourDigitYear: Boolean): string;

function FormatLongDate(Value: TDateTime): string;

function FormatLongDateTime(Value: TDateTime): string;

const
  DefaulTCnDateOrder = doDMY;

var
  FourDigitYear: Boolean;

const
  CenturyOffset: Byte = 60;
  NullDate: TDateTime = 0; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

uses
  CnDHibernateStringUtils;

function IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array[1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then
    Inc(Result);
end;

function FirstDayOfNextMonth: TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Day := 1;
  if Month < 12 then
    Inc(Month)
  else
  begin
    Inc(Year);
    Month := 1;
  end;
  Result := EncodeDate(Year, Month, Day);
end;

function FirstDayOfPrevMonth: TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Day := 1;
  if Month > 1 then
    Dec(Month)
  else
  begin
    Dec(Year);
    Month := 12;
  end;
  Result := EncodeDate(Year, Month, Day);
end;

function LastDayOfPrevMonth: TDateTime;
var
  D: TDateTime;
  Year, Month, Day: Word;
begin
  D := FirstDayOfPrevMonth;
  DecodeDate(D, Year, Month, Day);
  Day := DaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day);
end;

function ExtractDay(ADate: TDateTime): Word;
var
  M, Y: Word;
begin
  DecodeDate(ADate, Y, M, Result);
end;

function ExtractMonth(ADate: TDateTime): Word;
var
  D, Y: Word;
begin
  DecodeDate(ADate, Y, Result, D);
end;

function ExtractYear(ADate: TDateTime): Word;
var
  D, M: Word;
begin
  DecodeDate(ADate, Result, M, D);
end;

function IncDate(ADate: TDateTime; Days, Months, Years: Integer): TDateTime;
var
  D, M, Y: Word;
  Day, Month, Year: Longint;
begin
  DecodeDate(ADate, Y, M, D);
  Year := Y;
  Month := M;
  Day := D;
  Inc(Year, Years);
  Inc(Year, Months div 12);
  Inc(Month, Months mod 12);
  if Month < 1 then
  begin
    Inc(Month, 12);
    Dec(Year);
  end
  else if Month > 12 then
  begin
    Dec(Month, 12);
    Inc(Year);
  end;
  if Day > DaysPerMonth(Year, Month) then
    Day := DaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day) + Days + Frac(ADate);
end;

procedure DateDiff(Date1, Date2: TDateTime; var Days, Months, Years: Word);
var
  DtSwap: TDateTime;
  Day1, Day2, Month1, Month2, Year1, Year2: Word;
begin
  if Date1 > Date2 then
  begin
    DtSwap := Date1;
    Date1 := Date2;
    Date2 := DtSwap;
  end;
  DecodeDate(Date1, Year1, Month1, Day1);
  DecodeDate(Date2, Year2, Month2, Day2);
  Years := Year2 - Year1;
  Months := 0;
  Days := 0;
  if Month2 < Month1 then
  begin
    Inc(Months, 12);
    Dec(Years);
  end;
  Inc(Months, Month2 - Month1);
  if Day2 < Day1 then
  begin
    Inc(Days, DaysPerMonth(Year1, Month1));
    if Months = 0 then
    begin
      Dec(Years);
      Months := 11;
    end
    else
      Dec(Months);
  end;
  Inc(Days, Day2 - Day1);
end;

function IncDay(ADate: TDateTime; Delta: Integer): TDateTime;
begin
  Result := ADate + Delta;
end;

function IncMonth(ADate: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncDate(ADate, 0, Delta, 0);
end;

function IncYear(ADate: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncDate(ADate, 0, 0, Delta);
end;

function MonthsBetween(Date1, Date2: TDateTime): Double;
var
  D, M, Y: Word;
begin
  DateDiff(Date1, Date2, D, M, Y);
  Result := 12 * Y + M;
  if (D > 1) and (D < 7) then
    Result := Result + 0.25
  else if (D >= 7) and (D < 15) then
    Result := Result + 0.5
  else if (D >= 15) and (D < 21) then
    Result := Result + 0.75
  else if (D >= 21) then
    Result := Result + 1;
end;

function IsValidDate(Y, M, D: Word): Boolean;
begin
  Result := (Y >= 1) and (Y <= 9999) and (M >= 1) and (M <= 12) and (D >= 1) and (D <= DaysPerMonth(Y, M));
end;

function ValidDate(ADate: TDateTime): Boolean;
var
  Year, Month, Day: Word;
begin
  try
    DecodeDate(ADate, Year, Month, Day);
    Result := IsValidDate(Year, Month, Day);
  except
    Result := False;
  end;
end;

function DaysInPeriod(Date1, Date2: TDateTime): Longint;
begin
  if ValidDate(Date1) and ValidDate(Date2) then
    Result := Abs(Trunc(Date2) - Trunc(Date1)) + 1
  else
    Result := 0;
end;

function DaysBetween(Date1, Date2: TDateTime): Longint;
begin
  Result := Trunc(Date2) - Trunc(Date1) + 1;
  if Result < 0 then
    Result := 0;
end;

function IncTime(ATime: TDateTime; Hours, Minutes, Seconds, MSecs: Integer): TDateTime;
begin
  Result := ATime + (Hours div 24) + (((Hours mod 24) * 3600000 + Minutes * 60000 + Seconds * 1000 + MSecs) / MSecsPerDay);
  if Result < 0 then
    Result := Result + 1;
end;

function IncHour(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, Delta, 0, 0, 0);
end;

function IncMinute(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, 0, Delta, 0, 0);
end;

function IncSecond(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, 0, 0, Delta, 0);
end;

function IncMSec(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, 0, 0, 0, Delta);
end;

function CutTime(ADate: TDateTime): TDateTime;
begin
  Result := Trunc(ADate);
end;

function CurrentYear: Word;
var
  SystemTime: TSystemTime;
begin
  GetLocalTime(SystemTime);
  Result := SystemTime.wYear;
end;

procedure ScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do
    Inc(I);
  Pos := I;
end;

function ScanNumber(const S: string; MaxLength: Integer; var Pos: Integer; var Number: Longint): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (Longint(I - Pos) < MaxLength) and ({$IFDEF UNICODE}CharInSet(S[I],  ['0'..'9']){$ELSE}S[I] in ['0'..'9']{$ENDIF}) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
begin
  Result := False;
  ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

procedure ScanToNumber(const S: string; var Pos: Integer);
begin
  while (Pos <= Length(S)) and not ({$IFDEF UNICODE}CharInSet(S[Pos], ['0'..'9']){$ELSE}S[Pos] in ['0'..'9']{$ENDIF}) do
  begin
    if {$IFDEF UNICODE}CharInSet(S[Pos], LeadBytes){$ELSE}S[Pos] in LeadBytes{$ENDIF} then
      Inc(Pos);
    Inc(Pos);
  end;
end;

function GetCnDateOrder(const DateFormat: string): TCnDateOrder;
var
  I: Integer;
begin
  Result := DefaulTCnDateOrder;
  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'E':
        Result := doYMD;
      'Y':
        Result := doYMD;
      'M':
        Result := doMDY;
      'D':
        Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
end;

function ExpandYear(Year: Integer): Integer;
var
  N: Longint;
begin
  Result := Year;
  if Result < 100 then
  begin
    N := CurrentYear - CenturyOffset;
    Inc(Result, N div 100 * 100);
    if (CenturyOffset > 0) and (Result < N) then
      Inc(Result, 100);
  end;
end;

function ScanDate(const S, DateFormat: string; var Pos: Integer; var Y, M, D: Integer): Boolean;
var
  DateOrder: TCnDateOrder;
  N1, N2, N3: Longint;
begin
  Result := False;
  Y := 0;
  M := 0;
  D := 0;
  DateOrder := GetCnDateOrder(DateFormat);
  if {$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat[1] = 'g' then
    ScanToNumber(S, Pos);
  if not (ScanNumber(S, MaxInt, Pos, N1) and ScanChar(S, Pos, {$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}DateSeparator) and ScanNumber(S, MaxInt, Pos, N2)) then
    Exit;
  if ScanChar(S, Pos, {$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}DateSeparator) then
  begin
    if not ScanNumber(S, MaxInt, Pos, N3) then
      Exit;
    case DateOrder of
      doMDY:
        begin
          Y := N3;
          M := N1;
          D := N2;
        end;
      doDMY:
        begin
          Y := N3;
          M := N2;
          D := N1;
        end;
      doYMD:
        begin
          Y := N1;
          M := N2;
          D := N3;
        end;
    end;
    Y := ExpandYear(Y);
  end
  else
  begin
    Y := CurrentYear;
    if DateOrder = doDMY then
    begin
      D := N1;
      M := N2;
    end
    else
    begin
      M := N1;
      D := N2;
    end;
  end;
  ScanChar(S, Pos, {$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}DateSeparator);
  ScanBlanks(S, Pos);
  if SysLocale.FarEast and (System.Pos('ddd', {$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat) <> 0) then
  begin
    if {$IFDEF UNICODE}CharInSet({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortTimeFormat[1], ['0'..'9']){$ELSE}ShortTimeFormat[1] in ['0'..'9']{$ENDIF} then
      ScanToNumber(S, Pos)
    else
      repeat
        while (Pos <= Length(S)) and (S[Pos] <> ' ') do
          Inc(Pos);
        ScanBlanks(S, Pos);
      until(Pos > Length(S)) or (AnsiCompareText({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}TimeAMString, Copy(S, Pos, Length({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}TimeAMString))) = 0) or (AnsiCompareText({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}TimePMString, Copy(S, Pos, Length({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}TimePMString))) = 0);
  end;
  Result := IsValidDate(Y, M, D) and (Pos > Length(S));
end;

function MonthFromName(const S: string; MaxLen: Byte): Byte;
begin
  if Length(S) > 0 then
    for Result := 1 to 12 do
    begin
      if (Length({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}LongMonthNames[Result]) > 0) and (AnsiCompareText(Copy(S, 1, MaxLen), Copy({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}LongMonthNames[Result], 1, MaxLen)) = 0) then
        Exit;
    end;
  Result := 0;
end;

procedure ExtractMask(const Format, S: string; Ch: Char; Cnt: Integer; var I: Integer; Blank, Default: Integer);
var
  Tmp: string[20];
  J, L: Integer;
begin
  I := Default;
  Ch := UpCase(Ch);
  L := Length(Format);
  if Length(S) < L then
    L := Length(S)
  else if Length(S) > L then
    Exit;
  J := Pos(MakeStr(Ch, Cnt), AnsiUpperCase(Format));
  if J <= 0 then
    Exit;
  Tmp := '';
  while (UpCase(Format[J]) = Ch) and (J <= L) do
  begin
    if S[J] <> ' ' then
      Tmp := Tmp + {$IFDEF UNICODE}ShortString{$ENDIF}(S[J]);
    Inc(J);
  end;
  if Tmp = '' then
    I := Blank
  else if Cnt > 1 then
  begin
    I := MonthFromName({$IFDEF UNICODE}String{$ENDIF}(Tmp), Length(Tmp));
    if I = 0 then
      I := -1;
  end
  else
    I := StrToIntDef({$IFDEF UNICODE}String{$ENDIF}(Tmp), -1);
end;

function ScanDateStr(const Format, S: string; var D, M, Y: Integer): Boolean;
var
  Pos: Integer;
begin
  ExtractMask(Format, S, 'm', 3, M, -1, 0);
  if M = 0 then
    ExtractMask(Format, S, 'm', 1, M, -1, 0);
  ExtractMask(Format, S, 'd', 1, D, -1, 1);
  ExtractMask(Format, S, 'y', 1, Y, -1, CurrentYear);
  Y := ExpandYear(Y);
  Result := IsValidDate(Y, M, D);
  if not Result then
  begin
    Pos := 1;
    Result := ScanDate(S, Format, Pos, Y, M, D);
  end;
end;

function InternalStrToDate(const DateFormat, S: string; var Date: TDateTime): Boolean;
var
  D, M, Y: Integer;
begin
  if S = '' then
  begin
    Date := NullDate;
    Result := True;
  end
  else
  begin
    Result := ScanDateStr(DateFormat, S, D, M, Y);
    if Result then
      try
        Date := EncodeDate(Y, M, D);
      except
        Result := False;
      end;
  end;
end;

function StrToDateFmt(const DateFormat, S: string): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then
    raise EConvertError.CreateFmt(SInvalidDate, [S]);
end;

function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
begin
  if not InternalStrToDate({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat, S, Result) then
    Result := Trunc(Default);
end;

function StrToDateFmtDef(const DateFormat, S: string; Default: TDateTime): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then
    Result := Trunc(Default);
end;

function DefDateFormat(FourDigitYear: Boolean): string;
begin
  if FourDigitYear then
  begin
    case GetCnDateOrder({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat) of
      doMDY:
        Result := 'MM/DD/YYYY';
      doDMY:
        Result := 'DD/MM/YYYY';
      doYMD:
        Result := 'YYYY/MM/DD';
    end;
  end
  else
  begin
    case GetCnDateOrder({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat) of
      doMDY:
        Result := 'MM/DD/YY';
      doDMY:
        Result := 'DD/MM/YY';
      doYMD:
        Result := 'YY/MM/DD';
    end;
  end;
end;

function DefDateMask(BlanksChar: Char; FourDigitYear: Boolean): string;
begin
  if FourDigitYear then
  begin
    case GetCnDateOrder({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat) of
      doMDY, doDMY:
        Result := '!99/99/9999;1;';
      doYMD:
        Result := '!9999/99/99;1;';
    end;
  end
  else
  begin
    case GetCnDateOrder({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat) of
      doMDY, doDMY:
        Result := '!99/99/99;1;';
      doYMD:
        Result := '!99/99/99;1;';
    end;
  end;
  if Result <> '' then
    Result := Result + BlanksChar;
end;

function FormatLongDate(Value: TDateTime): string;
var
  Buffer: array[0..1023] of Char;
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(Value, SystemTime);
  SetString(Result, Buffer, GetDateFormat(GetThreadLocale, DATE_LONGDATE, @SystemTime, nil, Buffer, SizeOf(Buffer) - 1));
  Result := TrimRight(Result);
end;

function FormatLongDateTime(Value: TDateTime): string;
begin
  if Value <> NullDate then
    Result := FormatLongDate(Value) + FormatDateTime(' tt', Value)
  else
    Result := '';
end;

initialization
  FourDigitYear := Pos('YYYY', AnsiUpperCase({$IFDEF DELPHIXE3_UP}FormatSettings.{$ENDIF}ShortDateFormat)) > 0;

{$ENDIF SUPPORT_ADO}
end.
