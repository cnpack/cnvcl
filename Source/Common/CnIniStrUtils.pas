{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnIniStrUtils;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：扩展的 INI 访问的字符串处理单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：从 CnIni 单元分离而来
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2004.02.08 V1.0
*               从 CnIni 单元中分离出此单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Graphics, {$IFDEF MSWINDOWS} Windows, {$ELSE} System.Types, System.UITypes, {$ENDIF} SysUtils;

//==============================================================================
// 用于扩展 INI 类的字符串操作函数
//==============================================================================

function StringToFontStyles(const Styles: string): TFontStyles;
function FontStylesToString(Styles: TFontStyles): string;
function FontToString(Font: TFont): string;
function FontToStringEx(Font: TFont; BaseFont: TFont): string;
procedure StringToFont(const Str: string; Font: TFont);
procedure StringToFontEx(const Str: string; Font: TFont; BaseFont: TFont);
function RectToStr(Rect: TRect): string;
function StrToRect(const Str: string; const Def: TRect): TRect;
function PointToStr(P: TPoint): string;
function StrToPoint(const Str: string; const Def: TPoint): TPoint;

implementation

uses
  CnCommon;

const
  csLefts  = ['[', '{', '('];
  csRights = [']', '}', ')'];

//==============================================================================
// 用于扩展 INI 类的字符串操作函数
//==============================================================================

function FontStylesToString(Styles: TFontStyles): string;
begin
  Result := '';
{$IFDEF MSWINDOWS}
  if fsBold in Styles then Result := Result + 'B';
  if fsItalic in Styles then Result := Result + 'I';
  if fsUnderline in Styles then Result := Result + 'U';
  if fsStrikeOut in Styles then Result := Result + 'S';
{$ELSE}
  if TFontStyle.fsBold in Styles then Result := Result + 'B';
  if TFontStyle.fsItalic in Styles then Result := Result + 'I';
  if TFontStyle.fsUnderline in Styles then Result := Result + 'U';
  if TFontStyle.fsStrikeOut in Styles then Result := Result + 'S';
{$ENDIF}
end;

function StringToFontStyles(const Styles: string): TFontStyles;
begin
  Result := [];
{$IFDEF MSWINDOWS}
  if Pos('B', UpperCase(Styles)) > 0 then Include(Result, fsBold);
  if Pos('I', UpperCase(Styles)) > 0 then Include(Result, fsItalic);
  if Pos('U', UpperCase(Styles)) > 0 then Include(Result, fsUnderline);
  if Pos('S', UpperCase(Styles)) > 0 then Include(Result, fsStrikeOut);
{$ELSE}
  if Pos('B', UpperCase(Styles)) > 0 then Include(Result, TFontStyle.fsBold);
  if Pos('I', UpperCase(Styles)) > 0 then Include(Result, TFontStyle.fsItalic);
  if Pos('U', UpperCase(Styles)) > 0 then Include(Result, TFontStyle.fsUnderline);
  if Pos('S', UpperCase(Styles)) > 0 then Include(Result, TFontStyle.fsStrikeOut);
{$ENDIF}
end;

function FontToString(Font: TFont): string;
{$IFDEF MSWINDOWS}
var
  S: string;
{$ENDIF}
begin
  with Font do
  begin
{$IFDEF MSWINDOWS}
    if not CharsetToIdent(Charset, S) then
      S := IntToStr(Charset);
    Result := Format('%s,%d,%s,%d,%s,%s', [Name, Size,
      FontStylesToString(Style), Ord(Pitch), ColorToString(Color), S]);
{$ELSE}
    Result := Format('%s,%f,%s', [Family, Size, FontStylesToString(Style)]);
{$ENDIF}
  end;
end;

function FontToStringEx(Font: TFont; BaseFont: TFont): string;
var
  AName, ASize, AStyle {$IFDEF MSWINDOWS}, APitch, AColor, ACharSet {$ENDIF}: string;
begin
  if BaseFont = nil then
    Result := FontToString(Font)
  else
  begin
    AName := '';
    ASize := '';
    AStyle := '';

    if Font.Style <> BaseFont.Style then
      AStyle := FontStylesToString(Font.Style);

{$IFDEF MSWINDOWS}
    if not SameText(Font.Name, BaseFont.Name) then
      AName := Font.Name;
    if Font.Size <> BaseFont.Size then
      ASize := IntToStr(Font.Size);

    if Font.Pitch <> BaseFont.Pitch then
      APitch := IntToStr(Ord(Font.Pitch))
    else
      APitch := '';
    if Font.Color <> BaseFont.Color then
      AColor := ColorToString(Font.Color)
    else
      AColor := '';
    if Font.Charset <> BaseFont.Charset then
    begin
      if not CharsetToIdent(Font.Charset, ACharSet) then
        ACharSet := IntToStr(Font.Charset);
    end
    else
      ACharSet := '';

    Result := Format('%s,%s,%s,%s,%s,%s', [AName, ASize, AStyle, APitch, AColor,
      ACharSet]);

{$ELSE}
    if not SameText(Font.Family, BaseFont.Family) then
      AName := Font.Family;
    if Font.Size <> BaseFont.Size then
      ASize := FloatToStr(Font.Size);

    Result := Format('%s,%s,%s', [AName, ASize, AStyle]);
{$ENDIF}
  end;
end;

type
  THackFont = class(TFont);

procedure StringToFont(const Str: string; Font: TFont);
begin
  StringToFontEx(Str, Font, nil);
end;

procedure StringToFontEx(const Str: string; Font: TFont; BaseFont: TFont);
const
  Delims = [',', ';'];
var
{$IFDEF MSWINDOWS}
  FontChange: TNotifyEvent;
  Charset: Longint;
{$ENDIF}
  Pos: Integer;
  I: Byte;
  S: string;
begin
  if Font = nil then
    Exit;
  try
{$IFDEF MSWINDOWS}
    FontChange := Font.OnChange;
    Font.OnChange := nil;
{$ENDIF}

    try
      if BaseFont <> nil then
        Font.Assign(BaseFont);
      Pos := 1;
      I := 0;
      while Pos <= Length(Str) do begin
        Inc(I);
        S := Trim(ExtractSubstr(Str, Pos, Delims));
        case I of
          1: if S <> '' then
             begin
{$IFDEF MSWINDOWS}
               Font.Name := S;
{$ELSE}
               Font.Family := S;
{$ENDIF}
             end;
          2: if S <> '' then
             begin
{$IFDEF MSWINDOWS}
               Font.Size := StrToIntDef(S, Font.Size);
{$ELSE}
               Font.Size := StrToFloatDef(S, Font.Size);
{$ENDIF}
             end;
          3: if S <> '' then Font.Style := StringToFontStyles(S) else Font.Style := [];
{$IFDEF MSWINDOWS}
          4: if S <> '' then Font.Pitch := TFontPitch(StrToIntDef(S, Ord(Font.Pitch)));
          5: if S <> '' then Font.Color := StringToColor(S);
          6: if S <> '' then
            begin
              if IdentToCharset(S, Charset) then
                Font.Charset := Charset
              else
                Font.Charset := TFontCharset(StrToIntDef(S, Font.Charset));
            end;
{$ENDIF}
        end;
      end;
    finally
{$IFDEF MSWINDOWS}
      Font.OnChange := FontChange;
      THackFont(Font).Changed;
{$ENDIF}
    end;
  except
    ;
  end;
end;

function RectToStr(Rect: TRect): string;
begin
  with Rect do
    Result := Format('[%d,%d,%d,%d]', [Left, Top, Right, Bottom]);
end;

function StrToRect(const Str: string; const Def: TRect): TRect;
var
  S: string;
  Temp: string;
  I: Integer;
begin
  Result := Def;
  S := Str;
  if CharInSet(S[1], csLefts) and CharInSet(S[Length(S)], csRights) then begin
    Delete(S, 1, 1); SetLength(S, Length(S) - 1);
  end;
  I := Pos(',', S);
  if I > 0 then begin
    Temp := Trim(Copy(S, 1, I - 1));
    Result.Left := StrToIntDef(Temp, Def.Left);
    Delete(S, 1, I);
    I := Pos(',', S);
    if I > 0 then begin
      Temp := Trim(Copy(S, 1, I - 1));
      Result.Top := StrToIntDef(Temp, Def.Top);
      Delete(S, 1, I);
      I := Pos(',', S);
      if I > 0 then begin
        Temp := Trim(Copy(S, 1, I - 1));
        Result.Right := StrToIntDef(Temp, Def.Right);
        Delete(S, 1, I);
        Temp := Trim(S);
        Result.Bottom := StrToIntDef(Temp, Def.Bottom);
      end;
    end;
  end;
end;

function PointToStr(P: TPoint): string;
begin
  with P do Result := Format('[%d,%d]', [X, Y]);
end;

function StrToPoint(const Str: string; const Def: TPoint): TPoint;
var
  S: string;
  Temp: string;
  I: Integer;
begin
  Result := Def;
  S := Str;
  if CharInSet(S[1], csLefts) and CharInSet(S[Length(Str)], csRights) then begin
    Delete(S, 1, 1); SetLength(S, Length(S) - 1);
  end;
  I := Pos(',', S);
  if I > 0 then begin
    Temp := Trim(Copy(S, 1, I - 1));
    Result.X := StrToIntDef(Temp, Def.X);
    Delete(S, 1, I);
    Temp := Trim(S);
    Result.Y := StrToIntDef(Temp, Def.Y);
  end;
end;

end.
