{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CntUtils;
{
  CNT - CnPack NetTool
  通用工具函数单元

  跨平台支持：Delphi 5+, FPC 3+
}

interface

{$I CnPack.inc}

uses
  SysUtils, CnNative;

procedure DebugPrint(const Msg: string; Verbose, Debug: Boolean);
procedure PrintProgress(const Current, Total: Integer; const Msg: string);
function HexDump(Data: Pointer; Len: Integer; Offset: Int64; const Prefix: string): string; overload;
function HexDump(Data: Pointer; Len: Integer): string; overload;
function HexDump(const Data: AnsiString; Offset: Int64; const Prefix: string): string; overload;
function HexDump(const Data: AnsiString): string; overload;
function BytesToHexStr(Data: Pointer; Len: Integer): string;

implementation

const
  HexDigits: array[0..15] of Char = '0123456789ABCDEF';

function BytesToHexStr(Data: Pointer; Len: Integer): string;
var
  I: Integer;
  P: PByteArray;
begin
  SetLength(Result, Len * 2);
  P := PByteArray(Data);
  for I := 0 to Len - 1 do
  begin
    Result[I * 2 + 1] := HexDigits[(P^[I] shr 4) and $0F];
    Result[I * 2 + 2] := HexDigits[P^[I] and $0F];
  end;
end;

function HexDump(Data: Pointer; Len: Integer; Offset: Int64; const Prefix: string): string;
var
  I, Line: Integer;
  P: PByteArray;
  HexPart, AsciiPart: string;
  LineOffset: Int64;
begin
  Result := '';
  if (Data = nil) or (Len <= 0) then
    Exit;

  P := PByteArray(Data);
  Line := 0;

  while Line * 16 < Len do
  begin
    LineOffset := Offset + Line * 16;
    HexPart := Format('%8.8x  ', [LineOffset]);

    for I := 0 to 15 do
    begin
      if Line * 16 + I < Len then
        HexPart := HexPart + Format('%2.2x ', [P^[Line * 16 + I]])
      else
        HexPart := HexPart + '   ';
    end;

    HexPart := HexPart + ' |';

    AsciiPart := '';
    for I := 0 to 15 do
    begin
      if Line * 16 + I < Len then
      begin
        case P[Line * 16 + I] of
          32..126: AsciiPart := AsciiPart + Char(P[Line * 16 + I]);
          else       AsciiPart := AsciiPart + '.';
        end;
      end
      else
        AsciiPart := AsciiPart + ' ';
    end;

    HexPart := HexPart + AsciiPart + '|';
    Result := Result + Prefix + HexPart + #13#10;
    Inc(Line);
  end;
end;

function HexDump(Data: Pointer; Len: Integer): string;
begin
  Result := HexDump(Data, Len, 0, '');
end;

function HexDump(const Data: AnsiString; Offset: Int64; const Prefix: string): string;
begin
  Result := HexDump(Pointer(Data), Length(Data), Offset, Prefix);
end;

function HexDump(const Data: AnsiString): string;
begin
  Result := HexDump(Pointer(Data), Length(Data), 0, '');
end;

procedure DebugPrint(const Msg: string; Verbose, Debug: Boolean);
begin
  if Debug then
    WriteLn('[DEBUG] ', Msg)
  else if Verbose then
    WriteLn('[VERBOSE] ', Msg);
end;

procedure PrintProgress(const Current, Total: Integer; const Msg: string);
begin
  if Total > 0 then
    Write(#13, Msg, ': ', Current, '/', Total, ' (', (Current * 100) div Total, '%)')
  else
    Write(#13, Msg, ': ', Current);
end;

end.
