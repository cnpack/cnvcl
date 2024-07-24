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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

{******************************************************************************}
{        该单元部分内容基于 Dennis D. Spreen 的 UTBASE64.pas 改写。            }
{        下面是 UTBASE64.pas 的声明:                                           }
{ -----------------------------------------------------------------------------}
{ uTBase64 v1.0 - Simple Base64 encoding/decoding class                        }
{ Base64 described in RFC2045, Page 24, (w) 1996 Freed & Borenstein            }
{ Delphi implementation (w) 1999 Dennis D. Spreen (dennis@spreendigital.de)    }
{ This unit is freeware. Just drop me a line if this unit is useful for you.   }
{ -----------------------------------------------------------------------------}

unit CnBase64;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：Base64 编码算法单元
* 单元作者：詹葵（Solin） solin@21cn.com; http://www.ilovezhuzhu.net
*           wr960204
*           Liu Xiao
* 备    注：该单元包括标准 Base64 与 Base64URL 实现
*           Base64URL 规则基于标准 Base64 但把 + / 替换成了 - _，且删除后 =
* 开发平台：PWin2003Std + Delphi 6.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.10.04 V1.6
*               删除慢速实现。Base64Encode 与 Base64Decode 支持 Base64URL 的编码与解码
*           2019.12.12 V1.5
*               支持 TBytes
*           2019.04.15 V1.4
*               支持 Win32/Win64/MacOS
*           2018.06.22 V1.3
*               修正解出的原始内容可能包含多余 #0 或原始尾部 #0 被错误移除的问题
*           2016.05.03 V1.2
*               修正字符串中包含 #0 时可能会被截断的问题
*           2006.10.25 V1.1
*               增加 wr960204 的优化版本
*           2003.10.14 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnConsts;

function Base64Encode(InputData: TStream; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* 对流进行 Base64 编码或 Base64URL 编码，如编码成功返回 ECN_BASE64_OK
|<PRE>
  InputData: TStream           - 要编码的数据流
  var OutputData: AnsiString   - 编码后的数据
  URL: Boolean                 - True 则使用 Base64URL 编码，False 则使用标准 Base64 编码，默认 False
|</PRE>}

function Base64Encode(const InputData: AnsiString; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* 对字符串进行 Base64 编码或 Base64URL 编码，如编码成功返回 ECN_BASE64_OK
|<PRE>
  InputData: AnsiString        - 要编码的数据
  var OutputData: AnsiString   - 编码后的数据
  URL: Boolean                 - True 则使用 Base64URL 编码，False 则使用标准 Base64 编码，默认 False
|</PRE>}

function Base64Encode(InputData: Pointer; DataLen: Integer; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* 对数据进行 Base64 编码或 Base64URL 编码，如编码成功返回 ECN_BASE64_OK
|<PRE>
  InputData: AnsiString        - 要编码的数据
  var OutputData: AnsiString   - 编码后的数据
  URL: Boolean                 - True 则使用 Base64URL 编码，False 则使用标准 Base64 编码，默认 False
|</PRE>}

function Base64Encode(InputData: TBytes; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* 对 TBytes 进行 Base64 编码或 Base64URL 编码，如编码成功返回 ECN_BASE64_OK
|<PRE>
  InputData: TBytes            - 要编码的数据流
  var OutputData: AnsiString   - 编码后的数据
  URL: Boolean                 - True 则使用 Base64URL 编码，False 则使用标准 Base64 编码，默认 False
|</PRE>}

function Base64Decode(const InputData: AnsiString; var OutputData: AnsiString;
  FixZero: Boolean = True): Integer; overload;
{* 对数据进行 Base64 解码（包括 Base64URL 解码），如解码成功返回 ECN_BASE64_OK
|<PRE>
  InputData: AnsiString        - 要解码的数据
  var OutputData: AnsiString   - 解码后的数据
  FixZero: Boolean             - 是否移去尾部的 #0
|</PRE>}

function Base64Decode(const InputData: AnsiString; OutputData: TStream;
  FixZero: Boolean = True): Integer; overload;
{* 对数据进行 Base64 解码（包括 Base64URL 解码），如解码成功返回 ECN_BASE64_OK
|<PRE>
  InputData: AnsiString        - 要解码的数据
  var OutputData: TStream      - 解码后的数据
  FixZero: Boolean             - 是否移去尾部的 #0
|</PRE>}

function Base64Decode(InputData: string; out OutputData: TBytes;
  FixZero: Boolean = True): Integer; overload;
{* 对数据进行 Base64 解码（包括 Base64URL 解码），如解码成功返回 ECN_BASE64_OK
|<PRE>
  InputData: string            - 要解码的数据
  out OutputData: TBytes       - 解码后的数据
  FixZero: Boolean             - 是否移去尾部的 0
|</PRE>}

const
  ECN_BASE64_OK                        = ECN_OK; // 转换成功
  ECN_BASE64_ERROR_BASE                = ECN_CUSTOM_ERROR_BASE + $500; // Base64 错误码基准

  ECN_BASE64_ERROR                     = ECN_BASE64_ERROR_BASE + 1; // 转换错误（未知错误） (e.g. can't encode octet in input stream) -> error in implementation
  ECN_BASE64_INVALID                   = ECN_BASE64_ERROR_BASE + 2; // 输入的字符串中有非法字符 (在 FilterDecodeInput=False 时可能出现)
  ECN_BASE64_LENGTH                    = ECN_BASE64_ERROR_BASE + 3; // 数据长度非法
  ECN_BASE64_DATALEFT                  = ECN_BASE64_ERROR_BASE + 4; // too much input data left (receveived 'end of encoded data' but not end of input string)
  ECN_BASE64_PADDING                   = ECN_BASE64_ERROR_BASE + 5; // 输入的数据未能以正确的填充字符结束

implementation

var
  FilterDecodeInput: Boolean = True;

//------------------------------------------------------------------------------
// 编码的参考表
//------------------------------------------------------------------------------

  EnCodeTab: array[0..64] of AnsiChar =
  (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/',
    '=');

  EnCodeTabURL: array[0..64] of AnsiChar =
  (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '-', '_',
    '=');

//------------------------------------------------------------------------------
// 解码的参考表
//------------------------------------------------------------------------------

  { 不包含在 Base64 里面的字符直接给零，反正也取不到}
  DecodeTable: array[#0..#127] of Byte =
  (
    Byte('='), 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 62, 00, 62, 00, 63,  // 这里的第一个 62、和 63 是 + 和 /，因而 - 补上 62
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 00, 00, 00, 00, 00, 00,
    00, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 00, 00, 00, 00, 63,  // _ 补上 63
    00, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 00, 00, 00, 00, 00
  );

function Base64Encode(InputData: TStream; var OutputData: string; URL: Boolean): Integer;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.CopyFrom(InputData, InputData.Size);
    Result := Base64Encode(Mem.Memory, Mem.Size, OutputData, URL);
  finally
    Mem.Free;
  end;
end;

// 以下为 wr960204 改进的快速 Base64 编解码算法
function Base64Encode(InputData: Pointer; DataLen: Integer; var OutputData: string;
  URL: Boolean): Integer;
var
  Times, I: Integer;
  x1, x2, x3, x4: AnsiChar;
  Xt: Byte;
begin
  if (InputData = nil) or (DataLen <= 0) then
  begin
    Result := ECN_BASE64_LENGTH;
    Exit;
  end;

  if DataLen mod 3 = 0 then
    Times := DataLen div 3
  else
    Times := DataLen div 3 + 1;
  SetLength(OutputData, Times * 4);   //一次分配整块内存,避免一次次字符串相加,一次次释放分配内存

  if URL then
  begin
    for I := 0 to Times - 1 do
    begin
      if DataLen >= (3 + I * 3) then
      begin
        x1 := EnCodeTabURL[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        Xt := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        Xt := Xt or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        x2 := EnCodeTabURL[Xt];
        Xt := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        Xt := Xt or (Ord(PAnsiChar(InputData)[2 + I * 3]) shr 6);
        x3 := EnCodeTabURL[Xt];
        Xt := (Ord(PAnsiChar(InputData)[2 + I * 3]) and 63);
        x4 := EnCodeTabURL[Xt];
      end
      else if DataLen >= (2 + I * 3) then
      begin
        x1 := EnCodeTabURL[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        Xt := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        Xt := Xt or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        x2 := EnCodeTabURL[Xt];
        Xt := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        x3 := EnCodeTabURL[Xt ];
        x4 := '=';
      end
      else
      begin
        x1 := EnCodeTabURL[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        Xt := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        x2 := EnCodeTabURL[Xt];
        x3 := '=';
        x4 := '=';
      end;
      OutputData[I shl 2 + 1] := Char(X1);
      OutputData[I shl 2 + 2] := Char(X2);
      OutputData[I shl 2 + 3] := Char(X3);
      OutputData[I shl 2 + 4] := Char(X4);
    end;
  end
  else
  begin
    for I := 0 to Times - 1 do
    begin
      if DataLen >= (3 + I * 3) then
      begin
        x1 := EnCodeTab[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        Xt := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        Xt := Xt or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        x2 := EnCodeTab[Xt];
        Xt := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        Xt := Xt or (Ord(PAnsiChar(InputData)[2 + I * 3]) shr 6);
        x3 := EnCodeTab[Xt];
        Xt := (Ord(PAnsiChar(InputData)[2 + I * 3]) and 63);
        x4 := EnCodeTab[Xt];
      end
      else if DataLen >= (2 + I * 3) then
      begin
        x1 := EnCodeTab[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        Xt := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        Xt := Xt or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        x2 := EnCodeTab[Xt];
        Xt := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        x3 := EnCodeTab[Xt ];
        x4 := '=';
      end
      else
      begin
        x1 := EnCodeTab[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        Xt := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        x2 := EnCodeTab[Xt];
        x3 := '=';
        x4 := '=';
      end;
      OutputData[I shl 2 + 1] := Char(X1);
      OutputData[I shl 2 + 2] := Char(X2);
      OutputData[I shl 2 + 3] := Char(X3);
      OutputData[I shl 2 + 4] := Char(X4);
    end;
  end;

  OutputData := Trim(OutputData);
  if URL then
  begin
    // 删除 OutputData 尾部的 = 字符，理论上最多三个
    if (Length(OutputData) > 0) and (OutputData[Length(OutputData)] = '=') then
    begin
      Delete(OutputData, Length(OutputData), 1);
      if (Length(OutputData) > 0) and (OutputData[Length(OutputData)] = '=') then
      begin
        Delete(OutputData, Length(OutputData), 1);
        if (Length(OutputData) > 0) and (OutputData[Length(OutputData)] = '=') then
          Delete(OutputData, Length(OutputData), 1);
      end;
    end;
  end;
  Result := ECN_BASE64_OK;
end;

function Base64Encode(const InputData: AnsiString; var OutputData: string; URL: Boolean): Integer;
begin
  if InputData <> '' then
    Result := Base64Encode(@InputData[1], Length(InputData), OutputData, URL)
  else
    Result := ECN_BASE64_LENGTH;
end;

function Base64Encode(InputData: TBytes; var OutputData: string; URL: Boolean): Integer;
begin
  if Length(InputData) > 0 then
    Result := Base64Encode(@InputData[0], Length(InputData), OutputData, URL)
  else
    Result := ECN_BASE64_LENGTH;
end;

function Base64Decode(const InputData: AnsiString; OutputData: TStream; FixZero: Boolean): Integer;
var
  Data: TBytes;
begin
  Result := Base64Decode(string(InputData), Data, FixZero);
  if (Result = ECN_BASE64_OK) and (Length(Data) > 0) then
  begin
    OutputData.Size := Length(Data);
    OutputData.Position := 0;
    OutputData.Write(Data[0], Length(Data));
  end;
end;

function Base64Decode(InputData: string; out OutputData: TBytes;
  FixZero: Boolean): Integer;
var
  SrcLen, DstLen, Times, I: Integer;
  x1, x2, x3, x4, xt: Byte;
  C, ToDec: Integer;
  Data: AnsiString;

  function FilterLine(const Source: AnsiString): AnsiString;
  var
    P, PP: PAnsiChar;
    I: Integer;
  begin
    SrcLen := Length(Source);
    GetMem(P, Srclen);                   // 一次分配整块内存,避免一次次字符串相加,一次次释放分配内存
    PP := P;
    FillChar(P^, Srclen, 0);
    for I := 1 to SrcLen do
    begin
      if Source[I] in ['0'..'9', 'A'..'Z', 'a'..'z', '+', '/', '=', '-', '_'] then
      begin
        PP^ := Source[I];
        Inc(PP);
      end;
    end;
    SetString(Result, P, PP - P);        // 截取有效部分
    FreeMem(P, SrcLen);
  end;

begin
  if (InputData = '') then
  begin
    Result := ECN_BASE64_OK;
    Exit;
  end;
  OutPutData := nil;

  if FilterDecodeInput then
    Data := FilterLine(AnsiString(InputData))
  else
    Data := AnsiString(InputData);

  // 如果是 Base64URL 编码的结果去掉了尾部的 =，则需要根据长度是否是 4 的倍数而补上
  if (Length(Data) and $03) <> 0 then
    Data := Data + StringOfChar(AnsiChar('='), 4 - (Length(Data) and $03));

  SrcLen := Length(Data);
  DstLen := SrcLen * 3 div 4;
  ToDec := 0;

  // 尾部有一个等号意味着原始数据补了个 #0，两个等号意味着补了两个 #0，需要去掉也就是缩短长度
  // 注意这不等同于原始数据的尾部是 #0 的情况，后者无须去掉
  if Data[SrcLen] = '=' then
  begin
    Inc(ToDec);
    if (SrcLen > 1) and (Data[SrcLen - 1] = '=') then
      Inc(ToDec);
  end;

  SetLength(OutputData, DstLen);  // 一次分配整块内存,避免一次次字符串相加,一次次释放分配内存
  Times := SrcLen div 4;
  C := 0;

  for I := 0 to Times - 1 do
  begin
    x1 := DecodeTable[Data[1 + I shl 2]];
    x2 := DecodeTable[Data[2 + I shl 2]];
    x3 := DecodeTable[Data[3 + I shl 2]];
    x4 := DecodeTable[Data[4 + I shl 2]];
    x1 := x1 shl 2;
    xt := x2 shr 4;
    x1 := x1 or xt;
    x2 := x2 shl 4;
    OutputData[C] := x1;
    Inc(C);
    if x3 = 64 then
      Break;
    xt := x3 shr 2;
    x2 := x2 or xt;
    x3 := x3 shl 6;
    OutputData[C] := x2;
    Inc(C);
    if x4 = 64 then
      Break;
    x3 := x3 or x4;
    OutputData[C] := x3;
    Inc(C);
  end;

  // 根据补的等号数目决定是否删除尾部 #0
  while (ToDec > 0) and (OutputData[DstLen - 1] = 0) do
  begin
    Dec(ToDec);
    Dec(DstLen);
  end;
  SetLength(OutputData, DstLen);

  // 再根据外部要求删除尾部的 #0，其实无太大的实质性作用
  if FixZero then
  begin
    while (DstLen > 0) and (OutputData[DstLen - 1] = 0) do
      Dec(DstLen);
    SetLength(OutputData, DstLen);
  end;

  Result := ECN_BASE64_OK;
end;

function Base64Decode(const InputData: AnsiString; var OutputData: AnsiString; FixZero: Boolean): Integer;
var
  Data: TBytes;
begin
  Result := Base64Decode(string(InputData), Data, FixZero);
  if (Result = ECN_BASE64_OK) and (Length(Data) > 0) then
  begin
    SetLength(OutputData, Length(Data));
    Move(Data[0], OutputData[1], Length(Data));
  end;
end;

end.
