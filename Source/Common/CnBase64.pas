{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

{******************************************************************************}
{        该单元部分内容基于Dennis D. Spreen的UTBASE64.pas改写。                }
{        下面是UTBASE64.pas的声明:                                             }
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
* 单元名称：Base64编码算法单元
* 单元作者：詹葵（Solin） solin@21cn.com; http://www.ilovezhuzhu.net
*           wr960204
* 备    注：该单元有两个版本的Base64实现，分别属移植改进而来。
* 开发平台：PWin2003Std + Delphi 6.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2006.10.25 V1.1
*               增加 wr960204 的优化版本
*           2003.10.14 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

function Base64Encode(InputData: TStream; var OutputData: string): Byte; overload;
function Base64Encode(const InputData: AnsiString; var OutputData: string): Byte; overload;
{* 对数据进行BASE64编码，如编码成功返回Base64_OK
|<PRE>
  InputData:AnsiString        - 要编码的数据
  var OutputData: AnsiString  - 编码后的数据
|</PRE>}

function Base64Decode(const InputData: AnsiString; var OutputData: AnsiString; FixZero: Boolean = True): Byte; overload;
function Base64Decode(const InputData: AnsiString; OutputData: TStream; FixZero: Boolean = True): Byte; overload;
{* 对数据进行BASE64解码，如解码成功返回Base64_OK
|<PRE>
  InputData:AnsiString        - 要解码的数据
  var OutputData: AnsiString  - 解码后的数据
  FixZero: Boolean            - 是否移去尾部的 #0
|</PRE>}

// 原始移植的版本，比较慢
function Base64Encode_Slow(const InputData: AnsiString; var OutputData: AnsiString): Byte;

// 原始移植的版本，比较慢
function Base64Decode_Slow(const InputData: AnsiString; var OutputData: AnsiString): Byte;

const
  BASE64_OK       = 0; // 转换成功
  BASE64_ERROR    = 1; // 转换错误（未知错误） (e.g. can't encode octet in input stream) -> error in implementation
  BASE64_INVALID  = 2; // 输入的字符串中有非法字符 (在 FilterDecodeInput=False 时可能出现)
  BASE64_LENGTH   = 3; // 数据长度非法
  BASE64_DATALEFT = 4; // too much input data left (receveived 'end of encoded data' but not end of input string)
  BASE64_PADDING  = 5; // 输入的数据未能以正确的填充字符结束

implementation

var
  FilterDecodeInput: Boolean = True;

const
  Base64TableLength = 64;
  Base64Table:string[Base64TableLength]='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Pad = '=';

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

//------------------------------------------------------------------------------
// 解码的参考表
//------------------------------------------------------------------------------

  { 不包含在 Base64 里面的字符直接给零, 反正也取不到}
  DecodeTable: array[#0..#127] of Byte =
  (
    Byte('='), 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 62, 00, 00, 00, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 00, 00, 00, 00, 00, 00,
    00, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 00, 00, 00, 00, 00,
    00, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 00, 00, 00, 00, 00
    );  

// 原始移植的版本，比较慢
function Base64Encode_Slow(const InputData: AnsiString; var OutputData:AnsiString): Byte;
var
  i: Integer;
  CurrentB,PrevB: Byte;
  c: Byte;
  s: AnsiChar;
  InputLength: Integer;

  function ValueToCharacter(Value: Byte; var Character: AnsiChar): Boolean;
  //******************************************************************
  // 将一个在0..Base64TableLength-1区间内的值，转换为与Base64编码相对应
  // 的字符来表示，如果转换成功则返回True
  //******************************************************************
  begin
    Result := True;
    if (Value > Base64TableLength - 1) then
      Result := False
    else
      Character := AnsiChar(Base64Table[Value + 1]);
  end;

begin
  OutPutData := '';
  InputLength := Length(InputData);
  i:=1;
  if (InputLength = 0) then
  begin
    Result := BASE64_OK;
    Exit;
  end;

  repeat
    // 第一次转换
    CurrentB := Ord(InputData[i]);
    Inc(i);
    InputLength := InputLength-1;
    c := (CurrentB shr 2);
    if not ValueToCharacter(c, s) then
    begin
      Result := BASE64_ERROR;
      Exit;
    end;
    OutPutData := OutPutData + s;
    PrevB := CurrentB;

    // 第二次转换
    if InputLength = 0 then
      CurrentB := 0
    else
    begin
      CurrentB := Ord(InputData[i]);
      Inc(i);
    end;

    InputLength := InputLength-1;
    c:=(PrevB and $03) shl 4 + (CurrentB shr 4);  //取出XX后4位并将其左移4位与XX右移4位合并成六位
    if not ValueToCharacter(c,s) then             //检测取得的字符是否在Base64Table内
    begin
      Result := BASE64_ERROR;
      Exit;
    end;
    OutPutData := OutPutData+s;
    PrevB := CurrentB;

    // 第三次转换
    if InputLength<0 then
      s := pad
    else
    begin
      if InputLength = 0 then
        CurrentB := 0
      else
      begin
        CurrentB := Ord(InputData[i]);
        Inc(i);
      end;
      InputLength := InputLength - 1;
      c := (PrevB and $0F) shl 2 + (CurrentB shr 6);//取出XX后4位并将其左移2位与XX右移6位合并成六位
      if not ValueToCharacter(c, s) then           //检测取得的字符是否在Base64Table内
      begin
        Result := BASE64_ERROR;
        Exit;
      end;
    end;
    OutPutData:=OutPutData+s;

    // 第四次转换
    if InputLength < 0 then
      s := pad
    else
    begin
      c := (CurrentB and $3F);                      //取出XX后6位
      if not ValueToCharacter(c, s) then           //检测取得的字符是否在Base64Table内
      begin
        Result := BASE64_ERROR;
        Exit;
      end;
    end;
    OutPutData := OutPutData + s;
  until InputLength <= 0;

  Result:=BASE64_OK;
end;

// 原始移植的版本，比较慢
function Base64Decode_Slow(const InputData: AnsiString; var OutputData: AnsiString): Byte;
var
  i: Integer;
  InputLength: Integer;
  CurrentB, PrevB: Byte;
  c: Byte;
  s: AnsiChar;
  Data: AnsiString;

  function CharacterToValue(Character: AnsiChar; var Value: Byte): Boolean;
  //******************************************************************
  // 转换字符为一在0..Base64TableLength-1区间中的值，如果转换成功则返
  // 回True(即字符在Base64Table中)
  //******************************************************************
  begin
    Result := True;
    Value := Pos(Character, Base64Table);
    if Value=0 then
      Result := False
    else
      Value := Value - 1;
  end;

  function FilterLine(const InputData: AnsiString): AnsiString;
  //******************************************************************
  // 过滤所有不在Base64Table中的字符，返回值为过滤后的字符
  //******************************************************************
  var
    f: Byte;
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(InputData) do
      if CharacterToValue(inputData[i], f) or (InputData[i] = Pad) then
        Result:=Result + InputData[i];
  end;

begin
  if (InputData = '') then
  begin
    Result := BASE64_OK;
    Exit;
  end;
  OutPutData := '';

  if FilterDecodeInput then
    Data := FilterLine(InputData)
  else
    Data := InputData;

  InputLength := Length(Data);
  if InputLength mod 4 <> 0 then
  begin
    Result := BASE64_LENGTH;
    Exit;
  end;

  i := 0;
  repeat
    // 第一次转换
    Inc(i);
    s := Data[i];
    if not CharacterToValue(s, CurrentB) then
    begin
      Result := BASE64_INVALID;
      Exit;
    end;

    Inc(i);
    s := Data[i];
    if not CharacterToValue(s, PrevB) then
    begin
      Result := BASE64_INVALID;
      Exit;
    end;

    c := (CurrentB shl 2) + (PrevB shr 4);
    OutPutData := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(OutPutData + {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(Chr(c)));

    // 第二次转换
    Inc(i);
    s := Data[i];
    if s = pad then
    begin
      if (i <> InputLength-1) then
      begin
        Result := BASE64_DATALEFT;
        Exit;
      end
      else
      if Data[i + 1] <> pad then
      begin
        Result := BASE64_PADDING;
        Exit;
      end;
    end
    else
    begin
      if not CharacterToValue(s,CurrentB) then
      begin
        Result:=BASE64_INVALID;
        Exit;
      end;
      c:=(PrevB shl 4) + (CurrentB shr 2);
      OutPutData := OutPutData+{$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(chr(c));
    end;

    // 第三次转换
    Inc(i);
    s := Data[i];
    if s = pad then
    begin
      if (i <> InputLength) then
      begin
        Result := BASE64_DATALEFT;
        Exit;
      end;
    end
    else
    begin
     if not CharacterToValue(s, PrevB) then
     begin
       Result := BASE64_INVALID;
       Exit;
     end;
     c := (CurrentB shl 6) + (PrevB);
     OutPutData := OutPutData + {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(Chr(c));
    end;
  until (i >= InputLength);

  Result:=BASE64_OK;
end;

// 以下为 wr960204 改进的快速 Base64 编解码算法
function Base64Encode(InputData: TStream; var OutputData: string): Byte; overload;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    Str.CopyFrom(InputData, InputData.Size);
    Result := Base64Encode({$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(Str.DataString), OutputData);
  finally
    Str.Free;
  end;
end;

function Base64Encode(const InputData: AnsiString; var OutputData: string): Byte; overload;
var
  Times, SrcLen, i: Integer;
  x1, x2, x3, x4: AnsiChar;
  xt: byte;
begin
  SrcLen := Length(InputData);
  if SrcLen mod 3 = 0 then
    Times := SrcLen div 3
  else
    Times := SrcLen div 3 + 1;
  SetLength(OutputData, Times * 4);   //一次分配整块内存,避免一次次字符串相加,一次次释放分配内存

  for i := 0 to times - 1 do
  begin
    if SrcLen >= (3 + i * 3) then
    begin
      x1 := EnCodeTab[(ord(InputData[1 + i * 3]) shr 2)];
      xt := (ord(InputData[1 + i * 3]) shl 4) and 48;
      xt := xt or (ord(InputData[2 + i * 3]) shr 4);
      x2 := EnCodeTab[xt];
      xt := (Ord(InputData[2 + i * 3]) shl 2) and 60;
      xt := xt or (ord(InputData[3 + i * 3]) shr 6);
      x3 := EnCodeTab[xt];
      xt := (ord(InputData[3 + i * 3]) and 63);
      x4 := EnCodeTab[xt];
    end
    else if SrcLen >= (2 + i * 3) then
    begin
      x1 := EnCodeTab[(ord(InputData[1 + i * 3]) shr 2)];
      xt := (ord(InputData[1 + i * 3]) shl 4) and 48;
      xt := xt or (ord(InputData[2 + i * 3]) shr 4);
      x2 := EnCodeTab[xt];
      xt := (ord(InputData[2 + i * 3]) shl 2) and 60;
      x3 := EnCodeTab[xt ];
      x4 := '=';
    end
    else
    begin
      x1 := EnCodeTab[(ord(InputData[1 + i * 3]) shr 2)];
      xt := (ord(InputData[1 + i * 3]) shl 4) and 48;
      x2 := EnCodeTab[xt];
      x3 := '=';
      x4 := '=';
    end;
    OutputData[I shl 2 + 1] := Char(X1);
    OutputData[I shl 2 + 2] := Char(X2);
    OutputData[I shl 2 + 3] := Char(X3);
    OutputData[I shl 2 + 4] := Char(X4);
  end;
  OutputData := Trim(OutputData);
  Result := BASE64_OK;
end;

function Base64Decode(const InputData: AnsiString; OutputData: TStream; FixZero: Boolean): Byte; overload;
var
  Str: AnsiString;
begin
  Result := Base64Decode(InputData, Str, FixZero);
  OutputData.Size := Length(Str);
  OutputData.Position := 0;
  if Str <> '' then
    OutputData.Write(Str[1], Length(Str));
end;

function Base64Decode(const InputData: AnsiString; var OutputData: AnsiString; FixZero: Boolean): Byte;
var
  SrcLen, DstLen, Times, i: Integer;
  x1, x2, x3, x4, xt: Byte;
  C: Integer;
  Data: AnsiString;

  function FilterLine(const Source: AnsiString): AnsiString;
  var
    P, PP: PAnsiChar;
    I: Integer;
  begin
    SrcLen := Length(Source);
    GetMem(P, Srclen);                   //一次分配整块内存,避免一次次字符串相加,一次次释放分配内存
    PP := P;
    FillChar(P^, Srclen, 0);
    for I := 1 to SrcLen do
    begin
      if Source[I] in ['0'..'9', 'A'..'Z', 'a'..'z', '+', '/', '='] then
      begin
        PP^ := Source[I];
        Inc(PP);
      end;
    end;
    SetString(Result, P, PP - P);        //截取有效部分
    FreeMem(P, SrcLen);
  end;

begin
  if (InputData = '') then
  begin
    Result := BASE64_OK;
    Exit;
  end;
  OutPutData:='';

  if FilterDecodeInput then
    Data := FilterLine(InputData)
  else
    Data := InputData;

  SrcLen := Length(Data);
  DstLen := SrcLen * 3 div 4;
  SetLength(OutputData, DstLen);  //一次分配整块内存,避免一次次字符串相加,一次次释放分配内存
  Times := SrcLen div 4;
  C := 1;

  for i := 0 to Times - 1 do
  begin
    x1 := DecodeTable[Data[1 + i shl 2]];
    x2 := DecodeTable[Data[2 + i shl 2]];
    x3 := DecodeTable[Data[3 + i shl 2]];
    x4 := DecodeTable[Data[4 + i shl 2]];
    x1 := x1 shl 2;
    xt := x2 shr 4;
    x1 := x1 or xt;
    x2 := x2 shl 4;
    OutputData[C] := AnsiChar(Chr(x1));
    Inc(C);
    if x3 = 64 then
      Break;
    xt := x3 shr 2;
    x2 := x2 or xt;
    x3 := x3 shl 6;
    OutputData[C] := AnsiChar(Chr(x2));
    Inc(C);
    if x4 = 64 then
      Break;
    x3 := x3 or x4;
    OutputData[C] := AnsiChar(Chr(x3));
    Inc(C);
  end;

  // 删除尾部的 #0
  if FixZero then
  begin
    while (DstLen > 0) and (OutputData[DstLen] = #0) do
      Dec(DstLen);
    SetLength(OutputData, DstLen);
  end;

  OutputData := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}({$IFDEF DELPHI12_UP}String{$ENDIF}(OutputData));
  Result := BASE64_OK;
end;

end.
