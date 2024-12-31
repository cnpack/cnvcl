{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnCryptoExport;
{* |<PRE>
================================================================================
* 软件名称：CnPack 密码库
* 单元名称：CnPack 密码库 DLL 输出函数封装单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：编译成 DLL 时输出原始调用函数的封装单元，目前仅包括杂凑系列函数
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2024.06.18 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative;

const
  CN_HASH_TYPE_UNKNOWN   = 0;
  CN_HASH_TYPE_SM3       = 1;
  CN_HASH_TYPE_MD5       = 2;
  CN_HASH_TYPE_SHA1      = 3;
  CN_HASH_TYPE_SHA224    = 4;
  CN_HASH_TYPE_SHA256    = 5;
  CN_HASH_TYPE_SHA384    = 6;
  CN_HASH_TYPE_SHA512    = 7;
  CN_HASH_TYPE_SHA3_224  = 8;
  CN_HASH_TYPE_SHA3_256  = 9;
  CN_HASH_TYPE_SHA3_384  = 10;
  CN_HASH_TYPE_SHA3_512  = 11;
  CN_HASH_TYPE_POLY1305  = 12;
  CN_HASH_TYPE_CRC8      = 13;
  CN_HASH_TYPE_CRC16     = 14;
  CN_HASH_TYPE_CRC32     = 15;
  CN_HASH_TYPE_CRC64     = 16;
  CN_HASH_TYPE_FNV132    = 17;
  CN_HASH_TYPE_FNV164    = 18;
  CN_HASH_TYPE_FNV1128   = 19;
  CN_HASH_TYPE_FNV1256   = 20;
  CN_HASH_TYPE_FNV1512   = 21;
  CN_HASH_TYPE_FNV11024  = 22;
  CN_HASH_TYPE_FNV1A32   = 23;
  CN_HASH_TYPE_FNV1A64   = 24;
  CN_HASH_TYPE_FNV1A128  = 25;
  CN_HASH_TYPE_FNV1A256  = 26;
  CN_HASH_TYPE_FNV1A512  = 27;
  CN_HASH_TYPE_FNV1A1024 = 28;
  CN_HASH_TYPE_SHAKE128  = 29;
  CN_HASH_TYPE_SHAKE256  = 30;
  {* 支持的杂凑类型，注意只有部分类型支持 MAC}

function Cn_Hash_Data(InData: Pointer; DataByteLength: Cardinal; HashType: Integer;
  OutResult: Pointer): Integer; stdcall;
{* 根据不同的杂凑算法对指定数据块进行杂凑计算，计算结果放至 OutResult 指的内存区中
  返回值为杂凑结果的字节长度。
  如果 OutResult 传 nil，则不进行实际的杂凑，直接返回所需结果的长度供外界分配内存
  如果返回值为 0，代表不支持的杂凑算法}

function Cn_Hash_WideString(InWideStr: PWideChar; WideCharLength: Integer; HashType: Integer;
  OutResult: Pointer): Integer; stdcall;
{* 根据不同的杂凑算法对指定宽字符串进行杂凑计算，计算结果放至 OutResult 指的内存区中
  内部使用 UTF8 编码，返回值为杂凑结果的字节长度。
  如果 OutResult 传 nil，则不进行实际的杂凑，直接返回所需结果的长度供外界分配内存
  如果返回值为 0，代表不支持的杂凑算法}

function Cn_Hash_Mac_Data(InData: Pointer; DataByteLength: Cardinal; InKey: Pointer;
  KeyByteLength: Cardinal; HashType: Integer; OutResult: Pointer): Integer; stdcall;
{* 根据不同的杂凑算法及密码对指定数据块进行 MAC 杂凑计算，计算结果放至 OutResult 指的内存区中
  返回值为杂凑结果的字节长度。
  如果 OutResult 传 nil，则不进行实际的杂凑，直接返回所需结果的长度供外界分配内存
  如果返回值为 0，代表不支持的杂凑 MAC 算法}

function Cn_Hash_Mac_WideString(InWideStr: PWideChar; WideCharLength: Integer; InKey: Pointer;
  KeyByteLength: Cardinal; HashType: Integer; OutResult: Pointer): Integer; stdcall;
{* 根据不同的杂凑算法及密码对指定指定宽字符串进行 MAC 杂凑计算，计算结果放至 OutResult 指的内存区中
  输入的宽字符串内部使用 UTF8 编码，Key 不进行额外编码。返回值为杂凑结果的字节长度。
  如果 OutResult 传 nil，则不进行实际的杂凑，直接返回所需结果的长度供外界分配内存
  如果返回值为 0，代表不支持的杂凑 MAC 算法}

exports
  Cn_Hash_Data,
  Cn_Hash_WideString,
  Cn_Hash_Mac_Data,
  Cn_Hash_Mac_WideString;

implementation

uses
  CnWideStrings, CnSM3, CnMD5,CnSHA1, CnSHA2, CnSHA3, CnPoly1305, CnCRC32, CnFNV;

type
  TCn_Hash_Result = packed record
    case Integer of
      CN_HASH_TYPE_SM3:       (SM3: TCnSM3Digest);
      CN_HASH_TYPE_MD5:       (MD5: TCnMD5Digest);
      CN_HASH_TYPE_SHA1:      (SHA1: TCnSHA1Digest);
      CN_HASH_TYPE_SHA224:    (SHA224: TCnSHA224Digest);
      CN_HASH_TYPE_SHA256:    (SHA256: TCnSHA256Digest);
      CN_HASH_TYPE_SHA384:    (SHA384: TCnSHA384Digest);
      CN_HASH_TYPE_SHA512:    (SHA512: TCnSHA512Digest);
      CN_HASH_TYPE_SHA3_224:  (SHA3_224: TCnSHA3_224Digest);
      CN_HASH_TYPE_SHA3_256:  (SHA3_256: TCnSHA3_256Digest);
      CN_HASH_TYPE_SHA3_384:  (SHA3_384: TCnSHA3_384Digest);
      CN_HASH_TYPE_SHA3_512:  (SHA3_512: TCnSHA3_512Digest);
      CN_HASH_TYPE_POLY1305:  (POLY1305: TCnPoly1305Digest);
      CN_HASH_TYPE_CRC8:      (CRC8: Byte);
      CN_HASH_TYPE_CRC16:     (CRC16: Word);
      CN_HASH_TYPE_CRC32:     (CRC32: Cardinal);
      CN_HASH_TYPE_CRC64:     (CRC64: Int64);
      CN_HASH_TYPE_FNV132:    (FNV132: TCnFNVHash32);
      CN_HASH_TYPE_FNV164:    (FNV164: TCnFNVHash64);
      CN_HASH_TYPE_FNV1128:   (FNV1128: TCnFNVHash128);
      CN_HASH_TYPE_FNV1256:   (FNV1256: TCnFNVHash256);
      CN_HASH_TYPE_FNV1512:   (FNV1512: TCnFNVHash512);
      CN_HASH_TYPE_FNV11024:  (FNV11024: TCnFNVHash1024);
      CN_HASH_TYPE_FNV1A32:   (FNV1A32: TCnFNVHash32);
      CN_HASH_TYPE_FNV1A64:   (FNV1A64: TCnFNVHash64);
      CN_HASH_TYPE_FNV1A128:  (FNV1A128: TCnFNVHash128);
      CN_HASH_TYPE_FNV1A256:  (FNV1A256: TCnFNVHash256);
      CN_HASH_TYPE_FNV1A512:  (FNV1A512: TCnFNVHash512);
      CN_HASH_TYPE_FNV1A1024: (FNV1A1024: TCnFNVHash1024);
      CN_HASH_TYPE_SHAKE128:  (SHAKE128: array[0..CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH - 1] of Byte);
      CN_HASH_TYPE_SHAKE256:  (SHAKE256: array[0..CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH - 1] of Byte);
  end;

function Cn_Hash_Data(InData: Pointer; DataByteLength: Cardinal; HashType: Integer;
  OutResult: Pointer): Integer;
var
  Poly1305Key: TCnPoly1305Key;
  Dig: TCn_Hash_Result;
  R: TBytes;
begin
  Result := 0;
  case HashType of
    CN_HASH_TYPE_SM3:
      begin
        Result := SizeOf(TCnSM3Digest);
        if OutResult <> nil then
          Dig.SM3 := SM3(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_MD5:
      begin
        Result := SizeOf(TCnMD5Digest);
        if OutResult <> nil then
          Dig.MD5 := MD5(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA1:
      begin
        Result := SizeOf(TCnSHA1Digest);
        if OutResult <> nil then
          Dig.SHA1 := SHA1(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA224:
      begin
        Result := SizeOf(TCnSHA224Digest);
        if OutResult <> nil then
          Dig.SHA224 := SHA224(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA256:
      begin
        Result := SizeOf(TCnSHA256Digest);
        if OutResult <> nil then
          Dig.SHA256 := SHA256(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA384:
      begin
        Result := SizeOf(TCnSHA384Digest);
        if OutResult <> nil then
          Dig.SHA384 := SHA384(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA512:
      begin
        Result := SizeOf(TCnSHA512Digest);
        if OutResult <> nil then
          Dig.SHA512 := SHA512(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA3_224:
      begin
        Result := SizeOf(TCnSHA3_224Digest);
        if OutResult <> nil then
          Dig.SHA3_224 := SHA3_224(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA3_256:
      begin
        Result := SizeOf(TCnSHA3_256Digest);
        if OutResult <> nil then
          Dig.SHA3_256 := SHA3_256(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA3_384:
      begin
        Result := SizeOf(TCnSHA3_384Digest);
        if OutResult <> nil then
          Dig.SHA3_384 := SHA3_384(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_SHA3_512:
      begin
        Result := SizeOf(TCnSHA3_512Digest);
        if OutResult <> nil then
          Dig.SHA3_512 := SHA3_512(PAnsiChar(InData), DataByteLength);
      end;
    CN_HASH_TYPE_POLY1305:
      begin
        Result := SizeOf(TCnPoly1305Digest);
        if OutResult <> nil then
        begin
          FillChar(Poly1305Key, SizeOf(TCnPoly1305Key), 0); // 用 32 字节全 0 作为 Key
          Dig.POLY1305 := Poly1305Data(PAnsiChar(InData), DataByteLength, Poly1305Key);
        end;
      end;
    CN_HASH_TYPE_CRC8:
      begin
        Result := SizeOf(Byte);
        if OutResult <> nil then
          Dig.CRC8 := CRC8Calc(0, PAnsiChar(InData)^, DataByteLength);
      end;
    CN_HASH_TYPE_CRC16:
      begin
        Result := SizeOf(Word);
        if OutResult <> nil then
          Dig.CRC16 := CRC16Calc(0, PAnsiChar(InData)^, DataByteLength);
      end;
    CN_HASH_TYPE_CRC32:
      begin
        Result := SizeOf(Cardinal);
        if OutResult <> nil then
          Dig.CRC32 := CRC32Calc(0, PAnsiChar(InData)^, DataByteLength);
      end;
    CN_HASH_TYPE_CRC64:
      begin
        Result := SizeOf(Int64);
        if OutResult <> nil then
          Dig.CRC64 := CRC64Calc(0, PAnsiChar(InData)^, DataByteLength);
      end;
    CN_HASH_TYPE_FNV132:
      begin
        Result := SizeOf(TCnFNVHash32);
        if OutResult <> nil then
          Dig.FNV132 := FNV1Hash32(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV164:
      begin
        Result := SizeOf(TCnFNVHash64);
        if OutResult <> nil then
          Dig.FNV164 := FNV1Hash64(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1128:
      begin
        Result := SizeOf(TCnFNVHash128);
        if OutResult <> nil then
          Dig.FNV1128 := FNV1Hash128(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1256:
      begin
        Result := SizeOf(TCnFNVHash256);
        if OutResult <> nil then
          Dig.FNV1256 := FNV1Hash256(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1512:
      begin
        Result := SizeOf(TCnFNVHash512);
        if OutResult <> nil then
          Dig.FNV1512 := FNV1Hash512(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV11024:
      begin
        Result := SizeOf(TCnFNVHash1024);
        if OutResult <> nil then
          Dig.FNV11024 := FNV1Hash1024(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1A32:
      begin
        Result := SizeOf(TCnFNVHash32);
        Dig.FNV132 := FNV1AHash32(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1A64:
      begin
        Result := SizeOf(TCnFNVHash64);
        if OutResult <> nil then
          Dig.FNV1A64 := FNV1AHash64(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1A128:
      begin
        Result := SizeOf(TCnFNVHash128);
        if OutResult <> nil then
          Dig.FNV1A128 := FNV1AHash128(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1A256:
      begin
        Result := SizeOf(TCnFNVHash256);
        if OutResult <> nil then
          Dig.FNV1A256 := FNV1AHash256(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1A512:
      begin
        Result := SizeOf(TCnFNVHash512);
        if OutResult <> nil then
          Dig.FNV1A512 := FNV1AHash512(InData, DataByteLength);
      end;
    CN_HASH_TYPE_FNV1A1024:
      begin
        Result := SizeOf(TCnFNVHash1024);
        if OutResult <> nil then
          Dig.FNV1A1024 := FNV1AHash1024(InData, DataByteLength);
      end;
    CN_HASH_TYPE_SHAKE128:
      begin
        Result := CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH;
        if OutResult <> nil then
        begin
          R := SHAKE128Buffer(InData, DataByteLength, Result);
          Move(R[0], Dig.SHAKE128[0], Result);
        end;
      end;
    CN_HASH_TYPE_SHAKE256:
      begin
        Result := CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH;
        if OutResult <> nil then
        begin
          R := SHAKE256Buffer(InData, DataByteLength, Result);
          Move(R[0], Dig.SHAKE256[0], Result);
        end;
      end;
  end;

  if (Result > 0) and (OutResult <> nil) then
    Move(Dig, OutResult^, Result);
end;

function Cn_Hash_WideString(InWideStr: PWideChar; WideCharLength: Integer; HashType: Integer;
  OutResult: Pointer): Integer;
var
  WS: WideString;
  UTF8: AnsiString;
begin
  if (OutResult = nil) or (WideCharLength < 0) then
  begin
    Result := Cn_Hash_Data(nil, 0, HashType, nil);
    Exit;
  end;

  SetLength(WS, WideCharLength);
  if WideCharLength > 0 then
    Move(InWIdeStr^, WS[1], WideCharLength * SizeOf(WideChar));

  UTF8 := CnUtf8EncodeWideString(WS);
  if Length(UTF8) > 0 then
    Result := Cn_Hash_Data(@UTF8[1], Length(UTF8), HashType, OutResult)
  else
    Result := Cn_Hash_Data(nil, 0, HashType, OutResult);
end;

function Cn_Hash_Mac_Data(InData: Pointer; DataByteLength: Cardinal; InKey: Pointer;
  KeyByteLength: Cardinal; HashType: Integer; OutResult: Pointer): Integer;
var
  Poly1305Key: TCnPoly1305Key;
  Dig: TCn_Hash_Result;
begin
  Result := 0;
  case HashType of
    CN_HASH_TYPE_SM3:
      begin
        Result := SizeOf(TCnSM3Digest);
        if OutResult <> nil then
          SM3Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SM3);
      end;
    CN_HASH_TYPE_MD5:
      begin
        Result := SizeOf(TCnMD5Digest);
        if OutResult <> nil then
          MD5Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.MD5);
      end;
    CN_HASH_TYPE_SHA1:
      begin
        Result := SizeOf(TCnSHA1Digest);
        if OutResult <> nil then
          SHA1Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA1);
      end;
    CN_HASH_TYPE_SHA224:
      begin
        Result := SizeOf(TCnSHA224Digest);
        if OutResult <> nil then
          SHA224Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA224);
      end;
    CN_HASH_TYPE_SHA256:
      begin
        Result := SizeOf(TCnSHA256Digest);
        if OutResult <> nil then
          SHA256Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA256);
      end;
    CN_HASH_TYPE_SHA384:
      begin
        Result := SizeOf(TCnSHA384Digest);
        if OutResult <> nil then
          SHA384Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA384);
      end;
    CN_HASH_TYPE_SHA512:
      begin
        Result := SizeOf(TCnSHA512Digest);
        if OutResult <> nil then
          SHA512Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA512);
      end;
    CN_HASH_TYPE_SHA3_224:
      begin
        Result := SizeOf(TCnSHA3_224Digest);
        if OutResult <> nil then
          SHA3_224Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA3_224);
      end;
    CN_HASH_TYPE_SHA3_256:
      begin
        Result := SizeOf(TCnSHA3_256Digest);
        if OutResult <> nil then
          SHA3_256Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA3_256);
      end;
    CN_HASH_TYPE_SHA3_384:
      begin
        Result := SizeOf(TCnSHA3_384Digest);
        if OutResult <> nil then
          SHA3_384Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA3_384);
      end;
    CN_HASH_TYPE_SHA3_512:
      begin
        Result := SizeOf(TCnSHA3_512Digest);
        if OutResult <> nil then
          SHA3_512Hmac(PAnsiChar(InKey), KeyByteLength, PAnsiChar(InData), DataByteLength, Dig.SHA3_512);
      end;
    CN_HASH_TYPE_POLY1305:
      begin
        Result := SizeOf(TCnPoly1305Digest);
        if OutResult <> nil then
        begin
          FillChar(Poly1305Key, SizeOf(TCnPoly1305Key), 0);
          MoveMost(InKey^, Poly1305Key[0], KeyByteLength, Result);
          Dig.POLY1305 := Poly1305Data(PAnsiChar(InData), DataByteLength, Poly1305Key);
        end;
      end;
  end;

  if (Result > 0) and (OutResult <> nil) then
    Move(Dig, OutResult^, Result);
end;

function Cn_Hash_Mac_WideString(InWideStr: PWideChar; WideCharLength: Integer; InKey: Pointer;
  KeyByteLength: Cardinal; HashType: Integer; OutResult: Pointer): Integer;
var
  WS: WideString;
  UTF8: AnsiString;
begin
  if (OutResult = nil) or (WideCharLength < 0) then
  begin
    Result := Cn_Hash_Mac_Data(nil, 0, InKey, KeyByteLength, HashType, nil);
    Exit;
  end;

  SetLength(WS, WideCharLength);
  if WideCharLength > 0 then
    Move(InWIdeStr^, WS[1], WideCharLength * SizeOf(WideChar));

  UTF8 := CnUtf8EncodeWideString(WS);
  if Length(UTF8) > 0 then
    Result := Cn_Hash_Mac_Data(@UTF8[1], Length(UTF8), InKey, KeyByteLength, HashType, OutResult)
  else
    Result := Cn_Hash_Mac_Data(nil, 0, InKey, KeyByteLength, HashType, OutResult);
end;


end.
