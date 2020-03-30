{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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

unit CnKeyDerivation;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：密码生成算法（KDF）单元
* 单元作者：刘啸
* 备    注：
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.03.30 V1.0
*               创建单元，从 CnPemUtils 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnMD5, CnSHA2;

type
  TCnKeyDeriveHash = (ckdMd5, ckdSha256);

  ECnKeyDerivation = class(Exception);

function CnGetDeriveKey(const Password, Salt: AnsiString; OutKey: PAnsiChar; KeyLength: Cardinal;
  KeyHash: TCnKeyDeriveHash = ckdMd5): Boolean;
{* 类似于 Openssl 中的 BytesToKey，用密码和盐与指定的 Hash 算法生成加密 Key，
  KeyLength 最多支持两轮 Hash，也就是 MD5 32 字节，SHA256 64 字节}

function CnPBKDF1(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer): AnsiString;

function CnPBKDF2(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer): AnsiString;

implementation

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function CnGetDeriveKey(const Password, Salt: AnsiString; OutKey: PAnsiChar; KeyLength: Cardinal;
  KeyHash: TCnKeyDeriveHash): Boolean;
var
  Md5Dig, Md5Dig2: TMD5Digest;
  Sha256Dig, Sha256Dig2: TSHA256Digest;
  SaltBuf, PS, PSMD5, PSSHA256: AnsiString;
begin
  Result := False;

  if (Password = '') or (OutKey = nil) or (KeyLength < 8) then
    Exit;

  SetLength(SaltBuf, 8);
  FillChar(SaltBuf[1], Length(SaltBuf), 0);
  if Salt <> '' then
  Move(Salt[1], SaltBuf[1], Min(Length(Salt), 8));

  PS := AnsiString(Password) + SaltBuf; // 规定前 8 个字节作为 Salt
  if KeyHash = ckdMd5 then
  begin
    SetLength(PSMD5, SizeOf(TMD5Digest) + Length(PS));
    Move(PS[1], PSMD5[SizeOf(TMD5Digest) + 1], Length(PS));
    Md5Dig := MD5StringA(PS);
    // 密码与 Salt 拼起来的 MD5 结果（16 Byte）作为第一部分

    Move(Md5Dig[0], OutKey^, Min(KeyLength, SizeOf(TMD5Digest)));
    if KeyLength <= SizeOf(TMD5Digest) then
    begin
      Result := True;
      Exit;
    end;

    KeyLength := KeyLength - SizeOf(TMD5Digest);
    OutKey := PAnsiChar(Integer(OutKey) + SizeOf(TMD5Digest));

    Move(Md5Dig[0], PSMD5[1], SizeOf(TMD5Digest));
    Md5Dig2 := MD5StringA(PSMD5);
    Move(Md5Dig2[0], OutKey^, Min(KeyLength, SizeOf(TMD5Digest)));
    if KeyLength <= SizeOf(TMD5Digest) then
      Result := True;

    // 否则 KeyLength 太大，满足不了
  end
  else if KeyHash = ckdSha256 then
  begin
    SetLength(PSSHA256, SizeOf(TSHA256Digest) + Length(PS));
    Move(PS[1], PSSHA256[SizeOf(TSHA256Digest) + 1], Length(PS));
    Sha256Dig := SHA256StringA(PS);
    // 密码与 Salt 拼起来的 SHA256 结果（32 Byte）作为第一部分

    Move(Sha256Dig[0], PSSHA256[1], SizeOf(TSHA256Digest));
    Sha256Dig2 := SHA256StringA(PSSHA256);

    Move(Sha256Dig[0], OutKey^, Min(KeyLength, SizeOf(TSHA256Digest)));
    if KeyLength <= SizeOf(TSHA256Digest) then
    begin
      Result := True;
      Exit;
    end;

    KeyLength := KeyLength - SizeOf(TSHA256Digest);
    OutKey := PAnsiChar(Integer(OutKey) + SizeOf(TSHA256Digest));

    Move(Sha256Dig[0], PSMD5[1], SizeOf(TSHA256Digest));
    Md5Dig2 := MD5StringA(PSMD5);
    Move(Md5Dig2[0], OutKey^, Min(KeyLength, SizeOf(TSHA256Digest)));
    if KeyLength <= SizeOf(TSHA256Digest) then
      Result := True;

    // 否则 KeyLength 太大，满足不了
  end;
end;

function CnPBKDF1(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer): AnsiString;
begin

end;

function CnPBKDF2(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer): AnsiString;
begin

end;

end.
