{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnKDF;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：密码生成算法（KDF）单元
* 单元作者：刘啸
* 备    注：基于 RFC2898 的 PBKDF1 与 PBKDF2 实现，但 PBKDF1 不支持 MD2
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2021.11.25 V1.1
*               修正 CnSM2KDF 在 Unicode 下的兼容性问题
*           2020.03.30 V1.0
*               创建单元，从 CnPemUtils 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnMD5, CnSHA1, CnSHA2, CnSM3;

type
  TCnKeyDeriveHash = (ckdMd5, ckdSha256, ckdSha1);

  TCnPBKDF1KeyHash = (cpdfMd2, cpdfMd5, cpdfSha1);

  TCnPBKDF2KeyHash = (cpdfSha1Hmac, cpdfSha256Hmac);

  ECnKDFException = class(Exception);

function CnGetDeriveKey(const Password, Salt: AnsiString; OutKey: PAnsiChar; KeyLength: Cardinal;
  KeyHash: TCnKeyDeriveHash = ckdMd5): Boolean;
{* 类似于 Openssl 中的 BytesToKey，用密码和盐与指定的 Hash 算法生成加密 Key，
  目前的限制是 KeyLength 最多支持两轮 Hash，也就是 MD5 32 字节，SHA256 64 字节}

function CnPBKDF1(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer;
  KeyHash: TCnPBKDF1KeyHash = cpdfMd5): AnsiString;
{* Password Based KDF 1 实现，简单的固定 Hash 迭代，只支持 MD5 和 SHA1，
   DerivedKeyLength 是所需的密钥字节数，长度固定}

function CnPBKDF2(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer;
  KeyHash: TCnPBKDF2KeyHash = cpdfSha1Hmac): AnsiString;
{* Password Based KDF 2 实现，基于 HMAC-SHA1 或 HMAC-SHA256，
   DerivedKeyLength 是所需的密钥字节数，长度可变，允许超长}

function CnSM2KDF(const Data: AnsiString; DerivedKeyLength: Integer): AnsiString;
{* SM2 椭圆曲线公钥密码算法中规定的密钥派生函数，DerivedKeyLength 是所需的密钥字节数，
  均不支持规范中说的非整字节数，行为可能和下面的 CnSM9KDF 等同}

function CnSM9KDF(Data: Pointer; DataLen: Integer; DerivedKeyLength: Integer): AnsiString;
{* SM9 标识密码算法中规定的密钥派生函数，DerivedKeyLength 是所需的密钥字节数，
  均不支持规范中说的非整字节数，行为可能和上面的 CnSM2KDF 等同}

implementation

resourcestring
  SCnKDFErrorTooLong = 'Derived Key Too Long.';
  SCnKDFErrorParam = 'Invalid Parameters.';
  SCnKDFHashNOTSupport = 'Hash Method NOT Support.';

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

  if not (KeyHash in [ckdMd5, ckdSha256]) then
    raise ECnKDFException.Create(SCnKDFHashNOTSupport);

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

    Move(Sha256Dig[0], OutKey^, Min(KeyLength, SizeOf(TSHA256Digest)));
    if KeyLength <= SizeOf(TSHA256Digest) then
    begin
      Result := True;
      Exit;
    end;

    KeyLength := KeyLength - SizeOf(TSHA256Digest);
    OutKey := PAnsiChar(Integer(OutKey) + SizeOf(TSHA256Digest));

    Move(Sha256Dig[0], PSSHA256[1], SizeOf(TSHA256Digest));
    Sha256Dig2 := SHA256StringA(PSSHA256);
    Move(Sha256Dig2[0], OutKey^, Min(KeyLength, SizeOf(TSHA256Digest)));
    if KeyLength <= SizeOf(TSHA256Digest) then
      Result := True;

    // 否则 KeyLength 太大，满足不了
  end;
end;

(*
  T_1 = Hash (P || S) ,
  T_2 = Hash (T_1) ,
  ...
  T_c = Hash (T_{c-1}) ,
  DK = Tc<0..dkLen-1>
*)
function CnPBKDF1(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer;
  KeyHash: TCnPBKDF1KeyHash): AnsiString;
var
  I: Integer;
  Md5Dig, TM: TMD5Digest;
  Sha1Dig, TS: TSHA1Digest;
  Ptr: PAnsiChar;
begin
  if (Password = '') or (Count <= 0) or (DerivedKeyLength <= 0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  case KeyHash of
    cpdfMd5:
      begin
        if DerivedKeyLength > SizeOf(TMD5Digest) then
          raise ECnKDFException.Create(SCnKDFErrorTooLong);

        SetLength(Result, DerivedKeyLength);
        Md5Dig := MD5StringA(Password + Salt);  // Got T1
        if Count > 1 then
        begin
          Ptr := PAnsiChar(@TM[0]);
          for I := 2 to Count do
          begin
            TM := Md5Dig;
            Md5Dig := MD5Buffer(Ptr, SizeOf(TMD5Digest)); // Got T_c
          end;
        end;

        Move(Md5Dig[0], Result[1], DerivedKeyLength);
      end;
    cpdfSha1:
      begin
        if DerivedKeyLength > SizeOf(TSHA1Digest) then
          raise ECnKDFException.Create(SCnKDFErrorTooLong);

        SetLength(Result, DerivedKeyLength);
        Sha1Dig := SHA1StringA(Password + Salt);  // Got T1
        if Count > 1 then
        begin
          Ptr := PAnsiChar(@TS[0]);
          for I := 2 to Count do
          begin
            TS := Sha1Dig;
            Sha1Dig := SHA1Buffer(Ptr, SizeOf(TSHA1Digest)); // Got T_c
          end;
        end;

        Move(Sha1Dig[0], Result[1], DerivedKeyLength);
      end;
    else
      raise ECnKDFException.Create(SCnKDFHashNOTSupport);
  end;
end;

{
  DK = T1 + T2 + ... + Tdklen/hlen
  Ti = F(Password, Salt, c, i)

  F(Password, Salt, c, i) = U1 ^ U2 ^ ... ^ Uc

  U1 = PRF(Password, Salt + INT_32_BE(i))
  U2 = PRF(Password, U1)
  ...
  Uc = PRF(Password, Uc-1)
}
function CnPBKDF2(const Password, Salt: AnsiString; Count, DerivedKeyLength: Integer;
  KeyHash: TCnPBKDF2KeyHash): AnsiString;
var
  HLen, D, I, J, K: Integer;
  Sha1Dig1, Sha1Dig, T1: TSHA1Digest;
  Sha256Dig1, Sha256Dig, T256: TSHA256Digest;
  S, S1, S256: AnsiString;
begin
  Result := '';
  if (Password = '') or (Salt = '') or (Count <= 0) or (DerivedKeyLength <=0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  case KeyHash of
    cpdfSha1Hmac:
      HLen := 20;
    cpdfSha256Hmac:
      HLen := 32;
  else
    raise ECnKDFException.Create(SCnKDFErrorParam);
  end;

  D := (DerivedKeyLength div HLen) + 1;
  SetLength(S1, SizeOf(TSHA1Digest));
  SetLength(S256, SizeOf(TSHA256Digest));

  if KeyHash = cpdfSha1Hmac then
  begin
    for I := 1 to D do
    begin
      S := Salt + Chr(I shr 24) + Chr(I shr 16) + Chr(I shr 8) + Chr(I);
      SHA1Hmac(PAnsiChar(Password), Length(Password), PAnsiChar(S), Length(S), Sha1Dig1);
      T1 := Sha1Dig1;

      for J := 2 to Count do
      begin
        SHA1Hmac(PAnsiChar(Password), Length(Password), PAnsiChar(@T1[0]), SizeOf(TSHA1Digest), Sha1Dig);
        T1 := Sha1Dig;
        for K := Low(TSHA1Digest) to High(TSHA1Digest) do
          Sha1Dig1[K] := Sha1Dig1[K] xor T1[K];
      end;

      Move(Sha1Dig1[0], S1[1], SizeOf(TSHA1Digest));
      Result := Result + S1;
    end;
    Result := Copy(Result, 1, DerivedKeyLength);
  end
  else if KeyHash = cpdfSha256Hmac then
  begin
    for I := 1 to D do
    begin
      S := Salt + Chr(I shr 24) + Chr(I shr 16) + Chr(I shr 8) + Chr(I);
      SHA256Hmac(PAnsiChar(Password), Length(Password), PAnsiChar(S), Length(S), Sha256Dig1);
      T256 := Sha256Dig1;

      for J := 2 to Count do
      begin
        SHA256Hmac(PAnsiChar(Password), Length(Password), PAnsiChar(@T256[0]), SizeOf(TSHA256Digest), Sha256Dig);
        T256 := Sha256Dig;
        for K := Low(TSHA256Digest) to High(TSHA256Digest) do
          Sha256Dig1[K] := Sha256Dig1[K] xor T1[K];
      end;

      Move(Sha256Dig1[0], S256[1], SizeOf(TSHA256Digest));
      Result := Result + S256;
    end;
    Result := Copy(Result, 1, DerivedKeyLength);
  end;
end;

function CnSM2KDF(const Data: AnsiString; DerivedKeyLength: Integer): AnsiString;
var
  S, SDig: AnsiString;
  I, D: Integer;
  Dig: TSM3Digest;
begin
  Result := '';
  if (Data = '') or (DerivedKeyLength <= 0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  SetLength(SDig, SizeOf(TSM3Digest));
  D := DerivedKeyLength div SizeOf(TSM3Digest) + 1;
  for I := 1 to D do
  begin
{$IFDEF UNICODE}
    S := Data + AnsiChar(I shr 24) + AnsiChar(I shr 16) + AnsiChar(I shr 8) + AnsiChar(I);
{$ELSE}
    S := Data + Chr(I shr 24) + Chr(I shr 16) + Chr(I shr 8) + Chr(I);
{$ENDIF}
    Dig := SM3StringA(S);
    Move(Dig[0], SDig[1], SizeOf(TSM3Digest));
    Result := Result + SDig;
  end;
  Result := Copy(Result, 1, DerivedKeyLength);
end;

function CnSM9KDF(Data: Pointer; DataLen: Integer; DerivedKeyLength: Integer): AnsiString;
var
  DArr: array of Byte;
  CT, SCT: LongWord;
  I, CeilLen: Integer;
  IsInt: Boolean;
  SM3D: TSM3Digest;

  function SwapLongWord(Value: LongWord): LongWord;
  begin
    Result := ((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8)
      or ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24);
  end;

begin
  Result := '';
  if (Data = nil) or (DataLen <= 0) or (DerivedKeyLength <= 0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  DArr := nil;
  CT := 1;

  try
    SetLength(DArr, DataLen + SizeOf(LongWord));
    Move(Data^, DArr[0], DataLen);

    IsInt := DerivedKeyLength mod SizeOf(TSM3Digest) = 0;
    CeilLen := (DerivedKeyLength + SizeOf(TSM3Digest) - 1) div SizeOf(TSM3Digest);

    SetLength(Result, DerivedKeyLength);
    for I := 1 to CeilLen do
    begin
      SCT := SwapLongWord(CT);  // 虽然文档中没说，但要倒序一下
      Move(SCT, DArr[DataLen], SizeOf(LongWord));
      SM3D := SM3(@DArr[0], Length(DArr));

      if (I = CeilLen) and not IsInt then
      begin
        // 是最后一个，不整除 32 时只移动一部分
        Move(SM3D[0], Result[(I - 1) * SizeOf(TSM3Digest) + 1], (DerivedKeyLength mod SizeOf(TSM3Digest)));
      end
      else
        Move(SM3D[0], Result[(I - 1) * SizeOf(TSM3Digest) + 1], SizeOf(TSM3Digest));

      Inc(CT);
    end;
  finally
    SetLength(DArr, 0);
  end;
end;

end.
