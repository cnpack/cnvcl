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
* 修改记录：2022.06.21 V1.4
*               合并出一个基于字节数组的 CnSM2SM9KDF 函数，避免 AnsiString 在高版本 Delphi 下可能乱码
*           2022.04.26 V1.3
*               修改 LongWord 与 Integer 地址转换以支持 MacOS64
*           2022.01.02 V1.2
*               修正 CnPBKDF2 的一处问题以及在 Unicode 下的兼容性问题
*           2021.11.25 V1.1
*               修正 CnSM2KDF 在 Unicode 下的兼容性问题
*           2020.03.30 V1.0
*               创建单元，从 CnPemUtils 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnMD5, CnSHA1, CnSHA2, CnSM3;

type
  TCnKeyDeriveHash = (ckdMd5, ckdSha256, ckdSha1);
  {* CnGetDeriveKey 中使用的 Hash 途径}

  TCnPBKDF1KeyHash = (cpdfMd2, cpdfMd5, cpdfSha1);
  {* PBKDF1 规定的三种 Hash 途径，其中 MD2 我们不支持}

  TCnPBKDF2KeyHash = (cpdfSha1Hmac, cpdfSha256Hmac);
  {* PBKDF2 规定的两种 Hash 途径}

  ECnKDFException = class(Exception);
  {* KDF 相关异常}

function CnGetDeriveKey(const Password, Salt: AnsiString; OutKey: PAnsiChar; KeyLength: Cardinal;
  KeyHash: TCnKeyDeriveHash = ckdMd5): Boolean;
{* 类似于 Openssl 中的 BytesToKey，用密码和盐与指定的 Hash 算法生成加密 Key，
  目前的限制是 KeyLength 最多支持两轮 Hash，也就是 MD5 32 字节，SHA256 64 字节}

function CnPBKDF1(const Password, Salt: AnsiString; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF1KeyHash = cpdfMd5): AnsiString;
{* Password Based KDF 1 实现，简单的固定 Hash 迭代，只支持 MD5 和 SHA1，参数与返回值均为 AnsiString
   DerivedKeyByteLength 是所需的密钥字节数，长度固定}

function CnPBKDF2(const Password, Salt: AnsiString; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF2KeyHash = cpdfSha1Hmac): AnsiString;
{* Password Based KDF 2 实现，基于 HMAC-SHA1 或 HMAC-SHA256，参数与返回值均为 AnsiString
   DerivedKeyByteLength 是所需的密钥字节数，长度可变，允许超长}

function CnPBKDF1Bytes(const Password, Salt: TBytes; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF1KeyHash = cpdfMd5): TBytes;
{* Password Based KDF 1 实现，简单的固定 Hash 迭代，只支持 MD5 和 SHA1，参数与返回值均为字节数组
   DerivedKeyByteLength 是所需的密钥字节数，长度固定}

function CnPBKDF2Bytes(const Password, Salt: TBytes; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF2KeyHash = cpdfSha1Hmac): TBytes;
{* Password Based KDF 2 实现，基于 HMAC-SHA1 或 HMAC-SHA256，参数与返回值均为字节数组
   DerivedKeyByteLength 是所需的密钥字节数，长度可变，允许超长}

// ============ SM2/SM9 中规定的同一种密钥派生函数的六种封装实现 ===============

function CnSM2KDF(const Data: AnsiString; DerivedKeyByteLength: Integer): AnsiString;
{* SM2 椭圆曲线公钥密码算法中规定的密钥派生函数，DerivedKeyLength 是所需的密钥字节数，
  返回 AnsiString，同时似乎也是没有 SharedInfo 的 ANSI-X9.63-KDF}

function CnSM9KDF(Data: Pointer; DataLen: Integer; DerivedKeyByteLength: Integer): AnsiString;
{* SM9 标识密码算法中规定的密钥派生函数，DerivedKeyLength 是所需的密钥字节数，
  返回 AnsiString，同时似乎也是没有 SharedInfo 的 ANSI-X9.63-KDF}

function CnSM2KDFBytes(const Data: TBytes; DerivedKeyByteLength: Integer): TBytes;
{* 参数为字节数组形式的 SM2 椭圆曲线公钥密码算法中规定的密钥派生函数，DerivedKeyLength 是所需的密钥字节数，返回字节数组}

function CnSM9KDFBytes(Data: Pointer; DataLen: Integer; DerivedKeyByteLength: Integer): TBytes;
{* 参数为内存块形式的 SM9 标识密码算法中规定的密钥派生函数，DerivedKeyLength 是所需的密钥字节数，返回字节数组}

function CnSM2SM9KDF(Data: TBytes; DerivedKeyByteLength: Integer): TBytes; overload;
{* 参数为字节数组形式的 SM2 椭圆曲线公钥密码算法与 SM9 标识密码算法中规定的密钥派生函数，
   DerivedKeyLength 是所需的密钥字节数，返回派生的密钥字节数组}

function CnSM2SM9KDF(Data: Pointer; DataLen: Integer; DerivedKeyByteLength: Integer): TBytes; overload;
{* 参数为内存块形式的 SM2 椭圆曲线公钥密码算法与 SM9 标识密码算法中规定的密钥派生函数，
   DerivedKeyLength 是所需的密钥字节数，返回派生的密钥字节数组}

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
  Md5Dig, Md5Dig2: TCnMD5Digest;
  Sha256Dig, Sha256Dig2: TCnSHA256Digest;
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
    SetLength(PSMD5, SizeOf(TCnMD5Digest) + Length(PS));
    Move(PS[1], PSMD5[SizeOf(TCnMD5Digest) + 1], Length(PS));
    Md5Dig := MD5StringA(PS);
    // 密码与 Salt 拼起来的 MD5 结果（16 Byte）作为第一部分

    Move(Md5Dig[0], OutKey^, Min(KeyLength, SizeOf(TCnMD5Digest)));
    if KeyLength <= SizeOf(TCnMD5Digest) then
    begin
      Result := True;
      Exit;
    end;

    KeyLength := KeyLength - SizeOf(TCnMD5Digest);
    OutKey := PAnsiChar(TCnNativeInt(OutKey) + SizeOf(TCnMD5Digest));

    Move(Md5Dig[0], PSMD5[1], SizeOf(TCnMD5Digest));
    Md5Dig2 := MD5StringA(PSMD5);
    Move(Md5Dig2[0], OutKey^, Min(KeyLength, SizeOf(TCnMD5Digest)));
    if KeyLength <= SizeOf(TCnMD5Digest) then
      Result := True;

    // 否则 KeyLength 太大，满足不了
  end
  else if KeyHash = ckdSha256 then
  begin
    SetLength(PSSHA256, SizeOf(TCnSHA256Digest) + Length(PS));
    Move(PS[1], PSSHA256[SizeOf(TCnSHA256Digest) + 1], Length(PS));
    Sha256Dig := SHA256StringA(PS);
    // 密码与 Salt 拼起来的 SHA256 结果（32 Byte）作为第一部分

    Move(Sha256Dig[0], OutKey^, Min(KeyLength, SizeOf(TCnSHA256Digest)));
    if KeyLength <= SizeOf(TCnSHA256Digest) then
    begin
      Result := True;
      Exit;
    end;

    KeyLength := KeyLength - SizeOf(TCnSHA256Digest);
    OutKey := PAnsiChar(TCnNativeInt(OutKey) + SizeOf(TCnSHA256Digest));

    Move(Sha256Dig[0], PSSHA256[1], SizeOf(TCnSHA256Digest));
    Sha256Dig2 := SHA256StringA(PSSHA256);
    Move(Sha256Dig2[0], OutKey^, Min(KeyLength, SizeOf(TCnSHA256Digest)));
    if KeyLength <= SizeOf(TCnSHA256Digest) then
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
function CnPBKDF1(const Password, Salt: AnsiString; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF1KeyHash): AnsiString;
var
  P, S, Res: TBytes;
begin
  P := AnsiToBytes(Password);
  S := AnsiToBytes(Salt);
  Res := CnPBKDF1Bytes(P, S, Count, DerivedKeyByteLength, KeyHash);
  Result := BytesToAnsi(Res);
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
function CnPBKDF2(const Password, Salt: AnsiString; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF2KeyHash): AnsiString;
var
  P, S, Res: TBytes;
begin
  P := AnsiToBytes(Password);
  S := AnsiToBytes(Salt);
  Res := CnPBKDF2Bytes(P, S, Count, DerivedKeyByteLength, KeyHash);
  Result := BytesToAnsi(Res);
end;

function CnPBKDF1Bytes(const Password, Salt: TBytes; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF1KeyHash = cpdfMd5): TBytes;
var
  I: Integer;
  Md5Dig, TM: TCnMD5Digest;
  Sha1Dig, TS: TCnSHA1Digest;
  Ptr: PAnsiChar;
begin
  Result := nil;
  if (Password = nil) or (Count <= 0) or (DerivedKeyByteLength <= 0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  case KeyHash of
    cpdfMd5:
      begin
        if DerivedKeyByteLength > SizeOf(TCnMD5Digest) then
          raise ECnKDFException.Create(SCnKDFErrorTooLong);

        SetLength(Result, DerivedKeyByteLength);
        Md5Dig := MD5Bytes(ConcatBytes(Password, Salt));  // Got T1
        if Count > 1 then
        begin
          Ptr := PAnsiChar(@TM[0]);
          for I := 2 to Count do
          begin
            TM := Md5Dig;
            Md5Dig := MD5Buffer(Ptr, SizeOf(TCnMD5Digest)); // Got T_c
          end;
        end;

        Move(Md5Dig[0], Result[0], DerivedKeyByteLength);
      end;
    cpdfSha1:
      begin
        if DerivedKeyByteLength > SizeOf(TCnSHA1Digest) then
          raise ECnKDFException.Create(SCnKDFErrorTooLong);

        SetLength(Result, DerivedKeyByteLength);
        Sha1Dig := SHA1Bytes(ConcatBytes(Password, Salt));  // Got T1
        if Count > 1 then
        begin
          Ptr := PAnsiChar(@TS[0]);
          for I := 2 to Count do
          begin
            TS := Sha1Dig;
            Sha1Dig := SHA1Buffer(Ptr, SizeOf(TCnSHA1Digest)); // Got T_c
          end;
        end;

        Move(Sha1Dig[0], Result[0], DerivedKeyByteLength);
      end;
    else
      raise ECnKDFException.Create(SCnKDFHashNOTSupport);
  end;
end;

function CnPBKDF2Bytes(const Password, Salt: TBytes; Count, DerivedKeyByteLength: Integer;
  KeyHash: TCnPBKDF2KeyHash = cpdfSha1Hmac): TBytes;
var
  HLen, D, I, J, K: Integer;
  Sha1Dig1, Sha1Dig, T1: TCnSHA1Digest;
  Sha256Dig1, Sha256Dig, T256: TCnSHA256Digest;
  S, S1, S256, Pad: TBytes;
  PAddr: Pointer;
begin
  Result := nil;
  if (Salt = nil) or (Count <= 0) or (DerivedKeyByteLength <=0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  if (Password = nil) or (Length(Password) = 0) then
    PAddr := nil
  else
    PAddr := @Password[0];

  case KeyHash of
    cpdfSha1Hmac:
      HLen := 20;
    cpdfSha256Hmac:
      HLen := 32;
  else
    raise ECnKDFException.Create(SCnKDFErrorParam);
  end;

  D := (DerivedKeyByteLength div HLen) + 1;
  SetLength(S1, SizeOf(TCnSHA1Digest));
  SetLength(S256, SizeOf(TCnSHA256Digest));

  SetLength(Pad, 4);
  if KeyHash = cpdfSha1Hmac then
  begin
    for I := 1 to D do
    begin
      Pad[0] := I shr 24;
      Pad[1] := I shr 16;
      Pad[2] := I shr 8;
      Pad[3] := I;
      S := ConcatBytes(Salt, Pad);

      SHA1Hmac(PAddr, Length(Password), PAnsiChar(@S[0]), Length(S), Sha1Dig1);
      T1 := Sha1Dig1;

      for J := 2 to Count do
      begin
        SHA1Hmac(PAddr, Length(Password), PAnsiChar(@T1[0]), SizeOf(TCnSHA1Digest), Sha1Dig);
        T1 := Sha1Dig;
        for K := Low(TCnSHA1Digest) to High(TCnSHA1Digest) do
          Sha1Dig1[K] := Sha1Dig1[K] xor T1[K];
      end;

      Move(Sha1Dig1[0], S1[0], Length(S1));
      Result := ConcatBytes(Result, S1);
    end;
    Result := Copy(Result, 0, DerivedKeyByteLength);
  end
  else if KeyHash = cpdfSha256Hmac then
  begin
    for I := 1 to D do
    begin
      Pad[0] := I shr 24;
      Pad[1] := I shr 16;
      Pad[2] := I shr 8;
      Pad[3] := I;
      S := ConcatBytes(Salt, Pad);

      SHA256Hmac(PAddr, Length(Password), PAnsiChar(@S[0]), Length(S), Sha256Dig1);
      T256 := Sha256Dig1;

      for J := 2 to Count do
      begin
        SHA256Hmac(PAddr, Length(Password), PAnsiChar(@T256[0]), SizeOf(TCnSHA256Digest), Sha256Dig);
        T256 := Sha256Dig;
        for K := Low(TCnSHA256Digest) to High(TCnSHA256Digest) do
          Sha256Dig1[K] := Sha256Dig1[K] xor T256[K];
      end;

      Move(Sha256Dig1[0], S256[0], SizeOf(TCnSHA256Digest));
      Result := ConcatBytes(Result, S256);
    end;
    Result := Copy(Result, 0, DerivedKeyByteLength);
  end;
end;

function CnSM2KDF(const Data: AnsiString; DerivedKeyByteLength: Integer): AnsiString;
var
  Res: TBytes;
begin
  if (Data = '') or (DerivedKeyByteLength <= 0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  Res := CnSM2SM9KDF(@Data[1], Length(Data), DerivedKeyByteLength);
  Result := BytesToAnsi(Res);
end;

function CnSM9KDF(Data: Pointer; DataLen: Integer; DerivedKeyByteLength: Integer): AnsiString;
var
  Res: TBytes;
begin
  Res := CnSM2SM9KDF(Data, DataLen, DerivedKeyByteLength);
  Result := BytesToAnsi(Res);
end;

function CnSM2KDFBytes(const Data: TBytes; DerivedKeyByteLength: Integer): TBytes;
begin
  Result := CnSM2SM9KDF(Data, DerivedKeyByteLength);
end;

function CnSM9KDFBytes(Data: Pointer; DataLen: Integer; DerivedKeyByteLength: Integer): TBytes;
begin
  Result := CnSM2SM9KDF(Data, DataLen, DerivedKeyByteLength);
end;

function CnSM2SM9KDF(Data: TBytes; DerivedKeyByteLength: Integer): TBytes;
begin
  if (Data = nil) or (Length(Data) <= 0) or (DerivedKeyByteLength <= 0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  Result := CnSM2SM9KDF(@Data[0], Length(Data), DerivedKeyByteLength);
end;

function CnSM2SM9KDF(Data: Pointer; DataLen: Integer; DerivedKeyByteLength: Integer): TBytes; overload;
var
  DArr: TBytes;
  CT, SCT: Cardinal;
  I, CeilLen: Integer;
  IsInt: Boolean;
  SM3D: TCnSM3Digest;
begin
  Result := nil;
  if (Data = nil) or (DataLen <= 0) or (DerivedKeyByteLength <= 0) then
    raise ECnKDFException.Create(SCnKDFErrorParam);

  DArr := nil;
  CT := 1;

  try
    SetLength(DArr, DataLen + SizeOf(Cardinal));
    Move(Data^, DArr[0], DataLen);

    IsInt := DerivedKeyByteLength mod SizeOf(TCnSM3Digest) = 0;
    CeilLen := (DerivedKeyByteLength + SizeOf(TCnSM3Digest) - 1) div SizeOf(TCnSM3Digest);

    SetLength(Result, DerivedKeyByteLength);
    for I := 1 to CeilLen do
    begin
      SCT := UInt32HostToNetwork(CT);  // 虽然文档中没说，但要倒序一下
      Move(SCT, DArr[DataLen], SizeOf(Cardinal));
      SM3D := SM3(@DArr[0], Length(DArr));

      if (I = CeilLen) and not IsInt then
      begin
        // 是最后一个，不整除 32 时只移动一部分
        Move(SM3D[0], Result[(I - 1) * SizeOf(TCnSM3Digest)], (DerivedKeyByteLength mod SizeOf(TCnSM3Digest)));
      end
      else
        Move(SM3D[0], Result[(I - 1) * SizeOf(TCnSM3Digest)], SizeOf(TCnSM3Digest));

      Inc(CT);
    end;
  finally
    SetLength(DArr, 0);
  end;
end;

end.
