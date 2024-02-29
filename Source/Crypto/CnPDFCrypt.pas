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

unit CnPDFCrypt;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：PDF 简易解析生成单元
* 单元作者：刘啸
* 备    注：PDF 加解密机制的实现单元，从 CnPDF.pas 中独立出来，仅支持 Revision 2 3 4
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.02.29 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

uses
  SysUtils, Classes, CnNative;

type
  ECnPDFCryptException = class(Exception);

function CnPDFCalcEncryptKey(const UserPass: AnsiString; Revision: Integer; OwnerCipher: TBytes;
  Permission: Cardinal; ID: TBytes; KeyBitLength: Integer): TBytes;
{* 根据用户密码与 O 值等计算加密 Key，供加解密字符串与流内容}

function CnPDFCalcUserCipher(const UserPass: AnsiString; Revision: Integer;
  OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes; KeyBitLength: Integer): TBytes;
{* 根据用户密码等内容计算 U 值，内部包括计算加密 Key}

function CnPDFCalcOwnerCipher(const OwnerPass, UserPass: AnsiString;
  Revision, KeyBitLength: Integer): TBytes;
{* 根据权限密码与用户密码计算 O 值}

implementation

uses
  CnRandom, CnMD5, CnRC4;

const
  CN_PDF_ENCRYPT_SIZE = 32;       // 32 字节对齐的 PDF 加密模式

type
  TCnPDFPaddingKey = array[0..CN_PDF_ENCRYPT_SIZE - 1] of Byte;

const
  CN_PDF_ENCRYPT_PADDING: TCnPDFPaddingKey = (
    $28, $BF, $4E, $5E, $4E, $75, $8A, $41, $64, $00, $4E, $56, $FF, $FA, $01, $08,
    $2E, $2E, $00, $B6, $D0, $68, $3E, $80, $2F, $0C, $A9, $FE, $64, $53, $69, $7A
  );

resourcestring
  SCnErrorPDFKeyLength = 'Invalid Key Length';

function PaddingKey(const Password: AnsiString): TCnPDFPaddingKey;
var
  L: Integer;
begin
  L := Length(Password);
  if L > 0 then
  begin
    L := MoveMost(Password[1], Result[0], L, SizeOf(TCnPDFPaddingKey));
    if L < SizeOf(TCnPDFPaddingKey) then
      Move(CN_PDF_ENCRYPT_PADDING[0], Result[L], SizeOf(TCnPDFPaddingKey) - L);
  end
  else
    Move(CN_PDF_ENCRYPT_PADDING[0], Result[0], SizeOf(TCnPDFPaddingKey));
end;

function CnPDFCalcEncryptKey(const UserPass: AnsiString;
  Revision: Integer; OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes;
  KeyBitLength: Integer): TBytes;
var
  I, KL: Integer;
  PK: TCnPDFPaddingKey;
  Ctx: TCnMD5Context;
  Dig: TCnMD5Digest;
  P: Cardinal;
begin
  KL := KeyBitLength div 8;
  if (KL <= 0) or (KL > 16) then // 最多 16 字节
    raise ECnPDFCryptException.Create(SCnErrorPDFKeyLength);

  PK := PaddingKey(UserPass);

  MD5Init(Ctx);
  MD5Update(Ctx, @PK[0], SizeOf(TCnPDFPaddingKey));
  MD5Update(Ctx, @OwnerCipher[0], Length(OwnerCipher));

  P := UInt32ToLittleEndian(Permission); // 强制小端
  MD5Update(Ctx, @P, SizeOf(P));

  MD5Update(Ctx, @ID[0], Length(ID));

  if Revision >= 4 then // 只处理 Metadata 不加密的情况
  begin
    P := $FFFFFFFF;
    MD5Update(Ctx, @P, SizeOf(P));
  end;

  MD5Final(Ctx, Dig);

  if Revision >= 3 then  // 再五十轮 MD5
  begin
    for I := 1 to 50 do
      Dig := MD5(@Dig[0], KL);

    SetLength(Result, 16);
  end
  else
    SetLength(Result, 5);

  Move(Dig[0], Result[0], Length(Result));
end;

function CnPDFCalcOwnerCipher(const OwnerPass, UserPass: AnsiString;
  Revision, KeyBitLength: Integer): TBytes;
var
  I, J, KL: Integer;
  OPK, UPK, XK: TCnPDFPaddingKey;
  Dig: TCnMD5Digest;
  RK: TBytes;
begin
  if OwnerPass <> '' then
    OPK := PaddingKey(OwnerPass)
  else
    OPK := PaddingKey(UserPass);

  // 特定对齐至 32 字节并做一次 MD5
  Dig := MD5(@OPK[0], SizeOf(TCnPDFPaddingKey));

  // 对 MD5 结果再做 50 次 MD5
  for I := 1 to 50 do
    Dig := MD5(@Dig[0], SizeOf(TCnMD5Digest));

  if Revision <= 2 then
    KL := 5
  else
    KL := KeyBitLength div 8;

  if (KL <= 0) or (KL > 16) then // 最多 16 字节
    raise ECnPDFCryptException.Create(SCnErrorPDFKeyLength);

  SetLength(RK, KL);
  MoveMost(Dig[0], RK[0], KL, SizeOf(TCnMD5Digest));
  // RK 是从多次 MD5 结果中取出的指定长度最多 16 的字节作为 RC4 密钥

  // 用户密码对齐特定 32 字节到 UPK 中
  UPK := PaddingKey(UserPass);

  // RC4 用最多 16 字节的 RK 加密 32 字节的 UPK，结果放 UPK 里
  RC4Encrypt(@RK[0], KL, @UPK[0], @UPK[0], SizeOf(TCnPDFPaddingKey));

  if Revision >= 3 then
  begin
    for I := 1 to 19 do
    begin
      for J := 0 to KL - 1 do
        XK[J] := RK[J] xor I;

      RC4Encrypt(@XK[0], KL, @UPK[0], @UPK[0], SizeOf(TCnPDFPaddingKey));
    end;
  end;

  SetLength(Result, SizeOf(TCnPDFPaddingKey));
  Move(UPK[0], Result[0], SizeOf(TCnPDFPaddingKey))
end;

function CnPDFCalcUserCipher(const UserPass: AnsiString; Revision: Integer;
  OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes; KeyBitLength: Integer): TBytes;
var
  I, J, KL: Integer;
  Key: TBytes;
  Ctx: TCnMD5Context;
  Dig: TCnMD5Digest;
  XK: TCnPDFPaddingKey;
begin
  Key := CnPDFCalcEncryptKey(UserPass, Revision, OwnerCipher, Permission, ID, KeyBitLength);

  if Revision = 2 then
  begin
    SetLength(Result, SizeOf(TCnPDFPaddingKey));
    RC4Encrypt(@Key[0], Length(Key), @CN_PDF_ENCRYPT_PADDING[0], @Result[0], SizeOf(TCnPDFPaddingKey));
  end
  else if Revision in [3, 4] then
  begin
    MD5Init(Ctx);
    MD5Update(Ctx, @CN_PDF_ENCRYPT_PADDING[0], SizeOf(TCnPDFPaddingKey));
    MD5Update(Ctx, @ID[0], Length(ID));
    MD5Final(Ctx, Dig);

    RC4Encrypt(@Key[0], Length(Key), @Dig[0], @Dig[0], SizeOf(TCnMD5Digest));

    KL := KeyBitLength div 8;
    if (KL <= 0) or (KL > 16) then // 最多 16 字节
      raise ECnPDFCryptException.Create(SCnErrorPDFKeyLength);

    for I := 1 to 19 do
    begin
      for J := 0 to KL - 1 do
        XK[J] := Key[J] xor I;

      RC4Encrypt(@XK[0], KL, @Dig[0], @Dig[0], SizeOf(TCnPDFPaddingKey));
    end;

    SetLength(Result, SizeOf(TCnPDFPaddingKey));
    Move(Dig[0], Result[0], SizeOf(TCnMD5Digest));

    // 计算出前 16 字节后，后 16 字节用随机数填充
    CnRandomFillBytes(@Result[SizeOf(TCnMD5Digest)], SizeOf(TCnMD5Digest));
  end;
end;

end.
