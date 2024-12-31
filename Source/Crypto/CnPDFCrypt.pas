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

unit CnPDFCrypt;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：PDF 加解密机制实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 PDF 文件的加解密机制，从 CnPDF.pas 中独立出来。
*
*           PDF 文件的加解密相关概念：
*           Version 表示加密机制，0 无，1 为 40 位 RC4、2/3 为 40 到 128 位 RC4、4 看 Security Handler 里的 Crypt Filter
*           Revision 用来指示 Security Handler 如何处理权限，2 无、3 有、4 不知道，并对各 Key 的运算有影响
*
*    需支持 Version  Revision     加密算法                Key 指定的 Length                    PDF 规范版本
*           0        *            无
*        *  1        2            RC4                     无需指定，固定 40 位                 1.1
*        *  2        2            RC4                     可大于 40 位，推荐 128，无权限控制   1.4  此处不用
*        *  2        3            RC4                     可大于 40 位，推荐 128，有权限控制   1.4
*           3        3            （未公开，不使用）                                           1.4
*        *  4        4            依赖于 CF 等                                                 1.5
*                       CFM V2    RC4                     128，在 CFM 中
*                       CFM AESV2 AES128/CBC/PKCS5        128，在 CFM 中                       1.6
*           5        5  CFM AESV3 AES256/CBC/PKCS5        256，在 CFM 中                       1.7
*
*           加解密测试用例：
*               非 Unicode 编译器读 40RC4 加密通过       非 Unicode 编译器写 40RC4 加密通过
*               非 Unicode 编译器读 128RC4 加密通过      非 Unicode 编译器写 128RC4 加密通过
*               非 Unicode 编译器读 128AES 加密通过      非 Unicode 编译器写 128AES 加密通过
*               Unicode 编译器读 40RC4 加密通过          Unicode 编译器写 40RC4 加密通过
*               Unicode 编译器读 128RC4 加密通过         Unicode 编译器写 128RC4 加密通过
*               Unicode 编译器读 128AES 加密通过         Unicode 编译器写 128AES 加密通过
*
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.03.02 V1.2
*               实现 128AES 的加解密
*           2024.03.02 V1.1
*               实现 128RC4 的加解密
*           2024.02.29 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative;

type
  ECnPDFCryptException = class(Exception);
  {* PDF 加解密的异常}

  TCnPDFEncryptionMethod = (cpemNotSupport, cpem40RC4, cpem128RC4, cpem128AES); //, cpem256AES);
  {* 支持的几种加密模式，其中 40RC4 目前仍有问题}

  TCnPDFDataCryptor = class
  {* 加解密实现类，设置一次原始 Key，可反复对多个对象的字节流实施加解密}
  private
    FEncryptionMethod: TCnPDFEncryptionMethod;
    FKeyByteLength: Integer; // 加密算法要求的密钥字节长度，并非传入的 Key 字节数组长度
    FKey: TBytes;            // 传入的 Key 经过追加内容后存放的地方，供 MD5 用
    FLength: Integer;        // 传入的 Key 的原始长度
  protected
    procedure MakeKey(ID: Cardinal; Gen: Cardinal);
    {* 根据 ID 与 Gen 拼凑一个内部 Key。

       参数：
         ID: Cardinal                     - 待拼凑的 ID
         Gen: Cardinal                    - 待拼凑的 Gen

       返回值：（无）
    }
  public
    constructor Create(EncryptionMethod: TCnPDFEncryptionMethod;
      AKey: TBytes; KeyBitLength: Integer); virtual;
    {* 构造函数。

       参数：
         EncryptionMethod: TCnPDFEncryptionMethod         - 加密类型
         AKey: TBytes                                     - 密钥
         KeyBitLength: Integer                            - 密钥的位数，注意并非密钥字节或位长度

       返回值：                                           - 对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Encrypt(var Data: TBytes; ID: Cardinal; Generation: Cardinal);
    {* 根据指定 ID 和 Generation 加密字节数组 Data，结果重新放入 Data 中

       参数：
         var Data: TBytes                 - 待加密的字节数组，密文重新放入该字节数组
         ID: Cardinal                     - 指定 ID
         Generation: Cardinal             - 指定 Generation

       返回值：（无）
    }

    procedure Decrypt(var Data: TBytes; ID: Cardinal; Generation: Cardinal);
    {* 根据指定 ID 和 Generation 解密字节数组 Data，结果重新放入 Data 中

       参数：
         var Data: TBytes                 - 待解密的字节数组，明文重新放入该字节数组
         ID: Cardinal                     - 指定 ID
         Generation: Cardinal             - 指定 Generation

       返回值：（无）
    }
  end;

function CnPDFFindEncryptionMethod(Version: Integer; Revision: Integer;
  KeyBitLength: Integer; const CFMValue: string = ''): TCnPDFEncryptionMethod;
{* 根据 PDF 文件中 Encrypt 字段的 V R Length 值及 CTM 字段值判断使用哪种加密算法。

   参数：
     Version: Integer                     - PDF 版本号
     Revision: Integer                    - PDF 修订版本号
     KeyBitLength: Integer                - PDF 密钥位数
     const CFMValue: string               - CTM 字段值

   返回值：TCnPDFEncryptionMethod         - 返回的加密算法类型
}

function CnPDFCalcEncryptKey(const UserPass: AnsiString; Version: Integer;
  Revision: Integer; OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes;
  KeyBitLength: Integer): TBytes;
{* 生成 PDF 时调用，根据用户密码与 O 值等计算加密 Key，供加解密字符串与流内容。

   参数：
     const UserPass: AnsiString           - 用户密码
     Version: Integer                     - 加密版本号，只支持 1  2  3  4
     Revision: Integer                    - 加密修订版本号，只支持 2  3  4
     OwnerCipher: TBytes                  - PDF 文件中 Encrypt 节点的 O 字段值，一般 32 字节
     Permission: Cardinal                 - PDF 文件中 Encrypt 节点的 P 字段值，如是负值要强制转换成无符号 32 位
     ID: TBytes                           - PDF 文件中 Trailer 部分的 ID 数组的第一个字符串值
     KeyBitLength: Integer                - PDF 文件中 Encrypt 节点的 Length 字段值，一般是 128，实际除以 8 得到字节数

   返回值：TBytes                         - 返回用来加解密的密钥字节数组
}

function CnPDFCalcUserCipher(const UserPass: AnsiString; Version: Integer; Revision: Integer;
  OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes; KeyBitLength: Integer): TBytes;
{* 生成 PDF 时调用，根据用户密码等内容计算 U 值，内部包括计算加密 Key，可供与文档中的 U 值对比以确定是否是正确的用户密码。

   参数：
     const UserPass: AnsiString           - 用户密码
     Version: Integer                     - 加密版本号，只支持 1  2  3  4
     Revision: Integer                    - 加密修订版本号，只支持 2  3  4
     OwnerCipher: TBytes                  - PDF 文件中 Encrypt 节点的 O 字段值，一般 32 字节
     Permission: Cardinal                 - PDF 文件中 Encrypt 节点的 P 字段值，如是负值要强制转换成无符号 32 位
     ID: TBytes                           - PDF 文件中 Trailer 部分的 ID 数组的第一个字符串值
     KeyBitLength: Integer                - PDF 文件中 Encrypt 节点的 Length 字段值，一般是 128，实际除以 8 得到字节数

   返回值：TBytes                         - 返回可放置至 PDF 文件中 Encrypt 节点的 U 字段值，或与原始 U 值比较以确定用户密码是否正确
}

function CnPDFCalcOwnerCipher(const OwnerPass: AnsiString; const UserPass: AnsiString;
  Version: Integer; Revision: Integer; KeyBitLength: Integer): TBytes;
{* 生成 PDF 时调用，根据权限密码与用户密码计算 O 值。

   参数：
     const OwnerPass: AnsiString          - 权限密码
     const UserPass: AnsiString           - 用户密码
     Version: Integer                     - 加密版本号，只支持 1  2  3  4
     Revision: Integer                    - 加密修订版本号，只支持 2  3  4
     KeyBitLength: Integer                - PDF 文件中 Encrypt 节点的 Length 字段值，一般是 128，实际除以 8 得到字节数

   返回值：TBytes                         - 返回可放置至 PDF 文件中 Encrypt 节点的 O 字段值
}

function CnPDFCheckUserPassword(const UserPass: AnsiString; Version: Integer;
  Revision: Integer; OwnerCipher: TBytes; UserCipher: TBytes; Permission: Cardinal;
  ID: TBytes; KeyBitLength: Integer): TBytes;
{* 解析 PDF 时调用，检查用户输入的 UserPass 是否是合法的用户密码，检查通过返回加密密钥，否则返回 nil。

   参数：
     const UserPass: AnsiString           - 用户密码
     Version: Integer                     - 加密版本号，只支持 1  2  3  4
     Revision: Integer                    - 加密修订版本号，只支持 2  3  4
     OwnerCipher: TBytes                  - PDF 文件中 Encrypt 节点的 O 字段值，一般 32 字节
     UserCipher: TBytes                   - PDF 文件中 Encrypt 节点的 U 字段值，一般 32 字节
     Permission: Cardinal                 - PDF 文件中 Encrypt 节点的 P 字段值，如是负值要强制转换成无符号 32 位
     ID: TBytes                           - PDF 文件中 Trailer 部分的 ID 数组的第一个字符串值
     KeyBitLength: Integer                - PDF 文件中 Encrypt 节点的 Length 字段值，一般是 128，实际除以 8 得到字节数

   返回值：TBytes                         - 加解密的密钥字节数组
}

function CnPDFCheckOwnerPassword(const OwnerPass: AnsiString; Version: Integer;
  Revision: Integer; OwnerCipher: TBytes; UserCipher: TBytes; Permission: Cardinal;
  ID: TBytes; KeyBitLength: Integer): TBytes;
{* 解析 PDF 时调用，检查用户输入的 OwnerPass 是否是合法的权限密码，检查通过返回加密密钥，否则返回 nil

   参数：
     const OwnerPass: AnsiString          - 权限密码
     Version: Integer                     - 加密版本号，只支持 1  2  3  4
     Revision: Integer                    - 加密修订版本号，只支持 2  3  4
     OwnerCipher: TBytes                  - PDF 文件中 Encrypt 节点的 O 字段值，一般 32 字节
     UserCipher: TBytes                   - PDF 文件中 Encrypt 节点的 U 字段值，一般 32 字节
     Permission: Cardinal                 - PDF 文件中 Encrypt 节点的 P 字段值，如是负值要强制转换成无符号 32 位
     ID: TBytes                           - PDF 文件中 Trailer 部分的 ID 数组的第一个字符串值
     KeyBitLength: Integer                - PDF 文件中 Encrypt 节点的 Length 字段值，一般是 128，实际除以 8 得到字节数

   返回值：TBytes                         - 返回加解密的密钥字节数组
}

implementation

uses
  CnRandom, CnMD5, CnRC4, CnAES, CnPemUtils;

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
  SCnErrorPDFDataLength = 'Invalid Data Length';
  SCnErrorPDFEncryptParams = 'Invalid Encrypt Params';

function CnPDFFindEncryptionMethod(Version, Revision, KeyBitLength: Integer;
  const CFMValue: string = ''): TCnPDFEncryptionMethod;
begin
  Result := cpemNotSupport;
  case Version of
    1:
      if Revision = 2 then
        Result := cpem40RC4;
    2:
      if Revision in [2, 3] then
        Result := cpem128RC4;
    4:
      if Revision = 4 then
      begin
        if CFMValue = 'V2' then
          Result := cpem128RC4
        else if CFMValue = 'AESV2' then
          Result := cpem128AES;
      end;
  end;
end;

// 将明文密码加特定数据补齐成 32 字节内容
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

// 将补齐内容解开得到明文密码
function UnPaddingKey(var PaddingKey: TCnPDFPaddingKey): AnsiString;
var
  I: Integer;
begin
  for I := 0 to SizeOf(TCnPDFPaddingKey) - 1 do
  begin
    if CompareMem(@PaddingKey[I], @CN_PDF_ENCRYPT_PADDING[0], SizeOf(TCnPDFPaddingKey) - I) then
    begin
      SetLength(Result, I);
      if Length(Result) > 0 then
        Move(PaddingKey[0], Result[1], I);

      Exit;
    end;
  end;

  // 都没搜着，使用原始的
  SetLength(Result, SizeOf(TCnPDFPaddingKey));
  Move(PaddingKey[0], Result[1], SizeOf(TCnPDFPaddingKey));
end;

function CnPDFCalcEncryptKey(const UserPass: AnsiString; Version,
  Revision: Integer; OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes;
  KeyBitLength: Integer): TBytes;
var
  I, KL: Integer;
  PK: TCnPDFPaddingKey;
  Ctx: TCnMD5Context;
  Dig: TCnMD5Digest;
  P: Cardinal;
begin
  if Version <= 1 then
    KL := 5
  else
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

// 只处理 Metadata 加密的情况，因而此处不按规范加 FFFFFFFF
//  if Revision >= 4 then
//  begin
//    P := $FFFFFFFF;
//    MD5Update(Ctx, @P, SizeOf(P));
//  end;

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

{* 40 位 RC4 情况下生成与检验权限密码时计算密钥的简易实现}
procedure CalcOwnerKey40(const OwnerPass: AnsiString; Key: PAnsiChar);
var
  Dig: TCnMD5Digest;
  OPK: TCnPDFPaddingKey;
begin
  OPK := PaddingKey(OwnerPass);

  // 特定对齐至 32 字节并做一次 MD5
  Dig := MD5(@OPK[0], SizeOf(TCnPDFPaddingKey));

  // 直接复制前 5 字节
  Move(Dig[0], Key^, 5);
end;

{* 生成与检验权限密码时都要计算密钥，抽出来作为公用。40 位 RC4 时可能有问题需要用 CalcOwnerKey40 代替}
function CalcOwnerKey(const OwnerPass: AnsiString; Version, Revision, KeyBitLength: Integer): TBytes;
var
  I, KL: Integer;
  Dig: TCnMD5Digest;
  OPK: TCnPDFPaddingKey;
begin
  OPK := PaddingKey(OwnerPass);

  // 特定对齐至 32 字节并做一次 MD5
  Dig := MD5(@OPK[0], SizeOf(TCnPDFPaddingKey));

  if Revision <= 2 then
    KL := 5
  else
  begin
    KL := KeyBitLength div 8;

    // 对 MD5 结果再做 50 次 MD5
    for I := 1 to 50 do
      Dig := MD5(@Dig[0], SizeOf(TCnMD5Digest));
  end;

  if (KL <= 0) or (KL > 16) then // 最多 16 字节
    raise ECnPDFCryptException.Create(SCnErrorPDFKeyLength);

  SetLength(Result, KL);
  MoveMost(Dig[0], Result[0], KL, SizeOf(TCnMD5Digest));
  // Result 是从多次 MD5 结果中取出的指定长度最多 16 的字节作为 RC4 密钥
end;

function CnPDFCalcOwnerCipher(const OwnerPass, UserPass: AnsiString;
  Version, Revision, KeyBitLength: Integer): TBytes;
var
  I, J: Integer;
  UPK, XK: TCnPDFPaddingKey;
  RK: TBytes;
begin
  if (Version = 1) and (Revision = 2) then // 40 位 RC4 算法采用简化版
  begin
    SetLength(RK, 5);
    CalcOwnerKey40(OwnerPass, @RK[0]);
  end
  else
    RK := CalcOwnerKey(OwnerPass, Version, Revision, KeyBitLength);

  // 用户密码对齐特定 32 字节到 UPK 中
  UPK := PaddingKey(UserPass);

  // RC4 用最多 16 字节的 RK 加密 32 字节的 UPK，结果放 UPK 里
  RC4Encrypt(@RK[0], Length(RK), @UPK[0], @UPK[0], SizeOf(TCnPDFPaddingKey));

  if Revision >= 3 then
  begin
    for I := 1 to 19 do
    begin
      for J := 0 to Length(RK) - 1 do
        XK[J] := RK[J] xor I;

      RC4Encrypt(@XK[0], Length(RK), @UPK[0], @UPK[0], SizeOf(TCnPDFPaddingKey));
    end;
  end;

  SetLength(Result, SizeOf(TCnPDFPaddingKey));
  Move(UPK[0], Result[0], SizeOf(TCnPDFPaddingKey));
end;

function CnPDFCalcUserCipher(const UserPass: AnsiString; Version, Revision: Integer;
  OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes; KeyBitLength: Integer): TBytes;
var
  I, J, KL: Integer;
  Key: TBytes;
  Ctx: TCnMD5Context;
  Dig: TCnMD5Digest;
  XK: TCnPDFPaddingKey;
begin
  Key := CnPDFCalcEncryptKey(UserPass, Version, Revision, OwnerCipher, Permission, ID, KeyBitLength);

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

function CnPDFCheckUserPassword(const UserPass: AnsiString; Version, Revision: Integer;
  OwnerCipher, UserCipher: TBytes; Permission: Cardinal; ID: TBytes;
  KeyBitLength: Integer): TBytes;
var
  N: TBytes;
begin
  if (Length(OwnerCipher) = 0) or (Length(UserCipher) = 0) or (Length(ID) = 0) then
    raise ECnPDFCryptException.Create(SCnErrorPDFEncryptParams);

  N := CnPDFCalcUserCipher(UserPass, Version, Revision, OwnerCipher, Permission, ID, KeyBitLength);
  if CompareBytes(N, UserCipher, 16) then
    Result := CnPDFCalcEncryptKey(UserPass, Version, Revision, OwnerCipher, Permission, ID, KeyBitLength)
  else
    Result := nil;
end;

function CnPDFCheckOwnerPassword(const OwnerPass: AnsiString; Version, Revision: Integer;
  OwnerCipher, UserCipher: TBytes; Permission: Cardinal; ID: TBytes;
  KeyBitLength: Integer): TBytes;
var
  I, J: Integer;
  RK, OC, XK: TBytes;
  OCP: TCnPDFPaddingKey;
  UP: AnsiString;
begin
  if (Length(OwnerCipher) = 0) or (Length(UserCipher) = 0) or (Length(ID) = 0) then
    raise ECnPDFCryptException.Create(SCnErrorPDFEncryptParams);

  if (Version = 1) and (Revision = 2) then // 40 位 RC4 算法采用简化版
  begin
    SetLength(RK, 5);
    CalcOwnerKey40(OwnerPass, @RK[0]);
  end
  else
    RK := CalcOwnerKey(OwnerPass, Version, Revision, KeyBitLength);

  if Revision = 2 then
  begin
    SetLength(OC, Length(OwnerCipher));
    RC4Decrypt(@RK[0], Length(RK), @OwnerCipher[0], @OC[0], Length(OwnerCipher));
  end
  else if Revision >= 3 then
  begin
    SetLength(OC, Length(OwnerCipher));
    Move(OwnerCipher[0], OC[0], Length(OwnerCipher));

    SetLength(XK, Length(RK));
    for I := 19 downto 0 do
    begin
      for J := Length(RK) - 1 downto 0 do
        XK[J] := RK[J] xor I;

      RC4Decrypt(@XK[0], Length(XK), @OC[0], @OC[0], Length(OC));
    end;
  end;

  // OC 是解密出来的对齐的 Password，拿去验证
  MoveMost(OC[0], OCP[0], Length(OC), SizeOf(TCnPDFPaddingKey));
  UP := UnPaddingKey(OCP);

  // 验证通过则返回密钥，不通过则返回 nil
  Result := CnPDFCheckUserPassword(UP, Version, Revision, OwnerCipher, UserCipher,
    Permission, ID, KeyBitLength);
end;

{ TCnPDFDataCryptor }

constructor TCnPDFDataCryptor.Create(EncryptionMethod: TCnPDFEncryptionMethod;
  AKey: TBytes; KeyBitLength: Integer);
var
  L: Integer;
begin
  inherited Create;
  FEncryptionMethod := EncryptionMethod;

  FLength := Length(AKey);

  if FEncryptionMethod = cpem40RC4 then
    FKeyByteLength := 5
  else
    FKeyByteLength := KeyBitLength div 8;

  if FEncryptionMethod in [cpem40RC4, cpem128RC4] then
    L := FLength + 5
  else
    L := FLength + 9; // AES 额外加个四字节盐

  SetLength(FKey, L);
  if Length(AKey) > 0 then
    Move(AKey[0], FKey[0], Length(AKey)); // 头部先放原始 Key

  if not (FEncryptionMethod in [cpem40RC4, cpem128RC4]) then // AES 额外加个四字节盐
  begin
    FKey[L - 4] := $73;
    FKey[L - 3] := $41;
    FKey[L - 2] := $6C;
    FKey[L - 1] := $54;
  end;
end;

procedure TCnPDFDataCryptor.Decrypt(var Data: TBytes; ID, Generation: Cardinal);
var
  L: Integer;
  Res, Iv, K: TBytes;
  Dig: TCnMD5Digest;
begin
  if Length(Data) = 0 then
    Exit;

  MakeKey(ID, Generation);
  Dig := MD5(@FKey[0], Length(FKey));

  if FEncryptionMethod = cpem40RC4 then // RC4 解密，密钥长度分别是 5+5 和 16
    RC4Encrypt(@Dig[0], FKeyByteLength + 5, @Data[0], @Data[0], Length(Data))
  else if FEncryptionMethod = cpem128RC4 then
    RC4Encrypt(@Dig[0], SizeOf(TCnMD5Digest), @Data[0], @Data[0], Length(Data))
  else // AES
  begin
    // 前 16 字节抽出来做 Iv
    if Length(Data) <= CN_AES_BLOCKSIZE then
      raise ECnPDFCryptException.Create(SCnErrorPDFDataLength);

    SetLength(Iv, CN_AES_BLOCKSIZE);
    Move(Data[0], Iv[0], CN_AES_BLOCKSIZE);

    // 16 字节后的是密文
    SetLength(Res, Length(Data) - CN_AES_BLOCKSIZE);
    Move(Data[CN_AES_BLOCKSIZE], Res[0], Length(Res));

    L := FKeyByteLength + 5;
    if L > 16 then
      L := 16;
    SetLength(K, L);
    Move(Dig[0], K[0], L);

    Res := AESDecryptCbcBytes(Res, K, Iv, kbt128);
    if Length(Res) > 0 then // 解密后再解除 PKCS5 对齐
    begin
      BytesRemovePKCS7Padding(Res);
      Data := Res;
    end;
  end;
end;

procedure TCnPDFDataCryptor.Encrypt(var Data: TBytes; ID, Generation: Cardinal);
var
  L: Integer;
  Res, Iv, K: TBytes;
  Dig: TCnMD5Digest;
begin
  if Length(Data) = 0 then
    Exit;

  MakeKey(ID, Generation);
  Dig := MD5(@FKey[0], Length(FKey));

  if FEncryptionMethod = cpem40RC4 then // RC4 加密，密钥长度分别是 5+5 和 16
    RC4Encrypt(@Dig[0], FKeyByteLength + 5, @Data[0], @Data[0], Length(Data))
  else if FEncryptionMethod = cpem128RC4 then
    RC4Encrypt(@Dig[0], SizeOf(TCnMD5Digest), @Data[0], @Data[0], Length(Data))
  else // AES
  begin
    // 随机生成 16 字节做 Iv
    SetLength(Iv, CN_AES_BLOCKSIZE);
    CnRandomFillBytes(@Iv[0], CN_AES_BLOCKSIZE);

    L := FKeyByteLength + 5;
    if L > 16 then
      L := 16;
    SetLength(K, L);
    Move(Dig[0], K[0], L);

    SetLength(Res, Length(Data));          // 要做 PKCS5 对齐
    Move(Data[0], Res[0], Length(Data));
    BytesAddPKCS7Padding(Res, CN_AES_BLOCKSIZE);

    Res := AESEncryptCbcBytes(Res, K, Iv, kbt128); // 再加密
    if Length(Res) > 0 then
      Data := ConcatBytes(Iv, Res);
  end;
end;

destructor TCnPDFDataCryptor.Destroy;
begin
  SetLength(FKey, 0);
  inherited;
end;

procedure TCnPDFDataCryptor.MakeKey(ID, Gen: Cardinal);
begin
  // 低三位和低两位
  Move(ID, FKey[FLength], 3);
  Move(Gen, FKey[FLength + 3], 2);
end;

end.
