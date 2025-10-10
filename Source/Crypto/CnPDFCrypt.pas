{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnPDFCrypt;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�PDF �ӽ��ܻ���ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ���� PDF �ļ��ļӽ��ܻ��ƣ��� CnPDF.pas �ж���������
*
*           PDF �ļ��ļӽ�����ظ��
*           Version ��ʾ���ܻ��ƣ�0 �ޣ�1 Ϊ 40 λ RC4��2/3 Ϊ 40 �� 128 λ RC4��4 �� Security Handler ��� Crypt Filter
*           Revision ����ָʾ Security Handler ��δ���Ȩ�ޣ�2 �ޡ�3 �С�4 ��֪�������Ը� Key ��������Ӱ��
*
*    ��֧�� Version  Revision     �����㷨                Key ָ���� Length                    PDF �淶�汾
*           0        *            ��
*        *  1        2            RC4                     ����ָ�����̶� 40 λ                 1.1
*        *  2        2            RC4                     �ɴ��� 40 λ���Ƽ� 128����Ȩ�޿���   1.4  �˴�����
*        *  2        3            RC4                     �ɴ��� 40 λ���Ƽ� 128����Ȩ�޿���   1.4
*           3        3            ��δ��������ʹ�ã�                                           1.4
*        *  4        4            ������ CF ��                                                 1.5
*                       CFM V2    RC4                     128���� CFM ��
*                       CFM AESV2 AES128/CBC/PKCS5        128���� CFM ��                       1.6
*           5        5  CFM AESV3 AES256/CBC/PKCS5        256���� CFM ��                       1.7
*
*           �ӽ��ܲ���������
*
*               �� Unicode �������� 40RC4 ����ͨ��       �� Unicode ������д 40RC4 ����ͨ��
*               �� Unicode �������� 128RC4 ����ͨ��      �� Unicode ������д 128RC4 ����ͨ��
*               �� Unicode �������� 128AES ����ͨ��      �� Unicode ������д 128AES ����ͨ��
*               Unicode �������� 40RC4 ����ͨ��          Unicode ������д 40RC4 ����ͨ��
*               Unicode �������� 128RC4 ����ͨ��         Unicode ������д 128RC4 ����ͨ��
*               Unicode �������� 128AES ����ͨ��         Unicode ������д 128AES ����ͨ��
*
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2024.03.02 V1.2
*               ʵ�� 128AES �ļӽ���
*           2024.03.02 V1.1
*               ʵ�� 128RC4 �ļӽ���
*           2024.02.29 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative;

type
  ECnPDFCryptException = class(Exception);
  {* PDF �ӽ��ܵ��쳣}

  TCnPDFEncryptionMethod = (cpemNotSupport, cpem40RC4, cpem128RC4, cpem128AES);
  {* ֧�ֵļ��ּ���ģʽ������ 40RC4 Ŀǰ�������⣬256 AES ��֧��}

  TCnPDFDataCryptor = class
  {* �ӽ���ʵ���࣬����һ��ԭʼ Key���ɷ����Զ��������ֽ���ʵʩ�ӽ���}
  private
    FEncryptionMethod: TCnPDFEncryptionMethod;
    FKeyByteLength: Integer; // �����㷨Ҫ�����Կ�ֽڳ��ȣ����Ǵ���� Key �ֽ����鳤��
    FKey: TBytes;            // ����� Key ����׷�����ݺ��ŵĵط����� MD5 ��
    FLength: Integer;        // ����� Key ��ԭʼ����
  protected
    procedure MakeKey(ID: Cardinal; Gen: Cardinal);
    {* ���� ID �� Gen ƴ��һ���ڲ� Key��

       ������
         ID: Cardinal                     - ��ƴ�յ� ID
         Gen: Cardinal                    - ��ƴ�յ� Gen

       ����ֵ�����ޣ�
    }
  public
    constructor Create(EncryptionMethod: TCnPDFEncryptionMethod;
      AKey: TBytes; KeyBitLength: Integer); virtual;
    {* ���캯����

       ������
         EncryptionMethod: TCnPDFEncryptionMethod         - ��������
         AKey: TBytes                                     - ��Կ
         KeyBitLength: Integer                            - ��Կ��λ����ע�Ⲣ����Կ�ֽڻ�λ����

       ����ֵ��                                           - ����ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    procedure Encrypt(var Data: TBytes; ID: Cardinal; Generation: Cardinal);
    {* ����ָ�� ID �� Generation �����ֽ����� Data��������·��� Data ��

       ������
         var Data: TBytes                 - �����ܵ��ֽ����飬�������·�����ֽ�����
         ID: Cardinal                     - ָ�� ID
         Generation: Cardinal             - ָ�� Generation

       ����ֵ�����ޣ�
    }

    procedure Decrypt(var Data: TBytes; ID: Cardinal; Generation: Cardinal);
    {* ����ָ�� ID �� Generation �����ֽ����� Data��������·��� Data ��

       ������
         var Data: TBytes                 - �����ܵ��ֽ����飬�������·�����ֽ�����
         ID: Cardinal                     - ָ�� ID
         Generation: Cardinal             - ָ�� Generation

       ����ֵ�����ޣ�
    }
  end;

function CnPDFFindEncryptionMethod(Version: Integer; Revision: Integer;
  KeyBitLength: Integer; const CFMValue: string = ''): TCnPDFEncryptionMethod;
{* ���� PDF �ļ��� Encrypt �ֶε� V R Length ֵ�� CTM �ֶ�ֵ�ж�ʹ�����ּ����㷨��

   ������
     Version: Integer                     - PDF �汾��
     Revision: Integer                    - PDF �޶��汾��
     KeyBitLength: Integer                - PDF ��Կλ��
     const CFMValue: string               - CTM �ֶ�ֵ

   ����ֵ��TCnPDFEncryptionMethod         - ���صļ����㷨����
}

function CnPDFCalcEncryptKey(const UserPass: AnsiString; Version: Integer;
  Revision: Integer; OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes;
  KeyBitLength: Integer): TBytes;
{* ���� PDF ʱ���ã������û������� O ֵ�ȼ������ Key�����ӽ����ַ����������ݡ�

   ������
     const UserPass: AnsiString           - �û�����
     Version: Integer                     - ���ܰ汾�ţ�ֻ֧�� 1  2  3  4
     Revision: Integer                    - �����޶��汾�ţ�ֻ֧�� 2  3  4
     OwnerCipher: TBytes                  - PDF �ļ��� Encrypt �ڵ�� O �ֶ�ֵ��һ�� 32 �ֽ�
     Permission: Cardinal                 - PDF �ļ��� Encrypt �ڵ�� P �ֶ�ֵ�����Ǹ�ֵҪǿ��ת�����޷��� 32 λ
     ID: TBytes                           - PDF �ļ��� Trailer ���ֵ� ID ����ĵ�һ���ַ���ֵ
     KeyBitLength: Integer                - PDF �ļ��� Encrypt �ڵ�� Length �ֶ�ֵ��һ���� 128��ʵ�ʳ��� 8 �õ��ֽ���

   ����ֵ��TBytes                         - ���������ӽ��ܵ���Կ�ֽ�����
}

function CnPDFCalcUserCipher(const UserPass: AnsiString; Version: Integer; Revision: Integer;
  OwnerCipher: TBytes; Permission: Cardinal; ID: TBytes; KeyBitLength: Integer): TBytes;
{* ���� PDF ʱ���ã������û���������ݼ��� U ֵ���ڲ������������ Key���ɹ����ĵ��е� U ֵ�Ա���ȷ���Ƿ�����ȷ���û����롣

   ������
     const UserPass: AnsiString           - �û�����
     Version: Integer                     - ���ܰ汾�ţ�ֻ֧�� 1  2  3  4
     Revision: Integer                    - �����޶��汾�ţ�ֻ֧�� 2  3  4
     OwnerCipher: TBytes                  - PDF �ļ��� Encrypt �ڵ�� O �ֶ�ֵ��һ�� 32 �ֽ�
     Permission: Cardinal                 - PDF �ļ��� Encrypt �ڵ�� P �ֶ�ֵ�����Ǹ�ֵҪǿ��ת�����޷��� 32 λ
     ID: TBytes                           - PDF �ļ��� Trailer ���ֵ� ID ����ĵ�һ���ַ���ֵ
     KeyBitLength: Integer                - PDF �ļ��� Encrypt �ڵ�� Length �ֶ�ֵ��һ���� 128��ʵ�ʳ��� 8 �õ��ֽ���

   ����ֵ��TBytes                         - ���ؿɷ����� PDF �ļ��� Encrypt �ڵ�� U �ֶ�ֵ������ԭʼ U ֵ�Ƚ���ȷ���û������Ƿ���ȷ
}

function CnPDFCalcOwnerCipher(const OwnerPass: AnsiString; const UserPass: AnsiString;
  Version: Integer; Revision: Integer; KeyBitLength: Integer): TBytes;
{* ���� PDF ʱ���ã�����Ȩ���������û�������� O ֵ��

   ������
     const OwnerPass: AnsiString          - Ȩ������
     const UserPass: AnsiString           - �û�����
     Version: Integer                     - ���ܰ汾�ţ�ֻ֧�� 1  2  3  4
     Revision: Integer                    - �����޶��汾�ţ�ֻ֧�� 2  3  4
     KeyBitLength: Integer                - PDF �ļ��� Encrypt �ڵ�� Length �ֶ�ֵ��һ���� 128��ʵ�ʳ��� 8 �õ��ֽ���

   ����ֵ��TBytes                         - ���ؿɷ����� PDF �ļ��� Encrypt �ڵ�� O �ֶ�ֵ
}

function CnPDFCheckUserPassword(const UserPass: AnsiString; Version: Integer;
  Revision: Integer; OwnerCipher: TBytes; UserCipher: TBytes; Permission: Cardinal;
  ID: TBytes; KeyBitLength: Integer): TBytes;
{* ���� PDF ʱ���ã�����û������ UserPass �Ƿ��ǺϷ����û����룬���ͨ�����ؼ�����Կ�����򷵻� nil��

   ������
     const UserPass: AnsiString           - �û�����
     Version: Integer                     - ���ܰ汾�ţ�ֻ֧�� 1  2  3  4
     Revision: Integer                    - �����޶��汾�ţ�ֻ֧�� 2  3  4
     OwnerCipher: TBytes                  - PDF �ļ��� Encrypt �ڵ�� O �ֶ�ֵ��һ�� 32 �ֽ�
     UserCipher: TBytes                   - PDF �ļ��� Encrypt �ڵ�� U �ֶ�ֵ��һ�� 32 �ֽ�
     Permission: Cardinal                 - PDF �ļ��� Encrypt �ڵ�� P �ֶ�ֵ�����Ǹ�ֵҪǿ��ת�����޷��� 32 λ
     ID: TBytes                           - PDF �ļ��� Trailer ���ֵ� ID ����ĵ�һ���ַ���ֵ
     KeyBitLength: Integer                - PDF �ļ��� Encrypt �ڵ�� Length �ֶ�ֵ��һ���� 128��ʵ�ʳ��� 8 �õ��ֽ���

   ����ֵ��TBytes                         - �ӽ��ܵ���Կ�ֽ�����
}

function CnPDFCheckOwnerPassword(const OwnerPass: AnsiString; Version: Integer;
  Revision: Integer; OwnerCipher: TBytes; UserCipher: TBytes; Permission: Cardinal;
  ID: TBytes; KeyBitLength: Integer): TBytes;
{* ���� PDF ʱ���ã�����û������ OwnerPass �Ƿ��ǺϷ���Ȩ�����룬���ͨ�����ؼ�����Կ�����򷵻� nil

   ������
     const OwnerPass: AnsiString          - Ȩ������
     Version: Integer                     - ���ܰ汾�ţ�ֻ֧�� 1  2  3  4
     Revision: Integer                    - �����޶��汾�ţ�ֻ֧�� 2  3  4
     OwnerCipher: TBytes                  - PDF �ļ��� Encrypt �ڵ�� O �ֶ�ֵ��һ�� 32 �ֽ�
     UserCipher: TBytes                   - PDF �ļ��� Encrypt �ڵ�� U �ֶ�ֵ��һ�� 32 �ֽ�
     Permission: Cardinal                 - PDF �ļ��� Encrypt �ڵ�� P �ֶ�ֵ�����Ǹ�ֵҪǿ��ת�����޷��� 32 λ
     ID: TBytes                           - PDF �ļ��� Trailer ���ֵ� ID ����ĵ�һ���ַ���ֵ
     KeyBitLength: Integer                - PDF �ļ��� Encrypt �ڵ�� Length �ֶ�ֵ��һ���� 128��ʵ�ʳ��� 8 �õ��ֽ���

   ����ֵ��TBytes                         - ���ؼӽ��ܵ���Կ�ֽ�����
}

implementation

uses
  CnRandom, CnMD5, CnRC4, CnAES, CnPemUtils;

const
  CN_PDF_ENCRYPT_SIZE = 32;       // 32 �ֽڶ���� PDF ����ģʽ

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

// ������������ض����ݲ���� 32 �ֽ�����
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

// ���������ݽ⿪�õ���������
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

  // ��û���ţ�ʹ��ԭʼ��
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

  if (KL <= 0) or (KL > 16) then // ��� 16 �ֽ�
    raise ECnPDFCryptException.Create(SCnErrorPDFKeyLength);

  PK := PaddingKey(UserPass);

  MD5Init(Ctx);
  MD5Update(Ctx, @PK[0], SizeOf(TCnPDFPaddingKey));
  MD5Update(Ctx, @OwnerCipher[0], Length(OwnerCipher));

  P := UInt32ToLittleEndian(Permission); // ǿ��С��
  MD5Update(Ctx, @P, SizeOf(P));

  MD5Update(Ctx, @ID[0], Length(ID));

// ֻ���� Metadata ���ܵ����������˴������淶�� FFFFFFFF
//  if Revision >= 4 then
//  begin
//    P := $FFFFFFFF;
//    MD5Update(Ctx, @P, SizeOf(P));
//  end;

  MD5Final(Ctx, Dig);

  if Revision >= 3 then  // ����ʮ�� MD5
  begin
    for I := 1 to 50 do
      Dig := MD5(@Dig[0], KL);

    SetLength(Result, 16);
  end
  else
    SetLength(Result, 5);

  Move(Dig[0], Result[0], Length(Result));
end;

{* 40 λ RC4 ��������������Ȩ������ʱ������Կ�ļ���ʵ��}
procedure CalcOwnerKey40(const OwnerPass: AnsiString; Key: PAnsiChar);
var
  Dig: TCnMD5Digest;
  OPK: TCnPDFPaddingKey;
begin
  OPK := PaddingKey(OwnerPass);

  // �ض������� 32 �ֽڲ���һ�� MD5
  Dig := MD5(@OPK[0], SizeOf(TCnPDFPaddingKey));

  // ֱ�Ӹ���ǰ 5 �ֽ�
  Move(Dig[0], Key^, 5);
end;

{* ���������Ȩ������ʱ��Ҫ������Կ���������Ϊ���á�40 λ RC4 ʱ������������Ҫ�� CalcOwnerKey40 ����}
function CalcOwnerKey(const OwnerPass: AnsiString; Version, Revision, KeyBitLength: Integer): TBytes;
var
  I, KL: Integer;
  Dig: TCnMD5Digest;
  OPK: TCnPDFPaddingKey;
begin
  OPK := PaddingKey(OwnerPass);

  // �ض������� 32 �ֽڲ���һ�� MD5
  Dig := MD5(@OPK[0], SizeOf(TCnPDFPaddingKey));

  if Revision <= 2 then
    KL := 5
  else
  begin
    KL := KeyBitLength div 8;

    // �� MD5 ������� 50 �� MD5
    for I := 1 to 50 do
      Dig := MD5(@Dig[0], SizeOf(TCnMD5Digest));
  end;

  if (KL <= 0) or (KL > 16) then // ��� 16 �ֽ�
    raise ECnPDFCryptException.Create(SCnErrorPDFKeyLength);

  SetLength(Result, KL);
  MoveMost(Dig[0], Result[0], KL, SizeOf(TCnMD5Digest));
  // Result �ǴӶ�� MD5 �����ȡ����ָ��������� 16 ���ֽ���Ϊ RC4 ��Կ
end;

function CnPDFCalcOwnerCipher(const OwnerPass, UserPass: AnsiString;
  Version, Revision, KeyBitLength: Integer): TBytes;
var
  I, J: Integer;
  UPK, XK: TCnPDFPaddingKey;
  RK: TBytes;
begin
  if (Version = 1) and (Revision = 2) then // 40 λ RC4 �㷨���ü򻯰�
  begin
    SetLength(RK, 5);
    CalcOwnerKey40(OwnerPass, @RK[0]);
  end
  else
    RK := CalcOwnerKey(OwnerPass, Version, Revision, KeyBitLength);

  // �û���������ض� 32 �ֽڵ� UPK ��
  UPK := PaddingKey(UserPass);

  // RC4 ����� 16 �ֽڵ� RK ���� 32 �ֽڵ� UPK������� UPK ��
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
    if (KL <= 0) or (KL > 16) then // ��� 16 �ֽ�
      raise ECnPDFCryptException.Create(SCnErrorPDFKeyLength);

    for I := 1 to 19 do
    begin
      for J := 0 to KL - 1 do
        XK[J] := Key[J] xor I;

      RC4Encrypt(@XK[0], KL, @Dig[0], @Dig[0], SizeOf(TCnPDFPaddingKey));
    end;

    SetLength(Result, SizeOf(TCnPDFPaddingKey));
    Move(Dig[0], Result[0], SizeOf(TCnMD5Digest));

    // �����ǰ 16 �ֽں󣬺� 16 �ֽ�����������
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

  if (Version = 1) and (Revision = 2) then // 40 λ RC4 �㷨���ü򻯰�
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

  // OC �ǽ��ܳ����Ķ���� Password����ȥ��֤
  MoveMost(OC[0], OCP[0], Length(OC), SizeOf(TCnPDFPaddingKey));
  UP := UnPaddingKey(OCP);

  // ��֤ͨ���򷵻���Կ����ͨ���򷵻� nil
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
    L := FLength + 9; // AES ����Ӹ����ֽ���

  SetLength(FKey, L);
  if Length(AKey) > 0 then
    Move(AKey[0], FKey[0], Length(AKey)); // ͷ���ȷ�ԭʼ Key

  if not (FEncryptionMethod in [cpem40RC4, cpem128RC4]) then // AES ����Ӹ����ֽ���
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

  if FEncryptionMethod = cpem40RC4 then // RC4 ���ܣ���Կ���ȷֱ��� 5+5 �� 16
    RC4Encrypt(@Dig[0], FKeyByteLength + 5, @Data[0], @Data[0], Length(Data))
  else if FEncryptionMethod = cpem128RC4 then
    RC4Encrypt(@Dig[0], SizeOf(TCnMD5Digest), @Data[0], @Data[0], Length(Data))
  else // AES
  begin
    // ǰ 16 �ֽڳ������ Iv
    if Length(Data) <= CN_AES_BLOCKSIZE then
      raise ECnPDFCryptException.Create(SCnErrorPDFDataLength);

    SetLength(Iv, CN_AES_BLOCKSIZE);
    Move(Data[0], Iv[0], CN_AES_BLOCKSIZE);

    // 16 �ֽں��������
    SetLength(Res, Length(Data) - CN_AES_BLOCKSIZE);
    Move(Data[CN_AES_BLOCKSIZE], Res[0], Length(Res));

    L := FKeyByteLength + 5;
    if L > 16 then
      L := 16;
    SetLength(K, L);
    Move(Dig[0], K[0], L);

    Res := AESDecryptCbcBytes(Res, K, Iv, kbt128);
    if Length(Res) > 0 then // ���ܺ��ٽ�� PKCS5 ����
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

  if FEncryptionMethod = cpem40RC4 then // RC4 ���ܣ���Կ���ȷֱ��� 5+5 �� 16
    RC4Encrypt(@Dig[0], FKeyByteLength + 5, @Data[0], @Data[0], Length(Data))
  else if FEncryptionMethod = cpem128RC4 then
    RC4Encrypt(@Dig[0], SizeOf(TCnMD5Digest), @Data[0], @Data[0], Length(Data))
  else // AES
  begin
    // ������� 16 �ֽ��� Iv
    SetLength(Iv, CN_AES_BLOCKSIZE);
    CnRandomFillBytes(@Iv[0], CN_AES_BLOCKSIZE);

    L := FKeyByteLength + 5;
    if L > 16 then
      L := 16;
    SetLength(K, L);
    Move(Dig[0], K[0], L);

    SetLength(Res, Length(Data));          // Ҫ�� PKCS5 ����
    Move(Data[0], Res[0], Length(Data));
    BytesAddPKCS7Padding(Res, CN_AES_BLOCKSIZE);

    Res := AESEncryptCbcBytes(Res, K, Iv, kbt128); // �ټ���
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
  // ����λ�͵���λ
  Move(ID, FKey[FLength], 3);
  Move(Gen, FKey[FLength + 3], 2);
end;

end.
