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

unit CnSHA3;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�SHA3 �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
*           ������/���� Keccak C ������ Pascal ��������ֲ���������䲿�ֹ��ܡ�
* ��    ע������Ԫʵ���� SHA3 ϵ���Ӵ��㷨����Ӧ�� HMAC �㷨������ SHA3-224/256/384/512
*           ���ɱ䳤��ժҪ�� SHAKE128/SHAKE256�ȡ�
*
*           SHA3 �淶���� NIST.FIPS.202
*           SHA-3 Standard: Permutation-Based Hash and Extendable-Output Functions
*           ���ж��ⶨ���� Bit ���� Byte ����ת����
*           �����֮���� Bit �������ܹ����� 8 ʱÿ 8 �� Bit ��λ���þ���һ���ֽڣ��ֽڼ��˳�򱣳ֲ��䡣
*
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWinXP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2023.08.02 V1.4
*               ���� SHAKE128/SHAKE256 �Ŀɱ䳤��ժҪ�ļ���
*           2022.04.26 V1.3
*               �޸� LongWord �� Integer ��ַת����֧�� MacOS64
*           2019.12.12 V1.2
*               ֧�� TBytes
*           2019.04.15 V1.1
*               ֧�� Win32/Win64/MacOS
*           2017.11.10 V1.0
*               ������Ԫ��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative, CnConsts;

const
  CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH = 32;
  {* SHAKE128 Ĭ���Ӵս�����ֽڳ���}

  CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH = 64;
  {* SHAKE256 Ĭ���Ӵս�����ֽڳ���}

type
  PCnSHA3GeneralDigest = ^TCnSHA3GeneralDigest;
  {* SHA3 ϵ��ͨ�õ��Ӵս��ָ��}
  TCnSHA3GeneralDigest = array[0..63] of Byte;
  {* SHA3 ϵ��ͨ�õ��Ӵս���������� 64 �ֽ�Ϊ׼}

  PCnSHA3_224Digest = ^TCnSHA3_224Digest;
  {* SHA3_224 �Ӵս��ָ��}
  TCnSHA3_224Digest = array[0..27] of Byte;
  {* SHA3_224 �Ӵս����28 �ֽ�}

  PCnSHA3_256Digest = ^TCnSHA3_256Digest;
  {* SHA3_256 �Ӵս��ָ��}
  TCnSHA3_256Digest = array[0..31] of Byte;
  {* SHA3_256 �Ӵս����32 �ֽ�}

  PCnSHA3_384Digest = ^TCnSHA3_384Digest;
  {* SHA3_384 �Ӵս��ָ��}
  TCnSHA3_384Digest = array[0..47] of Byte;
  {* SHA3_384 �Ӵս����48 �ֽ�}

  PCnSHA3_512Digest = ^TCnSHA3_512Digest;
  {* SHA3_512 �Ӵս��ָ��}
  TCnSHA3_512Digest = array[0..63] of Byte;
  {* SHA3_512 �Ӵս����64 �ֽ�}

  TCnSHA3Context = packed record
  {* SHA3 ϵ��ͨ�õ������Ľṹ}
    State: array[0..24] of Int64;
    Index: Cardinal;
    DigestLen: Cardinal;
    Round: Cardinal;
    BlockLen: Cardinal;
    Block: array[0..255] of Byte;
    Ipad: array[0..143] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..143] of Byte;      {!< HMAC: outer padding        }
  end;

  TCnSHA3CalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* SHA3 ϵ��ͨ�õļ�����Ȼص��¼���������}

function SHA3_224(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_224Digest;
{* �����ݿ���� SHA3_224 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_256Digest;
{* �����ݿ���� SHA3_256 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_384Digest;
{* �����ݿ���� SHA3_384 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_512Digest;
{* �����ݿ���� SHA3_512 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHA3_224Buffer(const Buffer; Count: Cardinal): TCnSHA3_224Digest;
{* �����ݿ���� SHA3_224 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256Buffer(const Buffer; Count: Cardinal): TCnSHA3_256Digest;
{* �����ݿ���� SHA3_256 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384Buffer(const Buffer; Count: Cardinal): TCnSHA3_384Digest;
{* �����ݿ���� SHA3_384 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512Buffer(const Buffer; Count: Cardinal): TCnSHA3_512Digest;
{* �����ݿ���� SHA3_512 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHAKE128Buffer(const Buffer; Count: Cardinal;
  DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �����ݿ�����Ӵճ��ȿɱ�� SHAKE128 ���㣬���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE256Buffer(const Buffer; Count: Cardinal;
 DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �����ݿ�����Ӵճ��ȿɱ�� SHAKE128 ���㣬���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

function SHA3_224Bytes(Data: TBytes): TCnSHA3_224Digest;
{* ���ֽ�������� SHA3_224 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256Bytes(Data: TBytes): TCnSHA3_256Digest;
{* ���ֽ�������� SHA3_256 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384Bytes(Data: TBytes): TCnSHA3_384Digest;
{* ���ֽ�������� SHA3_384 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512Bytes(Data: TBytes): TCnSHA3_512Digest;
{* ���ֽ�������� SHA3_512 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHAKE128Bytes(Data: TBytes; DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* ���ֽ���������Ӵճ��ȿɱ�� SHAKE128 ���㣬���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     Data: TBytes                         - ��������ֽ�����
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE256Bytes(Data: TBytes; DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* ���ֽ���������Ӵճ��ȿɱ�� SHAKE256 ���㣬���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     Data: TBytes                         - ��������ֽ�����
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

function SHA3_224String(const Str: string): TCnSHA3_224Digest;
{* �� String �������ݽ��� SHA3_224 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256String(const Str: string): TCnSHA3_256Digest;
{* �� String �������ݽ��� SHA3_256 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384String(const Str: string): TCnSHA3_384Digest;
{* �� String �������ݽ��� SHA3_384 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512String(const Str: string): TCnSHA3_512Digest;
{* �� String �������ݽ��� SHA3_512 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHAKE128String(const Str: string; DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� String �������ݽ����Ӵճ��ȿɱ�� SHAKE128 ���㣬���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����
   ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString�������лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE256String(const Str: string; DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� String �������ݽ����Ӵճ��ȿɱ�� SHAKE256 ���㣬���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����
   ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString�������лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

function SHA3_224StringA(const Str: AnsiString): TCnSHA3_224Digest;
{* �� AnsiString �������ݽ��� SHA3_224 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_224StringW(const Str: WideString): TCnSHA3_224Digest;
{* �� WideString �������ݽ��� SHA3_224 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256StringA(const Str: AnsiString): TCnSHA3_256Digest;
{* �� AnsiString �������ݽ��� SHA3_256 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_256StringW(const Str: WideString): TCnSHA3_256Digest;
{* �� WideString�������ݽ��� SHA3_256 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384StringA(const Str: AnsiString): TCnSHA3_384Digest;
{* �� AnsiString �������ݽ��� SHA3_384 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_384StringW(const Str: WideString): TCnSHA3_384Digest;
{* �� WideString �������ݽ��� SHA3_384 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512StringA(const Str: AnsiString): TCnSHA3_512Digest;
{* �� AnsiString �������ݽ��� SHA3_512 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���
                                            
   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHA3_512StringW(const Str: WideString): TCnSHA3_512Digest;
{* �� WideString �������ݽ��� SHA512 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHAKE128StringA(const Str: AnsiString;
  DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� AnsiString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE128 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Str: AnsiString                - ��������ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE128StringW(const Str: WideString;
  DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� WideString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE128 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE256StringA(const Str: AnsiString; DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� AnsiString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE128 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Str: AnsiString                - ��������ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

function SHAKE256StringW(const Str: WideString; DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� WideString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE256 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

{$IFDEF UNICODE}

function SHA3_224UnicodeString(const Str: string): TCnSHA3_224Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_224 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256UnicodeString(const Str: string): TCnSHA3_256Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384UnicodeString(const Str: string): TCnSHA3_384Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_384 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512UnicodeString(const Str: string): TCnSHA3_512Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_512 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHAKE128UnicodeString(const Str: string;
  DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� UnicodeString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE128 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Str: string                    - ������Ŀ��ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE256UnicodeString(const Str: string;
  DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� UnicodeString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Str: string                    - ������Ŀ��ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

{$ELSE}

function SHA3_224UnicodeString(const Str: WideString): TCnSHA3_224Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_224 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256UnicodeString(const Str: WideString): TCnSHA3_256Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384UnicodeString(const Str: WideString): TCnSHA3_384Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_384 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512UnicodeString(const Str: WideString): TCnSHA3_512Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_512 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHAKE128UnicodeString(const Str: WideString;
  DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� UnicodeString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE128 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Str: WideString                - ������Ŀ��ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE256UnicodeString(const Str: WideString;
  DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH): TBytes;
{* �� UnicodeString �������ݽ����Ӵճ��ȿɱ��ֱ�� SHAKE256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const Str: WideString                - ������Ŀ��ַ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

{$ENDIF}

function SHA3_224File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc =
  nil): TCnSHA3_224Digest;
{* ��ָ���ļ����ݽ��� SHA3_224 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_224Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc = nil):
  TCnSHA3_224Digest;
{* ��ָ�������ݽ��� SHA3_224 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_224Digest              - ���ص� SHA3_224 �Ӵ�ֵ
}

function SHA3_256File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc =
  nil): TCnSHA3_256Digest;
{* ��ָ���ļ����ݽ��� SHA3_256 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_256Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc = nil):
  TCnSHA3_256Digest;
{* ��ָ�������ݽ��� SHA3_256 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_256Digest              - ���ص� SHA3_256 �Ӵ�ֵ
}

function SHA3_384File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc =
  nil): TCnSHA3_384Digest;
{* ��ָ���ļ����ݽ��� SHA3_384 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_384Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc = nil):
  TCnSHA3_384Digest;
{* ��ָ�������ݽ��� SHA3_384 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_384Digest              - ���ص� SHA3_384 �Ӵ�ֵ
}

function SHA3_512File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc =
  nil): TCnSHA3_512Digest;
{* ��ָ���ļ����ݽ��� SHA3_512 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHA3_512Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc = nil):
  TCnSHA3_512Digest;
{* ��ָ�������ݽ��� SHA3_512 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA3_512Digest              - ���ص� SHA3_512 �Ӵ�ֵ
}

function SHAKE128File(const FileName: string;
  DigestByteLength: Cardinal  = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH;
  CallBack: TCnSHA3CalcProgressFunc = nil): TBytes;
{* ��ָ���ļ����ݽ����Ӵճ��ȿɱ�� SHAKE128 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const FileName: string               - ��������ļ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TBytes                         - ���� SHAKE128�Ӵ�ֵ
}

function SHAKE128Stream(Stream: TStream;
  DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH;
  CallBack: TCnSHA3CalcProgressFunc = nil): TBytes;
{* ��ָ�������������Ӵճ��ȿɱ�� SHAKE128 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     Stream: TStream                      - �������������
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TBytes                         - ���� SHAKE128 �Ӵ�ֵ
}

function SHAKE256File(const FileName: string;
  DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH;
  CallBack: TCnSHA3CalcProgressFunc = nil): TBytes;
{* ��ָ���ļ����ݽ����Ӵճ��ȿɱ�� SHAKE256 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     const FileName: string               - ��������ļ���
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

function SHAKE256Stream(Stream: TStream;
  DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH;
  CallBack: TCnSHA3CalcProgressFunc = nil): TBytes;
{* ��ָ�������������Ӵճ��ȿɱ�� SHAKE256 ���㣬
   ���س���Ϊ DigestByteLength ���ֽ�������Ϊ�Ӵս����

   ������
     Stream: TStream                      - �������������
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���
     CallBack: TCnSHA3CalcProgressFunc    - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TBytes                         - ���� SHAKE256 �Ӵ�ֵ
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA3_224 ���㣬SHA3_224Update �ɶ�α�����

procedure SHA3_224Init(var Context: TCnSHA3Context);
{* ��ʼ��һ�� SHA3_224 ���������ģ�׼������ SHA3_224 �����

   ������
     var Context: TCnSHA3Context          - ����ʼ����ͨ�� SHA3 ������

   ����ֵ�����ޣ�
}

procedure SHA3_224Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA3_224 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA3_224Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_224Digest);
{* �������ּ��㣬�� SHA3_224 ��������� Digest �С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     var Digest: TCnSHA3_224Digest        - ���ص� SHA3_224 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA3_256 ���㣬SHA3_256Update �ɶ�α�����

procedure SHA3_256Init(var Context: TCnSHA3Context);
{* ��ʼ��һ�� SHA3_256 ���������ģ�׼������ SHA3_256 �����

   ������
     var Context: TCnSHA3Context          -  ����ʼ����ͨ�� SHA3 ������

   ����ֵ�����ޣ�
}

procedure SHA3_256Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA3_256 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA3_256Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_256Digest);
{* �������ּ��㣬�� SHA3_256 ��������� Digest �С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     var Digest: TCnSHA3_256Digest        - ���ص� SHA3_256 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA3_384 ���㣬SHA3_384Update �ɶ�α�����

procedure SHA3_384Init(var Context: TCnSHA3Context);
{* ��ʼ��һ�� SHA3_384 ���������ģ�׼������ SHA3_384 �����

   ������
     var Context: TCnSHA3Context          - ����ʼ����ͨ�� SHA3 ������

   ����ֵ�����ޣ�
}

procedure SHA3_384Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA3_384 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA3_384Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_384Digest);
{* �������ּ��㣬�� SHA3_384 ��������� Digest �С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     var Digest: TCnSHA3_384Digest        - ���ص� SHA3_384 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA3_512 ���㣬SHA3_512Update �ɶ�α�����

procedure SHA3_512Init(var Context: TCnSHA3Context);
{* ��ʼ��һ�� SHA3_512 ���������ģ�׼������ SHA3_512 ���

   ������
     var Context: TCnSHA3Context          - ����ʼ����ͨ�� SHA3 ������

   ����ֵ�����ޣ�
}

procedure SHA3_512Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA3_512 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA3_512Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_512Digest);
{* �������ּ��㣬�� SHA3_512 ��������� Digest �С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     var Digest: TCnSHA3_512Digest        - ���ص� SHA3_512 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHAKE128 ���㣬SHAKE128Update �ɶ�α�����

procedure SHAKE128Init(var Context: TCnSHA3Context; DigestByteLength: Cardinal = CN_SHAKE128_DEF_DIGEST_BYTE_LENGTH);
{* ��ʼ��һ�� SHAKE128 ���������ģ�׼������ SHAKE128 �����
   DigestByteLength Ϊ������Ӵյ��ֽڳ��ȡ�

   ������
     var Context: TCnSHA3Context          - ����ʼ����ͨ�� SHA3 ������
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHAKE128Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHAKE128 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHAKE128Final(var Context: TCnSHA3Context; out Digest: TBytes);
{* �������ּ��㣬�� SHAKE128 ��������� Digest �С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     out Digest: TBytes                   - ���ص� SHAKE128 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHAKE128 ���㣬SHAKE128Update �ɶ�α�����

procedure SHAKE256Init(var Context: TCnSHA3Context; DigestByteLength: Cardinal = CN_SHAKE256_DEF_DIGEST_BYTE_LENGTH);
{* ��ʼ��һ�� SHAKE256 ���������ģ�׼������ SHAKE256 �����
   DigestByteLength Ϊ������Ӵյ��ֽڳ��ȡ�

   ������
     var Context: TCnSHA3Context          - ����ʼ����ͨ�� SHA3 ������
     DigestByteLength: Cardinal           - ������Ӵս���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHAKE256Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHAKE256 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHAKE256Final(var Context: TCnSHA3Context; out Digest: TBytes);
{* �������ּ��㣬�� SHAKE256 ��������� Digest �С�

   ������
     var Context: TCnSHA3Context          - ͨ�� SHA3 ������
     out Digest: TBytes                   - ���ص� SHAKE256 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

function SHA3_224Print(const Digest: TCnSHA3_224Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_224 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA3_224Digest      - ָ���� SHA3_224 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA3_256Print(const Digest: TCnSHA3_256Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_256 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA3_256Digest      - ָ���� SHA3_256 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA3_384Print(const Digest: TCnSHA3_384Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_384 �Ӵ�ֵ��
   ������
     const Digest: TCnSHA3_384Digest      - ָ���� SHA3_384 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA3_512Print(const Digest: TCnSHA3_512Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA3_512 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA3_512Digest      - ָ���� SHA3_512 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHAKE128Print(const Digest: TBytes): string;
{* ��ʮ�����Ƹ�ʽ��� SHAKE128 �Ӵ�ֵ��

   ������
     const Digest: TBytes                 - ָ���� SHAKE128 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHAKE256Print(const Digest: TBytes): string;
{* ��ʮ�����Ƹ�ʽ��� SHAKE256 �Ӵ�ֵ��

   ������
     const Digest: TBytes                 - ָ���� SHAKE128 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA3_224Match(const D1: TCnSHA3_224Digest; const D2: TCnSHA3_224Digest): Boolean;
{* �Ƚ����� SHA3_224 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA3_224Digest          - ���Ƚϵ� SHA3_224 �Ӵ�ֵһ
     const D2: TCnSHA3_224Digest          - ���Ƚϵ� SHA3_224 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA3_256Match(const D1: TCnSHA3_256Digest; const D2: TCnSHA3_256Digest): Boolean;
{* �Ƚ����� SHA3_256 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA3_256Digest          - ���Ƚϵ� SHA3_256 �Ӵ�ֵһ
     const D2: TCnSHA3_256Digest          - ���Ƚϵ� SHA3_256 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA3_384Match(const D1: TCnSHA3_384Digest; const D2: TCnSHA3_384Digest): Boolean;
{* �Ƚ����� SHA3_384 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA3_384Digest          - ���Ƚϵ� SHA3_384 �Ӵ�ֵһ
     const D2: TCnSHA3_384Digest          - ���Ƚϵ� SHA3_384 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA3_512Match(const D1: TCnSHA3_512Digest; const D2: TCnSHA3_512Digest): Boolean;
{* �Ƚ����� SHA3_512 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA3_512Digest          - ���Ƚϵ� SHA3_512 �Ӵ�ֵһ
     const D2: TCnSHA3_512Digest          - ���Ƚϵ� SHA3_512 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHAKE128Match(const D1: TBytes; const D2: TBytes): Boolean;
{* �Ƚ����� SHAKE128 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TBytes                     - ���Ƚϵ� SHAKE128 �Ӵ�ֵһ
     const D2: TBytes                     - ���Ƚϵ� SHAKE128 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHAKE256Match(const D1: TBytes; const D2: TBytes): Boolean;
{* �Ƚ����� SHAKE256 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TBytes                     - ���Ƚϵ� SHAKE256 �Ӵ�ֵһ
     const D2: TBytes                     - ���Ƚϵ� SHAKE256 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA3_224DigestToStr(const Digest: TCnSHA3_224Digest): string;
{* SHA3_224 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA3_224Digest      - ��ת���� SHA3_224 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHA3_256DigestToStr(const Digest: TCnSHA3_256Digest): string;
{* SHA3_256 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���
 |<PRE>
   Digest: TSHA3_256Digest   - ��Ҫ
 |</PRE>

   ������
     const Digest: TCnSHA3_256Digest      - ��ת���� SHA3_256 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHA3_384DigestToStr(const Digest: TCnSHA3_384Digest): string;
{* SHA3_384 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA3_384Digest      - ��ת���� SHA3_384 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHA3_512DigestToStr(const Digest: TCnSHA3_512Digest): string;
{* SHA3_512 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA3_512Digest      - ��ת���� SHA3_512 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHAKE128DigestToStr(const Digest: TBytes): string;
{* SHAKE128 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TBytes                 - ��ת���� SHAKE128 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHAKE256DigestToStr(const Digest: TBytes): string;
{* SHAKE256 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TBytes                 - ��ת���� SHAKE256 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

procedure SHA3_224Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_224Digest);
{* ���� SHA3_224 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA3_224 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA3_224 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA3_224Digest        - ���ص� SHA3_224 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure SHA3_256Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_256Digest);
{* ���� SHA3_256 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA3_256 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA3_256 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA3_256Digest        - ���ص� SHA3_256 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure SHA3_384Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_384Digest);
{* ���� SHA3_384 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA3_384 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA3_384 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA3_384Digest        - ���ص� SHA3_384 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure SHA3_512Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_512Digest);
{* ���� SHA3_512 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA3_512 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA3_512 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA3_512Digest        - ���ص� SHA3_512 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

implementation

type
  TSHA3Type = (stSHA3_224, stSHA3_256, stSHA3_384, stSHA3_512, stSHAKE128, stSHAKE256);

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  STREAM_BUF_SIZE = 4096 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  SHA3_ROUNDS = 24;
  SHA3_STATE_LEN = 25;

  SHA3_224_OUTPUT_LENGTH_BYTE = 28;
  SHA3_256_OUTPUT_LENGTH_BYTE = 32;
  SHA3_384_OUTPUT_LENGTH_BYTE = 48;
  SHA3_512_OUTPUT_LENGTH_BYTE = 64;

  SHA3_224_BLOCK_SIZE_BYTE = 144;
  SHA3_256_BLOCK_SIZE_BYTE = 136;
  SHA3_384_BLOCK_SIZE_BYTE = 104;
  SHA3_512_BLOCK_SIZE_BYTE = 72;

  SHAKE128_BLOCK_SIZE_BYTE = 168;
  SHAKE256_BLOCK_SIZE_BYTE = 136;

  HMAC_SHA3_224_BLOCK_SIZE_BYTE = SHA3_224_BLOCK_SIZE_BYTE;
  HMAC_SHA3_256_BLOCK_SIZE_BYTE = SHA3_256_BLOCK_SIZE_BYTE;
  HMAC_SHA3_384_BLOCK_SIZE_BYTE = SHA3_384_BLOCK_SIZE_BYTE;
  HMAC_SHA3_512_BLOCK_SIZE_BYTE = SHA3_512_BLOCK_SIZE_BYTE;

  HMAC_SHA3_224_OUTPUT_LENGTH_BYTE = SHA3_224_OUTPUT_LENGTH_BYTE;
  HMAC_SHA3_256_OUTPUT_LENGTH_BYTE = SHA3_256_OUTPUT_LENGTH_BYTE;
  HMAC_SHA3_384_OUTPUT_LENGTH_BYTE = SHA3_384_OUTPUT_LENGTH_BYTE;
  HMAC_SHA3_512_OUTPUT_LENGTH_BYTE = SHA3_512_OUTPUT_LENGTH_BYTE;

  KECCAKF_ROUND_CONSTS: array[0..23] of TUInt64 = (
    $0000000000000001, $0000000000008082, $800000000000808A,
    $8000000080008000, $000000000000808B, $0000000080000001,
    $8000000080008081, $8000000000008009, $000000000000008A,
    $0000000000000088, $0000000080008009, $000000008000000A,
    $000000008000808B, $800000000000008B, $8000000000008089,
    $8000000000008003, $8000000000008002, $8000000000000080,
    $000000000000800A, $800000008000000A, $8000000080008081,
    $8000000000008080, $0000000080000001, $8000000080008008
  );

  KECCAKF_ROT_CONSTS: array[0..23] of Integer = (
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
  );

  KECCAKF_PILN: array[0..23] of Integer = (
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
  );

function ROTL64(Q: TUInt64; N: Integer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Q shl N) xor (Q shr (64 - N));
end;

// һ�� SHA3 ���㣬������ Block ���ݣ������ State ����
procedure SHA3_Transform(var Context: TCnSHA3Context);
type
  PUInt64Array = ^TUInt64Array;
  TUInt64Array = array[0..4095] of TUInt64;
var
  I, J, R, L: Integer;
  P: PUInt64Array;
  T: TUInt64;
  BC: array[0..4] of TUInt64;
begin
  P := PUInt64Array(@(Context.Block[0]));
  I := 0;
  L := Integer(Context.BlockLen div 8);
  while I < L do
  begin
    Context.State[I] := Context.State[I] xor P^[I];
    Inc(I);
  end;

  for R := 0 to Context.Round - 1 do
  begin
    // Theta
    for I := 0 to 4 do
    begin
      BC[I] := Context.State[I] xor Context.State[I + 5] xor Context.State[I + 10]
        xor Context.State[I + 15] xor Context.State[I + 20];
    end;
    for I := 0 to 4 do
    begin
      T := BC[(I + 4) mod 5] xor ROTL64(BC[(I + 1) mod 5], 1);
      for J := 0 to 4 do
        Context.State[5 * J + I] := Context.State[5 * J + I] xor T;
    end;

    // Rho Pi
    T := Context.State[1];
    for I := 0 to 23 do
    begin
      J := KECCAKF_PILN[I];
      BC[0] := Context.State[J];
      Context.State[J] := ROTL64(T, KECCAKF_ROT_CONSTS[I]);
      T := BC[0];
    end;

    // Chi
    for J := 0 to 4 do
    begin
      for I := 0 to 4 do
        BC[I] := Context.State[5 * J + I];

      for I := 0 to 4 do
        Context.State[5 * J + I] := Context.State[5 * J + I] xor
          ((not BC[(I + 1) mod 5]) and BC[(I + 2) mod 5]);
    end;

    // Iota
    Context.State[0] := Context.State[0] xor KECCAKF_ROUND_CONSTS[R];
  end;
end;

procedure SHA3Init(var Context: TCnSHA3Context; SHA3Type: TSHA3Type;
  DigestByteLength: Cardinal = 0);
begin
  FillChar(Context.State, SizeOf(Context.State), 0);
  FillChar(Context.Block, SizeOf(Context.Block), 0);
  Context.Index := 0;
  Context.Round := SHA3_ROUNDS;

  case SHA3Type of
  stSHA3_224:
    begin
      Context.BlockLen := SHA3_224_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_224_OUTPUT_LENGTH_BYTE;
    end;
  stSHA3_256:
    begin
      Context.BlockLen := SHA3_256_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_256_OUTPUT_LENGTH_BYTE;
    end;
  stSHA3_384:
    begin
      Context.BlockLen := SHA3_384_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_384_OUTPUT_LENGTH_BYTE;
    end;
  stSHA3_512:
    begin
      Context.BlockLen := SHA3_512_BLOCK_SIZE_BYTE;
      Context.DigestLen := SHA3_512_OUTPUT_LENGTH_BYTE;
    end;
  stSHAKE128:
    begin
      Context.BlockLen := SHAKE128_BLOCK_SIZE_BYTE;
      Context.DigestLen := DigestByteLength;
    end;
  stSHAKE256:
    begin
      Context.BlockLen := SHAKE256_BLOCK_SIZE_BYTE;
      Context.DigestLen := DigestByteLength;
    end;
  end;
end;

procedure SHA3Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  R, Idx: Cardinal;
begin
  Idx := Context.Index;                                 // Index �� Block �еĳ�ʼλ��ָ��
  repeat
    if ByteLength < Context.BlockLen - Idx then
      R := ByteLength                                   // ���
    else
      R := Context.BlockLen - Idx;                      // �������ܻ���ʣ

    FillChar(Context.Block[Idx], SizeOf(Context.Block) - Idx, 0);  // ȷ��β��Ϊ 0
    Move(Input^, Context.Block[Idx], R);                // �� Block ��ǰ�벿�ֲ�������

    if (Idx + R) < Context.BlockLen then                // ���û�������ֲ�����
    begin                                               // ֻ���� Index λ��ָ��
      Idx := Idx + R;
      Break;
    end;

    SHA3_Transform(Context);
    Dec(ByteLength, R);
    Idx := 0;
    Inc(Input, R);
  until False;
  Context.Index := Idx;
end;

procedure SHA3UpdateW(var Context: TCnSHA3Context; Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  Len: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    Len := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    SHA3Update(Context, Content, Len);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  SHA3Update(Context, @A[1], Length(A));
{$ENDIF}
end;

// SHA3_224/256/384/512 ר��
procedure SHA3Final(var Context: TCnSHA3Context; var Digest: TCnSHA3GeneralDigest); overload;
begin
  Context.Block[Context.Index] := 6;
  Context.Block[Context.BlockLen - 1] := Context.Block[Context.BlockLen - 1] or $80;
  SHA3_Transform(Context);
  Move(Context.State[0], Digest[0], Context.DigestLen);
end;

// SHAKE128 �� SHAKE256 ר��
procedure SHA3Final(var Context: TCnSHA3Context; out Digest: TBytes); overload;
var
  Idx, DL: Cardinal;
begin
  Context.Block[Context.Index] := $1F;
  Context.Block[Context.BlockLen - 1] := Context.Block[Context.BlockLen - 1] or $80;
  SHA3_Transform(Context);

  SetLength(Digest, Context.DigestLen);
  if Context.DigestLen <= Context.BlockLen then
    Move(Context.State[0], Digest[0], Context.DigestLen)
  else
  begin
    DL := Context.DigestLen;
    Idx := 0;

    while DL >= Context.BlockLen do
    begin
      Move(Context.State[0], Digest[Idx], Context.BlockLen);
      Inc(Idx, Context.BlockLen);
      Dec(DL, Context.BlockLen);

      if DL > 0 then
      begin
        FillChar(Context.Block[0], SizeOf(Context.Block), 0);
        SHA3_Transform(Context);
      end;
    end;

    if DL > 0 then
      Move(Context.State[0], Digest[Idx], DL);
  end;
end;

procedure SHA3_224Init(var Context: TCnSHA3Context);
begin
  SHA3Init(Context, stSHA3_224);
end;

procedure SHA3_224Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_224Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_224Digest);
var
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Final(Context, Res);
  Move(Res[0], Digest[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

procedure SHA3_256Init(var Context: TCnSHA3Context);
begin
  SHA3Init(Context, stSHA3_256);
end;

procedure SHA3_256Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_256Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_256Digest);
var
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Final(Context, Res);
  Move(Res[0], Digest[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

procedure SHA3_384Init(var Context: TCnSHA3Context);
begin
  SHA3Init(Context, stSHA3_384);
end;

procedure SHA3_384Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_384Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_384Digest);
var
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Final(Context, Res);
  Move(Res[0], Digest[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

procedure SHA3_512Init(var Context: TCnSHA3Context);
begin
  SHA3Init(Context, stSHA3_512);
end;

procedure SHA3_512Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_512Final(var Context: TCnSHA3Context; var Digest: TCnSHA3_512Digest);
var
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Final(Context, Res);
  Move(Res[0], Digest[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

procedure SHAKE128Init(var Context: TCnSHA3Context; DigestByteLength: Cardinal);
begin
  SHA3Init(Context, stSHAKE128, DigestByteLength);
end;

procedure SHAKE128Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHAKE128Final(var Context: TCnSHA3Context; out Digest: TBytes);
begin
  SHA3Final(Context, Digest);
end;

procedure SHAKE256Init(var Context: TCnSHA3Context; DigestByteLength: Cardinal);
begin
  SHA3Init(Context, stSHAKE256, DigestByteLength);
end;

procedure SHAKE256Update(var Context: TCnSHA3Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHAKE256Final(var Context: TCnSHA3Context; out Digest: TBytes);
begin
  SHA3Final(Context, Digest);
end;

// �����ݿ���� SHA3_224λ����
function SHA3_224(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_224Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, Input, ByteLength);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_256λ����
function SHA3_256(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_256Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, Input, ByteLength);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_384λ����
function SHA3_384(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_384Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, Input, ByteLength);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_512λ����
function SHA3_512(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA3_512Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, Input, ByteLength);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_224 ����
function SHA3_224Buffer(const Buffer; Count: Cardinal): TCnSHA3_224Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_256 ����
function SHA3_256Buffer(const Buffer; Count: Cardinal): TCnSHA3_256Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_384 ����
function SHA3_384Buffer(const Buffer; Count: Cardinal): TCnSHA3_384Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHA3_512 ����
function SHA3_512Buffer(const Buffer; Count: Cardinal): TCnSHA3_512Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(Buffer), Count);
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// �����ݿ���� SHAKE128 ����
function SHAKE128Buffer(const Buffer; Count: Cardinal; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE128Init(Context, DigestByteLength);
  SHAKE128Update(Context, PAnsiChar(Buffer), Count);
  SHAKE128Final(Context, Result);
end;

// �����ݿ���� SHAKE256 ����
function SHAKE256Buffer(const Buffer; Count: Cardinal; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE256Init(Context, DigestByteLength);
  SHAKE256Update(Context, PAnsiChar(Buffer), Count);
  SHAKE256Final(Context, Result);
end;

// ���ֽ�������� SHA3_224 ����
function SHA3_224Bytes(Data: TBytes): TCnSHA3_224Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// ���ֽ�������� SHA3_256 ����
function SHA3_256Bytes(Data: TBytes): TCnSHA3_256Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// ���ֽ�������� SHA3_384 ����
function SHA3_384Bytes(Data: TBytes): TCnSHA3_384Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// ���ֽ�������� SHA3_512 ����
function SHA3_512Bytes(Data: TBytes): TCnSHA3_512Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// ���ֽ�������� SHAKE128 ����
function SHAKE128Bytes(Data: TBytes; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE128Init(Context, DigestByteLength);
  SHAKE128Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHAKE128Final(Context, Result);
end;

// ���ֽ�������� SHAKE256 ����
function SHAKE256Bytes(Data: TBytes; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE256Init(Context, DigestByteLength);
  SHAKE256Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHAKE256Final(Context, Result);
end;

// �� String �������ݽ��� SHA3_224 ����
function SHA3_224String(const Str: string): TCnSHA3_224Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_224StringA(AStr);
end;

// �� String �������ݽ��� SHA3_256 ����
function SHA3_256String(const Str: string): TCnSHA3_256Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_256StringA(AStr);
end;

// �� String �������ݽ��� SHA3_384 ����
function SHA3_384String(const Str: string): TCnSHA3_384Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_384StringA(AStr);
end;

// �� String �������ݽ��� SHA3_512 ����
function SHA3_512String(const Str: string): TCnSHA3_512Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA3_512StringA(AStr);
end;

// �� String �������ݽ��� SHAKE128 ����
function SHAKE128String(const Str: string; DigestByteLength: Cardinal): TBytes;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHAKE128StringA(AStr, DigestByteLength);
end;

// �� String �������ݽ��� SHAKE256 ����
function SHAKE256String(const Str: string; DigestByteLength: Cardinal): TBytes;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHAKE256StringA(AStr, DigestByteLength);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_224 ���㣬������ת��
{$IFDEF UNICODE}
function SHA3_224UnicodeString(const Str: string): TCnSHA3_224Digest;
{$ELSE}
function SHA3_224UnicodeString(const Str: WideString): TCnSHA3_224Digest;
{$ENDIF}
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_256 ���㣬������ת��
{$IFDEF UNICODE}
function SHA3_256UnicodeString(const Str: string): TCnSHA3_256Digest;
{$ELSE}
function SHA3_256UnicodeString(const Str: WideString): TCnSHA3_256Digest;
{$ENDIF}
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_384 ���㣬������ת��
{$IFDEF UNICODE}
function SHA3_384UnicodeString(const Str: string): TCnSHA3_384Digest;
{$ELSE}
function SHA3_384UnicodeString(const Str: WideString): TCnSHA3_384Digest;
{$ENDIF}
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA3_512 ���㣬������ת��
{$IFDEF UNICODE}
function SHA3_512UnicodeString(const Str: string): TCnSHA3_512Digest;
{$ELSE}
function SHA3_512UnicodeString(const Str: WideString): TCnSHA3_512Digest;
{$ENDIF}
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHAKE128 ���㣬������ת��
{$IFDEF UNICODE}
function SHAKE128UnicodeString(const Str: string; DigestByteLength: Cardinal): TBytes;
{$ELSE}
function SHAKE128UnicodeString(const Str: WideString; DigestByteLength: Cardinal): TBytes;
{$ENDIF}
var
  Context: TCnSHA3Context;
begin
  SHAKE128Init(Context, DigestByteLength);
  SHAKE128Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHAKE128Final(Context, Result);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHAKE256 ���㣬������ת��
{$IFDEF UNICODE}
function SHAKE256UnicodeString(const Str: string; DigestByteLength: Cardinal): TBytes;
{$ELSE}
function SHAKE256UnicodeString(const Str: WideString; DigestByteLength: Cardinal): TBytes;
{$ENDIF}
var
  Context: TCnSHA3Context;
begin
  SHAKE256Init(Context, DigestByteLength);
  SHAKE256Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHAKE256Final(Context, Result);
end;

// �� AnsiString �������ݽ���SHA224 ����
function SHA3_224StringA(const Str: AnsiString): TCnSHA3_224Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_224 ����
function SHA3_224StringW(const Str: WideString): TCnSHA3_224Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_224);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_224_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ��� SHA3_256 ����
function SHA3_256StringA(const Str: AnsiString): TCnSHA3_256Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_256 ����
function SHA3_256StringW(const Str: WideString): TCnSHA3_256Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_256);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_256_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ��� SHA3_384 ����
function SHA3_384StringA(const Str: AnsiString): TCnSHA3_384Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_384 ����
function SHA3_384StringW(const Str: WideString): TCnSHA3_384Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_384);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_384_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ��� SHA3_512 ����
function SHA3_512StringA(const Str: AnsiString): TCnSHA3_512Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, PAnsiChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// �� WideString �������ݽ��� SHA3_512 ����
function SHA3_512StringW(const Str: WideString): TCnSHA3_512Digest;
var
  Context: TCnSHA3Context;
  Res: TCnSHA3GeneralDigest;
begin
  SHA3Init(Context, stSHA3_512);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str));
  SHA3Final(Context, Res);
  Move(Res[0], Result[0], SHA3_512_OUTPUT_LENGTH_BYTE);
end;

// �� AnsiString �������ݽ��� SHAKE128 ����
function SHAKE128StringA(const Str: AnsiString; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE128Init(Context, DigestByteLength);
  SHAKE128Update(Context, PAnsiChar(Str), Length(Str));
  SHAKE128Final(Context, Result);
end;

// �� WideString �������ݽ��� SHAKE128 ����
function SHAKE128StringW(const Str: WideString; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE128Init(Context, DigestByteLength);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str)); // SHAKE128UpdateW = SHA3UpdateW
  SHAKE128Final(Context, Result);
end;

// �� AnsiString �������ݽ��� SHAKE256 ����
function SHAKE256StringA(const Str: AnsiString; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE256Init(Context, DigestByteLength);
  SHAKE256Update(Context, PAnsiChar(Str), Length(Str));
  SHAKE256Final(Context, Result);
end;

// �� WideString �������ݽ��� SHAKE256 ����
function SHAKE256StringW(const Str: WideString; DigestByteLength: Cardinal): TBytes;
var
  Context: TCnSHA3Context;
begin
  SHAKE256Init(Context, DigestByteLength);
  SHA3UpdateW(Context, PWideChar(Str), Length(Str)); // SHAKE256UpdateW = SHA3UpdateW
  SHAKE256Final(Context, Result);
end;

// SHA3Type ֻ���� stSHA3_224, stSHA3_256, stSHA3_384, stSHA3_512
function InternalSHA3Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TCnSHA3GeneralDigest; SHA3Type: TSHA3Type; CallBack: TCnSHA3CalcProgressFunc): Boolean; overload;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
  Context: TCnSHA3Context;
begin
  Result := False;
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then
    Exit;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  SHA3Init(Context, SHA3Type);

  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        SHA3Update(Context, Buf, ReadBytes);

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    SHA3Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// SHA3Type ֻ���� stSHAKE128 �� stSHAKE256
function InternalSHA3Stream(Stream: TStream; const BufSize: Cardinal;
  SHA3Type: TSHA3Type; DigestByteLength: Cardinal; out D: TBytes;
  CallBack: TCnSHA3CalcProgressFunc): Boolean; overload;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
  Context: TCnSHA3Context;
begin
  Result := False;
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then
    Exit;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  SHA3Init(Context, SHA3Type, DigestByteLength);

  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        SHA3Update(Context, Buf, ReadBytes);

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    SHA3Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// ��ָ�������� SHA3_224 ����
function SHA3_224Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_224Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, STREAM_BUF_SIZE, Dig, stSHA3_224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_224Digest));
end;

// ��ָ�������� SHA3_256 ����
function SHA3_256Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_256Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, STREAM_BUF_SIZE, Dig, stSHA3_256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_256Digest));
end;

// ��ָ�������� SHA3_384 ����
function SHA3_384Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_384Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, STREAM_BUF_SIZE, Dig, stSHA3_384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_384Digest));
end;

// ��ָ�������� SHA3_512 ����
function SHA3_512Stream(Stream: TStream; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_512Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  InternalSHA3Stream(Stream, STREAM_BUF_SIZE, Dig, stSHA3_512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_512Digest));
end;

// ��ָ�������������Ӵճ��ȿɱ�� SHAKE128 ����
function SHAKE128Stream(Stream: TStream; DigestByteLength: Cardinal;
  CallBack: TCnSHA3CalcProgressFunc): TBytes;
begin
  InternalSHA3Stream(Stream, STREAM_BUF_SIZE, stSHAKE128, DigestByteLength, Result, CallBack);
end;

// ��ָ�������������Ӵճ��ȿɱ�� SHAKE256 ����
function SHAKE256Stream(Stream: TStream; DigestByteLength: Cardinal;
  CallBack: TCnSHA3CalcProgressFunc): TBytes;
begin
  InternalSHA3Stream(Stream, STREAM_BUF_SIZE, stSHAKE256, DigestByteLength, Result, CallBack);
end;

function FileSizeIsLargeThanMaxOrCanNotMap(const AFileName: string; out IsEmpty: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  H: THandle;
  Info: BY_HANDLE_FILE_INFORMATION;
  Rec: Int64Rec;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := False;
  IsEmpty := False;
  H := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  if H = INVALID_HANDLE_VALUE then
    Exit;
  try
    if not GetFileInformationByHandle(H, Info) then
      Exit;
  finally
    CloseHandle(H);
  end;
  Rec.Lo := Info.nFileSizeLow;
  Rec.Hi := Info.nFileSizeHigh;
  Result := (Rec.Hi > 0) or (Rec.Lo > MAX_FILE_SIZE);
  IsEmpty := (Rec.Hi = 0) and (Rec.Lo = 0);
{$ELSE}
  Result := True; // �� Windows ƽ̨���� True����ʾ�� Mapping
{$ENDIF}
end;

function InternalSHA3File(const FileName: string; SHA3Type: TSHA3Type;
  CallBack: TCnSHA3CalcProgressFunc): TCnSHA3GeneralDigest; overload;
var
{$IFDEF MSWINDOWS}
  Context: TCnSHA3Context;
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;
begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSHA3Stream(Stream, STREAM_BUF_SIZE, Result, SHA3Type, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    SHA3Init(Context, SHA3Type);
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
      FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
      FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
        if MapHandle <> 0 then
        begin
          try
            ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
            if ViewPointer <> nil then
            begin
              try
                SHA3Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
              finally
                UnmapViewOfFile(ViewPointer);
              end;
            end
            else
            begin
              raise Exception.Create(SCnErrorMapViewOfFile + IntToStr(GetLastError));
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end
        else
        begin
          if not FileIsZeroSize then
            raise Exception.Create(SCnErrorCreateFileMapping + IntToStr(GetLastError));
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
    SHA3Final(Context, Result);
{$ENDIF}
  end;
end;

function InternalSHA3File(const FileName: string; SHA3Type: TSHA3Type;
  DigestByteLength: Cardinal; CallBack: TCnSHA3CalcProgressFunc): TBytes; overload;
var
{$IFDEF MSWINDOWS}
  Context: TCnSHA3Context;
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;
begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSHA3Stream(Stream, STREAM_BUF_SIZE, SHA3Type, DigestByteLength, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    SHA3Init(Context, SHA3Type, DigestByteLength);
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
      FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
      FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
        if MapHandle <> 0 then
        begin
          try
            ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
            if ViewPointer <> nil then
            begin
              try
                SHA3Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
              finally
                UnmapViewOfFile(ViewPointer);
              end;
            end
            else
            begin
              raise Exception.Create(SCnErrorMapViewOfFile + IntToStr(GetLastError));
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end
        else
        begin
          if not FileIsZeroSize then
            raise Exception.Create(SCnErrorCreateFileMapping + IntToStr(GetLastError));
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
    SHA3Final(Context, Result);
{$ENDIF}
  end;
end;

// ��ָ���ļ����ݽ��� SHA3_224 ����
function SHA3_224File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_224Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_224Digest));
end;

// ��ָ���ļ����ݽ��� SHA3_256 ����
function SHA3_256File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_256Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_256Digest));
end;

// ��ָ���ļ����ݽ��� SHA3_384 ����
function SHA3_384File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_384Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_384Digest));
end;

// ��ָ���ļ����ݽ��� SHA3_512 ����
function SHA3_512File(const FileName: string; CallBack: TCnSHA3CalcProgressFunc):
  TCnSHA3_512Digest;
var
  Dig: TCnSHA3GeneralDigest;
begin
  Dig := InternalSHA3File(FileName, stSHA3_512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA3_512Digest));
end;

// ��ָ���ļ����ݽ����Ӵճ��ȿɱ�� SHAKE128 ����
function SHAKE128File(const FileName: string; DigestByteLength: Cardinal;
  CallBack: TCnSHA3CalcProgressFunc): TBytes;
begin
  Result := InternalSHA3File(FileName, stSHAKE128, DigestByteLength, CallBack);
end;

// ��ָ���ļ����ݽ����Ӵճ��ȿɱ�� SHAKE256 ����
function SHAKE256File(const FileName: string; DigestByteLength: Cardinal;
  CallBack: TCnSHA3CalcProgressFunc): TBytes;
begin
  Result := InternalSHA3File(FileName, stSHAKE256, DigestByteLength, CallBack);
end;

// ��ʮ�����Ƹ�ʽ��� SHA3_224 �Ӵ�ֵ
function SHA3_224Print(const Digest: TCnSHA3_224Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA3_224Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHA3_256 �Ӵ�ֵ
function SHA3_256Print(const Digest: TCnSHA3_256Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA3_256Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHA3_384 �Ӵ�ֵ
function SHA3_384Print(const Digest: TCnSHA3_384Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA3_384Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHA3_512 �Ӵ�ֵ
function SHA3_512Print(const Digest: TCnSHA3_512Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA3_512Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHAKE128 �Ӵ�ֵ
function SHAKE128Print(const Digest: TBytes): string;
begin
  Result := BytesToHex(Digest);
end;

// ��ʮ�����Ƹ�ʽ��� SHAKE256 �Ӵ�ֵ
function SHAKE256Print(const Digest: TBytes): string;
begin
  Result := BytesToHex(Digest);
end;

// �Ƚ����� SHA3_224 �Ӵ�ֵ�Ƿ����
function SHA3_224Match(const D1, D2: TCnSHA3_224Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnSHA3_224Digest));
end;

// �Ƚ����� SHA3_256 �Ӵ�ֵ�Ƿ����
function SHA3_256Match(const D1, D2: TCnSHA3_256Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnSHA3_256Digest));
end;

// �Ƚ����� SHA3_384 �Ӵ�ֵ�Ƿ����
function SHA3_384Match(const D1, D2: TCnSHA3_384Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnSHA3_384Digest));
end;

// �Ƚ����� SHA3_512 �Ӵ�ֵ�Ƿ����
function SHA3_512Match(const D1, D2: TCnSHA3_512Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnSHA3_512Digest));;
end;

// �Ƚ����� SHAKE128 �Ӵ�ֵ�Ƿ����
function SHAKE128Match(const D1, D2: TBytes): Boolean;
begin
  Result := CompareBytes(D1, D2);
end;

// �Ƚ����� SHAKE256 �Ӵ�ֵ�Ƿ����
function SHAKE256Match(const D1, D2: TBytes): Boolean;
begin
  Result := CompareBytes(D1, D2);
end;

// SHA3_224 �Ӵ�ֵת string
function SHA3_224DigestToStr(const Digest: TCnSHA3_224Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA3_224Digest));
end;

// SHA3_256 �Ӵ�ֵת string
function SHA3_256DigestToStr(const Digest: TCnSHA3_256Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA3_256Digest));;
end;

// SHA3_384 �Ӵ�ֵת string
function SHA3_384DigestToStr(const Digest: TCnSHA3_384Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA3_384Digest));
end;

// SHA3_512 �Ӵ�ֵת string
function SHA3_512DigestToStr(const Digest: TCnSHA3_512Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA3_512Digest));
end;

// SHAKE128 �Ӵ�ֵת string
function SHAKE128DigestToStr(const Digest: TBytes): string;
begin
  Result := BytesToString(Digest);
end;

// SHAKE256 �Ӵ�ֵת string
function SHAKE256DigestToStr(const Digest: TBytes): string;
begin
  Result := BytesToString(Digest);
end;

procedure SHA3_224HmacInit(var Context: TCnSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA3_224Digest;
begin
  if KeyLength > HMAC_SHA3_224_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_224Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_224_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_224_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_224_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_224_BLOCK_SIZE_BYTE);
end;

procedure SHA3_256HmacInit(var Context: TCnSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA3_256Digest;
begin
  if KeyLength > HMAC_SHA3_256_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_256Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_256_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_256_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_256_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_256_BLOCK_SIZE_BYTE);
end;

procedure SHA3_384HmacInit(var Context: TCnSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA3_384Digest;
begin
  if KeyLength > HMAC_SHA3_384_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_384Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_384_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_384_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_384_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_384_BLOCK_SIZE_BYTE);
end;

procedure SHA3_512HmacInit(var Context: TCnSHA3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA3_512Digest;
begin
  if KeyLength > HMAC_SHA3_512_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA3_512Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA3_512_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA3_512_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA3_512_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, @(Context.Ipad[0]), HMAC_SHA3_512_BLOCK_SIZE_BYTE);
end;

procedure SHA3_224HmacUpdate(var Context: TCnSHA3Context; Input: PAnsiChar;
  ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_256HmacUpdate(var Context: TCnSHA3Context; Input: PAnsiChar;
  ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_384HmacUpdate(var Context: TCnSHA3Context; Input: PAnsiChar;
  ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_512HmacUpdate(var Context: TCnSHA3Context; Input: PAnsiChar;
  ByteLength: Cardinal);
begin
  SHA3Update(Context, Input, ByteLength);
end;

procedure SHA3_224HmacFinal(var Context: TCnSHA3Context; var Output: TCnSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TCnSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_224_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_224);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_224_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_256HmacFinal(var Context: TCnSHA3Context; var Output: TCnSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TCnSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_256_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_256);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_256_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_384HmacFinal(var Context: TCnSHA3Context; var Output: TCnSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TCnSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_384_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_384);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_384_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_512HmacFinal(var Context: TCnSHA3Context; var Output: TCnSHA3GeneralDigest);
var
  Len: Integer;
  TmpBuf: TCnSHA3GeneralDigest;
begin
  Len := HMAC_SHA3_512_OUTPUT_LENGTH_BYTE;
  SHA3Final(Context, TmpBuf);
  SHA3Init(Context, stSHA3_512);
  SHA3Update(Context, @(Context.Opad[0]), HMAC_SHA3_512_BLOCK_SIZE_BYTE);
  SHA3Update(Context, @(TmpBuf[0]), Len);
  SHA3Final(Context, Output);
end;

procedure SHA3_224Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_224Digest);
var
  Context: TCnSHA3Context;
  Dig: TCnSHA3GeneralDigest;
begin
  SHA3_224HmacInit(Context, Key, KeyByteLength);
  SHA3_224HmacUpdate(Context, Input, ByteLength);
  SHA3_224HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

procedure SHA3_256Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_256Digest);
var
  Context: TCnSHA3Context;
  Dig: TCnSHA3GeneralDigest;
begin
  SHA3_256HmacInit(Context, Key, KeyByteLength);
  SHA3_256HmacUpdate(Context, Input, ByteLength);
  SHA3_256HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

procedure SHA3_384Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_384Digest);
var
  Context: TCnSHA3Context;
  Dig: TCnSHA3GeneralDigest;
begin
  SHA3_384HmacInit(Context, Key, KeyByteLength);
  SHA3_384HmacUpdate(Context, Input, ByteLength);
  SHA3_384HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

procedure SHA3_512Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA3_512Digest);
var
  Context: TCnSHA3Context;
  Dig: TCnSHA3GeneralDigest;
begin
  SHA3_512HmacInit(Context, Key, KeyByteLength);
  SHA3_512HmacUpdate(Context, Input, ByteLength);
  SHA3_512HmacFinal(Context, Dig);
  Move(Dig[0], Output[0], Context.DigestLen);
end;

end.
