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

unit CnBLAKE2;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�BLAKE �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
*           �ο� https://github.com/BLAKE2/BLAKE2 �� C ������ֲ�����䲿�ֹ���
* ��    ע������Ԫ�ο� RFC 7693 ʵ���� BLAKE2 ϵ���Ӵ��㷨 2S/2B��
*           ע�⣬��Ϊ BLAKE2 �ڲ������ Key ֵ������������� HMAC ʵ�֡�
* ����ƽ̨��PWin7 + Delphi 7.0
* ���ݲ��ԣ�PWinXP/7/10/11 + Delphi 5/6/7 ~ D12
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.06.15 V1.0
*               ������Ԫ��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} CnNative, CnConsts;

const
  CN_BLAKE2S_BLOCKBYTES    = 64;
  {* BLAKE2S ���С��64 �ֽ�}
  CN_BLAKE2S_OUTBYTES      = 32;
  {* BLAKE2S �Ӵս�����ȣ�32 �ֽ�}
  CN_BLAKE2S_KEYBYTES      = 32;
  {* BLAKE2S ���볤�ȣ�32 �ֽ�}
  CN_BLAKE2S_SALTBYTES     = 8;
  {* BLAKE2S �γ��ȣ�8 �ֽ�}
  CN_BLAKE2S_PERSONALBYTES = 8;
  {* BLAKE2S �ڲ����ȣ�8 �ֽ�}

  CN_BLAKE2B_BLOCKBYTES    = 128;
  {* BLAKE2S ���С��128 �ֽ�}
  CN_BLAKE2B_OUTBYTES      = 64;
  {* BLAKE2B �Ӵս����С��64 �ֽ�}
  CN_BLAKE2B_KEYBYTES      = 64;
  {* BLAKE2B ���볤�ȣ�64 �ֽ�}
  CN_BLAKE2B_SALTBYTES     = 16;
  {* BLAKE2B �γ��ȣ�16 �ֽ�}
  CN_BLAKE2B_PERSONALBYTES = 16;
  {* BLAKE2B �ڲ����ȣ�16 �ֽ�}

type
  ECnBLAKE2Exception = class(Exception);
  {* BLAKE2 ����쳣}

  PCnBLAKE2SDigest = ^TCnBLAKE2SDigest;
  {* BLAKE2S �Ӵս��ָ��}
  TCnBLAKE2SDigest = array[0..CN_BLAKE2S_OUTBYTES - 1] of Byte;
  {* BLAKE2S �Ӵս����� 32 �ֽ�}

  PCnBLAKE2BDigest = ^TCnBLAKE2BDigest;
  {* BLAKE2B �Ӵս��ָ��}
  TCnBLAKE2BDigest = array[0..CN_BLAKE2B_OUTBYTES - 1] of Byte;
  {* BLAKE2B �Ӵս����� 64 �ֽ�}

  TCnBLAKE2SContext = packed record
  {* BLAKE2S �������Ľṹ}
    H: array[0..7] of Cardinal;
    T: array[0..1] of Cardinal;
    F: array[0..1] of Cardinal;
    Buf: array[0..CN_BLAKE2S_BLOCKBYTES - 1] of Byte;
    BufLen: Integer;
    OutLen: Integer;
    Last: Byte;
  end;

  TCnBLAKE2BContext = packed record
  {* BLAKE2B �������Ľṹ}
    H: array[0..7] of TUInt64;
    T: array[0..1] of TUInt64;
    F: array[0..1] of TUInt64;
    Buf: array[0..CN_BLAKE2B_BLOCKBYTES - 1] of Byte;
    BufLen: Integer;
    OutLen: Integer;
    Last: Byte;
  end;

  TCnBLAKE2CalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* ���� BLAKE2 ϵ���Ӵս��Ȼص��¼���������}

function BLAKE2S(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar = nil;
  KeyLength: Integer = 0; DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* �����ݿ���� BLAKE2S ���㡣ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     Key: PAnsiChar                       - BLAKE2S ��Կ��Ĭ��Ϊ��
     KeyLength: Integer                   - BLAKE2S ��Կ�ֽڳ��ȣ�Ĭ��Ϊ 0
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest               - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2B(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar = nil;
  KeyLength: Integer = 0; DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* �����ݿ���� BLAKE2B ���㡣ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     Key: PAnsiChar                       - BLAKE2B ��Կ��Ĭ��Ϊ��
     KeyLength: Integer                   - BLAKE2B ��Կ�ֽڳ��ȣ�Ĭ��Ϊ 0
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

function BLAKE2SBuffer(const Buffer; Count: Cardinal; const Key; KeyCount: Cardinal;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* �����ݿ���� BLAKE2S ���㡣ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���
     const Key                            - BLAKE2S ��Կ��ַ
     KeyCount: Cardinal                   - BLAKE2S ��Կ�ֽڳ���
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest              - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2BBuffer(const Buffer; Count: Cardinal; const Key; KeyCount: Cardinal;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* �����ݿ���� BLAKE2B ���㡣ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���
     const Key                            - BLAKE2B ��Կ��ַ
     KeyCount: Cardinal                   - BLAKE2B ��Կ�ֽڳ���
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

function BLAKE2SBytes(Data: TBytes; Key: TBytes = nil; DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* ���ֽ�������� BLAKE2S ���㡣ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     Data: TBytes                         - ��������ֽ�����
     Key: TBytes                          - BLAKE2S ��Կ�ֽ����飬Ĭ��Ϊ��
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest               - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2BBytes(Data: TBytes; Key: TBytes = nil; DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* ���ֽ�������� BLAKE2B ���㡣ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     Data: TBytes                         - ��������ֽ�����
     Key: TBytes                          - BLAKE2B ��Կ�ֽ����飬Ĭ��Ϊ��
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

function BLAKE2SString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* �� String �������ݽ��� BLAKE2S ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     const Str: string                    - ��������ַ���
     const Key: string                    - BLAKE2S ��Կ���ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest               - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2BString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* �� String �������ݽ��� BLAKE2B ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     const Str: string                    - ��������ַ���
     const Key: string                    - BLAKE2B ��Կ���ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

function BLAKE2SStringA(const Str: AnsiString; const Key: AnsiString = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* �� AnsiString �������ݽ��� BLAKE2S ���㡣ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     const Str: AnsiString                - ��������ַ���
     const Key: AnsiString                - BLAKE2S ��Կ���ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest               - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2SStringW(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* �� WideString �������ݽ��� BLAKE2S ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣
   ע�⵱ Key �ǿ�ʱת����ĳ��Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     const Str: WideString                - ������Ŀ��ַ���
     const Key: WideString                - BLAKE2S ��Կ�Ŀ��ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest               - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2BStringA(const Str: AnsiString; const Key: AnsiString = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* �� AnsiString �������ݽ��� BLAKE2B ���㡣ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     const Str: AnsiString                - ��������ַ���
     const Key: AnsiString                - BLAKE2B ��Կ���ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

function BLAKE2BStringW(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* �� WideString �������ݽ��� BLAKE2B ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣
   ע�⵱ Key �ǿ�ʱת����ĳ��Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     const Str: WideString                - ������Ŀ��ַ���
     const Key: WideString                - BLAKE2B ��Կ�Ŀ��ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

{$IFDEF UNICODE}

function BLAKE2SUnicodeString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE2S ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     const Str: string                    - ������Ŀ��ַ���
     const Key: string                    - BLAKE2S ��Կ�Ŀ��ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest               - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2BUnicodeString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE2S ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     const Str: string                    - ������Ŀ��ַ���
     const Key: string                    - BLAKE2B ��Կ�Ŀ��ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

{$ELSE}

function BLAKE2SUnicodeString(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE2S ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     const Str: WideString                - ������Ŀ��ַ���
     const Key: WideString                - BLAKE2S ��Կ�Ŀ��ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��TCnBLAKE2SDigest               - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2BUnicodeString(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE2S ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����
   ע�⵱ Key �ǿ�ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     const Str: WideString                - ������Ŀ��ַ���
     const Key: WideString                - BLAKE2B ��Կ�Ŀ��ַ�����ʽ
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��TCnBLAKE2BDigest               - ���ص� BLAKE2B �Ӵ�ֵ
}

{$ENDIF}

function BLAKE2SFile(const FileName: string; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc =
  nil): TCnBLAKE2SDigest;
{* ��ָ���ļ����ݽ��� BLAKE2S ���㡣

   ������
     const FileName: string                - ��������ļ���
     Key: TBytes                           - BLAKE2S ��Կ�ֽ����飬Ĭ��Ϊ��
     DigestLength: Integer                 - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32
     CallBack: TCnBLAKE2CalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE2SDigest                - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2SStream(Stream: TStream; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc = nil):
  TCnBLAKE2SDigest;
{* ��ָ�������ݽ��� BLAKE2S ���㡣

   ������
     Stream: TStream                       - �������������
     Key: TBytes                           - BLAKE2S ��Կ�ֽ����飬Ĭ��Ϊ��
     DigestLength: Integer                 - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32
     CallBack: TCnBLAKE2CalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE2SDigest                - ���ص� BLAKE2S �Ӵ�ֵ
}

function BLAKE2BFile(const FileName: string; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc =
  nil): TCnBLAKE2BDigest;
{* ��ָ���ļ����ݽ��� BLAKE2B ���㡣

   ������
     const FileName: string                - ��������ļ���
     Key: TBytes                           - BLAKE2B ��Կ�ֽ����飬Ĭ��Ϊ��
     DigestLength: Integer                 - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32
     CallBack: TCnBLAKE2CalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE2BDigest                - ���ص� BLAKE2B �Ӵ�ֵ
}

function BLAKE2BStream(Stream: TStream; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc = nil):
  TCnBLAKE2BDigest;
{* ��ָ�������ݽ��� BLAKE2B ���㡣

   ������
     Stream: TStream                       - �������������
     Key: TBytes                           - BLAKE2B ��Կ�ֽ����飬Ĭ��Ϊ��
     DigestLength: Integer                 - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32
     CallBack: TCnBLAKE2CalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE2BDigest                - ���ص� BLAKE2B �Ӵ�ֵ
}

// �������ຯ�������ⲿ���������ݽ�����ɢ�� BLAKE2S ���㣬BLAKE2SUpdate �ɶ�α�����

procedure BLAKE2SInit(var Context: TCnBLAKE2SContext; Key: PAnsiChar = nil; KeyLength: Integer = 0;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES);
{* ��ʼ��һ�� BLAKE2S ���������ģ�׼������ BLAKE2S �����ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 32 �ֽڡ�

   ������
     var Context: TCnBLAKE2SContext       - ����ʼ���� BLAKE2S ������
     Key: PAnsiChar                       - BLAKE2S ��Կ��Ĭ��Ϊ��
     KeyLength: Integer                   - BLAKE2S ��Կ�ֽڳ��ȣ�Ĭ��Ϊ 0
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 32

   ����ֵ�����ޣ�
}

procedure BLAKE2SUpdate(var Context: TCnBLAKE2SContext; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� BLAKE2S ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnBLAKE2SContext       - BLAKE2S ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BLAKE2SFinal(var Context: TCnBLAKE2SContext; var Digest: TCnBLAKE2SDigest);
{* �������ּ��㣬�� BLAKE2S ��������� Digest �С�

   ������
     var Context: TCnBLAKE2SContext       - BLAKE2S ������
     var Digest: TCnBLAKE2SDigest         - ���ص� BLAKE2S �Ӵ�ֵ��ע�����ֻ��ǰ OutLength ���ֽ���Ч

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� BLAKE2B ���㣬BLAKE2BUpdate �ɶ�α�����

procedure BLAKE2BInit(var Context: TCnBLAKE2BContext; Key: PAnsiChar = nil; KeyLength: Integer = 0;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES);
{* ��ʼ��һ�� BLAKE2B ���������ģ�׼������ BLAKE2B �����ע�⵱ Key ����ʱ���Ƚ��ضϻ� #0 Ϊ 64 �ֽڡ�

   ������
     var Context: TCnBLAKE2BContext       - ����ʼ���� BLAKE2B ������
     Key: PAnsiChar                       - BLAKE2B ��Կ��Ĭ��Ϊ��
     KeyLength: Integer                   - BLAKE2B ��Կ�ֽڳ��ȣ�Ĭ��Ϊ 0
     DigestLength: Integer                - ָ�������ժҪ�ֽڳ��ȣ�Ĭ�� 64

   ����ֵ�����ޣ�
}

procedure BLAKE2BUpdate(var Context: TCnBLAKE2BContext; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� BLAKE2B ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnBLAKE2BContext       - BLAKE2B ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BLAKE2BFinal(var Context: TCnBLAKE2BContext; var Digest: TCnBLAKE2BDigest);
{* �������ּ��㣬�� BLAKE2B ��������� Digest �С�

   ������
     var Context: TCnBLAKE2BContext       - BLAKE2B ������
     var Digest: TCnBLAKE2BDigest         - ���ص� BLAKE2B �Ӵ�ֵ��ע�����ֻ��ǰ OutLength ���ֽ���Ч

   ����ֵ�����ޣ�
}

function BLAKE2SPrint(const Digest: TCnBLAKE2SDigest;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): string;
{* ��ʮ�����Ƹ�ʽ��� BLAKE2S �Ӵ�ֵ��

   ������
     const Digest: TCnBLAKE2SDigest       - ָ���� BLAKE2S �Ӵ�ֵ
     DigestLength: Integer                - �Ӵս���ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BLAKE2BPrint(const Digest: TCnBLAKE2BDigest;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): string;
{* ��ʮ�����Ƹ�ʽ��� BLAKE2B �Ӵ�ֵ��

   ������
     const Digest: TCnBLAKE2BDigest       - ָ���� BLAKE2B �Ӵ�ֵ
     DigestLength: Integer                - �Ӵս���ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BLAKE2SMatch(const D1, D2: TCnBLAKE2SDigest;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): Boolean;
{* �Ƚ����� BLAKE2S �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnBLAKE2SDigest           - ���Ƚϵ� BLAKE2S �Ӵ�ֵһ
     const D2: TCnBLAKE2SDigest           - ���Ƚϵ� BLAKE2S �Ӵ�ֵ��
     DigestLength: Integer                - �Ӵս���ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��Boolean                        - �����Ƿ����
}

function BLAKE2BMatch(const D1, D2: TCnBLAKE2BDigest;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): Boolean;
{* �Ƚ����� BLAKE2B �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnBLAKE2BDigest           - ���Ƚϵ� BLAKE2B �Ӵ�ֵһ
     const D2: TCnBLAKE2BDigest           - ���Ƚϵ� BLAKE2B �Ӵ�ֵ��
     DigestLength: Integer                - �Ӵս���ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��Boolean                        - �����Ƿ����
}

function BLAKE2SDigestToStr(const Digest: TCnBLAKE2SDigest;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): string;
{* BLAKE2S �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnBLAKE2SDigest       - ��ת���� BLAKE2S �Ӵ�ֵ
     DigestLength: Integer                - �Ӵս���ֽڳ��ȣ�Ĭ�� 32

   ����ֵ��string                         - ���ص��ַ���
}

function BLAKE2BDigestToStr(const Digest: TCnBLAKE2BDigest;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): string;
{* BLAKE2B �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnBLAKE2BDigest       - ��ת���� BLAKE2B �Ӵ�ֵ
     DigestLength: Integer                - �Ӵս���ֽڳ��ȣ�Ĭ�� 64

   ����ֵ��string                         - ���ص��ַ���
}

implementation

resourcestring
  SCnErrorBlake2InvalidKeySize = 'Invalid Key Length';
  SCnErrorBlake2InvalidDigestSize = 'Invalid Digest Length';

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  BLAKE2S_IV: array[0..7] of Cardinal = (
    $6A09E667, $BB67AE85, $3C6EF372, $A54FF53A,
    $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
  );

  BLAKE2B_IV: array[0..7] of TUInt64 = (
    $6A09E667F3BCC908, $BB67AE8584CAA73B,
    $3C6EF372FE94F82B, $A54FF53A5F1D36F1,
    $510E527FADE682D1, $9B05688C2B3E6C1F,
    $1F83D9ABFB41BD6B, $5BE0CD19137E2179
  );

  BLAKE2S_SIGMA: array[0..9, 0..15] of Byte = (
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
    ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
    (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
    (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
    (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
    ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
    ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
    (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
    ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 )
  );

  BLAKE2B_SIGMA: array[0..11, 0..15] of Byte = (
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
    ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
    (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
    (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
    (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
    ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
    ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
    (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
    ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 ),
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 )
  );

type
  TCnBLAKE2SParam = packed record
    DigestLength: Byte;
    KeyLength: Byte;
    FanOut: Byte;
    Depth: Byte;
    LeafLength: Cardinal;
    NodeOffset: Cardinal;
    XofLength: Word;
    NodeDepth: Byte;
    InnerLength: Byte;
    Salt: array[0..CN_BLAKE2S_SALTBYTES - 1] of Byte;
    Personal: array[0..CN_BLAKE2S_PERSONALBYTES - 1] of Byte;
  end;

  TCnBLAKE2BParam = packed record
    DigestLength: Byte;
    KeyLength: Byte;
    FanOut: Byte;
    Depth: Byte;
    LeafLength: Cardinal;
    NodeOffset: Cardinal;
    XofLength: Cardinal;
    NodeDepth: Byte;
    InnerLength: Byte;
    Reserved: array[0..13] of Byte; // ���� 32 ��
    Salt: array[0..CN_BLAKE2B_SALTBYTES - 1] of Byte;
    Personal: array[0..CN_BLAKE2B_PERSONALBYTES - 1] of Byte;
  end;

  PCnBLAKE2GeneralDigest = ^TCnBLAKE2GeneralDigest;
  TCnBLAKE2GeneralDigest = array[0..CN_BLAKE2B_OUTBYTES - 1] of Byte;

  TBLAKE2Type = (btBLAKE2B, btBLAKE2S);

function ROTRight256(A, B: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A shr B) or (A shl (32 - B));
end;

function ROTRight512(X: TUInt64; Y: Integer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X shr Y) or (X shl (64 - Y));
end;

procedure GS(MPtr: Pointer; R, I: Integer; var A, B, C, D: Cardinal);
var
  M: PCnUInt32Array;
begin
  M := PCnUInt32Array(MPtr);

  A := A + B + M[BLAKE2S_SIGMA[R][2 * I]];
  D := ROTRight256(D xor A, 16);
  C := C + D;
  B := ROTRight256(B xor C, 12);
  A := A + B + M[BLAKE2S_SIGMA[R][2 * I + 1]];
  D := ROTRight256(D xor A, 8);
  C := C + D;
  B := ROTRight256(B xor C, 7);
end;

procedure RoundS(MPtr, VPtr: Pointer; R: Integer);
var
  V: PCnUInt32Array;
begin
  V := PCnUInt32Array(VPtr);

  GS(MPtr, R, 0, V^[0], V^[4], V^[8], V^[12]);
  GS(MPtr, R, 1, V^[1], V^[5], V^[9], V^[13]);
  GS(MPtr, R, 2, V^[2], V^[6], V^[10], V^[14]);
  GS(MPtr, R, 3, V^[3], V^[7], V^[11], V^[15]);
  GS(MPtr, R, 4, V^[0], V^[5], V^[10], V^[15]);
  GS(MPtr, R, 5, V^[1], V^[6], V^[11], V^[12]);
  GS(MPtr, R, 6, V^[2], V^[7], V^[8], V^[13]);
  GS(MPtr, R, 7, V^[3], V^[4], V^[9], V^[14]);
end;

procedure GB(MPtr: Pointer; R, I: Integer; var A, B, C, D: TUInt64);
var
  M: PCnUInt64Array;
begin
  M := PCnUInt64Array(MPtr);

  A := A + B + M[BLAKE2B_SIGMA[R][2 * I]];
  D := ROTRight512(D xor A, 32);
  C := C + D;
  B := ROTRight512(B xor C, 24);
  A := A + B + M[BLAKE2B_SIGMA[R][2 * I + 1]];
  D := ROTRight512(D xor A, 16);
  C := C + D;
  B := ROTRight512(B xor C, 63);
end;

procedure RoundB(MPtr, VPtr: Pointer; R: Integer);
var
  V: PCnUInt64Array;
begin
  V := PCnUInt64Array(VPtr);

  GB(MPtr, R, 0, V^[0], V^[4], V^[8], V^[12]);
  GB(MPtr, R, 1, V^[1], V^[5], V^[9], V^[13]);
  GB(MPtr, R, 2, V^[2], V^[6], V^[10], V^[14]);
  GB(MPtr, R, 3, V^[3], V^[7], V^[11], V^[15]);
  GB(MPtr, R, 4, V^[0], V^[5], V^[10], V^[15]);
  GB(MPtr, R, 5, V^[1], V^[6], V^[11], V^[12]);
  GB(MPtr, R, 6, V^[2], V^[7], V^[8], V^[13]);
  GB(MPtr, R, 7, V^[3], V^[4], V^[9], V^[14]);
end;

procedure BLAKE2SCompress(var Context: TCnBLAKE2SContext; InPtr: Pointer);
var
  I: Integer;
  C: PCardinal;
  M, V: array[0..15] of Cardinal;
begin
  for I := 0 to 15 do
  begin
    C := PCardinal(TCnIntAddress(InPtr) + I * SizeOf(Cardinal));
    M[I] := UInt32ToLittleEndian(C^);
  end;

  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := BLAKE2S_IV[0];
  V[ 9] := BLAKE2S_IV[1];
  V[10] := BLAKE2S_IV[2];
  V[11] := BLAKE2S_IV[3];
  V[12] := Context.T[0] xor BLAKE2S_IV[4];
  V[13] := Context.T[1] xor BLAKE2S_IV[5];
  V[14] := Context.F[0] xor BLAKE2S_IV[6];
  V[15] := Context.F[1] xor BLAKE2S_IV[7];

  RoundS(@M[0], @V[0], 0);
  RoundS(@M[0], @V[0], 1);
  RoundS(@M[0], @V[0], 2);
  RoundS(@M[0], @V[0], 3);
  RoundS(@M[0], @V[0], 4);
  RoundS(@M[0], @V[0], 5);
  RoundS(@M[0], @V[0], 6);
  RoundS(@M[0], @V[0], 7);
  RoundS(@M[0], @V[0], 8);
  RoundS(@M[0], @V[0], 9);

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor V[I] xor V[I + 8];
end;

procedure BLAKE2BCompress(var Context: TCnBLAKE2BContext; InPtr: Pointer);
var
  I: Integer;
  C: PUInt64;
  M, V: array[0..15] of TUInt64;
begin
  for I := 0 to 15 do
  begin
    C := PUInt64(TCnIntAddress(InPtr) + I * SizeOf(TUInt64));
    M[I] := UInt64ToLittleEndian(C^);
  end;

  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := BLAKE2B_IV[0];
  V[ 9] := BLAKE2B_IV[1];
  V[10] := BLAKE2B_IV[2];
  V[11] := BLAKE2B_IV[3];
  V[12] := Context.T[0] xor BLAKE2B_IV[4];
  V[13] := Context.T[1] xor BLAKE2B_IV[5];
  V[14] := Context.F[0] xor BLAKE2B_IV[6];
  V[15] := Context.F[1] xor BLAKE2B_IV[7];

  RoundB(@M[0], @V[0], 0);
  RoundB(@M[0], @V[0], 1);
  RoundB(@M[0], @V[0], 2);
  RoundB(@M[0], @V[0], 3);
  RoundB(@M[0], @V[0], 4);
  RoundB(@M[0], @V[0], 5);
  RoundB(@M[0], @V[0], 6);
  RoundB(@M[0], @V[0], 7);
  RoundB(@M[0], @V[0], 8);
  RoundB(@M[0], @V[0], 9);
  RoundB(@M[0], @V[0], 10);
  RoundB(@M[0], @V[0], 11);

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor V[I] xor V[I + 8];
end;

procedure IncBLAKE2SCounter(var Context: TCnBLAKE2SContext; Step: Integer);
begin
  Context.T[0] := Context.T[0] + Cardinal(Step);
  if Context.T[0] < Cardinal(Step) then
    Inc(Context.T[1]);
end;

procedure IncBLAKE2BCounter(var Context: TCnBLAKE2BContext; Step: Integer);
begin
  Context.T[0] := Context.T[0] + Cardinal(Step);
  if Context.T[0] < TUInt64(Step) then
    Inc(Context.T[1]);
end;

// �������ຯ�������ⲿ���������ݽ�����ɢ�� BLAKE2S ���㣬BLAKE2SUpdate �ɶ�α�����

procedure BLAKE2SInit(var Context: TCnBLAKE2SContext; Key: PAnsiChar;
  KeyLength, DigestLength: Integer);
var
  I: Integer;
  B2SP: TCnBLAKE2SParam;
  P: PCnUInt32Array;
  B: array[0..CN_BLAKE2S_BLOCKBYTES - 1] of Byte;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  // ��ʼ���ṹ
  FillChar(Context, SizeOf(TCnBLAKE2SContext), 0);
  Context.OutLen := DigestLength;

  // ��ʼ��������
  FillChar(B2SP, SizeOf(TCnBLAKE2SParam), 0);

  if Key = nil then
    KeyLength := 0;
  if KeyLength > CN_BLAKE2S_KEYBYTES then
    KeyLength := CN_BLAKE2S_KEYBYTES;

  B2SP.DigestLength := DigestLength;
  B2SP.KeyLength := KeyLength;
  B2SP.FanOut := 1;
  B2SP.Depth := 1;

  // �ò�������ʼ���ṹ
  P := PCnUInt32Array(@B2SP);
  for I := 0 to 7 do
    Context.H[I] := BLAKE2S_IV[I] xor UInt32ToLittleEndian(P^[I]);

  // �� Key ���ȸ��ƽ�һ�������һ��
  if (Key <> nil) and (KeyLength > 0) then
  begin
    FillChar(B[0], SizeOf(B), 0);
    Move(Key^, B[0], KeyLength);
    BLAKE2SUpdate(Context, @B[0], CN_BLAKE2S_BLOCKBYTES);
  end;
end;

procedure BLAKE2SInitW(var Context: TCnBLAKE2SContext; Key: PWideChar;
  CharLength, DigestLength: Integer);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Key, CharLength, // ����ҳĬ���� 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    BLAKE2SInit(Context, Content, iLen, DigestLength);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Key);
  A := AnsiString(S);
  BLAKE2SInit(Context, @A[1], Length(A), DigestLength);
{$ENDIF}
end;

procedure BLAKE2SUpdate(var Context: TCnBLAKE2SContext; Input: PAnsiChar; ByteLength: Cardinal);
var
  Left, Fill: Cardinal;
begin
  Left := Context.BufLen;
  Fill := CN_BLAKE2S_BLOCKBYTES - Left;

  if (Input <> nil) and (ByteLength > 0) then
  begin
    if ByteLength > Fill then
    begin
      // �������ϻص�
      Context.BufLen := 0;
      Move(Input^, Context.Buf[Left], Fill);

      IncBLAKE2SCounter(Context, CN_BLAKE2S_BLOCKBYTES);
      BLAKE2SCompress(Context, @Context.Buf[0]);

      Inc(Input, Fill);
      Dec(ByteLength, Fill);

      // ѭ����������
      while ByteLength > CN_BLAKE2S_BLOCKBYTES do
      begin
        IncBLAKE2SCounter(Context, CN_BLAKE2S_BLOCKBYTES);
        BLAKE2SCompress(Context, Input);

        Inc(Input, CN_BLAKE2S_BLOCKBYTES);
        Dec(ByteLength, CN_BLAKE2S_BLOCKBYTES);
      end;
    end;

    // ���������ŵ��»ػ� Final
    Move(Input^, Context.Buf[Context.BufLen], ByteLength);
    Context.BufLen := Context.BufLen + Integer(ByteLength);
  end;
end;

procedure BLAKE2SUpdateW(var Context: TCnBLAKE2SContext;
  Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    BLAKE2SUpdate(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  BLAKE2SUpdate(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure BLAKE2SFinal(var Context: TCnBLAKE2SContext; var Digest: TCnBLAKE2SDigest);
var
  I: Integer;
  Dig: TCnBLAKE2SDigest;
  P: PCnUInt32Array;
begin
  IncBLAKE2SCounter(Context, Context.BufLen);

  // ���һ��û����Ĳ� 0 ����
  FillChar(Context.Buf[Context.BufLen], CN_BLAKE2S_BLOCKBYTES - Context.BufLen, 0);
  Context.F[0] := Cardinal(-1);
  BLAKE2SCompress(Context, @Context.Buf[0]);

  // ���ɽ��
  P := PCnUInt32Array(@Dig[0]);
  for I := 0 to 7 do
    P^[I] := UInt32ToLittleEndian(Context.H[I]);

  // �������
  FillChar(Digest[0], SizeOf(TCnBLAKE2SDigest), 0);
  Move(Dig[0], Digest[0], Context.OutLen);
end;

function BLAKE2S(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar;
  KeyLength, DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, Key, KeyLength, DigestLength);
  BLAKE2SUpdate(Context, Input, ByteLength);
  BLAKE2SFinal(Context, Result);
end;

function BLAKE2SBuffer(const Buffer; Count: Cardinal; const Key;
  KeyCount: Cardinal; DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, PAnsiChar(Key), KeyCount, DigestLength);
  BLAKE2SUpdate(Context, PAnsiChar(Buffer), Count);
  BLAKE2SFinal(Context, Result);
end;

// �������ຯ�������ⲿ���������ݽ�����ɢ�� BLAKE2S ���㣬BLAKE2SUpdate �ɶ�α�����

procedure BLAKE2BInit(var Context: TCnBLAKE2BContext; Key: PAnsiChar;
  KeyLength, DigestLength: Integer);
var
  I: Integer;
  B2BP: TCnBLAKE2BParam;
  P: PCnUInt64Array;
  B: array[0..CN_BLAKE2B_BLOCKBYTES - 1] of Byte;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  // ��ʼ���ṹ
  FillChar(Context, SizeOf(TCnBLAKE2BContext), 0);
  Context.OutLen := DigestLength;

  // ��ʼ��������
  FillChar(B2BP, SizeOf(TCnBLAKE2BParam), 0);

  if Key = nil then
    KeyLength := 0;
  if KeyLength > CN_BLAKE2B_KEYBYTES then
    KeyLength := CN_BLAKE2B_KEYBYTES;

  B2BP.DigestLength := DigestLength;
  B2BP.KeyLength := KeyLength;
  B2BP.FanOut := 1;
  B2BP.Depth := 1;

  // �ò�������ʼ���ṹ
  P := PCnUInt64Array(@B2BP);
  for I := 0 to 7 do
    Context.H[I] := BLAKE2B_IV[I] xor UInt64ToLittleEndian(P^[I]);

  // �� Key ���ȸ��ƽ�һ�������һ��
  if (Key <> nil) and (KeyLength > 0) then
  begin
    FillChar(B[0], SizeOf(B), 0);
    Move(Key^, B[0], KeyLength);
    BLAKE2BUpdate(Context, @B[0], CN_BLAKE2B_BLOCKBYTES);
  end;
end;

procedure BLAKE2BInitW(var Context: TCnBLAKE2BContext; Key: PWideChar;
  CharLength, DigestLength: Integer);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Key, CharLength, // ����ҳĬ���� 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    BLAKE2BInit(Context, Content, iLen, DigestLength);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Key);
  A := AnsiString(S);
  BLAKE2BInit(Context, @A[1], Length(A), DigestLength);
{$ENDIF}
end;

procedure BLAKE2BUpdate(var Context: TCnBLAKE2BContext; Input: PAnsiChar; ByteLength: Cardinal);
var
  Left, Fill: Cardinal;
begin
  Left := Context.BufLen;
  Fill := CN_BLAKE2B_BLOCKBYTES - Left;

  if (Input <> nil) and (ByteLength > 0) then
  begin
    if ByteLength > Fill then
    begin
      // �������ϻص�
      Context.BufLen := 0;
      Move(Input^, Context.Buf[Left], Fill);

      IncBLAKE2BCounter(Context, CN_BLAKE2B_BLOCKBYTES);
      BLAKE2BCompress(Context, @Context.Buf[0]);

      Inc(Input, Fill);
      Dec(ByteLength, Fill);

      // ѭ����������
      while ByteLength > CN_BLAKE2B_BLOCKBYTES do
      begin
        IncBLAKE2BCounter(Context, CN_BLAKE2B_BLOCKBYTES);
        BLAKE2BCompress(Context, Input);

        Inc(Input, CN_BLAKE2B_BLOCKBYTES);
        Dec(ByteLength, CN_BLAKE2B_BLOCKBYTES);
      end;
    end;

    // ���������ŵ��»ػ� Final
    Move(Input^, Context.Buf[Context.BufLen], ByteLength);
    Context.BufLen := Context.BufLen + Integer(ByteLength);
  end;
end;

procedure BLAKE2BUpdateW(var Context: TCnBLAKE2BContext;
  Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    BLAKE2BUpdate(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  BLAKE2BUpdate(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure BLAKE2BFinal(var Context: TCnBLAKE2BContext; var Digest: TCnBLAKE2BDigest);
var
  I: Integer;
  Dig: TCnBLAKE2BDigest;
  P: PCnUInt64Array;
begin
  IncBLAKE2BCounter(Context, Context.BufLen);

  // ���һ��û����Ĳ� 0 ����
  FillChar(Context.Buf[Context.BufLen], CN_BLAKE2B_BLOCKBYTES - Context.BufLen, 0);
  Context.F[0] := TUInt64(-1);
  BLAKE2BCompress(Context, @Context.Buf[0]);

  // ���ɽ��
  P := PCnUInt64Array(@Dig[0]);
  for I := 0 to 7 do
    P^[I] := UInt64ToLittleEndian(Context.H[I]);

  // �������
  FillChar(Digest[0], SizeOf(TCnBLAKE2BDigest), 0);
  Move(Dig[0], Digest[0], Context.OutLen);
end;

function BLAKE2B(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar;
  KeyLength, DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, Key, KeyLength, DigestLength);
  BLAKE2BUpdate(Context, Input, ByteLength);
  BLAKE2BFinal(Context, Result);
end;

function BLAKE2BBuffer(const Buffer; Count: Cardinal; const Key;
  KeyCount: Cardinal; DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, PAnsiChar(Key), KeyCount, DigestLength);
  BLAKE2BUpdate(Context, PAnsiChar(Buffer), Count);
  BLAKE2BFinal(Context, Result);
end;

function BLAKE2SBytes(Data: TBytes; Key: TBytes; DigestLength: Integer): TCnBLAKE2SDigest;
var
  D, K: PAnsiChar;
  DL, KL: Cardinal;
begin
  if (Data = nil) or (Length(Data) = 0) then
  begin
    D := nil;
    DL := 0;
  end
  else
  begin
    D := @Data[0];
    DL := Length(Data);
  end;

  if (Key = nil) or (Length(Key) = 0) then
  begin
    K := nil;
    KL := 0;
  end
  else
  begin
    K := @Key[0];
    KL := Length(Key);
  end;

  Result := BLAKE2S(D, DL, K, KL, DigestLength);
end;

function BLAKE2BBytes(Data: TBytes; Key: TBytes; DigestLength: Integer): TCnBLAKE2BDigest;
var
  D, K: PAnsiChar;
  DL, KL: Cardinal;
begin
  if (Data = nil) or (Length(Data) = 0) then
  begin
    D := nil;
    DL := 0;
  end
  else
  begin
    D := @Data[0];
    DL := Length(Data);
  end;

  if (Key = nil) or (Length(Key) = 0) then
  begin
    K := nil;
    KL := 0;
  end
  else
  begin
    K := @Key[0];
    KL := Length(Key);
  end;

  Result := BLAKE2B(D, DL, K, KL, DigestLength);
end;

function BLAKE2SString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2SDigest;
var
  AStr, AKey: AnsiString;
begin
  AStr := AnsiString(Str);
  AKey := AnsiString(Key);
  Result := BLAKE2SStringA(AStr, AKey, DigestLength);
end;

function BLAKE2BString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2BDigest;
var
  AStr, AKey: AnsiString;
begin
  AStr := AnsiString(Str);
  AKey := AnsiString(Key);
  Result := BLAKE2BStringA(AStr, AKey, DigestLength);
end;

function BLAKE2SStringA(const Str: AnsiString; const Key: AnsiString;
  DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, PAnsiChar(Key), Length(Key), DigestLength);
  BLAKE2SUpdate(Context, PAnsiChar(Str), Length(Str));
  BLAKE2SFinal(Context, Result);
end;

function BLAKE2SStringW(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInitW(Context, PWideChar(Key), Length(Key), DigestLength);
  BLAKE2SUpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE2SFinal(Context, Result);
end;

function BLAKE2BStringA(const Str: AnsiString; const Key: AnsiString;
  DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, PAnsiChar(Key), Length(Key), DigestLength);
  BLAKE2BUpdate(Context, PAnsiChar(Str), Length(Str));
  BLAKE2BFinal(Context, Result);
end;

function BLAKE2BStringW(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInitW(Context, PWideChar(Key), Length(Key), DigestLength);
  BLAKE2BUpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE2BFinal(Context, Result);
end;

{$IFDEF UNICODE}
function BLAKE2SUnicodeString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2SDigest;
{$ELSE}
function BLAKE2SUnicodeString(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2SDigest;
{$ENDIF}
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, PAnsiChar(@Key[1]), Length(Key) * SizeOf(WideChar), DigestLength);
  BLAKE2SUpdate(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE2SFinal(Context, Result);
end;

{$IFDEF UNICODE}
function BLAKE2BUnicodeString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2BDigest;
{$ELSE}
function BLAKE2BUnicodeString(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2BDigest;
{$ENDIF}
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, PAnsiChar(@Key[1]), Length(Key) * SizeOf(WideChar), DigestLength);
  BLAKE2BUpdate(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE2BFinal(Context, Result);
end;

function InternalBLAKE2Stream(Stream: TStream; Key: TBytes; DigestLength: Integer;
  const BufSize: Cardinal; var D: TCnBLAKE2GeneralDigest; BLAKE2Type: TBLAKE2Type;
  CallBack: TCnBLAKE2CalcProgressFunc): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
  KP: PAnsiChar;
  KL: Cardinal;

  Context2S: TCnBLAKE2SContext;
  Context2B: TCnBLAKE2BContext;
  Dig2S: TCnBLAKE2SDigest;
  Dig2B: TCnBLAKE2BDigest;

  procedure _BLAKE2Init;
  begin
    if (Key = nil) or (Length(Key) = 0) then
    begin
      KP := nil;
      KL := 0;
    end
    else
    begin
      KP := @Key[0];
      KL := Length(Key);
    end;

    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SInit(Context2S, KP, KL, DigestLength);
      btBLAKE2B:
        BLAKE2BInit(Context2B, KP, KL, DigestLength);
    end;
  end;

  procedure _BLAKE2Update;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SUpdate(Context2S, Buf, ReadBytes);
      btBLAKE2B:
        BLAKE2BUpdate(Context2B, Buf, ReadBytes);
    end;
  end;

  procedure _BLAKE2Final;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SFinal(Context2S, Dig2S);
      btBLAKE2B:
        BLAKE2BFinal(Context2B, Dig2B);
    end;
  end;

  procedure _CopyResult;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        Move(Dig2S[0], D[0], SizeOf(TCnBLAKE2SDigest));
      btBLAKE2B:
        Move(Dig2B[0], D[0], SizeOf(TCnBLAKE2BDigest));
    end;
  end;

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
  _BLAKE2Init;
 
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        _BLAKE2Update;

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    _BLAKE2Final;
    _CopyResult;
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

function BLAKE2SStream(Stream: TStream; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2SDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  InternalBLAKE2Stream(Stream, Key, DigestLength, 4096 * 1024, Dig, btBLAKE2S, CallBack);
  Move(Dig[0], Result[0], DigestLength);
end;

function BLAKE2BStream(Stream: TStream; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2BDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  InternalBLAKE2Stream(Stream, Key, DigestLength, 4096 * 1024, Dig, btBLAKE2B, CallBack);
  Move(Dig[0], Result[0], DigestLength);
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

function InternalBLAKE2File(const FileName: string; Key: TBytes; DigestLength: Integer;
  BLAKE2Type: TBLAKE2Type; CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2GeneralDigest;
var
  Context2S: TCnBLAKE2SContext;
  Context2B: TCnBLAKE2BContext;
  Dig2S: TCnBLAKE2SDigest;
  Dig2B: TCnBLAKE2BDigest;
  KP: PAnsiChar;
  KL: Cardinal;

{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  procedure _BLAKE2Init;
  begin
    if (Key = nil) or (Length(Key) = 0) then
    begin
      KP := nil;
      KL := 0;
    end
    else
    begin
      KP := @Key[0];
      KL := Length(Key);
    end;

    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SInit(Context2S, KP, KL, DigestLength);
      btBLAKE2B:
        BLAKE2BInit(Context2B, KP, KL, DigestLength);
    end;
  end;

{$IFDEF MSWINDOWS}
  procedure _BLAKE2Update;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SUpdate(Context2S, ViewPointer, GetFileSize(FileHandle, nil));
      btBLAKE2B:
        BLAKE2BUpdate(Context2B, ViewPointer, GetFileSize(FileHandle, nil));
    end;
  end;
{$ENDIF}

  procedure _BLAKE2Final;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SFinal(Context2S, Dig2S);
      btBLAKE2B:
        BLAKE2BFinal(Context2B, Dig2B);
    end;
  end;

  procedure _CopyResult(var D: TCnBLAKE2GeneralDigest);
  begin
    case BLAKE2Type of
      btBLAKE2S:
        Move(Dig2S[0], D[0], SizeOf(TCnBLAKE2SDigest));
      btBLAKE2B:
        Move(Dig2B[0], D[0], SizeOf(TCnBLAKE2BDigest));
    end;
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���� Windows ƽ̨����������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalBLAKE2Stream(Stream, Key, DigestLength, 4096 * 1024, Result, BLAKE2Type, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    _BLAKE2Init;
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
                _BLAKE2Update;
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
    _BLAKE2Final;
    _CopyResult(Result);
{$ENDIF}
  end;
end;

function BLAKE2SFile(const FileName: string; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2SDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Dig := InternalBLAKE2File(FileName, Key, DigestLength, btBLAKE2S, CallBack);
  Move(Dig[0], Result[0], DigestLength);
end;

function BLAKE2BFile(const FileName: string; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2BDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Dig := InternalBLAKE2File(FileName, Key, DigestLength, btBLAKE2B, CallBack);
  Move(Dig[0], Result[0], DigestLength);
end;

function BLAKE2SPrint(const Digest: TCnBLAKE2SDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := DataToHex(@Digest[0], DigestLength);
end;

function BLAKE2BPrint(const Digest: TCnBLAKE2BDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := DataToHex(@Digest[0], DigestLength);
end;

function BLAKE2SMatch(const D1, D2: TCnBLAKE2SDigest; DigestLength: Integer): Boolean;
var
  I: Integer;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  I := 0;
  Result := True;
  while Result and (I < DigestLength) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

function BLAKE2BMatch(const D1, D2: TCnBLAKE2BDigest; DigestLength: Integer): Boolean;
var
  I: Integer;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  I := 0;
  Result := True;
  while Result and (I < DigestLength) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

function BLAKE2SDigestToStr(const Digest: TCnBLAKE2SDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := MemoryToString(@Digest[0], DigestLength);
end;

function BLAKE2BDigestToStr(const Digest: TCnBLAKE2BDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := MemoryToString(@Digest[0], DigestLength);
end;

end.
