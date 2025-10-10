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

unit CnSHA2;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�SHA2 �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
*           ������/���� C ������ Pascal ��������ֲ���������䲿�ֹ��ܡ�
* ��    ע������Ԫʵ���� SHA2 ϵ���Ӵ��㷨����Ӧ�� HMAC �㷨������ SHA224/256/384/512��
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWinXP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2022.04.26 V1.4
*               �޸� LongWord �� Integer ��ַת����֧�� MacOS64
*           2019.04.15 V1.3
*               ֧�� Win32/Win64/MacOS32
*           2017.10.31 V1.2
*               ���� SHA512/384 HMAC ������������
*           2016.09.30 V1.1
*               ʵ�� SHA512/384��D567�����з��� Int64 �����޷��� UInt64
*           2016.09.27 V1.0
*               ������Ԫ��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative, CnConsts;

type
  PCnSHA2GeneralDigest = ^TCnSHA2GeneralDigest;
  TCnSHA2GeneralDigest = array[0..63] of Byte;

  PCnSHA224Digest = ^TCnSHA224Digest;
  TCnSHA224Digest = array[0..27] of Byte;
  {* SHA224 �Ӵս����28 �ֽ�}

  PCnSHA256Digest = ^TCnSHA256Digest;
  TCnSHA256Digest = array[0..31] of Byte;
  {* SHA256 �Ӵս����32 �ֽ�}

  PCnSHA384Digest = ^TCnSHA384Digest;
  TCnSHA384Digest = array[0..47] of Byte;
  {* SHA384 �Ӵս����48 �ֽ�}

  PCnSHA512Digest = ^TCnSHA512Digest;
  TCnSHA512Digest = array[0..63] of Byte;
  {* SHA512 �Ӵս����64 �ֽ�}

  TCnSHA256Context = packed record
  {* SHA256 �������Ľṹ}
    DataLen: Cardinal;
    Data: array[0..63] of Byte;
    BitLen: Int64;
    State: array[0..7] of Cardinal;
    Ipad: array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;

  TCnSHA224Context = TCnSHA256Context;
  {* SHA224 �������Ľṹ}

  TCnSHA512Context = packed record
  {* SHA512 �������Ľṹ}
    DataLen: Cardinal;
    Data: array[0..127] of Byte;
    TotalLen: Int64;
    State: array[0..7] of Int64;
    Ipad: array[0..127] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..127] of Byte;      {!< HMAC: outer padding        }
  end;

  TCnSHA384Context = TCnSHA512Context;
  {* SHA512 �������Ľṹ}

  TCnSHACalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* ���� SHA2 ϵ���Ӵս��Ȼص��¼���������}

function SHA224(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA224Digest;
{* �����ݿ���� SHA224 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA256Digest;
{* �����ݿ���� SHA256 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA384Digest;
{* �����ݿ���� SHA384 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA512Digest;
{* �����ݿ���� SHA512 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

function SHA224Buffer(const Buffer; Count: Cardinal): TCnSHA224Digest;
{* �����ݿ���� SHA224 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256Buffer(const Buffer; Count: Cardinal): TCnSHA256Digest;
{* �����ݿ���� SHA256 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384Buffer(const Buffer; Count: Cardinal): TCnSHA384Digest;
{* �����ݿ���� SHA384 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512Buffer(const Buffer; Count: Cardinal): TCnSHA512Digest;
{* �����ݿ���� SHA512 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

function SHA224Bytes(Data: TBytes): TCnSHA224Digest;
{* ���ֽ�������� SHA224 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256Bytes(Data: TBytes): TCnSHA256Digest;
{* ���ֽ�������� SHA256 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384Bytes(Data: TBytes): TCnSHA384Digest;
{* ���ֽ�������� SHA384 ���㡣
   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512Bytes(Data: TBytes): TCnSHA512Digest;
{* ���ֽ�������� SHA512 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

function SHA224String(const Str: string): TCnSHA224Digest;
{* �� String �������ݽ��� SHA224 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256String(const Str: string): TCnSHA256Digest;
{* �� String �������ݽ��� SHA256 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384String(const Str: string): TCnSHA384Digest;
{* �� String �������ݽ��� SHA384 ���㣬ע�� D2009�����ϰ汾��string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512String(const Str: string): TCnSHA512Digest;
{* �� String �������ݽ��� SHA512 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}


function SHA224StringA(const Str: AnsiString): TCnSHA224Digest;
{* �� AnsiString �������ݽ��� SHA224 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA224StringW(const Str: WideString): TCnSHA224Digest;
{* �� WideString �������ݽ��� SHA224 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256StringA(const Str: AnsiString): TCnSHA256Digest;
{* �� AnsiString �������ݽ��� SHA256 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA256StringW(const Str: WideString): TCnSHA256Digest;
{* �� WideString �������ݽ��� SHA256 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384StringA(const Str: AnsiString): TCnSHA384Digest;
{* �� AnsiString �������ݽ��� SHA384 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA384StringW(const Str: WideString): TCnSHA384Digest;
{* �� WideString �������ݽ��� SHA384 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512StringA(const Str: AnsiString): TCnSHA512Digest;
{* �� AnsiString �������ݽ��� SHA512 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

function SHA512StringW(const Str: WideString): TCnSHA512Digest;
{* �� WideString �������ݽ��� SHA512 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

{$IFDEF UNICODE}

function SHA224UnicodeString(const Str: string): TCnSHA224Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA224 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256UnicodeString(const Str: string): TCnSHA256Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384UnicodeString(const Str: string): TCnSHA384Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA384 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512UnicodeString(const Str: string): TCnSHA512Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA512 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

{$ELSE}

function SHA224UnicodeString(const Str: WideString): TCnSHA224Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA224 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256UnicodeString(const Str: WideString): TCnSHA256Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384UnicodeString(const Str: WideString): TCnSHA384Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA384 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512UnicodeString(const Str: WideString): TCnSHA512Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA512 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

{$ENDIF}

function SHA224File(const FileName: string; CallBack: TCnSHACalcProgressFunc =
  nil): TCnSHA224Digest;
{* ��ָ���ļ����ݽ��� SHA224 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA224Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc = nil):
  TCnSHA224Digest;
{* ��ָ�������ݽ��� SHA224 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA224Digest                - ���ص� SHA224 �Ӵ�ֵ
}

function SHA256File(const FileName: string; CallBack: TCnSHACalcProgressFunc =
  nil): TCnSHA256Digest;
{* ��ָ���ļ����ݽ��� SHA256 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA256Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc = nil):
  TCnSHA256Digest;
{* ��ָ�������ݽ��� SHA256 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA256Digest                - ���ص� SHA256 �Ӵ�ֵ
}

function SHA384File(const FileName: string; CallBack: TCnSHACalcProgressFunc =
  nil): TCnSHA384Digest;
{* ��ָ���ļ����ݽ��� SHA384 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA384Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc = nil):
  TCnSHA384Digest;
{* ��ָ�������ݽ��� SHA384 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA384Digest                - ���ص� SHA384�Ӵ�ֵ
}

function SHA512File(const FileName: string; CallBack: TCnSHACalcProgressFunc =
  nil): TCnSHA512Digest;
{* ��ָ���ļ����ݽ��� SHA512 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

function SHA512Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc = nil):
  TCnSHA512Digest;
{* ��ָ�������ݽ��� SHA512 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHACalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA512Digest                - ���ص� SHA512 �Ӵ�ֵ
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA224 ���㣬SHA224Update �ɶ�α�����

procedure SHA224Init(var Context: TCnSHA224Context);
{* ��ʼ��һ�� SHA224 ���������ģ�׼������ SHA224 �����

   ������
     var Context: TCnSHA224Context        - ����ʼ���� SHA224 ������

   ����ֵ�����ޣ�
}

procedure SHA224Update(var Context: TCnSHA224Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA224 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA224Context        - SHA224 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA224Final(var Context: TCnSHA224Context; var Digest: TCnSHA224Digest);
{* �������ּ��㣬�� SHA224 ��������� Digest �С�

   ������
     var Context: TCnSHA224Context        - SHA224 ������
     var Digest: TCnSHA224Digest          - ���ص� SHA224 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA256 ���㣬SHA256Update �ɶ�α�����

procedure SHA256Init(var Context: TCnSHA256Context);
{* ��ʼ��һ�� SHA256 ���������ģ�׼������ SHA256 �����

   ������
     var Context: TCnSHA256Context        - ����ʼ���� SHA256 ������

   ����ֵ�����ޣ�
}

procedure SHA256Update(var Context: TCnSHA256Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA256 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA256Context        - SHA256 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA256Final(var Context: TCnSHA256Context; var Digest: TCnSHA256Digest);
{* �������ּ��㣬�� SHA256 ��������� Digest �С�

   ������
     var Context: TCnSHA256Context        - SHA256 ������
     var Digest: TCnSHA256Digest          - ���ص� SHA256 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA384 ���㣬SHA384Update �ɶ�α�����

procedure SHA384Init(var Context: TCnSHA384Context);
{* ��ʼ��һ�� SHA384 ���������ģ�׼������ SHA384 �����

   ������
     var Context: TCnSHA384Context        - ����ʼ���� SHA384 ������

   ����ֵ�����ޣ�
}

procedure SHA384Update(var Context: TCnSHA384Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA384 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA384Context        - SHA384 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA384Final(var Context: TCnSHA384Context; var Digest: TCnSHA384Digest);
{* �������ּ��㣬�� SHA384 ��������� Digest �С�

   ������
     var Context: TCnSHA384Context        - SHA384 ������
     var Digest: TCnSHA384Digest          - ���ص� SHA384 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA512 ���㣬SHA512Update �ɶ�α�����

procedure SHA512Init(var Context: TCnSHA512Context);
{* ��ʼ��һ�� SHA512 ���������ģ�׼������ SHA512 �����

   ������
     var Context: TCnSHA512Context        - ����ʼ���� SHA512 ������

   ����ֵ�����ޣ�
}

procedure SHA512Update(var Context: TCnSHA512Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA512 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA512Context        - SHA512 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA512Final(var Context: TCnSHA512Context; var Digest: TCnSHA512Digest);
{* �������ּ��㣬�� SHA512 ��������� Digest �С�

   ������
     var Context: TCnSHA512Context        - SHA512 ������
     var Digest: TCnSHA512Digest          - ���ص� SHA512 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

function SHA224Print(const Digest: TCnSHA224Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA224 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA224Digest        - ָ���� SHA224 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA256Print(const Digest: TCnSHA256Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA256 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA256Digest        - ָ���� SHA256 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA384Print(const Digest: TCnSHA384Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA384 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA384Digest        - ָ���� SHA384 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA512Print(const Digest: TCnSHA512Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA512 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA512Digest        - ָ���� SHA512 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA224Match(const D1: TCnSHA224Digest; const D2: TCnSHA224Digest): Boolean;
{* �Ƚ����� SHA224 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA224Digest            - ���Ƚϵ� SHA224 �Ӵ�ֵһ
     const D2: TCnSHA224Digest            - ���Ƚϵ� SHA224 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA256Match(const D1: TCnSHA256Digest; const D2: TCnSHA256Digest): Boolean;
{* �Ƚ����� SHA256 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA256Digest            - ���Ƚϵ� SHA256 �Ӵ�ֵһ
     const D2: TCnSHA256Digest            - ���Ƚϵ� SHA256 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA384Match(const D1: TCnSHA384Digest; const D2: TCnSHA384Digest): Boolean;
{* �Ƚ����� SHA384 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA384Digest            - ���Ƚϵ� SHA384 �Ӵ�ֵһ
     const D2: TCnSHA384Digest            - ���Ƚϵ� SHA384 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA512Match(const D1: TCnSHA512Digest; const D2: TCnSHA512Digest): Boolean;
{* �Ƚ����� SHA512 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA512Digest            - ���Ƚϵ� SHA512 �Ӵ�ֵһ
     const D2: TCnSHA512Digest            - ���Ƚϵ� SHA512 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA224DigestToStr(const Digest: TCnSHA224Digest): string;
{* SHA224 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA224Digest        - ��ת���� SHA224 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHA256DigestToStr(const Digest: TCnSHA256Digest): string;
{* SHA256 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA256Digest        - ��ת���� SHA256 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHA384DigestToStr(const Digest: TCnSHA384Digest): string;
{* SHA384 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA384Digest        - ��ת���� SHA384 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function SHA512DigestToStr(const Digest: TCnSHA512Digest): string;
{* SHA512 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA512Digest        - ��ת���� SHA512 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

procedure SHA224Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA224Digest);
{* ���� SHA224 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA224 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA224 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA224Digest          - ���ص� SHA224 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure SHA256Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA256Digest);
{* ���� SHA256 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA256 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA256 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA256Digest          - ���ص� SHA256 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure SHA384Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA384Digest);
{* ���� SHA384 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA384 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA384 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA384Digest          - ���ص� SHA384 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure SHA512Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA512Digest);
{* ���� SHA512 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA512 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA512 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA512Digest          - ���ص� SHA512 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

implementation

const
  HMAC_SHA2_224_256_BLOCK_SIZE_BYTE = 64;
  HMAC_SHA2_384_512_BLOCK_SIZE_BYTE = 128;

  HMAC_SHA2_224_OUTPUT_LENGTH_BYTE = 28;
  HMAC_SHA2_256_OUTPUT_LENGTH_BYTE = 32;
  HMAC_SHA2_384_OUTPUT_LENGTH_BYTE = 48;
  HMAC_SHA2_512_OUTPUT_LENGTH_BYTE = 64;

type
  TSHA2Type = (stSHA2_224, stSHA2_256, stSHA2_384, stSHA2_512);

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  KEYS256: array[0..63] of Cardinal = ($428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
    $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5, $D807AA98, $12835B01, $243185BE,
    $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174, $E49B69C1, $EFBE4786,
    $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA, $983E5152,
    $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E,
    $92722C85, $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624,
    $F40E3585, $106AA070, $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3,
    $4ED8AA4A, $5B9CCA4F, $682E6FF3, $748F82EE, $78A5636F, $84C87814, $8CC70208,
    $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2);

  KEYS512: array[0..79] of TUInt64 = ($428A2F98D728AE22, $7137449123EF65CD,
    $B5C0FBCFEC4D3B2F, $E9B5DBA58189DBBC, $3956C25BF348B538, $59F111F1B605D019,
    $923F82A4AF194F9B, $AB1C5ED5DA6D8118, $D807AA98A3030242, $12835B0145706FBE,
    $243185BE4EE4B28C, $550C7DC3D5FFB4E2, $72BE5D74F27B896F, $80DEB1FE3B1696B1,
    $9BDC06A725C71235, $C19BF174CF692694, $E49B69C19EF14AD2, $EFBE4786384F25E3,
    $0FC19DC68B8CD5B5, $240CA1CC77AC9C65, $2DE92C6F592B0275, $4A7484AA6EA6E483,
    $5CB0A9DCBD41FBD4, $76F988DA831153B5, $983E5152EE66DFAB, $A831C66D2DB43210,
    $B00327C898FB213F, $BF597FC7BEEF0EE4, $C6E00BF33DA88FC2, $D5A79147930AA725,
    $06CA6351E003826F, $142929670A0E6E70, $27B70A8546D22FFC, $2E1B21385C26C926,
    $4D2C6DFC5AC42AED, $53380D139D95B3DF, $650A73548BAF63DE, $766A0ABB3C77B2A8,
    $81C2C92E47EDAEE6, $92722C851482353B, $A2BFE8A14CF10364, $A81A664BBC423001,
    $C24B8B70D0F89791, $C76C51A30654BE30, $D192E819D6EF5218, $D69906245565A910,
    $F40E35855771202A, $106AA07032BBD1B8, $19A4C116B8D2D0C8, $1E376C085141AB53,
    $2748774CDF8EEB99, $34B0BCB5E19B48A8, $391C0CB3C5C95A63, $4ED8AA4AE3418ACB,
    $5B9CCA4F7763E373, $682E6FF3D6B2B8A3, $748F82EE5DEFB2FC, $78A5636F43172F60,
    $84C87814A1F0AB72, $8CC702081A6439EC, $90BEFFFA23631E28, $A4506CEBDE82BDE9,
    $BEF9A3F7B2C67915, $C67178F2E372532B, $CA273ECEEA26619C, $D186B8C721C0C207,
    $EADA7DD6CDE0EB1E, $F57D4F7FEE6ED178, $06F067AA72176FBA, $0A637DC5A2C898A6,
    $113F9804BEF90DAE, $1B710B35131C471B, $28DB77F523047D84, $32CAAB7B40C72493,
    $3C9EBE0A15C9BEBC, $431D67C49C100D4C, $4CC5D4BECB3E42B6, $597F299CFC657E2A,
    $5FCB6FAB3AD6FAEC, $6C44198C4A475817);

function ROTRight256(A, B: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A shr B) or (A shl (32 - B));
end;

function ROTRight512(X: TUInt64; Y: Integer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X shr Y) or (X shl (64 - Y));
end;

function SHR512(X: TUInt64; Y: Integer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and $FFFFFFFFFFFFFFFF) shr Y;
end;

function CH256(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and Y) xor ((not X) and Z);
end;

function CH512(X, Y, Z: TUInt64): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (((Y xor Z) and X) xor Z);
end;

function MAJ256(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and Y) xor (X and Z) xor (Y and Z);
end;

function MAJ512(X, Y, Z: TUInt64): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ((X or Y) and Z) or (X and Y);
end;

function EP0256(X: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight256(X, 2) xor ROTRight256(X, 13) xor ROTRight256(X, 22);
end;

function EP1256(X: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight256(X, 6) xor ROTRight256(X, 11) xor ROTRight256(X, 25);
end;

function SIG0256(X: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight256(X, 7) xor ROTRight256(X, 18) xor (X shr 3);
end;

function SIG1256(X: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight256(X, 17) xor ROTRight256(X, 19) xor (X shr 10);
end;

function SIG0512(X: TUInt64): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight512(X, 28) xor ROTRight512(X, 34) xor ROTRight512(X, 39);
end;

function SIG1512(X: TUInt64): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight512(X, 14) xor ROTRight512(X, 18) xor ROTRight512(X, 41);
end;

function Gamma0512(X: TUInt64): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight512(X, 1) xor ROTRight512(X, 8) xor SHR512(X, 7);
end;

function Gamma1512(X: TUInt64): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := ROTRight512(X, 19) xor ROTRight512(X, 61) xor SHR512(X, 6);
end;

procedure SHA256Transform(var Context: TCnSHA256Context; Data: PAnsiChar);
var
  A, B, C, D, E, F, G, H, T1, T2: Cardinal;
  M: array[0..63] of Cardinal;
  I, J: Integer;
begin
  I := 0;
  J := 0;
  while I < 16 do
  begin
    M[I] := (Cardinal(Data[J]) shl 24) or (Cardinal(Data[J + 1]) shl 16) or (Cardinal(Data
      [J + 2]) shl 8) or Cardinal(Data[J + 3]);
    Inc(I);
    Inc(J, 4);
  end;

  while I < 64 do
  begin
    M[I] := SIG1256(M[I - 2]) + M[I - 7] + SIG0256(M[I - 15]) + M[I - 16];
    Inc(I);
  end;

  A := Context.State[0];
  B := Context.State[1];
  C := Context.State[2];
  D := Context.State[3];
  E := Context.State[4];
  F := Context.State[5];
  G := Context.State[6];
  H := Context.State[7];

  I := 0;
  while I < 64 do
  begin
    T1 := H + EP1256(E) + CH256(E, F, G) + KEYS256[I] + M[I];
    T2 := EP0256(A) + MAJ256(A, B, C);
    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
    Inc(I);
  end;

  Context.State[0] := Context.State[0] + A;
  Context.State[1] := Context.State[1] + B;
  Context.State[2] := Context.State[2] + C;
  Context.State[3] := Context.State[3] + D;
  Context.State[4] := Context.State[4] + E;
  Context.State[5] := Context.State[5] + F;
  Context.State[6] := Context.State[6] + G;
  Context.State[7] := Context.State[7] + H;
end;

{$WARNINGS OFF}

procedure SHA512Transform(var Context: TCnSHA512Context; Data: PAnsiChar; BlockCount: Integer);
var
  A, B, C, D, E, F, G, H, T1, T2: TUInt64;
  M: array[0..79] of TUInt64;
  I, J, K: Integer;
  OrigData: PAnsiChar;
begin
  OrigData := Data;
  for K := 0 to BlockCount - 1 do
  begin
    Data := PAnsiChar(TCnNativeInt(OrigData) + (K shl 7));

    I := 0;
    J := 0;
    while I < 16 do
    begin
      M[I] := (TUInt64(Data[J]) shl 56) or (TUInt64(Data[J + 1]) shl 48) or
        (TUInt64(Data[J + 2]) shl 40) or (TUInt64(Data[J + 3]) shl 32) or
        (TUInt64(Data[J + 4]) shl 24) or (TUInt64(Data[J + 5]) shl 16) or
        (TUInt64(Data[J + 6]) shl 8) or TUInt64(Data[J + 7]);
      Inc(I);
      Inc(J, 8);
    end;

    while I < 80 do
    begin
      M[I] := Gamma1512(M[I - 2]) + M[I - 7] + Gamma0512(M[I - 15]) + M[I - 16];
      Inc(I);
    end;

    A := Context.State[0];
    B := Context.State[1];
    C := Context.State[2];
    D := Context.State[3];
    E := Context.State[4];
    F := Context.State[5];
    G := Context.State[6];
    H := Context.State[7];

    I := 0;
    while I < 80 do
    begin
      T1 := H + SIG1512(E) + CH512(E, F, G) + KEYS512[I] + M[I];
      T2 := SIG0512(A) + MAJ512(A, B, C);
      H := G;
      G := F;
      F := E;
      E := D + T1;
      D := C;
      C := B;
      B := A;
      A := T1 + T2;
      Inc(I);
    end;

    // �����з����޷�������� Warning������Ӱ��
    Context.State[0] := Context.State[0] + A;
    Context.State[1] := Context.State[1] + B;
    Context.State[2] := Context.State[2] + C;
    Context.State[3] := Context.State[3] + D;
    Context.State[4] := Context.State[4] + E;
    Context.State[5] := Context.State[5] + F;
    Context.State[6] := Context.State[6] + G;
    Context.State[7] := Context.State[7] + H;
  end;
end;

{$WARNINGS ON}

procedure SHA224Init(var Context: TCnSHA224Context);
begin
  Context.DataLen := 0;
  Context.BitLen := 0;
  Context.State[0] := $C1059ED8;
  Context.State[1] := $367CD507;
  Context.State[2] := $3070DD17;
  Context.State[3] := $F70E5939;
  Context.State[4] := $FFC00B31;
  Context.State[5] := $68581511;
  Context.State[6] := $64F98FA7;
  Context.State[7] := $BEFA4FA4;
  FillChar(Context.Data, SizeOf(Context.Data), 0);
end;

procedure SHA224Update(var Context: TCnSHA224Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA256Update(Context, Input, ByteLength);
end;

procedure SHA256UpdateW(var Context: TCnSHA256Context; Input: PWideChar; CharLength: Cardinal); forward;

procedure SHA224UpdateW(var Context: TCnSHA224Context; Input: PWideChar; CharLength: Cardinal);
begin
  SHA256UpdateW(Context, Input, CharLength);
end;

procedure SHA224Final(var Context: TCnSHA224Context; var Digest: TCnSHA224Digest);
var
  Dig: TCnSHA256Digest;
begin
  SHA256Final(Context, Dig);
  Move(Dig[0], Digest[0], SizeOf(TCnSHA224Digest));
end;

procedure SHA256Init(var Context: TCnSHA256Context);
begin
  Context.DataLen := 0;
  Context.BitLen := 0;
  Context.State[0] := $6A09E667;
  Context.State[1] := $BB67AE85;
  Context.State[2] := $3C6EF372;
  Context.State[3] := $A54FF53A;
  Context.State[4] := $510E527F;
  Context.State[5] := $9B05688C;
  Context.State[6] := $1F83D9AB;
  Context.State[7] := $5BE0CD19;
  FillChar(Context.Data, SizeOf(Context.Data), 0);
end;

procedure SHA256Update(var Context: TCnSHA256Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  B: Cardinal;
begin
  while ByteLength > 0 do
  begin
    if 64 - Context.DataLen > ByteLength then
      B := ByteLength
    else
      B := 64 - Context.DataLen;

    Move(Input^, Context.Data[Context.DataLen], B);
    Inc(Context.DataLen, B);
    Dec(ByteLength, B);
    Inc(Input, B);

    if Context.DataLen = 64 then
    begin
      SHA256Transform(Context, @Context.Data[0]);
      Context.BitLen := Context.BitLen + 512;
      Context.DataLen := 0;
    end;
  end;
end;

procedure SHA256UpdateW(var Context: TCnSHA256Context; Input: PWideChar; CharLength: Cardinal);
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
    SHA256Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  SHA256Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure SHA256Final(var Context: TCnSHA256Context; var Digest: TCnSHA256Digest);
var
  I: Integer;
begin
  I := Context.DataLen;
  Context.Data[I] := $80;
  Inc(I);

  if Context.Datalen < 56 then
  begin
    while I < 56 do
    begin
      Context.Data[I] := 0;
      Inc(I);
    end;
  end
  else
  begin
    while I < 64 do
    begin
      Context.Data[I] := 0;
      Inc(I);
    end;

    SHA256Transform(Context, @(Context.Data[0]));
    FillChar(Context.Data, 56, 0);
  end;

  Context.BitLen := Context.BitLen + Context.DataLen * 8;
  Context.Data[63] := Context.Bitlen;
  Context.Data[62] := Context.Bitlen shr 8;
  Context.Data[61] := Context.Bitlen shr 16;
  Context.Data[60] := Context.Bitlen shr 24;
  Context.Data[59] := Context.Bitlen shr 32;
  Context.Data[58] := Context.Bitlen shr 40;
  Context.Data[57] := Context.Bitlen shr 48;
  Context.Data[56] := Context.Bitlen shr 56;
  SHA256Transform(Context, @(Context.Data[0]));

  for I := 0 to 3 do
  begin
    Digest[I] := (Context.State[0] shr (24 - I * 8)) and $000000FF;
    Digest[I + 4] := (Context.State[1] shr (24 - I * 8)) and $000000FF;
    Digest[I + 8] := (Context.State[2] shr (24 - I * 8)) and $000000FF;
    Digest[I + 12] := (Context.State[3] shr (24 - I * 8)) and $000000FF;
    Digest[I + 16] := (Context.State[4] shr (24 - I * 8)) and $000000FF;
    Digest[I + 20] := (Context.State[5] shr (24 - I * 8)) and $000000FF;
    Digest[I + 24] := (Context.State[6] shr (24 - I * 8)) and $000000FF;
    Digest[I + 28] := (Context.State[7] shr (24 - I * 8)) and $000000FF;
  end;
end;

{$WARNINGS OFF}

procedure SHA384Init(var Context: TCnSHA384Context);
begin
  Context.DataLen := 0;
  Context.TotalLen := 0;
  Context.State[0] := $CBBB9D5DC1059ED8;
  Context.State[1] := $629A292A367CD507;
  Context.State[2] := $9159015A3070DD17;
  Context.State[3] := $152FECD8F70E5939;
  Context.State[4] := $67332667FFC00B31;
  Context.State[5] := $8EB44A8768581511;
  Context.State[6] := $DB0C2E0D64F98FA7;
  Context.State[7] := $47B5481DBEFA4FA4;
  FillChar(Context.Data, SizeOf(Context.Data), 0);
end;

{$WARNINGS ON}

procedure SHA384Update(var Context: TCnSHA384Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  SHA512Update(Context, Input, ByteLength);
end;

procedure SHA512UpdateW(var Context: TCnSHA512Context; Input: PWideChar; CharLength: Cardinal); forward;

procedure SHA384UpdateW(var Context: TCnSHA384Context; Input: PWideChar; CharLength: Cardinal);
begin
  SHA512UpdateW(Context, Input, CharLength);
end;

procedure SHA384Final(var Context: TCnSHA384Context; var Digest: TCnSHA384Digest);
var
  Dig: TCnSHA512Digest;
begin
  SHA512Final(Context, Dig);
  Move(Dig[0], Digest[0], SizeOf(TCnSHA384Digest));
end;

{$WARNINGS OFF}

procedure SHA512Init(var Context: TCnSHA512Context);
begin
  Context.DataLen := 0;
  Context.TotalLen := 0;
  Context.State[0] := $6A09E667F3BCC908;
  Context.State[1] := $BB67AE8584CAA73B;
  Context.State[2] := $3C6EF372FE94F82B;
  Context.State[3] := $A54FF53A5F1D36F1;
  Context.State[4] := $510E527FADE682D1;
  Context.State[5] := $9B05688C2B3E6C1F;
  Context.State[6] := $1F83D9ABFB41BD6B;
  Context.State[7] := $5BE0CD19137E2179;
  FillChar(Context.Data, SizeOf(Context.Data), 0);
end;

{$WARNINGS ON}

procedure SHA512Update(var Context: TCnSHA512Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  TempLength, RemainLength, NewLength, BlockCount: Cardinal;
begin
  TempLength := 128 - Context.DataLen;
  if ByteLength < TempLength then
    RemainLength := ByteLength
  else
    RemainLength := TempLength;

  Move(Input^, Context.Data[Context.DataLen], RemainLength);
  if Context.DataLen + ByteLength < 128 then
  begin
    Inc(Context.DataLen, ByteLength);
    Exit;
  end;

  NewLength := Cardinal(ByteLength) - RemainLength;
  BlockCount := NewLength div 128;
  Input := PAnsiChar(TCnNativeUInt(Input) + RemainLength);

  SHA512Transform(Context, @Context.Data[0], 1);
  SHA512Transform(Context, Input, BlockCount);

  RemainLength := NewLength mod 128;
  Input := PAnsiChar(TCnNativeUInt(Input) + (BlockCount shl 7));
  Move(Input^, Context.Data[Context.DataLen], RemainLength);

  Context.DataLen := RemainLength;
  Inc(Context.TotalLen, (BlockCount + 1) shl 7);
end;

procedure SHA512UpdateW(var Context: TCnSHA512Context; Input: PWideChar; CharLength: Cardinal);
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
    SHA512Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  SHA512Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure SHA512Final(var Context: TCnSHA512Context; var Digest: TCnSHA512Digest);
var
  I: Integer;
  BlockCount, BitLength, PmLength: Cardinal;
begin
  if (Context.DataLen mod 128) > 111 then
    BlockCount := 2
  else
    BlockCount := 1;

  BitLength := (Context.TotalLen + Context.DataLen) shl 3;
  PmLength := BlockCount shl 7;
  FillChar(Context.Data[Context.DataLen], PmLength - Context.DataLen, 0);
  Context.Data[Context.DataLen] := $80;

  Context.Data[PmLength - 1] := Byte(BitLength);
  Context.Data[PmLength - 2] := Byte(BitLength shr 8);
  Context.Data[PmLength - 3] := Byte(BitLength shr 16);
  Context.Data[PmLength - 4] := Byte(BitLength shr 24);

  SHA512Transform(Context, @(Context.Data[0]), BlockCount);

  for I := 0 to 7 do
  begin
    Digest[I] := (Context.State[0] shr (56 - I * 8)) and $000000FF;
    Digest[I + 8] := (Context.State[1] shr (56 - I * 8)) and $000000FF;
    Digest[I + 16] := (Context.State[2] shr (56 - I * 8)) and $000000FF;
    Digest[I + 24] := (Context.State[3] shr (56 - I * 8)) and $000000FF;
    Digest[I + 32] := (Context.State[4] shr (56 - I * 8)) and $000000FF;
    Digest[I + 40] := (Context.State[5] shr (56 - I * 8)) and $000000FF;
    Digest[I + 48] := (Context.State[6] shr (56 - I * 8)) and $000000FF;
    Digest[I + 56] := (Context.State[7] shr (56 - I * 8)) and $000000FF;
  end;
end;

// �����ݿ���� SHA224 ����
function SHA224(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA224Digest;
var
  Context: TCnSHA224Context;
begin
  SHA224Init(Context);
  SHA224Update(Context, Input, ByteLength);
  SHA224Final(Context, Result);
end;

// �����ݿ���� SHA256 ����
function SHA256(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA256Digest;
var
  Context: TCnSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, Input, ByteLength);
  SHA256Final(Context, Result);
end;

// �����ݿ���� SHA384 ����
function SHA384(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA384Digest;
var
  Context: TCnSHA384Context;
begin
  SHA384Init(Context);
  SHA384Update(Context, Input, ByteLength);
  SHA384Final(Context, Result);
end;

// �����ݿ���� SHA512 ����
function SHA512(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA512Digest;
var
  Context: TCnSHA512Context;
begin
  SHA512Init(Context);
  SHA512Update(Context, Input, ByteLength);
  SHA512Final(Context, Result);
end;

// �����ݿ���� SHA224 ����
function SHA224Buffer(const Buffer; Count: Cardinal): TCnSHA224Digest;
var
  Context: TCnSHA224Context;
begin
  SHA224Init(Context);
  SHA224Update(Context, PAnsiChar(Buffer), Count);
  SHA224Final(Context, Result);
end;

// �����ݿ���� SHA256 ����
function SHA256Buffer(const Buffer; Count: Cardinal): TCnSHA256Digest;
var
  Context: TCnSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PAnsiChar(Buffer), Count);
  SHA256Final(Context, Result);
end;

// �����ݿ���� SHA384 ����
function SHA384Buffer(const Buffer; Count: Cardinal): TCnSHA384Digest;
var
  Context: TCnSHA384Context;
begin
  SHA384Init(Context);
  SHA384Update(Context, PAnsiChar(Buffer), Count);
  SHA384Final(Context, Result);
end;

// �����ݿ���� SHA512 ����
function SHA512Buffer(const Buffer; Count: Cardinal): TCnSHA512Digest;
var
  Context: TCnSHA512Context;
begin
  SHA512Init(Context);
  SHA512Update(Context, PAnsiChar(Buffer), Count);
  SHA512Final(Context, Result);
end;

// ���ֽ�������� SHA224 ����
function SHA224Bytes(Data: TBytes): TCnSHA224Digest;
var
  Context: TCnSHA224Context;
begin
  SHA224Init(Context);
  SHA224Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA224Final(Context, Result);
end;

// ���ֽ�������� SHA256 ����
function SHA256Bytes(Data: TBytes): TCnSHA256Digest;
var
  Context: TCnSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA256Final(Context, Result);
end;

// ���ֽ�������� SHA384 ����
function SHA384Bytes(Data: TBytes): TCnSHA384Digest;
var
  Context: TCnSHA384Context;
begin
  SHA384Init(Context);
  SHA384Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA384Final(Context, Result);
end;

// ���ֽ�������� SHA512 ����
function SHA512Bytes(Data: TBytes): TCnSHA512Digest;
var
  Context: TCnSHA512Context;
begin
  SHA512Init(Context);
  SHA512Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA512Final(Context, Result);
end;

// �� String �������ݽ��� SHA224 ����
function SHA224String(const Str: string): TCnSHA224Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA224StringA(AStr);
end;

// �� String �������ݽ��� SHA256 ����
function SHA256String(const Str: string): TCnSHA256Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA256StringA(AStr);
end;

// �� String �������ݽ��� SHA384 ����
function SHA384String(const Str: string): TCnSHA384Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA384StringA(AStr);
end;

// �� String �������ݽ��� SHA512 ����
function SHA512String(const Str: string): TCnSHA512Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA512StringA(AStr);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA224 ���㣬������ת��
{$IFDEF UNICODE}
function SHA224UnicodeString(const Str: string): TCnSHA224Digest;
{$ELSE}
function SHA224UnicodeString(const Str: WideString): TCnSHA224Digest;
{$ENDIF}
var
  Context: TCnSHA224Context;
begin
  SHA224Init(Context);
  SHA224Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA224Final(Context, Result);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA256 ���㣬������ת��
{$IFDEF UNICODE}
function SHA256UnicodeString(const Str: string): TCnSHA256Digest;
{$ELSE}
function SHA256UnicodeString(const Str: WideString): TCnSHA256Digest;
{$ENDIF}
var
  Context: TCnSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA256Final(Context, Result);
end;  

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA384 ���㣬������ת��
{$IFDEF UNICODE}
function SHA384UnicodeString(const Str: string): TCnSHA384Digest;
{$ELSE}
function SHA384UnicodeString(const Str: WideString): TCnSHA384Digest;
{$ENDIF}
var
  Context: TCnSHA384Context;
begin
  SHA384Init(Context);
  SHA384Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA384Final(Context, Result);
end;  

// �� UnicodeString �������ݽ���ֱ�ӵ� SHA512 ���㣬������ת��
{$IFDEF UNICODE}
function SHA512UnicodeString(const Str: string): TCnSHA512Digest;
{$ELSE}
function SHA512UnicodeString(const Str: WideString): TCnSHA512Digest;
{$ENDIF}
var
  Context: TCnSHA512Context;
begin
  SHA512Init(Context);
  SHA512Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA512Final(Context, Result);
end;

// �� AnsiString �������ݽ��� SHA224 ����
function SHA224StringA(const Str: AnsiString): TCnSHA224Digest;
var
  Context: TCnSHA224Context;
begin
  SHA224Init(Context);
  SHA224Update(Context, PAnsiChar(Str), Length(Str));
  SHA224Final(Context, Result);
end;

// �� WideString �������ݽ��� SHA224 ����
function SHA224StringW(const Str: WideString): TCnSHA224Digest;
var
  Context: TCnSHA224Context;
begin
  SHA224Init(Context);
  SHA224UpdateW(Context, PWideChar(Str), Length(Str));
  SHA224Final(Context, Result);
end;

// �� AnsiString �������ݽ��� SHA256 ����
function SHA256StringA(const Str: AnsiString): TCnSHA256Digest;
var
  Context: TCnSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PAnsiChar(Str), Length(Str));
  SHA256Final(Context, Result);
end;

// �� WideString �������ݽ��� SHA256 ����
function SHA256StringW(const Str: WideString): TCnSHA256Digest;
var
  Context: TCnSHA256Context;
begin
  SHA256Init(Context);
  SHA256UpdateW(Context, PWideChar(Str), Length(Str));
  SHA256Final(Context, Result);
end;

// �� AnsiString �������ݽ��� SHA384 ����
function SHA384StringA(const Str: AnsiString): TCnSHA384Digest;
var
  Context: TCnSHA384Context;
begin
  SHA384Init(Context);
  SHA384Update(Context, PAnsiChar(Str), Length(Str));
  SHA384Final(Context, Result);
end;

// �� WideString �������ݽ��� SHA384 ����
function SHA384StringW(const Str: WideString): TCnSHA384Digest;
var
  Context: TCnSHA384Context;
begin
  SHA384Init(Context);
  SHA384UpdateW(Context, PWideChar(Str), Length(Str));
  SHA384Final(Context, Result);
end;

// �� AnsiString �������ݽ��� SHA512 ����
function SHA512StringA(const Str: AnsiString): TCnSHA512Digest;
var
  Context: TCnSHA512Context;
begin
  SHA512Init(Context);
  SHA512Update(Context, PAnsiChar(Str), Length(Str));
  SHA512Final(Context, Result);
end;

// �� WideString �������ݽ��� SHA512 ����
function SHA512StringW(const Str: WideString): TCnSHA512Digest;
var
  Context: TCnSHA512Context;
begin
  SHA512Init(Context);
  SHA512UpdateW(Context, PWideChar(Str), Length(Str));
  SHA512Final(Context, Result);
end;

function InternalSHAStream(Stream: TStream; const BufSize: Cardinal; var D:
  TCnSHA2GeneralDigest; SHA2Type: TSHA2Type; CallBack: TCnSHACalcProgressFunc): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;

  Context224: TCnSHA224Context;
  Context256: TCnSHA256Context;
  Context384: TCnSHA384Context;
  Context512: TCnSHA512Context;
  Dig224: TCnSHA224Digest;
  Dig256: TCnSHA256Digest;
  Dig384: TCnSHA384Digest;
  Dig512: TCnSHA512Digest;

  procedure _SHAInit;
  begin
    case SHA2Type of
      stSHA2_224:
        SHA224Init(Context224);
      stSHA2_256:
        SHA256Init(Context256);
      stSHA2_384:
        SHA384Init(Context384);
      stSHA2_512:
        SHA512Init(Context512);
    end;
  end;

  procedure _SHAUpdate;
  begin
    case SHA2Type of
      stSHA2_224:
        SHA224Update(Context224, Buf, ReadBytes);
      stSHA2_256:
        SHA256Update(Context256, Buf, ReadBytes);
      stSHA2_384:
        SHA384Update(Context384, Buf, ReadBytes);
      stSHA2_512:
        SHA512Update(Context512, Buf, ReadBytes);
    end;
  end;

  procedure _SHAFinal;
  begin
    case SHA2Type of
      stSHA2_224:
        SHA224Final(Context224, Dig224);
      stSHA2_256:
        SHA256Final(Context256, Dig256);
      stSHA2_384:
        SHA384Final(Context384, Dig384);
      stSHA2_512:
        SHA512Final(Context512, Dig512);
    end;
  end;

  procedure _CopyResult;
  begin
    case SHA2Type of
      stSHA2_224:
        Move(Dig224[0], D[0], SizeOf(TCnSHA224Digest));
      stSHA2_256:
        Move(Dig256[0], D[0], SizeOf(TCnSHA256Digest));
      stSHA2_384:
        Move(Dig384[0], D[0], SizeOf(TCnSHA384Digest));
      stSHA2_512:
        Move(Dig512[0], D[0], SizeOf(TCnSHA512Digest));
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
  _SHAInit;
 
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        _SHAUpdate;

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    _SHAFinal;
    _CopyResult;
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// ��ָ�������� SHA224 ����
function SHA224Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc):
  TCnSHA224Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  InternalSHAStream(Stream, 4096 * 1024, Dig, stSHA2_224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA224Digest));
end;

// ��ָ�������� SHA256 ����
function SHA256Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc):
  TCnSHA256Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  InternalSHAStream(Stream, 4096 * 1024, Dig, stSHA2_256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA256Digest));
end;

// ��ָ�������� SHA384 ����
function SHA384Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc):
  TCnSHA384Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  InternalSHAStream(Stream, 4096 * 1024, Dig, stSHA2_384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA384Digest));
end;

// ��ָ�������� SHA512 ����
function SHA512Stream(Stream: TStream; CallBack: TCnSHACalcProgressFunc):
  TCnSHA512Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  InternalSHAStream(Stream, 4096 * 1024, Dig, stSHA2_512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA512Digest));
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

function InternalSHAFile(const FileName: string; SHA2Type: TSHA2Type;
  CallBack: TCnSHACalcProgressFunc): TCnSHA2GeneralDigest;
var
  Context224: TCnSHA224Context;
  Context256: TCnSHA256Context;
  Context384: TCnSHA384Context;
  Context512: TCnSHA512Context;
  Dig224: TCnSHA224Digest;
  Dig256: TCnSHA256Digest;
  Dig384: TCnSHA384Digest;
  Dig512: TCnSHA512Digest;

{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  procedure _SHAInit;
  begin
    case SHA2Type of
      stSHA2_224:
        SHA224Init(Context224);
      stSHA2_256:
        SHA256Init(Context256);
      stSHA2_384:
        SHA384Init(Context384);
      stSHA2_512:
        SHA512Init(Context512);
    end;
  end;

{$IFDEF MSWINDOWS}
  procedure _SHAUpdate;
  begin
    case SHA2Type of
      stSHA2_224:
        SHA224Update(Context224, ViewPointer, GetFileSize(FileHandle, nil));
      stSHA2_256:
        SHA256Update(Context256, ViewPointer, GetFileSize(FileHandle, nil));
      stSHA2_384:
        SHA384Update(Context384, ViewPointer, GetFileSize(FileHandle, nil));
      stSHA2_512:
        SHA512Update(Context512, ViewPointer, GetFileSize(FileHandle, nil));
    end;
  end;
{$ENDIF}

  procedure _SHAFinal;
  begin
    case SHA2Type of
      stSHA2_224:
        SHA224Final(Context224, Dig224);
      stSHA2_256:
        SHA256Final(Context256, Dig256);
      stSHA2_384:
        SHA384Final(Context384, Dig384);
      stSHA2_512:
        SHA512Final(Context512, Dig512);
    end;
  end;

  procedure _CopyResult(var D: TCnSHA2GeneralDigest);
  begin
    case SHA2Type of
      stSHA2_224:
        Move(Dig224[0], D[0], SizeOf(TCnSHA224Digest));
      stSHA2_256:
        Move(Dig256[0], D[0], SizeOf(TCnSHA256Digest));
      stSHA2_384:
        Move(Dig384[0], D[0], SizeOf(TCnSHA384Digest));
      stSHA2_512:
        Move(Dig512[0], D[0], SizeOf(TCnSHA512Digest));
    end;
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���� Windows ƽ̨����������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSHAStream(Stream, 4096 * 1024, Result, SHA2Type, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    _SHAInit;
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
                _SHAUpdate;
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
    _SHAFinal;
    _CopyResult(Result);
{$ENDIF}
  end;
end;

// ��ָ���ļ����ݽ��� SHA224 ����
function SHA224File(const FileName: string; CallBack: TCnSHACalcProgressFunc):
  TCnSHA224Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  Dig := InternalSHAFile(FileName, stSHA2_224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA224Digest));
end;

// ��ָ���ļ����ݽ��� SHA256 ����
function SHA256File(const FileName: string; CallBack: TCnSHACalcProgressFunc):
  TCnSHA256Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  Dig := InternalSHAFile(FileName, stSHA2_256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA256Digest));
end;

// ��ָ���ļ����ݽ��� SHA384 ����
function SHA384File(const FileName: string; CallBack: TCnSHACalcProgressFunc):
  TCnSHA384Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  Dig := InternalSHAFile(FileName, stSHA2_384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA384Digest));
end;

// ��ָ���ļ����ݽ��� SHA512 ����
function SHA512File(const FileName: string; CallBack: TCnSHACalcProgressFunc):
  TCnSHA512Digest;
var
  Dig: TCnSHA2GeneralDigest;
begin
  Dig := InternalSHAFile(FileName, stSHA2_512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnSHA512Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHA224 �Ӵ�ֵ
function SHA224Print(const Digest: TCnSHA224Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA224Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHA256 �Ӵ�ֵ
function SHA256Print(const Digest: TCnSHA256Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA256Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHA384 �Ӵ�ֵ
function SHA384Print(const Digest: TCnSHA384Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA384Digest));
end;

// ��ʮ�����Ƹ�ʽ��� SHA512 �Ӵ�ֵ
function SHA512Print(const Digest: TCnSHA512Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA512Digest));
end;

// �Ƚ����� SHA224 �Ӵ�ֵ�Ƿ����
function SHA224Match(const D1, D2: TCnSHA224Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 28) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// �Ƚ����� SHA256 �Ӵ�ֵ�Ƿ����
function SHA256Match(const D1, D2: TCnSHA256Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 32) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// �Ƚ����� SHA384 �Ӵ�ֵ�Ƿ����
function SHA384Match(const D1, D2: TCnSHA384Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 48) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// �Ƚ����� SHA512 �Ӵ�ֵ�Ƿ����
function SHA512Match(const D1, D2: TCnSHA512Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 64) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// SHA224 �Ӵ�ֵת string
function SHA224DigestToStr(const Digest: TCnSHA224Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA224Digest));
end;

// SHA256 �Ӵ�ֵת string
function SHA256DigestToStr(const Digest: TCnSHA256Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA256Digest));
end;

// SHA384 �Ӵ�ֵת string
function SHA384DigestToStr(const Digest: TCnSHA384Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA384Digest));
end;

// SHA512 �Ӵ�ֵת string
function SHA512DigestToStr(const Digest: TCnSHA512Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA512Digest));
end;

procedure SHA224HmacInit(var Context: TCnSHA224Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA224Digest;
begin
  if KeyLength > HMAC_SHA2_224_256_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA224Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA2_224_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA2_224_256_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA2_224_256_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA224Init(Context);
  SHA224Update(Context, @(Context.Ipad[0]), HMAC_SHA2_224_256_BLOCK_SIZE_BYTE);
end;

procedure SHA224HmacUpdate(var Context: TCnSHA224Context; Input: PAnsiChar; Length:
  Cardinal);
begin
  SHA224Update(Context, Input, Length);
end;

procedure SHA224HmacFinal(var Context: TCnSHA224Context; var Output: TCnSHA224Digest);
var
  Len: Integer;
  TmpBuf: TCnSHA224Digest;
begin
  Len := HMAC_SHA2_224_OUTPUT_LENGTH_BYTE;
  SHA224Final(Context, TmpBuf);
  SHA224Init(Context);
  SHA224Update(Context, @(Context.Opad[0]), HMAC_SHA2_224_256_BLOCK_SIZE_BYTE);
  SHA224Update(Context, @(TmpBuf[0]), Len);
  SHA224Final(Context, Output);
end;

procedure SHA256HmacInit(var Context: TCnSHA256Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA256Digest;
begin
  if KeyLength > HMAC_SHA2_224_256_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA256Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA2_256_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA2_224_256_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA2_224_256_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA256Init(Context);
  SHA256Update(Context, @(Context.Ipad[0]), HMAC_SHA2_224_256_BLOCK_SIZE_BYTE);
end;

procedure SHA256HmacUpdate(var Context: TCnSHA256Context; Input: PAnsiChar; Length:
  Cardinal);
begin
  SHA256Update(Context, Input, Length);
end;

procedure SHA256HmacFinal(var Context: TCnSHA256Context; var Output: TCnSHA256Digest);
var
  Len: Integer;
  TmpBuf: TCnSHA256Digest;
begin
  Len := HMAC_SHA2_256_OUTPUT_LENGTH_BYTE;
  SHA256Final(Context, TmpBuf);
  SHA256Init(Context);
  SHA256Update(Context, @(Context.Opad[0]), HMAC_SHA2_224_256_BLOCK_SIZE_BYTE);
  SHA256Update(Context, @(TmpBuf[0]), Len);
  SHA256Final(Context, Output);
end;

procedure SHA224Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA224Digest);
var
  Context: TCnSHA224Context;
begin
  SHA224HmacInit(Context, Key, KeyByteLength);
  SHA224HmacUpdate(Context, Input, ByteLength);
  SHA224HmacFinal(Context, Output);
end;

procedure SHA256Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA256Digest);
var
  Context: TCnSHA256Context;
begin
  SHA256HmacInit(Context, Key, KeyByteLength);
  SHA256HmacUpdate(Context, Input, ByteLength);
  SHA256HmacFinal(Context, Output);
end;

procedure SHA384HmacInit(var Context: TCnSHA384Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA384Digest;
begin
  if KeyLength > HMAC_SHA2_384_512_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA384Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA2_384_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA2_384_512_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA2_384_512_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA384Init(Context);
  SHA384Update(Context, @(Context.Ipad[0]), HMAC_SHA2_384_512_BLOCK_SIZE_BYTE);
end;

procedure SHA384HmacUpdate(var Context: TCnSHA384Context; Input: PAnsiChar;
  Length: Cardinal);
begin
  SHA384Update(Context, Input, Length);
end;

procedure SHA384HmacFinal(var Context: TCnSHA384Context; var Output: TCnSHA384Digest);
var
  Len: Integer;
  TmpBuf: TCnSHA384Digest;
begin
  Len := HMAC_SHA2_384_OUTPUT_LENGTH_BYTE;
  SHA384Final(Context, TmpBuf);
  SHA384Init(Context);
  SHA384Update(Context, @(Context.Opad[0]), HMAC_SHA2_384_512_BLOCK_SIZE_BYTE);
  SHA384Update(Context, @(TmpBuf[0]), Len);
  SHA384Final(Context, Output);
end;

procedure SHA384Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA384Digest);
var
  Context: TCnSHA384Context;
begin
  SHA384HmacInit(Context, Key, KeyByteLength);
  SHA384HmacUpdate(Context, Input, ByteLength);
  SHA384HmacFinal(Context, Output);
end;

procedure SHA512HmacInit(var Context: TCnSHA512Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA512Digest;
begin
  if KeyLength > HMAC_SHA2_384_512_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA512Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA2_512_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_SHA2_384_512_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_SHA2_384_512_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA512Init(Context);
  SHA512Update(Context, @(Context.Ipad[0]), HMAC_SHA2_384_512_BLOCK_SIZE_BYTE);
end;

procedure SHA512HmacUpdate(var Context: TCnSHA512Context; Input: PAnsiChar;
  Length: Cardinal);
begin
  SHA512Update(Context, Input, Length);
end;

procedure SHA512HmacFinal(var Context: TCnSHA512Context; var Output: TCnSHA512Digest);
var
  Len: Integer;
  TmpBuf: TCnSHA512Digest;
begin
  Len := HMAC_SHA2_512_OUTPUT_LENGTH_BYTE;
  SHA512Final(Context, TmpBuf);
  SHA512Init(Context);
  SHA512Update(Context, @(Context.Opad[0]), HMAC_SHA2_384_512_BLOCK_SIZE_BYTE);
  SHA512Update(Context, @(TmpBuf[0]), Len);
  SHA512Final(Context, Output);
end;

procedure SHA512Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA512Digest);
var
  Context: TCnSHA512Context;
begin
  SHA512HmacInit(Context, Key, KeyByteLength);
  SHA512HmacUpdate(Context, Input, ByteLength);
  SHA512HmacFinal(Context, Output);
end;

end.
