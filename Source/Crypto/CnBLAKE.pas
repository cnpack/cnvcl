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

unit CnBLAKE;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�BLAKE �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
*           �� https://github.com/veorq/BLAKE/ �� C ������ֲ���������䲿�ֹ���
* ��    ע������Ԫʵ���� BLAKE ϵ���Ӵ��㷨����Ӧ�� HMAC �㷨������ BLAKE224/256/384/512��
* ����ƽ̨��PWin7 + Delphi 7.0
* ���ݲ��ԣ�PWinXP/7/10/11 + Delphi 5/6/7 ~ D12
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.06.12 V1.0
*               ������Ԫ��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative, CnConsts;

type
  PCnBLAKEGeneralDigest = ^TCnBLAKEGeneralDigest;
  {* BLAKE ͨ���Ӵս��ָ��}
  TCnBLAKEGeneralDigest = array[0..63] of Byte;
  {* BLAKE ͨ���Ӵս����ȡ��� 64 �ֽ�}

  PCnBLAKE224Digest = ^TCnBLAKE224Digest;
  {* BLAKE224 �Ӵս��ָ��}
  TCnBLAKE224Digest = array[0..27] of Byte;
  {* BLAKE224 �Ӵս����28 �ֽ�}

  PCnBLAKE256Digest = ^TCnBLAKE256Digest;
  {* BLAKE256 �Ӵս��ָ��}
  TCnBLAKE256Digest = array[0..31] of Byte;
  {* BLAKE256 �Ӵս����32 �ֽ�}

  PCnBLAKE384Digest = ^TCnBLAKE384Digest;
  {* BLAKE384 �Ӵս��ָ��}
  TCnBLAKE384Digest = array[0..47] of Byte;
  {* BLAKE384 �Ӵս����48 �ֽ�}

  PCnBLAKE512Digest = ^TCnBLAKE512Digest;
  {* BLAKE512 �Ӵս��ָ��}
  TCnBLAKE512Digest = array[0..63] of Byte;
  {* BLAKE512 �Ӵս����64 �ֽ�}

  TCnBLAKE256Context = packed record
  {* BLAKE256 �������Ľṹ}
    H: array[0..7] of Cardinal;
    S: array[0..3] of Cardinal;
    T: array[0..1] of Cardinal;
    BufLen: Integer;
    Nullt: Integer;
    Buf: array[0..63] of Byte;
    Ipad: array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;

  TCnBLAKE224Context = TCnBLAKE256Context;
  {* BLAKE224 �������Ľṹ}

  TCnBLAKE512Context = packed record
  {* BLAKE512 �������Ľṹ}
    H: array[0..7] of TUInt64;
    S: array[0..3] of TUInt64;
    T: array[0..1] of TUInt64;
    BufLen: Integer;
    Nullt: Integer;
    Buf: array[0..127] of Byte;
    Ipad: array[0..127] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..127] of Byte;      {!< HMAC: outer padding        }
  end;

  TCnBLAKE384Context = TCnBLAKE512Context;
  {* BLAKE512 �������Ľṹ}

  TCnBLAKECalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* ���� BLAKE ϵ���Ӵս��Ȼص��¼���������}

function BLAKE224(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE224Digest;
{* �����ݿ���� BLAKE224 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE256Digest;
{* �����ݿ���� BLAKE256 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE384Digest;
{* �����ݿ���� BLAKE384 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE512Digest;
{* �����ݿ���� BLAKE512 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

function BLAKE224Buffer(const Buffer; Count: Cardinal): TCnBLAKE224Digest;
{* �����ݿ���� BLAKE224 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256Buffer(const Buffer; Count: Cardinal): TCnBLAKE256Digest;
{* �����ݿ���� BLAKE256 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384Buffer(const Buffer; Count: Cardinal): TCnBLAKE384Digest;
{* �����ݿ���� BLAKE384 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512Buffer(const Buffer; Count: Cardinal): TCnBLAKE512Digest;
{* �����ݿ���� BLAKE512 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

function BLAKE224Bytes(Data: TBytes): TCnBLAKE224Digest;
{* ���ֽ�������� BLAKE224 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256Bytes(Data: TBytes): TCnBLAKE256Digest;
{* ���ֽ�������� BLAKE256 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384Bytes(Data: TBytes): TCnBLAKE384Digest;
{* ���ֽ�������� BLAKE384 ���㡣
   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512Bytes(Data: TBytes): TCnBLAKE512Digest;
{* ���ֽ�������� BLAKE512 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

function BLAKE224String(const Str: string): TCnBLAKE224Digest;
{* �� String �������ݽ��� BLAKE224 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256String(const Str: string): TCnBLAKE256Digest;
{* �� String �������ݽ��� BLAKE256 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384String(const Str: string): TCnBLAKE384Digest;
{* �� String �������ݽ��� BLAKE384 ���㣬ע�� D2009�����ϰ汾��string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512String(const Str: string): TCnBLAKE512Digest;
{* �� String �������ݽ��� BLAKE512 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}


function BLAKE224StringA(const Str: AnsiString): TCnBLAKE224Digest;
{* �� AnsiString �������ݽ��� BLAKE224 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE224StringW(const Str: WideString): TCnBLAKE224Digest;
{* �� WideString �������ݽ��� BLAKE224 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256StringA(const Str: AnsiString): TCnBLAKE256Digest;
{* �� AnsiString �������ݽ��� BLAKE256 ���㡣

   ������
     const Str: AnsiString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE256StringW(const Str: WideString): TCnBLAKE256Digest;
{* �� WideString �������ݽ��� BLAKE256 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384StringA(const Str: AnsiString): TCnBLAKE384Digest;
{* �� AnsiString �������ݽ��� BLAKE384 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE384StringW(const Str: WideString): TCnBLAKE384Digest;
{* �� WideString �������ݽ��� BLAKE384 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512StringA(const Str: AnsiString): TCnBLAKE512Digest;
{* �� AnsiString �������ݽ��� BLAKE512 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

function BLAKE512StringW(const Str: WideString): TCnBLAKE512Digest;
{* �� WideString �������ݽ��� BLAKE512 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

{$IFDEF UNICODE}

function BLAKE224UnicodeString(const Str: string): TCnBLAKE224Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE224 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256UnicodeString(const Str: string): TCnBLAKE256Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384UnicodeString(const Str: string): TCnBLAKE384Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE384 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512UnicodeString(const Str: string): TCnBLAKE512Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE512 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

{$ELSE}

function BLAKE224UnicodeString(const Str: WideString): TCnBLAKE224Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE224 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256UnicodeString(const Str: WideString): TCnBLAKE256Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE256 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384UnicodeString(const Str: WideString): TCnBLAKE384Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE384 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512UnicodeString(const Str: WideString): TCnBLAKE512Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE512 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

{$ENDIF}

function BLAKE224File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc =
  nil): TCnBLAKE224Digest;
{* ��ָ���ļ����ݽ��� BLAKE256 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE224Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc = nil):
  TCnBLAKE224Digest;
{* ��ָ�������ݽ��� BLAKE224 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE224Digest              - ���ص� BLAKE224 �Ӵ�ֵ
}

function BLAKE256File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc =
  nil): TCnBLAKE256Digest;
{* ��ָ���ļ����ݽ��� BLAKE256 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE256Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc = nil):
  TCnBLAKE256Digest;
{* ��ָ�������ݽ��� BLAKE256 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE256Digest              - ���ص� BLAKE256 �Ӵ�ֵ
}

function BLAKE384File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc =
  nil): TCnBLAKE384Digest;
{* ��ָ���ļ����ݽ��� BLAKE384 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE384Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc = nil):
  TCnBLAKE384Digest;
{* ��ָ�������ݽ��� BLAKE384 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE384Digest              - ���ص� BLAKE384�Ӵ�ֵ
}

function BLAKE512File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc =
  nil): TCnBLAKE512Digest;
{* ��ָ���ļ����ݽ��� BLAKE512 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

function BLAKE512Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc = nil):
  TCnBLAKE512Digest;
{* ��ָ�������ݽ��� BLAKE512 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnBLAKECalcProgressFunc   - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnBLAKE512Digest              - ���ص� BLAKE512 �Ӵ�ֵ
}

// �����������������ⲿ���������ݽ�����ɢ�� BLAKE224 ���㣬BLAKE224Update �ɶ�α�����

procedure BLAKE224Init(var Context: TCnBLAKE224Context);
{* ��ʼ��һ�� BLAKE224 ���������ģ�׼������ BLAKE224 �����

   ������
     var Context: TCnBLAKE224Context      - ����ʼ���� BLAKE224 ������

   ����ֵ�����ޣ�
}

procedure BLAKE224Update(var Context: TCnBLAKE224Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� BLAKE224 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnBLAKE224Context      - BLAKE224 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BLAKE224Final(var Context: TCnBLAKE224Context; var Digest: TCnBLAKE224Digest);
{* �������ּ��㣬�� BLAKE224 ��������� Digest �С�

   ������
     var Context: TCnBLAKE224Context      - BLAKE224 ������
     var Digest: TCnBLAKE224Digest        - ���ص� BLAKE224 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� BLAKE256 ���㣬BLAKE256Update �ɶ�α�����

procedure BLAKE256Init(var Context: TCnBLAKE256Context);
{* ��ʼ��һ�� BLAKE256 ���������ģ�׼������ BLAKE256 �����

   ������
     var Context: TCnBLAKE256Context      - ����ʼ���� BLAKE256 ������

   ����ֵ�����ޣ�
}

procedure BLAKE256Update(var Context: TCnBLAKE256Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� BLAKE256 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnBLAKE256Context      - BLAKE256 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BLAKE256Final(var Context: TCnBLAKE256Context; var Digest: TCnBLAKE256Digest);
{* �������ּ��㣬�� BLAKE256 ��������� Digest �С�

   ������
     var Context: TCnBLAKE256Context      - BLAKE256 ������
     var Digest: TCnBLAKE256Digest        - ���ص� BLAKE256 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� BLAKE384 ���㣬BLAKE384Update �ɶ�α�����

procedure BLAKE384Init(var Context: TCnBLAKE384Context);
{* ��ʼ��һ�� BLAKE384 ���������ģ�׼������ BLAKE384 �����

   ������
     var Context: TCnBLAKE384Context      - ����ʼ���� BLAKE384 ������

   ����ֵ�����ޣ�
}

procedure BLAKE384Update(var Context: TCnBLAKE384Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� BLAKE384 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnBLAKE384Context      - BLAKE384 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BLAKE384Final(var Context: TCnBLAKE384Context; var Digest: TCnBLAKE384Digest);
{* �������ּ��㣬�� BLAKE384 ��������� Digest �С�

   ������
     var Context: TCnBLAKE384Context      - BLAKE384 ������
     var Digest: TCnBLAKE384Digest        - ���ص� BLAKE384 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� BLAKE512 ���㣬BLAKE512Update �ɶ�α�����

procedure BLAKE512Init(var Context: TCnBLAKE512Context);
{* ��ʼ��һ�� BLAKE512 ���������ģ�׼������ BLAKE512 �����

   ������
     var Context: TCnBLAKE512Context      - ����ʼ���� BLAKE512 ������

   ����ֵ�����ޣ�
}

procedure BLAKE512Update(var Context: TCnBLAKE512Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� BLAKE512 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnBLAKE512Context      - BLAKE512 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BLAKE512Final(var Context: TCnBLAKE512Context; var Digest: TCnBLAKE512Digest);
{* �������ּ��㣬�� BLAKE512 ��������� Digest ��

   ������
     var Context: TCnBLAKE512Context      - BLAKE512 ������
     var Digest: TCnBLAKE512Digest        - ���ص� BLAKE512 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

function BLAKE224Print(const Digest: TCnBLAKE224Digest): string;
{* ��ʮ�����Ƹ�ʽ��� BLAKE224 �Ӵ�ֵ��

   ������
     const Digest: TCnBLAKE224Digest      - ָ���� BLAKE224 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BLAKE256Print(const Digest: TCnBLAKE256Digest): string;
{* ��ʮ�����Ƹ�ʽ��� BLAKE256 �Ӵ�ֵ��

   ������
     const Digest: TCnBLAKE256Digest      - ָ���� BLAKE256 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BLAKE384Print(const Digest: TCnBLAKE384Digest): string;
{* ��ʮ�����Ƹ�ʽ��� BLAKE384 �Ӵ�ֵ��

   ������
     const Digest: TCnBLAKE384Digest      - ָ���� BLAKE384 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BLAKE512Print(const Digest: TCnBLAKE512Digest): string;
{* ��ʮ�����Ƹ�ʽ��� BLAKE512 �Ӵ�ֵ��

   ������
     const Digest: TCnBLAKE512Digest      - ָ���� BLAKE512 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BLAKE224Match(const D1: TCnBLAKE224Digest; const D2: TCnBLAKE224Digest): Boolean;
{* �Ƚ����� BLAKE224 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnBLAKE224Digest          - ���Ƚϵ� BLAKE224 �Ӵ�ֵһ
     const D2: TCnBLAKE224Digest          - ���Ƚϵ� BLAKE224 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function BLAKE256Match(const D1: TCnBLAKE256Digest; const D2: TCnBLAKE256Digest): Boolean;
{* �Ƚ����� BLAKE256 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnBLAKE256Digest          - ���Ƚϵ� BLAKE256 �Ӵ�ֵһ
     const D2: TCnBLAKE256Digest          - ���Ƚϵ� BLAKE256 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function BLAKE384Match(const D1: TCnBLAKE384Digest; const D2: TCnBLAKE384Digest): Boolean;
{* �Ƚ����� BLAKE384 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnBLAKE384Digest          - ���Ƚϵ� BLAKE384 �Ӵ�ֵһ
     const D2: TCnBLAKE384Digest          - ���Ƚϵ� BLAKE384 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function BLAKE512Match(const D1: TCnBLAKE512Digest; const D2: TCnBLAKE512Digest): Boolean;
{* �Ƚ����� BLAKE512 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnBLAKE512Digest          - ���Ƚϵ� BLAKE512 �Ӵ�ֵһ
     const D2: TCnBLAKE512Digest          - ���Ƚϵ� BLAKE512 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function BLAKE224DigestToStr(const Digest: TCnBLAKE224Digest): string;
{* BLAKE224 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnBLAKE224Digest      - ��ת���� BLAKE224 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function BLAKE256DigestToStr(const Digest: TCnBLAKE256Digest): string;
{* BLAKE256 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnBLAKE256Digest      - ��ת���� BLAKE256 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function BLAKE384DigestToStr(const Digest: TCnBLAKE384Digest): string;
{* BLAKE384 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnBLAKE384Digest      - ��ת���� BLAKE384 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function BLAKE512DigestToStr(const Digest: TCnBLAKE512Digest): string;
{* BLAKE512 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnBLAKE512Digest      - ��ת���� BLAKE512 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

procedure BLAKE224Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE224Digest);
{* ���� BLAKE224 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ BLAKE224 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ BLAKE224 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnBLAKE224Digest        - ���ص� BLAKE224 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure BLAKE256Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE256Digest);
{* ���� BLAKE256 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ BLAKE256 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ BLAKE256 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnBLAKE256Digest        - ���ص� BLAKE256 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure BLAKE384Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE384Digest);
{* ���� BLAKE384 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ BLAKE384 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ BLAKE384 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnBLAKE384Digest        - ���ص� BLAKE384 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

procedure BLAKE512Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE512Digest);
{* ���� BLAKE512 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ BLAKE512 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ BLAKE512 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnBLAKE512Digest        - ���ص� BLAKE512 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

implementation

type
  TBLAKEType = (btBLAKE224, btBLAKE256, btBLAKE384, btBLAKE512);

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE = 64;
  HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE = 128;

  HMAC_BLAKE_224_OUTPUT_LENGTH_BYTE = 28;
  HMAC_BLAKE_256_OUTPUT_LENGTH_BYTE = 32;
  HMAC_BLAKE_384_OUTPUT_LENGTH_BYTE = 48;
  HMAC_BLAKE_512_OUTPUT_LENGTH_BYTE = 64;

  Sigma: array[0..15, 0..15] of Byte = (
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ),
    (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 ),
    (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 ),
    ( 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 ),
    ( 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 ),
    ( 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 ),
    (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11 ),
    (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10 ),
    ( 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5 ),
    (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13 , 0 ),
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ),
    (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3 ),
    (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4 ),
    ( 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8 ),
    ( 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13 ),
    ( 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9 )
  );

  U256: array[0..15] of Cardinal = (
    $243F6A88, $85A308D3, $13198A2E, $03707344,
    $A4093822, $299F31D0, $082EFA98, $EC4E6C89,
    $452821E6, $38D01377, $BE5466CF, $34E90C6C,
    $C0AC29B7, $C97C50DD, $3F84D5B5, $B5470917
  );

  U512: array[0..15] of TUInt64 = (
    $243F6A8885A308D3, $13198A2E03707344,
    $A4093822299F31D0, $082EFA98EC4E6C89,
    $452821E638D01377, $BE5466CF34E90C6C,
    $C0AC29B7C97C50DD, $3F84D5B5B5470917,
    $9216D5D98979FB1B, $D1310BA698DFB5AC,
    $2FFD72DBD01ADFB7, $B8E1AFED6A267E96,
    $BA7C9045F12C7F99, $24A19947B3916CF7,
    $0801F2E2858EFC16, $636920D871574E69
  );

  Padding: array[0..128] of Byte =(
    $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

// ============================== ����������ʼ =================================

function ROTRight256(A, B: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A shr B) or (A shl (32 - B));
end;

function ROTRight512(X: TUInt64; Y: Integer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X shr Y) or (X shl (64 - Y));
end;

function U8To32Big(Ptr: Pointer): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  P: PByteArray;
begin
  P := PByteArray(Ptr);
  Result := (Cardinal(P^[0]) shl 24) or (Cardinal(P^[1]) shl 16)
    or (Cardinal(P^[2]) shl 8) or Cardinal(P^[3]);
end;

function U8To64Big(Ptr: Pointer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: Pointer;
begin
  T := Pointer(TCnIntAddress(Ptr) + SizeOf(Cardinal));
  Result := (TUInt64(U8To32Big(Ptr)) shl 32) or TUInt64(U8To32Big(T));
end;

procedure U32To8Big(Ptr: Pointer; V: Cardinal);
var
  P: PByteArray;
begin
  P := PByteArray(Ptr);
  P^[0] := Byte(V shr 24);
  P^[1] := Byte(V shr 16);
  P^[2] := Byte(V shr 8);
  P^[3] := Byte(V);
end;

procedure U64To8Big(Ptr: Pointer; V: TUInt64);
var
  T: Pointer;
begin
  T := Pointer(TCnIntAddress(Ptr) + SizeOf(Cardinal));
  U32To8Big(Ptr, Cardinal(V shr 32));
  U32To8Big(T, Cardinal(V));
end;

// M �� V ���� Cardinal �����׵�ַ
procedure G32(MPtr, VPtr: Pointer; A, B, C, D, E, I: Integer);
var
  M, V: PCnUInt32Array;
begin
  M := PCnUInt32Array(MPtr);
  V := PCnUInt32Array(VPtr);

  V^[A] := V^[A] + (M[Sigma[I][E]] xor U256[Sigma[I][E + 1]]) + V^[B];
  V^[D] := ROTRight256(V^[D] xor V^[A], 16);
  V^[C] := V^[C] + V[D];
  V^[B] := ROTRight256(V^[B] xor V^[C], 12);
  V^[A] := V^[A] + (M[Sigma[I][E + 1]] xor U256[Sigma[I][E]]) + V^[B];
  V^[D] := ROTRight256(V^[D] xor V^[A], 8);
  V^[C] := V^[C] + V^[D];
  V^[B] := ROTRight256(V^[B] xor V^[C], 7);
end;

// M �� V ���� TUInt64 �����׵�ַ
procedure G64(MPtr, VPtr: Pointer; A, B, C, D, E, I: Integer);
var
  M, V: PCnUInt64Array;
begin
  M := PCnUInt64Array(MPtr);
  V := PCnUInt64Array(VPtr);

  V^[A] := V^[A] + (M[Sigma[I][E]] xor U512[Sigma[I][E + 1]]) + V^[B];
  V^[D] := ROTRight512(V^[D] xor V^[A], 32);
  V^[C] := V^[C] + V[D];
  V^[B] := ROTRight512(V^[B] xor V^[C], 25);
  V^[A] := V^[A] + (M[Sigma[I][E + 1]] xor U512[Sigma[I][E]]) + V^[B];
  V^[D] := ROTRight512(V^[D] xor V^[A], 16);
  V^[C] := V^[C] + V^[D];
  V^[B] := ROTRight512(V^[B] xor V^[C], 11);
end;

// ============================== ����������� =================================

// 224/256 �������̣�Ptr ���ֽ������׵�ַ
procedure BLAKE224256Compress(var Context: TCnBLAKE224Context; Ptr: Pointer);
var
  I: Integer;
  M, V: array[0..15] of Cardinal;
begin
  for I := 0 to 15 do
    M[I] := U8To32Big(Pointer(TCnIntAddress(Ptr) + I * SizeOf(Cardinal)));
  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := Context.S[0] xor U256[0];
  V[ 9] := Context.S[1] xor U256[1];
  V[10] := Context.S[2] xor U256[2];
  V[11] := Context.S[3] xor U256[3];
  V[12] := U256[4];
  V[13] := U256[5];
  V[14] := U256[6];
  V[15] := U256[7];

  if Context.Nullt = 0 then
  begin
    V[12] := V[12] xor Context.T[0];
    V[13] := V[13] xor Context.T[0];
    V[14] := V[14] xor Context.T[1];
    V[15] := V[15] xor Context.T[1];
  end;

  for I := 0 to 13 do
  begin
    G32(@M[0], @V[0], 0,  4,  8, 12,  0, I);
    G32(@M[0], @V[0], 1,  5,  9, 13,  2, I);
    G32(@M[0], @V[0], 2,  6, 10, 14,  4, I);
    G32(@M[0], @V[0], 3,  7, 11, 15,  6, I);
    G32(@M[0], @V[0], 0,  5, 10, 15,  8, I);
    G32(@M[0], @V[0], 1,  6, 11, 12, 10, I);
    G32(@M[0], @V[0], 2,  7,  8, 13, 12, I);
    G32(@M[0], @V[0], 3,  4,  9, 14, 14, I);
  end;

  for I := 0 to 15 do
    Context.H[I mod 8] := Context.H[I mod 8] xor V[I];

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor Context.S[I mod 4];
end;

// 384/512 �������̣�Ptr ���ֽ������׵�ַ
procedure BLAKE384512Compress(var Context: TCnBLAKE384Context; Ptr: Pointer);
var
  I: Integer;
  M, V: array[0..15] of TUInt64;
begin
  for I := 0 to 15 do
    M[I] := U8To64Big(Pointer(TCnIntAddress(Ptr) + I * SizeOf(TUInt64)));
  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := Context.S[0] xor U512[0];
  V[ 9] := Context.S[1] xor U512[1];
  V[10] := Context.S[2] xor U512[2];
  V[11] := Context.S[3] xor U512[3];
  V[12] := U512[4];
  V[13] := U512[5];
  V[14] := U512[6];
  V[15] := U512[7];

  if Context.Nullt = 0 then
  begin
    V[12] := V[12] xor Context.T[0];
    V[13] := V[13] xor Context.T[0];
    V[14] := V[14] xor Context.T[1];
    V[15] := V[15] xor Context.T[1];
  end;

  for I := 0 to 15 do
  begin
    G64(@M[0], @V[0], 0,  4,  8, 12,  0, I);
    G64(@M[0], @V[0], 1,  5,  9, 13,  2, I);
    G64(@M[0], @V[0], 2,  6, 10, 14,  4, I);
    G64(@M[0], @V[0], 3,  7, 11, 15,  6, I);
    G64(@M[0], @V[0], 0,  5, 10, 15,  8, I);
    G64(@M[0], @V[0], 1,  6, 11, 12, 10, I);
    G64(@M[0], @V[0], 2,  7,  8, 13, 12, I);
    G64(@M[0], @V[0], 3,  4,  9, 14, 14, I);
  end;

  for I := 0 to 15 do
    Context.H[I mod 8] := Context.H[I mod 8] xor V[I];

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor Context.S[I mod 4];
end;

procedure BLAKE224256Update(var Context: TCnBLAKE256Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  Left, Fill: Cardinal;
begin
  Left := Context.BufLen;
  Fill := 64 - Left;

  if (Left > 0) and (ByteLength >= Fill) then
  begin
    Move(Input^, Context.Buf[Left], Fill);
    Context.T[0] := Context.T[0] + 512;
    if Context.T[0] = 0 then
      Inc(Context.T[1]);

    BLAKE224256Compress(Context, @(Context.Buf[0]));
    Inc(Input, Fill);
    Dec(ByteLength, Fill);
    Left := 0;
  end;

  while ByteLength >= 64 do
  begin
    Context.T[0] := Context.T[0] + 512;
    if Context.T[0] = 0 then
      Inc(Context.T[1]);

    BLAKE224256Compress(Context, Input);
    Inc(Input, 64);
    Dec(ByteLength, 64);
  end;

  if ByteLength > 0 then
  begin
    Move(Input^, Context.Buf[Left], ByteLength);
    Context.BufLen := Left + ByteLength;
  end
  else
    Context.BufLen := 0;
end;

procedure BLAKE224256UpdateW(var Context: TCnBLAKE256Context; Input: PWideChar; CharLength: Cardinal);
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
    BLAKE224256Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  BLAKE224256Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure BLAKE224Init(var Context: TCnBLAKE224Context);
begin
  Context.H[0] := $C1059ED8;
  Context.H[1] := $367CD507;
  Context.H[2] := $3070DD17;
  Context.H[3] := $F70E5939;
  Context.H[4] := $FFC00B31;
  Context.H[5] := $68581511;
  Context.H[6] := $64F98FA7;
  Context.H[7] := $BEFA4FA4;

  Context.T[0] := 0;
  Context.T[1] := 0;
  Context.BufLen := 0;
  Context.Nullt := 0;

  Context.S[0] := 0;
  Context.S[1] := 0;
  Context.S[2] := 0;
  Context.S[3] := 0;
end;

procedure BLAKE224Update(var Context: TCnBLAKE224Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  BLAKE224256Update(Context, Input, ByteLength);
end;

procedure BLAKE224Final(var Context: TCnBLAKE224Context; var Digest: TCnBLAKE224Digest);
var
  MsgLen: array[0..7] of Byte;
  ZZ, OZ: Byte;
  LO, HI: Cardinal;
begin
  ZZ := 0;
  OZ := $80;
  LO := Context.T[0] + Cardinal(Context.BufLen shl 3);
  HI := Context.T[1];

  if LO < Cardinal(Context.BufLen shl 3) then
    Inc(HI);

  U32To8Big(@MsgLen[0], HI);
  U32To8Big(@MsgLen[4], LO);

  if Context.BufLen = 55 then
  begin
    Context.T[0] := Context.T[0] - 8;
    BLAKE224256Update(Context, @OZ, 1);
  end
  else
  begin
    if Context.BufLen < 55 then
    begin
      if Context.BufLen = 0 then
        Context.Nullt := 1;
      Context.T[0] := Context.T[0] - Cardinal(440 - (Context.BufLen shl 3));
      BLAKE224256Update(Context, @Padding[0], 55 - Context.BufLen);
    end
    else
    begin
      Context.T[0] := Context.T[0] - Cardinal(512 - (Context.BufLen shl 3));
      BLAKE224256Update(Context, @Padding[0], 64 - Context.BufLen);
      Context.T[0] := Context.T[0] - 440;
      BLAKE224256Update(Context, @Padding[1], 55 );
      Context.Nullt := 1;
    end;

    BLAKE224256Update(Context, @ZZ, 1);
    Context.T[0] := Context.T[0] - 8;
  end;

  Context.T[0] := Context.T[0] - 64;
  BLAKE224256Update(Context, @MsgLen[0], 8);

  U32To8Big(@Digest[0], Context.H[0]);
  U32To8Big(@Digest[4], Context.H[1]);
  U32To8Big(@Digest[8], Context.H[2]);
  U32To8Big(@Digest[12], Context.H[3]);
  U32To8Big(@Digest[16], Context.H[4]);
  U32To8Big(@Digest[20], Context.H[5]);
  U32To8Big(@Digest[24], Context.H[6]);
end;

procedure BLAKE256Init(var Context: TCnBLAKE256Context);
begin
  Context.H[0] := $6A09E667;
  Context.H[1] := $BB67AE85;
  Context.H[2] := $3C6EF372;
  Context.H[3] := $A54FF53A;
  Context.H[4] := $510E527F;
  Context.H[5] := $9B05688C;
  Context.H[6] := $1F83D9AB;
  Context.H[7] := $5BE0CD19;

  Context.T[0] := 0;
  Context.T[1] := 0;
  Context.BufLen := 0;
  Context.Nullt := 0;

  Context.S[0] := 0;
  Context.S[1] := 0;
  Context.S[2] := 0;
  Context.S[3] := 0;
end;

procedure BLAKE256Update(var Context: TCnBLAKE256Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  BLAKE224256Update(Context, Input, ByteLength);
end;

procedure BLAKE256Final(var Context: TCnBLAKE256Context; var Digest: TCnBLAKE256Digest);
var
  MsgLen: array[0..7] of Byte;
  ZZ, OZ: Byte;
  LO, HI: Cardinal;
begin
  ZZ := 1;
  OZ := $81;
  LO := Context.T[0] + Cardinal(Context.BufLen shl 3);
  HI := Context.T[1];

  if LO < Cardinal(Context.BufLen shl 3) then
    Inc(HI);

  U32To8Big(@MsgLen[0], HI);
  U32To8Big(@MsgLen[4], LO);

  if Context.BufLen = 55 then
  begin
    Context.T[0] := Context.T[0] - 8;
    BLAKE224256Update(Context, @OZ, 1);
  end
  else
  begin
    if Context.BufLen < 55 then
    begin
      if Context.BufLen = 0 then
        Context.Nullt := 1;
      Context.T[0] := Context.T[0] - Cardinal(440 - (Context.BufLen shl 3));
      BLAKE224256Update(Context, @Padding[0], 55 - Context.BufLen);
    end
    else
    begin
      Context.T[0] := Context.T[0] - Cardinal(512 - (Context.BufLen shl 3));
      BLAKE224256Update(Context, @Padding[0], 64 - Context.BufLen);
      Context.T[0] := Context.T[0] - 440;
      BLAKE224256Update(Context, @Padding[1], 55);
      Context.Nullt := 1;
    end;

    BLAKE224256Update(Context, @ZZ, 1);
    Context.T[0] := Context.T[0] - 8;
  end;

  Context.T[0] := Context.T[0] - 64;
  BLAKE224256Update(Context, @MsgLen[0], 8);

  U32To8Big(@Digest[0], Context.H[0]);
  U32To8Big(@Digest[4], Context.H[1]);
  U32To8Big(@Digest[8], Context.H[2]);
  U32To8Big(@Digest[12], Context.H[3]);
  U32To8Big(@Digest[16], Context.H[4]);
  U32To8Big(@Digest[20], Context.H[5]);
  U32To8Big(@Digest[24], Context.H[6]);
  U32To8Big(@Digest[28], Context.H[7]);
end;

procedure BLAKE384512Update(var Context: TCnBLAKE512Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  Left, Fill: Cardinal;
begin
  Left := Context.BufLen;
  Fill := 128 - Left;

  if (Left > 0) and (ByteLength >= Fill) then
  begin
    Move(Input^, Context.Buf[Left], Fill);
    Context.T[0] := Context.T[0] + 1024;
    if Context.T[0] = 0 then
      Inc(Context.T[1]);

    BLAKE384512Compress(Context, @(Context.Buf[0]));
    Inc(Input, Fill);
    Dec(ByteLength, Fill);
    Left := 0;
  end;

  while ByteLength >= 128 do
  begin
    Context.T[0] := Context.T[0] + 1024;
    if Context.T[0] = 0 then
      Inc(Context.T[1]);

    BLAKE384512Compress(Context, Input);
    Inc(Input, 128);
    Dec(ByteLength, 128);
  end;

  if ByteLength > 0 then
  begin
    Move(Input^, Context.Buf[Left], ByteLength);
    Context.BufLen := Left + ByteLength;
  end
  else
    Context.BufLen := 0;
end;

procedure BLAKE384512UpdateW(var Context: TCnBLAKE512Context; Input: PWideChar; CharLength: Cardinal);
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
    BLAKE384512Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  BLAKE384512Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure BLAKE384Init(var Context: TCnBLAKE384Context);
begin
  Context.H[0] := $CBBB9D5DC1059ED8;
  Context.H[1] := $629A292A367CD507;
  Context.H[2] := $9159015A3070DD17;
  Context.H[3] := $152FECD8F70E5939;
  Context.H[4] := $67332667FFC00B31;
  Context.H[5] := $8EB44A8768581511;
  Context.H[6] := $DB0C2E0D64F98FA7;
  Context.H[7] := $47B5481DBEFA4FA4;

  Context.T[0] := 0;
  Context.T[1] := 0;
  Context.BufLen := 0;
  Context.Nullt := 0;

  Context.S[0] := 0;
  Context.S[1] := 0;
  Context.S[2] := 0;
  Context.S[3] := 0;
end;

procedure BLAKE384Update(var Context: TCnBLAKE384Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  BLAKE384512Update(Context, Input, ByteLength);
end;

procedure BLAKE384Final(var Context: TCnBLAKE384Context; var Digest: TCnBLAKE384Digest);
var
  MsgLen: array[0..15] of Byte;
  ZZ, OZ: Byte;
  LO, HI: TUInt64;
begin
  ZZ := 0;
  OZ := $80;
  LO := Context.T[0] + TUInt64(Context.BufLen shl 3);
  HI := Context.T[1];

  if LO < (Context.BufLen shl 3) then
    Inc(HI);

  U64To8Big(@MsgLen[0], HI);
  U64To8Big(@MsgLen[8], LO);

  if Context.BufLen = 111 then
  begin
    Context.T[0] := Context.T[0] - 8;
    BLAKE384512Update(Context, @OZ, 1);
  end
  else
  begin
    if Context.BufLen < 111 then
    begin
      if Context.BufLen = 0 then
        Context.Nullt := 1;
      Context.T[0] := Context.T[0] - TUInt64(888 - (Context.BufLen shl 3));
      BLAKE384512Update(Context, @Padding[0], 111 - Context.BufLen);
    end
    else
    begin
      Context.T[0] := Context.T[0] - TUInt64(1024 - (Context.BufLen shl 3));
      BLAKE384512Update(Context, @Padding[0], 128 - Context.BufLen);
      Context.T[0] := Context.T[0] - 888;
      BLAKE384512Update(Context, @Padding[1], 111);
      Context.Nullt := 1;
    end;

    BLAKE384512Update(Context, @ZZ, 1);
    Context.T[0] := Context.T[0] - 8;
  end;

  Context.T[0] := Context.T[0] - 128;
  BLAKE384512Update(Context, @MsgLen[0], 16);

  U64To8Big(@Digest[0], Context.H[0]);
  U64To8Big(@Digest[8], Context.H[1]);
  U64To8Big(@Digest[16], Context.H[2]);
  U64To8Big(@Digest[24], Context.H[3]);
  U64To8Big(@Digest[32], Context.H[4]);
  U64To8Big(@Digest[40], Context.H[5]);
end;

procedure BLAKE512Init(var Context: TCnBLAKE512Context);
begin
  Context.H[0] := $6A09E667F3BCC908;
  Context.H[1] := $BB67AE8584CAA73B;
  Context.H[2] := $3C6EF372FE94F82B;
  Context.H[3] := $A54FF53A5F1D36F1;
  Context.H[4] := $510E527FADE682D1;
  Context.H[5] := $9B05688C2B3E6C1F;
  Context.H[6] := $1F83D9ABFB41BD6B;
  Context.H[7] := $5BE0CD19137E2179;

  Context.T[0] := 0;
  Context.T[1] := 0;
  Context.BufLen := 0;
  Context.Nullt := 0;

  Context.S[0] := 0;
  Context.S[1] := 0;
  Context.S[2] := 0;
  Context.S[3] := 0;
end;

procedure BLAKE512Update(var Context: TCnBLAKE512Context; Input: PAnsiChar; ByteLength: Cardinal);
begin
  BLAKE384512Update(Context, Input, ByteLength);
end;

procedure BLAKE512Final(var Context: TCnBLAKE512Context; var Digest: TCnBLAKE512Digest);
var
  MsgLen: array[0..15] of Byte;
  ZZ, OZ: Byte;
  LO, HI: TUInt64;
begin
  ZZ := 1;
  OZ := $81;
  LO := Context.T[0] + TUInt64(Context.BufLen shl 3);
  HI := Context.T[1];

  if LO < (Context.BufLen shl 3) then
    Inc(HI);

  U64To8Big(@MsgLen[0], HI);
  U64To8Big(@MsgLen[8], LO);

  if Context.BufLen = 111 then
  begin
    Context.T[0] := Context.T[0] - 8;
    BLAKE384512Update(Context, @OZ, 1);
  end
  else
  begin
    if Context.BufLen < 111 then
    begin
      if Context.BufLen = 0 then
        Context.Nullt := 1;
      Context.T[0] := Context.T[0] - TUInt64(888 - (Context.BufLen shl 3));
      BLAKE384512Update(Context, @Padding[0], 111 - Context.BufLen);
    end
    else
    begin
      Context.T[0] := Context.T[0] - TUInt64(1024 - (Context.BufLen shl 3));
      BLAKE384512Update(Context, @Padding[0], 128 - Context.BufLen);
      Context.T[0] := Context.T[0] - 888;
      BLAKE384512Update(Context, @Padding[1], 111);
      Context.Nullt := 1;
    end;

    BLAKE384512Update(Context, @ZZ, 1);
    Context.T[0] := Context.T[0] - 8;
  end;

  Context.T[0] := Context.T[0] - 128;
  BLAKE384512Update(Context, @MsgLen[0], 16);

  U64To8Big(@Digest[0], Context.H[0]);
  U64To8Big(@Digest[8], Context.H[1]);
  U64To8Big(@Digest[16], Context.H[2]);
  U64To8Big(@Digest[24], Context.H[3]);
  U64To8Big(@Digest[32], Context.H[4]);
  U64To8Big(@Digest[40], Context.H[5]);
  U64To8Big(@Digest[48], Context.H[6]);
  U64To8Big(@Digest[56], Context.H[7]);
end;

// �����ݿ���� BLAKE224 ����
function BLAKE224(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE224Digest;
var
  Context: TCnBLAKE224Context;
begin
  BLAKE224Init(Context);
  BLAKE224Update(Context, Input, ByteLength);
  BLAKE224Final(Context, Result);
end;

// �����ݿ���� BLAKE256 ����
function BLAKE256(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE256Digest;
var
  Context: TCnBLAKE256Context;
begin
  BLAKE256Init(Context);
  BLAKE256Update(Context, Input, ByteLength);
  BLAKE256Final(Context, Result);
end;

// �����ݿ���� BLAKE384 ����
function BLAKE384(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE384Digest;
var
  Context: TCnBLAKE384Context;
begin
  BLAKE384Init(Context);
  BLAKE384Update(Context, Input, ByteLength);
  BLAKE384Final(Context, Result);
end;

// �����ݿ���� BLAKE512 ����
function BLAKE512(Input: PAnsiChar; ByteLength: Cardinal): TCnBLAKE512Digest;
var
  Context: TCnBLAKE512Context;
begin
  BLAKE512Init(Context);
  BLAKE512Update(Context, Input, ByteLength);
  BLAKE512Final(Context, Result);
end;

// �����ݿ���� BLAKE224 ����
function BLAKE224Buffer(const Buffer; Count: Cardinal): TCnBLAKE224Digest;
var
  Context: TCnBLAKE224Context;
begin
  BLAKE224Init(Context);
  BLAKE224Update(Context, PAnsiChar(Buffer), Count);
  BLAKE224Final(Context, Result);
end;

// �����ݿ���� BLAKE256 ����
function BLAKE256Buffer(const Buffer; Count: Cardinal): TCnBLAKE256Digest;
var
  Context: TCnBLAKE256Context;
begin
  BLAKE256Init(Context);
  BLAKE256Update(Context, PAnsiChar(Buffer), Count);
  BLAKE256Final(Context, Result);
end;

// �����ݿ���� BLAKE384 ����
function BLAKE384Buffer(const Buffer; Count: Cardinal): TCnBLAKE384Digest;
var
  Context: TCnBLAKE384Context;
begin
  BLAKE384Init(Context);
  BLAKE384Update(Context, PAnsiChar(Buffer), Count);
  BLAKE384Final(Context, Result);
end;

// �����ݿ���� BLAKE512 ����
function BLAKE512Buffer(const Buffer; Count: Cardinal): TCnBLAKE512Digest;
var
  Context: TCnBLAKE512Context;
begin
  BLAKE512Init(Context);
  BLAKE512Update(Context, PAnsiChar(Buffer), Count);
  BLAKE512Final(Context, Result);
end;

// ���ֽ�������� BLAKE224 ����
function BLAKE224Bytes(Data: TBytes): TCnBLAKE224Digest;
var
  Context: TCnBLAKE224Context;
begin
  BLAKE224Init(Context);
  BLAKE224Update(Context, PAnsiChar(@Data[0]), Length(Data));
  BLAKE224Final(Context, Result);
end;

// ���ֽ�������� BLAKE256 ����
function BLAKE256Bytes(Data: TBytes): TCnBLAKE256Digest;
var
  Context: TCnBLAKE256Context;
begin
  BLAKE256Init(Context);
  BLAKE256Update(Context, PAnsiChar(@Data[0]), Length(Data));
  BLAKE256Final(Context, Result);
end;

// ���ֽ�������� BLAKE384 ����
function BLAKE384Bytes(Data: TBytes): TCnBLAKE384Digest;
var
  Context: TCnBLAKE384Context;
begin
  BLAKE384Init(Context);
  BLAKE384Update(Context, PAnsiChar(@Data[0]), Length(Data));
  BLAKE384Final(Context, Result);
end;

// ���ֽ�������� BLAKE512 ����
function BLAKE512Bytes(Data: TBytes): TCnBLAKE512Digest;
var
  Context: TCnBLAKE512Context;
begin
  BLAKE512Init(Context);
  BLAKE512Update(Context, PAnsiChar(@Data[0]), Length(Data));
  BLAKE512Final(Context, Result);
end;

// �� String �������ݽ��� BLAKE224 ����
function BLAKE224String(const Str: string): TCnBLAKE224Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := BLAKE224StringA(AStr);
end;

// �� String �������ݽ��� BLAKE256 ����
function BLAKE256String(const Str: string): TCnBLAKE256Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := BLAKE256StringA(AStr);
end;

// �� String �������ݽ��� BLAKE384 ����
function BLAKE384String(const Str: string): TCnBLAKE384Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := BLAKE384StringA(AStr);
end;

// �� String �������ݽ��� BLAKE512 ����
function BLAKE512String(const Str: string): TCnBLAKE512Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := BLAKE512StringA(AStr);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE224 ���㣬������ת��
{$IFDEF UNICODE}
function BLAKE224UnicodeString(const Str: string): TCnBLAKE224Digest;
{$ELSE}
function BLAKE224UnicodeString(const Str: WideString): TCnBLAKE224Digest;
{$ENDIF}
var
  Context: TCnBLAKE224Context;
begin
  BLAKE224Init(Context);
  BLAKE224Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE224Final(Context, Result);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE256 ���㣬������ת��
{$IFDEF UNICODE}
function BLAKE256UnicodeString(const Str: string): TCnBLAKE256Digest;
{$ELSE}
function BLAKE256UnicodeString(const Str: WideString): TCnBLAKE256Digest;
{$ENDIF}
var
  Context: TCnBLAKE256Context;
begin
  BLAKE256Init(Context);
  BLAKE256Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE256Final(Context, Result);
end;  

// �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE384 ���㣬������ת��
{$IFDEF UNICODE}
function BLAKE384UnicodeString(const Str: string): TCnBLAKE384Digest;
{$ELSE}
function BLAKE384UnicodeString(const Str: WideString): TCnBLAKE384Digest;
{$ENDIF}
var
  Context: TCnBLAKE384Context;
begin
  BLAKE384Init(Context);
  BLAKE384Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE384Final(Context, Result);
end;  

// �� UnicodeString �������ݽ���ֱ�ӵ� BLAKE512 ���㣬������ת��
{$IFDEF UNICODE}
function BLAKE512UnicodeString(const Str: string): TCnBLAKE512Digest;
{$ELSE}
function BLAKE512UnicodeString(const Str: WideString): TCnBLAKE512Digest;
{$ENDIF}
var
  Context: TCnBLAKE512Context;
begin
  BLAKE512Init(Context);
  BLAKE512Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE512Final(Context, Result);
end;

// �� AnsiString �������ݽ��� BLAKE224 ����
function BLAKE224StringA(const Str: AnsiString): TCnBLAKE224Digest;
var
  Context: TCnBLAKE224Context;
begin
  BLAKE224Init(Context);
  BLAKE224Update(Context, PAnsiChar(Str), Length(Str));
  BLAKE224Final(Context, Result);
end;

// �� WideString �������ݽ��� BLAKE224 ����
function BLAKE224StringW(const Str: WideString): TCnBLAKE224Digest;
var
  Context: TCnBLAKE224Context;
begin
  BLAKE224Init(Context);
  BLAKE224256UpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE224Final(Context, Result);
end;

// �� AnsiString �������ݽ��� BLAKE256 ����
function BLAKE256StringA(const Str: AnsiString): TCnBLAKE256Digest;
var
  Context: TCnBLAKE256Context;
begin
  BLAKE256Init(Context);
  BLAKE256Update(Context, PAnsiChar(Str), Length(Str));
  BLAKE256Final(Context, Result);
end;

// �� WideString �������ݽ��� BLAKE256 ����
function BLAKE256StringW(const Str: WideString): TCnBLAKE256Digest;
var
  Context: TCnBLAKE256Context;
begin
  BLAKE256Init(Context);
  BLAKE224256UpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE256Final(Context, Result);
end;

// �� AnsiString �������ݽ��� BLAKE384 ����
function BLAKE384StringA(const Str: AnsiString): TCnBLAKE384Digest;
var
  Context: TCnBLAKE384Context;
begin
  BLAKE384Init(Context);
  BLAKE384Update(Context, PAnsiChar(Str), Length(Str));
  BLAKE384Final(Context, Result);
end;

// �� WideString �������ݽ��� BLAKE384 ����
function BLAKE384StringW(const Str: WideString): TCnBLAKE384Digest;
var
  Context: TCnBLAKE384Context;
begin
  BLAKE384Init(Context);
  BLAKE384512UpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE384Final(Context, Result);
end;

// �� AnsiString �������ݽ��� BLAKE512 ����
function BLAKE512StringA(const Str: AnsiString): TCnBLAKE512Digest;
var
  Context: TCnBLAKE512Context;
begin
  BLAKE512Init(Context);
  BLAKE512Update(Context, PAnsiChar(Str), Length(Str));
  BLAKE512Final(Context, Result);
end;

// �� WideString �������ݽ��� BLAKE512 ����
function BLAKE512StringW(const Str: WideString): TCnBLAKE512Digest;
var
  Context: TCnBLAKE512Context;
begin
  BLAKE512Init(Context);
  BLAKE384512UpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE512Final(Context, Result);
end;

function InternalBLAKEStream(Stream: TStream; const BufSize: Cardinal; var D:
  TCnBLAKEGeneralDigest; BLAKEType: TBLAKEType; CallBack: TCnBLAKECalcProgressFunc): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;

  Context224: TCnBLAKE224Context;
  Context256: TCnBLAKE256Context;
  Context384: TCnBLAKE384Context;
  Context512: TCnBLAKE512Context;
  Dig224: TCnBLAKE224Digest;
  Dig256: TCnBLAKE256Digest;
  Dig384: TCnBLAKE384Digest;
  Dig512: TCnBLAKE512Digest;

  procedure _BLAKEInit;
  begin
    case BLAKEType of
      btBLAKE224:
        BLAKE224Init(Context224);
      btBLAKE256:
        BLAKE256Init(Context256);
      btBLAKE384:
        BLAKE384Init(Context384);
      btBLAKE512:
        BLAKE512Init(Context512);
    end;
  end;

  procedure _BLAKEUpdate;
  begin
    case BLAKEType of
      btBLAKE224:
        BLAKE224Update(Context224, Buf, ReadBytes);
      btBLAKE256:
        BLAKE256Update(Context256, Buf, ReadBytes);
      btBLAKE384:
        BLAKE384Update(Context384, Buf, ReadBytes);
      btBLAKE512:
        BLAKE512Update(Context512, Buf, ReadBytes);
    end;
  end;

  procedure _BLAKEFinal;
  begin
    case BLAKEType of
      btBLAKE224:
        BLAKE224Final(Context224, Dig224);
      btBLAKE256:
        BLAKE256Final(Context256, Dig256);
      btBLAKE384:
        BLAKE384Final(Context384, Dig384);
      btBLAKE512:
        BLAKE512Final(Context512, Dig512);
    end;
  end;

  procedure _CopyResult;
  begin
    case BLAKEType of
      btBLAKE224:
        Move(Dig224[0], D[0], SizeOf(TCnBLAKE224Digest));
      btBLAKE256:
        Move(Dig256[0], D[0], SizeOf(TCnBLAKE256Digest));
      btBLAKE384:
        Move(Dig384[0], D[0], SizeOf(TCnBLAKE384Digest));
      btBLAKE512:
        Move(Dig512[0], D[0], SizeOf(TCnBLAKE512Digest));
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
  _BLAKEInit;
 
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        _BLAKEUpdate;

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    _BLAKEFinal;
    _CopyResult;
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// ��ָ�������� BLAKE224 ����
function BLAKE224Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE224Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  InternalBLAKEStream(Stream, 4096 * 1024, Dig, btBLAKE224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE224Digest));
end;

// ��ָ�������� BLAKE256 ����
function BLAKE256Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE256Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  InternalBLAKEStream(Stream, 4096 * 1024, Dig, btBLAKE256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE256Digest));
end;

// ��ָ�������� BLAKE384 ����
function BLAKE384Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE384Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  InternalBLAKEStream(Stream, 4096 * 1024, Dig, btBLAKE384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE384Digest));
end;

// ��ָ�������� BLAKE512 ����
function BLAKE512Stream(Stream: TStream; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE512Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  InternalBLAKEStream(Stream, 4096 * 1024, Dig, btBLAKE512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE512Digest));
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

function InternalBLAKEFile(const FileName: string; BLAKEType: TBLAKEType;
  CallBack: TCnBLAKECalcProgressFunc): TCnBLAKEGeneralDigest;
var
  Context224: TCnBLAKE224Context;
  Context256: TCnBLAKE256Context;
  Context384: TCnBLAKE384Context;
  Context512: TCnBLAKE512Context;
  Dig224: TCnBLAKE224Digest;
  Dig256: TCnBLAKE256Digest;
  Dig384: TCnBLAKE384Digest;
  Dig512: TCnBLAKE512Digest;

{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  procedure _BLAKEInit;
  begin
    case BLAKEType of
      btBLAKE224:
        BLAKE224Init(Context224);
      btBLAKE256:
        BLAKE256Init(Context256);
      btBLAKE384:
        BLAKE384Init(Context384);
      btBLAKE512:
        BLAKE512Init(Context512);
    end;
  end;

{$IFDEF MSWINDOWS}
  procedure _BLAKEUpdate;
  begin
    case BLAKEType of
      btBLAKE224:
        BLAKE224Update(Context224, ViewPointer, GetFileSize(FileHandle, nil));
      btBLAKE256:
        BLAKE256Update(Context256, ViewPointer, GetFileSize(FileHandle, nil));
      btBLAKE384:
        BLAKE384Update(Context384, ViewPointer, GetFileSize(FileHandle, nil));
      btBLAKE512:
        BLAKE512Update(Context512, ViewPointer, GetFileSize(FileHandle, nil));
    end;
  end;
{$ENDIF}

  procedure _BLAKEFinal;
  begin
    case BLAKEType of
      btBLAKE224:
        BLAKE224Final(Context224, Dig224);
      btBLAKE256:
        BLAKE256Final(Context256, Dig256);
      btBLAKE384:
        BLAKE384Final(Context384, Dig384);
      btBLAKE512:
        BLAKE512Final(Context512, Dig512);
    end;
  end;

  procedure _CopyResult(var D: TCnBLAKEGeneralDigest);
  begin
    case BLAKEType of
      btBLAKE224:
        Move(Dig224[0], D[0], SizeOf(TCnBLAKE224Digest));
      btBLAKE256:
        Move(Dig256[0], D[0], SizeOf(TCnBLAKE256Digest));
      btBLAKE384:
        Move(Dig384[0], D[0], SizeOf(TCnBLAKE384Digest));
      btBLAKE512:
        Move(Dig512[0], D[0], SizeOf(TCnBLAKE512Digest));
    end;
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���� Windows ƽ̨����������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalBLAKEStream(Stream, 4096 * 1024, Result, BLAKEType, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    _BLAKEInit;
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
                _BLAKEUpdate;
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
    _BLAKEFinal;
    _CopyResult(Result);
{$ENDIF}
  end;
end;

// ��ָ���ļ����ݽ��� BLAKE224 ����
function BLAKE224File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE224Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  Dig := InternalBLAKEFile(FileName, btBLAKE224, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE224Digest));
end;

// ��ָ���ļ����ݽ��� BLAKE256 ����
function BLAKE256File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE256Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  Dig := InternalBLAKEFile(FileName, btBLAKE256, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE256Digest));
end;

// ��ָ���ļ����ݽ��� BLAKE384 ����
function BLAKE384File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE384Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  Dig := InternalBLAKEFile(FileName, btBLAKE384, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE384Digest));
end;

// ��ָ���ļ����ݽ��� BLAKE512 ����
function BLAKE512File(const FileName: string; CallBack: TCnBLAKECalcProgressFunc):
  TCnBLAKE512Digest;
var
  Dig: TCnBLAKEGeneralDigest;
begin
  Dig := InternalBLAKEFile(FileName, btBLAKE512, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnBLAKE512Digest));
end;

// ��ʮ�����Ƹ�ʽ��� BLAKE224 �Ӵ�ֵ
function BLAKE224Print(const Digest: TCnBLAKE224Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnBLAKE224Digest));
end;

// ��ʮ�����Ƹ�ʽ��� BLAKE256 �Ӵ�ֵ
function BLAKE256Print(const Digest: TCnBLAKE256Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnBLAKE256Digest));
end;

// ��ʮ�����Ƹ�ʽ��� BLAKE384 �Ӵ�ֵ
function BLAKE384Print(const Digest: TCnBLAKE384Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnBLAKE384Digest));
end;

// ��ʮ�����Ƹ�ʽ��� BLAKE512 �Ӵ�ֵ
function BLAKE512Print(const Digest: TCnBLAKE512Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnBLAKE512Digest));
end;

// �Ƚ����� BLAKE224 �Ӵ�ֵ�Ƿ����
function BLAKE224Match(const D1, D2: TCnBLAKE224Digest): Boolean;
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

// �Ƚ����� BLAKE256 �Ӵ�ֵ�Ƿ����
function BLAKE256Match(const D1, D2: TCnBLAKE256Digest): Boolean;
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

// �Ƚ����� BLAKE384 �Ӵ�ֵ�Ƿ����
function BLAKE384Match(const D1, D2: TCnBLAKE384Digest): Boolean;
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

// �Ƚ����� BLAKE512 �Ӵ�ֵ�Ƿ����
function BLAKE512Match(const D1, D2: TCnBLAKE512Digest): Boolean;
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

// BLAKE224 �Ӵ�ֵת string
function BLAKE224DigestToStr(const Digest: TCnBLAKE224Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnBLAKE224Digest));
end;

// BLAKE256 �Ӵ�ֵת string
function BLAKE256DigestToStr(const Digest: TCnBLAKE256Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnBLAKE256Digest));
end;

// BLAKE384 �Ӵ�ֵת string
function BLAKE384DigestToStr(const Digest: TCnBLAKE384Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnBLAKE384Digest));
end;

// BLAKE512 �Ӵ�ֵת string
function BLAKE512DigestToStr(const Digest: TCnBLAKE512Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnBLAKE512Digest));
end;

procedure BLAKE224HmacInit(var Context: TCnBLAKE224Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnBLAKE224Digest;
begin
  if KeyLength > HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE then
  begin
    Sum := BLAKE224Buffer(Key, KeyLength);
    KeyLength := HMAC_BLAKE_224_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  BLAKE224Init(Context);
  BLAKE224Update(Context, @(Context.Ipad[0]), HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE);
end;

procedure BLAKE224HmacUpdate(var Context: TCnBLAKE224Context; Input: PAnsiChar; Length:
  Cardinal);
begin
  BLAKE224Update(Context, Input, Length);
end;

procedure BLAKE224HmacFinal(var Context: TCnBLAKE224Context; var Output: TCnBLAKE224Digest);
var
  Len: Integer;
  TmpBuf: TCnBLAKE224Digest;
begin
  Len := HMAC_BLAKE_224_OUTPUT_LENGTH_BYTE;
  BLAKE224Final(Context, TmpBuf);
  BLAKE224Init(Context);
  BLAKE224Update(Context, @(Context.Opad[0]), HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE);
  BLAKE224Update(Context, @(TmpBuf[0]), Len);
  BLAKE224Final(Context, Output);
end;

procedure BLAKE256HmacInit(var Context: TCnBLAKE256Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnBLAKE256Digest;
begin
  if KeyLength > HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE then
  begin
    Sum := BLAKE256Buffer(Key, KeyLength);
    KeyLength := HMAC_BLAKE_256_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  BLAKE256Init(Context);
  BLAKE256Update(Context, @(Context.Ipad[0]), HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE);
end;

procedure BLAKE256HmacUpdate(var Context: TCnBLAKE256Context; Input: PAnsiChar; Length:
  Cardinal);
begin
  BLAKE256Update(Context, Input, Length);
end;

procedure BLAKE256HmacFinal(var Context: TCnBLAKE256Context; var Output: TCnBLAKE256Digest);
var
  Len: Integer;
  TmpBuf: TCnBLAKE256Digest;
begin
  Len := HMAC_BLAKE_256_OUTPUT_LENGTH_BYTE;
  BLAKE256Final(Context, TmpBuf);
  BLAKE256Init(Context);
  BLAKE256Update(Context, @(Context.Opad[0]), HMAC_BLAKE_224_256_BLOCK_SIZE_BYTE);
  BLAKE256Update(Context, @(TmpBuf[0]), Len);
  BLAKE256Final(Context, Output);
end;

procedure BLAKE224Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE224Digest);
var
  Context: TCnBLAKE224Context;
begin
  BLAKE224HmacInit(Context, Key, KeyByteLength);
  BLAKE224HmacUpdate(Context, Input, ByteLength);
  BLAKE224HmacFinal(Context, Output);
end;

procedure BLAKE256Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE256Digest);
var
  Context: TCnBLAKE256Context;
begin
  BLAKE256HmacInit(Context, Key, KeyByteLength);
  BLAKE256HmacUpdate(Context, Input, ByteLength);
  BLAKE256HmacFinal(Context, Output);
end;

procedure BLAKE384HmacInit(var Context: TCnBLAKE384Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnBLAKE384Digest;
begin
  if KeyLength > HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE then
  begin
    Sum := BLAKE384Buffer(Key, KeyLength);
    KeyLength := HMAC_BLAKE_384_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  BLAKE384Init(Context);
  BLAKE384Update(Context, @(Context.Ipad[0]), HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE);
end;

procedure BLAKE384HmacUpdate(var Context: TCnBLAKE384Context; Input: PAnsiChar;
  Length: Cardinal);
begin
  BLAKE384Update(Context, Input, Length);
end;

procedure BLAKE384HmacFinal(var Context: TCnBLAKE384Context; var Output: TCnBLAKE384Digest);
var
  Len: Integer;
  TmpBuf: TCnBLAKE384Digest;
begin
  Len := HMAC_BLAKE_384_OUTPUT_LENGTH_BYTE;
  BLAKE384Final(Context, TmpBuf);
  BLAKE384Init(Context);
  BLAKE384Update(Context, @(Context.Opad[0]), HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE);
  BLAKE384Update(Context, @(TmpBuf[0]), Len);
  BLAKE384Final(Context, Output);
end;

procedure BLAKE384Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE384Digest);
var
  Context: TCnBLAKE384Context;
begin
  BLAKE384HmacInit(Context, Key, KeyByteLength);
  BLAKE384HmacUpdate(Context, Input, ByteLength);
  BLAKE384HmacFinal(Context, Output);
end;

procedure BLAKE512HmacInit(var Context: TCnBLAKE512Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnBLAKE512Digest;
begin
  if KeyLength > HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE then
  begin
    Sum := BLAKE512Buffer(Key, KeyLength);
    KeyLength := HMAC_BLAKE_512_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE, $36);
  FillChar(Context.Opad, HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  BLAKE512Init(Context);
  BLAKE512Update(Context, @(Context.Ipad[0]), HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE);
end;

procedure BLAKE512HmacUpdate(var Context: TCnBLAKE512Context; Input: PAnsiChar;
  Length: Cardinal);
begin
  BLAKE512Update(Context, Input, Length);
end;

procedure BLAKE512HmacFinal(var Context: TCnBLAKE512Context; var Output: TCnBLAKE512Digest);
var
  Len: Integer;
  TmpBuf: TCnBLAKE512Digest;
begin
  Len := HMAC_BLAKE_512_OUTPUT_LENGTH_BYTE;
  BLAKE512Final(Context, TmpBuf);
  BLAKE512Init(Context);
  BLAKE512Update(Context, @(Context.Opad[0]), HMAC_BLAKE_384_512_BLOCK_SIZE_BYTE);
  BLAKE512Update(Context, @(TmpBuf[0]), Len);
  BLAKE512Final(Context, Output);
end;

procedure BLAKE512Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnBLAKE512Digest);
var
  Context: TCnBLAKE512Context;
begin
  BLAKE512HmacInit(Context, Key, KeyByteLength);
  BLAKE512HmacUpdate(Context, Input, ByteLength);
  BLAKE512HmacFinal(Context, Output);
end;

end.
