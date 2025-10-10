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
(******************************************************************************)
(*                                                                            *)
(*                    Advanced Encryption Standard (AES)                      *)
(*                                                                            *)
(*                    Copyright (c) 1998-2001                                 *)
(*                    EldoS, Alexander Ionov                                  *)
(*                                                                            *)
(******************************************************************************)

unit CnAES;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�AES �ԳƼӽ����㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
*           �� EldoS, Alexander Ionov �ĵ�Ԫ��ֲ���������书�ܣ�����ԭ�а�Ȩ��Ϣ
* ��    ע������Ԫʵ���� AES 128/192/256 �ԳƼӽ����㷨���ֿ��С�̶� 16 �ֽڣ���ģʽ
*           �Ķ��뷽ʽ����ĩβ�� 0������Ԫ�ڲ���֧�� PKCS �ȿ���뷽ʽ������Ҫ�������ⲿ����
*           CnPemUtils.pas ��Ԫ�е� PKCS ϵ�к����Լӽ������ݽ��ж��⴦��
*
*           ����߰汾 Delphi ���뾡������ʹ�� AnsiString �����汾�ĺ�����ʮ�����Ƴ��⣩��
*           ���ⲻ�����ַ���������Ӱ��ӽ��ܽ����
*
*           ���䣺Java ��Ĭ�ϵ� AES ��Ӧ�˴��� AES256
*
*           ���⣬C++Builder 5/6 �¶� overload �ĺ�������ʴ����жϴ���Ӷ�����
*           ���ҵ����Σ��ʱ���Ԫ���˴������� overload �������� Delphi �´��ڣ�
*           �����ٷ�װ�˲��ֲ�ͬ���ĺ�����֧�� C++Builder 5/6 �±������С�
*
*           ECB/CBC �ǿ�ģʽ����Ҫ������롣CFB/OFB/CTR ������������ĵ���ģʽ��������뵽�顣
*
*           ���⣬CTR ģʽ���� RFC 3686 �淶������紫�ݵ� 4 �ֽ� Nonce��8 �ֽ�
*           ��ʼ��������4 �ֽ������ֽ���ļ�������ƴ�� 16 �ֽڵ������ĳ�ʼ������
*           ���� AES ��������㣬�ӽ��ܾ�Ϊ����������Ķ�����
*
* ����ƽ̨��Delphi5 + Win 7
* �޸ļ�¼��2024.07.25 V1.3
*               ���� CTR ģʽ��֧�֣���ѭ RFC 3686 �淶
*           2024.05.26 V1.2
*               ���䲿��֧�� C++Builder �ĺ���
*           2022.06.21 V1.1
*               ���뼸���ֽ����鵽ʮ�������ַ���֮��ļӽ��ܺ���
*           2021.12.11 V1.2
*               ���� CFB/OFB ģʽ��֧��
*           2019.04.15 V1.1
*               ֧�� Win32/Win64/MacOS
*           2015.01.21 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative;

const
  CN_AES_BLOCKSIZE = 16;
  {* AES �ķ�����ܿ��С����������λ�����٣���Ϊ 16 �ֽ�}

type
  TCnKeyBitType = (kbt128, kbt192, kbt256);
  {* AES ����������λ����16 �ֽڡ�24 �ֽں� 32 �ֽ�}

  ECnAESException = class(Exception);
  {* AES ����쳣}

  TCnAESBuffer = array [0..15] of Byte;
  {* AES �ӽ��ܿ� 16 �ֽ�}

  TCnAESKey128 = array [0..15] of Byte;
  {* AES128 ����Կ�ṹ��16 �ֽ�}

  TCnAESKey192 = array [0..23] of Byte;
  {* AES192 ����Կ�ṹ��24 �ֽ�}

  TCnAESKey256 = array [0..31] of Byte;
  {* AES256 ����Կ�ṹ��32 �ֽ�}

  TCnAESExpandedKey128 = array [0..43] of Cardinal;
  {* AES128 ����չ��Կ�ṹ}

  TCnAESExpandedKey192 = array [0..53] of Cardinal;
  {* AES192 ����չ��Կ�ṹ}

  TCnAESExpandedKey256 = array [0..63] of Cardinal;
  {* AES256 ����չ��Կ�ṹ}

  PCnAESBuffer = ^TCnAESBuffer;
  {* AES �ӽ��ܿ�ָ��}

  PCnAESKey128 = ^TCnAESKey128;
  {* AES128 ����Կ�ṹָ��}

  PCnAESKey192 = ^TCnAESKey192;
  {* AES192 ����Կ�ṹָ��}

  PCnAESKey256 = ^TCnAESKey256;
  {* AES256 ����Կ�ṹָ��}

  PCnAESExpandedKey128 = ^TCnAESExpandedKey128;
  {* AES128 ����չ��Կ�ṹָ��}

  PCnAESExpandedKey192 = ^TCnAESExpandedKey192;
  {* AES192 ����չ��Կ�ṹָ��}

  PCnAESExpandedKey256 = ^TCnAESExpandedKey256;
  {* AES256 ����չ��Կ�ṹָ��}

  TCnAESCTRNonce = array[0..3] of Byte;
  {* CTR ģʽ�µ� Nonce �ṹ��4 �ֽ�}

  TCnAESCTRIv = array[0..7] of Byte;
  {* CTR ģʽ�µĳ�ʼ�������ṹ��8 �ֽ�}

// Key Expansion Routines for Encryption

procedure ExpandAESKeyForEncryption128(const Key: TCnAESKey128;
  var ExpandedKey: TCnAESExpandedKey128);
{* �ڼ��ܳ�������չ AES128 ����Կ��

   ������
     const Key: TCnAESKey128                              - ����չ�� AES128 ��Կ
     var ExpandedKey: TCnAESExpandedKey128                - ������չ����� AES128 ��չ��Կ

   ����ֵ�����ޣ�
}

procedure ExpandAESKeyForEncryption192(const Key: TCnAESKey192;
  var ExpandedKey: TCnAESExpandedKey192);
{* �ڼ��ܳ�������չ AES192 ����Կ��

   ������
     const Key: TCnAESKey192                              - ����չ�� AES192 ��Կ
     var ExpandedKey: TCnAESExpandedKey192                - ������չ����� AES192 ��չ��Կ

   ����ֵ�����ޣ�
}

procedure ExpandAESKeyForEncryption256(const Key: TCnAESKey256;
  var ExpandedKey: TCnAESExpandedKey256);
{* �ڼ��ܳ�������չ AES256 ����Կ��

   ������
     const Key: TCnAESKey256                              - ����չ�� AES256 ��Կ
     var ExpandedKey: TCnAESExpandedKey256                - ������չ����� AES256 ��չ��Կ

   ����ֵ�����ޣ�
}

// Block Encryption Routines ��������ܣ�InBuf �� OutBuf ������ͬһ������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬���������� Delphi �¿���
procedure EncryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer); overload;
{* AES128 ���ܿ飬���� Delphi �¿��á�

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey128      - ��չ AES128 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}
procedure EncryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer); overload;
{* AES192 ���ܿ飬���� Delphi �¿��á�

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey192      - ��չ AES192 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}
procedure EncryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer); overload;
{* AES256 ���ܿ飬���� Delphi �¿��á�

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey256      - ��չ AES256 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}

{$ENDIF}

// ������������Delphi �� C++Builder �¾�����
procedure EncryptAES128(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer);
{* AES128 ���ܿ飬InBuf �� OutBuf ������ͬһ������

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey128      - ��չ AES128 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}
procedure EncryptAES192(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer);
{* AES192 ���ܿ飬InBuf �� OutBuf ������ͬһ������

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey192      - ��չ AES192 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}
procedure EncryptAES256(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer);
{* AES256 ���ܿ飬InBuf �� OutBuf ������ͬһ������

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey256      - ��չ AES256 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}

// Stream Encryption Routines (ECB mode) ECB �����

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream); overload;
{* AES128 ECB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream); overload;
{* AES128 ECB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream); overload;
{* AES256 ECB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream); overload;
{* AES192 ECB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream); overload;
{* AES256 ECB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream); overload;
{* AES256 ECB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure EncryptAES128StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream);
{* AES128 ECB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES128StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream);
{* AES128 ECB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAES192StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream);
{* AES192 ECB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES192StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream);
{* AES192 ECB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAES256StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream);
{* AES256 ECB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES256StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream);
{* AES256 ECB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Encryption Routines (CBC mode) CBC �����

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES128 CBC ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES128 CBC ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES192 CBC ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������ 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES192 CBC ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES256 CBC ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES256 CBC ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure EncryptAES128StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES128 CBC ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES128StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES128 CBC ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAES192StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES192 CBC ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES192StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES192 CBC ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAES256StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES256 CBC ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES256StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES256 CBC ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Encryption Routines (CFB mode) CFB ������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES128 CFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES128 CFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES192 CFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES192 CFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES256 CFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES256 CFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure EncryptAES128StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES128 CFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES128StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES128 CFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES192StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES192 CFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES192StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES192 CFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES256StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES256 CFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES256StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES256 CFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Encryption Routines (OFB mode) OFB ������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES128 OFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES128 OFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES192 OFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES192 OFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES256 OFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES256 OFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure EncryptAES128StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES128 OFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES128StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES128 OFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAES192StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES192 OFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - ���������??

   ����ֵ�����ޣ�
}
procedure EncryptAES192StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES192 OFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure EncryptAES256StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES256 OFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES256StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES256 OFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Encryption Routines (CTR mode) CTR ������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES128 CTR ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES128 CTR ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES192 CTR ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES192 CTR ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES256 CTR ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES256 CTR ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure EncryptAES128StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES128 CTR ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES128StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES128 CTR ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES192StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES192 CTR ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES192StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES192 CTR ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES256StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES256 CTR ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure EncryptAES256StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES256 CTR ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Key Transformation Routines for Decryption

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure ExpandAESKeyForDecryption(var ExpandedKey: TCnAESExpandedKey128); overload;
{* �ڽ��ܳ�������չ AES128 ����Կ��

   ������
     var ExpandedKey: TCnAESExpandedKey128                - ����չ�� AES128 ��չ��Կ

   ����ֵ�����ޣ�
}

procedure ExpandAESKeyForDecryption(const Key: TCnAESKey128;
  var ExpandedKey: TCnAESExpandedKey128); overload;
{* �ڽ��ܳ�������չ AES128 ����Կ��

   ������
     const Key: TCnAESKey128                              - ����չ�� AES128 ��Կ
     var ExpandedKey: TCnAESExpandedKey128                - ������չ����� AES128 ��չ��Կ

   ����ֵ�����ޣ�
}

procedure ExpandAESKeyForDecryption(var ExpandedKey: TCnAESExpandedKey192); overload;
{* �ڽ��ܳ�������չ AES192 ����Կ��

   ������
     var ExpandedKey: TCnAESExpandedKey192                - ����չ�� AES192 ��չ��Կ

   ����ֵ�����ޣ�
}
procedure ExpandAESKeyForDecryption(const Key: TCnAESKey192;
  var ExpandedKey: TCnAESExpandedKey192); overload;
{* �ڽ��ܳ�������չ AES192 ����Կ��

   ������
     const Key: TCnAESKey192                              - ����չ�� AES192 ��Կ
     var ExpandedKey: TCnAESExpandedKey192                - ������չ����� AES192 ��չ��Կ

   ����ֵ�����ޣ�
}

procedure ExpandAESKeyForDecryption(var ExpandedKey: TCnAESExpandedKey256); overload;
{* �ڽ��ܳ�������չ AES256 ����Կ��

   ������
     var ExpandedKey: TCnAESExpandedKey256                - ����չ�� AES256 ��չ��Կ

   ����ֵ�����ޣ�
}
procedure ExpandAESKeyForDecryption(const Key: TCnAESKey256;
  var ExpandedKey: TCnAESExpandedKey256); overload;
{* �ڽ��ܳ�������չ AES256 ����Կ��

   ������
     const Key: TCnAESKey256                              - ����չ�� AES256 ��Կ
     var ExpandedKey: TCnAESExpandedKey256                - ������չ����� AES256 ��չ��Կ

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure ExpandAESKeyForDecryption128(var ExpandedKey: TCnAESExpandedKey128);
{* �ڽ��ܳ�������չ AES128 ����Կ��

   ������
     var ExpandedKey: TCnAESExpandedKey128                - ����չ�� AES128 ��չ��Կ

   ����ֵ�����ޣ�
}
procedure ExpandAESKeyForDecryption128Expanded(const Key: TCnAESKey128;
  var ExpandedKey: TCnAESExpandedKey128);
{* �ڽ��ܳ�������չ AES128 ����Կ��

   ������
     const Key: TCnAESKey128                              - ����չ�� AES128 ��Կ
     var ExpandedKey: TCnAESExpandedKey128                - ������չ����� AES128 ��չ��Կ

   ����ֵ�����ޣ�
}

procedure ExpandAESKeyForDecryption192(var ExpandedKey: TCnAESExpandedKey192);
{* �ڽ��ܳ�������չ AES192 ����Կ��

   ������
     var ExpandedKey: TCnAESExpandedKey192                - ����չ�� AES192 ��չ��Կ

   ����ֵ�����ޣ�
}
procedure ExpandAESKeyForDecryption192Expanded(const Key: TCnAESKey192;
  var ExpandedKey: TCnAESExpandedKey192);
{* �ڽ��ܳ�������չ AES192 ����Կ��

   ������
     const Key: TCnAESKey192                              - ����չ�� AES192 ��Կ
     var ExpandedKey: TCnAESExpandedKey192                - ������չ����� AES192 ��չ��Կ

   ����ֵ�����ޣ�
}

procedure ExpandAESKeyForDecryption256(var ExpandedKey: TCnAESExpandedKey256);
{* �ڽ��ܳ�������չ AES256 ����Կ��

   ������
     var ExpandedKey: TCnAESExpandedKey256                - ����չ�� AES256 ��չ��Կ

   ����ֵ�����ޣ�
}
procedure ExpandAESKeyForDecryption256Expanded(const Key: TCnAESKey256;
  var ExpandedKey: TCnAESExpandedKey256);
{* �ڽ��ܳ�������չ AES256 ����Կ��

   ������
     const Key: TCnAESKey256                              - ����չ�� AES256 ��Կ
     var ExpandedKey: TCnAESExpandedKey256                - ������չ����� AES256 ��չ��Կ

   ����ֵ�����ޣ�
}

// Block Decryption Routines  ���������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬���������� Delphi �¿���
procedure DecryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer); overload;
{* AES128 ���ܿ飬���� Delphi �¿��á�

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey128      - ��չ AES128 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}

procedure DecryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer); overload;
{* AES192 ���ܿ飬���� Delphi �¿��á�

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey192      - ��չ AES192 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}

procedure DecryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer); overload;
{* AES256 ���ܿ飬���� Delphi �¿��á�

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey256      - ��չ AES256 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}

{$ENDIF}

// ������������Delphi �� C++Builder �¾�����
procedure DecryptAES128(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer);
{* AES128 ���ܿ飬InBuf �� OutBuf ������ͬһ������

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey128      - ��չ AES128 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}
procedure DecryptAES192(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer);
{* AES192 ���ܿ飬InBuf �� OutBuf ������ͬһ������

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey192      - ��չ AES192 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}
procedure DecryptAES256(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer);
{* AES256 ���ܿ飬InBuf �� OutBuf ������ͬһ������

   ������
     const InBuf: TCnAESBuffer            - �����ܵ��������ݿ�
     const Key: TCnAESExpandedKey256      - ��չ AES256 ��Կ
     var OutBuf: TCnAESBuffer             - �������ĵ����ݿ�

   ����ֵ�����ޣ�
}

// Stream Decryption Routines (ECB mode) ECB �����

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream); overload;
{* AES128 ECB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream); overload;
{* AES128 ECB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream); overload;
{* AES192 ECB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream); overload;
{* AES192 ECB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream); overload;
{* AES256 ECB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream); overload;
{* AES256 ECB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure DecryptAES128StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream);
{* AES128 ECB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES128StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream);
{* AES128 ECB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAES192StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream);
{* AES192 ECB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream);
{* AES192 ECB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAES256StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream);
{* AES256 ECB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream);
{* AES156 ECB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Decryption Routines (CBC mode) CBC �����

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES128 CBC ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES128 CBC ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES192 CBC ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES192 CBC ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES256 CBC ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES256 CBC ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure DecryptAES128StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES128 CBC ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES128StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES128 CBC ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES192 CBC ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES192 CBC ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES256 CBC ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES256 CBC ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Decryption Routines (CFB mode) CFB ������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES128 CFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES128 CFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES192 CFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES192 CFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES256 CFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES256 CFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure DecryptAES128StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES128 CFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES128StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES128 CFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES192 CFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES192 CFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES256 CFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES256 CFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Decryption Routines (OFB mode) OFB ������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES128 OFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES128 OFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES192 OFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES192 OFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream); overload;
{* AES256 OFB ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream); overload;
{* AES256 OFB ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure DecryptAES128StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES128 OFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES128StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES128 OFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES192 OFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES192 OFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
{* AES256 OFB ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const InitVector: TCnAESBuffer       - 16 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
{* AES256 OFB ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const InitVector: TCnAESBuffer                       - 16 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// Stream Decryption Routines (CTR mode) CTR ������

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES128 CTR ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES128 CTR ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES192 CTR ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES192 CTR ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES256 CTR ģʽ������������ Delphi �¿��á�

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream); overload;
{* AES256 CTR ģʽ��������ʹ����չ��Կ������ Delphi �¿��á�

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

{$ENDIF}

// ��������������Delphi �� C++Builder �¾�����
procedure DecryptAES128StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES128 CTR ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey128              - 16 �ֽ� AES128 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES128StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES128 CTR ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey128              - ��չ AES128 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES192 CTR ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey192              - 24 �ֽ� AES192 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES192StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES192 CTR ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey192              - ��չ AES192 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES256 CTR ģʽ��������

   ������
     Source: TStream                      - �����ܵ�������
     Count: Cardinal                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const Key: TCnAESKey256              - 32 �ֽ� AES256 ��Կ
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv        - 8 �ֽڳ�ʼ������
     Dest: TStream                        - �����������

   ����ֵ�����ޣ�
}
procedure DecryptAES256StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
{* AES256 CTR ģʽ��������ʹ����չ��Կ��

   ������
     Source: TStream                                      - �����ܵ�������
     Count: Cardinal                                      - ������ǰλ����Ĵ����ܵ��ֽڳ��ȣ���Ϊ 0����ʾ��ͷ����������
     const ExpandedKey: TCnAESExpandedKey256              - ��չ AES256 ��Կ
     const Nonce: TCnAESCTRNonce                          - 4 �ֽ� Nonce
     const InitVector: TCnAESCTRIv                        - 8 �ֽڳ�ʼ������
     Dest: TStream                                        - �����������

   ����ֵ�����ޣ�
}

// ============== �����ַ���������ʮ�������ַ���֮��ļӽ��� ===================

function AESEncryptEcbStrToHex(Value: AnsiString; Key: AnsiString;
  KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES ECB ģʽ�����ַ���������ת����ʮ�����ơ�

   ������
     Value: AnsiString                    - �����ܵ������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptEcbStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES ECB ����ʮ�������ַ�����

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ���������ַ���
}

function AESEncryptCbcStrToHex(Value: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CBC ģʽ�����ַ���������ת����ʮ�����ơ�

   ������
     Value: AnsiString                    - �����ܵ������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Iv: TCnAESBuffer               - 16 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptCbcStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CBC ����ʮ�������ַ�����

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Iv: TCnAESBuffer               - 16 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ���������ַ���
}

function AESEncryptCfbStrToHex(Value: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CFB ģʽ�����ַ���������ת����ʮ�����ơ�

   ������
     Value: AnsiString                    - �����ܵ������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Iv: TCnAESBuffer               - 16 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptCfbStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CFB ����ʮ�������ַ�����

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Iv: TCnAESBuffer               - 16 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ���������ַ���
}

function AESEncryptOfbStrToHex(Value: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES OFB ģʽ�����ַ���������ת����ʮ�����ơ�

   ������
     Value: AnsiString                    - �����ܵ������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Iv: TCnAESBuffer               - 16 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptOfbStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES OFB ����ʮ�������ַ�����

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Iv: TCnAESBuffer               - 16 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ���������ַ���
}

function AESEncryptCtrStrToHex(Value: AnsiString; Key: AnsiString;
  const Nonce: TCnAESCTRNonce; const Iv: TCnAESCTRIv; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CTR ģʽ�����ַ���������ת����ʮ�����ơ�

   ������
     Value: AnsiString                    - �����ܵ������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const Iv: TCnAESCTRIv                - 8 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptCtrStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Nonce: TCnAESCTRNonce; const Iv: TCnAESCTRIv; KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CTR ����ʮ�������ַ�����

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: AnsiString                      - AES ��Կ�ַ��������ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� #0
     const Nonce: TCnAESCTRNonce          - 4 �ֽ� Nonce
     const Iv: TCnAESCTRIv                - 8 �ֽڳ�ʼ������
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ���������ַ���
}

// ================= �����ֽ������������ֽ�����֮��ļӽ��� ====================

function AESEncryptEcbBytes(Value: TBytes; Key: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES ECB ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESDecryptEcbBytes(Value: TBytes; Key: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES ECB ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptCbcBytes(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CBC ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESDecryptCbcBytes(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CBC ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptCfbBytes(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CFB ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESDecryptCfbBytes(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CFB ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptOfbBytes(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES OFB ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESDecryptOfbBytes(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES OFB ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptCtrBytes(Value: TBytes; Key: TBytes; Nonce: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CTR ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Nonce: TBytes                        - 4 �ֽ� Nonce ���飬̫����ضϣ������� 0
     Iv: TBytes                           - 8 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESDecryptCtrBytes(Value: TBytes; Key: TBytes; Nonce: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CTR ģʽ�����ֽ����顣

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Nonce: TBytes                        - 4 �ֽ� Nonce ���飬̫����ضϣ������� 0
     Iv: TBytes                           - 8 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

// ============== �����ֽ�����������ʮ�������ַ���֮��ļӽ��� =================

function AESEncryptEcbBytesToHex(Value: TBytes; Key: TBytes;
  KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES ECB ģʽ�����ֽ����鲢����ת����ʮ�����ơ�

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptEcbBytesFromHex(const HexStr: AnsiString; Key: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES ECB ����ʮ�������ַ����������ֽ����顣

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptCbcBytesToHex(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CBC ģʽ�����ֽ����鲢����ת����ʮ�����ơ�

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptCbcBytesFromHex(const HexStr: AnsiString; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CBC ����ʮ�������ַ����������ֽ����顣

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptCfbBytesToHex(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CFB ģʽ�����ֽ����鲢����ת����ʮ�����ơ�

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptCfbBytesFromHex(const HexStr: AnsiString; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CFB ����ʮ�������ַ����������ֽ����顣

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptOfbBytesToHex(Value: TBytes; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES OFB ģʽ�����ֽ����鲢����ת����ʮ�����ơ�

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptOfbBytesFromHex(const HexStr: AnsiString; Key: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES OFB ����ʮ�������ַ����������ֽ����顣

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Iv: TBytes                           - 16 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AESEncryptCtrBytesToHex(Value: TBytes; Key: TBytes; Nonce: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): AnsiString;
{* AES CTR ģʽ�����ֽ����鲢����ת����ʮ�����ơ�

   ������
     Value: TBytes                        - �����ܵ������ֽ�����
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Nonce: TBytes                        - 4 �ֽ� Nonce �ֽ����飬̫����ضϣ������� 0
     Iv: TBytes                           - 8 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��AnsiString                     - ��������ʮ�������ַ���
}

function AESDecryptCtrBytesFromHex(const HexStr: AnsiString; Key: TBytes; Nonce: TBytes; Iv: TBytes;
  KeyBit: TCnKeyBitType = kbt128): TBytes;
{* AES CTR ����ʮ�������ַ����������ֽ����顣

   ������
     const HexStr: AnsiString             - �����ܵ�ʮ�����������ַ���
     Key: TBytes                          - AES ��Կ�ֽ����飬���ȸ��ݼ�������ȷ��Ϊ 16��24��32 �ֽڣ�̫����ضϣ������� 0
     Nonce: TBytes                        - 4 �ֽ� Nonce �ֽ����飬̫����ضϣ������� 0
     Iv: TBytes                           - 8 �ֽڳ�ʼ�������ֽ����飬̫����ضϣ������� 0
     KeyBit: TCnKeyBitType                - AES ��������

   ����ֵ��TBytes                         - ���������ֽ�����
}

implementation

resourcestring
  SCnErrorAESInvalidInBufSize = 'Invalid Buffer Size for Decryption';
  SCnErrorAESReadError = 'Stream Read Error';
  SCnErrorAESWriteError = 'Stream Write Error';

function Min(A, B: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

const
  Rcon: array [1..30] of Cardinal = (
    $00000001, $00000002, $00000004, $00000008, $00000010, $00000020,
    $00000040, $00000080, $0000001B, $00000036, $0000006C, $000000D8,
    $000000AB, $0000004D, $0000009A, $0000002F, $0000005E, $000000BC,
    $00000063, $000000C6, $00000097, $00000035, $0000006A, $000000D4,
    $000000B3, $0000007D, $000000FA, $000000EF, $000000C5, $00000091
  );

  ForwardTable: array [0..255] of Cardinal = (
    $A56363C6, $847C7CF8, $997777EE, $8D7B7BF6, $0DF2F2FF, $BD6B6BD6, $B16F6FDE, $54C5C591,
    $50303060, $03010102, $A96767CE, $7D2B2B56, $19FEFEE7, $62D7D7B5, $E6ABAB4D, $9A7676EC,
    $45CACA8F, $9D82821F, $40C9C989, $877D7DFA, $15FAFAEF, $EB5959B2, $C947478E, $0BF0F0FB,
    $ECADAD41, $67D4D4B3, $FDA2A25F, $EAAFAF45, $BF9C9C23, $F7A4A453, $967272E4, $5BC0C09B,
    $C2B7B775, $1CFDFDE1, $AE93933D, $6A26264C, $5A36366C, $413F3F7E, $02F7F7F5, $4FCCCC83,
    $5C343468, $F4A5A551, $34E5E5D1, $08F1F1F9, $937171E2, $73D8D8AB, $53313162, $3F15152A,
    $0C040408, $52C7C795, $65232346, $5EC3C39D, $28181830, $A1969637, $0F05050A, $B59A9A2F,
    $0907070E, $36121224, $9B80801B, $3DE2E2DF, $26EBEBCD, $6927274E, $CDB2B27F, $9F7575EA,
    $1B090912, $9E83831D, $742C2C58, $2E1A1A34, $2D1B1B36, $B26E6EDC, $EE5A5AB4, $FBA0A05B,
    $F65252A4, $4D3B3B76, $61D6D6B7, $CEB3B37D, $7B292952, $3EE3E3DD, $712F2F5E, $97848413,
    $F55353A6, $68D1D1B9, $00000000, $2CEDEDC1, $60202040, $1FFCFCE3, $C8B1B179, $ED5B5BB6,
    $BE6A6AD4, $46CBCB8D, $D9BEBE67, $4B393972, $DE4A4A94, $D44C4C98, $E85858B0, $4ACFCF85,
    $6BD0D0BB, $2AEFEFC5, $E5AAAA4F, $16FBFBED, $C5434386, $D74D4D9A, $55333366, $94858511,
    $CF45458A, $10F9F9E9, $06020204, $817F7FFE, $F05050A0, $443C3C78, $BA9F9F25, $E3A8A84B,
    $F35151A2, $FEA3A35D, $C0404080, $8A8F8F05, $AD92923F, $BC9D9D21, $48383870, $04F5F5F1,
    $DFBCBC63, $C1B6B677, $75DADAAF, $63212142, $30101020, $1AFFFFE5, $0EF3F3FD, $6DD2D2BF,
    $4CCDCD81, $140C0C18, $35131326, $2FECECC3, $E15F5FBE, $A2979735, $CC444488, $3917172E,
    $57C4C493, $F2A7A755, $827E7EFC, $473D3D7A, $AC6464C8, $E75D5DBA, $2B191932, $957373E6,
    $A06060C0, $98818119, $D14F4F9E, $7FDCDCA3, $66222244, $7E2A2A54, $AB90903B, $8388880B,
    $CA46468C, $29EEEEC7, $D3B8B86B, $3C141428, $79DEDEA7, $E25E5EBC, $1D0B0B16, $76DBDBAD,
    $3BE0E0DB, $56323264, $4E3A3A74, $1E0A0A14, $DB494992, $0A06060C, $6C242448, $E45C5CB8,
    $5DC2C29F, $6ED3D3BD, $EFACAC43, $A66262C4, $A8919139, $A4959531, $37E4E4D3, $8B7979F2,
    $32E7E7D5, $43C8C88B, $5937376E, $B76D6DDA, $8C8D8D01, $64D5D5B1, $D24E4E9C, $E0A9A949,
    $B46C6CD8, $FA5656AC, $07F4F4F3, $25EAEACF, $AF6565CA, $8E7A7AF4, $E9AEAE47, $18080810,
    $D5BABA6F, $887878F0, $6F25254A, $722E2E5C, $241C1C38, $F1A6A657, $C7B4B473, $51C6C697,
    $23E8E8CB, $7CDDDDA1, $9C7474E8, $211F1F3E, $DD4B4B96, $DCBDBD61, $868B8B0D, $858A8A0F,
    $907070E0, $423E3E7C, $C4B5B571, $AA6666CC, $D8484890, $05030306, $01F6F6F7, $120E0E1C,
    $A36161C2, $5F35356A, $F95757AE, $D0B9B969, $91868617, $58C1C199, $271D1D3A, $B99E9E27,
    $38E1E1D9, $13F8F8EB, $B398982B, $33111122, $BB6969D2, $70D9D9A9, $898E8E07, $A7949433,
    $B69B9B2D, $221E1E3C, $92878715, $20E9E9C9, $49CECE87, $FF5555AA, $78282850, $7ADFDFA5,
    $8F8C8C03, $F8A1A159, $80898909, $170D0D1A, $DABFBF65, $31E6E6D7, $C6424284, $B86868D0,
    $C3414182, $B0999929, $772D2D5A, $110F0F1E, $CBB0B07B, $FC5454A8, $D6BBBB6D, $3A16162C
  );

  LastForwardTable: array [0..255] of Cardinal = (
    $00000063, $0000007C, $00000077, $0000007B, $000000F2, $0000006B, $0000006F, $000000C5,
    $00000030, $00000001, $00000067, $0000002B, $000000FE, $000000D7, $000000AB, $00000076,
    $000000CA, $00000082, $000000C9, $0000007D, $000000FA, $00000059, $00000047, $000000F0,
    $000000AD, $000000D4, $000000A2, $000000AF, $0000009C, $000000A4, $00000072, $000000C0,
    $000000B7, $000000FD, $00000093, $00000026, $00000036, $0000003F, $000000F7, $000000CC,
    $00000034, $000000A5, $000000E5, $000000F1, $00000071, $000000D8, $00000031, $00000015,
    $00000004, $000000C7, $00000023, $000000C3, $00000018, $00000096, $00000005, $0000009A,
    $00000007, $00000012, $00000080, $000000E2, $000000EB, $00000027, $000000B2, $00000075,
    $00000009, $00000083, $0000002C, $0000001A, $0000001B, $0000006E, $0000005A, $000000A0,
    $00000052, $0000003B, $000000D6, $000000B3, $00000029, $000000E3, $0000002F, $00000084,
    $00000053, $000000D1, $00000000, $000000ED, $00000020, $000000FC, $000000B1, $0000005B,
    $0000006A, $000000CB, $000000BE, $00000039, $0000004A, $0000004C, $00000058, $000000CF,
    $000000D0, $000000EF, $000000AA, $000000FB, $00000043, $0000004D, $00000033, $00000085,
    $00000045, $000000F9, $00000002, $0000007F, $00000050, $0000003C, $0000009F, $000000A8,
    $00000051, $000000A3, $00000040, $0000008F, $00000092, $0000009D, $00000038, $000000F5,
    $000000BC, $000000B6, $000000DA, $00000021, $00000010, $000000FF, $000000F3, $000000D2,
    $000000CD, $0000000C, $00000013, $000000EC, $0000005F, $00000097, $00000044, $00000017,
    $000000C4, $000000A7, $0000007E, $0000003D, $00000064, $0000005D, $00000019, $00000073,
    $00000060, $00000081, $0000004F, $000000DC, $00000022, $0000002A, $00000090, $00000088,
    $00000046, $000000EE, $000000B8, $00000014, $000000DE, $0000005E, $0000000B, $000000DB,
    $000000E0, $00000032, $0000003A, $0000000A, $00000049, $00000006, $00000024, $0000005C,
    $000000C2, $000000D3, $000000AC, $00000062, $00000091, $00000095, $000000E4, $00000079,
    $000000E7, $000000C8, $00000037, $0000006D, $0000008D, $000000D5, $0000004E, $000000A9,
    $0000006C, $00000056, $000000F4, $000000EA, $00000065, $0000007A, $000000AE, $00000008,
    $000000BA, $00000078, $00000025, $0000002E, $0000001C, $000000A6, $000000B4, $000000C6,
    $000000E8, $000000DD, $00000074, $0000001F, $0000004B, $000000BD, $0000008B, $0000008A,
    $00000070, $0000003E, $000000B5, $00000066, $00000048, $00000003, $000000F6, $0000000E,
    $00000061, $00000035, $00000057, $000000B9, $00000086, $000000C1, $0000001D, $0000009E,
    $000000E1, $000000F8, $00000098, $00000011, $00000069, $000000D9, $0000008E, $00000094,
    $0000009B, $0000001E, $00000087, $000000E9, $000000CE, $00000055, $00000028, $000000DF,
    $0000008C, $000000A1, $00000089, $0000000D, $000000BF, $000000E6, $00000042, $00000068,
    $00000041, $00000099, $0000002D, $0000000F, $000000B0, $00000054, $000000BB, $00000016
  );

  InverseTable: array [0..255] of Cardinal = (
    $50A7F451, $5365417E, $C3A4171A, $965E273A, $CB6BAB3B, $F1459D1F, $AB58FAAC, $9303E34B,
    $55FA3020, $F66D76AD, $9176CC88, $254C02F5, $FCD7E54F, $D7CB2AC5, $80443526, $8FA362B5,
    $495AB1DE, $671BBA25, $980EEA45, $E1C0FE5D, $02752FC3, $12F04C81, $A397468D, $C6F9D36B,
    $E75F8F03, $959C9215, $EB7A6DBF, $DA595295, $2D83BED4, $D3217458, $2969E049, $44C8C98E,
    $6A89C275, $78798EF4, $6B3E5899, $DD71B927, $B64FE1BE, $17AD88F0, $66AC20C9, $B43ACE7D,
    $184ADF63, $82311AE5, $60335197, $457F5362, $E07764B1, $84AE6BBB, $1CA081FE, $942B08F9,
    $58684870, $19FD458F, $876CDE94, $B7F87B52, $23D373AB, $E2024B72, $578F1FE3, $2AAB5566,
    $0728EBB2, $03C2B52F, $9A7BC586, $A50837D3, $F2872830, $B2A5BF23, $BA6A0302, $5C8216ED,
    $2B1CCF8A, $92B479A7, $F0F207F3, $A1E2694E, $CDF4DA65, $D5BE0506, $1F6234D1, $8AFEA6C4,
    $9D532E34, $A055F3A2, $32E18A05, $75EBF6A4, $39EC830B, $AAEF6040, $069F715E, $51106EBD,
    $F98A213E, $3D06DD96, $AE053EDD, $46BDE64D, $B58D5491, $055DC471, $6FD40604, $FF155060,
    $24FB9819, $97E9BDD6, $CC434089, $779ED967, $BD42E8B0, $888B8907, $385B19E7, $DBEEC879,
    $470A7CA1, $E90F427C, $C91E84F8, $00000000, $83868009, $48ED2B32, $AC70111E, $4E725A6C,
    $FBFF0EFD, $5638850F, $1ED5AE3D, $27392D36, $64D90F0A, $21A65C68, $D1545B9B, $3A2E3624,
    $B1670A0C, $0FE75793, $D296EEB4, $9E919B1B, $4FC5C080, $A220DC61, $694B775A, $161A121C,
    $0ABA93E2, $E52AA0C0, $43E0223C, $1D171B12, $0B0D090E, $ADC78BF2, $B9A8B62D, $C8A91E14,
    $8519F157, $4C0775AF, $BBDD99EE, $FD607FA3, $9F2601F7, $BCF5725C, $C53B6644, $347EFB5B,
    $7629438B, $DCC623CB, $68FCEDB6, $63F1E4B8, $CADC31D7, $10856342, $40229713, $2011C684,
    $7D244A85, $F83DBBD2, $1132F9AE, $6DA129C7, $4B2F9E1D, $F330B2DC, $EC52860D, $D0E3C177,
    $6C16B32B, $99B970A9, $FA489411, $2264E947, $C48CFCA8, $1A3FF0A0, $D82C7D56, $EF903322,
    $C74E4987, $C1D138D9, $FEA2CA8C, $360BD498, $CF81F5A6, $28DE7AA5, $268EB7DA, $A4BFAD3F,
    $E49D3A2C, $0D927850, $9BCC5F6A, $62467E54, $C2138DF6, $E8B8D890, $5EF7392E, $F5AFC382,
    $BE805D9F, $7C93D069, $A92DD56F, $B31225CF, $3B99ACC8, $A77D1810, $6E639CE8, $7BBB3BDB,
    $097826CD, $F418596E, $01B79AEC, $A89A4F83, $656E95E6, $7EE6FFAA, $08CFBC21, $E6E815EF,
    $D99BE7BA, $CE366F4A, $D4099FEA, $D67CB029, $AFB2A431, $31233F2A, $3094A5C6, $C066A235,
    $37BC4E74, $A6CA82FC, $B0D090E0, $15D8A733, $4A9804F1, $F7DAEC41, $0E50CD7F, $2FF69117,
    $8DD64D76, $4DB0EF43, $544DAACC, $DF0496E4, $E3B5D19E, $1B886A4C, $B81F2CC1, $7F516546,
    $04EA5E9D, $5D358C01, $737487FA, $2E410BFB, $5A1D67B3, $52D2DB92, $335610E9, $1347D66D,
    $8C61D79A, $7A0CA137, $8E14F859, $893C13EB, $EE27A9CE, $35C961B7, $EDE51CE1, $3CB1477A,
    $59DFD29C, $3F73F255, $79CE1418, $BF37C773, $EACDF753, $5BAAFD5F, $146F3DDF, $86DB4478,
    $81F3AFCA, $3EC468B9, $2C342438, $5F40A3C2, $72C31D16, $0C25E2BC, $8B493C28, $41950DFF,
    $7101A839, $DEB30C08, $9CE4B4D8, $90C15664, $6184CB7B, $70B632D5, $745C6C48, $4257B8D0
  );

  LastInverseTable: array [0..255] of Cardinal = (
    $00000052, $00000009, $0000006A, $000000D5, $00000030, $00000036, $000000A5, $00000038,
    $000000BF, $00000040, $000000A3, $0000009E, $00000081, $000000F3, $000000D7, $000000FB,
    $0000007C, $000000E3, $00000039, $00000082, $0000009B, $0000002F, $000000FF, $00000087,
    $00000034, $0000008E, $00000043, $00000044, $000000C4, $000000DE, $000000E9, $000000CB,
    $00000054, $0000007B, $00000094, $00000032, $000000A6, $000000C2, $00000023, $0000003D,
    $000000EE, $0000004C, $00000095, $0000000B, $00000042, $000000FA, $000000C3, $0000004E,
    $00000008, $0000002E, $000000A1, $00000066, $00000028, $000000D9, $00000024, $000000B2,
    $00000076, $0000005B, $000000A2, $00000049, $0000006D, $0000008B, $000000D1, $00000025,
    $00000072, $000000F8, $000000F6, $00000064, $00000086, $00000068, $00000098, $00000016,
    $000000D4, $000000A4, $0000005C, $000000CC, $0000005D, $00000065, $000000B6, $00000092,
    $0000006C, $00000070, $00000048, $00000050, $000000FD, $000000ED, $000000B9, $000000DA,
    $0000005E, $00000015, $00000046, $00000057, $000000A7, $0000008D, $0000009D, $00000084,
    $00000090, $000000D8, $000000AB, $00000000, $0000008C, $000000BC, $000000D3, $0000000A,
    $000000F7, $000000E4, $00000058, $00000005, $000000B8, $000000B3, $00000045, $00000006,
    $000000D0, $0000002C, $0000001E, $0000008F, $000000CA, $0000003F, $0000000F, $00000002,
    $000000C1, $000000AF, $000000BD, $00000003, $00000001, $00000013, $0000008A, $0000006B,
    $0000003A, $00000091, $00000011, $00000041, $0000004F, $00000067, $000000DC, $000000EA,
    $00000097, $000000F2, $000000CF, $000000CE, $000000F0, $000000B4, $000000E6, $00000073,
    $00000096, $000000AC, $00000074, $00000022, $000000E7, $000000AD, $00000035, $00000085,
    $000000E2, $000000F9, $00000037, $000000E8, $0000001C, $00000075, $000000DF, $0000006E,
    $00000047, $000000F1, $0000001A, $00000071, $0000001D, $00000029, $000000C5, $00000089,
    $0000006F, $000000B7, $00000062, $0000000E, $000000AA, $00000018, $000000BE, $0000001B,
    $000000FC, $00000056, $0000003E, $0000004B, $000000C6, $000000D2, $00000079, $00000020,
    $0000009A, $000000DB, $000000C0, $000000FE, $00000078, $000000CD, $0000005A, $000000F4,
    $0000001F, $000000DD, $000000A8, $00000033, $00000088, $00000007, $000000C7, $00000031,
    $000000B1, $00000012, $00000010, $00000059, $00000027, $00000080, $000000EC, $0000005F,
    $00000060, $00000051, $0000007F, $000000A9, $00000019, $000000B5, $0000004A, $0000000D,
    $0000002D, $000000E5, $0000007A, $0000009F, $00000093, $000000C9, $0000009C, $000000EF,
    $000000A0, $000000E0, $0000003B, $0000004D, $000000AE, $0000002A, $000000F5, $000000B0,
    $000000C8, $000000EB, $000000BB, $0000003C, $00000083, $00000053, $00000099, $00000061,
    $00000017, $0000002B, $00000004, $0000007E, $000000BA, $00000077, $000000D6, $00000026,
    $000000E1, $00000069, $00000014, $00000063, $00000055, $00000021, $0000000C, $0000007D
  );

procedure ExpandAESKeyForEncryption128(const Key: TCnAESKey128; var ExpandedKey:
  TCnAESExpandedKey128);
var
  I, J: Integer;
  T: Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  ExpandedKey[0] := PCardinal(@Key[0])^;
  ExpandedKey[1] := PCardinal(@Key[4])^;
  ExpandedKey[2] := PCardinal(@Key[8])^;
  ExpandedKey[3] := PCardinal(@Key[12])^;
  I := 0; J := 1;
  repeat
    T := (ExpandedKey[I + 3] shl 24) or (ExpandedKey[I + 3] shr 8);
    W0 := LastForwardTable[Byte(T)]; W1 := LastForwardTable[Byte(T shr 8)];
    W2 := LastForwardTable[Byte(T shr 16)]; W3 := LastForwardTable[Byte(T shr 24)];
    ExpandedKey[I + 4] := ExpandedKey[I] xor
      (W0 xor ((W1 shl 8) or (W1 shr 24)) xor
      ((W2 shl 16) or (W2 shr 16)) xor ((W3 shl 24) or (W3 shr 8))) xor Rcon[J];
    Inc(J);
    ExpandedKey[I + 5] := ExpandedKey[I + 1] xor ExpandedKey[I + 4];
    ExpandedKey[I + 6] := ExpandedKey[I + 2] xor ExpandedKey[I + 5];
    ExpandedKey[I + 7] := ExpandedKey[I + 3] xor ExpandedKey[I + 6];
    Inc(I, 4);
  until I >= 40;
end;

procedure ExpandAESKeyForEncryption192(const Key: TCnAESKey192; var ExpandedKey:
  TCnAESExpandedKey192);
var
  I, J: Integer;
  T: Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  ExpandedKey[0] := PCardinal(@Key[0])^;
  ExpandedKey[1] := PCardinal(@Key[4])^;
  ExpandedKey[2] := PCardinal(@Key[8])^;
  ExpandedKey[3] := PCardinal(@Key[12])^;
  ExpandedKey[4] := PCardinal(@Key[16])^;
  ExpandedKey[5] := PCardinal(@Key[20])^;
  I := 0; J := 1;
  repeat
    T := (ExpandedKey[I + 5] shl 24) or (ExpandedKey[I + 5] shr 8);
    W0 := LastForwardTable[Byte(T)]; W1 := LastForwardTable[Byte(T shr 8)];
    W2 := LastForwardTable[Byte(T shr 16)]; W3 := LastForwardTable[Byte(T shr 24)];
    ExpandedKey[I + 6] := ExpandedKey[I] xor
      (W0 xor ((W1 shl 8) or (W1 shr 24)) xor
      ((W2 shl 16) or (W2 shr 16)) xor ((W3 shl 24) or (W3 shr 8))) xor Rcon[J];
    Inc(J);
    ExpandedKey[I + 7] := ExpandedKey[I + 1] xor ExpandedKey[I + 6];
    ExpandedKey[I + 8] := ExpandedKey[I + 2] xor ExpandedKey[I + 7];
    ExpandedKey[I + 9] := ExpandedKey[I + 3] xor ExpandedKey[I + 8];
    ExpandedKey[I + 10] := ExpandedKey[I + 4] xor ExpandedKey[I + 9];
    ExpandedKey[I + 11] := ExpandedKey[I + 5] xor ExpandedKey[I + 10];
    Inc(I, 6);
  until I >= 46;
end;

procedure ExpandAESKeyForEncryption256(const Key: TCnAESKey256; var ExpandedKey:
  TCnAESExpandedKey256);
var
  I, J: Integer;
  T: Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  ExpandedKey[0] := PCardinal(@Key[0])^;
  ExpandedKey[1] := PCardinal(@Key[4])^;
  ExpandedKey[2] := PCardinal(@Key[8])^;
  ExpandedKey[3] := PCardinal(@Key[12])^;
  ExpandedKey[4] := PCardinal(@Key[16])^;
  ExpandedKey[5] := PCardinal(@Key[20])^;
  ExpandedKey[6] := PCardinal(@Key[24])^;
  ExpandedKey[7] := PCardinal(@Key[28])^;
  I := 0; J := 1;
  repeat
    T := (ExpandedKey[I + 7] shl 24) or (ExpandedKey[I + 7] shr 8);
    W0 := LastForwardTable[Byte(T)]; W1 := LastForwardTable[Byte(T shr 8)];
    W2 := LastForwardTable[Byte(T shr 16)]; W3 := LastForwardTable[Byte(T shr 24)];
    ExpandedKey[I + 8] := ExpandedKey[I] xor
      (W0 xor ((W1 shl 8) or (W1 shr 24)) xor
      ((W2 shl 16) or (W2 shr 16)) xor ((W3 shl 24) or (W3 shr 8))) xor Rcon[J];
    Inc(J);
    ExpandedKey[I + 9] := ExpandedKey[I + 1] xor ExpandedKey[I + 8];
    ExpandedKey[I + 10] := ExpandedKey[I + 2] xor ExpandedKey[I + 9];
    ExpandedKey[I + 11] := ExpandedKey[I + 3] xor ExpandedKey[I + 10];
    W0 := LastForwardTable[Byte(ExpandedKey[I + 11])];
    W1 := LastForwardTable[Byte(ExpandedKey[I + 11] shr 8)];
    W2 := LastForwardTable[Byte(ExpandedKey[I + 11] shr 16)];
    W3 := LastForwardTable[Byte(ExpandedKey[I + 11] shr 24)];
    ExpandedKey[I + 12] := ExpandedKey[I + 4] xor
      (W0 xor ((W1 shl 8) or (W1 shr 24)) xor
      ((W2 shl 16) or (W2 shr 16)) xor ((W3 shl 24) or (W3 shr 8)));
    ExpandedKey[I + 13] := ExpandedKey[I + 5] xor ExpandedKey[I + 12];
    ExpandedKey[I + 14] := ExpandedKey[I + 6] xor ExpandedKey[I + 13];
    ExpandedKey[I + 15] := ExpandedKey[I + 7] xor ExpandedKey[I + 14];
    Inc(I, 8);
  until I >= 52;
end;

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬���������� Delphi �¿���
procedure EncryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer);
begin
  EncryptAES128(InBuf, Key, OutBuf);
end;

procedure EncryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer);
begin
  EncryptAES192(InBuf, Key, OutBuf);
end;

procedure EncryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer);
begin
  EncryptAES256(InBuf, Key, OutBuf);
end;

{$ENDIF}

procedure EncryptAES128(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer);
var
  T0, T1: array [0..3] of Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  // initializing
  T0[0] := PCardinal(@InBuf[0])^ xor Key[0];
  T0[1] := PCardinal(@InBuf[4])^ xor Key[1];
  T0[2] := PCardinal(@InBuf[8])^ xor Key[2];
  T0[3] := PCardinal(@InBuf[12])^ xor Key[3];

  // performing transformation 9 times
  // round 1
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
  // round 2
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
  // round 3
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
  // round 4
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
  // round 5
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
  // round 6
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
  // round 7
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
  // round 8
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
  // round 9
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
  // last round of transformations
  W0 := LastForwardTable[Byte(T1[0])]; W1 := LastForwardTable[Byte(T1[1] shr 8)];
  W2 := LastForwardTable[Byte(T1[2] shr 16)]; W3 := LastForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
  W0 := LastForwardTable[Byte(T1[1])]; W1 := LastForwardTable[Byte(T1[2] shr 8)];
  W2 := LastForwardTable[Byte(T1[3] shr 16)]; W3 := LastForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
  W0 := LastForwardTable[Byte(T1[2])]; W1 := LastForwardTable[Byte(T1[3] shr 8)];
  W2 := LastForwardTable[Byte(T1[0] shr 16)]; W3 := LastForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
  W0 := LastForwardTable[Byte(T1[3])]; W1 := LastForwardTable[Byte(T1[0] shr 8)];
  W2 := LastForwardTable[Byte(T1[1] shr 16)]; W3 := LastForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];

  // finalizing
  PCardinal(@OutBuf[0])^ := T0[0];
  PCardinal(@OutBuf[4])^ := T0[1];
  PCardinal(@OutBuf[8])^ := T0[2];
  PCardinal(@OutBuf[12])^ := T0[3];
end;

procedure EncryptAES192(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer);
var
  T0, T1: array [0..3] of Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  // initializing
  T0[0] := PCardinal(@InBuf[0])^ xor Key[0];
  T0[1] := PCardinal(@InBuf[4])^ xor Key[1];
  T0[2] := PCardinal(@InBuf[8])^ xor Key[2];
  T0[3] := PCardinal(@InBuf[12])^ xor Key[3];

  // performing transformation 11 times
  // round 1
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
  // round 2
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
  // round 3
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
  // round 4
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
  // round 5
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
  // round 6
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
  // round 7
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
  // round 8
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
  // round 9
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
  // round 10
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
  // round 11
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
  // last round of transformations
  W0 := LastForwardTable[Byte(T1[0])]; W1 := LastForwardTable[Byte(T1[1] shr 8)];
  W2 := LastForwardTable[Byte(T1[2] shr 16)]; W3 := LastForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[48];
  W0 := LastForwardTable[Byte(T1[1])]; W1 := LastForwardTable[Byte(T1[2] shr 8)];
  W2 := LastForwardTable[Byte(T1[3] shr 16)]; W3 := LastForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[49];
  W0 := LastForwardTable[Byte(T1[2])]; W1 := LastForwardTable[Byte(T1[3] shr 8)];
  W2 := LastForwardTable[Byte(T1[0] shr 16)]; W3 := LastForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[50];
  W0 := LastForwardTable[Byte(T1[3])]; W1 := LastForwardTable[Byte(T1[0] shr 8)];
  W2 := LastForwardTable[Byte(T1[1] shr 16)]; W3 := LastForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[51];

  // finalizing
  PCardinal(@OutBuf[0])^ := T0[0];
  PCardinal(@OutBuf[4])^ := T0[1];
  PCardinal(@OutBuf[8])^ := T0[2];
  PCardinal(@OutBuf[12])^ := T0[3];
end;

procedure EncryptAES256(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer);
var
  T0, T1: array [0..3] of Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  // initializing
  T0[0] := PCardinal(@InBuf[0])^ xor Key[0];
  T0[1] := PCardinal(@InBuf[4])^ xor Key[1];
  T0[2] := PCardinal(@InBuf[8])^ xor Key[2];
  T0[3] := PCardinal(@InBuf[12])^ xor Key[3];

  // performing transformation 13 times
  // round 1
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
  // round 2
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
  // round 3
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
  // round 4
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
  // round 5
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
  // round 6
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
  // round 7
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
  // round 8
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
  // round 9
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
  // round 10
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
  // round 11
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
  // round 12
  W0 := ForwardTable[Byte(T1[0])]; W1 := ForwardTable[Byte(T1[1] shr 8)];
  W2 := ForwardTable[Byte(T1[2] shr 16)]; W3 := ForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[48];
  W0 := ForwardTable[Byte(T1[1])]; W1 := ForwardTable[Byte(T1[2] shr 8)];
  W2 := ForwardTable[Byte(T1[3] shr 16)]; W3 := ForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[49];
  W0 := ForwardTable[Byte(T1[2])]; W1 := ForwardTable[Byte(T1[3] shr 8)];
  W2 := ForwardTable[Byte(T1[0] shr 16)]; W3 := ForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[50];
  W0 := ForwardTable[Byte(T1[3])]; W1 := ForwardTable[Byte(T1[0] shr 8)];
  W2 := ForwardTable[Byte(T1[1] shr 16)]; W3 := ForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[51];
  // round 13
  W0 := ForwardTable[Byte(T0[0])]; W1 := ForwardTable[Byte(T0[1] shr 8)];
  W2 := ForwardTable[Byte(T0[2] shr 16)]; W3 := ForwardTable[Byte(T0[3] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[52];
  W0 := ForwardTable[Byte(T0[1])]; W1 := ForwardTable[Byte(T0[2] shr 8)];
  W2 := ForwardTable[Byte(T0[3] shr 16)]; W3 := ForwardTable[Byte(T0[0] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[53];
  W0 := ForwardTable[Byte(T0[2])]; W1 := ForwardTable[Byte(T0[3] shr 8)];
  W2 := ForwardTable[Byte(T0[0] shr 16)]; W3 := ForwardTable[Byte(T0[1] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[54];
  W0 := ForwardTable[Byte(T0[3])]; W1 := ForwardTable[Byte(T0[0] shr 8)];
  W2 := ForwardTable[Byte(T0[1] shr 16)]; W3 := ForwardTable[Byte(T0[2] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[55];
  // last round of transformations
  W0 := LastForwardTable[Byte(T1[0])]; W1 := LastForwardTable[Byte(T1[1] shr 8)];
  W2 := LastForwardTable[Byte(T1[2] shr 16)]; W3 := LastForwardTable[Byte(T1[3] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[56];
  W0 := LastForwardTable[Byte(T1[1])]; W1 := LastForwardTable[Byte(T1[2] shr 8)];
  W2 := LastForwardTable[Byte(T1[3] shr 16)]; W3 := LastForwardTable[Byte(T1[0] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[57];
  W0 := LastForwardTable[Byte(T1[2])]; W1 := LastForwardTable[Byte(T1[3] shr 8)];
  W2 := LastForwardTable[Byte(T1[0] shr 16)]; W3 := LastForwardTable[Byte(T1[1] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[58];
  W0 := LastForwardTable[Byte(T1[3])]; W1 := LastForwardTable[Byte(T1[0] shr 8)];
  W2 := LastForwardTable[Byte(T1[1] shr 16)]; W3 := LastForwardTable[Byte(T1[2] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[59];

  // finalizing
  PCardinal(@OutBuf[0])^ := T0[0];
  PCardinal(@OutBuf[4])^ := T0[1];
  PCardinal(@OutBuf[8])^ := T0[2];
  PCardinal(@OutBuf[12])^ := T0[3];
end;

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure ExpandAESKeyForDecryption(var ExpandedKey: TCnAESExpandedKey128);
begin
  ExpandAESKeyForDecryption128(ExpandedKey);
end;

procedure ExpandAESKeyForDecryption(const Key: TCnAESKey128;
  var ExpandedKey: TCnAESExpandedKey128);
begin
  ExpandAESKeyForDecryption128Expanded(Key, ExpandedKey);
end;

procedure ExpandAESKeyForDecryption(var ExpandedKey: TCnAESExpandedKey192);
begin
  ExpandAESKeyForDecryption192(ExpandedKey);
end;

procedure ExpandAESKeyForDecryption(const Key: TCnAESKey192;
  var ExpandedKey: TCnAESExpandedKey192);
begin
  ExpandAESKeyForDecryption192Expanded(Key, ExpandedKey);
end;

procedure ExpandAESKeyForDecryption(var ExpandedKey: TCnAESExpandedKey256);
begin
  ExpandAESKeyForDecryption256(ExpandedKey);
end;

procedure ExpandAESKeyForDecryption(const Key: TCnAESKey256;
  var ExpandedKey: TCnAESExpandedKey256);
begin
  ExpandAESKeyForDecryption256Expanded(Key, ExpandedKey);
end;

{$ENDIF}

procedure ExpandAESKeyForDecryption128(var ExpandedKey: TCnAESExpandedKey128);
var
  I: Integer;
  U, F2, F4, F8, F9: Cardinal;
begin
  for I := 1 to 9 do
  begin
    F9 := ExpandedKey[I * 4];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 1];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 1] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 2];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 2] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 3];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 3] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
  end;
end;

procedure ExpandAESKeyForDecryption128Expanded(const Key: TCnAESKey128; var ExpandedKey:
  TCnAESExpandedKey128);
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  ExpandAESKeyForDecryption128(ExpandedKey);
end;

procedure ExpandAESKeyForDecryption192(var ExpandedKey: TCnAESExpandedKey192);
var
  I: Integer;
  U, F2, F4, F8, F9: Cardinal;
begin
  for I := 1 to 11 do
  begin
    F9 := ExpandedKey[I * 4];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 1];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 1] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 2];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 2] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 3];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 3] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
  end;
end;

procedure ExpandAESKeyForDecryption192Expanded(const Key: TCnAESKey192; var ExpandedKey:
  TCnAESExpandedKey192);
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  ExpandAESKeyForDecryption192(ExpandedKey);
end;

procedure ExpandAESKeyForDecryption256(var ExpandedKey: TCnAESExpandedKey256);
var
  I: Integer;
  U, F2, F4, F8, F9: Cardinal;
begin
  for I := 1 to 13 do
  begin
    F9 := ExpandedKey[I * 4];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 1];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 1] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 2];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 2] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
    F9 := ExpandedKey[I * 4 + 3];
    U := F9 and $80808080;
    F2 := ((F9 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F2 and $80808080;
    F4 := ((F2 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    U := F4 and $80808080;
    F8 := ((F4 and $7F7F7F7F) shl 1) xor ((U - (U shr 7)) and $1B1B1B1B);
    F9 := F9 xor F8;
    ExpandedKey[I * 4 + 3] := F2 xor F4 xor F8 xor
      (((F2 xor F9) shl 24) or ((F2 xor F9) shr 8)) xor
      (((F4 xor F9) shl 16) or ((F4 xor F9) shr 16)) xor ((F9 shl 8) or (F9 shr 24));
  end;
end;

procedure ExpandAESKeyForDecryption256Expanded(const Key: TCnAESKey256; var ExpandedKey:
  TCnAESExpandedKey256);
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  ExpandAESKeyForDecryption256(ExpandedKey);
end;

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬���������� Delphi �¿���
procedure DecryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer);
begin
  DecryptAES128(InBuf, Key, OutBuf);
end;

procedure DecryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer);
begin
  DecryptAES192(InBuf, Key, OutBuf);
end;

procedure DecryptAES(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer);
begin
  DecryptAES256(InBuf, Key, OutBuf);
end;

{$ENDIF}

procedure DecryptAES128(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey128;
  var OutBuf: TCnAESBuffer);
var
  T0, T1: array [0..3] of Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  // initializing
  T0[0] := PCardinal(@InBuf[0])^ xor Key[40];
  T0[1] := PCardinal(@InBuf[4])^ xor Key[41];
  T0[2] := PCardinal(@InBuf[8])^ xor Key[42];
  T0[3] := PCardinal(@InBuf[12])^ xor Key[43];

  // performing transformations 9 times
  // round 1
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
  // round 2
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
  // round 3
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
  // round 4
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
  // round 5
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
  // round 6
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
  // round 7
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
  // round 8
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
  // round 9
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
  // last round of transformations
  W0 := LastInverseTable[Byte(T1[0])]; W1 := LastInverseTable[Byte(T1[3] shr 8)];
  W2 := LastInverseTable[Byte(T1[2] shr 16)]; W3 := LastInverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[0];
  W0 := LastInverseTable[Byte(T1[1])]; W1 := LastInverseTable[Byte(T1[0] shr 8)];
  W2 := LastInverseTable[Byte(T1[3] shr 16)]; W3 := LastInverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[1];
  W0 := LastInverseTable[Byte(T1[2])]; W1 := LastInverseTable[Byte(T1[1] shr 8)];
  W2 := LastInverseTable[Byte(T1[0] shr 16)]; W3 := LastInverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[2];
  W0 := LastInverseTable[Byte(T1[3])]; W1 := LastInverseTable[Byte(T1[2] shr 8)];
  W2 := LastInverseTable[Byte(T1[1] shr 16)]; W3 := LastInverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[3];

  // finalizing
  PCardinal(@OutBuf[0])^ := T0[0];
  PCardinal(@OutBuf[4])^ := T0[1];
  PCardinal(@OutBuf[8])^ := T0[2];
  PCardinal(@OutBuf[12])^ := T0[3];
end;

procedure DecryptAES192(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey192;
  var OutBuf: TCnAESBuffer);
var
  T0, T1: array [0..3] of Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  // initializing
  T0[0] := PCardinal(@InBuf[0])^ xor Key[48];
  T0[1] := PCardinal(@InBuf[4])^ xor Key[49];
  T0[2] := PCardinal(@InBuf[8])^ xor Key[50];
  T0[3] := PCardinal(@InBuf[12])^ xor Key[51];

  // performing transformations 11 times
  // round 1
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
  // round 2
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
  // round 3
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
  // round 4
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
  // round 5
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
  // round 6
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
  // round 7
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
  // round 8
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
  // round 9
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
  // round 10
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
  // round 11
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
  // last round of transformations
  W0 := LastInverseTable[Byte(T1[0])]; W1 := LastInverseTable[Byte(T1[3] shr 8)];
  W2 := LastInverseTable[Byte(T1[2] shr 16)]; W3 := LastInverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[0];
  W0 := LastInverseTable[Byte(T1[1])]; W1 := LastInverseTable[Byte(T1[0] shr 8)];
  W2 := LastInverseTable[Byte(T1[3] shr 16)]; W3 := LastInverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[1];
  W0 := LastInverseTable[Byte(T1[2])]; W1 := LastInverseTable[Byte(T1[1] shr 8)];
  W2 := LastInverseTable[Byte(T1[0] shr 16)]; W3 := LastInverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[2];
  W0 := LastInverseTable[Byte(T1[3])]; W1 := LastInverseTable[Byte(T1[2] shr 8)];
  W2 := LastInverseTable[Byte(T1[1] shr 16)]; W3 := LastInverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[3];

  // finalizing
  PCardinal(@OutBuf[0])^ := T0[0];
  PCardinal(@OutBuf[4])^ := T0[1];
  PCardinal(@OutBuf[8])^ := T0[2];
  PCardinal(@OutBuf[12])^ := T0[3];
end;

procedure DecryptAES256(const InBuf: TCnAESBuffer; const Key: TCnAESExpandedKey256;
  var OutBuf: TCnAESBuffer);
var
  T0, T1: array [0..3] of Cardinal;
  W0, W1, W2, W3: Cardinal;
begin
  // initializing
  T0[0] := PCardinal(@InBuf[0])^ xor Key[56];
  T0[1] := PCardinal(@InBuf[4])^ xor Key[57];
  T0[2] := PCardinal(@InBuf[8])^ xor Key[58];
  T0[3] := PCardinal(@InBuf[12])^ xor Key[59];

  // performing transformations 13 times
  // round 1
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[52];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[53];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[54];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[55];
  // round 2
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[48];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[49];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[50];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[51];
  // round 3
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[44];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[45];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[46];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[47];
  // round 4
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[40];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[41];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[42];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[43];
  // round 5
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[36];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[37];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[38];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[39];
  // round 6
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[32];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[33];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[34];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[35];
  // round 7
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[28];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[29];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[30];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[31];
  // round 8
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[24];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[25];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[26];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[27];
  // round 9
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[20];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[21];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[22];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[23];
  // round 10
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[16];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[17];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[18];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[19];
  // round 11
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[12];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[13];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[14];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[15];
  // round 12
  W0 := InverseTable[Byte(T1[0])]; W1 := InverseTable[Byte(T1[3] shr 8)];
  W2 := InverseTable[Byte(T1[2] shr 16)]; W3 := InverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[8];
  W0 := InverseTable[Byte(T1[1])]; W1 := InverseTable[Byte(T1[0] shr 8)];
  W2 := InverseTable[Byte(T1[3] shr 16)]; W3 := InverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[9];
  W0 := InverseTable[Byte(T1[2])]; W1 := InverseTable[Byte(T1[1] shr 8)];
  W2 := InverseTable[Byte(T1[0] shr 16)]; W3 := InverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[10];
  W0 := InverseTable[Byte(T1[3])]; W1 := InverseTable[Byte(T1[2] shr 8)];
  W2 := InverseTable[Byte(T1[1] shr 16)]; W3 := InverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[11];
  // round 13
  W0 := InverseTable[Byte(T0[0])]; W1 := InverseTable[Byte(T0[3] shr 8)];
  W2 := InverseTable[Byte(T0[2] shr 16)]; W3 := InverseTable[Byte(T0[1] shr 24)];
  T1[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[4];
  W0 := InverseTable[Byte(T0[1])]; W1 := InverseTable[Byte(T0[0] shr 8)];
  W2 := InverseTable[Byte(T0[3] shr 16)]; W3 := InverseTable[Byte(T0[2] shr 24)];
  T1[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[5];
  W0 := InverseTable[Byte(T0[2])]; W1 := InverseTable[Byte(T0[1] shr 8)];
  W2 := InverseTable[Byte(T0[0] shr 16)]; W3 := InverseTable[Byte(T0[3] shr 24)];
  T1[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[6];
  W0 := InverseTable[Byte(T0[3])]; W1 := InverseTable[Byte(T0[2] shr 8)];
  W2 := InverseTable[Byte(T0[1] shr 16)]; W3 := InverseTable[Byte(T0[0] shr 24)];
  T1[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[7];
  // last round of transformations
  W0 := LastInverseTable[Byte(T1[0])]; W1 := LastInverseTable[Byte(T1[3] shr 8)];
  W2 := LastInverseTable[Byte(T1[2] shr 16)]; W3 := LastInverseTable[Byte(T1[1] shr 24)];
  T0[0] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[0];
  W0 := LastInverseTable[Byte(T1[1])]; W1 := LastInverseTable[Byte(T1[0] shr 8)];
  W2 := LastInverseTable[Byte(T1[3] shr 16)]; W3 := LastInverseTable[Byte(T1[2] shr 24)];
  T0[1] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[1];
  W0 := LastInverseTable[Byte(T1[2])]; W1 := LastInverseTable[Byte(T1[1] shr 8)];
  W2 := LastInverseTable[Byte(T1[0] shr 16)]; W3 := LastInverseTable[Byte(T1[3] shr 24)];
  T0[2] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[2];
  W0 := LastInverseTable[Byte(T1[3])]; W1 := LastInverseTable[Byte(T1[2] shr 8)];
  W2 := LastInverseTable[Byte(T1[1] shr 16)]; W3 := LastInverseTable[Byte(T1[0] shr 24)];
  T0[3] := (W0 xor ((W1 shl 8) or (W1 shr 24)) xor ((W2 shl 16) or (W2 shr 16))
    xor ((W3 shl 24) or (W3 shr 8))) xor Key[3];

  // finalizing
  PCardinal(@OutBuf[0])^ := T0[0];
  PCardinal(@OutBuf[4])^ := T0[1];
  PCardinal(@OutBuf[8])^ := T0[2];
  PCardinal(@OutBuf[12])^ := T0[3];
end;

// Stream Encryption Routines (ECB mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream);
begin
  EncryptAES128StreamECB(Source, Count, Key, Dest);
end;

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream);
begin
  EncryptAES128StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream);
begin
  EncryptAES192StreamECB(Source, Count, Key, Dest);
end;

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream);
begin
  EncryptAES192StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream);
begin
  EncryptAES256StreamECB(Source, Count, Key, Dest);
end;

procedure EncryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream);
begin
  EncryptAES256StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

{$ENDIF}

procedure EncryptAES128StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  EncryptAES128StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure EncryptAES192StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  EncryptAES192StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure EncryptAES256StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  EncryptAES256StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure EncryptAES128StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES128(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    EncryptAES128(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES192StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES192(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    EncryptAES192(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES256StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES256(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    EncryptAES256(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

// Stream Decryption Routines (ECB mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream);
begin
  DecryptAES128StreamECB(Source, Count, Key, Dest);
end;

procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream);
begin
  DecryptAES128StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream);
begin
  DecryptAES192StreamECB(Source, Count, Key, Dest);
end;

procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream);
begin
  DecryptAES192StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream);
begin
  DecryptAES256StreamECB(Source, Count, Key, Dest);
end;

procedure DecryptAESStreamECB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream);
begin
  DecryptAES256StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

{$ENDIF}

procedure DecryptAES128StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForDecryption128Expanded(Key, ExpandedKey);
  DecryptAES128StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure DecryptAES128StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  if (Count mod SizeOf(TCnAESBuffer)) > 0 then
    raise ECnAESException.Create(SCnErrorAESInvalidInBufSize);
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    DecryptAES128(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
end;

procedure DecryptAES192StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForDecryption192Expanded(Key, ExpandedKey);
  DecryptAES192StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure DecryptAES192StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  if (Count mod SizeOf(TCnAESBuffer)) > 0 then
    raise ECnAESException.Create(SCnErrorAESInvalidInBufSize);
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    DecryptAES192(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
end;

procedure DecryptAES256StreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForDecryption256Expanded(Key, ExpandedKey);
  DecryptAES256StreamECBExpanded(Source, Count, ExpandedKey, Dest);
end;

procedure DecryptAES256StreamECBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  if (Count mod SizeOf(TCnAESBuffer)) > 0 then
    raise ECnAESException.Create(SCnErrorAESInvalidInBufSize);
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    DecryptAES256(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
end;

// Stream Encryption Routines (CBC mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES128StreamCBC(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES128StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES192StreamCBC(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES192StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES256StreamCBC(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES256StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

{$ENDIF}

procedure EncryptAES128StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  EncryptAES128StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES128StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError); // Ҫ��ÿһ�鶼������
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^; // ԭʼ�������� IV �����
    EncryptAES128(TempIn, ExpandedKey, TempOut);                                    // ������ټ���
    Done := Dest.Write(TempOut, SizeOf(TempOut));                                   // ��������д����
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;                                                              // �������ݴ���ԭʼ IV ����һ�����ʹ��
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^;
    EncryptAES128(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES192StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  EncryptAES192StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES192StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192;  const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^;
    EncryptAES192(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^;
    EncryptAES192(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES256StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  EncryptAES256StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES256StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^;
    EncryptAES256(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^;
    EncryptAES256(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

// Stream Decryption Routines (CBC mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES128StreamCBC(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES128StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES192StreamCBC(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES192StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES256StreamCBC(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamCBC(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES256StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

{$ENDIF}

procedure DecryptAES128StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForDecryption128Expanded(Key, ExpandedKey);
  DecryptAES128StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES128StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector1, Vector2: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  if (Count mod SizeOf(TCnAESBuffer)) > 0 then
    raise ECnAESException.Create(SCnErrorAESInvalidInBufSize);
  Vector1 := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    Vector2 := TempIn;
    DecryptAES128(TempIn, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@Vector1[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@Vector1[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@Vector1[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@Vector1[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector1 := Vector2;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
end;

procedure DecryptAES192StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForDecryption192Expanded(Key, ExpandedKey);
  DecryptAES192StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES192StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector1, Vector2: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  if (Count mod SizeOf(TCnAESBuffer)) > 0 then
    raise ECnAESException.Create(SCnErrorAESInvalidInBufSize);
  Vector1 := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    Vector2 := TempIn;
    DecryptAES192(TempIn, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@Vector1[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@Vector1[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@Vector1[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@Vector1[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector1 := Vector2;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
end;

procedure DecryptAES256StreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForDecryption256Expanded(Key, ExpandedKey);
  DecryptAES256StreamCBCExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES256StreamCBCExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector1, Vector2: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  if (Count mod SizeOf(TCnAESBuffer)) > 0 then
    raise ECnAESException.Create(SCnErrorAESInvalidInBufSize);        // CBC ������������������Ϊ AES �ֿ���ܲ����ģ�����������������Ա���������
  Vector1 := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    Vector2 := TempIn;
    DecryptAES256(TempIn, ExpandedKey, TempOut);         // ���������Ƚ���
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@Vector1[0])^;   // ���ܺ�����ݺ� Iv ���õ�����
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@Vector1[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@Vector1[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@Vector1[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));      // ����д��ȥ
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector1 := Vector2;                                // ��������ȡ�� Iv ��Ϊ��һ�κͽ���������������
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
end;

// Stream Encryption Routines (CFB mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES128StreamCFB(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES128StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES192StreamCFB(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES192StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES256StreamCFB(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES256StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

{$ENDIF}

procedure EncryptAES128StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  EncryptAES128StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES128StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);                                       // Key �ȼ��� Iv
    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // ���ܽ�����������
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));                                   // ���Ľ��д�����Ľ��
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;                                                              // ���Ľ��ȡ�� Iv ����һ�ּ���
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempOut, Count);  // ���д���ֻ�������ĳ��ȵĲ��֣�����������
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES192StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  EncryptAES192StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES192StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempOut, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES256StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  EncryptAES256StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES256StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempOut, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

// Stream Decryption Routines (CFB mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES128StreamCFB(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES128StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES192StreamCFB(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES192StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES256StreamCFB(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamCFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES256StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

{$ENDIF}

procedure DecryptAES128StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  DecryptAES128StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES128StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  // CFB ��������������������Ϊ AES �ֿ���ܲ����Ķ�����򣨳����Ŀɶ������������������
  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));       // ��������
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);         // Iv �ȼ��ܡ���ע���Ǽ��ܣ����ǽ��ܣ�
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;   // ���ܺ�����ݺ��������õ�����
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));      // ����д��ȥ
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector := TempIn;                                 // ��������ȡ�� Iv ��Ϊ��һ�μ�������������
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then                                   // ���һ�鲻Ϊ��
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;   // ���ܺ�����ݺ��������õ�����
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempOut, Count);      // ����д��ȥ
    if Done < Count then
      raise EStreamError(SCnErrorAESWriteError);
  end;
end;

procedure DecryptAES192StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  DecryptAES192StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES192StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector := TempIn;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempOut, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESWriteError);
  end;
end;

procedure DecryptAES256StreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  DecryptAES256StreamCFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES256StreamCFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector := TempIn;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempOut, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESWriteError);
  end;
end;

// Stream Encryption Routines (OFB mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES128StreamOFB(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES128StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES192StreamOFB(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES192StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  EncryptAES256StreamOFB(Source, Count, Key, InitVector, Dest);
end;

procedure EncryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  EncryptAES256StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

{$ENDIF}

procedure EncryptAES128StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  EncryptAES128StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES128StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);                                      // Key �ȼ��� Iv
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // ���ܽ�����������
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));                                    // ���Ľ��д�����Ľ��
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;                                                             // ���ܽ��ȡ�� Iv ����һ�ּ��ܣ�ע�ⲻ�������
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, Count);  // ���д���ֻ�������ĳ��ȵĲ��֣�����������
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES192StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  EncryptAES192StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES192StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES256StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  EncryptAES256StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptAES256StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

// Stream Decryption Routines (OFB mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES128StreamOFB(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES128StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES192StreamOFB(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES192StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
begin
  DecryptAES256StreamOFB(Source, Count, Key, InitVector, Dest);
end;

procedure DecryptAESStreamOFB(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
begin
  DecryptAES256StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

{$ENDIF}

procedure DecryptAES128StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  DecryptAES128StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES128StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  // OFB ��������������������Ϊ AES �ֿ���ܲ����Ķ�����򣨳����Ŀɶ������������������
  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));       // ��������
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);       // Iv �ȼ��ܡ���ע���Ǽ��ܣ����ǽ��ܣ�
    PCardinal(@TempIn[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;   // ���ܺ�����ݺ��������õ�����
    PCardinal(@TempIn[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));       // ����д��ȥ
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector := TempOut;                               // ������������ȡ�� Iv ��Ϊ��һ�����ǰ������
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then                                   // ���һ�鲻Ϊ��
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES128(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;   // ���ܺ�����ݺ��������õ�����
    PCardinal(@TempIn[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempIn, Count);      // ����д��ȥ
    if Done < Count then
      raise EStreamError(SCnErrorAESWriteError);
  end;
end;

procedure DecryptAES192StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  DecryptAES192StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES192StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESWriteError);
  end;
end;

procedure DecryptAES256StreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const InitVector: TCnAESBuffer; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  DecryptAES256StreamOFBExpanded(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptAES256StreamOFBExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const InitVector: TCnAESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TCnAESBuffer;
  Vector: TCnAESBuffer;
  Done: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Vector := InitVector;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorAESWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TCnAESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESReadError);
    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;
    Done := Dest.Write(TempIn, Count);
    if Done < Count then
      raise EStreamError(SCnErrorAESWriteError);
  end;
end;

// Stream Encryption Routines (CTR mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES128StreamCTR(Source, Count, Key, Nonce, InitVector, Dest);
end;

procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES128StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES192StreamCTR(Source, Count, Key, Nonce, InitVector, Dest);
end;

procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES192StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES256StreamCTR(Source, Count, Key, Nonce, InitVector, Dest);
end;

procedure EncryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES256StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

{$ENDIF}

procedure EncryptAES128StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  EncryptAES128StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure EncryptAES128StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done, Cnt, T: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Cnt := 1;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);

    Move(Nonce[0], Vector[0], SizeOf(TCnAESCTRNonce));
    Move(InitVector[0], Vector[SizeOf(TCnAESCTRNonce)], SizeOf(TCnAESCTRIv));
    T := UInt32HostToNetwork(Cnt);
    Move(T, Vector[SizeOf(TCnAESCTRNonce) + SizeOf(TCnAESCTRIv)], SizeOf(Cardinal));

    EncryptAES128(Vector, ExpandedKey, TempOut);                                   // Key �ȼ���ƴ������ Iv
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // ���ܽ�����������
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));                                    // ���Ľ��д�����Ľ��
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESWriteError);

    Inc(Cnt);                                                                   // ��������һ
    Dec(Count, SizeOf(TCnAESBuffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);

    Move(Nonce[0], Vector[0], SizeOf(TCnAESCTRNonce));
    Move(InitVector[0], Vector[SizeOf(TCnAESCTRNonce)], SizeOf(TCnAESCTRIv));
    T := UInt32HostToNetwork(Cnt);
    Move(T, Vector[SizeOf(TCnAESCTRNonce) + SizeOf(TCnAESCTRIv)], SizeOf(Cardinal));

    EncryptAES128(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, Count);  // ���д���ֻ�������ĳ��ȵĲ��֣�����������
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES192StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  EncryptAES192StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure EncryptAES192StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done, Cnt, T: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Cnt := 1;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);

    Move(Nonce[0], Vector[0], SizeOf(TCnAESCTRNonce));
    Move(InitVector[0], Vector[SizeOf(TCnAESCTRNonce)], SizeOf(TCnAESCTRIv));
    T := UInt32HostToNetwork(Cnt);
    Move(T, Vector[SizeOf(TCnAESCTRNonce) + SizeOf(TCnAESCTRIv)], SizeOf(Cardinal));

    EncryptAES192(Vector, ExpandedKey, TempOut);                                   // Key �ȼ���ƴ������ Iv
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // ���ܽ�����������
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));                                    // ���Ľ��д�����Ľ��
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESWriteError);

    Inc(Cnt);                                                                   // ��������һ
    Dec(Count, SizeOf(TCnAESBuffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);

    Move(Nonce[0], Vector[0], SizeOf(TCnAESCTRNonce));
    Move(InitVector[0], Vector[SizeOf(TCnAESCTRNonce)], SizeOf(TCnAESCTRIv));
    T := UInt32HostToNetwork(Cnt);
    Move(T, Vector[SizeOf(TCnAESCTRNonce) + SizeOf(TCnAESCTRIv)], SizeOf(Cardinal));

    EncryptAES192(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, Count);  // ���д���ֻ�������ĳ��ȵĲ��֣�����������
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

procedure EncryptAES256StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  EncryptAES256StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure EncryptAES256StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  TempIn, TempOut, Vector: TCnAESBuffer;
  Done, Cnt, T: Cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then Exit;

  Cnt := 1;
  while Count >= SizeOf(TCnAESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESReadError);

    Move(Nonce[0], Vector[0], SizeOf(TCnAESCTRNonce));
    Move(InitVector[0], Vector[SizeOf(TCnAESCTRNonce)], SizeOf(TCnAESCTRIv));
    T := UInt32HostToNetwork(Cnt);
    Move(T, Vector[SizeOf(TCnAESCTRNonce) + SizeOf(TCnAESCTRIv)], SizeOf(Cardinal));

    EncryptAES256(Vector, ExpandedKey, TempOut);                                   // Key �ȼ���ƴ������ Iv
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // ���ܽ�����������
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, SizeOf(TempIn));                                    // ���Ľ��д�����Ľ��
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorAESWriteError);

    Inc(Cnt);                                                                   // ��������һ
    Dec(Count, SizeOf(TCnAESBuffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESReadError);

    Move(Nonce[0], Vector[0], SizeOf(TCnAESCTRNonce));
    Move(InitVector[0], Vector[SizeOf(TCnAESCTRNonce)], SizeOf(TCnAESCTRIv));
    T := UInt32HostToNetwork(Cnt);
    Move(T, Vector[SizeOf(TCnAESCTRNonce) + SizeOf(TCnAESCTRIv)], SizeOf(Cardinal));

    EncryptAES256(Vector, ExpandedKey, TempOut);
    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;
    Done := Dest.Write(TempIn, Count);  // ���д���ֻ�������ĳ��ȵĲ��֣�����������
    if Done < Count then
      raise EStreamError.Create(SCnErrorAESWriteError);
  end;
end;

// Stream Decryption Routines (CTR mode)

{$IFNDEF BCB5OR6}

// �� C++Builder �� overload �������⣬������������ Delphi �¿���
procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  DecryptAES128StreamCTR(Source, Count, Key, Nonce, InitVector, Dest);
end;

procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  DecryptAES128StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  DecryptAES192StreamCTR(Source, Count, Key, Nonce, InitVector, Dest);
end;

procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  DecryptAES192StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  DecryptAES256StreamCTR(Source, Count, Key, Nonce, InitVector, Dest);
end;

procedure DecryptAESStreamCTR(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  DecryptAES256StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

{$ENDIF}

procedure DecryptAES128StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey128;
begin
  ExpandAESKeyForEncryption128(Key, ExpandedKey);
  DecryptAES128StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure DecryptAES128StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey128; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES128StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure DecryptAES192StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey192;
begin
  ExpandAESKeyForEncryption192(Key, ExpandedKey);
  DecryptAES192StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure DecryptAES192StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey192; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES192StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure DecryptAES256StreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnAESKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
var
  ExpandedKey: TCnAESExpandedKey256;
begin
  ExpandAESKeyForEncryption256(Key, ExpandedKey);
  DecryptAES256StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

procedure DecryptAES256StreamCTRExpanded(Source: TStream; Count: Cardinal;
  const ExpandedKey: TCnAESExpandedKey256; const Nonce: TCnAESCTRNonce;
  const InitVector: TCnAESCTRIv; Dest: TStream);
begin
  EncryptAES256StreamCTRExpanded(Source, Count, ExpandedKey, Nonce, InitVector, Dest);
end;

// AES ECB �����ַ���������ת����ʮ������
function AESEncryptEcbStrToHex(Value: AnsiString; Key: AnsiString;
  KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[1])^, Length(Value));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamECB(SS, 0, AESKey128, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamECB(SS, 0, AESKey192, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamECB(SS, 0, AESKey256, DS);
      end;
    end;

    Result := AnsiString(DataToHex(DS.Memory, DS.Size));
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES ECB ����ʮ�������ַ���
function AESDecryptEcbStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  Tmp: TBytes;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    Tmp := HexToBytes(string(HexStr));
    SS.Write(PAnsiChar(@Tmp[0])^, Length(Tmp));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamECB(SS, SS.Size - SS.Position, AESKey128, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamECB(SS, SS.Size - SS.Position, AESKey192, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamECB(SS, SS.Size - SS.Position, AESKey256, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    Move(PAnsiChar(DS.Memory)^, Result[1], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CBC �����ַ���������ת����ʮ������
function AESEncryptCbcStrToHex(Value: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[1])^, Length(Value));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamCBC(SS, 0, AESKey128, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamCBC(SS, 0, AESKey192, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamCBC(SS, 0, AESKey256, Iv, DS);
      end;
    end;

    Result := AnsiString(DataToHex(DS.Memory, DS.Size));
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CBC ����ʮ�������ַ���
function AESDecryptCbcStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  Tmp: TBytes;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    Tmp := HexToBytes(string(HexStr));
    SS.Write(PAnsiChar(@Tmp[0])^, Length(Tmp));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamCBC(SS, SS.Size - SS.Position, AESKey128, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamCBC(SS, SS.Size - SS.Position, AESKey192, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamCBC(SS, SS.Size - SS.Position, AESKey256, Iv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    Move(PAnsiChar(DS.Memory)^, Result[1], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CFB ģʽ�����ַ���������ת����ʮ������
function AESEncryptCfbStrToHex(Value: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[1])^, Length(Value));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamCFB(SS, 0, AESKey128, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamCFB(SS, 0, AESKey192, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamCFB(SS, 0, AESKey256, Iv, DS);
      end;
    end;

    Result := AnsiString(DataToHex(DS.Memory, DS.Size));
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CFB ����ʮ�������ַ���
function AESDecryptCfbStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  Tmp: TBytes;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    Tmp := HexToBytes(string(HexStr));
    SS.Write(PAnsiChar(@Tmp[0])^, Length(Tmp));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamCFB(SS, SS.Size - SS.Position, AESKey128, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamCFB(SS, SS.Size - SS.Position, AESKey192, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamCFB(SS, SS.Size - SS.Position, AESKey256, Iv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    Move(PAnsiChar(DS.Memory)^, Result[1], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES OFB ģʽ�����ַ���������ת����ʮ������
function AESEncryptOfbStrToHex(Value: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[1])^, Length(Value));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamOFB(SS, 0, AESKey128, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamOFB(SS, 0, AESKey192, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamOFB(SS, 0, AESKey256, Iv, DS);
      end;
    end;

    Result := AnsiString(DataToHex(DS.Memory, DS.Size));
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES OFB ����ʮ�������ַ���
function AESDecryptOfbStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Iv: TCnAESBuffer; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  Tmp: TBytes;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    Tmp := HexToBytes(string(HexStr));
    SS.Write(PAnsiChar(@Tmp[0])^, Length(Tmp));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamOFB(SS, SS.Size - SS.Position, AESKey128, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamOFB(SS, SS.Size - SS.Position, AESKey192, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamOFB(SS, SS.Size - SS.Position, AESKey256, Iv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    Move(PAnsiChar(DS.Memory)^, Result[1], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CTR ģʽ�����ַ���������ת����ʮ������
function AESEncryptCtrStrToHex(Value: AnsiString; Key: AnsiString;
  const Nonce: TCnAESCTRNonce; const Iv: TCnAESCTRIv; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[1])^, Length(Value));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamCTR(SS, 0, AESKey128, Nonce, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamCTR(SS, 0, AESKey192, Nonce, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamCTR(SS, 0, AESKey256, Nonce, Iv, DS);
      end;
    end;

    Result := AnsiString(DataToHex(DS.Memory, DS.Size));
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CTR ����ʮ�������ַ���
function AESDecryptCtrStrFromHex(const HexStr: AnsiString; Key: AnsiString;
  const Nonce: TCnAESCTRNonce; const Iv: TCnAESCTRIv; KeyBit: TCnKeyBitType): AnsiString;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  Tmp: TBytes;
begin
  Result := '';
  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    Tmp := HexToBytes(string(HexStr));
    SS.Write(PAnsiChar(@Tmp[0])^, Length(Tmp));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamCTR(SS, SS.Size - SS.Position, AESKey128, Nonce, Iv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamCTR(SS, SS.Size - SS.Position, AESKey192, Nonce, Iv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamCTR(SS, SS.Size - SS.Position, AESKey256, Nonce, Iv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    Move(PAnsiChar(DS.Memory)^, Result[1], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES ECB ģʽ�����ֽ�����
function AESEncryptEcbBytes(Value, Key: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamECB(SS, 0, AESKey128, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamECB(SS, 0, AESKey192, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamECB(SS, 0, AESKey256, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES ECB ģʽ�����ֽ�����
function AESDecryptEcbBytes(Value, Key: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamECB(SS, SS.Size - SS.Position, AESKey128, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamECB(SS, SS.Size - SS.Position, AESKey192, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamECB(SS, SS.Size - SS.Position, AESKey256, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CBC ģʽ�����ֽ�����
function AESEncryptCbcBytes(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESBuffer;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamCBC(SS, 0, AESKey128, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamCBC(SS, 0, AESKey192, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamCBC(SS, 0, AESKey256, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CBC ģʽ�����ֽ�����
function AESDecryptCbcBytes(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESBuffer;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamCBC(SS, SS.Size - SS.Position, AESKey128, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamCBC(SS, SS.Size - SS.Position, AESKey192, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamCBC(SS, SS.Size - SS.Position, AESKey256, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CFB ģʽ�����ֽ�����
function AESEncryptCfbBytes(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESBuffer;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamCFB(SS, 0, AESKey128, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamCFB(SS, 0, AESKey192, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamCFB(SS, 0, AESKey256, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CFB ģʽ�����ֽ�����
function AESDecryptCfbBytes(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESBuffer;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamCFB(SS, SS.Size - SS.Position, AESKey128, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamCFB(SS, SS.Size - SS.Position, AESKey192, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamCFB(SS, SS.Size - SS.Position, AESKey256, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES OFB ģʽ�����ֽ�����
function AESEncryptOfbBytes(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESBuffer;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamOFB(SS, 0, AESKey128, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamOFB(SS, 0, AESKey192, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamOFB(SS, 0, AESKey256, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES OFB ģʽ�����ֽ�����
function AESDecryptOfbBytes(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESBuffer;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamOFB(SS, SS.Size - SS.Position, AESKey128, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamOFB(SS, SS.Size - SS.Position, AESKey192, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamOFB(SS, SS.Size - SS.Position, AESKey256, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CTR ģʽ�����ֽ�����
function AESEncryptCtrBytes(Value, Key, Nonce, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESCTRIv;
  AESNonce: TCnAESCTRNonce;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));

    FillChar(AESNonce, SizeOF(AESNonce), 0);
    Move(PAnsiChar(Nonce)^, AESNonce, Min(SizeOf(AESNonce), Length(Nonce)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        EncryptAES128StreamCTR(SS, 0, AESKey128, AESNonce, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        EncryptAES192StreamCTR(SS, 0, AESKey192, AESNonce, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        EncryptAES256StreamCTR(SS, 0, AESKey256, AESNonce, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES CTR ģʽ�����ֽ�����
function AESDecryptCtrBytes(Value, Key, Nonce, Iv: TBytes; KeyBit: TCnKeyBitType): TBytes;
var
  SS, DS: TMemoryStream;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AESIv: TCnAESCTRIv;
  AESNonce: TCnAESCTRNonce;
begin
  if Length(Value) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  SS := nil;
  DS := nil;

  try
    SS := TMemoryStream.Create;
    SS.Write(PAnsiChar(@Value[0])^, Length(Value));
    SS.Position := 0;

    FillChar(AESIv, SizeOF(AESIv), 0);
    Move(PAnsiChar(Iv)^, AESIv, Min(SizeOf(AESIv), Length(Iv)));

    FillChar(AESNonce, SizeOF(AESNonce), 0);
    Move(PAnsiChar(Nonce)^, AESNonce, Min(SizeOf(AESNonce), Length(Nonce)));
    DS := TMemoryStream.Create;

    case KeyBit of
    kbt128:
      begin
        FillChar(AESKey128, SizeOf(AESKey128), 0);
        Move(PAnsiChar(Key)^, AESKey128, Min(SizeOf(AESKey128), Length(Key)));
        DecryptAES128StreamCTR(SS, SS.Size - SS.Position, AESKey128, AESNonce, AESIv, DS);
      end;
    kbt192:
      begin
        FillChar(AESKey192, SizeOf(AESKey192), 0);
        Move(PAnsiChar(Key)^, AESKey192, Min(SizeOf(AESKey192), Length(Key)));
        DecryptAES192StreamCTR(SS, SS.Size - SS.Position, AESKey192, AESNonce, AESIv, DS);
      end;
    kbt256:
      begin
        FillChar(AESKey256, SizeOf(AESKey256), 0);
        Move(PAnsiChar(Key)^, AESKey256, Min(SizeOf(AESKey256), Length(Key)));
        DecryptAES256StreamCTR(SS, SS.Size - SS.Position, AESKey256, AESNonce, AESIv, DS);
      end;
    end;

    SetLength(Result, DS.Size);
    DS.Position := 0;
    DS.Read(Result[0], DS.Size);
  finally
    SS.Free;
    DS.Free;
  end;
end;

// AES ECB ģʽ�����ֽ����鲢����ת����ʮ������
function AESEncryptEcbBytesToHex(Value, Key: TBytes; KeyBit: TCnKeyBitType): AnsiString;
begin
  Result := AnsiString(BytesToHex(AESEncryptEcbBytes(Value, Key, KeyBit)));
end;

// AES ECB ����ʮ�������ַ����������ֽ�����
function AESDecryptEcbBytesFromHex(const HexStr: AnsiString; Key: TBytes;
  KeyBit: TCnKeyBitType): TBytes;
begin
  Result := AESDecryptEcbBytes(HexToBytes(string(HexStr)), Key, KeyBit);
end;

// AES CBC ģʽ�����ֽ����鲢����ת����ʮ������
function AESEncryptCbcBytesToHex(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): AnsiString;
begin
  Result := AnsiString(BytesToHex(AESEncryptCbcBytes(Value, Key, Iv, KeyBit)));
end;

// AES CBC ����ʮ�������ַ����������ֽ�����
function AESDecryptCbcBytesFromHex(const HexStr: AnsiString; Key, Iv: TBytes;
  KeyBit: TCnKeyBitType): TBytes;
begin
  Result := AESDecryptCbcBytes(HexToBytes(string(HexStr)), Key, Iv, KeyBit);
end;

// AES CFB ģʽ�����ֽ����鲢����ת����ʮ������
function AESEncryptCfbBytesToHex(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): AnsiString;
begin
  Result := AnsiString(BytesToHex(AESEncryptCfbBytes(Value, Key, Iv, KeyBit)));
end;

// AES CFB ����ʮ�������ַ����������ֽ�����
function AESDecryptCfbBytesFromHex(const HexStr: AnsiString; Key, Iv: TBytes;
  KeyBit: TCnKeyBitType): TBytes;
begin
  Result := AESDecryptCfbBytes(HexToBytes(string(HexStr)), Key, Iv, KeyBit);
end;

// AES OFB ģʽ�����ֽ����鲢����ת����ʮ������
function AESEncryptOfbBytesToHex(Value, Key, Iv: TBytes; KeyBit: TCnKeyBitType): AnsiString;
begin
  Result := AnsiString(BytesToHex(AESEncryptOfbBytes(Value, Key, Iv, KeyBit)));
end;

// AES OFB ����ʮ�������ַ����������ֽ�����
function AESDecryptOfbBytesFromHex(const HexStr: AnsiString; Key, Iv: TBytes;
  KeyBit: TCnKeyBitType): TBytes;
begin
  Result := AESDecryptOfbBytes(HexToBytes(string(HexStr)), Key, Iv, KeyBit);
end;

// AES CTR ģʽ�����ֽ����鲢����ת����ʮ������
function AESEncryptCtrBytesToHex(Value, Key, Nonce, Iv: TBytes; KeyBit: TCnKeyBitType): AnsiString;
begin
  Result := AnsiString(BytesToHex(AESEncryptCtrBytes(Value, Key, Nonce, Iv, KeyBit)));
end;

// AES CTR ����ʮ�������ַ����������ֽ�����
function AESDecryptCtrBytesFromHex(const HexStr: AnsiString; Key, Nonce, Iv: TBytes;
  KeyBit: TCnKeyBitType): TBytes;
begin
  Result := AESDecryptCtrBytes(HexToBytes(string(HexStr)), Key, Nonce, Iv, KeyBit);
end;

end.

