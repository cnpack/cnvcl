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

unit CnPemUtils;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�PEM ��ʽ�������뵥Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע������Ԫʵ���� PEM ��ʽ�Ķ�ȡ�뱣�棬�����ӽ��ܻ��ơ�
*           ����Ҳʵ���� PKCS1/PKCS5/PKCS7/ISO10126 �ȶ��봦����ơ�
*           ע����֧�� PKCS12 �淶��֤�鼰��Կ��װ��ʽ
* ����ƽ̨��WinXP + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2024.05.27 V1.6
*               �������� ISO10126 ����Ĵ�����
*           2023.12.14 V1.5
*               ���� SaveMemoryToPemStream ������δ��������
*           2022.03.09 V1.4
*               �������� PKCS5 ����Ĵ�����
*           2021.05.14 V1.3
*               �����ĸ� PKCS7 ����Ĵ�����
*           2020.03.27 V1.2
*               ģ�� Openssl ʵ�� PEM �ļ���д�룬ֻ֧�ֲ��ּ����㷨�����
*               Ŀǰд����� des/3des/aes128/192/256 PKCS7 ���룬���� Openssl 1.0.2g
*           2020.03.23 V1.1
*               ģ�� Openssl ʵ�� PEM �ļ��ܶ�ȡ��ֻ֧�ֲ��ּ����㷨�����
*               Ŀǰ��ȡ���� des/3des/aes128/192/256 PKCS7 ���룬���� Openssl 1.0.2g
*           2020.03.18 V1.0
*               ������Ԫ���� CnRSA �ж�������
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnRandom, CnKDF, CnBase64, CnAES, CnDES, CnSM4;

const
  CN_PKCS1_BLOCK_TYPE_PRIVATE_00       = 00;
  {* PKCS1 ����ʱ�Ŀ������ֶ�ֵһ��Ĭ��Ӧ���� RSA ��˽Կ���ܳ���}

  CN_PKCS1_BLOCK_TYPE_PRIVATE_FF       = 01;
  {* PKCS1 ����ʱ�Ŀ������ֶ�ֵ����Ĭ��Ӧ���� RSA ��˽Կǩ������}

  CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM    = 02;
  {* PKCS1 ����ʱ�Ŀ������ֶ�ֵ����Ĭ��Ӧ���� RSA �Ĺ�Կ���ܳ���}

  CN_PKCS1_PADDING_SIZE                = 11;
  {* PKCS1 �Ĵ�С��ֵ��һ��ǰ�� 00��һ�������ֽڡ����� 8 �ֽ�������䣬һ������� 00 ��β}

  CN_PKCS5_BLOCK_SIZE                  = 8;
  {* PKCS5 ��Ĭ�Ͽ��С}

type
  TCnKeyHashMethod = (ckhMd5, ckhSha256);
  {* PEM ��ʽ֧�ֵ��Ӵ�����}

  TCnKeyEncryptMethod = (ckeNone, ckeDES, cke3DES, ckeAES128, ckeAES192, ckeAES256,
    ckeSM4);
  {* PEM ��ʽ֧�ֵļ�������}

// ======================= PEM �ļ���д������֧�ּӽ��� ========================

function LoadPemFileToMemory(const FileName: string; const ExpectHead: string;
  const ExpectTail: string; MemoryStream: TMemoryStream; const Password: string = '';
  KeyHashMethod: TCnKeyHashMethod = ckhMd5): Boolean;
{* �� PEM ��ʽ������ļ�����ָ֤��ͷβ�����ʵ�����ݲ����ܽ��� Base64 ���롣

   ������
     const FileName: string               - ��������ļ���
     const ExpectHead: string             - ������ͷ��
     const ExpectTail: string             - ������β��
     MemoryStream: TMemoryStream          - ������ڴ���
     const Password: string               - ����ļ������ܣ����ڴ��ṩ����
     KeyHashMethod: TCnKeyHashMethod      - �ļ�����ʹ�õ��Ӵ�����

   ����ֵ��Boolean                        - ���ض����Ƿ�ɹ�
}

function LoadPemStreamToMemory(Stream: TStream; const ExpectHead: string;
  const ExpectTail: string; MemoryStream: TMemoryStream; const Password: string = '';
  KeyHashMethod: TCnKeyHashMethod = ckhMd5): Boolean;
{* �� PEM ��ʽ������ļ�����ָ֤��ͷβ�����ʵ�����ݲ����ܽ��� Base64 ���롣

   ������
     Stream: TStream                      - ���������
     const ExpectHead: string             - ������ͷ��
     const ExpectTail: string             - ������β��
     MemoryStream: TMemoryStream          - ������ڴ���
     const Password: string               - ����������ܣ����ڴ��ṩ����
     KeyHashMethod: TCnKeyHashMethod      - ������ʹ�õ��Ӵ�����

   ����ֵ��Boolean                        - ���ض����Ƿ�ɹ�
}

function SaveMemoryToPemFile(const FileName: string; const Head: string; const Tail: string;
  MemoryStream: TMemoryStream; KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''; Append: Boolean = False): Boolean;
{* �� Stream �����ݽ��� Base64 �������ܷ��в������ļ�ͷβ��д���ļ���Append Ϊ True ʱ��ʾ׷�ӡ�

   ������
     const FileName: string                               - ��д����ļ���
     const Head: string                                   - д���ͷ��
     const Tail: string                                   - д���β��
     MemoryStream: TMemoryStream                          - ��д�������
     KeyEncryptMethod: TCnKeyEncryptMethod                - ���ü������ͣ�Ĭ�ϲ�����
     KeyHashMethod: TCnKeyHashMethod                      - �����Ӵ�����
     const Password: string                               - �������룬��������򴫿�
     Append: Boolean                                      - �Ƿ���׷�ӵķ�ʽд��

   ����ֵ��Boolean                                        - �����Ƿ�д��ɹ�
}

function SaveMemoryToPemStream(Stream: TStream; const Head: string; const Tail: string;
  MemoryStream: TMemoryStream; KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''; Append: Boolean = False): Boolean;
{* �� Stream �����ݽ��� Base64 �������ܷ��в�����ͷβ��д������Append Ϊ True ʱ��ʾ׷�ӡ�

   ������
     Stream: TStream                                      - ��д�����
     const Head: string                                   - д���ͷ��
     const Tail: string                                   - д���β��
     MemoryStream: TMemoryStream                          - ��д�������
     KeyEncryptMethod: TCnKeyEncryptMethod                - ���ü������ͣ�Ĭ�ϲ�����
     KeyHashMethod: TCnKeyHashMethod                      - �����Ӵ����ͣ�Ĭ�ϲ��Ӵ�
     const Password: string                               - �������룬��������򴫿�
     Append: Boolean                                      - �Ƿ���׷�ӵķ�ʽд��

   ����ֵ��Boolean                                        - �����Ƿ�д��ɹ�
}

// ===================== PKCS1 / PKCS7 Padding ���봦���� ====================

function AddPKCS1Padding(PaddingType: Integer; BlockSize: Integer; Data: Pointer;
  DataByteLen: Integer; OutStream: TStream): Boolean;
{* �����ݿ鲹���������д�� Stream �У����سɹ�����ڲ������ô����롣
   PaddingType ȡ 0��1��2��BlockLen �ֽ����� 128 �ȡ���ʽ����
   EB = 00 || BT || PS || 00 || D
   ���� 00 ��ǰ���涨�ֽڣ�BT �� 1 �ֽڵ� PaddingType��0 1 2 �ֱ���� 00 FF �����
   PS �����Ķ��ֽ����ݣ��� 00 �ǹ涨�Ľ�β�ֽڡ�

   ������
     PaddingType: Integer                 - �������ͣ�ȡ 0 1 2
     BlockSize: Integer                   - �������ֽڳ���
     Data: Pointer                        - ���������ݿ�ĵ�ַ
     DataByteLen: Integer                 - ���������ݿ���ֽڳ���
     OutStream: TStream                   - �����

   ����ֵ��Boolean                        - ���ض��������Ƿ����ӳɹ�
}

function RemovePKCS1Padding(InData: Pointer; InDataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer): Boolean;
{* ȥ�����ݿ�� PKCS1 �� Padding�����سɹ����OutBuf ��ָ����Ŀ��ó�������������б�֤��
   ��ɹ���OutLen ����ԭ�����ݳ��ȡ�

   ������
     InData: Pointer                      - ��ȥ����������ݿ�ĵ�ַ
     InDataByteLen: Integer               - ��ȥ����������ݿ���ֽڳ���
     OutBuf: Pointer                      - ���������ȥ�����ݵ������䳤�ȱ����㹻
     out OutByteLen: Integer              - ����ȥ�����������ݳ���

   ����ֵ��Boolean                        - ���ض��������Ƿ�ȥ���ɹ�
}

function GetPKCS7PaddingByteLength(OrignalByteLen: Integer; BlockSize: Integer): Integer;
{* ����ԭʼ������鳤�ȼ��� PKCS7 �����ĳ��ȡ�

   ������
     OrignalByteLen: Integer              - ԭʼ�����ֽڳ���
     BlockSize: Integer                   - PKCS7 ���ֽڳ���

   ����ֵ��Integer                        - ���� PKCS7 ���������ֽڳ���
}

procedure AddPKCS7Padding(Stream: TMemoryStream; BlockSize: Integer);
{* ������ĩβ���� PKCS7 �涨����䡰����������������ݡ�

   ������
     Stream: TMemoryStream                - ��������ڴ������ݣ��������ݽ�׷��д����β��
     BlockSize: Integer                   - PKCS7 ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure RemovePKCS7Padding(Stream: TMemoryStream);
{* ȥ�� PKCS7 �涨��ĩβ��䡰����������������ݡ�

   ������
     Stream: TMemoryStream                - ��ȥ��������ڴ���

   ����ֵ�����ޣ�}

function StrAddPKCS7Padding(const Str: AnsiString; BlockSize: Integer): AnsiString;
{* ���ַ���ĩβ���� PKCS7 �涨����䡰����������������ݡ�

   ������
     const Str: AnsiString                - ��������ַ���
     BlockSize: Integer                   - PKCS7 ���ֽڳ���

   ����ֵ��AnsiString                     - ���ض������ַ���
}

function StrRemovePKCS7Padding(const Str: AnsiString): AnsiString;
{* ȥ�� PKCS7 �涨���ַ���ĩβ��䡰����������������ݡ�

   ������
     const Str: AnsiString                - ��ȥ��������ַ���

   ����ֵ��AnsiString                     - ����ȥ���������ַ���
}

procedure BytesAddPKCS7Padding(var Data: TBytes; BlockSize: Integer);
{* ���ֽ�����ĩβ���� PKCS7 �涨����䡰����������������ݡ�

   ������
     var Data: TBytes                     - ��������ֽ����飬�������ݽ�׷����β��
     BlockSize: Integer                   - PKCS7 ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BytesRemovePKCS7Padding(var Data: TBytes);
{* ȥ�� PKCS7 �涨���ֽ�����ĩβ��䡰����������������ݡ�

   ������
     var Data: TBytes                     - ��ȥ��������ֽ�����

   ����ֵ�����ޣ�
}

procedure AddPKCS5Padding(Stream: TMemoryStream);
{* ������ĩβ���� PKCS5 �涨����䡰����������������ݣ���ѭ PKCS7 �淶�����С�̶�Ϊ 8 �ֽڡ�

   ������
     Stream: TMemoryStream                - ��������ڴ������������ݽ�׷����β��

   ����ֵ�����ޣ�
}

procedure RemovePKCS5Padding(Stream: TMemoryStream);
{* ȥ�� PKCS7 �涨��ĩβ��䡰����������������ݣ���ѭ PKCS7 �淶�����С�̶�Ϊ 8 �ֽڡ�

   ������
     Stream: TMemoryStream                - ��ȥ��������ڴ���

   ����ֵ�����ޣ�
}

function StrAddPKCS5Padding(const Str: AnsiString): AnsiString;
{* ���ַ���ĩβ���� PKCS5 �涨����䡰����������������ݣ���ѭ PKCS7 �淶�����С�̶�Ϊ 8 �ֽڡ�

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��AnsiString                     - ���ض������ַ���
}

function StrRemovePKCS5Padding(const Str: AnsiString): AnsiString;
{* ȥ�� PKCS5 �涨���ַ���ĩβ��䡰����������������ݣ���ѭ PKCS7 �淶�����С�̶�Ϊ 8 �ֽڡ�

   ������
     const Str: AnsiString                - ��ȥ��������ַ���

   ����ֵ��AnsiString                     - ����ȥ���������ַ���
}

procedure BytesAddPKCS5Padding(var Data: TBytes);
{* ���ֽ�����ĩβ���� PKCS5 �涨����䡰����������������ݣ���ѭ PKCS7 �淶�����С�̶�Ϊ 8 �ֽڡ�

   ������
     var Data: TBytes                     - ��������ֽ����飬�������ݽ�׷����β��

   ����ֵ�����ޣ�
}

procedure BytesRemovePKCS5Padding(var Data: TBytes);
{* ȥ�� PKCS7 �涨���ֽ�����ĩβ��䡰����������������ݣ���ѭ PKCS7 �淶�����С�̶�Ϊ 8 �ֽڡ�

   ������
     var Data: TBytes                     - ��ȥ��������ֽ�����

   ����ֵ�����ޣ�
}

function GetISO10126PaddingByteLength(OrignalByteLen: Integer; BlockSize: Integer): Integer;
{* ����ԭʼ������鳤�ȼ��� ISO10126Padding �����ĳ��ȡ�

   ������
     OrignalByteLen: Integer              - ԭʼ�����ֽڳ���
     BlockSize: Integer                   - ISO10126 ���ֽڳ���

   ����ֵ��Integer                        - ���� PKCS7 ���������ֽڳ���
}

procedure AddISO10126Padding(Stream: TMemoryStream; BlockSize: Integer);
{* ������ĩβ���� ISO10126Padding �涨����䡰��ͼ�����������ݡ�

   ������
     Stream: TMemoryStream                - ��������ڴ������������ݽ�׷����β��
     BlockSize: Integer                   - ISO10126 ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure RemoveISO10126Padding(Stream: TMemoryStream);
{* ȥ�� ISO10126Padding �涨��ĩβ��䡰��ͼ�����������ݡ�

   ������
     Stream: TMemoryStream                - ��ȥ��������ڴ���

   ����ֵ�����ޣ�
}

function StrAddISO10126Padding(const Str: AnsiString; BlockSize: Integer): AnsiString;
{* ���ַ���ĩβ���� ISO10126Padding �涨����䡰��ͼ�����������ݡ�

   ������
     const Str: AnsiString                - ��������ַ���
     BlockSize: Integer                   - ISO10126 ���ֽڴ�С

   ����ֵ��AnsiString                     - ���ض������ַ���
}

function StrRemoveISO10126Padding(const Str: AnsiString): AnsiString;
{* ȥ�� ISO10126Padding �涨���ַ���ĩβ��䡰��ͼ�����������ݡ�

   ������
     const Str: AnsiString                - ��ȥ��������ַ���

   ����ֵ��AnsiString                     - ����ȥ���������ַ���
}

procedure BytesAddISO10126Padding(var Data: TBytes; BlockSize: Integer);
{* ���ֽ�����ĩβ���� ISO10126Padding �涨����䡰��ͼ�����������ݡ�

   ������
     var Data: TBytes                     - ��������ֽ����飬�������ݽ�׷����β��
     BlockSize: Integer                   - ISO10126 ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure BytesRemoveISO10126Padding(var Data: TBytes);
{* ȥ�� ISO10126Padding �涨���ֽ�����ĩβ��䡰��ͼ�����������ݡ�

   ������
     var Data: TBytes                     - ��ȥ��������ֽ�����

   ����ֵ�����ޣ�
}

implementation

const
  ENC_HEAD_PROCTYPE = 'Proc-Type:';
  ENC_HEAD_PROCTYPE_NUM = '4';
  ENC_HEAD_ENCRYPTED = 'ENCRYPTED';
  ENC_HEAD_DEK = 'DEK-Info:';

  ENC_TYPE_AES128 = 'AES-128';
  ENC_TYPE_AES192 = 'AES-192';
  ENC_TYPE_AES256 = 'AES-256';
  ENC_TYPE_DES    = 'DES';
  ENC_TYPE_3DES   = 'DES-EDE3';
  ENC_TYPE_SM4    = 'SM4';

  ENC_BLOCK_CBC   = 'CBC';

  ENC_TYPE_STRS: array[TCnKeyEncryptMethod] of string =
    ('', ENC_TYPE_DES, ENC_TYPE_3DES, ENC_TYPE_AES128, ENC_TYPE_AES192,
    ENC_TYPE_AES256, ENC_TYPE_SM4);

  ENC_TYPE_BLOCK_SIZE: array[TCnKeyEncryptMethod] of Byte =
    (0, 8, 8, 16, 16, 16, 16);

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function AddPKCS1Padding(PaddingType, BlockSize: Integer; Data: Pointer;
  DataByteLen: Integer; OutStream: TStream): Boolean;
var
  I: Integer;
  B, F: Byte;
begin
  Result := False;
  if (Data = nil) or (DataByteLen <= 0) then
    Exit;

  // ���������
  if DataByteLen > BlockSize - CN_PKCS1_PADDING_SIZE then
    Exit;

  B := 0;
  OutStream.Write(B, 1);       // дǰ���ֽ� 00
  B := PaddingType;
  F := BlockSize - DataByteLen - 3; // 3 ��ʾһ��ǰ�� 00��һ�������ֽڡ�һ������� 00 ��β

  OutStream.Write(B, 1);
  case PaddingType of
    CN_PKCS1_BLOCK_TYPE_PRIVATE_00:
      begin
        B := 0;
        for I := 1 to F do
          OutStream.Write(B, 1);
      end;
    CN_PKCS1_BLOCK_TYPE_PRIVATE_FF:
      begin
        B := $FF;
        for I := 1 to F do
          OutStream.Write(B, 1);
      end;
    CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM:
      begin
        Randomize;
        for I := 1 to F do
        begin
          B := Trunc(Random(255));
          if B = 0 then
            Inc(B);
          OutStream.Write(B, 1);
        end;
      end;
  else
    Exit;
  end;

  B := 0;
  OutStream.Write(B, 1);
  OutStream.Write(Data^, DataByteLen);
  Result := True;
end;

function RemovePKCS1Padding(InData: Pointer; InDataByteLen: Integer; OutBuf: Pointer;
  out OutByteLen: Integer): Boolean;
var
  P: PAnsiChar;
  I, J, Start: Integer;
begin
  Result := False;
  OutByteLen := 0;
  I := 0;

  P := PAnsiChar(InData);
  while P[I] = #0 do // ���ַ���һ���� #0�������Ѿ���ȥ����
    Inc(I);

  if I >= InDataByteLen then
    Exit;

  Start := 0;
  case Ord(P[I]) of
    CN_PKCS1_BLOCK_TYPE_PRIVATE_00:
      begin
        // �� P[I + 1] ��ʼѰ�ҷ� 00 ����
        J := I + 1;
        while J < InDataByteLen do
        begin
          if P[J] <> #0 then
          begin
            Start := J;
            Break;
          end;
          Inc(J);
        end;
      end;
    CN_PKCS1_BLOCK_TYPE_PRIVATE_FF,
    CN_PKCS1_BLOCK_TYPE_PUBLIC_RANDOM:
      begin
        // �� P[I + 1] ��ʼѰ�ҵ���һ�� 00 ��ı���
        J := I + 1;
        while J < InDataByteLen do
        begin
          if P[J] = #0 then
          begin
            Start := J;
            Break;
          end;
          Inc(J);
        end;

        if Start <> 0 then
          Inc(Start);
      end;
    else
      Start := I; // ���� #0 ʱʵ���Ͽ����Ѿ�������� CN_PKCS1_BLOCK_TYPE_PRIVATE_00
  end;

  if Start > 0 then
  begin
    Move(P[Start], OutBuf^, InDataByteLen - Start);
    OutByteLen := InDataByteLen - Start;
    Result := True;
  end;
end;

function GetPKCS7PaddingByteLength(OrignalByteLen: Integer; BlockSize: Integer): Integer;
var
  R: Byte;
begin
  R := OrignalByteLen mod BlockSize;
  R := BlockSize - R;
  if R = 0 then
    R := R + BlockSize;
  Result := OrignalByteLen + R;
end;

procedure AddPKCS7Padding(Stream: TMemoryStream; BlockSize: Integer);
var
  R: Byte;
  Buf: array[0..255] of Byte;
begin
  R := Stream.Size mod BlockSize;
  R := BlockSize - R;
  if R = 0 then
    R := R + BlockSize;

  FillChar(Buf[0], R, R);
  Stream.Position := Stream.Size;
  Stream.Write(Buf[0], R);
end;

procedure RemovePKCS7Padding(Stream: TMemoryStream);
var
  L: Byte;
  Len: Cardinal;
  Mem: Pointer;
begin
  // ȥ�� Stream ĩβ�� 9 �� 9 ���� Padding
  if Stream.Size > 1 then
  begin
    Stream.Position := Stream.Size - 1;
    Stream.Read(L, 1);

    if Stream.Size - L < 0 then  // �ߴ粻���ף�����
      Exit;

    Len := Stream.Size - L;
    Mem := GetMemory(Len);
    if Mem <> nil then
    begin
      Move(Stream.Memory^, Mem^, Len);
      Stream.Clear;
      Stream.Write(Mem^, Len);
      FreeMemory(Mem);
    end;
  end;
end;

function StrAddPKCS7Padding(const Str: AnsiString; BlockSize: Integer): AnsiString;
var
  I, L: Integer;
  R: Byte;
begin
  L := Length(Str);
  R := L mod BlockSize;
  R := BlockSize - R;
  if R = 0 then
    R := R + BlockSize;

  SetLength(Result, L + R);
  if L > 0 then
    Move(Str[1], Result[1], L);

  for I := 1 to R do
    Result[L + I] := AnsiChar(R);
end;

function StrRemovePKCS7Padding(const Str: AnsiString): AnsiString;
var
  L: Integer;
  V: Byte;
begin
  Result := Str;
  if Result = '' then
    Exit;

  L := Length(Result);
  V := Ord(Result[L]);  // ĩ�Ǽ���ʾ���˼�

  if V <= L then
    Delete(Result, L - V + 1, V);
end;

procedure AddPKCS5Padding(Stream: TMemoryStream);
begin
  AddPKCS7Padding(Stream, CN_PKCS5_BLOCK_SIZE);
end;

procedure RemovePKCS5Padding(Stream: TMemoryStream);
begin
  RemovePKCS7Padding(Stream);
end;

function StrAddPKCS5Padding(const Str: AnsiString): AnsiString;
begin
  Result := StrAddPKCS7Padding(Str, CN_PKCS5_BLOCK_SIZE);
end;

function StrRemovePKCS5Padding(const Str: AnsiString): AnsiString;
begin
  Result := StrRemovePKCS7Padding(Str);
end;

procedure BytesAddPKCS7Padding(var Data: TBytes; BlockSize: Integer);
var
  R: Byte;
  L, I: Integer;
begin
  L := Length(Data);
  R := L mod BlockSize;
  R := BlockSize - R;
  if R = 0 then
    R := R + BlockSize;

  SetLength(Data, L + R);
  for I := 0 to R - 1 do
    Data[L + I] := R;
end;

procedure BytesRemovePKCS7Padding(var Data: TBytes);
var
  L: Integer;
  V: Byte;
begin
  L := Length(Data);
  if L = 0 then
    Exit;

  V := Ord(Data[L - 1]);  // ĩ�Ǽ���ʾ���˼����ֽ�

  if V <= L then
    SetLength(Data, L - V);
end;

procedure BytesAddPKCS5Padding(var Data: TBytes);
begin
  BytesAddPKCS7Padding(Data, CN_PKCS5_BLOCK_SIZE);
end;

procedure BytesRemovePKCS5Padding(var Data: TBytes);
begin
  BytesRemovePKCS7Padding(Data);
end;

function GetISO10126PaddingByteLength(OrignalByteLen: Integer; BlockSize: Integer): Integer;
begin
  Result := GetPKCS7PaddingByteLength(OrignalByteLen, BlockSize); // ��Ϊ��ͬ����ֱ�ӵ���
end;

procedure AddISO10126Padding(Stream: TMemoryStream; BlockSize: Integer);
var
  R: Byte;
  Buf: array[0..255] of Byte;
begin
  R := Stream.Size mod BlockSize;
  R := BlockSize - R;
  if R = 0 then
    R := R + BlockSize;

  FillChar(Buf[0], R, 0);
  Buf[R - 1] := R;
  Stream.Position := Stream.Size;
  Stream.Write(Buf[0], R);
end;

procedure RemoveISO10126Padding(Stream: TMemoryStream);
begin
  RemovePKCS7Padding(Stream); // ��Ϊ��ͬ����ֱ�ӵ���
end;

function StrAddISO10126Padding(const Str: AnsiString; BlockSize: Integer): AnsiString;
var
  I, L: Integer;
  R: Byte;
begin
  L := Length(Str);
  R := L mod BlockSize;
  R := BlockSize - R;
  if R = 0 then
    R := R + BlockSize;

  SetLength(Result, L + R);
  if L > 0 then
    Move(Str[1], Result[1], L);

  if R > 1 then
  begin
    for I := 1 to R - 1 do
      Result[L + I] := #0;
  end;
  Result[L + R] := AnsiChar(R);
end;

function StrRemoveISO10126Padding(const Str: AnsiString): AnsiString;
begin
  Result := StrRemovePKCS7Padding(Str); // ��Ϊ��ͬ����ֱ�ӵ���
end;

procedure BytesAddISO10126Padding(var Data: TBytes; BlockSize: Integer);
var
  R: Byte;
  L, I: Integer;
begin
  L := Length(Data);
  R := L mod BlockSize;
  R := BlockSize - R;
  if R = 0 then
    R := R + BlockSize;

  SetLength(Data, L + R);
  if R > 1 then
  begin
    for I := 0 to R - 2 do
      Data[L + I] := 0;
  end;
  Data[L - 1 + R] := R;
end;

procedure BytesRemoveISO10126Padding(var Data: TBytes);
begin
  BytesRemovePKCS7Padding(Data); // ��Ϊ��ͬ����ֱ�ӵ���
end;

function EncryptPemStream(KeyHash: TCnKeyHashMethod; KeyEncrypt: TCnKeyEncryptMethod;
  Stream: TStream; const Password: string; out EncryptedHead: string): Boolean;
const
  CRLF = #13#10;
var
  ES: TMemoryStream;
  Keys: array[0..31] of Byte; // ��� Key Ҳֻ�� 32 �ֽ�
  IvStr: AnsiString;
  HexIv: string;
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  AesIv: TCnAESBuffer;
  DesKey: TCnDESKey;
  Des3Key: TCn3DESKey;
  DesIv: TCnDESIv;
  Sm4Key: TCnSM4Key;
  Sm4Iv: TCnSM4Iv;
begin
  Result := False;

  // ������
  if (KeyEncrypt = ckeNone) or (Password = '') then
    Exit;

  // ������� Iv
  SetLength(IvStr, ENC_TYPE_BLOCK_SIZE[KeyEncrypt]);
  CnRandomFillBytes(@(IvStr[1]), ENC_TYPE_BLOCK_SIZE[KeyEncrypt]);
  HexIv := DataToHex(@(IvStr[1]), ENC_TYPE_BLOCK_SIZE[KeyEncrypt], True); // Ҫ���д

  EncryptedHead := ENC_HEAD_PROCTYPE + ' ' +  ENC_HEAD_PROCTYPE_NUM + ',' + ENC_HEAD_ENCRYPTED + CRLF;
  EncryptedHead := EncryptedHead + ENC_HEAD_DEK + ' ' + ENC_TYPE_STRS[KeyEncrypt]
    + '-' + ENC_BLOCK_CBC + ',' + HexIv + CRLF;

  ES := TMemoryStream.Create;
  Stream.Position := 0;

  try
    if KeyHash = ckhMd5 then
    begin
      if not CnGetDeriveKey(AnsiString(Password), IvStr, @Keys[0], SizeOf(Keys)) then
        Exit;
    end
    else if KeyHash = ckhSha256 then
    begin
      if not CnGetDeriveKey(AnsiString(Password), IvStr, @Keys[0], SizeOf(Keys), ckdSha256) then
        Exit;
    end
    else
      Exit;

    case KeyEncrypt of
      ckeDES:
        begin
          Move(Keys[0], DesKey[0], SizeOf(TCnDESKey));
          Move(IvStr[1], DesIv[0], SizeOf(TCnDESIv));

          DESEncryptStreamCBC(Stream, Stream.Size, DesKey, DesIv, ES);
          Result := True;
        end;
      cke3DES:
        begin
          Move(Keys[0], Des3Key[0], SizeOf(TCn3DESKey));
          Move(IvStr[1], DesIv[0], SizeOf(TCn3DESIv));

          TripleDESEncryptStreamCBC(Stream, Stream.Size, Des3Key, DesIv, ES);
          Result := True;
        end;
      ckeAES128:
        begin
          Move(Keys[0], AESKey128[0], SizeOf(TCnAESKey128));
          Move(IvStr[1], AesIv[0], SizeOf(TCnAESBuffer));

          EncryptAES128StreamCBC(Stream, Stream.Size, AESKey128, AesIv, ES);
          Result := True;
        end;
      ckeAES192:
        begin
          Move(Keys[0], AESKey192[0], SizeOf(TCnAESKey192));
          Move(IvStr[1], AesIv[0], SizeOf(TCnAESBuffer));

          EncryptAES192StreamCBC(Stream, Stream.Size, AESKey192, AesIv, ES);
          Result := True;
        end;
      ckeAES256:
        begin
          Move(Keys[0], AESKey256[0], SizeOf(TCnAESKey256));
          Move(IvStr[1], AesIv[0], SizeOf(TCnAESBuffer));

          EncryptAES256StreamCBC(Stream, Stream.Size, AESKey256, AesIv, ES);
          Result := True;
        end;
      ckeSM4:
        begin
          Move(Keys[0], Sm4Key[0], SizeOf(TCnSM4Key));
          Move(IvStr[1], Sm4Iv[0], SizeOf(TCnSM4Iv));

          SM4EncryptStreamCBC(Stream, Stream.Size, Sm4Key, Sm4Iv, ES);
          Result := True;
        end;
    end;
  finally
    if ES.Size > 0 then
    begin
      // ES д�� Stream
      Stream.Size := 0;
      Stream.Position := 0;
      ES.SaveToStream(Stream);
      Stream.Position := 0;
    end;
    ES.Free;
  end;
end;

// �ü����㷨�������㡢��ʼ���������������⿪ Base64 ����� S����д�� Stream ��
function DecryptPemString(const S, M1, M2, HexIv, Password: string; Stream: TMemoryStream;
  KeyHash: TCnKeyHashMethod): Boolean;
var
  DS: TMemoryStream;
  Keys: array[0..31] of Byte; // ��� Key Ҳֻ�� 32 �ֽ�
  AESKey128: TCnAESKey128;
  AESKey192: TCnAESKey192;
  AESKey256: TCnAESKey256;
  IvStr: AnsiString;
  AesIv: TCnAESBuffer;
  DesKey: TCnDESKey;
  Des3Key: TCn3DESKey;
  DesIv: TCnDESIv;
  Sm4Key: TCnSM4Key;
  Sm4Iv: TCnSM4Iv;
begin
  Result := False;
  DS := nil;

  if (M1 = '') or (M2 = '') or (HexIv = '') or (Password = '') then
    Exit;

  try
    DS := TMemoryStream.Create;
    if ECN_BASE64_OK <> Base64Decode(S, DS, False) then
      Exit;

    DS.Position := 0;
    SetLength(IvStr, HexToData(HexIv));
    if Length(IvStr) > 0 then
      HexToData(HexIv, @IvStr[1]);

    // �������������� Salt �Լ� Hash �㷨������ӽ��ܵ� Key
    FillChar(Keys[0], SizeOf(Keys), 0);
    if KeyHash = ckhMd5 then
    begin
      if not CnGetDeriveKey(AnsiString(Password), IvStr, @Keys[0], SizeOf(Keys)) then
        Exit;
    end
    else if KeyHash = ckhSha256 then
    begin
      if not CnGetDeriveKey(AnsiString(Password), IvStr, @Keys[0], SizeOf(Keys), ckdSha256) then
        Exit;
    end
    else
      Exit;

    // DS �������ģ�Ҫ�⵽ Stream ��
    if (M1 = ENC_TYPE_AES256) and (M2 = ENC_BLOCK_CBC) then
    begin
      // �⿪ AES-256-CBC ���ܵ�����
      Move(Keys[0], AESKey256[0], SizeOf(TCnAESKey256));
      Move(IvStr[1], AesIv[0], Min(SizeOf(TCnAESBuffer), Length(IvStr)));

      DecryptAES256StreamCBC(DS, DS.Size, AESKey256, AesIv, Stream);
      RemovePKCS7Padding(Stream);
      Result := True;
    end
    else if (M1 = ENC_TYPE_AES192) and (M2 = ENC_BLOCK_CBC) then
    begin
      // �⿪ AES-192-CBC ���ܵ�����
      Move(Keys[0], AESKey192[0], SizeOf(TCnAESKey192));
      Move(IvStr[1], AesIv[0], Min(SizeOf(TCnAESBuffer), Length(IvStr)));

      DecryptAES192StreamCBC(DS, DS.Size, AESKey192, AesIv, Stream);
      RemovePKCS7Padding(Stream);
      Result := True;
    end
    else if (M1 = ENC_TYPE_AES128) and (M2 = ENC_BLOCK_CBC) then
    begin
      // �⿪ AES-128-CBC ���ܵ����ģ��� D5 ��ò�ƿ��������������� Bug ���³� AV��
      Move(Keys[0], AESKey128[0], SizeOf(TCnAESKey128));
      Move(IvStr[1], AesIv[0], Min(SizeOf(TCnAESBuffer), Length(IvStr)));

      DecryptAES128StreamCBC(DS, DS.Size, AESKey128, AesIv, Stream);
      RemovePKCS7Padding(Stream);
      Result := True;
    end
    else if (M1 = ENC_TYPE_DES) and (M2 = ENC_BLOCK_CBC) then
    begin
      // �⿪ DES-CBC ���ܵ�����
      Move(Keys[0], DesKey[0], SizeOf(TCnDESKey));
      Move(IvStr[1], DesIv[0], Min(SizeOf(TCnDESIv), Length(IvStr)));

      DESDecryptStreamCBC(DS, DS.Size, DesKey, DesIv, Stream);
      RemovePKCS7Padding(Stream);
      Result := True;
    end
    else if (M1 = ENC_TYPE_3DES) and (M2 = ENC_BLOCK_CBC) then
    begin
      // �⿪ 3DES-CBC ���ܵ�����
      Move(Keys[0], Des3Key[0], SizeOf(TCn3DESKey));
      Move(IvStr[1], DesIv[0], Min(SizeOf(TCn3DESIv), Length(IvStr)));

      TripleDESDecryptStreamCBC(DS, DS.Size, Des3Key, DesIv, Stream);
      RemovePKCS7Padding(Stream);
      Result := True;
    end
    else if (M1 = ENC_TYPE_SM4) and (M2 = ENC_BLOCK_CBC) then
    begin
      // �⿪ SM4-CBC ���ܵ�����
      Move(Keys[0], Sm4Key[0], SizeOf(TCnSM4Key));
      Move(IvStr[1], Sm4Iv[0], Min(SizeOf(TCnSM4Iv), Length(IvStr)));

      SM4DecryptStreamCBC(DS, DS.Size, Sm4Key, Sm4Iv, Stream);
      RemovePKCS7Padding(Stream);
      Result := True;
    end
  finally
    DS.Free;
  end;
end;

function LoadPemStreamToMemory(Stream: TStream; const ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream; const Password: string; KeyHashMethod: TCnKeyHashMethod): Boolean;
var
  I, J, HeadIndex, TailIndex: Integer;
  S, L1, L2, M1, M2, M3: string;
  Sl: TStringList;
begin
  Result := False;

  if (Stream <> nil) and (Stream.Size > 0) and (ExpectHead <> '') and (ExpectTail <> '') then
  begin
    Sl := TStringList.Create;
    try
      Sl.LoadFromStream(Stream);
      if Sl.Count > 2 then
      begin
        HeadIndex := -1;
        for I := 0 to Sl.Count - 1 do
        begin
          if Trim(Sl[I]) = ExpectHead then
          begin
            HeadIndex := I;
            Break;
          end;
        end;

        if HeadIndex < 0 then
          Exit;

        if HeadIndex > 0 then
          for I := 0 to HeadIndex - 1 do
            Sl.Delete(0);

        // �ҵ�ͷ�ˣ�������β��

        TailIndex := -1;
        for I := 0 to Sl.Count - 1 do
        begin
          if Trim(Sl[I]) = ExpectTail then
          begin
            TailIndex := I;
            Break;
          end;
        end;

        if TailIndex > 0 then // �ҵ���β�ͣ�ɾ��β�ͺ���Ķ���
        begin
          if TailIndex < Sl.Count - 1 then
            for I := Sl.Count - 1 downto TailIndex + 1 do
              Sl.Delete(Sl.Count - 1);
        end
        else
          Exit;

        if Sl.Count < 2 then  // û���ݣ��˳�
          Exit;

        // ͷβ��֤ͨ������ǰ�����ж��Ƿ����
        L1 := Sl[1];
        if Pos(ENC_HEAD_PROCTYPE, L1) = 1 then // �Ǽ��ܵ�
        begin
          Delete(L1, 1, Length(ENC_HEAD_PROCTYPE));
          I := Pos(',', L1);
          if I <= 1 then
            Exit;

          if Trim(Copy(L1, 1, I - 1)) <> ENC_HEAD_PROCTYPE_NUM then
            Exit;

          if Trim(Copy(L1, I + 1, MaxInt)) <> ENC_HEAD_ENCRYPTED then
            Exit;

          // ProcType: 4,ENCRYPTED �ж�ͨ��

          L2 := Sl[2];
          if Pos(ENC_HEAD_DEK, L2) <> 1 then
            Exit;

          Delete(L2, 1, Length(ENC_HEAD_DEK));
          I := Pos(',', L2);
          if I <= 1 then
            Exit;

          M1 := Trim(Copy(L2, 1, I - 1)); // �õ� AES256-CBC ����
          M3 := UpperCase(Trim(Copy(L2, I + 1, MaxInt)));  // �õ�����ʱʹ�õĳ�ʼ������
          I := Pos('-', M1);
          if I <= 1 then
            Exit;
          J := Pos('-', Copy(M1, I + 1, MaxInt));
          if J > 0 then
            I := I + J; // AES-256-CBC

          M2 := UpperCase(Trim(Copy(M1, I + 1, MaxInt)));  // �õ���ģʽ���� ECB �� CBC ��
          M1 := UpperCase(Trim(Copy(M1, 1, I - 1)));       // �õ������㷨���� DES �� AES ��

          // ͷβ��������ȫɾ��
          Sl.Delete(Sl.Count - 1);
          Sl.Delete(0);
          Sl.Delete(0);
          Sl.Delete(0);

          S := '';
          for I := 0 to Sl.Count - 1 do
            S := S + Sl[I];

          S := Trim(S);

          Result := DecryptPemString(S, M1, M2, M3, Password, MemoryStream, KeyHashMethod);
        end
        else // δ���ܵģ�ƴ�ճ� Base64 �����
        begin
          Sl.Delete(Sl.Count - 1);
          Sl.Delete(0);
          S := '';
          for I := 0 to Sl.Count - 1 do
            S := S + Sl[I];

          S := Trim(S);

          // To De Base64 S
          MemoryStream.Clear;
          Result := (ECN_BASE64_OK = Base64Decode(S, MemoryStream, False));
        end;
      end;
    finally
      Sl.Free;
    end;
  end;
end;

function LoadPemFileToMemory(const FileName, ExpectHead, ExpectTail: string;
  MemoryStream: TMemoryStream; const Password: string; KeyHashMethod: TCnKeyHashMethod): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadPemStreamToMemory(Stream, ExpectHead, ExpectTail, MemoryStream, Password, KeyHashMethod);
  finally
    Stream.Free;
  end;
end;

procedure SplitStringToList(const S: string; List: TStrings);
const
  LINE_WIDTH = 64;
var
  C, R: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  if S <> '' then
  begin
    R := S;
    while R <> '' do
    begin
      C := Copy(R, 1, LINE_WIDTH);
      Delete(R, 1, LINE_WIDTH);
      List.Add(C);
    end;
  end;
end;

function SaveMemoryToPemFile(const FileName, Head, Tail: string;
  MemoryStream: TMemoryStream; KeyEncryptMethod: TCnKeyEncryptMethod;
  KeyHashMethod: TCnKeyHashMethod; const Password: string; Append: Boolean): Boolean;
var
  S, EH: string;
  List, Sl: TStringList;
begin
  Result := False;
  if (MemoryStream <> nil) and (MemoryStream.Size <> 0) then
  begin
    MemoryStream.Position := 0;

    if (KeyEncryptMethod <> ckeNone) and (Password <> '') then
    begin
      // �� MemoryStream ����
      AddPKCS7Padding(MemoryStream, ENC_TYPE_BLOCK_SIZE[KeyEncryptMethod]);

      // �ټ���
      if not EncryptPemStream(KeyHashMethod, KeyEncryptMethod, MemoryStream, Password, EH) then
        Exit;
    end;

    if ECN_BASE64_OK = Base64Encode(MemoryStream, S) then
    begin
      List := TStringList.Create;
      try
        SplitStringToList(S, List);

        List.Insert(0, Head);  // ��ͨͷ
        if EH <> '' then       // ����ͷ
          List.Insert(1, EH);
        List.Add(Tail);        // ��ͨβ

        if Append and FileExists(FileName) then
        begin
          Sl := TStringList.Create;
          try
            Sl.LoadFromFile(FileName);
            Sl.AddStrings(List);
            Sl.SaveToFile(FileName);
          finally
            Sl.Free;
          end;
        end
        else
          List.SaveToFile(FileName);

        Result := True;
      finally
        List.Free;
      end;
    end;
  end;
end;

function SaveMemoryToPemStream(Stream: TStream; const Head, Tail: string;
  MemoryStream: TMemoryStream; KeyEncryptMethod: TCnKeyEncryptMethod;
  KeyHashMethod: TCnKeyHashMethod; const Password: string; Append: Boolean): Boolean;
var
  S, EH: string;
  List: TStringList;
begin
  Result := False;
  if (MemoryStream <> nil) and (MemoryStream.Size <> 0) then
  begin
    MemoryStream.Position := 0;

    if (KeyEncryptMethod <> ckeNone) and (Password <> '') then
    begin
      // �� MemoryStream ����
      AddPKCS7Padding(MemoryStream, ENC_TYPE_BLOCK_SIZE[KeyEncryptMethod]);

      // �ټ���
      if not EncryptPemStream(KeyHashMethod, KeyEncryptMethod, MemoryStream, Password, EH) then
        Exit;
    end;

    if ECN_BASE64_OK = Base64Encode(MemoryStream, S) then
    begin
      List := TStringList.Create;
      try
        SplitStringToList(S, List);

        List.Insert(0, Head);  // ��ͨͷ
        if EH <> '' then       // ����ͷ
          List.Insert(1, EH);
        List.Add(Tail);        // ��ͨβ

        if not Append then
          Stream.Size := 0;

        List.SaveToStream(Stream);

        Result := True;
      finally
        List.Free;
      end;
    end;
  end;
end;

end.
