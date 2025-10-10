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

unit CnAEAD;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�����ԳƼӽ����㷨�� AEAD ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע��AEAD �ǹ���������֤���ܵļ�ơ���������Կ�������������ʼ�������ȶ�
*           ���ݽ��м�����������֤���ݣ�����ʱ�����֤����ͨ������ʧ�ܡ�
*
*           Ŀǰ����Ԫʵ���� GHash128��Ҳ�� GMAC���Լ� CMAC������������ʵ����
*           AES128/192/256/SM4 �� GCM/CCM���� CCM ��� CMAC �͵����� CMAC ����ͬ��
*
*           GCM �ο��ĵ���The Galois/Counter Mode of Operation (GCM)���Լ�
*           ��NIST Special Publication 800-38D���Լ� RFC 8998 ����������
*
*           CMAC �ο��ĵ� NIST Special Publication 800-38B:
*           ��Recommendation for Block Cipher Modes of Operation:
*           The CMAC Mode for Authentication�� �Լ� RFC 4993 ����������(AES-128)
*
*           CCM �ο��ĵ� NIST Special Publication 800-38C:
*          ��Recommendation for Block Cipher Modes of Operation:
*           The CCM Mode for Authentication and Confidentiality��
*           �Լ� RFC 3610 ����������(AES-128)��RFC 8998 ���������� SM4-CCM
*
*           ע�� CCM �����������ڵĲ�����ժҪ���� CCM_M_LEN �����ĳ��ȵ��ֽڳ��� CCM_L_LEN
*           NIST 800-38C �������� 4��8��RFC 3610 �������� 8��2��RFC 8998 �� 16����
*           �����ͬ�˵���������ͬ�����޷�ͨ�� CCM ��ȷ�ӽ��ܡ�
*
*           ���䣺Java �������� AES/GCM/NoPadding �ļ��ܷ�ʽ����ʹ�� AES256-GCM��
*           ����� 16 �ֽڵ� Tag ƴ�����ĺ�һ�����
*
*           ����Ԫ�е� Nonce��Iv ��ȻҲ�г�ʼ�����������ڲ�������ֱ�Ӳ���ض�ʹ�� 16 �ֽڣ�
*           ���Ǹ��ݳ���ֱ�� 12 �ֽڻ� GHash �� 16 �ֽں�ȡǰ 12 �ֽڣ���ƴ����
*           �ֽڵļ������������Ϊ��ͳ Iv ʹ�á�
*
*           ע���ֽڴ�ת��Ϊ������ʱ�൱�ڴ�˱�﷨����Խ����±�ָ��Խ��λ�ĵ�ַ
*
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWinXP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2024.05.09 V1.3
*               �������� AES/SM4 GCM ��ʮ�����Ƶļӽ��ܺ���
*           2022.07.31 V1.2
*               ���� ChaCha20-Poly1305 �� XChaCha20-Poly1305 �㷨
*           2022.10.22 V1.1
*               ���� AES192/256 �� Key ���ܱ�����ضϵ����⣬������ Tag ƴ�������ĺ�Ĵ���
*           2022.07.27 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnPoly1305, CnNative;

const
  CN_AEAD_BLOCK  = 16;
  {* GHASH/GCM/CMAC �Լ� AES/SM4 �ȵķ��鳤�ȣ���Ϊ 16 �ֽ�}

  CN_GCM_NONCE_LENGTH = 12;
  {* 12 �ֽڵ� Nonce �������ƴ�������� Iv}

  CN_CCM_M_LEN = 8;
  {* CCM ��֤ Tag Ĭ�ϳ� 12 �ֽڣ�ȡֵ��Χ 4 �� 16��ʵ�ʴ�ŵ��� (M - 2) / 2}

  CN_CCM_L_LEN = 2;
  {* ���ĳ�����ռ���ֽ�����ȡֵ��Χ 2 �� 8��8 ���� Int64��ʵ�ʴ�ŵ��� L - 1}

  CN_CCM_NONCE = 15 - CN_CCM_L_LEN;
  {* CCM �� Nonce ����}

type
  TCn128BitsBuffer = array[0..CN_AEAD_BLOCK - 1] of Byte;
  {* AEAD ������ 128 λ����ķֿ�}

  TCnGHash128Key = array[0..CN_AEAD_BLOCK - 1] of Byte;
  {* GHash128 ����Կ}

  TCnGHash128Tag    = array[0..CN_AEAD_BLOCK - 1] of Byte;
  {* GHash128 �ļ�����}

  TCnGHash128Context = packed record
  {* ���ڶ�ηֿ����� GHash128 �����Ľṹ}
    HashKey:     TCn128BitsBuffer;
    State:       TCn128BitsBuffer;
    AADByteLen:  Integer;
    DataByteLen: Integer;
  end;

  TCnGCM128Key = array[0..CN_AEAD_BLOCK - 1] of Byte;
  {* GCM ģʽ����Կ���ڲ������� AES ���� SM4 ��Ϊ 16 �ֽ�}

  TCnGCM128Tag    = array[0..CN_AEAD_BLOCK - 1] of Byte;
  {* GCM �ļ�����}

  TCnCMAC128Key = array[0..CN_AEAD_BLOCK - 1] of Byte;
  {* CMAC ģʽ����Կ���ڲ������� AES ���� SM4 ��Ϊ 16 �ֽ�}

  TCnCMAC128Tag    = array[0..CN_AEAD_BLOCK - 1] of Byte;
  {* CMAC �ļ�����}

  TCnCCM128Tag = array[0..CN_CCM_M_LEN - 1] of Byte;
  {* CCM �ļ�����}

procedure GMulBlock128(var X: TCn128BitsBuffer; var Y: TCn128BitsBuffer; var R: TCn128BitsBuffer);
{* ʵ�� GHash �е�٤�޻��� (2^128) �ϵĿ�˷���������������ͨ����Ҳ���Ͻ����ɡ�
   ע�� 2 ����������˷���ļӷ���ģ 2 ��Ҳ�������Ҳ��ͬ��ģ 2 ������
   ͬʱ��Ҫģһ��ģ����ʽ GHASH_POLY�����еĵ��μ�ͬ��Ҳ�����

   ������
     var X: TCn128BitsBuffer              - ����һ
     var Y: TCn128BitsBuffer              - ������
     var R: TCn128BitsBuffer              - ��

   ����ֵ�����ޣ�
}

procedure GHash128(var HashKey: TCnGHash128Key; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer; var OutTag: TCnGHash128Tag);
{* ��ָ�� HashKey �븽������ AAD����һ�����ݽ��� GHash ����õ� Tag ժҪ��
   ��Ӧ�ĵ��е� GHash(H, A C)��

   ������
     var HashKey: TCnGHash128Key          - ���ڼ������Կ
     Data: Pointer                        - ����������ݿ��ַ
     DataByteLength: Integer              - ����������ݿ��ֽڳ���
     AAD: Pointer                         - ������ĸ������ݿ��ַ
     AADByteLength: Integer               - ������ĸ������ݿ��ֽڳ���
     var OutTag: TCnGHash128Tag           - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

function GHash128Bytes(var HashKey: TCnGHash128Key; Data: TBytes; AAD: TBytes): TCnGHash128Tag;
{* �ֽ����鷽ʽ���� GHash ���㣬�ڲ����� GHash128��

   ������
     var HashKey: TCnGHash128Key          - ���ڼ������Կ
     Data: TBytes                         - ��������ֽ�����
     AAD: TBytes                          - ������ĸ��������ֽ�����

   ����ֵ��TCnGHash128Tag                 - ���ؼ����� Tag
}

// �����������������ⲿ���������ݽ�����ɢ�� GHash128 ���㣬GHash128Update �ɶ�α�����
// ע�� GHash128Update �� Data ��Ҫ���������飬�粻���飬ĩβ�Ჹ 0 ���㣬
// �����������������Ӵպ������������ŵ���һ�ִ��������

procedure GHash128Start(var Ctx: TCnGHash128Context; var HashKey: TCnGHash128Key;
  AAD: Pointer; AADByteLength: Integer);
{* ��ʼ GHash128 �ĵ�һ������ʼ����ȫ������� AAD ���ݡ�

   ������
     var Ctx: TCnGHash128Context          - ����ʼ���������Ľṹ
     var HashKey: TCnGHash128Key          - ���ڼ������Կ
     AAD: Pointer                         - ������ĸ������ݿ��ַ
     AADByteLength: Integer               - ������ĸ������ݿ��ֽڳ���

   ����ֵ�����ޣ�
}

procedure GHash128Update(var Ctx: TCnGHash128Context; Data: Pointer; DataByteLength: Integer);
{* ��һ�����ݽ��� GHash128���������㳤�Ⱦ���¼�� Ctx �У����Զ�ε��á�
   ע����Ҫ���������飬�粻���飬ĩβ�Ჹ 0 ��ɱ��μ��㣬�����������������Ӵպ������������ŵ���һ�ִ�����ټ��㡣

   ������
     var Ctx: TCnGHash128Context          - �����Ľṹ
     Data: Pointer                        - ����������ݿ��ַ
     DataByteLength: Integer              - ����������ݿ��ֽڳ���

   ����ֵ�����ޣ�
}

procedure GHash128Finish(var Ctx: TCnGHash128Context; var Output: TCnGHash128Tag);
{* GHash128 ���������㳤�ȣ������ؽ����

   ������
     var Ctx: TCnGHash128Context          - �����Ľṹ
     var Output: TCnGHash128Tag           - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

// ======================= AES/SM4-GCM �ֽ�������ܺ��� ========================

function AES128GCMEncryptBytes(Key: TBytes; Iv: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-128-GCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES192GCMEncryptBytes(Key: TBytes; Iv: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-192-GCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES256GCMEncryptBytes(Key: TBytes; Iv: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-256-GCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function SM4GCMEncryptBytes(Key: TBytes; Iv: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� SM4-GCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

// ======================== AES/SM4-GCM ���ݿ���ܺ��� =========================

procedure AES128GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-128-GCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

procedure AES192GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-192-GCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

procedure AES256GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-256-GCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

procedure SM4GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� SM4-GCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnGCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

procedure AESGCMNoPaddingEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer;
  NonceByteLength: Integer; PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; OutEnData: Pointer);
{* ���� Java/Kotlin/OpenSSL �ȹ淶�� AES/GCM/NoPadding ���ܺ������ڲ�ʹ�� AES256��
   ���� Nonce ����ʼ��������Tag ��ֱ��ƴ�����ĺ����Ҫ�� OutEnData ��ָ������� PlainByteLength �������� 16 �ֽڡ�

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ

   ����ֵ�����ޣ�
}

// ======================= AES/SM4-GCM �ֽ�������ܺ��� ========================

function AES128GCMDecryptBytes(Key: TBytes; Iv: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-128-GCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES192GCMDecryptBytes(Key: TBytes; Iv: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-192-GCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES256GCMDecryptBytes(Key: TBytes; Iv: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-256-GCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function SM4GCMDecryptBytes(Key: TBytes; Iv: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnGCM128Tag): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� SM4-GCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

// ======================== AES/SM4-GCM ���ݿ���ܺ��� =========================

function AES128GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-128-GCM ���ܲ���֤��
   �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

function AES192GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-192-GCM ���ܲ���֤��
   �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

function AES256GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-256-GCM ���ܲ���֤��
   �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

function SM4GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� SM4-GCM ���ܲ���֤��
   �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnGCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

function AESGCMNoPaddingDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer;
  NonceByteLength: Integer; EnData: Pointer; EnByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; OutPlainData: Pointer): Boolean;
{* ���� Java/Kotlin/OpenSSL �ȹ淶�� AES/GCM/NoPadding ���ܺ������ڲ�ʹ�� AES256��
   ���� Nonce ����ʼ������������Ҫ�� Tag ��ֱ��ƴ�����ĺ�Ҳ���ǽ����ĺ� 16 �ֽڵ��� Tag ���н��ܺ�ıȶԡ�

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

// ======================= AES/SM4-CMAC �ֽ������Ӵպ��� =======================

function AES128CMAC128Bytes(Key: TBytes; Data: TBytes): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� AES-128-CMAC ���㣬���ؼ������ Tag��������Ϊ�ֽ����顣

   ������
     Key: TBytes                          - ��Կ�ֽ�����
     Data: TBytes                         - �����ֽ�����

   ����ֵ��TCnCMAC128Tag                  -
}

function AES192CMAC128Bytes(Key: TBytes; Data: TBytes): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� AES-192-CMAC ���㣬���ؼ������ Tag��������Ϊ�ֽ�����

   ������
     Key: TBytes                          - ��Կ�ֽ�����
     Data: TBytes                         - �����ֽ�����

   ����ֵ��TCnCMAC128Tag                  -
}

function AES256CMAC128Bytes(Key: TBytes; Data: TBytes): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� AES-256-CMAC ���㣬���ؼ������ Tag��������Ϊ�ֽ�����

   ������
     Key: TBytes                          - ��Կ�ֽ�����
     Data: TBytes                         -

   ����ֵ��TCnCMAC128Tag                  -
}

function SM4CMAC128Bytes(Key: TBytes; Data: TBytes): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� SM4-CMAC ���㣬���ؼ������ Tag��������Ϊ�ֽ�����

   ������
     Key: TBytes                          - ��Կ�ֽ�����
     Data: TBytes                         -

   ����ֵ��TCnCMAC128Tag                  -
}

// ======================== AES/SM4-CMAC ���ݿ��Ӵպ��� ========================

function AES128CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� AES-128-CMAC ���㣬���ؼ������ Tag��������Ϊ�ڴ�顣

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Data: Pointer                        - ����������ݿ��ַ
     DataByteLength: Integer              - ����������ݿ��ֽڳ���

   ����ֵ��TCnCMAC128Tag                  -
}

function AES192CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� AES-192-CMAC ���㣬���ؼ������ Tag��������Ϊ�ڴ�顣

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Data: Pointer                        - ����������ݿ��ַ
     DataByteLength: Integer              - ����������ݿ��ֽڳ���

   ����ֵ��TCnCMAC128Tag                  -
}

function AES256CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� AES-256-CMAC ���㣬���ؼ������ Tag��������Ϊ�ڴ�顣

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Data: Pointer                        - ����������ݿ��ַ
     DataByteLength: Integer              - ����������ݿ��ֽڳ���

   ����ֵ��TCnCMAC128Tag                  - ���ؼ����� Tag
}

function SM4CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
{* ��ָ���� Key �����ݽ��� SM4-CMAC ���㣬���ؼ������ Tag��������Ϊ�ڴ�顣

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Data: Pointer                        - ����������ݿ��ַ
     DataByteLength: Integer              - ����������ݿ��ֽڳ���

   ����ֵ��TCnCMAC128Tag                  - ���ؼ����� Tag
}

// ======================= AES/SM4-CCM �ֽ�������ܺ��� ========================

function AES128CCMEncryptBytes(Key: TBytes; Nonce: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-128-CCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES192CCMEncryptBytes(Key: TBytes; Nonce: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-192-CCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES256CCMEncryptBytes(Key: TBytes; Nonce: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-256-CCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function SM4CCMEncryptBytes(Key: TBytes; Nonce: TBytes; PlainData: TBytes; AAD: TBytes;
  var OutTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� SM4-CCM ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

// ======================= AES/SM4-CCM �ֽ�������ܺ��� ========================

function AES128CCMDecryptBytes(Key: TBytes; Nonce: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-128-CCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES192CCMDecryptBytes(Key: TBytes; Nonce: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-192-CCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function AES256CCMDecryptBytes(Key: TBytes; Nonce: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-256-CCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function SM4CCMDecryptBytes(Key: TBytes; Nonce: TBytes; EnData: TBytes; AAD: TBytes;
  var InTag: TCnCCM128Tag): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� SM4-CCM ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Nonce: TBytes                        - ��ʱ�����ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

// ======================== AES/SM4-CCM ���ݿ���ܺ��� =========================

procedure AES128CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-128-CCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

procedure AES192CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-192-CCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʼ���������ݿ��ַ
     NonceByteLength: Integer             - ��ʼ���������ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

procedure AES256CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-256-CCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

procedure SM4CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� SM4-CCM ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnCCM128Tag             - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

// ======================== AES/SM4-CCM ���ݿ���ܺ��� =========================

function AES128CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-128-CCM ���ܲ���֤��
  �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
  ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

function AES192CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-192-CCM ���ܲ���֤��
  �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
  ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

function AES256CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� AES-256-CCM ���ܲ���֤��
  �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
  ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

function SM4CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� SM4-CCM ���ܲ���֤��
  �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
  ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Nonce: Pointer                       - ��ʱ���ݿ��ַ
     NonceByteLength: Integer             - ��ʱ���ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnCCM128Tag              - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

// ======== ��װ�� AES|SM4/GCM ʮ�������ֽ�����ӽ��ܺ��������� Padding ========

function AESGCMEncryptToHex(Key: TBytes; Iv: TBytes; AD: TBytes; Input: TBytes): string;
{* ��װ�ĳ��ü��ܺ�����ʹ����Կ����ʼ���������������ݶ����Ľ��� AES-GCM ���ܲ�ת����ʮ�������ַ�����
   �㷨���� AES256��GCM ���� Padding����֤ Tag ƴ���ַ��������ڲ����ܽ���γ��������ġ�

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     AD: TBytes                           - ���������ֽ�����
     Input: TBytes                        - �����ܵ������ֽ�����

   ����ֵ��string                         - ����ʮ����������
}

function AESGCMDecryptFromHex(Key: TBytes; Iv: TBytes; AD: TBytes; const Input: string): TBytes;
{* ��װ�ĳ��ý��ܺ�����ʹ����Կ����ʼ���������������ݶ�ʮ���������Ľ��� AES-GCM ���ܲ���֤ Tag��
   �㷨���� AES256��GCM ���� Padding�����ؽ��ܺ�������ֽ����顣

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     AD: TBytes                           - ���������ֽ�����
     Input: string                        - �����ܵ�ʮ����������

   ����ֵ��TBytes                         - ���������ֽ�����
}

function SM4GCMEncryptToHex(Key: TBytes; Iv: TBytes; AD: TBytes; Input: TBytes): string;
{* ��װ�ĳ��ü��ܺ�����ʹ����Կ����ʼ���������������ݶ����Ľ��� SM4-GCM ���ܲ�ת����ʮ�������ַ�����
   �㷨���� SM4��GCM ���� Padding����֤ Tag ƴ���ַ��������ڲ����ܽ���γ��������ġ�

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     AD: TBytes                           - ���������ֽ�����
     Input: TBytes                        - �����ܵ������ֽ�����

   ����ֵ��string                         - ����ʮ����������
}

function SM4GCMDecryptFromHex(Key: TBytes; Iv: TBytes; AD: TBytes; const Input: string): TBytes;
{* ��װ�ĳ��ý��ܺ�����ʹ����Կ����ʼ���������������ݶ�ʮ���������Ľ��� SM4-GCM ���ܲ���֤ Tag��
   �㷨���� SM4��GCM ���� Padding�����ؽ��ܺ�������ֽ����顣

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     AD: TBytes                           - ���������ֽ�����
     const Input: string                  - �����ܵ�ʮ����������

   ����ֵ��TBytes                         - ���������ֽ�����
}

// =================== ChaCha20_Poly1305 ���ݿ�ӽ��ܺ��� ======================

procedure ChaCha20Poly1305Encrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnPoly1305Digest);
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� ChaCha20_Poly1305 ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��
   ���У�KeyByteLength Ҫ��Ϊ 32 �ֽڷ����ضϻ� 0��
   Iv Ҫ��Ϊ 12 �ֽڷ���Ҳ�ضϻ� 0����һ��˵���� 8 �ֽ�Ȼ��Ҫ����Ӹ� 4 �ֽڹ̶����ݣ�����δ���ã���
   ����� Tag Ϊ 16 �ֽڡ�

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnPoly1305Digest        - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

function ChaCha20Poly1305Decrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnPoly1305Digest): Boolean;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� ChaCha20_Poly1305 ���ܲ���֤��
  ���У�KeyByteLength Ҫ��Ϊ 32 �ֽڷ����ضϻ� 0��
  Iv Ҫ��Ϊ 12 �ֽڷ���Ҳ�ضϻ� 0����һ��˵���� 8 �ֽ�Ȼ��Ҫ����Ӹ� 4 �ֽڹ̶����ݣ�����δ���ã�
  �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
  ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False

   ������
     Key: Pointer                         - ��Կ���ݿ��ַ
     KeyByteLength: Integer               - ��Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnPoly1305Digest         - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

// ================== ChaCha20_Poly1305 �ֽ�����ӽ��ܺ��� =====================

function ChaCha20Poly1305EncryptBytes(Key: TBytes; Iv: TBytes; PlainData: TBytes;
  AAD: TBytes; var OutTag: TCnPoly1305Digest): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� ChaCha20_Poly1305 ���ܣ���������
  ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnPoly1305Digest        - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function ChaCha20Poly1305DecryptBytes(Key: TBytes; Iv: TBytes; EnData: TBytes;
  AAD: TBytes; var InTag: TCnPoly1305Digest): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� ChaCha20_Poly1305 ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnPoly1305Digest         - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

// =================== XChaCha20_Poly1305 ���ݿ�ӽ��ܺ��� =====================

procedure XChaCha20Poly1305Encrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer;
  IvByteLength: Integer; PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; OutEnData: Pointer; var OutTag: TCnPoly1305Digest);
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� XChaCha20_Poly1305 ���ܣ����������� OutEnData ��ָ�������С�
   OutEnData ��ָ�����򳤶�������Ϊ PlainByteLength�������������Խ������غ����
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������ OutTag �з�����֤���ݹ�������֤��
   ���У�KeyByteLength Ҫ��Ϊ 32 �ֽڷ����ضϻ� 0��
   Iv Ҫ��Ϊ 24 �ֽڷ���Ҳ�ضϻ� 0����һ��˵���� 8 �ֽ�Ȼ��Ҫ����Ӹ� 4 �ֽڹ̶����ݣ�����δ���ã���
   ����� Tag Ϊ 16 �ֽڡ�

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     PlainData: Pointer                   - �����ܵ��������ݿ��ַ
     PlainByteLength: Integer             - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutEnData: Pointer                   - ������ĵĴ�ŵ�ַ
     var OutTag: TCnPoly1305Digest        - ���ؼ����� Tag

   ����ֵ�����ޣ�
}

function XChaCha20Poly1305Decrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer;
  IvByteLength: Integer; EnData: Pointer; EnByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; OutPlainData: Pointer; var InTag: TCnPoly1305Digest): Boolean;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� XChaCha20_Poly1305 ���ܲ���֤��
   ���У�KeyByteLength Ҫ��Ϊ 32 �ֽڷ����ضϻ� 0��
   Iv Ҫ��Ϊ 24 �ֽڷ���Ҳ�ضϻ� 0����һ��˵���� 8 �ֽ�Ȼ��Ҫ����Ӹ� 4 �ֽڹ̶����ݣ�����δ���ã���
   �ɹ��򷵻� True �������ķ����� OutPlainData ��ָ�������У�
   ���ϲ�����Ϊ�ڴ�鲢ָ���ֽڳ��ȵ���ʽ������֤ InTag �Ƿ�Ϸ������Ϸ����� False��

   ������
     Key: Pointer                         - ������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������Կ���ݿ��ֽڳ���
     Iv: Pointer                          - ��ʼ���������ݿ��ַ
     IvByteLength: Integer                - ��ʼ���������ݿ��ֽڳ���
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     EnByteLength: Integer                - �����ܵ��������ݿ��ֽڳ���
     AAD: Pointer                         - �������ݿ��ַ
     AADByteLength: Integer               - �������ݿ��ֽڳ���
     OutPlainData: Pointer                - ������ĵĴ�ŵ�ַ
     var InTag: TCnPoly1305Digest         - ����֤�� Tag

   ����ֵ��Boolean                        - ���ؽ�������֤�Ƿ�ɹ�
}

// ================== XChaCha20_Poly1305 �ֽ�����ӽ��ܺ��� ====================

function XChaCha20Poly1305EncryptBytes(Key: TBytes; Iv: TBytes; PlainData: TBytes;
  AAD: TBytes; var OutTag: TCnPoly1305Digest): TBytes;
{* ʹ����Կ����ʱ���ݡ��������ݶ����Ľ��� XChaCha20_Poly1305 ���ܣ��������ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬���� OutTag �з�����֤���ݹ�������֤��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     PlainData: TBytes                    - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var OutTag: TCnPoly1305Digest        - ���ؼ����� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

function XChaCha20Poly1305DecryptBytes(Key: TBytes; Iv: TBytes; EnData: TBytes;
  AAD: TBytes; var InTag: TCnPoly1305Digest): TBytes;
{* ʹ����Կ����ʼ���������������ݶ����Ľ��� XChaCha20_Poly1305 ���ܲ���֤���ɹ��򷵻����ġ�
   ���ϲ����뷵��ֵ��Ϊ�ֽ����飬����֤ InTag �Ƿ�Ϸ������Ϸ����� nil��

   ������
     Key: TBytes                          - ������Կ�ֽ�����
     Iv: TBytes                           - ��ʼ�������ֽ�����
     EnData: TBytes                       - �����ܵ������ֽ�����
     AAD: TBytes                          - ���������ֽ�����
     var InTag: TCnPoly1305Digest         - ����֤�� Tag

   ����ֵ��TBytes                         - ���������ֽ�����
}

implementation

uses
  CnSM4, CnAES, CnChaCha20;

const
  GHASH_POLY: TCn128BitsBuffer = ($E1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  CMAC_POLY: TCn128BitsBuffer =  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $87);

type
  TAEADEncryptType = (aetAES128, aetAES192, aetAES256, aetSM4);
  {* ֧�ֵ����ֶԳƼ�������}

  TAEADContext = packed record
  case TAEADEncryptType of
    aetAES128: (ExpandedKey128: TCnAESExpandedKey128);
    aetAES192: (ExpandedKey192: TCnAESExpandedKey192);
    aetAES256: (ExpandedKey256: TCnAESExpandedKey256);
    aetSM4:    (SM4Context: TCnSM4Context);
  end;

// ע��˴��ж��ֽ��� Bit ��˳��Ҳ���ֽ��ڵĸ�λ�� 0
function AeadIsBitSet(AMem: Pointer; N: Integer): Boolean;
var
  P: PByte;
  A1, B1: Integer;
  V: Byte;
begin
  A1 := N div 8;
  B1 := 7 - (N mod 8);
  P := PByte(TCnIntAddress(AMem) + A1);

  V := Byte(1 shl B1);
  Result := (P^ and V) <> 0;
end;

procedure MoveMost128(const Source; var Dest; ByteLen: Integer);
begin
  MoveMost(Source, Dest, ByteLen, CN_AEAD_BLOCK);
end;

{
  GCM ʹ�õ� Galois(2^128) ���ڵĳ˷�

  һ����˵�����ֽڴ������� $FEDC���� BIT ˳���Ӧ���� FEDC��Ҳ���� 1111111011011100��
  GCM �У�����Ӧ������Ҳ��ͬ���⴮�����Ƶĳ����
  Ȼ�� LSB ָ�ұߵĵ�λ�ģ�MSB ָ��߸�λ�ģ�������λ���ϳ���ϰ�ߡ�
  �����������������⣺�±� 0 ��ָ���������ң�
  GCM �õ�ģ����ʽ�� 128,7,2,1,0�����㷨�еı���� E1000000000������˵���±����ұ߸�λ
  ��ˣ�127 �±���������λ���������������ĸ�λ�����ơ�
  ͬʱ��127 Ҳ��Ӧ�ŵ�ַ��λ��0 �ǵ�ַ��λ��������������ַ��λ������
}
procedure GMulBlock128(var X, Y: TCn128BitsBuffer; var R: TCn128BitsBuffer);
var
  I: Integer;
  Z, V: TCn128BitsBuffer;
  B: Boolean;
begin
  FillChar(Z[0], SizeOf(TCn128BitsBuffer), 0);
  Move(X[0], V[0], SizeOf(TCn128BitsBuffer));

  for I := 0 to 127 do
  begin
    if AeadIsBitSet(@Y[0], I) then
      MemoryXor(@Z[0], @V[0], SizeOf(TCn128BitsBuffer), @Z[0]);

    B := AeadIsBitSet(@V[0], 127); // �жϴ������ĸ�λ�Ƿ��� 1
    MemoryShiftRight(@V[0], nil, SizeOf(TCn128BitsBuffer), 1);
    if B then
      MemoryXor(@V[0], @GHASH_POLY[0], SizeOf(TCn128BitsBuffer), @V[0]);
  end;
  Move(Z[0], R[0], SizeOf(TCn128BitsBuffer));
end;

procedure GHash128(var HashKey: TCnGHash128Key; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer; var OutTag: TCnGHash128Tag);
var
  AL, DL: Integer;
  AL64, DL64: Int64;
  X, Y, H: TCn128BitsBuffer;
begin
  // �Ա� GHash(H, A, C)��Data �� C��AAD �� A��Key �� H
  // �� 16 �ֽڷ֣�C �� m �飬A �� n �飨ĩ�鶼���ܲ�������
  // ���� m + n ��������ݵ� GaloisMulBlock���ټ�һ��λ����

  FillChar(X[0], SizeOf(TCn128BitsBuffer), 0);  // ��ʼȫ 0
  Move(HashKey[0], H[0], SizeOf(TCn128BitsBuffer));

  AL := AADByteLength;
  DL := DataByteLength;
  if Data = nil then
    DL := 0;
  if AAD = nil then
    AL := 0;

  // ������ A
  while AL >= CN_AEAD_BLOCK do
  begin
    Move(AAD^, Y[0], CN_AEAD_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);  // һ�ּ������ٴη��� X

    AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK);
    Dec(AL, CN_AEAD_BLOCK);
  end;

  // ����� A������еĻ�
  if AL > 0 then
  begin
    FillChar(Y[0], SizeOf(TCn128BitsBuffer), 0);
    Move(AAD^, Y[0], AL);

    MemoryXor(@Y[0], @X[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // ������ C
  while DL >= CN_AEAD_BLOCK do
  begin
    Move(Data^, Y[0], CN_AEAD_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);  // һ�ּ������ٴη��� X

    Data := Pointer(TCnNativeUInt(Data) + CN_AEAD_BLOCK);
    Dec(DL, CN_AEAD_BLOCK);
  end;

  // ����� C������еĻ�
  if DL > 0 then
  begin
    FillChar(Y[0], SizeOf(TCn128BitsBuffer), 0);
    Move(Data^, Y[0], DL);

    MemoryXor(@Y[0], @X[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // �������һ�ֳ��ȣ�A �� C �����ֽ�ƴ������ƴ��Ҫ����������׼���Ķ�ϰ��Ҳ���� BigEndian
  FillChar(Y[0], SizeOf(TCn128BitsBuffer), 0);
  AL64 := Int64HostToNetwork(AADByteLength * 8);
  DL64 := Int64HostToNetwork(DataByteLength * 8);

  Move(AL64, Y[0], SizeOf(Int64));
  Move(DL64, Y[SizeOf(Int64)], SizeOf(Int64));

  MemoryXor(@Y[0], @X[0], SizeOf(TCn128BitsBuffer), @Y[0]);
  GMulBlock128(Y, H, X); // �ٳ�һ��

  Move(X[0], OutTag[0], SizeOf(TCnGHash128Tag));
end;

function GHash128Bytes(var HashKey: TCnGHash128Key; Data, AAD: TBytes): TCnGHash128Tag;
var
  C, A: Pointer;
begin
  if Data = nil then
    C := nil
  else
    C := @Data[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  GHash128(HashKey, C, Length(Data), A, Length(AAD), Result);
end;

procedure GHash128Start(var Ctx: TCnGHash128Context; var HashKey: TCnGHash128Key;
  AAD: Pointer; AADByteLength: Integer);
var
  Y: TCn128BitsBuffer;
begin
  FillChar(Ctx.State[0], SizeOf(TCn128BitsBuffer), 0);  // ��ʼȫ 0
  Move(HashKey[0], Ctx.HashKey[0], SizeOf(TCn128BitsBuffer));

  Ctx.DataByteLen := 0;
  Ctx.AADByteLen := AADByteLength;
  if AAD = nil then
    Ctx.AADByteLen := 0;

  // ������ A
  while AADByteLength >= CN_AEAD_BLOCK do
  begin
    Move(AAD^, Y[0], CN_AEAD_BLOCK);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);  // һ�ּ������ٴη��� Ctx.State

    AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK);
    Dec(AADByteLength, CN_AEAD_BLOCK);
  end;

  // ����� A������еĻ�
  if AADByteLength > 0 then
  begin
    FillChar(Y[0], SizeOf(TCn128BitsBuffer), 0);
    Move(AAD^, Y[0], AADByteLength);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);
  end;
end;

procedure GHash128Update(var Ctx: TCnGHash128Context; Data: Pointer; DataByteLength: Integer);
var
  Y: TCn128BitsBuffer;
begin
  if (Data = nil) or (DataByteLength <= 0) then
    Exit;

  Ctx.DataByteLen := Ctx.DataByteLen + DataByteLength;

  // ������ C
  while DataByteLength >= CN_AEAD_BLOCK do
  begin
    Move(Data^, Y[0], CN_AEAD_BLOCK);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);  // һ�ּ������ٴη��� Ctx.State

    Data := Pointer(TCnNativeUInt(Data) + CN_AEAD_BLOCK);
    Dec(DataByteLength, CN_AEAD_BLOCK);
  end;

  // ����� C������еĻ�
  if DataByteLength > 0 then
  begin
    FillChar(Y[0], SizeOf(TCn128BitsBuffer), 0);
    Move(Data^, Y[0], DataByteLength);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);
  end;
end;

procedure GHash128Finish(var Ctx: TCnGHash128Context; var Output: TCnGHash128Tag);
var
  Y: TCn128BitsBuffer;
  AL64, DL64: Int64;
begin
  // �������һ�ֳ��ȣ�A �� C �����ֽ�ƴ����
  FillChar(Y[0], SizeOf(TCn128BitsBuffer), 0);
  AL64 := Int64HostToNetwork(Ctx.AADByteLen * 8);
  DL64 := Int64HostToNetwork(Ctx.DataByteLen * 8);

  Move(AL64, Y[0], SizeOf(Int64));
  Move(DL64, Y[SizeOf(Int64)], SizeOf(Int64));

  MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TCn128BitsBuffer), @Y[0]);
  GMulBlock128(Y, Ctx.HashKey, Ctx.State); // �ٳ�һ�֣�

  Move(Ctx.State[0], Output[0], SizeOf(TCnGHash128Tag)); // ����� Output
end;

// ���ݶԳƼ����㷨���ͳ�ʼ��������Կ�ṹ��ע�ⲻ��Ҫ������Կ�ṹ
procedure AEADEncryptInit(var Context: TAEADContext; Key: Pointer;
  KeyByteLength: Integer; EncryptType: TAEADEncryptType);
var
  Key128: TCnAESKey128;
  Key192: TCnAESKey192;
  Key256: TCnAESKey256;
  SM4Key: TCnSM4Key;
begin
  FillChar(Context, SizeOf(TAEADContext), 0);

  case EncryptType of
    aetAES128:
      begin
        MoveMost(Key^, Key128[0], KeyByteLength, SizeOf(TCnAESKey128));
        ExpandAESKeyForEncryption128(Key128, Context.ExpandedKey128);
      end;
    aetAES192:
      begin
        MoveMost(Key^, Key192[0], KeyByteLength, SizeOf(TCnAESKey192));
        ExpandAESKeyForEncryption192(Key192, Context.ExpandedKey192);
      end;
    aetAES256:
      begin
        MoveMost(Key^, Key256[0], KeyByteLength, SizeOf(TCnAESKey256));
        ExpandAESKeyForEncryption256(Key256, Context.ExpandedKey256);
      end;
    aetSM4:
      begin
        MoveMost(Key^, SM4Key[0], KeyByteLength, SizeOf(TCnSM4Key));
        SM4SetKeyEnc(Context.SM4Context, @SM4Key[0]);
      end;
  end;
end;

// ���ݶԳƼ����㷨���ͼ���һ���飬���鴮�������Ǽ��ܽ����ע�ⲻ��Ҫ�����
procedure AEADEncryptBlock(var Context: TAEADContext; var InData, OutData: TCn128BitsBuffer;
  EncryptType: TAEADEncryptType);
begin
  case EncryptType of
    aetAES128: EncryptAES128(TCnAESBuffer(InData), Context.ExpandedKey128, TCnAESBuffer(OutData));
    aetAES192: EncryptAES192(TCnAESBuffer(InData), Context.ExpandedKey192, TCnAESBuffer(OutData));
    aetAES256: EncryptAES256(TCnAESBuffer(InData), Context.ExpandedKey256, TCnAESBuffer(OutData));
    aetSM4:    SM4OneRound(@(Context.SM4Context.Sk[0]), @InData[0], @OutData[0]);
  end;
end;

// ���� Key��Iv�����ĺ͸������ݣ����� GCM ������������֤���
procedure GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; EnData: Pointer; var OutTag: TCnGCM128Tag;
  EncryptType: TAEADEncryptType);
var
  H: TCnGHash128Key;
  Y, Y0: TCn128BitsBuffer; // Y ƴ���˼�����������
  Cnt, M: Cardinal;      // ������
  C: TCn128BitsBuffer;     // �����м����ݴ洢��
  AeadCtx: TAEADContext;
  GHashCtx: TCnGHash128Context;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Iv = nil then
    IvByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  AEADEncryptInit(AeadCtx, Key, KeyByteLength, EncryptType);

  // ���� Enc(Key, 128 �� 0)���õ� H
  FillChar(H[0], SizeOf(H), 0);
  AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(H), TCn128BitsBuffer(H), EncryptType);

  // ��ʼ�����������Ժ� Cnt ���������� Y �ĺ� 32 λ��
  if IvByteLength = CN_GCM_NONCE_LENGTH then
  begin
    Move(Iv^, Y[0], CN_GCM_NONCE_LENGTH);
    Cnt := 1;
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[CN_GCM_NONCE_LENGTH], SizeOf(M));
  end
  else
  begin
    GHash128(H, Iv, IvByteLength, nil, 0, TCnGHash128Tag(Y));
    Move(Y[CN_GCM_NONCE_LENGTH], Cnt, SizeOf(Cardinal));
    ReverseMemory(@Cnt, SizeOf(Cardinal));
  end;

  // �Ȱ��ʼ�� Y ֵ�ļ��ܽ���������
  AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(Y), TCn128BitsBuffer(Y0), EncryptType);

  // ��ʼ�� GHash
  GHash128Start(GHashCtx, H, AAD, AADByteLength);

  // ��ʼѭ����������
  while PlainByteLength >= CN_AEAD_BLOCK do
  begin
    // ���������������� Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[CN_GCM_NONCE_LENGTH], SizeOf(M));

    // �� Y ���� C ��ʱ�õ�����ļ��ܽ��
    AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(Y), C, EncryptType);

    // ���������C �õ�����Ľ����������
    MemoryXor(PlainData, @C[0], SizeOf(TCn128BitsBuffer), @C[0]);

    // ��������
    Move(C[0], EnData^, SizeOf(TCn128BitsBuffer));

    // C ���� GHash
    GHash128Update(GHashCtx, @C[0], SizeOf(TCn128BitsBuffer));

    // ׼����һ��
    PlainData := Pointer(TCnNativeUInt(PlainData) + CN_AEAD_BLOCK);
    EnData := Pointer(TCnNativeUInt(EnData) + CN_AEAD_BLOCK);
    Dec(PlainByteLength, CN_AEAD_BLOCK);
  end;

  if PlainByteLength > 0 then
  begin
    // ���������������� Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[CN_GCM_NONCE_LENGTH], SizeOf(M));

    // �� Y ���� C ��ʱ�õ�����ļ��ܽ��
    AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(Y), C, EncryptType);

    // ���������C �õ�����Ľ����������ֻ�� PlainByteLength
    MemoryXor(PlainData, @C[0], PlainByteLength, @C[0]);

    // �������ģ����
    Move(C[0], EnData^, PlainByteLength);

    // C ���� GHash
    GHash128Update(GHashCtx, @C[0], PlainByteLength);
  end;

  // ������� GHash �� Tag
  GHash128Finish(GHashCtx, TCnGHash128Tag(OutTag));

  // �ٺͿ�ʼ���������õ����� Tag
  MemoryXor(@OutTag[0], @Y0[0], SizeOf(TCnGHash128Tag), @OutTag[0]);
end;

// ���� Key��Iv�����ĺ͸������ݣ����� GCM ������������֤���
function GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; PlainData: Pointer; var InTag: TCnGCM128Tag;
  EncryptType: TAEADEncryptType): Boolean;
var
  H: TCnGHash128Key;
  Y, Y0: TCn128BitsBuffer; // Y ƴ���˼�����������
  Cnt, M: Cardinal;      // ������
  C: TCn128BitsBuffer;     // �����м����ݴ洢��
  AeadCtx: TAEADContext;
  GHashCtx: TCnGHash128Context;
  Tag: TCnGCM128Tag;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Iv = nil then
    IvByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  AEADEncryptInit(AeadCtx, Key, KeyByteLength, EncryptType);

  // ���� Enc(Key, 128 �� 0)���õ� H
  FillChar(H[0], SizeOf(H), 0);
  AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(H), TCn128BitsBuffer(H), EncryptType);

  // ��ʼ�����������Ժ� Cnt ���������� Y �ĺ� 32 λ��
  if IvByteLength = CN_GCM_NONCE_LENGTH then
  begin
    Move(Iv^, Y[0], CN_GCM_NONCE_LENGTH);
    Cnt := 1;
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[CN_GCM_NONCE_LENGTH], SizeOf(M));
  end
  else
  begin
    GHash128(H, Iv, IvByteLength, nil, 0, TCnGHash128Tag(Y));
    Move(Y[CN_GCM_NONCE_LENGTH], Cnt, SizeOf(Cardinal));
    ReverseMemory(@Cnt, SizeOf(Cardinal));
  end;

  // �Ȱ��ʼ�� Y ֵ�ļ��ܽ���������
  AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(Y), TCn128BitsBuffer(Y0), EncryptType);

  // ��ʼ�� GHash
  GHash128Start(GHashCtx, H, AAD, AADByteLength);

  // ��ʼѭ����������
  while EnByteLength >= CN_AEAD_BLOCK do
  begin
    // ���������������� Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[CN_GCM_NONCE_LENGTH], SizeOf(M));

    // �����Ƚ��� GHash
    GHash128Update(GHashCtx, EnData, SizeOf(TCn128BitsBuffer));

    // �� Y ���� C ��ʱ�õ�����ļ��ܽ��
    AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(Y), C, EncryptType);

    // ���������C �õ�����Ľ����������
    MemoryXor(EnData, @C[0], SizeOf(TCn128BitsBuffer), @C[0]);

    // ��������
    Move(C[0], PlainData^, SizeOf(TCn128BitsBuffer));

    // ׼����һ��
    EnData := Pointer(TCnNativeUInt(EnData) + CN_AEAD_BLOCK);
    PlainData := Pointer(TCnNativeUInt(PlainData) + CN_AEAD_BLOCK);
    Dec(EnByteLength, CN_AEAD_BLOCK);
  end;

  if EnByteLength > 0 then
  begin
    // ���������������� Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[CN_GCM_NONCE_LENGTH], SizeOf(M));

    // �����Ƚ��� GHash
    GHash128Update(GHashCtx, EnData, EnByteLength);

    // �� Y ���� C ��ʱ�õ�����ļ��ܽ��
    AEADEncryptBlock(AeadCtx, TCn128BitsBuffer(Y), C, EncryptType);

    // ���������C �õ�����Ľ����������ֻ�� EnByteLength
    MemoryXor(EnData, @C[0], EnByteLength, @C[0]);

    // �������ģ����
    Move(C[0], PlainData^, EnByteLength);
  end;

  // ������� GHash �� Tag
  GHash128Finish(GHashCtx, TCnGHash128Tag(Tag));

  // �ٺͿ�ʼ���������õ����� Tag
  MemoryXor(@Tag[0], @Y0[0], SizeOf(TCnGHash128Tag), @Tag[0]);

  Result := CompareMem(@Tag[0], @InTag[0], SizeOf(TCnGHash128Tag));
end;

function GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TCnGCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if PlainData = nil then
    P := nil
  else
    P := @PlainData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(PlainData) > 0 then
  begin
    SetLength(Result, Length(PlainData));
    GCMEncrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), @Result[0], OutTag, EncryptType);
  end
  else
  begin
    GCMEncrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), nil, OutTag, EncryptType);
  end;
end;

function GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TCnGCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if EnData = nil then
    P := nil
  else
    P := @EnData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(EnData) > 0 then
  begin
    SetLength(Result, Length(EnData));
    if not GCMDecrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), @Result[0], InTag, EncryptType) then // Tag �ȶ�ʧ���򷵻�
      SetLength(Result, 0);
  end
  else
  begin
    GCMDecrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), nil, InTag, EncryptType); // û���ģ���ʵ Tag �ȶԳɹ����û��
  end;
end;

function AES128GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetAES128);
end;

function AES192GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetAES192);
end;

function AES256GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetAES256);
end;

function SM4GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetSM4);
end;

procedure AES128GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES128);
end;

procedure AES192GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES192);
end;

procedure AES256GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES256);
end;

procedure SM4GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetSM4);
end;

procedure AESGCMNoPaddingEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer;
  NonceByteLength: Integer; PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; OutEnData: Pointer);
var
  OutTag: TCnGCM128Tag;
begin
  GCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES256);
  Move(OutTag[0], Pointer(TCnIntAddress(OutEnData) +PlainByteLength)^, SizeOf(TCnGCM128Tag));
end;

function AES128GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetAES128);
end;

function AES192GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetAES192);
end;

function AES256GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetAES256);
end;

function SM4GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TCnGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetSM4);
end;

function AES128GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES128);
end;

function AES192GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES192);
end;

function AES256GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES256);
end;

function SM4GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetSM4);
end;

function AESGCMNoPaddingDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer;
  NonceByteLength: Integer; EnData: Pointer; EnByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; OutPlainData: Pointer): Boolean;
var
  InTag: TCnGCM128Tag;
begin
  if EnByteLength < SizeOf(TCnGCM128Tag) then // ̫��˵��û Tag
  begin
    Result := False;
    Exit;
  end;

  Move(Pointer(TCnIntAddress(EnData) + EnByteLength - SizeOf(TCnGCM128Tag))^, InTag[0], SizeOf(TCnGCM128Tag));
  Result := GCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength - SizeOf(TCnGCM128Tag),
    AAD, AADByteLength, OutPlainData, InTag, aetAES256);
end;

procedure CMAC128(var Key: TCnCMAC128Key; Data: Pointer; DataByteLength: Integer;
  EncryptType: TAEADEncryptType; var OutTag: TCnCMAC128Tag);
var
  K1, K2: TCnCMAC128Key;
  L, X, Y: TCn128BitsBuffer;
  AeadCtx: TAEADContext;
  LastFull: Boolean;
begin
  AEADEncryptInit(AeadCtx, @Key[0], Length(Key), EncryptType);

  // ���� Enc(Key, 128 �� 0)���õ� L
  FillChar(L[0], SizeOf(L), 0);
  AEADEncryptBlock(AeadCtx, L, L, EncryptType);

  // ���� L ����������Կ
  MemoryShiftLeft(@L[0], @K1[0], SizeOf(TCnCMAC128Key), 1);
  if AeadIsBitSet(@L[0], 0) then
    MemoryXor(@K1[0], @CMAC_POLY[0], SizeOf(TCnCMAC128Key), @K1[0]);

  MemoryShiftLeft(@K1[0], @K2[0], SizeOf(TCnCMAC128Key), 1);
  if AeadIsBitSet(@K1[0], 0) then
    MemoryXor(@K2[0], @CMAC_POLY[0], SizeOf(TCnCMAC128Key), @K2[0]);

  // ��ʼ�ֿ���㣬ĩ��Ҫ���⴦��
  LastFull := (DataByteLength mod CN_AEAD_BLOCK) = 0;

  // ������ A
  FillChar(X[0], SizeOf(TCn128BitsBuffer), 0);
  while DataByteLength >= CN_AEAD_BLOCK do
  begin
    Move(Data^, L[0], CN_AEAD_BLOCK); // ���� L ��Ϊÿ��ԭʼ����
    if LastFull and (DataByteLength = CN_AEAD_BLOCK) then // ���һ������
    begin
      MemoryXor(@K1[0], @L[0], CN_AEAD_BLOCK, @L[0]);
      MemoryXor(@X[0], @L[0], CN_AEAD_BLOCK, @Y[0]);
      AEADEncryptBlock(AeadCtx, Y, TCn128BitsBuffer(OutTag), EncryptType); // ������� Tag
      Exit;
    end;

    MemoryXor(@L[0], @X[0], SizeOf(TCn128BitsBuffer), @Y[0]);
    AEADEncryptBlock(AeadCtx, Y, X, EncryptType); // һ�ּ������ٴη��� X

    Data := Pointer(TCnNativeUInt(Data) + CN_AEAD_BLOCK);
    Dec(DataByteLength, CN_AEAD_BLOCK);
  end;

  FillChar(L[0], SizeOf(TCn128BitsBuffer), 0);
  if DataByteLength > 0 then
  Move(Data^, L[0], DataByteLength);
  L[DataByteLength] := $80;         // ���һ������飬���� Padding

  MemoryXor(@K2[0], @L[0], CN_AEAD_BLOCK, @L[0]);
  MemoryXor(@X[0], @L[0], CN_AEAD_BLOCK, @Y[0]);
  AEADEncryptBlock(AeadCtx, Y, TCn128BitsBuffer(OutTag), EncryptType); // ������� Tag
end;

function CMAC128Bytes(Key, Data: TBytes; EncryptType: TAEADEncryptType): TCnCMAC128Tag;
var
  D: Pointer;
  Key128: TCnCMAC128Key;
begin
  if Data = nil then
    D := nil
  else
    D := @Data[0];

  MoveMost128(Key[0], Key128[0], Length(Key));
  CMAC128(Key128, D, Length(Data), EncryptType, Result);
end;

function AES128CMAC128Bytes(Key, Data: TBytes): TCnCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetAES128);
end;

function AES192CMAC128Bytes(Key, Data: TBytes): TCnCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetAES192);
end;

function AES256CMAC128Bytes(Key, Data: TBytes): TCnCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetAES256);
end;

function SM4CMAC128Bytes(Key, Data: TBytes): TCnCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetSM4);
end;

function AES128CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
var
  Key128: TCnCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetAES128, Result);
end;

function AES192CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
var
  Key128: TCnCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetAES192, Result);
end;

function AES256CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
var
  Key128: TCnCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetAES256, Result);
end;

function SM4CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCnCMAC128Tag;
var
  Key128: TCnCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetSM4, Result);
end;

procedure CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer;
  NonceByteLength: Integer; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer; EnData: Pointer; var OutTag: TCnCCM128Tag;
  EncryptType: TAEADEncryptType);
var
  CMacCtx: TAEADContext;
  CtrCtx: TAEADContext;
  B0: TCn128BitsBuffer;   // CMAC ��֤ʱ�� Iv
  CX: TCn128BitsBuffer;   // CMAC �ļ�������ŵ��м��
  A0: TCn128BitsBuffer;   // �������ݿ�ĵ�һ�飬�Լ������� CMAC �����е�ԭʼ���ݵ��м��
  S0: TCn128BitsBuffer;   // ��һ�����ܿ�������֤���������������
  SX: TCn128BitsBuffer;   // CTR �ļ�������ŵ��м��
  Ctr: TCn128BitsBuffer;  // CTR �ļ�����
  Cnt, T: Int64;
  P: PByte;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Nonce = nil then
    NonceByteLength := 0;
  if Data = nil then
    DataByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  FillChar(B0[0], SizeOf(TCn128BitsBuffer), 0);
  FillChar(Ctr[0], SizeOf(TCn128BitsBuffer), 0);

//   +----+-+-+-+-+-+-+-+-+    |L'(L) ����
//   | λ |7|6|5|4|3|2|1|0|    |Nonce �ĳ���
//   +----+-+-+-+-+-+-+-+-+
//   |    |0|A|  M' |  L' |
//   +----+-+-+-+-+-+-+-+-+

  B0[0] := 4 * (CN_CCM_M_LEN - 2) + CN_CCM_L_LEN - 1;
  if (AAD <> nil) and (AADByteLength > 0) then
    B0[0] := B0[0] + 64;   // B0 ��ĵ�һ���ֽ�׼���ã�A λ�� 1 ��ʾ�� AAD

  Ctr[0] := CN_CCM_L_LEN - 1;

  // ��� 15 - L �� Nonce
  MoveMost(Nonce^, B0[1], NonceByteLength, CN_CCM_NONCE);
  MoveMost(Nonce^, Ctr[1], NonceByteLength, CN_CCM_NONCE);

  // ���������ֽ�˳������ĳ��ȣ��ҴӸ�λ�ض��� CCM_L_LEN �ֽڣ������������ B0
  P := PByte(@T);
  Inc(P, SizeOf(Int64) - CN_CCM_L_LEN);  // �����佨�� P �� T �ĸ߼�λ�ĵ�ַ��ϵ���������ʹ��

  T := Int64HostToNetwork(DataByteLength);
  Move(P^, B0[CN_CCM_NONCE + 1], CN_CCM_L_LEN);

  // ��ʼ�� CMAC/CTR �� Key �ȣ�׼���� CMAC/CTR
  AEADEncryptInit(CMacCtx, Key, KeyByteLength, EncryptType);
  AEADEncryptInit(CtrCtx, Key, KeyByteLength, EncryptType);

  // Ctr �ĺ�˸��ֽ��Ǽ��������ֳ�ʼ��Ϊ 0�����Ҽ��� S0 ��Ϊ��֤�ֶ�֮һ
  Cnt := 0;
  AEADEncryptBlock(CtrCtx, Ctr, S0, EncryptType);

  // CMAC ���� B0���м����� CX��Ҳ���� RFC �е� CBC Iv Out
  AEADEncryptBlock(CMacCtx, B0, CX, EncryptType);

  // �� AAD �Ļ������� A0
  if (B0[0] and $40 <> 0) then
  begin
    FillChar(A0[0], CN_AEAD_BLOCK, 0);

    if AADByteLength < $1000 - $100 then
    begin
      PCnWord(@A0[0])^ := Int16HostToNetwork(SmallInt(AADByteLength)); // �����ֽ�

      // ��һ��׼���ã������������������Լ������������
      MoveMost(AAD^, A0[2], AADByteLength, CN_AEAD_BLOCK - 2);

      // ��һ��� CX ����� CMAC ֮������Ż� CX
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // ����׼����������
      AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK - 2);
      Dec(AADByteLength, CN_AEAD_BLOCK - 2);
    end
    else
    begin
      // A0[0] ǰ���ֽ�׼����
      A0[0] := $FF;
      A0[1] := $FE;
      PCardinal(@A0[2])^ := Int32HostToNetwork(AADByteLength); // �����ֽ�

      // ��һ��׼���ã������������������Լ������������
      MoveMost(AAD^, A0[6], AADByteLength, CN_AEAD_BLOCK - 6);

      // ��һ��� CX ����� CMAC ֮������Ż� CX
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // ����׼����������
      AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK - 6);
      Dec(AADByteLength, CN_AEAD_BLOCK - 6);
    end;

    // ����������Ļ���AADByteLength ��ʱС�ڵ��� 0��������
    while AADByteLength >= CN_AEAD_BLOCK do
    begin
      Move(AAD^, A0[0], CN_AEAD_BLOCK);

      // �����飨Ҳ���������һ�飩�� CX ����� CMAC ֮������Ż� CX
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // ����׼����������
      AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK);
      Dec(AADByteLength, CN_AEAD_BLOCK);
    end;

    if AADByteLength > 0 then // ����ʣ��ʱ���ٶ�һ��
    begin
      FillChar(A0[0], CN_AEAD_BLOCK, 0);
      Move(AAD^, A0[0], AADByteLength);

      // CMAC ���һ��
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);
    end;
  end;

  // ������ AAD �� CMAC ֵ����ʼ�� Data �ģ������� A0
  // ���ҿ�ʼ���ܿ�
  while DataByteLength >= CN_AEAD_BLOCK do
  begin
    Move(Data^, A0[0], CN_AEAD_BLOCK); // ���ķ� A0

    // ��������һ�����ɼ��ܿ�
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CN_CCM_NONCE + 1], CN_CCM_L_LEN);

    // �õ�����ļ��ܽ������ SX ��
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // �����������õ�����
    MemoryXor(@SX[0], @A0[0], CN_AEAD_BLOCK, EnData);

    // �����飨Ҳ���������һ�飩�� CX ����� CMAC ֮������Ż� CX
    MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

    // ����׼����������
    Data := Pointer(TCnNativeUInt(Data) + CN_AEAD_BLOCK);
    EnData := Pointer(TCnNativeUInt(EnData) + CN_AEAD_BLOCK);
    Dec(DataByteLength, CN_AEAD_BLOCK);
  end;

  if DataByteLength > 0 then // ����ʣ��ʱ���ٶ�һ��
  begin
    FillChar(A0[0], CN_AEAD_BLOCK, 0);
    Move(Data^, A0[0], DataByteLength);

    // ��������һ�����ɼ��ܿ�
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CN_CCM_NONCE + 1], CN_CCM_L_LEN);

    // �õ�����ļ��ܽ������ SX ��
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // �������һ���������õ�����
    MemoryXor(@SX[0], @A0[0], DataByteLength, EnData);

    // CMAC ���һ��
    MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);
  end;

  // ȡ����� CMAC �Ľ���� CTR 0 �ļ��ܽ�����
  MemoryXor(@CX[0], @S0[0], CN_AEAD_BLOCK, @CX[0]);

  // �ƶ��� OutTag �з���
  FillChar(OutTag[0], SizeOf(TCnCCM128Tag), 0);
  Move(CX[0], OutTag[0], CN_CCM_M_LEN);
end;

function CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCnCCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
var
  K, N, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Nonce = nil then
    N := nil
  else
    N := @Nonce[0];

  if PlainData = nil then
    P := nil
  else
    P := @PlainData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(PlainData) > 0 then
  begin
    SetLength(Result, Length(PlainData));
    CCMEncrypt(K, Length(Key), N, Length(Nonce), P, Length(PlainData), A,
      Length(AAD), @Result[0], OutTag, EncryptType);
    if Length(Result) > 0 then
      Exit;
  end
  else
  begin
    CCMEncrypt(K, Length(Key), N, Length(Nonce), P, Length(PlainData), A,
      Length(AAD), nil, OutTag, EncryptType);
  end;
end;

function AES128CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetAES128);
end;

function AES192CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetAES192);
end;

function AES256CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetAES256);
end;

function SM4CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetSM4);
end;

procedure AES128CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES128);
end;

procedure AES192CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES192);
end;

procedure AES256CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES256);
end;

procedure SM4CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetSM4);
end;

function CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; PlainData: Pointer; var InTag: TCnCCM128Tag;
  EncryptType: TAEADEncryptType): Boolean;
var
  CMacCtx: TAEADContext;
  CtrCtx: TAEADContext;
  B0: TCn128BitsBuffer;   // CMAC ��֤ʱ�� Iv
  CX: TCn128BitsBuffer;   // CMAC �ļ�������ŵ��м��
  A0: TCn128BitsBuffer;   // �������ݿ�ĵ�һ�飬�Լ������� CMAC �����е�ԭʼ���ݵ��м��
  S0: TCn128BitsBuffer;   // ��һ�����ܿ�������֤���������������
  SX: TCn128BitsBuffer;   // CTR �ļ�������ŵ��м��
  Ctr: TCn128BitsBuffer;  // CTR �ļ�����
  Cnt, T: Int64;
  P: PByte;
  Tag: TCnCCM128Tag;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Nonce = nil then
    NonceByteLength := 0;
  if EnData = nil then
    EnByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  FillChar(B0[0], SizeOf(TCn128BitsBuffer), 0);
  FillChar(Ctr[0], SizeOf(TCn128BitsBuffer), 0);

//   +----+-+-+-+-+-+-+-+-+    |L'(L) ����
//   | λ |7|6|5|4|3|2|1|0|    |Nonce �ĳ���
//   +----+-+-+-+-+-+-+-+-+
//   |    |0|A|  M' |  L' |
//   +----+-+-+-+-+-+-+-+-+

  B0[0] := 4 * (CN_CCM_M_LEN - 2) + CN_CCM_L_LEN - 1;
  if (AAD <> nil) and (AADByteLength > 0) then
    B0[0] := B0[0] + 64;   // B0 ��ĵ�һ���ֽ�׼���ã�A λ�� 1 ��ʾ�� AAD

  Ctr[0] := CN_CCM_L_LEN - 1;

  // ��� 15 - L �� Nonce
  MoveMost(Nonce^, B0[1], NonceByteLength, CN_CCM_NONCE);
  MoveMost(Nonce^, Ctr[1], NonceByteLength, CN_CCM_NONCE);

  // ���������ֽ�˳������ĳ��ȣ��ҴӸ�λ�ض��� CCM_L_LEN �ֽڣ������������ B0
  P := PByte(@T);
  Inc(P, SizeOf(Int64) - CN_CCM_L_LEN);  // �����佨�� P �� T �ĸ߼�λ�ĵ�ַ��ϵ���������ʹ��

  T := Int64HostToNetwork(EnByteLength);
  Move(P^, B0[CN_CCM_NONCE + 1], CN_CCM_L_LEN);

  // ��ʼ�� CMAC/CTR �� Key �ȣ�׼���� CMAC/CTR
  AEADEncryptInit(CMacCtx, Key, KeyByteLength, EncryptType);
  AEADEncryptInit(CtrCtx, Key, KeyByteLength, EncryptType);

  // Ctr �ĺ�˸��ֽ��Ǽ��������ֳ�ʼ��Ϊ 0�����Ҽ��� S0 ��Ϊ��֤�ֶ�֮һ
  Cnt := 0;
  AEADEncryptBlock(CtrCtx, Ctr, S0, EncryptType);

  // CMAC ���� B0���м����� CX��Ҳ���� RFC �е� CBC Iv Out
  AEADEncryptBlock(CMacCtx, B0, CX, EncryptType);

  // �� AAD �Ļ������� A0
  if (B0[0] and $40 <> 0) then
  begin
    FillChar(A0[0], CN_AEAD_BLOCK, 0);

    if AADByteLength < $1000 - $100 then
    begin
      PCnWord(@A0[0])^ := Int16HostToNetwork(SmallInt(AADByteLength)); // �����ֽ�

      // ��һ��׼���ã������������������Լ������������
      MoveMost(AAD^, A0[2], AADByteLength, CN_AEAD_BLOCK - 2);

      // ��һ��� CX ����� CMAC ֮������Ż� CX
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // ����׼����������
      AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK - 2);
      Dec(AADByteLength, CN_AEAD_BLOCK - 2);
    end
    else
    begin
      // A0[0] ǰ���ֽ�׼����
      A0[0] := $FF;
      A0[1] := $FE;
      PCardinal(@A0[2])^ := Int32HostToNetwork(AADByteLength); // �����ֽ�

      // ��һ��׼���ã������������������Լ������������
      MoveMost(AAD^, A0[6], AADByteLength, CN_AEAD_BLOCK - 6);

      // ��һ��� CX ����� CMAC ֮������Ż� CX
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // ����׼����������
      AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK - 6);
      Dec(AADByteLength, CN_AEAD_BLOCK - 6);
    end;

    // ����������Ļ���AADByteLength ��ʱС�ڵ��� 0��������
    while AADByteLength >= CN_AEAD_BLOCK do
    begin
      Move(AAD^, A0[0], CN_AEAD_BLOCK);

      // �����飨Ҳ���������һ�飩�� CX ����� CMAC ֮������Ż� CX
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // ����׼����������
      AAD := Pointer(TCnNativeUInt(AAD) + CN_AEAD_BLOCK);
      Dec(AADByteLength, CN_AEAD_BLOCK);
    end;

    if AADByteLength > 0 then // ����ʣ��ʱ���ٶ�һ��
    begin
      FillChar(A0[0], CN_AEAD_BLOCK, 0);
      Move(AAD^, A0[0], AADByteLength);

      // CMAC ���һ��
      MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);
    end;
  end;

  // ������ AAD �� CMAC ֵ����ʼ�� Data �ģ������� A0
  // ���ҿ�ʼ���ܿ鲢������
  while EnByteLength >= CN_AEAD_BLOCK do
  begin
    Move(EnData^, A0[0], CN_AEAD_BLOCK); // ���ķ� A0

    // ��������һ�����ɼ��ܿ�
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CN_CCM_NONCE + 1], CN_CCM_L_LEN);

    // �õ�����ļ��ܽ������ SX ��
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // �����������õ�����
    MemoryXor(@SX[0], @A0[0], CN_AEAD_BLOCK, PlainData);

    // �����飨Ҳ���������һ�飩���ĺ� CX ����� CMAC ֮������Ż� CX
    MemoryXor(PlainData, @CX[0], CN_AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

    // ����׼����������
    EnData := Pointer(TCnNativeUInt(EnData) + CN_AEAD_BLOCK);
    PlainData := Pointer(TCnNativeUInt(PlainData) + CN_AEAD_BLOCK);
    Dec(EnByteLength, CN_AEAD_BLOCK);
  end;

  if EnByteLength > 0 then // ����ʣ��ʱ���ٶ�һ��
  begin
    FillChar(A0[0], CN_AEAD_BLOCK, 0);
    Move(EnData^, A0[0], EnByteLength);

    // ��������һ�����ɼ��ܿ�
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CN_CCM_NONCE + 1], CN_CCM_L_LEN);

    // �õ�����ļ��ܽ������ SX ��
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // �������һ���������õ������ȷ� A0 �﹩�������
    MemoryXor(@SX[0], @A0[0], EnByteLength, @A0[0]);

    // CMAC ���һ��
    MemoryXor(@A0[0], @CX[0], CN_AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

    // �������һ������
    Move(A0[0], PlainData^, EnByteLength);
  end;

  // ȡ����� CMAC �Ľ���� CTR 0 �ļ��ܽ�����
  MemoryXor(@CX[0], @S0[0], CN_AEAD_BLOCK, @CX[0]);

  // CMAC ����ƶ��� Tag ��
  FillChar(Tag[0], SizeOf(TCnCCM128Tag), 0);
  Move(CX[0], Tag[0], CN_CCM_M_LEN);

  // �ȶ� Tag �Ƿ���ͬ
  Result := CompareMem(@Tag[0], @InTag[0], CN_CCM_M_LEN);
end;

function CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCnCCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
var
  K, N, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Nonce = nil then
    N := nil
  else
    N := @Nonce[0];

  if EnData = nil then
    P := nil
  else
    P := @EnData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(EnData) > 0 then
  begin
    SetLength(Result, Length(EnData));
    if not CCMDecrypt(K, Length(Key), N, Length(Nonce), P, Length(EnData), A,
      Length(AAD), @Result[0], InTag, EncryptType) then // Tag �ȶ�ʧ���򷵻�
      SetLength(Result, 0);
  end
  else
  begin
    CCMDecrypt(K, Length(Key), N, Length(Nonce), P, Length(EnData), A,
      Length(AAD), nil, InTag, EncryptType); // û���ģ���ʵ Tag �ȶԳɹ����û��
  end;
end;

function AES128CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetAES128);
end;

function AES192CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetAES192);
end;

function AES256CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetAES256);
end;

function SM4CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCnCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetSM4);
end;

function AES128CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES128);
end;

function AES192CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES192);
end;

function AES256CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES256);
end;

function SM4CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetSM4);
end;

// ======== ��װ�� AES|SM4/GCM ʮ�������ֽ�����ӽ��ܺ��������� Padding ========

function AESGCMEncryptToHex(Key, Iv, AD: TBytes; Input: TBytes): string;
var
  OutTag: TCnGCM128Tag;
  Res: TBytes;
  L: Integer;
begin
  Res := AES256GCMEncryptBytes(Key, Iv, Input, AD, OutTag);
  if Length(Res) > 0 then
  begin
    L := Length(Res);
    SetLength(Res, L + SizeOf(TCnGCM128Tag));
    Move(OutTag[0], Res[L], SizeOf(TCnGCM128Tag));
    Result := BytesToHex(Res);
  end
  else
    Result := '';
end;

function AESGCMDecryptFromHex(Key, Iv, AD: TBytes; const Input: string): TBytes;
var
  InTag: TCnGCM128Tag;
  Res: TBytes;
begin
  Res := HexToBytes(Input);
  if Length(Res) < SizeOf(TCnGCM128Tag) then // ̫��˵��û Tag
  begin
    Result := nil;
    Exit;
  end;

  Move(Res[Length(Res) - SizeOf(TCnGCM128Tag)], InTag[0], SizeOf(TCnGCM128Tag));
  SetLength(Res, Length(Res) - SizeOf(TCnGCM128Tag));
  Result := AES256GCMDecryptBytes(Key, Iv, Res, AD, InTag);
end;

function SM4GCMEncryptToHex(Key, Iv, AD: TBytes; Input: TBytes): string;
var
  OutTag: TCnGCM128Tag;
  Res: TBytes;
  L: Integer;
begin
  Res := SM4GCMEncryptBytes(Key, Iv, Input, AD, OutTag);
  if Length(Res) > 0 then
  begin
    L := Length(Res);
    SetLength(Res, L + SizeOf(TCnGCM128Tag));
    Move(OutTag[0], Res[L], SizeOf(TCnGCM128Tag));
    Result := BytesToHex(Res);
  end
  else
    Result := '';
end;

function SM4GCMDecryptFromHex(Key, Iv, AD: TBytes; const Input: string): TBytes;
var
  InTag: TCnGCM128Tag;
  Res: TBytes;
begin
  Res := HexToBytes(Input);
  if Length(Res) < SizeOf(TCnGCM128Tag) then // ̫��˵��û Tag
  begin
    Result := nil;
    Exit;
  end;

  Move(Res[Length(Res) - SizeOf(TCnGCM128Tag)], InTag[0], SizeOf(TCnGCM128Tag));
  SetLength(Res, Length(Res) - SizeOf(TCnGCM128Tag));
  Result := SM4GCMDecryptBytes(Key, Iv, Res, AD, InTag);
end;

// =================== ChaCha20_Poly1305 ���ݿ�ӽ��ܺ��� ======================

procedure ChaCha20Poly1305Encrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnPoly1305Digest);
var
  ChaChaKey: TCnChaChaKey;
  Nonce: TCnChaChaNonce;
  OutKey: TCnChaChaState;
  Poly1305Key: TCnPoly1305Key;
  Poly1305Context: TCnPoly1305Context;
  Lens: array[0..1] of Int64;
begin
  MoveMost(Key^, ChaChaKey[0], KeyByteLength, SizeOf(TCnChaChaKey));
  MoveMost(Iv^, Nonce[0], IvByteLength, SizeOf(TCnChaChaNonce));

  ChaCha20Block(ChaChaKey, Nonce, 0, OutKey); // ע������ļ������� 0

  // 64 �ֽڵ� OutKey ��ǰһ����Ϊ���� Poly1305 ժҪ Tag �� Key
  Move(OutKey[0], Poly1305Key[0], SizeOf(TCnPoly1305Key));

  ChaCha20EncryptData(ChaChaKey, Nonce, PlainData, PlainByteLength, OutEnData);

  // ��ʼ�ֿ���� Poly1305
  Poly1305Init(Poly1305Context, Poly1305Key);

  // ���� AAD ���� Padding
  Poly1305Update(Poly1305Context, AAD, AADByteLength, True);

  // �������ļ��� Padding
  Poly1305Update(Poly1305Context, OutEnData, PlainByteLength, True);

  Lens[0] := AADByteLength;
  Lens[1] := PlainByteLength;
  Lens[0] := Int64ToLittleEndian(Lens[0]); // RFC �涨Ҫ��С��
  Lens[1] := Int64ToLittleEndian(Lens[1]);

  // ������������
  Poly1305Update(Poly1305Context, @Lens[0], SizeOf(Lens));

  // ���õ����
  Poly1305Final(Poly1305Context, OutTag);
end;

function ChaCha20Poly1305Decrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnPoly1305Digest): Boolean;
var
  ChaChaKey: TCnChaChaKey;
  Nonce: TCnChaChaNonce;
  OutKey: TCnChaChaState;
  Poly1305Key: TCnPoly1305Key;
  Poly1305Context: TCnPoly1305Context;
  Tag: TCnPoly1305Digest;
  Lens: array[0..1] of Int64;
begin
  MoveMost(Key^, ChaChaKey[0], KeyByteLength, SizeOf(TCnChaChaKey));
  MoveMost(Iv^, Nonce[0], IvByteLength, SizeOf(TCnChaChaNonce));

  ChaCha20Block(ChaChaKey, Nonce, 0, OutKey); // ע������ļ������� 0

  // 64 �ֽڵ� OutKey ��ǰһ����Ϊ���� Poly1305 ժҪ Tag �� Key
  Move(OutKey[0], Poly1305Key[0], SizeOf(TCnPoly1305Key));

  ChaCha20DecryptData(ChaChaKey, Nonce, EnData, EnByteLength, OutPlainData);

  // ��ʼ�ֿ���� Poly1305
  Poly1305Init(Poly1305Context, Poly1305Key);

  // ���� AAD ���� Padding
  Poly1305Update(Poly1305Context, AAD, AADByteLength, True);

  // �������ļ��� Padding
  Poly1305Update(Poly1305Context, EnData, EnByteLength, True);

  Lens[0] := AADByteLength;
  Lens[1] := EnByteLength;
  Lens[0] := Int64ToLittleEndian(Lens[0]); // RFC �涨Ҫ��С��
  Lens[1] := Int64ToLittleEndian(Lens[1]);

  // ������������
  Poly1305Update(Poly1305Context, @Lens[0], SizeOf(Lens));

  // ���õ����
  Poly1305Final(Poly1305Context, Tag);

  // ���ҽ���������� Tag �ʹ��� Tag ��ͬ��ͨ��
  Result := CompareMem(@Tag[0], @InTag[0], SizeOf(TCnPoly1305Digest));
end;

// ================== ChaCha20_Poly1305 �ֽ�����ӽ��ܺ��� =====================

function ChaCha20Poly1305EncryptBytes(Key, Iv, PlainData, AAD: TBytes;
  var OutTag: TCnPoly1305Digest): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if PlainData = nil then
    P := nil
  else
    P := @PlainData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(PlainData) > 0 then
  begin
    SetLength(Result, Length(PlainData));
    ChaCha20Poly1305Encrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), @Result[0], OutTag);
  end
  else
  begin
    ChaCha20Poly1305Encrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), nil, OutTag);
  end;
end;

function ChaCha20Poly1305DecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TCnPoly1305Digest): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if EnData = nil then
    P := nil
  else
    P := @EnData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(EnData) > 0 then
  begin
    SetLength(Result, Length(EnData));
    if not ChaCha20Poly1305Decrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), @Result[0], InTag) then // Tag �ȶ�ʧ���򷵻�
      SetLength(Result, 0);
  end
  else
  begin
    ChaCha20Poly1305Decrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), nil, InTag); // û���ģ���ʵ Tag �ȶԳɹ����û��
  end;
end;

// =================== XChaCha20_Poly1305 ���ݿ�ӽ��ܺ��� =====================

procedure XChaCha20Poly1305Encrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCnPoly1305Digest);
var
  ChaChaKey: TCnChaChaKey;
  H: TCnHChaChaNonce;
  N: TCnChaChaNonce;
  OutKey: TCnHChaChaSubKey;
  P: PByte;
begin
  MoveMost(Key^, ChaChaKey[0], KeyByteLength, SizeOf(TCnChaChaKey));
  MoveMost(Iv^, H[0], IvByteLength, SizeOf(TCnHChaChaNonce));

  HChaCha20SubKey(ChaChaKey, H, OutKey); // ���һ���µ� Key
  N[0] := 0;                             // ���ֽ� 0 ���� XChaCha20 ��ʣ�� 8 �ֽڹ� 12 �ֽ���Ϊ ChaCha20_Poly1305 �� Nonce
  N[1] := 0;
  N[2] := 0;
  N[3] := 0;

  P := PByte(Iv);
  Inc(P, 16);
  Move(P^, N[4], CN_XCHACHA_NONCE_SIZE - CN_HCHACHA_NONCE_SIZE);

  ChaCha20Poly1305Encrypt(@OutKey[0], SizeOf(TCnHChaChaSubKey), @N[0], SizeOf(TCnChaChaNonce),
    PlainData, PlainByteLength, AAD, AADByteLength, OutEnData, OutTag);
end;

function XChaCha20Poly1305Decrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCnPoly1305Digest): Boolean;
var
  ChaChaKey: TCnChaChaKey;
  H: TCnHChaChaNonce;
  N: TCnChaChaNonce;
  OutKey: TCnHChaChaSubKey;
  P: PByte;
begin
  MoveMost(Key^, ChaChaKey[0], KeyByteLength, SizeOf(TCnChaChaKey));
  MoveMost(Iv^, H[0], IvByteLength, SizeOf(TCnHChaChaNonce));

  HChaCha20SubKey(ChaChaKey, H, OutKey); // ���һ���µ� Key
  N[0] := 0;                             // ���ֽ� 0 ���� XChaCha20 ��ʣ�� 8 �ֽڹ� 12 �ֽ���Ϊ ChaCha20_Poly1305 �� Nonce
  N[1] := 0;
  N[2] := 0;
  N[3] := 0;

  P := PByte(Iv);
  Inc(P, 16);
  Move(P^, N[4], CN_XCHACHA_NONCE_SIZE - CN_HCHACHA_NONCE_SIZE);

  Result := ChaCha20Poly1305Decrypt(@OutKey[0], SizeOf(TCnHChaChaSubKey), @N[0], SizeOf(TCnChaChaNonce),
    EnData, EnByteLength, AAD, AADByteLength, OutPlainData, InTag);
end;

// ================== XChaCha20_Poly1305 �ֽ�����ӽ��ܺ��� ====================

function XChaCha20Poly1305EncryptBytes(Key, Iv, PlainData, AAD: TBytes;
  var OutTag: TCnPoly1305Digest): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if PlainData = nil then
    P := nil
  else
    P := @PlainData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(PlainData) > 0 then
  begin
    SetLength(Result, Length(PlainData));
    XChaCha20Poly1305Encrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), @Result[0], OutTag);
  end
  else
  begin
    XChaCha20Poly1305Encrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), nil, OutTag);
  end;
end;

function XChaCha20Poly1305DecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TCnPoly1305Digest): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if EnData = nil then
    P := nil
  else
    P := @EnData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(EnData) > 0 then
  begin
    SetLength(Result, Length(EnData));
    if not XChaCha20Poly1305Decrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), @Result[0], InTag) then // Tag �ȶ�ʧ���򷵻�
      SetLength(Result, 0);
  end
  else
  begin
    XChaCha20Poly1305Decrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), nil, InTag); // û���ģ���ʵ Tag �ȶԳɹ����û��
  end;
end;

end.
