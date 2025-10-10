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

unit CnInt128;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ�128 λ���޷�������������ʵ��
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫ�� TCnInt128 �� TCnUInt128 �ṹʵ���� 128 λ�����Ļ����������㡣
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 XE 2
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2022.06.14 V1.1
*               ʵ�ֳ��������࣬������
*           2022.06.11 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, CnNative;

const
  SCN_MAX_INT128 = '170141183460469231731687303715884105727';
  {* �����з��� Int128 ֵ������ $7FFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF}

  SCN_MIN_INT128 = '-170141183460469231731687303715884105728';
  {* ��С���з��� Int128 ֵ������ $80000000 00000000 00000000 00000000}

  SCN_MAX_UINT128 = '340282366920938463463374607431768211455';
  {* �����޷��� UInt128 ֵ������ $FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF}

type
  TCnInt128 = packed record
  {* 128 λ�з��������ṹ}
    Lo64, Hi64: Int64;        // ע�� Lo64 �ڲ�����Ϊ 64 λ�޷�����������
  end;
  PCnInt128 = ^TCnInt128;
  {* 128 λ�з��������ṹָ��}

  TCnUInt128 = packed record
  {* 128 λ�޷��������ṹ}
    Lo64, Hi64: TUInt64;
  end;
  PCnUInt128 = ^TCnUInt128;
  {* 128 λ�޷��������ṹָ��}

// ========================= Int128 ���㺯�� ===================================

procedure Int128Set(var R: TCnInt128; Lo: Int64; Hi: Int64); overload;
{* �ֱ����� 128 λ�з��������ĸߵ� 64 λԭʼֵ�������⴦�������š�

   ������
     var R: TCnInt128                     - �����õ� 128 λ�з�������
     Lo: Int64                            - �� 64 λֵ
     Hi: Int64                            - �� 64 λֵ

   ����ֵ�����ޣ�
}

procedure Int128Set(var R: TCnInt128; Lo: Int64); overload;
{* ���� 128 λ�з��������ĵ� 64 λֵ���� 64 λ���� Lo �����������ȫ 0 ��ȫ 1��

   ������
     var R: TCnInt128                     - �����õ� 128 λ�з�������
     Lo: Int64                            - �� 64 λֵ

   ����ֵ�����ޣ�
}

procedure Int128Copy(var D: TCnInt128; var S: TCnInt128);
{* ���� 128 λ�з���������

   ������
     var D: TCnInt128                     - Ŀ�� 128 λ�з�������
     var S: TCnInt128                     - Դ 128 λ�з�������

   ����ֵ�����ޣ�
}

function Int128IsZero(var N: TCnInt128): Boolean;
{* �ж� һ 128 λ�з��������Ƿ��� 0��

   ������
     var N: TCnInt128                     - ���жϵ� 128 λ�з�������

   ����ֵ��Boolean                        - �����Ƿ��� 0
}

procedure Int128SetZero(var N: TCnInt128);
{* ��һ 128 λ�з��������� 0��

   ������
     var N: TCnInt128                     - �����õ� 128 λ�з�������

   ����ֵ�����ޣ�
}

procedure Int128Add(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128); overload;
{* 128 λ�з���������ӣ�����������������R��A��B ������ͬ��A B ʹ�ò�������ֿ���������ֵ��

   ������
     var R: TCnInt128                     - ��
     var A: TCnInt128                     - ����һ
     var B: TCnInt128                     - ������

   ����ֵ�����ޣ�
}

procedure Int128Add(var R: TCnInt128; var A: TCnInt128; V: Int64); overload;
{* ��һ 128 λ�з�����������һ�� 64 λ�з��������������� V Ϊ��ֵ�������

   ������
     var R: TCnInt128                     - ��
     var A: TCnInt128                     - ����һ
     V: Int64                             - ������

   ����ֵ�����ޣ�
}

procedure Int128Sub(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128); overload;
{* 128 λ�з����������������������������R��A��B ������ͬ��

   ������
     var R: TCnInt128                     - ��
     var A: TCnInt128                     - ������
     var B: TCnInt128                     - ����

   ����ֵ�����ޣ�
}

procedure Int128Sub(var R: TCnInt128; var A: TCnInt128; V: Int64); overload;
{* ��һ 128 λ�з���������ȥһ�� 64 λ�з��������������� V Ϊ��ֵ�������

   ������
     var R: TCnInt128                     - ��
     var A: TCnInt128                     - ������
     V: Int64                             - ����

   ����ֵ�����ޣ�
}

procedure Int128Mul(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 128 λ�з���������ˣ�����������쳣����ʱδʵ�� UInt128 �� ResHi �������ƣ���R��A��B ������ͬ��

   ������
     var R: TCnInt128                     - ��
     var A: TCnInt128                     - ����һ
     var B: TCnInt128                     - ������

   ����ֵ�����ޣ�
}

procedure Int128DivMod(var A: TCnInt128; var B: TCnInt128; var R: TCnInt128; var M: TCnInt128);
{* 128 λ�з��������������࣬A / B = R ... M ���� A��B��R��M ���Ը��õ� R M ������ͬ��

   ������
     var A: TCnInt128                     - ������
     var B: TCnInt128                     - ����
     var R: TCnInt128                     - ��
     var M: TCnInt128                     - ����

   ����ֵ�����ޣ�
}

procedure Int128Div(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 128 λ�з�������������R = A div B��R��A��B ������ͬ��

   ������
     var R: TCnInt128                     - ��
     var A: TCnInt128                     - ������
     var B: TCnInt128                     - ����

   ����ֵ�����ޣ�
}

procedure Int128Mod(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 128 λ�з����������࣬R = A mod B��R��A��B ������ͬ��

   ������
     var R: TCnInt128                     - ����
     var A: TCnInt128                     - ������
     var B: TCnInt128                     - ����

   ����ֵ�����ޣ�
}

procedure Int128ShiftLeft(var N: TCnInt128; S: Integer);
{* 128 λ�з���������λ���� S λ���� S Ϊ������ʾ���ơ�

   ������
     var N: TCnInt128                     - ����λ�� 128 λ�з�������
     S: Integer                           - ����λ��

   ����ֵ�����ޣ�
}

procedure Int128ShiftRight(var N: TCnInt128; S: Integer);
{* 128 λ�з���������λ���� S λ���� S Ϊ������ʾ����

   ������
     var N: TCnInt128                     - ����λ�� 128 λ�з�������
     S: Integer                           - ����λ��

   ����ֵ�����ޣ�
}

procedure Int128And(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* ���� 128 λ�з���������λ�롣

   ������
     var R: TCnInt128                     - ��λ����
     var A: TCnInt128                     - ���밴λ����������һ
     var B: TCnInt128                     - ���밴λ������������

   ����ֵ�����ޣ�
}

procedure Int128Or(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* ���� 128 λ�з���������λ��

   ������
     var R: TCnInt128                     - ��λ����
     var A: TCnInt128                     - ���밴λ����������һ
     var B: TCnInt128                     - ���밴λ������������

   ����ֵ�����ޣ�
}

procedure Int128Xor(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* ���� 128 λ�з���������λ���

   ������
     var R: TCnInt128                     - ��λ�����
     var A: TCnInt128                     - ���밴λ�����������һ
     var B: TCnInt128                     - ���밴λ�������������

   ����ֵ�����ޣ�
}

procedure Int128Negate(var N: TCnInt128);
{* ��һ 128 λ�з���������Ϊ���෴����ע�⵱��С�� Int128 ��ֵʱ���������쳣��

   ������
     var N: TCnInt128                     - ��ȡ��ֵ�� 128 λ�з�������

   ����ֵ�����ޣ�
}

procedure Int128Not(var N: TCnInt128);
{* ��һ 128 λ�з��������󷴡�

   ������
     var N: TCnInt128                     - ���󷴵� 128 λ�з�������

   ����ֵ�����ޣ�
}

procedure Int128SetBit(var N: TCnInt128; Bit: Integer);
{* ��һ 128 λ�з���������ĳһλ�� 1��Bit �� 0 �� 127��

   ������
     var N: TCnInt128                     - ����λ�� 128 λ�з�������
     Bit: Integer                         - λ������

   ����ֵ�����ޣ�
}

procedure Int128ClearBit(var N: TCnInt128; Bit: Integer);
{* ��һ 128 λ�з���������ĳһλ�� 0��Bit �� 0 �� 127��

   ������
     var N: TCnInt128                     - ����λ�� 128 λ�з�������
     Bit: Integer                         - λ������

   ����ֵ�����ޣ�
}

function Int128IsBitSet(var N: TCnInt128; Bit: Integer): Boolean;
{* ����һ 128 λ�з���������ĳһλ�Ƿ��� 1��Bit �� 0 �� 127

   ������
     var N: TCnInt128                     - ������� 128 λ�з�������
     Bit: Integer                         - λ������

   ����ֵ��Boolean                        - ���ظ�λ�Ƿ�Ϊ 1
}

function Int128IsNegative(var N: TCnInt128): Boolean;
{* �ж�һ 128 λ�з��������Ƿ��Ǹ�����

   ������
     var N: TCnInt128                     - ���жϵ� 128 λ�з�������

   ����ֵ��Boolean                        - �����Ƿ���
}

function Int128Equal(var A: TCnInt128; var B: TCnInt128): Boolean; overload;
{* �ж����� 128 λ�з��������Ƿ���ȡ�

   ������
     var A: TCnInt128                     - ���жϵ� 128 λ�з�������һ
     var B: TCnInt128                     - ���жϵ� 128 λ�з���������

   ����ֵ��Boolean                        - �����Ƿ����
}

function Int128Equal(var A: TCnInt128; B: Int64): Boolean; overload;
{* �ж�һ�� 128 λ�з���������һ�� Int64 �Ƿ���ȡ�

   ������
     var A: TCnInt128                     - ���жϵ� 128 λ�з�������
     B: Int64                             - ���жϵ� 64 λ����

   ����ֵ��Boolean                        - �����Ƿ����
}

function Int128Compare(var A: TCnInt128; var B: TCnInt128): Integer;
{* �Ƚ����� 128 λ�з����������ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     var A: TCnInt128                     - ���Ƚϵ� 128 λ�з�������һ
     var B: TCnInt128                     - ���Ƚϵ� 128 λ�з���������

   ����ֵ��Integer                        - ���رȽϽ��
}

function Int128ToHex(var N: TCnInt128): string;
{* �� 128 λ�з�������ת��Ϊʮ�������ַ�����

   ������
     var N: TCnInt128                     - ��ת���� 128 λ�з�������

   ����ֵ��string                         - ����ʮ�������ַ���
}

function HexToInt128(const S: string): TCnInt128;
{* ��ʮ�������ַ���ת��Ϊ 128 λ�з���������

   ������
     const S: string                      - ��ת����ʮ�������ַ���

   ����ֵ��TCnInt128                      - ���� 128 λ�з�������
}

function Int128ToStr(var N: TCnInt128): string;
{* �� 128 λ�з�������ת��Ϊʮ�����ַ�����

   ������
     var N: TCnInt128                     - ��ת���� 128 λ�з�������

   ����ֵ��string                         - ����ʮ�����ַ���
}

function StrToInt128(const S: string): TCnInt128;
{* ��ʮ�����ַ���ת��Ϊ 128 λ�з���������

   ������
     const S: string                      - ��ת����ʮ�����ַ���

   ����ֵ��TCnInt128                      - ���� 128 λ�з�������
}

// ======================== UInt128 ���㺯�� ===================================

procedure UInt128Set(var R: TCnUInt128; Lo: TUInt64; Hi: TUInt64); overload;
{* �ֱ����� 128 λ�޷��������ĸߵ� 64 λֵ��

   ������
     var R: TCnUInt128                    - �����õ� 128 λ�޷�������
     Lo: TUInt64                          - �� 64 λֵ
     Hi: TUInt64                          - �� 64 λֵ

   ����ֵ�����ޣ�
}

procedure UInt128Set(var R: TCnUInt128; Lo: TUInt64); overload;
{* ���� 128 λ�޷��������ĵ� 64 λֵ����λ�� 0��

   ������
     var R: TCnUInt128                    - �����õ� 128 λ�޷�������
     Lo: TUInt64                          - �� 64 λֵ

   ����ֵ�����ޣ�
}

procedure UInt128Copy(var D: TCnUInt128; var S: TCnUInt128);
{* ���� 128 λ�޷���������

   ������
     var D: TCnUInt128                    - Ŀ�� 128 λ�޷�������
     var S: TCnUInt128                    - Դ 128 λ�޷�������

   ����ֵ�����ޣ�
}

function UInt128IsZero(var N: TCnUInt128): Boolean;
{* �ж� һ 128 λ�޷��������Ƿ��� 0��

   ������
     var N: TCnUInt128                    - ���жϵ� 128 λ�޷�������

   ����ֵ��Boolean                        - �����Ƿ��� 0
}

procedure UInt128SetZero(var N: TCnUInt128);
{* ��һ 128 λ�޷��������� 0��

   ������
     var N: TCnUInt128                    - �����õ� 128 λ�޷�������

   ����ֵ�����ޣ�
}

procedure UInt128Add(var R: TCnUInt128; var A: TCnUInt128; V: TUInt64); overload;
{* ��һ 128 λ�޷�����������һ�� 64 λ�޷���������

   ������
     var R: TCnUInt128                    - ��
     var A: TCnUInt128                    - ����һ
     V: TUInt64                           - ������

   ����ֵ�����ޣ�
}

procedure UInt128Add(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128); overload;
{* 128 λ�޷���������ӣ�����������������R��A��B ������ͬ��

   ������
     var R: TCnUInt128                    - ��
     var A: TCnUInt128                    - ����һ
     var B: TCnUInt128                    - ������

   ����ֵ�����ޣ�
}

procedure UInt128Sub(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 128 λ�޷����������������������������R��A��B ������ͬ��

   ������
     var R: TCnUInt128                    - ��
     var A: TCnUInt128                    - ������
     var B: TCnUInt128                    - ����

   ����ֵ�����ޣ�
}

procedure UInt128Mul(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128; ResHi: PCnUInt128 = nil);
{* 128 λ�޷���������ˣ�������򳬹� 128 λ�ķ� ResHi �С�
   ResHi �紫 nil ����������쳣��R��A��B ������ͬ��

   ������
     var R: TCnUInt128                    - ��
     var A: TCnUInt128                    - ����һ
     var B: TCnUInt128                    - ������
     ResHi: PCnUInt128                    - ���λ��ŵ�ַ����Ϊ nil��������������쳣

   ����ֵ�����ޣ�
}

procedure UInt128DivMod(var A: TCnUInt128; var B: TCnUInt128; var R: TCnUInt128; var M: TCnUInt128);
{* 128 λ�޷��������������࣬A / B = R ... M��A��B��R��M ���Ը��õ� R M ������ͬ��

   ������
     var A: TCnUInt128                    - ������
     var B: TCnUInt128                    - ����
     var R: TCnUInt128                    - ��
     var M: TCnUInt128                    - ����

   ����ֵ�����ޣ�
}

procedure UInt128Div(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 128 λ�޷�������������R = A div B��R��A��B ������ͬ��

   ������
     var R: TCnUInt128                    - ��
     var A: TCnUInt128                    - ������
     var B: TCnUInt128                    - ����

   ����ֵ�����ޣ�
}

procedure UInt128Mod(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 128 λ�޷����������࣬R = A mod B��R��A��B ������ͬ��

   ������
     var R: TCnUInt128                    - ����
     var A: TCnUInt128                    - ������
     var B: TCnUInt128                    - ����

   ����ֵ�����ޣ�
}

procedure UInt128ShiftLeft(var N: TCnUInt128; S: Integer);
{* 128 λ�޷���������λ���� S λ���� S Ϊ������ʾ���ơ�

   ������
     var N: TCnUInt128                    - ����λ�� 128 λ�޷�������
     S: Integer                           - ����λ��

   ����ֵ�����ޣ�
}

procedure UInt128ShiftRight(var N: TCnUInt128; S: Integer);
{* 128 λ�޷���������λ���� S λ���� S Ϊ������ʾ���ơ�

   ������
     var N: TCnUInt128                    - ����λ�� 128 λ�޷�������
     S: Integer                           - ����λ��

   ����ֵ�����ޣ�
}

procedure UInt128And(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* ���� 128 λ�޷���������λ�롣

   ������
     var R: TCnUInt128                    - ��λ����
     var A: TCnUInt128                    - ���밴λ����������һ
     var B: TCnUInt128                    - ���밴λ������������

   ����ֵ�����ޣ�
}

procedure UInt128Or(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* ���� 128 λ�޷���������λ��

   ������
     var R: TCnUInt128                    - ��λ����
     var A: TCnUInt128                    - ���밴λ�����������һ
     var B: TCnUInt128                    - ���밴λ�������������

   ����ֵ�����ޣ�
}

procedure UInt128Xor(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* ���� 128 λ�޷���������λ���

   ������
     var R: TCnUInt128                    - ��λ�����
     var A: TCnUInt128                    - ���밴λ�����������һ
     var B: TCnUInt128                    - ���밴λ�������������

   ����ֵ�����ޣ�
}

procedure UInt128Not(var N: TCnUInt128);
{* 128 λ�޷��������󷴡�

   ������
     var N: TCnUInt128                    - ���󷴵� 128 λ�޷�������

   ����ֵ�����ޣ�
}

procedure UInt128SetBit(var N: TCnUInt128; Bit: Integer);
{* ��һ 128 λ�޷���������ĳһλ�� 1��Bit �� 0 �� 127��

   ������
     var N: TCnUInt128                    - ����λ�� 128 λ�޷�������
     Bit: Integer                         - λ������

   ����ֵ�����ޣ�
}

procedure UInt128ClearBit(var N: TCnUInt128; Bit: Integer);
{* ��һ 128 λ�޷���������ĳһλ�� 0��Bit �� 0 �� 127��

   ������
     var N: TCnUInt128                    - ����λ�� 128 λ�޷�������
     Bit: Integer                         - λ������

   ����ֵ�����ޣ�
}

function UInt128IsBitSet(var N: TCnUInt128; Bit: Integer): Boolean;
{* ����һ 128 λ�޷���������ĳһλ�Ƿ��� 0��Bit �� 0 �� 127��

   ������
     var N: TCnUInt128                    - ������� 128 λ�޷�������
     Bit: Integer                         - λ������

   ����ֵ��Boolean                        - ���ظ�λ�Ƿ�Ϊ 1
}

function UInt128Equal(var A: TCnUInt128; var B: TCnUInt128): Boolean; overload;
{* �ж����� 128 λ�޷��������Ƿ���ȡ�

   ������
     var A: TCnUInt128                    - ���жϵ� 128 λ�޷�������һ
     var B: TCnUInt128                    - ���жϵ� 128 λ�޷���������

   ����ֵ��Boolean                        - �����Ƿ����
}

function UInt128Equal(var A: TCnUInt128; B: TUInt64): Boolean; overload;
{* �ж�һ�� 128 λ�޷���������һ�� Int64/UInt64 �Ƿ���ȡ�

   ������
     var A: TCnUInt128                    - ���жϵ� 128 λ�޷�������
     B: TUInt64                           - ���жϵ� 64 λ����

   ����ֵ��Boolean                        - �����Ƿ����
}

function UInt128Compare(var A: TCnUInt128; var B: TCnUInt128): Integer;
{* �Ƚ����� 128 λ�޷����������ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     var A: TCnUInt128                    - ���Ƚϵ� 128 λ�޷�������һ
     var B: TCnUInt128                    - ���Ƚϵ� 128 λ�޷���������

   ����ֵ��Integer                        - ���رȽϽ��
}

function IsUInt128AddOverflow(var A: TCnUInt128; var B: TCnUInt128): Boolean;
{* �ж����� 64 λ�޷���������Ƿ���� 128 λ�޷������ޡ�

   ������
     var A: TCnUInt128                    - ����һ
     var B: TCnUInt128                    - ������

   ����ֵ��Boolean                        - �����Ƿ����
}

function UInt128ToHex(var N: TCnUInt128): string;
{* �� 128 λ�޷�������ת��Ϊʮ�������ַ�����

   ������
     var N: TCnUInt128                    - ��ת���� 128 λ�޷�������

   ����ֵ��string                         - ����ʮ�������ַ���
}

function HexToUInt128(const S: string): TCnUInt128;
{* ��ʮ�������ַ���ת��Ϊ 128 λ�޷���������

   ������
     const S: string                      - ��ת����ʮ�������ַ���

   ����ֵ��TCnUInt128                     - ���� 128 λ�޷�������
}

function UInt128ToStr(var N: TCnUInt128): string;
{* �� 128 λ�޷�������ת��Ϊʮ�����ַ�����

   ������
     var N: TCnUInt128                    - ��ת���� 128 λ�޷�������

   ����ֵ��string                         - ����ʮ�����ַ���
}

function StrToUInt128(const S: string): TCnUInt128;
{* ��ʮ�����ַ���ת��Ϊ 128 λ�޷���������

   ������
     const S: string                      - ��ת����ʮ�����ַ���

   ����ֵ��TCnUInt128                     - ���� 128 λ�޷�������
}

var
  CnInt128Zero: TCnInt128 = (Lo64: 0; Hi64: 0);
  {* ���� 0 �� 128 λ�з�����������}
  CnInt128One: TCnInt128 = (Lo64:1; Hi64: 0);
  {* ���� 1 �� 128 λ�з�����������}

  CnUInt128Zero: TCnUInt128 = (Lo64: 0; Hi64: 0);
  {* ���� 0 �� 128 λ�޷�����������}
  CnUInt128One: TCnUInt128 = (Lo64:1; Hi64: 0);
  {* ���� 0 �� 128 λ�޷�����������}

implementation

const
  SCnErrorInt128NegateOverflow = 'Int128 Negate Overflow';
  SCnErrorInt128MulOverflow = 'Int128 Mul Overflow';
  SCnErrorUint128MulOverflow = 'UInt128 Mul Overflow';

procedure Int128Set(var R: TCnInt128; Lo, Hi: Int64);
begin
  R.Lo64 := Lo;
  R.Hi64 := Hi;
end;

procedure Int128Set(var R: TCnInt128; Lo: Int64);
begin
  R.Lo64 := Lo;
  if Lo >= 0 then
    R.Hi64 := 0
  else
    R.Hi64 := not 0;
end;

procedure Int128Copy(var D, S: TCnInt128);
begin
  D.Lo64 := S.Lo64;
  D.Hi64 := S.Hi64;
end;

function Int128IsZero(var N: TCnInt128): Boolean;
begin
  Result := (N.Lo64 = 0) and (N.Hi64 = 0);
end;

procedure Int128SetZero(var N: TCnInt128);
begin
  N.Lo64 := 0;
  N.Hi64 := 0;
end;

procedure Int128Add(var R, A, B: TCnInt128);
var
  C: Integer;
begin
{$IFDEF SUPPORT_UINT64}
  UInt64Add(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(B.Lo64), C);
{$ELSE}
  UInt64Add(R.Lo64, A.Lo64, B.Lo64, C);
{$ENDIF}
  R.Hi64 := A.Hi64 + B.Hi64 + C;
end;

procedure Int128Add(var R, A: TCnInt128; V: Int64); overload;
var
  C: Integer;
begin
  if V < 0 then
  begin
    V := (not V) + 1; // �󷴼�һ����ֵȻ���
{$IFDEF SUPPORT_UINT64}
    UInt64Sub(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Sub(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end
  else // V >= 0���� UInt64 ͬ������
  begin
{$IFDEF SUPPORT_UINT64}
    UInt64Add(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Add(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end;
  R.Hi64 := A.Hi64 + C;
end;

procedure Int128Sub(var R, A, B: TCnInt128);
var
  C: Integer;
begin
{$IFDEF SUPPORT_UINT64}
  UInt64Sub(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(B.Lo64), C);
{$ELSE}
  UInt64Sub(R.Lo64, A.Lo64, B.Lo64, C);
{$ENDIF}
  R.Hi64 := A.Hi64 - B.Hi64 - C;
end;

procedure Int128Sub(var R, A: TCnInt128; V: Int64);
var
  C: Integer;
begin
  if V < 0 then
  begin
    V := (not V) + 1; // �󷴼�һ����ֵȻ���
{$IFDEF SUPPORT_UINT64}
    UInt64Add(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Add(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end
  else // V >= 0���� UInt64 ͬ������
  begin
{$IFDEF SUPPORT_UINT64}
    UInt64Sub(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Sub(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end;
  R.Hi64 := A.Hi64 - C;
end;

procedure Int128Mul(var R, A, B: TCnInt128);
var
  N1, N2: Boolean;
begin
  N1 := Int128IsNegative(A);
  N2 := Int128IsNegative(B);

  // ȫ����
  if N1 then
    Int128Negate(A);
  if N2 then
    Int128Negate(B);

  UInt128Mul(TCnUInt128(R), TCnUInt128(A), TCnUInt128(B));
  if Int128IsNegative(R) then // �˻��Ǹ�˵�������
    raise EIntOverflow.Create(SCnErrorInt128MulOverflow);

  if N1 <> N2 then // ֻҪ��һ�����
    Int128Negate(R);

  // ���ȥ
  if N1 then
    Int128Negate(A);
  if N2 then
    Int128Negate(B);
end;

procedure Int128DivMod(var A, B, R, M: TCnInt128);
var
  Sft: Integer;
  AA, BB: TCnInt128;
  NA, NB: Boolean;
begin
  if Int128IsZero(B) then
    raise EDivByZero.Create(SDivByZero);

  if Int128IsZero(A) then
  begin
    Int128SetZero(R);
    Int128SetZero(M);
    Exit;
  end;

  Int128Copy(AA, A);
  Int128Copy(BB, B);
  NA := Int128IsNegative(AA);
  NB := Int128IsNegative(BB);

  if NA then
    Int128Negate(AA);
  if NB then
    Int128Negate(BB);  // ȫת��

  if Int128Compare(AA, BB) < 0 then
  begin
    Int128Copy(M, AA);
    if NA <> NB then
      Int128Negate(M); // �����Ϊ��
    Int128SetZero(R);
    Exit;
  end;

  Int128SetZero(R);
  Int128Copy(M, AA);
  Sft := 0;

  // ����������ͱ��������λ��ͬ�ұȱ�����С
  while (Int128Compare(BB, M) < 0) and not GetUInt64BitSet(BB.Hi64, 62) do
  begin
    if Sft = 127 then
      Break;

    Int128ShiftLeft(BB, 1);
    Inc(Sft);
    if Int128Compare(BB, M) > 0 then
    begin
      Int128ShiftRight(BB, 1);
      Dec(Sft);
      Break;
    end;
  end;

  // �𲽳�
  while True do
  begin
    if Int128Compare(BB, M) <= 0 then // �����ƣ�ֻ��Ҫ��һ�Σ�D �����
    begin
      Int128Sub(M, M, BB);
      Int128SetBit(R, Sft);

      // �����ʱ M Ϊ 0��ò�ƿ�����������û������
      if Int128IsZero(M) then
        Exit;
    end;

    if Sft > 0 then
    begin
      Int128ShiftRight(BB, 1);
      Dec(Sft);
    end
    else
      Break;
  end;

  if NA <> NB then
    Int128Negate(R);
  if Int128IsNegative(A) then
    Int128Negate(M);
end;

procedure Int128Div(var R, A, B: TCnInt128);
var
  T: TCnInt128;
begin
  Int128DivMod(A, B, R, T);
end;

procedure Int128Mod(var R, A, B: TCnInt128);
var
  T: TCnInt128;
begin
  Int128DivMod(A, B, T, R);
end;

procedure Int128ShiftLeft(var N: TCnInt128; S: Integer);
begin
  UInt128ShiftLeft(TCnUInt128(N), S);
end;

procedure Int128ShiftRight(var N: TCnInt128; S: Integer);
begin
  UInt128ShiftRight(TCnUInt128(N), S);
end;

procedure Int128And(var R, A, B: TCnInt128);
begin
  R.Lo64 := A.Lo64 and B.Lo64;
  R.Hi64 := A.Hi64 and B.Hi64;
end;

procedure Int128Or(var R, A, B: TCnInt128);
begin
  R.Lo64 := A.Lo64 or B.Lo64;
  R.Hi64 := A.Hi64 or B.Hi64;
end;

procedure Int128Xor(var R, A, B: TCnInt128);
begin
  R.Lo64 := A.Lo64 xor B.Lo64;
  R.Hi64 := A.Hi64 xor B.Hi64;
end;

procedure Int128Negate(var N: TCnInt128);
var
  C: Integer;
begin
  // ȫ����Ȼ�������һ
  N.Lo64 := not N.Lo64;
  N.Hi64 := not N.Hi64;

{$IFDEF SUPPORT_UINT64}
  UInt64Add(UInt64(N.Lo64), UInt64(N.Lo64), 1, C);
{$ELSE}
  UInt64Add(N.Lo64, N.Lo64, 1, C);
{$ENDIF}
  if C > 0 then
  begin
    if N.Hi64 = CN_MAX_INT64 then // Hi64 ̫���������
      raise EIntOverflow.Create(SCnErrorInt128NegateOverflow);

    N.Hi64 := N.Hi64 + C;
  end;
end;

procedure Int128Not(var N: TCnInt128);
begin
  N.Lo64 := not N.Lo64;
  N.Hi64 := not N.Hi64;
end;

procedure Int128SetBit(var N: TCnInt128; Bit: Integer);
begin
{$IFDEF SUPPORT_UINT64}
  if Bit > 63 then
    UInt64SetBit(TUInt64(N.Hi64), Bit - 64)
  else
    UInt64SetBit(TUInt64(N.Lo64), Bit);
{$ELSE}
  if Bit > 63 then
    UInt64SetBit(N.Hi64, Bit - 64)
  else
    UInt64SetBit(N.Lo64, Bit);
{$ENDIF}
end;

procedure Int128ClearBit(var N: TCnInt128; Bit: Integer);
begin
{$IFDEF SUPPORT_UINT64}
  if Bit > 63 then
    UInt64ClearBit(TUInt64(N.Hi64), Bit - 64)
  else
    UInt64ClearBit(TUInt64(N.Lo64), Bit);
{$ELSE}
  if Bit > 63 then
    UInt64ClearBit(N.Hi64, Bit - 64)
  else
    UInt64ClearBit(N.Lo64, Bit);
{$ENDIF}
end;

function Int128IsBitSet(var N: TCnInt128; Bit: Integer): Boolean;
begin
  if Bit > 63 then
    Result := GetUInt64BitSet(N.Hi64, Bit - 64)
  else
    Result := GetUInt64BitSet(N.Hi64, Bit);
end;

function Int128IsNegative(var N: TCnInt128): Boolean;
begin
  Result := N.Hi64 < 0;
end;

function Int128Equal(var A, B: TCnInt128): Boolean;
begin
  Result := (A.Lo64 = B.Lo64) and (A.Hi64 = B.Hi64);
end;

function Int128Equal(var A: TCnInt128; B: Int64): Boolean; overload;
begin
  Result := (A.Hi64 = 0) and (A.Lo64 = B);
end;

function Int128Compare(var A, B: TCnInt128): Integer;
var
  R: Integer;
begin
  if A.Hi64 > B.Hi64 then
    Result := 1
  else if A.Hi64 < B.Hi64 then
    Result := -1
  else
  begin
    R := UInt64Compare(A.Lo64, B.Lo64); // �� 64 λ����Ϊ�޷������Ƚ�
    if A.Hi64 < 0 then // ����Ǹ�ֵ������
      R := -R;

    if R > 0 then
      Result := 1
    else if R < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;

function Int128ToHex(var N: TCnInt128): string;
var
  T, M, Mask: TCnInt128;
  Neg: Boolean;
begin
  if Int128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  Int128Copy(T, N);
  Neg := Int128IsNegative(T);
  if Neg then
    Int128Negate(T);

  Result := '';
  Int128Set(Mask, $F);
  while not Int128IsZero(T) do
  begin
    Int128And(M, T, Mask);
    Int128ShiftRight(T, 4);
    Result := IntToHex(M.Lo64, 1) + Result;
  end;

  if Neg then
    Result := '-' + Result;
end;

{$WARNINGS OFF}

function HexToInt128(const S: string): TCnInt128;
var
  I, K: Integer;
  St, T: TCnInt128;
  Neg: Boolean;
begin
  Int128SetZero(Result);
  Int128Set(St, 16);

  Neg := False;
  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9', 'a'..'f', 'A'..'F'] then
    begin
      Int128Mul(Result, Result, St);
      K := 0;
      if (S[I] >= '0') and (S[I] <= '9') then
        K := Ord(S[I]) - Ord('0')
      else if (S[I] >= 'A') and (S[I] <= 'F') then
        K := Ord(S[I]) - Ord('A') + 10
      else if (S[I] >= 'a') and (S[I] <= 'f') then
        K := Ord(S[I]) - Ord('a') + 10;

      Int128Set(T, K);
      Int128Add(Result, Result, T);
    end
    else if (I = 1) and (S[I] = '-') then
      Neg := True
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;

  if Neg then
    Int128Negate(Result);
end;

function Int128ToStr(var N: TCnInt128): string;
var
  T, Ten, M: TCnInt128;
  Neg: Boolean;
begin
  if Int128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  // ��С�ĸ�ֵ�������෴�������ֱ�Ӵ���
  if (N.Hi64 = CN_MIN_INT64) and (N.Lo64 = 0) then
  begin
    Result := SCN_MIN_INT128;
    Exit;
  end;

  Int128Copy(T, N);
  Int128Set(Ten, 10);

  Neg := Int128IsNegative(T);
  if Neg then
    Int128Negate(T); // ע�� T �������С�ĸ�ֵ���˴������������������ǰ�����

  Result := '';
  while not Int128IsZero(T) do
  begin
    Int128DivMod(T, Ten, T, M);
    Result := IntToStr(M.Lo64) + Result;
  end;

  if Neg then
    Result := '-' + Result;
end;

function StrToInt128(const S: string): TCnInt128;
var
  I: Integer;
  Ten, T: TCnInt128;
  Neg: Boolean;
begin
  Int128SetZero(Result);
  Int128Set(Ten, 10);

  Neg := False;
  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9'] then
    begin
      Int128Mul(Result, Result, Ten);
      Int128Set(T, Ord(S[I]) - Ord('0'));
      Int128Add(Result, Result, T);
    end
    else if (I = 1) and (S[I] = '-') then
      Neg := True
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;
  if Neg then
    Int128Negate(Result);
end;

{$WARNINGS ON}

// ======================== UInt128 ���㺯�� ===================================

procedure UInt128Set(var R: TCnUInt128; Lo, Hi: TUInt64);
begin
  R.Lo64 := Lo;
  R.Hi64 := Hi;
end;

procedure UInt128Set(var R: TCnUInt128; Lo: TUInt64);
begin
  R.Lo64 := Lo;
  R.Hi64 := 0;
end;

procedure UInt128Copy(var D, S: TCnUInt128);
begin
  D.Lo64 := S.Lo64;
  D.Hi64 := S.Hi64;
end;

function UInt128IsZero(var N: TCnUInt128): Boolean;
begin
  Result := (N.Lo64 = 0) and (N.Hi64 = 0);
end;

procedure UInt128SetZero(var N: TCnUInt128);
begin
  N.Lo64 := 0;
  N.Hi64 := 0;
end;

{$WARNINGS OFF}

procedure UInt128Add(var R, A, B: TCnUInt128);
var
  C: Integer;
begin
  UInt64Add(R.Lo64, A.Lo64, B.Lo64, C);
  R.Hi64 := A.Hi64 + B.Hi64 + C;
end;

procedure UInt128Add(var R: TCnUInt128; var A: TCnUInt128; V: TUInt64);
var
  C: Integer;
begin
  UInt64Add(R.Lo64, A.Lo64, V, C);
  R.Hi64 := R.Hi64 + C;
end;

// ���� 128 λ�޷���������ӣ�A + B => R������������������� 1 ���λ������������
procedure UInt128AddC(var R: TCnUInt128; A, B: TCnUInt128; out Carry: Integer);
begin
  UInt128Add(R, A, B);
  if UInt128Compare(R, A) < 0 then // �޷�����ӣ����ֻҪС����һ������˵�������
    Carry := 1
  else
    Carry := 0;
end;

procedure UInt128Sub(var R, A, B: TCnUInt128);
var
  C: Integer;
begin
  UInt64Sub(R.Lo64, A.Lo64, B.Lo64, C);
  R.Hi64 := A.Hi64 - B.Hi64 - C;
end;

{$WARNINGS ON}

procedure UInt128Mul(var R, A, B: TCnUInt128; ResHi: PCnUInt128);
var
  R0, R1, R2, R3, Lo, T: TCnUInt128;
  C1, C2: Integer;
begin
  UInt64MulUInt64(A.Lo64, B.Lo64, R0.Lo64, R0.Hi64); //       0       0   | R0.Hi64 R0.Lo64
  UInt64MulUInt64(A.Hi64, B.Lo64, R1.Lo64, R1.Hi64); //       0   R1.Hi64 | R1.Lo64    0
  UInt64MulUInt64(A.Lo64, B.Hi64, R2.Lo64, R2.Hi64); //       0   R2.Hi64 | R2.Lo64    0
  UInt64MulUInt64(A.Hi64, B.Hi64, R3.Lo64, R3.Hi64); //   R3.Hi64 R3.Lo64 |    0       0

  T.Lo64 := 0;
  T.Hi64 := R1.Lo64;
  UInt128AddC(Lo, R0, T, C1);

  T.Hi64 := R2.Lo64;
  UInt128AddC(Lo, Lo, T, C2);

  UInt128Copy(R, Lo); // �� 128 λ����Ѿ��õ���

  if (C1 > 0) or (C2 > 0) or (R1.Hi64 > 0) or (R2.Hi64 > 0) or (R3.Lo64 > 0) or (R3.Hi64 > 0) then
  begin
    // ������������ֵҪ�� ResHi^ �У�������û�ṩ�������쳣
    if ResHi = nil then
      raise EIntOverflow.Create(SCnErrorUint128MulOverflow);

    T.Hi64 := 0;
    T.Lo64 := R1.Hi64;
    UInt128Add(ResHi^, R3, T);

    T.Lo64 := R2.Hi64;
    UInt128Add(ResHi^, ResHi^, T);

    T.Lo64 := C1 + C2;
    UInt128Add(ResHi^, ResHi^, T); // �ӽ�λ�������ٳ��������
  end;
end;

procedure UInt128DivMod(var A, B, R, M: TCnUInt128);
var
  Sft: Integer;
  BB: TCnUInt128;
begin
  if UInt128IsZero(B) then
    raise EDivByZero.Create(SDivByZero);

  if UInt128IsZero(A) then
  begin
    UInt128SetZero(R);
    UInt128SetZero(M);
    Exit;
  end;

  if UInt128Compare(A, B) < 0 then
  begin
    UInt128Copy(M, A);
    UInt128SetZero(R);
    Exit;
  end;

  Sft := 0;
  UInt128Copy(BB, B);  // �� BB ���м���������� R M �ȿ����� A B �����޸Ĺ����г���
  UInt128Copy(M, A);   // �޸��� M��Ҫȷ������û�� A B
  UInt128SetZero(R);   // �޸��� R

  // ����������ͱ��������λ��ͬ�ұȱ�����С
  while (UInt128Compare(BB, M) < 0) and not GetUInt64BitSet(BB.Hi64, 63) do
  begin
    if Sft = 127 then
      Break;

    UInt128ShiftLeft(BB, 1);
    Inc(Sft);
    if UInt128Compare(BB, M) > 0 then
    begin
      UInt128ShiftRight(BB, 1);
      Dec(Sft);
      Break;
    end;
  end;

  // �𲽳�
  while True do
  begin
    if UInt128Compare(BB, M) <= 0 then // �����ƣ�ֻ��Ҫ��һ�Σ�D �����
    begin
      UInt128Sub(M, M, BB);
      UInt128SetBit(R, Sft);

      // �����ʱ M Ϊ 0��ò�ƿ�����������û������
      if UInt128IsZero(M) then
        Exit;
    end;

    if Sft > 0 then
    begin
      UInt128ShiftRight(BB, 1);
      Dec(Sft);
    end
    else
      Break;
  end;
end;

procedure UInt128Div(var R, A, B: TCnUInt128);
var
  T: TCnUInt128;
begin
  UInt128DivMod(A, B, R, T);
end;

procedure UInt128Mod(var R, A, B: TCnUInt128);
var
  T: TCnUInt128;
begin
  UInt128DivMod(A, B, T, R);
end;

procedure UInt128ShiftLeft(var N: TCnUInt128; S: Integer);
var
  T, M: TUInt64;
begin
  if S = 0 then
    Exit;

  if S < 0 then
    UInt128ShiftRight(N, -S);

  if S >= 128 then // ȫ������
  begin
    N.Hi64 := 0;
    N.Lo64 := 0;
  end
  else if S >= 64 then
  begin
    // Lo Ϊȫ 0
    N.Hi64 := N.Lo64 shl (S - 64);
    N.Lo64 := 0;
  end
  else
  begin
    // ȡ�� Lo �ĸ� S λ
    M := (not TUInt64(0)) shl (64 - S);
    T := N.Lo64 and M;
    T := T shr (64 - S);

    // Lo �� Hi ������ S
    N.Lo64 := N.Lo64 shl S;
    N.Hi64 := N.Hi64 shl S;

    // Lo ���Ƴ��ĸ߲��ַŵ� Hi �����ĵͲ���
    N.Hi64 := N.Hi64 or T;
  end;
end;

procedure UInt128ShiftRight(var N: TCnUInt128; S: Integer);
var
  T, M: TUInt64;
begin
  if S = 0 then
    Exit;

  if S < 0 then
    UInt128ShiftLeft(N, -S);

  if S >= 128 then // ȫ������
  begin
    N.Hi64 := 0;
    N.Lo64 := 0;
  end
  else if S >= 64 then
  begin
    // Hi Ϊȫ 0
    N.Lo64 := N.Hi64 shr (S - 64);
    N.Hi64 := 0;
  end
  else
  begin
    // ȡ�� Hi �ĵ� S λ
    M := (not TUInt64(0)) shr (64 - S);
    T := N.Hi64 and M;
    T := T shl (64 - S);

    // Lo �� Hi ������ S
    N.Lo64 := N.Lo64 shr S;
    N.Hi64 := N.Hi64 shr S;

    // Hi ���Ƴ��ĵͲ��ַŵ� Lo �����ĸ߲���
    N.Lo64 := N.Lo64 or T;
  end;
end;

procedure UInt128And(var R, A, B: TCnUInt128);
begin
  R.Lo64 := A.Lo64 and B.Lo64;
  R.Hi64 := A.Hi64 and B.Hi64;
end;

procedure UInt128Or(var R, A, B: TCnUInt128);
begin
  R.Lo64 := A.Lo64 or B.Lo64;
  R.Hi64 := A.Hi64 or B.Hi64;
end;

procedure UInt128Xor(var R, A, B: TCnUInt128);
begin
  R.Lo64 := A.Lo64 xor B.Lo64;
  R.Hi64 := A.Hi64 xor B.Hi64;
end;

procedure UInt128Not(var N: TCnUInt128);
begin
  N.Lo64 := not N.Lo64;
  N.Hi64 := not N.Hi64;
end;

procedure UInt128SetBit(var N: TCnUInt128; Bit: Integer);
begin
  if Bit > 63 then
    UInt64SetBit(N.Hi64, Bit - 64)
  else
    UInt64SetBit(N.Lo64, Bit);
end;

procedure UInt128ClearBit(var N: TCnUInt128; Bit: Integer);
begin
  if Bit > 63 then
    UInt64ClearBit(N.Hi64, Bit - 64)
  else
    UInt64ClearBit(N.Lo64, Bit);
end;

function UInt128IsBitSet(var N: TCnUInt128; Bit: Integer): Boolean;
begin
  if Bit > 63 then
    Result := GetUInt64BitSet(N.Hi64, Bit - 64)
  else
    Result := GetUInt64BitSet(N.Hi64, Bit);
end;

function UInt128Equal(var A, B: TCnUInt128): Boolean;
begin
  Result := (A.Lo64 = B.Lo64) and (A.Hi64 = B.Hi64);
end;

function UInt128Equal(var A: TCnUInt128; B: TUInt64): Boolean;
begin
  Result := (A.Lo64 = B) and (A.Hi64 = 0);
end;

function UInt128Compare(var A, B: TCnUInt128): Integer;
var
  T: Integer;
begin
  T := UInt64Compare(A.Hi64, B.Hi64);
  if T > 0 then
    Result := 1
  else if T < 0 then
    Result := -1
  else
  begin
    T := UInt64Compare(A.Lo64, B.Lo64);
    if T > 0 then
      Result := 1
    else if T < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;

function IsUInt128AddOverflow(var A, B: TCnUInt128): Boolean;
var
  R: TCnUInt128;
begin
  UInt128Add(R, A, B);
  Result := UInt128Compare(R, A) < 0;
end;

function UInt128ToHex(var N: TCnUInt128): string;
var
  T, M, Mask: TCnUInt128;
begin
  if UInt128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  UInt128Copy(T, N);
  Result := '';

  UInt128Set(Mask, $F);
  while not UInt128IsZero(T) do
  begin
    UInt128And(M, T, Mask);
    UInt128ShiftRight(T, 4);
    Result := IntToHex(M.Lo64, 1) + Result;
  end;
end;

{$WARNINGS OFF}

function HexToUInt128(const S: string): TCnUInt128;
var
  I, K: Integer;
  St, T: TCnUInt128;
begin
  UInt128SetZero(Result);
  UInt128Set(St, 16);

  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9', 'a'..'f', 'A'..'F'] then
    begin
      UInt128Mul(Result, Result, St);
      K := 0;
      if (S[I] >= '0') and (S[I] <= '9') then
        K := Ord(S[I]) - Ord('0')
      else if (S[I] >= 'A') and (S[I] <= 'F') then
        K := Ord(S[I]) - Ord('A') + 10
      else if (S[I] >= 'a') and (S[I] <= 'f') then
        K := Ord(S[I]) - Ord('a') + 10;

      UInt128Set(T, K);
      UInt128Add(Result, Result, T);
    end
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;
end;

function UInt128ToStr(var N: TCnUInt128): string;
var
  T, Ten, M: TCnUInt128;
begin
  if UInt128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  UInt128Copy(T, N);
  UInt128Set(Ten, 10);
  Result := '';

  while not UInt128IsZero(T) do
  begin
    UInt128DivMod(T, Ten, T, M);
    Result := IntToStr(M.Lo64) + Result;
  end;
end;

function StrToUInt128(const S: string): TCnUInt128;
var
  I: Integer;
  Ten, T: TCnUInt128;
begin
  UInt128SetZero(Result);
  UInt128Set(Ten, 10);

  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9'] then
    begin
      UInt128Mul(Result, Result, Ten);
      UInt128Set(T, Ord(S[I]) - Ord('0'));
      UInt128Add(Result, Result, T);
    end
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;
end;

{$WARNINGS ON}

end.
