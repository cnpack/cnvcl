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

unit CnFloat;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�������������ת����Ԫ
* ��Ԫ���ߣ���ǬԪ(wqyfavor@163.com)
* ��    ע���õ�Ԫʵ���˵����ȡ�˫���ȡ���չ���ȸ������Ľ�����ת����
*
*           ע�� Extended ֻ�� Win32 ���� 10 �ֽڣ�MacOS/Linux x64 �¾��� 16 �ֽڣ�Win64 �� ARM ƽ̨���� 8 �ֽ�
*           ���ң�MacOS64 �µ� 16 �ֽ���չ���Ȳ���  IEEE 754-2008 �й涨�� Quadruple ��ʽ������ǰ 10 �ֽڽضϣ�
*           �ڲ��ṹͬ Win32 �µ���չ 10 �ֽڡ�
*
* ����ƽ̨��WinXP + Delphi 2009
* ���ݲ��ԣ�Delphi 2007���� Extended ������ֻ֧��С��ģʽ
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2023.01.13
*               ���ݴ��� Win64 �� Extended �� 8 �ֽ� Double ������ 10 �ֽ���չ���ȵ�����
*               ���ݴ��� MacOS64/Linux64 �µ� 16 �ֽ� Extended��ֻ�ضϴ���ǰ 10 �ֽڣ�
*           2022.02.17
*               ���� FPC �ı���֧�֣�������
*           2021.09.05
*               ����������������ת��Ϊ UInt64����֧�� UInt64 ���� Int64 ���棩�ĺ���
*           2020.11.11
*               ���������� UInt64����֧�� UInt64 ���� Int64 ���棩ת��Ϊ�������ĺ���
*           2020.06.24
*               ���������������⿪��ƴ�յĺ���
*           2009.1.12
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, {$IFDEF MSWINDOWS} Windows, {$ENDIF} CnNative;

{
  IEEE 754 �涨�����ָ����ʽ����Ч���ڵ�λ 0��

  ������ Single             1 ����λ S��8 λָ�� E��23 λ��Ч�� M �������� 24 λ�� 1������ 4 �ֽ� 32 λ
  ˫���� Double             1 ����λ S��11 λָ�� E��52 λ��Ч�� M�������� 53 λ�� 1������ 8 �ֽ� 64 λ
  ��չ˫���� Extended       1 ����λ S��15 λָ�� E��64 λ��Ч�� M���� 64 λ��ʽ 1������ 10 �ֽ� 80 λ

  IEEE 754-2008 ����
  �ı����� Quadruple        1 ����λ S��15 λָ�� E��112 λ��Ч�� M���� 16 �ֽ� 128 λ
  �˱����� Octuple          1 ����λ S��19 λָ�� E��236 λ��Ч�� M���� 32 �ֽ� 256 λ

  ���У�����λ S��0 ��ʾ����1 ��ʾ����E Ҫ��ȥ 127/1023/16383/16383 ��������ָ��
        M: �淶����/˫���ȵĶ����� M �ĸ�λ�Ӹ� 1. ������Ч������չ������ӣ������� 1.
           ����ֵ����Ч���������� 1.xxxx ����ʽ������ 2 �� E �η���ע�ⲻ�� 10 �� E �η�����

  ��S ����λ��X ָ����M ��Ч���ֵ���������ʵֵ����Ϊ���滯���˲��ҿ���������С���㣩

  ��ʽ          �ֽ� 1    �ֽ� 2    �ֽ� 3    �ֽ� 4    ...  �ֽ� n��ÿ���ֽڵ��ұߵ�λ�� 0��

  ������ 4      SXXXXXXX  XMMMMMMM  MMMMMMMM  MMMMMMMM
      ����      01000110  00011100  01000000  00000000
                   4   6     1   C     4   0     0   0     ���ڴ���Ϊ��С�ˣ�������ȫ�ֵ��򣩸�����ʵ��ֵ 10000
         ��Ϊ�� 0 10001100 00111000100000000000000
         S��0��X��140��Ҫ��ȥ 127 �õ���ʵ�� 13����M��ԭʼֵ 1C4000
         ��λ�� 1 ��С���㣬�õ� 100111000100000000000000 Ϊʵ����Ч���֣���ʵֵ���� 1.001110001
         ������ʵ��ֵ��1.001110001 ���� 13 λ���õ�ʮ������ 2710.00��С�����ȫ 0�������ʮ���� 10000 ����ʵ�ʽ��

  ˫���� 8      SXXXXXXX  XXXXMMMM  MMMMMMMM  MMMMMMMM  ...  MMMMMMMM
      ����      01000000  11000011  10001000  0000000000000000000000000000000000000000
                   4   0     C   3     8   8     0   0     ���ڴ���Ϊ��С�ˣ�������ȫ�ֵ��򣩸�����ʵ��ֵ 10000
         ��Ϊ�� 0 10000001100 0011100010000000000000000000000000000000000000000000
         S��0��X��1036��Ҫ��ȥ 1023 �õ���ʵ�� 13����M��ԭʼֵ 3 88 00 00 00 00 00
         ��λ�� 1 ��С����õ������� 10011100010000000000000000000000000000000000000000000 Ϊʵ����Ч���֣���ʵֵ���� 1.001110001
         ������ʵ��ֵ��1.001110001 ���� 13 λ���õ�ʮ������ 2710.00��С�����ȫ 0�������ʮ���� 10000 ����ʵ�ʽ��

  ��չ˫���� 10 SXXXXXXX  XXXXXXXX  1MMMMMMM  MMMMMMMM  ...  MMMMMMMM  // ע��������Ч���ְ����� 1�����඼ʡ���� 1
      ����      01000000  00001100  10011100  01000000  ...
                   4   0     0   C     9   C     4   0     ���ڴ���Ϊ��С�ˣ�������ȫ�ֵ��򣩸�����ʵ��ֵ 10000
         ��Ϊ�� 0 100000000001100 1001110000100000 ...
         S��0��X��16396��Ҫ��ȥ 16383 �õ���ʵ�� 13����M��ԭʼֵ 9C 40 00 ...
         ��λ���ö���� 1��ֻҪ��С����õ������� 10011100 01000000 ... Ϊʵ����Ч���֣���ʵֵ���� 1.001110001
         ������ʵ��ֵ��1.001110001 ���� 13 λ���õ�ʮ������ 2710.00��С�����ȫ 0�������ʮ���� 10000 ����ʵ�ʽ��

  �ı����� 16   SXXXXXXX  XXXXXXXX  MMMMMMMM  MMMMMMMM  ...  MMMMMMMM
  �˱����� 32   SXXXXXXX  XXXXXXXX  XXXXMMMM  MMMMMMMM  ...  MMMMMMMM

  ע�⣺Little Endian �����ϣ��ֽ� 1 �� n �����������򡣱���Ԫ���ѵ�����

  0��ȫ 0
  -0��ȫ 0 ������λΪ 1
  ���������ָ��ȫ 1����Ч��ȫ 0������ 0 �� 1
}

type
  TCnQuadruple = packed record
  {* Delphi �����ı��������ͣ��ýṹ����ָ�����}
    Lo: TUInt64;
    Hi0: Cardinal;
    case Boolean of
      True:  (Hi1: Cardinal);
      False: (W0, W1: Word);   // С�˻����ϣ����ź�ָ��������� W1 ��
  end;
  PCnQuadruple = ^TCnQuadruple;
  {* ָ���ı����Ƚṹ��ָ��}

  TCnOctuple = packed record
  {* Delphi ���ް˱��������ͣ������� Int64 ����ָ����棬���޴�����}
    F0: Int64;
    F1: Int64;
    F2: Int64;
    F3: Int64;
  end;
  PCnOctuple = ^TCnOctuple;
  {* ָ��˱����Ƚṹ��ָ��}

  ECnFloatSizeError = class(Exception);
  {* �����������쳣}

const
  CN_EXTENDED_SIZE_8  =          8;
  {* Win64 �µ� Extended ���͵ĳ��ȣ�ֻ�� 8 �ֽ�}

  CN_EXTENDED_SIZE_10 =          10;
  {* Win32 �µ� Extended ���͵ĳ��ȣ��Ǳ�׼�� 10 �ֽ�}

  CN_EXTENDED_SIZE_16 =          16;
  {* MACOS64/Linux64 �µ� Extended ���͵ĳ��ȣ��� 16 �ֽ�}

  CN_SIGN_SINGLE_MASK =          $80000000;
  {* �����ȸ������ķ���λ����}

  CN_SIGN_DOUBLE_MASK =          $8000000000000000;
  {* ˫���ȸ������ķ���λ����}

  CN_SIGN_EXTENDED_MASK =        $8000;
  {* ��չ���ȸ������ķ���λ���룬�Ѿ���ȥ�� 8 �ֽ���Ч����}

  CN_SIGN_QUADRUPLE_MASK =       $80000000;
  {* �ı����ȸ������ķ���λ���룬ֻ���ǰ���ֽڣ���ȥ�˺�����������}

  CN_EXPONENT_SINGLE_MASK =      $7F800000;
  {* �����ȸ�������ָ�����룬��Ҫ���� 23 λ}

  CN_EXPONENT_DOUBLE_MASK =      $7FF0000000000000;
  {* ˫���ȸ�������ָ�����룬��Ҫ���� 52 λ}

  CN_EXPONENT_EXTENDED_MASK =    $7FFF;
  {* ��չ���ȸ�������ָ�����룬��ȥ�� 8 �ֽ���Ч����}

  CN_EXPONENT_QUADRUPLE_MASK =   $7FFF;
  {* �ı����ȸ�������ָ�����룬��ȥ�� 14 �ֽ���Ч����}

  CN_SIGNIFICAND_SINGLE_MASK =   $007FFFFF;
  {* �����ȸ���������Ч�������룬�� 23 λ}

  CN_SIGNIFICAND_DOUBLE_MASK =   $000FFFFFFFFFFFFF;
  {* ˫���ȸ���������Ч�������룬�� 52 λ}

  CN_SIGNIFICAND_EXTENDED_MASK = $FFFFFFFFFFFFFFFF;
  {* ��չ���ȸ���������Ч�������룬�� 64 λ����ʵ����ȫ�� 8 �ֽ���}

  CN_SIGNIFICAND_QUADRUPLE_MASK = $FFFF;
  {* �ı����ȸ���������Ч�������룬ֻ���ǰ���ֽڣ����м��Ϻ�����������}

  CN_SINGLE_SIGNIFICAND_BITLENGTH         = 23;
  {* �����ȸ���������Ч����λ����}

  CN_DOUBLE_SIGNIFICAND_BITLENGTH         = 52;
  {* ˫���ȸ���������Ч����λ����}

  CN_EXTENDED_SIGNIFICAND_BITLENGTH       = 63;
  {* ��չ���ȸ���������Ч����λ����}

  CN_EXPONENT_OFFSET_SINGLE               = 127;
  {* �����ȸ�������ָ��ƫ��ֵ��ʵ��ָ��ֵҪ���ϸ�ֵ���ܴ��뵥���ȸ�������ָ����}

  CN_EXPONENT_OFFSET_DOUBLE               = 1023;
  {* ˫���ȸ�������ָ��ƫ��ֵ��ʵ��ָ��ֵҪ���ϸ�ֵ���ܴ���˫���ȸ�������ָ����}

  CN_EXPONENT_OFFSET_EXTENDED             = 16383;
  {* ��չ���ȸ�������ָ��ƫ��ֵ��ʵ��ָ��ֵҪ���ϸ�ֵ���ܴ�����չ���ȸ�������ָ������10 �� 16 �ֽ���չ���ȸ�������Ϊ��ֵ}

  // �� Max ��������ָ��ȫ 1 �����Σ��������������
  CN_SINGLE_MIN_EXPONENT                  = -127;
  {* �����ȸ���������Сָ��}

  CN_SINGLE_MAX_EXPONENT                  = 127;     
  {* �����ȸ����������ָ��}

  CN_DOUBLE_MIN_EXPONENT                  = -1023;
  {* ˫���ȸ���������Сָ��}

  CN_DOUBLE_MAX_EXPONENT                  = 1023;
  {* ˫���ȸ����������ָ��}

  CN_EXTENDED_MIN_EXPONENT                = -16383;
  {* ��չ���ȸ���������Сָ��}

  CN_EXTENDED_MAX_EXPONENT                = 16383;
  {* ��չ���ȸ����������ָ��}

procedure ExtractFloatSingle(Value: Single; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: Cardinal);
{* �ӵ����ȸ������н������λ��ָ������������λ 1 ��ȥ����С�����������Ч���֡�
   ע�⣺ָ��Ϊ��ʵָ������Ч����Ϊ�� 24 λ������ԭʼ��Ϊ 0~22 λ���� 23 λΪ����ȥ�� 1��

   ������
     Value: Single                        - ���⿪�ĵ����ȸ�����
     out SignNegative: Boolean            - ����λ��True Ϊ��
     out Exponent: Integer                - ָ��
     out Mantissa: Cardinal               - ��Ч����

   ����ֵ�����ޣ�
}

procedure ExtractFloatDouble(Value: Double; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64);
{* ��˫���ȸ������н������λ��ָ������������λ 1 ��ȥ����С�����������Ч���֡�
   ע�⣺ָ��Ϊ��ʵָ������Ч����Ϊ�� 53 λ������ԭʼ��Ϊ 0~51 λ���� 52 λΪ����ȥ�� 1��

   ������
     Value: Double                        - ���⿪��˫���ȸ�����
     out SignNegative: Boolean            - ����λ��True Ϊ��
     out Exponent: Integer                - ָ��
     out Mantissa: TUInt64                - ��Ч����

   ����ֵ�����ޣ�
}

procedure ExtractFloatExtended(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64); overload;
{* ����չ���ȸ������н������λ��ָ����ȥ����С�����������Ч���֣�֧�� 8 �ֽڡ�10 �ֽڡ�
   �Լ� 16 �ֽڽض�Ϊ 10 �ֽڵ� Extended ��ʽ���ú���ʵ������ƽ̨�� Extended �ߴ硣
   ע�⣺ָ��Ϊ��ʵָ������Ч����Ϊȫ�� 64 λ�����λ 63 λΪ�Դ��� 1��

   ������
     Value: Extended                      - ���⿪����չ���ȸ�����
     out SignNegative: Boolean            - ����λ��True Ϊ��
     out Exponent: Integer                - ָ��
     out Mantissa: TUInt64                - ��Ч����

   ����ֵ�����ޣ�
}

procedure ExtractFloatExtended(ValueAddr: Pointer; ExtendedSize: Integer;
  out SignNegative: Boolean; out Exponent: Integer; out Mantissa: TUInt64); overload;
{* �Ӳ������ȵ���չ���ȸ��������ڵ�ַ�н������λ��ָ����ȥ����С�����������Ч���֣�֧�� 8 �ֽڡ�10 �ֽڡ�
   �Լ� 16 �ֽڽض�Ϊ 10 �ֽڵ� Extended ��ʽ���ú���ʵ���뱾ƽ̨�� Extended �ߴ��޹ء�
   ע�⣺ָ��Ϊ��ʵָ������Ч����Ϊȫ�� 64 λ�����λ 63 λΪ�Դ��� 1��

   ������
     ValueAddr: Pointer                   - ���⿪����չ���ȸ��������ڵ�ַ
     ExtendedSize: Integer                - ����չ���ȵĴ�С��ֻ֧�� 8��10��16 ����ֵ
     out SignNegative: Boolean            - ����λ��True Ϊ��
     out Exponent: Integer                - ָ��
     out Mantissa: TUInt64                - ��Ч����

   ����ֵ�����ޣ�
}

procedure ExtractFloatQuadruple(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out MantissaLo: TUInt64; out MantissaHi: TUInt64);
{* ��ʮ���ֽھ��ȸ������н������λ��ָ����ȥ����С�����������Ч���֣�ֻ�� Extended Ϊ 16 �ֽ�
   �Ҹ�ʽ�� IEEE 754-2008 ����ı����ȸ���ʱ��Ч��Ŀǰ Delphi ��֧�ָø�ʽ��
   ע�⣺ָ��Ϊ��ʵָ������Ч���� 112 λ����Ϊ�ߵ������֣�����ԭʼ��Ϊ 0~110 λ���� 111 λΪ����ȥ�� 1��

   ������
     Value: Extended                      - ���⿪��ʮ���ֽھ��ȸ�����
     out SignNegative: Boolean            - ����λ��True Ϊ��
     out Exponent: Integer                - ָ��
     out MantissaLo: TUInt64              - ��Ч���ֵ� 64 λ
     out MantissaHi: TUInt64              - ��Ч���ָ� 64 λ

   ����ֵ�����ޣ�
}

procedure CombineFloatSingle(SignNegative: Boolean; Exponent: Integer;
  Mantissa: Cardinal; var Value: Single);
{* �ѷ���λ��ָ������Ч����ƴ�ɵ����ȸ�������Ҫ����Ч����Ϊ���滯�ģ�Ҳ���� 24 λ�����λΪ 1��

   ������
     SignNegative: Boolean                - ����λ��True Ϊ��
     Exponent: Integer                    - ָ��
     Mantissa: Cardinal                   - ��Ч���֣����� 24 λ��Ч
     var Value: Single                    - ������ϵĵ����ȸ�����

   ����ֵ�����ޣ�
}

procedure CombineFloatDouble(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Double);
{* �ѷ���λ��ָ������Ч����ƴ��˫���ȸ�������Ҫ����Ч����Ϊ���滯�ģ�Ҳ���� 53 λ�����λΪ 1��

   ������
     SignNegative: Boolean                - ����λ��True Ϊ��
     Exponent: Integer                    - ָ��
     Mantissa: TUInt64                    - ��Ч���֣����� 53 λ��Ч
     var Value: Double                    - ������ϵ�˫���ȸ�����

   ����ֵ�����ޣ�
}

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Extended); overload;
{* �ѷ���λ��ָ������Ч����ƴ����չ���ȸ�������֧�� 10 �ֽڡ�
   �Լ� 16 �ֽڽض�Ϊ 10 �ֽڵ� Extended ��ʽ���ú���ʵ������ƽ̨�� Extended �ߴ硣
   Ҫ����Ч����Ϊ���滯�ģ�Ҳ���� 64 λ�����λΪ 1��

   ������
     SignNegative: Boolean                - ����λ��True Ϊ��
     Exponent: Integer                    - ָ��
     Mantissa: TUInt64                    - ��Ч���֣�64 λȫ��Ч
     var Value: Extended                  - ������ϵ���չ���ȸ�����

   ����ֵ�����ޣ�
}

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; ValueAddr: Pointer; ExtendedSize: Integer); overload;
{* �ѷ���λ��ָ������Ч����ƴ����չ���ȸ�������֧�� 10 �ֽڡ�
   �Լ� 16 �ֽڽض�Ϊ 10 �ֽڵ� Extended ��ʽ���ú���ʵ���뱾ƽ̨�� Extended �ߴ��޹ء�
   Ҫ����Ч����Ϊ���滯�ģ�Ҳ���� 64 λ�����λΪ 1��

   ������
     SignNegative: Boolean                - ����λ��True Ϊ��
     Exponent: Integer                    - ָ��
     Mantissa: TUInt64                    - ��Ч���֣�64 λȫ��Ч
     ValueAddr: Pointer                   - ������ϵ���չ���ȸ������ĵ�ַ
     ExtendedSize: Integer                - ����չ���ȵĴ�С��ֻ֧�� 8��10��16 ����ֵ

   ����ֵ�����ޣ�
}

procedure CombineFloatQuadruple(SignNegative: Boolean; Exponent: Integer;
  MantissaLo: TUInt64; MantissaHi: TUInt64; var Value: Extended);
{* �ѷ���λ��ָ������Ч����ƴ����չ���ȸ�������ֻ�� Extended Ϊ 16 �ֽ�
   �Ҹ�ʽ�� IEEE 754-2008 ����ı����ȸ���ʱ��Ч��Ŀǰ Delphi ��֧�ָø�ʽ����
   Ҫ����Ч����Ϊ���滯�ģ�Ҳ���� 112 λ�����λΪ 1��

   ������
     SignNegative: Boolean                - ����λ��True Ϊ��
     Exponent: Integer                    - ָ��
     MantissaLo: TUInt64                  - ��Ч���ֵ� 64 λ��64 λȫ��Ч
     MantissaHi: TUInt64                  - ��Ч���ָ� 64 λ������ 48 λ��Ч
     var Value: Extended                  - ������ϵ�ʮ���ֽھ��ȸ�����

   ����ֵ�����ޣ�
}

function UInt64ToSingle(U: TUInt64): Single;
{* ���� Int64 �з�������ģ��� 64 λ�޷������͸�ֵ�� Single������ʵ����ͬ��

   ������
     U: TUInt64                           - ����ֵ�� 64 λ�޷�������ֵ

   ����ֵ��Single                         - ���صĵ����ȸ�����
}

function UInt64ToDouble(U: TUInt64): Double;
{* ���� Int64 �з�������ģ��� 64 λ�޷������͸�ֵ�� Double������ʵ����ͬ��

   ������
     U: TUInt64                           - ����ֵ�� 64 λ�޷�������ֵ

   ����ֵ��Double                         - ���ص�˫���ȸ�����
}

function UInt64ToExtended(U: TUInt64): Extended;
{* ���� Int64 �з�������ģ��� 64 λ�޷������͸�ֵ�� Extended������ʵ����ͬ��

   ������
     U: TUInt64                           - ����ֵ�� 64 λ�޷�������ֵ

   ����ֵ��Extended                       - ���ص���չ���ȸ�����
}

function SingleToUInt64(F: Single): TUInt64;
{* �� Single ��ֵ���� Int64 �з�������ģ��� 64 λ�޷������ͣ�����ʵ����ͬ��

   ������
     F: Single                            - ����ֵ�ĵ����ȸ�����

   ����ֵ��TUInt64                        - ���ص� 64 λ�޷�������ֵ
}

function DoubleToUInt64(F: Double): TUInt64;
{* �� Double ��ֵ���� Int64 �з�������ģ��� 64 λ�޷������ͣ�����ʵ����ͬ��

   ������
     F: Double                            - ����ֵ��˫���ȸ�����

   ����ֵ��TUInt64                        - ���ص� 64 λ�޷�������ֵ
}

function ExtendedToUInt64(F: Extended): TUInt64;
{* �� Extended ��ֵ���� Int64 �з�������ģ��� 64 λ�޷������ͣ�����ʵ����ͬ��

   ������
     F: Extended                          - ����ֵ��˫���ȸ�����

   ����ֵ��TUInt64                        - ���ص� 64 λ�޷�������ֵ
}

function SingleIsInfinite(AValue: Single): Boolean;
{* �����ȸ������Ƿ������

   ������
     AValue: Single                       - ���жϵĵ����ȸ�����

   ����ֵ��Boolean                        - �����Ƿ������
}

function DoubleIsInfinite(AValue: Double): Boolean;
{* ˫���ȸ������Ƿ������

   ������
     AValue: Double                       - ���жϵ�˫���ȸ�����

   ����ֵ��Boolean                        - �����Ƿ������
}

function ExtendedIsInfinite(AValue: Extended): Boolean;
{* ��չ���ȸ������Ƿ������

   ������
     AValue: Extended                     - ���жϵ���չ���ȸ�����

   ����ֵ��Boolean                        - �����Ƿ������
}

function SingleIsNan(AValue: Single): Boolean;
{* �����ȸ������Ƿ��ʵ����

   ������
     AValue: Single                       - ���жϵĵ����ȸ�����

   ����ֵ��Boolean                        - �����Ƿ��ʵ��
}

function DoubleIsNan(AValue: Double): Boolean;
{* ˫���ȸ������Ƿ��ʵ����

   ������
     AValue: Double                       - ���жϵ�˫���ȸ�����

   ����ֵ��Boolean                        - �����Ƿ��ʵ��
}

function ExtendedIsNan(AValue: Extended): Boolean;
{* ��չ���ȸ������Ƿ��ʵ����

   ������
     AValue: Extended                     - ���жϵ���չ���ȸ�����

   ����ֵ��Boolean                        - �����Ƿ��ʵ��
}

function ExtendedToStr(AValue: Extended): string;
{* ����չ���ȸ�����ת��Ϊ�ַ�����֧�������ľ��ȡ�
   Delphi Ĭ�� 15 λС�������������� 18��Ҳ��֧�� 1234567899876543.21��

   ������
     AValue: Extended                     - ���жϵ���չ���ȸ�����

   ����ֵ��string                         - ����ת�����
}

// FPC��Windows 64/Linux 64 ��ƽ̨�Լ� Delphi 5��6 ��֧��������������
{$IFDEF WIN32}
{$IFDEF COMPILER7_UP}
{
  �˴�ʵ���������� Extended ����ת��Ϊ�����ˡ�ʮ�������ַ����ĺ�����
  �㷨�Ƕ�ȡ Extended �������ڴ��еĶ��������ݽ���ת�������� Extended ���͵�˵��
  ���Բο��������ϡ�Double �� Single ����Ϊϵͳͨ��֧�ֵĸ������ͣ��� Delphi ���е�
  Extended �ڴ洢��ʽ�����в�ͬ�����߾���β����񻯣��� Double �� Single β��������
  ����Ĭ�ϵ� 1������β������������Ϊ 1.001������ Double �� Single �д洢Ϊ 001����ȥ
  С����ǰ�� 1������ Extended ��洢Ϊ 1001��
  NaN ��Ϊ "not a number"�����Ǹ���������ο� Math.pas ��Ԫ�еĳ��� NaN
  Infinity Ϊ����󣬶���ο� Math.pas ��Ԫ�еĳ��� Infinity �� NegInfinity.
  ����һ�� DecimalExp �� AlwaysUseExponent ������
  ��ʮ���Ƹ�������ת������������ʱ�������ָ����ʽ����ѧ���㷨������Щ���
  Ҳֻ����ָ����ʽ������ 1E-1000������ָ��ʱ�� 0.0000000...0001��ת����ָ������
  ҲӦ������Ӧ���Ʊ�ʾ������ʱ��������ʮ���Ʊ�ʾָ�����֣���������ƴ�
  1.001E101����ֵΪ 100100����ָ����ʮ���Ʊ������һЩ 1.001D5����ʾ��С����
  ���� 5 λ��DecimalExp �����������ָ���Ƿ���ʮ���Ʊ��ָ�����ֵġ�ע�⣬��ʮ����
  ����ʾָ�����޹涨��﷨��������ʹ�� "D" ����ʾ��"E" Ϊ����Ӧ���Ʊ�ʾ�����⣬����
  ʮ�����ƱȽ����⣬"D" �� "E" ��Ϊʮ�����������ַ�������ʮ�����Ʊ��ʱʹ���� "^"
  �ַ���������� 3.BD^D(12)��A.BD^E(ABCE)���粻ϲ�����ָ�ʽ���������޸ġ�
  AlwaysUseExponent ����ָ���Ƿ�һ���ÿ�ѧ������������ 100.111 λ���Ƚ��٣�
  �����Զ��жϲ���Ҫʹ�ÿ�ѧ���������� AlwaysUseExponent Ϊ��ʱ��һ�����Ϊָ��
  ��ʽ 1.00111E2��
  const
    MaxBinDigits = 120;
    MaxHexDigits = 30;
    MaxOctDigits = 40;
  ����������ָ������������λ����������������ʱ����һ��ʹ�ÿ�ѧ��������
}

{ FloatDecimalToBinExtended, FloatDecimalToOctExtended��FloatDecimalToHexExtended
  �������� FloatDecimalToBinaryExtended ���̣�FloatDecimalToBinaryExtended ��������}

function FloatDecimalToBinExtended(fIn: Extended; DecimalExp: Boolean;
  AlwaysUseExponent: Boolean): AnsiString; deprecated; // Convert to binary

function FloatDecimalToOctExtended(fIn: Extended; DecimalExp: Boolean;
  AlwaysUseExponent: Boolean): AnsiString; deprecated; // Convert to octal

function FloatDecimalToHexExtended(fIn: Extended; DecimalExp: Boolean;
  AlwaysUseExponent: Boolean): AnsiString; deprecated; // Convert to hexdecimal

{$ENDIF}
{$ENDIF}

implementation

const
  UINT64_EXTENDED_EXP_MAX = $4040; // UINT64 ���������Ӧ Extended ��������ָ��

resourcestring
  SCN_ERROR_EXTENDED_SIZE = 'Extended Size Error';

type
  TExtendedRec10 = packed record
  {* 10 �ֽڵ���չ���ȸ�������ֻ Win32 ����Ч}
    Mantissa: TUInt64;
    ExpSign: Word;
  end;
  PExtendedRec10 = ^TExtendedRec10;

{$IFDEF WIN32}
{$IFDEF COMPILER7_UP}

type
  PConvertFloatSystem = ^TConvertFloatSystem;
  TConvertFloatSystem = record
    Negative: Boolean;
    ExpFlag, ExponentI: Integer;
  end;

const
  MaxBinDigits = 120;
  MaxHexDigits = 30;
  MaxOctDigits = 40;

function FloatDecimalToBinaryExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean; var ForHexOct: PConvertFloatSystem): AnsiString;
var
  Neg: Boolean;
  i, Flag, IntExp: Integer;
  Exp: AnsiString;
label UseExponent;
begin
{
Extended(32.125) in memory:
0   100000000000100  10000000 10000000 00000000 00000000 00000000 00000000 00000000 00000000
    9      8      7      6      5      4      3   2nd Byte  1stByte   0
sign exponent      digits
0 111111111111111 1000000000000000000000000000000000000000000000000000000000000000  + Inf
1 111111111111111 1000000000000000000000000000000000000000000000000000000000000000  - Inf
1 111111111111111 1100000000000000000000000000000000000000000000000000000000000000  Nan
0 111111111111111 1100000000000000000000000000000000000000000000000000000000000000  -Nan
}
  SetLength(Result, 255);
  SetLength(Exp, 2 * SizeOf(Extended) + 1);
  Neg := False;
  asm
    push EBX
    push ESI
    mov EBX, Result // Address of Result
    mov EBX, [EBX]
    mov EAX, 0
    // Test if fIN equals 0
    lea ESI, fIn[7] // get the first byte of digits
    mov AL, [ESI]
    test AL, 128 // 10000000B
    jz @Zero
    mov ECX, 0
    lea ESI, fIn[8]
    mov AX, [ESI]  // Get first two bytes
    test AX, 32768  // 32768D = 1000000000000000B
    jz @Positive
    mov Neg, 1
    sub AX, 32768 // Sign bit <- 0
  @Positive:
    // Test if fIn is NaN or Infinity
    cmp AX, 32767
    jnz @NotNAN_INF
    mov DL, [ESI - 1]
    test DL, 64  // 01000000B
    jz @INF
    mov Flag, 4  // NaN
    jmp @Done
  @INF:
    mov Flag, 3  // INF
    jmp @Done
  @NotNAN_INF:
    sub AX, 16383 // AX = AX - 011111111111111B
    jns @ExpPositive
    sub AX, 1
    not AX
    mov Flag, 2 // // Exponent sign negative
    jmp @JudgeDecimalExp
  @ExpPositive:
    mov Flag, 1 // Exponent sign positive
  @JudgeDecimalExp:
    mov IntExp, EAX
    cmp DecimalExp, 1
    je @MoveDigits
    // Binary string exponent. Convert AX to binary string and store it in Exp
    lea EBX, Exp
    mov EBX, [EBX]
    push ECX
    mov [EBX], 69 // 'E' // "D" for decimal exponent
    mov ECX, 1
    cmp Flag, 2
    jnz @NoNegativeInExp
    mov [EBX + 1], 45 // '-' // Add a "-" to exponent string
    mov ECX, 2
  @NoNegativeInExp:
    mov ESI, 0 // flag whehter "1" appears
    // Move exponent digits to Exp
    mov DX, 32768 // 1000000000000000
  @NextExpDigit:
    test AX, DX
    jz @AppendExp0
    mov [EBX + ECX], 49 // '1'
    mov ESI, 1
    jmp @NextExpIncECX
  @AppendExp0:
    cmp ESI, 0
    jz @NextExpNoIncECX // do not append this "0"
    mov [EBX + ECX], 48 // '0'
  @NextExpIncECX:
    inc ECX
  @NextExpNoIncECX:
    shr DX, 1
    cmp DX, 0
    jne @NextExpDigit
    pop ECX
    mov EBX, Result
    mov EBX, [EBX]
    jmp @MoveDigits
  @MoveDigits:
    // Move digits to Result
    mov ESI, 8
  @NextByte:
    dec ESI
    mov EAX, EBX
    lea EBX, fIn[ESI]
    mov DL, [EBX]
    mov EBX, EAX
    mov AL, 128 // 10000000
  @NextDigit:
    test DL, AL
    jz @Append0
    mov [EBX + ECX], 49 // '1'
    mov i, ECX
    jmp @Next
  @Append0:
    mov [EBX + ECX], 48 // '0'
  @Next:
    inc ECX
    shr AL, 1
    cmp AL, 0
    jne @NextDigit
    cmp ESI, 0 // if the last byte
    jne @NextByte
    jmp @Done
  @Zero:
    mov Flag, 0
  @Done:
    pop ESI
    pop EBX
  end;
  case Flag of
    0:
    begin
      ForHexOct := nil;
      Result := '0';
      Exit;
    end;
    1, 2:
    begin
      // Delete redundant "0" in Result
      Delete(Result, i + 2, MaxInt); // i stores the position of the last 1 in Result
      if Assigned(ForHexOct) then
      begin
        // Copy to ForHexOct
        with ForHexOct^ do
        begin
          Negative := Neg;
          ExpFlag := Flag;
          ExponentI := IntExp;
        end;
        Exit;
      end;
      // Add dot and exponent to Result
      if (IntExp = 0) then
      begin
        if (Length(Result) > 1) then
          Insert('.', Result, 2);
      end
      else
      begin
        { Decide whether use exponent. For example "1000.101" shouldn't be output
          as 1.000101E11 when AlwaysUseExponent is False. }
        if AlwaysUseExponent then
        begin
UseExponent:
          if DecimalExp then
            if Flag = 1 then
              Exp := 'D' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(IntExp))
            else
              Exp := 'D-' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(IntExp));
          if Length(Result) >=2 then
            Insert('.', Result, 2);
          Result := Result + Exp;
        end
        else
        begin
          // IntExp may be negative.
          if Flag = 1 then
          begin
            // Calculate all digits required without exponent
            if IntExp <= Length(Result) - 2 then
            begin
              // Do not use exponent
              Insert('.', Result, IntExp + 2);
            end
            else if IntExp = Length(Result) - 1 then
              { 1.001, Exp = 3, output 1001  }
            else
            begin
              if IntExp + 1> MaxBinDigits then
                goto UseExponent
              else
              begin
                Inc(IntExp);
                i := Length(Result);
                // Add zeros at tail
                SetLength(Result, IntExp);
                for i := i + 1 to IntExp do
                  Result := '0';
              end;
            end;
          end
          else
          begin
            if IntExp + Length(Result) > MaxBinDigits then
              goto UseExponent
            else
            begin
              // Add leading zeros and place "."
              SetLength(Exp, 1 + IntExp);
              Exp[1] := '0';
              Exp[2] := '.';
              for i := 3 to IntExp + 1 do
                Exp := '0';   //}
              Result := Exp + Result;
            end;
          end;
        end;
      end;
    end;
    3: // INF
    begin
      ForHexOct := nil;
      Result := 'INF';
    end;
    4: // NaN
    begin
      ForHexOct := nil;
      Result := 'NaN';
      Exit;
    end;
  end;
  if Neg then
    Result := '-' + Result;
end;

function FloatDecimalToBinExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
var
  PTmp: PConvertFloatSystem;
begin
  PTmp := nil;
  Result := FloatDecimalToBinaryExtended(fIn, DecimalExp, AlwaysUseExponent, PTmp);
end;

function FloatDecimalToHexExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
const
  DecToHex: array[0..15] of  AnsiChar =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  BinPow: array[0..3] of Integer = (8, 4, 2, 1);

  function IntToHex(Int: Integer): AnsiString;
  var
    k ,t: Integer;
    Buf: array[1..5] of AnsiChar;
  begin
    k := 1;
    while (Int <> 0) do
    begin
      Buf[k] := DecToHex[Int mod 16];
      Inc(k);
      Int := Int div 16;
    end;
    Dec(k);
    SetLength(Result, k);
    t := 1;
    while (k > 0) do
    begin
      Result[t] := Buf[k];
      Inc(t);
      Dec(k);
    end;
  end;

  function ToHex(const S: AnsiString; LeftToDot: Boolean): AnsiString;
  var
    i, l, t, m, k: Integer;
    Buf: array[1..20] of AnsiChar;
  begin
    { LeftToDot = True, S will be patched with zeroes on its left side.
      For example, S = '110', after patching, S = '0110'.
      LeftToDot = False, S will be patched with zeroes on its right side.
      S = '110', after patching, S = '1100'. }
    l := Length(S);
    if LeftToDot then
      t := (4 - (l mod 4)) mod 4
    else
      t := 0;
    i := 1;
    m := 1;
    k := 0;
    while i <= l do
    begin
      k := k + BinPow[t] * (Ord(S[i]) - Ord('0'));
      Inc(t);
      if (t = 4) or (i = l) then
      begin
        Buf[m] := DecToHex[k];
        Inc(m);
        k := 0;
        t := 0;
      end;
      Inc(i);
    end;
    Dec(m);
    SetLength(Result, m);

    while (m > 0) do
    begin
      Result[m] := Buf[m];
      Dec(m);
    end;
  end;

var
  PConvertData: PConvertFloatSystem;
  ConvertData: TConvertFloatSystem;
  tmpS: AnsiString;
  k, t, i, m: Integer;
label UseExponent;
begin
  PConvertData := @ConvertData;
  Result := FloatDecimalToBinaryExtended(fIn, True, True, PConvertData);
  // See FloatDecimalToBinaryExtended, PConvertData is set to nil when result is definite.
  if PConvertData = nil then
    Exit;
  with ConvertData do
  begin
    {  3.BD^D(12)
      A.BD^E(ABCE)
      AB.FFFF }
    k := Length(Result) - 1;
    if AlwaysUseExponent then
    begin
UseExponent:
      { Algorithm:
          X.XXXXXXXX^Y  Shift Count     Exp
          1.00000001^0 = 1.00000001 = 1.01^0  (16)
          1.00000001^1 = 10.0000001 = 2.02^0  (16)
          1.00000001^2 = 100.000001 = 4.04^0  (16)
          1.00000001^3 = 1000.00001 = 8.08^0  (16)
          1.00000001^4 = 1.00000001^100 = 1.01^1  (16)
          1.00000001^5 = 10.0000001^100 = 2.02^1  (16)
          Shift Count = Y mod 4
          Exp = Y div 4
          X.XXXXXXXXX^Y  Y < 0                      Exp
          1.00000001^-1 = 0.100000001 = 1000.00001^-100 = 8.08^-1
          1.00000001^-2 = 0.0100000001 = 100.000001^-100 = 4.04^-1
          1.00000001^-3 = 0.00100000001 = 10.0000001^-100 = 2.02^-1
          1.00000001^-4 = 0.000100000001 = 1.00000001^-100 = 1.01^-1
          1.00000001^-5 = 0.0000100000001 = 1000.00001^-100 = 8.08^-2
          Shift Count = 4 - (Abs(Y) mod 4)
          Exp = -(Abs(Y) div 4 + 1)      }
      if ExpFlag = 1 then
      begin
        t := ExponentI div 4; // Exp
        i := ExponentI mod 4; // Shift Count
      end
      else
      begin
        t := -((ExponentI - 1) div 4 + 1); // Exp
        i := (4 - (ExponentI mod 4)) mod 4; // Shift Count
      end;
      // Get hex digits
      if k < i then
      begin
        // Add extra zeroes
        SetLength(Result, i + 1);
        for m := k + 2 to i + 1 do
          Result[m] := '0';
        Result := ToHex(Result, True);
      end
      else if k = i then
        Result := ToHex(Result, True)
      else
      begin
        tmpS := Copy(Result, 1, i + 1);
        Delete(Result, 1, i + 1);
        Result := ToHex(tmpS, True) + '.' + ToHex(Result, False);
      end;
      if t <> 0 then
      begin
        // Format exponent
        if DecimalExp then
          Result := Result + '^D(' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(t)) + ')'
        else
        begin
          if ExpFlag = 1 then
            Result := Result + '^E(' + IntToHex(t) + ')'
          else // t < 0
            Result := Result + '^E(-' + IntToHex(-t) + ')';
        end;
      end;
    end
    else
    begin
      {  Always remember that Result equals "XXXXXXXX" not "X.XXXXXXX".
        Judge whether to use exponent:
        There are K "X" after '.', K = Length(Result) - 1, no "." in Result originally.
        X.XXXXXXX^Y  (Binary string, ExponentI = Abs(Y))
        case Y >= 0  (Condition: ExpFlag = 2)
          Y <= K:
            Y+1 binary digits on left side of '.', K-Y digits on right side��
            totally requires ((Y+1 - 1) div 4 + 1) + ((K-Y - 1) div 4 + 1) hex digits
          Y > K:
            Y+1 binary digits on left side, totally ((Y+1 - 1) div 4 + 1) hex digits
        case Y<0  (Condition: ExpFlag = 1) 0.XXXX or 0.000XXXX
            One digit '0' on left side and K+1+Abs(Y)-1 digits on right side,
            totally 1 + ((K+1+Abs(Y)-1-1) div 4 + 1) hex digits.
        Compare hdc = hex digit count with MaxHexDigits. If hdc > MaxHexDigits,
        goto UseExponent. }
      if ExponentI = 0 then
      begin
        if (Length(Result) > 1) then
          Result := '1.' + ToHex(Copy(Result, 2, MaxInt), False);
      end
      else
      begin
        if ExpFlag = 1 then
        begin
          if ExponentI < k then
          begin
            // No possible that "ExponentI div 4 + (k - ExponentI - 1) div 4 + 2" > MaxHexDigits
            tmpS := Copy(Result, 1, ExponentI + 1);
            Delete(Result, 1, ExponentI + 1);
            Result := ToHex(tmpS, True) + '.' + ToHex(Result, False);
          end
          else if ExponentI = k then
            // 1.01^2 = 101, no ".", no extra "0".
            Result := ToHex(Result, True)
          else
          begin
            t := ExponentI div 4 + 1;
            if t > MaxHexDigits then
              goto UseExponent
            else
            begin
              // Append "0" after Result
              Inc(ExponentI);
              // Add '0' to Result
              SetLength(Result, ExponentI);
              for t := k + 2{original Length(Result) + 1} to ExponentI do
                Result[t] := '0';
              Result := ToHex(Result, True);
            end;
          end;
        end
        else
        begin
          // ExpFlag = 2, X.XXXXXXX^Y, Y < 0
          t := 2 + (k + ExponentI - 1) div 4; {1 + ((K+1+Abs(Y)-1-1) div 4 + 1)}
          if t > MaxHexDigits then
            goto UseExponent
          else
          begin
            // Add leading zeroes before Result
            SetLength(tmpS, ExponentI - 1); // tmpS stores extra zeroes
            for t := 1 to ExponentI - 1 do
              tmpS[t] := '0';
            Result := '0.' + ToHex(tmpS + Result, False);
          end;
        end;
      end;
    end;
    if Negative then
      Result := '-' + Result;
  end;
end;

function FloatDecimalToOctExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
const
  DecToOct: array[0..7] of  AnsiChar =
    ('0', '1', '2', '3', '4', '5', '6', '7');
  BinPow: array[0..2] of Integer = (4, 2, 1);

  function IntToOct(Int: Integer): AnsiString;
  var
    k ,t: Integer;
    Buf: array[1..10] of AnsiChar;
  begin
    k := 1;
    while (Int <> 0) do
    begin
      Buf[k] := DecToOct[Int mod 8];
      Inc(k);
      Int := Int div 8;
    end;
    Dec(k);
    SetLength(Result, k);
    t := 1;
    while (k > 0) do
    begin
      Result[t] := Buf[k];
      Inc(t);
      Dec(k);
    end;
  end;

  function ToOct(const S: AnsiString; LeftToDot: Boolean): AnsiString;
  var
    i, l, t, m, k: Integer;
    Buf: array[1..30] of AnsiChar;
  begin
    { LeftToDot = True, S will be patched with zeroes on its left side.
      For example, S = '110', after patching, S = '0110'.
      LeftToDot = False, S will be patched with zeroes on its right side.
      S = '110', after patching, S = '1100'. }
    l := Length(S);
    if LeftToDot then
      t := (3 - (l mod 3)) mod 3
    else
      t := 0;
    i := 1;
    m := 1;
    k := 0;
    while i <= l do
    begin
      k := k + BinPow[t] * (Ord(S[i]) - Ord('0'));
      Inc(t);
      if (t = 3) or (i = l) then
      begin
        Buf[m] := DecToOct[k];
        Inc(m);
        k := 0;
        t := 0;
      end;
      Inc(i);
    end;
    Dec(m);
    SetLength(Result, m);

    while (m > 0) do
    begin
      Result[m] := Buf[m];
      Dec(m);
    end;
  end;

var
  PConvertData: PConvertFloatSystem;
  ConvertData: TConvertFloatSystem;
  tmpS: AnsiString;
  k, t, i, m: Integer;
label UseExponent;
begin
  PConvertData := @ConvertData;
  Result := FloatDecimalToBinaryExtended(fIn, True, True, PConvertData);
  // See FloatDecimalToBinaryExtended, PConvertData is set to nil when result is definite.
  if PConvertData = nil then
    Exit;
  with ConvertData do
  begin
    {  3.333D12  // 12 is decimal
      2.22E33  // 33 is octal}
    k := Length(Result) - 1;
    if AlwaysUseExponent then
    begin
UseExponent:
      if ExpFlag = 1 then
      begin
        t := ExponentI div 3; // Exp
        i := ExponentI mod 3; // Shift Count
      end
      else
      begin
        t := -((ExponentI - 1) div 3 + 1); // Exp
        i := (3 - (ExponentI mod 3)) mod 3; // Shift Count
      end;
      // Get hex digits
      if k < i then
      begin
        // Add extra zeroes
        SetLength(Result, i + 1);
        for m := k + 2 to i + 1 do
          Result[m] := '0';
        Result := ToOct(Result, True);
      end
      else if k = i then
        Result := ToOct(Result, True)
      else
      begin
        tmpS := Copy(Result, 1, i + 1);
        Delete(Result, 1, i + 1);
        Result := ToOct(tmpS, True) + '.' + ToOct(Result, False);
      end;
      if t <> 0 then
      begin
        // Format exponent
        if DecimalExp then
          Result := Result + 'D' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(t))
        else
        begin
          if ExpFlag = 1 then
            Result := Result + 'E' + IntToOct(t)
          else // t < 0
            Result := Result + 'E-' + IntToOct(-t);
        end;
      end;
    end
    else
    begin
      if ExponentI = 0 then
      begin
        if (Length(Result) > 1) then
          Result := '1.' + ToOct(Copy(Result, 2, MaxInt), False);
      end
      else
      begin
        if ExpFlag = 1 then
        begin
          if ExponentI < k then
          begin
            tmpS := Copy(Result, 1, ExponentI + 1);
            Delete(Result, 1, ExponentI + 1);
            Result := ToOct(tmpS, True) + '.' + ToOct(Result, False);
          end
          else if ExponentI = k then
            // 1.01^2 = 101, no ".", no extra "0".
            Result := ToOct(Result, True)
          else
          begin
            t := ExponentI div 3 + 1;
            if t > MaxHexDigits then
              goto UseExponent
            else
            begin
              // Append "0" after Result
              Inc(ExponentI);
              // Add '0' to Result
              SetLength(Result, ExponentI);
              for t := k + 2{original Length(Result) + 1} to ExponentI do
                Result[t] := '0';
              Result := ToOct(Result, True);
            end;
          end;
        end
        else
        begin
          // ExpFlag = 2, X.XXXXXXX^Y, Y < 0
          t := 2 + (k + ExponentI - 1) div 3;
          if t > MaxHexDigits then
            goto UseExponent
          else
          begin
            // Add leading zeroes before Result
            SetLength(tmpS, ExponentI - 1); // tmpS stores extra zeroes
            for t := 1 to ExponentI - 1 do
              tmpS[t] := '0';
            Result := '0.' + ToOct(tmpS + Result, False);
          end;
        end;
      end;
    end;
    if Negative then
      Result := '-' + Result;
  end;
end;

{$ENDIF}
{$ENDIF}

procedure ExtractFloatSingle(Value: Single; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: Cardinal);
begin
  SignNegative := (PCardinal(@Value)^ and CN_SIGN_SINGLE_MASK) <> 0;
  Exponent := ((PCardinal(@Value)^ and CN_EXPONENT_SINGLE_MASK) shr 23) - CN_EXPONENT_OFFSET_SINGLE;
  Mantissa := PCardinal(@Value)^ and CN_SIGNIFICAND_SINGLE_MASK;
  Mantissa := Mantissa or (1 shl 23); // ��λ�ټӸ� 1
end;

procedure ExtractFloatDouble(Value: Double; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64);
begin
  SignNegative := (PUInt64(@Value)^ and CN_SIGN_DOUBLE_MASK) <> 0;
  Exponent := ((PUInt64(@Value)^ and CN_EXPONENT_DOUBLE_MASK) shr 52) - CN_EXPONENT_OFFSET_DOUBLE;
  Mantissa := PUInt64(@Value)^ and CN_SIGNIFICAND_DOUBLE_MASK;
  Mantissa := Mantissa or (TUInt64(1) shl 52); // ��λ�ټӸ� 1
end;

procedure ExtractFloatExtended(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64);
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
  begin
    SignNegative := (PExtendedRec10(@Value)^.ExpSign and CN_SIGN_EXTENDED_MASK) <> 0;
    Exponent := (PExtendedRec10(@Value)^.ExpSign and CN_EXPONENT_EXTENDED_MASK) - CN_EXPONENT_OFFSET_EXTENDED;
    Mantissa := PExtendedRec10(@Value)^.Mantissa; // �� 1�����ü���
  end
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    ExtractFloatDouble(Value, SignNegative, Exponent, Mantissa)
  else
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);
end;

procedure ExtractFloatExtended(ValueAddr: Pointer; ExtendedSize: Integer;
  out SignNegative: Boolean; out Exponent: Integer; out Mantissa: TUInt64);
var
  D: Double;
begin
  if (ExtendedSize = CN_EXTENDED_SIZE_10) or (ExtendedSize = CN_EXTENDED_SIZE_16) then
  begin
    SignNegative := (PExtendedRec10(ValueAddr)^.ExpSign and CN_SIGN_EXTENDED_MASK) <> 0;
    Exponent := (PExtendedRec10(ValueAddr)^.ExpSign and CN_EXPONENT_EXTENDED_MASK) - CN_EXPONENT_OFFSET_EXTENDED;
    Mantissa := PExtendedRec10(ValueAddr)^.Mantissa; // �� 1�����ü���
  end
  else if ExtendedSize = CN_EXTENDED_SIZE_8 then
  begin
    Move(ValueAddr^, D, SizeOf(Double));
    ExtractFloatDouble(D, SignNegative, Exponent, Mantissa);
  end
  else
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);
end;

procedure ExtractFloatQuadruple(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out MantissaLo, MantissaHi: TUInt64);
begin
  if SizeOf(Extended) <> CN_EXTENDED_SIZE_16 then
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);

  SignNegative := (PCnQuadruple(@Value)^.W1 and CN_SIGN_QUADRUPLE_MASK) <> 0;
  Exponent := (PCnQuadruple(@Value)^.W1 and CN_EXPONENT_QUADRUPLE_MASK) - CN_EXPONENT_OFFSET_EXTENDED;

  // Extract 16 Bytes to Mantissas
  MantissaLo := PCnQuadruple(@Value)^.Lo;
  MantissaHi := TUInt64(PCnQuadruple(@Value)^.Hi0) or (TUInt64(PCnQuadruple(@Value)^.W0) shl 32) or (TUInt64(1) shl 48); // ��λ�ټӸ� 1
end;

procedure CombineFloatSingle(SignNegative: Boolean; Exponent: Integer;
  Mantissa: Cardinal; var Value: Single);
begin
  Mantissa := Mantissa and not (1 shl 23); // ȥ�� 23 λ�ϵ� 1������еĻ�
  PCardinal(@Value)^ := Mantissa and CN_SIGNIFICAND_SINGLE_MASK;
  Inc(Exponent, CN_EXPONENT_OFFSET_SINGLE);

  PCardinal(@Value)^ := PCardinal(@Value)^ or (LongWord(Exponent) shl 23);
  if SignNegative then
    PCardinal(@Value)^ := PCardinal(@Value)^ or CN_SIGN_SINGLE_MASK
  else
    PCardinal(@Value)^ := PCardinal(@Value)^ and not CN_SIGN_SINGLE_MASK;
end;

procedure CombineFloatDouble(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Double);
begin
  Mantissa := Mantissa and not (TUInt64(1) shl 52); // ȥ�� 52 λ�ϵ� 1������еĻ�
  PUInt64(@Value)^ := Mantissa and CN_SIGNIFICAND_DOUBLE_MASK;
  Inc(Exponent, CN_EXPONENT_OFFSET_DOUBLE);

  PUInt64(@Value)^ := PUInt64(@Value)^ or (TUInt64(Exponent) shl 52);
  if SignNegative then
    PUInt64(@Value)^ := PUInt64(@Value)^ or CN_SIGN_DOUBLE_MASK
  else
    PUInt64(@Value)^ := PUInt64(@Value)^ and not CN_SIGN_DOUBLE_MASK;
end;

{$HINTS OFF}

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Extended);
var
  D: Double;
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
  begin
    PExtendedRec10(@Value)^.Mantissa := Mantissa;
    Inc(Exponent, CN_EXPONENT_OFFSET_EXTENDED);

    PExtendedRec10(@Value)^.ExpSign := Exponent and CN_EXPONENT_EXTENDED_MASK;
    if SignNegative then
      PExtendedRec10(@Value)^.ExpSign := PExtendedRec10(@Value)^.ExpSign or CN_SIGN_EXTENDED_MASK
    else
      PExtendedRec10(@Value)^.ExpSign := PExtendedRec10(@Value)^.ExpSign and not CN_SIGN_EXTENDED_MASK;
  end
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
  begin
    CombineFloatDouble(SignNegative, Exponent, Mantissa, D);
    Value := D;
  end
  else
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);
end;

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; ValueAddr: Pointer; ExtendedSize: Integer);
var
  D: Double;
begin
  if (ExtendedSize = CN_EXTENDED_SIZE_10) or (ExtendedSize = CN_EXTENDED_SIZE_16) then
  begin
    PExtendedRec10(ValueAddr)^.Mantissa := Mantissa;
    Inc(Exponent, CN_EXPONENT_OFFSET_EXTENDED);

    PExtendedRec10(ValueAddr)^.ExpSign := Exponent and CN_EXPONENT_EXTENDED_MASK;
    if SignNegative then
      PExtendedRec10(ValueAddr)^.ExpSign := PExtendedRec10(ValueAddr)^.ExpSign or CN_SIGN_EXTENDED_MASK
    else
      PExtendedRec10(ValueAddr)^.ExpSign := PExtendedRec10(ValueAddr)^.ExpSign and not CN_SIGN_EXTENDED_MASK;
  end
  else if ExtendedSize = CN_EXTENDED_SIZE_8 then
  begin
    CombineFloatDouble(SignNegative, Exponent, Mantissa, D);
    Move(D, ValueAddr^, SizeOf(Double));
  end
  else
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);
end;

{$HINTS ON}

procedure CombineFloatQuadruple(SignNegative: Boolean; Exponent: Integer;
  MantissaLo, MantissaHi: TUInt64; var Value: Extended);
begin
  if SizeOf(Extended) <> CN_EXTENDED_SIZE_16 then
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);

  MantissaHi := MantissaHi and not (TUInt64(1) shl 48); // ȥ�� 112 λ�ϵ� 1������еĻ�
  PCnQuadruple(@Value)^.Lo := MantissaLo;
  PCnQuadruple(@Value)^.Hi0 := Cardinal(MantissaHi and $FFFFFFFF);
  PCnQuadruple(@Value)^.Hi1 := (MantissaHi shr 32) and CN_SIGNIFICAND_QUADRUPLE_MASK;

  Inc(Exponent, CN_EXPONENT_OFFSET_EXTENDED);
  PCnQuadruple(@Value)^.W1 := Exponent and CN_EXPONENT_QUADRUPLE_MASK;
  if SignNegative then
    PCnQuadruple(@Value)^.Hi1 := PCnQuadruple(@Value)^.Hi1 or CN_SIGN_QUADRUPLE_MASK
  else
    PCnQuadruple(@Value)^.Hi1 := PCnQuadruple(@Value)^.Hi1 and not CN_SIGN_QUADRUPLE_MASK;
end;

// �� UInt64 ��Ϊ������
function UFloat(U: TUInt64): Extended;
{$IFNDEF SUPPORT_UINT64}
var
  L, H: Cardinal;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  Result := U;
{$ELSE}
  if U < 0 then // Int64 С�� 0 ʱ������� UInt64 �Ǵ��� Int64 �����ֵ��
  begin
    H := Int64Rec(U).Hi;
    L := Int64Rec(U).Lo;
    Result := Int64(H) * Int64(CN_MAX_UINT16 + 1); // ��������
    Result := Result * (CN_MAX_UINT16 + 1);
    Result := Result + L;
  end
  else
    Result := U;
{$ENDIF}
end;

function UInt64ToSingle(U: TUInt64): Single;
begin
  Result := UFloat(U);
end;

function UInt64ToDouble(U: TUInt64): Double;
begin
  Result := UFloat(U);
end;

function UInt64ToExtended(U: TUInt64): Extended;
begin
  Result := UFloat(U);
end;

// ��ͨ Trunc ���������ֻ�ܷ��� Int64��������������� UInt64
function UTrunc(F: Extended): TUInt64;
var
  T: Integer;
  SignNeg: Boolean;
  Exponent: Integer;
  Mantissa: TUInt64;
begin
  // �õ���ʵָ���� 1 ��ͷ����Ч���֣�С������ 1 ��
  ExtractFloatExtended(F, SignNeg, Exponent, Mantissa);
  if SignNeg then
    raise ERangeError.Create(SRangeError); // ������֧��

  // Mantissa �� 64 λ��Ч���֣�����С����� 63 λ�����ָ��С�� 0 ˵��С����Ҫ�����ƣ���ôֵ���� 0 ��
  if Exponent < 0 then
    Result := 0
  else
  begin
    // ��С���������� Exponent λ��С������ߵ�����������
    T := 63 - Exponent;    // С������ 0 �� 63 λ�� 63 λ�ұߣ�С�������ƺ��� T λ�ұ�
    if T < 0 then
      raise ERangeError.Create(SRangeError); // Exponent ̫��

    Result := Mantissa shr T;
  end;
end;

function SingleToUInt64(F: Single): TUInt64;
begin
  Result := UTrunc(F);
end;

function DoubleToUInt64(F: Double): TUInt64;
begin
  Result := UTrunc(F);
end;

function ExtendedToUInt64(F: Extended): TUInt64;
begin
  Result := UTrunc(F);
end;

function SingleIsInfinite(AValue: Single): Boolean;
begin
  Result := ((PCardinal(@AValue)^ and $7F800000) = $7F800000) and
            ((PCardinal(@AValue)^ and $007FFFFF) = $00000000);
end;

function DoubleIsInfinite(AValue: Double): Boolean;
begin
  Result := ((PUInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PUInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000);
end;

function ExtendedIsInfinite(AValue: Extended): Boolean;
begin
  if SizeOf(Extended) = CN_EXTENDED_SIZE_10 then
    Result := ((PExtendedRec10(@AValue)^.ExpSign and $7FFF) = $7FFF) and
              ((PExtendedRec10(@AValue)^.Mantissa) = 0)
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    Result := DoubleIsInfinite(AValue)
  else
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);
end;

function SingleIsNan(AValue: Single): Boolean;
begin
  Result := ((PCardinal(@AValue)^ and $7F800000)  = $7F800000) and
            ((PCardinal(@AValue)^ and $007FFFFF) <> $00000000);
end;

function DoubleIsNan(AValue: Double): Boolean;
begin
  Result := ((PUInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PUInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;

function ExtendedIsNan(AValue: Extended): Boolean;
begin
  if SizeOf(Extended) = CN_EXTENDED_SIZE_10 then
    Result := ((PExtendedRec10(@AValue)^.ExpSign and $7FFF)  = $7FFF) and
              ((PExtendedRec10(@AValue)^.Mantissa and $7FFFFFFFFFFFFFFF) <> 0)
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    Result := DoubleIsNan(AValue)
  else
    raise ECnFloatSizeError.Create(SCN_ERROR_EXTENDED_SIZE);
end;

function ExtendedToStr(AValue: Extended): string;
var
  Buffer: array[0..63] of Char;
begin
  SetString(Result, Buffer, FloatToText(Buffer, AValue, {$IFNDEF FPC} fvExtended, {$ENDIF}
     ffGeneral,  18, 0)); // �ڲ���������� 18
end;

end.
