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

unit CnBigDecimal;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��󸡵����㷨��Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע��TCnBigDecimal �� TCnBigBinary ���� CnBigNumber ��ʾ��Ч���֣�
*           �� Integer ��ʾ����ָ��������ͬ���ǵ׷ֱ�Ϊ 10 �� 2
*           ���ֲο� Rudy Velthuis �� BigDecimal �Լ� Java �� BigDecimal
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2024.12.05 V1.3
*               ȥ��һ�����õ� const ����������ע��
*           2021.12.05 V1.3
*               ������ TCnBigRational ����ת���ĺ����Լ���ƽ�����ĺ���
*           2021.09.05 V1.2
*               ����һ�� TCnBigBinary ��������
*           2020.07.08 V1.1
*               ʵ�ֻ��ڶ����Ƶĸ����� TCnBigBinary
*           2020.06.25 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst,
  CnNative, CnFloat, CnContainers, CnBigRational, CnBigNumber;

const
  CN_BIG_DECIMAL_DEFAULT_PRECISION = 12;
  {* ��ʮ���Ƹ������˳�����С������Ĭ�Ͼ���}

  CN_BIG_BINARY_DEFAULT_PRECISION  = 32;
  {* ������Ƹ�����С������Ĭ�Ͼ���}

  CN_BIG_DECIMAL_DEFAULT_DIGITS    = 20;
  {* ��ʮ���Ƹ�����ת��ΪС��ʱĬ�ϱ�����λ��}

  CN_SQRT_DEFAULT_ROUND_COUNT      = 10;
  {* ��ʮ���Ƹ�������ƽ����ʱĬ�ϵ����Ĵ���}

type
  ECnBigDecimalException = class(Exception);
  {* ��ʮ���Ƹ���������쳣}

  TCnBigRoundMode = (
  {* ��ʮ���Ƹ�����ȡ����ģʽ��ʮ���ư������֣�����������������ɵ����������󣬶����ư���ǰ����
     ע�⣺�����������ֻ����������ֵ��������û������������������Ϊ�ᶯ����Ȼ��������ֵС����ȡ}
    drAwayFromZero,
    {* ������ֵ�����ȡ}
    drTowardsZero,
    {* ������ֵС����ȡ������ֻ���������ֵ� Trunc}
    drCeilingToInfinite,
    {* ���������ȡ}
    drFloorToNegInfinite,
    {* ���������ȡ}
    drRound,
    {* �������루������ģʽ���� 0 �� 1 �룩����������ֵ�����}
    dr465RoundEven
    {* �����������˫����֧�ֶ�����ģʽ������������ֵ�����}
  );

  TCnBigDecimal = class
  {* ��ʮ���Ƹ�����ʵ���࣬�� CnBigNumber ������Ч���֣��� Integer ����ָ��Ҳ����С����λ��
     FScale ����С��������Ч�������ұߵ�λ�ã�����Ϊ��������Ϊ����
     ��ʱ�����֮����С������� FScale λ����ʱ�����֮��Ҫ�� -FScale �� 0}
  private
    FValue: TCnBigNumber;
    FScale: Integer;                 // ��ȷֵΪ FValue / (10^FScale)
    function GetDecString: string;
    function GetDebugDump: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetZero;
    {* ���ó� 0}
    procedure SetOne;
    {* ���ó� 1}
    procedure SetNegative(Neg: Boolean);
    {* �����Ƿ�����

       ������
         Neg: Boolean                     - �����Ƿ���

       ����ֵ�����ޣ�
    }

    procedure Negate;
    {* ����Ϊ�෴��}

    function SetWord(W: Cardinal): Boolean;
    {* ����Ϊһ�� 32 λ�޷���������

       ������
         W: Cardinal                      - �����õ� 32 λ�޷�������

       ����ֵ��Boolean                    - ���������Ƿ�ɹ�
    }

    function SetInt64(W: Int64): Boolean;
    {* ����Ϊһ�� 64 λ�з���������

       ������
         W: Int64                         - �����õ� 64 λ�з�������

       ����ֵ��Boolean                    - ���������Ƿ�ɹ�
    }

    function SetDec(const Buf: string): Boolean;
    {* ����Ϊ�ַ���ֵ��

       ������
         const Buf: string                - �����õ��ַ���ֵ

       ����ֵ��Boolean                    - ���������Ƿ�ɹ�
    }

    procedure SetSingle(Value: Single);
    {* ����Ϊ�����ȸ���ֵ��

       ������
         Value: Single                    - �����õĵ����ȸ���ֵ

       ����ֵ�����ޣ�
    }

    procedure SetDouble(Value: Double);
    {* ����Ϊ˫���ȸ���ֵ��

       ������
         Value: Double                    - �����õ�˫���ȸ���ֵ

       ����ֵ�����ޣ�
    }

    procedure SetExtended(Value: Extended);
    {* ����Ϊ��չ���ȸ���ֵ��

       ������
         Value: Extended                  - �����õ���չ���ȸ���ֵ

       ����ֵ�����ޣ�
    }

    procedure AddWord(W: Cardinal);
    {* ����һ��������

       ������
         W: Cardinal                      - ����

       ����ֵ�����ޣ�
    }

    procedure SubWord(W: Cardinal);
    {* ��ȥһ��������

       ������
         W: Cardinal                      - ����

       ����ֵ�����ޣ�
    }

    procedure MulWord(W: Cardinal);
    {* ����һ��������

       ������
         W: Cardinal                      - ����

       ����ֵ�����ޣ�
    }

    procedure DivWord(W: Cardinal; DivPrecision: Integer = 0);
    {* ����һ��������DivPrecision ��ʾ����������ౣ��С�����λ��0 ��ʾ��Ĭ����������

       ������
         W: Cardinal                      - ����
         DivPrecision: Integer            - ����С��������λ��0 ��ʾ��Ĭ�Ͼ��ȴ���

       ����ֵ�����ޣ�
    }

    function IsNegative: Boolean;
    {* �Ƿ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ���
    }

    function IsZero: Boolean;
    {* �Ƿ�Ϊ 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 0
    }

    function IsOne: Boolean;
    {* �Ƿ�Ϊ 1��ֻ�ж�ֵ�� 1 ��ָ���� 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 1
    }

    procedure RoundTo(Precision: Integer; RoundMode: TCnBigRoundMode = dr465RoundEven);
    {* ������ָ��С��λ������ԭ��С��λ������ Precision �򲻶���

       ������
         Precision: Integer               - ָ��С��λ��
         RoundMode: TCnBigRoundMode       - ����Ĺ���

       ����ֵ�����ޣ�
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ����ʮ���Ƹ�����ת���ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ����ת�����ַ���
    }

    property DecString: string read GetDecString;
    property DebugDump: string read GetDebugDump;
  end;

  TCnBigDecimalPool = class(TCnMathObjectPool)
  {* ��ʮ���Ƹ�������ʵ���࣬����ʹ�õ���ʮ���Ƹ������ĵط����д�����ʮ���Ƹ�������}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigDecimal;
    {* �Ӷ���ػ�ȡһ�����󣬲���ʱ����� Recycle �黹��

       ������
         Num: TCnBigDecimal               - ���黹�����еĶ���

       ����ֵ�����ޣ�
    }
    procedure Recycle(Num: TCnBigDecimal);
    {* ��һ������黹������ء�

       ������
         Num: TCnBigDecimal               - ���黹�����еĶ���

       ����ֵ�����ޣ�
    }
  end;

  ECnBigBinaryException = class(Exception);
  {* ������Ƹ���������쳣}

  TCnBigBinary = class
  {* ������Ƹ�����ʵ���࣬�� CnBigNumber ������Ч���֣��� Integer ������� 2 ��ָ��
     FScale ���������ģʽ��С��������Ч�������ұߵ�λ�ã�����Ϊ��������Ϊ����
     ��ʱ�����֮���Ƕ�����ģʽ��С������� FScale λ����ʱ�����֮��Ҫ�� -FScale �� 0}
  private
    FValue: TCnBigNumber;
    FScale: Integer;                 // ��ȷֵΪ FValue / (2^FScale)��Ĭ�� FScale Ϊ 0��Ҳ���ǳ��� 1�����ڲ���
    function GetDebugDump: string;
    function GetDecString: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetZero;
    {* ���ó� 0}
    procedure SetOne;
    {* ���ó� 1}
    procedure SetNegative(Neg: Boolean);
    {* �����Ƿ�����

       ������
         Neg: Boolean                     - �����Ƿ���

       ����ֵ�����ޣ�
    }

    procedure Negate;
    {* ����Ϊ�෴��}

    function SetWord(W: Cardinal): Boolean;
    {* ����Ϊһ�� 32 λ�޷���������

       ������
         W: Cardinal                      - �����õ� 32 λ�޷�������

       ����ֵ��Boolean                    - ���������Ƿ�ɹ�
    }

    function SetInt64(W: Int64): Boolean;
    {* ����Ϊһ�� 64 λ�з���������

       ������
         W: Int64                         - �����õ� 64 λ�з�������

       ����ֵ��Boolean                    - ���������Ƿ�ɹ�
    }

    function SetDec(const Buf: string): Boolean;
    {* �����ַ���ֵ��

       ������
         const Buf: string                - �����õ��ַ���ֵ

       ����ֵ��Boolean                    - ���������Ƿ�ɹ�
    }

    procedure SetSingle(Value: Single);
    {* ����Ϊ�����ȸ���ֵ��

       ������
         Value: Single                    - �����õĵ����ȸ���ֵ

       ����ֵ�����ޣ�
    }

    procedure SetDouble(Value: Double);
    {* ����Ϊ˫���ȸ���ֵ��

       ������
         Value: Double                    - �����õ�˫���ȸ���ֵ

       ����ֵ�����ޣ�
    }

    procedure SetExtended(Value: Extended);
    {* ����Ϊ��չ���ȸ���ֵ��

       ������
         Value: Extended                  - �����õ���չ���ȸ���ֵ

       ����ֵ�����ޣ�
    }

    procedure SetBigNumber(Value: TCnBigNumber);
    {* ����Ϊ������ֵ��

       ������
         Value: TCnBigNumber              - �����õĴ�����ֵ

       ����ֵ�����ޣ�
    }

    procedure AddWord(W: Cardinal);
    {* ����һ��������

       ������
         W: Cardinal                      - ����

       ����ֵ�����ޣ�
    }

    procedure SubWord(W: Cardinal);
    {* ��ȥһ��������

       ������
         W: Cardinal                      - ����

       ����ֵ�����ޣ�
    }

    procedure MulWord(W: Cardinal);
    {* ����һ��������

       ������
         W: Cardinal                      - ����

       ����ֵ�����ޣ�
    }

    procedure DivWord(W: Cardinal; DivPrecision: Integer = 0);
    {* ����һ��������DivPrecision ��ʾ����������ౣ��С�����λ��0 ��ʾ��Ĭ����������

       ������
         W: Cardinal                      - ����
         DivPrecision: Integer            - ����С��������λ��0 ��ʾ��Ĭ�Ͼ��ȴ���

       ����ֵ�����ޣ�
    }

    procedure ShiftLeft(N: Integer);
    {* ���� N λ��

       ������
         N: Integer                       - �����Ƶ�λ��

       ����ֵ�����ޣ�
    }

    procedure ShiftRight(N: Integer);
    {* ���� N λ��

       ������
         N: Integer                       - �����Ƶ�λ��

       ����ֵ�����ޣ�
    }

    procedure Power(N: Integer);
    {* ���ݡ�

       ������
         N: Integer                       - ��ָ��

       ����ֵ�����ޣ�
    }

    function IsNegative: Boolean;
    {* �Ƿ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ���
    }

    function IsZero: Boolean;
    {* �Ƿ�Ϊ 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �Ƿ�Ϊ 0
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ����ʮ���Ƹ�����ת��Ϊ�ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ����ת�����ַ���
    }

    property DecString: string read GetDecString;
    property DebugDump: string read GetDebugDump;
  end;

  TCnBigBinaryPool = class(TCnMathObjectPool)
  {* ������Ƹ�������ʵ���࣬����ʹ�õ�������Ƹ������ĵط����д���������Ƹ�������}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigBinary; reintroduce;
    {* �Ӷ���ػ�ȡһ�����󣬲���ʱ����� Recycle �黹��

       ������
         ���ޣ�

       ����ֵ��TCnBigBinary               - ���س��еĴ�����Ƹ���������
    }

    procedure Recycle(Num: TCnBigBinary); reintroduce;
    {* ��һ������黹������ء�

       ������
         Num: TCnBigBinary                - ���黹�����еĶ���

       ����ֵ�����ޣ�
    }
  end;

// ====================== ��ʮ���Ƹ������������� ===============================

procedure BigDecimalClear(Num: TCnBigDecimal);
{* ���һ����ʮ���Ƹ���������ʵ������ Value �� Scale ������Ϊ 0��

   ������
     Num: TCnBigDecimal                   - ����յĴ�ʮ���Ƹ�����

   ����ֵ�����ޣ�
}

function BigDecimalSetDec(const Buf: string; Res: TCnBigDecimal): Boolean;
{* Ϊ��ʮ���Ƹ��������������ַ���ֵ��

   ������
     const Buf: string                    - �����õ��ַ���
     Res: TCnBigDecimal                   - �����õĴ�ʮ���Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigDecimalSetWord(W: Cardinal; Res: TCnBigDecimal): Boolean;
{* Ϊ��ʮ���Ƹ������������� 32 λ�޷�������ֵ��

   ������
     W: Cardinal                          - �����õ� 32 λ�޷�������ֵ
     Res: TCnBigDecimal                   - �����õĴ�ʮ���Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigDecimalSetInt64(W: Int64; Res: TCnBigDecimal): Boolean;
{* Ϊ��ʮ���Ƹ������������� 64 λ�з�������ֵ��

   ������
     W: Int64                             - �����õ� 64 λ�з�������ֵ
     Res: TCnBigDecimal                   - �����õĴ�ʮ���Ƹ�����

   ����ֵ��Boolean                        -
}

function BigDecimalSetSingle(Value: Single; Res: TCnBigDecimal): Boolean;
{* Ϊ��ʮ���Ƹ������������õ����ȸ���ֵ��

   ������
     Value: Single                        - �����õĵ����ȸ���ֵ
     Res: TCnBigDecimal                   - �����õĴ�ʮ���Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigDecimalSetDouble(Value: Double; Res: TCnBigDecimal): Boolean;
{* Ϊ��ʮ���Ƹ�������������˫���ȸ���ֵ��

   ������
     Value: Double                        - �����õ�˫���ȸ���ֵ
     Res: TCnBigDecimal                   - �����õĴ�ʮ���Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigDecimalSetExtended(Value: Extended; Res: TCnBigDecimal): Boolean;
{* Ϊ��ʮ���Ƹ���������������չ���ȸ���ֵ��

   ������
     Value: Extended                      - �����õ���չ���ȸ���ֵ
     Res: TCnBigDecimal                   - �����õĴ�ʮ���Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigDecimalToString(Num: TCnBigDecimal): string;
{* ��ʮ���Ƹ���������ת��Ϊ�ַ�����

   ������
     Num: TCnBigDecimal                   - ��ת���Ĵ�ʮ���Ƹ�����

   ����ֵ��string                         - ����ת�����ַ���
}

function BigDecimalToSingle(Num: TCnBigDecimal): Single;
{* ��ʮ���Ƹ���������ת��Ϊ�����ȸ�������

   ������
     Num: TCnBigDecimal                   - ��ת���Ĵ�ʮ���Ƹ�����

   ����ֵ��Single                         - ���ص����ȸ���ֵ
}

function BigDecimalToDouble(Num: TCnBigDecimal): Double;
{* ��ʮ���Ƹ���������ת��Ϊ˫���ȸ�������

   ������
     Num: TCnBigDecimal                   - ��ת���Ĵ�ʮ���Ƹ�����

   ����ֵ��Double                         - ����˫���ȸ���ֵ
}

function BigDecimalToExtended(Num: TCnBigDecimal): Extended;
{* ��ʮ���Ƹ���������ת��Ϊ��չ���ȸ�������

   ������
     Num: TCnBigDecimal                   - ��ת���Ĵ�ʮ���Ƹ�����

   ����ֵ��Extended                       - ������չ���ȸ���ֵ
}

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: TCnBigDecimal): Integer; overload;
{* �Ƚ�������ʮ���Ƹ��������󣬷ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     Num1: TCnBigDecimal                  - ���ȽϵĴ�ʮ���Ƹ�����һ
     Num2: TCnBigDecimal                  - ���ȽϵĴ�ʮ���Ƹ�������

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: Int64): Integer; overload;
{* �Ƚϴ�ʮ���Ƹ������������������ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     Num1: TCnBigDecimal                  - ���ȽϵĴ�ʮ���Ƹ�����
     Num2: Int64                          - ���Ƚϵ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: Extended): Integer; overload;
{* �Ƚϴ�ʮ���Ƹ����������븡�������ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     Num1: TCnBigDecimal                  - ���ȽϵĴ�ʮ���Ƹ�����
     Num2: Extended                       - ���Ƚϵĸ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

procedure BigDecimalCopy(Dest: TCnBigDecimal; Source: TCnBigDecimal);
{* ��ʮ���Ƹ�������ֵ��

   ������
     Dest: TCnBigDecimal                  - Ŀ���ʮ���Ƹ�����
     Source: TCnBigDecimal                - Դ��ʮ���Ƹ�����

   ����ֵ�����ޣ�
}

function BigDecimalGetPrecision(Num: TCnBigDecimal): Integer;
{* �����ʮ���Ƹ�������ʮ����λ����Ҳ����Ч���ֳ��ȡ�

   ������
     Num: TCnBigDecimal                   - ������Ĵ�ʮ���Ƹ�����

   ����ֵ��Integer                        - ������Ч���ֳ���
}

function BigDecimalGetIntDecimalCount(Num: TCnBigDecimal;
  out IntCount: Integer; out DecimalCount: Integer): Boolean;
{* �����ʮ���Ƹ��������������ֳ�����С�����ֳ��ȡ�

   ������
     Num: TCnBigDecimal                   - ������Ĵ�ʮ���Ƹ�����
     out IntCount: Integer                - �����������ֳ���
     out DecimalCount: Integer            - ����С�����ֳ���

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function BigDecimalGetHighScale(Num: TCnBigDecimal): Integer;
{* �����ʮ���Ƹ������������Ч����λ��С�����ڼ�λ���������С�� 0�����󸺺��ʾ��С����ǰ�ڼ�λ��

   ������
     Num: TCnBigDecimal                   - ������Ĵ�ʮ���Ƹ�����

   ����ֵ��Integer                        - ���������Ч����λ��С�����λ������
}

function BigDecimalAdd(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal): Boolean;
{* ��ʮ���Ƹ������ӣ�Res ������ Num1 �� Num2��Num1 ������ Num2��

   ������
     Res: TCnBigDecimal                   - ��ʮ���Ƹ�������
     Num1: TCnBigDecimal                  - ��ʮ���Ƹ���������һ
     Num2: TCnBigDecimal                  - ��ʮ���Ƹ�����������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigDecimalSub(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal): Boolean;
{* ��ʮ���Ƹ���������Res ������ Num1 �� Num2��Num1 ������ Num2��

   ������
     Res: TCnBigDecimal                   - ��ʮ���Ƹ�������
     Num1: TCnBigDecimal                  - ��ʮ���Ƹ�����������
     Num2: TCnBigDecimal                  - ��ʮ���Ƹ���������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigDecimalMul(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal; MulPrecision: Integer = 0): Boolean;
{* ��ʮ���Ƹ������ˣ�Res ������ Num1 �� Num2��Num1 ������ Num2��
   MulPrecision ��ʾ�˷���ౣ��С�����λ��0 ��ʾȫ��������

   ������
     Res: TCnBigDecimal                   - ��ʮ���Ƹ�������
     Num1: TCnBigDecimal                  - ��ʮ���Ƹ���������һ
     Num2: TCnBigDecimal                  - ��ʮ���Ƹ�����������
     MulPrecision: Integer                - ����С�����λ��0 ��ʾȫ������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigDecimalDiv(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal; DivPrecision: Integer = 0): Boolean;
{* ��ʮ���Ƹ���������Res ������ Num1 �� Num2��Num1 ������ Num2��
   DivPrecision ��ʾ����������ౣ��С�����λ��0 ��ʾ��Ĭ����������

   ������
     Res: TCnBigDecimal                   - ��ʮ���Ƹ�������
     Num1: TCnBigDecimal                  - ��ʮ���Ƹ�����������
     Num2: TCnBigDecimal                  - ��ʮ���Ƹ���������
     DivPrecision: Integer                - ����С�����λ��0 ��ʾ��Ĭ��������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigDecimalSqrt(Res: TCnBigDecimal; Num: TCnBigDecimal;
  SqrtPrecision: Integer = 0): Boolean;
{* ��ʮ���Ƹ�������ƽ������Res ������ Num��
   SqrtPrecision ��ʾ��ƽ����������ౣ��С�����λ��0 ��ʾ��Ĭ����������

   ������
     Res: TCnBigDecimal                   - ��ʮ���Ƹ�����ƽ����
     Num: TCnBigDecimal                   - ������Ĵ�ʮ���Ƹ�����
     SqrtPrecision: Integer               - ����С�����λ��0 ��ʾ��Ĭ��������

   ����ֵ��Boolean                        - ���ؿ����Ƿ�ɹ�
}

procedure BigDecimalSqrt2(Res: TCnBigDecimal; Num: TCnBigDecimal;
  RoundCount: Integer = CN_SQRT_DEFAULT_ROUND_COUNT);
{* ��ʮ���Ƹ�������ƽ������Res ������ Num��С�����׼ȷ��λ���׿�������˴�δʵ�֡�
   �ڲ�ʹ�ô����������־��ȣ�RoundCount ��ʾ�ڲ�����������0 ��ʾ��Ĭ����������
   ע�����������̫��ᵼ�´����������������

   ������
     Res: TCnBigDecimal                   - ��ʮ���Ƹ�����ƽ����
     Num: TCnBigDecimal                   - ������Ĵ�ʮ���Ƹ�����
     RoundCount: Integer                  - �ڲ�����������Ĭ�� 10 ��

   ����ֵ�����ޣ�
}

function BigDecimalChangeToScale(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* ����ʮ���Ƹ�������ֵ��զ���ǰ����ת����ָ�� Scale��Ҳ����С����� Scale λ�����ܲ������룬��ָ��ģʽ����
   ��� Scale Ϊ�����������뵽�� 10 �η���Res ������ Num��

   ������
     Res: TCnBigDecimal                   - ����ת����Ĵ�ʮ���Ƹ�����
     Num: TCnBigDecimal                   - ��ת���Ĵ�ʮ���Ƹ�����
     Scale: Integer                       - С����� Scale λ
     RoundMode: TCnBigRoundMode           - ����Ĺ���

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigDecimalRoundToDigits(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* ����ʮ���Ƹ�������ֵ��զ���ǰ���°�ָ��ģʽ���뵽ָ��С����� Digits λ��
   ��������Ȳ��� Digits λ�򲻱䡣Res ������ Num��

   ������
     Res: TCnBigDecimal                   - ���������Ĵ�ʮ���Ƹ�����
     Num: TCnBigDecimal                   - ������Ĵ�ʮ���Ƹ�����
     Digits: Integer                      - ����С�����λ
     RoundMode: TCnBigRoundMode           - ����Ĺ���

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigDecimalTrunc(Res: TCnBigDecimal; Num: TCnBigDecimal): Boolean;
{* ����ʮ���Ƹ����� Trunc ��ֻʣ������Res ������ Num��

   ������
     Res: TCnBigDecimal                   - ���صĴ�ʮ���Ƹ�����
     Num: TCnBigDecimal                   - ������Ĵ�ʮ���Ƹ�����

   ����ֵ��Boolean                        - ���ش����Ƿ�ɹ�
}

procedure BigDecimalToBigRational(Res: TCnBigRational; Num: TCnBigDecimal);
{* ��ʮ���Ƹ�����ת��Ϊ����������

   ������
     Res: TCnBigRational                  - ����ת���Ĵ�������
     Num: TCnBigDecimal                   - ��ת���Ĵ�ʮ���Ƹ�����

   ����ֵ�����ޣ�
}

procedure BigRationalToBigDecimal(Res: TCnBigDecimal; Num: TCnBigRational;
  Digits: Integer = 20);
{* ��������ת��Ϊ��ʮ���Ƹ������������о�����ʧ��Ĭ�ϱ���С����� 20 λ��

   ������
     Res: TCnBigDecimal                   - ����ת���Ĵ�ʮ���Ƹ�����
     Num: TCnBigRational                  - ��ת���Ĵ�������
     Digits: Integer                      - ����С�����λ��Ĭ�� 20

   ����ֵ�����ޣ�
}

function BigDecimalDebugDump(Num: TCnBigDecimal): string;
{* ��ӡ��ʮ���Ƹ������ڲ���Ϣ��

   ������
     Num: TCnBigDecimal                   - ����ӡ�Ĵ�ʮ���Ƹ�����

   ����ֵ��string                         - �����ڲ���Ϣ�ַ���
}

// ========================== ������Ƹ������������� ===========================

procedure BigBinaryClear(Num: TCnBigBinary);
{* ���һ��������Ƹ���������ʵ������ Value �� Scale ������Ϊ 0��

   ������
     Num: TCnBigBinary                    - ����յĴ�����Ƹ�����

   ����ֵ�����ޣ�
}

function BigBinarySetDec(const Buf: string; Res: TCnBigBinary): Boolean;
{* Ϊ������Ƹ��������������ַ���ֵ��

   ������
     const Buf: string                    - �����õ��ַ���
     Res: TCnBigBinary                    - �����õĴ�����Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinarySetWord(W: Cardinal; Res: TCnBigBinary): Boolean;
{* Ϊ������Ƹ�����������������ֵ��

   ������
     W: Cardinal                          - �����õ� 32 λ�޷�������ֵ
     Res: TCnBigBinary                    - �����õĴ�����Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinarySetInt64(W: Int64; Res: TCnBigBinary): Boolean;
{* Ϊ������Ƹ������������� Int64 ����ֵ��

   ������
     W: Int64                             - �����õ� 64 λ�з�������ֵ
     Res: TCnBigBinary                    - �����õĴ�����Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinarySetSingle(Value: Single; Res: TCnBigBinary): Boolean;
{* Ϊ������Ƹ������������õ����ȸ���ֵ��

   ������
     Value: Single                        - �����õĵ����ȸ���ֵ
     Res: TCnBigBinary                    - �����õĴ�����Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinarySetDouble(Value: Double; Res: TCnBigBinary): Boolean;
{* Ϊ������Ƹ�������������˫���ȸ���ֵ��

   ������
     Value: Double                        - �����õ�˫���ȸ���ֵ
     Res: TCnBigBinary                    - �����õĴ�����Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinarySetExtended(Value: Extended; Res: TCnBigBinary): Boolean;
{* Ϊ������Ƹ���������������չ���ȸ���ֵ��

   ������
     Value: Extended                      - �����õ���չ���ȸ���ֵ
     Res: TCnBigBinary                    - �����õĴ�����Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinarySetBigNumber(Num: TCnBigNumber; Res: TCnBigBinary): Boolean;
{* Ϊ������Ƹ������������ô���ֵ��

   ������
     Num: TCnBigNumber                    -
     Res: TCnBigBinary                    - �����õĴ�����Ƹ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinaryToString(Num: TCnBigBinary): string;
{* ������Ƹ���������ת��Ϊ�ַ�����

   ������
     Num: TCnBigBinary                    - ��ת���Ĵ�����Ƹ�����

   ����ֵ��string                         - ����ת�����ַ���
}

function BigBinaryToSingle(Num: TCnBigBinary): Single;
{* ������Ƹ���������ת��Ϊ�����ȸ�������

   ������
     Num: TCnBigBinary                    - ��ת���Ĵ�����Ƹ�����

   ����ֵ��Single                         - ���ص����ȸ���ֵ
}

function BigBinaryToDouble(Num: TCnBigBinary): Double;
{* ������Ƹ���������ת��Ϊ˫���ȸ�������

   ������
     Num: TCnBigBinary                    - ��ת���Ĵ�����Ƹ�����

   ����ֵ��Double                         - ����˫���ȸ���ֵ
}

function BigBinaryToExtended(Num: TCnBigBinary): Extended;
{* ������Ƹ���������ת��Ϊ��չ���ȸ�������

   ������
     Num: TCnBigBinary                    - ��ת���Ĵ�����Ƹ�����

   ����ֵ��Extended                       - ������չ���ȸ���ֵ
}

function BigBinaryCompare(Num1: TCnBigBinary; Num2: TCnBigBinary): Integer; overload;
{* �Ƚ�����������Ƹ��������󣬷ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     Num1: TCnBigBinary                   - ���ȽϵĴ�����Ƹ�����һ
     Num2: TCnBigBinary                   - ���ȽϵĴ�����Ƹ�������

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigBinaryCompare(Num1: TCnBigBinary; Num2: Int64): Integer; overload;
{* �Ƚϴ�����Ƹ������������������ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     Num1: TCnBigBinary                   - ���ȽϵĴ�����Ƹ�����
     Num2: Int64                          - ���Ƚϵ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigBinaryCompare(Num1: TCnBigBinary; Num2: Extended): Integer; overload;
{* �Ƚϴ�����Ƹ����������븡�������ֱ���ݱȽϵĽ���Ǵ��ڡ����ڻ���С�������� 1��0��-1��

   ������
     Num1: TCnBigBinary                   - ���ȽϵĴ�����Ƹ�����
     Num2: Extended                       - ���Ƚϵĸ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

procedure BigBinaryCopy(Dest: TCnBigBinary; Source: TCnBigBinary);
{* ������Ƹ�������ֵ

   ������
     Dest: TCnBigBinary                   - Ŀ�������Ƹ�����
     Source: TCnBigBinary                 - Դ������Ƹ�����

   ����ֵ�����ޣ�
}

function BigBinaryGetHighScale(Num: TCnBigBinary): Integer;
{* ���������Ƹ������������Ч����λ��С�����ڼ�λ���������С�� 0�����󸺺��ʾ��С����ǰ�ڼ�λ

   ������
     Num: TCnBigBinary                    - ������Ĵ�����Ƹ�����

   ����ֵ��Integer                        - ���������Ч����λ��С�����λ������
}

function BigBinaryAdd(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary): Boolean;
{* ������Ƹ������ӣ�Res ������ Num1 �� Num2��Num1 ������ Num2��

   ������
     Res: TCnBigBinary                    - ������Ƹ�������
     Num1: TCnBigBinary                   - ������Ƹ���������һ
     Num2: TCnBigBinary                   - ������Ƹ�����������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigBinarySub(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary): Boolean;
{* ������Ƹ���������Res ������ Num1 �� Num2��Num1 ������ Num2��

   ������
     Res: TCnBigBinary                    - ������Ƹ�������
     Num1: TCnBigBinary                   - ������Ƹ�����������
     Num2: TCnBigBinary                   - ������Ƹ���������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigBinaryMul(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary; MulPrecision: Integer = 0): Boolean;
{* ������Ƹ������ˣ�Res ������ Num1 �� Num2��Num1 ������ Num2��
   MulPrecision ��ʾ�˷���ౣ��С�����λ��0 ��ʾȫ��������

   ������
     Res: TCnBigBinary                    - ������Ƹ�������
     Num1: TCnBigBinary                   - ������Ƹ���������һ
     Num2: TCnBigBinary                   - ������Ƹ�����������
     MulPrecision: Integer                - ����С�����λ��0 ��ʾȫ������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigBinaryDiv(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary; DivPrecision: Integer = 0): Boolean;
{* ������Ƹ���������Res ������ Num1 �� Num2��Num1 ������ Num2��
   DivPrecision ��ʾ����������ౣ��С�����λ��0 ��ʾ��Ĭ����������

   ������
     Res: TCnBigBinary                    - ������Ƹ�������
     Num1: TCnBigBinary                   - ������Ƹ�����������
     Num2: TCnBigBinary                   - ������Ƹ���������
     DivPrecision: Integer                - ����С�����λ��0 ��ʾ��Ĭ��������

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

procedure BigBinaryShiftLeft(Res: TCnBigBinary; N: Integer);
{* ������Ƹ��������ƣ��ڲ�ֱ�ӵ��� FScale

   ������
     Res: TCnBigBinary                    - �����ƵĴ�����Ƹ�����
     N: Integer                           - ����λ��

   ����ֵ�����ޣ�
}

procedure BigBinaryShiftRight(Res: TCnBigBinary; N: Integer);
{* ������Ƹ��������ƣ��ڲ�ֱ�ӵ��� FScale��

   ������
     Res: TCnBigBinary                    - �����ƵĴ�����Ƹ�����
     N: Integer                           - ����λ��

   ����ֵ�����ޣ�
}

function BigBinaryPower(Res: TCnBigBinary; N: Integer): Boolean;
{* ������Ƹ��������ݣ�ֻ֧�ַǸ������ݡ�

   ������
     Res: TCnBigBinary                    - �����ݵĴ�����Ƹ�����
     N: Integer                           - ��ָ����ֻ֧�ַǸ�����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinaryChangeToScale(Res: TCnBigBinary; Num: TCnBigBinary;
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* ��������Ƹ�������ֵ��զ���ǰ����ת����ָ�� Scale��Ҳ����С����� Scale λ�����ܲ������룬��ָ��ģʽ���С�
   ��� Scale Ϊ�����������뵽�� 2 �η���Res ������ Num��

   ������
     Res: TCnBigBinary                    - ����ת����Ĵ�����Ƹ�����
     Num: TCnBigBinary                    - ��ת���Ĵ�����Ƹ�����
     Scale: Integer                       - С����� Scale λ
     RoundMode: TCnBigRoundMode           - ����Ĺ���

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinaryRoundToDigits(Res: TCnBigBinary; Num: TCnBigBinary;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* ��������Ƹ�������ֵ��զ���ǰ���°�ָ��ģʽ���뵽ָ��С����� Digits ������λ��
   ��������Ȳ��� Digits λ�򲻱䡣Res ������ Num��

   ������
     Res: TCnBigBinary                    - ���������Ĵ�����Ƹ�����
     Num: TCnBigBinary                    - ������Ĵ�����Ƹ�����
     Digits: Integer                      - ����С����󼸸�������λ
     RoundMode: TCnBigRoundMode           - ����Ĺ���

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigBinaryTrunc(Res: TCnBigBinary; Num: TCnBigBinary): Boolean;
{* ��������Ƹ����� Trunc ��ֻʣ������Res ������ Num��

   ������
     Res: TCnBigBinary                    - ���صĴ�����Ƹ�����
     Num: TCnBigBinary                    - ������Ĵ�����Ƹ�����

   ����ֵ��Boolean                        - ���ش����Ƿ�ɹ�
}

function BigBinaryTruncTo(Res: TCnBigNumber; Num: TCnBigBinary): Boolean;
{* ��������Ƹ����� Trunc ��ֻʣ�����������ڴ����С�

   ������
     Res: TCnBigNumber                    - ���صĴ�����
     Num: TCnBigBinary                    - ������Ĵ�����Ƹ�����

   ����ֵ��Boolean                        - ���ش����Ƿ�ɹ�
}

function BigBinaryDebugDump(Num: TCnBigBinary): string;
{* ��ӡ������Ƹ������ڲ���Ϣ��

   ������
     Num: TCnBigBinary                    - ����ӡ�Ĵ�����Ƹ�����

   ����ֵ��string                         - �����ڲ���Ϣ�ַ���
}

var
  CnBigDecimalOne: TCnBigDecimal = nil;     // ��ʾ 1 �ĳ���
  CnBigDecimalZero: TCnBigDecimal = nil;    // ��ʾ 0 �ĳ���

implementation

resourcestring
  SCnNotImplemented = 'NOT Implemented.';
  SCnScaleOutOfRange = 'Scale Out of Range.';
  SCnRoundModeNotSupport = 'Round Mode Not Support.';
  SCnSqrtRangeError = 'Sqrt Range Error.';

const
  SCN_FIVE_POWER_UINT32 = 13;
  SCN_POWER_FIVES32: array[0..13] of Cardinal = (
    1,                               // 5 ^ 0
    5,                               // 5 ^ 1
    25,                              // 5 ^ 2
    125,                             // 5 ^ 3
    625,                             // 5 ^ 4
    3125,                            // 5 ^ 5
    15625,                           // 5 ^ 6
    78125,                           // 5 ^ 7
    390625,                          // 5 ^ 8
    1953125,                         // 5 ^ 9
    9765625,                         // 5 ^ 10
    48828125,                        // 5 ^ 11
    244140625,                       // 5 ^ 12
    1220703125                       // 5 ^ 13
  );

  SCN_TEN_POWER_UINT32 = 9;
  SCN_POWER_TENS32: array[0..9] of Cardinal = (
    1,                               // 10 ^ 0
    10,                              // 10 ^ 1
    100,                             // 10 ^ 2
    1000,                            // 10 ^ 3
    10000,                           // 10 ^ 4
    100000,                          // 10 ^ 5
    1000000,                         // 10 ^ 6
    10000000,                        // 10 ^ 7
    100000000,                       // 10 ^ 8
    1000000000                       // 10 ^ 9
  );

//  SCN_POWER_TENS64: array[0..19] of TUInt64 = (
//    1,                               // 10 ^ 0
//    10,                              // 10 ^ 1
//    100,                             // 10 ^ 2
//    1000,                            // 10 ^ 3
//    10000,                           // 10 ^ 4
//    100000,                          // 10 ^ 5
//    1000000,                         // 10 ^ 6
//    10000000,                        // 10 ^ 7
//    100000000,                       // 10 ^ 8
//    1000000000,                      // 10 ^ 9
//    10000000000,                     // 10 ^ 10
//    100000000000,                    // 10 ^ 11
//    1000000000000,                   // 10 ^ 12
//    10000000000000,                  // 10 ^ 13
//    100000000000000,                 // 10 ^ 14
//    1000000000000000,                // 10 ^ 15
//    10000000000000000,               // 10 ^ 16
//    100000000000000000,              // 10 ^ 17
//    1000000000000000000,             // 10 ^ 18
//    $8AC7230489E80000                // 10 ^ 19
//
//    // 10 ^ 19 10000000000000000000 �Ѿ����� Int64 9223372036854775807
//    // ���Ե��� 16 ����д��û�� UInt64 18446744073709551615��10 ^ 20 �ų�
//  );

var
  FLocalBigDecimalPool: TCnBigDecimalPool = nil;
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalBigBinaryPool: TCnBigBinaryPool = nil;

  FDefaultDecimalPrecisionDigits: Integer = CN_BIG_DECIMAL_DEFAULT_PRECISION;
  FDefaultBinaryPrecisionDigits: Integer = CN_BIG_BINARY_DEFAULT_PRECISION;

// ���ݾ�����������һ����ֵ��������������ֹͣ
procedure GetGapFromPrecisionDigits(Precision: Integer; Gap: TCnBigDecimal);
begin
  if Precision <= 0 then
    Precision := FDefaultDecimalPrecisionDigits;

  Gap.FValue.SetOne;
  Gap.FScale := Precision + 1;
end;

function CheckScaleAddRange(Scale1, Scale2: Integer): Integer;
begin
  if IsInt32AddOverflow(Scale1, Scale2) then
    raise ECnBigDecimalException.Create(SCnScaleOutOfRange);
  Result := Scale1 + Scale2;
end;

procedure RoundDecimalByMode(Quotient, Divisor, Remainder: TCnBigNumber; QWillBeNeg: Boolean;
  Mode: TCnBigRoundMode);
var
  R2: TCnBigNumber;
  R2CD: Integer;
begin
  if Remainder.IsZero then
    Exit;

  case Mode of
    drAwayFromZero:            // ������ֵ�����ȡ
      begin
        BigNumberAddWord(Quotient, 1);
      end;
    drTowardsZero:             // ������ֵС����ȡ������ֻ���������ֵ� Trunc
      begin
        // ɶ��������
      end;
    drCeilingToInfinite:       // ���������ȡ
      begin
        if not QWillBeNeg then
          BigNumberAddWord(Quotient, 1);
      end;
    drFloorToNegInfinite:      // ���������ȡ
      begin
        if QWillBeNeg then
          BigNumberAddWord(Quotient, 1);
      end;
  else
    R2 := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(R2, Remainder);
      BigNumberShiftLeftOne(R2, R2);
      R2CD := BigNumberCompare(R2, Divisor);

      // ��������ģʽ�£�R2CD ������ڵ��� 0��˵���������ڵ��� 5��Ҫ��
      // ��������ģʽ�£�������� 1��Ҫ�ж��̵�ĩλ�Ƿ�ż��ż���룬�����������
      // ȷ�������������ٸ������������ᶼ�ǳ�����ֵС�ķ������Ǿ���ֵ��ķ���
      case Mode of
        drRound:         // �������롢��������ֵ�����
          begin
            if R2CD >= 0 then
              BigNumberAddWord(Quotient, 1);
          end;
        dr465RoundEven:     // �����������˫����������ֵ�����
          begin
            if (R2CD > 0) or ((R2CD = 0) and not Quotient.IsOdd) then
              BigNumberAddWord(Quotient, 1);
          end;
      end;
    finally
      FLocalBigNumberPool.Recycle(R2);
    end;
  end;
end;

// �������� 10 �� Power5 �η�����֧�ָ�
procedure BigNumberMulPower5(Num: TCnBigNumber; Power5: Integer);
var
  I, L, D, R: Integer;
begin
  if Power5 < 0 then
    raise ECnBigDecimalException.Create(SCnNotImplemented);

  L := High(SCN_POWER_FIVES32);       // һ������ 13 ��
  D := Power5 div L;
  R := Power5 mod L;

  for I := 1 to D do                  // һ���� 13 ����
    Num.MulWord(SCN_POWER_FIVES32[L]);
  Num.MulWord(SCN_POWER_FIVES32[R]);  // ���ϳ�ʣ�µ�
end;

// �������� 10 �� Power10 �η�����֧�ָ�
procedure BigNumberMulPower10(Num: TCnBigNumber; Power10: Integer);
var
  I, L, D, R: Integer;
begin
  if Power10 < 0 then
    raise ECnBigDecimalException.Create(SCnNotImplemented);

  L := High(SCN_POWER_TENS32);       // һ������ 9 ��
  D := Power10 div L;
  R := Power10 mod L;

  for I := 1 to D do                 // һ���� 9 ����
    Num.MulWord(SCN_POWER_TENS32[L]);
  Num.MulWord(SCN_POWER_TENS32[R]);  // ���ϳ�ʣ�µ�
end;

procedure BigDecimalClear(Num: TCnBigDecimal);
begin
  if Num <> nil then
  begin
    Num.FScale := 0;
    Num.FValue.SetZero;
  end;
end;

function BigDecimalSetDec(const Buf: string; Res: TCnBigDecimal): Boolean;
var
  Neg, ENeg: Boolean;
  E, DC: Integer;
  P, DotPos: PChar;
  S, V: string;
  C: Char;
begin
  Result := False;

  V := '';
  S := Trim(Buf);
  P := PChar(S);
  if P^ = #0 then
    Exit;

  Neg := False;
  ENeg := False;
  DotPos := nil;

  if (P^ = '+') or (P^ = '-') then
  begin
    Neg := (P^ = '-');
    Inc(P);
  end;

  if P^ = #0 then
    Exit;

  Res.FValue.SetZero;
  DC := 0;

  // ����ֵ��ֱ����β�����Ͽ�ѧ�������� E
  C := P^;
  while (C <> #0) and (C <> 'e') and (C <> 'E') do
  begin
    case C of
      '0'..'9':
        V := V + C;
      ',':
        ; // �ֽںź���
      '.':
        if Assigned(DotPos) then
          // С����ֻ����һ��
          Exit
        else
          DotPos := P;
    else
      Exit;
    end;
    Inc(P);
    C := P^;
  end;

  // V �ǲ�����С�����ʮ�����ַ���

  // ���������ԭ����С���㣬��� DC ��ֵ
  if Assigned(DotPos) then
    DC := P - DotPos - 1;

  E := 0;
  if (C = 'e') or (C = 'E') then
  begin
    // ��ѧ�������� E �����ָ��
    Inc(P);
    if (P^ = '+') or (P^ = '-') then
    begin
      ENeg := (P^ = '-');
      Inc(P);
    end;
    while P^ <> #0 do
    begin
      case P^ of
        '0'..'9':
          E := E * 10 + Ord(P^) - Ord('0');
      else
        Exit;
      end;
      Inc(P);
    end;
  end;

  if ENeg then
    E := -E;
  DC := DC - E; // ���ָ��һ�����С�����ֳ��ȸ� DC

  Res.FScale := DC;
  Res.FValue.SetDec(AnsiString(V));

  if (not Res.FValue.IsNegative) and Neg then
    Res.FValue.SetNegative(True);

  Result := True;
end;

function BigDecimalSetWord(W: Cardinal; Res: TCnBigDecimal): Boolean;
begin
  Res.FValue.SetWord(W);
  Res.FScale := 0;
  Result := True;
end;

function BigDecimalSetInt64(W: Int64; Res: TCnBigDecimal): Boolean;
begin
  Res.FValue.SetInt64(W);
  Res.FScale := 0;
  Result := True;
end;

function InternalBigDecimalSetFloat(Neg: Boolean; IntExponent: Integer; IntMantissa: TUInt64;
  Res: TCnBigDecimal): Boolean;
var
  C: Integer;
begin
  C := GetUInt64LowBits(IntMantissa); // ��� IntMantissa �ұߵ��㲢���� Exponent �Ի���
  if C > 0 then
  begin
    IntMantissa := IntMantissa shr C;
    Inc(IntExponent, C);
  end;

  // ֵ�� IntMantissa * 2^IntExponent
  BigNumberSetUInt64UsingInt64(Res.FValue, IntMantissa);
  if IntExponent > 0 then
  begin
    Res.FValue.ShiftLeft(IntExponent);   // ֱ����������������ָ����� 0
    Res.FScale := 0;
  end
  else // ָ���Ǹ���˵����С�����֣���ôÿ������ 2 ��Ҫ��ɳ��� 10��IntMantissa �͵����ÿ��ָ������ 5
  begin
    IntExponent := -IntExponent;
    Res.FScale := IntExponent;
    BigNumberMulPower5(Res.FValue, IntExponent);
  end;

  Res.FValue.SetNegative(Neg);
  Result := True;
end;

function BigDecimalSetSingle(Value: Single; Res: TCnBigDecimal): Boolean;
var
  N: Boolean;
  E: Integer;
  S: Cardinal;
begin
  if SingleIsInfinite(Value) or SingleIsNan(Value) then
    raise ECnBigDecimalException.Create(SInvalidOp);

  if Value = 0.0 then
  begin
    Res.FValue.SetZero;
    Res.FScale := 0;
    Result := True;
    Exit;
  end;

  ExtractFloatSingle(Value, N, E, S);
  // �� 1. ��ͷ����Ч���ֵ���������E ��Ҫ�� 23
  Result := InternalBigDecimalSetFloat(N, E - CN_SINGLE_SIGNIFICAND_BITLENGTH, TUInt64(S), Res);
end;

function BigDecimalSetDouble(Value: Double; Res: TCnBigDecimal): Boolean;
var
  N: Boolean;
  E: Integer;
  S: TUInt64;
begin
  if DoubleIsInfinite(Value) or DoubleIsNan(Value) then
    raise ECnBigDecimalException.Create(SInvalidOp);

  if Value = 0.0 then
  begin
    Res.FValue.SetZero;
    Res.FScale := 0;
    Result := True;
    Exit;
  end;

  ExtractFloatDouble(Value, N, E, S);
  // �� 1. ��ͷ����Ч���ֵ���������E ��Ҫ�� 52
  Result := InternalBigDecimalSetFloat(N, E - CN_DOUBLE_SIGNIFICAND_BITLENGTH, S, Res);
end;

function BigDecimalSetExtended(Value: Extended; Res: TCnBigDecimal): Boolean;
var
  N: Boolean;
  E: Integer;
  S: TUInt64;
begin
  if ExtendedIsInfinite(Value) or ExtendedIsNan(Value) then
    raise ECnBigDecimalException.Create(SInvalidOp);

  if Value = 0.0 then
  begin
    Res.FValue.SetZero;
    Res.FScale := 0;
    Result := True;
    Exit;
  end;

  ExtractFloatExtended(Value, N, E, S);
  // �� 1. ��ͷ����Ч���ֵ���������E ��Ҫ�� 63
  Result := InternalBigDecimalSetFloat(N, E - CN_EXTENDED_SIGNIFICAND_BITLENGTH, S, Res);
end;

function BigDecimalToString(Num: TCnBigDecimal): string;
var
  C: Char;
  S: string;
  L: Integer;
begin
  S := Num.FValue.ToDec;
  L := Length(S);

  if L = 0 then
  begin
    Result := '';
    Exit;
  end;

  // ������������
  C := #0;
  if (S[1] = '-') or (S[1] = '+') then
  begin
    C := S[1];
    Delete(S, 1, 1);
    Dec(L);
  end;

  // ȷ��С����λ��
  if Num.FScale < 0 then
    Result := S + StringOfChar('0', -Num.FScale)
  else if Num.FScale = 0 then
    Result := S
  else if Num.FScale >= L then
    Result := '0.' + StringOfChar('0', Num.FScale - L) + S
  else
    Result := Copy(S, 1, L - Num.FScale) + '.' + Copy(S, L - Num.FScale + 1, MaxInt);

  // �ٰ������żӻ���
  if C <> #0 then
    Result := C + Result;
end;

// ͨ���޴�ı任�ô�ʮ���Ƹ�������ԭʼֵ���� Value / 2^Scale��������Ч���������ض�λ�� 
function InternalBigDecimalConvertToBitsCount(Num: TCnBigDecimal; BitsCount: Integer): Boolean;
var
  C, D: Integer;
  Di, R: TCnBigNumber;
begin
//  FValue * 10^-FScale Ҫ��� M * 2^E �η�����ʽ��FValue �͵ó��� 5^-FScale����� FScale < 0������ֱ�ӳ� 5^-FScale
//  ��� FScale > 0����ζ�� FValue Ҫ���� 5 �� FScale �η���
//  (FValue * 5^-FScale) * 2^-FScale��Ȼ�󻹵��ٴι�Լ����ǰ�߳�Ϊ�ض�λ���������ٵ���

  Result := False;
  if Num <> nil then
  begin
    if Num.FScale < 0 then
    begin
      BigNumberMulPower5(Num.FValue, -Num.FScale);  // �� 5 ���������η�
    end
    else // FScale ���� 0
    begin
      // ���� 5 �� FScale �η���ע�ⲻ��ȡ�ɵس��� 10 �� FScale �η����� FScale��
      // �ٳ��� 2 �� FScale �η������ƣ�����Ϊ���������� FScale ���岻һ����
      // ���Զ����Ƶķ�ʽ�� FValue ֱ�ӳ��� 5 �� FScale �η����õ��Ľ��Ҫ�����Ƶ��� FScale ֵ

      Di := FLocalBigNumberPool.Obtain;
      R := FLocalBigNumberPool.Obtain;
      try
        Di.SetWord(5);
        Di.PowerWord(Num.FScale); // �õ�����

        // FValue / Di Ҫ�õ�С����������Բ���ֱ�� BigNumberDiv���ý� FValue ���� 2 �������η�
        // ������ٴη�ȡ���ھ��ȣ�������Ϊ��ҪС�����ʮ���� FScale λ����������� 2 ����
        C := Num.FScale * 2;
        if C < CN_BIG_BINARY_DEFAULT_PRECISION then
          C := CN_BIG_BINARY_DEFAULT_PRECISION;

        BigNumberShiftLeft(Num.FValue, Num.FValue, C);
        BigNumberDiv(Num.FValue, R, Num.FValue, Di);
        Num.FScale := Num.FScale + C;
      finally
        FLocalBigNumberPool.Recycle(R);
        FLocalBigNumberPool.Recycle(Di);
      end;
    end;

    // �ٹ�Լ��ע���ʱ FValue �� FScale �Ѿ��� 2^ ��ϵ�ˣ�Num �൱��һ�� TCnBigBinary �ˣ�
    C := Num.FValue.GetBitsCount;
    if C < BitsCount then
    begin
      D := BitsCount - C;
      Num.FValue.ShiftLeft(D);
      Num.FScale := Num.FScale + D;
    end
    else if C > BitsCount then
    begin
      D := C - BitsCount;  // Ҫ�ص� D ��λ��Ҳ����Ҫ�� FScale ���� D
      Num.FValue.ShiftRight(D);
      Num.FScale := Num.FScale - D;
    end;
    Result := True;
  end;
end;

function BigDecimalToSingle(Num: TCnBigDecimal): Single;
var
  T: TCnBigDecimal;
  E: Integer;
  M: Cardinal;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    BigDecimalCopy(T, Num);
    InternalBigDecimalConvertToBitsCount(T, CN_SINGLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // ������λ�� 1

    M := T.FValue.GetWord;
    E := -T.FScale;

    CombineFloatSingle(Num.IsNegative, E + CN_SINGLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

function BigDecimalToDouble(Num: TCnBigDecimal): Double;
var
  T: TCnBigDecimal;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    BigDecimalCopy(T, Num);
    InternalBigDecimalConvertToBitsCount(T, CN_DOUBLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // ������λ�� 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatDouble(Num.IsNegative, E + CN_DOUBLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

function BigDecimalToExtended(Num: TCnBigDecimal): Extended;
var
  T: TCnBigDecimal;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    BigDecimalCopy(T, Num);
    InternalBigDecimalConvertToBitsCount(T, CN_EXTENDED_SIGNIFICAND_BITLENGTH + 1);
    // ����������λ�� 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatExtended(Num.IsNegative, E + CN_EXTENDED_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

function BigDecimalCompare(Num1, Num2: TCnBigDecimal): Integer;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    if Num2.FValue.IsZero then
      Result := 0   // ���� 0�����
    else if Num2.FValue.IsNegative then
      Result := 1   // 0 ���ڸ�
    else
      Result := -1; // 0 С����
  end
  else if Num2.FValue.IsZero then
  begin
    if not Num1.FValue.IsNegative then
      Result := 1     // ������ 0
    else
      Result := -1;   // ��С�� 0
  end
  else if Num1.FValue.IsNegative and not Num2.FValue.IsNegative then // ����Ϊ 0����С����
    Result := -1
  else if not Num1.FValue.IsNegative and Num2.FValue.IsNegative then // ����Ϊ 0�������ڸ�
    Result := 1
  else if Num1.FScale = Num2.FScale then // ������ͬ���ȿ�ָ���Ƿ���ͬ
    Result := BigNumberCompare(Num1.FValue, Num2.FValue)
  else // ������ͬ��ָ����ͬ
  begin
    // Ҫ�� Scale ���Ҳ����С���㿿�����������Խ�С�� Value��
    // ���� 10 ��ָ��������Զ���С���㣬�ٺ���һ���Ƚϣ����豣��ֵ���䣬���ԺͼӼ�������
    T := FLocalBigNumberPool.Obtain;
    L := CheckScaleAddRange(Num1.FScale, -Num2.FScale);

    try
      if L > 0 then
      begin
        BigNumberCopy(T, Num2.FValue);
        BigNumberMulPower10(T, L);
        Result := BigNumberCompare(Num1.FValue, T);
      end
      else
      begin
        BigNumberCopy(T, Num1.FValue);
        L := -L;
        BigNumberMulPower10(T, L);
        Result := BigNumberCompare(T, Num2.FValue);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: Int64): Integer;
var
  T: TCnBigDecimal;
begin
  if not Num1.IsNegative and (Num2 < 0) then
    Result := 1
  else if Num1.IsNegative and (Num2 > 0) then
    Result := -1
  else if Num1.IsZero and (Num2 = 0) then
    Result := 0
  else
  begin
    T := FLocalBigDecimalPool.Obtain;
    try
      T.FScale := 0;
      T.FValue.SetInt64(Num2);
      Result := BigDecimalCompare(Num1, T);
    finally
      FLocalBigDecimalPool.Recycle(T);
    end;
  end;
end;

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: Extended): Integer;
var
  T: TCnBigDecimal;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    T.SetExtended(Num2);
    Result := BigDecimalCompare(Num1, T);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

procedure BigDecimalCopy(Dest: TCnBigDecimal; Source: TCnBigDecimal);
begin
  if (Source <> nil) and (Dest <> nil) and (Source <> Dest) then
  begin
    BigNumberCopy(Dest.FValue, Source.FValue);
    Dest.FScale := Source.FScale;
  end;
end;

function BigDecimalGetPrecision(Num: TCnBigDecimal): Integer;
begin
  Result := 0;
  if Num <> nil then
    Result := BigNumberGetTenPrecision(Num.FValue); // �õ�ʮ��������λ��
end;

function BigDecimalGetIntDecimalCount(Num: TCnBigDecimal;
  out IntCount: Integer; out DecimalCount: Integer): Boolean;
var
  P: Integer;
begin
  Result := False;
  if Num <> nil then
  begin
    P := BigNumberGetTenPrecision(Num.FValue);
    if Num.FScale > 0 then  // ��С������
    begin
      DecimalCount := Num.FScale;
      IntCount := P - DecimalCount;
      if IntCount < 0 then
        IntCount := 0;
    end
    else
    begin
      // û��С������
      DecimalCount := 0;
      IntCount := P + Num.FScale;
    end;
    Result := True;
  end;
end;

function BigDecimalGetHighScale(Num: TCnBigDecimal): Integer;
begin
  Result := 0;
  if Num <> nil then
  begin
    Result := BigNumberGetTenPrecision(Num.FValue);
    // С������� FScale λ����ȥ��Ч����
    Result := Num.FScale - Result + 1;
    if Result <= 0 then // С����ǰ�ڼ�λ�Ǵ� 1 ��ʼ��
      Dec(Result)
  end;
end;

function BigDecimalAdd(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal): Boolean;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    BigDecimalCopy(Res, Num2);
    Result := True;
    Exit;
  end
  else if Num2.FValue.IsZero then
  begin
    BigDecimalCopy(Res, Num1);
    Result := True;
    Exit;
  end
  else if Num1.FScale = Num2.FScale then
  begin
    // ָ����ֱͬ�Ӽ�
    Res.FScale := Num1.FScale;
    Result := BigNumberAdd(Res.FValue, Num1.FValue, Num2.FValue);
    Exit;
  end
  else
  begin
    // Ҫ�� Scale С��Ҳ����С���㿿�����������Խϴ�� Value��
    // ���� 10 ��ָ������ݲ���С��ͬ�ȵ� Scale �Զ���С���㲢������ֵ���䣬
    // �ٺ���һ����ӣ������ Scale ȡС��
    T := FLocalBigNumberPool.Obtain;
    L := CheckScaleAddRange(Num1.FScale, -Num2.FScale);

    try
      if L > 0 then
      begin
        BigNumberCopy(T, Num2.FValue);
        BigNumberMulPower10(T, L);
        Res.FScale := Num1.FScale;
        Result := BigNumberAdd(Res.FValue, Num1.FValue, T);
      end
      else
      begin
        BigNumberCopy(T, Num1.FValue);
        L := -L;
        BigNumberMulPower10(T, L);
        Res.FScale := Num2.FScale;
        Result := BigNumberAdd(Res.FValue, T, Num2.FValue);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

function BigDecimalSub(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal): Boolean;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    BigNumberCopy(Num2.FValue, Res.FValue);
    Res.FValue.Negate;
    Result := True;
    Exit;
  end
  else if Num2.FValue.IsZero then
  begin
    BigNumberCopy(Num1.FValue, Res.FValue);
    Result := True;
    Exit;
  end
  else if Num1.FScale = Num2.FScale then
  begin
    // ָ����ֱͬ�Ӽ�
    Res.FScale := Num1.FScale;
    Result := BigNumberSub(Res.FValue, Num1.FValue, Num2.FValue);
    Exit;
  end
  else
  begin
    // Ҫ�� Scale С��Ҳ����С���㿿�����������Խϴ�� Value��
    // ���� 10 ��ָ������ݲ���С��ͬ�ȵ� Scale �Զ���С���㲢������ֵ���䣬
    // �ٺ���һ������������ Scale ȡС��
    T := FLocalBigNumberPool.Obtain;
    L := CheckScaleAddRange(Num1.FScale, -Num2.FScale);

    try
      if L > 0 then
      begin
        BigNumberCopy(T, Num2.FValue);
        BigNumberMulPower10(T, L);
        Res.FScale := Num1.FScale;
        Result := BigNumberSub(Res.FValue, Num1.FValue, T);
      end
      else
      begin
        BigNumberCopy(T, Num1.FValue);
        L := -L;
        BigNumberMulPower10(T, L);
        Res.FScale := Num2.FScale;
        Result := BigNumberSub(Res.FValue, T, Num2.FValue);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

function BigDecimalMul(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal; MulPrecision: Integer): Boolean;
begin
  if Num1.FValue.IsZero or Num2.FValue.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end
  else
  begin
    Res.FScale := CheckScaleAddRange(Num1.FScale, Num2.FScale);
    Result := BigNumberMul(Res.FValue, Num1.FValue, Num2.FValue);
    if Result and (MulPrecision > 0) then
      Result := BigDecimalRoundToDigits(Res, Res, MulPrecision, drTowardsZero);
  end;
end;

function BigDecimalDiv(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal; DivPrecision: Integer): Boolean;
var
  S: Boolean;
  M, TS: Integer;
  T, R: TCnBigNumber;
begin
  if Num2.FValue.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if Num1.FValue.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  // ������
  S := Num1.FValue.isNegative <> Num2.FValue.IsNegative; // ���Ų��Ƚ���Ÿ�
  TS := Num1.FScale - Num2.FScale;

  if DivPrecision <= 0 then
    DivPrecision := FDefaultDecimalPrecisionDigits;
  if DivPrecision < 0 then
    DivPrecision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  // ���ݾ���Ҫ����㽫����������ı�����ע��Ϊ�˼��ٿ����� 1 λ���
  M := CheckScaleAddRange(DivPrecision, BigNumberGetTenPrecision2(Num2.FValue)
    - BigNumberGetTenPrecision2(Num1.FValue) + 1);
  if M < 0 then  // �������󡢾����Ѿ��㹻
    M := 0
  else if M > 0 then
    TS := CheckScaleAddRange(TS, M); // ����ı������������

  T := nil;
  R := nil;
  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberCopy(T, Num1.FValue);
    BigNumberMulPower10(T, M);

    R := FLocalBigNumberPool.Obtain;
    BigNumberDiv(Res.FValue, R, T, Num2.FValue);  // Num1.FValue * 10 ^ M div Num2.FValue �õ��̺�����

    RoundDecimalByMode(Res.FValue, Num2.FValue, R, Res.FValue.IsNegative, drTowardsZero);
    Res.FScale := TS;
    // TODO: ʮ����Լ��

    BigDecimalRoundToDigits(Res, Res, DivPrecision, drTowardsZero);
    Res.FValue.SetNegative(S);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(R);
  end;
end;

procedure BigDecimalSqrt2(Res: TCnBigDecimal; Num: TCnBigDecimal;
  RoundCount: Integer);
var
  I: Integer;
  X0, R, T, D, G: TCnBigRational;
begin
  if Num.IsNegative then
    raise ERangeError.Create('');

  if Num.IsZero or Num.IsOne then
  begin
    if Res <> Num then
      BigDecimalCopy(Res, Num);

    Exit;
  end;

  if RoundCount <= 0 then
    RoundCount := CN_SQRT_DEFAULT_ROUND_COUNT;

  X0 := nil;
  T := nil;
  D := nil;
  G := nil;

  try
    X0 := TCnBigRational.Create;
    BigDecimalToBigRational(X0, Num);
    R := TCnBigRational.Create;
    D := TCnBigRational.Create;
    D.SetIntValue(2);

    I := 0;
    while I < RoundCount do
    begin
      Inc(I);

      // R := (X0 + Num/X0) / 2;
      BigDecimalToBigRational(R, Num);
      BigRationalNumberDiv(R, R, X0);
      BigRationalNumberAdd(R, R, X0);
      BigRationalNumberDiv(R, R, D);

      X0.Assign(R);
    end;
    BigRationalToBigDecimal(Res, R);
  finally
    X0.Free;
    T.Free;
    D.Free;
    G.Free;
  end;
end;

function BigDecimalSqrt(Res: TCnBigDecimal; Num: TCnBigDecimal;
  SqrtPrecision: Integer = 0): Boolean;
var
  X0, R, T, D, G: TCnBigDecimal;
begin
  if Num.IsNegative then
    raise ERangeError.Create(SCnSqrtRangeError);

  if Num.IsZero or Num.IsOne then
  begin
    if Res <> Num then
      BigDecimalCopy(Res, Num);

    Result := True;
    Exit;
  end;

  X0 := nil;
  T := nil;
  D := nil;
  G := nil;

  if SqrtPrecision <= 0 then
    SqrtPrecision := CN_BIG_DECIMAL_DEFAULT_PRECISION; 

  try
    G := FLocalBigDecimalPool.Obtain;
    GetGapFromPrecisionDigits(SqrtPrecision, G);

    D := FLocalBigDecimalPool.Obtain;
    D.SetWord(2);

    T := FLocalBigDecimalPool.Obtain;

    X0 := FLocalBigDecimalPool.Obtain;
    BigDecimalCopy(X0, Num);

    if Res <> Num then
      R := Res
    else
      R := FLocalBigDecimalPool.Obtain;

    while True do
    begin
      // R := (X0 + Num/X0) / 2;
      BigDecimalCopy(R, Num);
      BigDecimalDiv(R, R, X0, SqrtPrecision * 2);
      BigDecimalAdd(R, R, X0);
      BigDecimalDiv(R, R, D, SqrtPrecision * 2);

      BigDecimalSub(T, R, X0);
      if T.IsNegative then
        T.Negate;

      if BigDecimalCompare(T, G) <= 0 then
        Break;

      // X0 := R;
      BigDecimalCopy(X0, R);
    end;

    if Num = Res then
    begin
      BigDecimalCopy(Res, R);
      FLocalBigDecimalPool.Recycle(R);
    end;

    Res.RoundTo(SqrtPrecision);
    Result := True;
  finally
    FLocalBigDecimalPool.Recycle(X0);
    FLocalBigDecimalPool.Recycle(T);
    FLocalBigDecimalPool.Recycle(D);
    FLocalBigDecimalPool.Recycle(G);
  end;
end;

function BigDecimalChangeToScale(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Scale: Integer; RoundMode: TCnBigRoundMode): Boolean;
var
  DS: Integer;
  D, Q, R: TCnBigNumber;
  Neg: Boolean;
begin
  DS := CheckScaleAddRange(Num.FScale, -Scale);
  if DS > 0 then // �µ�С������λ����ԭ���٣�Ҫ��֮������
  begin
    D := FLocalBigNumberPool.Obtain;
    Q := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    try
      D.SetOne;
      BigNumberMulPower10(D, DS);  // ����� 10 �� DS �η���������

      Neg := Num.FValue.IsNegative;
      Num.FValue.SetNegative(False);

      // �����̺�������
      BigNumberDiv(Q, R, Num.FValue, D);

      // �����̺������Լ������������
      RoundDecimalByMode(Q, D, R, Neg, RoundMode);

      BigNumberCopy(Res.FValue, Q);
      Res.FScale := Scale;
      Res.FValue.SetNegative(Neg);

      if Res <> Num then           // ��� Num �Ƕ����ģ�����Ҫ��ԭ�� Neg
        Num.FValue.SetNegative(Neg);
      Result := True;
    finally
      FLocalBigNumberPool.Recycle(D);
      FLocalBigNumberPool.Recycle(Q);
      FLocalBigNumberPool.Recycle(R);
    end;
  end
  else // �µ�С����λ����ԭ�����࣬�򵥱任һ�¾���
  begin
    BigNumberCopy(Res.FValue, Num.FValue);
    if DS < 0 then
      BigNumberMulPower10(Res.FValue, -DS);
    Res.FScale := Scale;
    Result := True;
  end;
end;

function BigDecimalRoundToDigits(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
var
  DS: Integer;
  D, Q, R: TCnBigNumber;
  Neg: Boolean;
begin
  Result := False;
  DS := CheckScaleAddRange(Num.FScale, -Digits);

  if DS > 0 then // �µ�С������λ���ñ�ԭ���٣����ܳ�֮������
  begin
    D := FLocalBigNumberPool.Obtain;
    Q := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    try
      D.SetOne;
      BigNumberMulPower10(D, DS);  // ����� 10 �� DS �η���������

      Neg := Num.FValue.IsNegative;
      Num.FValue.SetNegative(False);

      // �����̺�������
      BigNumberDiv(Q, R, Num.FValue, D);

      // �����̺������Լ������������
      RoundDecimalByMode(Q, D, R, Neg, RoundMode);

      BigNumberCopy(Res.FValue, Q);
      Res.FScale := Digits;
      Res.FValue.SetNegative(Neg);

      if Res <> Num then           // ��� Num �Ƕ����ģ�����Ҫ��ԭ�� Neg
        Num.FValue.SetNegative(Neg);
      Result := True;
    finally
      FLocalBigNumberPool.Recycle(D);
      FLocalBigNumberPool.Recycle(Q);
      FLocalBigNumberPool.Recycle(R);
    end;
  end;
end;

function BigDecimalTrunc(Res: TCnBigDecimal; Num: TCnBigDecimal): Boolean;
begin
  if Num.FScale <= 0 then // ��С������
  begin
    BigDecimalCopy(Res, Num);
    Result := True;
    Exit;
  end
  else // ��С������ FScale λ���ɵ�
  begin
    Result := BigDecimalChangeToScale(Res, Num, 0, drTowardsZero);
  end;
end;

procedure BigDecimalToBigRational(Res: TCnBigRational; Num: TCnBigDecimal);
var
  T: TCnBigNumber;
begin
  if (Res <> nil) and (Num <> nil) then
  begin
    BigNumberCopy(Res.Nominator, Num.FValue);

    // ��ȷֵΪ FValue / (10^FScale)����� FScale > 0 ��˷�����ĸ��ȥ�������෴���˷���������ȥ
    Res.Denominator.SetOne;
    if Num.FScale > 0 then
    begin
      Res.Denominator.SetWord(10);
      Res.Denominator.PowerWord(Num.FScale);
    end
    else
    begin
      T := FLocalBigNumberPool.Obtain;
      try
        T.SetWord(10);
        T.PowerWord(-Num.FScale);
        BigNumberMul(Res.Nominator, Res.Nominator, T);
      finally
        FLocalBigNumberPool.Recycle(T);
      end;
    end;
    Res.Reduce;
  end;
end;

procedure BigRationalToBigDecimal(Res: TCnBigDecimal; Num: TCnBigRational;
  Digits: Integer = 20);
var
  S: string;
begin
  if (Res <> nil) and (Num <> nil) then
  begin
    S := Num.ToDec(Digits);
    if S <> '' then
      Res.SetDec(S);
  end;
end;

function BigDecimalDebugDump(Num: TCnBigDecimal): string;
begin
  Result := '10 Scale: ' + IntToStr(Num.FScale) + '. ' + BigNumberDebugDump(Num.FValue);
end;

{ TCnBigDecimal }

procedure TCnBigDecimal.AddWord(W: Cardinal);
var
  T: TCnBigDecimal;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    T.SetWord(W);
    BigDecimalAdd(Self, Self, T);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

constructor TCnBigDecimal.Create;
begin
  inherited;
  FValue := TCnBigNumber.Create;
end;

destructor TCnBigDecimal.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TCnBigDecimal.DivWord(W: Cardinal; DivPrecision: Integer);
var
  T: TCnBigDecimal;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    T.SetWord(W);
    BigDecimalDiv(Self, Self, T, DivPrecision);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

function TCnBigDecimal.GetDebugDump: string;
begin
  Result := BigDecimalDebugDump(Self);
end;

function TCnBigDecimal.GetDecString: string;
begin
  Result := BigDecimalToString(Self);
end;

function TCnBigDecimal.IsNegative: Boolean;
begin
  Result := FValue.IsNegative;
end;

function TCnBigDecimal.IsOne: Boolean;
begin
  Result := FValue.IsOne and (FScale = 0);
end;

function TCnBigDecimal.IsZero: Boolean;
begin
  Result := FValue.IsZero;
end;

procedure TCnBigDecimal.MulWord(W: Cardinal);
begin
  FValue.MulWord(W);
end;

procedure TCnBigDecimal.Negate;
begin
  FValue.Negate;
end;

procedure TCnBigDecimal.RoundTo(Precision: Integer;
  RoundMode: TCnBigRoundMode);
begin
  BigDecimalChangeToScale(Self, Self, Precision, RoundMode);
end;

function TCnBigDecimal.SetDec(const Buf: string): Boolean;
begin
  Result := BigDecimalSetDec(Buf, Self);
end;

procedure TCnBigDecimal.SetDouble(Value: Double);
begin
  BigDecimalSetDouble(Value, Self);
end;

procedure TCnBigDecimal.SetExtended(Value: Extended);
begin
  BigDecimalSetExtended(Value, Self);
end;

function TCnBigDecimal.SetInt64(W: Int64): Boolean;
begin
  Result := BigDecimalSetInt64(W, Self);
end;

procedure TCnBigDecimal.SetNegative(Neg: Boolean);
begin
  FValue.SetNegative(Neg);
end;

procedure TCnBigDecimal.SetOne;
begin
  FValue.SetOne;
  FScale := 0;
end;

procedure TCnBigDecimal.SetSingle(Value: Single);
begin
  BigDecimalSetSingle(Value, Self);
end;

function TCnBigDecimal.SetWord(W: Cardinal): Boolean;
begin
  Result := BigDecimalSetWord(W, Self);
end;

procedure TCnBigDecimal.SetZero;
begin
  FValue.SetZero;
  FScale := 0;
end;

procedure TCnBigDecimal.SubWord(W: Cardinal);
var
  T: TCnBigDecimal;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    T.SetWord(W);
    BigDecimalSub(Self, Self, T);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

function TCnBigDecimal.ToString: string;
begin
  Result := BigDecimalToString(Self);
end;

{ TCnBigDecimalPool }

function TCnBigDecimalPool.CreateObject: TObject;
begin
  Result := TCnBigDecimal.Create;
end;

function TCnBigDecimalPool.Obtain: TCnBigDecimal;
begin
  Result := TCnBigDecimal(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigDecimalPool.Recycle(Num: TCnBigDecimal);
begin
  inherited Recycle(Num);
end;

procedure BigBinaryClear(Num: TCnBigBinary);
begin
  if Num <> nil then
  begin
    Num.FValue.SetZero;
    Num.FScale := 0;
  end;
end;

function BigBinarySetDec(const Buf: string; Res: TCnBigBinary): Boolean;
var
  Neg, ENeg: Boolean;
  E, DC, DMax, I: Integer;
  P, DotPos: PChar;
  S, V: string;
  C: Char;
  P10, T, DRes: TCnBigNumber;
begin
  Result := False;

  V := '';
  S := Trim(Buf);
  P := PChar(S);
  if P^ = #0 then
    Exit;

  Neg := False;
  ENeg := False;
  DotPos := nil;

  if (P^ = '+') or (P^ = '-') then
  begin
    Neg := (P^ = '-');
    Inc(P);
  end;

  if P^ = #0 then
    Exit;

  Res.FValue.SetZero;
  DC := 0;

  // ����ֵ��ֱ����β�����Ͽ�ѧ�������� E
  C := P^;
  while (C <> #0) and (C <> 'e') and (C <> 'E') do
  begin
    case C of
      '0'..'9':
        V := V + C;
      ',':
        ; // �ֽںź���
      '.':
        if Assigned(DotPos) then
          // С����ֻ����һ��
          Exit
        else
          DotPos := P;
    else
      Exit;
    end;
    Inc(P);
    C := P^;
  end;

  // V �ǲ�����С�����ʮ�����ַ���
  if not Assigned(DotPos) and (C <> 'e') and (C <> 'E') then
  begin
    // ���ûС������û��ָ����˵��������
    Res.FValue.SetDec(AnsiString(V));
    if (not Res.FValue.IsNegative) and Neg then
      Res.FValue.SetNegative(True);

    Result := True;
  end;

  // ���������ԭ����С���㣬��� DC ��ֵ
  if Assigned(DotPos) then
    DC := P - DotPos - 1;

  E := 0;
  if (C = 'e') or (C = 'E') then
  begin
    // ��ѧ�������� E �����ָ��
    Inc(P);
    if (P^ = '+') or (P^ = '-') then
    begin
      ENeg := (P^ = '-');
      Inc(P);
    end;
    while P^ <> #0 do
    begin
      case P^ of
        '0'..'9':
          E := E * 10 + Ord(P^) - Ord('0');
      else
        Exit;
      end;
      Inc(P);
    end;
  end;

  if ENeg then
    E := -E;
  DC := DC - E; // �����ָ�����ٵ�������С�����ֳ��ȸ� DC

  // ����õ���ֵ��û��С����� V���Լ�ָʾ����Ӧ����ʮ����С����λ�õ� DC���ֿ�����
  if DC = 0 then
  begin
    Res.FValue.SetDec(AnsiString(V));
    Res.FScale := 0;
  end
  else if DC < 0 then // ��Ҫ���� 10^-DC����������
  begin
    Res.FValue.SetDec(AnsiString(V));
    BigNumberMulPower10(Res.FValue, -DC);
  end
  else // DC > 0��˵����С��
  begin
    if Length(V) > DC then
    begin
      S := Copy(V, 1, Length(V) - DC);             // S ���������ֵ��ַ���
      Delete(V, 1, Length(V) - DC);                // V ��С�����Ĳ��ֵ��ַ���
    end
    else if Length(V) = DC then
    begin
      S := '0';
      // V ����ԭ��
    end
    else // V ���ȱ� DC Ҫ���λ����ҪС��ǰ��Ҫ�� 0
    begin
      S := '0';
      V := StringOfChar('0', DC - Length(V)) + V;
    end;

    // �ֱ��� S �� V������ת��Ϊ������С������
    DMax := Trunc(Length(V) * 5);  // FIXME: С���������ת�� DMax λ����������ѭ��ͣ������
    if DMax < CN_BIG_BINARY_DEFAULT_PRECISION then
      DMax := CN_BIG_BINARY_DEFAULT_PRECISION;

    P10 := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;
    DRes := FLocalBigNumberPool.Obtain;

    try
      P10.SetOne;
      BigNumberMulPower10(P10, Length(V)); // ÿ�γ˺�Ҫ�� P10 �Ƚ��Ծ�����һλ�ǲ��� 1

      T.SetDec(AnsiString(V));
      I := 0;
      DRes.SetZero;

      while (I <= DMax) and not T.IsZero do
      begin
        T.MulWord(2);
        if BigNumberCompare(T, P10) >= 0 then
        begin
          DRes.ShiftLeftOne;
          DRes.SetBit(0);
          BigNumberSub(T, T, P10);
        end
        else
        begin
          DRes.ShiftLeftOne;
          // DRes.ClearBit(0);
        end;

        Inc(I);
      end;

      // �õ� I λ������ֵ���� DRes �����С������С�������ˣ�����������ƴ����
      T.SetDec(AnsiString(S));
      T.ShiftLeft(I);
      BigNumberAdd(Res.FValue, T, DRes);
      Res.FScale := I;
    finally
      FLocalBigNumberPool.Recycle(P10);
      FLocalBigNumberPool.Recycle(T);
      FLocalBigNumberPool.Recycle(DRes);
    end;
  end;

  if (not Res.FValue.IsNegative) and Neg then
    Res.FValue.SetNegative(True);

  Result := True;
end;

function BigBinarySetWord(W: Cardinal; Res: TCnBigBinary): Boolean;
begin
  Res.FValue.SetWord(W);
  Res.FScale := 0;
  Result := True;
end;

function BigBinarySetInt64(W: Int64; Res: TCnBigBinary): Boolean;
begin
  Res.FValue.SetInt64(W);
  Res.FScale := 0;
  Result := True;
end;

function InternalBigBinarySetFloat(Neg: Boolean; IntExponent: Integer; IntMantissa: TUInt64;
  Res: TCnBigBinary): Boolean;
var
  C: Integer;
begin
  C := GetUInt64LowBits(IntMantissa);  // ��� IntMantissa �ұߵ��㲢���� Exponent �Ի���
  if C > 0 then
  begin
    IntMantissa := IntMantissa shr C;
    Inc(IntExponent, C);
  end;

  // ֵ�� IntMantissa * 2^IntExponent
  BigNumberSetUInt64UsingInt64(Res.FValue, IntMantissa);
  if IntExponent > 0 then
  begin
    Res.FValue.ShiftLeft(IntExponent);   // ֱ����������������ָ����� 0
    Res.FScale := 0;
  end
  else // ָ���Ǹ���˵����С������
  begin
    IntExponent := -IntExponent;
    Res.FScale := IntExponent;
  end;

  Res.FValue.SetNegative(Neg);
  Result := True;
end;

function BigBinarySetSingle(Value: Single; Res: TCnBigBinary): Boolean;
var
  N: Boolean;
  E: Integer;
  S: Cardinal;
begin
  if SingleIsInfinite(Value) or SingleIsNan(Value) then
    raise ECnBigBinaryException.Create(SInvalidOp);

  if Value = 0.0 then
  begin
    Res.FValue.SetZero;
    Res.FScale := 0;
    Result := True;
    Exit;
  end;

  ExtractFloatSingle(Value, N, E, S);
  // �� 1. ��ͷ����Ч���ֵ���������E ��Ҫ�� 23
  Result := InternalBigBinarySetFloat(N, E - 23, TUInt64(S), Res);
end;

function BigBinarySetDouble(Value: Double; Res: TCnBigBinary): Boolean;
var
  N: Boolean;
  E: Integer;
  S: TUInt64;
begin
  if DoubleIsInfinite(Value) or DoubleIsNan(Value) then
    raise ECnBigBinaryException.Create(SInvalidOp);

  if Value = 0.0 then
  begin
    Res.FValue.SetZero;
    Res.FScale := 0;
    Result := True;
    Exit;
  end;

  ExtractFloatDouble(Value, N, E, S);
  // �� 1. ��ͷ����Ч���ֵ���������E ��Ҫ�� 52
  Result := InternalBigBinarySetFloat(N, E - 52, S, Res);
end;

function BigBinarySetExtended(Value: Extended; Res: TCnBigBinary): Boolean;
var
  N: Boolean;
  E: Integer;
  S: TUInt64;
begin
  if ExtendedIsInfinite(Value) or ExtendedIsNan(Value) then
    raise ECnBigBinaryException.Create(SInvalidOp);

  if Value = 0.0 then
  begin
    Res.FValue.SetZero;
    Res.FScale := 0;
    Result := True;
    Exit;
  end;

  ExtractFloatExtended(Value, N, E, S);
  // �� 1. ��ͷ����Ч���ֵ���������E ��Ҫ�� 63
  Result := InternalBigBinarySetFloat(N, E - 63, S, Res);
end;

function BigBinarySetBigNumber(Num: TCnBigNumber; Res: TCnBigBinary): Boolean;
begin
  Res.FScale := 0;
  Result := BigNumberCopy(Res.FValue, Num) <> nil;
end;

function BigBinaryToString(Num: TCnBigBinary): string;
var
  T, P10, S: TCnBigNumber;
  I: Integer;
  D: string;
begin
  Result := '';
  if Num <> nil then
  begin
    if Num.FScale = 0 then
    begin
      Result := Num.FValue.ToDec;
      Exit;
    end
    else if Num.FScale < 0 then
    begin
      T := FLocalBigNumberPool.Obtain;
      try
        BigNumberCopy(T, Num.FValue);
        T.ShiftLeft(-Num.FScale);
        Result := T.ToDec;
      finally
        FLocalBigNumberPool.Recycle(T);
      end;
    end
    else // FScale > 0����С�����֣��������������
    begin
      T := FLocalBigNumberPool.Obtain;
      S := nil;
      P10 := nil;

      try
        BigNumberCopy(T, Num.FValue);
        T.ShiftRight(Num.FScale);
        Result := T.ToDec;  // �����Ƶõ���������

        // �ٰ�ʣ�µ�ת����С��
        BigNumberCopy(T, Num.FValue);
        BigNumberKeepLowBits(T, Num.FScale); // ֻ����С������
        if T.IsZero then  // ���ûС�����֣���ֱ�ӷ�����
          Exit;

        S := FLocalBigNumberPool.Obtain;
        P10 := FLocalBigNumberPool.Obtain;
        S.SetZero;
        P10.SetOne;
        BigNumberMulPower10(P10, Num.FScale); // ������ T.GetBitsCount�����߿����� 0����С

        for I := Num.FScale - 1 downto 0 do
        begin
          P10.ShiftRightOne;
          if T.IsBitSet(I) then
            BigNumberAdd(S, S, P10);
        end;
        if S.IsZero then
          Exit;

        D := S.ToDec; // ע�� ToDec �󳤶ȿ��ܲ��� FScale ����ǰͷҪ����
        if Length(D) < Num.FScale then
          D := StringOfChar('0', Num.FScale - Length(D)) + D;
        Result := Result + '.' + D;
      finally
        FLocalBigNumberPool.Recycle(T);
        FLocalBigNumberPool.Recycle(S);
        FLocalBigNumberPool.Recycle(P10);
      end;
    end;
  end;
end;

function BigBinaryCompare(Num1, Num2: TCnBigBinary): Integer; overload;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    if Num2.FValue.IsZero then
      Result := 0   // ���� 0�����
    else if Num2.FValue.IsNegative then
      Result := 1   // 0 ���ڸ�
    else
      Result := -1; // 0 С����
  end
  else if Num2.FValue.IsZero then
  begin
    if not Num1.FValue.IsNegative then
      Result := 1     // ������ 0
    else
      Result := -1;   // ��С�� 0
  end
  else if Num1.FValue.IsNegative and not Num2.FValue.IsNegative then // ����Ϊ 0����С����
    Result := -1
  else if not Num1.FValue.IsNegative and Num2.FValue.IsNegative then // ����Ϊ 0�������ڸ�
    Result := 1
  else if Num1.FScale = Num2.FScale then // ������ͬ���ȿ�ָ���Ƿ���ͬ
    Result := BigNumberCompare(Num1.FValue, Num2.FValue)
  else // ������ͬ��ָ����ͬ
  begin
    // Ҫ�� Scale ���Ҳ����С���㿿�����������Խ�С�� Value��
    // ���� 2 ��ָ��������Զ���С���㣬�ٺ���һ���Ƚϣ����豣��ֵ���䣬���ԺͼӼ�������
    T := FLocalBigNumberPool.Obtain;
    L := CheckScaleAddRange(Num1.FScale, -Num2.FScale);

    try
      if L > 0 then
      begin
        BigNumberCopy(T, Num2.FValue);
        T.ShiftLeft(L);
        Result := BigNumberCompare(Num1.FValue, T);
      end
      else
      begin
        BigNumberCopy(T, Num1.FValue);
        L := -L;
        T.ShiftLeft(L);
        Result := BigNumberCompare(T, Num2.FValue);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

function BigBinaryCompare(Num1: TCnBigBinary; Num2: Int64): Integer; overload;
var
  T: TCnBigBinary;
begin
  if not Num1.IsNegative and (Num2 < 0) then
    Result := 1
  else if Num1.IsNegative and (Num2 > 0) then
    Result := -1
  else if Num1.IsZero and (Num2 = 0) then
    Result := 0
  else
  begin
    T := FLocalBigBinaryPool.Obtain;
    try
      T.FScale := 0;
      T.FValue.SetInt64(Num2);
      Result := BigBinaryCompare(Num1, T);
    finally
      FLocalBigBinaryPool.Recycle(T);
    end;
  end;
end;

// ͨ��ֵ��������ı任�ô�����Ƹ���������Ч���������ض�λ��������ʱ�ضϣ�����ʱ�� 2 �������η���ȫ����ͬʱ������ FScale
function InternalBigBinaryChangeToBitsCount(Num: TCnBigBinary; BitsCount: Integer): Boolean;
var
  C, D: Integer;
begin
  Result := False;
  if Num <> nil then
  begin
    C := Num.FValue.GetBitsCount;
    if C < BitsCount then
    begin
      D := BitsCount - C;
      Num.FValue.ShiftLeft(D);
      Num.FScale := Num.FScale + D;
    end
    else if C > BitsCount then
    begin
      D := C - BitsCount;  // Ҫ�ص� D ��λ��Ҳ����Ҫ�� FScale ���� D��
      BigBinaryChangeToScale(Num, Num, Num.FScale - D);
    end;
    Result := True;
  end;
end;

function BigBinaryToSingle(Num: TCnBigBinary): Single;
var
  T: TCnBigBinary;
  E: Integer;
  M: Cardinal;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    BigBinaryCopy(T, Num);
    InternalBigBinaryChangeToBitsCount(T, CN_SINGLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // ������λ�� 1

    M := T.FValue.GetWord;
    E := -T.FScale;

    CombineFloatSingle(Num.IsNegative, E + CN_SINGLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function BigBinaryToDouble(Num: TCnBigBinary): Double;
var
  T: TCnBigBinary;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    BigBinaryCopy(T, Num);
    InternalBigBinaryChangeToBitsCount(T, CN_DOUBLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // ������λ�� 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatDouble(Num.IsNegative, E + CN_DOUBLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function BigBinaryToExtended(Num: TCnBigBinary): Extended;
var
  T: TCnBigBinary;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    BigBinaryCopy(T, Num);
    InternalBigBinaryChangeToBitsCount(T, CN_EXTENDED_SIGNIFICAND_BITLENGTH + 1);
    // ����������λ�� 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatExtended(Num.IsNegative, E + CN_EXTENDED_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function BigBinaryCompare(Num1: TCnBigBinary; Num2: Extended): Integer; overload;
var
  T: TCnBigBinary;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    T.SetExtended(Num2);
    Result := BigBinaryCompare(Num1, T);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

procedure BigBinaryCopy(Dest: TCnBigBinary; Source: TCnBigBinary);
begin
  if (Source <> nil) and (Dest <> nil) and (Source <> Dest) then
  begin
    BigNumberCopy(Dest.FValue, Source.FValue);
    Dest.FScale := Source.FScale;
  end;
end;

function BigBinaryGetHighScale(Num: TCnBigBinary): Integer;
begin
  Result := 0;
  if Num <> nil then
  begin
    Result := Num.FValue.GetBitsCount;
    // С������� FScale λ����ȥ��Ч����
    Result := Num.FScale - Result + 1;
    if Result <= 0 then // С����ǰ�ڼ�λ�Ǵ� 1 ��ʼ��
      Dec(Result)
  end;
end;

function BigBinaryAdd(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary): Boolean;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    BigBinaryCopy(Res, Num2);
    Result := True;
    Exit;
  end
  else if Num2.FValue.IsZero then
  begin
    BigBinaryCopy(Res, Num1);
    Result := True;
    Exit;
  end
  else if Num1.FScale = Num2.FScale then
  begin
    // ָ����ֱͬ�Ӽ�
    Res.FScale := Num1.FScale;
    Result := BigNumberAdd(Res.FValue, Num1.FValue, Num2.FValue);
    Exit;
  end
  else
  begin
    // Ҫ�� Scale С��Ҳ����С���㿿�����������Խϴ�� Value��
    // ���� 10 ��ָ������ݲ���С��ͬ�ȵ� Scale �Զ���С���㲢������ֵ���䣬
    // �ٺ���һ����ӣ������ Scale ȡС��
    T := FLocalBigNumberPool.Obtain;
    L := CheckScaleAddRange(Num1.FScale, -Num2.FScale);

    try
      if L > 0 then
      begin
        BigNumberCopy(T, Num2.FValue);
        T.ShiftLeft(L);
        Res.FScale := Num1.FScale;
        Result := BigNumberAdd(Res.FValue, Num1.FValue, T);
      end
      else
      begin
        BigNumberCopy(T, Num1.FValue);
        L := -L;
        T.ShiftLeft(L);
        Res.FScale := Num2.FScale;
        Result := BigNumberAdd(Res.FValue, T, Num2.FValue);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

function BigBinarySub(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary): Boolean;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    BigNumberCopy(Num2.FValue, Res.FValue);
    Res.FValue.Negate;
    Result := True;
    Exit;
  end
  else if Num2.FValue.IsZero then
  begin
    BigNumberCopy(Num1.FValue, Res.FValue);
    Result := True;
    Exit;
  end
  else if Num1.FScale = Num2.FScale then
  begin
    // ָ����ֱͬ�Ӽ�
    Res.FScale := Num1.FScale;
    Result := BigNumberSub(Res.FValue, Num1.FValue, Num2.FValue);
    Exit;
  end
  else
  begin
    // Ҫ�� Scale С��Ҳ����С���㿿�����������Խϴ�� Value��
    // ���� 10 ��ָ������ݲ���С��ͬ�ȵ� Scale �Զ���С���㲢������ֵ���䣬
    // �ٺ���һ������������ Scale ȡС��
    T := FLocalBigNumberPool.Obtain;
    L := CheckScaleAddRange(Num1.FScale, -Num2.FScale);

    try
      if L > 0 then
      begin
        BigNumberCopy(T, Num2.FValue);
        T.ShiftLeft(L);
        Res.FScale := Num1.FScale;
        Result := BigNumberSub(Res.FValue, Num1.FValue, T);
      end
      else
      begin
        BigNumberCopy(T, Num1.FValue);
        L := -L;
        T.ShiftLeft(L);
        Res.FScale := Num2.FScale;
        Result := BigNumberSub(Res.FValue, T, Num2.FValue);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

function BigBinaryMul(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary; MulPrecision: Integer = 0): Boolean;
begin
  if Num1.FValue.IsZero or Num2.FValue.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end
  else
  begin
    Res.FScale := CheckScaleAddRange(Num1.FScale, Num2.FScale);
    Result := BigNumberMul(Res.FValue, Num1.FValue, Num2.FValue);
    if Result and (MulPrecision > 0) then
      Result := BigBinaryRoundToDigits(Res, Res, MulPrecision, drTowardsZero);
  end;
end;

function BigBinaryDiv(Res: TCnBigBinary; Num1: TCnBigBinary; Num2: TCnBigBinary;
  DivPrecision: Integer = 0): Boolean;
var
  S: Boolean;
  M, TS: Integer;
  T, R: TCnBigNumber;
begin
  if Num2.FValue.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if Num1.FValue.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  // ������
  S := Num1.FValue.isNegative <> Num2.FValue.IsNegative; // ���Ų��Ƚ���Ÿ�
  TS := Num1.FScale - Num2.FScale;

  if DivPrecision <= 0 then
    DivPrecision := FDefaultBinaryPrecisionDigits;
  if DivPrecision < 0 then
    DivPrecision := CN_BIG_BINARY_DEFAULT_PRECISION;

  // ���ݾ���Ҫ����㽫����������ı���
  M := CheckScaleAddRange(DivPrecision, (Num2.FValue.GetBitsCount - Num1.FValue.GetBitsCount + 1));
  if M < 0 then // �������󡢾����Ѿ��㹻
    M := 0
  else if M > 0 then
    TS := CheckScaleAddRange(TS, M); // ����ı������������

  T := nil;
  R := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberCopy(T, Num1.FValue);
    T.ShiftLeft(M);

    R := FLocalBigNumberPool.Obtain;
    BigNumberDiv(Res.FValue, R, T, Num2.FValue);  // Num1.FValue * 2 ^ M div Num2.FValue �õ��̺�����

    // ֱ�� Trunc ������������
    Res.FScale := TS;
    // TODO: ������Լ��

    BigBinaryRoundToDigits(Res, Res, DivPrecision, drTowardsZero);
    Res.FValue.SetNegative(S);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(R);
  end;
end;

procedure BigBinaryShiftLeft(Res: TCnBigBinary; N: Integer);
begin
  Dec(Res.FScale, N);
end;

procedure BigBinaryShiftRight(Res: TCnBigBinary; N: Integer);
begin
  Inc(Res.FScale, N);
end;

function BigBinaryPower(Res: TCnBigBinary; N: Integer): Boolean;
begin
  Result := False;
  if N = 0 then
  begin
    if Res.IsZero then
      raise EZeroDivide.Create(SDivByZero);
    Res.SetOne;
  end
  else if N > 0 then
  begin
    Res.FScale := Res.FScale * N;
    Result := Res.FValue.PowerWord(N);
  end;
end;

procedure RoundBinaryByMode(Quotient: TCnBigNumber; RemainderSet: Boolean; QWillBeNeg: Boolean;
  Mode: TCnBigRoundMode);
begin
  case Mode of
    drAwayFromZero:            // ������ֵ�����ȡ
      begin
        BigNumberAddWord(Quotient, 1);
      end;
    drTowardsZero:             // ������ֵС����ȡ������ֻ���������ֵ� Trunc
      begin
        // ɶ��������
      end;
    drCeilingToInfinite:       // ���������ȡ
      begin
        if not QWillBeNeg then
          BigNumberAddWord(Quotient, 1);
      end;
    drFloorToNegInfinite:      // ���������ȡ
      begin
        if QWillBeNeg then
          BigNumberAddWord(Quotient, 1);
      end;
    drRound:
      begin
        if RemainderSet then // �������λ�� 1
          BigNumberAddWord(Quotient, 1);
      end;
  else
    raise ECnBigBinaryException.Create(SCnRoundModeNotSupport);
  end;
end;

function BigBinaryChangeToScale(Res: TCnBigBinary; Num: TCnBigBinary;
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
var
  DS: Integer;
  B, Neg: Boolean;
begin
  DS := CheckScaleAddRange(Num.FScale, -Scale);
  if DS > 0 then // �µ�С������λ����ԭ���٣�Ҫ��֮������
  begin
    Neg := Num.FValue.IsNegative;
    Num.FValue.SetNegative(False);

    B := Num.FValue.IsBitSet(DS - 1); // ֱ�ӻ�ȡ�������λ
    BigNumberCopy(Res.FValue, Num.FValue);
    Res.FValue.ShiftRight(DS);

    // ֱ�Ӹ������ƺ���̺��������λ�Լ������������
    RoundBinaryByMode(Res.FValue, B, Neg, RoundMode);

    Res.FScale := Scale;
    Res.FValue.SetNegative(Neg);

    if Res <> Num then           // ��� Num �Ƕ����ģ�����Ҫ��ԭ�� Neg
      Num.FValue.SetNegative(Neg);
    Result := True;
  end
  else // �µ�С����λ����ԭ�����࣬�򵥱任һ�¾���
  begin
    BigNumberCopy(Res.FValue, Num.FValue);
    if DS < 0 then
      Res.FValue.ShiftLeft(-DS);
    Res.FScale := Scale;
    Result := True;
  end;
end;

function BigBinaryRoundToDigits(Res: TCnBigBinary; Num: TCnBigBinary;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
var
  DS: Integer;
  B, Neg: Boolean;
begin
  Result := False;
  DS := CheckScaleAddRange(Num.FScale, -Digits);

  if DS > 0 then // �µ�С������λ���ñ�ԭ���٣���������
  begin
    Neg := Num.FValue.IsNegative;
    Num.FValue.SetNegative(False);

    B := Num.FValue.IsBitSet(DS - 1); // ֱ�ӻ�ȡ�������λ
    BigNumberCopy(Res.FValue, Num.FValue);
    Res.FValue.ShiftRight(DS);

    // ֱ�Ӹ������ƺ���̺��������λ�Լ������������
    RoundBinaryByMode(Res.FValue, B, Neg, RoundMode);

    Res.FScale := Digits;
    Res.FValue.SetNegative(Neg);

    if Res <> Num then           // ��� Num �Ƕ����ģ�����Ҫ��ԭ�� Neg
      Num.FValue.SetNegative(Neg);
    Result := True;
  end;
end;

function BigBinaryTrunc(Res: TCnBigBinary; Num: TCnBigBinary): Boolean;
begin
  if Num.FScale <= 0 then // ��С������
  begin
    BigBinaryCopy(Res, Num);
    Result := True;
    Exit;
  end
  else // ��С������ FScale λ���ɵ�
  begin
    Result := BigBinaryChangeToScale(Res, Num, 0, drTowardsZero);
  end;
end;

function BigBinaryTruncTo(Res: TCnBigNumber; Num: TCnBigBinary): Boolean;
var
  T: TCnBigBinary;
begin
  if Num.FScale <= 0 then // ��С������
  begin
    BigNumberCopy(Res, Num.FValue);
    Res.ShiftLeft(-Num.FScale);

    Result := True;
    Exit;
  end
  else // ��С������ FScale λ���ɵ�
  begin
    T := FLocalBigBinaryPool.Obtain;
    try
      Result := BigBinaryChangeToScale(T, Num, 0, drTowardsZero);
      BigNumberCopy(Res, T.FValue); // Scale �Ѿ�Ϊ 0 �˿���ֱ�Ӻ���
    finally
      FLocalBigBinaryPool.Recycle(T);
    end;
  end;
end;

function BigBinaryDebugDump(Num: TCnBigBinary): string;
begin
  Result := '2 Scale: ' + IntToStr(Num.FScale) + '. ' + BigNumberDebugDump(Num.FValue);
end;

{ TCnBigBinary }

procedure TCnBigBinary.AddWord(W: Cardinal);
var
  T: TCnBigBinary;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    T.SetWord(W);
    BigBinaryAdd(Self, Self, T);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

constructor TCnBigBinary.Create;
begin
  inherited;
  FValue := TCnBigNumber.Create;
end;

destructor TCnBigBinary.Destroy;
begin
  FValue.Free;
  inherited;
end;

procedure TCnBigBinary.DivWord(W: Cardinal; DivPrecision: Integer);
var
  T: TCnBigBinary;
begin
  if W = 0 then
    raise EDivByZero.Create(SDivByZero);

  while (W and 1) = 0 do
  begin
    W := W shr 1;
    Inc(FScale);
  end;

  if W = 1 then // ������ 2 �������η�
    Exit;

  T := FLocalBigBinaryPool.Obtain;
  try
    T.SetWord(W);
    BigBinaryDiv(Self, Self, T, DivPrecision);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function TCnBigBinary.GetDebugDump: string;
begin
  Result := BigBinaryDebugDump(Self);
end;

function TCnBigBinary.GetDecString: string;
begin
  Result := BigBinaryToString(Self);
end;

function TCnBigBinary.IsNegative: Boolean;
begin
  Result := FValue.IsNegative;
end;

function TCnBigBinary.IsZero: Boolean;
begin
  Result := FValue.IsZero;
end;

procedure TCnBigBinary.MulWord(W: Cardinal);
begin
  FValue.MulWord(W);
end;

procedure TCnBigBinary.Negate;
begin
  FValue.Negate;
end;

procedure TCnBigBinary.Power(N: Integer);
begin
  BigBinaryPower(Self, N);
end;

procedure TCnBigBinary.SetBigNumber(Value: TCnBigNumber);
begin
  BigBinarySetBigNumber(Value, Self);
end;

function TCnBigBinary.SetDec(const Buf: string): Boolean;
begin
  Result := BigBinarySetDec(Buf, Self);
end;

procedure TCnBigBinary.SetDouble(Value: Double);
begin
  BigBinarySetDouble(Value, Self);
end;

procedure TCnBigBinary.SetExtended(Value: Extended);
begin
  BigBinarySetExtended(Value, Self);
end;

function TCnBigBinary.SetInt64(W: Int64): Boolean;
begin
  Result := BigBinarySetInt64(W, Self);
end;

procedure TCnBigBinary.SetNegative(Neg: Boolean);
begin
  FValue.SetNegative(Neg);
end;

procedure TCnBigBinary.SetOne;
begin
  FValue.SetOne;
  FScale := 0;
end;

procedure TCnBigBinary.SetSingle(Value: Single);
begin
  BigBinarySetSingle(Value, Self);
end;

function TCnBigBinary.SetWord(W: Cardinal): Boolean;
begin
  Result := BigBinarySetWord(W, Self);
end;

procedure TCnBigBinary.SetZero;
begin
  FValue.SetZero;
  FScale := 0;
end;

procedure TCnBigBinary.ShiftLeft(N: Integer);
begin
  BigBinaryShiftLeft(Self, N);
end;

procedure TCnBigBinary.ShiftRight(N: Integer);
begin
  BigBinaryShiftRight(Self, N);
end;

procedure TCnBigBinary.SubWord(W: Cardinal);
var
  T: TCnBigBinary;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    T.SetWord(W);
    BigBinarySub(Self, Self, T);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function TCnBigBinary.ToString: string;
begin
  Result := BigBinaryToString(Self);
end;

{ TCnBigBinaryPool }

function TCnBigBinaryPool.CreateObject: TObject;
begin
  Result := TCnBigBinary.Create;
end;

function TCnBigBinaryPool.Obtain: TCnBigBinary;
begin
  Result := TCnBigBinary(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigBinaryPool.Recycle(Num: TCnBigBinary);
begin
  inherited Recycle(Num);
end;

initialization
  FLocalBigDecimalPool := TCnBigDecimalPool.Create;
  FLocalBigBinaryPool := TCnBigBinaryPool.Create;
  FLocalBigNumberPool := TCnBigNumberPool.Create;

  CnBigDecimalOne := TCnBigDecimal.Create;
  CnBigDecimalOne.SetOne;
  CnBigDecimalZero := TCnBigDecimal.Create;
  CnBigDecimalZero.SetZero;

finalization
//  CnBigDecimalZero.DecString; // �ֹ������������ֹ������������
//  CnBigDecimalZero.DebugDump;

  CnBigDecimalZero.Free;
  CnBigDecimalOne.Free;

  FLocalBigNumberPool.Free;
  FLocalBigBinaryPool.Free;
  FLocalBigDecimalPool.Free;

end.
