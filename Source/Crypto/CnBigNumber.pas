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

unit CnBigNumber;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�����������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ���˴����� TCnBigNumber �ļӼ��˳��ȸ������㼰������ TCnBigNumberPool��
*           ��������ʵ�ֻ���һ���ֲο� OpenSSL �� C �����д������������Ĭ��֧�ֶ��̡߳�
*
*           Word ϵ�в�������ָ�������� UInt32/UInt64 �������㡣
*           Words ϵ�в�������ָ�������м��������̡�
*
*           ������ʵ���� TCnBigNumber �ڲ���һ�� UInt32/UInt64 �����ʾ������
*           �������� BN_DATA_USE_64 ��������Ԫ���� UInt32 ���� UInt64��Ĭ��ǰ�ߡ�
*           ����Խ����Խ����������ĸ�λ������Ԫ���ڲ���ֵ������ CPU �Ĵ�С����
*           �����С�� CPU �ϣ������������ֵ�ϸ���ڸ����鰴�ֽڵ�������������
*
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ�Win32/Win64/MACOS D5~Delphi ���°档
*           ע��D5/D6/CB5/CB6 ���������ϱ����� Bug �޷��޸���
*           Ʃ��д Int64(AInt64Var) ������ǿ������ת��ʱ��������ʱ�ƹ��ˡ�
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2025.04.24 V3.0
*               ���� Lucas U V ���еļ��㼰 BPSW �����ж��㷨
*           2025.04.07 V2.9
*               ���뼸���б��·������˷��׵ļ��㣬�����������Ϊ�ǳ�ʼ���汾������
*           2024.11.05 V2.8
*               �����ſɱȷ��ŵļ��㼰÷ɭ�������ж�
*           2024.10.10 V2.7
*               ���ֺ�������Ĳ�����⣬���ڵ���ʱ���������ظ�ʱ���쳣
*           2023.01.12 V2.6
*               64 λģʽ�������� 64 λ�洢�����ģʽ�������У�Ĭ�Ͻ���
*           2022.06.04 V2.5
*               ���Ӹ�ģ��Ԫ���ɸ�����Լ���Լ����ڴ�ʵ�ֵĿ���ģ���㷨
*               ���� 2048 Bits ��Χ���ƺ���ֱ�ӳ���ģҪ�����٣�
*               �� UInt64 ��Χ�ڵ�ʡʱЧ����ͬ�����ܺ�ʱ�ڱ�
*           2022.04.26 V2.4
*               �޸� LongWord �� Integer ��ַת����֧�� MacOS64
*           2021.12.08 V2.3
*               ʵ���� Extended ��չ���ȸ�������˳�������һ���������������� AKS
*           2021.12.04 V2.2
*               ʵ���� Extended ��չ���ȸ���������ת��
*           2021.11.29 V2.2
*               ʵ��һ��ϡ��Ĵ����б���
*           2021.11.23 V2.1
*               ʵ������������Ĵ���
*           2021.09.20 V2.0
*               ʵ�ִ�����λ����
*           2021.09.05 V1.9
*               ʵ����ȫ�ݵ��ж�
*           2021.04.02 V1.8
*               POSIX 64 �� LongWord �� 64 λ��Ǩ��
*           2020.07.04 V1.7
*               �����������ض��󣬼�����߳̿���
*           2020.06.20 V1.6
*               ������ٳ˷�������ʮ����λ������
*           2020.01.16 V1.5
*               �Ż��˷��� MulMod ���ٶȣ�ȥ��������
*           2019.04.16 V1.4
*               ֧�� Win32/Win64/MacOS32
*           2017.04.04 V1.3
*               ����������������ص� Bug������չŷ�������ⷨ��������
*           2016.09.26 V1.2
*               �����������㣻�����ظĳ�ȫ�ַ�ʽ�����Ч��
*           2014.11.05 V1.1
*               �����ӽṹ��ʽ��Ϊ����ʽ�����Ӳ��ַ���
*           2014.10.15 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$UNDEF BN_DATA_USE_64}
{$IFDEF CPU64BITS}
  // {$DEFINE BN_DATA_USE_64}
  // BN_DATA_USE_64 ��ʾ�� 64 λ�£��ڲ�ʹ�� 64 λԪ�ؽ��д洢���������Ч�ʣ�������
  // �粻���壬Ĭ��ʹ�� 32 λԪ�أ����ȶ�
{$ENDIF}

uses
  Classes, SysUtils, Math, SysConst, CnNative {$IFDEF MSWINDOWS}, Windows {$ENDIF},
  Contnrs, CnContainers, CnHashMap, CnRandom
  {$IFNDEF COMPILER5}, Types {$ENDIF}
  {$IFDEF BN_DATA_USE_64}, CnInt128 {$ENDIF}
  {$IFDEF UNICODE}, AnsiStrings {$ENDIF};

const
  CN_BN_MILLER_RABIN_DEF_COUNT = 50;
  {* Miller-Rabin �㷨��Ĭ�ϲ��Դ���}

type
{$IFDEF SUPPORT_UINT64}
  TUInt64Array = array [0..MaxInt div SizeOf(UInt64) - 1] of UInt64;
  {* UInt64 ��������}
  PUInt64Array = ^TUInt64Array;
  {* UInt64 ����ָ������}
{$ENDIF}

{$IFDEF BN_DATA_USE_64}
  // �����ڲ�Ԫ�أ�ʹ�� 64 λ
  TCnBigNumberElement = UInt64;
  {* �����ڲ�Ԫ�����ͣ�64 λ}
  PCnBigNumberElement = PUInt64;
  {* �����ڲ�Ԫ��ָ�����ͣ�64 λ}
  PCnBigNumberElementArray = PUInt64Array;
  {* �����ڲ�Ԫ������ָ�����ͣ�64 λ}
{$ELSE}
  // �����ڲ�Ԫ�أ�ʹ�� 32 λ
  TCnBigNumberElement = Cardinal;
  {* �����ڲ�Ԫ�����ͣ�32 λ}
  PCnBigNumberElement = PCardinal;
  {* �����ڲ�Ԫ��ָ�����ͣ�32 λ}
  PCnBigNumberElementArray = PCnLongWord32Array;
  {* �����ڲ�Ԫ������ָ�����ͣ�32 λ}
{$ENDIF}

  ECnBigNumberException = class(Exception);
  {* ��������쳣}

  TCnBigNumber = class(TObject)
  {* ��������һ�������Ķ���}
  private
{$IFDEF DEBUG}
    FIsFromPool: Boolean;
{$ENDIF}
    function GetDecString: string;
    function GetHexString: string;
    function GetDebugDump: string;
  public
    D: PCnBigNumberElement;
    // һ�� array[0..Top-1] of UInt32/UInt64 ���飬Ԫ��Խ����Խ�����λ��Ԫ���ڲ����� CPU �ֽ���
    // �� x86 ����С�� CPU �ϣ��ô���ֵ�ϸ���ڱ������ֽڵ�������������
    // �����ÿ��Ԫ���ڲ�������ȫ���Ӹߵ��Ͷ��ֽڣ��ŷ��Ͽɶ���Ҫ�󣬾���Ҳû������
    // ����� ToBinary/FromBinary/SetBinary ʱ���и�Ԫ�ؼ�ĵ�����̣�
    // ����Ԫ���ڲ����ֽ��ڶ�ȡд��ʱʹ���˲��ֽ�ƴ�ӣ����Ĩƽ�� CPU �Ĵ�С������
    // ������Ӧ�� Binary �ڴ�����ӵ͵�ַ���ߵ�ַÿ���ֽڶ�����������Ķ�ϰ�ߣ����� CPU �Ĵ�С����ɶ

    Top: Integer;
    // Top ��ʾ�������ޣ�Ҳ���� Top ����Ч UInt32/UInt64��D[Top - 1] �����λ��Ч�����ڵ� UInt32/UInt64

    DMax: Integer;
    // D �����ѷ���Ĵ洢���ޣ���λ�� UInt32/UInt64 �������ڻ���� Top������������

    Neg: Integer;
    // 1 Ϊ����0 Ϊ��

    constructor Create; virtual;
    {* ���캯��}

    destructor Destroy; override;
    {* ��������}

    procedure Init;
    {* ��ʼ��Ϊȫ 0������Ϊ D �����ڴ�}

    procedure Clear;
    {* ���������ݿռ��� 0�������ͷ� D �ڴ�}

    function IsZero: Boolean;
    {* ���ش����Ƿ�Ϊ 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 0
    }

    function SetZero: Boolean;
    {* ����������Ϊ 0�������Ƿ����óɹ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ����óɹ�
    }

    function IsOne: Boolean;
    {* ���ش����Ƿ�Ϊ 1��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 1
    }

    function IsNegOne: Boolean;
    {* ���ش����Ƿ�Ϊ -1��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ -1
    }

    function SetOne: Boolean;
    {* ����������Ϊ 1�������Ƿ����óɹ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ����óɹ�
    }

    function IsOdd: Boolean;
    {* ���ش����Ƿ�Ϊ������

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ����
    }

    function IsEven: Boolean;
    {* ���ش����Ƿ�Ϊż����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊż��
    }

    function GetBitsCount: Integer;
    {* ���ش����ж��ٸ���Ч Bits λ��

       ������
         ���ޣ�

       ����ֵ��Integer                    - ������Чλ��
    }

    function GetBytesCount: Integer;
    {* ���ش����ж��ٸ���Ч Bytes �ֽڡ�

       ������
         ���ޣ�

       ����ֵ��Integer                    - ������Ч�ֽ���
    }

    function GetWordCount: Integer;
    {* ���ش����ж��ٸ���Ч UInt32/UInt64 Ԫ�ء�

       ������
         ���ޣ�

       ����ֵ��Integer                    - ������ЧԪ����
    }

    function GetTenPrecision: Integer;
    {* ���ش����ж��ٸ�ʮ����λ��

       ������
         ���ޣ�

       ����ֵ��Integer                    - ����ʮ����λ��
    }

    function IsCardinal: Boolean;
    {* �����Ƿ���һ�� 32 λ�޷���������Χ�ڵ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                        - �����Ƿ��� 32 λ�޷���������Χ��
    }

    function GetCardinal: Cardinal;
    {* ȡ 32 λ�޷�������ֵ���糬�磬���� $FFFFFFFF��

       ������
         ���ޣ�

       ����ֵ��Cardinal                   - ���� 32 λ�޷�������
    }

    function SetCardinal(W: Cardinal): Boolean;
    {* �������� 32 λ�޷�������ֵ��

       ������
         W: Cardinal                      - ����ֵ�� 32 λ�޷�������

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    function GetWord: Cardinal;
    {* ȡ 32 λ�޷�������ֵ���糬�磬���� $FFFFFFFF��

       ������
         ���ޣ�

       ����ֵ��Cardinal                   - ���� 32 λ�޷�������
    }

    function SetWord(W: Cardinal): Boolean;
    {* �������� 32 λ�޷�������ֵ��

       ������
         W: Cardinal                      - ����ֵ�� 32 λ�޷�������

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    function IsInteger: Boolean;
    {* �����Ƿ���һ�� 32 λ�з���������Χ�ڵ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                        - �����Ƿ��� 32 λ�з���������Χ��
    }

    function GetInteger: Integer;
    {* ȡ 32 λ�з�������ֵ���糬�磬���� $7FFFFFFF��

       ������
         ���ޣ�

       ����ֵ��Integer                    - ���� 32 λ�з�������
    }

    function SetInteger(W: Integer): Boolean;
    {* �������� 32 λ�з�������ֵ��

       ������
         W: Integer                       - ����ֵ�� 32 λ�з�������

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    function IsInt64: Boolean;
    {* �����Ƿ���һ�� 64 λ�з���������Χ�ڵ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                        - �����Ƿ��� 64 λ�з���������Χ��
    }

    function GetInt64: Int64;
    {* ȡ 64 λ�з�������ֵ���糬�磬���� $7FFFFFFFFFFFFFFF��

       ������
         ���ޣ�

       ����ֵ��Int64                      - ���� 64 λ�з�������
    }

    function SetInt64(W: Int64): Boolean;
    {* �������� 64 λ�з�������ֵ��

       ������
         W: Int64                         - ����ֵ�� 64 λ�з�������

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    function IsUInt64: Boolean;
    {* �����Ƿ���һ�� 64 λ�޷���������Χ�ڵ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                        - �����Ƿ��� 64 λ�޷���������Χ��
    }

{$IFDEF SUPPORT_UINT64}

    function GetUInt64: UInt64;
    {* ȡ 64 λ�޷�������ֵ���糬�磬���� $FFFFFFFFFFFFFFFF��

       ������
         ���ޣ�

       ����ֵ��UInt64                     - ���� 64 λ�޷�������
    }

    function SetUInt64(W: UInt64): Boolean;
    {* �������� 64 λ�޷�������ֵ��

       ������
         W: UInt64                        - ����ֵ�� 64 λ�޷�������

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

{$ENDIF}

    function IsWord(W: TCnBigNumberElement): Boolean;
    {* �����Ƿ����ָ�� UInt32/UInt64 Ԫ�ء�

       ������
         W: TCnBigNumberElement           - ���Ƚϵ�Ԫ��ֵ

       ����ֵ��Boolean                    - �����Ƿ����
    }

    function AddWord(W: TCnBigNumberElement): Boolean;
    {* ��������һ�� UInt32/UInt64 Ԫ�أ�����Է������У���������Ƿ�ɹ���

       ������
         W: TCnBigNumberElement           - ����Ԫ��

       ����ֵ��Boolean                    - ��������Ƿ�ɹ�
    }

    function SubWord(W: TCnBigNumberElement): Boolean;
    {* ������ȥһ�� UInt32/UInt64 Ԫ�أ�����Է������У���������Ƿ�ɹ���

       ������
         W: TCnBigNumberElement           - ����Ԫ��

       ����ֵ��Boolean                    - ��������Ƿ�ɹ�
    }

    function MulWord(W: TCnBigNumberElement): Boolean;
    {* ��������һ�� UInt32/UInt64 Ԫ�أ�����Է������У���������Ƿ�ɹ���

       ������
         W: TCnBigNumberElement           - ����Ԫ��

       ����ֵ��Boolean                    - ��������Ƿ�ɹ�
    }

    function ModWord(W: TCnBigNumberElement): TCnBigNumberElement;
    {* ������һ�� UInt32/UInt64 Ԫ�����࣬����������

       ������
         W: TCnBigNumberElement           - ����Ԫ��

       ����ֵ��TCnBigNumberElement        - ��������
    }

    function DivWord(W: TCnBigNumberElement): TCnBigNumberElement;
    {* ��������һ�� UInt32/UInt64 Ԫ�أ������·��������У�����������

       ������
         W: TCnBigNumberElement           - ����Ԫ��

       ����ֵ��TCnBigNumberElement        - ��������
    }

    function PowerWord(W: Cardinal): Boolean;
    {* �����˷���������·��������У����س˷��Ƿ�ɹ���

       ������
         W: Cardinal                      - �˷���ָ��

       ����ֵ��Boolean                    - ���س˷��Ƿ�ɹ�
    }

    procedure SetNegative(Negative: Boolean);
    {* ���ô����Ƿ�ֵ��

       ������
         Negative: Boolean                - �����Ƿ�Ϊ��ֵ

       ����ֵ�����ޣ�
    }

    function IsNegative: Boolean;
    {* ���ش����Ƿ�ֵ��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ��ֵ
    }

    procedure Negate;
    {* �������෴����Ҳ�������ŷ���}

    procedure ShiftLeftOne;
    {* ���� 1 λ}

    procedure ShiftRightOne;
    {* ���� 1 λ}

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

    function ClearBit(N: Integer): Boolean;
    {* �������ĵ� N �� Bit �� 0�����سɹ����N �����λ 0 �����λ GetBitsCount - 1��

       ������
         N: Integer                       - ���� 0 ��λ����

       ����ֵ��Boolean                    - �����Ƿ��� 0 �ɹ�
    }

    function SetBit(N: Integer): Boolean;
    {* �������ĵ� N �� Bit �� 1�����سɹ����N �����λ 0 �����λ GetBitsCount - 1��

       ������
         N: Integer                       - ���� 1 ��λ����

       ����ֵ��Boolean                    - �����Ƿ��� 1 �ɹ�
    }

    function IsBitSet(N: Integer): Boolean;
    {* ���ش����ĵ� N �� Bit �Ƿ�Ϊ 1��N �����λ 0 �����λ GetBitsCount - 1��

       ������
         N: Integer                       - λ����

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 1
    }

    function WordExpand(Words: Integer): TCnBigNumber;
    {* ��������չ��֧�� Words �� UInt32/UInt64 Ԫ�أ��ɹ�������չ�Ĵ��������� Self��ʧ�ܷ��� nil��

       ������
         Words: Integer                   - ����չ��Ԫ����

       ����ֵ��TCnBigNumber               - �ɹ��򷵻ش���������ʧ�ܷ��� nil
    }

    function ToBinary(const Buf: PAnsiChar; FixedLen: Integer = 0): Integer;
    {* ������ת���ɶ��������ݷ��� Buf �У�ʹ�÷����Ķ�ϰ�ߵ������ֽ�˳��
       Buf �ĳ��ȱ�����ڵ����� BytesCount������ Buf д���ʵ���ֽڳ��ȡ�

       ������
         const Buf: PAnsiChar             - ��д������ݿ��ַ
         FixedLen: Integer                - ָ������ʵ���ֽڳ��Ȳ���ʱʹ�õĹ̶��ֽڳ��ȣ�Ϊ 0 ʱ��ʹ�ô���ʵ���ֽڳ���

       ����ֵ��Integer                    - ����ʵ��д���ֽڳ���
    }

    function LoadFromStream(Stream: TStream): Boolean;
    {* �����м��ش�����

       ������
         Stream: TStream                  - �����ص���

       ����ֵ��Boolean                    - �����Ƿ���سɹ�
    }

    function SaveToStream(Stream: TStream; FixedLen: Integer): Integer;
    {* ������д������

       ������
         Stream: TStream                  - ��д�����
         FixedLen: Integer                - ָ������ʵ���ֽڳ��Ȳ���ʱʹ�õĹ̶��ֽڳ��ȣ�Ϊ 0 ʱ��ʹ�ô���ʵ���ֽڳ���

       ����ֵ��Integer                    - ����ʵ��д���ֽڳ���
    }

    function SetBinary(Buf: PAnsiChar; ByteLen: Integer): Boolean;
    {* ����һ�������ƿ������ֵ��ʹ�÷����Ķ�ϰ�ߵ������ֽ�˳���ڲ����������ݡ�

       ������
         Buf: PAnsiChar                   - ����ֵ�����ݿ��ַ
         ByteLen: Integer                 - ����ֵ�����ݿ��ֽڳ���

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    class function FromBinary(Buf: PAnsiChar; ByteLen: Integer): TCnBigNumber;
    {* ����һ�������ƿ����һ���µĴ�������ʹ�÷����Ķ�ϰ�ߵ������ֽ�˳�򣬶����ڲ����������ݡ�

       ������
         Buf: PAnsiChar                   - ��ʹ�õ����ݿ��ַ
         ByteLen: Integer                 - ����ֵ�����ݿ��ֽڳ���

       ����ֵ��TCnBigNumber               - �����½��Ĵ�������
    }

    class function FromBytes(Buf: TBytes): TCnBigNumber;
    {* ����һ���ֽ�����ת��������һ���µĴ�������ʹ�÷����Ķ�ϰ�ߵ������ֽ�˳�򣬶����ڲ����������ݡ�

       ������
         Buf: TBytes                      - ��ת�����ֽ�����

       ����ֵ��TCnBigNumber               - �����½��Ĵ�������
    }

    function ToBytes: TBytes;
    {* ����������ת�����ֽ����飬ʹ�÷����Ķ�ϰ�ߵ������ֽ�˳��

       ������
         ���ޣ�

       ����ֵ��TBytes                     - ����ת�����ֽ�����
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ������ת�����ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ���ش����ַ���
    }

    function GetHashCode: TCnHashCode; {$IFDEF OBJECT_HAS_GETHASHCODE} override; {$ENDIF}
    {* �����Ӵ�ֵ��

       ������
         ���ޣ�

       ����ֵ��TCnHashCode                - �����Ӵ�ֵ��ֻ 32 λ��Ч
    }

    function ToHex(FixedLen: Integer = 0): string;
    {* ������ת��ʮ�������ַ�����

       ������
         FixedLen: Integer                - ָ������ʵ���ֽڳ��Ȳ���ʱʹ�õĹ̶��ֽڳ��Ȳ��ַ���ǰ�油 00��Ϊ 0 ʱ��ʹ�ô���ʵ���ֽڳ���

       ����ֵ��string                     - ����ʮ�������ַ���
    }

    function SetHex(const Buf: AnsiString): Boolean;
    {* ����һ��ʮ�������ַ���������ֵ��

       ������
         const Buf: AnsiString            - ����ֵ��ʮ�������ַ���

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    class function FromHex(const Buf: AnsiString): TCnBigNumber;
    {* ����һ��ʮ�������ַ�������һ���µĴ�������

       ������
         const Buf: AnsiString            - ��ʹ�õ�ʮ�������ַ���

       ����ֵ��TCnBigNumber               - �����½��Ĵ�������
    }

    function ToBase64: string;
    {* ������ת�� Base64 �ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ���� Base64 �ַ���
    }

    function SetBase64(const Buf: AnsiString): Boolean;
    {* ����һ�� Base64 �ַ���������ֵ��

       ������
         const Buf: AnsiString            - ����ֵ�� Base64 �ַ���

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    class function FromBase64(const Buf: AnsiString): TCnBigNumber;
    {* ����һ�� Base64 �ַ�������һ���µĴ�������

       ������
         const Buf: AnsiString            - ��ʹ�õ� Base64 �ַ���

       ����ֵ��TCnBigNumber               - �����½��Ĵ�������
    }

    function ToDec: string;
    {* ������ת��ʮ�����ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ����ʮ�����ַ���
    }

    function SetDec(const Buf: AnsiString): Boolean;
    {* ����һ��ʮ�����ַ���������ֵ��

       ������
         const Buf: AnsiString            - ����ֵ��ʮ�������ַ���

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    class function FromDec(const Buf: AnsiString): TCnBigNumber;
    {* ����һ��ʮ�����ַ�������һ���µĴ�������

       ������
         const Buf: AnsiString            - ��ʹ�õ�ʮ�������ַ���

       ����ֵ��TCnBigNumber               - �����½��Ĵ�������
    }

    function IsFloat: Boolean;
    {* �����Ƿ���һ�� Extended ��չ���ȸ��㷶Χ�ڵ�����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�����չ���ȷ�Χ��
    }

    function GetFloat: Extended;
    {* ������ת��Ϊ��������

       ������
         ���ޣ�

       ����ֵ��Extended                   - ���ظ�����
    }

    function SetFloat(F: Extended): Boolean;
    {* ����һ������������ֵ��

       ������
         F: Extended                      - ����ֵ�ĸ�����

       ����ֵ��Boolean                    - �����Ƿ�ֵ�ɹ�
    }

    class function FromFloat(F: Extended): TCnBigNumber;
    {* ����һ������������һ���µĴ�������

       ������
         F: Extended                      - ��ʹ�õĸ�����

       ����ֵ��TCnBigNumber               - �����½��Ĵ�������
    }

    function RawDump(Mem: Pointer = nil): Integer;
    {* Dump ��ԭʼ�ڴ����ݣ����� Dump ���ֽڳ��ȡ��� Mem �� nil��ֻ���������ֽڳ��ȡ�

       ������
         Mem: Pointer                     - ����������ݿ��ַ

       ����ֵ��Integer                    - �����ֽڳ���
    }

    property DecString: string read GetDecString;
    {* ʮ�����ַ���}
    property HexString: string read GetHexString;
    {* ʮ�������ַ���}

    property DebugDump: string read GetDebugDump;
    {* �ڲ�����ַ���}
  end;
  PCnBigNumber = ^TCnBigNumber;

  TCnBigNumberList = class(TObjectList)
  {* ���ɴ����Ķ����б�ͬʱӵ�д���������}
  private

  protected
    function GetItem(Index: Integer): TCnBigNumber;
    procedure SetItem(Index: Integer; ABigNumber: TCnBigNumber);
  public
    constructor Create; reintroduce;
    {* ���캯��}

    function Add: TCnBigNumber; overload;
    {* ����һ���������󣬷��ظö���ע����Ӻ�����Ҳ��Ӧ�ֶ��ͷš�

       ������
         ���ޣ�

       ����ֵ��TCnBigNumber               - �ڲ������Ĵ�������
    }

    function Add(ABigNumber: TCnBigNumber): Integer; overload;
    {* ����ⲿ�Ĵ�������ע����Ӻ�����Ҳ��Ӧ�ֶ��ͷš�

       ������
         ABigNumber: TCnBigNumber         - ����ӵĴ�������

       ����ֵ��Integer                    - �����ĸô������������ֵ
    }

    function Add(Num: Integer): TCnBigNumber; overload;
    {* ���һ�������ڲ����ɴ�������ע�ⷵ�صĽ������Ҳ��Ӧ�ֶ��ͷš�

       ������
         Num: Integer                     - ����ӵ�����

       ����ֵ��TCnBigNumber               - �����ĸô�������
    }

    procedure AddList(List: TCnBigNumberList);
    {* ���һ�����б�Ҳ�������б��ڵ����д���������ӡ�

       ������
         List: TCnBigNumberList           - ����ӵ�����

       ����ֵ�����ޣ�
    }

    function Remove(ABigNumber: TCnBigNumber): Integer;
    {* ���б���ɾ��ָ�����õĴ��������ͷš�

       ������
         ABigNumber: TCnBigNumber         - ��ɾ���Ĵ�������

       ����ֵ��Integer                    - ɾ����λ�����������򷵻� -1
    }

    function IndexOfValue(ABigNumber: TCnBigNumber): Integer;
    {* ���ݴ�����ֵ���б��в��Ҹ�ֵ��Ӧ��λ��������

       ������
         ABigNumber: TCnBigNumber         - �����ҵĴ���ֵ

       ����ֵ��Integer                    - ����λ�����������򷵻� -1
    }

    procedure Insert(Index: Integer; ABigNumber: TCnBigNumber);
    {* �ڵ� Index ��λ��ǰ�����������ע����������Ҳ��Ӧ�ֶ��ͷš�

       ������
         Index: Integer                   - �������λ������
         ABigNumber: TCnBigNumber         - ������Ĵ�������

       ����ֵ�����ޣ�
    }

    procedure RemoveDuplicated;
    {* ȥ�أ�Ҳ����ɾ�����ͷ�ֵ�ظ��Ĵ�������ֻ��һ��}

    procedure SumTo(Sum: TCnBigNumber);
    {* �б�����������͡�

       ������
         Sum: TCnBigNumber                - ����ĺ�

       ����ֵ�����ޣ�
    }

    procedure BigNumberSort;
    {* �б��ڴ�����С��������}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* �������б�ת���ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - �����ַ���
    }

    property Items[Index: Integer]: TCnBigNumber read GetItem write SetItem; default;
    {* �����б���}
  end;

  TCnBigNumberPool = class(TCnMathObjectPool)
  {* ������ʵ���࣬����ʹ�õ������ĵط����д���������}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumber; reintroduce;
    {* �Ӷ���ػ�ȡһ�����󣬲���ʱ����� Recycle �黹��

       ������
         ���ޣ�

       ����ֵ��TCnBigNumber               - ���صĴ�������
    }

    procedure Recycle(Num: TCnBigNumber); reintroduce;
    {* ��һ������黹������ء�

       ������
         Num: TCnBigNumber                - ���黹�Ĵ�������

       ����ֵ�����ޣ�
    }
  end;

  TCnExponentBigNumberPair = class(TObject)
  {* ָ�������������࣬����ϡ���б�}
  private
    FExponent: Integer;
    FValue: TCnBigNumber;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ��ָ�������ת���ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - �����ַ���
    }

    property Exponent: Integer read FExponent write FExponent;
    {* ָ��}
    property Value: TCnBigNumber read FValue;
    {* ����}
  end;

  TCnSparseBigNumberList = class(TObjectList)
  {* ���ɴ�����ָ����ϡ������б�ͬʱӵ�� TCnExponentBigNumberPair �����ǣ�
     �ڲ��� Exponent ��С��������}
  private
    function GetItem(Index: Integer): TCnExponentBigNumberPair;
    procedure SetItem(Index: Integer; const Value: TCnExponentBigNumberPair);
    function BinarySearchExponent(AExponent: Integer; var OutIndex: Integer): Boolean;
    {* ���ַ����� AExponent ��λ�ã��ҵ����� True��OutIndex ���ö�Ӧ�б�����λ��
      ��δ�ҵ���OutIndex �򷵻ز���λ�ù�ֱ�� Insert��MaxInt ʱ�� Add}

    function InsertByOutIndex(OutIndex: Integer): Integer;
    {* ���ݶ��ַ�����ʧ�ܳ��Ϸ��ص� OutIndex ʵʩ���룬���ز�������ʵ Index}

    function GetSafeValue(Exponent: Integer): TCnBigNumber;
    function GetReadonlyValue(Exponent: Integer): TCnBigNumber;
    procedure SetSafeValue(Exponent: Integer; const Value: TCnBigNumber);
  public
    constructor Create; reintroduce;
    {* ���캯��}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ������Ԫ���е�ָ�������ת�ɶ����ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ����ת�����ַ���
    }

    function Top: TCnExponentBigNumberPair;
    {* �����ߴζ���

       ������
         ���ޣ�

       ����ֵ��TCnExponentBigNumberPair   - ������ߴζ���
    }

    function Bottom: TCnExponentBigNumberPair;
    {* �����ʹζ���

       ������
         ���ޣ�

       ����ֵ��TCnExponentBigNumberPair   - ������ʹζ���
    }

    // ��Ҫȡ������ɾ���ġ�ѹ�Ȳ���
    function AddPair(AExponent: Integer; Num: TCnBigNumber): TCnExponentBigNumberPair;
    {* ���һ�� Pair���ڲ����ƴ���

       ������
         AExponent: Integer               -
         Num: TCnBigNumber                -

       ����ֵ��TCnExponentBigNumberPair   -
    }
    procedure AssignTo(Dest: TCnSparseBigNumberList);
    {* �����ݸ��Ƹ�����һ���б����

       ������
         Dest: TCnSparseBigNumberList     - �����Ƶ�Ŀ�����

       ����ֵ�����ޣ�
    }

    procedure SetValues(LowToHighList: array of Int64);
    {* �ӵʹε��ߴ�����ֵ��

       ������
         LowToHighList: array of Int64    - ����ֵ�Ĳ����б�

       ����ֵ�����ޣ�
    }

    procedure Compact;
    {* ѹ����Ҳ����ɾ������ 0 ϵ����}
    procedure Negate;
    {* ����ϵ����}

    property SafeValue[Exponent: Integer]: TCnBigNumber read GetSafeValue write SetSafeValue;
    {* ��ȫ�ĸ��ݲ��� Exponent ��ȡ�����ķ�������ʱ���ڲ��鲻����������½�ֵ�����أ�
       дʱ���ڲ��鲻�������½�����ָ��λ�ú� Value ������� BigNumber ����}

    property ReadonlyValue[Exponent: Integer]: TCnBigNumber read GetReadonlyValue;
    {* ֻ���ĸ��ݲ��� Exponent ��ȡ�����ķ�������ʱ���ڲ��鲻�����᷵��һ�̶�����ֵ TCnBigNumber ���������޸���ֵ}

    property Items[Index: Integer]: TCnExponentBigNumberPair read GetItem write SetItem; default;
    {* ���ص� Items ����}
  end;

  TCnBigNumberHashMap = class(TCnHashMap)
  {* �洢���������ɢ�б�������ֵΪ�ȶ����������ң������Ƕ������ñ���}
  private
    FOwnsKey: Boolean;
    FOwnsValue: Boolean;
  protected
    function HashCodeFromObject(Obj: TObject): Integer; override;
    function KeyEqual(Key1: TObject; Key2: TObject
      {$IFNDEF CPU64BITS}; Key132: TObject; Key232: TObject {$ENDIF}): Boolean; override;
    procedure DoFreeNode(Node: TCnHashNode); override;
  public
    constructor Create(AOwnsKey: Boolean; AOwnsValue: Boolean); reintroduce; virtual;
    {* AOwnsKey Ϊ True ʱ��Key ��Ϊ���д����ڵ�ɾ��ʱ���ͷ���� Key ����
       AOwnsValue Ϊ True ʱ��Value Ҳ��Ϊ���д����ڵ�ɾ��ʱ���ͷ���� Value ����
       ע�⣺��Ϊ True ʱ��Key �� Value ������ Object ������ݡ�

       ������
         AOwnsKey: Boolean                - �Ƿ���� Key ����
         AOwnsValue: Boolean              - �Ƿ���� Value ����

       ����ֵ��TCnBigNumberHashMap        - ���ش����Ķ���ʵ��
    }

    function Find(Key: TCnBigNumber): TCnBigNumber;
    {* ����ָ�����������ֵ����Ӧ��ֵ��

       ������
         Key: TCnBigNumber                - �����ҵĴ����� Key ֵ

       ����ֵ��TCnBigNumber               - ���ز��ҵ��� Value �������ã����򷵻� nil
    }
  end;

function BigNumberNew: TCnBigNumber;
{* ����һ����̬����Ĵ������󣬵�ͬ�� TCnBigNumber.Create��

   ������
     ���ޣ�

   ����ֵ��TCnBigNumber                   - ���ش����Ĵ���ʵ��
}

procedure BigNumberFree(Num: TCnBigNumber);
{* ����Ҫ�ͷ�һ���� BigNumerNew ���������Ĵ������󣬲�����Ҫ�ͷ��� D ����
   ��ͬ��ֱ�ӵ��� Free��

   ������
     Num: TCnBigNumber                    - ���ͷŵĴ�������

   ����ֵ�����ޣ�
}

procedure BigNumberInit(Num: TCnBigNumber);
{* ��ʼ��һ����������ȫΪ 0�����������ڲ����ڴ棬�������ͷ��ڲ����е��ڴ档

   ������
     Num: TCnBigNumber                    - ����ʼ���Ĵ�������

   ����ֵ�����ޣ�
}

procedure BigNumberClear(Num: TCnBigNumber);
  {* ���һ���������󣬲��������ݿռ��� 0�����ͷ����ڲ����ڴ档

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ�����ޣ�
}

function BigNumberIsZero(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ������������Ĵ����Ƿ�Ϊ 0��

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�Ϊ 0
}

function BigNumberSetZero(Num: TCnBigNumber): Boolean;
{* ��һ������������Ĵ�������Ϊ 0��

   ������
     Num: TCnBigNumber                    - �����õĴ�������

   ����ֵ��Boolean                        - �����Ƿ����óɹ�
}

function BigNumberIsOne(Num: TCnBigNumber): Boolean;
{* ����һ������������Ĵ����Ƿ�Ϊ 1��

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�Ϊ 1
}

function BigNumberIsNegOne(Num: TCnBigNumber): Boolean;
{* ����һ������������Ĵ����Ƿ�Ϊ -1��

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�Ϊ -1
}

function BigNumberSetOne(Num: TCnBigNumber): Boolean;
{* ��һ������������Ĵ�������Ϊ 1��

   ������
     Num: TCnBigNumber                    - �����õĴ�������

   ����ֵ��Boolean                        - �����Ƿ����óɹ�
}

function BigNumberIsOdd(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ������������Ĵ����Ƿ�Ϊ������

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�Ϊ����
}

function BigNumberIsEven(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ������������Ĵ����Ƿ�Ϊż����

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�Ϊż��
}

function BigNumberGetBitsCount(Num: TCnBigNumber): Integer;
{* ����һ������������Ĵ����ж��ٸ���Ч Bits λ��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Integer                        - ������Чλ��
}

function BigNumberGetBytesCount(Num: TCnBigNumber): Integer;
{* ����һ������������Ĵ����ж��ٸ���Ч Bytes �ֽڡ�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Integer                        - ������Ч�ֽ���
}

function BigNumberGetWordsCount(Num: TCnBigNumber): Integer;
{* ����һ������������Ĵ����ж��ٸ���Ч UInt32/UInt64 Ԫ�ء�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Integer                        - ������ЧԪ����
}

function BigNumberGetTenPrecision(Num: TCnBigNumber): Integer;
{* ����һ������������Ĵ����ж��ٸ���Чʮ����λ����

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Integer                        - ����ʮ����λ��
}

function BigNumberGetTenPrecision2(Num: TCnBigNumber): Integer;
{* ���Է���һ������������Ĵ����ж��ٸ���Чʮ����λ���������� 1 λ���Ͽ졣

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Integer                        - ����ʮ����λ��
}

function BigNumberGetWord(Num: TCnBigNumber): Cardinal;
{* ȡһ�������������ֵ��Ҳ���ǵ� 32 λ�޷�������ֵ��ע���������̫���򷵻� $FFFFFFFF��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Cardinal                       - ���� 32 λ�޷�������
}

function BigNumberSetWord(Num: TCnBigNumber; W: Cardinal): Boolean;
{* ��һ������������ֵ��Ҳ���ǵ� 32 λ�޷�������ֵ��

   ������
     Num: TCnBigNumber                    - �����õĴ�������
     W: Cardinal                          - ����ֵ�� 32 λ�޷�������

   ����ֵ��Boolean                        - �����Ƿ����óɹ�
}

function BigNumberGetInteger(Num: TCnBigNumber): Integer;
{* ȡһ�������������ֵ��Ҳ���ǵ� 32 λ�з�������ע���������̫���򷵻� $7FFFFFFF��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Integer                        - ���� 32 λ�з�������
}

function BigNumberSetInteger(Num: TCnBigNumber; W: Integer): Boolean;
{* ��һ������������ֵ��Ҳ���ǵ� 32 λ�з���������

   ������
     Num: TCnBigNumber                    - �����õĴ�������
     W: Integer                           - ����ֵ�� 32 λ�з�������

   ����ֵ��Boolean                        - �����Ƿ����óɹ�
}

function BigNumberGetInt64(Num: TCnBigNumber): Int64;
{* ȡһ�������������ֵ Int64��Ҳ���� 64 λ�з���������ע���������̫���򷵻� $7FFFFFFFFFFFFFFF��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Int64                          - ���� 64 λ�з�������
}

function BigNumberSetInt64(Num: TCnBigNumber; W: Int64): Boolean;
{* ��һ������������ֵ Int64��Ҳ���� 64 λ�з���������

   ������
     Num: TCnBigNumber                    - �����õĴ�������
     W: Int64                             - ����ֵ�� 64 λ�з�������

   ����ֵ��Boolean                        - �����Ƿ����óɹ�
}

function BigNumberGetUInt64UsingInt64(Num: TCnBigNumber): TUInt64;
{* ʹ�� Int64 ȡһ�������������ֵ UInt64��Ҳ���� 64 λ�޷���������ע���������̫���򷵻� $FFFFFFFFFFFFFFFF��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��TUInt64                        - ���� 64 λ�޷�������
}

function BigNumberSetUInt64UsingInt64(Num: TCnBigNumber; W: TUInt64): Boolean;
{* ʹ�� Int64 ��һ���������� UInt64 ��ֵ��Ҳ���� 64 λ�޷���������

   ������
     Num: TCnBigNumber                    - �����õĴ�������
     W: TUInt64                           - ����ֵ�� 64 λ�޷�������

   ����ֵ��Boolean                        - �����Ƿ����óɹ�
}

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(Num: TCnBigNumber): UInt64;
{* ȡһ�������������ֵ UInt64��Ҳ���� 64 λ�޷���������ע���������̫���򷵻� $FFFFFFFFFFFFFFFF��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��UInt64                         - ���� 64 λ�޷�������
}

function BigNumberSetUInt64(Num: TCnBigNumber; W: UInt64): Boolean;
{* ��һ������������ֵ UInt64��Ҳ���� 64 λ�޷���������

   ������
     Num: TCnBigNumber                    - �����õĴ�������
     W: UInt64                            - ����ֵ�� 64 λ�޷�������

   ����ֵ��Boolean                        - �����Ƿ����óɹ�
}

{$ENDIF}

function BigNumberIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* ĳ�����Ƿ����ָ�� UInt32/UInt64 Ԫ�ء�

   ������
     Num: TCnBigNumber                    - ���ȽϵĴ�������
     W: TCnBigNumberElement               - ���Ƚϵ�Ԫ��ֵ

   ����ֵ��Boolean                        - �����Ƿ����
}

function BigNumberAbsIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* ĳ��������ֵ�Ƿ����ָ�� UInt32/UInt64 Ԫ�ء�

   ������
     Num: TCnBigNumber                    - ���ȽϵĴ�������
     W: TCnBigNumberElement               - ���Ƚϵ�Ԫ��ֵ

   ����ֵ��Boolean                        - ���ؾ���ֵ�Ƿ����
}

function BigNumberAddWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* ��������һ�� UInt32/UInt64 Ԫ�أ�����Է� Num �У���������Ƿ�ɹ���

   ������
     Num: TCnBigNumber                    - ���ӵĴ�������
     W: TCnBigNumberElement               - ����Ԫ��

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigNumberSubWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* ������ȥһ�� UInt32/UInt64 Ԫ�أ�����Է� Num �У���������Ƿ�ɹ���

   ������
     Num: TCnBigNumber                    - �����Ĵ�������
     W: TCnBigNumberElement               - ����Ԫ��

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigNumberMulWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* ��������һ�� UInt32/UInt64 Ԫ�أ�����Է� Num �У���������Ƿ�ɹ���

   ������
     Num: TCnBigNumber                    - ���˵Ĵ�������
     W: TCnBigNumberElement               - ����Ԫ��

   ����ֵ��Boolean                        - ��������Ƿ�ɹ�
}

function BigNumberModWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
{* ������һ�� UInt32/UInt64 Ԫ�����࣬����������
   ע�����ڲ� 64 λ��ʵ���У�W ���ܴ��� UInt32��32 λ�ڲ�ʵ���������ơ�

   ������
     Num: TCnBigNumber                    - ��������������
     W: TCnBigNumberElement               - ����Ԫ��

   ����ֵ��TCnBigNumberElement            - ��������
}

function BigNumberDivWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
{* ��������һ�� UInt32/UInt64 Ԫ�أ������·��� Num �У�����������

   ������
     Num: TCnBigNumber                    - ��������������
     W: TCnBigNumberElement               - ����Ԫ��

   ����ֵ��TCnBigNumberElement            - ����
}

procedure BigNumberAndWord(Num: TCnBigNumber; W: TCnBigNumberElement);
{* ������һ�� UInt32/UInt64 Ԫ������λ�룬����Է� Num �С�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     W: TCnBigNumberElement               - ����λ���Ԫ��

   ����ֵ�����ޣ�
}

procedure BigNumberOrWord(Num: TCnBigNumber; W: TCnBigNumberElement);
{* ������һ�� UInt32/UInt64 Ԫ������λ�򣬽���Է� Num �С�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     W: TCnBigNumberElement               - ����λ���Ԫ��

   ����ֵ�����ޣ�
}

procedure BigNumberXorWord(Num: TCnBigNumber; W: TCnBigNumberElement);
{* ������һ�� UInt32/UInt64 Ԫ������λ��򣬽���Է� Num �С�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     W: TCnBigNumberElement               - ����λ����Ԫ��

   ����ֵ�����ޣ�
}

function BigNumberAndWordTo(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
{* ������һ�� UInt32/UInt64 Ԫ������λ�룬���ص� 32/64 λ��������������䡣ע�������ݲ���Ҫ��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     W: TCnBigNumberElement               - ����λ���Ԫ��

   ����ֵ��TCnBigNumberElement            - ���ذ�λ����
}

procedure BigNumberSetNegative(Num: TCnBigNumber; Negative: Boolean);
{* ��һ���������������Ƿ�ֵ��

   ������
     Num: TCnBigNumber                    - �����õĴ�������
     Negative: Boolean                    - �Ƿ�ֵ

   ����ֵ�����ޣ�
}

function BigNumberIsNegative(Num: TCnBigNumber): Boolean;
{* ����һ�����������Ƿ�ֵ��ע�ⲻ�ж� 0��Ҳ����˵�� 0 Ҳ���� True��

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�ֵ
}

procedure BigNumberNegate(Num: TCnBigNumber);
{* ��һ��������������Ϊ���෴����Ҳ�����������󷴡�

   ������
     Num: TCnBigNumber                    - �����õĴ�������

   ����ֵ�����ޣ�
}

function BigNumberClearBit(Num: TCnBigNumber; N: Integer): Boolean;
{* ��һ����������ĵ� N �� Bit �� 0�����سɹ����N Ϊ 0 ʱ������������λ��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     N: Integer                           - ���� 0 ��λ����

   ����ֵ��Boolean                        - �����Ƿ��� 0 �ɹ�
}

function BigNumberKeepLowBits(Num: TCnBigNumber; Count: Integer): Boolean;
{* ��һ����������ֻ������ 0 �� Count - 1 �� Bit λ����λ���㣬���سɹ����

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     Count: Integer                       - �������ĵ�λ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberSetBit(Num: TCnBigNumber; N: Integer): Boolean;
{* ��һ����������ĵ� N �� Bit �� 1�����سɹ����N Ϊ 0 ʱ������������λ��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     N: Integer                           - ���� 1 ��λ����

   ����ֵ��Boolean                        - �����Ƿ��� 1 �ɹ�
}

function BigNumberIsBitSet(Num: TCnBigNumber; N: Integer): Boolean;
{* ����һ����������ĵ� N �� Bit �Ƿ�Ϊ 1��N Ϊ 0 ʱ������������λ��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     N: Integer                           - λ����

   ����ֵ��Boolean                        - �����Ƿ�Ϊ 1
}

function BigNumberWordExpand(Num: TCnBigNumber; Words: Integer): TCnBigNumber;
{* ��һ������������չ��֧�� Words �� UInt32/UInt64 Ԫ�أ��ɹ����ر���չ�Ĵ��������ַ��ʧ�ܷ��� nil��

   ������
     Num: TCnBigNumber                    - ����չ�Ĵ�������
     Words: Integer                       - ����չ��Ԫ����

   ����ֵ��TCnBigNumber                   - �ɹ��򷵻ش���������ʧ�ܷ��� nil
}

function BigNumberToBinary(Num: TCnBigNumber; Buf: PAnsiChar; FixedLen: Integer = 0): Integer;
{* ��һ������ת���ɶ��������ݷ��� Buf �У�Buf �ĳ��ȱ�����ڵ����� BytesCount��
   ���� Buf д��ĳ��ȣ�ע�ⲻ���������š���� Buf Ϊ nil����ֱ�ӷ������賤��
   �������ȳ��� FixedLen ʱ������ʵ���ֽڳ���д��������д�ֽ� 0 ���볤��
   ע���ڲ��и�Ԫ�ؼ䵹��Ĺ��̣�ͬʱԪ����Ҳ�в��ֽڵĹ��̣�Ĩƽ�� CPU ��С�˵Ĳ�ͬ
   Ҳ����˵���ڴ汻д����Ǵ����ڲ��ĸ�λ���ݣ�����������Ķ�ϰ��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     Buf: PAnsiChar                       - ��д������ݿ��ַ
     FixedLen: Integer                    - ָ������ʵ���ֽڳ��Ȳ���ʱʹ�õĹ̶��ֽڳ��ȣ�Ϊ 0 ʱ��ʹ�ô���ʵ���ֽڳ���

   ����ֵ��Integer                        - ����ʵ��д���ֽڳ���
}

function BigNumberFromBinary(Buf: PAnsiChar; ByteLen: Integer): TCnBigNumber;
{* ��һ�������ƿ�ת���ɴ�������ע�ⲻ���������š���������ʱ������ BigNumberFree �ͷš�

   ������
     Buf: PAnsiChar                       - ��ʹ�õ����ݿ��ַ
     ByteLen: Integer                     - ����ֵ�����ݿ��ֽڳ���

   ����ֵ��TCnBigNumber                   - �����½��Ĵ�������
}

function BigNumberReadBinaryFromStream(Num: TCnBigNumber; Stream: TStream): Boolean;
{* �����м��ش����������������ţ������Ƿ���سɹ���

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     Stream: TStream                      - �����ص���

   ����ֵ��Boolean                        - �����Ƿ���سɹ�
}

function BigNumberWriteBinaryToStream(Num: TCnBigNumber; Stream: TStream;
  FixedLen: Integer = 0): Integer;
{* ��һ�������Ķ����Ʋ���д�����������������ţ�����д�����ĳ��ȡ�
   ע���ڲ��и�Ԫ�ؼ��Լ�Ԫ���ڵ���Ĺ����Է���������Ķ�ϰ�ߡ�
   FixedLen ��ʾ�������ݲ��� FixedLen �ֽڳ���ʱ��λ���� 0 �Ա�֤ Stream ������̶� FixedLen �ֽڵĳ��ȡ�
   �������ȳ��� FixedLen ʱ������ʵ���ֽڳ���д��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     Stream: TStream                      - ��д�����
     FixedLen: Integer                    - ָ������ʵ���ֽڳ��Ȳ���ʱʹ�õĹ̶��ֽڳ��ȣ�Ϊ 0 ʱ��ʹ�ô���ʵ���ֽڳ���

   ����ֵ��Integer                        - ����ʵ��д���ֽڳ���
}

function BigNumberFromBytes(Buf: TBytes): TCnBigNumber;
{* ��һ���ֽ���������ת���ɴ��������ֽ�˳��ͬ Binary��ע�ⲻ���������š���������ʱ������ BigNumberFree �ͷš�

   ������
     Buf: TBytes                          - ��ת�����ֽ�����

   ����ֵ��TCnBigNumber                   - �����½��Ĵ�������
}

function BigNumberToBytes(Num: TCnBigNumber): TBytes;
{* ��һ������ת���ɶ���������д���ֽ����鲢���أ��ֽ�˳��ͬ Binary�������������ţ�ʧ�ܷ��� nil��

   ������
     Num: TCnBigNumber                    - ��ת���Ĵ�������

   ����ֵ��TBytes                         - �����ֽ�����
}

function BigNumberSetBinary(Buf: PAnsiChar; ByteLen: Integer; Res: TCnBigNumber): Boolean;
{* ��һ�������ƿ鸳ֵ��ָ����������ע�ⲻ���������ţ��ڲ����ø��ơ�
   ע���ڲ��и�Ԫ�ؼ䵹���Լ�����ֽ��ɵ͵���ƴ��һ��Ԫ�صĹ��̣��Է���������Ķ�ϰ�ߡ�

   ������
     Buf: PAnsiChar                       - ����ֵ�����ݿ��ַ
     ByteLen: Integer                     - ����ֵ�����ݿ��ֽڳ���
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������

   ����ֵ��Boolean                        - �����Ƿ�ֵ�ɹ�
}

function BigNumberToBase64(Num: TCnBigNumber): string;
{* ��һ����������ת�� Base64 �ַ����������������š�

   ������
     Num: TCnBigNumber                    - ��ת���Ĵ�������

   ����ֵ��string                         - ���� Base64 �ַ���
}

function BigNumberSetBase64(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
{* ��һ�� Base64 �ַ�����ֵ��ָ���������󣬲����������š�

   ������
     const Buf: AnsiString                - ��ת���� Base64 �ַ���
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������

   ����ֵ��Boolean                        - �����Ƿ�ֵ�ɹ�
}

function BigNumberFromBase64(const Buf: AnsiString): TCnBigNumber;
{* ����һ�� Base64 �ַ�������һ���µĴ������󣬲����������š���������ʱ������ BigNumberFree �ͷ�

   ������
     const Buf: AnsiString                - ��ʹ�õ� Base64 �ַ���

   ����ֵ��TCnBigNumber                   - �����½��Ĵ�������
}

function BigNumberToString(Num: TCnBigNumber): string;
{* ��һ����������ת����ͨ�ɶ���ʮ�������ַ��������� - ��ʾ��

   ������
     Num: TCnBigNumber                    - ��ת���Ĵ�������

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BigNumberToHex(Num: TCnBigNumber; FixedLen: Integer = 0): string;
{* ��һ����������ת��ʮ�������ַ��������� - ��ʾ��
   FixedLen ��ʾ�������ݲ��� FixedLen �ֽڳ���ʱ��λ���� 0 �Ա�֤���������̶� FixedLen �ֽڵĳ��ȣ����������ţ���
   �ڲ��������ȳ��� FixedLen ʱ������ʵ�ʳ���д��ע�� FixedLen ����ʮ�������ַ������ȡ�

   ������
     Num: TCnBigNumber                    - ��ת���Ĵ�������
     FixedLen: Integer                    - ָ������ʵ���ֽڳ��Ȳ���ʱʹ�õĹ̶��ֽڳ��Ȳ��ַ���ǰ�油 00��Ϊ 0 ʱ��ʹ�ô���ʵ���ֽڳ���

   ����ֵ��string                         - ����ʮ�������ַ���
}

function BigNumberSetHex(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
{* ��һ��ʮ�������ַ�����ֵ��ָ���������󣬸��� - ��ʾ���ڲ����ܰ����س����С�
   ע������ͨ���ַ�������߱�ʾ��λ���������ڲ���λ�ڸߵ�ַ��������ڲ��и�������̡�

   ������
     const Buf: AnsiString                - ����ֵ��ʮ�������ַ���
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������

   ����ֵ��Boolean                        - �����Ƿ�ֵ�ɹ�
}

function BigNumberFromHex(const Buf: AnsiString): TCnBigNumber;
{* ��һ��ʮ�������ַ���ת��Ϊ�������󣬸��� - ��ʾ����������ʱ������ BigNumberFree �ͷ�

   ������
     const Buf: AnsiString                - ��ʹ�õ�ʮ�������ַ���

   ����ֵ��TCnBigNumber                   - �����½��Ĵ�������
}

function BigNumberToDec(Num: TCnBigNumber): AnsiString;
{* ��һ����������ת��ʮ�����ַ��������� - ��ʾ��

   ������
     Num: TCnBigNumber                    - ��ת���Ĵ�������

   ����ֵ��AnsiString                     - ����ʮ�����ַ���
}

function BigNumberSetDec(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
{* ��һ��ʮ�����ַ�����ֵ��ָ���������󣬸��� - ��ʾ���ڲ����ܰ����س����С�

   ������
     const Buf: AnsiString                - ����ֵ��ʮ�����ַ���
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������

   ����ֵ��Boolean                        - �����Ƿ�ֵ�ɹ�
}

function BigNumberFromDec(const Buf: AnsiString): TCnBigNumber;
{* ��һ��ʮ�����ַ���ת��Ϊ�������󣬸��� - ��ʾ����������ʱ������ BigNumberFree �ͷš�

   ������
     const Buf: AnsiString                - ��ʹ�õ�ʮ�����ַ���

   ����ֵ��TCnBigNumber                   - �����½��Ĵ�������
}

function BigNumberSetFloat(F: Extended; Res: TCnBigNumber): Boolean;
{* �����������ø��������󣬺���С�����֡�

   ������
     F: Extended                          - ����ֵ�ĸ�����
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������

   ����ֵ��Boolean                        - �����Ƿ�ֵ�ɹ�
}

function BigNumberGetFloat(Num: TCnBigNumber): Extended;
{* ������ת��Ϊ������������ʱ��Ӧ�׳��쳣��Ŀǰ��δ����

   ������
     Num: TCnBigNumber                    - ��ת���Ĵ�������

   ����ֵ��Extended                       - ���ظ�����
}

function BigNumberFromFloat(F: Extended): TCnBigNumber;
{* ��������ת��Ϊ�½��Ĵ���������������ʱ������ BigNumberFree �ͷš�

   ������
     F: Extended                          - ��ʹ�õĸ�����

   ����ֵ��TCnBigNumber                   - �����½��Ĵ�������
}

function BigNumberEqual(Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean; overload;
{* �Ƚ��������������Ƿ���ȣ���ȷ��� True�����ȷ��� False��

   ������
     Num1: TCnBigNumber                   - ���ȽϵĴ�������һ
     Num2: TCnBigNumber                   - ���ȽϵĴ��������

   ����ֵ��Boolean                        - �����Ƿ����
}

function BigNumberEqual(Num1: TCnBigNumber; Num2: Int64): Boolean; overload;
{* �Ƚ�һ�����������Ƿ����ָ����������ȷ��� True�����ȷ��� False��

   ������
     Num1: TCnBigNumber                   - ���ȽϵĴ�������
     Num2: TCnBigNumber                   - ���Ƚϵ�����

   ����ֵ��Boolean                        - �����Ƿ����
}

function BigNumberCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
{* �����űȽ�������������ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1��

   ������
     Num1: TCnBigNumber                   - ���ȽϵĴ�������һ
     Num2: TCnBigNumber                   - ���ȽϵĴ��������

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Integer): Integer; overload;
{* �����űȽ�һ������������һ��������ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1��

   ������
     Num1: TCnBigNumber                   - ���ȽϵĴ�������
     Num2: Integer                        - ���Ƚϵ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Int64): Integer; overload;
{* �����űȽ�һ������������һ��������ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1��

   ������
     Num1: TCnBigNumber                   - ���ȽϵĴ�������
     Num2: Int64                          - ���Ƚϵ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigNumberUnsignedCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
{* �޷��űȽ�������������Ҳ���ǱȽϾ���ֵ��ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1��

   ������
     Num1: TCnBigNumber                   - ���ȽϵĴ�������һ
     Num2: TCnBigNumber                   - ���ȽϵĴ��������

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigNumberDuplicate(Num: TCnBigNumber): TCnBigNumber;
{* ����������һ���������󣬷��ش��´���������Ҫ�� BigNumberFree ���ͷš�

   ������
     Num: TCnBigNumber                    - �����ƵĴ�������

   ����ֵ��TCnBigNumber                   - �����½��Ĵ�������
}

function BigNumberCopy(Dst: TCnBigNumber; Src: TCnBigNumber): TCnBigNumber;
{* ����һ���������󣬳ɹ����� Dst ����

   ������
     Dst: TCnBigNumber                    - Ŀ���������
     Src: TCnBigNumber                    - Դ��������

   ����ֵ��TCnBigNumber                   - ���Ƴɹ��򷵻�Ŀ���������ʧ�ܷ��� nil
}

function BigNumberCopyLow(Dst: TCnBigNumber; Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
{* ����һ����������ĵ� WordCount �� UInt32/UInt64 Ԫ�أ��ɹ����� Dst��

   ������
     Dst: TCnBigNumber                    - Ŀ���������
     Src: TCnBigNumber                    - Դ��������
     WordCount: Integer                   - �����Ƶĵ�λԪ����

   ����ֵ��TCnBigNumber                   - ���Ƴɹ��򷵻�Ŀ���������ʧ�ܷ��� nil
}

function BigNumberCopyHigh(Dst: TCnBigNumber; Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
{* ����һ����������ĸ� WordCount �� UInt32/UInt64 Ԫ�أ��ɹ����� Dst��

   ������
     Dst: TCnBigNumber                    - Ŀ���������
     Src: TCnBigNumber                    - Դ��������
     WordCount: Integer                   - �����Ƶĸ�λԪ����

   ����ֵ��TCnBigNumber                   - ���Ƴɹ��򷵻�Ŀ���������ʧ�ܷ��� nil
}

function BigNumberGetLow32(Num: TCnBigNumber): Cardinal;
{* ȡ��һ�������ĵ� 32 λ����������š�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Cardinal                       - ���ص� 32 λ�޷�������
}

function BigNumberGetLow64(Num: TCnBigNumber): TUInt64;
{* ȡ��һ�������ĵ� 64 λ����������š�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��TUInt64                        - ���ص� 64 λ�޷�������
}

procedure BigNumberSwap(Num1: TCnBigNumber; Num2: TCnBigNumber);
{* ��������������������ݡ�

   ������
     Num1: TCnBigNumber                   - �������Ĵ�������һ
     Num2: TCnBigNumber                   - �������Ĵ��������

   ����ֵ�����ޣ�
}

procedure BigNumberSwapBit(Num: TCnBigNumber; BitIndex1: Integer; BitIndex2: Integer);
{* ��������������ָ�� Bit λ�����ݣ�BitIndex ���� 0 ��ʼ��

   ������
     Num: TCnBigNumber                    - �������Ĵ�������
     BitIndex1: Integer                   - ��������λ����һ
     BitIndex2: Integer                   - ��������λ������

   ����ֵ�����ޣ�
}

function BigNumberRandBytes(Num: TCnBigNumber; BytesCount: Integer): Boolean;
{* �����̶��ֽڳ��ȵ��������������֤���λ�� 1����������ֽڶ�����֤�� 0��

   ������
     Num: TCnBigNumber                    - ������������Ĵ�������
     BytesCount: Integer                  - �ֽڳ���

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigNumberRandBits(Num: TCnBigNumber; BitsCount: Integer): Boolean;
{* �����̶�λ���ȵ��������������֤���λ�� 1����������ֽڶ�����֤�� 0��

   ������
     Num: TCnBigNumber                    - ������������Ĵ�������
     BitsCount: Integer                   - λ����

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigNumberRandRange(Num: TCnBigNumber; Range: TCnBigNumber): Boolean;
{* ���� [0, Range) ֮������������

   ������
     Num: TCnBigNumber                    - ������������Ĵ�������
     Range: TCnBigNumber                  - ��������ޣ�������ڸ���

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigNumberAnd(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* ������������λ�룬������� Res �У����������Ƿ�ɹ���Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num1: TCnBigNumber                   - ����λ��Ĵ�������һ
     Num2: TCnBigNumber                   - ����λ��Ĵ��������

   ����ֵ��Boolean                        - �����Ƿ�λ��ɹ�
}

function BigNumberOr(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* ������������λ�򣬽������ Res �У����������Ƿ�ɹ���Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num1: TCnBigNumber                   - ����λ��Ĵ�������һ
     Num2: TCnBigNumber                   - ����λ��Ĵ��������

   ����ֵ��Boolean                        - �����Ƿ�λ��ɹ�
}

function BigNumberXor(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* ������������λ��򣬽������ Res �У����������Ƿ�ɹ���Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num1: TCnBigNumber                   - ����λ���Ĵ�������һ
     Num2: TCnBigNumber                   - ����λ���Ĵ��������

   ����ֵ��Boolean                        - �����Ƿ�λ���ɹ�
}

function BigNumberUnsignedAdd(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* �������������޷�����ӣ�������� Res �У���������Ƿ�ɹ���Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɺ͵Ĵ�������
     Num1: TCnBigNumber                   - ����һ
     Num2: TCnBigNumber                   - ������

   ����ֵ��Boolean                        - �����Ƿ���ӳɹ�
}

function BigNumberUnsignedSub(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* �������������޷��������Num1 �� Num2��������� Res �У�
   ��������Ƿ�ɹ����� Num1 < Num2 ��ʧ�ܡ�

   ������
     Res: TCnBigNumber                    - �������ɲ�Ĵ�������
     Num1: TCnBigNumber                   - ������������ֵ������ڵ��ڼ���
     Num2: TCnBigNumber                   - ����������ֵ����С�ڵ��ڱ�����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberAdd(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* �������������������ӣ�������� Res �У���������Ƿ�ɹ���Num1 ������ Num2��Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɺ͵Ĵ�������
     Num1: TCnBigNumber                   - ����һ
     Num2: TCnBigNumber                   - ������

   ����ֵ��Boolean                        - �����Ƿ���ӳɹ�
}

function BigNumberSub(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* ����������������������������� Res �У���������Ƿ�ɹ���Num1 ������ Num2��Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɲ�Ĵ�������
     Num1: TCnBigNumber                   - ������
     Num2: TCnBigNumber                   - ����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberShiftLeftOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* ��һ������������һλ��������� Res �У����������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - �����ƵĴ�������

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigNumberShiftRightOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* ��һ������������һλ��������� Res �У����������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - �����ƵĴ�������

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigNumberShiftLeft(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
{* ��һ������������ N λ��������� Res �У����������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - �����ƵĴ�������
     N: Integer                           - ����λ����Ϊ��ʱ��������

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigNumberShiftRight(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
{* ��һ������������ N λ��������� Res �У����������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - �����ƵĴ�������
     N: Integer                           - ����λ����Ϊ��ʱ��������

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function BigNumberSqr(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* ����һ���������ƽ��������� Res �У�����ƽ�������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberSqrt(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* ����һ���������ƽ�������������֣������ Res �У�����ƽ�������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberRoot(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Integer): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* ����һ��������� Exp �η������������֣������ Res �У����ظ������Ƿ�ɹ���
   Ҫ�� Num ����Ϊ����Exponent ����Ϊ 0 �򸺡�
   ע��FIXME: ��Ϊ�����޷����и�����㣬Ŀǰ����������ƫ����ƫ�󣬲��Ƽ�ʹ�ã�

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - ������Ĵ�������
     Exponent: Integer                    - �η�����

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberMul(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* ��������������ĳ˻�������� Res �У����س˻������Ƿ�ɹ���Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɻ��Ĵ�������
     Num1: TCnBigNumber                   - ����һ
     Num2: TCnBigNumber                   - ������

   ����ֵ��Boolean                        - �����Ƿ���˳ɹ�
}

function BigNumberMulKaratsuba(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* �� Karatsuba �㷨��������������ĳ˻�������� Res �У����س˻������Ƿ�ɹ���Res ������ Num1 �� Num2��
   ע������Ҳû���쵽����ȥ��

   ������
     Res: TCnBigNumber                    - �������ɻ��Ĵ�������
     Num1: TCnBigNumber                   - ����һ
     Num2: TCnBigNumber                   - ������

   ����ֵ��Boolean                        - �����Ƿ���˳ɹ�
}

function BigNumberMulFloat(Res: TCnBigNumber; Num: TCnBigNumber; F: Extended): Boolean;
{* ������������븡�����ĳ˻������ȡ����� Res �У����س˻������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɻ��Ĵ�������
     Num: TCnBigNumber                    - �����������
     F: Extended                          - ����������

   ����ֵ��Boolean                        - �����Ƿ���˳ɹ�
}

function BigNumberDiv(Res: TCnBigNumber; Remain: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
{* ���������������Num / Divisor���̷� Res �У������� Remain �У����س��������Ƿ�ɹ���
   Res ������ Num��Remain ������ nil �Բ���Ҫ����������
   �����������������������õ������̺�����������֮�����������������£�
   ���������������õ����̺�����������  1005 /  100 =  10 ...  5
   ���������������õ����̺͸��������� -1005 /  100 = -10 ... -5
   ���������������õ����̺�����������  1005 / -100 = -10 ...  5
   ���������������õ����̺͸��������� -1005 / -100 =  10 ... -5
   �������Ÿ��ű������ߣ���������ֵ��С�ڳ�������ֵ���Ҳ�������� 95 ���������

   ������
     Res: TCnBigNumber                    - ���������̵Ĵ�������
     Remain: TCnBigNumber                 - �������������Ĵ������󣬿�Ϊ nil
     Num: TCnBigNumber                    - ������
     Divisor: TCnBigNumber                - ����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberRoundDiv(Res: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber; out Rounding: Boolean): Boolean;
{* ���������������Num / Divisor������������� Res �У�Res ������ Num��
   ע����ķ���ʼ���Ǿ���ֵ��ķ����� Round ������������һ�£��������������������˫�Ĺ��򣬷�����롣
   ���س��������Ƿ�ɹ���Rounding ����������ʵ��������������True ��ʾ�룬False ��ʾ�ᡣ

   ������
     Res: TCnBigNumber                    - ���������̵Ĵ�������
     Num: TCnBigNumber                    - ������
     Divisor: TCnBigNumber                - ����
     out Rounding: Boolean                - ������ʵ��������������True ��ʾ�룬False ��ʾ��

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberMod(Remain: TCnBigNumber; Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* �������������࣬Num mod Divisor�������� Remain �У��������������ͬ�� BigNumberDiv ������
   ������������Ƿ�ɹ���Remain ������ Num��

   ������
     Remain: TCnBigNumber                 - �������������Ĵ�������
     Num: TCnBigNumber                    - ������
     Divisor: TCnBigNumber                - ����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberNonNegativeMod(Remain: TCnBigNumber;
  Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* ����������Ǹ����࣬Num mod Divisor�������� Remain �У�0 <= Remain < |Divisor|��
   ���������������ͬ�� BigNumberMod ����ͨ���Ӽ� Divisor ��ȷ�� Remain ʼ�մ����㡣
   �� BigNumberMod ��ͬ���ǣ�
   ���������������ȵõ����̺͸�������������������������� -1005 /  100 = ... 95
   ���������������ȵõ����̺͸�������������������������� -1005 / -100 = ... 95
   ������������Ƿ�ɹ���

   ������
     Remain: TCnBigNumber                 - �������������Ĵ�������
     Num: TCnBigNumber                    - ������
     Divisor: TCnBigNumber                - ����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberMulWordNonNegativeMod(Res: TCnBigNumber;
  Num: TCnBigNumber; N: Integer; Divisor: TCnBigNumber): Boolean;
{* ����������� 32 λ�з��������ٷǸ����࣬������ Res �У�0 <= Remain < |Divisor|��
   Res ʼ�մ����㣬������������Ƿ�ɹ���

   ������
     Res: TCnBigNumber                    - �������������Ĵ�������
     Num: TCnBigNumber                    - ������
     N: Integer                           - ����
     Divisor: TCnBigNumber                - ����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberAddMod(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* ����������ͺ�Ǹ����࣬Ҳ���� Res = (Num1 + Num2) mod Divisor��������������Ƿ�ɹ���

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num1: TCnBigNumber                   - ����һ
     Num2: TCnBigNumber                   - ������
     Divisor: TCnBigNumber                - ����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberSubMod(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* ������������Ǹ����࣬Ҳ���� Res = (Num1 - Num2) mod Divisor��������������Ƿ�ɹ���

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num1: TCnBigNumber                   - ������
     Num2: TCnBigNumber                   - ����
     Divisor: TCnBigNumber                - ����

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberDivFloat(Res: TCnBigNumber; Num: TCnBigNumber;
  F: Extended): Boolean;
{* ������������븡�������̣����ȡ����� Res �У���������Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - �������󱻳���
     F: Extended                          - ����������

   ����ֵ��Boolean                        - �����Ƿ�����ɹ�
}

function BigNumberPower(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Cardinal): Boolean;
{* ������������η������ؼ����Ƿ�ɹ���Res ������ Num��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - ����
     Exponent: Cardinal                   - ָ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberExp(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
{* ����� Num �� Exponent  �η������س˷������Ƿ�ɹ��������ʱ��
   Res ������ Num �� Exponent��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - ����
     Exponent: TCnBigNumber               - ָ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberGcd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* ���������� Num1 �� Num2 �����Լ����Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num1: TCnBigNumber                   - �������Լ���Ĵ�������һ
     Num2: TCnBigNumber                   - �������Լ���Ĵ��������

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberLcm(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* ���������� Num1 �� Num2 ����С��������Res ������ Num1 �� Num2��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num1: TCnBigNumber                   - ������С�������Ĵ�������һ
     Num2: TCnBigNumber                   - ������С�������Ĵ��������

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberUnsignedMulMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber): Boolean;
{* ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ�����
   ע��: ��������������Ը�ֵ��Ҳ���Ǿ�����ֵ������㡣

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ����һ
     B: TCnBigNumber                      - ������
     C: TCnBigNumber                      - ����

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberMulMod(Res: TCnBigNumber; A: TCnBigNumber; B: TCnBigNumber;
  C: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ�����
   ע��: A��B �����Ǹ�ֵ���˻�Ϊ��ʱ�����Ϊ C - �˻�Ϊ�����ࡣ
   ����÷�����Ϊ������� BigNumberDirectMulMod �������Բ�����ʹ�á�

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ����һ
     B: TCnBigNumber                      - ������
     C: TCnBigNumber                      - ����

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberDirectMulMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber): Boolean;
{* ��ͨ���� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ�����
   ע�⣺λ������ʱ���÷���������� BigNumberMulMod ����Ҫ�첻�٣������ڲ�ִ�е��� NonNegativeMod������Ϊ����

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ����һ
     B: TCnBigNumber                      - ������
     C: TCnBigNumber                      - ����

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberMontgomeryReduction(Res: TCnBigNumber; T: TCnBigNumber;
  R: TCnBigNumber; N: TCnBigNumber; NNegInv: TCnBigNumber): Boolean;
{* �ɸ�����Լ�򷨿��ټ��� (T * R^-1) mod N������Ҫ�� R �Ǹպñ� N ��� 2 �������ݣ�
   NNegInv ��Ԥ�ȼ���õ� N �� R �ĸ�ģ��Ԫ��T ����Ϊ���ұ���С�� N * R��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     T: TCnBigNumber                      - �ɸ�����Լ����� T
     R: TCnBigNumber                      - �ɸ�����Լ����� R
     N: TCnBigNumber                      - �ɸ�����Լ�����ģ�� N
     NNegInv: TCnBigNumber                - Ԥ�ȼ���õ� N �� R �ĸ�ģ��Ԫ

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberMontgomeryMulMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; R: TCnBigNumber; R2ModN: TCnBigNumber;
  N: TCnBigNumber; NNegInv: TCnBigNumber): Boolean;
{* �ɸ�����ģ�˷����ڲ�ʹ���Ĵ��ɸ�����Լ�򷨣����ټ��� A * B * R^-1 mod N������Ҫ�� R �Ǹպñ� N ��� 2 �������ݣ�
   R2ModN ��Ԥ�ȼ���õ� R^2 mod N ��ֵ��NNegInv ��Ԥ�ȼ���õ� N �� R �ĸ�ģ��Ԫ��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - �ɸ�����ģ�˷���ĳ���һ
     B: TCnBigNumber                      - �ɸ�����ģ�˷���ĳ�����
     R: TCnBigNumber                      - �ɸ�����ģ�˷���� R
     R2ModN: TCnBigNumber                 - Ԥ�ȼ���õ� R^2 mod N ��ֵ
     N: TCnBigNumber                      - �ɸ�����Լ�����ģ�� N
     NNegInv: TCnBigNumber                - Ԥ�ȼ���õ� N �� R �ĸ�ģ��Ԫ

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberPowerWordMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: Cardinal; C: TCnBigNumber): Boolean;
{* ���ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��C ֮һ���ڲ����� BigNumberPowerMod��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - �����������
     B: Cardinal                          - ��������
     C: TCnBigNumber                      - ģ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberPowerMod(Res: TCnBigNumber; A: TCnBigNumber; B: TCnBigNumber;
  C: TCnBigNumber): Boolean;
{* �������ڷ����ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ�����ܱ�������ɸ��������ô�Լ�ٷ�֮ʮ��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ����һ
     B: TCnBigNumber                      - ������
     C: TCnBigNumber                      - ģ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberMontgomeryPowerMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* �ɸ����������ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ�������Բ���Բ��á�

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ����һ
     B: TCnBigNumber                      - ������
     C: TCnBigNumber                      - ģ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberPowerPowerMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber; N: TCnBigNumber): Boolean;
{* ���ټ��� A ^ (B ^ C) mod N��������ֱ���㣬�����������Res ������ A��B��C��N ֮һ��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ����һ
     B: TCnBigNumber                      - ������
     C: TCnBigNumber                      - ָ��
     N: TCnBigNumber                      - ģ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberLog2(Num: TCnBigNumber): Extended;
{* ���ش����� 2 Ϊ�׵Ķ�������չ���ȸ���ֵ���ڲ�����չ���ȸ���ʵ�֡�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Extended                       - ���� 2 Ϊ�׵Ķ���ֵ
}

function BigNumberLog10(Num: TCnBigNumber): Extended;
{* ���ش����� 10 Ϊ�׵ĳ��ö�������չ���ȸ���ֵ���ڲ�����չ���ȸ���ʵ�֡�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Extended                       - ���س��ö���ֵ
}

function BigNumberLogN(Num: TCnBigNumber): Extended;
{* ���ش����� e Ϊ�׵���Ȼ��������չ���ȸ���ֵ���ڲ�����չ���ȸ���ʵ�֡�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��Extended                       - ������Ȼ����ֵ
}

function BigNumberFermatCheckComposite(A: TCnBigNumber; B: TCnBigNumber;
  C: TCnBigNumber; T: Integer): Boolean;
{* Miller-Rabin �㷨�еĵ��η�����ԣ����� True ��ʾ B ����������
   ע�� A B C ��������ѡ��B �Ǵ����Ե�������A ���������C �� B - 1 ���� T λ��õ��ĵ�һ��������

   ������
     A: TCnBigNumber                      - �����
     B: TCnBigNumber                      - �����Ե�����
     C: TCnBigNumber                      - B - 1 ���� T λ��õ��ĵ�һ������
     T: Integer                           - Miller-Rabin �㷨�е� T ֵ

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberIsProbablyPrime(Num: TCnBigNumber; TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* �������ж�һ�������Ƿ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ����
   ע�ⲻ�ܲ��ü򵥵ķ���С�����жϻ� Solovay-Strassen ���������Լ�⣬��Ϊ�� Carmichael ����Ч��

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������
     TestCount: Integer                   - ���Դ���

   ����ֵ��Boolean                        - �����Ƿ�����
}

function BigNumberGeneratePrime(Num: TCnBigNumber; BytesCount: Integer;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* ����һ��ָ���ֽڳ��ȵĴ�����������֤���λΪ 1��
   TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ����

   ������
     Num: TCnBigNumber                    - �������ɽ���Ĵ�������
     BytesCount: Integer                  - �ֽڳ���
     TestCount: Integer                   - ���Դ���

   ����ֵ��Boolean                        - �����Ƿ����ɳɹ�
}

function BigNumberGeneratePrimeByBitsCount(Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* ����һ��ָ��������λ���Ĵ����������λȷ��Ϊ 1��
   TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ����

   ������
     Num: TCnBigNumber                    - �������ɽ���Ĵ�������
     BitsCount: Integer                   - λ����
     TestCount: Integer                   - ���Դ���

   ����ֵ��Boolean                        - �����Ƿ����ɳɹ�
}

function BigNumberNextPrime(Res: TCnBigNumber; Num: TCnBigNumber;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* ����һ���� Num �����ȵĴ������������ Res��Res ������ Num��
   TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - ���ȽϵĴ�������
     TestCount: Integer                   - ���Դ���

   ����ֵ��Boolean                        - �����Ƿ����ɳɹ�
}

function BigNumberCheckPrimitiveRoot(R: TCnBigNumber; Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
{* ԭ���жϸ����������ж� R �Ƿ���� Prime - 1 ��ÿ�����ӣ����� R ^ (ʣ�����ӵĻ�) mod Prime <> 1��
   Factors ������ Prime - 1 �Ĳ��ظ������������б��ɴ� BigNumberFindFactors ��ȡ��ȥ�ض�����

   ������
     R: TCnBigNumber                      - ���жϵĴ�������
     Prime: TCnBigNumber                  - ����
     Factors: TCnBigNumberList            - Prime - 1 �Ĳ��ظ������������б�

   ����ֵ��Boolean                        - �����Ƿ��жϳɹ�
}

function BigNumberGetMinRootFromPrime(Res: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* ����һ������ԭ�������ؼ����Ƿ�ɹ���

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Prime: TCnBigNumber                  - ����

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberIsInt32(Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 32 λ�з���������Χ�ڵ�����

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ��� 32 λ�з���������Χ��
}

function BigNumberIsUInt32(Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 32 λ�޷���������Χ�ڵ�����

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ��� 32 λ�޷���������Χ��
}

function BigNumberIsInt64(Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 64 λ�з���������Χ�ڵ�����

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ��� 64 λ�з���������Χ��
}

function BigNumberIsUInt64(Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ�� 64 λ�޷������ͷ�Χ�ڵ�����

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ��� 64 λ�޷���������Χ��
}

function BigNumberIsFloat(Num: TCnBigNumber): Boolean;
{* �����Ƿ���һ����չ���ȸ��㷶Χ�ڵ�����ע����жϷ�Χ������֤���ȡ�
   ���� Win64 �� Extended ʵ������˫���ȣ�Win32 �²�����չ���ȡ�

   ������
     Num: TCnBigNumber                    - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�����չ���ȸ��㷶Χ��
}

procedure BigNumberExtendedEuclideanGcd(A: TCnBigNumber; B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* ��չŷ�����շת��������Ԫһ�β������� A * X + B * Y = 1 �������⡣
   �����������б�֤ A B ���أ���Ϊ���ֻ���� A * X + B * Y = GCD(A, B)��
   A, B ����֪������X, Y �ǽ�����Ľ����ע�� X �п���С�� 0������Ҫ�����������ټ��� B

   ������
     A: TCnBigNumber                      - ��Ԫһ�β�������ϵ�� A
     B: TCnBigNumber                      - ��Ԫһ�β�������ϵ�� B
     X: TCnBigNumber                      - �������ɽ�� X �Ĵ�������
     Y: TCnBigNumber                      - �������ɽ�� Y �Ĵ�������

   ����ֵ�����ޣ�
}

procedure BigNumberExtendedEuclideanGcd2(A: TCnBigNumber; B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* ��չŷ�����շת��������Ԫһ�β������� A * X - B * Y = 1 �������⡣
   �����������б�֤ A B ���أ���Ϊ���ֻ���� A * X + B * Y = GCD(A, B)��
   A, B ����֪������X, Y �ǽ�����Ľ����ע�� X �п���С�� 0������Ҫ�����������ټ��� B��
   X ����Ϊ A ��� B ��ģ��Ԫ�أ���˱��㷨Ҳ������ A ��� B ��ģ��Ԫ�ء�
   �����ڿ������� -Y�����Ա���������һ�����ǵ�ͬ�ģ���

   ������
     A: TCnBigNumber                      - ��Ԫһ�β�������ϵ�� A
     B: TCnBigNumber                      - ��Ԫһ�β�������ϵ�� B
     X: TCnBigNumber                      - �������ɽ�� X �Ĵ�������
     Y: TCnBigNumber                      - �������ɽ�� Y �Ĵ�������

   ����ֵ�����ޣ�
}

function BigNumberModularInverse(Res: TCnBigNumber;
  X: TCnBigNumber; Modulus: TCnBigNumber; CheckGcd: Boolean = False): Boolean;
{* �� X ��� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ��
   CheckGcd ����Ϊ True ʱ���ڲ����� X��Modulus �Ƿ��أ���������ֱ�ӷ��� False��
   �����������б�֤ X��Modulus ���أ��� Res ������ X �� Modulus��

   ������
     Res: TCnBigNumber                    - ��������ģ��Ԫ�Ĵ�������
     X: TCnBigNumber                      - ������Ĵ�������
     Modulus: TCnBigNumber                - ģ��
     CheckGcd: Boolean                    - �Ƿ��黥��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberPrimeModularInverse(Res: TCnBigNumber;
  X: TCnBigNumber; Modulus: TCnBigNumber): Boolean;
{* �� X ������� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ��
   �����������б�֤ Modulus Ϊ�������� Res ������ X �� Modulus���ڲ��÷���С������ֵ������

   ������
     Res: TCnBigNumber                    - ��������ģ��Ԫ�Ĵ�������
     X: TCnBigNumber                      - ������Ĵ�������
     Modulus: TCnBigNumber                - ģ����

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberNegativeModularInverse(Res: TCnBigNumber;
  X: TCnBigNumber; Modulus: TCnBigNumber; CheckGcd: Boolean = False): Boolean;
{* �� X ��� Modulus �ĸ�ģ����и�ģ��Ԫ Y������ (X * Y) mod M = -1��X ��Ϊ��ֵ��Y �����ֵ��
   �����������б�֤ X��Modulus ���أ��� Res ������ X �� Modulus��

   ������
     Res: TCnBigNumber                    - �������ɸ�ģ��Ԫ�Ĵ�������
     X: TCnBigNumber                      - ������Ĵ�������
     Modulus: TCnBigNumber                - ģ��
     CheckGcd: Boolean                    - �Ƿ��黥��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

procedure BigNumberModularInverseWord(Res: TCnBigNumber;
  X: Integer; Modulus: TCnBigNumber; CheckGcd: Boolean = False);
{* �� 32 λ�з����� X ��� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ��
   �����������б�֤ X��Modulus ���أ��� Res ������ X �� Modulus��

   ������
     Res: TCnBigNumber                    - ��������ģ��Ԫ�Ĵ�������
     X: Integer                           - �����������
     Modulus: TCnBigNumber                - ģ��
     CheckGcd: Boolean                    - ���黥��

   ����ֵ�����ޣ�
}

function BigNumberLegendre(A: TCnBigNumber; P: TCnBigNumber): Integer;
{* �ö��λ����ɵݹ�������õ·��� (A / P) ��ֵ���Ͽ졣�����������б�֤ P Ϊ��������

   ������
     A: TCnBigNumber                      - ���õ·����е� A
     P: TCnBigNumber                      - ���õ·����е� P

   ����ֵ��Integer                        - �������õ·���
}

function BigNumberLegendre2(A: TCnBigNumber; P: TCnBigNumber): Integer; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* ��ŷ���б𷨼������õ·��� (A / P) ��ֵ�����������Ƽ�ʹ�á�

   ������
     A: TCnBigNumber                      - ���õ·����е� A
     P: TCnBigNumber                      - ���õ·����е� P

   ����ֵ��Integer                        - �������õ·���
}

function BigNumberTonelliShanks(Res: TCnBigNumber; A: TCnBigNumber; P: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* ʹ�� Tonelli-Shanks �㷨����ģ��������ʣ����⣬Ҳ������ Res^2 mod P = A�������Ƿ��н⡣
   �����������б�֤ P Ϊ���������������������η����÷������������Ƽ�ʹ�á�

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ģ��������ʣ���е� A
     P: TCnBigNumber                      - ģ��������ʣ���е� P

   ����ֵ��Boolean                        - �����Ƿ��н�
}

function BigNumberLucas(Res: TCnBigNumber; A: TCnBigNumber; P: TCnBigNumber): Boolean;
{* ʹ�� IEEE P1363 �淶�е� Lucas ���н���ģ��������ʣ����⣬Ҳ������ Res^2 mod P = A�������Ƿ��н⡣
   �ƺ� P Ӧ����ģ 8 �� 1 ��������

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ģ��������ʣ���е� A
     P: TCnBigNumber                      - ģ��������ʣ���е� P

   ����ֵ��Boolean                        - �����Ƿ��н�
}

function BigNumberSquareRootModPrime(Res: TCnBigNumber; A: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* ͨ�ú�����ģ��������ʣ�� X^2 mod P = A �Ľ⣬�����Ƿ����ɹ�����ɹ���Res ������һ����ֵ�Ľ⡣

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ģ��������ʣ���е� A
     Prime: TCnBigNumber                  - ģ��������ʣ���е� P

   ����ֵ��Boolean                        - �����Ƿ��н�
}

function BigNumberJacobiSymbol(A: TCnBigNumber; N: TCnBigNumber): Integer;
{* �����ſɱȷ��ţ����� N ��������������� N �Ǹ��������� A Ϊ��ʱ��ͬ�� N Ϊ��������
   A Ϊ��ʱ��ͬ�� -(A / -N)����� N �����������ͬ�����õ·��š�A ����Ϊ����������

   ������
     A: TCnBigNumber                      - �ſɱȷ����е� A
     N: TCnBigNumber                      - �ſɱȷ����е� N

   ����ֵ��Integer                        - �����ſɱȷ���
}

procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
{* �ҳ����������������б����ظ���δ����

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     Factors: TCnBigNumberList            - �������ɽ���Ĵ����б�

   ����ֵ�����ޣ�
}

procedure BigNumberFindAllFactors(Num: TCnBigNumber; AllFactors: TCnBigNumberList);
{* �ҳ����������������б����ظ�������

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     AllFactors: TCnBigNumberList         - �������ɽ���Ĵ����б�

   ����ֵ�����ޣ�
}

procedure BigNumberEuler(Res: TCnBigNumber; Num: TCnBigNumber);
{* �󲻴��ڴ����� Num ������ Num ���ص��������ĸ�����Ҳ����ŷ��������

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ�����ޣ�
}

function BigNumberLucasUSequenceMod(P: TCnBigNumber; Q: TCnBigNumber; K: TCnBigNumber;
  N: TCnBigNumber; U: TCnBigNumber): Boolean;
{* ���� Lucas �� U ���У������������б�֤ N Ϊ��������
   Lucas �� U ���еݹ鶨��Ϊ��U0 = 0, U1 = 1, and Uk = P * Uk-1 - Q * Vk-2   for k >= 2��
   U ���� Uk mod N��

   ������
     P: TCnBigNumber                      - Lucas �����е� P ������������
     Q: TCnBigNumber                      - Lucas �����е� Q ������������
     K: TCnBigNumber                      - Lucas �����е� K ����
     N: TCnBigNumber                      - Lucas �����е� N ģ��
     U: TCnBigNumber                      - ���� Lucas �� U �����е� Uk mod N ��ֵ

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function BigNumberLucasVSequenceMod(X: TCnBigNumber; Y: TCnBigNumber; K: TCnBigNumber;
  N: TCnBigNumber; Q: TCnBigNumber; V: TCnBigNumber): Boolean;
{* ���� IEEE P1363 �Ĺ淶��˵���� Lucas �� V ���У������������б�֤ N Ϊ��������
   Lucas �� V ���еݹ鶨��Ϊ��V0 = 2, V1 = X, and Vk = X * Vk-1 - Y * Vk-2   for k >= 2��
   V ���� Vk mod N��Q ���� Y ^ (K div 2) mod N��

   ������
     X: TCnBigNumber                      - Lucas �����е� X ������������
     Y: TCnBigNumber                      - Lucas �����е� Y ������������
     K: TCnBigNumber                      - Lucas �����е� K ����
     N: TCnBigNumber                      - Lucas �����е� N ģ��
     Q: TCnBigNumber                      - ���� Lucas �� V �����е� Y ^ (K div 2) mod N ��ֵ
     V: TCnBigNumber                      - ���� Lucas �� V �����е� Vk mod N ��ֵ

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers: TCnBigNumberList; Factors: TCnBigNumberList): Boolean; overload;
{* ���й�ʣ�ඨ�����������뻥�صĳ�����һԪ����ͬ�෽�������С�⣬��������Ƿ�ɹ���
  ����Ϊ�����б�Remainers ֧�ָ���������������ȷ�� Factors ��Ϊ�����������ء�

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Remainers: TCnBigNumberList          - �����б�
     Factors: TCnBigNumberList            - �����б�

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers: TCnInt64List; Factors: TCnInt64List): Boolean; overload;
{* ���й�ʣ�ඨ�����������뻥�صĳ�����һԪ����ͬ�෽�������С�⣬��������Ƿ�ɹ���
   ����Ϊ Int64 �б�

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     Remainers: TCnInt64List              - �����б�
     Factors: TCnInt64List                - �����б�

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberIsPerfectPower(Num: TCnBigNumber): Boolean;
{* �жϴ����Ƿ�����ȫ�ݣ������ϴ�ʱ��һ����ʱ��

   ������
     Num: TCnBigNumber                    - ���жϴ�������

   ����ֵ��Boolean                        - �����Ƿ���ȫ��
}

procedure BigNumberFillCombinatorialNumbers(List: TCnBigNumberList; N: Integer);
{* ��������� C(m, N) �����ɴ�������������������У����� m �� 0 �� N��

   ������
     List: TCnBigNumberList               - ��������������Ĵ����б�
     N: Integer                           - ����� N

   ����ֵ�����ޣ�
}

procedure BigNumberFillCombinatorialNumbersMod(List: TCnBigNumberList; N: Integer; P: TCnBigNumber);
{* ��������� C(m, N) mod P �����ɴ�������������������У����� m �� 0 �� N��

   ������
     List: TCnBigNumberList               - �������ɽ���Ĵ����б�
     N: Integer                           - ����� N
     P: TCnBigNumber                      - ģ��

   ����ֵ�����ޣ�
}

function BigNumberAKSIsPrime(N: TCnBigNumber): Boolean; overload;
{* �� AKS �㷨׼ȷ�ж�ĳ���������Ƿ����������ж� 9223372036854775783 Լ�� 3 �룬
   �ж� 334551562509014333221406299998596765179 Լ�� 10 �롣

   ������
     N: TCnBigNumber                      - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�����
}

function BigNumberAKSIsPrime(N: Int64): Boolean; overload;
{* �� AKS �㷨׼ȷ�ж�ĳ�������Ƿ���������

   ������
     N: Int64                             - ���жϵ�������

   ����ֵ��Boolean                        - �����Ƿ�����
}

function BigNumberBPSWIsPrime(N: TCnBigNumber): Boolean; overload;
{* �� Baillie-PSW �㷨�����׼ȷ�ж�ĳ���������Ƿ����������ж� 2048 λ������ʱһ����
   RSA ���������� 2048 λ��������ʹ�ñ��㷨�����жϣ���ʱԼ 5 �롣

   ������
     N: TCnBigNumber                      - ���жϵĴ�������

   ����ֵ��Boolean                        - �����Ƿ�����
}

function BigNumberBPSWIsPrime(N: Int64): Boolean; overload;
{* �� Baillie-PSW �㷨�����׼ȷ�ж�ĳ�������Ƿ���������

   ������
     N: Int64                             - ���жϵ�������

   ����ֵ��Boolean                        - �����Ƿ�����
}

function BigNumberIsMersennePrime(E: Integer): Boolean;
{* �� Lucas-Lehmer ������ 2 �� E �η���һ�Ƿ���÷ɭ������E �ﵽ 20 ��ʱ�������Ϳ�ʼ���ˡ�

   ������
     E: Integer                           - ������ 2 ��ָ��

   ����ֵ��Boolean                        - �����Ƿ�÷ɭ����
}

function BigNumberNonAdjanceFormWidth(N: TCnBigNumber; Width: Integer = 1): TShortInts;
{* ���ش����� Width ��ȣ�Ҳ���� 2^Width ���ƣ��� NAF ����ֵ��������ʽ��Width Ϊ 1 ʱΪ��ͨ NAF ��ʽ��
   Width 1 �� 2 �ȼۡ�ÿ���ֽ����з���һ�����ֵС�� 2^(Width-1)������������ 1 < W <= 7��

   ������
     N: TCnBigNumber                      - ������Ĵ�������
     Width: Integer                       - ���

   ����ֵ��TShortInts                     - ���� NAF ���
}

function BigNumberBigStepGiantStep(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; M: TCnBigNumber): Boolean;
{* ��С���㷨����ɢ�������� A^X mod M = B �Ľ� Res��Ҫ�� A �� M ���ء�

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     A: TCnBigNumber                      - ��ɢ���������е� A
     B: TCnBigNumber                      - ��ɢ���������е� B
     M: TCnBigNumber                      - ��ɢ���������е�ģ�� M

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberMultiplicativeOrder(Res: TCnBigNumber; N: TCnBigNumber;
  R: TCnBigNumber): Boolean;
{* ��ŷ������������� N ģ R �ĳ˷��ף�Ҳ�������� N^K mod R = 1 �� K ����С������ֵ��
   �˷���һ���� R С������ N ��С�޹ء�����ŷ������������ʱ R ��ŷ������ֵһ�������ʽ��δ����С��
   ���Գ˷��ױ�Ȼ�� R ��ŷ������ֵ���������ɽ��������ֽ����ϱ������ȱ�������Ч�ʸߡ�
   ע��˷��ױ��� N �� R ���زŴ��ڣ��粻���أ����������� False��

   ������
     Res: TCnBigNumber                    - �������ɽ���Ĵ�������
     N: TCnBigNumber                      - ����˷��׵Ĵ���
     R: TCnBigNumber                      - ģ��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function BigNumberDebugDump(Num: TCnBigNumber): string;
{* ��ӡ�����ڲ���Ϣ��

   ������
     Num: TCnBigNumber                    - ������Ĵ�������

   ����ֵ��string                         - �����ڲ���Ϣ�ַ���
}

function BigNumberRawDump(Num: TCnBigNumber; Mem: Pointer = nil): Integer;
{* �������ڲ���Ϣԭ�ⲻ�� Dump �� Mem ��ָ���ڴ�������� Mem �� nil���򷵻�������ֽڳ��ȡ�

   ������
     Num: TCnBigNumber                    - ������Ĵ�������
     Mem: Pointer                         - ����������ݿ��ַ

   ����ֵ��Integer                        - ����ʵ��д���ֽڳ���
}

// ========================= ϡ������б�������� ==============================

function SparseBigNumberListIsZero(P: TCnSparseBigNumberList): Boolean;
{* �ж� SparseBigNumberList �Ƿ�Ϊ 0��ע�� nil��0 ���Ψһ 1 ������ 0������Ϊ 0 ����

   ������
     P: TCnSparseBigNumberList            - ���жϵĴ����б�

   ����ֵ��Boolean                        - �����Ƿ�Ϊ 0
}

function SparseBigNumberListEqual(A: TCnSparseBigNumberList; B: TCnSparseBigNumberList): Boolean;
{* �ж����� SparseBigNumberList �Ƿ���ȣ�ע�� nil��0 ���Ψһ 1 ������ 0������Ϊ 0 ����

   ������
     A: TCnSparseBigNumberList            - ���ȽϵĴ����б�һ
     B: TCnSparseBigNumberList            - ���ȽϵĴ����б��

   ����ֵ��Boolean                        - �����Ƿ����
}

procedure SparseBigNumberListCopy(Dst: TCnSparseBigNumberList; Src: TCnSparseBigNumberList);
{* �� Src ������ Dst��

   ������
     Dst: TCnSparseBigNumberList          - Ŀ������б�
     Src: TCnSparseBigNumberList          - Դ�����б�

   ����ֵ�����ޣ�
}

procedure SparseBigNumberListMerge(Dst: TCnSparseBigNumberList; Src1: TCnSparseBigNumberList;
  Src2: TCnSparseBigNumberList; Add: Boolean = True);
{* �ϲ����� SparseBigNumberList ��Ŀ�� List �У�ָ����ͬ��ϵ�� Add Ϊ True ʱ��ӣ����������
   Dst ������ Src1 �� Src2��Src1 �� Src2 ������ȡ�

   ������
     Dst: TCnSparseBigNumberList          - Ŀ������б�
     Src1: TCnSparseBigNumberList         - ���ϲ��Ĵ����б�һ
     Src2: TCnSparseBigNumberList         - ���ϲ��Ĵ����б��
     Add: Boolean                         - True ��ʾ��ӣ�False ��ʾ���

   ����ֵ�����ޣ�
}

// ============================ ������������ ===================================

function CnBigNumberIs64Mode: Boolean;
{* ��ǰ��������Ĺ���ģʽ�Ƿ����ڲ� 64 λ�洢ģʽ��������ʹ��

   ������
     ���ޣ�

   ����ֵ��Boolean                        - �����Ƿ� 64 λ��������ģʽ
}

var
  CnBigNumberOne: TCnBigNumber = nil;     // ��ʾ 1 �Ĵ�������
  CnBigNumberZero: TCnBigNumber = nil;    // ��ʾ 0 �Ĵ�������
  CnBigNumberNegOne: TCnBigNumber = nil;  // ��ʾ -1 �Ĵ�������

implementation

uses
  CnPrime, CnBigDecimal, CnFloat, CnBase64;

resourcestring
{$IFDEF BN_DATA_USE_64}
  SCnErrorBigNumberInvalid64ModRange = 'Mod Word only Supports Unsigned Int32';
{$ENDIF}
  SCnErrorBigNumberLogRange = 'Log Range Error';
  SCnErrorBigNumberLegendre = 'Legendre: A, P Must > 0';
  SCnErrorBigNumberJacobiSymbol = 'Jacobi Symbol: A, N Must > 0';
  SCnErrorBigNumberFloatExponentRange = 'Extended Float Exponent Range Error';
  SCnErrorBigNumberParamDupRef = 'Duplicated References for BigNumber Parameters';
  SCnErrorBigNumberFreeFromPool = 'Error. Try to Free a Big Number From Pool';

const
  Hex: string = '0123456789ABCDEF';

  BN_BITS_UINT_32       = 32;
  BN_BITS_UINT_64       = 64;

{$IFDEF BN_DATA_USE_64}
  BN_BYTES              = 8;      // D �����е�һ��Ԫ�����������ֽ���
  BN_BITS2              = 64;     // D �����е�һ��Ԫ����������λ��
  BN_BITS4              = 32;
  BN_MASK2              = $FFFFFFFFFFFFFFFF;
  BN_TBIT               = $8000000000000000;
  BN_MASK2l             = $FFFFFFFF;
  BN_MASK2h             = $FFFFFFFF00000000;
{$ELSE}
  BN_BYTES              = 4;      // D �����е�һ��Ԫ�����������ֽ���
  BN_BITS2              = 32;     // D �����е�һ��Ԫ����������λ��
  BN_BITS4              = 16;
  BN_MASK2              = $FFFFFFFF;
  BN_TBIT               = $80000000;
  BN_MASK2l             = $FFFF;
  BN_MASK2h             = $FFFF0000;
{$ENDIF}

  BN_MASK2S             = $7FFFFFFF; // ����������� 32/64 �Ķ�
  BN_MASK2h1            = $FFFF8000;
  BN_MASK3S             = $7FFFFFFFFFFFFFFF;
  BN_MASK3U             = $FFFFFFFFFFFFFFFF;

  BN_DEC_CONV = 1000000000;
  BN_DEC_FMT = '%u';
  BN_DEC_FMT2 = '%.9u';
  BN_PRIME_NUMBERS = 2048;

  BN_MUL_KARATSUBA = 80;  // ���ڵ��� 80 �� LongWord �ĳ˷����� Karatsuba �㷨
  CRLF = #13#10;

  SPARSE_BINARY_SEARCH_THRESHOLD = 4;

{$IFNDEF MSWINDOWS}
  MAXDWORD = Cardinal($FFFFFFFF);
{$ENDIF}

var
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalBigBinaryPool: TCnBigBinaryPool = nil;

{$IFDEF BN_DATA_USE_64}
  FCnBigNumberIs64: Boolean = True;
{$ELSE}
  FCnBigNumberIs64: Boolean = False;
{$ENDIF}

function DefBigNumberCompare(Item1, Item2: Pointer): Integer;
var
  A, B: TCnBigNumber;
begin
  A := TCnBigNumber(Item1);
  B := TCnBigNumber(Item2);

  if (A = nil) and (B = nil) then
    Result := 0
  else if A = nil then
    Result := -1
  else if B = nil then
    Result := 1
  else
    Result := BigNumberCompare(A, B);
end;

function CnBigNumberIs64Mode: Boolean;
begin
  Result := FCnBigNumberIs64;
end;

function BigNumberNew: TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
end;

procedure BigNumberInit(Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  Num.Top := 0;
  Num.Neg := 0;
  Num.DMax := 0;
  Num.D := nil;
end;

procedure BigNumberFree(Num: TCnBigNumber);
begin
  Num.Free;
end;

function BigNumberIsZero(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Num.Top = 0);
end;

function BigNumberSetZero(Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 0);
end;

function BigNumberIsOne(Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 0) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberIsNegOne(Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 1) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberSetOne(Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 1);
end;

function BigNumberIsOdd(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (Num.Top > 0) and ((PCnBigNumberElementArray(Num.D)^[0] and 1) <> 0) then
    Result := True
  else
    Result := False;
end;

function BigNumberIsEven(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (Num.Top = 0) or ((PCnBigNumberElementArray(Num.D)^[0] and 1) = 0) then
    Result := True
  else
    Result := False;
end;

function BigNumberGetWordBitsCount(L: Cardinal): Integer; {$IFDEF BN_DATA_USE_64} overload; {$ENDIF}
const
  Bits: array[0..255] of Byte = (
    0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
  );
begin
  if (L and $FFFF0000) <> 0 then
  begin
    if (L and $FF000000) <> 0 then
      Result := Bits[L shr 24] + 24
    else
      Result := Bits[L shr 16] + 16;
  end
  else
  begin
    if (L and $FF00) <> 0 then
      Result := Bits[L shr 8] + 8
    else
      Result := Bits[L];
  end;
end;

{$IFDEF BN_DATA_USE_64}

function BigNumberGetWordBitsCount(L: UInt64): Integer; overload;
var
  C: Cardinal;
begin
  C := Cardinal(L shr BN_BITS_UINT_32); // �� 32 λ
  if C = 0 then
    Result := BigNumberGetWordBitsCount(Cardinal(L and BN_MASK2)) // �� 32 λΪ 0 �򷵻ص� 32 λ��λ��
  else
    Result := BigNumberGetWordBitsCount(C) + BN_BITS_UINT_32;     // �� 32 λ��Ϊ 0 �򷵻ظ� 32 λ��λ���� 32
end;

{$ENDIF}

function BigNumberGetBitsCount(Num: TCnBigNumber): Integer;
var
  I: Integer;
begin
  Result := 0;
  if BigNumberIsZero(Num) then
    Exit;

  I := Num.Top - 1;
  Result := ((I * BN_BITS2) + BigNumberGetWordBitsCount(PCnBigNumberElementArray(Num.D)^[I]));
end;

function BigNumberGetBytesCount(Num: TCnBigNumber): Integer;
begin
  Result := (BigNumberGetBitsCount(Num) + 7) div 8;
end;

function BigNumberGetWordsCount(Num: TCnBigNumber): Integer;
begin
  Result := Num.Top;
end;

function BigNumberGetTenPrecision(Num: TCnBigNumber): Integer;
const
  LOG_10_2 = 0.30103;
var
  B, P, Q: Integer;
  N: TCnBigNumber;
begin
  Result := 0;
  if Num.IsZero then
    Exit;

  B := Num.GetBitsCount;
  if B <= 3 then
  begin
    Result := 1;
    Exit;
  end;

  P := Trunc(LOG_10_2 * B) + 1;
  Q := Trunc(LOG_10_2 * (B - 1)) + 1;
  // N λ������ȫ 1 ʱ�������Դ�ģ�Ҳ���� N + 1 λ�Ķ�����ֻ�и�λ 1 �ģ� 10 ������ʽ��λ������ (N) * Log10(2) ���������� + 1������Ϊ P
  // N λ������ֻ�и�λ 1 ʱ������ 10 ������ʽ��λ������ (N - 1) * Log10(2) ���������� + 1������Ϊ Q��Q �� P ����� 1���п������
  // Ҳ���� N λ���������ʱ���� 10 ����λ������ P��N λ��������Сֻ�и�λΪ 1 ʱ���� 10 ����λ������ Q
  // Ҳ����˵��N λ�����ƴ����ֵ����ֵ��Ȼ���� 10^(Q - 1)�������п�����ǰ���� 10^(P - 1)��P��Q ���� 1�����ֻҪ�Ƚ�һ�ξ�����

  // ����ο��ټ���� 10^Q�����ǵ��ö������ݳ˷� BigNumberPower

  if P = Q then  // ������ N�������С�� 10 ����λ����һ�������������Ƚϼ��㣬ֱ�ӷ���
  begin
    Result := P;
    Exit;
  end;

  N := FLocalBigNumberPool.Obtain;
  try
    // �ȼ��� P λ 10 ���Ƶ���Сֵ 10^(P - 1) ���ݣ��ͱ����Ƚϣ�ע������ P �� Q ��һ��
    N.SetWord(10);
    N.PowerWord(Q);

    Result := Q;
    if BignumberUnsignedCompare(Num, N) < 0 then
      Exit;

    Inc(Result); // ������ڻ���� P λ 10 ���Ƶ���Сֵ 10^(P - 1) ���ݣ���λ���� Q �� 1
  finally
    FLocalBigNumberPool.Recycle(N);
  end;
end;

function BigNumberGetTenPrecision2(Num: TCnBigNumber): Integer;
const
  LOG_10_2 = 0.30103;
var
  B: Integer;
begin
  Result := 0;
  if Num.IsZero then
    Exit;

  B := Num.GetBitsCount;
  if B <= 3 then
  begin
    Result := 1;
    Exit;
  end;

  Result := Trunc(LOG_10_2 * B) + 1;
end;

// ȷ�� Num �ڷ�������鳤���� Words �� Cardinal/UInt64
function BigNumberExpandInternal(Num: TCnBigNumber; Words: Integer): PCnBigNumberElement;
var
  A, B, TmpA: PCnBigNumberElement;
  I: Integer;
  A0, A1, A2, A3: TCnBigNumberElement;
begin
  Result := nil;
  if (Words <= 0) or (Words > (MaxInt div (4 * BN_BITS2))) then
    Exit;

  A := PCnBigNumberElement(GetMemory(SizeOf(TCnBigNumberElement) * Words));
  if A = nil then
    Exit;

  FillChar(A^, SizeOf(TCnBigNumberElement) * Words, 0);

  // ����Ƿ�Ҫ����֮ǰ��ֵ
  B := Num.D;
  if B <> nil then
  begin
    TmpA := A;
    I :=  Num.Top shr 2;
    while I > 0 do
    begin
      A0 := PCnBigNumberElementArray(B)^[0];
      A1 := PCnBigNumberElementArray(B)^[1];
      A2 := PCnBigNumberElementArray(B)^[2];
      A3 := PCnBigNumberElementArray(B)^[3];

      PCnBigNumberElementArray(TmpA)^[0] := A0;
      PCnBigNumberElementArray(TmpA)^[1] := A1;
      PCnBigNumberElementArray(TmpA)^[2] := A2;
      PCnBigNumberElementArray(TmpA)^[3] := A3;

      Dec(I);
      TmpA := PCnBigNumberElement(TCnIntAddress(TmpA) + 4 * SizeOf(TCnBigNumberElement));
      B := PCnBigNumberElement(TCnIntAddress(B) + 4 * SizeOf(TCnBigNumberElement));
    end;

    case Num.Top and 3 of
      3:
        begin
          PCnBigNumberElementArray(TmpA)^[2] := PCnBigNumberElementArray(B)^[2];
          PCnBigNumberElementArray(TmpA)^[1] := PCnBigNumberElementArray(B)^[1];
          PCnBigNumberElementArray(TmpA)^[0] := PCnBigNumberElementArray(B)^[0];
        end;
      2:
        begin
          PCnBigNumberElementArray(TmpA)^[1] := PCnBigNumberElementArray(B)^[1];
          PCnBigNumberElementArray(TmpA)^[0] := PCnBigNumberElementArray(B)^[0];
        end;
      1:
        begin
          PCnBigNumberElementArray(TmpA)^[0] := PCnBigNumberElementArray(B)^[0];
        end;
      0:
        begin
          ;
        end;
    end;
  end;

  Result := A;
end;

function BigNumberExpand2(Num: TCnBigNumber; Words: Integer): TCnBigNumber;
var
  P: PCnBigNumberElement;
begin
  Result := nil;
  if Words > Num.DMax then
  begin
    P := BigNumberExpandInternal(Num, Words);
    if P = nil then
      Exit;

    if Num.D <> nil then
      FreeMemory(Num.D);
    Num.D := P;
    Num.DMax := Words;

    Result := Num;
  end;
end;

function BigNumberWordExpand(Num: TCnBigNumber; Words: Integer): TCnBigNumber;
begin
  if Words <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, Words);
end;

function BigNumberExpandBits(Num: TCnBigNumber; Bits: Integer): TCnBigNumber;
begin
  if ((Bits + BN_BITS2 - 1) div BN_BITS2) <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, (Bits + BN_BITS2 - 1) div BN_BITS2);
end;

procedure BigNumberClear(Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  if Num.D <> nil then
    FillChar(Num.D^, Num.DMax * SizeOf(TCnBigNumberElement), 0);
  Num.Top := 0;
  Num.Neg := 0;
end;

// 64 λ��Ҳֻ���� 32 λ��
function BigNumberGetWord(Num: TCnBigNumber): Cardinal;
const
  MAX32 = $FFFFFFFF;
{$IFDEF BN_DATA_USE_64}
var
  T: TCnBigNumberElement;
{$ENDIF}
begin
  if Num.Top > 1 then
    Result := MAX32
  else if Num.Top = 1 then
  begin
{$IFDEF BN_DATA_USE_64}
    T := PCnBigNumberElementArray(Num.D)^[0];
    if T > MAX32 then
      Result := MAX32
    else
      Result := Cardinal(T);
{$ELSE}
    Result := PCnBigNumberElementArray(Num.D)^[0];
{$ENDIF}
  end
  else
    Result := 0;
end;

// 64 λ��Ҳ����
function BigNumberSetWord(Num: TCnBigNumber; W: Cardinal): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Cardinal) * 8) = nil then
    Exit;
  Num.Neg := 0;
  PCnBigNumberElementArray(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 1
  else
    Num.Top := 0;
  Result := True;
end;

function BigNumberGetInteger(Num: TCnBigNumber): Integer;
const
  MAX_INT_32 = $7FFFFFFF;
{$IFDEF BN_DATA_USE_64}
var
  T: TCnBigNumberElement;
{$ENDIF}
begin
  if Num.Top > 1 then
    Result := BN_MASK2S
  else if Num.Top = 1 then
  begin
{$IFDEF BN_DATA_USE_64}
    T := PCnBigNumberElementArray(Num.D)^[0];
    if T > MAX_INT_32 then        // UInt64 ������ Integer �ķ�Χ������ Max Integer
      Result := MAX_INT_32
    else
      Result := Integer(T);

    if Num.Neg <> 0 then // �����󷴼�һ
      Result := (not Result) + 1;
{$ELSE}
    Result := Integer(PCnBigNumberElementArray(Num.D)^[0]);
    if Result < 0 then        // UInt32 ���λ��ֵ��˵���Ѿ������� Integer �ķ�Χ������ Max Integer
      Result := BN_MASK2S
    else if Num.Neg <> 0 then // �����󷴼�һ
      Result := (not Result) + 1;
{$ENDIF}
  end
  else
    Result := 0;
end;

function BigNumberSetInteger(Num: TCnBigNumber; W: Integer): Boolean;
begin
  if W < 0 then
  begin
    BigNumberSetWord(Num, -W);
    Num.Negate;
  end
  else
    BigNumberSetWord(Num ,W);
  Result := True;
end;

function BigNumberGetInt64(Num: TCnBigNumber): Int64;
begin
  if Num.Top > 2 then
    Result := BN_MASK3S
  else if Num.Top = 2 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := BN_MASK3S;
{$ELSE}
    Result := PInt64Array(Num.D)^[0];
    if Result < 0 then        // UInt64 ���λ��ֵ��˵���Ѿ������� Int64 �ķ�Χ������ Max Int64
      Result := BN_MASK3S
    else if Num.Neg <> 0 then // �����󷴼�һ
      Result := (not Result) + 1;
{$ENDIF}
  end
  else if Num.Top = 1 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := Int64(PCnBigNumberElementArray(Num.D)^[0]); // UInt64 תΪ Int64 ����Ǹ��ģ���ʾ������
    if Result < 0 then
      Result := BN_MASK3S;
{$ELSE}
    Result := Int64(PCnBigNumberElementArray(Num.D)^[0]);
{$ENDIF}
  end
  else
    Result := 0;
end;

function BigNumberSetInt64(Num: TCnBigNumber; W: Int64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Int64) * 8) = nil then
    Exit;

  if W >= 0 then
  begin
    Num.Neg := 0;
    PInt64Array(Num.D)^[0] := W;
    if W = 0 then
      Num.Top := 0
    else
    begin
{$IFDEF BN_DATA_USE_64}
      Num.Top := 1;
{$ELSE}
      if ((W and $FFFFFFFF00000000) shr 32) = 0 then // ��� Int64 �� 32 λ�� 0
        Num.Top := 1
      else
        Num.Top := 2;
{$ENDIF}
    end;
  end
  else // W < 0
  begin
    Num.Neg := 1;
    W := (not W) + 1;
    PInt64Array(Num.D)^[0] := W;

{$IFDEF BN_DATA_USE_64}
    Num.Top := 1;
{$ELSE}
    if ((W and $FFFFFFFF00000000) shr 32) = 0 then // ��� Int64 �� 32 λ�� 0
      Num.Top := 1
    else
      Num.Top := 2;
{$ENDIF}
  end;
  Result := True;
end;

function BigNumberGetUInt64UsingInt64(Num: TCnBigNumber): TUInt64;
begin
  if Num.Top > 2 then
    Result := TUInt64(BN_MASK3U)
  else if Num.Top = 2 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := TUInt64(BN_MASK3U);
{$ELSE}
  {$IFDEF SUPPORT_UINT64}
    Result := TUInt64(PInt64Array(Num.D)^[0]);
  {$ELSE}
    Result := PInt64Array(Num.D)^[0]; // �� D5/6 �� Int64ת Int64 ���� C3517 ���󣡣���
  {$ENDIF}
{$ENDIF}
  end
  else if Num.Top = 1 then
    Result := TUInt64(PCnBigNumberElementArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetUInt64UsingInt64(Num: TCnBigNumber; W: TUInt64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Int64) * 8) = nil then
    Exit;

  Num.Neg := 0;
  PInt64Array(Num.D)^[0] := Int64(W);
  if W = 0 then
    Num.Top := 0
  else
  begin
{$IFDEF BN_DATA_USE_64}
    Num.Top := 1;
{$ELSE}
    if ((W and $FFFFFFFF00000000) shr 32) = 0 then // ��� Int64 �� 32 λ�� 0
      Num.Top := 1
    else
      Num.Top := 2;
{$ENDIF}
  end;

  Result := True;
end;

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(Num: TCnBigNumber): UInt64;
begin
  if Num.Top > 2 then
    Result := UInt64(BN_MASK3U)
  else if Num.Top = 2 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := UInt64(BN_MASK3U);
{$ELSE}
    Result := PUInt64Array(Num.D)^[0];
{$ENDIF}
  end
  else if Num.Top = 1 then // ���� 32 ���� 64 ��������ת��
    Result := UInt64(PCnBigNumberElementArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetUInt64(Num: TCnBigNumber; W: UInt64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(UInt64) * 8) = nil then
    Exit;

  Num.Neg := 0;
  PUInt64Array(Num.D)^[0] := W;

  if W = 0 then
    Num.Top := 0
  else
  begin
{$IFDEF BN_DATA_USE_64}
    Num.Top := 1;
{$ELSE}
    if ((W and $FFFFFFFF00000000) shr 32) = 0 then // ��� UInt64 �� 32 λ�� 0
      Num.Top := 1
    else
      Num.Top := 2;
{$ENDIF}
  end;  

  Result := True;
end;

{$ENDIF}

// ���� Top ��֤ D[Top - 1] ָ�����λ�� 0 ��
procedure BigNumberCorrectTop(Num: TCnBigNumber);
var
  Ftl: PCnBigNumberElement;
  Top: Integer;
begin
  Top := Num.Top;
  Ftl := @(PCnBigNumberElementArray(Num.D)^[Top - 1]);
  while Top > 0 do
  begin
    if Ftl^ <> 0 then
      Break;

    Ftl := PCnBigNumberElement(TCnIntAddress(Ftl) - SizeOf(TCnBigNumberElement));
    Dec(Top);
  end;
  Num.Top := Top;
end;

function BigNumberToBinary(Num: TCnBigNumber; Buf: PAnsiChar; FixedLen: Integer): Integer;
var
  I: Integer;
  L: TCnBigNumberElement;
begin
  Result := BigNumberGetBytesCount(Num);
  if Buf = nil then
    Exit;

  if FixedLen > Result then // Ҫ���ߴ���
  begin
    I := FixedLen - Result;
    while I > 0 do
    begin
      Dec(I);
      Buf^ := #0;
      Buf := PAnsiChar(TCnIntAddress(Buf) + 1); // �Ȳ� 0
    end;
  end;

  I := Result;
  while I > 0 do
  begin
    Dec(I);
    L := PCnBigNumberElementArray(Num.D)^[I div BN_BYTES];
    Buf^ := AnsiChar(Chr(L shr (8 * (I mod BN_BYTES)) and $FF));

    Buf := PAnsiChar(TCnIntAddress(Buf) + 1);
  end;
end;

function BigNumberReadBinaryFromStream(Num: TCnBigNumber; Stream: TStream): Boolean;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    M.LoadFromStream(Stream);
    Result := Num.SetBinary(M.Memory, M.Size);
  finally
    M.Free;
  end;
end;

function BigNumberWriteBinaryToStream(Num: TCnBigNumber; Stream: TStream;
  FixedLen: Integer): Integer;
var
  Buf: TBytes;
  Len: Integer;
begin
  Result := 0;
  Len := BigNumberGetBytesCount(Num);
  if (Stream <> nil) and (Len > 0) then
  begin
    if FixedLen > Len then
    begin
      SetLength(Buf, FixedLen);
      BigNumberToBinary(Num, @Buf[FixedLen - Len]);
      Result := Stream.Write(Buf[0], FixedLen);
    end
    else
    begin
      SetLength(Buf, Len);
      BigNumberToBinary(Num, @Buf[0]);
      Result := Stream.Write(Buf[0], Len);
    end;
    SetLength(Buf, 0);
  end;
end;

function BigNumberFromBinary(Buf: PAnsiChar; ByteLen: Integer): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetBinary(Buf, ByteLen, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberFromBytes(Buf: TBytes): TCnBigNumber;
begin
  Result := nil;
  if (Buf <> nil) and (Length(Buf) > 0) then
    Result := BigNumberFromBinary(@Buf[0], Length(Buf));
end;

function BigNumberToBytes(Num: TCnBigNumber): TBytes;
var
  L: Integer;
begin
  Result := nil;
  L := BigNumberGetBytesCount(Num);
  if L > 0 then
  begin
    SetLength(Result, L);
    BigNumberToBinary(Num, @Result[0]);
  end;
end;

function BigNumberSetBinary(Buf: PAnsiChar; ByteLen: Integer; Res: TCnBigNumber): Boolean;
var
  I, M, N, L: TCnBigNumberElement;
begin
  Result := False;
  L := 0;
  N := ByteLen;
  if N = 0 then
  begin
    Res.Top := 0;
    Exit;
  end;

  I := ((N - 1) div BN_BYTES) + 1;
  M := (N - 1) mod BN_BYTES;

  if BigNumberWordExpand(Res, I) = nil then
  begin
    BigNumberFree(Res);
    Exit;
  end;

  Res.Top := I;
  Res.Neg := 0;
  while N > 0 do
  begin
    L := (L shl 8) or Ord(Buf^);
    Buf := PAnsiChar(TCnIntAddress(Buf) + 1);  // Buf ��Խ����Խ���ߵ�ַ��

    if M = 0 then
    begin
      Dec(I);
      PCnBigNumberElementArray(Res.D)^[I] := L; // D �� I ����Խ����Խ���͵�ַ��
      L := 0;
      M := BN_BYTES - 1;
    end
    else
      Dec(M);

    Dec(N);
  end;
  BigNumberCorrectTop(Res);
  Result := True;
end;

{$WARNINGS OFF}

function BigNumberToBase64(Num: TCnBigNumber): string;
var
  B: TBytes;
begin
  Result := '';
  if Num <> nil then
  begin
    B := BigNumberToBytes(Num);
    if B <> nil then
      Base64Encode(@B[0], Length(B), Result);
  end;
end;

{$WARNINGS ON}

function BigNumberSetBase64(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
var
  B: TBytes;
begin
  Result := False;
  if Base64Decode(string(Buf), B) = ECN_BASE64_OK then
    Result := BigNumberSetBinary(@B[0], Length(B), Res);
end;

function BigNumberFromBase64(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetBase64(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

procedure BigNumberSetNegative(Num: TCnBigNumber; Negative: Boolean);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Negative then
    Num.Neg := 1
  else
    Num.Neg := 0;
end;

function BigNumberIsNegative(Num: TCnBigNumber): Boolean;
begin
  Result := Num.Neg <> 0;
end;

procedure BigNumberNegate(Num: TCnBigNumber);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Num.Neg <> 0 then
    Num.Neg := 0
  else
    Num.Neg := 1;
end;

function BigNumberClearBit(Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
    Exit;

  PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] and
    TCnBigNumberElement(not (1 shl J));

  BigNumberCorrectTop(Num);
  Result := True;
end;

// ��һ����������ֻ������ 0 �� Count - 1 �� Bit λ����λ���㣬���سɹ����
function BigNumberKeepLowBits(Num: TCnBigNumber; Count: Integer): Boolean;
var
  I, J: Integer;
  B: TCnBigNumberElement;
begin
  Result := False;
  if Count < 0 then
    Exit;

  if Count = 0 then
  begin
    Num.SetZero;
    Result := True;
    Exit;
  end;

  I := Count div BN_BITS2;
  J := Count mod BN_BITS2;

  if Num.Top <= I then
  begin
    Result := True;
    Exit;
  end;

  if J > 0 then // Ҫ�ౣ�����һ�� LongWord �е� 0 �� J - 1 λ���� J λ��J ��� 31/63
  begin
    Num.Top := I + 1;
    B := 1 shl J;         // 0000100000 ��� J �� 31/63 Ҳ�������
    B := B - 1;           // 0000011111
    PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] and B;
  end
  else
    Num.Top := I; // ��� J Ϊ 0����������һ�� LongWord ��

  BigNumberCorrectTop(Num);
  Result := True;
end;

function BigNumberSetBit(Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J, K: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
  begin
    if BigNumberWordExpand(Num, I + 1) = nil then
      Exit;

    for K := Num.Top to I do
      PCnBigNumberElementArray(Num.D)^[K] := 0;

    Num.Top := I + 1;
  end;

  PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] or
    TCnBigNumberElement(1 shl J);
  Result := True;
end;

function BigNumberIsBitSet(Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
    Exit;

  if (TCnBigNumberElement(PCnBigNumberElementArray(Num.D)^[I] shr J) and TCnBigNumberElement(1)) <> 0 then
    Result := True;
end;

function BigNumberEqual(Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
begin
  Result := BigNumberCompare(Num1, Num2) = 0;
end;

function BigNumberEqual(Num1: TCnBigNumber; Num2: Int64): Boolean;
begin
  Result := Num1.IsInt64 and (Num1.GetInt64 = Num2);
end;

function BigNumberCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
var
  I, Gt, Lt: Integer;
  T1, T2: TCnBigNumberElement;
begin
  if Num1 = Num2 then
  begin
    Result := 0;
    Exit;
  end;

  if (Num1 = nil) or (Num2 = nil) then
  begin
    if Num1 <> nil then
      Result := -1
    else if Num2 <> nil then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  if Num1.Neg <> Num2.Neg then
  begin
    if Num1.Neg <> 0 then
      Result := -1
    else
      Result := 1;
    Exit;
  end;

  if Num1.Neg = 0 then
  begin
    Gt := 1;
    Lt := -1;
  end
  else
  begin
    Gt := -1;
    Lt := 1;
  end;

  if Num1.Top > Num2.Top then
  begin
    Result := Gt;
    Exit;
  end
  else if Num1.Top < Num2.Top then
  begin
    Result := Lt;
    Exit;
  end;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PCnBigNumberElementArray(Num1.D)^[I];
    T2 := PCnBigNumberElementArray(Num2.D)^[I];
    if T1 > T2 then
    begin
      Result := Gt;
      Exit;
    end;
    if T1 < T2 then
    begin
      Result := Lt;
      Exit;
    end;
  end;
  Result := 0;
end;

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Integer): Integer;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInteger(Num2);
    Result := BigNumberCompare(Num1, T);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Int64): Integer;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInt64(Num2);
    Result := BigNumberCompare(Num1, T);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberUnsignedCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
var
  I: Integer;
  T1, T2: TCnBigNumberElement;
begin
  Result := Num1.Top - Num2.Top;
  if Result <> 0 then
    Exit;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PCnBigNumberElementArray(Num1.D)^[I];
    T2 := PCnBigNumberElementArray(Num2.D)^[I];
    if T1 > T2 then
    begin
      Result := 1;
      Exit;
    end;
    if T1 < T2 then
    begin
      Result := -1;
      Exit;
    end;
  end;
  Result := 0;
end;

// �����̶��ֽڳ��ȵ��������
function BigNumberRandBytes(Num: TCnBigNumber; BytesCount: Integer): Boolean;
begin
  Result := False;
  if BytesCount < 0 then
    Exit;
  if BytesCount = 0 then
  begin
    Result := BigNumberSetZero(Num);
    Exit;
  end;

  if BigNumberWordExpand(Num, (BytesCount + BN_BYTES - 1) div BN_BYTES) <> nil then
  begin
    // ���÷��ظ���ʼ���Ŀ��ٰ汾����֪�����޸����ã�
    Result := CnRandomFillBytes2(PAnsiChar(Num.D), BytesCount);
    if Result then
    begin
      Num.Top := (BytesCount + BN_BYTES - 1) div BN_BYTES;
      BigNumberCorrectTop(Num);
    end;
  end;
end;

// �����̶�λ���ȵ��������
function BigNumberRandBits(Num: TCnBigNumber; BitsCount: Integer): Boolean;
var
  C, I: Integer;
begin
  Result := False;
  if BitsCount < 0 then
    Exit;
  if BitsCount = 0 then
  begin
    Result := BigNumberSetZero(Num);
    Exit;
  end;

  // Ҫ���� N bits ������������ֽڼ���Ҳ���� (N + 7) div 8 bytes
  C := (BitsCount + 7) div 8;
  if not BigNumberRandBytes(Num, C) then
    Exit;

  // ��ͷ�Ͽ����ж���ģ��ٰ� C * 8 - 1 �� N ֮���λ���㣬ֻ�� 0 �� N - 1 λ
  if BitsCount <= C * 8 - 1 then
  begin
    for I := C * 8 - 1 downto BitsCount do
    begin
      if not BigNumberClearBit(Num, I) then
        Exit;
    end;
  end;

  Result := True;
end;

function BigNumberRandRange(Num: TCnBigNumber; Range: TCnBigNumber): Boolean;
var
  N, C, I: Integer;
begin
  Result := False;
  if (Range = nil) or (Num = nil) or (Range.Neg <> 0) or BigNumberIsZero(Range) then
    Exit;

  N := BigNumberGetBitsCount(Range);
  if N = 1 then
    BigNumberSetZero(Num)
  else
  begin
    // Ҫ���� N bits ������������ֽڼ���Ҳ���� (N + 7) div 8 bytes
    C := (N + 7) div 8;
    if not BigNumberRandBytes(Num, C) then
      Exit;

    // ��ͷ�Ͽ����ж���ģ��ٰ� C * 8 - 1 �� N + 1 ֮���λ����
    if N + 1 <= C * 8 - 1 then
      for I := C * 8 - 1 downto N + 1 do
        if BigNumberIsBitSet(Num, I) then
          if not BigNumberClearBit(Num, I) then
            Exit;
    // �Ӹ� IsBitSet ���жϣ���Ϊ ClearBit ���жϴ� Clear ��λ�Ƿ񳬳� Top��
    // ������ɵ�λ�������� 0�����Ѿ��� CorrectTop�ˣ���ô ClearBit �����

    while BigNumberCompare(Num, Range) >= 0 do
    begin
      if not BigNumberSub(Num, Num, Range) then
        Exit;
    end;
  end;
  Result := True;
end;

function BigNumberDuplicate(Num: TCnBigNumber): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if BigNumberCopy(Result, Num) = nil then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberCopy(Dst: TCnBigNumber; Src: TCnBigNumber): TCnBigNumber;
var
  I: Integer;
  A, B: PCnBigNumberElementArray;
  A0, A1, A2, A3: TCnBigNumberElement;
begin
  if Dst = Src then
  begin
    Result := Dst;
    Exit;
  end;

  if BigNumberWordExpand(Dst, Src.Top) = nil then
  begin
    Result := nil;
    Exit;
  end;

  A := PCnBigNumberElementArray(Dst.D);
  B := PCnBigNumberElementArray(Src.D);

  for I := (Src.Top shr 2) downto 1 do
  begin
    A0 := B^[0];
    A1 := B^[1];
    A2 := B^[2];
    A3 := B^[3];
    A^[0] := A0;
    A^[1] := A1;
    A^[2] := A2;
    A^[3] := A3;

    A := PCnBigNumberElementArray(TCnIntAddress(A) + 4 * SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElementArray(TCnIntAddress(B) + 4 * SizeOf(TCnBigNumberElement));
  end;

  case Src.Top and 3 of
  3:
    begin
      A[2] := B[2];
      A[1] := B[1];
      A[0] := B[0];
    end;
  2:
    begin
      A[1] := B[1];
      A[0] := B[0];
    end;
  1:
    begin
      A[0] := B[0];
    end;
  0:
    begin

    end;
  end;

  Dst.Top := Src.Top;
  Dst.Neg := Src.Neg;
  Result := Dst;
end;

function BigNumberCopyLow(Dst: TCnBigNumber; Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
var
  I: Integer;
  A, B: PCnBigNumberElementArray;
begin
  if WordCount <= 0 then
  begin
    Result := Dst;
    Dst.SetZero;
    Exit;
  end
  else if Src = Dst then // ��֧�� Src �� Dst ��ͬ�����
    Result := nil
  else
  begin
    if WordCount > Src.GetWordCount then
      WordCount := Src.GetWordCount;

    if BigNumberWordExpand(Dst, WordCount) = nil then
    begin
      Result := nil;
      Exit;
    end;

    A := PCnBigNumberElementArray(Dst.D);
    B := PCnBigNumberElementArray(Src.D);

    Result := Dst;
    for I := 0 to WordCount - 1 do // �� Src �� 0 �� WordCount - 1 ��ֵ�� Dst �� 0 �� WordCount - 1
      A^[I] := B^[I];

    Dst.Top := WordCount;
    Dst.Neg := Src.Neg;
  end;
end;

function BigNumberCopyHigh(Dst: TCnBigNumber; Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
var
  I: Integer;
  A, B: PCnBigNumberElementArray;
begin
  if WordCount <= 0 then
  begin
    Result := Dst;
    Dst.SetZero;
    Exit;
  end
  else if Src = Dst then // ��֧�� Src �� Dst ��ͬ�����
    Result := nil
  else
  begin
    if WordCount > Src.GetWordCount then
      WordCount := Src.GetWordCount;

    if BigNumberWordExpand(Dst, WordCount) = nil then
    begin
      Result := nil;
      Exit;
    end;

    A := PCnBigNumberElementArray(Dst.D);
    B := PCnBigNumberElementArray(Src.D);

    Result := Dst;
    for I := 0 to WordCount - 1 do // �� Src �� Top - WordCount �� Top - 1 ��ֵ�� Dst �� 0 �� WordCount - 1
      A^[I] := B^[Src.Top - WordCount + I];

    Dst.Top := WordCount;
    Dst.Neg := Src.Neg;
  end;
end;

function BigNumberGetLow32(Num: TCnBigNumber): Cardinal;
begin
  Result := 0;
  if Num.DMax > 0 then
    Result := Cardinal(Num.D^);
end;

function BigNumberGetLow64(Num: TCnBigNumber): TUInt64;
begin
  Result := 0;
{$IFDEF BN_DATA_USE_64}
  if Num.DMax > 0 then
    Result := TUInt64(Num.D^);
{$ELSE}
  if Num.DMax = 1 then
    Result := TUInt64(Num.D^)
  else if Num.DMax >= 2 then
    Result := TUInt64(PCnBigNumberElementArray(Num.D)^[0]) + (TUInt64(PCnBigNumberElementArray(Num.D)^[1]) shl 32);
{$ENDIF}
end;

procedure BigNumberSwap(Num1: TCnBigNumber; Num2: TCnBigNumber);
var
  TmpD: PCnBigNumberElement;
  TmpTop, TmpDMax, TmpNeg: Integer;
begin
  TmpD := Num1.D;
  TmpTop := Num1.Top;
  TmpDMax := Num1.DMax;
  TmpNeg := Num1.Neg;

  Num1.D := Num2.D;
  Num1.Top := Num2.Top;
  Num1.DMax := Num2.DMax;
  Num1.Neg := Num2.Neg;

  Num2.D := TmpD;
  Num2.Top := TmpTop;
  Num2.DMax := TmpDMax;
  Num2.Neg := TmpNeg;
end;

procedure BigNumberSwapBit(Num: TCnBigNumber; BitIndex1, BitIndex2: Integer);
var
  B1, B2: Boolean;
begin
  if (BitIndex1 = BitIndex2) or (BitIndex1 < 0) or (BitIndex2 < 0) then
    Exit;

  if (BitIndex1 >= Num.GetBitsCount) or (BitIndex2 >= Num.GetBitsCount) then
    Exit;

  B1 := Num.IsBitSet(BitIndex1);
  B2 := Num.IsBitSet(BitIndex2);

  if B2 then
    Num.SetBit(BitIndex1)
  else
    Num.ClearBit(BitIndex1);

  if B1 then
    Num.SetBit(BitIndex2)
  else
    Num.ClearBit(BitIndex2);
end;

// ============================ �ͽ����㶨�忪ʼ ===============================

{$IFDEF BN_DATA_USE_64}

// �����޷��� 64 λ N ��ƽ����ֵ�ߵ�λ���� H �� L
procedure Sqr(var L: UInt64; var H: UInt64; N: UInt64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  UInt64MulUInt64(N, N, L, H);
end;

// ���� UInt64 �� A * B + R + C
procedure MulAdd(var R: UInt64; A: UInt64; B: UInt64; var C: UInt64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TCnUInt128;
begin
  UInt64MulUInt64(A, B, T.Lo64, T.Hi64); // ���� A * B
  UInt128Add(T, T, R);                   // ���� R
  UInt128Add(T, T, C);                   // ���� C
  R := T.Lo64;
  C := T.Hi64;
end;

// ���� UInt64 �� A * B + C
procedure Mul(var R: UInt64; A: UInt64; B: UInt64; var C: UInt64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TCnUInt128;
begin
  UInt64MulUInt64(A, B, T.Lo64, T.Hi64); // ���� A * B
  UInt128Add(T, T, C);                   // ���� C
  R := T.Lo64;
  C := T.Hi64;
end;

{$ELSE}

// UInt64 �ķ�ʽ���� N ƽ��
procedure Sqr(var L: Cardinal; var H: Cardinal; N: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(N, N);
  // �޷��� 32 λ�������ֱ����ˣ��õ��� Int64 ��������õ���ֵ���÷�װ��������档
  L := Cardinal(T) and BN_MASK2;
  H := Cardinal(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 �ķ�ʽ���� A * B + R + C
procedure MulAdd(var R: Cardinal; A: Cardinal; B: Cardinal; var C: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + R + C;
  // �޷��� 32 λ�������ֱ����ˣ��õ��� Int64 ��������õ���ֵ���÷�װ��������档
  R := Cardinal(T) and BN_MASK2;
  C := Cardinal(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 �ķ�ʽ���� A * B + C
procedure Mul(var R: Cardinal; A: Cardinal; B: Cardinal; var C: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + C;
  // �޷��� 32 λ�������ֱ����ˣ��õ��� Int64 ��������õ���ֵ���÷�װ��������档
  R := Cardinal(T) and BN_MASK2;
  C := Cardinal(T shr BN_BITS2) and BN_MASK2;
end;

{$ENDIF}

// N �� Cardinal �����������ݽ���λ���㣬��� BP Ϊ nil����ʾ����������Ϊ 0 ����
procedure BigNumberBitOperation(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer; Op: TCnBitOperation);
begin
  if N <= 0 then
    Exit;

  if BP <> nil then
  begin
    while (N and (not 3)) <> 0 do
    begin
      case Op of
        boAnd:
          begin
            RP^[0] := TCnBigNumberElement((Int64(AP^[0]) and Int64(BP^[0])) and BN_MASK2);
            RP^[1] := TCnBigNumberElement((Int64(AP^[1]) and Int64(BP^[1])) and BN_MASK2);
            RP^[2] := TCnBigNumberElement((Int64(AP^[2]) and Int64(BP^[2])) and BN_MASK2);
            RP^[3] := TCnBigNumberElement((Int64(AP^[3]) and Int64(BP^[3])) and BN_MASK2);
          end;
        boOr:
          begin
            RP^[0] := TCnBigNumberElement((Int64(AP^[0]) or Int64(BP^[0])) and BN_MASK2);
            RP^[1] := TCnBigNumberElement((Int64(AP^[1]) or Int64(BP^[1])) and BN_MASK2);
            RP^[2] := TCnBigNumberElement((Int64(AP^[2]) or Int64(BP^[2])) and BN_MASK2);
            RP^[3] := TCnBigNumberElement((Int64(AP^[3]) or Int64(BP^[3])) and BN_MASK2);
          end;
        boXor:
          begin
            RP^[0] := TCnBigNumberElement((Int64(AP^[0]) xor Int64(BP^[0])) and BN_MASK2);
            RP^[1] := TCnBigNumberElement((Int64(AP^[1]) xor Int64(BP^[1])) and BN_MASK2);
            RP^[2] := TCnBigNumberElement((Int64(AP^[2]) xor Int64(BP^[2])) and BN_MASK2);
            RP^[3] := TCnBigNumberElement((Int64(AP^[3]) xor Int64(BP^[3])) and BN_MASK2);
          end;
      end;

      AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
      BP := PCnBigNumberElementArray(TCnIntAddress(BP) + 4 * SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

      Dec(N, 4);
    end;

    while N <> 0 do
    begin
      case Op of
        boAnd:
          RP^[0] := TCnBigNumberElement((Int64(AP^[0]) and Int64(BP^[0])) and BN_MASK2);
        boOr:
          RP^[0] := TCnBigNumberElement((Int64(AP^[0]) or Int64(BP^[0])) and BN_MASK2);
        boXor:
          RP^[0] := TCnBigNumberElement((Int64(AP^[0]) xor Int64(BP^[0])) and BN_MASK2);
      end;

      AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      BP := PCnBigNumberElementArray(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
      Dec(N);
    end;
  end
  else // BP Ϊ nil���������鲻���������� 0 ����
  begin
    if Op = boAnd then
      FillChar(RP[0], N * SizeOf(TCnBigNumberElement), 0)
    else if Op in [boOr, boXor] then
      Move(AP[0], RP[0], N * SizeOf(TCnBigNumberElement));
  end;
end;

// ============================ �ͽ����㶨����� ===============================

{* Words ϵ���ڲ����㺯����ʼ}

procedure BigNumberAndWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boAnd);
end;

procedure BigNumberOrWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boOr);
end;

procedure BigNumberXorWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray; BP: PCnBigNumberElementArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boXor);
end;

function BigNumberAddWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer): Cardinal;
var
{$IFDEF BN_DATA_USE_64}
  LL: TCnUInt128;
{$ELSE}
  LL: Int64;
{$ENDIF}
begin
  Result := 0;
  if N <= 0 then
    Exit;

{$IFDEF BN_DATA_USE_64}
  UInt128SetZero(LL);
{$ELSE}
  LL := 0;
{$ENDIF}

  while (N and (not 3)) <> 0 do
  begin
{$IFDEF BN_DATA_USE_64}
    UInt128Add(LL, LL, AP^[0]);
    UInt128Add(LL, LL, BP^[0]);
    RP^[0] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;

    UInt128Add(LL, LL, AP^[1]);
    UInt128Add(LL, LL, BP^[1]);
    RP^[1] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;

    UInt128Add(LL, LL, AP^[2]);
    UInt128Add(LL, LL, BP^[2]);
    RP^[2] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;

    UInt128Add(LL, LL, AP^[3]);
    UInt128Add(LL, LL, BP^[3]);
    RP^[3] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;
{$ELSE}
    LL := LL + Int64(AP^[0]) + Int64(BP^[0]);
    RP^[0] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP^[1]) + Int64(BP^[1]);
    RP^[1] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP^[2]) + Int64(BP^[2]);
    RP^[2] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP^[3]) + Int64(BP^[3]);
    RP^[3] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;
{$ENDIF}

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
{$IFDEF BN_DATA_USE_64}
    UInt128Add(LL, LL, AP^[0]);
    UInt128Add(LL, LL, BP^[0]);
    RP^[0] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;
{$ELSE}
    LL := LL + Int64(AP^[0]) + Int64(BP^[0]);
    RP^[0] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;
{$ENDIF}

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    Dec(N);
  end;

{$IFDEF BN_DATA_USE_64}
  Result := LL.Lo64;
{$ELSE}
  Result := Cardinal(LL);
{$ENDIF}
end;

function BigNumberSubWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray; BP: PCnBigNumberElementArray; N: Integer): Cardinal;
var
  T1, T2, C: TCnBigNumberElement;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  C := 0;
  while (N and (not 3)) <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[1];
    T2 := BP^[1];
    RP^[1] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[2];
    T2 := BP^[2];
    RP^[2] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[3];
    T2 := BP^[3];
    RP^[3] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    Dec(N);
  end;
  Result := C;
end;

function BigNumberMulAddWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  N: Integer; W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    MulAdd(RP^[1], AP^[1], W, Result);
    MulAdd(RP^[2], AP^[2], W, Result);
    MulAdd(RP^[3], AP^[3], W, Result);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    Dec(N);
  end;
end;

// AP ָ��� N �����ֶ����� W������ĵ� N λ�� RP �У���λ�ŷ���ֵ
function BigNumberMulWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  N: Integer; W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);
    Mul(RP^[1], AP^[1], W, Result);
    Mul(RP^[2], AP^[2], W, Result);
    Mul(RP^[3], AP^[3], W, Result);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

    Dec(N);
  end;
end;

procedure BigNumberSqrWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray; N: Integer);
begin
  if N = 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    Sqr(RP^[2], RP^[3], AP^[1]);
    Sqr(RP^[4], RP^[5], AP^[2]);
    Sqr(RP^[6], RP^[7], AP^[3]);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 8 * SizeOf(TCnBigNumberElement));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 2 * SizeOf(TCnBigNumberElement));
    Dec(N);
  end;
end;

{$IFDEF BN_DATA_USE_64}

// 128 λ���������� 64 λ�����������̣�Result := H L div D�������̵ĸ� 64 λ
// ���Ҫ��֤ D �����λΪ 1���̵ĸ� 64 λ�Ż�Ϊ 0���˺������òŲ������
function InternalDivWords64(H: UInt64; L: UInt64; D: UInt64): UInt64;
var
  R: UInt64;
begin
  UInt128DivUInt64Mod(L, H, D, Result, R);
end;

{$ENDIF}

// 64 λ���������� 32 λ�����������̣�Result := H L div D�������̵ĸ� 32 λ
// ���Ҫ��֤ D �����λΪ 1���̵ĸ� 32 λ�Ż�Ϊ 0���˺������òŲ���������� 32 λ�²ſ����� DIV ָ���Ż�
function InternalDivWords(H: Cardinal; L: Cardinal; D: Cardinal): Cardinal;
begin
  if D = 0 then
  begin
    Result := BN_MASK2;
    Exit;
  end;

{$IFDEF SUPPORT_UINT64}
  Result := Cardinal(((UInt64(H) shl 32) or UInt64(L)) div UInt64(D));
{$ELSE}
  Result := 0;
  asm
    MOV EAX, L
    MOV EDX, H
    DIV ECX       // DIV ò�Ƶ��� DIVL������Ż�������� _lludiv �ĺ�ʱ���� 20%
    MOV Result, EAX
  end;
{$ENDIF}
end;

{* Words ϵ���ڲ����㺯������}

function BigNumberAnd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  BigNumberAndWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  // AP ���ĺ�ͷ���� Dif һ��û�д�����Ҫ���ɺ� 0 һ������
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberAndWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberOr(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  BigNumberOrWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  // AP ���ĺ�ͷ���� Dif һ��û�д�����Ҫ���ɺ� 0 һ������
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberOrWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberXor(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  BigNumberXorWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  // AP ���ĺ�ͷ���� Dif һ��û�д�����Ҫ���ɺ� 0 һ������
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberXorWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberUnsignedAdd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  Carry, T1, T2: TCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max + 1) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  Carry := BigNumberAddWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  AP := PCnBigNumberElement(TCnIntAddress(AP) + Min * SizeOf(TCnBigNumberElement));
  RP := PCnBigNumberElement(TCnIntAddress(RP) + Min * SizeOf(TCnBigNumberElement));

  if Carry <> 0 then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      T2 := (T1 + 1) and BN_MASK2;

      RP^ := T2;
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if T2 <> 0 then
      begin
        Carry := 0;
        Break;
      end;
    end;

    if Carry <> 0 then
    begin
      RP^ := 1;
      Inc(Res.Top);
    end;
  end;

  if (Dif <> 0) and (RP <> AP) then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    end;
  end;

  Res.Neg := 0;
  Result := True;
end;

function BigNumberUnsignedSub(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif, I: Integer;
  AP, BP, RP: PCnBigNumberElement;
  Carry, T1, T2: TCnBigNumberElement;
begin
  Result := False;

  Max := Num1.Top;
  Min := Num2.Top;
  Dif := Max - Min;

  if Dif < 0 then
    Exit;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  AP := PCnBigNumberElement(Num1.D);
  BP := PCnBigNumberElement(Num2.D);
  RP := PCnBigNumberElement(Res.D);

  Carry := 0;
  for I := Min downto 1 do
  begin
    T1 := AP^;
    T2 := BP^;
    AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElement(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
    if Carry <> 0 then
    begin
      if T1 <= T2 then
        Carry := 1
      else
        Carry := 0;
      T1 := (T1 - T2 - 1) and BN_MASK2;
    end
    else
    begin
      if T1 < T2 then
        Carry := 1
      else
        Carry := 0;
      T1 := (T1 - T2) and BN_MASK2;
    end;
    RP^ := T1 and BN_MASK2;
    RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
  end;

  if Carry <> 0 then
  begin
    if Dif = 0 then  // Error! Num1 < Num2
      Exit;

    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      T2 := (T1 - 1) and BN_MASK2;

      RP^ := T2;
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
      if T1 <> 0 then
        Break;
    end;
  end;

  if RP <> AP then
  begin
    while True do
    begin
      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    end;
  end;

  Res.Top := Max;
  Res.Neg := 0;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberAdd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: TCnBigNumber;
  Neg: Integer;
begin
  Result := False;

  Neg := Num1.Neg;
  A := Num1;
  B := Num2;

  if Neg <> Num2.Neg then // One is negative
  begin
    if Neg <> 0 then
    begin
      Tmp := A;
      A := B;
      B := Tmp;
    end;

    // A is positive and B is negative
    if BigNumberUnsignedCompare(A, B) < 0 then
    begin
      if not BigNumberUnsignedSub(Res, B, A) then
        Exit;
      Res.Neg := 1;
    end
    else
    begin
      if not BigNumberUnsignedSub(Res, A, B) then
        Exit;
      Res.Neg := 0;
    end;
    Result := True;
    Exit;
  end;

  Result := BigNumberUnsignedAdd(Res, A, B);
  Res.Neg := Neg;
end;

function BigNumberSub(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: TCnBigNumber;
  Max, Add, Neg: Integer;
begin
  Result := False;
  Add := 0;
  Neg := 0;
  A := Num1;
  B := Num2;

  if A.Neg <> 0 then
  begin
    if B.Neg <> 0 then
    begin
      Tmp := A;
      A := B;
      B := Tmp;
    end
    else // A Negative B Positive
    begin
      Add := 1;
      Neg := 1;
    end;
  end
  else
  begin
    if B.Neg <> 0 then // A Positive B Negative
    begin
      Add := 1;
      Neg := 0;
    end;
  end;

  if Add = 1 then
  begin
    if not BigNumberUnsignedAdd(Res, A, B) then
      Exit;

    Res.Neg := Neg;
    Result := True;
    Exit;
  end;

  if A.Top > B.Top then
    Max := A.Top
  else
    Max := B.Top;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  if BigNumberUnsignedCompare(A, B) < 0 then
  begin
    if not BigNumberUnsignedSub(Res, B, A) then
      Exit;
    Res.Neg := 1;
  end
  else
  begin
    if not BigNumberUnsignedSub(Res, A, B) then
      Exit;
    Res.Neg := 0;
  end;
  Result := True;
end;

function BigNumberShiftLeftOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  RP, AP: PCnBigNumberElement;
  I: Integer;
  T, C: TCnBigNumberElement;
begin
  Result := False;

  if Res <> Num then
  begin
    Res.Neg := Num.Neg;
    if BigNumberWordExpand(Res, Num.Top + 1) = nil then
      Exit;

    Res.Top := Num.Top;
  end
  else
  begin
    if BigNumberWordExpand(Res, Num.Top + 1) = nil then
      Exit;
  end;

  AP := Num.D;
  RP := Res.D;
  C := 0;
  for I := 0 to Num.Top - 1 do
  begin
    T := AP^;
    AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP^ := ((T shl 1) or C) and BN_MASK2;
    RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

    if (T and BN_TBIT) <> 0 then
      C := 1
    else
      C := 0;
  end;

  if C <> 0 then
  begin
    RP^ := 1;
    Inc(Res.Top);
  end;
  Result := True;
end;

function BigNumberShiftRightOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  RP, AP: PCnBigNumberElement;
  I, J: Integer;
  T, C: TCnBigNumberElement;
begin
  Result := False;
  if BigNumberIsZero(Num) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  I := Num.Top;
  AP := Num.D;

  if PCnBigNumberElementArray(AP)^[I - 1] = 1 then
    J := I - 1
  else
    J := I;

  if Res <> Num then
  begin
    if BigNumberWordExpand(Res, J) = nil then
      Exit;
    Res.Neg := Num.Neg;
  end;

  RP := Res.D;
  Dec(I);
  T := PCnBigNumberElementArray(AP)^[I];

  if (T and 1) <> 0 then
    C := BN_TBIT
  else
    C := 0;

  T := T shr 1;
  if T <> 0 then
    PCnBigNumberElementArray(RP)^[I] := T;

  while I > 0 do
  begin
    Dec(I);
    T := PCnBigNumberElementArray(AP)^[I];
    PCnBigNumberElementArray(RP)^[I] := ((T shr 1) and BN_MASK2) or C;

    if (T and 1) <> 0 then
      C := BN_TBIT
    else
      C := 0;
  end;

  Res.Top := J;
  Result := True;
end;

function BigNumberShiftLeft(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, NW, LB, RB: Integer;
  L: TCnBigNumberElement;
  T, F: PCnBigNumberElementArray;
begin
  if N < 0 then
  begin
    Result := BigNumberShiftRight(Res, Num, -N);
    Exit;
  end;

  Result := False;
  Res.Neg := Num.Neg;
  NW := N div BN_BITS2;

  if BigNumberWordExpand(Res, Num.Top + NW + 1) = nil then
    Exit;

  LB := N mod BN_BITS2;
  RB := BN_BITS2 - LB;

  F := PCnBigNumberElementArray(Num.D);
  T := PCnBigNumberElementArray(Res.D);

  T^[Num.Top + NW] := 0;
  if LB = 0 then
  begin
    for I := Num.Top - 1 downto 0 do
      T^[NW + I] := F^[I];
  end
  else
  begin
    for I := Num.Top - 1 downto 0 do
    begin
      L := F^[I];
      T^[NW + I + 1] := T^[NW + I + 1] or ((L shr RB) and BN_MASK2);
      T^[NW + I] := (L shl LB) and BN_MASK2;
    end;
  end;

  FillChar(Pointer(T)^, NW * SizeOf(TCnBigNumberElement), 0);
  Res.Top := Num.Top + NW + 1;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberShiftRight(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, J, NW, LB, RB: Integer;
  L, Tmp: TCnBigNumberElement;
  T, F: PCnBigNumberElementArray;
begin
  if N < 0 then
  begin
    Result := BigNumberShiftLeft(Res, Num, -N);
    Exit;
  end;

  Result := False;
  NW := N div BN_BITS2;
  RB := N mod BN_BITS2;
  LB := BN_BITS2 - RB;

  if (NW >= Num.Top) or (Num.Top = 0) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  I := (BigNumberGetBitsCount(Num) - N + (BN_BITS2 - 1)) div BN_BITS2;
  if Res <> Num then
  begin
    Res.Neg := Num.Neg;
    if BigNumberWordExpand(Res, I) = nil then
      Exit;
  end
  else
  begin
    if N = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;

  F := PCnBigNumberElementArray(TCnIntAddress(Num.D) + NW * SizeOf(TCnBigNumberElement));
  T := PCnBigNumberElementArray(Res.D);
  J := Num.Top - NW;
  Res.Top := I;

  if RB = 0 then
  begin
    for I := J downto 1 do
    begin
      T^[0] := F^[0];
      F := PCnBigNumberElementArray(TCnIntAddress(F) + SizeOf(TCnBigNumberElement));
      T := PCnBigNumberElementArray(TCnIntAddress(T) + SizeOf(TCnBigNumberElement));
    end;
  end
  else
  begin
    L := F^[0];
    F := PCnBigNumberElementArray(TCnIntAddress(F) + SizeOf(TCnBigNumberElement));
    for I := J - 1 downto 1 do
    begin
      Tmp := (L shr RB) and BN_MASK2;
      L := F^[0];
      T^[0] := (Tmp or (L shl LB)) and BN_MASK2;

      F := PCnBigNumberElementArray(TCnIntAddress(F) + SizeOf(TCnBigNumberElement));
      T := PCnBigNumberElementArray(TCnIntAddress(T) + SizeOf(TCnBigNumberElement));
    end;

    L := (L shr RB) and BN_MASK2;
    if L <> 0 then
      T^[0] := L;
  end;
  Result := True;
end;

{* ������ Word ����ϵ�к�����ʼ}

// ĳ�����Ƿ����ָ�� UInt32/UInt64
function BigNumberIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
begin
  Result := False;
  if (W = 0) or (Num.Neg = 0) then
    if BigNumberAbsIsWord(Num, W) then
      Result := True;
end;

// ����һ�������ṹ��Ĵ����ľ���ֵ�Ƿ�Ϊָ���� UInt32/UInt64 ֵ
function BigNumberAbsIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
begin
  Result := True;
  if (W = 0) and (Num.Top = 0) then
    Exit;
  if (Num.Top = 1) and (PCnBigNumberElementArray(Num.D)^[0] = W) then // UInt64 �� Cardinal ������
    Exit;
  Result := False;
end;

function BigNumberAddWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
var
  I: Integer;
  L: TCnBigNumberElement;
begin
  Result := False;

  if W = 0 then
  begin
    Result := True;
    Exit;
  end;

  if BigNumberIsZero(Num) then
  begin
    Result := BigNumberSetWord(Num, W);
    Exit;
  end;

  if Num.Neg <> 0 then // �����ü���
  begin
    Num.Neg := 0;
    Result := BigNumberSubWord(Num, W);
    if not BigNumberIsZero(Num) then
      Num.Neg := 1 - Num.Neg;
    Exit;
  end;

  I := 0;
  while (W <> 0) and (I < Num.Top) do
  begin
    L := (PCnBigNumberElementArray(Num.D)^[I] + W) and BN_MASK2;
    PCnBigNumberElementArray(Num.D)^[I] := L;
    if W > L then // ����ȼ���С��˵��������߽�λ�ˣ��ѽ�λ�ø� W��������
      W := 1
    else
      W := 0;
    Inc(I);
  end;

  if (W <> 0) and (I = Num.Top) then // �����λ��Ȼ���������λ
  begin
    if BigNumberWordExpand(Num, Num.Top + 1) = nil then
      Exit;
    Inc(Num.Top);
    PCnBigNumberElementArray(Num.D)^[I] := W;
  end;
  Result := True;
end;

function BigNumberSubWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
var
  I: Integer;
begin
  if W = 0 then
  begin
    Result := True;
    Exit;
  end;

  if BigNumberIsZero(Num) then
  begin
    Result := BigNumberSetWord(Num, W);
    if Result then
      BigNumberSetNegative(Num, True);
    Exit;
  end;

  if Num.Neg <> 0 then
  begin
    Num.Neg := 0;
    Result := BigNumberAddWord(Num, W);
    Num.Neg := 1;
    Exit;
  end;

  if (Num.Top = 1) and (PCnBigNumberElementArray(Num.D)^[0] < W) then // ������
  begin
    PCnBigNumberElementArray(Num.D)^[0] := W - PCnBigNumberElementArray(Num.D)^[0];
    Num.Neg := 1;
    Result := True;
    Exit;
  end;

  I := 0;
  while True do
  begin
    if PCnBigNumberElementArray(Num.D)^[I] >= W then // ����ֱ�Ӽ�
    begin
      PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] - W;
      Break;
    end
    else
    begin
      PCnBigNumberElementArray(Num.D)^[I] := (PCnBigNumberElementArray(Num.D)^[I] - W) and BN_MASK2;
      Inc(I);
      W := 1;  // �������н�λ
    end;
  end;

  if (PCnBigNumberElementArray(Num.D)^[I] = 0) and (I = Num.Top - 1) then
    Dec(Num.Top);
  Result := True;
end;

function BigNumberMulWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
var
  L: TCnBigNumberElement;
begin
  Result := False;

  if Num.Top <> 0 then
  begin
    if W = 0 then
      BigNumberSetZero(Num)
    else
    begin
      L := BigNumberMulWords(PCnBigNumberElementArray(Num.D), PCnBigNumberElementArray(Num.D), Num.Top, W);
      if L <> 0 then
      begin
        if BigNumberWordExpand(Num, Num.Top + 1) = nil then
          Exit;
        PCnBigNumberElementArray(Num.D)^[Num.Top] := L;
        Inc(Num.Top);
      end;
    end;
  end;
  Result := True;
end;

function BigNumberModWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
var
  I: Integer;
{$IFDEF BN_DATA_USE_64}
  T: TCnBigNumberElement;
{$ENDIF}
begin
  if W = 0 then
    raise EDivByZero.Create(SDivByZero);

{$IFDEF BN_DATA_USE_64}
  if W > $FFFFFFFF then
    raise ECnBigNumberException.Create(SCnErrorBigNumberInvalid64ModRange);
{$ENDIF}

  Result := 0;
  for I := Num.Top - 1 downto 0 do
  begin
{$IFDEF BN_DATA_USE_64}
    Result := ((Result shl BN_BITS4) or ((PCnBigNumberElementArray(Num.D)^[I] shr BN_BITS4) and BN_MASK2l)) mod W;
    Result := ((Result shl BN_BITS4) or (PCnBigNumberElementArray(Num.D)^[I] and BN_MASK2l)) mod W;
{$ELSE}
    // 32 λ����չ��ȥ�� UInt64 ���࣬�𼶰���һ����������Ϊ��һ���ĸ� 64 λ����һ��ƴһ���ٳ�����
    Result := UInt64Mod((TUInt64(Result) shl BN_BITS2) or TUInt64(PCnBigNumberElementArray(Num.D)^[I]), W);
{$ENDIF}
  end;
end;

function BigNumberDivWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
var
  I, J: Integer;
  L, D: TCnBigNumberElement;
begin
  if W = 0 then
    raise EDivByZero.Create(SDivByZero);;

  Result := 0;
  if Num.Top = 0 then
    Exit;

  J := BN_BITS2 - BigNumberGetWordBitsCount(W);

  W := W shl J; // ��֤ W ���λΪ 1
  if not BigNumberShiftLeft(Num, Num, J) then
  begin
    Result := TCnBigNumberElement(-1);
    Exit;
  end;

  for I := Num.Top - 1 downto 0 do
  begin
    L := PCnBigNumberElementArray(Num.D)^[I];
{$IFDEF BN_DATA_USE_64}
    D := InternalDivWords64(Result, L, W); // W ��֤�����λΪ 1��������� 64 λ
{$ELSE}
    D := InternalDivWords(Result, L, W);   // W ��֤�����λΪ 1��������� 32 λ
{$ENDIF}
    Result := (L - ((D * W) and BN_MASK2)) and BN_MASK2;

    PCnBigNumberElementArray(Num.D)^[I] := D;
  end;

  if (Num.Top > 0) and (PCnBigNumberElementArray(Num.D)^[Num.Top - 1] = 0) then
    Dec(Num.Top);
  Result := Result shr J;
end;

procedure BigNumberAndWord(Num: TCnBigNumber; W: TCnBigNumberElement);
begin
  if Num.Top >= 1 then
  begin
    PCnBigNumberElementArray(Num.D)^[0] := PCnBigNumberElementArray(Num.D)^[0] and W;
    if PCnBigNumberElementArray(Num.D)^[0] <> 0 then // 32/64 λ���ϵĶ��� 0
      Num.Top := 1
    else
      Num.Top := 0;
  end;
end;

procedure BigNumberOrWord(Num: TCnBigNumber; W: TCnBigNumberElement);
begin
  if Num.Top > 0 then
    PCnBigNumberElementArray(Num.D)^[0] := PCnBigNumberElementArray(Num.D)^[0] and W
  else
    Num.SetWord(W);
end;

procedure BigNumberXorWord(Num: TCnBigNumber; W: TCnBigNumberElement);
begin
  if Num.Top > 0 then // 32/64 λ���ϵ� xor 0��������
    PCnBigNumberElementArray(Num.D)^[0] := PCnBigNumberElementArray(Num.D)^[0] xor W
  else
    Num.SetWord(W); // 0 ��� W ���� W
end;

function BigNumberAndWordTo(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
begin
  if Num.Top >= 1 then
    Result := PCnBigNumberElementArray(Num.D)^[0] and W
  else
    Result := 0;
end;

{* ������ Word ����ϵ�к�������}

function BigNumberToString(Num: TCnBigNumber): string;
var
  I, J, V, Z: Integer;
begin
  Result := '';
  if BigNumberIsZero(Num) then
  begin
    Result := '0';
    Exit;
  end;
  if BigNumberIsNegative(Num) then
    Result := '-';

  Z := 0;
  for I := Num.Top - 1 downto 0 do
  begin
    J := BN_BITS2 - 4;
    while J >= 0 do
    begin
      V := ((PCnBigNumberElementArray(Num.D)^[I]) shr Cardinal(J)) and $0F;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[V + 1];
        Z := 1;
      end;
      Dec(J, 4);
    end;
  end;
end;

function BigNumberToHex(Num: TCnBigNumber; FixedLen: Integer): string;
var
  I, J, V, Z: Integer;
begin
  Result := '';
  if BigNumberIsZero(Num) then
  begin
    if FixedLen <= 0 then
      Result := '0'
    else
      Result := StringOfChar('0', FixedLen * 2);
    Exit;
  end;

  Z := 0;
  for I := Num.Top - 1 downto 0 do
  begin
    J := BN_BITS2 - 8;
    while J >= 0 do
    begin
      V := ((PCnBigNumberElementArray(Num.D)^[I]) shr Cardinal(J)) and $FF;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[(V shr 4) + 1];
        Result := Result + Hex[(V and $0F) + 1];
        Z := 1;
      end;
      Dec(J, 8);
    end;
  end;

  if FixedLen * 2 > Length(Result) then
    Result := StringOfChar('0', FixedLen * 2 - Length(Result)) + Result;

  if BigNumberIsNegative(Num) then
    Result := '-' + Result;
end;

function BigNumberSetHex(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, H, M, J, I, K, C: Integer;
  L: TCnBigNumberElement;
begin
  Result := False;
  if (Buf = '') or (Res = nil) then
    Exit;

  P := @Buf[1];
  if P^ = '-' then
  begin
    Neg := 1;
    Inc(P);
  end
  else
    Neg := 0;

  // ����Ч���ȣ�һ����ĸ����ռ 4 λ
  I := 0;
  while PAnsiChar(TCnIntAddress(P) + I)^ in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberExpandBits(Res, (I + 2) * 4) = nil then // �����һ��
  begin
    BigNumberFree(Res);
    Exit;
  end;

  J := I;
  H := 0;
  while J > 0 do
  begin
    L := 0;
    if BN_BYTES * 2 <= J then
      M := BN_BYTES * 2
    else
      M := J;

    while True do
    begin
      C := Ord(PAnsiChar(TCnIntAddress(P) + J - M)^);
      if (C >= Ord('0')) and (C <= Ord('9')) then
        K := C - Ord('0')
      else if (C >= Ord('a')) and (C <= Ord('f')) then
        K := C - Ord('a') + 10
      else if (C >= Ord('A')) and (C <= Ord('F')) then
        K := C - Ord('A') + 10
      else
        K := 0;

      L := (L shl 4) or TCnBigNumberElement(K);

      Dec(M);
      if M <= 0 then
      begin
        PCnBigNumberElementArray(Res.D)^[H] := L;
        Inc(H);
        Break;
      end;
    end;
    Dec(J, BN_BYTES * 2);
  end;

  Res.Top := H;
  BigNumberCorrectTop(Res);
  Res.Neg := Neg;
  Result := True;
end;

function BigNumberFromHex(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetHex(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberToDec(Num: TCnBigNumber): AnsiString;
var
  I, N, R, Len: Integer;
  BnData, LP: PCnBigNumberElement;
  T: TCnBigNumber;
  P: PAnsiChar;

  function BufRemain(Nu: Integer; Pt: PAnsiChar; Res: PAnsiChar): Integer;
  begin
    Result := Nu + 3 - (TCnIntAddress(Pt) - TCnIntAddress(Res));
  end;

begin
  Result := '';

  I := BigNumberGetBitsCount(Num) * 3;
  N := ((I div 10) + (I div 1000) + 1) + 1;

  BnData := nil;
  T := nil;
  try
    BnData := PCnBigNumberElement(GetMemory(((N div 9) + 1) * SizeOf(TCnBigNumberElement)));
    if BnData = nil then
      Exit;

    SetLength(Result, N + 3);
    FillChar(Result[1], Length(Result), 0);

    T := FLocalBigNumberPool.Obtain;
    if BigNumberCopy(T, Num) = nil then
      Exit;

    P := @(Result[1]);
    LP := BnData;

    if BigNumberIsZero(T) then
    begin
      P^ := '0';
      Inc(P);
      P^ := Chr(0);
    end
    else
    begin
      if BigNumberIsNegative(T) then
      begin
        P^ := '-';
        Inc(P);
      end;

      while not BigNumberIsZero(T) do
      begin
        LP^ := BigNumberDivWord(T, BN_DEC_CONV);
        LP := PCnBigNumberElement(TCnIntAddress(LP) + SizeOf(TCnBigNumberElement));
      end;
      LP := PCnBigNumberElement(TCnIntAddress(LP) - SizeOf(TCnBigNumberElement));

      R := BufRemain(N, P, @(Result[1]));
{$IFDEF UNICODE}
      AnsiStrings.AnsiFormatBuf(P^, R, AnsiString(BN_DEC_FMT), Length(BN_DEC_FMT), [LP^]);
{$ELSE}
      FormatBuf(P^, R, BN_DEC_FMT, Length(BN_DEC_FMT), [LP^]);
{$ENDIF}
      while P^ <> #0 do
        Inc(P);
      while LP <> BnData do
      begin
        LP := PCnBigNumberElement(TCnIntAddress(LP) - SizeOf(TCnBigNumberElement));
        R := BufRemain(N, P, @(Result[1]));
{$IFDEF UNICODE}
        AnsiStrings.AnsiFormatBuf(P^, R, AnsiString(BN_DEC_FMT2), Length(BN_DEC_FMT2), [LP^]);
{$ELSE}
        FormatBuf(P^, R, BN_DEC_FMT2, Length(BN_DEC_FMT2), [LP^]);
{$ENDIF}
        while P^ <> #0 do
          Inc(P);
      end;
    end;
  finally
    if BnData <> nil then
      FreeMemory(BnData);

    FLocalBigNumberPool.Recycle(T);
  end;

  Len := SysUtils.StrLen(PAnsiChar(Result));
  if Len >= 0 then
    SetLength(Result, Len); // ȥ��β������� #0
end;

function BigNumberSetDec(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, J, I: Integer;
  L: TCnBigNumberElement;
begin
  Result := False;
  if (Buf = '') or (Res = nil) then
    Exit;

  P := @Buf[1];
  if P^ = '-' then
  begin
    Neg := 1;
    Inc(P);
  end
  else
    Neg := 0;

  // ����Ч����
  I := 0;
  while PAnsiChar(TCnIntAddress(P) + I)^ in ['0'..'9'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberExpandBits(Res, (I + 2) * 4) = nil then // һλʮ���������� 4 λ���������չһ���
  begin
    BigNumberFree(Res);
    Exit;
  end;

  J := 9 - (I mod 9);
  if J = 9 then
    J := 0;
  L := 0;

  while P^ <> #0 do
  begin
    L := L * 10;
    L := L + Ord(P^) - Ord('0');
    Inc(P);
    Inc(J);
    if J = 9 then
    begin
      BigNumberMulWord(Res, BN_DEC_CONV);
      BigNumberAddWord(Res, L);
      L := 0;
      J := 0;
    end;
  end;

  BigNumberCorrectTop(Res);
  Res.Neg := Neg;
  Result := True;
end;

function BigNumberFromDec(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetDec(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberSetFloat(F: Extended; Res: TCnBigNumber): Boolean;
var
  N: Boolean;
  E: Integer;
  M: TUInt64;
begin
  ExtractFloatExtended(F, N, E, M);

  BigNumberSetUInt64UsingInt64(Res, M);
  Res.SetNegative(N);

  E := E - 63;
  if E > 0 then
    Res.ShiftLeft(E)
  else
    Res.ShiftRight(-E);

  Result := True;
end;

function BigNumberGetFloat(Num: TCnBigNumber): Extended;
var
  N: Boolean;
  E, B, K: Integer;
  M, T: TUInt64;
  DB: Double;
begin
  Result := 0;
  if not Num.IsZero then
  begin
    N := Num.IsNegative;

    B := Num.GetBitsCount;
    E := B - 1;

    if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
    begin
      if E > CN_EXTENDED_MAX_EXPONENT then
        raise ERangeError.Create(SCnErrorBigNumberFloatExponentRange);

      if B <= 64 then
      begin
{$IFDEF BN_DATA_USE_64}
        M := PUInt64Array(Num.D)^[0];
{$ELSE}
        if B >= 32 then
          M := PInt64Array(Num.D)^[0]
        else
          M := PCnLongWord32Array(Num.D)^[0];
{$ENDIF}

        if B < 64 then   // 10 �ֽ���չ������ 64 λ��Ч���֣�Ҫ�����λΪ 1
          M := M shl (64 - B);
      end
      else // �� Top > 2����ֻ��ȡ��� 64 λ�� M ������ֻ������
      begin
        // (B - 1) div 64 �Ǹߵ�Ҫ���� 64 λ����ţ���ͷ�� B mod 64 ��λ
        K := (B - 1) div 64;
{$IFDEF SUPPORT_UINT64}
        T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
        T := PInt64Array(Num.D)^[K];
{$ENDIF}
        // T �õ���ߵ� 64 λ Element
        K := B mod 64;
        if K > 0 then // �� T ��ֻ�и� K λ
          T := T shl (64 - K);

        M := T; // M �õ�һ����λ��

        if K > 0 then // Ҫ����һ�� M �ĵ�λ
        begin
          K := ((B - 1) div 64) - 1;
{$IFDEF SUPPORT_UINT64}
          T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
          T := PInt64Array(Num.D)^[K];
{$ENDIF}
          // T �õ��θߵ� 64 λ Element
          K := 64 - (B mod 64); // Ҫ������� T �ĸ� K λ

          T := T shr (64 - K);
          M := M or T;
        end;
      end;

      CombineFloatExtended(N, E, M, Result);
    end
    else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    begin
      if E > CN_DOUBLE_MAX_EXPONENT then
        raise ERangeError.Create(SCnErrorBigNumberFloatExponentRange);

      if B <= 64 then
      begin
{$IFDEF BN_DATA_USE_64}
        M := PUInt64Array(Num.D)^[0];
{$ELSE}
        if B >= 32 then
          M := PInt64Array(Num.D)^[0]
        else
          M := PCnLongWord32Array(Num.D)^[0];
{$ENDIF}

        if B < 53 then
          M := M shl (53 - B)  // ˫���ȸ����� 53 λ��Ч���֣����λ�ڲ��ᱻ������ȥ
        else if B > 53 then
          M := M shr (B - 53); // ��� 64 λ�ڱ� 53 λ�ֻ࣬ȡ��� 53 λ��������������
        // ��ʱ M �ǵ� 53 λ��Ч������
      end
      else // �������λ������ 64����ֻ��ȡ��� 53 λ�� M ��������������
      begin
        // (B - 1) div 64 �Ǹߵ�Ҫ���� 64 λ����ţ���ͷ�� B mod 64 ��λ
        K := (B - 1) div 64;
{$IFDEF SUPPORT_UINT64}
        T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
        T := PInt64Array(Num.D)^[K];
{$ENDIF}
        // T �õ���ߵ� 64 λ Element
        K := B mod 64;
        if K > 0 then // T ��ֻ�и� K λ��Ч
          T := T shl (64 - K);

        M := T; // M �õ�һ����λ��

        if K > 0 then // Ҫ����һ�� M �ĵ�λ
        begin
          K := ((B - 1) div 64) - 1;
{$IFDEF SUPPORT_UINT64}
          T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
          T := PInt64Array(Num.D)^[K];
{$ENDIF}
          // T �õ��θߵ� 64 λ Element
          K := 64 - (B mod 64); // Ҫ������� T �ĸ� K λ

          T := T shr (64 - K);
          M := M or T;
        end;

        // ��ʱ M �Ǹ� 64 λ������ 11 λ���� 53 λ��Ч
        M := M shr 11;
      end;

      CombineFloatDouble(N, E, M, DB);
      Result := DB;
    end;
  end;
end;

function BigNumberFromFloat(F: Extended): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetFloat(F, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

// Tmp should have 2 * N UInt32/UInt64
procedure BigNumberSqrNormal(R: PCnBigNumberElement; A: PCnBigNumberElement;
  N: Integer; Tmp: PCnBigNumberElement);
var
  I, J, Max: Integer;
  AP, RP: PCnBigNumberElementArray;
begin
  Max := N * 2;
  AP := PCnBigNumberElementArray(A);
  RP := PCnBigNumberElementArray(R);
  RP^[0] := 0;
  RP^[Max - 1] := 0;

  RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
  J := N - 1;

  if J > 0 then
  begin
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP^[J] := BigNumberMulWords(RP, AP, J, PCnBigNumberElementArray(TCnIntAddress(AP) - SizeOf(TCnBigNumberElement))^[0]);
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 2 * SizeOf(TCnBigNumberElement));
  end;

  for I := N - 2 downto 1 do
  begin
    Dec(J);
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP^[J] := BigNumberMulAddWords(RP, AP, J, PCnBigNumberElementArray(TCnIntAddress(AP) - SizeOf(TCnBigNumberElement))^[0]);
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 2 * SizeOf(TCnBigNumberElement));
  end;

  BigNumberAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(R), PCnBigNumberElementArray(R), Max);
  BigNumberSqrWords(PCnBigNumberElementArray(Tmp), PCnBigNumberElementArray(A), N);
  BigNumberAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(R), PCnBigNumberElementArray(Tmp), Max);
end;

function BigNumberSqr(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  Max, AL: Integer;
  Tmp, RR: TCnBigNumber;
  T: array[0..15] of TCnBigNumberElement;
  IsFromPool: Boolean;
begin
  Result := False;
  AL := Num.Top;
  if AL <= 0 then
  begin
    Res.Top := 0;
    Res.Neg := 0;
    Result := True;
    Exit;
  end;

  RR := nil;
  Tmp := nil;
  IsFromPool := False;

  try
    if Num <> Res then
      RR := Res
    else
    begin
      RR := FLocalBigNumberPool.Obtain;
      IsFromPool := True;
    end;

    Tmp := FLocalBigNumberPool.Obtain;
    if (RR = nil) or (Tmp = nil) then
      Exit;

    Max := 2 * AL;
    if BigNumberWordExpand(RR, Max) = nil then
      Exit;

    if AL = 4 then
    begin
      BigNumberSqrNormal(RR.D, Num.D, 4, @(T[0]));
    end
    else if AL = 8 then
    begin
      BigNumberSqrNormal(RR.D, Num.D, 8, @(T[0]));
    end
    else
    begin
      if BigNumberWordExpand(Tmp, Max) = nil then
        Exit;
      BigNumberSqrNormal(RR.D, Num.D, AL, Tmp.D);
    end;

    RR.Neg := 0;
    if PCnBigNumberElementArray(Num.D)^[AL - 1] = (PCnBigNumberElementArray(Num.D)^[AL - 1] and BN_MASK2l) then
      RR.Top := Max - 1
    else
      RR.Top := Max;

    if RR <> Res then
      BigNumberCopy(Res, RR);
    Result := True;
  finally
    if IsFromPool then
      FLocalBigNumberPool.Recycle(RR);
    FLocalBigNumberPool.Recycle(Tmp);
  end;
end;

function BigNumberSqrt(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  U: TUInt64;
  BitLength, Shift: Integer;
  X, XNext: TCnBigNumber;
begin
  Result := False;
  if Num.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end
  else if Num.IsNegative then
    Exit
  else if Num.Top <= 2 then
  begin
    U := BigNumberGetUInt64UsingInt64(Num);
    U := UInt64Sqrt(U);
    BigNumberSetUInt64UsingInt64(Res, U);
    Result := True;
    Exit;
  end
  else
  begin
    BitLength := Num.GetBitsCount;
    Shift := BitLength - 63;
    if (Shift and 1) <> 0 then  // �� 63 λ������������� 64��Ҳ����ż��λȡ�� 64 λ������λȡ�� 63 λ����ƽ��������
      Inc(Shift);

    X := nil;
    XNext := nil;

    try
      X := FLocalBigNumberPool.Obtain;
      XNext := FLocalBigNumberPool.Obtain;

      BigNumberCopy(X, Num);
      X.ShiftRight(Shift); // ȡ��ߵ� 64 λ�� 63 λ������ƽ����

      U := X.GetInt64;
      U := UInt64Sqrt(U);
      X.SetInt64(U);

      X.ShiftLeft(Shift shr 1); // X �ǹ����ƽ����

      // ţ�ٵ�����
      while True do
      begin
        // Xnext = (x + n/x)/2
        BigNumberDiv(XNext, nil, Num, X);
        BigNumberAdd(XNext, XNext, X);
        XNext.ShiftRightOne;

        if BigNumberCompare(XNext, X) = 0 then
        begin
          // ���� X ���������ֲ��ٱ仯ʱ���ǽ��
          BigNumberCopy(Res, X);
          Result := True;
          Exit;
        end;
        // X := XNext
        BigNumberCopy(X, XNext);
      end;
    finally
      FLocalBigNumberPool.Recycle(XNext);
      FLocalBigNumberPool.Recycle(X);
    end;
  end;
end;

function BigNumberRoot(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Integer): Boolean;
var
  I: Integer;
  X0, X1, T1, T2, T3: TCnBigBinary;
  C0, C1: TCnBigNumber;
  U: TUInt64;
begin
  Result := False;
  if (Exponent <= 0) or Num.IsNegative then
    Exit;

  if Num.IsOne or Num.IsZero then
  begin
    BigNumberCopy(Res, Num);
    Result := True;
    Exit;
  end
  else if Exponent = 2 then
    Result := BigNumberSqrt(Res, Num)
  else if Num.Top <= 2 then
  begin
    U := BigNumberGetUInt64UsingInt64(Num);
    U := UInt64NonNegativeRoot(U, Exponent);
    BigNumberSetUInt64UsingInt64(Res, U);
    Result := True;
    Exit;
  end
  else
  begin
    // ţ�ٵ��������
    I := Num.GetBitsCount + 1;  // �õ���Լ Log2 N ��ֵ
    I := (I div Exponent) + 1;

    X0 := nil;
    X1 := nil;
    T1 := nil;
    T2 := nil;
    T3 := nil;
    C0 := nil;
    C1 := nil;

    // ��ĶԷ�ûʹ�ã��ӳٵ��˴���ʼ��
    if FLocalBigBinaryPool = nil then
      FLocalBigBinaryPool := TCnBigBinaryPool.Create;

    try
      X0 := FLocalBigBinaryPool.Obtain;
      X1 := FLocalBigBinaryPool.Obtain;
      T1 := FLocalBigBinaryPool.Obtain;
      T2 := FLocalBigBinaryPool.Obtain;
      T3 := FLocalBigBinaryPool.Obtain;

      C0 := FLocalBigNumberPool.Obtain;
      C1 := FLocalBigNumberPool.Obtain;

      X0.SetOne;
      X0.ShiftLeft(I);                  // �õ�һ���ϴ�� X0 ֵ��Ϊ��ʼֵ

      repeat
        // X1 := X0 - (Power(X0, Exponent) - N) / (Exponent * Power(X0, Exponent - 1));
        BigBinaryCopy(T1, X0);
        T1.Power(Exponent);
        T2.SetBigNumber(Num);
        BigBinarySub(T1, T1, T2);             // �õ� Power(X0, Exponent) - N

        BigBinaryCopy(T2, X0);
        T2.Power(Exponent - 1);
        T2.MulWord(Exponent);                // �õ� Exponent * Power(X0, Exponent - 1)

        BigBinaryDiv(T1, T1, T2, 10);        // �õ��̣�����һ������
        BigBinarySub(X1, X0, T1);            // ��� X1

        // �õ� X0 �� X1 ���������ֲ��Ƚ�
        BigBinaryTruncTo(C0, X0);
        BigBinaryTruncTo(C1, X1);
        if BigNumberCompare(C0, C1) = 0 then
        begin
          // ������Ϊ X0 X1 �������ֲ������仯����Ϊ�ﵽ������
          BigNumberCopy(Res, C0);
          Result := True;
          Exit;
        end;

        BigBinaryCopy(X0, X1);
      until False;
    finally
      FLocalBigBinaryPool.Recycle(X1);
      FLocalBigBinaryPool.Recycle(X0);
      FLocalBigBinaryPool.Recycle(T3);
      FLocalBigBinaryPool.Recycle(T2);
      FLocalBigBinaryPool.Recycle(T1);

      FLocalBigNumberPool.Recycle(C1);
      FLocalBigNumberPool.Recycle(C0);
    end;
  end;
end;

procedure BigNumberMulNormal(R: PCnBigNumberElement; A: PCnBigNumberElement; NA: Integer; B: PCnBigNumberElement;
  NB: Integer);
var
  RR: PCnBigNumberElement;
  Tmp: Integer;
begin
  if NA < NB then
  begin
    Tmp := NA;
    NA := NB;
    NB := Tmp;

    RR := B;
    B := A;
    A := RR;
  end;

  RR := PCnBigNumberElement(TCnIntAddress(R) + NA * SizeOf(TCnBigNumberElement));
  if NB <= 0 then
  begin
    BigNumberMulWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, 0);
    Exit;
  end
  else
    RR^ := BigNumberMulWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

  while True do
  begin
    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));

    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));
    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));
    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));
    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);
  end;
end;

function BigNumberMulKaratsuba(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  H: Integer;
  XL, XH, YL, YH, P1, P2, P3: TCnBigNumber;
begin
  H := Num1.GetWordCount;
  if H < Num2.GetWordCount then
    H := Num2.GetWordCount;

  Inc(H);
  H := H shr 1;

  XL := FLocalBigNumberPool.Obtain;
  XH := FLocalBigNumberPool.Obtain;
  YL := FLocalBigNumberPool.Obtain;
  YH := FLocalBigNumberPool.Obtain;
  P1 := FLocalBigNumberPool.Obtain;
  P2 := FLocalBigNumberPool.Obtain;
  P3 := FLocalBigNumberPool.Obtain;

  try
    BigNumberCopyLow(XL, Num1, H);
    BigNumberCopyHigh(XH, Num1, Num1.GetWordCount - H);
    BigNumberCopyLow(YL, Num2, H);
    BigNumberCopyHigh(YH, Num2, Num2.GetWordCount - H);

    BigNumberAdd(P1, XH, XL);
    BigNumberAdd(P2, YH, YL);
    BigNumberMul(P3, P1, P2); // p3=(xh+xl)*(yh+yl)

    BigNumberMul(P1, XH, YH); // p1 = xh*yh
    BigNumberMul(P2, XL, YL); // p2 = xl*yl

    // p1 * 2^(32*2*h) + (p3 - p1 - p2) * 2^(32*h) + p2
    BigNumberSub(P3, P3, P1);
    BigNumberSub(P3, P3, P2);
    BigNumberShiftLeft(P3, P3, 32 * H); // P3 �õ� (p3 - p1 - p2) * 2^(32*h)

    BigNumberShiftLeft(P1, P1, 32 * 2 * H); // P1 �õ� p1 * 2^(32*2*h)

    BigNumberAdd(Res, P3, P1);
    BigNumberAdd(Res, Res, P2);
    Res.SetNegative(Num1.IsNegative <> Num2.IsNegative);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(XL);
    FLocalBigNumberPool.Recycle(XH);
    FLocalBigNumberPool.Recycle(YL);
    FLocalBigNumberPool.Recycle(YH);
    FLocalBigNumberPool.Recycle(P1);
    FLocalBigNumberPool.Recycle(P2);
    FLocalBigNumberPool.Recycle(P3);
  end;
end;

function BigNumberMul(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Top, AL, BL: Integer;
  RR: TCnBigNumber;
  IsFromPool: Boolean;
begin
  Result := False;
  AL := Num1.Top;
  BL := Num2.Top;

  if (AL = 0) or (BL = 0) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  if (AL < BN_MUL_KARATSUBA) and (BL < BN_MUL_KARATSUBA) then // С�ġ�ֱ�ӳ�
  begin
    Top := AL + BL;

    RR := nil;
    IsFromPool := False;

    try
      if (Res = Num1) or (Res = Num2) then
      begin
        RR := FLocalBigNumberPool.Obtain;
        IsFromPool := True;
        if RR = nil then
          Exit;
      end
      else
        RR := Res;

      if Num1.Neg <> Num2.Neg then
        RR.Neg := 1
      else
        RR.Neg := 0;

      if BigNumberWordExpand(RR, Top) = nil then
        Exit;
      RR.Top := Top;
      BigNumberMulNormal(RR.D, Num1.D, AL, Num2.D, BL);

      if RR <> Res then
        BigNumberCopy(Res, RR);

      BigNumberCorrectTop(Res);
      Result := True;
    finally
      if IsFromPool then
        FLocalBigNumberPool.Recycle(RR);
    end;
  end
  else // ���������㷨
    Result := BigNumberMulKaratsuba(Res, Num1, Num2);
end;

function BigNumberMulFloat(Res: TCnBigNumber; Num: TCnBigNumber;
  F: Extended): Boolean;
var
  N: Boolean;
  E: Integer;
  M: TUInt64;
  B: TCnBigNumber;
begin
  if F = 0 then
    Res.SetZero
  else if (F = 1) or (F = -1) then
  begin
    BigNumberCopy(Res, Num);
    if F = -1 then
      Res.Negate;
  end
  else
  begin
    // ������š�ָ������Ч���ֽ�����������
    ExtractFloatExtended(F, N, E, M);

    // ע�� Extended �� Win32/Mac ���� 10 �ֽ���չ���ȣ��� Win64 ���� 8 �ֽ�˫����
    if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
      E := E - 63
    else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
      E := E - 52;
    // ����Ч���ָ��ݾ��ȵ���С���������������
    // ���ڵ���ʵֵΪ M * 2^E �η�������Ҫ���� M���ٳ��� 2^E

    B := FLocalBigNumberPool.Obtain;
    try
      BigNumberSetUInt64UsingInt64(B, M);
      BigNumberMul(Res, Num, B);

      B.SetWord(1);
      if E > 0 then
      begin
        B.ShiftLeft(E);
        BigNumberMul(Res, Res, B);
      end
      else
      begin
        B.ShiftLeft(-E);
        BigNumberDiv(Res, nil, Res, B);
      end;

      if N then
        Res.Negate;
    finally
      FLocalBigNumberPool.Recycle(B);
    end;
  end;
  Result := True;
end;

function BigNumberDiv(Res: TCnBigNumber; Remain: TCnBigNumber;
  Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
var
  Tmp, SNum, SDiv, SRes: TCnBigNumber;
  I, NormShift, Loop, NumN, DivN, Neg, BackupTop, BackupDMax, BackupNeg: Integer;
  D0, D1, Q, L0, N0, N1, Rem, T2L, T2H: TCnBigNumberElement;
  Resp, WNump, BackupD: PCnBigNumberElement;
  WNum: TCnBigNumber;
{$IFNDEF BN_DATA_USE_64}
  T2: TUInt64;
{$ENDIF}
begin
  Result := False;
  if (Num.Top > 0) and (PCnBigNumberElementArray(Num.D)^[Num.Top - 1] = 0) then
    Exit;

  if BigNumberIsZero(Divisor) then
    Exit;

  if BigNumberUnsignedCompare(Num, Divisor) < 0 then
  begin
    if Remain <> nil then
      if BigNumberCopy(Remain, Num) = nil then
        Exit;
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  WNum := nil;
  Tmp := nil;
  SNum := nil;
  SDiv := nil;
  BackupTop := 0;
  BackupDMax := 0;
  BackupNeg := 0;
  BackupD := nil;

  try
    Tmp := FLocalBigNumberPool.Obtain;
    SNum := FLocalBigNumberPool.Obtain;
    SDiv := FLocalBigNumberPool.Obtain;
    SRes := Res;

    if (Tmp = nil) or (SNum = nil) or (SDiv = nil) or (SRes = nil) then
      Exit;

    // �ѳ������Ƶ����λ�� 1������ SDiv���� ȷ������� D0 ���λ�� 1
    NormShift := BN_BITS2 - (BigNumberGetBitsCount(Divisor) mod BN_BITS2);
    if not BigNumberShiftLeft(SDiv, Divisor, NormShift) then
      Exit;

    SDiv.Neg := 0;
    // �ѱ�����ͬ�����ƣ���������һ����
    NormShift := NormShift + BN_BITS2;
    if not BigNumberShiftLeft(SNum, Num, NormShift) then
      Exit;

    SNum.Neg := 0;
    DivN := SDiv.Top;
    NumN := SNum.Top;
    Loop := NumN - DivN;

    WNum := FLocalBigNumberPool.Obtain;
    BackupNeg := WNum.Neg;
    BackupD := WNum.D;
    BackupTop := WNum.Top;
    BackupDMax := WNum.DMax;

    // ע�� WNum ��Ҫʹ���ⲿ�� D���ѳ������ó����Ķ����ȱ���
    WNum.Neg := 0;
    WNum.D := PCnBigNumberElement(TCnIntAddress(SNum.D) + Loop * SizeOf(TCnBigNumberElement));
    WNum.Top := DivN;
    WNum.DMax := SNum.DMax - Loop;

    D0 := PCnBigNumberElementArray(SDiv.D)^[DivN - 1];
    if DivN = 1 then
      D1 := 0
    else
      D1 := PCnBigNumberElementArray(SDiv.D)^[DivN - 2];
    // D0 D1 �� SDiv ������� UInt32/UInt64

    WNump := PCnBigNumberElement(TCnIntAddress(SNum.D) + (NumN - 1) * SizeOf(TCnBigNumberElement));

    if Num.Neg <> Divisor.Neg then
      SRes.Neg := 1
    else
      SRes.Neg := 0;

    if BigNumberWordExpand(SRes, Loop + 1) = nil then
      Exit;

    SRes.Top := Loop;
    Resp := PCnBigNumberElement(TCnIntAddress(SRes.D) + (Loop - 1) * SizeOf(TCnBigNumberElement));

    if BigNumberWordExpand(Tmp, DivN + 1) = nil then
      Exit;

    if BigNumberUnsignedCompare(WNum, SDiv) >= 0 then
    begin
      BigNumberSubWords(PCnBigNumberElementArray(WNum.D), PCnBigNumberElementArray(WNum.D),
        PCnBigNumberElementArray(SDiv.D), DivN);
      Resp^ := 1;
    end
    else
      Dec(SRes.Top);

    if SRes.Top = 0 then
      SRes.Neg := 0
    else
      Resp := PCnBigNumberElement(TCnIntAddress(Resp) - SizeOf(TCnBigNumberElement));

    for I := 0 to Loop - 2 do
    begin
//    Rem := 0;
      // �� N0/N1/D0/D1 �����һ�� Q ʹ | WNum - SDiv * Q | < SDiv
      N0 := WNump^;
      N1 := (PCnBigNumberElement(TCnIntAddress(WNump) - SizeOf(TCnBigNumberElement)))^;

      if N0 = D0 then
        Q := BN_MASK2
      else
      begin
{$IFDEF BN_DATA_USE_64}
        Q := InternalDivWords64(N0, N1, D0); // D0 �������ı�֤���λ�� 1
{$ELSE}
        Q := InternalDivWords(N0, N1, D0); // D0 �������ı�֤���λ�� 1
{$ENDIF}
        Rem := (N1 - Q * D0) and BN_MASK2;

{$IFDEF BN_DATA_USE_64}
        UInt64MulUInt64(D1, Q, T2L, T2H);
{$ELSE}
        T2 := UInt64Mul(D1, Q);
        T2H := (T2 shr 32) and BN_MASK2;
        T2L := T2 and BN_MASK2;
{$ENDIF}

        while True do
        begin
          if (T2H < Rem) or ((T2H = Rem) and
             (T2L <= (PCnBigNumberElement(TCnIntAddress(WNump) - 2 * SizeOf(TCnBigNumberElement)))^)) then
             Break;
          Dec(Q);
          Inc(Rem, D0);
          if Rem < D0 then
            Break;
          if T2L < D1 then
            Dec(T2H);
          Dec(T2L, D1);
        end;
      end;

      L0 := BigNumberMulWords(PCnBigNumberElementArray(Tmp.D), PCnBigNumberElementArray(SDiv.D), DivN, Q);
      PCnBigNumberElementArray(Tmp.D)^[DivN] := L0;
      WNum.D := PCnBigNumberElement(TCnIntAddress(WNum.D) - SizeOf(TCnBigNumberElement));

      if BigNumberSubWords(PCnBigNumberElementArray(WNum.D), PCnBigNumberElementArray(WNum.D),
        PCnBigNumberElementArray(Tmp.D), DivN + 1) <> 0 then
      begin
        Dec(Q);
        if BigNumberAddWords(PCnBigNumberElementArray(WNum.D), PCnBigNumberElementArray(WNum.D),
          PCnBigNumberElementArray(SDiv.D), DivN) <> 0 then
          WNump^ := WNump^ + 1;
      end;

      Resp^ := Q;
      WNump := PCnBigNumberElement(TCnIntAddress(WNump) - SizeOf(TCnBigNumberElement));
      Resp := PCnBigNumberElement(TCnIntAddress(Resp) - SizeOf(TCnBigNumberElement));
    end;

    BigNumberCorrectTop(SNum);
    Neg := Num.Neg;

    if Remain <> nil then // ��Ҫ����ʱ
    begin
      BigNumberShiftRight(Remain, SNum, NormShift);
      if not BigNumberIsZero(Remain) then
        Remain.Neg := Neg;
    end;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Tmp);
    FLocalBigNumberPool.Recycle(SNum);
    FLocalBigNumberPool.Recycle(SDiv);
    // �ָ� WNum ���ݲ��ӻس�����
    WNum.Neg := BackupNeg;
    WNum.D := BackupD;
    WNum.Top := BackupTop;
    WNum.DMax := BackupDMax;
    FLocalBigNumberPool.Recycle(WNum);
  end;
end;

function BigNumberRoundDiv(Res: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber; out Rounding: Boolean): Boolean;
var
  R, H: TCnBigNumber;
  C: Integer;
begin
  R := FLocalBigNumberPool.Obtain;
  H := FLocalBigNumberPool.Obtain;
  try
    Result := BigNumberDiv(Res, R, Num, Divisor);

    // �������� R �ж� Res �Ƿ�Ҫ�Ӽ�һ
    BigNumberShiftRightOne(H, Divisor);
    // H �ǳ���һ�����������С�ڵ��ڳ����ľ�ȷ��һ��

    if Divisor.IsOdd then // H �ľ���ֵ��һ
    begin
      if Divisor.IsNegative then
        H.SubWord(1)
      else
        H.AddWord(1);
    end;

    C := BigNumberUnsignedCompare(R, H); // �ȽϾ���ֵ
    if C >= 0 then
    begin
      // ������ż��ʱ��H �ǳ�����һ�룬����������ڻ���� H ʱ��������
      // ����������ʱ��H �ǳ�����һ���һ������������ڻ���� H ʱһ������һ�룬Ҳ��������
      // �������̵ľ���ֵ��һ��Ҫ���̵ķ��ž����Ǽ�һ���Ǽ�һ
      if Res.IsNegative then
        Res.SubWord(1)
      else
        Res.AddWord(1);
      Rounding := True;
    end
    else // ����������ᣬ����
      Rounding := False;
  finally
    FLocalBigNumberPool.Recycle(H);
    FLocalBigNumberPool.Recycle(R);
  end;
end;

function BigNumberMod(Remain: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
var
  Res: TCnBigNumber;
begin
  Res := FLocalBigNumberPool.Obtain;
  try
    Result := BigNumberDiv(Res, Remain, Num, Divisor);
  finally
    FLocalBigNumberPool.Recycle(Res);
  end;
end;

function BigNumberNonNegativeMod(Remain: TCnBigNumber;
  Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
begin
  Result := False;
  if not BigNumberMod(Remain, Num, Divisor) then
    Exit;

  Result := True;
  if Remain.Neg = 0 then
    Exit;

  // ���� -|Divisor| < Remain < 0��������Ҫ Remain := Remain + |Divisor|
  if Divisor.Neg <> 0 then
    Result := BigNumberSub(Remain, Remain, Divisor)
  else
    Result := BigNumberAdd(Remain, Remain, Divisor);
end;

function BigNumberMulWordNonNegativeMod(Res: TCnBigNumber;
  Num: TCnBigNumber; N: Integer; Divisor: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInteger(N);
    Result := BigNumberDirectMulMod(Res, Num, T, Divisor);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberAddMod(Res: TCnBigNumber; Num1, Num2: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  T := FLocalBigNumberPool.Obtain;
  try
    if not BigNumberAdd(T, Num1, Num2) then
      Exit;

    Result := BigNumberNonNegativeMod(Res, T, Divisor);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberSubMod(Res: TCnBigNumber; Num1, Num2: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  T := FLocalBigNumberPool.Obtain;
  try
    if not BigNumberSub(T, Num1, Num2) then
      Exit;

    Result := BigNumberNonNegativeMod(Res, T, Divisor);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberDivFloat(Res: TCnBigNumber; Num: TCnBigNumber;
  F: Extended): Boolean;
begin
  Result := False;
  if F = 0 then
     Exit;

  Result := BigNumberMulFloat(Res, Num, 1 / F);
end;

function BigNumberPower(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Cardinal): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  if Exponent = 0 then
  begin
    if Num.IsZero then  // 0 �� 0 �η�
      Exit;

    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent = 1 then // 1 �η�Ϊ����
  begin
    BigNumberCopy(Res, Num);
    Result := True;
    Exit;
  end;

  T := FLocalBigNumberPool.Obtain;
  BigNumberCopy(T, Num);

  try
    // ��������ʽ���ټ��� T �Ĵη���ֵ�� Res
    Res.SetOne;
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        BigNumberMul(Res, Res, T);

      Exponent := Exponent shr 1;
      if Exponent > 0 then // ������ 0 ʱҪ����������Ҫ���һ����
        BigNumberMul(T, T, T);
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberExp(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
var
  I, Bits: Integer;
  V, RR: TCnBigNumber;
  IsFromPool: Boolean;
begin
  Result := False;
  RR := nil;
  V := nil;
  IsFromPool := False;

  try
    if (Res = Num) or (Res = Exponent) then
    begin
      RR := FLocalBigNumberPool.Obtain;
      IsFromPool := True;
    end
    else
      RR := Res;

    V := FLocalBigNumberPool.Obtain;
    if (RR = nil) or (V = nil) then
      Exit;

    if BigNumberCopy(V, Num) = nil then
      Exit;

    Bits := BigNumberGetBitsCount(Exponent);
    if BigNumberIsOdd(Exponent) then
    begin
      if BigNumberCopy(RR, Num) = nil then
        Exit;
    end
    else
    begin
      if not BigNumberSetOne(RR) then
        Exit;
    end;

    for I := 1 to Bits - 1 do
    begin
      if not BigNumberSqr(V, V) then
        Exit;

      if BigNumberIsBitSet(Exponent, I) then
        if not BigNumberMul(RR, RR, V) then
          Exit;
    end;

    if Res <> RR then
      BigNumberCopy(Res, RR);
    Result := True;
  finally
    if IsFromPool then
      FLocalBigNumberPool.Recycle(RR);
    FLocalBigNumberPool.Recycle(V);
  end;
end;

// շת������� A �� B �����Լ������Լ������ A �� B �У����ص�ַ
function EuclidGcd(A: TCnBigNumber; B: TCnBigNumber): TCnBigNumber;
var
  T: TCnBigNumber;
  Shifts: Integer;
begin
  Result := nil;
  Shifts := 0;
  while not BigNumberIsZero(B) do
  begin
    if BigNumberIsOdd(A) then
    begin
      if BigNumberIsOdd(B) then
      begin
        // A �� B ��
        if not BigNumberSub(A, A, B) then
          Exit;
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end
      else  // A �� B ż
      begin
        if not BigNumberShiftRightOne(B, B) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end;
    end
    else // A ż
    begin
      if BigNumberIsOdd(B) then
      begin
        // A ż B ��
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end
      else // A ż B ż
      begin
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if not BigNumberShiftRightOne(B, B) then
          Exit;
        Inc(Shifts);
      end;
    end;
  end;

  if Shifts <> 0 then
    if not BigNumberShiftLeft(A, A, Shifts) then
      Exit;
  Result := A;
end;

function BigNumberGcd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  T, A, B: TCnBigNumber;
  R: Int64;
begin
  Result := False;

  // С��������� Int64 �汾�����Լ��٣���Ϊ��Լ��С����������һ��
  if Num1.IsInt64 and Num2.IsInt64 then
  begin
    R := CnInt64GreatestCommonDivisor2(Num1.GetInt64, Num2.GetInt64);
    Res.SetInt64(R);
    Result := True;
    Exit;
  end;

  A := nil;
  B := nil;

  try
    A := FLocalBigNumberPool.Obtain;
    B := FLocalBigNumberPool.Obtain;
    if (A = nil) or (B = nil) then
      Exit;

    if BigNumberCopy(A, Num1) = nil then
      Exit;
    if BigNumberCopy(B, Num2) = nil then
      Exit;

    A.Neg := 0;
    B.Neg := 0;
    if BigNumberCompare(A, B) < 0 then
    begin
      T := A;
      A := B;
      B := T;
    end;

    T := EuclidGcd(A, B);
    if T = nil then
      Exit;

    if BigNumberCopy(Res, T) = nil then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(A);
    FLocalBigNumberPool.Recycle(B);
  end;
end;

function BigNumberLcm(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  G, M, R: TCnBigNumber;
begin
  Result := False;
  if BigNumberCompare(Num1, Num2) = 0 then
  begin
    BigNumberCopy(Res, Num1);
    Result := True;
    Exit;
  end;

  G := nil;
  M := nil;
  R := nil;

  try
    G := FLocalBigNumberPool.Obtain;
    M := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;

    if not BigNumberGcd(G, Num1, Num2) then
      Exit;

    if not BigNumberMul(M, Num1, Num2) then
      Exit;

    if not BigNumberDiv(Res, R, M, G) then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(M);
    FLocalBigNumberPool.Recycle(G);
  end;
end;

// ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ�}
function BigNumberMulMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  T, P: TCnBigNumber;
begin
  if not BigNumberIsNegative(A) and not BigNumberIsNegative(B) then
    Result := BigNumberUnsignedMulMod(Res, A, B, C)
  else if BigNumberIsNegative(A) and BigNumberIsNegative(B) then
  begin
    T := FLocalBigNumberPool.Obtain;
    P := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(T, A);
      BigNumberCopy(P, B);
      BigNumberSetNegative(T, False);
      BigNumberSetNegative(P, False);
      Result := BigNumberUnsignedMulMod(Res, T, P, C);
    finally
      FLocalBigNumberPool.Recycle(T);
      FLocalBigNumberPool.Recycle(P);
    end;
  end
  else if BigNumberIsNegative(A) and not BigNumberIsNegative(B) then // A ��
  begin
    T := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(T, A);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, T, B, C);
      BigNumberSub(Res, C, Res);
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end
  else if not BigNumberIsNegative(A) and BigNumberIsNegative(B) then // B ��
  begin
    T := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(T, B);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, A, T, C);
      BigNumberSub(Res, C, Res);
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end
  else
    Result := False;
end;

// ���ټ��� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ�}
function BigNumberUnsignedMulMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  AA, BB: TCnBigNumber;
begin
  Result := False;
  if  Res = C then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  AA := nil;
  BB := nil;

  try
    // ʹ����ʱ��������֤ A��B �����ֵ�������仯
    AA := FLocalBigNumberPool.Obtain;
    BB := FLocalBigNumberPool.Obtain;

    BigNumberCopy(AA, A);
    BigNumberCopy(BB, B);
    BigNumberSetNegative(AA, False); // ȫ������
    BigNumberSetNegative(BB, False);

    if not BigNumberMod(AA, AA, C) then
      Exit;

    if not BigNumberMod(BB, BB, C) then
      Exit;

    Res.SetZero; // ��� Res �� A �� B���������������� AA �� BB���ı� A �� B ��Ӱ��

    while not BB.IsZero do
    begin
      if BigNumberIsBitSet(BB, 0) then
      begin
        if not BigNumberAdd(Res, Res, AA) then
          Exit;

        if not BigNumberMod(Res, Res, C) then
          Exit;
      end;

      if not BigNumberShiftLeftOne(AA, AA) then
        Exit;

      if BigNumberCompare(AA, C) >= 0 then
        if not BigNumberMod(AA, AA, C) then
          Exit;

      if not BigNumberShiftRightOne(BB, BB) then
        Exit;
    end;
  finally
    FLocalBigNumberPool.Recycle(AA);
    FLocalBigNumberPool.Recycle(BB);
  end;
  Result := True;
end;

{* ��ͨ���� (A * B) mod C�����ؼ����Ƿ�ɹ���Res ������ C��A��B��C ���ֲ��䣨��� Res ���� A��B �Ļ���}
function BigNumberDirectMulMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
begin
  Result := False;
  if A = B then
  begin
    if not BigNumberSqr(Res, A) then
      Exit;
  end
  else
  begin
    if not BigNumberMul(Res, A, B) then
      Exit;
  end;

  if Res = C then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  if not BigNumberNonNegativeMod(Res, Res, C) then
    Exit;
  Result := True;
end;

// �ɸ�����Լ�򷨿��ټ��� T * R^-1 mod N ����Ҫ�� R �Ǹպñ� N ��� 2 �������ݣ�
// NNegInv ��Ԥ�ȼ���õ� N �� R �ĸ�ģ��Ԫ��T ����Ϊ����С�� N * R
function BigNumberMontgomeryReduction(Res: TCnBigNumber;
  T, R, N, NNegInv: TCnBigNumber): Boolean;
var
  M: TCnBigNumber;
begin
  Result := False;
  M := nil;

  try
    M := FLocalBigNumberPool.Obtain;

    if not BigNumberMul(M, T, NNegInv) then // M := T * N'
      Exit;

    // M := T * N' mod R ��Ϊ R �� 2 ���ݣ����Կ��Կ��ٱ�����λ���õ��� M < R
    if not BigNumberKeepLowBits(M, R.GetBitsCount - 1) then
      Exit;

    // ���� M := (T + M * N) / R
    if not BigNumberMul(M, M, N) then
      Exit;

    if not BigNumberAdd(M, T, M) then
      Exit;

    // ��Ϊ R �� 2 ���ݣ����Կ��� M �����������������ҽ����Ϊ����
    if not BigNumberShiftRight(M, M, R.GetBitsCount - 1) then
      Exit;

    // M >= N ��� N
    if BigNumberCompare(M, N) >= 0 then
      Result := BigNumberSub(Res, M, N)
    else
      Result := BigNumberCopy(Res, M) <> nil;
  finally
    FLocalBigNumberPool.Recycle(M);
  end;
end;

// �ɸ����������ټ��� A * B mod N������Ҫ�� R �Ǹպñ� N ��� 2 �������ݣ�
// R2ModN ��Ԥ�ȼ���õ� R^2 mod N ��ֵ��NNegInv ��Ԥ�ȼ���õ� N �� R �ĸ�ģ��Ԫ
function BigNumberMontgomeryMulMod(Res: TCnBigNumber;
  A, B, R, R2ModN, N, NNegInv: TCnBigNumber): Boolean;
var
  AA, BB, RA, RB, M: TCnBigNumber;
begin
  Result := False;

  AA := nil;
  RA := nil;
  BB := nil;
  RB := nil;
  M := nil;

  try
    AA := FLocalBigNumberPool.Obtain;
    RA := FLocalBigNumberPool.Obtain;

    // AA := A * (R * R mod N) ������ N * R
    if not BigNumberMul(AA, A, R2ModN) then
      Exit;
    // �ɸ�������� RA := A*(R*R)*R^-1 mod N = A * R mod N
    if not BigNumberMontgomeryReduction(RA, AA, R, N, NNegInv) then
      Exit;

    BB := FLocalBigNumberPool.Obtain;
    RB := FLocalBigNumberPool.Obtain;

    // BB := B * (R * R mod N) ������ N * R
    if not BigNumberMul(BB, B, R2ModN) then
      Exit;
    // �ɸ�������� RB := B*(R*R)*R^-1 mod N = B * R mod N
    if not BigNumberMontgomeryReduction(RB, BB, R, N, NNegInv) then
      Exit;

    // M := (A*R * B*R) ������ N^2����Ϊ R �� N �󣬸�ȷ�� M < N * R
    M := FLocalBigNumberPool.Obtain;
    if not BigNumberMul(M, RA, RB) then
      Exit;

    // �ɸ�������� Res := (A*R * B*R) * R^-1 mod N = A*B*R mod N
    if not BigNumberMontgomeryReduction(Res, M, R, N, NNegInv) then
      Exit;

    // Res �м�ֵ�� M
    if BigNumberCopy(M, Res) = nil then
      Exit;

    // �ٴ��ɸ�������� A*B*R * R^-1 mod N = A*B mod N
    if not BigNumberMontgomeryReduction(Res, M, R, N, NNegInv) then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(M);
    FLocalBigNumberPool.Recycle(RB);
    FLocalBigNumberPool.Recycle(BB);
    FLocalBigNumberPool.Recycle(RA);
    FLocalBigNumberPool.Recycle(AA);
  end;
end;

// ���ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��C ֮һ���ڲ����� BigNumberPowerMod
function BigNumberPowerWordMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: Cardinal; C: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetWord(B);
    Result := BigNumberPowerMod(Res, A, T, C);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

// ���ټ��� (A ^ B) mod C�����ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ
function BigNumberPowerMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  I, J, Bits, WStart, WEnd, Window, WValue, Start: Integer;
  D: TCnBigNumber;
  Val: array[0..31] of TCnBigNumber;

  function WindowBit(B: Integer): Integer;
  begin
    if B > 671 then
      Result := 6
    else if B > 239 then
      Result := 5
    else if B > 79 then
      Result := 4
    else if B > 23 then
      Result := 3
    else
      Result := 1;
  end;

begin
  Result := False;
  if (Res = A) or (Res = B) or (Res = C) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  Bits := BigNumberGetBitsCount(B);

  if Bits = 0 then
  begin
    if BigNumberAbsIsWord(C, 1) then
      BigNumberSetZero(Res)
    else
      BigNumberSetOne(Res);
    Result := True;
    Exit;
  end;

  D := nil;
  for I := Low(Val) to High(Val) do
    Val[I] := nil;

  try
    Val[0] := FLocalBigNumberPool.Obtain;
    if not BigNumberNonNegativeMod(Val[0], A, C) then
      Exit;

    if BigNumberIsZero(Val[0]) then
    begin
      if not BigNumberSetZero(Res) then
        Exit;
      Result := True;
      Exit;
    end;

    Window := WindowBit(Bits);
    D := FLocalBigNumberPool.Obtain;
    if Window > 1 then
    begin
      if not BigNumberDirectMulMod(D, Val[0], Val[0], C) then
        Exit;

      J := 1 shl (Window - 1);
      for I := 1 to J - 1 do
      begin
        Val[I] := FLocalBigNumberPool.Obtain;
        if not BigNumberDirectMulMod(Val[I], Val[I - 1], D, C) then
          Exit;
      end;
    end;

    Start := 1;
    WStart := Bits - 1;

    if not BigNumberSetOne(Res) then
      Exit;

    while True do
    begin
      if not BigNumberIsBitSet(B, WStart) then
      begin
        if Start = 0 then
          if not BigNumberDirectMulMod(Res, Res, Res, C) then
            Exit;

        if WStart = 0 then
          Break;

        Dec(WStart);
        Continue;
      end;

      WValue := 1;
      WEnd := 0;
      for I := 1 to Window - 1 do
      begin
        if WStart - I < 0 then
          Break;

        if BigNumberIsBitSet(B, WStart - I) then
        begin
          WValue := WValue shl (I - WEnd);
          WValue := WValue or 1;
          WEnd := I;
        end;
      end;

      J := WEnd + 1;
      if Start = 0 then
      begin
        for I := 0 to J - 1 do
          if not BigNumberDirectMulMod(Res, Res, Res, C) then
            Exit;
      end;

      if not BigNumberDirectMulMod(Res, Res, Val[WValue shr 1], C) then
        Exit;

      WStart := WStart - WEnd - 1;
      Start := 0;
      if WStart < 0 then
        Break;
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(D);
    for I := Low(Val) to High(Val) do
      FLocalBigNumberPool.Recycle(Val[I]);
  end;
end;

// �ɸ����������ټ��� (A ^ B) mod C�������ؼ����Ƿ�ɹ���Res ������ A��B��C ֮һ
function BigNumberMontgomeryPowerMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  T, AA, BB: TCnBigNumber;
begin
  Result := False;
  if B.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end;

  if (Res = A) or (Res = B) or (Res = C) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  AA := nil;
  BB := nil;
  T := nil;

  try
    AA := FLocalBigNumberPool.Obtain;
    BB := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    if not T.SetOne then
      Exit;

    if not BigNumberMod(AA, A, C) then
      Exit;

    if BigNumberCopy(BB, B) = nil then
      Exit;

    while not BB.IsOne do
    begin
      if BigNumberIsBitSet(BB, 0) then
      begin
        if not BigNumberDirectMulMod(T, AA, T, C) then
          Exit;
      end;
      if not BigNumberDirectMulMod(AA, AA, AA, C) then
        Exit;

      if not BigNumberShiftRightOne(BB, BB) then
        Exit;
    end;

    if not BigNumberDirectMulMod(Res, AA, T, C) then
      Exit;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(AA);
    FLocalBigNumberPool.Recycle(BB);
  end;
  Result := True;
end;

function BigNumberPowerPowerMod(Res: TCnBigNumber; A, B, C, N: TCnBigNumber): Boolean;
var
  I, T: TCnBigNumber;
begin
  // A^(B^C) = A^(B*B*B*B...) �� C �� = ((A^B)^B)^B)^B �� C �� B
  if C.IsZero then
    Result := BigNumberCopy(Res, A) <> nil
  else if C.IsOne then
    Result := BigNumberPowerMod(Res, A, B, N)
  else
  begin
    if (Res = A) or (Res = B) or (Res = C) or (Res = N) then
      raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

    I := nil;
    T := nil;

    try
      Result := False;

      I := FLocalBigNumberPool.Obtain;
      I.SetZero;
      if BigNumberCopy(Res, A) = nil then
        Exit;

      T := FLocalBigNumberPool.Obtain;
      while BigNumberCompare(I, C) < 0 do
      begin
        if not BigNumberPowerMod(T, Res, B, N) then
          Exit;
        BigNumberCopy(Res, T);

        I.AddWord(1);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
      FLocalBigNumberPool.Recycle(I);
    end;
    Result := True;
  end;
end;

procedure CheckLog(Num: TCnBigNumber);
begin
  if Num.IsZero or Num.IsNegative then
    raise ERangeError.Create(SCnErrorBigNumberLogRange);
end;

function BigNumberLog2(Num: TCnBigNumber): Extended;
var
  F: Extended;
begin
  CheckLog(Num);
  if Num.IsOne then
    Result := 0
  else
  begin
    F := BigNumberGetFloat(Num);
    Result := Log2(F);
  end;
end;

function BigNumberLog10(Num: TCnBigNumber): Extended;
var
  F: Extended;
begin
  CheckLog(Num);
  if Num.IsOne then
    Result := 0
  else
  begin
    F := BigNumberGetFloat(Num);
    Result := Log10(F);
  end;
end;

function BigNumberLogN(Num: TCnBigNumber): Extended;
var
  F: Extended;
begin
  CheckLog(Num);
  if Num.IsOne then
    Result := 0
  else
  begin
    F := BigNumberGetFloat(Num);
    Result := Ln(F);
  end;
end;

function BigNumberFermatCheckComposite(A, B, C: TCnBigNumber; T: Integer): Boolean;
var
  I: Integer;
  R, L, S: TCnBigNumber;
begin
  Result := False;

  R := nil;
  L := nil;
  S := nil;

  try
    R := FLocalBigNumberPool.Obtain;
    if not BigNumberPowerMod(R, A, C, B) then
      Exit;

    L := FLocalBigNumberPool.Obtain;
    if BigNumberCopy(L, R) = nil then // L := R;
      Exit;

    S := FLocalBigNumberPool.Obtain;
    for I := 1 to T do
    begin
      if not BigNumberDirectMulMod(R, R, R, B) then
        Exit;
      // �� MulMod ��Ϊ DirectMulMod ���ж� 1024 λ������Ŵ� 1.6 ������ٵ� 1.4 ���

      if R.IsOne and not L.IsOne then
      begin
        BigNumberSub(S, B, L);
        if not S.IsOne then
        begin
          Result := True;
          Exit;
        end;
      end;

      if BigNumberCopy(L, R) = nil then
        Exit;
    end;

    Result := not R.IsOne;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(L);
    FLocalBigNumberPool.Recycle(S);
  end;
end;

// TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��
function BigNumberIsProbablyPrime(Num: TCnBigNumber; TestCount: Integer): Boolean;
var
  I, T: Integer;
  X, R, W: TCnBigNumber;
begin
  Result := False;
  if TestCount <= 1 then
    Exit;

  // �ų��� ������0��1 �Լ� 2 ֮���ż����
  if Num.IsZero or Num.IsNegative or Num.IsOne or (not Num.IsOdd and not BigNumberAbsIsWord(Num, 2))then
    Exit;

  // С�������ȶԱ��жϣ����� 2
  X := FLocalBigNumberPool.Obtain;
  try
    X.SetWord(CN_PRIME_NUMBERS_SQRT_UINT32[High(CN_PRIME_NUMBERS_SQRT_UINT32)]);
    if BigNumberCompare(Num, X) <= 0 then
    begin
      for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
      begin
        if BigNumberAbsIsWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I]) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(X);
  end;

  // ����С���������������� 2 �ˣ���Ϊ 2 ֮���ż���Ѿ����ų���
  for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) + 1 to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
  begin
    // 64 λģʽ�� BigNumberModWord ��֧�ֳ������� UInt32����������������ݷ���Ҫ��
    if BigNumberModWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I]) = 0 then
      Exit;
  end;

  // ��©���ˣ����� Miller-Rabin Test
  X := nil;
  R := nil;
  W := nil;

  try
    X := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    W := FLocalBigNumberPool.Obtain;

    if BigNumberCopy(X, Num) = nil then
      Exit;

    if not BigNumberSubWord(X, 1) then
      Exit;

    if BigNumberCopy(W, X) = nil then  // W := X := Num - 1;
      Exit;

    T := 0;
    while not X.IsOdd do // X and 1 = 0
    begin
      if not BigNumberShiftRightOne(X, X) then
        Exit;
      Inc(T);
    end;

    for I := 1 to TestCount do
    begin
      if not BigNumberRandRange(R, W) then
        Exit;

      if not BigNumberAddWord(R, 1) then
        Exit;

      if BigNumberFermatCheckComposite(R, Num, X, T) then
        Exit;
    end;
  finally
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(W);
  end;
  Result := True;
end;

function InternalGenerateProbablePrime(Num: TCnBigNumber; BitsCount: Integer): Boolean;
var
  Mods: array[0..BN_PRIME_NUMBERS - 1] of TCnBigNumberElement;
  Delta, MaxDelta: TCnBigNumberElement;
  I: Integer;
label
  AGAIN;
begin
  Result := False;

AGAIN:
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

  // 64 λģʽ�� BigNumberModWord ��֧�ֳ������� UInt32����������������ݷ���Ҫ��
  for I := 1 to BN_PRIME_NUMBERS - 1 do
    Mods[I] := BigNumberModWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I + 1]);

  MaxDelta := BN_MASK2 - CN_PRIME_NUMBERS_SQRT_UINT32[BN_PRIME_NUMBERS];
  Delta := 0;

  for I := 1 to BN_PRIME_NUMBERS - 1 do
  begin
    if ((Mods[I] + Delta) mod CN_PRIME_NUMBERS_SQRT_UINT32[I + 1]) <= 1 then
    begin
      Inc(Delta, 2);
      if Delta > MaxDelta then
        goto AGAIN;
      Continue;
    end;
  end;

  if not BigNumberAddWord(Num, Delta) then
    Exit;
  Result := True;
end;

// ����һ��ָ��λ���Ĵ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��
function BigNumberGeneratePrime(Num: TCnBigNumber; BytesCount: Integer;
  TestCount: Integer): Boolean;
begin
  Result := False;
  if not InternalGenerateProbablePrime(Num, BytesCount * 8) then
    Exit;

  while not BigNumberIsProbablyPrime(Num, TestCount) do
  begin
    if not InternalGenerateProbablePrime(Num, BytesCount * 8) then
      Exit;
  end;
  Result := True;
end;

// ����һ��ָ��������λ���Ĵ�������TestCount ָ Miller-Rabin �㷨�Ĳ��Դ�����Խ��Խ��ȷҲԽ��
function BigNumberGeneratePrimeByBitsCount(Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
begin
  Result := False;
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

  if not BigNumberSetBit(Num, BitsCount - 1) then
    Exit;

  if not Num.IsOdd then
    Num.AddWord(1);

  while not BigNumberIsProbablyPrime(Num, TestCount) do
    Num.AddWord(2);

  Result := True;
end;

function BigNumberNextPrime(Res, Num: TCnBigNumber;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
begin
  Result := True;
  if Num.IsNegative or Num.IsZero or Num.IsOne or (Num.GetWord = 2) then
  begin
    Res.SetWord(2);
    Exit;
  end
  else
  begin
    BigNumberCopy(Res, Num);
    if not Res.IsOdd then
      Res.AddWord(1);

    while not BigNumberIsProbablyPrime(Res, TestCount) do
      Res.AddWord(2);
  end;
end;

// �� R �Ƿ���� Prime - 1 ��ÿ�����ӣ����� R ^ (ʣ�����ӵĻ�) mod Prime <> 1
function BigNumberCheckPrimitiveRoot(R, Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
var
  I: Integer;
  Res, SubOne, T, Remain: TCnBigNumber;
begin
  Result := False;
  Res := FLocalBigNumberPool.Obtain;
  T := FLocalBigNumberPool.Obtain;
  Remain := FLocalBigNumberPool.Obtain;
  SubOne := FLocalBigNumberPool.Obtain;

  BigNumberCopy(SubOne, Prime);
  BigNumberSubWord(SubOne, 1);

  try
    for I := 0 to Factors.Count - 1 do
    begin
      BigNumberDiv(T, Remain, SubOne, Factors[I]);
      BigNumberMontgomeryPowerMod(Res, R, T, Prime);
      if Res.IsOne then
        Exit;
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Res);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(Remain);
    FLocalBigNumberPool.Recycle(SubOne);
  end;
end;

// ����һ������ԭ�������ؼ����Ƿ�ɹ�
function BigNumberGetMinRootFromPrime(Res, Prime: TCnBigNumber): Boolean;
var
  I: Integer;
  Num, PrimeSubOne: TCnBigNumber;
  Factors: TCnBigNumberList;
begin
  Result := False;
  PrimeSubOne := nil;
  Factors := nil;
  Num := nil;

  try
    PrimeSubOne := FLocalBigNumberPool.Obtain;
    BigNumberCopy(PrimeSubOne, Prime);
    BigNumberSubWord(PrimeSubOne, 1);

    Factors := TCnBigNumberList.Create;
    BigNumberFindFactors(PrimeSubOne, Factors);
    Factors.RemoveDuplicated;

    Num := FLocalBigNumberPool.Obtain;;
    Res.SetZero;
    for I := 2 to MaxInt do // ����̫��Ĵ���
    begin
      Num.SetWord(I);
      if BigNumberCheckPrimitiveRoot(Num, Prime, Factors) then
      begin
        Res.SetWord(I);
        Result := True;
        Exit;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(Num);
    Factors.Free;
    FLocalBigNumberPool.Recycle(PrimeSubOne);
  end;
end;

// �����Ƿ���һ�� 32 λ�з������ͷ�Χ�ڵ���
function BigNumberIsInt32(Num: TCnBigNumber): Boolean;
var
  C: Integer;
begin
  Result := False;

  C := Num.GetBitsCount;
  if C > BN_BITS_UINT_32 then // ����
    Exit;
  if C < BN_BITS_UINT_32 then // С�� 32 λ����
  begin
    Result := True;
    Exit;
  end;

  // 32 λ
  if Num.IsNegative then // ������С�� -$80000000 �򳬽�
  begin
    if not BigNumberIsBitSet(Num, BN_BITS_UINT_32 - 1) then
      Result := True  // ���λ��Ϊ 1��˵������ֵС�� $80000000
    else
    begin
      // ���λΪ 1������λ��Ҫȫ 0 ������ Int32
      for C := 0 to BN_BITS_UINT_32 - 2 do
        if BigNumberIsBitSet(Num, C) then // ֻҪ�и� 1 �ͱ�ʾ������
          Exit;
      Result := True;
    end;
  end
  else // ��������Ҫ�ж����λ�Ƿ��� 1���� 1 �򳬽磬Ҳ���Ǵ��� $7FFFFFFF
    Result := not BigNumberIsBitSet(Num, BN_BITS_UINT_32 - 1);
end;

// �����Ƿ���һ�� 32 λ�޷������ͷ�Χ�ڵ���
function BigNumberIsUInt32(Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_32);
end;

// �����Ƿ���һ�� 64 λ�з������ͷ�Χ�ڵ���
function BigNumberIsInt64(Num: TCnBigNumber): Boolean;
var
  C: Integer;
begin
  Result := False;

  C := Num.GetBitsCount;
  if C > BN_BITS_UINT_64 then // ����
    Exit;
  if C < BN_BITS_UINT_64 then // С�� 32 λ����
  begin
    Result := True;
    Exit;
  end;

  // 64 λ
  if Num.IsNegative then // ������С�� -$80000000 00000000 �򳬽�
  begin
    if not BigNumberIsBitSet(Num, BN_BITS_UINT_64 - 1) then
      Result := True  // ���λ��Ϊ 1��˵������ֵС�� $80000000 00000000
    else
    begin
      // ���λΪ 1������λ��Ҫȫ 0 ������ Int64
      for C := 0 to BN_BITS_UINT_64 - 2 do
        if BigNumberIsBitSet(Num, C) then // ֻҪ�и� 1 �ͱ�ʾ������
          Exit;
      Result := True;
    end;
  end
  else // ��������Ҫ�ж����λ�Ƿ��� 1���� 1 �򳬽磬Ҳ���Ǵ��� $7FFFFFFF
    Result := not BigNumberIsBitSet(Num, BN_BITS_UINT_64 - 1);
end;

// �����Ƿ���һ�� 64 λ�޷������ͷ�Χ�ڵ���
function BigNumberIsUInt64(Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_64);
end;

function BigNumberIsFloat(Num: TCnBigNumber): Boolean;
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
  begin
    // �ж��Ƿ����� 10 �ֽ���չ���ȷ�Χ
    Result := BigNumberGetBitsCount(Num) < CN_EXTENDED_MAX_EXPONENT;
  end
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
  begin
    // �ж��Ƿ����� 8 �ֽ�˫���ȷ�Χ
    Result := BigNumberGetBitsCount(Num) < CN_DOUBLE_MAX_EXPONENT;
  end;
end;

// ��չŷ�����շת��������Ԫһ�β������� A * X + B * Y = 1 ��������
procedure BigNumberExtendedEuclideanGcd(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
var
  T, P, M: TCnBigNumber;
begin
  if BigNumberIsZero(B) then
  begin
    BigNumberSetOne(X);
    BigNumberSetZero(Y);
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := FLocalBigNumberPool.Obtain;
      P := FLocalBigNumberPool.Obtain;
      M := FLocalBigNumberPool.Obtain;
      BigNumberMod(P, A, B);

      BigNumberExtendedEuclideanGcd(B, P, X, Y);
      BigNumberCopy(T, X);
      BigNumberCopy(X, Y);

      // �� CorrectTop ���� Top ֵ��̫��ԭ����
      BigNumberCorrectTop(X);
      BigNumberCorrectTop(Y);

      // T := X;
      // X := Y;
      // Y := T - (A div B) * Y;
      BigNumberDiv(P, M, A, B);
      BigNumberMul(P, P, Y);
      BigNumberSub(Y, T, P);
    finally
      FLocalBigNumberPool.Recycle(M);
      FLocalBigNumberPool.Recycle(P);
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

// ��չŷ�����շת��������Ԫһ�β������� A * X - B * Y = 1 ��������
procedure BigNumberExtendedEuclideanGcd2(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
var
  T, P, M: TCnBigNumber;
begin
  if BigNumberIsZero(B) then
  begin
    BigNumberSetOne(X);
    BigNumberSetZero(Y);
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := FLocalBigNumberPool.Obtain;
      P := FLocalBigNumberPool.Obtain;
      M := FLocalBigNumberPool.Obtain;
      BigNumberMod(P, A, B);

      BigNumberExtendedEuclideanGcd2(B, P, Y, X);

      // �� CorrectTop ���� Top ֵ��̫��ԭ����
      BigNumberCorrectTop(X);
      BigNumberCorrectTop(Y);

      // Y := Y - (A div B) * X;
      BigNumberDiv(P, M, A, B);
      BigNumberMul(P, P, X);
      BigNumberSub(Y, Y, P);
    finally
      FLocalBigNumberPool.Recycle(M);
      FLocalBigNumberPool.Recycle(P);
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

// �� X ��� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ�������������б�֤ X��Modulus ����
function BigNumberModularInverse(Res: TCnBigNumber; X, Modulus: TCnBigNumber;
  CheckGcd: Boolean): Boolean;
var
  Neg: Boolean;
  X1, Y: TCnBigNumber;
begin
  Result := False;
  if (Res = X) or (Res = Modulus) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  Neg := False;
  X1 := nil;
  Y := nil;

  try
    X1 := FLocalBigNumberPool.Obtain;
    Y := FLocalBigNumberPool.Obtain;

    if CheckGcd then
    begin
      BigNumberGcd(X1, X, Modulus);
      if not X1.IsOne then
        Exit;
    end;

    if BigNumberCopy(X1, X) = nil then
      Exit;

    if BigNumberIsNegative(X1) then
    begin
      BigNumberSetNegative(X1, False);
      Neg := True;
    end;

    // ��������ģ��Ԫ��������ģ��Ԫ����������ģ��Ԫ�ĸ�ֵ��������ĸ�ֵ�������ټ� Modulus
    BigNumberExtendedEuclideanGcd2(X1, Modulus, Res, Y);
    // ��չŷ�����շת��������Ԫһ�β������� A * X - B * Y = 1 ��������

    if Neg then
      BigNumberSetNegative(Res, not BigNumberIsNegative(Res));

    if BigNumberIsNegative(Res) then
      if not BigNumberAdd(Res, Res, Modulus) then
        Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(X1);
  end;
end;

{* �� X ������� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ��
   �����������б�֤ Modulus Ϊ�������� Res ������ X �� Modulus}
function BigNumberPrimeModularInverse(Res: TCnBigNumber; X, Modulus: TCnBigNumber): Boolean;
var
  P: TCnBigNumber;
begin
  if (Res = X) or (Res = Modulus) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  // �ɷ���С����֪ x^(p-1) = 1 mod p������ x ����Ԫ�� x^(p-2) mod p
  P := FLocalBigNumberPool.Obtain;
  try
    BigNumberCopy(P, Modulus);
    P.SubWord(2);
    Result := BigNumberPowerMod(Res, X, P, Modulus);
  finally
    FLocalBigNumberPool.Recycle(P);
  end;
end;

// �� X ��� Modulus �ĸ�ģ����и�ģ��Ԫ Y������ (X * Y) mod M = -1��X ��Ϊ��ֵ��Y �����ֵ
function BigNumberNegativeModularInverse(Res: TCnBigNumber;
  X, Modulus: TCnBigNumber; CheckGcd: Boolean): Boolean;
begin
  Result := BigNumberModularInverse(Res, X, Modulus, CheckGcd);
  if Result then
    Result := BigNumberSub(Res, Modulus, Res); // ����Ԫ����ģ������Ԫ
end;

// �� 32 λ�з����� X ��� Modulus ��ģ�����ģ��Ԫ Y������ (X * Y) mod M = 1��X ��Ϊ��ֵ��Y �����ֵ
procedure BigNumberModularInverseWord(Res: TCnBigNumber; X: Integer;
  Modulus: TCnBigNumber; CheckGcd: Boolean);
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInteger(X);
    BigNumberModularInverse(Res, T, Modulus, CheckGcd);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

// �ö��λ����ɵݹ�������õ·��� ( A / P) ��ֵ���Ͽ�
function BigNumberLegendre(A, P: TCnBigNumber): Integer;
var
  AA, Q: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise ECnBigNumberException.Create(SCnErrorBigNumberLegendre);

  if A.IsOne then
  begin
    Result := 1;
    Exit;
  end;

  AA := FLocalBigNumberPool.Obtain;
  Q := FLocalBigNumberPool.Obtain;

  try
    if A.IsOdd then
    begin
      // ����
      BigNumberMod(AA, P, A);
      Result := BigNumberLegendre(AA, A);

      // ���� (A-1)*(P-1)/4 �� -1 ���
      BigNumberSub(AA, A, CnBigNumberOne);
      BigNumberSub(Q, P, CnBigNumberOne);
      BigNumberMul(Q, AA, Q);
      BigNumberShiftRight(Q, Q, 2);

      if Q.IsOdd then // ������ -1 �˻��ǵ� -1
        Result := -Result;
    end
    else
    begin
      // ż��
      BigNumberShiftRight(AA, A, 1);
      Result := BigNumberLegendre(AA, P);

      // ���� (P^2 - 1)/8 �� -1 ���
      BigNumberMul(Q, P, P);
      BigNumberSubWord(Q, 1);
      BigNumberShiftRight(Q, Q, 3);

      if Q.IsOdd then // ������ -1 �˻��ǵ� -1
        Result := -Result;
    end;
  finally
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(AA);
  end;
end;

// ��ŷ���б𷨼������õ·��� ( A / P) ��ֵ������
function BigNumberLegendre2(A, P: TCnBigNumber): Integer;
var
  R, Res: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise ECnBigNumberException.Create(SCnErrorBigNumberLegendre);

  R := FLocalBigNumberPool.Obtain;
  Res := FLocalBigNumberPool.Obtain;

  try
    // ���������P ������ A ʱ���� 0����������ʱ����� A ����ȫƽ�����ͷ��� 1�����򷵻� -1
    BigNumberMod(R, A, P);
    if R.IsZero then
      Result := 0
    else
    begin
      BigNumberCopy(R, P);
      BigNumberSubWord(R, 1);
      BigNumberShiftRightOne(R, R);
      BigNumberMontgomeryPowerMod(Res, A, R, P);

      if Res.IsOne then // ŷ���б�
        Result := 1
      else
        Result := -1;
    end;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(Res);
  end;
end;

// ʹ�� Tonelli Shanks �㷨����ģ��������ʣ����⣬�����������б�֤ P Ϊ���������������������η�
function BigNumberTonelliShanks(Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
var
  Q, Z, C, R, T, N, L, U, B: TCnBigNumber;
  S, I, M: Integer;
begin
  Result := False;
  if (Res = nil) or A.IsZero or A.IsNegative or P.IsZero or P.IsNegative
    or (BigNumberCompare(A, P) >= 0) then
    Exit;

  // ������õ·��Ų�Ϊ 1��˵���޽⣬����Ͳ�������
  if BigNumberLegendre(A, P) <> 1 then
    Exit;

  Q := FLocalBigNumberPool.Obtain;
  Z := FLocalBigNumberPool.Obtain;
  C := FLocalBigNumberPool.Obtain;
  R := FLocalBigNumberPool.Obtain;
  T := FLocalBigNumberPool.Obtain;
  L := FLocalBigNumberPool.Obtain;
  U := FLocalBigNumberPool.Obtain;
  B := FLocalBigNumberPool.Obtain;
  N := FLocalBigNumberPool.Obtain;

  try
    S := 0;
    BigNumberSub(Q, P, CnBigNumberOne);
    while not Q.IsOdd do
    begin
      BigNumberShiftRightOne(Q, Q);
      Inc(S);
    end;

    // ����һ�� Z ���� ��� P �����õ·���Ϊ -1
    Z.SetWord(2);
    while BigNumberCompare(Z, P) < 0 do
    begin
      if BigNumberLegendre(Z, P) = -1 then
        Break;
      BigNumberAddWord(Z, 1);
    end;

    BigNumberAdd(N, Q, CnBigNumberOne);
    BigNumberShiftRight(N, N, 1);
    BigNumberMontgomeryPowerMod(C, Z, Q, P);
    BigNumberMontgomeryPowerMod(R, A, N, P);
    BigNumberMontgomeryPowerMod(T, A, Q, P);
    M := S;

    while True do
    begin
      BigNumberMod(U, T, P);
      if U.IsOne then
        Break;

      for I := 1 to M - 1 do
      begin
        U.SetOne;
        BigNumberShiftLeft(U, U, I);
        BigNumberMontgomeryPowerMod(N, T, U, P);
        if N.IsOne then
          Break;
      end;

      U.SetOne;
      BigNumberShiftLeft(U, U, M - I - 1);
      BigNumberMontgomeryPowerMod(B, C, U, P);
      M := I;
      BigNumberDirectMulMod(R, R, B, P);

      // T := T * B * B mod P = (T * B mod P) * (B mod P) mod P
      BigNumberDirectMulMod(U, T, B, P); // U := T * B mod P
      BigNumberMod(L, B, P);       // L := B mod P
      BigNumberDirectMulMod(T, U, L, P);

      BigNumberDirectMulMod(C, B, B, P);
    end;

    BigNumberMod(L, R, P);
    BigNumberAdd(L, L, P);
    BigNumberMod(Res, L, P);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(Z);
    FLocalBigNumberPool.Recycle(C);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(L);
    FLocalBigNumberPool.Recycle(U);
    FLocalBigNumberPool.Recycle(B);
    FLocalBigNumberPool.Recycle(N);
  end;
end;

// ʹ�� IEEE P1363 �淶�е� Lucas ���н���ģ��������ʣ�����
function BigNumberLucas(Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
var
  G, X, Z, U, V, T: TCnBigNumber;
begin
  Result := False;

  G := nil;
  X := nil;
  Z := nil;
  U := nil;
  V := nil;
  T := nil;

  try
    G := FLocalBigNumberPool.Obtain;
    X := FLocalBigNumberPool.Obtain;
    Z := FLocalBigNumberPool.Obtain;
    U := FLocalBigNumberPool.Obtain;
    V := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    while True do
    begin
      if not BigNumberRandRange(X, P) then
        Exit;

      BigNumberCopy(T, P);
      BigNumberAddWord(T, 1);
      BigNumberShiftRight(T, T, 1);
      if not BigNumberLucasVSequenceMod(X, A, T, P, U, V) then
        Exit;

      BigNumberCopy(Z, V);
      if not V.IsOdd then
      begin
        BigNumberShiftRight(Z, Z, 1);
        BigNumberMod(Z, Z, P);
      end
      else
      begin
        BigNumberAdd(Z, Z, P);
        BigNumberShiftRight(Z, Z, 1);
      end;

      if not BigNumberDirectMulMod(T, Z, Z, P) then
        Exit;
      T.SetNegative(False); // ���Է���

      if BigNumberCompare(T, A) = 0 then
      begin
        BigNumberCopy(Res, Z);
        Result := True;
        Exit;
      end
      else if BigNumberCompare(U, CnBigNumberOne) > 0 then
      begin
        BigNumberCopy(T, P);
        BigNumberSubWord(T, 1);

        if BigNumberCompare(U, T) < 0 then
          Break;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(G);
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(Z);
    FLocalBigNumberPool.Recycle(U);
    FLocalBigNumberPool.Recycle(V);
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberSquareRootModPrime(Res: TCnBigNumber; A, Prime: TCnBigNumber): Boolean;
var
  PrimeType: TCnPrimeType;
  Rem: TCnBigNumberElement;
  T, U, X, Y, Z, OldU, R: TCnBigNumber;
begin
  Result := False;
  if Prime.IsZero then
    Exit;

  if A.IsZero then // 0 ��ƽ�� mod P = 0
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  U := nil;
  OldU := nil;
  X := nil;
  Y := nil;
  Z := nil;
  R := nil;
  T := nil;

  try
    U := FLocalBigNumberPool.Obtain;
    BigNumberCopy(U, Prime);

    // Mod 4 �� 8 �Ż�Ϊֱ��ȡ�� 2 λ�� 3 λ
    Rem := BigNumberGetLow32(Prime) and 3;
    if Rem = 3 then
    begin
      PrimeType := pt4U3;
      BigNumberDivWord(U, 4);
    end
    else
    begin
      Rem := BigNumberGetLow32(Prime) and 7;
      if Rem = 1 then
        PrimeType := pt8U1
      else if Rem = 5 then
        PrimeType := pt8U5
      else
        Exit;
      BigNumberDivWord(U, 8);
    end;

    OldU := FLocalBigNumberPool.Obtain;
    BigNumberCopy(OldU, U); // ����һ�� U

    X := FLocalBigNumberPool.Obtain;
    Y := FLocalBigNumberPool.Obtain;
    Z := FLocalBigNumberPool.Obtain;

    // �õ��� Prime �����������Լ����� 4 �� 8 ��� U
    case PrimeType of
      pt4U3:
        begin
          // ����� g^(u+1) mod p
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Y, A, U, Prime);
          // �������������ƽ����ȥ mod Prime ��õ� -A ������ԭҪ�������Ҫ����

          BigNumberDirectMulMod(Z, Y, Y, Prime);
          if BigNumberCompare(Z, A) = 0 then
          begin
            BigNumberCopy(Res, Y);
            Result := True;
            Exit;
          end;
        end;
      pt8U1:
        begin
          if BigNumberLucas(Res, A, Prime) then
            Result := True;
        end;
      pt8U5:
        begin
          BigNumberMulWord(U, 2);
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Z, A, U, Prime);

          R := FLocalBigNumberPool.Obtain;
          BigNumberMod(R, Z, Prime);

          if R.IsOne then
          begin
            // ����� g^(u+1) mod p
            BigNumberCopy(U, OldU);
            BigNumberAddWord(U, 1);
            BigNumberMontgomeryPowerMod(Y, A, U, Prime);

            BigNumberCopy(Res, Y);
            Result := True;
          end
          else
          begin
            if R.IsNegative then
              BigNumberAdd(R, R, Prime);
            BigNumberSub(R, Prime, R);

            if R.IsOne then
            begin
              // �����(2g ��(4g)^u) mod p = (2g mod p * (4g)^u mod p) mod p
              BigNumberCopy(X, A);
              BigNumberMulWord(X, 2);
              BigNumberMod(R, X, Prime);  // R: 2g mod p

              BigNumberCopy(X, A);
              BigNumberMulWord(X, 4);

              T := FLocalBigNumberPool.Obtain;
              BigNumberMontgomeryPowerMod(T, X, OldU, Prime); // T: (4g)^u mod p
              BigNumberMulMod(Y, R, T, Prime);

              BigNumberCopy(Res, Y);
              Result := True;
            end;
          end;
        end;
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(Z);
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(OldU);
    FLocalBigNumberPool.Recycle(U);
  end;
end;

function BigNumberJacobiSymbol(A: TCnBigNumber; N: TCnBigNumber): Integer;
var
  R: Integer;
  AA, NN: TCnBigNumber;
  ANeg: Boolean;
begin
  if N.IsEven then        // N ż����֧��
    raise ECnBigNumberException.Create(SCnErrorBigNumberJacobiSymbol);

  if A.IsZero then
  begin
    Result := 0;
    Exit;
  end
  else if A.IsOne then
  begin
    Result := 1;
    Exit;
  end
  else if A.IsNegOne then // (-1, N) = (-1)^((N - 1)/2)
  begin
    NN := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(NN, N);
      NN.SubWord(1);
      BigNumberShiftRightOne(NN, NN);

      if NN.IsEven then
        Result := 1
      else
        Result := -1;
      Exit;
    finally
      FLocalBigNumberPool.Recycle(NN);
    end;
  end
  else if A.IsWord(2) then
  begin
    R := BigNumberGetLow32(N) and 7;
    if (R = 1) or (R = 7) then
      Result := 1
    else
      Result := -1;
    Exit;
  end;

  AA := nil;
  NN := nil;

  try
    AA := FLocalBigNumberPool.Obtain;
    BigNumberCopy(AA, A);

    ANeg := AA.IsNegative;  // �����ȷ�ת����¼
    if ANeg then
      AA.Negate;

    NN := FLocalBigNumberPool.Obtain;
    BigNumberCopy(NN, N);
    if NN.IsNegative then  // ģ����Ϊ��������ֱ��ת��
      NN.Negate;

    // A ̫����� mod N
    if BigNumberCompare(AA, NN) > 0 then
      BigNumberMod(AA, AA, NN);

    Result := 1;
    while not AA.IsZero do
    begin
      // A �� N С������Լ������
      R := BigNumberGetLow32(NN) and 7;
      while not AA.IsOdd do
      begin
        BigNumberShiftRightOne(AA, AA);
        if (R = 3) or (R = 5) then
          Result := -Result;
      end;
      BigNumberSwap(AA, NN);

      // ���λ���
      if ((BigNumberGetLow32(AA) and 3) = 3) and ((BigNumberGetLow32(NN) and 3) = 3) then // mod 4
        Result := -Result;

      // ��λ����ɱ��ֶ��λ�����׼����һ��
      BigNumberMod(AA, AA, NN);
    end;

    if not NN.IsOne then // N ��Ϊ 1 ˵��������
      Result := 0;

    // ԭʼ A Ϊ����Ҫ����һ�� -1 ���ſɱȷ��ţ�ģ��Ҫ������
    if ANeg and (Result <> 0) then
    begin
      if N.IsNegative then
      begin
        BigNumberCopy(NN, N);
        NN.Negate;

        // ���ԭʼ N Ϊ�������ó���һ������
        Result := -Result * BigNumberJacobiSymbol(CnBigNumberNegOne, NN); // N Ϊ����NN Ϊ��
      end
      else
        Result := Result * BigNumberJacobiSymbol(CnBigNumberNegOne, N);   // N Ϊ��
    end;
  finally
    FLocalBigNumberPool.Recycle(NN);
    FLocalBigNumberPool.Recycle(AA);
  end;
end;

procedure BigNumberPollardRho(X: TCnBigNumber; C: TCnBigNumber; Res: TCnBigNumber);
var
  I, K, X0, Y0, Y, D, X1, R: TCnBigNumber;
begin
  I := nil;
  K := nil;
  X0 := nil;
  X1 := nil;
  Y0 := nil;
  Y := nil;
  D := nil;
  R := nil;

  try
    I := FLocalBigNumberPool.Obtain;
    K := FLocalBigNumberPool.Obtain;
    X0 := FLocalBigNumberPool.Obtain;
    X1 := FLocalBigNumberPool.Obtain;
    Y0 := FLocalBigNumberPool.Obtain;
    Y := FLocalBigNumberPool.Obtain;
    D := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;

    I.SetOne;
    K.SetZero;
    BigNumberAddWord(K, 2);
    BigNumberCopy(X1, X);
    BigNumberSubWord(X1, 1);
    BigNumberRandRange(X0, X1);
    BigNumberAddWord(X1, 1);
    BigNumberCopy(Y, X0);

    while True do
    begin
      BigNumberAddWord(I, 1);

      BigNumberDirectMulMod(R, X0, X0, X);
      BigNumberAdd(R, R, C);
      BigNumberMod(X0, R, X);

      BigNumberSub(Y0, Y, X0);
      BigNumberGcd(D, Y0, X);

      if not D.IsOne and (BigNumberCompare(D, X) <> 0) then
      begin
        BigNumberCopy(Res, D);
        Exit;
      end;

      if BigNumberCompare(Y, X0) = 0 then
      begin
        BigNumberCopy(Res, X);
        Exit;
      end;

      if BigNumberCompare(I, K) = 0 then
      begin
        BigNumberCopy(Y, X0);
        BigNumberMulWord(K, 2);
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(I);
    FLocalBigNumberPool.Recycle(K);
    FLocalBigNumberPool.Recycle(X0);
    FLocalBigNumberPool.Recycle(X1);
    FLocalBigNumberPool.Recycle(Y0);
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(D);
  end;
end;

// �ҳ����������������б�
procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
var
  P, R, S, D, T: TCnBigNumber;
begin
  if Num.IsZero or Num.IsNegative or Num.IsOne then
    Exit;

  if BigNumberIsProbablyPrime(Num) then
  begin
    Factors.Add(BigNumberDuplicate(Num));
    Exit;
  end;

  P := nil;
  R := nil;
  S := nil;
  D := nil;
  T := nil;

  try
    P := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    S := FLocalBigNumberPool.Obtain;
    D := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    BigNumberCopy(P, Num);

    while BigNumberCompare(P, Num) >= 0 do
    begin
      BigNumberCopy(S, Num);
      BigNumberSubWord(S, 1);
      BigNumberRandRange(R, S);
      BigNumberAddWord(R, 1);
      BigNumberPollardRho(P, R, P);
    end;

    BigNumberFindFactors(P, Factors);
    T := FLocalBigNumberPool.Obtain;
    BigNumberDiv(T, R, Num, P);
    BigNumberFindFactors(T, Factors);
  finally
    FLocalBigNumberPool.Remove(T);
    FLocalBigNumberPool.Recycle(D);
    FLocalBigNumberPool.Recycle(S);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(P);
  end;
end;

procedure BigNumberFindAllFactors(Num: TCnBigNumber; AllFactors: TCnBigNumberList);
var
  I, J, L: Integer;
  P, PV, TN: TCnBigNumber;
  F, EF, EP, T: TCnBigNumberList;
  EC: TCnIntegerList;

  procedure GeneratePowerValues(P: TCnBigNumber; MaxExp: Integer; Powers: TCnBigNumberList);
  var
    K: Integer;
    CurrentPower, P2: TCnBigNumber;
  begin
    Powers.Clear;
    CurrentPower := Powers.Add;
    CurrentPower.SetOne; // P^0

    for K := 1 to MaxExp do
    begin
      P2 := Powers.Add;
      BigNumberMul(P2, CurrentPower, P);
      CurrentPower := P2;
    end;
  end;

begin
  if Num.IsZero then
    Exit;

  if Num.IsOne then
  begin
    AllFactors.Add(1);
    Exit;
  end
  else if BigNumberIsProbablyPrime(Num) then
  begin
    AllFactors.Add(1);
    AllFactors.Add(BigNumberDuplicate(Num));
    Exit;
  end;

  F := nil;
  EF := nil;
  EC := nil;
  EP := nil;

  try
    F := TCnBigNumberList.Create;
    BigNumberFindFactors(Num, F);   // �Ȼ�ȡ�������б����ظ�δ�����

    F.BigNumberSort;
    EF := TCnBigNumberList.Create;
    EC := TCnIntegerList.Create;

    for I := 0 to F.Count - 1 do
    begin
      P := F[I];
      J := EF.IndexOfValue(P);     // ���� P �� Factors �е�����
      if J = -1 then               // �� P �������� Factors
      begin
        EF.Add(BigNumberDuplicate(P));                 // �����������
        EC.Add(1);                 // ��ʼ������Ϊ 1
      end
      else
        EC[J] := EC[J] + 1;        // �����ڣ������� 1
    end;

    // �� EF �� EC �б��еõ����ظ����������Ӽ����Ӧ������
    AllFactors.Add(1);
    EP := TCnBigNumberList.Create;

    for I := 0 to EF.Count - 1 do
    begin
      P := EF[I];
      GeneratePowerValues(P, EC[I], EP);

      // ������ϣ��ѽ������ AllFactors �б���
      T := TCnBigNumberList.Create;
      try
        // ������ǰ���е�����
        for J := 0 to AllFactors.Count - 1 do
        begin
          // ������ǰ�����ӵ������ݴ�
          for L := 0 to EP.Count - 1 do
          begin
            PV := EP[L];
            // ��������������ӵ���ʱ�б�
            TN := T.Add;
            BigNumberMul(TN, AllFactors[J], PV);
          end;
        end;

        // �������ɵ������滻ԭ�е��б�
        AllFactors.Clear;
        AllFactors.AddList(T);
      finally
        T.Free;
      end;
    end;

    AllFactors.BigNumberSort;
  finally
    EP.Free;
    EC.Free;
    EF.Free;
    F.Free;
  end;
end;

procedure BigNumberEuler(Res: TCnBigNumber; Num: TCnBigNumber);
var
  F: TCnBigNumberList;
  T: TCnBigNumber;
  I: Integer;
begin
  // ���� Num �Ĳ��ظ����������ӣ������ù�ʽ Num * (1- 1/p1) * (1- 1/p2) ����
  F := nil;
  T := nil;

  try
    F := TCnBigNumberList.Create;
    BigNumberFindFactors(Num, F);

    // �ֹ�ȥ��
    F.RemoveDuplicated;

    BigNumberCopy(Res, Num);
    for I := 0 to F.Count - 1 do
      BigNumberDiv(Res, nil, Res, F[I]);

    T := FLocalBigNumberPool.Obtain;
    for I := 0 to F.Count - 1 do
    begin
      BigNumberCopy(T, F[I]);
      T.SubWord(1);
      BigNumberMul(Res, Res, T);
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    F.Free;
  end;
end;

// ���� Lucas �� U ����
function BigNumberLucasUSequenceMod(P: TCnBigNumber; Q: TCnBigNumber; K: TCnBigNumber;
  N: TCnBigNumber; U: TCnBigNumber): Boolean;
var
  C, I: Integer;
  U0, U1, V0, V1, Q0, Q1, T0, T1: TCnBigNumber;
begin
  Result := False;
  if K.IsNegative then
    Exit;

  if K.IsZero then
  begin
    U.SetZero;
    Exit;
  end
  else if K.IsOne then
  begin
    U.SetOne;
    Exit;
  end;

  C := BigNumberGetBitsCount(K); // ��Чλ������������� C - 1 λ�� 0 λѭ��
  if C < 1 then
    Exit;

  U0 := nil;
  U1 := nil;
  V0 := nil;
  V1 := nil;
  Q0 := nil;
  Q1 := nil;
  T0 := nil;
  T1 := nil;

  try
    U0 := FLocalBigNumberPool.Obtain;
    U1 := FLocalBigNumberPool.Obtain;
    V0 := FLocalBigNumberPool.Obtain;
    V1 := FLocalBigNumberPool.Obtain;
    Q0 := FLocalBigNumberPool.Obtain;
    Q1 := FLocalBigNumberPool.Obtain;
    T0 := FLocalBigNumberPool.Obtain;
    T1 := FLocalBigNumberPool.Obtain;

    U0.SetZero;            // U_0 = 0
    U1.SetOne;             // U_1 = 1
    V0.SetInteger(2);      // V_0 = 2
    BigNumberCopy(V1, P);  // V_1 = P
    Q0.SetOne;
    Q1.SetOne;

    for I := C - 1 downto 0 do
    begin
      if not BigNumberDirectMulMod(Q0, Q0, Q1, N) then Exit;

      if BigNumberIsBitSet(K, I) then
      begin
        if not BigNumberDirectMulMod(Q1, Q0, Q, N) then Exit;

        if not BigNumberDirectMulMod(T0, U1, V0, N) then Exit; // U_{2h+1} = U_{h+1} * V_h - Q^h * P
        if not BigNumberDirectMulMod(T1, Q0, P, N) then Exit;
        if not BigNumberSubMod(U0, T0, T1, N) then Exit;

        if not BigNumberDirectMulMod(T0, V1, V0, N) then Exit; // V_{2h+1} = V_{h+1} * V_h - Q^h * P
        if not BigNumberSubMod(V0, T0, T1, N) then Exit;       // T1 = Q^h * P �Ѽ���

        if not BigNumberDirectMulMod(U1, U1, V1, N) then Exit; // U_{2h+2} = U_{h+1} * V_{h+1}

        if not BigNumberDirectMulMod(T0, V1, V1, N) then Exit; // V_{2h+2} = V_{h+1}^2 - 2Q^{h+1}
        BigNumberCopy(T1, Q1);
        if not BigNumberShiftLeftOne(T1, T1) then Exit;
        if not BigNumberMod(T1, T1, N) then Exit;
        if not BigNumberSubMod(V1, T0, T1, N) then Exit;
      end
      else
      begin
        BigNumberCopy(Q1, Q0);

        if not BigNumberDirectMulMod(T0, U1, V0, N) then Exit; // U_{2h+1} = U_h+1 * V_h - Q^h
        if not BigNumberSubMod(U1, T0, Q0, N) then Exit;

        if not BigNumberDirectMulMod(T0, V1, V0, N) then Exit; // V_{2h+1} = V_h * V_{h+1} - Q^h * P
        if not BigNumberDirectMulMod(T1, Q0, P, N) then Exit;
        if not BigNumberSubMod(V1, T0, T1, N) then Exit;

        if not BigNumberDirectMulMod(U0, U0, V0, N) then Exit; // U_{2h} = U_h * V_h

        if not BigNumberDirectMulMod(T0, V0, V0, N) then Exit; // V_{2h} = V_{h}^2 - 2Q^{h}
        BigNumberCopy(T1, Q0);
        if not BigNumberShiftLeftOne(T1, T1) then Exit;
        if not BigNumberMod(T1, T1, N) then Exit;
        if not BigNumberSubMod(V0, T0, T1, N) then Exit;
      end;
    end;

    BigNumberCopy(U, U0);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T1);
    FLocalBigNumberPool.Recycle(T0);
    FLocalBigNumberPool.Recycle(Q1);
    FLocalBigNumberPool.Recycle(Q0);
    FLocalBigNumberPool.Recycle(V1);
    FLocalBigNumberPool.Recycle(V0);
    FLocalBigNumberPool.Recycle(U1);
    FLocalBigNumberPool.Recycle(U0);
  end;
end;

// ���� IEEE P1363 �Ĺ淶��˵���� Lucas �� V ����
function BigNumberLucasVSequenceMod(X, Y, K, N: TCnBigNumber; Q, V: TCnBigNumber): Boolean;
var
  C, I: Integer;
  V0, V1, Q0, Q1, T0, T1, C2: TCnBigNumber;
begin
  Result := False;
  if K.IsNegative then
    Exit;

  if K.IsZero then
  begin
    Q.SetOne;
    V.SetWord(2);
    Result := True;
    Exit;
  end
  else if K.IsOne then
  begin
    Q.SetOne;
    BigNumberCopy(V, X);
    Result := True;
    Exit;
  end;

  C := BigNumberGetBitsCount(K);  // ��Чλ������������� C - 1 λ�� 0 λѭ��
  if C < 1 then
    Exit;

  V0 := nil;
  V1 := nil;
  Q0 := nil;
  Q1 := nil;
  T0 := nil;
  T1 := nil;
  C2 := nil;

  try
    V0 := FLocalBigNumberPool.Obtain;
    V1 := FLocalBigNumberPool.Obtain;
    Q0 := FLocalBigNumberPool.Obtain;
    Q1 := FLocalBigNumberPool.Obtain;
    T0 := FLocalBigNumberPool.Obtain;
    T1 := FLocalBigNumberPool.Obtain;
    C2 := FLocalBigNumberPool.Obtain;

    C2.SetWord(2);
    V0.SetWord(2);
    BigNumberCopy(V1, X);
    Q0.SetOne;
    Q1.SetOne;

    for I := C - 1 downto 0 do
    begin
      if not BigNumberDirectMulMod(Q0, Q0, Q1, N) then
        Exit;

      if BigNumberIsBitSet(K, I) then
      begin
        if not BigNumberDirectMulMod(Q1, Q0, Y, N) then
          Exit;

        if not BigNumberDirectMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V0, T0, N) then
          Exit;

        if not BigNumberDirectMulMod(T0, V1, V1, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, C2, Q1, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;
      end
      else
      begin
        BigNumberCopy(Q1, Q0);

        if not BigNumberDirectMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;

        if not BigNumberDirectMulMod(T0, V0, V0, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, C2, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V0, T0, N) then
          Exit;
      end;
    end;

    BigNumberCopy(Q, Q0);
    BigNumberCopy(V, V0);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(V0);
    FLocalBigNumberPool.Recycle(V1);
    FLocalBigNumberPool.Recycle(Q0);
    FLocalBigNumberPool.Recycle(Q1);
    FLocalBigNumberPool.Recycle(T0);
    FLocalBigNumberPool.Recycle(T1);
    FLocalBigNumberPool.Recycle(C2);
  end;
end;

// ���й�ʣ�ඨ�����������뻥�صĳ�����һԪ����ͬ�෽�������С�⣬��������Ƿ�ɹ�
function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers, Factors: TCnBigNumberList): Boolean;
var
  I, J: Integer;
  G, N, Sum: TCnBigNumber;
begin
  Result := False;
  if (Remainers.Count <> Factors.Count) or (Remainers.Count = 0) then
    Exit;

  Sum := nil;
  G := nil;
  N := nil;

  try
    Sum := FLocalBigNumberPool.Obtain;
    G := FLocalBigNumberPool.Obtain;
    N := FLocalBigNumberPool.Obtain;

    BigNumberSetZero(Sum);
    for I := 0 to Remainers.Count - 1 do
    begin
      // ����ÿһ�������Ͷ�Ӧ�������ҳ����������Ĺ������г��Ըó����� 1 �������漰��ģ��Ԫ����
      // �� 5 7 �Ĺ����� 35n���� 3 �� 1 ���� 70��3 7 �� 5 �� 1 ���� 21��3 5 �� 7 �� 1 ���� 14
      // Ȼ��������͸�ģ��Ԫ���
      // ���еĳ˻���������mod һ��ȫ������ǵ���С���������͵õ������

      G.SetOne;
      for J := 0 to Factors.Count - 1 do
        if J <> I then
          if not BigNumberMul(G, G, Factors[J]) then
            Exit;

      // G �˿�����С����������Ϊ Factors ����
      // �� X ��� M ��ģ��Ԫ��Ҳ����ģ��Ԫ Y������ (X * Y) mod M = 1
      BigNumberModularInverse(N, G, Factors[I]);

      if not BigNumberMul(G, N, G) then // �õ�����
        Exit;

      if not BigNumberMul(G, Remainers[I], G) then // �������������
        Exit;

      if not BigNumberAdd(Sum, Sum, G) then // ���
        Exit;
    end;

    G.SetOne;
    for J := 0 to Factors.Count - 1 do
      if not BigNumberMul(G, G, Factors[J]) then
        Exit;

    Result := BigNumberNonNegativeMod(Res, Sum, G);
  finally
    FLocalBigNumberPool.Recycle(N);
    FLocalBigNumberPool.Recycle(G);
    FLocalBigNumberPool.Recycle(Sum);
  end;
end;

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers, Factors: TCnInt64List): Boolean; overload;
var
  I: Integer;
  BR, BF: TCnBigNumberList;
begin
  BR := nil;
  BF := nil;

  try
    BR := TCnBigNumberList.Create;
    BF := TCnBigNumberList.Create;

    for I := 0 to Remainers.Count - 1 do
      BR.Add.SetInt64(Remainers[I]);

    for I := 0 to Factors.Count - 1 do
      BF.Add.SetInt64(Factors[I]);

    Result := BigNumberChineseRemainderTheorem(Res, BR, BF);
  finally
    BF.Free;
    BR.Free;
  end;
end;

function BigNumberIsPerfectPower(Num: TCnBigNumber): Boolean;
var
  LG2, I: Integer;
  T: TCnBigNumber;
begin
  Result := False;
  if Num.IsNegative or Num.IsWord(2) or Num.IsWord(3) then
    Exit;

  if Num.IsZero or Num.IsOne then
  begin
    Result := True;
    Exit;
  end;

  LG2 := Num.GetBitsCount;
  T := FLocalBigNumberPool.Obtain;

  try
    for I := 2 to LG2 do
    begin
      // �� Num �� I �η������������֣�Ӧ�ò�����ָ������� Power 1/I �η�������������ƫС������
      BigNumberRoot(T, Num, I);
      // ��������������
      BigNumberPower(T, T, I);

      // �ж��Ƿ����
      if BigNumberCompare(T, Num) = 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

procedure BigNumberFillCombinatorialNumbers(List: TCnBigNumberList; N: Integer);
var
  M, MC: Integer;
  C, T: TCnBigNumber;
begin
  if (N < 0) or (List = nil) then
    Exit;

  List.Clear;
  List.Add.SetOne;
  if N = 0 then
    Exit;

  MC := N div 2;

  List.Count := N + 1;    // C(n, m) m �� 0 �� n��һ�� n+1 ��
  C := TCnBigNumber.Create;
  C.SetOne;
  List[N] := C;

  C := FLocalBigNumberPool.Obtain;
  C.SetOne;
  try
    for M := 0 to MC - 1 do
    begin
      T := TCnBigNumber.Create;
      BigNumberCopy(T, C);
      BigNumberMulWord(T, N - M);
      BigNumberDivWord(T, M + 1);

      List[M + 1] := T;
      if M + 1 <> N - M - 1 then
        List[N - M - 1] := BigNumberDuplicate(T);
      BigNumberCopy(C, T);
    end;
  finally
    FLocalBigNumberPool.Recycle(C);
  end;
end;

procedure BigNumberFillCombinatorialNumbersMod(List: TCnBigNumberList; N: Integer; P: TCnBigNumber);
var
  I: Integer;
begin
  if (P = nil) or (N < 0) then
    Exit;

  BigNumberFillCombinatorialNumbers(List, N);
  for I := 0 to List.Count - 1 do
    BigNumberNonNegativeMod(List[I], List[I], P);
end;

function BigNumberAKSIsPrime(N: Int64): Boolean;
var
  T: TCnBigNumber;
begin
  T := TCnBigNumber.Create;
  try
    T.SetInt64(N);
    Result := BigNumberAKSIsPrime(T);
  finally
    T.Free;
  end;
end;

function BigNumberAKSIsPrime(N: TCnBigNumber): Boolean;
var
  R, T, C, Q: TCnBigNumber;
  LG22: Int64;
  LG2: Extended;
  BK: TCnBigNumber;
begin
  Result := False;
  if N.IsNegative or N.IsZero or N.IsOne then
    Exit;
  if BigNumberIsPerfectPower(N) then // �������ȫ�����Ǻ���
    Exit;

  R := nil;
  T := nil;
  C := nil;
  Q := nil;
  BK := nil;

  try
    // �ҳ���С�� R ���� N mod R �ĳ˷��� > (Log����(N))^2
    // N mod R �ĳ˷��ף������ L����ָ���� N �� L �η��� mod R Ϊ 1 ����С L
    R := FLocalBigNumberPool.Obtain;
    R.SetOne;
    LG2 := BigNumberLog2(N);      // ������������Ҫ�õ����㣬һ�㲻�ᳬ�����㷶Χ
    LG22 := Trunc(LG2 * LG2);

    T := FLocalBigNumberPool.Obtain;
    BK := FLocalBigNumberPool.Obtain;

    // �ҳ���С�� R����һ��֮ǰ�ο�ά���ٿ��ϵ� K ������ 1 �� (Log����(N))^2����Ϊ��ʱ
    // �ָ�Ϊ�� R ��ŷ������ֵ�������������������ٶ���������ߣ�ǰ����ȡ�����ʱʹ�÷��ظ���ʼ���汾
    while True do
    begin
      R.AddWord(1);
      BigNumberMultiplicativeOrder(BK, N, R);
      if BigNumberCompareInteger(BK, LG22) > 0 then
        Break;
    end;

    // �õ� R���� R �������ң����ĳЩ�� R С�� T �� N �����أ����Ǻ���
    BigNumberCopy(T, R);
    C := FLocalBigNumberPool.Obtain;

    while BigNumberCompare(T, CnBigNumberOne) > 0 do
    begin
      BigNumberGcd(C, T, N);
      if (BigNumberCompare(C, CnBigNumberOne) > 0) and (BigNumberCompare(C, N) < 0) then
        Exit;

      T.SubWord(1);
    end;

    if BigNumberCompare(N, R) <= 0 then
    begin
      Result := True;
      Exit;
    end;

    Q := FLocalBigNumberPool.Obtain;
    BigNumberEuler(Q, R);
    BigNumberSqrt(Q, Q);
    BigNumberMulFloat(C, Q, LG2);
    // �˴�Ӧ����С�����㣬��Ϊ����������ϴ����
    // C := Trunc(Sqrt(Q) * LG2);

    // ���ڻ� (X^R-1, N) ����ǰ���� (X+Y)^N - (X^N + Y)��
    // Ҳ���� (X+Y)^N - (X^N + Y) չ������� X^R-1 ���࣬��ϵ������� N ȡģ
    // ���ݶ���ʽ���� (X+Y)^N չ�������ϵ�� mod N �󣬾ͱ���� X^N+Y^N��������������Ϊ 0
    // �� mod X^R - 1 ����ݼӷ���ģ����õ����� X^(N-R) + Y^N
    // X^N + Y �� X^R-1 ȡģ���� X^(N-R) + Y
    // һ�����õ��Ľ����ʵ�� Y^N - Y

    // �� 1 �� ŷ��(R)ƽ���� * (Log����(N)) ���������֣������Ϊ Y������ Y^N - Y mod N �Ƿ��� 0

    T.SetOne;
    while BigNumberCompare(T, C) <= 0 do
    begin
      if not BigNumberPowerMod(R, T, N, N) then // ���� R
        Exit;

      if not BigNumberSub(R, R, T) then
        Exit;

      if not BigNumberMod(R, R, N) then
        Exit;

      if not R.IsZero then
        Exit;

      T.AddWord(1);
    end;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(C);
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(BK);
  end;
end;

function BigNumberBPSWIsPrime(N: Int64): Boolean;
var
  T: TCnBigNumber;
begin
  T := TCnBigNumber.Create;
  try
    T.SetInt64(N);
    Result := BigNumberBPSWIsPrime(T);
  finally
    T.Free;
  end;
end;

function BigNumberBPSWIsPrime(N: TCnBigNumber): Boolean;
var
  T: Integer;
  X, Y, A, U: TCnBigNumber;
begin
  Result := False;
  if N.IsNegative or N.IsZero or N.IsOne then
    Exit;

  if BigNumberEqual(N, 2) then
  begin
    Result := True;
    Exit;
  end
  else if N.IsEven then
    Exit;

  X := nil;
  Y := nil;
  A := nil;
  U := nil;

  try
    X := FLocalBigNumberPool.Obtain;
    BigNumberCopy(X, N);

    X.SubWord(1);
    T := 0;
    while X.IsEven do
    begin
      X.ShiftRightOne;
      Inc(T);
    end;

    // ����һ���� 2 Ϊ���ķ����⣬�粻ͨ�����Ǻ���
    Y := FLocalBigNumberPool.Obtain;
    Y.SetWord(2);
    if BigNumberFermatCheckComposite(Y, N, X, T) then
      Exit;

    X.SetInteger(-3);
    repeat
      if X.IsNegative then
        X.SubWord(2)
      else
        X.AddWord(2);

      X.Negate;
    until BignumberJacobiSymbol(X, N) = -1;

    // X ���õ���ȷ�� D ֵ������ Q ֵ�ŵ� Y �У�P ֵ 1 Ҳ�� X ��
    X.SubWord(1);
    X.Negate;
    BigNumberShiftRight(Y, X, 2);
    X.SetOne;

    A := FLocalBigNumberPool.Obtain;
    BigNumberCopy(A, N);
    A.AddWord(1);

    U := FLocalBigNumberPool.Obtain;
    if not BigNumberLucasUSequenceMod(X, Y, A, N, U) then
      Exit;

    Result := U.IsZero;
  finally
    FLocalBigNumberPool.Recycle(U);
    FLocalBigNumberPool.Recycle(A);
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(X);
  end;
end;

function BigNumberIsMersennePrime(E: Integer): Boolean;
var
  I: Integer;
  K, M: TCnBigNumber;
begin
  Result := False;
  if E < 2 then
    Exit;

  if E = 2 then // 3 ��
  begin
    Result := True;
    Exit;
  end;

  K := nil;
  M := nil;

  try
    K := FLocalBigNumberPool.Obtain;

    // ���� Lucas-Lehmer ����
    K.SetWord(4);
    for I := 1 to E - 2 do
    begin
      // ÿһ��ƽ���� 2
      BigNumberSqr(K, K); // ע�� E ̫��ʱ�����غ�ʱ���ڴ�
      K.SubWord(2);
    end;

    // ������÷ɭ��
    M := FLocalBigNumberPool.Obtain;
    M.SetOne;
    M.ShiftLeft(E);
    M.SubWord(1);

    // �ж��Ƿ�����
    BigNumberMod(K, K, M);
    Result := K.IsZero;
  finally
    FLocalBigNumberPool.Recycle(M);
    FLocalBigNumberPool.Recycle(K);
  end;
end;

function BigNumberNonAdjanceFormWidth(N: TCnBigNumber; Width: Integer): TShortInts;
var
  K: TCnBigNumber;
  M, R, B1: Cardinal;
  I: Integer;
begin
  Result := nil;
  if (Width < 1) or (Width > 7) then
    Exit;

  K := nil;

  try
    K := FLocalBigNumberPool.Obtain;
    BigNumberCopy(K, N);
    SetLength(Result, K.GetBitsCount + 1);

    I := 0;
    if Width = 1 then
      M := 3                        // 1 ʱ��Ҫ mod 4�����ڱ����� 2 λ
    else
      M := not ((not 0) shl Width); // 0 �� W-1 λȫ 1
    B1 := 1 shl (Width - 1);        // 2^(W-1)

    while not K.IsZero do
    begin
      if K.IsOdd then
      begin
        R := BigNumberAndWordTo(K, M); // R �ǵͼ�λ��Ҳ�� Mod 2^W �� 4 ��ֵ�������� 0
        if Width = 1 then
          Result[I] := 2 - R
        else
        begin
          if R > B1 then
            Result[I] := R - B1 - B1   // �ͼ�λ�� Mod 2^W ֵ������ 2^W ��֮
          else
            Result[I] := R;
        end;

        if Result[I] > 0 then
          K.SubWord(Result[I])
        else if Result[I] < 0 then // SubWord �Ĳ������޷��ţ���������ټ�
          K.AddWord(-Result[I]);
      end
      else
        Result[I] := 0;

      Inc(I);
      K.ShiftRightOne;
    end;

    if I < Length(Result) then // ȥ�����೤��
      SetLength(Result, I);
  finally
    FLocalBigNumberPool.Recycle(K);
  end;
end;

// ��С���㷨����ɢ�������� A^X mod M = B �Ľ� Res��Ҫ�� A �� M ����
function BigNumberBigStepGiantStep(Res: TCnBigNumber; A, B, M: TCnBigNumber): Boolean;
var
  Map: TCnBigNumberHashMap;
  T, C, Q, N, K, V: TCnBigNumber;
begin
  Result := False;
  if A.IsNegative or B.IsNegative or M.IsNegative then
    Exit;

  T := nil;
  C := nil;
  K := nil;
  Q := nil;
  N := nil;
  Map := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberSqrt(T, M);
    T.AddWord(1);

    C := FLocalBigNumberPool.Obtain;
    BigNumberDirectMulMod(C, A, B, M);

    Map := TCnBigNumberHashMap.Create(True, True);
    K := FLocalBigNumberPool.Obtain;
    K.SetOne;

    while BigNumberCompare(K, T) < 0 do
    begin
      Map.Add(BigNumberDuplicate(C), BigNumberDuplicate(K));
      BigNumberDirectMulMod(C, A, C, M);
      K.AddWord(1);
    end;

    Q := FLocalBigNumberPool.Obtain;
    BigNumberPowerMod(Q, A, T, M);
    N := FLocalBigNumberPool.Obtain;
    BigNumberCopy(N, Q);

    K.SetOne;
    while BigNumberCompare(K, T) < 0 do
    begin
      if Map.HasKey(N) then
      begin
        V := Map.Find(N); // V ������
        BigNumberMul(Res, K, T);
        BigNumberSub(Res, Res, V);

        Result := True;
        Exit;
      end;
      BigNumberDirectMulMod(N, Q, N, M);
      K.AddWord(1);
    end;
  finally
    FLocalBigNumberPool.Recycle(N);
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(K);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(C);
    Map.Free;
  end;
end;

function BigNumberMultiplicativeOrder(Res: TCnBigNumber; N: TCnBigNumber;
  R: TCnBigNumber): Boolean;
var
  I: Integer;
  E, T: TCnBigNumber;
  F: TCnBigNumberList;
begin
  Result := False;

  T := nil;
  E := nil;
  F := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberGcd(T, N, R);
    if not T.IsOne then
      Exit;

    // ���ز��д��ڳ˷���
    E := FLocalBigNumberPool.Obtain;
    BigNumberEuler(E, R);

    F := TCnBigNumberList.Create;
    BigNumberFindAllFactors(E, F);

    // �õ�ȫ�����ӣ�������֤
    for I := 0 to F.Count - 1 do
    begin
      // ��֤ N �� F[I] �η� mod R �Ƿ�Ϊ 1
      BigNumberPowerMod(T, N, F[I], R);
      if T.IsOne then
      begin
        BigNumberCopy(Res, F[I]);
        Result := True;
        Exit;
      end;
    end;
  finally
    F.Free;
    FLocalBigNumberPool.Recycle(E);
    FLocalBigNumberPool.Recycle(T);
  end;
end;

// ��ӡ�����ڲ���Ϣ
function BigNumberDebugDump(Num: TCnBigNumber): string;
var
  I: Integer;
begin
  Result := '';
  if Num = nil then
    Exit;

  Result := Format('Neg %d. DMax %d. Top %d.', [Num.Neg, Num.DMax, Num.Top]);
  if (Num.D <> nil) and (Num.Top > 0) then
  begin
    for I := 0 to Num.Top - 1 do
    begin
{$IFDEF BN_DATA_USE_64}
      Result := Result + Format(' $%16.16x', [PCnBigNumberElementArray(Num.D)^[I]]);
{$ELSE}
      Result := Result + Format(' $%8.8x', [PCnBigNumberElementArray(Num.D)^[I]]);
{$ENDIF}
    end;
  end;
end;

// �������ڲ���Ϣԭ�ⲻ�� Dump �� Mem ��ָ���ڴ���
function BigNumberRawDump(Num: TCnBigNumber; Mem: Pointer): Integer;
begin
  if Num.D = nil then
  begin
    Result := 0;
    Exit;
  end
  else
    Result := Num.Top * SizeOf(TCnBigNumberElement);

  if Mem <> nil then
    Move(Num.D^, Mem^, Num.Top * SizeOf(TCnBigNumberElement));
end;

function SparseBigNumberListIsZero(P: TCnSparseBigNumberList): Boolean;
begin
  Result := (P = nil) or (P.Count = 0) or
    ((P.Count = 1) and (P[0].Exponent = 0) and (P[0].Value.IsZero));
end;

function SparseBigNumberListEqual(A, B: TCnSparseBigNumberList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  if (A = nil) and (B <> nil)then // һ���� nil����Ҫ�ж�����һ���ǲ��� 0
  begin
    if (B.Count = 0) or ((B.Count = 1) and (B[0].Exponent = 0) and B[0].Value.IsZero) then
    begin
      Result := True;
      Exit;
    end;
  end
  else if (B = nil) and (A <> nil) then
  begin
    if (A.Count = 0) or ((A.Count = 1) and (A[0].Exponent = 0) and A[0].Value.IsZero) then
    begin
      Result := True;
      Exit;
    end;
  end;

  if A.Count <> B.Count then
    Exit;

  for I := A.Count - 1 downto 0 do
  begin
    if (A[I].Exponent <> B[I].Exponent) or not BigNumberEqual(A[I].Value, B[I].Value) then
      Exit;
  end;
  Result := True;
end;

procedure SparseBigNumberListCopy(Dst, Src: TCnSparseBigNumberList);
var
  I: Integer;
  Pair: TCnExponentBigNumberPair;
begin
  if (Dst <> Src) and (Dst <> nil) then
  begin
    Dst.Clear;
    for I := 0 to Src.Count - 1 do
    begin
      Pair := TCnExponentBigNumberPair.Create;
      Pair.Exponent := Src[I].Exponent;
      BigNumberCopy(Pair.Value, Src[I].Value);
      Dst.Add(Pair);
    end;
  end;
end;

procedure SparseBigNumberListMerge(Dst, Src1, Src2: TCnSparseBigNumberList; Add: Boolean);
var
  I, J, K: Integer;
  P1, P2: TCnExponentBigNumberPair;
begin
  if Src1 = nil then                   // ֻҪ��һ���� nil��Dst �ͱ���Ϊ��һ��
  begin
    SparseBigNumberListCopy(Dst, Src2);
    if not Add then  // Src2 �Ǳ�����
      Dst.Negate;
  end
  else if Src2 = nil then
    SparseBigNumberListCopy(Dst, Src1)
  else if Src1 = Src2 then // ��� Src1 �� Src2 ��ͬһ�����ϲ���֧�� Dst Ҳ��ͬһ��������
  begin
    Dst.Count := Src1.Count;
    for I := 0 to Src1.Count - 1 do
    begin
      if Dst[I] = nil then
        Dst[I] := TCnExponentBigNumberPair.Create;
      Dst[I].Exponent := Src1[I].Exponent;
      if Add then
        BigNumberAdd(Dst[I].Value, Src1[I].Value, Src2[I].Value)
      else
        BigNumberSub(Dst[I].Value, Src1[I].Value, Src2[I].Value);
    end;
  end
  else // Src1 �� Src2 ����ͬһ����Ҫ�鲢
  begin
    if (Dst <> Src1) and (Dst <> Src2) then // �� Dst ���� Src1 �� Src2��Ҳ���������
    begin
      I := 0;
      J := 0;
      K := 0;

      Dst.Count := Src1.Count + Src2.Count;

      while (I < Src1.Count) and (J < Src2.Count) do
      begin
        P1 := Src1[I];
        P2 := Src2[J];

        if P1.Exponent = P2.Exponent then
        begin
          // ��ȣ����������� Dst ��
          if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;
          Dst[K].Exponent := P1.Exponent;

          if Add then
            BigNumberAdd(Dst[K].Value, P1.Value, P2.Value)
          else
            BigNumberSub(Dst[K].Value, P1.Value, P2.Value);

          Inc(I);
          Inc(J);
          Inc(K);
        end
        else if P1.Exponent < P2.Exponent then
        begin
          // P1 С���� P1 �� Dst[K] ��
          if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;
          Dst[K].Exponent := P1.Exponent;

          BigNumberCopy(Dst[K].Value, P1.Value);
          Inc(I);
          Inc(K);
        end
        else // P2 С���� P2 �� Dst[K] ��
        begin
          if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;
          Dst[K].Exponent := P2.Exponent;

          BigNumberCopy(Dst[K].Value, P2.Value);
          if not Add then
            Dst[K].Value.Negate;
          Inc(J);
          Inc(K);
        end;
      end;

      if (I = Src1.Count) and (J = Src2.Count) then
      begin
        Dst.Compact;
        Exit;
      end;

      // ʣ���ĸ��У���ȫ�ӵ� Dst �� K ��ʼ��λ��ȥ
      if I = Src1.Count then
      begin
        for I := J to Src2.Count - 1 do
        begin
          if K >= Dst.Count then
            Dst.Add(TCnExponentBigNumberPair.Create)
          else if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;

          Dst[K].Exponent := Src2[I].Exponent;
          BigNumberCopy(Dst[K].Value, Src2[I].Value);
          Inc(K);
        end;
      end
      else if J = Src2.Count then
      begin
        for J := I to Src1.Count - 1 do
        begin
          if K >= Dst.Count then
            Dst.Add(TCnExponentBigNumberPair.Create)
          else if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;

          Dst[K].Exponent := Src1[J].Exponent;
          BigNumberCopy(Dst[K].Value, Src1[J].Value);
          Inc(K);
        end;
      end;
      Dst.Compact;
    end
    else if Dst = Src1 then // Dst �� Src1���� Src1 �� Src2 ��ͬ
    begin
      // ���� Src2�������� Src1 ��
      for I := 0 to Src2.Count - 1 do
      begin
        P2 := Src2[I];
        if Add then
          BigNumberAdd(Dst.SafeValue[P2.Exponent], Dst.SafeValue[P2.Exponent], P2.Value)
        else
          BigNumberSub(Dst.SafeValue[P2.Exponent], Dst.SafeValue[P2.Exponent], P2.Value);
      end;
    end
    else if Dst = Src2 then // Dst �� Src2���� Src1 �� Src2 ��ͬ
    begin
      // ���� Src1�������� Src2 ��
      for I := 0 to Src1.Count - 1 do
      begin
        P1 := Src1[I];
        if Add then
          BigNumberAdd(Dst.SafeValue[P1.Exponent], Dst.SafeValue[P1.Exponent], P1.Value)
        else
          BigNumberSub(Dst.SafeValue[P1.Exponent], Dst.SafeValue[P1.Exponent], P1.Value);
      end;
    end;
  end;
end;

{ TCnBigNumber }

function TCnBigNumber.AddWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberAddWord(Self, W);
end;

procedure TCnBigNumber.Clear;
begin
  BigNumberClear(Self);
end;

function TCnBigNumber.ClearBit(N: Integer): Boolean;
begin
  Result := BigNumberClearBit(Self, N);
end;

constructor TCnBigNumber.Create;
begin
  inherited;
  Top := 0;
  Neg := 0;
  DMax := 0;
  D := nil;
end;

destructor TCnBigNumber.Destroy;
begin
{$IFDEF DEBUG}
  if FIsFromPool then
    raise ECnBigNumberException.Create(SCnErrorBigNumberFreeFromPool);
{$ENDIF}

  if D <> nil then
    FreeMemory(D);

  D := nil;
  inherited;
end;

function TCnBigNumber.DivWord(W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := BigNumberDivWord(Self, W);
end;

class function TCnBigNumber.FromBinary(Buf: PAnsiChar;
  ByteLen: Integer): TCnBigNumber;
begin
  Result := BigNumberFromBinary(Buf, ByteLen);
end;

class function TCnBigNumber.FromBytes(Buf: TBytes): TCnBigNumber;
begin
  Result := BigNumberFromBytes(Buf);
end;

function TCnBigNumber.ToBytes: TBytes;
begin
  Result := BigNumberToBytes(Self);
end;

class function TCnBigNumber.FromDec(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromDec(Buf);
end;

class function TCnBigNumber.FromHex(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromHex(Buf);
end;

function TCnBigNumber.GetBitsCount: Integer;
begin
  Result := BigNumberGetBitsCount(Self);
end;

function TCnBigNumber.GetBytesCount: Integer;
begin
  Result := BigNumberGetBytesCount(Self);
end;

function TCnBigNumber.GetCardinal: Cardinal;
begin
  Result := BigNumberGetWord(Self);
end;

function TCnBigNumber.GetWord: Cardinal;
begin
  Result := BigNumberGetWord(Self);
end;

{$IFDEF SUPPORT_UINT64}

function TCnBigNumber.GetUInt64: UInt64;
begin
  Result := BigNumberGetUInt64(Self);
end;

{$ENDIF}

procedure TCnBigNumber.Init;
begin
  BigNumberInit(Self);
end;

function TCnBigNumber.IsBitSet(N: Integer): Boolean;
begin
  Result := BigNumberIsBitSet(Self, N);
end;

function TCnBigNumber.IsNegative: Boolean;
begin
  Result := BigNumberIsNegative(Self);
end;

function TCnBigNumber.IsOdd: Boolean;
begin
  Result := BigNumberIsOdd(Self);
end;

function TCnBigNumber.IsEven: Boolean;
begin
  Result := BigNumberIsEven(Self);
end;

function TCnBigNumber.IsOne: Boolean;
begin
  Result := BigNumberIsOne(Self);
end;

function TCnBigNumber.IsNegOne: Boolean;
begin
  Result := BigNumberIsNegOne(Self);
end;

function TCnBigNumber.IsWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberIsWord(Self, W);
end;

function TCnBigNumber.IsCardinal: Boolean;
begin
  Result := BigNumberIsUInt32(Self);
end;

function TCnBigNumber.IsInteger: Boolean;
begin
  Result := BigNumberIsInt32(Self);
end;

function TCnBigNumber.IsInt64: Boolean;
begin
  Result := BigNumberIsInt64(Self);
end;

function TCnBigNumber.IsUInt64: Boolean;
begin
  Result := BigNumberIsUInt64(Self);
end;

function TCnBigNumber.IsZero: Boolean;
begin
  Result := BigNumberIsZero(Self);
end;

function TCnBigNumber.ModWord(W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := BigNumberModWord(Self, W);
end;

function TCnBigNumber.MulWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberMulWord(Self, W);
end;

function TCnBigNumber.SetBit(N: Integer): Boolean;
begin
  Result := BigNumberSetBit(Self, N);
end;

function TCnBigNumber.SetDec(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetDec(Buf, Self);
end;

function TCnBigNumber.SetBinary(Buf: PAnsiChar; ByteLen: Integer): Boolean;
begin
  Result := BigNumberSetBinary(Buf, ByteLen, Self);
end;

function TCnBigNumber.SetHex(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetHex(Buf, Self);
end;

procedure TCnBigNumber.SetNegative(Negative: Boolean);
begin
  BigNumberSetNegative(Self, Negative);
end;

function TCnBigNumber.SetOne: Boolean;
begin
  Result := BigNumberSetOne(Self);
end;

function TCnBigNumber.SetCardinal(W: Cardinal): Boolean;
begin
  Result := BigNumberSetWord(Self, W);
end;

function TCnBigNumber.SetWord(W: Cardinal): Boolean;
begin
  Result := BigNumberSetWord(Self, W);
end;

{$IFDEF SUPPORT_UINT64}

function TCnBigNumber.SetUInt64(W: UInt64): Boolean;
begin
  Result := BigNumberSetUInt64(Self, W);
end;

{$ENDIF}

function TCnBigNumber.SetZero: Boolean;
begin
  Result := BigNumberSetZero(Self);
end;

function TCnBigNumber.SubWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberSubWord(Self, W);
end;

function TCnBigNumber.ToBinary(const Buf: PAnsiChar; FixedLen: Integer): Integer;
begin
  Result := BigNumberToBinary(Self, Buf, FixedLen);
end;

function TCnBigNumber.ToDec: string;
begin
  Result := string(BigNumberToDec(Self));
end;

function TCnBigNumber.ToHex(FixedLen: Integer): string;
begin
  Result := BigNumberToHex(Self, FixedLen);
end;

function TCnBigNumber.ToString: string;
begin
  Result := BigNumberToString(Self);
end;

function TCnBigNumber.WordExpand(Words: Integer): TCnBigNumber;
begin
  Result := BigNumberWordExpand(Self, Words);
end;

function TCnBigNumber.GetDecString: string;
begin
  Result := ToDec;
end;

function TCnBigNumber.GetHexString: string;
begin
  Result := ToHex;
end;

function TCnBigNumber.GetDebugDump: string;
begin
  Result := BigNumberDebugDump(Self);
end;

function TCnBigNumber.RawDump(Mem: Pointer): Integer;
begin
  Result := BigNumberRawDump(Self, Mem);
end;

function TCnBigNumber.GetInt64: Int64;
begin
  Result := BigNumberGetInt64(Self);
end;

function TCnBigNumber.SetInt64(W: Int64): Boolean;
begin
  Result := BigNumberSetInt64(Self, W);
end;

procedure TCnBigNumber.Negate;
begin
  BigNumberNegate(Self);
end;

function TCnBigNumber.PowerWord(W: Cardinal): Boolean;
begin
  Result := BigNumberPower(Self, Self, W);
end;

function TCnBigNumber.GetTenPrecision: Integer;
begin
  Result := BigNumberGetTenPrecision(Self);
end;

procedure TCnBigNumber.ShiftLeft(N: Integer);
begin
  BigNumberShiftLeft(Self, Self, N);
end;

procedure TCnBigNumber.ShiftRight(N: Integer);
begin
  BigNumberShiftRight(Self, Self, N);
end;

procedure TCnBigNumber.ShiftLeftOne;
begin
  BigNumberShiftLeftOne(Self, Self);
end;

procedure TCnBigNumber.ShiftRightOne;
begin
  BigNumberShiftRightOne(Self, Self);
end;

function TCnBigNumber.GetInteger: Integer;
begin
  Result := BigNumberGetInteger(Self);
end;

function TCnBigNumber.SetInteger(W: Integer): Boolean;
begin
  Result := BigNumberSetInteger(Self, W);
end;

function TCnBigNumber.GetWordCount: Integer;
begin
  Result := BigNumberGetWordsCount(Self);
end;

function TCnBigNumber.GetHashCode: TCnHashCode;
var
  I: Integer;
begin
  // �� 32 λ������ȫ��������ؼ�����������Ԫ���� 64 λҲֻ����� 32 λ
  Result := 0;
  for I := 0 to Top - 1 do
    Result := Result + Integer(PCnBigNumberElementArray(D)^[I]);
end;

class function TCnBigNumber.FromFloat(F: Extended): TCnBigNumber;
begin
  Result := BigNumberFromFloat(F);
end;

function TCnBigNumber.SaveToStream(Stream: TStream;
  FixedLen: Integer): Integer;
begin
  Result := BigNumberWriteBinaryToStream(Self, Stream, FixedLen);
end;

function TCnBigNumber.LoadFromStream(Stream: TStream): Boolean;
begin
  Result := BigNumberReadBinaryFromStream(Self, Stream);
end;

class function TCnBigNumber.FromBase64(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromBase64(Buf);
end;

function TCnBigNumber.SetBase64(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetBase64(Buf, Self);
end;

function TCnBigNumber.ToBase64: string;
begin
  Result := BigNumberToBase64(Self);
end;

function TCnBigNumber.SetFloat(F: Extended): Boolean;
begin
  Result := BigNumberSetFloat(F, Self);
end;

function TCnBigNumber.IsFloat: Boolean;
begin
  Result := BigNumberIsFloat(Self);
end;

function TCnBigNumber.GetFloat: Extended;
begin
  Result := BigNumberGetFloat(Self);
end;

{ TCnBigNumberList }

function TCnBigNumberList.Add(ABigNumber: TCnBigNumber): Integer;
begin
  Result := inherited Add(ABigNumber);
end;

function TCnBigNumberList.Add: TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
  Add(Result);
end;

function TCnBigNumberList.Add(Num: Integer): TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
  Result.SetInteger(Num);
  Add(Result);
end;

procedure TCnBigNumberList.AddList(List: TCnBigNumberList);
var
  I: Integer;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
      Add(BigNumberDuplicate(List[I]));
  end;
end;

procedure TCnBigNumberList.BigNumberSort;
begin
  inherited Sort(DefBigNumberCompare);
end;

constructor TCnBigNumberList.Create;
begin
  inherited Create(True);
end;

function TCnBigNumberList.GetItem(Index: Integer): TCnBigNumber;
begin
  Result := TCnBigNumber(inherited GetItem(Index));
end;

function TCnBigNumberList.IndexOfValue(ABigNumber: TCnBigNumber): Integer;
begin
  Result := 0;
  while (Result < Count) and (BigNumberCompare(Items[Result], ABigNumber) <> 0) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TCnBigNumberList.Insert(Index: Integer;
  ABigNumber: TCnBigNumber);
begin
  inherited Insert(Index, ABigNumber);
end;

function TCnBigNumberList.Remove(ABigNumber: TCnBigNumber): Integer;
begin
  Result := inherited Remove(ABigNumber);
end;

procedure TCnBigNumberList.RemoveDuplicated;
var
  I, Idx: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    // ȥ���ظ�����
    Idx := IndexOfValue(Items[I]);
    if (Idx >= 0) and (Idx <> I) then
      Delete(I);
  end;
end;

procedure TCnBigNumberList.SetItem(Index: Integer;
  ABigNumber: TCnBigNumber);
begin
  inherited SetItem(Index, ABigNumber);
end;

procedure TCnBigNumberList.SumTo(Sum: TCnBigNumber);
var
  I: Integer;
begin
  Sum.SetZero;
  for I := 0 to Count - 1 do
    BigNumberAdd(Sum, Sum, Items[I]);
end;

function TCnBigNumberList.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := Items[I].ToDec
    else
      Result := Result + ',' + Items[I].ToDec;
  end;
end;

{ TCnBigNumberPool }

function TCnBigNumberPool.CreateObject: TObject;
begin
  Result := TCnBigNumber.Create;
end;

function TCnBigNumberPool.Obtain: TCnBigNumber;
begin
  Result := TCnBigNumber(inherited Obtain);
  Result.Clear;
end;

procedure TCnBigNumberPool.Recycle(Num: TCnBigNumber);
begin
  inherited Recycle(Num);
end;

{ TCnExponentBigNumberPair }

constructor TCnExponentBigNumberPair.Create;
begin
  inherited;
  FValue := TCnBigNumber.Create;
  FValue.SetZero;
end;

destructor TCnExponentBigNumberPair.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TCnExponentBigNumberPair.ToString: string;
begin
  Result := FValue.ToDec + '^' + IntToStr(FExponent);
end;

{ TCnSparseBigNumberList }

function TCnSparseBigNumberList.AddPair(AExponent: Integer;
  Num: TCnBigNumber): TCnExponentBigNumberPair;
begin
  Result := TCnExponentBigNumberPair.Create;
  Result.Exponent := AExponent;
  BigNumberCopy(Result.Value, Num);
  Add(Result);
end;

procedure TCnSparseBigNumberList.AssignTo(Dest: TCnSparseBigNumberList);
begin
  SparseBigNumberListCopy(Dest, Self);
end;

function TCnSparseBigNumberList.BinarySearchExponent(AExponent: Integer;
  var OutIndex: Integer): Boolean;
var
  I, Start,Stop, Mid: Integer;
  Pair: TCnExponentBigNumberPair;
  BreakFromStart: Boolean;
begin
  Result := False;
  if Count = 0 then
  begin
    OutIndex := MaxInt;
  end
  else if Count <= SPARSE_BINARY_SEARCH_THRESHOLD then
  begin
    // �����٣�ֱ����
    for I := 0 to Count - 1 do
    begin
      Pair := Items[I];
      if Pair.Exponent = AExponent then
      begin
        Result := True;
        OutIndex := I;
        Exit;
      end
      else if Pair.Exponent > AExponent then
      begin
        OutIndex := I;
        Exit;
      end;
    end;
    // AExponent �����һ�� Pair �Ļ���
    OutIndex := MaxInt;
  end
  else
  begin
    Pair := Top;
    if Pair.Exponent < AExponent then      // AExponent �����һ�� Pair �Ļ���
    begin
      OutIndex := MaxInt;
      Exit;
    end
    else if Pair.Exponent = AExponent then // AExponent ���������һ�� Pair
    begin
      OutIndex := Count - 1;
      Result := True;
      Exit;
    end
    else
    begin
      Pair := Bottom;
      if Pair.Exponent > AExponent then    // AExponent �ȵ�һ�� Pair �Ļ�С
      begin
        OutIndex := 0;
        Exit;
      end
      else if Pair.Exponent = AExponent then // AExponent �����ǵ�һ�� Pair
      begin
        OutIndex := 0;
        Result := True;
        Exit;
      end
    end;

    // ��ʼ�����Ķ��ֲ���
    Start := 0;
    Stop := Count - 1;
    Mid := 0;
    BreakFromStart := False;

    while Start <= Stop do
    begin
      Mid := (Start + Stop) div 2;

      Pair := Items[Mid];
      if Pair.Exponent = AExponent then
      begin
        Result := True;
        OutIndex := Mid;
        Exit;
      end
      else if Pair.Exponent < AExponent then
      begin
        Start := Mid + 1; // ������һ���Ǵ����˳��������λ��Ϊ Mid + 1
        BreakFromStart := True;
      end
      else if Pair.Exponent > AExponent then
      begin
        Stop := Mid - 1;  // ������һ���Ǵ����˳��������λ��Ϊ Mid - 1
        BreakFromStart := False;
      end;
    end;

    if BreakFromStart then
      OutIndex := Mid + 1
    else
      OutIndex := Mid;
    Result := False;
  end;
end;

function TCnSparseBigNumberList.Bottom: TCnExponentBigNumberPair;
begin
  Result := nil;
  if Count > 0 then
    Result := Items[0];
end;

procedure TCnSparseBigNumberList.Compact;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if (Items[I] = nil) or Items[I].Value.IsZero then
      Delete(I);
end;

constructor TCnSparseBigNumberList.Create;
begin
  inherited Create(True);
end;

function TCnSparseBigNumberList.GetItem(Index: Integer): TCnExponentBigNumberPair;
begin
  Result := TCnExponentBigNumberPair(inherited GetItem(Index));
end;

function TCnSparseBigNumberList.GetReadonlyValue(Exponent: Integer): TCnBigNumber;
var
  OutIndex: Integer;
begin
  if not BinarySearchExponent(Exponent, OutIndex) then
    Result := CnBigNumberZero
  else
    Result := Items[OutIndex].Value;
end;

function TCnSparseBigNumberList.GetSafeValue(Exponent: Integer): TCnBigNumber;
var
  OutIndex: Integer;
begin
  if not BinarySearchExponent(Exponent, OutIndex) then
  begin
    // δ�ҵ���Ҫ����
    OutIndex := InsertByOutIndex(OutIndex);
    Items[OutIndex].Exponent := Exponent;
  end;
  Result := Items[OutIndex].Value;
end;

function TCnSparseBigNumberList.InsertByOutIndex(
  OutIndex: Integer): Integer;
var
  Pair: TCnExponentBigNumberPair;
begin
  if OutIndex < 0 then
    OutIndex := 0;

  Pair := TCnExponentBigNumberPair.Create;
  if OutIndex >= Count then
  begin
    Add(Pair);
    Result := Count - 1;
  end
  else
  begin
    Insert(OutIndex, Pair);
    Result := OutIndex;
  end;
end;

procedure TCnSparseBigNumberList.Negate;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if (Items[I] <> nil) then
      Items[I].Value.Negate;
end;

procedure TCnSparseBigNumberList.SetItem(Index: Integer;
  const Value: TCnExponentBigNumberPair);
begin
  inherited SetItem(Index, Value);
end;

procedure TCnSparseBigNumberList.SetSafeValue(Exponent: Integer;
  const Value: TCnBigNumber);
var
  OutIndex: Integer;
begin
  if not BinarySearchExponent(Exponent, OutIndex) then
  begin
    // δ�ҵ������ Value �޻� 0����Ҫ����
    if (Value <> nil) and not Value.IsZero then
    begin
      OutIndex := InsertByOutIndex(OutIndex);
      Items[OutIndex].Exponent := Exponent;
    end
    else // δ�ҵ����� 0������
      Exit;
  end;

  // �ҵ��˻��߲����ˣ���ֵ
  if (Value <> nil) and not Value.IsZero then
    BigNumberCopy(Items[OutIndex].Value, Value)
  else
    Items[OutIndex].Value.SetZero;
end;

procedure TCnSparseBigNumberList.SetValues(LowToHighList: array of Int64);
var
  I: Integer;
  Pair: TCnExponentBigNumberPair;
begin
  Clear;
  for I := Low(LowToHighList) to High(LowToHighList) do
  begin
    Pair := TCnExponentBigNumberPair.Create;
    Pair.Exponent := I;
    Pair.Value.SetInt64(LowToHighList[I]);
    Add(Pair);
  end;
end;

function TCnSparseBigNumberList.Top: TCnExponentBigNumberPair;
begin
  Result := nil;
  if Count > 0 then
  begin
{$IFDEF LIST_INDEX_NATIVEINT}
    Result := TCnExponentBigNumberPair(Items[Count - 1]);
{$ELSE}
    Result := Items[Count - 1];
{$ENDIF}
  end;
end;

function TCnSparseBigNumberList.ToString: string;
var
  I: Integer;
  IsFirst: Boolean;
begin
  Result := '';
  IsFirst := True;
  for I := Count - 1 downto 0 do
  begin
    if IsFirst then
    begin
      Result := Items[I].ToString;
      IsFirst := False;
    end
    else
      Result := Result + CRLF + Items[I].ToString;
  end;
end;

{ TCnBigNumberHashMap }

constructor TCnBigNumberHashMap.Create(AOwnsKey, AOwnsValue: Boolean);
begin
  inherited Create;
  FOwnsKey := AOwnsKey;
  FOwnsValue := AOwnsValue;
end;

procedure TCnBigNumberHashMap.DoFreeNode(Node: TCnHashNode);
begin
  if FOwnsKey then
  begin
    Node.Key.Free;
    Node.Key := nil;
  end;
  if FOwnsValue then
  begin
    Node.Value.Free;
    Node.Value := nil;
  end;

  inherited;
end;

function TCnBigNumberHashMap.Find(Key: TCnBigNumber): TCnBigNumber;
begin
  Result := TCnBigNumber(inherited Find(Key));
end;

function TCnBigNumberHashMap.HashCodeFromObject(Obj: TObject): Integer;
begin
  if Obj is TCnBigNumber then // ��ʽд�����Ա�֤�Ͱ汾�ֶ����� GetHashCode
    Result := TCnBigNumber(Obj).GetHashCode
  else                        // ��������ø�����ݱ������汾�����Ƿ���� GetHashCode
    Result := inherited HashCodeFromObject(Obj)
end;

function TCnBigNumberHashMap.KeyEqual(Key1: TObject; Key2: TObject
  {$IFNDEF CPU64BITS}; Key132: TObject; Key232: TObject {$ENDIF}): Boolean;
begin
  Result := BigNumberEqual(TCnBigNumber(Key1), TCnBigNumber(Key2));
end;

initialization
  FLocalBigNumberPool := TCnBigNumberPool.Create;

  CnBigNumberOne := TCnBigNumber.Create;
  CnBigNumberOne.SetOne;
  CnBigNumberZero := TCnBigNumber.Create;
  CnBigNumberZero.SetZero;
  CnBigNumberNegOne := TCnBigNumber.Create;
  CnBigNumberNegOne.SetInteger(-1);

finalization
  CnBigNumberNegOne.Free;
  CnBigNumberZero.Free;
  CnBigNumberOne.Free;

  FLocalBigNumberPool.Free;
  FLocalBigBinaryPool.Free;

end.

