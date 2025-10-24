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

unit CnMatrix;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�������������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ���� Int64 ��Χ�ڼ���������Χ�ڵľ������㡣

*           �߽�����ʽ�Ĵ�������ʽ���㷽��������֤ͨ�����������������ܲ���������
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ���δ���С����� Hadamard ��س˷���������ɷ���ԭ���������
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2022.07.01 V1.3
*               �������б�������ļ���
*           2022.06.29 V1.2
*               ���븡��������
*           2019.06.12 V1.1
*               �����������������
*           2019.06.05 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, CnMath;

type
  ECnMatrixException = class(Exception);
  {* ��������쳣}

  TCnIntMatrix = class(TPersistent)
  {* Int64 ��Χ�ڵ����������ʵ����}
  private
    FMatrix: array of array of Int64;
    FColCount: Integer;
    FRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function GetValue(Row: Integer; Col: Integer): Int64;
    function GetZigZagValue(Index: Integer): Int64;
    procedure SetZigZagValue(Index: Integer; const Value: Int64);
  protected
    procedure SetValue(Row: Integer; Col: Integer; const AValue: Int64); virtual;

    function Add3(X: Int64; Y: Int64; Z: Int64): Int64; virtual;
    function Mul3(X: Int64; Y: Int64; Z: Int64): Int64; virtual;
    function NegativeOnePower(N: Integer): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    {* ���캯����

       ������
         ARowCount: Integer               - ָ����������
         AColCount: Integer               - ָ����������

       ����ֵ��                           - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    // ����������ʵ���Զ���ļӼ��˳�������������Ϊ��������û�г�����
    function OperationAdd(X: Int64; Y: Int64): Int64; virtual;
    {* Ԫ�ؼӷ�������

       ������
         X: Int64                         - ����һ
         Y: Int64                         - ������

       ����ֵ��Int64                      - ���غ�
    }

    function OperationSub(X: Int64; Y: Int64): Int64; virtual;
    {* Ԫ�ؼ���������

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ���ز�
    }

    function OperationMul(X: Int64; Y: Int64): Int64; virtual;
    {* Ԫ�س˷�������

       ������
         X: Int64                         - ����һ
         Y: Int64                         - ������

       ����ֵ��Int64                      - ���ػ�
    }

    function OperationDiv(X: Int64; Y: Int64): Int64; virtual;
    {* Ԫ�س�������������δʵ�֣������쳣��

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ������
    }


    procedure Add(Factor: Int64);
    {* �����Ԫ�ؼ���һ��������

       ������
         Factor: Int64                    - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Factor: Int64);
    {* �����Ԫ�س���һ��������

       ������
         Factor: Int64                    - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Factor: Int64); virtual;
    {* �����Ԫ�س���һ��������������Ϊ��������δʵ�ֳ����������쳣��

       ������
         Factor: Int64                    - ����

       ����ֵ�����ޣ�
    }

    procedure SetE(Size: Integer);
    {* ����Ϊ Size �׵�λ����

       ������
         Size: Integer                    - ����

       ����ֵ�����ޣ�
    }

    procedure SetZero;
    {* ����Ϊȫ 0 ����}

    procedure Transpose;
    {* ����ת�ã�Ҳ�������л���}

    function Determinant: Int64; virtual;
    {* ��������ʽֵ��

       ������
         ���ޣ�

       ����ֵ��Int64                      - ��������ʽֵ
    }

    function Trace: Int64;
    {* ����ļ���Ҳ�������ϵ����µĶԽ���Ԫ�صĺ͡�

       ������
         ���ޣ�

       ����ֵ��Int64                      - ���ؼ�
    }

    function IsSquare: Boolean;
    {* �Ƿ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ���
    }

    function IsZero: Boolean;
    {* �Ƿ�ȫ 0 ����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�ȫ 0 ����
    }

    function IsE: Boolean;
    {* �Ƿ�λ����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�λ����
    }

    function IsSymmetrical: Boolean;
    {* �Ƿ�ԳƷ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�ԳƷ���
    }

    function IsSingular: Boolean;
    {* �Ƿ����췽��Ҳ��������ʽ�Ƿ���� 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ����췽��
    }

    procedure DeleteRow(Row: Integer);
    {* ɾ������һ�С�

       ������
         Row: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    procedure DeleteCol(Col: Integer);
    {* ɾ������һ�С�

       ������
         Col: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* ������ַ����б�

       ������
         List: TStrings                   - �������ɽ�����ַ����б�
         Sep: Char                        - �ָ���

       ����ֵ�����ޣ�
    }

    property Value[Row, Col: Integer]: Int64 read GetValue write SetValue; default;
    {* ���������±���ʾ���Ԫ�أ��±궼�� 0 ��ʼ}
    property ZigZagValue[Index: Integer]: Int64 read GetZigZagValue write SetZigZagValue;
    {* ���������Ͻǿ�ʼб�ŵĵ�ֵ����}
  published
    property ColCount: Integer read FColCount write SetColCount;
    {* ��������}
    property RowCount: Integer read FRowCount write SetRowCount;
    {* ��������}
  end;

  TCnFloatMatrix = class(TPersistent)
  {* ��������Χ�ڵ����������ʵ����}
  private
    FMatrix: array of array of Extended;
    FColCount: Integer;
    FRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function GetValue(Row: Integer; Col: Integer): Extended;
    function GetZigZagValue(Index: Integer): Extended;
    procedure SetZigZagValue(Index: Integer; const Value: Extended);
  protected
    procedure SetValue(Row: Integer; Col: Integer; const AValue: Extended); virtual;

    function Add3(X: Extended; Y: Extended; Z: Extended): Extended; virtual;
    function Mul3(X: Extended; Y: Extended; Z: Extended): Extended; virtual;
    function NegativeOnePower(N: Integer): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    {* ���캯����

       ������
         ARowCount: Integer               - ָ����������
         AColCount: Integer               - ָ����������

       ����ֵ��                           - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    // ����������ʵ���Զ���ļӼ��˳�����
    function OperationAdd(X: Extended; Y: Extended): Extended; virtual;
    {* Ԫ�ؼӷ�������

       ������
         X: Extended                      - ����һ
         Y: Extended                      - ������

       ����ֵ��Extended                   - ���غ�
    }

    function OperationSub(X: Extended; Y: Extended): Extended; virtual;
    {* Ԫ�ؼ���������

       ������
         X: Extended                      - ������
         Y: Extended                      - ����

       ����ֵ��Extended                   - ���ز�
    }

    function OperationMul(X: Extended; Y: Extended): Extended; virtual;
    {* Ԫ�س˷�������

       ������
         X: Extended                      - ����һ
         Y: Extended                      - ������

       ����ֵ��Extended                   - ���ػ�
    }

    function OperationDiv(X: Extended; Y: Extended): Extended; virtual;
    {* Ԫ�س���������

       ������
         X: Extended                      - ������
         Y: Extended                      - ����

       ����ֵ��Extended                   - ������
    }

    procedure Add(Factor: Extended);
    {* �����Ԫ�ؼ���һ������

       ������
         Factor: Extended                 - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Factor: Extended);
    {* �����Ԫ�س���һ��������

       ������
         Factor: Extended                 - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Factor: Extended); virtual;
    {* �����Ԫ�س���һ��������

       ������
         Factor: Extended                 - ����

       ����ֵ�����ޣ�
    }

    procedure SetE(Size: Integer);
    {* ����Ϊ Size �׵�λ����

       ������
         Size: Integer                    - ����

       ����ֵ�����ޣ�
    }

    procedure SetZero;
    {* ����Ϊȫ 0 ����}

    procedure Transpose;
    {* ����ת�ã�Ҳ�������л���}

    function Determinant: Extended; virtual;
    {* ��������ʽֵ��

       ������
         ���ޣ�

       ����ֵ��Extended                   - ��������ʽֵ
    }

    function Trace: Extended;
    {* ����ļ���Ҳ�������ϵ����µĶԽ���Ԫ�صĺ͡�

       ������
         ���ޣ�

       ����ֵ��Extended                   - ���ؼ�
    }

    function IsSquare: Boolean;
    {* �Ƿ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ���
    }

    function IsZero: Boolean;
    {* �Ƿ�ȫ 0 ����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�ȫ 0 ����
    }

    function IsE: Boolean;
    {* �Ƿ�λ����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�λ����
    }

    function IsSymmetrical: Boolean;
    {* �Ƿ�ԳƷ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�ԳƷ���
    }

    function IsSingular: Boolean;
    {* �Ƿ����췽��Ҳ��������ʽ�Ƿ���� 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ����췽��
    }

    procedure DeleteRow(Row: Integer);
    {* ɾ������һ�С�

       ������
         Row: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    procedure DeleteCol(Col: Integer);
    {* ɾ������һ�С�

       ������
         Col: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* ������ַ����б�

       ������
         List: TStrings                   - �������ɽ�����ַ����б�
         Sep: Char                        - �ָ���

       ����ֵ�����ޣ�
    }

    property Value[Row, Col: Integer]: Extended read GetValue write SetValue; default;
    {* ���������±���ʾ���Ԫ�أ��±궼�� 0 ��ʼ}
    property ZigZagValue[Index: Integer]: Extended read GetZigZagValue write SetZigZagValue;
    {* ���������Ͻǿ�ʼб�ŵĵ�ֵ����}
  published
    property ColCount: Integer read FColCount write SetColCount;
    {* ��������}
    property RowCount: Integer read FRowCount write SetRowCount;
    {* ��������}
  end;

  TCnRationalNumber = class(TPersistent)
  {* ��ʾһ�������������ӷ�ĸ���� Int64 ��Χ��}
  private
    FNominator: Int64;
    FDenominator: Int64;
    procedure SetDenominator(const Value: Int64);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    function IsInt: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* �Ƿ�������Ҳ�����жϷ�ĸ�Ƿ������� 1��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�����
    }

    function IsZero: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* �Ƿ�Ϊ 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 0
    }

    function IsOne: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* �Ƿ�Ϊ 1��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 1
    }

    function IsNegative: Boolean;
    {* �Ƿ�Ϊ��ֵ��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�ֵ
    }

    procedure Neg;
    {* ����෴��}

    procedure Reciprocal;
    {* ��ɵ���}

    procedure SetZero;
    {* ��Ϊ 0}

    procedure SetOne;
    {* ��Ϊ 1}

    function EqualInt(Value: Int64): Boolean;
    {* �Ƿ�����һֵ��ȡ�

       ������
         Value: Int64                     - ���Ƚϵ�����

       ����ֵ��Boolean                    - �����Ƿ����
    }

    function Equal(Value: TCnRationalNumber): Boolean;
    {* �Ƿ�����һֵ��ȡ�

       ������
         Value: TCnRationalNumber         - ���Ƚϵ�������

       ����ֵ��Boolean                    - �����Ƿ����
    }

    procedure Add(Value: Int64); overload;
    {* ����һ��������

       ������
         Value: Int64                     - ����

       ����ֵ�����ޣ�
    }

    procedure Sub(Value: Int64); overload;
    {* ��ȥһ��������

       ������
         Value: Int64                     - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Value: Int64); overload;
    {* ����һ��������

       ������
         Value: Int64                     - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Value: Int64); overload;
    {* ����һ��������

       ������
         Value: Int64                     - ����

       ����ֵ�����ޣ�
    }

    procedure Add(Value: TCnRationalNumber); overload;
    {* ����һ����������

       ������
         Value: TCnRationalNumber         - ����

       ����ֵ�����ޣ�
    }

    procedure Sub(Value: TCnRationalNumber); overload;
    {* ��ȥһ����������

       ������
         Value: TCnRationalNumber         - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Value: TCnRationalNumber); overload;
    {* ����һ����������

       ������
         Value: TCnRationalNumber         - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Value: TCnRationalNumber); overload;
    {* ����һ����������

       ������
         Value: TCnRationalNumber         - ����

       ����ֵ�����ޣ�
    }

    procedure SetIntValue(Value: Int64);
    {* ֵ��Ϊһ��������

       ������
         Value: Int64                     - �����õ�����

       ����ֵ�����ޣ�
    }

    procedure SetValue(ANominator: Int64; ADenominator: Int64);
    {* ֵ��Ϊһ��������

       ������
         ANominator: Int64                - ����
         ADenominator: Int64              - ��ĸ

       ����ֵ�����ޣ�
    }

    procedure SetString(const Value: string);
    {* ֵ��Ϊһ���ַ����������Ǵ����֣���� / �ķ�����

       ������
         const Value: string              - �����õ��ַ���

       ����ֵ�����ޣ�
    }

    procedure Reduce;
    {* ����Լ��}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ������ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - �����ַ���
    }

    property Nominator: Int64 read FNominator write FNominator;
    {* ����}
    property Denominator: Int64 read FDenominator write SetDenominator;
    {* ��ĸ}
  end;

  TCn2DObjectList = class
  {* ��ά�������飬ӵ�����еĶ���}
  private
    FRowCount: Integer;
    FColCount: Integer;
    FRows: TObjectList;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
  protected
    function GetValueObject(Row: Integer; Col: Integer): TObject;
    procedure SetValueObject(Row: Integer; Col: Integer; const Value: TObject); // һ�� TObjectList
  public
    constructor Create(ARowCount: Integer; AColCount: Integer); virtual;
    {* ���캯����

       ������
         ARowCount: Integer               - ָ����ά��������
         AColCount: Integer               - ָ����ά��������

       ����ֵ��                           - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    procedure DeleteRow(Row: Integer);
    {* ɾ������һ�С�

       ������
         Row: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    procedure DeleteCol(Col: Integer);
    {* ɾ������һ�С�

       ������
         Col: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    property ValueObject[Row, Col: Integer]: TObject read GetValueObject write SetValueObject; default;
    {* ��ά����ֵ}
    property RowCount: Integer read GetRowCount write SetRowCount;
    {* ����}
    property ColCount: Integer read GetColCount write SetColCount;
    {* ����}
  end;

  TCnRationalMatrix = class(TPersistent)
  {* ��������Χ�ڵ����������ʵ����}
  private
    FMatrix: TCn2DObjectList;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetValue(Row: Integer; Col: Integer; const Value: TCnRationalNumber);
    function GetValue(Row: Integer; Col: Integer): TCnRationalNumber;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetZigZagValue(Index: Integer): TCnRationalNumber;
    procedure SetZigZagValue(Index: Integer;
      const Value: TCnRationalNumber);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    {* ���캯����

       ������
         ARowCount: Integer               - ָ����������
         AColCount: Integer               - ָ����������

       ����ֵ��                           - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}


    procedure Add(Factor: Int64); overload;
    {* �����Ԫ�ؼ���һ��������

       ������
         Factor: Int64                    - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Factor: Int64); overload;
    {* �����Ԫ�س���һ��������

       ������
         Factor: Int64                    - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Factor: Int64); overload;
    {* �����Ԫ�س���һ��������

       ������
         Factor: Int64                    - ����

       ����ֵ�����ޣ�
    }

    procedure Add(Factor: TCnRationalNumber); overload;
    {* �����Ԫ�ؼ���һ��������

       ������
         Factor: TCnRationalNumber        - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Factor: TCnRationalNumber); overload;
    {* �����Ԫ�س���һ��������

       ������
         Factor: TCnRationalNumber        - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Factor: TCnRationalNumber); overload;
    {* �����Ԫ�س���һ��������

       ������
         Factor: TCnRationalNumber        - ����

       ����ֵ�����ޣ�
    }

    procedure SetE(Size: Integer);
    {* ����Ϊ Size �׵�λ����

       ������
         Size: Integer                    - ����

       ����ֵ�����ޣ�
    }

    procedure SetZero;
    {* ����Ϊȫ 0 ����}

    procedure Transpose;
    {* ����ת�ã�Ҳ�������л���}

    procedure DeleteRow(Row: Integer);
    {* ɾ������һ�С�

       ������
         Row: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    procedure DeleteCol(Col: Integer);
    {* ɾ������һ�С�

       ������
         Col: Integer                     - ��ɾ�����к�

       ����ֵ�����ޣ�
    }

    procedure Determinant(D: TCnRationalNumber);
    {* ��������ʽֵ��

       ������
         D: TCnRationalNumber             - �������ɽ����������

       ����ֵ�����ޣ�
    }

    procedure Trace(T: TCnRationalNumber);
    {* ����ļ���Ҳ���ǶԽ���Ԫ�صĺ͡�

       ������
         T: TCnRationalNumber             - �������ɽ����������

       ����ֵ�����ޣ�
    }

    function IsSquare: Boolean;
    {* �Ƿ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ���
    }

    function IsZero: Boolean;
    {* �Ƿ�ȫ 0 ����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�ȫ 0 ����
    }

    function IsE: Boolean;
    {* �Ƿ�λ����

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�λ����
    }

    function IsSymmetrical: Boolean;
    {* �Ƿ�ԳƷ���

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�ԳƷ���
    }

    function IsSingular: Boolean;
    {* �Ƿ����췽��Ҳ��������ʽ�Ƿ���� 0��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ����췽��
    }

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* ������ַ����б�

       ������
         List: TStrings                   - �������ɽ�����ַ����б�
         Sep: Char                        - �ָ���

       ����ֵ�����ޣ�
    }

    property Value[Row, Col: Integer]: TCnRationalNumber read GetValue write SetValue; default;
    {* ���������±���ʾ���Ԫ�أ��±궼�� 0 ��ʼ}
    property ZigZagValue[Index: Integer]: TCnRationalNumber read GetZigZagValue write SetZigZagValue;
    {* ���������Ͻǿ�ʼб�ŵĵ�ֵ����}
  published
    property ColCount: Integer read GetColCount write SetColCount;
    {* ��������}
    property RowCount: Integer read GetRowCount write SetRowCount;
    {* ��������}
  end;

// ============================ �����������㷽�� ===============================

procedure CnMatrixMul(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix); overload;
{* ����������ˣ������ MulResult �����У�Ҫ�� Matrix1 ������ Martrix2 ������ȡ�
   MulResult ������ Matrix1 �� Matrix2��

   ������
     Matrix1: TCnIntMatrix                - ��������һ
     Matrix2: TCnIntMatrix                - ���������
     MulResult: TCnIntMatrix              - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix); overload;
{* ���� K ���ݣ������ PowerResult �����У�PowerResult ������ Matrix��

   ������
     Matrix: TCnIntMatrix                 - ��������
     K: Integer                           - ָ��
     PowerResult: TCnIntMatrix            - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixAdd(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix; AddResult: TCnIntMatrix); overload;
{* ����������ӣ������ AddResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ������ȡ�
   AddResult ������ Matrix1 �� Matrix2��

   ������
     Matrix1: TCnIntMatrix                - ��������һ
     Matrix2: TCnIntMatrix                - ���������
     AddResult: TCnIntMatrix              - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixHadamardProduct(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix); overload;
{* ���������������ˣ������ ProductResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ��ȡ�
   ProductResult ������ Matrix1 �� Matrix2 ��������

   ������
     Matrix1: TCnIntMatrix                - ��������һ
     Matrix2: TCnIntMatrix                - ���������
     ProductResult: TCnIntMatrix          - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixTranspose(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix); overload;
{* ת�þ��󣬽���һ������ת�����ڶ�����Matrix1��Matrix2 ������ȡ�

   ������
     Matrix1: TCnIntMatrix                - ��ת�õ�ԭ����
     Matrix2: TCnIntMatrix                - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixMinor(Matrix: TCnIntMatrix; Row: Integer; Col: Integer;
  MinorResult: TCnIntMatrix); overload;
{* ����������ʽ��Ҳ��ȥ��ָ�����к�ʣ�µľ���

   ������
     Matrix: TCnIntMatrix                 - ��������ʽ��ԭ����
     Row: Integer                         - ȥ�����к�
     Col: Integer                         - ȥ�����к�
     MinorResult: TCnIntMatrix            - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixAdjoint(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix); overload;
{* ����İ�����

   ������
     Matrix1: TCnIntMatrix                - ����������ԭ����
     Matrix2: TCnIntMatrix                - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixInverse(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix); overload;
{* ����������Ҳ���ǰ������������ʽ��ע�� TCnIntMatrix ��ֱ��֧�������
   ��Ϊ�����ܵ��·���������Ҫ������������������ʾ��������٤�޻�����

   ������
     Matrix1: TCnIntMatrix                - ����������ԭ����
     Matrix2: TCnIntMatrix                - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

// =========================== �������������㷽�� ==============================

procedure CnMatrixMul(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix; MulResult: TCnFloatMatrix); overload;
{* ����������ˣ������ MulResult �����У�Ҫ�� Matrix1 ������ Martrix2 ������ȡ�
   MulResult ������ Matrix1 �� Matrix2��

   ������
     Matrix1: TCnFloatMatrix              - ��������һ
     Matrix2: TCnFloatMatrix              - ���������
     MulResult: TCnFloatMatrix            - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixPower(Matrix: TCnFloatMatrix; K: Integer; PowerResult: TCnFloatMatrix); overload;
{* ���� K ���ݣ������ PowerResult �����У�PowerResult ������ Matrix��

   ������
     Matrix: TCnFloatMatrix               - ��������
     K: Integer                           - ָ��
     PowerResult: TCnFloatMatrix          - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixAdd(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix;
  AddResult: TCnFloatMatrix); overload;
{* ����������ӣ������ AddResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ������ȡ�
   AddResult ������ Matrix1 �� Matrix2��

   ������
     Matrix1: TCnFloatMatrix              - ��������һ
     Matrix2: TCnFloatMatrix              - ���������
     AddResult: TCnFloatMatrix            - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixHadamardProduct(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix;
  ProductResult: TCnFloatMatrix); overload;
{* ���������������ˣ������ ProductResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ��ȡ�
   ProductResult ������ Matrix1 �� Matrix2 ��������

   ������
     Matrix1: TCnFloatMatrix              - ��������һ
     Matrix2: TCnFloatMatrix              - ���������
     ProductResult: TCnFloatMatrix        - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixTranspose(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix); overload;
{* ת�þ��󣬽���һ������ת�����ڶ�����Matrix1��Matrix2 ������ȡ�

   ������
     Matrix1: TCnFloatMatrix              - ��ת�õ�ԭ����
     Matrix2: TCnFloatMatrix              - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixMinor(Matrix: TCnFloatMatrix; Row: Integer; Col: Integer;
  MinorResult: TCnFloatMatrix); overload;
{* ����������ʽ��Ҳ��ȥ��ָ�����к�ʣ�µľ���

   ������
     Matrix: TCnFloatMatrix               - ��������ʽ��ԭ����
     Row: Integer                         - ȥ�����к�
     Col: Integer                         - ȥ�����к�
     MinorResult: TCnFloatMatrix          - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixAdjoint(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix); overload;
{* ����İ�����

   ������
     Matrix1: TCnFloatMatrix              - ����������ԭ����
     Matrix2: TCnFloatMatrix              - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixInverse(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix); overload;
{* ����������Ҳ���ǰ������������ʽ��

   ������
     Matrix1: TCnFloatMatrix              - ����������ԭ����
     Matrix2: TCnFloatMatrix              - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

// =========================== �������������㷽�� ==============================

procedure CnIntToRationalMatrix(Int: TCnIntMatrix; Rational: TCnRationalMatrix);
{* ��һ����������ת��Ϊ����������

   ������
     Int: TCnIntMatrix                    - ��ת������������
     Rational: TCnRationalMatrix          - �������ɽ��������������

   ����ֵ�����ޣ�
}

procedure CnMatrixMul(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix;
  MulResult: TCnRationalMatrix); overload;
{* ����������ˣ������ MulResult �����У�Ҫ�� Matrix1 ������ Martrix2 ������ȡ�
   MulResult ������ Matrix1 �� Matrix2��

   ������
     Matrix1: TCnRationalMatrix           - ��������һ
     Matrix2: TCnRationalMatrix           - ���������
     MulResult: TCnRationalMatrix         - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixPower(Matrix: TCnRationalMatrix; K: Integer; PowerResult: TCnRationalMatrix); overload;
{* ���� K ���ݣ������ PowerResult �����У�PowerResult ������ Matrix��

   ������
     Matrix: TCnRationalMatrix            - ��������
     K: Integer                           - ָ��
     PowerResult: TCnRationalMatrix       - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixAdd(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix;
  AddResult: TCnRationalMatrix); overload;
{* ����������ӣ������ AddResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ��ȡ�
   AddResult ������ Matrix1 �� Matrix2��

   ������
     Matrix1: TCnRationalMatrix           - ��������һ
     Matrix2: TCnRationalMatrix           - ���������
     AddResult: TCnRationalMatrix         - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixHadamardProduct(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix;
  ProductResult: TCnRationalMatrix); overload;
{* ���������������ˣ������ ProductResult �����У�Ҫ�� Matrix1 �ߴ��� Martrix2 ��ȡ�
   ProductResult ������ Matrix1 �� Matrix2��

   ������
     Matrix1: TCnRationalMatrix           - ��������һ
     Matrix2: TCnRationalMatrix           - ���������
     ProductResult: TCnRationalMatrix     - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixTranspose(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix); overload;
{* ת�þ��󣬽���һ������ת�����ڶ�����Matrix1��Matrix2 ������ȡ�

   ������
     Matrix1: TCnRationalMatrix           - ��ת�õ�ԭ����
     Matrix2: TCnRationalMatrix           - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixMinor(Matrix: TCnRationalMatrix; Row: Integer; Col: Integer;
  MinorResult: TCnRationalMatrix); overload;
{* ����������ʽ��Ҳ��ȥ��ָ�����к�ʣ�µľ���

   ������
     Matrix: TCnRationalMatrix            - ��������ʽ��ԭ����
     Row: Integer                         - ȥ�����к�
     Col: Integer                         - ȥ�����к�
     MinorResult: TCnRationalMatrix       - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixAdjoint(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix); overload;
{* ����İ�����

   ������
     Matrix1: TCnRationalMatrix           - ����������ԭ����
     Matrix2: TCnRationalMatrix           - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

procedure CnMatrixInverse(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix); overload;
{* ����������Ҳ���ǰ������������ʽ����Ҫ��������������ʾ��

   ������
     Matrix1: TCnRationalMatrix           - ����������ԭ����
     Matrix2: TCnRationalMatrix           - �������ɽ���ľ���

   ����ֵ�����ޣ�
}

// ============================== ���������㷽�� ===============================

procedure CnRationalNumberAdd(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* �������ӷ�����������������ͬһ����

   ������
     Number1: TCnRationalNumber           - ����һ
     Number2: TCnRationalNumber           - ������
     RationalResult: TCnRationalNumber    - �������ɽ����������

   ����ֵ�����ޣ�
}

procedure CnRationalNumberAdd3(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �������������ӷ���RationalResult ������ Number1 �� Number2 �� Number3��

   ������
     Number1: TCnRationalNumber           - ����һ
     Number2: TCnRationalNumber           - ������
     Number3: TCnRationalNumber           - ������
     RationalResult: TCnRationalNumber    - �������ɽ����������

   ����ֵ�����ޣ�
}

procedure CnRationalNumberSub(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* ��������������������������ͬһ����

   ������
     Number1: TCnRationalNumber           - ������
     Number2: TCnRationalNumber           - ����
     RationalResult: TCnRationalNumber    - �������ɽ����������

   ����ֵ�����ޣ�
}

procedure CnRationalNumberMul(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* �������˷�����������������ͬһ����

   ������
     Number1: TCnRationalNumber           - ����һ
     Number2: TCnRationalNumber           - ������
     RationalResult: TCnRationalNumber    - �������ɽ����������

   ����ֵ�����ޣ�
}

procedure CnRationalNumberMul3(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* �������������˷���RationalResult ������ Number1 �� Number2��

   ������
     Number1: TCnRationalNumber           - ����һ
     Number2: TCnRationalNumber           - ������
     Number3: TCnRationalNumber           - ������
     RationalResult: TCnRationalNumber    - �������ɽ����������

   ����ֵ�����ޣ�
}

procedure CnRationalNumberDiv(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* ��������������������������ͬһ����

   ������
     Number1: TCnRationalNumber           - ������
     Number2: TCnRationalNumber           - ����
     RationalResult: TCnRationalNumber    - �������ɽ����������

   ����ֵ�����ޣ�
}

function CnRationalNumberCompare(Number1: TCnRationalNumber; Number2: TCnRationalNumber): Integer;
{* �Ƚ�������������ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1��

   ������
     Number1: TCnRationalNumber           - ���Ƚϵ�������һ
     Number2: TCnRationalNumber           - ���Ƚϵ���������

   ����ֵ��Integer                        - ���رȽϽ��
}

procedure CnReduceInt64(var X: Int64; var Y: Int64);
{* ������������С��Ҳ����Լ�֡�

   ������
     var X: Int64                         - ��Լ�ֵķ���
     var Y: Int64                         - ��Լ�ֵķ�ĸ

   ����ֵ�����ޣ�
}

function RowColToZigZag(ARow: Integer; ACol: Integer; N: Integer): Integer;
{* �� N �׷����е�����ֵת��Ϊ���Ͻ�б�ŵ�����ֵ���� 0 ��ʼ��

   ������
     ARow: Integer                        - �к�
     ACol: Integer                        - �к�
     N: Integer                           - �������

   ����ֵ��Integer                        - ��������ֵ
}

procedure ZigZagToRowCol(Index: Integer; out ARow: Integer; out ACol: Integer; N: Integer);
{* �� N �׷����е����Ͻ�б�ŵ�����ֵת��Ϊ����ֵ���� 0 ��ʼ��

   ������
     Index: Integer                       - ����ֵ
     out ARow: Integer                    - ������к�
     out ACol: Integer                    - ������к�
     N: Integer                           - �������

   ����ֵ�����ޣ�
}

implementation

resourcestring
  SCnErrorRowColCountFmt = 'Error Row or Col Count: %d';
  SCnErrorRowColIndexFmt = 'Error Row or Col: %d';
  SCnErrorRowColIndex2Fmt = 'Error Row or Col: %d, %d';
  SCnErrorZigZagIndexFmt = 'Error ZigZag Index: %d';
  SCnErrorZigZagRowColCount = 'ZigZag Row Col Count must Equal';
  SCnErrorResultFactors = 'Matrix Result can not Be Factors';
  SCnErrorMulRowCount = 'Matrix 1 Col Count must Equal to Matrix 2 Row Count';
  SCnErrorPowerSquare = 'Matrix Power Must Be Square';
  SCnErrorDivNotImplInt = 'Operation Div NOT Implemented in Int Matrix';
  SCnErrorTraceSquare = 'Only Square Matrix can Trace';
  SCnErrorInvalidPower = 'Invalid Matrix Power';
  SCnErrorRowColMustEqual = 'Matrix 1/2 Row/Col Count must Equal';
  SCnErrorRowColMinorFmt = 'Invalid Minor Row or Col %d, %d';
  SCnErrorAdjointSquare = 'Only Square can Adjoint';
  SCnErrorInverseZeroDeteminant = 'NO Inverse Matrix for Deteminant is 0';
  SCnErrorDeterminantSquare = 'Only Square can Determinant';

procedure CheckCount(Value: Integer);
begin
  if Value <= 0 then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColCountFmt, [Value]);
end;

procedure CheckIndex(Value: Integer);
begin
  if Value < 0 then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColIndexFmt, [Value]);
end;

{
  0  1  5  6
  2  4  7 12
  3  8 11 13
  9 10 14 15
}
// �� N �׷����е�����ֵת��Ϊ���Ͻ�б�ŵ�����ֵ���� 0 ��ʼ
function RowColToZigZag(ARow, ACol: Integer; N: Integer): Integer;
var
  L, A, T: Integer;
begin
  CheckIndex(ARow);
  CheckIndex(ACol);
  if (ARow >= N) or (ACol >= N) then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColIndex2Fmt, [ARow, ACol]);

  // �ڵ� Row + Col + 1 б�㣨���Ͻ�Ϊ�� 1 б�㣩������֮ǰ����б��������ͣ�����б���ڵ�ƫ��
  // ���б���ǵ� N �㣨�� N(N+1)/2 �������ܹ��� 2N - 1 ��
  L := ARow + ACol;
  if L <= N then
    A := L * (L + 1) div 2 // A ��֮ǰ����б���������
  else
  begin
    A := N * (N + 1) div 2;  // 1 �� N ��б���
    T := 2 * N - 1 - (ARow + ACol);
    A := A + (A - N - (T * (T + 1) div 2)); // N + 1 ���б���
  end;
  // A ��֮ǰ����б���������

  if L and 1 = 0 then
  begin
    // ��������б�㣬������������
    if L < N then
      Result := A + ACol
    else
    Result := A + ACol - (L + 1 - N);
  end
  else
  begin
    // ����ż��б�㣬������������
    if L < N then
      Result := A + ARow
    else
      Result := A + ARow - (L + 1 - N);
  end;
end;

// �� N �׷����е����Ͻ�б�ŵ�����ֵת��Ϊ����ֵ���� 0 ��ʼ
procedure ZigZagToRowCol(Index: Integer; out ARow, ACol: Integer; N: Integer);
var
  L, A: Integer;

  procedure FindLevelIndex(var Level, IndexLevel: Integer);
  var
    TA: Integer;
  begin
    TA := Trunc(Sqrt(IndexLevel * 2 + 2));
    if TA * TA + TA < IndexLevel * 2 + 2 then
    begin
      Level := TA;
      IndexLevel := IndexLevel - (TA * TA + TA) div 2;
    end
    else
    begin
      Level := TA - 1;
      if Level < 0 then
        Level := 0;
      IndexLevel := IndexLevel - (Level * Level + Level) div 2;
    end;
  end;

begin
  CheckIndex(Index);
  if Index > (N * N - 1) then
    raise ECnMatrixException.CreateFmt(SCnErrorZigZagIndexFmt, [Index]);

  A := N * (N + 1) div 2; // A �Ǵ����ϵ������Խ������ڵ���������

  L := 0;
  if Index < A then
  begin
    // ���������������Խ���
    FindLevelIndex(L, Index);
    // L �����ϲ������Լ�����������������Ϊ 0�������� Row + Col = L��Index �Ǳ����е�ƫ�ƣ�0 ��ʼ��������жϣ�

    if L and 1 = 0 then
    begin
      // ������������������б�㣬������������
      ACol := Index;
      ARow := L - Index;
    end
    else
    begin
      // ��������������ż��б�㣬������������
      ARow := Index;
      ACol := L - Index;
    end;
  end
  else
  begin
    // �����������������Խ���
    Index := N * N - 1 - Index;
    FindLevelIndex(L, Index);
    Index := L - Index;
    // L �����²������Լ�����������������Ϊ 0�������� (N - 1 - Row) + (N - 1 - Col) = L��
    // Ҳ���� (Row + Col = 2N - 2 - L)���� Index �Ǳ����е�ƫ�ƣ�0 ��ʼ��������жϣ�

    if L and 1 = 0 then
    begin
      // ������������������б�㣬������������
      ARow := N - 1 - Index;
      ACol := 2 * N - 2 - L - ARow;
    end
    else
    begin
      // ��������������ż��б�㣬������������
      ACol := N - 1 - Index;
      ARow := 2 * N - 2 - L - ACol;
    end;
  end;
end;

// ���� -1 �� N �η��������������ʽ��
function InternalNegativeOnePower(N: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (N and 1) * (-2) + 1;
end;

procedure CnIntToRationalMatrix(Int: TCnIntMatrix; Rational: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Int <> nil) and (Rational <> nil) then
  begin
    Rational.ColCount := Int.ColCount;
    Rational.RowCount := Int.RowCount;

    for I := 0 to Rational.RowCount - 1 do
      for J := 0 to Rational.ColCount - 1 do
        Rational.Value[I, J].SetIntValue(Int.Value[I, J]);
  end;
end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix);
var
  I, J, K: Integer;
  T, Sum: Int64;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create(SCnErrorResultFactors);

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create(SCnErrorMulRowCount);

  MulResult.RowCount := Matrix1.RowCount;
  MulResult.ColCount := Matrix2.ColCount;

  // Value[I, J] := ���� 1 �� I ������� 2 �� J �ж�Ӧ�˲����
  for I := 0 to Matrix1.RowCount - 1 do
  begin
    for J := 0 to Matrix2.ColCount - 1 do
    begin
      Sum := 0;
      for K := 0 to Matrix1.ColCount - 1 do
      begin
        T := Matrix1.OperationMul(Matrix1.Value[I, K], Matrix2.Value[K, J]);
        Sum := Matrix1.OperationAdd(Sum, T);
      end;
      MulResult.Value[I, J] := Sum;
    end;
  end;
end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnFloatMatrix; MulResult: TCnFloatMatrix);
var
  I, J, K: Integer;
  T, Sum: Extended;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create(SCnErrorResultFactors);

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create(SCnErrorMulRowCount);

  MulResult.RowCount := Matrix1.RowCount;
  MulResult.ColCount := Matrix2.ColCount;

  // Value[I, J] := ���� 1 �� I ������� 2 �� J �ж�Ӧ�˲����
  for I := 0 to Matrix1.RowCount - 1 do
  begin
    for J := 0 to Matrix2.ColCount - 1 do
    begin
      Sum := 0;
      for K := 0 to Matrix1.ColCount - 1 do
      begin
        T := Matrix1.OperationMul(Matrix1.Value[I, K], Matrix2.Value[K, J]);
        Sum := Matrix1.OperationAdd(Sum, T);
      end;
      MulResult.Value[I, J] := Sum;
    end;
  end;
end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnRationalMatrix; MulResult: TCnRationalMatrix);
var
  I, J, K: Integer;
  T, Sum: TCnRationalNumber;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create(SCnErrorResultFactors);

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create(SCnErrorMulRowCount);

  MulResult.RowCount := Matrix1.RowCount;
  MulResult.ColCount := Matrix2.ColCount;

  Sum := TCnRationalNumber.Create;
  T := TCnRationalNumber.Create;

  // Value[I, J] := ���� 1 �� I ������� 2 �� J �ж�Ӧ�˲����
  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        Sum.SetIntValue(0);
        for K := 0 to Matrix1.ColCount - 1 do
        begin
          CnRationalNumberMul(Matrix1.Value[I, K], Matrix2.Value[K, J], T);
          CnRationalNumberAdd(Sum, T, Sum);
        end;
        MulResult.Value[I, J] := Sum;
      end;
    end;
  finally
    Sum.Free;
  end;
end;

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix);
var
  I: Integer;
  T: TCnIntMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create(SCnErrorPowerSquare);

  if K < 0 then
    raise ECnMatrixException.Create(SCnErrorInvalidPower);

  if K = 0 then
  begin
    PowerResult.SetE(Matrix.RowCount);
    Exit;
  end
  else if K = 1 then
  begin
    PowerResult.Assign(Matrix);
    Exit;
  end;

  T := TCnIntMatrix.Create(Matrix.RowCount, Matrix.ColCount);
  try
    T.Assign(Matrix);
    for I := 0 to K - 2 do
    begin
      CnMatrixMul(Matrix, T, PowerResult);
      T.Assign(PowerResult);
    end;
  finally
    T.Free;
  end;
end;

procedure CnMatrixPower(Matrix: TCnFloatMatrix; K: Integer; PowerResult: TCnFloatMatrix);
var
  I: Integer;
  T: TCnFloatMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create(SCnErrorPowerSquare);

  if K < 0 then
    raise ECnMatrixException.Create(SCnErrorInvalidPower);

  if K = 0 then
  begin
    PowerResult.SetE(Matrix.RowCount);
    Exit;
  end
  else if K = 1 then
  begin
    PowerResult.Assign(Matrix);
    Exit;
  end;

  T := TCnFloatMatrix.Create(Matrix.RowCount, Matrix.ColCount);
  try
    T.Assign(Matrix);
    for I := 0 to K - 2 do
    begin
      CnMatrixMul(Matrix, T, PowerResult);
      T.Assign(PowerResult);
    end;
  finally
    T.Free;
  end;
end;

procedure CnMatrixPower(Matrix: TCnRationalMatrix; K: Integer; PowerResult: TCnRationalMatrix);
var
  I: Integer;
  T: TCnRationalMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create(SCnErrorPowerSquare);

  if K < 0 then
    raise ECnMatrixException.Create(SCnErrorInvalidPower);

  if K = 0 then
  begin
    PowerResult.SetE(Matrix.RowCount);
    Exit;
  end
  else if K = 1 then
  begin
    PowerResult.Assign(Matrix);
    Exit;
  end;

  T := TCnRationalMatrix.Create(Matrix.RowCount, Matrix.ColCount);
  try
    T.Assign(Matrix);
    for I := 0 to K - 2 do
    begin
      CnMatrixMul(Matrix, T, PowerResult);
      T.Assign(PowerResult);
    end;
  finally
    T.Free;
  end;
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnIntMatrix; AddResult: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      AddResult.Value[I, J] := Matrix1.OperationAdd(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnFloatMatrix; AddResult: TCnFloatMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      AddResult.Value[I, J] := Matrix1.OperationAdd(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnRationalMatrix; AddResult: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      CnRationalNumberAdd(Matrix1.Value[I, J], Matrix2.Value[I, J], AddResult.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      ProductResult.Value[I, J] := Matrix1.OperationMul(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnFloatMatrix; ProductResult: TCnFloatMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      ProductResult.Value[I, J] := Matrix1.OperationMul(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnRationalMatrix; ProductResult: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      CnRationalNumberMul(Matrix1.Value[I, J], Matrix2.Value[I, J], ProductResult.Value[I, J]);
end;

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnIntMatrix);
var
  I, J: Integer;
  Tmp: TCnIntMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnIntMatrix.Create(1, 1);
    try
      Tmp.Assign(Matrix1);
      Matrix2.ColCount := Tmp.RowCount;
      Matrix2.RowCount := Tmp.ColCount;

      for I := 0 to Tmp.RowCount - 1 do
        for J := 0 to Tmp.ColCount - 1 do
          Matrix2.Value[J, I] := Tmp.Value[I, J];
    finally
      Tmp.Free;
    end;
  end
  else
  begin
    Matrix2.ColCount := Matrix1.RowCount;
    Matrix2.RowCount := Matrix1.ColCount;

    for I := 0 to Matrix1.RowCount - 1 do
      for J := 0 to Matrix1.ColCount - 1 do
        Matrix2.Value[J, I] := Matrix1.Value[I, J];
  end;
end;

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnFloatMatrix);
var
  I, J: Integer;
  Tmp: TCnFloatMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnFloatMatrix.Create(1, 1);
    try
      Tmp.Assign(Matrix1);
      Matrix2.ColCount := Tmp.RowCount;
      Matrix2.RowCount := Tmp.ColCount;

      for I := 0 to Tmp.RowCount - 1 do
        for J := 0 to Tmp.ColCount - 1 do
          Matrix2.Value[J, I] := Tmp.Value[I, J];
    finally
      Tmp.Free;
    end;
  end
  else
  begin
    Matrix2.ColCount := Matrix1.RowCount;
    Matrix2.RowCount := Matrix1.ColCount;

    for I := 0 to Matrix1.RowCount - 1 do
      for J := 0 to Matrix1.ColCount - 1 do
        Matrix2.Value[J, I] := Matrix1.Value[I, J];
  end;
end;

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnRationalMatrix);
var
  I, J: Integer;
  Tmp: TCnRationalMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnRationalMatrix.Create(1, 1);
    try
      Tmp.Assign(Matrix1);
      Matrix2.ColCount := Tmp.RowCount;
      Matrix2.RowCount := Tmp.ColCount;

      for I := 0 to Tmp.RowCount - 1 do
        for J := 0 to Tmp.ColCount - 1 do
          Matrix2.Value[J, I] := Tmp.Value[I, J];
    finally
      Tmp.Free;
    end;
  end
  else
  begin
    Matrix2.ColCount := Matrix1.RowCount;
    Matrix2.RowCount := Matrix1.ColCount;

    for I := 0 to Matrix1.RowCount - 1 do
      for J := 0 to Matrix1.ColCount - 1 do
        Matrix2.Value[J, I] := Matrix1.Value[I, J];
  end;
end;

procedure CnMatrixMinor(Matrix: TCnIntMatrix; Row, Col: Integer; MinorResult: TCnIntMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColMinorFmt, [Row, Col]);

  MinorResult.ColCount := Matrix.ColCount - 1;
  MinorResult.RowCount := Matrix.RowCount - 1;

  SR := 0;
  DR := 0;

  while SR < Matrix.RowCount do
  begin
    if SR = Row then
    begin
      Inc(SR);
      if SR = Matrix.RowCount then
        Break;
    end;

    SC := 0;
    DC := 0;
    while SC < Matrix.ColCount do
    begin
      if SC = Col then
      begin
        Inc(SC);
        if SC = Matrix.ColCount then
          Break;
      end;

      MinorResult.Value[DR, DC] := Matrix.Value[SR, SC];
      Inc(SC);
      Inc(DC);
    end;

    Inc(SR);
    Inc(DR);
  end;
end;

procedure CnMatrixMinor(Matrix: TCnFloatMatrix; Row, Col: Integer; MinorResult: TCnFloatMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColMinorFmt, [Row, Col]);

  MinorResult.ColCount := Matrix.ColCount - 1;
  MinorResult.RowCount := Matrix.RowCount - 1;

  SR := 0;
  DR := 0;

  while SR < Matrix.RowCount do
  begin
    if SR = Row then
    begin
      Inc(SR);
      if SR = Matrix.RowCount then
        Break;
    end;

    SC := 0;
    DC := 0;
    while SC < Matrix.ColCount do
    begin
      if SC = Col then
      begin
        Inc(SC);
        if SC = Matrix.ColCount then
          Break;
      end;

      MinorResult.Value[DR, DC] := Matrix.Value[SR, SC];
      Inc(SC);
      Inc(DC);
    end;

    Inc(SR);
    Inc(DR);
  end;
end;

procedure CnMatrixMinor(Matrix: TCnRationalMatrix; Row, Col: Integer; MinorResult: TCnRationalMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColMinorFmt, [Row, Col]);

  MinorResult.ColCount := Matrix.ColCount - 1;
  MinorResult.RowCount := Matrix.RowCount - 1;

  SR := 0;
  DR := 0;

  while SR < Matrix.RowCount do
  begin
    if SR = Row then
    begin
      Inc(SR);
      if SR = Matrix.RowCount then
        Break;
    end;

    SC := 0;
    DC := 0;
    while SC < Matrix.ColCount do
    begin
      if SC = Col then
      begin
        Inc(SC);
        if SC = Matrix.ColCount then
          Break;
      end;

      MinorResult.Value[DR, DC] := Matrix.Value[SR, SC];
      Inc(SC);
      Inc(DC);
    end;

    Inc(SR);
    Inc(DR);
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnIntMatrix);
var
  I, J: Integer;
  Minor: TCnIntMatrix;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create(SCnErrorAdjointSquare);

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnIntMatrix(Matrix1.ClassType.NewInstance);
  Minor.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1); // ������ʵ��

  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Matrix2.Value[I, J] := Matrix1.NegativeOnePower(I + J) * Minor.Determinant;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    Minor.Free;
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnFloatMatrix);
var
  I, J: Integer;
  Minor: TCnFloatMatrix;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create(SCnErrorAdjointSquare);

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnFloatMatrix(Matrix1.ClassType.NewInstance);
  Minor.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1); // ������ʵ��

  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Matrix2.Value[I, J] := Matrix1.NegativeOnePower(I + J) * Minor.Determinant;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    Minor.Free;
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnRationalMatrix);
var
  I, J: Integer;
  Minor: TCnRationalMatrix;
  T: TCnRationalNumber;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create(SCnErrorAdjointSquare);

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnRationalMatrix.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1);
  T := TCnRationalNumber.Create;
  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Minor.Determinant(T);
        T.Mul(InternalNegativeOnePower(I + J));
        Matrix2.Value[I, J] := T;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    T.Free;
    Minor.Free;
  end;
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnIntMatrix);
var
  D: Int64;
begin
  D := Matrix1.Determinant;
  if D = 0 then
    raise ECnMatrixException.Create(SCnErrorInverseZeroDeteminant);

  CnMatrixAdjoint(Matrix1, Matrix2);
  Matrix2.Divide(D);
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnFloatMatrix);
var
  D: Extended;
begin
  D := Matrix1.Determinant;
  if FloatAlmostZero(D) then
    raise ECnMatrixException.Create(SCnErrorInverseZeroDeteminant);

  CnMatrixAdjoint(Matrix1, Matrix2);
  Matrix2.Divide(D);
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnRationalMatrix);
var
  D: TCnRationalNumber;
begin
  D := TCnRationalNumber.Create;
  try
    Matrix1.Determinant(D);
    if D.IsZero then
      raise ECnMatrixException.Create(SCnErrorInverseZeroDeteminant);

    CnMatrixAdjoint(Matrix1, Matrix2);
    Matrix2.Divide(D);
  finally
    D.Free;
  end;
end;

{ TCnIntMatrix }

procedure TCnIntMatrix.Add(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationAdd(FMatrix[I, J], Factor);
end;

function TCnIntMatrix.Add3(X, Y, Z: Int64): Int64;
begin
  Result := OperationAdd(OperationAdd(X, Y), Z);
end;

procedure TCnIntMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnIntMatrix then
  begin
    TCnIntMatrix(Dest).RowCount := FRowCount;
    TCnIntMatrix(Dest).ColCount := FColCount;

    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        TCnIntMatrix(Dest).Value[I, J] := FMatrix[I, J];
  end
  else
    inherited;
end;

constructor TCnIntMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRowCount := ARowCount;
  FColCount := AColCount;
  SetLength(FMatrix, FRowCount, FColCount);
end;

procedure TCnIntMatrix.DeleteCol(Col: Integer);
var
  T: array of array of Int64;
  I, J, SJ, DJ: Integer;
begin
  if (Col >= 0) or (Col < FColCount) then
  begin
    // ��ÿ Row ��Ԫ��ȡ�����ŵ���ʱ T ��޳��� Col ��
    SetLength(T, FRowCount, FColCount - 1);

    for I := 0 to FRowCount - 1 do
    begin
      SJ := 0;
      DJ := 0;
      while SJ < FColCount do
      begin
        if SJ = Col then
        begin
          Inc(SJ);
          Continue;
        end;
        T[I, DJ] := FMatrix[I, SJ];
        Inc(SJ);
        Inc(DJ);
      end;
    end;

    Dec(FColCount);
    SetLength(FMatrix, FRowCount, FColCount);
    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        FMatrix[I, J] := T[I, J];

    SetLength(T, 0);
  end;
end;

procedure TCnIntMatrix.DeleteRow(Row: Integer);
var
  I, J: Integer;
begin
  if (Row >= 0) or (Row < FRowCount) then
  begin
    // �ѵ� Row + 1 �е� FRowCount - 1 �е�һά���鳯ǰ�ƶ�һ��ĩ��ʱ������
    if Row < FRowCount - 1 then
    begin
      for I := Row + 1 to FRowCount - 1 do
      begin
        for J := 0 to FColCount - 1 do
        begin
          FMatrix[I - 1, J] := FMatrix[I, J];
        end;
      end;
    end;
    Dec(FRowCount);
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

destructor TCnIntMatrix.Destroy;
begin
  SetLength(FMatrix, 0);
  inherited;
end;

function TCnIntMatrix.Determinant: Int64;
var
  I: Integer;
  Minor: TCnIntMatrix;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorDeterminantSquare);

  if FRowCount = 1 then
    Result := FMatrix[0, 0]
  else if FRowCount = 2 then
    Result := FMatrix[0, 0] * FMatrix[1, 1] - FMatrix[0, 1] * FMatrix[1, 0]
  else if RowCount = 3 then
  begin
    Result := OperationSub(Add3(Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2]),
      Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0]),
      Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])),
      Add3(Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1]),
        Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2]),
        Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0])));
  end
  else
  begin
    // ���ô�������ʽ Minor/Cofactor ����߽�����ʽ
    Result := 0;
    Minor := TCnIntMatrix(ClassType.NewInstance); // ��Ҫ���������ͳһ������
    Minor.Create(FRowCount - 1, FColCount - 1);

    // Minor := Self.clas TCnIntMatrix.Create(FRowCount - 1, FColCount - 1);
    try
      for I := 0 to FColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);
        Result := OperationAdd(Result, Mul3(FMatrix[0, I], NegativeOnePower(I), Minor.Determinant));
      end;
    finally
      Minor.Free;
    end;
  end;
end;

procedure TCnIntMatrix.Divide(Factor: Int64);
begin
  raise ECnMatrixException.Create(SCnErrorDivNotImplInt);
end;

procedure TCnIntMatrix.DumpToStrings(List: TStrings; Sep: Char = ' ');
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  for I := 0 to FRowCount - 1 do
  begin
    S := '';
    for J := 0 to FColCount - 1 do
    begin
      if J = 0 then
        S := IntToStr(FMatrix[I, J])
      else
        S := S + Sep + IntToStr(FMatrix[I, J]);
    end;
    List.Add(S);
  end;
end;

function TCnIntMatrix.GetValue(Row, Col: Integer): Int64;
begin
  Result := FMatrix[Row, Col];
end;

function TCnIntMatrix.GetZigZagValue(Index: Integer): Int64;
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  Result := GetValue(R, C);
end;

function TCnIntMatrix.IsE: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if (I = J) and (FMatrix[I, J] <> 1) then
      begin
        Result := False;
        Exit;
      end
      else if (I <> J) and (FMatrix[I, J] <> 0) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCnIntMatrix.IsSingular: Boolean;
begin
  if not IsSquare then
    Result := False
  else
    Result := Determinant = 0;
end;

function TCnIntMatrix.IsSquare: Boolean;
begin
  Result := (FColCount = FRowCount);
end;

function TCnIntMatrix.IsSymmetrical: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
    for J := 0 to I do
      if FMatrix[I, J] <> FMatrix[J, I] then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

function TCnIntMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if not IsSquare then
    Exit;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if FMatrix[I, J] <> 0 then
        Exit;
    end;
  end;

  Result := True;
end;

procedure TCnIntMatrix.Mul(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationMul(FMatrix[I, J], Factor);
end;

function TCnIntMatrix.Mul3(X, Y, Z: Int64): Int64;
begin
  Result := OperationMul(OperationMul(X, Y), Z);
end;

function TCnIntMatrix.NegativeOnePower(N: Integer): Integer;
begin
  Result := InternalNegativeOnePower(N);
end;

function TCnIntMatrix.OperationAdd(X, Y: Int64): Int64;
begin
  Result := X + Y;
end;

function TCnIntMatrix.OperationDiv(X, Y: Int64): Int64;
begin
  raise ECnMatrixException.Create(SCnErrorDivNotImplInt);
end;

function TCnIntMatrix.OperationMul(X, Y: Int64): Int64;
begin
  Result := X * Y;
end;

function TCnIntMatrix.OperationSub(X, Y: Int64): Int64;
begin
  Result := X - Y;
end;

procedure TCnIntMatrix.SetColCount(const Value: Integer);
begin
  if FColCount <> Value then
  begin
    CheckCount(Value);
    FColCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnIntMatrix.SetE(Size: Integer);
var
  I, J: Integer;
begin
  CheckCount(Size);

  RowCount := Size;
  ColCount := Size;
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        FMatrix[I, J] := 1
      else
        FMatrix[I, J] := 0;
end;

procedure TCnIntMatrix.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
  begin
    CheckCount(Value);
    FRowCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnIntMatrix.SetValue(Row, Col: Integer; const AValue: Int64);
begin
  FMatrix[Row, Col] := AValue;
end;

procedure TCnIntMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := 0;
end;

procedure TCnIntMatrix.SetZigZagValue(Index: Integer; const Value: Int64);
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  SetValue(R, C, Value);
end;

function TCnIntMatrix.Trace: Int64;
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorTraceSquare);

  Result := 0;
  for I := 0 to FRowCount - 1 do
    Result := OperationAdd(Result, FMatrix[I, I]);
end;

procedure TCnIntMatrix.Transpose;
begin
  CnMatrixTranspose(Self, Self);
end;

{ TCnFloatMatrix }

procedure TCnFloatMatrix.Add(Factor: Extended);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationAdd(FMatrix[I, J], Factor);
end;

function TCnFloatMatrix.Add3(X, Y, Z: Extended): Extended;
begin
  Result := OperationAdd(OperationAdd(X, Y), Z);
end;

procedure TCnFloatMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnFloatMatrix then
  begin
    TCnFloatMatrix(Dest).RowCount := FRowCount;
    TCnFloatMatrix(Dest).ColCount := FColCount;

    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        TCnFloatMatrix(Dest).Value[I, J] := FMatrix[I, J];
  end
  else
    inherited;
end;

constructor TCnFloatMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRowCount := ARowCount;
  FColCount := AColCount;
  SetLength(FMatrix, FRowCount, FColCount);
end;

procedure TCnFloatMatrix.DeleteCol(Col: Integer);
var
  T: array of array of Extended;
  I, J, SJ, DJ: Integer;
begin
  if (Col >= 0) or (Col < FColCount) then
  begin
    // ��ÿ Row ��Ԫ��ȡ�����ŵ���ʱ T ��޳��� Col ��
    SetLength(T, FRowCount, FColCount - 1);

    for I := 0 to FRowCount - 1 do
    begin
      SJ := 0;
      DJ := 0;
      while SJ < FColCount do
      begin
        if SJ = Col then
        begin
          Inc(SJ);
          Continue;
        end;
        T[I, DJ] := FMatrix[I, SJ];
        Inc(SJ);
        Inc(DJ);
      end;
    end;

    Dec(FColCount);
    SetLength(FMatrix, FRowCount, FColCount);
    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        FMatrix[I, J] := T[I, J];

    SetLength(T, 0);
  end;
end;

procedure TCnFloatMatrix.DeleteRow(Row: Integer);
var
  I, J: Integer;
begin
  if (Row >= 0) or (Row < FRowCount) then
  begin
    // �ѵ� Row + 1 �е� FRowCount - 1 �е�һά���鳯ǰ�ƶ�һ��ĩ��ʱ������
    if Row < FRowCount - 1 then
    begin
      for I := Row + 1 to FRowCount - 1 do
      begin
        for J := 0 to FColCount - 1 do
        begin
          FMatrix[I - 1, J] := FMatrix[I, J];
        end;
      end;
    end;
    Dec(FRowCount);
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

destructor TCnFloatMatrix.Destroy;
begin
  SetLength(FMatrix, 0);
  inherited;
end;

function TCnFloatMatrix.Determinant: Extended;
var
  I: Integer;
  Minor: TCnFloatMatrix;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorDeterminantSquare);

  if FRowCount = 1 then
    Result := FMatrix[0, 0]
  else if FRowCount = 2 then
    Result := FMatrix[0, 0] * FMatrix[1, 1] - FMatrix[0, 1] * FMatrix[1, 0]
  else if RowCount = 3 then
  begin
    Result := OperationSub(Add3(Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2]),
      Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0]),
      Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])),
      Add3(Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1]),
        Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2]),
        Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0])));
  end
  else
  begin
    // ���ô�������ʽ Minor/Cofactor ����߽�����ʽ
    Result := 0;
    Minor := TCnFloatMatrix(ClassType.NewInstance); // ��Ҫ���������ͳһ������
    Minor.Create(FRowCount - 1, FColCount - 1);

    // Minor := Self.clas TCnFloatMatrix.Create(FRowCount - 1, FColCount - 1);
    try
      for I := 0 to FColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);
        Result := OperationAdd(Result, Mul3(FMatrix[0, I], NegativeOnePower(I), Minor.Determinant));
      end;
    finally
      Minor.Free;
    end;
  end;
end;

procedure TCnFloatMatrix.Divide(Factor: Extended);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationDiv(FMatrix[I, J], Factor);
end;

procedure TCnFloatMatrix.DumpToStrings(List: TStrings; Sep: Char);
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  for I := 0 to FRowCount - 1 do
  begin
    S := '';
    for J := 0 to FColCount - 1 do
    begin
      if J = 0 then
        S := FloatToStr(FMatrix[I, J])
      else
        S := S + Sep + FloatToStr(FMatrix[I, J]);
    end;
    List.Add(S);
  end;
end;

function TCnFloatMatrix.GetValue(Row, Col: Integer): Extended;
begin
  Result := FMatrix[Row, Col];
end;

function TCnFloatMatrix.GetZigZagValue(Index: Integer): Extended;
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  Result := GetValue(R, C);
end;

function TCnFloatMatrix.IsE: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if (I = J) and (FMatrix[I, J] <> 1) then
      begin
        Result := False;
        Exit;
      end
      else if (I <> J) and (FMatrix[I, J] <> 0) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCnFloatMatrix.IsSingular: Boolean;
begin
  if not IsSquare then
    Result := False
  else
    Result := FloatAlmostZero(Determinant);
end;

function TCnFloatMatrix.IsSquare: Boolean;
begin
  Result := (FColCount = FRowCount);
end;

function TCnFloatMatrix.IsSymmetrical: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
    for J := 0 to I do
      if FMatrix[I, J] <> FMatrix[J, I] then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

function TCnFloatMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if not IsSquare then
    Exit;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if FMatrix[I, J] <> 0 then
        Exit;
    end;
  end;

  Result := True;
end;

procedure TCnFloatMatrix.Mul(Factor: Extended);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationMul(FMatrix[I, J], Factor);
end;

function TCnFloatMatrix.Mul3(X, Y, Z: Extended): Extended;
begin
  Result := OperationMul(OperationMul(X, Y), Z);
end;

function TCnFloatMatrix.NegativeOnePower(N: Integer): Integer;
begin
  Result := InternalNegativeOnePower(N);
end;

function TCnFloatMatrix.OperationAdd(X, Y: Extended): Extended;
begin
  Result := X + Y;
end;

function TCnFloatMatrix.OperationDiv(X, Y: Extended): Extended;
begin
  Result := X / Y;
end;

function TCnFloatMatrix.OperationMul(X, Y: Extended): Extended;
begin
  Result := X * Y;
end;

function TCnFloatMatrix.OperationSub(X, Y: Extended): Extended;
begin
  Result := X - Y;
end;

procedure TCnFloatMatrix.SetColCount(const Value: Integer);
begin
  if FColCount <> Value then
  begin
    CheckCount(Value);
    FColCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnFloatMatrix.SetE(Size: Integer);
var
  I, J: Integer;
begin
  CheckCount(Size);

  RowCount := Size;
  ColCount := Size;
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        FMatrix[I, J] := 1
      else
        FMatrix[I, J] := 0;
end;

procedure TCnFloatMatrix.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
  begin
    CheckCount(Value);
    FRowCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnFloatMatrix.SetValue(Row, Col: Integer; const AValue: Extended);
begin
  FMatrix[Row, Col] := AValue;
end;

procedure TCnFloatMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := 0;
end;

procedure TCnFloatMatrix.SetZigZagValue(Index: Integer;
  const Value: Extended);
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  SetValue(R, C, Value);
end;

function TCnFloatMatrix.Trace: Extended;
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorTraceSquare);

  Result := 0;
  for I := 0 to FRowCount - 1 do
    Result := OperationAdd(Result, FMatrix[I, I]);
end;

procedure TCnFloatMatrix.Transpose;
begin
  CnMatrixTranspose(Self, Self);
end;

{ TCnRationalNumber }

procedure TCnRationalNumber.Add(Value: TCnRationalNumber);
begin
  CnRationalNumberAdd(Self, Value, Self);
end;

procedure TCnRationalNumber.Add(Value: Int64);
begin
  FNominator := FNominator + Value * FDenominator;
end;

procedure TCnRationalNumber.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnRationalNumber then
  begin
    TCnRationalNumber(Dest).Nominator := FNominator;
    TCnRationalNumber(Dest).Denominator := FDenominator;
  end
  else
    inherited;
end;

constructor TCnRationalNumber.Create;
begin
  FDenominator := 1;
end;

destructor TCnRationalNumber.Destroy;
begin
  inherited;

end;

procedure TCnRationalNumber.Divide(Value: TCnRationalNumber);
begin
  CnRationalNumberDiv(Self, Value, Self);
end;

procedure TCnRationalNumber.Divide(Value: Int64);
begin
  Denominator := FDenominator * Value;
  Reduce;
end;

function TCnRationalNumber.Equal(Value: TCnRationalNumber): Boolean;
begin
  Result := FNominator * Value.Denominator = FDenominator * Value.Nominator;
end;

function TCnRationalNumber.EqualInt(Value: Int64): Boolean;
begin
  Result := FNominator = FDenominator * Value;
end;

function TCnRationalNumber.IsInt: Boolean;
begin
  Result := (FDenominator = 1) or (FDenominator = -1);
end;

function TCnRationalNumber.IsNegative: Boolean;
begin
  Result := ((FNominator < 0) and (FDenominator > 0))
    or ((FNominator > 0) and (FDenominator < 0))
end;

function TCnRationalNumber.IsOne: Boolean;
begin
  Result := FNominator = FDenominator;
end;

function TCnRationalNumber.IsZero: Boolean;
begin
  Result := FNominator = 0;
end;

procedure TCnRationalNumber.Mul(Value: TCnRationalNumber);
begin
  CnRationalNumberMul(Self, Value, Self);
end;

procedure TCnRationalNumber.Mul(Value: Int64);
begin
  FNominator := FNominator * Value;
  Reduce;
end;

procedure TCnRationalNumber.Neg;
begin
  FNominator := - FNominator;
end;

procedure TCnRationalNumber.Reciprocal;
var
  T: Int64;
begin
  T := FDenominator;
  FDenominator := FNominator;
  FNominator := T;
end;

procedure TCnRationalNumber.Reduce;
begin
  if (FDenominator < 0) and (FNominator < 0) then
  begin
    FDenominator := -FDenominator;
    FNominator := -FNominator;
  end;

  if FNominator = 0 then
  begin
    FDenominator := 1;
    Exit;
  end;

  if not IsInt then
    CnReduceInt64(FNominator, FDenominator);
end;

procedure TCnRationalNumber.SetDenominator(const Value: Int64);
begin
  if Value = 0 then
    raise EDivByZero.Create('Denominator can NOT be Zero.');

  FDenominator := Value;
end;

procedure TCnRationalNumber.SetIntValue(Value: Int64);
begin
  FDenominator := 1;
  FNominator := Value;
end;

procedure TCnRationalNumber.SetOne;
begin
  FDenominator := 1;
  FNominator := 1;
end;

procedure TCnRationalNumber.SetString(const Value: string);
var
  P: Integer;
  N, D: string;
begin
  P := Pos('/', Value);
  if P > 1 then
  begin
    N := Copy(Value, 1, P - 1);
    D := Copy(Value, P + 1, MaxInt);
    FNominator := StrToInt64(N);
    FDenominator := StrToInt64(D);
  end
  else
  begin
    FNominator := StrToInt64(Value);
    FDenominator := 1;
  end;
end;

procedure TCnRationalNumber.SetValue(ANominator, ADenominator: Int64);
begin
  Denominator := ADenominator;
  Nominator := ANominator;
end;

procedure TCnRationalNumber.SetZero;
begin
  FDenominator := 1;
  FNominator := 0;
end;

procedure TCnRationalNumber.Sub(Value: TCnRationalNumber);
begin
  CnRationalNumberSub(Self, Value, Self);
end;

procedure TCnRationalNumber.Sub(Value: Int64);
begin
  FNominator := FNominator - Value * FDenominator;
end;

function TCnRationalNumber.ToString: string;
begin
  if IsInt or (FNominator = 0) then
    Result := IntToStr(FNominator)
  else
    Result := IntToStr(FNominator) + '/' + IntToStr(FDenominator);
end;

// ������ Int64 �����Լ����Ҫ�󶼴��� 0
function Int64Gcd(A, B: Int64): Int64;
begin
  if B = 0 then
    Result := A
  else
    Result := Int64Gcd(B, A mod B);
end;

// ������ Int64 ����С��������Ҫ�󶼴��� 0���ݲ����ǿ�����������
function Int64Lcm(A, B: Int64): Int64;
var
  D: Int64;
begin
  if A = B then
  begin
    Result := A;
    Exit;
  end;

  D := Int64Gcd(A, B);
  if D = 1 then
    Result := A * B
  else
  begin
    // �����ȳ����������
    if A > B then
      Result := A div D * B
    else
      Result := B div D * A;
  end;
end;

procedure CnRationalNumberAdd(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, F1, F2, D1, D2: Int64;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    RationalResult.Nominator := Number1.Nominator + Number2.Nominator;
  end
  else
  begin
    // ���ĸ����С������
    D1 := Number1.Denominator;
    D2 := Number2.Denominator;

    B1 := D1 < 0;
    B2 := D2 < 0;
    if B1 then
      D1 := -D1;

    if B2 then
      D2 := -D2;

    M := Int64Lcm(D1, D2);
    F1 := M div D1;
    F2 := M div D2;

    RationalResult.Denominator := M;
    RationalResult.Nominator := Number1.Nominator * F1 * SIGN_ARRAY[B1]
      + Number2.Nominator * F2 * SIGN_ARRAY[B2]; // ������������ް취
    RationalResult.Reduce;
  end;
end;

procedure CnRationalNumberAdd3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
begin
  CnRationalNumberAdd(Number1, Number2, RationalResult);
  CnRationalNumberAdd(RationalResult, Number3, RationalResult);
end;

procedure CnRationalNumberSub(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, F1, F2, D1, D2: Int64;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    RationalResult.Nominator := Number1.Nominator - Number2.Nominator;
  end
  else
  begin
    // ���ĸ����С������
    D1 := Number1.Denominator;
    D2 := Number2.Denominator;

    B1 := D1 < 0;
    B2 := D2 < 0;
    if B1 then
      D1 := -D1;

    if B2 then
      D2 := -D2;

    M := Int64Lcm(D1, D2);
    F1 := M div D1;
    F2 := M div D2;

    RationalResult.Denominator := M;
    RationalResult.Nominator := Number1.Nominator * F1 * SIGN_ARRAY[B1]
      - Number2.Nominator * F2 * SIGN_ARRAY[B2]; // ������������ް취
    RationalResult.Reduce;
  end;
end;

procedure CnRationalNumberMul(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
var
  X, Y: Int64;
begin
  // ���� Number1��Number2 �����Ѿ�Լ���ˣ�ֱ�ӳ�����������Ȼ���Լ
  X := Number1.Nominator;
  Y := Number2.Denominator;
  CnReduceInt64(X, Y);
  if X < Number1.Nominator then
  begin
    // ��Լ����
    RationalResult.Nominator := X * Number2.Nominator;
    RationalResult.Denominator := Number1.Denominator * Y;
  end
  else
  begin
    X := Number1.Denominator;
    Y := Number2.Nominator;
    CnReduceInt64(X, Y);
    if X < Number1.Denominator then
    begin
      // �е�Լ��
      RationalResult.Nominator := Number1.Nominator * Y;
      RationalResult.Denominator := X * Number2.Denominator;
    end
    else
    begin
      RationalResult.Nominator := Number1.Nominator * Number2.Nominator;
      RationalResult.Denominator := Number1.Denominator * Number2.Denominator;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnRationalNumberMul3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
begin
  CnRationalNumberMul(Number1, Number2, RationalResult);
  CnRationalNumberMul(RationalResult, Number3, RationalResult);
end;

procedure CnRationalNumberDiv(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
var
  X, Y: Int64;
begin
  // ���� Number1��Number2 �����Ѿ�Լ���ˣ�ֱ�ӳ�����������Ȼ���Լ
  X := Number1.Nominator;
  Y := Number2.Nominator;
  CnReduceInt64(X, Y);
  if X < Number1.Nominator then
  begin
    RationalResult.Nominator := X * Number2.Denominator;
    RationalResult.Denominator := Number1.Denominator * Y;
  end
  else
  begin
    X := Number1.Denominator;
    Y := Number2.Denominator;
    CnReduceInt64(X, Y);
    if X < Number1.Denominator then
    begin
      RationalResult.Nominator := Number1.Nominator * Y;
      RationalResult.Denominator := X * Number2.Nominator;
    end
    else
    begin
      RationalResult.Nominator := Number1.Nominator * Number2.Denominator;
      RationalResult.Denominator := Number1.Denominator * Number2.Nominator;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnReduceInt64(var X, Y: Int64);
var
  D: Int64;
begin
  D := Int64Gcd(X, Y);
  if D > 1 then
  begin
    X := X div D;
    Y := Y div D;
  end;
end;

function CnRationalNumberCompare(Number1, Number2: TCnRationalNumber): Integer;
var
  R: Int64;
begin
  if not Number1.IsNegative and Number2.IsNegative then
    Result := 1
  else if Number1.IsNegative and not Number2.IsNegative then
    Result := -1
  else  // ͬ���Ų���Ҫ����
  begin
    R := Number1.Nominator * Number2.Denominator - Number2.Nominator * Number1.Denominator;
    if R > 0 then
      Result := 1
    else if R < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;

{ TCn2DObjectList }

constructor TCn2DObjectList.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRows := TObjectList.Create(True);
  RowCount := ARowCount;
  ColCount := AColCount;
end;

procedure TCn2DObjectList.DeleteCol(Col: Integer);
var
  I: Integer;
begin
  for I := 0 to FRowCount - 1 do
    TObjectList(FRows[I]).Delete(Col);
  Dec(FColCount);
end;

procedure TCn2DObjectList.DeleteRow(Row: Integer);
begin
  FRows.Delete(Row);
  Dec(FRowCount);
end;

destructor TCn2DObjectList.Destroy;
begin
  FRows.Free;
  inherited;
end;

function TCn2DObjectList.GetColCount: Integer;
begin
  Result := FColCount;
end;

function TCn2DObjectList.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TCn2DObjectList.GetValueObject(Row, Col: Integer): TObject;
begin
  Result := TObjectList(FRows[Row])[Col];
end;

procedure TCn2DObjectList.SetColCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FColCount then
  begin
    CheckCount(Value);
    FColCount := Value;

    for I := 0 to FRows.Count - 1 do
    begin
      if FRows[I] = nil then
        FRows[I] := TObjectList.Create(True);

      TObjectList(FRows[I]).Count := FColCount;
    end;
  end;
end;

procedure TCn2DObjectList.SetRowCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FRowCount then
  begin
    CheckCount(Value);
    FRowCount := Value;
    FRows.Count := Value;

    for I := 0 to FRows.Count - 1 do
    begin
      if FRows[I] = nil then
      begin
        FRows[I] := TObjectList.Create(True);
        TObjectList(FRows[I]).Count := FColCount;
      end;
    end;
  end;
end;

procedure TCn2DObjectList.SetValueObject(Row, Col: Integer; const Value: TObject);
begin
  TObjectList(FRows[Row])[Col] := Value;
end;

{ TCnRationalMatrix }

procedure TCnRationalMatrix.Add(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Add(Factor);
end;

procedure TCnRationalMatrix.Add(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Add(Factor);
end;

procedure TCnRationalMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnRationalMatrix then
  begin
    TCnRationalMatrix(Dest).RowCount := RowCount;
    TCnRationalMatrix(Dest).ColCount := ColCount;

    for I := 0 to RowCount - 1 do
      for J := 0 to ColCount - 1 do
        TCnRationalMatrix(Dest).Value[I, J] := TCnRationalNumber(FMatrix[I, J]);
  end
  else
    inherited;
end;

constructor TCnRationalMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FMatrix := TCn2DObjectList.Create(ARowCount, AColCount);
end;

procedure TCnRationalMatrix.DeleteCol(Col: Integer);
begin
  FMatrix.DeleteCol(Col);
end;

procedure TCnRationalMatrix.DeleteRow(Row: Integer);
begin
  FMatrix.DeleteRow(Row);
end;

destructor TCnRationalMatrix.Destroy;
begin
  FMatrix.Free;
  inherited;
end;

procedure TCnRationalMatrix.Determinant(D: TCnRationalNumber);
var
  I: Integer;
  Minor: TCnRationalMatrix;
  T: TCnRationalNumber;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorDeterminantSquare);

  if RowCount = 1 then
    D.Assign(Value[0, 0])
  else if RowCount = 2 then
  begin
    T := TCnRationalNumber.Create;
    try
      CnRationalNumberMul(Value[0, 0], Value[1, 1], D);
      CnRationalNumberMul(Value[0, 1], Value[1, 0], T);
      CnRationalNumberSub(D, T, D);
    finally
      T.Free;
    end;
    // [0, 0] * [1, 1] - [0, 1] * [1, 0]
  end
  else if RowCount = 3 then
  begin
    T := TCnRationalNumber.Create;
    D.SetZero;
    try
      CnRationalNumberMul3(Value[0, 0], Value[1, 1], Value[2, 2], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 1], Value[1, 2], Value[2, 0], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 2], Value[1, 0], Value[2, 1], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 0], Value[1, 2], Value[2, 1], T);
      CnRationalNumberSub(D, T, D);
      CnRationalNumberMul3(Value[0, 1], Value[1, 0], Value[2, 2], T);
      CnRationalNumberSub(D, T, D);
      CnRationalNumberMul3(Value[0, 2], Value[1, 1], Value[2, 0], T);
      CnRationalNumberSub(D, T, D);
    finally
      T.Free;
    end
//    Result := Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2])
//      + Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0])
//      + Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])
//      - Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1])
//      - Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2])
//      - Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0]);
  end
  else
  begin
    // ���ô�������ʽ Minor/Cofactor ����߽�����ʽ
    D.SetZero;
    Minor := TCnRationalMatrix.Create(RowCount - 1, ColCount - 1);
    T := TCnRationalNumber.Create;
    try
      for I := 0 to ColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);

        Minor.Determinant(T);
        T.Mul(InternalNegativeOnePower(I));
        T.Mul(Value[0, I]);
        D.Add(T);
        // Result := Result + (FMatrix[0, I] * NegativeOnePower(I)* Minor.Determinant));
      end;
    finally
      Minor.Free;
      T.Free;
    end;
  end;
end;

procedure TCnRationalMatrix.Divide(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Divide(Factor);
end;

procedure TCnRationalMatrix.Divide(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Divide(Factor);
end;

procedure TCnRationalMatrix.DumpToStrings(List: TStrings; Sep: Char);
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  for I := 0 to RowCount - 1 do
  begin
    S := '';
    for J := 0 to ColCount - 1 do
    begin
      if J = 0 then
        S := Value[I, J].ToString
      else
        S := S + Sep + Value[I, J].ToString;
    end;
    List.Add(S);
  end;
end;

function TCnRationalMatrix.GetColCount: Integer;
begin
  Result := FMatrix.ColCount;
end;

function TCnRationalMatrix.GetRowCount: Integer;
begin
  Result := FMatrix.RowCount;
end;

function TCnRationalMatrix.GetValue(Row, Col: Integer): TCnRationalNumber;
begin
  Result := TCnRationalNumber(FMatrix[Row, Col]);
  if Result = nil then
  begin
    Result := TCnRationalNumber.Create;
    FMatrix[Row, Col] := Result;
  end;
end;

function TCnRationalMatrix.GetZigZagValue(Index: Integer): TCnRationalNumber;
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  Result := GetValue(R, C);
end;

function TCnRationalMatrix.IsE: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to RowCount - 1 do
  begin
    for J := 0 to ColCount - 1 do
    begin
      if (I = J) and not Value[I, J].IsOne then
      begin
        Result := False;
        Exit;
      end
      else if (I <> J) and not Value[I, J].IsZero then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCnRationalMatrix.IsSingular: Boolean;
var
  D: TCnRationalNumber;
begin
  if not IsSquare then
    Result := False
  else
  begin
    D := TCnRationalNumber.Create;
    try
      Determinant(D);
      Result := D.IsZero;
    finally
      D.Free;
    end;
  end;
end;

function TCnRationalMatrix.IsSquare: Boolean;
begin
  Result := (ColCount = RowCount);
end;

function TCnRationalMatrix.IsSymmetrical: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to RowCount - 1 do
    for J := 0 to I do
      if not Value[I, J].Equal(Value[J, I]) then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

function TCnRationalMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if not IsSquare then
    Exit;

  for I := 0 to RowCount - 1 do
  begin
    for J := 0 to ColCount - 1 do
    begin
      if not Value[I, J].IsZero then
        Exit;
    end;
  end;

  Result := True;
end;

procedure TCnRationalMatrix.Mul(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Mul(Factor);
end;

procedure TCnRationalMatrix.Mul(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Mul(Factor);
end;

procedure TCnRationalMatrix.SetColCount(const Value: Integer);
begin
  FMatrix.ColCount := Value;
end;

procedure TCnRationalMatrix.SetE(Size: Integer);
var
  I, J: Integer;
begin
  CheckCount(Size);

  RowCount := Size;
  ColCount := Size;
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        Value[I, J].SetOne
      else
        Value[I, J].SetZero;
end;

procedure TCnRationalMatrix.SetRowCount(const Value: Integer);
begin
  FMatrix.RowCount := Value;
end;

procedure TCnRationalMatrix.SetValue(Row, Col: Integer;
  const Value: TCnRationalNumber);
begin
  if FMatrix[Row, Col] = nil then
    FMatrix[Row, Col] := TCnRationalNumber.Create;
  TCnRationalNumber(FMatrix[Row, Col]).Assign(Value);
end;

procedure TCnRationalMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].SetZero;
end;

procedure TCnRationalMatrix.SetZigZagValue(Index: Integer; const Value: TCnRationalNumber);
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  SetValue(R, C, Value);
end;

procedure TCnRationalMatrix.Trace(T: TCnRationalNumber);
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorTraceSquare);

  T.SetZero;
  for I := 0 to RowCount - 1 do
    T.Add(Value[I, I]);
end;

procedure TCnRationalMatrix.Transpose;
begin
  CnMatrixTranspose(Self, Self);
end;

end.

