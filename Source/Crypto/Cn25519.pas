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

unit Cn25519;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�25519 ϵ����Բ�����㷨��Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע������ԪĿǰʵ���� RFC 7748 �е� 25519 �� 448 �����ض���Բ���ߵĵ�Ӽ��������㷨�� RFC 8032 �е�ǩ����ǩ�㷨��
*
*           25519 �� 448 ��Բ�����漰�ɸ�������Montgomery������ y^2 = x^3 + A*X^2 + x 
*           �Լ�Ť��/��Ť�����»���Edwards������ au^2 + v^2 = 1 + d * u^2 * v^2 ������ʽ������ a Ϊ 1 ʱ�Ƿ�Ť����ʽ��
*           ǰ�������� Curve ����������Ϊ x y������Ϊ A������������ Ed ����������Ϊ u v������Ϊ a d��
*
*           ����Ԫ�� 25519 ���ߵ��㷨����ʵ�ֽ����� X �Լ��ɸ��������ݵĿ��ٱ����ˡ���չ��Ԫ����Ŀ��ٵ�ӡ�
*           �Լ���϶���ʽԼ������ģ���������еļ����㷨����ԭʼ����㷨�ٶȵ���ʮ�����ϡ�
*
*           ע�⣺Ed25519/Ed448 �Ĺ�Կ�����紫ͳ ECC ��������˽Կֱ�ӵ�� G ����������Ǿ�����
*               ��������ŵõ��������ٵ�� G ��õ���Կ���ҿ��Բ������洢 X Y��ֻ�� Y
*               �ҽ� X ����ż���뼴�ɡ�
*
*           RFC 8032 �� Ed25519/Ed448 ��ǩ���淶�У������������ 32/57 �ֽ�ֵ�� SecretKey�������������
*               �ٵ�˵õ���Կ�ٴ� Y �� X ��ż�Ե� 32/57 �ֽڽ� PublicKey��������һ�� 64/114 �ֽ���Ϊ
*               һ�Թ�˽Կ��
*
*           ����Ԫ�еĹ�˽Կ�ȿɰ������ ECC ��˽Կ����Ҳ���� LoadFromData/SaveToData ������ 32/57 �ֽ�����ʵ�ּ��ش洢��
*
*           ���� Curve25519/448 ����Բ���ߵ�/��Կ�洢�� 32/56 �ֽڵ�������ʱ����Ǵ� X ֵ
*               ���� Ed25519/448 ����Բ���ߵ�/��Կ�洢�� 32/57 �ֽ��е����ݲ�ͬ�������Ǵ� Y �� X ����ż�ԡ�
*
*           ���� Ed25519 ������Ť�����»����������������Ԫ��չ��������٣�������Ϊ 2^255-19 ��������� 64 λ���������ʽ����
*               �� Ed448 �����Ƿ�Ť�����»����ߵ��²����� Ed25519 һ�����٣�ֻ������Ԫ��Ӱ����㷨���٣��ٶ�ƫ����
*
* ����ƽ̨��Win7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.08.12 V1.6
*               ���� 448 �ɸ������������Ť�����»����ߵ�����ʵ���Լ� Ed448 ��ǩ����ǩ����
*           2022.07.30 V1.5
*               ȥ���������õ��ж��Ծ������
*           2022.06.14 V1.4
*               ʵ�� Ed25519 ���ļ���ǩ������֤
*           2022.06.12 V1.3
*               ʵ�� Field64 ����ʽ���������������㷨��
*               �����ڴ˸����ɸ��������ݼ��ٱ���������չ��Ԫ����Ŀ��ٵ��������ˣ�
*               �ٶ��ٴ����һ�����ϣ���� 64 λ�£����ܶ����ٴ����һ��
*           2022.06.09 V1.2
*               ʵ�� Curve25519 ���ߵ��ɸ��������ݼ��ٱ����ˣ��ٶȽ�ԭʼ�˷���ʮ������
*           2022.06.08 V1.1
*               ʵ�� Ed25519 ǩ������֤
*           2022.06.07 V1.1
*               ʵ�� Ed25519 ��չ��Ԫ����Ŀ��ٵ��������ˣ��ٶȽ�ԭʼ�����˷���ʮ������
*           2022.06.05 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative, CnBigNumber, CnInt128, CnECC, CnSHA2, CnSHA3;

const
  CN_25519_BLOCK_BYTESIZE = 32;
  {* 25519 ��������㷨�����ݿ��С����λ���ֽ�}

  CN_448_CURVE_BLOCK_BYTESIZE = 56;
  {* �ɸ����� 448 ��������㷨�����ݿ��С����λ���ֽڣ�������ԿЭ�̵�}

  CN_448_EDWARDS_BLOCK_BYTESIZE = 57;
  {* Ť�����»� 448 ��������㷨�����ݿ��С����λ���ֽڣ�����ǩ����֤��}

type
  TCn25519Field64 = array[0..4] of TUInt64;
  {* �ö���ʽ�����ʾһ�� 2^255-19 ��Χ�ڵ�������Ԫ�أ�f0 + (2^51)*f1 + (2^102)*f2 + (2^153)*f3 + (2^204)*f4}

  TCn25519Field64EccPoint = packed record
  {* �ö���ʽ�����ʾ�� 25519 ��Բ�����ϵĵ㣨������ X ��Ӱ�㣬Z �� Y ���棩
     �������ټ��㣬���������������Բ����}
    X: TCn25519Field64;
    Y: TCn25519Field64;
  end;

  TCn25519Field64Ecc4Point = packed record
  {* �ö���ʽ�����ʾ�� 25519 ��Բ�����ϵ���Ԫ��չ��
     �������ټ��㣬���������������Բ����}
    X: TCn25519Field64;
    Y: TCn25519Field64;
    Z: TCn25519Field64;
    T: TCn25519Field64;
  end;

  TCnEcc4Point = class(TCnEcc3Point)
  {* ��չ����Ӱ/����/�ſɱ�����㣬������ T ���ڼ�¼�м���
     ������ x = X/Z  y = Y/Z  x*y = T/Z�����Ե��� ��0, 1, 1, 0��}
  private
    FT: TCnBigNumber;
    procedure SetT(const Value: TCnBigNumber);
  public
    constructor Create; override;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Assign(Source: TPersistent); override;
    {* ����������ֵ������

       ������
         Source: TPersistent                  - ����֮��ֵ��Դ����

       ����ֵ�����ޣ�
    }

    function ToString: string; override; // ������ ToString
    {* ת��Ϊ�ַ�����

       ������
        ���ޣ�

       ����ֵ��string                         - ת���������ַ���ֵ
    }

    property T: TCnBigNumber read FT write SetT;
    {* �м��� T}
  end;

  TCnTwistedEdwardsCurve = class
  {* �������ϵ�Ť�����»����� au^2 + v^2 = 1 + du^2v^2 ������ u v ���ɸ��������ߵ� x y ��ӳ���ϵ��}
  private
    FCoefficientA: TCnBigNumber;
    FCoefficientD: TCnBigNumber;
    FOrder: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FGenerator: TCnEccPoint;
    FCoFactor: Integer;

    function CalcXFromY(InY: TCnBigNumber; OutX: TCnBigNumber; XOdd: Boolean): Boolean;
    {* �� Y ֵ�Է��� x^2 = (Y^2 - 1) / (D*Y^2 - A) mod P ��⣬���� Y �� X �Ƿ���ż�ı�ǣ���������Ƿ�ɹ�}
  public
    constructor Create; overload; virtual;
    {* ��ͨ���캯����δ��ʼ������}
    constructor Create(const A: AnsiString; const D: AnsiString; const FieldPrime: AnsiString;
      const GX: AnsiString; const GY: AnsiString; const Order: AnsiString; H: Integer = 1); overload;
    {* ���캯�������뷽�̵� A, D �������������Ͻ� p��G �����ꡢG ��Ľ������������ӣ���Ҫʮ�������ַ�����

       ������
         const A: AnsiString                  - Ť�����»����߷��̵� a ������ʮ�������ַ�����ʽ
         const D: AnsiString                  - Ť�����»����߷��̵� d ������ʮ�������ַ�����ʽ
         const FieldPrime: AnsiString         - Ť�����»����߷������ڵ��������Ͻ��ʮ�������ַ�����ʽ
         const GX: AnsiString                 - Ť�����»����߷��̵� G ��� X �����ʮ�������ַ�����ʽ
         const GY: AnsiString                 - Ť�����»����߷��̵� G ��� Y �����ʮ�������ַ�����ʽ
         const Order: AnsiString              - Ť�����»����߷��̵� G ��Ľ׵�ʮ�������ַ�����ʽ
         H: Integer                           - Ť�����»����߷��̵ĸ�������

       ����ֵ��TCnTwistedEdwardsCurve         - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    procedure Load(const A: AnsiString; const D: AnsiString; const FieldPrime: AnsiString;
      const GX: AnsiString; const GY: AnsiString; const Order: AnsiString; H: Integer = 1); virtual;
    {* �������߲���������������ͬ Create ������ע���ַ�������Ҳ��ʮ�����Ƹ�ʽ��

       ������
         const A: AnsiString                  - Ť�����»����߷��̵� a ������ʮ�������ַ�����ʽ
         const D: AnsiString                  - Ť�����»����߷��̵� d ������ʮ�������ַ�����ʽ
         const FieldPrime: AnsiString         - Ť�����»����߷������ڵ��������Ͻ��ʮ�������ַ�����ʽ
         const GX: AnsiString                 - Ť�����»����߷��̵� G ��� X �����ʮ�������ַ�����ʽ
         const GY: AnsiString                 - Ť�����»����߷��̵� G ��� Y �����ʮ�������ַ�����ʽ
         const Order: AnsiString              - Ť�����»����߷��̵� G ��Ľ׵�ʮ�������ַ�����ʽ
         H: Integer                           - Ť�����»����߷��̵ĸ�������

       ����ֵ�����ޣ�
    }

    procedure MultiplePoint(K: Int64; P: TCnEccPoint); overload;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㡣

       ������
         K: Int64                             - ����
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    procedure MultiplePoint(K: TCnBigNumber; P: TCnEccPoint); overload; virtual;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㣬�ڲ�ʵ�ֵ�ͬ�� CnECC ��ͬ��������

       ������
         K: TCnBigNumber                      - ��������ʽΪ����
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    procedure PointAddPoint(P: TCnEccPoint; Q: TCnEccPoint; Sum: TCnEccPoint);
    {* ���� P + Q��ֵ���� Sum �У�Sum ������ P��Q ֮һ��P��Q ������ͬ��
       �˴��ļӷ��ļ��������൱�ڵ�λԲ�ϵ����� Y ��ļнǽǶ���ӷ���
       ���Ե�(0, 1)����ͬ�� Weierstrass �����е�����Զ�㡣

       ������
         P: TCnEccPoint                       - ��һ�������������
         Q: TCnEccPoint                       - �ڶ��������������
         Sum: TCnEccPoint                     - ����ĺ͵������

       ����ֵ�����ޣ�
    }

    procedure PointSubPoint(P: TCnEccPoint; Q: TCnEccPoint; Diff: TCnEccPoint);
    {* ���� P - Q��ֵ���� Diff �У�Diff ������ P��Q ֮һ��P��Q ������ͬ��

       ������
         P: TCnEccPoint                       - �������������
         Q: TCnEccPoint                       - �����������
         Diff: TCnEccPoint                    - ����Ĳ�������

       ����ֵ�����ޣ�
    }

    procedure PointInverse(P: TCnEccPoint);
    {* ���� P �����Ԫ -P��ֵ���·��� P��Ҳ���� X ֵȡ����

       ������
         P: TCnEccPoint                       - ��ȡ��Ԫ�������

       ����ֵ�����ޣ�
    }

    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* �ж� P ���Ƿ��ڱ������ϡ�

       ������
         P: TCnEccPoint                       - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ��ڱ�������
    }

    function IsNeutualPoint(P: TCnEccPoint): Boolean;
    {* �жϵ��Ƿ������Ե㣬Ҳ�����ж� X = 0 �� Y = 1���� Weierstrass ������Զ��ȫ 0 ��ͬ��

       ������
         P: TCnEccPoint                       - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ������Ե�
    }

    procedure SetNeutualPoint(P: TCnEccPoint);
    {* ������Ϊ���Ե㣬Ҳ���� X := 0 �� Y := 1��

       ������
         P: TCnEccPoint                       - �������õ������

       ����ֵ�����ޣ�
    }

    property Generator: TCnEccPoint read FGenerator;
    {* �������� G}
    property CoefficientA: TCnBigNumber read FCoefficientA;
    {* ����ϵ�� A}
    property CoefficientD: TCnBigNumber read FCoefficientD;
    {* ����ϵ�� B}
    property FiniteFieldSize: TCnBigNumber read FFiniteFieldSize;
    {* ��������Ͻ磬���� p}
    property Order: TCnBigNumber read FOrder;
    {* ����Ľ��� N��ע����ֻ�� H Ϊ 1 ʱ�ŵ��ڱ����ߵ��ܵ���}
    property CoFactor: Integer read FCoFactor;
    {* �������� H��Ҳ�����ܵ��� = N * H������ Integer ��ʾ}
  end;

  TCnMontgomeryCurve = class
  {* �������ϵ��ɸ��������� By^2 = x^3 + Ax^2 + x������ B*(A^2 - 4) <> 0}
  private
    FCoefficientB: TCnBigNumber;
    FCoefficientA: TCnBigNumber;
    FOrder: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FGenerator: TCnEccPoint;
    FCoFactor: Integer;
    FLadderConst: TCnBigNumber;
    FLadderField64: TCn25519Field64;
    procedure CheckLadderConst;
  public
    constructor Create; overload; virtual;
    {* ��ͨ���캯����δ��ʼ������}
    constructor Create(const A: AnsiString; const B: AnsiString; const FieldPrime: AnsiString;
      const GX: AnsiString; const GY: AnsiString; const Order: AnsiString; H: Integer = 1); overload;
    {* ���캯�������뷽�̵� A, B �������������Ͻ� p��G �����ꡢG ��Ľ������������ӣ���Ҫʮ�������ַ�����

       ������
         const A: AnsiString                  - �ɸ��������߷��̵� a ������ʮ�������ַ�����ʽ
         const B: AnsiString                  - �ɸ��������߷��̵� b ������ʮ�������ַ�����ʽ
         const FieldPrime: AnsiString         - �ɸ��������߷������ڵ��������Ͻ��ʮ�������ַ�����ʽ
         const GX: AnsiString                 - �ɸ��������߷��̵� G ��� X �����ʮ�������ַ�����ʽ
         const GY: AnsiString                 - �ɸ��������߷��̵� G ��� Y �����ʮ�������ַ�����ʽ
         const Order: AnsiString              - �ɸ��������߷��̵� G ��Ľ׵�ʮ�������ַ�����ʽ
         H: Integer                           - �ɸ��������߷��̵ĸ�������

       ����ֵ��TCnMontgomeryCurve             - ���ش����Ķ���ʵ��
    }

    destructor Destroy; override;
    {* ��������}

    procedure Load(const A: AnsiString; const B: AnsiString; const FieldPrime: AnsiString;
      const GX: AnsiString; const GY: AnsiString; const Order: AnsiString; H: Integer = 1); virtual;
    {* �������߲���������������ͬ Create ������ע���ַ�������Ҳ��ʮ�����Ƹ�ʽ��

       ������
         const A: AnsiString                  - �ɸ��������߷��̵� a ������ʮ�������ַ�����ʽ
         const B: AnsiString                  - �ɸ��������߷��̵� b ������ʮ�������ַ�����ʽ
         const FieldPrime: AnsiString         - �ɸ��������߷������ڵ��������Ͻ��ʮ�������ַ�����ʽ
         const GX: AnsiString                 - �ɸ��������߷��̵� G ��� X �����ʮ�������ַ�����ʽ
         const GY: AnsiString                 - �ɸ��������߷��̵� G ��� Y �����ʮ�������ַ�����ʽ
         const Order: AnsiString              - �ɸ��������߷��̵� G ��Ľ׵�ʮ�������ַ�����ʽ
         H: Integer                           - �ɸ��������߷��̵ĸ�������

       ����ֵ�����ޣ�
    }

    procedure MultiplePoint(K: Int64; P: TCnEccPoint); overload;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㡣

       ������
         K: Int64                             - ����
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    procedure MultiplePoint(K: TCnBigNumber; P: TCnEccPoint); overload; virtual;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㣬�ڲ�ʵ�ֵ�ͬ�� CnECC ��ͬ��������

       ������
         K: TCnBigNumber                      - ��������ʽΪ����
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    procedure PointAddPoint(P: TCnEccPoint; Q: TCnEccPoint; Sum: TCnEccPoint);
    {* ���� P + Q��ֵ���� Sum �У�Sum ������ P��Q ֮һ��P��Q ������ͬ
       �˴��ļӷ��ļ������������� Weierstrass ��Բ�����ϵ����߻����߽�����ȡ����ͬ����������Զ��(0, 0)��

       ������
         P: TCnEccPoint                       - ��һ�������������
         Q: TCnEccPoint                       - �ڶ��������������
         Sum: TCnEccPoint                     - ����ĺ͵������

       ����ֵ�����ޣ�
    }
    procedure PointSubPoint(P: TCnEccPoint; Q: TCnEccPoint; Diff: TCnEccPoint);
    {* ���� P - Q��ֵ���� Diff �У�Diff ������ P��Q ֮һ��P��Q ������ͬ��

       ������
         P: TCnEccPoint                       - �������������
         Q: TCnEccPoint                       - �����������
         Diff: TCnEccPoint                    - ����Ĳ�������

       ����ֵ�����ޣ�
    }

    procedure PointInverse(P: TCnEccPoint);
    {* ���� P �����Ԫ -P��ֵ���·��� P��Ҳ���� Y ֵȡ����

       ������
         P: TCnEccPoint                       - ��ȡ��Ԫ�������

       ����ֵ�����ޣ�
    }

    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* �ж� P ���Ƿ��ڱ������ϡ�

       ������
         P: TCnEccPoint                       - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ��ڱ�������
    }

    // ============ �ɸ����������㷨�еĽ� X ����Ӱ���������㷨 ==============

    procedure PointToXAffinePoint(DestPoint: TCnEccPoint; SourcePoint: TCnEccPoint);
    {* ������ X Y ����Բ���ߵ�ת��Ϊ��Ӱ���� X Y Z ��ֻ���� X Z ���ɸ����������㷨ʹ�ã�
       ��ʵ���� Y �� 1��SourcePoint �� DestPoint ������ͬ��

       ������
         DestPoint: TCnEccPoint               - ת�����Ŀ�������
         SourcePoint: TCnEccPoint             - ��ת����Դ�����

       ����ֵ�����ޣ�
    }

    procedure XAffinePointToPoint(DestPoint: TCnEccPoint; SourcePoint: TCnEccPoint);
    {* ��ֻ�� X Z���ڲ��� Y ���� Z������Ӱ�����ת��Ϊ��ͨ���ߵ㣬��ʵ������� Y ���滻 Z��
       SourcePoint �� DestPoint ������ͬ��

       ������
         DestPoint: TCnEccPoint               - ת�����Ŀ�������
         SourcePoint: TCnEccPoint             - ��ת����Դ�����

       ����ֵ�����ޣ�
    }

    procedure XAffinePointInverse(P: TCnEccPoint);
    {* ����� X ����Ӱ����� P �����Ԫ -P��ֵ���·��� P��Ҳ���� Y ֵȡ��
       ʵ���ڲ���Ϊû�� Y��ɶ����������

       ������
         P: TCnEccPoint                       - ��ȡ��Ԫ�������

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderPointXDouble(Dbl: TCnEccPoint; P: TCnEccPoint);
    {* �ɸ����������㷨�еĽ� X ����Ӱ�����Ķ��������㣬Y �ڲ��� Z �ã�Dbl ������ P��

       ������
         Dbl: TCnEccPoint                     - ����Ķ����������
         P: TCnEccPoint                       - ����ж���������������

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderPointXAdd(Sum: TCnEccPoint; P: TCnEccPoint;
      Q: TCnEccPoint; PMinusQ: TCnEccPoint);
    {* �ɸ����������㷨�еĽ� X ����Ӱ�����ĵ�����㣬Y �ڲ��� Z �ã�������Ҫ������ֵ�⻹��Ҫһ�����ֵ��

       ������
         Sum: TCnEccPoint                     - ����ĺ͵������
         P: TCnEccPoint                       - ��һ�������������
         Q: TCnEccPoint                       - �ڶ��������������
         PMinusQ: TCnEccPoint                 - ���������

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderMultiplePoint(K: Int64; P: TCnEccPoint); overload;
    {* ���ɸ����������㷨����� X ����Ӱ������ K ���㣬ֵ���·���õ㡣

       ������
         K: Int64                             - ����
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderMultiplePoint(K: TCnBigNumber; P: TCnEccPoint); overload;
    {* ���ɸ����������㷨����� X ����Ӱ������ K ���㣬ֵ���·���õ㡣

       ������
         K: TCnBigNumber                      - ��������ʽ�Ǵ���
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    property Generator: TCnEccPoint read FGenerator;
    {* �������� G}
    property CoefficientA: TCnBigNumber read FCoefficientA;
    {* ����ϵ�� A}
    property CoefficientB: TCnBigNumber read FCoefficientB;
    {* ����ϵ�� B}
    property FiniteFieldSize: TCnBigNumber read FFiniteFieldSize;
    {* ��������Ͻ磬���� p}
    property Order: TCnBigNumber read FOrder;
    {* ����Ľ��� N��ע����ֻ�� H Ϊ 1 ʱ�ŵ��ڱ����ߵ��ܵ���}
    property CoFactor: Integer read FCoFactor;
    {* �������� H��Ҳ�����ܵ��� = N * H������ Integer ��ʾ}
  end;

  TCnCurve25519Data = array[0..CN_25519_BLOCK_BYTESIZE - 1] of Byte;
  {* Curve25519 �ĳ������ݣ�RFC �涨��������С���ֽ�˳��}

  TCnCurve25519PrivateKey = class(TCnEccPrivateKey)
  {* Curve25519 ˽Կ��Ҳ�ǻ������}
  public
    procedure SaveToData(var Data: TCnCurve25519Data);
    {* ��˽Կ����ת���� 32 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnCurve25519Data          - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnCurve25519Data);
    {* �� 32 �ֽڵ�С���ֽ�˳�������м���˽Կ��

       ������
         Data: TCnCurve25519Data              - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnCurve25519PublicKey = class(TCnEccPublicKey)
  {* Curve25519 ��Կ���Ƕ�Ӧ˽Կ���Ի���õ�������}
  public
    procedure SaveToData(var Data: TCnCurve25519Data);
    {* ��Կ����ת���� 32 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnCurve25519Data          - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnCurve25519Data);
    {* �� 32 �ֽڵ�С���ֽ�˳�������м��ع�Կ��ע��ֻ���� Y ��û����� X��

       ������
         Data: TCnCurve25519Data              - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ��
       ͬ��ע��ֻ���� Y ��û����� X��

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnCurve25519 = class(TCnMontgomeryCurve)
  {* RFC 7748/8032 �й涨�� Curve25519 ����}
  public
    constructor Create; override;
    {* ���캯�����ڲ���ʼ���ɸ����� 25519 ���ߵĲ���}

    function GenerateKeys(PrivateKey: TCnCurve25519PrivateKey; PublicKey: TCnCurve25519PublicKey): Boolean;
    {* ����һ�� Curve25519 ��Բ���ߵĹ�˽Կ������˽Կ�ĸߵ�λ�����⴦��

       ������
         PrivateKey: TCnCurve25519PrivateKey  - ���ɵ� Curve25519 ��Բ���ߵ�˽Կ
         PublicKey: TCnCurve25519PublicKey    - ���ɵ� Curve25519 ��Բ���ߵĹ�Կ

       ����ֵ��Boolean                        - �����Ƿ�ɹ�
    }

    procedure MultiplePoint(K: TCnBigNumber; P: TCnEccPoint); override;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㣬P ������ֻ�� X ��Ϣ
       �ڲ�ʵ��ʹ�� 64 λ����ʽ������ɸ����������㷨��

       ������
         K: TCnBigNumber                      - ��������ʽ�Ǵ���
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    // ======= �ɸ����������㷨�еĽ� X ����Ӱ����� 2^51 ����ʽ�����㷨 =======

    procedure PointToField64XAffinePoint(var DestPoint: TCn25519Field64EccPoint; SourcePoint: TCnEccPoint);
    {* ������ X Y ����Բ���ߵ�ת��Ϊ��Ӱ���� X Y Z ��ֻ���� X Z ��ת��Ϊ����ʽ�㣬���ɸ����������㷨ʹ�á�

       ������
         var DestPoint: TCn25519Field64EccPoint  - Ŀ����Ӱ�����
         SourcePoint: TCnEccPoint                - Դ�����

       ����ֵ�����ޣ�
    }

    procedure Field64XAffinePointToPoint(DestPoint: TCnEccPoint; var SourcePoint: TCn25519Field64EccPoint);
    {* ������ʽ��ʽ��ֻ�� X Z(Y ���� Z) ����Ӱ�����ת��Ϊ��ͨ���ߵ㡣

       ������
         DestPoint: TCnEccPoint                   - Ŀ�������
         var SourcePoint: TCn25519Field64EccPoint - Դ��Ӱ�����

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderField64PointXDouble(var Dbl: TCn25519Field64EccPoint; var P: TCn25519Field64EccPoint);
    {* ����ʽ��ʽ���ɸ����������㷨�еĽ� X ����Ӱ�����Ķ��������㣬Y �ڲ��� Z �ã�Dbl ������ P��

       ������
         var Dbl: TCn25519Field64EccPoint     - ����Ķ����������
         var P: TCn25519Field64EccPoint       - ����ж���������������

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderField64PointXAdd(var Sum: TCn25519Field64EccPoint;
      var P: TCn25519Field64EccPoint; var Q: TCn25519Field64EccPoint;
      var PMinusQ: TCn25519Field64EccPoint);
    {* ����ʽ��ʽ���ɸ����������㷨�еĽ� X ����Ӱ�����ĵ�����㣬Y �ڲ��� Z �ã�������Ҫ������ֵ�⻹��Ҫһ�����ֵ��

       ������
         var Sum: TCn25519Field64EccPoint     - ����ĺ͵������
         var P: TCn25519Field64EccPoint       - ��һ�������������
         var Q: TCn25519Field64EccPoint       - �ڶ��������������
         var PMinusQ: TCn25519Field64EccPoint - ���������

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderField64MultiplePoint(K: Int64; var P: TCn25519Field64EccPoint); overload;
    {* �ö���ʽ��ʽ���ɸ����������㷨����� X ����Ӱ������ K ���㣬ֵ���·���õ㡣

       ������
         K: Int64                             - ����
         var P: TCn25519Field64EccPoint       - ���˵������

       ����ֵ�����ޣ�
    }

    procedure MontgomeryLadderField64MultiplePoint(K: TCnBigNumber; var P: TCn25519Field64EccPoint); overload;
    {* �ö���ʽ��ʽ���ɸ����������㷨����� X ����Ӱ������ K ���㣬ֵ���·���õ㡣

       ������
         K: TCnBigNumber                      - ��������ʽΪ����
         var P: TCn25519Field64EccPoint       - ���˵������

       ����ֵ�����ޣ�
    }
  end;

  TCnEd25519Data = array[0..CN_25519_BLOCK_BYTESIZE - 1] of Byte;
  {* Ed25519 �Ĺ�˽Կ���ݣ�RFC �涨��������С���ֽ�˳��}

  TCnEd25519SignatureData = array[0..2 * CN_25519_BLOCK_BYTESIZE - 1] of Byte;
  {* Ed25519 ��ǩ�����ݣ�����һ����С���ֽ�˳��}

  TCnEd25519PrivateKey = class(TCnEccPrivateKey)
  {* Ed25519 ˽Կ��ע�������ǻ���������Ӵպ�Ĳ������ݱ任�����}
  public
    procedure SaveToData(var Data: TCnEd25519Data);
    {* ��˽Կ����ת���� 32 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnEd25519Data             - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnEd25519Data);
    {* �� 32 �ֽڵ�С���ֽ�˳�������м���˽Կ��

       ������
         Data: TCnEd25519Data                 - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnEd25519PublicKey = class(TCnEccPublicKey)
  {* Ed25519 ��Կ��ע��������˽Կֱ�ӳ��Ի������������˽Կ�Ӵպ�Ĳ������ݱ任��˻���}
  public
    procedure SaveToData(var Data: TCnEd25519Data);
    {* ��Կ����ת���� 32 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnEd25519Data             - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnEd25519Data);
    {* �� 32 �ֽڵ�С���ֽ�˳�������м��ع�Կ��

       ������
         Data: TCnEd25519Data                 - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 64 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnEd25519 = class(TCnTwistedEdwardsCurve)
  {* RFC 7748/8032 �й涨�� Ed25519 ����}
  public
    constructor Create; override;
    {* ���캯��}

    function GenerateKeys(PrivateKey: TCnEd25519PrivateKey; PublicKey: TCnEd25519PublicKey): Boolean;
    {* ����һ�� Ed25519 ��Բ���ߵĹ�˽Կ�����й�Կ�Ļ���������� SHA512 ���������

       ������
         PrivateKey: TCnEd25519PrivateKey     - ���ɵ� Ed25519 ��Բ���ߵ�˽Կ
         PublicKey: TCnEd25519PublicKey       - ���ɵ� Ed25519 ��Բ���ߵĹ�Կ

       ����ֵ��Boolean                        - �����Ƿ�ɹ�
    }

    procedure PlainToPoint(Plain: TCnEd25519Data; OutPoint: TCnEccPoint);
    {* �� 32 �ֽ�ֵת��Ϊ����㣬�漰����⡣Ҳ���ڴ� 32 �ֽڸ�ʽ�Ĺ�Կ�лָ�����������㹫Կ��

       ������
         Plain: TCnEd25519Data                - ��ת���� 32 �ֽ�ֵ
         OutPoint: TCnEccPoint                - ת�������������

       ����ֵ�����ޣ�
    }

    procedure PointToPlain(Point: TCnEccPoint; var OutPlain: TCnEd25519Data);
    {* ��������ת���� 32 �ֽ�ֵ��ƴ Y ���� X ����һλ��

       ������
         Point: TCnEccPoint                   - ��ת���������
         var OutPlain: TCnEd25519Data         - ת�������� 32 �ֽ�ֵ

       ����ֵ�����ޣ�
    }

    procedure MultiplePoint(K: TCnBigNumber; P: TCnEccPoint); override;
    {* ���ظ������ͨ��ˣ��ڲ�������չ��Ԫ���ٳ˷���

       ������
         K: TCnBigNumber                      - ��������ʽ�Ǵ���
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    function IsNeutualExtendedPoint(P: TCnEcc4Point): Boolean;
    {* �жϵ��Ƿ������Ե㣬Ҳ�����ж� X = 0 �� Y = Z <> 0 �� T = 0���� Weierstrass ������Զ��ȫ 0 ��ͬ��

       ������
         P: TCnEcc4Point                      - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ������Ե�
    }

    procedure SetNeutualExtendedPoint(P: TCnEcc4Point);
    {* ������Ϊ���Ե㣬Ҳ���� X := 0 �� Y := 1 �� Z := 1 �� T := 0��

       ������
         P: TCnEcc4Point                      - �����õ������

       ����ֵ�����ޣ�
    }

    // ================= ��չŤ�����»����꣨��Ԫ��������㷨 ==================

    procedure ExtendedPointAddPoint(P: TCnEcc4Point; Q: TCnEcc4Point; Sum: TCnEcc4Point); virtual;
    {* ʹ����չŤ�����»����꣨��Ԫ���Ŀ��ٵ�ӷ����� P + Q��ֵ���� Sum �У�Diff ������ P��Q ֮һ��P��Q ������ͬ��
       ���㷨��Դ�� RFC 8032����Ҫ���Ť�����»����ߵ� A ��Ϊ -1����� Ed25519 ���߷��϶� Ed448 ���߲����ϡ�

       ������
         P: TCnEcc4Point                      - ��һ�������������
         Q: TCnEcc4Point                      - �ڶ��������������
         Sum: TCnEcc4Point                    - ����ĺ͵������

       ����ֵ�����ޣ�
    }

    procedure ExtendedPointSubPoint(P: TCnEcc4Point; Q: TCnEcc4Point; Diff: TCnEcc4Point);
    {* ʹ����չŤ�����»����꣨��Ԫ������ P - Q��ֵ���� Diff �У�Diff ������ P��Q ֮һ��P��Q ������ͬ��

       ������
         P: TCnEcc4Point                      - �������������
         Q: TCnEcc4Point                      - �����������
         Diff: TCnEcc4Point                   - ����Ĳ�������

       ����ֵ�����ޣ�
    }

    procedure ExtendedPointInverse(P: TCnEcc4Point);
    {* ʹ����չŤ�����»����꣨��Ԫ������ P �����Ԫ -P��ֵ���·��� P��Ҳ���� Y ֵȡ����

       ������
         P: TCnEcc4Point                      - ��ȡ��Ԫ�������

       ����ֵ�����ޣ�
    }

    function IsExtendedPointOnCurve(P: TCnEcc4Point): Boolean;
    {* �ж���չŤ�����»����꣨��Ԫ�� P ���Ƿ��ڱ������ϡ�

       ������
         P: TCnEcc4Point                      - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ��ڱ�������
    }

    procedure ExtendedMultiplePoint(K: Int64; P: TCnEcc4Point); overload;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㡣

       ������
         K: Int64                             - ����
         P: TCnEcc4Point                      - ���˵������

       ����ֵ�����ޣ�
    }

    procedure ExtendedMultiplePoint(K: TCnBigNumber; P: TCnEcc4Point); overload;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㣬�ٶȱ���ͨ�����˿�ʮ�����ϡ�

       ������
         K: TCnBigNumber                      - ��������ʽΪ����
         P: TCnEcc4Point                      - ���˵������

       ����ֵ�����ޣ�
    }

    // ============= ��չŤ�����»����꣨��Ԫ����Ķ���ʽ�����㷨 ==============

    procedure ExtendedField64PointAddPoint(var P: TCn25519Field64Ecc4Point;
      var Q: TCn25519Field64Ecc4Point; var Sum: TCn25519Field64Ecc4Point);
    {* ʹ����չŤ�����»����꣨��Ԫ�����������ʽ�Ŀ��ٵ�ӷ����� P + Q��ֵ���� Sum �У�Sum ������ P��Q ֮һ��P��Q ������ͬ��

       ������
         var P: TCn25519Field64Ecc4Point      - ��һ�������������
         var Q: TCn25519Field64Ecc4Point      - �ڶ��������������
         var Sum: TCn25519Field64Ecc4Point    - ����ĺ͵������

       ����ֵ�����ޣ�
    }

    procedure ExtendedField64PointSubPoint(var P: TCn25519Field64Ecc4Point;
      var Q: TCn25519Field64Ecc4Point; var Diff: TCn25519Field64Ecc4Point);
    {* ʹ����չŤ�����»����꣨��Ԫ�����������ʽ���� P - Q��ֵ���� Diff �У�Diff ������ P��Q ֮һ��P��Q ������ͬ��

       ������
         var P: TCn25519Field64Ecc4Point      - �������������
         var Q: TCn25519Field64Ecc4Point      - �����������
         var Diff: TCn25519Field64Ecc4Point   - ����Ĳ�������

       ����ֵ�����ޣ�
    }

    procedure ExtendedField64PointInverse(var P: TCn25519Field64Ecc4Point);
    {* ʹ����չŤ�����»����꣨��Ԫ�����������ʽ���� P �����Ԫ -P��ֵ���·��� P��Ҳ���� Y ֵȡ����

       ������
         var P: TCn25519Field64Ecc4Point      - ��ȡ��Ԫ�������

       ����ֵ�����ޣ�
    }

    function IsExtendedField64PointOnCurve(var P: TCn25519Field64Ecc4Point): Boolean;
    {* �ж���չŤ�����»����꣨��Ԫ�����������ʽ P ���Ƿ��ڱ������ϡ�

       ������
         var P: TCn25519Field64Ecc4Point      - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ��ڱ�������
    }

    procedure ExtendedField64MultiplePoint(K: Int64; var P: TCn25519Field64Ecc4Point); overload;
    {* ʹ����չŤ�����»����꣨��Ԫ�����������ʽ����ĳ�� P �� k * P ֵ��ֵ���·���õ㡣

       ������
         K: Int64                             - ����
         var P: TCn25519Field64Ecc4Point      - ���˵������

       ����ֵ�����ޣ�
    }

    procedure ExtendedField64MultiplePoint(K: TCnBigNumber; var P: TCn25519Field64Ecc4Point); overload;
    {* ʹ����չŤ�����»����꣨��Ԫ�����������ʽ����ĳ�� P �� k * P ֵ��ֵ���·���õ㡣

       ������
         K: TCnBigNumber                      - ��������ʽΪ����
         var P: TCn25519Field64Ecc4Point      - ���˵������

       ����ֵ�����ޣ�
    }
  end;

  TCnEd25519Signature = class(TPersistent)
  {* Ed25519 ��ǩ������һ������һ���������� TCnEccSignature ��ͬ}
  private
    FR: TCnEccPoint;
    FS: TCnBigNumber;
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Assign(Source: TPersistent); override;
    {* ����������ֵ������

       ������
         Source: TPersistent                  - ����֮��ֵ��Դ����

       ����ֵ�����ޣ�
    }

    procedure SaveToData(var Sig: TCnEd25519SignatureData);
    {* ����ת���� 64 �ֽ�С��˳��ǩ�����ݹ��洢�봫�䡣

       ������
         var Sig: TCnEd25519SignatureData     - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Sig: TCnEd25519SignatureData);
    {* �� 64 �ֽ�С��˳��ǩ�������м���ǩ����

       ������
         Sig: TCnEd25519SignatureData         - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 128 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 128 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 128 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }

    property R: TCnEccPoint read FR;
    {* ǩ���� R}
    property S: TCnBigNumber read FS;
    {* ǩ���� S}
  end;

  TCnCurve448Data = array[0..CN_448_CURVE_BLOCK_BYTESIZE - 1] of Byte;
  {* Curve448 �ĳ������� 56 �ֽڣ�RFC �涨��������С���ֽ�˳��}

  TCnCurve448PrivateKey = class(TCnEccPrivateKey)
  {* Curve448 ˽Կ��Ҳ�ǻ������}
  public
    procedure SaveToData(var Data: TCnCurve448Data);
    {* ��˽Կ����ת���� 56 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnCurve448Data            - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnCurve448Data);
    {* �� 56 �ֽڵ�С���ֽ�˳�������м���˽Կ��

       ������
         Data: TCnCurve448Data                - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 112 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 112 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 112 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnCurve448PublicKey = class(TCnEccPublicKey)
  {* Curve448 ��Կ���Ƕ�Ӧ˽Կ���Ի���õ�������}
  public
    procedure SaveToData(var Data: TCnCurve448Data);
    {* ��Կ����ת���� 56 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnCurve448Data            - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnCurve448Data);
    {* �� 56 �ֽڵ�С���ֽ�˳�������м��ع�Կ��

       ������
         Data: TCnCurve448Data                - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 112 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 112 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 112 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnCurve448 = class(TCnMontgomeryCurve)
  {* RFC 7748/8032 �й涨�� Curve448 ����}
  public
    constructor Create; override;
    {* ���캯�����ڲ���ʼ���ɸ����� 448 ���ߵĲ���}

    function GenerateKeys(PrivateKey: TCnCurve448PrivateKey; PublicKey: TCnCurve448PublicKey): Boolean;
    {* ����һ�� Curve448 ��Բ���ߵĹ�˽Կ������˽Կ�ĸߵ�λ�����⴦��

       ������
         PrivateKey: TCnCurve448PrivateKey    - ���ɵ� Curve448 ��Բ���ߵ�˽Կ
         PublicKey: TCnCurve448PublicKey      - ���ɵ� Curve448 ��Բ���ߵĹ�Կ

       ����ֵ��Boolean                        - �����Ƿ�ɹ�
    }

    procedure MultiplePoint(K: TCnBigNumber; P: TCnEccPoint); override;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㡣P ������ֻ�� X ��Ϣ
       ע�� 448 ������ 2^51 �Ķ���ʽ�����㷨���ڲ�ʵ�ֽ� X ����Ӱ����ɸ����������㷨��

       ������
         K: TCnBigNumber                      - ��������ʽ�Ǵ���
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }
  end;

  TCnEd448Data = array[0..CN_448_EDWARDS_BLOCK_BYTESIZE - 1] of Byte;
  {* Ed448 �Ĺ�˽Կ���ݣ�����һ����С���ֽ�˳��}

  TCnEd448SignatureData = array[0..2 * CN_448_EDWARDS_BLOCK_BYTESIZE - 1] of Byte;
  {* Ed448 ��ǩ�����ݣ�����һ����С���ֽ�˳��}

  TCnEd448Signature = class(TPersistent)
  {* Ed448 ��ǩ������һ������һ���������� TCnEccSignature ��ͬ}
  private
    FR: TCnEccPoint;
    FS: TCnBigNumber;
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure Assign(Source: TPersistent); override;
    {* ����������ֵ������

       ������
         Source: TPersistent                  - ����֮��ֵ��Դ����

       ����ֵ�����ޣ�
    }

    procedure SaveToData(var Sig: TCnEd448SignatureData);
    {* ����ת���� 114 �ֽ�ǩ�����ݹ��洢�봫�䡣

       ������
         var Sig: TCnEd448SignatureData       - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Sig: TCnEd448SignatureData);
    {* �� 114 �ֽ�ǩ�������м���ǩ����

       ������
         Sig: TCnEd448SignatureData           - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 228 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 228 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 228 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }

    property R: TCnEccPoint read FR;
    {* ǩ���� R}
    property S: TCnBigNumber read FS;
    {* ǩ���� S}
  end;

  TCnEd448PrivateKey = class(TCnEccPrivateKey)
  {* Ed448 ˽Կ��ע�������ǻ���������Ӵպ�Ĳ������ݱ任�����}
  public
    procedure SaveToData(var Data: TCnEd448Data);
    {* ��˽Կ����ת���� 57 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnEd448Data               - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnEd448Data);
    {* �� 57 �ֽڵ�С���ֽ�˳�������м���˽Կ��

       ������
         Sig: TCnEd448Data                    - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 114 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 114 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 114 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnEd448PublicKey = class(TCnEccPublicKey)
  {* Ed448 ��Կ��ע��������˽Կֱ�ӳ��Ի������������˽Կ�Ӵպ�Ĳ������ݱ任��˻���}
  public
    procedure SaveToData(var Data: TCnEd448Data);
    {* ˽Կ����ת���� 57 �ֽڵ�С���ֽ�˳�����ݹ��洢�봫�䡣

       ������
         var Data: TCnEd448Data               - ת��������С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    procedure LoadFromData(Data: TCnEd448Data);
    {* �� 57 �ֽڵ�С���ֽ�˳�������м���˽Կ��

       ������
         Sig: TCnEd448Data                    - �����ص�С���ֽ�˳������

       ����ֵ�����ޣ�
    }

    function SaveToHex(UseUpperCase: Boolean = True): string;
    {* ת��Ϊ 114 �ֽڵ�С���ֽ�˳���ʮ�������ַ������� RFC �е�һ�¡�

       ������
         UseUpperCase: Boolean                - ʮ�������ַ������Ƿ�ʹ�ô�д��ĸ

       ����ֵ��string                         - ת����� 114 �ֽڵ�С���ֽ�˳���ʮ�������ַ���
    }

    procedure LoadFromHex(const Hex: string);
    {* �� 114 �ֽڵ�С���ֽ�˳���ʮ�������ַ����м��أ��� RFC �е�һ�¡�

       ������
         const Hex: string                    - �����ص�ʮ�������ַ���

       ����ֵ�����ޣ�
    }
  end;

  TCnEd448 = class(TCnTwistedEdwardsCurve)
  {* RFC 7748/8032 �й涨�� Ed448 ����}
  public
    constructor Create; override;
    {* ���캯�����ڲ���ʼ����Ť�����»� 448 ���ߵĲ���}

    function GenerateKeys(PrivateKey: TCnEd448PrivateKey; PublicKey: TCnEd448PublicKey): Boolean;
    {* ����һ�� Ed448 ��Բ���ߵĹ�˽Կ�����й�Կ�Ļ���������� SHAKE256 ���������

       ������
         PrivateKey: TCnEd448PrivateKey       - ���ɵ� Ed448 ��Բ���ߵ�˽Կ
         PublicKey: TCnEd448PublicKey         - ���ɵ� Ed448 ��Բ���ߵĹ�Կ

       ����ֵ��Boolean                        - �����Ƿ�ɹ�
    }

    procedure PlainToPoint(Plain: TCnEd448Data; OutPoint: TCnEccPoint);
    {* �� 57 �ֽ�ֵת��Ϊ����㣬�漰����⡣Ҳ���ڴ� 57 �ֽڸ�ʽ�Ĺ�Կ�лָ�����������㹫Կ��

       ������
         Plain: TCnEd448Data                  - ��ת���� 57 �ֽ�ֵ
         OutPoint: TCnEccPoint                - ת�������������

       ����ֵ�����ޣ�
    }

    procedure PointToPlain(Point: TCnEccPoint; var OutPlain: TCnEd448Data);
    {* �������ת���� 57 �ֽ�ֵ��ƴ Y ���� X ����һλ��

       ������
         Point: TCnEccPoint                   - ��ת���������
         var OutPlain: TCnEd448Data           - ת�������� 57 �ֽ�ֵ

       ����ֵ�����ޣ�
    }

    procedure MultiplePoint(K: TCnBigNumber; P: TCnEccPoint); override;
    {* ���ظ������ͨ��ˣ��ڲ�������չ��Ԫ���ٳˡ�

       ������
         K: TCnBigNumber                      - ��������ʽ�Ǵ���
         P: TCnEccPoint                       - ���˵������

       ����ֵ�����ޣ�
    }

    function IsNeutualAffinePoint(P: TCnEcc3Point): Boolean;
    {* �жϵ��Ƿ�����Ԫ���Ե㣬Ҳ�����ж� X = 0 �� Y = Z <> 0���� Weierstrass ������Զ��ȫ 0 ��ͬ��

       ������
         P: TCnEcc3Point                      - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ������Ե�
    }

    procedure SetNeutualAffinePoint(P: TCnEcc3Point);
    {* ������Ϊ��Ԫ���Ե㣬Ҳ���� X := 0 �� Y := 1 �� Z := 1��

       ������
         P: TCnEcc3Point                      - �����õ������

       ����ֵ�����ޣ�
    }

    // ================ ��չ��Ť�����»����꣨��Ԫ��������㷨 =================

    procedure AffinePointAddPoint(P: TCnEcc3Point; Q: TCnEcc3Point; Sum: TCnEcc3Point);
    {* ʹ����չ��Ť�����»����꣨��Ԫ���Ŀ��ٵ�ӷ����� P + Q��ֵ���� Sum �У�Diff ������ P��Q ֮һ��P��Q ������ͬ��
       ���㷨��Դ�� RFC 8032����Ҫ��÷�Ť�����»����ߵ� A ����Ϊ 1��Ed448 ����ǡ�÷��ϡ�

       ������
         P: TCnEcc3Point                      - ��һ�������������
         Q: TCnEcc3Point                      - �ڶ��������������
         Sum: TCnEcc3Point                    - ����ĺ͵������

       ����ֵ�����ޣ�
    }

    procedure AffinePointSubPoint(P: TCnEcc3Point; Q: TCnEcc3Point; Diff: TCnEcc3Point);
    {* ʹ����չ��Ť�����»����꣨��Ԫ������ P - Q��ֵ���� Diff �У�Diff ������ P��Q ֮һ��P��Q ������ͬ��

       ������
         P: TCnEcc3Point                      - �������������
         Q: TCnEcc3Point                      - �����������
         Diff: TCnEcc3Point                   - ����Ĳ�������

       ����ֵ�����ޣ�
    }

    procedure AffinePointInverse(P: TCnEcc3Point);
    {* ʹ����չ��Ť�����»����꣨��Ԫ������ P �����Ԫ -P��ֵ���·��� P��Ҳ���� Y ֵȡ����

       ������
         P: TCnEcc3Point                      - ��ȡ��Ԫ�������

       ����ֵ�����ޣ�
    }

    function IsAffinePointOnCurve(P: TCnEcc3Point): Boolean;
    {* �ж���չ��Ť�����»����꣨��Ԫ�� P ���Ƿ��ڱ������ϡ�

       ������
         P: TCnEcc3Point                      - �����жϵ������

       ����ֵ��Boolean                        - �Ƿ��ڱ�������
    }

    procedure AffineMultiplePoint(K: Int64; P: TCnEcc3Point); overload;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㡣

       ������
         K: Int64                             - ����
         P: TCnEcc3Point                      - ���˵������

       ����ֵ�����ޣ�
    }

    procedure AffineMultiplePoint(K: TCnBigNumber; P: TCnEcc3Point); overload;
    {* ����ĳ�� P �� k * P ֵ��ֵ���·���õ㣬�ٶȱ���ͨ�����˿첻�١�

       ������
         K: TCnBigNumber                      - ��������ʽ�Ǵ���
         P: TCnEcc3Point                      - ���˵������

       ����ֵ�����ޣ�
    }
  end;

// ========================= ��Բ���������ת������ ============================

function CnEcc4PointToString(P: TCnEcc4Point): string;
{* ��һ�� TCnEcc4Point ������ת��Ϊʮ�����ַ�����

   ������
     P: TCnEcc4Point                      - ��ת���������

   ����ֵ��string                         - ʮ�����ַ�����ʽ��ת�����
}

function CnEcc4PointToHex(P: TCnEcc4Point): string;
{* ��һ�� TCnEcc4Point ������ת��Ϊʮ�������ַ�����

   ������
     P: TCnEcc4Point                      - ��ת���������

   ����ֵ��string                         - ʮ�������ַ�����ʽ��ת�����
}

function CnEcc4PointEqual(P: TCnEcc4Point; Q: TCnEcc4Point;
  Prime: TCnBigNumber): Boolean;
{* �ж����� TCnEcc4Point �Ƿ�ͬһ���㡣

   ������
     P: TCnEcc4Point                      - ���Ƚϵ������һ
     Q: TCnEcc4Point                      - ���Ƚϵ�������
     Prime: TCnBigNumber                  - �������Ͻ�

   ����ֵ��Boolean                        - �����Ƿ�ͬһ����
}

function CnEccPointToEcc4Point(DestPoint: TCnEcc4Point; SourcePoint: TCnEccPoint;
  Prime: TCnBigNumber): Boolean;
{* ������Χ�ڵ���ͨ���굽��չ��������ĵ�ת����

   ������
     DestPoint: TCnEcc4Point              - Ŀ����չ���������
     SourcePoint: TCnEccPoint             - Դ�����
     Prime: TCnBigNumber                  - �������Ͻ�

   ����ֵ��Boolean                        - ����ת���Ƿ�ɹ�
}

function CnEcc4PointToEccPoint(DestPoint: TCnEccPoint; SourcePoint: TCnEcc4Point;
  Prime: TCnBigNumber): Boolean;
{* ������Χ�ڵ���չ�������굽��ͨ����ĵ�ת����

   ������
     DestPoint: TCnEccPoint               - Ŀ�������
     SourcePoint: TCnEcc4Point            - Դ��չ���������
     Prime: TCnBigNumber                  - �������Ͻ�

   ����ֵ��Boolean                        - ����ת���Ƿ�ɹ�
}

// ========================= 25519 ��Բ���߸������� ============================

procedure CnCurve25519PointToEd25519Point(DestPoint: TCnEccPoint; SourcePoint: TCnEccPoint);
{* �� Curve25519 �������ת��Ϊ Ed25519 ������㣬Source �� Dest ������ͬ��

   ������
     DestPoint: TCnEccPoint               - Ŀ�� Ed25519 �����
     SourcePoint: TCnEccPoint             - Դ Curve25519 �����

   ����ֵ�����ޣ�
}

procedure CnEd25519PointToCurve25519Point(DestPoint: TCnEccPoint; SourcePoint: TCnEccPoint);
{* �� Ed25519 �������ת��Ϊ Curve25519 ������㣬Source �� Dest ������ͬ��

   ������
     DestPoint: TCnEccPoint               - Ŀ�� Curve25519 �����
     SourcePoint: TCnEccPoint             - Դ Ed25519 �����

   ����ֵ�����ޣ�
}

procedure CnCurve25519PointToData(P: TCnEccPoint; var Data: TCnCurve25519Data);
{* �� Curve25519 ��׼����Բ���ߵ�ת��Ϊѹ����ʽ�� 32 �ֽ����飬�� X ֵ��

   ������
     P: TCnEccPoint                       - ��ת���� Curve25519 �����
     var Data: TCnCurve25519Data          - ת�������Ľ������

   ����ֵ�����ޣ�
}

procedure CnCurve25519DataToPoint(Data: TCnCurve25519Data; P: TCnEccPoint);
{* �� Curve25519 ��׼�� 32 �ֽ�����ת��Ϊ��Բ���ߵ�ѹ����ʽ��P �з��ض�Ӧ X ֵ�������� Y��

   ������
     Data: TCnCurve25519Data              - ��ת��������
     P: TCnEccPoint                       - ת�������� Curve25519 �����

   ����ֵ�����ޣ�
}

procedure CnEd25519PointToData(P: TCnEccPoint; var Data: TCnEd25519Data);
{* �� Ed25519 ��׼����Բ���ߵ�ת��Ϊѹ����ʽ�� 32 �ֽ����飬�� Y ֵ�� X ����ż��

   ������
     P: TCnEccPoint                       - ��ת���� Ed25519 �����
     var Data: TCnEd25519Data             - ת�������Ľ������

   ����ֵ�����ޣ�
}

procedure CnEd25519DataToPoint(Data: TCnEd25519Data; P: TCnEccPoint; out XOdd: Boolean);
{* �� Ed25519 ��׼�� 32 �ֽ�����ת��Ϊ��Բ���ߵ�ѹ����ʽ��
  P �з��ض�Ӧ Y ֵ���Լ� XOdd �з��ض�Ӧ�� X ֵ�Ƿ�����������Ҫ������н� X

   ������
     Data: TCnEd25519Data                 - ��ת��������
     P: TCnEccPoint                       - ת�������� Ed25519 �����
     out XOdd: Boolean                    - ���ض�Ӧ�� X ֵ�Ƿ�Ϊ����

   ����ֵ�����ޣ�
}

procedure CnEd25519BigNumberToData(N: TCnBigNumber; var Data: TCnEd25519Data);
{* �� Ed25519 ��׼������ת��Ϊ 32 �ֽ����顣

   ������
     N: TCnBigNumber                      - ��ת���Ĵ���
     var Data: TCnEd25519Data             - ת�������Ľ������

   ����ֵ�����ޣ�
}

procedure CnEd25519DataToBigNumber(Data: TCnEd25519Data; N: TCnBigNumber);
{* �� Ed25519 ��׼�� 32 �ֽ�����ת��Ϊ������

   ������
     Data: TCnEd25519Data                 - ��ת��������
     N: TCnBigNumber                      - ת�������Ĵ���

   ����ֵ�����ޣ�
}

procedure CnCurve25519BigNumberToData(N: TCnBigNumber; var Data: TCnCurve25519Data);
{* �� Curve25519 ��׼������ת��Ϊ 32 �ֽ����顣

   ������
     N: TCnBigNumber                      - ��ת���Ĵ���
     var Data: TCnCurve25519Data          - ת�������Ľ������

   ����ֵ�����ޣ�
}

procedure CnCurve25519DataToBigNumber(Data: TCnCurve25519Data; N: TCnBigNumber);
{* �� Curve25519 ��׼�� 32 �ֽ�����ת��Ϊ������

   ������
     Data: TCnCurve25519Data              - ��ת��������
     N: TCnBigNumber                      - ת�������Ĵ���

   ����ֵ�����ޣ�
}

procedure CnProcess25519ScalarNumber(Num: TCnBigNumber);
{* �� RFC �涨���� 25519 ���������˽Կ

   ������
     Num: TCnBigNumber                    - ��������������˽Կ����ʽ�Ǵ���

   ����ֵ�����ޣ�
}

// ===================== Ed25519 ��Բ��������ǩ����֤�㷨 ======================

function CnEd25519SignData(PlainData: Pointer; DataByteLen: Integer; PrivateKey: TCnEd25519PrivateKey;
  PublicKey: TCnEd25519PublicKey; OutSignature: TCnEd25519Signature; Ed25519: TCnEd25519 = nil): Boolean;
{* �� Ed25519 ��˽Կ�����ݿ����ǩ������֧�� Ed25519ctx �� Ed25519ph������ǩ���Ƿ�ɹ���
   Ϊ������Ч������������б�֤��˽Կƥ�����ǩ����Ч��

   ������
     PlainData: Pointer                   - ��ǩ�������ݿ���ڴ��ַ
     DataByteLen: Integer                 - ��ǩ�������ݿ���ֽڳ���
     PrivateKey: TCnEd25519PrivateKey     - Ed25519 ˽Կ
     PublicKey: TCnEd25519PublicKey       - Ed25519 ��Կ
     OutSignature: TCnEd25519Signature    - �����ǩ��ֵ
     Ed25519: TCnEd25519                  - Ed25519 ʵ��

   ����ֵ��Boolean                        - ����ǩ���Ƿ�ɹ�
}

function CnEd25519VerifyData(PlainData: Pointer; DataByteLen: Integer; InSignature: TCnEd25519Signature;
  PublicKey: TCnEd25519PublicKey; Ed25519: TCnEd25519 = nil): Boolean;
{* �� Ed25519 ��Կ�����ݿ���ǩ��������֤����֧�� Ed25519ctx �� Ed25519ph��������֤�Ƿ�ɹ���

   ������
     PlainData: Pointer                   - ����֤�����ݿ���ڴ��ַ
     DataByteLen: Integer                 - ����֤�����ݿ���ֽڳ���
     InSignature: TCnEd25519Signature     - ����֤��ǩ��ֵ
     PublicKey: TCnEd25519PublicKey       - Ed25519 ��Կ
     Ed25519: TCnEd25519                  - Ed25519 ʵ��

   ����ֵ��Boolean                        - ������֤ǩ���Ƿ�ɹ�
}

function CnEd25519SignFile(const FileName: string; PrivateKey: TCnEd25519PrivateKey;
  PublicKey: TCnEd25519PublicKey; OutSignatureStream: TStream; Ed25519: TCnEd25519 = nil): Boolean;
{* �� Ed25519 ��˽Կ���ļ�����ǩ������֧�� Ed25519ctx �� Ed25519ph��
   ǩ��ֵ 64 �ֽ�д�� OutSignatureStream �У�����ǩ���Ƿ�ɹ���

   ������
     const FileName: string               - ��ǩ�����ļ���
     PrivateKey: TCnEd25519PrivateKey     - Ed25519 ˽Կ
     PublicKey: TCnEd25519PublicKey       - Ed25519 ��Կ
     OutSignatureStream: TStream          - �����ǩ��ֵ
     Ed25519: TCnEd25519                  - Ed25519 ʵ��

   ����ֵ��Boolean                        - ����ǩ���Ƿ�ɹ�
}

function CnEd25519VerifyFile(const FileName: string; InSignatureStream: TStream;
  PublicKey: TCnEd25519PublicKey; Ed25519: TCnEd25519 = nil): Boolean;
{* �� Ed25519 ��Կ���ļ���ǩ��������֤����֧�� Ed25519ctx �� Ed25519ph��
   InSignatureStream �ڲ����� 64 �ֽ�ǩ��ֵ��������֤�Ƿ�ɹ���

   ������
     const FileName: string               - ����֤���ļ���
     InSignatureStream: TStream           - ����֤��ǩ��ֵ
     PublicKey: TCnEd25519PublicKey       - Ed25519 ��Կ
     Ed25519: TCnEd25519                  - Ed25519 ʵ��

   ����ֵ��Boolean                        - ������֤ǩ���Ƿ�ɹ�
}

// =============== Curve25519 ��Բ���� Diffie-Hellman ��Կ����  ================

function CnCurve25519KeyExchangeStep1(SelfPrivateKey: TCnEccPrivateKey;
  OutPointToAnother: TCnEccPoint; Curve25519: TCnCurve25519 = nil): Boolean;
{* ���� Curve25519 �� Diffie-Hellman ��Կ�����㷨��A �� B ���ȵ��ô˷�����
   ���ݸ���˽Կ���ɵ����꣬�õ������跢���Է������������Ƿ�ɹ���

   ������
     SelfPrivateKey: TCnEccPrivateKey     - ����������� Curve25519 ˽Կ
     OutPointToAnother: TCnEccPoint       - ���ɵ�����㣬��������Է�
     Curve25519: TCnCurve25519            - Curve25519 ʵ��

   ����ֵ��Boolean                        - ���������Ƿ�ɹ�
}

function CnCurve25519KeyExchangeStep2(SelfPrivateKey: TCnEccPrivateKey;
  InPointFromAnother: TCnEccPoint; OutKey: TCnEccPoint; Curve25519: TCnCurve25519 = nil): Boolean;
{* ���� Curve25519 �� Diffie-Hellman ��Կ�����㷨��A �� B �յ��Է��� Point ������ٵ��ô˷�����
   ���ݸ���˽Կ����һ��ͬ������㣬��������Ϊ������Կ������ͨ��������һ�����ӻ���
   ���������Ƿ�ɹ���

   ������
     SelfPrivateKey: TCnEccPrivateKey     - ����������� Curve25519 ˽Կ
     InPointFromAnother: TCnEccPoint      - �ӵ�һ�����õ��ĶԷ����ɵ������
     OutKey: TCnEccPoint                  - ����Ĺ�����Կ
     Curve25519: TCnCurve25519            - Curve25519 ʵ��

   ����ֵ��Boolean                        - ���ع�����Կ�Ƿ����ɳɹ�
}

// ============================== ����ʽ�����㷨 ===============================

procedure Cn25519BigNumberToField64(var Field: TCn25519Field64; Num: TCnBigNumber);
{* ��һ������ת��Ϊ 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����

   ������
     var Field: TCn25519Field64           - ת�������� 64 λ����ʽϵ��
     Num: TCnBigNumber                    - ��ת���Ĵ���

   ����ֵ�����ޣ�
}

procedure Cn25519Field64ToBigNumber(Res: TCnBigNumber; var Field: TCn25519Field64);
{* ��һ������ת��Ϊ 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����

   ������
     Res: TCnBigNumber                    - ת�������Ĵ���
     var Field: TCn25519Field64           - ��ת���� 64 λ����ʽϵ��
                                                          
   ����ֵ�����ޣ�
}

procedure Cn25519Field64Reduce(var Field: TCn25519Field64);
{* ��һ�� 64 λ����ʽϵ���� 2^255-19 ������Χ�����滯��
   Ҳ���ǰ�ÿ��ϵ��ȷ���� 2^51 С����Ĳ��ֽ�λ����һ������ֵ�糬���������Ͻ�Ҳ���Զ���ģ��

   ������
     var Field: TCn25519Field64           - �����滯�� 64 λ����ʽϵ��

   ����ֵ�����ޣ�
}

function Cn25519Field64ToHex(var Field: TCn25519Field64): string;
{* ��һ�� 64 λ����ʽϵ��ת��Ϊʮ�������ַ�����

   ������
     var Field: TCn25519Field64           - ��ת���� 64 λ����ʽϵ��

   ����ֵ��string                         - ����ʮ�������ַ���
}

procedure Cn25519Field64Copy(var Dest: TCn25519Field64; var Source: TCn25519Field64);
{* ����һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����

   ������
     var Dest: TCn25519Field64            - Ŀ�� 64 λ����ʽϵ��
     var Source: TCn25519Field64          - Դ 64 λ����ʽϵ��

   ����ֵ�����ޣ�
}

function Cn25519Field64Equal(var A: TCn25519Field64; var B: TCn25519Field64): Boolean;
{* �ж����� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ���Ƿ���ȡ�

   ������
     var A: TCn25519Field64               - ���Ƚϵ� 64 λ����ʽϵ��һ
     var B: TCn25519Field64               - ���Ƚϵ� 64 λ����ʽϵ����

   ����ֵ��Boolean                        - �����Ƿ����
}

procedure Cn25519Field64Swap(var A: TCn25519Field64; var B: TCn25519Field64);
{* �������� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����

   ������
     var A: TCn25519Field64               - �������� 64 λ����ʽϵ��һ
     var B: TCn25519Field64               - �������� 64 λ����ʽϵ����

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Zero(var Field: TCn25519Field64);
{* ��һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����Ϊ 0��

   ������
     var Field: TCn25519Field64           - ���� 0 �� 64 λ����ʽϵ��

   ����ֵ�����ޣ�
}

procedure Cn25519Field64One(var Field: TCn25519Field64);
{* ��һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����Ϊ 1��

   ������
     var Field: TCn25519Field64           - ���� 1 �� 64 λ����ʽϵ��

   ����ֵ�����ޣ�
}

procedure Cn25519Field64NegOne(var Field: TCn25519Field64);
{* ��һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����Ϊ -1��

   ������
     var Field: TCn25519Field64           - ���� -1 �� 64 λ����ʽϵ��

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Negate(var Field: TCn25519Field64);
{* ��һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����Ϊ�෴����

   ������
     var Field: TCn25519Field64           - ��ȡ���� 64 λ����ʽϵ��

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Add(var Res: TCn25519Field64; var A: TCn25519Field64; var B: TCn25519Field64);
{* ���� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����ӣ�A + B => Res��Res ������ A �� B��A��B ������ͬһ����

   ������
     var Res: TCn25519Field64             - 64 λ����ʽϵ����
     var A: TCn25519Field64               - 64 λ����ʽϵ������һ
     var B: TCn25519Field64               - 64 λ����ʽϵ��������

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Sub(var Res: TCn25519Field64; var A: TCn25519Field64; var B: TCn25519Field64);
{* ���� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ�������A - B => Res��Res ������ A �� B��A��B ������ͬһ����

   ������
     var Res: TCn25519Field64             - 64 λ����ʽϵ����
     var A: TCn25519Field64               - 64 λ����ʽϵ��������
     var B: TCn25519Field64               - 64 λ����ʽϵ������

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Mul(var Res: TCn25519Field64; var A: TCn25519Field64; var B: TCn25519Field64);
{* ���� 2^255-19 ������Χ�ڵ� 64 λ����ʽϵ����ˣ�A * B => Res��Res ������ A �� B��A��B ������ͬһ����

   ������
     var Res: TCn25519Field64             - 64 λ����ʽϵ����
     var A: TCn25519Field64               - 64 λ����ʽϵ������һ
     var B: TCn25519Field64               - 64 λ����ʽϵ��������

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Power(var Res: TCn25519Field64; var A: TCn25519Field64; K: Cardinal); overload;
{* ����һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽ�� K �η�ֵ��A^K) => Res��Res ������ A��

   ������
     var Res: TCn25519Field64             - 64 λ����ʽϵ���˷����
     var A: TCn25519Field64               - ������˷��� 64 λ����ʽϵ��
     K: Cardinal                          - ָ��

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Power(var Res: TCn25519Field64; var A: TCn25519Field64; K: TCnBigNumber); overload;
{* ����һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽ�� K �η�ֵ��A^K  => Res��Res ������ A��

   ������
     var Res: TCn25519Field64             - 64 λ����ʽϵ���˷����
     var A: TCn25519Field64               - ������˷��� 64 λ����ʽϵ��
     K: TCnBigNumber                      - ָ������ʽ�Ǵ���

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Power2K(var Res: TCn25519Field64; var A: TCn25519Field64; K: Cardinal);
{* ����һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽ�� 2^K �η�ֵ��A^(2^K) => Res��Res ������ A��

   ������
     var Res: TCn25519Field64             - 64 λ����ʽϵ���˷����
     var A: TCn25519Field64               - ������ 2 �ݴη��� 64 λ����ʽϵ��
     K: Cardinal                          - 2 ��ָ��

   ����ֵ�����ޣ�
}

procedure Cn25519Field64ModularInverse(var Res: TCn25519Field64; var A: TCn25519Field64);
{* ����һ�� 2^255-19 ������Χ�ڵ� 64 λ����ʽ��ģ��Ԫ��A * Res mod P = 1��Res ������ A��

   ������
     var Res: TCn25519Field64             - 64 λ����ʽϵ��ģ��Ԫ���
     var A: TCn25519Field64               - ������ģ��Ԫ�� 64 λ����ʽϵ��

   ����ֵ�����ޣ�
}

// =========================== ����ʽ�㴦���� ================================

procedure Cn25519Field64EccPointZero(var Point: TCn25519Field64EccPoint);
{* ��һ����ʽ�����ʾ�� 25519 ��Բ�����ϵĵ��� 0��

   ������
     var Point: TCn25519Field64EccPoint   - ���� 0 �Ķ���ʽ���������

   ����ֵ�����ޣ�
}

procedure Cn25519Field64EccPointCopy(var DestPoint: TCn25519Field64EccPoint;
  var SourcePoint: TCn25519Field64EccPoint);
{* ���ƶ���ʽ�����ʾ�� 25519 ��Բ�����ϵĵ㡣

   ������
     var DestPoint: TCn25519Field64EccPoint               - Ŀ�����ʽ���������
     var SourcePoint: TCn25519Field64EccPoint             - Դ����ʽ���������

   ����ֵ�����ޣ�
}

function Cn25519Field64EccPointToHex(var Point: TCn25519Field64EccPoint): string;
{* ��һ����ʽ�����ʾ�� 25519 ��Բ�����ϵĵ�ת��Ϊʮ�������ַ���

   ������
     var Point: TCn25519Field64EccPoint   - ��ת���Ķ���ʽ���������

   ����ֵ��string                         - ����ʮ�������ַ���
}

function Cn25519Field64EccPointEqual(var A: TCn25519Field64EccPoint; var B: TCn25519Field64EccPoint): Boolean;
{* �ж���������ʽ�����ʾ�� 25519 ��Բ�����ϵĵ��Ƿ����

   ������
     var A: TCn25519Field64EccPoint       - ���ȽϵĶ���ʽ���������һ
     var B: TCn25519Field64EccPoint       - ���ȽϵĶ���ʽ����������

   ����ֵ��Boolean                        -
}

procedure Cn25519Field64Ecc4PointNeutual(var Point: TCn25519Field64Ecc4Point);
{* ��һ����ʽ�����ʾ�� 25519 ��Բ�����ϵ���Ԫ��չ����Ϊ���Ե㡣

   ������
     var Point: TCn25519Field64Ecc4Point  - ����Ϊ���Ե�Ķ���ʽ������Ԫ��չ��

   ����ֵ�����ޣ�
}

procedure Cn25519Field64Ecc4PointCopy(var DestPoint: TCn25519Field64Ecc4Point;
  var SourcePoint: TCn25519Field64Ecc4Point);
{* ���ƶ���ʽ�����ʾ�� 25519 ��Բ�����ϵ���Ԫ��չ�㡣

   ������
     var DestPoint: TCn25519Field64Ecc4Point              - Ŀ�����ʽ������Ԫ��չ��
     var SourcePoint: TCn25519Field64Ecc4Point            - Դ����ʽ������Ԫ��չ��

   ����ֵ�����ޣ�
}

function Cn25519Field64Ecc4PointToHex(var Point: TCn25519Field64Ecc4Point): string;
{* ��һ����ʽ�����ʾ�� 25519 ��Բ�����ϵ���Ԫ��չ��ת��Ϊʮ�������ַ�����

   ������
     var Point: TCn25519Field64Ecc4Point  - ��ת���Ķ���ʽ������Ԫ��չ��

   ����ֵ��string                         - ����ʮ�������ַ���
}

function Cn25519Field64Ecc4PointEqual(var A: TCn25519Field64Ecc4Point;
  var B: TCn25519Field64Ecc4Point): Boolean;
{* �ж���������ʽ�����ʾ�� 25519 ��Բ�����ϵĵ��Ƿ���ȡ�

   ������
     var A: TCn25519Field64Ecc4Point      - ���ȽϵĶ���ʽ������Ԫ��չ��һ
     var B: TCn25519Field64Ecc4Point      - ���ȽϵĶ���ʽ������Ԫ��չ���

   ����ֵ��Boolean                        - �����Ƿ����
}

function CnEccPointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEccPoint): Boolean;
{* ������Χ�ڵ���ͨ���굽��չ�������ʽ����ĵ�ת����

   ������
     var DestPoint: TCn25519Field64Ecc4Point              - ��չ�������ʽ�������
     SourcePoint: TCnEccPoint                             - ��ת������ͨ�����

   ����ֵ��Boolean                                        - ����ת���Ƿ�ɹ�
}

function CnField64Ecc4PointToEccPoint(DestPoint: TCnEccPoint;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
{* ������Χ�ڵ���չ�������ʽ���굽��ͨ����ĵ�ת����

   ������
     DestPoint: TCnEccPoint                               - ��ͨ�����ת�����
     var SourcePoint: TCn25519Field64Ecc4Point            - ��ת���ķ������ʽ�����

   ����ֵ��Boolean                                        - ����ת���Ƿ�ɹ�
}

function CnEcc4PointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEcc4Point): Boolean;
{* ������Χ�ڵ���չ�������굽��չ�������ʽ����ĵ�ת����

   ������
     var DestPoint: TCn25519Field64Ecc4Point              - ��չ�������ʽ�����ת�����
     SourcePoint: TCnEcc4Point                            - ��ת������չ���������

   ����ֵ��Boolean                                        - ����ת���Ƿ�ɹ�
}

function CnField64Ecc4PointToEcc4Point(DestPoint: TCnEcc4Point;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
{* ������Χ�ڵ���չ�������ʽ���굽��չ��������ĵ�ת����

   ������
     DestPoint: TCnEcc4Point                              - ��չ���������ת�����
     var SourcePoint: TCn25519Field64Ecc4Point            - ��ת������չ�������ʽ�����

   ����ֵ��Boolean                                        - ����ת���Ƿ�ɹ�
}

// ========================== 448 ��Բ���߸������� =============================

procedure CnCurve448PointToEd448Point(DestPoint: TCnEccPoint; SourcePoint: TCnEccPoint);
{* �� Curve448 �������ת��Ϊ Ed448 ������㣬Source �� Dest ������ͬ��
   ע��÷���δ��֤�ɹ�

   ������
     DestPoint: TCnEccPoint               - Ed448 �������ת�����
     SourcePoint: TCnEccPoint             - ��ת���� Curve448 �����

   ����ֵ�����ޣ�
}

procedure CnEd448PointToCurve448Point(DestPoint: TCnEccPoint; SourcePoint: TCnEccPoint);
{* �� Ed448 �������ת��Ϊ Curve448 ������㣬Source �� Dest ������ͬ��

   ������
     DestPoint: TCnEccPoint               - Curve448 �������ת�����
     SourcePoint: TCnEccPoint             - ��ת���� Ed448 �����

   ����ֵ�����ޣ�
}

procedure CnCurve448PointToData(P: TCnEccPoint; var Data: TCnCurve448Data);
{* ���ɸ����� 448 ��׼����Բ���ߵ�ת��Ϊѹ����ʽ�� 56 �ֽ����飬�� X ֵ��

   ������
     P: TCnEccPoint                       - ��ת���������
     var Data: TCnCurve448Data            - ���ɸ����� 448 ��׼ת�����ѹ����ʽ�� 56 �ֽ�������

   ����ֵ�����ޣ�
}

procedure CnCurve448DataToPoint(Data: TCnCurve448Data; P: TCnEccPoint);
{* ���ɸ����� 448 ��׼�� 56 �ֽ�����ת��Ϊ��Բ���ߵ�ѹ����ʽ��P �з��ض�Ӧ X ֵ��������� Y��

   ������
     Data: TCnCurve448Data                - ��ת���� 56 �ֽ�����
     P: TCnEccPoint                       - ���ɸ����� 448 ��׼ת����������

   ����ֵ�����ޣ�
}

procedure CnEd448PointToData(P: TCnEccPoint; var Data: TCnEd448Data);
{* ��Ť�����»� 448 ��׼����Բ���ߵ�ת��Ϊѹ����ʽ�� 57 �ֽ����飬�� Y ֵ�� X ����ż��

   ������
     P: TCnEccPoint                       - ��ת���������
     var Data: TCnEd448Data               - ��Ť�����»� 448 ��׼ת�����ѹ����ʽ�� 57 �ֽ�������

   ����ֵ�����ޣ�
}

procedure CnEd448DataToPoint(Data: TCnEd448Data; P: TCnEccPoint; out XOdd: Boolean);
{* ��Ť�����»� 448 ��׼�� 57 �ֽ�����ת��Ϊ��Բ���ߵ�ѹ����ʽ��
   P �з��ض�Ӧ Y ֵ���Լ� XOdd �з��ض�Ӧ�� X ֵ�Ƿ�����������Ҫ������н� X

   ������
     Data: TCnEd448Data                   - ��ת���� 57 �ֽ�����
     P: TCnEccPoint                       - ��Ť�����»� 448 ��׼ת����������
     out XOdd: Boolean                    - ���ظ������� X ֵ�Ƿ�Ϊ����

   ����ֵ�����ޣ�
}

procedure CnEd448BigNumberToData(N: TCnBigNumber; var Data: TCnEd448Data);
{* ��Ť�����»� 448 ��׼������ת��Ϊ 57 �ֽ����飬����ת���Ƿ�ɹ�

   ������
     N: TCnBigNumber                      - ��ת���ĳ���
     var Data: TCnEd448Data               - ��Ť�����»� 448 ��׼ת����� 57 �ֽ�����

   ����ֵ�����ޣ�
}

procedure CnEd448DataToBigNumber(Data: TCnEd448Data; N: TCnBigNumber);
{* ��Ť�����»� 448 ��׼�� 57 �ֽ�����ת��Ϊ����������ת���Ƿ�ɹ�

   ������
     Data: TCnEd448Data                   - ��ת���� 57 �ֽ�����
     N: TCnBigNumber                      - ��Ť�����»� 448 ��׼ת����ĳ���

   ����ֵ�����ޣ�
}

procedure CnCurve448BigNumberToData(N: TCnBigNumber; var Data: TCnCurve448Data);
{* ���ɸ����� 448 ��׼������ת��Ϊ 56 �ֽ����飬����ת���Ƿ�ɹ�

   ������
     N: TCnBigNumber                      - ��ת���ĳ���
     var Data: TCnCurve448Data            - ���ɸ����� 448 ��׼ת����� 56 �ֽ�����

   ����ֵ�����ޣ�
}

procedure CnCurve448DataToBigNumber(Data: TCnCurve448Data; N: TCnBigNumber);
{* ���ɸ����� 448 ��׼�� 56 �ֽ�����ת��Ϊ����������ת���Ƿ�ɹ�

   ������
     Data: TCnCurve448Data                - ��ת���� 56 �ֽ�����
     N: TCnBigNumber                      - ���ɸ����� 448 ��׼ת����ĳ���

   ����ֵ�����ޣ�
}

procedure CnProcessCurve448ScalarNumber(Num: TCnBigNumber);
{* �� RFC �涨�����ɸ����� 448 ���������˽Կ

   ������
     Num: TCnBigNumber                    - ������� Curve448 �����

   ����ֵ�����ޣ�
}

procedure CnProcessEd448ScalarNumber(Num: TCnBigNumber);
{* �� RFC �涨����Ť�����»� 448 ���������˽Կ

   ������
     Num: TCnBigNumber                    - ������� Ed448 �����

   ����ֵ�����ޣ�
}

// ================ Curve448 ��Բ���� Diffie-Hellman ��Կ����  =================

function CnCurve448KeyExchangeStep1(SelfPrivateKey: TCnEccPrivateKey;
  OutPointToAnother: TCnEccPoint; Curve448: TCnCurve448 = nil): Boolean;
{* ���� Curve448 �� Diffie-Hellman ��Կ�����㷨��A �� B ���ȵ��ô˷�����
   ���ݸ���˽Կ��������㣬��������跢���Է������������Ƿ�ɹ���

   ������
     SelfPrivateKey: TCnEccPrivateKey     - ����������� Curve448 ˽Կ
     OutPointToAnother: TCnEccPoint       - ���ɵ�����㣬��������Է�
     Curve448: TCnCurve448                - Curve448 ʵ��

   ����ֵ��Boolean                        - ����������Ƿ����ɳɹ�
}

function CnCurve448KeyExchangeStep2(SelfPrivateKey: TCnEccPrivateKey;
  InPointFromAnother: TCnEccPoint; OutKey: TCnEccPoint; Curve448: TCnCurve448 = nil): Boolean;
{* ���� Curve448 �� Diffie-Hellman ��Կ�����㷨��A �� B �յ��Է��� Point ������ٵ��ô˷�����
   ���ݸ���˽Կ����һ��ͬ������㣬��������Ϊ������Կ������ͨ��������һ�����ӻ���
   ���������Ƿ�ɹ���

   ������
     SelfPrivateKey: TCnEccPrivateKey     - ����������� Curve448 ˽Կ
     InPointFromAnother: TCnEccPoint      - �ӵ�һ�����õ��ĶԷ����ɵ������
     OutKey: TCnEccPoint                  - ����Ĺ�����Կ
     Curve448: TCnCurve448                - Curve448 ʵ��

   ����ֵ��Boolean                        - ���ع�����Կ�Ƿ����ɳɹ�
}

// ===================== Ed448 ��Բ��������ǩ����֤�㷨 ======================

function CnEd448SignData(PlainData: Pointer; DataByteLen: Integer; PrivateKey: TCnEd448PrivateKey;
  PublicKey: TCnEd448PublicKey; OutSignature: TCnEd448Signature;
  const UserContext: TBytes = nil; Ed448: TCnEd448 = nil): Boolean;
{* Ed448 �ù�˽Կ�����ݿ����ǩ��������ǩ���Ƿ�ɹ���Ϊ������Ч������������б�֤��˽Կƥ�����ǩ����Ч��

   ������
     PlainData: Pointer                   - ��ǩ�������ݿ���ڴ��ַ
     DataByteLen: Integer                 - ��ǩ�������ݿ���ֽڳ���
     PrivateKey: TCnEd448PrivateKey       - Ed448 ˽Կ
     PublicKey: TCnEd448PublicKey         - Ed448 ��Կ
     OutSignature: TCnEd448Signature      - �����ǩ��ֵ
     const UserContext: TBytes            - �û���֮���ǩ������������
     Ed448: TCnEd448                      - Ed448 ʵ��

   ����ֵ��Boolean                        - ����ǩ���Ƿ�ɹ�
}

function CnEd448VerifyData(PlainData: Pointer; DataByteLen: Integer; InSignature: TCnEd448Signature;
  PublicKey: TCnEd448PublicKey; const UserContext: TBytes = nil; Ed448: TCnEd448 = nil): Boolean;
{* Ed448 �ù�Կ�����ݿ���ǩ��������֤��������֤�Ƿ�ɹ���

   ������
     PlainData: Pointer                   - ����֤�����ݿ���ڴ��ַ
     DataByteLen: Integer                 - ����֤�����ݿ���ֽڳ���
     InSignature: TCnEd448Signature       - ����֤��ǩ��ֵ
     PublicKey: TCnEd448PublicKey         - Ed448 ��Կ
     const UserContext: TBytes            - �û���֮���ǩ�����������ݣ�����ǩ��ʱ��ͬ
     Ed448: TCnEd448                      - Ed448 ʵ��

   ����ֵ��Boolean                        - ������֤ǩ���Ƿ�ɹ�
}

function CnEd448SignFile(const FileName: string; PrivateKey: TCnEd448PrivateKey;
  PublicKey: TCnEd448PublicKey; OutSignatureStream: TStream;
  const UserContext: TBytes = nil; Ed448: TCnEd448 = nil): Boolean;
{* �� Ed448 ��˽Կ���ļ�����ǩ����ǩ��ֵ 114 �ֽ�д�� OutSignatureStream �У�����ǩ���Ƿ�ɹ���

   ������
     const FileName: string               - ��ǩ�����ļ���
     PrivateKey: TCnEd448PrivateKey       - Ed448 ˽Կ
     PublicKey: TCnEd448PublicKey         - Ed448 ��Կ
     OutSignatureStream: TStream          - �����ǩ��������
     const UserContext: TBytes            - �û���֮���ǩ������������
     Ed448: TCnEd448                      - Ed448 ʵ��

   ����ֵ��Boolean                        - ����ǩ���Ƿ�ɹ�
}

function CnEd448VerifyFile(const FileName: string; InSignatureStream: TStream;
  PublicKey: TCnEd448PublicKey; const UserContext: TBytes = nil; Ed448: TCnEd448 = nil): Boolean;
{* �� Ed448 ��Կ���ļ���ǩ��������֤��InSignatureStream �ڲ����� 114 �ֽ�ǩ��ֵ��������֤�Ƿ�ɹ���

   ������
     const FileName: string               - ����֤���ļ���
     InSignatureStream: TStream           - ǩ��������
     PublicKey: TCnEd448PublicKey         - Ed448 ��Կ
     const UserContext: TBytes            - �û���֮���ǩ�����������ݣ�����ǩ��ʱ��ͬ
     Ed448: TCnEd448                      - Ed448 ʵ��

   ����ֵ��Boolean                        - ������֤ǩ���Ƿ�ɹ�
}

// ======================= Ed25519/448 ˽Կ������㺯�� ========================

procedure CnCalcKeysFromEd25519PrivateKey(InPrivateKey: TCnBigNumber;
  OutMulFactor: TCnBigNumber; OutHashPrefix: TCnBigNumber);
{* ���� Ed 25519 �����˽ԿҲ�� Secret Key ���ɹ�Կ�� Ed25519 ǩ��ʹ�õ��Ӵ�ǰ׺��

   ������
     InPrivateKey: TCnBigNumber           - ����� Ed25519 ˽Կ
     OutMulFactor: TCnBigNumber           - �����˽Կ����
     OutHashPrefix: TCnBigNumber          - ������Ӵ�����

   ����ֵ�����ޣ�
}

procedure CnCalcKeysFromEd448PrivateKey(InPrivateKey: TCnBigNumber;
  OutMulFactor: TCnBigNumber; OutHashPrefix: TCnBigNumber);
{* ���� Ed448 �����˽ԿҲ�� Secret Key ���ɹ�Կ�� Ed448 ǩ��ʹ�õ��Ӵ�ǰ׺��

   ������
     InPrivateKey: TCnBigNumber           - ����� Ed448 ˽Կ
     OutMulFactor: TCnBigNumber           - �����˽Կ����
     OutHashPrefix: TCnBigNumber          - ������Ӵ�����

   ����ֵ�����ޣ�
}

implementation

resourcestring
  SCnErrorPointInverse = 'Point Inverse Error.';
  SCnErrorCanNOTCalcFmt = 'Can NOT Calucate %s,%s + %s,%s';
  SCnErrorNumberTooBig = 'Number is Too Big.';
  SCnErrorPointNotOnCurve = 'Point NOT On Curve.';
  SCnErrorInvalidHexLength = 'Invalid Hex String Length.';

const

// ============================ 25519 ���߲��� =================================

  SCN_25519_PRIME = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED';
  // 2^255 - 19

  SCN_25519_COFACTOR = 8;
  // �����Ӿ�Ϊ 8��Ҳ���� 25519 ��Բ�����ܵ����� G ������İ˱�

  SCN_25519_ORDER = '1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED';
  // ���������Ϊ 2^252 + 27742317777372353535851937790883648493

  // 25519 Ť�����»����߲���
  SCN_25519_EDWARDS_A = '-01';
  // -1

  SCN_25519_EDWARDS_D = '52036CEE2B6FFE738CC740797779E89800700A4D4141D8AB75EB4DCA135978A3';
  // -121655/121656��Ҳ���� 121656 * D mod P = P - 121655 ��� D =
  // 37095705934669439343138083508754565189542113879843219016388785533085940283555

  SCN_25519_EDWARDS_GX = '216936D3CD6E53FEC0A4E231FDD6DC5C692CC7609525A7B2C9562D608F25D51A';
  // 15112221349535400772501151409588531511454012693041857206046113283949847762202

  SCN_25519_EDWARDS_GY = '6666666666666666666666666666666666666666666666666666666666666658';
  // 46316835694926478169428394003475163141307993866256225615783033603165251855960

  // 25519 �ɸ��������߲���
  SCN_25519_MONT_A = '076D06';
  // 486662

  SCN_25519_MONT_B = '01';
  // 1

  SCN_25519_MONT_GX = '09';
  // 9

  SCN_25519_MONT_GY = '20AE19A1B8A086B4E01EDD2C7748D14C923D4D7E6D7C61B229E9C5A27ECED3D9';
  // ���� RFC �е� y = 14781619447589544791020593568409986887264606134616475288964881837755586237401�����ƺ����� 4/5��Ҳ���� 5 * Y mod P = 4
  // ������ 5F51E65E475F794B1FE122D388B72EB36DC2B28192839E4DD6163A5D81312C14 �ŷ��� 4/5 ���Һ� Ed25519 �� GY ��Ӧ

  SCN_25519_SQRT_NEG_486664 = '0F26EDF460A006BBD27B08DC03FC4F7EC5A1D3D14B7D1A82CC6E04AAFF457E06';
  // ��ǰ��õ� sqrt(-486664)����������ת������

  SCN_LOW51_MASK = $7FFFFFFFFFFFF;

// =============================================================================
// �ɸ��������� By^2 = x^3 + Ax^2 + x ��Ť�����»����� au^2 + v^2 = 1 + du^2v^2
// �����еȼ۵�һһӳ���ϵ������ A = 2(a+d)/(a-d) ������֤�� �� B = 4 /(a-d)
// �� Curve25519 ������ Ed25519 �����־����˲���������B = 4 /(a-d) ������
// ͬ����(x, y) �� (u, v) �Ķ�Ӧ��ϵҲ��Ϊ A B a d ��ϵ�ĵ������������׼ӳ��
// =============================================================================

// ============================== 448 ���߲��� =================================

  SCN_448_PRIME = 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
  // 2^448 - 2^224 - 1

  SCN_448_COFACTOR = 4;
  // �����Ӿ�Ϊ 4��Ҳ���� 448 ��Բ�����ܵ����� G ��������ı�

  SCN_448_ORDER = '3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7CCA23E9C44EDB49AED63690216CC2728DC58F552378C292AB5844F3';
  // �������Ϊ 2^446 - 13818066809895115352007386748515426880336692474882178609894547503885

  // 448 Ť�����»����߲������� 1 �ƺ������Ť��
  SCN_448_EDWARDS_A = '01';
  // 1

  SCN_448_EDWARDS_D = '-98A9';
  // -39081 ��Ҫת��

  SCN_448_EDWARDS_GX = '4F1970C66BED0DED221D15A622BF36DA9E146570470F1767EA6DE324A3D3A46412AE1AF72AB66511433B80E18B00938E2626A82BC70CC05E';
  // RFC �е� 224580040295924300187604334099896036246789641632564134246125461686950415467406032909029192869357953282578032075146446173674602635247710

  SCN_448_EDWARDS_GY = '693F46716EB6BC248876203756C9C7624BEA73736CA3984087789C1E05A0C2D73AD3FF1CE67C39C4FDBD132C4ED7C8AD9808795BF230FA14';
  // RFC �е� 298819210078481492676017930443930673437544040154080242095928241372331506189835876003536878655418784733982303233503462500531545062832660

  // 448 �ɸ��������߲���
  SCN_448_MONT_A = '0262A6';
  // 156326

  SCN_448_MONT_B = '01';
  // 1

  SCN_448_MONT_GX = '05';
  // U 5

  SCN_448_MONT_GY = '7D235D1295F5B1F66C98AB6E58326FCECBAE5D34F55545D060F75DC28DF3F6EDB8027E2346430D211312C4B150677AF76FD7223D457B5B1A';
  // ���� RFC �е� V = 355293926785568175264127502063783334808976399387714271831880898435169088786967410002932673765864550910142774147268105838985595290606362

  SCN_448_SQRT_156324 = 'BA4D3A0829B6112F8812E51BA0BB2ABEBC1CB08EB48E556936BA50FDD2E7D68AF8CB32160522425B3F990812ABBE635AD37A21E17551B193';
  // ��ǰ��õ� sqrt(156324)����������ת������

// =============================================================================
// 448 �������������������� RFC 7448 �б����� P �������ڵ�ӳ���ϵ
//
//  (u, v) = (y^2/x^2, (2 - x^2 - y^2)*y/x^3)                 ����֤
//  (x, y) = (4*v*(u^2 - 1)/(u^4 - 2*u^2 + 4*v^2 + 1),        δ��֤�ɹ�
//            -(u^5 - 2*u^3 - 4*u*v^2 + u)/(u^5 - 2*u^2*v^2 - 2*u^3 - 2*v^2 + u))
//
// =============================================================================

type
  TCnSHAKE256Digest = array[0..CN_448_EDWARDS_BLOCK_BYTESIZE * 2 - 1] of Byte;

var
  FBigNumberPool: TCnBigNumberPool = nil;
  FPrime25519: TCnBigNumber = nil;
  FPrime448: TCnBigNumber = nil;
  FEd448SignPrefix: AnsiString = 'SigEd448';

  // ����
  F25519Field64Zero: TCn25519Field64 = (0, 0, 0, 0, 0);
  F25519Field64One: TCn25519Field64 = (1, 0, 0, 0, 0);
  F25519Field64NegOne: TCn25519Field64 = (2251799813685228, 2251799813685247, 2251799813685247, 2251799813685247, 2251799813685247);

procedure ConditionalSwapPoint(Swap: Boolean; A, B: TCnEccPoint);
begin
  if Swap then
  begin
    BigNumberSwap(A.X, B.X);
    BigNumberSwap(A.Y, B.Y);
  end;
end;

procedure ConditionalSwapField64Point(Swap: Boolean; var A, B: TCn25519Field64EccPoint);
begin
  if Swap then
  begin
    Cn25519Field64Swap(A.X, B.X);
    Cn25519Field64Swap(A.Y, B.Y);
  end;
end;

// �� RFC �涨���� 25519 ���������˽Կ
procedure CnProcess25519ScalarNumber(Num: TCnBigNumber);
begin
  Num.ClearBit(0);                                // ����λ�� 0
  Num.ClearBit(1);
  Num.ClearBit(2);
  Num.ClearBit(CN_25519_BLOCK_BYTESIZE * 8 - 1);  // ���λ�� 0
  Num.SetBit(CN_25519_BLOCK_BYTESIZE * 8 - 2);    // �θ�λ�� 1
end;

// =============================================================================
//
//          Curve448 �� u v �� Ed448 �� x y ��˫��ӳ���ϵΪ��
//
//          (u, v) = (y^2/x^2, (2 - x^2 - y^2)*y/x^3)
//          (x, y) = (4*v*(u^2 - 1)/(u^4 - 2*u^2 + 4*v^2 + 1),
//                    -(u^5 - 2*u^3 - 4*u*v^2 + u)/(u^5 - 2*u^2*v^2 - 2*u^3 - 2*v^2 + u))
//
// =============================================================================

procedure CnCurve448PointToEd448Point(DestPoint, SourcePoint: TCnEccPoint);
var
  T1, T2, T3, Prime, TX: TCnBigNumber;
begin
  // x = 4*v*(u^2 - 1)/(u^4 - 2*u^2 + 4*v^2 + 1)
  // y = -(u^5 - 2*u^3 - 4*u*v^2 + u)/(u^5 - 2*u^2*v^2 - 2*u^3 - 2*v^2 + u)

  T1 := nil;
  T2 := nil;
  T3 := nil;
  Prime := nil;
  TX := nil;

  try
    T1 := FBigNumberPool.Obtain;
    T2 := FBigNumberPool.Obtain;
    T3 := FBigNumberPool.Obtain;

    Prime := FBigNumberPool.Obtain;
    Prime.SetHex(SCN_448_PRIME);

    // ���� x �ķ�ĸ
    BigNumberPowerWordMod(T1, SourcePoint.X, 4, Prime);       // T1 �õ� u^4
    BigNumberPowerWordMod(T2, SourcePoint.X, 2, Prime);       // T2 �õ� u^2
    BigNumberAddMod(T2, T2, T2, Prime);                       // T2 �õ� 2*u^2
    BigNumberSubMod(T1, T1, T2, Prime);                       // T1 �õ� u^4 - 2*u^2 �ͷ� T2

    BigNumberAddMod(T2, SourcePoint.Y, SourcePoint.Y, Prime); // T2 �õ� 2*v
    BigNumberDirectMulMod(T2, T2, T2, Prime);                 // T2 �õ� 4*v^2
    BigNumberAddMod(T1, T1, T2, Prime);
    BigNumberAddWord(T1, 1);
    BigNumberMod(T1, T1, Prime);                              // T1 �õ���ĸ
    BigNumberModularInverse(T1, T1, Prime);                   // T1 �õ���ĸ�����ȴ��˷��Ӳ���ռ�� T1

    // ���� x �ķ���
    BigNumberDirectMulMod(T2, SourcePoint.X, SourcePoint.X, Prime);
    BigNumberSubWord(T2, 1);                                  // T2 �õ� u^2 - 1
    BigNumberDirectMulMod(T2, T2, SourcePoint.Y, Prime);      // T2 �õ� v*(u^2 - 1)
    BigNumberAddMod(T2, T2, T2, Prime);
    BigNumberAddMod(T2, T2, T2, Prime);                       // ������ T2 �õ� 4*v*(u^2 - 1)

    TX := FBigNumberPool.Obtain;
    BigNumberDirectMulMod(TX, T2, T1, Prime);                 // �õ� X �ݴ沢�ͷ� T1 �� T2

    // ��ʼ�� y �ķ�ĸ
    BigNumberPowerWordMod(T1, SourcePoint.X, 5, Prime);       // T1 �õ� u^5

    BigNumberDirectMulMod(T2, SourcePoint.X, SourcePoint.Y, Prime);
    BigNumberDirectMulMod(T2, T2, T2, Prime);                 // T2 �õ� u^2*v^2
    BigNumberAddMod(T2, T2, T2, Prime);
    BigNumberSubMod(T1, T1, T2, Prime);                       // T1 �õ� u^5 - 2*u^2*v^2 ���ͷ� T2

    BigNumberPowerWordMod(T2, SourcePoint.X, 3, Prime);
    BigNumberAddMod(T2, T2, T2, Prime);                       // T2 �õ� 2*u^3
    BigNumberSubMod(T1, T1, T2, Prime);                       // T1 �õ� u^5 - 2*u^2*v^2 - 2*u^3 ���ͷ� T2

    BigNumberPowerWordMod(T2, SourcePoint.Y, 2, Prime);
    BigNumberAddMod(T2, T2, T2, Prime);                       // T2 �õ� 2*v^2

    BigNumberSubMod(T1, T1, T2, Prime);                       // T1 �õ� u^5 - 2*u^2*v^2 - 2*u^3 - 2v^2 ���ͷ� T2
    BigNumberAddMod(T1, T1, SourcePoint.X, Prime);            // ���� u �� T1 �õ���ĸ
    BigNumberModularInverse(T1, T1, Prime);                   // T1 �õ���ĸ�����ȴ��˷��Ӳ���ռ�� T1

    // ���� y �ķ��� -(u^5 - 2*u^3 - 4*u*v^2 + u)
    BigNumberPowerWordMod(T2, SourcePoint.X, 5, Prime);       // T2 �õ� u^5
    BigNumberPowerWordMod(T3, SourcePoint.X, 3, Prime);
    BigNumberAddMod(T3, T3, T3, Prime);                       // T3 �õ� 2*u^3
    BigNumberSubMod(T2, T2, T3, Prime);                       // T2 �õ� u^5 - 2*u^3 ���ͷ� T3

    BigNumberDirectMulMod(T3, SourcePoint.Y, SourcePoint.Y, Prime);
    BigNumberDirectMulMod(T3, SourcePoint.X, T3, Prime);
    BigNumberAddMod(T3, T3, T3, Prime);
    BigNumberAddMod(T3, T3, T3, Prime);                       // T3 �õ� 4*u*v^2
    BigNumberSubMod(T2, T2, T3, Prime);                       // T2 �õ� u^5 - 2*u^3 - 4*u*v^2 ���ͷ� T3
    BigNumberAddMod(T2, T2, SourcePoint.X, Prime);            // T2 �õ� u^5 - 2*u^3 - 4*u*v^2 + u

    BigNumberSubMod(T2, CnBigNumberZero, T2, Prime);          // �󸺺� T2 �õ���������
    BigNumberDirectMulMod(DestPoint.Y, T2, T1, Prime);        // �� T1 �����ĸ������˵õ� Y

    BigNumberCopy(DestPoint.X, TX);                           // ������ X
  finally
    FBigNumberPool.Recycle(TX);
    FBigNumberPool.Recycle(Prime);
    FBigNumberPool.Recycle(T3);
    FBigNumberPool.Recycle(T2);
    FBigNumberPool.Recycle(T1);
  end;
end;

procedure CnEd448PointToCurve448Point(DestPoint, SourcePoint: TCnEccPoint);
var
  T1, T2, Prime, TX: TCnBigNumber;
begin
  // u = y^2/x^2
  // v = (2 - x^2 - y^2)*y/x^3

  T1 := nil;
  T2 := nil;
  TX := nil;
  Prime := nil;

  try
    T1 := FBigNumberPool.Obtain;
    T2 := FBigNumberPool.Obtain;
    Prime := FBigNumberPool.Obtain;
    Prime.SetHex(SCN_448_PRIME);

    BigNumberDirectMulMod(T1, SourcePoint.X, SourcePoint.X, Prime);
    BigNumberPrimeModularInverse(T1, T1, Prime);            // T1 �õ� 1 / x^2
    BigNumberDirectMulMod(T2, SourcePoint.Y, SourcePoint.Y, Prime);

    TX := FBigNumberPool.Obtain;
    BigNumberDirectMulMod(TX, T1, T2, Prime);               // U �õ� y^2 / x^2 ���ݴ����������ͬһ����Ӱ��

    BigNumberDirectMulMod(T1, SourcePoint.X, SourcePoint.X, Prime);
    BigNumberDirectMulMod(T2, SourcePoint.Y, SourcePoint.Y, Prime);
    BigNumberAddMod(T2, T1, T2, Prime);                      // T2 �õ� x^2 + y^2 ���ͷ� T1
    BigNumberSubWord(T2, 2);                                 // T2 �õ� x^2 + y^2 - 2
    BigNumberSubMod(T2, CnBigNumberZero, T2, Prime);         // 0 - T2 �õ� 2 - x^2 - y^2
    BigNumberDirectMulMod(T2, T2, SourcePoint.Y, Prime);     // T2 �õ���ĸ (2 - x^2 - y^2)*y

    BigNumberDirectMulMod(T1, SourcePoint.X, SourcePoint.X, Prime);
    BigNumberDirectMulMod(T1, T1, SourcePoint.X, Prime);
    BigNumberPrimeModularInverse(T1, T1, Prime);             // T1 �õ� 1 / x^3

    BigNumberDirectMulMod(DestPoint.Y, T1, T2, Prime);       // V �õ� (2 - x^2 - y^2)*y/x^3
    BigNumberCopy(DestPoint.X, TX);                          // ���� U
  finally
    FBigNumberPool.Recycle(Prime);
    FBigNumberPool.Recycle(TX);
    FBigNumberPool.Recycle(T2);
    FBigNumberPool.Recycle(T1);
  end;
end;

procedure CnCurve448PointToData(P: TCnEccPoint; var Data: TCnCurve448Data);
begin
  if P = nil then
    Exit;

  FillChar(Data[0], SizeOf(TCnCurve448Data), 0);
  P.X.ToBinary(@Data[0], SizeOf(TCnCurve448Data));
  ReverseMemory(@Data[0], SizeOf(TCnCurve448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��
end;

procedure CnCurve448DataToPoint(Data: TCnCurve448Data; P: TCnEccPoint);
var
  D: TCnCurve448Data;
begin
  if P = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnCurve448Data));
  ReverseMemory(@D[0], SizeOf(TCnCurve448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  P.X.SetBinary(@D[0], SizeOf(TCnCurve448Data));
end;

procedure CnEd448PointToData(P: TCnEccPoint; var Data: TCnEd448Data);
begin
  if P = nil then
    Exit;

  FillChar(Data[0], SizeOf(TCnEd448Data), 0);
  P.Y.ToBinary(@Data[0], SizeOf(TCnEd448Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  if P.X.IsOdd then // X �����������λ�� 1
    Data[CN_448_EDWARDS_BLOCK_BYTESIZE - 1] := Data[CN_448_EDWARDS_BLOCK_BYTESIZE - 1] or $80  // ��λ�� 1
  else
    Data[CN_448_EDWARDS_BLOCK_BYTESIZE - 1] := Data[CN_448_EDWARDS_BLOCK_BYTESIZE - 1] and $7F; // ��λ�� 0
end;

procedure CnEd448DataToPoint(Data: TCnEd448Data; P: TCnEccPoint; out XOdd: Boolean);
var
  D: TCnEd448Data;
begin
  if P = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd448Data));
  ReverseMemory(@D[0], SizeOf(TCnEd448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  P.Y.SetBinary(@D[0], SizeOf(TCnEd448Data));

  // ���λ�Ƿ��� 0 ��ʾ�� X ����ż
  XOdd := P.Y.IsBitSet(8 * CN_448_EDWARDS_BLOCK_BYTESIZE - 1);

  // ���λ������
  P.Y.ClearBit(8 * CN_448_EDWARDS_BLOCK_BYTESIZE - 1);
end;

procedure CnEd448BigNumberToData(N: TCnBigNumber; var Data: TCnEd448Data);
begin
  if N = nil then
    Exit;

  if N.GetBytesCount > SizeOf(TCnEd448Data) then
    raise ECnEccException.Create(SCnErrorNumberTooBig);

  FillChar(Data[0], SizeOf(TCnEd448Data), 0);
  N.ToBinary(@Data[0], SizeOf(TCnEd448Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��
end;

procedure CnEd448DataToBigNumber(Data: TCnEd448Data; N: TCnBigNumber);
var
  D: TCnEd448Data;
begin
  if N = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd448Data));
  ReverseMemory(@D[0], SizeOf(TCnEd448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  N.SetBinary(@D[0], SizeOf(TCnEd448Data));
end;

procedure CnCurve448BigNumberToData(N: TCnBigNumber; var Data: TCnCurve448Data);
begin
  if N = nil then
    Exit;

  if N.GetBytesCount > SizeOf(TCnCurve448Data) then
    raise ECnEccException.Create(SCnErrorNumberTooBig);

  FillChar(Data[0], SizeOf(TCnCurve448Data), 0);
  N.ToBinary(@Data[0], SizeOf(TCnCurve448Data));
  ReverseMemory(@Data[0], SizeOf(TCnCurve448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��
end;

procedure CnCurve448DataToBigNumber(Data: TCnCurve448Data; N: TCnBigNumber);
var
  D: TCnCurve448Data;
begin
  if N = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnCurve448Data));
  ReverseMemory(@D[0], SizeOf(TCnCurve448Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  N.SetBinary(@D[0], SizeOf(TCnCurve448Data));
end;

// �� RFC �涨���� Curve448 ���������˽Կ
procedure CnProcessCurve448ScalarNumber(Num: TCnBigNumber);
begin
  Num.ClearBit(0);                                    // �Ͷ�λ�� 0
  Num.ClearBit(1);

  Num.SetBit(CN_448_CURVE_BLOCK_BYTESIZE * 8 - 1);    // ���λ�� 1
end;

// �� RFC �涨���� Ed448 ���������˽Կ
procedure CnProcessEd448ScalarNumber(Num: TCnBigNumber);
begin
  Num.ClearBit(0);                                    // �Ͷ�λ�� 0
  Num.ClearBit(1);

  Num.SetBit((CN_448_EDWARDS_BLOCK_BYTESIZE - 1) * 8 - 1);     // ���ֽڵ����λ�� 1
  BigNumberKeepLowBits(Num, (CN_448_EDWARDS_BLOCK_BYTESIZE - 1) * 8);   // ����ֽ��� 0
end;

function CnEd448SignData(PlainData: Pointer; DataByteLen: Integer; PrivateKey: TCnEd448PrivateKey;
  PublicKey: TCnEd448PublicKey; OutSignature: TCnEd448Signature;
  const UserContext: TBytes; Ed448: TCnEd448): Boolean;
var
  Is448Nil: Boolean;
  Stream: TMemoryStream;
  R, S, K, HP: TCnBigNumber;
  Dig: TCnSHAKE256Digest;
  Data: TCnEd448Data;
  E: Byte;
  D: TBytes;
begin
  Result := False;
  if (PlainData = nil) or (DataByteLen <= 0) or (PrivateKey = nil) or (PublicKey = nil)
    or (OutSignature = nil) then
    Exit;

  R := nil;
  S := nil;
  K := nil;
  HP := nil;
  Stream := nil;
  Is448Nil := Ed448 = nil;

  try
    if Is448Nil then
      Ed448 := TCnEd448.Create;

    R := FBigNumberPool.Obtain;
    S := FBigNumberPool.Obtain;
    K := FBigNumberPool.Obtain;
    HP := FBigNumberPool.Obtain;

    // ����˽Կ�õ�˽Կ���� s ���Ӵ�ǰ׺
    CnCalcKeysFromEd448PrivateKey(PrivateKey, S, HP);

    // SHAKE256(dom4(F, C) || HashPrefix || M, 114) ���� F �� 0��C �� � 255 �ַ������û���֮���
    // ע�� RFC 8032 �е� dom4(F, C) = "SigEd448" || octet(F) || octet(OLEN(C)) || C

    // �Ӵ�ǰ׺ƴ��ԭʼ����
    E := 0;
    Stream := TMemoryStream.Create;
    Stream.Write(FEd448SignPrefix[1], Length(FEd448SignPrefix));
    Stream.Write(E, 1);
    E := Length(UserContext);
    Stream.Write(E, 1);
    if E > 0 then
      Stream.Write(UserContext[0], E);   // "SigEd448" || octet(F) || octet(OLEN(C)) || C

    BigNumberWriteBinaryToStream(HP, Stream, CN_448_EDWARDS_BLOCK_BYTESIZE);
    Stream.Write(PlainData^, DataByteLen);

    // ����� 114 �ֽڵ� SHAKE256 ֵ��Ϊ r ������׼�����Ի�����Ϊ R ��
    D := SHAKE256Buffer(Stream.Memory, Stream.Size, SizeOf(TCnSHAKE256Digest));
    if Length(D) <> SizeOf(TCnSHAKE256Digest) then
      Exit;

    Move(D[0], Dig[0], SizeOf(TCnSHAKE256Digest));
    ReverseMemory(@Dig[0], SizeOf(TCnSHAKE256Digest));
    // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

    R.SetBinary(@Dig[0], SizeOf(TCnSHAKE256Digest));
    BigNumberNonNegativeMod(R, R, Ed448.Order);  // �� r ����ʵ����̫���� mod һ�½�

    OutSignature.R.Assign(Ed448.Generator);
    Ed448.MultiplePoint(R, OutSignature.R);      // ����õ�ǩ��ֵ R����ֵ��һ��������

    // SHAKE256("SigEd448" || octet(F) || octet(OLEN(C)) || C || R || PublicKey || M, 114)
    // �� Hash ���� S���ȵ� R ת��Ϊ�ֽ�����
    Ed448.PointToPlain(OutSignature.R, Data);

    // ƴ����
    Stream.Clear;
    E := 0;
    Stream.Write(FEd448SignPrefix[1], Length(FEd448SignPrefix));
    Stream.Write(E, 1);
    E := Length(UserContext);
    Stream.Write(E, 1);
    if E > 0 then
      Stream.Write(UserContext[0], E);   // "SigEd448" || octet(F) || octet(OLEN(C)) || C

    Stream.Write(Data[0], SizeOf(TCnEd448Data));

    // ��Կ��Ҳת��Ϊ�ֽ�����
    Ed448.PointToPlain(PublicKey, Data);
    Stream.Write(Data[0], SizeOf(TCnEd448Data));

    // д���ģ�ƴ�����
    Stream.Write(PlainData^, DataByteLen);

    // �ٴ��Ӵ�
    D := SHAKE256Buffer(Stream.Memory, Stream.Size, SizeOf(TCnSHAKE256Digest));
    if Length(D) <> SizeOf(TCnSHAKE256Digest) then
      Exit;

    Move(D[0], Dig[0], SizeOf(TCnSHAKE256Digest));
    ReverseMemory(@Dig[0], SizeOf(TCnSHAKE256Digest));
    // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ���������Ҫ��һ��

    K.SetBinary(@Dig[0], SizeOf(TCnSHAKE256Digest));
    BigNumberNonNegativeMod(K, K, Ed448.Order);  // ����̫������ mod һ�½�

    // ������� R + K * S mod Order
    BigNumberDirectMulMod(OutSignature.S, K, S, Ed448.Order);
    BigNumberAddMod(OutSignature.S, R, OutSignature.S, Ed448.Order);

    Result := True;
  finally
    Stream.Free;
    FBigNumberPool.Recycle(HP);
    FBigNumberPool.Recycle(K);
    FBigNumberPool.Recycle(S);
    FBigNumberPool.Recycle(R);
    if Is448Nil then
      Ed448.Free;
  end;
end;

function CnEd448VerifyData(PlainData: Pointer; DataByteLen: Integer; InSignature: TCnEd448Signature;
  PublicKey: TCnEd448PublicKey; const UserContext: TBytes; Ed448: TCnEd448): Boolean;
var
  Is448Nil: Boolean;
  L, R, M: TCnEccPoint;
  T: TCnBigNumber;
  Stream: TMemoryStream;
  Data: TCnEd448Data;
  Dig: TCnSHAKE256Digest;
  D: TBytes;
  E: Byte;
begin
  Result := False;
  if (PlainData = nil) or (DataByteLen <= 0) or (PublicKey = nil) or (InSignature = nil) then
    Exit;

  L := nil;
  R := nil;
  Stream := nil;
  T := nil;
  M := nil;
  Is448Nil := Ed448 = nil;

  try
    if Is448Nil then
      Ed448 := TCnEd448.Create;

    // ��֤ 4*S*���� �Ƿ� = 4*R�� + 4*Hash(R57λ||��Կ��57λ||����) * ��Կ��
    L := TCnEccPoint.Create;
    R := TCnEccPoint.Create;

    L.Assign(Ed448.Generator);
    Ed448.MultiplePoint(InSignature.S, L);
    Ed448.MultiplePoint(4, L);  // �㵽��ߵ�

    R.Assign(InSignature.R);
    Ed448.MultiplePoint(4, R);  // �㵽 4*R �����

    Stream := TMemoryStream.Create;
    // SHAKE256("SigEd448" || octet(F) || octet(OLEN(C)) || C || R || A || M, 114)
    E := 0;
    Stream.Write(FEd448SignPrefix[1], Length(FEd448SignPrefix));
    Stream.Write(E, 1);
    E := Length(UserContext);
    Stream.Write(E, 1);
    if E > 0 then
      Stream.Write(UserContext[0], E);   // "SigEd448" || octet(F) || octet(OLEN(C)) || C

    CnEd448PointToData(InSignature.R, Data);
    Stream.Write(Data[0], SizeOf(TCnEd448Data));        // ƴ R ��

    CnEd448PointToData(PublicKey, Data);
    Stream.Write(Data[0], SizeOf(TCnEd448Data));        // ƴ��Կ�� A
    Stream.Write(PlainData^, DataByteLen);              // ƴ����

    D := SHAKE256Buffer(Stream.Memory, Stream.Size, SizeOf(TCnSHAKE256Digest));
    if Length(D) <> SizeOf(TCnSHAKE256Digest) then      // ���� Hash ��Ϊ k 'ֵ
      Exit;

    Move(D[0], Dig[0], SizeOf(TCnSHAKE256Digest));
    ReverseMemory(@Dig[0], SizeOf(TCnSHAKE256Digest));      // ��Ҫ��תһ��

    T := FBigNumberPool.Obtain;                             // T �� RFC �е� k'
    T.SetBinary(@Dig[0], SizeOf(TCnSHAKE256Digest));
    T.MulWord(4);
    BigNumberNonNegativeMod(T, T, Ed448.Order);             // T ����̫���� mod һ�½�

    M := TCnEccPoint.Create;
    M.Assign(PublicKey);
    Ed448.MultiplePoint(T, M);      // T �˹�Կ��
    Ed448.PointAddPoint(R, M, R);   // ���

    Result := CnEccPointsEqual(L, R);
  finally
    M.Free;
    FBigNumberPool.Recycle(T);
    Stream.Free;
    R.Free;
    L.Free;
    if Is448Nil then
      Ed448.Free;
  end;
end;

function CnEd448SignFile(const FileName: string; PrivateKey: TCnEd448PrivateKey;
  PublicKey: TCnEd448PublicKey; OutSignatureStream: TStream;
  const UserContext: TBytes; Ed448: TCnEd448): Boolean;
var
  Stream: TMemoryStream;
  Sig: TCnEd448Signature;
  SigData: TCnEd448SignatureData;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) or (OutSignatureStream = nil)
    or not FileExists(FileName) then
    Exit;

  Stream := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    Sig := TCnEd448Signature.Create;

    if CnEd448SignData(Stream.Memory, Stream.Size, PrivateKey, PublicKey, Sig, UserContext, Ed448) then
    begin
      Sig.SaveToData(SigData);
      Result := OutSignatureStream.Write(SigData[0], SizeOf(TCnEd448SignatureData))
        = SizeOf(TCnEd448SignatureData);
    end;
  finally
    Sig.Free;
    Stream.Free;
  end;
end;

function CnEd448VerifyFile(const FileName: string; InSignatureStream: TStream;
  PublicKey: TCnEd448PublicKey; const UserContext: TBytes; Ed448: TCnEd448): Boolean;
var
  Stream: TMemoryStream;
  Sig: TCnEd448Signature;
  SigData: TCnEd448SignatureData;
begin
  Result := False;
  if (PublicKey = nil) or (InSignatureStream = nil) or not FileExists(FileName) then
    Exit;

  Stream := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    if InSignatureStream.Read(SigData[0], SizeOf(TCnEd448SignatureData)) <>
      SizeOf(TCnEd448SignatureData) then
      Exit;

    Sig := TCnEd448Signature.Create;
    Sig.LoadFromData(SigData);

    Result := CnEd448VerifyData(Stream.Memory, Stream.Size, Sig, PublicKey, UserContext, Ed448);
  finally
    Sig.Free;
    Stream.Free;
  end;
end;

function CnCurve448KeyExchangeStep1(SelfPrivateKey: TCnEccPrivateKey;
  OutPointToAnother: TCnEccPoint; Curve448: TCnCurve448): Boolean;
var
  Is448Nil: Boolean;
begin
  Result := False;
  if (SelfPrivateKey = nil) or (OutPointToAnother = nil) then
    Exit;

  Is448Nil := Curve448 = nil;

  try
    if Is448Nil then
      Curve448 := TCnCurve448.Create;

    OutPointToAnother.Assign(Curve448.Generator);
    Curve448.MultiplePoint(SelfPrivateKey, OutPointToAnother);

    Result := True;
  finally
    if Is448Nil then
      Curve448.Free;
  end;
end;

function CnCurve448KeyExchangeStep2(SelfPrivateKey: TCnEccPrivateKey;
  InPointFromAnother: TCnEccPoint; OutKey: TCnEccPoint; Curve448: TCnCurve448): Boolean;
var
  Is448Nil: Boolean;
begin
  Result := False;
  if (SelfPrivateKey = nil) or (InPointFromAnother = nil) or (OutKey = nil) then
    Exit;

  Is448Nil := Curve448 = nil;

  try
    if Is448Nil then
      Curve448 := TCnCurve448.Create;

    OutKey.Assign(InPointFromAnother);
    Curve448.MultiplePoint(SelfPrivateKey, OutKey);

    Result := True;
  finally
    if Is448Nil then
      Curve448.Free;
  end;
end;

// ��������� SHA512 ��������� 64 �ֽڣ�ע�� WriteBinaryToStream �ڲ��������ֽ�˳��
// �� RFC Ҫ����С�˷�ʽ����Ҳ�����Լ���֮ǰ�� Data ��ʽ���㣬�������ǰ��Ҫ��һ��
function CalcBigNumberSHA512Digest(const Num: TCnBigNumber; FixedLen: Integer): TCnSHA512Digest;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    FillChar(Result[0], SizeOf(TCnSHA512Digest), 0);
    if BigNumberWriteBinaryToStream(Num, Stream, FixedLen) <> FixedLen then
      Exit;

    ReverseMemory(Stream.Memory, Stream.Size);
    Result := SHA512Stream(Stream);
  finally
    Stream.Free;
  end;
end;

// ��������� SHAKE256 ���������ȡ 114 �ֽڣ�ע�� WriteBinaryToStream �ڲ��������ֽ�˳��
// �� RFC Ҫ����С�˷�ʽ����Ҳ�����Լ���֮ǰ�� Data ��ʽ���㣬�������ǰ��Ҫ��һ��
function CalcBigNumberSHAKE256Digest(const Num: TCnBigNumber; FixedLen: Integer): TCnSHAKE256Digest;
var
  Stream: TMemoryStream;
  D: TBytes;
begin
  Stream := TMemoryStream.Create;
  try
    FillChar(Result[0], SizeOf(TCnSHAKE256Digest), 0);
    if BigNumberWriteBinaryToStream(Num, Stream, FixedLen) <> FixedLen then
      Exit;
  
    ReverseMemory(Stream.Memory, Stream.Size);
    D := SHAKE256Stream(Stream, SizeOf(TCnSHAKE256Digest));
    if Length(D) = SizeOf(TCnSHAKE256Digest) then
      Move(D[0], Result[0], SizeOf(TCnSHAKE256Digest));
  finally
    Stream.Free;
  end;
end;

// �������˽Կ�����ɹ�Կ�� Ed25519 ǩ��ʹ�õ� Hash ����
procedure CnCalcKeysFromEd25519PrivateKey(InPrivateKey: TCnBigNumber;
  OutMulFactor, OutHashPrefix: TCnBigNumber);
var
  Dig: TCnSHA512Digest;
  Data: TCnEd25519Data;
begin
  // �� PrivateKey �� SHA512���õ� 64 �ֽڽ�� Dig
  Dig := CalcBigNumberSHA512Digest(InPrivateKey, CN_25519_BLOCK_BYTESIZE);

  // ������ SHA512���õ� 64 �ֽڽ����ǰ 32 �ֽ�ȡ�����������ȵ����ɴ������� 3 λ�����㣬
  // ���� CoFactor �� 2^3 = 8 ��Ӧ���������λ 2^255 ���� 0���θ�λ 2^254 ���� 1
  if OutMulFactor <> nil then
  begin
    Move(Dig[0], Data[0], CN_25519_BLOCK_BYTESIZE);
    CnEd25519DataToBigNumber(Data, OutMulFactor);          // ǰ 32 �ֽ��ڲ�������

    CnProcess25519ScalarNumber(OutMulFactor);
  end;

  // �� 32 �ֽ���Ϊ Hash ����ڲ���������δ���� Hash �ⲻ�������������Բ��õ���
  if OutHashPrefix <> nil then
    OutHashPrefix.SetBinary(@Dig[CN_25519_BLOCK_BYTESIZE], CN_25519_BLOCK_BYTESIZE);
end;

// �������˽Կ�����ɹ�Կ�� Ed448 ǩ��ʹ�õ� Hash ����
procedure CnCalcKeysFromEd448PrivateKey(InPrivateKey: TCnBigNumber;
  OutMulFactor, OutHashPrefix: TCnBigNumber);
var
  Dig: TCnSHAKE256Digest;
  Data: TCnEd448Data;
begin
  // �� PrivateKey �� SHAKE256���õ� 114 �ֽڽ�� Dig
  Dig := CalcBigNumberSHAKE256Digest(InPrivateKey, CN_448_EDWARDS_BLOCK_BYTESIZE);

  // ������ SHAKE256���õ� 114 �ֽڽ����ǰ 57 �ֽ�ȡ�����������ȵ��򣬵� 2 λ�����㣬
  // ���� CoFactor �� 2^2 = 4 ��Ӧ���������λ�ֽڵ�ȫ���� 0���θ�λ�ֽڵ����λ���� 1
  if OutMulFactor <> nil then
  begin
    Move(Dig[0], Data[0], CN_448_EDWARDS_BLOCK_BYTESIZE);
    CnEd448DataToBigNumber(Data, OutMulFactor);          // ǰ 57 �ֽ��ڲ�������

    CnProcessEd448ScalarNumber(OutMulFactor);
  end;

  // �� 57 �ֽ���Ϊ Hash ����ڲ���������δ���� Hash �ⲻ�������������Բ��õ���
  if OutHashPrefix <> nil then
    OutHashPrefix.SetBinary(@Dig[CN_448_EDWARDS_BLOCK_BYTESIZE], CN_448_EDWARDS_BLOCK_BYTESIZE);
end;

{ TCnTwistedEdwardsCurve }

constructor TCnTwistedEdwardsCurve.Create(const A: AnsiString; const D: AnsiString;
  const FieldPrime: AnsiString; const GX: AnsiString; const GY: AnsiString;
  const Order: AnsiString; H: Integer);
begin
  Create;
  Load(A, D, FieldPrime, GX, GY, Order, H);
end;

constructor TCnTwistedEdwardsCurve.Create;
begin
  inherited;
  FCoefficientA := TCnBigNumber.Create;
  FCoefficientD := TCnBigNumber.Create;
  FOrder := TCnBigNumber.Create;
  FFiniteFieldSize := TCnBigNumber.Create;
  FGenerator := TCnEccPoint.Create;
  FCoFactor := 1;
end;

destructor TCnTwistedEdwardsCurve.Destroy;
begin
  FGenerator.Free;
  FFiniteFieldSize.Free;
  FOrder.Free;
  FCoefficientD.Free;
  FCoefficientA.Free;
  inherited;
end;

function TCnTwistedEdwardsCurve.IsNeutualPoint(P: TCnEccPoint): Boolean;
begin
  Result := P.X.IsZero and P.Y.IsOne;
end;

function TCnTwistedEdwardsCurve.IsPointOnCurve(P: TCnEccPoint): Boolean;
var
  X, Y, L, R: TCnBigNumber;
begin
  // �ж� au^2 + v^2 �Ƿ���� 1 + du^2v^2������ U �� X ���棬V �� Y ����

  X := nil;
  Y := nil;
  L := nil;
  R := nil;

  try
    X := FBigNumberPool.Obtain;
    BigNumberCopy(X, P.X);
    BigNumberDirectMulMod(X, X, X, FFiniteFieldSize);

    Y := FBigNumberPool.Obtain;
    BigNumberCopy(Y, P.Y);
    BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize);

    L := FBigNumberPool.Obtain;
    BigNumberDirectMulMod(L, FCoefficientA, X, FFiniteFieldSize);
    BigNumberAddMod(L, L, Y, FFiniteFieldSize); // ��ʱ L := A * X^2 + Y^2

    R := FBigNumberPool.Obtain;
    BigNumberDirectMulMod(R, X, Y, FFiniteFieldSize);
    BigNumberDirectMulMod(R, FCoefficientD, R, FFiniteFieldSize);
    R.AddWord(1); // ��ʱ R := 1 + D * X^2 * Y^2

    Result := BigNumberEqual(L, R);
  finally
    FBigNumberPool.Recycle(R);
    FBigNumberPool.Recycle(L);
    FBigNumberPool.Recycle(Y);
    FBigNumberPool.Recycle(X);
  end;
end;

procedure TCnTwistedEdwardsCurve.Load(const A: AnsiString; const D: AnsiString;
  const FieldPrime: AnsiString; const GX: AnsiString; const GY: AnsiString;
  const Order: AnsiString; H: Integer);
begin
  FCoefficientA.SetHex(A);
  FCoefficientD.SetHex(D);
  FFiniteFieldSize.SetHex(FieldPrime);
  FGenerator.X.SetHex(GX);
  FGenerator.Y.SetHex(GY);
  FOrder.SetHex(Order);
  FCoFactor := H;
end;

procedure TCnTwistedEdwardsCurve.MultiplePoint(K: Int64; P: TCnEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := FBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MultiplePoint(BK, P);
  finally
    FBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnTwistedEdwardsCurve.MultiplePoint(K: TCnBigNumber; P: TCnEccPoint);
var
  I: Integer;
  E, R: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    PointInverse(P);
  end;

  if BigNumberIsZero(K) then
  begin
    SetNeutualPoint(P);
    Exit;
  end
  else if BigNumberIsOne(K) then // �� 1 ���趯
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEccPoint.Create;
    E := TCnEccPoint.Create;

    SetNeutualPoint(R); // R ������ʱĬ��Ϊ (0, 0)�����˴�����Ϊ���Ե� (0, 1)
    E.X := P.X;
    E.Y := P.Y;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        PointAddPoint(R, E, R);
      PointAddPoint(E, E, E);
    end;

    P.X := R.X;
    P.Y := R.Y;
  finally
    E.Free;
    R.Free;
  end;
end;

procedure TCnTwistedEdwardsCurve.PointAddPoint(P: TCnEccPoint; Q: TCnEccPoint;
  Sum: TCnEccPoint);
var
  X, Y, T, D1, D2, N1, N2: TCnBigNumber;
begin
//            x1 * y2 + x2 * y1                 y1 * y2 - a * x1 * x2
//   x3 = --------------------------,   y3 = ---------------------------  �������迼�� P/Q �Ƿ�ͬһ��
//         1 + d * x1 * x2 * y1 * y2          1 - d * x1 * x2 * y1 * y2

  X := nil;
  Y := nil;
  T := nil;
  D1 := nil;
  D2 := nil;
  N1 := nil;
  N2 := nil;

  try
    X := FBigNumberPool.Obtain;
    Y := FBigNumberPool.Obtain;
    T := FBigNumberPool.Obtain;
    D1 := FBigNumberPool.Obtain;
    D2 := FBigNumberPool.Obtain;
    N1 := FBigNumberPool.Obtain;
    N2 := FBigNumberPool.Obtain;

    BigNumberDirectMulMod(T, P.X, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(N1, Q.X, P.Y, FFiniteFieldSize);
    BigNumberAddMod(N1, N1, T, FFiniteFieldSize); // N1 �õ� x1 * y2 + x2 * y1���ͷ� T

    BigNumberDirectMulMod(T, P.X, Q.X, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientA, FFiniteFieldSize);
    BigNumberDirectMulMod(N2, P.Y, Q.Y, FFiniteFieldSize);
    BigNumberSubMod(N2, N2, T, FFiniteFieldSize); // N2 �õ� y1 * y2 - a * x1 * x2���ͷ� T

    BigNumberDirectMulMod(T, P.Y, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, Q.X, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, P.X, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientD, FFiniteFieldSize); // T �õ� d * x1 * x2 * y1 * y2

    BigNumberAddMod(D1, T, CnBigNumberOne, FFiniteFieldSize); // D1 �õ� 1 + d * x1 * x2 * y1 * y2
    BigNumberSubMod(D2, CnBigNumberOne, T, FFiniteFieldSize); // D2 �õ� 1 - d * x1 * x2 * y1 * y2

    BigNumberModularInverse(T, D1, FFiniteFieldSize);  // T �õ� D1 ��Ԫ
    BigNumberDirectMulMod(X, N1, T, FFiniteFieldSize); // �õ� Sum.X

    BigNumberModularInverse(T, D2, FFiniteFieldSize);  // T �õ� D2 ��Ԫ
    BigNumberDirectMulMod(Y, N2, T, FFiniteFieldSize); // �õ� Sum.Y

    BigNumberCopy(Sum.X, X);
    BigNumberCopy(Sum.Y, Y);
  finally
    FBigNumberPool.Recycle(N2);
    FBigNumberPool.Recycle(N1);
    FBigNumberPool.Recycle(D2);
    FBigNumberPool.Recycle(D1);
    FBigNumberPool.Recycle(T);
    FBigNumberPool.Recycle(Y);
    FBigNumberPool.Recycle(X);
  end;
end;

procedure TCnTwistedEdwardsCurve.PointInverse(P: TCnEccPoint);
begin
  if BigNumberIsNegative(P.X) or (BigNumberCompare(P.X, FFiniteFieldSize) >= 0) then
    raise ECnEccException.Create(SCnErrorPointInverse);

  BigNumberSub(P.X, FFiniteFieldSize, P.X);
end;

procedure TCnTwistedEdwardsCurve.PointSubPoint(P: TCnEccPoint; Q: TCnEccPoint;
  Diff: TCnEccPoint);
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    PointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

procedure TCnTwistedEdwardsCurve.SetNeutualPoint(P: TCnEccPoint);
begin
  P.X.SetZero;
  P.Y.SetOne;
end;

function TCnTwistedEdwardsCurve.CalcXFromY(InY: TCnBigNumber; OutX: TCnBigNumber;
  XOdd: Boolean): Boolean;
var
  T, Y, Inv: TCnBigNumber;
begin
  T := nil;
  Y := nil;
  Inv := nil;

  try
    T := FBigNumberPool.Obtain;
    Y := FBigNumberPool.Obtain;

    BigNumberDirectMulMod(Y, InY, InY, FFiniteFieldSize);
    Y.SubWord(1); // Y := Y^2 - 1

    BigNumberDirectMulMod(T, InY, InY, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientD, FFiniteFieldSize);
    BigNumberSubMod(T, T, FCoefficientA, FFiniteFieldSize);
    // T := D*Y^2 - A

    Inv := FBigNumberPool.Obtain;
    BigNumberModularInverse(Inv, T, FFiniteFieldSize);

    BigNumberDirectMulMod(Y, Y, Inv, FFiniteFieldSize);  // Y �õ������ұߵ�ֵ

    Result := BigNumberSquareRootModPrime(OutX, Y, FFiniteFieldSize);

    // ��� X ��
    if Result and OutX.IsBitSet(0) <> XOdd then
      BigNumberSub(OutX, FFiniteFieldSize, OutX);
  finally
    FBigNumberPool.Recycle(Inv);
    FBigNumberPool.Recycle(Y);
    FBigNumberPool.Recycle(T);
  end;
end;

{ TCnMontgomeryCurve }

constructor TCnMontgomeryCurve.Create(const A, B, FieldPrime, GX, GY,
  Order: AnsiString; H: Integer);
begin
  Create;
  Load(A, B, FieldPrime, GX, GY, Order, H);
end;

constructor TCnMontgomeryCurve.Create;
begin
  inherited;
  FCoefficientA := TCnBigNumber.Create;
  FCoefficientB := TCnBigNumber.Create;
  FOrder := TCnBigNumber.Create;
  FFiniteFieldSize := TCnBigNumber.Create;
  FGenerator := TCnEccPoint.Create;
  FCoFactor := 1;

  FLadderConst := TCnBigNumber.Create;
end;

destructor TCnMontgomeryCurve.Destroy;
begin
  FLadderConst.Free;
  FGenerator.Free;
  FFiniteFieldSize.Free;
  FOrder.Free;
  FCoefficientB.Free;
  FCoefficientA.Free;
  inherited;
end;

function TCnMontgomeryCurve.IsPointOnCurve(P: TCnEccPoint): Boolean;
var
  X, Y, T: TCnBigNumber;
begin
  // �ж� B*y^2 �Ƿ���� x^3 + A*x^2 + x mod P

  X := nil;
  Y := nil;
  T := nil;

  try
    X := FBigNumberPool.Obtain;
    BigNumberCopy(X, P.X);

    Y := FBigNumberPool.Obtain;
    BigNumberCopy(Y, P.Y);

    BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize);
    BigNumberDirectMulMod(Y, FCoefficientB, Y, FFiniteFieldSize);  // Y := B * y^2 mod P

    T := FBigNumberPool.Obtain;
    BigNumberDirectMulMod(T, FCoefficientA, X, FFiniteFieldSize);  // T := A*X

    T.AddWord(1); // T := A*X + 1
    BigNumberDirectMulMod(T, X, T, FFiniteFieldSize);       // T := X * (A*X + 1) = AX^2 + X
    BigNumberPowerWordMod(X, X, 3, FFiniteFieldSize);  // X^3
    BigNumberAddMod(X, X, T, FFiniteFieldSize); // X := x^3 + Ax^2 + x mod P

    Result := BigNumberEqual(X, Y);
  finally
    FBigNumberPool.Recycle(Y);
    FBigNumberPool.Recycle(X);
    FBigNumberPool.Recycle(T);
  end;
end;

procedure TCnMontgomeryCurve.Load(const A: AnsiString; const B: AnsiString;
  const FieldPrime: AnsiString; const GX: AnsiString; const GY: AnsiString;
  const Order: AnsiString; H: Integer);
begin
  FCoefficientA.SetHex(A);
  FCoefficientB.SetHex(B);
  FFiniteFieldSize.SetHex(FieldPrime);
  FGenerator.X.SetHex(GX);
  FGenerator.Y.SetHex(GY);
  FOrder.SetHex(Order);
  FCoFactor := H;

  // ��ǰ���� (A + 2) / 4 �Ա��ɸ����������㷨��ʹ��
  CheckLadderConst;
end;

procedure TCnMontgomeryCurve.MultiplePoint(K: Int64; P: TCnEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := FBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MultiplePoint(BK, P);
  finally
    FBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderMultiplePoint(K: TCnBigNumber;
  P: TCnEccPoint);
var
  I, C: Integer;
  X0, X1: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    XAffinePointInverse(P);
  end;

  if BigNumberIsZero(K) then 
  begin
    P.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // �� 1 ���趯
    Exit;

  X0 := nil;
  X1 := nil;

  try
    X0 := TCnEccPoint.Create;
    X1 := TCnEccPoint.Create;

    X1.Assign(P);
    MontgomeryLadderPointXDouble(X0, P);

    C := K.GetBitsCount;
    for I := C - 2 downto 0 do // �ڲ��Ȳ����� Time Constant ִ��ʱ��̶���Ҫ��
    begin
      ConditionalSwapPoint(K.IsBitSet(I + 1) <> K.IsBitSet(I), X0, X1); // ��

      MontgomeryLadderPointXAdd(X1, X0, X1, P);
      MontgomeryLadderPointXDouble(X0, X0);
    end;

    ConditionalSwapPoint(K.IsBitSet(0), X0, X1);
    P.Assign(X0);
  finally
    X1.Free;
    X0.Free;
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderPointXAdd(Sum, P, Q,
  PMinusQ: TCnEccPoint);
var
  V0, V1, V2, V3, V4: TCnBigNumber;
begin
  V0 := nil;
  V1 := nil;
  V2 := nil;
  V3 := nil;
  V4 := nil;

  try
    V0 := FBigNumberPool.Obtain;
    V1 := FBigNumberPool.Obtain;
    V2 := FBigNumberPool.Obtain;
    V3 := FBigNumberPool.Obtain;
    V4 := FBigNumberPool.Obtain;

    BigNumberAddMod(V0, P.X, P.Y, FFiniteFieldSize);
    BigNumberSubMod(V1, Q.X, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V1, V1, V0, FFiniteFieldSize);

    BigNumberSubMod(V0, P.X, P.Y, FFiniteFieldSize);
    BigNumberAddMod(V2, Q.X, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V2, V2, V0, FFiniteFieldSize);

    BigNumberAddMod(V3, V1, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(V3, V3, V3, FFiniteFieldSize);

    BigNumberSubMod(V4, V1, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(V4, V4, V4, FFiniteFieldSize);

    BigNumberCopy(V0, PMinusQ.X); // V0 ���ݣ����� Sum �� PMinusQ ��ͬһ����ʱ���Ķ�
    BigNumberDirectMulMod(Sum.X, PMinusQ.Y, V3, FFiniteFieldSize);
    BigNumberDirectMulMod(Sum.Y, V0, V4, FFiniteFieldSize);
  finally
    FBigNumberPool.Recycle(V4);
    FBigNumberPool.Recycle(V3);
    FBigNumberPool.Recycle(V2);
    FBigNumberPool.Recycle(V1);
    FBigNumberPool.Recycle(V0);
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderPointXDouble(Dbl,
  P: TCnEccPoint);
var
  V1, V2, V3: TCnBigNumber;
begin
  V1 := nil;
  V2 := nil;
  V3 := nil;

  try
    V1 := FBigNumberPool.Obtain;
    V2 := FBigNumberPool.Obtain;
    V3 := FBigNumberPool.Obtain;

    CheckLadderConst;

    BigNumberAddMod(V1, P.X, P.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V1, V1, V1, FFiniteFieldSize);
    BigNumberSubMod(V2, P.X, P.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V2, V2, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(Dbl.X, V1, V2, FFiniteFieldSize);

    BigNumberSubMod(V1, V1, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(V3, V1, FLadderConst, FFiniteFieldSize);
    BigNumberAddMod(V3, V3, V2, FFiniteFieldSize);

    BigNumberDirectMulMod(Dbl.Y, V1, V3, FFiniteFieldSize);
  finally
    FBigNumberPool.Recycle(V3);
    FBigNumberPool.Recycle(V2);
    FBigNumberPool.Recycle(V1);
  end;
end;

procedure TCnMontgomeryCurve.MultiplePoint(K: TCnBigNumber; P: TCnEccPoint);
var
  I: Integer;
  E, R: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    PointInverse(P);
  end;

  if BigNumberIsZero(K) then
  begin
    P.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // �� 1 ���趯
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEccPoint.Create;
    E := TCnEccPoint.Create;

    // R ������ʱĬ��Ϊ����Զ��
    E.X := P.X;
    E.Y := P.Y;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        PointAddPoint(R, E, R);
      PointAddPoint(E, E, E);
    end;

    P.X := R.X;
    P.Y := R.Y;
  finally
    E.Free;
    R.Free;
  end;
end;

procedure TCnMontgomeryCurve.PointAddPoint(P, Q, Sum: TCnEccPoint);
var
  K, X, Y, T, SX, SY: TCnBigNumber;
begin
  // �ȼ���б�ʣ������� X ���Ȼ����ʱ��б�ʷֱ�Ϊ
  //          (y2 - y1)           3*x1^2 + 2*A*x1 + 1
  // б�� K = ----------  �� =  ----------------------
  //          (x2 - x1)                2*y1
  //
  // x3 = B*K^2 - A - x1 - x2
  // y3 = -(y1 + K * (x3 - x1))

  K := nil;
  X := nil;
  Y := nil;
  T := nil;
  SX := nil;
  SY := nil;

  try
    if P.IsZero then
    begin
      Sum.Assign(Q);
      Exit;
    end
    else if Q.IsZero then
    begin
      Sum.Assign(P);
      Exit;
    end;

    K := FBigNumberPool.Obtain;
    X := FBigNumberPool.Obtain;
    Y := FBigNumberPool.Obtain;
    T := FBigNumberPool.Obtain;
    SX := FBigNumberPool.Obtain;
    SY := FBigNumberPool.Obtain;

    if (BigNumberCompare(P.X, Q.X) = 0) and (BigNumberCompare(P.Y, Q.Y) = 0) then
    begin
      if P.Y.IsZero then
      begin
        Sum.SetZero;
        Exit;
      end;

      // ͬһ���㣬������б��
      // ������ (3*x1^2 + 2*A*x1 + 1)
      BigNumberDirectMulMod(Y, FCoefficientA, P.X, FFiniteFieldSize);
      BigNumberAddMod(Y, Y, Y, FFiniteFieldSize);
      Y.AddWord(1); // Y �õ� 2*A*x1 + 1

      BigNumberDirectMulMod(T, P.X, P.X, FFiniteFieldSize);
      T.MulWord(3);
      BigNumberAddMod(Y, T, Y, FFiniteFieldSize); // Y �õ� 3*x1^2 + 2*A*x1 + 1���ͷ� T

      BigNumberAddMod(X, P.Y, P.Y, FFiniteFieldSize);  // 2Y
      BigNumberModularInverse(T, X, FFiniteFieldSize); // �õ���ĸ 2*y1

      BigNumberDirectMulMod(K, Y, T, FFiniteFieldSize); // K �õ�����б��
    end
    else
    begin
      if BigNumberCompare(P.X, Q.X) = 0 then // ��� X ��ȣ�Ҫ�ж� Y �ǲ��ǻ����������Ϊ 0�����������
      begin
        BigNumberAdd(T, P.Y, Q.Y);
        if BigNumberCompare(T, FFiniteFieldSize) = 0 then  // ��������Ϊ 0
          Sum.SetZero
        else                                               // ������������
          raise ECnEccException.CreateFmt(SCnErrorCanNOTCalcFmt,
            [P.X.ToDec, P.Y.ToDec, Q.X.ToDec, Q.Y.ToDec]);

        Exit;
      end;

      BigNumberSubMod(Y, Q.Y, P.Y, FFiniteFieldSize);   // �õ����� (y2 - y1)
      BigNumberSubMod(X, Q.X, P.X, FFiniteFieldSize);   // �õ���ĸ (x2 - x1)

      BigNumberModularInverse(T, X, FFiniteFieldSize);
      BigNumberDirectMulMod(K, Y, T, FFiniteFieldSize); // K �õ�����б��
    end;

    // x3 = B * K^2 - A - x1 - x2
    BigNumberDirectMulMod(SX, K, K, FFiniteFieldSize);
    BigNumberDirectMulMod(SX, FCoefficientB, SX, FFiniteFieldSize);
    BigNumberSubMod(SX, SX, FCoefficientA, FFiniteFieldSize);
    BigNumberSubMod(SX, SX, P.X, FFiniteFieldSize);
    BigNumberSubMod(SX, SX, Q.X, FFiniteFieldSize);

    // y3 = -(y1 + K * (x3 - x1))
    BigNumberSubMod(SY, SX, P.X, FFiniteFieldSize);
    BigNumberDirectMulMod(SY, SY, K, FFiniteFieldSize);
    BigNumberAddMod(SY, SY, P.Y, FFiniteFieldSize);
    BigNumberSub(SY, FFiniteFieldSize, SY);

    BigNumberCopy(Sum.X, SX);
    BigNumberCopy(Sum.Y, SY);
  finally
    FBigNumberPool.Recycle(SY);
    FBigNumberPool.Recycle(SX);
    FBigNumberPool.Recycle(T);
    FBigNumberPool.Recycle(Y);
    FBigNumberPool.Recycle(X);
    FBigNumberPool.Recycle(K);
  end;
end;

procedure TCnMontgomeryCurve.PointInverse(P: TCnEccPoint);
begin
  if BigNumberIsNegative(P.Y) or (BigNumberCompare(P.Y, FFiniteFieldSize) >= 0) then
    raise ECnEccException.Create(SCnErrorPointInverse);

  BigNumberSub(P.Y, FFiniteFieldSize, P.Y);
end;

procedure TCnMontgomeryCurve.PointSubPoint(P, Q, Diff: TCnEccPoint);
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    PointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

procedure TCnMontgomeryCurve.CheckLadderConst;
var
  T: TCnBigNumber;
begin
  if FLadderConst.IsZero then
  begin
    FLadderConst.SetWord(4);
    T := FBigNumberPool.Obtain;

    try
      BigNumberModularInverse(T, FLadderConst, FFiniteFieldSize); // ���� 4 ����Ԫ

      BigNumberCopy(FLadderConst, FCoefficientA); // ���� A+2
      FLadderConst.AddWord(2);

      BigNumberDirectMulMod(FLadderConst, FLadderConst, T, FFiniteFieldSize); // ����Ԫ���ڳ�

      Cn25519BigNumberToField64(FLadderField64, FLadderConst);
    finally
      FBigNumberPool.Recycle(T);
    end;
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderMultiplePoint(K: Int64; P: TCnEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := FBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MontgomeryLadderMultiplePoint(BK, P);
  finally
    FBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnMontgomeryCurve.PointToXAffinePoint(DestPoint,
  SourcePoint: TCnEccPoint);
begin
  BigNumberCopy(DestPoint.X, SourcePoint.X);
  if SourcePoint.X.IsZero and SourcePoint.Y.IsZero then
  begin
    DestPoint.X.SetOne;
    DestPoint.Y.SetZero;
  end
  else
    DestPoint.Y.SetOne;
end;

procedure TCnMontgomeryCurve.XAffinePointToPoint(DestPoint,
  SourcePoint: TCnEccPoint);
var
  T, X, DX: TCnBigNumber;
begin
  // ����Ϊ��Ӱ (X, Z)���� x = (X/Z)������ y
  if SourcePoint.Y.IsZero then
  begin
    DestPoint.SetZero;
    Exit;
  end;

  T := nil;
  X := nil;
  DX := nil;

  try
    T := FBigNumberPool.Obtain;
    X := FBigNumberPool.Obtain;
    DX := FBigNumberPool.Obtain;

    BigNumberModularInverse(T, SourcePoint.Y, FFiniteFieldSize); // Z^-1
    BigNumberDirectMulMod(DX, SourcePoint.X, T, FFiniteFieldSize); // ��� DX ���Ȳ���ֵ����Ӱ��

    BigNumberCopy(X, DX); // DestPoint.X = X/Z

    // �� X^3+A*X^2+X mod P
    BigNumberPowerWordMod(X, DX, 3, FFiniteFieldSize);  // X^3

    BigNumberDirectMulMod(T, DX, DX, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientA, FFiniteFieldSize);  // A*X^2

    BigNumberAddMod(X, T, X, FFiniteFieldSize);
    BigNumberAddMod(X, X, DX, FFiniteFieldSize);  // �õ� X^3+A*X^2+X mod P

    BigNumberSquareRootModPrime(DestPoint.Y, X, FFiniteFieldSize);  // ��ģƽ����
    BigNumberCopy(DestPoint.X, DX);
  finally
    FBigNumberPool.Recycle(DX);
    FBigNumberPool.Recycle(X);
    FBigNumberPool.Recycle(T);
  end;
end;

procedure TCnMontgomeryCurve.XAffinePointInverse(P: TCnEccPoint);
begin
  // P ���ö�
end;


{ TCnCurve25519PrivateKey }

procedure TCnCurve25519PrivateKey.LoadFromData(Data: TCnCurve25519Data);
begin
  CnCurve25519DataToBigNumber(Data, Self);
  CnProcess25519ScalarNumber(Self);
end;

procedure TCnCurve25519PrivateKey.LoadFromHex(const Hex: string);
var
  D: TCnCurve25519Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnCurve25519PrivateKey.SaveToData(var Data: TCnCurve25519Data);
begin
  CnProcess25519ScalarNumber(Self);
  CnCurve25519BigNumberToData(Self, Data);
end;

function TCnCurve25519PrivateKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnCurve25519Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnCurve25519PublicKey }

procedure TCnCurve25519PublicKey.LoadFromData(Data: TCnCurve25519Data);
var
  XOdd: Boolean;
begin
  CnEd25519DataToPoint(TCnEd25519Data(Data), Self, XOdd);   // ���� Ed25519 �ģ�ֻ���� Y ����ż�Ժ��߻�ʡ����
end;

procedure TCnCurve25519PublicKey.LoadFromHex(const Hex: string);
var
  D: TCnCurve25519Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnCurve25519PublicKey.SaveToData(var Data: TCnCurve25519Data);
begin
  CnEd25519PointToData(Self, TCnEd25519Data(Data));   // ���� Ed25519 �ģ�ֻ�� Y���Լ� X ����ż��
end;

function TCnCurve25519PublicKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnCurve25519Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnCurve25519 }

constructor TCnCurve25519.Create;
begin
  inherited;
  Load(SCN_25519_MONT_A, SCN_25519_MONT_B, SCN_25519_PRIME, SCN_25519_MONT_GX,
    SCN_25519_MONT_GY, SCN_25519_ORDER, SCN_25519_COFACTOR);
end;

function TCnCurve25519.GenerateKeys(PrivateKey: TCnCurve25519PrivateKey;
  PublicKey: TCnCurve25519PublicKey): Boolean;
begin
  Result := False;
  if not BigNumberRandRange(PrivateKey, FOrder) then  // �� 0 �󵫱Ȼ����С�������
    Exit;

  if PrivateKey.IsZero then                           // ��һ���õ� 0������Ϊ 4
    PrivateKey.SetWord(4);

  CnProcess25519ScalarNumber(PrivateKey);             // �� RFC �涨����˽Կ

  PublicKey.Assign(FGenerator);
  MultiplePoint(PrivateKey, PublicKey);               // ����� PrivateKey ��
  Result := True;
end;

procedure TCnCurve25519.MultiplePoint(K: TCnBigNumber; P: TCnEccPoint);
var
  M: TCn25519Field64EccPoint;
begin
  PointToField64XAffinePoint(M, P);
  MontgomeryLadderField64MultiplePoint(K, M);
  Field64XAffinePointToPoint(P, M);
end;

procedure TCnCurve25519.PointToField64XAffinePoint(
  var DestPoint: TCn25519Field64EccPoint; SourcePoint: TCnEccPoint);
var
  T: TCnEccPoint;
begin
  if SourcePoint = nil then
    Exit;

  T := TCnEccPoint.Create;
  try
    PointToXAffinePoint(T, SourcePoint); // ��ͨ��ת��Ϊ��Ӱ���� X Z ��

    Cn25519BigNumberToField64(DestPoint.X, T.X);    // ��Ӱ���� X Z ��ת��Ϊ����ʽ��
    Cn25519BigNumberToField64(DestPoint.Y, T.Y);
  finally
    T.Free;
  end;
end;

procedure TCnCurve25519.Field64XAffinePointToPoint(DestPoint: TCnEccPoint;
  var SourcePoint: TCn25519Field64EccPoint);
var
  T: TCnEccPoint;
begin
  if DestPoint = nil then
    Exit;

  T := TCnEccPoint.Create;
  try
    Cn25519Field64ToBigNumber(T.X, SourcePoint.X);  // ����ʽ��ת��Ϊ��Ӱ���� X Z ��
    Cn25519Field64ToBigNumber(T.Y, SourcePoint.Y);

    XAffinePointToPoint(DestPoint, T);   // ����ʽ��ת��Ϊ��Ӱ���� X Z ��
  finally
    T.Free;
  end;
end;

procedure TCnCurve25519.MontgomeryLadderField64MultiplePoint(
  K: TCnBigNumber; var P: TCn25519Field64EccPoint);
var
  I, C: Integer;
  X0, X1: TCn25519Field64EccPoint;
begin
  if BigNumberIsZero(K) then // ������ K Ϊ��ֵ�����
  begin
    Cn25519Field64Zero(P.X);
    Cn25519Field64Zero(P.Y);
    Exit;
  end
  else if BigNumberIsOne(K) then // �� 1 ���趯
    Exit;

  Cn25519Field64EccPointCopy(X1, P);
  MontgomeryLadderField64PointXDouble(X0, P);

  C := K.GetBitsCount;
  for I := C - 2 downto 0 do // �ڲ��Ȳ����� Time Constant ִ��ʱ��̶���Ҫ��
  begin
    ConditionalSwapField64Point(K.IsBitSet(I + 1) <> K.IsBitSet(I), X0, X1); // ��

    MontgomeryLadderField64PointXAdd(X1, X0, X1, P);
    MontgomeryLadderField64PointXDouble(X0, X0);
  end;

  ConditionalSwapField64Point(K.IsBitSet(0), X0, X1);
  Cn25519Field64EccPointCopy(P, X0);
end;

procedure TCnCurve25519.MontgomeryLadderField64MultiplePoint(K: Int64;
  var P: TCn25519Field64EccPoint);
var
  BK: TCnBigNumber;
begin
  BK := FBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MontgomeryLadderField64MultiplePoint(BK, P);
  finally
    FBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnCurve25519.MontgomeryLadderField64PointXAdd(var Sum, P,
  Q, PMinusQ: TCn25519Field64EccPoint);
var
  V0, V1, V2, V3, V4: TCn25519Field64;
begin
  Cn25519Field64Add(V0, P.X, P.Y);
  Cn25519Field64Sub(V1, Q.X, Q.Y);
  Cn25519Field64Mul(V1, V1, V0);

  Cn25519Field64Sub(V0, P.X, P.Y);
  Cn25519Field64Add(V2, Q.X, Q.Y);
  Cn25519Field64Mul(V2, V2, V0);

  Cn25519Field64Add(V3, V1, V2);
  Cn25519Field64Mul(V3, V3, V3);

  Cn25519Field64Sub(V4, V1, V2);
  Cn25519Field64Mul(V4, V4, V4);

  Cn25519Field64Copy(V0, PMinusQ.X);   // V0 ���ݣ����� Sum �� PMinusQ ��ͬһ����ʱ���Ķ�
  Cn25519Field64Mul(Sum.X, PMinusQ.Y, V3);
  Cn25519Field64Mul(Sum.Y, V0, V4);
end;

procedure TCnCurve25519.MontgomeryLadderField64PointXDouble(var Dbl,
  P: TCn25519Field64EccPoint);
var
  V1, V2, V3: TCn25519Field64;
begin
  CheckLadderConst;
  Cn25519Field64Add(V1, P.X, P.Y);
  Cn25519Field64Mul(V1, V1, V1);

  Cn25519Field64Sub(V2, P.X, P.Y);
  Cn25519Field64Mul(V2, V2, V2);

  Cn25519Field64Mul(Dbl.X, V1, V2);

  Cn25519Field64Sub(V1, V1, V2);
  Cn25519Field64Mul(V3, V1, FLadderField64);

  Cn25519Field64Add(V3, V3, V2);

  Cn25519Field64Mul(Dbl.Y, V1, V3);
end;

{ TCnEd25519 }

constructor TCnEd25519.Create;
begin
  inherited;
  Load(SCN_25519_EDWARDS_A, SCN_25519_EDWARDS_D, SCN_25519_PRIME, SCN_25519_EDWARDS_GX,
    SCN_25519_EDWARDS_GY, SCN_25519_ORDER, 8);
end;

procedure TCnEd25519.ExtendedField64MultiplePoint(K: Int64;
  var P: TCn25519Field64Ecc4Point);
var
  BK: TCnBigNumber;
begin
  BK := FBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    ExtendedField64MultiplePoint(BK, P);
  finally
    FBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnEd25519.ExtendedField64MultiplePoint(K: TCnBigNumber;
  var P: TCn25519Field64Ecc4Point);
var
  I: Integer;
  E, R: TCn25519Field64Ecc4Point;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    ExtendedField64PointInverse(P);
  end;

  if BigNumberIsZero(K) then
  begin
    Cn25519Field64Ecc4PointNeutual(P);
    Exit;
  end
  else if BigNumberIsOne(K) then // �� 1 ���趯
    Exit;

  // R Ҫ�����Ե�
  Cn25519Field64Ecc4PointNeutual(R);
  Cn25519Field64Ecc4PointCopy(E, P);

  for I := 0 to BigNumberGetBitsCount(K) - 1 do
  begin
    if BigNumberIsBitSet(K, I) then
      ExtendedField64PointAddPoint(R, E, R);
    ExtendedField64PointAddPoint(E, E, E);
  end;

  Cn25519Field64Ecc4PointCopy(P, R);
end;

procedure TCnEd25519.ExtendedField64PointAddPoint(var P, Q,
  Sum: TCn25519Field64Ecc4Point);
var
  A, B, C, D, E, F, G, H: TCn25519Field64;
  CoD: TCn25519Field64;
begin
  if Cn25519Field64Ecc4PointEqual(P, Q) then
  begin
    // ��ͬһ����
    Cn25519Field64Mul(A, P.X, P.X);   // A = X1^2
    Cn25519Field64Mul(B, P.Y, P.Y);   // B = Y1^2

    Cn25519Field64Mul(C, P.Z, P.Z);
    Cn25519Field64Add(C, C, C);       // C = 2*Z1^2

    Cn25519Field64Add(H, A, B);       // H = A+B

    Cn25519Field64Add(E, P.X, P.Y);
    Cn25519Field64Mul(E, E, E);
    Cn25519Field64Sub(E, H, E);       // E = H-(X1+Y1)^2

    Cn25519Field64Sub(G, A, B);       // G = A-B
    Cn25519Field64Add(F, C, G);       // F = C+G

    Cn25519Field64Mul(Sum.X, E, F);   // X3 = E*F
    Cn25519Field64Mul(Sum.Y, G, H);   // Y3 = G*H
    Cn25519Field64Mul(Sum.T, E, H);   // T3 = E*H
    Cn25519Field64Mul(Sum.Z, F, G);   // Z3 = F*G
  end
  else
  begin
    // ����ͬһ���㡣���� G H ����ʱ����
    Cn25519Field64Sub(G, P.Y, P.X);
    Cn25519Field64Sub(H, Q.Y, Q.X);
    Cn25519Field64Mul(A, G, H); // A = (Y1-X1)*(Y2-X2)

    Cn25519Field64Add(G, P.Y, P.X);
    Cn25519Field64Add(H, Q.Y, Q.X);
    Cn25519Field64Mul(B, G, H);  // B = (Y1+X1)*(Y2+X2)

    Cn25519BigNumberToField64(CoD, FCoefficientD);
    Cn25519Field64Add(C, CoD, CoD);
    Cn25519Field64Mul(C, P.T, C);
    Cn25519Field64Mul(C, Q.T, C);   // C = T1*2*d*T2

    Cn25519Field64Add(D, P.Z, P.Z);
    Cn25519Field64Mul(D, Q.Z, D);   // D = Z1*2*Z2

    Cn25519Field64Sub(E, B, A);   // E = B-A
    Cn25519Field64Sub(F, D, C);   // F = D-C
    Cn25519Field64Add(G, D, C);   // G = D+C
    Cn25519Field64Add(H, B, A);   // H = B+A

    Cn25519Field64Mul(Sum.X, E, F);   // X3 = E*F
    Cn25519Field64Mul(Sum.Y, G, H);   // Y3 = G*H
    Cn25519Field64Mul(Sum.T, E, H);   // T3 = E*H
    Cn25519Field64Mul(Sum.Z, F, G);   // Z3 = F*G
  end;
end;

procedure TCnEd25519.ExtendedField64PointInverse(
  var P: TCn25519Field64Ecc4Point);
var
  T: TCn25519Field64;
begin
  // X -> Prime - X
  Cn25519Field64Sub(P.X, F25519Field64Zero, P.X);

  // T := X * Y / Z^3
  if Cn25519Field64Equal(P.Z, F25519Field64One) then
  begin
    // Z = 1 ��ֱ�ӳ�
    Cn25519Field64Mul(P.T, P.X, P.Y);
  end
  else // Z <> 1 
  begin
    // ���� Z^3 ��ģ��Ԫ
    Cn25519Field64Mul(T, P.Z, P.Z);
    Cn25519Field64Mul(T, T, P.Z);

    Cn25519Field64ModularInverse(T, T);

    // �ٳ��� X * Y
    Cn25519Field64Mul(P.T, P.X, P.Y);
    Cn25519Field64Mul(P.T, P.T, T);
  end;
end;

procedure TCnEd25519.ExtendedField64PointSubPoint(var P, Q,
  Diff: TCn25519Field64Ecc4Point);
var
  Inv: TCn25519Field64Ecc4Point;
begin
  Cn25519Field64Ecc4PointCopy(Inv, Q);
  ExtendedField64PointInverse(Inv);
  ExtendedField64PointAddPoint(P, Inv, Diff);
end;

function TCnEd25519.IsExtendedField64PointOnCurve(
  var P: TCn25519Field64Ecc4Point): Boolean;
var
  Q: TCnEccPoint;
begin
  Q := TCnEccPoint.Create;
  try
    CnField64Ecc4PointToEccPoint(Q, P);
    Result := IsPointOnCurve(Q);
  finally
    Q.Free;
  end;
end;

function TCnEd25519.GenerateKeys(PrivateKey: TCnEd25519PrivateKey;
  PublicKey: TCnEd25519PublicKey): Boolean;
var
  K: TCnBigNumber;
begin
  Result := False;

  // ��� 32 �ֽ��� PrivateKey
  if not BigNumberRandBytes(PrivateKey, CN_25519_BLOCK_BYTESIZE) then
    Exit;

  K := FBigNumberPool.Obtain;
  try
    CnCalcKeysFromEd25519PrivateKey(PrivateKey, K, nil);

    // �ó��� K ���� G ��õ���Կ
    PublicKey.Assign(FGenerator);
    MultiplePoint(K, PublicKey);                         // ����� K ��

    Result := True;
  finally
    FBigNumberPool.Recycle(K);
  end;
end;

procedure TCnEd25519.MultiplePoint(K: TCnBigNumber; P: TCnEccPoint);
var
  P4: TCn25519Field64Ecc4Point;
begin
  CnEccPointToField64Ecc4Point(P4, P);
  ExtendedField64MultiplePoint(K, P4);
  CnField64Ecc4PointToEccPoint(P, P4);
end;

function TCnEd25519.IsNeutualExtendedPoint(P: TCnEcc4Point): Boolean;
begin
  Result := P.X.IsZero and P.T.IsZero and not P.Y.IsZero and not P.Z.IsZero
    and BigNumberEqual(P.Y, P.Z);
end;

procedure TCnEd25519.SetNeutualExtendedPoint(P: TCnEcc4Point);
begin
  P.X.SetZero;
  P.Y.SetOne;
  P.Z.SetOne;
  P.T.SetZero;
end;

procedure TCnEd25519.ExtendedMultiplePoint(K: Int64; P: TCnEcc4Point);
var
  BK: TCnBigNumber;
begin
  BK := FBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    ExtendedMultiplePoint(BK, P);
  finally
    FBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnEd25519.ExtendedMultiplePoint(K: TCnBigNumber; P: TCnEcc4Point);
var
  I: Integer;
  E, R: TCnEcc4Point;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    ExtendedPointInverse(P);
  end;

  if BigNumberIsZero(K) then
  begin
    SetNeutualExtendedPoint(P);
    Exit;
  end
  else if BigNumberIsOne(K) then // �� 1 ���趯
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEcc4Point.Create;
    E := TCnEcc4Point.Create;

    // R Ҫ�����Ե�
    SetNeutualExtendedPoint(R);

    E.X := P.X;
    E.Y := P.Y;
    E.Z := P.Z;
    E.T := P.T;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        ExtendedPointAddPoint(R, E, R);
      ExtendedPointAddPoint(E, E, E);
    end;

    P.X := R.X;
    P.Y := R.Y;
    P.Z := R.Z;
  finally
    R.Free;
    E.Free;
  end;
end;

procedure TCnEd25519.ExtendedPointAddPoint(P, Q, Sum: TCnEcc4Point);
var
  A, B, C, D, E, F, G, H: TCnBigNumber;
begin
{
  RFC 8032 ����Ԫ�����㷨��Ҫ�󷽳��еĲ��� A Ϊ -1

  ��ͬ���                 ͬ�㱶��

  A = (Y1-X1)*(Y2-X2)      A = X1^2
  B = (Y1+X1)*(Y2+X2)      B = Y1^2
  C = T1*2*d*T2            C = 2*Z1^2
  D = Z1*2*Z2              H = A+B
  E = B-A                  E = H-(X1+Y1)^2
  F = D-C                  G = A-B
  G = D+C                  F = C+G
  H = B+A

  X3 = E*F                 X3 = E*F
  Y3 = G*H                 Y3 = G*H
  T3 = E*H                 T3 = E*H
  Z3 = F*G                 Z3 = F*G
}

  A := nil;
  B := nil;
  C := nil;
  D := nil;
  E := nil;
  F := nil;
  G := nil;
  H := nil;

  try
    A := FBigNumberPool.Obtain;
    B := FBigNumberPool.Obtain;
    C := FBigNumberPool.Obtain;
    D := FBigNumberPool.Obtain;
    E := FBigNumberPool.Obtain;
    F := FBigNumberPool.Obtain;
    G := FBigNumberPool.Obtain;
    H := FBigNumberPool.Obtain;

    if CnEcc4PointEqual(P, Q, FFiniteFieldSize) then
    begin
      // ��ͬһ����
      BigNumberDirectMulMod(A, P.X, P.X, FFiniteFieldSize); // A = X1^2
      BigNumberDirectMulMod(B, P.Y, P.Y, FFiniteFieldSize);  // B = Y1^2

      BigNumberDirectMulMod(C, P.Z, P.Z, FFiniteFieldSize);
      BigNumberAddMod(C, C, C, FFiniteFieldSize);      // C = 2*Z1^2

      BigNumberAddMod(H, A, B, FFiniteFieldSize);      // H = A+B

      BigNumberAddMod(E, P.X, P.Y, FFiniteFieldSize);
      BigNumberDirectMulMod(E, E, E, FFiniteFieldSize);
      BigNumberSubMod(E, H, E, FFiniteFieldSize);      // E = H-(X1+Y1)^2

      BigNumberSubMod(G, A, B, FFiniteFieldSize);      // G = A-B
      BigNumberAddMod(F, C, G, FFiniteFieldSize);      // F = C+G

      BigNumberDirectMulMod(Sum.X, E, F, FFiniteFieldSize);  // X3 = E*F
      BigNumberDirectMulMod(Sum.Y, G, H, FFiniteFieldSize);  // Y3 = G*H
      BigNumberDirectMulMod(Sum.T, E, H, FFiniteFieldSize);  // T3 = E*H
      BigNumberDirectMulMod(Sum.Z, F, G, FFiniteFieldSize);  // Z3 = F*G
    end
    else
    begin
      // ����ͬһ���㡣���� G H ����ʱ����
      BigNumberSubMod(G, P.Y, P.X, FFiniteFieldSize);
      BigNumberSubMod(H, Q.Y, Q.X, FFiniteFieldSize);
      BigNumberDirectMulMod(A, G, H, FFiniteFieldSize); // A = (Y1-X1)*(Y2-X2)

      BigNumberAddMod(G, P.Y, P.X, FFiniteFieldSize);
      BigNumberAddMod(H, Q.Y, Q.X, FFiniteFieldSize);
      BigNumberDirectMulMod(B, G, H, FFiniteFieldSize);  // B = (Y1+X1)*(Y2+X2)

      BigNumberAdd(C, FCoefficientD, FCoefficientD);
      BigNumberDirectMulMod(C, P.T, C, FFiniteFieldSize);
      BigNumberDirectMulMod(C, Q.T, C, FFiniteFieldSize);  // C = T1*2*d*T2

      BigNumberAdd(D, P.Z, P.Z);
      BigNumberDirectMulMod(D, Q.Z, D, FFiniteFieldSize);  // D = Z1*2*Z2

      BigNumberSubMod(E, B, A, FFiniteFieldSize);  // E = B-A
      BigNumberSubMod(F, D, C, FFiniteFieldSize);  // F = D-C
      BigNumberAddMod(G, D, C, FFiniteFieldSize);  // G = D+C
      BigNumberAddMod(H, B, A, FFiniteFieldSize);  // H = B+A

      BigNumberDirectMulMod(Sum.X, E, F, FFiniteFieldSize);  // X3 = E*F
      BigNumberDirectMulMod(Sum.Y, G, H, FFiniteFieldSize);  // Y3 = G*H
      BigNumberDirectMulMod(Sum.T, E, H, FFiniteFieldSize);  // T3 = E*H
      BigNumberDirectMulMod(Sum.Z, F, G, FFiniteFieldSize);  // Z3 = F*G
    end;
  finally
    FBigNumberPool.Recycle(H);
    FBigNumberPool.Recycle(G);
    FBigNumberPool.Recycle(F);
    FBigNumberPool.Recycle(E);
    FBigNumberPool.Recycle(D);
    FBigNumberPool.Recycle(C);
    FBigNumberPool.Recycle(B);
    FBigNumberPool.Recycle(A);
  end;
end;

procedure TCnEd25519.ExtendedPointInverse(P: TCnEcc4Point);
var
  T: TCnBigNumber;
begin
  T := FBigNumberPool.Obtain;
  try
    // x -> -x����ζ�� X/Z -> P - X/Z��Ҳ���� (P*Z - X)/Z�������� X = P*Z - X��ǰ���� 0��������� P - X
    BigNumberDirectMulMod(T, P.Z, FFiniteFieldSize, FFiniteFieldSize);
    BigNumberSubMod(P.X, T, P.X, FFiniteFieldSize); // �ͷ� T

    // T := X * Y / Z^3
    BigNumberPowerWordMod(T, P.Z, 3, FFiniteFieldSize);
    BigNumberModularInverse(T, T, FFiniteFieldSize); // T �� Z^3 ����Ԫ
    BigNumberDirectMulMod(P.T, P.X, P.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(P.T, P.T, T, FFiniteFieldSize);
  finally
    FBigNumberPool.Recycle(T);
  end;
end;

function TCnEd25519.IsExtendedPointOnCurve(P: TCnEcc4Point): Boolean;
var
  Q: TCnEccPoint;
begin
  Q := TCnEccPoint.Create;
  try
    CnEcc4PointToEccPoint(Q, P, FFiniteFieldSize);
    Result := IsPointOnCurve(Q);
  finally
    Q.Free;
  end;
end;

procedure TCnEd25519.ExtendedPointSubPoint(P, Q, Diff: TCnEcc4Point);
var
  Inv: TCnEcc4Point;
begin
  Inv := TCnEcc4Point.Create;
  try
    Inv.Assign(Q);
    ExtendedPointInverse(Inv);
    ExtendedPointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

procedure TCnEd25519.PlainToPoint(Plain: TCnEd25519Data; OutPoint: TCnEccPoint);
var
  XOdd: Boolean;
begin
  if OutPoint = nil then
    Exit;

  // �ȴ� Plain �л�ԭ Y �����Լ� X �����ż��
  CnEd25519DataToPoint(Plain, OutPoint, XOdd);

  // �õ� Y ����� X�� ע������ 25519 �� 8u5 ����ʽ
  if not CalcXFromY(OutPoint.Y, OutPoint.X, XOdd) then
    raise ECnEccException.Create(SCnErrorPointNotOnCurve);
end;

procedure TCnEd25519.PointToPlain(Point: TCnEccPoint;
  var OutPlain: TCnEd25519Data);
begin
  if (Point = nil) or (BigNumberCompare(Point.Y, FFiniteFieldSize) >= 0) then
    Exit;

  CnEd25519PointToData(Point, OutPlain);
end;

function CnEcc4PointToString(P: TCnEcc4Point): string;
begin
  Result := Format('%s,%s,%s,%s', [P.X.ToDec, P.Y.ToDec, P.Z.ToDec, P.T.ToDec]);
end;

function CnEcc4PointToHex(P: TCnEcc4Point): string;
begin
  Result := Format('%s,%s,%s,%s', [P.X.ToHex, P.Y.ToHex, P.Z.ToHex, P.T.ToHex]);
end;

function CnEcc4PointEqual(P: TCnEcc4Point; Q: TCnEcc4Point; Prime: TCnBigNumber): Boolean;
var
  T1, T2: TCnBigNumber;
begin
  // X1*Z2 = X2*Z1 �� Y1*Z2 = Y2*Z1
  Result := False;
  if P = Q then
  begin
    Result := True;
    Exit;
  end;

  T1 := nil;
  T2 := nil;

  try
    T1 := FBigNumberPool.Obtain;
    T2 := FBigNumberPool.Obtain;

    BigNumberDirectMulMod(T1, P.X, Q.Z, Prime);
    BigNumberDirectMulMod(T2, Q.X, P.Z, Prime);

    if not BigNumberEqual(T1, T2) then
      Exit;

    BigNumberDirectMulMod(T1, P.Y, Q.Z, Prime);
    BigNumberDirectMulMod(T2, Q.Y, P.Z, Prime);

    if not BigNumberEqual(T1, T2) then
      Exit;

    Result := True;
  finally
    FBigNumberPool.Recycle(T2);
    FBigNumberPool.Recycle(T1);
  end;
end;

function CnEccPointToEcc4Point(DestPoint: TCnEcc4Point; SourcePoint: TCnEccPoint;
  Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not CnEccPointToEcc3Point(SourcePoint, DestPoint) then
    Exit;
  Result := BigNumberDirectMulMod(DestPoint.T, SourcePoint.X, SourcePoint.Y, Prime);
end;

function CnEcc4PointToEccPoint(DestPoint: TCnEccPoint; SourcePoint: TCnEcc4Point;
  Prime: TCnBigNumber): Boolean;
begin
  Result := CnAffinePointToEccPoint(SourcePoint, DestPoint, Prime);
end;

// =============================================================================
//
//          Curve25519 �� u v �� Ed25519 �� x y ��˫��ӳ���ϵΪ��
//
//              (u, v) = ((1+y)/(1-y), sqrt(-486664)*u/x)
//              (x, y) = (sqrt(-486664)*u/v, (u-1)/(u+1))
//
// =============================================================================

procedure CnCurve25519PointToEd25519Point(DestPoint, SourcePoint: TCnEccPoint);
var
  S, T, Inv, Prime, TX: TCnBigNumber;
begin
  // x = sqrt(-486664)*u/v
  // y = (u-1)/(u+1)

  S := nil;
  T := nil;
  Prime := nil;
  Inv := nil;
  TX := nil;

  try
    S := FBigNumberPool.Obtain;
    T := FBigNumberPool.Obtain;

    S.SetHex(SCN_25519_SQRT_NEG_486664);
    Prime := FBigNumberPool.Obtain;
    Prime.SetHex(SCN_25519_PRIME);

    BigNumberDirectMulMod(T, S, SourcePoint.X, Prime); // sqrt * u

    Inv := FBigNumberPool.Obtain;
    BigNumberModularInverse(Inv, SourcePoint.Y, Prime); // v^-1

    TX := FBigNumberPool.Obtain;
    BigNumberDirectMulMod(TX, T, Inv, Prime); // �㵽 X�����Ȳ���ֵ������ԴĿ��ͬ�������Ӱ��

    BigNumberCopy(T, SourcePoint.X);
    BigNumberCopy(S, SourcePoint.X);

    T.SubWord(1);  // u - 1
    S.AddWord(1);  // u + 1

    BigNumberModularInverse(Inv, S, Prime); // (u + 1)^1
    BigNumberDirectMulMod(DestPoint.Y, T, Inv, Prime);
    BigNumberCopy(DestPoint.X, TX);
  finally
    FBigNumberPool.Recycle(TX);
    FBigNumberPool.Recycle(Inv);
    FBigNumberPool.Recycle(Prime);
    FBigNumberPool.Recycle(T);
    FBigNumberPool.Recycle(S);
  end;
end;

procedure CnEd25519PointToCurve25519Point(DestPoint, SourcePoint: TCnEccPoint);
var
  S, T, Inv, Prime, TX: TCnBigNumber;
begin
  // u = (1+y)/(1-y)
  // v = sqrt(-486664)*u/x

  S := nil;
  T := nil;
  Prime := nil;
  Inv := nil;
  TX := nil;

  try
    S := FBigNumberPool.Obtain;
    T := FBigNumberPool.Obtain;

    BigNumberCopy(T, SourcePoint.Y);
    BigNumberCopy(S, SourcePoint.Y);
    T.AddWord(1);  // T �Ƿ��� 1+y

    Prime := FBigNumberPool.Obtain;
    Prime.SetHex(SCN_25519_PRIME);

    BigNumberSubMod(S, CnBigNumberOne, SourcePoint.Y, Prime); // S �Ƿ�ĸ 1-y

    Inv := FBigNumberPool.Obtain;
    BigNumberModularInverse(Inv, S, Prime); // Inv �Ƿ�ĸ����������

    TX := FBigNumberPool.Obtain;
    BigNumberDirectMulMod(TX, T, Inv, Prime); // �õ� U��������ֵ�����ݴ棬����ԴĿ��ͬ�����Ӱ��

    S.SetHex(SCN_25519_SQRT_NEG_486664);
    BigNumberDirectMulMod(T, S, TX, Prime);

    BigNumberModularInverse(Inv, SourcePoint.X, Prime);
    BigNumberDirectMulMod(DestPoint.Y, T, Inv, Prime);

    BigNumberCopy(DestPoint.X, TX); // ���ݴ�� TX ����Ŀ���
  finally
    FBigNumberPool.Recycle(TX);
    FBigNumberPool.Recycle(Inv);
    FBigNumberPool.Recycle(Prime);
    FBigNumberPool.Recycle(T);
    FBigNumberPool.Recycle(S);
  end;
end;

procedure CnCurve25519PointToData(P: TCnEccPoint; var Data: TCnCurve25519Data);
begin
  if P = nil then
    Exit;

  FillChar(Data[0], SizeOf(TCnCurve25519Data), 0);
  P.X.ToBinary(@Data[0], SizeOf(TCnCurve25519Data));
  ReverseMemory(@Data[0], SizeOf(TCnCurve25519Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��
end;

procedure CnCurve25519DataToPoint(Data: TCnCurve25519Data; P: TCnEccPoint);
var
  D: TCnCurve25519Data;
begin
  if P = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnCurve25519Data));
  ReverseMemory(@D[0], SizeOf(TCnCurve25519Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  P.Y.SetBinary(@D[0], SizeOf(TCnCurve25519Data));
end;

procedure CnEd25519PointToData(P: TCnEccPoint; var Data: TCnEd25519Data);
begin
  if P = nil then
    Exit;

  FillChar(Data[0], SizeOf(TCnEd25519Data), 0);
  P.Y.ToBinary(@Data[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd25519Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  if P.X.IsOdd then // X �����������λ�� 1
    Data[CN_25519_BLOCK_BYTESIZE - 1] := Data[CN_25519_BLOCK_BYTESIZE - 1] or $80  // ��λ�� 1
  else
    Data[CN_25519_BLOCK_BYTESIZE - 1] := Data[CN_25519_BLOCK_BYTESIZE - 1] and $7F; // ��λ�� 0
end;

procedure CnEd25519DataToPoint(Data: TCnEd25519Data; P: TCnEccPoint;
  out XOdd: Boolean);
var
  D: TCnEd25519Data;
begin
  if P = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@D[0], SizeOf(TCnEd25519Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  P.Y.SetBinary(@D[0], SizeOf(TCnEd25519Data));

  // ���λ�Ƿ��� 0 ��ʾ�� X ����ż
  XOdd := P.Y.IsBitSet(8 * CN_25519_BLOCK_BYTESIZE - 1);

  // ���λ������
  P.Y.ClearBit(8 * CN_25519_BLOCK_BYTESIZE - 1);
end;

procedure CnEd25519BigNumberToData(N: TCnBigNumber; var Data: TCnEd25519Data);
begin
  if N = nil then
    Exit;

  if N.GetBytesCount > SizeOf(TCnEd25519Data) then
    raise ECnEccException.Create(SCnErrorNumberTooBig);

  FillChar(Data[0], SizeOf(TCnEd25519Data), 0);
  N.ToBinary(@Data[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd25519Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��
end;

procedure CnEd25519DataToBigNumber(Data: TCnEd25519Data; N: TCnBigNumber);
var
  D: TCnEd25519Data;
begin
  if N = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@D[0], SizeOf(TCnEd25519Data));
  // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

  N.SetBinary(@D[0], SizeOf(TCnEd25519Data));
end;

procedure CnCurve25519BigNumberToData(N: TCnBigNumber; var Data: TCnCurve25519Data);
begin
  CnEd25519BigNumberToData(N, TCnEd25519Data(Data)); // ʵ���� Ed25519 ��ͬ
end;

procedure CnCurve25519DataToBigNumber(Data: TCnCurve25519Data; N: TCnBigNumber);
begin
  CnEd25519DataToBigNumber(TCnEd25519Data(Data), N); // ʵ���� Ed25519 ��ͬ
end;

function CnEd25519SignData(PlainData: Pointer; DataByteLen: Integer; PrivateKey: TCnEd25519PrivateKey;
  PublicKey: TCnEd25519PublicKey; OutSignature: TCnEd25519Signature; Ed25519: TCnEd25519): Boolean;
var
  Is25519Nil: Boolean;
  Stream: TMemoryStream;
  R, S, K, HP: TCnBigNumber;
  Dig: TCnSHA512Digest;
  Data: TCnEd25519Data;
begin
  Result := False;
  if (PlainData = nil) or (DataByteLen <= 0) or (PrivateKey = nil) or (PublicKey = nil)
    or (OutSignature = nil) then
    Exit;

  R := nil;
  S := nil;
  K := nil;
  HP := nil;
  Stream := nil;
  Is25519Nil := Ed25519 = nil;

  try
    if Is25519Nil then
      Ed25519 := TCnEd25519.Create;

    R := FBigNumberPool.Obtain;
    S := FBigNumberPool.Obtain;
    K := FBigNumberPool.Obtain;
    HP := FBigNumberPool.Obtain;

    // ����˽Կ�õ�˽Կ���� s ���Ӵ�ǰ׺
    CnCalcKeysFromEd25519PrivateKey(PrivateKey, S, HP);

    // �Ӵ�ǰ׺ƴ��ԭʼ���֣�ע�� RFC 8032 �е� dom2(x, y) �ڴ�Ϊ�գ��� Context �� phFlag ȫΪ��
    // �� PH ����Ϊԭʼ���������������
    Stream := TMemoryStream.Create;
    BigNumberWriteBinaryToStream(HP, Stream, CN_25519_BLOCK_BYTESIZE);
    Stream.Write(PlainData^, DataByteLen);

    // ����� 64 �ֽڵ� SHA512 ֵ��Ϊ r ������׼�����Ի�����Ϊ R ��
    Dig := SHA512Buffer(Stream.Memory, Stream.Size);

    ReverseMemory(@Dig[0], SizeOf(TCnSHA512Digest));
    // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ�������Ҫ��һ��

    R.SetBinary(@Dig[0], SizeOf(TCnSHA512Digest));
    BigNumberNonNegativeMod(R, R, Ed25519.Order);  // �� r ����ʵ��̫���� mod һ�½�

    OutSignature.R.Assign(Ed25519.Generator);
    Ed25519.MultiplePoint(R, OutSignature.R);      // ����õ�ǩ��ֵ R����ֵ��һ��������

    // �� Hash ���� S���ȵ� R ת��Ϊ�ֽ�����
    Ed25519.PointToPlain(OutSignature.R, Data);

    // ƴ����
    Stream.Clear;
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));

    // ��Կ��Ҳת��Ϊ�ֽ�����
    Ed25519.PointToPlain(PublicKey, Data);
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));

    // д���ģ�ƴ�����
    Stream.Write(PlainData^, DataByteLen);

    // �ٴ��Ӵ� R||PublicKey||����
    Dig := SHA512Buffer(Stream.Memory, Stream.Size);

    ReverseMemory(@Dig[0], SizeOf(TCnSHA512Digest));
    // RFC �涨��С���򵫴��� Binary �������ֽ�˳��Ҳ���Ǵ���������Ҫ��һ��

    K.SetBinary(@Dig[0], SizeOf(TCnSHA512Digest));
    BigNumberNonNegativeMod(K, K, Ed25519.Order);  // ����̫������ mod һ�½�

    // ������� R + K * S mod Order
    BigNumberDirectMulMod(OutSignature.S, K, S, Ed25519.Order);
    BigNumberAddMod(OutSignature.S, R, OutSignature.S, Ed25519.Order);

    Result := True;
  finally
    Stream.Free;
    FBigNumberPool.Recycle(HP);
    FBigNumberPool.Recycle(K);
    FBigNumberPool.Recycle(S);
    FBigNumberPool.Recycle(R);
    if Is25519Nil then
      Ed25519.Free;
  end;
end;

function CnEd25519VerifyData(PlainData: Pointer; DataByteLen: Integer;
  InSignature: TCnEd25519Signature; PublicKey: TCnEd25519PublicKey; Ed25519: TCnEd25519): Boolean;
var
  Is25519Nil: Boolean;
  L, R, M: TCnEccPoint;
  T: TCnBigNumber;
  Stream: TMemoryStream;
  Data: TCnEd25519Data;
  Dig: TCnSHA512Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataByteLen <= 0) or (PublicKey = nil) or (InSignature = nil) then
    Exit;

  L := nil;
  R := nil;
  Stream := nil;
  T := nil;
  M := nil;
  Is25519Nil := Ed25519 = nil;

  try
    if Is25519Nil then
      Ed25519 := TCnEd25519.Create;

    // ��֤ 8*S*���� �Ƿ� = 8*R�� + 8*Hash(R32λ||��Կ��32λ||����) * ��Կ��
    L := TCnEccPoint.Create;
    R := TCnEccPoint.Create;

    L.Assign(Ed25519.Generator);
    Ed25519.MultiplePoint(InSignature.S, L);
    Ed25519.MultiplePoint(8, L);  // �㵽��ߵ�

    R.Assign(InSignature.R);
    Ed25519.MultiplePoint(8, R);  // �㵽 8*R�����

    Stream := TMemoryStream.Create;
    CnEd25519PointToData(InSignature.R, Data);
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));        // ƴ R ��

    CnEd25519PointToData(PublicKey, Data);
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));        // ƴ��Կ��
    Stream.Write(PlainData^, DataByteLen);                // ƴ����

    Dig := SHA512Buffer(Stream.Memory, Stream.Size);      // ���� Hash ��Ϊֵ
    ReverseMemory(@Dig[0], SizeOf(TCnSHA512Digest));      // ��Ҫ��תһ��

    T := FBigNumberPool.Obtain;
    T.SetBinary(@Dig[0], SizeOf(TCnSHA512Digest));
    T.MulWord(8);
    BigNumberNonNegativeMod(T, T, Ed25519.Order); // T ����̫���� mod һ�½�

    M := TCnEccPoint.Create;
    M.Assign(PublicKey);
    Ed25519.MultiplePoint(T, M);      // T �˹�Կ��
    Ed25519.PointAddPoint(R, M, R);   // ���

    Result := CnEccPointsEqual(L, R);
  finally
    M.Free;
    FBigNumberPool.Recycle(T);
    Stream.Free;
    R.Free;
    L.Free;
    if Is25519Nil then
      Ed25519.Free;
  end;
end;

function CnEd25519SignFile(const FileName: string; PrivateKey: TCnEd25519PrivateKey;
  PublicKey: TCnEd25519PublicKey; OutSignatureStream: TStream; Ed25519: TCnEd25519): Boolean;
var
  Stream: TMemoryStream;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) or (OutSignatureStream = nil)
    or not FileExists(FileName) then
    Exit;

  Stream := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    Sig := TCnEd25519Signature.Create;

    if CnEd25519SignData(Stream.Memory, Stream.Size, PrivateKey, PublicKey, Sig, Ed25519) then
    begin
      Sig.SaveToData(SigData);
      Result := OutSignatureStream.Write(SigData[0], SizeOf(TCnEd25519SignatureData))
        = SizeOf(TCnEd25519SignatureData);
    end;
  finally
    Sig.Free;
    Stream.Free;
  end;
end;

function CnEd25519VerifyFile(const FileName: string; InSignatureStream: TStream;
  PublicKey: TCnEd25519PublicKey; Ed25519: TCnEd25519): Boolean;
var
  Stream: TMemoryStream;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
begin
  Result := False;
  if (PublicKey = nil) or (InSignatureStream = nil) or not FileExists(FileName) then
    Exit;

  Stream := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    if InSignatureStream.Read(SigData[0], SizeOf(TCnEd25519SignatureData)) <>
      SizeOf(TCnEd25519SignatureData) then
      Exit;

    Sig := TCnEd25519Signature.Create;
    Sig.LoadFromData(SigData);

    Result := CnEd25519VerifyData(Stream.Memory, Stream.Size, Sig, PublicKey, Ed25519);
  finally
    Sig.Free;
    Stream.Free;
  end;
end;

function CnCurve25519KeyExchangeStep1(SelfPrivateKey: TCnEccPrivateKey;
  OutPointToAnother: TCnEccPoint; Curve25519: TCnCurve25519): Boolean;
var
  Is25519Nil: Boolean;
begin
  Result := False;
  if (SelfPrivateKey = nil) or (OutPointToAnother = nil) then
    Exit;

  Is25519Nil := Curve25519 = nil;

  try
    if Is25519Nil then
      Curve25519 := TCnCurve25519.Create;

    OutPointToAnother.Assign(Curve25519.Generator);
    Curve25519.MultiplePoint(SelfPrivateKey, OutPointToAnother);

    Result := True;
  finally
    if Is25519Nil then
      Curve25519.Free;
  end;
end;

function CnCurve25519KeyExchangeStep2(SelfPrivateKey: TCnEccPrivateKey;
  InPointFromAnother: TCnEccPoint; OutKey: TCnEccPoint; Curve25519: TCnCurve25519): Boolean;
var
  Is25519Nil: Boolean;
begin
  Result := False;
  if (SelfPrivateKey = nil) or (InPointFromAnother = nil) or (OutKey = nil) then
    Exit;

  Is25519Nil := Curve25519 = nil;

  try
    if Is25519Nil then
      Curve25519 := TCnCurve25519.Create;

    OutKey.Assign(InPointFromAnother);
    Curve25519.MultiplePoint(SelfPrivateKey, OutKey);

    Result := True;
  finally
    if Is25519Nil then
      Curve25519.Free;
  end;
end;

{ TCnEd25519Sigature }

procedure TCnEd25519Signature.Assign(Source: TPersistent);
begin
  if Source is TCnEd25519Signature then
  begin
    FR.Assign((Source as TCnEd25519Signature).R);
    BigNumberCopy(FS, (Source as TCnEd25519Signature).S);
  end
  else
    inherited;
end;

constructor TCnEd25519Signature.Create;
begin
  inherited;
  FR := TCnEccPoint.Create;
  FS := TCnBigNumber.Create;
end;

destructor TCnEd25519Signature.Destroy;
begin
  FS.Free;
  FR.Free;
  inherited;
end;

{ TCnEcc4Point }

procedure TCnEcc4Point.Assign(Source: TPersistent);
begin
  if Source is TCnEcc4Point then
    BigNumberCopy(FT, (Source as TCnEcc4Point).T);
  inherited;
end;

constructor TCnEcc4Point.Create;
begin
  inherited;
  FT := TCnBigNumber.Create;
end;

destructor TCnEcc4Point.Destroy;
begin
  FT.Free;
  inherited;
end;

procedure TCnEcc4Point.SetT(const Value: TCnBigNumber);
begin
  BigNumberCopy(FT, Value);
end;

function TCnEcc4Point.ToString: string;
begin
  Result := CnEcc4PointToHex(Self);
end;

procedure TCnEd25519Signature.LoadFromData(Sig: TCnEd25519SignatureData);
var
  Data: TCnEd25519Data;
  Ed25519: TCnEd25519;
begin
  Move(Sig[0], Data[0], SizeOf(TCnEd25519Data));

  // �� Data �м��� R ��
  Ed25519 := TCnEd25519.Create;
  try
    Ed25519.PlainToPoint(Data, FR);
  finally
    Ed25519.Free;
  end;

  Move(Sig[SizeOf(TCnEd25519Data)], Data[0], SizeOf(TCnEd25519Data));
  // �� Data �м��� S ��
  CnEd25519DataToBigNumber(Data, FS);
end;

procedure TCnEd25519Signature.LoadFromHex(const Hex: string);
var
  D: TCnEd25519SignatureData;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnEd25519Signature.SaveToData(var Sig: TCnEd25519SignatureData);
var
  Data: TCnEd25519Data;
begin
  FillChar(Sig[0], SizeOf(TCnEd25519SignatureData), 0);

  // �� R ��д�� Data
  CnEd25519PointToData(FR, Data);
  Move(Data[0], Sig[0], SizeOf(TCnEd25519Data));

  // �� S ��д�� Data
  CnEd25519BigNumberToData(FS, Data);
  Move(Data[0], Sig[SizeOf(TCnEd25519Data)], SizeOf(TCnEd25519Data));
end;


function TCnEd25519Signature.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnEd25519SignatureData;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

procedure Cn25519BigNumberToField64(var Field: TCn25519Field64; Num: TCnBigNumber);
var
  D: TCn25519Field64;
begin
  if Num.IsNegative or (BigNumberUnsignedCompare(Num, FPrime25519) > 0) then
    BigNumberNonNegativeMod(Num, Num, FPrime25519);

  // ��� Num �� SetHex 8888888877777777666666665555555544444444333333332222222211111111
  // ��ô����ʵֵȷʵ�� 8888888877777777666666665555555544444444333333332222222211111111
  // �ڴ��е͵����� 11111111 22222222 33333333 44444444 55555555 66666666 77777777 88888888
  // ���������ֽڣ�ÿ�����ֽ��ڲ�����С�˲�ͬ�����𣬵��������账��
  // ��� 64 λ��ֵ�� D0=2222222211111111 D1=4444444433333333 D3=6666666655555555 D4=8888888877777777

  FillChar(D[0], SizeOf(TCn25519Field64), 0);
  BigNumberRawDump(Num, @D[0]);

  Field[0] := D[0] and $7FFFFFFFFFFFF;  // D0 ������ 51 λ��0 �� 50����1��
  Field[1] := (D[0] shr 51) or ((D[1] and $3FFFFFFFFF) shl 13); // D0 �ĸ� 13 λ��64 �� 51���� D1 �ĵ� 38 λ����1��ƴ����
  Field[2] := (D[1] shr 38) or ((D[2] and $1FFFFFF) shl 26); // D1 �ĸ� 26 λ��64 �� 38���� D2 �ĵ� 25 λ����1��ƴ����
  Field[3] := (D[2] shr 25) or ((D[3] and $0FFF) shl 39); // D2 �ĸ� 39 λ��64 �� 25���� D2 �ĵ� 12 λ����1��ƴ����
  Field[4] := D[3] shr 12;                             // D3 �ĸ� 52 λ��64 �� 12��
end;

procedure Cn25519Field64ToBigNumber(Res: TCnBigNumber; var Field: TCn25519Field64);
var
  B0, B1, B2, B3, B4: TCnBigNumber;
begin
  B0 := nil;
  B1 := nil;
  B2 := nil;
  B3 := nil;
  B4 := nil;

  try
    B0 := FBigNumberPool.Obtain;
    B1 := FBigNumberPool.Obtain;
    B2 := FBigNumberPool.Obtain;
    B3 := FBigNumberPool.Obtain;
    B4 := FBigNumberPool.Obtain;

    B0.SetInt64(Field[0]);
    B1.SetInt64(Field[1]);
    B2.SetInt64(Field[2]);
    B3.SetInt64(Field[3]);
    B4.SetInt64(Field[4]);

    B1.ShiftLeft(51);
    B2.ShiftLeft(102);
    B3.ShiftLeft(153);
    B4.ShiftLeft(204);

    Res.SetZero;
    BigNumberAdd(Res, B1, B0);
    BigNumberAdd(Res, Res, B2);
    BigNumberAdd(Res, Res, B3);
    BigNumberAdd(Res, Res, B4);

    BigNumberNonNegativeMod(Res, Res, FPrime25519);
  finally
    FBigNumberPool.Recycle(B4);
    FBigNumberPool.Recycle(B3);
    FBigNumberPool.Recycle(B2);
    FBigNumberPool.Recycle(B1);
    FBigNumberPool.Recycle(B0);
  end;
end;

procedure Cn25519Field64Reduce(var Field: TCn25519Field64);
var
  C: TCn25519Field64;
begin
  C[0] := Field[0] shr 51;
  C[1] := Field[1] shr 51;
  C[2] := Field[2] shr 51;
  C[3] := Field[3] shr 51;
  C[4] := Field[4] shr 51;

  Field[0] := Field[0] and SCN_LOW51_MASK;
  Field[1] := Field[1] and SCN_LOW51_MASK;
  Field[2] := Field[2] and SCN_LOW51_MASK;
  Field[3] := Field[3] and SCN_LOW51_MASK;
  Field[4] := Field[4] and SCN_LOW51_MASK;

  Field[0] := Field[0] + C[4] * 19; // ���λ�Ľ�λ�� mod ��ʣ�µĸ������λ
  Field[1] := Field[1] + C[0];
  Field[2] := Field[2] + C[1];
  Field[3] := Field[3] + C[2];
  Field[4] := Field[4] + C[3];
end;

function Cn25519Field64ToHex(var Field: TCn25519Field64): string;
begin
  Result := '$' + UInt64ToHex(Field[0]) + ' $' + UInt64ToHex(Field[1]) + ' $' +
    UInt64ToHex(Field[2]) + ' $'+ UInt64ToHex(Field[3]) + ' $' + UInt64ToHex(Field[4]);
end;

procedure Cn25519Field64Copy(var Dest: TCn25519Field64; var Source: TCn25519Field64);
begin
  Move(Source[0], Dest[0], SizeOf(TCn25519Field64));
end;

function Cn25519Field64Equal(var A: TCn25519Field64; var B: TCn25519Field64): Boolean;
begin
  Result := (A[0] = B[0]) and (A[1] = B[1]) and (A[2] = B[2])
    and (A[3] = B[3]) and (A[4] = B[4]);
  // ֻ���б��Ӧֵ������ Reduce �ж�

//  if not Result then
//  begin
//    Cn25519Field64Copy(T1, A);
//    Cn25519Field64Copy(T2, B);
//
//    Cn25519Field64Reduce(T1);
//    Cn25519Field64Reduce(T2);
//    Result := (T1[0] = T2[0]) and (T1[1] = T2[1]) and (T1[2] = T2[2])
//      and (T1[3] = T2[3]) and (T1[4] = T2[4]);
//  end;
end;

procedure Cn25519Field64Swap(var A: TCn25519Field64; var B: TCn25519Field64);
var
  I: Integer;
  T: TUInt64;
begin
  for I := Low(TCn25519Field64) to High(TCn25519Field64) do
  begin
    T := A[I];
    A[I] := B[I];
    B[I] := T;
  end;
end;

procedure Cn25519Field64Zero(var Field: TCn25519Field64);
begin
  Move(F25519Field64Zero[0], Field[0], SizeOf(TCn25519Field64));
end;

procedure Cn25519Field64One(var Field: TCn25519Field64);
begin
  Move(F25519Field64One[0], Field[0], SizeOf(TCn25519Field64));
end;

procedure Cn25519Field64NegOne(var Field: TCn25519Field64);
begin
  Move(F25519Field64NegOne[0], Field[0], SizeOf(TCn25519Field64));
end;

{$WARNINGS OFF}

procedure Cn25519Field64Negate(var Field: TCn25519Field64);
begin
  Field[0] := 36028797018963664 - Field[0];
  Field[1] := 36028797018963952 - Field[1];
  Field[2] := 36028797018963952 - Field[2];
  Field[3] := 36028797018963952 - Field[3];
  Field[4] := 36028797018963952 - Field[4];
  Cn25519Field64Reduce(Field);
end;

{$WARNINGS ON}

procedure Cn25519Field64Add(var Res, A, B: TCn25519Field64);
var
  I: Integer;
begin
  for I := Low(TCn25519Field64) to High(TCn25519Field64) do
    Res[I] := A[I] + B[I];
end;

{$WARNINGS OFF}

procedure Cn25519Field64Sub(var Res, A, B: TCn25519Field64);
begin
  Res[0] := A[0] + 36028797018963664 - B[0];
  Res[1] := A[1] + 36028797018963952 - B[1];
  Res[2] := A[2] + 36028797018963952 - B[2];
  Res[3] := A[3] + 36028797018963952 - B[3];
  Res[4] := A[4] + 36028797018963952 - B[4];
  Cn25519Field64Reduce(Res);
end;

{$WARNINGS ON}

procedure Cn25519Field64Mul(var Res, A, B: TCn25519Field64);
var
  B1, B2, B3, B4, C: TUInt64;
  C0, C1, C2, C3, C4, T: TCnUInt128;
begin
  B1 := B[1] * 19;
  B2 := B[2] * 19;
  B3 := B[3] * 19;
  B4 := B[4] * 19;

  UInt128SetZero(C0);
  // c0 = m(a[0],b[0]) + m(a[4],b1_19) + m(a[3],b2_19) + m(a[2],b3_19) + m(a[1],b4_19);
  UInt64MulUInt64(A[0], B[0], T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[4], B1, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[3], B2, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[2], B3, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[1], B4, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);

  UInt128SetZero(C1);
  // c1 = m(a[1],b[0]) + m(a[0],b[1])  + m(a[4],b2_19) + m(a[3],b3_19) + m(a[2],b4_19);
  UInt64MulUInt64(A[1], B[0], T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[0], B[1], T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[4], B2, T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[3], B3, T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[2], B4, T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);

  UInt128SetZero(C2);
  // c2 = m(a[2],b[0]) + m(a[1],b[1])  + m(a[0],b[2])  + m(a[4],b3_19) + m(a[3],b4_19);
  UInt64MulUInt64(A[2], B[0], T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[1], B[1], T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[0], B[2], T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[4], B3, T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[3], B4, T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);

  UInt128SetZero(C3);
  // c3 = m(a[3],b[0]) + m(a[2],b[1])  + m(a[1],b[2])  + m(a[0],b[3])  + m(a[4],b4_19);
  UInt64MulUInt64(A[3], B[0], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[2], B[1], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[1], B[2], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[0], B[3], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[4], B4, T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);

  UInt128SetZero(C4);
  // c4 = m(a[4],b[0]) + m(a[3],b[1])  + m(a[2],b[2])  + m(a[1],b[3])  + m(a[0],b[4]);
  UInt64MulUInt64(A[4], B[0], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[3], B[1], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[2], B[2], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[1], B[3], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[0], B[4], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);

  // ƴ���
  UInt128Copy(T, C0);
  UInt128ShiftRight(T, 51);
  UInt128Add(C1, C1, T.Lo64);
  Res[0] := C0.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C1);
  UInt128ShiftRight(T, 51);
  UInt128Add(C2, C2, T.Lo64);
  Res[1] := C1.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C2);
  UInt128ShiftRight(T, 51);
  UInt128Add(C3, C3, T.Lo64);
  Res[2] := C2.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C3);
  UInt128ShiftRight(T, 51);
  UInt128Add(C4, C4, T.Lo64);
  Res[3] := C3.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C4);
  UInt128ShiftRight(T, 51);
  C := T.Lo64;
  Res[4] := C4.Lo64 and SCN_LOW51_MASK;

  Res[0] := Res[0] + C * 19;
  Res[1] := Res[1] + (Res[0] shr 51);

  Res[0] := Res[0] and SCN_LOW51_MASK;
end;

procedure Cn25519Field64Power(var Res, A: TCn25519Field64; K: Cardinal);
var
  T: TCn25519Field64;
begin
  if K = 0 then
    Cn25519Field64One(Res)
  else if K = 1 then
    Cn25519Field64Copy(Res, A)
  else
  begin
    Cn25519Field64Copy(T, A);
    Cn25519Field64One(Res);

    while K > 0 do
    begin
      if (K and 1) <> 0 then
        Cn25519Field64Mul(Res, Res, T);

      K := K shr 1;
      Cn25519Field64Mul(T, T, T);
    end;
  end;
end;

procedure Cn25519Field64Power(var Res, A: TCn25519Field64; K: TCnBigNumber);
var
  T: TCn25519Field64;
  I, B: Integer;
begin
  if K.IsZero then
    Cn25519Field64One(Res)
  else if K.IsOne then
    Cn25519Field64Copy(Res, A)
  else
  begin
    Cn25519Field64Copy(T, A);
    Cn25519Field64One(Res);

    B := K.GetBitsCount;
    for I := 0 to B - 1 do
    begin
      if K.IsBitSet(I) then
        Cn25519Field64Mul(Res, Res, T);
      Cn25519Field64Mul(T, T, T);
    end;
  end;
end;

procedure Cn25519Field64Power2K(var Res, A: TCn25519Field64; K: Cardinal);
begin
  Cn25519Field64Copy(Res, A);
  if K = 0 then
    Exit;

  while K > 0 do
  begin
    Cn25519Field64Mul(Res, Res, Res);
    Dec(K);
  end;
end;

procedure Cn25519Field64ModularInverse(var Res, A: TCn25519Field64);
var
  P: TCnBigNumber;
begin
  // �÷���С������ A �� P - 2 �η�
  P := FBigNumberPool.Obtain;
  try
    BigNumberCopy(P, FPrime25519);
    P.SubWord(2);

    Cn25519Field64Power(Res, A, P);
  finally
    FBigNumberPool.Recycle(P);
  end;
end;

// =========================== ����ʽ�㴦���� ================================

procedure Cn25519Field64EccPointZero(var Point: TCn25519Field64EccPoint);
begin
  Cn25519Field64Zero(Point.X);
  Cn25519Field64Zero(Point.Y);
end;

procedure Cn25519Field64EccPointCopy(var DestPoint, SourcePoint: TCn25519Field64EccPoint);
begin
  Cn25519Field64Copy(DestPoint.X, SourcePoint.X);
  Cn25519Field64Copy(DestPoint.Y, SourcePoint.Y);
end;

function Cn25519Field64EccPointToHex(var Point: TCn25519Field64EccPoint): string;
begin
  Result := 'X: ' + Cn25519Field64ToHex(Point.X) + ' Y: ' + Cn25519Field64ToHex(Point.Y);
end;

function Cn25519Field64EccPointEqual(var A, B: TCn25519Field64EccPoint): Boolean;
begin
  Result := Cn25519Field64Equal(A.X, B.X) and  Cn25519Field64Equal(A.Y, B.Y);
end;

procedure Cn25519Field64Ecc4PointNeutual(var Point: TCn25519Field64Ecc4Point);
begin
  Cn25519Field64Zero(Point.X);
  Cn25519Field64One(Point.Y);
  Cn25519Field64One(Point.Z);
  Cn25519Field64Zero(Point.T);
end;

procedure Cn25519Field64Ecc4PointCopy(var DestPoint, SourcePoint: TCn25519Field64Ecc4Point);
begin
  Cn25519Field64Copy(DestPoint.X, SourcePoint.X);
  Cn25519Field64Copy(DestPoint.Y, SourcePoint.Y);
  Cn25519Field64Copy(DestPoint.Z, SourcePoint.Z);
  Cn25519Field64Copy(DestPoint.T, SourcePoint.T);
end;

function Cn25519Field64Ecc4PointToHex(var Point: TCn25519Field64Ecc4Point): string;
begin
  Result := 'X: ' + Cn25519Field64ToHex(Point.X) + ' Y: ' + Cn25519Field64ToHex(Point.Y)
    + ' Z: ' + Cn25519Field64ToHex(Point.Z) + ' T: ' + Cn25519Field64ToHex(Point.T);
end;

function Cn25519Field64Ecc4PointEqual(var A, B: TCn25519Field64Ecc4Point): Boolean;
var
  T1, T2: TCn25519Field64;
begin
  // X1Z2 = X2Z1 �� Y1Z2 = Y2Z1
  Result := False;

  Cn25519Field64Mul(T1, A.X, B.Z);
  Cn25519Field64Mul(T2, B.X, A.Z);

  if not Cn25519Field64Equal(T1, T2) then
    Exit;

  Cn25519Field64Mul(T1, A.Y, B.Z);
  Cn25519Field64Mul(T2, B.Y, A.Z);

  if not Cn25519Field64Equal(T1, T2) then
    Exit;

  Result := True;
end;

function CnEccPointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEccPoint): Boolean;
var
  P4: TCnEcc4Point;
begin
  P4 := TCnEcc4Point.Create;
  try
    CnEccPointToEcc4Point(P4, SourcePoint, FPrime25519);
    Result := CnEcc4PointToField64Ecc4Point(DestPoint, P4);
  finally
    P4.Free;
  end;
end;

function CnField64Ecc4PointToEccPoint(DestPoint: TCnEccPoint;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
var
  P4: TCnEcc4Point;
begin
  P4 := TCnEcc4Point.Create;
  try
    CnField64Ecc4PointToEcc4Point(P4, SourcePoint);
    Result := CnEcc4PointToEccPoint(DestPoint, P4, FPrime25519);
  finally
    P4.Free;
  end;
end;

function CnEcc4PointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEcc4Point): Boolean;
begin
  Cn25519BigNumberToField64(DestPoint.X, SourcePoint.X);
  Cn25519BigNumberToField64(DestPoint.Y, SourcePoint.Y);
  Cn25519BigNumberToField64(DestPoint.Z, SourcePoint.Z);
  Cn25519BigNumberToField64(DestPoint.T, SourcePoint.T);
  Result := True;
end;

function CnField64Ecc4PointToEcc4Point(DestPoint: TCnEcc4Point;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
begin
  Cn25519Field64ToBigNumber(DestPoint.X, SourcePoint.X);
  Cn25519Field64ToBigNumber(DestPoint.Y, SourcePoint.Y);
  Cn25519Field64ToBigNumber(DestPoint.Z, SourcePoint.Z);
  Cn25519Field64ToBigNumber(DestPoint.T, SourcePoint.T);
  Result := True;
end;

{ TCnEd25519PrivateKey }

procedure TCnEd25519PrivateKey.LoadFromData(Data: TCnEd25519Data);
begin
  CnEd25519DataToBigNumber(Data, Self);
end;

procedure TCnEd25519PrivateKey.LoadFromHex(const Hex: string);
var
  D: TCnEd25519Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnEd25519PrivateKey.SaveToData(var Data: TCnEd25519Data);
begin
  CnEd25519BigNumberToData(Self, Data);
end;

function TCnEd25519PrivateKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnEd25519Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnEd25519PublicKey }

procedure TCnEd25519PublicKey.LoadFromData(Data: TCnEd25519Data);
var
  Ed25519: TCnEd25519;
begin
  Ed25519 := TCnEd25519.Create;
  try
    Ed25519.PlainToPoint(Data, Self); // �ڲ���� Data �м��� Y������ X ��ֵ
  finally
    Ed25519.Free;
  end;
end;

procedure TCnEd25519PublicKey.LoadFromHex(const Hex: string);
var
  D: TCnEd25519Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnEd25519PublicKey.SaveToData(var Data: TCnEd25519Data);
begin
  CnEd25519PointToData(Self, Data); // ֻ�� Y���Լ� X ����ż��
end;

function TCnEd25519PublicKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnEd25519Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnCurve448PrivateKey }

procedure TCnCurve448PrivateKey.LoadFromData(Data: TCnCurve448Data);
begin
  CnCurve448DataToBigNumber(Data, Self);
  CnProcessCurve448ScalarNumber(Self);
end;

procedure TCnCurve448PrivateKey.LoadFromHex(const Hex: string);
var
  D: TCnCurve448Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnCurve448PrivateKey.SaveToData(var Data: TCnCurve448Data);
begin
  CnProcessCurve448ScalarNumber(Self);
  CnCurve448BigNumberToData(Self, Data);
end;

function TCnCurve448PrivateKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnCurve448Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnCurve448PublicKey }

procedure TCnCurve448PublicKey.LoadFromData(Data: TCnCurve448Data);
begin
  CnCurve448DataToPoint(Data, Self);
end;

procedure TCnCurve448PublicKey.LoadFromHex(const Hex: string);
var
  D: TCnCurve448Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnCurve448PublicKey.SaveToData(var Data: TCnCurve448Data);
begin
  CnCurve448PointToData(Self, Data);
end;

function TCnCurve448PublicKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnCurve448Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnCurve448 }

constructor TCnCurve448.Create;
begin
  inherited;
  Load(SCN_448_MONT_A, SCN_448_MONT_B, SCN_448_PRIME, SCN_448_MONT_GX,
    SCN_448_MONT_GY, SCN_448_ORDER, SCN_448_COFACTOR);
end;

function TCnCurve448.GenerateKeys(PrivateKey: TCnCurve448PrivateKey;
  PublicKey: TCnCurve448PublicKey): Boolean;
begin
  Result := False;
  if not BigNumberRandRange(PrivateKey, FOrder) then  // �� 0 �󵫱Ȼ����С�������
    Exit;

  if PrivateKey.IsZero then                           // ��һ���õ� 0������Ϊ 8
    PrivateKey.SetWord(8);

  CnProcessCurve448ScalarNumber(PrivateKey);          // �� RFC �涨����˽Կ

  PublicKey.Assign(FGenerator);
  MultiplePoint(PrivateKey, PublicKey);               // ����� PrivateKey ��
  Result := True;
end;

procedure TCnCurve448.MultiplePoint(K: TCnBigNumber; P: TCnEccPoint);
var
  T: TCnEccPoint;
begin
  T := TCnEccPoint.Create;   // ע�� 448 ������ 2^51 �Ķ���ʽ�����㷨
  try
    PointToXAffinePoint(T, P);
    MontgomeryLadderMultiplePoint(K, T);
    XAffinePointToPoint(P, T);
  finally
    T.Free;
  end;
end;

{ TCnEd448PrivateKey }

procedure TCnEd448PrivateKey.LoadFromData(Data: TCnEd448Data);
begin
  CnEd448DataToBigNumber(Data, Self);
end;

procedure TCnEd448PrivateKey.LoadFromHex(const Hex: string);
var
  D: TCnEd448Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnEd448PrivateKey.SaveToData(var Data: TCnEd448Data);
begin
  CnEd448BigNumberToData(Self, Data);
end;

function TCnEd448PrivateKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnEd448Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnEd448PublicKey }

procedure TCnEd448PublicKey.LoadFromData(Data: TCnEd448Data);
var
  Ed448: TCnEd448;
begin
  Ed448 := TCnEd448.Create;
  try
    Ed448.PlainToPoint(Data, Self); // �ڲ���� Data �м��� Y������ X ��ֵ
  finally
    Ed448.Free;
  end;
end;

procedure TCnEd448PublicKey.LoadFromHex(const Hex: string);
var
  D: TCnEd448Data;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnEd448PublicKey.SaveToData(var Data: TCnEd448Data);
begin
  CnEd448PointToData(Self, Data); // ֻ�� Y���Լ� X ����ż��
end;

function TCnEd448PublicKey.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnEd448Data;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

{ TCnEd448 }

procedure TCnEd448.AffineMultiplePoint(K: Int64; P: TCnEcc3Point);
var
  BK: TCnBigNumber;
begin
  BK := FBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    AffineMultiplePoint(BK, P);
  finally
    FBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnEd448.AffineMultiplePoint(K: TCnBigNumber; P: TCnEcc3Point);
var
  I, C: Integer;
  E, R: TCnEcc3Point;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    AffinePointInverse(P);
  end;

  if BigNumberIsZero(K) then
  begin
    SetNeutualAffinePoint(P);
    Exit;
  end
  else if BigNumberIsOne(K) then // �� 1 ���趯
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEcc3Point.Create;
    E := TCnEcc3Point.Create;

    // R Ҫ�����Ե�
    SetNeutualAffinePoint(R);

    E.X := P.X;
    E.Y := P.Y;
    E.Z := P.Z;

    C := BigNumberGetBitsCount(K);
    for I := 0 to C - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        AffinePointAddPoint(R, E, R);

      if I < C - 1 then
        AffinePointAddPoint(E, E, E);
    end;

    P.X := R.X;
    P.Y := R.Y;
    P.Z := R.Z;
  finally
    R.Free;
    E.Free;
  end;
end;

procedure TCnEd448.AffinePointAddPoint(P, Q, Sum: TCnEcc3Point);
var
  A, B, C, D, E, F, G, H, J: TCnBigNumber;
begin
{
    RFC 8032 ����Ԫ�����㷨��Ҫ�󷽳��еĲ��� A Ϊ 1

    ��ͬ���                 ͬ�㱶��

    A = Z1*Z2                B = (X1+Y1)^2
    B = A^2                  C = X1^2
    C = X1*X2                D = Y1^2
    D = Y1*Y2                E = C+D
    E = d*C*D                H = Z1^2
    F = B-E                  J = E-2*H
    G = B+E
    H = (X1+Y1)*(X2+Y2)

    X3 = A*F*(H-C-D)         X3 = (B-E)*J
    Y3 = A*G*(D-C)           Y3 = E*(C-D)
    Z3 = F*G                 Z3 = E*J
}

  A := nil;
  B := nil;
  C := nil;
  D := nil;
  E := nil;
  F := nil;
  G := nil;
  H := nil;
  J := nil;

  try
    A := FBigNumberPool.Obtain;
    B := FBigNumberPool.Obtain;
    C := FBigNumberPool.Obtain;
    D := FBigNumberPool.Obtain;
    E := FBigNumberPool.Obtain;
    F := FBigNumberPool.Obtain;
    G := FBigNumberPool.Obtain;
    H := FBigNumberPool.Obtain;
    J := FBigNumberPool.Obtain;

    if CnAffineEcc3PointEqual(P, Q, FFiniteFieldSize) then
    begin
      // ��ͬһ����
      BigNumberAddMod(B, P.X, P.Y, FFiniteFieldSize);
      BigNumberDirectMulMod(B, B, B, FFiniteFieldSize);      // B = (X + Y)^2

      BigNumberDirectMulMod(C, P.X, P.X, FFiniteFieldSize);  // C = X^2
      BigNumberDirectMulMod(D, P.Y, P.Y, FFiniteFieldSize);  // D = Y^2
      BigNumberAddMod(E, C, D, FFiniteFieldSize);            // E = C + D
      BigNumberDirectMulMod(H, P.Z, P.Z, FFiniteFieldSize);  // H = Z1^2

      BigNumberAddMod(H, H, H, FFiniteFieldSize);            // H = 2 * H
      BigNumberSubMod(J, E, H, FFiniteFieldSize);            // J = E - 2 * H

      BigNumberSubMod(B, B, E, FFiniteFieldSize);            // B �����仯�����治�� B ��
      BigNumberDirectMulMod(Sum.X, B, J, FFiniteFieldSize);  // X3 = (B - E) * J

      BigNumberSubMod(C, C, D, FFiniteFieldSize);            // C �����仯�����治�� C ��
      BigNumberDirectMulMod(Sum.Y, E, C, FFiniteFieldSize);  // Y3 = E * (C - D)

      BigNumberDirectMulMod(Sum.Z, E, J, FFiniteFieldSize);  // Z3 = E * J
    end
    else
    begin
      // ����ͬһ����
      BigNumberDirectMulMod(A, P.Z, Q.Z, FFiniteFieldSize);  // A = Z1 * Z2
      BigNumberDirectMulMod(B, A, A, FFiniteFieldSize);      // B = A^2
      BigNumberDirectMulMod(C, P.X, Q.X, FFiniteFieldSize);  // C = X1 * X2
      BigNumberDirectMulMod(D, P.Y, Q.Y, FFiniteFieldSize);  // D = Y1 * Y2

      BigNumberDirectMulMod(E, FCoefficientD, C, FFiniteFieldSize);
      BigNumberDirectMulMod(E, E, D, FFiniteFieldSize);      // E = d*C*D

      BigNumberSubMod(F, B, E, FFiniteFieldSize);            // F = B - E
      BigNumberAddMod(G, B, E, FFiniteFieldSize);            // G = B + E

      // ���� E �ò��ţ��� J һ������ʱ����
      BigNumberAddMod(J, Q.X, Q.Y, FFiniteFieldSize);        // J = X2 + Y2
      BigNumberAddMod(E, P.X, P.Y, FFiniteFieldSize);        // E = X1 + Y1
      BigNumberDirectMulMod(H, E, J, FFiniteFieldSize);      // H = (X1 + Y1) * (X2 + Y2)

      BigNumberSubMod(H, H, C, FFiniteFieldSize);
      BigNumberSubMod(H, H, D, FFiniteFieldSize);            // H �����仯�����治�� H ��
      BigNumberDirectMulMod(H, H, A, FFiniteFieldSize);
      BigNumberDirectMulMod(Sum.X, H, F, FFiniteFieldSize);  // X3 = A*F*(H-C-D)

      BigNumberSubMod(D, D, C, FFiniteFieldSize);            // D �����仯�����治�� D ��
      BigNumberDirectMulMod(D, D, A, FFiniteFieldSize);
      BigNumberDirectMulMod(Sum.Y, D, G, FFiniteFieldSize);  // Y3 = A*G*(D-C)

      BigNumberDirectMulMod(Sum.Z, F, G, FFiniteFieldSize);  // Z3 = F*G
    end;
  finally
    FBigNumberPool.Recycle(J);
    FBigNumberPool.Recycle(H);
    FBigNumberPool.Recycle(G);
    FBigNumberPool.Recycle(F);
    FBigNumberPool.Recycle(E);
    FBigNumberPool.Recycle(D);
    FBigNumberPool.Recycle(C);
    FBigNumberPool.Recycle(B);
    FBigNumberPool.Recycle(A);
  end;
end;

procedure TCnEd448.AffinePointInverse(P: TCnEcc3Point);
var
  T: TCnBigNumber;
begin
  T := FBigNumberPool.Obtain;
  try
    // x -> -x����ζ�� X/Z -> P - X/Z��Ҳ���� (P*Z - X)/Z�������� X = P*Z - X��ǰ���� 0��������� P - X
    BigNumberDirectMulMod(T, P.Z, FFiniteFieldSize, FFiniteFieldSize);
    BigNumberSubMod(P.X, T, P.X, FFiniteFieldSize); // �ͷ� T
  finally
    FBigNumberPool.Recycle(T);
  end;
end;

procedure TCnEd448.AffinePointSubPoint(P, Q, Diff: TCnEcc3Point);
var
  Inv: TCnEcc3Point;
begin
  Inv := TCnEcc3Point.Create;
  try
    Inv.Assign(Q);
    AffinePointInverse(Inv);
    AffinePointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

constructor TCnEd448.Create;
begin
  inherited;
  Load(SCN_448_EDWARDS_A, SCN_448_EDWARDS_D, SCN_448_PRIME, SCN_448_EDWARDS_GX,
    SCN_448_EDWARDS_GY, SCN_448_ORDER, SCN_448_COFACTOR);
end;

function TCnEd448.GenerateKeys(PrivateKey: TCnEd448PrivateKey;
  PublicKey: TCnEd448PublicKey): Boolean;
var
  K: TCnBigNumber;
begin
  Result := False;

  // ��� 57 �ֽ��� PrivateKey
  if not BigNumberRandBytes(PrivateKey, CN_448_EDWARDS_BLOCK_BYTESIZE) then
    Exit;

  K := FBigNumberPool.Obtain;
  try
    CnCalcKeysFromEd448PrivateKey(PrivateKey, K, nil);

    // �ó��� K ���� G ��õ���Կ
    PublicKey.Assign(FGenerator);
    MultiplePoint(K, PublicKey);                         // ����� K ��

    Result := True;
  finally
    FBigNumberPool.Recycle(K);
  end;
end;

function TCnEd448.IsAffinePointOnCurve(P: TCnEcc3Point): Boolean;
var
  Q: TCnEccPoint;
begin
  Q := TCnEccPoint.Create;
  try
    CnAffinePointToEccPoint(P, Q, FFiniteFieldSize);
    Result := IsPointOnCurve(Q);
  finally
    Q.Free;
  end;
end;

function TCnEd448.IsNeutualAffinePoint(P: TCnEcc3Point): Boolean;
begin
  Result := P.X.IsZero and not P.Y.IsZero and not P.Z.IsZero
    and BigNumberEqual(P.Y, P.Z);
end;

procedure TCnEd448.MultiplePoint(K: TCnBigNumber; P: TCnEccPoint);
var
  P3: TCnEcc3Point;
begin
  P3 := TCnEcc3Point.Create;
  try
    CnEccPointToEcc3Point(P, P3);
    AffineMultiplePoint(K, P3);
    CnAffinePointToEccPoint(P3, P, FPrime448);
  finally
    P3.Free;
  end;
end;

procedure TCnEd448.PlainToPoint(Plain: TCnEd448Data; OutPoint: TCnEccPoint);
var
  XOdd: Boolean;
begin
  if OutPoint = nil then
    Exit;

  // �ȴ� Plain �л�ԭ Y �����Լ� X �����ż��
  CnEd448DataToPoint(Plain, OutPoint, XOdd);

  // ����� X ����
  if not CalcXFromY(OutPoint.Y, OutPoint.X, XOdd) then
    raise ECnEccException.Create(SCnErrorPointNotOnCurve);
end;

procedure TCnEd448.PointToPlain(Point: TCnEccPoint; var OutPlain: TCnEd448Data);
begin
  if (Point = nil) or (BigNumberCompare(Point.Y, FFiniteFieldSize) >= 0) then
    Exit;

  CnEd448PointToData(Point, OutPlain);
end;

procedure TCnEd448.SetNeutualAffinePoint(P: TCnEcc3Point);
begin
  P.X.SetZero;
  P.Y.SetOne;
  P.Z.SetOne;
end;

{ TCnEd448Signature }

procedure TCnEd448Signature.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TCnEd448Signature.Create;
begin
  inherited;
  FR := TCnEccPoint.Create;
  FS := TCnBigNumber.Create;
end;

destructor TCnEd448Signature.Destroy;
begin
  FS.Free;
  FR.Free;
  inherited;
end;

procedure TCnEd448Signature.LoadFromData(Sig: TCnEd448SignatureData);
var
  Data: TCnEd448Data;
  Ed448: TCnEd448;
begin
  Move(Sig[0], Data[0], SizeOf(TCnEd448Data));

  // �� Data �м��� R ��
  Ed448 := TCnEd448.Create;
  try
    Ed448.PlainToPoint(Data, FR);
  finally
    Ed448.Free;
  end;

  Move(Sig[SizeOf(TCnEd448Data)], Data[0], SizeOf(TCnEd448Data));
  // �� Data �м��� S ��
  CnEd448DataToBigNumber(Data, FS);
end;

procedure TCnEd448Signature.LoadFromHex(const Hex: string);
var
  D: TCnEd448SignatureData;
begin
  if HexToData(Hex) <> SizeOf(D) then
    raise ECnEccException.Create(SCnErrorInvalidHexLength);

  HexToData(Hex, @D[0]);
  LoadFromData(D);
end;

procedure TCnEd448Signature.SaveToData(var Sig: TCnEd448SignatureData);
var
  Data: TCnEd448Data;
begin
  FillChar(Sig[0], SizeOf(TCnEd448SignatureData), 0);

  // �� R ��д�� Data
  CnEd448PointToData(FR, Data);
  Move(Data[0], Sig[0], SizeOf(TCnEd448Data));

  // �� S ��д�� Data
  CnEd448BigNumberToData(FS, Data);
  Move(Data[0], Sig[SizeOf(TCnEd448Data)], SizeOf(TCnEd448Data));
end;

function TCnEd448Signature.SaveToHex(UseUpperCase: Boolean): string;
var
  D: TCnEd448SignatureData;
begin
  SaveToData(D);
  Result := DataToHex(@D[0], SizeOf(D), UseUpperCase);
end;

initialization
  FBigNumberPool := TCnBigNumberPool.Create;
  FPrime25519 := TCnBigNumber.FromHex(SCN_25519_PRIME);
  FPrime448 := TCnBigNumber.FromHex(SCN_448_PRIME);

finalization
  FPrime448.Free;
  FPrime25519.Free;
  FBigNumberPool.Free;

end.
