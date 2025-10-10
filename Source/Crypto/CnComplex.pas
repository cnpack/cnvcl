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

unit CnComplex;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����㸴������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ������չ���ȸ������ĸ����ṹ TCnComplexNumber ����������㡣
*           Ϊ���Ч�ʣ�ʹ�� record ������ TObject��
*
*           Ҳʵ���˻��ڴ������ĸ����࣬ע�ⲻ֧�ֳ�������ֵ����Ҫ��������ĳ��ϡ�
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.06.26 V1.1
*               ���ӷ��������ֵ�Ⱥ���
*           2020.11.20 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, SysConst, Math, CnMath, CnBigNumber;

type
  ECnComplexNumberException = class(Exception);
  {* ������ص��쳣}

  TCnComplexNumber = packed record
  {* ���㾫�ȵĸ�����ʾ�ṹ}
    R: Extended;
    {* ʵ��}
    I: Extended;
    {* �鲿}
  end;
  PCnComplexNumber = ^TCnComplexNumber;
  {* ָ�����ṹ��ָ��}

  TCnComplexArray = array[0..8191] of TCnComplexNumber;
  {* �����ṹ����}

  PCnComplexArray = ^TCnComplexArray;
  {* ָ�����ṹ�����ָ��}

  TCnBigComplexNumber = class(TObject)
  {* ʵ���鲿��Ϊ�������ĸ�����}
  private
    FR: TCnBigNumber;
    FI: TCnBigNumber;
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    property R: TCnBigNumber read FR;
    {* ʵ��}
    property I: TCnBigNumber read FI;
    {* �鲿}
  end;

// ======================== ���㾫�ȵĸ������� =================================

function ComplexNumberIsZero(var Complex: TCnComplexNumber): Boolean;
{* ���ظ����Ƿ�Ϊ 0��

   ������
     var Complex: TCnComplexNumber        - ���жϵĸ���

   ����ֵ��Boolean                        - �����Ƿ���� 0
}

procedure ComplexNumberSetZero(var Complex: TCnComplexNumber);
{* ������ 0��

   ������
     var Complex: TCnComplexNumber        - �����õĸ���

   ����ֵ�����ޣ�
}

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  AR: Extended; AI: Extended); overload;
{* ������ֵ��

   ������
     var Complex: TCnComplexNumber        - ����ֵ�ĸ���
     AR: Extended                         - ������ʵ��
     AI: Extended                         - �������鲿

   ����ֵ�����ޣ�
}

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  const AR: string; const AI: string); overload;
{* ������ֵ��

   ������
     var Complex: TCnComplexNumber        - ����ֵ�ĸ���
     const AR: string                     - ʵ���ĸ����ַ�����ʽ
     const AI: string                     - �鲿�ĸ����ַ�����ʽ

   ����ֵ�����ޣ�
}

function ComplexNumberToString(var Complex: TCnComplexNumber): string;
{* ����ת��Ϊ���� a + bi ���ַ�����ʵ���鲿���� 0 ���Ӧʡ�ԡ�

   ������
     var Complex: TCnComplexNumber        - ��ת���ĸ���

   ����ֵ��string                         - ���ظ������ַ�����ʽ
}

function ComplexNumberEqual(var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber): Boolean;
{* �ж���������ֵ�Ƿ���ȡ�

   ������
     var Complex1: TCnComplexNumber       - ���Ƚϵĸ���һ
     var Complex2: TCnComplexNumber       - ���Ƚϵĸ�����

   ����ֵ��Boolean                        - ������������ֵ�Ƿ����
}

procedure ComplexNumberSwap(var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber);
{* ��������������ֵ��

   ������
     var Complex1: TCnComplexNumber       - �������ĸ���һ
     var Complex2: TCnComplexNumber       - �������ĸ�����

   ����ֵ�����ޣ�
}

procedure ComplexNumberCopy(var Dst: TCnComplexNumber; var Src: TCnComplexNumber);
{* ���Ƹ�����ֵ��

   ������
     var Dst: TCnComplexNumber            - Ŀ�긴��
     var Src: TCnComplexNumber            - Դ����

   ����ֵ�����ޣ�
}

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* �����ӷ���Complex1 �� Complex2 ������ͬһ���ṹ��Res ������ Complex1 �� Complex2��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex1: TCnComplexNumber       - ��������һ
     var Complex2: TCnComplexNumber       - ����������

   ����ֵ�����ޣ�
}

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* ����������Complex1 �� Complex2 ������ͬһ���ṹ��Res ������ Complex1 �� Complex2��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex1: TCnComplexNumber       - ����������
     var Complex2: TCnComplexNumber       - ��������

   ����ֵ�����ޣ�
}

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* �����˷���Complex1 �� Complex2 ������ͬһ���ṹ��Res ������ Complex1 �� Complex2��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex1: TCnComplexNumber       - ��������һ
     var Complex2: TCnComplexNumber       - ����������

   ����ֵ�����ޣ�
}

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* ����������Complex1 �� Complex2 ������ͬһ���ṹ��Res ������ Complex1 �� Complex2��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex1: TCnComplexNumber       - ����������
     var Complex2: TCnComplexNumber       - ��������

   ����ֵ�����ޣ�
}

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* �����븡�����ļӷ���Complex �� Res ������ͬһ���ṹ��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex: TCnComplexNumber        - ��������
     Value: Extended                      - ����������

   ����ֵ�����ޣ�
}

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* �����븡�����ļ�����Complex �� Res ������ͬһ���ṹ��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex: TCnComplexNumber        - ����������
     Value: Extended                      - ����������

   ����ֵ�����ޣ�
}

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* �����븡�����ĳ˷���Complex �� Res ������ͬһ���ṹ��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex: TCnComplexNumber        - ��������
     Value: Extended                      - ����������

   ����ֵ�����ޣ�
}

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* �����븡�����ĳ�����Complex �� Res ������ͬһ���ṹ��

   ������
     var Res: TCnComplexNumber            - ������
     var Complex: TCnComplexNumber        - ����������
     Value: Extended                      - �������

   ����ֵ�����ޣ�
}

procedure ComplexNumberSqrt(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
{* ������ƽ������ֻ��������һ���������Ҫ��һ����ʵ���鲿��ȡ�����С�

   ������
     var Res: TCnComplexNumber            - ����ƽ�������
     var Complex: TCnComplexNumber        - ����ƽ�����ĸ���

   ����ֵ�����ޣ�
}

procedure ComplexConjugate(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
{* ��ù������Res ������ Complex��

   ������
     var Res: TCnComplexNumber            - �����Ĺ�����
     var Complex: TCnComplexNumber        - ������ĸ���

   ����ֵ�����ޣ�
}

function ComplexIsPureReal(var Complex: TCnComplexNumber): Boolean;
{* �����Ƿ�ʵ����Ҳ�����ж��鲿�Ƿ�Ϊ 0��

   ������
     var Complex: TCnComplexNumber        - ���жϵĸ���

   ����ֵ��Boolean                        - �����Ƿ�ʵ��
}

function ComplexIsPureImaginary(var Complex: TCnComplexNumber): Boolean;
{* �����Ƿ�������Ҳ�����ж�ʵ���Ƿ�Ϊ 0 ���鲿��Ϊ 0��

   ������
     var Complex: TCnComplexNumber        - ���жϵĸ���

   ����ֵ��Boolean                        - �����Ƿ�����
}

function ComplexNumberAbsolute(var Complex: TCnComplexNumber): Extended;
{* ���ظ����ľ���ֵ��Ҳ���ิƽ��ԭ��ľ��롣

   ������
     var Complex: TCnComplexNumber        - ������ĸ���

   ����ֵ��Extended                       - ���ظ����ľ���ֵ
}

function ComplexNumberArgument(var Complex: TCnComplexNumber): Extended;
{* ���ظ����ķ�����ֵ��Ҳ���븴ƽ���� X ��ļнǣ���Χ�� 0 �� 2�С�

   ������
     var Complex: TCnComplexNumber        - ������ĸ���

   ����ֵ��Extended                       - ���ظ����ķ�����ֵ����λΪ����
}

procedure ComplexNumberSetAbsoluteArgument(var Complex: TCnComplexNumber;
  AnAbsolute: Extended; AnArgument: Extended);
{* ����һ�����ľ���ֵ�����ֵ��

   ������
     var Complex: TCnComplexNumber        - �����õĸ���
     AnAbsolute: Extended                 - �����õľ���ֵ
     AnArgument: Extended                 - �����õķ���ֵ

   ����ֵ�����ޣ�
}

// ========================== �������ĸ������� =================================

function BigComplexNumberIsZero(Complex: TCnBigComplexNumber): Boolean;
{* ���ش����������Ƿ�Ϊ 0��

   ������
     Complex: TCnBigComplexNumber         - ���жϵĴ���������

   ����ֵ��Boolean                        - �����Ƿ���� 0
}

procedure BigComplexNumberSetZero(Complex: TCnBigComplexNumber);
{* ������������ 0��

   ������
     Complex: TCnBigComplexNumber         - �����õĴ���������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberSetValue(Complex: TCnBigComplexNumber;
  AR: Int64; AI: Int64); overload;
{* ������������ֵ��

   ������
     Complex: TCnBigComplexNumber         - ����ֵ�Ĵ���������
     AR: Int64                            - ������������ʵ��
     AI: Int64                            - �������������鲿

   ����ֵ�����ޣ�
}

procedure BigComplexNumberSetValue(Complex: TCnBigComplexNumber;
  const AR: string; const AI: string); overload;
{* ������������ֵ��

   ������
     Complex: TCnBigComplexNumber         - ����ֵ�Ĵ���������
     const AR: string                     - ʵ����ʮ���������ַ�����ʽ
     const AI: string                     - �鲿��ʮ���������ַ�����ʽ

   ����ֵ�����ޣ�
}

function BigComplexNumberToString(Complex: TCnBigComplexNumber): string;
{* ����������ת��Ϊ���� a + bi ���ַ�����ʵ���鲿���� 0 ���Ӧʡ�ԡ�

   ������
     Complex: TCnBigComplexNumber         - ��ת���Ĵ���������

   ����ֵ��string                         - ���ش������������ַ�����ʽ
}

function BigComplexNumberEqual(Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber): Boolean;
{* �ж���������������ֵ�Ƿ���ȡ�

   ������
     Complex1: TCnBigComplexNumber        - ���ȽϵĴ���������һ
     Complex2: TCnBigComplexNumber        - ���ȽϵĴ�����������

   ����ֵ��Boolean                        - ������������������ֵ�Ƿ����
}

procedure BigComplexNumberSwap(Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber);
{* ��������������������ֵ��

   ������
     Complex1: TCnBigComplexNumber        - �������Ĵ���������һ
     Complex2: TCnBigComplexNumber        - �������Ĵ�����������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberCopy(Dst: TCnBigComplexNumber; Src: TCnBigComplexNumber);
{* ���ƴ�����������ֵ��

   ������
     Dst: TCnBigComplexNumber             - Ŀ�����������
     Src: TCnBigComplexNumber             - Դ����������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberAdd(Res: TCnBigComplexNumber;
  Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber); overload;
{* �����������ӷ���Complex1 �� Complex2 ������ͬһ������Res ������ Complex1 �� Complex2��

   ������
     Res: TCnBigComplexNumber             - ������������
     Complex1: TCnBigComplexNumber        - ��������������һ
     Complex2: TCnBigComplexNumber        - ����������������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberSub(Res: TCnBigComplexNumber;
  Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber); overload;
{* ����������������Complex1 �� Complex2 ������ͬһ������Res ������ Complex1 �� Complex2��

   ������
     Res: TCnBigComplexNumber             - ������������
     Complex1: TCnBigComplexNumber        - ����������������
     Complex2: TCnBigComplexNumber        - ��������������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberMul(Res: TCnBigComplexNumber;
  Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber); overload;
{* �����������˷���Complex1 �� Complex2 ������ͬһ������Res ������ Complex1 �� Complex2��

   ������
     Res: TCnBigComplexNumber             - ������������
     Complex1: TCnBigComplexNumber        - ��������������һ
     Complex2: TCnBigComplexNumber        - ����������������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberAdd(Res: TCnBigComplexNumber;
  Complex: TCnBigComplexNumber; Value: Int64); overload;
{* �����������������ļӷ���Complex �� Res ������ͬһ������

   ������
     Res: TCnBigComplexNumber             - ������������
     Complex: TCnBigComplexNumber         - ��������������
     Value: Int64                         - ��������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberSub(Res: TCnBigComplexNumber;
  Complex: TCnBigComplexNumber; Value: Int64); overload;
{* �����������������ļ�����Complex �� Res ������ͬһ������

   ������
     Res: TCnBigComplexNumber             - ������������
     Complex: TCnBigComplexNumber         - ����������������
     Value: Int64                         - ��������

   ����ֵ�����ޣ�
}

procedure BigComplexNumberMul(Res: TCnBigComplexNumber;
  Complex: TCnBigComplexNumber; Value: Int64); overload;
{* �������������������ĳ˷���Complex �� Res ������ͬһ������

   ������
     Res: TCnBigComplexNumber             - ������������
     Complex: TCnBigComplexNumber         - ��������������
     Value: Int64                         - ��������

   ����ֵ�����ޣ�
}

procedure BigComplexConjugate(Res: TCnBigComplexNumber; Complex: TCnBigComplexNumber);
{* ��ù��������������Res ������ Complex��

   ������
     Res: TCnBigComplexNumber            - �����������Ĺ�����
     Complex: TCnBigComplexNumber        - ������Ĵ���������

   ����ֵ�����ޣ�
}

function BigComplexIsPureReal(Complex: TCnBigComplexNumber): Boolean;
{* �����������Ƿ�ʵ����Ҳ�����ж��鲿�Ƿ�Ϊ 0��

   ������
     Complex: TCnBigComplexNumber         - ���жϵĴ���������

   ����ֵ��Boolean                        - �����Ƿ�ʵ��
}

function BigComplexIsPureImaginary(Complex: TCnBigComplexNumber): Boolean;
{* �����������Ƿ�������Ҳ�����ж�ʵ���Ƿ�Ϊ 0 ���鲿��Ϊ 0��

   ������
     Complex: TCnBigComplexNumber         - ���жϵĴ���������

   ����ֵ��Boolean                        - �����Ƿ�����
}

function BigComplexNumberAbsolute(Complex: TCnBigComplexNumber): Extended; overload;
{* ���ش����������ľ���ֵ��Ҳ���ิƽ��ԭ��ľ��룬�Ը�������ʾ��

   ������
     Complex: TCnBigComplexNumber         - ������Ĵ���������

   ����ֵ��Extended                       - ���ش����������ľ���ֵ
}

function BigComplexNumberAbsolute(Res: TCnBigNumber; Complex: TCnBigComplexNumber): Boolean; overload;
{* ���ش����������ľ���ֵ��Ҳ���ิƽ��ԭ��ľ��룬�Դ�������ʾ��

   ������
     Res: TCnBigComplexNumber             - �������ɽ���Ĵ���������
     Complex: TCnBigComplexNumber         - ������Ĵ���������

   ����ֵ��Boolean                        - �����Ƿ���ֵ�ɹ�
}

function BigComplexNumberArgument(Complex: TCnBigComplexNumber): Extended;
{* ���ش����������ķ�����ֵ��Ҳ���븴ƽ���� X ��ļнǣ���Χ�� 0 �� 2�С�

   ������
     Complex: TCnBigComplexNumber         - ������Ĵ���������

   ����ֵ��Extended                       - ���ش����������ķ�����ֵ����λΪ����
}

var
  CnComplexZero: TCnComplexNumber;
  {* ���� 0}

  CnComplexOne: TCnComplexNumber;
  {* ���� 1}

  CnComplexOneI: TCnComplexNumber;
  {* ���� i}

  CnComplexNegOneI: TCnComplexNumber;
  {* ���� -i}

  CnBigComplexNumberZero: TCnBigComplexNumber;
  {* ���� 0}

  CnBigComplexNumberOne: TCnBigComplexNumber;
  {* ���� 1}

  CnBigComplexNumberOneI: TCnBigComplexNumber;
  {* ���� i}

  CnBigComplexNumberNegOneI: TCnBigComplexNumber;
  {* ���� -i}

implementation

function ComplexNumberIsZero(var Complex: TCnComplexNumber): Boolean;
begin
  Result := (Complex.R = 0) and (Complex.I = 0);
end;

procedure ComplexNumberSetZero(var Complex: TCnComplexNumber);
begin
  Complex.R := 0.0;
  Complex.I := 0.0;
end;

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber; AR, AI: Extended);
begin
  Complex.R := AR;
  Complex.I := AI;
end;

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  const AR, AI: string);
begin
  ComplexNumberSetZero(Complex);
  if (AR = '') and (AI = '') then
    Exit
  else if AR = '' then
    Complex.I := StrToFloat(AI)
  else if AI = '' then
    Complex.R := StrToFloat(AR)
  else
    ComplexNumberSetValue(Complex, StrToFloat(AR), StrToFloat(AI));
end;

function ComplexNumberToString(var Complex: TCnComplexNumber): string;
begin
  if ComplexIsPureReal(Complex) then
    Result := Format('%f', [Complex.R])
  else if ComplexIsPureImaginary(Complex) then
    Result := Format('%fi', [Complex.I])
  else if Complex.I < 0 then
    Result := Format('%f%fi', [Complex.R, Complex.I])
  else
    Result := Format('%f+%fi', [Complex.R, Complex.I]);
end;

function ComplexNumberEqual(var Complex1, Complex2: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex1.R, Complex2.R) and FloatEqual(Complex1.I, Complex2.I);
end;

procedure ComplexNumberSwap(var Complex1, Complex2: TCnComplexNumber);
var
  T: Extended;
begin
  T := Complex1.R;
  Complex1.R := Complex2.R;
  Complex2.R := T;

  T := Complex1.I;
  Complex1.I := Complex2.I;
  Complex2.I := T;
end;

procedure ComplexNumberCopy(var Dst, Src: TCnComplexNumber);
begin
  Dst.R := Src.R;
  Dst.I := Src.I;
end;

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
begin
  Res.R := Complex1.R + Complex2.R;
  Res.I := Complex1.I + Complex2.I;
end;

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
begin
  Res.R := Complex1.R - Complex2.R;
  Res.I := Complex1.I - Complex2.I;
end;

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
var
  T: Extended;
begin
  T := Complex1.R * Complex2.R - Complex1.I * Complex2.I;
  Res.I := Complex1.R * Complex2.I + Complex1.I * Complex2.R;
  Res.R := T;
end;

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
var
  T, D: Extended;
begin
  D := Complex2.R * Complex2.R + Complex2.I * Complex2.I;
  if FloatEqual(D, 0.0) then
    raise EZeroDivide.Create(SZeroDivide);

  T := (Complex1.R * Complex2.R + Complex1.I * Complex2.I) / D;
  Res.I := (Complex1.I * Complex2.R - Complex1.R * Complex2.I) / D;
  Res.R := T;
end;

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R + Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R - Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R * Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R / Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberSqrt(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
var
  R, A: Extended;
begin
  R := FloatSqrt(ComplexNumberAbsolute(Complex));
  A := ComplexNumberArgument(Complex) / 2;

  ComplexNumberSetAbsoluteArgument(Res, R, A);
end;

procedure ComplexConjugate(var Res, Complex: TCnComplexNumber);
begin
  Res.R := Complex.R;
  Res.I := -Complex.I;
end;

function ComplexIsPureReal(var Complex: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex.I, 0.0);
end;

function ComplexIsPureImaginary(var Complex: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex.R, 0.0) and not FloatEqual(Complex.I, 0.0);
end;

function ComplexNumberAbsolute(var Complex: TCnComplexNumber): Extended;
begin
  Result := Sqrt(Complex.R * Complex.R + Complex.I * Complex.I);
end;

function ComplexNumberArgument(var Complex: TCnComplexNumber): Extended;
begin
  if Complex.I = 0 then
  begin
    if Complex.R >= 0 then     // ��ʵ�����Ƿ��� 0������ 0 Ҳ�պ��ŷ��� 0
      Result := 0
    else
      Result := CN_PI;         // ��ʵ�����Ƿ��� ��
  end
  else if Complex.R = 0 then
  begin
    if Complex.I > 0 then      // �����������Ƿ��ذ� ��
      Result := CN_PI / 2
    else
      Result := CN_PI + CN_PI / 2;   // �����������Ƿ��� 3��/2
  end
  else // ʵ���鲿����Ϊ 0
  begin
    Result := ArcTan2(Complex.I, Complex.R);
    if Result < 0 then
      Result := Result + CN_PI * 2;
  end;
end;

procedure ComplexNumberSetAbsoluteArgument(var Complex: TCnComplexNumber;
  AnAbsolute, AnArgument: Extended);
begin
  Complex.R := AnAbsolute * Cos(AnArgument);
  Complex.I := AnAbsolute * Sin(AnArgument);
end;

{ TCnBigComplexNumber }

constructor TCnBigComplexNumber.Create;
begin
  inherited;
  FR := TCnBigNumber.Create;
  FI := TCnBigNumber.Create;
end;

destructor TCnBigComplexNumber.Destroy;
begin
  FI.Free;
  FR.Free;
  inherited;
end;

function BigComplexNumberIsZero(Complex: TCnBigComplexNumber): Boolean;
begin
  Result := Complex.FR.IsZero and Complex.FI.IsZero;
end;

procedure BigComplexNumberSetZero(Complex: TCnBigComplexNumber);
begin
  Complex.FR.SetZero;
  Complex.FI.SetZero;
end;

procedure BigComplexNumberSetValue(Complex: TCnBigComplexNumber;
  AR: Int64; AI: Int64);
begin
  Complex.FR.SetInt64(AR);
  Complex.FI.SetInt64(AI);
end;

procedure BigComplexNumberSetValue(Complex: TCnBigComplexNumber;
  const AR: string; const AI: string);
begin
  Complex.FR.SetDec(AnsiString(AR));
  Complex.FI.SetDec(AnsiString(AI));
end;

function BigComplexNumberToString(Complex: TCnBigComplexNumber): string;
begin
  if BigComplexIsPureReal(Complex) then
    Result := Complex.FR.ToDec
  else if BigComplexIsPureImaginary(Complex) then
    Result := Complex.FI.ToDec + 'i'
  else if Complex.FI.IsNegative then
    Result := Complex.FR.ToDec + Complex.FI.ToDec
  else
    Result := Complex.FR.ToDec + '+' + Complex.FI.ToDec;
end;

function BigComplexNumberEqual(Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber): Boolean;
begin
  Result := BigNumberEqual(Complex1.FR, Complex2.FR) and BigNumberEqual(Complex1.FI, Complex2.FI);
end;

procedure BigComplexNumberSwap(Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber);
begin
  BigNumberSwap(Complex1.FR, Complex2.FR);
  BigNumberSwap(Complex1.FI, Complex2.FI);
end;

procedure BigComplexNumberCopy(Dst: TCnBigComplexNumber; Src: TCnBigComplexNumber);
begin
  BigNumberCopy(Dst.FR, Src.FR);
  BigNumberCopy(Dst.FI, Src.FI);
end;

procedure BigComplexNumberAdd(Res: TCnBigComplexNumber;
  Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber);
begin
  BigNumberAdd(Res.FR, Complex1.FR, Complex2.FR);
  BigNumberAdd(Res.FI, Complex1.FI, Complex2.FI);
end;

procedure BigComplexNumberSub(Res: TCnBigComplexNumber;
  Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber);
begin
  BigNumberSub(Res.FR, Complex1.FR, Complex2.FR);
  BigNumberSub(Res.FI, Complex1.FI, Complex2.FI);
end;

procedure BigComplexNumberMul(Res: TCnBigComplexNumber;
  Complex1: TCnBigComplexNumber; Complex2: TCnBigComplexNumber);
begin
  BigNumberMul(Res.FR, Complex1.FR, Complex2.FR);
  BigNumberMul(Res.FI, Complex1.FI, Complex2.FI);
end;

procedure BigComplexNumberAdd(Res: TCnBigComplexNumber;
  Complex: TCnBigComplexNumber; Value: Int64);
var
  T: TCnBigNumber;
begin
  BigComplexNumberCopy(Res, Complex);
  T := TCnBigNumber.Create;
  try
    T.SetInt64(Value);
    BigNumberAdd(Res.FR, Res.FR, T);
  finally
    T.Free;
  end;
end;

procedure BigComplexNumberSub(Res: TCnBigComplexNumber;
  Complex: TCnBigComplexNumber; Value: Int64);
var
  T: TCnBigNumber;
begin
  BigComplexNumberCopy(Res, Complex);
  T := TCnBigNumber.Create;
  try
    T.SetInt64(Value);
    BigNumberSub(Res.FR, Res.FR, T);
  finally
    T.Free;
  end;
end;

procedure BigComplexNumberMul(Res: TCnBigComplexNumber;
  Complex: TCnBigComplexNumber; Value: Int64);
var
  T: TCnBigNumber;
begin
  BigComplexNumberCopy(Res, Complex);
  T := TCnBigNumber.Create;
  try
    T.SetInt64(Value);
    BigNumberMul(Res.FR, Res.FR, T);
    BigNumberMul(Res.FI, Res.FI, T);
  finally
    T.Free;
  end;
end;

procedure BigComplexConjugate(Res: TCnBigComplexNumber; Complex: TCnBigComplexNumber);
begin
  BigNumberCopy(Res.FR, Complex.FR);
  BigNumberCopy(Res.FI, Complex.FI);
  Res.FI.Negate;
end;

function BigComplexIsPureReal(Complex: TCnBigComplexNumber): Boolean;
begin
  Result := Complex.FI.IsZero;
end;

function BigComplexIsPureImaginary(Complex: TCnBigComplexNumber): Boolean;
begin
  Result := Complex.FR.IsZero and not Complex.FI.IsZero;
end;

function BigComplexNumberAbsolute(Complex: TCnBigComplexNumber): Extended;
var
  X, Y: Extended;
begin
  X := BigNumberGetFloat(Complex.FR);
  Y := BigNumberGetFloat(Complex.FI);
  Result := Sqrt(X * X + Y * Y);
end;

function BigComplexNumberAbsolute(Res: TCnBigNumber; Complex: TCnBigComplexNumber): Boolean;
var
  X, Y: TCnBigNumber;
begin
  X := BigNumberDuplicate(Complex.FR);
  Y := BigNumberDuplicate(Complex.FI);
  BigNumberMul(X, X, X);
  BigNumberMul(Y, Y, Y);
  BigNumberAdd(Res, X, Y);
  Result := BigNumberSqrt(Res, Res);
end;

function BigComplexNumberArgument(Complex: TCnBigComplexNumber): Extended;
var
  X, Y: Extended;
begin
  X := BigNumberGetFloat(Complex.FR);
  Y := BigNumberGetFloat(Complex.FI);

  if Complex.FI.IsZero then
  begin
    if not Complex.FR.IsNegative then
      Result := 0                    // ��ʵ�����Ƿ��� 0������ 0 Ҳ�պ��ŷ��� 0
    else
      Result := CN_PI;               // ��ʵ�����Ƿ��� ��
  end
  else if Complex.FR.IsZero then
  begin
    if not Complex.FI.IsZero and not Complex.FI.IsNegative then
      Result := CN_PI / 2            // �����������Ƿ��ذ� ��
    else
      Result := CN_PI + CN_PI / 2;   // �����������Ƿ��� 3��/2
  end
  else // ʵ���鲿����Ϊ 0
  begin
    Result := ArcTan2(Y, X);
    if Result < 0 then
      Result := Result + CN_PI * 2;
  end;
end;

initialization
  ComplexNumberSetZero(CnComplexZero);

  CnComplexOne.R := 1;
  CnComplexOne.I := 0;

  CnComplexOneI.R := 0;
  CnComplexOneI.I := 1;

  CnComplexNegOneI.R := 0;
  CnComplexNegOneI.I := -1;

  CnBigComplexNumberZero := TCnBigComplexNumber.Create;
  CnBigComplexNumberZero.FR.SetZero;
  CnBigComplexNumberZero.FI.SetZero;

  CnBigComplexNumberOne := TCnBigComplexNumber.Create;
  CnBigComplexNumberOne.FR.SetOne;
  CnBigComplexNumberOne.FI.SetZero;

  CnBigComplexNumberOneI := TCnBigComplexNumber.Create;
  CnBigComplexNumberOneI.FR.SetZero;
  CnBigComplexNumberOneI.FI.SetOne;

  CnBigComplexNumberNegOneI := TCnBigComplexNumber.Create;
  CnBigComplexNumberNegOneI.FR.SetZero;
  CnBigComplexNumberNegOneI.FI.SetInteger(-1);

finalization
  CnBigComplexNumberNegOneI.Free;
  CnBigComplexNumberOneI.Free;
  CnBigComplexNumberOne.Free;
  CnBigComplexNumberZero.Free;

end.
