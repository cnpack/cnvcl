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

unit CnBigRational;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����޾���������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ�������޾��ȵ��������࣬�ڲ��ô������ı�ֵ��ʾ��������
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2021.12.06 V1.1
*               ����ػ��ƣ����������������˳������������һ��
*           2019.12.19 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, CnContainers, CnBigNumber;

type
  TCnBigRational = class(TPersistent)
  {* ��ʾһ�����޾��ȵĴ�������}
  private
    FNominator: TCnBigNumber;
    FDenominator: TCnBigNumber;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    {* �ڲ���ֵ}
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
    {* �Ƿ�Ϊ 1

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ 1
    }

    function IsNegative: Boolean;
    {* �Ƿ�Ϊ��ֵ��

       ������
         ���ޣ�

       ����ֵ��Boolean                    - �����Ƿ�Ϊ��ֵ
    }

    procedure Neg;
    {* ����෴��}

    procedure Reciprocal;
    {* ��ɵ���}

    procedure SetZero;
    {* ��Ϊ 0}

    procedure SetOne;
    {* ��Ϊ 1}

    function EqualInt(Value: Cardinal): Boolean; overload;
    {* �Ƿ�����һ������ȡ�

       ������
         Value: Cardinal                  - ���жϵ�����

       ����ֵ��Boolean                    - �����Ƿ����
    }

    function EqualInt(Value: TCnBigNumber): Boolean; overload;
    {* �Ƿ�����һ���������

       ������
         Value: TCnBigNumber              - ���жϴ�����

       ����ֵ��Boolean                    -
    }

    function Equal(Value: TCnBigRational): Boolean;
    {* �Ƿ�����һ�����������

       ������
         Value: TCnBigRational            - ���жϵĴ�������

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

    procedure Add(Value: TCnBigNumber); overload;
    {* ����һ����������

       ������
         Value: TCnBigNumber              - ����

       ����ֵ�����ޣ�
    }

    procedure Sub(Value: TCnBigNumber); overload;
    {* ��ȥһ����������

       ������
         Value: TCnBigNumber              - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Value: TCnBigNumber); overload;
    {* ����һ����������

       ������
         Value: TCnBigNumber              - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Value: TCnBigNumber); overload;
    {* ����һ����������

       ������
         Value: TCnBigNumber              - ����

       ����ֵ�����ޣ�
    }

    procedure Add(Value: TCnBigRational); overload;
    {* ����һ������������

       ������
         Value: TCnBigRational            - ����

       ����ֵ�����ޣ�
    }

    procedure Sub(Value: TCnBigRational); overload;
    {* ��ȥһ������������

       ������
         Value: TCnBigRational            - ����

       ����ֵ�����ޣ�
    }

    procedure Mul(Value: TCnBigRational); overload;
    {* ����һ������������

       ������
         Value: TCnBigRational            - ����

       ����ֵ�����ޣ�
    }

    procedure Divide(Value: TCnBigRational); overload;
    {* ����һ������������

       ������
         Value: TCnBigRational            - ����

       ����ֵ�����ޣ�
    }

    procedure SetIntValue(Value: Cardinal); overload;
    {* ֵ��Ϊһ��������

       ������
         Value: Cardinal                  - �����õ�����

       ����ֵ�����ޣ�
    }

    procedure SetIntValue(Value: TCnBigNumber); overload;
    {* ֵ��Ϊһ����������

       ������
         Value: TCnBigNumber              - �����õĴ�����

       ����ֵ�����ޣ�
    }

    procedure SetValue(ANominator: TCnBigNumber; ADenominator: TCnBigNumber); overload;
    {* ֵ��Ϊһ��������

       ������
         ANominator: TCnBigNumber         - ���ӣ���ʽΪ����
         ADenominator: TCnBigNumber       - ��ĸ����ʽΪ����

       ����ֵ�����ޣ�
    }

    procedure SetValue(const ANominator: string; const ADenominator: string); overload;
    {* ֵ��Ϊһ���������������ַ����ķ�ʽ���롣

       ������
         const ANominator: string         - �����ַ���
         const ADenominator: string       - ��ĸ�ַ���

       ����ֵ�����ޣ�
    }

    procedure SetString(const Value: string);
    {* ֵ��Ϊһ���ַ����������Ǵ����֣���� / �ķ�������С����

       ������
         const Value: string              - �����õ��ַ���

       ����ֵ�����ޣ�
    }

    procedure SetFloat(AFloat: Extended);
    {* ֵ��Ϊһ�����������Ѹ���������Ч���ֺ�ָ���𿪴���

       ������
         AFloat: Extended                 - �����õĸ�����

       ����ֵ�����ޣ�
    }

    procedure Reduce;
    {* ����Լ��}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ������ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ���ص��ַ���
    }

    function ToDec(Digits: Integer = 20): string;
    {* �����С����Ĭ����С����� 20 λ���ȡ�

       ������
         Digits: Integer                  - ��������С�����ľ���λ��

       ����ֵ��string                     - ���ص��ַ���
    }

    property Nominator: TCnBigNumber read FNominator;
    {* ����}
    property Denominator: TCnBigNumber read FDenominator;
    {* ��ĸ}
  end;

  TCnBigRationalPool = class(TCnMathObjectPool)
  {* ����������ʵ���࣬����ʹ�õ����������ĵط����д����󸡵�����}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigRational; reintroduce;
    {* �Ӷ���ػ�ȡһ�����󣬲���ʱ����� Recycle �黹��

       ������
         ���ޣ�

       ����ֵ��TCnBigRational             - ���صĴ�����������
    }

    procedure Recycle(Num: TCnBigRational); reintroduce;
    {* ��һ������黹������ء�

       ������
         Num: TCnBigRational              - ���黹�Ĵ�����������

       ����ֵ�����ޣ�
    }
  end;

// ============================= �����������㷽�� ==============================

procedure BigRationalNumberAdd(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* ���������ӷ�������������ȡ�

   ������
     Res: TCnBigRational                  - ����������
     Num1: TCnBigRational                 - ������������һ
     Num2: TCnBigRational                 - ��������������

   ����ֵ�����ޣ�
}

procedure BigRationalNumberSub(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* ������������������������ȡ�

   ������
     Res: TCnBigRational                  - ����������
     Num1: TCnBigRational                 - ��������������
     Num2: TCnBigRational                 - ������������

   ����ֵ�����ޣ�
}

procedure BigRationalNumberMul(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* ���������˷�������������ȡ�

   ������
     Res: TCnBigRational                  - ����������
     Num1: TCnBigRational                 - ������������һ
     Num2: TCnBigRational                 - ��������������

   ����ֵ�����ޣ�
}

procedure BigRationalNumberDiv(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* ������������������������ȡ�

   ������
     Res: TCnBigRational                  - ����������
     Num1: TCnBigRational                 - ��������������
     Num2: TCnBigRational                 - ������������

   ����ֵ�����ޣ�
}

function BigRationalNumberCompare(Num1: TCnBigRational; Num2: TCnBigRational): Integer; overload;
{* ���������Ƚϣ�ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1��

   ������
     Num1: TCnBigRational                 - ���ȽϵĴ�������һ
     Num2: TCnBigRational                 - ���ȽϵĴ���������

   ����ֵ��Integer                        - ���رȽϽ��
}

function BigRationalNumberCompare(Num1: TCnBigRational; Num2: Int64): Integer; overload;
{^ ���������������Ƚϣ�ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1��

   ������
     Num1: TCnBigRational                 - ���ȽϵĴ�������
     Num2: TCnBigRational                 - ���Ƚϵ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

procedure ReduceBigNumber(X: TCnBigNumber; Y: TCnBigNumber);
{* ����������С��Ҳ����Լ�֡�

   ������
     X: TCnBigNumber                      - ��Լ�ֵĴ���ֵһ
     Y: TCnBigNumber                      - ��Լ�ֵĴ���ֵ��

   ����ֵ�����ޣ�
}

var
  CnBigRationalNumberOne: TCnBigRational = nil;
  CnBigRationalNumberZero: TCnBigRational = nil;

implementation

var
  FLocalBigRationalPool: TCnBigRationalPool = nil;
  FLocalBigNumberPool: TCnBigNumberPool = nil;

procedure BigRationalNumberAdd(Res: TCnBigRational;
  Num1, Num2: TCnBigRational);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, R, F1, F2, D1, D2: TCnBigNumber;
  B1, B2: Boolean;
begin
  if Num1.IsInt and Num2.IsInt then
  begin
    BigNumberAdd(Res.Nominator, Num1.Nominator, Num2.Nominator);
    Res.Denominator.SetOne;
    Exit;
  end
  else if Num1.IsZero then
  begin
    if Num2 <> Res then
      Res.Assign(Num2);
  end
  else if Num2.IsZero then
  begin
    if Num1 <> Res then
      Res.Assign(Num1);
  end
  else
  begin
    M := nil;
    R := nil;
    F1 := nil;
    F2 := nil;
    D1 := nil;
    D2 := nil;

    try
      // ���ĸ����С������
      M := FLocalBigNumberPool.Obtain;
      R := FLocalBigNumberPool.Obtain;
      F1 := FLocalBigNumberPool.Obtain;
      F2 := FLocalBigNumberPool.Obtain;
      D1 := FLocalBigNumberPool.Obtain;
      D2 := FLocalBigNumberPool.Obtain;

      BigNumberCopy(D1, Num1.Denominator);
      BigNumberCopy(D2, Num2.Denominator);

      B1 := Num1.Denominator.IsNegative;
      B2 := Num2.Denominator.IsNegative;

      D1.SetNegative(False);
      D2.SetNegative(False);

      BigNumberLcm(M, D1, D2);
      BigNumberDiv(F1, R, M, D1);
      BigNumberDiv(F2, R, M, D2);

      BigNumberCopy(Res.Denominator, M);
      BigNumberMul(R, Num1.Nominator, F1);
      if B1 then
        R.SetNegative(not R.IsNegative);
      BigNumberMul(M, Num2.Nominator, F2);
      if B2 then
        M.SetNegative(not M.IsNegative);

      BigNumberAdd(Res.Nominator, R, M);
    finally
      FLocalBigNumberPool.Recycle(D2);
      FLocalBigNumberPool.Recycle(D1);
      FLocalBigNumberPool.Recycle(F2);
      FLocalBigNumberPool.Recycle(F1);
      FLocalBigNumberPool.Recycle(R);
      FLocalBigNumberPool.Recycle(M);
    end;
  end;
  Res.Reduce;
end;

procedure BigRationalNumberSub(Res: TCnBigRational;
  Num1, Num2: TCnBigRational);
begin
  Num2.Nominator.SetNegative(not Num2.Nominator.IsNegative);
  BigRationalNumberAdd(Res, Num1, Num2);
  if Res <> Num2 then
    Num2.Nominator.SetNegative(not Num2.Nominator.IsNegative);
end;

procedure BigRationalNumberMul(Res: TCnBigRational;
  Num1, Num2: TCnBigRational);
begin
  BigNumberMul(Res.Nominator, Num1.Nominator, Num2.Nominator);
  BigNumberMul(Res.Denominator, Num1.Denominator, Num2.Denominator);
  Res.Reduce;
end;

procedure BigRationalNumberDiv(Res: TCnBigRational;
  Num1, Num2: TCnBigRational);
var
  N: TCnBigNumber;
begin
  if Num2.IsZero then
    raise EDivByZero.Create('Divide by Zero.');

  N := FLocalBigNumberPool.Obtain;  // ������ˣ��������м��������ֹ RationalResult �� Number1 �� Number 2
  try
    BigNumberMul(N, Num1.Nominator, Num2.Denominator);
    BigNumberMul(Res.Denominator, Num1.Denominator, Num2.Nominator);
    BigNumberCopy(Res.Nominator, N);
  finally
    FLocalBigNumberPool.Recycle(N);
  end;
  Res.Reduce;
end;

function BigRationalNumberCompare(Num1, Num2: TCnBigRational): Integer;
var
  Res: TCnBigRational;
begin
  if not Num1.IsNegative and Num2.IsNegative then
    Result := 1
  else if Num1.IsNegative and not Num2.IsNegative then
    Result := -1
  else if Num1.IsZero and Num2.IsZero then
    Result := 0
  else if Num1.IsInt and Num2.IsInt then
    Result := BigNumberCompare(Num1.Nominator, Num2.Nominator)
  else
  begin
    //  ͬ�ţ��������Ƚ�
    Res := FLocalBigRationalPool.Obtain;
    try
      BigRationalNumberSub(Res, Num1, Num2);
      if Res.IsZero then
        Result := 0
      else if Res.IsNegative then
        Result := -1
      else
        Result := 1;
    finally
      FLocalBigRationalPool.Recycle(Res);
    end;
  end;
end;

function BigRationalNumberCompare(Num1: TCnBigRational; Num2: Int64): Integer;
var
  Res: TCnBigNumber;
begin
  if not Num1.IsNegative and (Num2 < 0) then
    Result := 1
  else if Num1.IsNegative and (Num2 > 0) then
    Result := -1
  else if Num1.IsZero and (Num2 = 0) then
    Result := 0
  else
  begin
    Res := FLocalBigNumberPool.Obtain;
    try
      Res.SetInt64(Num2);
      if not Num1.IsInt then
        BigNumberMul(Res, Num1.Denominator, Res);
      Result := BigNumberCompare(Num1.Nominator, Res);
    finally
      FLocalBigNumberPool.Recycle(Res);
    end;
  end;
end;

procedure ReduceBigNumber(X, Y: TCnBigNumber);
var
  N, R: TCnBigNumber;
begin
  N := FLocalBigNumberPool.Obtain;
  try
    if BigNumberGcd(N, X, Y) then
    begin
      if not N.IsOne then
      begin
        R := FLocalBigNumberPool.Obtain;
        try
          BigNumberDiv(X, R, X, N);
          BigNumberDiv(Y, R, Y, N);
        finally
          FLocalBigNumberPool.Recycle(R);
        end;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(N);
  end;
end;

{ TCnBigRationalNumber }

procedure TCnBigRational.Add(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    BigRationalNumberAdd(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Add(Value: Int64);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    BigRationalNumberAdd(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Add(Value: TCnBigRational);
begin
  BigRationalNumberAdd(Self, Self, Value);
end;

procedure TCnBigRational.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnBigRational then
  begin
    BigNumberCopy(TCnBigRational(Dest).Nominator, FNominator);
    BigNumberCopy(TCnBigRational(Dest).Denominator, FDenominator);
  end
  else
    inherited;
end;

constructor TCnBigRational.Create;
begin
  FNominator := TCnBigNumber.Create;
  FDenominator := TCnBigNumber.Create;
  FDenominator.SetOne;
  FNominator.SetZero;
end;

destructor TCnBigRational.Destroy;
begin
  FDenominator.Free;
  FNominator.Free;
  inherited;
end;

procedure TCnBigRational.Divide(Value: Int64);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    BigRationalNumberDiv(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Divide(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    BigRationalNumberDiv(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Divide(Value: TCnBigRational);
begin
  BigRationalNumberDiv(Self, Self, Value);
end;

function TCnBigRational.Equal(Value: TCnBigRational): Boolean;
begin
  Result := BigRationalNumberCompare(Self, Value) = 0;
end;

function TCnBigRational.EqualInt(Value: TCnBigNumber): Boolean;
begin
  if FDenominator.IsOne then
    Result := BigNumberCompare(Value, FNominator) = 0
  else if FDenominator.IsNegOne then
    Result := (BigNumberUnsignedCompare(Value, FNominator) = 0)
      and (FNominator.IsNegative <> Value.IsNegative)
  else
    Result := False;
end;

function TCnBigRational.EqualInt(Value: Cardinal): Boolean;
begin
  if FDenominator.IsOne then
    Result := FNominator.IsWord(Value)
  else if FDenominator.IsNegOne then
    Result := BigNumberAbsIsWord(FNominator, Value) and FNominator.IsNegative
  else
    Result := False;
end;

function TCnBigRational.IsInt: Boolean;
begin
  Result := FDenominator.IsOne or FDenominator.IsNegOne;
end;

function TCnBigRational.IsNegative: Boolean;
begin
  Result := FNominator.IsNegative <> FDenominator.IsNegative;
end;

function TCnBigRational.IsOne: Boolean;
begin
  Result := not FNominator.IsZero and (BigNumberCompare(FNominator, FDenominator) = 0);
end;

function TCnBigRational.IsZero: Boolean;
begin
  Result := FNominator.IsZero;
end;

procedure TCnBigRational.Mul(Value: TCnBigRational);
begin
  BigRationalNumberMul(Self, Self, Value);
end;

procedure TCnBigRational.Mul(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    BigRationalNumberMul(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Mul(Value: Int64);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    BigRationalNumberMul(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Neg;
begin
  FNominator.SetNegative(not FNominator.IsNegative);
  if FNominator.IsNegative and FDenominator.IsNegative then
  begin
    FNominator.SetNegative(False);
    FDenominator.SetNegative(False);
  end;
end;

procedure TCnBigRational.Reciprocal;
var
  T: TCnBigNumber;
begin
  if FNominator.IsZero then
    raise EDivByZero.Create(SDivByZero);

  T := FLocalBigNumberPool.Obtain;
  try
    BigNumberCopy(T, FDenominator);
    BigNumberCopy(FDenominator, FNominator);
    BigNumberCopy(FNominator, T);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

procedure TCnBigRational.Reduce;
begin
  if FDenominator.IsNegative and FNominator.IsNegative then
  begin
    FDenominator.SetNegative(False);
    FNominator.SetNegative(False);
  end
  else if FDenominator.IsNegative and not FNominator.IsNegative then  // ��ĸ�ĸ����Ƶ�����
  begin
    FDenominator.SetNegative(False);
    FNominator.SetNegative(True);
  end;

  if FNominator.IsZero then
  begin
    FDenominator.SetOne;
    Exit;
  end;

  if not IsInt then
    ReduceBigNumber(FNominator, FDenominator);
end;

procedure TCnBigRational.SetIntValue(Value: Cardinal);
begin
  FNominator.SetWord(Value);
  FDenominator.SetOne;
end;

procedure TCnBigRational.SetFloat(AFloat: Extended);
var
  F: TFloatRec;
  I, L: Integer;
begin
  // ���������λ����Ч������ָ��
  FloatToDecimal(F, AFloat, fvExtended, 18, 9999);

  L := StrLen(PAnsiChar(@F.Digits[0]));
  // ��ĸ�� 10 �� L - F.Exponent �η��������Ǵ��� Digits
  FDenominator.SetOne;
  for I := 1 to L - F.Exponent do
    FDenominator.MulWord(10);

  FNominator.SetDec(PAnsiChar(@F.Digits[0]));
  FNominator.SetNegative(F.Negative);
  Reduce;
end;

procedure TCnBigRational.SetIntValue(Value: TCnBigNumber);
begin
  BigNumberCopy(FNominator, Value);
  FDenominator.SetOne;
end;

procedure TCnBigRational.SetOne;
begin
  FNominator.SetOne;
  FDenominator.SetOne;
end;

procedure TCnBigRational.SetString(const Value: string);
var
  P: Integer;
  N, D: string;
begin
  P := Pos('/', Value);
  if P > 1 then
  begin
    N := Copy(Value, 1, P - 1);
    D := Copy(Value, P + 1, MaxInt);
    FNominator.SetDec(AnsiString(N));
    FDenominator.SetDec(AnsiString(D));
    Reduce;
  end
  else
  begin
    P := Pos('.', Value);
    if P > 1 then
    begin
      // ����С����
      N := Copy(Value, 1, P - 1);
      D := Copy(Value, P + 1, MaxInt);
      FNominator.SetDec(AnsiString(N + D));
      FDenominator.SetOne;
      for P := 1 to Length(D) do
        FDenominator.MulWord(10);
      Reduce;
    end
    else
    begin
      FNominator.SetDec(AnsiString(Value));
      FDenominator.SetOne;
    end;
  end;
end;

procedure TCnBigRational.SetValue(ANominator, ADenominator: TCnBigNumber);
begin
  BigNumberCopy(FNominator, ANominator);
  BigNumberCopy(FDenominator, ADenominator);
end;

procedure TCnBigRational.SetValue(const ANominator, ADenominator: string);
begin
  FNominator.SetDec(AnsiString(ANominator));
  FDenominator.SetDec(AnsiString(ADenominator));
end;

procedure TCnBigRational.SetZero;
begin
  FNominator.SetZero;
  FDenominator.SetOne;
end;

procedure TCnBigRational.Sub(Value: Int64);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    BigRationalNumberSub(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Sub(Value: TCnBigRational);
begin
  BigRationalNumberSub(Self, Self, Value);
end;

procedure TCnBigRational.Sub(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    BigRationalNumberSub(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

function TCnBigRational.ToDec(Digits: Integer): string;
var
  Remain, Res: TCnBigNumber;
  I: Integer;
  R: string;
  IsNeg: Boolean;
begin
  Remain := nil;
  Res := nil;

  // ����˼�����ȳ����õ��������֣�������������ͼ����� 0 ����
  try
    if IsInt then
    begin
      Result := FNominator.ToDec;
      Exit;
    end;

    IsNeg := IsNegative;
    if IsNeg then
      Neg;

    Remain := FLocalBigNumberPool.Obtain;
    Res := FLocalBigNumberPool.Obtain;

    BigNumberDiv(Res, Remain, FNominator, FDenominator);
    Result := Res.ToDec;
    if Remain.IsZero or (Digits <= 0) then
    begin
      if IsNeg then
        Neg;
      Exit;
    end;

    R := '.';
    for I := 1 to Digits do
    begin
      // Remain * 10������������̣������ͼ� 0����һ�ּ����� 10
      Remain.MulWord(10);
      if BigNumberCompare(Remain, FDenominator) > 0 then
      begin
        BigNumberDiv(Res, Remain, Remain, FDenominator);
        R := R + Res.ToDec;
        if Remain.IsZero then
          Break;
      end
      else
      begin
        R := R + '0';
      end;
    end;

    if IsNeg then
      Neg;
    Result := Result + R;
  finally
    FLocalBigNumberPool.Recycle(Res);
    FLocalBigNumberPool.Recycle(Remain);
  end;
end;

function TCnBigRational.ToString: string;
begin
  if FDenominator.IsOne then
    Result := FNominator.ToDec
  else
    Result := FNominator.ToDec + ' / ' + FDenominator.ToDec;
end;

{ TCnBigRationalPool }

function TCnBigRationalPool.CreateObject: TObject;
begin
  Result := TCnBigRational.Create;
end;

function TCnBigRationalPool.Obtain: TCnBigRational;
begin
  Result := TCnBigRational(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigRationalPool.Recycle(Num: TCnBigRational);
begin
  inherited Recycle(Num);
end;

initialization
  CnBigRationalNumberOne := TCnBigRational.Create;
  CnBigRationalNumberZero := TCnBigRational.Create;
  CnBigRationalNumberOne.SetOne;
  CnBigRationalNumberZero.SetZero;

  FLocalBigRationalPool := TCnBigRationalPool.Create;
  FLocalBigNumberPool := TCnBigNumberPool.Create;

finalization
  FLocalBigNumberPool.Free;
  FLocalBigRationalPool.Free;

  CnBigRationalNumberOne.Free;
  CnBigRationalNumberZero.Free;

end.
