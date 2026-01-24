{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnBigRational;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：无限精度有理数实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了无限精度的有理数类 TCnBigRational，
*           内部用大整数 TCnBigNumber 的比值表示有理数。
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2021.12.06 V1.1
*               加入池机制，调整函数名与参数顺序以与其他类一致
*           2019.12.19 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, CnContainers, CnBigNumber;

type
  TCnBigRational = class(TPersistent)
  {* 表示一个无限精度的大有理数}
  private
    FNumerator: TCnBigNumber;
    FDenominator: TCnBigNumber;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    {* 内部赋值}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function IsInt: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否整数，也就是判断分母是否是正负 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否整数
    }

    function IsZero: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    function IsOne: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 1

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    function IsNegative: Boolean;
    {* 是否为负值。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为负值
    }

    procedure Neg;
    {* 变成相反数}

    procedure Reciprocal;
    {* 变成倒数}

    procedure SetZero;
    {* 设为 0}

    procedure SetOne;
    {* 设为 1}

    function EqualInt(Value: Cardinal): Boolean; overload;
    {* 是否与另一整数相等。

       参数：
         Value: Cardinal                  - 待判断的整数

       返回值：Boolean                    - 返回是否相等
    }

    function EqualInt(Value: TCnBigNumber): Boolean; overload;
    {* 是否与另一大整数相等

       参数：
         Value: TCnBigNumber              - 待判断大整数

       返回值：Boolean                    -
    }

    function Equal(Value: TCnBigRational): Boolean;
    {* 是否与另一大有理数相等

       参数：
         Value: TCnBigRational            - 待判断的大有理数

       返回值：Boolean                    - 返回是否相等
    }

    procedure Add(Value: Int64); overload;
    {* 加上一个整数。

       参数：
         Value: Int64                     - 加数

       返回值：（无）
    }

    procedure Sub(Value: Int64); overload;
    {* 减去一个整数。

       参数：
         Value: Int64                     - 减数

       返回值：（无）
    }

    procedure Mul(Value: Int64); overload;
    {* 乘以一个整数。

       参数：
         Value: Int64                     - 乘数

       返回值：（无）
   }

    procedure Divide(Value: Int64); overload;
    {* 除以一个整数。

       参数：
         Value: Int64                     - 除数

       返回值：（无）
    }

    procedure Add(Value: TCnBigNumber); overload;
    {* 加上一个大整数。

       参数：
         Value: TCnBigNumber              - 加数

       返回值：（无）
    }

    procedure Sub(Value: TCnBigNumber); overload;
    {* 减去一个大整数。

       参数：
         Value: TCnBigNumber              - 减数

       返回值：（无）
    }

    procedure Mul(Value: TCnBigNumber); overload;
    {* 乘以一个大整数。

       参数：
         Value: TCnBigNumber              - 乘数

       返回值：（无）
    }

    procedure Divide(Value: TCnBigNumber); overload;
    {* 除以一个大整数。

       参数：
         Value: TCnBigNumber              - 除数

       返回值：（无）
    }

    procedure Add(Value: TCnBigRational); overload;
    {* 加上一个大有理数。

       参数：
         Value: TCnBigRational            - 加数

       返回值：（无）
    }

    procedure Sub(Value: TCnBigRational); overload;
    {* 减去一个大有理数。

       参数：
         Value: TCnBigRational            - 减数

       返回值：（无）
    }

    procedure Mul(Value: TCnBigRational); overload;
    {* 乘以一个大有理数。

       参数：
         Value: TCnBigRational            - 乘数

       返回值：（无）
    }

    procedure Divide(Value: TCnBigRational); overload;
    {* 除以一个大有理数。

       参数：
         Value: TCnBigRational            - 除数

       返回值：（无）
    }

    procedure SetIntValue(Value: Cardinal); overload;
    {* 值设为一个整数。

       参数：
         Value: Cardinal                  - 待设置的整数

       返回值：（无）
    }

    procedure SetIntValue(Value: TCnBigNumber); overload;
    {* 值设为一个大整数。

       参数：
         Value: TCnBigNumber              - 待设置的大整数

       返回值：（无）
    }

    procedure SetValue(ANumerator: TCnBigNumber; ADenominator: TCnBigNumber); overload;
    {* 值设为一个分数。

       参数：
         ANumerator: TCnBigNumber         - 分子，形式为大数
         ADenominator: TCnBigNumber       - 分母，形式为大数

       返回值：（无）
    }

    procedure SetValue(const ANumerator: string; const ADenominator: string); overload;
    {* 值设为一个分数，数字用字符串的方式输入。

       参数：
         const ANumerator: string         - 分子字符串
         const ADenominator: string       - 分母字符串

       返回值：（无）
    }

    procedure SetString(const Value: string);
    {* 值设为一个字符串，可以是纯数字，或带 / 的分数，或小数。

       参数：
         const Value: string              - 待设置的字符串

       返回值：（无）
    }

    procedure SetFloat(AFloat: Extended);
    {* 值设为一个浮点数，把浮点数的有效数字和指数拆开处理。

       参数：
         AFloat: Extended                 - 待设置的浮点数

       返回值：（无）
    }

    procedure Reduce;
    {* 尽量约分}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 输出成字符串。

       参数：
         （无）

       返回值：string                     - 返回的字符串
    }

    function ToDec(Digits: Integer = 20): string;
    {* 输出成小数，默认留小数点后 20 位精度。

       参数：
         Digits: Integer                  - 待保留的小数点后的精度位数

       返回值：string                     - 返回的字符串
    }

    property Numerator: TCnBigNumber read FNumerator;
    {* 分子}
    property Denominator: TCnBigNumber read FDenominator;
    {* 分母}
  end;

  TCnBigRationalPool = class(TCnMathObjectPool)
  {* 大有理数池实现类，允许使用到大有理数的地方自行创建大浮点数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigRational; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigRational             - 返回的大有理数对象
    }

    procedure Recycle(Num: TCnBigRational); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnBigRational              - 待归还的大有理数对象

       返回值：（无）
    }
  end;

// ============================= 大有理数运算方法 ==============================

procedure BigRationalNumberAdd(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* 大有理数加法，三数可以相等。

   参数：
     Res: TCnBigRational                  - 大有理数和
     Num1: TCnBigRational                 - 大有理数加数一
     Num2: TCnBigRational                 - 大有理数加数二

   返回值：（无）
}

procedure BigRationalNumberSub(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* 大有理数减法，三数可以相等。

   参数：
     Res: TCnBigRational                  - 大有理数差
     Num1: TCnBigRational                 - 大有理数被减数
     Num2: TCnBigRational                 - 大有理数减数

   返回值：（无）
}

procedure BigRationalNumberMul(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* 大有理数乘法，三数可以相等。

   参数：
     Res: TCnBigRational                  - 大有理数积
     Num1: TCnBigRational                 - 大有理数乘数一
     Num2: TCnBigRational                 - 大有理数乘数二

   返回值：（无）
}

procedure BigRationalNumberDiv(Res: TCnBigRational; Num1: TCnBigRational; Num2: TCnBigRational);
{* 大有理数除法，三数可以相等。

   参数：
     Res: TCnBigRational                  - 大有理数商
     Num1: TCnBigRational                 - 大有理数被除数
     Num2: TCnBigRational                 - 大有理数除数

   返回值：（无）
}

function BigRationalNumberCompare(Num1: TCnBigRational; Num2: TCnBigRational): Integer; overload;
{* 大有理数比较，前者大于、等于、小于后者时分别返回 1、0、-1。

   参数：
     Num1: TCnBigRational                 - 待比较的大有理数一
     Num2: TCnBigRational                 - 待比较的大有理数二

   返回值：Integer                        - 返回比较结果
}

function BigRationalNumberCompare(Num1: TCnBigRational; Num2: Int64): Integer; overload;
{^ 大有理数与整数比较，前者大于、等于、小于后者时分别返回 1、0、-1。

   参数：
     Num1: TCnBigRational                 - 待比较的大有理数
     Num2: TCnBigRational                 - 待比较的整数

   返回值：Integer                        - 返回比较结果
}

procedure ReduceBigNumber(X: TCnBigNumber; Y: TCnBigNumber);
{* 尽量比例缩小，也就是约分。

   参数：
     X: TCnBigNumber                      - 待约分的大数值一
     Y: TCnBigNumber                      - 待约分的大数值二

   返回值：（无）
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
    BigNumberAdd(Res.Numerator, Num1.Numerator, Num2.Numerator);
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
      // 求分母的最小公倍数
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
      BigNumberMul(R, Num1.Numerator, F1);
      if B1 then
        R.SetNegative(not R.IsNegative);
      BigNumberMul(M, Num2.Numerator, F2);
      if B2 then
        M.SetNegative(not M.IsNegative);

      BigNumberAdd(Res.Numerator, R, M);
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
  if Num1 = Num2 then
  begin
    Res.SetZero;
    Exit;
  end;

  Num2.Numerator.SetNegative(not Num2.Numerator.IsNegative);
  BigRationalNumberAdd(Res, Num1, Num2);
  if Res <> Num2 then
    Num2.Numerator.SetNegative(not Num2.Numerator.IsNegative);
end;

procedure BigRationalNumberMul(Res: TCnBigRational;
  Num1, Num2: TCnBigRational);
begin
  BigNumberMul(Res.Numerator, Num1.Numerator, Num2.Numerator);
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

  N := FLocalBigNumberPool.Obtain;  // 交叉相乘，必须用中间变量，防止 RationalResult 是 Number1 或 Number 2
  try
    BigNumberMul(N, Num1.Numerator, Num2.Denominator);
    BigNumberMul(Res.Denominator, Num1.Denominator, Num2.Numerator);
    BigNumberCopy(Res.Numerator, N);
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
    Result := BigNumberCompare(Num1.Numerator, Num2.Numerator)
  else
  begin
    //  同号，非整，比较
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
      Result := BigNumberCompare(Num1.Numerator, Res);
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
    BigNumberCopy(N.Numerator, Value);
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
    N.Numerator.SetInt64(Value);
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
    BigNumberCopy(TCnBigRational(Dest).Numerator, FNumerator);
    BigNumberCopy(TCnBigRational(Dest).Denominator, FDenominator);
  end
  else
    inherited;
end;

constructor TCnBigRational.Create;
begin
  FNumerator := TCnBigNumber.Create;
  FDenominator := TCnBigNumber.Create;
  FDenominator.SetOne;
  FNumerator.SetZero;
end;

destructor TCnBigRational.Destroy;
begin
  FDenominator.Free;
  FNumerator.Free;
  inherited;
end;

procedure TCnBigRational.Divide(Value: Int64);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    N.Numerator.SetInt64(Value);
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
    BigNumberCopy(N.Numerator, Value);
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
    Result := BigNumberCompare(Value, FNumerator) = 0
  else if FDenominator.IsNegOne then
    Result := (BigNumberUnsignedCompare(Value, FNumerator) = 0)
      and (FNumerator.IsNegative <> Value.IsNegative)
  else
    Result := False;
end;

function TCnBigRational.EqualInt(Value: Cardinal): Boolean;
begin
  if FDenominator.IsOne then
    Result := FNumerator.IsWord(Value)
  else if FDenominator.IsNegOne then
    Result := BigNumberAbsIsWord(FNumerator, Value) and FNumerator.IsNegative
  else
    Result := False;
end;

function TCnBigRational.IsInt: Boolean;
begin
  Result := FDenominator.IsOne or FDenominator.IsNegOne;
end;

function TCnBigRational.IsNegative: Boolean;
begin
  Result := FNumerator.IsNegative <> FDenominator.IsNegative;
end;

function TCnBigRational.IsOne: Boolean;
begin
  Result := not FNumerator.IsZero and (BigNumberCompare(FNumerator, FDenominator) = 0);
end;

function TCnBigRational.IsZero: Boolean;
begin
  Result := FNumerator.IsZero;
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
    BigNumberCopy(N.Numerator, Value);
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
    N.Numerator.SetInt64(Value);
    BigRationalNumberMul(Self, Self, N);
  finally
    FLocalBigRationalPool.Recycle(N);
  end;
end;

procedure TCnBigRational.Neg;
begin
  FNumerator.SetNegative(not FNumerator.IsNegative);
  if FNumerator.IsNegative and FDenominator.IsNegative then
  begin
    FNumerator.SetNegative(False);
    FDenominator.SetNegative(False);
  end;
end;

procedure TCnBigRational.Reciprocal;
var
  T: TCnBigNumber;
begin
  if FNumerator.IsZero then
    raise EDivByZero.Create(SDivByZero);

  T := FLocalBigNumberPool.Obtain;
  try
    BigNumberCopy(T, FDenominator);
    BigNumberCopy(FDenominator, FNumerator);
    BigNumberCopy(FNumerator, T);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

procedure TCnBigRational.Reduce;
begin
  if FDenominator.IsNegative and FNumerator.IsNegative then
  begin
    FDenominator.SetNegative(False);
    FNumerator.SetNegative(False);
  end
  else if FDenominator.IsNegative and not FNumerator.IsNegative then  // 分母的负号移到分子
  begin
    FDenominator.SetNegative(False);
    FNumerator.SetNegative(True);
  end;

  if FNumerator.IsZero then
  begin
    FDenominator.SetOne;
    Exit;
  end;

  if not IsInt then
    ReduceBigNumber(FNumerator, FDenominator);
end;

procedure TCnBigRational.SetIntValue(Value: Cardinal);
begin
  FNumerator.SetWord(Value);
  FDenominator.SetOne;
end;

procedure TCnBigRational.SetFloat(AFloat: Extended);
var
  F: TFloatRec;
  I, L: Integer;
begin
  // 分离出符号位、有效数字与指数
  FloatToDecimal(F, AFloat, fvExtended, 18, 9999);

  L := StrLen(PAnsiChar(@F.Digits[0]));
  // 分母是 10 的 L - F.Exponent 次方，分子是纯的 Digits
  FDenominator.SetOne;
  for I := 1 to L - F.Exponent do
    FDenominator.MulWord(10);

  FNumerator.SetDec(PAnsiChar(@F.Digits[0]));
  FNumerator.SetNegative(F.Negative);
  Reduce;
end;

procedure TCnBigRational.SetIntValue(Value: TCnBigNumber);
begin
  BigNumberCopy(FNumerator, Value);
  FDenominator.SetOne;
end;

procedure TCnBigRational.SetOne;
begin
  FNumerator.SetOne;
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
    FNumerator.SetDec(AnsiString(N));
    FDenominator.SetDec(AnsiString(D));
    Reduce;
  end
  else
  begin
    P := Pos('.', Value);
    if P > 1 then
    begin
      // 处理小数点
      N := Copy(Value, 1, P - 1);
      D := Copy(Value, P + 1, MaxInt);
      FNumerator.SetDec(AnsiString(N + D));
      FDenominator.SetOne;
      for P := 1 to Length(D) do
        FDenominator.MulWord(10);
      Reduce;
    end
    else
    begin
      FNumerator.SetDec(AnsiString(Value));
      FDenominator.SetOne;
    end;
  end;
end;

procedure TCnBigRational.SetValue(ANumerator, ADenominator: TCnBigNumber);
begin
  BigNumberCopy(FNumerator, ANumerator);
  BigNumberCopy(FDenominator, ADenominator);
end;

procedure TCnBigRational.SetValue(const ANumerator, ADenominator: string);
begin
  FNumerator.SetDec(AnsiString(ANumerator));
  FDenominator.SetDec(AnsiString(ADenominator));
end;

procedure TCnBigRational.SetZero;
begin
  FNumerator.SetZero;
  FDenominator.SetOne;
end;

procedure TCnBigRational.Sub(Value: Int64);
var
  N: TCnBigRational;
begin
  N := FLocalBigRationalPool.Obtain;
  try
    N.Denominator.SetOne;
    N.Numerator.SetInt64(Value);
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
    BigNumberCopy(N.Numerator, Value);
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

  // 基本思想是先除，得到整数部分，如果有余数，就计数加 0 求余
  try
    if IsInt then
    begin
      Result := FNumerator.ToDec;
      Exit;
    end;

    IsNeg := IsNegative;
    if IsNeg then
      Neg;

    Remain := FLocalBigNumberPool.Obtain;
    Res := FLocalBigNumberPool.Obtain;

    BigNumberDiv(Res, Remain, FNumerator, FDenominator);
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
      // Remain * 10，如果够除就商，不够就加 0，下一轮继续乘 10
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
    Result := FNumerator.ToDec
  else
    Result := FNumerator.ToDec + ' / ' + FDenominator.ToDec;
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
