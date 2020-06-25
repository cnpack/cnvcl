{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnBigRational;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：无限精度有理数实现单元
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：用大整数的比值表示有理数
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2019.12.19 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnBigNumber;

type
  TCnBigRational = class(TPersistent)
  {* 表示一个无限精度的大有理数}
  private
    FNominator: TCnBigNumber;
    FDenominator: TCnBigNumber;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function IsInt: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否整数，也就是判断分母是否是正负 1}
    function IsZero: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 0}
    function IsOne: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 1}
    function IsNegative: Boolean;
    {* 是否为负值}
    procedure Neg;
    {* 变成相反数}
    procedure Reciprocal;
    {* 变成倒数}
    procedure SetZero;
    {* 设为 0}
    procedure SetOne;
    {* 设为 1}

    function EqualInt(Value: LongWord): Boolean; overload;
    {* 是否与另一值相等}
    function EqualInt(Value: TCnBigNumber): Boolean; overload;
    {* 是否与另一值相等}
    function Equal(Value: TCnBigRational): Boolean;
    {* 是否与另一值相等}

    procedure Add(Value: Int64); overload;
    {* 加上一个整数}
    procedure Sub(Value: Int64); overload;
    {* 减去一个整数}
    procedure Mul(Value: Int64); overload;
    {* 乘以一个整数}
    procedure Divide(Value: Int64); overload;
    {* 除以一个整数}
    procedure Add(Value: TCnBigNumber); overload;
    {* 加上一个整数}
    procedure Sub(Value: TCnBigNumber); overload;
    {* 减去一个整数}
    procedure Mul(Value: TCnBigNumber); overload;
    {* 乘以一个整数}
    procedure Divide(Value: TCnBigNumber); overload;
    {* 除以一个整数}
    procedure Add(Value: TCnBigRational); overload;
    {* 加上一个有理数}
    procedure Sub(Value: TCnBigRational); overload;
    {* 减去一个有理数}
    procedure Mul(Value: TCnBigRational); overload;
    {* 乘以一个有理数}
    procedure Divide(Value: TCnBigRational); overload;
    {* 除以一个有理数}

    procedure SetIntValue(Value: LongWord); overload;
    {* 值设为一个整数}
    procedure SetIntValue(Value: TCnBigNumber); overload;
    {* 值设为一个整数}
    procedure SetValue(ANominator, ADenominator: TCnBigNumber); overload;
    {* 值设为一个分数}
    procedure SetValue(const ANominator, ADenominator: string); overload;
    {* 值设为一个分数，数字用字符串的方式输入}
    procedure SetString(const Value: string);
    {* 值设为一个字符串，可以是纯数字，或带 / 的分数，或小数}
    procedure SetFloat(AFloat: Extended);
    {* 值设为一个浮点数，把浮点数的有效数字和指数拆开处理}
    procedure Reduce;
    {* 尽量约分}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 输出成字符串}
    function ToDec(Digits: Integer = 20): string;
    {* 输出成小数，默认留小数点后 20 位精度}

    property Nominator: TCnBigNumber read FNominator;
    {* 分子}
    property Denominator: TCnBigNumber read FDenominator;
    {* 分母}
  end;

// ============================= 大有理数运算方法 ==============================

procedure CnBigRationalNumberAdd(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
{* 大有理数加法，三数可以相等}

procedure CnBigRationalNumberSub(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
{* 大有理数减法，三数可以相等}

procedure CnBigRationalNumberMul(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
{* 大有理数乘法，三数可以相等}

procedure CnBigRationalNumberDiv(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
{* 大有理数除法，三数可以相等}

function CnBigRationalNumberCompare(Number1, Number2: TCnBigRational): Integer; overload;
{* 大有理数比较，> = < 分别返回 1 0 -1}

function CnBigRationalNumberCompare(Number1: TCnBigRational; Number2: Int64): Integer; overload;
{^ 大有理数与整数比较，> = < 分别返回 1 0 -1}

procedure CnReduceBigNumber(X, Y: TCnBigNumber);
{* 尽量比例缩小，也就是约分}

var
  CnBigRationalNumberOne: TCnBigRational = nil;
  CnBigRationalNumberZero: TCnBigRational = nil;

implementation

procedure CnBigRationalNumberAdd(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, R, F1, F2, D1, D2: TCnBigNumber;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    BigNumberAdd(RationalResult.Nominator, Number1.Nominator, Number2.Nominator);
    Exit;
  end
  else if Number1.IsZero then
  begin
    if Number2 <> RationalResult then
      RationalResult.Assign(Number2);
  end
  else if Number2.IsZero then
  begin
    if Number1 <> RationalResult then
      RationalResult.Assign(Number1);
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
      M := TCnBigNumber.Create;
      R := TCnBigNumber.Create;
      F1 := TCnBigNumber.Create;
      F2 := TCnBigNumber.Create;
      D1 := TCnBigNumber.Create;
      D2 := TCnBigNumber.Create;

      BigNumberCopy(D1, Number1.Denominator);
      BigNumberCopy(D2, Number2.Denominator);

      B1 := Number1.Denominator.IsNegative;
      B2 := Number2.Denominator.IsNegative;

      D1.SetNegative(False);
      D2.SetNegative(False);

      BigNumberLcm(M, D1, D2);
      BigNumberDiv(F1, R, M, D1);
      BigNumberDiv(F2, R, M, D2);

      BigNumberCopy(RationalResult.Denominator, M);
      BigNumberMul(R, Number1.Nominator, F1);
      if B1 then
        R.SetNegative(not R.IsNegative);
      BigNumberMul(M, Number2.Nominator, F2);
      if B2 then
        M.SetNegative(not M.IsNegative);

      BigNumberAdd(RationalResult.Nominator, R, M);
    finally
      D2.Free;
      D1.Free;
      F2.Free;
      F1.Free;
      R.Free;
      M.Free;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnBigRationalNumberSub(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
begin
  Number2.Nominator.SetNegative(not Number2.Nominator.IsNegative);
  CnBigRationalNumberAdd(Number1, Number2, RationalResult);
  if RationalResult <> Number2 then
    Number2.Nominator.SetNegative(not Number2.Nominator.IsNegative);
end;

procedure CnBigRationalNumberMul(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
var
  N: TCnBigNumber;
begin
  N := TCnBigNumber.Create;
  try
    BigNumberMul(N, Number1.Nominator, Number2.Nominator);
    BigNumberMul(RationalResult.Denominator, Number1.Denominator, Number2.Denominator);
    BigNumberCopy(RationalResult.Nominator, N);
  finally
    N.Free;
  end;
  RationalResult.Reduce;
end;

procedure CnBigRationalNumberDiv(Number1, Number2: TCnBigRational; RationalResult: TCnBigRational);
var
  N: TCnBigNumber;
begin
  if Number2.IsZero then
    raise EDivByZero.Create('Divide by Zero.');

  N := TCnBigNumber.Create;
  try
    BigNumberMul(N, Number1.Nominator, Number2.Denominator);
    BigNumberMul(RationalResult.Denominator, Number1.Denominator, Number2.Nominator);
    BigNumberCopy(RationalResult.Nominator, N);
  finally
    N.Free;
  end;
  RationalResult.Reduce;
end;

function CnBigRationalNumberCompare(Number1, Number2: TCnBigRational): Integer;
var
  Res: TCnBigRational;
begin
  if not Number1.IsNegative and Number2.IsNegative then
    Result := 1
  else if Number1.IsNegative and not Number2.IsNegative then
    Result := -1
  else if Number1.IsZero and Number2.IsZero then
    Result := 0
  else if Number1.IsInt and Number2.IsInt then
    Result := BigNumberCompare(Number1.Nominator, Number2.Nominator)
  else
  begin
    //  同号，非整，比较
    Res := TCnBigRational.Create;
    try
      CnBigRationalNumberSub(Number1, Number2, Res);
      if Res.IsZero then
        Result := 0
      else if Res.IsNegative then
        Result := -1
      else
        Result := 1;
    finally
      Res.Free;
    end;
  end;
end;

function CnBigRationalNumberCompare(Number1: TCnBigRational; Number2: Int64): Integer;
var
  Res: TCnBigNumber;
begin
  if not Number1.IsNegative and (Number2 < 0) then
    Result := 1
  else if Number1.IsNegative and (Number2 > 0) then
    Result := -1
  else if Number1.IsZero and (Number2 = 0) then
    Result := 0
  else
  begin
    Res := TCnBigNumber.Create;
    try
      Res.SetInt64(Number2);
      if not Number1.IsInt then
        BigNumberMul(Res, Number1.Denominator, Res);
      Result := BigNumberCompare(Number1.Nominator, Res);
    finally
      Res.Free;
    end;
  end;
end;

procedure CnReduceBigNumber(X, Y: TCnBigNumber);
var
  N, R: TCnBigNumber;
begin
  N := TCnBigNumber.Create;
  if BigNumberGcd(N, X, Y) then
  begin
    if not N.IsOne then
    begin
      R := TCnBigNumber.Create;
      BigNumberDiv(X, R, X, N);
      BigNumberDiv(Y, R, Y, N);
      R.Free;
    end;
  end;
  N.Free;
end;

{ TCnBigRationalNumber }

procedure TCnBigRational.Add(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberAdd(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRational.Add(Value: Int64);
var
  N: TCnBigRational;
begin
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberAdd(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRational.Add(Value: TCnBigRational);
begin
  CnBigRationalNumberAdd(Self, Value, Self);
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
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberDiv(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRational.Divide(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberDiv(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRational.Divide(Value: TCnBigRational);
begin
  CnBigRationalNumberDiv(Self, Value, Self);
end;

function TCnBigRational.Equal(Value: TCnBigRational): Boolean;
begin
  Result := CnBigRationalNumberCompare(Self, Value) = 0;
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

function TCnBigRational.EqualInt(Value: LongWord): Boolean;
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
  Result := BigNumberCompare(FNominator, FDenominator) = 0;
end;

function TCnBigRational.IsZero: Boolean;
begin
  Result := FNominator.IsZero;
end;

procedure TCnBigRational.Mul(Value: TCnBigRational);
begin
  CnBigRationalNumberMul(Self, Value, Self);
end;

procedure TCnBigRational.Mul(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberMul(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRational.Mul(Value: Int64);
var
  N: TCnBigRational;
begin
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberMul(Self, N, Self);
  finally
    N.Free;
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
  T := TCnBigNumber.Create;
  BigNumberCopy(T, FDenominator);
  BigNumberCopy(FDenominator, FNominator);
  BigNumberCopy(FNominator, T);
end;

procedure TCnBigRational.Reduce;
begin
  if FDenominator.IsNegative and FNominator.IsNegative then
  begin
    FDenominator.SetNegative(False);
    FNominator.SetNegative(False);
  end
  else if FDenominator.IsNegative and not FNominator.IsNegative then  // 分母的负号移到分子
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
    CnReduceBigNumber(FNominator, FDenominator);
end;

procedure TCnBigRational.SetIntValue(Value: LongWord);
begin
  FNominator.SetWord(Value);
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
    FNominator.SetDec(N);
    FDenominator.SetDec(D);
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
      FNominator.SetDec(N + D);
      FDenominator.SetOne;
      for P := 1 to Length(D) do
        FDenominator.MulWord(10);
      Reduce;
    end
    else
    begin
      FNominator.SetDec(Value);
      FDenominator.SetOne;
    end;
  end;
end;

procedure TCnBigRational.SetValue(ANominator,
  ADenominator: TCnBigNumber);
begin
  BigNumberCopy(FNominator, ANominator);
  BigNumberCopy(FDenominator, ADenominator);
end;

procedure TCnBigRational.SetValue(const ANominator,
  ADenominator: string);
begin
  FNominator.SetDec(ANominator);
  FDenominator.SetDec(ADenominator);
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
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    N.Nominator.SetInt64(Value);
    CnBigRationalNumberSub(Self, N, Self);
  finally
    N.Free;
  end;
end;

procedure TCnBigRational.Sub(Value: TCnBigRational);
begin
  CnBigRationalNumberSub(Self, Value, Self);
end;

procedure TCnBigRational.Sub(Value: TCnBigNumber);
var
  N: TCnBigRational;
begin
  N := TCnBigRational.Create;
  try
    N.Denominator.SetOne;
    BigNumberCopy(N.Nominator, Value);
    CnBigRationalNumberSub(Self, N, Self);
  finally
    N.Free;
  end;
end;

function TCnBigRational.ToDec(Digits: Integer): string;
var
  Remain, Res: TCnBigNumber;
  I: Integer;
  R: string;
  IsNeg: Boolean;
begin
  Remain := TCnBigNumber.Create;
  Res := TCnBigNumber.Create;

  // 基本思想是先除，得到整数部分，如果有余数，就计数加0求余
  try
    if IsInt then
    begin
      Result := FNominator.ToDec;
      Exit;
    end;
    IsNeg := IsNegative;
    if IsNeg then
      Neg;

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
    Res.Free;
    Remain.Free;
  end;
end;

function TCnBigRational.ToString: string;
begin
  if FDenominator.IsOne then
    Result := FNominator.ToDec
  else
    Result := FNominator.ToDec + ' / ' + FDenominator.ToDec;
end;

initialization
  CnBigRationalNumberOne := TCnBigRational.Create;
  CnBigRationalNumberZero := TCnBigRational.Create;
  CnBigRationalNumberOne.SetOne;
  CnBigRationalNumberZero.SetZero;

finalization
  CnBigRationalNumberOne.Free;
  CnBigRationalNumberZero.Free;

end.
