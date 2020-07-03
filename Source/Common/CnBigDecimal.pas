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

unit CnBigDecimal;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：大浮点数算法单元
* 单元作者：刘啸
* 备    注：用 CnBigNumber 表示有效数字，用 Integer 表示正负指数
*           部分参考 Rudy Velthuis 的 BigDecimal 以及 Java 的 BigDecimal
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.06.25 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, SysConst, Math,
  CnNativeDecl, CnFloatConvert, CnBigNumber;

const
  CN_BIG_DECIMAL_DEFAULT_PRECISION = 12;                       // 乘除法的小数点后的默认精度

type
  ECnBigDecimalException = class(Exception);

  TCnBigDecimalRoundMode = (
  {* 取整的模式，包括六种，不处理四舍六入五成单的特殊需求
    注意：四舍五入的入只有入至绝对值大的情况，没有正负无穷的情况，因为舍动作必然是往绝对值小的数取}
    drAwayFromZero,            // 往绝对值大的数取
    drTowardsZero,             // 往绝对值小的数取，等于只留整数部分的 Trunc
    drCeilingToInfinite,       // 往正无穷大取
    drFloorToNegInfinite,      // 往负无穷大取
    dr45Round,                 // 四舍五入、入至绝对值大的数
    dr465RoundEven             // 四舍六入五成双、入至绝对值大的数
  );

  TCnBigDecimal = class
  {* 大浮点数实现类，用 CnBigNumber 保存有效数字，用 Integer 保存指数也就是小数点位置
    FScale 代表小数点离有效数字最右边的位置，往左为正，往右为负，
    正时简而言之就是小数点后有 FScale 位，负时简而言之还要加 -FScale 个 0}
  private
    FValue: TCnBigNumber;
    FScale: Integer;                 // 精确值为 FValue / (10^FScale)
    function GetDecString: string;
    function GetDebugDump: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetZero;
    {* 设置成 0}
    procedure SetOne;
    {* 设置成 1}
    procedure SetNegative(Neg: Boolean);
    {* 设置是否负数}
    procedure Negate;
    {* 负号设置反}

    function SetWord(W: LongWord): Boolean;
    {* 设置为一个 UInt32}
    function SetInt64(W: Int64): Boolean;
    {* 设置为一个 Int64}
    function SetDec(const Buf: string): Boolean;
    {* 设置字符串值}
    procedure SetSingle(Value: Single);
    {* 单精度浮点值}
    procedure SetDouble(Value: Double);
    {* 双精度浮点值}
    procedure SetExtended(Value: Extended);
    {* 扩展精度浮点值}

    procedure AddWord(W: LongWord);
    {* 加上一个 UInt32}
    procedure SubWord(W: LongWord);
    {* 减去一个 UInt32}
    procedure MulWord(W: LongWord);
    {* 乘以一个 UInt32}
    procedure DivWord(W: LongWord);
    {* 除以一个 UInt32}

    function IsNegative: Boolean;
    {* 是否负数}
    function IsZero: Boolean;
    {* 是否是 0}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大数转成字符串}

//    class function GetMulDivPrecision: Integer;
//    class function SetMulDivPrecision(Value: Integer);

    property DecString: string read GetDecString;
    property DebugDump: string read GetDebugDump;
  end;

  TCnBigDecimalPool = class(TObjectList)
  {* 大浮点数池实现类，允许使用到大浮点数的地方自行创建大浮点数池}
  private
  public
    constructor Create;
    destructor Destroy; override;

    function Obtain: TCnBigDecimal;
    procedure Recycle(Num: TCnBigDecimal);
  end;

procedure BigDecimalClear(const Num: TCnBigDecimal);
{* 清空一个大浮点数，实质上是 Value 与 Scale 都塞 0}

function BigDecimalSetDec(const Buf: string; const Res: TCnBigDecimal): Boolean;
{* 为大浮点数对象设置字符串值}

function BigDecimalSetWord(W: LongWord; const Res: TCnBigDecimal): Boolean;
{* 为大浮点数对象设置整数值}

function BigDecimalSetInt64(W: Int64; const Res: TCnBigDecimal): Boolean;
{* 为大浮点数对象设置 Int64 整数值}

function BigDecimalSetSingle(const Value: Single; const Res: TCnBigDecimal): Boolean;
{* 为大浮点数对象设置单精度浮点值}

function BigDecimalSetDouble(const Value: Double; const Res: TCnBigDecimal): Boolean;
{* 为大浮点数对象设置双精度浮点值}

function BigDecimalSetExtended(const Value: Extended; const Res: TCnBigDecimal): Boolean;
{* 为大浮点数对象设置扩展精度浮点值}

function BigDecimalToString(const Num: TCnBigDecimal): string;
{* 大浮点数对象转换为字符串}

function BigDecimalCompare(const Num1, Num2: TCnBigDecimal): Integer; overload;
{* 比较两个大浮点数对象}

function BigDecimalCompare(const Num1: TCnBigDecimal; Num2: Int64): Integer; overload;
{* 比较大浮点数对象与整数}

procedure BigDecimalCopy(const Dest, Source: TCnBigDecimal);
{* 大浮点数赋值}

function BigDecimalGetPrecision(const Num: TCnBigDecimal): Integer;
{* 计算大浮点数的十进制位数，也即有效数字长度}

function BigDecimalGetIntDecimalCount(const Num: TCnBigDecimal;
  out IntCount: Integer; out DecimalCount: Integer): Boolean;
{* 计算大浮点数的整数部分长度与小数部分长度}

function BigDecimalGetHighScale(const Num: TCnBigDecimal): Integer;
{* 计算大浮点数的最高有效数字位是小数点后第几位，如果返回小于 0，则求负后表示是小数点前第几位}

function BigDecimalAdd(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal): Boolean;
{* 大浮点数加，Res 可以是 Num1 或 Num2，Num1 可以是 Num2}

function BigDecimalSub(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal): Boolean;
{* 大浮点数减，Res 可以是 Num1 或 Num2，Num1 可以是 Num2}

function BigDecimalMul(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal; MulPrecision: Integer = 0): Boolean;
{* 大浮点数乘，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
  MulPrecision 表示乘法最多保留小数点后几位，0 表示全保留}

function BigDecimalDiv(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal; DivPrecision: Integer = 0): Boolean;
{* 大浮点数除，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
  DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来}

function BigDecimalChangeToScale(const Res: TCnBigDecimal; const Num: TCnBigDecimal;
  Scale: Integer; RoundMode: TCnBigDecimalRoundMode = drTowardsZero): Boolean;
{* 将大浮点数按指定模式舍入到指定 Scale，也就是小数点后 Scale 位，如果 Scale 负代表小数点左方，也就是舍入到整 10 次方}

function BigDecimalRoundToDigits(const Res: TCnBigDecimal; const Num: TCnBigDecimal;
  Digits: Integer; RoundMode: TCnBigDecimalRoundMode = drTowardsZero): Boolean;
{* 将大浮点数按指定模式舍入到指定小数点后 Digits 位，如果本身精度不够 Digits 位则不变}

function BigDecimalTrunc(const Res: TCnBigDecimal; const Num: TCnBigDecimal): Boolean;
{* 将大浮点数 Trunc 到只剩整数}

function BigDecimalDebugDump(const Num: TCnBigDecimal): string;
{* 打印大浮点数内部信息}

var
  CnBigDecimalOne: TCnBigDecimal = nil;     // 表示 1 的常量
  CnBigDecimalZero: TCnBigDecimal = nil;    // 表示 0 的常量

implementation

resourcestring
  SCnNotImplemented = 'NOT Implemented.';
  SCnScaleOutOfRange = 'Scale Out of Range.';

const
  SCN_FIVE_POWER_UINT32 = 13;
  SCN_POWER_FIVES32: array[0..13] of LongWord = (
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
  SCN_POWER_TENS32: array[0..9] of LongWord = (
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

//const
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
//    // 10 ^ 19 10000000000000000000 已经超了 Int64 9223372036854775807
//    // 所以得用 16 进制写但没超 UInt64 18446744073709551615，10 ^ 20 才超
//  );

var
  FLocalBigDecimalPool: TCnBigDecimalPool = nil;
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FDefaultPrecisionDigits: Integer = CN_BIG_DECIMAL_DEFAULT_PRECISION;

function CheckScaleAddRange(Scale1, Scale2: Integer): Integer;
begin
  if IsInt32AddOverflow(Scale1, Scale2) then
    raise ECnBigDecimalException.Create(SCnScaleOutOfRange);
  Result := Scale1 + Scale2;
end;

procedure RoundByMode(Quotient, Divisor, Remainder: TCnBigNumber; QWillBeNeg: Boolean;
  Mode: TCnBigDecimalRoundMode);
var
  R2: TCnBigNumber;
  R2CD: Integer;
begin
  if Remainder.IsZero then
    Exit;

  case Mode of
    drAwayFromZero:            // 往绝对值大的数取
      begin
        BigNumberAddWord(Quotient, 1);
      end;
    drTowardsZero:             // 往绝对值小的数取，等于只留整数部分的 Trunc
      begin
        // 啥都不用做
      end;
    drCeilingToInfinite:       // 往正无穷大取
      begin
        if not QWillBeNeg then
          BigNumberAddWord(Quotient, 1);
      end;
    drFloorToNegInfinite:      // 往负无穷大取
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

      // 四舍五入模式下，R2CD 如果大于等于 0，说明余数大于等于 5，要入
      // 四舍六入模式下，如果等于 1，要判断商的末位是否偶，偶才入，其余情况不入
      // 确定入与否后，无需再根据正负处理，舍都是朝绝对值小的方向，入是绝对值大的方向
      case Mode of
        dr45Round:         // 四舍五入、入至绝对值大的数
          begin
            if R2CD >= 0 then
              BigNumberAddWord(Quotient, 1);
          end;
        dr465RoundEven:     // 四舍六入五成双、入至绝对值大的数
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

// 大数乘以 10 的 Power5 次方
procedure BigNumberMulPower5(Num: TCnBigNumber; Power5: Integer);
var
  I, L, D, R: Integer;
begin
  if Power5 < 0 then
    raise ECnBigDecimalException.Create(SCnNotImplemented);

  L := High(SCN_POWER_FIVES32);       // 一次能整 13 个
  D := Power5 div L;
  R := Power5 mod L;

  for I := 1 to D do                  // 一次整 13 个乘
    Num.MulWord(SCN_POWER_FIVES32[L]);
  Num.MulWord(SCN_POWER_FIVES32[R]);  // 补上乘剩下的
end;

// 大数乘以 10 的 Power10 次方
procedure BigNumberMulPower10(Num: TCnBigNumber; Power10: Integer);
var
  I, L, D, R: Integer;
begin
  if Power10 < 0 then
    raise ECnBigDecimalException.Create(SCnNotImplemented);

  L := High(SCN_POWER_TENS32);       // 一次能整 9 个
  D := Power10 div L;
  R := Power10 mod L;

  for I := 1 to D do                 // 一次整 9 个乘
    Num.MulWord(SCN_POWER_TENS32[L]);
  Num.MulWord(SCN_POWER_TENS32[R]);  // 补上乘剩下的
end;

procedure BigDecimalClear(const Num: TCnBigDecimal);
begin
  if Num <> nil then
  begin
    Num.FScale := 0;
    Num.FValue.SetZero;
  end;
end;

function BigDecimalSetDec(const Buf: string; const Res: TCnBigDecimal): Boolean;
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

  // 解析值，直到结尾或碰上科学计数法的 E
  C := P^;
  while (C <> #0) and (C <> 'e') and (C <> 'E') do
  begin
    case C of
      '0'..'9':
        V := V + C;
      ',':
        ; // 分节号忽略
      '.':
        if Assigned(DotPos) then
          // 小数点只能有一个
          Exit
        else
          DotPos := P;
    else
      Exit;
    end;
    Inc(P);
    C := P^;
  end;

  // V 是不包括小数点的十进制字符串

  // 如果数据中原来有小数点，则给 DC 赋值
  if Assigned(DotPos) then
    DC := P - DotPos - 1;

  E := 0;
  if (C = 'e') or (C = 'E') then
  begin
    // 科学计数法的 E 后面的指数
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
  DC := DC - E; // 结合指数一起计算小数部分长度给 DC

  Res.FScale := DC;
  Res.FValue.SetDec(V);

  if (not Res.FValue.IsNegative) and Neg then
    Res.FValue.SetNegative(True);

  Result := True;
end;

function BigDecimalSetWord(W: LongWord; const Res: TCnBigDecimal): Boolean;
begin
  Res.FValue.SetWord(W);
  Res.FScale := 0;
  Result := True;
end;

function BigDecimalSetInt64(W: Int64; const Res: TCnBigDecimal): Boolean;
begin
  Res.FValue.SetInt64(W);
  Res.FScale := 0;
  Result := True;
end;

function InternalBigDecimalSetFloat(Neg: Boolean; IntExponent: Integer; IntMantissa: TUInt64;
  const Res: TCnBigDecimal): Boolean;
var
  C: Integer;
begin
  C := GetUInt64LowBits(IntMantissa);
  if C > 0 then
  begin
    IntMantissa := IntMantissa shr C;
    Inc(IntExponent, C);
  end;

  // 值是 IntMantissa * 2^IntExponent
  BigNumberSetUInt64UsingInt64(Res.FValue, IntMantissa);
  if IntExponent > 0 then
  begin
    Res.FValue.ShiftLeft(IntExponent);   // 直接算出大整数结果，指数变成 0
    Res.FScale := 0;
  end
  else // 指数是负数说明有小数部分，那么每个除以 2 的要变成除以 10，IntMantissa 就得针对每个指数乘以 5
  begin
    IntExponent := -IntExponent;
    Res.FScale := IntExponent;
    BigNumberMulPower5(Res.FValue, IntExponent);
  end;

  Res.FValue.SetNegative(Neg);
  Result := True;
end;

function BigDecimalSetSingle(const Value: Single; const Res: TCnBigDecimal): Boolean;
var
  N: Boolean;
  E: Integer;
  S: LongWord;
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
  // 把 1. 开头的有效数字当成整数，E 需要减 23
  Result := InternalBigDecimalSetFloat(N, E - 23, TUInt64(S), Res);
end;

function BigDecimalSetDouble(const Value: Double; const Res: TCnBigDecimal): Boolean;
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
  // 把 1. 开头的有效数字当成整数，E 需要减 52
  Result := InternalBigDecimalSetFloat(N, E - 52, S, Res);
end;

function BigDecimalSetExtended(const Value: Extended; const Res: TCnBigDecimal): Boolean;
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
  // 把 1. 开头的有效数字当成整数，E 需要减 63
  Result := InternalBigDecimalSetFloat(N, E - 63, S, Res);
end;

function BigDecimalToString(const Num: TCnBigDecimal): string;
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

  // 抛弃正负号先
  C := #0;
  if (S[1] = '-') or (S[1] = '+') then
  begin
    C := S[1];
    Delete(S, 1, 1);
    Dec(L);
  end;

  // 确定小数点位置
  if Num.FScale < 0 then
    Result := S + StringOfChar('0', -Num.FScale)
  else if Num.FScale = 0 then
    Result := S
  else if Num.FScale >= L then
    Result := '0.' + StringOfChar('0', Num.FScale - L) + S
  else
    Result := Copy(S, 1, L - Num.FScale) + '.' + Copy(S, L - Num.FScale + 1, MaxInt);

  // 再把正负号加回来
  if C <> #0 then
    Result := C + Result;
end;

function BigDecimalCompare(const Num1, Num2: TCnBigDecimal): Integer;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    if Num2.FValue.IsZero then
      Result := 0   // 都是 0，相等
    else if Num2.FValue.IsNegative then
      Result := 1   // 0 大于负
    else
      Result := -1; // 0 小于正
  end
  else if Num2.FValue.IsZero then
  begin
    if not Num1.FValue.IsNegative then
      Result := 1     // 正大于 0
    else
      Result := -1;   // 负小于 0
  end
  else if Num1.FValue.IsNegative and not Num2.FValue.IsNegative then // 都不为 0，负小于正
    Result := -1
  else if not Num1.FValue.IsNegative and Num2.FValue.IsNegative then // 都不为 0，正大于负
    Result := 1
  else if Num1.FScale = Num2.FScale then // 符号相同，先看指数是否相同
    Result := BigNumberCompare(Num1.FValue, Num2.FValue)
  else // 符号相同，指数不同
  begin
    // 要把 Scale 大的也就是小数点靠左因而可能相对较小的 Value，
    // 乘以 10 的指数差次幂以对齐小数点，再和另一个比较（无需保持值不变，所以和加减有区别）
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

function BigDecimalCompare(const Num1: TCnBigDecimal; Num2: Int64): Integer; overload;
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

procedure BigDecimalCopy(const Dest, Source: TCnBigDecimal);
begin
  if (Source <> nil) and (Dest <> nil) and (Source <> Dest) then
  begin
    BigNumberCopy(Dest.FValue, Source.FValue);
    Dest.FScale := Source.FScale;
  end;
end;

function BigDecimalGetPrecision(const Num: TCnBigDecimal): Integer;
begin
  Result := 0;
  if Num <> nil then
    Result := BigNumberGetTenPrecision(Num.FValue); // 得到十进制整数位数
end;

function BigDecimalGetIntDecimalCount(const Num: TCnBigDecimal;
  out IntCount: Integer; out DecimalCount: Integer): Boolean;
var
  P: Integer;
begin
  Result := False;
  if Num <> nil then
  begin
    P := BigNumberGetTenPrecision(Num.FValue);
    if Num.FScale > 0 then  // 有小数部分
    begin
      DecimalCount := Num.FScale;
      IntCount := P - DecimalCount;
      if IntCount < 0 then
        IntCount := 0;
    end
    else
    begin
      // 没有小数部分
      DecimalCount := 0;
      IntCount := P + Num.FScale;
    end;
    Result := True;
  end;
end;

function BigDecimalGetHighScale(const Num: TCnBigDecimal): Integer;
begin
  Result := 0;
  if Num <> nil then
  begin
    Result := BigNumberGetTenPrecision(Num.FValue);
    // 小数点后有 FScale 位，减去有效数字
    Result := Num.FScale - Result + 1;
    if Result <= 0 then // 小数点前第几位是从 1 开始的
      Dec(Result)
  end;
end;

function BigDecimalAdd(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal): Boolean;
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
    // 指数相同直接加
    Res.FScale := Num1.FScale;
    Result := BigNumberAdd(Res.FValue, Num1.FValue, Num2.FValue);
    Exit;
  end
  else
  begin
    // 要把 Scale 小的也就是小数点靠右因而可能相对较大的 Value，
    // 乘以 10 的指数差次幂并减小到同等的 Scale 以对齐小数点并保持总值不变，
    // 再和另一个相加，结果的 Scale 取小的
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

function BigDecimalSub(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal): Boolean;
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
    // 指数相同直接减
    Res.FScale := Num1.FScale;
    Result := BigNumberSub(Res.FValue, Num1.FValue, Num2.FValue);
    Exit;
  end
  else
  begin
    // 要把 Scale 小的也就是小数点靠右因而可能相对较大的 Value，
    // 乘以 10 的指数差次幂并减小到同等的 Scale 以对齐小数点并保持总值不变，
    // 再和另一个相减，结果的 Scale 取小的
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

function BigDecimalMul(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal; MulPrecision: Integer): Boolean;
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

function BigDecimalDiv(const Res: TCnBigDecimal; const Num1: TCnBigDecimal;
  const Num2: TCnBigDecimal; DivPrecision: Integer): Boolean;
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

  // 继续除
  S := Num1.FValue.isNegative <> Num2.FValue.IsNegative; // 符号不等结果才负
  TS := Num1.FScale - Num2.FScale;

  if DivPrecision <= 0 then
    DivPrecision := FDefaultPrecisionDigits;
  if DivPrecision < 0 then
    DivPrecision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  // 根据精度要求计算将被除数和除数同时扩大的倍数，注意存在乘以 9 时就可能溢出的情况但先不管
  M := CheckScaleAddRange(DivPrecision, (Num2.FValue.Top - Num2.FValue.Top + 1) * 9 + 3);
  TS := CheckScaleAddRange(TS, M);

  T := nil;
  R := nil;
  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberCopy(T, Num1.FValue);
    BigNumberMulPower10(T, M);

    R := FLocalBigNumberPool.Obtain;
    BigNumberDiv(Res.FValue, R, T, Num2.FValue);  // Num1.FValue * 10 ^ M div Num2.FValue 得到商和余数

    RoundByMode(Res.FValue, Num2.FValue, R, Res.FValue.IsNegative, drTowardsZero);
    Res.FScale := TS;
    // TODO: 十进制约分

    BigDecimalRoundToDigits(Res, Res, DivPrecision, drTowardsZero);
    Res.FValue.SetNegative(S);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(R);
  end;
end;

function BigDecimalChangeToScale(const Res: TCnBigDecimal; const Num: TCnBigDecimal;
  Scale: Integer; RoundMode: TCnBigDecimalRoundMode): Boolean;
var
  DS: Integer;
  D, Q, R: TCnBigNumber;
  Neg: Boolean;
begin
  DS := CheckScaleAddRange(Num.FScale, -Scale);
  if DS > 0 then // 新的小数点后的位数比原来少，要除之后舍入
  begin
    D := FLocalBigNumberPool.Obtain;
    Q := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    try
      D.SetOne;
      BigNumberMulPower10(D, DS);  // 算出个 10 的 DS 次方，做除数

      Neg := Num.FValue.IsNegative;
      Num.FValue.SetNegative(False);

      // 除出商和余数来
      BigNumberDiv(Q, R, Num.FValue, D);

      // 根据商和余数以及规则决定舍入
      RoundByMode(Q, D, R, Neg, RoundMode);

      BigNumberCopy(Res.FValue, Q);
      Res.FScale := Scale;
      Res.FValue.SetNegative(Neg);

      if Res <> Num then           // 如果 Num 是独立的，这里要还原其 Neg
        Num.FValue.SetNegative(Neg);
      Result := True;
    finally
      FLocalBigNumberPool.Recycle(D);
      FLocalBigNumberPool.Recycle(Q);
      FLocalBigNumberPool.Recycle(R);
    end;
  end
  else // 新的小数点位数比原来还多，简单变换一下就行
  begin
    BigNumberCopy(Res.FValue, Num.FValue);
    if DS < 0 then
      BigNumberMulPower10(Res.FValue, -DS);
    Res.FScale := Scale;
    Result := True;
  end;
end;

function BigDecimalRoundToDigits(const Res: TCnBigDecimal; const Num: TCnBigDecimal;
  Digits: Integer; RoundMode: TCnBigDecimalRoundMode = drTowardsZero): Boolean;
var
  DS: Integer;
  D, Q, R: TCnBigNumber;
  Neg: Boolean;
begin
  Result := False;
  DS := CheckScaleAddRange(Num.FScale, -Digits);

  if DS > 0 then // 新的小数点后的位数得比原来少，才能除之后舍入
  begin
    D := FLocalBigNumberPool.Obtain;
    Q := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    try
      D.SetOne;
      BigNumberMulPower10(D, DS);  // 算出个 10 的 DS 次方，做除数

      Neg := Num.FValue.IsNegative;
      Num.FValue.SetNegative(False);

      // 除出商和余数来
      BigNumberDiv(Q, R, Num.FValue, D);

      // 根据商和余数以及规则决定舍入
      RoundByMode(Q, D, R, Neg, RoundMode);

      BigNumberCopy(Res.FValue, Q);
      Res.FScale := Digits;
      Res.FValue.SetNegative(Neg);

      if Res <> Num then           // 如果 Num 是独立的，这里要还原其 Neg
        Num.FValue.SetNegative(Neg);
      Result := True;
    finally
      FLocalBigNumberPool.Recycle(D);
      FLocalBigNumberPool.Recycle(Q);
      FLocalBigNumberPool.Recycle(R);
    end;
  end;
end;

function BigDecimalTrunc(const Res: TCnBigDecimal; const Num: TCnBigDecimal): Boolean;
begin
  if Num.FScale <= 0 then // 无小数部分
  begin
    BigDecimalCopy(Res, Num);
    Result := True;
    Exit;
  end
  else // 有小数部分 FScale 位，干掉
  begin
    Result := BigDecimalChangeToScale(Res, Num, 0, drTowardsZero);
  end;
end;

function BigDecimalDebugDump(const Num: TCnBigDecimal): string;
begin
  Result := 'Scale: ' + IntToStr(Num.FScale) + '. ' + BigNumberDebugDump(Num.FValue);
end;

{ TCnBigDecimal }

procedure TCnBigDecimal.AddWord(W: LongWord);
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

procedure TCnBigDecimal.DivWord(W: LongWord);
var
  T: TCnBigDecimal;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    T.SetWord(W);
    BigDecimalDiv(Self, Self, T);
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

function TCnBigDecimal.IsZero: Boolean;
begin
  Result := FValue.IsZero;
end;

procedure TCnBigDecimal.MulWord(W: LongWord);
begin
  FValue.MulWord(W);
end;

procedure TCnBigDecimal.Negate;
begin
  FValue.Negate;
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

function TCnBigDecimal.SetWord(W: LongWord): Boolean;
begin
  Result := BigDecimalSetWord(W, Self);
end;

procedure TCnBigDecimal.SetZero;
begin
  FValue.SetZero;
  FScale := 0;
end;

procedure TCnBigDecimal.SubWord(W: LongWord);
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

constructor TCnBigDecimalPool.Create;
begin
  inherited Create(False);
end;

destructor TCnBigDecimalPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited;
end;

function TCnBigDecimalPool.Obtain: TCnBigDecimal;
begin
  if Count = 0 then
  begin
    Result := TCnBigDecimal.Create
  end
  else
  begin
    Result := TCnBigDecimal(Items[Count - 1]);
    Delete(Count - 1);
    Result.SetZero;
  end;
end;

procedure TCnBigDecimalPool.Recycle(Num: TCnBigDecimal);
begin
  if Num <> nil then
    Add(Num);
end;

initialization
  FLocalBigDecimalPool := TCnBigDecimalPool.Create;
  FLocalBigNumberPool := TCnBigNumberPool.Create;

  CnBigDecimalOne := TCnBigDecimal.Create;
  CnBigDecimalOne.SetOne;
  CnBigDecimalZero := TCnBigDecimal.Create;
  CnBigDecimalZero.SetZero;

finalization
  CnBigDecimalZero.DecString; // 手工调用这两句防止被编译器忽略
  CnBigDecimalZero.DebugDump;

  CnBigDecimalZero.Free;
  CnBigDecimalOne.Free;

  FLocalBigNumberPool.Free;
  FLocalBigDecimalPool.Free;

end.
