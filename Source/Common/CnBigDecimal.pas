{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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
* 备    注：TCnBigDecimal 与 TCnBigBinary 均用 CnBigNumber 表示有效数字，
*           用 Integer 表示正负指数，所不同的是底分别为 10 和 2
*           部分参考 Rudy Velthuis 的 BigDecimal 以及 Java 的 BigDecimal
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2021.09.05 V1.2
*               增加一批 TCnBigBinary 操作函数
*           2020.07.08 V1.1
*               实现基于二进制的浮点数 TCnBigBinary
*           2020.06.25 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, SysConst, Math {$IFDEF MSWINDOWS}, Windows {$ENDIF},
  CnNativeDecl, CnFloatConvert, CnContainers, CnBigNumber;

const
  CN_BIG_DECIMAL_DEFAULT_PRECISION = 12;         // 大浮点数乘除法的小数点后的默认精度
  CN_BIG_BINARY_DEFAULT_PRECISION  = 32;         // 大二进制浮点数小数点后的默认精度

type
  ECnBigDecimalException = class(Exception);

  TCnBigRoundMode = (
  {* 大浮点数取整的模式，十进制包括六种，不处理四舍六入五成单的特殊需求，二进制包括前五种
    注意：四舍五入的入只有入至绝对值大的情况，没有正负无穷的情况，因为舍动作必然是往绝对值小的数取}
    drAwayFromZero,            // 往绝对值大的数取
    drTowardsZero,             // 往绝对值小的数取，等于只留整数部分的 Trunc
    drCeilingToInfinite,       // 往正无穷大取
    drFloorToNegInfinite,      // 往负无穷大取
    drRound,                   // 四舍五入（二进制模式下是 0 舍 1 入）、入至绝对值大的数
    dr465RoundEven             // 四舍六入五成双（不支持二进制模式）、入至绝对值大的数
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
    procedure DivWord(W: LongWord; DivPrecision: Integer = 0);
    {* 除以一个 UInt32。DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来}

    function IsNegative: Boolean;
    {* 是否负数}
    function IsZero: Boolean;
    {* 是否是 0}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大浮点数转成字符串}

    property DecString: string read GetDecString;
    property DebugDump: string read GetDebugDump;
  end;

  TCnBigDecimalPool = class(TCnMathObjectPool)
  {* 大浮点数池实现类，允许使用到大浮点数的地方自行创建大浮点数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigDecimal;
    procedure Recycle(Num: TCnBigDecimal);
  end;

  ECnBigBinaryException = class(Exception);

  TCnBigBinary = class
  {* 大二进制浮点数实现类，用 CnBigNumber 保存有效数字，用 Integer 保存基于 2 的指数
    FScale 代表二进制模式下小数点离有效数字最右边的位置，往左为正，往右为负，
    正时简而言之就是二进制模式下小数点后有 FScale 位，负时简而言之还要加 -FScale 个 0}
  private
    FValue: TCnBigNumber;
    FScale: Integer;                 // 精确值为 FValue / (2^FScale)，默认 FScale 为 0，也就是除以 1，等于不除
    function GetDebugDump: string;
    function GetDecString: string;
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
    procedure SetBigNumber(Value: TCnBigNumber);
    {* 大整数值}

    procedure AddWord(W: LongWord);
    {* 加上一个 UInt32}
    procedure SubWord(W: LongWord);
    {* 减去一个 UInt32}
    procedure MulWord(W: LongWord);
    {* 乘以一个 UInt32}
    procedure DivWord(W: LongWord; DivPrecision: Integer = 0);
    {* 除以一个 UInt32。DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来}

    procedure ShiftLeft(N: Integer);
    {* 左移 N 位}
    procedure ShiftRight(N: Integer);
    {* 右移 N 位}

    procedure Power(N: Integer);
    {* 求幂}

    function IsNegative: Boolean;
    {* 是否负数}
    function IsZero: Boolean;
    {* 是否是 0}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大浮点数转成字符串}

    property DecString: string read GetDecString;
    property DebugDump: string read GetDebugDump;
  end;

  TCnBigBinaryPool = class(TCnMathObjectPool)
  {* 大浮点数池实现类，允许使用到大浮点数的地方自行创建大浮点数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigBinary; reintroduce;
    procedure Recycle(Num: TCnBigBinary); reintroduce;
  end;

// ======================== 大浮点数操作函数 ===================================

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

function BigDecimalToSingle(const Num: TCnBigDecimal): Single;
{* 大浮点数对象转换为单精度浮点数}

function BigDecimalToDouble(const Num: TCnBigDecimal): Double;
{* 大浮点数对象转换为双精度浮点数}

function BigDecimalToExtended(const Num: TCnBigDecimal): Extended;
{* 大浮点数对象转换为扩展精度浮点数}

function BigDecimalCompare(const Num1, Num2: TCnBigDecimal): Integer; overload;
{* 比较两个大浮点数对象}

function BigDecimalCompare(const Num1: TCnBigDecimal; Num2: Int64): Integer; overload;
{* 比较大浮点数对象与整数}

function BigDecimalCompare(const Num1: TCnBigDecimal; Num2: Extended): Integer; overload;
{* 比较大浮点数对象与浮点数}

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
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大浮点数在值不咋变的前提下转换到指定 Scale，也就是小数点后 Scale 位，可能产生舍入，按指定模式来
  如果 Scale 为负，代表舍入到整 10 次方。Res 可以是 Num}

function BigDecimalRoundToDigits(const Res: TCnBigDecimal; const Num: TCnBigDecimal;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大浮点数在值不咋变的前提下按指定模式舍入到指定小数点后 Digits 位，
  如果本身精度不够 Digits 位则不变。Res 可以是 Num}

function BigDecimalTrunc(const Res: TCnBigDecimal; const Num: TCnBigDecimal): Boolean;
{* 将大浮点数 Trunc 到只剩整数。Res 可以是 Num}

function BigDecimalDebugDump(const Num: TCnBigDecimal): string;
{* 打印大浮点数内部信息}

// ========================== 大二进制浮点数操作函数 ===========================

procedure BigBinaryClear(const Num: TCnBigBinary);
{* 清空一个大二进制浮点数，实质上是 Value 与 Scale 都塞 0}

function BigBinarySetDec(const Buf: string; const Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置字符串值}

function BigBinarySetWord(W: LongWord; const Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置整数值}

function BigBinarySetInt64(W: Int64; const Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置 Int64 整数值}

function BigBinarySetSingle(const Value: Single; const Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置单精度浮点值}

function BigBinarySetDouble(const Value: Double; const Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置双精度浮点值}

function BigBinarySetExtended(const Value: Extended; const Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置扩展精度浮点值}

function BigBinarySetBigNumber(const Num: TCnBigNumber; const Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置大数值}

function BigBinaryToString(const Num: TCnBigBinary): string;
{* 大二进制浮点数对象转换为字符串}

function BigBinaryToSingle(const Num: TCnBigBinary): Single;
{* 大二进制浮点数对象转换为单精度浮点数}

function BigBinaryToDouble(const Num: TCnBigBinary): Double;
{* 大二进制浮点数对象转换为双精度浮点数}

function BigBinaryToExtended(const Num: TCnBigBinary): Extended;
{* 大二进制浮点数对象转换为扩展精度浮点数}

function BigBinaryCompare(const Num1, Num2: TCnBigBinary): Integer; overload;
{* 比较两个大二进制浮点数对象}

function BigBinaryCompare(const Num1: TCnBigBinary; Num2: Int64): Integer; overload;
{* 比较大二进制浮点数对象与整数}

function BigBinaryCompare(const Num1: TCnBigBinary; Num2: Extended): Integer; overload;
{* 比较大二进制浮点数对象与浮点数}

procedure BigBinaryCopy(const Dest, Source: TCnBigBinary);
{* 大二进制浮点数赋值}

function BigBinaryGetHighScale(const Num: TCnBigBinary): Integer;
{* 计算大二进制浮点数的最高有效数字位是小数点后第几位，如果返回小于 0，则求负后表示是小数点前第几位}

function BigBinaryAdd(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary): Boolean;
{* 大二进制浮点数加，Res 可以是 Num1 或 Num2，Num1 可以是 Num2}

function BigBinarySub(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary): Boolean;
{* 大二进制浮点数减，Res 可以是 Num1 或 Num2，Num1 可以是 Num2}

function BigBinaryMul(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary; MulPrecision: Integer = 0): Boolean;
{* 大二进制浮点数乘，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
  MulPrecision 表示乘法最多保留小数点后几位，0 表示全保留}

function BigBinaryDiv(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary; DivPrecision: Integer = 0): Boolean;
{* 大二进制浮点数除，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
  DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来}

procedure BigBinaryShiftLeft(const Res: TCnBigBinary; const N: Integer);
{* 大二进制浮点数左移，内部直接调整 FScale}

procedure BigBinaryShiftRight(const Res: TCnBigBinary; const N: Integer);
{* 大二进制浮点数右移，内部直接调整 FScale}

function BigBinaryPower(const Res: TCnBigBinary; const N: Integer): Boolean;
{* 大二进制求幂，只支持非负整数幂}

function BigBinaryChangeToScale(const Res: TCnBigBinary; const Num: TCnBigBinary;
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大二进制浮点数在值不咋变的前提下转换到指定 Scale，也就是小数点后 Scale 位，可能产生舍入，按指定模式进行。
  如果 Scale 为负，代表舍入到整 2 次方。Res 可以是 Num}

function BigBinaryRoundToDigits(const Res: TCnBigBinary; const Num: TCnBigBinary;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大二进制浮点数在值不咋变的前提下按指定模式舍入到指定小数点后 Digits 二进制位，
  如果本身精度不够 Digits 位则不变。Res 可以是 Num}

function BigBinaryTrunc(const Res: TCnBigBinary; const Num: TCnBigBinary): Boolean;
{* 将大二进制浮点数 Trunc 到只剩整数。Res 可以是 Num}

function BigBinaryTruncTo(const Res: TCnBigNumber; const Num: TCnBigBinary): Boolean;
{* 将大二进制浮点数 Trunc 到只剩整数并放大数中。}

function BigBinaryDebugDump(const Num: TCnBigBinary): string;
{* 打印大二进制浮点数内部信息}

var
  CnBigDecimalOne: TCnBigDecimal = nil;     // 表示 1 的常量
  CnBigDecimalZero: TCnBigDecimal = nil;    // 表示 0 的常量

implementation

resourcestring
  SCnNotImplemented = 'NOT Implemented.';
  SCnScaleOutOfRange = 'Scale Out of Range.';
  SCnRoundModeNotSupport = 'Round Mode Not Support.';

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

const
  SCN_POWER_TENS64: array[0..19] of TUInt64 = (
    1,                               // 10 ^ 0
    10,                              // 10 ^ 1
    100,                             // 10 ^ 2
    1000,                            // 10 ^ 3
    10000,                           // 10 ^ 4
    100000,                          // 10 ^ 5
    1000000,                         // 10 ^ 6
    10000000,                        // 10 ^ 7
    100000000,                       // 10 ^ 8
    1000000000,                      // 10 ^ 9
    10000000000,                     // 10 ^ 10
    100000000000,                    // 10 ^ 11
    1000000000000,                   // 10 ^ 12
    10000000000000,                  // 10 ^ 13
    100000000000000,                 // 10 ^ 14
    1000000000000000,                // 10 ^ 15
    10000000000000000,               // 10 ^ 16
    100000000000000000,              // 10 ^ 17
    1000000000000000000,             // 10 ^ 18
    $8AC7230489E80000                // 10 ^ 19

    // 10 ^ 19 10000000000000000000 已经超了 Int64 9223372036854775807
    // 所以得用 16 进制写但没超 UInt64 18446744073709551615，10 ^ 20 才超
  );

var
  FLocalBigDecimalPool: TCnBigDecimalPool = nil;
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalBigBinaryPool: TCnBigBinaryPool = nil;

  FDefaultDecimalPrecisionDigits: Integer = CN_BIG_DECIMAL_DEFAULT_PRECISION;
  FDefaultBinaryPrecisionDigits: Integer = CN_BIG_BINARY_DEFAULT_PRECISION;

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
        drRound:         // 四舍五入、入至绝对值大的数
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

// 大数乘以 10 的 Power5 次方，不支持负
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

// 大数乘以 10 的 Power10 次方，不支持负
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
  C := GetUInt64LowBits(IntMantissa); // 清掉 IntMantissa 右边的零并调整 Exponent 以化简
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
  Result := InternalBigDecimalSetFloat(N, E - CN_SINGLE_SIGNIFICAND_BITLENGTH, TUInt64(S), Res);
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
  Result := InternalBigDecimalSetFloat(N, E - CN_DOUBLE_SIGNIFICAND_BITLENGTH, S, Res);
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
  Result := InternalBigDecimalSetFloat(N, E - CN_EXTENDED_SIGNIFICAND_BITLENGTH, S, Res);
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

// 通过巨大的变换让大浮点数的原始值等于 Value / 2^Scale，并且有效数字满足特定位数 
function InternalBigDecimalConvertToBitsCount(const Num: TCnBigDecimal; BitsCount: Integer): Boolean;
var
  C, D: Integer;
  Di, R: TCnBigNumber;
begin
//  FValue * 10^-FScale 要变成 M * 2^E 次方的形式，FValue 就得乘以 5^-FScale，如果 FScale < 0，就是直接乘 5^-FScale
//  如果 FScale > 0，意味着 FValue 要除以 5 的 FScale 次方，
//  (FValue * 5^-FScale) * 2^-FScale，然后还得再次规约，让前者成为特定位数，后者再调整

  Result := False;
  if Num <> nil then
  begin
    if Num.FScale < 0 then
    begin
      BigNumberMulPower5(Num.FValue, -Num.FScale);  // 乘 5 的正整数次方
    end
    else // FScale 大于 0
    begin
      // 除以 5 的 FScale 次方。注意不能取巧地除以 10 的 FScale 次方（减 FScale）
      // 再乘以 2 的 FScale 次方（左移），因为两个操作里 FScale 含义不一样了
      // 得以二进制的方式把 FValue 直接除以 5 的 FScale 次方，得到的结果要二进制调整 FScale 值

      Di := FLocalBigNumberPool.Obtain;
      R := FLocalBigNumberPool.Obtain;
      try
        Di.SetWord(5);
        Di.PowerWord(Num.FScale); // 得到除数

        // FValue / Di 要得到小数结果，所以不能直接 BigNumberDiv，得将 FValue 扩大 2 的整数次方
        // 扩大多少次方取决于精度，可以认为需要小数点后十进制 FScale 位，胡乱折算成 2 进制
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

    // 再规约，注意此时 FValue 和 FScale 已经是 2^ 关系了（Num 相当于一个 TCnBigBinary 了）
    C := Num.FValue.GetBitsCount;
    if C < BitsCount then
    begin
      D := BitsCount - C;
      Num.FValue.ShiftLeft(D);
      Num.FScale := Num.FScale + D;
    end
    else if C > BitsCount then
    begin
      D := C - BitsCount;  // 要截掉 D 个位，也就是要把 FScale 减少 D
      Num.FValue.ShiftRight(D);
      Num.FScale := Num.FScale - D;
    end;
    Result := True;
  end;
end;

function BigDecimalToSingle(const Num: TCnBigDecimal): Single;
var
  T: TCnBigDecimal;
  E: Integer;
  M: Cardinal;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    BigDecimalCopy(T, Num);
    InternalBigDecimalConvertToBitsCount(T, CN_SINGLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // 清除最高位的 1

    M := T.FValue.GetWord;
    E := -T.FScale;

    CombineFloatSingle(Num.IsNegative, E + CN_SINGLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

function BigDecimalToDouble(const Num: TCnBigDecimal): Double;
var
  T: TCnBigDecimal;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    BigDecimalCopy(T, Num);
    InternalBigDecimalConvertToBitsCount(T, CN_DOUBLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // 清除最高位的 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatDouble(Num.IsNegative, E + CN_DOUBLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
end;

function BigDecimalToExtended(const Num: TCnBigDecimal): Extended;
var
  T: TCnBigDecimal;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigDecimalPool.Obtain;
  try
    BigDecimalCopy(T, Num);
    InternalBigDecimalConvertToBitsCount(T, CN_EXTENDED_SIGNIFICAND_BITLENGTH + 1);
    // 无需清除最高位的 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatExtended(Num.IsNegative, E + CN_EXTENDED_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigDecimalPool.Recycle(T);
  end;
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

function BigDecimalCompare(const Num1: TCnBigDecimal; Num2: Int64): Integer;
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

function BigDecimalCompare(const Num1: TCnBigDecimal; Num2: Extended): Integer;
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
    DivPrecision := FDefaultDecimalPrecisionDigits;
  if DivPrecision < 0 then
    DivPrecision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  // 根据精度要求计算将被除数扩大的倍数，注意存在乘以 9 时就可能溢出的情况但先不管
  M := CheckScaleAddRange(DivPrecision, (Num2.FValue.Top - Num2.FValue.Top + 1) * 9 + 3);
  TS := CheckScaleAddRange(TS, M); // 扩大的倍数在这里抵消

  T := nil;
  R := nil;
  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberCopy(T, Num1.FValue);
    BigNumberMulPower10(T, M);

    R := FLocalBigNumberPool.Obtain;
    BigNumberDiv(Res.FValue, R, T, Num2.FValue);  // Num1.FValue * 10 ^ M div Num2.FValue 得到商和余数

    RoundDecimalByMode(Res.FValue, Num2.FValue, R, Res.FValue.IsNegative, drTowardsZero);
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
  Scale: Integer; RoundMode: TCnBigRoundMode): Boolean;
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
      RoundDecimalByMode(Q, D, R, Neg, RoundMode);

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
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
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
      RoundDecimalByMode(Q, D, R, Neg, RoundMode);

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
  Result := '10 Scale: ' + IntToStr(Num.FScale) + '. ' + BigNumberDebugDump(Num.FValue);
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

procedure TCnBigDecimal.DivWord(W: LongWord; DivPrecision: Integer);
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

procedure BigBinaryClear(const Num: TCnBigBinary);
begin
  if Num <> nil then
  begin
    Num.FValue.SetZero;
    Num.FScale := 0;
  end;
end;

function BigBinarySetDec(const Buf: string; const Res: TCnBigBinary): Boolean;
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
  if not Assigned(DotPos) and (C <> 'e') and (C <> 'E') then
  begin
    // 如果没小数点又没有指数，说明是整数
    Res.FValue.SetDec(V);
    if (not Res.FValue.IsNegative) and Neg then
      Res.FValue.SetNegative(True);

    Result := True;
  end;

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
  DC := DC - E; // 如果有指数，再调整计算小数部分长度给 DC

  // 这里得到的值是没有小数点的 V，以及指示其中应该有十进制小数点位置的 DC，分开处理
  if DC = 0 then
  begin
    Res.FValue.SetDec(V);
    Res.FScale := 0;
  end
  else if DC < 0 then // 还要乘以 10^-DC，还是整数
  begin
    Res.FValue.SetDec(V);
    BigNumberMulPower10(Res.FValue, -DC);
  end
  else // DC > 0，说明有小数
  begin
    if Length(V) > DC then
    begin
      S := Copy(V, 1, Length(V) - DC);             // S 是整数部分的字符串
      Delete(V, 1, Length(V) - DC);                // V 是小数点后的部分的字符串
    end
    else if Length(V) = DC then
    begin
      S := '0';
      // V 保持原样
    end
    else // V 长度比 DC 要求的位数还要小，前面要加 0
    begin
      S := '0';
      V := StringOfChar('0', DC - Length(V)) + V;
    end;

    // 分别处理 S 和 V，将其转换为整数与小数部分
    DMax := Trunc(Length(V) * 5);  // FIXME: 小数部分最多转换 DMax 位，避免遇到循环停不下来
    if DMax < CN_BIG_BINARY_DEFAULT_PRECISION then
      DMax := CN_BIG_BINARY_DEFAULT_PRECISION;

    P10 := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;
    DRes := FLocalBigNumberPool.Obtain;

    try
      P10.SetOne;
      BigNumberMulPower10(P10, Length(V)); // 每次乘后要和 P10 比较以决定这一位是不是 1

      T.SetDec(V);
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

      // 得到 I 位二进制值，在 DRes 里，就是小数点后的小数部分了，和整数部分拼起来
      T.SetDec(S);
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

function BigBinarySetWord(W: LongWord; const Res: TCnBigBinary): Boolean;
begin
  Res.FValue.SetWord(W);
  Res.FScale := 0;
  Result := True;
end;

function BigBinarySetInt64(W: Int64; const Res: TCnBigBinary): Boolean;
begin
  Res.FValue.SetInt64(W);
  Res.FScale := 0;
  Result := True;
end;

function InternalBigBinarySetFloat(Neg: Boolean; IntExponent: Integer; IntMantissa: TUInt64;
  const Res: TCnBigBinary): Boolean;
var
  C: Integer;
begin
  C := GetUInt64LowBits(IntMantissa);  // 清掉 IntMantissa 右边的零并调整 Exponent 以化简
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
  else // 指数是负数说明有小数部分
  begin
    IntExponent := -IntExponent;
    Res.FScale := IntExponent;
  end;

  Res.FValue.SetNegative(Neg);
  Result := True;
end;

function BigBinarySetSingle(const Value: Single; const Res: TCnBigBinary): Boolean;
var
  N: Boolean;
  E: Integer;
  S: LongWord;
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
  // 把 1. 开头的有效数字当成整数，E 需要减 23
  Result := InternalBigBinarySetFloat(N, E - 23, TUInt64(S), Res);
end;

function BigBinarySetDouble(const Value: Double; const Res: TCnBigBinary): Boolean;
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
  // 把 1. 开头的有效数字当成整数，E 需要减 52
  Result := InternalBigBinarySetFloat(N, E - 52, S, Res);
end;

function BigBinarySetExtended(const Value: Extended; const Res: TCnBigBinary): Boolean;
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
  // 把 1. 开头的有效数字当成整数，E 需要减 63
  Result := InternalBigBinarySetFloat(N, E - 63, S, Res);
end;

function BigBinarySetBigNumber(const Num: TCnBigNumber; const Res: TCnBigBinary): Boolean;
begin
  Res.FScale := 0;
  Result := BigNumberCopy(Res.FValue, Num) <> nil;
end;

function BigBinaryToString(const Num: TCnBigBinary): string;
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
    else // FScale > 0，有小数部分，单独拎出来处理
    begin
      T := FLocalBigNumberPool.Obtain;
      S := nil;
      P10 := nil;

      try
        BigNumberCopy(T, Num.FValue);
        T.ShiftRight(Num.FScale);
        Result := T.ToDec;  // 先右移得到整数部分

        // 再把剩下的转换成小数
        BigNumberCopy(T, Num.FValue);
        BigNumberKeepLowBits(T, Num.FScale); // 只保留小数部分
        if T.IsZero then  // 如果没小数部分，就直接返回了
          Exit;

        S := FLocalBigNumberPool.Obtain;
        P10 := FLocalBigNumberPool.Obtain;
        S.SetZero;
        P10.SetOne;
        BigNumberMulPower10(P10, Num.FScale); // 不能用 T.GetBitsCount，后者可能有 0，更小

        for I := Num.FScale - 1 downto 0 do
        begin
          P10.ShiftRightOne;
          if T.IsBitSet(I) then
            BigNumberAdd(S, S, P10);
        end;
        if S.IsZero then
          Exit;

        D := S.ToDec; // 注意 ToDec 后长度可能不够 FScale 个，前头要补零
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

function BigBinaryCompare(const Num1, Num2: TCnBigBinary): Integer; overload;
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
    // 乘以 2 的指数差次幂以对齐小数点，再和另一个比较（无需保持值不变，所以和加减有区别）
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

function BigBinaryCompare(const Num1: TCnBigBinary; Num2: Int64): Integer; overload;
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

// 通过值基本不变的变换让大二进制浮点数的有效数字满足特定位数，超长时截断，不够时乘 2 的整数次方补全，并同时都调整 FScale
function InternalBigBinaryChangeToBitsCount(const Num: TCnBigBinary; BitsCount: Integer): Boolean;
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
      D := C - BitsCount;  // 要截掉 D 个位，也就是要把 FScale 减少 D，
      BigBinaryChangeToScale(Num, Num, Num.FScale - D);
    end;
    Result := True;
  end;
end;

function BigBinaryToSingle(const Num: TCnBigBinary): Single;
var
  T: TCnBigBinary;
  E: Integer;
  M: Cardinal;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    BigBinaryCopy(T, Num);
    InternalBigBinaryChangeToBitsCount(T, CN_SINGLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // 清除最高位的 1

    M := T.FValue.GetWord;
    E := -T.FScale;

    CombineFloatSingle(Num.IsNegative, E + CN_SINGLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function BigBinaryToDouble(const Num: TCnBigBinary): Double;
var
  T: TCnBigBinary;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    BigBinaryCopy(T, Num);
    InternalBigBinaryChangeToBitsCount(T, CN_DOUBLE_SIGNIFICAND_BITLENGTH + 1);
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // 清除最高位的 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatDouble(Num.IsNegative, E + CN_DOUBLE_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function BigBinaryToExtended(const Num: TCnBigBinary): Extended;
var
  T: TCnBigBinary;
  E: Integer;
  M: TUInt64;
begin
  T := FLocalBigBinaryPool.Obtain;
  try
    BigBinaryCopy(T, Num);
    InternalBigBinaryChangeToBitsCount(T, CN_EXTENDED_SIGNIFICAND_BITLENGTH + 1);
    // 无需清除最高位的 1

    M := BigNumberGetUInt64UsingInt64(T.FValue);
    E := -T.FScale;

    CombineFloatExtended(Num.IsNegative, E + CN_EXTENDED_SIGNIFICAND_BITLENGTH, M, Result);
  finally
    FLocalBigBinaryPool.Recycle(T);
  end;
end;

function BigBinaryCompare(const Num1: TCnBigBinary; Num2: Extended): Integer; overload;
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

procedure BigBinaryCopy(const Dest, Source: TCnBigBinary);
begin
  if (Source <> nil) and (Dest <> nil) and (Source <> Dest) then
  begin
    BigNumberCopy(Dest.FValue, Source.FValue);
    Dest.FScale := Source.FScale;
  end;
end;

function BigBinaryGetHighScale(const Num: TCnBigBinary): Integer;
begin
  Result := 0;
  if Num <> nil then
  begin
    Result := Num.FValue.GetBitsCount;
    // 小数点后有 FScale 位，减去有效数字
    Result := Num.FScale - Result + 1;
    if Result <= 0 then // 小数点前第几位是从 1 开始的
      Dec(Result)
  end;
end;

function BigBinaryAdd(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary): Boolean;
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

function BigBinarySub(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary): Boolean;
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

function BigBinaryMul(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary; MulPrecision: Integer = 0): Boolean;
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

function BigBinaryDiv(const Res: TCnBigBinary; const Num1: TCnBigBinary;
  const Num2: TCnBigBinary; DivPrecision: Integer = 0): Boolean;
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
    DivPrecision := FDefaultBinaryPrecisionDigits;
  if DivPrecision < 0 then
    DivPrecision := CN_BIG_BINARY_DEFAULT_PRECISION;

  // 根据精度要求计算将被除数扩大的倍数
  M := CheckScaleAddRange(DivPrecision, (Num2.FValue.GetBitsCount - Num2.FValue.GetBitsCount + 1));
  TS := CheckScaleAddRange(TS, M); // 扩大的倍数在这里抵消

  T := nil;
  R := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberCopy(T, Num1.FValue);
    T.ShiftLeft(M);

    R := FLocalBigNumberPool.Obtain;
    BigNumberDiv(Res.FValue, R, T, Num2.FValue);  // Num1.FValue * 2 ^ M div Num2.FValue 得到商和余数

    // 直接 Trunc 掉，不舍入了
    Res.FScale := TS;
    // TODO: 二进制约分

    BigBinaryRoundToDigits(Res, Res, DivPrecision, drTowardsZero);
    Res.FValue.SetNegative(S);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(R);
  end;
end;

procedure BigBinaryShiftLeft(const Res: TCnBigBinary; const N: Integer);
begin
  Dec(Res.FScale, N);
end;

procedure BigBinaryShiftRight(const Res: TCnBigBinary; const N: Integer);
begin
  Inc(Res.FScale, N);
end;

function BigBinaryPower(const Res: TCnBigBinary; const N: Integer): Boolean;
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
    drRound:
      begin
        if RemainderSet then // 余数最高位是 1
          BigNumberAddWord(Quotient, 1);
      end;
  else
    raise ECnBigBinaryException.Create(SCnRoundModeNotSupport);
  end;
end;

function BigBinaryChangeToScale(const Res: TCnBigBinary; const Num: TCnBigBinary;
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
var
  DS: Integer;
  B, Neg: Boolean;
begin
  DS := CheckScaleAddRange(Num.FScale, -Scale);
  if DS > 0 then // 新的小数点后的位数比原来少，要除之后舍入
  begin
    Neg := Num.FValue.IsNegative;
    Num.FValue.SetNegative(False);

    B := Num.FValue.IsBitSet(DS - 1); // 直接获取余数最高位
    BigNumberCopy(Res.FValue, Num.FValue);
    Res.FValue.ShiftRight(DS);

    // 直接根据右移后的商和余数最高位以及规则决定舍入
    RoundBinaryByMode(Res.FValue, B, Neg, RoundMode);

    Res.FScale := Scale;
    Res.FValue.SetNegative(Neg);

    if Res <> Num then           // 如果 Num 是独立的，这里要还原其 Neg
      Num.FValue.SetNegative(Neg);
    Result := True;
  end
  else // 新的小数点位数比原来还多，简单变换一下就行
  begin
    BigNumberCopy(Res.FValue, Num.FValue);
    if DS < 0 then
      Res.FValue.ShiftLeft(-DS);
    Res.FScale := Scale;
    Result := True;
  end;
end;

function BigBinaryRoundToDigits(const Res: TCnBigBinary; const Num: TCnBigBinary;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
var
  DS: Integer;
  B, Neg: Boolean;
begin
  Result := False;
  DS := CheckScaleAddRange(Num.FScale, -Digits);

  if DS > 0 then // 新的小数点后的位数得比原来少，才能舍入
  begin
    Neg := Num.FValue.IsNegative;
    Num.FValue.SetNegative(False);

    B := Num.FValue.IsBitSet(DS - 1); // 直接获取余数最高位
    BigNumberCopy(Res.FValue, Num.FValue);
    Res.FValue.ShiftRight(DS);

    // 直接根据右移后的商和余数最高位以及规则决定舍入
    RoundBinaryByMode(Res.FValue, B, Neg, RoundMode);

    Res.FScale := Digits;
    Res.FValue.SetNegative(Neg);

    if Res <> Num then           // 如果 Num 是独立的，这里要还原其 Neg
      Num.FValue.SetNegative(Neg);
    Result := True;
  end;
end;

function BigBinaryTrunc(const Res: TCnBigBinary; const Num: TCnBigBinary): Boolean;
begin
  if Num.FScale <= 0 then // 无小数部分
  begin
    BigBinaryCopy(Res, Num);
    Result := True;
    Exit;
  end
  else // 有小数部分 FScale 位，干掉
  begin
    Result := BigBinaryChangeToScale(Res, Num, 0, drTowardsZero);
  end;
end;

function BigBinaryTruncTo(const Res: TCnBigNumber; const Num: TCnBigBinary): Boolean;
var
  T: TCnBigBinary;
begin
  if Num.FScale <= 0 then // 无小数部分
  begin
    BigNumberCopy(Res, Num.FValue);
    Res.ShiftLeft(-Num.FScale);

    Result := True;
    Exit;
  end
  else // 有小数部分 FScale 位，干掉
  begin
    T := FLocalBigBinaryPool.Obtain;
    try
      Result := BigBinaryChangeToScale(T, Num, 0, drTowardsZero);
      BigNumberCopy(Res, T.FValue); // Scale 已经为 0 了可以直接忽略
    finally
      FLocalBigBinaryPool.Recycle(T);
    end;
  end;
end;

function BigBinaryDebugDump(const Num: TCnBigBinary): string;
begin
  Result := '2 Scale: ' + IntToStr(Num.FScale) + '. ' + BigNumberDebugDump(Num.FValue);
end;

{ TCnBigBinary }

procedure TCnBigBinary.AddWord(W: LongWord);
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

procedure TCnBigBinary.DivWord(W: LongWord; DivPrecision: Integer);
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

  if W = 1 then // 除的是 2 的整数次方
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

procedure TCnBigBinary.MulWord(W: LongWord);
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

function TCnBigBinary.SetWord(W: LongWord): Boolean;
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

procedure TCnBigBinary.SubWord(W: LongWord);
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
//  CnBigDecimalZero.DecString; // 手工调用这两句防止被编译器忽略
//  CnBigDecimalZero.DebugDump;

  CnBigDecimalZero.Free;
  CnBigDecimalOne.Free;

  FLocalBigNumberPool.Free;
  FLocalBigBinaryPool.Free;
  FLocalBigDecimalPool.Free;

end.
