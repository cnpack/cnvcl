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

unit CnBigDecimal;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：无限精度浮点数实现单元
* 单元作者：CnPack 开发组
* 备    注：本单元实现了基于十进制的无限精度浮点数类 TCnBigDecimal 与基于二进制
*           的无限精度浮点数类 TCnBigBinary。两者 均用大整数 TCnBigNumber 表示有效数字，
*           用 Integer 表示正负指数，所不同的是底分别为 10 和 2。
*
*           部分实现参考了 Rudy Velthuis 的 BigDecimal 以及 Java 中的 BigDecimal。
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.12.05 V1.3
*               去掉一批无用的 const 声明并调整注释
*           2021.12.05 V1.3
*               增加与 TCnBigRational 互相转换的函数以及求平方根的函数
*           2021.09.05 V1.2
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
  SysUtils, Classes, SysConst,
  CnNative, CnFloat, CnContainers, CnBigRational, CnBigNumber;

const
  CN_BIG_DECIMAL_DEFAULT_PRECISION = 12;
  {* 大十进制浮点数乘除法的小数点后的默认精度}

  CN_BIG_BINARY_DEFAULT_PRECISION  = 32;
  {* 大二进制浮点数小数点后的默认精度}

  CN_BIG_DECIMAL_DEFAULT_DIGITS    = 20;
  {* 大十进制浮点数转换为小数时默认保留的位数}

  CN_SQRT_DEFAULT_ROUND_COUNT      = 10;
  {* 大十进制浮点数求平方根时默认迭代的次数}

type
  ECnBigDecimalException = class(Exception);
  {* 大十进制浮点数相关异常}

  TCnBigRoundMode = (drAwayFromZero, drTowardsZero, drCeilingToInfinite,
    drFloorToNegInfinite, drRound, dr465RoundEven);
  {* 大十进制浮点数取整的模式，十进制包括六种，不处理四舍六入五成单的特殊需求，二进制包括前五种。
     注意：四舍五入的入只有入至绝对值大的情况，没有正负无穷的情况，因为舍动作必然是往绝对值小的数取。

     枚举值：
       drAwayFromZero                     - 往绝对值大的数取
       drTowardsZero                      - 往绝对值小的数取，等于只留整数部分的 Trunc
       drCeilingToInfinite                - 往正无穷大取
       drFloorToNegInfinite               - 往负无穷大取
       drRound                            - 四舍五入（二进制模式下是 0 舍 1 入）、入至绝对值大的数
       dr465RoundEven                     - 四舍六入五成双（不支持二进制模式）、入至绝对值大的数
  }


  TCnBigDecimal = class
  {* 大十进制浮点数实现类，用 CnBigNumber 保存有效数字，用 Integer 保存指数也就是小数点位置
     FScale 代表小数点离有效数字最右边的位置，往左为正，往右为负，
     正时简而言之就是小数点后有 FScale 位，负时简而言之还要加 -FScale 个 0}
  private
    FValue: TCnBigNumber;
    FScale: Integer;                 // 精确值为 FValue / (10^FScale)
    function GetDecString: string;
    function GetDebugDump: string;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SetZero;
    {* 设置成 0}
    procedure SetOne;
    {* 设置成 1}
    procedure SetNegative(Neg: Boolean);
    {* 设置是否负数。

       参数：
         Neg: Boolean                     - 设置是否负数

       返回值：（无）
    }

    procedure Negate;
    {* 设置为相反数}

    function SetWord(W: Cardinal): Boolean;
    {* 设置为一个 32 位无符号整数。

       参数：
         W: Cardinal                      - 待设置的 32 位无符号整数

       返回值：Boolean                    - 返回设置是否成功
    }

    function SetInt64(W: Int64): Boolean;
    {* 设置为一个 64 位有符号整数。

       参数：
         W: Int64                         - 待设置的 64 位有符号整数

       返回值：Boolean                    - 返回设置是否成功
    }

    function SetDec(const Buf: string): Boolean;
    {* 设置为字符串值。

       参数：
         const Buf: string                - 待设置的字符串值

       返回值：Boolean                    - 返回设置是否成功
    }

    procedure SetSingle(Value: Single);
    {* 设置为单精度浮点值。

       参数：
         Value: Single                    - 待设置的单精度浮点值

       返回值：（无）
    }

    procedure SetDouble(Value: Double);
    {* 设置为双精度浮点值。

       参数：
         Value: Double                    - 待设置的双精度浮点值

       返回值：（无）
    }

    procedure SetExtended(Value: Extended);
    {* 设置为扩展精度浮点值。

       参数：
         Value: Extended                  - 待设置的扩展精度浮点值

       返回值：（无）
    }

    procedure AddWord(W: Cardinal);
    {* 加上一个整数。

       参数：
         W: Cardinal                      - 加数

       返回值：（无）
    }

    procedure SubWord(W: Cardinal);
    {* 减去一个整数。

       参数：
         W: Cardinal                      - 减数

       返回值：（无）
    }

    procedure MulWord(W: Cardinal);
    {* 乘以一个整数。

       参数：
         W: Cardinal                      - 乘数

       返回值：（无）
    }

    procedure DivWord(W: Cardinal; DivPrecision: Integer = 0);
    {* 除以一个整数。DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来。

       参数：
         W: Cardinal                      - 除数
         DivPrecision: Integer            - 保留小数点后多少位，0 表示按默认精度处理

       返回值：（无）
    }

    function IsNegative: Boolean;
    {* 是否负数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否负数
    }

    function IsZero: Boolean;
    {* 是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    function IsOne: Boolean;
    {* 是否为 1，只判断值是 1 且指数是 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    procedure RoundTo(Precision: Integer; RoundMode: TCnBigRoundMode = dr465RoundEven);
    {* 舍入至指定小数位数，如原来小数位数少于 Precision 则不动。

       参数：
         Precision: Integer               - 指定小数位数
         RoundMode: TCnBigRoundMode       - 舍入的规则

       返回值：（无）
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大十进制浮点数转成字符串。

       参数：
         （无）

       返回值：string                     - 返回转换的字符串
    }

    property Value: TCnBigNumber read FValue;
    {* 有效数字，放出来供特殊需要}
    property Scale: Integer read FScale write FScale;
    {* 指数，放出来供特殊需要}
    property DecString: string read GetDecString;
    {* 十进制字符串}

    property DebugDump: string read GetDebugDump;
  end;

  TCnBigDecimalPool = class(TCnMathObjectPool)
  {* 大十进制浮点数池实现类，允许使用到大十进制浮点数的地方自行创建大十进制浮点数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigDecimal;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         Num: TCnBigDecimal               - 待归还至池中的对象

       返回值：（无）
    }
    procedure Recycle(Num: TCnBigDecimal);
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnBigDecimal               - 待归还至池中的对象

       返回值：（无）
    }
  end;

  ECnBigBinaryException = class(Exception);
  {* 大二进制浮点数相关异常}

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
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SetZero;
    {* 设置成 0}
    procedure SetOne;
    {* 设置成 1}
    procedure SetNegative(Neg: Boolean);
    {* 设置是否负数。

       参数：
         Neg: Boolean                     - 设置是否负数

       返回值：（无）
    }

    procedure Negate;
    {* 设置为相反数}

    function SetWord(W: Cardinal): Boolean;
    {* 设置为一个 32 位无符号整数。

       参数：
         W: Cardinal                      - 待设置的 32 位无符号整数

       返回值：Boolean                    - 返回设置是否成功
    }

    function SetInt64(W: Int64): Boolean;
    {* 设置为一个 64 位有符号整数。

       参数：
         W: Int64                         - 待设置的 64 位有符号整数

       返回值：Boolean                    - 返回设置是否成功
    }

    function SetDec(const Buf: string): Boolean;
    {* 设置字符串值。

       参数：
         const Buf: string                - 待设置的字符串值

       返回值：Boolean                    - 返回设置是否成功
    }

    procedure SetSingle(Value: Single);
    {* 设置为单精度浮点值。

       参数：
         Value: Single                    - 待设置的单精度浮点值

       返回值：（无）
    }

    procedure SetDouble(Value: Double);
    {* 设置为双精度浮点值。

       参数：
         Value: Double                    - 待设置的双精度浮点值

       返回值：（无）
    }

    procedure SetExtended(Value: Extended);
    {* 设置为扩展精度浮点值。

       参数：
         Value: Extended                  - 待设置的扩展精度浮点值

       返回值：（无）
    }

    procedure SetBigNumber(Value: TCnBigNumber);
    {* 设置为大整数值。

       参数：
         Value: TCnBigNumber              - 待设置的大整数值

       返回值：（无）
    }

    procedure AddWord(W: Cardinal);
    {* 加上一个整数。

       参数：
         W: Cardinal                      - 加数

       返回值：（无）
    }

    procedure SubWord(W: Cardinal);
    {* 减去一个整数。

       参数：
         W: Cardinal                      - 减数

       返回值：（无）
    }

    procedure MulWord(W: Cardinal);
    {* 乘以一个整数。

       参数：
         W: Cardinal                      - 乘数

       返回值：（无）
    }

    procedure DivWord(W: Cardinal; DivPrecision: Integer = 0);
    {* 除以一个整数。DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来。

       参数：
         W: Cardinal                      - 除数
         DivPrecision: Integer            - 保留小数点后多少位，0 表示按默认精度处理

       返回值：（无）
    }

    procedure ShiftLeft(N: Integer);
    {* 左移 N 位。

       参数：
         N: Integer                       - 待左移的位数

       返回值：（无）
    }

    procedure ShiftRight(N: Integer);
    {* 右移 N 位。

       参数：
         N: Integer                       - 待右移的位数

       返回值：（无）
    }

    procedure Power(N: Integer);
    {* 求幂。

       参数：
         N: Integer                       - 幂指数

       返回值：（无）
    }

    function IsNegative: Boolean;
    {* 是否负数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否负数
    }

    function IsZero: Boolean;
    {* 是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 是否为 0
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大十进制浮点数转换为字符串。

       参数：
         （无）

       返回值：string                     - 返回转换的字符串
    }

    property Value: TCnBigNumber read FValue;
    {* 有效数字，放出来供特殊需要}
    property Scale: Integer read FScale write FScale;
    {* 指数，放出来供特殊需要}
    property DecString: string read GetDecString;
    {* 十进制字符串}

    property DebugDump: string read GetDebugDump;
  end;

  TCnBigBinaryPool = class(TCnMathObjectPool)
  {* 大二进制浮点数池实现类，允许使用到大二进制浮点数的地方自行创建大二进制浮点数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigBinary; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigBinary               - 返回池中的大二进制浮点数对象
    }

    procedure Recycle(Num: TCnBigBinary); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnBigBinary                - 待归还至池中的对象

       返回值：（无）
    }
  end;

// ====================== 大十进制浮点数操作函数 ===============================

procedure BigDecimalClear(Num: TCnBigDecimal);
{* 清空一个大十进制浮点数对象，实质上是 Value 与 Scale 都设置为 0。

   参数：
     Num: TCnBigDecimal                   - 待清空的大十进制浮点数

   返回值：（无）
}

function BigDecimalSetDec(const Buf: string; Res: TCnBigDecimal): Boolean;
{* 为大十进制浮点数对象设置字符串值。

   参数：
     const Buf: string                    - 待设置的字符串
     Res: TCnBigDecimal                   - 待设置的大十进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigDecimalSetWord(W: Cardinal; Res: TCnBigDecimal): Boolean;
{* 为大十进制浮点数对象设置 32 位无符号整数值。

   参数：
     W: Cardinal                          - 待设置的 32 位无符号整数值
     Res: TCnBigDecimal                   - 待设置的大十进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigDecimalSetInt64(W: Int64; Res: TCnBigDecimal): Boolean;
{* 为大十进制浮点数对象设置 64 位有符号整数值。

   参数：
     W: Int64                             - 待设置的 64 位有符号整数值
     Res: TCnBigDecimal                   - 待设置的大十进制浮点数

   返回值：Boolean                        -
}

function BigDecimalSetSingle(Value: Single; Res: TCnBigDecimal): Boolean;
{* 为大十进制浮点数对象设置单精度浮点值。

   参数：
     Value: Single                        - 待设置的单精度浮点值
     Res: TCnBigDecimal                   - 待设置的大十进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigDecimalSetDouble(Value: Double; Res: TCnBigDecimal): Boolean;
{* 为大十进制浮点数对象设置双精度浮点值。

   参数：
     Value: Double                        - 待设置的双精度浮点值
     Res: TCnBigDecimal                   - 待设置的大十进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigDecimalSetExtended(Value: Extended; Res: TCnBigDecimal): Boolean;
{* 为大十进制浮点数对象设置扩展精度浮点值。

   参数：
     Value: Extended                      - 待设置的扩展精度浮点值
     Res: TCnBigDecimal                   - 待设置的大十进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigDecimalToString(Num: TCnBigDecimal): string;
{* 大十进制浮点数对象转换为字符串。

   参数：
     Num: TCnBigDecimal                   - 待转换的大十进制浮点数

   返回值：string                         - 返回转换的字符串
}

function BigDecimalToSingle(Num: TCnBigDecimal): Single;
{* 大十进制浮点数对象转换为单精度浮点数。

   参数：
     Num: TCnBigDecimal                   - 待转换的大十进制浮点数

   返回值：Single                         - 返回单精度浮点值
}

function BigDecimalToDouble(Num: TCnBigDecimal): Double;
{* 大十进制浮点数对象转换为双精度浮点数。

   参数：
     Num: TCnBigDecimal                   - 待转换的大十进制浮点数

   返回值：Double                         - 返回双精度浮点值
}

function BigDecimalToExtended(Num: TCnBigDecimal): Extended;
{* 大十进制浮点数对象转换为扩展精度浮点数。

   参数：
     Num: TCnBigDecimal                   - 待转换的大十进制浮点数

   返回值：Extended                       - 返回扩展精度浮点值
}

function BigDecimalEqual(Num1: TCnBigDecimal; Num2: TCnBigDecimal): Boolean;
{* 比较两个大十进制浮点数对象是否相等。

   参数：
     Num1: TCnBigDecimal                  - 待比较的大十进制浮点数一
     Num2: TCnBigDecimal                  - 待比较的大十进制浮点数二

   返回值：Boolean                        - 返回是否相等
}

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: TCnBigDecimal): Integer; overload;
{* 比较两个大十进制浮点数对象，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     Num1: TCnBigDecimal                  - 待比较的大十进制浮点数一
     Num2: TCnBigDecimal                  - 待比较的大十进制浮点数二

   返回值：Integer                        - 返回比较结果
}

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: Int64): Integer; overload;
{* 比较大十进制浮点数对象与整数，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     Num1: TCnBigDecimal                  - 待比较的大十进制浮点数
     Num2: Int64                          - 待比较的整数

   返回值：Integer                        - 返回比较结果
}

function BigDecimalCompare(Num1: TCnBigDecimal; Num2: Extended): Integer; overload;
{* 比较大十进制浮点数对象与浮点数，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     Num1: TCnBigDecimal                  - 待比较的大十进制浮点数
     Num2: Extended                       - 待比较的浮点数

   返回值：Integer                        - 返回比较结果
}

procedure BigDecimalCopy(Dest: TCnBigDecimal; Source: TCnBigDecimal);
{* 大十进制浮点数赋值。

   参数：
     Dest: TCnBigDecimal                  - 目标大十进制浮点数
     Source: TCnBigDecimal                - 源大十进制浮点数

   返回值：（无）
}

function BigDecimalGetPrecision(Num: TCnBigDecimal): Integer;
{* 计算大十进制浮点数的十进制位数，也即有效数字长度。

   参数：
     Num: TCnBigDecimal                   - 待计算的大十进制浮点数

   返回值：Integer                        - 返回有效数字长度
}

function BigDecimalGetIntDecimalCount(Num: TCnBigDecimal;
  out IntCount: Integer; out DecimalCount: Integer): Boolean;
{* 计算大十进制浮点数的整数部分长度与小数部分长度。

   参数：
     Num: TCnBigDecimal                   - 待计算的大十进制浮点数
     out IntCount: Integer                - 返回整数部分长度
     out DecimalCount: Integer            - 返回小数部分长度

   返回值：Boolean                        - 返回计算是否成功
}

function BigDecimalGetHighScale(Num: TCnBigDecimal): Integer;
{* 计算大十进制浮点数的最高有效数字位是小数点后第几位，如果返回小于 0，则求负后表示是小数点前第几位。

   参数：
     Num: TCnBigDecimal                   - 待计算的大十进制浮点数

   返回值：Integer                        - 返回最高有效数字位离小数点的位数距离
}

function BigDecimalAdd(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal): Boolean;
{* 大十进制浮点数加，Res 可以是 Num1 或 Num2，Num1 可以是 Num2。

   参数：
     Res: TCnBigDecimal                   - 大十进制浮点数和
     Num1: TCnBigDecimal                  - 大十进制浮点数加数一
     Num2: TCnBigDecimal                  - 大十进制浮点数加数二

   返回值：Boolean                        - 返回相加是否成功
}

function BigDecimalSub(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal): Boolean;
{* 大十进制浮点数减，Res 可以是 Num1 或 Num2，Num1 可以是 Num2。

   参数：
     Res: TCnBigDecimal                   - 大十进制浮点数差
     Num1: TCnBigDecimal                  - 大十进制浮点数被减数
     Num2: TCnBigDecimal                  - 大十进制浮点数减数

   返回值：Boolean                        - 返回相减是否成功
}

function BigDecimalMul(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal; MulPrecision: Integer = 0): Boolean;
{* 大十进制浮点数乘，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
   MulPrecision 表示乘法最多保留小数点后几位，0 表示全部保留。

   参数：
     Res: TCnBigDecimal                   - 大十进制浮点数积
     Num1: TCnBigDecimal                  - 大十进制浮点数乘数一
     Num2: TCnBigDecimal                  - 大十进制浮点数乘数二
     MulPrecision: Integer                - 保留小数点后几位，0 表示全部保留

   返回值：Boolean                        - 返回相乘是否成功
}

function BigDecimalDiv(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal; DivPrecision: Integer = 0): Boolean;
{* 大十进制浮点数除，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
   DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来。

   参数：
     Res: TCnBigDecimal                   - 大十进制浮点数商
     Num1: TCnBigDecimal                  - 大十进制浮点数被除数
     Num2: TCnBigDecimal                  - 大十进制浮点数除数
     DivPrecision: Integer                - 保留小数点后几位，0 表示按默认设置来

   返回值：Boolean                        - 返回相除是否成功
}

function BigDecimalSqrt(Res: TCnBigDecimal; Num: TCnBigDecimal;
  SqrtPrecision: Integer = 0): Boolean;
{* 大十进制浮点数开平方根，Res 可以是 Num。
   SqrtPrecision 表示开平方根精度最多保留小数点后几位，0 表示按默认设置来。

   参数：
     Res: TCnBigDecimal                   - 大十进制浮点数平方根
     Num: TCnBigDecimal                   - 待计算的大十进制浮点数
     SqrtPrecision: Integer               - 保留小数点后几位，0 表示按默认设置来

   返回值：Boolean                        - 返回开方是否成功
}

procedure BigDecimalSqrt2(Res: TCnBigDecimal; Num: TCnBigDecimal;
  RoundCount: Integer = CN_SQRT_DEFAULT_ROUND_COUNT);
{* 大十进制浮点数开平方根，Res 可以是 Num，小数点后准确几位不易控制因而此处未实现。
   内部使用大有理数保持精度，RoundCount 表示内部迭代次数，0 表示按默认设置来。
   注意迭代次数如太多会导致大有理数运算较慢。

   参数：
     Res: TCnBigDecimal                   - 大十进制浮点数平方根
     Num: TCnBigDecimal                   - 待计算的大十进制浮点数
     RoundCount: Integer                  - 内部迭代次数，默认 10 次

   返回值：（无）
}

function BigDecimalChangeToScale(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大十进制浮点数在值不咋变的前提下转换到指定 Scale，也就是小数点后 Scale 位，可能产生舍入，按指定模式来。
   如果 Scale 为负，代表舍入到整 10 次方。Res 可以是 Num。

   参数：
     Res: TCnBigDecimal                   - 返回转换后的大十进制浮点数
     Num: TCnBigDecimal                   - 待转换的大十进制浮点数
     Scale: Integer                       - 小数点后 Scale 位
     RoundMode: TCnBigRoundMode           - 舍入的规则

   返回值：Boolean                        - 返回舍入是否成功
}

function BigDecimalRoundToDigits(Res: TCnBigDecimal; Num: TCnBigDecimal;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大十进制浮点数在值不咋变的前提下按指定模式舍入到指定小数点后 Digits 位，
   如果本身精度不够 Digits 位则不变。Res 可以是 Num。

   参数：
     Res: TCnBigDecimal                   - 返回舍入后的大十进制浮点数
     Num: TCnBigDecimal                   - 待舍入的大十进制浮点数
     Digits: Integer                      - 保留小数点后几位
     RoundMode: TCnBigRoundMode           - 舍入的规则

   返回值：Boolean                        - 返回舍入是否成功
}

function BigDecimalTrunc(Res: TCnBigDecimal; Num: TCnBigDecimal): Boolean;
{* 将大十进制浮点数 Trunc 到只剩整数。Res 可以是 Num。

   参数：
     Res: TCnBigDecimal                   - 返回的大十进制浮点数
     Num: TCnBigDecimal                   - 待处理的大十进制浮点数

   返回值：Boolean                        - 返回处理是否成功
}

procedure BigDecimalToBigRational(Res: TCnBigRational; Num: TCnBigDecimal);
{* 大十进制浮点数转换为大有理数。

   参数：
     Res: TCnBigRational                  - 返回转换的大有理数
     Num: TCnBigDecimal                   - 待转换的大十进制浮点数

   返回值：（无）
}

procedure BigRationalToBigDecimal(Res: TCnBigDecimal; Num: TCnBigRational;
  Digits: Integer = 20);
{* 大有理数转换为大十进制浮点数，可能有精度损失，默认保留小数点后 20 位。

   参数：
     Res: TCnBigDecimal                   - 返回转换的大十进制浮点数
     Num: TCnBigRational                  - 待转换的大有理数
     Digits: Integer                      - 保留小数点后几位，默认 20

   返回值：（无）
}

function BigDecimalDebugDump(Num: TCnBigDecimal): string;
{* 打印大十进制浮点数内部信息。

   参数：
     Num: TCnBigDecimal                   - 待打印的大十进制浮点数

   返回值：string                         - 返回内部信息字符串
}

// ========================== 大二进制浮点数操作函数 ===========================

procedure BigBinaryClear(Num: TCnBigBinary);
{* 清空一个大二进制浮点数对象，实质上是 Value 与 Scale 都设置为 0。

   参数：
     Num: TCnBigBinary                    - 待清空的大二进制浮点数

   返回值：（无）
}

function BigBinarySetDec(const Buf: string; Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置字符串值。

   参数：
     const Buf: string                    - 待设置的字符串
     Res: TCnBigBinary                    - 待设置的大二进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigBinarySetWord(W: Cardinal; Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置整数值。

   参数：
     W: Cardinal                          - 待设置的 32 位无符号整数值
     Res: TCnBigBinary                    - 待设置的大二进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigBinarySetInt64(W: Int64; Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置 Int64 整数值。

   参数：
     W: Int64                             - 待设置的 64 位有符号整数值
     Res: TCnBigBinary                    - 待设置的大二进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigBinarySetSingle(Value: Single; Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置单精度浮点值。

   参数：
     Value: Single                        - 待设置的单精度浮点值
     Res: TCnBigBinary                    - 待设置的大二进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigBinarySetDouble(Value: Double; Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置双精度浮点值。

   参数：
     Value: Double                        - 待设置的双精度浮点值
     Res: TCnBigBinary                    - 待设置的大二进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigBinarySetExtended(Value: Extended; Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置扩展精度浮点值。

   参数：
     Value: Extended                      - 待设置的扩展精度浮点值
     Res: TCnBigBinary                    - 待设置的大二进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigBinarySetBigNumber(Num: TCnBigNumber; Res: TCnBigBinary): Boolean;
{* 为大二进制浮点数对象设置大数值。

   参数：
     Num: TCnBigNumber                    -
     Res: TCnBigBinary                    - 待设置的大二进制浮点数

   返回值：Boolean                        - 返回设置是否成功
}

function BigBinaryToString(Num: TCnBigBinary): string;
{* 大二进制浮点数对象转换为字符串。

   参数：
     Num: TCnBigBinary                    - 待转换的大二进制浮点数

   返回值：string                         - 返回转换的字符串
}

function BigBinaryToSingle(Num: TCnBigBinary): Single;
{* 大二进制浮点数对象转换为单精度浮点数。

   参数：
     Num: TCnBigBinary                    - 待转换的大二进制浮点数

   返回值：Single                         - 返回单精度浮点值
}

function BigBinaryToDouble(Num: TCnBigBinary): Double;
{* 大二进制浮点数对象转换为双精度浮点数。

   参数：
     Num: TCnBigBinary                    - 待转换的大二进制浮点数

   返回值：Double                         - 返回双精度浮点值
}

function BigBinaryToExtended(Num: TCnBigBinary): Extended;
{* 大二进制浮点数对象转换为扩展精度浮点数。

   参数：
     Num: TCnBigBinary                    - 待转换的大二进制浮点数

   返回值：Extended                       - 返回扩展精度浮点值
}

function BigBinaryEqual(Num1: TCnBigBinary; Num2: TCnBigBinary): Boolean;
{* 比较两个大二进制浮点数对象是否相等。

   参数：
     Num1: TCnBigBinary                   - 待比较的大二进制浮点数一
     Num2: TCnBigBinary                   - 待比较的大二进制浮点数二

   返回值：Boolean                        - 返回是否相等
}

function BigBinaryCompare(Num1: TCnBigBinary; Num2: TCnBigBinary): Integer; overload;
{* 比较两个大二进制浮点数对象，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     Num1: TCnBigBinary                   - 待比较的大二进制浮点数一
     Num2: TCnBigBinary                   - 待比较的大二进制浮点数二

   返回值：Integer                        - 返回比较结果
}

function BigBinaryCompare(Num1: TCnBigBinary; Num2: Int64): Integer; overload;
{* 比较大二进制浮点数对象与整数，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     Num1: TCnBigBinary                   - 待比较的大二进制浮点数
     Num2: Int64                          - 待比较的整数

   返回值：Integer                        - 返回比较结果
}

function BigBinaryCompare(Num1: TCnBigBinary; Num2: Extended): Integer; overload;
{* 比较大二进制浮点数对象与浮点数，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     Num1: TCnBigBinary                   - 待比较的大二进制浮点数
     Num2: Extended                       - 待比较的浮点数

   返回值：Integer                        - 返回比较结果
}

procedure BigBinaryCopy(Dest: TCnBigBinary; Source: TCnBigBinary);
{* 大二进制浮点数赋值

   参数：
     Dest: TCnBigBinary                   - 目标大二进制浮点数
     Source: TCnBigBinary                 - 源大二进制浮点数

   返回值：（无）
}

function BigBinaryGetHighScale(Num: TCnBigBinary): Integer;
{* 计算大二进制浮点数的最高有效数字位是小数点后第几位，如果返回小于 0，则求负后表示是小数点前第几位

   参数：
     Num: TCnBigBinary                    - 待计算的大二进制浮点数

   返回值：Integer                        - 返回最高有效数字位离小数点的位数距离
}

function BigBinaryAdd(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary): Boolean;
{* 大二进制浮点数加，Res 可以是 Num1 或 Num2，Num1 可以是 Num2。

   参数：
     Res: TCnBigBinary                    - 大二进制浮点数和
     Num1: TCnBigBinary                   - 大二进制浮点数加数一
     Num2: TCnBigBinary                   - 大二进制浮点数加数二

   返回值：Boolean                        - 返回相加是否成功
}

function BigBinarySub(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary): Boolean;
{* 大二进制浮点数减，Res 可以是 Num1 或 Num2，Num1 可以是 Num2。

   参数：
     Res: TCnBigBinary                    - 大二进制浮点数差
     Num1: TCnBigBinary                   - 大二进制浮点数被减数
     Num2: TCnBigBinary                   - 大二进制浮点数减数

   返回值：Boolean                        - 返回相减是否成功
}

function BigBinaryMul(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary; MulPrecision: Integer = 0): Boolean;
{* 大二进制浮点数乘，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
   MulPrecision 表示乘法最多保留小数点后几位，0 表示全部保留。

   参数：
     Res: TCnBigBinary                    - 大二进制浮点数积
     Num1: TCnBigBinary                   - 大二进制浮点数乘数一
     Num2: TCnBigBinary                   - 大二进制浮点数乘数二
     MulPrecision: Integer                - 保留小数点后几位，0 表示全部保留

   返回值：Boolean                        - 返回相乘是否成功
}

function BigBinaryDiv(Res: TCnBigBinary; Num1: TCnBigBinary;
  Num2: TCnBigBinary; DivPrecision: Integer = 0): Boolean;
{* 大二进制浮点数除，Res 可以是 Num1 或 Num2，Num1 可以是 Num2，
   DivPrecision 表示除法精度最多保留小数点后几位，0 表示按默认设置来。

   参数：
     Res: TCnBigBinary                    - 大二进制浮点数商
     Num1: TCnBigBinary                   - 大二进制浮点数被除数
     Num2: TCnBigBinary                   - 大二进制浮点数除数
     DivPrecision: Integer                - 保留小数点后几位，0 表示按默认设置来

   返回值：Boolean                        - 返回相除是否成功
}

procedure BigBinaryShiftLeft(Res: TCnBigBinary; N: Integer);
{* 大二进制浮点数左移，内部直接调整 FScale

   参数：
     Res: TCnBigBinary                    - 待左移的大二进制浮点数
     N: Integer                           - 左移位数

   返回值：（无）
}

procedure BigBinaryShiftRight(Res: TCnBigBinary; N: Integer);
{* 大二进制浮点数右移，内部直接调整 FScale。

   参数：
     Res: TCnBigBinary                    - 待右移的大二进制浮点数
     N: Integer                           - 右移位数

   返回值：（无）
}

function BigBinaryPower(Res: TCnBigBinary; N: Integer): Boolean;
{* 大二进制浮点数求幂，只支持非负整数幂。

   参数：
     Res: TCnBigBinary                    - 待求幂的大二进制浮点数
     N: Integer                           - 幂指数，只支持非负整数

   返回值：Boolean                        - 返回求幂是否成功
}

function BigBinaryChangeToScale(Res: TCnBigBinary; Num: TCnBigBinary;
  Scale: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大二进制浮点数在值不咋变的前提下转换到指定 Scale，也就是小数点后 Scale 位，可能产生舍入，按指定模式进行。
   如果 Scale 为负，代表舍入到整 2 次方。Res 可以是 Num。

   参数：
     Res: TCnBigBinary                    - 返回转换后的大二进制浮点数
     Num: TCnBigBinary                    - 待转换的大二进制浮点数
     Scale: Integer                       - 小数点后 Scale 位
     RoundMode: TCnBigRoundMode           - 舍入的规则

   返回值：Boolean                        - 返回舍入是否成功
}

function BigBinaryRoundToDigits(Res: TCnBigBinary; Num: TCnBigBinary;
  Digits: Integer; RoundMode: TCnBigRoundMode = drTowardsZero): Boolean;
{* 将大二进制浮点数在值不咋变的前提下按指定模式舍入到指定小数点后 Digits 二进制位，
   如果本身精度不够 Digits 位则不变。Res 可以是 Num。

   参数：
     Res: TCnBigBinary                    - 返回舍入后的大二进制浮点数
     Num: TCnBigBinary                    - 待舍入的大二进制浮点数
     Digits: Integer                      - 保留小数点后几个二进制位
     RoundMode: TCnBigRoundMode           - 舍入的规则

   返回值：Boolean                        - 返回舍入是否成功
}

function BigBinaryTrunc(Res: TCnBigBinary; Num: TCnBigBinary): Boolean;
{* 将大二进制浮点数 Trunc 到只剩整数。Res 可以是 Num。

   参数：
     Res: TCnBigBinary                    - 返回的大二进制浮点数
     Num: TCnBigBinary                    - 待处理的大二进制浮点数

   返回值：Boolean                        - 返回处理是否成功
}

function BigBinaryTruncTo(Res: TCnBigNumber; Num: TCnBigBinary): Boolean;
{* 将大二进制浮点数 Trunc 到只剩整数并放置于大数中。

   参数：
     Res: TCnBigNumber                    - 返回的大整数
     Num: TCnBigBinary                    - 待处理的大二进制浮点数

   返回值：Boolean                        - 返回处理是否成功
}

function BigBinaryDebugDump(Num: TCnBigBinary): string;
{* 打印大二进制浮点数内部信息。

   参数：
     Num: TCnBigBinary                    - 待打印的大二进制浮点数

   返回值：string                         - 返回内部信息字符串
}

var
  CnBigDecimalOne: TCnBigDecimal = nil;     // 表示 1 的常量
  CnBigDecimalZero: TCnBigDecimal = nil;    // 表示 0 的常量
  CnBigDecimalNegOne: TCnBigDecimal = nil;  // 表示 -1 的常量

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
//    // 10 ^ 19 10000000000000000000 已经超了 Int64 9223372036854775807
//    // 所以得用 16 进制写但没超 UInt64 18446744073709551615，10 ^ 20 才超
//  );

var
  FLocalBigDecimalPool: TCnBigDecimalPool = nil;
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalBigBinaryPool: TCnBigBinaryPool = nil;

  FDefaultDecimalPrecisionDigits: Integer = CN_BIG_DECIMAL_DEFAULT_PRECISION;
  FDefaultBinaryPrecisionDigits: Integer = CN_BIG_BINARY_DEFAULT_PRECISION;

// 根据精度数字整出一个差值供迭代求解过程中停止
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
  // 把 1. 开头的有效数字当成整数，E 需要减 23
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
  // 把 1. 开头的有效数字当成整数，E 需要减 52
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
  // 把 1. 开头的有效数字当成整数，E 需要减 63
  Result := InternalBigDecimalSetFloat(N, E - CN_EXTENDED_SIGNIFICAND_BITLENGTH, S, Res);
end;

function BigDecimalToString(Num: TCnBigDecimal): string;
var
  C: Char;
  S: string;
  L: Integer;

  function TrimRightZero(const Str: string): string;
  var
    I, Len: Integer;
  begin
    Len := Length(Str);
    for I := Len downto 1 do
    begin
      if Str[I] <> '0' then
      begin
        Result := Copy(Str, 1, I);
        Exit;
      end;
    end;
    Result := ''; // 全是'0'时返回空字符串
  end;

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
  begin
    Result := '0.' + StringOfChar('0', Num.FScale - L) + S;
    Result := TrimRightZero(Result);
  end
  else
  begin
    Result := Copy(S, 1, L - Num.FScale) + '.' + Copy(S, L - Num.FScale + 1, MaxInt);
    Result := TrimRightZero(Result);
  end;

  // 小数点后去掉 0 之后，再把正负号加回来
  if C <> #0 then
    Result := C + Result;
end;

// 通过巨大的变换让大十进制浮点数的原始值等于 Value / 2^Scale，并且有效数字满足特定位数 
function InternalBigDecimalConvertToBitsCount(Num: TCnBigDecimal; BitsCount: Integer): Boolean;
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

function BigDecimalToSingle(Num: TCnBigDecimal): Single;
var
  T: TCnBigDecimal;
  E: Integer;
  M: Cardinal;
begin
  if Num.Value.IsZero then
  begin
    Result := 0.0;
    Exit;
  end;

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

function BigDecimalToDouble(Num: TCnBigDecimal): Double;
var
  T: TCnBigDecimal;
  E: Integer;
  M: TUInt64;
begin
  if Num.Value.IsZero then
  begin
    Result := 0.0;
    Exit;
  end;

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

function BigDecimalToExtended(Num: TCnBigDecimal): Extended;
var
  T: TCnBigDecimal;
  E: Integer;
  M: TUInt64;
begin
  if Num.Value.IsZero then
  begin
    Result := 0.0;
    Exit;
  end;

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

function BigDecimalEqual(Num1: TCnBigDecimal; Num2: TCnBigDecimal): Boolean;
begin
  Result := BigDecimalCompare(Num1, Num2) = 0;
end;

function BigDecimalCompare(Num1, Num2: TCnBigDecimal): Integer;
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
    Result := BigNumberGetTenPrecision(Num.FValue); // 得到十进制整数位数
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

function BigDecimalGetHighScale(Num: TCnBigDecimal): Integer;
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

function BigDecimalSub(Res: TCnBigDecimal; Num1: TCnBigDecimal;
  Num2: TCnBigDecimal): Boolean;
var
  T: TCnBigNumber;
  L: Integer;
begin
  if Num1.FValue.IsZero then
  begin
    BigNumberCopy(Res.FValue, Num2.FValue);
    Res.FScale := Num2.FScale;
    Res.FValue.Negate;
    Result := True;
    Exit;
  end
  else if Num2.FValue.IsZero then
  begin
    BigNumberCopy(Res.FValue, Num1.FValue);
    Res.FScale := Num1.FScale;
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

  // 继续除
  S := Num1.FValue.isNegative <> Num2.FValue.IsNegative; // 符号不等结果才负
  TS := Num1.FScale - Num2.FScale;

  if DivPrecision <= 0 then
    DivPrecision := FDefaultDecimalPrecisionDigits;
  if DivPrecision < 0 then
    DivPrecision := CN_BIG_DECIMAL_DEFAULT_PRECISION;

  // 根据精度要求计算将被除数扩大的倍数，注意为了加速可能有 1 位误差
  M := CheckScaleAddRange(DivPrecision, BigNumberGetTenPrecision2(Num2.FValue)
    - BigNumberGetTenPrecision2(Num1.FValue) + 1);
  if M < 0 then  // 无需扩大、精度已经足够
    M := 0
  else if M > 0 then
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

procedure BigDecimalSqrt2(Res: TCnBigDecimal; Num: TCnBigDecimal;
  RoundCount: Integer);
var
  I: Integer;
  X0, R, D: TCnBigRational;
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
  D := nil;

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
    D.Free;
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

function BigDecimalRoundToDigits(Res: TCnBigDecimal; Num: TCnBigDecimal;
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

function BigDecimalTrunc(Res: TCnBigDecimal; Num: TCnBigDecimal): Boolean;
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

procedure BigDecimalToBigRational(Res: TCnBigRational; Num: TCnBigDecimal);
var
  T: TCnBigNumber;
begin
  if (Res <> nil) and (Num <> nil) then
  begin
    BigNumberCopy(Res.Nominator, Num.FValue);

    // 精确值为 FValue / (10^FScale)，如果 FScale > 0 则乘方到分母上去，否则相反数乘方到分子上去
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
    Res.FValue.SetDec(AnsiString(V));
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
    Res.FValue.SetDec(AnsiString(V));
    Res.FScale := 0;
  end
  else if DC < 0 then // 还要乘以 10^-DC，还是整数
  begin
    Res.FValue.SetDec(AnsiString(V));
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

      // 得到 I 位二进制值，在 DRes 里，就是小数点后的小数部分了，和整数部分拼起来
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
  // 把 1. 开头的有效数字当成整数，E 需要减 23
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
  // 把 1. 开头的有效数字当成整数，E 需要减 52
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
  // 把 1. 开头的有效数字当成整数，E 需要减 63
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

  function TrimRightZero(const Str: string): string;
  var
    I, Len: Integer;
  begin
    Len := Length(Str);
    for I := Len downto 1 do
    begin
      if Str[I] <> '0' then
      begin
        Result := Copy(Str, 1, I);
        Exit;
      end;
    end;
    Result := ''; // 全是'0'时返回空字符串
  end;

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
        Result := TrimRightZero(Result);
      finally
        FLocalBigNumberPool.Recycle(T);
        FLocalBigNumberPool.Recycle(S);
        FLocalBigNumberPool.Recycle(P10);
      end;
    end;
  end;
end;

function BigBinaryEqual(Num1: TCnBigBinary; Num2: TCnBigBinary): Boolean;
begin
  Result := BigBinaryCompare(Num1, Num2) = 0;
end;

function BigBinaryCompare(Num1, Num2: TCnBigBinary): Integer; overload;
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

// 通过值基本不变的变换让大二进制浮点数的有效数字满足特定位数，超长时截断，不够时乘 2 的整数次方补全，并同时都调整 FScale
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
      D := C - BitsCount;  // 要截掉 D 个位，也就是要把 FScale 减少 D，
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
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // 清除最高位的 1

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
    T.FValue.ClearBit(T.FValue.GetBitsCount - 1); // 清除最高位的 1

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
    // 无需清除最高位的 1

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
    // 小数点后有 FScale 位，减去有效数字
    Result := Num.FScale - Result + 1;
    if Result <= 0 then // 小数点前第几位是从 1 开始的
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

  // 继续除
  S := Num1.FValue.isNegative <> Num2.FValue.IsNegative; // 符号不等结果才负
  TS := Num1.FScale - Num2.FScale;

  if DivPrecision <= 0 then
    DivPrecision := FDefaultBinaryPrecisionDigits;
  if DivPrecision < 0 then
    DivPrecision := CN_BIG_BINARY_DEFAULT_PRECISION;

  // 根据精度要求计算将被除数扩大的倍数
  M := CheckScaleAddRange(DivPrecision, (Num2.FValue.GetBitsCount - Num1.FValue.GetBitsCount + 1));
  if M < 0 then // 无需扩大、精度已经足够
    M := 0
  else if M > 0 then
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

function BigBinaryChangeToScale(Res: TCnBigBinary; Num: TCnBigBinary;
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

function BigBinaryRoundToDigits(Res: TCnBigBinary; Num: TCnBigBinary;
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

function BigBinaryTrunc(Res: TCnBigBinary; Num: TCnBigBinary): Boolean;
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

function BigBinaryTruncTo(Res: TCnBigNumber; Num: TCnBigBinary): Boolean;
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
  CnBigDecimalNegOne := TCnBigDecimal.Create;
  CnBigDecimalNegOne.SetOne;
  CnBigDecimalNegOne.Negate;

finalization
//  CnBigDecimalZero.DecString; // 手工调用这两句防止被编译器忽略
//  CnBigDecimalZero.DebugDump;

  CnBigDecimalNegOne.Free;
  CnBigDecimalZero.Free;
  CnBigDecimalOne.Free;

  FLocalBigNumberPool.Free;
  FLocalBigBinaryPool.Free;
  FLocalBigDecimalPool.Free;

end.
