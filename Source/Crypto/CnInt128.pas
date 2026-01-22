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

unit CnInt128;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：128 位有无符号整数的运算实现
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元以 TCnInt128 与 TCnUInt128 结构实现了 128 位整数的基本四则运算。
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 XE 2
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.06.14 V1.1
*               实现除法与求余，待测试
*           2022.06.11 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, CnNative;

const
  SCN_MAX_INT128 = '170141183460469231731687303715884105727';
  {* 最大的有符号 Int128 值，等于 $7FFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF}

  SCN_MIN_INT128 = '-170141183460469231731687303715884105728';
  {* 最小的有符号 Int128 值，等于 $80000000 00000000 00000000 00000000}

  SCN_MAX_UINT128 = '340282366920938463463374607431768211455';
  {* 最大的无符号 UInt128 值，等于 $FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF}

type
  TCnInt128 = packed record
  {* 128 位有符号整数结构}
    Lo64, Hi64: Int64;        // 注意 Lo64 内部仍作为 64 位无符号整数处理
  end;
  PCnInt128 = ^TCnInt128;
  {* 128 位有符号整数结构指针}

  TCnUInt128 = packed record
  {* 128 位无符号整数结构}
    Lo64, Hi64: TUInt64;
  end;
  PCnUInt128 = ^TCnUInt128;
  {* 128 位无符号整数结构指针}

// ========================= Int128 计算函数 ===================================

procedure Int128Set(var R: TCnInt128; Lo: Int64; Hi: Int64); overload;
{* 分别设置 128 位有符号整数的高低 64 位原始值，不额外处理正负号。

   参数：
     var R: TCnInt128                     - 待设置的 128 位有符号整数
     Lo: Int64                            - 低 64 位值
     Hi: Int64                            - 高 64 位值

   返回值：（无）
}

procedure Int128Set(var R: TCnInt128; Lo: Int64); overload;
{* 设置 128 位有符号整数的低 64 位值，高 64 位根据 Lo 的正负情况置全 0 或全 1。

   参数：
     var R: TCnInt128                     - 待设置的 128 位有符号整数
     Lo: Int64                            - 低 64 位值

   返回值：（无）
}

procedure Int128Copy(var D: TCnInt128; var S: TCnInt128);
{* 复制 128 位有符号整数。

   参数：
     var D: TCnInt128                     - 目的 128 位有符号整数
     var S: TCnInt128                     - 源 128 位有符号整数

   返回值：（无）
}

function Int128IsZero(var N: TCnInt128): Boolean;
{* 判断 一 128 位有符号整数是否是 0。

   参数：
     var N: TCnInt128                     - 待判断的 128 位有符号整数

   返回值：Boolean                        - 返回是否是 0
}

procedure Int128SetZero(var N: TCnInt128);
{* 将一 128 位有符号整数置 0。

   参数：
     var N: TCnInt128                     - 待设置的 128 位有符号整数

   返回值：（无）
}

procedure Int128Add(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128); overload;
{* 128 位有符号整数相加，不考虑溢出的情况。R、A、B 可以相同。A B 使用补码无需分开考虑正负值。

   参数：
     var R: TCnInt128                     - 和
     var A: TCnInt128                     - 加数一
     var B: TCnInt128                     - 加数二

   返回值：（无）
}

procedure Int128Add(var R: TCnInt128; var A: TCnInt128; V: Int64); overload;
{* 给一 128 位有符号整数加上一个 64 位有符号整数。考虑了 V 为负值的情况。

   参数：
     var R: TCnInt128                     - 和
     var A: TCnInt128                     - 加数一
     V: Int64                             - 加数二

   返回值：（无）
}

procedure Int128Sub(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128); overload;
{* 128 位有符号整数相减，不考虑溢出的情况。R、A、B 可以相同。

   参数：
     var R: TCnInt128                     - 差
     var A: TCnInt128                     - 被减数
     var B: TCnInt128                     - 减数

   返回值：（无）
}

procedure Int128Sub(var R: TCnInt128; var A: TCnInt128; V: Int64); overload;
{* 给一 128 位有符号整数减去一个 64 位有符号整数。考虑了 V 为负值的情况。

   参数：
     var R: TCnInt128                     - 差
     var A: TCnInt128                     - 被减数
     V: Int64                             - 减数

   返回值：（无）
}

procedure Int128Mul(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 128 位有符号整数相乘，有溢出则抛异常（暂时未实现 UInt128 的 ResHi 参数机制）。R、A、B 可以相同。

   参数：
     var R: TCnInt128                     - 积
     var A: TCnInt128                     - 乘数一
     var B: TCnInt128                     - 乘数二

   返回值：（无）
}

procedure Int128DivMod(var A: TCnInt128; var B: TCnInt128; var R: TCnInt128; var M: TCnInt128);
{* 128 位有符号整数整除求余，A / B = R ... M 其中 A、B、R、M 可以复用但 R M 不能相同。

   参数：
     var A: TCnInt128                     - 被除数
     var B: TCnInt128                     - 除数
     var R: TCnInt128                     - 商
     var M: TCnInt128                     - 余数

   返回值：（无）
}

procedure Int128Div(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 128 位有符号整数整除，R = A div B。R、A、B 可以相同。

   参数：
     var R: TCnInt128                     - 商
     var A: TCnInt128                     - 被除数
     var B: TCnInt128                     - 除数

   返回值：（无）
}

procedure Int128Mod(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 128 位有符号整数求余，R = A mod B。R、A、B 可以相同。

   参数：
     var R: TCnInt128                     - 余数
     var A: TCnInt128                     - 被除数
     var B: TCnInt128                     - 除数

   返回值：（无）
}

procedure Int128ShiftLeft(var N: TCnInt128; S: Integer);
{* 128 位有符号整数按位左移 S 位，如 S 为负，表示右移。

   参数：
     var N: TCnInt128                     - 待移位的 128 位有符号整数
     S: Integer                           - 左移位数

   返回值：（无）
}

procedure Int128ShiftRight(var N: TCnInt128; S: Integer);
{* 128 位有符号整数按位右移 S 位，如 S 为负，表示左移

   参数：
     var N: TCnInt128                     - 待移位的 128 位有符号整数
     S: Integer                           - 右移位数

   返回值：（无）
}

procedure Int128And(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 两个 128 位有符号整数按位与。

   参数：
     var R: TCnInt128                     - 按位与结果
     var A: TCnInt128                     - 参与按位与的运算参数一
     var B: TCnInt128                     - 参与按位与的运算参数二

   返回值：（无）
}

procedure Int128Or(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 两个 128 位有符号整数按位或。

   参数：
     var R: TCnInt128                     - 按位或结果
     var A: TCnInt128                     - 参与按位或的运算参数一
     var B: TCnInt128                     - 参与按位或的运算参数二

   返回值：（无）
}

procedure Int128Xor(var R: TCnInt128; var A: TCnInt128; var B: TCnInt128);
{* 两个 128 位有符号整数按位异或。

   参数：
     var R: TCnInt128                     - 按位异或结果
     var A: TCnInt128                     - 参与按位异或的运算参数一
     var B: TCnInt128                     - 参与按位异或的运算参数二

   返回值：（无）
}

procedure Int128Negate(var N: TCnInt128);
{* 将一 128 位有符号整数置为其相反数，注意当最小的 Int128 负值时会产生溢出异常。

   参数：
     var N: TCnInt128                     - 待取负值的 128 位有符号整数

   返回值：（无）
}

procedure Int128Not(var N: TCnInt128);
{* 将一 128 位有符号整数求反。

   参数：
     var N: TCnInt128                     - 待求反的 128 位有符号整数

   返回值：（无）
}

procedure Int128SetBit(var N: TCnInt128; Bit: Integer);
{* 将一 128 位有符号整数的某一位置 1，Bit 从 0 到 127。

   参数：
     var N: TCnInt128                     - 待置位的 128 位有符号整数
     Bit: Integer                         - 位索引号

   返回值：（无）
}

procedure Int128ClearBit(var N: TCnInt128; Bit: Integer);
{* 将一 128 位有符号整数的某一位置 0，Bit 从 0 到 127。

   参数：
     var N: TCnInt128                     - 待置位的 128 位有符号整数
     Bit: Integer                         - 位索引号

   返回值：（无）
}

function Int128IsBitSet(var N: TCnInt128; Bit: Integer): Boolean;
{* 返回一 128 位有符号整数的某一位是否是 1，Bit 从 0 到 127

   参数：
     var N: TCnInt128                     - 待计算的 128 位有符号整数
     Bit: Integer                         - 位索引号

   返回值：Boolean                        - 返回该位是否为 1
}

function Int128IsNegative(var N: TCnInt128): Boolean;
{* 判断一 128 位有符号整数是否是负数。

   参数：
     var N: TCnInt128                     - 待判断的 128 位有符号整数

   返回值：Boolean                        - 返回是否负数
}

function Int128Equal(var A: TCnInt128; var B: TCnInt128): Boolean; overload;
{* 判断两个 128 位有符号整数是否相等。

   参数：
     var A: TCnInt128                     - 待判断的 128 位有符号整数一
     var B: TCnInt128                     - 待判断的 128 位有符号整数二

   返回值：Boolean                        - 返回是否相等
}

function Int128Equal(var A: TCnInt128; B: Int64): Boolean; overload;
{* 判断一个 128 位有符号整数与一个 Int64 是否相等。

   参数：
     var A: TCnInt128                     - 待判断的 128 位有符号整数
     B: Int64                             - 待判断的 64 位整数

   返回值：Boolean                        - 返回是否相等
}

function Int128Compare(var A: TCnInt128; var B: TCnInt128): Integer;
{* 比较两个 128 位有符号整数，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     var A: TCnInt128                     - 待比较的 128 位有符号整数一
     var B: TCnInt128                     - 待比较的 128 位有符号整数二

   返回值：Integer                        - 返回比较结果
}

function Int128ToHex(var N: TCnInt128): string;
{* 将 128 位有符号整数转换为十六进制字符串。

   参数：
     var N: TCnInt128                     - 待转换的 128 位有符号整数

   返回值：string                         - 返回十六进制字符串
}

function HexToInt128(const S: string): TCnInt128;
{* 将十六进制字符串转换为 128 位有符号整数。

   参数：
     const S: string                      - 待转换的十六进制字符串

   返回值：TCnInt128                      - 返回 128 位有符号整数
}

function Int128ToStr(var N: TCnInt128): string;
{* 将 128 位有符号整数转换为十进制字符串。

   参数：
     var N: TCnInt128                     - 待转换的 128 位有符号整数

   返回值：string                         - 返回十进制字符串
}

function StrToInt128(const S: string): TCnInt128;
{* 将十进制字符串转换为 128 位有符号整数。

   参数：
     const S: string                      - 待转换的十进制字符串

   返回值：TCnInt128                      - 返回 128 位有符号整数
}

// ======================== UInt128 计算函数 ===================================

procedure UInt128Set(var R: TCnUInt128; Lo: TUInt64; Hi: TUInt64); overload;
{* 分别设置 128 位无符号整数的高低 64 位值。

   参数：
     var R: TCnUInt128                    - 待设置的 128 位无符号整数
     Lo: TUInt64                          - 低 64 位值
     Hi: TUInt64                          - 高 64 位值

   返回值：（无）
}

procedure UInt128Set(var R: TCnUInt128; Lo: TUInt64); overload;
{* 设置 128 位无符号整数的低 64 位值，高位置 0。

   参数：
     var R: TCnUInt128                    - 待设置的 128 位无符号整数
     Lo: TUInt64                          - 低 64 位值

   返回值：（无）
}

procedure UInt128Copy(var D: TCnUInt128; var S: TCnUInt128);
{* 复制 128 位无符号整数。

   参数：
     var D: TCnUInt128                    - 目的 128 位无符号整数
     var S: TCnUInt128                    - 源 128 位无符号整数

   返回值：（无）
}

function UInt128IsZero(var N: TCnUInt128): Boolean;
{* 判断 一 128 位无符号整数是否是 0。

   参数：
     var N: TCnUInt128                    - 待判断的 128 位无符号整数

   返回值：Boolean                        - 返回是否是 0
}

procedure UInt128SetZero(var N: TCnUInt128);
{* 将一 128 位无符号整数置 0。

   参数：
     var N: TCnUInt128                    - 待设置的 128 位无符号整数

   返回值：（无）
}

procedure UInt128Add(var R: TCnUInt128; var A: TCnUInt128; V: TUInt64); overload;
{* 给一 128 位无符号整数加上一个 64 位无符号整数。

   参数：
     var R: TCnUInt128                    - 和
     var A: TCnUInt128                    - 加数一
     V: TUInt64                           - 加数二

   返回值：（无）
}

procedure UInt128Add(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128); overload;
{* 128 位无符号整数相加，不考虑溢出的情况。R、A、B 可以相同。

   参数：
     var R: TCnUInt128                    - 和
     var A: TCnUInt128                    - 加数一
     var B: TCnUInt128                    - 加数二

   返回值：（无）
}

procedure UInt128Sub(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 128 位无符号整数相减，不考虑溢出的情况。R、A、B 可以相同。

   参数：
     var R: TCnUInt128                    - 差
     var A: TCnUInt128                    - 被减数
     var B: TCnUInt128                    - 减数

   返回值：（无）
}

procedure UInt128Mul(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128; ResHi: PCnUInt128 = nil);
{* 128 位无符号整数相乘，有溢出则超过 128 位的放 ResHi 中。
   ResHi 如传 nil 且溢出则抛异常。R、A、B 可以相同。

   参数：
     var R: TCnUInt128                    - 积
     var A: TCnUInt128                    - 乘数一
     var B: TCnUInt128                    - 乘数二
     ResHi: PCnUInt128                    - 溢出位存放地址，如为 nil，碰到溢出则抛异常

   返回值：（无）
}

procedure UInt128DivMod(var A: TCnUInt128; var B: TCnUInt128; var R: TCnUInt128; var M: TCnUInt128);
{* 128 位无符号整数整除求余，A / B = R ... M。A、B、R、M 可以复用但 R M 不能相同。

   参数：
     var A: TCnUInt128                    - 被除数
     var B: TCnUInt128                    - 除数
     var R: TCnUInt128                    - 商
     var M: TCnUInt128                    - 余数

   返回值：（无）
}

procedure UInt128Div(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 128 位无符号整数整除，R = A div B。R、A、B 可以相同。

   参数：
     var R: TCnUInt128                    - 商
     var A: TCnUInt128                    - 被除数
     var B: TCnUInt128                    - 除数

   返回值：（无）
}

procedure UInt128Mod(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 128 位无符号整数求余，R = A mod B。R、A、B 可以相同。

   参数：
     var R: TCnUInt128                    - 余数
     var A: TCnUInt128                    - 被除数
     var B: TCnUInt128                    - 除数

   返回值：（无）
}

procedure UInt128ShiftLeft(var N: TCnUInt128; S: Integer);
{* 128 位无符号整数按位左移 S 位，如 S 为负，表示右移。

   参数：
     var N: TCnUInt128                    - 待移位的 128 位无符号整数
     S: Integer                           - 左移位数

   返回值：（无）
}

procedure UInt128ShiftRight(var N: TCnUInt128; S: Integer);
{* 128 位无符号整数按位右移 S 位，如 S 为负，表示左移。

   参数：
     var N: TCnUInt128                    - 待移位的 128 位无符号整数
     S: Integer                           - 右移位数

   返回值：（无）
}

procedure UInt128And(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 两个 128 位无符号整数按位与。

   参数：
     var R: TCnUInt128                    - 按位与结果
     var A: TCnUInt128                    - 参与按位与的运算参数一
     var B: TCnUInt128                    - 参与按位与的运算参数二

   返回值：（无）
}

procedure UInt128Or(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 两个 128 位无符号整数按位或。

   参数：
     var R: TCnUInt128                    - 按位或结果
     var A: TCnUInt128                    - 参与按位异或的运算参数一
     var B: TCnUInt128                    - 参与按位异或的运算参数二

   返回值：（无）
}

procedure UInt128Xor(var R: TCnUInt128; var A: TCnUInt128; var B: TCnUInt128);
{* 两个 128 位无符号整数按位异或。

   参数：
     var R: TCnUInt128                    - 按位异或结果
     var A: TCnUInt128                    - 参与按位异或的运算参数一
     var B: TCnUInt128                    - 参与按位异或的运算参数二

   返回值：（无）
}

procedure UInt128Not(var N: TCnUInt128);
{* 128 位无符号整数求反。

   参数：
     var N: TCnUInt128                    - 待求反的 128 位无符号整数

   返回值：（无）
}

procedure UInt128SetBit(var N: TCnUInt128; Bit: Integer);
{* 将一 128 位无符号整数的某一位置 1，Bit 从 0 到 127。

   参数：
     var N: TCnUInt128                    - 待置位的 128 位无符号整数
     Bit: Integer                         - 位索引号

   返回值：（无）
}

procedure UInt128ClearBit(var N: TCnUInt128; Bit: Integer);
{* 将一 128 位无符号整数的某一位置 0，Bit 从 0 到 127。

   参数：
     var N: TCnUInt128                    - 待置位的 128 位无符号整数
     Bit: Integer                         - 位索引号

   返回值：（无）
}

function UInt128IsBitSet(var N: TCnUInt128; Bit: Integer): Boolean;
{* 返回一 128 位无符号整数的某一位是否是 0，Bit 从 0 到 127。

   参数：
     var N: TCnUInt128                    - 待计算的 128 位无符号整数
     Bit: Integer                         - 位索引号

   返回值：Boolean                        - 返回该位是否为 1
}

function UInt128Equal(var A: TCnUInt128; var B: TCnUInt128): Boolean; overload;
{* 判断两个 128 位无符号整数是否相等。

   参数：
     var A: TCnUInt128                    - 待判断的 128 位无符号整数一
     var B: TCnUInt128                    - 待判断的 128 位无符号整数二

   返回值：Boolean                        - 返回是否相等
}

function UInt128Equal(var A: TCnUInt128; B: TUInt64): Boolean; overload;
{* 判断一个 128 位无符号整数与一个 Int64/UInt64 是否相等。

   参数：
     var A: TCnUInt128                    - 待判断的 128 位无符号整数
     B: TUInt64                           - 待判断的 64 位整数

   返回值：Boolean                        - 返回是否相等
}

function UInt128Compare(var A: TCnUInt128; var B: TCnUInt128): Integer;
{* 比较两个 128 位无符号整数，分别根据比较的结果是大于、等于还是小于来返回 1、0、-1。

   参数：
     var A: TCnUInt128                    - 待比较的 128 位无符号整数一
     var B: TCnUInt128                    - 待比较的 128 位无符号整数二

   返回值：Integer                        - 返回比较结果
}

function IsUInt128AddOverflow(var A: TCnUInt128; var B: TCnUInt128): Boolean;
{* 判断两个 64 位无符号数相加是否溢出 128 位无符号上限。

   参数：
     var A: TCnUInt128                    - 加数一
     var B: TCnUInt128                    - 加数二

   返回值：Boolean                        - 返回是否溢出
}

function UInt128ToHex(var N: TCnUInt128): string;
{* 将 128 位无符号整数转换为十六进制字符串。

   参数：
     var N: TCnUInt128                    - 待转换的 128 位无符号整数

   返回值：string                         - 返回十六进制字符串
}

function HexToUInt128(const S: string): TCnUInt128;
{* 将十六进制字符串转换为 128 位无符号整数。

   参数：
     const S: string                      - 待转换的十六进制字符串

   返回值：TCnUInt128                     - 返回 128 位无符号整数
}

function UInt128ToStr(var N: TCnUInt128): string;
{* 将 128 位无符号整数转换为十进制字符串。

   参数：
     var N: TCnUInt128                    - 待转换的 128 位无符号整数

   返回值：string                         - 返回十进制字符串
}

function StrToUInt128(const S: string): TCnUInt128;
{* 将十进制字符串转换为 128 位无符号整数。

   参数：
     const S: string                      - 待转换的十进制字符串

   返回值：TCnUInt128                     - 返回 128 位无符号整数
}

var
  CnInt128Zero: TCnInt128 = (Lo64: 0; Hi64: 0);
  {* 代表 0 的 128 位有符号整数常量}
  CnInt128One: TCnInt128 = (Lo64:1; Hi64: 0);
  {* 代表 1 的 128 位有符号整数常量}

  CnUInt128Zero: TCnUInt128 = (Lo64: 0; Hi64: 0);
  {* 代表 0 的 128 位无符号整数常量}
  CnUInt128One: TCnUInt128 = (Lo64:1; Hi64: 0);
  {* 代表 0 的 128 位无符号整数常量}

implementation

const
  SCnErrorInt128NegateOverflow = 'Int128 Negate Overflow';
  SCnErrorInt128MulOverflow = 'Int128 Mul Overflow';
  SCnErrorUint128MulOverflow = 'UInt128 Mul Overflow';

procedure Int128Set(var R: TCnInt128; Lo, Hi: Int64);
begin
  R.Lo64 := Lo;
  R.Hi64 := Hi;
end;

procedure Int128Set(var R: TCnInt128; Lo: Int64);
begin
  R.Lo64 := Lo;
  if Lo >= 0 then
    R.Hi64 := 0
  else
    R.Hi64 := not 0;
end;

procedure Int128Copy(var D, S: TCnInt128);
begin
  D.Lo64 := S.Lo64;
  D.Hi64 := S.Hi64;
end;

function Int128IsZero(var N: TCnInt128): Boolean;
begin
  Result := (N.Lo64 = 0) and (N.Hi64 = 0);
end;

procedure Int128SetZero(var N: TCnInt128);
begin
  N.Lo64 := 0;
  N.Hi64 := 0;
end;

procedure Int128Add(var R, A, B: TCnInt128);
var
  C: Integer;
begin
{$IFDEF SUPPORT_UINT64}
  UInt64Add(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(B.Lo64), C);
{$ELSE}
  UInt64Add(R.Lo64, A.Lo64, B.Lo64, C);
{$ENDIF}
  R.Hi64 := A.Hi64 + B.Hi64 + C;
end;

procedure Int128Add(var R, A: TCnInt128; V: Int64); overload;
var
  C: Integer;
begin
  if V < 0 then
  begin
    V := (not V) + 1; // 求反加一变正值然后减
{$IFDEF SUPPORT_UINT64}
    UInt64Sub(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Sub(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end
  else // V >= 0，和 UInt64 同样处理
  begin
{$IFDEF SUPPORT_UINT64}
    UInt64Add(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Add(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end;
  R.Hi64 := A.Hi64 + C;
end;

procedure Int128Sub(var R, A, B: TCnInt128);
var
  C: Integer;
begin
{$IFDEF SUPPORT_UINT64}
  UInt64Sub(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(B.Lo64), C);
{$ELSE}
  UInt64Sub(R.Lo64, A.Lo64, B.Lo64, C);
{$ENDIF}
  R.Hi64 := A.Hi64 - B.Hi64 - C;
end;

procedure Int128Sub(var R, A: TCnInt128; V: Int64);
var
  C: Integer;
begin
  if V < 0 then
  begin
    V := (not V) + 1; // 求反加一变正值然后加
{$IFDEF SUPPORT_UINT64}
    UInt64Add(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Add(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end
  else // V >= 0，和 UInt64 同样处理
  begin
{$IFDEF SUPPORT_UINT64}
    UInt64Sub(UInt64(R.Lo64), UInt64(A.Lo64), UInt64(V), C);
{$ELSE}
    UInt64Sub(R.Lo64, A.Lo64, V, C);
{$ENDIF}
  end;
  R.Hi64 := A.Hi64 - C;
end;

procedure Int128Mul(var R, A, B: TCnInt128);
var
  N1, N2: Boolean;
begin
  N1 := Int128IsNegative(A);
  N2 := Int128IsNegative(B);

  // 全变正
  if N1 then
    Int128Negate(A);
  if N2 then
    Int128Negate(B);

  UInt128Mul(TCnUInt128(R), TCnUInt128(A), TCnUInt128(B));
  if Int128IsNegative(R) then // 乘积是负说明溢出了
    raise EIntOverflow.Create(SCnErrorInt128MulOverflow);

  if N1 <> N2 then // 只要有一个变过
    Int128Negate(R);

  // 变回去
  if N1 then
    Int128Negate(A);
  if N2 then
    Int128Negate(B);
end;

procedure Int128DivMod(var A, B, R, M: TCnInt128);
var
  Sft: Integer;
  AA, BB: TCnInt128;
  NA, NB: Boolean;
begin
  if Int128IsZero(B) then
    raise EDivByZero.Create(SDivByZero);

  if Int128IsZero(A) then
  begin
    Int128SetZero(R);
    Int128SetZero(M);
    Exit;
  end;

  Int128Copy(AA, A);
  Int128Copy(BB, B);
  NA := Int128IsNegative(AA);
  NB := Int128IsNegative(BB);

  if NA then
    Int128Negate(AA);
  if NB then
    Int128Negate(BB);  // 全转正

  if Int128Compare(AA, BB) < 0 then
  begin
    Int128Copy(M, AA);
    if NA <> NB then
      Int128Negate(M); // 异号商为负
    Int128SetZero(R);
    Exit;
  end;

  Int128SetZero(R);
  Int128Copy(M, AA);
  Sft := 0;

  // 扩大除数至和被除数最高位相同且比被除数小
  while (Int128Compare(BB, M) < 0) and not GetUInt64BitSet(BB.Hi64, 62) do
  begin
    if Sft = 127 then
      Break;

    Int128ShiftLeft(BB, 1);
    Inc(Sft);
    if Int128Compare(BB, M) > 0 then
    begin
      Int128ShiftRight(BB, 1);
      Dec(Sft);
      Break;
    end;
  end;

  // 逐步除
  while True do
  begin
    if Int128Compare(BB, M) <= 0 then // 二进制，只需要减一次，D 无需乘
    begin
      Int128Sub(M, M, BB);
      Int128SetBit(R, Sft);

      // 如果此时 M 为 0，貌似可以跳出，都没余数了
      if Int128IsZero(M) then
        Exit;
    end;

    if Sft > 0 then
    begin
      Int128ShiftRight(BB, 1);
      Dec(Sft);
    end
    else
      Break;
  end;

  if NA <> NB then
    Int128Negate(R);
  if Int128IsNegative(A) then
    Int128Negate(M);
end;

procedure Int128Div(var R, A, B: TCnInt128);
var
  T: TCnInt128;
begin
  Int128DivMod(A, B, R, T);
end;

procedure Int128Mod(var R, A, B: TCnInt128);
var
  T: TCnInt128;
begin
  Int128DivMod(A, B, T, R);
end;

procedure Int128ShiftLeft(var N: TCnInt128; S: Integer);
begin
  UInt128ShiftLeft(TCnUInt128(N), S);
end;

procedure Int128ShiftRight(var N: TCnInt128; S: Integer);
begin
  UInt128ShiftRight(TCnUInt128(N), S);
end;

procedure Int128And(var R, A, B: TCnInt128);
begin
  R.Lo64 := A.Lo64 and B.Lo64;
  R.Hi64 := A.Hi64 and B.Hi64;
end;

procedure Int128Or(var R, A, B: TCnInt128);
begin
  R.Lo64 := A.Lo64 or B.Lo64;
  R.Hi64 := A.Hi64 or B.Hi64;
end;

procedure Int128Xor(var R, A, B: TCnInt128);
begin
  R.Lo64 := A.Lo64 xor B.Lo64;
  R.Hi64 := A.Hi64 xor B.Hi64;
end;

procedure Int128Negate(var N: TCnInt128);
var
  C: Integer;
begin
  // 全部求反然后总体加一
  N.Lo64 := not N.Lo64;
  N.Hi64 := not N.Hi64;

{$IFDEF SUPPORT_UINT64}
  UInt64Add(UInt64(N.Lo64), UInt64(N.Lo64), 1, C);
{$ELSE}
  UInt64Add(N.Lo64, N.Lo64, 1, C);
{$ENDIF}
  if C > 0 then
  begin
    if N.Hi64 = CN_MAX_INT64 then // Hi64 太大会产生溢出
      raise EIntOverflow.Create(SCnErrorInt128NegateOverflow);

    N.Hi64 := N.Hi64 + C;
  end;
end;

procedure Int128Not(var N: TCnInt128);
begin
  N.Lo64 := not N.Lo64;
  N.Hi64 := not N.Hi64;
end;

procedure Int128SetBit(var N: TCnInt128; Bit: Integer);
begin
{$IFDEF SUPPORT_UINT64}
  if Bit > 63 then
    UInt64SetBit(TUInt64(N.Hi64), Bit - 64)
  else
    UInt64SetBit(TUInt64(N.Lo64), Bit);
{$ELSE}
  if Bit > 63 then
    UInt64SetBit(N.Hi64, Bit - 64)
  else
    UInt64SetBit(N.Lo64, Bit);
{$ENDIF}
end;

procedure Int128ClearBit(var N: TCnInt128; Bit: Integer);
begin
{$IFDEF SUPPORT_UINT64}
  if Bit > 63 then
    UInt64ClearBit(TUInt64(N.Hi64), Bit - 64)
  else
    UInt64ClearBit(TUInt64(N.Lo64), Bit);
{$ELSE}
  if Bit > 63 then
    UInt64ClearBit(N.Hi64, Bit - 64)
  else
    UInt64ClearBit(N.Lo64, Bit);
{$ENDIF}
end;

function Int128IsBitSet(var N: TCnInt128; Bit: Integer): Boolean;
begin
  if Bit > 63 then
    Result := GetUInt64BitSet(N.Hi64, Bit - 64)
  else
    Result := GetUInt64BitSet(N.Lo64, Bit);
end;

function Int128IsNegative(var N: TCnInt128): Boolean;
begin
  Result := N.Hi64 < 0;
end;

function Int128Equal(var A, B: TCnInt128): Boolean;
begin
  Result := (A.Lo64 = B.Lo64) and (A.Hi64 = B.Hi64);
end;

function Int128Equal(var A: TCnInt128; B: Int64): Boolean; overload;
begin
  Result := (A.Hi64 = 0) and (A.Lo64 = B);
end;

function Int128Compare(var A, B: TCnInt128): Integer;
var
  R: Integer;
begin
  if A.Hi64 > B.Hi64 then
    Result := 1
  else if A.Hi64 < B.Hi64 then
    Result := -1
  else
  begin
    R := UInt64Compare(A.Lo64, B.Lo64); // 低 64 位须作为无符号数比较
    if A.Hi64 < 0 then // 如果是负值，则变号
      R := -R;

    if R > 0 then
      Result := 1
    else if R < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;

function Int128ToHex(var N: TCnInt128): string;
var
  T, M, Mask: TCnInt128;
  Neg: Boolean;
begin
  if Int128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  Int128Copy(T, N);
  Neg := Int128IsNegative(T);
  if Neg then
    Int128Negate(T);

  Result := '';
  Int128Set(Mask, $F);
  while not Int128IsZero(T) do
  begin
    Int128And(M, T, Mask);
    Int128ShiftRight(T, 4);
    Result := IntToHex(M.Lo64, 1) + Result;
  end;

  if Neg then
    Result := '-' + Result;
end;

{$WARNINGS OFF}

function HexToInt128(const S: string): TCnInt128;
var
  I, K: Integer;
  St, T: TCnInt128;
  Neg: Boolean;
begin
  Int128SetZero(Result);
  Int128Set(St, 16);

  Neg := False;
  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9', 'a'..'f', 'A'..'F'] then
    begin
      Int128Mul(Result, Result, St);
      K := 0;
      if (S[I] >= '0') and (S[I] <= '9') then
        K := Ord(S[I]) - Ord('0')
      else if (S[I] >= 'A') and (S[I] <= 'F') then
        K := Ord(S[I]) - Ord('A') + 10
      else if (S[I] >= 'a') and (S[I] <= 'f') then
        K := Ord(S[I]) - Ord('a') + 10;

      Int128Set(T, K);
      Int128Add(Result, Result, T);
    end
    else if (I = 1) and (S[I] = '-') then
      Neg := True
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;

  if Neg then
    Int128Negate(Result);
end;

function Int128ToStr(var N: TCnInt128): string;
var
  T, Ten, M: TCnInt128;
  Neg: Boolean;
begin
  if Int128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  // 最小的负值下面求相反数会出错，直接处理
  if (N.Hi64 = CN_MIN_INT64) and (N.Lo64 = 0) then
  begin
    Result := SCN_MIN_INT128;
    Exit;
  end;

  Int128Copy(T, N);
  Int128Set(Ten, 10);

  Neg := Int128IsNegative(T);
  if Neg then
    Int128Negate(T); // 注意 T 如果是最小的负值，此处会溢出，所以上面提前处理掉

  Result := '';
  while not Int128IsZero(T) do
  begin
    Int128DivMod(T, Ten, T, M);
    Result := IntToStr(M.Lo64) + Result;
  end;

  if Neg then
    Result := '-' + Result;
end;

function StrToInt128(const S: string): TCnInt128;
var
  I: Integer;
  Ten, T: TCnInt128;
  Neg: Boolean;
begin
  Int128SetZero(Result);
  Int128Set(Ten, 10);

  Neg := False;
  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9'] then
    begin
      Int128Mul(Result, Result, Ten);
      Int128Set(T, Ord(S[I]) - Ord('0'));
      Int128Add(Result, Result, T);
    end
    else if (I = 1) and (S[I] = '-') then
      Neg := True
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;
  if Neg then
    Int128Negate(Result);
end;

{$WARNINGS ON}

// ======================== UInt128 计算函数 ===================================

procedure UInt128Set(var R: TCnUInt128; Lo, Hi: TUInt64);
begin
  R.Lo64 := Lo;
  R.Hi64 := Hi;
end;

procedure UInt128Set(var R: TCnUInt128; Lo: TUInt64);
begin
  R.Lo64 := Lo;
  R.Hi64 := 0;
end;

procedure UInt128Copy(var D, S: TCnUInt128);
begin
  D.Lo64 := S.Lo64;
  D.Hi64 := S.Hi64;
end;

function UInt128IsZero(var N: TCnUInt128): Boolean;
begin
  Result := (N.Lo64 = 0) and (N.Hi64 = 0);
end;

procedure UInt128SetZero(var N: TCnUInt128);
begin
  N.Lo64 := 0;
  N.Hi64 := 0;
end;

{$WARNINGS OFF}

procedure UInt128Add(var R, A, B: TCnUInt128);
var
  C: Integer;
begin
  UInt64Add(R.Lo64, A.Lo64, B.Lo64, C);
  R.Hi64 := A.Hi64 + B.Hi64 + C;
end;

procedure UInt128Add(var R: TCnUInt128; var A: TCnUInt128; V: TUInt64);
var
  C: Integer;
begin
  UInt64Add(R.Lo64, A.Lo64, V, C);
  R.Hi64 := R.Hi64 + C;
end;

// 两个 128 位无符号整数相加，A + B => R，如果有溢出，则溢出的 1 搁进位标记里，否则清零
procedure UInt128AddC(var R: TCnUInt128; A, B: TCnUInt128; out Carry: Integer);
begin
  UInt128Add(R, A, B);
  if UInt128Compare(R, A) < 0 then // 无符号相加，结果只要小于任一个数就说明溢出了
    Carry := 1
  else
    Carry := 0;
end;

procedure UInt128Sub(var R, A, B: TCnUInt128);
var
  C: Integer;
begin
  UInt64Sub(R.Lo64, A.Lo64, B.Lo64, C);
  R.Hi64 := A.Hi64 - B.Hi64 - C;
end;

{$WARNINGS ON}

procedure UInt128Mul(var R, A, B: TCnUInt128; ResHi: PCnUInt128);
var
  R0, R1, R2, R3, Lo, T: TCnUInt128;
  C1, C2: Integer;
begin
  UInt64MulUInt64(A.Lo64, B.Lo64, R0.Lo64, R0.Hi64); //       0       0   | R0.Hi64 R0.Lo64
  UInt64MulUInt64(A.Hi64, B.Lo64, R1.Lo64, R1.Hi64); //       0   R1.Hi64 | R1.Lo64    0
  UInt64MulUInt64(A.Lo64, B.Hi64, R2.Lo64, R2.Hi64); //       0   R2.Hi64 | R2.Lo64    0
  UInt64MulUInt64(A.Hi64, B.Hi64, R3.Lo64, R3.Hi64); //   R3.Hi64 R3.Lo64 |    0       0

  T.Lo64 := 0;
  T.Hi64 := R1.Lo64;
  UInt128AddC(Lo, R0, T, C1);

  T.Hi64 := R2.Lo64;
  UInt128AddC(Lo, Lo, T, C2);

  UInt128Copy(R, Lo); // 低 128 位结果已经拿到了

  if (C1 > 0) or (C2 > 0) or (R1.Hi64 > 0) or (R2.Hi64 > 0) or (R3.Lo64 > 0) or (R3.Hi64 > 0) then
  begin
    // 有溢出，溢出的值要放 ResHi^ 中，如果外界没提供，就抛异常
    if ResHi = nil then
      raise EIntOverflow.Create(SCnErrorUint128MulOverflow);

    T.Hi64 := 0;
    T.Lo64 := R1.Hi64;
    UInt128Add(ResHi^, R3, T);

    T.Lo64 := R2.Hi64;
    UInt128Add(ResHi^, ResHi^, T);

    T.Lo64 := C1 + C2;
    UInt128Add(ResHi^, ResHi^, T); // 加进位，不会再朝上溢出了
  end;
end;

procedure UInt128DivMod(var A, B, R, M: TCnUInt128);
var
  Sft: Integer;
  BB: TCnUInt128;
begin
  if UInt128IsZero(B) then
    raise EDivByZero.Create(SDivByZero);

  if UInt128IsZero(A) then
  begin
    UInt128SetZero(R);
    UInt128SetZero(M);
    Exit;
  end;

  if UInt128Compare(A, B) < 0 then
  begin
    UInt128Copy(M, A);
    UInt128SetZero(R);
    Exit;
  end;

  Sft := 0;
  UInt128Copy(BB, B);  // 用 BB 做中间变量，避免 R M 等可能是 A B 导致修改过程中出错
  UInt128Copy(M, A);   // 修改了 M，要确保后面没动 A B
  UInt128SetZero(R);   // 修改了 R

  // 扩大除数至和被除数最高位相同且比被除数小
  while (UInt128Compare(BB, M) < 0) and not GetUInt64BitSet(BB.Hi64, 63) do
  begin
    if Sft = 127 then
      Break;

    UInt128ShiftLeft(BB, 1);
    Inc(Sft);
    if UInt128Compare(BB, M) > 0 then
    begin
      UInt128ShiftRight(BB, 1);
      Dec(Sft);
      Break;
    end;
  end;

  // 逐步除
  while True do
  begin
    if UInt128Compare(BB, M) <= 0 then // 二进制，只需要减一次，D 无需乘
    begin
      UInt128Sub(M, M, BB);
      UInt128SetBit(R, Sft);

      // 如果此时 M 为 0，貌似可以跳出，都没余数了
      if UInt128IsZero(M) then
        Exit;
    end;

    if Sft > 0 then
    begin
      UInt128ShiftRight(BB, 1);
      Dec(Sft);
    end
    else
      Break;
  end;
end;

procedure UInt128Div(var R, A, B: TCnUInt128);
var
  T: TCnUInt128;
begin
  UInt128DivMod(A, B, R, T);
end;

procedure UInt128Mod(var R, A, B: TCnUInt128);
var
  T: TCnUInt128;
begin
  UInt128DivMod(A, B, T, R);
end;

procedure UInt128ShiftLeft(var N: TCnUInt128; S: Integer);
var
  T, M: TUInt64;
begin
  if S = 0 then
    Exit;

  if S < 0 then
    UInt128ShiftRight(N, -S);

  if S >= 128 then // 全移跑了
  begin
    N.Hi64 := 0;
    N.Lo64 := 0;
  end
  else if S >= 64 then
  begin
    // Lo 为全 0
    N.Hi64 := N.Lo64 shl (S - 64);
    N.Lo64 := 0;
  end
  else
  begin
    // 取出 Lo 的高 S 位
    M := (not TUInt64(0)) shl (64 - S);
    T := N.Lo64 and M;
    T := T shr (64 - S);

    // Lo 和 Hi 都左移 S
    N.Lo64 := N.Lo64 shl S;
    N.Hi64 := N.Hi64 shl S;

    // Lo 左移出的高部分放到 Hi 留出的低部分
    N.Hi64 := N.Hi64 or T;
  end;
end;

procedure UInt128ShiftRight(var N: TCnUInt128; S: Integer);
var
  T, M: TUInt64;
begin
  if S = 0 then
    Exit;

  if S < 0 then
    UInt128ShiftLeft(N, -S);

  if S >= 128 then // 全移跑了
  begin
    N.Hi64 := 0;
    N.Lo64 := 0;
  end
  else if S >= 64 then
  begin
    // Hi 为全 0
    N.Lo64 := N.Hi64 shr (S - 64);
    N.Hi64 := 0;
  end
  else
  begin
    // 取出 Hi 的低 S 位
    M := (not TUInt64(0)) shr (64 - S);
    T := N.Hi64 and M;
    T := T shl (64 - S);

    // Lo 和 Hi 都右移 S
    N.Lo64 := N.Lo64 shr S;
    N.Hi64 := N.Hi64 shr S;

    // Hi 右移出的低部分放到 Lo 留出的高部分
    N.Lo64 := N.Lo64 or T;
  end;
end;

procedure UInt128And(var R, A, B: TCnUInt128);
begin
  R.Lo64 := A.Lo64 and B.Lo64;
  R.Hi64 := A.Hi64 and B.Hi64;
end;

procedure UInt128Or(var R, A, B: TCnUInt128);
begin
  R.Lo64 := A.Lo64 or B.Lo64;
  R.Hi64 := A.Hi64 or B.Hi64;
end;

procedure UInt128Xor(var R, A, B: TCnUInt128);
begin
  R.Lo64 := A.Lo64 xor B.Lo64;
  R.Hi64 := A.Hi64 xor B.Hi64;
end;

procedure UInt128Not(var N: TCnUInt128);
begin
  N.Lo64 := not N.Lo64;
  N.Hi64 := not N.Hi64;
end;

procedure UInt128SetBit(var N: TCnUInt128; Bit: Integer);
begin
  if Bit > 63 then
    UInt64SetBit(N.Hi64, Bit - 64)
  else
    UInt64SetBit(N.Lo64, Bit);
end;

procedure UInt128ClearBit(var N: TCnUInt128; Bit: Integer);
begin
  if Bit > 63 then
    UInt64ClearBit(N.Hi64, Bit - 64)
  else
    UInt64ClearBit(N.Lo64, Bit);
end;

function UInt128IsBitSet(var N: TCnUInt128; Bit: Integer): Boolean;
begin
  if Bit > 63 then
    Result := GetUInt64BitSet(N.Hi64, Bit - 64)
  else
    Result := GetUInt64BitSet(N.Lo64, Bit);
end;

function UInt128Equal(var A, B: TCnUInt128): Boolean;
begin
  Result := (A.Lo64 = B.Lo64) and (A.Hi64 = B.Hi64);
end;

function UInt128Equal(var A: TCnUInt128; B: TUInt64): Boolean;
begin
  Result := (A.Lo64 = B) and (A.Hi64 = 0);
end;

function UInt128Compare(var A, B: TCnUInt128): Integer;
var
  T: Integer;
begin
  T := UInt64Compare(A.Hi64, B.Hi64);
  if T > 0 then
    Result := 1
  else if T < 0 then
    Result := -1
  else
  begin
    T := UInt64Compare(A.Lo64, B.Lo64);
    if T > 0 then
      Result := 1
    else if T < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;

function IsUInt128AddOverflow(var A, B: TCnUInt128): Boolean;
var
  R: TCnUInt128;
begin
  UInt128Add(R, A, B);
  Result := UInt128Compare(R, A) < 0;
end;

function UInt128ToHex(var N: TCnUInt128): string;
var
  T, M, Mask: TCnUInt128;
begin
  if UInt128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  UInt128Copy(T, N);
  Result := '';

  UInt128Set(Mask, $F);
  while not UInt128IsZero(T) do
  begin
    UInt128And(M, T, Mask);
    UInt128ShiftRight(T, 4);
    Result := IntToHex(M.Lo64, 1) + Result;
  end;
end;

{$WARNINGS OFF}

function HexToUInt128(const S: string): TCnUInt128;
var
  I, K: Integer;
  St, T: TCnUInt128;
begin
  UInt128SetZero(Result);
  UInt128Set(St, 16);

  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9', 'a'..'f', 'A'..'F'] then
    begin
      UInt128Mul(Result, Result, St);
      K := 0;
      if (S[I] >= '0') and (S[I] <= '9') then
        K := Ord(S[I]) - Ord('0')
      else if (S[I] >= 'A') and (S[I] <= 'F') then
        K := Ord(S[I]) - Ord('A') + 10
      else if (S[I] >= 'a') and (S[I] <= 'f') then
        K := Ord(S[I]) - Ord('a') + 10;

      UInt128Set(T, K);
      UInt128Add(Result, Result, T);
    end
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;
end;

function UInt128ToStr(var N: TCnUInt128): string;
var
  T, Ten, M: TCnUInt128;
begin
  if UInt128IsZero(N) then
  begin
    Result := '0';
    Exit;
  end;

  UInt128Copy(T, N);
  UInt128Set(Ten, 10);
  Result := '';

  while not UInt128IsZero(T) do
  begin
    UInt128DivMod(T, Ten, T, M);
    Result := IntToStr(M.Lo64) + Result;
  end;
end;

function StrToUInt128(const S: string): TCnUInt128;
var
  I: Integer;
  Ten, T: TCnUInt128;
begin
  UInt128SetZero(Result);
  UInt128Set(Ten, 10);

  for I := 1 to Length(S) do
  begin
    if S[I] in ['0'..'9'] then
    begin
      UInt128Mul(Result, Result, Ten);
      UInt128Set(T, Ord(S[I]) - Ord('0'));
      UInt128Add(Result, Result, T);
    end
    else
      raise EConvertError.CreateFmt(SInvalidInteger, [S[I]]);
  end;
end;

{$WARNINGS ON}

end.
