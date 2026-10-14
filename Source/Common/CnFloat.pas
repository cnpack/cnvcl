{******************************************************************************}
{                                                                              }
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

unit CnFloat;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：浮点数解析与转换单元
* 单元作者：王乾元(wqyfavor@163.com)
* 备    注：该单元实现了单精度、双精度、扩展精度浮点数的解析与转换。
*
*           注意 Extended 只有 Win32 下是 10 字节，MacOS/Linux x64 下均是 16 字节，Win64 和 ARM 平台均是 8 字节
*           而且，MacOS64 下的 16 字节扩展精度并非  IEEE 754-2008 中规定的 Quadruple 格式，而是前 10 字节截断，
*           内部结构同 Win32 下的扩展 10 字节。
*
* 开发平台：WinXP + Delphi 2009
* 兼容测试：Delphi 2007，且 Extended 或以上只支持小端模式
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.10.14
*               加入 Single、Double 与 Extended 的无穷大与 NaN 构造函数，兼容 Delphi 5 等
*               Math 单元没有 Infinity 常量的版本
*               10 字节扩展精度浮点数结构 TCnExtendedRec10 由实现段移到接口段并公开，
*               供 CnBigDecimal 等单元共用
*           2023.01.13
*               兼容处理 Win64 下 Extended 是 8 字节 Double 而不是 10 字节扩展精度的问题
*               兼容处理 MacOS64/Linux64 下的 16 字节 Extended（只截断处理前 10 字节）
*           2022.02.17
*               增加 FPC 的编译支持，待测试
*           2021.09.05
*               加入三个将浮点数转换为 UInt64（不支持 UInt64 的以 Int64 代替）的函数
*           2020.11.11
*               加入三个将 UInt64（不支持 UInt64 的以 Int64 代替）转换为浮点数的函数
*           2020.06.24
*               加入六个浮点数解开与拼凑的函数
*           2009.1.12
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, {$IFDEF MSWINDOWS} Windows, {$ENDIF} CnNative;

{
  IEEE 754 规定的三种浮点格式，有效数在低位 0：

  单精度 Single             1 符号位 S，8 位指数 E，23 位有效数 M （隐含第 24 位的 1），共 4 字节 32 位
  双精度 Double             1 符号位 S，11 位指数 E，52 位有效数 M（隐含第 53 位的 1），共 8 字节 64 位
  扩展双精度 Extended       1 符号位 S，15 位指数 E，64 位有效数 M（第 64 位显式 1），共 10 字节 80 位

  IEEE 754-2008 加了
  四倍精度 Quadruple        1 符号位 S，15 位指数 E，112 位有效数 M，共 16 字节 128 位
  八倍精度 Octuple          1 符号位 S，19 位指数 E，236 位有效数 M，共 32 字节 256 位

  其中，符号位 S，0 表示正，1 表示负；E 要减去 127/1023/16383/16383 才是真正指数
        M: 规范化单/双精度的二进制 M 的高位加个 1. 代表有效数，扩展的无需加，自身有 1.
           最终值：有效数（二进制 1.xxxx 的形式）乘以 2 的 E 次方（注意不是 10 的 E 次方！）

  （S 符号位、X 指数、M 有效数字但不代表真实值，因为正规化过了并且可能有隐含小数点）

  格式          字节 1    字节 2    字节 3    字节 4    ...  字节 n（每个字节的右边低位是 0）

  单精度 4      SXXXXXXX  XMMMMMMM  MMMMMMMM  MMMMMMMM
      例：      01000110  00011100  01000000  00000000
                   4   6     1   C     4   0     0   0     （内存因为大小端，可能有全局倒序）浮点数实际值 10000
         分为： 0 10001100 00111000100000000000000
         S：0、X：140（要减去 127 得到真实的 13）、M：原始值 1C4000
         高位加 1 和小数点，得到 100111000100000000000000 为实际有效数字，真实值则是 1.001110001
         浮点数实际值：1.001110001 左移 13 位，得到十六进制 2710.00，小数点后全 0，折算成十进制 10000 符合实际结果

  双精度 8      SXXXXXXX  XXXXMMMM  MMMMMMMM  MMMMMMMM  ...  MMMMMMMM
      例：      01000000  11000011  10001000  0000000000000000000000000000000000000000
                   4   0     C   3     8   8     0   0     （内存因为大小端，可能有全局倒序）浮点数实际值 10000
         分为： 0 10000001100 0011100010000000000000000000000000000000000000000000
         S：0、X：1036（要减去 1023 得到真实的 13）、M：原始值 3 88 00 00 00 00 00
         高位加 1 和小数点得到二进制 10011100010000000000000000000000000000000000000000000 为实际有效数字，真实值则是 1.001110001
         浮点数实际值：1.001110001 左移 13 位，得到十六进制 2710.00，小数点后全 0，折算成十进制 10000 符合实际结果

  扩展双精度 10 SXXXXXXX  XXXXXXXX  1MMMMMMM  MMMMMMMM  ...  MMMMMMMM  // 注意它的有效数字包括了 1，其余都省略了 1
      例：      01000000  00001100  10011100  01000000  ...
                   4   0     0   C     9   C     4   0     （内存因为大小端，可能有全局倒序）浮点数实际值 10000
         分为： 0 100000000001100 1001110000100000 ...
         S：0、X：16396（要减去 16383 得到真实的 13）、M：原始值 9C 40 00 ...
         高位不用额外加 1，只要加小数点得到二进制 10011100 01000000 ... 为实际有效数字，真实值则是 1.001110001
         浮点数实际值：1.001110001 左移 13 位，得到十六进制 2710.00，小数点后全 0，折算成十进制 10000 符合实际结果

  四倍精度 16   SXXXXXXX  XXXXXXXX  MMMMMMMM  MMMMMMMM  ...  MMMMMMMM
  八倍精度 32   SXXXXXXX  XXXXXXXX  XXXXMMMM  MMMMMMMM  ...  MMMMMMMM

  注意：Little Endian 机器上，字节 1 到 n 还会来个倒序。本单元均已倒序处理

  0：全 0
  -0：全 0 但符号位为 1
  正负无穷大：指数全 1，有效数全 0（扩展双精度 10 字节，有效数字的最高位需 1），符号 0 或 1
}

type
  TCnQuadruple = packed record
  {* Delphi 中无四倍精度类型，用结构及其指针代替}
    Lo: TUInt64;
    Hi0: Cardinal;
    case Boolean of
      True:  (Hi1: Cardinal);
      False: (W0, W1: Word);   // 小端机器上，符号和指数都在这个 W1 里
  end;
  PCnQuadruple = ^TCnQuadruple;
  {* 指向四倍精度结构的指针}

  TCnOctuple = packed record
  {* Delphi 中无八倍精度类型，用两个 Int64 及其指针代替，暂无处理函数}
    F0: Int64;
    F1: Int64;
    F2: Int64;
    F3: Int64;
  end;
  PCnOctuple = ^TCnOctuple;
  {* 指向八倍精度结构的指针}

  TCnExtendedRec10 = packed record
  {* 10 字节扩展精度浮点数的内存布局，即 Win32 与 macOS/Linux x86 平台上 Extended 的实际
     格式，也用于截取 16 字节 Extended 的前 10 字节。小端下尾数在低 8 字节，符号位与
     15 位指数域都在 ExpSign 里；大端下布局不同，不适用本结构。}
    Mantissa: TUInt64;
    {* 8 字节尾数，最高位 63 是显式的整数位}
    ExpSign: Word;
    {* 低 15 位为指数域，最高位为符号位}
  end;
  PCnExtendedRec10 = ^TCnExtendedRec10;
  {* 指向 10 字节扩展精度浮点数结构的指针}

  ECnFloatSizeError = class(Exception);
  {* 浮点数长度异常}

  ECnFloatRangeError = class(Exception);
  {* 浮点数范围异常}

  ECnFloatFormatError = class(Exception);
  {* 浮点数文本格式错误异常}

const
  CN_EXTENDED_SIZE_8  =          8;
  {* Win64 下的 Extended 类型的长度，只有 8 字节}

  CN_EXTENDED_SIZE_10 =          10;
  {* Win32 下的 Extended 类型的长度，是标准的 10 字节}

  CN_EXTENDED_SIZE_16 =          16;
  {* MACOS64/Linux64 下的 Extended 类型的长度，是 16 字节}

  CN_SIGN_SINGLE_MASK =          $80000000;
  {* 单精度浮点数的符号位掩码}

  CN_SIGN_DOUBLE_MASK =          $8000000000000000;
  {* 双精度浮点数的符号位掩码}

  CN_SIGN_EXTENDED_MASK =        $8000;
  {* 扩展精度浮点数的符号位掩码，已经刨去了 8 字节有效数字}

  CN_SIGN_QUADRUPLE_MASK =       $80000000;
  {* 四倍精度浮点数的符号位掩码，只针对前四字节，刨去了后面所有内容}

  CN_EXPONENT_SINGLE_MASK =      $7F800000;
  {* 单精度浮点数的指数掩码，还要右移 23 位}

  CN_EXPONENT_DOUBLE_MASK =      $7FF0000000000000;
  {* 双精度浮点数的指数掩码，还要右移 52 位}

  CN_EXPONENT_EXTENDED_MASK =    $7FFF;
  {* 扩展精度浮点数的指数掩码，刨去了 8 字节有效数字}

  CN_EXPONENT_QUADRUPLE_MASK =   $7FFF;
  {* 四倍精度浮点数的指数掩码，刨去了 14 字节有效数字}

  CN_SIGNIFICAND_SINGLE_MASK =   $007FFFFF;
  {* 单精度浮点数的有效数字掩码，低 23 位}

  CN_SIGNIFICAND_DOUBLE_MASK =   $000FFFFFFFFFFFFF;
  {* 双精度浮点数的有效数字掩码，低 52 位}

  CN_SIGNIFICAND_EXTENDED_MASK = $FFFFFFFFFFFFFFFF;
  {* 扩展精度浮点数的有效数字掩码，低 64 位，其实就是全部 8 字节整}

  CN_SIGNIFICAND_QUADRUPLE_MASK = $FFFF;
  {* 四倍精度浮点数的有效数字掩码，只针对前四字节，还有加上后面所有内容}

  CN_SINGLE_NAN_QUIET_MASK = $00400000;
  {* 单精度 NaN 的静默位，即尾数域的最高位。该位为 1 表示是静默 NaN，参与运算时
     只把 NaN 传播下去而不触发无效操作异常；该位为 0 则是信号 NaN，部分平台会报异常}

  CN_DOUBLE_NAN_QUIET_MASK = $0008000000000000;
  {* 双精度 NaN 的静默位，即尾数域的最高位，含义同单精度}

  CN_EXTENDED_NAN_QUIET_MASK = $4000000000000000;
  {* 80 位扩展精度 NaN 的静默位，即尾数域的次高位，其最高位是固定的显式整数位}

  CN_SINGLE_SIGNIFICAND_BITLENGTH         = 23;
  {* 单精度浮点数的有效数字位长度}

  CN_DOUBLE_SIGNIFICAND_BITLENGTH         = 52;
  {* 双精度浮点数的有效数字位长度}

  CN_EXTENDED_SIGNIFICAND_BITLENGTH       = 63;
  {* 扩展精度浮点数的有效数字位长度}

  CN_EXPONENT_OFFSET_SINGLE               = 127;
  {* 单精度浮点数的指数偏移值，实际指数值要加上该值才能存入单精度浮点数的指数区}

  CN_EXPONENT_OFFSET_DOUBLE               = 1023;
  {* 双精度浮点数的指数偏移值，实际指数值要加上该值才能存入双精度浮点数的指数区}

  CN_EXPONENT_OFFSET_EXTENDED             = 16383;
  {* 扩展精度浮点数的指数偏移值，实际指数值要加上该值才能存入扩展精度浮点数的指数区，10 和 16 字节扩展精度浮点数均为该值}

  // 仨 Max 均不包括指数全 1 的情形（那是正负无穷大）
  CN_SINGLE_MIN_EXPONENT                  = -127;
  {* 单精度浮点数的最小指数}

  CN_SINGLE_MAX_EXPONENT                  = 127;
  {* 单精度浮点数的最大指数}

  CN_DOUBLE_MIN_EXPONENT                  = -1023;
  {* 双精度浮点数的最小指数}

  CN_DOUBLE_MAX_EXPONENT                  = 1023;
  {* 双精度浮点数的最大指数}

  CN_EXTENDED_MIN_EXPONENT                = -16383;
  {* 扩展精度浮点数的最小指数}

  CN_EXTENDED_MAX_EXPONENT                = 16383;
  {* 扩展精度浮点数的最大指数}

  CN_SINGLE_MIN_NORMAL_EXPONENT           = CN_SINGLE_MIN_EXPONENT + 1;
  {* 单精度浮点数的最小规格化指数。非规格化数的指数也按约定取该值}

  CN_DOUBLE_MIN_NORMAL_EXPONENT           = CN_DOUBLE_MIN_EXPONENT + 1;
  {* 双精度浮点数的最小规格化指数。非规格化数的指数也按约定取该值}

  CN_EXTENDED_MIN_NORMAL_EXPONENT         = CN_EXTENDED_MIN_EXPONENT + 1;
  {* 扩展精度浮点数的最小规格化指数。非规格化数的指数也按约定取该值}

procedure ExtractFloatSingle(Value: Single; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: Cardinal);
{* 从单精度浮点数中解出符号位、指数、添加了最高位 1 并去除了小数点的完整有效数字。
   重要约定：真值 = (-1)^符号位 × 有效数字 × 2^(指数 - 23)，对全部有限单精度值成立。
   规格化数的有效数字共 24 位且第 23 位为隐含前导 1，此时指数为浮点值中解出来的原始指数。
   非规格化数（含零）没有隐含前导 1，有效数字就是裸小数，指数取最小规格化指数 -126，
   上述公式仍然精确成立。
   无穷与非数不在约定范围内。

   参数：
     Value: Single                        - 待解开的单精度浮点数
     out SignNegative: Boolean            - 符号位，True 为负
     out Exponent: Integer                - 指数
     out Mantissa: Cardinal               - 有效数字

   返回值：（无）
}

procedure ExtractFloatDouble(Value: Double; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64);
{* 从双精度浮点数中解出符号位、指数、添加了最高位 1 并去除了小数点的完整有效数字。
   重要约定：值 = (-1)^符号位 × 有效数字 × 2^(指数 - 52)，对全部有限双精度值成立。
   规格化数的有效数字共 53 位且第 52 位为隐含前导 1，此时指数为浮点值中解出来的原始指数。
   非规格化数（含零）没有隐含前导 1，有效数字就是裸小数，指数取最小规格化指数 -1022，
   上述公式仍然精确成立。
   无穷与非数不在约定范围内。

   参数：
     Value: Double                        - 待解开的双精度浮点数
     out SignNegative: Boolean            - 符号位，True 为负
     out Exponent: Integer                - 指数
     out Mantissa: TUInt64                - 有效数字

   返回值：（无）
}

procedure ExtractFloatExtended(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64); overload;
{* 从扩展精度浮点数中解出符号位、指数、去除了小数点的完整有效数字，支持 8 字节、10 字节、
   以及 16 字节截断为 10 字节的 Extended 格式。该函数实现依赖平台的 Extended 尺寸。
   注意：指数为真实指数；有效数字为全部 64 位，最高位 63 位为自带的 1。

   参数：
     Value: Extended                      - 待解开的扩展精度浮点数
     out SignNegative: Boolean            - 符号位，True 为负
     out Exponent: Integer                - 指数
     out Mantissa: TUInt64                - 有效数字

   返回值：（无）
}

procedure ExtractFloatExtended(ValueAddr: Pointer; ExtendedSize: Integer;
  out SignNegative: Boolean; out Exponent: Integer; out Mantissa: TUInt64); overload;
{* 从不定长度的扩展精度浮点数所在地址中解出符号位、指数、去除了小数点的完整有效数字，支持 8 字节、10 字节、
   以及 16 字节截断为 10 字节的 Extended 格式。该函数实现与本平台的 Extended 尺寸无关。
   注意：指数为真实指数；有效数字为全部 64 位，最高位 63 位为自带的 1。

   参数：
     ValueAddr: Pointer                   - 待解开的扩展精度浮点数所在地址
     ExtendedSize: Integer                - 该扩展精度的大小，只支持 8、10、16 三个值
     out SignNegative: Boolean            - 符号位，True 为负
     out Exponent: Integer                - 指数
     out Mantissa: TUInt64                - 有效数字

   返回值：（无）
}

procedure ExtractFloatQuadruple(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out MantissaLo: TUInt64; out MantissaHi: TUInt64);
{* 从十六字节精度浮点数中解出符号位、指数、去除了小数点的完整有效数字，只在 Extended 为 16 字节
   且格式是 IEEE 754-2008 里的四倍精度浮点时有效（目前 Delphi 不支持该格式）
   注意：指数为真实指数；有效数字 112 位，分为高低两部分，其中原始的为 0~110 位，第 111 位为补上去的 1。

   参数：
     Value: Extended                      - 待解开的十六字节精度浮点数
     out SignNegative: Boolean            - 符号位，True 为负
     out Exponent: Integer                - 指数
     out MantissaLo: TUInt64              - 有效数字低 64 位
     out MantissaHi: TUInt64              - 有效数字高 64 位

   返回值：（无）
}

procedure CombineFloatSingle(SignNegative: Boolean; Exponent: Integer;
  Mantissa: Cardinal; var Value: Single);
{* 把符号位、指数、有效数字拼成单精度浮点数，与 ExtractFloatSingle 互为逆操作。
   重要约定：生成的真值 = (-1)^符号位 × 有效数字 × 2^(指数 - 23)。
   有效数字共 24 位且第 23 位为 1 时表示规格化数（该位即隐含前导 1，写入时去除）；
   第 23 位为 0 时表示非规格化数，整个有效数字就是小数部分，指数应取 -126。
   有效数字为 0 时结果为带符号零。
   在以上约定下，Combine(Extract(V)) 对全部有限单精度值位精确成立。
   无法精确表示（尾数超宽、指数越界导致上溢或下溢）时抛 ECnFloatRangeError。

   参数：
     SignNegative: Boolean                - 符号位，True 为负
     Exponent: Integer                    - 指数
     Mantissa: Cardinal                   - 有效数字，仅低 24 位有效
     var Value: Single                    - 返回组合的单精度浮点数

   返回值：（无）
}

procedure CombineFloatDouble(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Double);
{* 把符号位、指数、有效数字拼成双精度浮点数，与 ExtractFloatDouble 互为逆操作。
   重要约定：生成的真值 = (-1)^符号位 × 有效数字 × 2^(指数 - 52)。
   有效数字共 53 位且第 52 位为 1 时表示规格化数（该位即隐含前导 1，写入时去除）；
   第 52 位为 0 时表示非规格化数，整个有效数字就是小数部分，指数应取 -1022。
   有效数字为 0 时结果为带符号零。
   在以上约定下，Combine(Extract(V)) 对全部有限双精度值位精确成立。
   无法精确表示（尾数超宽、指数越界导致上溢或下溢）时抛 ECnFloatRangeError。

   参数：
     SignNegative: Boolean                - 符号位，True 为负
     Exponent: Integer                    - 指数
     Mantissa: TUInt64                    - 有效数字，仅低 53 位有效
     var Value: Double                    - 返回组合的双精度浮点数

   返回值：（无）
}

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Extended); overload;
{* 把符号位、指数、有效数字拼成扩展精度浮点数，支持 10 字节、
   以及 16 字节截断为 10 字节的 Extended 格式。该函数实现依赖平台的 Extended 尺寸。
   要求有效数字为正规化的，也即共 64 位且最高位为 1。
   指数需要在依赖平台尺寸的扩展精度合法范围内，否则抛异常。

   参数：
     SignNegative: Boolean                - 符号位，True 为负
     Exponent: Integer                    - 指数
     Mantissa: TUInt64                    - 有效数字，64 位全有效
     var Value: Extended                  - 返回组合的扩展精度浮点数

   返回值：（无）
}

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; ValueAddr: Pointer; ExtendedSize: Integer); overload;
{* 把符号位、指数、有效数字拼成扩展精度浮点数，支持 10 字节、
   以及 16 字节截断为 10 字节的 Extended 格式。该函数实现与本平台的 Extended 尺寸无关。
   要求有效数字为正规化的，也即共 64 位且最高位为 1。
   指数需要在依赖平台尺寸的扩展精度合法范围内，否则抛异常。

   参数：
     SignNegative: Boolean                - 符号位，True 为负
     Exponent: Integer                    - 指数
     Mantissa: TUInt64                    - 有效数字，64 位全有效
     ValueAddr: Pointer                   - 容纳组合的扩展精度浮点数的地址
     ExtendedSize: Integer                - 该扩展精度的大小，只支持 8、10、16 三个值

   返回值：（无）
}

procedure CombineFloatQuadruple(SignNegative: Boolean; Exponent: Integer;
  MantissaLo: TUInt64; MantissaHi: TUInt64; var Value: Extended);
{* 把符号位、指数、有效数字拼成四倍精度浮点数，只在 Extended 为 16 字节
   且格式是 IEEE 754-2008 里的四倍精度浮点时有效（目前 Delphi 不支持该格式）。
   要求有效数字为正规化的，也即共 112 位且最高位为 1。
   指数需要在依赖平台尺寸的扩展精度合法范围内，否则抛异常。

   参数：
     SignNegative: Boolean                - 符号位，True 为负
     Exponent: Integer                    - 指数
     MantissaLo: TUInt64                  - 有效数字低 64 位，64 位全有效
     MantissaHi: TUInt64                  - 有效数字高 64 位，仅低 48 位有效
     var Value: Extended                  - 返回组合的十六字节精度浮点数

   返回值：（无）
}

function UInt64ToSingle(U: TUInt64): Single;
{* 把用 Int64 有符号整型模拟的 64 位无符号整型赋值给 Single，仨函数实现相同。

   参数：
     U: TUInt64                           - 待赋值的 64 位无符号整型值

   返回值：Single                         - 返回的单精度浮点数
}

function UInt64ToDouble(U: TUInt64): Double;
{* 把用 Int64 有符号整型模拟的 64 位无符号整型赋值给 Double，仨函数实现相同。

   参数：
     U: TUInt64                           - 待赋值的 64 位无符号整型值

   返回值：Double                         - 返回的双精度浮点数
}

function UInt64ToExtended(U: TUInt64): Extended;
{* 把用 Int64 有符号整型模拟的 64 位无符号整型赋值给 Extended，仨函数实现相同。

   参数：
     U: TUInt64                           - 待赋值的 64 位无符号整型值

   返回值：Extended                       - 返回的扩展精度浮点数
}

function SingleToUInt64(F: Single): TUInt64;
{* 把 Single 赋值给用 Int64 有符号整型模拟的 64 位无符号整型，仨函数实现相同。

   参数：
     F: Single                            - 待赋值的单精度浮点数

   返回值：TUInt64                        - 返回的 64 位无符号整型值
}

function DoubleToUInt64(F: Double): TUInt64;
{* 把 Double 赋值给用 Int64 有符号整型模拟的 64 位无符号整型，仨函数实现相同。

   参数：
     F: Double                            - 待赋值的双精度浮点数

   返回值：TUInt64                        - 返回的 64 位无符号整型值
}

function ExtendedToUInt64(F: Extended): TUInt64;
{* 把 Extended 赋值给用 Int64 有符号整型模拟的 64 位无符号整型，仨函数实现相同。

   参数：
     F: Extended                          - 待赋值的扩展精度浮点数

   返回值：TUInt64                        - 返回的 64 位无符号整型值
}

function SingleInfinity(Negative: Boolean = False): Single;
{* 构造单精度浮点数的正或负无穷大。老版本 Delphi（如 Delphi 5）的 Math 单元并没有
   Infinity 与 NegInfinity 常量，而用 1.0 / 0.0 的方式又会触发无效操作检测并抛出
   EInvalidOp 异常，因此直接按 IEEE 754 的位模式构造：指数域全 1、尾数域全 0，
   符号位为 1 时得到负无穷。判定请用同单元的 SingleIsInfinite。

   参数：
     Negative: Boolean                     - 是否取负无穷，False 为正无穷

   返回值：Single                          - 返回构造出的正或负无穷大
}

function DoubleInfinity(Negative: Boolean = False): Double;
{* 构造双精度浮点数的正或负无穷大，按 IEEE 754 的位模式构造：指数域全 1、尾数域全 0。
   判定请用同单元的 DoubleIsInfinite。

   参数：
     Negative: Boolean                     - 是否取负无穷，False 为正无穷

   返回值：Double                          - 返回构造出的正或负无穷大
}

function ExtendedInfinity(Negative: Boolean = False): Extended;
{* 构造扩展精度浮点数的正或负无穷大。Win32 与 macOS/Linux x86 平台上 Extended 是
   10 字节扩展精度（macOS/Linux x86_64 的 16 字节格式截取前 10 字节），其无穷为
   "伪无穷"，即指数域全 1 且尾数的显式整数位为 1；Win64 与 ARM 平台上 Extended 实为
   8 字节 Double，按双精度构造。判定请用同单元的 ExtendedIsInfinite。

   参数：
     Negative: Boolean                     - 是否取负无穷，False 为正无穷

   返回值：Extended                        - 返回构造出的正或负无穷大
}

function SingleNan: Single;
{* 构造单精度浮点数的静默 NaN（Quiet NaN），即指数域全 1、尾数域最高位的
   静默位为 1、其余尾数位为 0。请用同单元的 SingleIsNan 判定，不要用 S = S 之类的
   比较判断，部分平台会因此触发无效操作异常。

   参数：（无）

   返回值：Single                          - 返回构造出的静默 NaN
}

function DoubleNan: Double;
{* 构造双精度浮点数的静默 NaN（Quiet NaN），即指数域全 1、尾数域最高位的
   静默位为 1、其余尾数位为 0。请用同单元的 DoubleIsNan 判定。

   参数：（无）

   返回值：Double                          - 返回构造出的静默 NaN
}

function ExtendedNan: Extended;
{* 构造扩展精度浮点数的静默 NaN。10 字节与 16 字节平台上为"伪 NaN"，即指数域全 1
   且尾数含显式整数位与静默位；Win64 与 ARM 平台上 Extended 实为 8 字节 Double，
   按双精度构造。请用同单元的 ExtendedIsNan 判定。

   参数：（无）

   返回值：Extended                        - 返回构造出的静默 NaN
}

function SingleIsInfinite(AValue: Single): Boolean;
{* 单精度浮点数是否无穷大。

   参数：
     AValue: Single                       - 待判断的单精度浮点数

   返回值：Boolean                        - 返回是否无穷大
}

function DoubleIsInfinite(AValue: Double): Boolean;
{* 双精度浮点数是否无穷大。

   参数：
     AValue: Double                       - 待判断的双精度浮点数

   返回值：Boolean                        - 返回是否无穷大
}

function ExtendedIsInfinite(AValue: Extended): Boolean;
{* 扩展精度浮点数是否无穷大。

   参数：
     AValue: Extended                     - 待判断的扩展精度浮点数

   返回值：Boolean                        - 返回是否无穷大
}

function SingleIsNan(AValue: Single): Boolean;
{* 单精度浮点数是否非实数。

   参数：
     AValue: Single                       - 待判断的单精度浮点数

   返回值：Boolean                        - 返回是否非实数
}

function DoubleIsNan(AValue: Double): Boolean;
{* 双精度浮点数是否非实数。

   参数：
     AValue: Double                       - 待判断的双精度浮点数

   返回值：Boolean                        - 返回是否非实数
}

function ExtendedIsNan(AValue: Extended): Boolean;
{* 扩展精度浮点数是否非实数。

   参数：
     AValue: Extended                     - 待判断的扩展精度浮点数

   返回值：Boolean                        - 返回是否非实数
}

function ExtendedToStr(AValue: Extended): string;
{* 将扩展精度浮点数转换为不变格式字符串，支持其最大的精度。
   Delphi 默认 15 位小数，本函数增大到 18，也即支持 1234567899876543.21。
   返回结果固定使用 '.' 作为小数点，不受当前区域设置影响。

   参数：
     AValue: Extended                     - 待判断的扩展精度浮点数

   返回值：string                         - 返回转换结果
}

function StrToExtended(const S: string): Extended;
{* 将不变格式的浮点数字符串转换为扩展精度浮点数。
   输入格式使用固定的 '.' 作为小数点，不受当前区域设置影响；不接受
   区域设置使用的逗号小数点。转换要求输入被完整消费，非法格式将引发
   ECnFloatFormatError 异常。

   参数：
     S: string                             - 待转换的不变格式浮点数字符串

   返回值：Extended                        - 返回转换结果
}

// FPC、Windows 64/Linux 64 等平台以及 Delphi 5、6 不支持以下三个函数
{$IFDEF WIN32}
{$IFDEF COMPILER7_UP}
{
  此处实现了三个将 Extended 类型转换为二、八、十六进制字符串的函数。
  算法是读取 Extended 类型在内存中的二进制内容进行转换。关于 Extended 类型的说明
  可以参考其他资料。Double 与 Single 类型为系统通用支持的浮点类型，与 Delphi 特有的
  Extended 在存储形式上稍有不同。三者均将尾数规格化，但 Double 与 Single 尾数部分略
  掉了默认的 1。比如尾数二进制内容为 1.001，则在 Double 与 Single 中存储为 001，略去
  小数点前的 1，而在 Extended 里存储为 1001。
  NaN 意为 "not a number"，不是个数，定义参看 Math.pas 单元中的常量 NaN
  Infinity 为无穷大，定义参看 Math.pas 单元中的常量 Infinity 与 NegInfinity.
  解释一下 DecimalExp 与 AlwaysUseExponent 参数。
  将十进制浮点数度转换成其他进制时，如果用指数形式（科学计算法）表达（有些情况
  也只能用指数形式，比如 1E-1000，不用指数时是 0.0000000...0001，转换后指数部分
  也应该用相应进制表示。但有时可以仍用十进制表示指数部分，比如二进制串
  1.001E101，真值为 100100，将指数用十进制表达更清楚一些 1.001D5，表示将小数点
  右移 5 位。DecimalExp 这个参数就是指定是否用十进制表达指数部分的。注意，用十进制
  数表示指数并无规定表达法，程序中使用 "D" 来表示，"E" 为用相应进制表示。另外，由于
  十六进制比较特殊，"D" 与 "E" 均为十六进制特殊字符，所以十六进制表达时使用了 "^"
  字符，输出样例 3.BD^D(12)、A.BD^E(ABCE)。如不喜欢这种格式可以自行修改。
  AlwaysUseExponent 参数指定是否一定用科学读数法表达，比如 100.111 位数比较少，
  程序自动判断不需要使用科学计数法，当 AlwaysUseExponent 为真时则一定表达为指数
  形式 1.00111E2。
  const
    MaxBinDigits = 120;
    MaxHexDigits = 30;
    MaxOctDigits = 40;
  这三个常量指定最长能输出多少位，当结果超过这个数时，则一定使用科学计数法。
}

{ FloatDecimalToBinExtended, FloatDecimalToOctExtended，FloatDecimalToHexExtended
  均调用了 FloatDecimalToBinaryExtended 过程，FloatDecimalToBinaryExtended 不公开。}

function FloatDecimalToBinExtended(fIn: Extended; DecimalExp: Boolean;
  AlwaysUseExponent: Boolean): AnsiString; deprecated; // Convert to binary

function FloatDecimalToOctExtended(fIn: Extended; DecimalExp: Boolean;
  AlwaysUseExponent: Boolean): AnsiString; deprecated; // Convert to octal

function FloatDecimalToHexExtended(fIn: Extended; DecimalExp: Boolean;
  AlwaysUseExponent: Boolean): AnsiString; deprecated; // Convert to hexdecimal

{$ENDIF}
{$ENDIF}

implementation

const
  UINT64_EXTENDED_EXP_MAX = $4040; // UINT64 最大整数对应 Extended 浮点的最大指数

resourcestring
  SCnErrorExtendedSizeFmt = 'Extended Size Error %d';
  SCnErrorExtendedInvalidFormat = 'Invalid Invariant Extended Value';
  SCnErrorFloatExponentFmt = 'Invalid Exponent %d';

{$IFDEF WIN32}
{$IFDEF COMPILER7_UP}

type
  PConvertFloatSystem = ^TConvertFloatSystem;
  TConvertFloatSystem = record
    Negative: Boolean;
    ExpFlag, ExponentI: Integer;
  end;

const
  MaxBinDigits = 120;
  MaxHexDigits = 30;
  MaxOctDigits = 40;

function FloatDecimalToBinaryExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean; var ForHexOct: PConvertFloatSystem): AnsiString;
var
  Neg: Boolean;
  i, Flag, IntExp: Integer;
  Exp: AnsiString;
label UseExponent;
begin
{
Extended(32.125) in memory:
0   100000000000100  10000000 10000000 00000000 00000000 00000000 00000000 00000000 00000000
    9      8      7      6      5      4      3   2nd Byte  1stByte   0
sign exponent      digits
0 111111111111111 1000000000000000000000000000000000000000000000000000000000000000  + Inf
1 111111111111111 1000000000000000000000000000000000000000000000000000000000000000  - Inf
1 111111111111111 1100000000000000000000000000000000000000000000000000000000000000  Nan
0 111111111111111 1100000000000000000000000000000000000000000000000000000000000000  -Nan
}
  SetLength(Result, 255);
  SetLength(Exp, 2 * SizeOf(Extended) + 1);
  Neg := False;
  asm
    push EBX
    push ESI
    mov EBX, Result // Address of Result
    mov EBX, [EBX]
    mov EAX, 0
    // Test if fIN equals 0
    lea ESI, fIn[7] // get the first byte of digits
    mov AL, [ESI]
    test AL, 128 // 10000000B
    jz @Zero
    mov ECX, 0
    lea ESI, fIn[8]
    mov AX, [ESI]  // Get first two bytes
    test AX, 32768  // 32768D = 1000000000000000B
    jz @Positive
    mov Neg, 1
    sub AX, 32768 // Sign bit <- 0
  @Positive:
    // Test if fIn is NaN or Infinity
    cmp AX, 32767
    jnz @NotNAN_INF
    mov DL, [ESI - 1]
    test DL, 64  // 01000000B
    jz @INF
    mov Flag, 4  // NaN
    jmp @Done
  @INF:
    mov Flag, 3  // INF
    jmp @Done
  @NotNAN_INF:
    sub AX, 16383 // AX = AX - 011111111111111B
    jns @ExpPositive
    sub AX, 1
    not AX
    mov Flag, 2 // // Exponent sign negative
    jmp @JudgeDecimalExp
  @ExpPositive:
    mov Flag, 1 // Exponent sign positive
  @JudgeDecimalExp:
    mov IntExp, EAX
    cmp DecimalExp, 1
    je @MoveDigits
    // Binary string exponent. Convert AX to binary string and store it in Exp
    lea EBX, Exp
    mov EBX, [EBX]
    push ECX
    mov [EBX], 69 // 'E' // "D" for decimal exponent
    mov ECX, 1
    cmp Flag, 2
    jnz @NoNegativeInExp
    mov [EBX + 1], 45 // '-' // Add a "-" to exponent string
    mov ECX, 2
  @NoNegativeInExp:
    mov ESI, 0 // flag whehter "1" appears
    // Move exponent digits to Exp
    mov DX, 32768 // 1000000000000000
  @NextExpDigit:
    test AX, DX
    jz @AppendExp0
    mov [EBX + ECX], 49 // '1'
    mov ESI, 1
    jmp @NextExpIncECX
  @AppendExp0:
    cmp ESI, 0
    jz @NextExpNoIncECX // do not append this "0"
    mov [EBX + ECX], 48 // '0'
  @NextExpIncECX:
    inc ECX
  @NextExpNoIncECX:
    shr DX, 1
    cmp DX, 0
    jne @NextExpDigit
    pop ECX
    mov EBX, Result
    mov EBX, [EBX]
    jmp @MoveDigits
  @MoveDigits:
    // Move digits to Result
    mov ESI, 8
  @NextByte:
    dec ESI
    mov EAX, EBX
    lea EBX, fIn[ESI]
    mov DL, [EBX]
    mov EBX, EAX
    mov AL, 128 // 10000000
  @NextDigit:
    test DL, AL
    jz @Append0
    mov [EBX + ECX], 49 // '1'
    mov i, ECX
    jmp @Next
  @Append0:
    mov [EBX + ECX], 48 // '0'
  @Next:
    inc ECX
    shr AL, 1
    cmp AL, 0
    jne @NextDigit
    cmp ESI, 0 // if the last byte
    jne @NextByte
    jmp @Done
  @Zero:
    mov Flag, 0
  @Done:
    pop ESI
    pop EBX
  end;
  case Flag of
    0:
    begin
      ForHexOct := nil;
      Result := '0';
      Exit;
    end;
    1, 2:
    begin
      // Delete redundant "0" in Result
      Delete(Result, i + 2, MaxInt); // i stores the position of the last 1 in Result
      if Assigned(ForHexOct) then
      begin
        // Copy to ForHexOct
        with ForHexOct^ do
        begin
          Negative := Neg;
          ExpFlag := Flag;
          ExponentI := IntExp;
        end;
        Exit;
      end;
      // Add dot and exponent to Result
      if (IntExp = 0) then
      begin
        if (Length(Result) > 1) then
          Insert('.', Result, 2);
      end
      else
      begin
        { Decide whether use exponent. For example "1000.101" shouldn't be output
          as 1.000101E11 when AlwaysUseExponent is False. }
        if AlwaysUseExponent then
        begin
UseExponent:
          if DecimalExp then
            if Flag = 1 then
              Exp := 'D' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(IntExp))
            else
              Exp := 'D-' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(IntExp));
          if Length(Result) >=2 then
            Insert('.', Result, 2);
          Result := Result + Exp;
        end
        else
        begin
          // IntExp may be negative.
          if Flag = 1 then
          begin
            // Calculate all digits required without exponent
            if IntExp <= Length(Result) - 2 then
            begin
              // Do not use exponent
              Insert('.', Result, IntExp + 2);
            end
            else if IntExp = Length(Result) - 1 then
              { 1.001, Exp = 3, output 1001  }
            else
            begin
              if IntExp + 1> MaxBinDigits then
                goto UseExponent
              else
              begin
                Inc(IntExp);
                i := Length(Result);
                // Add zeros at tail
                SetLength(Result, IntExp);
                for i := i + 1 to IntExp do
                  Result[i] := '0';
              end;
            end;
          end
          else
          begin
            if IntExp + Length(Result) > MaxBinDigits then
              goto UseExponent
            else
            begin
              // Add leading zeros and place "."
              SetLength(Exp, 1 + IntExp);
              Exp[1] := '0';
              Exp[2] := '.';
              for i := 3 to IntExp + 1 do
                Exp[i] := '0';   //}
              Result := Exp + Result;
            end;
          end;
        end;
      end;
    end;
    3: // INF
    begin
      ForHexOct := nil;
      Result := 'INF';
    end;
    4: // NaN
    begin
      ForHexOct := nil;
      Result := 'NaN';
      Exit;
    end;
  end;
  if Neg then
    Result := '-' + Result;
end;

function FloatDecimalToBinExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
var
  PTmp: PConvertFloatSystem;
begin
  PTmp := nil;
  Result := FloatDecimalToBinaryExtended(fIn, DecimalExp, AlwaysUseExponent, PTmp);
end;

function FloatDecimalToHexExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
const
  DecToHex: array[0..15] of  AnsiChar =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  BinPow: array[0..3] of Integer = (8, 4, 2, 1);

  function IntToHex(Int: Integer): AnsiString;
  var
    k ,t: Integer;
    Buf: array[1..5] of AnsiChar;
  begin
    k := 1;
    while (Int <> 0) do
    begin
      Buf[k] := DecToHex[Int mod 16];
      Inc(k);
      Int := Int div 16;
    end;
    Dec(k);
    SetLength(Result, k);
    t := 1;
    while (k > 0) do
    begin
      Result[t] := Buf[k];
      Inc(t);
      Dec(k);
    end;
  end;

  function ToHex(const S: AnsiString; LeftToDot: Boolean): AnsiString;
  var
    i, l, t, m, k: Integer;
    Buf: array[1..64] of AnsiChar;
  begin
    { LeftToDot = True, S will be patched with zeroes on its left side.
      For example, S = '110', after patching, S = '0110'.
      LeftToDot = False, S will be patched with zeroes on its right side.
      S = '110', after patching, S = '1100'. }
    l := Length(S);
    if LeftToDot then
      t := (4 - (l mod 4)) mod 4
    else
      t := 0;
    i := 1;
    m := 1;
    k := 0;
    while i <= l do
    begin
      k := k + BinPow[t] * (Ord(S[i]) - Ord('0'));
      Inc(t);
      if (t = 4) or (i = l) then
      begin
        Buf[m] := DecToHex[k];
        Inc(m);
        k := 0;
        t := 0;
      end;
      Inc(i);
    end;
    Dec(m);
    SetLength(Result, m);

    while (m > 0) do
    begin
      Result[m] := Buf[m];
      Dec(m);
    end;
  end;

var
  PConvertData: PConvertFloatSystem;
  ConvertData: TConvertFloatSystem;
  tmpS: AnsiString;
  k, t, i, m: Integer;
label UseExponent;
begin
  PConvertData := @ConvertData;
  Result := FloatDecimalToBinaryExtended(fIn, True, True, PConvertData);
  // See FloatDecimalToBinaryExtended, PConvertData is set to nil when result is definite.
  if PConvertData = nil then
    Exit;
  with ConvertData do
  begin
    {  3.BD^D(12)
      A.BD^E(ABCE)
      AB.FFFF }
    k := Length(Result) - 1;
    if AlwaysUseExponent then
    begin
UseExponent:
      { Algorithm:
          X.XXXXXXXX^Y  Shift Count     Exp
          1.00000001^0 = 1.00000001 = 1.01^0  (16)
          1.00000001^1 = 10.0000001 = 2.02^0  (16)
          1.00000001^2 = 100.000001 = 4.04^0  (16)
          1.00000001^3 = 1000.00001 = 8.08^0  (16)
          1.00000001^4 = 1.00000001^100 = 1.01^1  (16)
          1.00000001^5 = 10.0000001^100 = 2.02^1  (16)
          Shift Count = Y mod 4
          Exp = Y div 4
          X.XXXXXXXXX^Y  Y < 0                      Exp
          1.00000001^-1 = 0.100000001 = 1000.00001^-100 = 8.08^-1
          1.00000001^-2 = 0.0100000001 = 100.000001^-100 = 4.04^-1
          1.00000001^-3 = 0.00100000001 = 10.0000001^-100 = 2.02^-1
          1.00000001^-4 = 0.000100000001 = 1.00000001^-100 = 1.01^-1
          1.00000001^-5 = 0.0000100000001 = 1000.00001^-100 = 8.08^-2
          Shift Count = 4 - (Abs(Y) mod 4)
          Exp = -(Abs(Y) div 4 + 1)      }
      if ExpFlag = 1 then
      begin
        t := ExponentI div 4; // Exp
        i := ExponentI mod 4; // Shift Count
      end
      else
      begin
        t := -((ExponentI - 1) div 4 + 1); // Exp
        i := (4 - (ExponentI mod 4)) mod 4; // Shift Count
      end;
      // Get hex digits
      if k < i then
      begin
        // Add extra zeroes
        SetLength(Result, i + 1);
        for m := k + 2 to i + 1 do
          Result[m] := '0';
        Result := ToHex(Result, True);
      end
      else if k = i then
        Result := ToHex(Result, True)
      else
      begin
        tmpS := Copy(Result, 1, i + 1);
        Delete(Result, 1, i + 1);
        Result := ToHex(tmpS, True) + '.' + ToHex(Result, False);
      end;
      if t <> 0 then
      begin
        // Format exponent
        if DecimalExp then
          Result := Result + '^D(' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(t)) + ')'
        else
        begin
          if ExpFlag = 1 then
            Result := Result + '^E(' + IntToHex(t) + ')'
          else // t < 0
            Result := Result + '^E(-' + IntToHex(-t) + ')';
        end;
      end;
    end
    else
    begin
      {  Always remember that Result equals "XXXXXXXX" not "X.XXXXXXX".
        Judge whether to use exponent:
        There are K "X" after '.', K = Length(Result) - 1, no "." in Result originally.
        X.XXXXXXX^Y  (Binary string, ExponentI = Abs(Y))
        case Y >= 0  (Condition: ExpFlag = 2)
          Y <= K:
            Y+1 binary digits on left side of '.', K-Y digits on right side，
            totally requires ((Y+1 - 1) div 4 + 1) + ((K-Y - 1) div 4 + 1) hex digits
          Y > K:
            Y+1 binary digits on left side, totally ((Y+1 - 1) div 4 + 1) hex digits
        case Y<0  (Condition: ExpFlag = 1) 0.XXXX or 0.000XXXX
            One digit '0' on left side and K+1+Abs(Y)-1 digits on right side,
            totally 1 + ((K+1+Abs(Y)-1-1) div 4 + 1) hex digits.
        Compare hdc = hex digit count with MaxHexDigits. If hdc > MaxHexDigits,
        goto UseExponent. }
      if ExponentI = 0 then
      begin
        if (Length(Result) > 1) then
          Result := '1.' + ToHex(Copy(Result, 2, MaxInt), False);
      end
      else
      begin
        if ExpFlag = 1 then
        begin
          if ExponentI < k then
          begin
            // No possible that "ExponentI div 4 + (k - ExponentI - 1) div 4 + 2" > MaxHexDigits
            tmpS := Copy(Result, 1, ExponentI + 1);
            Delete(Result, 1, ExponentI + 1);
            Result := ToHex(tmpS, True) + '.' + ToHex(Result, False);
          end
          else if ExponentI = k then
            // 1.01^2 = 101, no ".", no extra "0".
            Result := ToHex(Result, True)
          else
          begin
            t := ExponentI div 4 + 1;
            if t > MaxHexDigits then
              goto UseExponent
            else
            begin
              // Append "0" after Result
              Inc(ExponentI);
              // Add '0' to Result
              SetLength(Result, ExponentI);
              for t := k + 2{original Length(Result) + 1} to ExponentI do
                Result[t] := '0';
              Result := ToHex(Result, True);
            end;
          end;
        end
        else
        begin
          // ExpFlag = 2, X.XXXXXXX^Y, Y < 0
          t := 2 + (k + ExponentI - 1) div 4; {1 + ((K+1+Abs(Y)-1-1) div 4 + 1)}
          if t > MaxHexDigits then
            goto UseExponent
          else
          begin
            // Add leading zeroes before Result
            SetLength(tmpS, ExponentI - 1); // tmpS stores extra zeroes
            for t := 1 to ExponentI - 1 do
              tmpS[t] := '0';
            Result := '0.' + ToHex(tmpS + Result, False);
          end;
        end;
      end;
    end;
    if Negative then
      Result := '-' + Result;
  end;
end;

function FloatDecimalToOctExtended(fIn: Extended; DecimalExp,
  AlwaysUseExponent: Boolean): AnsiString;
const
  DecToOct: array[0..7] of  AnsiChar =
    ('0', '1', '2', '3', '4', '5', '6', '7');
  BinPow: array[0..2] of Integer = (4, 2, 1);

  function IntToOct(Int: Integer): AnsiString;
  var
    k ,t: Integer;
    Buf: array[1..10] of AnsiChar;
  begin
    k := 1;
    while (Int <> 0) do
    begin
      Buf[k] := DecToOct[Int mod 8];
      Inc(k);
      Int := Int div 8;
    end;
    Dec(k);
    SetLength(Result, k);
    t := 1;
    while (k > 0) do
    begin
      Result[t] := Buf[k];
      Inc(t);
      Dec(k);
    end;
  end;

  function ToOct(const S: AnsiString; LeftToDot: Boolean): AnsiString;
  var
    i, l, t, m, k: Integer;
    Buf: array[1..64] of AnsiChar;
  begin
    { LeftToDot = True, S will be patched with zeroes on its left side.
      For example, S = '110', after patching, S = '0110'.
      LeftToDot = False, S will be patched with zeroes on its right side.
      S = '110', after patching, S = '1100'. }
    l := Length(S);
    if LeftToDot then
      t := (3 - (l mod 3)) mod 3
    else
      t := 0;
    i := 1;
    m := 1;
    k := 0;
    while i <= l do
    begin
      k := k + BinPow[t] * (Ord(S[i]) - Ord('0'));
      Inc(t);
      if (t = 3) or (i = l) then
      begin
        Buf[m] := DecToOct[k];
        Inc(m);
        k := 0;
        t := 0;
      end;
      Inc(i);
    end;
    Dec(m);
    SetLength(Result, m);

    while (m > 0) do
    begin
      Result[m] := Buf[m];
      Dec(m);
    end;
  end;

var
  PConvertData: PConvertFloatSystem;
  ConvertData: TConvertFloatSystem;
  tmpS: AnsiString;
  k, t, i, m: Integer;
label UseExponent;
begin
  PConvertData := @ConvertData;
  Result := FloatDecimalToBinaryExtended(fIn, True, True, PConvertData);
  // See FloatDecimalToBinaryExtended, PConvertData is set to nil when result is definite.
  if PConvertData = nil then
    Exit;
  with ConvertData do
  begin
    {  3.333D12  // 12 is decimal
      2.22E33  // 33 is octal}
    k := Length(Result) - 1;
    if AlwaysUseExponent then
    begin
UseExponent:
      if ExpFlag = 1 then
      begin
        t := ExponentI div 3; // Exp
        i := ExponentI mod 3; // Shift Count
      end
      else
      begin
        t := -((ExponentI - 1) div 3 + 1); // Exp
        i := (3 - (ExponentI mod 3)) mod 3; // Shift Count
      end;
      // Get hex digits
      if k < i then
      begin
        // Add extra zeroes
        SetLength(Result, i + 1);
        for m := k + 2 to i + 1 do
          Result[m] := '0';
        Result := ToOct(Result, True);
      end
      else if k = i then
        Result := ToOct(Result, True)
      else
      begin
        tmpS := Copy(Result, 1, i + 1);
        Delete(Result, 1, i + 1);
        Result := ToOct(tmpS, True) + '.' + ToOct(Result, False);
      end;
      if t <> 0 then
      begin
        // Format exponent
        if DecimalExp then
          Result := Result + 'D' + {$IFDEF UNICODE}AnsiString{$ENDIF}(IntToStr(t))
        else
        begin
          if ExpFlag = 1 then
            Result := Result + 'E' + IntToOct(t)
          else // t < 0
            Result := Result + 'E-' + IntToOct(-t);
        end;
      end;
    end
    else
    begin
      if ExponentI = 0 then
      begin
        if (Length(Result) > 1) then
          Result := '1.' + ToOct(Copy(Result, 2, MaxInt), False);
      end
      else
      begin
        if ExpFlag = 1 then
        begin
          if ExponentI < k then
          begin
            tmpS := Copy(Result, 1, ExponentI + 1);
            Delete(Result, 1, ExponentI + 1);
            Result := ToOct(tmpS, True) + '.' + ToOct(Result, False);
          end
          else if ExponentI = k then
            // 1.01^2 = 101, no ".", no extra "0".
            Result := ToOct(Result, True)
          else
          begin
            t := ExponentI div 3 + 1;
            if t > MaxOctDigits then
              goto UseExponent
            else
            begin
              // Append "0" after Result
              Inc(ExponentI);
              // Add '0' to Result
              SetLength(Result, ExponentI);
              for t := k + 2{original Length(Result) + 1} to ExponentI do
                Result[t] := '0';
              Result := ToOct(Result, True);
            end;
          end;
        end
        else
        begin
          // ExpFlag = 2, X.XXXXXXX^Y, Y < 0
          t := 2 + (k + ExponentI - 1) div 3;
          if t > MaxOctDigits then
            goto UseExponent
          else
          begin
            // Add leading zeroes before Result
            SetLength(tmpS, ExponentI - 1); // tmpS stores extra zeroes
            for t := 1 to ExponentI - 1 do
              tmpS[t] := '0';
            Result := '0.' + ToOct(tmpS + Result, False);
          end;
        end;
      end;
    end;
    if Negative then
      Result := '-' + Result;
  end;
end;

{$ENDIF}
{$ENDIF}

procedure ExtractFloatSingle(Value: Single; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: Cardinal);
begin
  SignNegative := (PCardinal(@Value)^ and CN_SIGN_SINGLE_MASK) <> 0;
  Exponent := ((PCardinal(@Value)^ and CN_EXPONENT_SINGLE_MASK) shr 23) - CN_EXPONENT_OFFSET_SINGLE;
  Mantissa := PCardinal(@Value)^ and CN_SIGNIFICAND_SINGLE_MASK;
  if Exponent > -CN_EXPONENT_OFFSET_SINGLE then
    Mantissa := Mantissa or (1 shl CN_SINGLE_SIGNIFICAND_BITLENGTH) // 规格化，高位再加个 1
  else
    // 非规格化数没有隐含前导 1，指数取最小规格化指数，使统一公式
    // 值 = 有效数字 * 2^(指数 - 23) 仍然精确成立
    Exponent := CN_SINGLE_MIN_NORMAL_EXPONENT;
end;

procedure ExtractFloatDouble(Value: Double; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64);
begin
  SignNegative := (PUInt64(@Value)^ and CN_SIGN_DOUBLE_MASK) <> 0;
  Exponent := ((PUInt64(@Value)^ and CN_EXPONENT_DOUBLE_MASK) shr 52) - CN_EXPONENT_OFFSET_DOUBLE;
  Mantissa := PUInt64(@Value)^ and CN_SIGNIFICAND_DOUBLE_MASK;
  if Exponent > -CN_EXPONENT_OFFSET_DOUBLE then
    Mantissa := Mantissa or (TUInt64(1) shl CN_DOUBLE_SIGNIFICAND_BITLENGTH) // 规格化，高位再加个 1
  else
    // 非规格化数没有隐含前导 1，指数取最小规格化指数，使统一公式
    // 值 = 有效数字 * 2^(指数 - 52) 仍然精确成立
    Exponent := CN_DOUBLE_MIN_NORMAL_EXPONENT;
end;

procedure ExtractFloatExtended(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out Mantissa: TUInt64);
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
  begin
    SignNegative := (PCnExtendedRec10(@Value)^.ExpSign and CN_SIGN_EXTENDED_MASK) <> 0;
    Exponent := (PCnExtendedRec10(@Value)^.ExpSign and CN_EXPONENT_EXTENDED_MASK) - CN_EXPONENT_OFFSET_EXTENDED;
    Mantissa := PCnExtendedRec10(@Value)^.Mantissa; // 有 1，不用加了
  end
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    ExtractFloatDouble(Value, SignNegative, Exponent, Mantissa)
  else
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [SizeOf(Extended)]);
end;

procedure ExtractFloatExtended(ValueAddr: Pointer; ExtendedSize: Integer;
  out SignNegative: Boolean; out Exponent: Integer; out Mantissa: TUInt64);
var
  D: Double;
begin
  if (ExtendedSize = CN_EXTENDED_SIZE_10) or (ExtendedSize = CN_EXTENDED_SIZE_16) then
  begin
    SignNegative := (PCnExtendedRec10(ValueAddr)^.ExpSign and CN_SIGN_EXTENDED_MASK) <> 0;
    Exponent := (PCnExtendedRec10(ValueAddr)^.ExpSign and CN_EXPONENT_EXTENDED_MASK) - CN_EXPONENT_OFFSET_EXTENDED;
    Mantissa := PCnExtendedRec10(ValueAddr)^.Mantissa; // 有 1，不用加了
  end
  else if ExtendedSize = CN_EXTENDED_SIZE_8 then
  begin
    Move(ValueAddr^, D, SizeOf(Double));
    ExtractFloatDouble(D, SignNegative, Exponent, Mantissa);
  end
  else
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [ExtendedSize]);
end;

procedure ExtractFloatQuadruple(Value: Extended; out SignNegative: Boolean;
  out Exponent: Integer; out MantissaLo, MantissaHi: TUInt64);
begin
  if SizeOf(Extended) <> CN_EXTENDED_SIZE_16 then
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [SizeOf(Extended)]);

  SignNegative := (PCnQuadruple(@Value)^.W1 and (CN_SIGN_QUADRUPLE_MASK shr 16)) <> 0;
  Exponent := (PCnQuadruple(@Value)^.W1 and CN_EXPONENT_QUADRUPLE_MASK) - CN_EXPONENT_OFFSET_EXTENDED;

  // Extract 16 Bytes to Mantissas
  MantissaLo := PCnQuadruple(@Value)^.Lo;
  MantissaHi := TUInt64(PCnQuadruple(@Value)^.Hi0) or (TUInt64(PCnQuadruple(@Value)^.W0) shl 32) or (TUInt64(1) shl 48); // 高位再加个 1
end;

procedure CombineFloatSingle(SignNegative: Boolean; Exponent: Integer;
  Mantissa: Cardinal; var Value: Single);
var
  Frac: Cardinal;
  Shift: Integer;
begin
  if (Exponent < CN_SINGLE_MIN_EXPONENT) or (Exponent > CN_SINGLE_MAX_EXPONENT) then
    raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);

  if Mantissa = 0 then
  begin
    // 零
    PCardinal(@Value)^ := 0;
    if SignNegative then
      PCardinal(@Value)^ := CN_SIGN_SINGLE_MASK;
    Exit;
  end;

  if (Mantissa and Cardinal(1 shl CN_SINGLE_SIGNIFICAND_BITLENGTH)) = 0 then
  begin
    // 非规格化数：无隐含前导 1，整个有效数字就是小数部分，指数域为 0
    if Mantissa > CN_SIGNIFICAND_SINGLE_MASK then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    PCardinal(@Value)^ := Mantissa;
  end
  else if Exponent >= CN_SINGLE_MIN_NORMAL_EXPONENT then
  begin
    // 规格化数：去掉第 23 位的隐含前导 1 后作为小数域，指数域为 Exponent + 127
    PCardinal(@Value)^ := (Mantissa and CN_SIGNIFICAND_SINGLE_MASK)
      or (Cardinal(Exponent + CN_EXPONENT_OFFSET_SINGLE) shl 23);
  end
  else
  begin
    // 尾数呈规格化形态但指数低于最小规格化指数，真值落在非规格化区间：
    // 右移变成小数部分，不能整除说明无法精确表示
    Shift := CN_SINGLE_MIN_NORMAL_EXPONENT - Exponent;
    if Shift >= CN_SINGLE_SIGNIFICAND_BITLENGTH then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    Frac := Mantissa shr Shift;
    if (Mantissa and ((Cardinal(1) shl Shift) - 1)) <> 0 then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    if Frac > CN_SIGNIFICAND_SINGLE_MASK then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    PCardinal(@Value)^ := Frac;
  end;

  if SignNegative then
    PCardinal(@Value)^ := PCardinal(@Value)^ or CN_SIGN_SINGLE_MASK
  else
    PCardinal(@Value)^ := PCardinal(@Value)^ and not CN_SIGN_SINGLE_MASK;
end;

procedure CombineFloatDouble(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Double);
var
  Frac: TUInt64;
  Shift: Integer;
begin
  if (Exponent < CN_DOUBLE_MIN_EXPONENT) or (Exponent > CN_DOUBLE_MAX_EXPONENT) then
    raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);

  if Mantissa = 0 then
  begin
    // 零
    PUInt64(@Value)^ := 0;
    if SignNegative then
      PUInt64(@Value)^ := CN_SIGN_DOUBLE_MASK;
    Exit;
  end;

  if (Mantissa and (TUInt64(1) shl CN_DOUBLE_SIGNIFICAND_BITLENGTH)) = 0 then
  begin
    // 非规格化数：无隐含前导 1，整个有效数字就是小数部分，指数域为 0
    if Mantissa > CN_SIGNIFICAND_DOUBLE_MASK then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    PUInt64(@Value)^ := Mantissa;
  end
  else if Exponent >= CN_DOUBLE_MIN_NORMAL_EXPONENT then
  begin
    // 规格化数：去掉第 52 位的隐含前导 1 后作为小数域，指数域为 Exponent + 1023
    PUInt64(@Value)^ := (Mantissa and CN_SIGNIFICAND_DOUBLE_MASK)
      or (TUInt64(Exponent + CN_EXPONENT_OFFSET_DOUBLE) shl 52);
  end
  else
  begin
    // 尾数呈规格化形态但指数低于最小规格化指数，真值落在非规格化区间：
    // 右移变成小数部分，不能整除说明无法精确表示
    Shift := CN_DOUBLE_MIN_NORMAL_EXPONENT - Exponent;
    if Shift >= CN_DOUBLE_SIGNIFICAND_BITLENGTH then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    Frac := Mantissa shr Shift;
    if (Mantissa and ((TUInt64(1) shl Shift) - 1)) <> 0 then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    if Frac > CN_SIGNIFICAND_DOUBLE_MASK then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);
    PUInt64(@Value)^ := Frac;
  end;

  if SignNegative then
    PUInt64(@Value)^ := PUInt64(@Value)^ or CN_SIGN_DOUBLE_MASK
  else
    PUInt64(@Value)^ := PUInt64(@Value)^ and not CN_SIGN_DOUBLE_MASK;
end;

{$HINTS OFF}

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; var Value: Extended);
var
  D: Double;
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
  begin
    if (Exponent < CN_EXTENDED_MIN_EXPONENT) or (Exponent > CN_EXTENDED_MAX_EXPONENT) then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);

    if SizeOf(Extended) = CN_EXTENDED_SIZE_16 then
      FillChar(Value, SizeOf(Extended), 0);

    PCnExtendedRec10(@Value)^.Mantissa := Mantissa;
    Inc(Exponent, CN_EXPONENT_OFFSET_EXTENDED);

    PCnExtendedRec10(@Value)^.ExpSign := Exponent and CN_EXPONENT_EXTENDED_MASK;
    if SignNegative then
      PCnExtendedRec10(@Value)^.ExpSign := PCnExtendedRec10(@Value)^.ExpSign or CN_SIGN_EXTENDED_MASK
    else
      PCnExtendedRec10(@Value)^.ExpSign := PCnExtendedRec10(@Value)^.ExpSign and not CN_SIGN_EXTENDED_MASK;
  end
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
  begin
    CombineFloatDouble(SignNegative, Exponent, Mantissa, D);
    Value := D;
  end
  else
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [SizeOf(Extended)]);
end;

procedure CombineFloatExtended(SignNegative: Boolean; Exponent: Integer;
  Mantissa: TUInt64; ValueAddr: Pointer; ExtendedSize: Integer);
var
  D: Double;
begin
  if (ExtendedSize = CN_EXTENDED_SIZE_10) or (ExtendedSize = CN_EXTENDED_SIZE_16) then
  begin
    if (Exponent < CN_EXTENDED_MIN_EXPONENT) or (Exponent > CN_EXTENDED_MAX_EXPONENT) then
      raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);

    if ExtendedSize = CN_EXTENDED_SIZE_16 then
      FillChar(ValueAddr^, ExtendedSize, 0);

    PCnExtendedRec10(ValueAddr)^.Mantissa := Mantissa;
    Inc(Exponent, CN_EXPONENT_OFFSET_EXTENDED);

    PCnExtendedRec10(ValueAddr)^.ExpSign := Exponent and CN_EXPONENT_EXTENDED_MASK;
    if SignNegative then
      PCnExtendedRec10(ValueAddr)^.ExpSign := PCnExtendedRec10(ValueAddr)^.ExpSign or CN_SIGN_EXTENDED_MASK
    else
      PCnExtendedRec10(ValueAddr)^.ExpSign := PCnExtendedRec10(ValueAddr)^.ExpSign and not CN_SIGN_EXTENDED_MASK;
  end
  else if ExtendedSize = CN_EXTENDED_SIZE_8 then
  begin
    CombineFloatDouble(SignNegative, Exponent, Mantissa, D);
    Move(D, ValueAddr^, SizeOf(Double));
  end
  else
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [ExtendedSize]);
end;

{$HINTS ON}

procedure CombineFloatQuadruple(SignNegative: Boolean; Exponent: Integer;
  MantissaLo, MantissaHi: TUInt64; var Value: Extended);
begin
  if SizeOf(Extended) <> CN_EXTENDED_SIZE_16 then
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [SizeOf(Extended)]);

  if (Exponent < CN_EXTENDED_MIN_EXPONENT) or (Exponent > CN_EXTENDED_MAX_EXPONENT) then
    raise ECnFloatRangeError.CreateFmt(SCnErrorFloatExponentFmt, [Exponent]);

  MantissaHi := MantissaHi and not (TUInt64(1) shl 48); // 去掉 112 位上的 1，如果有的话
  PCnQuadruple(@Value)^.Lo := MantissaLo;
  PCnQuadruple(@Value)^.Hi0 := Cardinal(MantissaHi and $FFFFFFFF);
  PCnQuadruple(@Value)^.Hi1 := (MantissaHi shr 32) and CN_SIGNIFICAND_QUADRUPLE_MASK;

  Inc(Exponent, CN_EXPONENT_OFFSET_EXTENDED);
  PCnQuadruple(@Value)^.W1 := Exponent and CN_EXPONENT_QUADRUPLE_MASK;
  if SignNegative then
    PCnQuadruple(@Value)^.Hi1 := PCnQuadruple(@Value)^.Hi1 or CN_SIGN_QUADRUPLE_MASK
  else
    PCnQuadruple(@Value)^.Hi1 := PCnQuadruple(@Value)^.Hi1 and not CN_SIGN_QUADRUPLE_MASK;
end;

// 将 UInt64 设为浮点数
function UFloat(U: TUInt64): Extended;
{$IFNDEF SUPPORT_UINT64}
var
  L, H: Cardinal;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  Result := U;
{$ELSE}
  if U < 0 then // Int64 小于 0 时，代表的 UInt64 是大于 Int64 的最大值的
  begin
    H := Int64Rec(U).Hi;
    L := Int64Rec(U).Lo;
    Result := Int64(H) * Int64(CN_MAX_UINT16 + 1); // 拆开两步乘
    Result := Result * (CN_MAX_UINT16 + 1);
    Result := Result + L;
  end
  else
    Result := U;
{$ENDIF}
end;

function UInt64ToSingle(U: TUInt64): Single;
begin
  Result := UFloat(U);
end;

function UInt64ToDouble(U: TUInt64): Double;
begin
  Result := UFloat(U);
end;

function UInt64ToExtended(U: TUInt64): Extended;
begin
  Result := UFloat(U);
end;

// 普通 Trunc 浮点数最大只能返回 Int64，本函数返回最大 UInt64
function UTrunc(F: Extended): TUInt64;
var
  T, L: Integer;
  SignNeg: Boolean;
  Exponent: Integer;
  Mantissa: TUInt64;
begin
  // 得到真实指数与 1 开头的有效数字（小数点在 1 后）
  ExtractFloatExtended(F, SignNeg, Exponent, Mantissa);
  if SignNeg and (Mantissa <> 0) then
    raise ERangeError.Create(SRangeError); // 有效负数不支持

  if (Mantissa = 0) and SignNeg then       // 如果是负 0 则变成正 0
    SignNeg := False;

  if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    L := CN_DOUBLE_SIGNIFICAND_BITLENGTH
  else
    L := CN_EXTENDED_SIGNIFICAND_BITLENGTH;

  // 真实值等于 Mantissa 乘以 2 的 Exponent - L 次方，Mantissa 共 L + 1 位有效数字。
  // 指数小于 0 时真实值小于 1，截断为 0，注意此时右移量会达到 L 以上，不能真的去移位
  if Exponent < 0 then
  begin
    Result := 0;
    Exit;
  end;

  if Exponent < L then
  begin
    // 小数点往左移 T 位，丢掉小数部分即得整数部分，T 在 0 到 L 之间，不会达到 64
    T := L - Exponent;
    Result := Mantissa shr T;
  end
  else
  begin
    // 小数点往右移，也就是要左移。8 字节 Extended 时指数最大 1023，远大于 L 的 52，必须支持
    T := Exponent - L;                     // 左移位数，可能为 0
    if T = 0 then
    begin
      Result := Mantissa;
      Exit;
    end;

    if T >= 64 then
      raise ERangeError.Create(SRangeError); // Exponent 太大

    // 左移前先检查最高 T 位，不为 0 说明结果放不进 UInt64
    if (Mantissa shr (64 - T)) <> 0 then
      raise ERangeError.Create(SRangeError); // Exponent 太大

    Result := Mantissa shl T;
  end;
end;

function SingleToUInt64(F: Single): TUInt64;
begin
  Result := UTrunc(F);
end;

function DoubleToUInt64(F: Double): TUInt64;
begin
  Result := UTrunc(F);
end;

function ExtendedToUInt64(F: Extended): TUInt64;
begin
  Result := UTrunc(F);
end;

function SingleInfinity(Negative: Boolean = False): Single;
begin
  PCardinal(@Result)^ := CN_EXPONENT_SINGLE_MASK; // 指数域全 1、尾数域全 0
  if Negative then
    PCardinal(@Result)^ := PCardinal(@Result)^ or CN_SIGN_SINGLE_MASK;
end;

function DoubleInfinity(Negative: Boolean = False): Double;
begin
  PUInt64(@Result)^ := CN_EXPONENT_DOUBLE_MASK; // 指数域全 1、尾数域全 0
  if Negative then
    PUInt64(@Result)^ := PUInt64(@Result)^ or CN_SIGN_DOUBLE_MASK;
end;

{$HINTS OFF}
{$WARNINGS OFF}

function ExtendedInfinity(Negative: Boolean = False): Extended;
var
  P: PCnExtendedRec10;
begin
  if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
  begin
    // Win64 与 ARM 平台上 Extended 实为 8 字节 Double
    PDouble(@Result)^ := DoubleInfinity(Negative);
  end
  else
  begin
    if SizeOf(Extended) = CN_EXTENDED_SIZE_16 then
      FillChar(Result, SizeOf(Extended), 0); // 16 字节格式的低 6 字节是填充，必须先清零

    P := PCnExtendedRec10(@Result);
    P.Mantissa := TUInt64($8000000000000000); // 伪无穷：尾数最高位是显式整数位
    P.ExpSign := CN_EXPONENT_EXTENDED_MASK;   // 指数域全 1
    if Negative then
      P.ExpSign := P.ExpSign or CN_SIGN_EXTENDED_MASK;
  end;
end;

function SingleNan: Single;
begin
  PCardinal(@Result)^ := CN_EXPONENT_SINGLE_MASK or CN_SINGLE_NAN_QUIET_MASK;
end;

function DoubleNan: Double;
begin
  PUInt64(@Result)^ := CN_EXPONENT_DOUBLE_MASK or CN_DOUBLE_NAN_QUIET_MASK;
end;

function ExtendedNan: Extended;
var
  P: PCnExtendedRec10;
begin
  if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    PDouble(@Result)^ := DoubleNan
  else
  begin
    if SizeOf(Extended) = CN_EXTENDED_SIZE_16 then
      FillChar(Result, SizeOf(Extended), 0);

    P := PCnExtendedRec10(@Result);
    P.Mantissa := TUInt64($8000000000000000); // 显式整数位
    P.Mantissa := P.Mantissa or CN_EXTENDED_NAN_QUIET_MASK;
    P.ExpSign := CN_EXPONENT_EXTENDED_MASK;
  end;
end;

{$WARNINGS ON}
{$HINTS ON}

function SingleIsInfinite(AValue: Single): Boolean;
begin
  Result := ((PCardinal(@AValue)^ and $7F800000) = $7F800000) and
            ((PCardinal(@AValue)^ and $007FFFFF) = $00000000);
end;

function DoubleIsInfinite(AValue: Double): Boolean;
begin
  Result := ((PUInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PUInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000);
end;

function ExtendedIsInfinite(AValue: Extended): Boolean;
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
    Result := ((PCnExtendedRec10(@AValue)^.ExpSign and $7FFF) = $7FFF) and
              ((PCnExtendedRec10(@AValue)^.Mantissa) = TUInt64($8000000000000000))
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    Result := DoubleIsInfinite(AValue)
  else
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [SizeOf(Extended)]);
end;

function SingleIsNan(AValue: Single): Boolean;
begin
  Result := ((PCardinal(@AValue)^ and $7F800000)  = $7F800000) and
            ((PCardinal(@AValue)^ and $007FFFFF) <> $00000000);
end;

function DoubleIsNan(AValue: Double): Boolean;
begin
  Result := ((PUInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PUInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;

function ExtendedIsNan(AValue: Extended): Boolean;
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
    Result := ((PCnExtendedRec10(@AValue)^.ExpSign and $7FFF)  = $7FFF) and
              ((PCnExtendedRec10(@AValue)^.Mantissa and $7FFFFFFFFFFFFFFF) <> 0)
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    Result := DoubleIsNan(AValue)
  else
    raise ECnFloatSizeError.CreateFmt(SCnErrorExtendedSizeFmt, [SizeOf(Extended)]);
end;

function ExtendedToStr(AValue: Extended): string;
var
  Buffer: array[0..63] of Char;
  I, L: Integer;
  LDecimalSeparator: Char;
begin
  L := FloatToText(Buffer, AValue, {$IFNDEF FPC} fvExtended, {$ENDIF}
    ffGeneral, 18, 0); // 内部限制了最大 18
  SetString(Result, Buffer, L);

  // FloatToText 在 Delphi 5/6/7 和 FPC 中均可能使用当前区域设置的
  // DecimalSeparator。这里仅规范化返回文本，不修改全局设置，避免影响其他线程。
{$IFDEF FPC}
  LDecimalSeparator := DefaultFormatSettings.DecimalSeparator;
{$ELSE}
  {$IFDEF SUPPORT_GLOBAL_FORMAT_SETTINGS}
  LDecimalSeparator := FormatSettings.DecimalSeparator;
  {$ELSE}
  LDecimalSeparator := DecimalSeparator;
  {$ENDIF}
{$ENDIF}
  if LDecimalSeparator <> '.' then
    for I := 1 to Length(Result) do
      if Result[I] = LDecimalSeparator then
        Result[I] := '.';
end;

function StrToExtended(const S: string): Extended;
var
  E: Integer;
  V: Extended;
begin
  V := 0;
  Val(S, V, E);
  if (S = '') or (E <> 0) then
    raise ECnFloatFormatError.Create(SCnErrorExtendedInvalidFormat);
  Result := V;
end;

end.
