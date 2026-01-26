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

unit CnBigNumber;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：大整数运算实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了大整数 TCnBigNumber 的加减乘除等各种运算及大数池 TCnBigNumberPool。
*           大整数的实现机制一部分参考 OpenSSL 的 C 代码改写而来。大数池默认支持多线程。
*
*           Word 系列操作函数指大整数与 UInt32/UInt64 进行运算。
*           Words 系列操作函数指大整数中间的运算过程。
*
*           大整数实现类 TCnBigNumber 内部用一个 UInt32/UInt64 数组表示大数，
*           编译条件 BN_DATA_USE_64 控制数组元素是 UInt32 还是 UInt64，默认前者。
*           数组越往后越代表大整数的高位，数组元素内部的值依赖于 CPU 的大小端序。
*           因而在小端 CPU 上，大整数对象的值严格等于该数组按字节倒序所表达的数。
*
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：Win32/Win64/MACOS D5~Delphi 最新版。
*           注：D5/D6/CB5/CB6 下曾经遇上编译器 Bug 无法修复，
*           譬如写 Int64(AInt64Var) 这样的强制类型转换时，现在暂时绕过了。
* 本 地 化：该单元无需本地化处理
* 修改记录：2025.04.24 V3.0
*               加入 Lucas U V 序列的计算及 BPSW 素数判定算法
*           2025.04.07 V2.9
*               加入几个列表新方法、乘法阶的计算，调整随机调用为非初始化版本以提速
*           2024.11.05 V2.8
*               加入雅可比符号的计算及梅森素数的判定
*           2024.10.10 V2.7
*               部分函数加入的参数检测，以在调用时参数引用重复时抛异常
*           2023.01.12 V2.6
*               64 位模式下增加用 64 位存储计算的模式，测试中，默认禁用
*           2022.06.04 V2.5
*               增加负模逆元与蒙哥马利约简以及基于此实现的快速模乘算法
*               但在 2048 Bits 范围内似乎比直接乘再模要慢不少，
*               与 UInt64 范围内的省时效果不同，可能耗时在别处
*           2022.04.26 V2.4
*               修改 LongWord 与 Integer 地址转换以支持 MacOS64
*           2021.12.08 V2.3
*               实现与 Extended 扩展精度浮点数相乘除，增加一批对数函数，完善 AKS
*           2021.12.04 V2.2
*               实现与 Extended 扩展精度浮点数互相转换
*           2021.11.29 V2.2
*               实现一个稀疏的大数列表类
*           2021.11.23 V2.1
*               实现生成组合数的大数
*           2021.09.20 V2.0
*               实现大数按位计算
*           2021.09.05 V1.9
*               实现完全幂的判断
*           2021.04.02 V1.8
*               POSIX 64 的 LongWord 是 64 位，迁移
*           2020.07.04 V1.7
*               独立出大数池对象，加入多线程控制
*           2020.06.20 V1.6
*               加入快速乘方函数与十进制位数函数
*           2020.01.16 V1.5
*               优化乘法与 MulMod 的速度，去除汇编代码
*           2019.04.16 V1.4
*               支持 Win32/Win64/MacOS32
*           2017.04.04 V1.3
*               修正几处大数池相关的 Bug，但扩展欧几里得求解法还有问题
*           2016.09.26 V1.2
*               加入素数计算；大数池改成全局方式以提高效率
*           2014.11.05 V1.1
*               大数从结构方式改为对象方式，增加部分方法
*           2014.10.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$UNDEF BN_DATA_USE_64}
{$IFDEF CPU64BITS}
  // {$DEFINE BN_DATA_USE_64}
  // BN_DATA_USE_64 表示在 64 位下，内部使用 64 位元素进行存储计算以提高效率，待测试
  // 如不定义，默认使用 32 位元素，较稳定
{$ENDIF}

uses
  Classes, SysUtils, Math, SysConst, CnNative {$IFDEF MSWINDOWS}, Windows {$ENDIF},
  Contnrs, CnContainers, CnHashMap, CnRandom
  {$IFNDEF COMPILER5}, Types {$ENDIF}
  {$IFDEF BN_DATA_USE_64}, CnInt128 {$ENDIF}
  {$IFDEF UNICODE}, AnsiStrings {$ENDIF};

const
  CN_BN_MILLER_RABIN_DEF_COUNT = 50;
  {* Miller-Rabin 算法的默认测试次数}

type
{$IFDEF SUPPORT_UINT64}
  TUInt64Array = array [0..MaxInt div SizeOf(UInt64) - 1] of UInt64;
  {* UInt64 数组类型}
  PUInt64Array = ^TUInt64Array;
  {* UInt64 数组指针类型}
{$ENDIF}

{$IFDEF BN_DATA_USE_64}
  // 大数内部元素，使用 64 位
  TCnBigNumberElement = UInt64;
  {* 大数内部元素类型，64 位}
  PCnBigNumberElement = PUInt64;
  {* 大数内部元素指针类型，64 位}
  PCnBigNumberElementArray = PUInt64Array;
  {* 大数内部元素数组指针类型，64 位}
{$ELSE}
  // 大数内部元素，使用 32 位
  TCnBigNumberElement = Cardinal;
  {* 大数内部元素类型，32 位}
  PCnBigNumberElement = PCardinal;
  {* 大数内部元素指针类型，32 位}
  PCnBigNumberElementArray = PCnLongWord32Array;
  {* 大数内部元素数组指针类型，32 位}
{$ENDIF}

  ECnBigNumberException = class(Exception);
  {* 大数相关异常}

  TCnBigNumber = class(TObject)
  {* 用来代表一个大数的对象}
  private
{$IFDEF DEBUG}
    FIsFromPool: Boolean;
{$ENDIF}
    function GetDecString: string;
    function GetHexString: string;
    function GetDebugDump: string;
  public
    D: PCnBigNumberElement;
    // 一个 array[0..Top-1] of UInt32/UInt64 数组，元素越往后越代表高位，元素内部依赖 CPU 字节序。
    // 在 x86 这种小端 CPU 上，该大数值严格等于本数组字节倒序所表达的数，
    // 大端则每个元素内部倒序再全部从高到低读字节，才符合可读的要求，具体也没条件测
    // 因而在 ToBinary/FromBinary/SetBinary 时，有个元素间的倒序过程，
    // 另外元素内部的字节在读取写入时使用了拆字节拼接，因而抹平了 CPU 的大小端区别
    // 这样对应的 Binary 内存区域从低地址到高地址每个字节都符合网络或阅读习惯，无论 CPU 的大小端是啥

    Top: Integer;
    // Top 表示数字上限，也即有 Top 个有效 UInt32/UInt64，D[Top - 1] 是最高位有效数所在的 UInt32/UInt64

    DMax: Integer;
    // D 数组已分配的存储上限，单位是 UInt32/UInt64 个，大于或等于 Top，不参与运算

    Neg: Integer;
    // 1 为负，0 为正

    constructor Create; virtual;
    {* 构造函数}

    destructor Destroy; override;
    {* 析构函数}

    procedure Init;
    {* 初始化为全 0，并不为 D 分配内存}

    procedure Clear;
    {* 将自身数据空间填 0，并不释放 D 内存}

    function IsZero: Boolean;
    {* 返回大数是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    function SetZero: Boolean;
    {* 将大数设置为 0，返回是否设置成功。

       参数：
         （无）

       返回值：Boolean                    - 返回是否设置成功
    }

    function IsOne: Boolean;
    {* 返回大数是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    function IsNegOne: Boolean;
    {* 返回大数是否为 -1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 -1
    }

    function SetOne: Boolean;
    {* 将大数设置为 1，返回是否设置成功。

       参数：
         （无）

       返回值：Boolean                    - 返回是否设置成功
    }

    function IsOdd: Boolean;
    {* 返回大数是否为奇数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为奇数
    }

    function IsEven: Boolean;
    {* 返回大数是否为偶数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为偶数
    }

    function GetBitsCount: Integer;
    {* 返回大数有多少个有效 Bits 位。

       参数：
         （无）

       返回值：Integer                    - 返回有效位数
    }

    function GetBytesCount: Integer;
    {* 返回大数有多少个有效 Bytes 字节。

       参数：
         （无）

       返回值：Integer                    - 返回有效字节数
    }

    function GetWordCount: Integer;
    {* 返回大数有多少个有效 UInt32/UInt64 元素。

       参数：
         （无）

       返回值：Integer                    - 返回有效元素数
    }

    function GetTenPrecision: Integer;
    {* 返回大数有多少个十进制位。

       参数：
         （无）

       返回值：Integer                    - 返回十进制位数
    }

    function IsCardinal: Boolean;
    {* 大数是否是一个 32 位无符号整数范围内的数。

       参数：
         （无）

       返回值：Boolean                        - 返回是否在 32 位无符号整数范围内
    }

    function GetCardinal: Cardinal;
    {* 取 32 位无符号整数值，如超界，返回 $FFFFFFFF。

       参数：
         （无）

       返回值：Cardinal                   - 返回 32 位无符号整数
    }

    function SetCardinal(W: Cardinal): Boolean;
    {* 给大数赋 32 位无符号整数值。

       参数：
         W: Cardinal                      - 待赋值的 32 位无符号整数

       返回值：Boolean                    - 返回是否赋值成功
    }

    function GetWord: Cardinal;
    {* 取 32 位无符号整数值，如超界，返回 $FFFFFFFF。

       参数：
         （无）

       返回值：Cardinal                   - 返回 32 位无符号整数
    }

    function SetWord(W: Cardinal): Boolean;
    {* 给大数赋 32 位无符号整数值。

       参数：
         W: Cardinal                      - 待赋值的 32 位无符号整数

       返回值：Boolean                    - 返回是否赋值成功
    }

    function IsInteger: Boolean;
    {* 大数是否是一个 32 位有符号整数范围内的数。

       参数：
         （无）

       返回值：Boolean                        - 返回是否在 32 位有符号整数范围内
    }

    function GetInteger: Integer;
    {* 取 32 位有符号整数值，如超界，返回 $7FFFFFFF。

       参数：
         （无）

       返回值：Integer                    - 返回 32 位有符号整数
    }

    function SetInteger(W: Integer): Boolean;
    {* 给大数赋 32 位有符号整数值。

       参数：
         W: Integer                       - 待赋值的 32 位有符号整数

       返回值：Boolean                    - 返回是否赋值成功
    }

    function IsInt64: Boolean;
    {* 大数是否是一个 64 位有符号整数范围内的数。

       参数：
         （无）

       返回值：Boolean                        - 返回是否在 64 位有符号整数范围内
    }

    function GetInt64: Int64;
    {* 取 64 位有符号整数值，如超界，返回 $7FFFFFFFFFFFFFFF。

       参数：
         （无）

       返回值：Int64                      - 返回 64 位有符号整数
    }

    function SetInt64(W: Int64): Boolean;
    {* 给大数赋 64 位有符号整数值。

       参数：
         W: Int64                         - 待赋值的 64 位有符号整数

       返回值：Boolean                    - 返回是否赋值成功
    }

    function IsUInt64: Boolean;
    {* 大数是否是一个 64 位无符号整数范围内的数。

       参数：
         （无）

       返回值：Boolean                        - 返回是否在 64 位无符号整数范围内
    }

{$IFDEF SUPPORT_UINT64}

    function GetUInt64: UInt64;
    {* 取 64 位无符号整数值，如超界，返回 $FFFFFFFFFFFFFFFF。

       参数：
         （无）

       返回值：UInt64                     - 返回 64 位无符号整数
    }

    function SetUInt64(W: UInt64): Boolean;
    {* 给大数赋 64 位无符号整数值。

       参数：
         W: UInt64                        - 待赋值的 64 位无符号整数

       返回值：Boolean                    - 返回是否赋值成功
    }

{$ENDIF}

    function IsWord(W: TCnBigNumberElement): Boolean;
    {* 大数是否等于指定 UInt32/UInt64 元素。

       参数：
         W: TCnBigNumberElement           - 待比较的元素值

       返回值：Boolean                    - 返回是否相等
    }

    function AddWord(W: TCnBigNumberElement): Boolean;
    {* 大数加上一个 UInt32/UInt64 元素，结果仍放自身中，返回相加是否成功。

       参数：
         W: TCnBigNumberElement           - 加数元素

       返回值：Boolean                    - 返回相加是否成功
    }

    function SubWord(W: TCnBigNumberElement): Boolean;
    {* 大数减去一个 UInt32/UInt64 元素，结果仍放自身中，返回相减是否成功。

       参数：
         W: TCnBigNumberElement           - 减数元素

       返回值：Boolean                    - 返回相减是否成功
    }

    function MulWord(W: TCnBigNumberElement): Boolean;
    {* 大数乘以一个 UInt32/UInt64 元素，结果仍放自身中，返回相乘是否成功。

       参数：
         W: TCnBigNumberElement           - 乘数元素

       返回值：Boolean                    - 返回相乘是否成功
    }

    function ModWord(W: TCnBigNumberElement): TCnBigNumberElement;
    {* 大数对一个 UInt32/UInt64 元素求余，返回余数。

       参数：
         W: TCnBigNumberElement           - 除数元素

       返回值：TCnBigNumberElement        - 返回余数
    }

    function DivWord(W: TCnBigNumberElement): TCnBigNumberElement;
    {* 大数除以一个 UInt32/UInt64 元素，商重新放在自身中，返回余数。

       参数：
         W: TCnBigNumberElement           - 除数元素

       返回值：TCnBigNumberElement        - 返回余数
    }

    function PowerWord(W: Cardinal): Boolean;
    {* 大数乘方，结果重新放在自身中，返回乘方是否成功。

       参数：
         W: Cardinal                      - 乘方的指数

       返回值：Boolean                    - 返回乘方是否成功
    }

    procedure SetNegative(Negative: Boolean);
    {* 设置大数是否负值。

       参数：
         Negative: Boolean                - 设置是否为负值

       返回值：（无）
    }

    function IsNegative: Boolean;
    {* 返回大数是否负值。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为负值
    }

    procedure Negate;
    {* 大数求相反数，也即正负号反号}

    procedure ShiftLeftOne;
    {* 左移 1 位}

    procedure ShiftRightOne;
    {* 右移 1 位}

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

    function ClearBit(N: Integer): Boolean;
    {* 给大数的第 N 个 Bit 置 0，返回成功与否。N 从最低位 0 到最高位 GetBitsCount - 1。

       参数：
         N: Integer                       - 待置 0 的位索引

       返回值：Boolean                    - 返回是否置 0 成功
    }

    function SetBit(N: Integer): Boolean;
    {* 给大数的第 N 个 Bit 置 1，返回成功与否。N 从最低位 0 到最高位 GetBitsCount - 1。

       参数：
         N: Integer                       - 待置 1 的位索引

       返回值：Boolean                    - 返回是否置 1 成功
    }

    function IsBitSet(N: Integer): Boolean;
    {* 返回大数的第 N 个 Bit 是否为 1。N 从最低位 0 到最高位 GetBitsCount - 1。

       参数：
         N: Integer                       - 位索引

       返回值：Boolean                    - 返回是否为 1
    }

    function WordExpand(Words: Integer): TCnBigNumber;
    {* 将大数扩展成支持 Words 个 UInt32/UInt64 元素，成功返回扩展的大数对象本身 Self，失败返回 nil。

       参数：
         Words: Integer                   - 待扩展的元素数

       返回值：TCnBigNumber               - 成功则返回大数对象本身，失败返回 nil
    }

    function ToBinary(const Buf: PAnsiChar; FixedLen: Integer = 0): Integer;
    {* 将大数转换成二进制数据放入 Buf 中，使用符合阅读习惯的网络字节顺序。
       Buf 的长度必须大于等于其 BytesCount，返回 Buf 写入的实际字节长度。

       参数：
         const Buf: PAnsiChar             - 待写入的数据块地址
         FixedLen: Integer                - 指定大数实际字节长度不足时使用的固定字节长度，为 0 时则使用大数实际字节长度

       返回值：Integer                    - 返回实际写入字节长度
    }

    function LoadFromStream(Stream: TStream): Boolean;
    {* 从流中加载大数。

       参数：
         Stream: TStream                  - 待加载的流

       返回值：Boolean                    - 返回是否加载成功
    }

    function SaveToStream(Stream: TStream; FixedLen: Integer): Integer;
    {* 将大数写入流。

       参数：
         Stream: TStream                  - 待写入的流
         FixedLen: Integer                - 指定大数实际字节长度不足时使用的固定字节长度，为 0 时则使用大数实际字节长度

       返回值：Integer                    - 返回实际写入字节长度
    }

    function SetBinary(Buf: PAnsiChar; ByteLen: Integer): Boolean;
    {* 根据一个二进制块给自身赋值，使用符合阅读习惯的网络字节顺序，内部复制其内容。

       参数：
         Buf: PAnsiChar                   - 待赋值的数据块地址
         ByteLen: Integer                 - 待赋值的数据块字节长度

       返回值：Boolean                    - 返回是否赋值成功
    }

    class function FromBinary(Buf: PAnsiChar; ByteLen: Integer): TCnBigNumber;
    {* 根据一个二进制块产生一个新的大数对象，使用符合阅读习惯的网络字节顺序，对象内部复制其内容。

       参数：
         Buf: PAnsiChar                   - 待使用的数据块地址
         ByteLen: Integer                 - 待赋值的数据块字节长度

       返回值：TCnBigNumber               - 返回新建的大数对象
    }

    class function FromBytes(const Buf: TBytes): TCnBigNumber;
    {* 根据一个字节数组转换并产生一个新的大数对象，使用符合阅读习惯的网络字节顺序，对象内部复制其内容。

       参数：
         const Buf: TBytes                - 待转换的字节数组

       返回值：TCnBigNumber               - 返回新建的大数对象
    }

    function ToBytes: TBytes;
    {* 将大数内容转换成字节数组，使用符合阅读习惯的网络字节顺序。

       参数：
         （无）

       返回值：TBytes                     - 返回转换的字节数组
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大数转换成字符串。

       参数：
         （无）

       返回值：string                     - 返回大数字符串
    }

    function GetHashCode: TCnHashCode; {$IFDEF OBJECT_HAS_GETHASHCODE} override; {$ENDIF}
    {* 生成杂凑值。

       参数：
         （无）

       返回值：TCnHashCode                - 返回杂凑值，只 32 位有效
    }

    function ToHex(FixedLen: Integer = 0): string;
    {* 将大数转成十六进制字符串。

       参数：
         FixedLen: Integer                - 指定大数实际字节长度不足时使用的固定字节长度并字符串前面补 00，为 0 时则使用大数实际字节长度

       返回值：string                     - 返回十六进制字符串
    }

    function SetHex(const Buf: AnsiString): Boolean;
    {* 根据一串十六进制字符串给自身赋值。

       参数：
         const Buf: AnsiString            - 待赋值的十六进制字符串

       返回值：Boolean                    - 返回是否赋值成功
    }

    class function FromHex(const Buf: AnsiString): TCnBigNumber;
    {* 根据一串十六进制字符串产生一个新的大数对象。

       参数：
         const Buf: AnsiString            - 待使用的十六进制字符串

       返回值：TCnBigNumber               - 返回新建的大数对象
    }

    function ToBase64: string;
    {* 将大数转成 Base64 字符串。

       参数：
         （无）

       返回值：string                     - 返回 Base64 字符串
    }

    function SetBase64(const Buf: AnsiString): Boolean;
    {* 根据一串 Base64 字符串给自身赋值。

       参数：
         const Buf: AnsiString            - 待赋值的 Base64 字符串

       返回值：Boolean                    - 返回是否赋值成功
    }

    class function FromBase64(const Buf: AnsiString): TCnBigNumber;
    {* 根据一串 Base64 字符串产生一个新的大数对象。

       参数：
         const Buf: AnsiString            - 待使用的 Base64 字符串

       返回值：TCnBigNumber               - 返回新建的大数对象
    }

    function ToDec: string;
    {* 将大数转成十进制字符串。

       参数：
         （无）

       返回值：string                     - 返回十进制字符串
    }

    function SetDec(const Buf: AnsiString): Boolean;
    {* 根据一串十进制字符串给自身赋值。

       参数：
         const Buf: AnsiString            - 待赋值的十六进制字符串

       返回值：Boolean                    - 返回是否赋值成功
    }

    class function FromDec(const Buf: AnsiString): TCnBigNumber;
    {* 根据一串十进制字符串产生一个新的大数对象。

       参数：
         const Buf: AnsiString            - 待使用的十六进制字符串

       返回值：TCnBigNumber               - 返回新建的大数对象
    }

    function IsFloat: Boolean;
    {* 大数是否是一个 Extended 扩展精度浮点范围内的数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否在扩展精度范围内
    }

    function GetFloat: Extended;
    {* 将大数转换为浮点数。

       参数：
         （无）

       返回值：Extended                   - 返回浮点数
    }

    function SetFloat(F: Extended): Boolean;
    {* 根据一浮点数给自身赋值。

       参数：
         F: Extended                      - 待赋值的浮点数

       返回值：Boolean                    - 返回是否赋值成功
    }

    class function FromFloat(F: Extended): TCnBigNumber;
    {* 根据一个浮点数产生一个新的大数对象。

       参数：
         F: Extended                      - 待使用的浮点数

       返回值：TCnBigNumber               - 返回新建的大数对象
    }

    function RawDump(Mem: Pointer = nil): Integer;
    {* Dump 出原始内存内容，返回 Dump 的字节长度。如 Mem 传 nil，只返回所需字节长度。

       参数：
         Mem: Pointer                     - 待输出的数据块地址

       返回值：Integer                    - 返回字节长度
    }

    property DecString: string read GetDecString;
    {* 十进制字符串}
    property HexString: string read GetHexString;
    {* 十六进制字符串}

    property DebugDump: string read GetDebugDump;
    {* 内部输出字符串}
  end;
  PCnBigNumber = ^TCnBigNumber;

  TCnBigNumberList = class(TObjectList)
  {* 容纳大数的对象列表，同时拥有大数对象们}
  private

  protected
    function GetItem(Index: Integer): TCnBigNumber;
    procedure SetItem(Index: Integer; ABigNumber: TCnBigNumber);
  public
    constructor Create; reintroduce;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function Add: TCnBigNumber; overload;
    {* 新增一个大数对象，返回该对象。注意添加后返回的对象已由列表纳入管理，无需也不应手动释放。

       参数：
         （无）

       返回值：TCnBigNumber               - 内部新增的大数对象
    }

    function Add(ABigNumber: TCnBigNumber): Integer; overload;
    {* 添加外部的大数对象，注意添加后该对象由列表纳入管理，无需也不应手动释放。

       参数：
         ABigNumber: TCnBigNumber         - 待添加的大数对象

       返回值：Integer                    - 新增的该大数对象的索引值
    }

    function Add(Num: Integer): TCnBigNumber; overload;
    {* 添加一整数，内部生成大数对象，注意返回的结果已由列表纳入管理，无需也不应手动释放。

       参数：
         Num: Integer                     - 待添加的整数

       返回值：TCnBigNumber               - 新增的该大数对象
    }

    procedure AddList(List: TCnBigNumberList);
    {* 添加一大数列表，也即复制列表内的所有大数对象并添加。

       参数：
         List: TCnBigNumberList           - 待添加的整数

       返回值：（无）
    }

    function Remove(ABigNumber: TCnBigNumber): Integer;
    {* 从列表中删除指定引用的大数对象并释放。

       参数：
         ABigNumber: TCnBigNumber         - 待删除的大数对象

       返回值：Integer                    - 删除的位置索引，无则返回 -1
    }

    function IndexOfValue(ABigNumber: TCnBigNumber): Integer;
    {* 根据大数的值在列表中查找该值对应的位置索引。

       参数：
         ABigNumber: TCnBigNumber         - 待查找的大数值

       返回值：Integer                    - 返回位置索引，无则返回 -1
    }

    procedure Insert(Index: Integer; ABigNumber: TCnBigNumber);
    {* 在第 Index 个位置前插入大数对象，注意插入后无需也不应手动释放。

       参数：
         Index: Integer                   - 待插入的位置索引
         ABigNumber: TCnBigNumber         - 待插入的大数对象

       返回值：（无）
    }

    procedure RemoveDuplicated;
    {* 去重，也就是删除并释放值重复的大数对象，只留一个}

    procedure SumTo(Sum: TCnBigNumber);
    {* 列表内所有数求和。

       参数：
         Sum: TCnBigNumber                - 输出的和

       返回值：（无）
    }

    procedure BigNumberSort;
    {* 列表内大数从小到大排序}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大数列表转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property Items[Index: Integer]: TCnBigNumber read GetItem write SetItem; default;
    {* 大数列表项}
  end;

  TCnBigNumberPool = class(TCnMathObjectPool)
  {* 大数池实现类，允许使用到大数的地方自行创建大数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumber; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigNumber               - 返回的大数对象
    }

    procedure Recycle(Num: TCnBigNumber); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnBigNumber                - 待归还的大数对象

       返回值：（无）
    }
  end;

  TCnExponentBigNumberPair = class(TObject)
  {* 指数与大数的组合类，用于稀疏列表}
  private
    FExponent: Integer;
    FValue: TCnBigNumber;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将指数与大数转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property Exponent: Integer read FExponent write FExponent;
    {* 指数}
    property Value: TCnBigNumber read FValue;
    {* 大数}
  end;

  TCnSparseBigNumberList = class(TObjectList)
  {* 容纳大数与指数的稀疏对象列表，同时拥有 TCnExponentBigNumberPair 对象们，
     内部按 Exponent 从小到大排序}
  private
    function GetItem(Index: Integer): TCnExponentBigNumberPair;
    procedure SetItem(Index: Integer; const Value: TCnExponentBigNumberPair);
    function BinarySearchExponent(AExponent: Integer; var OutIndex: Integer): Boolean;
    {* 二分法查找 AExponent 的位置，找到返回 True，OutIndex 放置对应列表索引位置
      如未找到，OutIndex 则返回插入位置供直接 Insert，MaxInt 时供 Add}

    function InsertByOutIndex(OutIndex: Integer): Integer;
    {* 根据二分法查找失败场合返回的 OutIndex 实施插入，返回插入后的真实 Index}

    function GetSafeValue(Exponent: Integer): TCnBigNumber;
    function GetReadonlyValue(Exponent: Integer): TCnBigNumber;
    procedure SetSafeValue(Exponent: Integer; const Value: TCnBigNumber);
  public
    constructor Create; reintroduce;
    {* 构造函数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将所有元素中的指数与大数转成多行字符串。

       参数：
         （无）

       返回值：string                     - 返回转换的字符串
    }

    function Top: TCnExponentBigNumberPair;
    {* 获得最高次对象。

       参数：
         （无）

       返回值：TCnExponentBigNumberPair   - 返回最高次对象
    }

    function Bottom: TCnExponentBigNumberPair;
    {* 获得最低次对象。

       参数：
         （无）

       返回值：TCnExponentBigNumberPair   - 返回最低次对象
    }

    // 需要取、增、删、改、压等操作
    function AddPair(AExponent: Integer; Num: TCnBigNumber): TCnExponentBigNumberPair;
    {* 添加一个 Pair，内部复制大数

       参数：
         AExponent: Integer               -
         Num: TCnBigNumber                -

       返回值：TCnExponentBigNumberPair   -
    }
    procedure AssignTo(Dest: TCnSparseBigNumberList);
    {* 将内容复制给另外一份列表对象。

       参数：
         Dest: TCnSparseBigNumberList     - 待复制的目标对象

       返回值：（无）
    }

    procedure SetValues(LowToHighList: array of Int64);
    {* 从低次到高次设置值。

       参数：
         LowToHighList: array of Int64    - 待赋值的参数列表

       返回值：（无）
    }

    procedure Compact;
    {* 压缩，也就是删掉所有 0 系数项}
    procedure Negate;
    {* 所有系数求反}

    property SafeValue[Exponent: Integer]: TCnBigNumber read GetSafeValue write SetSafeValue;
    {* 安全的根据参数 Exponent 获取大数的方法，读时如内部查不到，会插入新建值并返回，
       写时如内部查不到，则新建插入指定位置后将 Value 复制入此 BigNumber 对象}

    property ReadonlyValue[Exponent: Integer]: TCnBigNumber read GetReadonlyValue;
    {* 只读的根据参数 Exponent 获取大数的方法，读时如内部查不到，会返回一固定的零值 TCnBigNumber 对象，切勿修改其值}

    property Items[Index: Integer]: TCnExponentBigNumberPair read GetItem write SetItem; default;
    {* 重载的 Items 方法}
  end;

  TCnBigNumberHashMap = class(TCnHashMap)
  {* 存储大数对象的散列表，允许以值为比对内容来查找，而不是对象引用本身}
  private
    FOwnsKey: Boolean;
    FOwnsValue: Boolean;
  protected
    function HashCodeFromObject(Obj: TObject): Integer; override;
    function KeyEqual(Key1: TObject; Key2: TObject
      {$IFNDEF CPU64BITS}; Key132: TObject; Key232: TObject {$ENDIF}): Boolean; override;
    procedure DoFreeNode(Node: TCnHashNode); override;
  public
    constructor Create(AOwnsKey: Boolean; AOwnsValue: Boolean); reintroduce; virtual;
    {* AOwnsKey 为 True 时，Key 作为持有处理，节点删除时会释放这个 Key 对象。
       AOwnsValue 为 True 时，Value 也作为持有处理，节点删除时会释放这个 Value 对象。
       注意：设为 True 时，Key 和 Value 不允许传 Object 外的内容。

       参数：
         AOwnsKey: Boolean                - 是否持有 Key 对象
         AOwnsValue: Boolean              - 是否持有 Value 对象

       返回值：TCnBigNumberHashMap        - 返回创建的对象实例
    }

    function Find(Key: TCnBigNumber): TCnBigNumber;
    {* 查找指定大数对象的值所对应的值。

       参数：
         Key: TCnBigNumber                - 待查找的大数的 Key 值

       返回值：TCnBigNumber               - 返回查找到的 Value 对象引用，无则返回 nil
    }
  end;

function BigNumberNew: TCnBigNumber;
{* 创建一个动态分配的大数对象，等同于 TCnBigNumber.Create。

   参数：
     （无）

   返回值：TCnBigNumber                   - 返回创建的大数实例
}

procedure BigNumberFree(Num: TCnBigNumber);
{* 按需要释放一个由 BigNumerNew 函数创建的大数对象，并按需要释放其 D 对象，
   等同于直接调用 Free。

   参数：
     Num: TCnBigNumber                    - 待释放的大数对象

   返回值：（无）
}

procedure BigNumberInit(Num: TCnBigNumber);
{* 初始化一个大数对象，全为 0，并不分配内部的内存，但不会释放内部已有的内存。

   参数：
     Num: TCnBigNumber                    - 待初始化的大数对象

   返回值：（无）
}

procedure BigNumberClear(Num: TCnBigNumber);
  {* 清除一个大数对象，并将其数据空间填 0，不释放其内部的内存。

   参数：
     Num: TCnBigNumber                    - 待清除的大数对象

   返回值：（无）
}

function BigNumberIsZero(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 返回一个大数对象里的大数是否为 0。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否为 0
}

function BigNumberSetZero(Num: TCnBigNumber): Boolean;
{* 将一个大数对象里的大数设置为 0。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象

   返回值：Boolean                        - 返回是否设置成功
}

function BigNumberIsOne(Num: TCnBigNumber): Boolean;
{* 返回一个大数对象里的大数是否为 1。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否为 1
}

function BigNumberIsNegOne(Num: TCnBigNumber): Boolean;
{* 返回一个大数对象里的大数是否为 -1。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否为 -1
}

function BigNumberSetOne(Num: TCnBigNumber): Boolean;
{* 将一个大数对象里的大数设置为 1。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象

   返回值：Boolean                        - 返回是否设置成功
}

function BigNumberIsOdd(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 返回一个大数对象里的大数是否为奇数。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否为奇数
}

function BigNumberIsEven(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 返回一个大数对象里的大数是否为偶数。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否为偶数
}

function BigNumberGetBitsCount(Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效 Bits 位。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Integer                        - 返回有效位数
}

function BigNumberGetBytesCount(Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效 Bytes 字节。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Integer                        - 返回有效字节数
}

function BigNumberGetWordsCount(Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效 UInt32/UInt64 元素。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Integer                        - 返回有效元素数
}

function BigNumberGetTenPrecision(Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效十进制位数。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Integer                        - 返回十进制位数
}

function BigNumberGetTenPrecision2(Num: TCnBigNumber): Integer;
{* 粗略返回一个大数对象里的大数有多少个有效十进制位数，可能有 1 位误差但较快。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Integer                        - 返回十进制位数
}

function BigNumberGetWord(Num: TCnBigNumber): Cardinal;
{* 取一个大数对象的首值，也就是低 32 位无符号整数值。注意如果大数太大则返回 $FFFFFFFF。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Cardinal                       - 返回 32 位无符号整数
}

function BigNumberSetWord(Num: TCnBigNumber; W: Cardinal): Boolean;
{* 给一个大数对象赋首值，也就是低 32 位无符号整数值。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象
     W: Cardinal                          - 待赋值的 32 位无符号整数

   返回值：Boolean                        - 返回是否设置成功
}

function BigNumberGetInteger(Num: TCnBigNumber): Integer;
{* 取一个大数对象的首值，也就是低 32 位有符号数。注意如果大数太大则返回 $7FFFFFFF。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Integer                        - 返回 32 位有符号整数
}

function BigNumberSetInteger(Num: TCnBigNumber; W: Integer): Boolean;
{* 给一个大数对象赋首值，也就是低 32 位有符号整数。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象
     W: Integer                           - 待赋值的 32 位有符号整数

   返回值：Boolean                        - 返回是否设置成功
}

function BigNumberGetInt64(Num: TCnBigNumber): Int64;
{* 取一个大数对象的首值 Int64，也就是 64 位有符号整数。注意如果大数太大则返回 $7FFFFFFFFFFFFFFF。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Int64                          - 返回 64 位有符号整数
}

function BigNumberSetInt64(Num: TCnBigNumber; W: Int64): Boolean;
{* 给一个大数对象赋首值 Int64，也就是 64 位有符号整数。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象
     W: Int64                             - 待赋值的 64 位有符号整数

   返回值：Boolean                        - 返回是否设置成功
}

function BigNumberGetUInt64UsingInt64(Num: TCnBigNumber): TUInt64;
{* 使用 Int64 取一个大数对象的首值 UInt64，也就是 64 位无符号整数。注意如果大数太大则返回 $FFFFFFFFFFFFFFFF。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：TUInt64                        - 返回 64 位无符号整数
}

function BigNumberSetUInt64UsingInt64(Num: TCnBigNumber; W: TUInt64): Boolean;
{* 使用 Int64 给一个大数对象赋 UInt64 首值，也就是 64 位无符号整数。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象
     W: TUInt64                           - 待赋值的 64 位无符号整数

   返回值：Boolean                        - 返回是否设置成功
}

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(Num: TCnBigNumber): UInt64;
{* 取一个大数对象的首值 UInt64，也就是 64 位无符号整数。注意如果大数太大则返回 $FFFFFFFFFFFFFFFF。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：UInt64                         - 返回 64 位无符号整数
}

function BigNumberSetUInt64(Num: TCnBigNumber; W: UInt64): Boolean;
{* 给一个大数对象赋首值 UInt64，也就是 64 位无符号整数。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象
     W: UInt64                            - 待赋值的 64 位无符号整数

   返回值：Boolean                        - 返回是否设置成功
}

{$ENDIF}

function BigNumberIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* 某大数是否等于指定 UInt32/UInt64 元素。

   参数：
     Num: TCnBigNumber                    - 待比较的大数对象
     W: TCnBigNumberElement               - 待比较的元素值

   返回值：Boolean                        - 返回是否相等
}

function BigNumberAbsIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* 某大数绝对值是否等于指定 UInt32/UInt64 元素。

   参数：
     Num: TCnBigNumber                    - 待比较的大数对象
     W: TCnBigNumberElement               - 待比较的元素值

   返回值：Boolean                        - 返回绝对值是否相等
}

function BigNumberAddWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* 大数加上一个 UInt32/UInt64 元素，结果仍放 Num 中，返回相加是否成功。

   参数：
     Num: TCnBigNumber                    - 待加的大数对象
     W: TCnBigNumberElement               - 加数元素

   返回值：Boolean                        - 返回相加是否成功
}

function BigNumberSubWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* 大数减去一个 UInt32/UInt64 元素，结果仍放 Num 中，返回相减是否成功。

   参数：
     Num: TCnBigNumber                    - 待减的大数对象
     W: TCnBigNumberElement               - 减数元素

   返回值：Boolean                        - 返回相减是否成功
}

function BigNumberMulWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
{* 大数乘以一个 UInt32/UInt64 元素，结果仍放 Num 中，返回相乘是否成功。

   参数：
     Num: TCnBigNumber                    - 待乘的大数对象
     W: TCnBigNumberElement               - 乘数元素

   返回值：Boolean                        - 返回相乘是否成功
}

function BigNumberModWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
{* 大数对一个 UInt32/UInt64 元素求余，返回余数。
   注意在内部 64 位的实现中，W 不能大于 UInt32。32 位内部实现则无限制。

   参数：
     Num: TCnBigNumber                    - 被除数大数对象
     W: TCnBigNumberElement               - 除数元素

   返回值：TCnBigNumberElement            - 返回余数
}

function BigNumberDivWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
{* 大数除以一个 UInt32/UInt64 元素，商重新放在 Num 中，返回余数。

   参数：
     Num: TCnBigNumber                    - 被除数大数对象
     W: TCnBigNumberElement               - 除数元素

   返回值：TCnBigNumberElement            - 余数
}

procedure BigNumberAndWord(Num: TCnBigNumber; W: TCnBigNumberElement);
{* 大数与一个 UInt32/UInt64 元素做按位与，结果仍放 Num 中。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     W: TCnBigNumberElement               - 待按位与的元素

   返回值：（无）
}

procedure BigNumberOrWord(Num: TCnBigNumber; W: TCnBigNumberElement);
{* 大数与一个 UInt32/UInt64 元素做按位或，结果仍放 Num 中。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     W: TCnBigNumberElement               - 待按位或的元素

   返回值：（无）
}

procedure BigNumberXorWord(Num: TCnBigNumber; W: TCnBigNumberElement);
{* 大数与一个 UInt32/UInt64 元素做按位异或，结果仍放 Num 中。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     W: TCnBigNumberElement               - 待按位异或的元素

   返回值：（无）
}

function BigNumberAndWordTo(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
{* 大数与一个 UInt32/UInt64 元素做按位与，返回低 32/64 位结果，大数自身不变。注意或、异或暂不需要。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     W: TCnBigNumberElement               - 待按位与的元素

   返回值：TCnBigNumberElement            - 返回按位与结果
}

procedure BigNumberSetNegative(Num: TCnBigNumber; Negative: Boolean);
{* 给一个大数对象设置是否负值。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象
     Negative: Boolean                    - 是否负值

   返回值：（无）
}

function BigNumberIsNegative(Num: TCnBigNumber): Boolean;
{* 返回一个大数对象是否负值，注意不判断 0，也就是说负 0 也返回 True。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否负值
}

procedure BigNumberNegate(Num: TCnBigNumber);
{* 给一个大数对象设置为其相反数，也就是正负号求反。

   参数：
     Num: TCnBigNumber                    - 待设置的大数对象

   返回值：（无）
}

function BigNumberClearBit(Num: TCnBigNumber; N: Integer): Boolean;
{* 给一个大数对象的第 N 个 Bit 置 0，返回成功与否。N 为 0 时代表二进制最低位。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     N: Integer                           - 待置 0 的位索引

   返回值：Boolean                        - 返回是否置 0 成功
}

function BigNumberKeepLowBits(Num: TCnBigNumber; Count: Integer): Boolean;
{* 给一个大数对象只保留第 0 到 Count - 1 个 Bit 位，高位清零，返回成功与否。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     Count: Integer                       - 待保留的低位数

   返回值：Boolean                        - 返回是否保留成功
}

function BigNumberSetBit(Num: TCnBigNumber; N: Integer): Boolean;
{* 给一个大数对象的第 N 个 Bit 置 1，返回成功与否。N 为 0 时代表二进制最低位。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     N: Integer                           - 待置 1 的位索引

   返回值：Boolean                        - 返回是否置 1 成功
}

function BigNumberIsBitSet(Num: TCnBigNumber; N: Integer): Boolean;
{* 返回一个大数对象的第 N 个 Bit 是否为 1。N 为 0 时代表二进制最低位。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     N: Integer                           - 位索引

   返回值：Boolean                        - 返回是否为 1
}

function BigNumberWordExpand(Num: TCnBigNumber; Words: Integer): TCnBigNumber;
{* 将一个大数对象扩展成支持 Words 个 UInt32/UInt64 元素，成功返回被扩展的大数对象地址，失败返回 nil。

   参数：
     Num: TCnBigNumber                    - 待扩展的大数对象
     Words: Integer                       - 待扩展的元素数

   返回值：TCnBigNumber                   - 成功则返回大数对象本身，失败返回 nil
}

function BigNumberToBinary(Num: TCnBigNumber; Buf: PAnsiChar; FixedLen: Integer = 0): Integer;
{* 将一个大数转换成二进制数据放入 Buf 中，Buf 的长度必须大于等于其 BytesCount，
   返回 Buf 写入的长度，注意不处理正负号。如果 Buf 为 nil，则直接返回所需长度。
   大数长度超过 FixedLen 时按大数实际字节长度写，否则先写字节 0 补齐长度
   注意内部有个元素间倒序的过程，同时元素内也有拆字节的过程，抹平了 CPU 大小端的不同。
   也就是说低内存被写入的是大数内部的高位数据，符合网络或阅读习惯。

   参数：
     Num: TCnBigNumber                    - 待处理的大数对象
     Buf: PAnsiChar                       - 待写入的数据块地址
     FixedLen: Integer                    - 指定大数实际字节长度不足时使用的固定字节长度，为 0 时则使用大数实际字节长度

   返回值：Integer                        - 返回实际写入字节长度
}

function BigNumberFromBinary(Buf: PAnsiChar; ByteLen: Integer): TCnBigNumber;
{* 将一个二进制块转换成大数对象，注意不处理正负号。其结果不用时必须用 BigNumberFree 释放。

   参数：
     Buf: PAnsiChar                       - 待使用的数据块地址
     ByteLen: Integer                     - 待赋值的数据块字节长度

   返回值：TCnBigNumber                   - 返回新建的大数对象
}

function BigNumberReadBinaryFromStream(Num: TCnBigNumber; Stream: TStream): Boolean;
{* 从流中加载大数，不处理正负号，返回是否加载成功。

   参数：
     Num: TCnBigNumber                    - 待处理的大数对象
     Stream: TStream                      - 待加载的流

   返回值：Boolean                        - 返回是否加载成功
}

function BigNumberWriteBinaryToStream(Num: TCnBigNumber; Stream: TStream;
  FixedLen: Integer = 0): Integer;
{* 将一个大数的二进制部分写入流，不处理正负号，返回写入流的长度。
   注意内部有个元素间以及元素内倒序的过程以符合网络或阅读习惯。
   FixedLen 表示大数内容不够 FixedLen 字节长度时高位补足 0 以保证 Stream 中输出固定 FixedLen 字节的长度。
   大数长度超过 FixedLen 时按大数实际字节长度写。

   参数：
     Num: TCnBigNumber                    - 待处理的大数对象
     Stream: TStream                      - 待写入的流
     FixedLen: Integer                    - 指定大数实际字节长度不足时使用的固定字节长度，为 0 时则使用大数实际字节长度

   返回值：Integer                        - 返回实际写入字节长度
}

function BigNumberFromBytes(const Buf: TBytes): TCnBigNumber;
{* 将一个字节数组内容转换成大数对象，字节顺序同 Binary，注意不处理正负号。其结果不用时必须用 BigNumberFree 释放。

   参数：
     Buf: TBytes                          - 待转换的字节数组

   返回值：TCnBigNumber                   - 返回新建的大数对象
}

function BigNumberToBytes(Num: TCnBigNumber; FixedLen: Integer = 0): TBytes;
{* 将一个大数转换成二进制数据写入字节数组并返回，字节顺序同 Binary，不处理正负号，失败返回 nil。

   参数：
     Num: TCnBigNumber                    - 待转换的大数对象
     FixedLen: Integer                    - 指定大数实际字节长度不足时使用的固定字节长度，为 0 时则使用大数实际字节长度

   返回值：TBytes                         - 返回字节数组
}

function BigNumberSetBinary(Buf: PAnsiChar; ByteLen: Integer; Res: TCnBigNumber): Boolean;
{* 将一个二进制块赋值给指定大数对象，注意不处理正负号，内部采用复制。
   注意内部有个元素间倒序以及逐个字节由低到高拼成一个元素的过程，以符合网络或阅读习惯。

   参数：
     Buf: PAnsiChar                       - 待赋值的数据块地址
     ByteLen: Integer                     - 待赋值的数据块字节长度
     Res: TCnBigNumber                    - 用来容纳结果的大数对象

   返回值：Boolean                        - 返回是否赋值成功
}

function BigNumberToBase64(Num: TCnBigNumber): string;
{* 将一个大数对象转成 Base64 字符串，不处理正负号。

   参数：
     Num: TCnBigNumber                    - 待转换的大数对象

   返回值：string                         - 返回 Base64 字符串
}

function BigNumberSetBase64(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
{* 将一串 Base64 字符串赋值给指定大数对象，不处理正负号。

   参数：
     const Buf: AnsiString                - 待转换的 Base64 字符串
     Res: TCnBigNumber                    - 用来容纳结果的大数对象

   返回值：Boolean                        - 返回是否赋值成功
}

function BigNumberFromBase64(const Buf: AnsiString): TCnBigNumber;
{* 根据一串 Base64 字符串产生一个新的大数对象，不处理正负号。其结果不用时必须用 BigNumberFree 释放

   参数：
     const Buf: AnsiString                - 待使用的 Base64 字符串

   返回值：TCnBigNumber                   - 返回新建的大数对象
}

function BigNumberToString(Num: TCnBigNumber): string;
{* 将一个大数对象转成普通可读的十六进制字符串，负以 - 表示。

   参数：
     Num: TCnBigNumber                    - 待转换的大数对象

   返回值：string                         - 返回十六进制字符串
}

function BigNumberToHex(Num: TCnBigNumber; FixedLen: Integer = 0): string;
{* 将一个大数对象转成十六进制字符串，负以 - 表示。
   FixedLen 表示大数内容不够 FixedLen 字节长度时高位补足 0 以保证结果中输出固定 FixedLen 字节的长度（不包括负号）。
   内部大数长度超过 FixedLen 时按大数实际长度写。注意 FixedLen 不是十六进制字符串长度。

   参数：
     Num: TCnBigNumber                    - 待转换的大数对象
     FixedLen: Integer                    - 指定大数实际字节长度不足时使用的固定字节长度并字符串前面补 00，为 0 时则使用大数实际字节长度

   返回值：string                         - 返回十六进制字符串
}

function BigNumberSetHex(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
{* 将一串十六进制字符串赋值给指定大数对象，负以 - 表示，内部不能包括回车换行。
   注意由于通常字符串的左边表示高位，而大数内部高位在高地址方向，因而内部有个倒序过程。

   参数：
     const Buf: AnsiString                - 待赋值的十六进制字符串
     Res: TCnBigNumber                    - 用来容纳结果的大数对象

   返回值：Boolean                        - 返回是否赋值成功
}

function BigNumberFromHex(const Buf: AnsiString): TCnBigNumber;
{* 将一串十六进制字符串转换为大数对象，负以 - 表示。其结果不用时必须用 BigNumberFree 释放

   参数：
     const Buf: AnsiString                - 待使用的十六进制字符串

   返回值：TCnBigNumber                   - 返回新建的大数对象
}

function BigNumberToDec(Num: TCnBigNumber): AnsiString;
{* 将一个大数对象转成十进制字符串，负以 - 表示。

   参数：
     Num: TCnBigNumber                    - 待转换的大数对象

   返回值：AnsiString                     - 返回十进制字符串
}

function BigNumberSetDec(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
{* 将一串十进制字符串赋值给指定大数对象，负以 - 表示，内部不能包括回车换行。

   参数：
     const Buf: AnsiString                - 待赋值的十进制字符串
     Res: TCnBigNumber                    - 用来容纳结果的大数对象

   返回值：Boolean                        - 返回是否赋值成功
}

function BigNumberFromDec(const Buf: AnsiString): TCnBigNumber;
{* 将一串十进制字符串转换为大数对象，负以 - 表示。其结果不用时必须用 BigNumberFree 释放。

   参数：
     const Buf: AnsiString                - 待使用的十进制字符串

   返回值：TCnBigNumber                   - 返回新建的大数对象
}

function BigNumberSetFloat(F: Extended; Res: TCnBigNumber): Boolean;
{* 将浮点数设置给大数对象，忽略小数部分。

   参数：
     F: Extended                          - 待赋值的浮点数
     Res: TCnBigNumber                    - 用来容纳结果的大数对象

   返回值：Boolean                        - 返回是否赋值成功
}

function BigNumberGetFloat(Num: TCnBigNumber): Extended;
{* 将大数转换为浮点数，超标时本应抛出异常但目前暂未处理。

   参数：
     Num: TCnBigNumber                    - 待转换的大数对象

   返回值：Extended                       - 返回浮点数
}

function BigNumberFromFloat(F: Extended): TCnBigNumber;
{* 将浮点数转换为新建的大数对象，其结果不用时必须用 BigNumberFree 释放。

   参数：
     F: Extended                          - 待使用的浮点数

   返回值：TCnBigNumber                   - 返回新建的大数对象
}

function BigNumberEqual(Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean; overload;
{* 比较两个大数对象是否相等，相等返回 True，不等返回 False。

   参数：
     Num1: TCnBigNumber                   - 待比较的大数对象一
     Num2: TCnBigNumber                   - 待比较的大数对象二

   返回值：Boolean                        - 返回是否相等
}

function BigNumberEqual(Num1: TCnBigNumber; Num2: Int64): Boolean; overload;
{* 比较一个大数对象是否等于指定整数，相等返回 True，不等返回 False。

   参数：
     Num1: TCnBigNumber                   - 待比较的大数对象
     Num2: TCnBigNumber                   - 待比较的整数

   返回值：Boolean                        - 返回是否相等
}

function BigNumberCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
{* 带符号比较两个大数对象，前者大于、等于、小于后者时分别返回 1、0、-1。

   参数：
     Num1: TCnBigNumber                   - 待比较的大数对象一
     Num2: TCnBigNumber                   - 待比较的大数对象二

   返回值：Integer                        - 返回比较结果
}

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Integer): Integer; overload;
{* 带符号比较一个大数对象与一个整数，前者大于、等于、小于后者时分别返回 1、0、-1。

   参数：
     Num1: TCnBigNumber                   - 待比较的大数对象
     Num2: Integer                        - 待比较的整数

   返回值：Integer                        - 返回比较结果
}

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Int64): Integer; overload;
{* 带符号比较一个大数对象与一个整数，前者大于、等于、小于后者时分别返回 1、0、-1。

   参数：
     Num1: TCnBigNumber                   - 待比较的大数对象
     Num2: Int64                          - 待比较的整数

   返回值：Integer                        - 返回比较结果
}

function BigNumberUnsignedCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
{* 无符号比较两个大数对象，也就是比较绝对值。前者大于、等于、小于后者时分别返回 1、0、-1。

   参数：
     Num1: TCnBigNumber                   - 待比较的大数对象一
     Num2: TCnBigNumber                   - 待比较的大数对象二

   返回值：Integer                        - 返回比较结果
}

function BigNumberDuplicate(Num: TCnBigNumber): TCnBigNumber;
{* 创建并复制一个大数对象，返回此新大数对象，需要用 BigNumberFree 来释放。

   参数：
     Num: TCnBigNumber                    - 待复制的大数对象

   返回值：TCnBigNumber                   - 返回新建的大数对象
}

function BigNumberCopy(Dest: TCnBigNumber; Source: TCnBigNumber): TCnBigNumber;
{* 复制一个大数对象，成功返回 Dst 对象。

   参数：
     Dest: TCnBigNumber                   - 目标大数对象
     Source: TCnBigNumber                 - 源大数对象

   返回值：TCnBigNumber                   - 复制成功则返回目标大数对象，失败返回 nil
}

function BigNumberCopyLow(Dest: TCnBigNumber; Source: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
{* 复制一个大数对象的低 WordCount 个 UInt32/UInt64 元素，成功返回 Dst。

   参数：
     Dest: TCnBigNumber                   - 目标大数对象
     Source: TCnBigNumber                 - 源大数对象
     WordCount: Integer                   - 待复制的低位元素数

   返回值：TCnBigNumber                   - 复制成功则返回目标大数对象，失败返回 nil
}

function BigNumberCopyHigh(Dest: TCnBigNumber; Source: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
{* 复制一个大数对象的高 WordCount 个 UInt32/UInt64 元素，成功返回 Dst。

   参数：
     Dest: TCnBigNumber                   - 目标大数对象
     Source: TCnBigNumber                 - 源大数对象
     WordCount: Integer                   - 待复制的高位元素数

   返回值：TCnBigNumber                   - 复制成功则返回目标大数对象，失败返回 nil
}

function BigNumberGetLow32(Num: TCnBigNumber): Cardinal;
{* 取出一个大数的低 32 位，不处理符号。

   参数：
     Num: TCnBigNumber                    - 待处理的大数对象

   返回值：Cardinal                       - 返回低 32 位无符号整数
}

function BigNumberGetLow64(Num: TCnBigNumber): TUInt64;
{* 取出一个大数的低 64 位，不处理符号。

   参数：
     Num: TCnBigNumber                    - 待处理的大数对象

   返回值：TUInt64                        - 返回低 64 位无符号整数
}

procedure BigNumberSwap(Num1: TCnBigNumber; Num2: TCnBigNumber);
{* 交换两个大数对象的内容。

   参数：
     Num1: TCnBigNumber                   - 待交换的大数对象一
     Num2: TCnBigNumber                   - 待交换的大数对象二

   返回值：（无）
}

procedure BigNumberSwapBit(Num: TCnBigNumber; BitIndex1: Integer; BitIndex2: Integer);
{* 交换大数中两个指定 Bit 位的内容，BitIndex 均以 0 开始。

   参数：
     Num: TCnBigNumber                    - 待交换的大数对象
     BitIndex1: Integer                   - 待交换的位索引一
     BitIndex2: Integer                   - 待交换的位索引二

   返回值：（无）
}

function BigNumberRandBytes(Num: TCnBigNumber; BytesCount: Integer): Boolean;
{* 产生固定字节长度的随机大数，不保证最高位置 1，甚至最高字节都不保证非 0。

   参数：
     Num: TCnBigNumber                    - 待容纳随机数的大数对象
     BytesCount: Integer                  - 字节长度

   返回值：Boolean                        - 返回生成是否成功
}

function BigNumberRandBits(Num: TCnBigNumber; BitsCount: Integer): Boolean;
{* 产生固定位长度的随机大数，不保证最高位置 1，甚至最高字节都不保证非 0。

   参数：
     Num: TCnBigNumber                    - 待容纳随机数的大数对象
     BitsCount: Integer                   - 位长度

   返回值：Boolean                        - 返回生成是否成功
}

function BigNumberRandRange(Num: TCnBigNumber; Range: TCnBigNumber): Boolean;
{* 产生 [0, Range) 之间的随机大数。

   参数：
     Num: TCnBigNumber                    - 待容纳随机数的大数对象
     Range: TCnBigNumber                  - 随机数上限，不会等于该数

   返回值：Boolean                        - 返回生成是否成功
}

function BigNumberAnd(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 两个大数对象按位与，结果放至 Res 中，返回运算是否成功。Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num1: TCnBigNumber                   - 待按位与的大数对象一
     Num2: TCnBigNumber                   - 待按位与的大数对象二

   返回值：Boolean                        - 返回是否按位与成功
}

function BigNumberOr(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 两个大数对象按位或，结果放至 Res 中，返回运算是否成功。Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num1: TCnBigNumber                   - 待按位或的大数对象一
     Num2: TCnBigNumber                   - 待按位或的大数对象二

   返回值：Boolean                        - 返回是否按位或成功
}

function BigNumberXor(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 两个大数对象按位异或，结果放至 Res 中，返回运算是否成功。Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num1: TCnBigNumber                   - 待按位异或的大数对象一
     Num2: TCnBigNumber                   - 待按位异或的大数对象二

   返回值：Boolean                        - 返回是否按位异或成功
}

function BigNumberUnsignedAdd(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 两个大数对象无符号相加，结果放至 Res 中，返回相加是否成功。Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳和的大数对象
     Num1: TCnBigNumber                   - 加数一
     Num2: TCnBigNumber                   - 加数二

   返回值：Boolean                        - 返回是否相加成功
}

function BigNumberUnsignedSub(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 两个大数对象无符号相减，Num1 减 Num2，结果放至 Res 中，
   返回相减是否成功，如 Num1 < Num2 则失败。

   参数：
     Res: TCnBigNumber                    - 用来容纳差的大数对象
     Num1: TCnBigNumber                   - 被减数，绝对值必须大于等于减数
     Num2: TCnBigNumber                   - 减数，绝对值必须小于等于被减数

   返回值：Boolean                        - 返回是否相减成功
}

function BigNumberAdd(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 两个大数对象带符号相加，结果放至 Res 中，返回相加是否成功，Num1 可以是 Num2，Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳和的大数对象
     Num1: TCnBigNumber                   - 加数一
     Num2: TCnBigNumber                   - 加数二

   返回值：Boolean                        - 返回是否相加成功
}

function BigNumberSub(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 两个大数对象带符号相减，结果放至 Res 中，返回相减是否成功，Num1 可以是 Num2，Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳差的大数对象
     Num1: TCnBigNumber                   - 被减数
     Num2: TCnBigNumber                   - 减数

   返回值：Boolean                        - 返回是否相减成功
}

function BigNumberShiftLeftOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* 将一大数对象左移一位，结果放至 Res 中，返回左移是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待左移的大数对象

   返回值：Boolean                        - 返回左移是否成功
}

function BigNumberShiftRightOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* 将一大数对象右移一位，结果放至 Res 中，返回右移是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待右移的大数对象

   返回值：Boolean                        - 返回右移是否成功
}

function BigNumberShiftLeft(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
{* 将一大数对象左移 N 位，结果放至 Res 中，返回左移是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待左移的大数对象
     N: Integer                           - 左移位数，为负时代表右移

   返回值：Boolean                        - 返回左移是否成功
}

function BigNumberShiftRight(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
{* 将一大数对象右移 N 位，结果放至 Res 中，返回右移是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待右移的大数对象
     N: Integer                           - 右移位数，为负时代表左移

   返回值：Boolean                        - 返回右移是否成功
}

function BigNumberSqr(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* 计算一大数对象的平方，结果放 Res 中，返回平方计算是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberSqrt(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
{* 计算一大数对象的平方根的整数部分，结果放 Res 中，返回平方计算是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberRoot(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Integer): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 计算一大数对象的 Exp 次方根的整数部分，结果放 Res 中，返回根计算是否成功。
   要求 Num 不能为负，Exponent 不能为 0 或负。
   注：FIXME: 因为大数无法进行浮点计算，目前整数运算有偏差，结果偏大，不推荐使用！

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待计算的大数对象
     Exponent: Integer                    - 次方根数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberMul(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 计算两大数对象的乘积，结果放 Res 中，返回乘积计算是否成功，Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳积的大数对象
     Num1: TCnBigNumber                   - 乘数一
     Num2: TCnBigNumber                   - 乘数二

   返回值：Boolean                        - 返回是否相乘成功
}

function BigNumberMulKaratsuba(Res: TCnBigNumber; Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
{* 用 Karatsuba 算法计算两大数对象的乘积，结果放 Res 中，返回乘积计算是否成功，Res 可以是 Num1 或 Num2。
   注：好像也没见快到哪里去。

   参数：
     Res: TCnBigNumber                    - 用来容纳积的大数对象
     Num1: TCnBigNumber                   - 乘数一
     Num2: TCnBigNumber                   - 乘数二

   返回值：Boolean                        - 返回是否相乘成功
}

function BigNumberMulFloat(Res: TCnBigNumber; Num: TCnBigNumber; F: Extended): Boolean;
{* 计算大数对象与浮点数的乘积，结果取整后放 Res 中，返回乘积计算是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳积的大数对象
     Num: TCnBigNumber                    - 大数对象乘数
     F: Extended                          - 浮点数乘数

   返回值：Boolean                        - 返回是否相乘成功
}

function BigNumberDiv(Res: TCnBigNumber; Remain: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
{* 两大数对象相除，Num / Divisor，商放 Res 中，余数放 Remain 中，返回除法计算是否成功，
   Res 可以是 Num，Remain 可以是 nil 以不需要计算余数。
   被除数与除数均以正数相除得到正的商和正的余数，之后的正负规则举例如下：
   正被除数正除数得到正商和正余数，如  1005 /  100 =  10 ...  5
   负被除数正除数得到负商和负余数，如 -1005 /  100 = -10 ... -5
   正被除数负除数得到负商和正余数，如  1005 / -100 = -10 ...  5
   负被除数负除数得到正商和负余数，如 -1005 / -100 =  10 ... -5
   余数符号跟着被除数走，余数绝对值会小于除数绝对值，且不会出现余 95 这种情况。

   参数：
     Res: TCnBigNumber                    - 用来容纳商的大数对象
     Remain: TCnBigNumber                 - 用来容纳余数的大数对象，可为 nil
     Num: TCnBigNumber                    - 被除数
     Divisor: TCnBigNumber                - 除数

   返回值：Boolean                        - 返回是否相除成功
}

function BigNumberRoundDiv(Res: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber; out Rounding: Boolean): Boolean;
{* 两大数对象相除，Num / Divisor，商四舍五入放 Res 中，Res 可以是 Num，
   注意入的方向始终是绝对值大的方向，与 Round 函数基本保持一致，但忽略其四舍六入五成双的规则，逢五必入。
   返回除法计算是否成功，Rounding 参数返回真实结果的舍入情况，True 表示入，False 表示舍。

   参数：
     Res: TCnBigNumber                    - 用来容纳商的大数对象
     Num: TCnBigNumber                    - 被除数
     Divisor: TCnBigNumber                - 除数
     out Rounding: Boolean                - 返回真实结果的舍入情况，True 表示入，False 表示舍

   返回值：Boolean                        - 返回是否相除成功
}

function BigNumberMod(Remain: TCnBigNumber; Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* 两大数对象求余，Num mod Divisor，余数放 Remain 中，余数正负规则等同于 BigNumberDiv 函数。
   返回求余计算是否成功，Remain 可以是 Num。

   参数：
     Remain: TCnBigNumber                 - 用来容纳余数的大数对象
     Num: TCnBigNumber                    - 被除数
     Divisor: TCnBigNumber                - 除数

   返回值：Boolean                        - 返回是否求余成功
}

function BigNumberNonNegativeMod(Remain: TCnBigNumber;
  Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* 两大数对象非负求余，Num mod Divisor，余数放 Remain 中，0 <= Remain < |Divisor|，
   余数的正负规则等同于 BigNumberMod 后再通过加减 Divisor 以确保 Remain 始终大于零。
   与 BigNumberMod 不同的是：
   负被除数正除数先得到负商和负余数，负余数需加正除数，如 -1005 /  100 = ... 95
   负被除数负除数先得到正商和负余数，负余数需减负除数，如 -1005 / -100 = ... 95
   返回求余计算是否成功。

   参数：
     Remain: TCnBigNumber                 - 用来容纳余数的大数对象
     Num: TCnBigNumber                    - 被除数
     Divisor: TCnBigNumber                - 除数

   返回值：Boolean                        - 返回是否求余成功
}

function BigNumberMulWordNonNegativeMod(Res: TCnBigNumber;
  Num: TCnBigNumber; N: Integer; Divisor: TCnBigNumber): Boolean;
{* 大数对象乘以 32 位有符号整型再非负求余，余数放 Res 中，0 <= Remain < |Divisor|。
   Res 始终大于零，返回求余计算是否成功。

   参数：
     Res: TCnBigNumber                    - 用来容纳余数的大数对象
     Num: TCnBigNumber                    - 被除数
     N: Integer                           - 乘数
     Divisor: TCnBigNumber                - 除数

   返回值：Boolean                        - 返回是否求余成功
}

function BigNumberAddMod(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* 大数对象求和后非负求余，也就是 Res = (Num1 + Num2) mod Divisor，返回求余计算是否成功。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num1: TCnBigNumber                   - 加数一
     Num2: TCnBigNumber                   - 加数二
     Divisor: TCnBigNumber                - 除数

   返回值：Boolean                        - 返回是否求余成功
}

function BigNumberSubMod(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
{* 大数对象求差后非负求余，也就是 Res = (Num1 - Num2) mod Divisor，返回求余计算是否成功。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num1: TCnBigNumber                   - 被减数
     Num2: TCnBigNumber                   - 减数
     Divisor: TCnBigNumber                - 除数

   返回值：Boolean                        - 返回是否求余成功
}

function BigNumberDivFloat(Res: TCnBigNumber; Num: TCnBigNumber;
  F: Extended): Boolean;
{* 计算大数对象与浮点数的商，结果取整后放 Res 中，返回相除是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 大数对象被除数
     F: Extended                          - 浮点数除数

   返回值：Boolean                        - 返回是否相除成功
}

function BigNumberPower(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Cardinal): Boolean;
{* 求大数的整数次方，返回计算是否成功，Res 可以是 Num。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 底数
     Exponent: Cardinal                   - 指数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberExp(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
{* 求大数 Num 的 Exponent  次方，返回乘方计算是否成功，极其耗时。
   Res 可以是 Num 或 Exponent。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 底数
     Exponent: TCnBigNumber               - 指数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberGcd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 求两个大数 Num1 与 Num2 的最大公约数，Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num1: TCnBigNumber                   - 待求最大公约数的大数对象一
     Num2: TCnBigNumber                   - 待求最大公约数的大数对象二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberLcm(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 求两个大数 Num1 与 Num2 的最小公倍数，Res 可以是 Num1 或 Num2。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num1: TCnBigNumber                   - 待求最小公倍数的大数对象一
     Num2: TCnBigNumber                   - 待求最小公倍数的大数对象二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberUnsignedMulMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber): Boolean;
{* 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）。
   注意: 三个参数均会忽略负值，也就是均用正值参与计算。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 乘数一
     B: TCnBigNumber                      - 乘数二
     C: TCnBigNumber                      - 除数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberMulMod(Res: TCnBigNumber; A: TCnBigNumber; B: TCnBigNumber;
  C: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）。
   注意: A、B 允许是负值，乘积为负时，结果为 C - 乘积为正的余。
   另外该方法因为比下面的 BigNumberDirectMulMod 慢，所以不建议使用。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 乘数一
     B: TCnBigNumber                      - 乘数二
     C: TCnBigNumber                      - 除数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberDirectMulMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber): Boolean;
{* 普通计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）。
   注意：位数较少时，该方法比上面的 BigNumberMulMod 方法要快不少，另外内部执行的是 NonNegativeMod，余数为正。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 乘数一
     B: TCnBigNumber                      - 乘数二
     C: TCnBigNumber                      - 除数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberMontgomeryReduction(Res: TCnBigNumber; T: TCnBigNumber;
  R: TCnBigNumber; N: TCnBigNumber; NNegInv: TCnBigNumber): Boolean;
{* 蒙哥马利约简法快速计算 (T * R^-1) mod N，其中要求 R 是刚好比 N 大的 2 整数次幂，
   NNegInv 是预先计算好的 N 对 R 的负模逆元，T 不能为负且必须小于 N * R。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     T: TCnBigNumber                      - 蒙哥马利约简法里的 T
     R: TCnBigNumber                      - 蒙哥马利约简法里的 R
     N: TCnBigNumber                      - 蒙哥马利约简法里的模数 N
     NNegInv: TCnBigNumber                - 预先计算好的 N 对 R 的负模逆元

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberMontgomeryMulMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; R: TCnBigNumber; R2ModN: TCnBigNumber;
  N: TCnBigNumber; NNegInv: TCnBigNumber): Boolean;
{* 蒙哥马利模乘法（内部使用四次蒙哥马利约简法）快速计算 A * B * R^-1 mod N，其中要求 R 是刚好比 N 大的 2 整数次幂，
   R2ModN 是预先计算好的 R^2 mod N 的值，NNegInv 是预先计算好的 N 对 R 的负模逆元。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 蒙哥马利模乘法里的乘数一
     B: TCnBigNumber                      - 蒙哥马利模乘法里的乘数二
     R: TCnBigNumber                      - 蒙哥马利模乘法里的 R
     R2ModN: TCnBigNumber                 - 预先计算好的 R^2 mod N 的值
     N: TCnBigNumber                      - 蒙哥马利约简法里的模数 N
     NNegInv: TCnBigNumber                - 预先计算好的 N 对 R 的负模逆元

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPowerWordMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: Cardinal; C: TCnBigNumber): Boolean;
{* 快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、C 之一，内部调用 BigNumberPowerMod。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 大数对象乘数
     B: Cardinal                          - 整数乘数
     C: TCnBigNumber                      - 模数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPowerMod(Res: TCnBigNumber; A: TCnBigNumber; B: TCnBigNumber;
  C: TCnBigNumber): Boolean;
{* 滑动窗口法快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、B、C 之一，性能比下面的蒙哥马利法好大约百分之十。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 乘数一
     B: TCnBigNumber                      - 乘数二
     C: TCnBigNumber                      - 模数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberMontgomeryPowerMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 蒙哥马利法快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、B、C 之一，性能略差，可以不用。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 乘数一
     B: TCnBigNumber                      - 乘数二
     C: TCnBigNumber                      - 模数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPowerPowerMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; C: TCnBigNumber; N: TCnBigNumber): Boolean;
{* 快速计算 A ^ (B ^ C) mod N，更不能直接算，更容易溢出。Res 不能是 A、B、C、N 之一。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 底数一
     B: TCnBigNumber                      - 底数二
     C: TCnBigNumber                      - 指数
     N: TCnBigNumber                      - 模数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberLog2(Num: TCnBigNumber): Extended;
{* 返回大数的 2 为底的对数的扩展精度浮点值，内部用扩展精度浮点实现。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Extended                       - 返回 2 为底的对数值
}

function BigNumberLog10(Num: TCnBigNumber): Extended;
{* 返回大数的 10 为底的常用对数的扩展精度浮点值，内部用扩展精度浮点实现。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Extended                       - 返回常用对数值
}

function BigNumberLogN(Num: TCnBigNumber): Extended;
{* 返回大数的 e 为底的自然对数的扩展精度浮点值，内部用扩展精度浮点实现。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：Extended                       - 返回自然对数值
}

function BigNumberFermatCheckComposite(A: TCnBigNumber; B: TCnBigNumber;
  C: TCnBigNumber; T: Integer): Boolean;
{* Miller-Rabin 算法中的单次费马测试，返回 True 表示 B 不是素数，
   注意 A B C 并非任意选择，B 是待测试的素数，A 是随机数，C 是 B - 1 右移 T 位后得到的第一个奇数。

   参数：
     A: TCnBigNumber                      - 随机数
     B: TCnBigNumber                      - 待测试的素数
     C: TCnBigNumber                      - B - 1 右移 T 位后得到的第一个奇数
     T: Integer                           - Miller-Rabin 算法中的 T 值

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberIsProbablyPrime(Num: TCnBigNumber; TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 概率性判断一个大数是否素数，TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢。
   注意不能采用简单的费马小定理判断或 Solovay-Strassen 概率性素性检测，因为对 Carmichael 数无效。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象
     TestCount: Integer                   - 测试次数

   返回值：Boolean                        - 返回是否素数
}

function BigNumberGeneratePrime(Num: TCnBigNumber; BytesCount: Integer;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 生成一个指定字节长度的大素数，不保证最高位为 1。
   TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢。

   参数：
     Num: TCnBigNumber                    - 用来容纳结果的大数对象
     BytesCount: Integer                  - 字节长度
     TestCount: Integer                   - 测试次数

   返回值：Boolean                        - 返回是否生成成功
}

function BigNumberGeneratePrimeByBitsCount(Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 生成一个指定二进制位数的大素数，最高位确保为 1。
   TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢。

   参数：
     Num: TCnBigNumber                    - 用来容纳结果的大数对象
     BitsCount: Integer                   - 位长度
     TestCount: Integer                   - 测试次数

   返回值：Boolean                        - 返回是否生成成功
}

function BigNumberNextPrime(Res: TCnBigNumber; Num: TCnBigNumber;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 生成一个比 Num 大或相等的大素数，结果放 Res，Res 可以是 Num。
   TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待比较的大数对象
     TestCount: Integer                   - 测试次数

   返回值：Boolean                        - 返回是否生成成功
}

function BigNumberCheckPrimitiveRoot(R: TCnBigNumber; Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
{* 原根判断辅助函数。判断 R 是否对于 Prime - 1 的每个因子，都有 R ^ (剩余因子的积) mod Prime <> 1。
   Factors 必须是 Prime - 1 的不重复的素数因子列表，可从 BigNumberFindFactors 获取并去重而来。

   参数：
     R: TCnBigNumber                      - 待判断的大数对象
     Prime: TCnBigNumber                  - 素数
     Factors: TCnBigNumberList            - Prime - 1 的不重复的素数因子列表

   返回值：Boolean                        - 返回是否判断成功
}

function BigNumberGetMinRootFromPrime(Res: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 计算一素数的原根，返回计算是否成功。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Prime: TCnBigNumber                  - 素数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberIsInt32(Num: TCnBigNumber): Boolean;
{* 大数是否是一个 32 位有符号整数范围内的数。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否在 32 位有符号整数范围内
}

function BigNumberIsUInt32(Num: TCnBigNumber): Boolean;
{* 大数是否是一个 32 位无符号整数范围内的数。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否在 32 位无符号整数范围内
}

function BigNumberIsInt64(Num: TCnBigNumber): Boolean;
{* 大数是否是一个 64 位有符号整数范围内的数。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否在 64 位有符号整数范围内
}

function BigNumberIsUInt64(Num: TCnBigNumber): Boolean;
{* 大数是否是一个 64 位无符号整型范围内的数。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否在 64 位无符号整数范围内
}

function BigNumberIsFloat(Num: TCnBigNumber): Boolean;
{* 大数是否是一个扩展精度浮点范围内的数，注意仅判断范围，不保证精度。
   另外 Win64 下 Extended 实际上是双精度，Win32 下才是扩展精度。

   参数：
     Num: TCnBigNumber                    - 待判断的大数对象

   返回值：Boolean                        - 返回是否在扩展精度浮点范围内
}

procedure BigNumberExtendedEuclideanGcd(A: TCnBigNumber; B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解。
   调用者需自行保证 A B 互素，因为结果只满足 A * X + B * Y = GCD(A, B)。
   A, B 是已知大数，X, Y 是解出来的结果，注意 X 有可能小于 0，如需要正数，可以再加上 B

   参数：
     A: TCnBigNumber                      - 二元一次不定方程系数 A
     B: TCnBigNumber                      - 二元一次不定方程系数 B
     X: TCnBigNumber                      - 用来容纳结果 X 的大数对象
     Y: TCnBigNumber                      - 用来容纳结果 Y 的大数对象

   返回值：（无）
}

procedure BigNumberExtendedEuclideanGcd2(A: TCnBigNumber; B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X - B * Y = 1 的整数解。
   调用者需自行保证 A B 互素，因为结果只满足 A * X + B * Y = GCD(A, B)。
   A, B 是已知大数，X, Y 是解出来的结果，注意 X 有可能小于 0，如需要正数，可以再加上 B。
   X 被称为 A 针对 B 的模反元素，因此本算法也用来算 A 针对 B 的模反元素。
   （由于可以视作 -Y，所以本方法与上一方法是等同的）。

   参数：
     A: TCnBigNumber                      - 二元一次不定方程系数 A
     B: TCnBigNumber                      - 二元一次不定方程系数 B
     X: TCnBigNumber                      - 用来容纳结果 X 的大数对象
     Y: TCnBigNumber                      - 用来容纳结果 Y 的大数对象

   返回值：（无）
}

function BigNumberModularInverse(Res: TCnBigNumber;
  X: TCnBigNumber; Modulus: TCnBigNumber; CheckGcd: Boolean = False): Boolean;
{* 求 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。
   CheckGcd 参数为 True 时，内部会检查 X、Modulus 是否互素，不互素则直接返回 False。
   调用者须自行保证 X、Modulus 互素，且 Res 不能是 X 或 Modulus。

   参数：
     Res: TCnBigNumber                    - 用来容纳模逆元的大数对象
     X: TCnBigNumber                      - 待计算的大数对象
     Modulus: TCnBigNumber                - 模数
     CheckGcd: Boolean                    - 是否检查互素

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPrimeModularInverse(Res: TCnBigNumber;
  X: TCnBigNumber; Modulus: TCnBigNumber): Boolean;
{* 求 X 针对素数 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。
   调用者须自行保证 Modulus 为素数，且 Res 不能是 X 或 Modulus，内部用费马小定理求值，略慢

   参数：
     Res: TCnBigNumber                    - 用来容纳模逆元的大数对象
     X: TCnBigNumber                      - 待计算的大数对象
     Modulus: TCnBigNumber                - 模素数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberNegativeModularInverse(Res: TCnBigNumber;
  X: TCnBigNumber; Modulus: TCnBigNumber; CheckGcd: Boolean = False): Boolean;
{* 求 X 针对 Modulus 的负模反或叫负模逆元 Y，满足 (X * Y) mod M = -1，X 可为负值，Y 求出正值。
   调用者须自行保证 X、Modulus 互素，且 Res 不能是 X 或 Modulus。

   参数：
     Res: TCnBigNumber                    - 用来容纳负模逆元的大数对象
     X: TCnBigNumber                      - 待计算的大数对象
     Modulus: TCnBigNumber                - 模数
     CheckGcd: Boolean                    - 是否检查互素

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberModularInverseWord(Res: TCnBigNumber;
  X: Integer; Modulus: TCnBigNumber; CheckGcd: Boolean = False);
{* 求 32 位有符号数 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。
   调用者须自行保证 X、Modulus 互素，且 Res 不能是 X 或 Modulus。

   参数：
     Res: TCnBigNumber                    - 用来容纳模逆元的大数对象
     X: Integer                           - 待计算的整数
     Modulus: TCnBigNumber                - 模数
     CheckGcd: Boolean                    - 否检查互素

   返回值：（无）
}

function BigNumberLegendre(A: TCnBigNumber; P: TCnBigNumber): Integer;
{* 用二次互反律递归计算勒让德符号 (A / P) 的值，较快。调用者需自行保证 P 为奇素数。

   参数：
     A: TCnBigNumber                      - 勒让德符号中的 A
     P: TCnBigNumber                      - 勒让德符号中的 P

   返回值：Integer                        - 返回勒让德符号
}

function BigNumberLegendre2(A: TCnBigNumber; P: TCnBigNumber): Integer; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 用欧拉判别法计算勒让德符号 (A / P) 的值，较慢，不推荐使用。

   参数：
     A: TCnBigNumber                      - 勒让德符号中的 A
     P: TCnBigNumber                      - 勒让德符号中的 P

   返回值：Integer                        - 返回勒让德符号
}

function BigNumberTonelliShanks(Res: TCnBigNumber; A: TCnBigNumber; P: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 使用 Tonelli-Shanks 算法进行模素数二次剩余求解，也就是求 Res^2 mod P = A，返回是否有解。
   调用者需自行保证 P 为奇素数或奇素数的整数次方，该方法略慢，不推荐使用。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 模素数二次剩余中的 A
     P: TCnBigNumber                      - 模素数二次剩余中的 P

   返回值：Boolean                        - 返回是否有解
}

function BigNumberLucas(Res: TCnBigNumber; A: TCnBigNumber; P: TCnBigNumber): Boolean;
{* 使用 IEEE P1363 规范中的 Lucas 序列进行模素数二次剩余求解，也就是求 Res^2 mod P = A，返回是否有解。
   似乎 P 应该是模 8 余 1 型素数。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 模素数二次剩余中的 A
     P: TCnBigNumber                      - 模素数二次剩余中的 P

   返回值：Boolean                        - 返回是否有解
}

function BigNumberSquareRootModPrime(Res: TCnBigNumber; A: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 通用函数求模素数二次剩余 X^2 mod P = A 的解，返回是否求解成功，如成功，Res 是其中一个正值的解。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 模素数二次剩余中的 A
     Prime: TCnBigNumber                  - 模素数二次剩余中的 P

   返回值：Boolean                        - 返回是否有解
}

function BigNumberJacobiSymbol(A: TCnBigNumber; N: TCnBigNumber): Integer;
{* 计算雅可比符号，其中 N 必须是奇数，如果 N 是负奇数，则 A 为正时等同于 N 为正奇数，
   A 为负时等同于 -(A / -N)。如果 N 是奇素数则等同于勒让德符号。A 可以为任意整数。

   参数：
     A: TCnBigNumber                      - 雅可比符号中的 A
     N: TCnBigNumber                      - 雅可比符号中的 N

   返回值：Integer                        - 返回雅可比符号
}

procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
{* 找出大数的素数因子列表，可重复且未排序。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     Factors: TCnBigNumberList            - 用来容纳结果的大数列表

   返回值：（无）
}

procedure BigNumberFindAllFactors(Num: TCnBigNumber; AllFactors: TCnBigNumberList);
{* 找出大数的所有因子列表，不重复且排序。

   参数：
     Num: TCnBigNumber                    - 待计算的大数对象
     AllFactors: TCnBigNumberList         - 用来容纳结果的大数列表

   返回值：（无）
}

procedure BigNumberEuler(Res: TCnBigNumber; Num: TCnBigNumber);
{* 求不大于大整数 Num 的且与 Num 互素的正整数的个数，也就是欧拉函数。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Num: TCnBigNumber                    - 待计算的大数对象

   返回值：（无）
}

function BigNumberLucasUSequenceMod(P: TCnBigNumber; Q: TCnBigNumber; K: TCnBigNumber;
  N: TCnBigNumber; U: TCnBigNumber): Boolean;
{* 计算 Lucas 的 U 序列，调用者需自行保证 N 为奇素数。
   Lucas 的 U 序列递归定义为：U0 = 0, U1 = 1, and Uk = P * Uk-1 - Q * Vk-2   for k >= 2，
   U 返回 Uk mod N。

   参数：
     P: TCnBigNumber                      - Lucas 序列中的 P 参数大数对象
     Q: TCnBigNumber                      - Lucas 序列中的 Q 参数大数对象
     K: TCnBigNumber                      - Lucas 序列中的 K 序数
     N: TCnBigNumber                      - Lucas 序列中的 N 模数
     U: TCnBigNumber                      - 容纳 Lucas 的 U 序列中的 Uk mod N 的值

   返回值：Boolean                        - 返回计算是否成功
}

function BigNumberLucasVSequenceMod(X: TCnBigNumber; Y: TCnBigNumber; K: TCnBigNumber;
  N: TCnBigNumber; Q: TCnBigNumber; V: TCnBigNumber): Boolean;
{* 计算 IEEE P1363 的规范中说明的 Lucas 的 V 序列，调用者需自行保证 N 为奇素数。
   Lucas 的 V 序列递归定义为：V0 = 2, V1 = X, and Vk = X * Vk-1 - Y * Vk-2   for k >= 2，
   V 返回 Vk mod N，Q 返回 Y ^ (K div 2) mod N。

   参数：
     X: TCnBigNumber                      - Lucas 序列中的 X 参数大数对象
     Y: TCnBigNumber                      - Lucas 序列中的 Y 参数大数对象
     K: TCnBigNumber                      - Lucas 序列中的 K 序数
     N: TCnBigNumber                      - Lucas 序列中的 N 模数
     Q: TCnBigNumber                      - 容纳 Lucas 的 V 序列中的 Y ^ (K div 2) mod N 的值
     V: TCnBigNumber                      - 容纳 Lucas 的 V 序列中的 Vk mod N 的值

   返回值：Boolean                        - 返回计算是否成功
}

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers: TCnBigNumberList; Factors: TCnBigNumberList): Boolean; overload;
{* 用中国剩余定理，根据余数与互素的除数求一元线性同余方程组的最小解，返回求解是否成功。
  参数为大数列表。Remainers 支持负余数，调用者须确保 Factors 均为正且两两互素。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Remainers: TCnBigNumberList          - 余数列表
     Factors: TCnBigNumberList            - 除数列表

   返回值：Boolean                        - 返回是否求解成功
}

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers: TCnInt64List; Factors: TCnInt64List): Boolean; overload;
{* 用中国剩余定理，根据余数与互素的除数求一元线性同余方程组的最小解，返回求解是否成功。
   参数为 Int64 列表

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     Remainers: TCnInt64List              - 余数列表
     Factors: TCnInt64List                - 除数列表

   返回值：Boolean                        - 返回是否求解成功
}

function BigNumberIsPerfectPower(Num: TCnBigNumber): Boolean;
{* 判断大数是否是完全幂，大数较大时有一定耗时。

   参数：
     Num: TCnBigNumber                    - 待判断大数对象

   返回值：Boolean                        - 返回是否完全幂
}

procedure BigNumberFillCombinatorialNumbers(List: TCnBigNumberList; N: Integer);
{* 计算组合数 C(m, N) 并生成大数对象放至大数数组中，其中 m 从 0 到 N。

   参数：
     List: TCnBigNumberList               - 用来容纳组合数的大数列表
     N: Integer                           - 组合数 N

   返回值：（无）
}

procedure BigNumberFillCombinatorialNumbersMod(List: TCnBigNumberList; N: Integer; P: TCnBigNumber);
{* 计算组合数 C(m, N) mod P 并生成大数对象放至大数数组中，其中 m 从 0 到 N。

   参数：
     List: TCnBigNumberList               - 用来容纳结果的大数列表
     N: Integer                           - 组合数 N
     P: TCnBigNumber                      - 模数

   返回值：（无）
}

function BigNumberAKSIsPrime(N: TCnBigNumber): Boolean; overload;
{* 用 AKS 算法准确判断某大正整数是否是素数，判断 9223372036854775783 约需 3 秒，
   判断 334551562509014333221406299998596765179 约需 10 秒。

   参数：
     N: TCnBigNumber                      - 待判断的大数对象

   返回值：Boolean                        - 返回是否素数
}

function BigNumberAKSIsPrime(N: Int64): Boolean; overload;
{* 用 AKS 算法准确判断某正整数是否是素数。

   参数：
     N: Int64                             - 待判断的正整数

   返回值：Boolean                        - 返回是否素数
}

function BigNumberBPSWIsPrime(N: TCnBigNumber): Boolean; overload;
{* 用 Baillie-PSW 算法大概率准确判断某大正整数是否是素数，判断 2048 位素数耗时一秒内
   RSA 中生成两个 2048 位的素数如使用本算法进行判断，耗时约 5 秒。

   参数：
     N: TCnBigNumber                      - 待判断的大数对象

   返回值：Boolean                        - 返回是否素数
}

function BigNumberBPSWIsPrime(N: Int64): Boolean; overload;
{* 用 Baillie-PSW 算法大概率准确判断某正整数是否是素数。

   参数：
     N: Int64                             - 待判断的正整数

   返回值：Boolean                        - 返回是否素数
}

function BigNumberIsMersennePrime(E: Integer): Boolean;
{* 用 Lucas-Lehmer 定理检查 2 的 E 次方减一是否是梅森素数，E 达到 20 多时跑起来就开始慢了。

   参数：
     E: Integer                           - 待检查的 2 的指数

   返回值：Boolean                        - 返回是否梅森素数
}

function BigNumberNonAdjanceFormWidth(N: TCnBigNumber; Width: Integer = 1): TShortInts;
{* 返回大数的 Width 宽度（也就是 2^Width 进制）的 NAF 非零值不相邻形式，Width 为 1 时为普通 NAF 形式。
   Width 1 和 2 等价。每个字节是有符号一项，绝对值小于 2^(Width-1)，所以有限制 1 < W <= 7。

   参数：
     N: TCnBigNumber                      - 待计算的大数对象
     Width: Integer                       - 宽度

   返回值：TShortInts                     - 返回 NAF 结果
}

function BigNumberBigStepGiantStep(Res: TCnBigNumber; A: TCnBigNumber;
  B: TCnBigNumber; M: TCnBigNumber): Boolean;
{* 大步小步算法求离散对数问题 A^X mod M = B 的解 Res，要求 A 和 M 互素。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     A: TCnBigNumber                      - 离散对数问题中的 A
     B: TCnBigNumber                      - 离散对数问题中的 B
     M: TCnBigNumber                      - 离散对数问题中的模数 M

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberMultiplicativeOrder(Res: TCnBigNumber; N: TCnBigNumber;
  R: TCnBigNumber): Boolean;
{* 用欧拉函数求大整数 N 模 R 的乘法阶，也即求满足 N^K mod R = 1 的 K 的最小正整数值。
   乘法阶一定比 R 小，但与 N 大小无关。且由欧拉定理，俩互素时 R 的欧拉函数值一定满足等式但未必最小。
   所以乘法阶必然是 R 的欧拉函数值的因数，可将其因数分解后组合遍历，比暴力遍历效率高。
   注意乘法阶必须 N 和 R 互素才存在，如不互素，本函数返回 False。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     N: TCnBigNumber                      - 待求乘法阶的大数
     R: TCnBigNumber                      - 模数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberDebugDump(Num: TCnBigNumber): string;
{* 打印大数内部信息。

   参数：
     Num: TCnBigNumber                    - 待输出的大数对象

   返回值：string                         - 返回内部信息字符串
}

function BigNumberRawDump(Num: TCnBigNumber; Mem: Pointer = nil): Integer;
{* 将大数内部信息原封不动 Dump 至 Mem 所指的内存区，如果 Mem 传 nil，则返回所需的字节长度。

   参数：
     Num: TCnBigNumber                    - 待输出的大数对象
     Mem: Pointer                         - 待输出的数据块地址

   返回值：Integer                        - 返回实际写入字节长度
}

// ========================= 稀疏大数列表操作函数 ==============================

function SparseBigNumberListIsZero(P: TCnSparseBigNumberList): Boolean;
{* 判断 SparseBigNumberList 是否为 0，注意 nil、0 个项、唯一 1 个项是 0，均作为 0 处理。

   参数：
     P: TCnSparseBigNumberList            - 待判断的大数列表

   返回值：Boolean                        - 返回是否为 0
}

function SparseBigNumberListEqual(A: TCnSparseBigNumberList; B: TCnSparseBigNumberList): Boolean;
{* 判断两个 SparseBigNumberList 是否相等，注意 nil、0 个项、唯一 1 个项是 0，均作为 0 处理。

   参数：
     A: TCnSparseBigNumberList            - 待比较的大数列表一
     B: TCnSparseBigNumberList            - 待比较的大数列表二

   返回值：Boolean                        - 返回是否相等
}

procedure SparseBigNumberListCopy(Dest: TCnSparseBigNumberList; Source: TCnSparseBigNumberList);
{* 将 Src 复制至 Dst。

   参数：
     Dest: TCnSparseBigNumberList         - 目标大数列表
     Source: TCnSparseBigNumberList       - 源大数列表

   返回值：（无）
}

procedure SparseBigNumberListMerge(Dest: TCnSparseBigNumberList; Source1: TCnSparseBigNumberList;
  Source2: TCnSparseBigNumberList; Add: Boolean = True);
{* 合并两个 SparseBigNumberList 至目标 List 中，指数相同的系数 Add 为 True 时相加，否则相减。
   Dest 可以是 Source1 或 Source2，Source1 和 Source2 可以相等。

   参数：
     Dest: TCnSparseBigNumberList         - 目标大数列表
     Source1: TCnSparseBigNumberList      - 待合并的大数列表一
     Source2: TCnSparseBigNumberList      - 待合并的大数列表二
     Add: Boolean                         - True 表示相加，False 表示相减

   返回值：（无）
}

// ============================ 其他大数函数 ===================================

function CnBigNumberIs64Mode: Boolean;
{* 当前大数整体的工作模式是否是内部 64 位存储模式，供调试使用

   参数：
     （无）

   返回值：Boolean                        - 返回是否 64 位大数工作模式
}

var
  CnBigNumberOne: TCnBigNumber = nil;     // 表示 1 的大数常量
  CnBigNumberZero: TCnBigNumber = nil;    // 表示 0 的大数常量
  CnBigNumberNegOne: TCnBigNumber = nil;  // 表示 -1 的大数常量

implementation

uses
  CnPrime, CnBigDecimal, CnFloat, CnBase64;

resourcestring
{$IFDEF BN_DATA_USE_64}
  SCnErrorBigNumberInvalid64ModRange = 'Mod Word only Supports Unsigned Int32';
{$ENDIF}
  SCnErrorBigNumberLogRange = 'Log Range Error';
  SCnErrorBigNumberLegendre = 'Legendre: A, P Must > 0';
  SCnErrorBigNumberJacobiSymbol = 'Jacobi Symbol: A, N Must > 0';
  SCnErrorBigNumberFloatExponentRange = 'Extended Float Exponent Range Error';
  SCnErrorBigNumberParamDupRef = 'Duplicated References for BigNumber Parameters';
  SCnErrorBigNumberFreeFromPool = 'Error. Try to Free a Big Number From Pool';
  SCnErrorBigNumberZeroZeroExp = '0^0 Not Defined';

const
  Hex: string = '0123456789ABCDEF';

  BN_BITS_UINT_32       = 32;
  BN_BITS_UINT_64       = 64;

{$IFDEF BN_DATA_USE_64}
  BN_BYTES              = 8;      // D 数组中的一个元素所包含的字节数
  BN_BITS2              = 64;     // D 数组中的一个元素所包含的位数
  BN_BITS4              = 32;
  BN_MASK2              = $FFFFFFFFFFFFFFFF;
  BN_TBIT               = $8000000000000000;
  BN_MASK2l             = $FFFFFFFF;
  BN_MASK2h             = $FFFFFFFF00000000;
{$ELSE}
  BN_BYTES              = 4;      // D 数组中的一个元素所包含的字节数
  BN_BITS2              = 32;     // D 数组中的一个元素所包含的位数
  BN_BITS4              = 16;
  BN_MASK2              = $FFFFFFFF;
  BN_TBIT               = $80000000;
  BN_MASK2l             = $FFFF;
  BN_MASK2h             = $FFFF0000;
{$ENDIF}

  BN_MASK2S             = $7FFFFFFF; // 以下无需跟随 32/64 改动
  BN_MASK2h1            = $FFFF8000;
  BN_MASK3S             = $7FFFFFFFFFFFFFFF;
  BN_MASK3U             = $FFFFFFFFFFFFFFFF;

  BN_DEC_CONV = 1000000000;
  BN_DEC_FMT = '%u';
  BN_DEC_FMT2 = '%.9u';
  BN_PRIME_NUMBERS = 2048;

  BN_MUL_KARATSUBA = 80;  // 大于等于 80 个 LongWord 的乘法才用 Karatsuba 算法
  CRLF = #13#10;

  SPARSE_BINARY_SEARCH_THRESHOLD = 4;

{$IFNDEF MSWINDOWS}
  MAXDWORD = Cardinal($FFFFFFFF);
{$ENDIF}

var
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalBigBinaryPool: TCnBigBinaryPool = nil;

{$IFDEF BN_DATA_USE_64}
  FCnBigNumberIs64: Boolean = True;
{$ELSE}
  FCnBigNumberIs64: Boolean = False;
{$ENDIF}

function DefBigNumberCompare(Item1, Item2: Pointer): Integer;
var
  A, B: TCnBigNumber;
begin
  A := TCnBigNumber(Item1);
  B := TCnBigNumber(Item2);

  if (A = nil) and (B = nil) then
    Result := 0
  else if A = nil then
    Result := -1
  else if B = nil then
    Result := 1
  else
    Result := BigNumberCompare(A, B);
end;

function CnBigNumberIs64Mode: Boolean;
begin
  Result := FCnBigNumberIs64;
end;

function BigNumberNew: TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
end;

procedure BigNumberInit(Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  Num.Top := 0;
  Num.Neg := 0;
  Num.DMax := 0;
  Num.D := nil;
end;

procedure BigNumberFree(Num: TCnBigNumber);
begin
  Num.Free;
end;

function BigNumberIsZero(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Num.Top = 0);
end;

function BigNumberSetZero(Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 0);
end;

function BigNumberIsOne(Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 0) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberIsNegOne(Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 1) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberSetOne(Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 1);
end;

function BigNumberIsOdd(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (Num.Top > 0) and ((PCnBigNumberElementArray(Num.D)^[0] and 1) <> 0) then
    Result := True
  else
    Result := False;
end;

function BigNumberIsEven(Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (Num.Top = 0) or ((PCnBigNumberElementArray(Num.D)^[0] and 1) = 0) then
    Result := True
  else
    Result := False;
end;

function BigNumberGetWordBitsCount(L: Cardinal): Integer; {$IFDEF BN_DATA_USE_64} overload; {$ENDIF}
const
  Bits: array[0..255] of Byte = (
    0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
  );
begin
  if (L and $FFFF0000) <> 0 then
  begin
    if (L and $FF000000) <> 0 then
      Result := Bits[L shr 24] + 24
    else
      Result := Bits[L shr 16] + 16;
  end
  else
  begin
    if (L and $FF00) <> 0 then
      Result := Bits[L shr 8] + 8
    else
      Result := Bits[L];
  end;
end;

{$IFDEF BN_DATA_USE_64}

function BigNumberGetWordBitsCount(L: UInt64): Integer; overload;
var
  C: Cardinal;
begin
  C := Cardinal(L shr BN_BITS_UINT_32); // 高 32 位
  if C = 0 then
    Result := BigNumberGetWordBitsCount(Cardinal(L and BN_MASK2)) // 高 32 位为 0 则返回低 32 位的位数
  else
    Result := BigNumberGetWordBitsCount(C) + BN_BITS_UINT_32;     // 高 32 位不为 0 则返回高 32 位的位数加 32
end;

{$ENDIF}

function BigNumberGetBitsCount(Num: TCnBigNumber): Integer;
var
  I: Integer;
begin
  Result := 0;
  if BigNumberIsZero(Num) then
    Exit;

  I := Num.Top - 1;
  Result := ((I * BN_BITS2) + BigNumberGetWordBitsCount(PCnBigNumberElementArray(Num.D)^[I]));
end;

function BigNumberGetBytesCount(Num: TCnBigNumber): Integer;
begin
  Result := (BigNumberGetBitsCount(Num) + 7) div 8;
end;

function BigNumberGetWordsCount(Num: TCnBigNumber): Integer;
begin
  Result := Num.Top;
end;

function BigNumberGetTenPrecision(Num: TCnBigNumber): Integer;
const
  LOG_10_2 = 0.30103;
var
  B, P, Q: Integer;
  N: TCnBigNumber;
begin
  Result := 0;
  if Num.IsZero then
    Exit;

  B := Num.GetBitsCount;
  if B <= 3 then
  begin
    Result := 1;
    Exit;
  end;

  P := Trunc(LOG_10_2 * B) + 1;
  Q := Trunc(LOG_10_2 * (B - 1)) + 1;
  // N 位二进制全 1 时，比它略大的（也就是 N + 1 位的二进制只有高位 1 的） 10 进制形式的位数，是 (N) * Log10(2) 的整数部分 + 1，假设为 P
  // N 位二进制只有高位 1 时，它的 10 进制形式的位数，是 (N - 1) * Log10(2) 的整数部分 + 1，假设为 Q，Q 比 P 最多差个 1，有可能相等
  // 也就是 N 位二进制最大时，其 10 进制位数等于 P，N 位二进制最小只有高位为 1 时，其 10 进制位数等于 Q
  // 也就是说，N 位二进制代表的值，其值必然大于 10^(Q - 1)，并且有可能提前大于 10^(P - 1)，P、Q 最多差 1，最多只要比较一次就行了

  // 但如何快速计算出 10^Q，还是得用二进制幂乘法 BigNumberPower

  if P = Q then  // 如果这个 N，最大最小的 10 进制位数都一样，则无需额外比较计算，直接返回
  begin
    Result := P;
    Exit;
  end;

  N := FLocalBigNumberPool.Obtain;
  try
    // 先计算 P 位 10 进制的最小值 10^(P - 1) 次幂，和本数比较，注意这里 P 比 Q 大一，
    N.SetWord(10);
    N.PowerWord(Q);

    Result := Q;
    if BignumberUnsignedCompare(Num, N) < 0 then
      Exit;

    Inc(Result); // 如果大于或等于 P 位 10 进制的最小值 10^(P - 1) 次幂，则位数比 Q 多 1
  finally
    FLocalBigNumberPool.Recycle(N);
  end;
end;

function BigNumberGetTenPrecision2(Num: TCnBigNumber): Integer;
const
  LOG_10_2 = 0.30103;
var
  B: Integer;
begin
  Result := 0;
  if Num.IsZero then
    Exit;

  B := Num.GetBitsCount;
  if B <= 3 then
  begin
    Result := 1;
    Exit;
  end;

  Result := Trunc(LOG_10_2 * B) + 1;
end;

// 确保 Num 内分配的数组长度有 Words 个 Cardinal/UInt64
function BigNumberExpandInternal(Num: TCnBigNumber; Words: Integer): PCnBigNumberElement;
var
  A, B, TmpA: PCnBigNumberElement;
  I: Integer;
  A0, A1, A2, A3: TCnBigNumberElement;
  CopyTop: Integer;
begin
  Result := nil;
  if (Words <= 0) or (Words > (MaxInt div (4 * BN_BITS2))) then
    Exit;

  A := PCnBigNumberElement(GetMemory(SizeOf(TCnBigNumberElement) * Words));
  if A = nil then
    Exit;

  FillChar(A^, SizeOf(TCnBigNumberElement) * Words, 0);

  // 查查是否要复制之前的值
  B := Num.D;
  if B <> nil then
  begin
    if Num.Top > Words then
      CopyTop := Words
    else
      CopyTop := Num.Top;

    TmpA := A;
    I :=  CopyTop shr 2;
    while I > 0 do
    begin
      A0 := PCnBigNumberElementArray(B)^[0];
      A1 := PCnBigNumberElementArray(B)^[1];
      A2 := PCnBigNumberElementArray(B)^[2];
      A3 := PCnBigNumberElementArray(B)^[3];

      PCnBigNumberElementArray(TmpA)^[0] := A0;
      PCnBigNumberElementArray(TmpA)^[1] := A1;
      PCnBigNumberElementArray(TmpA)^[2] := A2;
      PCnBigNumberElementArray(TmpA)^[3] := A3;

      Dec(I);
      TmpA := PCnBigNumberElement(TCnIntAddress(TmpA) + 4 * SizeOf(TCnBigNumberElement));
      B := PCnBigNumberElement(TCnIntAddress(B) + 4 * SizeOf(TCnBigNumberElement));
    end;

    case CopyTop and 3 of
      3:
        begin
          PCnBigNumberElementArray(TmpA)^[2] := PCnBigNumberElementArray(B)^[2];
          PCnBigNumberElementArray(TmpA)^[1] := PCnBigNumberElementArray(B)^[1];
          PCnBigNumberElementArray(TmpA)^[0] := PCnBigNumberElementArray(B)^[0];
        end;
      2:
        begin
          PCnBigNumberElementArray(TmpA)^[1] := PCnBigNumberElementArray(B)^[1];
          PCnBigNumberElementArray(TmpA)^[0] := PCnBigNumberElementArray(B)^[0];
        end;
      1:
        begin
          PCnBigNumberElementArray(TmpA)^[0] := PCnBigNumberElementArray(B)^[0];
        end;
      0:
        begin
          ;
        end;
    end;
  end;

  Result := A;
end;

function BigNumberExpand2(Num: TCnBigNumber; Words: Integer): TCnBigNumber;
var
  P: PCnBigNumberElement;
begin
  Result := nil;
  if Words > Num.DMax then
  begin
    P := BigNumberExpandInternal(Num, Words);
    if P = nil then
      Exit;

    if Num.D <> nil then
      FreeMemory(Num.D);
    Num.D := P;
    Num.DMax := Words;

    Result := Num;
  end;
end;

function BigNumberWordExpand(Num: TCnBigNumber; Words: Integer): TCnBigNumber;
begin
  if Words <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, Words);
end;

function BigNumberExpandBits(Num: TCnBigNumber; Bits: Integer): TCnBigNumber;
begin
  if ((Bits + BN_BITS2 - 1) div BN_BITS2) <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, (Bits + BN_BITS2 - 1) div BN_BITS2);
end;

procedure BigNumberClear(Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  if Num.D <> nil then
    FillChar(Num.D^, Num.DMax * SizeOf(TCnBigNumberElement), 0);
  Num.Top := 0;
  Num.Neg := 0;
end;

// 64 位下也只返回 32 位的
function BigNumberGetWord(Num: TCnBigNumber): Cardinal;
const
  MAX32 = $FFFFFFFF;
{$IFDEF BN_DATA_USE_64}
var
  T: TCnBigNumberElement;
{$ENDIF}
begin
  if Num.Top > 1 then
    Result := MAX32
  else if Num.Top = 1 then
  begin
{$IFDEF BN_DATA_USE_64}
    T := PCnBigNumberElementArray(Num.D)^[0];
    if T > MAX32 then
      Result := MAX32
    else
      Result := Cardinal(T);
{$ELSE}
    Result := PCnBigNumberElementArray(Num.D)^[0];
{$ENDIF}
  end
  else
    Result := 0;
end;

// 64 位下也兼容
function BigNumberSetWord(Num: TCnBigNumber; W: Cardinal): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Cardinal) * 8) = nil then
    Exit;
  Num.Neg := 0;
  PCnBigNumberElementArray(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 1
  else
    Num.Top := 0;
  Result := True;
end;

function BigNumberGetInteger(Num: TCnBigNumber): Integer;
const
  MAX_INT_32 = $7FFFFFFF;
{$IFDEF BN_DATA_USE_64}
var
  T: TCnBigNumberElement;
{$ENDIF}
begin
  if Num.Top > 1 then
    Result := BN_MASK2S
  else if Num.Top = 1 then
  begin
{$IFDEF BN_DATA_USE_64}
    T := PCnBigNumberElementArray(Num.D)^[0];
    if T > MAX_INT_32 then        // UInt64 超出了 Integer 的范围，返回 Max Integer
      Result := MAX_INT_32
    else
      Result := Integer(T);

    if Num.Neg <> 0 then // 负则求反加一
      Result := (not Result) + 1;
{$ELSE}
    Result := Integer(PCnBigNumberElementArray(Num.D)^[0]);
    if Result < 0 then        // UInt32 最高位有值，说明已经超出了 Integer 的范围，返回 Max Integer
      Result := BN_MASK2S
    else if Num.Neg <> 0 then // 负则求反加一
      Result := (not Result) + 1;
{$ENDIF}
  end
  else
    Result := 0;
end;

function BigNumberSetInteger(Num: TCnBigNumber; W: Integer): Boolean;
begin
  if W < 0 then
  begin
    BigNumberSetWord(Num, -W);
    Num.Negate;
  end
  else
    BigNumberSetWord(Num ,W);
  Result := True;
end;

function BigNumberGetInt64(Num: TCnBigNumber): Int64;
begin
  if Num.Top > 2 then
    Result := BN_MASK3S
  else if Num.Top = 2 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := BN_MASK3S;
{$ELSE}
    Result := PInt64Array(Num.D)^[0];
    if Result < 0 then        // UInt64 最高位有值，说明已经超出了 Int64 的范围，返回 Max Int64
      Result := BN_MASK3S
    else if Num.Neg <> 0 then // 负则求反加一
      Result := (not Result) + 1;
{$ENDIF}
  end
  else if Num.Top = 1 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := Int64(PCnBigNumberElementArray(Num.D)^[0]); // UInt64 转为 Int64 如果是负的，表示超界了
    if Result < 0 then
      Result := BN_MASK3S;
{$ELSE}
    Result := Int64(PCnBigNumberElementArray(Num.D)^[0]);
{$ENDIF}
  end
  else
    Result := 0;
end;

function BigNumberSetInt64(Num: TCnBigNumber; W: Int64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Int64) * 8) = nil then
    Exit;

  if W >= 0 then
  begin
    Num.Neg := 0;
    PInt64Array(Num.D)^[0] := W;
    if W = 0 then
      Num.Top := 0
    else
    begin
{$IFDEF BN_DATA_USE_64}
      Num.Top := 1;
{$ELSE}
      if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 Int64 高 32 位是 0
        Num.Top := 1
      else
        Num.Top := 2;
{$ENDIF}
    end;
  end
  else // W < 0
  begin
    Num.Neg := 1;
    W := (not W) + 1;
    PInt64Array(Num.D)^[0] := W;

{$IFDEF BN_DATA_USE_64}
    Num.Top := 1;
{$ELSE}
    if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 Int64 高 32 位是 0
      Num.Top := 1
    else
      Num.Top := 2;
{$ENDIF}
  end;
  Result := True;
end;

function BigNumberGetUInt64UsingInt64(Num: TCnBigNumber): TUInt64;
begin
  if Num.Top > 2 then
    Result := TUInt64(BN_MASK3U)
  else if Num.Top = 2 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := TUInt64(BN_MASK3U);
{$ELSE}
  {$IFDEF SUPPORT_UINT64}
    Result := TUInt64(PInt64Array(Num.D)^[0]);
  {$ELSE}
    Result := PInt64Array(Num.D)^[0]; // 在 D5/6 下 Int64转 Int64 出现 C3517 错误！！！
  {$ENDIF}
{$ENDIF}
  end
  else if Num.Top = 1 then
    Result := TUInt64(PCnBigNumberElementArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetUInt64UsingInt64(Num: TCnBigNumber; W: TUInt64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Int64) * 8) = nil then
    Exit;

  Num.Neg := 0;
  PInt64Array(Num.D)^[0] := Int64(W);
  if W = 0 then
    Num.Top := 0
  else
  begin
{$IFDEF BN_DATA_USE_64}
    Num.Top := 1;
{$ELSE}
    if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 Int64 高 32 位是 0
      Num.Top := 1
    else
      Num.Top := 2;
{$ENDIF}
  end;

  Result := True;
end;

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(Num: TCnBigNumber): UInt64;
begin
  if Num.Top > 2 then
    Result := UInt64(BN_MASK3U)
  else if Num.Top = 2 then
  begin
{$IFDEF BN_DATA_USE_64}
    Result := UInt64(BN_MASK3U);
{$ELSE}
    Result := PUInt64Array(Num.D)^[0];
{$ENDIF}
  end
  else if Num.Top = 1 then // 无论 32 还是 64 都能这样转换
    Result := UInt64(PCnBigNumberElementArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetUInt64(Num: TCnBigNumber; W: UInt64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(UInt64) * 8) = nil then
    Exit;

  Num.Neg := 0;
  PUInt64Array(Num.D)^[0] := W;

  if W = 0 then
    Num.Top := 0
  else
  begin
{$IFDEF BN_DATA_USE_64}
    Num.Top := 1;
{$ELSE}
    if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 UInt64 高 32 位是 0
      Num.Top := 1
    else
      Num.Top := 2;
{$ENDIF}
  end;  

  Result := True;
end;

{$ENDIF}

// 调整 Top 保证 D[Top - 1] 指向最高位非 0 处
procedure BigNumberCorrectTop(Num: TCnBigNumber);
var
  Ftl: PCnBigNumberElement;
  Top: Integer;
begin
 if (Num = nil) then
    Exit;

  if Num.D = nil then
  begin
    Num.Top := 0;
    Exit;
  end
  else if Num.Top = 0 then
    Exit;

  Top := Num.Top;
  if (Top < 0) or (Top > Num.DMax) then
    Top := Num.DMax;

  Ftl := @(PCnBigNumberElementArray(Num.D)^[Top - 1]);
  while Top > 0 do
  begin
    if Ftl^ <> 0 then
      Break;

    Ftl := PCnBigNumberElement(TCnIntAddress(Ftl) - SizeOf(TCnBigNumberElement));
    Dec(Top);
  end;
  Num.Top := Top;
end;

function BigNumberToBinary(Num: TCnBigNumber; Buf: PAnsiChar; FixedLen: Integer): Integer;
var
  I: Integer;
  L: TCnBigNumberElement;
begin
  Result := BigNumberGetBytesCount(Num);
  if Buf = nil then
    Exit;

  if FixedLen > Result then // 要往高处靠
  begin
    I := FixedLen - Result;
    while I > 0 do
    begin
      Dec(I);
      Buf^ := #0;
      Buf := PAnsiChar(TCnIntAddress(Buf) + 1); // 先补 0
    end;
  end;

  I := Result;
  while I > 0 do
  begin
    Dec(I);
    L := PCnBigNumberElementArray(Num.D)^[I div BN_BYTES];
    Buf^ := AnsiChar(Chr(L shr (8 * (I mod BN_BYTES)) and $FF));

    Buf := PAnsiChar(TCnIntAddress(Buf) + 1);
  end;
end;

function BigNumberReadBinaryFromStream(Num: TCnBigNumber; Stream: TStream): Boolean;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    M.LoadFromStream(Stream);
    Result := Num.SetBinary(M.Memory, M.Size);
  finally
    M.Free;
  end;
end;

function BigNumberWriteBinaryToStream(Num: TCnBigNumber; Stream: TStream;
  FixedLen: Integer): Integer;
var
  Buf: TBytes;
  Len: Integer;
begin
  Result := 0;
  Len := BigNumberGetBytesCount(Num);
  if (Stream <> nil) and (Len > 0) then
  begin
    if FixedLen > Len then
    begin
      SetLength(Buf, FixedLen);
      BigNumberToBinary(Num, @Buf[FixedLen - Len]);
      Result := Stream.Write(Buf[0], FixedLen);
    end
    else
    begin
      SetLength(Buf, Len);
      BigNumberToBinary(Num, @Buf[0]);
      Result := Stream.Write(Buf[0], Len);
    end;
    SetLength(Buf, 0);
  end;
end;

function BigNumberFromBinary(Buf: PAnsiChar; ByteLen: Integer): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetBinary(Buf, ByteLen, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberFromBytes(const Buf: TBytes): TCnBigNumber;
begin
  Result := nil;
  if (Buf <> nil) and (Length(Buf) > 0) then
    Result := BigNumberFromBinary(@Buf[0], Length(Buf));
end;

function BigNumberToBytes(Num: TCnBigNumber; FixedLen: Integer): TBytes;
var
  L: Integer;
begin
  Result := nil;
  L := BigNumberGetBytesCount(Num);
  if L > 0 then
  begin
    if L >= FixedLen then
    begin
      SetLength(Result, L);
      BigNumberToBinary(Num, @Result[0]);
    end
    else
    begin
      SetLength(Result, FixedLen);
      BigNumberToBinary(Num, @Result[FixedLen - L]);
    end;
  end;
end;

function BigNumberSetBinary(Buf: PAnsiChar; ByteLen: Integer; Res: TCnBigNumber): Boolean;
var
  I, M, N, L: TCnBigNumberElement;
begin
  Result := False;
  L := 0;
  N := ByteLen;
  if N = 0 then
  begin
    Res.Top := 0;
    Exit;
  end;

  I := ((N - 1) div BN_BYTES) + 1;
  M := (N - 1) mod BN_BYTES;

  if BigNumberWordExpand(Res, I) = nil then
  begin
    BigNumberFree(Res);
    Exit;
  end;

  Res.Top := I;
  Res.Neg := 0;
  while N > 0 do
  begin
    L := (L shl 8) or Ord(Buf^);
    Buf := PAnsiChar(TCnIntAddress(Buf) + 1);  // Buf 是越处理越往高地址走

    if M = 0 then
    begin
      Dec(I);
      PCnBigNumberElementArray(Res.D)^[I] := L; // D 的 I 则是越处理越往低地址走
      L := 0;
      M := BN_BYTES - 1;
    end
    else
      Dec(M);

    Dec(N);
  end;
  BigNumberCorrectTop(Res);
  Result := True;
end;

{$WARNINGS OFF}

function BigNumberToBase64(Num: TCnBigNumber): string;
var
  B: TBytes;
begin
  Result := '';
  if Num <> nil then
  begin
    B := BigNumberToBytes(Num);
    if B <> nil then
      Base64Encode(@B[0], Length(B), Result);
  end;
end;

{$WARNINGS ON}

function BigNumberSetBase64(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
var
  B: TBytes;
begin
  Result := False;
  if Base64Decode(string(Buf), B) = ECN_BASE64_OK then
    Result := BigNumberSetBinary(@B[0], Length(B), Res);
end;

function BigNumberFromBase64(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetBase64(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

procedure BigNumberSetNegative(Num: TCnBigNumber; Negative: Boolean);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Negative then
    Num.Neg := 1
  else
    Num.Neg := 0;
end;

function BigNumberIsNegative(Num: TCnBigNumber): Boolean;
begin
  Result := Num.Neg <> 0;
end;

procedure BigNumberNegate(Num: TCnBigNumber);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Num.Neg <> 0 then
    Num.Neg := 0
  else
    Num.Neg := 1;
end;

function BigNumberClearBit(Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
    Exit;

  PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] and
    TCnBigNumberElement(not (1 shl J));

  BigNumberCorrectTop(Num);
  Result := True;
end;

// 给一个大数对象只保留第 0 到 Count - 1 个 Bit 位，高位清零，返回成功与否
function BigNumberKeepLowBits(Num: TCnBigNumber; Count: Integer): Boolean;
var
  I, J: Integer;
  B: TCnBigNumberElement;
begin
  Result := False;
  if Count < 0 then
    Exit;

  if Count = 0 then
  begin
    Num.SetZero;
    Result := True;
    Exit;
  end;

  I := Count div BN_BITS2;
  J := Count mod BN_BITS2;

  if Num.Top <= I then
  begin
    Result := True;
    Exit;
  end;

  if J > 0 then // 要多保留最高一个 LongWord 中的 0 到 J - 1 位，共 J 位，J 最多 31/63
  begin
    Num.Top := I + 1;
    B := 1 shl J;         // 0000100000 如果 J 是 31/63 也不会溢出
    B := B - 1;           // 0000011111
    PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] and B;
  end
  else
    Num.Top := I; // 如果 J 为 0，无需多最高一个 LongWord 了

  BigNumberCorrectTop(Num);
  Result := True;
end;

function BigNumberSetBit(Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J, K: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
  begin
    if BigNumberWordExpand(Num, I + 1) = nil then
      Exit;

    for K := Num.Top to I do
      PCnBigNumberElementArray(Num.D)^[K] := 0;

    Num.Top := I + 1;
  end;

  PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] or
    TCnBigNumberElement(1 shl J);
  Result := True;
end;

function BigNumberIsBitSet(Num: TCnBigNumber; N: Integer): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if N < 0 then
    Exit;

  I := N div BN_BITS2;
  J := N mod BN_BITS2;

  if Num.Top <= I then
    Exit;

  if (TCnBigNumberElement(PCnBigNumberElementArray(Num.D)^[I] shr J) and TCnBigNumberElement(1)) <> 0 then
    Result := True;
end;

function BigNumberEqual(Num1: TCnBigNumber; Num2: TCnBigNumber): Boolean;
begin
  Result := BigNumberCompare(Num1, Num2) = 0;
end;

function BigNumberEqual(Num1: TCnBigNumber; Num2: Int64): Boolean;
begin
  Result := Num1.IsInt64 and (Num1.GetInt64 = Num2);
end;

function BigNumberCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
var
  I, Gt, Lt: Integer;
  T1, T2: TCnBigNumberElement;
begin
  if Num1 = Num2 then
  begin
    Result := 0;
    Exit;
  end;

  if (Num1 = nil) or (Num2 = nil) then
  begin
    if Num1 <> nil then
      Result := -1
    else if Num2 <> nil then
      Result := 1
    else
      Result := 0;
    Exit;
  end;

  if Num1.Neg <> Num2.Neg then
  begin
    if Num1.Neg <> 0 then
      Result := -1
    else
      Result := 1;
    Exit;
  end;

  if Num1.Neg = 0 then
  begin
    Gt := 1;
    Lt := -1;
  end
  else
  begin
    Gt := -1;
    Lt := 1;
  end;

  if Num1.Top > Num2.Top then
  begin
    Result := Gt;
    Exit;
  end
  else if Num1.Top < Num2.Top then
  begin
    Result := Lt;
    Exit;
  end;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PCnBigNumberElementArray(Num1.D)^[I];
    T2 := PCnBigNumberElementArray(Num2.D)^[I];
    if T1 > T2 then
    begin
      Result := Gt;
      Exit;
    end;
    if T1 < T2 then
    begin
      Result := Lt;
      Exit;
    end;
  end;
  Result := 0;
end;

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Integer): Integer;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInteger(Num2);
    Result := BigNumberCompare(Num1, T);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberCompareInteger(Num1: TCnBigNumber; Num2: Int64): Integer;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInt64(Num2);
    Result := BigNumberCompare(Num1, T);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberUnsignedCompare(Num1: TCnBigNumber; Num2: TCnBigNumber): Integer;
var
  I: Integer;
  T1, T2: TCnBigNumberElement;
begin
  Result := Num1.Top - Num2.Top;
  if Result <> 0 then
    Exit;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PCnBigNumberElementArray(Num1.D)^[I];
    T2 := PCnBigNumberElementArray(Num2.D)^[I];
    if T1 > T2 then
    begin
      Result := 1;
      Exit;
    end;
    if T1 < T2 then
    begin
      Result := -1;
      Exit;
    end;
  end;
  Result := 0;
end;

// 产生固定字节长度的随机大数
function BigNumberRandBytes(Num: TCnBigNumber; BytesCount: Integer): Boolean;
begin
  Result := False;
  if BytesCount < 0 then
    Exit;
  if BytesCount = 0 then
  begin
    Result := BigNumberSetZero(Num);
    Exit;
  end;

  if BigNumberWordExpand(Num, (BytesCount + BN_BYTES - 1) div BN_BYTES) <> nil then
  begin
    // 改用非重复初始化的快速版本，不知道有无副作用？
    Result := CnRandomFillBytes2(PAnsiChar(Num.D), BytesCount);
    if Result then
    begin
      Num.Top := (BytesCount + BN_BYTES - 1) div BN_BYTES;
      BigNumberCorrectTop(Num);
    end;
  end;
end;

// 产生固定位长度的随机大数
function BigNumberRandBits(Num: TCnBigNumber; BitsCount: Integer): Boolean;
var
  C, I: Integer;
begin
  Result := False;
  if BitsCount < 0 then
    Exit;
  if BitsCount = 0 then
  begin
    Result := BigNumberSetZero(Num);
    Exit;
  end;

  // 要产生 N bits 的随机大数，字节计算也就是 (N + 7) div 8 bytes
  C := (BitsCount + 7) div 8;
  if not BigNumberRandBytes(Num, C) then
    Exit;

  // 但头上可能有多余的，再把 C * 8 - 1 到 N 之间的位清零，只留 0 到 N - 1 位
  if BitsCount <= C * 8 - 1 then
  begin
    for I := C * 8 - 1 downto BitsCount do
    begin
      if not BigNumberClearBit(Num, I) then
        Exit;
    end;
  end;

  Result := True;
end;

function BigNumberRandRange(Num: TCnBigNumber; Range: TCnBigNumber): Boolean;
var
  N, C, I: Integer;
begin
  Result := False;
  if (Range = nil) or (Num = nil) or (Range.Neg <> 0) or BigNumberIsZero(Range) then
    Exit;

  N := BigNumberGetBitsCount(Range);
  if N = 1 then
    BigNumberSetZero(Num)
  else
  begin
    // 要产生 N bits 的随机大数，字节计算也就是 (N + 7) div 8 bytes
    C := (N + 7) div 8;
    if not BigNumberRandBytes(Num, C) then
      Exit;

    // 但头上可能有多余的，再把 C * 8 - 1 到 N + 1 之间的位清零
    if N + 1 <= C * 8 - 1 then
      for I := C * 8 - 1 downto N + 1 do
        if BigNumberIsBitSet(Num, I) then
          if not BigNumberClearBit(Num, I) then
            Exit;
    // 加个 IsBitSet 的判断，因为 ClearBit 会判断待 Clear 的位是否超出 Top，
    // 如果生成的位本来就是 0并且已经被 CorrectTop了，那么 ClearBit 会出错。

    while BigNumberCompare(Num, Range) >= 0 do
    begin
      if not BigNumberSub(Num, Num, Range) then
        Exit;
    end;
  end;
  Result := True;
end;

function BigNumberDuplicate(Num: TCnBigNumber): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if BigNumberCopy(Result, Num) = nil then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberCopy(Dest: TCnBigNumber; Source: TCnBigNumber): TCnBigNumber;
var
  I: Integer;
  A, B: PCnBigNumberElementArray;
  A0, A1, A2, A3: TCnBigNumberElement;
begin
  if Dest = Source then
  begin
    Result := Dest;
    Exit;
  end;

  if BigNumberWordExpand(Dest, Source.Top) = nil then
  begin
    Result := nil;
    Exit;
  end;

  A := PCnBigNumberElementArray(Dest.D);
  B := PCnBigNumberElementArray(Source.D);

  for I := (Source.Top shr 2) downto 1 do
  begin
    A0 := B^[0];
    A1 := B^[1];
    A2 := B^[2];
    A3 := B^[3];
    A^[0] := A0;
    A^[1] := A1;
    A^[2] := A2;
    A^[3] := A3;

    A := PCnBigNumberElementArray(TCnIntAddress(A) + 4 * SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElementArray(TCnIntAddress(B) + 4 * SizeOf(TCnBigNumberElement));
  end;

  case Source.Top and 3 of
  3:
    begin
      A[2] := B[2];
      A[1] := B[1];
      A[0] := B[0];
    end;
  2:
    begin
      A[1] := B[1];
      A[0] := B[0];
    end;
  1:
    begin
      A[0] := B[0];
    end;
  0:
    begin

    end;
  end;

  Dest.Top := Source.Top;
  Dest.Neg := Source.Neg;
  Result := Dest;
end;

function BigNumberCopyLow(Dest: TCnBigNumber; Source: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
var
  I: Integer;
  A, B: PCnBigNumberElementArray;
begin
  if WordCount <= 0 then
  begin
    Result := Dest;
    Dest.SetZero;
    Exit;
  end
  else if Source = Dest then // 不支持 Source 和 Dest 相同的情况
    Result := nil
  else
  begin
    if WordCount > Source.GetWordCount then
      WordCount := Source.GetWordCount;

    if BigNumberWordExpand(Dest, WordCount) = nil then
    begin
      Result := nil;
      Exit;
    end;

    A := PCnBigNumberElementArray(Dest.D);
    B := PCnBigNumberElementArray(Source.D);

    Result := Dest;
    for I := 0 to WordCount - 1 do // 从 Source 的 0 到 WordCount - 1 赋值给 Dst 的 0 到 WordCount - 1
      A^[I] := B^[I];

    Dest.Top := WordCount;
    Dest.Neg := Source.Neg;
  end;
end;

function BigNumberCopyHigh(Dest: TCnBigNumber; Source: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
var
  I: Integer;
  A, B: PCnBigNumberElementArray;
begin
  if WordCount <= 0 then
  begin
    Result := Dest;
    Dest.SetZero;
    Exit;
  end
  else if Source = Dest then // 不支持 Source 和 Dest 相同的情况
    Result := nil
  else
  begin
    if WordCount > Source.GetWordCount then
      WordCount := Source.GetWordCount;

    if BigNumberWordExpand(Dest, WordCount) = nil then
    begin
      Result := nil;
      Exit;
    end;

    A := PCnBigNumberElementArray(Dest.D);
    B := PCnBigNumberElementArray(Source.D);

    Result := Dest;
    for I := 0 to WordCount - 1 do // 从 Src 的 Top - WordCount 到 Top - 1 赋值给 Dst 的 0 到 WordCount - 1
      A^[I] := B^[Source.Top - WordCount + I];

    Dest.Top := WordCount;
    Dest.Neg := Source.Neg;
  end;
end;

function BigNumberGetLow32(Num: TCnBigNumber): Cardinal;
begin
  Result := 0;
  if Num.DMax > 0 then
    Result := Cardinal(Num.D^);
end;

function BigNumberGetLow64(Num: TCnBigNumber): TUInt64;
begin
  Result := 0;
{$IFDEF BN_DATA_USE_64}
  if Num.DMax > 0 then
    Result := TUInt64(Num.D^);
{$ELSE}
  if Num.DMax = 1 then
    Result := TUInt64(Num.D^)
  else if Num.DMax >= 2 then
    Result := TUInt64(PCnBigNumberElementArray(Num.D)^[0]) + (TUInt64(PCnBigNumberElementArray(Num.D)^[1]) shl 32);
{$ENDIF}
end;

procedure BigNumberSwap(Num1: TCnBigNumber; Num2: TCnBigNumber);
var
  TmpD: PCnBigNumberElement;
  TmpTop, TmpDMax, TmpNeg: Integer;
begin
  TmpD := Num1.D;
  TmpTop := Num1.Top;
  TmpDMax := Num1.DMax;
  TmpNeg := Num1.Neg;

  Num1.D := Num2.D;
  Num1.Top := Num2.Top;
  Num1.DMax := Num2.DMax;
  Num1.Neg := Num2.Neg;

  Num2.D := TmpD;
  Num2.Top := TmpTop;
  Num2.DMax := TmpDMax;
  Num2.Neg := TmpNeg;
end;

procedure BigNumberSwapBit(Num: TCnBigNumber; BitIndex1, BitIndex2: Integer);
var
  B1, B2: Boolean;
begin
  if (BitIndex1 = BitIndex2) or (BitIndex1 < 0) or (BitIndex2 < 0) then
    Exit;

  if (BitIndex1 >= Num.GetBitsCount) or (BitIndex2 >= Num.GetBitsCount) then
    Exit;

  B1 := Num.IsBitSet(BitIndex1);
  B2 := Num.IsBitSet(BitIndex2);

  if B2 then
    Num.SetBit(BitIndex1)
  else
    Num.ClearBit(BitIndex1);

  if B1 then
    Num.SetBit(BitIndex2)
  else
    Num.ClearBit(BitIndex2);
end;

// ============================ 低阶运算定义开始 ===============================

{$IFDEF BN_DATA_USE_64}

// 计算无符号 64 位 N 的平方，值高低位放入 H 和 L
procedure Sqr(var L: UInt64; var H: UInt64; N: UInt64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  UInt64MulUInt64(N, N, L, H);
end;

// 计算 UInt64 的 A * B + R + C
procedure MulAdd(var R: UInt64; A: UInt64; B: UInt64; var C: UInt64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TCnUInt128;
begin
  UInt64MulUInt64(A, B, T.Lo64, T.Hi64); // 计算 A * B
  UInt128Add(T, T, R);                   // 加上 R
  UInt128Add(T, T, C);                   // 加上 C
  R := T.Lo64;
  C := T.Hi64;
end;

// 计算 UInt64 的 A * B + C
procedure Mul(var R: UInt64; A: UInt64; B: UInt64; var C: UInt64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TCnUInt128;
begin
  UInt64MulUInt64(A, B, T.Lo64, T.Hi64); // 计算 A * B
  UInt128Add(T, T, C);                   // 加上 C
  R := T.Lo64;
  C := T.Hi64;
end;

{$ELSE}

// UInt64 的方式计算 N 平方
procedure Sqr(var L: Cardinal; var H: Cardinal; N: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(N, N);
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  L := Cardinal(T) and BN_MASK2;
  H := Cardinal(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 的方式计算 A * B + R + C
procedure MulAdd(var R: Cardinal; A: Cardinal; B: Cardinal; var C: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + R + C;
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  R := Cardinal(T) and BN_MASK2;
  C := Cardinal(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 的方式计算 A * B + C
procedure Mul(var R: Cardinal; A: Cardinal; B: Cardinal; var C: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + C;
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  R := Cardinal(T) and BN_MASK2;
  C := Cardinal(T shr BN_BITS2) and BN_MASK2;
end;

{$ENDIF}

// N 个 Cardinal 长的数组内容进行位运算，如果 BP 为 nil，表示不够长，作为 0 处理
procedure BigNumberBitOperation(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer; Op: TCnBitOperation);
begin
  if N <= 0 then
    Exit;

  if BP <> nil then
  begin
    while (N and (not 3)) <> 0 do
    begin
      case Op of
        boAnd:
          begin
            RP^[0] := TCnBigNumberElement((Int64(AP^[0]) and Int64(BP^[0])) and BN_MASK2);
            RP^[1] := TCnBigNumberElement((Int64(AP^[1]) and Int64(BP^[1])) and BN_MASK2);
            RP^[2] := TCnBigNumberElement((Int64(AP^[2]) and Int64(BP^[2])) and BN_MASK2);
            RP^[3] := TCnBigNumberElement((Int64(AP^[3]) and Int64(BP^[3])) and BN_MASK2);
          end;
        boOr:
          begin
            RP^[0] := TCnBigNumberElement((Int64(AP^[0]) or Int64(BP^[0])) and BN_MASK2);
            RP^[1] := TCnBigNumberElement((Int64(AP^[1]) or Int64(BP^[1])) and BN_MASK2);
            RP^[2] := TCnBigNumberElement((Int64(AP^[2]) or Int64(BP^[2])) and BN_MASK2);
            RP^[3] := TCnBigNumberElement((Int64(AP^[3]) or Int64(BP^[3])) and BN_MASK2);
          end;
        boXor:
          begin
            RP^[0] := TCnBigNumberElement((Int64(AP^[0]) xor Int64(BP^[0])) and BN_MASK2);
            RP^[1] := TCnBigNumberElement((Int64(AP^[1]) xor Int64(BP^[1])) and BN_MASK2);
            RP^[2] := TCnBigNumberElement((Int64(AP^[2]) xor Int64(BP^[2])) and BN_MASK2);
            RP^[3] := TCnBigNumberElement((Int64(AP^[3]) xor Int64(BP^[3])) and BN_MASK2);
          end;
      end;

      AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
      BP := PCnBigNumberElementArray(TCnIntAddress(BP) + 4 * SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

      Dec(N, 4);
    end;

    while N <> 0 do
    begin
      case Op of
        boAnd:
          RP^[0] := TCnBigNumberElement((Int64(AP^[0]) and Int64(BP^[0])) and BN_MASK2);
        boOr:
          RP^[0] := TCnBigNumberElement((Int64(AP^[0]) or Int64(BP^[0])) and BN_MASK2);
        boXor:
          RP^[0] := TCnBigNumberElement((Int64(AP^[0]) xor Int64(BP^[0])) and BN_MASK2);
      end;

      AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      BP := PCnBigNumberElementArray(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
      Dec(N);
    end;
  end
  else // BP 为 nil，代表数组不够长，当成 0 处理
  begin
    if Op = boAnd then
      FillChar(RP[0], N * SizeOf(TCnBigNumberElement), 0)
    else if Op in [boOr, boXor] then
      Move(AP[0], RP[0], N * SizeOf(TCnBigNumberElement));
  end;
end;

// ============================ 低阶运算定义结束 ===============================

{* Words 系列内部计算函数开始}

procedure BigNumberAndWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boAnd);
end;

procedure BigNumberOrWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boOr);
end;

procedure BigNumberXorWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray; BP: PCnBigNumberElementArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boXor);
end;

function BigNumberAddWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  BP: PCnBigNumberElementArray; N: Integer): Cardinal;
var
{$IFDEF BN_DATA_USE_64}
  LL: TCnUInt128;
{$ELSE}
  LL: Int64;
{$ENDIF}
begin
  Result := 0;
  if N <= 0 then
    Exit;

{$IFDEF BN_DATA_USE_64}
  UInt128SetZero(LL);
{$ELSE}
  LL := 0;
{$ENDIF}

  while (N and (not 3)) <> 0 do
  begin
{$IFDEF BN_DATA_USE_64}
    UInt128Add(LL, LL, AP^[0]);
    UInt128Add(LL, LL, BP^[0]);
    RP^[0] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;

    UInt128Add(LL, LL, AP^[1]);
    UInt128Add(LL, LL, BP^[1]);
    RP^[1] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;

    UInt128Add(LL, LL, AP^[2]);
    UInt128Add(LL, LL, BP^[2]);
    RP^[2] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;

    UInt128Add(LL, LL, AP^[3]);
    UInt128Add(LL, LL, BP^[3]);
    RP^[3] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;
{$ELSE}
    LL := LL + Int64(AP^[0]) + Int64(BP^[0]);
    RP^[0] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP^[1]) + Int64(BP^[1]);
    RP^[1] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP^[2]) + Int64(BP^[2]);
    RP^[2] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP^[3]) + Int64(BP^[3]);
    RP^[3] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;
{$ENDIF}

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
{$IFDEF BN_DATA_USE_64}
    UInt128Add(LL, LL, AP^[0]);
    UInt128Add(LL, LL, BP^[0]);
    RP^[0] := LL.Lo64;
    LL.Lo64 := LL.Hi64;
    LL.Hi64 := 0;
{$ELSE}
    LL := LL + Int64(AP^[0]) + Int64(BP^[0]);
    RP^[0] := Cardinal(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;
{$ENDIF}

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    Dec(N);
  end;

{$IFDEF BN_DATA_USE_64}
  Result := LL.Lo64;
{$ELSE}
  Result := Cardinal(LL);
{$ENDIF}
end;

function BigNumberSubWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray; BP: PCnBigNumberElementArray; N: Integer): Cardinal;
var
  T1, T2, C: TCnBigNumberElement;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  C := 0;
  while (N and (not 3)) <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[1];
    T2 := BP^[1];
    RP^[1] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[2];
    T2 := BP^[2];
    RP^[2] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    T1 := AP^[3];
    T2 := BP^[3];
    RP^[3] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElementArray(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    Dec(N);
  end;
  Result := C;
end;

function BigNumberMulAddWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  N: Integer; W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    MulAdd(RP^[1], AP^[1], W, Result);
    MulAdd(RP^[2], AP^[2], W, Result);
    MulAdd(RP^[3], AP^[3], W, Result);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    Dec(N);
  end;
end;

// AP 指向的 N 个数字都乘以 W，结果的低 N 位放 RP 中，高位放返回值
function BigNumberMulWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray;
  N: Integer; W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);
    Mul(RP^[1], AP^[1], W, Result);
    Mul(RP^[2], AP^[2], W, Result);
    Mul(RP^[3], AP^[3], W, Result);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 4 * SizeOf(TCnBigNumberElement));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

    Dec(N);
  end;
end;

procedure BigNumberSqrWords(RP: PCnBigNumberElementArray; AP: PCnBigNumberElementArray; N: Integer);
begin
  if N = 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    Sqr(RP^[2], RP^[3], AP^[1]);
    Sqr(RP^[4], RP^[5], AP^[2]);
    Sqr(RP^[6], RP^[7], AP^[3]);

    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + 4 * SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 8 * SizeOf(TCnBigNumberElement));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 2 * SizeOf(TCnBigNumberElement));
    Dec(N);
  end;
end;

{$IFDEF BN_DATA_USE_64}

// 128 位被除数整除 64 位除数，返回商，Result := H L div D，不管商的高 64 位
// 因此要保证 D 的最高位为 1，商的高 64 位才会为 0，此函数调用才不会出错
function InternalDivWords64(H: UInt64; L: UInt64; D: UInt64): UInt64;
var
  R: UInt64;
begin
  UInt128DivUInt64Mod(L, H, D, Result, R);
end;

{$ENDIF}

// 64 位被除数整除 32 位除数，返回商，Result := H L div D，不管商的高 32 位
// 因此要保证 D 的最高位为 1，商的高 32 位才会为 0，此函数调用才不会出错，所以 32 位下才可以用 DIV 指令优化
function InternalDivWords(H: Cardinal; L: Cardinal; D: Cardinal): Cardinal;
begin
  if D = 0 then
  begin
    Result := BN_MASK2;
    Exit;
  end;

{$IFDEF SUPPORT_UINT64}
  Result := Cardinal(((UInt64(H) shl 32) or UInt64(L)) div UInt64(D));
{$ELSE}
  Result := 0;
  asm
    MOV EAX, L
    MOV EDX, H
    DIV ECX       // DIV 貌似等于 DIVL，这段优化比下面调 _lludiv 的耗时少了 20%
    MOV Result, EAX
  end;
{$ENDIF}
end;

{* Words 系列内部计算函数结束}

function BigNumberAnd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  BigNumberAndWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  // AP 长的后头还有 Dif 一段没有处理，需要当成和 0 一块运算
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberAndWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberOr(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  BigNumberOrWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  // AP 长的后头还有 Dif 一段没有处理，需要当成和 0 一块运算
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberOrWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberXor(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  BigNumberXorWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  // AP 长的后头还有 Dif 一段没有处理，需要当成和 0 一块运算
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberXorWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberUnsignedAdd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PCnBigNumberElement;
  Carry, T1, T2: TCnBigNumberElement;
  A, B, Tmp: TCnBigNumber;
begin
  Result := False;

  A := Num1;
  B := Num2;
  if A.Top < B.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A.Top;
  Min := B.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max + 1) = nil then
    Exit;

  Res.Top := Max;
  AP := PCnBigNumberElement(A.D);
  BP := PCnBigNumberElement(B.D);
  RP := PCnBigNumberElement(Res.D);

  Carry := BigNumberAddWords(PCnBigNumberElementArray(RP), PCnBigNumberElementArray(AP), PCnBigNumberElementArray(BP), Min);

  AP := PCnBigNumberElement(TCnIntAddress(AP) + Min * SizeOf(TCnBigNumberElement));
  RP := PCnBigNumberElement(TCnIntAddress(RP) + Min * SizeOf(TCnBigNumberElement));

  if Carry <> 0 then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      T2 := (T1 + 1) and BN_MASK2;

      RP^ := T2;
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if T2 <> 0 then
      begin
        Carry := 0;
        Break;
      end;
    end;

    if Carry <> 0 then
    begin
      RP^ := 1;
      Inc(Res.Top);
    end;
  end;

  if (Dif <> 0) and (RP <> AP) then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    end;
  end;

  Res.Neg := 0;
  Result := True;
end;

function BigNumberUnsignedSub(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif, I: Integer;
  AP, BP, RP: PCnBigNumberElement;
  Carry, T1, T2: TCnBigNumberElement;
begin
  Result := False;

  Max := Num1.Top;
  Min := Num2.Top;
  Dif := Max - Min;

  if Dif < 0 then
    Exit;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  AP := PCnBigNumberElement(Num1.D);
  BP := PCnBigNumberElement(Num2.D);
  RP := PCnBigNumberElement(Res.D);

  Carry := 0;
  for I := Min downto 1 do
  begin
    T1 := AP^;
    T2 := BP^;
    AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    BP := PCnBigNumberElement(TCnIntAddress(BP) + SizeOf(TCnBigNumberElement));
    if Carry <> 0 then
    begin
      if T1 <= T2 then
        Carry := 1
      else
        Carry := 0;
      T1 := (T1 - T2 - 1) and BN_MASK2;
    end
    else
    begin
      if T1 < T2 then
        Carry := 1
      else
        Carry := 0;
      T1 := (T1 - T2) and BN_MASK2;
    end;
    RP^ := T1 and BN_MASK2;
    RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
  end;

  if Carry <> 0 then
  begin
    if Dif = 0 then  // Error! Num1 < Num2
      Exit;

    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      T2 := (T1 - 1) and BN_MASK2;

      RP^ := T2;
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
      if T1 <> 0 then
        Break;
    end;
  end;

  if RP <> AP then
  begin
    while True do
    begin
      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
      RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
    end;
  end;

  Res.Top := Max;
  Res.Neg := 0;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberAdd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: TCnBigNumber;
  Neg: Integer;
begin
  Result := False;

  Neg := Num1.Neg;
  A := Num1;
  B := Num2;

  if Neg <> Num2.Neg then // One is negative
  begin
    if Neg <> 0 then
    begin
      Tmp := A;
      A := B;
      B := Tmp;
    end;

    // A is positive and B is negative
    if BigNumberUnsignedCompare(A, B) < 0 then
    begin
      if not BigNumberUnsignedSub(Res, B, A) then
        Exit;
      Res.Neg := 1;
    end
    else
    begin
      if not BigNumberUnsignedSub(Res, A, B) then
        Exit;
      Res.Neg := 0;
    end;
    Result := True;
    Exit;
  end;

  Result := BigNumberUnsignedAdd(Res, A, B);
  Res.Neg := Neg;
end;

function BigNumberSub(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: TCnBigNumber;
  Max, Add, Neg: Integer;
begin
  Result := False;
  Add := 0;
  Neg := 0;
  A := Num1;
  B := Num2;

  if A.Neg <> 0 then
  begin
    if B.Neg <> 0 then
    begin
      Tmp := A;
      A := B;
      B := Tmp;
    end
    else // A Negative B Positive
    begin
      Add := 1;
      Neg := 1;
    end;
  end
  else
  begin
    if B.Neg <> 0 then // A Positive B Negative
    begin
      Add := 1;
      Neg := 0;
    end;
  end;

  if Add = 1 then
  begin
    if not BigNumberUnsignedAdd(Res, A, B) then
      Exit;

    Res.Neg := Neg;
    Result := True;
    Exit;
  end;

  if A.Top > B.Top then
    Max := A.Top
  else
    Max := B.Top;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  if BigNumberUnsignedCompare(A, B) < 0 then
  begin
    if not BigNumberUnsignedSub(Res, B, A) then
      Exit;
    Res.Neg := 1;
  end
  else
  begin
    if not BigNumberUnsignedSub(Res, A, B) then
      Exit;
    Res.Neg := 0;
  end;
  Result := True;
end;

function BigNumberShiftLeftOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  RP, AP: PCnBigNumberElement;
  I: Integer;
  T, C: TCnBigNumberElement;
begin
  Result := False;

  if Res <> Num then
  begin
    Res.Neg := Num.Neg;
    if BigNumberWordExpand(Res, Num.Top + 1) = nil then
      Exit;

    Res.Top := Num.Top;
  end
  else
  begin
    if BigNumberWordExpand(Res, Num.Top + 1) = nil then
      Exit;
  end;

  AP := Num.D;
  RP := Res.D;
  C := 0;
  for I := 0 to Num.Top - 1 do
  begin
    T := AP^;
    AP := PCnBigNumberElement(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP^ := ((T shl 1) or C) and BN_MASK2;
    RP := PCnBigNumberElement(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));

    if (T and BN_TBIT) <> 0 then
      C := 1
    else
      C := 0;
  end;

  if C <> 0 then
  begin
    RP^ := 1;
    Inc(Res.Top);
  end;
  Result := True;
end;

function BigNumberShiftRightOne(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  RP, AP: PCnBigNumberElement;
  I, J: Integer;
  T, C: TCnBigNumberElement;
begin
  Result := False;
  if BigNumberIsZero(Num) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  I := Num.Top;
  AP := Num.D;

  if PCnBigNumberElementArray(AP)^[I - 1] = 1 then
    J := I - 1
  else
    J := I;

  if Res <> Num then
  begin
    if BigNumberWordExpand(Res, J) = nil then
      Exit;
    Res.Neg := Num.Neg;
  end;

  RP := Res.D;
  Dec(I);
  T := PCnBigNumberElementArray(AP)^[I];

  if (T and 1) <> 0 then
    C := BN_TBIT
  else
    C := 0;

  T := T shr 1;
  if T <> 0 then
    PCnBigNumberElementArray(RP)^[I] := T;

  while I > 0 do
  begin
    Dec(I);
    T := PCnBigNumberElementArray(AP)^[I];
    PCnBigNumberElementArray(RP)^[I] := ((T shr 1) and BN_MASK2) or C;

    if (T and 1) <> 0 then
      C := BN_TBIT
    else
      C := 0;
  end;

  Res.Top := J;
  Result := True;
end;

function BigNumberShiftLeft(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, NW, LB, RB: Integer;
  L: TCnBigNumberElement;
  T, F: PCnBigNumberElementArray;
begin
  if N < 0 then
  begin
    Result := BigNumberShiftRight(Res, Num, -N);
    Exit;
  end;

  Result := False;
  Res.Neg := Num.Neg;
  NW := N div BN_BITS2;

  if BigNumberWordExpand(Res, Num.Top + NW + 1) = nil then
    Exit;

  LB := N mod BN_BITS2;
  RB := BN_BITS2 - LB;

  F := PCnBigNumberElementArray(Num.D);
  T := PCnBigNumberElementArray(Res.D);

  T^[Num.Top + NW] := 0;
  if LB = 0 then
  begin
    for I := Num.Top - 1 downto 0 do
      T^[NW + I] := F^[I];
  end
  else
  begin
    for I := Num.Top - 1 downto 0 do
    begin
      L := F^[I];
      T^[NW + I + 1] := T^[NW + I + 1] or ((L shr RB) and BN_MASK2);
      T^[NW + I] := (L shl LB) and BN_MASK2;
    end;
  end;

  FillChar(Pointer(T)^, NW * SizeOf(TCnBigNumberElement), 0);
  Res.Top := Num.Top + NW + 1;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberShiftRight(Res: TCnBigNumber; Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, J, NW, LB, RB: Integer;
  L, Tmp: TCnBigNumberElement;
  T, F: PCnBigNumberElementArray;
begin
  if N < 0 then
  begin
    Result := BigNumberShiftLeft(Res, Num, -N);
    Exit;
  end;

  Result := False;
  NW := N div BN_BITS2;
  RB := N mod BN_BITS2;
  LB := BN_BITS2 - RB;

  if (NW >= Num.Top) or (Num.Top = 0) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  I := (BigNumberGetBitsCount(Num) - N + (BN_BITS2 - 1)) div BN_BITS2;
  if Res <> Num then
  begin
    Res.Neg := Num.Neg;
    if BigNumberWordExpand(Res, I) = nil then
      Exit;
  end
  else
  begin
    if N = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;

  F := PCnBigNumberElementArray(TCnIntAddress(Num.D) + NW * SizeOf(TCnBigNumberElement));
  T := PCnBigNumberElementArray(Res.D);
  J := Num.Top - NW;
  Res.Top := I;

  if RB = 0 then
  begin
    for I := J downto 1 do
    begin
      T^[0] := F^[0];
      F := PCnBigNumberElementArray(TCnIntAddress(F) + SizeOf(TCnBigNumberElement));
      T := PCnBigNumberElementArray(TCnIntAddress(T) + SizeOf(TCnBigNumberElement));
    end;
  end
  else
  begin
    L := F^[0];
    F := PCnBigNumberElementArray(TCnIntAddress(F) + SizeOf(TCnBigNumberElement));
    for I := J - 1 downto 1 do
    begin
      Tmp := (L shr RB) and BN_MASK2;
      L := F^[0];
      T^[0] := (Tmp or (L shl LB)) and BN_MASK2;

      F := PCnBigNumberElementArray(TCnIntAddress(F) + SizeOf(TCnBigNumberElement));
      T := PCnBigNumberElementArray(TCnIntAddress(T) + SizeOf(TCnBigNumberElement));
    end;

    L := (L shr RB) and BN_MASK2;
    if L <> 0 then
      T^[0] := L;
  end;
  Result := True;
end;

{* 大数与 Word 运算系列函数开始}

// 某大数是否等于指定 UInt32/UInt64
function BigNumberIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
begin
  Result := False;
  if (W = 0) or (Num.Neg = 0) then
    if BigNumberAbsIsWord(Num, W) then
      Result := True;
end;

// 返回一个大数结构里的大数的绝对值是否为指定的 UInt32/UInt64 值
function BigNumberAbsIsWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
begin
  Result := True;
  if (W = 0) and (Num.Top = 0) then
    Exit;
  if (Num.Top = 1) and (PCnBigNumberElementArray(Num.D)^[0] = W) then // UInt64 和 Cardinal 都适用
    Exit;
  Result := False;
end;

function BigNumberAddWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
var
  I: Integer;
  L: TCnBigNumberElement;
begin
  Result := False;

  if W = 0 then
  begin
    Result := True;
    Exit;
  end;

  if BigNumberIsZero(Num) then
  begin
    Result := BigNumberSetWord(Num, W);
    Exit;
  end;

  if Num.Neg <> 0 then // 负就用减法
  begin
    Num.Neg := 0;
    Result := BigNumberSubWord(Num, W);
    if not BigNumberIsZero(Num) then
      Num.Neg := 1 - Num.Neg;
    Exit;
  end;

  I := 0;
  while (W <> 0) and (I < Num.Top) do
  begin
    L := (PCnBigNumberElementArray(Num.D)^[I] + W) and BN_MASK2;
    PCnBigNumberElementArray(Num.D)^[I] := L;
    if W > L then // 结果比加数小，说明溢出或者进位了，把进位置给 W，继续加
      W := 1
    else
      W := 0;
    Inc(I);
  end;

  if (W <> 0) and (I = Num.Top) then // 如果进位竟然超过了最高位
  begin
    if BigNumberWordExpand(Num, Num.Top + 1) = nil then
      Exit;
    Inc(Num.Top);
    PCnBigNumberElementArray(Num.D)^[I] := W;
  end;
  Result := True;
end;

function BigNumberSubWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
var
  I: Integer;
begin
  if W = 0 then
  begin
    Result := True;
    Exit;
  end;

  if BigNumberIsZero(Num) then
  begin
    Result := BigNumberSetWord(Num, W);
    if Result then
      BigNumberSetNegative(Num, True);
    Exit;
  end;

  if Num.Neg <> 0 then
  begin
    Num.Neg := 0;
    Result := BigNumberAddWord(Num, W);
    Num.Neg := 1;
    Exit;
  end;

  if (Num.Top = 1) and (PCnBigNumberElementArray(Num.D)^[0] < W) then // 不够减
  begin
    PCnBigNumberElementArray(Num.D)^[0] := W - PCnBigNumberElementArray(Num.D)^[0];
    Num.Neg := 1;
    Result := True;
    Exit;
  end;

  I := 0;
  while True do
  begin
    if PCnBigNumberElementArray(Num.D)^[I] >= W then // 够减直接减
    begin
      PCnBigNumberElementArray(Num.D)^[I] := PCnBigNumberElementArray(Num.D)^[I] - W;
      Break;
    end
    else
    begin
      PCnBigNumberElementArray(Num.D)^[I] := (PCnBigNumberElementArray(Num.D)^[I] - W) and BN_MASK2;
      Inc(I);
      W := 1;  // 不够减有借位
    end;
  end;

  if (PCnBigNumberElementArray(Num.D)^[I] = 0) and (I = Num.Top - 1) then
    Dec(Num.Top);
  Result := True;
end;

function BigNumberMulWord(Num: TCnBigNumber; W: TCnBigNumberElement): Boolean;
var
  L: TCnBigNumberElement;
begin
  Result := False;

  if Num.Top <> 0 then
  begin
    if W = 0 then
      BigNumberSetZero(Num)
    else
    begin
      L := BigNumberMulWords(PCnBigNumberElementArray(Num.D), PCnBigNumberElementArray(Num.D), Num.Top, W);
      if L <> 0 then
      begin
        if BigNumberWordExpand(Num, Num.Top + 1) = nil then
          Exit;
        PCnBigNumberElementArray(Num.D)^[Num.Top] := L;
        Inc(Num.Top);
      end;
    end;
  end;
  Result := True;
end;

function BigNumberModWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
var
  I: Integer;
{$IFDEF BN_DATA_USE_64}
  T: TCnBigNumberElement;
{$ENDIF}
begin
  if W = 0 then
    raise EDivByZero.Create(SDivByZero);

{$IFDEF BN_DATA_USE_64}
  if W > $FFFFFFFF then
    raise ECnBigNumberException.Create(SCnErrorBigNumberInvalid64ModRange);
{$ENDIF}

  Result := 0;
  for I := Num.Top - 1 downto 0 do
  begin
{$IFDEF BN_DATA_USE_64}
    Result := ((Result shl BN_BITS4) or ((PCnBigNumberElementArray(Num.D)^[I] shr BN_BITS4) and BN_MASK2l)) mod W;
    Result := ((Result shl BN_BITS4) or (PCnBigNumberElementArray(Num.D)^[I] and BN_MASK2l)) mod W;
{$ELSE}
    // 32 位下扩展过去做 UInt64 求余，逐级把上一级的余数作为下一级的高 64 位和下一级拼一块再除求余
    Result := UInt64Mod((TUInt64(Result) shl BN_BITS2) or TUInt64(PCnBigNumberElementArray(Num.D)^[I]), W);
{$ENDIF}
  end;
end;

function BigNumberDivWord(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
var
  I, J: Integer;
  L, D: TCnBigNumberElement;
begin
  if W = 0 then
    raise EDivByZero.Create(SDivByZero);;

  Result := 0;
  if Num.Top = 0 then
    Exit;

  J := BN_BITS2 - BigNumberGetWordBitsCount(W);

  W := W shl J; // 保证 W 最高位为 1
  if not BigNumberShiftLeft(Num, Num, J) then
  begin
    Result := TCnBigNumberElement(-1);
    Exit;
  end;

  for I := Num.Top - 1 downto 0 do
  begin
    L := PCnBigNumberElementArray(Num.D)^[I];
{$IFDEF BN_DATA_USE_64}
    D := InternalDivWords64(Result, L, W); // W 保证了最高位为 1，结果才是 64 位
{$ELSE}
    D := InternalDivWords(Result, L, W);   // W 保证了最高位为 1，结果才是 32 位
{$ENDIF}
    Result := (L - ((D * W) and BN_MASK2)) and BN_MASK2;

    PCnBigNumberElementArray(Num.D)^[I] := D;
  end;

  if (Num.Top > 0) and (PCnBigNumberElementArray(Num.D)^[Num.Top - 1] = 0) then
    Dec(Num.Top);
  Result := Result shr J;
end;

procedure BigNumberAndWord(Num: TCnBigNumber; W: TCnBigNumberElement);
begin
  if Num.Top >= 1 then
  begin
    PCnBigNumberElementArray(Num.D)^[0] := PCnBigNumberElementArray(Num.D)^[0] and W;
    if PCnBigNumberElementArray(Num.D)^[0] <> 0 then // 32/64 位以上的都是 0
      Num.Top := 1
    else
      Num.Top := 0;
  end;
end;

procedure BigNumberOrWord(Num: TCnBigNumber; W: TCnBigNumberElement);
begin
  if Num.Top > 0 then
    PCnBigNumberElementArray(Num.D)^[0] := PCnBigNumberElementArray(Num.D)^[0] and W
  else
    Num.SetWord(W);
end;

procedure BigNumberXorWord(Num: TCnBigNumber; W: TCnBigNumberElement);
begin
  if Num.Top > 0 then // 32/64 位以上的 xor 0，都不变
    PCnBigNumberElementArray(Num.D)^[0] := PCnBigNumberElementArray(Num.D)^[0] xor W
  else
    Num.SetWord(W); // 0 异或 W 等于 W
end;

function BigNumberAndWordTo(Num: TCnBigNumber; W: TCnBigNumberElement): TCnBigNumberElement;
begin
  if Num.Top >= 1 then
    Result := PCnBigNumberElementArray(Num.D)^[0] and W
  else
    Result := 0;
end;

{* 大数与 Word 运算系列函数结束}

function BigNumberToString(Num: TCnBigNumber): string;
var
  I, J, V, Z: Integer;
begin
  Result := '';
  if BigNumberIsZero(Num) then
  begin
    Result := '0';
    Exit;
  end;
  if BigNumberIsNegative(Num) then
    Result := '-';

  Z := 0;
  for I := Num.Top - 1 downto 0 do
  begin
    J := BN_BITS2 - 4;
    while J >= 0 do
    begin
      V := ((PCnBigNumberElementArray(Num.D)^[I]) shr Cardinal(J)) and $0F;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[V + 1];
        Z := 1;
      end;
      Dec(J, 4);
    end;
  end;
end;

function BigNumberToHex(Num: TCnBigNumber; FixedLen: Integer): string;
var
  I, J, V, Z: Integer;
begin
  Result := '';
  if BigNumberIsZero(Num) then
  begin
    if FixedLen <= 0 then
      Result := '0'
    else
      Result := StringOfChar('0', FixedLen * 2);
    Exit;
  end;

  Z := 0;
  for I := Num.Top - 1 downto 0 do
  begin
    J := BN_BITS2 - 8;
    while J >= 0 do
    begin
      V := ((PCnBigNumberElementArray(Num.D)^[I]) shr Cardinal(J)) and $FF;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[(V shr 4) + 1];
        Result := Result + Hex[(V and $0F) + 1];
        Z := 1;
      end;
      Dec(J, 8);
    end;
  end;

  if FixedLen * 2 > Length(Result) then
    Result := StringOfChar('0', FixedLen * 2 - Length(Result)) + Result;

  if BigNumberIsNegative(Num) then
    Result := '-' + Result;
end;

function BigNumberSetHex(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, H, M, J, I, K, C: Integer;
  L: TCnBigNumberElement;
begin
  Result := False;
  if (Buf = '') or (Res = nil) then
    Exit;

  P := @Buf[1];
  if P^ = '-' then
  begin
    Neg := 1;
    Inc(P);
  end
  else
    Neg := 0;

  // 求有效长度，一个字母数字占 4 位
  I := 0;
  while PAnsiChar(TCnIntAddress(P) + I)^ in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberExpandBits(Res, (I + 2) * 4) = nil then // 多分配一点
  begin
    BigNumberFree(Res);
    Exit;
  end;

  J := I;
  H := 0;
  while J > 0 do
  begin
    L := 0;
    if BN_BYTES * 2 <= J then
      M := BN_BYTES * 2
    else
      M := J;

    while True do
    begin
      C := Ord(PAnsiChar(TCnIntAddress(P) + J - M)^);
      if (C >= Ord('0')) and (C <= Ord('9')) then
        K := C - Ord('0')
      else if (C >= Ord('a')) and (C <= Ord('f')) then
        K := C - Ord('a') + 10
      else if (C >= Ord('A')) and (C <= Ord('F')) then
        K := C - Ord('A') + 10
      else
        K := 0;

      L := (L shl 4) or TCnBigNumberElement(K);

      Dec(M);
      if M <= 0 then
      begin
        PCnBigNumberElementArray(Res.D)^[H] := L;
        Inc(H);
        Break;
      end;
    end;
    Dec(J, BN_BYTES * 2);
  end;

  Res.Top := H;
  BigNumberCorrectTop(Res);
  Res.Neg := Neg;
  Result := True;
end;

function BigNumberFromHex(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetHex(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberToDec(Num: TCnBigNumber): AnsiString;
var
  I, N, R, Len: Integer;
  BnData, LP: PCnBigNumberElement;
  T: TCnBigNumber;
  P: PAnsiChar;

  function BufRemain(Nu: Integer; Pt: PAnsiChar; Res: PAnsiChar): Integer;
  begin
    Result := Nu + 3 - (TCnIntAddress(Pt) - TCnIntAddress(Res));
  end;

begin
  Result := '';

  I := BigNumberGetBitsCount(Num) * 3;
  N := ((I div 10) + (I div 1000) + 1) + 1;

  BnData := nil;
  T := nil;
  try
    BnData := PCnBigNumberElement(GetMemory(((N div 9) + 1) * SizeOf(TCnBigNumberElement)));
    if BnData = nil then
      Exit;

    SetLength(Result, N + 3);
    FillChar(Result[1], Length(Result), 0);

    T := FLocalBigNumberPool.Obtain;
    if BigNumberCopy(T, Num) = nil then
      Exit;

    P := @(Result[1]);
    LP := BnData;

    if BigNumberIsZero(T) then
    begin
      P^ := '0';
      Inc(P);
      P^ := Chr(0);
    end
    else
    begin
      if BigNumberIsNegative(T) then
      begin
        P^ := '-';
        Inc(P);
      end;

      while not BigNumberIsZero(T) do
      begin
        LP^ := BigNumberDivWord(T, BN_DEC_CONV);
        LP := PCnBigNumberElement(TCnIntAddress(LP) + SizeOf(TCnBigNumberElement));
      end;
      LP := PCnBigNumberElement(TCnIntAddress(LP) - SizeOf(TCnBigNumberElement));

      R := BufRemain(N, P, @(Result[1]));
{$IFDEF UNICODE}
      AnsiStrings.AnsiFormatBuf(P^, R, AnsiString(BN_DEC_FMT), Length(BN_DEC_FMT), [LP^]);
{$ELSE}
      FormatBuf(P^, R, BN_DEC_FMT, Length(BN_DEC_FMT), [LP^]);
{$ENDIF}
      while P^ <> #0 do
        Inc(P);
      while LP <> BnData do
      begin
        LP := PCnBigNumberElement(TCnIntAddress(LP) - SizeOf(TCnBigNumberElement));
        R := BufRemain(N, P, @(Result[1]));
{$IFDEF UNICODE}
        AnsiStrings.AnsiFormatBuf(P^, R, AnsiString(BN_DEC_FMT2), Length(BN_DEC_FMT2), [LP^]);
{$ELSE}
        FormatBuf(P^, R, BN_DEC_FMT2, Length(BN_DEC_FMT2), [LP^]);
{$ENDIF}
        while P^ <> #0 do
          Inc(P);
      end;
    end;
  finally
    if BnData <> nil then
      FreeMemory(BnData);

    FLocalBigNumberPool.Recycle(T);
  end;

  Len := SysUtils.StrLen(PAnsiChar(Result));
  if Len >= 0 then
    SetLength(Result, Len); // 去除尾部多余的 #0
end;

function BigNumberSetDec(const Buf: AnsiString; Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, J, I: Integer;
  L: TCnBigNumberElement;
begin
  Result := False;
  if (Buf = '') or (Res = nil) then
    Exit;

  P := @Buf[1];
  if P^ = '-' then
  begin
    Neg := 1;
    Inc(P);
  end
  else
    Neg := 0;

  // 求有效长度
  I := 0;
  while PAnsiChar(TCnIntAddress(P) + I)^ in ['0'..'9'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberExpandBits(Res, (I + 2) * 4) = nil then // 一位十进制数少于 4 位，这里多扩展一点点
  begin
    BigNumberFree(Res);
    Exit;
  end;

  J := 9 - (I mod 9);
  if J = 9 then
    J := 0;
  L := 0;

  while P^ <> #0 do
  begin
    L := L * 10;
    L := L + Ord(P^) - Ord('0');
    Inc(P);
    Inc(J);
    if J = 9 then
    begin
      BigNumberMulWord(Res, BN_DEC_CONV);
      BigNumberAddWord(Res, L);
      L := 0;
      J := 0;
    end;
  end;

  BigNumberCorrectTop(Res);
  Res.Neg := Neg;
  Result := True;
end;

function BigNumberFromDec(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetDec(Buf, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberSetFloat(F: Extended; Res: TCnBigNumber): Boolean;
var
  N: Boolean;
  E: Integer;
  M: TUInt64;
begin
  ExtractFloatExtended(F, N, E, M);

  BigNumberSetUInt64UsingInt64(Res, M);
  Res.SetNegative(N);

  E := E - 63;
  if E > 0 then
    Res.ShiftLeft(E)
  else
    Res.ShiftRight(-E);

  Result := True;
end;

function BigNumberGetFloat(Num: TCnBigNumber): Extended;
var
  N: Boolean;
  E, B, K: Integer;
  M, T: TUInt64;
  DB: Double;
begin
  Result := 0;
  if not Num.IsZero then
  begin
    N := Num.IsNegative;

    B := Num.GetBitsCount;
    E := B - 1;

    if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
    begin
      if E > CN_EXTENDED_MAX_EXPONENT then
        raise ERangeError.Create(SCnErrorBigNumberFloatExponentRange);

      if B <= 64 then
      begin
{$IFDEF BN_DATA_USE_64}
        M := PUInt64Array(Num.D)^[0];
{$ELSE}
        if B >= 32 then
          M := PInt64Array(Num.D)^[0]
        else
          M := PCnLongWord32Array(Num.D)^[0];
{$ENDIF}

        if B < 64 then   // 10 字节扩展精度有 64 位有效数字，要求最高位为 1
          M := M shl (64 - B);
      end
      else // 如 Top > 2，则只能取最高 64 位放 M 里，其余的只能舍弃
      begin
        // (B - 1) div 64 是高的要读的 64 位的序号，里头有 B mod 64 个位
        K := (B - 1) div 64;
{$IFDEF SUPPORT_UINT64}
        T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
        T := PInt64Array(Num.D)^[K];
{$ENDIF}
        // T 拿到最高的 64 位 Element
        K := B mod 64;
        if K > 0 then // 但 T 里只有高 K 位
          T := T shl (64 - K);

        M := T; // M 拿到一批高位了

        if K > 0 then // 要补充一批 M 的低位
        begin
          K := ((B - 1) div 64) - 1;
{$IFDEF SUPPORT_UINT64}
          T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
          T := PInt64Array(Num.D)^[K];
{$ENDIF}
          // T 拿到次高的 64 位 Element
          K := 64 - (B mod 64); // 要补充的是 T 的高 K 位

          T := T shr (64 - K);
          M := M or T;
        end;
      end;

      CombineFloatExtended(N, E, M, Result);
    end
    else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
    begin
      if E > CN_DOUBLE_MAX_EXPONENT then
        raise ERangeError.Create(SCnErrorBigNumberFloatExponentRange);

      if B <= 64 then
      begin
{$IFDEF BN_DATA_USE_64}
        M := PUInt64Array(Num.D)^[0];
{$ELSE}
        if B >= 32 then
          M := PInt64Array(Num.D)^[0]
        else
          M := PCnLongWord32Array(Num.D)^[0];
{$ENDIF}

        if B < 53 then
          M := M shl (53 - B)  // 双精度浮点有 53 位有效数字，最高位内部会被隐含舍去
        else if B > 53 then
          M := M shr (B - 53); // 如果 64 位内比 53 位多，只取最高 53 位，其余舍弃精度
        // 此时 M 是低 53 位有效的数字
      end
      else // 如二进制位数大于 64，则只能取最高 53 位放 M 里，其余的舍弃精度
      begin
        // (B - 1) div 64 是高的要读的 64 位的序号，里头有 B mod 64 个位
        K := (B - 1) div 64;
{$IFDEF SUPPORT_UINT64}
        T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
        T := PInt64Array(Num.D)^[K];
{$ENDIF}
        // T 拿到最高的 64 位 Element
        K := B mod 64;
        if K > 0 then // T 里只有高 K 位有效
          T := T shl (64 - K);

        M := T; // M 拿到一批高位了

        if K > 0 then // 要补充一批 M 的低位
        begin
          K := ((B - 1) div 64) - 1;
{$IFDEF SUPPORT_UINT64}
          T := TUInt64(PInt64Array(Num.D)^[K]);
{$ELSE}
          T := PInt64Array(Num.D)^[K];
{$ENDIF}
          // T 拿到次高的 64 位 Element
          K := 64 - (B mod 64); // 要补充的是 T 的高 K 位

          T := T shr (64 - K);
          M := M or T;
        end;

        // 此时 M 是高 64 位，右移 11 位让其 53 位有效
        M := M shr 11;
      end;

      CombineFloatDouble(N, E, M, DB);
      Result := DB;
    end;
  end;
end;

function BigNumberFromFloat(F: Extended): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetFloat(F, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

// Tmp should have 2 * N UInt32/UInt64
procedure BigNumberSqrNormal(R: PCnBigNumberElement; A: PCnBigNumberElement;
  N: Integer; Tmp: PCnBigNumberElement);
var
  I, J, Max: Integer;
  AP, RP: PCnBigNumberElementArray;
begin
  Max := N * 2;
  AP := PCnBigNumberElementArray(A);
  RP := PCnBigNumberElementArray(R);
  RP^[0] := 0;
  RP^[Max - 1] := 0;

  RP := PCnBigNumberElementArray(TCnIntAddress(RP) + SizeOf(TCnBigNumberElement));
  J := N - 1;

  if J > 0 then
  begin
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP^[J] := BigNumberMulWords(RP, AP, J, PCnBigNumberElementArray(TCnIntAddress(AP) - SizeOf(TCnBigNumberElement))^[0]);
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 2 * SizeOf(TCnBigNumberElement));
  end;

  for I := N - 2 downto 1 do
  begin
    Dec(J);
    AP := PCnBigNumberElementArray(TCnIntAddress(AP) + SizeOf(TCnBigNumberElement));
    RP^[J] := BigNumberMulAddWords(RP, AP, J, PCnBigNumberElementArray(TCnIntAddress(AP) - SizeOf(TCnBigNumberElement))^[0]);
    RP := PCnBigNumberElementArray(TCnIntAddress(RP) + 2 * SizeOf(TCnBigNumberElement));
  end;

  BigNumberAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(R), PCnBigNumberElementArray(R), Max);
  BigNumberSqrWords(PCnBigNumberElementArray(Tmp), PCnBigNumberElementArray(A), N);
  BigNumberAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(R), PCnBigNumberElementArray(Tmp), Max);
end;

function BigNumberSqr(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  Max, AL: Integer;
  Tmp, RR: TCnBigNumber;
  T: array[0..15] of TCnBigNumberElement;
  IsFromPool: Boolean;
begin
  Result := False;
  AL := Num.Top;
  if AL <= 0 then
  begin
    Res.Top := 0;
    Res.Neg := 0;
    Result := True;
    Exit;
  end;

  RR := nil;
  Tmp := nil;
  IsFromPool := False;

  try
    if Num <> Res then
      RR := Res
    else
    begin
      RR := FLocalBigNumberPool.Obtain;
      IsFromPool := True;
    end;

    Tmp := FLocalBigNumberPool.Obtain;
    if (RR = nil) or (Tmp = nil) then
      Exit;

    Max := 2 * AL;
    if BigNumberWordExpand(RR, Max) = nil then
      Exit;

    if AL = 4 then
    begin
      BigNumberSqrNormal(RR.D, Num.D, 4, @(T[0]));
    end
    else if AL = 8 then
    begin
      BigNumberSqrNormal(RR.D, Num.D, 8, @(T[0]));
    end
    else
    begin
      if BigNumberWordExpand(Tmp, Max) = nil then
        Exit;
      BigNumberSqrNormal(RR.D, Num.D, AL, Tmp.D);
    end;

    RR.Neg := 0;
    if PCnBigNumberElementArray(Num.D)^[AL - 1] = (PCnBigNumberElementArray(Num.D)^[AL - 1] and BN_MASK2l) then
      RR.Top := Max - 1
    else
      RR.Top := Max;

    if RR <> Res then
      BigNumberCopy(Res, RR);
    Result := True;
  finally
    if IsFromPool then
      FLocalBigNumberPool.Recycle(RR);
    FLocalBigNumberPool.Recycle(Tmp);
  end;
end;

function BigNumberSqrt(Res: TCnBigNumber; Num: TCnBigNumber): Boolean;
var
  U: TUInt64;
  BitLength, Shift: Integer;
  X, XNext: TCnBigNumber;
begin
  Result := False;
  if Num.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end
  else if Num.IsNegative then
    Exit
  else if Num.Top <= 2 then
  begin
    U := BigNumberGetUInt64UsingInt64(Num);
    U := UInt64Sqrt(U);
    BigNumberSetUInt64UsingInt64(Res, U);
    Result := True;
    Exit;
  end
  else
  begin
    BitLength := Num.GetBitsCount;
    Shift := BitLength - 63;
    if (Shift and 1) <> 0 then  // 减 63 位如果是奇数则变成 64，也就是偶数位取高 64 位，奇数位取高 63 位，作平方根估算
      Inc(Shift);

    X := nil;
    XNext := nil;

    try
      X := FLocalBigNumberPool.Obtain;
      XNext := FLocalBigNumberPool.Obtain;

      BigNumberCopy(X, Num);
      X.ShiftRight(Shift); // 取最高的 64 位或 63 位来估算平方根

      U := X.GetInt64;
      U := UInt64Sqrt(U);
      X.SetInt64(U);

      X.ShiftLeft(Shift shr 1); // X 是估算的平方根

      // 牛顿迭代法
      while True do
      begin
        // Xnext = (x + n/x)/2
        BigNumberDiv(XNext, nil, Num, X);
        BigNumberAdd(XNext, XNext, X);
        XNext.ShiftRightOne;

        if BigNumberCompare(XNext, X) = 0 then
        begin
          // 迭代 X 的整数部分不再变化时就是结果
          BigNumberCopy(Res, X);
          Result := True;
          Exit;
        end;
        // X := XNext
        BigNumberCopy(X, XNext);
      end;
    finally
      FLocalBigNumberPool.Recycle(XNext);
      FLocalBigNumberPool.Recycle(X);
    end;
  end;
end;

function BigNumberRoot(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Integer): Boolean;
var
  I: Integer;
  X0, X1, T1, T2, T3: TCnBigBinary;
  C0, C1: TCnBigNumber;
  U: TUInt64;
begin
  Result := False;
  if (Exponent <= 0) or Num.IsNegative then
    Exit;

  if Num.IsOne or Num.IsZero then
  begin
    BigNumberCopy(Res, Num);
    Result := True;
    Exit;
  end
  else if Exponent = 2 then
    Result := BigNumberSqrt(Res, Num)
  else if Num.Top <= 2 then
  begin
    U := BigNumberGetUInt64UsingInt64(Num);
    U := UInt64NonNegativeRoot(U, Exponent);
    BigNumberSetUInt64UsingInt64(Res, U);
    Result := True;
    Exit;
  end
  else
  begin
    // 牛顿迭代法求根
    I := Num.GetBitsCount + 1;  // 得到大约 Log2 N 的值
    I := (I div Exponent) + 1;

    X0 := nil;
    X1 := nil;
    T1 := nil;
    T2 := nil;
    T3 := nil;
    C0 := nil;
    C1 := nil;

    // 别的对方没使用，延迟到此处初始化
    if FLocalBigBinaryPool = nil then
      FLocalBigBinaryPool := TCnBigBinaryPool.Create;

    try
      X0 := FLocalBigBinaryPool.Obtain;
      X1 := FLocalBigBinaryPool.Obtain;
      T1 := FLocalBigBinaryPool.Obtain;
      T2 := FLocalBigBinaryPool.Obtain;
      T3 := FLocalBigBinaryPool.Obtain;

      C0 := FLocalBigNumberPool.Obtain;
      C1 := FLocalBigNumberPool.Obtain;

      X0.SetOne;
      X0.ShiftLeft(I);                  // 得到一个较大的 X0 值作为起始值

      repeat
        // X1 := X0 - (Power(X0, Exponent) - N) / (Exponent * Power(X0, Exponent - 1));
        BigBinaryCopy(T1, X0);
        T1.Power(Exponent);
        T2.SetBigNumber(Num);
        BigBinarySub(T1, T1, T2);             // 得到 Power(X0, Exponent) - N

        BigBinaryCopy(T2, X0);
        T2.Power(Exponent - 1);
        T2.MulWord(Exponent);                // 得到 Exponent * Power(X0, Exponent - 1)

        BigBinaryDiv(T1, T1, T2, 10);        // 得到商，保留一定精度
        BigBinarySub(X1, X0, T1);            // 算出 X1

        // 得到 X0 和 X1 的整数部分并比较
        BigBinaryTruncTo(C0, X0);
        BigBinaryTruncTo(C1, X1);
        if BigNumberCompare(C0, C1) = 0 then
        begin
          // 暂且认为 X0 X1 整数部分不发生变化即认为达到精度了
          BigNumberCopy(Res, C0);
          Result := True;
          Exit;
        end;

        BigBinaryCopy(X0, X1);
      until False;
    finally
      FLocalBigBinaryPool.Recycle(X1);
      FLocalBigBinaryPool.Recycle(X0);
      FLocalBigBinaryPool.Recycle(T3);
      FLocalBigBinaryPool.Recycle(T2);
      FLocalBigBinaryPool.Recycle(T1);

      FLocalBigNumberPool.Recycle(C1);
      FLocalBigNumberPool.Recycle(C0);
    end;
  end;
end;

procedure BigNumberMulNormal(R: PCnBigNumberElement; A: PCnBigNumberElement; NA: Integer; B: PCnBigNumberElement;
  NB: Integer);
var
  RR: PCnBigNumberElement;
  Tmp: Integer;
begin
  if NA < NB then
  begin
    Tmp := NA;
    NA := NB;
    NB := Tmp;

    RR := B;
    B := A;
    A := RR;
  end;

  RR := PCnBigNumberElement(TCnIntAddress(R) + NA * SizeOf(TCnBigNumberElement));
  if NB <= 0 then
  begin
    BigNumberMulWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, 0);
    Exit;
  end
  else
    RR^ := BigNumberMulWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

  while True do
  begin
    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));

    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));
    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));
    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PCnBigNumberElement(TCnIntAddress(RR) + SizeOf(TCnBigNumberElement));
    R := PCnBigNumberElement(TCnIntAddress(R) + SizeOf(TCnBigNumberElement));
    B := PCnBigNumberElement(TCnIntAddress(B) + SizeOf(TCnBigNumberElement));
    RR^ := BigNumberMulAddWords(PCnBigNumberElementArray(R), PCnBigNumberElementArray(A), NA, B^);
  end;
end;

function BigNumberMulKaratsuba(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  H: Integer;
  XL, XH, YL, YH, P1, P2, P3: TCnBigNumber;
begin
  H := Num1.GetWordCount;
  if H < Num2.GetWordCount then
    H := Num2.GetWordCount;

  Inc(H);
  H := H shr 1;

  XL := FLocalBigNumberPool.Obtain;
  XH := FLocalBigNumberPool.Obtain;
  YL := FLocalBigNumberPool.Obtain;
  YH := FLocalBigNumberPool.Obtain;
  P1 := FLocalBigNumberPool.Obtain;
  P2 := FLocalBigNumberPool.Obtain;
  P3 := FLocalBigNumberPool.Obtain;

  try
    BigNumberCopyLow(XL, Num1, H);
    BigNumberCopyHigh(XH, Num1, Num1.GetWordCount - H);
    BigNumberCopyLow(YL, Num2, H);
    BigNumberCopyHigh(YH, Num2, Num2.GetWordCount - H);

    BigNumberAdd(P1, XH, XL);
    BigNumberAdd(P2, YH, YL);
    BigNumberMul(P3, P1, P2); // p3=(xh+xl)*(yh+yl)

    BigNumberMul(P1, XH, YH); // p1 = xh*yh
    BigNumberMul(P2, XL, YL); // p2 = xl*yl

    // p1 * 2^(32*2*h) + (p3 - p1 - p2) * 2^(32*h) + p2
    BigNumberSub(P3, P3, P1);
    BigNumberSub(P3, P3, P2);
    BigNumberShiftLeft(P3, P3, 32 * H); // P3 得到 (p3 - p1 - p2) * 2^(32*h)

    BigNumberShiftLeft(P1, P1, 32 * 2 * H); // P1 得到 p1 * 2^(32*2*h)

    BigNumberAdd(Res, P3, P1);
    BigNumberAdd(Res, Res, P2);
    Res.SetNegative(Num1.IsNegative <> Num2.IsNegative);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(XL);
    FLocalBigNumberPool.Recycle(XH);
    FLocalBigNumberPool.Recycle(YL);
    FLocalBigNumberPool.Recycle(YH);
    FLocalBigNumberPool.Recycle(P1);
    FLocalBigNumberPool.Recycle(P2);
    FLocalBigNumberPool.Recycle(P3);
  end;
end;

function BigNumberMul(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  Top, AL, BL: Integer;
  RR: TCnBigNumber;
  IsFromPool: Boolean;
begin
  Result := False;
  AL := Num1.Top;
  BL := Num2.Top;

  if (AL = 0) or (BL = 0) then
  begin
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  if (AL < BN_MUL_KARATSUBA) and (BL < BN_MUL_KARATSUBA) then // 小的、直接乘
  begin
    Top := AL + BL;

    RR := nil;
    IsFromPool := False;

    try
      if (Res = Num1) or (Res = Num2) then
      begin
        RR := FLocalBigNumberPool.Obtain;
        IsFromPool := True;
        if RR = nil then
          Exit;
      end
      else
        RR := Res;

      if Num1.Neg <> Num2.Neg then
        RR.Neg := 1
      else
        RR.Neg := 0;

      if BigNumberWordExpand(RR, Top) = nil then
        Exit;
      RR.Top := Top;
      BigNumberMulNormal(RR.D, Num1.D, AL, Num2.D, BL);

      if RR <> Res then
        BigNumberCopy(Res, RR);

      BigNumberCorrectTop(Res);
      Result := True;
    finally
      if IsFromPool then
        FLocalBigNumberPool.Recycle(RR);
    end;
  end
  else // 超长，换算法
    Result := BigNumberMulKaratsuba(Res, Num1, Num2);
end;

function BigNumberMulFloat(Res: TCnBigNumber; Num: TCnBigNumber;
  F: Extended): Boolean;
var
  N: Boolean;
  E: Integer;
  M: TUInt64;
  B: TCnBigNumber;
begin
  if F = 0 then
    Res.SetZero
  else if (F = 1) or (F = -1) then
  begin
    BigNumberCopy(Res, Num);
    if F = -1 then
      Res.Negate;
  end
  else
  begin
    // 解出符号、指数、有效数字进行整数运算
    ExtractFloatExtended(F, N, E, M);

    // 注意 Extended 在 Win32/Mac 下是 10 字节扩展精度，而 Win64 下是 8 字节双精度
    if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
      E := E - 63
    else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
      E := E - 52;
    // 把有效数字根据精度调整小数点后变成了整数，
    // 现在的真实值为 M * 2^E 次方，所以要乘以 M，再乘以 2^E

    B := FLocalBigNumberPool.Obtain;
    try
      BigNumberSetUInt64UsingInt64(B, M);
      BigNumberMul(Res, Num, B);

      B.SetWord(1);
      if E > 0 then
      begin
        B.ShiftLeft(E);
        BigNumberMul(Res, Res, B);
      end
      else
      begin
        B.ShiftLeft(-E);
        BigNumberDiv(Res, nil, Res, B);
      end;

      if N then
        Res.Negate;
    finally
      FLocalBigNumberPool.Recycle(B);
    end;
  end;
  Result := True;
end;

function BigNumberDiv(Res: TCnBigNumber; Remain: TCnBigNumber;
  Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
var
  Tmp, SNum, SDiv, SRes: TCnBigNumber;
  I, NormShift, Loop, NumN, DivN, Neg, BackupTop, BackupDMax, BackupNeg: Integer;
  D0, D1, Q, L0, N0, N1, Rem, T2L, T2H: TCnBigNumberElement;
  Resp, WNump, BackupD: PCnBigNumberElement;
  WNum: TCnBigNumber;
{$IFNDEF BN_DATA_USE_64}
  T2: TUInt64;
{$ENDIF}
begin
  Result := False;
  if (Num.Top > 0) and (PCnBigNumberElementArray(Num.D)^[Num.Top - 1] = 0) then
    Exit;

  if BigNumberIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if BigNumberUnsignedCompare(Num, Divisor) < 0 then
  begin
    if Remain <> nil then
      if BigNumberCopy(Remain, Num) = nil then
        Exit;
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  WNum := nil;
  Tmp := nil;
  SNum := nil;
  SDiv := nil;
  BackupTop := 0;
  BackupDMax := 0;
  BackupNeg := 0;
  BackupD := nil;

  try
    Tmp := FLocalBigNumberPool.Obtain;
    SNum := FLocalBigNumberPool.Obtain;
    SDiv := FLocalBigNumberPool.Obtain;
    SRes := Res;

    if (Tmp = nil) or (SNum = nil) or (SDiv = nil) or (SRes = nil) then
      Exit;

    // 把除数左移到最高位是 1，放入 SDiv，以 确保下面的 D0 最高位是 1
    NormShift := BN_BITS2 - (BigNumberGetBitsCount(Divisor) mod BN_BITS2);
    if not BigNumberShiftLeft(SDiv, Divisor, NormShift) then
      Exit;

    SDiv.Neg := 0;
    // 把被除数同样左移，并再左移一个字
    NormShift := NormShift + BN_BITS2;
    if not BigNumberShiftLeft(SNum, Num, NormShift) then
      Exit;

    SNum.Neg := 0;
    DivN := SDiv.Top;
    NumN := SNum.Top;
    Loop := NumN - DivN;

    WNum := FLocalBigNumberPool.Obtain;
    BackupNeg := WNum.Neg;
    BackupD := WNum.D;
    BackupTop := WNum.Top;
    BackupDMax := WNum.DMax;

    // 注意 WNum 需要使用外部的 D，把池子里拿出来的东西先备份
    WNum.Neg := 0;
    WNum.D := PCnBigNumberElement(TCnIntAddress(SNum.D) + Loop * SizeOf(TCnBigNumberElement));
    WNum.Top := DivN;
    WNum.DMax := SNum.DMax - Loop;

    D0 := PCnBigNumberElementArray(SDiv.D)^[DivN - 1];
    if DivN = 1 then
      D1 := 0
    else
      D1 := PCnBigNumberElementArray(SDiv.D)^[DivN - 2];
    // D0 D1 是 SDiv 的最高俩 UInt32/UInt64

    WNump := PCnBigNumberElement(TCnIntAddress(SNum.D) + (NumN - 1) * SizeOf(TCnBigNumberElement));

    if Num.Neg <> Divisor.Neg then
      SRes.Neg := 1
    else
      SRes.Neg := 0;

    if BigNumberWordExpand(SRes, Loop + 1) = nil then
      Exit;

    SRes.Top := Loop;
    Resp := PCnBigNumberElement(TCnIntAddress(SRes.D) + (Loop - 1) * SizeOf(TCnBigNumberElement));

    if BigNumberWordExpand(Tmp, DivN + 1) = nil then
      Exit;

    if BigNumberUnsignedCompare(WNum, SDiv) >= 0 then
    begin
      BigNumberSubWords(PCnBigNumberElementArray(WNum.D), PCnBigNumberElementArray(WNum.D),
        PCnBigNumberElementArray(SDiv.D), DivN);
      Resp^ := 1;
    end
    else
      Dec(SRes.Top);

    if SRes.Top = 0 then
      SRes.Neg := 0
    else
      Resp := PCnBigNumberElement(TCnIntAddress(Resp) - SizeOf(TCnBigNumberElement));

    for I := 0 to Loop - 2 do
    begin
//    Rem := 0;
      // 用 N0/N1/D0/D1 计算出一个 Q 使 | WNum - SDiv * Q | < SDiv
      N0 := WNump^;
      N1 := (PCnBigNumberElement(TCnIntAddress(WNump) - SizeOf(TCnBigNumberElement)))^;

      if N0 = D0 then
        Q := BN_MASK2
      else
      begin
{$IFDEF BN_DATA_USE_64}
        Q := InternalDivWords64(N0, N1, D0); // D0 已由上文保证最高位是 1
{$ELSE}
        Q := InternalDivWords(N0, N1, D0); // D0 已由上文保证最高位是 1
{$ENDIF}
        Rem := (N1 - Q * D0) and BN_MASK2;

{$IFDEF BN_DATA_USE_64}
        UInt64MulUInt64(D1, Q, T2L, T2H);
{$ELSE}
        T2 := UInt64Mul(D1, Q);
        T2H := (T2 shr 32) and BN_MASK2;
        T2L := T2 and BN_MASK2;
{$ENDIF}

        while True do
        begin
          if (T2H < Rem) or ((T2H = Rem) and
             (T2L <= (PCnBigNumberElement(TCnIntAddress(WNump) - 2 * SizeOf(TCnBigNumberElement)))^)) then
             Break;
          Dec(Q);
          Inc(Rem, D0);
          if Rem < D0 then
            Break;
          if T2L < D1 then
            Dec(T2H);
          Dec(T2L, D1);
        end;
      end;

      L0 := BigNumberMulWords(PCnBigNumberElementArray(Tmp.D), PCnBigNumberElementArray(SDiv.D), DivN, Q);
      PCnBigNumberElementArray(Tmp.D)^[DivN] := L0;
      WNum.D := PCnBigNumberElement(TCnIntAddress(WNum.D) - SizeOf(TCnBigNumberElement));

      if BigNumberSubWords(PCnBigNumberElementArray(WNum.D), PCnBigNumberElementArray(WNum.D),
        PCnBigNumberElementArray(Tmp.D), DivN + 1) <> 0 then
      begin
        Dec(Q);
        if BigNumberAddWords(PCnBigNumberElementArray(WNum.D), PCnBigNumberElementArray(WNum.D),
          PCnBigNumberElementArray(SDiv.D), DivN) <> 0 then
          WNump^ := WNump^ + 1;
      end;

      Resp^ := Q;
      WNump := PCnBigNumberElement(TCnIntAddress(WNump) - SizeOf(TCnBigNumberElement));
      Resp := PCnBigNumberElement(TCnIntAddress(Resp) - SizeOf(TCnBigNumberElement));
    end;

    BigNumberCorrectTop(SNum);
    Neg := Num.Neg;

    if Remain <> nil then // 需要余数时
    begin
      BigNumberShiftRight(Remain, SNum, NormShift);
      if not BigNumberIsZero(Remain) then
        Remain.Neg := Neg;
    end;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Tmp);
    FLocalBigNumberPool.Recycle(SNum);
    FLocalBigNumberPool.Recycle(SDiv);
    // 恢复 WNum 内容并扔回池子里
    WNum.Neg := BackupNeg;
    WNum.D := BackupD;
    WNum.Top := BackupTop;
    WNum.DMax := BackupDMax;
    FLocalBigNumberPool.Recycle(WNum);
  end;
end;

function BigNumberRoundDiv(Res: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber; out Rounding: Boolean): Boolean;
var
  R, H: TCnBigNumber;
  C: Integer;
begin
  R := FLocalBigNumberPool.Obtain;
  H := FLocalBigNumberPool.Obtain;
  try
    Result := BigNumberDiv(Res, R, Num, Divisor);

    // 根据余数 R 判断 Res 是否要加减一
    BigNumberShiftRightOne(H, Divisor);
    // H 是除数一半的整数部分小于等于除数的精确的一半

    if Divisor.IsOdd then // H 的绝对值加一
    begin
      if Divisor.IsNegative then
        H.SubWord(1)
      else
        H.AddWord(1);
    end;

    C := BigNumberUnsignedCompare(R, H); // 比较绝对值
    if C >= 0 then
    begin
      // 除数是偶数时，H 是除数的一半，因而余数大于或等于 H 时符合五入
      // 除数是奇数时，H 是除数的一半大一，因而余数大于或等于 H 时一定大于一半，也符合五入
      // 五入是商的绝对值加一，要看商的符号决定是加一还是减一
      if Res.IsNegative then
        Res.SubWord(1)
      else
        Res.AddWord(1);
      Rounding := True;
    end
    else // 其余情况四舍，不动
      Rounding := False;
  finally
    FLocalBigNumberPool.Recycle(H);
    FLocalBigNumberPool.Recycle(R);
  end;
end;

function BigNumberMod(Remain: TCnBigNumber; Num: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
var
  Res: TCnBigNumber;
begin
  Res := FLocalBigNumberPool.Obtain;
  try
    Result := BigNumberDiv(Res, Remain, Num, Divisor);
  finally
    FLocalBigNumberPool.Recycle(Res);
  end;
end;

function BigNumberNonNegativeMod(Remain: TCnBigNumber;
  Num: TCnBigNumber; Divisor: TCnBigNumber): Boolean;
begin
  Result := False;
  if not BigNumberMod(Remain, Num, Divisor) then
    Exit;

  Result := True;
  if Remain.Neg = 0 then
    Exit;

  // 现在 -|Divisor| < Remain < 0，所以需要 Remain := Remain + |Divisor|
  if Divisor.Neg <> 0 then
    Result := BigNumberSub(Remain, Remain, Divisor)
  else
    Result := BigNumberAdd(Remain, Remain, Divisor);
end;

function BigNumberMulWordNonNegativeMod(Res: TCnBigNumber;
  Num: TCnBigNumber; N: Integer; Divisor: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInteger(N);
    Result := BigNumberDirectMulMod(Res, Num, T, Divisor);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberAddMod(Res: TCnBigNumber; Num1, Num2: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  T := FLocalBigNumberPool.Obtain;
  try
    if not BigNumberAdd(T, Num1, Num2) then
      Exit;

    Result := BigNumberNonNegativeMod(Res, T, Divisor);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberSubMod(Res: TCnBigNumber; Num1, Num2: TCnBigNumber;
  Divisor: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  T := FLocalBigNumberPool.Obtain;
  try
    if not BigNumberSub(T, Num1, Num2) then
      Exit;

    Result := BigNumberNonNegativeMod(Res, T, Divisor);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberDivFloat(Res: TCnBigNumber; Num: TCnBigNumber;
  F: Extended): Boolean;
begin
  Result := False;
  if F = 0 then
     Exit;

  Result := BigNumberMulFloat(Res, Num, 1 / F);
end;

function BigNumberPower(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: Cardinal): Boolean;
var
  T: TCnBigNumber;
begin
  if Exponent = 0 then
  begin
    if Num.IsZero then  // 0 无 0 次方
      raise EMathError.Create(SCnErrorBigNumberZeroZeroExp);

    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent = 1 then // 1 次方为本身
  begin
    BigNumberCopy(Res, Num);
    Result := True;
    Exit;
  end;

  T := FLocalBigNumberPool.Obtain;
  BigNumberCopy(T, Num);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        BigNumberMul(Res, Res, T);

      Exponent := Exponent shr 1;
      if Exponent > 0 then // 最后等于 0 时要跳出，不需要多乘一次了
        BigNumberMul(T, T, T);
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberExp(Res: TCnBigNumber; Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
var
  I, Bits: Integer;
  V, RR: TCnBigNumber;
  IsFromPool: Boolean;
begin
  Result := False;
  RR := nil;
  V := nil;
  IsFromPool := False;

  try
    if (Res = Num) or (Res = Exponent) then
    begin
      RR := FLocalBigNumberPool.Obtain;
      IsFromPool := True;
    end
    else
      RR := Res;

    V := FLocalBigNumberPool.Obtain;
    if (RR = nil) or (V = nil) then
      Exit;

    if BigNumberCopy(V, Num) = nil then
      Exit;

    Bits := BigNumberGetBitsCount(Exponent);
    if BigNumberIsOdd(Exponent) then
    begin
      if BigNumberCopy(RR, Num) = nil then
        Exit;
    end
    else
    begin
      if not BigNumberSetOne(RR) then
        Exit;
    end;

    for I := 1 to Bits - 1 do
    begin
      if not BigNumberSqr(V, V) then
        Exit;

      if BigNumberIsBitSet(Exponent, I) then
        if not BigNumberMul(RR, RR, V) then
          Exit;
    end;

    if Res <> RR then
      BigNumberCopy(Res, RR);
    Result := True;
  finally
    if IsFromPool then
      FLocalBigNumberPool.Recycle(RR);
    FLocalBigNumberPool.Recycle(V);
  end;
end;

// 辗转相除法求 A 和 B 的最大公约数，公约数放在 A 或 B 中，返回地址
function EuclidGcd(A: TCnBigNumber; B: TCnBigNumber): TCnBigNumber;
var
  T: TCnBigNumber;
  Shifts: Integer;
begin
  Result := nil;
  Shifts := 0;
  while not BigNumberIsZero(B) do
  begin
    if BigNumberIsOdd(A) then
    begin
      if BigNumberIsOdd(B) then
      begin
        // A 奇 B 奇
        if not BigNumberSub(A, A, B) then
          Exit;
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end
      else  // A 奇 B 偶
      begin
        if not BigNumberShiftRightOne(B, B) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end;
    end
    else // A 偶
    begin
      if BigNumberIsOdd(B) then
      begin
        // A 偶 B 奇
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if BigNumberCompare(A, B) < 0 then
        begin
          T := A;
          A := B;
          B := T;
        end;
      end
      else // A 偶 B 偶
      begin
        if not BigNumberShiftRightOne(A, A) then
          Exit;
        if not BigNumberShiftRightOne(B, B) then
          Exit;
        Inc(Shifts);
      end;
    end;
  end;

  if Shifts <> 0 then
    if not BigNumberShiftLeft(A, A, Shifts) then
      Exit;
  Result := A;
end;

function BigNumberGcd(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  T, A, B: TCnBigNumber;
  R: Int64;
begin
  Result := False;

  // 小点儿的数用 Int64 版本处理以加速，因为公约数小。公倍数则不一定
  if Num1.IsInt64 and Num2.IsInt64 then
  begin
    R := CnInt64GreatestCommonDivisor2(Num1.GetInt64, Num2.GetInt64);
    Res.SetInt64(R);
    Result := True;
    Exit;
  end;

  A := nil;
  B := nil;

  try
    A := FLocalBigNumberPool.Obtain;
    B := FLocalBigNumberPool.Obtain;
    if (A = nil) or (B = nil) then
      Exit;

    if BigNumberCopy(A, Num1) = nil then
      Exit;
    if BigNumberCopy(B, Num2) = nil then
      Exit;

    A.Neg := 0;
    B.Neg := 0;
    if BigNumberCompare(A, B) < 0 then
    begin
      T := A;
      A := B;
      B := T;
    end;

    T := EuclidGcd(A, B);
    if T = nil then
      Exit;

    if BigNumberCopy(Res, T) = nil then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(A);
    FLocalBigNumberPool.Recycle(B);
  end;
end;

function BigNumberLcm(Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  G, M, R: TCnBigNumber;
begin
  Result := False;
  if BigNumberCompare(Num1, Num2) = 0 then
  begin
    BigNumberCopy(Res, Num1);
    Result := True;
    Exit;
  end;

  G := nil;
  M := nil;
  R := nil;

  try
    G := FLocalBigNumberPool.Obtain;
    M := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;

    if not BigNumberGcd(G, Num1, Num2) then
      Exit;

    if not BigNumberMul(M, Num1, Num2) then
      Exit;

    if not BigNumberDiv(Res, R, M, G) then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(M);
    FLocalBigNumberPool.Recycle(G);
  end;
end;

// 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话}
function BigNumberMulMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  T, P: TCnBigNumber;
begin
  if not BigNumberIsNegative(A) and not BigNumberIsNegative(B) then
    Result := BigNumberUnsignedMulMod(Res, A, B, C)
  else if BigNumberIsNegative(A) and BigNumberIsNegative(B) then
  begin
    T := FLocalBigNumberPool.Obtain;
    P := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(T, A);
      BigNumberCopy(P, B);
      BigNumberSetNegative(T, False);
      BigNumberSetNegative(P, False);
      Result := BigNumberUnsignedMulMod(Res, T, P, C);
    finally
      FLocalBigNumberPool.Recycle(T);
      FLocalBigNumberPool.Recycle(P);
    end;
  end
  else if BigNumberIsNegative(A) and not BigNumberIsNegative(B) then // A 负
  begin
    T := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(T, A);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, T, B, C);
      BigNumberSub(Res, C, Res);
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end
  else if not BigNumberIsNegative(A) and BigNumberIsNegative(B) then // B 负
  begin
    T := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(T, B);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, A, T, C);
      BigNumberSub(Res, C, Res);
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end
  else
    Result := False;
end;

// 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话}
function BigNumberUnsignedMulMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  AA, BB: TCnBigNumber;
begin
  Result := False;
  if  Res = C then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  AA := nil;
  BB := nil;

  try
    // 使用临时变量，保证 A、B 自身的值不发生变化
    AA := FLocalBigNumberPool.Obtain;
    BB := FLocalBigNumberPool.Obtain;

    BigNumberCopy(AA, A);
    BigNumberCopy(BB, B);
    BigNumberSetNegative(AA, False); // 全正处理
    BigNumberSetNegative(BB, False);

    if not BigNumberMod(AA, AA, C) then
      Exit;

    if not BigNumberMod(BB, BB, C) then
      Exit;

    Res.SetZero; // 如果 Res 是 A 或 B，后面参与运算的是 AA 或 BB，改变 A 或 B 不影响

    while not BB.IsZero do
    begin
      if BigNumberIsBitSet(BB, 0) then
      begin
        if not BigNumberAdd(Res, Res, AA) then
          Exit;

        if not BigNumberMod(Res, Res, C) then
          Exit;
      end;

      if not BigNumberShiftLeftOne(AA, AA) then
        Exit;

      if BigNumberCompare(AA, C) >= 0 then
        if not BigNumberMod(AA, AA, C) then
          Exit;

      if not BigNumberShiftRightOne(BB, BB) then
        Exit;
    end;
  finally
    FLocalBigNumberPool.Recycle(AA);
    FLocalBigNumberPool.Recycle(BB);
  end;
  Result := True;
end;

{* 普通计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）}
function BigNumberDirectMulMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
begin
  Result := False;
  if A = B then
  begin
    if not BigNumberSqr(Res, A) then
      Exit;
  end
  else
  begin
    if not BigNumberMul(Res, A, B) then
      Exit;
  end;

  if Res = C then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  if not BigNumberNonNegativeMod(Res, Res, C) then
    Exit;
  Result := True;
end;

// 蒙哥马利约简法快速计算 T * R^-1 mod N 其中要求 R 是刚好比 N 大的 2 整数次幂，
// NNegInv 是预先计算好的 N 对 R 的负模逆元，T 不能为负且小于 N * R
function BigNumberMontgomeryReduction(Res: TCnBigNumber;
  T, R, N, NNegInv: TCnBigNumber): Boolean;
var
  M: TCnBigNumber;
begin
  Result := False;
  M := nil;

  try
    M := FLocalBigNumberPool.Obtain;

    if not BigNumberMul(M, T, NNegInv) then // M := T * N'
      Exit;

    // M := T * N' mod R 因为 R 是 2 次幂，所以可以快速保留低位，得到的 M < R
    if not BigNumberKeepLowBits(M, R.GetBitsCount - 1) then
      Exit;

    // 复用 M := (T + M * N) / R
    if not BigNumberMul(M, M, N) then
      Exit;

    if not BigNumberAdd(M, T, M) then
      Exit;

    // 因为 R 是 2 次幂，所以可以 M 快速右移做除法，且结果必为整数
    if not BigNumberShiftRight(M, M, R.GetBitsCount - 1) then
      Exit;

    // M >= N 则减 N
    if BigNumberCompare(M, N) >= 0 then
      Result := BigNumberSub(Res, M, N)
    else
      Result := BigNumberCopy(Res, M) <> nil;
  finally
    FLocalBigNumberPool.Recycle(M);
  end;
end;

// 蒙哥马利法快速计算 A * B mod N，其中要求 R 是刚好比 N 大的 2 整数次幂，
// R2ModN 是预先计算好的 R^2 mod N 的值，NNegInv 是预先计算好的 N 对 R 的负模逆元
function BigNumberMontgomeryMulMod(Res: TCnBigNumber;
  A, B, R, R2ModN, N, NNegInv: TCnBigNumber): Boolean;
var
  AA, BB, RA, RB, M: TCnBigNumber;
begin
  Result := False;

  AA := nil;
  RA := nil;
  BB := nil;
  RB := nil;
  M := nil;

  try
    AA := FLocalBigNumberPool.Obtain;
    RA := FLocalBigNumberPool.Obtain;

    // AA := A * (R * R mod N) 不超过 N * R
    if not BigNumberMul(AA, A, R2ModN) then
      Exit;
    // 蒙哥马利算得 RA := A*(R*R)*R^-1 mod N = A * R mod N
    if not BigNumberMontgomeryReduction(RA, AA, R, N, NNegInv) then
      Exit;

    BB := FLocalBigNumberPool.Obtain;
    RB := FLocalBigNumberPool.Obtain;

    // BB := B * (R * R mod N) 不超过 N * R
    if not BigNumberMul(BB, B, R2ModN) then
      Exit;
    // 蒙哥马利算得 RB := B*(R*R)*R^-1 mod N = B * R mod N
    if not BigNumberMontgomeryReduction(RB, BB, R, N, NNegInv) then
      Exit;

    // M := (A*R * B*R) 不超过 N^2，因为 R 比 N 大，更确保 M < N * R
    M := FLocalBigNumberPool.Obtain;
    if not BigNumberMul(M, RA, RB) then
      Exit;

    // 蒙哥马利算得 Res := (A*R * B*R) * R^-1 mod N = A*B*R mod N
    if not BigNumberMontgomeryReduction(Res, M, R, N, NNegInv) then
      Exit;

    // Res 中间值给 M
    if BigNumberCopy(M, Res) = nil then
      Exit;

    // 再次蒙哥马利算得 A*B*R * R^-1 mod N = A*B mod N
    if not BigNumberMontgomeryReduction(Res, M, R, N, NNegInv) then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(M);
    FLocalBigNumberPool.Recycle(RB);
    FLocalBigNumberPool.Recycle(BB);
    FLocalBigNumberPool.Recycle(RA);
    FLocalBigNumberPool.Recycle(AA);
  end;
end;

// 快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、C 之一，内部调用 BigNumberPowerMod
function BigNumberPowerWordMod(Res: TCnBigNumber; A: TCnBigNumber;
  B: Cardinal; C: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetWord(B);
    Result := BigNumberPowerMod(Res, A, T, C);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

// 快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、B、C 之一
function BigNumberPowerMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  I, J, Bits, WStart, WEnd, Window, WValue, Start: Integer;
  D: TCnBigNumber;
  Val: array[0..31] of TCnBigNumber;

  function WindowBit(B: Integer): Integer;
  begin
    if B > 671 then
      Result := 6
    else if B > 239 then
      Result := 5
    else if B > 79 then
      Result := 4
    else if B > 23 then
      Result := 3
    else
      Result := 1;
  end;

begin
  Result := False;
  if (Res = A) or (Res = B) or (Res = C) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  Bits := BigNumberGetBitsCount(B);

  if Bits = 0 then
  begin
    if BigNumberAbsIsWord(C, 1) then
      BigNumberSetZero(Res)
    else
      BigNumberSetOne(Res);
    Result := True;
    Exit;
  end;

  D := nil;
  for I := Low(Val) to High(Val) do
    Val[I] := nil;

  try
    Val[0] := FLocalBigNumberPool.Obtain;
    if not BigNumberNonNegativeMod(Val[0], A, C) then
      Exit;

    if BigNumberIsZero(Val[0]) then
    begin
      if not BigNumberSetZero(Res) then
        Exit;
      Result := True;
      Exit;
    end;

    Window := WindowBit(Bits);
    D := FLocalBigNumberPool.Obtain;
    if Window > 1 then
    begin
      if not BigNumberDirectMulMod(D, Val[0], Val[0], C) then
        Exit;

      J := 1 shl (Window - 1);
      for I := 1 to J - 1 do
      begin
        Val[I] := FLocalBigNumberPool.Obtain;
        if not BigNumberDirectMulMod(Val[I], Val[I - 1], D, C) then
          Exit;
      end;
    end;

    Start := 1;
    WStart := Bits - 1;

    if not BigNumberSetOne(Res) then
      Exit;

    while True do
    begin
      if not BigNumberIsBitSet(B, WStart) then
      begin
        if Start = 0 then
          if not BigNumberDirectMulMod(Res, Res, Res, C) then
            Exit;

        if WStart = 0 then
          Break;

        Dec(WStart);
        Continue;
      end;

      WValue := 1;
      WEnd := 0;
      for I := 1 to Window - 1 do
      begin
        if WStart - I < 0 then
          Break;

        if BigNumberIsBitSet(B, WStart - I) then
        begin
          WValue := WValue shl (I - WEnd);
          WValue := WValue or 1;
          WEnd := I;
        end;
      end;

      J := WEnd + 1;
      if Start = 0 then
      begin
        for I := 0 to J - 1 do
          if not BigNumberDirectMulMod(Res, Res, Res, C) then
            Exit;
      end;

      if not BigNumberDirectMulMod(Res, Res, Val[WValue shr 1], C) then
        Exit;

      WStart := WStart - WEnd - 1;
      Start := 0;
      if WStart < 0 then
        Break;
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(D);
    for I := Low(Val) to High(Val) do
      FLocalBigNumberPool.Recycle(Val[I]);
  end;
end;

// 蒙哥马利法快速计算 (A ^ B) mod C，，返回计算是否成功，Res 不能是 A、B、C 之一
function BigNumberMontgomeryPowerMod(Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
var
  T, AA, BB: TCnBigNumber;
begin
  Result := False;
  if B.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end;

  if (Res = A) or (Res = B) or (Res = C) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  AA := nil;
  BB := nil;
  T := nil;

  try
    AA := FLocalBigNumberPool.Obtain;
    BB := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    if not T.SetOne then
      Exit;

    if not BigNumberMod(AA, A, C) then
      Exit;

    if BigNumberCopy(BB, B) = nil then
      Exit;

    while not BB.IsOne do
    begin
      if BigNumberIsBitSet(BB, 0) then
      begin
        if not BigNumberDirectMulMod(T, AA, T, C) then
          Exit;
      end;
      if not BigNumberDirectMulMod(AA, AA, AA, C) then
        Exit;

      if not BigNumberShiftRightOne(BB, BB) then
        Exit;
    end;

    if not BigNumberDirectMulMod(Res, AA, T, C) then
      Exit;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(AA);
    FLocalBigNumberPool.Recycle(BB);
  end;
  Result := True;
end;

function BigNumberPowerPowerMod(Res: TCnBigNumber; A, B, C, N: TCnBigNumber): Boolean;
var
  I, T: TCnBigNumber;
begin
  // A^(B^C) = A^(B*B*B*B...) 共 C 个 = ((A^B)^B)^B)^B 共 C 层 B
  if C.IsZero then
    Result := BigNumberCopy(Res, A) <> nil
  else if C.IsOne then
    Result := BigNumberPowerMod(Res, A, B, N)
  else
  begin
    if (Res = A) or (Res = B) or (Res = C) or (Res = N) then
      raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

    I := nil;
    T := nil;

    try
      Result := False;

      I := FLocalBigNumberPool.Obtain;
      I.SetZero;
      if BigNumberCopy(Res, A) = nil then
        Exit;

      T := FLocalBigNumberPool.Obtain;
      while BigNumberCompare(I, C) < 0 do
      begin
        if not BigNumberPowerMod(T, Res, B, N) then
          Exit;
        BigNumberCopy(Res, T);

        I.AddWord(1);
      end;
    finally
      FLocalBigNumberPool.Recycle(T);
      FLocalBigNumberPool.Recycle(I);
    end;
    Result := True;
  end;
end;

procedure CheckLog(Num: TCnBigNumber);
begin
  if Num.IsZero or Num.IsNegative then
    raise ERangeError.Create(SCnErrorBigNumberLogRange);
end;

function BigNumberLog2(Num: TCnBigNumber): Extended;
var
  F: Extended;
begin
  CheckLog(Num);
  if Num.IsOne then
    Result := 0
  else
  begin
    F := BigNumberGetFloat(Num);
    Result := Log2(F);
  end;
end;

function BigNumberLog10(Num: TCnBigNumber): Extended;
var
  F: Extended;
begin
  CheckLog(Num);
  if Num.IsOne then
    Result := 0
  else
  begin
    F := BigNumberGetFloat(Num);
    Result := Log10(F);
  end;
end;

function BigNumberLogN(Num: TCnBigNumber): Extended;
var
  F: Extended;
begin
  CheckLog(Num);
  if Num.IsOne then
    Result := 0
  else
  begin
    F := BigNumberGetFloat(Num);
    Result := Ln(F);
  end;
end;

function BigNumberFermatCheckComposite(A, B, C: TCnBigNumber; T: Integer): Boolean;
var
  I: Integer;
  R, L, S: TCnBigNumber;
begin
  Result := False;

  R := nil;
  L := nil;
  S := nil;

  try
    R := FLocalBigNumberPool.Obtain;
    if not BigNumberPowerMod(R, A, C, B) then
      Exit;

    L := FLocalBigNumberPool.Obtain;
    if BigNumberCopy(L, R) = nil then // L := R;
      Exit;

    S := FLocalBigNumberPool.Obtain;
    for I := 1 to T do
    begin
      if not BigNumberDirectMulMod(R, R, R, B) then
        Exit;
      // 从 MulMod 改为 DirectMulMod 后，判断 1024 位素数大概从 1.6 秒多提速到 1.4 秒多

      if R.IsOne and not L.IsOne then
      begin
        BigNumberSub(S, B, L);
        if not S.IsOne then
        begin
          Result := True;
          Exit;
        end;
      end;

      if BigNumberCopy(L, R) = nil then
        Exit;
    end;

    Result := not R.IsOne;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(L);
    FLocalBigNumberPool.Recycle(S);
  end;
end;

// TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢
function BigNumberIsProbablyPrime(Num: TCnBigNumber; TestCount: Integer): Boolean;
var
  I, T: Integer;
  X, R, W: TCnBigNumber;
begin
  Result := False;
  if TestCount <= 1 then
    Exit;

  // 排除了 负数、0、1 以及 2 之外的偶数，
  if Num.IsZero or Num.IsNegative or Num.IsOne or (not Num.IsOdd and not BigNumberAbsIsWord(Num, 2))then
    Exit;

  // 小额素数先对比判断，包括 2
  X := FLocalBigNumberPool.Obtain;
  try
    X.SetWord(CN_PRIME_NUMBERS_SQRT_UINT32[High(CN_PRIME_NUMBERS_SQRT_UINT32)]);
    if BigNumberCompare(Num, X) <= 0 then
    begin
      for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
      begin
        if BigNumberAbsIsWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I]) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(X);
  end;

  // 再用小额素数整除，不用 2 了，因为 2 之外的偶数已经被排除了
  for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) + 1 to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
  begin
    // 64 位模式下 BigNumberModWord 不支持除数大于 UInt32，这里素数表的内容符合要求
    if BigNumberModWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I]) = 0 then
      Exit;
  end;

  // 都漏网了，再做 Miller-Rabin Test
  X := nil;
  R := nil;
  W := nil;

  try
    X := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    W := FLocalBigNumberPool.Obtain;

    if BigNumberCopy(X, Num) = nil then
      Exit;

    if not BigNumberSubWord(X, 1) then
      Exit;

    if BigNumberCopy(W, X) = nil then  // W := X := Num - 1;
      Exit;

    T := 0;
    while not X.IsOdd do // X and 1 = 0
    begin
      if not BigNumberShiftRightOne(X, X) then
        Exit;
      Inc(T);
    end;

    for I := 1 to TestCount do
    begin
      if not BigNumberRandRange(R, W) then
        Exit;

      if not BigNumberAddWord(R, 1) then
        Exit;

      if BigNumberFermatCheckComposite(R, Num, X, T) then
        Exit;
    end;
  finally
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(W);
  end;
  Result := True;
end;

function InternalGenerateProbablePrime(Num: TCnBigNumber; BitsCount: Integer): Boolean;
var
  Mods: array[0..BN_PRIME_NUMBERS - 1] of TCnBigNumberElement;
  Delta, MaxDelta: TCnBigNumberElement;
  I: Integer;
label
  AGAIN;
begin
  Result := False;

AGAIN:
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

  // 64 位模式下 BigNumberModWord 不支持除数大于 UInt32，这里素数表的内容符合要求
  for I := 1 to BN_PRIME_NUMBERS - 1 do
    Mods[I] := BigNumberModWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I + 1]);

  MaxDelta := BN_MASK2 - CN_PRIME_NUMBERS_SQRT_UINT32[BN_PRIME_NUMBERS];
  Delta := 0;

  for I := 1 to BN_PRIME_NUMBERS - 1 do
  begin
    if ((Mods[I] + Delta) mod CN_PRIME_NUMBERS_SQRT_UINT32[I + 1]) <= 1 then
    begin
      Inc(Delta, 2);
      if Delta > MaxDelta then
        goto AGAIN;
      Continue;
    end;
  end;

  if not BigNumberAddWord(Num, Delta) then
    Exit;
  Result := True;
end;

// 生成一个指定位数的大素数，TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢
function BigNumberGeneratePrime(Num: TCnBigNumber; BytesCount: Integer;
  TestCount: Integer): Boolean;
begin
  Result := False;
  if not InternalGenerateProbablePrime(Num, BytesCount * 8) then
    Exit;

  while not BigNumberIsProbablyPrime(Num, TestCount) do
  begin
    if not InternalGenerateProbablePrime(Num, BytesCount * 8) then
      Exit;
  end;
  Result := True;
end;

// 生成一个指定二进制位数的大素数，TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢
function BigNumberGeneratePrimeByBitsCount(Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
begin
  Result := False;
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

  if not BigNumberSetBit(Num, BitsCount - 1) then
    Exit;

  if not Num.IsOdd then
    Num.AddWord(1);

  while not BigNumberIsProbablyPrime(Num, TestCount) do
    Num.AddWord(2);

  Result := True;
end;

function BigNumberNextPrime(Res, Num: TCnBigNumber;
  TestCount: Integer = CN_BN_MILLER_RABIN_DEF_COUNT): Boolean;
begin
  Result := True;
  if Num.IsNegative or Num.IsZero or Num.IsOne or (Num.GetWord = 2) then
  begin
    Res.SetWord(2);
    Exit;
  end
  else
  begin
    BigNumberCopy(Res, Num);
    if not Res.IsOdd then
      Res.AddWord(1);

    while not BigNumberIsProbablyPrime(Res, TestCount) do
      Res.AddWord(2);
  end;
end;

// 查 R 是否对于 Prime - 1 的每个因子，都有 R ^ (剩余因子的积) mod Prime <> 1
function BigNumberCheckPrimitiveRoot(R, Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
var
  I: Integer;
  Res, SubOne, T, Remain: TCnBigNumber;
begin
  Result := False;
  Res := FLocalBigNumberPool.Obtain;
  T := FLocalBigNumberPool.Obtain;
  Remain := FLocalBigNumberPool.Obtain;
  SubOne := FLocalBigNumberPool.Obtain;

  BigNumberCopy(SubOne, Prime);
  BigNumberSubWord(SubOne, 1);

  try
    for I := 0 to Factors.Count - 1 do
    begin
      BigNumberDiv(T, Remain, SubOne, Factors[I]);
      BigNumberMontgomeryPowerMod(Res, R, T, Prime);
      if Res.IsOne then
        Exit;
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Res);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(Remain);
    FLocalBigNumberPool.Recycle(SubOne);
  end;
end;

// 计算一素数的原根，返回计算是否成功
function BigNumberGetMinRootFromPrime(Res, Prime: TCnBigNumber): Boolean;
var
  I: Integer;
  Num, PrimeSubOne: TCnBigNumber;
  Factors: TCnBigNumberList;
begin
  Result := False;
  PrimeSubOne := nil;
  Factors := nil;
  Num := nil;

  try
    PrimeSubOne := FLocalBigNumberPool.Obtain;
    BigNumberCopy(PrimeSubOne, Prime);
    BigNumberSubWord(PrimeSubOne, 1);

    Factors := TCnBigNumberList.Create;
    BigNumberFindFactors(PrimeSubOne, Factors);
    Factors.RemoveDuplicated;

    Num := FLocalBigNumberPool.Obtain;;
    Res.SetZero;
    for I := 2 to MaxInt do // 不查太大的大数
    begin
      Num.SetWord(I);
      if BigNumberCheckPrimitiveRoot(Num, Prime, Factors) then
      begin
        Res.SetWord(I);
        Result := True;
        Exit;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(Num);
    Factors.Free;
    FLocalBigNumberPool.Recycle(PrimeSubOne);
  end;
end;

// 大数是否是一个 32 位有符号整型范围内的数
function BigNumberIsInt32(Num: TCnBigNumber): Boolean;
var
  C: Integer;
begin
  Result := False;

  C := Num.GetBitsCount;
  if C > BN_BITS_UINT_32 then // 超界
    Exit;
  if C < BN_BITS_UINT_32 then // 小于 32 位，是
  begin
    Result := True;
    Exit;
  end;

  // 32 位
  if Num.IsNegative then // 负数，小于 -$80000000 则超界
  begin
    if not BigNumberIsBitSet(Num, BN_BITS_UINT_32 - 1) then
      Result := True  // 最高位不为 1，说明绝对值小于 $80000000
    else
    begin
      // 最高位为 1，其他位需要全 0 才属于 Int32
      for C := 0 to BN_BITS_UINT_32 - 2 do
        if BigNumberIsBitSet(Num, C) then // 只要有个 1 就表示超界了
          Exit;
      Result := True;
    end;
  end
  else // 正数，需要判断最高位是否是 1，是 1 则超界，也就是大于 $7FFFFFFF
    Result := not BigNumberIsBitSet(Num, BN_BITS_UINT_32 - 1);
end;

// 大数是否是一个 32 位无符号整型范围内的数
function BigNumberIsUInt32(Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_32);
end;

// 大数是否是一个 64 位有符号整型范围内的数
function BigNumberIsInt64(Num: TCnBigNumber): Boolean;
var
  C: Integer;
begin
  Result := False;

  C := Num.GetBitsCount;
  if C > BN_BITS_UINT_64 then // 超界
    Exit;
  if C < BN_BITS_UINT_64 then // 小于 32 位，是
  begin
    Result := True;
    Exit;
  end;

  // 64 位
  if Num.IsNegative then // 负数，小于 -$80000000 00000000 则超界
  begin
    if not BigNumberIsBitSet(Num, BN_BITS_UINT_64 - 1) then
      Result := True  // 最高位不为 1，说明绝对值小于 $80000000 00000000
    else
    begin
      // 最高位为 1，其他位需要全 0 才属于 Int64
      for C := 0 to BN_BITS_UINT_64 - 2 do
        if BigNumberIsBitSet(Num, C) then // 只要有个 1 就表示超界了
          Exit;
      Result := True;
    end;
  end
  else // 正数，需要判断最高位是否是 1，是 1 则超界，也就是大于 $7FFFFFFF
    Result := not BigNumberIsBitSet(Num, BN_BITS_UINT_64 - 1);
end;

// 大数是否是一个 64 位无符号整型范围内的数
function BigNumberIsUInt64(Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_64);
end;

function BigNumberIsFloat(Num: TCnBigNumber): Boolean;
begin
  if (SizeOf(Extended) = CN_EXTENDED_SIZE_10) or (SizeOf(Extended) = CN_EXTENDED_SIZE_16) then
  begin
    // 判断是否属于 10 字节扩展精度范围
    Result := BigNumberGetBitsCount(Num) < CN_EXTENDED_MAX_EXPONENT;
  end
  else if SizeOf(Extended) = CN_EXTENDED_SIZE_8 then
  begin
    // 判断是否属于 8 字节双精度范围
    Result := BigNumberGetBitsCount(Num) < CN_DOUBLE_MAX_EXPONENT;
  end;
end;

// 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解
procedure BigNumberExtendedEuclideanGcd(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
var
  T, P, M: TCnBigNumber;
begin
  if BigNumberIsZero(B) then
  begin
    BigNumberSetOne(X);
    BigNumberSetZero(Y);
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := FLocalBigNumberPool.Obtain;
      P := FLocalBigNumberPool.Obtain;
      M := FLocalBigNumberPool.Obtain;
      BigNumberMod(P, A, B);

      BigNumberExtendedEuclideanGcd(B, P, X, Y);
      BigNumberCopy(T, X);
      BigNumberCopy(X, Y);

      // 须 CorrectTop 否则 Top 值会太大，原因不详
      BigNumberCorrectTop(X);
      BigNumberCorrectTop(Y);

      // T := X;
      // X := Y;
      // Y := T - (A div B) * Y;
      BigNumberDiv(P, M, A, B);
      BigNumberMul(P, P, Y);
      BigNumberSub(Y, T, P);
    finally
      FLocalBigNumberPool.Recycle(M);
      FLocalBigNumberPool.Recycle(P);
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

// 扩展欧几里得辗转相除法求二元一次不定方程 A * X - B * Y = 1 的整数解
procedure BigNumberExtendedEuclideanGcd2(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
var
  T, P, M: TCnBigNumber;
begin
  if BigNumberIsZero(B) then
  begin
    BigNumberSetOne(X);
    BigNumberSetZero(Y);
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := FLocalBigNumberPool.Obtain;
      P := FLocalBigNumberPool.Obtain;
      M := FLocalBigNumberPool.Obtain;
      BigNumberMod(P, A, B);

      BigNumberExtendedEuclideanGcd2(B, P, Y, X);

      // 须 CorrectTop 否则 Top 值会太大，原因不详
      BigNumberCorrectTop(X);
      BigNumberCorrectTop(Y);

      // Y := Y - (A div B) * X;
      BigNumberDiv(P, M, A, B);
      BigNumberMul(P, P, X);
      BigNumberSub(Y, Y, P);
    finally
      FLocalBigNumberPool.Recycle(M);
      FLocalBigNumberPool.Recycle(P);
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

// 求 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。调用者须自行保证 X、Modulus 互素
function BigNumberModularInverse(Res: TCnBigNumber; X, Modulus: TCnBigNumber;
  CheckGcd: Boolean): Boolean;
var
  Neg: Boolean;
  X1, Y: TCnBigNumber;
begin
  Result := False;
  if (Res = X) or (Res = Modulus) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  Neg := False;
  X1 := nil;
  Y := nil;

  try
    X1 := FLocalBigNumberPool.Obtain;
    Y := FLocalBigNumberPool.Obtain;

    if CheckGcd then
    begin
      if not BigNumberGcd(X1, X, Modulus) then
        Exit;
      if not X1.IsOne then
        Exit;
    end;

    if BigNumberCopy(X1, X) = nil then
      Exit;

    if BigNumberIsNegative(X1) then
    begin
      BigNumberSetNegative(X1, False);
      Neg := True;
    end;

    // 求正数的模逆元。负数的模逆元等于正数的模逆元的负值，解出来的负值还可以再加 Modulus
    BigNumberExtendedEuclideanGcd2(X1, Modulus, Res, Y);
    // 扩展欧几里得辗转相除法求二元一次不定方程 A * X - B * Y = 1 的整数解

    if Neg then
      BigNumberSetNegative(Res, not BigNumberIsNegative(Res));

    if BigNumberIsNegative(Res) then
      if not BigNumberAdd(Res, Res, Modulus) then
        Exit;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(X1);
  end;
end;

{* 求 X 针对素数 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。
   调用者须自行保证 Modulus 为素数，且 Res 不能是 X 或 Modulus}
function BigNumberPrimeModularInverse(Res: TCnBigNumber; X, Modulus: TCnBigNumber): Boolean;
var
  P: TCnBigNumber;
begin
  if (Res = X) or (Res = Modulus) then
    raise ECnBigNumberException.Create(SCnErrorBigNumberParamDupRef);

  // 由费马小定理知 x^(p-1) = 1 mod p，所以 x 的逆元是 x^(p-2) mod p
  P := FLocalBigNumberPool.Obtain;
  try
    BigNumberCopy(P, Modulus);
    P.SubWord(2);
    Result := BigNumberPowerMod(Res, X, P, Modulus);
  finally
    FLocalBigNumberPool.Recycle(P);
  end;
end;

// 求 X 针对 Modulus 的负模反或叫负模逆元 Y，满足 (X * Y) mod M = -1，X 可为负值，Y 求出正值
function BigNumberNegativeModularInverse(Res: TCnBigNumber;
  X, Modulus: TCnBigNumber; CheckGcd: Boolean): Boolean;
begin
  Result := BigNumberModularInverse(Res, X, Modulus, CheckGcd);
  if Result then
    Result := BigNumberSub(Res, Modulus, Res); // 负逆元等于模数减逆元
end;

// 求 32 位有符号数 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值
procedure BigNumberModularInverseWord(Res: TCnBigNumber; X: Integer;
  Modulus: TCnBigNumber; CheckGcd: Boolean);
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInteger(X);
    BigNumberModularInverse(Res, T, Modulus, CheckGcd);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

// 用二次互反律递归计算勒让德符号 ( A / P) 的值，较快
function BigNumberLegendre(A, P: TCnBigNumber): Integer;
var
  AA, Q: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise ECnBigNumberException.Create(SCnErrorBigNumberLegendre);

  if A.IsOne then
  begin
    Result := 1;
    Exit;
  end;

  AA := FLocalBigNumberPool.Obtain;
  Q := FLocalBigNumberPool.Obtain;

  try
    if A.IsOdd then
    begin
      // 奇数
      BigNumberMod(AA, P, A);
      Result := BigNumberLegendre(AA, A);

      // 计算 (A-1)*(P-1)/4 个 -1 相乘
      BigNumberSub(AA, A, CnBigNumberOne);
      BigNumberSub(Q, P, CnBigNumberOne);
      BigNumberMul(Q, AA, Q);
      BigNumberShiftRight(Q, Q, 2);

      if Q.IsOdd then // 奇数个 -1 乘还是得 -1
        Result := -Result;
    end
    else
    begin
      // 偶数
      BigNumberShiftRight(AA, A, 1);
      Result := BigNumberLegendre(AA, P);

      // 计算 (P^2 - 1)/8 个 -1 相乘
      BigNumberMul(Q, P, P);
      BigNumberSubWord(Q, 1);
      BigNumberShiftRight(Q, Q, 3);

      if Q.IsOdd then // 奇数个 -1 乘还是得 -1
        Result := -Result;
    end;
  finally
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(AA);
  end;
end;

// 用欧拉判别法计算勒让德符号 ( A / P) 的值，较慢
function BigNumberLegendre2(A, P: TCnBigNumber): Integer;
var
  R, Res: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise ECnBigNumberException.Create(SCnErrorBigNumberLegendre);

  R := FLocalBigNumberPool.Obtain;
  Res := FLocalBigNumberPool.Obtain;

  try
    // 三种情况：P 能整除 A 时返回 0，不能整除时，如果 A 是完全平方数就返回 1，否则返回 -1
    BigNumberMod(R, A, P);
    if R.IsZero then
      Result := 0
    else
    begin
      BigNumberCopy(R, P);
      BigNumberSubWord(R, 1);
      BigNumberShiftRightOne(R, R);
      BigNumberMontgomeryPowerMod(Res, A, R, P);

      if Res.IsOne then // 欧拉判别法
        Result := 1
      else
        Result := -1;
    end;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(Res);
  end;
end;

// 使用 Tonelli Shanks 算法进行模素数二次剩余求解，调用者需自行保证 P 为奇素数或奇素数的整数次方
function BigNumberTonelliShanks(Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
var
  Q, Z, C, R, T, N, L, U, B: TCnBigNumber;
  S, I, M: Integer;
begin
  Result := False;
  if (Res = nil) or A.IsZero or A.IsNegative or P.IsZero or P.IsNegative
    or (BigNumberCompare(A, P) >= 0) then
    Exit;

  // 如果勒让德符号不为 1，说明无解，下面就不用跑了
  if BigNumberLegendre(A, P) <> 1 then
    Exit;

  Q := FLocalBigNumberPool.Obtain;
  Z := FLocalBigNumberPool.Obtain;
  C := FLocalBigNumberPool.Obtain;
  R := FLocalBigNumberPool.Obtain;
  T := FLocalBigNumberPool.Obtain;
  L := FLocalBigNumberPool.Obtain;
  U := FLocalBigNumberPool.Obtain;
  B := FLocalBigNumberPool.Obtain;
  N := FLocalBigNumberPool.Obtain;

  try
    S := 0;
    BigNumberSub(Q, P, CnBigNumberOne);
    while not Q.IsOdd do
    begin
      BigNumberShiftRightOne(Q, Q);
      Inc(S);
    end;

    // 先找一个 Z 满足 针对 P 的勒让德符号为 -1
    Z.SetWord(2);
    while BigNumberCompare(Z, P) < 0 do
    begin
      if BigNumberLegendre(Z, P) = -1 then
        Break;
      BigNumberAddWord(Z, 1);
    end;

    BigNumberAdd(N, Q, CnBigNumberOne);
    BigNumberShiftRight(N, N, 1);
    BigNumberMontgomeryPowerMod(C, Z, Q, P);
    BigNumberMontgomeryPowerMod(R, A, N, P);
    BigNumberMontgomeryPowerMod(T, A, Q, P);
    M := S;

    while True do
    begin
      BigNumberMod(U, T, P);
      if U.IsOne then
        Break;

      for I := 1 to M - 1 do
      begin
        U.SetOne;
        BigNumberShiftLeft(U, U, I);
        BigNumberMontgomeryPowerMod(N, T, U, P);
        if N.IsOne then
          Break;
      end;

      U.SetOne;
      BigNumberShiftLeft(U, U, M - I - 1);
      BigNumberMontgomeryPowerMod(B, C, U, P);
      M := I;
      BigNumberDirectMulMod(R, R, B, P);

      // T := T * B * B mod P = (T * B mod P) * (B mod P) mod P
      BigNumberDirectMulMod(U, T, B, P); // U := T * B mod P
      BigNumberMod(L, B, P);       // L := B mod P
      BigNumberDirectMulMod(T, U, L, P);

      BigNumberDirectMulMod(C, B, B, P);
    end;

    BigNumberMod(L, R, P);
    BigNumberAdd(L, L, P);
    BigNumberMod(Res, L, P);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(Z);
    FLocalBigNumberPool.Recycle(C);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(L);
    FLocalBigNumberPool.Recycle(U);
    FLocalBigNumberPool.Recycle(B);
    FLocalBigNumberPool.Recycle(N);
  end;
end;

// 使用 IEEE P1363 规范中的 Lucas 序列进行模素数二次剩余求解
function BigNumberLucas(Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
var
  G, X, Z, U, V, T: TCnBigNumber;
begin
  Result := False;

  G := nil;
  X := nil;
  Z := nil;
  U := nil;
  V := nil;
  T := nil;

  try
    G := FLocalBigNumberPool.Obtain;
    X := FLocalBigNumberPool.Obtain;
    Z := FLocalBigNumberPool.Obtain;
    U := FLocalBigNumberPool.Obtain;
    V := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    while True do
    begin
      if not BigNumberRandRange(X, P) then
        Exit;

      BigNumberCopy(T, P);
      BigNumberAddWord(T, 1);
      BigNumberShiftRight(T, T, 1);
      if not BigNumberLucasVSequenceMod(X, A, T, P, U, V) then
        Exit;

      BigNumberCopy(Z, V);
      if not V.IsOdd then
      begin
        BigNumberShiftRight(Z, Z, 1);
        BigNumberMod(Z, Z, P);
      end
      else
      begin
        BigNumberAdd(Z, Z, P);
        BigNumberShiftRight(Z, Z, 1);
      end;

      if not BigNumberDirectMulMod(T, Z, Z, P) then
        Exit;
      T.SetNegative(False); // 忽略符号

      if BigNumberCompare(T, A) = 0 then
      begin
        BigNumberCopy(Res, Z);
        Result := True;
        Exit;
      end
      else if BigNumberCompare(U, CnBigNumberOne) > 0 then
      begin
        BigNumberCopy(T, P);
        BigNumberSubWord(T, 1);

        if BigNumberCompare(U, T) < 0 then
          Break;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(G);
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(Z);
    FLocalBigNumberPool.Recycle(U);
    FLocalBigNumberPool.Recycle(V);
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberSquareRootModPrime(Res: TCnBigNumber; A, Prime: TCnBigNumber): Boolean;
var
  PrimeType: TCnPrimeType;
  Rem: TCnBigNumberElement;
  T, U, X, Y, Z, OldU, R: TCnBigNumber;
begin
  Result := False;
  if Prime.IsZero then
    Exit;

  if A.IsZero then // 0 的平方 mod P = 0
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  U := nil;
  OldU := nil;
  X := nil;
  Y := nil;
  Z := nil;
  R := nil;
  T := nil;

  try
    U := FLocalBigNumberPool.Obtain;
    BigNumberCopy(U, Prime);

    // Mod 4 或 8 优化为直接取低 2 位或 3 位
    Rem := BigNumberGetLow32(Prime) and 3;
    if Rem = 3 then
    begin
      PrimeType := pt4U3;
      BigNumberDivWord(U, 4);
    end
    else
    begin
      Rem := BigNumberGetLow32(Prime) and 7;
      if Rem = 1 then
        PrimeType := pt8U1
      else if Rem = 5 then
        PrimeType := pt8U5
      else
        Exit;
      BigNumberDivWord(U, 8);
    end;

    OldU := FLocalBigNumberPool.Obtain;
    BigNumberCopy(OldU, U); // 备份一个 U

    X := FLocalBigNumberPool.Obtain;
    Y := FLocalBigNumberPool.Obtain;
    Z := FLocalBigNumberPool.Obtain;

    // 得到了 Prime 的素数类型以及整除 4 或 8 后的 U
    case PrimeType of
      pt4U3:
        begin
          // 结果是 g^(u+1) mod p
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Y, A, U, Prime);
          // 但解可能逆运算平方回去 mod Prime 后得到 -A 不符合原要求因而需要验算

          BigNumberDirectMulMod(Z, Y, Y, Prime);
          if BigNumberCompare(Z, A) = 0 then
          begin
            BigNumberCopy(Res, Y);
            Result := True;
            Exit;
          end;
        end;
      pt8U1:
        begin
          if BigNumberLucas(Res, A, Prime) then
            Result := True;
        end;
      pt8U5:
        begin
          BigNumberMulWord(U, 2);
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Z, A, U, Prime);

          R := FLocalBigNumberPool.Obtain;
          BigNumberMod(R, Z, Prime);

          if R.IsOne then
          begin
            // 结果是 g^(u+1) mod p
            BigNumberCopy(U, OldU);
            BigNumberAddWord(U, 1);
            BigNumberMontgomeryPowerMod(Y, A, U, Prime);

            BigNumberCopy(Res, Y);
            Result := True;
          end
          else
          begin
            if R.IsNegative then
              BigNumberAdd(R, R, Prime);
            BigNumberSub(R, Prime, R);

            if R.IsOne then
            begin
              // 结果是(2g ·(4g)^u) mod p = (2g mod p * (4g)^u mod p) mod p
              BigNumberCopy(X, A);
              BigNumberMulWord(X, 2);
              BigNumberMod(R, X, Prime);  // R: 2g mod p

              BigNumberCopy(X, A);
              BigNumberMulWord(X, 4);

              T := FLocalBigNumberPool.Obtain;
              BigNumberMontgomeryPowerMod(T, X, OldU, Prime); // T: (4g)^u mod p
              BigNumberMulMod(Y, R, T, Prime);

              BigNumberCopy(Res, Y);
              Result := True;
            end;
          end;
        end;
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(Z);
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(OldU);
    FLocalBigNumberPool.Recycle(U);
  end;
end;

function BigNumberJacobiSymbol(A: TCnBigNumber; N: TCnBigNumber): Integer;
var
  R: Integer;
  AA, NN: TCnBigNumber;
  ANeg: Boolean;
begin
  if N.IsEven then        // N 偶数不支持
    raise ECnBigNumberException.Create(SCnErrorBigNumberJacobiSymbol);

  if A.IsZero then
  begin
    Result := 0;
    Exit;
  end
  else if A.IsOne then
  begin
    Result := 1;
    Exit;
  end
  else if A.IsNegOne then // (-1, N) = (-1)^((N - 1)/2)
  begin
    NN := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(NN, N);
      NN.SubWord(1);
      BigNumberShiftRightOne(NN, NN);

      if NN.IsEven then
        Result := 1
      else
        Result := -1;
      Exit;
    finally
      FLocalBigNumberPool.Recycle(NN);
    end;
  end
  else if A.IsWord(2) then
  begin
    R := BigNumberGetLow32(N) and 7;
    if (R = 1) or (R = 7) then
      Result := 1
    else
      Result := -1;
    Exit;
  end;

  AA := nil;
  NN := nil;

  try
    AA := FLocalBigNumberPool.Obtain;
    BigNumberCopy(AA, A);

    ANeg := AA.IsNegative;  // 负就先翻转并记录
    if ANeg then
      AA.Negate;

    NN := FLocalBigNumberPool.Obtain;
    BigNumberCopy(NN, N);
    if NN.IsNegative then  // 模数如为负，可以直接转正
      NN.Negate;

    // A 太大就先 mod N
    if BigNumberCompare(AA, NN) > 0 then
      BigNumberMod(AA, AA, NN);

    Result := 1;
    while not AA.IsZero do
    begin
      // A 比 N 小，除二约成奇数
      R := BigNumberGetLow32(NN) and 7;
      while not AA.IsOdd do
      begin
        BigNumberShiftRightOne(AA, AA);
        if (R = 3) or (R = 5) then
          Result := -Result;
      end;
      BigNumberSwap(AA, NN);

      // 二次互反
      if ((BigNumberGetLow32(AA) and 3) = 3) and ((BigNumberGetLow32(NN) and 3) = 3) then // mod 4
        Result := -Result;

      // 换位，完成本轮二次互反，准备下一轮
      BigNumberMod(AA, AA, NN);
    end;

    if not NN.IsOne then // N 不为 1 说明不互素
      Result := 0;

    // 原始 A 为负，要乘以一个 -1 的雅可比符号，模数要求正数
    if ANeg and (Result <> 0) then
    begin
      if N.IsNegative then
      begin
        BigNumberCopy(NN, N);
        NN.Negate;

        // 如果原始 N 为负，还得乘以一个负号
        Result := -Result * BigNumberJacobiSymbol(CnBigNumberNegOne, NN); // N 为负，NN 为正
      end
      else
        Result := Result * BigNumberJacobiSymbol(CnBigNumberNegOne, N);   // N 为正
    end;
  finally
    FLocalBigNumberPool.Recycle(NN);
    FLocalBigNumberPool.Recycle(AA);
  end;
end;

procedure BigNumberPollardRho(X: TCnBigNumber; C: TCnBigNumber; Res: TCnBigNumber);
var
  I, K, X0, Y0, Y, D, X1, R: TCnBigNumber;
begin
  I := nil;
  K := nil;
  X0 := nil;
  X1 := nil;
  Y0 := nil;
  Y := nil;
  D := nil;
  R := nil;

  try
    I := FLocalBigNumberPool.Obtain;
    K := FLocalBigNumberPool.Obtain;
    X0 := FLocalBigNumberPool.Obtain;
    X1 := FLocalBigNumberPool.Obtain;
    Y0 := FLocalBigNumberPool.Obtain;
    Y := FLocalBigNumberPool.Obtain;
    D := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;

    I.SetOne;
    K.SetZero;
    BigNumberAddWord(K, 2);
    BigNumberCopy(X1, X);
    BigNumberSubWord(X1, 1);
    BigNumberRandRange(X0, X1);
    BigNumberAddWord(X1, 1);
    BigNumberCopy(Y, X0);

    while True do
    begin
      BigNumberAddWord(I, 1);

      BigNumberDirectMulMod(R, X0, X0, X);
      BigNumberAdd(R, R, C);
      BigNumberMod(X0, R, X);

      BigNumberSub(Y0, Y, X0);
      BigNumberGcd(D, Y0, X);

      if not D.IsOne and (BigNumberCompare(D, X) <> 0) then
      begin
        BigNumberCopy(Res, D);
        Exit;
      end;

      if BigNumberCompare(Y, X0) = 0 then
      begin
        BigNumberCopy(Res, X);
        Exit;
      end;

      if BigNumberCompare(I, K) = 0 then
      begin
        BigNumberCopy(Y, X0);
        BigNumberMulWord(K, 2);
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(I);
    FLocalBigNumberPool.Recycle(K);
    FLocalBigNumberPool.Recycle(X0);
    FLocalBigNumberPool.Recycle(X1);
    FLocalBigNumberPool.Recycle(Y0);
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(D);
  end;
end;

// 找出大数的素数因子列表
procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
var
  P, R, S, D, T: TCnBigNumber;
begin
  if Num.IsZero or Num.IsNegative or Num.IsOne then
    Exit;

  if BigNumberIsProbablyPrime(Num) then
  begin
    Factors.Add(BigNumberDuplicate(Num));
    Exit;
  end;

  P := nil;
  R := nil;
  S := nil;
  D := nil;
  T := nil;

  try
    P := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;
    S := FLocalBigNumberPool.Obtain;
    D := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    BigNumberCopy(P, Num);

    while BigNumberCompare(P, Num) >= 0 do
    begin
      BigNumberCopy(S, Num);
      BigNumberSubWord(S, 1);
      BigNumberRandRange(R, S);
      BigNumberAddWord(R, 1);
      BigNumberPollardRho(P, R, P);
    end;

    BigNumberFindFactors(P, Factors);
    T := FLocalBigNumberPool.Obtain;
    BigNumberDiv(T, R, Num, P);
    BigNumberFindFactors(T, Factors);
  finally
    FLocalBigNumberPool.Remove(T);
    FLocalBigNumberPool.Recycle(D);
    FLocalBigNumberPool.Recycle(S);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(P);
  end;
end;

procedure BigNumberFindAllFactors(Num: TCnBigNumber; AllFactors: TCnBigNumberList);
var
  I, J, L: Integer;
  P, PV, TN: TCnBigNumber;
  F, EF, EP, T: TCnBigNumberList;
  EC: TCnIntegerList;

  procedure GeneratePowerValues(P: TCnBigNumber; MaxExp: Integer; Powers: TCnBigNumberList);
  var
    K: Integer;
    CurrentPower, P2: TCnBigNumber;
  begin
    Powers.Clear;
    CurrentPower := Powers.Add;
    CurrentPower.SetOne; // P^0

    for K := 1 to MaxExp do
    begin
      P2 := Powers.Add;
      BigNumberMul(P2, CurrentPower, P);
      CurrentPower := P2;
    end;
  end;

begin
  if Num.IsZero then
    Exit;

  if Num.IsOne then
  begin
    AllFactors.Add(1);
    Exit;
  end
  else if BigNumberIsProbablyPrime(Num) then
  begin
    AllFactors.Add(1);
    AllFactors.Add(BigNumberDuplicate(Num));
    Exit;
  end;

  F := nil;
  EF := nil;
  EC := nil;
  EP := nil;

  try
    F := TCnBigNumberList.Create;
    BigNumberFindFactors(Num, F);   // 先获取素因数列表，可重复未排序的

    F.BigNumberSort;
    EF := TCnBigNumberList.Create;
    EC := TCnIntegerList.Create;

    for I := 0 to F.Count - 1 do
    begin
      P := F[I];
      J := EF.IndexOfValue(P);     // 查找 P 在 Factors 中的索引
      if J = -1 then               // 若 P 不存在于 Factors
      begin
        EF.Add(BigNumberDuplicate(P));                 // 添加新素因子
        EC.Add(1);                 // 初始化次数为 1
      end
      else
        EC[J] := EC[J] + 1;        // 若存在，次数加 1
    end;

    // 在 EF 和 EC 列表中得到不重复的素数因子及其对应次幂数
    AllFactors.Add(1);
    EP := TCnBigNumberList.Create;

    for I := 0 to EF.Count - 1 do
    begin
      P := EF[I];
      GeneratePowerValues(P, EC[I], EP);

      // 遍历组合，把结果存入 AllFactors 列表中
      T := TCnBigNumberList.Create;
      try
        // 遍历当前所有的因数
        for J := 0 to AllFactors.Count - 1 do
        begin
          // 遍历当前素因子的所有幂次
          for L := 0 to EP.Count - 1 do
          begin
            PV := EP[L];
            // 计算新因数并添加到临时列表
            TN := T.Add;
            BigNumberMul(TN, AllFactors[J], PV);
          end;
        end;

        // 用新生成的因数替换原有的列表
        AllFactors.Clear;
        AllFactors.AddList(T);
      finally
        T.Free;
      end;
    end;

    AllFactors.BigNumberSort;
  finally
    EP.Free;
    EC.Free;
    EF.Free;
    F.Free;
  end;
end;

procedure BigNumberEuler(Res: TCnBigNumber; Num: TCnBigNumber);
var
  F: TCnBigNumberList;
  T: TCnBigNumber;
  I: Integer;
begin
  // 先求 Num 的不重复的素数因子，再利用公式 Num * (1- 1/p1) * (1- 1/p2) ……
  F := nil;
  T := nil;

  try
    F := TCnBigNumberList.Create;
    BigNumberFindFactors(Num, F);

    // 手工去重
    F.RemoveDuplicated;

    BigNumberCopy(Res, Num);
    for I := 0 to F.Count - 1 do
      BigNumberDiv(Res, nil, Res, F[I]);

    T := FLocalBigNumberPool.Obtain;
    for I := 0 to F.Count - 1 do
    begin
      BigNumberCopy(T, F[I]);
      T.SubWord(1);
      BigNumberMul(Res, Res, T);
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    F.Free;
  end;
end;

// 计算 Lucas 的 U 序列
function BigNumberLucasUSequenceMod(P: TCnBigNumber; Q: TCnBigNumber; K: TCnBigNumber;
  N: TCnBigNumber; U: TCnBigNumber): Boolean;
var
  C, I: Integer;
  U0, U1, V0, V1, Q0, Q1, T0, T1: TCnBigNumber;
begin
  Result := False;
  if K.IsNegative then
    Exit;

  if K.IsZero then
  begin
    U.SetZero;
    Exit;
  end
  else if K.IsOne then
  begin
    U.SetOne;
    Exit;
  end;

  C := BigNumberGetBitsCount(K); // 有效位数，所以下面从 C - 1 位到 0 位循环
  if C < 1 then
    Exit;

  U0 := nil;
  U1 := nil;
  V0 := nil;
  V1 := nil;
  Q0 := nil;
  Q1 := nil;
  T0 := nil;
  T1 := nil;

  try
    U0 := FLocalBigNumberPool.Obtain;
    U1 := FLocalBigNumberPool.Obtain;
    V0 := FLocalBigNumberPool.Obtain;
    V1 := FLocalBigNumberPool.Obtain;
    Q0 := FLocalBigNumberPool.Obtain;
    Q1 := FLocalBigNumberPool.Obtain;
    T0 := FLocalBigNumberPool.Obtain;
    T1 := FLocalBigNumberPool.Obtain;

    U0.SetZero;            // U_0 = 0
    U1.SetOne;             // U_1 = 1
    V0.SetInteger(2);      // V_0 = 2
    BigNumberCopy(V1, P);  // V_1 = P
    Q0.SetOne;
    Q1.SetOne;

    for I := C - 1 downto 0 do
    begin
      if not BigNumberDirectMulMod(Q0, Q0, Q1, N) then Exit;

      if BigNumberIsBitSet(K, I) then
      begin
        if not BigNumberDirectMulMod(Q1, Q0, Q, N) then Exit;

        if not BigNumberDirectMulMod(T0, U1, V0, N) then Exit; // U_{2h+1} = U_{h+1} * V_h - Q^h * P
        if not BigNumberDirectMulMod(T1, Q0, P, N) then Exit;
        if not BigNumberSubMod(U0, T0, T1, N) then Exit;

        if not BigNumberDirectMulMod(T0, V1, V0, N) then Exit; // V_{2h+1} = V_{h+1} * V_h - Q^h * P
        if not BigNumberSubMod(V0, T0, T1, N) then Exit;       // T1 = Q^h * P 已计算

        if not BigNumberDirectMulMod(U1, U1, V1, N) then Exit; // U_{2h+2} = U_{h+1} * V_{h+1}

        if not BigNumberDirectMulMod(T0, V1, V1, N) then Exit; // V_{2h+2} = V_{h+1}^2 - 2Q^{h+1}
        BigNumberCopy(T1, Q1);
        if not BigNumberShiftLeftOne(T1, T1) then Exit;
        if not BigNumberMod(T1, T1, N) then Exit;
        if not BigNumberSubMod(V1, T0, T1, N) then Exit;
      end
      else
      begin
        BigNumberCopy(Q1, Q0);

        if not BigNumberDirectMulMod(T0, U1, V0, N) then Exit; // U_{2h+1} = U_h+1 * V_h - Q^h
        if not BigNumberSubMod(U1, T0, Q0, N) then Exit;

        if not BigNumberDirectMulMod(T0, V1, V0, N) then Exit; // V_{2h+1} = V_h * V_{h+1} - Q^h * P
        if not BigNumberDirectMulMod(T1, Q0, P, N) then Exit;
        if not BigNumberSubMod(V1, T0, T1, N) then Exit;

        if not BigNumberDirectMulMod(U0, U0, V0, N) then Exit; // U_{2h} = U_h * V_h

        if not BigNumberDirectMulMod(T0, V0, V0, N) then Exit; // V_{2h} = V_{h}^2 - 2Q^{h}
        BigNumberCopy(T1, Q0);
        if not BigNumberShiftLeftOne(T1, T1) then Exit;
        if not BigNumberMod(T1, T1, N) then Exit;
        if not BigNumberSubMod(V0, T0, T1, N) then Exit;
      end;
    end;

    BigNumberCopy(U, U0);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T1);
    FLocalBigNumberPool.Recycle(T0);
    FLocalBigNumberPool.Recycle(Q1);
    FLocalBigNumberPool.Recycle(Q0);
    FLocalBigNumberPool.Recycle(V1);
    FLocalBigNumberPool.Recycle(V0);
    FLocalBigNumberPool.Recycle(U1);
    FLocalBigNumberPool.Recycle(U0);
  end;
end;

// 计算 IEEE P1363 的规范中说明的 Lucas 的 V 序列
function BigNumberLucasVSequenceMod(X, Y, K, N: TCnBigNumber; Q, V: TCnBigNumber): Boolean;
var
  C, I: Integer;
  V0, V1, Q0, Q1, T0, T1, C2: TCnBigNumber;
begin
  Result := False;
  if K.IsNegative then
    Exit;

  if K.IsZero then
  begin
    Q.SetOne;
    V.SetWord(2);
    Result := True;
    Exit;
  end
  else if K.IsOne then
  begin
    Q.SetOne;
    BigNumberCopy(V, X);
    Result := True;
    Exit;
  end;

  C := BigNumberGetBitsCount(K);  // 有效位数，所以下面从 C - 1 位到 0 位循环
  if C < 1 then
    Exit;

  V0 := nil;
  V1 := nil;
  Q0 := nil;
  Q1 := nil;
  T0 := nil;
  T1 := nil;
  C2 := nil;

  try
    V0 := FLocalBigNumberPool.Obtain;
    V1 := FLocalBigNumberPool.Obtain;
    Q0 := FLocalBigNumberPool.Obtain;
    Q1 := FLocalBigNumberPool.Obtain;
    T0 := FLocalBigNumberPool.Obtain;
    T1 := FLocalBigNumberPool.Obtain;
    C2 := FLocalBigNumberPool.Obtain;

    C2.SetWord(2);
    V0.SetWord(2);
    BigNumberCopy(V1, X);
    Q0.SetOne;
    Q1.SetOne;

    for I := C - 1 downto 0 do
    begin
      if not BigNumberDirectMulMod(Q0, Q0, Q1, N) then
        Exit;

      if BigNumberIsBitSet(K, I) then
      begin
        if not BigNumberDirectMulMod(Q1, Q0, Y, N) then
          Exit;

        if not BigNumberDirectMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V0, T0, N) then
          Exit;

        if not BigNumberDirectMulMod(T0, V1, V1, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, C2, Q1, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;
      end
      else
      begin
        BigNumberCopy(Q1, Q0);

        if not BigNumberDirectMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;

        if not BigNumberDirectMulMod(T0, V0, V0, N) then
          Exit;
        if not BigNumberDirectMulMod(T1, C2, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V0, T0, N) then
          Exit;
      end;
    end;

    BigNumberCopy(Q, Q0);
    BigNumberCopy(V, V0);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(V0);
    FLocalBigNumberPool.Recycle(V1);
    FLocalBigNumberPool.Recycle(Q0);
    FLocalBigNumberPool.Recycle(Q1);
    FLocalBigNumberPool.Recycle(T0);
    FLocalBigNumberPool.Recycle(T1);
    FLocalBigNumberPool.Recycle(C2);
  end;
end;

// 用中国剩余定理，根据余数与互素的除数求一元线性同余方程组的最小解，返回求解是否成功
function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers, Factors: TCnBigNumberList): Boolean;
var
  I, J: Integer;
  G, N, Sum: TCnBigNumber;
begin
  Result := False;
  if (Remainers.Count <> Factors.Count) or (Remainers.Count = 0) then
    Exit;

  Sum := nil;
  G := nil;
  N := nil;

  try
    Sum := FLocalBigNumberPool.Obtain;
    G := FLocalBigNumberPool.Obtain;
    N := FLocalBigNumberPool.Obtain;

    BigNumberSetZero(Sum);
    for I := 0 to Remainers.Count - 1 do
    begin
      // 对于每一个余数和对应除数，找出其他除数的公倍数中除以该除数余 1 的数（涉及到模逆元），
      // 如 5 7 的公倍数 35n，对 3 余 1 的是 70。3 7 对 5 余 1 的是 21，3 5 对 7 余 1 的是 14
      // 然后该余数和该模逆元相乘
      // 所有的乘积加起来，mod 一下全体除数们的最小公倍数，就得到结果了

      G.SetOne;
      for J := 0 to Factors.Count - 1 do
        if J <> I then
          if not BigNumberMul(G, G, Factors[J]) then
            Exit;

      // G 此刻是最小公倍数，因为 Factors 互素
      // 求 X 针对 M 的模反元素也就是模逆元 Y，满足 (X * Y) mod M = 1
      BigNumberModularInverse(N, G, Factors[I]);

      if not BigNumberMul(G, N, G) then // 得到乘数
        Exit;

      if not BigNumberMul(G, Remainers[I], G) then // 乘数与余数相乘
        Exit;

      if not BigNumberAdd(Sum, Sum, G) then // 求和
        Exit;
    end;

    G.SetOne;
    for J := 0 to Factors.Count - 1 do
      if not BigNumberMul(G, G, Factors[J]) then
        Exit;

    Result := BigNumberNonNegativeMod(Res, Sum, G);
  finally
    FLocalBigNumberPool.Recycle(N);
    FLocalBigNumberPool.Recycle(G);
    FLocalBigNumberPool.Recycle(Sum);
  end;
end;

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers, Factors: TCnInt64List): Boolean; overload;
var
  I: Integer;
  BR, BF: TCnBigNumberList;
begin
  BR := nil;
  BF := nil;

  try
    BR := TCnBigNumberList.Create;
    BF := TCnBigNumberList.Create;

    for I := 0 to Remainers.Count - 1 do
      BR.Add.SetInt64(Remainers[I]);

    for I := 0 to Factors.Count - 1 do
      BF.Add.SetInt64(Factors[I]);

    Result := BigNumberChineseRemainderTheorem(Res, BR, BF);
  finally
    BF.Free;
    BR.Free;
  end;
end;

function BigNumberIsPerfectPower(Num: TCnBigNumber): Boolean;
var
  LG2, I: Integer;
  T: TCnBigNumber;
begin
  Result := False;
  if Num.IsNegative or Num.IsWord(2) or Num.IsWord(3) then
    Exit;

  if Num.IsZero or Num.IsOne then
  begin
    Result := True;
    Exit;
  end;

  LG2 := Num.GetBitsCount;
  T := FLocalBigNumberPool.Obtain;

  try
    for I := 2 to LG2 do
    begin
      // 求 Num 的 I 次方根的整数部分，应该不会出现浮点数的 Power 1/I 次方那样整数部分偏小的问题
      BigNumberRoot(T, Num, I);
      // 整数部分再求幂
      BigNumberPower(T, T, I);

      // 判断是否相等
      if BigNumberCompare(T, Num) = 0 then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

procedure BigNumberFillCombinatorialNumbers(List: TCnBigNumberList; N: Integer);
var
  M, MC: Integer;
  C, T: TCnBigNumber;
begin
  if (N < 0) or (List = nil) then
    Exit;

  List.Clear;
  List.Add.SetOne;
  if N = 0 then
    Exit;

  MC := N div 2;

  List.Count := N + 1;    // C(n, m) m 从 0 到 n，一共 n+1 个
  C := TCnBigNumber.Create;
  C.SetOne;
  List[N] := C;

  C := FLocalBigNumberPool.Obtain;
  C.SetOne;
  try
    for M := 0 to MC - 1 do
    begin
      T := TCnBigNumber.Create;
      BigNumberCopy(T, C);
      BigNumberMulWord(T, N - M);
      BigNumberDivWord(T, M + 1);

      List[M + 1] := T;
      if M + 1 <> N - M - 1 then
        List[N - M - 1] := BigNumberDuplicate(T);
      BigNumberCopy(C, T);
    end;
  finally
    FLocalBigNumberPool.Recycle(C);
  end;
end;

procedure BigNumberFillCombinatorialNumbersMod(List: TCnBigNumberList; N: Integer; P: TCnBigNumber);
var
  I: Integer;
begin
  if (P = nil) or (N < 0) then
    Exit;

  BigNumberFillCombinatorialNumbers(List, N);
  for I := 0 to List.Count - 1 do
    BigNumberNonNegativeMod(List[I], List[I], P);
end;

function BigNumberAKSIsPrime(N: Int64): Boolean;
var
  T: TCnBigNumber;
begin
  T := TCnBigNumber.Create;
  try
    T.SetInt64(N);
    Result := BigNumberAKSIsPrime(T);
  finally
    T.Free;
  end;
end;

function BigNumberAKSIsPrime(N: TCnBigNumber): Boolean;
var
  R, T, C, Q: TCnBigNumber;
  LG22: Int64;
  LG2: Extended;
  BK: TCnBigNumber;
begin
  Result := False;
  if N.IsNegative or N.IsZero or N.IsOne then
    Exit;
  if BigNumberIsPerfectPower(N) then // 如果是完全幂则是合数
    Exit;

  R := nil;
  T := nil;
  C := nil;
  Q := nil;
  BK := nil;

  try
    // 找出最小的 R 满足 N mod R 的乘法阶 > (Log二底(N))^2
    // N mod R 的乘法阶（假设叫 L），指满足 N 的 L 次方后 mod R 为 1 的最小 L
    R := FLocalBigNumberPool.Obtain;
    R.SetOne;
    LG2 := BigNumberLog2(N);      // 整数会有误差，需要用到浮点，一般不会超出浮点范围
    LG22 := Trunc(LG2 * LG2);

    T := FLocalBigNumberPool.Obtain;
    BK := FLocalBigNumberPool.Obtain;

    // 找出最小的 R。这一步之前参考维基百科上的 K 暴力从 1 到 (Log二底(N))^2，较为耗时
    // 现改为从 R 的欧拉函数值的所有因数中搜索，速度有明显提高，前提是取随机数时使用非重复初始化版本
    while True do
    begin
      R.AddWord(1);
      BigNumberMultiplicativeOrder(BK, N, R);
      if BigNumberCompareInteger(BK, LG22) > 0 then
        Break;
    end;

    // 得到 R，从 R 往低了找，如果某些比 R 小的 T 和 N 不互素，则是合数
    BigNumberCopy(T, R);
    C := FLocalBigNumberPool.Obtain;

    while BigNumberCompare(T, CnBigNumberOne) > 0 do
    begin
      BigNumberGcd(C, T, N);
      if (BigNumberCompare(C, CnBigNumberOne) > 0) and (BigNumberCompare(C, N) < 0) then
        Exit;

      T.SubWord(1);
    end;

    if BigNumberCompare(N, R) <= 0 then
    begin
      Result := True;
      Exit;
    end;

    Q := FLocalBigNumberPool.Obtain;
    BigNumberEuler(Q, R);
    BigNumberSqrt(Q, Q);
    BigNumberMulFloat(C, Q, LG2);
    // 此处应该用小数计算，因为整数会产生较大误差
    // C := Trunc(Sqrt(Q) * LG2);

    // 先在环 (X^R-1, N) 上提前计算 (X+Y)^N - (X^N + Y)，
    // 也就是 (X+Y)^N - (X^N + Y) 展开后针对 X^R-1 求余，且系数都针对 N 取模
    // 根据二项式定理 (X+Y)^N 展开后各项系数 mod N 后，就变成了 X^N+Y^N，其余项余数均为 0
    // 再 mod X^R - 1 后根据加法求模规则得到的是 X^(N-R) + Y^N
    // X^N + Y 对 X^R-1 取模则是 X^(N-R) + Y
    // 一减，得到的结果其实是 Y^N - Y

    // 从 1 到 欧拉(R)平方根 * (Log二底(N)) 的整数部分，逐个作为 Y，计算 Y^N - Y mod N 是否是 0

    T.SetOne;
    while BigNumberCompare(T, C) <= 0 do
    begin
      if not BigNumberPowerMod(R, T, N, N) then // 复用 R
        Exit;

      if not BigNumberSub(R, R, T) then
        Exit;

      if not BigNumberMod(R, R, N) then
        Exit;

      if not R.IsZero then
        Exit;

      T.AddWord(1);
    end;

    Result := True;
  finally
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(C);
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(BK);
  end;
end;

function BigNumberBPSWIsPrime(N: Int64): Boolean;
var
  T: TCnBigNumber;
begin
  T := TCnBigNumber.Create;
  try
    T.SetInt64(N);
    Result := BigNumberBPSWIsPrime(T);
  finally
    T.Free;
  end;
end;

function BigNumberBPSWIsPrime(N: TCnBigNumber): Boolean;
var
  T: Integer;
  X, Y, A, U: TCnBigNumber;
begin
  Result := False;
  if N.IsNegative or N.IsZero or N.IsOne then
    Exit;

  if BigNumberEqual(N, 2) then
  begin
    Result := True;
    Exit;
  end
  else if N.IsEven then
    Exit;

  X := nil;
  Y := nil;
  A := nil;
  U := nil;

  try
    X := FLocalBigNumberPool.Obtain;
    BigNumberCopy(X, N);

    X.SubWord(1);
    T := 0;
    while X.IsEven do
    begin
      X.ShiftRightOne;
      Inc(T);
    end;

    // 计算一轮以 2 为基的费马检测，如不通过则是合数
    Y := FLocalBigNumberPool.Obtain;
    Y.SetWord(2);
    if BigNumberFermatCheckComposite(Y, N, X, T) then
      Exit;

    X.SetInteger(-3);
    repeat
      if X.IsNegative then
        X.SubWord(2)
      else
        X.AddWord(2);

      X.Negate;
    until BigNumberJacobiSymbol(X, N) = -1;

    // X 中拿到正确的 D 值，计算 Q 值放到 Y 中，P 值 1 也放 X 中
    X.SubWord(1);
    X.Negate;
    BigNumberShiftRight(Y, X, 2);
    X.SetOne;

    A := FLocalBigNumberPool.Obtain;
    BigNumberCopy(A, N);
    A.AddWord(1);

    U := FLocalBigNumberPool.Obtain;
    if not BigNumberLucasUSequenceMod(X, Y, A, N, U) then
      Exit;

    Result := U.IsZero;
  finally
    FLocalBigNumberPool.Recycle(U);
    FLocalBigNumberPool.Recycle(A);
    FLocalBigNumberPool.Recycle(Y);
    FLocalBigNumberPool.Recycle(X);
  end;
end;

function BigNumberIsMersennePrime(E: Integer): Boolean;
var
  I: Integer;
  K, M: TCnBigNumber;
begin
  Result := False;
  if E < 2 then
    Exit;

  if E = 2 then // 3 是
  begin
    Result := True;
    Exit;
  end;

  K := nil;
  M := nil;

  try
    K := FLocalBigNumberPool.Obtain;

    // 计算 Lucas-Lehmer 序列
    K.SetWord(4);
    for I := 1 to E - 2 do
    begin
      // 每一轮平方减 2
      BigNumberSqr(K, K); // 注意 E 太大时这里特耗时耗内存
      K.SubWord(2);
    end;

    // 并计算梅森数
    M := FLocalBigNumberPool.Obtain;
    M.SetOne;
    M.ShiftLeft(E);
    M.SubWord(1);

    // 判断是否整除
    BigNumberMod(K, K, M);
    Result := K.IsZero;
  finally
    FLocalBigNumberPool.Recycle(M);
    FLocalBigNumberPool.Recycle(K);
  end;
end;

function BigNumberNonAdjanceFormWidth(N: TCnBigNumber; Width: Integer): TShortInts;
var
  K: TCnBigNumber;
  M, R, B1: Cardinal;
  I: Integer;
begin
  Result := nil;
  if (Width < 1) or (Width > 7) then
    Exit;

  K := nil;

  try
    K := FLocalBigNumberPool.Obtain;
    BigNumberCopy(K, N);
    SetLength(Result, K.GetBitsCount + 1);

    I := 0;
    if Width = 1 then
      M := 3                        // 1 时需要 mod 4，等于保留低 2 位
    else
      M := not ((not 0) shl Width); // 0 到 W-1 位全 1
    B1 := 1 shl (Width - 1);        // 2^(W-1)

    while not K.IsZero do
    begin
      if K.IsOdd then
      begin
        R := BigNumberAndWordTo(K, M); // R 是低几位，也是 Mod 2^W 或 4 的值，但大于 0
        if Width = 1 then
          Result[I] := ShortInt(2 - R)
        else
        begin
          if R > B1 then
            Result[I] := R - B1 - B1   // 低几位是 Mod 2^W 值，再用 2^W 减之
          else
            Result[I] := R;
        end;

        if Result[I] > 0 then
          K.SubWord(Result[I])
        else if Result[I] < 0 then // SubWord 的参数是无符号，因而得求负再加
          K.AddWord(-Result[I]);
      end
      else
        Result[I] := 0;

      Inc(I);
      K.ShiftRightOne;
    end;

    if I < Length(Result) then // 去除多余长度
      SetLength(Result, I);
  finally
    FLocalBigNumberPool.Recycle(K);
  end;
end;

// 大步小步算法求离散对数问题 A^X mod M = B 的解 Res，要求 A 和 M 互素
function BigNumberBigStepGiantStep(Res: TCnBigNumber; A, B, M: TCnBigNumber): Boolean;
var
  Map: TCnBigNumberHashMap;
  T, C, Q, N, K, V: TCnBigNumber;
begin
  Result := False;
  if A.IsNegative or B.IsNegative or M.IsNegative then
    Exit;

  T := nil;
  C := nil;
  K := nil;
  Q := nil;
  N := nil;
  Map := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberSqrt(T, M);
    T.AddWord(1);

    C := FLocalBigNumberPool.Obtain;
    BigNumberDirectMulMod(C, A, B, M);

    Map := TCnBigNumberHashMap.Create(True, True);
    K := FLocalBigNumberPool.Obtain;
    K.SetOne;

    while BigNumberCompare(K, T) < 0 do
    begin
      Map.Add(BigNumberDuplicate(C), BigNumberDuplicate(K));
      BigNumberDirectMulMod(C, A, C, M);
      K.AddWord(1);
    end;

    Q := FLocalBigNumberPool.Obtain;
    BigNumberPowerMod(Q, A, T, M);
    N := FLocalBigNumberPool.Obtain;
    BigNumberCopy(N, Q);

    K.SetOne;
    while BigNumberCompare(K, T) < 0 do
    begin
      if Map.HasKey(N) then
      begin
        V := Map.Find(N); // V 是引用
        BigNumberMul(Res, K, T);
        BigNumberSub(Res, Res, V);

        Result := True;
        Exit;
      end;
      BigNumberDirectMulMod(N, Q, N, M);
      K.AddWord(1);
    end;
  finally
    FLocalBigNumberPool.Recycle(N);
    FLocalBigNumberPool.Recycle(Q);
    FLocalBigNumberPool.Recycle(K);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(C);
    Map.Free;
  end;
end;

function BigNumberMultiplicativeOrder(Res: TCnBigNumber; N: TCnBigNumber;
  R: TCnBigNumber): Boolean;
var
  I: Integer;
  E, T: TCnBigNumber;
  F: TCnBigNumberList;
begin
  Result := False;

  T := nil;
  E := nil;
  F := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberGcd(T, N, R);
    if not T.IsOne then
      Exit;

    // 互素才有存在乘法阶
    E := FLocalBigNumberPool.Obtain;
    BigNumberEuler(E, R);

    F := TCnBigNumberList.Create;
    BigNumberFindAllFactors(E, F);

    // 拿到全部因子，遍历验证
    for I := 0 to F.Count - 1 do
    begin
      // 验证 N 的 F[I] 次方 mod R 是否为 1
      BigNumberPowerMod(T, N, F[I], R);
      if T.IsOne then
      begin
        BigNumberCopy(Res, F[I]);
        Result := True;
        Exit;
      end;
    end;
  finally
    F.Free;
    FLocalBigNumberPool.Recycle(E);
    FLocalBigNumberPool.Recycle(T);
  end;
end;

// 打印大数内部信息
function BigNumberDebugDump(Num: TCnBigNumber): string;
var
  I: Integer;
begin
  Result := '';
  if Num = nil then
    Exit;

  Result := Format('Neg %d. DMax %d. Top %d.', [Num.Neg, Num.DMax, Num.Top]);
  if (Num.D <> nil) and (Num.Top > 0) then
  begin
    for I := 0 to Num.Top - 1 do
    begin
{$IFDEF BN_DATA_USE_64}
      Result := Result + Format(' $%16.16x', [PCnBigNumberElementArray(Num.D)^[I]]);
{$ELSE}
      Result := Result + Format(' $%8.8x', [PCnBigNumberElementArray(Num.D)^[I]]);
{$ENDIF}
    end;
  end;
end;

// 将大数内部信息原封不动 Dump 至 Mem 所指的内存区
function BigNumberRawDump(Num: TCnBigNumber; Mem: Pointer): Integer;
begin
  if Num.D = nil then
  begin
    Result := 0;
    Exit;
  end
  else
    Result := Num.Top * SizeOf(TCnBigNumberElement);

  if Mem <> nil then
    Move(Num.D^, Mem^, Num.Top * SizeOf(TCnBigNumberElement));
end;

function SparseBigNumberListIsZero(P: TCnSparseBigNumberList): Boolean;
begin
  Result := (P = nil) or (P.Count = 0) or
    ((P.Count = 1) and (P[0].Exponent = 0) and (P[0].Value.IsZero));
end;

function SparseBigNumberListEqual(A, B: TCnSparseBigNumberList): Boolean;
var
  I: Integer;
begin
  Result := False;
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  if (A = nil) and (B <> nil)then // 一个是 nil，需要判断另外一个是不是 0
  begin
    if (B.Count = 0) or ((B.Count = 1) and (B[0].Exponent = 0) and B[0].Value.IsZero) then
    begin
      Result := True;
      Exit;
    end;
  end
  else if (B = nil) and (A <> nil) then
  begin
    if (A.Count = 0) or ((A.Count = 1) and (A[0].Exponent = 0) and A[0].Value.IsZero) then
    begin
      Result := True;
      Exit;
    end;
  end;

  if A.Count <> B.Count then
    Exit;

  for I := A.Count - 1 downto 0 do
  begin
    if (A[I].Exponent <> B[I].Exponent) or not BigNumberEqual(A[I].Value, B[I].Value) then
      Exit;
  end;
  Result := True;
end;

procedure SparseBigNumberListCopy(Dest, Source: TCnSparseBigNumberList);
var
  I: Integer;
  Pair: TCnExponentBigNumberPair;
begin
  if (Dest <> Source) and (Dest <> nil) then
  begin
    Dest.Clear;
    for I := 0 to Source.Count - 1 do
    begin
      Pair := TCnExponentBigNumberPair.Create;
      Pair.Exponent := Source[I].Exponent;
      BigNumberCopy(Pair.Value, Source[I].Value);
      Dest.Add(Pair);
    end;
  end;
end;

procedure SparseBigNumberListMerge(Dest, Source1, Source2: TCnSparseBigNumberList; Add: Boolean);
var
  I, J, K: Integer;
  P1, P2: TCnExponentBigNumberPair;
begin
  if Source1 = nil then                   // 只要有一个是 nil，Dst 就被塞为另一个
  begin
    SparseBigNumberListCopy(Dest, Source2);
    if not Add then  // Source2 是被减数
      Dest.Negate;
  end
  else if Source2 = nil then
    SparseBigNumberListCopy(Dest, Source1)
  else if Source1 = Source2 then // 如果 Source1 和 Source2 是同一个，合并，支持 Dest 也是同一个的情形
  begin
    Dest.Count := Source1.Count;
    for I := 0 to Source1.Count - 1 do
    begin
      if Dest[I] = nil then
        Dest[I] := TCnExponentBigNumberPair.Create;
      Dest[I].Exponent := Source1[I].Exponent;
      if Add then
        BigNumberAdd(Dest[I].Value, Source1[I].Value, Source2[I].Value)
      else
        BigNumberSub(Dest[I].Value, Source1[I].Value, Source2[I].Value);
    end;
  end
  else // Source1 和 Source2 不是同一个，要归并
  begin
    if (Dest <> Source1) and (Dest <> Source2) then // 但 Dest 不是 Source1 或 Source2，也就是仨各异
    begin
      I := 0;
      J := 0;
      K := 0;

      Dest.Count := Source1.Count + Source2.Count;

      while (I < Source1.Count) and (J < Source2.Count) do
      begin
        P1 := Source1[I];
        P2 := Source2[J];

        if P1.Exponent = P2.Exponent then
        begin
          // 相等，并起来塞到 Dest 里
          if Dest[K] = nil then
            Dest[K] := TCnExponentBigNumberPair.Create;
          Dest[K].Exponent := P1.Exponent;

          if Add then
            BigNumberAdd(Dest[K].Value, P1.Value, P2.Value)
          else
            BigNumberSub(Dest[K].Value, P1.Value, P2.Value);

          Inc(I);
          Inc(J);
          Inc(K);
        end
        else if P1.Exponent < P2.Exponent then
        begin
          // P1 小，把 P1 搁 Dest[K] 里
          if Dest[K] = nil then
            Dest[K] := TCnExponentBigNumberPair.Create;
          Dest[K].Exponent := P1.Exponent;

          BigNumberCopy(Dest[K].Value, P1.Value);
          Inc(I);
          Inc(K);
        end
        else // P2 小，把 P2 搁 Dest[K] 里
        begin
          if Dest[K] = nil then
            Dest[K] := TCnExponentBigNumberPair.Create;
          Dest[K].Exponent := P2.Exponent;

          BigNumberCopy(Dest[K].Value, P2.Value);
          if not Add then
            Dest[K].Value.Negate;
          Inc(J);
          Inc(K);
        end;
      end;

      if (I = Source1.Count) and (J = Source2.Count) then
      begin
        Dest.Compact;
        Exit;
      end;

      // 剩下哪个有，就全加到 Dest 里 K 开始的位置去
      if I = Source1.Count then
      begin
        for I := J to Source2.Count - 1 do
        begin
          if K >= Dest.Count then
            Dest.Add(TCnExponentBigNumberPair.Create)
          else if Dest[K] = nil then
            Dest[K] := TCnExponentBigNumberPair.Create;

          Dest[K].Exponent := Source2[I].Exponent;
          BigNumberCopy(Dest[K].Value, Source2[I].Value);
          Inc(K);
        end;
      end
      else if J = Source2.Count then
      begin
        for J := I to Source1.Count - 1 do
        begin
          if K >= Dest.Count then
            Dest.Add(TCnExponentBigNumberPair.Create)
          else if Dest[K] = nil then
            Dest[K] := TCnExponentBigNumberPair.Create;

          Dest[K].Exponent := Source1[J].Exponent;
          BigNumberCopy(Dest[K].Value, Source1[J].Value);
          Inc(K);
        end;
      end;
      Dest.Compact;
    end
    else if Dest = Source1 then // Dest 是 Source1，且 Source1 和 Source2 不同
    begin
      // 遍历 Source2，加塞到 Source1 中
      for I := 0 to Source2.Count - 1 do
      begin
        P2 := Source2[I];
        if Add then
          BigNumberAdd(Dest.SafeValue[P2.Exponent], Dest.SafeValue[P2.Exponent], P2.Value)
        else
          BigNumberSub(Dest.SafeValue[P2.Exponent], Dest.SafeValue[P2.Exponent], P2.Value);
      end;
    end
    else if Dest = Source2 then // Dest 是 Source2，且 Source1 和 Source2 不同
    begin
      // 遍历 Source1，加塞到 Source2 中
      for I := 0 to Source1.Count - 1 do
      begin
        P1 := Source1[I];
        if Add then
          BigNumberAdd(Dest.SafeValue[P1.Exponent], Dest.SafeValue[P1.Exponent], P1.Value)
        else
          BigNumberSub(Dest.SafeValue[P1.Exponent], Dest.SafeValue[P1.Exponent], P1.Value);
      end;
    end;
  end;
end;

{ TCnBigNumber }

function TCnBigNumber.AddWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberAddWord(Self, W);
end;

procedure TCnBigNumber.Clear;
begin
  BigNumberClear(Self);
end;

function TCnBigNumber.ClearBit(N: Integer): Boolean;
begin
  Result := BigNumberClearBit(Self, N);
end;

constructor TCnBigNumber.Create;
begin
  inherited;
  Top := 0;
  Neg := 0;
  DMax := 0;
  D := nil;
end;

destructor TCnBigNumber.Destroy;
begin
{$IFDEF DEBUG}
  if FIsFromPool then
    raise ECnBigNumberException.Create(SCnErrorBigNumberFreeFromPool);
{$ENDIF}

  if D <> nil then
    FreeMemory(D);

  D := nil;
  inherited;
end;

function TCnBigNumber.DivWord(W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := BigNumberDivWord(Self, W);
end;

class function TCnBigNumber.FromBinary(Buf: PAnsiChar;
  ByteLen: Integer): TCnBigNumber;
begin
  Result := BigNumberFromBinary(Buf, ByteLen);
end;

class function TCnBigNumber.FromBytes(const Buf: TBytes): TCnBigNumber;
begin
  Result := BigNumberFromBytes(Buf);
end;

function TCnBigNumber.ToBytes: TBytes;
begin
  Result := BigNumberToBytes(Self);
end;

class function TCnBigNumber.FromDec(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromDec(Buf);
end;

class function TCnBigNumber.FromHex(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromHex(Buf);
end;

function TCnBigNumber.GetBitsCount: Integer;
begin
  Result := BigNumberGetBitsCount(Self);
end;

function TCnBigNumber.GetBytesCount: Integer;
begin
  Result := BigNumberGetBytesCount(Self);
end;

function TCnBigNumber.GetCardinal: Cardinal;
begin
  Result := BigNumberGetWord(Self);
end;

function TCnBigNumber.GetWord: Cardinal;
begin
  Result := BigNumberGetWord(Self);
end;

{$IFDEF SUPPORT_UINT64}

function TCnBigNumber.GetUInt64: UInt64;
begin
  Result := BigNumberGetUInt64(Self);
end;

{$ENDIF}

procedure TCnBigNumber.Init;
begin
  BigNumberInit(Self);
end;

function TCnBigNumber.IsBitSet(N: Integer): Boolean;
begin
  Result := BigNumberIsBitSet(Self, N);
end;

function TCnBigNumber.IsNegative: Boolean;
begin
  Result := BigNumberIsNegative(Self);
end;

function TCnBigNumber.IsOdd: Boolean;
begin
  Result := BigNumberIsOdd(Self);
end;

function TCnBigNumber.IsEven: Boolean;
begin
  Result := BigNumberIsEven(Self);
end;

function TCnBigNumber.IsOne: Boolean;
begin
  Result := BigNumberIsOne(Self);
end;

function TCnBigNumber.IsNegOne: Boolean;
begin
  Result := BigNumberIsNegOne(Self);
end;

function TCnBigNumber.IsWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberIsWord(Self, W);
end;

function TCnBigNumber.IsCardinal: Boolean;
begin
  Result := BigNumberIsUInt32(Self);
end;

function TCnBigNumber.IsInteger: Boolean;
begin
  Result := BigNumberIsInt32(Self);
end;

function TCnBigNumber.IsInt64: Boolean;
begin
  Result := BigNumberIsInt64(Self);
end;

function TCnBigNumber.IsUInt64: Boolean;
begin
  Result := BigNumberIsUInt64(Self);
end;

function TCnBigNumber.IsZero: Boolean;
begin
  Result := BigNumberIsZero(Self);
end;

function TCnBigNumber.ModWord(W: TCnBigNumberElement): TCnBigNumberElement;
begin
  Result := BigNumberModWord(Self, W);
end;

function TCnBigNumber.MulWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberMulWord(Self, W);
end;

function TCnBigNumber.SetBit(N: Integer): Boolean;
begin
  Result := BigNumberSetBit(Self, N);
end;

function TCnBigNumber.SetDec(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetDec(Buf, Self);
end;

function TCnBigNumber.SetBinary(Buf: PAnsiChar; ByteLen: Integer): Boolean;
begin
  Result := BigNumberSetBinary(Buf, ByteLen, Self);
end;

function TCnBigNumber.SetHex(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetHex(Buf, Self);
end;

procedure TCnBigNumber.SetNegative(Negative: Boolean);
begin
  BigNumberSetNegative(Self, Negative);
end;

function TCnBigNumber.SetOne: Boolean;
begin
  Result := BigNumberSetOne(Self);
end;

function TCnBigNumber.SetCardinal(W: Cardinal): Boolean;
begin
  Result := BigNumberSetWord(Self, W);
end;

function TCnBigNumber.SetWord(W: Cardinal): Boolean;
begin
  Result := BigNumberSetWord(Self, W);
end;

{$IFDEF SUPPORT_UINT64}

function TCnBigNumber.SetUInt64(W: UInt64): Boolean;
begin
  Result := BigNumberSetUInt64(Self, W);
end;

{$ENDIF}

function TCnBigNumber.SetZero: Boolean;
begin
  Result := BigNumberSetZero(Self);
end;

function TCnBigNumber.SubWord(W: TCnBigNumberElement): Boolean;
begin
  Result := BigNumberSubWord(Self, W);
end;

function TCnBigNumber.ToBinary(const Buf: PAnsiChar; FixedLen: Integer): Integer;
begin
  Result := BigNumberToBinary(Self, Buf, FixedLen);
end;

function TCnBigNumber.ToDec: string;
begin
  Result := string(BigNumberToDec(Self));
end;

function TCnBigNumber.ToHex(FixedLen: Integer): string;
begin
  Result := BigNumberToHex(Self, FixedLen);
end;

function TCnBigNumber.ToString: string;
begin
  Result := BigNumberToString(Self);
end;

function TCnBigNumber.WordExpand(Words: Integer): TCnBigNumber;
begin
  Result := BigNumberWordExpand(Self, Words);
end;

function TCnBigNumber.GetDecString: string;
begin
  Result := ToDec;
end;

function TCnBigNumber.GetHexString: string;
begin
  Result := ToHex;
end;

function TCnBigNumber.GetDebugDump: string;
begin
  Result := BigNumberDebugDump(Self);
end;

function TCnBigNumber.RawDump(Mem: Pointer): Integer;
begin
  Result := BigNumberRawDump(Self, Mem);
end;

function TCnBigNumber.GetInt64: Int64;
begin
  Result := BigNumberGetInt64(Self);
end;

function TCnBigNumber.SetInt64(W: Int64): Boolean;
begin
  Result := BigNumberSetInt64(Self, W);
end;

procedure TCnBigNumber.Negate;
begin
  BigNumberNegate(Self);
end;

function TCnBigNumber.PowerWord(W: Cardinal): Boolean;
begin
  Result := BigNumberPower(Self, Self, W);
end;

function TCnBigNumber.GetTenPrecision: Integer;
begin
  Result := BigNumberGetTenPrecision(Self);
end;

procedure TCnBigNumber.ShiftLeft(N: Integer);
begin
  BigNumberShiftLeft(Self, Self, N);
end;

procedure TCnBigNumber.ShiftRight(N: Integer);
begin
  BigNumberShiftRight(Self, Self, N);
end;

procedure TCnBigNumber.ShiftLeftOne;
begin
  BigNumberShiftLeftOne(Self, Self);
end;

procedure TCnBigNumber.ShiftRightOne;
begin
  BigNumberShiftRightOne(Self, Self);
end;

function TCnBigNumber.GetInteger: Integer;
begin
  Result := BigNumberGetInteger(Self);
end;

function TCnBigNumber.SetInteger(W: Integer): Boolean;
begin
  Result := BigNumberSetInteger(Self, W);
end;

function TCnBigNumber.GetWordCount: Integer;
begin
  Result := BigNumberGetWordsCount(Self);
end;

function TCnBigNumber.GetHashCode: TCnHashCode;
var
  I: Integer;
begin
  // 把 32 位的内容全不管溢出地加起来，哪怕元素是 64 位也只加其低 32 位
  Result := 0;
  for I := 0 to Top - 1 do
    Result := Result + Integer(PCnBigNumberElementArray(D)^[I]);
end;

class function TCnBigNumber.FromFloat(F: Extended): TCnBigNumber;
begin
  Result := BigNumberFromFloat(F);
end;

function TCnBigNumber.SaveToStream(Stream: TStream;
  FixedLen: Integer): Integer;
begin
  Result := BigNumberWriteBinaryToStream(Self, Stream, FixedLen);
end;

function TCnBigNumber.LoadFromStream(Stream: TStream): Boolean;
begin
  Result := BigNumberReadBinaryFromStream(Self, Stream);
end;

class function TCnBigNumber.FromBase64(const Buf: AnsiString): TCnBigNumber;
begin
  Result := BigNumberFromBase64(Buf);
end;

function TCnBigNumber.SetBase64(const Buf: AnsiString): Boolean;
begin
  Result := BigNumberSetBase64(Buf, Self);
end;

function TCnBigNumber.ToBase64: string;
begin
  Result := BigNumberToBase64(Self);
end;

function TCnBigNumber.SetFloat(F: Extended): Boolean;
begin
  Result := BigNumberSetFloat(F, Self);
end;

function TCnBigNumber.IsFloat: Boolean;
begin
  Result := BigNumberIsFloat(Self);
end;

function TCnBigNumber.GetFloat: Extended;
begin
  Result := BigNumberGetFloat(Self);
end;

{ TCnBigNumberList }

function TCnBigNumberList.Add(ABigNumber: TCnBigNumber): Integer;
begin
  Result := inherited Add(ABigNumber);
end;

function TCnBigNumberList.Add: TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
  Add(Result);
end;

function TCnBigNumberList.Add(Num: Integer): TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
  Result.SetInteger(Num);
  Add(Result);
end;

procedure TCnBigNumberList.AddList(List: TCnBigNumberList);
var
  I: Integer;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
      Add(BigNumberDuplicate(List[I]));
  end;
end;

procedure TCnBigNumberList.BigNumberSort;
begin
  inherited Sort(DefBigNumberCompare);
end;

constructor TCnBigNumberList.Create;
begin
  inherited Create(True);
end;

destructor TCnBigNumberList.Destroy;
begin

  inherited;
end;

function TCnBigNumberList.GetItem(Index: Integer): TCnBigNumber;
begin
  Result := TCnBigNumber(inherited GetItem(Index));
end;

function TCnBigNumberList.IndexOfValue(ABigNumber: TCnBigNumber): Integer;
begin
  Result := 0;
  while (Result < Count) and (BigNumberCompare(Items[Result], ABigNumber) <> 0) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TCnBigNumberList.Insert(Index: Integer;
  ABigNumber: TCnBigNumber);
begin
  inherited Insert(Index, ABigNumber);
end;

function TCnBigNumberList.Remove(ABigNumber: TCnBigNumber): Integer;
begin
  Result := inherited Remove(ABigNumber);
end;

procedure TCnBigNumberList.RemoveDuplicated;
var
  I, Idx: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    // 去除重复的项
    Idx := IndexOfValue(Items[I]);
    if (Idx >= 0) and (Idx <> I) then
      Delete(I);
  end;
end;

procedure TCnBigNumberList.SetItem(Index: Integer;
  ABigNumber: TCnBigNumber);
begin
  inherited SetItem(Index, ABigNumber);
end;

procedure TCnBigNumberList.SumTo(Sum: TCnBigNumber);
var
  I: Integer;
begin
  Sum.SetZero;
  for I := 0 to Count - 1 do
    BigNumberAdd(Sum, Sum, Items[I]);
end;

function TCnBigNumberList.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := Items[I].ToDec
    else
      Result := Result + ',' + Items[I].ToDec;
  end;
end;

{ TCnBigNumberPool }

function TCnBigNumberPool.CreateObject: TObject;
begin
  Result := TCnBigNumber.Create;
end;

function TCnBigNumberPool.Obtain: TCnBigNumber;
begin
  Result := TCnBigNumber(inherited Obtain);
  Result.Clear;
end;

procedure TCnBigNumberPool.Recycle(Num: TCnBigNumber);
begin
  inherited Recycle(Num);
end;

{ TCnExponentBigNumberPair }

constructor TCnExponentBigNumberPair.Create;
begin
  inherited;
  FValue := TCnBigNumber.Create;
  FValue.SetZero;
end;

destructor TCnExponentBigNumberPair.Destroy;
begin
  FValue.Free;
  inherited;
end;

function TCnExponentBigNumberPair.ToString: string;
begin
  Result := FValue.ToDec + '^' + IntToStr(FExponent);
end;

{ TCnSparseBigNumberList }

function TCnSparseBigNumberList.AddPair(AExponent: Integer;
  Num: TCnBigNumber): TCnExponentBigNumberPair;
begin
  Result := TCnExponentBigNumberPair.Create;
  Result.Exponent := AExponent;
  BigNumberCopy(Result.Value, Num);
  Add(Result);
end;

procedure TCnSparseBigNumberList.AssignTo(Dest: TCnSparseBigNumberList);
begin
  SparseBigNumberListCopy(Dest, Self);
end;

function TCnSparseBigNumberList.BinarySearchExponent(AExponent: Integer;
  var OutIndex: Integer): Boolean;
var
  I, Start,Stop, Mid: Integer;
  Pair: TCnExponentBigNumberPair;
  BreakFromStart: Boolean;
begin
  Result := False;
  if Count = 0 then
  begin
    OutIndex := MaxInt;
  end
  else if Count <= SPARSE_BINARY_SEARCH_THRESHOLD then
  begin
    // 数量少，直接搜
    for I := 0 to Count - 1 do
    begin
      Pair := Items[I];
      if Pair.Exponent = AExponent then
      begin
        Result := True;
        OutIndex := I;
        Exit;
      end
      else if Pair.Exponent > AExponent then
      begin
        OutIndex := I;
        Exit;
      end;
    end;
    // AExponent 比最后一个 Pair 的还大
    OutIndex := MaxInt;
  end
  else
  begin
    Pair := Top;
    if Pair.Exponent < AExponent then      // AExponent 比最后一个 Pair 的还大
    begin
      OutIndex := MaxInt;
      Exit;
    end
    else if Pair.Exponent = AExponent then // AExponent 正好是最后一个 Pair
    begin
      OutIndex := Count - 1;
      Result := True;
      Exit;
    end
    else
    begin
      Pair := Bottom;
      if Pair.Exponent > AExponent then    // AExponent 比第一个 Pair 的还小
      begin
        OutIndex := 0;
        Exit;
      end
      else if Pair.Exponent = AExponent then // AExponent 正好是第一个 Pair
      begin
        OutIndex := 0;
        Result := True;
        Exit;
      end
    end;

    // 开始真正的二分查找
    Start := 0;
    Stop := Count - 1;
    Mid := 0;
    BreakFromStart := False;

    while Start <= Stop do
    begin
      Mid := (Start + Stop) div 2;

      Pair := Items[Mid];
      if Pair.Exponent = AExponent then
      begin
        Result := True;
        OutIndex := Mid;
        Exit;
      end
      else if Pair.Exponent < AExponent then
      begin
        Start := Mid + 1; // 如果最后一次是从这退出，则插入位置为 Mid + 1
        BreakFromStart := True;
      end
      else if Pair.Exponent > AExponent then
      begin
        Stop := Mid - 1;  // 如果最后一次是从这退出，则插入位置为 Mid - 1
        BreakFromStart := False;
      end;
    end;

    if BreakFromStart then
      OutIndex := Mid + 1
    else
      OutIndex := Mid;
    Result := False;
  end;
end;

function TCnSparseBigNumberList.Bottom: TCnExponentBigNumberPair;
begin
  Result := nil;
  if Count > 0 then
    Result := Items[0];
end;

procedure TCnSparseBigNumberList.Compact;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if (Items[I] = nil) or Items[I].Value.IsZero then
      Delete(I);
end;

constructor TCnSparseBigNumberList.Create;
begin
  inherited Create(True);
end;

function TCnSparseBigNumberList.GetItem(Index: Integer): TCnExponentBigNumberPair;
begin
  Result := TCnExponentBigNumberPair(inherited GetItem(Index));
end;

function TCnSparseBigNumberList.GetReadonlyValue(Exponent: Integer): TCnBigNumber;
var
  OutIndex: Integer;
begin
  if not BinarySearchExponent(Exponent, OutIndex) then
    Result := CnBigNumberZero
  else
    Result := Items[OutIndex].Value;
end;

function TCnSparseBigNumberList.GetSafeValue(Exponent: Integer): TCnBigNumber;
var
  OutIndex: Integer;
begin
  if not BinarySearchExponent(Exponent, OutIndex) then
  begin
    // 未找到，要插入
    OutIndex := InsertByOutIndex(OutIndex);
    Items[OutIndex].Exponent := Exponent;
  end;
  Result := Items[OutIndex].Value;
end;

function TCnSparseBigNumberList.InsertByOutIndex(
  OutIndex: Integer): Integer;
var
  Pair: TCnExponentBigNumberPair;
begin
  if OutIndex < 0 then
    OutIndex := 0;

  Pair := TCnExponentBigNumberPair.Create;
  if OutIndex >= Count then
  begin
    Add(Pair);
    Result := Count - 1;
  end
  else
  begin
    Insert(OutIndex, Pair);
    Result := OutIndex;
  end;
end;

procedure TCnSparseBigNumberList.Negate;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if (Items[I] <> nil) then
      Items[I].Value.Negate;
end;

procedure TCnSparseBigNumberList.SetItem(Index: Integer;
  const Value: TCnExponentBigNumberPair);
begin
  inherited SetItem(Index, Value);
end;

procedure TCnSparseBigNumberList.SetSafeValue(Exponent: Integer;
  const Value: TCnBigNumber);
var
  OutIndex: Integer;
begin
  if not BinarySearchExponent(Exponent, OutIndex) then
  begin
    // 未找到，如果 Value 无或 0，则要插入
    if (Value <> nil) and not Value.IsZero then
    begin
      OutIndex := InsertByOutIndex(OutIndex);
      Items[OutIndex].Exponent := Exponent;
    end
    else // 未找到且是 0，不动
      Exit;
  end;

  // 找到了或者插入了，赋值
  if (Value <> nil) and not Value.IsZero then
    BigNumberCopy(Items[OutIndex].Value, Value)
  else
    Items[OutIndex].Value.SetZero;
end;

procedure TCnSparseBigNumberList.SetValues(LowToHighList: array of Int64);
var
  I: Integer;
  Pair: TCnExponentBigNumberPair;
begin
  Clear;
  for I := Low(LowToHighList) to High(LowToHighList) do
  begin
    Pair := TCnExponentBigNumberPair.Create;
    Pair.Exponent := I;
    Pair.Value.SetInt64(LowToHighList[I]);
    Add(Pair);
  end;
end;

function TCnSparseBigNumberList.Top: TCnExponentBigNumberPair;
begin
  Result := nil;
  if Count > 0 then
  begin
{$IFDEF LIST_INDEX_NATIVEINT}
    Result := TCnExponentBigNumberPair(Items[Count - 1]);
{$ELSE}
    Result := Items[Count - 1];
{$ENDIF}
  end;
end;

function TCnSparseBigNumberList.ToString: string;
var
  I: Integer;
  IsFirst: Boolean;
begin
  Result := '';
  IsFirst := True;
  for I := Count - 1 downto 0 do
  begin
    if IsFirst then
    begin
      Result := Items[I].ToString;
      IsFirst := False;
    end
    else
      Result := Result + CRLF + Items[I].ToString;
  end;
end;

{ TCnBigNumberHashMap }

constructor TCnBigNumberHashMap.Create(AOwnsKey, AOwnsValue: Boolean);
begin
  inherited Create;
  FOwnsKey := AOwnsKey;
  FOwnsValue := AOwnsValue;
end;

procedure TCnBigNumberHashMap.DoFreeNode(Node: TCnHashNode);
begin
  if FOwnsKey then
  begin
    Node.Key.Free;
    Node.Key := nil;
  end;
  if FOwnsValue then
  begin
    Node.Value.Free;
    Node.Value := nil;
  end;

  inherited;
end;

function TCnBigNumberHashMap.Find(Key: TCnBigNumber): TCnBigNumber;
begin
  Result := TCnBigNumber(inherited Find(Key));
end;

function TCnBigNumberHashMap.HashCodeFromObject(Obj: TObject): Integer;
begin
  if Obj is TCnBigNumber then // 显式写明，以保证低版本手动调用 GetHashCode
    Result := TCnBigNumber(Obj).GetHashCode
  else                        // 其余情况让父类根据编译器版本决定是否调用 GetHashCode
    Result := inherited HashCodeFromObject(Obj)
end;

function TCnBigNumberHashMap.KeyEqual(Key1: TObject; Key2: TObject
  {$IFNDEF CPU64BITS}; Key132: TObject; Key232: TObject {$ENDIF}): Boolean;
begin
  Result := BigNumberEqual(TCnBigNumber(Key1), TCnBigNumber(Key2));
end;

initialization
  FLocalBigNumberPool := TCnBigNumberPool.Create;

  CnBigNumberOne := TCnBigNumber.Create;
  CnBigNumberOne.SetOne;
  CnBigNumberZero := TCnBigNumber.Create;
  CnBigNumberZero.SetZero;
  CnBigNumberNegOne := TCnBigNumber.Create;
  CnBigNumberNegOne.SetInteger(-1);

finalization
  CnBigNumberNegOne.Free;
  CnBigNumberZero.Free;
  CnBigNumberOne.Free;

  FLocalBigNumberPool.Free;
  FLocalBigBinaryPool.Free;

end.

