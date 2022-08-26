{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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

unit CnNative;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：32 位和 64 位的一些统一声明以及一堆基础实现
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：Delphi XE 2 支持 32 和 64 以来，开放出的 NativeInt 和 NativeUInt 随
*           当前是 32 位还是 64 而动态变化，影响到的是 Pointer、Reference等东西。
*           考虑到兼容性，固定长度的 32 位 Cardinal/Integer 等和 Pointer 这些就
*           不能再通用了，即使 32 位下也被编译器禁止。因此本单元声明了几个类型，
*           供同时在低版本和高版本的 Delphi 中使用。
*           后来加入 UInt64 的包装，注意 D567 下不直接支持 UInt64 的运算，需要用
*           辅助函数实现，目前实现了 div 与 mod
*           另外地址运算 Integer(APtr) 在 64 位下尤其是 MacOS 上容易出现截断，需要用 NativeInt
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 XE 2
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.07.23 V2.2
*               增加几个内存位运算函数与二进制转换字符串函数，并改名为 CnNative
*           2022.06.08 V2.1
*               增加四个时间固定的交换函数以及内存倒排函数
*           2022.03.14 V2.0
*               增加几个十六进制转换函数
*           2022.02.17 V1.9
*               增加 FPC 的编译支持
*           2022.02.09 V1.8
*               加入运行期的大小端判断函数
*           2021.09.05 V1.7
*               加入 Int64/UInt64 的整数次幂与根的运算函数
*           2020.10.28 V1.6
*               加入 UInt64 溢出相关的判断与运算函数
*           2020.09.06 V1.5
*               加入求 UInt64 整数平方根的函数
*           2020.07.01 V1.5
*               加入判断 32 位与 64 位有无符号数相加是否溢出的函数
*           2020.06.20 V1.4
*               加入 32 位与 64 位获取最高与最低的 1 位位置的函数
*           2020.01.01 V1.3
*               加入 32 位无符号整型的 mul 运算，在不支持 UInt64 的系统上以 Int64 代替以避免溢出
*           2018.06.05 V1.2
*               加入 64 位整型的 div/mod 运算，在不支持 UInt64 的系统上以 Int64 代替 
*           2016.09.27 V1.1
*               加入 64 位整型的一些定义
*           2011.07.06 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, SysConst, Math {$IFDEF COMPILER5}, Windows {$ENDIF};
                                    // D5 下需要引用 Windows 中的 PByte
type
{$IFDEF SUPPORT_32_AND_64}
  TCnNativeInt     = NativeInt;
  TCnNativeUInt    = NativeUInt;
  TCnNativePointer = NativeUInt;
{$ELSE}
  TCnNativeInt     = Integer;
  TCnNativeUInt    = Cardinal;
  TCnNativePointer = Cardinal;
{$ENDIF}

{$IFDEF CPUX64}
  TCnUInt64        = NativeUInt;
  TCnInt64         = NativeInt;
{$ELSE}
  {$IFDEF SUPPORT_UINT64}
  TCnUInt64        = UInt64;
  {$ELSE}
  TCnUInt64 = packed record  // 只能用这样的结构代替
    case Boolean of
      True:  (Value: Int64);
      False: (Lo32, Hi32: Cardinal);
  end;
  {$ENDIF}
  TCnInt64         = Int64;
{$ENDIF}

// TUInt64 用于 cnvcl 库中不支持 UInt64 的运算如 div mod 等
{$IFDEF SUPPORT_UINT64}
  TUInt64          = UInt64;
  {$IFNDEF SUPPORT_PUINT64}
  PUInt64          = ^UInt64;
  {$ENDIF}
{$ELSE}
  TUInt64          = Int64;
  PUInt64          = ^TUInt64;
{$ENDIF}

{$IFNDEF SUPPORT_INT64ARRAY}
  // 如果系统没有定义 Int64Array
  Int64Array  = array[0..$0FFFFFFE] of Int64;
  PInt64Array = ^Int64Array;
{$ENDIF}

  TUInt64Array = array of TUInt64; // 这个动态数组声明似乎容易和静态数组声明有冲突

  ExtendedArray = array[0..65537] of Extended;
  PExtendedArray = ^ExtendedArray;

  PCnWord16Array = ^TCnWord16Array;
  TCnWord16Array = array [0..0] of Word;

{$IFDEF POSIX64}
  TCnLongWord32 = Cardinal; // Linux64/MacOS64 (or POSIX64?) LongWord is 64 Bits
{$ELSE}
  TCnLongWord32 = LongWord;
{$ENDIF}
  PCnLongWord32 = ^TCnLongWord32;

  TCnLongWord32Array = array [0..MaxInt div SizeOf(Integer) - 1] of TCnLongWord32;

  PCnLongWord32Array = ^TCnLongWord32Array;

{$IFNDEF TBYTES_DEFINED}
  TBytes = array of Byte;
  {* 无符号字节数组，未定义时定义上}
{$ENDIF}

  TShortInts = array of ShortInt;
  {* 有符号字节数组}

  PCnByte = ^Byte;
  PCnWord = ^Word;

{$IFDEF COMPILER5}
  PCardinal = ^Cardinal;
  {* D5 下 System 单元中未定义，定义上}
  PByte = Windows.PByte;
  {* D5 下 PByte 定义在 Windows 中，其他版本定义在 System 中，
    这里统一一下供外界使用 PByte 时无需 uses Windows，以有利于跨平台}
{$ENDIF}

  TCnBitOperation = (boAnd, boOr, boXor, boNot);
  {* 位操作类型}

type
  TMemSortCompareProc = function (P1, P2: Pointer; ElementByteSize: Integer): Integer;
  {* 内存固定块尺寸的数组排序比较函数原型}

const
  MAX_SQRT_INT64: Cardinal               = 3037000499;
  MAX_UINT16: Word                       = $FFFF;
  MAX_UINT32: Cardinal                   = $FFFFFFFF;
  MAX_TUINT64: TUInt64                   = $FFFFFFFFFFFFFFFF;
  MAX_SIGNED_INT64_IN_TUINT64: TUInt64   = $7FFFFFFFFFFFFFFF;

{*
  对于 D567 等不支持 UInt64 的编译器，虽然可以用 Int64 代替 UInt64 进行加减、存储
  但乘除运算则无法直接完成，这里封装了两个调用 System 库中的 _lludiv 与 _llumod
  函数，实现以 Int64 表示的 UInt64 数据的 div 与 mod 功能。
}
function UInt64Mod(A, B: TUInt64): TUInt64;
{* 两个 UInt64 求余}

function UInt64Div(A, B: TUInt64): TUInt64;
{* 两个 UInt64 整除}

function UInt64Mul(A, B: Cardinal): TUInt64;
{* 无符号 32 位整数不溢出的相乘，在不支持 UInt64 的平台上，结果以 UInt64 的形式放在 Int64 里，
  如果结果直接使用 Int64 计算则有可能溢出}

procedure UInt64AddUInt64(A, B: TUInt64; var ResLo, ResHi: TUInt64);
{* 两个无符号 64 位整数相加，处理溢出的情况，结果放 ResLo 与 ResHi 中
  注：内部实现按算法来看较为复杂，实际上如果溢出，ResHi 必然是 1，直接判断溢出并将其设 1 即可}

procedure UInt64MulUInt64(A, B: TUInt64; var ResLo, ResHi: TUInt64);
{* 两个无符号 64 位整数相乘，结果放 ResLo 与 ResHi 中，64 位下用汇编实现，提速约一倍以上}

function UInt64ToHex(N: TUInt64): string;
{* 将 UInt64 转换为十六进制字符串}

function UInt64ToStr(N: TUInt64): string;
{* 将 UInt64 转换为字符串}

function StrToUInt64(const S: string): TUInt64;
{* 将字符串转换为 UInt64}

function UInt64Compare(A, B: TUInt64): Integer;
{* 比较两个 UInt64 值，分别根据 > = < 返回 1、0、-1}

function UInt64Sqrt(N: TUInt64): TUInt64;
{* 求 UInt64 的平方根的整数部分}

function UInt32IsNegative(N: Cardinal): Boolean;
{* 该 Cardinal 被当成 Integer 时是否小于 0}

function UInt64IsNegative(N: TUInt64): Boolean;
{* 该 UInt64 被当成 Int64 时是否小于 0}

procedure UInt64SetBit(var B: TUInt64; Index: Integer);
{* 给 UInt64 的某一位置 1，位 Index 从 0 开始}

procedure UInt64ClearBit(var B: TUInt64; Index: Integer);
{* 给 UInt64 的某一位置 0，位 Index 从 0 开始}

function GetUInt64BitSet(B: TUInt64; Index: Integer): Boolean;
{* 返回 UInt64 的某一位是否是 1，位 Index 从 0 开始}

function GetUInt64HighBits(B: TUInt64): Integer;
{* 返回 UInt64 的是 1 的最高二进制位是第几位，最低位是 0，如果没有 1，返回 -1}

function GetUInt32HighBits(B: Cardinal): Integer;
{* 返回 Cardinal 的是 1 的最高二进制位是第几位，最低位是 0，如果没有 1，返回 -1}

function GetUInt64LowBits(B: TUInt64): Integer;
{* 返回 Int64 的是 1 的最低二进制位是第几位，最低位是 0，基本等同于末尾几个 0。如果没有 1，返回 -1}

function GetUInt32LowBits(B: Cardinal): Integer;
{* 返回 Cardinal 的是 1 的最低二进制位是第几位，最低位是 0，基本等同于末尾几个 0。如果没有 1，返回 -1}

function Int64Mod(M, N: Int64): Int64;
{* 封装的 Int64 Mod，M 碰到负值时取反求模再模减，但 N 仍要求正数否则结果不靠谱}

function IsUInt32PowerOf2(N: Cardinal): Boolean;
{* 判断一 32 位无符号整数是否 2 的整数次幂}

function IsUInt64PowerOf2(N: TUInt64): Boolean;
{* 判断一 64 位无符号整数是否 2 的整数次幂}

function GetUInt32PowerOf2GreaterEqual(N: Cardinal): Cardinal;
{* 得到一比指定 32 位无符号整数数大或等的 2 的整数次幂，如溢出则返回 0}

function GetUInt64PowerOf2GreaterEqual(N: TUInt64): TUInt64;
{* 得到一比指定 64 位无符号整数数大或等的 2 的整数次幂，如溢出则返回 0}

function IsInt32AddOverflow(A, B: Integer): Boolean;
{* 判断两个 32 位有符号数相加是否溢出 32 位有符号上限}

function IsUInt32AddOverflow(A, B: Cardinal): Boolean;
{* 判断两个 32 位无符号数相加是否溢出 32 位无符号上限}

function IsInt64AddOverflow(A, B: Int64): Boolean;
{* 判断两个 64 位有符号数相加是否溢出 64 位有符号上限}

function IsUInt64AddOverflow(A, B: TUInt64): Boolean;
{* 判断两个 64 位无符号数相加是否溢出 64 位无符号上限}

procedure UInt64Add(var R: TUInt64; A, B: TUInt64; out Carry: Integer);
{* 两个 64 位无符号数相加，A + B => R，如果有溢出，则溢出的 1 搁进位标记里，否则清零}

procedure UInt64Sub(var R: TUInt64; A, B: TUInt64; out Carry: Integer);
{* 两个 64 位无符号数相减，A - B => R，如果不够减有借位，则借的 1 搁借位标记里，否则清零}

function IsInt32MulOverflow(A, B: Integer): Boolean;
{* 判断两个 32 位有符号数相乘是否溢出 32 位有符号上限}

function IsUInt32MulOverflow(A, B: Cardinal): Boolean;
{* 判断两个 32 位无符号数相乘是否溢出 32 位无符号上限}

function IsUInt32MulOverflowInt64(A, B: Cardinal; out R: TUInt64): Boolean;
{* 判断两个 32 位无符号数相乘是否溢出 64 位有符号数，如未溢出也即返回 False 时，R 中直接返回结果
  如溢出也即返回 True，外界需要重新调用 UInt64Mul 才能实施相乘}

function IsInt64MulOverflow(A, B: Int64): Boolean;
{* 判断两个 64 位有符号数相乘是否溢出 64 位有符号上限}

function PointerToInteger(P: Pointer): Integer;
{* 指针类型转换成整型，支持 32/64 位}

function IntegerToPointer(I: Integer): Pointer;
{* 整型转换成指针类型，支持 32/64 位}

function Int64NonNegativeAddMod(A, B, N: Int64): Int64;
{* 求 Int64 范围内俩加数的和求余，处理溢出的情况，要求 N 大于 0}

function UInt64NonNegativeAddMod(A, B, N: TUInt64): TUInt64;
{* 求 UInt64 范围内俩加数的和求余，处理溢出的情况，要求 N 大于 0}

function Int64NonNegativeMulMod(A, B, N: Int64): Int64;
{* Int64 范围内的相乘求余，不能直接计算，容易溢出。要求 N 大于 0}

function UInt64NonNegativeMulMod(A, B, N: TUInt64): TUInt64;
{* UInt64 范围内的相乘求余，不能直接计算，容易溢出。}

function Int64NonNegativeMod(N: Int64; P: Int64): Int64;
{* 封装的 Int64 非负求余函数，也就是余数为负时，加个除数变正，调用者需保证 P 大于 0}

function Int64NonNegativPower(N: Int64; Exp: Integer): Int64;
{* Int64 的非负整数指数幂，不考虑溢出的情况}

function Int64NonNegativeRoot(N: Int64; Exp: Integer): Int64;
{* 求 Int64 的非负整数次方根的整数部分，不考虑溢出的情况}

function UInt64NonNegativPower(N: TUInt64; Exp: Integer): TUInt64;
{* UInt64 的非负整数指数幂，不考虑溢出的情况}

function UInt64NonNegativeRoot(N: TUInt64; Exp: Integer): TUInt64;
{* 求 UInt64 的非负整数次方根的整数部分，不考虑溢出的情况}

function CurrentByteOrderIsBigEndian: Boolean;
{* 返回当前运行期环境是否是大端，也就是是否将整数中的高序字节存储在较低的起始地址，如部分指定的 ARM 和 MIPS}

function CurrentByteOrderIsLittleEndian: Boolean;
{* 返回当前运行期环境是否是小端，也就是是否将整数中的高序字节存储在较高的起始地址，如 x86 与部分默认 arm}

function Int64ToBigEndian(Value: Int64): Int64;
{* 确保 Int64 值为大端，在小端环境中会进行转换}

function Int32ToBigEndian(Value: Integer): Integer;
{* 确保 Int32 值为大端，在小端环境中会进行转换}

function Int16ToBigEndian(Value: SmallInt): SmallInt;
{* 确保 Int16 值为大端，在小端环境中会进行转换}

function Int64ToLittleEndian(Value: Int64): Int64;
{* 确保 Int64 值为小端，在大端环境中会进行转换}

function Int32ToLittleEndian(Value: Integer): Integer;
{* 确保 Int32 值为小端，在大端环境中会进行转换}

function Int16ToLittleEndian(Value: SmallInt): SmallInt;
{* 确保 Int16 值为小端，在大端环境中会进行转换}

function Int64HostToNetwork(Value: Int64): Int64;
{* 将 Int64 值从主机字节顺序转换为网络字节顺序，在小端环境中会进行转换}

function Int32HostToNetwork(Value: Integer): Integer;
{* 将 Int32 值从主机字节顺序转换为网络字节顺序，在小端环境中会进行转换}

function Int16HostToNetwork(Value: SmallInt): SmallInt;
{* 将 Int16 值从主机字节顺序转换为网络字节顺序，在小端环境中会进行转换}

function Int64NetworkToHost(Value: Int64): Int64;
{* 将 Int64 值从网络字节顺序转换为主机字节顺序，在小端环境中会进行转换}

function Int32NetworkToHost(Value: Integer): Integer;
{* 将 Int32值从网络字节顺序转换为主机字节顺序，在小端环境中会进行转换}

function Int16NetworkToHost(Value: SmallInt): SmallInt;
{* 将 Int16 值从网络字节顺序转换为主机字节顺序，在小端环境中会进行转换}

function UInt64HostToNetwork(Value: TUInt64): TUInt64;
{* 将 UInt64 值从主机字节顺序转换为网络字节顺序，在小端环境中会进行转换}

function UInt32HostToNetwork(Value: Cardinal): Cardinal;
{* 将 UInt32 值从主机字节顺序转换为网络字节顺序，在小端环境中会进行转换}

function UInt16HostToNetwork(Value: Word): Word;
{* 将 UInt16 值从主机字节顺序转换为网络字节顺序，在小端环境中会进行转换}

function UInt64NetworkToHost(Value: TUInt64): TUInt64;
{* 将 UInt64 值从网络字节顺序转换为主机字节顺序，在小端环境中会进行转换}

function UInt32NetworkToHost(Value: Cardinal): Cardinal;
{* 将 UInt32值从网络字节顺序转换为主机字节顺序，在小端环境中会进行转换}

function UInt16NetworkToHost(Value: Word): Word;
{* 将 UInt16 值从网络字节顺序转换为主机字节顺序，在小端环境中会进行转换}

procedure ReverseMemory(AMem: Pointer; MemByteLen: Integer);
{* 按字节顺序倒置一块内存块，字节内部不变}

function ReverseBitsInInt8(V: Byte): Byte;
{* 倒置一字节内容}

function ReverseBitsInInt16(V: Word): Word;
{* 倒置二字节内容}

function ReverseBitsInInt32(V: Cardinal): Cardinal;
{* 倒置四字节内容}

function ReverseBitsInInt64(V: Int64): Int64;
{* 倒置八字节内容}

procedure ReverseMemoryWithBits(AMem: Pointer; MemByteLen: Integer);
{* 按字节顺序倒置一块内存块，并且每个字节也倒过来}

procedure MemoryAnd(AMem, BMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
{* 两块长度相同的内存 AMem 和 BMem 按位与，结果放 ResMem 中，三者可相同}

procedure MemoryOr(AMem, BMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
{* 两块长度相同的内存 AMem 和 BMem 按位或，结果放 ResMem 中，三者可相同}

procedure MemoryXor(AMem, BMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
{* 两块长度相同的内存 AMem 和 BMem 按位异或，结果放 ResMem 中，三者可相同}

procedure MemoryNot(AMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
{* 一块内存 AMem 取反，结果放 ResMem 中，两者可相同}

procedure MemoryShiftLeft(AMem, BMem: Pointer; MemByteLen: Integer; BitCount: Integer);
{* AMem 整块内存左移 BitCount 位至 BMem，往内存地址低位移，空位补 0，两者可相等}

procedure MemoryShiftRight(AMem, BMem: Pointer; MemByteLen: Integer; BitCount: Integer);
{* AMem 整块内存右移 BitCount 位至 BMem，往内存地址高位移，空位补 0，两者可相等}

function MemoryIsBitSet(AMem: Pointer; N: Integer): Boolean;
{* 返回内存块某 Bit 位是否置 1，内存地址低位是 0，字节内还是右边为 0}

procedure MemorySetBit(AMem: Pointer; N: Integer);
{* 给内存块某 Bit 位置 1，内存地址低位是 0，字节内还是右边为 0}

procedure MemoryClearBit(AMem: Pointer; N: Integer);
{* 给内存块某 Bit 位置 0，内存地址低位是 0，字节内还是右边为 0}

function MemoryToBinStr(AMem: Pointer; MemByteLen: Integer; Sep: Boolean = False): string;
{* 将一块内存内容从低到高字节顺序输出为二进制字符串，Sep 表示是否空格分隔}

procedure MemorySwap(AMem, BMem: Pointer; MemByteLen: Integer);
{* 交换两块相同长度的内存块的内容}

function MemoryCompare(AMem, BMem: Pointer; MemByteLen: Integer): Integer;
{* 以无符号数的方式比较两块内存，返回 1、0、-1}

procedure MemoryQuickSort(Mem: Pointer; ElementByteSize: Integer;
  ElementCount: Integer; CompareProc: TMemSortCompareProc = nil);
{* 针对固定大小的元素的数组进行排序}

function UInt8ToBinStr(V: Byte): string;
{* 将一字节转换为二进制字符串}

function UInt16ToBinStr(V: Word): string;
{* 将一字转换为二进制字符串}

function UInt32ToBinStr(V: Cardinal): string;
{* 将一四字节整数转换为二进制字符串}

function UInt64ToBinStr(V: TUInt64): string;
{* 将一无符号 64 字节整数转换为二进制字符串}

function DataToHex(InData: Pointer; ByteLength: Integer; UseUpperCase: Boolean = True): string;
{* 内存块转换为十六进制字符串，内存低位的内容出现在字符串左方，相当于网络字节顺序，
  UseUpperCase 控制输出内容的大小写}

function HexToData(const Hex: string; OutData: Pointer): string;
{* 十六进制字符串转换为内存块，字符串左方的内容出现在内存低位，相当于网络字节顺序，
  十六进制字符串长度为奇或转换失败时抛出异常
  注意 OutData 应该指向足够容纳转换内容的区域，长度至少为 Length(Hex) div 2}

function StringToHex(const Data: string; UseUpperCase: Boolean = True): string;
{* 字符串转换为十六进制字符串，UseUpperCase 控制输出内容的大小写}

function HexToString(const Hex: string): string;
{* 十六进制字符串转换为字符串，十六进制字符串长度为奇或转换失败时抛出异常}

function BytesToHex(Data: TBytes; UseUpperCase: Boolean = True): string;
{* 字节数组转换为十六进制字符串，下标低位的内容出现在字符串左方，相当于网络字节顺序，
  UseUpperCase 控制输出内容的大小写}

function HexToBytes(const Hex: string): TBytes;
{* 十六进制字符串转换为字节数组，字符串左边的内容出现在下标低位，相当于网络字节顺序，
  字符串长度为奇或转换失败时抛出异常}

procedure ReverseBytes(Data: TBytes);
{* 按字节顺序倒置一字节数组}

procedure MoveMost(const Source; var Dest; ByteLen, MostLen: Integer);
{* 从 Source 移动 ByteLen 且不超过 MostLen 个字节到 Dest 中，
  如 ByteLen 小于 MostLen，则 Dest 填充 0，要求 Dest 容纳至少 MostLen}

procedure ConstantTimeConditionalSwap8(CanSwap: Boolean; var A, B: Byte);
{* 针对两个字节变量的执行时间固定的条件交换，CanSwap 为 True 时才实施 A B 交换}

procedure ConstantTimeConditionalSwap16(CanSwap: Boolean; var A, B: Word);
{* 针对两个双字节变量的执行时间固定的条件交换，CanSwap 为 True 时才实施 A B 交换}

procedure ConstantTimeConditionalSwap32(CanSwap: Boolean; var A, B: Cardinal);
{* 针对两个四字节变量的执行时间固定的条件交换，CanSwap 为 True 时才实施 A B 交换}

procedure ConstantTimeConditionalSwap64(CanSwap: Boolean; var A, B: TUInt64);
{* 针对两个八字节变量的执行时间固定的条件交换，CanSwap 为 True 时才实施 A B 交换}

{$IFDEF MSWINDOWS}

procedure Int64DivInt32Mod(A: Int64; B: Integer; var DivRes, ModRes: Integer);
{* 64 位有符号数除以 32 位有符号数，商放 DivRes，余数放 ModRes
  调用者须自行保证商在 32 位范围内，否则会抛溢出异常}

procedure UInt64DivUInt32Mod(A: TUInt64; B: Cardinal; var DivRes, ModRes: Cardinal);
{* 64 位无符号数除以 32 位无符号数，商放 DivRes，余数放 ModRes
  调用者须自行保证商在 32 位范围内，否则会抛溢出异常}

procedure Int128DivInt64Mod(ALo, AHi: Int64; B: Int64; var DivRes, ModRes: Int64);
{* 128 位有符号数除以 64 位有符号数，商放 DivRes，余数放 ModRes
  调用者须自行保证商在 64 位范围内，否则会抛溢出异常}

procedure UInt128DivUInt64Mod(ALo, AHi: TUInt64; B: TUInt64; var DivRes, ModRes: TUInt64);
{* 128 位有符号数除以 64 位有符号数，商放 DivRes，余数放 ModRes
  调用者须自行保证商在 64 位范围内，否则会抛溢出异常}

{$ENDIF}

function IsUInt128BitSet(Lo, Hi: TUInt64; N: Integer): Boolean;
{* 针对两个 Int64 拼成的 128 位数字，返回第 N 位是否为 1，N 从 0 到 127}

procedure SetUInt128Bit(var Lo, Hi: TUInt64; N: Integer);
{* 针对两个 Int64 拼成的 128 位数字，设置第 N 位为 1，N 从 0 到 127}

procedure ClearUInt128Bit(var Lo, Hi: TUInt64; N: Integer);
{* 针对两个 Int64 拼成的 128 位数字，清掉第 N 位，N 从 0 到 127}

implementation

uses
  CnFloatConvert;

var
  FByteOrderIsBigEndian: Boolean = False;

function CurrentByteOrderIsBigEndian: Boolean;
type
  TByteOrder = packed record
    case Boolean of
      False: (C: array[0..1] of Byte);
      True: (W: Word);
  end;
var
  T: TByteOrder;
begin
  T.W := $00CC;
  Result := T.C[1] = $CC;
end;

function CurrentByteOrderIsLittleEndian: Boolean;
begin
  Result := not CurrentByteOrderIsBigEndian;
end;

function SwapInt64(Value: Int64): Int64;
var
  Lo, Hi: Cardinal;
  Rec: Int64Rec;
begin
  Lo := Int64Rec(Value).Lo;
  Hi := Int64Rec(Value).Hi;
  Lo := ((Lo and $000000FF) shl 24) or ((Lo and $0000FF00) shl 8)
    or ((Lo and $00FF0000) shr 8) or ((Lo and $FF000000) shr 24);
  Hi := ((Hi and $000000FF) shl 24) or ((Hi and $0000FF00) shl 8)
    or ((Hi and $00FF0000) shr 8) or ((Hi and $FF000000) shr 24);
  Rec.Lo := Hi;
  Rec.Hi := Lo;
  Result := Int64(Rec);
end;

function SwapUInt64(Value: TUInt64): TUInt64;
var
  Lo, Hi: Cardinal;
  Rec: Int64Rec;
begin
  Lo := Int64Rec(Value).Lo;
  Hi := Int64Rec(Value).Hi;
  Lo := ((Lo and $000000FF) shl 24) or ((Lo and $0000FF00) shl 8)
    or ((Lo and $00FF0000) shr 8) or ((Lo and $FF000000) shr 24);
  Hi := ((Hi and $000000FF) shl 24) or ((Hi and $0000FF00) shl 8)
    or ((Hi and $00FF0000) shr 8) or ((Hi and $FF000000) shr 24);
  Rec.Lo := Hi;
  Rec.Hi := Lo;
  Result := TUInt64(Rec);
end;

function Int64ToBigEndian(Value: Int64): Int64;
begin
  if FByteOrderIsBigEndian then
    Result := Value
  else
    Result := SwapInt64(Value);
end;

function Int32ToBigEndian(Value: Integer): Integer;
begin
  if FByteOrderIsBigEndian then
    Result := Value
  else
    Result := Integer((Value and $000000FF) shl 24) or Integer((Value and $0000FF00) shl 8)
      or Integer((Value and $00FF0000) shr 8) or Integer((Value and $FF000000) shr 24);
end;

function Int16ToBigEndian(Value: SmallInt): SmallInt;
begin
  if FByteOrderIsBigEndian then
    Result := Value
  else
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8);
end;

function Int64ToLittleEndian(Value: Int64): Int64;
begin
  if not FByteOrderIsBigEndian then
    Result := Value
  else
    Result := SwapInt64(Value);
end;

function Int32ToLittleEndian(Value: Integer): Integer;
begin
  if not FByteOrderIsBigEndian then
    Result := Value
  else
    Result := Integer((Value and $000000FF) shl 24) or Integer((Value and $0000FF00) shl 8)
      or Integer((Value and $00FF0000) shr 8) or Integer((Value and $FF000000) shr 24);
end;

function Int16ToLittleEndian(Value: SmallInt): SmallInt;
begin
  if not FByteOrderIsBigEndian then
    Result := Value
  else
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8);
end;

function Int64HostToNetwork(Value: Int64): Int64;
begin
  if not FByteOrderIsBigEndian then
    Result := SwapInt64(Value)
  else
    Result := Value;
end;

function Int32HostToNetwork(Value: Integer): Integer;
begin
  if not FByteOrderIsBigEndian then
    Result := Integer((Value and $000000FF) shl 24) or Integer((Value and $0000FF00) shl 8)
      or Integer((Value and $00FF0000) shr 8) or Integer((Value and $FF000000) shr 24)
  else
    Result := Value;
end;

function Int16HostToNetwork(Value: SmallInt): SmallInt;
begin
  if not FByteOrderIsBigEndian then
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8)
  else
    Result := Value;
end;

function Int64NetworkToHost(Value: Int64): Int64;
begin
  if not FByteOrderIsBigEndian then
    REsult := SwapInt64(Value)
  else
    Result := Value;
end;

function Int32NetworkToHost(Value: Integer): Integer;
begin
  if not FByteOrderIsBigEndian then
    Result := Integer((Value and $000000FF) shl 24) or Integer((Value and $0000FF00) shl 8)
      or Integer((Value and $00FF0000) shr 8) or Integer((Value and $FF000000) shr 24)
  else
    Result := Value;
end;

function Int16NetworkToHost(Value: SmallInt): SmallInt;
begin
  if not FByteOrderIsBigEndian then
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8)
  else
    Result := Value;
end;

function UInt64HostToNetwork(Value: TUInt64): TUInt64;
begin
  if CurrentByteOrderIsBigEndian then
    Result := Value
  else
    Result := SwapUInt64(Value);
end;

function UInt32HostToNetwork(Value: Cardinal): Cardinal;
begin
  if not FByteOrderIsBigEndian then
    Result := Cardinal((Value and $000000FF) shl 24) or Cardinal((Value and $0000FF00) shl 8)
      or Cardinal((Value and $00FF0000) shr 8) or Cardinal((Value and $FF000000) shr 24)
  else
    Result := Value;
end;

function UInt16HostToNetwork(Value: Word): Word;
begin
  if not FByteOrderIsBigEndian then
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8)
  else
    Result := Value;
end;

function UInt64NetworkToHost(Value: TUInt64): TUInt64;
begin
  if CurrentByteOrderIsBigEndian then
    Result := Value
  else
    Result := SwapUInt64(Value);
end;

function UInt32NetworkToHost(Value: Cardinal): Cardinal;
begin
  if not FByteOrderIsBigEndian then
    Result := Cardinal((Value and $000000FF) shl 24) or Cardinal((Value and $0000FF00) shl 8)
      or Cardinal((Value and $00FF0000) shr 8) or Cardinal((Value and $FF000000) shr 24)
  else
    Result := Value;
end;

function UInt16NetworkToHost(Value: Word): Word;
begin
  if not FByteOrderIsBigEndian then
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8)
  else
    Result := Value;
end;

function ReverseBitsInInt8(V: Byte): Byte;
begin
  // 0 和 1 交换、2 和 3 交换、4 和 5 交换、6 和 7 交换
  V := ((V and $AA) shr 1) or ((V and $55) shl 1);
  // 01 和 23 交换、45 和 67 交换
  V := ((V and $CC) shr 2) or ((V and $33) shl 2);
  // 0123 和 4567 交换
  V := (V shr 4) or (V shl 4);
  Result := V;
end;

function ReverseBitsInInt16(V: Word): Word;
begin
  Result := (ReverseBitsInInt8(V and $00FF) shl 8)
    or ReverseBitsInInt8((V and $FF00) shr 8);
end;

function ReverseBitsInInt32(V: Cardinal): Cardinal;
begin
  Result := (ReverseBitsInInt16(V and $0000FFFF) shl 16)
    or ReverseBitsInInt16((V and $FFFF0000) shr 16);
end;

function ReverseBitsInInt64(V: Int64): Int64;
begin
  Result := (Int64(ReverseBitsInInt32(V and $00000000FFFFFFFF)) shl 32)
    or ReverseBitsInInt32((V and $FFFFFFFF00000000) shr 32);
end;

procedure ReverseMemory(AMem: Pointer; MemByteLen: Integer);
var
  I, L: Integer;
  P: PByteArray;
  T: Byte;
begin
  if (AMem = nil) or (MemByteLen < 2) then
    Exit;

  L := MemByteLen div 2;
  P := PByteArray(AMem);
  for I := 0 to L - 1 do
  begin
    // 交换第 I 和第 MemLen - I - 1
    T := P^[I];
    P^[I] := P^[MemByteLen - I - 1];
    P^[MemByteLen - I - 1] := T;
  end;
end;

procedure ReverseMemoryWithBits(AMem: Pointer; MemByteLen: Integer);
var
  I: Integer;
  P: PByteArray;
begin
  if (AMem = nil) or (MemByteLen <= 0) then
    Exit;

  ReverseMemory(AMem, MemByteLen);
  P := PByteArray(AMem);

  for I := 0 to MemByteLen - 1 do
    P^[I] := ReverseBitsInInt8(P^[I]);
end;

// N 字节长度的内存块的位操作
procedure MemoryBitOperation(AMem, BMem, RMem: Pointer; N: Integer; Op: TCnBitOperation);
var
  A, B, R: PCnLongWord32Array;
  BA, BB, BR: PByteArray;
begin
  if N <= 0 then
    Exit;

  if (AMem = nil) or ((BMem = nil) and (Op <> boNot)) or (RMem = nil) then
    Exit;

  A := PCnLongWord32Array(AMem);
  B := PCnLongWord32Array(BMem);
  R := PCnLongWord32Array(RMem);

  while (N and (not 3)) <> 0 do
  begin
    case Op of
      boAnd:
        R^[0] := A^[0] and B^[0];
      boOr:
        R^[0] := A^[0] or B^[0];
      boXor:
        R^[0] := A^[0] xor B^[0];
      boNot: // 求反时忽略 B
        R^[0] := not A^[0];
    end;

    A := PCnLongWord32Array(TCnNativeInt(A) + SizeOf(Cardinal));
    B := PCnLongWord32Array(TCnNativeInt(B) + SizeOf(Cardinal));
    R := PCnLongWord32Array(TCnNativeInt(R) + SizeOf(Cardinal));

    Dec(N, 4);
  end;

  if N > 0 then
  begin
    BA := PByteArray(A);
    BB := PByteArray(B);
    BR := PByteArray(R);

    while N <> 0 do
    begin
      case Op of
        boAnd:
          BR^[0] := BA^[0] and BB^[0];
        boOr:
          BR^[0] := BA^[0] or BB^[0];
        boXor:
          BR^[0] := BA^[0] xor BB^[0];
        boNot:
          BR^[0] := not BA^[0];
      end;

      BA := PByteArray(TCnNativeInt(BA) + SizeOf(Byte));
      BB := PByteArray(TCnNativeInt(BB) + SizeOf(Byte));
      BR := PByteArray(TCnNativeInt(BR) + SizeOf(Byte));
      Dec(N);
    end;
  end;
end;

procedure MemoryAnd(AMem, BMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
begin
  MemoryBitOperation(AMem, BMem, ResMem, MemByteLen, boAnd);
end;

procedure MemoryOr(AMem, BMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
begin
  MemoryBitOperation(AMem, BMem, ResMem, MemByteLen, boOr);
end;

procedure MemoryXor(AMem, BMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
begin
  MemoryBitOperation(AMem, BMem, ResMem, MemByteLen, boXor);
end;

procedure MemoryNot(AMem: Pointer; MemByteLen: Integer; ResMem: Pointer);
begin
  MemoryBitOperation(AMem, nil, ResMem, MemByteLen, boNot);
end;

procedure MemoryShiftLeft(AMem, BMem: Pointer; MemByteLen: Integer; BitCount: Integer);
var
  I, L, N, LB, RB: Integer;
  PF, PT: PByteArray;
begin
  if (AMem = nil) or (MemByteLen <= 0) or (BitCount = 0) then
    Exit;

  if BitCount < 0 then
  begin
    MemoryShiftRight(AMem, BMem, MemByteLen, -BitCount);
    Exit;
  end;

  if BMem = nil then
    BMem := AMem;

  if (MemByteLen * 8) <= BitCount then // 移太多不够，全 0
  begin
    FillChar(BMem^, MemByteLen, 0);
    Exit;
  end;

  N := BitCount div 8;  // 移位超过的整字节数
  RB := BitCount mod 8; // 去除整字节后剩下的位数
  LB := 8 - RB;         // 上面剩下的位数在一字节内再剩下的位数

  PF := PByteArray(AMem);
  PT := PByteArray(BMem);

  if RB = 0 then // 整块，好办，要移位的字节数是 MemLen - NW
  begin
    Move(PF^[N], PT^[0], MemByteLen - N);
    FillChar(PT^[MemByteLen - N], N, 0);
  end
  else
  begin
    // 起点是 PF^[N] 和 PT^[0]，长度 MemLen - N 个字节，但相邻字节间有交叉
    L := MemByteLen - N;
    PF := PByteArray(TCnNativeInt(PF) + N);

    for I := 1 to L do // 从低位往低移动，先处理低的
    begin
      PT^[0] := Byte(PF^[0] shl RB);
      if I < L then    // 最高一个字节 PF^[1] 会超界
        PT^[0] := (PF^[1] shr LB) or PT^[0];

      PF := PByteArray(TCnNativeInt(PF) + 1);
      PT := PByteArray(TCnNativeInt(PT) + 1);
    end;

    // 剩下的要填 0
    if N > 0 then
      FillChar(PT^[0], N, 0);
  end;
end;

procedure MemoryShiftRight(AMem, BMem: Pointer; MemByteLen: Integer; BitCount: Integer);
var
  I, L, N, LB, RB: Integer;
  PF, PT: PByteArray;
begin
  if (AMem = nil) or (MemByteLen <= 0) or (BitCount = 0) then
    Exit;

  if BitCount < 0 then
  begin
    MemoryShiftLeft(AMem, BMem, MemByteLen, -BitCount);
    Exit;
  end;

  if BMem = nil then
    BMem := AMem;

  if (MemByteLen * 8) <= BitCount then // 移太多不够，全 0
  begin
    FillChar(BMem^, MemByteLen, 0);
    Exit;
  end;

  N := BitCount div 8;  // 移位超过的整字节数
  RB := BitCount mod 8; // 去除整字节后剩下的位数
  LB := 8 - RB;         // 上面剩下的位数在一字节内再剩下的位数

  if RB = 0 then // 整块，好办，要移位的字节数是 MemLen - N
  begin
    PF := PByteArray(AMem);
    PT := PByteArray(BMem);

    Move(PF^[0], PT^[N], MemByteLen - N);
    FillChar(PT^[0], N, 0);
  end
  else
  begin
    // 起点是 PF^[0] 和 PT^[N]，长度 MemLen - N 个字节，但得从高处开始，且相邻字节间有交叉
    L := MemByteLen - N;

    PF := PByteArray(TCnNativeInt(AMem) + L - 1);
    PT := PByteArray(TCnNativeInt(BMem) + MemByteLen - 1);

    for I := L downto 1 do // 从高位往高位移动，先处理后面的
    begin
      PT^[0] := Byte(PF^[0] shr RB);
      if I > 1 then        // 最低一个字节 PF^[-1] 会超界
      begin
        PF := PByteArray(TCnNativeInt(PF) - 1);
        PT^[0] := (PF^[0] shl LB) or PT^[0];
      end
      else
        PF := PByteArray(TCnNativeInt(PF) - 1);

      PT := PByteArray(TCnNativeInt(PT) - 1);
    end;

    // 剩下的最前面的要填 0
    if N > 0 then
      FillChar(BMem^, N, 0);
  end;
end;

function MemoryIsBitSet(AMem: Pointer; N: Integer): Boolean;
var
  P: PByte;
  A, B: Integer;
  V: Byte;
begin
  if (AMem = nil) or (N < 0) then
    raise Exception.Create(SRangeError);

  A := N div 8;
  B := N mod 8;
  P := PByte(TCnNativeInt(AMem) + A);

  V := Byte(1 shl B);
  Result := (P^ and V) <> 0;
end;

procedure MemorySetBit(AMem: Pointer; N: Integer);
var
  P: PByte;
  A, B: Integer;
  V: Byte;
begin
  if (AMem = nil) or (N < 0) then
    raise Exception.Create(SRangeError);

  A := N div 8;
  B := N mod 8;
  P := PByte(TCnNativeInt(AMem) + A);

  V := Byte(1 shl B);
  P^ := P^ or V;
end;

procedure MemoryClearBit(AMem: Pointer; N: Integer);
var
  P: PByte;
  A, B: Integer;
  V: Byte;
begin
  if (AMem = nil) or (N < 0) then
    raise Exception.Create(SRangeError);

  A := N div 8;
  B := N mod 8;
  P := PByte(TCnNativeInt(AMem) + A);

  V := not Byte(1 shl B);
  P^ := P^ and V;
end;

function MemoryToBinStr(AMem: Pointer; MemByteLen: Integer; Sep: Boolean): string;
var
  J, L: Integer;
  P: PByteArray;
  B: PChar;

  procedure FillAByteToBuf(V: Byte; Buf: PChar);
  const
    M = $80;
  var
    I: Integer;
  begin
    for I := 0 to 7 do
    begin
      if (V and M) <> 0 then
        Buf[I] := '1'
      else
        Buf[I] := '0';
      V := V shl 1;
    end;
  end;

begin
  Result := '';
  if (AMem = nil) or (MemByteLen <= 0) then
    Exit;

  L := MemByteLen * 8;
  if Sep then
    L := L + MemByteLen - 1; // 中间用空格分隔

  SetLength(Result, L);
  B := PChar(@Result[1]);
  P := PByteArray(AMem);

  for J := 0 to MemByteLen - 1 do
  begin
    FillAByteToBuf(P^[J], B);
    if Sep then
    begin
      B[8] := ' ';
      Inc(B, 9);
    end
    else
      Inc(B, 8);
  end;
end;

procedure MemorySwap(AMem, BMem: Pointer; MemByteLen: Integer);
var
  A, B: PCnLongWord32Array;
  BA, BB: PByteArray;
  TC: Cardinal;
  TB: Byte;
begin
  if (AMem = nil) or (BMem = nil) or (MemByteLen <= 0) then
    Exit;

  A := PCnLongWord32Array(AMem);
  B := PCnLongWord32Array(BMem);

  while (MemByteLen and (not 3)) <> 0 do
  begin
    TC := A[0];
    A[0] := B[0];
    B[0] := TC;

    A := PCnLongWord32Array(TCnNativeInt(A) + SizeOf(Cardinal));
    B := PCnLongWord32Array(TCnNativeInt(B) + SizeOf(Cardinal));

    Dec(MemByteLen, 4);
  end;

  if MemByteLen > 0 then
  begin
    BA := PByteArray(A);
    BB := PByteArray(B);

    while MemByteLen <> 0 do
    begin
      TB := BA[0];
      BA[0] := BB[0];
      BB[0] := TB;

      BA := PByteArray(TCnNativeInt(BA) + SizeOf(Byte));
      BB := PByteArray(TCnNativeInt(BB) + SizeOf(Byte));

      Dec(MemByteLen);
    end;
  end;
end;

function MemoryCompare(AMem, BMem: Pointer; MemByteLen: Integer): Integer;
var
  A, B: PCnLongWord32Array;
  BA, BB: PByteArray;
begin
  Result := 0;
  if (AMem = nil) and (BMem = nil) then
    Exit;
  if MemByteLen <= 0 then
    Exit;

  if AMem = nil then
  begin
    Result := -1;
    Exit;
  end;
  if BMem = nil then
  begin
    Result := 1;
    Exit;
  end;

  A := PCnLongWord32Array(AMem);
  B := PCnLongWord32Array(BMem);

  while (MemByteLen and (not 3)) <> 0 do
  begin
    if A[0] > B[0] then
    begin
      Result := 1;
      Exit;
    end
    else if A[0] < B[0] then
    begin
      Result := -1;
      Exit;
    end;

    A := PCnLongWord32Array(TCnNativeInt(A) + SizeOf(Cardinal));
    B := PCnLongWord32Array(TCnNativeInt(B) + SizeOf(Cardinal));

    Dec(MemByteLen, 4);
  end;

  if MemByteLen > 0 then
  begin
    BA := PByteArray(A);
    BB := PByteArray(B);

    while MemByteLen <> 0 do
    begin
      if BA[0] > BB[0] then
      begin
        Result := 1;
        Exit;
      end
      else if BA[0] < BB[0] then
      begin
        Result := -1;
        Exit;
      end;

      BA := PByteArray(TCnNativeInt(BA) + SizeOf(Byte));
      BB := PByteArray(TCnNativeInt(BB) + SizeOf(Byte));

      Dec(MemByteLen);
    end;
  end;
end;

function UInt8ToBinStr(V: Byte): string;
const
  M = $80;
var
  I: Integer;
begin
  SetLength(Result, 8 * SizeOf(V));
  for I := 1 to 8 * SizeOf(V) do
  begin
    if (V and M) <> 0 then
      Result[I] := '1'
    else
      Result[I] := '0';
    V := V shl 1;
  end;
end;

function UInt16ToBinStr(V: Word): string;
const
  M = $8000;
var
  I: Integer;
begin
  SetLength(Result, 8 * SizeOf(V));
  for I := 1 to 8 * SizeOf(V) do
  begin
    if (V and M) <> 0 then
      Result[I] := '1'
    else
      Result[I] := '0';
    V := V shl 1;
  end;
end;

function UInt32ToBinStr(V: Cardinal): string;
const
  M = $80000000;
var
  I: Integer;
begin
  SetLength(Result, 8 * SizeOf(V));
  for I := 1 to 8 * SizeOf(V) do
  begin
    if (V and M) <> 0 then
      Result[I] := '1'
    else
      Result[I] := '0';
    V := V shl 1;
  end;
end;

function UInt64ToBinStr(V: TUInt64): string;
const
  M = $8000000000000000;
var
  I: Integer;
begin
  SetLength(Result, 8 * SizeOf(V));

  for I := 1 to 8 * SizeOf(V) do
  begin
    if (V and M) <> 0 then
      Result[I] := '1'
    else
      Result[I] := '0';
    V := V shl 1;
  end;
end;

const
  HiDigits: array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
const
  LoDigits: array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');

function HexToInt(const Hex: string): Integer;
var
  I, Res: Integer;
  C: Char;
begin
  Res := 0;
  for I := 0 to Length(Hex) - 1 do
  begin
    C := Hex[I + 1];
    if (C >= '0') and (C <= '9') then
      Res := Res * 16 + Ord(C) - Ord('0')
    else if (C >= 'A') and (C <= 'F') then
      Res := Res * 16 + Ord(C) - Ord('A') + 10
    else if (C >= 'a') and (C <= 'f') then
      Res := Res * 16 + Ord(C) - Ord('a') + 10
    else
      raise Exception.Create('Error: not a Hex String');
  end;
  Result := Res;
end;

function DataToHex(InData: Pointer; ByteLength: Integer; UseUpperCase: Boolean = True): string;
var
  I: Integer;
  B: Byte;
begin
  Result := '';
  if ByteLength <= 0 then
    Exit;

  SetLength(Result, ByteLength * 2);
  if UseUpperCase then
  begin
    for I := 0 to ByteLength - 1 do
    begin
      B := PByte(TCnNativeInt(InData) + I * SizeOf(Byte))^;
      Result[I * 2 + 1] := HiDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := HiDigits[B and $0F];
    end;
  end
  else
  begin
    for I := 0 to ByteLength - 1 do
    begin
      B := PByte(TCnNativeInt(InData) + I * SizeOf(Byte))^;
      Result[I * 2 + 1] := LoDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := LoDigits[B and $0F];
    end;
  end;
end;

function HexToData(const Hex: string; OutData: Pointer): string;
var
  I, L: Integer;
  S: string;
begin
  L := Length(Hex);
  if (L mod 2) <> 0 then
    raise Exception.Create('Error: not a Hex String');

  for I := 1 to L div 2 do
  begin
    S := Copy(Hex, I * 2 - 1, 2);
    PByte(TCnNativeInt(OutData) + I - 1)^ := Byte(HexToInt(S));
  end;
end;

function StringToHex(const Data: string; UseUpperCase: Boolean): string;
var
  I, L: Integer;
  B: Byte;
  Buffer: PChar;
begin
  Result := '';
  L := Length(Data);
  if L = 0 then
    Exit;

  SetLength(Result, L * 2);
  Buffer := @Data[1];

  if UseUpperCase then
  begin
    for I := 0 to L - 1 do
    begin
      B := PByte(TCnNativeInt(Buffer) + I * SizeOf(Char))^;
      Result[I * 2 + 1] := HiDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := HiDigits[B and $0F];
    end;
  end
  else
  begin
    for I := 0 to L - 1 do
    begin
      B := PByte(TCnNativeInt(Buffer) + I * SizeOf(Char))^;
      Result[I * 2 + 1] := LoDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := LoDigits[B and $0F];
    end;
  end;
end;

function HexToString(const Hex: string): string;
var
  I, L: Integer;
  S: string;
begin
  L := Length(Hex);
  if (L mod 2) <> 0 then
    raise Exception.Create('Error: not a Hex String');

  SetLength(Result, L div 2);
  for I := 1 to L div 2 do
  begin
    S := Copy(Hex, I * 2 - 1, 2);
    Result[I] := Chr(HexToInt(S));
  end;
end;

function BytesToHex(Data: TBytes; UseUpperCase: Boolean): string;
var
  I, L: Integer;
  B: Byte;
  Buffer: PAnsiChar;
begin
  Result := '';
  L := Length(Data);
  if L = 0 then
    Exit;

  SetLength(Result, L * 2);
  Buffer := @Data[0];

  if UseUpperCase then
  begin
    for I := 0 to L - 1 do
    begin
      B := PByte(TCnNativeInt(Buffer) + I)^;
      Result[I * 2 + 1] := HiDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := HiDigits[B and $0F];
    end;
  end
  else
  begin
    for I := 0 to L - 1 do
    begin
      B := PByte(TCnNativeInt(Buffer) + I)^;
      Result[I * 2 + 1] := LoDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := LoDigits[B and $0F];
    end;
  end;
end;

function HexToBytes(const Hex: string): TBytes;
var
  I, L: Integer;
  S: string;
begin
  L := Length(Hex);
  if (L mod 2) <> 0 then
    raise Exception.Create('Error: not a Hex String');

  SetLength(Result, L div 2);
  for I := 1 to L div 2 do
  begin
    S := Copy(Hex, I * 2 - 1, 2);
    Result[I - 1] := Byte(HexToInt(S));
  end;
end;

procedure ReverseBytes(Data: TBytes);
var
  I, L, M: Integer;
  T: Byte;
begin
  if (Data = nil) or (Length(Data) <= 1) then
    Exit;
  L := Length(Data);
  M := L div 2;
  for I := 0 to M - 1 do
  begin
    // 交换 I 和 L - I - 1
    T := Data[I];
    Data[I] := Data[L - I - 1];
    Data[L - I - 1] := T;
  end;
end;

procedure MoveMost(const Source; var Dest; ByteLen, MostLen: Integer);
begin
  if MostLen <= 0 then
    Exit;

  if ByteLen > MostLen then
    ByteLen := MostLen
  else if ByteLen < MostLen then
    FillChar(Dest, MostLen, 0);

  Move(Source, Dest, ByteLen);
end;

procedure ConstantTimeConditionalSwap8(CanSwap: Boolean; var A, B: Byte);
var
  T, V: Byte;
begin
  if CanSwap then
    T := $FF
  else
    T := 0;

  V := (A xor B) and T;
  A := A xor V;
  B := B xor V;
end;

procedure ConstantTimeConditionalSwap16(CanSwap: Boolean; var A, B: Word);
var
  T, V: Word;
begin
  if CanSwap then
    T := $FFFF
  else
    T := 0;

  V := (A xor B) and T;
  A := A xor V;
  B := B xor V;
end;

procedure ConstantTimeConditionalSwap32(CanSwap: Boolean; var A, B: Cardinal);
var
  T, V: Cardinal;
begin
  if CanSwap then
    T := $FFFFFFFF
  else
    T := 0;

  V := (A xor B) and T;
  A := A xor V;
  B := B xor V;
end;

procedure ConstantTimeConditionalSwap64(CanSwap: Boolean; var A, B: TUInt64);
var
  T, V: TUInt64;
begin
  if CanSwap then
{$IFDEF SUPPORT_UINT64}
    T := $FFFFFFFFFFFFFFFF
{$ELSE}
    T := not 0
{$ENDIF}
  else
    T := 0;

  V := (A xor B) and T;
  A := A xor V;
  B := B xor V;
end;

{$IFDEF MSWINDOWS}

{$IFDEF CPUX64}

// 64 位汇编用 IDIV 和 IDIV 指令实现，其中 A 在 RCX 里，B 在 EDX/RDX 里，DivRes 地址在 R8 里，ModRes 地址在 R9 里
procedure Int64DivInt32Mod(A: Int64; B: Integer; var DivRes, ModRes: Integer); assembler;
asm
        PUSH    RCX                           // RCX 是 A
        MOV     RCX, RDX                      // 除数 B 放入 RCX
        POP     RAX                           // 被除数 A 放入 RAX
        XOR     RDX, RDX                      // 被除数高 64 位清零
        IDIV    RCX
        MOV     [R8], EAX                     // 商放入 R8 所指的 DivRes
        MOV     [R9], EDX                     // 余数放入 R9 所指的 ModRes
end;

procedure UInt64DivUInt32Mod(A: TUInt64; B: Cardinal; var DivRes, ModRes: Cardinal); assembler;
asm
        PUSH    RCX                           // RCX 是 A
        MOV     RCX, RDX                      // 除数 B 放入 RCX
        POP     RAX                           // 被除数 A 放入 RAX
        XOR     RDX, RDX                      // 被除数高 64 位清零
        DIV     RCX
        MOV     [R8], EAX                     // 商放入 R8 所指的 DivRes
        MOV     [R9], EDX                     // 余数放入 R9 所指的 ModRes
end;

// 64 位汇编用 IDIV 和 IDIV 指令实现，ALo 在 RCX，AHi 在 RDX，B 在 R8，DivRes 的地址在 R9，
procedure Int128DivInt64Mod(ALo, AHi: Int64; B: Int64; var DivRes, ModRes: Int64); assembler;
asm
        MOV     RAX, RCX                      // ALo 放入 RAX，AHi 已经在 RDX 了
        MOV     RCX, R8                       // B 放入 RCX
        IDIV    RCX
        MOV     [R9], RAX                     // 商放入 R9 所指的 DivRes
        MOV     RAX, [RBP + $30]              // ModRes 地址放入 RAX
        MOV     [RAX], RDX                    // 余数放入 RAX 所指的 ModRes
end;

procedure UInt128DivUInt64Mod(ALo, AHi: UInt64; B: UInt64; var DivRes, ModRes: UInt64); assembler
asm
        MOV     RAX, RCX                      // ALo 放入 RAX，AHi 已经在 RDX 了
        MOV     RCX, R8                       // B 放入 RCX
        DIV     RCX
        MOV     [R9], RAX                     // 商放入 R9 所指的 DivRes
        MOV     RAX, [RBP + $30]              // ModRes 地址放入 RAX
        MOV     [RAX], RDX                    // 余数放入 RAX 所指的 ModRes
end;

{$ELSE}

// 32 位汇编用 IDIV 和 IDIV 指令实现，其中 A 在堆栈上，B 在 EAX，DivRes 地址在 EDX，ModRes 地址在 ECX
procedure Int64DivInt32Mod(A: Int64; B: Integer; var DivRes, ModRes: Integer); assembler;
asm
        PUSH    ECX                           // ECX 是 ModRes 地址，先保存
        MOV     ECX, B                        // B 在 EAX 中，搬移到 ECX 中
        PUSH    EDX                           // DivRes 的地址在 EDX 中，也保存
        MOV     EAX, [EBP + $8]               // A Lo
        MOV     EDX, [EBP + $C]               // A Hi
        IDIV    ECX
        POP     ECX                           // 弹出 ECX，拿到 DivRes 地址
        MOV     [ECX], EAX
        POP     ECX                           // 弹出 ECX，拿到 ModRes 地址
        MOV     [ECX], EDX
end;

procedure UInt64DivUInt32Mod(A: TUInt64; B: Cardinal; var DivRes, ModRes: Cardinal); assembler;
asm
        PUSH    ECX                           // ECX 是 ModRes 地址，先保存
        MOV     ECX, B                        // B 在 EAX 中，搬移到 ECX 中
        PUSH    EDX                           // DivRes 的地址在 EDX 中，也保存
        MOV     EAX, [EBP + $8]               // A Lo
        MOV     EDX, [EBP + $C]               // A Hi
        DIV     ECX
        POP     ECX                           // 弹出 ECX，拿到 DivRes 地址
        MOV     [ECX], EAX
        POP     ECX                           // 弹出 ECX，拿到 ModRes 地址
        MOV     [ECX], EDX
end;

// 32 位下的实现
procedure Int128DivInt64Mod(ALo, AHi: Int64; B: Int64; var DivRes, ModRes: Int64);
var
  C: Integer;
begin
  if B = 0 then
    raise EDivByZero.Create(SDivByZero);

  if (AHi = 0) or (AHi = $FFFFFFFFFFFFFFFF) then // 高 64 位为 0 的正值或负值
  begin
    DivRes := ALo div B;
    ModRes := ALo mod B;
  end
  else
  begin
    if B < 0 then // 除数是负数
    begin
      Int128DivInt64Mod(ALo, AHi, -B, DivRes, ModRes);
      DivRes := -DivRes;
      Exit;
    end;

    if AHi < 0 then // 被除数是负数
    begin
      // AHi, ALo 求反加 1，以得到正值
      AHi := not AHi;
      ALo := not ALo;
{$IFDEF SUPPORT_UINT64}
      UInt64Add(UInt64(ALo), UInt64(ALo), 1, C);
{$ELSE}
      UInt64Add(ALo, ALo, 1, C);
{$ENDIF}
      if C > 0 then
        AHi := AHi + C;

      // 被除数转正了
      Int128DivInt64Mod(ALo, AHi, B, DivRes, ModRes);

      // 结果再调整
      if ModRes = 0 then
        DivRes := -DivRes
      else
      begin
        DivRes := -DivRes - 1;
        ModRes := B - ModRes;
      end;
      Exit;
    end;

    // 全正后，按无符号来除
{$IFDEF SUPPORT_UINT64}
    UInt128DivUInt64Mod(TUInt64(ALo), TUInt64(AHi), TUInt64(B), TUInt64(DivRes), TUInt64(ModRes));
{$ELSE}
    UInt128DivUInt64Mod(ALo, AHi, B, DivRes, ModRes);
{$ENDIF}
  end;
end;

procedure UInt128DivUInt64Mod(ALo, AHi: TUInt64; B: TUInt64; var DivRes, ModRes: TUInt64);
var
  I, Cnt: Integer;
  Q, R: TUInt64;
begin
  if B = 0 then
    raise EDivByZero.Create(SDivByZero);

  if AHi = 0 then
  begin
    DivRes := UInt64Div(ALo, B);
    ModRes := UInt64Mod(ALo, B);
  end
  else
  begin
    // 有高位有低位咋办？先判断是否会溢出，如果 AHi >= B，则表示商要超 64 位，溢出
    if UInt64Compare(AHi, B) >= 0 then
      raise Exception.Create(SIntOverflow);

    Q := 0;
    R := 0;
    Cnt := GetUInt64LowBits(AHi) + 64;
    for I := Cnt downto 0 do
    begin
      R := R shl 1;
      if IsUInt128BitSet(ALo, AHi, I) then  // 被除数的第 I 位是否是 0
        R := R or 1
      else
        R := R and TUInt64(not 1);

      if UInt64Compare(R, B) >= 0 then
      begin
        R := R - B;
        Q := Q or (TUInt64(1) shl I);
      end;
    end;
    DivRes := Q;
    ModRes := R;
  end;
end;

{$ENDIF}

{$ENDIF}

{$IFDEF CPUX64}

function UInt64Mod(A, B: TUInt64): TUInt64;
begin
  Result := A mod B;
end;

function UInt64Div(A, B: TUInt64): TUInt64;
begin
  Result := A div B;
end;

{$ELSE}

{$IFDEF FPC}

function UInt64Mod(A, B: TUInt64): TUInt64;
begin
  Result := A mod B;
end;

function UInt64Div(A, B: TUInt64): TUInt64;
begin
  Result := A div B;
end;

{$ELSE}
{
  UInt64 求 A mod B

  调用的入栈顺序是 A 的高位，A 的低位，B 的高位，B 的低位。挨个 push 完毕并进入函数后，
  ESP 是返回地址，ESP+4 是 B 的低位，ESP + 8 是 B 的高位，ESP + C 是 A 的低位，ESP + 10 是 A 的高位
  进入后 push esp 让 ESP 减了 4，然后 mov ebp esp，之后用 EBP 来寻址，全要多加 4

  而 System.@_llumod 要求在刚进入时，EAX <- A 的低位，EDX <- A 的高位，（System 源码注释中 EAX/EDX 写反了）
  [ESP + 8]（也就是 EBP + C）<- B 的高位，[ESP + 4] （也就是 EBP + 8）<- B 的低位

  所以 CALL 前加了四句搬移代码。UInt64 Div 的也类似
}
function UInt64Mod(A, B: TUInt64): TUInt64;
asm
        // PUSH ESP 让 ESP 减了 4，要补上
        MOV     EAX, [EBP + $10]              // A Lo
        MOV     EDX, [EBP + $14]              // A Hi
        PUSH    DWORD PTR[EBP + $C]           // B Hi
        PUSH    DWORD PTR[EBP + $8]           // B Lo
        CALL    System.@_llumod;
end;

function UInt64Div(A, B: TUInt64): TUInt64;
asm
        // PUSH ESP 让 ESP 减了 4，要补上
        MOV     EAX, [EBP + $10]              // A Lo
        MOV     EDX, [EBP + $14]              // A Hi
        PUSH    DWORD PTR[EBP + $C]           // B Hi
        PUSH    DWORD PTR[EBP + $8]           // B Lo
        CALL    System.@_lludiv;
end;

{$ENDIF}

{$ENDIF}

{$IFDEF SUPPORT_UINT64}

function UInt64Mul(A, B: Cardinal): TUInt64;
begin
  Result := TUInt64(A) * B;
end;

{$ELSE}

{
  无符号 32 位整数相乘，如果结果直接使用 Int64 会溢出，模拟 64 位无符号运算

  调用寄存器约定是 A -> EAX，B -> EDX，不使用堆栈
  而 System.@_llmul 要求在刚进入时，EAX <- A 的低位，EDX <- A 的高位 0，
  [ESP + 8]（也就是 EBP + C）<- B 的高位 0，[ESP + 4] （也就是 EBP + 8）<- B 的低位
}
function UInt64Mul(A, B: Cardinal): TUInt64;
asm
        PUSH    0               // PUSH B 高位 0
        PUSH    EDX             // PUSH B 低位
                                // EAX A 低位，已经是了
        XOR     EDX, EDX        // EDX A 高位 0
        CALL    System.@_llmul; // 返回 EAX 低 32 位、EDX 高 32 位
end;

{$ENDIF}

// 两个无符号 64 位整数相加，处理溢出的情况，结果放 ResLo 与 ResHi 中
procedure UInt64AddUInt64(A, B: TUInt64; var ResLo, ResHi: TUInt64);
var
  X, Y, Z, T, R0L, R0H, R1L, R1H: Cardinal;
  R0, R1, R01, R12: TUInt64;
begin
  // 基本思想：2^32 是系数 M，拆成 (xM+y) + (zM+t) = (x+z) M + (y+t)
  // y+t 是 R0 占 0、1，x+z 是 R1 占 1、2，把 R0, R1 再拆开相加成 R01, R12
  if IsUInt64AddOverflow(A, B) then
  begin
    X := Int64Rec(A).Hi;
    Y := Int64Rec(A).Lo;
    Z := Int64Rec(B).Hi;
    T := Int64Rec(B).Lo;

    R0 := TUInt64(Y) + TUInt64(T);
    R1 := TUInt64(X) + TUInt64(Z);

    R0L := Int64Rec(R0).Lo;
    R0H := Int64Rec(R0).Hi;
    R1L := Int64Rec(R1).Lo;
    R1H := Int64Rec(R1).Hi;

    R01 := TUInt64(R0H) + TUInt64(R1L);
    R12 := TUInt64(R1H) + TUInt64(Int64Rec(R01).Hi);

    Int64Rec(ResLo).Lo := R0L;
    Int64Rec(ResLo).Hi := Int64Rec(R01).Lo;
    Int64Rec(ResHi).Lo := Int64Rec(R12).Lo;
    Int64Rec(ResHi).Hi := Int64Rec(R12).Hi;
  end
  else
  begin
    ResLo := A + B;
    ResHi := 0;
  end;
end;

{$IFDEF WIN64}

// 64 位下两个无符号 64 位整数相乘，结果放 ResLo 与 ResHi 中，直接用汇编实现，比下面快了一倍以上
procedure UInt64MulUInt64(A, B: UInt64; var ResLo, ResHi: UInt64); assembler;
asm
  PUSH RAX
  MOV RAX, RCX
  IMUL RDX
  MOV [R8], RAX
  MOV [R9], RDX
  POP RAX
end;

{$ELSE}

// 两个无符号 64 位整数相乘，结果放 ResLo 与 ResHi 中
procedure UInt64MulUInt64(A, B: TUInt64; var ResLo, ResHi: TUInt64);
var
  X, Y, Z, T: Cardinal;
  YT, XT, ZY, ZX: TUInt64;
  P, R1Lo, R1Hi, R2Lo, R2Hi: TUInt64;
begin
  // 基本思想：2^32 是系数 M，拆成 (xM+y)*(zM+t) = xzM^2 + (xt+yz)M + yt
  // 各项系数都是 UInt64，xz 占 2、3、4，xt+yz 占 1、2、3，yt 占 0、1，然后累加
  X := Int64Rec(A).Hi;
  Y := Int64Rec(A).Lo;
  Z := Int64Rec(B).Hi;
  T := Int64Rec(B).Lo;

  YT := UInt64Mul(Y, T);
  XT := UInt64Mul(X, T);
  ZY := UInt64Mul(Y, Z);
  ZX := UInt64Mul(X, Z);

  Int64Rec(ResLo).Lo := Int64Rec(YT).Lo;

  P := Int64Rec(YT).Hi;
  UInt64AddUInt64(P, XT, R1Lo, R1Hi);
  UInt64AddUInt64(ZY, R1Lo, R2Lo, R2Hi);

  Int64Rec(ResLo).Hi := Int64Rec(R2Lo).Lo;

  P := TUInt64(Int64Rec(R2Lo).Hi) + TUInt64(Int64Rec(ZX).Lo);

  Int64Rec(ResHi).Lo := Int64Rec(P).Lo;
  Int64Rec(ResHi).Hi := Int64Rec(R1Hi).Lo + Int64Rec(R2Hi).Lo + Int64Rec(ZX).Hi + Int64Rec(P).Hi;
end;

{$ENDIF}

function _ValUInt64(const S: string; var Code: Integer): TUInt64;
const
  FirstIndex = 1;
var
  I: Integer;
  Dig: Integer;
  Sign: Boolean;
  Empty: Boolean;
begin
  I := FirstIndex;
  Dig := 0;
  Result := 0;

  if S = '' then
  begin
    Code := 1;
    Exit;
  end;
  while S[I] = Char(' ') do
    Inc(I);
  Sign := False;
  if S[I] =  Char('-') then
  begin
    Sign := True;
    Inc(I);
  end
  else if S[I] =  Char('+') then
    Inc(I);
  Empty := True;

  if (S[I] =  Char('$')) or (UpCase(S[I]) =  Char('X'))
    or ((S[I] =  Char('0')) and (I < Length(S)) and (UpCase(S[I+1]) =  Char('X'))) then
  begin
    if S[I] =  Char('0') then
      Inc(I);
    Inc(I);
    while True do
    begin
      case   Char(S[I]) of
       Char('0').. Char('9'): Dig := Ord(S[I]) -  Ord('0');
       Char('A').. Char('F'): Dig := Ord(S[I]) - (Ord('A') - 10);
       Char('a').. Char('f'): Dig := Ord(S[I]) - (Ord('a') - 10);
      else
        Break;
      end;
      if Result > (MAX_TUINT64 shr 4) then
        Break;
      if Sign and (Dig <> 0) then
        Break;
      Result := Result shl 4 + Dig;
      Inc(I);
      Empty := False;
    end;
  end
  else
  begin
    while True do
    begin
      case Char(S[I]) of
        Char('0').. Char('9'): Dig := Ord(S[I]) - Ord('0');
      else
        Break;
      end;

      if Result > UInt64Div(MAX_TUINT64, 10) then
        Break;
      if Sign and (Dig <> 0) then
        Break;
      Result := Result * 10 + Dig;
      Inc(I);
      Empty := False;
    end;
  end;

  if (S[I] <> Char(#0)) or Empty then
    Code := I + 1 - FirstIndex
  else
    Code := 0;
end;

function UInt64ToHex(N: TUInt64): string;
const
  Digits: array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

  function HC(B: Byte): string;
  begin
    Result := string(Digits[(B shr 4) and $0F] + Digits[B and $0F]);
  end;

begin
  Result :=
      HC(Byte((N and $FF00000000000000) shr 56))
    + HC(Byte((N and $00FF000000000000) shr 48))
    + HC(Byte((N and $0000FF0000000000) shr 40))
    + HC(Byte((N and $000000FF00000000) shr 32))
    + HC(Byte((N and $00000000FF000000) shr 24))
    + HC(Byte((N and $0000000000FF0000) shr 16))
    + HC(Byte((N and $000000000000FF00) shr 8))
    + HC(Byte((N and $00000000000000FF)));
end;

function UInt64ToStr(N: TUInt64): string;
begin
  Result := Format('%u', [N]);
end;

function StrToUInt64(const S: string): TUInt64;
{$IFNDEF DELPHIXE6_UP}
var
  E: Integer;
{$ENDIF}
begin
{$IFDEF DELPHIXE6_UP}
  Result := SysUtils.StrToUInt64(S);  // StrToUInt64 only exists under XE6 or above
{$ELSE}
  Result := _ValUInt64(S,  E);
  if E <> 0 then raise EConvertError.CreateResFmt(@SInvalidInteger, [S]);
{$ENDIF}
end;

function UInt64Compare(A, B: TUInt64): Integer;
{$IFNDEF SUPPORT_UINT64}
var
  HiA, HiB, LoA, LoB: LongWord;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  if A > B then
    Result := 1
  else if A < B then
    Result := -1
  else
    Result := 0;
{$ELSE}
  HiA := (A and $FFFFFFFF00000000) shr 32;
  HiB := (B and $FFFFFFFF00000000) shr 32;
  if HiA > HiB then
    Result := 1
  else if HiA < HiB then
    Result := -1
  else
  begin
    LoA := LongWord(A and $00000000FFFFFFFF);
    LoB := LongWord(B and $00000000FFFFFFFF);
    if LoA > LoB then
      Result := 1
    else if LoA < LoB then
      Result := -1
    else
      Result := 0;
  end;
{$ENDIF}
end;

function UInt64Sqrt(N: TUInt64): TUInt64;
var
  Rem, Root: TUInt64;
  I: Integer;
begin
  Result := 0;
  if N = 0 then
    Exit;

  if UInt64Compare(N, 4) < 0 then
  begin
    Result := 1;
    Exit;
  end;

  Rem := 0;
  Root := 0;

  for I := 0 to 31 do
  begin
    Root := Root shl 1;
    Inc(Root);

    Rem := Rem shl 2;
    Rem := Rem or (N shr 62);
    N := N shl 2;

    if UInt64Compare(Root, Rem) <= 0 then
    begin
      Rem := Rem - Root;
      Inc(Root);
    end
    else
      Dec(Root);
  end;
  Result := Root shr 1;
end;

function UInt32IsNegative(N: Cardinal): Boolean;
begin
  Result := (N and (1 shl 31)) <> 0;
end;

function UInt64IsNegative(N: TUInt64): Boolean;
begin
{$IFDEF SUPPORT_UINT64}
  Result := (N and (UInt64(1) shl 63)) <> 0;
{$ELSE}
  Result := N < 0;
{$ENDIF}
end;

// 给 UInt64 的某一位置 1，位 Index 从 0 开始
procedure UInt64SetBit(var B: TUInt64; Index: Integer);
begin
  B := B or (TUInt64(1) shl Index);
end;

// 给 UInt64 的某一位置 0，位 Index 从 0 开始
procedure UInt64ClearBit(var B: TUInt64; Index: Integer);
begin
  B := B and not (TUInt64(1) shl Index);
end;

// 返回 UInt64 的第几位是否是 1，0 开始
function GetUInt64BitSet(B: TUInt64; Index: Integer): Boolean;
begin
  B := B and (TUInt64(1) shl Index);
  Result := B <> 0;
end;

// 返回 Int64 的是 1 的最高二进制位是第几位，最低位是 0，如果没有 1，返回 -1
function GetUInt64HighBits(B: TUInt64): Integer;
begin
  if B = 0 then
  begin
    Result := -1;
    Exit;
  end;

  Result := 1;
  if B shr 32 = 0 then
  begin
   Inc(Result, 32);
   B := B shl 32;
  end;
  if B shr 48 = 0 then
  begin
   Inc(Result, 16);
   B := B shl 16;
  end;
  if B shr 56 = 0 then
  begin
    Inc(Result, 8);
    B := B shl 8;
  end;
  if B shr 60 = 0 then
  begin
    Inc(Result, 4);
    B := B shl 4;
  end;
  if B shr 62 = 0 then
  begin
    Inc(Result, 2);
    B := B shl 2;
  end;
  Result := Result - Integer(B shr 63); // 得到前导 0 的数量
  Result := 63 - Result;
end;

// 返回 Cardinal 的是 1 的最高二进制位是第几位，最低位是 0，如果没有 1，返回 -1
function GetUInt32HighBits(B: Cardinal): Integer;
begin
  if B = 0 then
  begin
    Result := -1;
    Exit;
  end;

  Result := 1;
  if B shr 16 = 0 then
  begin
   Inc(Result, 16);
   B := B shl 16;
  end;
  if B shr 24 = 0 then
  begin
    Inc(Result, 8);
    B := B shl 8;
  end;
  if B shr 28 = 0 then
  begin
    Inc(Result, 4);
    B := B shl 4;
  end;
  if B shr 30 = 0 then
  begin
    Inc(Result, 2);
    B := B shl 2;
  end;
  Result := Result - Integer(B shr 31); // 得到前导 0 的数量
  Result := 31 - Result;
end;

// 返回 Int64 的是 1 的最低二进制位是第几位，最低位是 0，如果没有 1，返回 -1
function GetUInt64LowBits(B: TUInt64): Integer;
var
  Y: TUInt64;
  N: Integer;
begin
  Result := -1;
  if B = 0 then
    Exit;

  N := 63;
  Y := B shl 32;
  if Y <> 0 then
  begin
    Dec(N, 32);
    B := Y;
  end;
  Y := B shl 16;
  if Y <> 0 then
  begin
    Dec(N, 16);
    B := Y;
  end;
  Y := B shl 8;
  if Y <> 0 then
  begin
    Dec(N, 8);
    B := Y;
  end;
  Y := B shl 4;
  if Y <> 0 then
  begin
    Dec(N, 4);
    B := Y;
  end;
  Y := B shl 2;
  if Y <> 0 then
  begin
    Dec(N, 2);
    B := Y;
  end;
  B := B shl 1;
  Result := N - Integer(B shr 63);
end;

// 返回 Cardinal 的是 1 的最低二进制位是第几位，最低位是 0，如果没有 1，返回 -1
function GetUInt32LowBits(B: Cardinal): Integer;
var
  Y, N: Integer;
begin
  Result := -1;
  if B = 0 then
    Exit;

  N := 31;
  Y := B shl 16;
  if Y <> 0 then
  begin
    Dec(N, 16);
    B := Y;
  end;
  Y := B shl 8;
  if Y <> 0 then
  begin
    Dec(N, 8);
    B := Y;
  end;
  Y := B shl 4;
  if Y <> 0 then
  begin
    Dec(N, 4);
    B := Y;
  end;
  Y := B shl 2;
  if Y <> 0 then
  begin
    Dec(N, 2);
    B := Y;
  end;
  B := B shl 1;
  Result := N - Integer(B shr 31);
end;

// 封装的 Int64 Mod，碰到负值时取反求模再模减
function Int64Mod(M, N: Int64): Int64;
begin
  if M > 0 then
    Result := M mod N
  else
    Result := N - ((-M) mod N);
end;

// 判断一 32 位无符号整数是否 2 的整数次幂
function IsUInt32PowerOf2(N: Cardinal): Boolean;
begin
  Result := (N and (N - 1)) = 0;
end;

// 判断一 64 位无符号整数是否 2 的整数次幂
function IsUInt64PowerOf2(N: TUInt64): Boolean;
begin
  Result := (N and (N - 1)) = 0;
end;

// 得到一比指定 32 位无符号整数数大或等的 2 的整数次幂，如溢出则返回 0
function GetUInt32PowerOf2GreaterEqual(N: Cardinal): Cardinal;
begin
  Result := N - 1;
  Result := Result or (Result shr 1);
  Result := Result or (Result shr 2);
  Result := Result or (Result shr 4);
  Result := Result or (Result shr 8);
  Result := Result or (Result shr 16);
  Inc(Result);
end;

// 得到一比指定 64 位无符号整数数大的 2 的整数次幂，如溢出则返回 0
function GetUInt64PowerOf2GreaterEqual(N: TUInt64): TUInt64;
begin
  Result := N - 1;
  Result := Result or (Result shr 1);
  Result := Result or (Result shr 2);
  Result := Result or (Result shr 4);
  Result := Result or (Result shr 8);
  Result := Result or (Result shr 16);
  Result := Result or (Result shr 32);
  Inc(Result);
end;

// 判断两个 32 位有符号数相加是否溢出 32 位有符号上限
function IsInt32AddOverflow(A, B: Integer): Boolean;
var
  C: Integer;
begin
  C := A + B;
  Result := ((A > 0) and (B > 0) and (C < 0)) or   // 同符号且结果换号了说明出现了溢出
    ((A < 0) and (B < 0) and (C > 0));
end;

// 判断两个 32 位无符号数相加是否溢出 32 位无符号上限
function IsUInt32AddOverflow(A, B: Cardinal): Boolean;
begin
  Result := (A + B) < A; // 无符号相加，结果只要小于任一个数就说明溢出了
end;

// 判断两个 64 位有符号数相加是否溢出 64 位有符号上限
function IsInt64AddOverflow(A, B: Int64): Boolean;
var
  C: Int64;
begin
  C := A + B;
  Result := ((A > 0) and (B > 0) and (C < 0)) or   // 同符号且结果换号了说明出现了溢出
    ((A < 0) and (B < 0) and (C > 0));
end;

// 判断两个 64 位无符号数相加是否溢出 64 位无符号上限
function IsUInt64AddOverflow(A, B: TUInt64): Boolean;
begin
  Result := UInt64Compare(A + B, A) < 0; // 无符号相加，结果只要小于任一个数就说明溢出了
end;

// 两个 64 位无符号数相加，A + B => R，如果有溢出，则溢出的 1 搁进位标记里，否则清零
procedure UInt64Add(var R: TUInt64; A, B: TUInt64; out Carry: Integer);
begin
  R := A + B;
  if UInt64Compare(R, A) < 0 then // 无符号相加，结果只要小于任一个数就说明溢出了
    Carry := 1
  else
    Carry := 0;
end;

// 两个 64 位无符号数相减，A - B => R，如果不够减有借位，则借的 1 搁借位标记里，否则清零
procedure UInt64Sub(var R: TUInt64; A, B: TUInt64; out Carry: Integer);
begin
  R := A - B;
  if UInt64Compare(R, A) > 0 then // 无符号相减，结果只要大于被减数就说明借位了
    Carry := 1
  else
    Carry := 0;
end;

// 判断两个 32 位有符号数相乘是否溢出 32 位有符号上限
function IsInt32MulOverflow(A, B: Integer): Boolean;
var
  T: Integer;
begin
  T := A * B;
  Result := (B <> 0) and ((T div B) <> A);
end;

// 判断两个 32 位无符号数相乘是否溢出 32 位无符号上限
function IsUInt32MulOverflow(A, B: Cardinal): Boolean;
var
  T: TUInt64;
begin
  T := TUInt64(A) * TUInt64(B);
  Result := (T = Cardinal(T));
end;

// 判断两个 32 位无符号数相乘是否溢出 64 位有符号数，如未溢出也即返回 False 时，R 中直接返回结果
function IsUInt32MulOverflowInt64(A, B: Cardinal; out R: TUInt64): Boolean;
var
  T: Int64;
begin
  T := Int64(A) * Int64(B);
  Result := T < 0; // 如果出现 Int64 负值则说明溢出
  if not Result then
    R := TUInt64(T);
end;

// 判断两个 64 位有符号数相乘是否溢出 64 位有符号上限
function IsInt64MulOverflow(A, B: Int64): Boolean;
var
  T: Int64;
begin
  T := A * B;
  Result := (B <> 0) and ((T div B) <> A);
end;

// 指针类型转换成整型，支持 32/64 位
function PointerToInteger(P: Pointer): Integer;
begin
{$IFDEF CPUX64}
  // 先这么写，利用 Pointer 的低 32 位存 Integer
  Result := Integer(P);
{$ELSE}
  Result := Integer(P);
{$ENDIF}
end;

// 整型转换成指针类型，支持 32/64 位
function IntegerToPointer(I: Integer): Pointer;
begin
{$IFDEF CPUX64}
  // 先这么写，利用 Pointer 的低 32 位存 Integer
  Result := Pointer(I);
{$ELSE}
  Result := Pointer(I);
{$ENDIF}
end;

// 求 Int64 范围内俩加数的和求余，处理溢出的情况，要求 N 大于 0
function Int64NonNegativeAddMod(A, B, N: Int64): Int64;
begin
  if IsInt64AddOverflow(A, B) then // 如果加起来溢出 Int64
  begin
    if A > 0 then
    begin
      // A 和 B 都大于 0，采用 UInt64 相加取模（和未溢出 UInt64 上限），注意 N 未溢出 Int64 因此取模结果小于 Int64 上限，不会变成负值
      Result := UInt64NonNegativeAddMod(A, B, N);
    end
    else
    begin
      // A 和 B 都小于 0，取反后采用 UInt64 相加取模（反后的和未溢出 UInt64 上限），模再被除数减一下
      Result := N - UInt64NonNegativeAddMod(-A, -B, N);
    end;
  end
  else // 不溢出，直接加起来求余
    Result := Int64NonNegativeMod(A + B, N);
end;

// 求 UInt64 范围内俩加数的和求余，处理溢出的情况，要求 N 大于 0
function UInt64NonNegativeAddMod(A, B, N: TUInt64): TUInt64;
var
  C, D: TUInt64;
begin
  if IsUInt64AddOverflow(A, B) then // 如果加起来溢出
  begin
    C := UInt64Mod(A, N);  // 就各自求模
    D := UInt64Mod(B, N);
    if IsUInt64AddOverflow(C, D) then
    begin
      // 如果还是溢出，说明模比两个加数都大，各自求模没用。
      // 至少有一个加数大于等于 2^63，N 至少是 2^63 + 1
      // 和 = 溢出结果 + 2^64
      // 和 mod N = 溢出结果 mod N + (2^64 - 1) mod N) + 1
      // 这里 N 至少是 2^63 + 1，溢出结果最多是 2^64 - 2，所以前两项相加不会溢出，可以直接相加后减一再求模
      Result := UInt64Mod(UInt64Mod(A + B, N) + UInt64Mod(MAX_TUINT64, N) + 1, N);
    end
    else
      Result := UInt64Mod(C + D, N);
  end
  else
  begin
    Result := UInt64Mod(A + B, N);
  end;
end;

function Int64NonNegativeMulMod(A, B, N: Int64): Int64;
var
  Neg: Boolean;
begin
  if N <= 0 then
    raise EDivByZero.Create(SDivByZero);

  // 范围小就直接算
  if not IsInt64MulOverflow(A, B) then
  begin
    Result := A * B mod N;
    if Result < 0 then
      Result := Result + N;
    Exit;
  end;

  // 调整符号到正
  Result := 0;
  if (A = 0) or (B = 0) then
    Exit;

  Neg := False;
  if (A < 0) and (B > 0) then
  begin
    A := -A;
    Neg := True;
  end
  else if (A > 0) and (B < 0) then
  begin
    B := -B;
    Neg := True;
  end
  else if (A < 0) and (B < 0) then
  begin
    A := -A;
    B := -B;
  end;

  // 移位循环算
  while B <> 0 do
  begin
    if (B and 1) <> 0 then
      Result := ((Result mod N) + (A mod N)) mod N;

    A := A shl 1;
    if A >= N then
      A := A mod N;

    B := B shr 1;
  end;

  if Neg then
    Result := N - Result;
end;

function UInt64NonNegativeMulMod(A, B, N: TUInt64): TUInt64;
begin
  Result := 0;
  if (UInt64Compare(A, MAX_UINT32) <= 0) and (UInt64Compare(B, MAX_UINT32) <= 0) then
  begin
    Result := UInt64Mod(A * B, N); // 足够小的话直接乘后求模
  end
  else
  begin
    while B <> 0 do
    begin
      if (B and 1) <> 0 then
        Result := UInt64NonNegativeAddMod(Result, A, N);

      A := UInt64NonNegativeAddMod(A, A, N);
      // 不能用传统算法里的 A := A shl 1，大于 N 后再 mod N，因为会溢出

      B := B shr 1;
    end;
  end;
end;

// 封装的非负求余函数，也就是余数为负时，加个除数变正，调用者需保证 P 大于 0
function Int64NonNegativeMod(N: Int64; P: Int64): Int64;
begin
  if P <= 0 then
    raise EDivByZero.Create(SDivByZero);

  Result := N mod P;
  if Result < 0 then
    Inc(Result, P);
end;

// Int64 的非负整数指数幂
function Int64NonNegativPower(N: Int64; Exp: Integer): Int64;
var
  T: Int64;
begin
  if Exp < 0 then
    raise ERangeError.Create(SRangeError)
  else if Exp = 0 then
  begin
    if N <> 0 then
      Result := 1
    else
      raise EDivByZero.Create(SDivByZero);
  end
  else if Exp = 1 then
    Result := N
  else
  begin
    Result := 1;
    T := N;

    while Exp > 0 do
    begin
      if (Exp and 1) <> 0 then
        Result := Result * T;

      Exp := Exp shr 1;
      T := T * T;
    end;
  end;
end;

function Int64NonNegativeRoot(N: Int64; Exp: Integer): Int64;
var
  I: Integer;
  X: Int64;
  X0, X1: Extended;
begin
  if (Exp < 0) or (N < 0) then
    raise ERangeError.Create(SRangeError)
  else if Exp = 0 then
    raise EDivByZero.Create(SDivByZero)
  else if (N = 0) or (N = 1) then
    Result := N
  else if Exp = 2 then
    Result := UInt64Sqrt(N)
  else
  begin
    // 牛顿迭代法求根
    I := GetUInt64HighBits(N) + 1; // 得到大约 Log2 N 的值
    I := (I div Exp) + 1;
    X := 1 shl I;                  // 得到一个较大的 X0 值作为起始值

    X0 := X;
    X1 := X0 - (Power(X0, Exp) - N) / (Exp * Power(X0, Exp - 1));

    while True do
    begin
      if (Trunc(X0) = Trunc(X1)) and (Abs(X0 - X1) < 0.001) then
      begin
        Result := Trunc(X1); // Trunc 只支持 Int64，超界了会出错
        Exit;
      end;

      X0 := X1;
      X1 := X0 - (Power(X0, Exp) - N) / (Exp * Power(X0, Exp - 1));
    end;
  end;
end;

function UInt64NonNegativPower(N: TUInt64; Exp: Integer): TUInt64;
var
  T, RL, RH: TUInt64;
begin
  if Exp < 0 then
    raise ERangeError.Create(SRangeError)
  else if Exp = 0 then
  begin
    if N <> 0 then
      Result := 1
    else
      raise EDivByZero.Create(SDivByZero);
  end
  else if Exp = 1 then
    Result := N
  else
  begin
    Result := 1;
    T := N;

    while Exp > 0 do
    begin
      if (Exp and 1) <> 0 then
      begin
        UInt64MulUInt64(Result, T, RL, RH);
        Result := RL;
      end;

      Exp := Exp shr 1;
      UInt64MulUInt64(T, T, RL, RH);
      T := RL;
    end;
  end;
end;

function UInt64NonNegativeRoot(N: TUInt64; Exp: Integer): TUInt64;
var
  I: Integer;
  X: TUInt64;
  XN, X0, X1: Extended;
begin
  if Exp < 0 then
    raise ERangeError.Create(SRangeError)
  else if Exp = 0 then
    raise EDivByZero.Create(SDivByZero)
  else if (N = 0) or (N = 1) then
    Result := N
  else if Exp = 2 then
    Result := UInt64Sqrt(N)
  else
  begin
    // 牛顿迭代法求根
    I := GetUInt64HighBits(N) + 1; // 得到大约 Log2 N 的值
    I := (I div Exp) + 1;
    X := 1 shl I;                  // 得到一个较大的 X0 值作为起始值

    X0 := UInt64ToExtended(X);
    XN := UInt64ToExtended(N);
    X1 := X0 - (Power(X0, Exp) - XN) / (Exp * Power(X0, Exp - 1));

    while True do
    begin
      if (ExtendedToUInt64(X0) = ExtendedToUInt64(X1)) and (Abs(X0 - X1) < 0.001) then
      begin
        Result := ExtendedToUInt64(X1);
        Exit;
      end;

      X0 := X1;
      X1 := X0 - (Power(X0, Exp) - XN) / (Exp * Power(X0, Exp - 1));
    end;
  end;
end;

function IsUInt128BitSet(Lo, Hi: TUInt64; N: Integer): Boolean;
begin
  if N < 64 then
    Result := (Lo and (TUInt64(1) shl N)) <> 0
  else
  begin
    Dec(N, 64);
    Result := (Hi and (TUInt64(1) shl N)) <> 0;
  end;
end;

procedure SetUInt128Bit(var Lo, Hi: TUInt64; N: Integer);
begin
  if N < 64 then
    Lo := Lo or (TUInt64(1) shl N)
  else
  begin
    Dec(N, 64);
    Hi := Hi or (TUInt64(1) shl N);
  end;
end;

procedure ClearUInt128Bit(var Lo, Hi: TUInt64; N: Integer);
begin
  if N < 64 then
    Lo := Lo and not (TUInt64(1) shl N)
  else
  begin
    Dec(N, 64);
    Hi := Hi and not (TUInt64(1) shl N);
  end;
end;

procedure InternalQuickSort(Mem: Pointer; L, R: Integer; ElementByteSize: Integer;
  CompareProc: TMemSortCompareProc = nil);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while CompareProc(Pointer(TCnNativeInt(Mem) + I * ElementByteSize),
        Pointer(TCnNativeInt(Mem) + P * ElementByteSize), ElementByteSize) < 0 do
        Inc(I);
      while CompareProc(Pointer(TCnNativeInt(Mem) + J * ElementByteSize),
        Pointer(TCnNativeInt(Mem) + P * ElementByteSize), ElementByteSize) > 0 do
        Dec(J);

      if I <= J then
      begin
        MemorySwap(Pointer(TCnNativeInt(Mem) + I * ElementByteSize),
          Pointer(TCnNativeInt(Mem) + J * ElementByteSize), ElementByteSize);

        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;

    if L < J then
      InternalQuickSort(Mem, L, J, ElementByteSize, CompareProc);
    L := I;
  until I >= R;
end;

function DefaultCompareProc(P1, P2: Pointer; ElementByteSize: Integer): Integer;
begin
  Result := MemoryCompare(P1, P2, ElementByteSize);
end;

procedure MemoryQuickSort(Mem: Pointer; ElementByteSize: Integer;
  ElementCount: Integer; CompareProc: TMemSortCompareProc);
begin
  if (Mem <> nil) and (ElementCount > 0) and (ElementCount > 0) then
  begin
    if Assigned(CompareProc) then
      InternalQuickSort(Mem, 0, ElementCount - 1, ElementByteSize, CompareProc)
    else
      InternalQuickSort(Mem, 0, ElementCount - 1, ElementByteSize, DefaultCompareProc);
  end;
end;

initialization
  FByteOrderIsBigEndian := CurrentByteOrderIsBigEndian;

end.
