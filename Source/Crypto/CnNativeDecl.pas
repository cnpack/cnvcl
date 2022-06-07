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

unit CnNativeDecl;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：32 位和 64 位的一些统一声明
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：Delphi XE 2 支持 32 和 64 以来，开放出的 NativeInt 和 NativeUInt 随
*           当前是 32 位还是 64 而动态变化，影响到的是 Pointer、Reference等东西。
*           考虑到兼容性，固定长度的 32 位 Cardinal/Integer 等和 Pointer 这些就
*           不能再通用了，即使 32 位下也被编译器禁止。因此本单元声明了几个类型，
*           供同时在低版本和高版本的 Delphi 中使用。
*           后来加入 UInt64 的包装，注意 D567 下不直接支持UInt64 的运算，需要用
*           辅助函数实现，目前实现了 div 与 mod
*           另外地址运算 Integer(APtr) 在 64 位下尤其是 MacOS 上容易出现截断，需要用 NativeInt
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 XE 2
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.03.14 V2.0
*               增加几个十六进制转换函数
*           2022.02.17 V1.9
*               增加 FPC 的编译支持，待测试
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
  Classes, SysUtils, SysConst, Math;

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
      False: (Low32, Hi32: Cardinal);
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

{$IFDEF POSIX64}
  TCnLongWord32 = Cardinal; // Linux64/MacOS64 (or POSIX64?) LongWord is 64 Bits
{$ELSE}
  TCnLongWord32 = LongWord;
{$ENDIF}
  PCnLongWord32 = ^TCnLongWord32;

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
{* 两个无符号 64 位整数相加，处理溢出的情况，结果放 ResLo 与 ResHi 中}

procedure UInt64MulUInt64(A, B: TUInt64; var ResLo, ResHi: TUInt64);
{* 两个无符号 64 位整数相乘，结果放 ResLo 与 ResHi 中}

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

function GetUInt64BitSet(B: TUInt64; Index: Integer): Boolean;
{* 返回 Int64 的某一位是否是 1，位 Index 从 0 开始}

function GetUInt64HighBits(B: TUInt64): Integer;
{* 返回 Int64 的是 1 的最高二进制位是第几位，最低位是 0，如果没有 1，返回 -1}

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
{* 返回当前运行期环境是否是大端，也就是是否将整数中的高序字节存储在较低的起始地址}

function CurrentByteOrderIsLittleEndian: Boolean;
{* 返回当前运行期环境是否是小端，也就是是否将整数中的高序字节存储在较高的起始地址}

function Int64ToBigEndian(Value: Int64): Int64;
{* 确保 Int64 值为大端，小端环境中进行转换}

function Int32ToBigEndian(Value: Integer): Integer;
{* 确保 Int32 值为大端，小端环境中进行转换}

function Int16ToBigEndian(Value: SmallInt): SmallInt;
{* 确保 Int16 值为大端，小端环境中进行转换}

function Int64ToLittleEndian(Value: Int64): Int64;
{* 确保 Int64 值为小端，大端环境中进行转换}

function Int32ToLittleEndian(Value: Integer): Integer;
{* 确保 Int32 值为小端，大端环境中进行转换}

function Int16ToLittleEndian(Value: SmallInt): SmallInt;
{* 确保 Int16 值为小端，大端环境中进行转换}

procedure ReverseMemory(AMem: Pointer; MemLen: Integer);
{* 按字节顺序倒置一块内存块}

function DataToHex(InData: Pointer; ByteLength: Integer; UseUpperCase: Boolean = True): string;
{* 内存块转换为十六进制字符串，UseUpperCase 控制输出内容的大小写}

function HexToData(const Hex: string; OutData: Pointer): string;
{* 十六进制字符串转换为内存块，十六进制字符串长度为奇或转换失败时抛出异常
  注意 OutData 应该指向足够容纳转换内容的区域，长度至少为 Length(Hex) div 2}

function StringToHex(const Data: string; UseUpperCase: Boolean = True): string;
{* 字符串转换为十六进制字符串，UseUpperCase 控制输出内容的大小写}

function HexToString(const Hex: string): string;
{* 十六进制字符串转换为字符串，十六进制字符串长度为奇或转换失败时抛出异常}

{$IFDEF TBYTES_DEFINED}

function BytesToHex(Data: TBytes; UseUpperCase: Boolean = True): string;
{* 字节数组转换为十六进制字符串，UseUpperCase 控制输出内容的大小写}

function HexToBytes(const Hex: string): TBytes;
{* 十六进制字符串转换为字节数组，字符串长度为奇或转换失败时抛出异常}

procedure ReverseBytes(Data: TBytes);
{* 按字节顺序倒置一字节数组}

{$ENDIF}

implementation

uses
  CnFloatConvert;

type
  PCnByte = ^Byte; // 不引用 Windows 中的 PByte

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
  Lo, Hi: LongWord;
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

function Int64ToBigEndian(Value: Int64): Int64;
begin
  if CurrentByteOrderIsBigEndian then
    Result := Value
  else
    Result := SwapInt64(Value);
end;

function Int32ToBigEndian(Value: Integer): Integer;
begin
  if CurrentByteOrderIsBigEndian then
    Result := Value
  else
    Result := Integer((Value and $000000FF) shl 24) or Integer((Value and $0000FF00) shl 8)
      or Integer((Value and $00FF0000) shr 8) or Integer((Value and $FF000000) shr 24);
end;

function Int16ToBigEndian(Value: SmallInt): SmallInt;
begin
  if CurrentByteOrderIsBigEndian then
    Result := Value
  else
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8);
end;

function Int64ToLittleEndian(Value: Int64): Int64;
begin
  if CurrentByteOrderIsLittleEndian then
    Result := Value
  else
    Result := SwapInt64(Value);
end;

function Int32ToLittleEndian(Value: Integer): Integer;
begin
  if CurrentByteOrderIsLittleEndian then
    Result := Value
  else
    Result := Integer((Value and $000000FF) shl 24) or Integer((Value and $0000FF00) shl 8)
      or Integer((Value and $00FF0000) shr 8) or Integer((Value and $FF000000) shr 24);
end;

function Int16ToLittleEndian(Value: SmallInt): SmallInt;
begin
  if CurrentByteOrderIsLittleEndian then
    Result := Value
  else
    Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8);
end;

procedure ReverseMemory(AMem: Pointer; MemLen: Integer);
var
  I, L: Integer;
  P: PByteArray;
  T: Byte;
begin
  if (AMem = nil) or (MemLen < 2) then
    Exit;

  L := MemLen div 2;
  P := PByteArray(AMem);
  for I := 0 to L - 1 do
  begin
    // 交换第 I 和第 MemLen - I - 1
    T := P^[I];
    P^[I] := P^[MemLen - I - 1];
    P^[MemLen - I - 1] := T;
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
      B := PCnByte(TCnNativeInt(InData) + I * SizeOf(Byte))^;
      Result[I * 2 + 1] := HiDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := HiDigits[B and $0F];
    end;
  end
  else
  begin
    for I := 0 to ByteLength - 1 do
    begin
      B := PCnByte(TCnNativeInt(InData) + I * SizeOf(Byte))^;
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
    PCnByte(TCnNativeInt(OutData) + I - 1)^ := Byte(HexToInt(S));
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
      B := PCnByte(TCnNativeInt(Buffer) + I * SizeOf(Char))^;
      Result[I * 2 + 1] := HiDigits[(B shr 4) and $0F];
      Result[I * 2 + 2] := HiDigits[B and $0F];
    end;
  end
  else
  begin
    for I := 0 to L - 1 do
    begin
      B := PCnByte(TCnNativeInt(Buffer) + I * SizeOf(Char))^;
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

{$IFDEF TBYTES_DEFINED}

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

// 两个无符号 64 位整数相乘，结果放 ResLo 与 ResHi 中
procedure UInt64MulUInt64(A, B: TUInt64; var ResLo, ResHi: TUInt64);
var
  X, Y, Z, T: Cardinal;
  YT, XT, ZY, ZX: TUInt64;
  P, R1Lo, R1Hi, R2Lo, R2Hi: TUInt64;
begin
  // 基本思想：2^32 是系数 M，拆成 (xM+y)*(zM+t) = xzM^2 + (xt+yz)M + yt
  // 各项系数都是 UInt64，xz 占 2、3、4，xt+yz 占 1、2、3，yt 占0、1，然后累加
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

end.
