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

unit CnBigNumber;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：大数算法单元
* 单元作者：刘啸
* 备    注：大部分从 Openssl 的 C 代码移植而来，大数池不支持多线程
*           Word 系列操作函数指大数与 DWORD 进行运算，而 Words 系列操作函数指
*           大数中间的运算过程。
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.06.20 V1.6
*               加入快速乘方函数与十进制位数函数
*           2020.01.16 V1.5
*               优化乘法与 MulMod 的速度，去除汇编代码
*           2019.04.16 V1.4
*               支持 Win32/Win64/MacOS
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

uses
  Classes, SysUtils, Math, CnNativeDecl {$IFDEF MSWINDOWS}, Windows {$ENDIF},
  Contnrs, CnRandom {$IFDEF UNICODE}, AnsiStrings {$ENDIF};

const
  BN_FLG_MALLOCED       = $1;    // 本大数对象中的 D 内存是动态分配而来并自行管理
  BN_FLG_STATIC_DATA    = $2;    // 本大数对象中的 D 内存是指向外部的静态数据

  BN_FLG_FREE           = $8000;

  BN_BITS_UINT_32       = 32;
  BN_BITS_UINT_64       = 64;
  BN_BYTES              = 4;
  BN_BITS2              = 32;     // D 数组中的一个元素所包含的位数
  BN_BITS4              = 16;
  BN_TBIT               = $80000000;
  BN_MASK2              = $FFFFFFFF;
  BN_MASK2l             = $FFFF;
  BN_MASK2h             = $FFFF0000;
  BN_MASK2h1            = $FFFF8000;
  BN_MASK3S             = $7FFFFFFFFFFFFFFF;
  BN_MASK3U             = $FFFFFFFFFFFFFFFF;

  BN_MILLER_RABIN_DEF_COUNT = 50; // Miller-Rabin 算法的默认测试次数

type
  TLongWordArray = array [0..MaxInt div SizeOf(Integer) - 1] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TInt64Array = array [0..MaxInt div SizeOf(Int64) - 1] of Int64;
  PInt64Array = ^TInt64Array;

{$IFDEF SUPPORT_UINT64}
  TUInt64Array = array [0..MaxInt div SizeOf(UInt64) - 1] of UInt64;
  PUInt64Array = ^TUInt64Array;
{$ENDIF}

  {* 用来代表一个大数的对象}
  TCnBigNumber = class(TObject)
  private
{$IFDEF DEBUG}
    FIsFromPool: Boolean;
{$ENDIF}
    function GetDecString: string;
    function GetHexString: string;
    function GetDebugDump: string;
  public
    D: PLongWord;       // 一个 array[0..Top-1] of LongWord 数组，越往后越代表高位
    Top: Integer;       // Top 表示上限，D[Top] 为 0，D[Top - 1] 是最高位有效数
    DMax: Integer;      // D 数组的存储上限
    Neg: Integer;       // 1 为负，0 为正
    Flags: Integer;

    constructor Create;
    destructor Destroy; override;

    procedure Init;
    {* 初始化为全 0，并不分配 D 内存}

    procedure Clear;
    {* 将自身数据空间填 0，并不释放 D 内存}

    function IsZero: Boolean;
    {* 返回大数是否为 0}

    procedure SetZero;
    {* 将大数设置为 0}

    function IsOne: Boolean;
    {* 返回大数是否为 1}

    function IsNegOne: Boolean;
    {* 返回大数是否为 -1}

    function SetOne: Boolean;
    {* 将大数设置为 1}

    function IsOdd: Boolean;
    {* 返回大数是否为奇数}

    function GetBitsCount: Integer;
    {* 返回大数有多少个有效 Bits}

    function GetBytesCount: Integer;
    {* 返回大数有多少个有效 Bytes}

    function GetTenPrecision: Integer;
    {* 返回大数有多少个十进制位}

    function GetWord: LongWord;
    {* 取 DWORD 型首值}

    function SetWord(W: LongWord): Boolean;
    {* 给大数赋 DWORD 型首值}

    function GetInt64: Int64;
    {* 取 Int64 型首值}

    function SetInt64(W: Int64): Boolean;
    {* 给大数赋 Int64 型首值}

{$IFDEF SUPPORT_UINT64}

    function GetUInt64: UInt64;
    {* 取 UInt64 型首值}

    function SetUInt64(W: UInt64): Boolean;
    {* 给大数赋 UInt64 型首值}

{$ENDIF}

    function IsWord(W: LongWord): Boolean;
    {* 大数是否等于指定 DWORD}

    function AddWord(W: LongWord): Boolean;
    {* 大数加上一个 DWORD，结果仍放自身中，返回相加是否成功}

    function SubWord(W: LongWord): Boolean;
    {* 大数减去一个 DWORD，结果仍放自身中，返回相减是否成功}

    function MulWord(W: LongWord): Boolean;
    {* 大数乘以一个 DWORD，结果仍放自身中，返回相乘是否成功}

    function ModWord(W: LongWord): LongWord;
    {* 大数对一个 DWORD 求余，返回余数}

    function DivWord(W: LongWord): LongWord;
    {* 大数除以一个 DWORD，商重新放在自身中，返回余数}

    function PowerWord(W: LongWord): Boolean;
    {* 大数乘方，结果重新放在自身中，返回乘方是否成功}

    procedure SetNegative(Negative: Boolean);
    {* 设置大数是否负值}

    function IsNegative: Boolean;
    {* 返回大数是否负值}

    procedure Negate;
    {* 大数正负号反号}

    function ClearBit(N: Integer): Boolean;
    {* 给大数的第 N 个 Bit 置 0，返回成功与否。N 从最低位 0 到最高位 GetBitsCount - 1}

    function SetBit(N: Integer): Boolean;
    {* 给大数的第 N 个 Bit 置 1，返回成功与否。N 从最低位 0 到最高位 GetBitsCount - 1}

    function IsBitSet(N: Integer): Boolean;
    {* 返回大数的第 N 个 Bit 是否为 1。N 从最低位 0 到最高位 GetBitsCount - 1}

    function WordExpand(Words: Integer): TCnBigNumber;
    {* 将大数扩展成支持 Words 个 DWORD，成功返回扩展的大数对象本身 Self，失败返回 nil}

    function ToBinary(const Buf: PAnsiChar): Integer;
    {* 将大数转换成二进制数据放入 Buf 中，Buf 的长度必须大于等于其 BytesCount，
       返回 Buf 写入的长度}
    function SetBinary(Buf: PAnsiChar; Len: Integer): Boolean;
    {* 根据一个二进制块给自身赋值}

    class function FromBinary(Buf: PAnsiChar; Len: Integer): TCnBigNumber;
    {* 根据一个二进制块产生一个新的大数对象}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大数转成字符串}

    function ToHex: string;
    {* 将大数转成十六进制字符串}

    function SetHex(const Buf: AnsiString): Boolean;
    {* 根据一串十六进制字符串给自身赋值}

    class function FromHex(const Buf: AnsiString): TCnBigNumber;
    {* 根据一串十六进制字符串产生一个新的大数对象}

    function ToDec: string;
    {* 将大数转成十进制字符串}

    function SetDec(const Buf: AnsiString): Boolean;
    {* 根据一串十进制字符串给自身赋值}

    class function FromDec(const Buf: AnsiString): TCnBigNumber;
    {* 根据一串十进制字符串产生个新的大数对象}

    property DecString: string read GetDecString;
    property HexString: string read GetHexString;

    property DebugDump: string read GetDebugDump;
  end;
  PCnBigNumber = ^TCnBigNumber;

  TCnBigNumberList = class(TObjectList)
  {* 容纳大数的对象列表，同时拥有大数对象们}
  private

  protected
    function GetItem(Index: Integer): TCnBigNumber;
    procedure SetItem(Index: Integer; ABigNumber: TCnBigNumber);
  public
    constructor Create(AOwnsObjects: Boolean); overload;

    function Add(ABigNumber: TCnBigNumber): Integer;
    {* 添加大数对象，注意添加后无需手动释放}
    function Remove(ABigNumber: TCnBigNumber): Integer;
    function IndexOfValue(ABigNumber: TCnBigNumber): Integer;
    procedure Insert(Index: Integer; ABigNumber: TCnBigNumber);
    procedure RemoveDuplicated;
    property Items[Index: Integer]: TCnBigNumber read GetItem write SetItem; default;
  end;

function BigNumberNew: TCnBigNumber;
{* 创建一个动态分配的大数对象，等同于 TCnBigNumber.Create}

procedure BigNumberFree(const Num: TCnBigNumber);
{* 按需要释放一个由 BigNumerNew 函数创建的大数对象，并按需要释放其 D 对象
   等同于直接调用 Free}

procedure BigNumberInit(const Num: TCnBigNumber);
{* 初始化一个大数对象，全为 0，并不分配 D 内存}

procedure BigNumberClear(const Num: TCnBigNumber);
{* 清除一个大数对象，并将其数据空间填 0，并不释放 D 内存}

function BigNumberIsZero(const Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 返回一个大数对象里的大数是否为 0}

function BigNumberSetZero(const Num: TCnBigNumber): Boolean;
{* 将一个大数对象里的大数设置为 0}

function BigNumberIsOne(const Num: TCnBigNumber): Boolean;
{* 返回一个大数对象里的大数是否为 1}

function BigNumberIsNegOne(const Num: TCnBigNumber): Boolean;
{* 返回一个大数对象里的大数是否为 -1}

function BigNumberSetOne(const Num: TCnBigNumber): Boolean;
{* 将一个大数对象里的大数设置为 1}

function BigNumberIsOdd(const Num: TCnBigNumber): Boolean;
{* 返回一个大数对象里的大数是否为奇数}

function BigNumberGetBitsCount(const Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效 Bits}

function BigNumberGetBytesCount(const Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效 Bytes}

function BigNumberGetTenPrecision(const Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效十进制位数}

function BigNumberGetWord(const Num: TCnBigNumber): LongWord;
{* 取一个大数对象的首值}

function BigNumberSetWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* 给一个大数对象赋首值}

function BigNumberGetInt64(const Num: TCnBigNumber): Int64;
{* 取一个大数对象的首值 Int64}

function BigNumberSetInt64(const Num: TCnBigNumber; W: Int64): Boolean;
{* 给一个大数对象赋首值 Int64}

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(const Num: TCnBigNumber): UInt64;
{* 取一个大数对象的首值 UInt64}

function BigNumberSetUInt64(const Num: TCnBigNumber; W: UInt64): Boolean;
{* 给一个大数对象赋首值 UInt64}

{$ENDIF}

function BigNumberIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* 某大数是否等于指定 DWORD}

function BigNumberAbsIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* 某大数绝对值是否等于指定 DWORD}

function BigNumberAddWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* 大数加上一个 DWORD，结果仍放 Num 中，返回相加是否成功}

function BigNumberSubWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* 大数减去一个 DWORD，结果仍放 Num 中，返回相减是否成功}

function BigNumberMulWord(const Num: TCnBigNumber; W: LongWord): Boolean;
{* 大数乘以一个 DWORD，结果仍放 Num 中，返回相乘是否成功}

function BigNumberModWord(const Num: TCnBigNumber; W: LongWord): LongWord;
{* 大数对一个 DWORD 求余，返回余数}

function BigNumberDivWord(const Num: TCnBigNumber; W: LongWord): LongWord;
{* 大数除以一个 DWORD，商重新放在 Num 中，返回余数}

procedure BigNumberSetNegative(const Num: TCnBigNumber; Negative: Boolean);
{* 给一个大数对象设置是否负值}

function BigNumberIsNegative(const Num: TCnBigNumber): Boolean;
{* 返回一个大数对象是否负值}

procedure BigNumberNegate(const Num: TCnBigNumber);
{* 给一个大数对象设置正负反号}

function BigNumberClearBit(const Num: TCnBigNumber; N: Integer): Boolean;
{* 给一个大数对象的第 N 个 Bit 置 0，返回成功与否。N 为 0 时代表二进制最低位。}

function BigNumberSetBit(const Num: TCnBigNumber; N: Integer): Boolean;
{* 给一个大数对象的第 N 个 Bit 置 1，返回成功与否。N 为 0 时代表二进制最低位。}

function BigNumberIsBitSet(const Num: TCnBigNumber; N: Integer): Boolean;
{* 返回一个大数对象的第 N 个 Bit 是否为 1。N 为 0 时代表二进制最低位。}

function BigNumberWordExpand(const Num: TCnBigNumber; Words: Integer): TCnBigNumber;
{* 将一个大数对象扩展成支持 Words 个 DWORD，成功返回扩展的大数对象地址，失败返回 nil}

function BigNumberToBinary(const Num: TCnBigNumber; Buf: PAnsiChar): Integer;
{* 将一个大数转换成二进制数据放入 Buf 中，Buf 的长度必须大于等于其 BytesCount，
   返回 Buf 写入的长度，注意不处理正负号}

function BigNumberWriteBinaryToStream(const Num: TCnBigNumber; Stream: TStream): Integer;
{* 将一个大数的二进制部分写入流，返回写入的长度}

function BigNumberFromBinary(Buf: PAnsiChar; Len: Integer): TCnBigNumber;
{* 将一个二进制块转换成大数对象，注意不处理正负号。其结果不用时必须用 BigNumberFree 释放}

function BigNumberSetBinary(Buf: PAnsiChar; Len: Integer;
  const Res: TCnBigNumber): Boolean;
{* 将一个二进制块赋值给指定大数对象，注意不处理正负号}

function BigNumberToString(const Num: TCnBigNumber): string;
{* 将一个大数对象转成字符串，负以 - 表示}

function BigNumberToHex(const Num: TCnBigNumber): string;
{* 将一个大数对象转成十六进制字符串，负以 - 表示}

function BigNumberSetHex(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
{* 将一串十六进制字符串赋值给指定大数对象，负以 - 表示，内部不能包括回车换行}

function BigNumberFromHex(const Buf: AnsiString): TCnBigNumber;
{* 将一串十六进制字符串转换为大数对象，负以 - 表示。其结果不用时必须用 BigNumberFree 释放}

function BigNumberToDec(const Num: TCnBigNumber): AnsiString;
{* 将一个大数对象转成十进制字符串，负以 - 表示}

function BigNumberSetDec(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
{* 将一串十进制字符串赋值给指定大数对象，负以 - 表示，内部不能包括回车换行}

function BigNumberFromDec(const Buf: AnsiString): TCnBigNumber;
{* 将一串十进制字符串转换为大数对象，负以 - 表示。其结果不用时必须用 BigNumberFree 释放}

function BigNumberCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
{* 带符号比较两个大数对象，前者大于等于小于后者分别返回 1、0、-1}

function BigNumberUnsignedCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
{* 无符号比较两个大数对象，前者大于等于小于后者分别返回 1、0、-1}

function BigNumberDuplicate(const Num: TCnBigNumber): TCnBigNumber;
{* 创建并复制一个大数对象，返回此新大数对象，需要用 BigNumberFree 来释放}

function BigNumberCopy(const Dst: TCnBigNumber; const Src: TCnBigNumber): TCnBigNumber;
{* 复制一个大数对象，成功返回 Dst}

procedure BigNumberSwap(const Num1: TCnBigNumber; const Num2: TCnBigNumber);
{* 交换两个大数对象的内容}

function BigNumberRandBytes(const Num: TCnBigNumber; BytesCount: Integer): Boolean;
{* 产生固定字节长度的随机大数}

function BigNumberRandBits(const Num: TCnBigNumber; BitsCount: Integer): Boolean;
{* 产生固定位长度的随机大数}

function BigNumberRandRange(const Num: TCnBigNumber; const Range: TCnBigNumber): Boolean;
{* 产生 [0, Range) 之间的随机大数}

function BigNumberUnsignedAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象无符号相加，结果放至 Res 中，返回相加是否成功}

function BigNumberUnsignedSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象无符号相减，Num1 减 Num2，结果放至 Res 中，
  返回相减是否成功，如 Num1 < Num2 则失败}

function BigNumberAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象带符号相加，结果放至 Res 中，返回相加是否成功，Res 可以是 Num1 或 Num2}

function BigNumberSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象带符号相减，结果放至 Res 中，返回相减是否成功，Res 可以是 Num1 或 Num2}

function BigNumberShiftLeftOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
{* 将一大数对象左移一位，结果放至 Res 中，返回左移是否成功，Res 可以是 Num}

function BigNumberShiftRightOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
{* 将一大数对象右移一位，结果放至 Res 中，返回右移是否成功，Res 可以是 Num}

function BigNumberShiftLeft(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
{* 将一大数对象左移 N 位，结果放至 Res 中，返回左移是否成功，Res 可以是 Num}

function BigNumberShiftRight(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
{* 将一大数对象右移 N 位，结果放至 Res 中，返回右移是否成功，Res 可以是 Num}

function BigNumberSqr(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
{* 计算一大数对象的平方，结果放 Res 中，返回平方计算是否成功}

function BigNumberMul(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 计算两大数对象的乘积，结果放 Res 中，返回乘积计算是否成功，Res 可以是 Num1 或 Num2}

function BigNumberDiv(const Res: TCnBigNumber; const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* 两大数对象相除，Num / Divisor，商放 Res 中，余数放 Remain 中，返回除法计算是否成功，
   Res 可以是 Num}

function BigNumberMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* 两大数对象求余，Num mod Divisor，余数放 Remain 中，返回求余计算是否成功，Remain 可以是 Num}

function BigNumberNonNegativeMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* 两大数对象非负求余，Num mod Divisor，余数放 Remain 中，0 <= Remain < |Divisor|
   Remain 始终大于零，返回求余计算是否成功}

function BigNumberPower(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: LongWord): Boolean;
{* 求大数的整数次方，返回计算是否成功，Res 可以是 Num}

function BigNumberExp(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
{* 求大数 Num 的 Exponent  次方，返回乘方计算是否成功，极其耗时}

function BigNumberGcd(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 求俩大数 Num1 与 Num2 的最大公约数}

function BigNumberLcm(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 求俩大数 Num1 与 Num2 的最小公倍数}

function BigNumberUnsignedMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
{* 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）
  注意: 三个参数均会忽略负值，也就是均用正值参与计算}

function BigNumberMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
{* 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）
  注意: A、B 允许是负值，乘积为负时，结果为 C - 乘积为正的余}

function BigNumberDirectMulMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
{* 普通计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）
  注意：位数较少时，该方法比上面的 BigNumberMulMod 方法要快不少，其余同上}

function BigNumberPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
{* 快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、B、C 之一，性能比下面的蒙哥马利法好大约百分之十}

function BigNumberMontgomeryPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
{* 蒙哥马利法快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、B、C 之一，性能略差，可以不用}

function BigNumberIsProbablyPrime(const Num: TCnBigNumber; TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 概率性判断一个大数是否素数，TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢}

function BigNumberGeneratePrime(const Num: TCnBigNumber; BytesCount: Integer;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 生成一个指定字节位数的大素数，TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢}

function BigNumberGeneratePrimeByBitsCount(const Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 生成一个指定二进制位数的大素数，TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢}

function BigNumberCheckPrimitiveRoot(R, Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
{* 原根判断辅助函数。判断 R 是否对于 Prime - 1 的每个因子，都有 R ^ (剩余因子的积) mod Prime <> 1
   Factors 必须是 Prime - 1 的不重复的质因数列表，可从 BigNumberFindFactors 获取并去重而来}

function BigNumberIsInt32(const Num: TCnBigNumber): Boolean;
{* 大数是否是一个 32 位有符号整型范围内的数}

function BigNumberIsUInt32(const Num: TCnBigNumber): Boolean;
{* 大数是否是一个 32 位无符号整型范围内的数}

function BigNumberIsInt64(const Num: TCnBigNumber): Boolean;
{* 大数是否是一个 64 位有符号整型范围内的数}

function BigNumberIsUInt64(const Num: TCnBigNumber): Boolean;
{* 大数是否是一个 64 位无符号整型范围内的数}

procedure BigNumberExtendedEuclideanGcd(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解
   A, B 是已知大数，X, Y 是解出来的结果，注意 X 有可能小于 0，如需要正数，可以再加上 B}

procedure BigNumberExtendedEuclideanGcd2(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X - B * Y = 1 的整数解
   A, B 是已知大数，X, Y 是解出来的结果，注意 X 有可能小于 0，如需要正数，可以再加上 B
   X 被称为 A 针对 B 的模反元素，因此本算法也用来算 A 针对 B 的模反元素
   （由于可以视作 -Y，所以本方法与上一方法是等同的 ）}

procedure BigNumberModularInverse(const Res: TCnBigNumber; X, Modulus: TCnBigNumber);
{* 求 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。
   调用者须自行保证 X、Modulus 互质}

function BigNumberLegendre(A, P: TCnBigNumber): Integer;
{* 用二次互反律递归计算勒让德符号 ( A / P) 的值，较快}

function BigNumberLegendre2(A, P: TCnBigNumber): Integer;
{* 用欧拉判别法计算勒让德符号 ( A / P) 的值，较慢}

function BigNumberTonelliShanks(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
{* 使用 Tonelli-Shanks 算法进行模素数二次剩余求解，也就是求 Res^2 mod P = A，返回是否有解
   调用者需自行保证 P 为奇素数或奇素数的整数次方}

function BigNumberLucas(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
{* 使用 IEEE P1363 规范中的 Lucas 序列进行模素数二次剩余求解，也就是求 Res^2 mod P = A，返回是否有解}

procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
{* 找出大数的质因数列表}

function BigNumberLucasSequenceMod(X, Y, K, N: TCnBigNumber; Q, V: TCnBigNumber): Boolean;
{* 计算 IEEE P1363 的规范中说明的 Lucas 序列，调用者需自行保证 N 为奇素数
   Lucas 序列递归定义为：V0 = 2, V1 = X, and Vk = X * Vk-1 - Y * Vk-2   for k >= 2
   V 返回 Vk mod N，Q 返回 Y ^ (K div 2) mod N}

function BigNumberDebugDump(const Num: TCnBigNumber): string;
{* 打印大数内部信息}

var
  CnBigNumberOne: TCnBigNumber = nil;     // 表示 1 的常量
  CnBigNumberZero: TCnBigNumber = nil;    // 表示 0 的常量

implementation

uses
  CnPrimeNumber;

const
  Hex: string = '0123456789ABCDEF';

  BN_CTX_POOL_SIZE = 16;
  BN_CTX_START_FRAMES = 32;
  BN_DEC_CONV = 1000000000;
  BN_DEC_FMT = '%u';
  BN_DEC_FMT2 = '%.9u';
  BN_PRIME_NUMBERS = 2048;

{$IFNDEF MSWINDOWS}
  MAXDWORD = LongWord($FFFFFFFF);
{$ENDIF}

var
  FLocalBigNumberPool: TObjectList = nil;

{* 大数池操作方法开始}

function ObtainBigNumberFromPool: TCnBigNumber;
begin
  if FLocalBigNumberPool.Count = 0 then
  begin
    Result := TCnBigNumber.Create;
{$IFDEF DEBUG}
    Result.FIsFromPool := True;
{$ENDIF}
  end
  else
  begin
    Result := TCnBigNumber(FLocalBigNumberPool.Items[FLocalBigNumberPool.Count - 1]);
    FLocalBigNumberPool.Delete(FLocalBigNumberPool.Count - 1);
    Result.Clear;
  end;
end;

procedure RecycleBigNumberToPool(Num: TCnBigNumber);
begin
  if Num <> nil then
    FLocalBigNumberPool.Add(Num);
end;

{* 大数池操作方法结束}

procedure BigNumberSetFlag(const Num: TCnBigNumber; N: Integer); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Num.Flags := Num.Flags or N;
end;

function BigNumberGetFlag(const Num: TCnBigNumber; N: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Num.Flags and N;
end;

function BigNumberNew: TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
end;

procedure BigNumberInit(const Num: TCnBigNumber);
begin
  // FillChar(Num, SizeOf(TCnBigNumber), 0);
  if Num = nil then
    Exit;
  Num.Flags := 0;
  Num.Top := 0;
  Num.Neg := 0;
  Num.DMax := 0;
  Num.D := nil;
end;

procedure BigNumberFree(const Num: TCnBigNumber);
begin
  Num.Free;
end;

function BigNumberIsZero(const Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Num.Top = 0);
end;

function BigNumberSetZero(const Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 0);
end;

// 返回一个大数结构里的大数的绝对值是否为指定的 DWORD 值
function BigNumberAbsIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
begin
  Result := True;
  if (W = 0) and (Num.Top = 0) then
    Exit;
  if (Num.Top = 1) and (PLongWordArray(Num.D)^[0] = W) then
    Exit;
  Result := False;
end;

function BigNumberIsOne(const Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 0) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberIsNegOne(const Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Neg = 1) and BigNumberAbsIsWord(Num, 1);
end;

function BigNumberSetOne(const Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 1);
end;

function BigNumberIsOdd(const Num: TCnBigNumber): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (Num.Top > 0) and ((PLongWordArray(Num.D)^[0] and 1) <> 0) then
    Result := True
  else
    Result := False;
end;

function BigNumberGetWordBitsCount(L: LongWord): Integer;
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

function BigNumberGetBitsCount(const Num: TCnBigNumber): Integer;
var
  I: Integer;
begin
  Result := 0;
  if BigNumberIsZero(Num) then
    Exit;

  I := Num.Top - 1;
  Result := ((I * BN_BITS2) + BigNumberGetWordBitsCount(PLongWordArray(Num.D)^[I]));
end;

function BigNumberGetBytesCount(const Num: TCnBigNumber): Integer;
begin
  Result := (BigNumberGetBitsCount(Num) + 7) div 8;
end;

function BigNumberGetTenPrecision(const Num: TCnBigNumber): Integer;
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

  P := Trunc(LOG_10_2 * (B + 1)) + 1;
  Q := Trunc(LOG_10_2 * (B - 1));
  B := P - Q;
  // N 位二进制全 1 时，比它略大的 10 次幂，是 (N + 1) * Log10(2) 的整数部分 + 1，假设为 P
  // N 位二进制只有高位 1 时，比它略小的 10 次幂，是 (N - 1) * Log10(2) 的整数部分，假设为 Q
  // 也就是说，N 位二进制代表的值，其值必然在 10^Q 到 10^P 之间，P Q 最多差 2 或 3，只要比较两三次就行了
  // 但如何快速计算出 10^Q，还是得用二进制幂乘法 BigNumberPower

  N := ObtainBigNumberFromPool;
  try
    // 先计算 10 的 Q + 1 次幂（10 的 Q 次幂肯定比 Num 小，无需比）
    Inc(Q);
    Dec(B);
    N.SetWord(10);
    N.PowerWord(Q);

    Result := Q;
    while B >= 0 do // 计算到 P 为止
    begin
      if BignumberUnsignedCompare(Num, N) < 0 then
        Exit;

      Inc(Result);
      N.MulWord(10);
      Dec(B);
    end;
  finally
    RecycleBigNumberToPool(N);
  end;
end;

function BigNumberExpandInternal(const Num: TCnBigNumber; Words: Integer): PLongWord;
var
  A, B, TmpA: PLongWord;
  I: Integer;
  A0, A1, A2, A3: LongWord;
begin
  Result := nil;
  if Words > (MaxInt div (4 * BN_BITS2)) then
    Exit;

  if BigNumberGetFlag(Num, BN_FLG_STATIC_DATA) <> 0 then
    Exit;

  A := PLongWord(GetMemory(SizeOf(MAXDWORD) * Words));
  if A = nil then
    Exit;

  FillChar(A^, SizeOf(MAXDWORD) * Words, 0);

  // 查查是否要复制之前的值
  B := Num.D;
  if B <> nil then
  begin
    TmpA := A;
    I :=  Num.Top shr 2;
    while I > 0 do
    begin
      A0 := PLongWordArray(B)^[0];
      A1 := PLongWordArray(B)^[1];
      A2 := PLongWordArray(B)^[2];
      A3 := PLongWordArray(B)^[3];

      PLongWordArray(TmpA)^[0] := A0;
      PLongWordArray(TmpA)^[1] := A1;
      PLongWordArray(TmpA)^[2] := A2;
      PLongWordArray(TmpA)^[3] := A3;

      Dec(I);
      TmpA := PLongWord(Integer(TmpA) + 4 * SizeOf(LongWord));
      B := PLongWord(Integer(B) + 4 * SizeOf(LongWord));
    end;

    case Num.Top and 3 of
      3:
        begin
          PLongWordArray(TmpA)^[2] := PLongWordArray(B)^[2];
          PLongWordArray(TmpA)^[1] := PLongWordArray(B)^[1];
          PLongWordArray(TmpA)^[0] := PLongWordArray(B)^[0];
        end;
      2:
        begin
          PLongWordArray(TmpA)^[1] := PLongWordArray(B)^[1];
          PLongWordArray(TmpA)^[0] := PLongWordArray(B)^[0];
        end;
      1:
        begin
          PLongWordArray(TmpA)^[0] := PLongWordArray(B)^[0];
        end;
      0:
        begin
          ;
        end;
    end;
  end;

  Result := A;
end;

function BigNumberExpand2(const Num: TCnBigNumber; Words: Integer): TCnBigNumber;
var
  P: PLongWord;
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

function BigNumberWordExpand(const Num: TCnBigNumber; Words: Integer): TCnBigNumber;
begin
  if Words <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, Words);
end;

function BigNumberExpandBits(const Num: TCnBigNumber; Bits: Integer): TCnBigNumber;
begin
  if ((Bits + BN_BITS2 - 1) div BN_BITS2) <= Num.DMax then
    Result := Num
  else
    Result := BigNumberExpand2(Num, (Bits + BN_BITS2 - 1) div BN_BITS2);
end;

procedure BigNumberClear(const Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  if Num.D <> nil then
    FillChar(Num.D^, Num.DMax * SizeOf(LongWord), 0);
  Num.Top := 0;
  Num.Neg := 0;
end;

function BigNumberSetWord(const Num: TCnBigNumber; W: LongWord): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(LongWord) * 8) = nil then
    Exit;
  Num.Neg := 0;
  PLongWordArray(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 1
  else
    Num.Top := 0;
  Result := True;
end;

function BigNumberGetWord(const Num: TCnBigNumber): LongWord;
begin
  if Num.Top > 1 then
    Result := BN_MASK2
  else if Num.Top = 1 then
    Result := PLongWordArray(Num.D)^[0]
  else
    Result := 0;
end;

function BigNumberGetInt64(const Num: TCnBigNumber): Int64;
begin
  if Num.Top > 2 then
    Result := BN_MASK3S
  else if Num.Top = 2 then
  begin
    Result := PInt64Array(Num.D)^[0];
    if Result < 0 then      // Int64 high bit set
      Result := BN_MASK3S
    else if Num.Neg <> 0 then // 负则求反加一
      Result := (not Result) + 1;
  end
  else if Num.Top = 1 then
    Result := Int64(PLongWordArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetInt64(const Num: TCnBigNumber; W: Int64): Boolean;
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
      Num.Top := 2;
  end                 
  else // W < 0
  begin
    Num.Neg := 1;
    PInt64Array(Num.D)^[0] := (not W) + 1;
    Num.Top := 2;
  end;
  Result := True;
end;

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(const Num: TCnBigNumber): UInt64;
begin
  if Num.Top > 2 then
    Result := UInt64(BN_MASK3U)
  else if Num.Top = 2 then
    Result := PUInt64Array(Num.D)^[0]
  else if Num.Top = 1 then
    Result := UInt64(PLongWordArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetUInt64(const Num: TCnBigNumber; W: UInt64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(UInt64) * 8) = nil then
    Exit;

  Num.Neg := 0;
  PUInt64Array(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 2
  else
    Num.Top := 0;
  Result := True;
end;

{$ENDIF}

// 某大数是否等于指定 DWORD
function BigNumberIsWord(const Num: TCnBigNumber; W: LongWord): Boolean;
begin
  Result := False;
  if (W = 0) or (Num.Neg = 0) then
    if BigNumberAbsIsWord(Num, W) then
      Result := True;
end;

// 调整 Top 保证 D[Top - 1] 指向最高位非 0 处
procedure BigNumberCorrectTop(const Num: TCnBigNumber);
var
  Ftl: PLongWord;
  Top: Integer;
begin
  Top := Num.Top;
  Ftl := @(PLongWordArray(Num.D)^[Top - 1]);
  while Top > 0 do
  begin
    if Ftl^ <> 0 then
      Break;

    Ftl := PLongWord(Integer(Ftl) - SizeOf(LongWord));
    Dec(Top);
  end;
  Num.Top := Top;
end;

function BigNumberToBinary(const Num: TCnBigNumber; Buf: PAnsiChar): Integer;
var
  I, N: Integer;
  L: LongWord;
begin
  N := BigNumberGetBytesCount(Num);
  I := N;
  while I > 0 do
  begin
    Dec(I);
    L := PLongWordArray(Num.D)^[I div BN_BYTES];
    Buf^ := AnsiChar(Chr(L shr (8 * (I mod BN_BYTES)) and $FF));

    Buf := PAnsiChar(Integer(Buf) + 1);
  end;
  Result := N;
end;

function BigNumberWriteBinaryToStream(const Num: TCnBigNumber; Stream: TStream): Integer;
var
  Buf: array of Byte;
  Len: Integer;
begin
  Result := 0;
  Len := BigNumberGetBytesCount(Num);
  if (Stream <> nil) and (Len > 0) then
  begin
    SetLength(Buf, Len);
    BigNumberToBinary(Num, @Buf[0]);
    Stream.Write(Buf[0], Len);
    SetLength(Buf, 0);
  end;
end;

function BigNumberFromBinary(Buf: PAnsiChar; Len: Integer): TCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if not BigNumberSetBinary(Buf, Len, Result) then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberSetBinary(Buf: PAnsiChar; Len: Integer;
  const Res: TCnBigNumber): Boolean;
var
  I, M, N, L: LongWord;
begin
  Result := False;
  L := 0;
  N := Len;
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
    Buf := PAnsiChar(Integer(Buf) + 1);

    if M = 0 then
    begin
      Dec(I);
      PLongWordArray(Res.D)^[I] := L;
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

procedure BigNumberSetNegative(const Num: TCnBigNumber; Negative: Boolean);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Negative then
    Num.Neg := 1
  else
    Num.Neg := 0;
end;

function BigNumberIsNegative(const Num: TCnBigNumber): Boolean;
begin
  Result := Num.Neg <> 0;
end;

procedure BigNumberNegate(const Num: TCnBigNumber);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Num.Neg <> 0 then
    Num.Neg := 0
  else
    Num.Neg := 1;
end;

function BigNumberClearBit(const Num: TCnBigNumber; N: Integer): Boolean;
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

  PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] and LongWord(not (1 shl J));
  BigNumberCorrectTop(Num);
  Result := True;
end;

function BigNumberSetBit(const Num: TCnBigNumber; N: Integer): Boolean;
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
      PLongWordArray(Num.D)^[K] := 0;

    Num.Top := I + 1;
  end;

  PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] or LongWord(1 shl J);
  Result := True;
end;

function BigNumberIsBitSet(const Num: TCnBigNumber; N: Integer): Boolean;
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

  if (LongWord(PLongWordArray(Num.D)^[I] shr J) and LongWord(1)) <> 0 then
    Result := True;
end;

function BigNumberCompareWords(var Num1: TCnBigNumber; var Num2: TCnBigNumber;
  N: Integer): Integer;
var
  I: Integer;
  A, B: LongWord;
begin
  A := PLongWordArray(Num1.D)^[N - 1];
  B := PLongWordArray(Num2.D)^[N - 1];

  if A <> B then
  begin
    if A > B then
      Result := 1
    else
      Result := -1;
    Exit;
  end;

  for I := N - 2 downto 0 do
  begin
    A := PLongWordArray(Num1.D)^[I];
    B := PLongWordArray(Num2.D)^[I];

    if A <> B then
    begin
      if A > B then
        Result := 1
      else
        Result := -1;
      Exit;
    end;
  end;
  Result := 0;
end;

function BigNumberCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
var
  I, Gt, Lt: Integer;
  T1, T2: LongWord;
begin
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
    T1 := PLongWordArray(Num1.D)^[I];
    T2 := PLongWordArray(Num2.D)^[I];
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

function BigNumberUnsignedCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
var
  I: Integer;
  T1, T2: LongWord;
begin
  Result := Num1.Top - Num2.Top;
  if Result <> 0 then
    Exit;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PLongWordArray(Num1.D)^[I];
    T2 := PLongWordArray(Num2.D)^[I];
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
function BigNumberRandBytes(const Num: TCnBigNumber; BytesCount: Integer): Boolean;
begin
  Result := False;
  if BytesCount < 0 then
    Exit;
  if BytesCount = 0 then
  begin
    Result := BigNumberSetZero(Num);
    Exit;
  end;

  if BigNumberWordExpand(Num, (BytesCount + 3) div 4) <> nil then
  begin
    Result := CnRandomFillBytes(PAnsiChar(Num.D), BytesCount);
    if Result then
    begin
      Num.Top := (BytesCount + 3) div 4;
      BigNumberCorrectTop(Num);
    end;
  end;
end;

// 产生固定位长度的随机大数
function BigNumberRandBits(const Num: TCnBigNumber; BitsCount: Integer): Boolean;
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
    for I := C * 8 - 1 downto BitsCount do
      if not BigNumberClearBit(Num, I) then
        Exit;

  Result := True;
end;

function BigNumberRandRange(const Num: TCnBigNumber; const Range: TCnBigNumber): Boolean;
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

function BigNumberDuplicate(const Num: TCnBigNumber): TCnBigNumber;
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

function BigNumberCopy(const Dst: TCnBigNumber; const Src: TCnBigNumber): TCnBigNumber;
var
  I: Integer;
  A, B: PLongWordArray;
  A0, A1, A2, A3: LongWord;
begin
  if Dst = Src then
  begin
    Result := Dst;
    Exit;
  end;

  if BigNumberWordExpand(Dst, Src.Top) = nil then
  begin
    Result := nil;
    Exit;
  end;

  A := PLongWordArray(Dst.D);
  B := PLongWordArray(Src.D);

  for I := (Src.Top shr 2) downto 1 do
  begin
    A0 := B[0]; A1 := B[1]; A2 := B[2]; A3 := B[3];
    A[0] := A0; A[1] := A1; A[2] := A2; A[3] := A3;

    A := PLongWordArray(Integer(A) + 4 * SizeOf(LongWord));
    B := PLongWordArray(Integer(B) + 4 * SizeOf(LongWord));
  end;

  case Src.Top and 3 of
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

  Dst.Top := Src.Top;
  Dst.Neg := Src.Neg;
  Result := Dst;
end;

procedure BigNumberSwap(const Num1: TCnBigNumber; const Num2: TCnBigNumber);
var
  OldFlag1, OldFlag2: LongWord;
  TmpD: PLongWord;
  TmpTop, TmpDMax, TmpNeg: Integer;
begin
  OldFlag1 := Num1.Flags;
  OldFlag2 := Num2.Flags;

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

  // 数据区的属性交换
  Num1.Flags := (OldFlag1 and BN_FLG_MALLOCED) or (OldFlag2 and BN_FLG_STATIC_DATA);
  Num2.Flags := (OldFlag2 and BN_FLG_MALLOCED) or (OldFlag1 and BN_FLG_STATIC_DATA);
end;

// ============================ 低阶运算定义开始 ===============================

// UInt64 的方式计算 N 平方
procedure Sqr(var L: LongWord; var H: LongWord; N: LongWord); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(N, N);
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  L := LongWord(T) and BN_MASK2;
  H := LongWord(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 的方式计算 A * B + R + C
procedure MulAdd(var R: LongWord; A: LongWord; B: LongWord; var C: LongWord); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + R + C;
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  R := LongWord(T) and BN_MASK2;
  C := LongWord(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 的方式计算 A * B + C
procedure Mul(var R: LongWord; A: LongWord; B: LongWord; var C: LongWord); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + C;
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  R := LongWord(T) and BN_MASK2;
  C := LongWord(T shr BN_BITS2) and BN_MASK2;
end;

// ============================ 低阶运算定义结束 ===============================

{* Words 系列内部计算函数开始}

function BigNumberAddWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer): LongWord;
var
  LL: Int64;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  LL := 0;
  while (N and (not 3)) <> 0 do
  begin
    LL := LL + Int64(AP[0]) + Int64(BP[0]);
    RP[0] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[1]) + Int64(BP[1]);
    RP[1] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[2]) + Int64(BP[2]);
    RP[2] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[3]) + Int64(BP[3]);
    RP[3] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    LL := LL + Int64(AP[0]) + Int64(BP[0]);
    RP[0] := LongWord(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
    Dec(N);
  end;
  Result := LongWord(LL);
end;

function BigNumberSubWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer): LongWord;
var
  T1, T2, C: LongWord;
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

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    BP := PLongWordArray(Integer(BP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
    Dec(N);
  end;
  Result := C;
end;

function BigNumberMulAddWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer; W: LongWord): LongWord;
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

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
    Dec(N);
  end;
end;

// AP 指向的 N 个数字都乘以 W，结果的低 N 位放 RP 中，高位放返回值
function BigNumberMulWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer; W: LongWord): LongWord;
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

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(LongWord));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);

    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));

    Dec(N);
  end;
end;

procedure BigNumberSqrWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer);
begin
  if N = 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    Sqr(RP^[2], RP^[3], AP^[1]);
    Sqr(RP^[4], RP^[5], AP^[2]);
    Sqr(RP^[6], RP^[7], AP^[3]);

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 8 * SizeOf(LongWord));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(LongWord));
    Dec(N);
  end;
end;

// 64 位被除数整除 32 位除数，返回商，Result := H L div D，不管商的高 32 位
// 因此要保证 D 的最高位为 1，商的高 32 位才会为 0，此函数调用才不会出错，所以 32 位下才可以用 DIV 指令优化
function InternalDivWords(H: LongWord; L: LongWord; D: LongWord): LongWord;
begin
  if D = 0 then
  begin
    Result := BN_MASK2;
    Exit;
  end;

{$IFDEF WIN64}
  Result := LongWord(((UInt64(H) shl 32) or UInt64(L)) div UInt64(D));
{$ELSE}
  Result := 0;
  asm
    MOV EAX, L
    MOV EDX, H
    DIV ECX       // DIV 貌似等于 DIVL，这段优化比下面调 _lludiv 的耗时少了 20%
    MOV Result, EAX
  end;
//  asm
//    PUSH 0
//    PUSH D
//    MOV EAX, L
//    MOV EDX, H
//    CALL System.@_lludiv;
//    // Delphi 自身汇编实现的 64 位无符号除法函数，入参要求
//    // Dividend(EAX(lo):EDX(hi)), Divisor([ESP+8](hi):[ESP+4](lo))
//    MOV Result, EAX
//  end;
{$ENDIF}
end;

{*  Words 系列内部计算函数结束}

function BigNumberUnsignedAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PLongWord;
  Carry, T1, T2: LongWord;
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
  AP := PLongWord(A.D);
  BP := PLongWord(B.D);
  RP := PLongWord(Res.D);

  Carry := BigNumberAddWords(PLongWordArray(RP), PLongWordArray(AP), PLongWordArray(BP), Min);

  AP := PLongWord(Integer(AP) + Min * SizeOf(LongWord));
  RP := PLongWord(Integer(RP) + Min * SizeOf(LongWord));

  if Carry <> 0 then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      T2 := (T1 + 1) and BN_MASK2;

      RP^ := T2;
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

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
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));
    end;
  end;

  Res.Neg := 0;
  Result := True;
end;

function BigNumberUnsignedSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif, I: Integer;
  AP, BP, RP: PLongWord;
  Carry, T1, T2: LongWord;
begin
  Result := False;

  Max := Num1.Top;
  Min := Num2.Top;
  Dif := Max - Min;

  if Dif < 0 then
    Exit;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  AP := PLongWord(Num1.D);
  BP := PLongWord(Num2.D);
  RP := PLongWord(Res.D);

  Carry := 0;
  for I := Min downto 1 do
  begin
    T1 := AP^;
    T2 := BP^;
    AP := PLongWord(Integer(AP) + SizeOf(LongWord));
    BP := PLongWord(Integer(BP) + SizeOf(LongWord));
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
    RP := PLongWord(Integer(RP) + SizeOf(LongWord));
  end;

  if Carry <> 0 then
  begin
    if Dif = 0 then  // Error! Num1 < Num2
      Exit;

    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      T2 := (T1 - 1) and BN_MASK2;

      RP^ := T2;
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));
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
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(LongWord));
      RP := PLongWord(Integer(RP) + SizeOf(LongWord));
    end;
  end;

  Res.Top := Max;
  Res.Neg := 0;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
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

function BigNumberSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
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

function BigNumberShiftLeftOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
var
  RP, AP: PLongWord;
  I: Integer;
  T, C: LongWord;
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
    AP := PLongWord(Integer(AP) + SizeOf(LongWord));
    RP^ := ((T shl 1) or C) and BN_MASK2;
    RP := PLongWord(Integer(RP) + SizeOf(LongWord));

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

function BigNumberShiftRightOne(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
var
  RP, AP: PLongWord;
  I, J: Integer;
  T, C: LongWord;
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

  if PLongWordArray(AP)^[I - 1] = 1 then
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
  T := PLongWordArray(AP)^[I];

  if (T and 1) <> 0 then
    C := BN_TBIT
  else
    C := 0;

  T := T shr 1;
  if T <> 0 then
    PLongWordArray(RP)^[I] := T;

  while I > 0 do
  begin
    Dec(I);
    T := PLongWordArray(AP)^[I];
    PLongWordArray(RP)^[I] := ((T shr 1) and BN_MASK2) or C;

    if (T and 1) <> 0 then
      C := BN_TBIT
    else
      C := 0;
  end;

  Res.Top := J;
  Result := True;
end;

function BigNumberShiftLeft(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, NW, LB, RB: Integer;
  L: LongWord;
  T, F: PLongWordArray;
begin
  Result := False;
  Res.Neg := Num.Neg;
  NW := N div BN_BITS2;

  if BigNumberWordExpand(Res, Num.Top + NW + 1) = nil then
    Exit;

  LB := N mod BN_BITS2;
  RB := BN_BITS2 - LB;

  F := PLongWordArray(Num.D);
  T := PLongWordArray(Res.D);

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
      L := F[I];
      T^[NW + I + 1] := T^[NW + I + 1] or ((L shr RB) and BN_MASK2);
      T^[NW + I] := (L shl LB) and BN_MASK2;
    end;
  end;

  FillChar(Pointer(T)^, NW * SizeOf(LongWord), 0);
  Res.Top := Num.Top + NW + 1;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberShiftRight(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, J, NW, LB, RB: Integer;
  L, Tmp: LongWord;
  T, F: PLongWordArray;
begin
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

  F := PLongWordArray(Integer(Num.D) + NW * SizeOf(LongWord));
  T := PLongWordArray(Res.D);
  J := Num.Top - NW;
  Res.Top := I;

  if RB = 0 then
  begin
    for I := J downto 1 do
    begin
      T^[0] := F^[0];
      F := PLongWordArray(Integer(F) + SizeOf(LongWord));
      T := PLongWordArray(Integer(T) + SizeOf(LongWord));
    end;
  end
  else
  begin
    L := F^[0];
    F := PLongWordArray(Integer(F) + SizeOf(LongWord));
    for I := J - 1 downto 1 do
    begin
      Tmp := (L shr RB) and BN_MASK2;
      L := F^[0];
      T^[0] := (Tmp or (L shl LB)) and BN_MASK2;

      F := PLongWordArray(Integer(F) + SizeOf(LongWord));
      T := PLongWordArray(Integer(T) + SizeOf(LongWord));
    end;

    L := (L shr RB) and BN_MASK2;
    if L <> 0 then
      T^[0] := L;
  end;
  Result := True;
end;

{* 大数与 Word 运算系列函数开始}

function BigNumberAddWord(const Num: TCnBigNumber; W: LongWord): Boolean;
var
  I: Integer;
  L: LongWord;
begin
  Result := False;
  W := W and BN_MASK2;

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
    L := (PLongWordArray(Num.D)^[I] + W) and BN_MASK2;
    PLongWordArray(Num.D)^[I] := L;
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
    PLongWordArray(Num.D)^[I] := W;
  end;
  Result := True;
end;

function BigNumberSubWord(const Num: TCnBigNumber; W: LongWord): Boolean;
var
  I: Integer;
begin
  W := W and BN_MASK2;

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

  if (Num.Top = 1) and (PLongWordArray(Num.D)^[0] < W) then // 不够减
  begin
    PLongWordArray(Num.D)^[0] := W - PLongWordArray(Num.D)^[0];
    Num.Neg := 1;
    Result := True;
    Exit;
  end;

  I := 0;
  while True do
  begin
    if PLongWordArray(Num.D)^[I] >= W then // 够减直接减
    begin
      PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] - W;
      Break;
    end
    else
    begin
      PLongWordArray(Num.D)^[I] := (PLongWordArray(Num.D)^[I] - W) and BN_MASK2;
      Inc(I);
      W := 1;  // 不够减有借位
    end;
  end;

  if (PLongWordArray(Num.D)^[I] = 0) and (I = Num.Top - 1) then
    Dec(Num.Top);
  Result := True;
end;

function BigNumberMulWord(const Num: TCnBigNumber; W: LongWord): Boolean;
var
  L: LongWord;
begin
  Result := False;
  W := W and BN_MASK2;
  if Num.Top <> 0 then
  begin
    if W = 0 then
      BigNumberSetZero(Num)
    else
    begin
      L := BigNumberMulWords(PLongWordArray(Num.D), PLongWordArray(Num.D), Num.Top, W);
      if L <> 0 then
      begin
        if BigNumberWordExpand(Num, Num.Top + 1) = nil then
          Exit;
        PLongWordArray(Num.D)^[Num.Top] := L;
        Inc(Num.Top);
      end;
    end;
  end;
  Result := True;
end;

function BigNumberModWord(const Num: TCnBigNumber; W: LongWord): LongWord;
var
  I: Integer;
begin
  if W = 0 then
  begin
    Result := LongWord(-1);
    Exit;
  end;

  Result := 0;
  W := W and BN_MASK2;
  for I := Num.Top - 1 downto 0 do
  begin
    Result := ((Result shl BN_BITS4) or ((PLongWordArray(Num.D)^[I] shr BN_BITS4) and BN_MASK2l)) mod W;
    Result := ((Result shl BN_BITS4) or (PLongWordArray(Num.D)^[I] and BN_MASK2l)) mod W;
  end;
end;

function BigNumberDivWord(const Num: TCnBigNumber; W: LongWord): LongWord;
var
  I, J: Integer;
  L, D: LongWord;
begin
  W := W and BN_MASK2;
  if W = 0 then
  begin
    Result := LongWord(-1);
    Exit;
  end;

  Result := 0;
  if Num.Top = 0 then
    Exit;

  J := BN_BITS2 - BigNumberGetWordBitsCount(W);

  W := W shl J; // 保证 W 最高位为 1
  if not BigNumberShiftLeft(Num, Num, J) then
  begin
    Result := LongWord(-1);
    Exit;
  end;

  for I := Num.Top - 1 downto 0 do
  begin
    L := PLongWordArray(Num.D)^[I];
    D := InternalDivWords(Result, L, W); // W 保证了最高位为 1，结果才是 32 位
    Result := (L - ((D * W) and BN_MASK2)) and BN_MASK2;

    PLongWordArray(Num.D)^[I] := D;
  end;

  if (Num.Top > 0) and (PLongWordArray(Num.D)^[Num.Top - 1] = 0) then
    Dec(Num.Top);
  Result := Result shr J;
end;

{* 大数与 Word 运算系列函数结束}

function BigNumberToString(const Num: TCnBigNumber): string;
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
      V := ((PLongWordArray(Num.D)^[I]) shr LongWord(J)) and $0F;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[V + 1];
        Z := 1;
      end;
      Dec(J, 4);
    end;
  end;
end;

function BigNumberToHex(const Num: TCnBigNumber): string;
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
    J := BN_BITS2 - 8;
    while J >= 0 do
    begin
      V := ((PLongWordArray(Num.D)^[I]) shr LongWord(J)) and $FF;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[(V shr 4) + 1];
        Result := Result + Hex[(V and $0F) + 1];
        Z := 1;
      end;
      Dec(J, 8);
    end;
  end;
end;

function BigNumberSetHex(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, H, M, J, I, K, C: Integer;
  L: LongWord;
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
  while PAnsiChar(Integer(P) + I)^ in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberWordExpand(Res, I * 4) = nil then
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
      C := Ord(PAnsiChar(Integer(P) + J - M)^);
      if (C >= Ord('0')) and (C <= Ord('9')) then
        K := C - Ord('0')
      else if (C >= Ord('a')) and (C <= Ord('f')) then
        K := C - Ord('a') + 10
      else if (C >= Ord('A')) and (C <= Ord('F')) then
        K := C - Ord('A') + 10
      else
        K := 0;

      L := (L shl 4) or LongWord(K);

      Dec(M);
      if M <= 0 then
      begin
        PLongWordArray(Res.D)^[H] := L;
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

function BigNumberToDec(const Num: TCnBigNumber): AnsiString;
var
  I, N, R, Len: Integer;
  BnData, LP: PLongWord;
  T: TCnBigNumber;
  P: PAnsiChar;

  function BufRemain(Nu: Integer; Pt: PAnsiChar; Res: PAnsiChar): Integer;
  begin
    Result := Nu + 3 - (Integer(Pt) - Integer(Res));
  end;

begin
  Result := '';

  I := BigNumberGetBitsCount(Num) * 3;
  N := ((I div 10) + (I div 1000) + 1) + 1;

  BnData := nil;
  T := nil;
  try
    BnData := PLongWord(GetMemory(((N div 9) + 1) * SizeOf(LongWord)));
    if BnData = nil then
      Exit;

    SetLength(Result, N + 3);
    FillChar(Result[1], Length(Result), 0);

    T := ObtainBigNumberFromPool;
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
        LP := PLongWord(Integer(LP) + SizeOf(LongWord));
      end;
      LP := PLongWord(Integer(LP) - SizeOf(LongWord));

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
        LP := PLongWord(Integer(LP) - SizeOf(LongWord));
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
    if T <> nil then
      RecycleBigNumberToPool(T);
  end;

  Len := SysUtils.StrLen(PAnsiChar(Result));
  if Len >= 0 then
    SetLength(Result, Len); // 去除尾部多余的 #0
end;

function BigNumberSetDec(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, J, I: Integer;
  L: LongWord;
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
  while PAnsiChar(Integer(P) + I)^ in ['0'..'9'] do
    Inc(I);

  BigNumberSetZero(Res);

  if BigNumberWordExpand(Res, I * 4) = nil then
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

// Tmp should have 2 * N DWORDs
procedure BigNumberSqrNormal(R: PLongWord; A: PLongWord; N: Integer; Tmp: PLongWord);
var
  I, J, Max: Integer;
  AP, RP: PLongWordArray;
begin
  Max := N * 2;
  AP := PLongWordArray(A);
  RP := PLongWordArray(R);
  RP[0] := 0;
  RP[Max - 1] := 0;

  RP := PLongWordArray(Integer(RP) + SizeOf(LongWord));
  J := N - 1;

  if J > 0 then
  begin
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP[J] := BigNumberMulWords(RP, AP, J, PLongWordArray(Integer(AP) - SizeOf(LongWord))^[0]);
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(LongWord));
  end;

  for I := N - 2 downto 1 do
  begin
    Dec(J);
    AP := PLongWordArray(Integer(AP) + SizeOf(LongWord));
    RP[J] := BigNumberMulAddWords(RP, AP, J, PLongWordArray(Integer(AP) - SizeOf(LongWord))^[0]);
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(LongWord));
  end;

  BigNumberAddWords(PLongWordArray(R), PLongWordArray(R), PLongWordArray(R), Max);
  BigNumberSqrWords(PLongWordArray(Tmp), PLongWordArray(A), N);
  BigNumberAddWords(PLongWordArray(R), PLongWordArray(R), PLongWordArray(Tmp), Max);
end;

function BigNumberSqr(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
var
  Max, AL: Integer;
  Tmp, RR: TCnBigNumber;
  T: array[0..15] of LongWord;
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
      RR := ObtainBigNumberFromPool;
      IsFromPool := True;
    end;

    Tmp := ObtainBigNumberFromPool;
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
    if PLongWordArray(Num.D)^[AL - 1] = (PLongWordArray(Num.D)^[AL - 1] and BN_MASK2l) then
      RR.Top := Max - 1
    else
      RR.Top := Max;

    if RR <> Res then
      BigNumberCopy(Res, RR);
    Result := True;
  finally
    if IsFromPool then
      RecycleBigNumberToPool(RR);
    RecycleBigNumberToPool(Tmp);
  end;
end;

procedure BigNumberMulNormal(R: PLongWord; A: PLongWord; NA: Integer; B: PLongWord;
  NB: Integer);
var
  RR: PLongWord;
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

  RR := PLongWord(Integer(R) + NA * SizeOf(LongWord));
  if NB <= 0 then
  begin
    BigNumberMulWords(PLongWordArray(R), PLongWordArray(A), NA, 0);
    Exit;
  end
  else
    RR^ := BigNumberMulWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

  while True do
  begin
    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));

    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(LongWord));
    R := PLongWord(Integer(R) + SizeOf(LongWord));
    B := PLongWord(Integer(B) + SizeOf(LongWord));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);
  end;
end;

function BigNumberMul(const Res: TCnBigNumber; Num1: TCnBigNumber;
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
  Top := AL + BL;

  RR := nil;
  IsFromPool := False;

  try
    if (Res = Num1) or (Res = Num2) then
    begin
      RR := ObtainBigNumberFromPool;
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
      RecycleBigNumberToPool(RR);
  end;
end;

function BigNumberDiv(const Res: TCnBigNumber; const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
var
  Tmp, SNum, SDiv, SRes: TCnBigNumber;
  I, NormShift, Loop, NumN, DivN, Neg, BackupTop, BackupDMax, BackupNeg: Integer;
  D0, D1, Q, L0, N0, N1, Rem, T2L, T2H: LongWord;
  Resp, WNump, BackupD: PLongWord;
  WNum: TCnBigNumber;
  T2: TUInt64;
begin
  Result := False;
  if (Num.Top > 0) and (PLongWordArray(Num.D)^[Num.Top - 1] = 0) then
    Exit;

  if BigNumberIsZero(Divisor) then
    Exit;

  if BigNumberUnsignedCompare(Num, Divisor) < 0 then
  begin
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
    Tmp := ObtainBigNumberFromPool;
    SNum := ObtainBigNumberFromPool;
    SDiv := ObtainBigNumberFromPool;
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

    WNum := ObtainBigNumberFromPool;
    BackupNeg := WNum.Neg;
    BackupD := WNum.D;
    BackupTop := WNum.Top;
    BackupDMax := WNum.DMax;

    // 注意 WNum 需要使用外部的 D，把池子里拿出来的东西先备份
    WNum.Neg := 0;
    WNum.D := PLongWord(Integer(SNum.D) + Loop * SizeOf(LongWord));
    WNum.Top := DivN;
    WNum.DMax := SNum.DMax - Loop;

    D0 := PLongWordArray(SDiv.D)^[DivN - 1];
    if DivN = 1 then
      D1 := 0
    else
      D1 := PLongWordArray(SDiv.D)^[DivN - 2];
    // D0 D1 是 SDiv 的最高俩 DWORD

    WNump := PLongWord(Integer(SNum.D) + (NumN - 1) * SizeOf(LongWord));

    if Num.Neg <> Divisor.Neg then
      SRes.Neg := 1
    else
      SRes.Neg := 0;

    if BigNumberWordExpand(SRes, Loop + 1) = nil then
      Exit;

    SRes.Top := Loop;
    Resp := PLongWord(Integer(SRes.D) + (Loop - 1) * SizeOf(LongWord));

    if BigNumberWordExpand(Tmp, DivN + 1) = nil then
      Exit;

    if BigNumberUnsignedCompare(WNum, SDiv) >= 0 then
    begin
      BigNumberSubWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
        PLongWordArray(SDiv.D), DivN);
      Resp^ := 1;
    end
    else
      Dec(SRes.Top);

    if SRes.Top = 0 then
      SRes.Neg := 0
    else
      Resp := PLongWord(Integer(Resp) - SizeOf(LongWord));

    for I := 0 to Loop - 2 do
    begin
//    Rem := 0;
      // 用 N0/N1/D0/D1 计算出一个 Q 使 | WNum - SDiv * Q | < SDiv
      N0 := WNump^;
      N1 := (PLongWord(Integer(WNump) - SizeOf(LongWord)))^;

      if N0 = D0 then
        Q := BN_MASK2
      else
      begin
        Q := InternalDivWords(N0, N1, D0); // D0 已由上文保证最高位是 1
        Rem := (N1 - Q * D0) and BN_MASK2;

        T2 := UInt64Mul(D1, Q);
        T2H := (T2 shr 32) and BN_MASK2;
        T2L := T2 and BN_MASK2;

        while True do
        begin
          if (T2H < Rem) or ((T2H = Rem) and
             (T2L <= (PLongWord(Integer(WNump) - 2 * SizeOf(LongWord)))^)) then
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

      L0 := BigNumberMulWords(PLongWordArray(Tmp.D), PLongWordArray(SDiv.D), DivN, Q);
      PLongWordArray(Tmp.D)^[DivN] := L0;
      WNum.D := PLongWord(Integer(WNum.D) - SizeOf(LongWord));

      if BigNumberSubWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
        PLongWordArray(Tmp.D), DivN + 1) <> 0 then
      begin
        Dec(Q);
        if BigNumberAddWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
          PLongWordArray(SDiv.D), DivN) <> 0 then
          WNump^ := WNump^ + 1;
      end;

      Resp^ := Q;
      WNump := PLongWord(Integer(WNump) - SizeOf(LongWord));
      Resp := PLongWord(Integer(Resp) - SizeOf(LongWord));
    end;

    BigNumberCorrectTop(SNum);
    Neg := Num.Neg;
    BigNumberShiftRight(Remain, SNum, NormShift);
    if not BigNumberIsZero(Remain) then
      Remain.Neg := Neg;

    Result := True;
  finally
    RecycleBigNumberToPool(Tmp);
    RecycleBigNumberToPool(SNum);
    RecycleBigNumberToPool(SDiv);
    // 恢复 WNum 内容并扔回池子里
    WNum.Neg := BackupNeg;
    WNum.D := BackupD;
    WNum.Top := BackupTop;
    WNum.DMax := BackupDMax;
    RecycleBigNumberToPool(WNum);
  end;
end;

function BigNumberMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
var
  Res: TCnBigNumber;
begin
  Res := ObtainBigNumberFromPool;
  try
    Result := BigNumberDiv(Res, Remain, Num, Divisor);
  finally
    RecycleBigNumberToPool(Res);
  end;
end;

function BigNumberNonNegativeMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
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

function BigNumberPower(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: LongWord): Boolean;
var
  T: TCnBigNumber;
begin
  Result := False;
  if Exponent = 0 then
  begin
    if Num.IsZero then  // 0 无 0 次方
      Exit;

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

  T := ObtainBigNumberFromPool;
  BigNumberCopy(T, Num);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        BigNumberMul(Res, Res, T);

      Exponent := Exponent shr 1;
      BigNumberMul(T, T, T);
    end;
    Result := True;
  finally
    RecycleBigNumberToPool(T);
  end;
end;

function BigNumberExp(const Res: TCnBigNumber; const Num: TCnBigNumber;
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
      RR := ObtainBigNumberFromPool;
      IsFromPool := True;
    end
    else
      RR := Res;

    V := ObtainBigNumberFromPool;
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
      RecycleBigNumberToPool(RR);
    RecycleBigNumberToPool(V);
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

function BigNumberGcd(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  T, A, B: TCnBigNumber;
begin
  Result := False;

  A := nil;
  B := nil;

  try
    A := ObtainBigNumberFromPool;
    B := ObtainBigNumberFromPool;
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
    RecycleBigNumberToPool(A);
    RecycleBigNumberToPool(B);
  end;
end;

function BigNumberLcm(const Res: TCnBigNumber; Num1: TCnBigNumber;
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
    G := ObtainBigNumberFromPool;
    M := ObtainBigNumberFromPool;
    R := ObtainBigNumberFromPool;

    if not BigNumberGcd(G, Num1, Num2) then
      Exit;

    if not BigNumberMul(M, Num1, Num2) then
      Exit;

    if not BigNumberDiv(Res, R, M, G) then
      Exit;

    Result := True;
  finally
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(M);
    RecycleBigNumberToPool(G);
  end;
end;

// 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话}
function BigNumberMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
var
  T, P: TCnBigNumber;
begin
  if not BigNumberIsNegative(A) and not BigNumberIsNegative(B) then
    Result := BigNumberUnsignedMulMod(Res, A, B, C)
  else if BigNumberIsNegative(A) and BigNumberIsNegative(B) then
  begin
    T := ObtainBigNumberFromPool;
    P := ObtainBigNumberFromPool;
    try
      BigNumberCopy(T, A);
      BigNumberCopy(P, B);
      BigNumberSetNegative(T, False);
      BigNumberSetNegative(P, False);
      Result := BigNumberUnsignedMulMod(Res, T, P, C);
    finally
      RecycleBigNumberToPool(T);
      RecycleBigNumberToPool(P);
    end;
  end
  else if BigNumberIsNegative(A) and not BigNumberIsNegative(B) then // A 负
  begin
    T := ObtainBigNumberFromPool;
    try
      BigNumberCopy(T, A);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, T, B, C);
      BigNumberSub(Res, C, Res);
    finally
      RecycleBigNumberToPool(T);
    end;
  end
  else if not BigNumberIsNegative(A) and BigNumberIsNegative(B) then // B 负
  begin
    T := ObtainBigNumberFromPool;
    try
      BigNumberCopy(T, B);
      BigNumberSetNegative(T, False);
      Result := BigNumberUnsignedMulMod(Res, A, T, C);
      BigNumberSub(Res, C, Res);
    finally
      RecycleBigNumberToPool(T);
    end;
  end
  else
    Result := False;
end;

// 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话}
function BigNumberUnsignedMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
var
  AA, BB: TCnBigNumber;
begin
  Result := False;
  AA := nil;
  BB := nil;

  try
    // 使用临时变量，保证 A、B 自身的值不发生变化
    AA := ObtainBigNumberFromPool;
    BB := ObtainBigNumberFromPool;

    BigNumberCopy(AA, A);
    BigNumberCopy(BB, B);
    BigNumberSetNegative(AA, False); // 全正处理
    BigNumberSetNegative(BB, False);

    if not BigNumberMod(AA, AA, C) then
      Exit;

    if not BigNumberMod(BB, BB, C) then
      Exit;

    Res.SetZero; // 如果 Res 是 A 或 B，后面参与运算的是 AA 或 BB，改变 A 或 B不影响

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
    RecycleBigNumberToPool(AA);
    RecycleBigNumberToPool(BB);
  end;
  Result := True;
end;

{* 普通计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）}
function BigNumberDirectMulMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
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

  if not BigNumberNonNegativeMod(Res, Res, C) then
    Exit;
  Result := True;
end;

// 快速计算 (A ^ B) mod C，返回计算是否成功，Res 不能是 A、B、C 之一
function BigNumberPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
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
    Val[0] := ObtainBigNumberFromPool;
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
    D := ObtainBigNumberFromPool;
    if Window > 1 then
    begin
      if not BigNumberDirectMulMod(D, Val[0], Val[0], C) then
        Exit;

      J := 1 shl (Window - 1);
      for I := 1 to J - 1 do
      begin
        Val[I] := ObtainBigNumberFromPool;
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
    RecycleBigNumberToPool(D);
    for I := Low(Val) to High(Val) do
      RecycleBigNumberToPool(Val[I]);
  end;
end;

// 蒙哥马利法快速计算 (A ^ B) mod C，，返回计算是否成功，Res 不能是 A、B、C 之一
function BigNumberMontgomeryPowerMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
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

  AA := nil;
  BB := nil;
  T := nil;

  try
    AA := ObtainBigNumberFromPool;
    BB := ObtainBigNumberFromPool;
    T := ObtainBigNumberFromPool;

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
        if not BigNumberMulMod(T, AA, T, C) then
          Exit;
      end;
      if not BigNumberMulMod(AA, AA, AA, C) then
        Exit;

      if not BigNumberShiftRightOne(BB, BB) then
        Exit;
    end;

    if not BigNumberMulMod(Res, AA, T, C) then
      Exit;
  finally
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(AA);
    RecycleBigNumberToPool(BB);
  end;
  Result := True;
end;

function BigNumberFermatCheckComposite(const A, B, C: TCnBigNumber; T: Integer): Boolean;
var
  I: Integer;
  R, L, S: TCnBigNumber;
begin
  Result := False;

  R := nil;
  L := nil;
  S := nil;

  try
    R := ObtainBigNumberFromPool;
    if not BigNumberPowerMod(R, A, C, B) then
      Exit;

    L := ObtainBigNumberFromPool;
    if BigNumberCopy(L, R) = nil then // L := R;
      Exit;

    S := ObtainBigNumberFromPool;
    for I := 1 to T do
    begin
      if not BigNumberMulMod(R, R, R, B) then
        Exit;

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
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(L);
    RecycleBigNumberToPool(S);
  end;
end;

// TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢
function BigNumberIsProbablyPrime(const Num: TCnBigNumber; TestCount: Integer): Boolean;
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
  X := ObtainBigNumberFromPool;
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
    RecycleBigNumberToPool(X);
  end;

  // 再用小额素数整除，不用 2 了，因为 2 之外的偶数已经被排除了
  for I := Low(CN_PRIME_NUMBERS_SQRT_UINT32) + 1 to High(CN_PRIME_NUMBERS_SQRT_UINT32) do
  begin
    if BigNumberModWord(Num, CN_PRIME_NUMBERS_SQRT_UINT32[I]) = 0 then
      Exit;
  end;

  // 都漏网了，再做 Miller-Rabin Test
  X := nil;
  R := nil;
  W := nil;

  try
    X := ObtainBigNumberFromPool;
    R := ObtainBigNumberFromPool;
    W := ObtainBigNumberFromPool;

    if BigNumberCopy(X, Num) = nil then
      Exit;

    if not BigNumberSubWord(X, 1) then
      Exit;

    if BigNumberCopy(W, X) = nil then  // W := Num - 1;
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
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(W);
  end;
  Result := True;
end;

function InternalGenerateProbablePrime(const Num: TCnBigNumber; BitsCount: Integer): Boolean;
var
  Mods: array[0..BN_PRIME_NUMBERS - 1] of Cardinal;
  Delta, MaxDelta: Cardinal;
  I: Integer;
label
  AGAIN;
begin
  Result := False;

AGAIN:
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

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
function BigNumberGeneratePrime(const Num: TCnBigNumber; BytesCount: Integer;
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
function BigNumberGeneratePrimeByBitsCount(const Num: TCnBigNumber; BitsCount: Integer;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
begin
  Result := False;
  if not BigNumberRandBits(Num, BitsCount) then
    Exit;

  if not BigNumberSetBit(Num, BitsCount - 1) then
    Exit;

  if not Num.IsOdd then
    Num.AddWord(1);

  while not BigNumberIsProbablyPrime(Num, TestCount) do
  begin
    Num.AddWord(2);
//    if not BigNumberRandBits(Num, BitsCount) then
//      Exit;
//    if not BigNumberSetBit(Num, BitsCount - 1) then
//      Exit;
//
//    if not Num.IsOdd then
//      Num.AddWord(1);
  end;
  Result := True;
end;

// 查 R 是否对于 Prime - 1 的每个因子，都有 R ^ (剩余因子的积) mod Prime <> 1
function BigNumberCheckPrimitiveRoot(R, Prime: TCnBigNumber; Factors: TCnBigNumberList): Boolean;
var
  I: Integer;
  Res, SubOne, T, Remain: TCnBigNumber;
begin
  Result := False;
  Res := ObtainBigNumberFromPool;
  T := ObtainBigNumberFromPool;
  Remain := ObtainBigNumberFromPool;
  SubOne := ObtainBigNumberFromPool;

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
    RecycleBigNumberToPool(Res);
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(Remain);
    RecycleBigNumberToPool(SubOne);
  end;
end;

// 大数是否是一个 32 位有符号整型范围内的数
function BigNumberIsInt32(const Num: TCnBigNumber): Boolean;
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
function BigNumberIsUInt32(const Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_32);
end;

// 大数是否是一个 64 位有符号整型范围内的数
function BigNumberIsInt64(const Num: TCnBigNumber): Boolean;
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
function BigNumberIsUInt64(const Num: TCnBigNumber): Boolean;
begin
  Result := not Num.IsNegative and (Num.GetBitsCount <= BN_BITS_UINT_64);
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
      T := ObtainBigNumberFromPool;
      P := ObtainBigNumberFromPool;
      M := ObtainBigNumberFromPool;
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
      RecycleBigNumberToPool(M);
      RecycleBigNumberToPool(P);
      RecycleBigNumberToPool(T);
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
      T := ObtainBigNumberFromPool;
      P := ObtainBigNumberFromPool;
      M := ObtainBigNumberFromPool;
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
      RecycleBigNumberToPool(M);
      RecycleBigNumberToPool(P);
      RecycleBigNumberToPool(T);
    end;
  end;
end;

// 求 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。调用者须自行保证 X、Modulus 互质
procedure BigNumberModularInverse(const Res: TCnBigNumber; X, Modulus: TCnBigNumber);
var
  Neg: Boolean;
  X1, Y: TCnBigNumber;
begin
  Neg := False;

  X1 := nil;
  Y := nil;

  try
    X1 := ObtainBigNumberFromPool;
    Y := ObtainBigNumberFromPool;

    BigNumberCopy(X1, X);
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
      BigNumberAdd(Res, Res, Modulus);
  finally
    RecycleBigNumberToPool(X1);
    RecycleBigNumberToPool(Y);
  end;
end;

// 用二次互反律递归计算勒让德符号 ( A / P) 的值，较快
function BigNumberLegendre(A, P: TCnBigNumber): Integer;
var
  AA, Q: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise Exception.Create('A, P Must > 0');

  if A.IsOne then
  begin
    Result := 1;
    Exit;
  end;

  AA := ObtainBigNumberFromPool;
  Q := ObtainBigNumberFromPool;

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
    RecycleBigNumberToPool(Q);
    RecycleBigNumberToPool(AA);
  end;
end;

// 用欧拉判别法计算勒让德符号 ( A / P) 的值，较慢
function BigNumberLegendre2(A, P: TCnBigNumber): Integer;
var
  R, Res: TCnBigNumber;
begin
  if A.IsZero or A.IsNegative or P.IsZero or P.IsNegative then
    raise Exception.Create('A, P Must > 0');

  R := ObtainBigNumberFromPool;
  Res := ObtainBigNumberFromPool;

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
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(Res);
  end;
end;

// 使用 Tonelli Shanks 算法进行模素数二次剩余求解，调用者需自行保证 P 为奇素数或奇素数的整数次方
function BigNumberTonelliShanks(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
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

  Q := ObtainBigNumberFromPool;
  Z := ObtainBigNumberFromPool;
  C := ObtainBigNumberFromPool;
  R := ObtainBigNumberFromPool;
  T := ObtainBigNumberFromPool;
  L := ObtainBigNumberFromPool;
  U := ObtainBigNumberFromPool;
  B := ObtainBigNumberFromPool;
  N := ObtainBigNumberFromPool;

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
      BigNumberMulMod(R, R, B, P);

      // T := T*B*B mod P = (T*B mod P) * (B mod P) mod P
      BigNumberMulMod(U, T, B, P); // U := T*B mod P
      BigNumberMod(L, B, P);       // L := B mod P
      BigNumberMulMod(T, U, L, P);

      BigNumberMulMod(C, B, B, P);
    end;

    BigNumberMod(L, R, P);
    BigNumberAdd(L, L, P);
    BigNumberMod(Res, L, P);
    Result := True;
  finally
    RecycleBigNumberToPool(Q);
    RecycleBigNumberToPool(Z);
    RecycleBigNumberToPool(C);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(L);
    RecycleBigNumberToPool(U);
    RecycleBigNumberToPool(B);
    RecycleBigNumberToPool(N);
  end;
end;

// 使用 IEEE P1363 规范中的 Lucas 序列进行模素数二次剩余求解
function BigNumberLucas(const Res: TCnBigNumber; A, P: TCnBigNumber): Boolean;
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
    G := ObtainBigNumberFromPool;
    X := ObtainBigNumberFromPool;
    Z := ObtainBigNumberFromPool;
    U := ObtainBigNumberFromPool;
    V := ObtainBigNumberFromPool;
    T := ObtainBigNumberFromPool;

    while True do
    begin
      if not BigNumberRandRange(X, P) then
        Exit;

      BigNumberCopy(T, P);
      BigNumberAddWord(T, 1);
      BigNumberShiftRight(T, T, 1);
      if not BigNumberLucasSequenceMod(X, A, T, P, U, V) then
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

      if not BigNumberUnsignedMulMod(T, Z, Z, P) then
        Exit;

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
    RecycleBigNumberToPool(G);
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Z);
    RecycleBigNumberToPool(U);
    RecycleBigNumberToPool(V);
    RecycleBigNumberToPool(T);
  end;
end;

procedure BigNumberPollardRho(X: TCnBigNumber; C: TCnBigNumber; Res: TCnBigNumber);
var
  I, K, X0, Y0, Y, D, X1, R: TCnBigNumber;
begin
  I := ObtainBigNumberFromPool;
  K := ObtainBigNumberFromPool;
  X0 := ObtainBigNumberFromPool;
  X1 := ObtainBigNumberFromPool;
  Y0 := ObtainBigNumberFromPool;
  Y := ObtainBigNumberFromPool;
  D := ObtainBigNumberFromPool;
  R := ObtainBigNumberFromPool;

  try
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

      BigNumberMulMod(R, X0, X0, X);
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
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(I);
    RecycleBigNumberToPool(K);
    RecycleBigNumberToPool(X0);
    RecycleBigNumberToPool(X1);
    RecycleBigNumberToPool(Y0);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(D);
  end;
end;

// 找出大数的质因数列表
procedure BigNumberFindFactors(Num: TCnBigNumber; Factors: TCnBigNumberList);
var
  P, R, S, D: TCnBigNumber;
begin
  if BigNumberIsProbablyPrime(Num) then
  begin
    Factors.Add(BigNumberDuplicate(Num));
    Exit;
  end;

  P := ObtainBigNumberFromPool;
  R := ObtainBigNumberFromPool;
  S := ObtainBigNumberFromPool;
  D := ObtainBigNumberFromPool;
  try
    P := BigNumberCopy(P, Num);

    while BigNumberCompare(P, Num) >= 0 do
    begin
      BigNumberCopy(S, Num);
      BigNumberSubWord(S, 1);
      BigNumberRandRange(R, S);
      BigNumberAddWord(R, 1);
      BigNumberPollardRho(P, R, P);
    end;

    BigNumberFindFactors(P, Factors);
    D := ObtainBigNumberFromPool;
    BigNumberDiv(D, R, Num, P);
    BigNumberFindFactors(D, Factors);
  finally
    RecycleBigNumberToPool(D);
    RecycleBigNumberToPool(S);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(P);
  end;
end;

// 计算 IEEE P1363 的规范中说明的 Lucas 序列
function BigNumberLucasSequenceMod(X, Y, K, N: TCnBigNumber; Q, V: TCnBigNumber): Boolean;
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

  V0 := nil;
  V1 := nil;
  Q0 := nil;
  Q1 := nil;
  T0 := nil;
  T1 := nil;
  C2 := nil;

  try
    V0 := ObtainBigNumberFromPool;
    V1 := ObtainBigNumberFromPool;
    Q0 := ObtainBigNumberFromPool;
    Q1 := ObtainBigNumberFromPool;
    T0 := ObtainBigNumberFromPool;
    T1 := ObtainBigNumberFromPool;
    C2 := ObtainBigNumberFromPool;

    C2.SetWord(2);
    V0.SetWord(2);
    BigNumberCopy(V1, X);
    Q0.SetOne;
    Q1.SetOne;

    C := BigNumberGetBitsCount(K);
    if C < 1 then
      Exit;

    for I := C - 1 downto 0 do
    begin
      if not BigNumberMulMod(Q0, Q0, Q1, N) then
        Exit;

      if BigNumberIsBitSet(K, I) then
      begin
        if not BigNumberMulMod(Q1, Q0, Y, N) then
          Exit;

        if not BigNumberMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V0, T0, N) then
          Exit;

        if not BigNumberMulMod(T0, V1, V1, N) then
          Exit;
        if not BigNumberMulMod(T1, C2, Q1, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;
      end
      else
      begin
        BigNumberCopy(Q1, Q0);

        if not BigNumberMulMod(T0, V0, V1, N) then
          Exit;
        if not BigNumberMulMod(T1, X, Q0, N) then
          Exit;
        if not BigNumberSub(T0, T0, T1) then
          Exit;
        if not BigNumberNonNegativeMod(V1, T0, N) then
          Exit;

        if not BigNumberMulMod(T0, V0, V0, N) then
          Exit;
        if not BigNumberMulMod(T1, C2, Q0, N) then
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
    RecycleBigNumberToPool(V0);
    RecycleBigNumberToPool(V1);
    RecycleBigNumberToPool(Q0);
    RecycleBigNumberToPool(Q1);
    RecycleBigNumberToPool(T0);
    RecycleBigNumberToPool(T1);
    RecycleBigNumberToPool(C2);
  end;
end;

// 打印大数内部信息
function BigNumberDebugDump(const Num: TCnBigNumber): string;
var
  I: Integer;
begin
  Result := '';
  if Num = nil then
    Exit;

//    D: PDWORD;          // 一个 array[0..Top-1] of DWORD 数组，越往后越代表高位
//    Top: Integer;       // Top 表示上限，D[Top] 为 0，D[Top - 1] 是最高位有效数
//    DMax: Integer;      // D 数组的存储上限
//    Neg: Integer;       // 1 为负，0 为正
//    Flags: Integer;

  Result := Format('Flag %d. Neg %d. DMax %d. Top %d.', [Num.Flags, Num.Neg, Num.DMax, Num.Top]);
  if (Num.D <> nil) and (Num.Top > 0) then
    for I := 0 to Num.Top do
      Result := Result + Format(' $%8.8x', [PLongWordArray(Num.D)^[I]]);
end;

{ TCnBigNumber }

function TCnBigNumber.AddWord(W: LongWord): Boolean;
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
  Flags := BN_FLG_MALLOCED;
  Top := 0;
  Neg := 0;
  DMax := 0;
  D := nil;
end;

destructor TCnBigNumber.Destroy;
begin
{$IFDEF DEBUG}
  if FIsFromPool then
    raise Exception.Create('Error. Try to Free a Big Number From Pool.');
{$ENDIF}

  if (D <> nil) and (BigNumberGetFlag(Self, BN_FLG_STATIC_DATA) = 0) then
    FreeMemory(Self.D);     // 不是外部管理的静态数据，需要释放
  if BigNumberGetFlag(Self, BN_FLG_MALLOCED) <> 0 then
  begin
    // Dispose(Num);
  end
  else
  begin
    BigNumberSetFlag(Self, BN_FLG_FREE);
    D := nil;
  end;
  inherited;
end;

function TCnBigNumber.DivWord(W: LongWord): LongWord;
begin
  Result := BigNumberDivWord(Self, W);
end;

class function TCnBigNumber.FromBinary(Buf: PAnsiChar;
  Len: Integer): TCnBigNumber;
begin
  Result := BigNumberFromBinary(Buf, Len);
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

function TCnBigNumber.GetWord: LongWord;
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

function TCnBigNumber.IsOne: Boolean;
begin
  Result := BigNumberIsOne(Self);
end;

function TCnBigNumber.IsNegOne: Boolean;
begin
  Result := BigNumberIsNegOne(Self);
end;

function TCnBigNumber.IsWord(W: LongWord): Boolean;
begin
  Result := BigNumberIsWord(Self, W);
end;

function TCnBigNumber.IsZero: Boolean;
begin
  Result := BigNumberIsZero(Self);
end;

function TCnBigNumber.ModWord(W: LongWord): LongWord;
begin
  Result := BigNumberModWord(Self, W);
end;

function TCnBigNumber.MulWord(W: LongWord): Boolean;
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

function TCnBigNumber.SetBinary(Buf: PAnsiChar; Len: Integer): Boolean;
begin
  Result := BigNumberSetBinary(Buf, Len, Self);
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

function TCnBigNumber.SetWord(W: LongWord): Boolean;
begin
  Result := BigNumberSetWord(Self, W);
end;

{$IFDEF SUPPORT_UINT64}

function TCnBigNumber.SetUInt64(W: UInt64): Boolean;
begin
  Result := BigNumberSetUInt64(Self, W);
end;

{$ENDIF}

procedure TCnBigNumber.SetZero;
begin
  BigNumberSetZero(Self);
end;

function TCnBigNumber.SubWord(W: LongWord): Boolean;
begin
  Result := BigNumberSubWord(Self, W);
end;

function TCnBigNumber.ToBinary(const Buf: PAnsiChar): Integer;
begin
  Result := BigNumberToBinary(Self, Buf);
end;

function TCnBigNumber.ToDec: string;
begin
  Result := string(BigNumberToDec(Self));
end;

function TCnBigNumber.ToHex: string;
begin
  Result := BigNumberToHex(Self);
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

procedure FreeBigNumberPool;
var
  I: Integer;
begin
  for I := 0 to FLocalBigNumberPool.Count - 1 do
  begin
{$IFDEF DEBUG}
    TCnBigNumber(FLocalBigNumberPool[I]).FIsFromPool := False;
{$ENDIF}
    TObject(FLocalBigNumberPool[I]).Free;
  end;
  FreeAndNil(FLocalBigNumberPool);
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

function TCnBigNumber.PowerWord(W: LongWord): Boolean;
begin
  Result := BigNumberPower(Self, Self, W);
end;

function TCnBigNumber.GetTenPrecision: Integer;
begin
  Result := BigNumberGetTenPrecision(Self);
end;

{ TCnBigNumberList }

function TCnBigNumberList.Add(ABigNumber: TCnBigNumber): Integer;
begin
  Result := inherited Add(ABigNumber);
end;

constructor TCnBigNumberList.Create(AOwnsObjects: Boolean);
begin
  if not AOwnsObjects then
    raise Exception.Create('MUST Owns Objects.');
  inherited Create(True);
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

initialization
  FLocalBigNumberPool := TObjectList.Create(False);
  CnBigNumberOne := TCnBigNumber.Create;
  CnBigNumberOne.SetOne;
  CnBigNumberZero := TCnBigNumber.Create;
  CnBigNumberZero.SetZero;

finalization
  CnBigNumberOne.Free;
  CnBigNumberZero.Free;
  FreeBigNumberPool;

end.

