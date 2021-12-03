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

unit CnBigNumber;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：大数算法单元
* 单元作者：刘啸
* 备    注：大部分从 Openssl 的 C 代码移植而来，大数池不支持多线程
*           Word 系列操作函数指大数与 DWORD 进行运算，而 Words 系列操作函数指
*           大数中间的运算过程。
*           ======== !!! D5/D6/CB5/CB6 下可能遇上编译器 Bug 无法修复 !!! =======
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2021.11.29 V2.2
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
  Contnrs, CnContainers, CnRandom {$IFDEF UNICODE}, AnsiStrings {$ENDIF};

const
  BN_BITS_UINT_32       = 32;
  BN_BITS_UINT_64       = 64;
  BN_BYTES              = 4;
  BN_BITS2              = 32;     // D 数组中的一个元素所包含的位数
  BN_BITS4              = 16;
  BN_TBIT               = $80000000;
  BN_MASK2S             = $7FFFFFFF;
  BN_MASK2              = $FFFFFFFF;
  BN_MASK2l             = $FFFF;
  BN_MASK2h             = $FFFF0000;
  BN_MASK2h1            = $FFFF8000;
  BN_MASK3S             = $7FFFFFFFFFFFFFFF;
  BN_MASK3U             = $FFFFFFFFFFFFFFFF;

  BN_MILLER_RABIN_DEF_COUNT = 50; // Miller-Rabin 算法的默认测试次数

type
  TLongWordArray = array [0..MaxInt div SizeOf(Integer) - 1] of TCnLongWord32;
  PLongWordArray = ^TLongWordArray;

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
    Top: Integer;       // Top 表示数字上限，也即有 Top 个有效 LongWord，D[Top] 值为 0，D[Top - 1] 是最高位有效数所在的 LongWord
    DMax: Integer;      // D 数组已分配的存储上限，单位是 LongWord 个，大于或等于 Top，不参与运算
    Neg: Integer;       // 1 为负，0 为正

    constructor Create; virtual;
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

    function GetWordCount: Integer;
    {* 返回大数有多少个有效 LongWord}

    function GetTenPrecision: Integer;
    {* 返回大数有多少个十进制位}

    function GetWord: TCnLongWord32;
    {* 取 DWORD 型首值}

    function SetWord(W: TCnLongWord32): Boolean;
    {* 给大数赋 DWORD 型首值}

    function GetInteger: Integer;
    {* 取 Integer 型首值}

    function SetInteger(W: Integer): Boolean;
    {* 给大数赋 Integer 型首值}

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

    function IsWord(W: TCnLongWord32): Boolean;
    {* 大数是否等于指定 DWORD}

    function AddWord(W: TCnLongWord32): Boolean;
    {* 大数加上一个 DWORD，结果仍放自身中，返回相加是否成功}

    function SubWord(W: TCnLongWord32): Boolean;
    {* 大数减去一个 DWORD，结果仍放自身中，返回相减是否成功}

    function MulWord(W: TCnLongWord32): Boolean;
    {* 大数乘以一个 DWORD，结果仍放自身中，返回相乘是否成功}

    function ModWord(W: TCnLongWord32): TCnLongWord32;
    {* 大数对一个 DWORD 求余，返回余数}

    function DivWord(W: TCnLongWord32): TCnLongWord32;
    {* 大数除以一个 DWORD，商重新放在自身中，返回余数}

    function PowerWord(W: TCnLongWord32): Boolean;
    {* 大数乘方，结果重新放在自身中，返回乘方是否成功}

    procedure SetNegative(Negative: Boolean);
    {* 设置大数是否负值}

    function IsNegative: Boolean;
    {* 返回大数是否负值}

    procedure Negate;
    {* 大数正负号反号}

    procedure ShiftLeftOne;
    {* 左移 1 位}

    procedure ShiftRightOne;
    {* 右移 1 位}

    procedure ShiftLeft(N: Integer);
    {* 左移 N 位}

    procedure ShiftRight(N: Integer);
    {* 右移 N 位}

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
    constructor Create; reintroduce;

    function Add: TCnBigNumber; overload;
    {* 新增一个大数对象，返回该对象，注意添加后无需也不能手动释放}
    function Add(ABigNumber: TCnBigNumber): Integer; overload;
    {* 添加外部的大数对象，注意添加后无需也不能手动释放}
    function Remove(ABigNumber: TCnBigNumber): Integer;
    function IndexOfValue(ABigNumber: TCnBigNumber): Integer;
    procedure Insert(Index: Integer; ABigNumber: TCnBigNumber);
    procedure RemoveDuplicated;
    property Items[Index: Integer]: TCnBigNumber read GetItem write SetItem; default;
  end;

  TCnBigNumberPool = class(TCnMathObjectPool)
  {* 大数池实现类，允许使用到大数的地方自行创建大数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumber; reintroduce;
    procedure Recycle(Num: TCnBigNumber); reintroduce;
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
    {* 将指数与大数转成字符串}

    property Exponent: Integer read FExponent write FExponent;
    property Value: TCnBigNumber read FValue;
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

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将所有元素中的指数与大数转成多行字符串}

    function Top: TCnExponentBigNumberPair;
    {* 获得最高次对象}
    function Bottom: TCnExponentBigNumberPair;
    {* 获得最低次对象}

    // 需要取、增、删、改、压等操作
    function AddPair(AExponent: Integer; Num: TCnBigNumber): TCnExponentBigNumberPair;
    {* 添加一个 Pair，内部复制大数}
    procedure AssignTo(Dest: TCnSparseBigNumberList);
    {* 复制给另外一份}
    procedure SetValues(LowToHighList: array of Int64);
    {* 从低到高设置值}
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

function BigNumberGetWordsCount(const Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效 LongWord}

function BigNumberGetTenPrecision(const Num: TCnBigNumber): Integer;
{* 返回一个大数对象里的大数有多少个有效十进制位数}

function BigNumberGetWord(const Num: TCnBigNumber): TCnLongWord32;
{* 取一个大数对象的首值，也就是低 32 位无符号值}

function BigNumberSetWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
{* 给一个大数对象赋首值，也就是低 32 位无符号值}

function BigNumberGetInteger(const Num: TCnBigNumber): Integer;
{* 取一个大数对象的首值，也就是低 32 位有符号数}

function BigNumberSetInteger(const Num: TCnBigNumber; W: Integer): Boolean;
{* 给一个大数对象赋首值，也就是低 32 位有符号数}

function BigNumberGetInt64(const Num: TCnBigNumber): Int64;
{* 取一个大数对象的首值 Int64}

function BigNumberSetInt64(const Num: TCnBigNumber; W: Int64): Boolean;
{* 给一个大数对象赋首值 Int64}

function BigNumberGetUInt64UsingInt64(const Num: TCnBigNumber): TUInt64;
{* 使用 Int64 取一个大数对象的首值 UInt64}

function BigNumberSetUInt64UsingInt64(const Num: TCnBigNumber; W: TUInt64): Boolean;
{* 使用 Int64 给一个大数对象赋 UInt64 首值}

{$IFDEF SUPPORT_UINT64}

function BigNumberGetUInt64(const Num: TCnBigNumber): UInt64;
{* 取一个大数对象的首值 UInt64}

function BigNumberSetUInt64(const Num: TCnBigNumber; W: UInt64): Boolean;
{* 给一个大数对象赋首值 UInt64}

{$ENDIF}

function BigNumberIsWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
{* 某大数是否等于指定 DWORD}

function BigNumberAbsIsWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
{* 某大数绝对值是否等于指定 DWORD}

function BigNumberAddWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
{* 大数加上一个 DWORD，结果仍放 Num 中，返回相加是否成功}

function BigNumberSubWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
{* 大数减去一个 DWORD，结果仍放 Num 中，返回相减是否成功}

function BigNumberMulWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
{* 大数乘以一个 DWORD，结果仍放 Num 中，返回相乘是否成功}

function BigNumberModWord(const Num: TCnBigNumber; W: TCnLongWord32): TCnLongWord32;
{* 大数对一个 DWORD 求余，返回余数}

function BigNumberDivWord(const Num: TCnBigNumber; W: TCnLongWord32): TCnLongWord32;
{* 大数除以一个 DWORD，商重新放在 Num 中，返回余数}

procedure BigNumberSetNegative(const Num: TCnBigNumber; Negative: Boolean);
{* 给一个大数对象设置是否负值}

function BigNumberIsNegative(const Num: TCnBigNumber): Boolean;
{* 返回一个大数对象是否负值}

procedure BigNumberNegate(const Num: TCnBigNumber);
{* 给一个大数对象设置正负反号}

function BigNumberClearBit(const Num: TCnBigNumber; N: Integer): Boolean;
{* 给一个大数对象的第 N 个 Bit 置 0，返回成功与否。N 为 0 时代表二进制最低位。}

function BigNumberKeepLowBits(const Num: TCnBigNumber; Count: Integer): Boolean;
{* 给一个大数对象只保留第 0 到 Count - 1 个 Bit 位，高位清零，返回成功与否。}

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

function BigNumberEqual(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Boolean;
{* 比较两个大数对象是否相等，相等返回 True，不等返回 False}

function BigNumberCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
{* 带符号比较两个大数对象，前者大于等于小于后者分别返回 1、0、-1}

function BigNumberCompareInteger(const Num1: TCnBigNumber; const Num2: Integer): Integer;
{* 带符号比较一个大数对象与一个整数，前者大于等于小于后者分别返回 1、0、-1}

function BigNumberUnsignedCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
{* 无符号比较两个大数对象，前者大于等于小于后者分别返回 1、0、-1}

function BigNumberDuplicate(const Num: TCnBigNumber): TCnBigNumber;
{* 创建并复制一个大数对象，返回此新大数对象，需要用 BigNumberFree 来释放}

function BigNumberCopy(const Dst: TCnBigNumber; const Src: TCnBigNumber): TCnBigNumber;
{* 复制一个大数对象，成功返回 Dst}

function BigNumberCopyLow(const Dst: TCnBigNumber; const Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
{* 复制一个大数对象的低 WordCount 个 LongWord，成功返回 Dst}

function BigNumberCopyHigh(const Dst: TCnBigNumber; const Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
{* 复制一个大数对象的高 WordCount 个 LongWord，成功返回 Dst}

procedure BigNumberSwap(const Num1: TCnBigNumber; const Num2: TCnBigNumber);
{* 交换两个大数对象的内容}

function BigNumberRandBytes(const Num: TCnBigNumber; BytesCount: Integer): Boolean;
{* 产生固定字节长度的随机大数}

function BigNumberRandBits(const Num: TCnBigNumber; BitsCount: Integer): Boolean;
{* 产生固定位长度的随机大数}

function BigNumberRandRange(const Num: TCnBigNumber; const Range: TCnBigNumber): Boolean;
{* 产生 [0, Range) 之间的随机大数}

function BigNumberAnd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象按位与，结果放至 Res 中，返回运算是否成功。Res 可以是 Num1 或 Num2}

function BigNumberOr(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象按位或，结果放至 Res 中，返回运算是否成功。Res 可以是 Num1 或 Num2}

function BigNumberXor(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象按位异或，结果放至 Res 中，返回运算是否成功。Res 可以是 Num1 或 Num2}

function BigNumberUnsignedAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象无符号相加，结果放至 Res 中，返回相加是否成功。Res 可以是 Num1 或 Num2}

function BigNumberUnsignedSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象无符号相减，Num1 减 Num2，结果放至 Res 中，
  返回相减是否成功，如 Num1 < Num2 则失败}

function BigNumberAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象带符号相加，结果放至 Res 中，返回相加是否成功，Num1 可以是 Num2，Res 可以是 Num1 或 Num2}

function BigNumberSub(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
{* 两个大数对象带符号相减，结果放至 Res 中，返回相减是否成功，Num1 可以是 Num2，Res 可以是 Num1 或 Num2}

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

function BigNumberSqrt(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
{* 计算一大数对象的平方根的整数部分，结果放 Res 中，返回平方计算是否成功}

function BigNumberRoot(const Res: TCnBigNumber; const Num: TCnBigNumber; Exponent: Integer): Boolean;
{* 计算一大数对象的 Exp 次方根的整数部分，结果放 Res 中，返回根计算是否成功
  要求 Num 不能为负，Exponent 不能为 0 或负
  注：FIXME: 因为大数无法进行浮点计算，目前整数运算有偏差，结果偏大，不推荐使用！}

function BigNumberMul(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 计算两大数对象的乘积，结果放 Res 中，返回乘积计算是否成功，Res 可以是 Num1 或 Num2}

function BigNumberMulKaratsuba(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 用 Karatsuba 算法计算两大数对象的乘积，结果放 Res 中，返回乘积计算是否成功，Res 可以是 Num1 或 Num2
  注：好像也没见快到哪里去}

function BigNumberDiv(const Res: TCnBigNumber; const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* 两大数对象相除，Num / Divisor，商放 Res 中，余数放 Remain 中，返回除法计算是否成功，
   Res 可以是 Num，Remain 可以是 nil 以不需要计算余数}

function BigNumberMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* 两大数对象求余，Num mod Divisor，余数放 Remain 中，返回求余计算是否成功，Remain 可以是 Num}

function BigNumberNonNegativeMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
{* 两大数对象非负求余，Num mod Divisor，余数放 Remain 中，0 <= Remain < |Divisor|
   Remain 始终大于零，返回求余计算是否成功}

function BigNumberMulWordNonNegativeMod(const Res: TCnBigNumber;
  const Num: TCnBigNumber; N: Integer; const Divisor: TCnBigNumber): Boolean;
{* 大数对象乘以 32位有符号整型再非负求余，余数放 Res 中，0 <= Remain < |Divisor|
   Res 始终大于零，返回求余计算是否成功}

function BigNumberPower(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: TCnLongWord32): Boolean;
{* 求大数的整数次方，返回计算是否成功，Res 可以是 Num}

function BigNumberExp(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: TCnBigNumber): Boolean;
{* 求大数 Num 的 Exponent  次方，返回乘方计算是否成功，极其耗时}

function BigNumberGcd(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 求俩大数 Num1 与 Num2 的最大公约数，Res 可以是 Num1 或 Num2}

function BigNumberLcm(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
{* 求俩大数 Num1 与 Num2 的最小公倍数，Res 可以是 Num1 或 Num2}

function BigNumberUnsignedMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
{* 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）
  注意: 三个参数均会忽略负值，也就是均用正值参与计算}

function BigNumberMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 快速计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）
  注意: A、B 允许是负值，乘积为负时，结果为 C - 乘积为正的余
  另外该方法因为比下面的 BigNumberDirectMulMod 慢，所以不建议使用}

function BigNumberDirectMulMod(const Res: TCnBigNumber; A, B, C: TCnBigNumber): Boolean;
{* 普通计算 (A * B) mod C，返回计算是否成功，Res 不能是 C。A、B、C 保持不变（如果 Res 不是 A、B 的话）
  注意：位数较少时，该方法比上面的 BigNumberMulMod 方法要快不少，另外内部执行的是 NonNegativeMod，余数为正}

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

function BigNumberNextPrime(Res, Num: TCnBigNumber;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
{* 生成一个比 Num 大或相等的大素数，结果放 Res，Res 可以是 Num，
  TestCount 指 Miller-Rabin 算法的测试次数，越大越精确也越慢}

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
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解，调用者需自行保证 A B 互素
   A, B 是已知大数，X, Y 是解出来的结果，注意 X 有可能小于 0，如需要正数，可以再加上 B}

procedure BigNumberExtendedEuclideanGcd2(A, B: TCnBigNumber; X: TCnBigNumber;
  Y: TCnBigNumber);
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X - B * Y = 1 的整数解，调用者需自行保证 A B 互素
   A, B 是已知大数，X, Y 是解出来的结果，注意 X 有可能小于 0，如需要正数，可以再加上 B
   X 被称为 A 针对 B 的模反元素，因此本算法也用来算 A 针对 B 的模反元素
   （由于可以视作 -Y，所以本方法与上一方法是等同的 ）}

procedure BigNumberModularInverse(const Res: TCnBigNumber; X, Modulus: TCnBigNumber);
{* 求 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。
   调用者须自行保证 X、Modulus 互质，且 Res 不能是 X 或 Modulus}

procedure BigNumberModularInverseWord(const Res: TCnBigNumber; X: Integer; Modulus: TCnBigNumber);
{* 求 32 位有符号数 X 针对 Modulus 的模反或叫模逆元 Y，满足 (X * Y) mod M = 1，X 可为负值，Y 求出正值。
   调用者须自行保证 X、Modulus 互质，且 Res 不能是 X 或 Modulus}

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

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers, Factors: TCnBigNumberList): Boolean; overload;
{* 用中国剩余定理，根据余数与互素的除数求一元线性同余方程组的最小解，返回求解是否成功
  参数为大数列表}

function BigNumberChineseRemainderTheorem(Res: TCnBigNumber;
  Remainers, Factors: TCnInt64List): Boolean; overload;
{* 用中国剩余定理，根据余数与互素的除数求一元线性同余方程组的最小解，返回求解是否成功
   参数为 Int64 列表}

function BigNumberIsPerfectPower(Num: TCnBigNumber): Boolean;
{* 判断大数是否是完全幂，大数较大时有一定耗时}

procedure BigNumberFillCombinatorialNumbers(List: TCnBigNumberList; N: Integer);
{* 计算组合数 C(m, N) 并生成大数对象放至大数数组中，其中 m 从 0 到 N}

procedure BigNumberFillCombinatorialNumbersMod(List: TCnBigNumberList; N: Integer; P: TCnBigNumber);
{* 计算组合数 C(m, N) mod P 并生成大数对象放至大数数组中，其中 m 从 0 到 N}

function BigNumberAKSIsPrime(N: Int64): Boolean;
{* 用 AKS 算法判断某正整数是否是素数，基于稀疏列表的大数二元多项式，比较慢，不具有可用性}

function BigNumberDebugDump(const Num: TCnBigNumber): string;
{* 打印大数内部信息}

function SparseBigNumberListIsZero(P: TCnSparseBigNumberList): Boolean;
{* 判断 SparseBigNumberList 是否为 0，注意 nil、0 个项、唯一 1 个项是 0，均作为 0 处理}

function SparseBigNumberListEqual(A, B: TCnSparseBigNumberList): Boolean;
{* 判断两个 SparseBigNumberList 是否相等，注意 nil、0 个项、唯一 1 个项是 0，均作为 0 处理}

procedure SparseBigNumberListCopy(Dst, Src: TCnSparseBigNumberList);
{* 将 Src 复制至 Dst}

procedure SparseBigNumberListMerge(Dst, Src1, Src2: TCnSparseBigNumberList; Add: Boolean = True);
{* 合并两个 SparseBigNumberList 至目标 List 中，指数相同的系数 Add 为 True 时相加，否则相减
  Dst 可以是 Src1 或 Src2，Src1 和 Src2 可以相等}

var
  CnBigNumberOne: TCnBigNumber = nil;     // 表示 1 的常量
  CnBigNumberZero: TCnBigNumber = nil;    // 表示 0 的常量

implementation

uses
  CnPrimeNumber, CnBigDecimal, CnPolynomial;

const
  Hex: string = '0123456789ABCDEF';

  BN_CTX_POOL_SIZE = 16;
  BN_CTX_START_FRAMES = 32;
  BN_DEC_CONV = 1000000000;
  BN_DEC_FMT = '%u';
  BN_DEC_FMT2 = '%.9u';
  BN_PRIME_NUMBERS = 2048;

  BN_MUL_KARATSUBA = 80;  // 大于等于 80 个 LongWord 的乘法才用 Karatsuba 算法
  CRLF = #13#10;

  SPARSE_BINARY_SEARCH_THRESHOLD = 4;

{$IFNDEF MSWINDOWS}
  MAXDWORD = TCnLongWord32($FFFFFFFF);
{$ENDIF}

var
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalBigBinaryPool: TCnBigBinaryPool = nil;

function BigNumberNew: TCnBigNumber;
begin
  Result := TCnBigNumber.Create;
end;

procedure BigNumberInit(const Num: TCnBigNumber);
begin
  // FillChar(Num, SizeOf(TCnBigNumber), 0);
  if Num = nil then
    Exit;

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
function BigNumberAbsIsWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
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

function BigNumberGetWordBitsCount(L: TCnLongWord32): Integer;
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

function BigNumberGetWordsCount(const Num: TCnBigNumber): Integer;
begin
  Result := Num.Top;
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

function BigNumberExpandInternal(const Num: TCnBigNumber; Words: Integer): PLongWord;
var
  A, B, TmpA: PLongWord;
  I: Integer;
  A0, A1, A2, A3: TCnLongWord32;
begin
  Result := nil;
  if Words > (MaxInt div (4 * BN_BITS2)) then
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
      TmpA := PLongWord(Integer(TmpA) + 4 * SizeOf(TCnLongWord32));
      B := PLongWord(Integer(B) + 4 * SizeOf(TCnLongWord32));
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
    FillChar(Num.D^, Num.DMax * SizeOf(TCnLongWord32), 0);
  Num.Top := 0;
  Num.Neg := 0;
end;

function BigNumberGetWord(const Num: TCnBigNumber): TCnLongWord32;
begin
  if Num.Top > 1 then
    Result := BN_MASK2
  else if Num.Top = 1 then
    Result := PLongWordArray(Num.D)^[0]
  else
    Result := 0;
end;

function BigNumberSetWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(TCnLongWord32) * 8) = nil then
    Exit;
  Num.Neg := 0;
  PLongWordArray(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 1
  else
    Num.Top := 0;
  Result := True;
end;

function BigNumberGetInteger(const Num: TCnBigNumber): Integer;
begin
  if Num.Top > 1 then
    Result := BN_MASK2S
  else if Num.Top = 1 then
  begin
    Result := Integer(PLongWordArray(Num.D)^[0]);
    if Result < 0 then        // UInt32 最高位有值，说明已经超出了 Integer 的范围，返回 Max Integer
      Result := BN_MASK2S
    else if Num.Neg <> 0 then // 负则求反加一
      Result := (not Result) + 1;
  end
  else
    Result := 0;
end;

function BigNumberSetInteger(const Num: TCnBigNumber; W: Integer): Boolean;
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

function BigNumberGetInt64(const Num: TCnBigNumber): Int64;
begin
  if Num.Top > 2 then
    Result := BN_MASK3S
  else if Num.Top = 2 then
  begin
    Result := PInt64Array(Num.D)^[0];
    if Result < 0 then        // UInt64 最高位有值，说明已经超出了 Int64 的范围，返回 Max Int64
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
    else if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 Int64 高 32 位是 0
      Num.Top := 1
    else
      Num.Top := 2;
  end
  else // W < 0
  begin
    Num.Neg := 1;
    W := (not W) + 1;
    PInt64Array(Num.D)^[0] := W;

    if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 Int64 高 32 位是 0
      Num.Top := 1
    else
      Num.Top := 2;
  end;
  Result := True;
end;

function BigNumberGetUInt64UsingInt64(const Num: TCnBigNumber): TUInt64;
begin
  if Num.Top > 2 then
    Result := TUInt64(BN_MASK3U)
  else if Num.Top = 2 then
  begin
{$IFDEF SUPPORT_UINT64}
    Result := TUInt64(PInt64Array(Num.D)^[0]);
{$ELSE}
    Result := PInt64Array(Num.D)^[0]; // 在 D5/6 下 Int64转 Int64 出现 C3517 错误！！！
{$ENDIF}
  end
  else if Num.Top = 1 then
    Result := TUInt64(PLongWordArray(Num.D)^[0])
  else
    Result := 0;
end;

function BigNumberSetUInt64UsingInt64(const Num: TCnBigNumber; W: TUInt64): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(Int64) * 8) = nil then
    Exit;

  Num.Neg := 0;
  PInt64Array(Num.D)^[0] := Int64(W);
  if W = 0 then
    Num.Top := 0
  else if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 Int64 高 32 位是 0
    Num.Top := 1
  else
    Num.Top := 2;

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
  else if ((W and $FFFFFFFF00000000) shr 32) = 0 then // 如果 UInt64 高 32 位是 0
    Num.Top := 1
  else
    Num.Top := 0;
  Result := True;
end;

{$ENDIF}

// 某大数是否等于指定 DWORD
function BigNumberIsWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
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

    Ftl := PLongWord(Integer(Ftl) - SizeOf(TCnLongWord32));
    Dec(Top);
  end;
  Num.Top := Top;
end;

function BigNumberToBinary(const Num: TCnBigNumber; Buf: PAnsiChar): Integer;
var
  I, N: Integer;
  L: TCnLongWord32;
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
  I, M, N, L: TCnLongWord32;
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

  PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] and TCnLongWord32(not (1 shl J));
  BigNumberCorrectTop(Num);
  Result := True;
end;

function BigNumberKeepLowBits(const Num: TCnBigNumber; Count: Integer): Boolean;
var
  I, J: Integer;
  B: TCnLongWord32;
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

  Num.Top := I + 1;
  if J > 0 then // 要保留最高一个 LongWord 中的 0 到 J - 1 位，共 J 位，J 最多 31
  begin
    B := 1 shl J;         // 0000100000 如果 J 是 31 也不会溢出
    B := B - 1;           // 0000011111
    PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] and B;
  end;

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

  PLongWordArray(Num.D)^[I] := PLongWordArray(Num.D)^[I] or TCnLongWord32(1 shl J);
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

  if (TCnLongWord32(PLongWordArray(Num.D)^[I] shr J) and TCnLongWord32(1)) <> 0 then
    Result := True;
end;

function BigNumberCompareWords(var Num1: TCnBigNumber; var Num2: TCnBigNumber;
  N: Integer): Integer;
var
  I: Integer;
  A, B: TCnLongWord32;
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

function BigNumberEqual(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Boolean;
begin
  Result := BigNumberCompare(Num1, Num2) = 0;
end;

function BigNumberCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
var
  I, Gt, Lt: Integer;
  T1, T2: TCnLongWord32;
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

function BigNumberCompareInteger(const Num1: TCnBigNumber; const Num2: Integer): Integer;
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

function BigNumberUnsignedCompare(const Num1: TCnBigNumber; const Num2: TCnBigNumber): Integer;
var
  I: Integer;
  T1, T2: TCnLongWord32;
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
  A0, A1, A2, A3: TCnLongWord32;
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

    A := PLongWordArray(Integer(A) + 4 * SizeOf(TCnLongWord32));
    B := PLongWordArray(Integer(B) + 4 * SizeOf(TCnLongWord32));
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

function BigNumberCopyLow(const Dst: TCnBigNumber; const Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
var
  I: Integer;
  A, B: PLongWordArray;
begin
  if WordCount <= 0 then
  begin
    Result := Dst;
    Dst.SetZero;
    Exit;
  end
  else if Src = Dst then // 不支持 Src 和 Dst 相同的情况
    Result := nil
  else
  begin
    if WordCount > Src.GetWordCount then
      WordCount := Src.GetWordCount;

    if BigNumberWordExpand(Dst, WordCount) = nil then
    begin
      Result := nil;
      Exit;
    end;

    A := PLongWordArray(Dst.D);
    B := PLongWordArray(Src.D);

    Result := Dst;
    for I := 0 to WordCount - 1 do // 从 Src 的 0 到 WordCount - 1 赋值给 Dst 的 0 到 WordCount - 1
      A[I] := B[I];

    Dst.Top := WordCount;
    Dst.Neg := Src.Neg;
  end;
end;

function BigNumberCopyHigh(const Dst: TCnBigNumber; const Src: TCnBigNumber;
  WordCount: Integer): TCnBigNumber;
var
  I: Integer;
  A, B: PLongWordArray;
begin
  if WordCount <= 0 then
  begin
    Result := Dst;
    Dst.SetZero;
    Exit;
  end
  else if Src = Dst then // 不支持 Src 和 Dst 相同的情况
    Result := nil
  else
  begin
    if WordCount > Src.GetWordCount then
      WordCount := Src.GetWordCount;

    if BigNumberWordExpand(Dst, WordCount) = nil then
    begin
      Result := nil;
      Exit;
    end;

    A := PLongWordArray(Dst.D);
    B := PLongWordArray(Src.D);

    Result := Dst;
    for I := 0 to WordCount - 1 do // 从 Src 的 Top - WordCount 到 Top - 1 赋值给 Dst 的 0 到 WordCount - 1
      A[I] := B[Src.Top - WordCount + I];

    Dst.Top := WordCount;
    Dst.Neg := Src.Neg;
  end;
end;

procedure BigNumberSwap(const Num1: TCnBigNumber; const Num2: TCnBigNumber);
var
  TmpD: PLongWord;
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

// ============================ 低阶运算定义开始 ===============================

// UInt64 的方式计算 N 平方
procedure Sqr(var L: TCnLongWord32; var H: TCnLongWord32; N: TCnLongWord32); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(N, N);
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  L := TCnLongWord32(T) and BN_MASK2;
  H := TCnLongWord32(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 的方式计算 A * B + R + C
procedure MulAdd(var R: TCnLongWord32; A: TCnLongWord32; B: TCnLongWord32; var C: TCnLongWord32); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + R + C;
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  R := TCnLongWord32(T) and BN_MASK2;
  C := TCnLongWord32(T shr BN_BITS2) and BN_MASK2;
end;

// UInt64 的方式计算 A * B + C
procedure Mul(var R: TCnLongWord32; A: TCnLongWord32; B: TCnLongWord32; var C: TCnLongWord32); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  T: TUInt64;
begin
  T := UInt64Mul(A, B) + C;
  // 无符号 32 位整型如果直接相乘，得到的 Int64 可能溢出得到负值，用封装的运算代替。
  R := TCnLongWord32(T) and BN_MASK2;
  C := TCnLongWord32(T shr BN_BITS2) and BN_MASK2;
end;

type
  TCnBitOperation = (boAnd, boOr, boXor);

// N 个 TCnLongWord32 长的数组内容进行位运算，如果 BP 为 nil，表示不够长，作为 0 处理
procedure BigNumberBitOperation(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray;
  N: Integer; Op: TCnBitOperation);
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
            RP[0] := TCnLongWord32((Int64(AP[0]) and Int64(BP[0])) and BN_MASK2);
            RP[1] := TCnLongWord32((Int64(AP[1]) and Int64(BP[1])) and BN_MASK2);
            RP[2] := TCnLongWord32((Int64(AP[2]) and Int64(BP[2])) and BN_MASK2);
            RP[3] := TCnLongWord32((Int64(AP[3]) and Int64(BP[3])) and BN_MASK2);
          end;
        boOr:
          begin
            RP[0] := TCnLongWord32((Int64(AP[0]) or Int64(BP[0])) and BN_MASK2);
            RP[1] := TCnLongWord32((Int64(AP[1]) or Int64(BP[1])) and BN_MASK2);
            RP[2] := TCnLongWord32((Int64(AP[2]) or Int64(BP[2])) and BN_MASK2);
            RP[3] := TCnLongWord32((Int64(AP[3]) or Int64(BP[3])) and BN_MASK2);
          end;
        boXor:
          begin
            RP[0] := TCnLongWord32((Int64(AP[0]) xor Int64(BP[0])) and BN_MASK2);
            RP[1] := TCnLongWord32((Int64(AP[1]) xor Int64(BP[1])) and BN_MASK2);
            RP[2] := TCnLongWord32((Int64(AP[2]) xor Int64(BP[2])) and BN_MASK2);
            RP[3] := TCnLongWord32((Int64(AP[3]) xor Int64(BP[3])) and BN_MASK2);
          end;
      end;

      AP := PLongWordArray(Integer(AP) + 4 * SizeOf(TCnLongWord32));
      BP := PLongWordArray(Integer(BP) + 4 * SizeOf(TCnLongWord32));
      RP := PLongWordArray(Integer(RP) + 4 * SizeOf(TCnLongWord32));

      Dec(N, 4);
    end;

    while N <> 0 do
    begin
      case Op of
        boAnd:
          RP[0] := TCnLongWord32((Int64(AP[0]) and Int64(BP[0])) and BN_MASK2);
        boOr:
          RP[0] := TCnLongWord32((Int64(AP[0]) or Int64(BP[0])) and BN_MASK2);
        boXor:
          RP[0] := TCnLongWord32((Int64(AP[0]) xor Int64(BP[0])) and BN_MASK2);
      end;

      AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
      BP := PLongWordArray(Integer(BP) + SizeOf(TCnLongWord32));
      RP := PLongWordArray(Integer(RP) + SizeOf(TCnLongWord32));
      Dec(N);
    end;
  end
  else // BP 为 nil，代表数组不够长，当成 0 处理
  begin
    if Op = boAnd then
      FillChar(RP[0], N * SizeOf(TCnLongWord32), 0)
    else if Op in [boOr, boXor] then
      Move(AP[0], RP[0], N * SizeOf(TCnLongWord32));
  end;
end;

// ============================ 低阶运算定义结束 ===============================

{* Words 系列内部计算函数开始}

procedure BigNumberAndWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boAnd);
end;

procedure BigNumberOrWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boOr);
end;

procedure BigNumberXorWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer);
begin
  BigNumberBitOperation(RP, AP, BP, N, boXor);
end;

function BigNumberAddWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer): TCnLongWord32;
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
    RP[0] := TCnLongWord32(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[1]) + Int64(BP[1]);
    RP[1] := TCnLongWord32(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[2]) + Int64(BP[2]);
    RP[2] := TCnLongWord32(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + Int64(AP[3]) + Int64(BP[3]);
    RP[3] := TCnLongWord32(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(TCnLongWord32));
    BP := PLongWordArray(Integer(BP) + 4 * SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(TCnLongWord32));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    LL := LL + Int64(AP[0]) + Int64(BP[0]);
    RP[0] := TCnLongWord32(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
    BP := PLongWordArray(Integer(BP) + SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + SizeOf(TCnLongWord32));
    Dec(N);
  end;
  Result := TCnLongWord32(LL);
end;

function BigNumberSubWords(RP: PLongWordArray; AP: PLongWordArray; BP: PLongWordArray; N: Integer): TCnLongWord32;
var
  T1, T2, C: TCnLongWord32;
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

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(TCnLongWord32));
    BP := PLongWordArray(Integer(BP) + 4 * SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(TCnLongWord32));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
    BP := PLongWordArray(Integer(BP) + SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + SizeOf(TCnLongWord32));
    Dec(N);
  end;
  Result := C;
end;

function BigNumberMulAddWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer; W: TCnLongWord32): TCnLongWord32;
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

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(TCnLongWord32));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], W, Result);
    AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + SizeOf(TCnLongWord32));
    Dec(N);
  end;
end;

// AP 指向的 N 个数字都乘以 W，结果的低 N 位放 RP 中，高位放返回值
function BigNumberMulWords(RP: PLongWordArray; AP: PLongWordArray; N: Integer; W: TCnLongWord32): TCnLongWord32;
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

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + 4 * SizeOf(TCnLongWord32));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Mul(RP^[0], AP^[0], W, Result);

    AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + SizeOf(TCnLongWord32));

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

    AP := PLongWordArray(Integer(AP) + 4 * SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + 8 * SizeOf(TCnLongWord32));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Sqr(RP^[0], RP^[1], AP^[0]);
    AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(TCnLongWord32));
    Dec(N);
  end;
end;

// 64 位被除数整除 32 位除数，返回商，Result := H L div D，不管商的高 32 位
// 因此要保证 D 的最高位为 1，商的高 32 位才会为 0，此函数调用才不会出错，所以 32 位下才可以用 DIV 指令优化
function InternalDivWords(H: TCnLongWord32; L: TCnLongWord32; D: TCnLongWord32): TCnLongWord32;
begin
  if D = 0 then
  begin
    Result := BN_MASK2;
    Exit;
  end;

{$IFDEF CPUX64}
  Result := TCnLongWord32(((UInt64(H) shl 32) or UInt64(L)) div UInt64(D));
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

function BigNumberAnd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PLongWord;
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
  AP := PLongWord(A.D);
  BP := PLongWord(B.D);
  RP := PLongWord(Res.D);

  BigNumberAndWords(PLongWordArray(RP), PLongWordArray(AP), PLongWordArray(BP), Min);

  // AP 长的后头还有 Dif 一段没有处理，需要当成和 0 一块运算
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberAndWords(PLongWordArray(RP), PLongWordArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberOr(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PLongWord;
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
  AP := PLongWord(A.D);
  BP := PLongWord(B.D);
  RP := PLongWord(Res.D);

  BigNumberOrWords(PLongWordArray(RP), PLongWordArray(AP), PLongWordArray(BP), Min);

  // AP 长的后头还有 Dif 一段没有处理，需要当成和 0 一块运算
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberOrWords(PLongWordArray(RP), PLongWordArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberXor(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PLongWord;
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
  AP := PLongWord(A.D);
  BP := PLongWord(B.D);
  RP := PLongWord(Res.D);

  BigNumberXorWords(PLongWordArray(RP), PLongWordArray(AP), PLongWordArray(BP), Min);

  // AP 长的后头还有 Dif 一段没有处理，需要当成和 0 一块运算
  Inc(AP, Min);
  Inc(RP, Min);
  BigNumberXorWords(PLongWordArray(RP), PLongWordArray(AP), nil, Dif);

  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberUnsignedAdd(const Res: TCnBigNumber; const Num1: TCnBigNumber;
  const Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PLongWord;
  Carry, T1, T2: TCnLongWord32;
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

  AP := PLongWord(Integer(AP) + Min * SizeOf(TCnLongWord32));
  RP := PLongWord(Integer(RP) + Min * SizeOf(TCnLongWord32));

  if Carry <> 0 then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
      T2 := (T1 + 1) and BN_MASK2;

      RP^ := T2;
      RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));

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
      AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
      RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));
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
  Carry, T1, T2: TCnLongWord32;
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
    AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
    BP := PLongWord(Integer(BP) + SizeOf(TCnLongWord32));
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
    RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));
  end;

  if Carry <> 0 then
  begin
    if Dif = 0 then  // Error! Num1 < Num2
      Exit;

    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
      T2 := (T1 - 1) and BN_MASK2;

      RP^ := T2;
      RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));
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
      AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
      RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
      RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
      RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
      RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));
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
  T, C: TCnLongWord32;
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
    AP := PLongWord(Integer(AP) + SizeOf(TCnLongWord32));
    RP^ := ((T shl 1) or C) and BN_MASK2;
    RP := PLongWord(Integer(RP) + SizeOf(TCnLongWord32));

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
  T, C: TCnLongWord32;
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
  L: TCnLongWord32;
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

  FillChar(Pointer(T)^, NW * SizeOf(TCnLongWord32), 0);
  Res.Top := Num.Top + NW + 1;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberShiftRight(const Res: TCnBigNumber; const Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, J, NW, LB, RB: Integer;
  L, Tmp: TCnLongWord32;
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

  F := PLongWordArray(Integer(Num.D) + NW * SizeOf(TCnLongWord32));
  T := PLongWordArray(Res.D);
  J := Num.Top - NW;
  Res.Top := I;

  if RB = 0 then
  begin
    for I := J downto 1 do
    begin
      T^[0] := F^[0];
      F := PLongWordArray(Integer(F) + SizeOf(TCnLongWord32));
      T := PLongWordArray(Integer(T) + SizeOf(TCnLongWord32));
    end;
  end
  else
  begin
    L := F^[0];
    F := PLongWordArray(Integer(F) + SizeOf(TCnLongWord32));
    for I := J - 1 downto 1 do
    begin
      Tmp := (L shr RB) and BN_MASK2;
      L := F^[0];
      T^[0] := (Tmp or (L shl LB)) and BN_MASK2;

      F := PLongWordArray(Integer(F) + SizeOf(TCnLongWord32));
      T := PLongWordArray(Integer(T) + SizeOf(TCnLongWord32));
    end;

    L := (L shr RB) and BN_MASK2;
    if L <> 0 then
      T^[0] := L;
  end;
  Result := True;
end;

{* 大数与 Word 运算系列函数开始}

function BigNumberAddWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
var
  I: Integer;
  L: TCnLongWord32;
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

function BigNumberSubWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
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

function BigNumberMulWord(const Num: TCnBigNumber; W: TCnLongWord32): Boolean;
var
  L: TCnLongWord32;
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

function BigNumberModWord(const Num: TCnBigNumber; W: TCnLongWord32): TCnLongWord32;
var
  I: Integer;
begin
  if W = 0 then
  begin
    Result := TCnLongWord32(-1);
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

function BigNumberDivWord(const Num: TCnBigNumber; W: TCnLongWord32): TCnLongWord32;
var
  I, J: Integer;
  L, D: TCnLongWord32;
begin
  W := W and BN_MASK2;
  if W = 0 then
  begin
    Result := TCnLongWord32(-1);
    Exit;
  end;

  Result := 0;
  if Num.Top = 0 then
    Exit;

  J := BN_BITS2 - BigNumberGetWordBitsCount(W);

  W := W shl J; // 保证 W 最高位为 1
  if not BigNumberShiftLeft(Num, Num, J) then
  begin
    Result := TCnLongWord32(-1);
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
      V := ((PLongWordArray(Num.D)^[I]) shr TCnLongWord32(J)) and $0F;
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
      V := ((PLongWordArray(Num.D)^[I]) shr TCnLongWord32(J)) and $FF;
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
  L: TCnLongWord32;
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

      L := (L shl 4) or TCnLongWord32(K);

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
    BnData := PLongWord(GetMemory(((N div 9) + 1) * SizeOf(TCnLongWord32)));
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
        LP := PLongWord(Integer(LP) + SizeOf(TCnLongWord32));
      end;
      LP := PLongWord(Integer(LP) - SizeOf(TCnLongWord32));

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
        LP := PLongWord(Integer(LP) - SizeOf(TCnLongWord32));
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

function BigNumberSetDec(const Buf: AnsiString; const Res: TCnBigNumber): Boolean;
var
  P: PAnsiChar;
  Neg, J, I: Integer;
  L: TCnLongWord32;
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

  RP := PLongWordArray(Integer(RP) + SizeOf(TCnLongWord32));
  J := N - 1;

  if J > 0 then
  begin
    AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
    RP[J] := BigNumberMulWords(RP, AP, J, PLongWordArray(Integer(AP) - SizeOf(TCnLongWord32))^[0]);
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(TCnLongWord32));
  end;

  for I := N - 2 downto 1 do
  begin
    Dec(J);
    AP := PLongWordArray(Integer(AP) + SizeOf(TCnLongWord32));
    RP[J] := BigNumberMulAddWords(RP, AP, J, PLongWordArray(Integer(AP) - SizeOf(TCnLongWord32))^[0]);
    RP := PLongWordArray(Integer(RP) + 2 * SizeOf(TCnLongWord32));
  end;

  BigNumberAddWords(PLongWordArray(R), PLongWordArray(R), PLongWordArray(R), Max);
  BigNumberSqrWords(PLongWordArray(Tmp), PLongWordArray(A), N);
  BigNumberAddWords(PLongWordArray(R), PLongWordArray(R), PLongWordArray(Tmp), Max);
end;

function BigNumberSqr(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
var
  Max, AL: Integer;
  Tmp, RR: TCnBigNumber;
  T: array[0..15] of TCnLongWord32;
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
    if PLongWordArray(Num.D)^[AL - 1] = (PLongWordArray(Num.D)^[AL - 1] and BN_MASK2l) then
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

function BigNumberSqrt(const Res: TCnBigNumber; const Num: TCnBigNumber): Boolean;
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

function BigNumberRoot(const Res: TCnBigNumber; const Num: TCnBigNumber;
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

        BigBinaryDiv(T1, T1, T2, 10);            // 得到商，保留一定精度
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

  RR := PLongWord(Integer(R) + NA * SizeOf(TCnLongWord32));
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
    RR := PLongWord(Integer(RR) + SizeOf(TCnLongWord32));
    R := PLongWord(Integer(R) + SizeOf(TCnLongWord32));
    B := PLongWord(Integer(B) + SizeOf(TCnLongWord32));

    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(TCnLongWord32));
    R := PLongWord(Integer(R) + SizeOf(TCnLongWord32));
    B := PLongWord(Integer(B) + SizeOf(TCnLongWord32));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(TCnLongWord32));
    R := PLongWord(Integer(R) + SizeOf(TCnLongWord32));
    B := PLongWord(Integer(B) + SizeOf(TCnLongWord32));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PLongWord(Integer(RR) + SizeOf(TCnLongWord32));
    R := PLongWord(Integer(R) + SizeOf(TCnLongWord32));
    B := PLongWord(Integer(B) + SizeOf(TCnLongWord32));
    RR^ := BigNumberMulAddWords(PLongWordArray(R), PLongWordArray(A), NA, B^);
  end;
end;

function BigNumberMulKaratsuba(const Res: TCnBigNumber; Num1: TCnBigNumber;
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

function BigNumberDiv(const Res: TCnBigNumber; const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
var
  Tmp, SNum, SDiv, SRes: TCnBigNumber;
  I, NormShift, Loop, NumN, DivN, Neg, BackupTop, BackupDMax, BackupNeg: Integer;
  D0, D1, Q, L0, N0, N1, Rem, T2L, T2H: TCnLongWord32;
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
    WNum.D := PLongWord(Integer(SNum.D) + Loop * SizeOf(TCnLongWord32));
    WNum.Top := DivN;
    WNum.DMax := SNum.DMax - Loop;

    D0 := PLongWordArray(SDiv.D)^[DivN - 1];
    if DivN = 1 then
      D1 := 0
    else
      D1 := PLongWordArray(SDiv.D)^[DivN - 2];
    // D0 D1 是 SDiv 的最高俩 DWORD

    WNump := PLongWord(Integer(SNum.D) + (NumN - 1) * SizeOf(TCnLongWord32));

    if Num.Neg <> Divisor.Neg then
      SRes.Neg := 1
    else
      SRes.Neg := 0;

    if BigNumberWordExpand(SRes, Loop + 1) = nil then
      Exit;

    SRes.Top := Loop;
    Resp := PLongWord(Integer(SRes.D) + (Loop - 1) * SizeOf(TCnLongWord32));

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
      Resp := PLongWord(Integer(Resp) - SizeOf(TCnLongWord32));

    for I := 0 to Loop - 2 do
    begin
//    Rem := 0;
      // 用 N0/N1/D0/D1 计算出一个 Q 使 | WNum - SDiv * Q | < SDiv
      N0 := WNump^;
      N1 := (PLongWord(Integer(WNump) - SizeOf(TCnLongWord32)))^;

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
             (T2L <= (PLongWord(Integer(WNump) - 2 * SizeOf(TCnLongWord32)))^)) then
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
      WNum.D := PLongWord(Integer(WNum.D) - SizeOf(TCnLongWord32));

      if BigNumberSubWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
        PLongWordArray(Tmp.D), DivN + 1) <> 0 then
      begin
        Dec(Q);
        if BigNumberAddWords(PLongWordArray(WNum.D), PLongWordArray(WNum.D),
          PLongWordArray(SDiv.D), DivN) <> 0 then
          WNump^ := WNump^ + 1;
      end;

      Resp^ := Q;
      WNump := PLongWord(Integer(WNump) - SizeOf(TCnLongWord32));
      Resp := PLongWord(Integer(Resp) - SizeOf(TCnLongWord32));
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

function BigNumberMod(const Remain: TCnBigNumber;
  const Num: TCnBigNumber; const Divisor: TCnBigNumber): Boolean;
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

function BigNumberMulWordNonNegativeMod(const Res: TCnBigNumber;
  const Num: TCnBigNumber; N: Integer; const Divisor: TCnBigNumber): Boolean;
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

function BigNumberPower(const Res: TCnBigNumber; const Num: TCnBigNumber;
  Exponent: TCnLongWord32): Boolean;
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
      BigNumberMul(T, T, T);
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T);
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

function BigNumberGcd(const Res: TCnBigNumber; Num1: TCnBigNumber;
  Num2: TCnBigNumber): Boolean;
var
  T, A, B: TCnBigNumber;
begin
  Result := False;

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
function BigNumberMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
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
function BigNumberUnsignedMulMod(const Res: TCnBigNumber; const A, B, C: TCnBigNumber): Boolean;
var
  AA, BB: TCnBigNumber;
begin
  Result := False;
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
    R := FLocalBigNumberPool.Obtain;
    if not BigNumberPowerMod(R, A, C, B) then
      Exit;

    L := FLocalBigNumberPool.Obtain;
    if BigNumberCopy(L, R) = nil then // L := R;
      Exit;

    S := FLocalBigNumberPool.Obtain;
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
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(L);
    FLocalBigNumberPool.Recycle(S);
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
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(W);
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
    Num.AddWord(2);

  Result := True;
end;

function BigNumberNextPrime(Res, Num: TCnBigNumber;
  TestCount: Integer = BN_MILLER_RABIN_DEF_COUNT): Boolean;
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
    X1 := FLocalBigNumberPool.Obtain;
    Y := FLocalBigNumberPool.Obtain;

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
    FLocalBigNumberPool.Recycle(X1);
    FLocalBigNumberPool.Recycle(Y);
  end;
end;

procedure BigNumberModularInverseWord(const Res: TCnBigNumber; X: Integer;
  Modulus: TCnBigNumber);
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetInteger(X);
    BigNumberModularInverse(Res, T, Modulus);
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
    raise Exception.Create('A, P Must > 0');

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
    raise Exception.Create('A, P Must > 0');

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
    FLocalBigNumberPool.Recycle(G);
    FLocalBigNumberPool.Recycle(X);
    FLocalBigNumberPool.Recycle(Z);
    FLocalBigNumberPool.Recycle(U);
    FLocalBigNumberPool.Recycle(V);
    FLocalBigNumberPool.Recycle(T);
  end;
end;

procedure BigNumberPollardRho(X: TCnBigNumber; C: TCnBigNumber; Res: TCnBigNumber);
var
  I, K, X0, Y0, Y, D, X1, R: TCnBigNumber;
begin
  I := FLocalBigNumberPool.Obtain;
  K := FLocalBigNumberPool.Obtain;
  X0 := FLocalBigNumberPool.Obtain;
  X1 := FLocalBigNumberPool.Obtain;
  Y0 := FLocalBigNumberPool.Obtain;
  Y := FLocalBigNumberPool.Obtain;
  D := FLocalBigNumberPool.Obtain;
  R := FLocalBigNumberPool.Obtain;

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

  P := FLocalBigNumberPool.Obtain;
  R := FLocalBigNumberPool.Obtain;
  S := FLocalBigNumberPool.Obtain;
  D := FLocalBigNumberPool.Obtain;
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
    D := FLocalBigNumberPool.Obtain;
    BigNumberDiv(D, R, Num, P);
    BigNumberFindFactors(D, Factors);
  finally
    FLocalBigNumberPool.Recycle(D);
    FLocalBigNumberPool.Recycle(S);
    FLocalBigNumberPool.Recycle(R);
    FLocalBigNumberPool.Recycle(P);
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

    Result := BigNumberMod(Res, Sum, G);
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
      // 求 Num 的 I 次方根的整数部分
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
  NR: Boolean;
  R, T, C: Int64;
  K, LG22: Integer;
  LG2, Q: Extended;
  BN, BT: TCnBigNumber;
  X1: TCnInt64Polynomial;
  BP1, BP2, BD: TCnBigNumberBiPolynomial;
  BX2, BRes: TCnBigNumberPolynomial;
begin
  Result := False;
  if N <= 1 then
    Exit;
  if CnInt64IsPerfectPower(N) then // 如果是完全幂则是合数
    Exit;

  // 找出最小的 R 满足 欧拉(R) > (Log二底(N))^2。
  NR := True;
  R := 1;
  LG2 := Log2(N);
  // LG2 := GetUInt64HighBits(N); // 整数会有误差
  LG22 := Trunc(LG2 * LG2);

  // 找出最小的 R
  while NR do
  begin
    Inc(R);
    NR := False;

    K := 1;
    while not NR and (K <= LG22) do
    begin
      T := MontgomeryPowerMod(N, K, R);  // 非负 Int64 可以使用 TUInt64 版本的 MontgomeryPowerMod
      NR := (T = 0) or (T = 1);
      Inc(K);
    end;
  end;

  // 得到 R，如果某些比 R 小的 T 和 N 不互素，则是合数
  T := R;
  while T > 1 do
  begin
    C := CnInt64GreatestCommonDivisor(T, N);
    if (C > 1) and (C < N) then
      Exit;

    Dec(T);
  end;

  if N <= R then
  begin
    Result := True;
    Exit;
  end;

  Q := CnEulerInt64(R);
  // 此处应该用小数计算，因为整数会产生较大误差
  C := Trunc(Sqrt(Q) * LG2);

  X1 := nil;
  BN := nil;
  BT := nil;
  BD := nil;
  BX2 := nil;
  BP1 := nil;
  BP2 := nil;
  BRes := nil;

  try
    // 先在环 (X^R-1, N) 上提前计算 (X+Y)^N，也就是(X+Y)^N 展开后针对 X^R-1 求余，且系数都针对 N 取模

    // 快速展开得到 (X+Y)^N 的系数
    X1 := TCnInt64Polynomial.Create;
    // BigNumberFillCombinatorialNumbersMod(BX1, N, BN);
    CnInt64FillCombinatorialNumbersMod(X1, N, N); // BigNumber 生成万级别的组合数较为耗时，换成 Int64 的

    // 然后将一元 X1 转换成二元 P1，注意此处之前用 N * N * Int64 较耗内存，这里换成基于稀疏列表的 TCnBigNumberBiPolynomial
    BP1 := TCnBigNumberBiPolynomial.Create;
    BP1.MaxXDegree := N;
    BP1.MaxYDegree := N;
    for K := 0 to N do
      BP1.SafeValue[K, N - K].SetInt64(X1[K]);

    BD := TCnBigNumberBiPolynomial.Create;
    BD.SetOne;  // 曾经有一个 SetOne 失效的 BUG，结果在计算 39779 时特别快还准确，不理解
    BD.Negate;
    BD.SetYCoefficents(R, [1]); // D 得到 X^R - 1

    BN := TCnBigNumber.Create;
    BN.SetInt64(N);

    BigNumberBiPolynomialGaloisModX(BP1, BP1, BD, BN);  // P1 得到 (X+Y)^N 针对 X^R - 1 求余
    BigNumberBiPolynomialNonNegativeModWord(BP1, N); // P1 系数再针对 N 取模

    // 再在环 (X^R-1, N) 上提前计算 X^N + Y
    BP2 := TCnBigNumberBiPolynomial.Create;
    BP2.SafeValue[N, 0].SetOne;
    BP2.SafeValue[0, 1].SetOne; // P2 是 X^N + Y
    BigNumberBiPolynomialGaloisModX(BP2, BP2, BD, BN);  // P2 得到 X^N + Y 针对 X^R - 1 求余
    BigNumberBiPolynomialNonNegativeModWord(BP2, N); // P2 系数再针对 N 取模

    BigNumberBiPolynomialGaloisSub(BP1, BP1, BP2, BN);  // P1 - P2 得到 P1 里关于 Y 的表达式

    BRes := TCnBigNumberPolynomial.Create;

    // 从 1 到 欧拉(R)平方根 * (Log二底(N)) 的整数部分

    BT := TCnBigNumber.Create;
    BT.SetOne;
    while BigNumberCompareInteger(BT, C) <= 0 do
    begin
      // 求 P1 的表达式的值，猜测此时 P1 中已无 X 有效项，只有 Y 项
      BT.SetInt64(T);
      BigNumberBiPolynomialGaloisEvaluateByY(BRes, BP1, BT, BN);

      if BRes.MaxDegree > 0 then // 说明还有 X 值，不止常数项
        Exit;
      BigNumberPolynomialNonNegativeModWord(BRes, N); // 常数项如果能被 N 整除
      if not BRes.IsZero then
        Exit;

      Inc(T);
    end;

    Result := True;
  finally
    BD.Free;
    BT.Free;
    BN.Free;
    BX2.Free;
    BP1.Free;
    BP2.Free;
    BRes.Free;
    X1.Free;
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

  Result := Format('Neg %d. DMax %d. Top %d.', [Num.Neg, Num.DMax, Num.Top]);
  if (Num.D <> nil) and (Num.Top > 0) then
    for I := 0 to Num.Top do
      Result := Result + Format(' $%8.8x', [PLongWordArray(Num.D)^[I]]);
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

procedure SparseBigNumberListCopy(Dst, Src: TCnSparseBigNumberList);
var
  I: Integer;
  Pair: TCnExponentBigNumberPair;
begin
  if (Dst <> Src) and (Dst <> nil) then
  begin
    Dst.Clear;
    for I := 0 to Src.Count - 1 do
    begin
      Pair := TCnExponentBigNumberPair.Create;
      Pair.Exponent := Src[I].Exponent;
      BigNumberCopy(Pair.Value, Src[I].Value);
      Dst.Add(Pair);
    end;
  end;
end;

procedure SparseBigNumberListMerge(Dst, Src1, Src2: TCnSparseBigNumberList; Add: Boolean);
var
  I, J, K: Integer;
  P1, P2: TCnExponentBigNumberPair;
begin
  if Src1 = nil then                   // 只要有一个是 nil，Dst 就被塞为另一个
  begin
    SparseBigNumberListCopy(Dst, Src2);
    if not Add then  // Src2 是被减数
      Dst.Negate;
  end
  else if Src2 = nil then
    SparseBigNumberListCopy(Dst, Src1)
  else if Src1 = Src2 then // 如果 Src1 和 Src2 是同一个，合并，支持 Dst 也是同一个的情形
  begin
    Dst.Count := Src1.Count;
    for I := 0 to Src1.Count - 1 do
    begin
      if Dst[I] = nil then
        Dst[I] := TCnExponentBigNumberPair.Create;
      Dst[I].Exponent := Src1[I].Exponent;
      if Add then
        BigNumberAdd(Dst[I].Value, Src1[I].Value, Src2[I].Value)
      else
        BigNumberSub(Dst[I].Value, Src1[I].Value, Src2[I].Value);
    end;
  end
  else // Src1 和 Src2 不是同一个，要归并
  begin
    if (Dst <> Src1) and (Dst <> Src2) then // 但 Dst 不是 Src1 或 Src2，也就是仨各异
    begin
      I := 0;
      J := 0;
      K := 0;

      Dst.Count := Src1.Count + Src2.Count;

      while (I < Src1.Count) and (J < Src2.Count) do
      begin
        P1 := Src1[I];
        P2 := Src2[J];

        if P1.Exponent = P2.Exponent then
        begin
          // 相等，并起来塞到 Dst 里
          if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;
          Dst[K].Exponent := P1.Exponent;

          if Add then
            BigNumberAdd(Dst[K].Value, P1.Value, P2.Value)
          else
            BigNumberSub(Dst[K].Value, P1.Value, P2.Value);

          Inc(I);
          Inc(J);
          Inc(K);
        end
        else if P1.Exponent < P2.Exponent then
        begin
          // P1 小，把 P1 搁 Dst[K] 里
          if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;
          Dst[K].Exponent := P1.Exponent;

          BigNumberCopy(Dst[K].Value, P1.Value);
          Inc(I);
          Inc(K);
        end
        else // P2 小，把 P2 搁 Dst[K] 里
        begin
          if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;
          Dst[K].Exponent := P2.Exponent;

          BigNumberCopy(Dst[K].Value, P2.Value);
          if not Add then
            Dst[K].Value.Negate;
          Inc(J);
          Inc(K);
        end;
      end;

      if (I = Src1.Count) and (J = Src2.Count) then
      begin
        Dst.Compact;
        Exit;
      end;

      // 剩下哪个有，就全加到 Dst 里 K 开始的位置去
      if I = Src1.Count then
      begin
        for I := J to Src2.Count - 1 do
        begin
          if K >= Dst.Count then
            Dst.Add(TCnExponentBigNumberPair.Create)
          else if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;

          Dst[K].Exponent := Src2[I].Exponent;
          BigNumberCopy(Dst[K].Value, Src2[I].Value);
          Inc(K);
        end;
      end
      else if J = Src2.Count then
      begin
        for J := I to Src1.Count - 1 do
        begin
          if K >= Dst.Count then
            Dst.Add(TCnExponentBigNumberPair.Create)
          else if Dst[K] = nil then
            Dst[K] := TCnExponentBigNumberPair.Create;

          Dst[K].Exponent := Src1[J].Exponent;
          BigNumberCopy(Dst[K].Value, Src1[J].Value);
          Inc(K);
        end;
      end;
      Dst.Compact;
    end
    else if Dst = Src1 then // Dst 是 Src1，且 Src1 和 Src2 不同
    begin
      // 遍历 Src2，加塞到 Src1 中
      for I := 0 to Src2.Count - 1 do
      begin
        P2 := Src2[I];
        if Add then
          BigNumberAdd(Dst.SafeValue[P2.Exponent], Dst.SafeValue[P2.Exponent], P2.Value)
        else
          BigNumberSub(Dst.SafeValue[P2.Exponent], Dst.SafeValue[P2.Exponent], P2.Value);
      end;
    end
    else if Dst = Src2 then // Dst 是 Src2，且 Src1 和 Src2 不同
    begin
      // 遍历 Src1，加塞到 Src2 中
      for I := 0 to Src1.Count - 1 do
      begin
        P1 := Src1[I];
        if Add then
          BigNumberAdd(Dst.SafeValue[P1.Exponent], Dst.SafeValue[P1.Exponent], P1.Value)
        else
          BigNumberSub(Dst.SafeValue[P1.Exponent], Dst.SafeValue[P1.Exponent], P1.Value);
      end;
    end;
  end;
end;

{ TCnBigNumber }

function TCnBigNumber.AddWord(W: TCnLongWord32): Boolean;
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
    raise Exception.Create('Error. Try to Free a Big Number From Pool.');
{$ENDIF}

  if D <> nil then
    FreeMemory(D);

  D := nil;
  inherited;
end;

function TCnBigNumber.DivWord(W: TCnLongWord32): TCnLongWord32;
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

function TCnBigNumber.GetWord: TCnLongWord32;
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

function TCnBigNumber.IsWord(W: TCnLongWord32): Boolean;
begin
  Result := BigNumberIsWord(Self, W);
end;

function TCnBigNumber.IsZero: Boolean;
begin
  Result := BigNumberIsZero(Self);
end;

function TCnBigNumber.ModWord(W: TCnLongWord32): TCnLongWord32;
begin
  Result := BigNumberModWord(Self, W);
end;

function TCnBigNumber.MulWord(W: TCnLongWord32): Boolean;
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

function TCnBigNumber.SetWord(W: TCnLongWord32): Boolean;
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

function TCnBigNumber.SubWord(W: TCnLongWord32): Boolean;
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

function TCnBigNumber.PowerWord(W: TCnLongWord32): Boolean;
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

constructor TCnBigNumberList.Create;
begin
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
  Result := '^' + IntToStr(FExponent) + ' ' + FValue.ToDec;
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
    Result := Items[Count - 1];
end;

function TCnSparseBigNumberList.ToString: string;
var
  I: Integer;
  First: Boolean;
begin
  Result := '';
  First := True;
  for I := Count - 1 downto 0 do
  begin
    if First then
    begin
      Result := Items[I].ToString;
      First := False;
    end
    else
      Result := Result + CRLF + Items[I].ToString;
  end;
end;

initialization
  FLocalBigNumberPool := TCnBigNumberPool.Create;

  CnBigNumberOne := TCnBigNumber.Create;
  CnBigNumberOne.SetOne;
  CnBigNumberZero := TCnBigNumber.Create;
  CnBigNumberZero.SetZero;

finalization
//  CnBigNumberZero.DecString;  // 手工调用这两句防止被编译器忽略
//  CnBigNumberZero.DebugDump;

  CnBigNumberOne.Free;
  CnBigNumberZero.Free;
  FLocalBigNumberPool.Free;
  FLocalBigBinaryPool.Free;

end.

