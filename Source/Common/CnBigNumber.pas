{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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
* 备    注：大部分从 Openssl 的 C 代码移植而来
*           Word 系列操作函数指大数与 DWORD 进行运算，而 Words 系列操作函数指
*           大数中间的运算过程。
*           Div 对于数位过大时似乎仍有问题，待进一步修正。
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2014.10.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Windows;

const
  BN_FLG_MALLOCED       = $1;    // 本大数结构是动态分配而来
  BN_FLG_STATIC_DATA    = $2;    // 本大数结构中的 D 内存是静态数据
  BN_FLG_CONSTTIME      = $4;

  BN_FLG_FREE           = $8000;

  BN_BITS               = 64;
  BN_BYTES              = 4;
  BN_BITS2              = 32;     // D 数组中的一个元素所包含的位数
  BN_BITS4              = 16;
  BN_TBIT               = $80000000;
  BN_MASK2              = $FFFFFFFF;
  BN_MASK2l             = $FFFF;
  BN_MASK2h             = $FFFF0000;
  BN_MASK2h1            = $FFFF8000;

type
  TDWordArray = array [0..MaxInt div SizeOf(Integer) - 1] of DWORD;
  PDWordArray = ^TDWordArray;

  {* 用来代表一个大数的结构体 }
  TCnBigNumber = packed record
    D: PDWORD;          // 一个 array[0..Top-1] of DWORD 数组，越往后越代表高位
    Top: Integer;       // Top 表示上限，D[Top] 为 0，D[Top - 1] 是最高位有效数
    DMax: Integer;      // D 数组的存储上限
    Neg: Integer;       // 1 为负，0 为正
    Flags: Integer;
  end;
  PCnBigNumber = ^TCnBigNumber;

function BigNumberNew: PCnBigNumber;
{* 创建一个动态分配的大数结构，返回其指针，此指针不用时必须用 BigNumberFree 释放}

procedure BigNumberFree(Num: PCnBigNumber);
{* 按需要释放一个由 BigNumerNew 函数创建的大数结构指针，并按需要释放其 D 结构
   对于非 BigNumerNew 函数创建的大数结构指针则只按需要释放其 D 结构  }

procedure BigNumberInit(var Num: TCnBigNumber);
{* 初始化一个大数结构，全为 0，并不分配 D 内存}

procedure BigNumberClear(var Num: TCnBigNumber);
{* 清除一个大数结构，并将其数据空间填 0，并不释放 D 内存 }

function BigNumberIsZero(var Num: TCnBigNumber): Boolean;
{* 返回一个大数结构里的大数是否为 0 }

function BigNumberSetZero(var Num: TCnBigNumber): Boolean;
{* 将一个大数结构里的大数设置为 0 }

function BigNumberIsOne(var Num: TCnBigNumber): Boolean;
{* 返回一个大数结构里的大数是否为 1 }

function BigNumberSetOne(var Num: TCnBigNumber): Boolean;
{* 将一个大数结构里的大数设置为 1 }

function BigNumberIsOdd(var Num: TCnBigNumber): Boolean;
{* 返回一个大数结构里的大数是否为奇数 }

function BigNumberGetBitsCount(var Num: TCnBigNumber): Integer;
{* 返回一个大数结构里的大数有多少个有效 bit }

function BigNumberGetBytesCount(var Num: TCnBigNumber): Integer;
{* 返回一个大数结构里的大数有多少个有效 bytes }

function BigNumberGetWord(var Num: TCnBigNumber): DWORD;
{* 取一个大数结构的首值 }

function BigNumberSetWord(var Num: TCnBigNumber; W: DWORD): Boolean;
{* 给一个大数结构赋首值 }

procedure BigNumberSetNegative(var Num: TCnBigNumber; Negative: Boolean);
{* 给一个大数结构设置是否负值 }

function BigNumberIsNegative(var Num: TCnBigNumber): Boolean;
{* 返回一个大数结构是否负值 }

function BigNumberClearBit(var Num: TCnBigNumber; N: Integer): Boolean;
{* 给一个大数结构的第 N 个 Bit 置 0，返回成功与否 }

function BigNumberSetBit(var Num: TCnBigNumber; N: Integer): Boolean;
{* 给一个大数结构的第 N 个 Bit 置 1，返回成功与否 }

function BigNumberIsBitSet(var Num: TCnBigNumber; N: Integer): Boolean;
{* 返回一个大数结构的第 N 个 Bit 是否为 1 }

function BigNumberWordExpand(var Num: TCnBigNumber; Words: Integer): PCnBigNumber;
{* 将一个大数结构扩展成支持 Words 个 DWORD，成功返回扩展的大数结构地址，失败返回 nil}

function BigNumberToString(var Num: TCnBigNumber): string;
{* 将一个大数结构转成字符串 }

function BigNumberToHex(var Num: TCnBigNumber): string;
{* 将一个大数结构转成十六进制字符串}

function BigNumberCompare(var Num1: TCnBigNumber; var Num2: TCnBigNumber): Integer;
{* 带符号比较两个大数结构，前者大于等于小于后者分别返回 1、0、-1 }

function BigNumberUnsignedCompare(var Num1: TCnBigNumber; var Num2: TCnBigNumber): Integer;
{* 无符号比较两个大数结构，前者大于等于小于后者分别返回 1、0、-1 }

function BigNumberDuplicate(var Num: TCnBigNumber): PCnBigNumber;
{* 创建并复制一个大数结构，返回此新大数结构，需要用 BigNumberFree 来释放}

function BigNumberCopy(var Dst: TCnBigNumber; var Src: TCnBigNumber): PCnBigNumber;
{* 复制一个大数结构，成功返回 Dst}

procedure BigNumberSwap(var Num1: TCnBigNumber; var Num2: TCnBigNumber);
{* 交换两个大数结构}

function BigNumberRandBytes(var Num: TCnBigNumber; BytesCount: Integer): Boolean;
{* 产生固定字节长度的随机大数 }

function BigNumberUnsignedAdd(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
{* 两个大数结构无符号相加，结果放至 Res 中，返回相加是否成功}

function BigNumberUnsignedSub(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
{* 两个大数结构无符号相减，Num1 减 Num2，结果放至 Res 中，
  返回相减是否成功，如 Num1 < Num2 则失败}

function BigNumberAdd(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
{* 两个大数结构带符号相加，结果放至 Res 中，返回相加是否成功}

function BigNumberSub(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
{* 两个大数结构带符号相减，结果放至 Res 中，返回相减是否成功}

function BigNumberShiftLeftOne(var Res: TCnBigNumber; var Num: TCnBigNumber): Boolean;
{* 将一大数结构左移一位，结果放至 Res 中，返回左移是否成功}

function BigNumberShiftRightOne(var Res: TCnBigNumber; var Num: TCnBigNumber): Boolean;
{* 将一大数结构右移一位，结果放至 Res 中，返回右移是否成功}

function BigNumberShiftLeft(var Res: TCnBigNumber; var Num: TCnBigNumber;
  N: Integer): Boolean;
{* 将一大数结构左移 N 位，结果放至 Res 中，返回左移是否成功}

function BigNumberShiftRight(var Res: TCnBigNumber; var Num: TCnBigNumber;
  N: Integer): Boolean;
{* 将一大数结构右移 N 位，结果放至 Res 中，返回右移是否成功}

function BigNumberSqr(var Res: TCnBigNumber; var Num: TCnBigNumber): Boolean;
{* 计算一大数结构的平方，结果放 Res 中，返回平方计算是否成功}

function BigNumberMul(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
{* 计算两大数结构的乘积，结果放 Res 中，返回乘积计算是否成功}

function BigNumberDiv(var Res: TCnBigNumber; var Remain: TCnBigNumber;
  var Num: TCnBigNumber; var Divisor: TCnBigNumber): Boolean;
{* 两大数结构相除，Num / Divisor，商放 Res 中，余数放 Remain 中，返回除法计算是否成功}

function BigNumberMod(var Remain: TCnBigNumber;
  var Num: TCnBigNumber; var Divisor: TCnBigNumber): Boolean;
{* 两大数结构求余，Num mod Divisor，余数放 Remain 中，返回求余计算是否成功}

implementation

const
  Hex: string = '0123456789ABCDEF';

  ADVAPI32 = 'advapi32.dll';

  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = $8;
  CRYPT_DELETEKEYSET = $10;

  PROV_RSA_FULL = 1;

  BN_CTX_POOL_SIZE = 16;
  BN_CTX_START_FRAMES = 32;

type
  {* 大数运算的中间结构中的双向链表元素，
     每个元素包含一数组，容纳 BN_CTX_POOL_SIZE 个大数结构}
  PBigNumberPoolItem = ^TBigNumberPoolItem;
  TBigNumberPoolItem = packed record
    Vals: array[0..BN_CTX_POOL_SIZE - 1] of TCnBigNumber;
    Prev: PBigNumberPoolItem;
    Next: PBigNumberPoolItem;
  end;

  {* 大数运算的中间池，一双向链表 }
  TBigNumberPool = packed record
    Head: PBigNumberPoolItem;
    Current: PBigNumberPoolItem;
    Tail: PBigNumberPoolItem;
    Used: DWORD;
    Size: DWORD;
  end;

  {* 大数运算堆栈}
  TBigNumberStack = packed record
    Indexes: PDWORD;
    Depth: DWORD;
    Size: DWORD;
  end;

  {* 大数运算中间结构 }
  PBigNumberContext = ^TBigNumberContext;
  TBigNumberContext = packed record
    Pool: TBigNumberPool;
    Stack: TBigNumberStack;
    Used: DWORD;
    ErrStack: Integer;
    TooMany: Integer;
  end;

function CryptAcquireContext(phProv: PULONG; pszContainer: PAnsiChar;
  pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI32 name 'CryptAcquireContextA';

function CryptReleaseContext(hProv: ULONG; dwFlags: DWORD): BOOL;
  stdcall; external ADVAPI32 name 'CryptReleaseContext';

function CryptGenRandom(hProv: ULONG; dwLen: DWORD; pbBuffer: PAnsiChar): BOOL;
  stdcall; external ADVAPI32 name 'CryptGenRandom';

procedure BigNumberSetFlag(var Num: TCnBigNumber; N: Integer);
begin
  Num.Flags := Num.Flags or N;
end;

function BigNumberGetFlag(var Num: TCnBigNumber; N: Integer): Integer;
begin
  Result := Num.Flags and N;
end;

function BigNumberNew: PCnBigNumber;
begin
  New(Result);
  Result^.Flags := BN_FLG_MALLOCED;
  Result^.Top := 0;
  Result^.Neg := 0;
  Result^.DMax := 0;
  Result^.D := nil;
end;

procedure BigNumberInit(var Num: TCnBigNumber);
begin
  FillChar(Num, SizeOf(TCnBigNumber), 0);
end;

procedure BigNumberFree(Num: PCnBigNumber);
begin
  if Num = nil then
    Exit;

  if (Num^.D <> nil) and (BigNumberGetFlag(Num^, BN_FLG_STATIC_DATA) <> 0) then
    Dispose(Num^.D);
  if BigNumberGetFlag(Num^, BN_FLG_MALLOCED) <> 0 then
  begin
    Dispose(Num);
  end
  else
  begin
    BigNumberSetFlag(Num^, BN_FLG_FREE);
    Num^.D := nil;
  end;
end;

function BigNumberIsZero(var Num: TCnBigNumber): Boolean;
begin
  Result := (Num.Top = 0);
end;

function BigNumberSetZero(var Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 0);
end;

// 返回一个大数结构里的大数的绝对值是否为指定的 DWORD 值
function BigNumberAbsIsWord(var Num: TCnBigNumber; W: DWORD): Boolean;
begin
  Result := True;
  if (W = 0) and (Num.Top = 0) then
    Exit;
  if (Num.Top = 1) and (PDWordArray(Num.D)^[0] = W) then
    Exit;
  Result := False;
end;

function BigNumberIsOne(var Num: TCnBigNumber): Boolean;
begin
  if (Num.Neg = 0) and BigNumberAbsIsWord(Num, 1) then
    Result := True
  else
    Result := False;
end;

function BigNumberSetOne(var Num: TCnBigNumber): Boolean;
begin
  Result := BigNumberSetWord(Num, 1);
end;

function BigNumberIsOdd(var Num: TCnBigNumber): Boolean;
begin
  if (Num.Top > 0) and ((PDWordArray(Num.D)^[0] and 1) <> 0) then
    Result := True
  else
    Result := False;
end;

function BigNumberGetWordBitsCount(L: DWORD): Integer;
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

function BigNumberGetBitsCount(var Num: TCnBigNumber): Integer;
var
  I: Integer;
begin
  Result := 0;
  if BigNumberIsZero(Num) then
    Exit;

  I := Num.Top - 1;
  Result := ((I * BN_BITS2) + BigNumberGetWordBitsCount(PDWordArray(Num.D)^[I]));
end;

function BigNumberGetBytesCount(var Num: TCnBigNumber): Integer;
begin
  Result := (BigNumberGetBitsCount(Num) + 7) div 8;
end;

function BigNumberExpandInternal(var Num: TCnBigNumber; Words: Integer): PDWORD;
var
  A, B, TmpA: PDWORD;
  I: Integer;
  A0, A1, A2, A3: DWORD;
begin
  Result := nil;
  if Words > (MaxInt div (4 * BN_BITS2)) then
    Exit;

  if BigNumberGetFlag(Num, BN_FLG_STATIC_DATA) <> 0 then
    Exit;

  A := PDWORD(GetMemory(SizeOf(MAXDWORD) * Words));
  if A = nil then
    Exit;

  ZeroMemory(A, SizeOf(MAXDWORD) * Words);

  // 查查是否要复制之前的值
  B := Num.D;
  if B <> nil then
  begin
    TmpA := A;
    I :=  Num.Top shr 2;
    while I > 0 do
    begin
      A0 := PDWordArray(B)^[0];
      A1 := PDWordArray(B)^[1];
      A2 := PDWordArray(B)^[2];
      A3 := PDWordArray(B)^[3];

      PDWordArray(TmpA)^[0] := A0;
      PDWordArray(TmpA)^[1] := A1;
      PDWordArray(TmpA)^[2] := A2;
      PDWordArray(TmpA)^[3] := A3;

      Dec(I);
      TmpA := PDWORD(Integer(TmpA) + SizeOf(DWORD));
      B := PDWORD(Integer(B) + SizeOf(DWORD));
    end;

    case Num.Top and 3 of
      3:
        begin
          PDWordArray(TmpA)^[2] := PDWordArray(B)^[2];
          PDWordArray(TmpA)^[1] := PDWordArray(B)^[1];
          PDWordArray(TmpA)^[0] := PDWordArray(B)^[0];
        end;
      2:
        begin
          PDWordArray(TmpA)^[1] := PDWordArray(B)^[1];
          PDWordArray(TmpA)^[0] := PDWordArray(B)^[0];
        end;
      1:
        begin
          PDWordArray(TmpA)^[0] := PDWordArray(B)^[0];
        end;
      0:
        begin
          ;
        end;
    end;
  end;

  Result := A;
end;

function BigNumberExpand2(var Num: TCnBigNumber; Words: Integer): PCnBigNumber;
var
  P: PDWORD;
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

    Result := @Num;
  end;
end;

function BigNumberWordExpand(var Num: TCnBigNumber; Words: Integer): PCnBigNumber;
begin
  if Words <= Num.DMax then
    Result := @Num
  else
    Result := BigNumberExpand2(Num, Words);
end;

function BigNumberExpandBits(var Num: TCnBigNumber; Bits: Integer): PCnBigNumber;
begin
  if ((Bits + BN_BITS2 - 1) div BN_BITS2) <= Num.DMax then
    Result := @Num
  else
    Result := BigNumberExpand2(Num, (Bits + BN_BITS2 - 1) div BN_BITS2);
end;

procedure BigNumberClear(var Num: TCnBigNumber);
begin
  if Num.D <> nil then
    ZeroMemory(Num.D, Num.DMax * SizeOf(DWORD));
  Num.Top := 0;
  Num.Neg := 0;
end;

procedure BigNumberClearFree(var Num: TCnBigNumber);
begin

end;

function BigNumberSetWord(var Num: TCnBigNumber; W: DWORD): Boolean;
begin
  Result := False;
  if BigNumberExpandBits(Num, SizeOf(DWORD) * 8) = nil then
    Exit;
  Num.Neg := 0;
  PDWordArray(Num.D)^[0] := W;
  if W <> 0 then
    Num.Top := 1
  else
    Num.Top := 0;
  Result := True;
end;

function BigNumberGetWord(var Num: TCnBigNumber): DWORD;
begin
  if Num.Top > 1 then
    Result := BN_MASK2
  else if Num.Top = 1 then
    Result := PDWordArray(Num.D)^[0]
  else
    Result := 0;
end;

// 某大数是否等于指定 DWORD
function BigNumberIsWord(var Num: TCnBigNumber; W: DWORD): Boolean;
begin
  Result := False;
  if (W = 0) or (Num.Neg = 0) then
    if BigNumberAbsIsWord(Num, W) then
      Result := True;
end;

// 调整 Top 保证 D[Top - 1] 指向最高位非 0 处
procedure BigNumberCorrectTop(var Num: TCnBigNumber);
var
  Ftl: PDWORD;
  Top: Integer;
begin
  Top := Num.Top;
  Ftl := @(PDWordArray(Num.D)^[Top - 1]);
  while Top > 0 do
  begin
    if Ftl^ <> 0 then
      Break;

    Ftl := PDWORD(Integer(Ftl) - SizeOf(DWORD));
    Dec(Top);
  end;
  Num.Top := Top;
end;

function BigNumberToBinary(var Num: TCnBigNumber; Buf: PAnsiChar): Integer;
var
  I, N: Integer;
  L: DWORD;
begin
  N := BigNumberGetBytesCount(Num);
  I := N;
  while I > 0 do
  begin
    L := PDWordArray(Num.D)^[I div BN_BYTES];
    Buf^ := Chr(L shr (8 * (I mod BN_BYTES)) and $FF);

    Dec(I);
    Buf := PAnsiChar(Integer(Buf) + 1);
  end;
  Result := N;
end;

function BigNumberFromBinary(Buf: PAnsiChar; Len: Integer): PCnBigNumber;
var
  I, M, N, L: DWORD;
begin
  Result := BigNumberNew;

  L := 0;
  N := Len;
  if N = 0 then
  begin
    Result^.Top := 0;
    Exit;
  end;

  I := ((N - 1) div BN_BYTES) + 1;
  M := (N - 1) mod BN_BYTES;

  if BigNumberWordExpand(Result^, I) = nil then
  begin
    BigNumberFree(Result);
    Result := nil;
    Exit;
  end;

  Result^.Top := I;
  Result^.Neg := 0;
  while N > 0 do
  begin
    L := (L shl 8) or Ord(Buf^);
    Buf := PAnsiChar(Integer(Buf) + 1);

    if M = 0 then
    begin
      Dec(I);
      PDWordArray(Result^.D)^[I] := L;
      L := 0;
      M := BN_BYTES - 1;
    end
    else
      Dec(M);

    Dec(N);
  end;
end;

procedure BigNumberSetNegative(var Num: TCnBigNumber; Negative: Boolean);
begin
  if BigNumberIsZero(Num) then
    Exit;
  if Negative then
    Num.Neg := 1
  else
    Num.Neg := 0;
end;

function BigNumberIsNegative(var Num: TCnBigNumber): Boolean;
begin
  Result := Num.Neg <> 0;
end;

function BigNumberClearBit(var Num: TCnBigNumber; N: Integer): Boolean;
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

  PDWordArray(Num.D)^[I] := PDWordArray(Num.D)^[I] and DWORD(not (1 shl J));
  BigNumberCorrectTop(Num);
  Result := True;
end;

function BigNumberSetBit(var Num: TCnBigNumber; N: Integer): Boolean;
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
      PDWordArray(Num.D)^[K] := 0;

    Num.Top := I + 1;
  end;

  PDWordArray(Num.D)^[I] := PDWordArray(Num.D)^[I] or DWORD(1 shl J);
  Result := True;
end;

function BigNumberIsBitSet(var Num: TCnBigNumber; N: Integer): Boolean;
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

  if (DWORD(PDWordArray(Num.D)^[I] shr J) and DWORD(1)) <> 0 then
    Result := True;
end;

function BigNumberCompareWords(var Num1: TCnBigNumber; var Num2: TCnBigNumber;
  N: Integer): Integer;
var
  I: Integer;
  A, B: DWORD;
begin
  A := PDWordArray(Num1.D)^[N - 1];
  B := PDWordArray(Num2.D)^[N - 1];

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
    A := PDWordArray(Num1.D)^[I];
    B := PDWordArray(Num2.D)^[I];

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

function BigNumberCompare(var Num1: TCnBigNumber; var Num2: TCnBigNumber): Integer;
var
  I, Gt, Lt: Integer;
  T1, T2: DWORD;
begin
//  if (Num1 = nil) or (Num2 = nil) then
//  begin
//    if Num1 <> nil then
//      Result := -1
//    else if Num2 <> nil then
//      Result := 1
//    else
//      Result := 0;
//
//    Exit;
//  end;

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
    T1 := PDWordArray(Num1.D)^[I];
    T2 := PDWordArray(Num2.D)^[I];
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

function BigNumberUnsignedCompare(var Num1: TCnBigNumber; var Num2: TCnBigNumber): Integer;
var
  I: Integer;
  T1, T2: DWORD;
begin
  Result := Num1.Top - Num2.Top;
  if Result <> 0 then
    Exit;

  for I := Num1.Top - 1 downto 0 do
  begin
    T1 := PDWordArray(Num1.D)^[I];
    T2 := PDWordArray(Num2.D)^[I];
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

function BigNumberToString(var Num: TCnBigNumber): string;
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
      V := ((PDWordArray(Num.D)^[I]) shr DWORD(J)) and $0F;
      if (Z <> 0) or (V <> 0) then
      begin
        Result := Result + Hex[V + 1];
        Z := 1;
      end;
      Dec(J, 4);
    end;
  end;
end;

function BigNumberToHex(var Num: TCnBigNumber): string;
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
      V := ((PDWordArray(Num.D)^[I]) shr DWORD(J)) and $FF;
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

// 使用 Windows API 实现区块随机填充
function InternalRandBytes(Buf: PAnsiChar; Len: Integer): Boolean;
var
  HProv: Cardinal;
begin
  HProv := 0;
  Result := False;
  if not CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, 0) then
    Exit;

  if HProv <> 0 then
  begin
    try
      Result := CryptGenRandom(HProv, Len, Buf);
//      if not Result then
//      begin
//        Ret := GetLastError;
//        Result := Ret <> 0;
//      end;
    finally
      CryptReleaseContext(HProv, 0);
    end;
  end;
end;

// 产生固定字节长度的随机大数
function BigNumberRandBytes(var Num: TCnBigNumber; BytesCount: Integer): Boolean;
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
    Result := InternalRandBytes(PAnsiChar(Num.D), BytesCount);
    if Result then
    begin
      Num.Top := (BytesCount + 3) div 4;
      BigNumberCorrectTop(Num);
    end;
  end;
end;

function BigNumberRandRange(var Num: TCnBigNumber; var Range: TCnBigNumber): Boolean;
var
  N: Integer;
begin
  Result := False;
  if (Range.Neg <> 0) or BigNumberIsZero(Range) then
    Exit;

  N := BigNumberGetBitsCount(Range);
  if N = 1 then
    BigNumberSetZero(Num)
  else if (not BigNumberIsBitSet(Range, N - 2))
    and (not BigNumberIsBitSet(Range, N - 3)) then
  begin
    // TODO: CONTINUE
  end
end;

function BigNumberDuplicate(var Num: TCnBigNumber): PCnBigNumber;
begin
  Result := BigNumberNew;
  if Result = nil then
    Exit;

  if BigNumberCopy(Result^, Num) = nil then
  begin
    BigNumberFree(Result);
    Result := nil;
  end;
end;

function BigNumberCopy(var Dst: TCnBigNumber; var Src: TCnBigNumber): PCnBigNumber;
var
  I: Integer;
  A, B: PDWordArray;
  A0, A1, A2, A3: DWORD;
begin
  if @Dst = @Src then
  begin
    Result := @Dst;
    Exit;
  end;

  if BigNumberWordExpand(Dst, Src.Top) = nil then
  begin
    Result := nil;
    Exit;
  end;

  A := PDWordArray(Dst.D);
  B := PDWordArray(Src.D);

  for I := (Src.Top shr 2) downto 1 do
  begin
    A0 := B[0]; A1 := B[1]; A2 := B[2]; A3 := B[3];
    A[0] := A0; A[1] := A1; A[2] := A2; A[3] := A3;

    A := PDWordArray(Integer(A) + 4 * SizeOf(DWORD));
    B := PDWordArray(Integer(B) + 4 * SizeOf(DWORD));
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
  Result := @Dst;
end;

procedure BigNumberSwap(var Num1: TCnBigNumber; var Num2: TCnBigNumber);
var
  OldFlag1, OldFlag2: DWORD;
  TmpD: PDWORD;
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

function LBITS(Num: DWORD): DWORD;
begin
  Result := Num and BN_MASK2l;
end;

function HBITS(Num: DWORD): DWORD;
begin
  Result := (Num shr BN_BITS4) and BN_MASK2l;
end;

function L2HBITS(Num: DWORD): DWORD;
begin
  Result := (Num shl BN_BITS4) and BN_MASK2;
end;

// 计算 BL * BH，结果的高低位分别放 H 和 L
procedure Mul64(var L: DWORD; var H: DWORD; var BL: DWORD; var BH: DWORD);
var
  M, M1, LT, HT: DWORD;
begin
  LT := L;
  HT := H;
  M := BH * LT;
  LT := BL * LT;
  M1 := BL * HT;
  HT := BH * HT;
  M := (M + M1) and BN_MASK2;
  if M < M1 then
    HT := HT + L2HBITS(DWORD(1));
  HT := HT + HBITS(M);
  M1 := L2HBITS(M);
  LT := (LT + M1) and BN_MASK2;
  if LT < M1 then
    Inc(HT);
  L := LT;
  H := HT;
end;

// 计算 InNum 的平方，结果的高低位分别放 Ho 和 Lo
procedure Sqr64(var Lo: DWORD; var Ho: DWORD; var InNum: DWORD);
var
  L, H, M: DWORD;
begin
  H := InNum;
  L := LBITS(H);
  H := HBITS(H);
  M := L * H;
  L := L * L;
  H := H * H;
  H := H + ((M and BN_MASK2h1) shr (BN_BITS4 - 1));
  M := (M and BN_MASK2l) shl (BN_BITS4 + 1);
  L := (L + M) and BN_MASK2;
  if L < M then
    Inc(H);
  Lo := L;
  Ho := H;
end;

procedure MulAdd(var R: DWORD; var A: DWORD; var BL: DWORD; var BH: DWORD; var C: DWORD);
var
  L, H: DWORD;
begin
  H := A;
  L := LBITS(H);
  H := HBITS(H);
  Mul64(L, H, BL, BH);

  L := (L + C) and BN_MASK2;
  if L < C then
    Inc(H);
  C := R;
  L := (L + C) and BN_MASK2;
  if L < C then
    Inc(H);
  C := H and BN_MASK2;
  R := L;
end;

procedure Mul(var R: DWORD; var A: DWORD; var BL: DWORD; var BH: DWORD; var C: DWORD);
var
  L, H: DWORD;
begin
  H := A;
  L := LBITS(H);
  H := HBITS(H);
  Mul64(L, H, BL ,BH);

  L := L + C;
  if (L and BN_MASK2) < C then
    Inc(H);
  C := H and BN_MASK2;
  R := L and BN_MASK2;
end;

{* Words 系列内部计算函数开始 }

function BigNumberAddWords(RP: PDWordArray; AP: PDWordArray; BP: PDWordArray; N: Integer): DWORD;
var
  LL: LONGLONG;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  LL := 0;
  while (N and (not 3)) <> 0 do
  begin
    LL := LL + LONGLONG(AP[0]) + LONGLONG(BP[0]);
    RP[0] := DWORD(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + LONGLONG(AP[1]) + LONGLONG(BP[1]);
    RP[1] := DWORD(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + LONGLONG(AP[2]) + LONGLONG(BP[2]);
    RP[2] := DWORD(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    LL := LL + LONGLONG(AP[3]) + LONGLONG(BP[3]);
    RP[3] := DWORD(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PDWordArray(Integer(AP) + 4 * SizeOf(DWORD));
    BP := PDWordArray(Integer(BP) + 4 * SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + 4 * SizeOf(DWORD));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    LL := LL + LONGLONG(AP[0]) + LONGLONG(BP[0]);
    RP[0] := DWORD(LL) and BN_MASK2;
    LL := LL shr BN_BITS2;

    AP := PDWordArray(Integer(AP) + SizeOf(DWORD));
    BP := PDWordArray(Integer(BP) + SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + SizeOf(DWORD));
    Dec(N);
  end;
  Result := DWORD(LL);
end;

function BigNumberSubWords(RP: PDWordArray; AP: PDWordArray; BP: PDWordArray; N: Integer): DWORD;
var
  T1, T2, C: DWORD;
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

    AP := PDWordArray(Integer(AP) + 4 * SizeOf(DWORD));
    BP := PDWordArray(Integer(BP) + 4 * SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + 4 * SizeOf(DWORD));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    T1 := AP^[0];
    T2 := BP^[0];
    RP^[0] := (T1 - T2 - C) and BN_MASK2;
    if T1 <> T2 then
      if T1 < T2 then C := 1 else C := 0;

    AP := PDWordArray(Integer(AP) + SizeOf(DWORD));
    BP := PDWordArray(Integer(BP) + SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + SizeOf(DWORD));
    Dec(N);
  end;
  Result := C;
end;

function BigNumberMulAddWords(RP: PDWordArray; AP: PDWordArray; N: Integer; W: DWORD): DWORD;
var
  BL, BH: DWORD;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  BL := LBITS(W);
  BH := HBITS(W);

  while (N and (not 3)) <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], BL, BH, Result);
    MulAdd(RP^[1], AP^[1], BL, BH, Result);
    MulAdd(RP^[2], AP^[2], BL, BH, Result);
    MulAdd(RP^[3], AP^[3], BL, BH, Result);

    AP := PDWordArray(Integer(AP) + 4 * SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + 4 * SizeOf(DWORD));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    MulAdd(RP^[0], AP^[0], BL, BH, Result);
    AP := PDWordArray(Integer(AP) + SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + SizeOf(DWORD));
    Dec(N);
  end;
end;

function BigNumberMulWords(RP: PDWordArray; AP: PDWordArray; N: Integer; W: DWORD): DWORD;
var
  Carry, BL, BH: DWORD;
begin
  Result := 0;
  if N <= 0 then
    Exit;

  BL := LBITS(W);
  BH := HBITS(W);

  Carry := 0;
  while (N and (not 3)) <> 0 do
  begin
    Mul(RP^[0], AP^[0], BL, BH, Carry);

    AP := PDWordArray(Integer(AP) + 4 * SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + 4 * SizeOf(DWORD));

    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Mul(RP^[0], AP^[0], BL, BH, Carry);
    AP := PDWordArray(Integer(AP) + SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + SizeOf(DWORD));

    Dec(N);
  end;
  Result := Carry;
end;

procedure BigNumberSqrWords(RP: PDWordArray; AP: PDWordArray; N: Integer);
begin
  if N = 0 then
    Exit;

  while (N and (not 3)) <> 0 do
  begin
    Sqr64(RP^[0], RP^[1], AP^[0]);
    Sqr64(RP^[2], RP^[3], AP^[1]);
    Sqr64(RP^[4], RP^[5], AP^[2]);
    Sqr64(RP^[6], RP^[7], AP^[3]);

    AP := PDWordArray(Integer(AP) + 4 * SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + 8 * SizeOf(DWORD));
    Dec(N, 4);
  end;

  while N <> 0 do
  begin
    Sqr64(RP^[0], RP^[1], AP^[0]);
    AP := PDWordArray(Integer(AP) + SizeOf(DWORD));
    RP := PDWordArray(Integer(RP) + 2 * SizeOf(DWORD));
    Dec(N);
  end;
end;

// 64 位被除数整除 32 位除数，返回商，Result := H L div D
function BigNumberDivWords(H: DWORD; L: DWORD; D: DWORD): DWORD;
var
  I, Count: Integer;
  DH, DL, Q, TH, TL, T: DWORD;
begin
  if D = 0 then
  begin
    Result := BN_MASK2;
    Exit;
  end;

  Result := 0;
  I := BigNumberGetWordBitsCount(D);
  if (I <> BN_BITS2) and (H > DWORD(1 shl I)) then
    Exit;

  I := BN_BITS2 - I;
  if H >= D then
    H := H - D;

  if I <> 0 then
  begin
    D := D shl I;
    H := (H shl I) or (L shr (BN_BITS2 - I));
    L := L shl I;
  end;

  DH := (D and BN_MASK2h) shr BN_BITS4;
  DL := (D and BN_MASK2l);

  Count := 2;
  Q := 0;
  while True do
  begin
    if (H shr BN_BITS4) = DH then
      Q := BN_MASK2l
    else
      Q := H div DH;

    TH := Q * DH;
    TL := DL * Q;

    while True do
    begin
      T := H - TH;
      if ((T and BN_MASK2h) <> 0) or
        (TL <= ((T shl BN_BITS4) or ((L and BN_MASK2h) shr BN_BITS4))) then
        Break;
      Dec(Q);
      TH := TH - DH;
      TL := TL - DL;
    end;

    T := TL shr BN_BITS4;
    TL := (TL shl BN_BITS4) and BN_MASK2h;
    TH := TH + T;

    if L < TL then
      Inc(TH);
    if H < TH then
    begin
      H := H + D;
      Dec(Q);
    end;
    H := H - TH;

    Dec(Count);
    if Count = 0 then
      Break;

    Result := Q shl BN_BITS4;
    H := ((H shl BN_BITS4) or (L shr BN_BITS4)) and BN_MASK2;
    L := (L and BN_MASK2l) shl BN_BITS4;
  end;

  Result := Result or Q;
end;

{*  Words 系列内部计算函数结束 }

function BigNumberUnsignedAdd(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif: Integer;
  AP, BP, RP: PDWORD;
  Carry, T1, T2: DWORD;
  A, B, Tmp: PCnBigNumber;
begin
  Result := False;

  A := @Num1;
  B := @Num2;
  if A^.Top < B^.Top then
  begin
    Tmp := A;
    A := B;
    B := Tmp;
  end;

  Max := A^.Top;
  Min := B^.Top;
  Dif := Max - Min;

  if BigNumberWordExpand(Res, Max + 1) = nil then
    Exit;

  Res.Top := Max;
  AP := PDWORD(A^.D);
  BP := PDWORD(B^.D);
  RP := PDWORD(Res.D);

  Carry := BigNumberAddWords(PDWordArray(RP), PDWordArray(AP), PDWordArray(BP), Min);

  AP := PDWORD(Integer(AP) + Min * SizeOf(DWORD));
//  BP := PDWORD(Integer(BP) + Min * SizeOf(DWORD));
  RP := PDWORD(Integer(RP) + Min * SizeOf(DWORD));

  if Carry <> 0 then
  begin
    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PDWORD(Integer(AP) + SizeOf(DWORD));
      T2 := (T1 + 1) and BN_MASK2;

      RP^ := T2;
      RP := PDWORD(Integer(RP) + SizeOf(DWORD));

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
      AP := PDWORD(Integer(AP) + SizeOf(DWORD));
      RP := PDWORD(Integer(RP) + SizeOf(DWORD));
    end;
  end;

  Res.Neg := 0;
  Result := True;
end;

function BigNumberUnsignedSub(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
var
  Max, Min, Dif, I: Integer;
  AP, BP, RP: PDWORD;
  Carry, T1, T2: DWORD;
begin
  Result := False;

  Max := Num1.Top;
  Min := Num2.Top;
  Dif := Max - Min;

  if Dif < 0 then
    Exit;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  AP := PDWORD(Num1.D);
  BP := PDWORD(Num2.D);
  RP := PDWORD(Res.D);

  Carry := 0;
  for I := Min downto 1 do
  begin
    T1 := AP^;
    T2 := BP^;
    AP := PDWORD(Integer(AP) + SizeOf(DWORD));
    BP := PDWORD(Integer(BP) + SizeOf(DWORD));
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
    RP := PDWORD(Integer(RP) + SizeOf(DWORD));
  end;

  if Carry <> 0 then
  begin
    if Dif = 0 then  // Error! Num1 < Num2
      Exit;

    while Dif <> 0 do
    begin
      Dec(Dif);
      T1 := AP^;
      AP := PDWORD(Integer(AP) + SizeOf(DWORD));
      T2 := (T1 - 1) and BN_MASK2;

      RP^ := T2;
      RP := PDWORD(Integer(RP) + SizeOf(DWORD));
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
      AP := PDWORD(Integer(AP) + SizeOf(DWORD));
      RP := PDWORD(Integer(RP) + SizeOf(DWORD));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PDWORD(Integer(AP) + SizeOf(DWORD));
      RP := PDWORD(Integer(RP) + SizeOf(DWORD));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PDWORD(Integer(AP) + SizeOf(DWORD));
      RP := PDWORD(Integer(RP) + SizeOf(DWORD));

      if Dif = 0 then Break;
      Dec(Dif);
      RP^ := AP^;
      AP := PDWORD(Integer(AP) + SizeOf(DWORD));
      RP := PDWORD(Integer(RP) + SizeOf(DWORD));
    end;
  end;

  Res.Top := Max;
  Res.Neg := 0;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberAdd(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: PCnBigNumber;
  Neg: Integer;
begin
  Result := False;
  
  Neg := Num1.Neg;
  A := @Num1;
  B := @Num2;

  if Neg <> Num2.Neg then // One is negative
  begin
    if Neg <> 0 then
    begin
      Tmp := A;
      A := B;
      B := Tmp;
    end;

    // A is positive and B is negative
    if BigNumberUnsignedCompare(A^, B^) < 0 then
    begin
      if not BigNumberUnsignedSub(Res, B^, A^) then
        Exit;
      Res.Neg := 1;
    end
    else
    begin
      if not BigNumberUnsignedSub(Res, A^, B^) then
        Exit;
      Res.Neg := 0;
    end;
    Result := True;
    Exit;
  end;

  Result := BigNumberUnsignedAdd(Res, A^, B^);
  Res.Neg := Neg;
end;

function BigNumberSub(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
var
  A, B, Tmp: PCnBigNumber;
  Max, Add, Neg: Integer;
begin
  Result := False;
  Add := 0;
  Neg := 0;
  A := @Num1;
  B := @Num2;

  if A^.Neg <> 0 then
  begin
    if B^.Neg <> 0 then
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
    if B^.Neg <> 0 then // A Positive B Negative
    begin
      Add := 1;
      Neg := 0;
    end;
  end;

  if Add = 1 then
  begin
    if not BigNumberUnsignedAdd(Res, A^, B^) then
      Exit;

    Res.Neg := Neg;
    Result := True;
    Exit;
  end;

  if A^.Top > B^.Top then
    Max := A^.Top
  else
    Max := B^.Top;

  if BigNumberWordExpand(Res, Max) = nil then
    Exit;

  if BigNumberUnsignedCompare(A^, B^) < 0 then
  begin
    if not BigNumberUnsignedSub(Res, B^, A^) then
      Exit;
    Res.Neg := 1;
  end
  else
  begin
    if not BigNumberUnsignedSub(Res, A^, B^) then
      Exit;
    Res.Neg := 0;
  end;
  Result := True;
end;

function BigNumberShiftLeftOne(var Res: TCnBigNumber; var Num: TCnBigNumber): Boolean;
var
  RP, AP: PDWORD;
  I: Integer;
  T, C: DWORD;
begin
  Result := False;

  if @Res <> @Num then
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
    AP := PDWORD(Integer(AP) + SizeOf(DWORD));
    RP^ := ((T shl 1) or C) and BN_MASK2;
    RP := PDWORD(Integer(RP) + SizeOf(DWORD));

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

function BigNumberShiftRightOne(var Res: TCnBigNumber; var Num: TCnBigNumber): Boolean;
var
  RP, AP: PDWORD;
  I, J: Integer;
  T, C: DWORD;
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

  if PDWordArray(AP)^[I - 1] = 1 then
    J := I - 1
  else
    J := I;

  if @Res <> @Num then
  begin
    if BigNumberWordExpand(Res, J) = nil then
      Exit;
    Res.Neg := Num.Neg;
  end;

  RP := Res.D;
  Dec(I);
  T := PDWordArray(AP)^[I];

  if (T and 1) <> 0 then
    C := BN_TBIT
  else
    C := 0;

  T := T shr 1;
  if T <> 0 then
    PDWordArray(RP)^[I] := T;

  while I > 0 do
  begin
    Dec(I);
    T := PDWordArray(AP)^[I];
    PDWordArray(RP)^[I] := ((T shr 1) and BN_MASK2) or C;

    if (T and 1) <> 0 then
      C := BN_TBIT
    else
      C := 0;
  end;

  Res.Top := J;
  Result := True;
end;

function BigNumberShiftLeft(var Res: TCnBigNumber; var Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, NW, LB, RB: Integer;
  L: DWORD;
  T, F: PDWordArray;
begin
  Result := False;
  Res.Neg := Num.Neg;
  NW := N div BN_BITS2;

  if BigNumberWordExpand(Res, Num.Top + NW + 1) = nil then
    Exit;

  LB := N mod BN_BITS2;
  RB := BN_BITS2 - LB;

  F := PDWordArray(Num.D);
  T := PDWordArray(Res.D);

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

  ZeroMemory(Pointer(T), NW * SizeOf(DWORD));
  Res.Top := Num.Top + NW + 1;
  BigNumberCorrectTop(Res);
  Result := True;
end;

function BigNumberShiftRight(var Res: TCnBigNumber; var Num: TCnBigNumber;
  N: Integer): Boolean;
var
  I, J, NW, LB, RB: Integer;
  L, Tmp: DWORD;
  T, F: PDWordArray;
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
  if @Res <> @Num then
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

  F := PDWordArray(Integer(Num.D) + NW * SizeOf(DWORD));
  T := PDWordArray(Res.D);
  J := Num.Top - NW;
  Res.Top := I;

  if RB = 0 then
  begin
    for I := J downto 1 do
    begin
      T^[0] := F^[0];
      F := PDWordArray(Integer(F) + SizeOf(DWORD));
      T := PDWordArray(Integer(T) + SizeOf(DWORD));
    end;
  end
  else
  begin
    L := F^[0];
    F := PDWordArray(Integer(F) + SizeOf(DWORD));
    for I := J - 1 downto 1 do
    begin
      Tmp := (L shr RB) and BN_MASK2;
      L := F^[0];
      T^[0] := (Tmp or (L shl LB)) and BN_MASK2;

      F := PDWordArray(Integer(F) + SizeOf(DWORD));
      T := PDWordArray(Integer(T) + SizeOf(DWORD));
    end;

    L := (L shr RB) and BN_MASK2;
    if L <> 0 then
      T^[0] := L;
  end;
  Result := True;
end;

function BigNumberModWord(var Num: TCnBigNumber; W: DWORD): DWORD;
var
  I: Integer;
begin
  if W = 0 then
  begin
    Result := DWORD(-1);
    Exit;
  end;

  Result := 0;
  W := W and BN_MASK2;
  for I := Num.Top - 1 downto 0 do
  begin
    Result := ((Result shl BN_BITS4) or ((PDWordArray(Num.D)^[I] shr BN_BITS4) and BN_MASK2l)) mod W;
    Result := ((Result shl BN_BITS4) or (PDWordArray(Num.D)^[I] and BN_MASK2l)) mod W;
  end;
end;

function BigNumberDivWord(var Num: TCnBigNumber; W: DWORD): DWORD;
var
  I, J: Integer;
  L, D: DWORD;
begin
  if W = 0 then
  begin
    Result := DWORD(-1);
    Exit;
  end;

  Result := 0;
  if Num.Top = 0 then
    Exit;

  W := W and BN_MASK2;
  J := BN_BITS2 - BigNumberGetWordBitsCount(W);

  W := W shl J;
  if not BigNumberShiftLeft(Num, Num, J) then
  begin
    Result := DWORD(-1);
    Exit;
  end;

  for I := Num.Top - 1 downto 0 do
  begin
    L := PDWordArray(Num.D)^[I];
    D := BigNumberDivWords(Result, L, W);
    Result := (L - ((D * W) and BN_MASK2)) and BN_MASK2;

    PDWordArray(Num.D)^[I] := D;
  end;

  if (Num.Top > 0) and (PDWordArray(Num.D)^[Num.Top - 1] = 0) then
    Dec(Num.Top);
  Result := Result shr J;
end;

{* BigNumberPool 双向链表池操作函数开始 }

// 初始化一 BigNumberPool
procedure BigNumberPoolInit(var Pool: TBigNumberPool);
begin
  with Pool do
  begin
    Head := nil;
    Current := nil;
    Tail := nil;
    Used := 0;
    Size := 0;
  end;
end;

// 遍历并释放一 BigNumberPool 内的所有元素
procedure BigNumberPoolFinish(var Pool: TBigNumberPool);
var
  I: Integer;
begin
  while Pool.Head <> nil do
  begin
    // 只会释放 D 内存而不释放大数结构本身，因为没有 MALLOC 标志
    for I := 0 to BN_CTX_POOL_SIZE - 1 do
      BigNumberFree(@(Pool.Head.Vals[I]));

    Pool.Current := Pool.Head.Next;
    FreeMemory(Pool.Head);
    Pool.Head := Pool.Current;
  end;
end;

procedure BigNumberPoolReset(var Pool: TBigNumberPool);
var
  Item: PBigNumberPoolItem;
  I: Integer;
begin
  Item := Pool.Head;
  while Item <> nil do
  begin
    for I := 0 to BN_CTX_POOL_SIZE - 1 do
      BigNumberClear(Item.Vals[I]);

    Item := Item.Next;
  end;

  Pool.Current := Pool.Head;
  Pool.Used := 0;
end;

// 从池中分配并取出一个大数结构地址
function BigNumberPoolGet(var Pool: TBigNumberPool): PCnBigNumber;
var
  I: Integer;
  Item: PBigNumberPoolItem;
begin
  if Pool.Used = Pool.Size then
  begin
    // This Item is Full. Get another
    New(Item);
    for I := 0 to BN_CTX_POOL_SIZE - 1 do
      BigNumberInit(Item.Vals[I]);

    Item.Prev := Pool.Tail;
    Item.Next := nil;

    if Pool.Head = nil then
    begin
      Pool.Head := Item;
      Pool.Current := Item;
      Pool.Tail := Item;
    end
    else
    begin
      Pool.Tail.Next := Item;
      Pool.Tail := Item;
      Pool.Current := Item;
    end;

    Inc(Pool.Size, BN_CTX_POOL_SIZE);
    Inc(Pool.Used);
    Result := @(Item.Vals[0]);
    Exit;
  end;

  if Pool.Used = 0 then
    Pool.Current := Pool.Head
  else if (Pool.Used mod BN_CTX_POOL_SIZE) = 0 then
    Pool.Current := Pool.Current.Next;

  Result := @(Pool.Current.Vals[Pool.Used mod BN_CTX_POOL_SIZE]);
  Inc(Pool.Used);
end;

// 从池尾部缩小 Num 个大数结构，仅作标记，不释放内存
procedure BigNumberPoolRelease(var Pool: TBigNumberPool; Num: Integer);
var
  Offset: Integer;
begin
  Offset := (Pool.Used - 1) mod BN_CTX_POOL_SIZE;
  Dec(Pool.Used, Num);
  while Num <> 0 do
  begin
    if Offset = 0 then
    begin
      Offset := BN_CTX_POOL_SIZE - 1;
      Pool.Current := Pool.Current.Prev;
    end
    else
      Dec(Offset);

    Dec(Num);
  end;
end;

{* BigNumberPool 双向链表池操作函数结束 }

{* BigNumberStack 堆栈操作函数开始 }

// 初始化一个大数堆栈
procedure BigNumberStackInit(var Stack: TBigNumberStack);
begin
  Stack.Indexes := nil;
  Stack.Depth := 0;
  Stack.Size := 0;
end;

// 释放一个大数堆栈的内部存储区
procedure BigNumberStackFinish(var Stack: TBigNumberStack);
begin
  if Stack.Size > 0 then
    FreeMemory(Stack.Indexes);
end;

// 重置大数堆栈
procedure BigNumberStackReset(var Stack: TBigNumberStack);
begin
  Stack.Depth := 0;
end;

// 将一个数据推入堆栈
function BigNumberStackPush(var Stack: TBigNumberStack; Idx: DWORD): Boolean;
var
  NewSize: Integer;
  NewItems: PDWORD;
begin
  Result := False;
  if Stack.Depth = Stack.Size then
  begin
    if Stack.Size = 0 then
      NewSize := BN_CTX_START_FRAMES
    else
      NewSize := (Stack.Size * 3) div 2;

    NewItems := PDWORD(GetMemory(NewSize * SizeOf(DWORD)));
    if NewItems = nil then
      Exit;

    if Stack.Depth > 0 then
      CopyMemory(NewItems, Stack.Indexes, Stack.Depth * SizeOf(DWORD));
    if Stack.Size > 0 then
      FreeMemory(Stack.Indexes);

    Stack.Indexes := NewItems;
    Stack.Size := NewSize;
  end;

  PDWordArray(Stack.Indexes)^[Stack.Depth] := Idx;
  Inc(Stack.Depth);
  Result := True;
end;

// 从堆栈中弹出
function BigNumberStackPop(var Stack: TBigNumberStack): DWORD;
begin
  Dec(Stack.Depth);
  Result := PDWordArray(Stack.Indexes)^[Stack.Depth];
end;

{* BigNumberStack 堆栈操作函数结束 }

{* BigNumberContext 中间结构操作函数开始 }

procedure BigNumberContextInit(var Ctx: TBigNumberContext);
begin
  BigNumberPoolReset(Ctx.Pool);
  BigNumberStackReset(Ctx.Stack);
  Ctx.Used := 0;
  Ctx.ErrStack := 0;
  Ctx.TooMany := 0;
end;

function BigNumberContextNew: PBigNumberContext;
begin
  New(Result);
  if Result = nil then
    Exit;
  BigNumberPoolInit(Result^.Pool);
  BigNumberStackInit(Result^.Stack);
  Result^.Used := 0;
  Result^.ErrStack := 0;
  Result^.TooMany := 0;
end;

procedure BigNumberContextFree(Ctx: PBigNumberContext);
begin
  if Ctx <> nil then
  begin
    BigNumberStackFinish(Ctx^.Stack);
    BigNumberPoolFinish(Ctx^.Pool);
    FreeMemory(Ctx);
  end;
end;

procedure BigNumberContextStart(var Ctx: TBigNumberContext);
begin
  if (Ctx.ErrStack <> 0) or (Ctx.TooMany <> 0) then
    Inc(Ctx.ErrStack)
  else if not BigNumberStackPush(Ctx.Stack, Ctx.Used) then
    Inc(Ctx.ErrStack);
end;

procedure BigNumberContextEnd(var Ctx: TBigNumberContext);
var
  FP: DWORD;
begin
  if Ctx.ErrStack <> 0 then
    Dec(Ctx.ErrStack)
  else
  begin
    FP := BigNumberStackPop(Ctx.Stack);
    if FP < Ctx.Used then
      BigNumberPoolRelease(Ctx.Pool, Ctx.Used - FP);
    Ctx.Used := FP;
    Ctx.TooMany := 0;
  end;
end;

function BigNumberContextGet(var Ctx: TBigNumberContext): PCnBigNumber;
begin
  Result := nil;
  if (Ctx.ErrStack <> 0) or (Ctx.TooMany <> 0) then
    Exit;

  Result := BigNumberPoolGet(Ctx.Pool);
  if Result = nil then
  begin
    Ctx.TooMany := 1;
    Exit;
  end;

  BigNumberSetZero(Result^);
  Inc(Ctx.Used);
end;

{* BigNumberContext 中间结构操作函数结束 }

// Tmp should have 2 * N DWORDs
procedure BigNumberSqrNormal(R: PDWORD; A: PDWORD; N: Integer; Tmp: PDWORD);
var
  I, J, Max: Integer;
  AP, RP: PDWordArray;
begin
  Max := N * 2;
  AP := PDWordArray(A);
  RP := PDWordArray(R);
  RP[0] := 0;
  RP[Max - 1] := 0;

  RP := PDWordArray(Integer(RP) + SizeOf(DWORD));
  J := N - 1;

  if J > 0 then
  begin
    AP := PDWordArray(Integer(AP) + SizeOf(DWORD));
    RP[J] := BigNumberMulWords(RP, AP, J, PDWordArray(Integer(AP) - SizeOf(DWORD))^[0]);
    RP := PDWordArray(Integer(RP) + 2 * SizeOf(DWORD));
  end;

  for I := N - 2 downto 1 do
  begin
    Dec(J);
    AP := PDWordArray(Integer(AP) + SizeOf(DWORD));
    RP[J] := BigNumberMulAddWords(RP, AP, J, PDWordArray(Integer(AP) - SizeOf(DWORD))^[0]);
    RP := PDWordArray(Integer(RP) + 2 * SizeOf(DWORD));
  end;

  BigNumberAddWords(PDWordArray(R), PDWordArray(R), PDWordArray(R), Max);
  BigNumberSqrWords(PDWordArray(Tmp), PDWordArray(A), N);
  BigNumberAddWords(PDWordArray(R), PDWordArray(R), PDWordArray(Tmp), Max);
end;

function BigNumberSqr(var Res: TCnBigNumber; var Num: TCnBigNumber): Boolean;
var
  Ctx: PBigNumberContext;
  Max, AL: Integer;
  Tmp, RR: PCnBigNumber;
  T: array[0..15] of DWORD;
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

  Ctx := BigNumberContextNew;
  BigNumberContextStart(Ctx^);

  try
    if @Num <> @Res then
      RR := @Res
    else
      RR := BigNumberContextGet(Ctx^);

    Tmp := BigNumberContextGet(Ctx^);
    if (RR = nil) or (Tmp = nil) then
      Exit;

    Max := 2 * AL;
    if BigNumberWordExpand(RR^, Max) = nil then
      Exit;

    if AL = 4 then
    begin
      BigNumberSqrNormal(RR^.D, Num.D, 4, @(T[0]));
    end
    else if AL = 8 then
    begin
      BigNumberSqrNormal(RR^.D, Num.D, 8, @(T[0]));
    end
    else
    begin
      if BigNumberWordExpand(Tmp^, Max) = nil then
        Exit;
      BigNumberSqrNormal(RR^.D, Num.D, AL, Tmp^.D);
    end;

    RR^.Neg := 0;
    if PDWordArray(Num.D)^[AL - 1] = (PDWordArray(Num.D)^[AL - 1] and BN_MASK2l) then
      RR^.Top := Max - 1
    else
      RR^.Top := Max;

    if RR <> @Res then
      BigNumberCopy(Res, RR^);
    Result := True;
  finally
    BigNumberContextEnd(Ctx^);
    BigNumberContextFree(Ctx);
  end;
end;

procedure BigNumberMulNormal(R: PDWORD; A: PDWORD; NA: Integer; B: PDWORD;
  NB: Integer);
var
  RR: PDWORD;
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

  RR := PDWORD(Integer(R) + NA * SizeOf(DWORD));
  if NB <= 0 then
  begin
    BigNumberMulWords(PDWordArray(R), PDWordArray(A), NA, 0);
    Exit;
  end
  else
    RR^ := BigNumberMulWords(PDWordArray(R), PDWordArray(A), NA, B^);

  while True do
  begin
    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PDWORD(Integer(RR) + SizeOf(DWORD));
    R := PDWORD(Integer(R) + SizeOf(DWORD));
    B := PDWORD(Integer(B) + SizeOf(DWORD));

    RR^ := BigNumberMulAddWords(PDWordArray(R), PDWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PDWORD(Integer(RR) + SizeOf(DWORD));
    R := PDWORD(Integer(R) + SizeOf(DWORD));
    B := PDWORD(Integer(B) + SizeOf(DWORD));
    RR^ := BigNumberMulAddWords(PDWordArray(R), PDWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PDWORD(Integer(RR) + SizeOf(DWORD));
    R := PDWORD(Integer(R) + SizeOf(DWORD));
    B := PDWORD(Integer(B) + SizeOf(DWORD));
    RR^ := BigNumberMulAddWords(PDWordArray(R), PDWordArray(A), NA, B^);

    Dec(NB);
    if NB <=0 then
      Exit;
    RR := PDWORD(Integer(RR) + SizeOf(DWORD));
    R := PDWORD(Integer(R) + SizeOf(DWORD));
    B := PDWORD(Integer(B) + SizeOf(DWORD));
    RR^ := BigNumberMulAddWords(PDWordArray(R), PDWordArray(A), NA, B^);
  end;
end;

function BigNumberMul(var Res: TCnBigNumber; var Num1: TCnBigNumber;
  var Num2: TCnBigNumber): Boolean;
var
  Ctx: PBigNumberContext;
  Top, AL, BL: Integer;
  RR: PCnBigNumber;
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

  Ctx := BigNumberContextNew;
  BigNumberContextStart(Ctx^);

  try
    if (@Res = @Num1) or (@Res = @Num2) then
    begin
      RR := BigNumberContextGet(Ctx^);
      if RR = nil then
        Exit;
    end
    else
      RR := @Res;

    if Num1.Neg <> Num2.Neg then
      RR^.Neg := 1
    else
      RR^.Neg := 0;

    if BigNumberWordExpand(RR^, Top) = nil then
      Exit;
    RR^.Top := Top;
    BigNumberMulNormal(RR^.D, Num1.D, AL, Num2.D, BL);

    if RR <> @Res then
      BigNumberCopy(Res, RR^);
    Result := True;
  finally
    BigNumberContextEnd(Ctx^);
    BigNumberContextFree(Ctx);
  end;
end;

function BigNumberDiv(var Res: TCnBigNumber; var Remain: TCnBigNumber;
  var Num: TCnBigNumber; var Divisor: TCnBigNumber): Boolean;
var
  NoBranch: Integer;
  Ctx: PBigNumberContext;
  Tmp, SNum, SDiv, SRes: PCnBigNumber;
  I, NormShift, Loop, NumN, DivN, Neg: Integer;
  D0, D1, Q, L0, N0, N1, Rem, T2L, T2H, QL, QH: DWORD;
  Resp, WNump: PDWORD;
  WNum: TCnBigNumber;
begin
  Result := False;
  if (Num.Top > 0) and (PDWordArray(Num.D)^[Num.Top - 1] = 0) then
    Exit;

  if BigNumberIsZero(Divisor) then
    Exit;

  if (BigNumberGetFlag(Num, BN_FLG_CONSTTIME) <> 0) or
    (BigNumberGetFlag(Divisor, BN_FLG_CONSTTIME) <> 0) then
    NoBranch := 1
  else
    NoBranch := 0;

  if (NoBranch = 0) and (BigNumberUnsignedCompare(Num, Divisor) < 0) then
  begin
    if BigNumberCopy(Remain, Num) = nil then
      Exit;
    BigNumberSetZero(Res);
    Result := True;
    Exit;
  end;

  Ctx := BigNumberContextNew;
  BigNumberContextStart(Ctx^);
  
  try
    Tmp := BigNumberContextGet(Ctx^);
    SNum := BigNumberContextGet(Ctx^);
    SDiv := BigNumberContextGet(Ctx^);
    SRes := @Res;

    if (Tmp = nil) or (SNum = nil) or (SDiv = nil) or (SRes = nil) then
      Exit;

    // 把除数左移到最高位是 1，放入SDiv
    NormShift := BN_BITS2 - (BigNumberGetBitsCount(Divisor) mod BN_BITS2);
    if not BigNumberShiftLeft(SDiv^, Divisor, NormShift) then
      Exit;

    SDiv^.Neg := 0;
    // 把被除数同样左移，并再左移一个字
    NormShift := NormShift + BN_BITS2;
    if not BigNumberShiftLeft(SNum^, Num, NormShift) then
      Exit;
    SNum^.Neg := 0;

    if NoBranch <> 0 then
    begin
      if SNum^.Top <= SDiv^.Top + 1 then
      begin
        if BigNumberWordExpand(SNum^, SDiv^.Top + 2) = nil then
          Exit;
        for I := SNum^.Top to SDiv^.Top + 1 do
          PDWordArray(SNum^.D)^[I] := 0;
        SNum^.Top := SDiv^.Top + 2;
      end
      else
      begin
        if BigNumberWordExpand(SNum^, SDiv^.Top + 1) = nil then
          Exit;
        PDWordArray(SNum^.D)^[SNum^.Top] := 0;
        Inc(SNum^.Top);
      end;
    end;

    DivN := SDiv^.Top;
    NumN := SNum^.Top;
    Loop := NumN - DivN;

    WNum.Neg := 0;
    WNum.D := PDWORD(Integer(SNum^.D) + Loop * SizeOf(DWORD));
    WNum.Top := DivN;
    WNum.DMax := SNum^.DMax - Loop;

    D0 := PDWordArray(SDiv.D)^[DivN - 1];
    if DivN = 1 then
      D1 := 0
    else
      D1 := PDWordArray(SDiv.D)^[DivN - 2];
    // D0 D1 是 SDiv 的最高俩 DWORD

    WNump := PDWORD(Integer(SNum^.D) + (NumN - 1) * SizeOf(DWORD));

    if Num.Neg <> Divisor.Neg then
      SRes^.Neg := 1
    else
      SRes^.Neg := 0;

    if BigNumberWordExpand(SRes^, Loop + 1) = nil then
      Exit;

    SRes^.Top := Loop - NoBranch;
    Resp := PDWORD(Integer(SRes^.D) + (Loop - 1) * SizeOf(DWORD));

    if BigNumberWordExpand(Tmp^, DivN + 1) = nil then
      Exit;

    if NoBranch = 0 then
    begin
      if BigNumberUnsignedCompare(WNum, SDiv^) >= 0 then
      begin
        BigNumberSubWords(PDWordArray(WNum.D), PDWordArray(WNum.D),
          PDWordArray(SDiv^.D), DivN);
        Resp^ := 1;
      end
      else
        Dec(SRes^.Top);
    end;

    if SRes^.Top = 0 then
      SRes^.Neg := 0
    else
      Resp := PDWORD(Integer(Resp) - SizeOf(DWORD));

    for I := 0 to Loop - 2 do
    begin
//    Rem := 0;
      // 用 N0/N1/D0/D1 计算出一个 Q 使 | WNum - SDiv * Q | < SDiv
      N0 := WNump^;
      N1 := (PDWORD(Integer(WNump) - SizeOf(DWORD)))^;

      if N0 = D0 then
        Q := BN_MASK2
      else
      begin
        Q := BigNumberDivWords(N0, N1, D0);
        Rem := (N1 - Q * D0) and BN_MASK2;

        T2L := LBITS(D1);
        T2H := HBITS(D1);
        QL := LBITS(Q);
        QH := HBITS(Q);
        Mul64(T2L, T2H, QL, QH);

        while True do
        begin
          if (T2H < Rem) or ((T2H = Rem) and
             (T2L <= (PDWORD(Integer(WNump) - 2 * SizeOf(DWORD)))^)) then
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

      L0 := BigNumberMulWords(PDWordArray(Tmp^.D), PDWordArray(SDiv^.D), DivN, Q);
      PDWordArray(Tmp^.D)^[DivN] := L0;
      WNum.D := PDWORD(Integer(WNum.D) - SizeOf(DWORD));

      if BigNumberSubWords(PDWordArray(WNum.D), PDWordArray(WNum.D),
        PDWordArray(Tmp^.D), DivN + 1) <> 0 then
      begin
        Dec(Q);
        if BigNumberAddWords(PDWordArray(WNum.D), PDWordArray(WNum.D),
          PDWordArray(SDiv^.D), DivN) <> 0 then
          WNump^ := WNump^ + 1;
      end;

      Resp^ := Q;
      WNump := PDWORD(Integer(WNump) - SizeOf(DWORD));
      Resp := PDWORD(Integer(Resp) - SizeOf(DWORD));
    end;

    BigNumberCorrectTop(SNum^);
    Neg := Num.Neg;
    BigNumberShiftRight(Remain, SNum^, NormShift);
    if not BigNumberIsZero(Remain) then
      Remain.Neg := Neg;
    if NoBranch <> 0 then
      BigNumberCorrectTop(SRes^);
    Result := True;
  finally
    BigNumberContextEnd(Ctx^);
    BigNumberContextFree(Ctx);
  end;
end;

function BigNumberMod(var Remain: TCnBigNumber;
  var Num: TCnBigNumber; var Divisor: TCnBigNumber): Boolean;
var
  Res: PCnBigNumber;
begin
  Res := BigNumberNew;
  try
    Result := BigNumberDiv(Res^, Remain, Num, Divisor);
  finally
    BigNumberFree(Res);
  end;
end;

end.
