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
* 开发平台：PWin2003Std + Delphi 6.0
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
  BN_FLG_MALLOCED       = $1;
  BN_FLG_STATIC_DATA    = $2;
  BN_FLG_CONSTTIME      = $4;

  BN_FLG_FREE           = $8000;

  BN_BITS               = 64;
  BN_BYTES              = 4;
  BN_BITS2              = 32;     // D 数组中的一个元素所包含的位数
  BN_BITS4              = 16;

  BN_MASK2              = $FFFFFFFF;

type
  TDWordArray = array [0..MaxInt div SizeOf(Integer) - 1] of DWORD;
  PDWordArray = ^TDWordArray;

  {* 用来代表一个大数的结构体 }
  TCnBigNumber = packed record
    D: PDWORD;
    Top: Integer;
    DMax: Integer;
    Neg: Integer;
    Flags: Integer;
  end;
  PCnBigNumber = ^TCnBigNumber;

function BigNumerNew(): PCnBigNumber;

procedure BigNumberInit(var Num: TCnBigNumber);

procedure BigNumberClear(var Num: TCnBigNumber);

procedure BigNumberFree(Num: PCnBigNumber);

function BigNumberIsZero(var Num: TCnBigNumber): Boolean;
{* 返回一个大数结构里的大数是否为 0 }

function BigNumberSetZero(var Num: TCnBigNumber): Boolean;
{* 将一个大数结构里的大数设置为 0 }

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

implementation

const
  Hex: string = '0123456789ABCDEF';

  ADVAPI32 = 'advapi32.dll';

  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = $8;
  CRYPT_DELETEKEYSET = $10;

  PROV_RSA_FULL = 1;

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

function BigNumerNew(): PCnBigNumber;
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

// 调整 Top 保证其指向最高位非 0 处
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
    Num.Top := Top;
    Dec(Top);
  end;
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
  Result := BigNumerNew;

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

  end
end;

function BigNumberDuplicate(var Num: TCnBigNumber): PCnBigNumber;
begin
  Result := BigNumerNew;
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

end.
