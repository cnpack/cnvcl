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
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 XE 2
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2020.09.06 V1.5
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
  Classes, SysUtils, SysConst {$IFDEF MACOS}, System.Generics.Collections {$ENDIF};

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

{$IFDEF WIN64}
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
{$ELSE}
  TUInt64          = Int64;
  PUInt64          = ^TUInt64;
{$ENDIF}

  TUInt64Array = array of TUInt64;

{$IFDEF MACOS}
  TCnUInt32List = TList<LongWord>;
  TCnUInt64List = TList<UInt64>;
{$ENDIF}

const
  MAX_TUINT64: TUInt64                   = $FFFFFFFFFFFFFFFF;
  MAX_SIGNED_INT64_IN_TUINT64: TUInt64   = $7FFFFFFFFFFFFFFF;

type
  TCnIntegerList = class(TList)
  {* 整数列表}
  private
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const Value: Integer);
  public
    function Add(Item: Integer): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: Integer); reintroduce;
    property Items[Index: Integer]: Integer read Get write Put; default;
  end;

  PInt64List = ^TInt64List;
  TInt64List = array[0..MaxListSize - 1] of Int64;

  TCnInt64List = class(TObject)
  {* 64 位整数列表}
  private
    FList: PInt64List;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Int64;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Int64);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Int64): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TCnInt64List;
    function First: Int64;
    function IndexOf(Item: Int64): Integer;
    procedure Insert(Index: Integer; Item: Int64);
    function Last: Int64;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Int64): Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Int64 read Get write Put; default;
    property List: PInt64List read FList;
  end;

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

function IsInt32AddOverflow(A, B: Integer): Boolean;
{* 判断两个 32 位有符号数相加是否溢出}

function IsUInt32AddOverflow(A, B: Cardinal): Boolean;
{* 判断两个 32 位无符号数相加是否溢出}

function IsInt64AddOverflow(A, B: Int64): Boolean;
{* 判断两个 64 位有符号数相加是否溢出}

function IsUInt64AddOverflow(A, B: TUInt64): Boolean;
{* 判断两个 64 位无符号数相加是否溢出}

function PointerToInteger(P: Pointer): Integer;
{* 指针类型转换成整型，支持 32/64 位}

function IntegerToPointer(I: Integer): Pointer;
{* 整型转换成指针类型，支持 32/64 位}

implementation

resourcestring
  SCnInt64ListError = 'Int64 List Error. %d';

{$IFDEF WIN64}

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

  if N < 4 then
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
  Result := (N and (1 shl 63)) <> 0;
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

// 判断两个 32 位有符号数相加是否溢出
function IsInt32AddOverflow(A, B: Integer): Boolean;
var
  C: Integer;
begin
  C := A + B;
  Result := ((A > 0) and (B > 0) and (C < 0)) or   // 同符号且结果换号了说明出现了溢出
    ((A < 0) and (B < 0) and (C > 0));
end;

// 判断两个 32 位无符号数相加是否溢出
function IsUInt32AddOverflow(A, B: Cardinal): Boolean;
begin
  Result := (A + B) < A; // 无符号相加，结果只要小于任一个数就说明溢出了
end;

// 判断两个 64 位有符号数相加是否溢出
function IsInt64AddOverflow(A, B: Int64): Boolean;
var
  C: Int64;
begin
  C := A + B;
  Result := ((A > 0) and (B > 0) and (C < 0)) or   // 同符号且结果换号了说明出现了溢出
    ((A < 0) and (B < 0) and (C > 0));
end;

// 判断两个 64 位无符号数相加是否溢出
function IsUInt64AddOverflow(A, B: TUInt64): Boolean;
begin
  Result := UInt64Compare(A + B, A) < 0; // 无符号相加，结果只要小于任一个数就说明溢出了
end;

// 指针类型转换成整型，支持 32/64 位
function PointerToInteger(P: Pointer): Integer;
begin
{$IFDEF WIN64}
  // 先这么写，利用 Pointer 的低 32 位存 Integer
  Result := Integer(P);
{$ELSE}
  Result := Integer(P);
{$ENDIF}
end;

// 整型转换成指针类型，支持 32/64 位
function IntegerToPointer(I: Integer): Pointer;
begin
{$IFDEF WIN64}
  // 先这么写，利用 Pointer 的低 32 位存 Integer
  Result := Pointer(I);
{$ELSE}
  Result := Pointer(I);
{$ENDIF}
end;

{ TCnIntegerList }

function TCnIntegerList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(IntegerToPointer(Item));
end;

function TCnIntegerList.Get(Index: Integer): Integer;
begin
  Result := PointerToInteger(inherited Get(Index));
end;

procedure TCnIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, IntegerToPointer(Item));
end;

procedure TCnIntegerList.Put(Index: Integer; const Value: Integer);
begin
  inherited Put(Index, IntegerToPointer(Value));
end;

{ TCnInt64List }

destructor TCnInt64List.Destroy;
begin
  Clear;
end;

function TCnInt64List.Add(Item: Int64): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnInt64List.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnInt64List.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Int64));
end;

class procedure TCnInt64List.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

procedure TCnInt64List.Exchange(Index1, Index2: Integer);
var
  Item: Int64;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SCnInt64ListError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SCnInt64ListError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnInt64List.Expand: TCnInt64List;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnInt64List.First: Int64;
begin
  Result := Get(0);
end;

function TCnInt64List.Get(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);
  Result := FList^[Index];
end;

procedure TCnInt64List.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnInt64List.IndexOf(Item: Int64): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnInt64List.Insert(Index: Integer; Item: Int64);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SCnInt64ListError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Int64));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TCnInt64List.Last: Int64;
begin
  Result := Get(FCount - 1);
end;

procedure TCnInt64List.Move(CurIndex, NewIndex: Integer);
var
  Item: Int64;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SCnInt64ListError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnInt64List.Put(Index: Integer; Item: Int64);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);

  FList^[Index] := Item;
end;

function TCnInt64List.Remove(Item: Int64): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnInt64List.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SCnInt64ListError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Int64));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnInt64List.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SCnInt64ListError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Int64), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

end.
