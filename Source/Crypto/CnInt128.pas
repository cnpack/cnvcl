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

unit CnInt128;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：128 位有无符号整数的运算实现
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：缺除法与取模等，并且待完整测试
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 XE 2
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.06.11 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNativeDecl;

type
  TCnInt128 = packed record   // 128 位有符号整数结构
    Lo64, Hi64: Int64;        // 注意 Lo64 内部仍作为 64 位无符号整数处理
  end;
  PCnInt128 = ^TCnInt128;

  TCnUInt128 = packed record  // 128 位无符号整数结构
    Lo64, Hi64: TUInt64;
  end;
  PCnUInt128 = ^TCnUInt128;

// ========================= Int128 计算函数 ===================================

procedure Int128Set(var R: TCnInt128; Lo, Hi: Int64); overload;
{* 分别设置 128 位有符号数的高低 64 位原始值，不额外处理正负号}

procedure Int128Set(var R: TCnInt128; Lo: Int64); overload;
{* 设置 128 位有符号数的低 64 位值，高位根据正负情况置全 0 或全 F}

procedure Int128Copy(var D, S: TCnInt128);
{* 复制 128 位有符号数}

procedure Int128SetZero(var N: TCnInt128);
{* 将一 128 位有符号数置 0}

procedure Int128Add(var R, A, B: TCnInt128); overload;
{* 128 位有符号数相加，不考虑溢出的情况。R、A、B 可以相同。A B 使用补码无需分开考虑正负值}

procedure Int128Add(var R, A: TCnInt128; V: Int64); overload;
{* 给一 128 位有符号数加上一个 64 位有符号数。考虑了 B 为负值的情况}

procedure Int128Sub(var R, A, B: TCnInt128); overload;
{* 128 位有符号数相减，不考虑溢出的情况。R、A、B 可以相同}

procedure Int128Sub(var R, A: TCnInt128; V: Int64); overload;
{* 给一 128 位有符号数减去一个 64 位有符号数。考虑了 B 为负值的情况}

procedure Int128Mul(var R, A, B: TCnInt128; ResHi: PCnInt128 = nil);
{* 128 位有符号数相乘，有溢出则抛异常（ResHi 参数暂不起作用）。R、A、B 可以相同}

procedure Int128ShiftLeft(var N: TCnInt128; S: Integer);
{* 128 位有符号数按位左移}

procedure Int128ShiftRight(var N: TCnInt128; S: Integer);
{* 128 位有符号数按位右移}

procedure Int128And(var R, A, B: TCnInt128);
{* 两个 128 位有符号数按位与}

procedure Int128Or(var R, A, B: TCnInt128);
{* 两个 128 位有符号数按位或}

procedure Int128Xor(var R, A, B: TCnInt128);
{* 两个 128 位有符号数按位异或}

procedure Int128Negate(var N: TCnInt128);
{* 将一 128 位有符号数置为其相反数}

procedure Int128Not(var N: TCnInt128);
{* 将一 128 位有符号数求反}

procedure Int128SetBit(var N: TCnInt128; Bit: Integer);
{* 将一 128 位有符号数的某一位置 1，Bit 从 0 到 127}

procedure Int128ClearBit(var N: TCnInt128; Bit: Integer);
{* 将一 128 位有符号数的某一位置 0，Bit 从 0 到 127}

function Int128IsBitSet(var N: TCnInt128; Bit: Integer): Boolean;
{* 返回一　128 位有符号数的某一位是否是 0，Bit 从 0 到 127}

function Int128IsNegative(var N: TCnInt128): Boolean;
{* 判断一 128 位有符号数是否是负数}

function Int128Equal(var A, B: TCnInt128): Boolean;
{* 判断两个 128 位有符号数是否相等}

function Int128Compare(var A, B: TCnInt128): Integer;
{* 比较两个 128 位有符号数，大于等于小于分别返回 1、0、-1}

function Int128ToHex(var N: TCnInt128): string;
{* 将 128 位有符号数转换为十六进制字符串}

// ======================== UInt128 计算函数 ===================================

procedure UInt128Set(var R: TCnUInt128; Lo, Hi: TUInt64); overload;
{* 分别设置 128 位无符号数的高低 64 位值}

procedure UInt128Set(var R: TCnUInt128; Lo: TUInt64); overload;
{* 设置 128 位无符号数的低 64 位值，高位置 0}

procedure UInt128Copy(var D, S: TCnUInt128);
{* 复制 128 位无符号数}

procedure UInt128SetZero(var N: TCnUInt128);
{* 将一 128 位无符号数置 0}

procedure UInt128Add(var R: TCnUInt128; V: TUInt64); overload;
{* 给一 128 位无符号数加上一个 64 位无符号数}

procedure UInt128Add(var R, A, B: TCnUInt128); overload;
{* 128 位无符号数相加，不考虑溢出的情况。R、A、B可以相同}

procedure UInt128Sub(var R, A, B: TCnUInt128);
{* 128 位无符号数相减，不考虑溢出的情况。R、A、B可以相同}

procedure UInt128Mul(var R, A, B: TCnUInt128; ResHi: PCnUInt128 = nil);
{* 128 位无符号数相乘，有溢出则超过 128 位的放 ResHi 中
  如传 nil 且溢出则抛异常。R、A、B可以相同}

procedure UInt128ShiftLeft(var N: TCnUInt128; S: Integer);
{* 128 位无符号数按位左移}

procedure UInt128ShiftRight(var N: TCnUInt128; S: Integer);
{* 128 位无符号数按位右移}

procedure UInt128And(var R, A, B: TCnUInt128);
{* 两个 128 位无符号数按位与}

procedure UInt128Or(var R, A, B: TCnUInt128);
{* 两个 128 位无符号数按位或}

procedure UInt128Xor(var R, A, B: TCnUInt128);
{* 两个 128 位无符号数按位异或}

procedure UInt128Not(var N: TCnUInt128);
{* 128 位无符号数求反}

procedure UInt128SetBit(var N: TCnUInt128; Bit: Integer);
{* 将一 128 位无符号数的某一位置 1，Bit 从 0 到 127}

procedure UInt128ClearBit(var N: TCnUInt128; Bit: Integer);
{* 将一 128 位无符号数的某一位置 0，Bit 从 0 到 127}

function UInt128IsBitSet(var N: TCnUInt128; Bit: Integer): Boolean;
{* 返回一　128 位无符号数的某一位是否是 0，Bit 从 0 到 127}

function UInt128Equal(var A, B: TCnUInt128): Boolean;
{* 判断两个 128 位无符号数是否相等}

function UInt128Compare(var A, B: TCnUInt128): Integer;
{* 比较两个 128 位无符号数，大于等于小于分别返回 1、0、-1}

function IsUInt128AddOverflow(var A, B: TCnUInt128): Boolean;
{* 判断两个 64 位无符号数相加是否溢出 128 位无符号上限}

function UInt128ToHex(var N: TCnUInt128): string;
{* 将 128 位无符号数转换为十六进制字符串}

implementation

var
  FInt128Zero: TCnInt128 = (Lo64: 0; Hi64: 0);
  FInt128One: TCnInt128 = (Lo64:1; Hi64: 0);

  FUInt128Zero: TCnUInt128 = (Lo64: 0; Hi64: 0);
  FUInt128One: TCnUInt128 = (Lo64:1; Hi64: 0);

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

procedure Int128Mul(var R, A, B: TCnInt128; ResHi: PCnInt128);
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
    raise EIntOverflow.Create('Int128 Mul Overflow');

  if N1 <> N2 then // 只要有一个变过
  begin
    Int128Negate(R);
    // TODO: ResHi 如何变？先不传
  end;

  // 变回去
  if N1 then
    Int128Negate(A);
  if N2 then
    Int128Negate(B);
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
    N.Hi64 := N.Hi64 + C;
end;

procedure Int128Not(var N: TCnInt128);
begin
  N.Lo64 := not N.Lo64;
  N.Hi64 := not N.Hi64;
end;

procedure Int128SetBit(var N: TCnInt128; Bit: Integer);
begin
  if Bit > 63 then
    UInt64SetBit(N.Hi64, Bit - 64)
  else
    UInt64SetBit(N.Lo64, Bit);
end;

procedure Int128ClearBit(var N: TCnInt128; Bit: Integer);
begin
  if Bit > 63 then
    UInt64ClearBit(N.Hi64, Bit - 64)
  else
    UInt64ClearBit(N.Lo64, Bit);
end;

function Int128IsBitSet(var N: TCnInt128; Bit: Integer): Boolean;
begin
  if Bit > 63 then
    Result := GetUInt64BitSet(N.Hi64, Bit - 64)
  else
    Result := GetUInt64BitSet(N.Hi64, Bit);
end;

function Int128IsNegative(var N: TCnInt128): Boolean;
begin
  Result := N.Hi64 < 0;
end;

function Int128Equal(var A, B: TCnInt128): Boolean;
begin
  Result := (A.Lo64 = B.Lo64) and (A.Hi64 = B.Hi64);
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
  T: TCnInt128;
begin
  if N.Hi64 < 0 then
  begin
    Int128Copy(T, N);
    Int128Negate(T);
    Result := '-' + UInt64ToHex(T.Hi64) + UInt64ToHex(T.Lo64);
  end
  else
    Result := UInt64ToHex(N.Hi64) + UInt64ToHex(N.Lo64);
end;

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

procedure UInt128SetZero(var N: TCnUInt128);
begin
  N.Lo64 := 0;
  N.Hi64 := 0;
end;

procedure UInt128Add(var R, A, B: TCnUInt128);
var
  C: Integer;
begin
  UInt64Add(R.Lo64, A.Lo64, B.Lo64, C);
  R.Hi64 := A.Hi64 + B.Hi64 + C;
end;

procedure UInt128Add(var R: TCnUInt128; V: TUInt64);
var
  C: Integer;
begin
  UInt64Add(R.Lo64, R.Lo64, V, C);
  R.Hi64 := R.Hi64 + C;
end;

// 两个 128 位无符号数相加，A + B => R，如果有溢出，则溢出的 1 搁进位标记里，否则清零
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
      raise EIntOverflow.Create('UInt128 Mul Overflow');

    T.Hi64 := 0;
    T.Lo64 := R1.Hi64;
    UInt128Add(ResHi^, R3, T);

    T.Lo64 := R2.Hi64;
    UInt128Add(ResHi^, ResHi^, T);

    T.Lo64 := C1 + C2;
    UInt128Add(ResHi^, ResHi^, T); // 加进位，不会再朝上溢出了
  end;
end;

procedure UInt128ShiftLeft(var N: TCnUInt128; S: Integer);
var
  T, M: TUInt64;
begin
  if S = 0 then
    Exit;

  if S < 0 then
    UInt128ShiftRight(N, -S);

  if S > 128 then // 全移跑了
  begin
    N.Hi64 := 0;
    N.Lo64 := 0;
  end
  else if S > 64 then
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

  if S > 128 then // 全移跑了
  begin
    N.Hi64 := 0;
    N.Lo64 := 0;
  end
  else if S > 64 then
  begin
    // Lo 为全 0
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
    Result := GetUInt64BitSet(N.Hi64, Bit);
end;

function UInt128Equal(var A, B: TCnUInt128): Boolean;
begin
  Result := (A.Lo64 = B.Lo64) and (A.Hi64 = B.Hi64);
end;

function UInt128Compare(var A, B: TCnUInt128): Integer;
begin
  if A.Hi64 > B.Hi64 then
    Result := 1
  else if A.Hi64 < B.Hi64 then
    Result := -1
  else
  begin
    if A.Lo64 > B.Lo64 then
      Result := 1
    else if A.Lo64 < B.Lo64 then
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
begin
  Result := UInt64ToHex(N.Hi64) + UInt64ToHex(N.Lo64);
end;

end.

