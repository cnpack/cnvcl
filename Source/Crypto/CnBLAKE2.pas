{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnBLAKE2;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：BLAKE 杂凑算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
*           从 https://github.com/BLAKE2/BLAKE2 的 C 代码移植而来并补充部分功能
* 备    注：本单元实现了 BLAKE2 系列杂凑算法如 2S/2B 等。
*           注：BLAKE2 内部设计了 Key 值，因而无需 HMAC 实现。
* 开发平台：PWin7 + Delphi 7.0
* 兼容测试：PWinXP/7/10/11 + Delphi 5/6/7 ~ D12
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.06.15 V1.0
*               创建单元。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative;

const
  CN_BLAKE2S_BLOCKBYTES    = 64;
  CN_BLAKE2S_OUTBYTES      = 32;
  CN_BLAKE2S_KEYBYTES      = 32;
  CN_BLAKE2S_SALTBYTES     = 8;
  CN_BLAKE2S_PERSONALBYTES = 8;

  CN_BLAKE2B_BLOCKBYTES    = 128;
  CN_BLAKE2B_OUTBYTES      = 64;
  CN_BLAKE2B_KEYBYTES      = 64;
  CN_BLAKE2B_SALTBYTES     = 16;
  CN_BLAKE2B_PERSONALBYTES = 16;

type
  TCnBLAKE2SContext = packed record
  {* BLAKE2S 的上下文结构}
    H: array[0..7] of Cardinal;
    T: array[0..1] of Cardinal;
    F: array[0..1] of Cardinal;
    Buf: array[0..CN_BLAKE2S_BLOCKBYTES - 1] of Byte;
    BufLen: Integer;
    OutLen: Integer;
    Last: Byte;
  end;

  TCnBLAKE2BContext = packed record
  {* BLAKE2B 的上下文结构}
    H: array[0..7] of TUInt64;
    T: array[0..1] of TUInt64;
    F: array[0..1] of TUInt64;
    Buf: array[0..CN_BLAKE2B_BLOCKBYTES - 1] of Byte;
    BufLen: Integer;
    OutLen: Integer;
    Last: Byte;
  end;

implementation

resourcestring
  SCnErrorBlake2InvalidKeySize = 'Invalid Key Length';
  SCnErrorBlake2InvalidDigestSize = 'Invalid Digest Length';

const
  BLAKE2S_IV: array[0..7] of Cardinal = (
    $6A09E667, $BB67AE85, $3C6EF372, $A54FF53A,
    $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
  );

  BLAKE2B_IV: array[0..7] of TUInt64 = (
    $6A09E667F3BCC908, $BB67AE8584CAA73B,
    $3C6EF372FE94F82B, $A54FF53A5F1D36F1,
    $510E527FADE682D1, $9B05688C2B3E6C1F,
    $1F83D9ABFB41BD6B, $5BE0CD19137E2179
  );

  BLAKE2S_SIGMA: array[0..9, 0..15] of Byte = (
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
    ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
    (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
    (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
    (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
    ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
    ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
    (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
    ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 )
  );

  BLAKE2B_SIGMA: array[0..11, 0..15] of Byte = (
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
    ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
    (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
    (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
    (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
    ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
    ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
    (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
    ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 ),
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 )
  );

type
  TCnBLAKE2SParam = packed record
    DigestLength: Byte;
    KeyLength: Byte;
    FanOut: Byte;
    Depth: Byte;
    LeafLength: Cardinal;
    NodeOffset: Cardinal;
    XofLength: Word;
    NodeDepth: Byte;
    InnerLength: Byte;
    Salt: array[0..CN_BLAKE2S_SALTBYTES - 1] of Byte;
    Personal: array[0..CN_BLAKE2S_PERSONALBYTES - 1] of Byte;
  end;

  TCnBLAKE2BParam = packed record
    DigestLength: Byte;
    KeyLength: Byte;
    FanOut: Byte;
    Depth: Byte;
    LeafLength: Cardinal;
    NodeOffset: Cardinal;
    XofLength: Cardinal;
    NodeDepth: Byte;
    InnerLength: Byte;
    Reserved: array[0..13] of Byte; // 对齐 32 用
    Salt: array[0..CN_BLAKE2B_SALTBYTES - 1] of Byte;
    Personal: array[0..CN_BLAKE2B_PERSONALBYTES - 1] of Byte;
  end;

function ROTRight256(A, B: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A shr B) or (A shl (32 - B));
end;

function ROTRight512(X: TUInt64; Y: Integer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X shr Y) or (X shl (64 - Y));
end;

procedure GS(MPtr: Pointer; R, I: Integer; var A, B, C, D: Cardinal);
var
  M: PCnUInt32Array;
begin
  M := PCnUInt32Array(MPtr);

  A := A + B + M[BLAKE2S_SIGMA[R][2 * I]];
  D := ROTRight256(D xor A, 16);
  C := C + D;
  B := ROTRight256(B xor C, 12);
  A := A + B + M[BLAKE2S_SIGMA[R][2 * I + 1]];
  D := ROTRight256(D xor A, 8);
  C := C + D;
  B := ROTRight256(B xor C, 7);
end;

procedure RoundS(MPtr, VPtr: Pointer; R: Integer);
var
  V: PCnUInt32Array;
begin
  V := PCnUInt32Array(VPtr);

  GS(MPtr, R, 0, V^[0], V^[4], V^[8], V^[12]);
  GS(MPtr, R, 1, V^[1], V^[5], V^[9], V^[13]);
  GS(MPtr, R, 2, V^[2], V^[6], V^[10], V^[14]);
  GS(MPtr, R, 3, V^[3], V^[7], V^[11], V^[15]);
  GS(MPtr, R, 4, V^[0], V^[5], V^[10], V^[15]);
  GS(MPtr, R, 5, V^[1], V^[6], V^[11], V^[12]);
  GS(MPtr, R, 6, V^[2], V^[7], V^[8], V^[13]);
  GS(MPtr, R, 7, V^[3], V^[4], V^[9], V^[14]);
end;

procedure GB(MPtr: Pointer; R, I: Integer; var A, B, C, D: TUInt64);
var
  M: PCnUInt64Array;
begin
  M := PCnUInt64Array(MPtr);

  A := A + B + M[BLAKE2B_SIGMA[R][2 * I]];
  D := ROTRight512(D xor A, 32);
  C := C + D;
  B := ROTRight512(B xor C, 24);
  A := A + B + M[BLAKE2B_SIGMA[R][2 * I + 1]];
  D := ROTRight512(D xor A, 16);
  C := C + D;
  B := ROTRight512(B xor C, 63);
end;

procedure RoundB(MPtr, VPtr: Pointer; R: Integer);
var
  V: PCnUInt64Array;
begin
  V := PCnUInt64Array(VPtr);

  GB(MPtr, R, 0, V^[0], V^[4], V^[8], V^[12]);
  GB(MPtr, R, 1, V^[1], V^[5], V^[9], V^[13]);
  GB(MPtr, R, 2, V^[2], V^[6], V^[10], V^[14]);
  GB(MPtr, R, 3, V^[3], V^[7], V^[11], V^[15]);
  GB(MPtr, R, 4, V^[0], V^[5], V^[10], V^[15]);
  GB(MPtr, R, 5, V^[1], V^[6], V^[11], V^[12]);
  GB(MPtr, R, 6, V^[2], V^[7], V^[8], V^[13]);
  GB(MPtr, R, 7, V^[3], V^[4], V^[9], V^[14]);
end;

procedure BLAKE2SCompress(var Context: TCnBLAKE2SContext; InPtr: Pointer);
var
  I: Integer;
  C: PCardinal;
  M, V: array[0..15] of Cardinal;
begin
  for I := 0 to 15 do
  begin
    C := PCardinal(TCnIntAddress(InPtr) + I * SizeOf(Cardinal));
    M[I] := UInt32ToLittleEndian(C^);
  end;

  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := BLAKE2S_IV[0];
  V[ 9] := BLAKE2S_IV[1];
  V[10] := BLAKE2S_IV[2];
  V[11] := BLAKE2S_IV[3];
  V[12] := Context.T[0] xor BLAKE2S_IV[4];
  V[13] := Context.T[1] xor BLAKE2S_IV[5];
  V[14] := Context.F[0] xor BLAKE2S_IV[6];
  V[15] := Context.F[1] xor BLAKE2S_IV[7];

  RoundS(@M[0], @V[0], 0);
  RoundS(@M[0], @V[0], 1);
  RoundS(@M[0], @V[0], 2);
  RoundS(@M[0], @V[0], 3);
  RoundS(@M[0], @V[0], 4);
  RoundS(@M[0], @V[0], 5);
  RoundS(@M[0], @V[0], 6);
  RoundS(@M[0], @V[0], 7);
  RoundS(@M[0], @V[0], 8);
  RoundS(@M[0], @V[0], 9);

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor V[I] xor V[I + 8];
end;

procedure BLAKE2BCompress(var Context: TCnBLAKE2BContext; InPtr: Pointer);
var
  I: Integer;
  P: PCnUInt8Array;
  C: PUInt64;
  M, V: array[0..15] of TUInt64;
begin
  P := PCnUInt8Array(InPtr);
  for I := 0 to 15 do
  begin
    C := PUInt64(TCnIntAddress(InPtr) + I * SizeOf(TUInt64));
    M[I] := UInt64ToLittleEndian(C^);
  end;

  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := BLAKE2B_IV[0];
  V[ 9] := BLAKE2B_IV[1];
  V[10] := BLAKE2B_IV[2];
  V[11] := BLAKE2B_IV[3];
  V[12] := Context.T[0] xor BLAKE2B_IV[4];
  V[13] := Context.T[1] xor BLAKE2B_IV[5];
  V[14] := Context.F[0] xor BLAKE2B_IV[6];
  V[15] := Context.F[1] xor BLAKE2B_IV[7];

  RoundB(@M[0], @V[0], 0);
  RoundB(@M[0], @V[0], 1);
  RoundB(@M[0], @V[0], 2);
  RoundB(@M[0], @V[0], 3);
  RoundB(@M[0], @V[0], 4);
  RoundB(@M[0], @V[0], 5);
  RoundB(@M[0], @V[0], 6);
  RoundB(@M[0], @V[0], 7);
  RoundB(@M[0], @V[0], 8);
  RoundB(@M[0], @V[0], 9);
  RoundB(@M[0], @V[0], 10);
  RoundB(@M[0], @V[0], 11);

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor V[I] xor V[I + 8];
end;

end.
