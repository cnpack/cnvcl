{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnZUC;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：祖冲之算法实现单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：参考国密算法公开文档《祖冲之序列密码算法 ZUC stream cipher algorithm》
*           与英文的《Specification of the 3GPP Confidentiality and Integrity
*           Algorithms 128-EEA3 & 128-EIA3》仨文档，并参考移植其 C 实现代码，
*           注意 ZUCEEA3 加解密调用相同，也就是其他参数相同的情况下，传明文得到密文，传密文得到明文
*           祖冲之算法面向四字节为单位的数据流，四字节内部的大小端未处理
*           规范中要求数据均为大端，因而在小端 CPU 上需要颠倒四字节内部的内容，目前未实现
* 开发平台：Windows 7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.04.26 V1.2
*               修改 LongWord 与 Integer 地址转换以支持 MacOS64
*           2019.04.15 V1.1
*               支持 Win32/Win64/MacOS32
*           2017.04.20 V1.0
*               移植并创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative;

procedure ZUC(Key: PByte; IV: PByte; KeyStream: PCardinal; KeyStreamLen: Cardinal);
{*
  祖冲之基础算法

  Key 和 IV 为输入的 16 字节数据。
  KeyStream 为输出地址，长度应为 KeyStreamLen * SizeOf(Cardinal)
  KeyStreamLen 是要求计算的长度，以四字节为单位
}

procedure ZUCEEA3(CK: PByte; Count, Bearer, Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; C: PCardinal);
{*
  基于祖冲之算法的 128-EEA3 机密性保护算法。一个流加密系统，使用机密性密钥 CK 来加解密数据块

  输入：
  参数         规模（比特数）    说明
  Count        32                计数器
  Bearer       5                 The bearer identity
  Direction    1                 传输方向
  CK           128               机密性密钥
  M            BitLen            输入比特流的内存块地址，长度需是四字节的倍数
  BitLen       32                输入消息的 Bit 长度

  输出：
  参数         规模（比特数）    说明
  C            BitLen            放置输出比特流的内存块地址，能容纳的 Bit 长度必须大于或等于 BitLen，且是四字节的倍数

  注意因为是流异或加密。传入如果是明文，加密一次便输出密文，如果传入密文，则调用一次会还原成明文
}

procedure ZUCEIA3(IK: PByte; Count, Bearer, Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; out Mac: Cardinal);
{*
  基于祖冲之算法的 128-EIA3 完整性保护算法，使用一个完整性密钥 IK 对给定的输入消息计算出一个 32 位的 MAC 值

  输入：
  参数         规模（比特数）    说明
  Count        32                计数器
  Bearer       5                 The bearer identity
  DIRECTION    1                 传输方向
  IK           128               完整性密钥
  M            BitLen            输入比特流的内存地址，长度需是四字节的倍数
  BitLen       32                输入比特流的长度

  输出：
  参数         规模（比特数）    说明
  MAC          32                32 位 MAC 值
}

implementation

const
  S0: array[0..255] of Byte = (
    $3E, $72, $5B, $47, $CA, $E0, $00, $33, $04, $D1, $54, $98, $09, $B9, $6D, $CB,
    $7B, $1B, $F9, $32, $AF, $9D, $6A, $A5, $B8, $2D, $FC, $1D, $08, $53, $03, $90,
    $4D, $4E, $84, $99, $E4, $CE, $D9, $91, $DD, $B6, $85, $48, $8B, $29, $6E, $AC,
    $CD, $C1, $F8, $1E, $73, $43, $69, $C6, $B5, $BD, $FD, $39, $63, $20, $D4, $38,
    $76, $7D, $B2, $A7, $CF, $ED, $57, $C5, $F3, $2C, $BB, $14, $21, $06, $55, $9B,
    $E3, $EF, $5E, $31, $4F, $7F, $5A, $A4, $0D, $82, $51, $49, $5F, $BA, $58, $1C,
    $4A, $16, $D5, $17, $A8, $92, $24, $1F, $8C, $FF, $D8, $AE, $2E, $01, $D3, $AD,
    $3B, $4B, $DA, $46, $EB, $C9, $DE, $9A, $8F, $87, $D7, $3A, $80, $6F, $2F, $C8,
    $B1, $B4, $37, $F7, $0A, $22, $13, $28, $7C, $CC, $3C, $89, $C7, $C3, $96, $56,
    $07, $BF, $7E, $F0, $0B, $2B, $97, $52, $35, $41, $79, $61, $A6, $4C, $10, $FE,
    $BC, $26, $95, $88, $8A, $B0, $A3, $FB, $C0, $18, $94, $F2, $E1, $E5, $E9, $5D,
    $D0, $DC, $11, $66, $64, $5C, $EC, $59, $42, $75, $12, $F5, $74, $9C, $AA, $23,
    $0E, $86, $AB, $BE, $2A, $02, $E7, $67, $E6, $44, $A2, $6C, $C2, $93, $9F, $F1,
    $F6, $FA, $36, $D2, $50, $68, $9E, $62, $71, $15, $3D, $D6, $40, $C4, $E2, $0F,
    $8E, $83, $77, $6B, $25, $05, $3F, $0C, $30, $EA, $70, $B7, $A1, $E8, $A9, $65,
    $8D, $27, $1A, $DB, $81, $B3, $A0, $F4, $45, $7A, $19, $DF, $EE, $78, $34, $60
  );

  S1: array[0..255] of Byte = (
    $55, $C2, $63, $71, $3B, $C8, $47, $86, $9F, $3C, $DA, $5B, $29, $AA, $FD, $77,
    $8C, $C5, $94, $0C, $A6, $1A, $13, $00, $E3, $A8, $16, $72, $40, $F9, $F8, $42,
    $44, $26, $68, $96, $81, $D9, $45, $3E, $10, $76, $C6, $A7, $8B, $39, $43, $E1,
    $3A, $B5, $56, $2A, $C0, $6D, $B3, $05, $22, $66, $BF, $DC, $0B, $FA, $62, $48,
    $DD, $20, $11, $06, $36, $C9, $C1, $CF, $F6, $27, $52, $BB, $69, $F5, $D4, $87,
    $7F, $84, $4C, $D2, $9C, $57, $A4, $BC, $4F, $9A, $DF, $FE, $D6, $8D, $7A, $EB,
    $2B, $53, $D8, $5C, $A1, $14, $17, $FB, $23, $D5, $7D, $30, $67, $73, $08, $09,
    $EE, $B7, $70, $3F, $61, $B2, $19, $8E, $4E, $E5, $4B, $93, $8F, $5D, $DB, $A9,
    $AD, $F1, $AE, $2E, $CB, $0D, $FC, $F4, $2D, $46, $6E, $1D, $97, $E8, $D1, $E9,
    $4D, $37, $A5, $75, $5E, $83, $9E, $AB, $82, $9D, $B9, $1C, $E0, $CD, $49, $89,
    $01, $B6, $BD, $58, $24, $A2, $5F, $38, $78, $99, $15, $90, $50, $B8, $95, $E4,
    $D0, $91, $C7, $CE, $ED, $0F, $B4, $6F, $A0, $CC, $F0, $02, $4A, $79, $C3, $DE,
    $A3, $EF, $EA, $51, $E6, $6B, $18, $EC, $1B, $2C, $80, $F7, $74, $E7, $FF, $21,
    $5A, $6A, $54, $1E, $41, $31, $92, $35, $C4, $33, $07, $0A, $BA, $7E, $0E, $34,
    $88, $B1, $98, $7C, $F3, $3D, $60, $6C, $7B, $CA, $D3, $1F, $32, $65, $04, $28,
    $64, $BE, $85, $9B, $2F, $59, $8A, $D7, $B0, $25, $AC, $AF, $12, $03, $E2, $F2
  );

  EK_D: array[0..15] of Cardinal = (
    $44D7,  $26BC,  $626B,  $135E,  $5789,  $35E2,  $7135,  $09AF,
    $4D78,  $2F13,  $6BC4,  $1AF1,  $5E26,  $3C4D,  $789A,  $47AC
  );

var
  LFSR_S: array[0..15] of Cardinal = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  F_R1: Cardinal = 0;
  F_R2: Cardinal = 0;
  BRC_X: array[0..3] of Cardinal = (0, 0, 0, 0);
  // 以上四个变量均在 ZUCInitialization 中被初始化

// 32 位处理平台上，两个 31 比特字 a 和 b 模 2^31-1 加法运算 c =a + b mod(2^31-1)
// 可以通过下面的两步计算实现：
function AddM(A, B: Cardinal): Cardinal;
var
  C: Cardinal;
begin
  C := A + B;
  Result := (C and $7FFFFFFF) + (C shr 31);
end;

function MulByPow2(X, K: Cardinal): Cardinal;
begin
  Result :=  ((X shl K) or (X shr (31 - K))) and $7FFFFFFF;
end;

procedure ZUCLFSRWithInitializationMode(U: Cardinal);
var
  F, V, I: Cardinal;
begin
  F := LFSR_S[0];
  V := MulByPow2(LFSR_S[0], 8);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[4], 20);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[10], 21);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[13], 17);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[15], 15);
  F := AddM(F, V);

  F := AddM(F, U);

  for I := 0 to 14 do
    LFSR_S[I] := LFSR_S[I + 1];

  LFSR_S[15] := F;
end;

procedure ZUCLFSRWithWorkMode;
var
  F, V, I: Cardinal;
begin
  F := LFSR_S[0];
  V := MulByPow2(LFSR_S[0], 8);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[4], 20);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[10], 21);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[13], 17);
  F := AddM(F, V);

  V := MulByPow2(LFSR_S[15], 15);
  F := AddM(F, V);

  for I := 0 to 14 do
    LFSR_S[I] := LFSR_S[I + 1];

  LFSR_S[15] := F;
end;

procedure ZUCBitReorganization;
begin
  BRC_X[0] := ((LFSR_S[15] and $7FFF8000) shl 1) or (LFSR_S[14] and $FFFF);
  BRC_X[1] := ((LFSR_S[11] and $FFFF) shl 16) or (LFSR_S[9] shr 15);
  BRC_X[2] := ((LFSR_S[7] and $FFFF) shl 16) or (LFSR_S[5] shr 15);
  BRC_X[3] := ((LFSR_S[2] and $FFFF) shl 16) or (LFSR_S[0] shr 15);
end;

function ROT(A: Cardinal; K: Cardinal): Cardinal;
begin
  Result := (A shl K) or (A shr (32 - k));
end;

function L1(X: Cardinal): Cardinal;
begin
  Result := (X xor ROT(X, 2) xor ROT(X, 10) xor ROT(X, 18) xor ROT(X, 24));
end;

function L2(X: Cardinal): Cardinal;
begin
  Result := (X xor ROT(X, 8) xor ROT(X, 14) xor ROT(X, 22) xor ROT(X, 30));;
end;

function MakeDWord(A, B, C, D: Cardinal): Cardinal;
begin
  Result := (A shl 24) or (B shl 16) or (C shl 8) or D;
end;

// 非线性函数
function F: Cardinal;
var
  W, W1, W2, U, V: Cardinal;
begin
  W := (BRC_X[0] xor F_R1) + F_R2;
  W1 := F_R1 + BRC_X[1];
  W2 := F_R2 xor BRC_X[2];
  U := L1((W1 shl 16) or (W2 shr 16));
  V := L2((W2 shl 16) or (W1 shr 16));
  F_R1 := MakeDWord(S0[U shr 24], S1[(U shr 16) and $FF], S0[(U shr 8) and $FF], S1[U and $FF]);
  F_R2 := MakeDWord(S0[V shr 24], S1[(V shr 16) and $FF], S0[(V shr 8) and $FF], S1[V and $FF]);
  Result := W;
end;

function MakeUInt31(A, B, C: Cardinal): Cardinal;
begin
  Result := (A shl 23) or (B shl 8) or C;
end;

procedure ZUCInitialization(Key: PByte; IV: PByte);
var
  W, NC: Cardinal;
  I: Integer;
begin
  for I := 0 to 16 do
    LFSR_S[I] := MakeUInt31((PByte(TCnNativeInt(Key) + I))^, EK_D[I], (PByte(TCnNativeInt(IV) + I))^);

  F_R1 := 0;
  F_R2 := 0;
  NC := 32;
  while NC > 0 do
  begin
    ZUCBitReorganization();
    W := F();
    ZUCLFSRWithInitializationMode(W shr 1);
    Dec(NC);
  end;
end;

procedure ZUCGenerateKeyStream(KeyStream: PCardinal; KeyStreamLen: Cardinal);
var
  I: Integer;
begin
  if (KeyStream = nil) or (KeyStreamLen = 0) then
    Exit;

  ZUCBitReorganization();
  F();
  ZUCLFSRWithWorkMode();

  for I := 0 to KeyStreamLen - 1 do
  begin
    ZUCBitReorganization();
    (PCardinal(TCnNativeInt(KeyStream) + SizeOf(Cardinal) * I))^ := F() xor BRC_X[3];
    ZUCLFSRWithWorkMode();
  end;
end;

procedure ZUC(Key: PByte; IV: PByte; KeyStream: PCardinal; KeyStreamLen: Cardinal);
begin
  ZUCInitialization(Key, IV);
  ZUCGenerateKeyStream(KeyStream, KeyStreamLen);
end;

procedure ZUCEEA3(CK: PByte; Count, Bearer, Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; C: PCardinal);
var
  IV: array[0..15] of Byte;
  I: Integer;
  L, LB, K: Cardinal;
  Z: PCardinal;
  PC, PM, PZ: PCnLongWord32Array;
begin
  L := (BitLen + 31) div 32;                   // 四字节为单位的长度，末块已补齐
  LB := L * 32 - BitLen;                       // 最后一块四字节的有效位数
  Z := PCardinal(GetMemory(L * SizeOf(Cardinal)));

  IV[0] := (Count shr 24) and $FF;
  IV[1] := (Count shr 16) and $FF;
  IV[2] := (Count shr 8) and $FF;
  IV[3] := Count and $FF;

  IV[4] := ((Bearer shl 3) or ((Direction and 1) shl 2)) and $FC;
  IV[5] := 0;
  IV[6] := 0;
  IV[7] := 0;

  IV[8] := IV[0];
  IV[9] := IV[1];
  IV[10] := IV[2];
  IV[11] := IV[3];
  IV[12] := IV[4];
  IV[13] := IV[5];
  IV[14] := IV[6];
  IV[15] := IV[7];

  ZUC(CK, @IV[0], Z, L);

  PC := PCnLongWord32Array(C);
  PM := PCnLongWord32Array(M);
  PZ := PCnLongWord32Array(Z);

  for I := 0 to L - 1 do
    PC^[I] := PM^[I] xor PZ^[I];

  if LB > 0 then // 最后一块不满四字节的话，最后一个 Cardinal 结果里超出有效 Bit 的部分应该填 0，也就是只保留低 Bit 的结果
  begin
    K := $100000000 - (1 shl LB); // FPC 下写一块会出 Internal Error，分开独立用个新变量 K，作为低 LB 位全 0 的 Mask
    PC^[L - 1] := PC^[L - 1] and K;
  end;
  FreeMemory(Z);
end;

// 取内存块第 I 个 Bit 起的一个 DWORD，I 从 0 开始
function GetDWord(Data: PCardinal; I: Integer): Cardinal;
var
  T: Integer;
begin
  T := I mod 32;
  I := I div 32;

  if T = 0 then
    Result := PCardinal(TCnNativeInt(Data) + SizeOf(Cardinal) * I)^
  else
    Result := (PCardinal(TCnNativeInt(Data) + SizeOf(Cardinal) * I)^ shl T) or
      (PCardinal(TCnNativeInt(Data) + SizeOf(Cardinal) * (I + 1))^ shr (32 - T));
end;

// 取内存块第 I 个 Bit 起的一个 Bit，I 从 0 开始，返回 0 或 1
function GetBit(Data: PCardinal; I: Integer): Byte;
var
  A, B: Cardinal;
begin
  A := PCardinal(TCnNativeInt(Data) + SizeOf(Cardinal) * (I div 32))^;
  B := 1 shl (31 - (I mod 32));
  if (A and B) <> 0 then
    Result := 1
  else
    Result := 0;
end;

procedure ZUCEIA3(IK: PByte; Count, Bearer, Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; out Mac: Cardinal);
var
  IV: array[0..15] of Byte;
  T: Cardinal;
  I, N, L: Integer;
  Z: PCardinal;
begin
  IV[0] := (Count shr 24) and $FF;
  IV[1] := (Count shr 16) and $FF;
  IV[2] := (Count shr 8) and $FF;
  IV[3] := Count and $FF;

  IV[4] := (Bearer shl 3) and $F8;
  IV[5] := 0;
  IV[6] := 0;
  IV[7] := 0;

  IV[8] := ((Count shr 24) and $FF) xor ((Direction and 1) shl 7);
  IV[9] := (Count shr 16) and $FF;
  IV[10] := (Count shr 8) and $FF;
  IV[11] := Count and $FF;

  IV[12] := IV[4];
  IV[13] := IV[5];
  IV[14] := IV[6] xor ((Direction and 1) shl 7);
  IV[15] := IV[7];

  N := BitLen + 64;
  L := (N + 31) div 32;
  Z := PCardinal(GetMemory(L * SizeOf(Cardinal)));

  ZUC(IK, @IV[0], Z, L);
  T := 0;
  for I := 0 to BitLen - 1 do
  begin
    if GetBit(M, I) <> 0 then
      T := T xor GetDWord(Z, I);
  end;
  T := T xor GetDWord(Z, BitLen);

  Mac := T xor PCardinal(TCnNativeInt(Z) + SizeOf(Cardinal) * (L - 1))^;
  FreeMemory(Z);
end;

end.
