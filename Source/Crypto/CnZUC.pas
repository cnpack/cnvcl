{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnZUC;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ����������������֮�㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack �����飨master@cnpack.org)
* ��    ע������Ԫʵ���˹������������еĻ������ֽ��������֮�ӽ����㷨��
*           �ο������㷨�����ĵ������֮���������㷨 ZUC stream cipher algorithm��
*           GM/T 0001.1-2012 GB/T33133.2-2021 ��Ӣ�ĵ�
*           ��Specification of the 3GPP Confidentiality and Integrity
*           Algorithms 128-EEA3 & 128-EIA3�����ĵ������ο���ֲ�� C ʵ�ִ��롣
*
*           ע�� ZUCEEA3 �ӽ��ܵ�����ͬ��Ҳ��������������ͬ������£������ĵõ����ģ������ĵõ�����
*           ���֮�㷨�Ļ������������������㷨��Ȼ֧������λ���ı�����������Ҫ������������ڴ�����
*           �ɷ����ֽڳ�����Ϊ���ֽڵ���������Ҳ���� ((BitLen + 31) div 32) �� Cardinal����
*           ���������ڲ���Ч������ʵ�Ҫ�󣬴���ǰ�����ڴ泤��ʱӦע�⡣
*
*           ���ֽ��ڲ��Ĵ�С��δ�����淶��Ҫ�����ݾ�Ϊ��ˣ�
*           �����С�� CPU ����Ҫ�ߵ����ֽ��ڲ������ݣ�Ŀǰδʵ�֡�
*
* ����ƽ̨��Windows 7 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2022.04.26 V1.2
*               �޸� LongWord �� Integer ��ַת����֧�� MacOS64
*           2019.04.15 V1.1
*               ֧�� Win32/Win64/MacOS32
*           2017.04.20 V1.0
*               ��ֲ��������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative;

const
  CN_ZUC_KEYSIZE = 16;
  {* ���֮�㷨�����볤�ȣ�16 �ֽ�}

procedure ZUC(Key: PByte; Iv: PByte; KeyStream: PCardinal; KeyStreamLen: Cardinal);
{* ���֮�����㷨����������� 16 �ֽ������ 16 �ֽڳ�ʼ��������
   ���� KeyStreamLen �����ֽ��������з��� KeyStream ��ָ���ڴ��С�

   ������
     Key: PByte                           - 16 �ֽ�������ڴ��ַ
     Iv: PByte                            - 16 �ֽڳ�ʼ���������ڴ��ַ
     KeyStream: PCardinal                 - ����������ݿ��ַ������ӦΪ KeyStreamLen * SizeOf(Cardinal)
     KeyStreamLen: Cardinal               - �����ĳ��ȣ���λΪ���ֽ�

   ����ֵ�����ޣ�
}

function ZUCEEA3(CK: PByte; Count: Cardinal; Bearer: Cardinal; Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; C: PCardinal): Cardinal;
{* �������֮�㷨�� 128-EEA3 �����Ա����㷨��һ��������ϵͳ��ʹ�û�������Կ CK ���ӽ������ݿ顣
   �������롢���ء�����Ȳ����������������ݼ����������ݡ���������ֵΪ������ݵ��ֽڳ��ȡ�
   ע����Ϊ���������ܡ�������������ģ�����һ�α�������ģ�����������ģ������һ�λỹԭ�����ġ�

   ������
     CK: PByte                            - 16 �ֽڻ�������Կ���ڴ��ַ
     Count: Cardinal                      - 4 �ֽڼ�����
     Bearer: Cardinal                     - 4 �ֽڸ��ر�ʶ����Ч���� 5 λ
     Direction: Cardinal                  - 4 �ֽڴ��䷽����Ч���� 1 λ
     M: PCardinal                         - ������������ڴ���ַ����ʵ�ʿɷ��ʵı��س�������ڻ���� BitLen �������ֽڵı���Ҳ���� 32 ����������
                                           ��ʵ�ʿɷ��ʵ��ֽڳ������� 4 �������������� 8 ��Ҫ���ڵ��� BitLen���Ա������ڲ����ֽڸ�Ч����
     BitLen: Cardinal                     - �����������λ���ȣ������Ƿ��ֽ�������
     C: PCardinal                         - ���洢������������ڴ���ַ�������ɵ�λ���ȱ�����ڻ���� BitLen���������ֽڵı���Ҳ���� 32 ��������

   ����ֵ��Cardinal                       - ����������ݵ��ֽڳ���
}

procedure ZUCEIA3(IK: PByte; Count: Cardinal; Bearer: Cardinal; Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; out Mac: Cardinal);
{* �������֮�㷨�� 128-EIA3 �����Ա����㷨��ʹ��һ����������Կ IK �Ը�����������Ϣ�����һ�� 32 λ�� MAC ֵ��
   �������롢���ء�����Ȳ����������������ݼ���� MAC ֵ��

   ������
     IK: PByte                            - 16 �ֽ���������Կ���ڴ��ַ
     Count: Cardinal                      - 4 �ֽڼ�����
     Bearer: Cardinal                     - 4 �ֽڸ��ر�ʶ����Ч���� 5 λ
     Direction: Cardinal                  - 4 �ֽڴ��䷽����Ч���� 1 λ
     M: PCardinal                         - ������������ڴ��ַ����ʵ�ʿɷ��ʵı��س�������ڻ���� BitLen �������ֽڵı���Ҳ���� 32 ����������
                                           ��ʵ�ʿɷ��ʵ��ֽڳ������� 4 �������������� 8 ��Ҫ���ڵ��� BitLen���Ա������ڲ����ֽڸ�Ч����
     BitLen: Cardinal                     - �����������λ���ȣ������Ƿ��ֽ�������
     out Mac: Cardinal                    - ��� 4 �ֽ� MAC ֵ��Ҳ�� 32 λ

   ����ֵ�����ޣ�
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

  EK_D: array[0..CN_ZUC_KEYSIZE - 1] of Cardinal = (
    $44D7,  $26BC,  $626B,  $135E,  $5789,  $35E2,  $7135,  $09AF,
    $4D78,  $2F13,  $6BC4,  $1AF1,  $5E26,  $3C4D,  $789A,  $47AC
  );

var
  LFSR_S: array[0..CN_ZUC_KEYSIZE - 1] of Cardinal = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  F_R1: Cardinal = 0;
  F_R2: Cardinal = 0;
  BRC_X: array[0..3] of Cardinal = (0, 0, 0, 0);
  // �����ĸ��������� ZUCInitialization �б���ʼ��

// 32 λ����ƽ̨�ϣ����� 31 ������ a �� b ģ 2^31-1 �ӷ����� c =a + b mod(2^31-1)
// ����ͨ���������������ʵ�֣�
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
  Result := (X xor ROT(X, 8) xor ROT(X, 14) xor ROT(X, 22) xor ROT(X, 30));
end;

function MakeDWord(A, B, C, D: Cardinal): Cardinal;
begin
  Result := (A shl 24) or (B shl 16) or (C shl 8) or D;
end;

// �����Ժ���
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

procedure ZUCInitialization(Key: PByte; Iv: PByte);
var
  W, NC: Cardinal;
  I: Integer;
begin
  for I := 0 to CN_ZUC_KEYSIZE - 1 do
    LFSR_S[I] := MakeUInt31((PByte(TCnIntAddress(Key) + I))^, EK_D[I], (PByte(TCnIntAddress(Iv) + I))^);

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
    (PCardinal(TCnIntAddress(KeyStream) + SizeOf(Cardinal) * I))^ := F() xor BRC_X[3];
    ZUCLFSRWithWorkMode();
  end;
end;

procedure ZUC(Key: PByte; Iv: PByte; KeyStream: PCardinal; KeyStreamLen: Cardinal);
begin
  ZUCInitialization(Key, Iv);
  ZUCGenerateKeyStream(KeyStream, KeyStreamLen);
end;

function ZUCEEA3(CK: PByte; Count, Bearer, Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; C: PCardinal): Cardinal;
var
  Iv: array[0..CN_ZUC_KEYSIZE - 1] of Byte;
  I: Integer;
  L, LB, K: Cardinal;
  Z: PCardinal;
  PC, PM, PZ: PCnLongWord32Array;
begin
  L := (BitLen + 31) div 32;                   // ���ֽ�Ϊ��λ�ĳ��ȣ�ĩ���Ѳ��볤��
  Result := L * SizeOf(Cardinal);
  if C = nil then                              // ����ֵΪ��������ĵ��ֽ���
    Exit;

  LB := L * 32 - BitLen;                       // ���һ�����ֽڵ���Чλ��
  Z := PCardinal(GetMemory(Result));

  try
    Iv[0] := (Count shr 24) and $FF;
    Iv[1] := (Count shr 16) and $FF;
    Iv[2] := (Count shr 8) and $FF;
    Iv[3] := Count and $FF;

    Iv[4] := ((Bearer shl 3) or ((Direction and 1) shl 2)) and $FC;
    Iv[5] := 0;
    Iv[6] := 0;
    Iv[7] := 0;

    Iv[8] := Iv[0];
    Iv[9] := Iv[1];
    Iv[10] := Iv[2];
    Iv[11] := Iv[3];
    Iv[12] := Iv[4];
    Iv[13] := Iv[5];
    Iv[14] := Iv[6];
    Iv[15] := Iv[7];

    ZUC(CK, @Iv[0], Z, L);

    PC := PCnLongWord32Array(C);
    PM := PCnLongWord32Array(M);
    PZ := PCnLongWord32Array(Z);

    for I := 0 to L - 1 do
      PC^[I] := PM^[I] xor PZ^[I];

    if LB > 0 then // ���һ�鲻�����ֽڵĻ������һ�� Cardinal ����ﳬ����Ч Bit �Ĳ���Ӧ���� 0��Ҳ����ֻ������ Bit �Ľ��
    begin
      K := $100000000 - (1 shl LB); // FPC ��дһ���� Internal Error���ֿ������ø��±��� K����Ϊ�� LB λȫ 0 �� Mask
      PC^[L - 1] := PC^[L - 1] and K;
    end;
  finally
    FreeMemory(Z);
  end;
end;

// ȡ�ڴ��� I �� Bit ���һ�� DWORD��I �� 0 ��ʼ
function GetDWord(Data: PCardinal; I: Integer): Cardinal;
var
  T: Integer;
begin
  T := I mod 32;
  I := I div 32;

  if T = 0 then
    Result := PCardinal(TCnIntAddress(Data) + SizeOf(Cardinal) * I)^
  else
    Result := (PCardinal(TCnIntAddress(Data) + SizeOf(Cardinal) * I)^ shl T) or
      (PCardinal(TCnIntAddress(Data) + SizeOf(Cardinal) * (I + 1))^ shr (32 - T));
end;

// ȡ�ڴ��� I �� Bit ���һ�� Bit��I �� 0 ��ʼ������ 0 �� 1
function GetBit(Data: PCardinal; I: Integer): Byte;
var
  A, B: Cardinal;
begin
  A := PCardinal(TCnIntAddress(Data) + SizeOf(Cardinal) * (I div 32))^;
  B := 1 shl (31 - (I mod 32));
  if (A and B) <> 0 then
    Result := 1
  else
    Result := 0;
end;

procedure ZUCEIA3(IK: PByte; Count, Bearer, Direction: Cardinal;
  M: PCardinal; BitLen: Cardinal; out Mac: Cardinal);
var
  Iv: array[0..15] of Byte;
  T: Cardinal;
  I, N, L: Integer;
  Z: PCardinal;
begin
  Iv[0] := (Count shr 24) and $FF;
  Iv[1] := (Count shr 16) and $FF;
  Iv[2] := (Count shr 8) and $FF;
  Iv[3] := Count and $FF;

  Iv[4] := (Bearer shl 3) and $F8;
  Iv[5] := 0;
  Iv[6] := 0;
  Iv[7] := 0;

  Iv[8] := ((Count shr 24) and $FF) xor ((Direction and 1) shl 7);
  Iv[9] := (Count shr 16) and $FF;
  Iv[10] := (Count shr 8) and $FF;
  Iv[11] := Count and $FF;

  Iv[12] := Iv[4];
  Iv[13] := Iv[5];
  Iv[14] := Iv[6] xor ((Direction and 1) shl 7);
  Iv[15] := Iv[7];

  N := BitLen + 64;
  L := (N + 31) div 32;
  Z := PCardinal(GetMemory(L * SizeOf(Cardinal)));

  try
    ZUC(IK, @Iv[0], Z, L);
    T := 0;
    for I := 0 to BitLen - 1 do
    begin
      if GetBit(M, I) <> 0 then
        T := T xor GetDWord(Z, I);
    end;
    T := T xor GetDWord(Z, BitLen);

    Mac := T xor PCardinal(TCnIntAddress(Z) + SizeOf(Cardinal) * (L - 1))^;
  finally
    FreeMemory(Z);
  end;
end;

end.
