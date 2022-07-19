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

unit CnChaCha20;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：ChaCha20 流密码算法实现单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：根据 RFC 7539 实现，其中 nonce 类似于初始化向量
*           块运算；输入 32 字节 Key、12 字节 nonce、4 字节 Counter，输出 64 字节内容
*           流运算：输入 32 字节 Key、12 字节 nonce、4 字节 Counter，任意长度明/密文
*                   输出相同长度密/明文
* 开发平台：Windows 7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.07.19 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNativeDecl;

const
  CHACHA_STATE_SIZE   = 16;
  {* ChaCha20 算法的状态块数，64 字节}

  CHACHA_KEY_SIZE     = 32;
  {* ChaCha20 算法的 Key 字节长度}

  CHACHA_NONCE_SIZE   = 12;
  {* ChaCha20 算法的 Nonce 字节长度}

  CHACHA_COUNT_SIZE   = 4;
  {* ChaCha20 算法的计数器字节长度，实际运算时使用 TCnLongWord32 代替}

type
  TChaChaKey = array[0..CHACHA_KEY_SIZE - 1] of Byte;
  {* ChaCha20 算法的 Key}

  TChaChaNonce = array[0..CHACHA_NONCE_SIZE - 1] of Byte;
  {* ChaCha20 算法的 Nonce}

  TChaChaCounter = TCnLongWord32;
  {* ChaCha20 算法的计数器}

  TChaChaState = array[0..CHACHA_STATE_SIZE - 1] of TCnLongWord32;
  {* ChaCha20 算法的状态块}

procedure ChaCha20Block(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  Counter: TChaChaCounter; var OutState: TChaChaState);
{* 进行一次块运算，包括 20 轮的子运算}

function ChaCha20EncryptBytes(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  Data: TBytes): TBytes;
{* 对字节数组进行 ChaCha20 加密}

function ChaCha20DecryptBytes(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  EnData: TBytes): TBytes;
{* 对字节数组进行 ChaCha20 解密}

function ChaCha20EncryptData(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* 对 Data 所指的 DataByteLength 长度的数据块进行 ChaCha20 加密，
  密文放 Output 所指的内存，要求长度至少能容纳 DataByteLength}

function ChaCha20DecryptData(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* 对 Data 所指的 DataByteLength 长度的密文数据块进行 ChaCha20 解密，
  明文放 Output 所指的内存，要求长度至少能容纳 DataByteLength}

implementation

const
  CHACHA20_CONST0 = $61707865;
  CHACHA20_CONST1 = $3320646E;
  CHACHA20_CONST2 = $79622D32;
  CHACHA20_CONST3 = $6B206574;

procedure ROT(var X: TCnLongWord32; N: BYTE);
begin
  X := (X shl N) or (X shr (32 - N));
end;

procedure QuarterRound(var A, B, C, D: TCnLongWord32);
begin
  A := A + B;
  D := D xor A;
  ROT(D, 16);

  C := C + D;
  B := B xor C;
  ROT(B, 12);

  A := A + B;
  D := D xor A;
  ROT(D, 8);

  C := C + D;
  B := B xor C;
  ROT(B, 7);
end;

procedure QuarterRoundState(var State: TChaChaState; A, B, C, D: Integer);
begin
  QuarterRound(State[A], State[B], State[C], State[D]);
end;

procedure BuildState(var State: TChaChaState; var Key: TChaChaKey;
  var Nonce: TChaChaNonce; Counter: TChaChaCounter);
begin
  State[0] := CHACHA20_CONST0;
  State[1] := CHACHA20_CONST1;
  State[2] := CHACHA20_CONST2;
  State[3] := CHACHA20_CONST3;

  State[4] := PCnLongWord32(@Key[0])^;
  State[5] := PCnLongWord32(@Key[4])^;
  State[6] := PCnLongWord32(@Key[8])^;
  State[7] := PCnLongWord32(@Key[12])^;
  State[8] := PCnLongWord32(@Key[16])^;
  State[9] := PCnLongWord32(@Key[20])^;
  State[10] := PCnLongWord32(@Key[24])^;
  State[11] := PCnLongWord32(@Key[28])^;

  State[12] := Counter;

  State[13] := PCnLongWord32(@Nonce[0])^;
  State[14] := PCnLongWord32(@Nonce[4])^;
  State[15] := PCnLongWord32(@Nonce[8])^;
end;

procedure ChaCha20InnerBlock(var State: TChaChaState);
begin
  QuarterRoundState(State, 0, 4, 8, 12);
  QuarterRoundState(State, 1, 5, 9, 13);
  QuarterRoundState(State, 2, 6, 10, 14);
  QuarterRoundState(State, 3, 7, 11, 15);

  QuarterRoundState(State, 0, 5, 10, 15);
  QuarterRoundState(State, 1, 6, 11, 12);
  QuarterRoundState(State, 2, 7, 8, 13);
  QuarterRoundState(State, 3, 4, 9, 14);
end;

procedure ChaCha20Block(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  Counter: TChaChaCounter; var OutState: TChaChaState);
var
  I: Integer;
  State: TChaChaState;
begin
  BuildState(State, Key, Nonce, Counter);
  Move(State[0], OutState[0], SizeOf(TChaChaState));

  for I := 1 to 10 do
    ChaCha20InnerBlock(OutState);

  for I := Low(TChaChaState) to High(TChaChaState) do
    OutState[I] := OutState[I] + State[I];
end;

function ChaCha20Data(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
var
  I, J, L, B: Integer;
  Cnt: TChaChaCounter;
  Stream: TChaChaState;
  P, Q, M: PByteArray;
begin
  Result := False;
  if (Data = nil) or (DataByteLength <= 0) or (Output = nil) then
    Exit;

  Cnt := 1;
  B := DataByteLength div (SizeOf(TCnLongWord32) * CHACHA_STATE_SIZE); // 有 B 个完整块
  P := PByteArray(Data);
  Q := PByteArray(Output);
  M := PByteArray(@Stream[0]);

  if B > 0 then
  begin
    for I := 1 to B do
    begin
      ChaCha20Block(Key, Nonce, Cnt, Stream);

      // P、Q 已各指向要处理的原始块与密文块
      for J := 0 to SizeOf(TCnLongWord32) * CHACHA_STATE_SIZE - 1 do
        Q^[J] := P^[J] xor M[J];

      // 指向下一块
      P := PByteArray(TCnNativeInt(P) + SizeOf(TCnLongWord32) * CHACHA_STATE_SIZE);
      Q := PByteArray(TCnNativeInt(Q) + SizeOf(TCnLongWord32) * CHACHA_STATE_SIZE);

      Inc(Cnt);
    end;
  end;

  L := DataByteLength mod (SizeOf(TCnLongWord32) * CHACHA_STATE_SIZE);
  if L > 0 then // 还有剩余块，长度为 L
  begin
    ChaCha20Block(Key, Nonce, Cnt, Stream);

    // P、Q 已各指向要处理的原始块与密文块
    for J := 0 to L - 1 do
      Q^[J] := P^[J] xor M[J];
  end;
end;

function ChaCha20EncryptBytes(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  Data: TBytes): TBytes;
var
  L: Integer;
begin
  Result := nil;
  if Data = nil then
    Exit;

  L := Length(Data);
  if L > 0 then
  begin
    SetLength(Result, L);
    if not ChaCha20Data(Key, Nonce, @Data[0], L, @Result[0]) then
      SetLength(Result, 0);
  end;
end;

function ChaCha20DecryptBytes(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  EnData: TBytes): TBytes;
var
  L: Integer;
begin
  Result := nil;
  if EnData = nil then
    Exit;

  L := Length(EnData);
  if L > 0 then
  begin
    SetLength(Result, L);
    if not ChaCha20Data(Key, Nonce, @EnData[0], L, @Result[0]) then
      SetLength(Result, 0);
  end;
end;

function ChaCha20EncryptData(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := ChaCha20Data(Key, Nonce, Data, DataByteLength, Output);
end;

function ChaCha20DecryptData(var Key: TChaChaKey; var Nonce: TChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := ChaCha20Data(Key, Nonce, EnData, DataByteLength, Output);
end;

end.
