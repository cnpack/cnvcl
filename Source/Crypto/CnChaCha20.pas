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

unit CnChaCha20;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：ChaCha20 流密码算法实现单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：根据 RFC 7539 实现 ChaCha20 和草案实现 XChaCha20，其中 Nonce 类似于初始化向量
*           ChaCha20 块运算；输入 32 字节 Key、12 字节 Nonce、4 字节 Counter，输出 64 字节内容
*           ChaCha20 流运算：输入 32 字节 Key、12 字节 Nonce、4 字节 Counter，任意长度明/密文
*                    输出相同长度密/明文，内部 Counter 初始值默认使用 1
* 开发平台：Windows 7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2023.07.30 V1.1
*               根据草案实现 XChaCha20，包括 HChaCha20 的 Key 生成算法
*           2022.07.19 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative;

const
  CN_CHACHA_STATE_SIZE   = 16;
  {* ChaCha20 算法的状态块数，64 字节}

  CN_CHACHA_KEY_SIZE     = 32;
  {* ChaCha20 算法的 Key 字节长度}

  CN_CHACHA_NONCE_SIZE   = 12;
  {* ChaCha20 算法的 Nonce 字节长度}

  CN_HCHACHA_NONCE_SIZE  = 16;
  {* HChaCha20 算法的 Nonce 字节长度，前 4 字节是 Counter}

  CN_XCHACHA_NONCE_SIZE   = 24;
  {* XChaCha20 算法的 Nonce 字节长度}

  CN_HCHACHA_SUBKEY_SIZE = 32;
  {* HChaCha20 算法的输出的 Key 的字节长度}

  CN_CHACHA_COUNT_SIZE   = 4;
  {* ChaCha20 算法的计数器字节长度，实际运算时使用 Cardinal 代替}

type
  TCnChaChaKey = array[0..CN_CHACHA_KEY_SIZE - 1] of Byte;
  {* ChaCha20 算法的 Key}

  TCnChaChaNonce = array[0..CN_CHACHA_NONCE_SIZE - 1] of Byte;
  {* ChaCha20 算法的 Nonce}

  TCnHChaChaNonce = array[0..CN_HCHACHA_NONCE_SIZE - 1] of Byte;
  {* HChaCha20 算法的 Nonce}

  TCnHChaChaSubKey = array[0..CN_HCHACHA_SUBKEY_SIZE - 1] of Byte;
  {* HChaCha20 算法的 SubKey}

  TCnXChaChaNonce = array[0..CN_XCHACHA_NONCE_SIZE - 1] of Byte;
  {* XChaCha20 算法的 Nonce}

  TCnChaChaCounter = Cardinal;
  {* ChaCha20 算法的计数器}

  TCnChaChaState = array[0..CN_CHACHA_STATE_SIZE - 1] of Cardinal;
  {* ChaCha20 算法的状态块}

procedure ChaCha20Block(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Counter: TCnChaChaCounter; var OutState: TCnChaChaState);
{* 进行一次块运算，包括 20 轮的子运算}

procedure HChaCha20SubKey(var Key: TCnChaChaKey; var Nonce: TCnHChaChaNonce;
  var OutSubKey: TCnHChaChaSubKey);
{* 进行一次 HChaCha20 块运算，包括 20 轮的子运算，输出 SubKey}

function ChaCha20EncryptBytes(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Data: TBytes): TBytes;
{* 对字节数组进行 ChaCha20 加密}

function ChaCha20DecryptBytes(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  EnData: TBytes): TBytes;
{* 对字节数组进行 ChaCha20 解密}

function ChaCha20EncryptData(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* 对 Data 所指的 DataByteLength 长度的数据块进行 ChaCha20 加密，
  密文放 Output 所指的内存，要求长度至少能容纳 DataByteLength}

function ChaCha20DecryptData(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* 对 Data 所指的 DataByteLength 长度的密文数据块进行 ChaCha20 解密，
  明文放 Output 所指的内存，要求长度至少能容纳 DataByteLength}

function XChaCha20EncryptBytes(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: TBytes): TBytes;
{* 对字节数组进行 XChaCha20 加密}

function XChaCha20DecryptBytes(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: TBytes): TBytes;
{* 对字节数组进行 XChaCha20 解密}

function XChaCha20EncryptData(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* 对 Data 所指的 DataByteLength 长度的数据块进行 XChaCha20 加密，
  密文放 Output 所指的内存，要求长度至少能容纳 DataByteLength}

function XChaCha20DecryptData(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* 对 Data 所指的 DataByteLength 长度的密文数据块进行 XChaCha20 解密，
  明文放 Output 所指的内存，要求长度至少能容纳 DataByteLength}

implementation

const
  CHACHA20_CONST0 = $61707865;
  CHACHA20_CONST1 = $3320646E;
  CHACHA20_CONST2 = $79622D32;
  CHACHA20_CONST3 = $6B206574;

procedure ROT(var X: Cardinal; N: BYTE);
begin
  X := (X shl N) or (X shr (32 - N));
end;

procedure QuarterRound(var A, B, C, D: Cardinal);
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

procedure QuarterRoundState(var State: TCnChaChaState; A, B, C, D: Integer);
begin
  QuarterRound(State[A], State[B], State[C], State[D]);
end;

procedure BuildState(var State: TCnChaChaState; var Key: TCnChaChaKey;
  var Nonce: TCnChaChaNonce; Counter: TCnChaChaCounter);
begin
  State[0] := CHACHA20_CONST0;
  State[1] := CHACHA20_CONST1;
  State[2] := CHACHA20_CONST2;
  State[3] := CHACHA20_CONST3;

  State[4] := PCardinal(@Key[0])^;
  State[5] := PCardinal(@Key[4])^;
  State[6] := PCardinal(@Key[8])^;
  State[7] := PCardinal(@Key[12])^;
  State[8] := PCardinal(@Key[16])^;
  State[9] := PCardinal(@Key[20])^;
  State[10] := PCardinal(@Key[24])^;
  State[11] := PCardinal(@Key[28])^;

  State[12] := Counter;

  State[13] := PCardinal(@Nonce[0])^;
  State[14] := PCardinal(@Nonce[4])^;
  State[15] := PCardinal(@Nonce[8])^;
end;

procedure ChaCha20InnerBlock(var State: TCnChaChaState);
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

procedure ChaCha20Block(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Counter: TCnChaChaCounter; var OutState: TCnChaChaState);
var
  I: Integer;
  State: TCnChaChaState;
begin
  BuildState(State, Key, Nonce, Counter);
  Move(State[0], OutState[0], SizeOf(TCnChaChaState));

  for I := 1 to 10 do
    ChaCha20InnerBlock(OutState);

  for I := Low(TCnChaChaState) to High(TCnChaChaState) do
    OutState[I] := OutState[I] + State[I];
end;

procedure HChaCha20SubKey(var Key: TCnChaChaKey; var Nonce: TCnHChaChaNonce;
  var OutSubKey: TCnHChaChaSubKey);
var
  I: Integer;
  Counter: TCnChaChaCounter;
  N: TCnChaChaNonce;
  State: TCnChaChaState;
begin
  Move(Nonce[0], Counter, SizeOf(Cardinal));
  Move(Nonce[4], N[0], SizeOf(TCnChaChaNonce));

  BuildState(State, Key, N, Counter);
  for I := 1 to 10 do
    ChaCha20InnerBlock(State);

  Move(State[0], OutSubKey[0], 16);
  Move(State[12], OutSubKey[16], 16);
end;

function ChaCha20Data(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce; Data: Pointer;
  DataByteLength: Integer; Output: Pointer; BlockCounter: TCnChaChaCounter = 1): Boolean;
var
  I, J, L, B: Integer;
  Cnt: TCnChaChaCounter;
  Stream: TCnChaChaState;
  P, Q, M: PByteArray;
begin
  Result := False;
  if (Data = nil) or (DataByteLength <= 0) or (Output = nil) then
    Exit;

  Cnt := BlockCounter;
  B := DataByteLength div (SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE); // 有 B 个完整块
  P := PByteArray(Data);
  Q := PByteArray(Output);
  M := PByteArray(@Stream[0]);

  if B > 0 then
  begin
    for I := 1 to B do
    begin
      ChaCha20Block(Key, Nonce, Cnt, Stream);

      // P、Q 已各指向要处理的原始块与密文块
      for J := 0 to SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE - 1 do
        Q^[J] := P^[J] xor M^[J];

      // 指向下一块
      P := PByteArray(TCnNativeInt(P) + SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE);
      Q := PByteArray(TCnNativeInt(Q) + SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE);

      Inc(Cnt);
    end;
  end;

  L := DataByteLength mod (SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE);
  if L > 0 then // 还有剩余块，长度为 L
  begin
    ChaCha20Block(Key, Nonce, Cnt, Stream);

    // P、Q 已各指向要处理的原始块与密文块
    for J := 0 to L - 1 do
      Q^[J] := P^[J] xor M^[J];
  end;
  Result := True;
end;

function ChaCha20EncryptBytes(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
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

function ChaCha20DecryptBytes(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
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

function ChaCha20EncryptData(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := ChaCha20Data(Key, Nonce, Data, DataByteLength, Output);
end;

function ChaCha20DecryptData(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := ChaCha20Data(Key, Nonce, EnData, DataByteLength, Output);
end;

function XChaCha20Data(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer; BlockCounter: TCnChaChaCounter = 1): Boolean;
var
  SubKey: TCnHChaChaSubKey;
  XKey: TCnChaChaKey;
  HN: TCnHChaChaNonce;
  N: TCnChaChaNonce;
begin
  Move(Nonce[0], HN[0], SizeOf(TCnHChaChaNonce)); // 取出 XChaCha20 的 24 字节 Nonce 的前 16 字节计算
  HChaCha20SubKey(Key, HN, SubKey);

  Move(SubKey[0], XKey[0], SizeOf(TCnChaChaKey)); // 计算的 HChaCha20 SubKey 作为 ChaCha20 的 Key
  N[0] := 0;                                      // 四字节 0 加上 XChaCha20 的剩下 8 字节共 12 字节作为 ChaCha20 的 Nonce
  N[1] := 0;
  N[2] := 0;
  N[3] := 0;
  Move(Nonce[16], N[4], CN_XCHACHA_NONCE_SIZE - CN_HCHACHA_NONCE_SIZE);

  Result := ChaCha20Data(XKey, N, Data, DataByteLength, Output, BlockCounter);
end;

function XChaCha20EncryptBytes(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
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
    if not XChaCha20Data(Key, Nonce, @Data[0], L, @Result[0]) then
      SetLength(Result, 0);
  end;
end;

function XChaCha20DecryptBytes(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
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
    if not XChaCha20Data(Key, Nonce, @EnData[0], L, @Result[0]) then
      SetLength(Result, 0);
  end;
end;

function XChaCha20EncryptData(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := XChaCha20Data(Key, Nonce, Data, DataByteLength, Output);
end;

function XChaCha20DecryptData(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := XChaCha20Data(Key, Nonce, EnData, DataByteLength, Output);
end;

end.
