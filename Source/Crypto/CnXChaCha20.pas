{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     ?§Û?????????????????????????                         }
{                   (C)Copyright 2001-2023 CnPack ??????                       }
{                   ------------------------------------                       }
{                                                                              }
{            ????????????????????????????????? CnPack ?????§¿??????        }
{        ??????¡¤??????????                                                }
{                                                                              }
{            ????????????????????????????????????¦Ê¦Å????????????        }
{        ???????????????????????????????????? CnPack ????§¿?î•        }
{                                                                              }
{            ???????????????????????? CnPack ????§¿???????????        }
{        ????§µ????????????????                                            }
{                                                                              }
{            ????????http://www.cnpack.org                                   }
{            ?????????master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnXChaCha20;

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative;

const
  CN_XCHACHA_STATE_SIZE   = 21; // Update to 21 to accommodate the larger Nonce and Counter
  CN_XCHACHA_KEY_SIZE     = 64; // 512-bit key (64 bytes)
  CN_XCHACHA_NONCE_SIZE   = 32; // 256-bit nonce (32 bytes)
  CN_XCHACHA_COUNT_SIZE   = 4;

type
  TCnXChaChaKey = array[0..CN_XCHACHA_KEY_SIZE - 1] of Byte; // 512-bit key (64 bytes)
  TCnXChaChaNonce = array[0..CN_XCHACHA_NONCE_SIZE - 1] of Byte; // 256-bit nonce (32 bytes)
  TCnXChaChaCounter = Cardinal;
  TCnXChaChaState = array[0..CN_XCHACHA_STATE_SIZE - 1] of Cardinal; // 21 words (84 bytes) - Update the size

procedure XChaCha20Block(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  Counter: TCnXChaChaCounter; var OutState: TCnXChaChaState);

function XChaCha20EncryptBytes(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: TBytes): TBytes;

function XChaCha20DecryptBytes(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: TBytes): TBytes;

function XChaCha20EncryptData(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;

function XChaCha20DecryptData(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;

implementation

const
  XCHACHA20_CONST0 = $61707865;
  XCHACHA20_CONST1 = $3320646E;
  XCHACHA20_CONST2 = $79622D32;
  XCHACHA20_CONST3 = $6B206574;

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

procedure QuarterRoundState(var State: TCnXChaChaState; A, B, C, D: Integer);
begin
  QuarterRound(State[A], State[B], State[C], State[D]);
end;

procedure BuildState(var State: TCnXChaChaState; var Key: TCnXChaChaKey;
  var Nonce: TCnXChaChaNonce; Counter: TCnXChaChaCounter);
begin
  State[0] := XCHACHA20_CONST0;
  State[1] := XCHACHA20_CONST1;
  State[2] := XCHACHA20_CONST2;
  State[3] := XCHACHA20_CONST3;

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
  State[16] := PCardinal(@Nonce[12])^;
  State[17] := PCardinal(@Nonce[16])^;
  State[18] := PCardinal(@Nonce[20])^;
  State[19] := PCardinal(@Nonce[24])^;
  State[20] := PCardinal(@Nonce[28])^;
end;

procedure XChaCha20InnerBlock(var State: TCnXChaChaState);
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

procedure XChaCha20Block(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  Counter: TCnXChaChaCounter; var OutState: TCnXChaChaState);
var
  I: Integer;
  State: TCnXChaChaState;
begin
  BuildState(State, Key, Nonce, Counter);
  Move(State[0], OutState[0], SizeOf(TCnXChaChaState));

  for I := 1 to 10 do
    XChaCha20InnerBlock(OutState);

  for I := Low(TCnXChaChaState) to High(TCnXChaChaState) do
    OutState[I] := OutState[I] + State[I];
end;

function XChaCha20Data(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
var
  I, J, L, B: Integer;
  Cnt: TCnXChaChaCounter;
  Stream: TCnXChaChaState;
  P, Q, M: PByteArray;
begin
  Result := False;
  if (Data = nil) or (DataByteLength <= 0) or (Output = nil) then
    Exit;

  Cnt := 1;
  B := DataByteLength div (SizeOf(Cardinal) * CN_XCHACHA_STATE_SIZE);
  P := PByteArray(Data);
  Q := PByteArray(Output);
  M := PByteArray(@Stream[0]);

  if B > 0 then
  begin
    for I := 1 to B do
    begin
      XChaCha20Block(Key, Nonce, Cnt, Stream);

      for J := 0 to SizeOf(Cardinal) * CN_XCHACHA_STATE_SIZE - 1 do
        Q^[J] := P^[J] xor M^[J];

      P := PByteArray(TCnNativeInt(P) + SizeOf(Cardinal) * CN_XCHACHA_STATE_SIZE);
      Q := PByteArray(TCnNativeInt(Q) + SizeOf(Cardinal) * CN_XCHACHA_STATE_SIZE);

      Inc(Cnt);
    end;
  end;

  L := DataByteLength mod (SizeOf(Cardinal) * CN_XCHACHA_STATE_SIZE);
  if L > 0 then
  begin
    XChaCha20Block(Key, Nonce, Cnt, Stream);

    for J := 0 to L - 1 do
      Q^[J] := P^[J] xor M^[J];
  end;
  Result := True;
end;

function XChaCha20EncryptBytes(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
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

function XChaCha20DecryptBytes(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
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

function XChaCha20EncryptData(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := XChaCha20Data(Key, Nonce, Data, DataByteLength, Output);
end;

function XChaCha20DecryptData(var Key: TCnXChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
begin
  Result := XChaCha20Data(Key, Nonce, EnData, DataByteLength, Output);
end;

end.
