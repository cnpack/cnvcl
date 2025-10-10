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

unit CnChaCha20;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�ChaCha20 �������㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ���� ChaCha20 ϵ��������ӽ����㷨������ ChaCha20 ���� RFC 7539 ʵ�֣�
*           XChaCha20 ���ݲݰ�ʵ�֡��㷨�е� Nonce �����ڳ�ʼ��������
*
*           ChaCha20 �����㣺���� 32 �ֽ� Key��12 �ֽ� Nonce��4 �ֽ� Counter����� 64 �ֽ����ݡ�
*
*           ChaCha20 �����㣺���� 32 �ֽ� Key��12 �ֽ� Nonce��4 �ֽ� Counter�����ⳤ����/���ġ�
*                    �����ͬ������/���ģ��ڲ� Counter ��ʼֵĬ��ʹ�� 1
*
* ����ƽ̨��Windows 7 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2023.07.30 V1.1
*               ���ݲݰ�ʵ�� XChaCha20������ HChaCha20 �� Key �����㷨
*           2022.07.19 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative;

const
  CN_CHACHA_STATE_SIZE   = 16;
  {* ChaCha20 �㷨��״̬������64 �ֽ�}

  CN_CHACHA_KEY_SIZE     = 32;
  {* ChaCha20 �㷨�� Key �ֽڳ���}

  CN_CHACHA_NONCE_SIZE   = 12;
  {* ChaCha20 �㷨�� Nonce �ֽڳ���}

  CN_HCHACHA_NONCE_SIZE  = 16;
  {* HChaCha20 �㷨�� Nonce �ֽڳ��ȣ�ǰ 4 �ֽ��� Counter}

  CN_XCHACHA_NONCE_SIZE   = 24;
  {* XChaCha20 �㷨�� Nonce �ֽڳ���}

  CN_HCHACHA_SUBKEY_SIZE = 32;
  {* HChaCha20 �㷨������� Key ���ֽڳ���}

  CN_CHACHA_COUNT_SIZE   = 4;
  {* ChaCha20 �㷨�ļ������ֽڳ��ȣ�ʵ������ʱʹ�� Cardinal ����}

type
  TCnChaChaKey = array[0..CN_CHACHA_KEY_SIZE - 1] of Byte;
  {* ChaCha20 �㷨�� Key}

  TCnChaChaNonce = array[0..CN_CHACHA_NONCE_SIZE - 1] of Byte;
  {* ChaCha20 �㷨�� Nonce}

  TCnHChaChaNonce = array[0..CN_HCHACHA_NONCE_SIZE - 1] of Byte;
  {* HChaCha20 �㷨�� Nonce}

  TCnHChaChaSubKey = array[0..CN_HCHACHA_SUBKEY_SIZE - 1] of Byte;
  {* HChaCha20 �㷨�� SubKey}

  TCnXChaChaNonce = array[0..CN_XCHACHA_NONCE_SIZE - 1] of Byte;
  {* XChaCha20 �㷨�� Nonce}

  TCnChaChaCounter = Cardinal;
  {* ChaCha20 �㷨�ļ�����}

  TCnChaChaState = array[0..CN_CHACHA_STATE_SIZE - 1] of Cardinal;
  {* ChaCha20 �㷨��״̬��}

procedure ChaCha20Block(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Counter: TCnChaChaCounter; var OutState: TCnChaChaState);
{* ����һ�� ChaCha20 �����㣬���� 20 �ֵ������㣬�ⲿָ�� 12 �ֽڵ� Nonce ��һ�����ֽڼ�������

   ������
     var Key: TCnChaChaKey                - ChaCha20 ����
     var Nonce: TCnChaChaNonce            - һ����������� Nonce
     Counter: TCnChaChaCounter            - ������
     var OutState: TCnChaChaState         - ״̬��

   ����ֵ�����ޣ�
}

procedure HChaCha20SubKey(var Key: TCnChaChaKey; var Nonce: TCnHChaChaNonce;
  var OutSubKey: TCnHChaChaSubKey);
{* ����һ�� HChaCha20 �����㣬���� 20 �ֵ������㣬��� SubKey��
   �ⲿָ�� 16 �ֽڵ� Nonce��ʵ������ Nonce ǰ���ֽ���Ϊ���������� 12 �ֽ�Ϊ ChaCha20 �� Nonce��

   ������
     var Key: TCnChaChaKey                - HChaCha20 ����
     var Nonce: TCnHChaChaNonce           - һ����������� Nonce
     var OutSubKey: TCnHChaChaSubKey      - ��״̬��

   ����ֵ�����ޣ�
}

function ChaCha20EncryptBytes(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Data: TBytes): TBytes;
{* ���ֽ�������� ChaCha20 ���ܣ��ڲ�ʹ�õļ�������ʼֵĬ��Ϊ 1��

   ������
     var Key: TCnChaChaKey                - ChaCha20 ����
     var Nonce: TCnChaChaNonce            - һ����������� Nonce
     Data: TBytes                         - �����ܵ������ֽ�����

   ����ֵ��TBytes                         - ���������ֽ�����
}

function ChaCha20DecryptBytes(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  EnData: TBytes): TBytes;
{* ���ֽ�������� ChaCha20 ���ܣ��ڲ�ʹ�õļ�������ʼֵĬ��Ϊ 1��

   ������
     var Key: TCnChaChaKey                - ChaCha20 ����
     var Nonce: TCnChaChaNonce            - һ����������� Nonce
     EnData: TBytes                       - �����ܵ������ֽ�����

   ����ֵ��TBytes                         - ���������ֽ�����
}

function ChaCha20EncryptData(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* �� Data ��ָ�� DataByteLength ���ȵ����ݿ���� ChaCha20 ���ܣ��ڲ�ʹ�õļ�����Ϊ 1��
   ���ķ� Output ��ָ���ڴ棬Ҫ�󳤶����������� DataByteLength��

   ������
     var Key: TCnChaChaKey                - ChaCha20 ����
     var Nonce: TCnChaChaNonce            - һ����������� Nonce
     Data: Pointer                        - �����ܵ��������ݿ��ַ
     DataByteLength: Integer              - �����ܵ��������ݿ��ֽڳ���
     Output: Pointer                      - �����������ĵ�ַ

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function ChaCha20DecryptData(var Key: TCnChaChaKey; var Nonce: TCnChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* �� Data ��ָ�� DataByteLength ���ȵ��������ݿ���� ChaCha20 ���ܣ��ڲ�ʹ�õļ�����Ϊ 1��
   ���ķ� Output ��ָ���ڴ棬Ҫ�󳤶����������� DataByteLength��

   ������
     var Key: TCnChaChaKey                - ChaCha20 ����
     var Nonce: TCnChaChaNonce            - һ����������� Nonce
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     DataByteLength: Integer              - �����ܵ��������ݿ��ֽڳ���
     Output: Pointer                      - �����������ĵ�ַ

   ����ֵ��Boolean                        - ���ؽ����Ƿ�ɹ�
}

function XChaCha20EncryptBytes(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: TBytes): TBytes;
{* ���ֽ�������� XChaCha20 ���ܣ��ڲ�ʹ�õļ�������ʼֵĬ��Ϊ 1��

   ������
     var Key: TCnChaChaKey                - XChaCha20 ����
     var Nonce: TCnXChaChaNonce           - һ����������� Nonce
     Data: TBytes                         - �����ܵ������ֽ�����

   ����ֵ��TBytes                         - ���������ֽ�����
}

function XChaCha20DecryptBytes(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: TBytes): TBytes;
{* ���ֽ�������� XChaCha20 ���ܣ��ڲ�ʹ�õļ�������ʼֵĬ��Ϊ 1��

   ������
     var Key: TCnChaChaKey                - XChaCha20 ����
     var Nonce: TCnXChaChaNonce           - һ����������� Nonce
     EnData: TBytes                       - �����ܵ������ֽ�����

   ����ֵ��TBytes                         - ���������ֽ�����
}

function XChaCha20EncryptData(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  Data: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* �� Data ��ָ�� DataByteLength ���ȵ����ݿ���� XChaCha20 ���ܣ��ڲ�ʹ�õļ�������ʼֵĬ��Ϊ 1��
   ���ķ� Output ��ָ���ڴ棬Ҫ�󳤶����������� DataByteLength��

   ������
     var Key: TCnChaChaKey                - XChaCha20 ����
     var Nonce: TCnXChaChaNonce           - һ����������� Nonce
     Data: Pointer                        - �����ܵ��������ݿ��ַ
     DataByteLength: Integer              - �����ܵ��������ݿ��ֽڳ���
     Output: Pointer                      - �����������ĵ�ַ

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function XChaCha20DecryptData(var Key: TCnChaChaKey; var Nonce: TCnXChaChaNonce;
  EnData: Pointer; DataByteLength: Integer; Output: Pointer): Boolean;
{* �� Data ��ָ�� DataByteLength ���ȵ��������ݿ���� XChaCha20 ���ܣ��ڲ�ʹ�õļ�������ʼֵĬ��Ϊ 1��
   ���ķ� Output ��ָ���ڴ棬Ҫ�󳤶����������� DataByteLength��

   ������
     var Key: TCnChaChaKey                - XChaCha20 ����
     var Nonce: TCnXChaChaNonce           - һ����������� Nonce
     EnData: Pointer                      - �����ܵ��������ݿ��ַ
     DataByteLength: Integer              - �����ܵ��������ݿ��ֽڳ���
     Output: Pointer                      - �����������ĵ�ַ

   ����ֵ��Boolean                        - ���ؽ����Ƿ�ɹ�
}

implementation

const
  CHACHA20_CONST0 = $61707865;
  CHACHA20_CONST1 = $3320646E;
  CHACHA20_CONST2 = $79622D32;
  CHACHA20_CONST3 = $6B206574;

procedure ROT(var X: Cardinal; N: BYTE); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
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
  B := DataByteLength div (SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE); // �� B ��������
  P := PByteArray(Data);
  Q := PByteArray(Output);
  M := PByteArray(@Stream[0]);

  if B > 0 then
  begin
    for I := 1 to B do
    begin
      ChaCha20Block(Key, Nonce, Cnt, Stream);

      // P��Q �Ѹ�ָ��Ҫ�����ԭʼ�������Ŀ�
      for J := 0 to SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE - 1 do
        Q^[J] := P^[J] xor M^[J];

      // ָ����һ��
      P := PByteArray(TCnNativeUInt(P) + SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE);
      Q := PByteArray(TCnNativeUInt(Q) + SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE);

      Inc(Cnt);
    end;
  end;

  L := DataByteLength mod (SizeOf(Cardinal) * CN_CHACHA_STATE_SIZE);
  if L > 0 then // ����ʣ��飬����Ϊ L
  begin
    ChaCha20Block(Key, Nonce, Cnt, Stream);

    // P��Q �Ѹ�ָ��Ҫ�����ԭʼ�������Ŀ�
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
  Move(Nonce[0], HN[0], SizeOf(TCnHChaChaNonce)); // ȡ�� XChaCha20 �� 24 �ֽ� Nonce ��ǰ 16 �ֽڼ���
  HChaCha20SubKey(Key, HN, SubKey);

  Move(SubKey[0], XKey[0], SizeOf(TCnChaChaKey)); // ����� HChaCha20 SubKey ��Ϊ ChaCha20 �� Key
  N[0] := 0;                                      // ���ֽ� 0 ���� XChaCha20 ��ʣ�� 8 �ֽڹ� 12 �ֽ���Ϊ ChaCha20 �� Nonce
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
