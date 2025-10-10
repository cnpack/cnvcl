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

unit CnSM3;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������������� SM3 �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack �����飨master@cnpack.org)
*           �ο������㷨�����ĵ���SM3 Cryptographic Hash Algorith��
*           http://www.oscca.gov.cn/UpFile/20101222141857786.pdf
*           ���ο��˲��� goldboar �� C ����
* ��    ע������Ԫʵ���˹����������� SM3 �Ӵ��㷨����Ӧ�� HMAC �㷨��
*           ʵ�ֹ��̲ο������㷨�����ĵ���SM3 Cryptographic Hash Algorith����
* ����ƽ̨��Windows 7 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.12.12 V1.2
*               ֧�� TBytes
*           2019.04.15 V1.1
*               ֧�� Win32/Win64/MacOS
*           2014.09.23 V1.0
*               ��ֲ��������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative, CnConsts {$IFDEF MSWINDOWS}, Windows {$ENDIF};

type
  PCnSM3Digest = ^TCnSM3Digest;
  {* SM3 �Ӵս��ָ��}
  TCnSM3Digest = array[0..31] of Byte;
  {* SM3 �Ӵս����32 �ֽ�}

  TCnSM3Context = packed record
  {* SM3 �������Ľṹ}
    Total: array[0..1] of Cardinal;     {!< number of bytes processed  }
    State: array[0..7] of Cardinal;     {!< intermediate digest state  }
    Buffer: array[0..63] of Byte;       {!< data block being processed }
    Ipad: array[0..63] of Byte;         {!< HMAC: inner padding        }
    Opad: array[0..63] of Byte;         {!< HMAC: outer padding        }
  end;
  PCnSM3Context = ^TCnSM3Context;

  TCnSM3CalcProgressFunc = procedure (ATotal, AProgress: Int64;
    var Cancel: Boolean) of object;
  {* SM3 �Ӵս��Ȼص��¼���������}

function SM3(Input: PAnsiChar; ByteLength: Cardinal): TCnSM3Digest;
{* �����ݿ���� SM3 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ

 }

function SM3Buffer(const Buffer; Count: Cardinal): TCnSM3Digest;
{* �����ݿ���� SM3 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

function SM3Bytes(Data: TBytes): TCnSM3Digest;
{* ���ֽ�������� SM3 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

function SM3String(const Str: string): TCnSM3Digest;
{* �� String �������ݽ��� SM3 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

function SM3StringA(const Str: AnsiString): TCnSM3Digest;
{* �� AnsiString �������ݽ��� SM3 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

function SM3StringW(const Str: WideString): TCnSM3Digest;
{* �� WideString �����ַ�������ת�������� SM3 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

{$IFDEF UNICODE}

function SM3UnicodeString(const Str: string): TCnSM3Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SM3 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

{$ELSE}

function SM3UnicodeString(const Str: WideString): TCnSM3Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SM3 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

{$ENDIF}

function SM3File(const FileName: string; CallBack: TCnSM3CalcProgressFunc = nil): TCnSM3Digest;
{* ��ָ���ļ����ݽ��� SM3 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSM3CalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

function SM3Stream(Stream: TStream; CallBack: TCnSM3CalcProgressFunc = nil): TCnSM3Digest;
{* ��ָ�������ݽ��� SM3 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSM3CalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSM3Digest                   - ���ص� SM3 �Ӵ�ֵ
}

// �����������������ⲿ���������ݽ�����ɢ�� SM3 ���㣬SM3Update �ɶ�α�����

procedure SM3Init(var Context: TCnSM3Context);
{* ��ʼ��һ�� SM3 ���������ģ�׼������ SM3 ���

   ������
     var Context: TCnSM3Context           - ����ʼ���� SM3 ������

   ����ֵ�����ޣ�
}

procedure SM3Update(var Context: TCnSM3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SM3 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSM3Context           - SM3 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SM3Final(var Context: TCnSM3Context; var Digest: TCnSM3Digest);
{* �������ּ��㣬�� SM3 ��������� Digest ��

   ������
     var Context: TCnSM3Context           - SM3 ������
     var Digest: TCnSM3Digest             - ���ص� SM3 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

function SM3Print(const Digest: TCnSM3Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SM3 �Ӵ�ֵ��

   ������
     const Digest: TCnSM3Digest           - ָ���� SM3 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SM3Match(const D1: TCnSM3Digest; const D2: TCnSM3Digest): Boolean;
{* �Ƚ����� SM3 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSM3Digest               - ���Ƚϵ� SM3 �Ӵ�ֵһ
     const D2: TCnSM3Digest               - ���Ƚϵ� SM3 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SM3DigestToStr(const Digest: TCnSM3Digest): string;
{* SM3 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSM3Digest           - ��ת���� SM3 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

procedure SM3Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSM3Digest);
{* ���� SM3 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SM3 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SM3 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSM3Digest             - ���ص� SM3 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

implementation

const
  SM3Padding: array[0..63] of Byte =
    (
      $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    );

  SM3_T: array[0..63] of Cardinal = (
    $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519,
    $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519, $79CC4519,
    $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A,
    $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A,
    $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A,
    $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A,
    $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A,
    $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A, $7A879D8A
  );

  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  HMAC_SM3_BLOCK_SIZE_BYTE = 64;
  HMAC_SM3_OUTPUT_LENGTH_BYTE = 32;

type
  TSM3ProcessData = array[0..63] of Byte;

procedure GetULongBe(var N: Cardinal; B: PAnsiChar; I: Integer); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
var
  D: Cardinal;
begin
  D := (Cardinal(B[I]) shl 24) or (Cardinal(B[I + 1]) shl 16) or
    (Cardinal(B[I + 2]) shl 8) or (Cardinal(B[I + 3]));
  N := D;
end;

procedure PutULongBe(N: Cardinal; B: PAnsiChar; I: Integer); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  B[I] := AnsiChar(N shr 24);
  B[I + 1] := AnsiChar(N shr 16);
  B[I + 2] := AnsiChar(N shr 8);
  B[I + 3] := AnsiChar(N);
end;

function FF0(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := X xor Y xor Z;
end;

function FF1(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and Y) or (Y and Z) or (X and Z);
end;

function GG0(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := X xor Y xor Z;
end;

function GG1(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and Y) or ((not X) and Z);
end;

function SM3Shl(X: Cardinal; N: Integer): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and $FFFFFFFF) shl N;
end;

// ѭ�����ơ�ע�� N Ϊ 0 �� 32 ʱ����ֵ��Ϊ X��N Ϊ 33 ʱ����ֵ���� N Ϊ 1 ʱ�ķ���ֵ
function ROTL(X: Cardinal; N: Integer): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := SM3Shl(X, N) or (X shr (32 - N));
end;

function P0(X: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := X xor ROTL(X, 9) xor ROTL(X, 17);
end;

function P1(X: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := X xor ROTL(X, 15) xor ROTL(X, 23);
end;

procedure SM3Init(var Context: TCnSM3Context);
begin
  Context.Total[0] := 0;
  Context.Total[1] := 0;

  Context.State[0] := $7380166F;
  Context.State[1] := $4914B2B9;
  Context.State[2] := $172442D7;
  Context.State[3] := $DA8A0600;
  Context.State[4] := $A96F30BC;
  Context.State[5] := $163138AA;
  Context.State[6] := $E38DEE4D;
  Context.State[7] := $B0FB0E4E;

  FillChar(Context.Buffer, SizeOf(Context.Buffer), 0);
end;

// һ�δ��� 64 �ֽ�Ҳ���� 512 λ���ݿ�
procedure SM3Process(var Context: TCnSM3Context; Data: PAnsiChar);
var
  SS1, SS2, TT1, TT2: Cardinal;
  W: array[0..67] of Cardinal;
  W1: array[0..63] of Cardinal;
  A, B, C, D, E, F, G, H: Cardinal;
  Temp1, Temp2: Cardinal;
  J: Integer;
begin
  GetULongBe(W[ 0], Data,  0);
  GetULongBe(W[ 1], Data,  4);
  GetULongBe(W[ 2], Data,  8);
  GetULongBe(W[ 3], Data, 12);
  GetULongBe(W[ 4], Data, 16);
  GetULongBe(W[ 5], Data, 20);
  GetULongBe(W[ 6], Data, 24);
  GetULongBe(W[ 7], Data, 28);
  GetULongBe(W[ 8], Data, 32);
  GetULongBe(W[ 9], Data, 36);
  GetULongBe(W[10], Data, 40);
  GetULongBe(W[11], Data, 44);
  GetULongBe(W[12], Data, 48);
  GetULongBe(W[13], Data, 52);
  GetULongBe(W[14], Data, 56);
  GetULongBe(W[15], Data, 60);

  for J := 16 to 67 do
  begin
    Temp1 := W[J - 16] xor W[J - 9];
    Temp2 := ROTL(W[J - 3], 15);
    W[J] := P1(Temp1 xor Temp2) xor (ROTL(W[J - 13], 7) xor W[J - 6]);
  end;

  for J := 0 to 63 do
    W1[J] := W[J] xor W[J + 4];

  // �Ѿ������������W/W1��ֵ��

  A := Context.State[0];
  B := Context.State[1];
  C := Context.State[2];
  D := Context.State[3];
  E := Context.State[4];
  F := Context.State[5];
  G := Context.State[6];
  H := Context.State[7];

  for J := 0 to 15 do
  begin
    SS1 := ROTL((ROTL(A, 12) + E + ROTL(SM3_T[J], J)), 7);
    SS2 := SS1 xor ROTL(A, 12);
    TT1 := FF0(A, B, C) + D + SS2 + W1[J];
    TT2 := GG0(E, F, G) + H + SS1 + W[J];
    D := C;
    C := ROTL(B, 9);
    B := A;
    A := TT1;
    H := G;
    G := ROTL(F, 19);
    F := E;
    E := P0(TT2);
  end;

  for J := 16 to 63 do
  begin
    SS1 := ROTL((ROTL(A, 12) + E + ROTL(SM3_T[J], J)), 7);
    SS2 := SS1 xor ROTL(A, 12);
    TT1 := FF1(A, B, C) + D + SS2 + W1[J];
    TT2 := GG1(E, F, G) + H + SS1 + W[J];
    D := C;
    C := ROTL(B,9);
    B := A;
    A := TT1;
    H := G;
    G := ROTL(F,19);
    F := E;
    E := P0(TT2);
  end;

  Context.State[0] := Context.State[0] xor A;
  Context.State[1] := Context.State[1] xor B;
  Context.State[2] := Context.State[2] xor C;
  Context.State[3] := Context.State[3] xor D;
  Context.State[4] := Context.State[4] xor E;
  Context.State[5] := Context.State[5] xor F;
  Context.State[6] := Context.State[6] xor G;
  Context.State[7] := Context.State[7] xor H;

  // ��������
end;

procedure SM3UpdateW(var Context: TCnSM3Context; Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  pContent: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(pContent, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(pContent), CharLength * SizeOf(WideChar), nil, nil);
    SM3Update(Context, pContent, iLen);
  finally
    FreeMem(pContent);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  SM3Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure SM3Update(var Context: TCnSM3Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  Fill, Left: Cardinal;
begin
  if (Input = nil) or (ByteLength <= 0) then
    Exit;

  Left := Context.Total[0] and $3F;
  Fill := 64 - Left;

  Context.Total[0] := Context.Total[0] + ByteLength;
  Context.Total[0] := Context.Total[0] and $FFFFFFFF;

  if Context.Total[0] < ByteLength then
    Context.Total[1] := Context.Total[1] + 1;

  if (Left <> 0) and (ByteLength >= Fill) then
  begin
    Move(Input^, Context.Buffer[Left], Fill);
    SM3Process(Context, @(Context.Buffer[0]));
    Input := Input + Fill;
    ByteLength := ByteLength - Fill;
    Left := 0;
  end;

  while ByteLength >= 64 do
  begin
    SM3Process(Context, Input);
    Input := Input + 64;
    ByteLength := ByteLength - 64;
  end;

  if ByteLength > 0 then
    Move(Input^, Context.Buffer[Left], ByteLength);
end;

procedure SM3Final(var Context: TCnSM3Context; var Digest: TCnSM3Digest);
var
  Last, Padn: Cardinal;
  High, Low: Cardinal;
  MsgLen: array[0..7] of Byte;
begin
  High := (Context.Total[0] shr 29) or (Context.Total[1] shl 3);
  Low := Context.Total[0] shl 3;

  PutULongBe(High, @(MsgLen[0]), 0);
  PutULongBe(Low, @(MsgLen[0]), 4);

  Last := Context.Total[0] and $3F;
  if Last < 56 then
    Padn := 56 - Last
  else
    Padn := 120 - Last;

  SM3Update(Context, @(SM3Padding[0]), Padn);
  SM3Update(Context, @(MsgLen[0]), 8);

  PutULongBe(Context.State[0], @Digest,  0);
  PutULongBe(Context.State[1], @Digest,  4);
  PutULongBe(Context.State[2], @Digest,  8);
  PutULongBe(Context.State[3], @Digest, 12);
  PutULongBe(Context.State[4], @Digest, 16);
  PutULongBe(Context.State[5], @Digest, 20);
  PutULongBe(Context.State[6], @Digest, 24);
  PutULongBe(Context.State[7], @Digest, 28);
end;

function SM3(Input: PAnsiChar; ByteLength: Cardinal): TCnSM3Digest;
var
  Ctx: TCnSM3Context;
begin
  SM3Init(Ctx);
  SM3Update(Ctx, Input, ByteLength);
  SM3Final(Ctx, Result);
end;

procedure SM3HmacStarts(var Ctx: TCnSM3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSM3Digest;
begin
  if KeyLength > HMAC_SM3_BLOCK_SIZE_BYTE then
  begin
    Sum := SM3(Key, KeyLength);
    KeyLength := HMAC_SM3_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Ctx.Ipad, HMAC_SM3_BLOCK_SIZE_BYTE, $36);
  FillChar(Ctx.Opad, HMAC_SM3_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Ctx.Ipad[I] := Byte(Ctx.Ipad[I] xor Byte(Key[I]));
    Ctx.Opad[I] := Byte(Ctx.Opad[I] xor Byte(Key[I]));
  end;

  SM3Init(Ctx);
  SM3Update(Ctx, @(Ctx.Ipad[0]), HMAC_SM3_BLOCK_SIZE_BYTE);
end;

procedure SM3HmacUpdate(var Ctx: TCnSM3Context; Input: PAnsiChar; Length: Cardinal);
begin
  SM3Update(Ctx, Input, Length);
end;

procedure SM3HmacFinish(var Ctx: TCnSM3Context; var Output: TCnSM3Digest);
var
  Len: Integer;
  TmpBuf: TCnSM3Digest;
begin
  Len := HMAC_SM3_OUTPUT_LENGTH_BYTE;
  SM3Final(Ctx, TmpBuf);
  SM3Init(Ctx);
  SM3Update(Ctx, @(Ctx.Opad[0]), HMAC_SM3_BLOCK_SIZE_BYTE);
  SM3Update(Ctx, @(TmpBuf[0]), Len);
  SM3Final(Ctx, Output);
end;

procedure SM3Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSM3Digest);
var
  Ctx: TCnSM3Context;
begin
  SM3HmacStarts(Ctx, Key, KeyByteLength);
  SM3HmacUpdate(Ctx, Input, ByteLength);
  SM3HmacFinish(Ctx, Output);
end;

function SM3Buffer(const Buffer; Count: Cardinal): TCnSM3Digest;
var
  Context: TCnSM3Context;
begin
  SM3Init(Context);
  SM3Update(Context, PAnsiChar(Buffer), Count);
  SM3Final(Context, Result);
end;

function SM3Bytes(Data: TBytes): TCnSM3Digest;
var
  Context: TCnSM3Context;
begin
  SM3Init(Context);
  SM3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SM3Final(Context, Result);
end;

function SM3String(const Str: string): TCnSM3Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SM3StringA(AStr);
end;

function SM3StringA(const Str: AnsiString): TCnSM3Digest;
var
  Context: TCnSM3Context;
begin
  SM3Init(Context);
  SM3Update(Context, PAnsiChar(Str), Length(Str));
  SM3Final(Context, Result);
end;

function SM3StringW(const Str: WideString): TCnSM3Digest;
var
  Context: TCnSM3Context;
begin
  SM3Init(Context);
  SM3UpdateW(Context, PWideChar(Str), Length(Str));
  SM3Final(Context, Result);
end;

{$IFDEF UNICODE}
function SM3UnicodeString(const Str: string): TCnSM3Digest;
{$ELSE}
function SM3UnicodeString(const Str: WideString): TCnSM3Digest;
{$ENDIF}
var
  Context: TCnSM3Context;
begin
  SM3Init(Context);
  SM3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SM3Final(Context, Result);
end;

function InternalSM3Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TCnSM3Digest; CallBack: TCnSM3CalcProgressFunc): Boolean;
var
  Context: TCnSM3Context;
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
begin
  Result := False;
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then Exit;
  if Size < BufSize then BufLen := Size
  else BufLen := BufSize;

  CancelCalc := False;
  SM3Init(Context);
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        SM3Update(Context, Buf, ReadBytes);
        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    SM3Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

function SM3File(const FileName: string;
  CallBack: TCnSM3CalcProgressFunc): TCnSM3Digest;
var
{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TCnSM3Context;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  function FileSizeIsLargeThanMaxOrCanNotMap(const AFileName: string; out IsEmpty: Boolean): Boolean;
{$IFDEF MSWINDOWS}
  var
    H: THandle;
    Info: BY_HANDLE_FILE_INFORMATION;
    Rec : Int64Rec;
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    Result := False;
    IsEmpty := False;
    H := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if H = INVALID_HANDLE_VALUE then Exit;
    try
      if not GetFileInformationByHandle(H, Info) then Exit;
    finally
      CloseHandle(H);
    end;
    Rec.Lo := Info.nFileSizeLow;
    Rec.Hi := Info.nFileSizeHigh;
    Result := (Rec.Hi > 0) or (Rec.Lo > MAX_FILE_SIZE);
    IsEmpty := (Rec.Hi = 0) and (Rec.Lo = 0);
{$ELSE}
    Result := True; // �� Windows ƽ̨���� True����ʾ�� Mapping
{$ENDIF}
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���� Windows ƽ̨������������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSM3Stream(Stream, 4096 * 1024, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    SM3Init(Context);
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
                  FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
                  FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
        if MapHandle <> 0 then
        begin
          try
            ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
            if ViewPointer <> nil then
            begin
              try
                SM3Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
              finally
                UnmapViewOfFile(ViewPointer);
              end;
            end
            else
            begin
              raise Exception.Create(SCnErrorMapViewOfFile + IntToStr(GetLastError));
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end
        else
        begin
          if not FileIsZeroSize then
            raise Exception.Create(SCnErrorCreateFileMapping + IntToStr(GetLastError));
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
    SM3Final(Context, Result);
{$ENDIF}
  end;
end;

function SM3Stream(Stream: TStream;
  CallBack: TCnSM3CalcProgressFunc): TCnSM3Digest;
begin
  InternalSM3Stream(Stream, 4096 * 1024, Result, CallBack);
end;

function SM3Print(const Digest: TCnSM3Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSM3Digest));
end;

function SM3Match(const D1, D2: TCnSM3Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnSM3Digest));
end;

function SM3DigestToStr(const Digest: TCnSM3Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSM3Digest));
end;

end.
