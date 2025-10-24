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

unit CnMath;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ���ѧ������㷨��Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע������Ԫʵ����һЩ��ѧ������Ŀ���������� Math �⣬����Ч�ʽϹٷ�ʵ�ֿ����Ե͡�
* ����ƽ̨��Win 7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2021.12.08 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

const
  CN_PI = 3.1415926535897932384626;
  {* Բ���ʵĸ���ֵ}

  CN_FLOAT_DEFAULT_DIGIT = 10;
  {* Ĭ�ϵĸ�������λ��}

function CnAbs(F: Extended): Extended; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ���㸡�����ľ���ֵ��

   ������
     F: Extended                          - ������ĸ�����

   ����ֵ��Extended                       - ���صľ���ֵ
}

function CnFloor(F: Extended): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* �����������Ḻ����ȡ����

   ������
     F: Extended                          - ��ȡ���ĸ�����

   ����ֵ��Integer                        - ���ص�ȡ��ֵ
}

function CnCeil(F: Extended): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ������������������ȡ����

   ������
     F: Extended                          - ��ȡ���ĸ�����

   ����ֵ��Integer                        - ���ص�ȡ��ֵ
}

{
  ������������
                  A1
  B0 +  ----------------------
                     A2
        B1 + -----------------
                       A3
             B2 + ------------
                            An
                  B4 + ... ---
                            Bn
}
// function Int64ContinuedFraction

function Int64Sqrt(N: Int64): Extended;
{* ���� Int64 ��ƽ������ʹ��ţ�ٵ��� Xn+1 = (Xn + N/Xn)/2

   ������
     N: Int64                             - ������ƽ����������

   ����ֵ��Extended                       - ����ƽ����
}

function FloatSqrt(F: Extended): Extended;
{* ������չ���ȸ�������ƽ������ʹ��ţ�ٵ��� Xn+1 = (Xn + N/Xn)/2

   ������
     F: Extended                          - ������ƽ�����ĸ�����

   ����ֵ��Extended                       - ����ƽ����
}

function Int64LogN(N: Int64): Extended;
{* ���� Int64 ����Ȼ������ʹ�÷�˫������չ����

   ������
     N: Int64                             - ��������Ȼ����������

   ����ֵ��Extended                       - ������Ȼ����
}

function FloatLogN(F: Extended): Extended;
{* ������չ���ȸ���������Ȼ������ʹ�÷�˫������չ����

   ������
     F: Extended                          - ��������Ȼ�����ĸ�����

   ����ֵ��Extended                       - ������Ȼ����
}

function Int64Log10(N: Int64): Extended;
{* ���� Int64 �ĳ��ö�����ֱ��ʹ����Ȼ�������㡣

   ������
     N: Int64                             - �����㳣�ö���������

   ����ֵ��Extended                       - ���س��ö���
}

function FloatLog10(F: Extended): Extended;
{* ������չ���ȸ������ĳ��ö�����ֱ��ʹ����Ȼ�������㡣

   ������
     F: Extended                          - �����㳣�ö����ĸ�����

   ����ֵ��Extended                       - ���س��ö���
}

function Int64Log2(N: Int64): Extended;
{* ���� Int64 �� 2 Ϊ�׵Ķ�����ֱ��ʹ����Ȼ�������㡣

   ������
     N: Int64                             - ������� 2 Ϊ�׵Ķ���������

   ����ֵ��Extended                       - ���� 2 Ϊ�׵Ķ���
}

function FloatLog2(F: Extended): Extended;
{* ������չ���ȸ������� 2 Ϊ�׵Ķ�����ֱ��ʹ����Ȼ�������㡣

   ������
     F: Extended                          - ������� 2 Ϊ�׵Ķ����ĸ�����

   ����ֵ��Extended                       - ���� 2 Ϊ�׵Ķ���
}

function FloatGaussLegendrePi(RoundCount: Integer = 3): string;
{* ��չ���ȷ�Χ���ø�˹���õ¹�ʽ���� Pi��3 �ֱ��ѵִ���չ���ȼ��ޡ�

   ������
     RoundCount: Integer                  - ��������

   ����ֵ��string                         - ���ص� Pi ֵ�ַ���
}

function GaussLegendrePi(RoundCount: Integer = 8): string;
{* �󸡵����ø�˹���õ¹�ʽ���� Pi��8 �ε������Ⱦ͵��� 100 ��λ��12 �ֺ�ʱ 5 �롣

   ������
     RoundCount: Integer                  - ��������

   ����ֵ��string                         - ���ص� Pi ֵ�ַ���
}

function XavierGourdonEuler(BlockSize: Integer = 1000): string;
{* �� Xavier Gourdon ������ŷ������ e ��ֵ������Ϊ����������

   ������
     BlockSize: Integer                   - ��������

   ����ֵ��string                         - ���ص� e ֵ�ַ���
}

function FloatAlmostZero(F: Extended): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* �ж�һ�������Ƿ��� 0 �㹻����

   ������
     F: Extended                          - ���жϵĸ�����

   ����ֵ��Boolean                        - �Ƿ��� 0 �㹻��
}

function FloatEqual(A: Extended; B: Extended): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ��װ�������������Ƿ���ȵ��жϡ�

   ������
     A: Extended                          - ���жϵĸ�����һ
     B: Extended                          - ���жϵĸ�������

   ����ֵ��Boolean                        - �����Ƿ�������
}

function NormalizeAngle(Angle: Extended): Extended;
{* ���Ƕȱ��� [0, 2��) ��Χ�ڣ�Ҳ����һ����

   ������
     Angle: Extended                      - ����һ���ĽǶ�ֵ

   ����ֵ��Extended                       - ���ع�һ����ĽǶ�
}

function FloatToHex(Value: Extended; MaxDigit: Integer = CN_FLOAT_DEFAULT_DIGIT): string;
{* ������ת��Ϊʮ�������ַ�������������������С�����֣�MaxDigit ָ��������ʱ��ౣ��С��������λ��

   ������
     Value: Extended                      - ��ת���ĸ�����
     MaxDigit: Integer                    - ָ��������ʱ��ౣ��С��������λ

   ����ֵ��string                         - ���ر�ʾ�ø�������ʮ�������ַ���
}

function HexToFloat(const Hex: string): Extended;
{* ʮ�������ַ���ת���ɸ�������֧�ִ�С�����С����

   ������
     const Hex: string                    - ��ת����ʮ�������ַ�����֧�ִ�С����

   ����ֵ��Extended                       - ���صĸ�����
}

function CnIntAbs(N: Integer): Integer;
{* ���������ľ���ֵ��

   ������
     N: Integer                           - ���������ֵ������

   ����ֵ��Integer                        - ���ؾ���ֵ
}

function CnInt64Abs(N: Int64): Int64;
{* ���� Int64 �ľ���ֵ��

   ������
     N: Int64                             - ���������ֵ������

   ����ֵ��Integer                        - ���ؾ���ֵ
}

function FastInverseSqrt(X: Single): Single;
{* ���ټ��㿪���ŵĵ�����

   ������
     X: Single                            - ������ĵ����ȸ�����

   ����ֵ��Single                         - ���ؼ�����
}

function FastSqrt(N: Cardinal): Cardinal;
{* ��λȷ�������ټ���������ƽ�������������֡�

   ������
     N: Cardinal                          - �����������

   ����ֵ��Cardinal                       - ����ƽ��������������
}

function FastSqrt64(N: Int64): Int64;
{* ��λȷ�������ټ���������ƽ�������������֡�

   ������
     N: Int64                             - �����������

   ����ֵ��Int64                          - ����ƽ��������������
}

implementation

uses
  CnBigDecimal, CnNative;

const
  SCN_FLOAT_GAP = 0.000001;         // ��ͨ�����ж�
  SCN_EXTEND_GAP = 0.00000000001;   // ����Ԫ�еĵ��������ֵ
  SCN_LOGN_TO_LOG2 = 1.4426950408889634073599246810019;
  SCN_LOGN_TO_LOG10 = 0.43429448190325182765112891891661;

resourcestring
  SCnErrorMathSqrtRange = 'Sqrt Range Error.';
  SCnErrorMathLogRange = 'Log Range Error.';

function CnAbs(F: Extended): Extended;
begin
  if F < 0 then
    Result := -F
  else
    Result := F;
end;

function CnFloor(F: Extended): Integer;
begin
  Result := Trunc(F);
  if Frac(F) < 0 then
    Dec(Result);
end;

function CnCeil(F: Extended): Integer;
begin
  Result := Trunc(F);
  if Frac(F) > 0 then
    Inc(Result);
end;

{$HINTS OFF}

function Int64Sqrt(N: Int64): Extended;
var
  X0: Extended;
begin
  if N < 0 then
    raise ERangeError.Create(SCnErrorMathSqrtRange);

  Result := 0;
  if (N = 0) or (N = 1) then
  begin
    Result := N;
    Exit;
  end;

  X0 := N;
  while True do
  begin
    Result := (X0 + N/X0) / 2;

    if CnAbs(Result - X0) < SCN_EXTEND_GAP then
      Break;
    X0 := Result;
  end;
end;

function FloatSqrt(F: Extended): Extended;
var
  X0: Extended;
begin
  if F < 0 then
    raise ERangeError.Create(SCnErrorMathSqrtRange);

  Result := 0;
  if (F = 0) or (F = 1) then
  begin
    Result := F;
    Exit;
  end;

  X0 := F;
  while True do
  begin
    Result := (X0 + F/X0) / 2;

    if CnAbs(Result - X0) < SCN_EXTEND_GAP then
      Break;
    X0 := Result;
  end;
end;

{$HINTS ON}

function Int64LogN(N: Int64): Extended;
var
  I: Integer;
  F: Extended;
  Z, D: Extended;
begin
  if N <= 0 then
    raise ERangeError.Create(SCnErrorMathLogRange);

  Result := 0;
  if N = 1 then
    Exit;

  //           [ z-1   1 (z-1)^3   1 (z-1)^5        ]
  // lnz = 2 * | --- + - ------- + - ------- + .... |
  //           [ z+1   3 (z+1)^3   5 (z+1)^5        ]

  F := N;
  Z := (F - 1) / (F + 1);
  D := Z;
  Z := Z * Z;
  I := 1;

  while True do
  begin
    Result := Result + D / I;
    Inc(I, 2);
    D := D * Z;

    if CnAbs(D) < SCN_EXTEND_GAP then
      Break;
  end;
  Result := Result * 2;
end;

function FloatLogN(F: Extended): Extended;
var
  I: Integer;
  Z, D: Extended;
begin
  if F <= 0 then
    raise ERangeError.Create(SCnErrorMathLogRange);

  Result := 0;
  if F = 1 then
    Exit;

  //           [ z-1   1 (z-1)^3   1 (z-1)^5        ]
  // lnz = 2 * | --- + - ------- + - ------- + .... |
  //           [ z+1   3 (z+1)^3   5 (z+1)^5        ]

  Z := (F - 1) / (F + 1);
  D := Z;
  Z := Z * Z;
  I := 1;

  while True do
  begin
    Result := Result + D / I;
    Inc(I, 2);
    D := D * Z;

    if CnAbs(D) < SCN_EXTEND_GAP then
      Break;
  end;
  Result := Result * 2;
end;

function Int64Log10(N: Int64): Extended;
begin
  Result := Int64LogN(N) * SCN_LOGN_TO_LOG10;
end;

function FloatLog10(F: Extended): Extended;
begin
  Result := FloatLogN(F) * SCN_LOGN_TO_LOG10;
end;

function Int64Log2(N: Int64): Extended;
begin
  Result := Int64LogN(N) * SCN_LOGN_TO_LOG2;
end;

function FloatLog2(F: Extended): Extended;
begin
  Result := FloatLogN(F) * SCN_LOGN_TO_LOG2;
end;

function FloatGaussLegendrePi(RoundCount: Integer): string;
var
  I: Integer;
  A0, B0, T0, P0: Extended;
  A1, B1, T1, P1: Extended;
  Res: Extended;
begin
  A0 := 1;
  B0 := Sqrt(2) / 2;
  T0 := 0.25;
  P0 := 1;
  Res := 0;

  for I := 1 to RoundCount do
  begin
    A1 := (A0 + B0) / 2;
    B1 := Sqrt(A0 * B0);
    T1 := T0 - P0 * (A0 - A1) * (A0 - A1);
    P1 := P0 * 2;

    Res := (A1 + B1) * (A1 + B1) / (T1 * 4);

    A0 := A1;
    B0 := B1;
    T0 := T1;
    P0 := P1;
  end;

  Result := FloatToStr(Res);
end;

function GaussLegendrePi(RoundCount: Integer): string;
var
  I, P: Integer;
  A0, B0, T0, P0: TCnBigDecimal;
  A1, B1, T1, P1: TCnBigDecimal;
  X1, X2: TCnBigDecimal;
  Res: TCnBigDecimal;
begin
  A0 := nil;
  B0 := nil;
  T0 := nil;
  P0 := nil;

  A1 := nil;
  B1 := nil;
  T1 := nil;
  P1 := nil;

  Res := nil;
  X1 := nil;
  X2 := nil;

  try
    A0 := TCnBigDecimal.Create;
    B0 := TCnBigDecimal.Create;
    T0 := TCnBigDecimal.Create;
    P0 := TCnBigDecimal.Create;

    A1 := TCnBigDecimal.Create;
    B1 := TCnBigDecimal.Create;
    T1 := TCnBigDecimal.Create;
    P1 := TCnBigDecimal.Create;

    Res := TCnBigDecimal.Create;

    // ��ʱ����
    X1 := TCnBigDecimal.Create;
    X1.SetWord(2);
    X2 := TCnBigDecimal.Create;

    P := 1 shl RoundCount;  // ���� Round ������ǰȷ������
    if P < 16 then
      P := 16;

    A0.SetOne;
    B0.SetWord(2);
    BigDecimalSqrt(B0, B0, P);
    BigDecimalDiv(B0, B0, X1, P);
    T0.SetExtended(0.25);
    P0.SetOne;

    Res.SetZero;
    for I := 1 to RoundCount do
    begin
      // A1 := (A0 + B0) / 2;
      BigDecimalAdd(A1, A0, B0);
      BigDecimalDiv(A1, A1, X1, P);

      // B1 := Sqrt(A0 * B0);
      BigDecimalMul(B1, A0, B0);
      BigDecimalSqrt(B1, B1, P);

      // T1 := T0 - P0 * (A0 - A1) * (A0 - A1);
      BigDecimalSub(T1, A0, A1);
      BigDecimalMul(T1, T1, T1);
      BigDecimalMul(T1, T1, P0);
      BigDecimalSub(T1, T0, T1);

      // P1 := P0 * 2;
      BigDecimalAdd(P1, P0, P0);

      // Res := (A1 + B1) * (A1 + B1) / (T1 * 4);
      BigDecimalAdd(Res, A1, B1);
      BigDecimalMul(Res, Res, Res);
      BigDecimalAdd(X2, T1, T1);
      BigDecimalAdd(X2, X2, X2);

      BigDecimalDiv(Res, Res, X2, P);

      // ׼����һ�ֵ���
      BigDecimalCopy(A0, A1);
      BigDecimalCopy(B0, B1);
      BigDecimalCopy(T0, T1);
      BigDecimalCopy(P0, P1);
    end;

    Result := Res.ToString;
  finally
    X1.Free;
    X2.Free;

    Res.Free;

    A1.Free;
    B1.Free;
    T1.Free;
    P1.Free;

    A0.Free;
    B0.Free;
    T0.Free;
    P0.Free;
  end;
end;

function XavierGourdonEuler(BlockSize: Integer = 1000): string;
var
  N, M, X: Integer;
  A: array of Integer;
begin
  if BlockSize <= 0 then
    Exit;

  SetLength(A, BlockSize);
  N := BlockSize;
  M := BlockSize;
  Dec(N);
  A[0] := 0;
  while N <> 0 do
  begin
    A[N] := 1;
    Dec(N);
  end;
  A[1] := 2;
  X := 65536; // X ��Ȼ����Ǽ�����û��ʼ��ò�ƶ��У�

  while M > 9 do
  begin
    N := M;
    Dec(M);
    Dec(N);
    while N <> 0 do
    begin
      A[N] := X mod N;
      X := 10 * A[N - 1] + X div N;
      Dec(N);
    end;

    Result := Result + IntToStr(X);
  end;

  if Length(Result) > 2 then
    Insert('.', Result, 2);
end;

function FloatAlmostZero(F: Extended): Boolean;
{$IFDEF SUPPORT_INLINE}
const
  SCN_FLOAT_GAP = 0.000001; // inline ����ʹ����ߵĳ���
{$ENDIF}
begin
  Result := CnAbs(F) < SCN_FLOAT_GAP;
end;

function FloatEqual(A: Extended; B: Extended): Boolean;
begin
  Result := FloatAlmostZero(A - B);
end;

function NormalizeAngle(Angle: Extended): Extended;
begin
  Result := Angle;
  Result := Result - 2 * CN_PI * CnFloor(Result / (2 * CN_PI));
  if Result < 0 then
    Result := Result + 2 * CN_PI;
end;

function FloatToHex(Value: Extended; MaxDigit: Integer): string;
var
  A, B: Extended;
  S: string;
  Neg: Boolean;
  R, C: Integer;
begin
  A := Int(Value);
  B := Frac(Value);

  Neg := A < 0;
  if Neg then
  begin
    A := -A;
    B := -B;
  end;

  Result := '';
  while not FloatAlmostZero(A) do
  begin
    // �� A ���� 16 ������
    R := Trunc(A - Int(A / 16.0) * 16);

    // ������ת��Ϊʮ�������ַ�����ӵ��ַ���
    Result := IntToHex(R, 1) + Result;

    // �������ֳ��� 16
    A := Int(A / 16);
  end;

  C := 0;
  S := '.';
  while (CnAbs(B) >= SCN_EXTEND_GAP) and (C <= MaxDigit) do
  begin
    B := B * 16;               // ���� 16 ȡ��������
    R := Trunc(B);
    S := S + IntToHex(R, 1);

    B := B - R;
    Inc(C);
  end;

  if Result = '' then
    Result := '0'
  else if Neg then
    Result := '-' + Result;

  if S <> '.' then
    Result := Result + S;
end;

function HexToFloat(const Hex: string): Extended;
var
  I: Integer;
  S: string;
  Neg: Boolean;

  function HexIntegerToFloat(Hex: PChar; CharLen: Integer): Extended;
  var
    I: Integer;
    C: Char;
  begin
    Result := 0;
    for I := 0 to CharLen - 1 do
    begin
      C := Hex[I];
      if (C >= '0') and (C <= '9') then
        Result := Result * 16 + Ord(C) - Ord('0')
      else if (C >= 'A') and (C <= 'F') then
        Result := Result * 16 + Ord(C) - Ord('A') + 10
      else if (C >= 'a') and (C <= 'f') then
        Result := Result * 16 + Ord(C) - Ord('a') + 10
      else
        raise Exception.CreateFmt('Error: not a Hex PChar: %c', [C]);
    end;
  end;

  function HexDecimalToFloat(Hex: PChar; CharLen: Integer): Extended;
  var
    I: Integer;
    C: Char;
    R: Extended;
  begin
    Result := 0;
    R := 1;
    for I := 0 to CharLen - 1 do
    begin
      C := Hex[I];
      R := R / 16;
      if (C >= '0') and (C <= '9') then
        Result := Result + (Ord(C) - Ord('0')) * R
      else if (C >= 'A') and (C <= 'F') then
        Result := Result + (Ord(C) - Ord('A') + 10) * R
      else if (C >= 'a') and (C <= 'f') then
        Result := Result + (Ord(C) - Ord('a') + 10) * R
      else
        raise Exception.CreateFmt('Error: not a Hex PChar: %c', [C]);
    end;
  end;

begin
  I := Pos('.', Hex);
  if I > 0 then
    S := Copy(Hex, 1, I - 1)
  else
    S := Hex;

  Neg := False;
  if (Length(S) > 0) and (S[1] = '-') then
  begin
    Delete(S, 1, 1);
    Neg := True;
  end;

  // ��������ת����ֵ
  Result := HexIntegerToFloat(PChar(S), Length(S));

  if I > 0 then
  begin
    S := Copy(Hex, I + 1, MaxInt);

    // ��С������ת����ֵ
    Result := Result + HexDecimalToFloat(PChar(S), Length(S));
  end;

  if Neg then
    Result := -Result;
end;

function CnIntAbs(N: Integer): Integer;
begin
  if N < 0 then
    Result := -N
  else
    Result := N;
end;

function CnInt64Abs(N: Int64): Int64;
begin
  if N < 0 then
    Result := -N
  else
    Result := N;
end;

// ���ټ��㿪���ŵĵ���
function FastInverseSqrt(X: Single): Single;
type
  PCnInteger = ^Integer;
  PCnSingle = ^Single;
var
  xHalf: Single;
  I: Integer;
begin
  xHalf := 0.5 * X;
  I := (PCnInteger(@X))^;
  I := $5f375a86 - (I shr 1);
  X := (PCnSingle(@I))^;
  X := X *(1.5 - xHalf * X * X);
  X := X *(1.5 - xHalf * X * X);
  Result := X;
end;

// ��λȷ�������ټ���������ƽ��������������
function FastSqrt(N: Cardinal): Cardinal;
var
  T, B: Cardinal;
  Sft: Cardinal;
begin
  Result := 0;
  B := $8000;
  Sft := 15;
  repeat
    T := ((Result shl 1)+ B) shl Sft;
    Dec(Sft);
    if N >= T then
    begin
      Result := Result + B;
      N := N - T;
    end;
    B := B shr 1;
  until B = 0;
end;

// ��λȷ�������ټ���������ƽ��������������
function FastSqrt64(N: Int64): Int64;
var
  T, B: Int64;
  Sft: Int64;
begin
  Result := 0;
  B := $80000000;
  Sft := 31;
  repeat
    T := ((Result shl 1)+ B) shl Sft;
    Dec(Sft);
    if N >= T then
    begin
      Result := Result + B;
      N := N - T;
    end;
    B := B shr 1;
  until B = 0;
end;

end.
