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

unit CnFEC;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�ǰ��У�������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ���˻��ں����루Hamming����ǰ��У�����Ŀǰֻ�ܴ���һ������
*           ��λ�ĺ�����У�飬�� CnCalcHammingCode �� CnVerifyHammingCode ����Ϊ����
*           ע��У������Ա�ԭʼ�볤���١�
*
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.11.16 V1.2
*               ����У��Ĳ���˳��
*           2019.06.20 V1.1
*               ʵ��٤�޻� 2^8 ���������
*           2019.05.28 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnMatrix;

type
  ECnHammingException = class(Exception);
  {* ������ǰ��У������쳣}

  ECnCalculationRuleException = class(Exception);
  {* ǰ���������쳣}

  TCnCalculationRule = class
  {* ��������������������ʵ���������������}
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    function Add(X: Int64; Y: Int64): Int64; virtual;
    {* ����ʽ�ӷ���

       ������
         X: Int64                         - ����һ
         Y: Int64                         - ������

       ����ֵ��Int64                      - ��
    }

    function Subtract(X: Int64; Y: Int64): Int64; virtual;
    {* ����ʽ������

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ��
    }

    function Multiply(X: Int64; Y: Int64): Int64; virtual;
    {* ����ʽ�˷���

       ������
         X: Int64                         - ����һ
         Y: Int64                         - ������

       ����ֵ��Int64                      - ��
    }

    function Divide(X: Int64; Y: Int64): Int64; virtual;
    {* ����ʽ������

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ��
    }
  end;

  TCnGalois2Power8Rule = class(TCnCalculationRule)
  {* ٤�޻��� GP(2^8) ��Ķ���ʽ�����������}
  private
    FExpToValue: array[0..255] of Integer;
    FValueToExp: array[0..255] of Integer;
  public
    constructor Create; override;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    function Add(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ�ӷ���

       ������
         X: Int64                         - ����һ
         Y: Int64                         - ������

       ����ֵ��Int64                      - ��
    }

    function Subtract(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ������

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ��
    }

    function Multiply(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ�˷���

       ������
         X: Int64                         - ����һ
         Y: Int64                         - ������

       ����ֵ��Int64                      - ��
    }

    function Divide(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ������

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ��
    }
  end;

  TCnGalois2Power8Matrix = class(TCnIntMatrix)
  {* ٤�޻��� GP(2^8) ��Ķ���ʽ����}
  protected
    procedure SetValue(Row: Integer; Col: Integer; const AValue: Int64); override;
    {* ����ָ�����е�Ԫ�ء�

       ������
         Row: Integer                     - ָ����λ��
         Col: Integer                     - ָ����λ��
         const AValue: Int64              - �����õ�ֵ

       ����ֵ�����ޣ�
    }

    function NegativeOnePower(N: Integer): Integer; override;
    {* ���� -1 �� N �η�����Ϊ����ʽ�����еļӼ��滻������Ϊ�Ӽ���Ϊ������Դ˴��㶨���� 1��

       ������
         N: Integer                       - ָ��

       ����ֵ��Integer                    - ���� 1
    }

  public
    function OperationAdd(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ�����ڵļӷ���

       ������
         X: Int64                         - ����һ
         Y: Int64                         - ������

       ����ֵ��Int64                      - ��
    }

    function OperationSub(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ�����ڵļ�����

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ��
    }

    function OperationMul(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ�����ڵĳ˷���

       ������
         X: Int64                         - ����
         Y: Int64                         - ����

       ����ֵ��Int64                      - ��
    }

    function OperationDiv(X: Int64; Y: Int64): Int64; override;
    {* ٤�޻��� GP(2^8) ��Ķ���ʽ�����ڵĳ�����

       ������
         X: Int64                         - ������
         Y: Int64                         - ����

       ����ֵ��Int64                      - ��
    }

    function Determinant: Int64; override;
    {* ��������ʽֵ��

       ������
         ���ޣ�

       ����ֵ��Int64                      - ���ص�����ʽֵ
    }

    procedure Divide(Factor: Int64); override;
    {* �����Ԫ�س���һ��������

       ������
         Factor: Int64                    - ���Եĳ���

       ����ֵ�����ޣ�
    }
  end;

procedure CnCalcHammingCode(InBits: TBits; OutBits: TBits; BlockBitCount: Integer = 8);
{* ����һ�� Bits ������ Hamming �룬Ĭ�Ϸ��� 8 Bit Ҳ���� 1 �ֽڡ�
   ���� InBits �Ǵ��������ݣ�OutBits �Ǳ��������� InBits �����鳤�ȼ������У���룬
   ������Ϻ�InBits �� OutBits ��ͬ��������һ������������� InBits ���ܳ���
   ��һ��ʹ�� CnVerifyHammingCode �ж����޴��󲢾���

   ������
     InBits: TBits                        - ԭʼ������У���������
     OutBits: TBits                       - ���ɵ�У����
     BlockBitCount: Integer               - ���λ����

   ����ֵ�����ޣ�
}

procedure CnVerifyHammingCode(InBits: TBits; OutBits: TBits; BlockBitCount: Integer = 8);
{* ���� Hamming ������� Bits ��ԭ��У�������ݣ�Ĭ�Ϸ��� 8 Bit Ҳ���� 1 �ֽڡ�
   ���� InBits ���յ��Ŀ��ܳ����˵����ݣ�OutBits �� CnCalcHammingCode ���� InBits
   �����鳤�ȼ������У���롣������У���������ݲ���������

   ������
     InBits: TBits                        - �������Ĵ�У���λ���ݣ����������ɵ�����Ҳ������
     OutBits: TBits                       - �������Ĵ�У���У����
     BlockBitCount: Integer               - ���λ����

   ����ֵ�����ޣ�
}

function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
{* ���� Hamming ����� Bit ���ȼ���У�� Bit �ĳ��ȡ�

   ������
     BlockBitCount: Integer               - ������ķ���λ����

   ����ֵ��Integer                        - ���ؼ���ĺ�����У��λ����
}

function CnGalois2Power8Rule: TCnCalculationRule;
{* ����ȫ�ֵ� GP(2^8) ��������������á�

   ������
     ���ޣ�

   ����ֵ��TCnCalculationRule             - ���ص�ȫ�� GP(2^8) ���������ʵ��
}

implementation

resourcestring
  SCnErrorHammingBlockBitCount = 'Error Hamming BlockBitCount: %d';
  SCnErrorInBitsCalculateHamming = 'Error InBits Calculate Hamming.';
  SCnErrorPaddingSizeForBlockBit = 'Error Padding Size %d for Block Bit Count %d.';
  SCnErrorInBitsVerifyHamming = 'Error InBits Verify Hamming.';
  SCnErrorPaddingSizeForVerifyBit = 'Error Padding Size %d for Verify Bit Count %d.';
  SCnErrorOutOfRangeForGalois28 = 'Out of Range for Galois 2^8: %d';
  SCnErrorOutOfRangeForGalois281 = 'Out of Range for Galois 2^8: %d, %d';

const
  GALOIS2POWER8_LIMIT = 255;
  // ٤�޻��� 2^8 �����Χ

  GALOIS2POWER8_IRREDUCIBLE_POLYNOMIAL = $12D;
  // ٤�޻��� 2^8 ʹ�õĲ���Լ����ʽ֮һ����ȡģ��

var
  FGalois2Power8Rule: TCnCalculationRule = nil;

{* ����ȫ�ֵ� GP(2^8) ���������}
function CnGalois2Power8Rule: TCnCalculationRule;
begin
  if FGalois2Power8Rule = nil then
    FGalois2Power8Rule := TCnGalois2Power8Rule.Create;
  Result := FGalois2Power8Rule;
end;

// BlockBitCount (n), VerificationBitCount (k) ���� 2^k - 1 >= n + k
function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
begin
  if BlockBitCount = 1 then
    Result := 2
  else if BlockBitCount in [2..4] then
    Result := 3
  else if BlockBitCount in [5..11] then
    Result := 4
  else if BlockBitCount in [12..26] then
    Result := 5
  else if BlockBitCount in [27..57] then
    Result := 6
  else if BlockBitCount in [58..120] then
    Result := 7
  else
    raise ECnHammingException.CreateFmt(SCnErrorHammingBlockBitCount, [BlockBitCount]);
end;

procedure CnCalcHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
var
  OffsetIn, OffsetOut, VerificationBitCount: Integer;

  procedure CalcHammingBlock(InStartOffset, OutStartOffset: Integer);
  const
    VERIFICATION_BITS_COUNT: array[0..7] of Integer = (1, 2, 4, 8, 16, 32, 64, 128);
    VERIFICATION_BITS: set of Byte = [0, 1, 3, 7, 15, 31, 63, 127];
  var
    InIdx, OutIdx, BitIdx: Integer;
    Ver: Boolean;
  begin
    InIdx := 0;
    OutIdx := 0;

    // ���������ݲ�������������� Hamming ��У��λ�ռ�
    while InIdx < BlockBitCount do
    begin
      while OutIdx in VERIFICATION_BITS do
      begin
        OutBits.Bits[OutStartOffset + OutIdx] := False;
        Inc(OutIdx);
      end;
      OutBits.Bits[OutStartOffset + OutIdx] := InBits.Bits[InStartOffset + InIdx];
      Inc(InIdx);
      Inc(OutIdx);
    end;

    BitIdx := 0;

    // �����λ Hamming ��У��λ�����ȥ��BitIdx �� 0 ��ʼ��VerificationBitCount - 1 ������
    // Ϊ�˱�����⣬OutIdx ��������±���ĳ� 1 ��ʼ
    while BitIdx < VerificationBitCount do
    begin
      // ���� BitIdx �� Hamming У���룬���㷽���������У��±����ֵ� BitIdx λΪ 1 ��Ҫ�������
      // ��������±�Ϊ VERIFICATION_BITS_COUNT[BitIdx] ��
      Ver := False;
      for OutIdx := 1 to BlockBitCount + VerificationBitCount do
      begin
        if (OutIdx and (1 shl BitIdx)) <> 0 then
          Ver := Ver xor OutBits.Bits[OutStartOffset + OutIdx - 1];
      end;
      OutBits.Bits[OutStartOffset + VERIFICATION_BITS_COUNT[BitIdx] - 1] := Ver;

      Inc(BitIdx);
    end;
  end;

begin
  VerificationBitCount := CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount);

  if (InBits = nil) or (InBits.Size <= 0) then
    raise ECnHammingException.Create(SCnErrorInBitsCalculateHamming);

  if InBits.Size mod BlockBitCount <> 0 then
    raise ECnHammingException.CreateFmt(SCnErrorPaddingSizeForBlockBit, [InBits.Size, BlockBitCount]);

  OutBits.Size := (InBits.Size div BlockBitCount) * (BlockBitCount + VerificationBitCount);
  OffsetIn := 0;
  OffsetOut := 0;

  while OffsetIn < InBits.Size - 1 do
  begin
    CalcHammingBlock(OffsetIn, OffsetOut);
    Inc(OffsetIn, BlockBitCount);
    Inc(OffsetOut, BlockBitCount + VerificationBitCount);
  end;
end;

procedure CnVerifyHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
var
  OffsetIn, OffsetOut, VerificationBitCount: Integer;

  procedure VerifyHammingBlock(InStartOffset, OutStartOffset: Integer);
  const
    VERIFICATION_BITS_COUNT: array[0..7] of Integer = (1, 2, 4, 8, 16, 32, 64, 128);
    VERIFICATION_BITS: set of Byte = [0, 1, 3, 7, 15, 31, 63, 127];
  var
    InIdx, OutIdx, BitIdx, ErrIdx: Integer;
    Ver: Boolean;
  begin
    BitIdx := 0;
    ErrIdx := 0;

    // �����λ Hamming ��У��λ�����ȥ��BitIdx �� 0 ��ʼ��VerificationBitCount - 1 ������
    // Ϊ�˱�����⣬OutIdx ��������±���ĳ� 1 ��ʼ
    while BitIdx < VerificationBitCount do
    begin
      // ���� BitIdx �� Hamming У���룬���㷽���������У��±����ֵ� BitIdx λΪ 1 ��Ҫ�������
      // ��������±�Ϊ VERIFICATION_BITS_COUNT[BitIdx] ��
      Ver := False;
      for OutIdx := 1 to BlockBitCount + VerificationBitCount do
      begin
        if (OutIdx and (1 shl BitIdx)) <> 0 then
          Ver := Ver xor OutBits.Bits[InStartOffset + OutIdx - 1];
      end;

      if Ver then  // �д���ƴ����λ��
        ErrIdx := ErrIdx or (1 shl BitIdx);

      Inc(BitIdx);
    end;

    // ����һλ��
    if ErrIdx <> 0 then
    begin
      OutBits.Bits[InStartOffset + ErrIdx - 1] := not
        OutBits.Bits[InStartOffset + ErrIdx - 1];
    end;

    InIdx := 0;
    OutIdx := 0;
    // ������Ϻ󣬲��������ݲ�����������
    while InIdx < BlockBitCount + VerificationBitCount do
    begin
      while InIdx in VERIFICATION_BITS do
        Inc(InIdx);

      InBits.Bits[OutStartOffset + OutIdx] := OutBits.Bits[InStartOffset + InIdx];
      Inc(InIdx);
      Inc(OutIdx);
    end;
  end;

begin
  VerificationBitCount := CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount);

  if (OutBits = nil) or (OutBits.Size <= 0) then
    raise ECnHammingException.Create(SCnErrorInBitsVerifyHamming);

  if OutBits.Size mod (BlockBitCount + VerificationBitCount) <> 0 then
    raise ECnHammingException.CreateFmt(SCnErrorPaddingSizeForVerifyBit, [OutBits.Size, VerificationBitCount]);

  InBits.Size := (OutBits.Size div (VerificationBitCount + BlockBitCount)) * BlockBitCount;
  OffsetIn := 0;
  OffsetOut := 0;

  while OffsetIn < OutBits.Size - 1 do
  begin
    VerifyHammingBlock(OffsetIn, OffsetOut);
    Inc(OffsetIn, BlockBitCount + VerificationBitCount);
    Inc(OffsetOut, BlockBitCount);
  end;
end;

{ TCnCalculationRule }

function TCnCalculationRule.Add(X, Y: Int64): Int64;
begin
  Result := X + Y;
end;

function TCnCalculationRule.Subtract(X, Y: Int64): Int64;
begin
  Result := X - Y;
end;

function TCnCalculationRule.Multiply(X, Y: Int64): Int64;
begin
  Result := X * Y;
end;

function TCnCalculationRule.Divide(X, Y: Int64): Int64;
begin
  Result := X div Y;
end;

constructor TCnCalculationRule.Create;
begin

end;

destructor TCnCalculationRule.Destroy;
begin
  inherited;

end;

{ TCnGalois2Power8Rule }

procedure CheckGalois2Power8Value(X: Int64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (X < 0) or (X > GALOIS2POWER8_LIMIT) then
    raise ECnCalculationRuleException.CreateFmt(SCnErrorOutOfRangeForGalois28, [X]);
end;

procedure CheckGalois2Power8Values(X, Y: Int64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (X < 0) or (X > GALOIS2POWER8_LIMIT) or
    (Y < 0) or (Y > GALOIS2POWER8_LIMIT) then
    raise ECnCalculationRuleException.CreateFmt(SCnErrorOutOfRangeForGalois281, [X, Y]);
end;

function TCnGalois2Power8Rule.Add(X, Y: Int64): Int64;
begin
  CheckGalois2Power8Values(X, Y);
  Result := X xor Y;
end;

function TCnGalois2Power8Rule.Subtract(X, Y: Int64): Int64;
begin
  CheckGalois2Power8Values(X, Y);
  Result := X xor Y;
end;

function TCnGalois2Power8Rule.Multiply(X, Y: Int64): Int64;
var
  A, B: Integer;
begin
  CheckGalois2Power8Values(X, Y);
  if (X = 0) or (Y = 0) then
  begin
    Result := 0;
    Exit;
  end;
  // �鵽����������ӣ���ԭ
  A := FValueToExp[X];
  B := FValueToExp[Y];

  A := (A + B) mod GALOIS2POWER8_LIMIT;
  Result := FExpToValue[A];
end;

function TCnGalois2Power8Rule.Divide(X, Y: Int64): Int64;
var
  A, B: Integer;
begin
  CheckGalois2Power8Values(X, Y);
  // �鵽���������������ԭ
  if X = 0 then
  begin
    Result := 0;
    Exit;
  end;

  A := FValueToExp[X];
  B := FValueToExp[Y];
  if A < B then
    A := A + GALOIS2POWER8_LIMIT;

  A := (A - B) mod GALOIS2POWER8_LIMIT;
  Result := FExpToValue[A];
end;

constructor TCnGalois2Power8Rule.Create;
var
  I, J: Integer;
begin
  inherited;
  // ������Ԫ x ��������������������Ԫ�ص�����ӳ���
  // ��Ӧ����Լ����ʽ�� x8+x5+x3+x2+1��Ҳ����1 0010 1101

  FExpToValue[0] := 1;
  for I := 1 to 254 do
  begin
    J := FExpToValue[I - 1] shl 1;
    if (J and $100) <> 0 then
      J := J xor GALOIS2POWER8_IRREDUCIBLE_POLYNOMIAL;
    FExpToValue[I] := J;
  end;
  FExpToValue[255] := 0;

  FValueToExp[0] := 255;
  FValueToExp[1] := 0;
  for I := 1 to 254 do
    FValueToExp[FExpToValue[I]] := I;
end;

destructor TCnGalois2Power8Rule.Destroy;
begin

  inherited;
end;

{ TCnGalois2Power8Matrix }

function TCnGalois2Power8Matrix.Determinant: Int64;
begin
  Result := inherited Determinant;
  if Result < 0 then
    Inc(Result, GALOIS2POWER8_LIMIT)
  else
    Result := Result mod GALOIS2POWER8_LIMIT;
end;

procedure TCnGalois2Power8Matrix.Divide(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J] := OperationDiv(Value[I, J], Factor);
end;

function TCnGalois2Power8Matrix.NegativeOnePower(N: Integer): Integer;
begin
  Result := 1;
end;

function TCnGalois2Power8Matrix.OperationAdd(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Add(X, Y);
end;

function TCnGalois2Power8Matrix.OperationDiv(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Divide(X, Y);
end;

function TCnGalois2Power8Matrix.OperationMul(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Multiply(X, Y);
end;

function TCnGalois2Power8Matrix.OperationSub(X, Y: Int64): Int64;
begin
  Result := CnGalois2Power8Rule.Subtract(X, Y);
end;

procedure TCnGalois2Power8Matrix.SetValue(Row, Col: Integer;
  const AValue: Int64);
begin
  CheckGalois2Power8Value(AValue);
  inherited;
end;

end.
