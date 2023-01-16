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

unit CnFEC;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：前向纠错实现单元，目前包括 Hamming （汉明码）校验
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：该单元目前只处理二进制位
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2019.06.20 V1.1
*               实现伽罗华 2^8 矩阵的运算
*           2019.05.28 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnMatrix;

type
  ECnHammingException = class(Exception);

  ECnCalculationRuleException = class(Exception);

  TCnCalculationRule = class
  {* 四则运算规则，子类可重载实现有限域运算规则}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(X, Y: Int64): Int64; virtual;
    function Subtract(X, Y: Int64): Int64; virtual;
    function Multiply(X, Y: Int64): Int64; virtual;
    function Divide(X, Y: Int64): Int64; virtual;
  end;

  TCnGalois2Power8Rule = class(TCnCalculationRule)
  {* 伽罗华域 GP(2^8) 里的多项式四则运算规则}
  private
    FExpToValue: array[0..255] of Integer;
    FValueToExp: array[0..255] of Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Add(X, Y: Int64): Int64; override;
    function Subtract(X, Y: Int64): Int64; override;
    function Multiply(X, Y: Int64): Int64; override;
    function Divide(X, Y: Int64): Int64; override;
  end;

  TCnGalois2Power8Matrix = class(TCnIntMatrix)
  {* 伽罗华域 GP(2^8) 里的多项式矩阵}
  protected
    procedure SetValue(Row, Col: Integer; const AValue: Int64); override;
    function NegativeOnePower(N: Integer): Integer; override;
    // 行列式计算中的加减替换动作因为加减均为异或，因此恒定返回 1
  public
    function OperationAdd(X, Y: Int64): Int64; override;
    function OperationSub(X, Y: Int64): Int64; override;
    function OperationMul(X, Y: Int64): Int64; override;
    function OperationDiv(X, Y: Int64): Int64; override;

    function Determinant: Int64; override;
    procedure Divide(Factor: Int64); override;
  end;

procedure CnCalcHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
{* 根据一批 Bits 计算其 Hamming 码，默认分组 8 bit 也就是 1 字节}

procedure CnVerifyHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
{* 根据 Hamming 编码过的 Bits 还原并校验其内容，默认分组 8 bit 也就是 1 字节}

function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
{* 根据 Hamming 分组的 bit 长度计算校验 bit 的长度}

function CnGalois2Power8Rule: TCnCalculationRule;
{* 返回全局的 GP(2^8) 的运算规则供外界调用}

implementation

const
  GALOIS2POWER8_LIMIT = 255;  // 伽罗华域 2^8 的最大范围
  GALOIS2POWER8_IRREDUCIBLE_POLYNOMIAL = $12D; // 伽罗华域 2^8 使用的不可约多项式之一，供取模用

var
  FGalois2Power8Rule: TCnCalculationRule = nil;

{* 返回全局的 GP(2^8) 的运算规则}
function CnGalois2Power8Rule: TCnCalculationRule;
begin
  if FGalois2Power8Rule = nil then
    FGalois2Power8Rule := TCnGalois2Power8Rule.Create;
  Result := FGalois2Power8Rule;
end;

// BlockBitCount (n), VerificationBitCount (k) 满足 2^k - 1 >= n + k
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
    raise ECnHammingException.CreateFmt('Error Hamming BlockBitCount: %d', [BlockBitCount]);
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

    // 拆开输入数据并将其填到输出里，留出 Hamming 码校验位空间
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

    // 计算多位 Hamming 码校验位并填进去，BitIdx 以 0 开始，VerificationBitCount - 1 结束，
    // 为了便于理解，OutIdx 所代表的下标均改成 1 开始
    while BitIdx < VerificationBitCount do
    begin
      // 对于 BitIdx 号 Hamming 校验码，计算方法是数据中，下标数字第 BitIdx 位为 1 的要参与异或，
      // 异或结果放下标为 VERIFICATION_BITS_COUNT[BitIdx] 中
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
    raise ECnHammingException.Create('Error InBits Calculate Hamming.');

  if InBits.Size mod BlockBitCount <> 0 then
    raise ECnHammingException.CreateFmt('Error Padding Size %d for Block Bit Count %d.', [InBits.Size, BlockBitCount]);

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

    // 计算多位 Hamming 码校验位并填进去，BitIdx 以 0 开始，VerificationBitCount - 1 结束，
    // 为了便于理解，OutIdx 所代表的下标均改成 1 开始
    while BitIdx < VerificationBitCount do
    begin
      // 对于 BitIdx 号 Hamming 校验码，计算方法是数据中，下标数字第 BitIdx 位为 1 的要参与异或，
      // 异或结果放下标为 VERIFICATION_BITS_COUNT[BitIdx] 中
      Ver := False;
      for OutIdx := 1 to BlockBitCount + VerificationBitCount do
      begin
        if (OutIdx and (1 shl BitIdx)) <> 0 then
          Ver := Ver xor InBits.Bits[InStartOffset + OutIdx - 1];
      end;

      if Ver then  // 有错误，拼纠错位置
        ErrIdx := ErrIdx or (1 shl BitIdx);

      Inc(BitIdx);
    end;

    // 纠错一位码
    if ErrIdx <> 0 then
    begin
      InBits.Bits[InStartOffset + ErrIdx - 1] := not
        InBits.Bits[InStartOffset + ErrIdx - 1];
    end;

    InIdx := 0;
    OutIdx := 0;
    // 纠错完毕后，拆开输入数据并将其填到输出里
    while InIdx < BlockBitCount + VerificationBitCount do
    begin
      while InIdx in VERIFICATION_BITS do
        Inc(InIdx);

      OutBits.Bits[OutStartOffset + OutIdx] := InBits.Bits[InStartOffset + InIdx];
      Inc(InIdx);
      Inc(OutIdx);
    end;
  end;

begin
  VerificationBitCount := CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount);

  if (InBits = nil) or (InBits.Size <= 0) then
    raise ECnHammingException.Create('Error InBits Verify Hamming.');

  if InBits.Size mod (BlockBitCount + VerificationBitCount) <> 0 then
    raise ECnHammingException.CreateFmt('Error Padding Size %d for Verify Bit Count %d.', [InBits.Size, VerificationBitCount]);

  OutBits.Size := (InBits.Size div (VerificationBitCount + BlockBitCount)) * BlockBitCount;
  OffsetIn := 0;
  OffsetOut := 0;

  while OffsetIn < InBits.Size - 1 do
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
    raise ECnCalculationRuleException.CreateFmt('Out of Range for Galois 2^8: %d', [X]);
end;

procedure CheckGalois2Power8Values(X, Y: Int64); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if (X < 0) or (X > GALOIS2POWER8_LIMIT) or
    (Y < 0) or (Y > GALOIS2POWER8_LIMIT) then
    raise ECnCalculationRuleException.CreateFmt('Out of Range for Galois 2^8: %d, %d', [X, Y]);
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
  // 查到对数结果，加，还原
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
  // 查到对数结果，减，还原
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
  // 用生成元 x 的幂来遍历并生成所有元素的正反映射表，
  // 对应不可约多项式是 x8+x5+x3+x2+1，也就是1 0010 1101

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
