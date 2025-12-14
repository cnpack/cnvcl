{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnFEC;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：前向校验与纠错实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了基于汉明码（Hamming）及里德·所罗门码（Reed Solomon）的前向校验纠错，
*           前者目前只能处理一串二进制位的汉明码校验，注意校验码可以比原始码长不少。
*
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2025.12.14 V1.3
*               实现 Reed Solomon 纠错码
*           2023.11.16 V1.2
*               调整校验的参数顺序
*           2019.06.20 V1.1
*               实现伽罗华 2^8 矩阵的运算
*           2019.05.28 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnMatrix;

type
  ECnHammingException = class(Exception);
  {* 汉明码前向校验相关异常}

  ECnReedSolomonException = class(Exception);
  {* 里德所罗门纠错相关异常}

  ECnCalculationRuleException = class(Exception);
  {* 前向纠错相关异常}

  TCnCalculationRule = class
  {* 四则运算规则，子类可重载实现有限域运算规则}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function Add(X: Int64; Y: Int64): Int64; virtual;
    {* 多项式加法。

       参数：
         X: Int64                         - 加数一
         Y: Int64                         - 加数二

       返回值：Int64                      - 和
    }

    function Subtract(X: Int64; Y: Int64): Int64; virtual;
    {* 多项式减法。

       参数：
         X: Int64                         - 被减数
         Y: Int64                         - 减数

       返回值：Int64                      - 差
    }

    function Multiply(X: Int64; Y: Int64): Int64; virtual;
    {* 多项式乘法。

       参数：
         X: Int64                         - 乘数一
         Y: Int64                         - 乘数二

       返回值：Int64                      - 积
    }

    function Divide(X: Int64; Y: Int64): Int64; virtual;
    {* 多项式除法。

       参数：
         X: Int64                         - 被除数
         Y: Int64                         - 除数

       返回值：Int64                      - 商
    }
  end;

  TCnGalois2Power8Rule = class(TCnCalculationRule)
  {* 伽罗华域 GP(2^8) 里的多项式四则运算规则}
  private
    FExpToValue: array[0..255] of Integer;
    FValueToExp: array[0..255] of Integer;
  public
    constructor Create; override;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function Add(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式加法。

       参数：
         X: Int64                         - 加数一
         Y: Int64                         - 加数二

       返回值：Int64                      - 和
    }

    function Subtract(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式减法。

       参数：
         X: Int64                         - 被减数
         Y: Int64                         - 减数

       返回值：Int64                      - 差
    }

    function Multiply(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式乘法。

       参数：
         X: Int64                         - 乘数一
         Y: Int64                         - 乘数二

       返回值：Int64                      - 积
    }

    function Divide(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式除法。

       参数：
         X: Int64                         - 被除数
         Y: Int64                         - 除数

       返回值：Int64                      - 商
    }
  end;

  TCnGalois2Power8Matrix = class(TCnIntMatrix)
  {* 伽罗华域 GP(2^8) 里的多项式矩阵}
  protected
    procedure SetValue(Row: Integer; Col: Integer; const AValue: Int64); override;
    {* 设置指定行列的元素。

       参数：
         Row: Integer                     - 指定行位置
         Col: Integer                     - 指定列位置
         const AValue: Int64              - 待设置的值

       返回值：（无）
    }

    function NegativeOnePower(N: Integer): Integer; override;
    {* 计算 -1 的 N 次方。因为行列式计算中的加减替换动作因为加减均为异或，所以此处恒定返回 1。

       参数：
         N: Integer                       - 指数

       返回值：Integer                    - 返回 1
    }

  public
    function OperationAdd(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式矩阵内的加法。

       参数：
         X: Int64                         - 加数一
         Y: Int64                         - 加数二

       返回值：Int64                      - 和
    }

    function OperationSub(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式矩阵内的减法。

       参数：
         X: Int64                         - 被减数
         Y: Int64                         - 减数

       返回值：Int64                      - 差
    }

    function OperationMul(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式矩阵内的乘法。

       参数：
         X: Int64                         - 乘数
         Y: Int64                         - 乘数

       返回值：Int64                      - 积
    }

    function OperationDiv(X: Int64; Y: Int64): Int64; override;
    {* 伽罗华域 GP(2^8) 里的多项式矩阵内的除法。

       参数：
         X: Int64                         - 被除数
         Y: Int64                         - 除数

       返回值：Int64                      - 商
    }

    function Determinant: Int64; override;
    {* 求方阵行列式值。

       参数：
         （无）

       返回值：Int64                      - 返回的行列式值
    }

    procedure Divide(Factor: Int64); override;
    {* 矩阵各元素除以一个常数。

       参数：
         Factor: Int64                    - 除以的常数

       返回值：（无）
    }
  end;

procedure CnCalcHammingCode(InBits: TBits; OutBits: TBits; BlockBitCount: Integer = 8);
{* 根据一批 Bits 计算其 Hamming 码，默认分组 8 Bit 也就是 1 字节。
   假设 InBits 是待发送内容，OutBits 是本函数根据 InBits 及分组长度计算出的校验码，
   计算完毕后，InBits 和 OutBits 共同发送至另一处，传输过程中 InBits 可能出错。
   另一处使用 CnVerifyHammingCode 判断有无错误并纠错。

   参数：
     InBits: TBits                        - 原始待生成校验码的内容
     OutBits: TBits                       - 生成的校验码
     BlockBitCount: Integer               - 块的位长度

   返回值：（无）
}

procedure CnVerifyHammingCode(InBits: TBits; OutBits: TBits; BlockBitCount: Integer = 8);
{* 根据 Hamming 编码过的 Bits 还原并校验其内容，默认分组 8 Bit 也就是 1 字节。
   假设 InBits 是收到的可能出错了的内容，OutBits 是 CnCalcHammingCode 根据 InBits
   及分组长度计算出的校验码。本函数校验两者内容并尽量纠错。

   参数：
     InBits: TBits                        - 传输来的待校验的位内容，纠错所生成的内容也在其中
     OutBits: TBits                       - 传输来的待校验的校验码
     BlockBitCount: Integer               - 块的位长度

   返回值：（无）
}

function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
{* 根据 Hamming 分组的 Bit 长度计算校验 Bit 的长度。

   参数：
     BlockBitCount: Integer               - 汉明码的分组位长度

   返回值：Integer                        - 返回计算的汉明码校验位长度
}

function CnGalois2Power8Rule: TCnCalculationRule;
{* 返回全局的 GP(2^8) 的运算规则供外界调用。

   参数：
     （无）

   返回值：TCnCalculationRule             - 返回的全局 GP(2^8) 的运算规则实例
}

function CnCalcReedSolomonCode(Data: TBytes; NeedByteLength: Integer): TBytes;
{* 根据字节数组内容及要求的纠错长度 M，计算其 Reed Solomon 纠错码，内部默认分组 8 Bit 也就是 1 字节。

   参数：
     Data: TBytes                         - 原始待生成校验码的字节数组，长度即算法中的 N
     NeedByteLength: Integer              - 生成的带校验码的总数据长度，也即算法中的 M

   返回值：TBytes                         - 返回原始及带校验码的数据
}

function CnVerifyReedSolomonCode(Code: TBytes; CodeByteLength: Integer; Indexes: TBytes = nil): TBytes;
{* 根据 Reed Solomon 纠错码还原并校验其内容，内部默认分组 8 Bit 也就是 1 字节。
   Code 表示挑出的原始数据长度个字节（其他的是错误字节抛弃掉了），
   Indexes 代表这批字节在纠错码中的序号，下标从 0 开始到原始数据长度 - 1，
   内容从 0 开始到纠错数据长度也就是 CodeByteLength - 1。
   如传 nil，表示 Code 是纠错码前 M 位，直接当成原始数据返回。

   参数：
     Code: TBytes                         - 传输来的字节数组，混合纠错所生成的内容挑出了原始数据长度的字节
     CodeByteLength: Integer              - 本次纠错码的总字节长度
     Indexes: TBytes                      - 纠错码各字节的序号，如传 nil，表示序号就是前 N 个

   返回值：TBytes                         - 返回原始数据
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
  SCnErrorVandermondeMatrixParam = 'Vandermonde Matrix Params Error.';
  SCnErrorReedSolomonIndexParam = 'Reed Solomon Code Index Error.';

const
  GALOIS2POWER8_LIMIT = 255;
  // 伽罗华域 2^8 的最大范围

  GALOIS2POWER8_IRREDUCIBLE_POLYNOMIAL = $12D;
  // 伽罗华域 2^8 使用的不可约多项式之一，供取模用

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
          Ver := Ver xor OutBits.Bits[InStartOffset + OutIdx - 1];
      end;

      if Ver then  // 有错误，拼纠错位置
        ErrIdx := ErrIdx or (1 shl BitIdx);

      Inc(BitIdx);
    end;

    // 纠错一位码
    if ErrIdx <> 0 then
    begin
      OutBits.Bits[InStartOffset + ErrIdx - 1] := not
        OutBits.Bits[InStartOffset + ErrIdx - 1];
    end;

    InIdx := 0;
    OutIdx := 0;
    // 纠错完毕后，拆开输入数据并将其填到输出里
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

// 生成 M 行 N 列的在伽罗华域上的范德蒙矩阵
procedure VandermondeGalois(V: TCnGalois2Power8Matrix; M, N: Integer);
var
  I, J: Integer;
  Arr: array of Int64;
begin
  if (M <= 0) or (N <= 0) or (M <= N) then
    raise ECnReedSolomonException.Create(SCnErrorVandermondeMatrixParam);

  if V = nil then
    Exit;

  V.RowCount := M;
  V.ColCount := N;

  for I := 0 to N - 1 do
  begin
    for J := 0 to N - 1 do
      if I = J then
        V.Value[I, J] := 1
      else
        V.Value[I, J] := 0;
  end;

  for J := 0 to N - 1 do
    V.Value[N, J] := 1;

  SetLength(Arr, N);
  for I := 0 to N - 1 do
    Arr[I] := I + 1;

  for I := N + 1 to M - 1 do
  begin
    for J := 0 to N - 1 do
    begin
      V.Value[I, J] := Arr[J];
      Arr[J] := CnGalois2Power8Rule.Multiply(Arr[J], Arr[J]);
    end;
  end;
end;

function CnCalcReedSolomonCode(Data: TBytes; NeedByteLength: Integer): TBytes;
var
  V, D, R: TCnGalois2Power8Matrix;
  M, N, I: Integer;
begin
  V := nil;
  D := nil;
  R := nil;

  M := NeedByteLength;
  N := Length(Data);

  if (M <= 0) or (N <= 0) or (M <= N) then
    raise ECnReedSolomonException.Create(SCnErrorVandermondeMatrixParam);

  try
    V := TCnGalois2Power8Matrix.Create(1, 1);
    VandermondeGalois(V, M, N);

    D := TCnGalois2Power8Matrix.Create(N, 1);
    R := TCnGalois2Power8Matrix.Create(1, 1);

    for I := 0 to N - 1 do
      D.Value[I, 0] := Data[I];

    CnMatrixMul(V, D, R); // M 行 N 列 * N 行 1 列 = M 行 1 列
    SetLength(Result, M);
    for I := 0 to M - 1 do
      Result[I] := R[I, 0];
  finally
    R.Free;
    D.Free;
    V.Free;
  end;
end;

function CnVerifyReedSolomonCode(Code: TBytes; CodeByteLength: Integer; Indexes: TBytes): TBytes;
var
  V, D, R: TCnGalois2Power8Matrix;
  M, N, I: Integer;
  L: TList;
begin
  M := CodeByteLength;
  N := Length(Code);

  if (M <= 0) or (N <= 0) or (M <= N) or ((Length(Indexes) > 0) and (Length(Indexes) <> N)) then
    raise ECnReedSolomonException.Create(SCnErrorVandermondeMatrixParam);

  if Length(Indexes) = 0 then
  begin
    Result := NewBytesFromMemory(@Code[0], N);
    Exit;
  end;

  // 检查码块
  if Length(Indexes) > 1 then
  begin
    for I := 0 to Length(Indexes) - 1 do
    begin
      // 内容必须递增
      if I < Length(Indexes) - 1 then
      begin
        if Indexes[I + 1] <= Indexes[I] then
          raise ECnReedSolomonException.Create(SCnErrorReedSolomonIndexParam);
      end;

      // 最大不能超过整个纠错码长度
      if Indexes[I] >= M then
        raise ECnReedSolomonException.Create(SCnErrorReedSolomonIndexParam);
    end;
  end;

  // 开始还原

  V := nil;
  D := nil;
  R := nil;
  L := nil;

  try
    V := TCnGalois2Power8Matrix.Create(1, 1);
    VandermondeGalois(V, M, N);

    // 根据 Indexes 里缺失的部分，去掉 V 里对应的行
    L := TList.Create;
    for I := 0 to M - 1 do            // 准备 0 到 M - 1 的全集
      L.Add(Pointer(I));

    for I := N - 1 downto 0 do        // 删去已经在 Indexes 中出现的
      L.Remove(Pointer(Indexes[I]));

    for I := L.Count - 1 downto 0 do  // 剩下的就是该在范德蒙矩阵删去的内容
      V.DeleteRow(Integer(L[I]));

    // 准备残缺数据的矩阵
    D := TCnGalois2Power8Matrix.Create(N, 1);
    for I := 0 to N - 1 do
      D.Value[I, 0] := Code[I];

    // 求残缺范德蒙矩阵的逆
    R := TCnGalois2Power8Matrix.Create(1, 1);
    CnMatrixInverse(V, R);

    CnMatrixMul(R, D, V); // V 没用了，复用之。N 行 N 列 * N 行 1 列 = N 行 1 列
    SetLength(Result, N);
    for I := 0 to N - 1 do
      Result[I] := V[I, 0];
  finally
    R.Free;
    D.Free;
    L.Free;
    V.Free;
  end;
end;

end.
