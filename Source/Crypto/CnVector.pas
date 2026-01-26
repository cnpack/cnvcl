{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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

unit CnVector;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：向量计算单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 Int64 及大整数范围内的向量相关计算。
*           约定下标 0 代表向量行表达式最左边或列表达式最上面的维度的数据。
*
*           另外，没有向量叉乘也就是外积的实现，因为外积从三维向量朝高维推广较困难。
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.08.22 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnContainers, CnBigNumber, CnPolynomial;

type
  ECnVectorException = class(Exception);
  {* 向量相关的异常}

  TCnInt64Vector = class(TCnInt64List)
  {* Int64 整数向量，下标值即为对应维度值}
  private
    function GetDimension: Integer;
    procedure SetDimension(const Value: Integer);
  public
    constructor Create(ADimension: Integer = 1); virtual;
    {* 构造函数，参数是向量维度。

       参数：
         ADimension: Integer              - 向量维度

       返回值：TCnInt64Vector             - 返回创建的对象
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将 Int64 向量转成字符串。

       参数：
         （无）

       返回值：string                     - 返回向量字符串
    }

    property Dimension: Integer read GetDimension write SetDimension;
    {* 向量维度}
  end;

  TCnBigNumberVector = class(TCnBigNumberList)
  {* 大整数向量，下标值即为对应维度值}
  private
    function GetDimension: Integer;
    procedure SetDimension(const Value: Integer);
  public
    constructor Create(ADimension: Integer = 1); virtual;
    {* 构造函数，参数是向量维度。

       参数：
         ADimension: Integer              - 向量维度

       返回值：TCnBigNumberVector         - 返回创建的对象
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大整数向量转成字符串。

       参数：
         （无）

       返回值：string                     - 返回向量字符串
    }

    property Dimension: Integer read GetDimension write SetDimension;
    {* 向量维度，设置后内部能自动创建大数对象}
  end;

  TCnBigNumberVectorPool = class(TCnMathObjectPool)
  {* 大整数向量池实现类，允许使用到大整数向量的地方自行创建大整数向量池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumberVector; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigNumberVector         - 返回池中的大整数向量对象
    }

    procedure Recycle(Num: TCnBigNumberVector); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnBigNumberVector          - 待归还至池中的对象

       返回值：（无）
    }
  end;

  TCnInt64PolynomialVector = class(TCnInt64PolynomialList)
  {* 一元整系数多项式向量，下标值即为对应维度值}
  private
    function GetDimension: Integer;
    procedure SetDimension(const Value: Integer);
  public
    constructor Create(ADimension: Integer = 1); virtual;
    {* 构造函数，参数是向量维度。

       参数：
         ADimension: Integer              - 向量维度

       返回值：TCnInt64PolynomialVector   - 返回创建的对象
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大整数向量转成字符串。

       参数：
         （无）

       返回值：string                     - 返回向量字符串
    }

    property Dimension: Integer read GetDimension write SetDimension;
    {* 向量维度，设置后内部能自动创建一元整系数多项式对象}

  end;

  TCnInt64PolynomialVectorPool = class(TCnMathObjectPool)
  {* 一元整系数多项式向量池实现类，允许使用到一元整系数多项式向量的地方自行创建一元整系数多项式向量池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnInt64PolynomialVector; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnInt64PolynomialVector   - 返回池中的大整数向量对象
    }

    procedure Recycle(Num: TCnInt64PolynomialVector); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnInt64PolynomialVector    - 待归还至池中的对象

       返回值：（无）
    }
  end;

// ======================== Int64 整数向量计算函数 =============================

function Int64VectorToString(V: TCnInt64Vector): string;
{* 将 Int64 向量转换为字符串形式供输出。

   参数：
     V: TCnInt64Vector                    - 待转换的向量

   返回值：string                         - 返回向量的字符串形式
}

function Int64VectorModule(V: TCnInt64Vector): Extended;
{* 返回 Int64 向量长度（模长），也即各项平方和的平方根。

   参数：
     V: TCnInt64Vector                    - 待计算的向量

   返回值：Extended                       - 返回向量的模长
}

function Int64VectorModuleSquare(V: TCnInt64Vector): Int64;
{* 返回 Int64 向量长度（模长）的平方，也即各项平方的和。

   参数：
     V: TCnInt64Vector                    - 待计算的向量

   返回值：Int64                          - 返回向量的模长的平方
}

procedure Int64VectorCopy(Dest: TCnInt64Vector; Source: TCnInt64Vector);
{* 复制 Int64 向量的内容。

   参数：
     Dest: TCnInt64Vector                 - 目标向量
     Source: TCnInt64Vector               - 源向量

   返回值：（无）
}

procedure Int64VectorSwap(A: TCnInt64Vector; B: TCnInt64Vector);
{* 交换俩 Int64 向量的内容，要求两个向量同维。

   参数：
     A: TCnInt64Vector                    - 待交换的向量一
     B: TCnInt64Vector                    - 待交换的向量二

   返回值：（无）
}

function Int64VectorEqual(A: TCnInt64Vector; B: TCnInt64Vector): Boolean;
{* 判断俩 Int64 向量是否相等。

   参数：
     A: TCnInt64Vector                    - 待比较的向量一
     B: TCnInt64Vector                    - 待比较的向量二

   返回值：Boolean                        - 返回向量内容是否相等
}

procedure Int64VectorNegate(Res: TCnInt64Vector; A: TCnInt64Vector);
{* 求 Int64 向量的反向量，Res 和 A 可以是同一个对象。

   参数：
     Res: TCnInt64Vector                  - 结果反向量
     A: TCnInt64Vector                    - 原向量

   返回值：（无）
}

procedure Int64VectorAdd(Res: TCnInt64Vector; A: TCnInt64Vector; B: TCnInt64Vector);
{* 俩 Int64 向量的加法，和向量返回各维度对应和。Res 和 A、B 可以是同一个对象。

   参数：
     Res: TCnInt64Vector                  - 向量和
     A: TCnInt64Vector                    - 向量加数一
     B: TCnInt64Vector                    - 向量加数二

   返回值：（无）
}

procedure Int64VectorSub(Res: TCnInt64Vector; A: TCnInt64Vector; B: TCnInt64Vector);
{* 俩 Int64 向量的减法，差向量返回各维度对应差。Res 和 A、B 可以是同一个对象。

   参数：
     Res: TCnInt64Vector                  - 向量差
     A: TCnInt64Vector                    - 向量被减数
     B: TCnInt64Vector                    - 向量减数

   返回值：（无）
}

procedure Int64VectorMul(Res: TCnInt64Vector; A: TCnInt64Vector; N: Int64);
{* Int64 向量和数的标量乘法，也即每个维度乘以该数。Res 和 A 可以是同一个对象。

   参数：
     Res: TCnInt64Vector                  - 向量与数的标量乘结果
     A: TCnInt64Vector                    - 待标量乘的向量
     N: Int64                             - 乘数

   返回值：（无）
}

function Int64VectorDotProduct(A: TCnInt64Vector; B: TCnInt64Vector): Int64;
{* 俩 Int64 向量的标量乘法也就是点乘或者叫内积，返回各维度对应乘积之和。A 和 B 可以是同一个对象。

   参数：
     A: TCnInt64Vector                    - 点乘向量一
     B: TCnInt64Vector                    - 点乘向量二

   返回值：Int64                          - 返回点乘结果
}

// ========================= 大整数向量计算函数 ================================

function BigNumberVectorToString(V: TCnBigNumberVector): string;
{* 将大整数向量转换为字符串形式供输出。

   参数：
     V: TCnBigNumberVector                - 待转换的向量

   返回值：string                         - 返回向量的字符串形式
}

procedure BigNumberVectorModule(Res: TCnBigNumber; V: TCnBigNumberVector);
{* 返回大整数向量长度（模长），也即各项平方和的平方根，数字取整。

   参数：
     Res: TCnBigNumber                    - 待计算的向量
     V: TCnBigNumberVector                - 向量的模长，数字取整

   返回值：（无）
}

procedure BigNumberVectorModuleSquare(Res: TCnBigNumber; V: TCnBigNumberVector);
{* 返回大整数向量长度（模长）的平方，也即各项平方的和。

   参数：
     Res: TCnBigNumber                    - 待计算的向量
     V: TCnBigNumberVector                - 向量的模长的平方，数字取整

   返回值：（无）
}

procedure BigNumberVectorCopy(Dest: TCnBigNumberVector; Source: TCnBigNumberVector);
{* 复制大整数向量的内容。

   参数：
     Dst: TCnBigNumberVector              - 目标向量
     Src: TCnBigNumberVector              - 源向量

   返回值：（无）
}

procedure BigNumberVectorSwap(A: TCnBigNumberVector; B: TCnBigNumberVector);
{* 交换俩大整数向量的内容。

   参数：
     A: TCnBigNumberVector                - 待交换的向量一
     B: TCnBigNumberVector                - 待交换的向量二

   返回值：（无）
}

function BigNumberVectorEqual(A: TCnBigNumberVector; B: TCnBigNumberVector): Boolean;
{* 判断俩大整数向量是否相等。

   参数：
     A: TCnBigNumberVector                - 待比较的向量一
     B: TCnBigNumberVector                - 待比较的向量二

   返回值：Boolean                        - 返回向量内容是否相等
}

procedure BigNumberVectorNegate(Res: TCnBigNumberVector; A: TCnBigNumberVector);
{* 求大整数向量的反向量，Res 和 A 可以是同一个对象。

   参数：
     Res: TCnBigNumberVector              - 结果反向量
     A: TCnBigNumberVector                - 原向量

   返回值：（无）
}

procedure BigNumberVectorAdd(Res: TCnBigNumberVector; A: TCnBigNumberVector; B: TCnBigNumberVector);
{* 俩大整数向量的加法，和向量返回各维度对应和。Res 和 A、B 可以是同一个对象。

   参数：
     Res: TCnBigNumberVector              - 向量和
     A: TCnBigNumberVector                - 向量加数一
     B: TCnBigNumberVector                - 向量加数二

   返回值：（无）
}

procedure BigNumberVectorSub(Res: TCnBigNumberVector; A: TCnBigNumberVector; B: TCnBigNumberVector);
{* 俩大整数向量的减法，差向量返回各维度对应差。Res 和 A、B 可以是同一个对象。

   参数：
     Res: TCnBigNumberVector              - 向量差
     A: TCnBigNumberVector                - 向量被减数
     B: TCnBigNumberVector                - 向量减数

   返回值：（无）
}

procedure BigNumberVectorMul(Res: TCnBigNumberVector; A: TCnBigNumberVector; N: TCnBigNumber);
{* 大整数向量与数的标量乘法，也即每个维度乘以该数。Res 和 A 可以是同一个对象。

   参数：
     Res: TCnBigNumberVector              - 向量与数的标量乘结果
     A: TCnBigNumberVector                - 待标量乘的向量
     N: TCnBigNumber                      - 乘数

   返回值：（无）
}

procedure BigNumberVectorDotProduct(Res: TCnBigNumber; A: TCnBigNumberVector; B: TCnBigNumberVector);
{* 俩大整数向量的标量乘法也就是点乘，返回各维度对应乘积之和。A 和 B 可以是同一个对象。

   参数：
     Res: TCnBigNumber                    - 点乘结果
     A: TCnBigNumberVector                - 点乘向量一
     B: TCnBigNumberVector                - 点乘向量二

   返回值：（无）
}

// ==================== 一元整系数多项式向量计算函数 ===========================

function Int64PolynomialVectorToString(V: TCnInt64PolynomialVector): string;
{* 将一元整系数多项式向量转换为字符串形式供输出。

   参数：
     V: TCnInt64PolynomialVector          - 待转换的向量

   返回值：string                         - 返回向量的字符串形式
}

procedure Int64PolynomialVectorModuleSquare(Res: TCnInt64Polynomial; V: TCnInt64PolynomialVector);
{* 返回一元整系数多项式向量长度（模长）的平方，也即各项平方的和。

   参数：
     Res: TCnInt64Polynomial              - 待计算的向量
     V: TCnInt64PolynomialVector          - 向量的模长的平方，数字取整

   返回值：（无）
}

procedure Int64PolynomialVectorCopy(Dest: TCnInt64PolynomialVector; Source: TCnInt64PolynomialVector);
{* 复制一元整系数多项式向量的内容。

   参数：
     Dst: TCnInt64PolynomialVector        - 目标向量
     Src: TCnInt64PolynomialVector        - 源向量

   返回值：（无）
}

procedure Int64PolynomialVectorSwap(A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
{* 交换俩一元整系数多项式向量的内容。

   参数：
     A: TCnInt64PolynomialVector          - 待交换的向量一
     B: TCnInt64PolynomialVector          - 待交换的向量二

   返回值：（无）
}

function Int64PolynomialVectorEqual(A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector): Boolean;
{* 判断俩一元整系数多项式向量是否相等。

   参数：
     A: TCnInt64PolynomialVector          - 待比较的向量一
     B: TCnInt64PolynomialVector          - 待比较的向量二

   返回值：Boolean                        - 返回向量内容是否相等
}

procedure Int64PolynomialVectorNegate(Res: TCnInt64PolynomialVector; A: TCnInt64PolynomialVector);
{* 求一元整系数多项式向量的反向量，Res 和 A 可以是同一个对象。

   参数：
     Res: TCnInt64PolynomialVector        - 结果反向量
     A: TCnInt64PolynomialVector          - 原向量

   返回值：（无）
}

procedure Int64PolynomialVectorAdd(Res: TCnInt64PolynomialVector;
  A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
{* 俩一元整系数多项式向量的加法，和向量返回各维度对应和。Res 和 A、B 可以是同一个对象。

   参数：
     Res: TCnInt64PolynomialVector        - 向量和
     A: TCnInt64PolynomialVector          - 向量加数一
     B: TCnInt64PolynomialVector          - 向量加数二

   返回值：（无）
}

procedure Int64PolynomialVectorSub(Res: TCnInt64PolynomialVector;
  A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
{* 俩一元整系数多项式向量的减法，差向量返回各维度对应差。Res 和 A、B 可以是同一个对象。

   参数：
     Res: TCnInt64PolynomialVector        - 向量差
     A: TCnInt64PolynomialVector          - 向量被减数
     B: TCnInt64PolynomialVector          - 向量减数

   返回值：（无）
}

procedure Int64PolynomialVectorMul(Res: TCnInt64PolynomialVector;
  A: TCnInt64PolynomialVector; N: TCnInt64Polynomial);
{* 一元整系数多项式向量与数的标量乘法，也即每个维度乘以该数。Res 和 A 可以是同一个对象。

   参数：
     Res: TCnInt64PolynomialVector        - 向量与数的标量乘结果
     A: TCnInt64PolynomialVector          - 待标量乘的向量
     N: TCnInt64Polynomial                - 乘数

   返回值：（无）
}

procedure Int64PolynomialVectorDotProduct(Res: TCnInt64Polynomial;
  A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
{* 俩一元整系数多项式向量的标量乘法也就是点乘，返回各维度对应乘积之和。A 和 B 可以是同一个对象。

   参数：
     Res: TCnInt64Polynomial              - 点乘结果
     A: TCnInt64PolynomialVector          - 点乘向量一
     B: TCnInt64PolynomialVector          - 点乘向量二

   返回值：（无）
}

implementation

resourcestring
  SCnErrorVectorDimensionInvalid = 'Invalid Dimension!';
  SCnErrorVectorDimensionNotEqual = 'Error Dimension NOT Equal!';

var
  FBigNumberPool: TCnBigNumberPool = nil;
  FInt64PolynomialPool: TCnInt64PolynomialPool = nil;

// ======================= Int64 整数向量计算函数 ==============================

procedure CheckInt64VectorDimensionEqual(A, B: TCnInt64Vector);
begin
  if A.Dimension <> B.Dimension then
    raise ECnVectorException.Create(SCnErrorVectorDimensionNotEqual);
end;

function Int64VectorToString(V: TCnInt64Vector): string;
var
  I: Integer;
begin
  Result := '(';
  for I := 0 to V.Dimension - 1 do
  begin
    if I = 0 then
      Result := Result + IntToStr(V[I])
    else
      Result := Result + ', ' + IntToStr(V[I]);
  end;
  Result := Result + ')';
end;

function Int64VectorModule(V: TCnInt64Vector): Extended;
var
  T: Extended;
begin
  T := Int64VectorModuleSquare(V);
  Result := Sqrt(T);
end;

function Int64VectorModuleSquare(V: TCnInt64Vector): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to V.Dimension - 1 do
    Result := Result + V[I] * V[I];
end;

procedure Int64VectorCopy(Dest: TCnInt64Vector; Source: TCnInt64Vector);
var
  I: Integer;
begin
  if Source <> Dest then
  begin
    Dest.Dimension := Source.Dimension;
    for I := 0 to Source.Dimension - 1 do
      Dest[I] := Source[I];
  end;
end;

procedure Int64VectorSwap(A: TCnInt64Vector; B: TCnInt64Vector);
var
  I: Integer;
  T: Int64;
begin
  if A <> B then
  begin
    CheckInt64VectorDimensionEqual(A, B);

    for I := 0 to A.Dimension - 1 do
    begin
      T := A[I];
      A[I] := B[I];
      B[I] := T;
    end;
  end;
end;

function Int64VectorEqual(A: TCnInt64Vector; B: TCnInt64Vector): Boolean;
var
  I: Integer;
begin
  Result := A.Dimension = B.Dimension;
  if Result then
  begin
    for I := 0 to A.Dimension - 1 do
    begin
      if A[I] <> B[I] then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure Int64VectorNegate(Res: TCnInt64Vector; A: TCnInt64Vector);
var
  I: Integer;
begin
  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    Res[I] := -A[I];
end;

procedure Int64VectorAdd(Res: TCnInt64Vector; A: TCnInt64Vector; B: TCnInt64Vector);
var
  I: Integer;
begin
  CheckInt64VectorDimensionEqual(A, B);

  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    Res[I] := A[I] + B[I];
end;

procedure Int64VectorSub(Res: TCnInt64Vector; A: TCnInt64Vector; B: TCnInt64Vector);
var
  I: Integer;
begin
  CheckInt64VectorDimensionEqual(A, B);

  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    Res[I] := A[I] - B[I];
end;

procedure Int64VectorMul(Res: TCnInt64Vector; A: TCnInt64Vector; N: Int64);
var
  I: Integer;
begin
  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    Res[I] := A[I] * N;
end;

function Int64VectorDotProduct(A: TCnInt64Vector; B: TCnInt64Vector): Int64;
var
  I: Integer;
begin
  CheckInt64VectorDimensionEqual(A, B);

  Result := 0;
  for I := 0 to A.Dimension - 1 do
    Result := Result + A[I] * B[I];
end;

{ TCnInt64Vector }

constructor TCnInt64Vector.Create(ADimension: Integer);
begin
  inherited Create;
  SetDimension(ADimension);
end;

function TCnInt64Vector.GetDimension: Integer;
begin
  Result := Count;
end;

procedure TCnInt64Vector.SetDimension(const Value: Integer);
begin
  if Value <= 0 then
    raise ECnVectorException.Create(SCnErrorVectorDimensionInvalid);

  SetCount(Value);
end;

function TCnInt64Vector.ToString: string;
begin
  Result := Int64VectorToString(Self);
end;

// ========================= 大整数向量计算函数 ================================

procedure CheckBigNumberVectorDimensionEqual(A, B: TCnBigNumberVector);
begin
  if A.Dimension <> B.Dimension then
    raise ECnVectorException.Create(SCnErrorVectorDimensionNotEqual);
end;

function BigNumberVectorToString(V: TCnBigNumberVector): string;
var
  I: Integer;
begin
  Result := '(';
  for I := 0 to V.Dimension - 1 do
  begin
    if I = 0 then
      Result := Result + V[I].ToString
    else
      Result := Result + ', ' + V[I].ToString;
  end;
  Result := Result + ')';
end;

procedure BigNumberVectorModule(Res: TCnBigNumber; V: TCnBigNumberVector);
begin
  BigNumberVectorModuleSquare(Res, V);
  BigNumberSqrt(Res, Res);
end;

procedure BigNumberVectorModuleSquare(Res: TCnBigNumber; V: TCnBigNumberVector);
var
  I: Integer;
  T: TCnBigNumber;
begin
  Res.SetZero;
  T := FBigNumberPool.Obtain;
  try
    for I := 0 to V.Dimension - 1 do
    begin
      BigNumberMul(T, V[I], V[I]);
      BigNumberAdd(Res, Res, T);
    end;
  finally
    FBigNumberPool.Recycle(T);
  end;
end;

procedure BigNumberVectorCopy(Dest: TCnBigNumberVector; Source: TCnBigNumberVector);
var
  I: Integer;
begin
  if Source <> Dest then
  begin
    Dest.Dimension := Source.Dimension;
    for I := 0 to Source.Dimension - 1 do
      BigNumberCopy(Dest[I], Source[I]);
  end;
end;

procedure BigNumberVectorSwap(A: TCnBigNumberVector; B: TCnBigNumberVector);
var
  I: Integer;
begin
  if A <> B then
  begin
    CheckBigNumberVectorDimensionEqual(A, B);

    for I := 0 to A.Dimension - 1 do
      BigNumberSwap(A[I], B[I]);
  end;
end;

function BigNumberVectorEqual(A: TCnBigNumberVector; B: TCnBigNumberVector): Boolean;
var
  I: Integer;
begin
  Result := A.Dimension = B.Dimension;
  if Result then
  begin
    for I := 0 to A.Dimension - 1 do
    begin
      if not BigNumberEqual(A[I], B[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure BigNumberVectorNegate(Res: TCnBigNumberVector; A: TCnBigNumberVector);
var
  I: Integer;
begin
  BigNumberVectorCopy(Res, A);
  for I := 0 to A.Dimension - 1 do
    Res[I].Negate;
end;

procedure BigNumberVectorAdd(Res: TCnBigNumberVector; A: TCnBigNumberVector; B: TCnBigNumberVector);
var
  I: Integer;
begin
  CheckBigNumberVectorDimensionEqual(A, B);

  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    BigNumberAdd(Res[I], A[I], B[I]);
end;

procedure BigNumberVectorSub(Res: TCnBigNumberVector; A: TCnBigNumberVector; B: TCnBigNumberVector);
var
  I: Integer;
begin
  CheckBigNumberVectorDimensionEqual(A, B);

  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    BigNumberSub(Res[I], A[I], B[I]);
end;

procedure BigNumberVectorMul(Res: TCnBigNumberVector; A: TCnBigNumberVector; N: TCnBigNumber);
var
  I: Integer;
begin
  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    BigNumberMul(Res[I], A[I], N);
end;

procedure BigNumberVectorDotProduct(Res: TCnBigNumber; A: TCnBigNumberVector; B: TCnBigNumberVector);
var
  I: Integer;
  T: TCnBigNumber;
begin
  CheckBigNumberVectorDimensionEqual(A, B);

  Res.SetZero;
  T := FBigNumberPool.Obtain;
  try
    for I := 0 to A.Dimension - 1 do
    begin
      BigNumberMul(T, A[I], B[I]);
      BigNumberAdd(Res, Res, T);
    end;
  finally
    FBigNumberPool.Recycle(T);
  end;
end;

{ TCnBigNumberVector }

constructor TCnBigNumberVector.Create(ADimension: Integer);
begin
  inherited Create;
  SetDimension(ADimension);
end;

function TCnBigNumberVector.GetDimension: Integer;
begin
  Result := Count;
end;

procedure TCnBigNumberVector.SetDimension(const Value: Integer);
var
  I, OC: Integer;
begin
  if Value <= 0 then
    raise ECnVectorException.Create(SCnErrorVectorDimensionInvalid);

  OC := Count;
  Count := Value; // 直接设置 Count，如变小，会自动释放多余的对象

  if Count > OC then  // 增加的部分创建新对象
  begin
    for I := OC to Count - 1 do
      Items[I] := TCnBigNumber.Create;
  end;
end;

function TCnBigNumberVector.ToString: string;
begin
  Result := BigNumberVectorToString(Self);
end;

{ TCnBigNumberVectorPool }

function TCnBigNumberVectorPool.CreateObject: TObject;
begin
  Result := TCnBigNumberVector.Create(1);
end;

function TCnBigNumberVectorPool.Obtain: TCnBigNumberVector;
begin
  Result := TCnBigNumberVector(inherited Obtain);
  Result.SetDimension(1);
end;

procedure TCnBigNumberVectorPool.Recycle(Num: TCnBigNumberVector);
begin
  inherited Recycle(Num);
end;

// ======================= 整数多项式向量计算函数 ==============================

procedure CheckInt64PolynomialVectorDimensionEqual(A, B: TCnInt64PolynomialVector);
begin
  if A.Dimension <> B.Dimension then
    raise ECnVectorException.Create(SCnErrorVectorDimensionNotEqual);
end;

function Int64PolynomialVectorToString(V: TCnInt64PolynomialVector): string;
var
  I: Integer;
begin
  Result := '(';
  for I := 0 to V.Dimension - 1 do
  begin
    if I = 0 then
      Result := Result + V[I].ToString
    else
      Result := Result + ', ' + V[I].ToString;
  end;
  Result := Result + ')';
end;

procedure Int64PolynomialVectorModuleSquare(Res: TCnInt64Polynomial; V: TCnInt64PolynomialVector);
var
  I: Integer;
  T: TCnInt64Polynomial;
begin
  Res.SetZero;
  T := FInt64PolynomialPool.Obtain;
  try
    for I := 0 to V.Dimension - 1 do
    begin
      Int64PolynomialMul(T, V[I], V[I]);
      Int64PolynomialAdd(Res, Res, T);
    end;
  finally
    FInt64PolynomialPool.Recycle(T);
  end;
end;

procedure Int64PolynomialVectorCopy(Dest: TCnInt64PolynomialVector; Source: TCnInt64PolynomialVector);
var
  I: Integer;
begin
  if Source <> Dest then
  begin
    Dest.Dimension := Source.Dimension;
    for I := 0 to Source.Dimension - 1 do
      Int64PolynomialCopy(Dest[I], Source[I]);
  end;
end;

procedure Int64PolynomialVectorSwap(A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
var
  I: Integer;
begin
  if A <> B then
  begin
    CheckInt64PolynomialVectorDimensionEqual(A, B);

    for I := 0 to A.Dimension - 1 do
      Int64PolynomialSwap(A[I], B[I]);
  end;
end;

function Int64PolynomialVectorEqual(A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector): Boolean;
var
  I: Integer;
begin
  Result := A.Dimension = B.Dimension;
  if Result then
  begin
    for I := 0 to A.Dimension - 1 do
    begin
      if not Int64PolynomialEqual(A[I], B[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure Int64PolynomialVectorNegate(Res: TCnInt64PolynomialVector; A: TCnInt64PolynomialVector);
var
  I: Integer;
begin
  Int64PolynomialVectorCopy(Res, A);
  for I := 0 to A.Dimension - 1 do
    Res[I].Negate;
end;

procedure Int64PolynomialVectorAdd(Res: TCnInt64PolynomialVector;
  A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
var
  I: Integer;
begin
  CheckInt64PolynomialVectorDimensionEqual(A, B);

  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    Int64PolynomialAdd(Res[I], A[I], B[I]);
end;

procedure Int64PolynomialVectorSub(Res: TCnInt64PolynomialVector;
  A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
var
  I: Integer;
begin
  CheckInt64PolynomialVectorDimensionEqual(A, B);

  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    Int64PolynomialSub(Res[I], A[I], B[I]);
end;

procedure Int64PolynomialVectorMul(Res: TCnInt64PolynomialVector;
  A: TCnInt64PolynomialVector; N: TCnInt64Polynomial);
var
  I: Integer;
begin
  Res.Dimension := A.Dimension;
  for I := 0 to A.Dimension - 1 do
    Int64PolynomialMul(Res[I], A[I], N);
end;

procedure Int64PolynomialVectorDotProduct(Res: TCnInt64Polynomial;
  A: TCnInt64PolynomialVector; B: TCnInt64PolynomialVector);
var
  I: Integer;
  T: TCnInt64Polynomial;
begin
  CheckInt64PolynomialVectorDimensionEqual(A, B);

  Res.SetZero;
  T := FInt64PolynomialPool.Obtain;
  try
    for I := 0 to A.Dimension - 1 do
    begin
      Int64PolynomialMul(T, A[I], B[I]);
      Int64PolynomialAdd(Res, Res, T);
    end;
  finally
    FInt64PolynomialPool.Recycle(T);
  end;
end;

{ TCnInt64PolynomialVector }

constructor TCnInt64PolynomialVector.Create(ADimension: Integer);
begin
  inherited Create;
  SetDimension(ADimension);
end;

function TCnInt64PolynomialVector.GetDimension: Integer;
begin
  Result := Count;
end;

procedure TCnInt64PolynomialVector.SetDimension(const Value: Integer);
var
  I, OC: Integer;
begin
  if Value <= 0 then
    raise ECnVectorException.Create(SCnErrorVectorDimensionInvalid);

  OC := Count;
  Count := Value; // 直接设置 Count，如变小，会自动释放多余的对象

  if Count > OC then  // 增加的部分创建新对象
  begin
    for I := OC to Count - 1 do
      Items[I] := TCnInt64Polynomial.Create;
  end;
end;

function TCnInt64PolynomialVector.ToString: string;
begin
  Int64PolynomialVectorToString(Self);
end;

{ TCnInt64PolynomialVectorPool }

function TCnInt64PolynomialVectorPool.CreateObject: TObject;
begin
  Result := TCnInt64PolynomialVector.Create(1);
end;

function TCnInt64PolynomialVectorPool.Obtain: TCnInt64PolynomialVector;
begin
  Result := TCnInt64PolynomialVector(inherited Obtain);
  Result.SetDimension(1);
end;

procedure TCnInt64PolynomialVectorPool.Recycle(
  Num: TCnInt64PolynomialVector);
begin
  inherited Recycle(Num);
end;

initialization
  FBigNumberPool := TCnBigNumberPool.Create;
  FInt64PolynomialPool := TCnInt64PolynomialPool.Create;

finalization
  FInt64PolynomialPool.Free;
  FBigNumberPool.Free;

end.
