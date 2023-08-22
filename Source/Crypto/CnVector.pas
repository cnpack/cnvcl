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

unit CnVector;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：向量计算单元
* 单元作者：刘啸
* 备    注：
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
  SysUtils, Classes, CnNative, CnContainers, CnBigNumber;

type
  ECnVectorException = class(Exception);
  {* 向量相关的异常}

  TCnInt64Vector = class(TCnInt64List)
  {* Int64 整数向量，下标值即为对应维度值}
  private
    function GetDimension: Integer;
    procedure SetDimension(const Value: Integer);

  public
    constructor Create(ADimension: Integer); virtual;
    {* 构造函数，参数是向量维度}

    property Dimension: Integer read GetDimension write SetDimension;
    {* 向量维度}
  end;

  TCnBigNumberVector = class(TCnBigNumberList)
  {* 大整数向量，下标值即为对应维度值}
  private
    function GetDimension: Integer;
    procedure SetDimension(const Value: Integer);

  public
    constructor Create(ADimension: Integer); virtual;
    {* 构造函数，参数是向量维度}

    property Dimension: Integer read GetDimension write SetDimension;
    {* 向量维度，设置后能自动创建大数对象}
  end;

  TCnBigNumberVectorPool = class(TCnMathObjectPool)
  {* 大整数向量池实现类，允许使用到大整数向量的地方自行创建大整数向量池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumberVector; reintroduce;
    procedure Recycle(Num: TCnBigNumberVector); reintroduce;
  end;

// ======================== Int64 整数向量计算函数 =============================

function Int64VectorModule(const V: TCnInt64Vector): Extended;
{* 返回 Int64 向量长度（模长），也即各项平方和的平方根}

function Int64VectorModuleSquare(const V: TCnInt64Vector): Int64;
{* 返回 Int64 向量长度（模长）的平方，也即各项平方的和}

procedure Int64VectorCopy(const Dst: TCnInt64Vector; const Src: TCnInt64Vector);
{* 复制 Int64 向量的内容}

procedure Int64VectorSwap(const A: TCnInt64Vector; const B: TCnInt64Vector);
{* 交换俩 Int64 向量的内容}

function Int64VectorEqual(const A: TCnInt64Vector; const B: TCnInt64Vector): Boolean;
{* 判断俩 Int64 向量是否相等}

procedure Int64VectorAdd(const Res: TCnInt64Vector; const A: TCnInt64Vector;
  const B: TCnInt64Vector);
{* 俩 Int64 向量的加法，和向量返回各维度对应和。Res 和 A B 可以是同一个对象}

procedure Int64VectorSub(const Res: TCnInt64Vector; const A: TCnInt64Vector;
  const B: TCnInt64Vector);
{* 俩 Int64 向量的减法，和向量返回各维度对应差。Res 和 A B 可以是同一个对象}

function Int64VectorDotProduct(const A: TCnInt64Vector; const B: TCnInt64Vector): Int64;
{* 俩 Int64 向量的标量乘法也就是点乘，返回各维度对应乘积之和。A 和 B 可以是同一个对象}

// ========================= 大整数向量计算函数 ================================

procedure BigNumberVectorModule(const Res: TCnBigNumber; const V: TCnBigNumberVector);
{* 返回大整数向量长度（模长），也即各项平方和的平方根，数字取整}

procedure BigNumberVectorModuleSquare(const Res: TCnBigNumber; const V: TCnBigNumberVector);
{* 返回大整数向量长度（模长）的平方，也即各项平方的和}

procedure BigNumberVectorCopy(const Dst: TCnBigNumberVector; const Src: TCnBigNumberVector);
{* 复制大整数向量的内容}

procedure BigNumberVectorSwap(const A: TCnBigNumberVector; const B: TCnBigNumberVector);
{* 交换俩大整数向量的内容}

function BigNumberVectorEqual(const A: TCnBigNumberVector; const B: TCnBigNumberVector): Boolean;
{* 判断俩大整数向量是否相等}

procedure BigNumberVectorAdd(const Res: TCnBigNumberVector; const A: TCnBigNumberVector;
  const B: TCnBigNumberVector);
{* 俩大整数向量的加法，和向量返回各维度对应和。Res 和 A B 可以是同一个对象}

procedure BigNumberVectorSub(const Res: TCnBigNumberVector; const A: TCnBigNumberVector;
  const B: TCnBigNumberVector);
{* 俩大整数向量的减法，和向量返回各维度对应差。Res 和 A B 可以是同一个对象}

procedure BigNumberVectorDotProduct(const Res: TCnBigNumber; A: TCnBigNumberVector;
  const B: TCnBigNumberVector);
{* 俩大整数向量的标量乘法也就是点乘，返回各维度对应乘积之和。A 和 B 可以是同一个对象}

implementation

resourcestring
  SCnErrorVectorDimensionInvalid = 'Invalid Dimension!';
  SCnErrorVectorDimensionNotEqual = 'Error Dimension NOT Equal!';

var
  FBigNumberPool: TCnBigNumberPool = nil;
  FBigNumberVectorPool: TCnBigNumberVectorPool = nil;

procedure CheckInt64VectorDimensionEqual(const A, B: TCnInt64Vector);
begin
  if A.Dimension <> B.Dimension then
    raise ECnVectorException.Create(SCnErrorVectorDimensionNotEqual);
end;

function Int64VectorModule(const V: TCnInt64Vector): Extended;
var
  I: Integer;
  S: Int64;
  T: Extended;
begin
  S := 0;
  for I := 0 to V.Dimension - 1 do
    S := S + V[I] * V[I];

  T := S;
  Result := Sqrt(T);
end;

function Int64VectorModuleSquare(const V: TCnInt64Vector): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to V.Dimension - 1 do
    Result := Result + V[I] * V[I];
end;

procedure Int64VectorCopy(const Dst: TCnInt64Vector; const Src: TCnInt64Vector);
var
  I: Integer;
begin
  if Src <> Dst then
  begin
    Dst.Dimension := Src.Dimension;
    for I := 0 to Src.Dimension - 1 do
      Dst[I] := Src[I];
  end;
end;

procedure Int64VectorSwap(const A: TCnInt64Vector; const B: TCnInt64Vector);
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

function Int64VectorEqual(const A: TCnInt64Vector; const B: TCnInt64Vector): Boolean;
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

procedure Int64VectorAdd(const Res: TCnInt64Vector; const A: TCnInt64Vector;
  const B: TCnInt64Vector);
var
  I: Integer;
begin
  CheckInt64VectorDimensionEqual(A, B);

  for I := 0 to A.Dimension - 1 do
    Res[I] := A[I] + B[I];
end;

procedure Int64VectorSub(const Res: TCnInt64Vector; const A: TCnInt64Vector;
  const B: TCnInt64Vector);
var
  I: Integer;
begin
  CheckInt64VectorDimensionEqual(A, B);

  for I := 0 to A.Dimension - 1 do
    Res[I] := A[I] - B[I];
end;

function Int64VectorDotProduct(const A: TCnInt64Vector; const B: TCnInt64Vector): Int64;
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
  Count := Value + 1; // 直接设置 Count，如变小，会自动释放多余的对象

  if Count > OC then  // 增加的部分创建新对象
  begin
    for I := OC to Count - 1 do
      Items[I] := TCnBigNumber.Create;
  end;
end;

procedure CheckBigNumberVectorDimensionEqual(const A, B: TCnBigNumberVector);
begin
  if A.Dimension <> B.Dimension then
    raise ECnVectorException.Create(SCnErrorVectorDimensionNotEqual);
end;

procedure BigNumberVectorModule(const Res: TCnBigNumber; const V: TCnBigNumberVector);
begin
  BigNumberVectorModuleSquare(Res, V);
  BigNumberSqrt(Res, Res);
end;

procedure BigNumberVectorModuleSquare(const Res: TCnBigNumber; const V: TCnBigNumberVector);
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

procedure BigNumberVectorCopy(const Dst: TCnBigNumberVector; const Src: TCnBigNumberVector);
var
  I: Integer;
begin
  if Src <> Dst then
  begin
    Dst.Dimension := Src.Dimension;
    for I := 0 to Src.Dimension - 1 do
      BigNumberCopy(Dst[I], Src[I]);
  end;
end;

procedure BigNumberVectorSwap(const A: TCnBigNumberVector; const B: TCnBigNumberVector);
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

function BigNumberVectorEqual(const A: TCnBigNumberVector; const B: TCnBigNumberVector): Boolean;
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

procedure BigNumberVectorAdd(const Res: TCnBigNumberVector; const A: TCnBigNumberVector;
  const B: TCnBigNumberVector);
var
  I: Integer;
begin
  CheckBigNumberVectorDimensionEqual(A, B);

  for I := 0 to A.Dimension - 1 do
    BigNumberAdd(Res[I], A[I], B[I]);
end;

procedure BigNumberVectorSub(const Res: TCnBigNumberVector; const A: TCnBigNumberVector;
  const B: TCnBigNumberVector);
var
  I: Integer;
begin
  CheckBigNumberVectorDimensionEqual(A, B);

  for I := 0 to A.Dimension - 1 do
    BigNumberSub(Res[I], A[I], B[I]);
end;

procedure BigNumberVectorDotProduct(const Res: TCnBigNumber; A: TCnBigNumberVector;
  const B: TCnBigNumberVector);
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

initialization
  FBigNumberPool := TCnBigNumberPool.Create;
  FBigNumberVectorPool := TCnBigNumberVectorPool.Create;

finalization
  FBigNumberVectorPool.Free;
  FBigNumberPool.Free;

end.
