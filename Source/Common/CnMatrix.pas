{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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

unit CnMatrix;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：整数矩阵运算实现单元
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：高阶行列式的代数余子式计算方法初步验证通过，矩阵求逆结果可能不是整数
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2019.06.5 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

type
  ECnMatrixException = class(Exception);

  TCnIntMatrix = class(TPersistent)
  {* Int64 范围内的整数矩阵的实现类}
  private
    FMatrix: array of array of Int64;
    FColCount: Integer;
    FRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure CheckCount(Value: Int64);
    procedure SetValue(Row, Col: Integer; const Value: Int64);
    function GetValue(Row, Col: Integer): Int64;
  protected
    function OperationAdd(X, Y: Int64): Int64; virtual;
    function OperationMul(X, Y: Int64): Int64; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ARowCount, AColCount: Integer); virtual;
    destructor Destroy; override;

    procedure Mul(Factor: Int64);
    {* 矩阵各元素乘以一个常数}
    procedure Divide(Factor: Int64);
    {* 矩阵各元素除以一个常数}
    procedure Add(Factor: Int64);
    {* 矩阵各元素加上一个常数}
    procedure SetE(Size: Integer);
    {* 设置为 Size 阶单位矩阵}
    procedure SetZero;
    {* 设置为全 0 矩阵}

    function Determinant: Int64;
    {* 求方阵行列式值}
    function Trace: Int64;
    {* 求方阵的迹，也就是对角线元素的和}
    function IsSquare: Boolean;
    {* 是否方阵}
    function IsZero: Boolean;
    {* 是否全 0 方阵}
    function IsE: Boolean;
    {* 是否单位方阵}
    function IsSymmetrical: Boolean;
    {* 是否对称方阵}
    function IsSingular: Boolean;
    {* 是否奇异方阵，也就是行列式是否等于 0}

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* 输出到字符串}

    property Value[Row, Col: Integer]: Int64 read GetValue write SetValue;
    {* 根据行列下标访问矩阵元素，下标都从 0 开始}
  published
    property ColCount: Integer read FColCount write SetColCount;
    {* 矩阵列数}
    property RowCount: Integer read FRowCount write SetRowCount;
    {* 矩阵行数}
  end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix);
{* 两个矩阵相乘，结果放 MulResult 矩阵中，要求 Matrix1 列数与 Martrix2 行数相等。
  MulResult 不能是 Matrix1 或 Matrix2}

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix);
{* 求方阵 K 次幂，结果放 PowerResult 矩阵中，PowerResult 不能是 Matrix}

procedure CnMatrixAdd(Matrix1, Matrix2: TCnIntMatrix; AddResult: TCnIntMatrix);
{* 两个矩阵相加，结果放 AddResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 行数相等。
  AddResult 可以是 Matrix1 或 Matrix2 或其他}

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix);
{* 两个矩阵哈达马相乘，结果放 ProductResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 行数相等。
  ProductResult 可以是 Matrix1 或 Matrix2 或其他}

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnIntMatrix);
{* 转置矩阵，将第一个矩阵转置至第二个，Matrix1、Matrix2 可以相等}

procedure CnMatrixMinor(Matrix: TCnIntMatrix; Row, Col: Integer; MinorResult: TCnIntMatrix);
{* 求矩阵的余子式，也即去除指定行列后剩下的矩阵}

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnIntMatrix);
{* 求方阵的伴随阵}

procedure CnMatrixInverse(Matrix1, Matrix2: TCnIntMatrix);
{* 求方阵的逆矩阵，但对于 TCnIntMatrix 来讲，逆矩阵是伴随阵除以行列式，结果不一定是整数，表达可能出错}

implementation

// 计算 -1 的 N 次方，供求代数余子式用
function NegativeOnePower(N: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (N and 1) * (-2) + 1;
end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix);
var
  I, J, K, T, Sum: Integer;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create('Matrix Result can not Be Factors.');

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create('Matrix 1 Col Count must Equal to Matrix 2 Row Count.');

  MulResult.RowCount := Matrix1.RowCount;
  MulResult.ColCount := Matrix2.ColCount;

  // Value[I, J] := 矩阵 1 第 I 行与矩阵 2 第 J 列对应乘并相加
  for I := 0 to Matrix1.RowCount - 1 do
  begin
    for J := 0 to Matrix2.ColCount - 1 do
    begin
      Sum := 0;
      for K := 0 to Matrix1.ColCount - 1 do
      begin
        T := Matrix1.OperationMul(Matrix1.Value[I, K], Matrix2.Value[K, J]);
        Sum := Matrix1.OperationAdd(Sum, T);
      end;
      MulResult.Value[I, J] := Sum;
    end;
  end;
end;

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix);
var
  I: Integer;
  T: TCnIntMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create('Matrix Power Must Be Square.');

  if K < 0 then
    raise ECnMatrixException.Create('Invalid Matrix Power.');

  if K = 0 then
  begin
    PowerResult.SetE(Matrix.RowCount);
    Exit;
  end
  else if K = 1 then
  begin
    PowerResult.Assign(Matrix);
    Exit;
  end;

  T := TCnIntMatrix.Create(Matrix.RowCount, Matrix.ColCount);
  try
    T.Assign(Matrix);
    for I := 0 to K - 2 do
    begin
      CnMatrixMul(Matrix, T, PowerResult);
      T.Assign(PowerResult);
    end;
  finally
    T.Free;
  end;
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnIntMatrix; AddResult: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create('Matrix 1/2 Row/Col Count must Equal.');

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      AddResult.Value[I, J] := Matrix1.OperationAdd(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create('Matrix 1/2 Row/Col Count must Equal.');

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      ProductResult.Value[I, J] := Matrix1.OperationMul(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnIntMatrix);
var
  I, J: Integer;
  Tmp: TCnIntMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnIntMatrix.Create(1, 1);
    try
      Tmp.Assign(Matrix1);
      Matrix2.ColCount := Tmp.RowCount;
      Matrix2.RowCount := Tmp.ColCount;

      for I := 0 to Tmp.RowCount - 1 do
        for J := 0 to Tmp.ColCount - 1 do
          Matrix2.Value[J, I] := Tmp.Value[I, J];
    finally
      Tmp.Free;
    end;
  end
  else
  begin
    Matrix2.ColCount := Matrix1.RowCount;
    Matrix2.RowCount := Matrix1.ColCount;

    for I := 0 to Matrix1.RowCount - 1 do
      for J := 0 to Matrix1.ColCount - 1 do
        Matrix2.Value[J, I] := Matrix1.Value[I, J];
  end;
end;

procedure CnMatrixMinor(Matrix: TCnIntMatrix; Row, Col: Integer; MinorResult: TCnIntMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.Create('Invalid Minor Row or Col.');

  MinorResult.ColCount := Matrix.ColCount - 1;
  MinorResult.RowCount := Matrix.RowCount - 1;

  SR := 0;
  DR := 0;

  while SR < Matrix.RowCount do
  begin
    if SR = Row then
    begin
      Inc(SR);
      if SR = Matrix.RowCount then
        Break;
    end;

    SC := 0;
    DC := 0;
    while SC < Matrix.ColCount do
    begin
      if SC = Col then
      begin
        Inc(SC);
        if SC = Matrix.ColCount then
          Break;
      end;

      MinorResult.Value[DR, DC] := Matrix.Value[SR, SC];
      Inc(SC);
      Inc(DC);
    end;

    Inc(SR);
    Inc(DR);
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnIntMatrix);
var
  I, J: Integer;
  Minor: TCnIntMatrix;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create('Only Square can Adjoint.');

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnIntMatrix.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1);
  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Matrix2.Value[I, J] := NegativeOnePower(I + J) * Minor.Determinant;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    Minor.Free;
  end;
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnIntMatrix);
var
  D: Int64;
begin
  D := Matrix1.Determinant;
  if D = 0 then
    raise ECnMatrixException.Create('NO Inverse Matrix for Deteminant is 0');

  CnMatrixAdjoint(Matrix1, Matrix2);
  Matrix2.Divide(D); // 注意不一定是整数了，不推荐直接用逆矩阵
end;

{ TCnIntMatrix }

procedure TCnIntMatrix.Add(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationAdd(FMatrix[I, J], Factor);
end;

procedure TCnIntMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnIntMatrix then
  begin
    TCnIntMatrix(Dest).RowCount := FRowCount;
    TCnIntMatrix(Dest).ColCount := FColCount;

    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        TCnIntMatrix(Dest).Value[I, J] := FMatrix[I, J];
  end
  else
    inherited;
end;

procedure TCnIntMatrix.CheckCount(Value: Int64);
begin
  if Value <= 0 then
    raise ECnMatrixException.Create('Error Row or Col Count: ' + IntToStr(Value));
end;

constructor TCnIntMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRowCount := ARowCount;
  FColCount := AColCount;
  SetLength(FMatrix, FRowCount, FColCount);
end;

destructor TCnIntMatrix.Destroy;
begin
  SetLength(FMatrix, 0);
  inherited;
end;

function TCnIntMatrix.Determinant: Int64;
var
  I: Integer;
  Minor: TCnIntMatrix;
begin
  if not IsSquare then
    raise ECnMatrixException.Create('Only Square can Determinant.');

  if FRowCount = 1 then
    Result := FMatrix[0, 0]
  else if FRowCount = 2 then
    Result := FMatrix[0, 0] * FMatrix[1, 1] - FMatrix[0, 1] * FMatrix[1, 0]
  else if RowCount = 3 then
  begin
    Result := FMatrix[0, 0] * FMatrix[1, 1] * FMatrix[2, 2]
      + FMatrix[0, 1] * FMatrix[1, 2] * FMatrix[2, 0]
      + FMatrix[0, 2] * FMatrix[1, 0] * FMatrix[2, 1]
      - FMatrix[0, 0] * FMatrix[1, 2] * FMatrix[2, 1]
      - FMatrix[0, 1] * FMatrix[1, 0] * FMatrix[2, 2]
      - FMatrix[0, 2] * FMatrix[1, 1] * FMatrix[2, 0];
  end
  else
  begin
    // 利用代数余子式 Minor/Cofactor 计算高阶行列式
    Result := 0;
    Minor := TCnIntMatrix.Create(FRowCount - 1, FColCount - 1);
    try
      for I := 0 to FColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);
        Result := Result + FMatrix[0, I] * NegativeOnePower(I) * Minor.Determinant;
      end;
    finally
      Minor.Free;
    end;
  end;
end;

procedure TCnIntMatrix.Divide(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := FMatrix[I, J] div Factor;
end;

procedure TCnIntMatrix.DumpToStrings(List: TStrings; Sep: Char = ' ');
var
  I, J: Integer;
  S: string;
begin
  for I := 0 to FRowCount - 1 do
  begin
    S := '';
    for J := 0 to FColCount - 1 do
    begin
      if J = 0 then
        S := IntToStr(FMatrix[I, J])
      else
        S := S + Sep + IntToStr(FMatrix[I, J]);
    end;
    List.Add(S);
  end;
end;

function TCnIntMatrix.GetValue(Row, Col: Integer): Int64;
begin
  Result := FMatrix[Row, Col];
end;

function TCnIntMatrix.IsE: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if (I = J) and (FMatrix[I, J] <> 1) then
      begin
        Result := False;
        Exit;
      end
      else if (I <> J) and (FMatrix[I, J] <> 0) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCnIntMatrix.IsSingular: Boolean;
begin
  if not IsSquare then
    Result := False
  else
    Result := Determinant = 0;
end;

function TCnIntMatrix.IsSquare: Boolean;
begin
  Result := (FColCount = FRowCount);
end;

function TCnIntMatrix.IsSymmetrical: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
    for J := 0 to I do
      if FMatrix[I, J] <> FMatrix[J, I] then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

function TCnIntMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      if FMatrix[I, J] <> 0 then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

procedure TCnIntMatrix.Mul(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationMul(FMatrix[I, J], Factor);
end;

function TCnIntMatrix.OperationAdd(X, Y: Int64): Int64;
begin
  Result := X + Y;
end;

function TCnIntMatrix.OperationMul(X, Y: Int64): Int64;
begin
  Result := X * Y;
end;

procedure TCnIntMatrix.SetColCount(const Value: Integer);
begin
  if FColCount <> Value then
  begin
    CheckCount(Value);
    FColCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnIntMatrix.SetE(Size: Integer);
var
  I, J: Integer;
begin
  CheckCount(Size);

  RowCount := Size;
  ColCount := Size;
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        FMatrix[I, J] := 1
      else
        FMatrix[I, J] := 0;
end;

procedure TCnIntMatrix.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
  begin
    CheckCount(Value);
    FRowCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnIntMatrix.SetValue(Row, Col: Integer; const Value: Int64);
begin
  FMatrix[Row, Col] := Value;
end;

procedure TCnIntMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := 0;
end;

function TCnIntMatrix.Trace: Int64;
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create('Only Square Matrix can Trace.');

  Result := 0;
  for I := 0 to FRowCount - 1 do
    Result := OperationAdd(Result, FMatrix[I, I]);
end;

end.

