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

unit CnQuantum;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：量子基础库实现单元
* 单元作者：CnPack 开发组
* 备    注：
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.06.27 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

{
  一个量子比特，是两个基态的概率波的叠加。俩波各有振幅、相位等参数，
  似乎频率相等，或者说不用考虑频率。

  数学上，一个量子比特是一个二维复系数向量，Alpha 乘 0 态和 Beta 乘 1 态之和，
  且俩复系数是概率振幅，其绝对值平方和为 1，其绝对值平方即为基态的各自概率。
  表示法为上 Alpha 下 Beta 的列矩阵。

  本来 a 和 b 俩复数是有四维的，但又因为有 |a|^2+|b|^2 = 1 的约束条件，
  因而是在四维空间单位超球面上的点，可以再加上忽视全局相位的条件，非线性映射到三维的单位球面上

  因为|a|^2+|b|^2=1，所以可设 a 为一个幅度是某角度 cos 值 theta，自身角度任意的复数，
  b 为一个幅度是同一角度的 sin 值，自身角度任意的另一个复数。这个自身角度叫做全局相位
  这样取绝对值后，俩自身角度消失，只剩 cos 平方加 sin 平方，等于 1。

  但因为 a 和 b 有自身角度的存在，它们之间的角度差，就是相对相位 phi，范围在 0 到 2π。
  控制幅度的那个角 theta，范围在 0 到半 π。
  因此，只要两个角，就能确定 a 和 b，类似于一个方位角加一个仰角就能确定球面上的点。
  似乎还要加个前提：共同全局相位无需考虑，因而可设 a 为实数。

  俩角、仨坐标、俩复数（其中 a 的虚部为 0）的对应关系如何理解？

  a     1     0                                                                 (a)
  b = a 0 + b 1，要先证明这步矩阵运算，还好理解。说明这个量子比特的矩阵形式就是 (b)

  Bloch 球，Z 轴正方向最上点表示基态 0，最下点表示基态 1，XY轴前后左右四个点是根号 2 分之一正负排列。但不理解三根轴的几何意义。

  向量内积好理解，各项相乘之和，得到一个数字（标量）。
  矩阵克罗内克积（矩阵直积或张量积）不是矩阵乘法。是任意矩阵（包括向量）的各项乘法分配展开，也还好理解。

  一个矩阵如果乘以某个向量，结果等于某个常数乘以某个向量，那么这常数和这向量就称为该矩阵的特征向量和特征值。好理解，但不知道有啥用。

  双量子比特状态的纠缠难以理解。

  量子比特的矩阵变换，是变换矩阵在左，量子比特矩阵也就是列向量在右，结果是一个列向量
  矩阵一行对应元素乘列向量对应元素，得到结果的对应列元素

}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Math, CnMath, CnComplex;

type
  ECnQuantumException = class(Exception);
  {* 量子数学相关的异常}

  TCnQuBit = class(TPersistent)
  {* 单比特量子态实现类}
  private
    FAlpha: TCnComplexNumber;  // 复数 a
    FBeta: TCnComplexNumber;   // 复数 b
    FPhi: Extended;            // 相位差角，[0, 2π)
    FTheta: Extended;          // 辐角，[0, π]，实现中除以了 2 以符合习惯
    FX: Extended;              // Bloch 球面点的 X 坐标
    FY: Extended;              // Bloch 球面点的 Y 坐标
    FZ: Extended;              // Bloch 球面点的 Z 坐标
    procedure CalcCoordinateFromAngle;
    procedure CalcAngleFromCoordinate;
    procedure CalcAngleFromComplex;
    procedure CalcComplexFromAngle;
    procedure CalcComplexFromCoordinate;
    procedure CalcCoordinateFromComplex;
    procedure SetAlpha(const Value: TCnComplexNumber);
    procedure SetBeta(const Value: TCnComplexNumber);
    procedure SetFX(const Value: Extended);
    procedure SetFY(const Value: Extended);
    procedure SetFZ(const Value: Extended);
    procedure SetPhi(const Value: Extended);
    procedure SetTheta(const Value: Extended);
  protected
    procedure UpdateFromComplex;
    procedure UpdateFromAngle;
    procedure UpdateFromCoordinate;

    procedure CheckValid;
    function ValidComplex: Boolean;
    {* 判断概率和是否为 1}
    procedure ValidAngle;
    {* 将两个角度规整为合法范围内}
  public
    constructor Create(AR, AI, BR, BI: Extended); overload;
    {* 创建并设置其两个复向量值}
    constructor Create(APhi, ATheta: Extended); overload;
    {* 创建并设置其相位差角与辐角值}
    constructor Create(AX, AY, AZ: Extended); overload;
    {* 创建并设置其笛卡尔坐标系值}

    constructor CreateAsOne;
    {* 创建并设置为基态 1}
    constructor CreateAsZero;
    {* 创建并设置为基态 0}

    procedure Assign(Source: TPersistent); override;
    {* 复制量子比特}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 输出成字符串}

    function ZeroProbability: Extended;
    {* 观测获知其坍缩至基态 0 的概率}
    function OneProbability: Extended;
    {* 观测获知其坍缩至基态 1 的概率}

    property X: Extended read FX write SetFX;
    {* 该量子比特的在布洛赫球的笛卡尔坐标系中的 X 坐标}
    property Y: Extended read FY write SetFY;
    {* 该量子比特的在布洛赫球的笛卡尔坐标系中的 Y 坐标}
    property Z: Extended read FZ write SetFZ;
    {* 该量子比特的在布洛赫球的笛卡尔坐标系中的 Z 坐标}

    property Phi: Extended read FPhi write SetPhi;
    {* 相位差角，体现 a 和 b 复向量的夹角，范围在 [0, 2π)}
    property Theta: Extended read FTheta write SetTheta;
    {* 辐角，体现 a 和 b 复向量的绝对值涨落，范围在 [0, π]，实现中除以了 2 以符合习惯}

    property Alpha: TCnComplexNumber read FAlpha write SetAlpha;
    {* 该量子比特的第一个复向量}
    property Beta: TCnComplexNumber read FBeta write SetBeta;
    {* 该量子比特的第二个复向量}
  end;

procedure CnQuBitMulMatrix(InQ, OutQ: TCnQuBit; var M00, M01, M10, M11: TCnComplexNumber); overload;
{* 对量子比特进行二维方阵运算，参数是复数，InQ，OutQ 可以是同一个对象
  00 代表矩阵中的首行首列元素，01 代表首行次列，10 代表次行首列，11 代表次行次列}

procedure CnQuBitMulMatrix(InQ, OutQ: TCnQuBit; M00, M01, M10, M11: Extended); overload;
{* 对量子比特进行二维方阵运算，参数是实数，InQ，OutQ 可以是同一个对象
  00 代表矩阵中的首行首列元素，01 代表首行次列，10 代表次行首列，11 代表次行次列}

procedure CnQuBitHadamardGate(InQ, OutQ: TCnQuBit);
{* 量子比特 Hadamard 门变换}

procedure CnQuBitPauliXGate(InQ, OutQ: TCnQuBit);
{* 量子比特泡利 X 门变换，相当于 NOT，也即 |0> 和 |1> 的系数互换}

procedure CnQuBitPauliYGate(InQ, OutQ: TCnQuBit);
{* 量子比特泡利 Y 门变换}

procedure CnQuBitPauliZGate(InQ, OutQ: TCnQuBit);
{* 量子比特泡利 Z 门变换，保留基本状态 |0> 不变，并且将 |1> 的系数换成负的}

procedure CnQuBitPhaseShiftGate(InQ, OutQ: TCnQuBit; ATheta: Extended);
{* 量子比特相位偏移门变换，保留基本状态 |0> 并且将 |1> 的系数换成辐角为 ATheta 的单位模长复向量}

var
  CnQuBitBaseZero: TCnQuBit = nil;
  CnQuBitBaseOne: TCnQuBit = nil;

implementation

resourcestring
  SCN_ERROR_QUBIT_AB_RANGE = 'A B Modulus Summary NOT Valid';
  SCN_ERROR_QUBIT_ANGLE_RANGE = 'Phi or Theta Angle NOT Valid';

{ TCnQuBit }

procedure TCnQuBit.Assign(Source: TPersistent);
begin
  if Source is TCnQuBit then
  begin
    FX := (Source as TCnQuBit).X;
    FY := (Source as TCnQuBit).Y;
    FZ := (Source as TCnQuBit).Z;

    FPhi := (Source as TCnQuBit).Phi;
    FTheta := (Source as TCnQuBit).Theta;

    ComplexNumberCopy(FAlpha, (Source as TCnQuBit).FAlpha);
    ComplexNumberCopy(FBeta, (Source as TCnQuBit).FBeta);
  end
  else
    inherited;
end;

procedure TCnQuBit.CalcAngleFromComplex;
var
  A: Extended;
begin
  CheckValid;

  A := ComplexNumberAbsolute(FAlpha);
  // B := ComplexNumberAbsolute(FBeta);

  FTheta := ArcCos(A) * 2; // 因为做了 Valid 判断因而等同于 ArcSin(B) * 2

  FPhi := ComplexNumberArgument(FBeta) - ComplexNumberArgument(FAlpha);
  if FPhi < 0 then
    FPhi := FPhi + CN_PI * 2;
end;

procedure TCnQuBit.CalcAngleFromCoordinate;
begin
  FTheta := ArcCos(FZ);
  FPhi := ArcCos(FX / Sin(FTheta));
end;

procedure TCnQuBit.CalcComplexFromAngle;
begin
  FAlpha.R := Cos(FTheta / 2);
  FAlpha.I := 0;

  FBeta.R := Sin(FTheta / 2) * Cos(FPhi);
  FBeta.I := Sin(FTheta / 2) * Sin(FPhi);
end;

procedure TCnQuBit.CalcComplexFromCoordinate;
begin
  CalcAngleFromCoordinate;
  CalcComplexFromAngle;
end;

procedure TCnQuBit.CalcCoordinateFromAngle;
begin
  FX := Sin(FTheta) * Cos(FPhi);
  FY := Sin(FTheta) * Sin(FPhi);
  FZ := Cos(FTheta);
end;

procedure TCnQuBit.CalcCoordinateFromComplex;
begin
  CalcAngleFromComplex;
  CalcCoordinateFromAngle;
end;

constructor TCnQuBit.Create(AR, AI, BR, BI: Extended);
begin
  inherited Create;
  FAlpha.R := AR;
  FAlpha.I := AI;
  FBeta.R := BR;
  FBeta.I := BI;

  UpdateFromComplex; // 里面已经调用了 CheckValid
end;

procedure TCnQuBit.CheckValid;
begin
  if not ValidComplex then
    raise ECnQuantumException.Create(SCN_ERROR_QUBIT_AB_RANGE);
end;

constructor TCnQuBit.Create(AX, AY, AZ: Extended);
begin
  inherited Create;
  FX := AX;
  FY := AY;
  FZ := AZ;

  UpdateFromCoordinate;
  CheckValid;
end;

constructor TCnQuBit.Create(APhi, ATheta: Extended);
begin
  inherited Create;
  FPhi := APhi;
  FTheta := ATheta;

  UpdateFromAngle;
  CheckValid;
end;

procedure TCnQuBit.SetAlpha(const Value: TCnComplexNumber);
begin
  FAlpha := Value;
  UpdateFromComplex;
end;

procedure TCnQuBit.SetBeta(const Value: TCnComplexNumber);
begin
  FBeta := Value;
  UpdateFromComplex;
end;

procedure TCnQuBit.SetFX(const Value: Extended);
begin
  FX := Value;
  UpdateFromCoordinate;
end;

procedure TCnQuBit.SetFY(const Value: Extended);
begin
  FY := Value;
  UpdateFromCoordinate;
end;

procedure TCnQuBit.SetFZ(const Value: Extended);
begin
  FZ := Value;
  UpdateFromCoordinate;
end;

procedure TCnQuBit.SetPhi(const Value: Extended);
begin
  FPhi := Value;
  UpdateFromAngle;
end;

procedure TCnQuBit.SetTheta(const Value: Extended);
begin
  FTheta := Value;
  UpdateFromAngle;
end;

function TCnQuBit.ToString: string;
begin
  Result := ComplexNumberToString(FAlpha) + '|0> + ' + ComplexNumberToString(FBeta) + '|1>'
    + '   Theta: ' + FloatToStr(FTheta) + ' Phi: ' + FloatToStr(FPhi);
end;

procedure TCnQuBit.UpdateFromAngle;
begin
  ValidAngle;

  CalcComplexFromAngle;
  CalcCoordinateFromAngle;
end;

procedure TCnQuBit.UpdateFromComplex;
begin
  CalcCoordinateFromComplex;
  // CalcAngleFromComplex;
end;

procedure TCnQuBit.UpdateFromCoordinate;
begin
  CalcComplexFromCoordinate;
  // CalcAngleFromCoordinate;
end;

function TCnQuBit.ValidComplex: Boolean;
begin
  Result := FloatEqual(FAlpha.R * FAlpha.R + FAlpha.I * FAlpha.I +
    FBeta.R * FBeta.R + FBeta.I * FBeta.I, 1);
end;

procedure TCnQuBit.ValidAngle;
begin
  // 将 FPhi 相位差角限制于 [0, 2π)
  FPhi := NormalizeAngle(FPhi);

  // 将 FTheta 辐角限制于 [0, π]
  FTheta := NormalizeAngle(FTheta);

  if FTheta > CN_PI then
    raise ECnQuantumException.Create(SCN_ERROR_QUBIT_ANGLE_RANGE);
end;

procedure CnQuBitMulMatrix(InQ, OutQ: TCnQuBit; var M00, M01, M10, M11: TCnComplexNumber);
var
  T1, T2, T: TCnComplexNumber;
begin
  // OutQ.Alpha = M00 * InQ.Alpha + M01 * InQ.Beta
  // OutQ.Beta  = M10 * InQ.Alpha + M11 * InQ.Beta

  ComplexNumberMul(T1, InQ.FAlpha, M00);
  ComplexNumberMul(T2, InQ.FBeta,  M01);
  ComplexNumberAdd(T, T1, T2);

  ComplexNumberMul(T1, InQ.FAlpha, M10);
  ComplexNumberMul(T2, InQ.FBeta,  M11);

  ComplexNumberCopy(OutQ.FAlpha, T);
  ComplexNumberAdd(T, T1, T2);
  ComplexNumberCopy(OutQ.FBeta, T);

  OutQ.UpdateFromComplex;
end;

procedure CnQuBitMulMatrix(InQ, OutQ: TCnQuBit; M00, M01, M10, M11: Extended);
var
  C00, C01, C10, C11: TCnComplexNumber;
begin
  C00.R := M00;
  C01.R := M01;
  C10.R := M10;
  C11.R := M11;

  C00.I := 0;
  C01.I := 0;
  C10.I := 0;
  C11.I := 0;

  CnQuBitMulMatrix(InQ, OutQ, C00, C01, C10, C11);
end;

procedure CnQuBitHadamardGate(InQ, OutQ: TCnQuBit);
begin
  CnQuBitMulMatrix(InQ, OutQ, Sqrt(2)/2, Sqrt(2)/2, Sqrt(2)/2, -Sqrt(2)/2);
end;

procedure CnQuBitPauliXGate(InQ, OutQ: TCnQuBit);
begin
  CnQuBitMulMatrix(InQ, OutQ, 0, 1, 1, 0);
end;

procedure CnQuBitPauliYGate(InQ, OutQ: TCnQuBit);
begin
  CnQuBitMulMatrix(InQ, OutQ, CnComplexZero, CnComplexOneI, CnComplexNegOneI, CnComplexZero);
end;

procedure CnQuBitPauliZGate(InQ, OutQ: TCnQuBit);
begin
  CnQuBitMulMatrix(InQ, OutQ, 1, 0, 0, -1);
end;

procedure CnQuBitPhaseShiftGate(InQ, OutQ: TCnQuBit; ATheta: Extended);
var
  C: TCnComplexNumber;
begin
  ComplexNumberSetAbsoluteArgument(C, 1, ATheta);
  CnQuBitMulMatrix(InQ, OutQ, CnComplexOne, CnComplexZero, CnComplexZero, C);
end;

constructor TCnQuBit.CreateAsOne;
begin
  Create(0, 0, 1, 0);
end;

constructor TCnQuBit.CreateAsZero;
begin
  Create(1, 0, 0, 0);
end;

function TCnQuBit.OneProbability: Extended;
begin
  Result := ComplexNumberAbsolute(FBeta);
end;

function TCnQuBit.ZeroProbability: Extended;
begin
  Result := ComplexNumberAbsolute(FAlpha);
end;

initialization
  CnQuBitBaseZero := TCnQuBit.Create(1, 0, 0, 0);
  CnQuBitBaseOne := TCnQuBit.Create(0, 0, 1, 0);

finalization
  CnQuBitBaseZero.Free;
  CnQuBitBaseOne.Free;

end.
