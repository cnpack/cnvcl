{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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

unit Cn25519;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：25519 系列椭圆曲线算法单元
* 单元作者：刘啸
* 备    注：目前实现了 Montgomery 椭圆曲线 y^2 = x^3 + A*X^2 + x
*           以及扭曲 Edwards 椭圆曲线 au^2 + v^2 = 1 + d * u^2 * v^2 的点加减乘
*           暂未实现仅基于 X 以及蒙哥马利阶梯的快速计算
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.06.05 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNativeDecl, CnBigNumber, CnECC;

type
  TCnTwistedEdwardsCurve = class
  {* 有限域上的扭曲爱德华曲线 au^2 + v^2 = 1 + du^2v^2 (其中 u v 与蒙哥马利曲线的 x y 有映射关系)}
  private
    FCoefficientA: TCnBigNumber;
    FCoefficientD: TCnBigNumber;
    FOrder: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FGenerator: TCnEccPoint;
    FCoFactor: Integer;
  public
    constructor Create; overload; virtual;
    {* 普通构造函数，未初始化参数}
    constructor Create(const A, D, FieldPrime, GX, GY, Order: AnsiString; H: Integer = 1); overload;
    {* 构造函数，传入方程的 A, D 参数、有限域上界 p、G 点坐标、G 点的阶数，需要十六进制字符串}

    destructor Destroy; override;
    {* 析构函数}

    procedure Load(const A, D, FieldPrime, GX, GY, Order: AnsiString; H: Integer = 1); virtual;
    {* 加载曲线参数，注意字符串参数是十六进制格式}

    procedure MultiplePoint(K: Int64; Point: TCnEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P}
    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P，内部实现等同于 CnECC 中同名方法}

    function PointAddPoint(P, Q, Sum: TCnEccPoint): Boolean;
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同
      此处的加法的几何意义相当于单位圆上的与正 Y 轴的夹角角度相加法则，
      中性点(0, 1)，等同于 Weierstrass 曲线中的无穷远点}
    function PointSubPoint(P, Q, Diff: TCnEccPoint): Boolean;
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure PointInverse(P: TCnEccPoint);
    {* 计算 P 点的逆元 -P，值重新放入 P，也就是 X 值取负}
    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* 判断 P 点是否在本曲线上}

    function IsNeutualPoint(P: TCnEccPoint): Boolean;
    {* 判断点是否是中性点，也就是判断 X = 0 且 Y = 1，与 Weierstrass 的无限远点全 0 不同}
    procedure SetNeutualPoint(P: TCnEccPoint);
    {* 将点设为中性点，也就是 X := 0 且 Y := 1}

    property Generator: TCnEccPoint read FGenerator;
    {* 基点坐标 G}
    property CoefficientA: TCnBigNumber read FCoefficientA;
    {* 方程系数 A}
    property CoefficientD: TCnBigNumber read FCoefficientD;
    {* 方程系数 B}
    property FiniteFieldSize: TCnBigNumber read FFiniteFieldSize;
    {* 有限域的上界，素数 p}
    property Order: TCnBigNumber read FOrder;
    {* 基点的阶数 N，注意它只在 H 为 1 时才等于本曲线的总点数}
    property CoFactor: Integer read FCoFactor;
    {* 辅助因子 H，也就是总点数 = N * H，先用 Integer 表示}
  end;

  TCnMontgomeryCurve = class
  {* 有限域上的蒙哥马利曲线 By^2 = x^3 + Ax^2 + x，其中 B*(A^2 - 4) <> 0}
  private
    FCoefficientB: TCnBigNumber;
    FCoefficientA: TCnBigNumber;
    FOrder: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FGenerator: TCnEccPoint;
    FCoFactor: Integer;
  public
    constructor Create; overload; virtual;
    {* 普通构造函数，未初始化参数}
    constructor Create(const A, B, FieldPrime, GX, GY, Order: AnsiString; H: Integer = 1); overload;
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、G 点坐标、G 点的阶数，需要十六进制字符串}

    destructor Destroy; override;
    {* 析构函数}

    procedure Load(const A, B, FieldPrime, GX, GY, Order: AnsiString; H: Integer = 1); virtual;
    {* 加载曲线参数，注意字符串参数是十六进制格式}

    procedure MultiplePoint(K: Int64; Point: TCnEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P}
    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P，内部实现等同于 CnECC 中同名方法}

    function PointAddPoint(P, Q, Sum: TCnEccPoint): Boolean;
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同
      此处的加法的几何意义类似于 Weierstrass 椭圆曲线上的连线或切线交点再取负，同样存在无穷远点(0, 0)}
    function PointSubPoint(P, Q, Diff: TCnEccPoint): Boolean;
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure PointInverse(P: TCnEccPoint);
    {* 计算 P 点的逆元 -P，值重新放入 P，也就是 Y 值取负}
    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* 判断 P 点是否在本曲线上}

    property Generator: TCnEccPoint read FGenerator;
    {* 基点坐标 G}
    property CoefficientA: TCnBigNumber read FCoefficientA;
    {* 方程系数 A}
    property CoefficientB: TCnBigNumber read FCoefficientB;
    {* 方程系数 B}
    property FiniteFieldSize: TCnBigNumber read FFiniteFieldSize;
    {* 有限域的上界，素数 p}
    property Order: TCnBigNumber read FOrder;
    {* 基点的阶数 N，注意它只在 H 为 1 时才等于本曲线的总点数}
    property CoFactor: Integer read FCoFactor;
    {* 辅助因子 H，也就是总点数 = N * H，先用 Integer 表示}
  end;

// =============================================================================
//
//       Curve25519 的 x y 和 Ed25519 的 u v 的双向映射关系为：
//           (u, v) = ((1+y)/(1-y), sqrt(-486664)*u/x)
//           (x, y) = (sqrt(-486664)*u/v, (u-1)/(u+1))
//
// =============================================================================

  TCnCurve25519 = class(TCnMontgomeryCurve)
  {* rfc 7748/8032 中规定的 Curve25519 曲线}
  public
    constructor Create; override;
  end;

  TCnEd25519 = class(TCnTwistedEdwardsCurve)
  {* rfc 7748/8032 中规定的 Ed25519 曲线}
  public
    constructor Create; override;
  end;

implementation

const
  SCN_25519_PRIME = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFED';
  // 2^255 - 19

  SCN_25519_COFACTOR = 8;
  // 余因子均为 8，也就是椭圆曲线总点数是 G 点阶数的八倍

  SCN_25519_ORDER = '1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED';
  // 基点阶数均为 2^252 + 27742317777372353535851937790883648493

  // 25519 扭曲爱德华曲线参数
  SCN_25519_EDWARDS_A = '-01';
  // -1

  SCN_25519_EDWARDS_D = '52036CEE2B6FFE738CC740797779E89800700A4D4141D8AB75EB4DCA135978A3';
  // -121655/121656，也就是 121656 * D mod P = P - 121655 算得 D =
  // 37095705934669439343138083508754565189542113879843219016388785533085940283555

  SCN_25519_EDWARDS_GX = '216936D3CD6E53FEC0A4E231FDD6DC5C692CC7609525A7B2C9562D608F25D51A';
  // 15112221349535400772501151409588531511454012693041857206046113283949847762202

  SCN_25519_EDWARDS_GY = '6666666666666666666666666666666666666666666666666666666666666658';
  // 46316835694926478169428394003475163141307993866256225615783033603165251855960

  // 25519 蒙哥马利曲线参数
  SCN_25519_MONT_A = '076D06';
  // 486662

  SCN_25519_MONT_B = '01';
  // 1

  SCN_25519_MONT_GX = '09';
  // 9
  SCN_25519_MONT_GY = '20AE19A1B8A086B4E01EDD2C7748D14C923D4D7E6D7C61B229E9C5A27ECED3D9';
  // 4/5，也就是 5 * Y mod P = 4  y = 14781619447589544791020593568409986887264606134616475288964881837755586237401? 似乎不对
  // 可能是 46316835694926478169428394003475163141307993866256225615783033603165251855960 才对？

var
  F25519BigNumberPool: TCnBigNumberPool = nil;

{ TCnTwistedEdwardsCurve }

constructor TCnTwistedEdwardsCurve.Create(const A, D, FieldPrime, GX, GY,
  Order: AnsiString; H: Integer);
begin
  Create;
  Load(A, D, FieldPrime, GX, GY, Order, H);
end;

constructor TCnTwistedEdwardsCurve.Create;
begin
  inherited;
  FCoefficientA := TCnBigNumber.Create;
  FCoefficientD := TCnBigNumber.Create;
  FOrder := TCnBigNumber.Create;
  FFiniteFieldSize := TCnBigNumber.Create;
  FGenerator := TCnEccPoint.Create;
  FCoFactor := 1;
end;

destructor TCnTwistedEdwardsCurve.Destroy;
begin
  FGenerator.Free;
  FFiniteFieldSize.Free;
  FOrder.Free;
  FCoefficientD.Free;
  FCoefficientA.Free;
  inherited;
end;

function TCnTwistedEdwardsCurve.IsNeutualPoint(P: TCnEccPoint): Boolean;
begin
  Result := P.X.IsZero and P.Y.IsOne;
end;

function TCnTwistedEdwardsCurve.IsPointOnCurve(P: TCnEccPoint): Boolean;
var
  X, Y, L, R: TCnBigNumber;
begin
  // 判断 au^2 + v^2 是否等于 1 + du^2v^2，其中 U 用 X 代替，V 用 Y 代替
  Result := False;
  X := nil;
  Y := nil;
  L := nil;
  R := nil;

  try
    X := F25519BigNumberPool.Obtain;
    if BigNumberCopy(X, P.X) = nil then
      Exit;
    if not BigNumberDirectMulMod(X, X, X, FFiniteFieldSize) then
      Exit;

    Y := F25519BigNumberPool.Obtain;
    if BigNumberCopy(Y, P.Y) = nil then
      Exit;
    if not BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize) then
      Exit;

    L := F25519BigNumberPool.Obtain;
    if not BigNumberDirectMulMod(L, FCoefficientA, X, FFiniteFieldSize) then
      Exit;
    if not BigNumberAddMod(L, L, Y, FFiniteFieldSize) then
      Exit; // 此时 L := A * X^2 + Y^2

    R := F25519BigNumberPool.Obtain;
    if not BigNumberDirectMulMod(R, X, Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(R, FCoefficientD, R, FFiniteFieldSize) then
      Exit;
    R.AddWord(1); // 此时 R := 1 + D * X^2 * Y^2

    Result := BigNumberEqual(L, R);
  finally
    F25519BigNumberPool.Recycle(R);
    F25519BigNumberPool.Recycle(L);
    F25519BigNumberPool.Recycle(Y);
    F25519BigNumberPool.Recycle(X);
  end;
end;

procedure TCnTwistedEdwardsCurve.Load(const A, D, FieldPrime, GX, GY,
  Order: AnsiString; H: Integer);
begin
  FCoefficientA.SetHex(A);
  FCoefficientD.SetHex(D);
  FFiniteFieldSize.SetHex(FieldPrime);
  FGenerator.X.SetHex(GX);
  FGenerator.Y.SetHex(GY);
  FOrder.SetHex(Order);
  FCoFactor := H;
end;

procedure TCnTwistedEdwardsCurve.MultiplePoint(K: Int64;
  Point: TCnEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := F25519BigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MultiplePoint(BK, Point);
  finally
    F25519BigNumberPool.Recycle(BK);
  end;
end;

procedure TCnTwistedEdwardsCurve.MultiplePoint(K: TCnBigNumber;
  Point: TCnEccPoint);
var
  I: Integer;
  E, R: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    PointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    SetNeutualPoint(Point);
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEccPoint.Create;
    E := TCnEccPoint.Create;

    SetNeutualPoint(R); // R 被创建时默认为 (0, 0)，但此处必须为中性点 (0, 1)
    E.X := Point.X;
    E.Y := Point.Y;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        PointAddPoint(R, E, R);
      PointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
  finally
    E.Free;
    R.Free;
  end;
end;

function TCnTwistedEdwardsCurve.PointAddPoint(P, Q, Sum: TCnEccPoint): Boolean;
var
  X, Y, T, D1, D2, N1, N2: TCnBigNumber;
begin
//            x1 * y2 + x2 * y1                y1 * y2 - a * x1 * x2
//   x3 = --------------------------,   y3 = ---------------------------  并且无需考虑 P/Q 是否同一点
//         1 + d * x1 * x2 * y1 * y2          1 - d * x1 * x2 * y1 * y2

  Result := False;

  X := nil;
  Y := nil;
  T := nil;
  D1 := nil;
  D2 := nil;
  N1 := nil;
  N2 := nil;

  try
    X := F25519BigNumberPool.Obtain;
    Y := F25519BigNumberPool.Obtain;
    T := F25519BigNumberPool.Obtain;
    D1 := F25519BigNumberPool.Obtain;
    D2 := F25519BigNumberPool.Obtain;
    N1 := F25519BigNumberPool.Obtain;
    N2 := F25519BigNumberPool.Obtain;

    if not BigNumberDirectMulMod(T, P.X, Q.Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(N1, Q.X, P.Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberAddMod(N1, N1, T, FFiniteFieldSize) then // N1 得到 x1 * y2 + x2 * y1，释放 T
      Exit;

    if not BigNumberDirectMulMod(T, P.X, Q.X, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(T, T, FCoefficientA, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(N2, P.Y, Q.Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberSubMod(N2, N2, T, FFiniteFieldSize) then // N2 得到 y1 * y2 - a * x1 * x2，释放 T
      Exit;

    if not BigNumberDirectMulMod(T, P.Y, Q.Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(T, T, Q.X, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(T, T, P.X, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(T, T, FCoefficientD, FFiniteFieldSize) then // T 得到 d * x1 * x2 * y1 * y2
      Exit;

    if not BigNumberAddMod(D1, T, CnBigNumberOne, FFiniteFieldSize) then // D1 得到 1 + d * x1 * x2 * y1 * y2
      Exit;
    if not BigNumberSubMod(D2, CnBigNumberOne, T, FFiniteFieldSize) then // D2 得到 1 - d * x1 * x2 * y1 * y2
      Exit;

    if not BigNumberModularInverse(T, D1, FFiniteFieldSize) then  // T 得到 D1 逆元
      Exit;
    if not BigNumberDirectMulMod(X, N1, T, FFiniteFieldSize) then // 得到 Sum.X
      Exit;

    if not BigNumberModularInverse(T, D2, FFiniteFieldSize) then  // T 得到 D2 逆元
      Exit;
    if not BigNumberDirectMulMod(Y, N2, T, FFiniteFieldSize) then // 得到 Sum.Y
      Exit;

    if BigNumberCopy(Sum.X, X) = nil then
      Exit;
    if BigNumberCopy(Sum.Y, Y) = nil then
      Exit;

    Result := True;
  finally
    F25519BigNumberPool.Recycle(N2);
    F25519BigNumberPool.Recycle(N1);
    F25519BigNumberPool.Recycle(D2);
    F25519BigNumberPool.Recycle(D1);
    F25519BigNumberPool.Recycle(T);
    F25519BigNumberPool.Recycle(Y);
    F25519BigNumberPool.Recycle(X);
  end;
end;

procedure TCnTwistedEdwardsCurve.PointInverse(P: TCnEccPoint);
begin
  if BigNumberIsNegative(P.X) or (BigNumberCompare(P.X, FFiniteFieldSize) >= 0) then
    raise ECnEccException.Create('Inverse Error.');

  BigNumberSub(P.X, FFiniteFieldSize, P.X);
end;

function TCnTwistedEdwardsCurve.PointSubPoint(P, Q, Diff: TCnEccPoint): Boolean;
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    Result := PointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

procedure TCnTwistedEdwardsCurve.SetNeutualPoint(P: TCnEccPoint);
begin
  P.X.SetZero;
  P.Y.SetOne;
end;

{ TCnMontgomeryCurve }

constructor TCnMontgomeryCurve.Create(const A, B, FieldPrime, GX, GY,
  Order: AnsiString; H: Integer);
begin
  Create;
  Load(A, B, FieldPrime, GX, GY, Order, H);
end;

constructor TCnMontgomeryCurve.Create;
begin
  inherited;
  FCoefficientA := TCnBigNumber.Create;
  FCoefficientB := TCnBigNumber.Create;
  FOrder := TCnBigNumber.Create;
  FFiniteFieldSize := TCnBigNumber.Create;
  FGenerator := TCnEccPoint.Create;
  FCoFactor := 1;
end;

destructor TCnMontgomeryCurve.Destroy;
begin
  FGenerator.Free;
  FFiniteFieldSize.Free;
  FOrder.Free;
  FCoefficientB.Free;
  FCoefficientA.Free;
  inherited;
end;

function TCnMontgomeryCurve.IsPointOnCurve(P: TCnEccPoint): Boolean;
var
  X, Y, T: TCnBigNumber;
begin
  // 判断 B*y^2 是否等于 x^3 + A*x^2 + x mod P
  Result := False;
  X := nil;
  Y := nil;
  T := nil;

  try
    X := F25519BigNumberPool.Obtain;
    if BigNumberCopy(X, P.X) = nil then
      Exit;

    Y := F25519BigNumberPool.Obtain;
    if BigNumberCopy(Y, P.Y) = nil then
      Exit;

    if not BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(Y, FCoefficientB, Y, FFiniteFieldSize) then  // Y := B * y^2 mod P
      Exit;

    T := F25519BigNumberPool.Obtain;
    if not BigNumberDirectMulMod(T, FCoefficientA, X, FFiniteFieldSize) then
      Exit;  // T := A*X

    T.AddWord(1); // T := A*X + 1
    if not BigNumberDirectMulMod(T, X, T, FFiniteFieldSize) then
      Exit;       // T := X * (A*X + 1) = AX^2 + X

    if not BigNumberPowerWordMod(X, X, 3, FFiniteFieldSize) then  // X^3
      Exit;

    if not BigNumberAddMod(X, X, T, FFiniteFieldSize) then // X := x^3 + Ax^2 + x mod P
      Exit;

    Result := BigNumberEqual(X, Y);
  finally
    F25519BigNumberPool.Recycle(Y);
    F25519BigNumberPool.Recycle(X);
    F25519BigNumberPool.Recycle(T);
  end;
end;

procedure TCnMontgomeryCurve.Load(const A, B, FieldPrime, GX, GY,
  Order: AnsiString; H: Integer);
begin
  FCoefficientA.SetHex(A);
  FCoefficientB.SetHex(B);
  FFiniteFieldSize.SetHex(FieldPrime);
  FGenerator.X.SetHex(GX);
  FGenerator.Y.SetHex(GY);
  FOrder.SetHex(Order);
  FCoFactor := H;
end;

procedure TCnMontgomeryCurve.MultiplePoint(K: Int64; Point: TCnEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := F25519BigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MultiplePoint(BK, Point);
  finally
    F25519BigNumberPool.Recycle(BK);
  end;
end;

procedure TCnMontgomeryCurve.MultiplePoint(K: TCnBigNumber;
  Point: TCnEccPoint);
var
  I: Integer;
  E, R: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    PointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    Point.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEccPoint.Create;
    E := TCnEccPoint.Create;

    // R 被创建时默认为无穷远点
    E.X := Point.X;
    E.Y := Point.Y;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        PointAddPoint(R, E, R);
      PointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
  finally
    E.Free;
    R.Free;
  end;
end;

function TCnMontgomeryCurve.PointAddPoint(P, Q, Sum: TCnEccPoint): Boolean;
var
  K, X, Y, T, SX, SY: TCnBigNumber;
begin
  // 先计算斜率，当两点 X 不等或相等时，斜率分别为
  //          (y2 - y1)^2          (3*x1^2 + 2*A*x1 + 1)^2
  // 斜率 K = ------------  或 =  -------------------------
  //          (x2 - x1)^2                (2*y1)^2
  //
  // x3 = K^2 - A - x1 - x2
  // y3 = -(y1 + K * (x3 - x1))
  Result := True;
  K := nil;
  X := nil;
  Y := nil;
  T := nil;
  SX := nil;
  SY := nil;

  try
    if P.IsZero then
    begin
      Sum.Assign(Q);
      Exit;
    end
    else if Q.IsZero then
    begin
      Sum.Assign(P);
      Exit;
    end;

    K := F25519BigNumberPool.Obtain;
    X := F25519BigNumberPool.Obtain;
    Y := F25519BigNumberPool.Obtain;
    T := F25519BigNumberPool.Obtain;
    SX := F25519BigNumberPool.Obtain;
    SY := F25519BigNumberPool.Obtain;

    if (BigNumberCompare(P.X, Q.X) = 0) and (BigNumberCompare(P.Y, Q.Y) = 0) then
    begin
      if P.Y.IsZero then
      begin
        Sum.SetZero;
        Exit;
      end;

      // 同一个点，求切线斜率
      if not BigNumberSubMod(Y, Q.Y, P.Y, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize) then // 得到分子 (y2 - y1)^2
        Exit;
      if not BigNumberSubMod(X, Q.X, P.X, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(X, X, X, FFiniteFieldSize) then // 得到分母 (x2 - x1)^2
        Exit;
      if not BigNumberModularInverse(T, X, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(K, Y, T, FFiniteFieldSize) then // K 得到切线斜率
        Exit;
    end
    else
    begin
      if BigNumberCompare(P.X, Q.X) = 0 then // 如果 X 相等，要判断 Y 是不是互反，是则和为 0，不是则挂了
      begin
        BigNumberAdd(T, P.Y, Q.Y);
        if BigNumberCompare(T, FFiniteFieldSize) = 0 then  // 互反，和为 0
          Sum.SetZero
        else                                               // 不互反，挂了
          raise ECnEccException.CreateFmt('Can NOT Calucate %s,%s + %s,%s',
            [P.X.ToDec, P.Y.ToDec, Q.X.ToDec, Q.Y.ToDec]);

        Exit;
      end;

      // 计算连线斜率
      // 分子是 (3*x1^2 + 2*A*x1 + 1)^2
      if not BigNumberDirectMulMod(Y, FCoefficientA, P.X, FFiniteFieldSize) then
        Exit;
      if not BigNumberAddMod(Y, Y, Y, FFiniteFieldSize) then
        Exit;
      Y.AddWord(1); // Y 得到 2*A*x1 + 1

      if not BigNumberDirectMulMod(T, P.X, P.X, FFiniteFieldSize) then
        Exit;
      T.MulWord(3);
      if not BigNumberAddMod(Y, T, Y, FFiniteFieldSize) then // Y 得到 3*x1^2 + 2*A*x1 + 1，释放 T
        Exit;

      if not BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize) then // Y 再平方
        Exit;

      if not BigNumberAddMod(X, P.Y, P.Y, FFiniteFieldSize) then  // 2Y
        Exit;
      if not BigNumberDirectMulMod(X, X, X, FFiniteFieldSize) then // 4Y^2
        Exit;
      if not BigNumberModularInverse(T, X, FFiniteFieldSize) then // 得到分母 (2*y1)^2
        Exit;

      if not BigNumberDirectMulMod(K, Y, T, FFiniteFieldSize) then // K 得到割线斜率
        Exit;
    end;

    // x3 = K^2 - A - x1 - x2
    if not BigNumberDirectMulMod(SX, K, K, FFiniteFieldSize) then
      Exit;
    if not BigNumberSubMod(SX, SX, FCoefficientA, FFiniteFieldSize) then
      Exit;
    if not BigNumberSubMod(SX, SX, P.X, FFiniteFieldSize) then
      Exit;
    if not BigNumberSubMod(SX, SX, Q.X, FFiniteFieldSize) then
      Exit;

    // y3 = -(y1 + K * (x3 - x1))
    if not BigNumberSubMod(SY, SX, P.X, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(SY, SY, K, FFiniteFieldSize) then
      Exit;
    if not BigNumberAddMod(SY, SY, P.Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberSub(SY, FFiniteFieldSize, SY) then
      Exit;

    Result := True;
  finally
    F25519BigNumberPool.Recycle(SY);
    F25519BigNumberPool.Recycle(SX);
    F25519BigNumberPool.Recycle(T);
    F25519BigNumberPool.Recycle(Y);
    F25519BigNumberPool.Recycle(X);
    F25519BigNumberPool.Recycle(K);
  end;
end;

procedure TCnMontgomeryCurve.PointInverse(P: TCnEccPoint);
begin
  if BigNumberIsNegative(P.Y) or (BigNumberCompare(P.Y, FFiniteFieldSize) >= 0) then
    raise ECnEccException.Create('Inverse Error.');

  BigNumberSub(P.Y, FFiniteFieldSize, P.Y);
end;

function TCnMontgomeryCurve.PointSubPoint(P, Q, Diff: TCnEccPoint): Boolean;
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    Result := PointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

{ TCnCurve25519 }

constructor TCnCurve25519.Create;
begin
  inherited;
  Load(SCN_25519_MONT_A, SCN_25519_MONT_B, SCN_25519_PRIME, SCN_25519_MONT_GX,
    SCN_25519_MONT_GY, SCN_25519_ORDER, 8);
end;

{ TCnEd25519 }

constructor TCnEd25519.Create;
begin
  inherited;
  Load(SCN_25519_EDWARDS_A, SCN_25519_EDWARDS_D, SCN_25519_PRIME, SCN_25519_EDWARDS_GX,
    SCN_25519_EDWARDS_GY, SCN_25519_ORDER, 8);
end;

initialization
  F25519BigNumberPool := TCnBigNumberPool.Create;

finalization
  F25519BigNumberPool.Free;

end.
