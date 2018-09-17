{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnECC;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：椭圆曲线算法单元
* 单元作者：刘啸
* 备    注：目前只实现 Int64 范围内形如 y^2 = x^3 + Ax + B mod p 这类椭圆曲线的计算。
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2018.09.13 V1.2
*               初步实现大数椭圆曲线的加解密功能，支持 SM2 以及 Secp256k1 曲线
*           2018.09.10 V1.1
*               能够生成系数很小的椭圆曲线参数
*           2018.09.05 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, Windows, CnNativeDecl, CnPrimeNumber, CnBigNumber;

type
  ECnEccException = class(Exception);

  TCnInt64EccPoint = packed record
  {* Int64 范围内的椭圆曲线上的点描述结构}
    X: Int64;
    Y: Int64;
  end;

  TCnInt64PublicKey = TCnInt64EccPoint;
  {* Int64 范围内的椭圆曲线的公钥，G 点计算 k 次后的点坐标}

  TCnInt64PrivateKey = Int64;
  {* Int64 范围内的椭圆曲线的私钥，计算次数 k 次}

  TCnInt64Ecc = class
  {* 描述一有限域 p 也就是 0 到 p - 1 上的椭圆曲线 y^2 = x^3 + Ax + B mod p，参数均在 Int64 范围内}
  private
    FGenerator: TCnInt64EccPoint;
    FCoefficientA: Int64;
    FCoefficientB: Int64;
    FFiniteFieldSize: Int64;
    FOrder: Int64;
  protected

  public
    constructor Create(A, B, FieldPrime, GX, GY, Order: Int64);
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、G 点坐标、G 点的阶数}
    destructor Destroy; override;
    {* 析构函数}

    procedure MultiplePoint(K: Int64; var Point: TCnInt64EccPoint);
    {* 计算某点 P 的 k * P 值，值重新放入 P}
    procedure PointAddPoint(var P, Q, Sum: TCnInt64EccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同}
    procedure PointSubPoint(var P, Q, Diff: TCnInt64EccPoint);
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure PointInverse(var P: TCnInt64EccPoint);
    {* 计算 P 点的逆元 -P，值重新放入 P}
    function IsPointOnCurve(var P: TCnInt64EccPoint): Boolean;
    {* 判断 P 点是否在本曲线上}

    function PlainToPoint(Plain: Int64; var OutPoint: TCnInt64EccPoint): Boolean;
    {* 将要加密的明文数值包装成一个待加密的点}

    procedure GenerateKeys(out PrivateKey: TCnInt64PrivateKey; out PublicKey: TCnInt64PublicKey);
    {* 生成一对该椭圆曲线的公私钥，私钥是运算次数 k，公钥是基点 G 经过 k 次乘法后得到的点坐标 K}
    procedure Encrypt(var PlainPoint: TCnInt64EccPoint; PublicKey: TCnInt64PublicKey;
      var OutDataPoint1, OutDataPoint2: TCnInt64EccPoint; RandomKey: Int64 = 0);
    {* 公钥加密明文点 M，得到两个点的输出密文，内部包含了随机值 r，也就是 C1 = M + rK; C2 = r * G
      如果传入的 RandomKey 是 0，则内部随机生成}
    procedure Decrypt(var DataPoint1, DataPoint2: TCnInt64EccPoint;
      PrivateKey: TCnInt64PrivateKey; var OutPlainPoint: TCnInt64EccPoint);
    {* 私钥解密密文点，也就是计算 C1 - k * C2 就得到了原文点 M}

    property Generator: TCnInt64EccPoint read FGenerator;
    {* 基点坐标 G}
    property CoefficientA: Int64 read FCoefficientA;
    {* 方程系数 A}
    property CoefficientB: Int64 read FCoefficientB;
    {* 方程系数 B}
    property FiniteFieldSize: Int64 read FFiniteFieldSize;
    {* 有限域的上界，素数 p}
    property Order: Int64 read FOrder;
    {* 基点的阶数}
  end;

  TCnEccPoint = class(TPersistent)
  {* 椭圆曲线上的点描述类}
  private
    FY: TCnBigNumber;
    FX: TCnBigNumber;
    procedure SetX(const Value: TCnBigNumber);
    procedure SetY(const Value: TCnBigNumber);
  public
    constructor Create; overload;
    constructor Create(const XDec, YDec: AnsiString); overload;

    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function IsZero: Boolean;
    procedure SetZero;

    property X: TCnBigNumber read FX write SetX;
    property Y: TCnBigNumber read FY write SetY;
  end;

  TCnEccPublicKey = TCnEccPoint;
  {* 椭圆曲线的公钥，G 点计算 k 次后的点坐标}

  TCnEccPrivateKey = TCnBigNumber;
  {* 椭圆曲线的私钥，计算次数 k 次}

  TCnEccPredefinedCurveType = (ctCustomized, ctSM2, ctSecp224r1, ctSecp224k1, ctSecp256k1);
  {* 支持的椭圆曲线类型}

  TCnEccPrimeType = (pt4U3, pt8U5, pt8U1);
  {* 素数类型，mod 4 余 3、mod 8 余 5、mod 8 余 1，用于椭圆曲线方程中快速求 Y}

  TCnEcc = class
  {* 描述一有限域 p 也就是 0 到 p - 1 上的椭圆曲线 y^2 = x^3 + Ax + B mod p}
  private
    FCoefficientB: TCnBigNumber;
    FCoefficientA: TCnBigNumber;
    FOrder: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FGenerator: TCnEccPoint;
    FBigNumberPool: TObjectList;
    FSizeUFactor: TCnBigNumber;
    FSizePrimeType: TCnEccPrimeType;
    function ObtainBigNumberFromPool: TCnBigNumber;
    procedure RecycleBigNumberToPool(Num: TCnBigNumber);
    // 计算 X, Y 的第 K 的 Lucas 序列 mod p 的值
    procedure CalcLucasSequence(X, Y: TCnBigNumber; U, V, K, P: TCnBigNumber);
    function GetBitsCount: Integer;
  protected
    procedure CalcX3AddAXAddB(X: TCnBigNumber); // 计算 X^3 + A*X + B，结果放入 X
  public
    constructor Create; overload;
    constructor Create(Predefined: TCnEccPredefinedCurveType); overload;
    constructor Create(const A, B, FieldPrime, GX, GY, Order: AnsiString); overload;
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、G 点坐标、G 点的阶数，需要十六进制字符串}
    destructor Destroy; override;
    {* 析构函数}

    procedure Load(Predefined: TCnEccPredefinedCurveType); overload;
    procedure Load(const A, B, FieldPrime, GX, GY, Order: AnsiString); overload;
    {* 加载曲线参数}

    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
    {* 计算某点 P 的 k * P 值，值重新放入 P}
    procedure PointAddPoint(P, Q, Sum: TCnEccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同}
    procedure PointSubPoint(P, Q, Diff: TCnEccPoint);
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure PointInverse(P: TCnEccPoint);
    {* 计算 P 点的逆元 -P，值重新放入 P}
    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* 判断 P 点是否在本曲线上}

    function PlainToPoint(Plain: TCnBigNumber; OutPoint: TCnEccPoint): Boolean;
    {* 将要加密的明文数值包装成一个待加密的点，特慢，不推荐}
    function PointToPlain(Point: TCnEccPoint; OutPlain: TCnBigNumber): Boolean;
    {* 将解密出的明文点解开成一个明文数值}

    procedure GenerateKeys(PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey);
    {* 生成一对该椭圆曲线的公私钥，私钥是运算次数 k，公钥是基点 G 经过 k 次乘法后得到的点坐标 K}
    procedure Encrypt(PlainPoint: TCnEccPoint; PublicKey: TCnEccPublicKey;
      OutDataPoint1, OutDataPoint2: TCnEccPoint);
    {* 公钥加密明文点 M，得到两个点的输出密文，内部包含了随机值 r，也就是 C1 = M + rK; C2 = r * G}
    procedure Decrypt(DataPoint1, DataPoint2: TCnEccPoint;
      PrivateKey: TCnEccPrivateKey; OutPlainPoint: TCnEccPoint);
    {* 私钥解密密文点，也就是计算 C1 - k * C2 就得到了原文点 M}

    property Generator: TCnEccPoint read FGenerator;
    {* 基点坐标 G}
    property CoefficientA: TCnBigNumber read FCoefficientA;
    {* 方程系数 A}
    property CoefficientB: TCnBigNumber read FCoefficientB;
    {* 方程系数 B}
    property FiniteFieldSize: TCnBigNumber read FFiniteFieldSize;
    {* 有限域的上界，素数 p}
    property Order: TCnBigNumber read FOrder;
    {* 基点的阶数}
    property BitsCount: Integer read GetBitsCount;
    {* 该椭圆曲线的素数域位数}
  end;

function CnInt64EccPointToString(var P: TCnInt64EccPoint): string;
{* 将一个 TCnInt64EccPoint 点坐标转换为字符串}

function CnEccPointToString(const P: TCnEccPoint): string;
{* 将一个 TCnEccPoint 点坐标转换为字符串}

function CnInt64EccGenerateParams(var FiniteFieldSize, CoefficientA, CoefficientB,
  GX, GY, Order: Int64): Boolean;
{* 生成椭圆曲线 y^2 = x^3 + Ax + B mod p 的各个参数，难以完整实现，只能先生成系数很小的}

function CnInt64EccDiffieHellmanGenerateOutKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  out PublicKey: TCnInt64PublicKey): Boolean;
{* 根据自身选择的随机数 PrivateKey 生成 ECDH 密钥协商的输出公钥点
   其中 OutPublicKey = SelfPrivateKey * G}

function CnInt64EccDiffieHellmanComputeKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  var OtherPublicKey: TCnInt64PublicKey; var SharedSecretKey: TCnInt64PublicKey): Boolean;
{* 根据对方发送的 ECDH 密钥协商的输出公钥计算生成公认的密钥点
   其中 SecretKey = SelfPrivateKey * OtherPublicKey}

function CnEccPointsEqual(P1, P2: TCnEccPoint): Boolean;
{* 判断两个点是否相等}

function CnEccDiffieHellmanGenerateOutKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
{* 根据自身选择的随机数 PrivateKey 生成 ECDH 密钥协商的输出公钥点
   其中 PublicKey = SelfPrivateKey * G}

function CnEccDiffieHellmanComputeKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  OtherPublicKey: TCnEccPublicKey; SharedSecretKey: TCnEccPublicKey): Boolean;
{* 根据对方发送的 ECDH 密钥协商的输出公钥计算生成公认的密钥点，一般拿点的 X 坐标来做密钥
   其中 SecretKey = SelfPrivateKey * OtherPublicKey}

implementation

type
  TCnEccPredefinedHexParams = packed record
    P: AnsiString;
    A: AnsiString;
    B: AnsiString;
    GX: AnsiString;
    GY: AnsiString;
    N: AnsiString;
    H: AnsiString;
  end;

const
  ECC_PRE_DEFINED_PARAMS: array[TCnEccPredefinedCurveType] of TCnEccPredefinedHexParams = (
    (P: ''; A: ''; B: ''; GX: ''; GY: ''; N: ''; H: ''),
    ( // SM2
      P: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFF';
      A: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFC';
      B: '28E9FA9E9D9F5E344D5A9E4BCF6509A7F39789F515AB8F92DDBCBD414D940E93';
      GX: '32C4AE2C1F1981195F9904466A39C9948FE30BBFF2660BE1715A4589334C74C7';
      GY: 'BC3736A2F4F6779C59BDCEE36B692153D0A9877CC62A474002DF32E52139F0A0';
      N: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFF7203DF6B21C6052B53BBF40939D54123';
      H: '01'
    ),
    ( // ctSecp224r1
      P: '00ffffffffffffffffffffffffffffffff000000000000000000000001';
      A: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE';
      B: '00B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4';
      GX: 'B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21';
      GY: 'BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34';
      N: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D';
      H: '01'
    ),
    ( // ctSecp224k1
      P: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D';
      A: '00';
      B: '05';
      GX: 'A1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C';
      GY: '7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5';
      N: '010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7';
      H: '01'
    ),
    ( // secp256k1
      P: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F';
      A: '00';
      B: '07';
      GX: '79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798';
      GY: '483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8';
      N: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141';
      H: '01'
    )
  );

// 将一个 TCnInt64EccPoint 点坐标转换为字符串
function CnInt64EccPointToString(var P: TCnInt64EccPoint): string;
begin
  Result := Format('%d,%d', [P.X, P.Y]);
end;

// 将一个 TCnEccPoint 点坐标转换为字符串
function CnEccPointToString(const P: TCnEccPoint): string;
begin
  Result := Format('%s,%s', [P.X.ToDec, P.Y.ToDec]);
end;

// 判断两个点是否相等
function CnEccPointsEqual(P1, P2: TCnEccPoint): Boolean;
begin
  if P1 = P2 then
  begin
    Result := True;
    Exit;
  end;
  Result := (BigNumberCompare(P1.X, P2.X) = 0) and (BigNumberCompare(P1.Y, P2.Y) = 0);
end;

// 计算勒让德符号 ( A / P) 的值
function CalcInt64Legendre(A, P: Int64): Integer;
begin
  // 三种情况：P 能整除 A 时返回 0，不能整除时，如果 A 是完全平方数就返回 1，否则返回 -1
  if A mod P = 0 then
    Result := 0
  else if MontgomeryPowerMod(A, (P - 1) shr 1, P) = 1 then // 欧拉判别法
    Result := 1
  else
    Result := -1;
end;

// 生成椭圆曲线 y^2 = x^3 + Ax + B mod p 的各个参数，难以实现
function CnInt64EccGenerateParams(var FiniteFieldSize, CoefficientA, CoefficientB,
  GX, GY, Order: Int64): Boolean;
var
  I: Integer;
  N: Int64;
  P: TCnInt64EccPoint;
  Ecc64: TCnInt64Ecc;
begin
  // 步骤：随机选有限域素数 p，与随机的 a、b，用 SEA 算法计算该曲线的阶 N
  // 判断 N 是大素数或其一半或三分之一是大素数，然后这个大素数作为循环子群的阶 n
  // 再根据 n 寻找基点 G 的坐标。如果 n 就等于 N 这个大素数，则 G 随便选都行。

  repeat
    // FiniteFieldSize := CnGenerateUInt32Prime; // 先用小点儿的素数，但也不能太小
    Randomize;
    I := Trunc(Random * (High(CN_PRIME_NUMBERS_SQRT_UINT32) - 100)) + 100;
    FiniteFieldSize := CN_PRIME_NUMBERS_SQRT_UINT32[I];
    CoefficientA := Trunc(Random * 16);
    CoefficientB := Trunc(Random * 256);
    N := 1; // 0,0 天然就算

    // A、B 都比较小，这里不用担心溢出
    if (4 * CoefficientA * CoefficientA * CoefficientA - 27 * CoefficientB * CoefficientB)
      mod FiniteFieldSize = 0 then
      Continue;

    GX := 0;
    GY := 0;

    // 以下求该椭圆曲线的阶，不懂 SEA，原先只能用特慢的穷举法，后改用勒让德公式
    // N := 1 + P + 所有的勒让德((x^3+ax+b)/p)之和，其中 X 从 0 到 P - 1
    Inc(N, FiniteFieldSize);
    for I := 0 to FiniteFieldSize - 1 do
    begin
      // 这里得用 Int64 先转换一下，否则 I 的三次方超过 Integer 溢出了
      N := N + CalcInt64Legendre(Int64(I) * Int64(I) * Int64(I) + CoefficientA * I + CoefficientB, FiniteFieldSize);
    end;

    // 然后随机找一个 X 求 Y
    Ecc64 := TCnInt64Ecc.Create(CoefficientA, CoefficientB, FiniteFieldSize, 0, 0, FiniteFieldSize);
    repeat
      P.X := Trunc(Random * (FiniteFieldSize - 1)) + 1;
      for I := 0 to FiniteFieldSize - 1 do
      begin
        P.Y := I;
        if Ecc64.IsPointOnCurve(P) then
        begin
          GX := P.X;
          GY := P.Y;
          Break;
        end;
      end;
    until (GX > 0) and (GY > 0);
    Ecc64.Free;

//    以下代码用穷举法来验证 N 是否正确，目前小范围看起来基本上没错
//    N := 1;
//    Ecc64 := TCnInt64Ecc.Create(CoefficientA, CoefficientB, FiniteFieldSize, 0, 0, FiniteFieldSize);
//    for I := 0 to FiniteFieldSize - 1 do
//    begin
//      for J := 0 to FiniteFieldSize - 1 do
//      begin
//        P.X := I;
//        P.Y := J;
//        if Ecc64.IsPointOnCurve(P) then
//        begin
//          Inc(N);
//          if (GX = 0) or (GY = 0) then // 第一个满足的就当作基点
//          begin
//            GX := P.X;
//            GY := P.Y;
//          end;
//
//          if P.Y > 0 then
//          begin
//            P.Y := FiniteFieldSize - P.Y;
//            if Ecc64.IsPointOnCurve(P) then
//              Inc(N);
//          end;
//
//          // 这个 X 已经查完了，每个 X 不会有多于两个 Y。
//          Break;
//        end;
//      end;
//      // Break 到此，进行下一个 X 的循环
//    end;

    // N 为这条椭圆曲线的阶
  until CnInt64IsPrime(N);

  Order := N;
  Result := True;
end;

// 逐位确定法快速计算整数的平方根的整数部分，如果是负数则先取正
function FastSqrt64(N: Int64): Int64;
var
  T, B: Int64;
  Sft: Int64;
begin
  Result := 0;
  if N < 0 then N := -N;

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

// 支持 A、B 为负数的乘积取模，但 C 仍要求正数否则结果不靠谱
function Int64MultipleMod(A, B, C: Int64): Int64;
begin
  if (A > 0) and (B > 0) then
    Result := MultipleMod(A, B, C)
  else if (A < 0) and (B < 0) then
    Result := MultipleMod(-A, -B, C)
  else if (A > 0) and (B < 0) then
    Result := C - MultipleMod(A, -B, C)
  else if (A < 0) and (B > 0) then
    Result := C - MultipleMod(-A, B, C)
  else
    Result := 0;
end;

// 求 X 针对 M 的模反元素也就是模逆元 Y，满足 (X * Y) mod M = 1，范围为 Int64，也就是说支持 X 为负值
function Int64ModularInverse(X: Int64; Modulus: Int64): Int64;
var
  Neg: Boolean;
begin
  Neg := False;
  if X < 0 then
  begin
    X := -X;
    Neg := True;
  end;

  // 负数的模逆元，等于正数的模逆元的负值，负值还可以再加 Modulus
  Result := CnInt64ModularInverse(X, Modulus);
  if Neg and (Result > 0) then
    Result := -Result;

  if Result < 0 then
    Result := Result + Modulus;
end;

{ TCnInt64Ecc }

constructor TCnInt64Ecc.Create(A, B, FieldPrime, GX, GY, Order: Int64);
begin
  inherited Create;
  if not CnInt64IsPrime(FieldPrime) or not CnInt64IsPrime(Order) then
    raise ECnEccException.Create('Infinite Field and Order must be a Prime Number.');

  if not (GX >= 0) and (GX < FieldPrime) or
    not (GY >= 0) and (GY < FieldPrime) then
    raise ECnEccException.Create('Generator Point must be in Infinite Field.');

  FCoefficientA := A;
  FCoefficientB := B;
  FFiniteFieldSize := FieldPrime;
  FGenerator.X := GX;
  FGenerator.Y := GY;
  FOrder := Order;
end;

procedure TCnInt64Ecc.Decrypt(var DataPoint1, DataPoint2: TCnInt64EccPoint;
  PrivateKey: TCnInt64PrivateKey; var OutPlainPoint: TCnInt64EccPoint);
var
  P: TCnInt64EccPoint;
begin
  P := DataPoint2;
  MultiplePoint(PrivateKey, P);
  PointSubPoint(DataPoint1, P, OutPlainPoint);
end;

destructor TCnInt64Ecc.Destroy;
begin

  inherited;
end;

procedure TCnInt64Ecc.Encrypt(var PlainPoint: TCnInt64EccPoint;
  PublicKey: TCnInt64PublicKey; var OutDataPoint1,
  OutDataPoint2: TCnInt64EccPoint; RandomKey: Int64);
begin
  if RandomKey = 0 then
  begin
    Randomize;
    RandomKey := Trunc(Random * (FOrder - 1)) + 1; // 比 0 大但比基点阶小的随机数
  end;

  if RandomKey mod FOrder = 0 then
    raise ECnEccException.CreateFmt('Error RandomKey %d for Order.', [RandomKey]);

  // M + rK;
  OutDataPoint1 := PublicKey;
  MultiplePoint(RandomKey, OutDataPoint1);
  PointAddPoint(PlainPoint, OutDataPoint1, OutDataPoint1);

  // r * G
  OutDataPoint2 := FGenerator;
  MultiplePoint(RandomKey, OutDataPoint2);
end;

procedure TCnInt64Ecc.GenerateKeys(out PrivateKey: TCnInt64PrivateKey;
  out PublicKey: TCnInt64PublicKey);
begin
  Randomize;
  PrivateKey := Trunc(Random * (FOrder - 1)) + 1; // 比 0 大但比基点阶小的随机数
  PublicKey := FGenerator;
  MultiplePoint(PrivateKey, PublicKey);           // 基点乘 PrivateKey 次
end;

function TCnInt64Ecc.IsPointOnCurve(var P: TCnInt64EccPoint): Boolean;
var
  Y2, X3, AX, B: Int64;
begin
  // 计算 (Y^2 - X^3 - A*X - B) mod p 是否等于 0，应用分配律
  // 也就是计算(Y^2 mod p - X^3 mod p - A*X mod p - B mod p) mod p
  Y2 := MontgomeryPowerMod(P.Y, 2, FFiniteFieldSize);
  X3 := MontgomeryPowerMod(P.X, 3, FFiniteFieldSize);
  AX := Int64MultipleMod(FCoefficientA, P.X, FFiniteFieldSize);
  B := FCoefficientB mod FFiniteFieldSize;

  Result := ((Y2 - X3 - AX - B) mod FFiniteFieldSize) = 0;
end;

procedure TCnInt64Ecc.MultiplePoint(K: Int64; var Point: TCnInt64EccPoint);
var
  E, R: TCnInt64EccPoint;
begin
  if K < 0 then
  begin
    K := -K;
    PointInverse(Point);
  end;

  if K = 0 then
  begin
    Point.X := 0;
    Point.Y := 0;
    Exit;
  end;

  R.X := 0;
  R.Y := 0;
  E := Point;

  while K <> 0 do
  begin
    if (K and 1) <> 0 then
      PointAddPoint(R, E, R);

    PointAddPoint(E, E, E);
    K := K shr 1;
  end;

  Point := R;
end;

function TCnInt64Ecc.PlainToPoint(Plain: Int64;
  var OutPoint: TCnInt64EccPoint): Boolean;
var
  Y2, X3, AX, B, Y: Int64;
begin
  // 解方程求 Y： (y^2 - (Plain^3 + A * Plain + B)) mod p = 0
  // 注意 Plain 如果太大，计算过程中会溢出，不好处理，只能用分配律。
  // (Y^2 mod p - Plain ^ 3 mod p - A * Plain mod p - B mod p) mod p = 0;
  X3 := MontgomeryPowerMod(Plain, 3, FFiniteFieldSize);
  AX := Int64MultipleMod(FCoefficientA, Plain, FFiniteFieldSize);
  B := FCoefficientB mod FFiniteFieldSize;

  B := X3 + Ax + B; // 如果不溢出的话

  // 化为 Y^2 = N * p + B 要求找出 N 让右边为完全平方数，再求 Y 的正值
  // 只能 N 从 0 开始加 1 遍历并开方计算是否完全平方数

  Y2 := B;
  while True do
  begin
    if Y2 > 0 then
    begin
      Y := FastSqrt64(Y2);
      if Y * Y = Y2 then
      begin
        // Y2 是完全平方数
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
        Exit;
      end;
    end;
    Inc(Y2, FFiniteFieldSize);
  end;
  Result := False;
end;

procedure TCnInt64Ecc.PointAddPoint(var P, Q, Sum: TCnInt64EccPoint);
var
  K, X, Y, PX: Int64;
begin
  K := 0;
  if (P.X = 0) and (P.Y = 0) then
  begin
    Sum := Q;
    Exit;
  end
  else if (Q.X = 0) and (Q.Y = 0) then
  begin
    Sum := P;
    Exit;
  end
  else if (P.X = Q.X) and (P.Y = Q.Y) then
  begin
    // 俩加数是同一个点，切线斜率为两边求导，3 * X^2 + A / (2 * Y) 但如 Y = 0 则直接是无限远 0。
    X := 3 * P.X * P.X + FCoefficientA;
    Y := 2 * P.Y;

    if Y = 0 then
    begin
      Sum.X := 0;
      Sum.Y := 0;
    end;

    Y := Int64ModularInverse(Y, FFiniteFieldSize);
    K := Int64MultipleMod(X, Y, FFiniteFieldSize); // 得到斜率
  end
  else if (P.X = Q.X) and ((P.Y = -Q.Y) or (P.Y + Q.Y = FFiniteFieldSize)) then        // P = -Q
  begin
    Sum.X := 0;
    Sum.Y := 0;
    Exit;
  end
  else if P.X <> Q.X then
  begin
    // 斜率 K := ((Q.Y - P.Y) / (Q.X - P.X)) mod p
    Y := Q.Y - P.Y;
    X := Q.X - P.X;

    // Y/X = Y*X^-1 = Y * (X 针对 p 的逆元)
    X := Int64ModularInverse(X, FFiniteFieldSize);
    K := Int64MultipleMod(Y, X, FFiniteFieldSize); // 得到斜率
  end
  else if P.Y <> Q.Y then
  begin
    // P、Q 两点 X 相同，Y 不同但又不是逆元，该如何相加？理论上不会出现
    raise ECnEccException.CreateFmt('Can NOT Calucate %d,%d + %d,%d', [P.X, P.Y, Q.X, Q.Y]);
  end;

  // Xsum = (K^2 - X1 - X2) mod p
  X := K * K - P.X - Q.X;
  while X < 0 do
    X := X + FFiniteFieldSize;
  PX := P.X; // 如果 Sum 和 P 是同一个，要避免 P.X 被冲掉，因而得先存着 P.X
  if X < 0 then
  begin
    X := -X;
    Sum.X := X mod FFiniteFieldSize;
    Sum.X := FFiniteFieldSize - Sum.X;
  end
  else
    Sum.X := X mod FFiniteFieldSize;

  // Ysum = (K * (X1 - Xsum) - Y1) mod p  注意要取负
  //   也 = (K * (X2 - Xsum) - Y2) mod p  注意要取负
  X := PX - Sum.X;
  Y := K * X - P.Y;
  if Y < 0 then
  begin
    Y := -Y;
    Sum.Y := Y mod FFiniteFieldSize;
    Sum.Y := FFiniteFieldSize - Sum.Y;
  end
  else
    Sum.Y := Y mod FFiniteFieldSize;
end;

procedure TCnInt64Ecc.PointInverse(var P: TCnInt64EccPoint);
begin
  // P.Y := -P.Y mod p 注意这里的负值取模不等于 Delphi 的取正后取模再变负
  P.Y := FFiniteFieldSize - (P.Y mod FFiniteFieldSize);
end;

procedure TCnInt64Ecc.PointSubPoint(var P, Q, Diff: TCnInt64EccPoint);
var
  Inv: TCnInt64EccPoint;
begin
  Inv.X := Q.X;
  Inv.Y := Q.Y;
  PointInverse(Inv);
  PointAddPoint(P, Inv, Diff);
end;

// 根据自身选择的随机数 PrivateKey 生成 ECDH 密钥协商的输出公钥点
function CnInt64EccDiffieHellmanGenerateOutKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  out PublicKey: TCnInt64PublicKey): Boolean;
begin
  // OutPublicKey = SelfPrivateKey * G
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey > 0) then
  begin
    PublicKey := Ecc.Generator;
    Ecc.MultiplePoint(SelfPrivateKey, PublicKey);
    Result := True;
  end;
end;

// 根据对方发送的 ECDH 密钥协商的输出公钥计算生成公认的密钥点
function CnInt64EccDiffieHellmanComputeKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  var OtherPublicKey: TCnInt64PublicKey; var SharedSecretKey: TCnInt64PublicKey): Boolean;
begin
  // SecretKey = SelfPrivateKey * OtherPublicKey
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey > 0) then
  begin
    SharedSecretKey := OtherPublicKey;
    Ecc.MultiplePoint(SelfPrivateKey, SharedSecretKey);
    Result := True;
  end;
end;

{ TCnEccPoint }

procedure TCnEccPoint.Assign(Source: TPersistent);
begin
  if Source is TCnEccPoint then
  begin
    BigNumberCopy(FX, (Source as TCnEccPoint).X);
    BigNumberCopy(FY, (Source as TCnEccPoint).Y);
  end
  else
    inherited;
end;

constructor TCnEccPoint.Create;
begin
  inherited;
  FX := TCnBigNumber.Create;
  FY := TCnBigNumber.Create;
  FX.SetZero;
  FY.SetZero;
end;

constructor TCnEccPoint.Create(const XDec, YDec: AnsiString);
begin
  Create;
  FX.SetDec(XDec);
  FY.SetDec(YDec);
end;

destructor TCnEccPoint.Destroy;
begin
  FY.Free;
  FX.Free;
  inherited;
end;

function TCnEccPoint.IsZero: Boolean;
begin
  Result := FX.IsZero and FY.IsZero;
end;

procedure TCnEccPoint.SetX(const Value: TCnBigNumber);
begin
  BigNumberCopy(FX, Value);
end;

procedure TCnEccPoint.SetY(const Value: TCnBigNumber);
begin
  BigNumberCopy(FY, Value);
end;

procedure TCnEccPoint.SetZero;
begin
  FX.SetZero;
  FY.SetZero;
end;

{ TCnEcc }

// 计算 X, Y 的第 K 的 Lucas 序列 mod p 的值
procedure TCnEcc.CalcLucasSequence(X, Y, U, V, K, P: TCnBigNumber);
var
  I: Integer;
  D, T, U1, V1: TCnBigNumber;
begin
  D := nil;
  T := nil;
  U1 := nil;
  V1 := nil;

  if K.IsNegative or K.IsZero or P.IsNegative or P.IsZero then
    raise ECnEccException.Create('Invalid K or P for Lucas Sequance');

  try
    D := ObtainBigNumberFromPool;
    T := ObtainBigNumberFromPool;
    U1 := ObtainBigNumberFromPool;
    V1 := ObtainBigNumberFromPool;

    BigNumberMul(D, X, X);   // D: X^2
    BigNumberCopy(T, Y);
    BigNumberMulWord(T, 4);  // T: 4 * Y
    BigNumberSub(D, D, T);   // D = X^2 - 4 * Y

    U1.SetOne;
    BigNumberCopy(V1, X);

    if not BigNumberIsBitSet(K, K.GetBitsCount - 1) then
      raise ECnEccException.Create('Invalid K Bits for Lucas Sequance');

    for I := K.GetBitsCount - 2 downto 0 do
    begin
      // 用本轮的初始值 U1、V1 计算下一轮的 U、V，并赋值回 U1、V1
      BigNumberMulMod(U, U1, V1, P);   // U = (U*V) mod p 计算 U 时不能改变 U1 因为 计算 V 时还要用到

      BigNumberMul(V1, V1, V1);        // V = ((V^2 +D*U^2)/2) mod p  // 计算 V 时随便改变 U1、V1
      BigNumberMul(U1, U1, U1);
      BigNumberMul(U1, U1, D);
      BigNumberAdd(V1, U1, V1);
      BigNumberShiftRight(V1, V1, 1);
      BigNumberMod(V, V1, P);

      BigNumberCopy(U1, U);
      BigNumberCopy(V1, V);
      if BigNumberIsBitSet(K, I) then
      begin
        // 用 U1、V1 算 U、V，并赋值回 U1、V1
        BigNumberMul(U, U1, X);   // U = ((X * U +V)/2) mod p
        BigNumberAdd(U, U, V1);
        BigNumberShiftRight(U, U, 1);
        BigNumberMod(U, U, P);    // 计算 U 时不能改变 U1 因为 计算 V 时还要用到

        BigNumberMul(V1, V1, X);        // V = ((X*V + D*U)/2) mod p
        BigNumberMul(U1, U1, D);
        BigNumberAdd(V1, U1, V1);
        BigNumberShiftRight(V1, V1, 1); // 计算 V 时随便改变 U1、V1
        BigNumberMod(V, V1, P);

        BigNumberCopy(U1, U);
        BigNumberCopy(V1, V);
      end;
    end;
  finally
    RecycleBigNumberToPool(D);
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(U1);
    RecycleBigNumberToPool(V1);
  end;
end;

procedure TCnEcc.CalcX3AddAXAddB(X: TCnBigNumber);
var
  M: TCnBigNumber;
begin
  M := ObtainBigNumberFromPool;
  try
    BigNumberCopy(M, X);
    BigNumberMul(X, X, X);
    BigNumberMul(X, X, M); // X: X^3

    BigNumberMul(M, M, FCoefficientA); // M: A*X
    BigNumberAdd(X, X, M);             // X: X^3 + A*X
    BigNumberAdd(X, X, FCoefficientB); // X: X^3 + A*X + B
  finally
    RecycleBigNumberToPool(M);
  end;
end;

constructor TCnEcc.Create(const A, B, FieldPrime, GX, GY, Order: AnsiString);
begin
  Create;
  Load(A, B, FIeldPrime, GX, GY, Order);
end;

constructor TCnEcc.Create;
begin
  inherited;
  FGenerator := TCnEccPoint.Create;
  FCoefficientB := TCnBigNumber.Create;
  FCoefficientA := TCnBigNumber.Create;
  FOrder := TCnBigNumber.Create;
  FFiniteFieldSize := TCnBigNumber.Create;
  FSizeUFactor := TCnBigNumber.Create;
end;

constructor TCnEcc.Create(Predefined: TCnEccPredefinedCurveType);
begin
  Create;
  Load(Predefined);
end;

procedure TCnEcc.Decrypt(DataPoint1, DataPoint2: TCnEccPoint;
  PrivateKey: TCnEccPrivateKey; OutPlainPoint: TCnEccPoint);
var
  P: TCnEccPoint;
begin
  if (BigNumberCompare(PrivateKey, CnBigNumberZero) <= 0) or
    not IsPointOnCurve(DataPoint1) or not IsPointOnCurve(DataPoint2) then
    raise ECnEccException.Create('Invalid Private Key or Data.');

  P := TCnEccPoint.Create;
  try
    P.Assign(DataPoint2);
    MultiplePoint(PrivateKey, P);
    PointSubPoint(DataPoint1, P, OutPlainPoint);
  finally
    P.Free;
  end;
end;

destructor TCnEcc.Destroy;
var
  I: Integer;
begin
  if FBigNumberPool <> nil then
  begin
    for I := 0 to FBigNumberPool.Count - 1 do
      FBigNumberPool[I].Free;
    FBigNumberPool.Free;
  end;

  FSizeUFactor.Free;
  FGenerator.Free;
  FCoefficientB.Free;
  FCoefficientA.Free;
  FOrder.Free;
  FFiniteFieldSize.Free;
  inherited;
end;

procedure TCnEcc.Encrypt(PlainPoint: TCnEccPoint;
  PublicKey: TCnEccPublicKey; OutDataPoint1, OutDataPoint2: TCnEccPoint);
var
  RandomKey: TCnBigNumber;
begin
  if not IsPointOnCurve(PublicKey) or not IsPointOnCurve(PlainPoint) then
    raise ECnEccException.Create('Invalid Public Key or Data.');

  RandomKey := ObtainBigNumberFromPool;
  try
    BigNumberRandRange(RandomKey, FOrder);    // 比 0 大但比基点阶小的随机数
    if BigNumberIsZero(RandomKey) then
      BigNumberSetOne(RandomKey);

    // M + rK;
    OutDataPoint1.Assign(PublicKey);
    MultiplePoint(RandomKey, OutDataPoint1);
    PointAddPoint(PlainPoint, OutDataPoint1, OutDataPoint1);

    // r * G
    OutDataPoint2.Assign(FGenerator);
    MultiplePoint(RandomKey, OutDataPoint2);
  finally
    RecycleBigNumberToPool(RandomKey);
  end;
end;

procedure TCnEcc.GenerateKeys(PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey);
begin
  BigNumberRandRange(PrivateKey, FOrder);           // 比 0 大但比基点阶小的随机数
  if PrivateKey.IsZero then                         // 万一真拿到 0，就加 1
    PrivateKey.SetOne;

  PublicKey.Assign(FGenerator);
  MultiplePoint(PrivateKey, PublicKey);             // 基点乘 PrivateKey 次
end;

function TCnEcc.GetBitsCount: Integer;
begin
  Result := FFiniteFieldSize.GetBitsCount;
end;

function TCnEcc.IsPointOnCurve(P: TCnEccPoint): Boolean;
var
  X, Y, A: TCnBigNumber;
begin
  X := ObtainBigNumberFromPool;
  Y := ObtainBigNumberFromPool;
  A := ObtainBigNumberFromPool;

  try
    BigNumberCopy(X, P.X);
    BigNumberCopy(Y, P.Y);

    BigNumberMul(Y, Y, Y);                // Y: Y^2
    BigNumberMod(Y, Y, FFiniteFieldSize); // Y^2 mod P

    CalcX3AddAXAddB(X);                   // X: X^3 + A*X + B
    BigNumberMod(X, X, FFiniteFieldSize); // X: (X^3 + A*X + B) mod P
    Result := BigNumberCompare(X, Y) = 0;
  finally
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(A);
  end;
end;

procedure TCnEcc.Load(Predefined: TCnEccPredefinedCurveType);
begin
  Load(ECC_PRE_DEFINED_PARAMS[Predefined].A, ECC_PRE_DEFINED_PARAMS[Predefined].B,
    ECC_PRE_DEFINED_PARAMS[Predefined].P, ECC_PRE_DEFINED_PARAMS[Predefined].GX,
    ECC_PRE_DEFINED_PARAMS[Predefined].GY, ECC_PRE_DEFINED_PARAMS[Predefined].N);
end;

procedure TCnEcc.Load(const A, B, FieldPrime, GX, GY, Order: AnsiString);
var
  R: DWORD;
begin
  FGenerator.X.SetHex(GX);
  FGenerator.Y.SetHex(GY);
  FCoefficientA.SetHex(A);
  FCoefficientB.SetHex(B);
  FFiniteFieldSize.SetHex(FieldPrime);
  FOrder.SetHex(Order);

  // TODO: 要确保 4*a^3+27*b^2 <> 0
//  if not BigNumberIsProbablyPrime(FFiniteFieldSize) then
//    raise ECnEccException.Create('Error: Finite Field Size must be Prime.');

  // 确定 PrimeType
  R := BigNumberModWord(FFiniteFieldSize, 4);
  BigNumberCopy(FSizeUFactor, FFiniteFieldSize);
  if R = 3 then  // RFC 5639 要求 p 满足 4u + 3 的形式以便方便地计算 Y，但其他曲线未必
  begin
    FSizePrimeType := pt4U3;
    BigNumberDivWord(FSizeUFactor, 4);
  end
  else
  begin
    R := BigNumberModWord(FFiniteFieldSize, 8);
    if R = 1 then
    begin
      FSizePrimeType := pt8U1;
      BigNumberDivWord(FSizeUFactor, 8);
    end
    else if R = 5 then
    begin
      FSizePrimeType := pt8U5;
      BigNumberDivWord(FSizeUFactor, 8);
    end
    else
      raise ECnEccException.Create('Invalid Finite Field Size.');
  end;
end;

procedure TCnEcc.MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
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
    Point.X.SetZero;
    Point.Y.SetZero;
    Exit;
  end;

  R := nil;
  E := nil;

  try
    R := TCnEccPoint.Create;
    E := TCnEccPoint.Create;
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
    R.Free;
    E.Free;
  end;
end;

function TCnEcc.ObtainBigNumberFromPool: TCnBigNumber;
begin
  if FBigNumberPool = nil then
    Result := TCnBigNumber.Create
  else if FBigNumberPool.Count = 0 then
    Result := TCnBigNumber.Create
  else
  begin
    Result := TCnBigNumber(FBigNumberPool.Last);
    Result.Clear;
    FBigNumberPool.Delete(FBigNumberPool.Count - 1);
  end;
end;

function TCnEcc.PlainToPoint(Plain: TCnBigNumber;
  OutPoint: TCnEccPoint): Boolean;
var
  X, Y, Z, U, R, T, LU, LV, X3: TCnBigNumber;
begin
  Result := False;
  if Plain.IsNegative then
    Exit;

  if BigNumberCompare(Plain, FFiniteFieldSize) >= 0 then
    Exit;

  X := nil;
  U := nil;
  Y := nil;
  Z := nil;
  R := nil;
  T := nil;
  LU := nil;
  LV := nil;
  X3 := nil;

  try
    X := ObtainBigNumberFromPool;
    Y := ObtainBigNumberFromPool;
    Z := ObtainBigNumberFromPool;
    U := ObtainBigNumberFromPool;
    X3 := ObtainBigNumberFromPool;

    BigNumberCopy(X, Plain);
    BigNumberCopy(U, FSizeUFactor);

    CalcX3AddAXAddB(X);
    BigNumberMod(X, X, FFiniteFieldSize);
    BigNumberCopy(X3, X);    // 保存原始 g

    // 参考自《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节，这里 g 是 X 经过运算后的方程右半部分值
    case FSizePrimeType of
      pt4U3:
        begin
          // 结果是 g^(u+1) mod p
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Y, X, U, FFiniteFieldSize);
          BigNumberMulMod(Z, Y, Y, FFiniteFieldSize);
          if BigNumberCompare(Z, X) = 0 then
          begin
            BigNumberCopy(OutPoint.X, Plain);
            BigNumberCopy(OutPoint.Y, Y);
            Result := True;
            Exit;
          end;
        end;
      pt8U5:
        begin
          BigNumberMulWord(U, 2);
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Z, X, U, FFiniteFieldSize);
          R := ObtainBigNumberFromPool;
          BigNumberMod(R, Z, FFiniteFieldSize);

          if R.IsOne then
          begin
            // 结果是 g^(u+1) mod p
            BigNumberCopy(U, FSizeUFactor);
            BigNumberAddWord(U, 1);
            BigNumberMontgomeryPowerMod(Y, X, U, FFiniteFieldSize);

            BigNumberCopy(OutPoint.X, Plain);
            BigNumberCopy(OutPoint.Y, Y);
            Result := True;
          end
          else
          begin
            if R.IsNegative then
              BigNumberAdd(R, R, FFiniteFieldSize);
            BigNumberSub(R, FFiniteFieldSize, R);
            if R.IsOne then
            begin
              // 结果是(2g ·(4g)^u) mod p = (2g mod p * (4g)^u mod p) mod p
              BigNumberCopy(X, X3);
              BigNumberMulWord(X, 2);
              BigNumberMod(R, X, FFiniteFieldSize);  // R: 2g mod p

              BigNumberCopy(X, X3);
              BigNumberMulWord(X, 4);
              T := ObtainBigNumberFromPool;
              BigNumberMontgomeryPowerMod(T, X, FSizeUFactor, FFiniteFieldSize); // T: (4g)^u mod p
              BigNumberMulMod(Y, R, T, FFiniteFieldSize);

              BigNumberCopy(OutPoint.X, Plain);
              BigNumberCopy(OutPoint.Y, Y);
              Result := True;
            end;
          end;
        end;
      pt8U1: // Lucas 序列计算法
        begin
          LU := ObtainBigNumberFromPool;
          LV := ObtainBigNumberFromPool;
          T := ObtainBigNumberFromPool;

          BigNumberMulWord(U, 4);
          BigNumberAddWord(U, 1);

          while True do
          begin
            BigNumberCopy(Y, X3);
            BigNumberRandRange(X, FFiniteFieldSize);
            CalcLucasSequence(X, Y, LU, LV, U, FFiniteFieldSize);

            // V^2 mod p = 4Y mod p?
            BigNumberCopy(T, LV);
            BigNumberMul(T, T, T);
            BigNumberMod(T, T, FFiniteFieldSize); // T: V^2 mod p

            BigNumberMulWord(Y, 4);
            BigNumberMod(Y, Y, FFiniteFieldSize); // Y: 4Y mod p
            if BigNumberCompare(Y, T) = 0 then
            begin
              BigNumberShiftRight(Y, Y, 1);       // Y: Y/2
              BigNumberMod(OutPoint.Y, Y, FFiniteFieldSize);
              BigNumberCopy(OutPoint.X, Plain);
              Result := True;
              Exit;
            end;

            BigNumberMod(T, LU, FFiniteFieldSize);
            // U modp <> 1 且 U mod p <> p-1 则无解
            if not T.IsOne then
            begin
              BigNumberAddWord(T, 1);
              if not BigNumberCompare(T, FFiniteFieldSize) = 0 then
                Exit;
            end;
          end;
        end;
    end;
  finally
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(Z);
    RecycleBigNumberToPool(U);
    RecycleBigNumberToPool(R);
    RecycleBigNumberToPool(T);
    RecycleBigNumberToPool(LU);
    RecycleBigNumberToPool(LV);
    RecycleBigNumberToPool(X3);
  end;
end;

procedure TCnEcc.PointAddPoint(P, Q, Sum: TCnEccPoint);
var
  K, X, Y, A, SX, SY: TCnBigNumber;
begin
  K := nil;
  X := nil;
  Y := nil;
  A := nil;
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
    end
    else if (BigNumberCompare(P.X, Q.X) = 0) and (BigNumberCompare(P.Y, Q.Y) = 0) then
    begin
      // 俩加数是同一个点，切线斜率为两边求导，3 * X^2 + A / (2 * Y) 但如 Y = 0 则直接是无限远 0。
      if P.Y.IsZero then
      begin
        Sum.SetZero;
        Exit;
      end;

      X := ObtainBigNumberFromPool;
      Y := ObtainBigNumberFromPool;
      K := ObtainBigNumberFromPool;

      // X := 3 * P.X * P.X + CoefficientA;
      BigNumberMul(X, P.X, P.X);             // X: P.X^2
      BigNumberMulWord(X, 3);                // X: 3 * P.X^2
      BigNumberAdd(X, X, FCoefficientA);     // X: 3 * P.X^2 + A

      // Y := 2 * P.Y;
      BigNumberCopy(Y, P.Y);
      BigNumberMulWord(Y, 2);                // Y: 2 * P.Y

      A := ObtainBigNumberFromPool;
      BigNumberCopy(A, Y);
      BigNumberModularInverse(Y, A, FFiniteFieldSize);

      // K := X * Y mod FFiniteFieldSize;
      BigNumberMulMod(K, X, Y, FFiniteFieldSize);      // 得到斜率
    end
    else // 是不同点
    begin
      if BigNumberCompare(P.X, Q.X) = 0 then // 如果 X 相等，要判断 Y 是不是互反，是则和为 0，不是则挂了
      begin
        A := ObtainBigNumberFromPool;
        BigNumberAdd(A, P.Y, Q.Y);
        if BigNumberCompare(A, FFiniteFieldSize) = 0 then  // 互反，和为 0
          Sum.SetZero
        else                                               // 不互反，挂了
          raise ECnEccException.CreateFmt('Can NOT Calucate %s,%s + %s,%s', [P.X.ToDec, P.Y.ToDec, Q.X.ToDec, Q.Y.ToDec]);

        Exit;
      end;

      // 到这里，X 确定不同，斜率 K := ((Q.Y - P.Y) / (Q.X - P.X)) mod p
      X := ObtainBigNumberFromPool;
      Y := ObtainBigNumberFromPool;
      K := ObtainBigNumberFromPool;

      // Y := Q.Y - P.Y;
      // X := Q.X - P.X;
      BigNumberSub(Y, Q.Y, P.Y);
      BigNumberSub(X, Q.X, P.X);

      A := ObtainBigNumberFromPool;
      BigNumberCopy(A, X);
      BigNumberModularInverse(X, A, FFiniteFieldSize);
      BigNumberMulMod(K, Y, X, FFiniteFieldSize);      // 得到斜率
    end;

    BigNumberCopy(X, K);
    BigNumberMul(X, X, K);
    BigNumberSub(X, X, P.X);
    BigNumberSub(X, X, Q.X);    //  X := K * K - P.X - Q.X;

    SX := ObtainBigNumberFromPool;
    if BigNumberIsNegative(X) then // 负值的模等于正值的模被模数减
    begin
      BigNumberSetNegative(X, False);
      BigNumberMod(SX, X, FFiniteFieldSize);
      BigNumberSub(SX, FFiniteFieldSize, SX);
    end
    else
      BigNumberMod(SX, X, FFiniteFieldSize);

    // Ysum = (K * (X1 - Xsum) - Y1) mod p  注意要取负
    //   也 = (K * (X2 - Xsum) - Y2) mod p  注意要取负
    BigNumberSub(X, P.X, SX);
    BigNumberMul(Y, K, X);
    BigNumberSub(Y, Y, P.Y);

    SY := ObtainBigNumberFromPool;
    if BigNumberIsNegative(Y) then
    begin
      BigNumberSetNegative(Y, False);
      BigNumberMod(SY, Y, FFiniteFieldSize);
      BigNumberSub(SY, FFiniteFieldSize, SY);
    end
    else
      BigNumberMod(SY, Y, FFiniteFieldSize);

    BigNumberCopy(Sum.X, SX);
    BigNumberCopy(Sum.Y, SY);
  finally
    RecycleBigNumberToPool(K);
    RecycleBigNumberToPool(X);
    RecycleBigNumberToPool(Y);
    RecycleBigNumberToPool(A);
    RecycleBigNumberToPool(SX);
    RecycleBigNumberToPool(SY);
  end;
end;

procedure TCnEcc.PointInverse(P: TCnEccPoint);
begin
  if BigNumberIsNegative(P.Y) or (BigNumberCompare(P.Y, FFiniteFieldSize) >= 0) then
    raise ECnEccException.Create('Inverse Error.');

  BigNumberSub(P.Y, FFiniteFieldSize, P.Y);
end;

procedure TCnEcc.PointSubPoint(P, Q, Diff: TCnEccPoint);
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  Inv.X := Q.X;
  Inv.Y := Q.Y;
  PointInverse(Inv);
  PointAddPoint(P, Inv, Diff);
  Inv.Free;
end;

function TCnEcc.PointToPlain(Point: TCnEccPoint;
  OutPlain: TCnBigNumber): Boolean;
begin
  Result := False;
  if (Point <> nil) and (OutPlain <> nil) and IsPointOnCurve(Point) then
  begin
    BigNumberCopy(OutPlain, Point.X);
    Result := True;
  end;
end;

procedure TCnEcc.RecycleBigNumberToPool(Num: TCnBigNumber);
begin
  if Num = nil then
    Exit;

  if FBigNumberPool = nil then
    FBigNumberPool := TObjectList.Create(False);
  FBigNumberPool.Add(Num);
end;

function CnEccDiffieHellmanGenerateOutKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
begin
  // PublicKey = SelfPrivateKey * G
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey <> nil) and not BigNumberIsNegative(SelfPrivateKey) then
  begin
    PublicKey.Assign(Ecc.Generator);
    Ecc.MultiplePoint(SelfPrivateKey, PublicKey);
    Result := True;
  end;
end;

function CnEccDiffieHellmanComputeKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  OtherPublicKey: TCnEccPublicKey; SharedSecretKey: TCnEccPublicKey): Boolean;
begin
  // SecretKey = SelfPrivateKey * OtherPublicKey
  Result := False;
  if (Ecc <> nil) and (SelfPrivateKey <> nil) and not BigNumberIsNegative(SelfPrivateKey) then
  begin
    SharedSecretKey.Assign(OtherPublicKey);
    Ecc.MultiplePoint(SelfPrivateKey, SharedSecretKey);
    Result := True;
  end;
end;

end.
