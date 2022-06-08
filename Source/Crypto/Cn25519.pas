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
*           签名基于 rfc 8032 的说明
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.06.08 V1.1
*               实现 Ed25519 签名与验证
*           2022.06.07 V1.1
*               实现 Ed25519 扩展四元坐标的快速点加与标量乘实现，速度快十倍以上
*           2022.06.05 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNativeDecl, CnBigNumber, CnECC, CnSHA2;

const
  CN_25519_BLOCK_BYTESIZE = 32;

type
  TCnEcc4Point = class(TCnEcc3Point)
  {* 扩展的射影/仿射/雅可比坐标点，增加了 T 用于记录中间结果}
  private
    FT: TCnBigNumber;
    procedure SetT(const Value: TCnBigNumber);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function ToString: string; override; // 基类有 ToString

    property T: TCnBigNumber read FT write SetT;
  end;

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
    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); overload; virtual;
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

    procedure GenerateKeys(PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey); virtual;
    {* 生成一对该椭圆曲线的公私钥，私钥是运算次数 k，公钥是基点 G 经过 k 次乘法后得到的点坐标 K}

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

  TCnCurve25519 = class(TCnMontgomeryCurve)
  {* rfc 7748/8032 中规定的 Curve25519 曲线}
  public
    constructor Create; override;

    procedure GenerateKeys(PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey); override;
    {* 生成一对 Curve25519 椭圆曲线的公私钥，其中私钥的高低位有特殊处理}
  end;

  TCnEd25519Data = array[0..CN_25519_BLOCK_BYTESIZE - 1] of Byte;

  TCnEd25519SignatureData = array[0..2 * CN_25519_BLOCK_BYTESIZE - 1] of Byte;

  TCnEd25519 = class(TCnTwistedEdwardsCurve)
  {* rfc 7748/8032 中规定的 Ed25519 曲线}
  public
    constructor Create; override;

    function GenerateKeys(PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey): Boolean;
    {* 生成一对 Ed25519 椭圆曲线的公私钥，其中公钥的基点乘数根据 SHA512 运算而来}

    function PlainToPoint(Plain: TCnEd25519Data; OutPoint: TCnEccPoint): Boolean;
    {* 将 32 字节值转换为坐标点，涉及到求解}
    function PointToPlain(Point: TCnEccPoint; var OutPlain: TCnEd25519Data): Boolean;
    {* 将点坐标转换成 32 字节值，拼 Y 并放 X 正负一位}

    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); override;
    {* 重载父类的普通点乘，内部改用扩展四元快速乘}

    function IsNeutualExtendedPoint(P: TCnEcc4Point): Boolean;
    {* 判断点是否是中性点，也就是判断 X = 0 且 Y = 1，与 Weierstrass 的无限远点全 0 不同}
    procedure SetNeutualExtendedPoint(P: TCnEcc4Point);
    {* 将点设为中性点，也就是 X := 0 且 Y := 1}

    function ExtendedPointAddPoint(P, Q, Sum: TCnEcc4Point): Boolean;
    {* 使用扩展扭曲爱德华坐标（四元）的快速点加法计算 P + Q，值放入 Sum 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    function ExtendedPointSubPoint(P, Q, Diff: TCnEcc4Point): Boolean;
    {* 使用扩展扭曲爱德华坐标（四元）计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure ExtendedPointInverse(P: TCnEcc4Point);
    {* 使用扩展扭曲爱德华坐标（四元）计算 P 点的逆元 -P，值重新放入 P，也就是 Y 值取负}
    function IsExtendedPointOnCurve(P: TCnEcc4Point): Boolean;
    {* 判断扩展扭曲爱德华坐标（四元） P 点是否在本曲线上}

    procedure ExtendedMultiplePoint(K: Int64; Point: TCnEcc4Point); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P}
    procedure ExtendedMultiplePoint(K: TCnBigNumber; Point: TCnEcc4Point); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P，速度比普通标量乘快十倍以上}
  end;

  TCnEd25519Signature = class(TPersistent)
  {* Ed25519 的签名，是一个点与一个大数，与 TCnEccSignature 不同}
  private
    FR: TCnEccPoint;
    FS: TCnBigNumber;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure SaveToData(var Sig: TCnEd25519SignatureData);
    {* 内容转换成 64 字节签名数组供存储与传输}

    procedure LoadFromData(Sig: TCnEd25519SignatureData);
    {* 从64 字节签名数组中加载签名}

    property R: TCnEccPoint read FR;
    {* 签名点 R}
    property S: TCnBigNumber read FS;
    {* 签名数 S}
  end;

function CnEcc4PointToString(const P: TCnEcc4Point): string;
{* 将一个 TCnEcc4Point 点坐标转换为十进制字符串}

function CnEcc4PointToHex(const P: TCnEcc4Point): string;
{* 将一个 TCnEcc4Point 点坐标转换为十六进制字符串}

function CnEcc4PointEqual(const P, Q: TCnEcc4Point; Prime: TCnBigNumber): Boolean;
{* 判断两个 TCnEcc4Point 是否同一个点}

function CnEccPointToEcc4Point(P: TCnEccPoint; P4: TCnEcc4Point; Prime: TCnBigNumber): Boolean;
{* 大数范围内的普通坐标到扩展仿射坐标的点转换}

function CnEcc4PointToEccPoint(P4: TCnEcc4Point; P: TCnEccPoint; Prime: TCnBigNumber): Boolean;
{* 大数范围内的扩展仿射坐标到普通坐标的点转换}

function CnCurve25519PointToEd25519Point(DestPoint, SourcePoint: TCnEccPoint): Boolean;
{* 将 Curve25519 的坐标点转换为 Ed25519 的坐标点}

function CnEd25519PointToCurve25519Point(DestPoint, SourcePoint: TCnEccPoint): Boolean;
{* 将 Ed25519 的坐标点转换为 Curve25519 的坐标点}

function CnEd25519PointToData(P: TCnEccPoint; var Data: TCnEd25519Data): Boolean;
{* 按 25519 标准将椭圆曲线点转换为压缩方式的 32 字节数组，返回转换是否成功}

function CnEd25519DataToPoint(Data: TCnEd25519Data; P: TCnEccPoint; out XOdd: Boolean): Boolean;
{* 按 25519 标准将 32 字节数组转换为椭圆曲线点压缩方式，返回转换是否成功，
  如果成功，P 中返回对应 Y 值，以及 XOdd 中返回对应的 X 值是否是奇数，需要外界自行解 X}

function CnEd25519BigNumberToData(N: TCnBigNumber; var Data: TCnEd25519Data): Boolean;
{* 按 25519 标准将乘数转换为 32 字节数组，返回转换是否成功}

function CnEd25519DataToBigNumber(Data: TCnEd25519Data; N: TCnBigNumber): Boolean;
{* 按 25519 标准将 32 字节数组转换为乘数，返回转换是否成功}

// ===================== Ed25519 椭圆曲线数字签名验证算法 ======================

function CnEd25519SignData(PlainData: Pointer; DataLen: Integer; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; OutSignature: TCnEd25519Signature; Ed25519: TCnEd25519 = nil): Boolean;
{* Ed25519 用公私钥对数据块进行签名，返回签名是否成功}

function CnEd25519VerifyData(PlainData: Pointer; DataLen: Integer; InSignature: TCnEd25519Signature;
  PublicKey: TCnEccPublicKey; Ed25519: TCnEd25519 = nil): Boolean;
{* Ed25519 用公钥对数据块与签名进行验证，返回验证是否成功}

// ================= Ed25519 椭圆曲线 Diffie-Hellman 密钥交换  =================

function CnCurve25519KeyExchangeStep1(SelfPrivateKey: TCnEccPrivateKey;
  OutPointToAnother: TCnEccPoint; Curve25519: TCnCurve25519 = nil): Boolean;
{* 基于 25519 的 Diffie-Hellman 密钥交换算法，A 与 B 均先调用此方法，
  根据各自私钥生成点坐标，该点坐标需发给对方。返回生成是否成功}

function CnCurve25519KeyExchangeStep2(SelfPrivateKey: TCnEccPrivateKey;
  InPointFromAnother: TCnEccPoint; OutKey: TCnEccPoint; Curve25519: TCnCurve25519 = nil): Boolean;
{* 基于 25519 的 Diffie-Hellman 密钥交换算法，A 与 B 收到对方的 Point 坐标后再调用此方法，
  根据各自私钥生成一共同的点坐标，该点坐标便为共享密钥，可再通过派生进一步复杂化。
  返回生成是否成功}

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
  // 等于 RFC 中的 y = 14781619447589544791020593568409986887264606134616475288964881837755586237401，但似乎不是 4/5，也就是 5 * Y mod P = 4
  // 可能是 5F51E65E475F794B1FE122D388B72EB36DC2B28192839E4DD6163A5D81312C14 才符合 4/5 并且和 Ed25519 的 GY 对应

  SCN_25519_SQRT_NEG_486664 = '0F26EDF460A006BBD27B08DC03FC4F7EC5A1D3D14B7D1A82CC6E04AAFF457E06';
  // 提前算好的 sqrt(-486664)，供点坐标转换计算

// =============================================================================
// 蒙哥马利曲线 By^2 = x^3 + Ax^2 + x 与扭曲爱德华曲线 au^2 + v^2 = 1 + du^2v^2
// 照理有等价的一一映射关系，其中 A = 2(a+d)/(a-d) （已验证） 且 B = 4 /(a-d)
// 但 Curve25519 曲线与 Ed25519 曲线又经过了参数调整，B = 4 /(a-d) 不成立
// 同样，(x, y) 与 (u, v) 的对应关系也因为 A B a d 关系的调整而不满足标准映射
// =============================================================================


var
  F25519BigNumberPool: TCnBigNumberPool = nil;

function CalcBigNumberDigest(const Num: TCnBigNumber; FixedLen: Integer): TSHA512Digest;
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    FillChar(Result[0], SizeOf(TSHA512Digest), 0);
    if BigNumberWriteBinaryToStream(Num, Stream, FixedLen) <> FixedLen then
      Exit;

    Result := SHA512Stream(Stream);
  finally
    Stream.Free;
  end;
end;

// 根据随机私钥，生成公钥与 Ed25519 签名使用的 Hash 种子
function CalcBigNumbersFromPrivateKey(const InPrivateKey: TCnBigNumber; FixedLen: Integer;
  OutMulFactor, OutHashPrefix: TCnBigNumber): Boolean;
var
  Dig: TSHA512Digest;
begin
  // 拿 PrivateKey 做 Sha512，得到 64 字节结果 Dig
  Dig := CalcBigNumberDigest(InPrivateKey, CN_25519_BLOCK_BYTESIZE);

  // 拿它做 Sha512，得到 64 字节结果，前 32 字节取来做乘数，先倒序，低 3 位得清零，
  // （和 CoFactor 是 2^3 = 8 对应），且最高位 2^255 得置 0，次高位 2^254 得置 1
  if OutMulFactor <> nil then
  begin
    ReverseMemory(@Dig[0], CN_25519_BLOCK_BYTESIZE);         // 得倒个序
    OutMulFactor.SetBinary(@Dig[0], CN_25519_BLOCK_BYTESIZE);

    OutMulFactor.ClearBit(0);                                // 低三位置 0
    OutMulFactor.ClearBit(1);
    OutMulFactor.ClearBit(2);
    OutMulFactor.ClearBit(CN_25519_BLOCK_BYTESIZE * 8 - 1);  // 最高位置 0
    OutMulFactor.SetBit(CN_25519_BLOCK_BYTESIZE * 8 - 2);    // 次高位置 1
  end;

  // 后 32 字节作为 Hash 的入口参数
  if OutHashPrefix <> nil then
    OutHashPrefix.SetBinary(@Dig[CN_25519_BLOCK_BYTESIZE], CN_25519_BLOCK_BYTESIZE);

  Result := True;
end;

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
//            x1 * y2 + x2 * y1                 y1 * y2 - a * x1 * x2
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

procedure TCnMontgomeryCurve.GenerateKeys(PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey);
begin
  BigNumberRandRange(PrivateKey, FOrder);           // 比 0 大但比基点阶小的随机数
  if PrivateKey.IsZero then                         // 万一真拿到 0，就加 1
    PrivateKey.SetOne;

  PublicKey.Assign(FGenerator);
  MultiplePoint(PrivateKey, PublicKey);             // 基点乘 PrivateKey 次
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
  //          (y2 - y1)           3*x1^2 + 2*A*x1 + 1
  // 斜率 K = ----------  或 =  ----------------------
  //          (x2 - x1)                2*y1
  //
  // x3 = B*K^2 - A - x1 - x2
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
      // 分子是 (3*x1^2 + 2*A*x1 + 1)
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

      if not BigNumberAddMod(X, P.Y, P.Y, FFiniteFieldSize) then  // 2Y
        Exit;

      if not BigNumberModularInverse(T, X, FFiniteFieldSize) then // 得到分母 2*y1
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

      if not BigNumberSubMod(Y, Q.Y, P.Y, FFiniteFieldSize) then   // 得到分子 (y2 - y1)
        Exit;

      if not BigNumberSubMod(X, Q.X, P.X, FFiniteFieldSize) then   // 得到分母 (x2 - x1)
        Exit;

      if not BigNumberModularInverse(T, X, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(K, Y, T, FFiniteFieldSize) then // K 得到割线斜率
        Exit;
    end;

    // x3 = B * K^2 - A - x1 - x2
    if not BigNumberDirectMulMod(SX, K, K, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(SX, FCoefficientB, SX, FFiniteFieldSize) then
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

    if BigNumberCopy(Sum.X, SX) = nil then
      Exit;
    if BigNumberCopy(Sum.Y, SY) = nil then
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

procedure TCnCurve25519.GenerateKeys(PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey);
begin
  BigNumberRandRange(PrivateKey, FOrder);           // 比 0 大但比基点阶小的随机数
  if PrivateKey.IsZero then                         // 万一真拿到 0，就加 1
    PrivateKey.SetOne;

  PrivateKey.ClearBit(0);                                // 低三位置 0
  PrivateKey.ClearBit(1);
  PrivateKey.ClearBit(2);
  PrivateKey.ClearBit(CN_25519_BLOCK_BYTESIZE * 8 - 1);  // 最高位置 0
  PrivateKey.SetBit(CN_25519_BLOCK_BYTESIZE * 8 - 2);    // 次高位置 1

  PublicKey.Assign(FGenerator);
  MultiplePoint(PrivateKey, PublicKey);             // 基点乘 PrivateKey 次
end;

{ TCnEd25519 }

constructor TCnEd25519.Create;
begin
  inherited;
  Load(SCN_25519_EDWARDS_A, SCN_25519_EDWARDS_D, SCN_25519_PRIME, SCN_25519_EDWARDS_GX,
    SCN_25519_EDWARDS_GY, SCN_25519_ORDER, 8);
end;

procedure TCnEd25519.ExtendedMultiplePoint(K: Int64; Point: TCnEcc4Point);
var
  BK: TCnBigNumber;
begin
  BK := F25519BigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    ExtendedMultiplePoint(BK, Point);
  finally
    F25519BigNumberPool.Recycle(BK);
  end;
end;

procedure TCnEd25519.ExtendedMultiplePoint(K: TCnBigNumber;
  Point: TCnEcc4Point);
var
  I: Integer;
  E, R: TCnEcc4Point;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    ExtendedPointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    SetNeutualExtendedPoint(Point);
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEcc4Point.Create;
    E := TCnEcc4Point.Create;

    // R 要是中性点
    SetNeutualExtendedPoint(R);

    E.X := Point.X;
    E.Y := Point.Y;
    E.Z := Point.Z;
    E.T := Point.T;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        ExtendedPointAddPoint(R, E, R);
      ExtendedPointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
    Point.Z := R.Z;
  finally
    R.Free;
    E.Free;
  end;
end;

function TCnEd25519.ExtendedPointAddPoint(P, Q, Sum: TCnEcc4Point): Boolean;
var
  A, B, C, D, E, F, G, H: TCnBigNumber;
begin
  Result := False;
  A := nil;
  B := nil;
  C := nil;
  D := nil;
  E := nil;
  F := nil;
  G := nil;
  H := nil;

  try
    A := F25519BigNumberPool.Obtain;
    B := F25519BigNumberPool.Obtain;
    C := F25519BigNumberPool.Obtain;
    D := F25519BigNumberPool.Obtain;
    E := F25519BigNumberPool.Obtain;
    F := F25519BigNumberPool.Obtain;
    G := F25519BigNumberPool.Obtain;
    H := F25519BigNumberPool.Obtain;

    if CnEcc4PointEqual(P, Q, FFiniteFieldSize) then
    begin
      // 是同一个点
      if not BigNumberDirectMulMod(A, P.X, P.X, FFiniteFieldSize) then // A = X1^2
        Exit;

      if not BigNumberDirectMulMod(B, P.Y, P.Y, FFiniteFieldSize) then // B = Y1^2
        Exit;

      if not BigNumberDirectMulMod(C, P.Z, P.Z, FFiniteFieldSize) then 
        Exit;
      if not BigNumberAddMod(C, C, C, FFiniteFieldSize) then     // C = 2*Z1^2
        Exit;

      if not BigNumberAddMod(H, A, B, FFiniteFieldSize) then     // H = A+B
        Exit;

      if not BigNumberAddMod(E, P.X, P.Y, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(E, E, E, FFiniteFieldSize) then
        Exit;
      if not BigNumberSubMod(E, H, E, FFiniteFieldSize) then     // E = H-(X1+Y1)^2
        Exit;

      if not BigNumberSubMod(G, A, B, FFiniteFieldSize) then     // G = A-B
        Exit;

      if not BigNumberAddMod(F, C, G, FFiniteFieldSize) then     // F = C+G
        Exit;

      if not BigNumberDirectMulMod(Sum.X, E, F, FFiniteFieldSize) then // X3 = E*F
        Exit;

      if not BigNumberDirectMulMod(Sum.Y, G, H, FFiniteFieldSize) then // Y3 = G*H
        Exit;

      if not BigNumberDirectMulMod(Sum.T, E, H, FFiniteFieldSize) then // T3 = E*H
        Exit;

      if not BigNumberDirectMulMod(Sum.Z, F, G, FFiniteFieldSize) then // Z3 = F*G
        Exit;

      Result := True;
    end
    else
    begin
      // 不是同一个点。先用 G H 做临时变量
      if not BigNumberSubMod(G, P.Y, P.X, FFiniteFieldSize) then
        Exit;
      if not BigNumberSubMod(H, Q.Y, Q.X, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(A, G, H, FFiniteFieldSize) then // A = (Y1-X1)*(Y2-X2)
        Exit;

      if not BigNumberAddMod(G, P.Y, P.X, FFiniteFieldSize) then
        Exit;
      if not BigNumberAddMod(H, Q.Y, Q.X, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(B, G, H, FFiniteFieldSize) then  // B = (Y1+X1)*(Y2+X2)
        Exit;

      if not BigNumberAdd(C, FCoefficientD, FCoefficientD) then
        Exit;
      if not BigNumberDirectMulMod(C, P.T, C, FFiniteFieldSize) then
        Exit;
      if not BigNumberDirectMulMod(C, Q.T, C, FFiniteFieldSize) then  // C = T1*2*d*T2
        Exit;

      if not BigNumberAdd(D, P.Z, P.Z) then
        Exit;
      if not BigNumberDirectMulMod(D, Q.Z, D, FFiniteFieldSize) then  // D = Z1*2*Z2
        Exit;

      if not BigNumberSubMod(E, B, A, FFiniteFieldSize) then  // E = B-A
        Exit;

      if not BigNumberSubMod(F, D, C, FFiniteFieldSize) then  // F = D-C
        Exit;

      if not BigNumberAddMod(G, D, C, FFiniteFieldSize) then  // G = D+C
        Exit;
      if not BigNumberAddMod(H, B, A, FFiniteFieldSize) then  // H = B+A
        Exit;

      if not BigNumberDirectMulMod(Sum.X, E, F, FFiniteFieldSize) then  // X3 = E*F
        Exit;

      if not BigNumberDirectMulMod(Sum.Y, G, H, FFiniteFieldSize) then  // Y3 = G*H
        Exit;

      if not BigNumberDirectMulMod(Sum.T, E, H, FFiniteFieldSize) then  // T3 = E*H
        Exit;

      if not BigNumberDirectMulMod(Sum.Z, F, G, FFiniteFieldSize) then  // Z3 = F*G
        Exit;

      Result := True;
    end;
  finally
    F25519BigNumberPool.Recycle(H);
    F25519BigNumberPool.Recycle(G);
    F25519BigNumberPool.Recycle(F);
    F25519BigNumberPool.Recycle(E);
    F25519BigNumberPool.Recycle(D);
    F25519BigNumberPool.Recycle(C);
    F25519BigNumberPool.Recycle(B);
    F25519BigNumberPool.Recycle(A);
  end;
end;

procedure TCnEd25519.ExtendedPointInverse(P: TCnEcc4Point);
var
  T: TCnBigNumber;
begin
  T := F25519BigNumberPool.Obtain;
  try
    BigNumberDirectMulMod(T, P.Z, FFiniteFieldSize, FFiniteFieldSize);
    BigNumberSubMod(P.X, T, P.X, FFiniteFieldSize);

    // T := X * Y / Z^3
    BigNumberPowerWordMod(T, P.Z, 3, FFiniteFieldSize);
    BigNumberModularInverse(T, T, FFiniteFieldSize); // T 是 Z^3 的逆元
    BigNumberDirectMulMod(P.T, P.X, P.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(P.T, P.T, T, FFiniteFieldSize);
  finally
    F25519BigNumberPool.Recycle(T);
  end;
end;

function TCnEd25519.ExtendedPointSubPoint(P, Q,
  Diff: TCnEcc4Point): Boolean;
var
  Inv: TCnEcc4Point;
begin
  Inv := TCnEcc4Point.Create;
  try
    Inv.Assign(Q);
    ExtendedPointInverse(Inv);
    Result := ExtendedPointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

function TCnEd25519.GenerateKeys(PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
var
  K: TCnBigNumber;
begin
  Result := False;

  // 随机 32 字节做 PrivateKey
  if not BigNumberRandBytes(PrivateKey, CN_25519_BLOCK_BYTESIZE) then
    Exit;

  K := F25519BigNumberPool.Obtain;
  try

    if not CalcBigNumbersFromPrivateKey(PrivateKey, CN_25519_BLOCK_BYTESIZE,
      K, nil) then
      Exit;

    // 该乘数 K 乘以 G 点得到公钥
    PublicKey.Assign(FGenerator);
    MultiplePoint(K, PublicKey);                         // 基点乘 K 次

    Result := True;
  finally
    F25519BigNumberPool.Recycle(K);
  end;
end;

function CnEcc4PointToString(const P: TCnEcc4Point): string;
begin
  Result := Format('%s,%s,%s,%s', [P.X.ToDec, P.Y.ToDec, P.Z.ToDec, P.T.ToDec]);
end;

function CnEcc4PointToHex(const P: TCnEcc4Point): string;
begin
  Result := Format('%s,%s,%s,%s', [P.X.ToHex, P.Y.ToHex, P.Z.ToHex, P.T.ToHex]);
end;

function CnEcc4PointEqual(const P, Q: TCnEcc4Point; Prime: TCnBigNumber): Boolean;
var
  T1, T2: TCnBigNumber;
begin
  // X1Z2 = X2Z1 且 Y1Z2 = Y2Z1
  Result := False;
  if P = Q then
  begin
    Result := True;
    Exit;
  end;

  T1 := nil;
  T2 := nil;

  try
    T1 := F25519BigNumberPool.Obtain;
    T2 := F25519BigNumberPool.Obtain;

    BigNumberDirectMulMod(T1, P.X, Q.Z, Prime);
    BigNumberDirectMulMod(T2, Q.X, P.Z, Prime);

    if not BigNumberEqual(T1, T2) then
      Exit;

    BigNumberDirectMulMod(T1, P.Y, Q.Z, Prime);
    BigNumberDirectMulMod(T2, Q.Y, P.Z, Prime);

    if not BigNumberEqual(T1, T2) then
      Exit;

    Result := True;
  finally
    F25519BigNumberPool.Recycle(T2);
    F25519BigNumberPool.Recycle(T1);
  end;
end;

function CnEccPointToEcc4Point(P: TCnEccPoint; P4: TCnEcc4Point; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not CnEccPointToEcc3Point(P, P4) then
    Exit;
  Result := BigNumberDirectMulMod(P4.T, P.X, P.Y, Prime);
end;

function CnEcc4PointToEccPoint(P4: TCnEcc4Point; P: TCnEccPoint; Prime: TCnBigNumber): Boolean;
begin
  Result := CnAffinePointToEccPoint(P4, P, Prime);
end;

// =============================================================================
//
//          Curve25519 的 u v 和 Ed25519 的 x y 的双向映射关系为：
//
//              (u, v) = ((1+y)/(1-y), sqrt(-486664)*u/x)
//              (x, y) = (sqrt(-486664)*u/v, (u-1)/(u+1))
//
// =============================================================================

function CnCurve25519PointToEd25519Point(DestPoint, SourcePoint: TCnEccPoint): Boolean;
var
  S, T, Inv, Prime: TCnBigNumber;
begin
  // x = sqrt(-486664)*u/v
  // y = (u-1)/(u+1)
  Result := False;

  S := nil;
  T := nil;
  Prime := nil;
  Inv := nil;

  try
    S := F25519BigNumberPool.Obtain;
    T := F25519BigNumberPool.Obtain;

    S.SetHex(SCN_25519_SQRT_NEG_486664);
    Prime := F25519BigNumberPool.Obtain;
    Prime.SetHex(SCN_25519_PRIME);

    if not BigNumberDirectMulMod(T, S, SourcePoint.X, Prime) then // sqrt * u
      Exit;

    Inv := F25519BigNumberPool.Obtain;
    if not BigNumberModularInverse(Inv, SourcePoint.Y, Prime) then // v^-1
      Exit;

    if not BigNumberDirectMulMod(DestPoint.X, T, Inv, Prime) then // 算到 X
      Exit;

    if BigNumberCopy(T, SourcePoint.X) = nil then
      Exit;
    if BigNumberCopy(S, SourcePoint.X) = nil then
      Exit;

    T.SubWord(1);  // u - 1
    S.AddWord(1);  // u + 1

    if not BigNumberModularInverse(Inv, S, Prime) then // (u + 1)^1
      Exit;
    if not BigNumberDirectMulMod(DestPoint.Y, T, Inv, Prime) then
      Exit;

    Result := True;
  finally
    F25519BigNumberPool.Recycle(Inv);
    F25519BigNumberPool.Recycle(Prime);
    F25519BigNumberPool.Recycle(T);
    F25519BigNumberPool.Recycle(S);
  end;
end;

function CnEd25519PointToCurve25519Point(DestPoint, SourcePoint: TCnEccPoint): Boolean;
var
  S, T, Inv, Prime: TCnBigNumber;
begin
  // u = (1+y)/(1-y)
  // v = sqrt(-486664)*u/x
  Result := False;

  S := nil;
  T := nil;
  Prime := nil;
  Inv := nil;

  try
    S := F25519BigNumberPool.Obtain;
    T := F25519BigNumberPool.Obtain;

    if BigNumberCopy(T, SourcePoint.Y) = nil then
      Exit;
    if BigNumberCopy(S, SourcePoint.Y) = nil then
      Exit;
    T.AddWord(1);  // T 是分子 1+y

    Prime := F25519BigNumberPool.Obtain;
    Prime.SetHex(SCN_25519_PRIME);

    if not BigNumberSubMod(S, CnBigNumberOne, SourcePoint.Y, Prime) then
      Exit;        // S 是分母 1-y

    Inv := F25519BigNumberPool.Obtain;
    if not BigNumberModularInverse(Inv, S, Prime) then // Inv 是分母负倒数供乘
      Exit;

    if not BigNumberDirectMulMod(DestPoint.X, T, Inv, Prime) then // 得到 U
      Exit;

    S.SetHex(SCN_25519_SQRT_NEG_486664);
    if not BigNumberDirectMulMod(T, S, DestPoint.X, Prime) then
      Exit;

    if not BigNumberModularInverse(Inv, SourcePoint.X, Prime) then
      Exit;

    if not BigNumberDirectMulMod(DestPoint.Y, T, Inv, Prime) then
      Exit;

    Result := True;
  finally
    F25519BigNumberPool.Recycle(Inv);
    F25519BigNumberPool.Recycle(Prime);
    F25519BigNumberPool.Recycle(T);
    F25519BigNumberPool.Recycle(S);
  end;
end;

function CnEd25519PointToData(P: TCnEccPoint; var Data: TCnEd25519Data): Boolean;
begin
  Result := False;
  if P = nil then
    Exit;

  FillChar(Data[0], SizeOf(TCnEd25519Data), 0);
  P.Y.ToBinary(@Data[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd25519Data)); // 小端序，需要倒一下

  if P.X.IsOdd then // X 是奇数，最低位是 1
    Data[CN_25519_BLOCK_BYTESIZE - 1] := Data[CN_25519_BLOCK_BYTESIZE - 1] or $80  // 高位置 1
  else
    Data[CN_25519_BLOCK_BYTESIZE - 1] := Data[CN_25519_BLOCK_BYTESIZE - 1] and $7F; // 高位清 0

  Result := True;
end;

function CnEd25519DataToPoint(Data: TCnEd25519Data; P: TCnEccPoint;
  out XOdd: Boolean): Boolean;
var
  D: TCnEd25519Data;
begin
  Result := False;
  if P = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@D[0], SizeOf(TCnEd25519Data));
  P.Y.SetBinary(@D[0], SizeOf(TCnEd25519Data));

  // 最高位是否是 0 表示了 X 的奇偶
  XOdd := P.Y.IsBitSet(8 * CN_25519_BLOCK_BYTESIZE - 1);

  // 最高位得清零
  P.Y.ClearBit(8 * CN_25519_BLOCK_BYTESIZE - 1);
  Result := True;
end;

function CnEd25519BigNumberToData(N: TCnBigNumber; var Data: TCnEd25519Data): Boolean;
begin
  Result := False;
  if (N = nil) or (N.GetBytesCount > SizeOf(TCnEd25519Data)) then
    Exit;

  FillChar(Data[0], SizeOf(TCnEd25519Data), 0);
  N.ToBinary(@Data[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd25519Data));
  Result := True;
end;

function CnEd25519DataToBigNumber(Data: TCnEd25519Data; N: TCnBigNumber): Boolean;
var
  D: TCnEd25519Data;
begin
  Result := False;
  if N = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@D[0], SizeOf(TCnEd25519Data));
  N.SetBinary(@D[0], SizeOf(TCnEd25519Data));
  Result := True;
end;

function CnEd25519SignData(PlainData: Pointer; DataLen: Integer; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; OutSignature: TCnEd25519Signature; Ed25519: TCnEd25519): Boolean;
var
  Is25519Nil: Boolean;
  Stream: TMemoryStream;
  R, S, K, HP: TCnBigNumber;
  Dig: TSHA512Digest;
  Data: TCnEd25519Data;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (PrivateKey = nil) or (PublicKey = nil)
    or (OutSignature = nil) then
    Exit;

  R := nil;
  S := nil;
  K := nil;
  HP := nil;
  Stream := nil;
  Is25519Nil := Ed25519 = nil;

  try
    if Is25519Nil then
      Ed25519 := TCnEd25519.Create;

    R := F25519BigNumberPool.Obtain;
    S := F25519BigNumberPool.Obtain;
    K := F25519BigNumberPool.Obtain;
    HP := F25519BigNumberPool.Obtain;

    // 根据私钥得到私钥乘数 s 与杂凑前缀
    if not CalcBigNumbersFromPrivateKey(PrivateKey, CN_25519_BLOCK_BYTESIZE, S, HP) then
      Exit;

    // 杂凑前缀拼上原始文字
    Stream := TMemoryStream.Create;
    BigNumberWriteBinaryToStream(HP, Stream, CN_25519_BLOCK_BYTESIZE);
    Stream.Write(PlainData^, DataLen);

    // 计算出 SHA512 值作为 r 乘数，准备乘以基点作为 R 点
    Dig := SHA512Buffer(Stream.Memory, Stream.Size);

    ReverseMemory(@Dig[0], SizeOf(TSHA512Digest)); // 需要倒转一次
    R.SetBinary(@Dig[0], SizeOf(TSHA512Digest));
    if not BigNumberNonNegativeMod(R, R, Ed25519.Order) then // r 乘数太大先 mod 一下阶
      Exit;

    OutSignature.R.Assign(Ed25519.Generator);
    Ed25519.MultiplePoint(R, OutSignature.R);      // 计算得到签名值 R，该值是一个点坐标

    // 再 Hash 计算 S，先点 R 转换为字节数组
    if not Ed25519.PointToPlain(OutSignature.R, Data) then
      Exit;

    // 拼起来
    Stream.Clear;
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));

    // 公钥点也转换为字节数组
    if not Ed25519.PointToPlain(PublicKey, Data) then
      Exit;
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));

    // 写明文，拼凑完毕
    Stream.Write(PlainData^, DataLen);

    // 再次杂凑 R||PublicKey||明文
    Dig := SHA512Buffer(Stream.Memory, Stream.Size);

    ReverseMemory(@Dig[0], SizeOf(TSHA512Digest)); // 又需要倒转一次
    K.SetBinary(@Dig[0], SizeOf(TSHA512Digest));
    if not BigNumberNonNegativeMod(K, K, Ed25519.Order) then // 乘数太大再先 mod 一下阶
      Exit;

    // 计算乘数 R + K * S mod Order
    if not BigNumberDirectMulMod(OutSignature.S, K, S, Ed25519.Order) then
      Exit;
    if not BigNumberAddMod(OutSignature.S, R, OutSignature.S, Ed25519.Order) then
      Exit;

    Result := True;
  finally
    Stream.Free;
    F25519BigNumberPool.Recycle(HP);
    F25519BigNumberPool.Recycle(K);
    F25519BigNumberPool.Recycle(S);
    F25519BigNumberPool.Recycle(R);
    if Is25519Nil then
      Ed25519.Free;
  end;
end;

function CnEd25519VerifyData(PlainData: Pointer; DataLen: Integer; InSignature: TCnEd25519Signature;
  PublicKey: TCnEccPublicKey; Ed25519: TCnEd25519): Boolean;
var
  Is25519Nil: Boolean;
  L, R, M: TCnEccPoint;
  T: TCnBigNumber;
  Stream: TMemoryStream;
  Data: TCnEd25519Data;
  Dig: TSHA512Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (PublicKey = nil) or (InSignature = nil) then
    Exit;

  L := nil;
  R := nil;
  Stream := nil;
  T := nil;
  M := nil;
  Is25519Nil := Ed25519 = nil;

  try
    if Is25519Nil then
      Ed25519 := TCnEd25519.Create;

    // 验证 8*S*基点 是否 = 8*R点 + 8*Hash(R32位||公钥点32位||明文) * 公钥点
    L := TCnEccPoint.Create;
    R := TCnEccPoint.Create;

    L.Assign(Ed25519.Generator);
    Ed25519.MultiplePoint(InSignature.S, L);
    Ed25519.MultiplePoint(8, L);  // 算到左边点

    R.Assign(InSignature.R);
    Ed25519.MultiplePoint(8, R);  // 算到 8*R点待加

    Stream := TMemoryStream.Create;
    if not CnEd25519PointToData(InSignature.R, Data) then
      Exit;
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));        // 拼 R 点
    if not CnEd25519PointToData(PublicKey, Data) then
      Exit;
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));        // 拼公钥点
    Stream.Write(PlainData^, DataLen);                    // 拼明文

    Dig := SHA512Buffer(Stream.Memory, Stream.Size);      // 计算 Hash 作为值
    ReverseMemory(@Dig[0], SizeOf(TSHA512Digest));        // 需要倒转一次

    T := F25519BigNumberPool.Obtain;
    T.SetBinary(@Dig[0], SizeOf(TSHA512Digest));
    T.MulWord(8);
    if not BigNumberNonNegativeMod(T, T, Ed25519.Order) then // T 乘数太大先 mod 一下阶
      Exit;

    M := TCnEccPoint.Create;
    M.Assign(PublicKey);
    Ed25519.MultiplePoint(T, M);      // T 乘公钥点
    Ed25519.PointAddPoint(R, M, R);   // 点加

    Result := CnEccPointsEqual(L, R);
  finally
    M.Free;
    F25519BigNumberPool.Recycle(T);
    Stream.Free;
    R.Free;
    L.Free;
    if Is25519Nil then
      Ed25519.Free;
  end;
end;

function CnCurve25519KeyExchangeStep1(SelfPrivateKey: TCnEccPrivateKey;
  OutPointToAnother: TCnEccPoint; Curve25519: TCnCurve25519): Boolean;
var
  Is25519Nil: Boolean;
begin
  Result := False;
  if (SelfPrivateKey = nil) or (OutPointToAnother = nil) then
    Exit;

  Is25519Nil := Curve25519 = nil;

  try
    if Is25519Nil then
      Curve25519 := TCnCurve25519.Create;

    OutPointToAnother.Assign(Curve25519.Generator);
    Curve25519.MultiplePoint(SelfPrivateKey, OutPointToAnother);

    Result := True;
  finally
    if Is25519Nil then
      Curve25519.Free;
  end;
end;

function CnCurve25519KeyExchangeStep2(SelfPrivateKey: TCnEccPrivateKey;
  InPointFromAnother: TCnEccPoint; OutKey: TCnEccPoint; Curve25519: TCnCurve25519): Boolean;
var
  Is25519Nil: Boolean;
begin
  Result := False;
  if (SelfPrivateKey = nil) or (InPointFromAnother = nil) or (OutKey = nil) then
    Exit;

  Is25519Nil := Curve25519 = nil;

  try
    if Is25519Nil then
      Curve25519 := TCnCurve25519.Create;

    OutKey.Assign(InPointFromAnother);
    Curve25519.MultiplePoint(SelfPrivateKey, OutKey);

    Result := True;
  finally
    if Is25519Nil then
      Curve25519.Free;
  end;
end;

function TCnEd25519.IsExtendedPointOnCurve(P: TCnEcc4Point): Boolean;
var
  Q: TCnEccPoint;
begin
  Q := TCnEccPoint.Create;
  try
    CnEcc4PointToEccPoint(P, Q, FFiniteFieldSize);
    Result := IsPointOnCurve(Q);
  finally
    Q.Free;
  end;
end;

function TCnEd25519.IsNeutualExtendedPoint(P: TCnEcc4Point): Boolean;
begin
  Result := P.X.IsZero and P.T.IsZero and not P.Y.IsZero and not P.Z.IsZero
    and BigNumberEqual(P.Y, P.Z);
end;

procedure TCnEd25519.MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
var
  P4: TCnEcc4Point;
begin
  P4 := TCnEcc4Point.Create;
  try
    CnEccPointToEcc4Point(Point, P4, FFiniteFieldSize);
    ExtendedMultiplePoint(K, P4);
    CnEcc4PointToEccPoint(P4, Point, FFiniteFieldSize);
  finally
    P4.Free;
  end;
end;

function TCnEd25519.PlainToPoint(Plain: TCnEd25519Data;
  OutPoint: TCnEccPoint): Boolean;
var
  XOdd: Boolean;
  T, Y, Inv: TCnBigNumber;
begin
  Result := False;
  if OutPoint = nil then
    Exit;

  // 先从 Plain 中还原 Y 坐标以及 X 点的奇偶性
  if not CnEd25519DataToPoint(Plain, OutPoint, XOdd) then
    Exit;

  // 得到 Y 后求解 x 的方程 x^2 = (Y^2 - 1) / (D*Y^2 + 1) mod P
  // 注意素数 25519 是 8u5 的形式

  T := nil;
  Y := nil;
  Inv := nil;

  try
    T := F25519BigNumberPool.Obtain;
    Y := F25519BigNumberPool.Obtain;

    if not BigNumberDirectMulMod(Y, OutPoint.Y, OutPoint.Y, FFiniteFieldSize) then
      Exit;
    Y.SubWord(1); // Y := Y^2 - 1

    if not BigNumberDirectMulMod(T, OutPoint.Y, OutPoint.Y, FFiniteFieldSize) then
      Exit;
    if not BigNumberDirectMulMod(T, T, FCoefficientD, FFiniteFieldSize) then
      Exit;
    T.AddWord(1); // T := D*Y^2 + 1

    Inv := F25519BigNumberPool.Obtain;
    if not BigNumberModularInverse(Inv, T, FFiniteFieldSize) then
      Exit;

    if not BigNumberDirectMulMod(Y, Y, Inv, FFiniteFieldSize) then // Y 得到方程右边的值
      Exit;

    if not BigNumberSquareRootModPrime(OutPoint.X, Y, FFiniteFieldSize) then
      Exit;

    // 算出 X 了
    if OutPoint.X.IsBitSet(0) <> XOdd then
      if BigNumberSub(OutPoint.X, FFiniteFieldSize, OutPoint.X) then
        Exit;

    Result := True;
  finally
    F25519BigNumberPool.Recycle(Inv);
    F25519BigNumberPool.Recycle(Y);
    F25519BigNumberPool.Recycle(T);
  end;
end;

function TCnEd25519.PointToPlain(Point: TCnEccPoint;
  var OutPlain: TCnEd25519Data): Boolean;
begin
  Result := False;
  if (Point = nil) or (BigNumberCompare(Point.Y, FFiniteFieldSize) >= 0) then
    Exit;

  Result := CnEd25519PointToData(Point, OutPlain);
end;

procedure TCnEd25519.SetNeutualExtendedPoint(P: TCnEcc4Point);
begin
  P.X.SetZero;
  P.Y.SetOne;
  P.Z.SetOne;
  P.T.SetZero;
end;

{ TCnEd25519Sigature }

procedure TCnEd25519Signature.Assign(Source: TPersistent);
begin
  if Source is TCnEd25519Signature then
  begin
    FR.Assign((Source as TCnEd25519Signature).R);
    BigNumberCopy(FS, (Source as TCnEd25519Signature).S);
  end
  else
    inherited;
end;

constructor TCnEd25519Signature.Create;
begin
  inherited;
  FR := TCnEccPoint.Create;
  FS := TCnBigNumber.Create;
end;

destructor TCnEd25519Signature.Destroy;
begin
  FS.Free;
  FR.Free;
  inherited;
end;

{ TCnEcc4Point }

procedure TCnEcc4Point.Assign(Source: TPersistent);
begin
  if Source is TCnEcc4Point then
    BigNumberCopy(FT, (Source as TCnEcc4Point).T);
  inherited;
end;

constructor TCnEcc4Point.Create;
begin
  inherited;
  FT := TCnBigNumber.Create;
end;

destructor TCnEcc4Point.Destroy;
begin
  FT.Free;
  inherited;
end;

procedure TCnEcc4Point.SetT(const Value: TCnBigNumber);
begin
  BigNumberCopy(FT, Value);
end;

function TCnEcc4Point.ToString: string;
begin
  Result := CnEcc4PointToHex(Self);
end;

procedure TCnEd25519Signature.LoadFromData(Sig: TCnEd25519SignatureData);
var
  Data: TCnEd25519Data;
  Ed25519: TCnEd25519;
begin
  Move(Sig[0], Data[0], SizeOf(TCnEd25519Data));

  // 从 Data 中加载 R 点
  Ed25519 := TCnEd25519.Create;
  try
    Ed25519.PlainToPoint(Data, FR);
  finally
    Ed25519.Free;
  end;

  Move(Sig[SizeOf(TCnEd25519Data)], Data[0], SizeOf(TCnEd25519Data));
  // 从 Data 中加载 S 数
  CnEd25519DataToBigNumber(Data, FS);
end;

procedure TCnEd25519Signature.SaveToData(var Sig: TCnEd25519SignatureData);
var
  Data: TCnEd25519Data;
begin
  FillChar(Sig[0], SizeOf(TCnEd25519SignatureData), 0);

  // 把 R 点写入 Data
  CnEd25519PointToData(FR, Data);
  Move(Data[0], Sig[0], SizeOf(TCnEd25519Data));

  // 把 S 数写入 Data
  CnEd25519BigNumberToData(FS, Data);
  Move(Data[0], Sig[SizeOf(TCnEd25519Data)], SizeOf(TCnEd25519Data));
end;

initialization
  F25519BigNumberPool := TCnBigNumberPool.Create;

finalization
  F25519BigNumberPool.Free;

end.
