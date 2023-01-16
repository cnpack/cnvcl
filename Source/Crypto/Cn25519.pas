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

unit Cn25519;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：25519 系列椭圆曲线算法单元
* 单元作者：刘啸
* 备    注：目前实现了 Montgomery 椭圆曲线 y^2 = x^3 + A*X^2 + x
*           以及扭曲 Edwards 椭圆曲线 au^2 + v^2 = 1 + d * u^2 * v^2 的点加减乘
*           已实现仅基于 X 以及蒙哥马利阶梯的快速标量乘以及扩展四元坐标的快速点加
*           以及结合多项式约减代替模运算所进行的加速算法，是原始点加算法速度的五十倍以上
*           签名基于 rfc 8032 的说明
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.07.30 V1.5
*               去除部分无用的判断以精简代码
*           2022.06.14 V1.4
*               实现 Ed25519 对文件的签名与验证
*           2022.06.12 V1.3
*               实现 Field64 多项式拆项的有限域快速算法，
*               并基于此改造蒙哥马利阶梯加速标量乘与扩展四元坐标的快速点加与标量乘，
*               速度再次提高一倍以上，如果 64 位下，还能额外再次提高一倍
*           2022.06.09 V1.2
*               实现 Curve25519 曲线的蒙哥马利阶梯加速标量乘，速度较原始乘法快十倍以上
*           2022.06.08 V1.1
*               实现 Ed25519 签名与验证
*           2022.06.07 V1.1
*               实现 Ed25519 扩展四元坐标的快速点加与标量乘，速度较原始点加与乘法快十倍以上
*           2022.06.05 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative, CnBigNumber, CnInt128, CnECC, CnSHA2;

const
  CN_25519_BLOCK_BYTESIZE = 32;

type
  TCn25519Field64 = array[0..4] of TUInt64;
  {* 用多项式拆项法表示一个 2^255-19 范围内的有限域元素，f0 + 2*51*f1 + 2^102*f2 + 2^153*f3 + 2^204*f4}

  TCn25519Field64EccPoint = packed record
  {* 用多项式拆项法表示的 25519 椭圆曲线上的点（包括纯 X 射影点，Z 用 Y 代替）
    用于提速计算，不适用其他域的椭圆曲线}
    X: TCn25519Field64;
    Y: TCn25519Field64;
  end;

  TCn25519Field64Ecc4Point = packed record
  {* 用多项式拆项法表示的 25519 椭圆曲线上的四元扩展点
    用于提速计算，不适用其他域的椭圆曲线}
    X: TCn25519Field64;
    Y: TCn25519Field64;
    Z: TCn25519Field64;
    T: TCn25519Field64;
  end;

  TCnEcc4Point = class(TCnEcc3Point)
  {* 扩展的射影/仿射/雅可比坐标点，增加了 T 用于记录中间结果
     其中有 x = X/Z  y = Y/Z  x*y = T/Z，中性点是 （0, 1, 1, 0）}
  private
    FT: TCnBigNumber;
    procedure SetT(const Value: TCnBigNumber);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function ToString: string; override; // 基类有 ToString

    property T: TCnBigNumber read FT write SetT;
    {* 中间结果 T}
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

    procedure PointAddPoint(P, Q, Sum: TCnEccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同
      此处的加法的几何意义相当于单位圆上的与正 Y 轴的夹角角度相加法则，
      中性点(0, 1)，等同于 Weierstrass 曲线中的无穷远点}
    procedure PointSubPoint(P, Q, Diff: TCnEccPoint);
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
    FLadderConst: TCnBigNumber;
    FLadderField64: TCn25519Field64;
    procedure CheckLadderConst;
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
    {* 计算某点 P 的 k * P 值，值重新放入 Point}
    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); overload; virtual;
    {* 计算某点 P 的 k * P 值，值重新放入 Point，内部实现等同于 CnECC 中同名方法}

    procedure PointAddPoint(P, Q, Sum: TCnEccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同
      此处的加法的几何意义类似于 Weierstrass 椭圆曲线上的连线或切线交点再取负，同样存在无穷远点(0, 0)}
    procedure PointSubPoint(P, Q, Diff: TCnEccPoint);
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure PointInverse(P: TCnEccPoint);
    {* 计算 P 点的逆元 -P，值重新放入 P，也就是 Y 值取负}
    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* 判断 P 点是否在本曲线上}

    // ============ 蒙哥马利阶梯算法中的仅 X 的射影坐标点加速算法 ==============

    procedure PointToXAffinePoint(DestPoint, SourcePoint: TCnEccPoint);
    {* 将包含 X Y 的椭圆曲线点转换为射影坐标 X Y Z 并只保留 X Z 供蒙哥马利阶梯算法使用，
      其实就是 Y 置 1，SourcePoint 和 DestPoint 可以相同}
    procedure XAffinePointToPoint(DestPoint, SourcePoint: TCnEccPoint);
    {* 将只含 X Z(Y 代替 Z) 的射影坐标点转换为普通曲线点，其实就是求解 Y 并替换 Z，
      SourcePoint 和 DestPoint 可以相同}

    procedure XAffinePointInverse(P: TCnEccPoint);
    {* 计算仅 X 的射影坐标点 P 点的逆元 -P，值重新放入 P，也就是 Y 值取负
      实际内部因为没有 Y，啥都不需做}

    procedure MontgomeryLadderPointXDouble(Dbl: TCnEccPoint; P: TCnEccPoint);
    {* 蒙哥马利阶梯算法中的仅 X 的射影坐标点的二倍点运算，Y 内部作 Z 用，Dbl 可以是 P}
    procedure MontgomeryLadderPointXAdd(Sum, P, Q, PMinusQ: TCnEccPoint);
    {* 蒙哥马利阶梯算法中的仅 X 的射影坐标点的点加运算，Y 内部作 Z 用，除了需要两个点值外还需要一个差点值}

    procedure MontgomeryLadderMultiplePoint(K: Int64; Point: TCnEccPoint); overload;
    {* 用蒙哥马利阶梯算法计算仅 X 的射影坐标点的 K 倍点，值重新放入 Point}
    procedure MontgomeryLadderMultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); overload;
    {* 用蒙哥马利阶梯算法计算仅 X 的射影坐标点的 K 倍点，值重新放入 Point}

    // ======= 蒙哥马利阶梯算法中的仅 X 的射影坐标点 2^51 多项式加速算法 =======

    procedure PointToField64XAffinePoint(var DestPoint: TCn25519Field64EccPoint; SourcePoint: TCnEccPoint);
    {* 将包含 X Y 的椭圆曲线点转换为射影坐标 X Y Z 并只保留 X Z 并转换为多项式点，供蒙哥马利阶梯算法使用}
    procedure Field64XAffinePointToPoint(DestPoint: TCnEccPoint; var SourcePoint: TCn25519Field64EccPoint);
    {* 将多项式形式的只含 X Z(Y 代替 Z) 的射影坐标点转换为普通曲线点}

    procedure MontgomeryLadderField64PointXDouble(var Dbl: TCn25519Field64EccPoint; var P: TCn25519Field64EccPoint);
    {* 多项式形式的蒙哥马利阶梯算法中的仅 X 的射影坐标点的二倍点运算，Y 内部作 Z 用，Dbl 可以是 P}
    procedure MontgomeryLadderField64PointXAdd(var Sum, P, Q, PMinusQ: TCn25519Field64EccPoint);
    {* 多项式形式的蒙哥马利阶梯算法中的仅 X 的射影坐标点的点加运算，Y 内部作 Z 用，除了需要两个点值外还需要一个差点值}

    procedure MontgomeryLadderField64MultiplePoint(K: Int64; var Point: TCn25519Field64EccPoint); overload;
    {* 用多项式形式的蒙哥马利阶梯算法计算仅 X 的射影坐标点的 K 倍点，值重新放入 Point}
    procedure MontgomeryLadderField64MultiplePoint(K: TCnBigNumber; var Point: TCn25519Field64EccPoint); overload;
    {* 用多项式形式的蒙哥马利阶梯算法计算仅 X 的射影坐标点的 K 倍点，值重新放入 Point}

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

    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); override;
    {* 计算某点 P 的 k * P 值，值重新放入 Point，内部实现使用 64 位多项式拆项的蒙哥马利阶梯算法}
  end;

  TCnEd25519Data = array[0..CN_25519_BLOCK_BYTESIZE - 1] of Byte;

  TCnEd25519SignatureData = array[0..2 * CN_25519_BLOCK_BYTESIZE - 1] of Byte;

  TCnEd25519 = class(TCnTwistedEdwardsCurve)
  {* rfc 7748/8032 中规定的 Ed25519 曲线}
  public
    constructor Create; override;

    function GenerateKeys(PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey): Boolean;
    {* 生成一对 Ed25519 椭圆曲线的公私钥，其中公钥的基点乘数根据 SHA512 运算而来}

    procedure PlainToPoint(Plain: TCnEd25519Data; OutPoint: TCnEccPoint);
    {* 将 32 字节值转换为坐标点，涉及到求解}
    procedure PointToPlain(Point: TCnEccPoint; var OutPlain: TCnEd25519Data);
    {* 将点坐标转换成 32 字节值，拼 Y 并放 X 正负一位}

    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); override;
    {* 重载父类的普通点乘，内部改用扩展四元快速乘}

    function IsNeutualExtendedPoint(P: TCnEcc4Point): Boolean;
    {* 判断点是否是中性点，也就是判断 X = 0 且 Y = Z <> 0 且 T = 0，与 Weierstrass 的无限远点全 0 不同}
    procedure SetNeutualExtendedPoint(P: TCnEcc4Point);
    {* 将点设为中性点，也就是 X := 0 且 Y := 1 且 Z := 1 且 T := 0}

    // ================= 扩展扭曲爱德华坐标（四元）点加速算法 ==================

    procedure ExtendedPointAddPoint(P, Q, Sum: TCnEcc4Point);
    {* 使用扩展扭曲爱德华坐标（四元）的快速点加法计算 P + Q，值放入 Sum 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure ExtendedPointSubPoint(P, Q, Diff: TCnEcc4Point);
    {* 使用扩展扭曲爱德华坐标（四元）计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure ExtendedPointInverse(P: TCnEcc4Point);
    {* 使用扩展扭曲爱德华坐标（四元）计算 P 点的逆元 -P，值重新放入 P，也就是 Y 值取负}
    function IsExtendedPointOnCurve(P: TCnEcc4Point): Boolean;
    {* 判断扩展扭曲爱德华坐标（四元） P 点是否在本曲线上}

    procedure ExtendedMultiplePoint(K: Int64; Point: TCnEcc4Point); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P}
    procedure ExtendedMultiplePoint(K: TCnBigNumber; Point: TCnEcc4Point); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P，速度比普通标量乘快十倍以上}

    // ============= 扩展扭曲爱德华坐标（四元）点的多项式加速算法 ==============

    function ExtendedField64PointAddPoint(var P, Q, Sum: TCn25519Field64Ecc4Point): Boolean;
    {* 使用扩展扭曲爱德华坐标（四元）有限域多项式的快速点加法计算 P + Q，值放入 Sum 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    function ExtendedField64PointSubPoint(var P, Q, Diff: TCn25519Field64Ecc4Point): Boolean;
    {* 使用扩展扭曲爱德华坐标（四元）有限域多项式计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同}
    procedure ExtendedField64PointInverse(var P: TCn25519Field64Ecc4Point);
    {* 使用扩展扭曲爱德华坐标（四元）有限域多项式计算 P 点的逆元 -P，值重新放入 P，也就是 Y 值取负}
    function IsExtendedField64PointOnCurve(var P: TCn25519Field64Ecc4Point): Boolean;
    {* 判断扩展扭曲爱德华坐标（四元）有限域多项式 P 点是否在本曲线上}

    procedure ExtendedField64MultiplePoint(K: Int64; var Point: TCn25519Field64Ecc4Point); overload;
    {* 使用扩展扭曲爱德华坐标（四元）有限域多项式计算某点 P 的 k * P 值，值重新放入 P}
    procedure ExtendedField64MultiplePoint(K: TCnBigNumber; var Point: TCn25519Field64Ecc4Point); overload;
    {* 使用扩展扭曲爱德华坐标（四元）有限域多项式计算某点 P 的 k * P 值，值重新放入 P}
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

function CnEccPointToEcc4Point(DestPoint: TCnEcc4Point; SourcePoint: TCnEccPoint;
  Prime: TCnBigNumber): Boolean;
{* 大数范围内的普通坐标到扩展仿射坐标的点转换}

function CnEcc4PointToEccPoint(DestPoint: TCnEccPoint; SourcePoint: TCnEcc4Point;
  Prime: TCnBigNumber): Boolean;
{* 大数范围内的扩展仿射坐标到普通坐标的点转换}

procedure CnCurve25519PointToEd25519Point(DestPoint, SourcePoint: TCnEccPoint);
{* 将 Curve25519 的坐标点转换为 Ed25519 的坐标点，Source 和 Dest 可以相同}

procedure CnEd25519PointToCurve25519Point(DestPoint, SourcePoint: TCnEccPoint);
{* 将 Ed25519 的坐标点转换为 Curve25519 的坐标点，Source 和 Dest 可以相同}

procedure CnEd25519PointToData(P: TCnEccPoint; var Data: TCnEd25519Data);
{* 按 25519 标准将椭圆曲线点转换为压缩方式的 32 字节数组}

procedure CnEd25519DataToPoint(Data: TCnEd25519Data; P: TCnEccPoint; out XOdd: Boolean);
{* 按 25519 标准将 32 字节数组转换为椭圆曲线点压缩方式
  P 中返回对应 Y 值，以及 XOdd 中返回对应的 X 值是否是奇数，需要外界自行解 X}

procedure CnEd25519BigNumberToData(N: TCnBigNumber; var Data: TCnEd25519Data);
{* 按 25519 标准将乘数转换为 32 字节数组，返回转换是否成功}

procedure CnEd25519DataToBigNumber(Data: TCnEd25519Data; N: TCnBigNumber);
{* 按 25519 标准将 32 字节数组转换为乘数，返回转换是否成功}

// ===================== Ed25519 椭圆曲线数字签名验证算法 ======================

function CnEd25519SignData(PlainData: Pointer; DataLen: Integer; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; OutSignature: TCnEd25519Signature; Ed25519: TCnEd25519 = nil): Boolean;
{* Ed25519 用公私钥对数据块进行签名，返回签名是否成功}

function CnEd25519VerifyData(PlainData: Pointer; DataLen: Integer; InSignature: TCnEd25519Signature;
  PublicKey: TCnEccPublicKey; Ed25519: TCnEd25519 = nil): Boolean;
{* Ed25519 用公钥对数据块与签名进行验证，返回验证是否成功}

function CnEd25519SignFile(const FileName: string; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; OutSignatureStream: TStream; Ed25519: TCnEd25519 = nil): Boolean;
{* Ed25519 用公私钥对文件进行签名，签名值 64 字节写入 OutSignatureStream 中，返回签名是否成功}

function CnEd25519VerifyFile(const FileName: string; InSignatureStream: TStream;
  PublicKey: TCnEccPublicKey; Ed25519: TCnEd25519 = nil): Boolean;
{* Ed25519 用公钥对文件与签名进行验证，InSignatureStream 内部须是 64 字节签名值，返回验证是否成功}

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

// ============================== 多项式加速算法 ===============================

procedure Cn25519BigNumberToField64(var Field: TCn25519Field64; const Num: TCnBigNumber);
{* 将一个大数转换为 2^255-19 有限域范围内的 64 位多项式系数}

procedure Cn25519Field64ToBigNumber(const Res: TCnBigNumber; var Field: TCn25519Field64);
{* 将一个大数转换为 2^255-19 有限域范围内的 64 位多项式系数}

procedure Cn25519Field64Reduce(var Field: TCn25519Field64);
{* 将一个 64 位多项式系数在 2^255-19 有限域范围内正规化，
  也就是把每个系数确保比 2^51 小，大的部分进位到下一个，总值如超出有限域上界也会自动求模}

function Cn25519Field64ToHex(var Field: TCn25519Field64): string;
{* 将一个 64 位多项式系数转换为十六进制字符串}

procedure Cn25519Field64Copy(var Dest, Source: TCn25519Field64);
{* 复制一个 2^255-19 有限域范围内的 64 位多项式系数}

function Cn25519Field64Equal(var A, B: TCn25519Field64): Boolean;
{* 判断两个 2^255-19 有限域范围内的 64 位多项式系数是否相等}

procedure Cn25519Field64Swap(var A, B: TCn25519Field64);
{* 交换两个 2^255-19 有限域范围内的 64 位多项式系数}

procedure Cn25519Field64Zero(var Field: TCn25519Field64);
{* 将一个 2^255-19 有限域范围内的 64 位多项式系数置为 0}

procedure Cn25519Field64One(var Field: TCn25519Field64);
{* 将一个 2^255-19 有限域范围内的 64 位多项式系数置为 1}

procedure Cn25519Field64NegOne(var Field: TCn25519Field64);
{* 将一个 2^255-19 有限域范围内的 64 位多项式系数置为 -1}

procedure Cn25519Field64Negate(var Field: TCn25519Field64);
{* 将一个 2^255-19 有限域范围内的 64 位多项式系数置为相反数}

procedure Cn25519Field64Add(var Res, A, B: TCn25519Field64);
{* 两个 2^255-19 有限域范围内的 64 位多项式系数相加，A + B => Res，Res 可以是 A 或 B，A、B 可以是同一个}

procedure Cn25519Field64Sub(var Res, A, B: TCn25519Field64);
{* 两个 2^255-19 有限域范围内的 64 位多项式系数相减，A - B => Res，Res 可以是 A 或 B，A、B 可以是同一个}

procedure Cn25519Field64Mul(var Res, A, B: TCn25519Field64);
{* 两个 2^255-19 有限域范围内的 64 位多项式系数相乘，A * B => Res，Res 可以是 A 或 B，A、B 可以是同一个}

procedure Cn25519Field64Power(var Res, A: TCn25519Field64; K: Cardinal); overload;
{* 计算一个 2^255-19 有限域范围内的 64 位多项式的 K 次方值，A^K) => Res，Res 可以是 A}

procedure Cn25519Field64Power(var Res, A: TCn25519Field64; K: TCnBigNumber); overload;
{* 计算一个 2^255-19 有限域范围内的 64 位多项式的 K 次方值，A^K) => Res，Res 可以是 A}

procedure Cn25519Field64Power2K(var Res, A: TCn25519Field64; K: Cardinal);
{* 计算一个 2^255-19 有限域范围内的 64 位多项式的 2^K 次方值，A^(2^K) => Res，Res 可以是 A}

procedure Cn25519Field64ModularInverse(var Res, A: TCn25519Field64);
{* 计算一个 2^255-19 有限域范围内的 64 位多项式的模逆元，A * Res mod P = 1，Res 可以是 A}

// =========================== 多项式点处理函数 ================================

procedure Cn25519Field64EccPointZero(var Point: TCn25519Field64EccPoint);
{* 将一多项式拆项法表示的 25519 椭圆曲线上的点置 0}

procedure Cn25519Field64EccPointCopy(var DestPoint, SourcePoint: TCn25519Field64EccPoint);
{* 复制多项式拆项法表示的 25519 椭圆曲线上的点}

function Cn25519Field64EccPointToHex(var Point: TCn25519Field64EccPoint): string;
{* 将一多项式拆项法表示的 25519 椭圆曲线上的点转换为十六进制字符串}

function Cn25519Field64EccPointEqual(var A, B: TCn25519Field64EccPoint): Boolean;
{* 判断两个多项式拆项法表示的 25519 椭圆曲线上的点是否相等}

procedure Cn25519Field64Ecc4PointNeutual(var Point: TCn25519Field64Ecc4Point);
{* 将一多项式拆项法表示的 25519 椭圆曲线上的四元扩展点置为中性点}

procedure Cn25519Field64Ecc4PointCopy(var DestPoint, SourcePoint: TCn25519Field64Ecc4Point);
{* 复制多项式拆项法表示的 25519 椭圆曲线上的四元扩展点}

function Cn25519Field64Ecc4PointToHex(var Point: TCn25519Field64Ecc4Point): string;
{* 将一多项式拆项法表示的 25519 椭圆曲线上的四元扩展点转换为十六进制字符串}

function Cn25519Field64Ecc4PointEqual(var A, B: TCn25519Field64Ecc4Point): Boolean;
{* 判断两个多项式拆项法表示的 25519 椭圆曲线上的点是否相等}

function CnEccPointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEccPoint): Boolean;
{* 大数范围内的普通坐标到扩展仿射多项式坐标的点转换}

function CnField64Ecc4PointToEccPoint(DestPoint: TCnEccPoint;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
{* 大数范围内的扩展仿射多项式坐标到普通坐标的点转换}

function CnEcc4PointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEcc4Point): Boolean;
{* 大数范围内的扩展仿射坐标到扩展仿射多项式坐标的点转换}

function CnField64Ecc4PointToEcc4Point(DestPoint: TCnEcc4Point;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
{* 大数范围内的扩展仿射多项式坐标到扩展仿射坐标的点转换}

implementation

resourcestring
  SCnEInverseError = 'Point Inverse Error.';
  SCnECanNOTCalcErrorFmt = 'Can NOT Calucate %s,%s + %s,%s';

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

  SCN_LOW51_MASK = $7FFFFFFFFFFFF;

// =============================================================================
// 蒙哥马利曲线 By^2 = x^3 + Ax^2 + x 与扭曲爱德华曲线 au^2 + v^2 = 1 + du^2v^2
// 照理有等价的一一映射关系，其中 A = 2(a+d)/(a-d) （已验证） 且 B = 4 /(a-d)
// 但 Curve25519 曲线与 Ed25519 曲线又经过了参数调整，B = 4 /(a-d) 不成立
// 同样，(x, y) 与 (u, v) 的对应关系也因为 A B a d 关系的调整而不满足标准映射
// =============================================================================

var
  F25519BigNumberPool: TCnBigNumberPool = nil;
  FPrime25519: TCnBigNumber = nil;

  // 仨常量
  F25519Field64Zero: TCn25519Field64 = (0, 0, 0, 0, 0);
  F25519Field64One: TCn25519Field64 = (1, 0, 0, 0, 0);
  F25519Field64NegOne: TCn25519Field64 = (2251799813685228, 2251799813685247, 2251799813685247, 2251799813685247, 2251799813685247);

procedure ConditionalSwapPoint(Swap: Boolean; A, B: TCnEccPoint);
begin
  if Swap then
  begin
    BigNumberSwap(A.X, B.X);
    BigNumberSwap(A.Y, B.Y);
  end;
end;

procedure ConditionalSwapField64Point(Swap: Boolean; var A, B: TCn25519Field64EccPoint);
begin
  if Swap then
  begin
    Cn25519Field64Swap(A.X, B.X);
    Cn25519Field64Swap(A.Y, B.Y);
  end;
end;

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
procedure CalcBigNumbersFromPrivateKey(const InPrivateKey: TCnBigNumber; FixedLen: Integer;
  OutMulFactor, OutHashPrefix: TCnBigNumber);
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

  X := nil;
  Y := nil;
  L := nil;
  R := nil;

  try
    X := F25519BigNumberPool.Obtain;
    BigNumberCopy(X, P.X);
    BigNumberDirectMulMod(X, X, X, FFiniteFieldSize);

    Y := F25519BigNumberPool.Obtain;
    BigNumberCopy(Y, P.Y);
    BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize);

    L := F25519BigNumberPool.Obtain;
    BigNumberDirectMulMod(L, FCoefficientA, X, FFiniteFieldSize);
    BigNumberAddMod(L, L, Y, FFiniteFieldSize); // 此时 L := A * X^2 + Y^2

    R := F25519BigNumberPool.Obtain;
    BigNumberDirectMulMod(R, X, Y, FFiniteFieldSize);
    BigNumberDirectMulMod(R, FCoefficientD, R, FFiniteFieldSize);
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

procedure TCnTwistedEdwardsCurve.PointAddPoint(P, Q, Sum: TCnEccPoint);
var
  X, Y, T, D1, D2, N1, N2: TCnBigNumber;
begin
//            x1 * y2 + x2 * y1                 y1 * y2 - a * x1 * x2
//   x3 = --------------------------,   y3 = ---------------------------  并且无需考虑 P/Q 是否同一点
//         1 + d * x1 * x2 * y1 * y2          1 - d * x1 * x2 * y1 * y2

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

    BigNumberDirectMulMod(T, P.X, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(N1, Q.X, P.Y, FFiniteFieldSize);
    BigNumberAddMod(N1, N1, T, FFiniteFieldSize); // N1 得到 x1 * y2 + x2 * y1，释放 T

    BigNumberDirectMulMod(T, P.X, Q.X, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientA, FFiniteFieldSize);
    BigNumberDirectMulMod(N2, P.Y, Q.Y, FFiniteFieldSize);
    BigNumberSubMod(N2, N2, T, FFiniteFieldSize); // N2 得到 y1 * y2 - a * x1 * x2，释放 T

    BigNumberDirectMulMod(T, P.Y, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, Q.X, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, P.X, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientD, FFiniteFieldSize); // T 得到 d * x1 * x2 * y1 * y2

    BigNumberAddMod(D1, T, CnBigNumberOne, FFiniteFieldSize); // D1 得到 1 + d * x1 * x2 * y1 * y2
    BigNumberSubMod(D2, CnBigNumberOne, T, FFiniteFieldSize); // D2 得到 1 - d * x1 * x2 * y1 * y2

    BigNumberModularInverse(T, D1, FFiniteFieldSize);  // T 得到 D1 逆元
    BigNumberDirectMulMod(X, N1, T, FFiniteFieldSize); // 得到 Sum.X

    BigNumberModularInverse(T, D2, FFiniteFieldSize);  // T 得到 D2 逆元
    BigNumberDirectMulMod(Y, N2, T, FFiniteFieldSize); // 得到 Sum.Y

    BigNumberCopy(Sum.X, X);
    BigNumberCopy(Sum.Y, Y);
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
    raise ECnEccException.Create(SCnEInverseError);

  BigNumberSub(P.X, FFiniteFieldSize, P.X);
end;

procedure TCnTwistedEdwardsCurve.PointSubPoint(P, Q, Diff: TCnEccPoint);
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    PointAddPoint(P, Inv, Diff);
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

  FLadderConst := TCnBigNumber.Create;
end;

destructor TCnMontgomeryCurve.Destroy;
begin
  FLadderConst.Free;
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

  X := nil;
  Y := nil;
  T := nil;

  try
    X := F25519BigNumberPool.Obtain;
    BigNumberCopy(X, P.X);

    Y := F25519BigNumberPool.Obtain;
    BigNumberCopy(Y, P.Y);

    BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize);
    BigNumberDirectMulMod(Y, FCoefficientB, Y, FFiniteFieldSize);  // Y := B * y^2 mod P

    T := F25519BigNumberPool.Obtain;
    BigNumberDirectMulMod(T, FCoefficientA, X, FFiniteFieldSize);  // T := A*X

    T.AddWord(1); // T := A*X + 1
    BigNumberDirectMulMod(T, X, T, FFiniteFieldSize);       // T := X * (A*X + 1) = AX^2 + X
    BigNumberPowerWordMod(X, X, 3, FFiniteFieldSize);  // X^3
    BigNumberAddMod(X, X, T, FFiniteFieldSize); // X := x^3 + Ax^2 + x mod P

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

  // 提前计算 (A+2)/4 以备蒙哥马利阶梯算法中使用
  CheckLadderConst;
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

procedure TCnMontgomeryCurve.MontgomeryLadderMultiplePoint(K: TCnBigNumber;
  Point: TCnEccPoint);
var
  I, C: Integer;
  X0, X1: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    XAffinePointInverse(Point);
  end;

  if BigNumberIsZero(K) then 
  begin
    Point.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  X0 := nil;
  X1 := nil;

  try
    X0 := TCnEccPoint.Create;
    X1 := TCnEccPoint.Create;

    X1.Assign(Point);
    MontgomeryLadderPointXDouble(X0, Point);

    C := K.GetBitsCount;
    for I := C - 2 downto 0 do // 内部先不考虑 Time Constant 执行时间固定的要求
    begin
      ConditionalSwapPoint(K.IsBitSet(I + 1) <> K.IsBitSet(I), X0, X1); // 换

      MontgomeryLadderPointXAdd(X1, X0, X1, Point);
      MontgomeryLadderPointXDouble(X0, X0);
    end;

    ConditionalSwapPoint(K.IsBitSet(0), X0, X1);
    Point.Assign(X0);
  finally
    X1.Free;
    X0.Free;
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderPointXAdd(Sum, P, Q,
  PMinusQ: TCnEccPoint);
var
  V0, V1, V2, V3, V4: TCnBigNumber;
begin
  V0 := nil;
  V1 := nil;
  V2 := nil;
  V3 := nil;
  V4 := nil;

  try
    V0 := F25519BigNumberPool.Obtain;
    V1 := F25519BigNumberPool.Obtain;
    V2 := F25519BigNumberPool.Obtain;
    V3 := F25519BigNumberPool.Obtain;
    V4 := F25519BigNumberPool.Obtain;

    BigNumberAddMod(V0, P.X, P.Y, FFiniteFieldSize);
    BigNumberSubMod(V1, Q.X, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V1, V1, V0, FFiniteFieldSize);

    BigNumberSubMod(V0, P.X, P.Y, FFiniteFieldSize);
    BigNumberAddMod(V2, Q.X, Q.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V2, V2, V0, FFiniteFieldSize);

    BigNumberAddMod(V3, V1, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(V3, V3, V3, FFiniteFieldSize);

    BigNumberSubMod(V4, V1, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(V4, V4, V4, FFiniteFieldSize);

    BigNumberCopy(V0, PMinusQ.X); // V0 备份，避免 Sum 和 PMinusQ 是同一个点时被改动
    BigNumberDirectMulMod(Sum.X, PMinusQ.Y, V3, FFiniteFieldSize);
    BigNumberDirectMulMod(Sum.Y, V0, V4, FFiniteFieldSize);
  finally
    F25519BigNumberPool.Recycle(V4);
    F25519BigNumberPool.Recycle(V3);
    F25519BigNumberPool.Recycle(V2);
    F25519BigNumberPool.Recycle(V1);
    F25519BigNumberPool.Recycle(V0);
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderPointXDouble(Dbl,
  P: TCnEccPoint);
var
  V1, V2, V3: TCnBigNumber;
begin
  V1 := nil;
  V2 := nil;
  V3 := nil;

  try
    V1 := F25519BigNumberPool.Obtain;
    V2 := F25519BigNumberPool.Obtain;
    V3 := F25519BigNumberPool.Obtain;

    CheckLadderConst;

    BigNumberAddMod(V1, P.X, P.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V1, V1, V1, FFiniteFieldSize);
    BigNumberSubMod(V2, P.X, P.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(V2, V2, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(Dbl.X, V1, V2, FFiniteFieldSize);

    BigNumberSubMod(V1, V1, V2, FFiniteFieldSize);
    BigNumberDirectMulMod(V3, V1, FLadderConst, FFiniteFieldSize);
    BigNumberAddMod(V3, V3, V2, FFiniteFieldSize);

    BigNumberDirectMulMod(Dbl.Y, V1, V3, FFiniteFieldSize);
  finally
    F25519BigNumberPool.Recycle(V3);
    F25519BigNumberPool.Recycle(V2);
    F25519BigNumberPool.Recycle(V1);
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

procedure TCnMontgomeryCurve.PointAddPoint(P, Q, Sum: TCnEccPoint);
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
      BigNumberDirectMulMod(Y, FCoefficientA, P.X, FFiniteFieldSize);
      BigNumberAddMod(Y, Y, Y, FFiniteFieldSize);
      Y.AddWord(1); // Y 得到 2*A*x1 + 1

      BigNumberDirectMulMod(T, P.X, P.X, FFiniteFieldSize);
      T.MulWord(3);
      BigNumberAddMod(Y, T, Y, FFiniteFieldSize); // Y 得到 3*x1^2 + 2*A*x1 + 1，释放 T

      BigNumberAddMod(X, P.Y, P.Y, FFiniteFieldSize);  // 2Y
      BigNumberModularInverse(T, X, FFiniteFieldSize); // 得到分母 2*y1

      BigNumberDirectMulMod(K, Y, T, FFiniteFieldSize); // K 得到切线斜率
    end
    else
    begin
      if BigNumberCompare(P.X, Q.X) = 0 then // 如果 X 相等，要判断 Y 是不是互反，是则和为 0，不是则挂了
      begin
        BigNumberAdd(T, P.Y, Q.Y);
        if BigNumberCompare(T, FFiniteFieldSize) = 0 then  // 互反，和为 0
          Sum.SetZero
        else                                               // 不互反，挂了
          raise ECnEccException.CreateFmt(SCnECanNOTCalcErrorFmt,
            [P.X.ToDec, P.Y.ToDec, Q.X.ToDec, Q.Y.ToDec]);

        Exit;
      end;

      BigNumberSubMod(Y, Q.Y, P.Y, FFiniteFieldSize);   // 得到分子 (y2 - y1)
      BigNumberSubMod(X, Q.X, P.X, FFiniteFieldSize);   // 得到分母 (x2 - x1)

      BigNumberModularInverse(T, X, FFiniteFieldSize);
      BigNumberDirectMulMod(K, Y, T, FFiniteFieldSize); // K 得到割线斜率
    end;

    // x3 = B * K^2 - A - x1 - x2
    BigNumberDirectMulMod(SX, K, K, FFiniteFieldSize);
    BigNumberDirectMulMod(SX, FCoefficientB, SX, FFiniteFieldSize);
    BigNumberSubMod(SX, SX, FCoefficientA, FFiniteFieldSize);
    BigNumberSubMod(SX, SX, P.X, FFiniteFieldSize);
    BigNumberSubMod(SX, SX, Q.X, FFiniteFieldSize);

    // y3 = -(y1 + K * (x3 - x1))
    BigNumberSubMod(SY, SX, P.X, FFiniteFieldSize);
    BigNumberDirectMulMod(SY, SY, K, FFiniteFieldSize);
    BigNumberAddMod(SY, SY, P.Y, FFiniteFieldSize);
    BigNumberSub(SY, FFiniteFieldSize, SY);

    BigNumberCopy(Sum.X, SX);
    BigNumberCopy(Sum.Y, SY);
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
    raise ECnEccException.Create(SCnEInverseError);

  BigNumberSub(P.Y, FFiniteFieldSize, P.Y);
end;

procedure TCnMontgomeryCurve.PointSubPoint(P, Q, Diff: TCnEccPoint);
var
  Inv: TCnEccPoint;
begin
  Inv := TCnEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    PointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

procedure TCnMontgomeryCurve.CheckLadderConst;
var
  T: TCnBigNumber;
begin
  if FLadderConst.IsZero then
  begin
    FLadderConst.SetWord(4);
    T := F25519BigNumberPool.Obtain;

    try
      BigNumberModularInverse(T, FLadderConst, FFiniteFieldSize); // 先求 4 的逆元

      BigNumberCopy(FLadderConst, FCoefficientA); // 再算 A+2
      FLadderConst.AddWord(2);

      BigNumberDirectMulMod(FLadderConst, FLadderConst, T, FFiniteFieldSize); // 乘逆元等于除

      Cn25519BigNumberToField64(FLadderField64, FLadderConst);
    finally
      F25519BigNumberPool.Recycle(T);
    end;
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderMultiplePoint(K: Int64;
  Point: TCnEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := F25519BigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MontgomeryLadderMultiplePoint(BK, Point);
  finally
    F25519BigNumberPool.Recycle(BK);
  end;
end;

procedure TCnMontgomeryCurve.PointToXAffinePoint(DestPoint,
  SourcePoint: TCnEccPoint);
begin
  BigNumberCopy(DestPoint.X, SourcePoint.X);
  if SourcePoint.X.IsZero and SourcePoint.Y.IsZero then
  begin
    DestPoint.X.SetOne;
    DestPoint.Y.SetZero;
  end
  else
    DestPoint.Y.SetOne;
end;

procedure TCnMontgomeryCurve.XAffinePointToPoint(DestPoint,
  SourcePoint: TCnEccPoint);
var
  T, X, DX: TCnBigNumber;
begin
  // 输入为射影 (X, Z)，先 x = (X/Z)，再求 y
  if SourcePoint.Y.IsZero then
  begin
    DestPoint.SetZero;
    Exit;
  end;

  T := nil;
  X := nil;
  DX := nil;

  try
    T := F25519BigNumberPool.Obtain;
    X := F25519BigNumberPool.Obtain;
    DX := F25519BigNumberPool.Obtain;

    BigNumberModularInverse(T, SourcePoint.Y, FFiniteFieldSize); // Z^-1
    BigNumberDirectMulMod(DX, SourcePoint.X, T, FFiniteFieldSize); // 算出 DX 但先不赋值避免影响

    BigNumberCopy(X, DX); // DestPoint.X = X/Z

    // 求 X^3+A*X^2+X mod P
    BigNumberPowerWordMod(X, DX, 3, FFiniteFieldSize);  // X^3

    BigNumberDirectMulMod(T, DX, DX, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientA, FFiniteFieldSize);  // A*X^2

    BigNumberAddMod(X, T, X, FFiniteFieldSize);
    BigNumberAddMod(X, X, DX, FFiniteFieldSize);  // 得到 X^3+A*X^2+X mod P

    BigNumberSquareRootModPrime(DestPoint.Y, X, FFiniteFieldSize);  // 求模平方根
    BigNumberCopy(DestPoint.X, DX);
  finally
    F25519BigNumberPool.Recycle(DX);
    F25519BigNumberPool.Recycle(X);
    F25519BigNumberPool.Recycle(T);
  end;
end;

procedure TCnMontgomeryCurve.Field64XAffinePointToPoint(DestPoint: TCnEccPoint;
  var SourcePoint: TCn25519Field64EccPoint);
var
  T: TCnEccPoint;
begin
  if DestPoint = nil then
    Exit;

  T := TCnEccPoint.Create;
  try
    Cn25519Field64ToBigNumber(T.X, SourcePoint.X);  // 多项式点转换为射影坐标 X Z 点
    Cn25519Field64ToBigNumber(T.Y, SourcePoint.Y);

    XAffinePointToPoint(DestPoint, T);   // 多项式点转换为射影坐标 X Z 点
  finally
    T.Free;
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderField64MultiplePoint(
  K: TCnBigNumber; var Point: TCn25519Field64EccPoint);
var
  I, C: Integer;
  X0, X1: TCn25519Field64EccPoint;
begin
  if BigNumberIsZero(K) then // 不考虑 K 为负值的情况
  begin
    Cn25519Field64Zero(Point.X);
    Cn25519Field64Zero(Point.Y);
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  Cn25519Field64EccPointCopy(X1, Point);
  MontgomeryLadderField64PointXDouble(X0, Point);

  C := K.GetBitsCount;
  for I := C - 2 downto 0 do // 内部先不考虑 Time Constant 执行时间固定的要求
  begin
    ConditionalSwapField64Point(K.IsBitSet(I + 1) <> K.IsBitSet(I), X0, X1); // 换

    MontgomeryLadderField64PointXAdd(X1, X0, X1, Point);
    MontgomeryLadderField64PointXDouble(X0, X0);
  end;

  ConditionalSwapField64Point(K.IsBitSet(0), X0, X1);
  Cn25519Field64EccPointCopy(Point, X0);
end;

procedure TCnMontgomeryCurve.MontgomeryLadderField64MultiplePoint(K: Int64;
  var Point: TCn25519Field64EccPoint);
var
  BK: TCnBigNumber;
begin
  BK := F25519BigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MontgomeryLadderField64MultiplePoint(BK, Point);
  finally
    F25519BigNumberPool.Recycle(BK);
  end;
end;

procedure TCnMontgomeryCurve.MontgomeryLadderField64PointXAdd(var Sum, P,
  Q, PMinusQ: TCn25519Field64EccPoint);
var
  V0, V1, V2, V3, V4: TCn25519Field64;
begin
  Cn25519Field64Add(V0, P.X, P.Y);
  Cn25519Field64Sub(V1, Q.X, Q.Y);
  Cn25519Field64Mul(V1, V1, V0);

  Cn25519Field64Sub(V0, P.X, P.Y);
  Cn25519Field64Add(V2, Q.X, Q.Y);
  Cn25519Field64Mul(V2, V2, V0);

  Cn25519Field64Add(V3, V1, V2);
  Cn25519Field64Mul(V3, V3, V3);

  Cn25519Field64Sub(V4, V1, V2);
  Cn25519Field64Mul(V4, V4, V4);

  Cn25519Field64Copy(V0, PMinusQ.X);   // V0 备份，避免 Sum 和 PMinusQ 是同一个点时被改动
  Cn25519Field64Mul(Sum.X, PMinusQ.Y, V3);
  Cn25519Field64Mul(Sum.Y, V0, V4);
end;

procedure TCnMontgomeryCurve.MontgomeryLadderField64PointXDouble(var Dbl,
  P: TCn25519Field64EccPoint);
var
  V1, V2, V3: TCn25519Field64;
begin
  CheckLadderConst;
  Cn25519Field64Add(V1, P.X, P.Y);
  Cn25519Field64Mul(V1, V1, V1);

  Cn25519Field64Sub(V2, P.X, P.Y);
  Cn25519Field64Mul(V2, V2, V2);

  Cn25519Field64Mul(Dbl.X, V1, V2);

  Cn25519Field64Sub(V1, V1, V2);
  Cn25519Field64Mul(V3, V1, FLadderField64);

  Cn25519Field64Add(V3, V3, V2);

  Cn25519Field64Mul(Dbl.Y, V1, V3);
end;

procedure TCnMontgomeryCurve.PointToField64XAffinePoint(
  var DestPoint: TCn25519Field64EccPoint; SourcePoint: TCnEccPoint);
var
  T: TCnEccPoint;
begin
  if SourcePoint = nil then
    Exit;

  T := TCnEccPoint.Create;
  try
    PointToXAffinePoint(T, SourcePoint); // 普通点转换为射影坐标 X Z 点

    Cn25519BigNumberToField64(DestPoint.X, T.X);    // 射影坐标 X Z 点转换为多项式点
    Cn25519BigNumberToField64(DestPoint.Y, T.Y);
  finally
    T.Free;
  end;
end;

procedure TCnMontgomeryCurve.XAffinePointInverse(P: TCnEccPoint);
begin
  // P 不用动
end;

{ TCnCurve25519 }

constructor TCnCurve25519.Create;
begin
  inherited;
  Load(SCN_25519_MONT_A, SCN_25519_MONT_B, SCN_25519_PRIME, SCN_25519_MONT_GX,
    SCN_25519_MONT_GY, SCN_25519_ORDER, SCN_25519_COFACTOR);
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

procedure TCnCurve25519.MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
var
  M: TCn25519Field64EccPoint;
begin
  PointToField64XAffinePoint(M, Point);
  MontgomeryLadderField64MultiplePoint(K, M);
  Field64XAffinePointToPoint(Point, M);
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

procedure TCnEd25519.ExtendedField64MultiplePoint(K: Int64;
  var Point: TCn25519Field64Ecc4Point);
var
  BK: TCnBigNumber;
begin
  BK := F25519BigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    ExtendedField64MultiplePoint(BK, Point);
  finally
    F25519BigNumberPool.Recycle(BK);
  end;
end;

procedure TCnEd25519.ExtendedField64MultiplePoint(K: TCnBigNumber;
  var Point: TCn25519Field64Ecc4Point);
var
  I: Integer;
  E, R: TCn25519Field64Ecc4Point;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    ExtendedField64PointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    Cn25519Field64Ecc4PointNeutual(Point);
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  // R 要是中性点
  Cn25519Field64Ecc4PointNeutual(R);
  Cn25519Field64Ecc4PointCopy(E, Point);

  for I := 0 to BigNumberGetBitsCount(K) - 1 do
  begin
    if BigNumberIsBitSet(K, I) then
      ExtendedField64PointAddPoint(R, E, R);
    ExtendedField64PointAddPoint(E, E, E);
  end;

  Cn25519Field64Ecc4PointCopy(Point, R);
end;

function TCnEd25519.ExtendedField64PointAddPoint(var P, Q,
  Sum: TCn25519Field64Ecc4Point): Boolean;
var
  A, B, C, D, E, F, G, H: TCn25519Field64;
  CoD: TCn25519Field64;
begin
  if Cn25519Field64Ecc4PointEqual(P, Q) then
  begin
    // 是同一个点
    Cn25519Field64Mul(A, P.X, P.X);   // A = X1^2
    Cn25519Field64Mul(B, P.Y, P.Y);   // B = Y1^2

    Cn25519Field64Mul(C, P.Z, P.Z);
    Cn25519Field64Add(C, C, C);       // C = 2*Z1^2

    Cn25519Field64Add(H, A, B);       // H = A+B

    Cn25519Field64Add(E, P.X, P.Y);
    Cn25519Field64Mul(E, E, E);
    Cn25519Field64Sub(E, H, E);       // E = H-(X1+Y1)^2

    Cn25519Field64Sub(G, A, B);       // G = A-B
    Cn25519Field64Add(F, C, G);       // F = C+G

    Cn25519Field64Mul(Sum.X, E, F);   // X3 = E*F
    Cn25519Field64Mul(Sum.Y, G, H);   // Y3 = G*H
    Cn25519Field64Mul(Sum.T, E, H);   // T3 = E*H
    Cn25519Field64Mul(Sum.Z, F, G);   // Z3 = F*G

    Result := True;
  end
  else
  begin
    // 不是同一个点。先用 G H 做临时变量
    Cn25519Field64Sub(G, P.Y, P.X);
    Cn25519Field64Sub(H, Q.Y, Q.X);
    Cn25519Field64Mul(A, G, H); // A = (Y1-X1)*(Y2-X2)

    Cn25519Field64Add(G, P.Y, P.X);
    Cn25519Field64Add(H, Q.Y, Q.X);
    Cn25519Field64Mul(B, G, H);  // B = (Y1+X1)*(Y2+X2)

    Cn25519BigNumberToField64(CoD, FCoefficientD);
    Cn25519Field64Add(C, CoD, CoD);
    Cn25519Field64Mul(C, P.T, C);
    Cn25519Field64Mul(C, Q.T, C);   // C = T1*2*d*T2

    Cn25519Field64Add(D, P.Z, P.Z);
    Cn25519Field64Mul(D, Q.Z, D);   // D = Z1*2*Z2

    Cn25519Field64Sub(E, B, A);   // E = B-A
    Cn25519Field64Sub(F, D, C);   // F = D-C
    Cn25519Field64Add(G, D, C);   // G = D+C
    Cn25519Field64Add(H, B, A);   // H = B+A

    Cn25519Field64Mul(Sum.X, E, F);   // X3 = E*F
    Cn25519Field64Mul(Sum.Y, G, H);   // Y3 = G*H
    Cn25519Field64Mul(Sum.T, E, H);   // T3 = E*H
    Cn25519Field64Mul(Sum.Z, F, G);   // Z3 = F*G

    Result := True;
  end;
end;

procedure TCnEd25519.ExtendedField64PointInverse(
  var P: TCn25519Field64Ecc4Point);
var
  T: TCn25519Field64;
begin
  // X -> Prime - X
  Cn25519Field64Sub(P.X, F25519Field64Zero, P.X);

  // T := X * Y / Z^3
  if Cn25519Field64Equal(P.Z, F25519Field64One) then
  begin
    // Z = 1 则直接乘
    Cn25519Field64Mul(P.T, P.X, P.Y);
  end
  else // Z <> 1 
  begin
    // 计算 Z^3 的模逆元
    Cn25519Field64Mul(T, P.Z, P.Z);
    Cn25519Field64Mul(T, T, P.Z);

    Cn25519Field64ModularInverse(T, T);

    // 再乘以 X * Y
    Cn25519Field64Mul(P.T, P.X, P.Y);
    Cn25519Field64Mul(P.T, P.T, T);
  end;
end;

function TCnEd25519.ExtendedField64PointSubPoint(var P, Q,
  Diff: TCn25519Field64Ecc4Point): Boolean;
var
  Inv: TCn25519Field64Ecc4Point;
begin
  Cn25519Field64Ecc4PointCopy(Inv, Q);
  ExtendedField64PointInverse(Inv);
  Result := ExtendedField64PointAddPoint(P, Inv, Diff);
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

procedure TCnEd25519.ExtendedPointAddPoint(P, Q, Sum: TCnEcc4Point);
var
  A, B, C, D, E, F, G, H: TCnBigNumber;
begin
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
      BigNumberDirectMulMod(A, P.X, P.X, FFiniteFieldSize); // A = X1^2
      BigNumberDirectMulMod(B, P.Y, P.Y, FFiniteFieldSize);  // B = Y1^2

      BigNumberDirectMulMod(C, P.Z, P.Z, FFiniteFieldSize);
      BigNumberAddMod(C, C, C, FFiniteFieldSize);      // C = 2*Z1^2

      BigNumberAddMod(H, A, B, FFiniteFieldSize);      // H = A+B

      BigNumberAddMod(E, P.X, P.Y, FFiniteFieldSize);
      BigNumberDirectMulMod(E, E, E, FFiniteFieldSize);
      BigNumberSubMod(E, H, E, FFiniteFieldSize);      // E = H-(X1+Y1)^2

      BigNumberSubMod(G, A, B, FFiniteFieldSize);      // G = A-B
      BigNumberAddMod(F, C, G, FFiniteFieldSize);      // F = C+G

      BigNumberDirectMulMod(Sum.X, E, F, FFiniteFieldSize);  // X3 = E*F
      BigNumberDirectMulMod(Sum.Y, G, H, FFiniteFieldSize);  // Y3 = G*H
      BigNumberDirectMulMod(Sum.T, E, H, FFiniteFieldSize);  // T3 = E*H
      BigNumberDirectMulMod(Sum.Z, F, G, FFiniteFieldSize);  // Z3 = F*G
    end
    else
    begin
      // 不是同一个点。先用 G H 做临时变量
      BigNumberSubMod(G, P.Y, P.X, FFiniteFieldSize);
      BigNumberSubMod(H, Q.Y, Q.X, FFiniteFieldSize);
      BigNumberDirectMulMod(A, G, H, FFiniteFieldSize); // A = (Y1-X1)*(Y2-X2)

      BigNumberAddMod(G, P.Y, P.X, FFiniteFieldSize);
      BigNumberAddMod(H, Q.Y, Q.X, FFiniteFieldSize);
      BigNumberDirectMulMod(B, G, H, FFiniteFieldSize);  // B = (Y1+X1)*(Y2+X2)

      BigNumberAdd(C, FCoefficientD, FCoefficientD);
      BigNumberDirectMulMod(C, P.T, C, FFiniteFieldSize);
      BigNumberDirectMulMod(C, Q.T, C, FFiniteFieldSize);  // C = T1*2*d*T2

      BigNumberAdd(D, P.Z, P.Z);
      BigNumberDirectMulMod(D, Q.Z, D, FFiniteFieldSize);  // D = Z1*2*Z2

      BigNumberSubMod(E, B, A, FFiniteFieldSize);  // E = B-A
      BigNumberSubMod(F, D, C, FFiniteFieldSize);  // F = D-C
      BigNumberAddMod(G, D, C, FFiniteFieldSize);  // G = D+C
      BigNumberAddMod(H, B, A, FFiniteFieldSize);  // H = B+A

      BigNumberDirectMulMod(Sum.X, E, F, FFiniteFieldSize);  // X3 = E*F
      BigNumberDirectMulMod(Sum.Y, G, H, FFiniteFieldSize);  // Y3 = G*H
      BigNumberDirectMulMod(Sum.T, E, H, FFiniteFieldSize);  // T3 = E*H
      BigNumberDirectMulMod(Sum.Z, F, G, FFiniteFieldSize);  // Z3 = F*G
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
    // x -> -x，意味着 X/Z -> P - X/Z，也就是 (P*Z - X)/Z，所以新 X = P*Z - X，前者是 0，因而还是 P - X
    BigNumberDirectMulMod(T, P.Z, FFiniteFieldSize, FFiniteFieldSize);
    BigNumberSubMod(P.X, T, P.X, FFiniteFieldSize); // 释放 T

    // T := X * Y / Z^3
    BigNumberPowerWordMod(T, P.Z, 3, FFiniteFieldSize);
    BigNumberModularInverse(T, T, FFiniteFieldSize); // T 是 Z^3 的逆元
    BigNumberDirectMulMod(P.T, P.X, P.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(P.T, P.T, T, FFiniteFieldSize);
  finally
    F25519BigNumberPool.Recycle(T);
  end;
end;

procedure TCnEd25519.ExtendedPointSubPoint(P, Q, Diff: TCnEcc4Point);
var
  Inv: TCnEcc4Point;
begin
  Inv := TCnEcc4Point.Create;
  try
    Inv.Assign(Q);
    ExtendedPointInverse(Inv);
    ExtendedPointAddPoint(P, Inv, Diff);
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
    CalcBigNumbersFromPrivateKey(PrivateKey, CN_25519_BLOCK_BYTESIZE, K, nil);

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
  // X1*Z2 = X2*Z1 且 Y1*Z2 = Y2*Z1
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

function CnEccPointToEcc4Point(DestPoint: TCnEcc4Point; SourcePoint: TCnEccPoint;
  Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not CnEccPointToEcc3Point(SourcePoint, DestPoint) then
    Exit;
  Result := BigNumberDirectMulMod(DestPoint.T, SourcePoint.X, SourcePoint.Y, Prime);
end;

function CnEcc4PointToEccPoint(DestPoint: TCnEccPoint; SourcePoint: TCnEcc4Point;
  Prime: TCnBigNumber): Boolean;
begin
  Result := CnAffinePointToEccPoint(SourcePoint, DestPoint, Prime);
end;

// =============================================================================
//
//          Curve25519 的 u v 和 Ed25519 的 x y 的双向映射关系为：
//
//              (u, v) = ((1+y)/(1-y), sqrt(-486664)*u/x)
//              (x, y) = (sqrt(-486664)*u/v, (u-1)/(u+1))
//
// =============================================================================

procedure CnCurve25519PointToEd25519Point(DestPoint, SourcePoint: TCnEccPoint);
var
  S, T, Inv, Prime, TX: TCnBigNumber;
begin
  // x = sqrt(-486664)*u/v
  // y = (u-1)/(u+1)

  S := nil;
  T := nil;
  Prime := nil;
  Inv := nil;
  TX := nil;

  try
    S := F25519BigNumberPool.Obtain;
    T := F25519BigNumberPool.Obtain;

    S.SetHex(SCN_25519_SQRT_NEG_486664);
    Prime := F25519BigNumberPool.Obtain;
    Prime.SetHex(SCN_25519_PRIME);

    BigNumberDirectMulMod(T, S, SourcePoint.X, Prime); // sqrt * u

    Inv := F25519BigNumberPool.Obtain;
    BigNumberModularInverse(Inv, SourcePoint.Y, Prime); // v^-1

    TX := F25519BigNumberPool.Obtain;
    BigNumberDirectMulMod(TX, T, Inv, Prime); // 算到 X，但先不赋值，避免源目标同对象造成影响

    BigNumberCopy(T, SourcePoint.X);
    BigNumberCopy(S, SourcePoint.X);

    T.SubWord(1);  // u - 1
    S.AddWord(1);  // u + 1

    BigNumberModularInverse(Inv, S, Prime); // (u + 1)^1
    BigNumberDirectMulMod(DestPoint.Y, T, Inv, Prime);
    BigNumberCopy(DestPoint.X, TX);
  finally
    F25519BigNumberPool.Recycle(TX);
    F25519BigNumberPool.Recycle(Inv);
    F25519BigNumberPool.Recycle(Prime);
    F25519BigNumberPool.Recycle(T);
    F25519BigNumberPool.Recycle(S);
  end;
end;

procedure CnEd25519PointToCurve25519Point(DestPoint, SourcePoint: TCnEccPoint);
var
  S, T, Inv, Prime, TX: TCnBigNumber;
begin
  // u = (1+y)/(1-y)
  // v = sqrt(-486664)*u/x

  S := nil;
  T := nil;
  Prime := nil;
  Inv := nil;
  TX := nil;

  try
    S := F25519BigNumberPool.Obtain;
    T := F25519BigNumberPool.Obtain;

    BigNumberCopy(T, SourcePoint.Y);
    BigNumberCopy(S, SourcePoint.Y);
    T.AddWord(1);  // T 是分子 1+y

    Prime := F25519BigNumberPool.Obtain;
    Prime.SetHex(SCN_25519_PRIME);

    BigNumberSubMod(S, CnBigNumberOne, SourcePoint.Y, Prime); // S 是分母 1-y

    Inv := F25519BigNumberPool.Obtain;
    BigNumberModularInverse(Inv, S, Prime); // Inv 是分母负倒数供乘

    TX := F25519BigNumberPool.Obtain;
    BigNumberDirectMulMod(TX, T, Inv, Prime); // 得到 U，但不赋值，先暂存，避免源目标同对象的影响

    S.SetHex(SCN_25519_SQRT_NEG_486664);
    BigNumberDirectMulMod(T, S, TX, Prime);

    BigNumberModularInverse(Inv, SourcePoint.X, Prime);
    BigNumberDirectMulMod(DestPoint.Y, T, Inv, Prime);

    BigNumberCopy(DestPoint.X, TX); // 将暂存的 TX 整回目标点
  finally
    F25519BigNumberPool.Recycle(TX);
    F25519BigNumberPool.Recycle(Inv);
    F25519BigNumberPool.Recycle(Prime);
    F25519BigNumberPool.Recycle(T);
    F25519BigNumberPool.Recycle(S);
  end;
end;

procedure CnEd25519PointToData(P: TCnEccPoint; var Data: TCnEd25519Data);
begin
  if P = nil then
    Exit;

  FillChar(Data[0], SizeOf(TCnEd25519Data), 0);
  P.Y.ToBinary(@Data[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd25519Data)); // 小端序，需要倒一下

  if P.X.IsOdd then // X 是奇数，最低位是 1
    Data[CN_25519_BLOCK_BYTESIZE - 1] := Data[CN_25519_BLOCK_BYTESIZE - 1] or $80  // 高位置 1
  else
    Data[CN_25519_BLOCK_BYTESIZE - 1] := Data[CN_25519_BLOCK_BYTESIZE - 1] and $7F; // 高位清 0
end;

procedure CnEd25519DataToPoint(Data: TCnEd25519Data; P: TCnEccPoint;
  out XOdd: Boolean);
var
  D: TCnEd25519Data;
begin
  if P = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@D[0], SizeOf(TCnEd25519Data));
  P.Y.SetBinary(@D[0], SizeOf(TCnEd25519Data));

  // 最高位是否是 0 表示了 X 的奇偶
  XOdd := P.Y.IsBitSet(8 * CN_25519_BLOCK_BYTESIZE - 1);

  // 最高位得清零
  P.Y.ClearBit(8 * CN_25519_BLOCK_BYTESIZE - 1);
end;

procedure CnEd25519BigNumberToData(N: TCnBigNumber; var Data: TCnEd25519Data);
begin
  if (N = nil) or (N.GetBytesCount > SizeOf(TCnEd25519Data)) then
    Exit;

  FillChar(Data[0], SizeOf(TCnEd25519Data), 0);
  N.ToBinary(@Data[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@Data[0], SizeOf(TCnEd25519Data));
end;

procedure CnEd25519DataToBigNumber(Data: TCnEd25519Data; N: TCnBigNumber);
var
  D: TCnEd25519Data;
begin
  if N = nil then
    Exit;

  Move(Data[0], D[0], SizeOf(TCnEd25519Data));
  ReverseMemory(@D[0], SizeOf(TCnEd25519Data));
  N.SetBinary(@D[0], SizeOf(TCnEd25519Data));
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
    CalcBigNumbersFromPrivateKey(PrivateKey, CN_25519_BLOCK_BYTESIZE, S, HP);

    // 杂凑前缀拼上原始文字
    Stream := TMemoryStream.Create;
    BigNumberWriteBinaryToStream(HP, Stream, CN_25519_BLOCK_BYTESIZE);
    Stream.Write(PlainData^, DataLen);

    // 计算出 SHA512 值作为 r 乘数，准备乘以基点作为 R 点
    Dig := SHA512Buffer(Stream.Memory, Stream.Size);

    ReverseMemory(@Dig[0], SizeOf(TSHA512Digest)); // 需要倒转一次
    R.SetBinary(@Dig[0], SizeOf(TSHA512Digest));
    BigNumberNonNegativeMod(R, R, Ed25519.Order);  // r 乘数太大先 mod 一下阶

    OutSignature.R.Assign(Ed25519.Generator);
    Ed25519.MultiplePoint(R, OutSignature.R);      // 计算得到签名值 R，该值是一个点坐标

    // 再 Hash 计算 S，先点 R 转换为字节数组
    Ed25519.PointToPlain(OutSignature.R, Data);

    // 拼起来
    Stream.Clear;
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));

    // 公钥点也转换为字节数组
    Ed25519.PointToPlain(PublicKey, Data);
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));

    // 写明文，拼凑完毕
    Stream.Write(PlainData^, DataLen);

    // 再次杂凑 R||PublicKey||明文
    Dig := SHA512Buffer(Stream.Memory, Stream.Size);

    ReverseMemory(@Dig[0], SizeOf(TSHA512Digest)); // 又需要倒转一次
    K.SetBinary(@Dig[0], SizeOf(TSHA512Digest));
    BigNumberNonNegativeMod(K, K, Ed25519.Order);  // 乘数太大再先 mod 一下阶

    // 计算乘数 R + K * S mod Order
    BigNumberDirectMulMod(OutSignature.S, K, S, Ed25519.Order);
    BigNumberAddMod(OutSignature.S, R, OutSignature.S, Ed25519.Order);

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

function CnEd25519VerifyData(PlainData: Pointer; DataLen: Integer;
  InSignature: TCnEd25519Signature; PublicKey: TCnEccPublicKey; Ed25519: TCnEd25519): Boolean;
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
    CnEd25519PointToData(InSignature.R, Data);
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));        // 拼 R 点

    CnEd25519PointToData(PublicKey, Data);
    Stream.Write(Data[0], SizeOf(TCnEd25519Data));        // 拼公钥点
    Stream.Write(PlainData^, DataLen);                    // 拼明文

    Dig := SHA512Buffer(Stream.Memory, Stream.Size);      // 计算 Hash 作为值
    ReverseMemory(@Dig[0], SizeOf(TSHA512Digest));        // 需要倒转一次

    T := F25519BigNumberPool.Obtain;
    T.SetBinary(@Dig[0], SizeOf(TSHA512Digest));
    T.MulWord(8);
    BigNumberNonNegativeMod(T, T, Ed25519.Order); // T 乘数太大先 mod 一下阶

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

function CnEd25519SignFile(const FileName: string; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; OutSignatureStream: TStream; Ed25519: TCnEd25519): Boolean;
var
  Stream: TMemoryStream;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) or (OutSignatureStream = nil)
    or not FileExists(FileName) then
    Exit;

  Stream := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    Sig := TCnEd25519Signature.Create;

    if CnEd25519SignData(Stream.Memory, Stream.Size, PrivateKey, PublicKey, Sig, Ed25519) then
    begin
      Sig.SaveToData(SigData);
      Result := OutSignatureStream.Write(SigData[0], SizeOf(TCnEd25519SignatureData))
        = SizeOf(TCnEd25519SignatureData);
    end;
  finally
    Sig.Free;
    Stream.Free;
  end;
end;

function CnEd25519VerifyFile(const FileName: string; InSignatureStream: TStream;
  PublicKey: TCnEccPublicKey; Ed25519: TCnEd25519 = nil): Boolean;
var
  Stream: TMemoryStream;
  Sig: TCnEd25519Signature;
  SigData: TCnEd25519SignatureData;
begin
  Result := False;
  if (PublicKey = nil) or (InSignatureStream = nil) or not FileExists(FileName) then
    Exit;

  Stream := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    if InSignatureStream.Read(SigData[0], SizeOf(TCnEd25519SignatureData)) <>
      SizeOf(TCnEd25519SignatureData) then
      Exit;

    Sig := TCnEd25519Signature.Create;
    Sig.LoadFromData(SigData);

    Result := CnEd25519VerifyData(Stream.Memory, Stream.Size, Sig, PublicKey, Ed25519);
  finally
    Sig.Free;
    Stream.Free;
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
    CnEcc4PointToEccPoint(Q, P, FFiniteFieldSize);
    Result := IsPointOnCurve(Q);
  finally
    Q.Free;
  end;
end;

function TCnEd25519.IsExtendedField64PointOnCurve(
  var P: TCn25519Field64Ecc4Point): Boolean;
var
  Q: TCnEccPoint;
begin
  Q := TCnEccPoint.Create;
  try
    CnField64Ecc4PointToEccPoint(Q, P);
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
  P4: TCn25519Field64Ecc4Point;
begin
  CnEccPointToField64Ecc4Point(P4, Point);
  ExtendedField64MultiplePoint(K, P4);
  CnField64Ecc4PointToEccPoint(Point, P4);
end;

procedure TCnEd25519.PlainToPoint(Plain: TCnEd25519Data;
  OutPoint: TCnEccPoint);
var
  XOdd: Boolean;
  T, Y, Inv: TCnBigNumber;
begin
  if OutPoint = nil then
    Exit;

  // 先从 Plain 中还原 Y 坐标以及 X 点的奇偶性
  CnEd25519DataToPoint(Plain, OutPoint, XOdd);

  // 得到 Y 后求解 x 的方程 x^2 = (Y^2 - 1) / (D*Y^2 + 1) mod P
  // 注意素数 25519 是 8u5 的形式

  T := nil;
  Y := nil;
  Inv := nil;

  try
    T := F25519BigNumberPool.Obtain;
    Y := F25519BigNumberPool.Obtain;

    BigNumberDirectMulMod(Y, OutPoint.Y, OutPoint.Y, FFiniteFieldSize);
    Y.SubWord(1); // Y := Y^2 - 1

    BigNumberDirectMulMod(T, OutPoint.Y, OutPoint.Y, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FCoefficientD, FFiniteFieldSize);
    T.AddWord(1); // T := D*Y^2 + 1

    Inv := F25519BigNumberPool.Obtain;
    BigNumberModularInverse(Inv, T, FFiniteFieldSize);

    BigNumberDirectMulMod(Y, Y, Inv, FFiniteFieldSize);  // Y 得到方程右边的值

    BigNumberSquareRootModPrime(OutPoint.X, Y, FFiniteFieldSize);

    // 算出 X 了
    if OutPoint.X.IsBitSet(0) <> XOdd then
      BigNumberSub(OutPoint.X, FFiniteFieldSize, OutPoint.X);
  finally
    F25519BigNumberPool.Recycle(Inv);
    F25519BigNumberPool.Recycle(Y);
    F25519BigNumberPool.Recycle(T);
  end;
end;

procedure TCnEd25519.PointToPlain(Point: TCnEccPoint;
  var OutPlain: TCnEd25519Data);
begin
  if (Point = nil) or (BigNumberCompare(Point.Y, FFiniteFieldSize) >= 0) then
    Exit;

  CnEd25519PointToData(Point, OutPlain);
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

procedure Cn25519BigNumberToField64(var Field: TCn25519Field64; const Num: TCnBigNumber);
var
  D: TCn25519Field64;
begin
  if Num.IsNegative or (BigNumberUnsignedCompare(Num, FPrime25519) > 0) then
    BigNumberNonNegativeMod(Num, Num, FPrime25519);

  // 如果 Num 是 SetHex 8888888877777777666666665555555544444444333333332222222211111111
  // 那么其真实值确实是 8888888877777777666666665555555544444444333333332222222211111111
  // 内存中低到高是 11111111 22222222 33333333 44444444 55555555 66666666 77777777 88888888
  // 共八组四字节，每组四字节内部按大小端不同有区别，但这里无需处理
  // 拆成 64 位的值则 D0=2222222211111111 D1=4444444433333333 D3=6666666655555555 D4=8888888877777777

  FillChar(D[0], SizeOf(TCn25519Field64), 0);
  BigNumberRawDump(Num, @D[0]);

  Field[0] := D[0] and $7FFFFFFFFFFFF;  // D0 保留低 51 位（0 到 50，与1）
  Field[1] := (D[0] shr 51) or ((D[1] and $3FFFFFFFFF) shl 13); // D0 的高 13 位（64 减 51）与 D1 的低 38 位（与1）拼起来
  Field[2] := (D[1] shr 38) or ((D[2] and $1FFFFFF) shl 26); // D1 的高 26 位（64 减 38）与 D2 的低 25 位（与1）拼起来
  Field[3] := (D[2] shr 25) or ((D[3] and $0FFF) shl 39); // D2 的高 39 位（64 减 25）与 D2 的低 12 位（与1）拼起来
  Field[4] := D[3] shr 12;                             // D3 的高 52 位（64 减 12）
end;

procedure Cn25519Field64ToBigNumber(const Res: TCnBigNumber; var Field: TCn25519Field64);
var
  B0, B1, B2, B3, B4: TCnBigNumber;
begin
  B0 := nil;
  B1 := nil;
  B2 := nil;
  B3 := nil;
  B4 := nil;

  try
    B0 := F25519BigNumberPool.Obtain;
    B1 := F25519BigNumberPool.Obtain;
    B2 := F25519BigNumberPool.Obtain;
    B3 := F25519BigNumberPool.Obtain;
    B4 := F25519BigNumberPool.Obtain;

    B0.SetInt64(Field[0]);
    B1.SetInt64(Field[1]);
    B2.SetInt64(Field[2]);
    B3.SetInt64(Field[3]);
    B4.SetInt64(Field[4]);

    B1.ShiftLeft(51);
    B2.ShiftLeft(102);
    B3.ShiftLeft(153);
    B4.ShiftLeft(204);

    Res.SetZero;
    BigNumberAdd(Res, B1, B0);
    BigNumberAdd(Res, Res, B2);
    BigNumberAdd(Res, Res, B3);
    BigNumberAdd(Res, Res, B4);

    BigNumberNonNegativeMod(Res, Res, FPrime25519);
  finally
    F25519BigNumberPool.Recycle(B4);
    F25519BigNumberPool.Recycle(B3);
    F25519BigNumberPool.Recycle(B2);
    F25519BigNumberPool.Recycle(B1);
    F25519BigNumberPool.Recycle(B0);
  end;
end;

procedure Cn25519Field64Reduce(var Field: TCn25519Field64);
var
  C: TCn25519Field64;
begin
  C[0] := Field[0] shr 51;
  C[1] := Field[1] shr 51;
  C[2] := Field[2] shr 51;
  C[3] := Field[3] shr 51;
  C[4] := Field[4] shr 51;

  Field[0] := Field[0] and SCN_LOW51_MASK;
  Field[1] := Field[1] and SCN_LOW51_MASK;
  Field[2] := Field[2] and SCN_LOW51_MASK;
  Field[3] := Field[3] and SCN_LOW51_MASK;
  Field[4] := Field[4] and SCN_LOW51_MASK;

  Field[0] := Field[0] + C[4] * 19; // 最高位的进位被 mod 后剩下的搁在最低位
  Field[1] := Field[1] + C[0];
  Field[2] := Field[2] + C[1];
  Field[3] := Field[3] + C[2];
  Field[4] := Field[4] + C[3];
end;

function Cn25519Field64ToHex(var Field: TCn25519Field64): string;
begin
  Result := '$' + UInt64ToHex(Field[0]) + ' $' + UInt64ToHex(Field[1]) + ' $' +
    UInt64ToHex(Field[2]) + ' $'+ UInt64ToHex(Field[3]) + ' $' + UInt64ToHex(Field[4]);
end;

procedure Cn25519Field64Copy(var Dest, Source: TCn25519Field64);
begin
  Move(Source[0], Dest[0], SizeOf(TCn25519Field64));
end;

function Cn25519Field64Equal(var A, B: TCn25519Field64): Boolean;
begin
  Result := (A[0] = B[0]) and (A[1] = B[1]) and (A[2] = B[2])
    and (A[3] = B[3]) and (A[4] = B[4]);
  // 只简单判别对应值，不做 Reduce 判断

//  if not Result then
//  begin
//    Cn25519Field64Copy(T1, A);
//    Cn25519Field64Copy(T2, B);
//
//    Cn25519Field64Reduce(T1);
//    Cn25519Field64Reduce(T2);
//    Result := (T1[0] = T2[0]) and (T1[1] = T2[1]) and (T1[2] = T2[2])
//      and (T1[3] = T2[3]) and (T1[4] = T2[4]);
//  end;
end;

procedure Cn25519Field64Swap(var A, B: TCn25519Field64);
var
  I: Integer;
  T: TUInt64;
begin
  for I := Low(TCn25519Field64) to High(TCn25519Field64) do
  begin
    T := A[I];
    A[I] := B[I];
    B[I] := T;
  end;
end;

procedure Cn25519Field64Zero(var Field: TCn25519Field64);
begin
  Move(F25519Field64Zero[0], Field[0], SizeOf(TCn25519Field64));
end;

procedure Cn25519Field64One(var Field: TCn25519Field64);
begin
  Move(F25519Field64One[0], Field[0], SizeOf(TCn25519Field64));
end;

procedure Cn25519Field64NegOne(var Field: TCn25519Field64);
begin
  Move(F25519Field64NegOne[0], Field[0], SizeOf(TCn25519Field64));
end;

procedure Cn25519Field64Negate(var Field: TCn25519Field64);
begin
  Field[0] := 36028797018963664 - Field[0];
  Field[1] := 36028797018963952 - Field[1];
  Field[2] := 36028797018963952 - Field[2];
  Field[3] := 36028797018963952 - Field[3];
  Field[4] := 36028797018963952 - Field[4];
  Cn25519Field64Reduce(Field);
end;

procedure Cn25519Field64Add(var Res, A, B: TCn25519Field64);
var
  I: Integer;
begin
  for I := Low(TCn25519Field64) to High(TCn25519Field64) do
    Res[I] := A[I] + B[I];
end;

procedure Cn25519Field64Sub(var Res, A, B: TCn25519Field64);
begin
  Res[0] := A[0] + 36028797018963664 - B[0];
  Res[1] := A[1] + 36028797018963952 - B[1];
  Res[2] := A[2] + 36028797018963952 - B[2];
  Res[3] := A[3] + 36028797018963952 - B[3];
  Res[4] := A[4] + 36028797018963952 - B[4];
  Cn25519Field64Reduce(Res);
end;

procedure Cn25519Field64Mul(var Res, A, B: TCn25519Field64);
var
  B1, B2, B3, B4, C: TUInt64;
  C0, C1, C2, C3, C4, T: TCnUInt128;
begin
  B1 := B[1] * 19;
  B2 := B[2] * 19;
  B3 := B[3] * 19;
  B4 := B[4] * 19;

  UInt128SetZero(C0);
  // c0 = m(a[0],b[0]) + m(a[4],b1_19) + m(a[3],b2_19) + m(a[2],b3_19) + m(a[1],b4_19);
  UInt64MulUInt64(A[0], B[0], T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[4], B1, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[3], B2, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[2], B3, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);
  UInt64MulUInt64(A[1], B4, T.Lo64, T.Hi64);
  UInt128Add(C0, C0, T);

  UInt128SetZero(C1);
  // c1 = m(a[1],b[0]) + m(a[0],b[1])  + m(a[4],b2_19) + m(a[3],b3_19) + m(a[2],b4_19);
  UInt64MulUInt64(A[1], B[0], T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[0], B[1], T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[4], B2, T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[3], B3, T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);
  UInt64MulUInt64(A[2], B4, T.Lo64, T.Hi64);
  UInt128Add(C1, C1, T);

  UInt128SetZero(C2);
  // c2 = m(a[2],b[0]) + m(a[1],b[1])  + m(a[0],b[2])  + m(a[4],b3_19) + m(a[3],b4_19);
  UInt64MulUInt64(A[2], B[0], T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[1], B[1], T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[0], B[2], T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[4], B3, T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);
  UInt64MulUInt64(A[3], B4, T.Lo64, T.Hi64);
  UInt128Add(C2, C2, T);

  UInt128SetZero(C3);
  // c3 = m(a[3],b[0]) + m(a[2],b[1])  + m(a[1],b[2])  + m(a[0],b[3])  + m(a[4],b4_19);
  UInt64MulUInt64(A[3], B[0], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[2], B[1], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[1], B[2], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[0], B[3], T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);
  UInt64MulUInt64(A[4], B4, T.Lo64, T.Hi64);
  UInt128Add(C3, C3, T);

  UInt128SetZero(C4);
  // c4 = m(a[4],b[0]) + m(a[3],b[1])  + m(a[2],b[2])  + m(a[1],b[3])  + m(a[0],b[4]);
  UInt64MulUInt64(A[4], B[0], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[3], B[1], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[2], B[2], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[1], B[3], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);
  UInt64MulUInt64(A[0], B[4], T.Lo64, T.Hi64);
  UInt128Add(C4, C4, T);

  // 拼结果
  UInt128Copy(T, C0);
  UInt128ShiftRight(T, 51);
  UInt128Add(C1, T.Lo64);
  Res[0] := C0.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C1);
  UInt128ShiftRight(T, 51);
  UInt128Add(C2, T.Lo64);
  Res[1] := C1.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C2);
  UInt128ShiftRight(T, 51);
  UInt128Add(C3, T.Lo64);
  Res[2] := C2.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C3);
  UInt128ShiftRight(T, 51);
  UInt128Add(C4, T.Lo64);
  Res[3] := C3.Lo64 and SCN_LOW51_MASK;

  UInt128Copy(T, C4);
  UInt128ShiftRight(T, 51);
  C := T.Lo64;
  Res[4] := C4.Lo64 and SCN_LOW51_MASK;

  Res[0] := Res[0] + C * 19;
  Res[1] := Res[1] + (Res[0] shr 51);

  Res[0] := Res[0] and SCN_LOW51_MASK;
end;

procedure Cn25519Field64Power(var Res, A: TCn25519Field64; K: Cardinal);
var
  T: TCn25519Field64;
begin
  if K = 0 then
    Cn25519Field64One(Res)
  else if K = 1 then
    Cn25519Field64Copy(Res, A)
  else
  begin
    Cn25519Field64Copy(T, A);
    Cn25519Field64One(Res);

    while K > 0 do
    begin
      if (K and 1) <> 0 then
        Cn25519Field64Mul(Res, Res, T);

      K := K shr 1;
      Cn25519Field64Mul(T, T, T);
    end;
  end;
end;

procedure Cn25519Field64Power(var Res, A: TCn25519Field64; K: TCnBigNumber);
var
  T: TCn25519Field64;
  I, B: Integer;
begin
  if K.IsZero then
    Cn25519Field64One(Res)
  else if K.IsOne then
    Cn25519Field64Copy(Res, A)
  else
  begin
    Cn25519Field64Copy(T, A);
    Cn25519Field64One(Res);

    B := K.GetBitsCount;
    for I := 0 to B - 1 do
    begin
      if K.IsBitSet(I) then
        Cn25519Field64Mul(Res, Res, T);
      Cn25519Field64Mul(T, T, T);
    end;
  end;
end;

procedure Cn25519Field64Power2K(var Res, A: TCn25519Field64; K: Cardinal);
begin
  Cn25519Field64Copy(Res, A);
  if K = 0 then
    Exit;

  while K > 0 do
  begin
    Cn25519Field64Mul(Res, Res, Res);
    Dec(K);
  end;
end;

procedure Cn25519Field64ModularInverse(var Res, A: TCn25519Field64);
var
  P: TCnBigNumber;
begin
  // 用费马小定理，求 A 的 P - 2 次方
  P := F25519BigNumberPool.Obtain;
  try
    BigNumberCopy(P, FPrime25519);
    P.SubWord(2);

    Cn25519Field64Power(Res, A, P);
  finally
    F25519BigNumberPool.Recycle(P);
  end;
end;

// =========================== 多项式点处理函数 ================================

procedure Cn25519Field64EccPointZero(var Point: TCn25519Field64EccPoint);
begin
  Cn25519Field64Zero(Point.X);
  Cn25519Field64Zero(Point.Y);
end;

procedure Cn25519Field64EccPointCopy(var DestPoint, SourcePoint: TCn25519Field64EccPoint);
begin
  Cn25519Field64Copy(DestPoint.X, SourcePoint.X);
  Cn25519Field64Copy(DestPoint.Y, SourcePoint.Y);
end;

function Cn25519Field64EccPointToHex(var Point: TCn25519Field64EccPoint): string;
begin
  Result := 'X: ' + Cn25519Field64ToHex(Point.X) + ' Y: ' + Cn25519Field64ToHex(Point.Y);
end;

function Cn25519Field64EccPointEqual(var A, B: TCn25519Field64EccPoint): Boolean;
begin
  Result := Cn25519Field64Equal(A.X, B.X) and  Cn25519Field64Equal(A.Y, B.Y);
end;

procedure Cn25519Field64Ecc4PointNeutual(var Point: TCn25519Field64Ecc4Point);
begin
  Cn25519Field64Zero(Point.X);
  Cn25519Field64One(Point.Y);
  Cn25519Field64One(Point.Z);
  Cn25519Field64Zero(Point.T);
end;

procedure Cn25519Field64Ecc4PointCopy(var DestPoint, SourcePoint: TCn25519Field64Ecc4Point);
begin
  Cn25519Field64Copy(DestPoint.X, SourcePoint.X);
  Cn25519Field64Copy(DestPoint.Y, SourcePoint.Y);
  Cn25519Field64Copy(DestPoint.Z, SourcePoint.Z);
  Cn25519Field64Copy(DestPoint.T, SourcePoint.T);
end;

function Cn25519Field64Ecc4PointToHex(var Point: TCn25519Field64Ecc4Point): string;
begin
  Result := 'X: ' + Cn25519Field64ToHex(Point.X) + ' Y: ' + Cn25519Field64ToHex(Point.Y)
    + ' Z: ' + Cn25519Field64ToHex(Point.Z) + ' T: ' + Cn25519Field64ToHex(Point.T);
end;

function Cn25519Field64Ecc4PointEqual(var A, B: TCn25519Field64Ecc4Point): Boolean;
var
  T1, T2: TCn25519Field64;
begin
  // X1Z2 = X2Z1 且 Y1Z2 = Y2Z1
  Result := False;

  Cn25519Field64Mul(T1, A.X, B.Z);
  Cn25519Field64Mul(T2, B.X, A.Z);

  if not Cn25519Field64Equal(T1, T2) then
    Exit;

  Cn25519Field64Mul(T1, A.Y, B.Z);
  Cn25519Field64Mul(T2, B.Y, A.Z);

  if not Cn25519Field64Equal(T1, T2) then
    Exit;

  Result := True;
end;

function CnEccPointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEccPoint): Boolean;
var
  P4: TCnEcc4Point;
begin
  P4 := TCnEcc4Point.Create;
  try
    CnEccPointToEcc4Point(P4, SourcePoint, FPrime25519);
    Result := CnEcc4PointToField64Ecc4Point(DestPoint, P4);
  finally
    P4.Free;
  end;
end;

function CnField64Ecc4PointToEccPoint(DestPoint: TCnEccPoint;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
var
  P4: TCnEcc4Point;
begin
  P4 := TCnEcc4Point.Create;
  try
    CnField64Ecc4PointToEcc4Point(P4, SourcePoint);
    Result := CnEcc4PointToEccPoint(DestPoint, P4, FPrime25519);
  finally
    P4.Free;
  end;
end;

function CnEcc4PointToField64Ecc4Point(var DestPoint: TCn25519Field64Ecc4Point;
  SourcePoint: TCnEcc4Point): Boolean;
begin
  Cn25519BigNumberToField64(DestPoint.X, SourcePoint.X);
  Cn25519BigNumberToField64(DestPoint.Y, SourcePoint.Y);
  Cn25519BigNumberToField64(DestPoint.Z, SourcePoint.Z);
  Cn25519BigNumberToField64(DestPoint.T, SourcePoint.T);
  Result := True;
end;

function CnField64Ecc4PointToEcc4Point(DestPoint: TCnEcc4Point;
  var SourcePoint: TCn25519Field64Ecc4Point): Boolean;
begin
  Cn25519Field64ToBigNumber(DestPoint.X, SourcePoint.X);
  Cn25519Field64ToBigNumber(DestPoint.Y, SourcePoint.Y);
  Cn25519Field64ToBigNumber(DestPoint.Z, SourcePoint.Z);
  Cn25519Field64ToBigNumber(DestPoint.T, SourcePoint.T);
  Result := True;
end;

initialization
  F25519BigNumberPool := TCnBigNumberPool.Create;
  FPrime25519 := TCnBigNumber.FromHex(SCN_25519_PRIME);

finalization
  FPrime25519.Free;
  F25519BigNumberPool.Free;

end.
