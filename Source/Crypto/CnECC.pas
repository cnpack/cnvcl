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

unit CnECC;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：魏尔斯特拉斯（Weierstrass）椭圆曲线算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 Int64 范围内以及大整数形式的形如 y^2 = x^3 + Ax + B mod p
*           这类魏尔斯特拉斯（Weierstrass）椭圆曲线的计算，如点乘、签名等，x 和 y 限于有限素域。
*           并实现了较慢的椭圆曲线求阶算法 Schoof。
*           椭圆曲线的公私钥允许从 PEM 或二进制等格式的文件或流中加载保存。
*
*           概念：椭圆曲线的阶是曲线上的总点数（似乎不包括无限远点）
*           基点的阶是基点标量乘多少等于无限远点。两者是倍数整除关系，可能相等。
*
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行，注意部分辅助函数缺乏固定长度处理，待修正，但 ASN.1 包装可无需指定固定长度
* 本 地 化：该单元无需本地化处理
* 修改记录：2024.04.14 V2.6
*               加入一根 384 及一根 512 曲线
*           2024.02.07 V2.5
*               ASN1 输出签名时允许指定固定字节长度，可避免不同的签名长度不同
*           2023.12.16 V2.4
*               修正 PKCS1 格式的 ECC PEM 格式解析问题，PKCS8 暂不支持
*           2023.06.24 V2.3
*               补上完整的 Fast Schoof 实现但验证未通过，不能使用
*           2023.05.28 V2.2
*               能够计算 Int64 型椭圆曲线的判别式与 j 不变量
*           2022.11.01 V2.1
*               增加校验公私钥是否配对的函数
*           2022.06.10 V2.0
*               点乘改为默认使用仿射坐标以加速，点加不变
*           2021.12.22 V1.9
*               增加 In64 与大数范围内仿射坐标与雅可比坐标的加减法与乘法
*           2021.12.07 V1.8
*               增加 SM9 与 WAPI 的两条曲线定义
*           2020.11.13 V1.7
*               实现大数范围内的基础 Schoof 算法并初步小范围测试通过，
*               支持 Unicode 版编译器、支持 Win64，但大范围无从验证
*           2020.10.25 V1.6
*               实现 Int64 范围内的基础 Schoof 算法并初步测试通过
*           2020.04.06 V1.5
*               实现 ECC 签名验签，类似于 openssl 的功能
*               openssl dgst -sha256 -sign ec.pem -out hello.sig hello
*               openssl dgst -sha256 -verify ecpub.pem -signature hello.sig hello
*               注意 Ecc 的签名只针对消息 Hash，输出中不带 Hash 算法种类与用户信息，
*               与 SM2 规范不同，与 RSA 的 Hash 后补 Hash 种类再对齐成 BER 内容也不同
*           2020.03.28 V1.4
*               实现 ECC 公私钥 PEM 文件的生成与读写，类似于 openssl 的功能
*               openssl ecparam -name secp256k1 -genkey -out ec_pkcs1.pem
*                    // PKCS#1 格式的公私钥
*               openssl pkcs8 -topk8 -inform PEM -in ec_pkcs1.pem -outform PEM -nocrypt -out ec_pkcs8.pem
*                    // PKCS#8 格式的公私钥
*               openssl ec -in ec.pem -pubout -out ecpub_pkcs1.pem
*                    // PKCS#1 格式的公钥
*               openssl ec -in ec_pkcs8.pem -outform PEM -pubout -out ecpub_pkcs8.pem
*                    // PKCS#8 格式的公钥
*           2018.09.29 V1.3
*               实现大数椭圆曲线根据 X 求 Y 的两种算法，并默认用速度更快的 Lucas
*           2018.09.13 V1.2
*               初步实现大数椭圆曲线的加解密功能，支持 SM2 以及 Secp256k1 等曲线
*           2018.09.10 V1.1
*               能够生成系数很小的椭圆曲线参数
*           2018.09.05 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$DEFINE USE_LUCAS}
// 定义此条件，求点也就是根据 X 计算椭圆曲线方程的 Y 值时使用 Lucas 序列算法来计算
// 如不定义，则使用 Tonelli-Shanks 算法计算。Tonelli-Shanks 速度较慢，大数范围内
// 比起 Lucas 序列慢 10 倍以上。

uses
  SysUtils, Classes, Contnrs, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  CnNative, CnPrime, CnBigNumber, CnMatrix,
  CnPolynomial, CnPemUtils, CnBerUtils, CnMD5, CnSHA1, CnSHA2, CnSM3;

const

  CN_OID_EC_PUBLIC_KEY: array [0..6] of Byte = (               // 1.2.840.10045.2.1
    $2A, $86, $48, $CE, $3D, $02, $01
  );
  {* ecPublicKey 的 OID}

type
  TCnEccSignDigestType = (esdtMD5, esdtSHA1, esdtSHA256, esdtSM3);
  {* ECC 签名所支持的数字摘要算法，不支持无摘要的方式}

  ECnEccException = class(Exception);
  {* 椭圆曲线相关异常}

  TCnInt64EccPoint = packed record
  {* Int64 范围内的椭圆曲线上的坐标点描述结构}
    X: Int64;
    Y: Int64;
  end;

  TCnInt64Ecc3Point = packed record
  {* Int64 范围内的射影/仿射/雅可比坐标点的描述结构，用于内部计算提速。Z = 0 时表示无穷远点也就是 0 点}
    X: Int64;
    Y: Int64;
    Z: Int64;
  end;

  TCnInt64PublicKey = TCnInt64EccPoint;
  {* Int64 范围内的椭圆曲线的公钥，G 点计算 k 次后的坐标点}

  TCnInt64PrivateKey = Int64;
  {* Int64 范围内的椭圆曲线的私钥，计算次数 k 次}

  TCnInt64Ecc = class
  {* 描述一有限素域 p 也就是 0 到 p - 1 上的椭圆曲线 y^2 = x^3 + Ax + B mod p，参数均在 Int64 范围内}
  private
    FGenerator: TCnInt64EccPoint;
    FCoefficientA: Int64;
    FCoefficientB: Int64;
    FFiniteFieldSize: Int64;
    FOrder: Int64;
    FSizeUFactor: Int64;
    FSizePrimeType: TCnPrimeType;
    F2Inverse: Int64;               // 2 针对 FFiniteFieldSize 的模逆元，供雅可比坐标计算
    function GetJInvariance: Int64;
    function GetDelta: Int64;
  protected
    function TonelliShanks(X: Int64; P: Int64; out Y: Int64): Boolean;
    {* Tonelli-Shanks 模素数二次剩余求解，返回 False 表示失败，调用者需自行保证 P 为素数}

    function Lucas(X: Int64; P: Int64; out Y: Int64): Boolean;
    {* Lucas 序列模素数二次剩余求解，返回 False 表示失败，只针对 P 为 8*u + 1 的形式}
  public
    constructor Create(A: Int64; B: Int64; FieldPrime: Int64; GX: Int64; GY: Int64; Order: Int64);
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、G 点坐标、G 点的阶数。

       参数：
         A: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         FieldPrime: Int64                - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         GX: Int64                        - 魏尔斯特拉斯椭圆曲线方程的 G 点的 X 坐标
         GY: Int64                        - 魏尔斯特拉斯椭圆曲线方程的 G 点的 Y 坐标
         Order: Int64                     - 魏尔斯特拉斯椭圆曲线方程的 G 点的阶

       返回值：TCnInt64Ecc                - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure AffinePointAddPoint(var P: TCnInt64Ecc3Point;
      var Q: TCnInt64Ecc3Point; var Sum: TCnInt64Ecc3Point);
    {* 使用仿射坐标系进行点加，避免取模逆元导致的开销。

       参数：
         var P: TCnInt64Ecc3Point         - 第一个加数的点坐标
         var Q: TCnInt64Ecc3Point         - 第二个加数的点坐标
         var Sum: TCnInt64Ecc3Point       - 输出的和的点坐标

       返回值：（无）
    }

    procedure JacobianPointAddPoint(var P: TCnInt64Ecc3Point;
      var Q: TCnInt64Ecc3Point; var Sum: TCnInt64Ecc3Point);
    {* 使用雅可比坐标系进行点加，避免取模逆元导致的开销。

       参数：
         var P: TCnInt64Ecc3Point         - 第一个加数的点坐标
         var Q: TCnInt64Ecc3Point         - 第二个加数的点坐标
         var Sum: TCnInt64Ecc3Point       - 输出的和的点坐标

       返回值：（无）
    }

    procedure AffineMultiplePoint(K: Int64; var Point: TCnInt64Ecc3Point);
    {* 使用仿射坐标系进行点乘，避免取模导致的开销。

       参数：
         K: Int64                         - 乘数
         var Point: TCnInt64Ecc3Point     - 被乘的坐标点

       返回值：（无）
    }

    procedure JacobianMultiplePoint(K: Int64; var Point: TCnInt64Ecc3Point);
    {* 使用雅可比坐标系进行点乘，避免取模导致的开销。

       参数：
         K: Int64                         - 乘数
         var Point: TCnInt64Ecc3Point     - 被乘的坐标点

       返回值：（无）
    }

    procedure MultiplePoint(K: Int64; var Point: TCnInt64EccPoint);
    {* 计算某点 P 的 k * P 值，值重新放入 P。

       参数：
         K: Int64                         - 乘数
         var Point: TCnInt64EccPoint      - 被乘的坐标点

       返回值：（无）
    }

    procedure PointAddPoint(var P: TCnInt64EccPoint; var Q: TCnInt64EccPoint; var Sum: TCnInt64EccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同。

       参数：
         var P: TCnInt64EccPoint          - 第一个加数的点坐标
         var Q: TCnInt64EccPoint          - 第二个加数的点坐标
         var Sum: TCnInt64EccPoint        - 输出的和的点坐标

       返回值：（无）
    }

    procedure PointSubPoint(var P: TCnInt64EccPoint; var Q: TCnInt64EccPoint; var Diff: TCnInt64EccPoint);
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同。

       参数：
         var P: TCnInt64EccPoint          - 被减数的点坐标
         var Q: TCnInt64EccPoint          - 减数的点坐标
         var Diff: TCnInt64EccPoint       - 输出的差的点坐标

       返回值：（无）
    }

    procedure PointInverse(var P: TCnInt64EccPoint);
    {* 计算 P 点的加法逆元 -P，值重新放入 P。

       参数：
         var P: TCnInt64EccPoint          - 待取逆元的坐标点

       返回值：（无）
    }

    procedure AffinePointInverse(var P: TCnInt64Ecc3Point);
    {* 计算以仿射坐标表示的 P 点的加法逆元 -P，值重新放入 P。

       参数：
         var P: TCnInt64Ecc3Point         - 待取逆元的坐标点

       返回值：（无）
    }

    procedure JacobianPointInverse(var P: TCnInt64Ecc3Point);
    {* 计算以雅可比坐标表示的 P 点的加法逆元 -P，值重新放入 P。

       参数：
         var P: TCnInt64Ecc3Point         - 待取逆元的坐标点

       返回值：（无）
    }

    function IsPointOnCurve(var P: TCnInt64EccPoint): Boolean;
    {* 判断 P 点是否在本曲线上。

       参数：
         var P: TCnInt64EccPoint          - 待判断的坐标点

       返回值：Boolean                    - 返回是否在曲线上
    }

    function DivisionPolynomial(Degree: Integer; outDivisionPolynomial: TCnInt64Polynomial): Boolean;
    {* 递归计算第 Degree 个可除多项式，返回计算是否成功。

       参数：
         Degree: Integer                                  - 可除多项式的序号
         outDivisionPolynomial: TCnInt64Polynomial        - 返回的可除多项式

       返回值：Boolean                                    - 返回计算是否成功
    }

    function PlainToPoint(Plain: Int64; var OutPoint: TCnInt64EccPoint): Boolean;
    {* 将要加密的明文数值包装成一个待加密的点，也就是以明文为 X 求方程的 Y。
       注意 Plain 为 0 时直接对应至零点，即使椭圆曲线上有（0, 非零 Y）形式的合法点存在。

       参数：
         Plain: Int64                     - 待加密的明文数
         var OutPoint: TCnInt64EccPoint   - 输出的明文坐标点

       返回值：Boolean                    - 返回求解是否成功
    }

    function PointToPlain(var Point: TCnInt64EccPoint): Int64;
    {* 将解密出的明文点解开成一个明文数值，也就是将点的 X 值取出。

       参数：
         Point: TCnInt64EccPoint          - 待解开的明文坐标点

       返回值：Int64                      - 返回明文数值
    }

    procedure GenerateKeys(out PrivateKey: TCnInt64PrivateKey; out PublicKey: TCnInt64PublicKey);
    {* 生成一对该椭圆曲线的公私钥，私钥是运算次数 k，公钥是基点 G 经过 k 次乘法后得到的点坐标 K。

       参数：
         out PrivateKey: TCnInt64PrivateKey               - 生成的椭圆曲线的私钥
         out PublicKey: TCnInt64PublicKey                 - 生成的椭圆曲线的公钥

       返回值：（无）
    }

    procedure Encrypt(var PlainPoint: TCnInt64EccPoint; PublicKey: TCnInt64PublicKey;
      var OutDataPoint1: TCnInt64EccPoint; var OutDataPoint2: TCnInt64EccPoint; RandomKey: Int64 = 0);
    {* 公钥加密明文点 M，得到两个点的输出密文，内部包含了随机值 r，也就是 C1 = M + rK; C2 = r * G。
       如果传入的 RandomKey 是 0，则内部随机生成。

       参数：
         var PlainPoint: TCnInt64EccPoint                 - 待加密的明文坐标点
         PublicKey: TCnInt64PublicKey                     - 用于加密的椭圆曲线公钥
         var OutDataPoint1: TCnInt64EccPoint              - 输出密文坐标点一
         var OutDataPoint2: TCnInt64EccPoint              - 输出密文坐标点二
         RandomKey: Int64                                 - 随机数

       返回值：（无）
    }

    procedure Decrypt(var DataPoint1: TCnInt64EccPoint; var DataPoint2: TCnInt64EccPoint;
      PrivateKey: TCnInt64PrivateKey; var OutPlainPoint: TCnInt64EccPoint);
    {* 私钥解密密文点，也就是计算 C1 - k * C2 就得到了原文点 M。

       参数：
         var DataPoint1: TCnInt64EccPoint                 - 待解密的密文坐标点一
         var DataPoint2: TCnInt64EccPoint                 - 待解密的密文坐标点二
         PrivateKey: TCnInt64PrivateKey                   - 用于解密的椭圆曲线私钥
         var OutPlainPoint: TCnInt64EccPoint              - 输出明文坐标点

       返回值：（无）
    }

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

    property Delta: Int64 read GetDelta;
    {* 判别式}
    property JInvariance: Int64 read GetJInvariance;
    {* j 不变量}
  end;

  TCnEcc = class;

  TCnEccPoint = class(TPersistent)
  {* 有限素域上的椭圆曲线上的点描述类}
  private
    FY: TCnBigNumber;
    FX: TCnBigNumber;
    procedure SetX(const Value: TCnBigNumber);
    procedure SetY(const Value: TCnBigNumber);
  public
    constructor Create; overload;
    {* 构造函数}
    constructor Create(const XDec: AnsiString; const YDec: AnsiString); overload;
    {* 构造函数

       参数：
         const XDec: AnsiString           - X 坐标的十进制字符串
         const YDec: AnsiString           - Y 坐标的十进制字符串

       返回值：TCnEccPoint                - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent              - 欲从之赋值的源对象

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 是否为无穷远点也即 0 点。

       参数：
         （无）

       返回值：Boolean                    - 返回是否无穷远点
    }

    procedure SetZero;
    {* 设为无穷远点也即 0 点}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 转换为字符串，简单输出用逗号分隔的十六进制 X 和 Y 坐标值。

       参数：
         （无）

       返回值：string                     - 返回十六进制字符串
    }

    procedure SetHex(const Buf: AnsiString; Ecc: TCnEcc = nil);
    {* 从十六进制字符串中加载点坐标，内部有 02 03 04 前缀的处理，
       如果无 02 03 04 前缀则对半劈开分别赋值给 X 和 Y，
       如果前缀是 02 或 03，说明内容只有 X 坐标，此时需传入椭圆曲线实例来计算 Y 坐标。

       参数：
         const Buf: AnsiString            - 待加载的十六进制字符串
         Ecc: TCnEcc                      - 需计算 Y 坐标时所需的椭圆曲线实例

       返回值：（无）
    }

    function ToHex(FixedLen: Integer = 0): string;
    {* 输出成带 03 或 04 前缀的十六进制字符串，如果只有 X 值，使用 03 前缀

       参数：
         FixedLen: Integer                - 指定数据的固定字节长度，不足则高位补 0

       返回值：string                     - 返回
    }

    procedure SetBase64(const Buf: AnsiString; Ecc: TCnEcc = nil);
    {* 从 Base64 字符串中加载点坐标，内部有 02 03 04 前缀的处理，
       如果无 02 03 04 前缀则对半劈开分别赋值给 X 和 Y，
       如果前缀是 02 或 03，说明内容只有 X 坐标，此时需传入 Ecc 曲线实例来计算 Y 坐标。

       参数：
         const Buf: AnsiString            - 待加载的 Base64 字符串
         Ecc: TCnEcc                      - 需计算 Y 坐标时所需的椭圆曲线实例

       返回值：（无）
    }

    function ToBase64(FixedLen: Integer = 0): string;
    {* 输出成带 03 或 04 前缀的 Base64 字符串，如果只有 X 值，使用 03 前缀。

       参数：
         FixedLen: Integer                - 指定数据的固定字节长度，不足则高位补 0

       返回值：string                     - 返回 Base64 字符串
    }

    property X: TCnBigNumber read FX write SetX;
    {* 椭圆曲线点的 X 坐标}
    property Y: TCnBigNumber read FY write SetY;
    {* 椭圆曲线点的 Y 坐标}
  end;

  TCnEcc3Point = class(TPersistent)
  {* 射影/仿射/雅可比坐标点，用于内部计算提速}
  private
    FX: TCnBigNumber;
    FY: TCnBigNumber;
    FZ: TCnBigNumber;
    procedure SetX(const Value: TCnBigNumber);
    procedure SetY(const Value: TCnBigNumber);
    procedure SetZ(const Value: TCnBigNumber);
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent              - 欲从之赋值的源对象

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 是否为无穷远点也即 0 点。

       参数：
         （无）

       返回值：Boolean                    - 返回是否无穷远点
    }

    procedure SetZero;
    {* 设为无穷远点也即 0 点}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ELSE} virtual; {$ENDIF}
    {* 转换为字符串，简单输出用逗号分隔的十六进制 X、Y、Z 坐标值。

       参数：
         （无）

       返回值：string                     - 返回十六进制字符串
    }

    property X: TCnBigNumber read FX write SetX;
    {* X 坐标}
    property Y: TCnBigNumber read FY write SetY;
    {* Y 坐标}
    property Z: TCnBigNumber read FZ write SetZ;
    {* Z 坐标。如果为 0 则表示是无穷远点}
  end;

  TCnEccPublicKey = class(TCnEccPoint);
  {* 椭圆曲线的公钥，G 点计算 k 次后的点坐标}

  TCnEccPrivateKey = class(TCnBigNumber);
  {* 椭圆曲线的私钥，计算次数 k 次}

  TCnEccSignature = class(TPersistent)
  {* 椭圆曲线的签名，两个大数 R S}
  private
    FR: TCnBigNumber;
    FS: TCnBigNumber;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent              - 欲从之赋值的源对象

       返回值：（无）
    }

    function ToHex(FixedLen: Integer = 0): string;
    {* 转换为十六进制字符串，内部 R S 简单拼接，注意需要从十六进制中恢复时
       宜指定 FixedLen 为对应椭圆曲线的 BytesCount，避免存在前导 0 字节而出错。


       参数：
         FixedLen: Integer                - 指定数据的固定字节长度，不足则高位补 0

       返回值：string                     - 返回十六进制字符串
    }

    procedure SetHex(const Buf: AnsiString);
    {* 从十六进制字符串中加载，内部对半拆分。

       参数：
         const Buf: AnsiString            - 待加载的十六进制字符串

       返回值：（无）
    }

    function ToBase64(FixedLen: Integer = 0): string;
    {* 转换为 Base64 字符串，内部 R S 简单拼接后转换，
       宜指定 FixedLen 为对应椭圆曲线的 BytesCount，避免存在前导 0 字节而出错。

       参数：
         FixedLen: Integer                - 指定数据的固定字节长度，不足则高位补 0

       返回值：string                     -
    }

    function SetBase64(const Buf: AnsiString): Boolean;
    {* 从 Base64 字符串中加载，内部对半拆分。返回加载是否成功。

       参数：
         const Buf: AnsiString            - 待加载的 Base64 字符串

       返回值：Boolean                    - 返回加载是否成功
    }

    function ToAsn1Hex(FixedLen: Integer = 0): string;
    {* 将 R S 拼接包装为 ASN1 的 BER/DER 格式的十六进制字符串，
       可指定 FixedLen 为对应椭圆曲线的 BytesCount，避免存在前导 0 字节而出错。

       参数：
         FixedLen: Integer                - 指定数据的固定字节长度，不足则高位补 0

       返回值：string                     - 返回十六进制字符串
    }

    function SetAsn1Hex(const Buf: AnsiString): Boolean;
    {* 从 ASN1 的 BER/DER 格式的十六进制字符串中加载 R S，返回加载是否成功。

       参数：
         const Buf: AnsiString            - 待加载的 ASN1 的十六进制字符串

       返回值：Boolean                    - 返回加载是否成功
    }

    function ToAsn1Base64: string;
    {* 将 R S 拼接包装为 ASN1 的 BER/DER 格式的内容后再 Base64 编码。

       参数：
         （无）

       返回值：string                     - 返回 ASN1 格式的 Base64 字符串
    }

    function SetAsn1Base64(const Buf: AnsiString): Boolean;
    {* 从 ASN1 的 BER/DER 格式的 Base64 字符串中加载 R S，返回加载是否成功。

       参数：
         const Buf: AnsiString            - 待加载的 ASN1 格式的 Base64 字符串

       返回值：Boolean                    - 返回加载是否成功
    }

    property R: TCnBigNumber read FR;
    {* 签名 R 值}
    property S: TCnBigNumber read FS;
    {* 签名 S 值}
  end;

  TCnEccCurveType = (ctCustomized, ctSM2, ctSM2Example192, ctSM2Example256,
    ctRfc4754ECDSAExample256, ctSecp224r1, ctSecp224k1, ctSecp256k1, ctPrime256v1,
    ctWapiPrime192v1, ctSM9Bn256v1, ctSecp384r1, ctSecp521r1);
  {* 本单元支持的椭圆曲线类型}

  TCnEcc = class
  {* 描述一有限素域 p 也就是 0 到 p - 1 上的椭圆曲线 y^2 = x^3 + Ax + B mod p}
  private
    FCoefficientB: TCnBigNumber;
    FCoefficientA: TCnBigNumber;
    FOrder: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FGenerator: TCnEccPoint;
    FSizeUFactor: TCnBigNumber;
    FSizePrimeType: TCnPrimeType;
    FCoFactor: Integer;
    F2Inverse: TCnBigNumber;
    function GetBitsCount: Integer;
    function GetBytesCount: Integer;
  protected
    procedure CalcX3AddAXAddB(X: TCnBigNumber); // 计算 X^3 + A*X + B，结果放入 X
  public
    constructor Create; overload; virtual;
    {* 构造函数}
    constructor Create(Predefined: TCnEccCurveType); overload;
    {* 构造函数。

       参数：
         Predefined: TCnEccCurveType      - 待加载的椭圆曲线参数类型

       返回值：TCnEcc                     - 返回创建的对象实例
    }

    constructor Create(const A: AnsiString; const B: AnsiString; const FieldPrime: AnsiString;
      const GX: AnsiString; const GY: AnsiString; const Order: AnsiString; H: Integer = 1); overload;
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、G 点坐标、G 点的阶数等，除辅助因子外需要十六进制字符串。

       参数：
         const A: AnsiString              - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         const B: AnsiString              - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         const FieldPrime: AnsiString     - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         const GX: AnsiString             - 魏尔斯特拉斯椭圆曲线方程的 G 点的 X 坐标
         const GY: AnsiString             - 魏尔斯特拉斯椭圆曲线方程的 G 点的 Y 坐标
         const Order: AnsiString          - 魏尔斯特拉斯椭圆曲线方程的 G 点的阶
         H: Integer                       - 魏尔斯特拉斯椭圆曲线方程的辅助因子

       返回值：TCnEcc                     - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Load(Predefined: TCnEccCurveType); overload; virtual;
    {* 加载椭圆曲线参数。

       参数：
         Predefined: TCnEccCurveType      - 待加载的椭圆曲线参数类型

       返回值：（无）
    }

    procedure Load(const A: AnsiString; const B: AnsiString; const FieldPrime: AnsiString;
      const GX: AnsiString; const GY: AnsiString; const Order: AnsiString; H: Integer = 1); overload; virtual;
    {* 加载椭圆曲线参数，传入方程的 A, B 参数、有限域上界 p、G 点坐标、G 点的阶数等，除辅助因子外均需要十六进制字符串。

       参数：
         const A: AnsiString              - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         const B: AnsiString              - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         const FieldPrime: AnsiString     - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         const GX: AnsiString             - 魏尔斯特拉斯椭圆曲线方程的 G 点的 X 坐标
         const GY: AnsiString             - 魏尔斯特拉斯椭圆曲线方程的 G 点的 Y 坐标
         const Order: AnsiString          - 魏尔斯特拉斯椭圆曲线方程的阶
         H: Integer                       - 魏尔斯特拉斯椭圆曲线方程的辅助因子

       返回值：（无）
    }

    procedure AffinePointAddPoint(P: TCnEcc3Point; Q: TCnEcc3Point; Sum: TCnEcc3Point);
    {* 使用仿射坐标系进行点加，避免取模逆元导致的开销。

       参数：
         P: TCnEcc3Point                  - 第一个加数的点坐标
         Q: TCnEcc3Point                  - 第二个加数的点坐标
         Sum: TCnEcc3Point                - 输出的和的点坐标

       返回值：（无）
    }
    procedure AffinePointSubPoint(P: TCnEcc3Point; Q: TCnEcc3Point; Diff: TCnEcc3Point);
    {* 使用仿射坐标系进行点减，避免取模逆元导致的开销。

       参数：
         P: TCnEcc3Point                  - 被减数的点坐标
         Q: TCnEcc3Point                  - 减数的点坐标
         Diff: TCnEcc3Point               - 输出的差的点坐标

       返回值：（无）
    }

    procedure JacobianPointAddPoint(P: TCnEcc3Point; Q: TCnEcc3Point; Sum: TCnEcc3Point);
    {* 使用雅可比坐标系进行点加，避免取模逆元导致的开销

       参数：
         P: TCnEcc3Point                  - 第一个加数的点坐标
         Q: TCnEcc3Point                  - 第二个加数的点坐标
         Sum: TCnEcc3Point                - 输出的和的点坐标

       返回值：（无）
    }

    procedure JacobianPointSubPoint(P: TCnEcc3Point; Q: TCnEcc3Point; Diff: TCnEcc3Point);
    {* 使用雅可比坐标系进行点减，避免取模逆元导致的开销

       参数：
         P: TCnEcc3Point                  - 被减数的点坐标
         Q: TCnEcc3Point                  - 减数的点坐标
         Diff: TCnEcc3Point               - 输出的差的点坐标

       返回值：（无）
    }

    procedure AffineMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point); virtual;
    {* 使用仿射坐标系进行点乘，避免取模逆元导致的开销。

       参数：
         K: TCnBigNumber                  - 乘数，形式为大数
         Point: TCnEcc3Point              - 被乘的坐标点

       返回值：（无）
    }
    procedure JacobianMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point); virtual;
    {* 使用雅可比坐标系进行点乘，避免取模逆元导致的开销。

       参数：
         K: TCnBigNumber                  - 乘数，形式为大数
         Point: TCnEcc3Point              - 被乘的坐标点

       返回值：（无）
    }

    procedure MultiplePoint(K: Int64; Point: TCnEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P。

       参数：
         K: Int64                         - 乘数
         Point: TCnEccPoint               - 被乘的坐标点

       返回值：（无）
    }
    procedure MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P，内部用仿射坐标点乘进行加速。

       参数：
         K: TCnBigNumber                  - 乘数，形式为大数
         Point: TCnEccPoint               - 被乘的坐标点

       返回值：（无）
    }

    procedure NormalMultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
    {* 计算某点 P 的 k * P 值，值重新放入 P。

       参数：
         K: TCnBigNumber                  - 乘数，形式为大数
         Point: TCnEccPoint               - 被乘的坐标点

       返回值：（无）
    }

    procedure PointAddPoint(P: TCnEccPoint; Q: TCnEccPoint; Sum: TCnEccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同，内部普通实现

       参数：
         P: TCnEccPoint                   - 第一个加数的点坐标
         Q: TCnEccPoint                   - 第二个加数的点坐标
         Sum: TCnEccPoint                 - 输出的和的点坐标

       返回值：（无）
    }

    procedure PointSubPoint(P: TCnEccPoint; Q: TCnEccPoint; Diff: TCnEccPoint);
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同

       参数：
         P: TCnEccPoint                   - 被减数的点坐标
         Q: TCnEccPoint                   - 减数的点坐标
         Diff: TCnEccPoint                - 输出的差的点坐标

       返回值：（无）
    }

    procedure PointInverse(P: TCnEccPoint);
    {* 计算 P 点的加法逆元 -P，值重新放入 P。

       参数：
         P: TCnEccPoint                   - 待取逆元的坐标点

       返回值：（无）
    }

    procedure AffinePointInverse(P: TCnEcc3Point);
    {* 计算以仿射坐标表示的 P 点的加法逆元 -P，值重新放入 P。

       参数：
         P: TCnEcc3Point                  - 待取逆元的坐标点

       返回值：（无）
    }

    procedure JacobianPointInverse(P: TCnEcc3Point);
    {* 计算以雅可比坐标表示的 P 点的加法逆元 -P，值重新放入 P。

       参数：
         P: TCnEcc3Point                  - 待取逆元的多项式坐标点

       返回值：（无）
    }
    function IsPointOnCurve(P: TCnEccPoint): Boolean;
    {* 判断 P 点是否在本曲线上。

       参数：
         P: TCnEccPoint                   - 待判断的坐标点

       返回值：Boolean                    - 返回是否在曲线上
    }

    function PlainToPoint(Plain: TCnBigNumber; OutPoint: TCnEccPoint): Boolean;
    {* 将要加密的明文数值包装成一个待加密的点，也就是以明文为 X 求方程的 Y。
       注意 Plain 为 0 时直接对应至零点，即使椭圆曲线上有（0, 非零 Y）形式的合法点存在。

       参数：
         Plain: TCnBigNumber              - 待加密的明文数
         OutPoint: TCnEccPoint            - 输出的明文坐标点

       返回值：Boolean                    - 返回求解是否成功
    }

    function PointToPlain(Point: TCnEccPoint; OutPlain: TCnBigNumber): Boolean;
    {* 将解密出的明文点解开成一个明文数值，也就是将点的 X 值取出。

       参数：
         Point: TCnEccPoint               - 待解开的明文坐标点
         OutPlain: TCnBigNumber           - 返回明文数值

       返回值：Boolean                    - 返回解开是否成功
    }

    procedure GenerateKeys(PrivateKey: TCnEccPrivateKey; PublicKey: TCnEccPublicKey);
    {* 生成一对该椭圆曲线的公私钥，私钥是运算次数 k，公钥是基点 G 经过 k 次乘法后得到的点坐标 K。

       参数：
         PrivateKey: TCnEccPrivateKey     - 生成的椭圆曲线的私钥
         PublicKey: TCnEccPublicKey       - 生成的椭圆曲线的公钥

       返回值：（无）
    }

    procedure Encrypt(PlainPoint: TCnEccPoint; PublicKey: TCnEccPublicKey;
      OutDataPoint1: TCnEccPoint; OutDataPoint2: TCnEccPoint);
    {* 公钥加密明文点 M，得到两个点的输出密文，内部包含了随机值 r，也就是 C1 = M + rK; C2 = r * G。

       参数：
         PlainPoint: TCnEccPoint          - 待加密的明文坐标点
         PublicKey: TCnEccPublicKey       - 用于加密的椭圆曲线公钥
         OutDataPoint1: TCnEccPoint       - 输出密文坐标点一
         OutDataPoint2: TCnEccPoint       - 输出密文坐标点二

       返回值：（无）
    }

    procedure Decrypt(DataPoint1: TCnEccPoint; DataPoint2: TCnEccPoint;
      PrivateKey: TCnEccPrivateKey; OutPlainPoint: TCnEccPoint);
    {* 私钥解密密文点，也就是计算 C1 - k * C2 就得到了原文点 M。

       参数：
         DataPoint1: TCnEccPoint          - 待解密的密文坐标点一
         DataPoint2: TCnEccPoint          - 待解密的密文坐标点二
         PrivateKey: TCnEccPrivateKey     - 用于解密的椭圆曲线私钥
         OutPlainPoint: TCnEccPoint       - 输出明文坐标点

       返回值：（无）
    }

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
    {* 辅助因子 H，也就是总点数 = N * H，先用 Integer 表示，一般都是 1}
    property BitsCount: Integer read GetBitsCount;
    {* 该椭圆曲线的素数域位数}
    property BytesCount: Integer read GetBytesCount;
    {* 该椭圆曲线的素数域字节数}
  end;

  TCnEccKeyType = (cktPKCS1, cktPKCS8);
  {* ECC 密钥文件格式。注意它和 CnRSA 中的 TCnRSAKeyType 名字重复，使用时要注意}

  TCnEcc2Matrix = class(TCn2DObjectList)
  {* 容纳 TCnEccPoint 的二维数组对象}
  private
    function GetValueObject(Row: Integer; Col: Integer): TCnEccPoint;
    procedure SetValueObject(Row: Integer; Col: Integer; const Value: TCnEccPoint);
  protected

  public
    constructor Create(ARow: Integer; ACol: Integer); override;
    {* 构造函数。

       参数：
         ARow: Integer                    - 行数
         ACol: Integer                    - 列数

       返回值：TCnEcc2Matrix              - 返回创建的对象实例
    }

    property ValueObject[Row, Col: Integer]: TCnEccPoint read GetValueObject write SetValueObject; default;
    {* 二维数组值}
  end;

  TCnEcc3Matrix = class(TCn2DObjectList)
  {* 容纳 TCnEcc3Point 的二维数组对象}
  private
    function GetValueObject(Row: Integer; Col: Integer): TCnEcc3Point;
    procedure SetValueObject(Row: Integer; Col: Integer; const Value: TCnEcc3Point);
  protected

  public
    constructor Create(ARow: Integer; ACol: Integer); override;
    {* 构造函数。

       参数：
         ARow: Integer                    - 行数
         ACol: Integer                    - 列数

       返回值：TCnEcc3Matrix              - 返回创建的对象实例
    }

    property ValueObject[Row, Col: Integer]: TCnEcc3Point read GetValueObject write SetValueObject; default;
    {* 二维数组值}
  end;

  TCnInt64PolynomialEccPoint = class(TPersistent)
  {* 有限扩域上的椭圆曲线上的多项式点描述类}
  private
    FY: TCnInt64Polynomial;
    FX: TCnInt64Polynomial;
    procedure SetX(const Value: TCnInt64Polynomial);
    procedure SetY(const Value: TCnInt64Polynomial);
  public
    constructor Create; overload;
    {* 构造函数}

    constructor Create(const XLowToHighCoefficients: array of const;
      const YLowToHighCoefficients: array of const); overload;
    {* 构造函数。

       参数：
         const XLowToHighCoefficients: array of const     - X 系数数组，从低次到高次
         const YLowToHighCoefficients: array of const     - Y 系数数组，从低次到高次

       返回值：TCnInt64PolynomialEccPoint                 - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent              - 欲从之赋值的源对象

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 是否为无穷远点也即 0 点。

       参数：
         （无）

       返回值：Boolean                    - 返回是否无穷远点
    }

    procedure SetZero;
    {* 设为无穷远点也即 0 点}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property X: TCnInt64Polynomial read FX write SetX;
    {* X 坐标多项式}
    property Y: TCnInt64Polynomial read FY write SetY;
    {* Y 坐标多项式}
  end;

  TCnInt64PolynomialEcc = class
  {* 描述一有限扩域 p 也就是 0 到 p - 1 上 n 次方内的多项式椭圆曲线 y^2 = x^3 + Ax + B mod p，参数均在 Int64 范围内}
  private
    FGenerator: TCnInt64PolynomialEccPoint;
    FCoefficientA: Int64;
    FCoefficientB: Int64;
    FFiniteFieldSize: Int64;
    FOrder: Int64;
    FExtension: Integer;
    FPrimitive: TCnInt64Polynomial;
    procedure SetPrimitive(const Value: TCnInt64Polynomial);
  protected

  public
    constructor Create(A: Int64; B: Int64; FieldPrime: Int64; Ext: Integer; GX: array of const;
      GY: array of const; Order: Int64; PrimitivePolynomial: array of const);
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、扩域次数，G 点坐标多项式、G 点的阶数、本原多项式

       参数：
         A: Int64                                         - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: Int64                                         - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         FieldPrime: Int64                                - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         Ext: Integer                                     - 有限域的扩域次数
         GX: array of const                               - 魏尔斯特拉斯椭圆曲线方程的 G 点的 X 坐标系数
         GY: array of const                               - 魏尔斯特拉斯椭圆曲线方程的 G 点的 Y 坐标系数
         Order: Int64                                     - 魏尔斯特拉斯椭圆曲线方程的 G 点的阶
         const PrimitivePolynomial: array of const        - 本原多项式系数

       返回值：TCnInt64PolynomialEcc                      - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure MultiplePoint(K: Int64; Point: TCnInt64PolynomialEccPoint);
    {* 计算某点 P 的 k * P 值，值重新放入 P。

       参数：
         K: Int64                                         - 乘数
         Point: TCnInt64PolynomialEccPoint                - 被乘的多项式坐标点

       返回值：（无）
    }

    procedure PointAddPoint(P: TCnInt64PolynomialEccPoint; Q: TCnInt64PolynomialEccPoint;
      Sum: TCnInt64PolynomialEccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同。

       参数：
         P: TCnInt64PolynomialEccPoint    - 第一个加数的多项式坐标点
         Q: TCnInt64PolynomialEccPoint    - 第二个加数的多项式坐标点
         Sum: TCnInt64PolynomialEccPoint  - 输出的和的多项式坐标点

       返回值：（无）
    }

    procedure PointSubPoint(P: TCnInt64PolynomialEccPoint; Q: TCnInt64PolynomialEccPoint;
      Diff: TCnInt64PolynomialEccPoint);
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同。

       参数：
         P: TCnInt64PolynomialEccPoint    - 被减数的多项式坐标点
         Q: TCnInt64PolynomialEccPoint    - 减数的多项式坐标点
         Diff: TCnInt64PolynomialEccPoint - 输出的差的多项式坐标点

       返回值：（无）
    }

    procedure PointInverse(P: TCnInt64PolynomialEccPoint);
    {* 计算 P 点的加法逆元 -P，值重新放入 P。

       参数：
         P: TCnInt64PolynomialEccPoint    - 待取逆元的多项式坐标点

       返回值：（无）
    }

    function IsPointOnCurve(P: TCnInt64PolynomialEccPoint): Boolean;
    {* 判断 P 点是否在本曲线上。

       参数：
         P: TCnInt64PolynomialEccPoint    - 待判断的多项式坐标点

       返回值：Boolean                    - 返回是否在曲线上
    }

    function DivisionPolynomial(Degree: Integer; outDivisionPolynomial: TCnInt64Polynomial): Boolean;
    {* 递归计算第 Degree 个可除多项式，返回计算是否成功，注意次数一多就容易慢。

       参数：
         Degree: Integer                                  - 可除多项式的序号
         outDivisionPolynomial: TCnInt64Polynomial        - 返回的可除多项式

       返回值：Boolean                                    - 返回计算是否成功
    }

    class function IsPointOnCurve2(PX: TCnInt64Polynomial; PY: TCnInt64Polynomial;
      A: Int64; B: Int64; APrime: Int64; APrimitive: TCnInt64Polynomial): Boolean;
    {* 供外界直接调用的判断（PX, PY）点是否在本曲线上，
       椭圆曲线参数直接指定 A、B、素域上界与本原多项式，无需基点和阶以及扩域次数。

       参数：
         PX: TCnInt64Polynomial           - 待判断的多项式坐标点的 X 坐标多项式
         PY: TCnInt64Polynomial           - 待判断的多项式坐标点的 Y 坐标多项式
         A: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: Int64                    - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         APrimitive: TCnInt64Polynomial   - 本原多项式

       返回值：Boolean                    - 返回计算是否成功
    }

    class procedure RationalPointAddPoint(PX: TCnInt64RationalPolynomial;
      PY: TCnInt64RationalPolynomial; QX: TCnInt64RationalPolynomial;
      QY: TCnInt64RationalPolynomial; SX: TCnInt64RationalPolynomial;
      SY: TCnInt64RationalPolynomial; A: Int64; B: Int64; APrime: Int64;
      APrimitive: TCnInt64Polynomial = nil);
    {* 供外界直接调用的点加方法，将点（PX, PY * y) 和点（QX, QY * y）相加，结果放到（SX, SY * y）点中。
       注意本方法中并不把除法转换为乘法，所有内容包括斜率等内容需要用分式表示，结果也以分式形式输出。
       PX、PY、QX、QY、SX、SY 均为分子分母为纯 x 多项式的分式，SX、SY 不能是 PX、PY、QX、QY。
       另外该方法一般不用于计算后代入具体数值求值，因为计算时无法直接判断值是否相等导致斜率计算与实际值有偏差。
       Schoof 算法中，本原多项式为指定阶数的可除多项式，以构造多项式环来降低运算次数，初步验证通过。

       参数：
         PX: TCnInt64RationalPolynomial   - 有理分式坐标点加数一的 X 坐标有理分式
         PY: TCnInt64RationalPolynomial   - 有理分式坐标点加数一的 Y 坐标有理分式
         QX: TCnInt64RationalPolynomial   - 有理分式坐标点加数二的 X 坐标有理分式
         QY: TCnInt64RationalPolynomial   - 有理分式坐标点加数二的 Y 坐标有理分式
         SX: TCnInt64RationalPolynomial   - 和的有理分多项式坐标点的 X 坐标有理分式
         SY: TCnInt64RationalPolynomial   - 和的有理分多项式坐标点的 Y 坐标有理分式
         A: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: Int64                    - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         APrimitive: TCnInt64Polynomial   - 本原多项式

       返回值：（无）
    }

    class procedure RationalMultiplePoint(K: Integer; MX: TCnInt64RationalPolynomial; MY: TCnInt64RationalPolynomial;
      A: Int64; B: Int64; APrime: Int64; APrimitive: TCnInt64Polynomial = nil);
    {* 供外界直接调用的多倍点方法，使用可除多项式直接计算点（x, 1 * y) 的 k * P 值，值放入 MX, MY * y。
       注意本方法中并不把除法转换为乘法，所有内容包括斜率等内容需要用分式表示，结果也以分式形式输出。
       如果 MX 与 MY 可为 nil 表示不计算 X 或 Y，只计算不为 nil 的。
       另外该方法一般不用于计算后代入具体数值求值，因为计算时无法直接判断值是否相等导致斜率计算与实际值有偏差。
       Schoof 算法中，本原多项式为指定阶数的可除多项式，以构造多项式环来降低运算次数。

       参数：
         K: Integer                       - 乘数
         MX: TCnInt64RationalPolynomial   - 有理分式坐标点乘数的 X 坐标有理分式
         MY: TCnInt64RationalPolynomial   - 有理分式坐标点乘数的 Y 坐标有理分式
         A: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: Int64                    - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         APrimitive: TCnInt64Polynomial   - 本原多项式

       返回值：（无）
    }

    class function IsRationalPointOnCurve(PX: TCnInt64RationalPolynomial; PY: TCnInt64RationalPolynomial;
      A: Int64; B: Int64; APrime: Int64; APrimitive: TCnInt64Polynomial = nil): Boolean;
    {* 供外界直接调用的无本原多项式的判断（PX, PY * y）点是否在本曲线上，
       椭圆曲线参数直接指定 A、B、素域上界与，无需本原多项式、基点和阶以及扩域次数。
       注意所有内容包括斜率等内容均用分式表示，即使有本原多项式存在，除法也不转换为乘法。

       参数：
         PX: TCnInt64RationalPolynomial   - 待判断的有理分式坐标点的 X 坐标有理分式
         PY: TCnInt64RationalPolynomial   - 待判断的有理分式坐标点的 Y 坐标有理分式
         A: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: Int64                         - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: Int64                    - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         APrimitive: TCnInt64Polynomial   - 本原多项式

       返回值：Boolean                    - 返回是否在曲线上
    }

    property Generator: TCnInt64PolynomialEccPoint read FGenerator;
    {* 基点坐标 G}
    property CoefficientA: Int64 read FCoefficientA;
    {* 方程系数 A}
    property CoefficientB: Int64 read FCoefficientB;
    {* 方程系数 B}
    property FiniteFieldSize: Int64 read FFiniteFieldSize;
    {* 有限素域的上界，素数 p}
    property Extension: Integer read FExtension write FExtension;
    {* 有限扩域的次数，也即素数 p 的指数}
    property Order: Int64 read FOrder;
    {* 基点的阶数}
    property Primitive: TCnInt64Polynomial read FPrimitive write SetPrimitive;
    {* 本原多项式}
  end;

  TCnPolynomialEccPoint = class(TPersistent)
  {* 有限扩域上的椭圆曲线上的多项式点描述类}
  private
    FY: TCnBigNumberPolynomial;
    FX: TCnBigNumberPolynomial;
    procedure SetX(const Value: TCnBigNumberPolynomial);
    procedure SetY(const Value: TCnBigNumberPolynomial);
  public
    constructor Create; overload;
    {* 构造函数}
    constructor Create(const XLowToHighCoefficients: array of const;
      const YLowToHighCoefficients: array of const); overload;
    {* 构造函数

       参数：
         const XLowToHighCoefficients: array of const     - X 系数数组，从低次到高次
         const YLowToHighCoefficients: array of const     - Y 系数数组，从低次到高次

       返回值：TCnPolynomialEccPoint                      - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Assign(Source: TPersistent); override;
    {* 从其他对象赋值而来。

       参数：
         Source: TPersistent              - 欲从之赋值的源对象

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 是否为无穷远点也即 0 点。

       参数：
         （无）

       返回值：Boolean                    - 返回是否无穷远点
    }

    procedure SetZero;
    {* 设为无穷远点也即 0 点}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property X: TCnBigNumberPolynomial read FX write SetX;
    {* X 坐标多项式}
    property Y: TCnBigNumberPolynomial read FY write SetY;
    {* Y 坐标多项式}
  end;

  TCnPolynomialEcc = class
  {* 描述一有限扩域 p 也就是 0 到 p - 1 上 n 次方内的多项式椭圆曲线 y^2 = x^3 + Ax + B mod p，参数均以大数表示}
  private
    FGenerator: TCnPolynomialEccPoint;
    FCoefficientA: TCnBigNumber;
    FCoefficientB: TCnBigNumber;
    FFiniteFieldSize: TCnBigNumber;
    FOrder: TCnBigNumber;
    FExtension: Integer;
    FPrimitive: TCnBigNumberPolynomial;
    procedure SetPrimitive(const Value: TCnBigNumberPolynomial);
  protected

  public
    constructor Create(const A: AnsiString; const B: AnsiString; const FieldPrime: AnsiString;
      Ext: Integer; GX: TCnBigNumberPolynomial; GY: TCnBigNumberPolynomial;
      const Order: AnsiString; PrimitivePolynomial: TCnBigNumberPolynomial); overload;
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、扩域次数，G 点坐标多项式、G 点的阶数、本原多项式
       参数均复制内容入对象内部，不持有参数的对象引用。字符串参数均需十六进制字符串。

       参数：
         const A: AnsiString                              - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         const B: AnsiString                              - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         const FieldPrime: AnsiString                     - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         Ext: Integer                                     - 有限域的扩域次数
         GX: TCnBigNumberPolynomial                       - 魏尔斯特拉斯椭圆曲线方程的 G 点的 X 坐标
         GY: TCnBigNumberPolynomial                       - 魏尔斯特拉斯椭圆曲线方程的 G 点的 Y 坐标
         const Order: AnsiString                          - 魏尔斯特拉斯椭圆曲线方程的 G 点的阶
         PrimitivePolynomial: TCnBigNumberPolynomial      - 本原多项式

       返回值：TCnPolynomialEcc                           - 返回创建的对象实例
    }

    constructor Create(A: TCnBigNumber; B: TCnBigNumber; FieldPrime: TCnBigNumber;
      Ext: Integer; GX: TCnBigNumberPolynomial; GY: TCnBigNumberPolynomial;
      AnOrder: TCnBigNumber; PrimitivePolynomial: TCnBigNumberPolynomial); overload;
    {* 构造函数，传入方程的 A, B 参数、有限域上界 p、扩域次数，G 点坐标多项式、G 点的阶数、本原多项式
       参数均复制内容入对象内部，不持有参数的对象引用。

       参数：
         A: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         FieldPrime: TCnBigNumber                         - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         Ext: Integer                                     - 有限域的扩域次数
         GX: TCnBigNumberPolynomial                       - 魏尔斯特拉斯椭圆曲线方程的 G 点的 X 坐标
         GY: TCnBigNumberPolynomial                       - 魏尔斯特拉斯椭圆曲线方程的 G 点的 Y 坐标
         AnOrder: TCnBigNumber                            - 魏尔斯特拉斯椭圆曲线方程的 G 点的阶
         PrimitivePolynomial: TCnBigNumberPolynomial      - 本原多项式

       返回值：TCnPolynomialEcc                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure MultiplePoint(K: Int64; Point: TCnPolynomialEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P。

       参数：
         K: Int64                         - 乘数
         Point: TCnPolynomialEccPoint     - 被乘的坐标点

       返回值：（无）
    }

    procedure MultiplePoint(K: TCnBigNumber; Point: TCnPolynomialEccPoint); overload;
    {* 计算某点 P 的 k * P 值，值重新放入 P

       参数：
         K: TCnBigNumber                  - 乘数，形式为大数
         Point: TCnPolynomialEccPoint     - 被乘的坐标点

       返回值：（无）
    }

    procedure PointAddPoint(P: TCnPolynomialEccPoint; Q: TCnPolynomialEccPoint;
      Sum: TCnPolynomialEccPoint);
    {* 计算 P + Q，值放入 Sum 中，Sum 可以是 P、Q 之一，P、Q 可以相同。

       参数：
         P: TCnPolynomialEccPoint         - 第一个加数的多项式坐标点
         Q: TCnPolynomialEccPoint         - 第二个加数的多项式坐标点
         Sum: TCnPolynomialEccPoint       - 输出的和的多项式坐标点

       返回值：（无）
    }

    procedure PointSubPoint(P: TCnPolynomialEccPoint; Q: TCnPolynomialEccPoint;
      Diff: TCnPolynomialEccPoint);
    {* 计算 P - Q，值放入 Diff 中，Diff 可以是 P、Q 之一，P、Q 可以相同

       参数：
         P: TCnPolynomialEccPoint         - 被减数的多项式坐标点
         Q: TCnPolynomialEccPoint         - 减数的多项式坐标点
         Diff: TCnPolynomialEccPoint      - 输出的差的多项式坐标点

       返回值：（无）
    }

    procedure PointInverse(P: TCnPolynomialEccPoint);
    {* 计算 P 点的加法逆元 -P，值重新放入 P。

       参数：
         P: TCnPolynomialEccPoint         - 待取逆元的多项式坐标点

       返回值：（无）
    }

    function IsPointOnCurve(P: TCnPolynomialEccPoint): Boolean;
    {* 判断 P 点是否在本曲线上。

       参数：
         P: TCnPolynomialEccPoint         - 待判断的多项式坐标点

       返回值：Boolean                    - 返回是否在曲线上
    }

    function DivisionPolynomial(Degree: Integer; outDivisionPolynomial: TCnBigNumberPolynomial): Boolean;
    {* 递归计算第 Degree 个可除多项式，返回计算是否成功，注意次数一多就容易慢。

       参数：
         Degree: Integer                                  - 可除多项式的序号
         outDivisionPolynomial: TCnBigNumberPolynomial    - 返回的可除多项式

       返回值：Boolean                                    - 返回计算是否成功
    }

    class function IsPointOnCurve2(PX: TCnBigNumberPolynomial; PY: TCnBigNumberPolynomial;
      A: TCnBigNumber; B: TCnBigNumber; APrime: TCnBigNumber; APrimitive: TCnBigNumberPolynomial): Boolean;
    {* 供外界直接调用的判断（PX, PY）点是否在本曲线上，
       椭圆曲线参数直接指定 A、B、素域上界与本原多项式，无需基点和阶以及扩域次数。

       参数：
         PX: TCnBigNumberPolynomial                       - 待判断的多项式坐标点的 X 坐标多项式
         PY: TCnBigNumberPolynomial                       - 待判断的多项式坐标点的 Y 坐标多项式
         A: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: TCnBigNumber                             - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         APrimitive: TCnBigNumberPolynomial               - 本原多项式

       返回值：Boolean                                    - 返回计算是否成功
    }

    class procedure RationalPointAddPoint(PX: TCnBigNumberRationalPolynomial;
      PY: TCnBigNumberRationalPolynomial; QX: TCnBigNumberRationalPolynomial;
      QY: TCnBigNumberRationalPolynomial; SX: TCnBigNumberRationalPolynomial;
      SY: TCnBigNumberRationalPolynomial; A: TCnBigNumber; B: TCnBigNumber;
      APrime: TCnBigNumber; APrimitive: TCnBigNumberPolynomial = nil);
    {* 供外界直接调用的点加方法，将点（PX, PY * y) 和点（QX, QY * y）相加，结果放到（SX, SY * y）点中。
       注意本方法中并不把除法转换为乘法，所有内容包括斜率等内容需要用分式表示，结果也以分式形式输出。
       PX、PY、QX、QY、SX、SY均为分子分母为纯 x 多项式的分式，SX、SY 不能是 PX、PY、QX、QY。
       另外该方法一般不用于计算后代入具体数值求值，因为计算时无法直接判断值是否相等导致斜率计算与实际值有偏差。
       Schoof 算法中，本原多项式为指定阶数的可除多项式，以构造多项式环来降低运算次数，初步验证通过。

       参数：
         PX: TCnBigNumberRationalPolynomial               - 有理分式坐标点加数一的 X 坐标有理分式
         PY: TCnBigNumberRationalPolynomial               - 有理分式坐标点加数一的 Y 坐标有理分式
         QX: TCnBigNumberRationalPolynomial               - 有理分式坐标点加数二的 X 坐标有理分式
         QY: TCnBigNumberRationalPolynomial               - 有理分式坐标点加数二的 Y 坐标有理分式
         SX: TCnBigNumberRationalPolynomial               - 和的有理分多项式坐标点的 X 坐标有理分式
         SY: TCnBigNumberRationalPolynomial               - 和的有理分多项式坐标点的 Y 坐标有理分式
         A: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: TCnBigNumber                             - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         APrimitive: TCnBigNumberPolynomial               - 本原多项式

       返回值：（无）
    }

    class procedure RationalMultiplePoint(K: Integer; MX: TCnBigNumberRationalPolynomial;
      MY: TCnBigNumberRationalPolynomial; A: TCnBigNumber; B: TCnBigNumber; APrime: TCnBigNumber;
      APrimitive: TCnBigNumberPolynomial = nil); overload;
    {* 供外界直接调用的多倍点方法，使用可除多项式直接计算点（x, 1 * y) 的 k * P 值，值放入 MX, MY * y
       注意本方法中并不把除法转换为乘法，所有内容包括斜率等内容需要用分式表示，结果也以分式形式输出。
       PX、PY、QX、QY、SX、SY均为分子分母为纯 x 多项式的分式，，SX、SY 不能是 PX、PY、QX、QY。
       另外该方法一般不用于计算后代入具体数值求值，因为计算时无法直接判断值是否相等导致斜率计算与实际值有偏差。
       Schoof 算法中，本原多项式为指定阶数的可除多项式，以构造多项式环来降低运算次数。

       参数：
         K: Integer                                       - 乘数
         MX: TCnBigNumberRationalPolynomial               - 有理分式坐标点乘数的 X 坐标有理分式
         MY: TCnBigNumberRationalPolynomial               - 有理分式坐标点乘数的 Y 坐标有理分式
         A: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: TCnBigNumber                             - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界
         APrimitive: TCnBigNumberPolynomial               - 本原多项式

       返回值：（无）
    }

    class function IsRationalPointOnCurve(PX: TCnBigNumberRationalPolynomial;
      PY: TCnBigNumberRationalPolynomial; A: TCnBigNumber; B: TCnBigNumber; APrime: TCnBigNumber): Boolean;
    {* 供外界直接调用的无本原多项式的判断（PX, PY * y）点是否在本曲线上，
       椭圆曲线参数直接指定 A、B、素域上界与，无需本原多项式、基点和阶以及扩域次数。
       注意在无本原多项式的情况下，除法无法转换为乘法，所有内容包括斜率等内容需要用分式表示。

       参数：
         PX: TCnBigNumberRationalPolynomial               - 待判断的有理分式坐标点的 X 坐标有理分式
         PY: TCnBigNumberRationalPolynomial               - 待判断的有理分式坐标点的 Y 坐标有理分式
         A: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 a 参数
         B: TCnBigNumber                                  - 魏尔斯特拉斯椭圆曲线方程的 b 参数
         APrime: TCnBigNumber                             - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界

       返回值：Boolean                                    - 返回是否在曲线上
    }

    property Generator: TCnPolynomialEccPoint read FGenerator;
    {* 基点坐标 G}
    property CoefficientA: TCnBigNumber read FCoefficientA;
    {* 方程系数 A}
    property CoefficientB: TCnBigNumber read FCoefficientB;
    {* 方程系数 B}
    property FiniteFieldSize: TCnBigNumber read FFiniteFieldSize;
    {* 有限素域的上界，素数 p}
    property Extension: Integer read FExtension write FExtension;
    {* 有限扩域的次数，也即素数 p 的指数}
    property Order: TCnBigNumber read FOrder;
    {* 基点的阶数}
    property Primitive: TCnBigNumberPolynomial read FPrimitive write SetPrimitive;
    {* 本原多项式}
  end;

function CnInt64EccPointToString(var P: TCnInt64EccPoint): string;
{* 将一个 TCnInt64EccPoint 坐标点转换为字符串。

   参数：
     var P: TCnInt64EccPoint              - 待转换的坐标点

   返回值：string                         - 返回字符串
}

function CnInt64EccSchoof(A: Int64; B: Int64; Q: Int64): Int64;
{* 用 Schoof 算法求椭圆曲线 y^2 = x^3 + Ax + B 在素域 Fq 上的点总数，
   Q 最大支持 Sqrt(2 * Max UInt64)，略大于 Max UInt32。
   Schoof 算法有两个版本，思想一样，但运算过程不同，
   一个是利用点的多项分式在素数域以及基于可除多项式环上进行完整循环运算，比较慢。
   一个是判断时多用各种分子的最大公因式以减少数据量。

   参数：
     A: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     Q: Int64                             - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界

   返回值：Int64                          - 返回点总数
}

function CnEccPointToString(P: TCnEccPoint): string;
{* 将一个 TCnEccPoint 点坐标转换为十进制字符串。

   参数：
     P: TCnEccPoint                       - 待转换的坐标点

   返回值：string                         - 返回十进制字符串
}

function CnEccPointToHex(P: TCnEccPoint): string;
{* 将一个 TCnEccPoint 点坐标转换为十六进制字符串。

   参数：
     P: TCnEccPoint                       - 待转换的坐标点

   返回值：string                         - 返回十六进制字符串
}

function CnInt64Ecc3PointToString(var P: TCnInt64Ecc3Point): string;
{* 将一个 TCnInt64Ecc3Point 点坐标转换为字符串。

   参数：
     var P: TCnInt64Ecc3Point             - 待转换的坐标点

   返回值：string                         - 返回字符串
}

function CnEcc3PointToString(P: TCnEcc3Point): string;
{* 将一个 TCnEcc3Point 点坐标转换为十进制字符串。

   参数：
     P: TCnEcc3Point                      - 待转换的坐标点

   返回值：string                         - 返回十进制字符串
}

function CnEcc3PointToHex(P: TCnEcc3Point): string;
{* 将一个 TCnEcc3Point 点坐标转换为十六进制字符串。

   参数：
     P: TCnEcc3Point                      - 待转换的坐标点

   返回值：string                         - 返回十六进制字符串
}

function CnAffineEcc3PointEqual(P1: TCnEcc3Point; P2: TCnEcc3Point; Prime: TCnBigNumber = nil): Boolean;
{* 判断两个 TCnEcc3Point 点是否相等，如 Prime 为 nil 则只判断值，不做 Z 的除法，
   否则根据射影坐标点计算判断。

   参数：
     P1: TCnEcc3Point                     - 待比较的坐标点一
     P2: TCnEcc3Point                     - 待比较的坐标点二
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否相等
}

function CnEccSchoof(Res: TCnBigNumber; A: TCnBigNumber; B: TCnBigNumber; Q: TCnBigNumber): Boolean;
{* 用 Schoof 算法求椭圆曲线 y^2 = x^3 + Ax + B 在素域 Fq 上的点总数，参数支持大数。

   参数：
     Res: TCnBigNumber                    - 返回点总数
     A: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     Q: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界

   返回值：Boolean                        - 返回计算是否成功
}

function CnEccSchoof2(Res: TCnBigNumber; A: TCnBigNumber; B: TCnBigNumber; Q: TCnBigNumber): Boolean;
{* 用 Wikipedia 上的改进型 Schoof 算法求椭圆曲线 y^2 = x^3 + Ax + B 在素域 Fq 上的点总数，参数支持大数。
   运算速度较上面的原始版本无明显提升。

   参数：
     Res: TCnBigNumber                    - 返回点总数
     A: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     Q: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界

   返回值：Boolean                        - 返回计算是否成功
}

function CnEccFastSchoof(Res: TCnBigNumber; A: TCnBigNumber; B: TCnBigNumber;
  Q: TCnBigNumber): Boolean; {$IFDEF SUPPORT_DEPRECATED} deprecated; {$ENDIF}
{* 用增强型 GCD 的 Schoof 算法求椭圆曲线 y^2 = x^3 + Ax + B 在素域 Fq 上的点总数，参数支持大数。
   TODO: P16 计算基本通过。P19X, P19Y 计算验证未通过，不能投入实际使用。

   参数：
     Res: TCnBigNumber                    - 返回点总数
     A: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     Q: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程所在的有限域上界

   返回值：Boolean                        - 返回计算是否成功
}

function CnInt64EccGenerateParams(out FiniteFieldSize: Int64; out CoefficientA: Int64;
  out CoefficientB: Int64; out GX: Int64; out GY: Int64; out Order: Int64): Boolean;
{* 生成椭圆曲线 y^2 = x^3 + Ax + B mod p 的各个参数，难以完整实现，只能先生成系数很小的。

   参数：
     out FiniteFieldSize: Int64           - 生成的魏尔斯特拉斯椭圆曲线方程的有限域上界
     out CoefficientA: Int64              - 生成的魏尔斯特拉斯椭圆曲线方程的 a 参数
     out CoefficientB: Int64              - 生成的魏尔斯特拉斯椭圆曲线方程的 a 参数
     out GX: Int64                        - 生成的魏尔斯特拉斯椭圆曲线方程的 G 点的 X 坐标
     out GY: Int64                        - 生成的魏尔斯特拉斯椭圆曲线方程的 G 点的 Y 坐标
     out Order: Int64                     - 生成的魏尔斯特拉斯椭圆曲线方程的 G 点的阶

   返回值：Boolean                        - 返回生成是否成功
}

function CnInt64EccDiffieHellmanGenerateOutKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  out PublicKey: TCnInt64PublicKey): Boolean;
{* 根据自身选择的随机数 PrivateKey 生成 ECDH 密钥协商的输出公钥坐标点，双方均需调用。
   其中 OutPublicKey = SelfPrivateKey * G。

   参数：
     Ecc: TCnInt64Ecc                     - 密钥协商所需的椭圆曲线实例
     SelfPrivateKey: TCnInt64PrivateKey   - 自身的椭圆曲线私钥
     out PublicKey: TCnInt64PublicKey     - 输出的椭圆曲线公钥坐标点，需传输至对方

   返回值：Boolean                        - 返回生成是否成功
}

function CnInt64EccDiffieHellmanComputeKey(Ecc: TCnInt64Ecc; SelfPrivateKey: TCnInt64PrivateKey;
  var OtherPublicKey: TCnInt64PublicKey; var SharedSecretKey: TCnInt64PublicKey): Boolean;
{* 根据对方发送的 ECDH 密钥协商的输出公钥计算生成公共密钥坐标点，双方均需调用。
   其中 SecretKey = SelfPrivateKey * OtherPublicKey。

   参数：
     Ecc: TCnInt64Ecc                                     - 密钥协商所需的椭圆曲线实例
     SelfPrivateKey: TCnInt64PrivateKey                   - 自身的椭圆曲线私钥
     var OtherPublicKey: TCnInt64PublicKey                - 由对方生成并传输而来的椭圆曲线公钥
     var SharedSecretKey: TCnInt64PublicKey               - 协商输出的公共密钥坐标点

   返回值：Boolean                                        - 返回协商是否成功
}

function CnInt64EccPointsEqual(var P1: TCnInt64EccPoint; var P2: TCnInt64EccPoint): Boolean;
{* 判断两个 TCnInt64EccPoint 点是否相等。

   参数：
     var P1: TCnInt64EccPoint             - 待比较的坐标点一
     var P2: TCnInt64EccPoint             - 待比较的坐标点二

   返回值：Boolean                        - 返回是否相等
}

function CnEccPointsEqual(P1: TCnEccPoint; P2: TCnEccPoint): Boolean;
{* 判断两个 TCnEccPoint 点是否相等。

   参数：
     P1: TCnEccPoint                      - 待比较的坐标点一
     P2: TCnEccPoint                      - 待比较的坐标点二

   返回值：Boolean                        - 返回是否相等
}

function CnPolynomialEccPointToString(P: TCnPolynomialEccPoint): string;
{* 将一个 TCnPolynomialEccPoint 多项式坐标点转换为字符串。

   参数：
     P: TCnPolynomialEccPoint             - 待转换的多项式坐标点

   返回值：string                         - 返回字符串
}

function CnPolynomialEccPointsEqual(P1: TCnPolynomialEccPoint; P2: TCnPolynomialEccPoint): Boolean;
{* 判断两个 TCnPolynomialEccPoint 多项式坐标点是否相等。

   参数：
     P1: TCnPolynomialEccPoint            - 待比较的多项式坐标点一
     P2: TCnPolynomialEccPoint            - 待比较的多项式坐标点二

   返回值：Boolean                        - 返回是否相等
}

function CnEccDiffieHellmanGenerateOutKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
{* 根据自身选择的随机数 PrivateKey 生成 ECDH 密钥协商的输出公钥坐标点，双方均需调用。
   其中 PublicKey = SelfPrivateKey * G。

   参数：
     Ecc: TCnEcc                          - 密钥协商所需的椭圆曲线实例
     SelfPrivateKey: TCnEccPrivateKey     - 自身的椭圆曲线私钥
     PublicKey: TCnEccPublicKey           - 输出的椭圆曲线公钥坐标点，需传输至对方

   返回值：Boolean                        - 返回生成是否成功
}

function CnEccDiffieHellmanComputeKey(Ecc: TCnEcc; SelfPrivateKey: TCnEccPrivateKey;
  OtherPublicKey: TCnEccPublicKey; SharedSecretKey: TCnEccPublicKey): Boolean;
{* 根据对方发送的 ECDH 密钥协商的输出公钥计算生成公共密钥坐标点，一般拿点的 X 坐标来做密钥，双方均需调用。
   其中 SecretKey = SelfPrivateKey * OtherPublicKey

   参数：
     Ecc: TCnEcc                          - 密钥协商所需的椭圆曲线实例
     SelfPrivateKey: TCnEccPrivateKey     - 自身的椭圆曲线私钥
     OtherPublicKey: TCnEccPublicKey      - 由对方生成并传输而来的椭圆曲线公钥
     SharedSecretKey: TCnEccPublicKey     - 协商输出的公共密钥坐标点

   返回值：Boolean                        - 返回协商是否成功
}

function CnInt64EccPointToEcc3Point(var P: TCnInt64EccPoint; var P3: TCnInt64Ecc3Point): Boolean;
{* Int64 范围内的普通坐标点到仿射或雅可比坐标点的转换，等同于 CnInt64EccPointToAffinePoint 和 CnInt64EccPointToJacobianPoint。

   参数：
     var P: TCnInt64EccPoint              - 待转换的普通坐标点
     var P3: TCnInt64Ecc3Point            - 输出的仿射坐标点或雅可比坐标点

   返回值：Boolean                        - 返回转换是否成功
}

function CnInt64AffinePointToEccPoint(var P3: TCnInt64Ecc3Point;
  var P: TCnInt64EccPoint; Prime: Int64): Boolean;
{* Int64 范围内的仿射坐标点到普通坐标点的转换。

   参数：
     var P3: TCnInt64Ecc3Point            - 待转换的仿射坐标点
     var P: TCnInt64EccPoint              - 输出的普通坐标点
     Prime: Int64                         - 有限域上界

   返回值：Boolean                        - 返回转换是否成功
}

function CnInt64JacobianPointToEccPoint(var P3: TCnInt64Ecc3Point;
  var P: TCnInt64EccPoint; Prime: Int64): Boolean;
{* Int64 范围内的雅可比坐标点到普通坐标点的转换。

   参数：
     var P3: TCnInt64Ecc3Point            - 待转换的雅可比坐标点
     var P: TCnInt64EccPoint              - 输出的普通坐标点
     Prime: Int64                         - 有限域上界

   返回值：Boolean                        - 返回转换是否成功
}

function CnEccPointToEcc3Point(P: TCnEccPoint; P3: TCnEcc3Point): Boolean;
{* 大数范围内的普通坐标点到仿射或雅可比坐标点的转换，等同于 CnEccPointToAffinePoint 和 CnEccPointToJacobianPoint。

   参数：
     P: TCnEccPoint                       - 待转换的普通坐标点
     P3: TCnEcc3Point                     - 输出的仿射坐标点或雅可比坐标点

   返回值：Boolean                        - 返回转换是否成功
}

function CnAffinePointToEccPoint(P3: TCnEcc3Point; P: TCnEccPoint; Prime: TCnBigNumber): Boolean;
{* 大数范围内的仿射坐标点到普通坐标点的转换。

   参数：
     P3: TCnEcc3Point                     - 待转换的仿射坐标点
     P: TCnEccPoint                       - 输出的普通坐标点
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回转换是否成功
}

function CnJacobianPointToEccPoint(P3: TCnEcc3Point; P: TCnEccPoint; Prime: TCnBigNumber): Boolean;
{* 大数范围内的雅可比坐标点到普通坐标点的转换。

   参数：
     P3: TCnEcc3Point                     - 待转换的雅可比坐标点
     P: TCnEccPoint                       - 输出的普通坐标点
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回转换是否成功
}

function CnEccPointToStream(P: TCnEccPoint; Stream: TStream; FixedLen: Integer = 0): Integer;
{* 将一椭圆曲线坐标点的内容写入流，返回写入长度。
   FixedLen 表示椭圆曲线点内大数内容不够 FixedLen 字节长度时高位补足 0，
   以保证 Stream 中输出固定 FixedLen 的长度，内部大数长度超过 FixedLen 时按大数实际长度写。

   参数：
     P: TCnEccPoint                       - 待写入的椭圆曲线坐标点
     Stream: TStream                      - 待写入的流
     FixedLen: Integer                    - 指定数据的固定字节长度，不足则高位补 0

   返回值：Integer                        - 返回写入的实际字节数
}

function CnEccVerifyKeys(Ecc: TCnEcc; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean; overload;
{* 校验某椭圆曲线的公私钥是否配对。

   参数：
     Ecc: TCnEcc                          - 用于校验的椭圆曲线实例
     PrivateKey: TCnEccPrivateKey         - 待校验的椭圆曲线私钥
     PublicKey: TCnEccPublicKey           - 待校验的椭圆曲线公钥

   返回值：Boolean                        - 返回校验是否成功
}

function CnEccVerifyKeys(CurveType: TCnEccCurveType; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean; overload;
{* 校验某椭圆曲线的公私钥是否配对。

   参数：
     CurveType: TCnEccCurveType           - 待校验的椭圆曲线类型
     PrivateKey: TCnEccPrivateKey         - 待校验的椭圆曲线私钥
     PublicKey: TCnEccPublicKey           - 待校验的椭圆曲线公钥

   返回值：Boolean                        - 返回校验是否成功
}

// ======================= 椭圆曲线密钥 PEM 读写实现 ===========================

function CnEccLoadKeysFromPem(const PemFileName: string; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的文件中加载公私钥数据，如某钥参数为空则不载入。

   参数：
     const PemFileName: string            - 待加载的 PEM 文件名
     PrivateKey: TCnEccPrivateKey         - 加载后的内容存入该椭圆曲线私钥
     PublicKey: TCnEccPublicKey           - 加载后的内容存入该椭圆曲线公钥
     out CurveType: TCnEccCurveType       - 加载后的椭圆曲线类型
     KeyHashMethod: TCnKeyHashMethod      - PEM 文件如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 内容自动判断
     const Password: string               - PEM 文件如加密，此处应传对应的密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnEccLoadKeysFromPem(PemStream: TStream; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的流中加载公私钥数据，如某钥参数为空则不载入。

   参数：
     PemStream: TStream                   - 待加载的 PEM 格式的流
     PrivateKey: TCnEccPrivateKey         - 加载后的内容存入该椭圆曲线私钥
     PublicKey: TCnEccPublicKey           - 加载后的内容存入该椭圆曲线公钥
     out CurveType: TCnEccCurveType       - 加载后的椭圆曲线类型
     KeyHashMethod: TCnKeyHashMethod      - PEM 流如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 内容自动判断
     const Password: string               - PEM 流如加密，此处应传对应的密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnEccSaveKeysToPem(const PemFileName: string; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType; KeyType: TCnEccKeyType = cktPKCS1;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 将公私钥写入 PEM 格式的文件中，返回是否成功。

   参数：
     const PemFileName: string                            - 待保存的 PEM 文件名
     PrivateKey: TCnEccPrivateKey                         - 待保存的椭圆曲线私钥
     PublicKey: TCnEccPublicKey                           - 待保存的椭圆曲线公钥
     CurveType: TCnEccCurveType                           - 待保存的椭圆曲线类型
     KeyType: TCnEccKeyType                               - 保存的 PEM 格式类型，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 文件的加密模式，默认不加密，并忽略后面的参数
     KeyHashMethod: TCnKeyHashMethod                      - 保存的 PEM 文件的杂凑模式，默认 MD5
     const Password: string                               - 保存的 PEM 文件如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

function CnEccSaveKeysToPem(PemStream: TStream; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType; KeyType: TCnEccKeyType = cktPKCS1;
  KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 将公私钥写入 PEM 格式的流中，返回是否成功。

   参数：
     PemStream: TStream                                   - 待保存的 PEM 格式的流
     PrivateKey: TCnEccPrivateKey                         - 待保存的椭圆曲线私钥
     PublicKey: TCnEccPublicKey                           - 待保存的椭圆曲线公钥
     CurveType: TCnEccCurveType                           - 待保存的椭圆曲线类型
     KeyType: TCnEccKeyType                               - 保存的 PEM 格式类型，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 流的加密模式，默认不加密，并忽略后面的参数
     KeyHashMethod: TCnKeyHashMethod                      - 保存的 PEM 流的杂凑模式，默认 MD5
     const Password: string                               - 保存的 PEM 流如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

function CnEccLoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的文件中加载公钥数据，返回是否成功。

   参数：
     const PemFileName: string            - 待加载的 PEM 文件名
     PublicKey: TCnEccPublicKey           - 加载后的内容存入该椭圆曲线公钥
     out CurveType: TCnEccCurveType       - 加载后的椭圆曲线类型
     KeyHashMethod: TCnKeyHashMethod      - PEM 文件如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 内容自动判断
     const Password: string               - PEM 文件如加密，此处应传对应的密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnEccLoadPublicKeyFromPem(PemStream: TStream;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 从 PEM 格式的流中加载公钥数据，返回是否成功。

   参数：
     PemStream: TStream                   - 待加载的 PEM 格式的流
     PublicKey: TCnEccPublicKey           - 加载后的内容存入该椭圆曲线公钥
     out CurveType: TCnEccCurveType       - 加载后的椭圆曲线类型
     KeyHashMethod: TCnKeyHashMethod      - PEM 流如加密，此处应传对应的加密杂凑算法，默认 MD5。无法根据 PEM 内容自动判断
     const Password: string               - PEM 流如加密，此处应传对应的密码

   返回值：Boolean                        - 返回加载是否成功
}

function CnEccSavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType;
  KeyType: TCnEccKeyType = cktPKCS1; KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 将公钥写入 PEM 格式的文件中，返回是否成功。

   参数：
     const PemFileName: string                            - 待保存的 PEM 文件名
     PublicKey: TCnEccPublicKey                           - 待保存的椭圆曲线公钥
     CurveType: TCnEccCurveType                           - 待保存的椭圆曲线类型
     KeyType: TCnEccKeyType                               - 保存的 PEM 格式类型，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 文件的加密模式，默认不加密，并忽略后面的参数
     KeyHashMethod: TCnKeyHashMethod                      - 保存的 PEM 文件的杂凑模式，默认 MD5
     const Password: string                               - 保存的 PEM 文件如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

function CnEccSavePublicKeyToPem(PemStream: TStream;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType;
  KeyType: TCnEccKeyType = cktPKCS1; KeyEncryptMethod: TCnKeyEncryptMethod = ckeNone;
  KeyHashMethod: TCnKeyHashMethod = ckhMd5; const Password: string = ''): Boolean; overload;
{* 将公钥写入 PEM 格式的流中，返回是否成功。

   参数：
     PemStream: TStream                                   - 待保存的 PEM 格式的流
     PublicKey: TCnEccPublicKey                           - 待保存的椭圆曲线公钥
     CurveType: TCnEccCurveType                           - 待保存的椭圆曲线类型
     KeyType: TCnEccKeyType                               - 保存的 PEM 格式类型，默认 PKCS1
     KeyEncryptMethod: TCnKeyEncryptMethod                - 保存的 PEM 流的加密模式，默认不加密，并忽略后面的参数
     KeyHashMethod: TCnKeyHashMethod                      - 保存的 PEM 流的杂凑模式，默认 MD5
     const Password: string                               - 保存的 PEM 流如需加密，此处应传加密密码，如不加密则无需传

   返回值：Boolean                                        - 返回保存是否成功
}

// ========================= ECC 文件签名与验证实现 ============================
//
// 流与文件分开实现是因为计算文件摘要时支持大文件，而 FileStream 低版本不支持
// 注意 ECC 签名验证并不是像 RSA 那样解密后比对加密进去的杂凑值
// 而是比对中间结果的大数，ECC 签名内容并不能在验签名时还原原始杂凑值
//
// =============================================================================

function CnEccSignFile(const InFileName: string; const OutSignFileName: string; Ecc: TCnEcc;
  PrivateKey: TCnEccPrivateKey; SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用私钥签名指定文件，Ecc 中需要预先指定曲线。
   使用指定数字摘要算法对文件进行计算得到杂凑值，再将原始的二进制杂凑值进行 BER 编码再 PKCS1 补齐，再用私钥加密。

   参数：
     const InFileName: string             - 待签名的文件名
     const OutSignFileName: string        - 签名内容保存至此文件名
     Ecc: TCnEcc                          - 用于签名的椭圆曲线实例
     PrivateKey: TCnEccPrivateKey         - 用来签名的椭圆曲线私钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型

   返回值：Boolean                        - 返回签名是否成功
}

function CnEccSignFile(const InFileName: string; const OutSignFileName: string; CurveType: TCnEccCurveType;
  PrivateKey: TCnEccPrivateKey; SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用私钥签名指定文件，使用预定义曲线。
   使用指定数字摘要算法对文件进行计算得到杂凑值，再将原始的二进制杂凑值进行 BER 编码再 PKCS1 补齐，再用私钥加密。

   参数：
     const InFileName: string             - 待签名的文件名
     const OutSignFileName: string        - 签名内容保存至此文件名
     CurveType: TCnEccCurveType           - 用于签名的椭圆曲线类型
     PrivateKey: TCnEccPrivateKey         - 用来签名的椭圆曲线私钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型

   返回值：Boolean                        - 返回签名是否成功
}

function CnEccVerifyFile(const InFileName: string; const InSignFileName: string; Ecc: TCnEcc;
  PublicKey: TCnEccPublicKey; SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用公钥与签名值验证指定文件，也即用指定数字摘要算法对文件进行计算得到杂凑值，
   并用公钥解密签名内容并解开 PKCS1 补齐再解开 BER 编码得到杂凑算法与杂凑值，
   并比对两个二进制杂凑值是否相同，返回验证是否通过。
   Ecc 中需要预先指定曲线。

   参数：
     const InFileName: string             - 待验证的文件名
     const InSignFileName: string         - 待验证的签名文件
     Ecc: TCnEcc                          - 用于验证的椭圆曲线实例
     PublicKey: TCnEccPublicKey           - 用来验证的椭圆曲线公钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名文件保持一致

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnEccVerifyFile(const InFileName: string; const InSignFileName: string; CurveType: TCnEccCurveType;
  PublicKey: TCnEccPublicKey; SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用预定义曲线与公钥与签名值验证指定文件，也即用指定数字摘要算法对文件进行计算得到杂凑值，
   并用公钥解密签名内容并解开 PKCS1 补齐再解开 BER 编码得到杂凑算法与杂凑值，
   并比对两个二进制杂凑值是否相同，返回验证是否通过

   参数：
     const InFileName: string             - 待验证的文件名
     const InSignFileName: string         - 待验证的签名文件
     CurveType: TCnEccCurveType           - 用于验证的椭圆曲线类型
     PublicKey: TCnEccPublicKey           - 用来验证的椭圆曲线公钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名文件保持一致

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnEccRecoverPublicKeyFromFile(const InFileName: string; const InSignFileName: string;
  Ecc: TCnEcc; OutPublicKey1: TCnEccPublicKey; OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 从指定文件及其签名文件中还原椭圆曲线公钥值，有一奇一偶两个。Ecc 中需要预先指定曲线。

   参数：
     const InFileName: string             - 已签名的明文文件名
     const InSignFileName: string         - 签名文件
     Ecc: TCnEcc                          - 用于还原的椭圆曲线实例
     OutPublicKey1: TCnEccPublicKey       - 还原的椭圆曲线公钥一
     OutPublicKey2: TCnEccPublicKey       - 还原的椭圆曲线公钥二
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名文件保持一致

   返回值：Boolean                        - 返回还原是否成功
}

function CnEccRecoverPublicKeyFromFile(const InFileName: string; const InSignFileName: string;
  CurveType: TCnEccCurveType; OutPublicKey1: TCnEccPublicKey; OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用预定义曲线从指定文件及其签名文件中还原椭圆曲线公钥值，有一奇一偶两个。

   参数：
     const InFileName: string             - 已签名的明文文件名
     const InSignFileName: string         - 签名文件
     CurveType: TCnEccCurveType           - 用于还原的椭圆曲线类型
     OutPublicKey1: TCnEccPublicKey       - 还原的椭圆曲线公钥一
     OutPublicKey2: TCnEccPublicKey       - 还原的椭圆曲线公钥二
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名文件保持一致

   返回值：Boolean                        - 返回还原是否成功
}

function CnEccSignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  Ecc: TCnEcc; PrivateKey: TCnEccPrivateKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用私钥签名指定内存流，Ecc 中需要预先指定曲线，签名格式是 ASN1/BER 包装的 R S。

   参数：
     InStream: TMemoryStream              - 待签名的内存流
     OutSignStream: TMemoryStream         - 输出的签名内容内存流
     Ecc: TCnEcc                          - 用于签名的椭圆曲线实例
     PrivateKey: TCnEccPrivateKey         - 用来签名的椭圆曲线私钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型

   返回值：Boolean                        - 返回签名是否成功
}

function CnEccSignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  CurveType: TCnEccCurveType; PrivateKey: TCnEccPrivateKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用预定义曲线与私钥签名指定内存流，签名格式是 ASN1/BER 包装的 R S。

   参数：
     InStream: TMemoryStream              - 待签名的内存流
     OutSignStream: TMemoryStream         - 输出的签名内容内存流
     CurveType: TCnEccCurveType           - 用于签名的椭圆曲线类型
     PrivateKey: TCnEccPrivateKey         - 用来签名的椭圆曲线私钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型

   返回值：Boolean                        - 返回签名是否成功
}

function CnEccVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  Ecc: TCnEcc; PublicKey: TCnEccPublicKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用公钥与签名值验证指定内存流，Ecc 中需要预先指定曲线。

   参数：
     InStream: TMemoryStream              - 待验证的内存流
     InSignStream: TMemoryStream          - 签名内容内存流
     Ecc: TCnEcc                          - 用于验证的椭圆曲线实例
     PublicKey: TCnEccPublicKey           - 用来验证的椭圆曲线公钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名内容保持一致

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnEccVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  CurveType: TCnEccCurveType; PublicKey: TCnEccPublicKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用预定义曲线与公钥与签名值验证指定内存流。

   参数：
     InStream: TMemoryStream              - 待验证的内存流
     InSignStream: TMemoryStream          - 签名内容内存流
     CurveType: TCnEccCurveType           - 用于验证的椭圆曲线类型
     PublicKey: TCnEccPublicKey           - 用来验证的椭圆曲线公钥
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名内容保持一致

   返回值：Boolean                        - 返回验证签名是否成功
}

function CnEccRecoverPublicKeyFromStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  Ecc: TCnEcc; OutPublicKey1: TCnEccPublicKey; OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 从指定内存流及其内存流签名中还原椭圆曲线公钥值，有一奇一偶两个。
   Ecc 中需要预先指定曲线。

   参数：
     InStream: TMemoryStream              - 已签名的明文流
     InSignStream: TMemoryStream          - 签名流
     Ecc: TCnEcc                          - 用于还原的椭圆曲线实例
     OutPublicKey1: TCnEccPublicKey       - 还原的椭圆曲线公钥一
     OutPublicKey2: TCnEccPublicKey       - 还原的椭圆曲线公钥二
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名内容保持一致

   返回值：Boolean                        - 返回还原是否成功
}

function CnEccRecoverPublicKeyFromStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  CurveType: TCnEccCurveType; OutPublicKey1: TCnEccPublicKey; OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean; overload;
{* 用预定义曲线从指定内存流及其内存流签名中还原椭圆曲线公钥值，有一奇一偶两个。
   Ecc 中需要预先指定曲线。

   参数：
     InStream: TMemoryStream              - 已签名的明文流
     InSignStream: TMemoryStream          - 签名内容流
     CurveType: TCnEccCurveType           - 用于还原的椭圆曲线类型
     OutPublicKey1: TCnEccPublicKey       - 还原的椭圆曲线公钥一
     OutPublicKey2: TCnEccPublicKey       - 还原的椭圆曲线公钥二
     SignType: TCnEccSignDigestType       - 签名的杂凑类型，需和签名内容保持一致

   返回值：Boolean                        - 返回还原是否成功
}

// ===================== 基于有限扩域的多项式椭圆曲线运算 ======================

function CnInt64PolynomialEccPointToString(P: TCnInt64PolynomialEccPoint): string;
{* 将一个 Int64 多项式坐标点转换为多项式字符串。

   参数：
     const P: TCnInt64PolynomialEccPoint  - 待转换的多项式坐标点

   返回值：string                         - 返回字符串
}

function CnInt64PolynomialEccPointsEqual(P1: TCnInt64PolynomialEccPoint;
  P2: TCnInt64PolynomialEccPoint): Boolean;
{* 判断两个 Int64 多项式坐标点是否相等。

   参数：
     P1: TCnInt64PolynomialEccPoint       - 待比较的多项式坐标点一
     P2: TCnInt64PolynomialEccPoint       - 待比较的多项式坐标点二

   返回值：Boolean                        - 返回是否相等
}

// ============================= 其他辅助函数 ==================================

function CheckEccPublicKey(Ecc: TCnEcc; PublicKey: TCnEccPublicKey): Boolean;
{* 检验给定曲线的 PublicKey 是否合法。

   参数：
     Ecc: TCnEcc                          - 用于校验的椭圆曲线实例
     PublicKey: TCnEccPublicKey           - 待校验的椭圆曲线公钥

   返回值：Boolean                        - 返回校验是否成功
}

function GetCurveTypeFromOID(Data: PAnsiChar; DataByteLen: Cardinal): TCnEccCurveType;
{* 通过 BER 中的原始 OID 数据（包括头）获取对应的椭圆曲线类型。

   参数：
     Data: PAnsiChar                      - 原始 OID 数据块地址
     DataByteLen: Cardinal                - 原始 OID 数据块字节长度

   返回值：TCnEccCurveType                - 返回椭圆曲线类型
}

function GetOIDFromCurveType(Curve: TCnEccCurveType; out OIDAddr: Pointer): Integer;
{* 根据椭圆曲线类型返回其 OID 地址与长度，外界使用后无需释放。

   参数：
     Curve: TCnEccCurveType               - 椭圆曲线类型
     out OIDAddr: Pointer                 - 返回的 OID 数据块地址，如无则返回 nil

   返回值：Integer                        - 返回的 OID 数据块字节长度，如无则返回 0
}

function ReadEccPublicKeyFromBitStringNode(BitStringNode: TCnBerReadNode;
  PublicKey: TCnEccPublicKey): Boolean;
{* 读取 BER 节点 BITSTRING 中的 ECC 公钥，返回是否成功。

   参数：
     BitStringNode: TCnBerReadNode        - 待读取的 BER 节点
     PublicKey: TCnEccPublicKey           - 读出的椭圆曲线公钥

   返回值：Boolean                        - 返回读取是否成功
}

function WriteEccPublicKeyToBitStringNode(Writer: TCnBerWriter;
  ParentNode: TCnBerWriteNode; PublicKey: TCnEccPublicKey): Boolean;
{* 将 ECC 公钥写入 BER 中的 BITSTRING 节点。

   参数：
     Writer: TCnBerWriter                 - BER 写入对象实例
     ParentNode: TCnBerWriteNode          - 待写入的 BER 父节点
     PublicKey: TCnEccPublicKey           - 待写入的椭圆曲线公钥

   返回值：Boolean                        - 返回写入是否成功
}

function GetEccDigestNameFromSignDigestType(Digest: TCnEccSignDigestType): string;
{* 从签名杂凑算法类型的枚举值获取其名称。

   参数：
     Digest: TCnEccSignDigestType         - 签名杂凑算法类型

   返回值：string                         - 返回签名杂凑算法名称
}

procedure CnInt64GenerateGaloisDivisionPolynomials(A: Int64; B: Int64; APrime: Int64;
  MaxDegree: Integer; PolynomialList: TObjectList);
{* 批量生成 0 到 MaxDegree 阶的可除多项式，要确保和 Int64PolynomialGaloisCalcDivisionPolynomial
   的递归实现完全相同。

   参数：
     A: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     APrime: Int64                        - 魏尔斯特拉斯椭圆曲线方程的有限域上界
     MaxDegree: Integer                   - 待计算的可除多项式的最高阶数
     PolynomialList: TObjectList          - 容纳可除多项式对象的列表

   返回值：（无）
}

procedure Int64RationalMultiplePointX(Res: TCnInt64RationalPolynomial; PX: TCnInt64RationalPolynomial;
  K: Integer; A: Int64; B: Int64; APrime: Int64; DivisionPolynomialList: TObjectList;
  APrimitive: TCnInt64Polynomial = nil);
{* 用可除多项式直接算到 K 次倍点的坐标点的 X 坐标，范围是 Int64。
   DivisionPolynomialList 必须是 CnInt64GenerateGaloisDivisionPolynomials 生成的相同 A、B、Prime 的可除多项式列表。

   计算原理如下：
   (x, y) * K 用可除多项式计算出的结果可以写作 (F(x), G(x) * y)，
   那么 (f(x), g(x) * y) * K 用可除多项式计算出的结果可以代入写作(F(f(x))，G(f(x)) * g(x) * y)，
   本函数返回 F(f(x))。

   参数：
     Res: TCnInt64RationalPolynomial      - 返回有理分式坐标点的 K 倍点的 X 坐标有理分式计算结果
     PX: TCnInt64RationalPolynomial       - 待计算的有理分式坐标点的 X 坐标有理分式
     K: Integer                           - 乘数
     A: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     APrime: Int64                        - 魏尔斯特拉斯椭圆曲线方程的有限域上界
     DivisionPolynomialList: TObjectList  - 预先生成的可除多项式对象的列表
     APrimitive: TCnInt64Polynomial       - 本原多项式

   返回值：（无）
}

procedure Int64RationalMultiplePointY(Res: TCnInt64RationalPolynomial; PX: TCnInt64RationalPolynomial;
  PY: TCnInt64RationalPolynomial; K: Integer; A: Int64; B: Int64; APrime: Int64;
  DivisionPolynomialList: TObjectList; APrimitive: TCnInt64Polynomial = nil);
{* 用可除多项式直接算到 K 次倍点的坐标点的 Y 坐标，范围是 Int64。
   DivisionPolynomialList 必须是 CnInt64GenerateGaloisDivisionPolynomials 生成的相同 A、B、Prime 的可除多项式列表。

   计算原理如下：
   (x, y) * K 用可除多项式计算出的结果可以写作 (F(x), G(x) * y)，
   那么 (f(x), g(x) * y) * K 用可除多项式计算出的结果可以代入写作(F(f(x))，G(f(x)) * g(x) * y)，
   本函数返回 G(f(x)) * g(x)。

   参数：
     Res: TCnInt64RationalPolynomial      - 返回有理分式坐标点的 K 倍点的 Y 坐标有理分式计算结果
     PX: TCnInt64RationalPolynomial       - 待计算的有理分式坐标点的 X 坐标有理分式
     PY: TCnInt64RationalPolynomial       - 待计算的有理分式坐标点的 Y 坐标有理分式
     K: Integer                           - 乘数
     A: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: Int64                             - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     APrime: Int64                        - 魏尔斯特拉斯椭圆曲线方程的有限域上界
     DivisionPolynomialList: TObjectList  - 预先生成的可除多项式对象的列表
     APrimitive: TCnInt64Polynomial       - 本原多项式

   返回值：（无）
}

procedure CnGenerateGaloisDivisionPolynomials(A: TCnBigNumber; B: TCnBigNumber; APrime: TCnBigNumber;
  MaxDegree: Integer; PolynomialList: TObjectList);
{* 批量生成 0 到 MaxDegree 阶的可除多项式，要确保和 BigNumberPolynomialGaloisCalcDivisionPolynomial
   的递归实现完全相同

   参数：
     A: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     APrime: TCnBigNumber                 - 魏尔斯特拉斯椭圆曲线方程的有限域上界
     MaxDegree: Integer                   - 待计算的可除多项式的最高阶数
     PolynomialList: TObjectList          - 容纳可除多项式对象的列表

   返回值：（无）
}

procedure RationalMultiplePointX(Res: TCnBigNumberRationalPolynomial; PX: TCnBigNumberRationalPolynomial;
  K: Integer; A: TCnBigNumber; B: TCnBigNumber; APrime: TCnBigNumber;
  DivisionPolynomialList: TObjectList; APrimitive: TCnBigNumberPolynomial = nil);
{* 用可除多项式直接计算 K 次倍点的坐标点的 X 坐标，范围是大整数。
   DivisionPolynomialList 必须是 CnGenerateGaloisDivisionPolynomials 生成的相同 A、B、Prime 的可除多项式列表。

   计算原理如下：
   (x, y) * K 用可除多项式计算出的结果可以写作 (F(x), G(x) * y)，
   那么 (f(x), g(x) * y) * K 用可除多项式计算出的结果可以代入写作(F(f(x))，G(f(x)) * g(x) * y)，
   本函数返回 F(f(x))。

   参数：
     Res: TCnBigNumberRationalPolynomial  - 返回有理分式坐标点的 K 倍点的 X 坐标有理分式计算结果
     PX: TCnBigNumberRationalPolynomial   - 待计算的有理分式坐标点的 X 坐标有理分式
     K: Integer                           - 乘数
     A: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     APrime: TCnBigNumber                 - 魏尔斯特拉斯椭圆曲线方程的有限域上界
     DivisionPolynomialList: TObjectList  - 预先生成的可除多项式对象的列表
     APrimitive: TCnBigNumberPolynomial   - 本原多项式

   返回值：（无）
}

procedure RationalMultiplePointY(Res: TCnBigNumberRationalPolynomial; PX: TCnBigNumberRationalPolynomial;
  PY: TCnBigNumberRationalPolynomial; K: Integer; A: TCnBigNumber; B: TCnBigNumber; APrime: TCnBigNumber;
  DivisionPolynomialList: TObjectList; APrimitive: TCnBigNumberPolynomial = nil);
{* 用可除多项式直接计算 K 次倍点的坐标点的 Y 坐标，范围是大整数。
   DivisionPolynomialList 必须是 CnGenerateGaloisDivisionPolynomials 生成的相同 A、B、Prime 的可除多项式列表。

   计算原理如下：
   (x, y) * K 用可除多项式计算出的结果可以写作 (F(x), G(x) * y)，
   那么 (f(x), g(x) * y) * K 用可除多项式计算出的结果可以代入写作(F(f(x))，G(f(x)) * g(x) * y)，
   本函数返回 G(f(x)) * g(x)

   参数：
     Res: TCnBigNumberRationalPolynomial  - 返回有理分式坐标点的 K 倍点的 X 坐标有理分式计算结果
     PX: TCnBigNumberRationalPolynomial   - 待计算的有理分式坐标点的 X 坐标有理分式
     PY: TCnBigNumberRationalPolynomial   - 待计算的有理分式坐标点的 Y 坐标有理分式
     K: Integer                           - 乘数
     A: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: TCnBigNumber                      - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     APrime: TCnBigNumber                 - 魏尔斯特拉斯椭圆曲线方程的有限域上界
     DivisionPolynomialList: TObjectList  - 预先生成的可除多项式对象的列表
     APrimitive: TCnBigNumberPolynomial   - 本原多项式

   返回值：（无）
}

implementation

uses
  CnContainers, CnRandom, CnBase64;

resourcestring
  SCnErrorEccCurveType = 'Invalid Curve Type.';
  SCnErrorEccKeyData = 'Invalid Key or Data.';

type
  TCnEccPredefinedHexParams = packed record
    P: AnsiString;
    A: AnsiString;
    B: AnsiString;
    X: AnsiString;
    Y: AnsiString;
    N: AnsiString;
    H: AnsiString;
  end;

const
  ECC_PRE_DEFINED_PARAMS: array[TCnEccCurveType] of TCnEccPredefinedHexParams = (
    (P: ''; A: ''; B: ''; X: ''; Y: ''; N: ''; H: ''),
    ( // SM2 = SM2 Prime 256 v1
      P: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFF';
      A: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFC';
      B: '28E9FA9E9D9F5E344D5A9E4BCF6509A7F39789F515AB8F92DDBCBD414D940E93';
      X: '32C4AE2C1F1981195F9904466A39C9948FE30BBFF2660BE1715A4589334C74C7';
      Y: 'BC3736A2F4F6779C59BDCEE36B692153D0A9877CC62A474002DF32E52139F0A0';
      N: 'FFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFF7203DF6B21C6052B53BBF40939D54123';
      H: '01'
    ),
    ( // SM2 Example 192
      P: 'BDB6F4FE3E8B1D9E0DA8C0D46F4C318CEFE4AFE3B6B8551F';
      A: 'BB8E5E8FBC115E139FE6A814FE48AAA6F0ADA1AA5DF91985';
      B: '1854BEBDC31B21B7AEFC80AB0ECD10D5B1B3308E6DBF11C1';
      X: '4AD5F7048DE709AD51236DE65E4D4B482C836DC6E4106640';
      Y: '02BB3A02D4AAADACAE24817A4CA3A1B014B5270432DB27D2';
      N: 'BDB6F4FE3E8B1D9E0DA8C0D40FC962195DFAE76F56564677';
      H: '01'
    ),
    ( // SM2 Example 256
      P: '8542D69E4C044F18E8B92435BF6FF7DE457283915C45517D722EDB8B08F1DFC3';
      A: '787968B4FA32C3FD2417842E73BBFEFF2F3C848B6831D7E0EC65228B3937E498';
      B: '63E4C6D3B23B0C849CF84241484BFE48F61D59A5B16BA06E6E12D1DA27C5249A';
      X: '421DEBD61B62EAB6746434EBC3CC315E32220B3BADD50BDC4C4E6C147FEDD43D';
      Y: '0680512BCBB42C07D47349D2153B70C4E5D7FDFCBFA36EA1A85841B9E46E09A2';
      N: '8542D69E4C044F18E8B92435BF6FF7DD297720630485628D5AE74EE7C32E79B7';
      H: '01'
    ),
    ( // RFC 4754 ECDSA Example 256
      P: 'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF';
      A: '-03';
      B: '5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B';
      X: '6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296';
      Y: '4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5';
      N: 'FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551';
      H: '01'
    ),
    ( // ctSecp224r1
      P: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001';
      A: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE';
      B: '00B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4';
      X: 'B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21';
      Y: 'BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34';
      N: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D';
      H: '01'
    ),
    ( // ctSecp224k1
      P: '00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D';
      A: '00';
      B: '05';
      X: 'A1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C';
      Y: '7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5';
      N: '010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7';
      H: '01'
    ),
    ( // ctSecp256k1
      P: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F';
      A: '00';
      B: '07';
      X: '79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798';
      Y: '483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8';
      N: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141';
      H: '01'
    ),
    ( // ctPrime256v1
      P: 'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF';
      A: 'FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC';
      B: '5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B';
      X: '6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296';
      Y: '4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5';
      N: 'FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551';
      H: '01'
    ),
    ( // ctWapiPrime192v1
      P: 'BDB6F4FE3E8B1D9E0DA8C0D46F4C318CEFE4AFE3B6B8551F';
      A: 'BB8E5E8FBC115E139FE6A814FE48AAA6F0ADA1AA5DF91985';
      B: '1854BEBDC31B21B7AEFC80AB0ECD10D5B1B3308E6DBF11C1';
      X: '4AD5F7048DE709AD51236DE65E4D4B482C836DC6E4106640';
      Y: '02BB3A02D4AAADACAE24817A4CA3A1B014B5270432DB27D2';
      N: 'BDB6F4FE3E8B1D9E0DA8C0D40FC962195DFAE76F56564677';
      H: '01'
    ),
    ( // ctSM9Bn256v1
      P: 'B640000002A3A6F1D603AB4FF58EC74521F2934B1A7AEEDBE56F9B27E351457D';
      A: '0000000000000000000000000000000000000000000000000000000000000000';
      B: '0000000000000000000000000000000000000000000000000000000000000005';
      X: '93DE051D62BF718FF5ED0704487D01D6E1E4086909DC3280E8C4E4817C66DDDD';
      Y: '21FE8DDA4F21E607631065125C395BBC1C1C00CBFA6024350C464CD70A3EA616';
      N: 'B640000002A3A6F1D603AB4FF58EC74449F2934B18EA8BEEE56EE19CD69ECF25';
      H: '01'
    ),
    ( // ctSecp384r1
      P: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF';
      A: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC';
      B: 'B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF';
      X: 'AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7';
      Y: '3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C00A60B1CE1D7E819D7A431D7C90EA0E5F';
      N: 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973';
      H: '01'
    ),
    ( // ctSecp521r1
      P: '01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
      A: '01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC';
      B: '0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00';
      X: '00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66';
      Y: '011839296A789A3BC0045C8A5FB42C7D1BD998F54449579B446817AFBD17273E662C97EE72995EF42640C550B9013FAD0761353C7086A272C24088BE94769FD16650';
      N: '01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409';
      H: '01'
    )
  );

  // PKCS#1
  PEM_EC_PARAM_HEAD = '-----BEGIN EC PARAMETERS-----';
  PEM_EC_PARAM_TAIL = '-----END EC PARAMETERS-----';

  PEM_EC_PRIVATE_HEAD = '-----BEGIN EC PRIVATE KEY-----';
  PEM_EC_PRIVATE_TAIL = '-----END EC PRIVATE KEY-----';

  PEM_EC_PUBLIC_HEAD = '-----BEGIN PUBLIC KEY-----';
  PEM_EC_PUBLIC_TAIL = '-----END PUBLIC KEY-----';

  // PKCS#8
  PEM_PRIVATE_HEAD = '-----BEGIN PRIVATE KEY-----';
  PEM_PRIVATE_TAIL = '-----END PRIVATE KEY-----';

  // ECC 私钥文件里两个节点的 BER Tag 要求的特殊 TypeMask
  ECC_PRIVATEKEY_TYPE_MASK  = $80;

  // 公钥的存储形式
  EC_PUBLICKEY_COMPRESSED_EVEN  = 02; // 省略了 Y，其中 Y 是偶数
  EC_PUBLICKEY_COMPRESSED_ODD   = 03; // 省略了 Y，其中 Y 是奇数
  EC_PUBLICKEY_UNCOMPRESSED     = 04; // X Y 都有

  // 预定义的椭圆曲线类型的 OID 及其最大长度
  EC_CURVE_TYPE_OID_MAX_LENGTH = 8;

  OID_ECPARAM_CURVE_TYPE_SECP256K1: array[0..4] of Byte = ( // 1.3.132.0.10
    $2B, $81, $04, $00, $0A
  );

  OID_ECPARAM_CURVE_TYPE_SM2: array[0..7] of Byte = (       // 1.2.156.10197.301
    $2A, $81, $1C, $CF, $55, $01, $82, $2D
  );

  OID_ECPARAM_CURVE_TYPE_PRIME256V1: array[0..7] of Byte = (  // 1.2.840.10045.3.1.7
    $2A, $86, $48, $CE, $3D, $03, $01, $07
  );

var
  FEccBigNumberPool: TCnBigNumberPool = nil;
  FEccInt64PolynomialPool: TCnInt64PolynomialPool = nil;
  FEccPolynomialPool: TCnBigNumberPolynomialPool = nil;
  FEccInt64RationalPolynomialPool: TCnInt64RationalPolynomialPool = nil;
  FEccRationalPolynomialPool: TCnBigNumberRationalPolynomialPool = nil;

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{* 取 X 的左边高 W 位，其中 W 是 N 的 BitsCount，该函数用于签名验签
   注意它和 SM2 中的同名函数功能不同}
procedure BuildShortXValue(X: TCnBigNumber; Order: TCnBigNumber);
var
  W: Integer;
begin
  W := X.GetBitsCount - Order.GetBitsCount;
  if W > 0 then
    BigNumberShiftRight(X, X, W);
end;

// 将一个 TCnInt64EccPoint 点坐标转换为字符串
function CnInt64EccPointToString(var P: TCnInt64EccPoint): string;
begin
  Result := Format('%d,%d', [P.X, P.Y]);
end;

// 将一个 TCnEccPoint 点坐标转换为十进制字符串
function CnEccPointToString(P: TCnEccPoint): string;
begin
  Result := Format('%s,%s', [P.X.ToDec, P.Y.ToDec]);
end;

// 将一个 TCnEccPoint 点坐标转换为十六进制字符串
function CnEccPointToHex(P: TCnEccPoint): string;
begin
  Result := Format('%s,%s', [P.X.ToHex, P.Y.ToHex]);
end;

// 将一个 TCnInt64Ecc3Point 点坐标转换为字符串}
function CnInt64Ecc3PointToString(var P: TCnInt64Ecc3Point): string;
begin
  Result := Format('%d,%d,%d', [P.X, P.Y, P.Z]);
end;

// 将一个 TCnEcc3Point 点坐标转换为十进制字符串
function CnEcc3PointToString(P: TCnEcc3Point): string;
begin
  Result := Format('%s,%s,%s', [P.X.ToDec, P.Y.ToDec, P.Z.ToDec]);
end;

// 将一个 TCnEcc3Point 点坐标转换为十六进制字符串}
function CnEcc3PointToHex(P: TCnEcc3Point): string;
begin
  Result := Format('%s,%s,%s', [P.X.ToHex, P.Y.ToHex, P.Z.ToHex]);
end;

// 判断两个 TCnEcc3Point 点是否相等，暂时只判断值，不做 Z 的除法
function CnAffineEcc3PointEqual(P1, P2: TCnEcc3Point; Prime: TCnBigNumber): Boolean;
var
  T1, T2, Z1, Z2: TCnBigNumber;
begin
  if P1 = P2 then
    Result := True
  else
  begin
    Result := (BigNumberCompare(P1.X, P2.X) = 0) and (BigNumberCompare(P1.Y, P2.Y) = 0)
      and (BigNumberCompare(P1.Z, P2.Z) = 0);
    if Result or (Prime = nil) then
      Exit;

    // 算 X/Z 和 Y/Z 是否相等
    Z1 := nil;
    Z2 := nil;
    T1 := nil;
    T2 := nil;

    try
      Z1 := FEccBigNumberPool.Obtain;
      Z2 := FEccBigNumberPool.Obtain;

      BigNumberModularInverse(Z1, P1.Z, Prime);
      BigNumberModularInverse(Z2, P2.Z, Prime);

      T1 := FEccBigNumberPool.Obtain;
      T2 := FEccBigNumberPool.Obtain;

      BigNumberDirectMulMod(T1, P1.X, Z1, Prime);
      BigNumberDirectMulMod(T2, P2.X, Z2, Prime);

      if not BigNumberEqual(T1, T2) then // X 不等
        Exit;

      BigNumberDirectMulMod(T1, P1.Y, Z1, Prime);
      BigNumberDirectMulMod(T2, P2.Y, Z2, Prime);

      if not BigNumberEqual(T1, T2) then // Y 不等
        Exit;

      Result := True;
    finally
      FEccBigNumberPool.Recycle(T2);
      FEccBigNumberPool.Recycle(T1);
      FEccBigNumberPool.Recycle(Z2);
      FEccBigNumberPool.Recycle(Z1);
    end;
  end;
end;

// 将一个 TCnPolynomialEccPoint 点坐标转换为字符串
function CnPolynomialEccPointToString(P: TCnPolynomialEccPoint): string;
begin
  Result := Format('%s,%s', [P.X.ToString, P.Y.ToString]);
end;

// 判断两个 TCnPolynomialEccPoint 点是否相等
function CnPolynomialEccPointsEqual(P1, P2: TCnPolynomialEccPoint): Boolean;
begin
  if P1 = P2 then
    Result := True
  else
    Result := BigNumberPolynomialEqual(P1.X, P2.X) and BigNumberPolynomialEqual(P1.Y, P2.Y);
end;

// 判断两个点是否相等
function CnEccPointsEqual(P1, P2: TCnEccPoint): Boolean;
begin
  if P1 = P2 then
    Result := True
  else
    Result := (BigNumberCompare(P1.X, P2.X) = 0) and (BigNumberCompare(P1.Y, P2.Y) = 0);
end;

function CnInt64EccPointsEqual(var P1, P2: TCnInt64EccPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

// 生成椭圆曲线 y^2 = x^3 + Ax + B mod p 的各个参数，难以实现
function CnInt64EccGenerateParams(out FiniteFieldSize, CoefficientA, CoefficientB,
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
      N := N + CnInt64Legendre(Int64(I) * Int64(I) * Int64(I) + CoefficientA * I + CoefficientB, FiniteFieldSize);
    end;
  until CnInt64IsPrime(N);

  // 然后随机找一个 X 求 Y
  Ecc64 := TCnInt64Ecc.Create(CoefficientA, CoefficientB, FiniteFieldSize, 0, 0, FiniteFieldSize);
  try
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
  finally
    Ecc64.Free;
  end;

  Order := N;
  Result := True;
end;

// 求 X 针对 M 的模反元素也就是模逆元 Y，满足 (X * Y) mod M = 1，范围为 Int64，也就是说支持 X 为负值
function MyInt64ModularInverse(X: Int64; Modulus: Int64): Int64;
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

procedure TCnInt64Ecc.AffineMultiplePoint(K: Int64;
  var Point: TCnInt64Ecc3Point);
var
  E, R: TCnInt64Ecc3Point;
begin
  if K < 0 then
  begin
    K := -K;
    AffinePointInverse(Point);
  end;

  if K = 0 then
  begin
    Point.X := 0;
    Point.Y := 0;
    Point.Z := 0;
    Exit;
  end;

  if K > 1 then
  begin
    R.X := 0;
    R.Y := 0;
    R.Z := 0;

    E := Point;

    while K <> 0 do
    begin
      if (K and 1) <> 0 then
        AffinePointAddPoint(R, E, R);

      AffinePointAddPoint(E, E, E);
      K := K shr 1;
    end;

    Point := R;
  end;
end;

procedure TCnInt64Ecc.AffinePointAddPoint(var P, Q: TCnInt64Ecc3Point;
  var Sum: TCnInt64Ecc3Point);
var
  T, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11: Int64;
begin
  if P.Z = 0 then
  begin
    Sum := Q;
    Exit;
  end
  else if Q.Z = 0 then
  begin
    Sum := P;
    Exit;
  end;

  // D1 := px * qz
  D1 := Int64NonNegativeMulMod(P.X, Q.Z, FFiniteFieldSize);

  // D2 := qx * pz
  D2 := Int64NonNegativeMulMod(Q.X, P.Z, FFiniteFieldSize);

  // D4 := py * qz
  D4 := Int64NonNegativeMulMod(P.Y, Q.Z, FFiniteFieldSize);

  // D5 := qy * pz
  D5 := Int64NonNegativeMulMod(Q.Y, P.Z, FFiniteFieldSize);

  if (D1 = D2) and (D4 = D5) then // P.X/P.Z = Q.X/Q.Z 并且 P.Y/P.Z = Q.Y/Q.Z，说明是同一个点
  begin
    // 同一个点，切线法
    // D1 := 3 px^2 + A * pz^2
    D1 := Int64NonNegativeMulMod(P.X, P.X, FFiniteFieldSize);
    D1 := Int64NonNegativeMulMod(D1, 3, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(P.Z, P.Z, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, FCoefficientA, FFiniteFieldSize);
    D1 := Int64NonNegativeAddMod(D1, T, FFiniteFieldSize);

    // D2 := 2 * py * pz
    D2 := Int64NonNegativeMulMod(P.Y, 2, FFiniteFieldSize);
    D2 := Int64NonNegativeMulMod(D2, P.Z, FFiniteFieldSize);

    // D3 := py^2
    D3 := Int64NonNegativeMulMod(P.Y, P.Y, FFiniteFieldSize);

    // D4 := D3 * px * pz
    D4 := Int64NonNegativeMulMod(D3, P.X, FFiniteFieldSize);
    D4 := Int64NonNegativeMulMod(D4, P.Z, FFiniteFieldSize);

    // D5 := D2^2
    D5 := Int64NonNegativeMulMod(D2, D2, FFiniteFieldSize);

    // D6 := D1^2 - 8 * D4
    D6 := Int64NonNegativeMulMod(D1, D1, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(D4, 8, FFiniteFieldSize);
    D6 := Int64NonNegativeAddMod(D6, -T, FFiniteFieldSize);

    // X := D2 * D6
    Sum.X := Int64NonNegativeMulMod(D2, D6, FFiniteFieldSize);

    // Y := D1 * (4 * D4 - D6) - 2 * D5 * D3
    T := Int64NonNegativeMulMod(D4, 4, FFiniteFieldSize);
    T := Int64NonNegativeAddMod(T, -D6, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, D1, FFiniteFieldSize);

    Sum.Y := Int64NonNegativeMulMod(D3, D5, FFiniteFieldSize);
    Sum.Y := Int64NonNegativeAddMod(Sum.Y, Sum.Y, FFiniteFieldSize);
    Sum.Y := Int64NonNegativeAddMod(T, -Sum.Y, FFiniteFieldSize);

    // Z := D2 * D5
    Sum.Z := Int64NonNegativeMulMod(D2, D5, FFiniteFieldSize);
  end
  else  // 不同点，割线法
  begin
    // 因为有不同的 Z 存在，得这样判断，同样不用求逆
    if D1 = D2 then
    begin
      if D4 + D5 = FFiniteFieldSize then // X 相等且 Y 互补
      begin
        Sum.X := 0;
        Sum.Y := 0;
        Sum.Z := 0;
        Exit;
      end
      else // X 相等且 Y 不互补，没法相加
        raise ECnEccException.CreateFmt('Can NOT Calucate Affine %d,%d,%d + %d,%d,%d',
          [P.X, P.Y, P.Z, Q.X, Q.Y, Q.Z]);
    end;

    // D3 := D1 - D2
    D3 := Int64NonNegativeAddMod(D1, -D2, FFiniteFieldSize);

    // D6 := D4 - D5
    D6 := Int64NonNegativeAddMod(D4, -D5, FFiniteFieldSize);

    // D7 := D1 + D2
    D7 := Int64NonNegativeAddMod(D1, D2, FFiniteFieldSize);

    // D8 := pz * qz
    D8 := Int64NonNegativeMulMod(P.Z, Q.Z, FFiniteFieldSize);

    // D9 := D3 ^ 2
    D9 := Int64NonNegativeMulMod(D3, D3, FFiniteFieldSize);

    // D10 := D3 * D9
    D10 := Int64NonNegativeMulMod(D3, D9, FFiniteFieldSize);

    // D11 := D8 * D6 ^ 2 - D7 * D9
    D11 := Int64NonNegativeMulMod(D6, D6, FFiniteFieldSize);
    D11 := Int64NonNegativeMulMod(D11, D8, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(D7, D9, FFiniteFieldSize);
    D11 := Int64NonNegativeAddMod(D11, -T, FFiniteFieldSize);

    // Y := D6 * (D9 * D1 - D11) - D4 * D10
    T := Int64NonNegativeMulMod(D9, D1, FFiniteFieldSize);
    T := Int64NonNegativeAddMod(T, -D11, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, D6, FFiniteFieldSize);

    Sum.Y := Int64NonNegativeMulMod(D4, D10, FFiniteFieldSize);
    Sum.Y := Int64NonNegativeAddMod(T, -Sum.Y, FFiniteFieldSize);

    // X := D3 * D11
    Sum.X := Int64NonNegativeMulMod(D3, D11, FFiniteFieldSize);

    // Z := D10 * D8
    Sum.Z := Int64NonNegativeMulMod(D10, D8, FFiniteFieldSize);
  end;

  if Sum.Z = 0 then
  begin
    Sum.X := 0;
    Sum.Y := 0;
  end;
end;

constructor TCnInt64Ecc.Create(A, B, FieldPrime, GX, GY, Order: Int64);
var
  R: Int64;
begin
  inherited Create;

  // 由外界保证 Order 为素数
  if not CnInt64IsPrime(FieldPrime) then // or not CnInt64IsPrime(Order) then
    raise ECnEccException.Create('Infinite Field must be a Prime Number.');

  if not (GX >= 0) and (GX < FieldPrime) or
    not (GY >= 0) and (GY < FieldPrime) then
    raise ECnEccException.Create('Generator Point must be in Infinite Field.');

  // 要确保 4*a^3+27*b^2 <> 0
  if 4 * A * A * A + 27 * B * B = 0 then
    raise ECnEccException.Create('Error: 4 * A^3 + 27 * B^2 = 0');

  FCoefficientA := A;
  FCoefficientB := B;
  FFiniteFieldSize := FieldPrime;
  FGenerator.X := GX;
  FGenerator.Y := GY;
  FOrder := Order;

  R := FFiniteFieldSize mod 4;
  if R = 3 then  // RFC 5639 要求 p 满足 4u + 3 的形式以便方便地计算 Y，但其他曲线未必
  begin
    FSizePrimeType := pt4U3;
    FSizeUFactor := FFiniteFieldSize div 4;
  end
  else
  begin
    R := FFiniteFieldSize mod 8;
    if R = 1 then
    begin
      FSizePrimeType := pt8U1;
      FSizeUFactor := FFiniteFieldSize div 8;
    end
    else if R = 5 then
    begin
      FSizePrimeType := pt8U5;
      FSizeUFactor := FFiniteFieldSize div 8;
    end
    else
      raise ECnEccException.Create('Invalid Finite Field Size.');
  end;
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

function TCnInt64Ecc.DivisionPolynomial(Degree: Integer;
  outDivisionPolynomial: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialGaloisCalcDivisionPolynomial(FCoefficientA, FCoefficientB,
    Degree, outDivisionPolynomial, FFiniteFieldSize);
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

procedure TCnInt64Ecc.JacobianMultiplePoint(K: Int64;
  var Point: TCnInt64Ecc3Point);
var
  E, R: TCnInt64Ecc3Point;
begin
  if K < 0 then
  begin
    K := -K;
    JacobianPointInverse(Point);
  end;

  if K = 0 then
  begin
    Point.X := 0;
    Point.Y := 0;
    Exit;
  end;

  if K > 1 then
  begin
    R.X := 0;
    R.Y := 0;
    R.Z := 0;

    E := Point;

    while K <> 0 do
    begin
      if (K and 1) <> 0 then
        JacobianPointAddPoint(R, E, R);

      JacobianPointAddPoint(E, E, E);
      K := K shr 1;
    end;

    Point := R;
  end;
end;

procedure TCnInt64Ecc.JacobianPointAddPoint(var P, Q, Sum: TCnInt64Ecc3Point);
var
  T, D1, D2, D3, D4, D5, D6, D7, D8, D9: Int64;
begin
  if P.Z = 0 then
  begin
    Sum := Q;
    Exit;
  end
  else if Q.Z = 0 then
  begin
    Sum := P;
    Exit;
  end;

  // D1 := PX * QZ^2
  D1 := Int64NonNegativeMulMod(Q.Z, Q.Z, FFiniteFieldSize);
  D1 := Int64NonNegativeMulMod(D1, P.X, FFiniteFieldSize);

  // D2 := QX * PZ^2
  D2 := Int64NonNegativeMulMod(P.Z, P.Z, FFiniteFieldSize);
  D2 := Int64NonNegativeMulMod(D2, Q.X, FFiniteFieldSize);

  // D4 := PY * QZ^3
  D4 := Int64NonNegativeMulMod(Q.Z, Q.Z, FFiniteFieldSize);
  D4 := Int64NonNegativeMulMod(D4, Q.Z, FFiniteFieldSize);
  D4 := Int64NonNegativeMulMod(D4, P.Y, FFiniteFieldSize);

  // D5 := QY * PZ^3
  D5 := Int64NonNegativeMulMod(P.Z, P.Z, FFiniteFieldSize);
  D5 := Int64NonNegativeMulMod(D5, P.Z, FFiniteFieldSize);
  D5 := Int64NonNegativeMulMod(D5, Q.Y, FFiniteFieldSize);

  if (D1 = D2) and (D4 = D5) then // P.X/P.Z^2 = Q.X/Q.Z^2 并且 P.Y/P.Z^3 = Q.Y/Q.Z^3，说明是同一个点
  begin
    // 同一个点，切线法
    // D1 := 3 * PX^2 + A * PZ^4
    T := Int64NonNegativeMulMod(P.Z, P.Z, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, T, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, FCoefficientA, FFiniteFieldSize);
    D1 := Int64NonNegativeMulMod(P.X, P.X, FFiniteFieldSize);
    D1 := Int64NonNegativeMulMod(D1, 3, FFiniteFieldSize);
    D1 := Int64NonNegativeAddMod(D1, T, FFiniteFieldSize);

    // D2 := 4 * PX * PY^2
    D2 := Int64NonNegativeMulMod(P.Y, P.Y, FFiniteFieldSize);
    D2 := Int64NonNegativeMulMod(D2, P.X, FFiniteFieldSize);
    D2 := Int64NonNegativeMulMod(D2, 4, FFiniteFieldSize);

    // D3 := 8 * PY^4
    D3 := Int64NonNegativeMulMod(P.Y, P.Y, FFiniteFieldSize);
    D3 := Int64NonNegativeMulMod(D3, D3, FFiniteFieldSize);
    D3 := Int64NonNegativeMulMod(D3, 8, FFiniteFieldSize);

    // X := D1^2 - 2 * D2
    Sum.X := Int64NonNegativeMulMod(D1, D1, FFiniteFieldSize);
    T := Int64NonNegativeAddMod(D2, D2, FFiniteFieldSize);
    Sum.X := Int64NonNegativeAddMod(Sum.X, -T, FFiniteFieldSize);

    // Y := D1 * (D2 - X) - D3
    T := Int64NonNegativeAddMod(D2, -Sum.X, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(D1, T, FFiniteFieldSize);
    T := Int64NonNegativeAddMod(T, -D3, FFiniteFieldSize); // 先不给 Sum.Y 赋值，免得可能影响 P.Y

    // Z := 2 * PY * PZ
    Sum.Z := Int64NonNegativeMulMod(P.Y, P.Z, FFiniteFieldSize);
    Sum.Z := Int64NonNegativeAddMod(Sum.Z, Sum.Z, FFiniteFieldSize);

    Sum.Y := T; // P.Y 和 P.Z 都用过后，再给 Sum.Y 赋值
  end
  else  // 不同点，割线法
  begin
    // 因为有不同的 Z 存在，得这样判断，同样不用求逆
    if D1 = D2 then
    begin
      if D4 + D5 = FFiniteFieldSize then // X 相等且 Y 互补
      begin
        Sum.X := 0;
        Sum.Y := 0;
        Sum.Z := 0;
        Exit;
      end
      else // X 相等且 Y 不互补，没法相加
        raise ECnEccException.CreateFmt('Can NOT Calucate Jacobian %d,%d,%d + %d,%d,%d',
          [P.X, P.Y, P.Z, Q.X, Q.Y, Q.Z]);
    end;

    // D3 := D1 - D2
    D3 := Int64NonNegativeAddMod(D1, -D2, FFiniteFieldSize);

    // D6 := D4 - D5
    D6 := Int64NonNegativeAddMod(D4, -D5, FFiniteFieldSize);

    // D7 := D1 + D2
    D7 := Int64NonNegativeAddMod(D1, D2, FFiniteFieldSize);

    // D8 := D4 + D5
    D8 := Int64NonNegativeAddMod(D4, D5, FFiniteFieldSize);

    // X := D6^2 - D7 * D3^2
    Sum.X := Int64NonNegativeMulMod(D6, D6, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(D3, D3, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, D7, FFiniteFieldSize);
    Sum.X := Int64NonNegativeAddMod(Sum.X, -T, FFiniteFieldSize);

    // D9 := D7 * D3^2 - 2 * X
    D9 := Int64NonNegativeMulMod(D3, D3, FFiniteFieldSize);
    D9 := Int64NonNegativeMulMod(D9, D7, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(Sum.X, 2, FFiniteFieldSize);
    D9 := Int64NonNegativeAddMod(D9, -T, FFiniteFieldSize);

    // Y := (D9 * D6 - D8 * D3^3) / 2
    T := Int64NonNegativeMulMod(D3, D3, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, D3, FFiniteFieldSize);
    T := Int64NonNegativeMulMod(T, D8, FFiniteFieldSize);
    Sum.Y := Int64NonNegativeMulMod(D6, D9, FFiniteFieldSize);
    Sum.Y := Int64NonNegativeAddMod(Sum.Y, -T, FFiniteFieldSize);

    if F2Inverse = 0 then
      F2Inverse := MyInt64ModularInverse(2, FFiniteFieldSize); // 除以 2
    Sum.Y := Int64NonNegativeMulMod(Sum.Y, F2Inverse, FFiniteFieldSize);

    // Z := PZ * QZ * D3
    Sum.Z := Int64NonNegativeMulMod(P.Z, Q.Z, FFiniteFieldSize);
    Sum.Z := Int64NonNegativeMulMod(Sum.Z, D3, FFiniteFieldSize);
  end;
end;

function TCnInt64Ecc.Lucas(X, P: Int64; out Y: Int64): Boolean;
var
  G, U, V, Z: Int64;
begin
  Result := False;
  G := X;

  while True do
  begin
    // 随机取 X
    X := RandomInt64LessThan(P);

    // 再计算 Lucas 序列中的 V，其下标 K 为 (P+1)/2
    CnLucasVSequenceMod(X, G, (P + 1) shr 1, P, U, V);

    // V 偶则直接右移 1 再 mod P，V 奇则加 P 再右移 1
    if (V and 1) = 0 then
      Z := (V shr 1) mod P
    else
      Z := (V + P) shr 1;
    // Z := (V div 2) mod P;

    if Int64MultipleMod(Z, Z, P) = G then
    begin
      Y := Z;
      Result := True;
      Exit;
    end
    else if (U > 1) and (U < P - 1) then
      Break;
  end;
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

  if K > 1 then
  begin
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
end;

{$WARNINGS OFF}

function TCnInt64Ecc.PlainToPoint(Plain: Int64;
  var OutPoint: TCnInt64EccPoint): Boolean;
var
  X3, AX, B, G, Y, Z: Int64;
begin
  Result := False;
  if Plain = 0 then
  begin
    OutPoint.X := 0;
    OutPoint.Y := 0;
    Result := True;
    Exit;
  end;

  // 解方程求 Y： (y^2 - (Plain^3 + A * Plain + B)) mod p = 0
  // 注意 Plain 如果太大，计算过程中会溢出，不好处理，只能用分配律。
  // (Y^2 mod p - Plain ^ 3 mod p - A * Plain mod p - B mod p) mod p = 0;
  X3 := MontgomeryPowerMod(Plain, 3, FFiniteFieldSize);
  AX := Int64MultipleMod(FCoefficientA, Plain, FFiniteFieldSize);
  B := FCoefficientB mod FFiniteFieldSize;

  G := (X3 + AX + B) mod FFiniteFieldSize; // 如果不溢出的话
  if G = 0 then   // 如果 X^3 + AX + B 为 0，则直接返回 (Plain, 0) 并且肯定满足曲线方程
  begin
    OutPoint.X := Plain;
    OutPoint.Y := 0;
    Result := True;
    Exit;
  end;

  // 化为 Y^2 = N * p + B 要求找出 N 让右边为完全平方数，再求 Y 的正值
  // 要是硬算 N 从 0 开始加 1 遍历并开方计算是否完全平方数会特慢，不能这么整
  // 改用二次剩余素数模的快速求法，根据素数 P 的特性分三种：

  case FSizePrimeType of
  pt4U3:  // 参考自《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节
    begin
      Y := MontgomeryPowerMod(G, FSizeUFactor + 1, FFiniteFieldSize);
      Z := Int64MultipleMod(Y, Y, FFiniteFieldSize);
      if Z = G then
      begin
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end;
    end;
  pt8U5:  // 参考自《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节
    begin
      Z := MontgomeryPowerMod(G, 2 * FSizeUFactor + 1, FFiniteFieldSize);
      if Z = 1 then
      begin
        Y := MontgomeryPowerMod(G, FSizeUFactor + 1, FFiniteFieldSize);
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end
      else
      begin
        Z := FFiniteFieldSize - Z;
        if Z = 1 then
        begin
          // y = (2g * (4g)^u) mod p = (2g mod p * (4^u * g^u) mod p) mod p
          Y := (Int64MultipleMod(G, 2, FFiniteFieldSize) *
            MontgomeryPowerMod(4, FSizeUFactor, FFiniteFieldSize) *
            MontgomeryPowerMod(G, FSizeUFactor, FFiniteFieldSize)) mod FFiniteFieldSize;
          OutPoint.X := Plain;
          OutPoint.Y := Y;
          Result := True;
        end;
      end;
    end;
  pt8U1: // 参考自 wikipedia 上的 Tonelli-Shanks 二次剩余求解算法以及 IEEE P1363 里的 Lucas 序列算法
    begin
{$IFDEF USE_LUCAS}
      // 《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节 Lucas 序列计算出来的结果实在不对
      if Lucas(G, FFiniteFieldSize, Y) then
      begin
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end;
{$ELSE}
      //  改用 Tonelli-Shanks 算法进行模素数二次剩余求解，但内部先要通过勒让德符号判断其根是否存在，否则会陷入死循环
      if TonelliShanks(G, FFiniteFieldSize, Y) then
      begin
        OutPoint.X := Plain;
        OutPoint.Y := Y;
        Result := True;
      end;
{$ENDIF}
    end;
  end;
end;

{$WARNINGS ON}

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

    Y := MyInt64ModularInverse(Y, FFiniteFieldSize);
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
    X := MyInt64ModularInverse(X, FFiniteFieldSize);
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
    if Sum.X > 0 then                      // 如果 X 刚好整除，则是 0
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
    if Sum.Y > 0 then                      // 如果 Y 刚好整除，则是 0
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

procedure TCnInt64Ecc.AffinePointInverse(var P: TCnInt64Ecc3Point);
begin
  P.Y := (FFiniteFieldSize * P.Z - (P.Y mod FFiniteFieldSize)) mod FFiniteFieldSize;
end;

procedure TCnInt64Ecc.JacobianPointInverse(var P: TCnInt64Ecc3Point);
begin
  P.Y := (FFiniteFieldSize * P.Z * P.Z * P.Z - (P.Y mod FFiniteFieldSize)) mod FFiniteFieldSize;
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

function TCnInt64Ecc.TonelliShanks(X, P: Int64; out Y: Int64): Boolean;
var
  I: Integer;
  Q, S, Z, C, R, T, M, B: Int64;
begin
  Result := False;
  if (X <= 0) or (P <= 0) or (X >= P) then
    Exit;

  // 先要通过勒让德符号判断其根是否存在，否则下面会陷入死循环
  if CnInt64Legendre(X, P) <> 1 then
    Exit;

  S := 0;
  Q := P - 1;
  while (Q mod 2) = 0 do
  begin
    Q := Q shr 1;
    Inc(S);
  end;

  Z := 2;
  while Z < P do
  begin
    if CnInt64Legendre(Z, P) = -1 then
      Break;
    Inc(Z);
  end;

  // 先找一个 Z 满足 针对 P 的勒让德符号为 -1
  C := MontgomeryPowerMod(Z, Q, P);
  R := MontgomeryPowerMod(X, (Q + 1) div 2, P);
  T := MontgomeryPowerMod(X, Q, P);
  M := S;

  while True do
  begin
    if T mod P = 1 then
      Break;

    for I := 1 to M - 1 do
    begin
      if MontgomeryPowerMod(T, 1 shl I, P) = 1 then
        Break;
    end;

    B := MontgomeryPowerMod(C, 1 shl (M - I - 1), P);
    M := I; // M 每回都会减小，算法收敛

    R := Int64MultipleMod(R, B, P);
    T := Int64MultipleMod(Int64MultipleMod(T, B, P),
      B mod P, P); // T*B*B mod P = (T*B mod P) * (B mod P) mod P
    C := Int64MultipleMod(B, B, P);
  end;
  Y := (R mod P + P) mod P;
  Result := True;
end;

function TCnInt64Ecc.GetJInvariance: Int64;
var
  D, T: Int64;
begin
{
  对于椭圆曲线方程 y^2 + a1*xy + a3*y = x^3 + a2*x^2 + a4*x + a6
  映射到 y^2 = x^3 + Ax + B，可得 a1 = 0，a2 = 0，a3 = 0，a4 = A，a6 = B
  则：

    b2 = a1^2 + 4a2 = 0
    b4 = a1*a3 + 2a4 = 2A
    b6 = a3^2 + 4a6 = 4B
    b8 = a1^2*a6 + 4a2*a6 - a1*a3*a4 + a2*a3^2 - a4^2 = -A^2

    c4 = b2^2 - 24b4 = -48A
    c6 = b2^3 + 36b2*b4 - 216b6 = -864B

    Delta = -b2^2*b8 -8b4^3 -27b6^2 + 9b2*b4*b6 = -64A^3 - 432B^2

    j 不变量 = c4^3 / Delta = (-110592 * A^3) / (-64A^3 - 432B^2)
}

  D := GetDelta;
  D := MyInt64ModularInverse(D, FFiniteFieldSize);
  T := Int64NonNegativeMulMod(-110592, FCoefficientA, FFiniteFieldSize);
  T := Int64NonNegativeMulMod(T, FCoefficientA, FFiniteFieldSize);
  T := Int64NonNegativeMulMod(T, FCoefficientA, FFiniteFieldSize);
  Result := Int64NonNegativeMulMod(T, D, FFiniteFieldSize);
end;

function TCnInt64Ecc.GetDelta: Int64;
begin
{
  对于椭圆曲线方程 y^2 + a1*xy + a3*y = x^3 + a2*x^2 + a4*x + a6
  映射到 y^2 = x^3 + Ax + B，可得 a1 = 0，a2 = 0，a3 = 0，a4 = A，a6 = B
  则：

    b2 = a1^2 + 4a2 = 0
    b4 = a1*a3 + 2a4 = 2A
    b6 = a3^2 + 4a6 = 4B
    b8 = a1^2*a6 + 4a2*a6 - a1*a3*a4 + a2*a3^2 - a4^2 = -A^2

    c4 = b2^2 - 24b4 = -48A
    c6 = b2^3 + 36b2*b4 - 216b6 = -864B

    Delta = -b2^2*b8 -8b4^3 -27b6^2 + 9b2*b4*b6 = -64A^3 - 432B^2
}
  Result := -64 * FCoefficientA * FCoefficientA * FCoefficientA
    - 432 * FCoefficientB * FCoefficientB;
end;

function TCnInt64Ecc.PointToPlain(var Point: TCnInt64EccPoint): Int64;
begin
  Result := Point.X;
end;

{ TCnEccPoint }

procedure TCnEccPoint.Assign(Source: TPersistent);
begin
  if Source is TCnEccPoint then
  begin
    BigNumberCopy(FX, (Source as TCnEccPoint).X);
    BigNumberCopy(FY, (Source as TCnEccPoint).Y);
  end
  else if Source is TCnEcc3Point then
  begin
    BigNumberCopy(FX, (Source as TCnEcc3Point).X);
    BigNumberCopy(FY, (Source as TCnEcc3Point).Y);
    if FX.IsZero and FY.IsZero then
      (Source as TCnEcc3Point).Z.SetZero
    else
      (Source as TCnEcc3Point).Z.SetOne;
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

procedure TCnEccPoint.SetBase64(const Buf: AnsiString; Ecc: TCnEcc);
var
  B: TBytes;
begin
  if Base64Decode(string(Buf), B) = ECN_BASE64_OK then
    SetHex(AnsiString(BytesToHex(B)), Ecc);
end;

procedure TCnEccPoint.SetHex(const Buf: AnsiString; Ecc: TCnEcc);
var
  C: Integer;
  S: AnsiString;
  P: TCnEccPoint;
begin
  if Length(Buf) < 4 then
    raise ECnEccException.Create(SCnErrorEccKeyData);

  C := StrToIntDef(string(Copy(Buf, 1, 2)), 0);
  S := Copy(Buf, 3, MaxInt);

  if (C = EC_PUBLICKEY_UNCOMPRESSED) or (C = EC_PUBLICKEY_COMPRESSED_ODD) or
    (C = EC_PUBLICKEY_COMPRESSED_EVEN) then
  begin
    // 前导字节后面的内容，要不就是一半公钥一半私钥，长度得相等，要不就是公钥 X，均要求被 4 整除
    if (Length(S) mod 4) <> 0 then
    begin
      // 前导字节后面的内容长度不对，这里重新把前导字节算进来，当公私钥一块判断
      if (Length(Buf) mod 4) <> 0 then // 如果长度还不对，则出错
        raise ECnEccException.Create(SCnErrorEccKeyData);

      // 把前导字节算进来的长度是对的，直接劈开 Buf 赋值
      C := Length(Buf) div 2;
      FX.SetHex(Copy(Buf, 1, C));
      FY.SetHex(Copy(Buf, C + 1, MaxInt));
    end
    else // 前导字节内容后的长度对
    begin
      if C = EC_PUBLICKEY_UNCOMPRESSED then
      begin
        C := Length(S) div 2;
        FX.SetHex(Copy(S, 1, C));
        FY.SetHex(Copy(S, C + 1, MaxInt));
      end
      else if (C = EC_PUBLICKEY_COMPRESSED_EVEN) or (C = EC_PUBLICKEY_COMPRESSED_ODD) then
      begin
        FX.SetHex(S);
        FY.SetZero;  // 压缩格式全是公钥 X，Y 先 0，再去求解

        if Ecc <> nil then
        begin
          P := TCnEccPoint.Create;
          try
            // 将 Y 是奇偶的信息带出去供外界在求得的两个 Y 值中求解
            if Ecc.PlainToPoint(FX, P) then
            begin
              if P.Y.IsOdd and (C = EC_PUBLICKEY_COMPRESSED_ODD) then
                BigNumberCopy(FY, P.Y)
              else
              begin
                Ecc.PointInverse(P);
                BigNumberCopy(FY, P.Y);
              end;
            end;
          finally
            P.Free;
          end;
        end;
      end
      else  // 前导字节内容非法
        raise ECnEccException.Create(SCnErrorEccKeyData);
    end;
  end
  else // 前导字节非合法值，说明无前导字节
  begin
    if (Length(Buf) mod 4) <> 0 then // 一半公钥一半私钥，长度得相等
      raise ECnEccException.Create(SCnErrorEccKeyData);

    C := Length(Buf) div 2;
    FX.SetHex(Copy(Buf, 1, C));
    FY.SetHex(Copy(Buf, C + 1, MaxInt));
  end;
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

function TCnEccPoint.ToBase64(FixedLen: Integer): string;
var
  B: Byte;
  Stream: TMemoryStream;
begin
  if FY.IsZero then
    B := 3          // 不知道 Y 具体值，无法确定奇偶，暂时写 03
  else
    B := 4;

  Stream := TMemoryStream.Create;
  try
    Stream.Write(B, SizeOf(B));
    BigNumberWriteBinaryToStream(FX, Stream, FixedLen);

    if not FY.IsZero then
      BigNumberWriteBinaryToStream(FY, Stream, FixedLen);

    Base64Encode(Stream.Memory, Stream.Size, Result);
  finally
    Stream.Free;
  end;
end;

function TCnEccPoint.ToHex(FixedLen: Integer): string;
begin
  if FY.IsZero then
    Result := '03' + FX.ToHex(FixedLen) // 不知道 Y 具体值，无法确定奇偶，暂时写 03
  else
    Result := '04' + FX.ToHex(FixedLen) + FY.ToHex(FixedLen);
end;

function TCnEccPoint.ToString: string;
begin
  Result := CnEccPointToHex(Self);
end;

{ TCnEcc }

procedure TCnEcc.CalcX3AddAXAddB(X: TCnBigNumber);
var
  M: TCnBigNumber;
begin
  M := FEccBigNumberPool.Obtain;
  try
    BigNumberCopy(M, X);
    BigNumberMul(X, X, X);
    BigNumberMul(X, X, M); // X: X^3

    BigNumberMul(M, M, FCoefficientA); // M: A*X
    BigNumberAdd(X, X, M);             // X: X^3 + A*X
    BigNumberAdd(X, X, FCoefficientB); // X: X^3 + A*X + B
  finally
    FEccBigNumberPool.Recycle(M);
  end;
end;

constructor TCnEcc.Create(const A, B, FieldPrime, GX, GY, Order: AnsiString; H: Integer);
begin
  Create;
  Load(A, B, FIeldPrime, GX, GY, Order, H);
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

  // 无需提前创建
//  F2Inverse := TCnBigNumber.Create;
//  F2Inverse.SetZero;
end;

constructor TCnEcc.Create(Predefined: TCnEccCurveType);
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
begin
  F2Inverse.Free; // 无需提前创建
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
    raise ECnEccException.Create(SCnErrorEccKeyData);

  RandomKey := FEccBigNumberPool.Obtain;
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
    FEccBigNumberPool.Recycle(RandomKey);
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
  X, Y: TCnBigNumber;
begin
  Result := False;
  if P = nil then
    Exit;

  X := nil;
  Y := nil;

  try
    X := FEccBigNumberPool.Obtain;
    if BigNumberCopy(X, P.X) = nil then
      Exit;

    Y := FEccBigNumberPool.Obtain;
    if BigNumberCopy(Y, P.Y) = nil then
      Exit;

    if not BigNumberDirectMulMod(Y, Y, Y, FFiniteFieldSize) then // Y: Y^2 mod P
      Exit;

    CalcX3AddAXAddB(X);                   // X: X^3 + A*X + B
    if not BigNumberMod(X, X, FFiniteFieldSize) then // X: (X^3 + A*X + B) mod P
      Exit;

    Result := BigNumberCompare(X, Y) = 0;
  finally
    FEccBigNumberPool.Recycle(Y);
    FEccBigNumberPool.Recycle(X);
  end;
end;

procedure TCnEcc.Load(Predefined: TCnEccCurveType);
begin
  Load(ECC_PRE_DEFINED_PARAMS[Predefined].A, ECC_PRE_DEFINED_PARAMS[Predefined].B,
    ECC_PRE_DEFINED_PARAMS[Predefined].P, ECC_PRE_DEFINED_PARAMS[Predefined].X,
    ECC_PRE_DEFINED_PARAMS[Predefined].Y, ECC_PRE_DEFINED_PARAMS[Predefined].N,
    StrToIntDef(string(ECC_PRE_DEFINED_PARAMS[Predefined].H), 1));
end;

procedure TCnEcc.Load(const A, B, FieldPrime, GX, GY, Order: AnsiString; H: Integer);
var
  R: Cardinal;
begin
  FGenerator.X.SetHex(GX);
  FGenerator.Y.SetHex(GY);
  FCoefficientA.SetHex(A);
  FCoefficientB.SetHex(B);
  FFiniteFieldSize.SetHex(FieldPrime);
  FOrder.SetHex(Order);
  FCoFactor := H;

  // TODO: 要确保 4*a^3+27*b^2 <> 0

//  由调用者保证有限域边界为素数
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

function TCnEcc.GetBytesCount: Integer;
begin
  Result := FFiniteFieldSize.GetBytesCount;
end;

procedure TCnEcc.NormalMultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
var
  I, C: Integer;
  E, R: TCnEccPoint;
begin
  if BigNumberIsNegative(K) then
  begin
    // BigNumberSetNegative(K, False);
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

    C := BigNumberGetBitsCount(K);
    for I := 0 to C - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        PointAddPoint(R, E, R);

      if I < C - 1 then
        PointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
  finally
    E.Free;
    R.Free;
  end;
end;

procedure TCnEcc.AffineMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point);
var
  I, C: Integer;
  E, R: TCnEcc3Point;
begin
  if BigNumberIsNegative(K) then
  begin
    // BigNumberSetNegative(K, False);
    AffinePointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    Point.X.SetZero;
    Point.Y.SetZero;
    Point.Z.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEcc3Point.Create;
    E := TCnEcc3Point.Create;

    E.X := Point.X;
    E.Y := Point.Y;
    E.Z := Point.Z;

    C := BigNumberGetBitsCount(K);
    for I := 0 to C - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        AffinePointAddPoint(R, E, R);

      if I < C - 1 then // 最后一次循环无需加 E
        AffinePointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
    Point.Z := R.Z;
  finally
    R.Free;
    E.Free;
  end;
end;

procedure TCnEcc.JacobianMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point);
var
  I, C: Integer;
  E, R: TCnEcc3Point;
begin
  if BigNumberIsNegative(K) then
  begin
    // BigNumberSetNegative(K, False);
    JacobianPointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    Point.X.SetZero;
    Point.Y.SetZero;
    Point.Z.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  R := nil;
  E := nil;

  try
    R := TCnEcc3Point.Create;
    E := TCnEcc3Point.Create;

    E.X := Point.X;
    E.Y := Point.Y;
    E.Z := Point.Z;

    C := BigNumberGetBitsCount(K);
    for I := 0 to C - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        JacobianPointAddPoint(R, E, R);

      if I < C - 1 then
        JacobianPointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
    Point.Z := R.Z;
  finally
    R.Free;
    E.Free;
  end;
end;

procedure TCnEcc.MultiplePoint(K: Int64; Point: TCnEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := FEccBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MultiplePoint(BK, Point);
  finally
    FEccBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnEcc.MultiplePoint(K: TCnBigNumber; Point: TCnEccPoint);
var
  P3: TCnEcc3Point;
begin
  P3 := TCnEcc3Point.Create;
  try
    CnEccPointToEcc3Point(Point, P3);
    AffineMultiplePoint(K, P3);
    CnAffinePointToEccPoint(P3, Point, FFiniteFieldSize);
  finally
    P3.Free;
  end;
end;

function TCnEcc.PlainToPoint(Plain: TCnBigNumber;
  OutPoint: TCnEccPoint): Boolean;
var
  X, Y, Z, U, R, T, X3: TCnBigNumber;
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
  X3 := nil;

  try
    X := FEccBigNumberPool.Obtain;
    Y := FEccBigNumberPool.Obtain;
    Z := FEccBigNumberPool.Obtain;
    U := FEccBigNumberPool.Obtain;
    X3 := FEccBigNumberPool.Obtain;

    BigNumberCopy(X, Plain);
    BigNumberCopy(U, FSizeUFactor);

    CalcX3AddAXAddB(X);
    BigNumberMod(X, X, FFiniteFieldSize);
    BigNumberCopy(X3, X);    // 保存原始 g

    if X3.IsZero then // 如果 (X^3 + AX + B) mod p 为 0，则直接返回 (Plain, 0) 并且肯定满足曲线方程
    begin
      BigNumberCopy(OutPoint.X, Plain);
      OutPoint.Y.SetZero;
      Result := True;
      Exit;
    end;

    // 参考自《SM2椭圆曲线公钥密码算法》附录 B 中的“模素数平方根的求解”一节，这里 g 是 X 经过运算后的方程右半部分值
    case FSizePrimeType of
      pt4U3:
        begin
          // 结果是 g^(u+1) mod p
          BigNumberAddWord(U, 1);
          BigNumberMontgomeryPowerMod(Y, X, U, FFiniteFieldSize);
          BigNumberDirectMulMod(Z, Y, Y, FFiniteFieldSize);
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
          R := FEccBigNumberPool.Obtain;
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
              T := FEccBigNumberPool.Obtain;
              BigNumberMontgomeryPowerMod(T, X, FSizeUFactor, FFiniteFieldSize); // T: (4g)^u mod p
              BigNumberDirectMulMod(Y, R, T, FFiniteFieldSize);

              BigNumberCopy(OutPoint.X, Plain);
              BigNumberCopy(OutPoint.Y, Y);
              Result := True;
            end;
          end;
        end;
      pt8U1: // Lucas 序列计算法与 Tonelli-Shanks 算法均能进行模素数二次剩余求解
        begin
{$IFDEF USE_LUCAS}
          if BigNumberLucas(OutPoint.Y, X3, FFiniteFieldSize) then
          begin
            BigNumberCopy(OutPoint.X, Plain);
            Result := True;
          end;
{$ELSE}
          if BigNumberTonelliShanks(OutPoint.Y, X3, FFiniteFieldSize) then
          begin
            BigNumberCopy(OutPoint.X, Plain);
            Result := True;
          end;
{$ENDIF}
        end;
    end;
  finally
    FEccBigNumberPool.Recycle(X);
    FEccBigNumberPool.Recycle(Y);
    FEccBigNumberPool.Recycle(Z);
    FEccBigNumberPool.Recycle(U);
    FEccBigNumberPool.Recycle(R);
    FEccBigNumberPool.Recycle(T);
    FEccBigNumberPool.Recycle(X3);
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

      X := FEccBigNumberPool.Obtain;
      Y := FEccBigNumberPool.Obtain;
      K := FEccBigNumberPool.Obtain;

      // X := 3 * P.X * P.X + CoefficientA;
      BigNumberMul(X, P.X, P.X);             // X: P.X^2
      BigNumberMulWord(X, 3);                // X: 3 * P.X^2
      BigNumberAdd(X, X, FCoefficientA);     // X: 3 * P.X^2 + A

      // Y := 2 * P.Y;
      BigNumberCopy(Y, P.Y);
      BigNumberMulWord(Y, 2);                // Y: 2 * P.Y

      A := FEccBigNumberPool.Obtain;
      BigNumberCopy(A, Y);
      BigNumberModularInverse(Y, A, FFiniteFieldSize); // Y := Y^-1

      // K := X * Y mod FFiniteFieldSize;
      BigNumberDirectMulMod(K, X, Y, FFiniteFieldSize);      // 得到斜率
    end
    else // 是不同点
    begin
      if BigNumberCompare(P.X, Q.X) = 0 then // 如果 X 相等，要判断 Y 是不是互反，是则和为 0，不是则挂了
      begin
        A := FEccBigNumberPool.Obtain;
        BigNumberAdd(A, P.Y, Q.Y);
        if BigNumberCompare(A, FFiniteFieldSize) = 0 then  // 互反，和为 0
          Sum.SetZero
        else                                               // 不互反，挂了
          raise ECnEccException.CreateFmt('Can NOT Calucate %s,%s + %s,%s',
            [P.X.ToDec, P.Y.ToDec, Q.X.ToDec, Q.Y.ToDec]);

        Exit;
      end;

      // 到这里，X 确定不同，斜率 K := ((Q.Y - P.Y) / (Q.X - P.X)) mod p
      X := FEccBigNumberPool.Obtain;
      Y := FEccBigNumberPool.Obtain;
      K := FEccBigNumberPool.Obtain;

      // Y := Q.Y - P.Y;
      // X := Q.X - P.X;
      BigNumberSub(Y, Q.Y, P.Y);
      BigNumberSub(X, Q.X, P.X);

      A := FEccBigNumberPool.Obtain;
      BigNumberCopy(A, X);
      BigNumberModularInverse(X, A, FFiniteFieldSize);
      BigNumberDirectMulMod(K, Y, X, FFiniteFieldSize);      // 得到斜率
    end;

    BigNumberCopy(X, K);
    BigNumberMul(X, X, K);
    BigNumberSub(X, X, P.X);
    BigNumberSub(X, X, Q.X);    //  X := K * K - P.X - Q.X;

    SX := FEccBigNumberPool.Obtain;
    if BigNumberIsNegative(X) then // 负值的模等于正值的模被模数减
    begin
      BigNumberSetNegative(X, False);
      BigNumberMod(SX, X, FFiniteFieldSize);
      if not SX.IsZero then                   // 刚好整除时无需减，保持 0，避免出现 X 值等于有限域上界的情况
        BigNumberSub(SX, FFiniteFieldSize, SX);
    end
    else
      BigNumberMod(SX, X, FFiniteFieldSize);

    // Ysum = (K * (X1 - Xsum) - Y1) mod p  注意要取负
    //   也 = (K * (X2 - Xsum) - Y2) mod p  注意要取负
    BigNumberSub(X, P.X, SX);
    BigNumberMul(Y, K, X);
    BigNumberSub(Y, Y, P.Y);

    SY := FEccBigNumberPool.Obtain;
    if BigNumberIsNegative(Y) then
    begin
      BigNumberSetNegative(Y, False);
      BigNumberMod(SY, Y, FFiniteFieldSize);
      if not SY.IsZero then                     // 刚好整除时无需减，保持 0，避免出现 Y 值等于有限域上界的情况
        BigNumberSub(SY, FFiniteFieldSize, SY);
    end
    else
      BigNumberMod(SY, Y, FFiniteFieldSize);

    BigNumberCopy(Sum.X, SX);
    BigNumberCopy(Sum.Y, SY);
  finally
    FEccBigNumberPool.Recycle(K);
    FEccBigNumberPool.Recycle(X);
    FEccBigNumberPool.Recycle(Y);
    FEccBigNumberPool.Recycle(A);
    FEccBigNumberPool.Recycle(SX);
    FEccBigNumberPool.Recycle(SY);
  end;
end;

procedure TCnEcc.AffinePointAddPoint(P, Q, Sum: TCnEcc3Point);
var
  T, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11: TCnBigNumber;
begin
  if P.Z.IsZero then
  begin
    BigNumberCopy(Sum.X, Q.X);
    BigNumberCopy(Sum.Y, Q.Y);
    BigNumberCopy(Sum.Z, Q.Z);
    Exit;
  end
  else if Q.Z.IsZero then
  begin
    BigNumberCopy(Sum.X, P.X);
    BigNumberCopy(Sum.Y, P.Y);
    BigNumberCopy(Sum.Z, P.Z);
    Exit;
  end;

  T := nil;
  D1 := nil;
  D2 := nil;
  D3 := nil;
  D4 := nil;
  D5 := nil;
  D6 := nil;
  D7 := nil;
  D8 := nil;
  D9 := nil;
  D10 := nil;
  D11 := nil;

  try
    T := FEccBigNumberPool.Obtain;
    D1 := FEccBigNumberPool.Obtain;
    D2 := FEccBigNumberPool.Obtain;
    D3 := FEccBigNumberPool.Obtain;
    D4 := FEccBigNumberPool.Obtain;
    D5 := FEccBigNumberPool.Obtain;
    D6 := FEccBigNumberPool.Obtain;
    D7 := FEccBigNumberPool.Obtain;
    D8 := FEccBigNumberPool.Obtain;
    D9 := FEccBigNumberPool.Obtain;
    D10 := FEccBigNumberPool.Obtain;
    D11 := FEccBigNumberPool.Obtain;

    // D1 := px * qz
    BigNumberDirectMulMod(D1, P.X, Q.Z, FFiniteFieldSize);

    // D2 := qx * pz
    BigNumberDirectMulMod(D2, Q.X, P.Z, FFiniteFieldSize);

    // D4 := py * qz
    BigNumberDirectMulMod(D4, P.Y, Q.Z, FFiniteFieldSize);

    // D5 := qy * pz
    BigNumberDirectMulMod(D5, Q.Y, P.Z, FFiniteFieldSize);

    if BigNumberEqual(D1, D2) and BigNumberEqual(D4, D5) then
    begin
      // 同一个点，切线法

      // D1 := 3 px^2 + A * pz^2
      BigNumberDirectMulMod(D1, P.X, P.X, FFiniteFieldSize);
      BigNumberMulWordNonNegativeMod(D1, D1, 3, FFiniteFieldSize);
      BigNumberDirectMulMod(T, P.Z, P.Z, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, FCoefficientA, FFiniteFieldSize);
      BigNumberAddMod(D1, D1, T, FFiniteFieldSize);

      // D2 := 2 * py * pz
      BigNumberMulWordNonNegativeMod(D2, P.Y, 2, FFiniteFieldSize);
      BigNumberDirectMulMod(D2, D2, P.Z, FFiniteFieldSize);

      // D3 := py^2
      BigNumberDirectMulMod(D3, P.Y, P.Y, FFiniteFieldSize);

      // D4 := D3 * px * pz
      BigNumberDirectMulMod(D4, D3, P.X, FFiniteFieldSize);
      BigNumberDirectMulMod(D4, D4, P.Z, FFiniteFieldSize);

      // D5 := D2^2
      BigNumberDirectMulMod(D5, D2, D2, FFiniteFieldSize);

      // D6 := D1^2 - 8 * D4
      BigNumberDirectMulMod(D6, D1, D1, FFiniteFieldSize);
      BigNumberMulWordNonNegativeMod(T, D4, 8, FFiniteFieldSize);
      BigNumberSubMod(D6, D6, T, FFiniteFieldSize);

      // X := D2 * D6
      BigNumberDirectMulMod(Sum.X, D2, D6, FFiniteFieldSize);

      // Y := D1 * (4 * D4 - D6) - 2 * D5 * D3
      BigNumberMulWordNonNegativeMod(T, D4, 4, FFiniteFieldSize);
      BigNumberSubMod(T, T, D6, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, D1, FFiniteFieldSize);

      BigNumberDirectMulMod(Sum.Y, D3, D5, FFiniteFieldSize);
      BigNumberAddMod(Sum.Y, Sum.Y, Sum.Y, FFiniteFieldSize);
      BigNumberSubMod(Sum.Y, T, Sum.Y, FFiniteFieldSize);

      // Z := D2 * D5
      BigNumberDirectMulMod(Sum.Z, D2, D5, FFiniteFieldSize);
    end
    else // 不同点，割线法
    begin
      // 因为有不同的 Z 存在，得这样判断，同样不用求逆
      if BigNumberEqual(D1, D2) then
      begin
        BigNumberAdd(T, D4, D5);
        if BigNumberEqual(T, FFiniteFieldSize) then // X 相等且 Y 互补
        begin
          Sum.X.SetZero;
          Sum.Y.SetZero;
          Sum.Z.SetZero;
          Exit;
        end
        else // X 相等且 Y 不互补，没法相加
          raise ECnEccException.CreateFmt('Can NOT Calucate Affine %d,%d,%d + %d,%d,%d',
            [P.X.ToDec, P.Y.ToDec, P.Z.ToDec, Q.X.ToDec, Q.Y.ToDec, Q.Z.ToDec]);
      end;

      // D3 := D1 - D2
      BigNumberSubMod(D3, D1, D2, FFiniteFieldSize);

      // D6 := D4 - D5
      BigNumberSubMod(D6, D4, D5, FFiniteFieldSize);

      // D7 := D1 + D2
      BigNumberAddMod(D7, D1, D2, FFiniteFieldSize);

      // D8 := pz * qz
      BigNumberDirectMulMod(D8, P.Z, Q.Z, FFiniteFieldSize);

      // D9 := D3 ^ 2
      BigNumberDirectMulMod(D9, D3, D3, FFiniteFieldSize);

      // D10 := D3 * D9
      BigNumberDirectMulMod(D10, D3, D9, FFiniteFieldSize);

      // D11 := D8 * D6 ^ 2 - D7 * D9
      BigNumberDirectMulMod(D11, D6, D6, FFiniteFieldSize);
      BigNumberDirectMulMod(D11, D11, D8, FFiniteFieldSize);
      BigNumberDirectMulMod(T, D7, D9, FFiniteFieldSize);
      BigNumberSubMod(D11, D11, T, FFiniteFieldSize);

      // Y := D6 * (D9 * D1 - D11) - D4 * D10
      BigNumberDirectMulMod(T, D9, D1, FFiniteFieldSize);
      BigNumberSubMod(T, T, D11, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, D6, FFiniteFieldSize);

      BigNumberDirectMulMod(Sum.Y, D4, D10, FFiniteFieldSize);
      BigNumberSubMod(Sum.Y, T, Sum.Y, FFiniteFieldSize);

      // X := D3 * D11
      BigNumberDirectMulMod(Sum.X, D3, D11, FFiniteFieldSize);

      // Z := D10 * D8
      BigNumberDirectMulMod(Sum.Z, D10, D8, FFiniteFieldSize);
    end;

    if Sum.Z.IsZero then
    begin
      Sum.X.SetZero;
      Sum.Y.SetZero;
    end;
  finally
    FEccBigNumberPool.Recycle(D11);
    FEccBigNumberPool.Recycle(D10);
    FEccBigNumberPool.Recycle(D9);
    FEccBigNumberPool.Recycle(D8);
    FEccBigNumberPool.Recycle(D7);
    FEccBigNumberPool.Recycle(D6);
    FEccBigNumberPool.Recycle(D5);
    FEccBigNumberPool.Recycle(D4);
    FEccBigNumberPool.Recycle(D3);
    FEccBigNumberPool.Recycle(D2);
    FEccBigNumberPool.Recycle(D1);
    FEccBigNumberPool.Recycle(T);
  end;
end;

procedure TCnEcc.JacobianPointAddPoint(P, Q, Sum: TCnEcc3Point);
var
  T, D1, D2, D3, D4, D5, D6, D7, D8, D9: TCnBigNumber;
begin
  if P.Z.IsZero then
  begin
    BigNumberCopy(Sum.X, Q.X);
    BigNumberCopy(Sum.Y, Q.Y);
    BigNumberCopy(Sum.Z, Q.Z);
    Exit;
  end
  else if Q.Z.IsZero then
  begin
    BigNumberCopy(Sum.X, P.X);
    BigNumberCopy(Sum.Y, P.Y);
    BigNumberCopy(Sum.Z, P.Z);
    Exit;
  end;

  T := nil;
  D1 := nil;
  D2 := nil;
  D3 := nil;
  D4 := nil;
  D5 := nil;
  D6 := nil;
  D7 := nil;
  D8 := nil;
  D9 := nil;

  try
    T := FEccBigNumberPool.Obtain;
    D1 := FEccBigNumberPool.Obtain;
    D2 := FEccBigNumberPool.Obtain;
    D3 := FEccBigNumberPool.Obtain;
    D4 := FEccBigNumberPool.Obtain;
    D5 := FEccBigNumberPool.Obtain;
    D6 := FEccBigNumberPool.Obtain;
    D7 := FEccBigNumberPool.Obtain;
    D8 := FEccBigNumberPool.Obtain;
    D9 := FEccBigNumberPool.Obtain;

    // D1 := PX * QZ^2
    BigNumberDirectMulMod(D1, Q.Z, Q.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(D1, D1, P.X, FFiniteFieldSize);

    // D2 := QX * PZ^2
    BigNumberDirectMulMod(D2, P.Z, P.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(D2, D2, Q.X, FFiniteFieldSize);

    // D4 := PY * QZ^3
    BigNumberDirectMulMod(D4, Q.Z, Q.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(D4, D4, Q.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(D4, D4, P.Y, FFiniteFieldSize);

    // D5 := QY * PZ^3
    BigNumberDirectMulMod(D5, P.Z, P.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(D5, D5, P.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(D5, D5, Q.Y, FFiniteFieldSize);

    if BigNumberEqual(D1, D2) and BigNumberEqual(D4, D5) then
    begin
      // 同一个点，切线法
      // D1 := 3 * PX^2 + A * PZ^4
      BigNumberDirectMulMod(T, P.Z, P.Z, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, T, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, FCoefficientA, FFiniteFieldSize);
      BigNumberDirectMulMod(D1, P.X, P.X, FFiniteFieldSize);
      BigNumberMulWordNonNegativeMod(D1, D1, 3, FFiniteFieldSize);
      BigNumberAddMod(D1, D1, T, FFiniteFieldSize);

      // D2 := 4 * PX * PY^2
      BigNumberDirectMulMod(D2, P.Y, P.Y, FFiniteFieldSize);
      BigNumberDirectMulMod(D2, D2, P.X, FFiniteFieldSize);
      BigNumberMulWordNonNegativeMod(D2, D2, 4, FFiniteFieldSize);

      // D3 := 8 * PY^4
      BigNumberDirectMulMod(D3, P.Y, P.Y, FFiniteFieldSize);
      BigNumberDirectMulMod(D3, D3, D3, FFiniteFieldSize);
      BigNumberMulWordNonNegativeMod(D3, D3, 8, FFiniteFieldSize);

      // X := D1^2 - 2 * D2
      BigNumberDirectMulMod(Sum.X, D1, D1, FFiniteFieldSize);
      BigNumberAddMod(T, D2, D2, FFiniteFieldSize);
      BigNumberSubMod(Sum.X, Sum.X, T, FFiniteFieldSize);

      // Y := D1 * (D2 - X) - D3
      BigNumberSubMod(T, D2, Sum.X, FFiniteFieldSize);
      BigNumberDirectMulMod(T, D1, T, FFiniteFieldSize);
      BigNumberSubMod(T, T, D3, FFiniteFieldSize); // 先不给 Sum.Y 赋值，免得可能影响 P.Y

      // Z := 2 * PY * PZ
      BigNumberDirectMulMod(Sum.Z, P.Y, P.Z, FFiniteFieldSize);
      BigNumberAddMod(Sum.Z, Sum.Z, Sum.Z, FFiniteFieldSize);

      BigNumberCopy(Sum.Y, T); // P.Y 和 P.Z 都用过后，再给 Sum.Y 赋值
    end
    else // 不同点，割线法
    begin
      if BigNumberEqual(D1, D2) then
      begin
        BigNumberAdd(T, D4, D5);
        if BigNumberEqual(T, FFiniteFieldSize) then // X 相等且 Y 互补
        begin
          Sum.X.SetZero;
          Sum.Y.SetZero;
          Sum.Z.SetZero;
          Exit;
        end
        else // X 相等且 Y 不互补，没法相加
          raise ECnEccException.CreateFmt('Can NOT Calucate Jacobian %d,%d,%d + %d,%d,%d',
            [P.X.ToDec, P.Y.ToDec, P.Z.ToDec, Q.X.ToDec, Q.Y.ToDec, Q.Z.ToDec]);
      end;

      // D3 := D1 - D2
      BigNumberSubMod(D3, D1, D2, FFiniteFieldSize);

      // D6 := D4 - D5
      BigNumberSubMod(D6, D4, D5, FFiniteFieldSize);

      // D7 := D1 + D2
      BigNumberAddMod(D7, D1, D2, FFiniteFieldSize);

      // D8 := D4 + D5
      BigNumberAddMod(D8, D4, D5, FFiniteFieldSize);

      // X := D6^2 - D7 * D3^2
      BigNumberDirectMulMod(Sum.X, D6, D6, FFiniteFieldSize);
      BigNumberDirectMulMod(T, D3, D3, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, D7, FFiniteFieldSize);
      BigNumberSubMod(Sum.X, Sum.X, T, FFiniteFieldSize);

      // D9 := D7 * D3^2 - 2 * X
      BigNumberDirectMulMod(D9, D3, D3, FFiniteFieldSize);
      BigNumberDirectMulMod(D9, D9, D7, FFiniteFieldSize);
      BigNumberMulWordNonNegativeMod(T, Sum.X, 2, FFiniteFieldSize);
      BigNumberSubMod(D9, D9, T, FFiniteFieldSize);

      // Y := (D9 * D6 - D8 * D3^3) / 2
      BigNumberDirectMulMod(T, D3, D3, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, D3, FFiniteFieldSize);
      BigNumberDirectMulMod(T, T, D8, FFiniteFieldSize);
      BigNumberDirectMulMod(Sum.Y, D6, D9, FFiniteFieldSize);
      BigNumberSubMod(Sum.Y, Sum.Y, T, FFiniteFieldSize);

      if F2Inverse = nil then
      begin
        F2Inverse := TCnBigNumber.Create;
        T.SetWord(2);
        BigNumberModularInverse(F2Inverse, T, FFiniteFieldSize);
      end;
      BigNumberDirectMulMod(Sum.Y, Sum.Y, F2Inverse, FFiniteFieldSize);

      // Z := PZ * QZ * D3
      BigNumberDirectMulMod(Sum.Z, P.Z, Q.Z, FFiniteFieldSize);
      BigNumberDirectMulMod(Sum.Z, Sum.Z, D3, FFiniteFieldSize);
    end;
  finally
    FEccBigNumberPool.Recycle(D9);
    FEccBigNumberPool.Recycle(D8);
    FEccBigNumberPool.Recycle(D7);
    FEccBigNumberPool.Recycle(D6);
    FEccBigNumberPool.Recycle(D5);
    FEccBigNumberPool.Recycle(D4);
    FEccBigNumberPool.Recycle(D3);
    FEccBigNumberPool.Recycle(D2);
    FEccBigNumberPool.Recycle(D1);
    FEccBigNumberPool.Recycle(T);
  end;
end;

procedure TCnEcc.AffinePointSubPoint(P, Q, Diff: TCnEcc3Point);
var
  Inv: TCnEcc3Point;
begin
  Inv := TCnEcc3Point.Create;
  try
    Inv.X := Q.X;
    Inv.Y := Q.Y;
    Inv.Z := Q.Z;

    AffinePointInverse(Inv);
    AffinePointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

procedure TCnEcc.JacobianPointSubPoint(P, Q, Diff: TCnEcc3Point);
var
  Inv: TCnEcc3Point;
begin
  Inv := TCnEcc3Point.Create;
  try
    Inv.X := Q.X;
    Inv.Y := Q.Y;
    Inv.Z := Q.Z;

    JacobianPointInverse(Inv);
    JacobianPointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

procedure TCnEcc.PointInverse(P: TCnEccPoint);
begin
  if BigNumberIsNegative(P.Y) or (BigNumberCompare(P.Y, FFiniteFieldSize) >= 0) then
    raise ECnEccException.Create('Inverse Error.');

  BigNumberSub(P.Y, FFiniteFieldSize, P.Y);
end;

procedure TCnEcc.AffinePointInverse(P: TCnEcc3Point);
var
  T: TCnBigNumber;
begin
  T := FEccBigNumberPool.Obtain;
  try
    BigNumberDirectMulMod(T, P.Z, FFiniteFieldSize, FFiniteFieldSize);
    BigNumberSubMod(P.Y, T, P.Y, FFiniteFieldSize);
  finally
    FEccBigNumberPool.Recycle(T);
  end;
end;

procedure TCnEcc.JacobianPointInverse(P: TCnEcc3Point);
var
  T: TCnBigNumber;
begin
  T := FEccBigNumberPool.Obtain;
  try
    BigNumberDirectMulMod(T, P.Z, P.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, P.Z, FFiniteFieldSize);
    BigNumberDirectMulMod(T, T, FFiniteFieldSize, FFiniteFieldSize);
    BigNumberSubMod(P.Y, T, P.Y, FFiniteFieldSize);
  finally
    FEccBigNumberPool.Recycle(T);
  end;
end;

procedure TCnEcc.PointSubPoint(P, Q, Diff: TCnEccPoint);
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

function TCnEcc.PointToPlain(Point: TCnEccPoint; OutPlain: TCnBigNumber): Boolean;
begin
  Result := False;
  if (Point <> nil) and (OutPlain <> nil) and IsPointOnCurve(Point) then
  begin
    BigNumberCopy(OutPlain, Point.X);
    Result := True;
  end;
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

// ============== 普通二元坐标点到三元仿射坐标/雅可比坐标点转换 ================

function CnInt64EccPointToEcc3Point(var P: TCnInt64EccPoint; var P3: TCnInt64Ecc3Point): Boolean;
begin
  P3.X := P.X;
  P3.Y := P.Y;

  if (P3.X = 0) and (P3.Y = 0) then
    P3.Z := 0
  else
    P3.Z := 1;
  Result := True;
end;

function CnInt64AffinePointToEccPoint(var P3: TCnInt64Ecc3Point;
  var P: TCnInt64EccPoint; Prime: Int64): Boolean;
var
  V: Int64;
begin
  V := MyInt64ModularInverse(P3.Z, Prime);
  P.X := Int64NonNegativeMulMod(P3.X, V, Prime);
  P.Y := Int64NonNegativeMulMod(P3.Y, V, Prime);
  Result := True;
end;

function CnInt64JacobianPointToEccPoint(var P3: TCnInt64Ecc3Point;
  var P: TCnInt64EccPoint; Prime: Int64): Boolean;
var
  T, V: Int64;
begin
  T := Int64NonNegativeMulMod(P3.Z, P3.Z, Prime); // Z^2
  V := MyInt64ModularInverse(T, Prime);       // 1 / Z^2
  P.X := Int64NonNegativeMulMod(P3.X, V, Prime);

  T := Int64NonNegativeMulMod(P3.Z, T, Prime); // Z^3
  V := MyInt64ModularInverse(T, Prime);       // 1 / Z^3
  P.Y := Int64NonNegativeMulMod(P3.Y, V, Prime);
  Result := True;
end;

function CnEccPointToEcc3Point(P: TCnEccPoint; P3: TCnEcc3Point): Boolean;
begin
  BigNumberCopy(P3.X, P.X);
  BigNumberCopy(P3.Y, P.Y);

  if P3.X.IsZero and P3.Y.IsZero then
    P3.Z.SetZero
  else
    P3.Z.SetOne;
  Result := True;
end;

function CnAffinePointToEccPoint(P3: TCnEcc3Point; P: TCnEccPoint; Prime: TCnBigNumber): Boolean;
var
  V: TCnBigNumber;
begin
  // X := X/Z   Y := Y/Z

  V := FEccBigNumberPool.Obtain;
  try
    BigNumberModularInverse(V, P3.Z, Prime);
    BigNumberDirectMulMod(P.X, P3.X, V, Prime);
    BigNumberDirectMulMod(P.Y, P3.Y, V, Prime);

    Result := True;
  finally
    FEccBigNumberPool.Recycle(V);
  end;
end;

function CnJacobianPointToEccPoint(P3: TCnEcc3Point; P: TCnEccPoint; Prime: TCnBigNumber): Boolean;
var
  T, V: TCnBigNumber;
begin
  // X := X/Z^2   Y := Y/Z^3
  T := nil;
  V := nil;

  try
    T := FEccBigNumberPool.Obtain;
    V := FEccBigNumberPool.Obtain;

    BigNumberDirectMulMod(T, P3.Z, P3.Z, Prime);
    BigNumberModularInverse(V, T, Prime);
    BigNumberDirectMulMod(P.X, P3.X, V, Prime);

    BigNumberDirectMulMod(T, T, P3.Z, Prime);
    BigNumberModularInverse(V, T, Prime);
    BigNumberDirectMulMod(P.Y, P3.Y, V, Prime);

    Result := True;
  finally
    FEccBigNumberPool.Recycle(V);
    FEccBigNumberPool.Recycle(T);
  end;
end;

function CnEccPointToStream(P: TCnEccPoint; Stream: TStream; FixedLen: Integer): Integer;
begin
  Result := BigNumberWriteBinaryToStream(P.X, Stream, FixedLen)
    + BigNumberWriteBinaryToStream(P.Y, Stream, FixedLen);
end;

function CnEccVerifyKeys(Ecc: TCnEcc; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
var
  P: TCnEccPoint;
begin
  Result := False;
  if (Ecc = nil) or (PrivateKey = nil) or (PublicKey = nil) then
    Exit;

  P := TCnEccPoint.Create;
  try
    P.Assign(Ecc.Generator);
    Ecc.MultiplePoint(PrivateKey, P);
    Result := CnEccPointsEqual(P, PublicKey);
  finally
    P.Free;
  end;
end;

function CnEccVerifyKeys(CurveType: TCnEccCurveType; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey): Boolean;
var
  Ecc: TCnEcc;
begin
  if CurveType = ctCustomized then
    raise ECnEccException.Create(SCnErrorEccCurveType);

  Ecc := TCnEcc.Create(CurveType);
  try
    Result := CnEccVerifyKeys(Ecc, PrivateKey, PublicKey);
  finally
    Ecc.Free;
  end;
end;

function GetCurveTypeFromOID(Data: PAnsiChar; DataByteLen: Cardinal): TCnEccCurveType;
var
  P: PByte;
  L: Byte;
begin
  Result := ctCustomized;
  if (Data = nil) or (DataByteLen < 3) then
    Exit;

  P := PByte(Data);
  if P^ <> CN_BER_TAG_OBJECT_IDENTIFIER then
    Exit;
  Inc(P);

  L := P^;
  if L > EC_CURVE_TYPE_OID_MAX_LENGTH then
    Exit;

  Inc(P);
  if CompareMem(P, @OID_ECPARAM_CURVE_TYPE_SECP256K1[0],
    Min(L, SizeOf(OID_ECPARAM_CURVE_TYPE_SECP256K1))) then
    Result := ctSecp256k1
  else if CompareMem(P, @OID_ECPARAM_CURVE_TYPE_SM2[0],
    Min(L, SizeOf(OID_ECPARAM_CURVE_TYPE_SM2))) then
    Result := ctSM2
  else if CompareMem(P, @OID_ECPARAM_CURVE_TYPE_PRIME256V1[0],
    Min(L, SizeOf(OID_ECPARAM_CURVE_TYPE_PRIME256V1))) then
    Result := ctPrime256v1
end;

// 根据曲线类型返回其 OID 地址与长度，外界使用后无需释放
function GetOIDFromCurveType(Curve: TCnEccCurveType; out OIDAddr: Pointer): Integer;
begin
  Result := 0;
  OIDAddr := nil;

  case Curve of
    ctSecp256k1:
      begin
        OIDAddr := @OID_ECPARAM_CURVE_TYPE_SECP256K1[0];
        Result := SizeOf(OID_ECPARAM_CURVE_TYPE_SECP256K1);
      end;
    ctSM2:
      begin
        OIDAddr := @OID_ECPARAM_CURVE_TYPE_SM2[0];
        Result := SizeOf(OID_ECPARAM_CURVE_TYPE_SM2);
      end;
    ctPrime256v1:
      begin
        OIDAddr := @OID_ECPARAM_CURVE_TYPE_PRIME256V1[0];
        Result := SizeOf(OID_ECPARAM_CURVE_TYPE_PRIME256V1);
      end;
  end;
end;

function ReadEccPublicKeyFromBitStringNode(BitStringNode: TCnBerReadNode; PublicKey: TCnEccPublicKey): Boolean;
var
  B: PByte;
  Len: Integer;
begin
  Result := False;
  if (BitStringNode = nil) or (PublicKey = nil) then
    Exit;

  // PubNode 的 Data 是 BITSTRING，00 04 开头
  // BITSTRING 数据区第一个内容字节是该 BITSTRING 凑成 8 的倍数所缺少的 Bit 数，这里是 0，跳过
  B := BitStringNode.BerDataAddress;
  Inc(B); // 跳过 00，指向压缩模式字节

  if B^ = EC_PUBLICKEY_UNCOMPRESSED then
  begin
    // 未压缩格式，前一半是公钥的 X，后一半是公钥的 Y
    Inc(B);
    Len := (BitStringNode.BerDataLength - 2) div 2;
    PublicKey.X.SetBinary(PAnsiChar(B), Len);
    Inc(B, Len);
    PublicKey.Y.SetBinary(PAnsiChar(B), Len);

    Result := True;
  end
  else if (B^ = EC_PUBLICKEY_COMPRESSED_ODD) or (B^ = EC_PUBLICKEY_COMPRESSED_EVEN) then
  begin
    Inc(B);
    // 压缩格式，全是公钥 X
    PublicKey.X.SetBinary(PAnsiChar(B), BitStringNode.BerDataLength - 2);
    PublicKey.Y.SetZero; // Y 先 0，外部再去求解

    Result := True;
  end;
end;

function WriteEccPublicKeyToBitStringNode(Writer: TCnBerWriter;
  ParentNode: TCnBerWriteNode; PublicKey: TCnEccPublicKey): Boolean;
var
  Cnt: Integer;
  B: Byte;
  OP, P: PByte;
begin
  Result := False;
  if (ParentNode = nil) or (PublicKey = nil) then
    Exit;

  Cnt := PublicKey.X.GetBytesCount;
  if not PublicKey.Y.IsZero then
  begin
    Cnt := Cnt + PublicKey.Y.GetBytesCount;
    B := EC_PUBLICKEY_UNCOMPRESSED;
  end
  else if PublicKey.Y.IsOdd then
    B := EC_PUBLICKEY_COMPRESSED_ODD
  else
    B := EC_PUBLICKEY_COMPRESSED_EVEN;

  OP := GetMemory(Cnt + 1);
  P := OP;
  P^ := B;

  Inc(P);
  PublicKey.X.ToBinary(PAnsiChar(P));
  if B = EC_PUBLICKEY_UNCOMPRESSED then
  begin
    Inc(P, PublicKey.X.GetBytesCount);
    PublicKey.Y.ToBinary(PAnsiChar(P));
  end;
  Writer.AddBasicNode(CN_BER_TAG_BIT_STRING, OP, Cnt + 1, ParentNode);
  FreeMemory(OP);
end;

(*
  SEQUENCE (2 elem)
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.10045.2.1 ecPublicKey (ANSI X9.62 public key type)
      OBJECT IDENTIFIER 1.3.132.0.10 secp256k1 (SECG (Certicom) named elliptic curve)
    BIT STRING
*)
function CnEccLoadPublicKeyFromPem(const PemFileName: string;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnEccLoadPublicKeyFromPem(Stream, PublicKey, CurveType, KeyHashMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnEccLoadPublicKeyFromPem(PemStream: TStream;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  MemStream: TMemoryStream;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
begin
  Result := False;
  MemStream := nil;
  Reader := nil;

  if PublicKey = nil then
    Exit;

  try
    MemStream := TMemoryStream.Create;
    if LoadPemStreamToMemory(PemStream, PEM_EC_PUBLIC_HEAD, PEM_EC_PUBLIC_TAIL,
      MemStream, Password, KeyHashMethod) then
    begin
      Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size);
      Reader.ParseToTree;
      if Reader.TotalCount >= 5 then
      begin
        // 2 要判断是否公钥
        Node := Reader.Items[2];
        if (Node.BerDataLength <> SizeOf(CN_OID_EC_PUBLIC_KEY)) or not CompareMem(@CN_OID_EC_PUBLIC_KEY[0],
          Node.BerDataAddress, Node.BerDataLength) then
          Exit;

        // 3 是曲线类型
        Node := Reader.Items[3];
        CurveType := GetCurveTypeFromOID(Node.BerAddress, Node.BerLength);

        // 读 4 里的公钥
        Result := ReadEccPublicKeyFromBitStringNode(Reader.Items[4], PublicKey);
      end;
    end;
  finally
    MemStream.Free;
    Reader.Free;
  end;
end;

(*
  PKCS#1: RFC5915

  ECPrivateKey ::= SEQUENCE {
    version        INTEGER { ecPrivkeyVer1(1) } (ecPrivkeyVer1),
    privateKey     OCTET STRING,
    parameters [0] ECParameters {{ NamedCurve }} OPTIONAL,
    publicKey  [1] BIT STRING OPTIONAL
  }

  SEQUENCE (4 elem)
    INTEGER 1
    OCTET STRING (32 byte) 私钥
    [0] (1 elem)
      OBJECT IDENTIFIER 1.3.132.0.10 secp256k1 (SECG (Certicom) named elliptic curve)
    [1] (1 elem)
      BIT STRING  公钥

  PKCS#8: 复杂一点

  SEQUENCE (3 elem)
    INTEGER 0  Version
    SEQUENCE (2 elem)
      OBJECT IDENTIFIER 1.2.840.10045.2.1 ecPublicKey (ANSI X9.62 public key type)
      OBJECT IDENTIFIER 1.3.132.0.10 secp256k1 (SECG (Certicom) named elliptic curve)
    OCTET STRING (109 byte) …
      SEQUENCE (3 elem)
        INTEGER 1
        OCTET STRING (32 byte) 私钥
        [0] (1 elem)               // 注意：本行与下面一行 OI 可选，因而代码中要兼容处理，同时上面一行可能有子内容
          OBJECT IDENTIFIER 1.2.156.10197.1.301 sm2ECC (China GM Standards Committee)
        [1] (1 elem)
          BIT STRING (520 bit) 公钥

*)
function CnEccLoadKeysFromPem(const PemFileName: string; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := CnEccLoadKeysFromPem(Stream, PrivateKey, PublicKey, CurveType,
      KeyHashMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnEccLoadKeysFromPem(PemStream: TStream; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; out CurveType: TCnEccCurveType;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  MemStream: TMemoryStream;
  Reader: TCnBerReader;
  Node: TCnBerReadNode;
  CurveType2: TCnEccCurveType;
  OldPos: Int64;
  IsPkcs1: Boolean;
begin
  Result := False;
  MemStream := nil;
  Reader := nil;

  try
    MemStream := TMemoryStream.Create;
    OldPos := PemStream.Position;
    IsPkcs1 := False;
    if LoadPemStreamToMemory(PemStream, PEM_EC_PARAM_HEAD, PEM_EC_PARAM_TAIL,
      MemStream, Password, KeyHashMethod) then
    begin
      // 读 ECPARAM 也即椭圆曲线类型
      CurveType := GetCurveTypeFromOID(PAnsiChar(MemStream.Memory), MemStream.Size);
      IsPkcs1 := True;
    end;

    PemStream.Position := OldPos;
    if IsPkcs1 then
    begin
      if LoadPemStreamToMemory(PemStream, PEM_EC_PRIVATE_HEAD, PEM_EC_PRIVATE_TAIL,
        MemStream, Password, KeyHashMethod) then
      begin
        Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size);
        Reader.ParseToTree;
        if Reader.TotalCount >= 7 then
        begin
          Node := Reader.Items[1]; // 0 是整个 Sequence，1 是 Version
          if Node.AsByte = 1 then  // 只支持版本 1
          begin
            // 2 是私钥
            if PrivateKey <> nil then
              PutIndexedBigIntegerToBigNumber(Reader.Items[2], PrivateKey);

            // 4 又是曲线类型
            Node := Reader.Items[4];
            CurveType2 := GetCurveTypeFromOID(Node.BerAddress, Node.BerLength);
            if (CurveType <> ctCustomized) and (CurveType2 <> CurveType) then
              Exit;

            CurveType := CurveType2; // 如果俩读出不一样，以第二个为准

            // 读 6 里的公钥
            if PublicKey <> nil then
              Result := ReadEccPublicKeyFromBitStringNode(Reader.Items[6], PublicKey);
          end;
        end;
      end;
    end
    else // 不是 PKCS#1，判断是否有 PKCS#8 的标记
    begin
      if LoadPemStreamToMemory(PemStream, PEM_PRIVATE_HEAD, PEM_PRIVATE_TAIL,
        MemStream, Password, KeyHashMethod) then
      begin
        Reader := TCnBerReader.Create(PByte(MemStream.Memory), MemStream.Size, True);
        Reader.ParseToTree;
        if Reader.TotalCount >= 11 then // 有 PKCS#8 标记且数量够
        begin
          Node := Reader.Items[1]; // 0 是整个 Sequence，1 是 Version
          if Node.AsByte = 0 then  // 只支持版本 0
          begin
            Node := Reader.Items[3]; // 3 是 ecPublicKey 的 Object Identifier
            if CompareObjectIdentifier(Node, @CN_OID_EC_PUBLIC_KEY[0], SizeOf(CN_OID_EC_PUBLIC_KEY)) then
            begin
              // 4 又是曲线类型
              Node := Reader.Items[4];
              CurveType := GetCurveTypeFromOID(Node.BerAddress, Node.BerLength);

              if PrivateKey <> nil then
                PutIndexedBigIntegerToBigNumber(Reader.Items[8], PrivateKey);

              if PublicKey <> nil then
              begin
                Result := ReadEccPublicKeyFromBitStringNode(Reader.Items[10], PublicKey);
                if not Result then // 兼容处理，可能上面多出一个 OI 分支且多解析出私钥的子节点
                  Result := ReadEccPublicKeyFromBitStringNode(Reader.Items[12], PublicKey);
                if not Result then
                  Result := ReadEccPublicKeyFromBitStringNode(Reader.Items[13], PublicKey);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    MemStream.Free;
    Reader.Free;
  end;
end;

function CnEccSaveKeysToPem(const PemFileName: string; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType; KeyType: TCnEccKeyType;
  KeyEncryptMethod: TCnKeyEncryptMethod; KeyHashMethod: TCnKeyHashMethod;
  const Password: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmCreate);
  try
    Result := CnEccSaveKeysToPem(Stream, PrivateKey, PublicKey, CurveType,
      KeyType, KeyEncryptMethod, KeyHashMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnEccSaveKeysToPem(PemStream: TStream; PrivateKey: TCnEccPrivateKey;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType; KeyType: TCnEccKeyType;
  KeyEncryptMethod: TCnKeyEncryptMethod;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  OIDPtr: Pointer;
  OIDLen: Integer;
  B: Byte;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) then
    Exit;

  OIDLen := GetOIDFromCurveType(CurveType, OIDPtr);
  if (OIDPtr = nil) or (OIDLen <= 0) then
    Exit;

  Mem := nil;
  Writer := nil;

  try
    if KeyType = cktPKCS1 then // PKCS1 格式，分两段
    begin
      Mem := TMemoryStream.Create;
      if (KeyEncryptMethod = ckeNone) or (Password = '') then
      begin
        // 不加密，分两段，第一段手工写
        B := CN_BER_TAG_OBJECT_IDENTIFIER;
        Mem.Write(B, 1);
        B := OIDLen;
        Mem.Write(B, 1);

        Mem.Write(OIDPtr^, OIDLen);
        if not SaveMemoryToPemStream(PemStream, PEM_EC_PARAM_HEAD, PEM_EC_PARAM_TAIL, Mem) then
          Exit;

        Mem.Clear;
      end;

      Writer := TCnBerWriter.Create;

      // 第二段组树
      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      B := 1;
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root); // 写 Version 1
      AddBigNumberToWriter(Writer, PrivateKey, Root, CN_BER_TAG_OCTET_STRING);   // 写私钥

      Node := Writer.AddContainerNode(CN_BER_TAG_RESERVED, Root);
      Node.BerTypeMask := ECC_PRIVATEKEY_TYPE_MASK;
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, PByte(OIDPtr), OIDLen, Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_BOOLEAN, Root); // 居然要用 BOOLEAN 才行
      Node.BerTypeMask := ECC_PRIVATEKEY_TYPE_MASK;

      WriteEccPublicKeyToBitStringNode(Writer, Node, PublicKey);
      Writer.SaveToStream(Mem);
      Result := SaveMemoryToPemStream(PemStream, PEM_EC_PRIVATE_HEAD, PEM_EC_PRIVATE_TAIL, Mem,
        KeyEncryptMethod, KeyHashMethod, Password, True);
    end
    else if KeyType = cktPKCS8 then
    begin
      Writer := TCnBerWriter.Create;

      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      B := 0;
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Root); // 写 Version 0

      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @CN_OID_EC_PUBLIC_KEY[0], SizeOf(CN_OID_EC_PUBLIC_KEY), Node);
      Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, PByte(OIDPtr), OIDLen, Node);

      Node := Writer.AddContainerNode(CN_BER_TAG_OCTET_STRING, Root);
      Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Node);
      B := 1;
      Writer.AddBasicNode(CN_BER_TAG_INTEGER, @B, 1, Node);

      AddBigNumberToWriter(Writer, PrivateKey, Node, CN_BER_TAG_OCTET_STRING);   // 写私钥

      Node := Writer.AddContainerNode(CN_BER_TAG_BOOLEAN, Node);
      Node.BerTypeMask := ECC_PRIVATEKEY_TYPE_MASK;
      WriteEccPublicKeyToBitStringNode(Writer, Node, PublicKey);                 // 写公钥

      Mem := TMemoryStream.Create;
      Writer.SaveToStream(Mem);

      Result := SaveMemoryToPemStream(PemStream, PEM_PRIVATE_HEAD, PEM_PRIVATE_TAIL, Mem,
        KeyEncryptMethod, KeyHashMethod, Password, True);
    end;
  finally
    Writer.Free;
    Mem.Free;
  end;
end;

function CnEccSavePublicKeyToPem(const PemFileName: string;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType;
  KeyType: TCnEccKeyType; KeyEncryptMethod: TCnKeyEncryptMethod;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(PemFileName, fmCreate);
  try
    Result := CnEccSavePublicKeyToPem(Stream, PublicKey, CurveType, KeyType,
      KeyEncryptMethod, KeyHashMethod, Password);
  finally
    Stream.Free;
  end;
end;

function CnEccSavePublicKeyToPem(PemStream: TStream;
  PublicKey: TCnEccPublicKey; CurveType: TCnEccCurveType;
  KeyType: TCnEccKeyType; KeyEncryptMethod: TCnKeyEncryptMethod;
  KeyHashMethod: TCnKeyHashMethod; const Password: string): Boolean;
var
  Root, Node: TCnBerWriteNode;
  Writer: TCnBerWriter;
  Mem: TMemoryStream;
  OIDPtr: Pointer;
  OIDLen: Integer;
begin
  // TODO: PKCS8 待实现
  Result := False;
  if (PublicKey = nil) or (PublicKey.X.IsZero) then
    Exit;

  OIDLen := GetOIDFromCurveType(CurveType, OIDPtr);
  if (OIDPtr = nil) or (OIDLen <= 0) then
    Exit;

  Writer := nil;
  Mem := nil;

  try
    Writer := TCnBerWriter.Create;
    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    Node := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE, Root);

    // 给 Node 加 ECPublicKey 与 曲线类型的 ObjectIdentifier
    Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, @CN_OID_EC_PUBLIC_KEY[0],
      SizeOf(CN_OID_EC_PUBLIC_KEY), Node);
    Writer.AddBasicNode(CN_BER_TAG_OBJECT_IDENTIFIER, OIDPtr, OIDLen, Node);
    WriteEccPublicKeyToBitStringNode(Writer, Root, PublicKey);

    Mem := TMemoryStream.Create;
    Writer.SaveToStream(Mem);

    Result := SaveMemoryToPemStream(PemStream, PEM_EC_PUBLIC_HEAD, PEM_EC_PUBLIC_TAIL, Mem,
      KeyEncryptMethod, KeyHashMethod, Password);
  finally
    Mem.Free;
    Writer.Free;
  end;
end;

// ============================ ECC 签名与验证 =================================

// 根据指定数字摘要算法计算指定流的二进制杂凑值并写入 Stream
function CalcDigestStream(InStream: TStream; SignType: TCnEccSignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TCnMD5Digest;
  Sha1: TCnSHA1Digest;
  Sha256: TCnSHA256Digest;
  Sm3Dig: TCnSM3Digest;
begin
  Result := False;
  case SignType of
    esdtMD5:
      begin
        Md5 := MD5Stream(InStream);
        outStream.Write(Md5, SizeOf(TCnMD5Digest));
        Result := True;
      end;
    esdtSHA1:
      begin
        Sha1 := SHA1Stream(InStream);
        outStream.Write(Sha1, SizeOf(TCnSHA1Digest));
        Result := True;
      end;
    esdtSHA256:
      begin
        Sha256 := SHA256Stream(InStream);
        outStream.Write(Sha256, SizeOf(TCnSHA256Digest));
        Result := True;
      end;
    esdtSM3:
      begin
        Sm3Dig := SM3Stream(InStream);
        outStream.Write(Sm3Dig, SizeOf(TCnSM3Digest));
        Result := True;
      end;
  end;
end;

// 根据指定数字摘要算法计算文件的二进制杂凑值并写入 Stream
function CalcDigestFile(const FileName: string; SignType: TCnEccSignDigestType;
  outStream: TStream): Boolean;
var
  Md5: TCnMD5Digest;
  Sha1: TCnSHA1Digest;
  Sha256: TCnSHA256Digest;
  Sm3Dig: TCnSM3Digest;
begin
  Result := False;
  case SignType of
    esdtMD5:
      begin
        Md5 := MD5File(FileName);
        outStream.Write(Md5, SizeOf(TCnMD5Digest));
        Result := True;
      end;
    esdtSHA1:
      begin
        Sha1 := SHA1File(FileName);
        outStream.Write(Sha1, SizeOf(TCnSHA1Digest));
        Result := True;
      end;
    esdtSHA256:
      begin
        Sha256 := SHA256File(FileName);
        outStream.Write(Sha256, SizeOf(TCnSHA256Digest));
        Result := True;
      end;
    esdtSM3:
      begin
        Sm3Dig := SM3File(FileName);
        outStream.Write(Sm3Dig, SizeOf(TCnSM3Digest));
        Result := True;
      end;
  end;
end;

{
  按维基百科上说明的 ECDSA 算法进行签名：
  https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm

  r = 随机k * G点（的 x）
  s = (r * Private + 明文) / k

  均对椭圆曲线的阶求模，并非对有限域求模
}
function EccSignValue(Ecc: TCnEcc; PrivateKey: TCnEccPrivateKey; InE: TCnBigNumber;
  OutSignature: TCnEccSignature): Boolean;
var
  K, X, KInv: TCnBigNumber;
  P: TCnEccPoint;
begin
  Result := False;
  BuildShortXValue(InE, Ecc.Order); // InE 现在是 z

  K := nil;
  X := nil;
  KInv := nil;
  P := nil;

  try
    K := TCnBigNumber.Create;
    KInv := TCnBigNumber.Create;
    X := TCnBigNumber.Create;
    P := TCnEccPoint.Create;

    while True do
    begin
      if not BigNumberRandRange(K, Ecc.Order) then // 生成重要的随机 K
        Exit;

      P.Assign(Ecc.Generator);
      Ecc.MultiplePoint(K, P);

      if not BigNumberNonNegativeMod(OutSignature.R, P.X, Ecc.Order) then
        Exit;

      if OutSignature.R.IsZero then
        Continue;
      // 算出了签名的一部分 R

      if not BigNumberMul(X, PrivateKey, OutSignature.R) then   // X <= r * PrivateKey
        Exit;
      if not BigNumberAdd(X, X, InE) then             // X <= X + z
        Exit;
      if not BigNumberModularInverse(KInv, K, Ecc.Order) then
        Exit;
      if not BigNumberMul(X, KInv, X) then            // X <= K^-1 * X
        Exit;
      if not BigNumberNonNegativeMod(OutSignature.S, X, Ecc.Order) then  // OutS <= K^-1 * (z + r * PrivateKey) mod N
        Exit;

      if OutSignature.S.IsZero then
        Continue;

      Break;
    end;
    Result := True;
  finally
    P.Free;
    KInv.Free;
    X.Free;
    K.Free;
  end;
end;

{
  从数据块与签名等信息还原出 SM2 公钥，返回是否还原成功。注意结果有两个，需要外部判断

  因为 r = 随机k * G点（的 x），且 s = (r * Private + 明文z) / k

  两边同时乘以 k 得 k*s*G = (r*Private + 明文)*G

  组合 s*(k*G) = r*Private*G+ 明文*G

  s*(kG) = r*Public + 明文*G

  Public = r^-1 * (s*(kG) - 明文*G)，其中 k*G 的 x 坐标是 r，可求出两个 y 来
}
function CnEccRecoverPublicKey(Ecc: TCnEcc; InE: TCnBigNumber; InSignature: TCnEccSignature;
  OutPublicKey1, OutPublicKey2: TCnEccPublicKey): Boolean;
var
  P, Q, T: TCnEccPoint;
  RInv: TCnBigNumber;
begin
  Result := False;

  RInv := nil;
  P := nil;
  Q := nil;
  T := nil;

  try
    RInv := TCnBigNumber.Create;
    if not BigNumberModularInverse(RInv, InSignature.R, Ecc.Order) then
      Exit;

    P := TCnEccPoint.Create;
    if not Ecc.PlainToPoint(InSignature.R, P) then  // P.Y 是一个 y ，所以此处 P 是 k*G 的一个取值
      Exit;

    Q := TCnEccPoint.Create;
    Q.Assign(Ecc.Generator);
    Ecc.MultiplePoint(InE, Q); // 得到明文*G
    Ecc.PointInverse(Q);       // Q 得到 -明文 * G

    T := TCnEccPoint.Create;
    T.Assign(P);
    Ecc.MultiplePoint(InSignature.S, T);    // T 得到 s*(k*G)

    Ecc.PointAddPoint(Q, T, OutPublicKey1);
    Ecc.MultiplePoint(RInv, OutPublicKey1); // PublicKey1 得到 r^-1 * (s*(kG) - 明文*G)

    Ecc.PointInverse(P);
    T.Assign(P);
    Ecc.MultiplePoint(InSignature.S, T);    // T 再次得到 s* 另一个(k*G)

    Ecc.PointAddPoint(Q, T, OutPublicKey2);
    Ecc.MultiplePoint(RInv, OutPublicKey2); // PublicKey2 得到 r^-1 * (s*(kG) - 明文*G)

    Result := True;
  finally
    T.Free;
    Q.Free;
    P.Free;
    RInv.Free;
  end;
end;

function CnEccSignFile(const InFileName, OutSignFileName: string; Ecc: TCnEcc;
  PrivateKey: TCnEccPrivateKey; SignType: TCnEccSignDigestType = esdtMD5): Boolean;
var
  Stream: TMemoryStream;
  E: TCnBigNumber;
  Sig: TCnEccSignature;
  Writer: TCnBerWriter;
  Root: TCnBerWriteNode;
begin
  Result := False;
  Stream := nil;
  Writer := nil;
  E := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的杂凑值
      Exit;

    E := TCnBigNumber.Create;
    E.SetBinary(Stream.Memory, Stream.Size);

    Sig := TCnEccSignature.Create;
    if EccSignValue(Ecc, PrivateKey, E, Sig) then
    begin
      // 然后按格式进行 BER 编码
      Writer := TCnBerWriter.Create;
      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      AddBigNumberToWriter(Writer, Sig.R, Root);
      AddBigNumberToWriter(Writer, Sig.S, Root);

      Writer.SaveToFile(OutSignFileName);
      Result := True;
    end;
  finally
    Stream.Free;
    E.Free;
    Sig.Free;
    Writer.Free;
  end;
end;

function CnEccSignFile(const InFileName, OutSignFileName: string; CurveType: TCnEccCurveType;
  PrivateKey: TCnEccPrivateKey; SignType: TCnEccSignDigestType = esdtMD5): Boolean;
var
  Ecc: TCnEcc;
begin
  if CurveType = ctCustomized then
    raise ECnEccException.Create(SCnErrorEccCurveType);

  Ecc := TCnEcc.Create(CurveType);
  try
    Result := CnEccSignFile(InFileName, OutSignFileName, Ecc, PrivateKey, SignType);
  finally
    Ecc.Free;
  end;
end;

{
  按维基百科上说明的 ECDSA 算法进行签名验证：
  https://en.wikipedia.org/wiki/Elliptic_Curve_Digital_Signature_Algorithm
}
function EccVerifyValue(Ecc: TCnEcc; PublicKey: TCnEccPublicKey; InE: TCnBigNumber;
  InSignature: TCnEccSignature): Boolean;
var
  U1, U2, SInv: TCnBigNumber;
  P1, P2: TCnEccPoint;
begin
  Result := False;
  if not CheckEccPublicKey(Ecc, PublicKey) then
    Exit;

  BuildShortXValue(InE, Ecc.Order); // InE is z

  U1 := nil;
  U2 := nil;
  P1 := nil;
  P2 := nil;
  SInv := nil;

  try
    SInv := TCnBigNumber.Create;
    BigNumberModularInverse(SInv, InSignature.S, Ecc.Order);
    U1 := TCnBigNumber.Create;
    if not BigNumberMul(U1, InE, SInv) then
      Exit;
    if not BigNumberNonNegativeMod(U1, U1, Ecc.Order) then // u1 = (z * s^-1) mod N
      Exit;

    U2 := TCnBigNumber.Create;
    if not BigNumberMul(U2, InSignature.R, SInv) then
      Exit;
    if not BigNumberNonNegativeMod(U1, U1, Ecc.Order) then // u2 = (r * s^-1) mod N
      Exit;

    P1 := TCnEccPoint.Create;
    P1.Assign(Ecc.Generator);
    Ecc.MultiplePoint(U1, P1);

    P2 := TCnEccPoint.Create;
    P2.Assign(PublicKey);
    Ecc.MultiplePoint(U2, P2);
    Ecc.PointAddPoint(P1, P2, P1);  // 计算 u1 * G + u2 * PublicKey 点
    if P1.IsZero then
      Exit;

    if not BigNumberNonNegativeMod(P1.X, P1.X, Ecc.Order) then // 计算 P1.X mod N
      Exit;

    if not BigNumberNonNegativeMod(P1.Y, InSignature.R, Ecc.Order) then  // 计算 r mod N
      Exit;

    Result := BigNumberCompare(P1.X, P1.Y) = 0;
  finally
    SInv.Free;
    P2.Free;
    P1.Free;
    U2.Free;
    U1.Free;
  end;
end;

function CnEccVerifyFile(const InFileName, InSignFileName: string; Ecc: TCnEcc;
  PublicKey: TCnEccPublicKey; SignType: TCnEccSignDigestType): Boolean;
var
  Stream: TMemoryStream;
  E: TCnBigNumber;
  Sig: TCnEccSignature;
  Reader: TCnBerReader;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  E := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;

    if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的杂凑值
      Exit;

    E := TCnBigNumber.Create;
    E.SetBinary(Stream.Memory, Stream.Size);

    Stream.Clear;
    Stream.LoadFromFile(InSignFileName);
    Reader := TCnBerReader.Create(Stream.Memory, Stream.Size);
    Reader.ParseToTree;

    if Reader.TotalCount <> 3 then
      Exit;

    Sig := TCnEccSignature.Create;
    PutIndexedBigIntegerToBigNumber(Reader.Items[1], Sig.R);
    PutIndexedBigIntegerToBigNumber(Reader.Items[2], Sig.S);

    Result := EccVerifyValue(Ecc, PublicKey, E, Sig);
  finally
    Stream.Free;
    Reader.Free;
    E.Free;
    Sig.Free
  end;
end;

function CnEccVerifyFile(const InFileName, InSignFileName: string; CurveType: TCnEccCurveType;
  PublicKey: TCnEccPublicKey; SignType: TCnEccSignDigestType): Boolean;
var
  Ecc: TCnEcc;
begin
  if CurveType = ctCustomized then
    raise ECnEccException.Create(SCnErrorEccCurveType);

  Ecc := TCnEcc.Create(CurveType);
  try
    Result := CnEccVerifyFile(InFileName, InSignFileName, Ecc, PublicKey, SignType);
  finally
    Ecc.Free;
  end;
end;

function CnEccRecoverPublicKeyFromFile(const InFileName, InSignFileName: string;
  Ecc: TCnEcc; OutPublicKey1, OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType): Boolean; overload;
var
  Stream: TMemoryStream;
  E: TCnBigNumber;
  Sig: TCnEccSignature;
  Reader: TCnBerReader;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  E := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;

    if not CalcDigestFile(InFileName, SignType, Stream) then // 计算文件的杂凑值
      Exit;

    E := TCnBigNumber.Create;
    E.SetBinary(Stream.Memory, Stream.Size);

    Stream.Clear;
    Stream.LoadFromFile(InSignFileName);
    Reader := TCnBerReader.Create(Stream.Memory, Stream.Size);
    Reader.ParseToTree;

    if Reader.TotalCount <> 3 then
      Exit;

    Sig := TCnEccSignature.Create;
    PutIndexedBigIntegerToBigNumber(Reader.Items[1], Sig.R);
    PutIndexedBigIntegerToBigNumber(Reader.Items[2], Sig.S);

    Result := CnEccRecoverPublicKey(Ecc, E, Sig, OutPublicKey1, OutPublicKey2);
  finally
    Stream.Free;
    Reader.Free;
    E.Free;
    Sig.Free
  end;
end;

function CnEccRecoverPublicKeyFromFile(const InFileName, InSignFileName: string;
  CurveType: TCnEccCurveType; OutPublicKey1, OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType): Boolean; overload;
var
  Ecc: TCnEcc;
begin
  if CurveType = ctCustomized then
    raise ECnEccException.Create(SCnErrorEccCurveType);

  Ecc := TCnEcc.Create(CurveType);
  try
    Result := CnEccRecoverPublicKeyFromFile(InFileName, InSignFileName, Ecc,
      OutPublicKey1, OutPublicKey2, SignType);
  finally
    Ecc.Free;
  end;
end;

{
  ECC 签名输出的 BER 格式如下，直接存成二进制文件即可
  SEQUENCE (2 elem)
    INTEGER r
    INTEGER s
}
function CnEccSignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  Ecc: TCnEcc; PrivateKey: TCnEccPrivateKey;
  SignType: TCnEccSignDigestType): Boolean;
var
  Stream: TMemoryStream;
  E: TCnBigNumber;
  Sig: TCnEccSignature;
  Writer: TCnBerWriter;
  Root: TCnBerWriteNode;
begin
  Result := False;
  Stream := nil;
  Writer := nil;
  E := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    if not CalcDigestStream(InStream, SignType, Stream) then // 计算流的杂凑值
      Exit;

    E := TCnBigNumber.Create;
    E.SetBinary(Stream.Memory, Stream.Size);

    Sig := TCnEccSignature.Create;
    if EccSignValue(Ecc, PrivateKey, E, Sig) then
    begin
      // 然后按格式进行 BER 编码
      Writer := TCnBerWriter.Create;
      Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
      AddBigNumberToWriter(Writer, Sig.R, Root);
      AddBigNumberToWriter(Writer, Sig.S, Root);

      Writer.SaveToStream(OutSignStream);
      Result := True;
    end;
  finally
    Stream.Free;
    E.Free;
    Sig.Free;
    Writer.Free;
  end;
end;

function CnEccSignStream(InStream: TMemoryStream; OutSignStream: TMemoryStream;
  CurveType: TCnEccCurveType; PrivateKey: TCnEccPrivateKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean;
var
  Ecc: TCnEcc;
begin
  if CurveType = ctCustomized then
    raise ECnEccException.Create(SCnErrorEccCurveType);

  Ecc := TCnEcc.Create(CurveType);
  try
    Result := CnEccSignStream(InStream, OutSignStream, Ecc, PrivateKey, SignType);
  finally
    Ecc.Free;
  end;
end;

function CnEccVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  Ecc: TCnEcc; PublicKey: TCnEccPublicKey;
  SignType: TCnEccSignDigestType): Boolean;
var
  Stream: TMemoryStream;
  E: TCnBigNumber;
  Sig: TCnEccSignature;
  Reader: TCnBerReader;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  E := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    if not CalcDigestStream(InStream, SignType, Stream) then // 计算流的杂凑值
      Exit;

    E := TCnBigNumber.Create;
    E.SetBinary(Stream.Memory, Stream.Size);

    Stream.Clear;
    Stream.LoadFromStream(InSignStream);
    Reader := TCnBerReader.Create(Stream.Memory, Stream.Size);
    Reader.ParseToTree;

    if Reader.TotalCount <> 3 then
      Exit;

    Sig := TCnEccSignature.Create;
    PutIndexedBigIntegerToBigNumber(Reader.Items[1], Sig.R);
    PutIndexedBigIntegerToBigNumber(Reader.Items[2], Sig.S);

    Result := EccVerifyValue(Ecc, PublicKey, E, Sig);
  finally
    Stream.Free;
    Reader.Free;
    E.Free;
    Sig.Free;
  end;
end;

function CnEccVerifyStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  CurveType: TCnEccCurveType; PublicKey: TCnEccPublicKey;
  SignType: TCnEccSignDigestType = esdtMD5): Boolean;
var
  Ecc: TCnEcc;
begin
  if CurveType = ctCustomized then
    raise ECnEccException.Create(SCnErrorEccCurveType);

  Ecc := TCnEcc.Create(CurveType);
  try
    Result := CnEccVerifyStream(InStream, InSignStream, Ecc, PublicKey, SignType);
  finally
    Ecc.Free;
  end;
end;

function CnEccRecoverPublicKeyFromStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  Ecc: TCnEcc; OutPublicKey1, OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType): Boolean; overload;
var
  Stream: TMemoryStream;
  E: TCnBigNumber;
  Sig: TCnEccSignature;
  Reader: TCnBerReader;
begin
  Result := False;
  Stream := nil;
  Reader := nil;
  E := nil;
  Sig := nil;

  try
    Stream := TMemoryStream.Create;
    if not CalcDigestStream(InStream, SignType, Stream) then // 计算流的杂凑值
      Exit;

    E := TCnBigNumber.Create;
    E.SetBinary(Stream.Memory, Stream.Size);

    Stream.Clear;
    Stream.LoadFromStream(InSignStream);
    Reader := TCnBerReader.Create(Stream.Memory, Stream.Size);
    Reader.ParseToTree;

    if Reader.TotalCount <> 3 then
      Exit;

    Sig := TCnEccSignature.Create;
    PutIndexedBigIntegerToBigNumber(Reader.Items[1], Sig.R);
    PutIndexedBigIntegerToBigNumber(Reader.Items[2], Sig.S);

    Result := CnEccRecoverPublicKey(Ecc, E, Sig, OutPublicKey1, OutPublicKey2);
  finally
    Stream.Free;
    Reader.Free;
    E.Free;
    Sig.Free;
  end;
end;

function CnEccRecoverPublicKeyFromStream(InStream: TMemoryStream; InSignStream: TMemoryStream;
  CurveType: TCnEccCurveType; OutPublicKey1, OutPublicKey2: TCnEccPublicKey;
  SignType: TCnEccSignDigestType): Boolean; overload;
var
  Ecc: TCnEcc;
begin
  if CurveType = ctCustomized then
    raise ECnEccException.Create(SCnErrorEccCurveType);

  Ecc := TCnEcc.Create(CurveType);
  try
    Result := CnEccRecoverPublicKeyFromStream(InStream, InSignStream, Ecc,
      OutPublicKey1, OutPublicKey2, SignType);
  finally
    Ecc.Free;
  end;
end;

function CheckEccPublicKey(Ecc: TCnEcc; PublicKey: TCnEccPublicKey): Boolean;
var
  P: TCnEccPoint;
begin
  Result := False;
  if (Ecc <> nil) and (PublicKey <> nil) then
  begin
    if PublicKey.IsZero then
      Exit;
    if not Ecc.IsPointOnCurve(PublicKey) then
      Exit;

    P := TCnEccPoint.Create;
    try
      P.Assign(PublicKey);
      Ecc.MultiplePoint(Ecc.Order, P);
      Result := P.IsZero;
    finally
      P.Free;
    end;
  end;
end;

function GetEccDigestNameFromSignDigestType(Digest: TCnEccSignDigestType): string;
begin
  case Digest of
    esdtMD5: Result := 'MD5';
    esdtSHA1: Result := 'SHA1';
    esdtSHA256: Result := 'SHA256';
    esdtSM3: Result := 'SM3';
  else
    Result := '<Unknown>';
  end;
end;

{ TCnInt64PolynomialEccPoint }

procedure TCnInt64PolynomialEccPoint.Assign(Source: TPersistent);
begin
  if Source is TCnInt64PolynomialEccPoint then
  begin
    Int64PolynomialCopy(FX, (Source as TCnInt64PolynomialEccPoint).X);
    Int64PolynomialCopy(FY, (Source as TCnInt64PolynomialEccPoint).Y);
  end
  else
    inherited;
end;

constructor TCnInt64PolynomialEccPoint.Create;
begin
  inherited;
  FX := TCnInt64Polynomial.Create;
  FY := TCnInt64Polynomial.Create;
end;

constructor TCnInt64PolynomialEccPoint.Create(const XLowToHighCoefficients,
  YLowToHighCoefficients: array of const);
begin
  Create;
  FX.SetCoefficents(XLowToHighCoefficients);
  FY.SetCoefficents(YLowToHighCoefficients);
end;

destructor TCnInt64PolynomialEccPoint.Destroy;
begin
  FY.Free;
  FX.Free;
  inherited;
end;

function TCnInt64PolynomialEccPoint.IsZero: Boolean;
begin
  Result := FX.IsZero and FY.IsZero;
end;

procedure TCnInt64PolynomialEccPoint.SetX(
  const Value: TCnInt64Polynomial);
begin
  if Value <> nil then
    Int64PolynomialCopy(FX, Value);
end;

procedure TCnInt64PolynomialEccPoint.SetY(
  const Value: TCnInt64Polynomial);
begin
  if Value <> nil then
    Int64PolynomialCopy(FY, Value);
end;

procedure TCnInt64PolynomialEccPoint.SetZero;
begin
  FX.SetZero;
  FY.SetZero;
end;

function TCnInt64PolynomialEccPoint.ToString: string;
begin
  Result := CnInt64PolynomialEccPointToString(Self);
end;

function CnInt64PolynomialEccPointToString(P: TCnInt64PolynomialEccPoint): string;
begin
  Result := Format('%s; %s', [P.X.ToString, P.Y.ToString]);
end;

function CnInt64PolynomialEccPointsEqual(P1, P2: TCnInt64PolynomialEccPoint): Boolean;
begin
  Result := Int64PolynomialEqual(P1.X, P2.X) and Int64PolynomialEqual(P1.Y, P2.Y);
end;

{ TCnInt64PolynomialEcc }

constructor TCnInt64PolynomialEcc.Create(A, B, FieldPrime: Int64; Ext: Integer;
  GX, GY: array of const; Order: Int64; PrimitivePolynomial: array of const);
begin
  inherited Create;

  // 由外界保证 Prime 与 Order 为素数
  // if not CnInt64IsPrime(FieldPrime) then // or not CnInt64IsPrime(Order) then
  //  raise ECnEccException.Create('Infinite Field must be a Prime Number.');

  // 扩域次数得大于 1
  if Ext <= 1 then
    raise ECnEccException.Create('Field Extension must > 1.');

  // 要确保 4*a^3+27*b^2 <> 0
  if 4 * A * A * A + 27 * B * B = 0 then
    raise ECnEccException.Create('Error: 4 * A^3 + 27 * B^2 = 0');

  FCoefficientA := A;
  FCoefficientB := B;
  FFiniteFieldSize := FieldPrime;
  FExtension := Ext;

  FGenerator := TCnInt64PolynomialEccPoint.Create;
  FGenerator.X.SetCoefficents(GX);
  FGenerator.Y.SetCoefficents(GY);

  FOrder := Order;

  FPrimitive := TCnInt64Polynomial.Create;
  FPrimitive.SetCoefficents(PrimitivePolynomial);
end;

destructor TCnInt64PolynomialEcc.Destroy;
begin
  FPrimitive.Free;
  FGenerator.Free;
  inherited;
end;

function TCnInt64PolynomialEcc.DivisionPolynomial(Degree: Integer;
  outDivisionPolynomial: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialGaloisCalcDivisionPolynomial(FCoefficientA, FCoefficientB,
    Degree, outDivisionPolynomial, FFiniteFieldSize);
end;

function TCnInt64PolynomialEcc.IsPointOnCurve(P: TCnInt64PolynomialEccPoint): Boolean;
var
  X, Y: TCnInt64Polynomial;
begin
  // 计算 (Y^2 - X^3 - A*X - B) mod primitive （多项式系数运算要 mod p）是否等于 0 多项式
  Result := False;
  if P = nil then
    Exit;

  X := nil;
  Y := nil;

  try
    X := FEccInt64PolynomialPool.Obtain;
    Y := FEccInt64PolynomialPool.Obtain;

    Int64PolynomialCopy(Y, P.Y);
    Int64PolynomialGaloisMul(Y, Y, Y, FFiniteFieldSize, FPrimitive);

    Int64PolynomialCopy(X, P.X);
    Int64PolynomialGaloisPower(X, X, 3, FFiniteFieldSize, FPrimitive);

    Int64PolynomialGaloisSub(Y, Y, X, FFiniteFieldSize, FPrimitive);  // Y := Y^2 - X^3 mod

    Int64PolynomialCopy(X, P.X);
    Int64PolynomialMulWord(X, FCoefficientA);
    Int64PolynomialAddWord(X, FCoefficientB);
    Int64PolynomialNonNegativeModWord(X, FFiniteFieldSize);  // X := A*X + B mod

    Int64PolynomialGaloisSub(Y, Y, X, FFiniteFieldSize, FPrimitive);
    Int64PolynomialGaloisMod(Y, Y, FPrimitive, FFiniteFieldSize);

    Result := Y.IsZero;
  finally
    FEccInt64PolynomialPool.Recycle(Y);
    FEccInt64PolynomialPool.Recycle(X);
  end;
end;

class function TCnInt64PolynomialEcc.IsPointOnCurve2(PX, PY: TCnInt64Polynomial;
  A, B, APrime: Int64; APrimitive: TCnInt64Polynomial): Boolean;
var
  X, Y: TCnInt64Polynomial;
begin
  // 计算 (Y^2 - X^3 - A*X - B) mod primitive （多项式系数运算要 mod p）是否等于 0 多项式
  X := nil;
  Y := nil;

  try
    X := FEccInt64PolynomialPool.Obtain;
    Y := FEccInt64PolynomialPool.Obtain;

    Int64PolynomialCopy(Y, PY);
    Int64PolynomialGaloisMul(Y, Y, Y, APrime, APrimitive);

    Int64PolynomialCopy(X, PX);
    Int64PolynomialGaloisPower(X, X, 3, APrime, APrimitive);

    Int64PolynomialGaloisSub(Y, Y, X, APrime, APrimitive);                // Y := Y^2 - X^3

    Int64PolynomialCopy(X, PX);
    Int64PolynomialMulWord(X, A);
    Int64PolynomialAddWord(X, B);   // X := A*X + B
    Int64PolynomialNonNegativeModWord(X, APrime);

    Int64PolynomialGaloisSub(Y, Y, X, APrime, APrimitive);
    Int64PolynomialGaloisMod(Y, Y, APrimitive, APrime);

    Result := Y.IsZero;
  finally
    FEccInt64PolynomialPool.Recycle(Y);
    FEccInt64PolynomialPool.Recycle(X);
  end;
end;

class function TCnInt64PolynomialEcc.IsRationalPointOnCurve(PX,
  PY: TCnInt64RationalPolynomial; A, B, APrime: Int64;
  APrimitive: TCnInt64Polynomial): Boolean;
var
  Y2, T1: TCnInt64Polynomial;
  RL, RR, T2: TCnInt64RationalPolynomial;
begin
  // 计算 PY^2 * (x^3 + Ax + B) 是否等于 PX^3 + A * PX + B，系数均 mod APrime
  Y2 := nil;
  T1 := nil;
  T2 := nil;
  RL := nil;
  RR := nil;

  try
    Y2 := FEccInt64PolynomialPool.Obtain;
    Y2.SetCoefficents([B, A, 0, 1]);

    RL := FEccInt64RationalPolynomialPool.Obtain;
    Int64RationalPolynomialGaloisMul(PY, PY, RL, APrime);
    Int64RationalPolynomialGaloisMul(RL, Y2, RL, APrime);  // 得到等号左边的值

    RR := FEccInt64RationalPolynomialPool.Obtain;
    Int64RationalPolynomialGaloisMul(PX, PX, RR, APrime);
    Int64RationalPolynomialGaloisMul(RR, PX, RR, APrime);  // 得到 PX^3

    T1 := FEccInt64PolynomialPool.Obtain;
    T1.SetCoefficents([A]);

    T2 := FEccInt64RationalPolynomialPool.Obtain;
    Int64RationalPolynomialGaloisMul(PX, T1, T2, APrime);  // T2 得到 A * PX

    T1.SetCoefficents([B]);
    Int64RationalPolynomialGaloisAdd(T2, T1, T2, APrime);  // T2 得到 A * PX + B

    Int64RationalPolynomialGaloisAdd(T2, RR, RR, APrime);  // RR 得到 PX^3 + A * PX + B

    if APrimitive <> nil then
    begin
      Int64PolynomialGaloisMod(RL.Nominator, RL.Nominator, APrimitive, APrime);
      Int64PolynomialGaloisMod(RL.Denominator, RL.Denominator, APrimitive, APrime);
      Int64PolynomialGaloisMod(RR.Nominator, RR.Nominator, APrimitive, APrime);
      Int64PolynomialGaloisMod(RR.Denominator, RR.Denominator, APrimitive, APrime);
    end;

    Result := Int64RationalPolynomialGaloisEqual(RL, RR, APrime, APrimitive);       // 比较是否相等
  finally
    FEccInt64PolynomialPool.Recycle(Y2);
    FEccInt64PolynomialPool.Recycle(T1);
    FEccInt64RationalPolynomialPool.Recycle(T2);
    FEccInt64RationalPolynomialPool.Recycle(RL);
    FEccInt64RationalPolynomialPool.Recycle(RR);
  end;
end;

procedure TCnInt64PolynomialEcc.MultiplePoint(K: Int64;
  Point: TCnInt64PolynomialEccPoint);
var
  E, R: TCnInt64PolynomialEccPoint;
begin
  if K = 0 then
  begin
    Point.SetZero;
    Exit;
  end
  else if K < 0 then
  begin
    K := -K;
    PointInverse(Point);
  end;

  R := nil;
  E := nil;

  try
    R := TCnInt64PolynomialEccPoint.Create;
    E := TCnInt64PolynomialEccPoint.Create;

    R.SetZero;
    E.Assign(Point);

    while K <> 0 do
    begin
      if (K and 1) <> 0 then
        PointAddPoint(R, E, R);

      PointAddPoint(E, E, E);
      K := K shr 1;
    end;

    Point.Assign(R);
  finally
    R.Free;
    E.Free;
  end;
end;

//class procedure TCnInt64PolynomialEcc.MultiplePoint1(K: Integer; PX,
//  PY: TCnInt64Polynomial; A, B, APrime: Int64;
//  APrimitive: TCnInt64Polynomial);
//var
//  EX, EY, RX, RY, SX, SY: TCnInt64Polynomial;
//begin
//  if K = 0 then
//  begin
//    PX.SetZero;
//    PY.SetZero;
//    Exit;
//  end
//  else if K < 0 then
//    raise ECnEccException.Create('Negative Multiple NOT Support');
//
//  EX := nil;
//  EY := nil;
//  RX := nil;
//  RY := nil;
//  SX := nil;
//  SY := nil;
//
//  try
//    EX := FEccInt64PolynomialPool.Obtain;
//    EY := FEccInt64PolynomialPool.Obtain;
//    RX := FEccInt64PolynomialPool.Obtain;
//    RY := FEccInt64PolynomialPool.Obtain;
//    SX := FEccInt64PolynomialPool.Obtain;
//    SY := FEccInt64PolynomialPool.Obtain;
//
//    RX.SetZero;
//    RY.SetZero;
//
//    Int64PolynomialCopy(EX, PX);
//    Int64PolynomialCopy(EY, PY);
//
//    while K <> 0 do
//    begin
//      if (K and 1) <> 0 then
//      begin
//        PointAddPoint1(RX, RY, EX, EY, SX, SY, A, B, APrime, APrimitive);
//        Int64PolynomialCopy(RX, SX);
//        Int64PolynomialCopy(RY, SY);
//      end;
//
//      PointAddPoint1(EX, EY, EX, EY, SX, SY, A, B, APrime, APrimitive);
//      Int64PolynomialCopy(EX, SX);
//      Int64PolynomialCopy(EY, SY);
//
//      K := K shr 1;
//    end;
//
//    Int64PolynomialCopy(PX, RX);
//    Int64PolynomialCopy(PY, RY);
//  finally
//    FEccInt64PolynomialPool.Recycle(EX);
//    FEccInt64PolynomialPool.Recycle(EY);
//    FEccInt64PolynomialPool.Recycle(RX);
//    FEccInt64PolynomialPool.Recycle(RY);
//    FEccInt64PolynomialPool.Recycle(SX);
//    FEccInt64PolynomialPool.Recycle(SY);
//  end;
//end;

procedure TCnInt64PolynomialEcc.PointAddPoint(P, Q,
  Sum: TCnInt64PolynomialEccPoint);
var
  K, X, Y, T: TCnInt64Polynomial;
begin
  K := nil;
  X := nil;
  Y := nil;
  T := nil;

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
    else if Int64PolynomialEqual(P.X, Q.X) and Int64PolynomialEqual(P.Y, Q.Y) then
    begin
      // 俩加数是同一个点，切线斜率为两边求导，3 * X^2 + A / (2 * Y) 但如 Y = 0 则直接是无限远 0。
      X := FEccInt64PolynomialPool.Obtain;
      Y := FEccInt64PolynomialPool.Obtain;

      // X := 3 * P.X * P.X + FCoefficientA
      Int64PolynomialGaloisMul(X, P.X, P.X, FFiniteFieldSize, FPrimitive);
      Int64PolynomialGaloisMulWord(X, 3, FFiniteFieldSize);
      Int64PolynomialGaloisAddWord(X, FCoefficientA, FFiniteFieldSize);

      // Y := 2 * P.Y;
      Int64PolynomialCopy(Y, P.Y);
      Int64PolynomialGaloisMulWord(Y, 2, FFiniteFieldSize);

      if Y.IsZero then
      begin
        Sum.X.SetZero;
        Sum.Y.SetZero;
      end;

      // Y := Y^-1
      T := FEccInt64PolynomialPool.Obtain;
      Int64PolynomialCopy(T, Y);
      Int64PolynomialGaloisModularInverse(Y, T, FPrimitive, FFiniteFieldSize);

      // K := X * Y mod FFiniteFieldSize;
      K := FEccInt64PolynomialPool.Obtain;
      Int64PolynomialGaloisMul(K, X, Y, FFiniteFieldSize, FPrimitive);
      // 得到切线斜率 K
    end
    else // 是不同点
    begin
      if Int64PolynomialEqual(P.X, Q.X) then // 如果 X 相等，要判断 Y 是不是互反，是则和为 0，不是则挂了
      begin
        T := FEccInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisAdd(T, P.Y, Q.Y, FFiniteFieldSize);
        if T.IsZero then
          Sum.SetZero
        else
          raise ECnEccException.CreateFmt('Can NOT Calucate %s,%s + %s,%s',
            [P.X.ToString, P.Y.ToString, Q.X.ToString, Q.Y.ToString]);

        Exit;
      end;

      // 到这里，X 确定不同，斜率 K := ((Q.Y - P.Y) / (Q.X - P.X)) mod p
      X := FEccInt64PolynomialPool.Obtain;
      Y := FEccInt64PolynomialPool.Obtain;
      K := FEccInt64PolynomialPool.Obtain;

      Int64PolynomialGaloisSub(Y, Q.Y, P.Y, FFiniteFieldSize);
      Int64PolynomialGaloisSub(X, Q.X, P.X, FFiniteFieldSize);

      T := FEccInt64PolynomialPool.Obtain;
      Int64PolynomialCopy(T, X);
      Int64PolynomialGaloisModularInverse(X, T, FPrimitive, FFiniteFieldSize);
      Int64PolynomialGaloisMul(K, Y, X, FFiniteFieldSize, FPrimitive); // 得到斜率
    end;

    //  X := K * K - P.X - Q.X;
    Int64PolynomialCopy(X, K);
    Int64PolynomialGaloisMul(X, X, K, FFiniteFieldSize, FPrimitive);
    Int64PolynomialGaloisSub(X, X, P.X, FFiniteFieldSize);
    Int64PolynomialGaloisSub(X, X, Q.X, FFiniteFieldSize);

    // Ysum = (K * (X1 - Xsum) - Y1) mod p
    Int64PolynomialGaloisSub(X, P.X, X, FFiniteFieldSize);
    Int64PolynomialGaloisMul(Y, K, X, FFiniteFieldSize, FPrimitive);
    Int64PolynomialGaloisSub(Y, Y, P.Y, FFiniteFieldSize);

    Int64PolynomialCopy(Sum.X, X);
    Int64PolynomialCopy(Sum.Y, Y);
  finally
    FEccInt64PolynomialPool.Recycle(K);
    FEccInt64PolynomialPool.Recycle(X);
    FEccInt64PolynomialPool.Recycle(Y);
    FEccInt64PolynomialPool.Recycle(T);
  end;
end;

procedure TCnInt64PolynomialEcc.PointInverse(
  P: TCnInt64PolynomialEccPoint);
var
  I: Integer;
begin
  for I := 0 to P.Y.MaxDegree do
    P.Y[I] := FFiniteFieldSize - P.Y[I];
end;

procedure TCnInt64PolynomialEcc.PointSubPoint(P, Q,
  Diff: TCnInt64PolynomialEccPoint);
var
  Inv: TCnInt64PolynomialEccPoint;
begin
  Inv := TCnInt64PolynomialEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    PointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

class procedure TCnInt64PolynomialEcc.RationalMultiplePoint(K: Integer;
  MX, MY: TCnInt64RationalPolynomial; A, B, APrime: Int64; APrimitive: TCnInt64Polynomial);
var
  Neg: Boolean;
  FN, FNa1, FNa2, FNs1, FNs2, P1, P2, X1, Y2: TCnInt64Polynomial;
begin
  if K = 0 then
  begin
    if MX <> nil then
      MX.SetZero;
    if MY <> nil then
      MY.SetZero;
    Exit;
  end;

  Neg := K < 0;
  if Neg then
    K := -K;

  if K = 1 then // 没乘，原封不动返回 x 和 1
  begin
    if MX <> nil then
    begin
      MX.Nominator.SetCoefficents([0, 1]);
      MX.Denominator.SetOne;
    end;

    if MY <> nil then
    begin
      MY.Nominator.SetOne;
      MY.Denominator.SetOne;
    end;
  end
  else
  begin
    FN := FEccInt64PolynomialPool.Obtain;
    FNa1 := FEccInt64PolynomialPool.Obtain;
    FNa2 := FEccInt64PolynomialPool.Obtain;
    FNs1 := FEccInt64PolynomialPool.Obtain;
    FNs2 := FEccInt64PolynomialPool.Obtain;
    X1 := FEccInt64PolynomialPool.Obtain;
    Y2 := FEccInt64PolynomialPool.Obtain;
    P1 := FEccInt64PolynomialPool.Obtain;
    P2 := FEccInt64PolynomialPool.Obtain;

    try
      X1.SetCoefficents([0, 1]);
      Y2.SetCoefficents([B, A, 0, 1]);

      Int64PolynomialGaloisCalcDivisionPolynomial(A, B, K, FN, APrime);
      Int64PolynomialGaloisCalcDivisionPolynomial(A, B, K + 1, FNa1, APrime);
      Int64PolynomialGaloisCalcDivisionPolynomial(A, B, K + 2, FNa2, APrime);
      Int64PolynomialGaloisCalcDivisionPolynomial(A, B, K - 1, FNs1, APrime);
      Int64PolynomialGaloisCalcDivisionPolynomial(A, B, K - 2, FNs2, APrime);

      // 求 X 表达式
      if MX <> nil then
      begin
        if (K and 1) = 0 then // K 偶数时
        begin
          // 结果的 x 坐标为 (x*fn^2 * Y^2 - fn+1 * fn-1) / fn^2 * Y^2
          Int64PolynomialGaloisMul(MX.Denominator, FN, FN, APrime);
          Int64PolynomialGaloisMul(MX.Denominator, MX.Denominator, Y2, APrime);

          Int64PolynomialGaloisMul(P1, FNa1, FNs1, APrime); // P1 得到 fn+1 * fn-1
          Int64PolynomialGaloisMul(P2, FN, FN, APrime);
          Int64PolynomialGaloisMul(P2, P2, X1, APrime);     // P2 得到 x*fn^2
          Int64PolynomialGaloisMul(P2, P2, Y2, APrime);     // P2 得到 x*fn^2 * Y^2

          Int64PolynomialGaloisSub(MX.Nominator, P2, P1, APrime); // MX 计算完毕
        end
        else // K 奇数时
        begin
          // 结果的 x 坐标为 (x*fn^2 - Y^2 * fn+1 * fn-1) / fn^2
          Int64PolynomialGaloisMul(MX.Denominator, FN, FN, APrime);

          Int64PolynomialGaloisMul(P1, FNa1, FNs1, APrime); // P1 得到 fn+1 * fn-1
          Int64PolynomialGaloisMul(P1, P1, Y2, APrime);     // P1 得到 Y^2 * fn+1 * fn-1

          Int64PolynomialGaloisMul(P2, FN, FN, APrime);
          Int64PolynomialGaloisMul(P2, P2, X1, APrime);     // P2 得到 x*fn^2
          Int64PolynomialGaloisSub(MX.Nominator, P2, P1, APrime); // MX 计算完毕
        end;
      end;

      // 求 Y 表达式
      if MY <> nil then
      begin
        if K = 2 then // Y 的分子是 f2n，n 为 2 时不需递归，直接用 f4
        begin
          Int64PolynomialCopy(MY.Nominator, FNa2);
        end
        else
        begin
          // 结果的 y 坐标分子为 fn+2 * fn-1^2 - fn-2 * fn+1 ^2
          Int64PolynomialGaloisMul(P1, FNs1, FNs1, APrime);
          Int64PolynomialGaloisMul(P1, P1, FNa2, APrime);
          Int64PolynomialGaloisMul(P2, FNa1, FNa1, APrime);
          Int64PolynomialGaloisMul(P2, P2, FNs2, APrime);

          Int64PolynomialGaloisSub(MY.Nominator, P1, P2, APrime); // MY 分子计算完毕
        end;

        Int64PolynomialGaloisPower(MY.Denominator, FN, 3, APrime);
        Int64PolynomialGaloisMulWord(MY.Denominator, 4, APrime);   // 奇数分母 4 * fn^3 计算完毕

        if (K and 1) = 0 then // 偶数分母还得乘以 y^4
        begin
          Int64PolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime);
          Int64PolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime);
        end;
      end;
    finally
      FEccInt64PolynomialPool.Recycle(FN);
      FEccInt64PolynomialPool.Recycle(FNa1);
      FEccInt64PolynomialPool.Recycle(FNa2);
      FEccInt64PolynomialPool.Recycle(FNs1);
      FEccInt64PolynomialPool.Recycle(FNs2);
      FEccInt64PolynomialPool.Recycle(X1);
      FEccInt64PolynomialPool.Recycle(Y2);
      FEccInt64PolynomialPool.Recycle(P1);
      FEccInt64PolynomialPool.Recycle(P2);
    end;
  end;

  if Neg then
    MY.Neg;

  if APrimitive <> nil then
  begin
    if MX <> nil then
    begin
      Int64PolynomialGaloisMod(MX.Nominator, MX.Nominator, APrimitive, APrime);
      Int64PolynomialGaloisMod(MX.Denominator, MX.Denominator, APrimitive, APrime);
    end;
    if MY <> nil then
    begin
      Int64PolynomialGaloisMod(MY.Nominator, MY.Nominator, APrimitive, APrime);
      Int64PolynomialGaloisMod(MY.Denominator, MY.Denominator, APrimitive, APrime);
    end;
  end;
end;

class procedure TCnInt64PolynomialEcc.RationalPointAddPoint(PX, PY, QX, QY,
  SX, SY: TCnInt64RationalPolynomial; A, B, APrime: Int64; APrimitive: TCnInt64Polynomial);
var
  R, T1, T2: TCnInt64RationalPolynomial;
  Y2, C: TCnInt64Polynomial;
begin
  // 点 (PX, PY * y) + (QX, QY * y) = (SX, SY * y)
  // 先求斜率 R = y * (QY - PY) / (QX - PX) 或 (3PX^2 + A) / 2PY * y

  if PX.IsZero and PY.IsZero then
  begin
    Int64RationalPolynomialCopy(SX, QX);
    Int64RationalPolynomialCopy(SY, QY);
    Exit;
  end
  else if QX.IsZero and QY.IsZero then
  begin
    Int64RationalPolynomialCopy(SX, PX);
    Int64RationalPolynomialCopy(SY, PY);
    Exit;
  end;

  R := nil;
  T1 := nil;
  T2 := nil;

  Y2 := nil;
  C := nil;

  try
    R := FEccInt64RationalPolynomialPool.Obtain;
    T1 := FEccInt64RationalPolynomialPool.Obtain;
    T2 := FEccInt64RationalPolynomialPool.Obtain;

    Y2 := FEccInt64PolynomialPool.Obtain;
    C := FEccInt64PolynomialPool.Obtain;
    Y2.SetCoefficents([B, A, 0, 1]);

    if Int64RationalPolynomialGaloisEqual(PX, QX, APrime, APrimitive) then // 不能直接判断相等，得互乘后各自针对本原多项式求余后再判断相等
    begin
      // X 相等，判断 Y 是否相等，不等则假设它们相反，返回 0
      // TODO: 判断 PY QY 是否相反
      if not Int64RationalPolynomialGaloisEqual(PY, QY, APrime, APrimitive) then
      begin
        SX.SetZero;
        SY.SetZero;
        Exit;
      end;

      // X Y 都相等，求导
      C.SetCoefficents([3]);

      Int64RationalPolynomialGaloisMul(PX, PX, T1, APrime);
      Int64RationalPolynomialGaloisMul(T1, C, T1, APrime);  // T1 得到 3PX^2

      C.SetCoefficents([A]);
      Int64RationalPolynomialGaloisAdd(T1, C, T1, APrime);  // T1 得到 3PX^2 + A

      C.SetCoefficents([2]);
      Int64RationalPolynomialGaloisMul(PY, C, T2, APrime);  // T2 得到 2PY，实际上还要乘以一个 y

      Int64RationalPolynomialGaloisDiv(T1, T2, R, APrime);  // 得到斜率 R，但真实的斜率分母实际上还要乘以一个 y，后面补上

      // SX = 真实斜率^2 - PX - QX = R^2 / (x^3+Ax+B) - PX - QX
      // 真实斜率的平方 = R^2 / y^2，分母可替换成 x^3+Ax+B
      Int64RationalPolynomialGaloisMul(R, R, SX, APrime);
      Int64RationalPolynomialGaloisDiv(SX, Y2, SX, APrime);
      Int64RationalPolynomialGaloisSub(SX, PX, SX, APrime);
      Int64RationalPolynomialGaloisSub(SX, QX, SX, APrime);

      if APrimitive <> nil then
      begin
        Int64PolynomialGaloisMod(SX.Nominator, SX.Nominator, APrimitive, APrime);
        Int64PolynomialGaloisMod(SX.Denominator, SX.Denominator, APrimitive, APrime);
      end;

      // SY * y = 真实斜率 * (PX - SX) - PY * y
      // SY = (R/y * (PX - SX) - PY * y) / y = R * (PX - SX)/ y^2 - PY
      Int64RationalPolynomialGaloisSub(PX, SX, SY, APrime);
      Int64RationalPolynomialGaloisMul(SY, R, SY, APrime);
      Int64RationalPolynomialGaloisDiv(SY, Y2, SY, APrime);
      Int64RationalPolynomialGaloisSub(SY, PY, SY, APrime);

      if APrimitive <> nil then
      begin
        Int64PolynomialGaloisMod(SY.Nominator, SY.Nominator, APrimitive, APrime);
        Int64PolynomialGaloisMod(SY.Denominator, SY.Denominator, APrimitive, APrime);
      end;
    end
    else
    begin
      // 不相等，减，真实斜率等于 y * (QY - PY) / (QX - PX)
      Int64RationalPolynomialGaloisSub(QY, PY, T1, APrime);
      Int64RationalPolynomialGaloisSub(QX, PX, T2, APrime);
      Int64RationalPolynomialGaloisDiv(T1, T2, R, APrime);

      // R 得到斜率了，但真实的斜率分子实际上还要乘以一个 y，后面补上
      // SX = R^2 * (x^3+Ax+B) - PX - QX
      Int64RationalPolynomialGaloisMul(R, R, SX, APrime);
      Int64RationalPolynomialGaloisMul(SX, Y2, SX, APrime);
      Int64RationalPolynomialGaloisSub(SX, PX, SX, APrime);
      Int64RationalPolynomialGaloisSub(SX, QX, SX, APrime);
      if APrimitive <> nil then
      begin
        Int64PolynomialGaloisMod(SX.Nominator, SX.Nominator, APrimitive, APrime);
        Int64PolynomialGaloisMod(SX.Denominator, SX.Denominator, APrimitive, APrime);
      end;

      // SY * y = R * y * (PX - SX) - PY * y 都除以 y 得 SY = R * (PX - SX) - PY
      Int64RationalPolynomialGaloisSub(PX, SX, SY, APrime);
      Int64RationalPolynomialGaloisMul(SY, R, SY, APrime);
      Int64RationalPolynomialGaloisSub(SY, PY, SY, APrime);

      if APrimitive <> nil then
      begin
        Int64PolynomialGaloisMod(SY.Nominator, SY.Nominator, APrimitive, APrime);
        Int64PolynomialGaloisMod(SY.Denominator, SY.Denominator, APrimitive, APrime);
      end;
    end;
  finally
    FEccInt64PolynomialPool.Recycle(Y2);
    FEccInt64PolynomialPool.Recycle(C);

    FEccInt64RationalPolynomialPool.Recycle(T2);
    FEccInt64RationalPolynomialPool.Recycle(T1);
    FEccInt64RationalPolynomialPool.Recycle(R);
  end;
end;

procedure TCnInt64PolynomialEcc.SetPrimitive(const Value: TCnInt64Polynomial);
begin
  if Value <> nil then
  begin
    if Value.MaxDegree <> FExtension then
      raise ECnEccException.Create('Primitive Polynomial Max Degree must be Field Extension.');
    Int64PolynomialCopy(FPrimitive, Value);
  end;
end;

procedure CnInt64GenerateGaloisDivisionPolynomials(A, B, APrime: Int64; MaxDegree: Integer;
  PolynomialList: TObjectList);
var
  I, N: Integer;

  // 返回第 Degree 个可除表达式的引用，并同时存入 PolynomialList 的对应位置，注意返回值不要改动
  function GetInt64GaloisDivisionPolynomial(Degree: Integer): TCnInt64Polynomial;
  var
    MI, T1, T2: Int64;
    F1, F2, F3, F4, F5: TCnInt64Polynomial;  // 从递归 GetInt64GaloisDivisionPolynomial 拿到的引用，不允许改动
    D1, D2, D3, Y4: TCnInt64Polynomial;      // 计算中间结果，要创建要释放
  begin
    if PolynomialList[Degree] <> nil then // 如果有缓存就返回缓存的
    begin
      Result := TCnInt64Polynomial(PolynomialList[Degree]);
      Exit;
    end;

    if Degree = 0 then
    begin
      Result := TCnInt64Polynomial.Create;
      Result.SetCoefficents([0]);  // f0(X) = 0
      PolynomialList[0] := Result;
    end
    else if Degree = 1 then
    begin
      Result := TCnInt64Polynomial.Create;
      Result.SetCoefficents([1]);  // f1(X) = 1
      PolynomialList[1] := Result;
    end
    else if Degree = 2 then
    begin
      Result := TCnInt64Polynomial.Create;
      Result.SetCoefficents([2]);  // f2(X) = 2
      PolynomialList[2] := Result;
    end
    else if Degree = 3 then   // f3(X) = 3 X4 + 6 a X2 + 12 b X - a^2
    begin
      Result := TCnInt64Polynomial.Create;
      Result.MaxDegree := 4;
      Result[4] := 3;
      Result[3] := 0;
      Result[2] := Int64NonNegativeMulMod(6, A, APrime);
      Result[1] := Int64NonNegativeMulMod(12, B, APrime);
      Result[0] := Int64NonNegativeMulMod(-A, A, APrime);

      PolynomialList[3] := Result;
    end
    else if Degree = 4 then // f4(X) = 4 X6 + 20 a X4 + 80 b X3 - 20 a2X2 - 16 a b X - 4 a3 - 32 b^2
    begin
      Result := TCnInt64Polynomial.Create;
      Result.MaxDegree := 6;
      Result[6] := 4;
      Result[5] := 0;
      Result[4] := Int64NonNegativeMulMod(20, A, APrime);
      Result[3] := Int64NonNegativeMulMod(80, B, APrime);
      Result[2] := Int64NonNegativeMulMod(Int64NonNegativeMulMod(-20, A, APrime), A, APrime);
      Result[1] := Int64NonNegativeMulMod(Int64NonNegativeMulMod(-16, A, APrime), B, APrime);
      T1 := Int64NonNegativeMulMod(Int64NonNegativeMulMod(Int64NonNegativeMulMod(-4, A, APrime), A, APrime), A, APrime);
      T2 := Int64NonNegativeMulMod(Int64NonNegativeMulMod(-32, B, APrime), B, APrime);
      Result[0] := Int64NonNegativeMod(T1 + T2, APrime); // TODO: 暂未处理相加溢出的取模

      PolynomialList[4] := Result;
    end
    else
    begin
      // 计算第 Degree 个可除表达式，中间可能递归调用到自身，同样尽量从缓存中取
      D1 := nil;
      D2 := nil;
      D3 := nil;
      Y4 := nil;

      try
        // 开始递归计算
        N := Degree shr 1;
        if (Degree and 1) = 0 then // Degree 是偶数
        begin
          F1 := GetInt64GaloisDivisionPolynomial(N + 2); // F1 得到 Fn+2
          F2 := GetInt64GaloisDivisionPolynomial(N - 1); // F2 得到 Fn-1

          D2 := FEccInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisMul(D2, F2, F2, APrime);   // D2 得到 Fn-1 ^ 2

          D1 := FEccInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisMul(D1, F1, D2, APrime);   // D1 得到 Fn+2 * Fn-1 ^ 2

          F3 := GetInt64GaloisDivisionPolynomial(N - 2);  // F3 得到 Fn-2
          F4 := GetInt64GaloisDivisionPolynomial(N + 1);  // F4 得到 Fn+1

          Int64PolynomialGaloisMul(D2, F4, F4, APrime);   // D2 得到 Fn+1 ^ 2
          Int64PolynomialGaloisMul(D2, D2, F3, APrime);   // D2 得到 Fn-2 * Fn+1 ^ 2

          Int64PolynomialGaloisSub(D1, D1, D2, APrime);   // D1 得到 Fn+2 * Fn-1 ^ 2 - Fn-2 * Fn+1 ^ 2

          F5 := GetInt64GaloisDivisionPolynomial(N);     // F5 得到 Fn

          Result := TCnInt64Polynomial.Create;
          Int64PolynomialGaloisMul(Result, F5, D1, APrime);           // 相乘得到 Fn * (Fn+2 * Fn-1 ^ 2 - Fn-2 * Fn+1 ^ 2)

          MI := CnInt64ModularInverse(2, APrime);
          Int64PolynomialGaloisMulWord(Result, MI, APrime);           // 再除以 2

          PolynomialList[Degree] := Result;
        end
        else // Degree 是奇数
        begin
          Y4 := FEccInt64PolynomialPool.Obtain;
          Y4.SetCoefficents([B, A, 0, 1]);
          Int64PolynomialGaloisMul(Y4, Y4, Y4, APrime);

          F1 := GetInt64GaloisDivisionPolynomial(N + 2); // F1 得到 Fn+2

          D2 := FEccInt64PolynomialPool.Obtain;
          F2 := GetInt64GaloisDivisionPolynomial(N);     // F2 得到 Fn
          Int64PolynomialGaloisPower(D2, F2, 3, APrime);  // D2 得到 Fn^3

          D3 := FEccInt64PolynomialPool.Obtain;
          F3 := GetInt64GaloisDivisionPolynomial(N + 1); // F3 得到 Fn+1
          Int64PolynomialGaloisPower(D3, F3, 3, APrime);  // D3 得到 Fn+1 ^ 3

          if (N and 1) <> 0 then // N 是奇数
          begin
            D1 := FEccInt64PolynomialPool.Obtain;
            Int64PolynomialGaloisMul(D1, F1, D2, APrime);     // D1 得到 Fn+2 * Fn ^ 3，并释放 D2

            F4 := GetInt64GaloisDivisionPolynomial(N - 1);
            Int64PolynomialGaloisMul(D2, F4, Y4, APrime);     // D2 得到 Fn-1 * Y^2

            Int64PolynomialGaloisMul(D2, D2, D3, APrime);     // D2 得到 Fn+1 ^ 3 * Fn-1(Y)

            Result := TCnInt64Polynomial.Create;
            Int64PolynomialGaloisSub(Result, D1, D2, APrime); // D1 - D2

            PolynomialList[Degree] := Result;
          end
          else // N 是偶数
          begin
            D1 := FEccInt64PolynomialPool.Obtain;
            Int64PolynomialGaloisMul(D1, F1, D2, APrime);     // D1 得到 Fn+2 * Fn ^ 3，并释放 D2
            Int64PolynomialGaloisMul(D1, D1, Y4, APrime);     // D1 得到 Y * Fn+2 * Fn ^ 3

            F4 := GetInt64GaloisDivisionPolynomial(N - 1);   // F4 得到 Fn-1

            Int64PolynomialGaloisMul(D2, F4, D3, APrime);     // D2 得到 Fn+1 ^ 3 * Fn-1

            Result := TCnInt64Polynomial.Create;
            Int64PolynomialGaloisSub(Result, D1, D2, APrime); // D1 - D2

            PolynomialList[Degree] := Result;
          end;
        end;
      finally
        FEccInt64PolynomialPool.Recycle(D1);
        FEccInt64PolynomialPool.Recycle(D2);
        FEccInt64PolynomialPool.Recycle(D3);
        FEccInt64PolynomialPool.Recycle(Y4);
      end;
    end;
  end;

begin
  // 生成 0 至 MaxDegree 的可除多项式并存储于 PolynomialList 中。
  PolynomialList.Clear;
  PolynomialList.Count := MaxDegree + 1;

  for I := 0 to MaxDegree do
    GetInt64GaloisDivisionPolynomial(I);
end;

procedure CnGenerateGaloisDivisionPolynomials(A, B, APrime: TCnBigNumber; MaxDegree: Integer;
  PolynomialList: TObjectList);
var
  I: Integer;

  // 返回第 Degree 个可除表达式的引用，并同时存入 PolynomialList 的对应位置，注意返回值不要改动
  function GetGaloisDivisionPolynomial(Degree: Integer): TCnBigNumberPolynomial;
  var
    N: Integer;
    MI, T: TCnBigNumber;
    F1, F2, F3, F4, F5: TCnBigNumberPolynomial;  // 从递归 GetGaloisDivisionPolynomial 拿到的引用，不允许改动
    D1, D2, D3, Y4: TCnBigNumberPolynomial;      // 计算中间结果，要创建要释放
  begin
    if PolynomialList[Degree] <> nil then // 如果有缓存就返回缓存的
    begin
      Result := TCnBigNumberPolynomial(PolynomialList[Degree]);
      Exit;
    end;

    if Degree = 0 then
    begin
      Result := TCnBigNumberPolynomial.Create;
      Result.SetCoefficents([0]);  // f0(X) = 0
      PolynomialList[0] := Result;
    end
    else if Degree = 1 then
    begin
      Result := TCnBigNumberPolynomial.Create;
      Result.SetCoefficents([1]);  // f1(X) = 1
      PolynomialList[1] := Result;
    end
    else if Degree = 2 then
    begin
      Result := TCnBigNumberPolynomial.Create;
      Result.SetCoefficents([2]);  // f2(X) = 2
      PolynomialList[2] := Result;
    end
    else if Degree = 3 then   // f3(X) = 3 X4 + 6 a X2 + 12 b X - a^2
    begin
      Result := TCnBigNumberPolynomial.Create;
      Result.MaxDegree := 4;
      Result[4].SetWord(3);
      Result[3].SetWord(0);
      BigNumberMulWordNonNegativeMod(Result[2], A, 6, APrime);
      BigNumberMulWordNonNegativeMod(Result[1], B, 12, APrime);

      T := FEccBigNumberPool.Obtain;
      try
        BigNumberCopy(T, A);
        T.Negate;
        BigNumberDirectMulMod(Result[0], T, A, APrime);
      finally
        FEccBigNumberPool.Recycle(T);
      end;
      PolynomialList[3] := Result;
    end
    else if Degree = 4 then // f4(X) = 4 X6 + 20 a X4 + 80 b X3 - 20 a2X2 - 16 a b X - 4 a3 - 32 b^2
    begin
      Result := TCnBigNumberPolynomial.Create;
      Result.MaxDegree := 6;
      Result[6].SetWord(4);
      Result[5].SetWord(0);
      BigNumberMulWordNonNegativeMod(Result[4], A, 20, APrime);
      BigNumberMulWordNonNegativeMod(Result[3], B, 80, APrime);

      T := FEccBigNumberPool.Obtain;
      try
        BigNumberMulWordNonNegativeMod(T, A, -20, APrime);
        BigNumberDirectMulMod(Result[2], T, A, APrime);
        BigNumberMulWordNonNegativeMod(T, A, -16, APrime);
        BigNumberDirectMulMod(Result[1], T, B, APrime);

        BigNumberMulWordNonNegativeMod(T, A, -4, APrime);
        BigNumberDirectMulMod(T, T, A, APrime);
        BigNumberDirectMulMod(Result[0], T, A, APrime);

        BigNumberMulWordNonNegativeMod(T, B, -32, APrime);
        BigNumberDirectMulMod(T, T, B, APrime);
        BigNumberAdd(Result[0], Result[0], T);
        BigNumberNonNegativeMod(Result[0], Result[0], APrime);
      finally
        FEccBigNumberPool.Recycle(T);
      end;
      PolynomialList[4] := Result;
    end
    else
    begin
      // 计算第 Degree 个可除表达式，中间可能递归调用到自身，同样尽量从缓存中取
      D1 := nil;
      D2 := nil;
      D3 := nil;
      Y4 := nil;
      MI := nil;

      try
        // 开始递归计算
        N := Degree shr 1;
        if (Degree and 1) = 0 then // Degree 是偶数
        begin
          F1 := GetGaloisDivisionPolynomial(N + 2); // F1 得到 Fn+2
          F2 := GetGaloisDivisionPolynomial(N - 1); // F2 得到 Fn-1

          D2 := FEccPolynomialPool.Obtain;
          BigNumberPolynomialGaloisMul(D2, F2, F2, APrime);   // D2 得到 Fn-1 ^ 2

          D1 := FEccPolynomialPool.Obtain;
          BigNumberPolynomialGaloisMul(D1, F1, D2, APrime);   // D1 得到 Fn+2 * Fn-1 ^ 2

          F3 := GetGaloisDivisionPolynomial(N - 2);  // F3 得到 Fn-2
          F4 := GetGaloisDivisionPolynomial(N + 1);  // F4 得到 Fn+1

          BigNumberPolynomialGaloisMul(D2, F4, F4, APrime);   // D2 得到 Fn+1 ^ 2
          BigNumberPolynomialGaloisMul(D2, D2, F3, APrime);   // D2 得到 Fn-2 * Fn+1 ^ 2

          BigNumberPolynomialGaloisSub(D1, D1, D2, APrime);   // D1 得到 Fn+2 * Fn-1 ^ 2 - Fn-2 * Fn+1 ^ 2

          F5 := GetGaloisDivisionPolynomial(N);     // F5 得到 Fn

          Result := TCnBigNumberPolynomial.Create;
          BigNumberPolynomialGaloisMul(Result, F5, D1, APrime);           // 相乘得到 Fn * (Fn+2 * Fn-1 ^ 2 - Fn-2 * Fn+1 ^ 2)

          MI := FEccBigNumberPool.Obtain;
          BigNumberModularInverseWord(MI, 2, APrime);
          BigNumberPolynomialGaloisMulBigNumber(Result, MI, APrime);           // 再除以 2

          PolynomialList[Degree] := Result;
        end
        else // Degree 是奇数
        begin
          Y4 := FEccPolynomialPool.Obtain;
          Y4.MaxDegree := 3;
          BigNumberCopy(Y4[0], B);
          BigNumberCopy(Y4[1], A);
          Y4[2].SetZero;
          Y4[3].SetOne;

          BigNumberPolynomialGaloisMul(Y4, Y4, Y4, APrime);

          F1 := GetGaloisDivisionPolynomial(N + 2); // F1 得到 Fn+2

          D2 := FEccPolynomialPool.Obtain;
          F2 := GetGaloisDivisionPolynomial(N);     // F2 得到 Fn
          BigNumberPolynomialGaloisPower(D2, F2, 3, APrime);  // D2 得到 Fn^3

          D3 := FEccPolynomialPool.Obtain;
          F3 := GetGaloisDivisionPolynomial(N + 1); // F3 得到 Fn+1
          BigNumberPolynomialGaloisPower(D3, F3, 3, APrime);  // D3 得到 Fn+1 ^ 3

          if (N and 1) <> 0 then // N 是奇数
          begin
            D1 := FEccPolynomialPool.Obtain;
            BigNumberPolynomialGaloisMul(D1, F1, D2, APrime);     // D1 得到 Fn+2 * Fn ^ 3，并释放 D2

            F4 := GetGaloisDivisionPolynomial(N - 1);
            BigNumberPolynomialGaloisMul(D2, F4, Y4, APrime);     // D2 得到 Fn-1 * Y^2

            BigNumberPolynomialGaloisMul(D2, D2, D3, APrime);     // D2 得到 Fn+1 ^ 3 * Fn-1(Y)

            Result := TCnBigNumberPolynomial.Create;
            BigNumberPolynomialGaloisSub(Result, D1, D2, APrime); // D1 - D2

            PolynomialList[Degree] := Result;
          end
          else // N 是偶数
          begin
            D1 := FEccPolynomialPool.Obtain;
            BigNumberPolynomialGaloisMul(D1, F1, D2, APrime);     // D1 得到 Fn+2 * Fn ^ 3，并释放 D2
            BigNumberPolynomialGaloisMul(D1, D1, Y4, APrime);     // D1 得到 Y * Fn+2 * Fn ^ 3

            F4 := GetGaloisDivisionPolynomial(N - 1);   // F4 得到 Fn-1

            BigNumberPolynomialGaloisMul(D2, F4, D3, APrime);     // D2 得到 Fn+1 ^ 3 * Fn-1

            Result := TCnBigNumberPolynomial.Create;
            BigNumberPolynomialGaloisSub(Result, D1, D2, APrime); // D1 - D2

            PolynomialList[Degree] := Result;
          end;
        end;
      finally
        FEccPolynomialPool.Recycle(D1);
        FEccPolynomialPool.Recycle(D2);
        FEccPolynomialPool.Recycle(D3);
        FEccPolynomialPool.Recycle(Y4);
        FEccBigNumberPool.Recycle(MI);
      end;
    end;
  end;

begin
  // 生成 0 至 MaxDegree 的可除多项式并存储于 PolynomialList 中。
  PolynomialList.Clear;
  PolynomialList.Count := MaxDegree + 1;

  for I := 0 to MaxDegree do
    GetGaloisDivisionPolynomial(I);
end;

// 用可除多项式直接算到 K 次倍点的坐标，原理如下：
// (x, y) * K 用可除多项式计算出的结果可以写作 (F(x), G(x) * y)
// 那么 (f(x), g(x) * y) * K 用可除多项式计算出的结果可以代入写作(F(f(x))，G(f(x)) * g(x) * y)
// 本函数返回 F(f(x))
procedure Int64RationalMultiplePointX(Res, PX: TCnInt64RationalPolynomial; K: Integer;
  A, B, APrime: Int64; DivisionPolynomialList: TObjectList; APrimitive: TCnInt64Polynomial);
var
  MX: TCnInt64RationalPolynomial;
  FN, FNa1, FNs1, P1, P2, X1, Y2: TCnInt64Polynomial;
begin
  if K = 0 then
  begin
    Res.SetZero;
    Exit;
  end;

  if K < 0 then
    K := -K;

  MX := FEccInt64RationalPolynomialPool.Obtain;
  if K = 1 then
  begin
    MX.Nominator.SetCoefficents([0, 1]);
    MX.Denominator.SetOne;
  end
  else
  begin
    X1 := FEccInt64PolynomialPool.Obtain;
    Y2 := FEccInt64PolynomialPool.Obtain;
    P1 := FEccInt64PolynomialPool.Obtain;
    P2 := FEccInt64PolynomialPool.Obtain;

    try
      X1.SetCoefficents([0, 1]);
      Y2.SetCoefficents([B, A, 0, 1]);

      FN := TCnInt64Polynomial(DivisionPolynomialList[K]);
      FNa1 := TCnInt64Polynomial(DivisionPolynomialList[K + 1]);
      FNs1 := TCnInt64Polynomial(DivisionPolynomialList[K - 1]);

      // 求 X 表达式
      if (K and 1) = 0 then // K 偶数时
      begin
        // 结果的 x 坐标为 (x*fn^2 * Y^2 - fn+1 * fn-1) / fn^2 * Y^2
        Int64PolynomialGaloisMul(MX.Denominator, FN, FN, APrime, APrimitive);
        Int64PolynomialGaloisMul(MX.Denominator, MX.Denominator, Y2, APrime, APrimitive);

        Int64PolynomialGaloisMul(P1, FNa1, FNs1, APrime, APrimitive); // P1 得到 fn+1 * fn-1
        Int64PolynomialGaloisMul(P2, FN, FN, APrime, APrimitive);
        Int64PolynomialGaloisMul(P2, P2, X1, APrime, APrimitive);     // P2 得到 x*fn^2
        Int64PolynomialGaloisMul(P2, P2, Y2, APrime, APrimitive);     // P2 得到 x*fn^2 * Y^2

        Int64PolynomialGaloisSub(MX.Nominator, P2, P1, APrime, APrimitive); // MX 计算完毕
      end
      else // K 奇数时
      begin
        // 结果的 x 坐标为 (x*fn^2 - Y^2 * fn+1 * fn-1) / fn^2
        Int64PolynomialGaloisMul(MX.Denominator, FN, FN, APrime, APrimitive);

        Int64PolynomialGaloisMul(P1, FNa1, FNs1, APrime, APrimitive); // P1 得到 fn+1 * fn-1
        Int64PolynomialGaloisMul(P1, P1, Y2, APrime, APrimitive);     // P1 得到 Y^2 * fn+1 * fn-1

        Int64PolynomialGaloisMul(P2, FN, FN, APrime, APrimitive);
        Int64PolynomialGaloisMul(P2, P2, X1, APrime, APrimitive);     // P2 得到 x*fn^2
        Int64PolynomialGaloisSub(MX.Nominator, P2, P1, APrime, APrimitive); // MX 计算完毕
      end;
    finally
      FEccInt64PolynomialPool.Recycle(X1);
      FEccInt64PolynomialPool.Recycle(Y2);
      FEccInt64PolynomialPool.Recycle(P1);
      FEccInt64PolynomialPool.Recycle(P2);
    end;

    if APrimitive <> nil then
    begin
      Int64PolynomialGaloisMod(MX.Nominator, MX.Nominator, APrimitive, APrime);
      Int64PolynomialGaloisMod(MX.Denominator, MX.Denominator, APrimitive, APrime);
    end;
  end;

  Int64RationalPolynomialGaloisCompose(Res, MX, PX, APrime, APrimitive);
  FEccInt64RationalPolynomialPool.Recycle(MX);

  if APrimitive <> nil then
  begin
    Int64PolynomialGaloisMod(Res.Nominator, Res.Nominator, APrimitive, APrime);
    Int64PolynomialGaloisMod(Res.Denominator, Res.Denominator, APrimitive, APrime);
  end;
end;

// 用可除多项式直接算到 K 次倍点的坐标，原理如下：
// (x, y) * K 用可除多项式计算出的结果可以写作 (F(x), G(x) * y)
// 那么 (f(x), g(x) * y) * K 用可除多项式计算出的结果可以代入写作(F(f(x))，G(f(x)) * g(x) * y)
// 本函数返回 G(f(x)) * g(x)
procedure Int64RationalMultiplePointY(Res, PX, PY: TCnInt64RationalPolynomial; K: Integer;
  A, B, APrime: Int64; DivisionPolynomialList: TObjectList; APrimitive: TCnInt64Polynomial);
var
  Neg: Boolean;
  MY: TCnInt64RationalPolynomial;
  FN, FNa1, FNa2, FNs1, FNs2, P1, P2, X1, Y2: TCnInt64Polynomial;
begin
  if K = 0 then
  begin
    Res.SetZero;
    Exit;
  end;

  Neg := K < 0;
  if K < 0 then
    K := -K;

  MY := FEccInt64RationalPolynomialPool.Obtain;
  if K = 1 then // 没乘，原封不动返回 x 和 1
  begin
    MY.Nominator.SetOne;
    MY.Denominator.SetOne;
  end
  else
  begin
    X1 := FEccInt64PolynomialPool.Obtain;
    Y2 := FEccInt64PolynomialPool.Obtain;
    P1 := FEccInt64PolynomialPool.Obtain;
    P2 := FEccInt64PolynomialPool.Obtain;

    try
      X1.SetCoefficents([0, 1]);
      Y2.SetCoefficents([B, A, 0, 1]);

      FN := TCnInt64Polynomial(DivisionPolynomialList[K]);
      FNa1 := TCnInt64Polynomial(DivisionPolynomialList[K + 1]);
      FNa2 := TCnInt64Polynomial(DivisionPolynomialList[K + 2]);
      FNs1 := TCnInt64Polynomial(DivisionPolynomialList[K - 1]);
      FNs2 := TCnInt64Polynomial(DivisionPolynomialList[K - 2]);

      if K = 2 then // Y 的分子是 f2n，n 为 2 时不需递归，直接用 f4
      begin
        MY.Denominator.SetOne;
        Int64PolynomialCopy(MY.Nominator, FNa2);
      end
      else
      begin
        // 结果的 y 坐标分子为 fn+2 * fn-1^2 - fn-2 * fn+1 ^2
        Int64PolynomialGaloisMul(P1, FNs1, FNs1, APrime, APrimitive);
        Int64PolynomialGaloisMul(P1, P1, FNa2, APrime, APrimitive);
        Int64PolynomialGaloisMul(P2, FNa1, FNa1, APrime, APrimitive);
        Int64PolynomialGaloisMul(P2, P2, FNs2, APrime, APrimitive);

        Int64PolynomialGaloisSub(MY.Nominator, P1, P2, APrime, APrimitive); // MY 分子计算完毕
      end;

      Int64PolynomialGaloisPower(MY.Denominator, FN, 3, APrime, APrimitive);
      Int64PolynomialGaloisMulWord(MY.Denominator, 4, APrime);   // 奇数分母 4 * fn^3 计算完毕

      if (K and 1) = 0 then // 偶数分母还得乘以 y^4
      begin
        Int64PolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime, APrimitive);
        Int64PolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime, APrimitive);
      end;
    finally
      FEccInt64PolynomialPool.Recycle(X1);
      FEccInt64PolynomialPool.Recycle(Y2);
      FEccInt64PolynomialPool.Recycle(P1);
      FEccInt64PolynomialPool.Recycle(P2);
    end;
  end;

  if Neg then
    MY.Neg;

  if APrimitive <> nil then
  begin
    Int64PolynomialGaloisMod(MY.Nominator, MY.Nominator, APrimitive, APrime);
    Int64PolynomialGaloisMod(MY.Denominator, MY.Denominator, APrimitive, APrime);
  end;

  Int64RationalPolynomialGaloisCompose(Res, MY, PX, APrime, APrimitive);
  Int64RationalPolynomialGaloisMul(Res, PY, Res, APrime);
  FEccInt64RationalPolynomialPool.Recycle(MY);

  if APrimitive <> nil then
  begin
    Int64PolynomialGaloisMod(Res.Nominator, Res.Nominator, APrimitive, APrime);
    Int64PolynomialGaloisMod(Res.Denominator, Res.Denominator, APrimitive, APrime);
  end;
end;

function CnInt64EccSchoof(A, B, Q: Int64): Int64;
var
  Pa, Ta: TCnInt64List;
  QMul, QMax, L, K, W: Int64;
  I, J: Integer;
  Q2Lo, Q2Hi: TUInt64;
  F, G, Y2, P1, P2, LDP: TCnInt64Polynomial;
  Pi2PX, Pi2PY, PiPX, PiPY, KPX, KPY, LSX, LSY, RSX, RSY, TSX, TSY: TCnInt64RationalPolynomial;
  DPs: TObjectList;
begin
  // 用 Schoof 算法求椭圆曲线 y^2 = x^3 + Ax + B 在素域 Fq 上的点总数
  // 先建个 List，存所需的 2 ~ lmax 的素数，其中 3 * ... * lmax 刚好 > 4 倍根号 q
  // 求 x^q -x 与 x^3 + Ax + B 的公因式，如果是 1 则 t2 = 1，否则 t2 = 0，
  // 这里 t2 是 List 中针对素数 2 的元素，并非下标，后面同

  Pa := nil;
  Ta := nil;

  Y2 := FEccInt64PolynomialPool.Obtain;
  P1 := FEccInt64PolynomialPool.Obtain;
  P2 := FEccInt64PolynomialPool.Obtain;

  F := FEccInt64PolynomialPool.Obtain;
  G := FEccInt64PolynomialPool.Obtain;

  QMax := 4 * (UInt64Sqrt(Q) + 1);
  QMul := 1;
  I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

  DPs := nil;
  Pi2PX := FEccInt64RationalPolynomialPool.Obtain;
  Pi2PY := FEccInt64RationalPolynomialPool.Obtain;
  PiPX := FEccInt64RationalPolynomialPool.Obtain;
  PiPY := FEccInt64RationalPolynomialPool.Obtain;
  KPX := FEccInt64RationalPolynomialPool.Obtain;
  KPY := FEccInt64RationalPolynomialPool.Obtain;
  LSX := FEccInt64RationalPolynomialPool.Obtain;
  LSY := FEccInt64RationalPolynomialPool.Obtain;
  RSX := FEccInt64RationalPolynomialPool.Obtain;
  RSY := FEccInt64RationalPolynomialPool.Obtain;
  TSX := FEccInt64RationalPolynomialPool.Obtain;
  TSY := FEccInt64RationalPolynomialPool.Obtain;

  try
    Pa := TCnInt64List.Create;
    Ta := TCnInt64List.Create;

    while (QMul <= QMax) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      QMul := QMul * CN_PRIME_NUMBERS_SQRT_UINT32[I];
      Pa.Add(CN_PRIME_NUMBERS_SQRT_UINT32[I]);
      Ta.Add(0);
      Inc(I);
    end;

    if I > High(CN_PRIME_NUMBERS_SQRT_UINT32) then
      raise ECnEccException.Create('Prime Number is Too Large.');

    Y2.SetCoefficents([B, A, 0, 1]);

    // Ta 与 Pa 数组已准备好，先处理 t = 2 的情况
    P1.SetCoefficents([0, 1]); // P1 := X
    Int64PolynomialGaloisPower(P1, P1, Q, Q, Y2); // X^q 先 mod Y^2

    P2.SetCoefficents([0, 1]); // P2 := X
    Int64PolynomialGaloisSub(P1, P1, P2, Q); // P1 := (X^q mod Y^2) - x

    // 求最大公约式
    Int64PolynomialGaloisGreatestCommonDivisor(G, P1, Y2, Q);

    if G.IsOne then
      Ta[0] := 1
    else
      Ta[0] := 0;   // 求得 T2。理解了并且基本算对了

    // 提前算好最大素数 + 2 阶的可除多项式们以及准备好 Y^2
    DPs := TObjectList.Create(True);
    CnInt64GenerateGaloisDivisionPolynomials(A, B, Q, Pa[Pa.Count - 1] + 2, DPs);

    for I := 1 to Ta.Count - 1 do  // 针对每一个 L
    begin
      L := Pa[I];
      K := Q mod L;

      // 先得到 L 阶可除多项式，作为后续计算的模多项式
      LDP := TCnInt64Polynomial(DPs[L]);

      Pi2PX.SetOne;                           // 原始点
      Pi2PX.Nominator.SetCoefficents([0, 1]); // x
      Pi2PY.Setone;                           // 1 * y

      // 算得 π^2 的 X 坐标在 LDP 环内的表达分式，也就是 Q*Q 个 x 相乘再 mod LDP
      Int64PolynomialGaloisPower(Pi2PX.Nominator, Pi2PX.Nominator, Q, Q, LDP);
      Int64PolynomialGaloisPower(Pi2PX.Nominator, Pi2PX.Nominator, Q, Q, LDP);  // 直接 Q*Q 容易溢出，分步算

      // 算得 π^2 的 Y 坐标在 LDP 环内的表达分式，Q*Q 个 y 相乘等于 y * [(Q*Q shr 1) 个 y^2 相乘]，而 y^2 可替换成 x^3+Ax+B
      UInt64MulUInt64(Q, Q, Q2Lo, Q2Hi);
      if Q2Hi = 0 then
        Int64PolynomialGaloisPower(Pi2PY.Nominator, Y2, (Q * Q) shr 1, Q, LDP)
      else
      begin
        // 处理 (Q * Q) > UInt64 的情形，还是有问题
        Q2Lo := Q2Lo shr 1;
        if (Q2Hi and 1) <> 0 then
          Q2Lo := Q2Lo or $8000000000000000;
        Q2Hi := Q2Hi shr 1;

        Int64PolynomialGaloisPower(Pi2PY.Nominator, Y2, Q2Lo, Q, LDP, Q2Hi);
      end;

      KPX.SetOne;                             // 原始点
      KPX.Nominator.SetCoefficents([0, 1]);   // x
      KPY.SetOne;                             // 1 * y

      // 算得 K * P 的 X Y 坐标
      TCnInt64PolynomialEcc.RationalMultiplePoint(K, KPX, KPY, A, B, Q, LDP);

      PiPX.SetOne;                            // 原始点
      PiPX.Nominator.SetCoefficents([0, 1]);  // x
      PiPY.Setone;                            // 1 * y

      // 求 π^2(P) + K * (P) 的和点 SX SY
      TCnInt64PolynomialEcc.RationalPointAddPoint(Pi2PX, Pi2PY, KPX, KPY, LSX, LSY, A, B, Q, LDP);

      if LSX.IsZero and LSY.IsZero then  // 如果和点为 0，则表示 t * π结果等于 0，t 自然等于 0
        Ta[I] := 0
      else
      begin
        // 算得 π的 X 坐标在 LDP 环内的表达分式，也就是 Q 个 x 相乘再 mod LDP
        Int64PolynomialGaloisPower(PiPX.Nominator, PiPX.Nominator, Q, Q, LDP);

        // 算得 π的 Y 坐标在 LDP 环内的表达分式，Q 个 y 相乘等于 y * [(Q shr 1) 个 y^2 相乘]，而 y^2 可替换成 x^3+Ax+B
        Int64PolynomialGaloisPower(PiPY.Nominator, Y2, Q shr 1, Q, LDP);

        Int64RationalPolynomialCopy(RSX, PiPX);
        Int64RationalPolynomialCopy(RSY, PiPY);
        for J := 1 to (L + 1) shr 1 do
        begin
          // 本来可以直接用可除多项式计算 RSX := J * (PiPX, PiPY) 的 X，但似乎还会比点加慢，还是用点加
          // Int64RationalMultiplePointX(RSX, PiPX, J, A, B, Q, DPs, LDP);

          if Int64RationalPolynomialGaloisEqual(LSX, RSX, Q, LDP) then
          begin
            // 本来可以直接用可除多项式计算 RSY := J * (PiPX, PiPY) 的 Y，但似乎还会比点加慢，还是用点加
            // Int64RationalMultiplePointY(RSY, PiPX, PiPY, J, A, B, Q, DPs, LDP);

            if Int64RationalPolynomialGaloisEqual(LSY, RSY, Q, LDP) then
              Ta[I] := J
            else
              Ta[I] := L - J;
            Break;
          end;

          // 本来可以直接用可除多项式计算，不在此处反复相加，但前者较慢放弃，还是点加
          TCnInt64PolynomialEcc.RationalPointAddPoint(RSX, RSY, PiPX, PiPY, TSX, TSY, A, B, Q, LDP);
          Int64RationalPolynomialCopy(RSX, TSX);
          Int64RationalPolynomialCopy(RSY, TSY);
        end;
      end;
    end;

    // 求出各个余数后，用中国剩余定理求最终解
    L := ChineseRemainderTheoremInt64(Ta, Pa); // 复用 L W K 等变量

    // 注意求出的 T 必须满足 Hasse 定理：T 的绝对值 <= 2 * 根号 Q，如超出范围，还得修正
    K := UInt64Sqrt(TUInt64(Q)) * 2 + 1;
    if (L <= -K) or (L >= K) then
    begin
      // 中国剩余定理求出的一般是最小正数，需要减去全体 Pa 的乘积
      W := 1;
      for J := 0 to Pa.Count - 1 do
        W := W * Pa[J];

      if L <= -K then
        L := L + W
      else
        L := L - W;
    end;

    Result := Q + 1 - L;
  finally
    FEccInt64PolynomialPool.Recycle(Y2);
    FEccInt64PolynomialPool.Recycle(P1);
    FEccInt64PolynomialPool.Recycle(P2);

    FEccInt64PolynomialPool.Recycle(G);
    FEccInt64PolynomialPool.Recycle(F);

    FEccInt64RationalPolynomialPool.Recycle(Pi2PX);
    FEccInt64RationalPolynomialPool.Recycle(Pi2PY);
    FEccInt64RationalPolynomialPool.Recycle(PiPX);
    FEccInt64RationalPolynomialPool.Recycle(PiPY);
    FEccInt64RationalPolynomialPool.Recycle(KPX);
    FEccInt64RationalPolynomialPool.Recycle(KPY);
    FEccInt64RationalPolynomialPool.Recycle(LSX);
    FEccInt64RationalPolynomialPool.Recycle(LSY);
    FEccInt64RationalPolynomialPool.Recycle(RSX);
    FEccInt64RationalPolynomialPool.Recycle(RSY);
    FEccInt64RationalPolynomialPool.Recycle(TSX);
    FEccInt64RationalPolynomialPool.Recycle(TSY);

    DPs.Free;
    Pa.Free;
    Ta.Free;
  end;
end;

{ TCnPolynomialEccPoint }

procedure TCnPolynomialEccPoint.Assign(Source: TPersistent);
begin
  if Source is TCnPolynomialEccPoint then
  begin
    BigNumberPolynomialCopy(FX, (Source as TCnPolynomialEccPoint).X);
    BigNumberPolynomialCopy(FY, (Source as TCnPolynomialEccPoint).Y);
  end
  else
    inherited;
end;

constructor TCnPolynomialEccPoint.Create;
begin
  inherited;
  FX := TCnBigNumberPolynomial.Create;
  FY := TCnBigNumberPolynomial.Create;
end;

constructor TCnPolynomialEccPoint.Create(const XLowToHighCoefficients,
  YLowToHighCoefficients: array of const);
begin
  Create;
  FX.SetCoefficents(XLowToHighCoefficients);
  FY.SetCoefficents(YLowToHighCoefficients);
end;

destructor TCnPolynomialEccPoint.Destroy;
begin
  FY.Free;
  FX.Free;
  inherited;
end;

function TCnPolynomialEccPoint.IsZero: Boolean;
begin
  Result := FX.IsZero and FY.IsZero;
end;

procedure TCnPolynomialEccPoint.SetX(const Value: TCnBigNumberPolynomial);
begin
  if Value <> nil then
    BigNumberPolynomialCopy(FX, Value);
end;

procedure TCnPolynomialEccPoint.SetY(const Value: TCnBigNumberPolynomial);
begin
  if Value <> nil then
    BigNumberPolynomialCopy(FY, Value);
end;

procedure TCnPolynomialEccPoint.SetZero;
begin
  FX.SetZero;
  FY.SetZero;
end;

function TCnPolynomialEccPoint.ToString: string;
begin
  Result := CnPolynomialEccPointToString(Self);
end;

{ TCnPolynomialEcc }

constructor TCnPolynomialEcc.Create(A, B, FieldPrime: TCnBigNumber;
  Ext: Integer; GX, GY: TCnBigNumberPolynomial; AnOrder: TCnBigNumber;
  PrimitivePolynomial: TCnBigNumberPolynomial);
begin
  inherited Create;
  if not BigNumberIsProbablyPrime(FieldPrime) then
    raise ECnEccException.Create('Infinite Field must be a Prime Number.');

  // 扩域次数得大于 1
  if Ext <= 1 then
    raise ECnEccException.Create('Field Extension must > 1.');

  // TODO: 要确保 4*a^3+27*b^2 <> 0，由外界确定

  FGenerator := TCnPolynomialEccPoint.Create;
  FCoefficientB := TCnBigNumber.Create;
  FCoefficientA := TCnBigNumber.Create;
  FOrder := TCnBigNumber.Create;
  FFiniteFieldSize := TCnBigNumber.Create;
  FPrimitive := TCnBigNumberPolynomial.Create;

  BigNumberCopy(FCoefficientA, A);
  BigNumberCopy(FCoefficientB, B);
  BigNumberCopy(FOrder, AnOrder);
  BigNumberCopy(FFiniteFieldSize, FieldPrime);

  BigNumberPolynomialCopy(FGenerator.X, GX);
  BigNumberPolynomialCopy(FGenerator.Y, GY);

  FExtension := Ext;
  BigNumberPolynomialCopy(FPrimitive, PrimitivePolynomial);
end;

constructor TCnPolynomialEcc.Create(const A, B, FieldPrime: AnsiString;
  Ext: Integer; GX, GY: TCnBigNumberPolynomial; const Order: AnsiString;
  PrimitivePolynomial: TCnBigNumberPolynomial);
var
  BA, BB, BFP, BO: TCnBigNumber;
begin
  BA := nil;
  BB := nil;
  BFP := nil;
  BO := nil;

  try
    BA := FEccBigNumberPool.Obtain;
    BB := FEccBigNumberPool.Obtain;
    BFP := FEccBigNumberPool.Obtain;
    BO := FEccBigNumberPool.Obtain;

    BA.SetHex(A);
    BB.SetHex(B);
    BFP.SetHex(FieldPrime);
    BO.SetHex(Order);

    Create(BA, BB, BFP, Ext, GX, GY, BO, PrimitivePolynomial);
  finally
    FEccBigNumberPool.Recycle(BO);
    FEccBigNumberPool.Recycle(BB);
    FEccBigNumberPool.Recycle(BFP);
    FEccBigNumberPool.Recycle(BA);
  end;
end;

destructor TCnPolynomialEcc.Destroy;
begin
  FPrimitive.Free;
  FGenerator.Free;
  FCoefficientB.Free;
  FCoefficientA.Free;
  FOrder.Free;
  FFiniteFieldSize.Free;
  inherited;
end;

function TCnPolynomialEcc.DivisionPolynomial(Degree: Integer;
  outDivisionPolynomial: TCnBigNumberPolynomial): Boolean;
begin
  Result := BigNumberPolynomialGaloisCalcDivisionPolynomial(FCoefficientA, FCoefficientB,
    Degree, outDivisionPolynomial, FFiniteFieldSize);
end;

function TCnPolynomialEcc.IsPointOnCurve(P: TCnPolynomialEccPoint): Boolean;
var
  X, Y: TCnBigNumberPolynomial;
begin
  // 计算 (Y^2 - X^3 - A*X - B) mod primitive （多项式系数运算要 mod p）是否等于 0 多项式
  Result := False;
  if P = nil then
    Exit;

  X := nil;
  Y := nil;

  try
    X := FEccPolynomialPool.Obtain;
    Y := FEccPolynomialPool.Obtain;

    BigNumberPolynomialCopy(Y, P.Y);
    BigNumberPolynomialGaloisMul(Y, Y, Y, FFiniteFieldSize, FPrimitive);

    BigNumberPolynomialCopy(X, P.X);
    BigNumberPolynomialGaloisPower(X, X, 3, FFiniteFieldSize, FPrimitive);

    BigNumberPolynomialGaloisSub(Y, Y, X, FFiniteFieldSize, FPrimitive);   // Y := Y^2 - X^3 mod

    BigNumberPolynomialCopy(X, P.X);
    BigNumberPolynomialMulBigNumber(X, FCoefficientA);
    BigNumberPolynomialAddBigNumber(X, FCoefficientB);
    BigNumberPolynomialNonNegativeModBigNumber(X, FFiniteFieldSize);  // X := A*X + B  mod

    BigNumberPolynomialGaloisSub(Y, Y, X, FFiniteFieldSize, FPrimitive);
    BigNumberPolynomialGaloisMod(Y, Y, FPrimitive, FFiniteFieldSize);

    Result := Y.IsZero;
  finally
    FEccPolynomialPool.Recycle(Y);
    FEccPolynomialPool.Recycle(X);
  end;
end;

class function TCnPolynomialEcc.IsPointOnCurve2(PX,
  PY: TCnBigNumberPolynomial; A, B, APrime: TCnBigNumber;
  APrimitive: TCnBigNumberPolynomial): Boolean;
var
  X, Y: TCnBigNumberPolynomial;
begin
  // 计算 (Y^2 - X^3 - A*X - B) mod primitive （多项式系数运算要 mod p）是否等于 0 多项式
  X := nil;
  Y := nil;

  try
    X := FEccPolynomialPool.Obtain;
    Y := FEccPolynomialPool.Obtain;

    BigNumberPolynomialCopy(Y, PY);
    BigNumberPolynomialGaloisMul(Y, Y, Y, APrime, APrimitive);

    BigNumberPolynomialCopy(X, PX);
    BigNumberPolynomialGaloisPower(X, X, 3, APrime, APrimitive);

    BigNumberPolynomialGaloisSub(Y, Y, X, APrime, APrimitive);  // Y := Y^2 - X^3 mod

    BigNumberPolynomialCopy(X, PX);
    BigNumberPolynomialMulBigNumber(X, A);
    BigNumberPolynomialAddBigNumber(X, B);
    BigNumberPolynomialNonNegativeModBigNumber(X, APrime); // X := A*X + B mod

    BigNumberPolynomialGaloisSub(Y, Y, X, APrime, APrimitive);
    BigNumberPolynomialGaloisMod(Y, Y, APrimitive, APrime);

    Result := Y.IsZero;
  finally
    FEccPolynomialPool.Recycle(Y);
    FEccPolynomialPool.Recycle(X);
  end;
end;

class function TCnPolynomialEcc.IsRationalPointOnCurve(PX,
  PY: TCnBigNumberRationalPolynomial; A, B, APrime: TCnBigNumber): Boolean;
var
  Y2, T1: TCnBigNumberPolynomial;
  RL, RR, T2: TCnBigNumberRationalPolynomial;
begin
  // 计算 PY^2 * (x^3 + Ax + B) 是否等于 PX^3 + A * PX + B，系数均 mod APrime
  Y2 := nil;
  T1 := nil;
  T2 := nil;
  RL := nil;
  RR := nil;

  try
    Y2 := FEccPolynomialPool.Obtain;
    Y2.SetCoefficents([B, A, 0, 1]);

    RL := FEccRationalPolynomialPool.Obtain;
    BigNumberRationalPolynomialGaloisMul(PY, PY, RL, APrime);
    BigNumberRationalPolynomialGaloisMul(RL, Y2, RL, APrime);  // 得到等号左边的值

    RR := FEccRationalPolynomialPool.Obtain;
    BigNumberRationalPolynomialGaloisMul(PX, PX, RR, APrime);
    BigNumberRationalPolynomialGaloisMul(RR, PX, RR, APrime);  // 得到 PX^3

    T1 := FEccPolynomialPool.Obtain;
    T1.SetCoefficents([A]);

    T2 := FEccRationalPolynomialPool.Obtain;
    BigNumberRationalPolynomialGaloisMul(PX, T1, T2, APrime);  // T2 得到 A * PX

    T1.SetCoefficents([B]);
    BigNumberRationalPolynomialGaloisAdd(T2, T1, T2, APrime);  // T2 得到 A * PX + B

    BigNumberRationalPolynomialGaloisAdd(T2, RR, RR, APrime);  // RR 得到 PX^3 + A * PX + B

    Result := BigNumberRationalPolynomialGaloisEqual(RL, RR, APrime);       // 比较是否相等
  finally
    FEccPolynomialPool.Recycle(Y2);
    FEccPolynomialPool.Recycle(T1);
    FEccRationalPolynomialPool.Recycle(T2);
    FEccRationalPolynomialPool.Recycle(RL);
    FEccRationalPolynomialPool.Recycle(RR);
  end;
end;

procedure TCnPolynomialEcc.MultiplePoint(K: TCnBigNumber;
  Point: TCnPolynomialEccPoint);
var
  I, C: Integer;
  E, R: TCnPolynomialEccPoint;
begin
  if K.IsZero then
  begin
    Point.SetZero;
    Exit;
  end
  else if K.IsNegative then
  begin
    K.Negate;
    PointInverse(Point);
  end;

  R := nil;
  E := nil;

  try
    R := TCnPolynomialEccPoint.Create;
    E := TCnPolynomialEccPoint.Create;

    R.SetZero;
    E.Assign(Point);

    C := BigNumberGetBitsCount(K);
    for I := 0 to C - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        PointAddPoint(R, E, R);

      if I < C - 1 then
        PointAddPoint(E, E, E);
    end;

    Point.Assign(R);
  finally
    R.Free;
    E.Free;
  end;
end;

procedure TCnPolynomialEcc.MultiplePoint(K: Int64;
  Point: TCnPolynomialEccPoint);
var
  BK: TCnBigNumber;
begin
  BK := FEccBigNumberPool.Obtain;
  try
    BK.SetInt64(K);
    MultiplePoint(BK, Point);
  finally
    FEccBigNumberPool.Recycle(BK);
  end;
end;

procedure TCnPolynomialEcc.PointAddPoint(P, Q, Sum: TCnPolynomialEccPoint);
var
  K, X, Y, T: TCnBigNumberPolynomial;
begin
  K := nil;
  X := nil;
  Y := nil;
  T := nil;

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
    else if BigNumberPolynomialEqual(P.X, Q.X) and BigNumberPolynomialEqual(P.Y, Q.Y) then
    begin
      // 俩加数是同一个点，切线斜率为两边求导，3 * X^2 + A / (2 * Y) 但如 Y = 0 则直接是无限远 0。
      X := FEccPolynomialPool.Obtain;
      Y := FEccPolynomialPool.Obtain;

      // X := 3 * P.X * P.X + FCoefficientA
      BigNumberPolynomialGaloisMul(X, P.X, P.X, FFiniteFieldSize, FPrimitive);
      BigNumberPolynomialGaloisMulWord(X, 3, FFiniteFieldSize);
      BigNumberPolynomialGaloisAddBigNumber(X, FCoefficientA, FFiniteFieldSize);

      // Y := 2 * P.Y;
      BigNumberPolynomialCopy(Y, P.Y);
      BigNumberPolynomialGaloisMulWord(Y, 2, FFiniteFieldSize);

      if Y.IsZero then
      begin
        Sum.X.SetZero;
        Sum.Y.SetZero;
      end;

      // Y := Y^-1
      T := FEccPolynomialPool.Obtain;
      BigNumberPolynomialCopy(T, Y);
      BigNumberPolynomialGaloisModularInverse(Y, T, FPrimitive, FFiniteFieldSize);

      // K := X * Y mod FFiniteFieldSize;
      K := FEccPolynomialPool.Obtain;
      BigNumberPolynomialGaloisMul(K, X, Y, FFiniteFieldSize, FPrimitive);
      // 得到切线斜率 K
    end
    else // 是不同点
    begin
      if BigNumberPolynomialEqual(P.X, Q.X) then // 如果 X 相等，要判断 Y 是不是互反，是则和为 0，不是则挂了
      begin
        T := FEccPolynomialPool.Obtain;
        BigNumberPolynomialGaloisAdd(T, P.Y, Q.Y, FFiniteFieldSize);
        if T.IsZero then
          Sum.SetZero
        else
          raise ECnEccException.CreateFmt('Can NOT Calucate %s,%s + %s,%s',
            [P.X.ToString, P.Y.ToString, Q.X.ToString, Q.Y.ToString]);

        Exit;
      end;

      // 到这里，X 确定不同，斜率 K := ((Q.Y - P.Y) / (Q.X - P.X)) mod p
      X := FEccPolynomialPool.Obtain;
      Y := FEccPolynomialPool.Obtain;
      K := FEccPolynomialPool.Obtain;

      BigNumberPolynomialGaloisSub(Y, Q.Y, P.Y, FFiniteFieldSize);
      BigNumberPolynomialGaloisSub(X, Q.X, P.X, FFiniteFieldSize);

      T := FEccPolynomialPool.Obtain;
      BigNumberPolynomialCopy(T, X);
      BigNumberPolynomialGaloisModularInverse(X, T, FPrimitive, FFiniteFieldSize);
      BigNumberPolynomialGaloisMul(K, Y, X, FFiniteFieldSize, FPrimitive); // 得到斜率
    end;

    //  X := K * K - P.X - Q.X;
    BigNumberPolynomialCopy(X, K);
    BigNumberPolynomialGaloisMul(X, X, K, FFiniteFieldSize, FPrimitive);
    BigNumberPolynomialGaloisSub(X, X, P.X, FFiniteFieldSize);
    BigNumberPolynomialGaloisSub(X, X, Q.X, FFiniteFieldSize);

    // Ysum = (K * (X1 - Xsum) - Y1) mod p
    BigNumberPolynomialGaloisSub(X, P.X, X, FFiniteFieldSize);
    BigNumberPolynomialGaloisMul(Y, K, X, FFiniteFieldSize, FPrimitive);
    BigNumberPolynomialGaloisSub(Y, Y, P.Y, FFiniteFieldSize);

    BigNumberPolynomialCopy(Sum.X, X);
    BigNumberPolynomialCopy(Sum.Y, Y);
  finally
    FEccPolynomialPool.Recycle(K);
    FEccPolynomialPool.Recycle(X);
    FEccPolynomialPool.Recycle(Y);
    FEccPolynomialPool.Recycle(T);
  end;
end;

procedure TCnPolynomialEcc.PointInverse(P: TCnPolynomialEccPoint);
var
  I: Integer;
begin
  for I := 0 to P.Y.MaxDegree do
    BigNumberSub(P.Y[I], FFiniteFieldSize, P.Y[I]);
end;

procedure TCnPolynomialEcc.PointSubPoint(P, Q,
  Diff: TCnPolynomialEccPoint);
var
  Inv: TCnPolynomialEccPoint;
begin
  Inv := TCnPolynomialEccPoint.Create;
  try
    Inv.Assign(Q);
    PointInverse(Inv);
    PointAddPoint(P, Inv, Diff);
  finally
    Inv.Free;
  end;
end;

class procedure TCnPolynomialEcc.RationalMultiplePoint(K: Integer; MX,
  MY: TCnBigNumberRationalPolynomial; A, B, APrime: TCnBigNumber;
  APrimitive: TCnBigNumberPolynomial);
var
  Neg: Boolean;
  FN, FNa1, FNa2, FNs1, FNs2, P1, P2, X1, Y2: TCnBigNumberPolynomial;
begin
  if K = 0 then
  begin
    MX.SetZero;
    MY.SetZero;
    Exit;
  end;

  Neg := K < 0;
  if Neg then
    K := -K;

  if K = 1 then // 没乘，原封不动返回 MX 和 MY，不做改动
  begin
//    MX.Nominator.SetCoefficents([0, 1]);
//    MX.Denominator.SetOne;
//
//    MY.Nominator.SetOne;
//    MY.Denominator.SetOne;
  end
  else
  begin
    FN := FEccPolynomialPool.Obtain;
    FNa1 := FEccPolynomialPool.Obtain;
    FNa2 := FEccPolynomialPool.Obtain;
    FNs1 := FEccPolynomialPool.Obtain;
    FNs2 := FEccPolynomialPool.Obtain;
    X1 := FEccPolynomialPool.Obtain;
    Y2 := FEccPolynomialPool.Obtain;
    P1 := FEccPolynomialPool.Obtain;
    P2 := FEccPolynomialPool.Obtain;

    try
      X1.SetCoefficents([0, 1]);
      Y2.SetCoefficents([B, A, 0, 1]);

      BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, K, FN, APrime);
      BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, K + 1, FNa1, APrime);
      BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, K + 2, FNa2, APrime);
      BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, K - 1, FNs1, APrime);
      BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, K - 2, FNs2, APrime);

      // 求 X 表达式
      if (K and 1) = 0 then // K 偶数时
      begin
        // 结果的 x 坐标为 (x*fn^2 * Y^2 - fn+1 * fn-1) / fn^2 * Y^2
        BigNumberPolynomialGaloisMul(MX.Denominator, FN, FN, APrime);
        BigNumberPolynomialGaloisMul(MX.Denominator, MX.Denominator, Y2, APrime);

        BigNumberPolynomialGaloisMul(P1, FNa1, FNs1, APrime); // P1 得到 fn+1 * fn-1
        BigNumberPolynomialGaloisMul(P2, FN, FN, APrime);
        BigNumberPolynomialGaloisMul(P2, P2, X1, APrime);     // P2 得到 x*fn^2
        BigNumberPolynomialGaloisMul(P2, P2, Y2, APrime);     // P2 得到 x*fn^2 * Y^2

        BigNumberPolynomialGaloisSub(MX.Nominator, P2, P1, APrime); // MX 计算完毕
      end
      else // K 奇数时
      begin
        // 结果的 x 坐标为 (x*fn^2 - Y^2 * fn+1 * fn-1) / fn^2
        BigNumberPolynomialGaloisMul(MX.Denominator, FN, FN, APrime);

        BigNumberPolynomialGaloisMul(P1, FNa1, FNs1, APrime); // P1 得到 fn+1 * fn-1
        BigNumberPolynomialGaloisMul(P1, P1, Y2, APrime);     // P1 得到 Y^2 * fn+1 * fn-1

        BigNumberPolynomialGaloisMul(P2, FN, FN, APrime);
        BigNumberPolynomialGaloisMul(P2, P2, X1, APrime);     // P2 得到 x*fn^2
        BigNumberPolynomialGaloisSub(MX.Nominator, P2, P1, APrime); // MX 计算完毕
      end;

      // 求 Y 表达式
      if K = 2 then // Y 的分子是 f2n，n 为 2 时不需递归，直接用 f4
      begin
        MY.Denominator.SetOne;
        BigNumberPolynomialCopy(MY.Nominator, FNa2);
      end
      else
      begin
        // 结果的 y 坐标分子为 fn+2 * fn-1^2 - fn-2 * fn+1 ^2
        BigNumberPolynomialGaloisMul(P1, FNs1, FNs1, APrime);
        BigNumberPolynomialGaloisMul(P1, P1, FNa2, APrime);
        BigNumberPolynomialGaloisMul(P2, FNa1, FNa1, APrime);
        BigNumberPolynomialGaloisMul(P2, P2, FNs2, APrime);

        BigNumberPolynomialGaloisSub(MY.Nominator, P1, P2, APrime); // MY 分子计算完毕
      end;

      BigNumberPolynomialGaloisPower(MY.Denominator, FN, 3, APrime);
      BigNumberPolynomialGaloisMulWord(MY.Denominator, 4, APrime);   // 奇数分母 4 * fn^3 计算完毕

      if (K and 1) = 0 then // 偶数分母还得乘以 y^4
      begin
        BigNumberPolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime);
        BigNumberPolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime);
      end;
    finally
      FEccPolynomialPool.Recycle(FN);
      FEccPolynomialPool.Recycle(FNa1);
      FEccPolynomialPool.Recycle(FNa2);
      FEccPolynomialPool.Recycle(FNs1);
      FEccPolynomialPool.Recycle(FNs2);
      FEccPolynomialPool.Recycle(X1);
      FEccPolynomialPool.Recycle(Y2);
      FEccPolynomialPool.Recycle(P1);
      FEccPolynomialPool.Recycle(P2);
    end;
  end;

  if Neg then
    MY.Neg;

  if APrimitive <> nil then
  begin
    BigNumberPolynomialGaloisMod(MX.Nominator, MX.Nominator, APrimitive, APrime);
    BigNumberPolynomialGaloisMod(MX.Denominator, MX.Denominator, APrimitive, APrime);
    BigNumberPolynomialGaloisMod(MY.Nominator, MY.Nominator, APrimitive, APrime);
    BigNumberPolynomialGaloisMod(MY.Denominator, MY.Denominator, APrimitive, APrime);
  end;
end;

class procedure TCnPolynomialEcc.RationalPointAddPoint(PX, PY, QX, QY, SX,
  SY: TCnBigNumberRationalPolynomial; A, B, APrime: TCnBigNumber;
  APrimitive: TCnBigNumberPolynomial);
var
  R, T1, T2: TCnBigNumberRationalPolynomial;
  Y2, C: TCnBigNumberPolynomial;
begin
  // 点 (PX, PY * y) + (QX, QY * y) = (SX, SY * y)
  // 先求斜率 R = y * (QY - PY) / (QX - PX) 或 (3PX^2 + A) / 2PY * y

  if PX.IsZero and PY.IsZero then
  begin
    BigNumberRationalPolynomialCopy(SX, QX);
    BigNumberRationalPolynomialCopy(SY, QY);
    Exit;
  end
  else if QX.IsZero and QY.IsZero then
  begin
    BigNumberRationalPolynomialCopy(SX, PX);
    BigNumberRationalPolynomialCopy(SY, PY);
    Exit;
  end;

  R := nil;
  T1 := nil;
  T2 := nil;

  Y2 := nil;
  C := nil;

  try
    R := FEccRationalPolynomialPool.Obtain;
    T1 := FEccRationalPolynomialPool.Obtain;
    T2 := FEccRationalPolynomialPool.Obtain;

    Y2 := FEccPolynomialPool.Obtain;
    C := FEccPolynomialPool.Obtain;
    Y2.SetCoefficents([B, A, 0, 1]);

    if BigNumberRationalPolynomialGaloisEqual(PX, QX, APrime, APrimitive) then // 不能直接判断相等，得互乘后各自针对本原多项式求余后再判断相等
    begin
      // X 相等，判断 Y 是否相等，不等则假设它们相反，返回 0
      // TODO: 判断 PY QY 是否相反
      if not BigNumberRationalPolynomialGaloisEqual(PY, QY, APrime, APrimitive) then
      begin
        SX.SetZero;
        SY.SetZero;
        Exit;
      end;

      // X Y 都相等，求导
      C.SetCoefficents([3]);

      BigNumberRationalPolynomialGaloisMul(PX, PX, T1, APrime);
      BigNumberRationalPolynomialGaloisMul(T1, C, T1, APrime);  // T1 得到 3PX^2

      C.SetCoefficents([A]);
      BigNumberRationalPolynomialGaloisAdd(T1, C, T1, APrime);  // T1 得到 3PX^2 + A

      C.SetCoefficents([2]);
      BigNumberRationalPolynomialGaloisMul(PY, C, T2, APrime);  // T2 得到 2PY，实际上还要乘以一个 y

      BigNumberRationalPolynomialGaloisDiv(T1, T2, R, APrime);  // 得到斜率 R，但真实的斜率分母实际上还要乘以一个 y，后面补上

      // SX = 真实斜率^2 - PX - QX = R^2 / (x^3+Ax+B) - PX - QX
      // 真实斜率的平方 = R^2 / y^2，分母可替换成 x^3+Ax+B
      BigNumberRationalPolynomialGaloisMul(R, R, SX, APrime);
      BigNumberRationalPolynomialGaloisDiv(SX, Y2, SX, APrime);
      BigNumberRationalPolynomialGaloisSub(SX, PX, SX, APrime);
      BigNumberRationalPolynomialGaloisSub(SX, QX, SX, APrime);

      if APrimitive <> nil then
      begin
        BigNumberPolynomialGaloisMod(SX.Nominator, SX.Nominator, APrimitive, APrime);
        BigNumberPolynomialGaloisMod(SX.Denominator, SX.Denominator, APrimitive, APrime);
      end;

      // SY * y = 真实斜率 * (PX - SX) - PY * y
      // SY = (R/y * (PX - SX) - PY * y) / y = R * (PX - SX)/ y^2 - PY
      BigNumberRationalPolynomialGaloisSub(PX, SX, SY, APrime);
      BigNumberRationalPolynomialGaloisMul(SY, R, SY, APrime);
      BigNumberRationalPolynomialGaloisDiv(SY, Y2, SY, APrime);
      BigNumberRationalPolynomialGaloisSub(SY, PY, SY, APrime);

      if APrimitive <> nil then
      begin
        BigNumberPolynomialGaloisMod(SY.Nominator, SY.Nominator, APrimitive, APrime);
        BigNumberPolynomialGaloisMod(SY.Denominator, SY.Denominator, APrimitive, APrime);
      end;
    end
    else
    begin
      // 不相等，减，真实斜率等于 y * (QY - PY) / (QX - PX)
      BigNumberRationalPolynomialGaloisSub(QY, PY, T1, APrime);
      BigNumberRationalPolynomialGaloisSub(QX, PX, T2, APrime);
      BigNumberRationalPolynomialGaloisDiv(T1, T2, R, APrime);

      // R 得到斜率了，但真实的斜率分子实际上还要乘以一个 y，后面补上
      // SX = R^2 * (x^3+Ax+B) - PX - QX
      BigNumberRationalPolynomialGaloisMul(R, R, SX, APrime);
      BigNumberRationalPolynomialGaloisMul(SX, Y2, SX, APrime);
      BigNumberRationalPolynomialGaloisSub(SX, PX, SX, APrime);
      BigNumberRationalPolynomialGaloisSub(SX, QX, SX, APrime); // 这步溢出了？

      if APrimitive <> nil then
      begin
        BigNumberPolynomialGaloisMod(SX.Nominator, SX.Nominator, APrimitive, APrime);
        BigNumberPolynomialGaloisMod(SX.Denominator, SX.Denominator, APrimitive, APrime);
      end;

      // SY * y = R * y * (PX - SX) - PY * y 都除以 y 得 SY = R * (PX - SX) - PY
      BigNumberRationalPolynomialGaloisSub(PX, SX, SY, APrime);
      BigNumberRationalPolynomialGaloisMul(SY, R, SY, APrime);
      BigNumberRationalPolynomialGaloisSub(SY, PY, SY, APrime);

      if APrimitive <> nil then
      begin
        BigNumberPolynomialGaloisMod(SY.Nominator, SY.Nominator, APrimitive, APrime);
        BigNumberPolynomialGaloisMod(SY.Denominator, SY.Denominator, APrimitive, APrime);
      end;
    end;
  finally
    FEccPolynomialPool.Recycle(Y2);
    FEccPolynomialPool.Recycle(C);

    FEccRationalPolynomialPool.Recycle(T2);
    FEccRationalPolynomialPool.Recycle(T1);
    FEccRationalPolynomialPool.Recycle(R);
  end;
end;

procedure TCnPolynomialEcc.SetPrimitive(
  const Value: TCnBigNumberPolynomial);
begin
  if Value <> nil then
  begin
    if Value.MaxDegree <> FExtension then
      raise ECnEccException.Create('Primitive Polynomial Max Degree must be Field Extension.');
    BigNumberPolynomialCopy(FPrimitive, Value);
  end;
end;

procedure RationalMultiplePointX(Res, PX: TCnBigNumberRationalPolynomial; K: Integer;
  A, B, APrime: TCnBigNumber; DivisionPolynomialList: TObjectList; APrimitive: TCnBigNumberPolynomial);
var
  MX: TCnBigNumberRationalPolynomial;
  FN, FNa1, FNs1, P1, P2, X1, Y2: TCnBigNumberPolynomial;
begin
  if K = 0 then
  begin
    Res.SetZero;
    Exit;
  end;

  if K < 0 then
    K := -K;

  MX := FEccRationalPolynomialPool.Obtain;
  if K = 1 then // 没乘，原封不动返回 x 和 1
  begin
    MX.Nominator.SetCoefficents([0, 1]);
    MX.Denominator.SetOne;
  end
  else
  begin
    X1 := FEccPolynomialPool.Obtain;
    Y2 := FEccPolynomialPool.Obtain;
    P1 := FEccPolynomialPool.Obtain;
    P2 := FEccPolynomialPool.Obtain;

    try
      X1.SetCoefficents([0, 1]);
      Y2.SetCoefficents([B, A, 0, 1]);

      FN := TCnBigNumberPolynomial(DivisionPolynomialList[K]);
      FNa1 := TCnBigNumberPolynomial(DivisionPolynomialList[K + 1]);
      FNs1 := TCnBigNumberPolynomial(DivisionPolynomialList[K - 1]);

      // 求 X 表达式
      if (K and 1) = 0 then // K 偶数时
      begin
        // 结果的 x 坐标为 (x*fn^2 * Y^2 - fn+1 * fn-1) / fn^2 * Y^2
        BigNumberPolynomialGaloisMul(MX.Denominator, FN, FN, APrime, APrimitive);
        BigNumberPolynomialGaloisMul(MX.Denominator, MX.Denominator, Y2, APrime, APrimitive);

        BigNumberPolynomialGaloisMul(P1, FNa1, FNs1, APrime, APrimitive); // P1 得到 fn+1 * fn-1
        BigNumberPolynomialGaloisMul(P2, FN, FN, APrime, APrimitive);
        BigNumberPolynomialGaloisMul(P2, P2, X1, APrime, APrimitive);     // P2 得到 x*fn^2
        BigNumberPolynomialGaloisMul(P2, P2, Y2, APrime, APrimitive);     // P2 得到 x*fn^2 * Y^2

        BigNumberPolynomialGaloisSub(MX.Nominator, P2, P1, APrime); // MX 计算完毕
      end
      else // K 奇数时
      begin
        // 结果的 x 坐标为 (x*fn^2 - Y^2 * fn+1 * fn-1) / fn^2
        BigNumberPolynomialGaloisMul(MX.Denominator, FN, FN, APrime, APrimitive);

        BigNumberPolynomialGaloisMul(P1, FNa1, FNs1, APrime, APrimitive); // P1 得到 fn+1 * fn-1
        BigNumberPolynomialGaloisMul(P1, P1, Y2, APrime, APrimitive);     // P1 得到 Y^2 * fn+1 * fn-1

        BigNumberPolynomialGaloisMul(P2, FN, FN, APrime, APrimitive);
        BigNumberPolynomialGaloisMul(P2, P2, X1, APrime, APrimitive);     // P2 得到 x*fn^2
        BigNumberPolynomialGaloisSub(MX.Nominator, P2, P1, APrime, APrimitive); // MX 计算完毕
      end;
    finally
      FEccPolynomialPool.Recycle(X1);
      FEccPolynomialPool.Recycle(Y2);
      FEccPolynomialPool.Recycle(P1);
      FEccPolynomialPool.Recycle(P2);
    end;

    if APrimitive <> nil then
    begin
      BigNumberPolynomialGaloisMod(MX.Nominator, MX.Nominator, APrimitive, APrime);
      BigNumberPolynomialGaloisMod(MX.Denominator, MX.Denominator, APrimitive, APrime);
    end;
  end;

  BigNumberRationalPolynomialGaloisCompose(Res, MX, PX, APrime, APrimitive);
  FEccRationalPolynomialPool.Recycle(MX);

  if APrimitive <> nil then
  begin
    BigNumberPolynomialGaloisMod(Res.Nominator, Res.Nominator, APrimitive, APrime);
    BigNumberPolynomialGaloisMod(Res.Denominator, Res.Denominator, APrimitive, APrime);
  end;
end;

procedure RationalMultiplePointY(Res, PX, PY: TCnBigNumberRationalPolynomial; K: Integer;
  A, B, APrime: TCnBigNumber; DivisionPolynomialList: TObjectList; APrimitive: TCnBigNumberPolynomial = nil);
var
  Neg: Boolean;
  MY: TCnBigNumberRationalPolynomial;
  FN, FNa1, FNa2, FNs1, FNs2, P1, P2, X1, Y2: TCnBigNumberPolynomial;
begin
  if K = 0 then
  begin
    Res.SetZero;
    Exit;
  end;

  Neg := K < 0;
  if K < 0 then
    K := -K;

  MY := FEccRationalPolynomialPool.Obtain;
  if K = 1 then // 没乘，原封不动返回 x 和 1
  begin
    MY.Nominator.SetOne;
    MY.Denominator.SetOne;
  end
  else
  begin
    X1 := FEccPolynomialPool.Obtain;
    Y2 := FEccPolynomialPool.Obtain;
    P1 := FEccPolynomialPool.Obtain;
    P2 := FEccPolynomialPool.Obtain;

    try
      X1.SetCoefficents([0, 1]);
      Y2.SetCoefficents([B, A, 0, 1]);

      FN := TCnBigNumberPolynomial(DivisionPolynomialList[K]);
      FNa1 := TCnBigNumberPolynomial(DivisionPolynomialList[K + 1]);
      FNa2 := TCnBigNumberPolynomial(DivisionPolynomialList[K + 2]);
      FNs1 := TCnBigNumberPolynomial(DivisionPolynomialList[K - 1]);
      FNs2 := TCnBigNumberPolynomial(DivisionPolynomialList[K - 2]);

      if K = 2 then // Y 的分子是 f2n，n 为 2 时不需递归，直接用 f4
      begin
        MY.Denominator.SetOne;
        BigNumberPolynomialCopy(MY.Nominator, FNa2);
      end
      else
      begin
        // 结果的 y 坐标分子为 fn+2 * fn-1^2 - fn-2 * fn+1 ^2
        BigNumberPolynomialGaloisMul(P1, FNs1, FNs1, APrime, APrimitive);
        BigNumberPolynomialGaloisMul(P1, P1, FNa2, APrime, APrimitive);
        BigNumberPolynomialGaloisMul(P2, FNa1, FNa1, APrime, APrimitive);
        BigNumberPolynomialGaloisMul(P2, P2, FNs2, APrime, APrimitive);

        BigNumberPolynomialGaloisSub(MY.Nominator, P1, P2, APrime, APrimitive); // MY 分子计算完毕
      end;

      BigNumberPolynomialGaloisPower(MY.Denominator, FN, 3, APrime, APrimitive);
      BigNumberPolynomialGaloisMulWord(MY.Denominator, 4, APrime);   // 奇数分母 4 * fn^3 计算完毕

      if (K and 1) = 0 then // 偶数分母还得乘以 y^4
      begin
        BigNumberPolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime, APrimitive);
        BigNumberPolynomialGaloisMul(MY.Denominator, Y2, MY.Denominator, APrime, APrimitive);
      end;
    finally
      FEccPolynomialPool.Recycle(X1);
      FEccPolynomialPool.Recycle(Y2);
      FEccPolynomialPool.Recycle(P1);
      FEccPolynomialPool.Recycle(P2);
    end;
  end;

  if Neg then
    MY.Neg;

  if APrimitive <> nil then
  begin
    BigNumberPolynomialGaloisMod(MY.Nominator, MY.Nominator, APrimitive, APrime);
    BigNumberPolynomialGaloisMod(MY.Denominator, MY.Denominator, APrimitive, APrime);
  end;

  BigNumberRationalPolynomialGaloisCompose(Res, MY, PX, APrime, APrimitive);
  BigNumberRationalPolynomialGaloisMul(Res, PY, Res, APrime);
  FEccRationalPolynomialPool.Recycle(MY);

  if APrimitive <> nil then
  begin
    BigNumberPolynomialGaloisMod(Res.Nominator, Res.Nominator, APrimitive, APrime);
    BigNumberPolynomialGaloisMod(Res.Denominator, Res.Denominator, APrimitive, APrime);
  end;
end;

function CnEccSchoof(Res, A, B, Q: TCnBigNumber): Boolean;
var
  Pa, Ta: TCnInt64List;
  QMul, QMax, BQ: TCnBigNumber;
  L, K: Int64;
  I, J: Integer;
  G, Y2, P1, P2, LDP: TCnBigNumberPolynomial;
  Pi2PX, Pi2PY, PiPX, PiPY, KPX, KPY, LSX, LSY, RSX, RSY, TSX, TSY: TCnBigNumberRationalPolynomial;
  DPs: TObjectList;
begin
  // 用 Schoof 算法求椭圆曲线 y^2 = x^3 + Ax + B 在素域 Fq 上的点总数
  // 先建个 List，存所需的 2 ~ lmax 的素数，其中 3 * ... * lmax 刚好 > 4 倍根号 q
  // 求 x^q -x 与 x^3 + Ax + B 的公因式，如果是 1 则 t2 = 1，否则 t2 = 0，
  // 这里 t2 是 List 中针对素数 2 的元素，并非下标，后面同

  Result := False;
  if Q.IsZero or Q.IsNegative then
    Exit;

  Pa := nil;
  Ta := nil;
  DPs := nil;
  Pi2PX := nil;
  Pi2PY := nil;
  PiPX := nil;
  PiPY := nil;
  KPX := nil;
  KPY := nil;
  LSX := nil;
  LSY := nil;
  RSX := nil;
  RSY := nil;
  TSX := nil;
  TSY := nil;

  Y2 := nil;
  P1 := nil;
  P2 := nil;

  G := nil;

  QMax := nil;
  QMul := nil;
  BQ := nil;

  try
    Pa := TCnInt64List.Create;
    Ta := TCnInt64List.Create;

    Y2 := FEccPolynomialPool.Obtain;
    P1 := FEccPolynomialPool.Obtain;
    P2 := FEccPolynomialPool.Obtain;

    G := FEccPolynomialPool.Obtain;

    QMax := FEccBigNumberPool.Obtain;
    QMul := FEccBigNumberPool.Obtain;
    BQ := FEccBigNumberPool.Obtain;

    if not BigNumberSqrt(QMax, Q) then
      Exit;

    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);
    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    Pi2PX := FEccRationalPolynomialPool.Obtain;
    Pi2PY := FEccRationalPolynomialPool.Obtain;
    PiPX := FEccRationalPolynomialPool.Obtain;
    PiPY := FEccRationalPolynomialPool.Obtain;
    KPX := FEccRationalPolynomialPool.Obtain;
    KPY := FEccRationalPolynomialPool.Obtain;
    LSX := FEccRationalPolynomialPool.Obtain;
    LSY := FEccRationalPolynomialPool.Obtain;
    RSX := FEccRationalPolynomialPool.Obtain;
    RSY := FEccRationalPolynomialPool.Obtain;
    TSX := FEccRationalPolynomialPool.Obtain;
    TSY := FEccRationalPolynomialPool.Obtain;

    while (BigNumberCompare(QMul, QMax) <= 0) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      BigNumberMulWord(QMul, CN_PRIME_NUMBERS_SQRT_UINT32[I]);
      Pa.Add(CN_PRIME_NUMBERS_SQRT_UINT32[I]);
      Ta.Add(0);
      Inc(I);
    end;

    if I > High(CN_PRIME_NUMBERS_SQRT_UINT32) then
      raise ECnEccException.Create('Prime Number is Too Large.');

    Y2.SetCoefficents([B, A, 0, 1]);

    // Ta 与 Pa 数组已准备好，先处理 t = 2 的情况
    P1.SetCoefficents([0, 1]); // P1 := X
    BigNumberPolynomialGaloisPower(P1, P1, Q, Q, Y2); // X^q 先 mod Y^2

    P2.SetCoefficents([0, 1]); // P2 := X
    BigNumberPolynomialGaloisSub(P1, P1, P2, Q); // P1 := (X^q mod Y^2) - x

    // 求最大公约式
    BigNumberPolynomialGaloisGreatestCommonDivisor(G, P1, Y2, Q);

    if G.IsOne then
      Ta[0] := 1
    else
      Ta[0] := 0;   // 求得 T2。理解了并且基本算对了

    // 提前算好最大素数 + 2 阶的可除多项式们以及准备好 Y^2
    DPs := TObjectList.Create(True);
    CnGenerateGaloisDivisionPolynomials(A, B, Q, Pa[Pa.Count - 1] + 2, DPs);

    for I := 1 to Ta.Count - 1 do  // 针对每一个 L，都满足 π^2(P) + K * (P) = J * π^(P) mod L阶可除多项式
    begin
      L := Pa[I];
      K := BigNumberModWord(Q, L);

      // 先得到 L 阶可除多项式，作为后续计算的模多项式
      LDP := TCnBigNumberPolynomial(DPs[L]);

      Pi2PX.SetOne;                           // 原始点
      Pi2PX.Nominator.SetCoefficents([0, 1]); // x
      Pi2PY.Setone;                           // 1 * y

      // 算得 π^2 的 X 坐标在 LDP 环内的表达分式，也就是 Q*Q 个 x 相乘再 mod LDP
      BigNumberPolynomialGaloisPower(Pi2PX.Nominator, Pi2PX.Nominator, Q, Q, LDP);
      BigNumberPolynomialGaloisPower(Pi2PX.Nominator, Pi2PX.Nominator, Q, Q, LDP);  // 直接 Q*Q 容易溢出，分步算

      // 算得 π^2 的 Y 坐标在 LDP 环内的表达分式，Q*Q 个 y 相乘等于 y * [(Q*Q shr 1) 个 y^2 相乘]，而 y^2 可替换成 x^3+Ax+B
      BigNumberMul(BQ, Q, Q);
      BigNumberShiftRightOne(BQ, BQ);
      BigNumberPolynomialGaloisPower(Pi2PY.Nominator, Y2, BQ, Q, LDP);

      KPX.SetOne;                             // 原始点
      KPX.Nominator.SetCoefficents([0, 1]);   // x
      KPY.SetOne;                             // 1 * y

      // 算得 K * P 的 X Y 坐标
      TCnPolynomialEcc.RationalMultiplePoint(K, KPX, KPY, A, B, Q, LDP);

      PiPX.SetOne;                            // 原始点
      PiPX.Nominator.SetCoefficents([0, 1]);  // x
      PiPY.Setone;                            // 1 * y

      // 求 π^2(P) + K * (P) 的和点 SX SY
      TCnPolynomialEcc.RationalPointAddPoint(Pi2PX, Pi2PY, KPX, KPY, LSX, LSY, A, B, Q, LDP);

      if LSX.IsZero and LSY.IsZero then  // 如果和点为 0，则表示 t * π结果等于 0，t 自然等于 0
      begin
        Ta[I] := 0;
      end
      else
      begin
        // 算得 π的 X 坐标在 LDP 环内的表达分式，也就是 Q 个 x 相乘再 mod LDP
        BigNumberPolynomialGaloisPower(PiPX.Nominator, PiPX.Nominator, Q, Q, LDP);

        // 算得 π的 Y 坐标在 LDP 环内的表达分式，Q 个 y 相乘等于 y * [(Q shr 1) 个 y^2 相乘]，而 y^2 可替换成 x^3+Ax+B
        BigNumberShiftRightOne(BQ, Q);
        BigNumberPolynomialGaloisPower(PiPY.Nominator, Y2, BQ, Q, LDP);

        BigNumberRationalPolynomialCopy(RSX, PiPX);
        BigNumberRationalPolynomialCopy(RSY, PiPY);

        for J := 1 to (L + 1) shr 1 do
        begin
          // 本来可以直接用可除多项式计算 RSX := J * (PiPX, PiPY) 的 X，但似乎还会比点加慢，还是用点加
          // RationalMultiplePointX(RSX, PiPX, J, A, B, Q, DPs, LDP);

          if BigNumberRationalPolynomialGaloisEqual(LSX, RSX, Q, LDP) then
          begin
            // 本来可以直接用可除多项式计算 RSY := J * (PiPX, PiPY) 的 Y，但似乎还会比点加慢，还是用点加
            // RationalMultiplePointY(RSY, PiPX, PiPY, J, A, B, Q, DPs, LDP);

            if BigNumberRationalPolynomialGaloisEqual(LSY, RSY, Q, LDP) then
              Ta[I] := J
            else
              Ta[I] := L - J;
            Break;
          end;

          TCnPolynomialEcc.RationalPointAddPoint(RSX, RSY, PiPX, PiPY, TSX, TSY, A, B, Q, LDP);
          BigNumberRationalPolynomialCopy(RSX, TSX);
          BigNumberRationalPolynomialCopy(RSY, TSY);
        end;
      end;
    end;

    // 求出各个余数后，用中国剩余定理求最终解
    BigNumberChineseRemainderTheorem(Res, Ta, Pa);

    // 注意求出的 T 必须满足 Hasse 定理：T 的绝对值 <= 2 * 根号 Q，如超出范围，还得修正
    BigNumberSqrt(QMax, Q);
    QMax.AddWord(1);
    QMax.ShiftLeftOne;     // QMax 复用，是 2 根号 Q + 1，其绝对值必须比 Res 大

    if BigNumberUnsignedCompare(Res, QMax) >= 0 then
    begin
      // 中国剩余定理求出的一般是最小正数，需要减去全体 Pa 的乘积
      QMul.SetOne;
      for J := 0 to Pa.Count - 1 do
      begin
        BQ.SetInt64(Pa[J]);
        BigNumberMul(QMul, QMul, BQ);
      end;

      if Res.IsNegative then
        BigNumberAdd(Res, Res, QMul)
      else
        BigNumberSub(Res, Res, QMul);
    end;

    Res.Negate;
    BigNumberAdd(Res, Res, Q);
    Res.AddWord(1); // Q + 1 - L
    Result := True;
  finally
    FEccPolynomialPool.Recycle(Y2);
    FEccPolynomialPool.Recycle(P1);
    FEccPolynomialPool.Recycle(P2);

    FEccPolynomialPool.Recycle(G);

    FEccBigNumberPool.Recycle(QMax);
    FEccBigNumberPool.Recycle(QMul);
    FEccBigNumberPool.Recycle(BQ);

    FEccRationalPolynomialPool.Recycle(Pi2PX);
    FEccRationalPolynomialPool.Recycle(Pi2PY);
    FEccRationalPolynomialPool.Recycle(PiPX);
    FEccRationalPolynomialPool.Recycle(PiPY);
    FEccRationalPolynomialPool.Recycle(KPX);
    FEccRationalPolynomialPool.Recycle(KPY);
    FEccRationalPolynomialPool.Recycle(LSX);
    FEccRationalPolynomialPool.Recycle(LSY);
    FEccRationalPolynomialPool.Recycle(RSX);
    FEccRationalPolynomialPool.Recycle(RSY);
    FEccRationalPolynomialPool.Recycle(TSX);
    FEccRationalPolynomialPool.Recycle(TSY);

    DPs.Free;
    Pa.Free;
    Ta.Free;
  end;
end;

function CnEccSchoof2(Res, A, B, Q: TCnBigNumber): Boolean;
var
  Pa, Ta: TCnInt64List;
  QMul, QMax, BQ: TCnBigNumber;
  L, W, K: Int64;
  I, J: Integer;
  G, Y2, P1, P2, LDP: TCnBigNumberPolynomial;
  Pi2PX, Pi2PY, PiPX, PiPY, KPX, KPY, LSX, LSY, RSX, RSY, TSX, TSY, WPiPX, WPiPY: TCnBigNumberRationalPolynomial;
  DPs: TObjectList;
begin
  // 用 Schoof 算法求椭圆曲线 y^2 = x^3 + Ax + B 在素域 Fq 上的点总数
  // 先建个 List，存所需的 2 ~ lmax 的素数，其中 3 * ... * lmax 刚好 > 4 倍根号 q
  // 求 x^q -x 与 x^3 + Ax + B 的公因式，如果是 1 则 t2 = 1，否则 t2 = 0，
  // 这里 t2 是 List 中针对素数 2 的元素，并非下标，后面同

  Result := False;
  if Q.IsZero or Q.IsNegative then
    Exit;

  Pa := nil;
  Ta := nil;
  DPs := nil;
  Pi2PX := nil;
  Pi2PY := nil;
  PiPX := nil;
  PiPY := nil;
  KPX := nil;
  KPY := nil;
  LSX := nil;
  LSY := nil;
  RSX := nil;
  RSY := nil;
  TSX := nil;
  TSY := nil;
  WPiPX := nil;
  WPiPY := nil;

  Y2 := nil;
  P1 := nil;
  P2 := nil;

  G := nil;

  QMax := nil;
  QMul := nil;
  BQ := nil;

  try
    Pa := TCnInt64List.Create;
    Ta := TCnInt64List.Create;

    Y2 := FEccPolynomialPool.Obtain;
    P1 := FEccPolynomialPool.Obtain;
    P2 := FEccPolynomialPool.Obtain;

    G := FEccPolynomialPool.Obtain;

    QMax := FEccBigNumberPool.Obtain;
    QMul := FEccBigNumberPool.Obtain;
    BQ := FEccBigNumberPool.Obtain;

    if not BigNumberSqrt(QMax, Q) then
      Exit;

    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);
    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    Pi2PX := FEccRationalPolynomialPool.Obtain;
    Pi2PY := FEccRationalPolynomialPool.Obtain;
    PiPX := FEccRationalPolynomialPool.Obtain;
    PiPY := FEccRationalPolynomialPool.Obtain;
    KPX := FEccRationalPolynomialPool.Obtain;
    KPY := FEccRationalPolynomialPool.Obtain;
    LSX := FEccRationalPolynomialPool.Obtain;
    LSY := FEccRationalPolynomialPool.Obtain;
    RSX := FEccRationalPolynomialPool.Obtain;
    RSY := FEccRationalPolynomialPool.Obtain;
    TSX := FEccRationalPolynomialPool.Obtain;
    TSY := FEccRationalPolynomialPool.Obtain;
    WPiPX := FEccRationalPolynomialPool.Obtain;
    WPiPY := FEccRationalPolynomialPool.Obtain;

    while (BigNumberCompare(QMul, QMax) <= 0) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      BigNumberMulWord(QMul, CN_PRIME_NUMBERS_SQRT_UINT32[I]);
      Pa.Add(CN_PRIME_NUMBERS_SQRT_UINT32[I]);
      Ta.Add(0);
      Inc(I);
    end;

    if I > High(CN_PRIME_NUMBERS_SQRT_UINT32) then
      raise ECnEccException.Create('Prime Number is Too Large.');

    Y2.SetCoefficents([B, A, 0, 1]);

    // Ta 与 Pa 数组已准备好，先处理 t = 2 的情况
    P1.SetCoefficents([0, 1]); // P1 := X
    BigNumberPolynomialGaloisPower(P1, P1, Q, Q, Y2); // X^q 先 mod Y^2

    P2.SetCoefficents([0, 1]); // P2 := X
    BigNumberPolynomialGaloisSub(P1, P1, P2, Q); // P1 := (X^q mod Y^2) - x

    // 求最大公约式
    BigNumberPolynomialGaloisGreatestCommonDivisor(G, P1, Y2, Q);

    if G.IsOne then
      Ta[0] := 1
    else
      Ta[0] := 0;   // 求得 T2。理解了并且基本算对了

    // 提前算好最大素数 + 2 阶的可除多项式们以及准备好 Y^2
    DPs := TObjectList.Create(True);
    CnGenerateGaloisDivisionPolynomials(A, B, Q, Pa[Pa.Count - 1] + 2, DPs);

    for I := 1 to Ta.Count - 1 do  // 针对每一个 L，都满足 π^2(P) + K * (P) = J * π^(P) mod L阶可除多项式
    begin
      L := Pa[I];
      K := BigNumberModWord(Q, L);

      // 先得到 L 阶可除多项式，作为后续计算的模多项式
      LDP := TCnBigNumberPolynomial(DPs[L]);

      Pi2PX.SetOne;                           // 原始点
      Pi2PX.Nominator.SetCoefficents([0, 1]); // x
      Pi2PY.Setone;                           // 1 * y

      // 算得 π^2 的 X 坐标在 LDP 环内的表达分式，也就是 Q*Q 个 x 相乘再 mod LDP
      BigNumberPolynomialGaloisPower(Pi2PX.Nominator, Pi2PX.Nominator, Q, Q, LDP);
      BigNumberPolynomialGaloisPower(Pi2PX.Nominator, Pi2PX.Nominator, Q, Q, LDP);  // 直接 Q*Q 容易溢出，分步算

      // 算得 π^2 的 Y 坐标在 LDP 环内的表达分式，Q*Q 个 y 相乘等于 y * [(Q*Q shr 1) 个 y^2 相乘]，而 y^2 可替换成 x^3+Ax+B
      BigNumberMul(BQ, Q, Q);
      BigNumberShiftRightOne(BQ, BQ);
      BigNumberPolynomialGaloisPower(Pi2PY.Nominator, Y2, BQ, Q, LDP);

      KPX.SetOne;                             // 原始点
      KPX.Nominator.SetCoefficents([0, 1]);   // x
      KPY.SetOne;                             // 1 * y

      // 算得 K * P 的 X Y 坐标，这里 K 相当于 Wikepedia 步骤中的 q 杆
      TCnPolynomialEcc.RationalMultiplePoint(K, KPX, KPY, A, B, Q, LDP);

      // 此处 Wikipedia 上增加了 KPX 与 Pi2PX 是否相同的判断，相同则可以用二次剩余判断
      if BigNumberRationalPolynomialGaloisEqual(KPX, Pi2PX, Q) then
      begin
        // 两点的 X 坐标相同，求平方剩余 w^2 = K mod L
        W := CnInt64SquareRoot(K, L);
        if W = 0 then // 不存在二次剩余，t 为 0
        begin
          Ta[I] := 0;
          Continue;
        end;

        // 存在二次剩余，t 为正负 2W，判断其符号，要计算 W *  π的 X 和 Y 坐标

        PiPX.SetOne;                            // 原始点
        PiPX.Nominator.SetCoefficents([0, 1]);  // x
        PiPY.Setone;                            // 1 * y

        // 算得 π的 X 坐标在 LDP 环内的表达分式，也就是 Q 个 x 相乘再 mod LDP
        BigNumberPolynomialGaloisPower(PiPX.Nominator, PiPX.Nominator, Q, Q, LDP);

        // 算得 π的 Y 坐标在 LDP 环内的表达分式，Q 个 y 相乘等于 y * [(Q shr 1) 个 y^2 相乘]，而 y^2 可替换成 x^3+Ax+B
        BigNumberShiftRightOne(BQ, Q);
        BigNumberPolynomialGaloisPower(PiPY.Nominator, Y2, BQ, Q, LDP);

        // 复制过去后乘 W 倍点
        BigNumberRationalPolynomialCopy(WPiPX, PiPX);
        BigNumberRationalPolynomialCopy(WPiPY, PiPY);

        TCnPolynomialEcc.RationalMultiplePoint(W, WPiPX, WPiPY, A, B, Q, LDP);

        if BigNumberRationalPolynomialGaloisEqual(WPiPX, Pi2PX, Q, LDP) then
        begin
          if BigNumberRationalPolynomialGaloisEqual(WPiPY, Pi2PY, Q, LDP) then
           Ta[I] := 2 * W
          else
          begin
           BigNumberRationalPolynomialGaloisNegate(WPiPY, Q);
           if BigNumberRationalPolynomialGaloisEqual(WPiPY, Pi2PY, Q, LDP) then
             Ta[I] := L - 2 * W
           else
             Ta[I] := 0;
          end;
        end
        else
          Ta[I] := 0;
      end
      else
      begin
        // 不同，还是要累加
        // 求 π^2(P) + K * (P) 的和点 SX SY
        TCnPolynomialEcc.RationalPointAddPoint(Pi2PX, Pi2PY, KPX, KPY, LSX, LSY, A, B, Q, LDP);

        PiPX.SetOne;                            // 原始点
        PiPX.Nominator.SetCoefficents([0, 1]);  // x
        PiPY.Setone;                            // 1 * y

        // 算得 π的 X 坐标在 LDP 环内的表达分式，也就是 Q 个 x 相乘再 mod LDP
        BigNumberPolynomialGaloisPower(PiPX.Nominator, PiPX.Nominator, Q, Q, LDP);

        // 算得 π的 Y 坐标在 LDP 环内的表达分式，Q 个 y 相乘等于 y * [(Q shr 1) 个 y^2 相乘]，而 y^2 可替换成 x^3+Ax+B
        BigNumberShiftRightOne(BQ, Q);
        BigNumberPolynomialGaloisPower(PiPY.Nominator, Y2, BQ, Q, LDP);

        BigNumberRationalPolynomialCopy(RSX, PiPX);
        BigNumberRationalPolynomialCopy(RSY, PiPY);

        for J := 1 to (L + 1) shr 1 do
        begin
          // 本来可以直接用可除多项式计算 RSX := J * (PiPX, PiPY) 的 X，但似乎还会比点加慢，还是用点加
          // RationalMultiplePointX(RSX, PiPX, J, A, B, Q, DPs, LDP);

          if BigNumberRationalPolynomialGaloisEqual(LSX, RSX, Q, LDP) then
          begin
            // 本来可以直接用可除多项式计算 RSY := J * (PiPX, PiPY) 的 Y，但似乎还会比点加慢，还是用点加
            // RationalMultiplePointY(RSY, PiPX, PiPY, J, A, B, Q, DPs, LDP);

            if BigNumberRationalPolynomialGaloisEqual(LSY, RSY, Q, LDP) then
              Ta[I] := J
            else
              Ta[I] := L - J;
            Break;
          end;

          TCnPolynomialEcc.RationalPointAddPoint(RSX, RSY, PiPX, PiPY, TSX, TSY, A, B, Q, LDP);
          BigNumberRationalPolynomialCopy(RSX, TSX);
          BigNumberRationalPolynomialCopy(RSY, TSY);
        end;
      end;
    end;

    // 求出各个余数后，用中国剩余定理求最终解
    BigNumberChineseRemainderTheorem(Res, Ta, Pa);

    // 注意求出的 T 必须满足 Hasse 定理：T 的绝对值 <= 2 * 根号 Q，如超出范围，还得修正
    BigNumberSqrt(QMax, Q);
    QMax.AddWord(1);
    QMax.ShiftLeftOne;     // QMax 复用，是 2 根号 Q + 1，其绝对值必须比 Res 大

    if BigNumberUnsignedCompare(Res, QMax) >= 0 then
    begin
      // 中国剩余定理求出的一般是最小正数，需要减去全体 Pa 的乘积
      QMul.SetOne;
      for J := 0 to Pa.Count - 1 do
      begin
        BQ.SetInt64(Pa[J]);
        BigNumberMul(QMul, QMul, BQ);
      end;

      if Res.IsNegative then
        BigNumberAdd(Res, Res, QMul)
      else
        BigNumberSub(Res, Res, QMul);
    end;

    Res.Negate;
    BigNumberAdd(Res, Res, Q);
    Res.AddWord(1); // Q + 1 - L
    Result := True;
  finally
    FEccPolynomialPool.Recycle(Y2);
    FEccPolynomialPool.Recycle(P1);
    FEccPolynomialPool.Recycle(P2);

    FEccPolynomialPool.Recycle(G);

    FEccBigNumberPool.Recycle(QMax);
    FEccBigNumberPool.Recycle(QMul);
    FEccBigNumberPool.Recycle(BQ);

    FEccRationalPolynomialPool.Recycle(Pi2PX);
    FEccRationalPolynomialPool.Recycle(Pi2PY);
    FEccRationalPolynomialPool.Recycle(PiPX);
    FEccRationalPolynomialPool.Recycle(PiPY);
    FEccRationalPolynomialPool.Recycle(KPX);
    FEccRationalPolynomialPool.Recycle(KPY);
    FEccRationalPolynomialPool.Recycle(LSX);
    FEccRationalPolynomialPool.Recycle(LSY);
    FEccRationalPolynomialPool.Recycle(RSX);
    FEccRationalPolynomialPool.Recycle(RSY);
    FEccRationalPolynomialPool.Recycle(TSX);
    FEccRationalPolynomialPool.Recycle(TSY);
    FEccRationalPolynomialPool.Recycle(WPiPX);
    FEccRationalPolynomialPool.Recycle(WPiPY);

    DPs.Free;
    Pa.Free;
    Ta.Free;
  end;
end;

function CnEccFastSchoof(Res, A, B, Q: TCnBigNumber): Boolean;
var
  Pa, Ta: TCnInt64List;
  QMul, QMax, BQ, Q12, Q32, Q23, QT: TCnBigNumber;
  L, K, W: Int64;
  I, J, T: Integer;
  G, Y2, P1, P2, LDP: TCnBigNumberPolynomial;
  PXP2X, PXPX, NPXP2X, PXP2XPX, P16, P17, P18, P19X, P19Y, T1, T2, T3, T4, PAlpha, PBeta: TCnBigNumberPolynomial;
  DPs: TObjectList;

  function F(DPIdx: Integer): TCnBigNumberPolynomial; // 简化的得到 Division Polynomial 的
  begin
    Result := TCnBigNumberPolynomial(DPs[DPIdx]);
  end;

begin
{
    René Schoof’s Algorithm
  for Determining the Order of the Group of Points
    on an Elliptic Curve over a Finite Field
}

  Result := False;
  if Q.IsZero or Q.IsNegative then
    Exit;

  Pa := nil;
  Ta := nil;
  DPs := nil;

  Y2 := nil;
  P1 := nil;
  P2 := nil;

  G := nil;

  QMax := nil;
  QMul := nil;
  BQ := nil;
  Q12 := nil;
  Q32 := nil;
  Q23 := nil;
  QT := nil;

  PXP2X := nil;
  PXPX := nil;
  NPXP2X := nil;
  PXP2XPX := nil;
  T1 := nil;
  T2 := nil;
  T3 := nil;
  T4 := nil;
  P16 := nil;
  P17 := nil;
  P18 := nil;
  P19X := nil;
  P19Y := nil;
  PAlpha := nil;
  PBeta := nil;

  try
    Y2 := FEccPolynomialPool.Obtain;
    P1 := FEccPolynomialPool.Obtain;
    P2 := FEccPolynomialPool.Obtain;

    G := FEccPolynomialPool.Obtain;

    QMax := FEccBigNumberPool.Obtain;
    QMul := FEccBigNumberPool.Obtain;
    BQ := FEccBigNumberPool.Obtain;
    Q12 := FEccBigNumberPool.Obtain;
    Q32 := FEccBigNumberPool.Obtain;

    if not BigNumberSqrt(QMax, Q) then
      Exit;

    BigNumberAddWord(QMax, 1);
    BigNumberMulWord(QMax, 4);
    QMul.SetOne;
    I := Low(CN_PRIME_NUMBERS_SQRT_UINT32);

    Pa := TCnInt64List.Create;
    Ta := TCnInt64List.Create;

    PXP2X := FEccPolynomialPool.Obtain;
    PXPX := FEccPolynomialPool.Obtain;
    T1 := FEccPolynomialPool.Obtain;
    T2 := FEccPolynomialPool.Obtain;
    T3 := FEccPolynomialPool.Obtain;
    T4 := FEccPolynomialPool.Obtain;
    P16 := FEccPolynomialPool.Obtain;
    P17 := FEccPolynomialPool.Obtain;
    P18 := FEccPolynomialPool.Obtain;
    P19X := FEccPolynomialPool.Obtain;
    P19Y := FEccPolynomialPool.Obtain;

    while (BigNumberCompare(QMul, QMax) <= 0) and (I <= High(CN_PRIME_NUMBERS_SQRT_UINT32)) do
    begin
      BigNumberMulWord(QMul, CN_PRIME_NUMBERS_SQRT_UINT32[I]);
      Pa.Add(CN_PRIME_NUMBERS_SQRT_UINT32[I]);
      Ta.Add(0);
      Inc(I);
    end;

    if I > High(CN_PRIME_NUMBERS_SQRT_UINT32) then
      raise ECnEccException.Create('Prime Number is Too Large.');

    // 准备好 Y2，等于 x^3 + Ax + B
    Y2.SetCoefficents([B, A, 0, 1]);

    // Ta 与 Pa 数组已准备好，先处理 t = 2 的情况
    P1.SetCoefficents([0, 1]); // P1 := X
    BigNumberPolynomialGaloisPower(P1, P1, Q, Q, Y2); // X^q 先 mod Y^2

    P2.SetCoefficents([0, 1]); // P2 := X
    BigNumberPolynomialGaloisSub(P1, P1, P2, Q); // P1 := (X^q mod Y^2) - x

    // 求最大公约式
    BigNumberPolynomialGaloisGreatestCommonDivisor(G, P1, Y2, Q);

    if G.IsOne then
      Ta[0] := 1
    else
      Ta[0] := 0;   // 求得 T2。理解了并且基本算对了

    // 提前算好最大素数 + 2 阶的可除多项式们以及准备好 Y^2
    DPs := TObjectList.Create(True);
    CnGenerateGaloisDivisionPolynomials(A, B, Q, Pa[Pa.Count - 1] + 2, DPs);

    for I := 1 to Ta.Count - 1 do  // 针对每一个 L，都满足 π^2(P) + K * (P) = J * π^(P) mod L阶可除多项式
    begin
      L := Pa[I];
      K := BigNumberModWord(Q, L);

      // 先得到 L 阶可除多项式，作为后续计算的模多项式
      LDP := F(L);

      // 准备好 PXP2X 和 Y2，分别等于 x^(q^2) - x 和 x^3 + Ax + B
      PXP2X.SetCoefficents([0, 1]); // PXP2X := x
      BigNumberPolynomialGaloisPower(PXP2X, PXP2X, Q, Q, LDP); // x^q
      BigNumberPolynomialGaloisPower(PXP2X, PXP2X, Q, Q, LDP); // x^(q^2)
      T1.SetCoefficents([0, 1]);   // T1 = x
      BigNumberPolynomialGaloisSub(PXP2X, PXP2X, T1, Q, LDP);  // x^(q^2) - x

      // 准备好 PXPX，等于 x^q - x
      PXPX.SetCoefficents([0, 1]); // PXP2X := X
      BigNumberPolynomialGaloisPower(PXPX, PXPX, Q, Q, LDP); // X^q
      T1.SetCoefficents([0, 1]);   // T1 = x
      BigNumberPolynomialGaloisSub(PXPX, PXPX, T1, Q, LDP);  // X^q - X

      // 判断是否存在 L 阶扭点 P，使得 π^2(P) = 正负 K * (P)，分 K 是奇偶来分别计算 P16，基本上和论文中的计算例子一致
      if K and 1 <> 0 then
      begin
        // K 是奇数，P16 = (X^(q^2) - X) * F[K]^2 + F[K-1] * F[K+1] * (x^3 + Ax + B)
        BigNumberPolynomialGaloisMul(T1, F(K), F(K), Q, LDP);
        BigNumberPolynomialGaloisMul(T1, T1, PXP2X, Q, LDP);

        BigNumberPolynomialGaloisMul(T2, F(K - 1), F(K + 1), Q, LDP);
        BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP);

        BigNumberPolynomialGaloisAdd(P16, T1, T2, Q, LDP);
      end
      else
      begin
        // K 是偶数，P16 = (X^(q^2) - X) * F[K]^2 * (x^3 + Ax + B) + F[K-1] * F[K+1]
        BigNumberPolynomialGaloisMul(T1, F(K), F(K), Q, LDP);
        BigNumberPolynomialGaloisMul(T1, T1, Y2, Q, LDP);
        BigNumberPolynomialGaloisMul(T1, T1, PXP2X, Q, LDP);

        BigNumberPolynomialGaloisMul(T2, F(K - 1), F(K + 1), Q, LDP);

        BigNumberPolynomialGaloisAdd(P16, T1, T2, Q, LDP);
      end;

      // 得到 P16 后计算公因式
      BigNumberPolynomialGaloisGreatestCommonDivisor(T1, P16, LDP, Q);

      if not T1.IsOne then // 有公因式，所以 π^2(P) = 正负 K * (P)
      begin
        // 分正负两种情况处理。如果正，则求 W^2 = K mod L，W 存在则说明 K 是 L 的二次剩余
        W := CnInt64SquareRoot(K, L);
        if W = 0 then // 不存在二次剩余，t 为 0
        begin
          Ta[I] := 0;
          Continue;
        end;

        // 存在二次剩余，t 为正负 2W，判断其符号，要计算 P17，基本上和论文中的计算代码阅读对比后认为一致
        if W and 1 <> 0 then
        begin
          // W 是奇数，P17 = (X^q - X) * F[W]^2 + F[W-1] * F[W+1] * (x^3 + Ax + B)
          BigNumberPolynomialGaloisMul(T1, F(W), F(W), Q, LDP);
          BigNumberPolynomialGaloisMul(T1, T1, PXPX, Q, LDP);

          BigNumberPolynomialGaloisMul(T2, F(W - 1), F(W + 1), Q, LDP);
          BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP);

          BigNumberPolynomialGaloisAdd(P17, T1, T2, Q, LDP);
        end
        else
        begin
          // W 是偶数，P17 = (X^q - X) * F[W]^2 * (x^3 + Ax + B) + F[W-1] * F[W+1]
          BigNumberPolynomialGaloisMul(T1, F(W), F(W), Q, LDP);
          BigNumberPolynomialGaloisMul(T1, T1, PXPX, Q, LDP);
          BigNumberPolynomialGaloisMul(T1, T1, Y2, Q, LDP);

          BigNumberPolynomialGaloisMul(T2, F(W - 1), F(W + 1), Q, LDP);

          BigNumberPolynomialGaloisAdd(P17, T1, T2, Q, LDP);
        end;

        // 得到 P17 后计算公因式
        BigNumberPolynomialGaloisGreatestCommonDivisor(T1, P17, LDP, Q);
        if T1.IsOne then // 互素，t 为 0
        begin
          Ta[I] := 0;
          Continue;
        end;

        // 否则 t 是正负 2W，再判断正负号，求 P18，基本上和论文中的计算代码阅读对比后认为一致
        BigNumberCopy(Q12, Q);
        Q12.SubWord(1);
        Q12.ShiftRightOne;   // 得到 (Q - 1) / 2

        BigNumberCopy(Q32, Q);
        Q32.SubWord(3);
        Q32.ShiftRightOne;   // 得到 (Q - 3) / 2

        if W and 1 <> 0 then
        begin
          // W 是奇数，P18 = 4*(x^3 + Ax + B)^(Q-1)/2) * F[W]^3 - F[W+2]^2 * F[W-1] + F[W-2]^2 * F[W+1]
          BigNumberPolynomialGaloisPower(T1, Y2, Q12, Q, LDP);
        end
        else
        begin
          // W 是偶数，P18 = 4*(x^3 + Ax + B)^(Q+3)/2) * F[W]^3 - F[W+2]^2 * F[W-1] + F[W-2]^2 * F[W+1]
          BigNumberPolynomialGaloisPower(T1, Y2, Q32, Q, LDP);
        end;
        BigNumberPolynomialGaloisMulWord(T1, 4, Q);
        BigNumberPolynomialGaloisPower(T2, F(W), 3, Q, LDP);
        BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP); // T1 得到第一项大乘

        BigNumberPolynomialGaloisMul(T2, F(W + 2), F(W + 2), Q, LDP);  // T2 得到减项
        BigNumberPolynomialGaloisMul(T2, T2, F(W - 1), Q, LDP);

        BigNumberPolynomialGaloisMul(T3, F(W - 2), F(W - 2), Q, LDP);  // T3 得到加项
        BigNumberPolynomialGaloisMul(T3, T3, F(W + 1), Q, LDP);

        BigNumberPolynomialGaloisSub(P18, T1, T2, Q, LDP);
        BigNumberPolynomialGaloisAdd(P18, P18, T3, Q, LDP);

        // 得到 P18 后计算公因式
        BigNumberPolynomialGaloisGreatestCommonDivisor(T1, P18, LDP, Q);
        if T1.IsOne then
          Ta[I] := L - 2 * W
        else
          Ta[I] := 2 * W;
      end
      else // 不等于正负，开始整 P19X 和 P19Y
      begin
        QT := FEccBigNumberPool.Obtain; // 准备一个临时大数作为 Q 相关的指数的临时存储

        PAlpha := FEccPolynomialPool.Obtain;
        PBeta := FEccPolynomialPool.Obtain;

        // 先计算 Alpha
        BigNumberPolynomialGaloisMul(T1, F(K - 1), F(K - 1), Q, LDP);
        BigNumberPolynomialGaloisMul(T1, T1, F(K + 2), Q, LDP);                 // T1 得到 Fk-1^2 * Fk+2

        BigNumberPolynomialGaloisMul(T2, F(K + 1), F(K + 1), Q, LDP);
        BigNumberPolynomialGaloisMul(T2, T2, F(K - 2), Q, LDP);                 // T2 得到 Fk+1^2 * Fk-2

        BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP); // T1 是减式 Fk-1^2 * Fk+2 - Fk+1^2 * Fk-2，释放 T2

        Q23 := FEccBigNumberPool.Obtain;
        BigNumberMul(Q23, Q, Q);

        if K and 1 <> 0 then  // K 偶数时需要 (Q^2 + 3)/2
          Q23.AddWord(3)
        else
          Q23.AddWord(1);     // K 奇数时需要 (Q^2 + 1)/2

        Q23.ShiftRightOne;  // 得到 (Q^2 + 3)/2 或 (Q^2 + 1)/2，用来做 Y^2 的指数

        BigNumberPolynomialGaloisPower(T2, Y2, Q23, Q, LDP);
        BigNumberPolynomialGaloisPower(T3, F(K), 3, Q, LDP);
        BigNumberPolynomialGaloisMul(T2, T2, T3, Q, LDP);
        BigNumberPolynomialGaloisMulWord(T2, 4, Q); // 得到第三个减式

        BigNumberPolynomialSub(PAlpha, T1, T2);     // 计算出 PAlpha，释放 T1 T2
        // 注意此处如果 K 是奇数，得到的 PAlpha 在使用时还要乘以一个 Y2
        // 如果 K 是偶数，得到的 PAlpha 其实是 Alpha / y 的值

        // 再计算 Beta
        NPXP2X := FEccPolynomialPool.Obtain;
        BigNumberPolynomialCopy(NPXP2X, PXP2X);
        BigNumberPolynomialGaloisNegate(NPXP2X, Q); // NPXPX 得到 x - x^(q^2)

        BigNumberPolynomialGaloisMul(T1, F(K), F(K), Q, LDP); // T1 得到 Fk^2
        BigNumberPolynomialGaloisMul(T1, NPXP2X, T1, Q, LDP); // T1 得到 Fk^2 * (x - x^(q^2))，备下面使用

        BigNumberPolynomialGaloisMul(T2, F(K - 1), F(K + 1), Q, LDP); // T2 得到 F(k-1)* F(k+1)

        if K and 1 <> 0 then // 奇数
        begin
          // K 是奇数，对应的 Alpha 是纯 x 的系数，Beta 则需要乘一个 y，也就是说，PBeta 其实是 Beta / y 的值
          // Alpha = Y^2 * 上面的 PAlpha
          BigNumberPolynomialGaloisMul(PAlpha, PAlpha, Y2, Q, LDP); // 完整的 Alpha 计算完毕

          // 再分别计算 Beta T2 要乘以 Y2
          BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP);

          BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP); // T1 得到减的结果

          // 再乘以 4Fk
          BigNumberPolynomialGaloisMul(PBeta, T1, F(K), Q, LDP);
          BigNumberPolynomialGaloisMulWord(PBeta, 4, Q);          // 得到的 PBeta 在使用时需要额外乘以一个 y
        end
        else // 偶数
        begin
          // K 是偶数，对应的 Alpha 是需要乘一个 y，也就是说，PAlpha 其实是 Alpha / y 的值，Beta 则是纯 x 的系数

          // 再分别计算 Beta，T1 要乘以 Y2
          BigNumberPolynomialGaloisMul(T1, T1, Y2, Q, LDP);

          BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP); // T1 得到减的结果

          // 再乘以 4Fk
          BigNumberPolynomialGaloisMul(PBeta, T1, F(K), Q, LDP);
          BigNumberPolynomialGaloisMulWord(PBeta, 4, Q);

          // Beta = Y^2 * 上面的 PBeta
          BigNumberPolynomialGaloisMul(PBeta, PBeta, Y2, Q, LDP);    // 完整的 Beta 计算完毕
        end;

        // 以上 Alpha 和 Beta 准备好了，另外准备好 x^(p^2) + x^p + x
        PXP2XPX := FEccPolynomialPool.Obtain;
        PXP2XPX.SetCoefficents([0, 2]);        // 得到 2x
        BigNumberPolynomialGaloisAdd(PXP2XPX, PXP2X, PXP2XPX, Q, LDP); // 与旧的加一下得到 x^(p^2) + x

        T3.SetCoefficents([0, 1]);
        BigNumberPolynomialGaloisPower(T3, T3, Q, Q, LDP);
        BigNumberPolynomialGaloisAdd(PXP2XPX, PXP2XPX, T3, Q, LDP);   // 得到 x^(p^2) + x^p + x

        if K and 1 <> 0 then
        begin
          for T := 1 to L - 1 do // 这个 T 指希腊字母中的 Tao
          begin
            // K 是奇数的情况下，对应 PBeta 实际上是 Beta / y，需要乘以一个 Y
            // 也挨个计算 P19X，当其 mod LDP = 0 且和 LDP 的最大公约式 <> 1 时，有正负 T 符合要求
            // K 奇 t 奇的情况下 P19X = （以下 a 表示 alpha，b 表示 beta/y）
            // Ft^2p * (b^2 * Y^2 * (Y^2 * Fk-1 * Fk+1 - Fk^2 *(x^(p^2) + x^p + x) + a^2 * Fk^2)) + Fk^2 * b^2 * Y^2 * (Ft-1 * Ft+1)^p * (Y^2)^p
            // t 偶的情况下 P19X 变成（均可变为纯 x 多项式）
            // Ft^2p * (Y^2)^p * (b^2 * Y^2 * (Y^2 * Fk-1 * Fk+1 - Fk^2 *(x^(p^2) + x^p + x) + a^2 * Fk^2)) + Fk^2 * b^2 * Y^2 * (Ft-1 * Ft+1)^p

            // 先计算前后各项，但不包括前后的 Y^2p，之后再根据 T 的奇偶性各自乘上
            // 先计算 Fk^2 * b^2 * Y^2 * (Ft-1 * Ft+1)^p 放到 T3 里
            BigNumberPolynomialGaloisMul(T1, F(K), F(K), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, PBeta, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, Y2, Q, LDP); // T1 得到 Fk^2 * b^2 * Y^2，

            BigNumberPolynomialGaloisMul(T2, F(T - 1), F(T + 1), Q, LDP);
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);

            BigNumberPolynomialGaloisMul(T3, T1, T2, Q, LDP); // T3 得到 Fk^2 * b^2 * Y^2 * (Ft-1 * Ft+1)^p，释放 T1 T2
            if T and 1 <> 0 then // T 为奇数时要多乘一项
            begin
              BigNumberPolynomialGaloisPower(T1, Y2, Q, Q, LDP);
              BigNumberPolynomialGaloisMul(T3, T3, T1, Q, LDP);
            end;

            // 计算前面的加项，不能用 T3
            BigNumberPolynomialGaloisMul(T1, F(K - 1), F(K + 1), Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, Y2, Q, LDP); // T1 得到 Y^2 * Fk-1 * Fk+1

            BigNumberPolynomialGaloisMul(T2, F(K), F(K), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PXP2XPX, Q, LDP); // T2 得到 Fk^2 *(x^(p^2) + x^p + x)

            BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP); // T1 减后释放 T2

            BigNumberPolynomialGaloisMul(T2, F(K), PAlpha, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, T2, Q, LDP); // T2 得到 a^2 * Fk^2

            BigNumberPolynomialGaloisAdd(T1, T1, T2, Q, LDP); // T1 得到全减式，释放 T2

            BigNumberPolynomialGaloisMul(T2, PBeta, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP); // T2 得到 b^2 * Y^2，对应 PBeta 需要乘以一个 Y

            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP); // 全减式乘后放入 T1，释放 T2

            BigNumberPolynomialGaloisMul(T2, F(T), F(T), Q, LDP);        // T2 得到 Ft^2

            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);// T2 得到 (Ft^2)^p = Ft^2p

            if T and 1 = 0 then // T 为偶数时 T2 要多乘一项 (Y^2)^p
            begin
              BigNumberPolynomialGaloisPower(T4, Y2, Q, Q, LDP);
              BigNumberPolynomialGaloisMul(T2, T2, T4, Q, LDP);
            end;

            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP); // T1 得到加号左边的，和右边相加
            BigNumberPolynomialGaloisAdd(P19X, T1, T3, Q, LDP);  // 加后得到 P19X

            BigNumberPolynomialGaloisGreatestCommonDivisor(T1, P19X, LDP, Q);

            if T1.IsOne then // 不为 1 时存在符合要求的点。为 1 时不存在，本轮 T 不符合要求，下一循环
              Continue;

            // 不为 1 时，正负 T 都符合要求，再求 K 是奇数情形下的 P19Y，分 T 是奇偶处理
            // 分别求两个公共项，并求差，
            // 第一个公共项：4Ft^3p * ( ( (2x^(p^2)+x)*a*y^2*b^2 - b^3*y^(p^2+3) - a^3  )* Fk^2 - a*y^2*b^2 * Fk-1* Fk+1 )
            //（这项如果 T 是偶，还得乘以 y^(3p-3)
            // 第二个公共项：b^3 * Fk^2 * (Ft-1^2 * Ft+2 - Ft-2 * Ft+1^2)^p
            //（这项如果 T 是奇，还得乘以 y^(p+3)

            BigNumberPolynomialGaloisPower(T1, Y2, Q23, Q, LDP);                // T1 得到 y^(p^2+3)
            BigNumberPolynomialGaloisMul(T1, PBeta, T1, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, PBeta, T1, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, PBeta, T1, Q, LDP);                // T1 得到 b^3*y^(p^2+3)

            BigNumberPolynomialGaloisMul(T2, PAlpha, PAlpha, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PAlpha, Q, LDP);               // T2 得到 a^3

            BigNumberPolynomialGaloisAdd(T1, T1, T2, Q, LDP);                   // T1 得到  b^3*y^(p^2+3) + a^3，留着被减，并释放 T2，只占着 T1

            T2.SetCoefficents([0, 1]);                                          // x
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);                  // x^q
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);                  // x^(q^2)
            BigNumberPolynomialGaloisAdd(T2, T2, T2, Q, LDP);                   // 2*x^(q^2)

            T3.SetCoefficents([0, 1]);
            BigNumberPolynomialGaloisAdd(T2, T2, T3, Q, LDP);                   // T2 得到 2x^(p^2)+x)，释放 T3

            BigNumberPolynomialGaloisMul(T2, T2, PAlpha, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PBeta, Q, LDP);                // T2 得到 (2x^(p^2)+x)*a*y^2*b^2

            BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP);                   // T1 得到 (2x^(p^2)+x)*a*y^2*b^2 - b^3*y^(p^2+3) - a^3，并释放 T2

            BigNumberPolynomialGaloisMul(T1, T1, F(K), Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, F(K), Q, LDP);                 // T1 再乘以 Fk^2

            BigNumberPolynomialGaloisMul(T2, PAlpha, Y2, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, F(K - 1), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, F(K + 1), Q, LDP);             // T2 得到 a*y^2*b^2 * Fk-1* Fk+1

            BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP);                   // T1 得到未乘以 4Ft^3p 的第一个公共项，并释放 T2

            BigNumberPolynomialCopy(T2, F(T));                                  // 用 T2 计算 4Ft^3p
            BigNumberCopy(QT, Q);
            BigNumberMulWord(QT, 3);
            BigNumberPolynomialGaloisPower(T2, T2, QT, Q, LDP);
            BigNumberPolynomialGaloisMulWord(T2, 4, Q);                         // T2 得到 4Ft^3p

            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP);                   // T1 得到第一个公共项，并且只占用 T1

            // 计算第二个公共项 b^3 * Fk^2 * (Ft-1^2 * Ft+2 - Ft-2 * Ft+1^2)^p
            BigNumberPolynomialGaloisMul(T2, F(T - 1), F(T + 2), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, F(T - 1), T2, Q, LDP);
            BigNumberPolynomialGaloisMul(T3, F(T + 1), F(T - 2), Q, LDP);
            BigNumberPolynomialGaloisMul(T3, F(T + 1), T3, Q, LDP);

            BigNumberPolynomialGaloisSub(T2, T2, T3, Q, LDP);                   // 得到减式 Ft-1^2 * Ft+2 - Ft-2 * Ft+1^2，并释放 T3
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);                  // p 次方

            BigNumberPolynomialGaloisMul(T3, F(K), F(K), Q, LDP);               // T3 得到 Fk^2
            BigNumberPolynomialGaloisMul(T2, T2, T3, Q, LDP);                   // T2 乘以 T3，并释放 T3

            BigNumberPolynomialGaloisPower(T3, PBeta, 3, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, T3, Q, LDP);                   // T2 再乘以 T3，得到第二个公共项，并释放 T3

            if T and 1 = 0 then // T 是偶，公共项一要乘
            begin
              // 先求 y^(3p-3)
              BigNumberCopy(QT, Q);
              BigNumberMulWord(QT, 3);
              BigNumberSubWord(QT, 3);
              QT.ShiftRightOne;

              BigNumberPolynomialGaloisPower(T3, Y2, QT, Q, LDP);
              BigNumberPolynomialGaloisMul(T1, T1, T3, Q, LDP);
            end
            else // T 是奇，公共项二要乘
            begin
              // 先求 y^(p+3)
              BigNumberCopy(QT, Q);
              BigNumberAddWord(QT, 3);
              QT.ShiftRightOne;

              BigNumberPolynomialGaloisPower(T3, Y2, QT, Q, LDP);
              BigNumberPolynomialGaloisMul(T2, T2, T3, Q, LDP);
            end;

            BigNumberPolynomialGaloisSub(P19Y, T1, T2, Q, LDP);  // 俩公共项减后得到 P19Y

            BigNumberPolynomialGaloisGreatestCommonDivisor(T1, P19Y, LDP, Q);

            if T1.IsOne then // 不为 1 时 T，否则 -T
              Ta[I] := T
            else
              Ta[I] := L - T;
          end;
        end
        else
        begin
          for T := 1 to L - 1 do
          begin
            // K 偶数时，PAlpha 其实是 Alpha / y 的值
            // K 偶 t 奇的情况下 P19X = 以下 a 表示 alpha/y，b 表示 beta
            // Ft^2p * (b^2 * (Fk-1 * Fk+1 - Y^2 * Fk^2 *(x^(p^2) + x^p + x) + (Y^2)^2 * a^2 * Fk^2)) + Fk^2 * b^2 * Y^2 *(Ft-1 * Ft+1)^p * (Y^2)^p
            // t 偶的情况下 P19X 变成（均可变为纯 x 多项式）
            // Ft^2p * (Y^2)^p * (b^2 * (Fk-1 * Fk+1 - Y^2 * Fk^2 *(x^(p^2) + x^p + x) + (Y^2)^2 * a^2 * Fk^2)) + Fk^2 * b^2 * Y^2 *(Ft-1 * Ft+1)^p

            // 先计算前后各项，但不包括前后的 Y^2p，之后再根据 T 的奇偶性各自乘上
            // 先计算 Fk^2 * b^2 * Y^2 * (Ft-1 * Ft+1)^p 放到 T3 里
            BigNumberPolynomialGaloisMul(T1, F(K), F(K), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, PBeta, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, Y2, Q, LDP); // T1 得到 Fk^2 * b^2 * Y^2

            BigNumberPolynomialGaloisMul(T2, F(T - 1), F(T + 1), Q, LDP);
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);

            BigNumberPolynomialGaloisMul(T3, T1, T2, Q, LDP); // T3 得到 Fk^2 * b^2 * Y^2 * (Ft-1 * Ft+1)^p，释放 T1 T2
            if T and 1 <> 0 then // T 为奇数时要多乘一项
            begin
              BigNumberPolynomialGaloisPower(T1, Y2, Q, Q, LDP);
              BigNumberPolynomialGaloisMul(T3, T3, T1, Q, LDP);
            end;

            // 计算前面的加项，不能用 T3
            BigNumberPolynomialGaloisMul(T1, F(K - 1), F(K + 1), Q, LDP);       // T1 得到 Fk-1 * Fk+1

            BigNumberPolynomialGaloisMul(T2, F(K), F(K), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PXP2XPX, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP);  // T2 得到 y^2 * Fk^2 *(x^(p^2) + x^p + x)

            BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP); // T1 减后释放 T2

            BigNumberPolynomialGaloisMul(T2, F(K), PAlpha, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, T2, Q, LDP); // T2 得到 a^2 * Fk^2
            BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP); // T2 得到 (Y^2)^2 * a^2 * Fk^2

            BigNumberPolynomialGaloisAdd(T1, T1, T2, Q, LDP); // T1 得到全减式，释放 T2

            BigNumberPolynomialGaloisMul(T2, PBeta, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP); // 全减式乘 Beta^2 后放入 T1，释放 T2

            BigNumberPolynomialGaloisMul(T2, F(T), F(T), Q, LDP);        // T2 得到 Ft^2

            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);// T2 得到 (Ft^2)^p = Ft^2p

            if T and 1 = 0 then // T 为偶数时 T2 要多乘一项 (Y^2)^p
            begin
              BigNumberPolynomialGaloisPower(T4, Y2, Q, Q, LDP);
              BigNumberPolynomialGaloisMul(T2, T2, T4, Q, LDP);
            end;

            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP); // T1 得到加号左边的，和右边相加
            BigNumberPolynomialGaloisAdd(P19X, T1, T3, Q, LDP);  // 加后得到 P19X

            BigNumberPolynomialGaloisGreatestCommonDivisor(T1, P19X, LDP, Q);

            if T1.IsOne then // 不为 1 时存在符合要求的点。为 1 时不存在，本轮 T 不符合要求，下一循环
              Continue;

            // 不为 1 时，正负 T 都符合要求，再求 K 是偶数情形下的 P19Y，分 T 是奇偶处理
            // 分别求两个公共项，并求差，
            // 第一个公共项：4Ft^3p * ( ( (2x^(p^2)+x)*a*b^2 - b^3*y^(p^2-1) - y^2 * a^3  )* y^2 * Fk^2 - a*b^2 * Fk-1* Fk+1 )
            //（这项如果 T 是偶，还得乘以 y^(3p-1)
            // 第二个公共项：b^3 * Fk^2 * (Ft-1^2 * Ft+2 - Ft-2 * Ft+1^2)^p
            //（这项如果 T 是奇，还得乘以 y^(p+1)

            BigNumberMul(QT, Q, Q);
            BigNumberSubWord(QT, 1);
            QT.ShiftRightOne;
            BigNumberPolynomialGaloisPower(T1, Y2, QT, Q, LDP);                // T1 得到 y^(p^2-1)
            BigNumberPolynomialGaloisMul(T1, PBeta, T1, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, PBeta, T1, Q, LDP);
            BigNumberPolynomialGaloisMul(T1, PBeta, T1, Q, LDP);                // T1 得到 b^3*y^(p^2-1)

            BigNumberPolynomialGaloisMul(T2, PAlpha, PAlpha, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PAlpha, Q, LDP);               // T2 得到 a^3
            BigNumberPolynomialGaloisMul(T2, T2, Y2, Q, LDP);                   // T2 得到 y^2 * a^3

            BigNumberPolynomialGaloisAdd(T1, T1, T2, Q, LDP);                   // T1 得到  b^3*y^(p^2-1) + y^2 * a^3，留着被减，并释放 T2，只占着 T1

            T2.SetCoefficents([0, 1]);                                          // x
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);                  // x^q
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);                  // x^(q^2)
            BigNumberPolynomialGaloisAdd(T2, T2, T2, Q, LDP);                   // 2*x^(q^2)

            T3.SetCoefficents([0, 1]);
            BigNumberPolynomialGaloisAdd(T2, T2, T3, Q, LDP);                   // T2 得到 2x^(p^2)+x)，释放 T3

            BigNumberPolynomialGaloisMul(T2, T2, PAlpha, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PBeta, Q, LDP);                // T2 得到 (2x^(p^2)+x)*a*b^2

            BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP);                   // T1 得到 (2x^(p^2)+x)*a*b^2 - b^3*y^(p^2-1) - y^2 * a^3，并释放 T2

            BigNumberPolynomialGaloisMul(T1, T1, F(K), Q, LDP);
            BigNumberPolynomialGaloisMul(T1, T1, F(K), Q, LDP);                 // T1 再乘以 Fk^2
            BigNumberPolynomialGaloisMul(T1, T1, Y2, Q, LDP);                   // T1 再乘以 y^2

            BigNumberPolynomialGaloisMul(T2, PAlpha, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, PBeta, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, F(K - 1), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, F(K + 1), Q, LDP);             // T2 得到 a*b^2 * Fk-1* Fk+1

            BigNumberPolynomialGaloisSub(T1, T1, T2, Q, LDP);                   // T1 得到未乘以 4Ft^3p 的第一个公共项，并释放 T2

            BigNumberPolynomialCopy(T2, F(T));                                  // 用 T2 计算 4Ft^3p
            BigNumberCopy(QT, Q);
            BigNumberMulWord(QT, 3);
            BigNumberPolynomialGaloisPower(T2, T2, QT, Q, LDP);
            BigNumberPolynomialGaloisMulWord(T2, 4, Q);                         // T2 得到 4Ft^3p

            BigNumberPolynomialGaloisMul(T1, T1, T2, Q, LDP);                   // T1 得到第一个公共项，并且只占用 T1

            // 计算第二个公共项 b^3 * Fk^2 * (Ft-1^2 * Ft+2 - Ft-2 * Ft+1^2)^p
            BigNumberPolynomialGaloisMul(T2, F(T - 1), F(T + 2), Q, LDP);
            BigNumberPolynomialGaloisMul(T2, F(T - 1), T2, Q, LDP);
            BigNumberPolynomialGaloisMul(T3, F(T + 1), F(T - 2), Q, LDP);
            BigNumberPolynomialGaloisMul(T3, F(T + 1), T3, Q, LDP);

            BigNumberPolynomialGaloisSub(T2, T2, T3, Q, LDP);                   // 得到减式 Ft-1^2 * Ft+2 - Ft-2 * Ft+1^2，并释放 T3
            BigNumberPolynomialGaloisPower(T2, T2, Q, Q, LDP);                  // p 次方

            BigNumberPolynomialGaloisMul(T3, F(K), F(K), Q, LDP);               // T3 得到 Fk^2
            BigNumberPolynomialGaloisMul(T2, T2, T3, Q, LDP);                   // T2 乘以 T3，并释放 T3

            BigNumberPolynomialGaloisPower(T3, PBeta, 3, Q, LDP);
            BigNumberPolynomialGaloisMul(T2, T2, T3, Q, LDP);                   // T2 再乘以 T3，得到第二个公共项，并释放 T3

            if T and 1 = 0 then // T 是偶，公共项一要乘
            begin
              // 先求 y^(3p-1)
              BigNumberCopy(QT, Q);
              BigNumberMulWord(QT, 3);
              BigNumberSubWord(QT, 1);
              QT.ShiftRightOne;

              BigNumberPolynomialGaloisPower(T3, Y2, QT, Q, LDP);
              BigNumberPolynomialGaloisMul(T1, T1, T3, Q, LDP);
            end
            else // T 是奇，公共项二要乘
            begin
              // 先求 y^(p+1)
              BigNumberCopy(QT, Q);
              BigNumberAddWord(QT, 1);
              QT.ShiftRightOne;

              BigNumberPolynomialGaloisPower(T3, Y2, QT, Q, LDP);
              BigNumberPolynomialGaloisMul(T2, T2, T3, Q, LDP);
            end;

            BigNumberPolynomialGaloisSub(P19Y, T1, T2, Q, LDP);  // 俩公共项减后得到 P19Y

            BigNumberPolynomialGaloisGreatestCommonDivisor(T1, P19Y, LDP, Q);

            if T1.IsOne then // 不为 1 时 T，否则 -T
              Ta[I] := T
            else
              Ta[I] := L - T;
          end;
        end;
      end;
    end;

    // 求出各个余数后，用中国剩余定理求最终解
    BigNumberChineseRemainderTheorem(Res, Ta, Pa);

    // 注意求出的 T 必须满足 Hasse 定理：T 的绝对值 <= 2 * 根号 Q，如超出范围，还得修正
    BigNumberSqrt(QMax, Q);
    QMax.AddWord(1);
    QMax.ShiftLeftOne;     // QMax 复用，是 2 根号 Q + 1，其绝对值必须比 Res 大

    if BigNumberUnsignedCompare(Res, QMax) >= 0 then
    begin
      // 中国剩余定理求出的一般是最小正数，需要减去全体 Pa 的乘积
      QMul.SetOne;
      for J := 0 to Pa.Count - 1 do
      begin
        BQ.SetInt64(Pa[J]);
        BigNumberMul(QMul, QMul, BQ);
      end;

      if Res.IsNegative then
        BigNumberAdd(Res, Res, QMul)
      else
        BigNumberSub(Res, Res, QMul);
    end;

    Res.Negate;
    BigNumberAdd(Res, Res, Q);
    Res.AddWord(1); // Q + 1 - L
    Result := True;
  finally
    FEccPolynomialPool.Recycle(PXP2X);
    FEccPolynomialPool.Recycle(PXPX);
    FEccPolynomialPool.Recycle(NPXP2X);
    FEccPolynomialPool.Recycle(PXP2XPX);
    FEccPolynomialPool.Recycle(T1);
    FEccPolynomialPool.Recycle(T2);
    FEccPolynomialPool.Recycle(T3);
    FEccPolynomialPool.Recycle(T4);
    FEccPolynomialPool.Recycle(P16);
    FEccPolynomialPool.Recycle(P17);
    FEccPolynomialPool.Recycle(P18);
    FEccPolynomialPool.Recycle(P19X);
    FEccPolynomialPool.Recycle(P19Y);
    FEccPolynomialPool.Recycle(PAlpha);
    FEccPolynomialPool.Recycle(PBeta);

    FEccPolynomialPool.Recycle(Y2);
    FEccPolynomialPool.Recycle(P1);
    FEccPolynomialPool.Recycle(P2);

    FEccPolynomialPool.Recycle(G);

    FEccBigNumberPool.Recycle(QMax);
    FEccBigNumberPool.Recycle(QMul);
    FEccBigNumberPool.Recycle(BQ);
    FEccBigNumberPool.Recycle(Q12);
    FEccBigNumberPool.Recycle(Q32);
    FEccBigNumberPool.Recycle(Q23);
    FEccBigNumberPool.Recycle(QT);

    DPs.Free;
    Pa.Free;
    Ta.Free;
  end;
end;

{ TCnEcc3Point }

procedure TCnEcc3Point.Assign(Source: TPersistent);
begin
  if Source is TCnEcc3Point then
  begin
    BigNumberCopy(FX, (Source as TCnEcc3Point).X);
    BigNumberCopy(FY, (Source as TCnEcc3Point).Y);
    BigNumberCopy(FZ, (Source as TCnEcc3Point).Z);
  end
  else
    inherited;
end;

constructor TCnEcc3Point.Create;
begin
  inherited;
  FX := TCnBigNumber.Create;
  FY := TCnBigNumber.Create;
  FZ := TCnBigNumber.Create;
end;

destructor TCnEcc3Point.Destroy;
begin
  FZ.Free;
  FY.Free;
  FX.Free;
  inherited;
end;

function TCnEcc3Point.IsZero: Boolean;
begin
  Result := Z.IsZero;
end;

procedure TCnEcc3Point.SetX(const Value: TCnBigNumber);
begin
  BigNumberCopy(FX, Value);
end;

procedure TCnEcc3Point.SetY(const Value: TCnBigNumber);
begin
  BigNumberCopy(FY, Value);
end;

procedure TCnEcc3Point.SetZ(const Value: TCnBigNumber);
begin
  BigNumberCopy(FZ, Value);
end;

procedure TCnEcc3Point.SetZero;
begin
  X.SetZero;
  Y.SetZero;
  Z.SetZero;
end;

function TCnEcc3Point.ToString: string;
begin
  Result := CnEcc3PointToHex(Self);
end;

{ TCnEccSignature }

procedure TCnEccSignature.Assign(Source: TPersistent);
begin
  if Source is TCnEccSignature then
  begin
    BigNumberCopy(FR, (Source as TCnEccSignature).R);
    BigNumberCopy(FS, (Source as TCnEccSignature).S);
  end
  else
    inherited;
end;

constructor TCnEccSignature.Create;
begin
  inherited;
  FR := TCnBigNumber.Create;
  FS := TCnBigNumber.Create;
end;

destructor TCnEccSignature.Destroy;
begin
  FS.Free;
  FR.Free;
  inherited;
end;

function TCnEccSignature.SetAsn1Base64(const Buf: AnsiString): Boolean;
var
  B: TBytes;
  Reader: TCnBerReader;
  NR, NS: TCnBerReadNode;
begin
  Result := False;
  Reader := nil;

  try
    if Base64Decode(string(Buf), B) = ECN_BASE64_OK then
    begin
      Reader := TCnBerReader.Create(PByte(@B[0]), Length(B));
      Reader.ParseToTree;

      if Reader.TotalCount = 3 then
      begin
        NR := Reader.Items[1];
        NS := Reader.Items[2];

        PutIndexedBigIntegerToBigNumber(NR, FR);
        PutIndexedBigIntegerToBigNumber(NS, FS);
        Result := True;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

function TCnEccSignature.SetAsn1Hex(const Buf: AnsiString): Boolean;
var
  B: TBytes;
  Reader: TCnBerReader;
  NR, NS: TCnBerReadNode;
begin
  Result := False;
  B := HexToBytes(string(Buf));
  if Length(B) <= 1 then
    Exit;

  Reader := nil;
  try
    Reader := TCnBerReader.Create(PByte(@B[0]), Length(B));
    Reader.ParseToTree;

    if Reader.TotalCount = 3 then
    begin
      NR := Reader.Items[1];
      NS := Reader.Items[2];

      PutIndexedBigIntegerToBigNumber(NR, FR);
      PutIndexedBigIntegerToBigNumber(NS, FS);
      Result := True;
    end;
  finally
    Reader.Free;
  end;
end;

function TCnEccSignature.SetBase64(const Buf: AnsiString): Boolean;
var
  B: TBytes;
begin
  Result := False;
  if Base64Decode(string(Buf), B) = ECN_BASE64_OK then
  begin
    SetHex(AnsiString(BytesToHex(B)));
    Result := True;
  end;
end;

procedure TCnEccSignature.SetHex(const Buf: AnsiString);
var
  C: Integer;
begin
  if (Length(Buf) < 4) or ((Length(Buf) mod 4) <> 0) then
    raise ECnEccException.Create(SCnErrorEccKeyData);

  // 一半一半，长度得相等
  C := Length(Buf) div 2;
  FR.SetHex(Copy(Buf, 1, C));
  FS.SetHex(Copy(Buf, C + 1, MaxInt));
end;

function TCnEccSignature.ToAsn1Base64: string;
var
  Writer: TCnBerWriter;
  Root: TCnBerWriteNode;
  Stream: TMemoryStream;
begin
  Writer := nil;
  Stream := nil;

  try
    Writer := TCnBerWriter.Create;

    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);
    AddBigNumberToWriter(Writer, FR, Root);
    AddBigNumberToWriter(Writer, FS, Root);

    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);

    Base64Encode(Stream.Memory, Stream.Size, Result);
  finally
    Writer.Free;
    Stream.Free;
  end;
end;

function TCnEccSignature.ToAsn1Hex(FixedLen: Integer): string;
var
  Writer: TCnBerWriter;
  Root: TCnBerWriteNode;
  Stream: TMemoryStream;
begin
  Writer := nil;
  Stream := nil;

  try
    Writer := TCnBerWriter.Create;

    Root := Writer.AddContainerNode(CN_BER_TAG_SEQUENCE);

    // 如果不加 FixedLen 的要求，目测 ASN1 解析时如果因为有前导 0 导致位数不足也能通过
    // 不过输出时容易给人以长度不一的困惑，因此这儿加上，允许外界指定固定长度
    AddBigNumberToWriter(Writer, FR, Root, CN_BER_TAG_INTEGER, FixedLen);
    AddBigNumberToWriter(Writer, FS, Root, CN_BER_TAG_INTEGER, FixedLen);

    Stream := TMemoryStream.Create;
    Writer.SaveToStream(Stream);

    Result := DataToHex(Stream.Memory, Stream.Size);
  finally
    Writer.Free;
    Stream.Free;
  end;
end;

function TCnEccSignature.ToBase64(FixedLen: Integer): string;
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    FR.SaveToStream(M, FixedLen);
    FS.SaveToStream(M, FixedLen);
    Base64Encode(M.Memory, M.Size, Result);
  finally
    M.Free;
  end;
end;

function TCnEccSignature.ToHex(FixedLen: Integer): string;
begin
  Result := FR.ToHex(FixedLen) + FS.ToHex(FixedLen);
end;

{ TCnEcc2Matrix }

constructor TCnEcc2Matrix.Create(ARow, ACol: Integer);
var
  I, J: Integer;
begin
  inherited;
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      ValueObject[I, J] := TCnEccPoint.Create;
end;

function TCnEcc2Matrix.GetValueObject(Row, Col: Integer): TCnEccPoint;
begin
  Result := TCnEccPoint(inherited GetValueObject(Row, Col));
end;

procedure TCnEcc2Matrix.SetValueObject(Row, Col: Integer;
  const Value: TCnEccPoint);
begin
  inherited SetValueObject(Row, Col, Value);
end;

{ TCnEcc3Matrix }

constructor TCnEcc3Matrix.Create(ARow, ACol: Integer);
var
  I, J: Integer;
begin
  inherited;
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      ValueObject[I, J] := TCnEcc3Point.Create;
end;

function TCnEcc3Matrix.GetValueObject(Row, Col: Integer): TCnEcc3Point;
begin
  Result := TCnEcc3Point(inherited GetValueObject(Row, Col));
end;

procedure TCnEcc3Matrix.SetValueObject(Row, Col: Integer;
  const Value: TCnEcc3Point);
begin
  inherited SetValueObject(Row, Col, Value);
end;

initialization
  FEccBigNumberPool := TCnBigNumberPool.Create;
  FEccInt64PolynomialPool := TCnInt64PolynomialPool.Create;
  FEccPolynomialPool := TCnBigNumberPolynomialPool.Create;
  FEccInt64RationalPolynomialPool := TCnInt64RationalPolynomialPool.Create;
  FEccRationalPolynomialPool := TCnBigNumberRationalPolynomialPool.Create;

finalization
  FEccInt64RationalPolynomialPool.Free;
  FEccRationalPolynomialPool.Free;
  FEccPolynomialPool.Free;
  FEccInt64PolynomialPool.Free;
  FEccBigNumberPool.Free;

end.
