{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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

unit CnSM9;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SM9 基于椭圆曲线双线性映射的身份认证算法单元
* 单元作者：刘啸
* 备    注：参考了 GmSSL/PBC/Federico2014 源码。
*           二次、四次、十二次扩域分别有 U V W 乘法操作
*           仿射坐标系/雅可比坐标系里的三元点也有加、乘、求反、Frobenius 等操作
*           并基于以上实现了基于 SM9 的 BN 曲线参数的基本 R-ate 计算
*           注意 Miller 算法是定义在 F(q^k) 扩域上的椭圆曲线中的，因而一个元素是 k 维向量
*           Miller 算法计算的现实意义是什么？
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.04.04 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Consts, SysConst,
  CnContainers, CnBigNumber, CnECC, CnNativeDecl;

type
  TCnFP2 = class
  {* 二次扩域大整系数元素实现类}
  private
    F0: TCnBigNumber;
    F1: TCnBigNumber;
    function GetItems(Index: Integer): TCnBigNumber;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    function IsZero: Boolean;
    function IsOne: Boolean;
    function SetZero: Boolean;
    function SetOne: Boolean;
    function SetU: Boolean;
    function SetBigNumber(const Num: TCnBigNumber): Boolean;
    function SetHex(const S0, S1: string): Boolean;
    function SetWord(Value: Cardinal): Boolean;
    function SetWords(Value0, Value1: Cardinal): Boolean;

    property Items[Index: Integer]: TCnBigNumber read GetItems; default;
  end;

  TCnFP2Pool = class(TCnMathObjectPool)
  {* 二次扩域大整系数元素池实现类，允许使用到二次扩域大整系数元素的地方自行创建池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnFP2; reintroduce;
    procedure Recycle(Num: TCnFP2); reintroduce;
  end;

  TCnFP4 = class
  {* 四次扩域大整系数元素实现类}
  private
    F0: TCnFP2;
    F1: TCnFP2;
    function GetItems(Index: Integer): TCnFP2;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    function IsZero: Boolean;
    function IsOne: Boolean;
    function SetZero: Boolean;
    function SetOne: Boolean;
    function SetU: Boolean;
    function SetV: Boolean;
    function SetBigNumber(const Num: TCnBigNumber): Boolean;
    function SetBigNumbers(const Num0, Num1: TCnBigNumber): Boolean;
    function SetHex(const S0, S1, S2, S3: string): Boolean;
    function SetWord(Value: Cardinal): Boolean;
    function SetWords(Value0, Value1, Value2, Value3: Cardinal): Boolean;

    property Items[Index: Integer]: TCnFP2 read GetItems; default;
  end;

  TCnFP4Pool = class(TCnMathObjectPool)
  {* 四次扩域大整系数元素池实现类，允许使用到四次扩域大整系数元素的地方自行创建池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnFP4; reintroduce;
    procedure Recycle(Num: TCnFP4); reintroduce;
  end;

  TCnFP12 = class
  {* 十二次扩域大整系数元素实现类}
  private
    F0: TCnFP4;
    F1: TCnFP4;
    F2: TCnFP4;
    function GetItems(Index: Integer): TCnFP4;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    function IsZero: Boolean;
    function IsOne: Boolean;
    function SetZero: Boolean;
    function SetOne: Boolean;
    function SetU: Boolean;
    function SetV: Boolean;
    function SetW: Boolean;
    function SetWSqr: Boolean;
    function SetBigNumber(const Num: TCnBigNumber): Boolean;
    function SetBigNumbers(const Num0, Num1, Num2: TCnBigNumber): Boolean;
    function SetHex(const S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11: string): Boolean;
      
    function SetWord(Value: Cardinal): Boolean;
    function SetWords(Value0, Value1, Value2, Value3, Value4, Value5, Value6,
      Value7, Value8, Value9, Value10, Value11: Cardinal): Boolean;

    property Items[Index: Integer]: TCnFP4 read GetItems; default;
  end;

  TCnFP12Pool = class(TCnMathObjectPool)
  {* 十二次扩域大整系数元素池实现类，允许使用到四次扩域大整系数元素的地方自行创建池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnFP12; reintroduce;
    procedure Recycle(Num: TCnFP12); reintroduce;
  end;

  TCnAffinePoint = class
  {* 仿射坐标系里的平面点，由三个坐标组成}
  private
    FY: TCnFP2;
    FX: TCnFP2;
    FZ: TCnFP2;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 转换为字符串}
    procedure SetZero;
    {* 设置为全 0，似乎没啥用}
    function IsAtInfinity: Boolean;
    {* 是否位于无限远处}
    function SetToInfinity: Boolean;
    {* 坐标设为无限远}
    function GetCoordinatesFP2(const FP2X, FP2Y: TCnFP2): Boolean;
    {* 获取 XY 坐标值，内部采用复制}
    function SetCoordinatesFP2(const FP2X, FP2Y: TCnFP2): Boolean;
    {* 设置 XY 坐标值，内部采用复制}
    function SetCoordinatesHex(const SX0, SX1, SY0, SY1: string): Boolean;
    {* 设置 XY 坐标值，使用十六进制字符串}
    function SetCoordinatesBigNumbers(const X0, X1, Y0, Y1: TCnBigNumber): Boolean;
    {* 设置 XY 坐标值，使用大数对象，内部采用复制}
    function GetExtCoordinatesFP12(const FP12X, FP12Y: TCnFP12; Prime: TCnBigNumber): Boolean;
    {* 获取扩展 XY 坐标值，内部采用复制}
    function SetExtCoordinatesFP12(const FP12X, FP12Y: TCnFP12; Prime: TCnBigNumber): Boolean;
    {* 设置扩展 XY 坐标值，内部采用复制}
    function IsOnCurve(Prime: TCnBigNumber): Boolean;
    {* 判断是否在椭圆曲线 y^2 = x^3 + 5 上}

    property X: TCnFP2 read FX;
    property Y: TCnFP2 read FY;
    property Z: TCnFP2 read FZ;
  end;

  TCnAffinePointPool = class(TCnMathObjectPool)
  {* 仿射坐标系里的平面点池实现类，允许使用到仿射坐标系里的平面点的地方自行创建池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnAffinePoint; reintroduce;
    procedure Recycle(Num: TCnAffinePoint); reintroduce;
  end;

  TCnSM9 = class
  private
    FEcc: TCnEcc;
    FP1: TCnEccPoint;
    FP2: TCnEccPoint;
    FT: TCnBigNumber;
  protected
    procedure Init;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

// ====================== 二次扩域大整系数元素运算函数 =========================

function FP2New: TCnFP2;
{* 创建一二次扩域大整系数元素对象，等同于 TCnFP2.Create}

procedure FP2Free(FP2: TCnFP2);
{* 释放一二次扩域大整系数元素对象，等同于 TCnFP2.Free}

function FP2IsZero(FP2: TCnFP2): Boolean;
{* 判断一二次扩域大整系数元素对象是否为 0}

function FP2IsOne(FP2: TCnFP2): Boolean;
{* 判断一二次扩域大整系数元素对象是否为 1}

function FP2SetZero(FP2: TCnFP2): Boolean;
{* 将一二次扩域大整系数元素对象设置为 0}

function FP2SetOne(FP2: TCnFP2): Boolean;
{* 将一二次扩域大整系数元素对象设置为 1，也就是 [0] 为 1，[1] 为 0}

function FP2SetU(FP2: TCnFP2): Boolean;
{* 将一二次扩域大整系数元素对象设为 U，也就是 [0] 为 0，[1] 为 1}

function FP2SetBigNumber(const FP2: TCnFP2; const Num: TCnBigNumber): Boolean;
{* 将一二次扩域大整系数元素对象设置为某一个大数}

function FP2SetBigNumbers(const FP2: TCnFP2; const Num0, Num1: TCnBigNumber): Boolean;
{* 将一二次扩域大整系数元素对象设置为两个大数值}

function FP2SetHex(const FP2: TCnFP2; const S0, S1: string): Boolean;
{* 将一二次扩域大整系数元素对象设置为两个十六进制字符串}

function FP2ToString(const FP2: TCnFP2): string;
{* 将一二次扩域大整系数元素对象转换为字符串}

function FP2SetWord(const FP2: TCnFP2; Value: Cardinal): Boolean;
{* 将一二次扩域大整系数元素对象设置为一个 Cardinal}

function FP2SetWords(const FP2: TCnFP2; Value0, Value1: Cardinal): Boolean;
{* 将一二次扩域大整系数元素对象设置为两个 Cardinal}

function FP2Equal(const F1, F2: TCnFP2): Boolean;
{* 判断两个二次扩域大整系数元素对象值是否相等}

function FP2Copy(const Dst, Src: TCnFP2): TCnFP2;
{* 将一二次扩域大整系数元素对象值复制到另一个二次扩域大整系数元素对象中}

function FP2Negate(const Res: TCnFP2; const F: TCnFP2; Prime: TCnBigNumber): Boolean;
{* 将一二次扩域大整系数元素对象值有限域中求负}

function FP2Add(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
{* 有限域中二次扩域大整系数元素加法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2}

function FP2Sub(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
{* 有限域中二次扩域大整系数元素减法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2}

function FP2Mul(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean; overload;
{* 有限域中二次扩域大整系数元素乘法，Prime 为域素数，Res 不可以是 F1 或 F2，F1 可以是 F2}

function FP2Mul3(const Res: TCnFP2; const F: TCnFP2; Prime: TCnBigNumber): Boolean;
{* 有限域中二次扩域大整系数元素对象乘以 3，Prime 为域素数，Res 可以是 F}

function FP2MulU(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
{* 有限域中二次扩域大整系数元素 U 乘法，Prime 为域素数，Res 不可以是 F1 或 F2，F1 可以是 F2}

function FP2Mul(const Res: TCnFP2; const F: TCnFP2; Num: TCnBigNumber; Prime: TCnBigNumber): Boolean; overload;
{* 有限域中二次扩域大整系数元素与大数的乘法，Prime 为域素数，Res 可以是 F，但 Num 不能是 Res 或 F 中的内容}

function FP2Inverse(const Res: TCnFP2; const F: TCnFP2; Prime: TCnBigNumber): Boolean;
{* 有限域中二次扩域大整系数元素求模反，Prime 为域素数，Res 可以是 F}

function FP2Div(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
{* 有限域中二次扩域大整系数元素除法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2，内部用模反乘法实现}

function FP2ToStream(FP2: TCnFP2; Stream: TStream): Integer;
{* 将一二次扩域大整系数元素对象的内容写入流，返回写入长度}

// ====================== 四次扩域大整系数元素运算函数 =========================

function FP4New: TCnFP4;
{* 创建一四次扩域大整系数元素对象，等同于 TCnFP4.Create}

procedure FP4Free(FP4: TCnFP4);
{* 释放一四次扩域大整系数元素对象，等同于 TCnFP4.Free}

function FP4IsZero(FP4: TCnFP4): Boolean;
{* 判断一四次扩域大整系数元素对象是否为 0}

function FP4IsOne(FP4: TCnFP4): Boolean;
{* 判断一四次扩域大整系数元素对象是否为 1}

function FP4SetZero(FP4: TCnFP4): Boolean;
{* 将一四次扩域大整系数元素对象设置为 0}

function FP4SetOne(FP4: TCnFP4): Boolean;
{* 将一四次扩域大整系数元素对象设置为 1，也就是 [0] 为 1，[1] 为 0}

function FP4SetU(FP4: TCnFP4): Boolean;
{* 将一四次扩域大整系数元素对象设为 U，也就是 [0] 为 U，[1] 为 0}

function FP4SetV(FP4: TCnFP4): Boolean;
{* 将一四次扩域大整系数元素对象设为 V，也就是 [0] 为 0，[1] 为 1}

function FP4SetBigNumber(const FP4: TCnFP4; const Num: TCnBigNumber): Boolean;
{* 将一四次扩域大整系数元素对象设置为某一个大数}

function FP4SetBigNumbers(const FP4: TCnFP4; const Num0, Num1: TCnBigNumber): Boolean;
{* 将一四次扩域大整系数元素对象设置为两个大数值}

function FP4SetFP2(const FP4: TCnFP4; const FP2: TCnFP2): Boolean;
{* 将一四次扩域大整系数元素对象设置为一个二次扩域大整系数元素}

function FP4Set2FP2S(const FP4: TCnFP4; const FP20, FP21: TCnFP2): Boolean;
{* 将一四次扩域大整系数元素对象设置为两个二次扩域大整系数元素}

function FP4SetHex(const FP4: TCnFP4; const S0, S1, S2, S3: string): Boolean;
{* 将一四次扩域大整系数元素对象设置为四个十六进制字符串}

function FP4ToString(const FP4: TCnFP4): string;
{* 将一四次扩域大整系数元素对象转换为字符串}

function FP4SetWord(const FP4: TCnFP4; Value: Cardinal): Boolean;
{* 将一四次扩域大整系数元素对象设置为一个 Cardinal}

function FP4SetWords(const FP4: TCnFP4; Value0, Value1, Value2, Value3: Cardinal): Boolean;
{* 将一四次扩域大整系数元素对象设置为四个 Cardinal}

function FP4Equal(const F1, F2: TCnFP4): Boolean;
{* 判断两个四次扩域大整系数元素对象值是否相等}

function FP4Copy(const Dst, Src: TCnFP4): TCnFP4;
{* 将一四次扩域大整系数元素对象值复制到另一个四次扩域大整系数元素对象中}

function FP4Negate(const Res: TCnFP4; const F: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 将一四次扩域大整系数元素对象值有限域中求负}

function FP4Add(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 有限域中四次扩域大整系数元素加法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2}

function FP4Sub(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 有限域中四次扩域大整系数元素减法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2}

function FP4Mul(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 有限域中四次扩域大整系数元素乘法，Prime 为域素数，Res 不可以是 F1 或 F2，F1 可以是 F2}

function FP4Mul3(const Res: TCnFP4; const F: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 有限域中四次扩域大整系数元素对象乘以 3，Prime 为域素数，Res 可以是 F}

function FP4MulV(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 有限域中四次扩域大整系数元素 V 乘法，Prime 为域素数，Res 不可以是 F1 或 F2，F1 可以是 F2}

function FP4Inverse(const Res: TCnFP4; const F: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 有限域中四次扩域大整系数元素求模反，Prime 为域素数，Res 可以是 F}

function FP4Div(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
{* 有限域中四次扩域大整系数元素除法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2，内部用模反乘法实现}

function FP4ToStream(FP4: TCnFP4; Stream: TStream): Integer;
{* 将一四次扩域大整系数元素对象的内容写入流，返回写入长度}

// ===================== 十二次扩域大整系数元素运算函数 ========================

function FP12New: TCnFP12;
{* 创建一十二次扩域大整系数元素对象，等同于 TCnFP12.Create}

procedure FP12Free(FP12: TCnFP12);
{* 释放一十二次扩域大整系数元素对象，等同于 TCnFP12.Free}

function FP12IsZero(FP12: TCnFP12): Boolean;
{* 判断一十二次扩域大整系数元素对象是否为 0}

function FP12IsOne(FP12: TCnFP12): Boolean;
{* 判断一十二次扩域大整系数元素对象是否为 1}

function FP12SetZero(FP12: TCnFP12): Boolean;
{* 将一十二次扩域大整系数元素对象设置为 0}

function FP12SetOne(FP12: TCnFP12): Boolean;
{* 将一十二次扩域大整系数元素对象设置为 1}

function FP12SetU(FP12: TCnFP12): Boolean;
{* 将一十二次扩域大整系数元素对象设为 U，也就是仨 FP4 分别 U、0、0}

function FP12SetV(FP12: TCnFP12): Boolean;
{* 将一十二次扩域大整系数元素对象设为 V，也就是仨 FP4 分别 V、0、0}

function FP12SetW(FP12: TCnFP12): Boolean;
{* 将一十二次扩域大整系数元素对象设为 W，也就是仨 FP4 分别 0、1、0}

function FP12SetWSqr(FP12: TCnFP12): Boolean;
{* 将一十二次扩域大整系数元素对象设为 W^2，也就是仨 FP4 分别 0、0、1}

function FP12SetBigNumber(const FP12: TCnFP12; const Num: TCnBigNumber): Boolean;
{* 将一十二次扩域大整系数元素对象设置为某一个大数}

function FP12SetBigNumbers(const FP12: TCnFP12; const Num0, Num1, Num2: TCnBigNumber): Boolean;
{* 将一十二次扩域大整系数元素对象设置为三个大数值}

function FP12SetFP4(const FP12: TCnFP12; const FP4: TCnFP4): Boolean;
{* 将一十二次扩域大整系数元素对象设置为一个四次扩域大整系数元素}

function FP12Set3FP4S(const FP12: TCnFP12; const FP40, FP41, FP42: TCnFP4): Boolean;
{* 将一十二次扩域大整系数元素对象设置为三个四次扩域大整系数元素}

function FP12SetFP2(const FP12: TCnFP12; const FP2: TCnFP2): Boolean;
{* 将一十二次扩域大整系数元素对象设置为一个二次扩域大整系数元素}

function FP12SetHex(const FP12: TCnFP12; const S0, S1, S2, S3, S4, S5, S6, S7, S8,
  S9, S10, S11: string): Boolean;
{* 将一十二次扩域大整系数元素对象设置为十二个十六进制字符串}

function FP12ToString(const FP12: TCnFP12): string;
{* 将一十二次扩域大整系数元素对象转换为字符串}

function FP12SetWord(const FP12: TCnFP12; Value: Cardinal): Boolean;
{* 将一十二次扩域大整系数元素对象设置为一个 Cardinal}

function FP12SetWords(const FP12: TCnFP12; Value0, Value1, Value2, Value3, Value4,
  Value5, Value6, Value7, Value8, Value9, Value10, Value11: Cardinal): Boolean;
{* 将一十二次扩域大整系数元素对象设置为十二个 Cardinal}

function FP12Equal(const F1, F2: TCnFP12): Boolean;
{* 判断两个十二次扩域大整系数元素对象值是否相等}

function FP12Copy(const Dst, Src: TCnFP12): TCnFP12;
{* 将一十二次扩域大整系数元素对象值复制到另一个十二次扩域大整系数元素对象中}

function FP12Negate(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 将一十二次扩域大整系数元素对象值有限域中求负}

function FP12Add(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 有限域中十二次扩域大整系数元素加法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2}

function FP12Sub(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 有限域中十二次扩域大整系数元素减法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2}

function FP12Mul(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 有限域中十二次扩域大整系数元素乘法，Prime 为域素数，Res 不可以是 F1 或 F2，F1 可以是 F2}

function FP12Mul3(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 有限域中十二次扩域大整系数元素对象乘以 3，Prime 为域素数，Res 可以是 F}

function FP12Inverse(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 有限域中十二次扩域大整系数元素求模反，Prime 为域素数，Res 可以是 F}

function FP12Div(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 有限域中十二次扩域大整系数元素除法，Prime 为域素数，Res 可以是 F1、F2，F1 可以是 F2，内部用模反乘法实现}

function FP12Power(const Res: TCnFP12; const F: TCnFP12; Exponent: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 有限域中十二次扩域大整系数元素乘方，Prime 为域素数，Res 可以是 F，暂未测试}

function FP12ToStream(FP12: TCnFP12; Stream: TStream): Integer;
{* 将一十二次扩域大整系数元素对象的内容写入流，返回写入长度}

// ===================== 仿射坐标系里的三元点的运算函数 ========================

function AffinePointNew: TCnAffinePoint;
{* 创建一仿射坐标系里的三元点对象，等同于 TCnAffinePoint.Create}

procedure AffinePointFree(P: TCnAffinePoint);
{* 释放一仿射坐标系里的三元点对象，等同于 TCnAffinePoint.Free}

function AffinePointSetZero(P: TCnAffinePoint): Boolean;
{* 将一个仿射坐标系里的三元点坐标设置为全 0}

function AffinePointToString(const P: TCnAffinePoint): string;
{* 将一仿射坐标系里的三元点对象转换为字符串}

function AffinePointEqual(const P1, P2: TCnAffinePoint): Boolean;
{* 判断两个仿射坐标系里的三元点对象值是否相等}

function AffinePointCopy(const Dst, Src: TCnAffinePoint): TCnAffinePoint;
{* 将一仿射坐标系里的三元点对象值复制到另一个仿射坐标系里的三元点对象中}

function AffinePointIsAtInfinity(const P: TCnAffinePoint): Boolean;
{* 判断一仿射坐标系里的三元点对象是否位于无限远处}

function AffinePointSetToInfinity(const P: TCnAffinePoint): Boolean;
{* 将一仿射坐标系里的三元点对象坐标设为无限远}

function AffinePointGetCoordinatesFP2(const P: TCnAffinePoint; const FP2X, FP2Y: TCnFP2): Boolean;
{* 获取一仿射坐标系里的三元点对象的 XY 坐标值，内部采用复制}

function AffinePointSetCoordinatesFP2(const P: TCnAffinePoint; const FP2X, FP2Y: TCnFP2): Boolean;
{* 设置一仿射坐标系里的三元点对象的 XY 坐标值，内部采用复制}

function AffinePointSetCoordinatesHex(const P: TCnAffinePoint;
  const SX0, SX1, SY0, SY1: string): Boolean;
{* 设置一仿射坐标系里的三元点对象的 XY 坐标值，使用十六进制字符串}

function AffinePointSetCoordinatesBigNumbers(const P: TCnAffinePoint;
  const X0, X1, Y0, Y1: TCnBigNumber): Boolean;
{* 设置一仿射坐标系里的三元点对象的 XY 坐标值，使用大数对象，内部采用复制}

function AffinePointGetJacobianCoordinatesFP12(const P: TCnAffinePoint;
  const FP12X, FP12Y: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 获取一仿射坐标系里的三元点对象的雅可比 XY 坐标值，内部采用复制}

function AffinePointSetJacobianCoordinatesFP12(const P: TCnAffinePoint;
  const FP12X, FP12Y: TCnFP12; Prime: TCnBigNumber): Boolean;
{* 设置一仿射坐标系里的三元点对象的雅可比 XY 坐标值，内部采用复制}

function AffinePointIsOnCurve(const P: TCnAffinePoint; Prime: TCnBigNumber): Boolean;
{* 判断一仿射坐标系里的三元点对象是否在椭圆曲线 y^2 = x^3 + 5 上}

function AffinePointNegate(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
{* 一个仿射坐标系里的三元点对象的椭圆曲线求反，Res 可以是 P}

function AffinePointDouble(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
{* 一个仿射坐标系里的三元点对象的椭圆曲线倍点法，Res 可以是 P}

function AffinePointAdd(const Res: TCnAffinePoint; const P, Q: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
{* 两个仿射坐标系里的三元点对象的椭圆曲线加法，Res 可以是 P 或 Q，P 可以是 Q}

function AffinePointSub(const Res: TCnAffinePoint; const P, Q: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
{* 两个仿射坐标系里的三元点对象的椭圆曲线减法，Res 可以是 P 或 Q，P 可以是 Q}

function AffinePointMul(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Num: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 一个仿射坐标系里的三元点对象的椭圆曲线 N 倍点法，Res 可以是 P}

function AffinePointFrobenius(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
{* 计算一个仿射坐标系里的三元点对象的弗罗贝尼乌斯自同态值，Res 可以是 P
  其实就是 P 的 Prime 次方的结果 mod Prime}

// ============================ 双线性对计算函数 ===============================

function Rate(const F: TCnFP12; const Q: TCnAffinePoint; const XP, YP: TCnBigNumber;
  const A: TCnBigNumber; const K: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 计算 R-ate 对。输出是一个 FP12 值，输入是一个 BN 曲线上的点的坐标 XP、YP，
  一个 FP2 上的 XYZ 仿射坐标点，一个指数 K、一个循环次数 A}

function SM9RatePairing(const F: TCnFP12; const Q: TCnAffinePoint; const P: TCnEccPoint): Boolean;
{* 根据 SM9 指定的 BN 曲线的参数以及指定点计算 R-ate 对，输入为一个 BN 曲线上的点
  一个 FP2 上的 XYZ 仿射坐标点，输出为一个 FP12 值}

implementation

const
  CRLF = #13#10;

  // 一个参数 T，不知道叫啥，但 SM9 所选择的 BN 曲线里，
  // 基域特征、阶和弗罗贝尼乌斯自同态映射的迹均是 T 的指定的多项式表达式
  CN_SM9_T = '600000000058F98A';

  // SM9 椭圆曲线方程的 A 系数值
  CN_SM9_ECC_A = 0;

  // SM9 椭圆曲线方程的 B 系数值
  CN_SM9_ECC_B = 5;

  // SM9 椭圆曲线的素数域，也叫基域特征，在这里等于 36t^4 + 36t^3 + 24t^2 + 6t + 1：
  CN_SM9_FINITE_FIELD = 'B640000002A3A6F1D603AB4FF58EC74521F2934B1A7AEEDBE56F9B27E351457D';

  // SM9 椭圆曲线的阶，也就是总点数，在这里等于 36t^4 + 36t^3 + 18t^2 + 6t + 1：
  // （貌似叫 N，要乘以 cf 才能叫 Order，但这里 cf = 1，所以 N 和 Order 等价）
  CN_SM9_ORDER = 'B640000002A3A6F1D603AB4FF58EC74449F2934B18EA8BEEE56EE19CD69ECF25';

  // SM9 椭圆曲线的余因子，乘以 N 就得到阶
  CN_SM9_CF = 1;

  // SM9 椭圆曲线的嵌入次数，也就是 Prime 的最小嵌入次数次方对 Order 求模为 1
  CN_SM9_K = 12;

  // 弗罗贝尼乌斯自同态映射的迹，也就是 Hasse 定理中的 阶=q+1-trace 中的 trace
  // 在 SM9 椭圆曲线中等于 6t^2 + 1
  CN_SM9_FROBENIUS_TRACE = 'D8000000019062ED0000B98B0CB27659';

  // G1 生成元的单坐标
  CN_SM9_G1_P1X = '93DE051D62BF718FF5ED0704487D01D6E1E4086909DC3280E8C4E4817C66DDDD';
  CN_SM9_G1_P1Y = '21FE8DDA4F21E607631065125C395BBC1C1C00CBFA6024350C464CD70A3EA616';

  // G2 生成元的双坐标
  CN_SM9_G2_P2X0 = '85AEF3D078640C98597B6027B441A01FF1DD2C190F5E93C454806C11D8806141';
  CN_SM9_G2_P2X1 = '3722755292130B08D2AAB97FD34EC120EE265948D19C17ABF9B7213BAF82D65B';
  CN_SM9_G2_P2Y0 = '17509B092E845C1266BA0D262CBEE6ED0736A96FA347C8BD856DC76B84EBEB96';
  CN_SM9_G2_P2Y1 = 'A7CF28D519BE3DA65F3170153D278FF247EFBA98A71A08116215BBA5C999A7C7';

  // R-ate 对的计算参数，其实就是 6T + 2
  CN_SM9_6T_PLUS_2 = '02400000000215D93E';

  CN_SM9_FAST_EXP_P3 = '5C5E452404034E2AF12FCAD3B31FE2B0D62CD8FB7B497A0ADC53E586930846F1' +
    'BA4CADE09029E4717C0CA02D9B0D8649A5782C82FDB6B0A10DA3D71BCDB13FE5E0D49DE3AA8A4748' +
    '83687EE0C6D9188C44BF9D0FA74DDFB7A9B2ADA593152855';

  CN_SM9_FAST_EXP_PW20 = 'F300000002A3A6F2780272354F8B78F4D5FC11967BE65334';
  CN_SM9_FAST_EXP_PW21 = 'B640000002A3A6F0E303AB4FF2EB2052A9F02115CAEF75E70F738991676AF249';
  CN_SM9_FAST_EXP_PW22 = 'F300000002A3A6F2780272354F8B78F4D5FC11967BE65333';
  CN_SM9_FAST_EXP_PW23 = 'B640000002A3A6F0E303AB4FF2EB2052A9F02115CAEF75E70F738991676AF24A';

var
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalFP2Pool: TCnFP2Pool = nil;
  FLocalFP4Pool: TCnFP4Pool = nil;
  FLocalFP12Pool: TCnFP12Pool = nil;
  FLocalAffinePointPool: TCnAffinePointPool = nil;

  // SM9 运算的相关常数
  FSM9FiniteFieldSize: TCnBigNumber = nil;
  FSM9Order: TCnBigNumber = nil;
  FSM9K: Integer = 12;
  FSM9G1P1X: TCnBigNumber = nil;
  FSM9G1P1Y: TCnBigNumber = nil;
  FSM9G2P2X0: TCnBigNumber = nil;
  FSM9G2P2X1: TCnBigNumber = nil;
  FSM9G2P2Y0: TCnBigNumber = nil;
  FSM9G2P2Y1: TCnBigNumber = nil;
  FSM96TPlus2: TCnBigNumber = nil;
  FSM9FastExpP3: TCnBigNumber = nil;
  FFP12FastExpPW20: TCnBigNumber = nil;
  FFP12FastExpPW21: TCnBigNumber = nil;
  FFP12FastExpPW22: TCnBigNumber = nil;
  FFP12FastExpPW23: TCnBigNumber = nil;

{ TCnSM9 }

constructor TCnSM9.Create;
begin
  inherited;
  FEcc := TCnEcc.Create;
  FP1 := TCnEccPoint.Create;
  FP2 := TCnEccPoint.Create;
  FT := TCnBigNumber.Create;

  Init;
end;

destructor TCnSM9.Destroy;
begin
  FT.Free;
  FP2.Free;
  FP1.Free;
  FEcc.Free;
  inherited;
end;

procedure TCnSM9.Init;
begin
  FEcc.Load('00', '05', 'B640000002A3A6F1D603AB4FF58EC74521F2934B1A7AEEDBE56F9B27E351457D',
    '', '', 'B640000002A3A6F1D603AB4FF58EC74449F2934B18EA8BEEE56EE19CD69ECF25', 1);

  FT.SetHex('600000000058F98A');

  FP1.X.SetHex('93DE051D 62BF718F F5ED0704 487D01D6 E1E40869 09DC3280 E8C4E481 7C66DDDD');
  FP1.Y.SetHex('21FE8DDA 4F21E607 63106512 5C395BBC 1C1C00CB FA602435 0C464CD7 0A3EA616');
  FP2.X.SetHex('');
  FP2.Y.SetHex('');
end;

// ====================== 二次扩域大整系数元素运算函数 =========================

function FP2New: TCnFP2;
begin
  Result := TCnFP2.Create;
end;

procedure FP2Free(FP2: TCnFP2);
begin
  FP2.Free;
end;

function FP2IsZero(FP2: TCnFP2): Boolean;
begin
  Result := FP2[0].IsZero and FP2[1].IsZero;
end;

function FP2IsOne(FP2: TCnFP2): Boolean;
begin
  Result := FP2[0].IsOne and FP2[1].IsZero;
end;

function FP2SetZero(FP2: TCnFP2): Boolean;
begin
  Result := False;
  if not FP2[0].SetZero then Exit;
  if not FP2[1].SetZero then Exit;
  Result := True;
end;

function FP2SetOne(FP2: TCnFP2): Boolean;
begin
  Result := False;
  if not FP2[0].SetOne then Exit;
  if not FP2[1].SetZero then Exit;
  Result := True;
end;

function FP2SetU(FP2: TCnFP2): Boolean;
begin
  Result := False;
  if not FP2[0].SetZero then Exit;
  if not FP2[1].SetOne then Exit;
  Result := True;
end;

function FP2SetBigNumber(const FP2: TCnFP2; const Num: TCnBigNumber): Boolean;
begin
  Result := False;
  if BigNumberCopy(FP2[0], Num) = nil then Exit;
  if not FP2[1].SetZero then Exit;
  Result := True;
end;

function FP2SetBigNumbers(const FP2: TCnFP2; const Num0, Num1: TCnBigNumber): Boolean;
begin
  Result := False;
  if BigNumberCopy(FP2[0], Num0) = nil then Exit;
  if BigNumberCopy(FP2[1], Num1) = nil then Exit;
  Result := True;
end;

function FP2SetHex(const FP2: TCnFP2; const S0, S1: string): Boolean;
begin
  Result := False;
  if not FP2[0].SetHex(S0) then Exit;
  if not FP2[1].SetHex(S1) then Exit;
  Result := True;
end;

function FP2ToString(const FP2: TCnFP2): string;
begin
  Result := FP2[1].ToHex + ',' + FP2[0].ToHex;
end;

function FP2SetWord(const FP2: TCnFP2; Value: Cardinal): Boolean;
begin
  Result := False;
  if not FP2[0].SetWord(Value) then Exit;
  if not FP2[1].SetZero then Exit;
  Result := True;
end;

function FP2SetWords(const FP2: TCnFP2; Value0, Value1: Cardinal): Boolean;
begin
  Result := False;
  if not FP2[0].SetWord(Value0) then Exit;
  if not FP2[1].SetWord(Value1) then Exit;
  Result := True;
end;

function FP2Equal(const F1, F2: TCnFP2): Boolean;
begin
  Result := BigNumberEqual(F1[0], F2[0]) and BigNumberEqual(F1[1], F2[1]);
end;

function FP2Copy(const Dst, Src: TCnFP2): TCnFP2;
begin
  Result := nil;
  if BigNumberCopy(Dst[0], Src[0]) = nil then Exit;
  if BigNumberCopy(Dst[1], Src[1]) = nil then Exit;
  Result := Dst;
end;

function FP2Negate(const Res: TCnFP2; const F: TCnFP2; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not BigNumberSub(Res[0], Prime, F[0]) then Exit;
  if not BigNumberSub(Res[1], Prime, F[1]) then Exit;
  if not BigNumberNonNegativeMod(Res[0], Res[0], Prime) then Exit;
  if not BigNumberNonNegativeMod(Res[1], Res[1], Prime) then Exit;
  Result := True;
end;

function FP2Add(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not BigNumberAdd(Res[0], F1[0], F2[0]) then Exit;
  if not BigNumberAdd(Res[1], F1[1], F2[1]) then Exit;
  if not BigNumberNonNegativeMod(Res[0], Res[0], Prime) then Exit;
  if not BigNumberNonNegativeMod(Res[1], Res[1], Prime) then Exit;
  Result := True;
end;

function FP2Sub(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not BigNumberSub(Res[0], F1[0], F2[0]) then Exit;
  if not BigNumberSub(Res[1], F1[1], F2[1]) then Exit;
  if not BigNumberNonNegativeMod(Res[0], Res[0], Prime) then Exit;
  if not BigNumberNonNegativeMod(Res[1], Res[1], Prime) then Exit;
  Result := True;
end;

function FP2Mul(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
var
  T0, T1, R0: TCnBigNumber;
begin
  Result := False;

  // r0 = a0 * b0 - 2 * a1 * b1
  // r1 = a0 * b1 + a1 * b0
  T0 := nil;
  T1 := nil;
  R0 := nil;

  try
    T0 := FLocalBigNumberPool.Obtain;
    T1 := FLocalBigNumberPool.Obtain;
    R0 := FLocalBigNumberPool.Obtain;

    if not BigNumberMul(T0, F1[0], F2[0]) then Exit;
    if not BigNumberMul(T1, F1[1], F2[1]) then Exit;
    if not BigNumberAdd(T1, T1, T1) then Exit;
    if not BigNumberSub(T0, T0, T1) then Exit;
    if not BigNumberNonNegativeMod(R0, T0, Prime) then Exit; // 不能直接给 Res[0] 赋值，万一 F1 和 Res 相同则会提前影响 F0

    if not BigNumberMul(T0, F1[0], F2[1]) then Exit;
    if not BigNumberMul(T1, F1[1], F2[0]) then Exit;
    if not BigNumberAdd(T1, T0, T1) then Exit;
    if not BigNumberNonNegativeMod(Res[1], T1, Prime) then Exit;

    if BigNumberCopy(Res[0], R0) = nil then Exit;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(R0);
    FLocalBigNumberPool.Recycle(T1);
    FLocalBigNumberPool.Recycle(T0);
  end;
end;

function FP2Mul3(const Res: TCnFP2; const F: TCnFP2; Prime: TCnBigNumber): Boolean;
var
  T: TCnFP2;
begin
  Result := False;
  T := FLocalFP2Pool.Obtain;
  try
    if not FP2Add(T, F, F, Prime) then Exit;
    if not FP2Add(Res, T, F, Prime) then Exit;
    Result := True;
  finally
    FLocalFP2Pool.Recycle(T);
  end;
end;

function FP2MulU(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
var
  T0, T1: TCnBigNumber;
begin
  Result := False;

  // r0 = -2 * (a0 * b1 + a1 * b0)
  // r1 = a0 * b0 - 2 * a1 * b1
  T0 := nil;
  T1 := nil;
  try
    T0 := FLocalBigNumberPool.Obtain;
    T1 := FLocalBigNumberPool.Obtain;

    if not BigNumberMul(T0, F1[0], F2[1]) then Exit;
    if not BigNumberMul(T1, F1[1], F2[0]) then Exit;
    if not BigNumberAdd(T0, T0, T1) then Exit;
    if not T0.MulWord(2) then Exit;
    T0.Negate;
    if not BigNumberNonNegativeMod(Res[0], T0, Prime) then Exit;

    if not BigNumberMul(T0, F1[0], F2[0]) then Exit;
    if not BigNumberMul(T1, F1[1], F2[1]) then Exit;
    if not T1.MulWord(2) then Exit;
    if not BigNumberSub(T1, T0, T1) then Exit;
    if not BigNumberNonNegativeMod(Res[1], T1, Prime) then Exit;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(T1);
    FLocalBigNumberPool.Recycle(T0);
  end;
end;

function FP2Mul(const Res: TCnFP2; const F: TCnFP2; Num: TCnBigNumber; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not BigNumberMul(Res[0], F[0], Num) then Exit;
  if not BigNumberMul(Res[1], F[1], Num) then Exit;
  if not BigNumberNonNegativeMod(Res[0], Res[0], Prime) then Exit;
  if not BigNumberNonNegativeMod(Res[1], Res[1], Prime) then Exit;
  Result := True;
end;

function FP2Inverse(const Res: TCnFP2; const F: TCnFP2; Prime: TCnBigNumber): Boolean;
var
  K, T: TCnBigNumber;
begin
  Result := False;
  if F[0].IsZero then
  begin
    if not Res[0].SetZero then Exit;
    // r1 = -((2 * a1)^-1) */
    if not BigNumberAdd(Res[1], F[1], F[1]) then Exit;
    BigNumberModularInverse(Res[1], Res[1], Prime);
    if not BigNumberNonNegativeMod(Res[1], Res[1], Prime) then Exit;
    if not BigNumberSub(Res[1], Prime, Res[1]) then Exit;
    Result := True;
  end
  else if F[1].IsZero then
  begin
    if not Res[1].SetZero then Exit;
    // r0 = a0^-1
    BigNumberModularInverse(Res[0], F[0], Prime);
    Result := True;
  end
  else
  begin
    // k = (a[0]^2 + 2 * a[1]^2)^-1
    // r[0] = a[0] * k
    // r[1] = -a[1] * k
    K := nil;
    T := nil;
    try
      K := FLocalBigNumberPool.Obtain;
      T := FLocalBigNumberPool.Obtain;

      if not BigNumberMul(T, F[1], F[1]) then Exit;
      if not T.MulWord(2) then Exit;
      if not BigNumberMul(K, F[0], F[0]) then Exit;
      if not BigNumberAdd(K, T, K) then Exit;
      BigNumberModularInverse(K, K, Prime);

      if not BigNumberMul(Res[0], F[0], K) then Exit;
      if not BigNumberNonNegativeMod(Res[0], Res[0], Prime) then Exit;

      if not BigNumberMul(Res[1], F[1], K) then Exit;
      if not BigNumberNonNegativeMod(Res[1], Res[1], Prime) then Exit;
      if not BigNumberSub(Res[1], Prime, Res[1]) then Exit;
      Result := True;
    finally
      FLocalBigNumberPool.Recycle(T);
      FLocalBigNumberPool.Recycle(K);
    end;
  end;
end;

function FP2Div(const Res: TCnFP2; const F1, F2: TCnFP2; Prime: TCnBigNumber): Boolean;
var
  Inv: TCnFP2;
begin
  Result := False;
  if F2.IsZero then
    raise EZeroDivide.Create(SDivByZero);

  if F1 = F2 then
  begin
    if not Res.SetOne then Exit;
    Result := True;
  end
  else
  begin
    Inv := FLocalFP2Pool.Obtain;
    try
      if not FP2Inverse(Inv, F2, Prime) then Exit;
      if not FP2Mul(Res, F1, Inv, Prime) then Exit;
      Result := True;
    finally
      FLocalFP2Pool.Recycle(Inv);
    end;
  end;
end;

function FP2ToStream(FP2: TCnFP2; Stream: TStream): Integer;
begin
  Result := BigNumberWriteBinaryToStream(FP2[0], Stream) + BigNumberWriteBinaryToStream(FP2[1], Stream);
end;

// ====================== 四次扩域大整系数元素运算函数 =========================

function FP4New: TCnFP4;
begin
  Result := TCnFP4.Create;
end;

procedure FP4Free(FP4: TCnFP4);
begin
  FP4.Free;
end;

function FP4IsZero(FP4: TCnFP4): Boolean;
begin
  Result := FP4[0].IsZero and FP4[1].IsZero;
end;

function FP4IsOne(FP4: TCnFP4): Boolean;
begin
  Result := FP4[0].IsOne and FP4[1].IsZero;
end;

function FP4SetZero(FP4: TCnFP4): Boolean;
begin
  Result := False;
  if not FP4[0].SetZero then Exit;
  if not FP4[1].SetZero then Exit;
  Result := True;
end;

function FP4SetOne(FP4: TCnFP4): Boolean;
begin
  Result := False;
  if not FP4[1].SetZero then Exit;
  if not FP4[0].SetOne then Exit;
  Result := True;
end;

function FP4SetU(FP4: TCnFP4): Boolean;
begin
  Result := False;
  if not FP4[1].SetZero then Exit;
  if not FP4[0].SetU then Exit;
  Result := True;
end;

function FP4SetV(FP4: TCnFP4): Boolean;
begin
  Result := False;
  if not FP4[0].SetZero then Exit;
  if not FP4[1].SetOne then Exit;
  Result := True;
end;

function FP4SetBigNumber(const FP4: TCnFP4; const Num: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP4[1].SetZero then Exit;
  if not FP4[0].SetBigNumber(Num) then Exit;
  Result := True;
end;

function FP4SetBigNumbers(const FP4: TCnFP4; const Num0, Num1: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP4[0].SetBigNumber(Num0) then Exit;
  if not FP4[1].SetBigNumber(Num1) then Exit;
  Result := True;
end;

function FP4SetFP2(const FP4: TCnFP4; const FP2: TCnFP2): Boolean;
begin
  Result := False;
  if not FP4[1].SetZero then Exit;
  if FP2Copy(FP4[0], FP2) = nil then Exit;
  Result := True;
end;

function FP4Set2FP2S(const FP4: TCnFP4; const FP20, FP21: TCnFP2): Boolean;
begin
  Result := False;
  if FP2Copy(FP4[0], FP20) = nil then Exit;
  if FP2Copy(FP4[1], FP21) = nil then Exit;
  Result := True;
end;

function FP4SetHex(const FP4: TCnFP4; const S0, S1, S2, S3: string): Boolean;
begin
  Result := False;
  if not FP4[1].SetHex(S2, S3) then Exit;
  if not FP4[0].SetHex(S0, S1) then Exit;
  Result := True;
end;

function FP4ToString(const FP4: TCnFP4): string;
begin
  Result := FP4[1].ToString + CRLF + FP4[0].ToString;
end;

function FP4SetWord(const FP4: TCnFP4; Value: Cardinal): Boolean;
begin
  Result := False;
  if not FP4[1].SetZero then Exit;
  if not FP4[0].SetWord(Value) then Exit;
  Result := True;
end;

function FP4SetWords(const FP4: TCnFP4; Value0, Value1, Value2, Value3: Cardinal): Boolean;
begin
  Result := False;
  if not FP4[0].SetWords(Value0, Value1) then Exit;
  if not FP4[1].SetWords(Value2, Value3) then Exit;
  Result := True;
end;

function FP4Equal(const F1, F2: TCnFP4): Boolean;
begin
  Result := FP2Equal(F1[0], F2[0]) and FP2Equal(F1[1], F2[1]);
end;

function FP4Copy(const Dst, Src: TCnFP4): TCnFP4;
begin
  Result := nil;
  if FP2Copy(Dst[0], Src[0]) = nil then Exit;
  if FP2Copy(Dst[1], Src[1]) = nil then Exit;
  Result := Dst;
end;

function FP4Negate(const Res: TCnFP4; const F: TCnFP4; Prime: TCnBigNumber): Boolean;
begin
  Result := FP2Negate(Res[0], F[0], Prime) and FP2Negate(Res[1], F[1], Prime);
end;

function FP4Add(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
begin
  Result := FP2Add(Res[0], F1[0], F2[0], Prime) and FP2Add(Res[1], F1[1], F2[1], Prime);
end;

function FP4Sub(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
begin
  Result := FP2Sub(Res[0], F1[0], F2[0], Prime) and FP2Sub(Res[1], F1[1], F2[1], Prime);
end;

function FP4Mul(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
var
  T, R0, R1: TCnFP2;
begin
  Result := False;

  // r0 = a0 * b0 + a1 * b1 * u
  // r1 = a0 * b1 + a1 * b0
  T := nil;
  R0 := nil;
  R1 := nil;

  try
    T := FLocalFP2Pool.Obtain;
    R0 := FLocalFP2Pool.Obtain;
    R1 := FLocalFP2Pool.Obtain;

    if not FP2Mul(R0, F1[0], F2[0], Prime) then Exit;
    if not FP2MulU(T, F1[1], F2[1], Prime) then Exit;
    if not FP2Add(R0, R0, T, Prime) then Exit;

    if not FP2Mul(R1, F1[0], F2[1], Prime) then Exit;
    if not FP2Mul(T, F1[1], F2[0], Prime) then Exit;
    if not FP2Add(Res[1], R1, T, Prime) then Exit;

    if FP2Copy(Res[0], R0) = nil then Exit;
    Result := True;
  finally
    FLocalFP2Pool.Recycle(R1);
    FLocalFP2Pool.Recycle(R0);
    FLocalFP2Pool.Recycle(T);
  end;
end;

function FP4Mul3(const Res: TCnFP4; const F: TCnFP4; Prime: TCnBigNumber): Boolean;
var
  T: TCnFP4;
begin
  Result := False;
  T := FLocalFP4Pool.Obtain;
  try
    if not FP4Add(T, F, F, Prime) then Exit;
    if not FP4Add(Res, T, F, Prime) then Exit;
    Result := True;
  finally
    FLocalFP4Pool.Recycle(T);
  end;
end;

function FP4MulV(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
var
  T, R0, R1: TCnFP2;
begin
  Result := False;

  // r0 = a0 * b1 * u + a1 * b0 * u
  // r1 = a0 * b0 + a1 * b1 * u
  T := nil;
  R0 := nil;
  R1 := nil;

  try
    T := FLocalFP2Pool.Obtain;
    R0 := FLocalFP2Pool.Obtain;
    R1 := FLocalFP2Pool.Obtain;

    if not FP2MulU(R0, F1[0], F2[1], Prime) then Exit;
    if not FP2MulU(T, F1[1], F2[0], Prime) then Exit;
    if not FP2Add(R0, R0, T, Prime) then Exit;

    if not FP2Mul(R1, F1[0], F2[0], Prime) then Exit;
    if not FP2MulU(T, F1[1], F2[1], Prime) then Exit;
    if not FP2Add(Res[1], R1, T, Prime) then Exit;

    if FP2Copy(Res[0], R0) = nil then Exit;
    Result := True;
  finally
    FLocalFP2Pool.Recycle(R1);
    FLocalFP2Pool.Recycle(R0);
    FLocalFP2Pool.Recycle(T);
  end;
end;

function FP4Inverse(const Res: TCnFP4; const F: TCnFP4; Prime: TCnBigNumber): Boolean;
var
  R0, R1, K: TCnFP2;
begin
  Result := False;

  // k = (f1^2 * u - f0^2)^-1
  // r0 = -(f0 * k)
  // r1 = f1 * k
  K := nil;
  R0 := nil;
  R1 := nil;

  try
    K := FLocalFP2Pool.Obtain;
    R0 := FLocalFP2Pool.Obtain;
    R1 := FLocalFP2Pool.Obtain;

    if not FP2MulU(K, F[1], F[1], Prime) then Exit;
    if not FP2Mul(R0, F[0], F[0], Prime) then Exit;
    if not FP2Sub(K, K, R0, Prime) then Exit;
    if not FP2Inverse(K, K, Prime) then Exit;

    if not FP2Mul(R0, F[0], K, Prime) then Exit;
    if not FP2Negate(R0, R0, Prime) then Exit;

    if not FP2Mul(R1, F[1], K, Prime) then Exit;

    if FP2Copy(Res[0], R0) = nil then Exit;
    if FP2Copy(Res[1], R1) = nil then Exit;
    Result := True;
  finally
    FLocalFP2Pool.Recycle(R1);
    FLocalFP2Pool.Recycle(R0);
    FLocalFP2Pool.Recycle(K);
  end;
end;

function FP4Div(const Res: TCnFP4; const F1, F2: TCnFP4; Prime: TCnBigNumber): Boolean;
var
  Inv: TCnFP4;
begin
  Result := False;
  if F2.IsZero then
    raise EZeroDivide.Create(SDivByZero);

  if F1 = F2 then
  begin
    if not Res.SetOne then Exit;
    Result := True;
  end
  else
  begin
    Inv := FLocalFP4Pool.Obtain;
    try
      if not FP4Inverse(Inv, F2, Prime) then Exit;
      if not FP4Mul(Res, F1, Inv, Prime) then Exit;
    finally
      FLocalFP4Pool.Recycle(Inv);
    end;
  end;
end;

function FP4ToStream(FP4: TCnFP4; Stream: TStream): Integer;
begin
  Result := FP2ToStream(FP4[0], Stream) + FP2ToStream(FP4[1], Stream);
end;

// ===================== 十二次扩域大整系数元素运算函数 ========================

function FP12New: TCnFP12;
begin
  Result := TCnFP12.Create;
end;

procedure FP12Free(FP12: TCnFP12);
begin
  FP12.Free;
end;

function FP12IsZero(FP12: TCnFP12): Boolean;
begin
  Result := FP12[0].IsZero and FP12[1].IsZero and FP12[2].IsZero;
end;

function FP12IsOne(FP12: TCnFP12): Boolean;
begin
  Result := FP12[0].IsOne and FP12[1].IsZero and FP12[2].IsZero;
end;

function FP12SetZero(FP12: TCnFP12): Boolean;
begin
  Result := False;
  if not FP12[0].SetZero then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetOne(FP12: TCnFP12): Boolean;
begin
  Result := False;
  if not FP12[0].SetOne then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetU(FP12: TCnFP12): Boolean;
begin
  Result := False;
  if not FP12[0].SetU then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetV(FP12: TCnFP12): Boolean;
begin
  Result := False;
  if not FP12[0].SetV then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetW(FP12: TCnFP12): Boolean;
begin
  Result := False;
  if not FP12[0].SetZero then Exit;
  if not FP12[1].SetOne then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetWSqr(FP12: TCnFP12): Boolean;
begin
  Result := False;
  if not FP12[0].SetZero then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetOne then Exit;
  Result := True;
end;

function FP12SetBigNumber(const FP12: TCnFP12; const Num: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP12[0].SetBigNumber(Num) then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetBigNumbers(const FP12: TCnFP12; const Num0, Num1, Num2: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP12[0].SetBigNumber(Num0) then Exit;
  if not FP12[1].SetBigNumber(Num1) then Exit;
  if not FP12[2].SetBigNumber(Num2) then Exit;
  Result := True;
end;

function FP12SetFP4(const FP12: TCnFP12; const FP4: TCnFP4): Boolean;
begin
  Result := False;
  if FP4Copy(FP12[0], FP4) = nil then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12Set3FP4S(const FP12: TCnFP12; const FP40, FP41, FP42: TCnFP4): Boolean;
begin
  Result := False;
  if FP4Copy(FP12[0], FP40) = nil then Exit;
  if FP4Copy(FP12[1], FP41) = nil then Exit;
  if FP4Copy(FP12[2], FP42) = nil then Exit;
  Result := True;
end;

function FP12SetFP2(const FP12: TCnFP12; const FP2: TCnFP2): Boolean;
begin
  Result := False;
  if not FP4SetFP2(FP12[0], FP2) then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetHex(const FP12: TCnFP12; const S0, S1, S2, S3, S4, S5, S6, S7, S8,
  S9, S10, S11: string): Boolean;
begin
  Result := False;
  if not FP12[0].SetHex(S0, S1, S2, S3) then Exit;
  if not FP12[1].SetHex(S4, S5, S6, S7) then Exit;
  if not FP12[2].SetHex(S8, S9, S10, S11) then Exit;
  Result := True;
end;

function FP12ToString(const FP12: TCnFP12): string;
begin
  Result := FP12[2].ToString + CRLF + FP12[1].ToString + CRLF + FP12[0].ToString;
end;

function FP12SetWord(const FP12: TCnFP12; Value: Cardinal): Boolean;
begin
  Result := False;
  if not FP4SetWord(FP12[0], Value) then Exit;
  if not FP12[1].SetZero then Exit;
  if not FP12[2].SetZero then Exit;
  Result := True;
end;

function FP12SetWords(const FP12: TCnFP12; Value0, Value1, Value2, Value3, Value4,
  Value5, Value6, Value7, Value8, Value9, Value10, Value11: Cardinal): Boolean;
begin
  Result := False;
  if not FP12[0].SetWords(Value0, Value1, Value2, Value3) then Exit;
  if not FP12[1].SetWords(Value4, Value5, Value6, Value7) then Exit;
  if not FP12[2].SetWords(Value8, Value9, Value10, Value11) then Exit;
  Result := True;
end;

function FP12Equal(const F1, F2: TCnFP12): Boolean;
begin
  Result := FP4Equal(F1[0], F2[0]) and FP4Equal(F1[1], F2[1]) and FP4Equal(F1[2], F2[2]);
end;

function FP12Copy(const Dst, Src: TCnFP12): TCnFP12;
begin
  Result := nil;
  if FP4Copy(Dst[0], Src[0]) = nil then Exit;
  if FP4Copy(Dst[1], Src[1]) = nil then Exit;
  if FP4Copy(Dst[2], Src[2]) = nil then Exit;
  Result := Dst;
end;

function FP12Negate(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP4Negate(Res[0], F[0], Prime) then Exit;
  if not FP4Negate(Res[1], F[1], Prime) then Exit;
  if not FP4Negate(Res[2], F[2], Prime) then Exit;
  Result := True;
end;

function FP12Add(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP4Add(Res[0], F1[0], F2[0], Prime) then Exit;
  if not FP4Add(Res[1], F1[1], F2[1], Prime) then Exit;
  if not FP4Add(Res[2], F1[2], F2[2], Prime) then Exit;
  Result := True;
end;

function FP12Sub(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP4Sub(Res[0], F1[0], F2[0], Prime) then Exit;
  if not FP4Sub(Res[1], F1[1], F2[1], Prime) then Exit;
  if not FP4Sub(Res[2], F1[2], F2[2], Prime) then Exit;
  Result := True;
end;

function FP12Mul(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
var
  T, R0, R1, R2: TCnFP4;
begin
  Result := False;

  // r0 = a0 * b0 + a1 * b2 * v + a2 * b1 * v
  // r1 = a0 * b1 + a1 * b0 + a2 * b2 *v
  // r2 = a0 * b2 + a1 * b1 + a2 * b0
  T := nil;
  R0 := nil;
  R1 := nil;
  R2 := nil;

  try
    T := FLocalFP4Pool.Obtain;
    R0 := FLocalFP4Pool.Obtain;
    R1 := FLocalFP4Pool.Obtain;
    R2 := FLocalFP4Pool.Obtain;

    if not FP4Mul(R0, F1[0], F2[0], Prime) then Exit;
    if not FP4MulV(T, F1[1], F2[2], Prime) then Exit;
    if not FP4Add(R0, R0, T, Prime) then Exit;
    if not FP4MulV(T, F1[2], F2[1], Prime) then Exit;
    if not FP4Add(R0, R0, T, Prime) then Exit;

    if not FP4Mul(R1, F1[0], F2[1], Prime) then Exit;
    if not FP4Mul(T, F1[1], F2[0], Prime) then Exit;
    if not FP4Add(R1, R1, T, Prime) then Exit;
    if not FP4MulV(T, F1[2], F2[2], Prime) then Exit;
    if not FP4Add(R1, R1, T, Prime) then Exit;

    if not FP4Mul(R2, F1[0], F2[2], Prime) then Exit;
    if not FP4Mul(T, F1[1], F2[1], Prime) then Exit;
    if not FP4Add(R2, R2, T, Prime) then Exit;
    if not FP4Mul(T, F1[2], F2[0], Prime) then Exit;
    if not FP4Add(R2, R2, T, Prime) then Exit;

    if FP4Copy(Res[0], R0) = nil then Exit;
    if FP4Copy(Res[1], R1) = nil then Exit;
    if FP4Copy(Res[2], R2) = nil then Exit;

    Result := True;
  finally
    FLocalFP4Pool.Recycle(R2);
    FLocalFP4Pool.Recycle(R1);
    FLocalFP4Pool.Recycle(R0);
    FLocalFP4Pool.Recycle(T);
  end;
end;

function FP12Mul3(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
var
  T: TCnFP12;
begin
  Result := False;
  T := FLocalFP12Pool.Obtain;
  try
    if not FP12Add(T, F, F, Prime) then Exit;
    if not FP12Add(Res, T, F, Prime) then Exit;
    Result := True;
  finally
    FLocalFP12Pool.Recycle(T);
  end;
end;

function FP12Inverse(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
var
  K, T, T0, T1, T2, T3: TCnFP4;
begin
  Result := False;

  if FP4IsZero(F[2]) then // 分开处理
  begin
    // k = (f0^3 + f1^3 * v)^-1
    // r2 = f1^2 * k
    // r1 = -(f0 * f1 * k)
    // r0 = f0^2 * k
    K := nil;
    T := nil;

    try
      K := FLocalFP4Pool.Obtain;
      T := FLocalFP4Pool.Obtain;

      if not FP4Mul(K, F[0], F[0], Prime) then Exit;
      if not FP4Mul(K, K, F[0], Prime) then Exit;
      if not FP4MulV(T, F[1], F[1], Prime) then Exit;
      if not FP4Mul(T, T, F[1], Prime) then Exit;
      if not FP4Add(K, K, T, Prime) then Exit;
      if not FP4Inverse(K, K, Prime) then Exit;

      if not FP4Mul(T, F[1], F[1], Prime) then Exit;
      if not FP4Mul(Res[2], T, K, Prime) then Exit;

      if not FP4Mul(T, F[0], F[1], Prime) then Exit;
      if not FP4Mul(T, T, K, Prime) then Exit;
      if not FP4Negate(Res[1], T, Prime) then Exit;

      if not FP4Mul(T, F[0], F[0], Prime) then Exit;
      if not FP4Mul(Res[0], T, K, Prime) then Exit;

      Result := True;
    finally
      FLocalFP4Pool.Recycle(T);
      FLocalFP4Pool.Recycle(K);
    end;
  end
  else
  begin
    T := nil;
    T0 := nil;
    T1 := nil;
    T2 := nil;
    T3 := nil;

    try
      T := FLocalFP4Pool.Obtain;
      T0 := FLocalFP4Pool.Obtain;
      T1 := FLocalFP4Pool.Obtain;
      T2 := FLocalFP4Pool.Obtain;
      T3 := FLocalFP4Pool.Obtain;

      // t0 = f1^2 - f0 * f2
      // t1 = f0 * f1 - f2^2 * v
      // t2 = f0^2 - f1 * f2 * v
      // t3 = f2 * (t1^2 - t0 * t2)^-1
      if not FP4Mul(T0, F[1], F[1], Prime) then Exit;
      if not FP4Mul(T1, F[0], F[2], Prime) then Exit;
      if not FP4Sub(T0, T0, T1, Prime) then Exit;

      if not FP4Mul(T1, F[0], F[1], Prime) then Exit;
      if not FP4MulV(T2, F[2], F[2], Prime) then Exit;
      if not FP4Sub(T1, T1, T2, Prime) then Exit;

      if not FP4Mul(T2, F[0], F[0], Prime) then Exit;
      if not FP4MulV(T3, F[1], F[2], Prime) then Exit;
      if not FP4Sub(T2, T2, T3, Prime) then Exit;

      if not FP4Mul(T3, T1, T1, Prime) then Exit;
      if not FP4Mul(T, T0, T2, Prime) then Exit;
      if not FP4Sub(T3, T3, T, Prime) then Exit;
      if not FP4Inverse(T3, T3, Prime) then Exit;
      if not FP4Mul(T3, F[2], T3, Prime) then Exit;

      // r0 = t2 * t3
      // r1 = -(t1 * t3)
      // r2 = t0 * t3
      if not FP4Mul(Res[0], T2, T3, Prime) then Exit;

      if not FP4Mul(Res[1], T1, T3, Prime) then Exit;
      if not FP4Negate(Res[1], Res[1], Prime) then Exit;

      if not FP4Mul(Res[2], T0, T3, Prime) then Exit;

      Result := True;
    finally
      FLocalFP4Pool.Recycle(T3);
      FLocalFP4Pool.Recycle(T2);
      FLocalFP4Pool.Recycle(T1);
      FLocalFP4Pool.Recycle(T0);
      FLocalFP4Pool.Recycle(T);
    end;
  end;
end;

function FP12Div(const Res: TCnFP12; const F1, F2: TCnFP12; Prime: TCnBigNumber): Boolean;
var
  Inv: TCnFP12;
begin
  Result := False;
  if F2.IsZero then
    raise EZeroDivide.Create(SDivByZero);

  if F1 = F2 then
  begin
    if not Res.SetOne then Exit;
    Result := True;
  end
  else
  begin
    Inv := FLocalFP12Pool.Obtain;
    try
      if not FP12Inverse(Inv, F2, Prime) then Exit;
      if not FP12Mul(Res, F1, Inv, Prime) then Exit;
    finally
      FLocalFP12Pool.Recycle(Inv);
    end;
  end;
end;

function FP12Power(const Res: TCnFP12; const F: TCnFP12; Exponent: TCnBigNumber;
  Prime: TCnBigNumber): Boolean;
var
  I, N: Integer;
  T: TCnFP12;
begin
  Result := False;
  if Exponent.IsZero then
  begin
    if not Res.SetOne then Exit;
    Result := True;
    Exit;
  end
  else if Exponent.IsOne then
  begin
    if FP12Copy(Res, F) = nil then Exit;
    Result := True;
    Exit;
  end;

  N := Exponent.GetBitsCount;
  if Res = F then
    T := FLocalFP12Pool.Obtain
  else
    T := Res;

  if FP12Copy(T, F) = nil then Exit;

  try
    for I := N - 2 downto 0 do  // 指数粗略拿 6 和 13 验证过似乎是对的
    begin
      if not FP12Mul(T, T, T, Prime) then Exit;
      if Exponent.IsBitSet(I) then
        if not FP12Mul(T, T, F, Prime) then Exit;
    end;

    if Res = F then
      if FP12Copy(Res, T) = nil then Exit;
    Result := True;
  finally
    if Res = F then
      FLocalFP12Pool.Recycle(T);
  end;
end;

function FP12ToStream(FP12: TCnFP12; Stream: TStream): Integer;
begin
  Result := FP4ToStream(FP12[0], Stream) + FP4ToStream(FP12[1], Stream)
    + FP4ToStream(FP12[2], Stream);
end;

// ===================== 仿射坐标系里的三元点的运算函数 ========================

function AffinePointNew: TCnAffinePoint;
begin
  Result := TCnAffinePoint.Create;
end;

procedure AffinePointFree(P: TCnAffinePoint);
begin
  P.Free;
end;

function AffinePointSetZero(P: TCnAffinePoint): Boolean;
begin
  Result := False;
  if not P.X.SetZero then Exit;
  if not P.Y.SetZero then Exit;
  if not P.Z.SetZero then Exit;
  Result := True;
end;

function AffinePointToString(const P: TCnAffinePoint): string;
begin
  Result := 'X: ' + P.X.ToString + CRLF + 'Y: ' + P.Y.ToString + CRLF + 'Z: ' + P.Z.ToString;
end;

function AffinePointEqual(const P1, P2: TCnAffinePoint): Boolean;
begin
  Result := FP2Equal(P1.X, P2.X) and FP2Equal(P1.Y, P2.Y) and FP2Equal(P1.Z, P2.Z);
end;

function AffinePointCopy(const Dst, Src: TCnAffinePoint): TCnAffinePoint;
begin
  Result := nil;
  if FP2Copy(Dst.X, Src.X) = nil then Exit;
  if FP2Copy(Dst.Y, Src.Y) = nil then Exit;
  if FP2Copy(Dst.Z, Src.Z) = nil then Exit;
  Result := Dst;
end;

function AffinePointIsAtInfinity(const P: TCnAffinePoint): Boolean;
begin
  Result := FP2IsZero(P.X) and FP2IsOne(P.Y) and FP2IsZero(P.Z);
end;

function AffinePointSetToInfinity(const P: TCnAffinePoint): Boolean;
begin
  Result := False;
  if not P.X.SetZero then Exit;
  if not P.Y.SetOne then Exit;
  if not P.Z.SetZero then Exit;
  Result := True;
end;

function AffinePointGetCoordinatesFP2(const P: TCnAffinePoint; const FP2X, FP2Y: TCnFP2): Boolean;
begin
  Result := False;
  if P.Z.IsOne then
  begin
    if FP2Copy(FP2X, P.X) = nil then Exit;
    if FP2Copy(FP2Y, P.Y) = nil then Exit;
    Result := True;
  end;
end;

function AffinePointSetCoordinatesFP2(const P: TCnAffinePoint; const FP2X, FP2Y: TCnFP2): Boolean;
begin
  Result := False;
  if FP2Copy(P.X, FP2X) = nil then Exit;
  if FP2Copy(P.Y, FP2Y) = nil then Exit;
  if not FP2SetOne(P.Z) then Exit;
  Result := True;
end;

function AffinePointSetCoordinatesHex(const P: TCnAffinePoint;
  const SX0, SX1, SY0, SY1: string): Boolean;
begin
  Result := False;
  if not FP2SetHex(P.X, SX0, SX1) then Exit;
  if not FP2SetHex(P.Y, SY0, SY1) then Exit;
  if not FP2SetOne(P.Z) then Exit;
  Result := True;
end;

function AffinePointSetCoordinatesBigNumbers(const P: TCnAffinePoint;
  const X0, X1, Y0, Y1: TCnBigNumber): Boolean;
begin
  Result := False;
  if not FP2SetBigNumbers(P.X, X0, X1) then Exit;
  if not FP2SetBigNumbers(P.Y, X1, Y1) then Exit;
  if not FP2SetOne(P.Z) then Exit;
  Result := True;
end;

function AffinePointGetJacobianCoordinatesFP12(const P: TCnAffinePoint;
  const FP12X, FP12Y: TCnFP12; Prime: TCnBigNumber): Boolean;
var
  X, Y: TCnFP2;
  W: TCnFP12;
begin
  Result := False;

  X := nil;
  Y := nil;
  W := nil;

  try
    X := FLocalFP2Pool.Obtain;
    Y := FLocalFP2Pool.Obtain;
    W := FLocalFP12Pool.Obtain;

    if not AffinePointGetCoordinatesFP2(P, X, Y) then Exit;
    if not FP12SetFP2(FP12X, X) then Exit;
    if not FP12SetFP2(FP12Y, Y) then Exit;

    // x = x * w^-2
    if not FP12SetWSqr(W) then Exit;
    if not FP12Inverse(W, W, Prime) then Exit;
    if not FP12Mul(FP12X, FP12X, W, Prime) then Exit;

    // y = y * w^-3
    if not FP12SetV(W) then Exit;
    if not FP12Inverse(W, W, Prime) then Exit;
    if not FP12Mul(FP12Y, FP12Y, W, Prime) then Exit;

    Result := True;
  finally
    FLocalFP2Pool.Recycle(Y);
    FLocalFP2Pool.Recycle(X);
    FLocalFP12Pool.Recycle(W);
  end;
end;

function AffinePointSetJacobianCoordinatesFP12(const P: TCnAffinePoint;
  const FP12X, FP12Y: TCnFP12; Prime: TCnBigNumber): Boolean;
var
  TX, TY: TCnFP12;
begin
  Result := False;

  TX := nil;
  TY := nil;

  try
    TX := FLocalFP12Pool.Obtain;
    TY := FLocalFP12Pool.Obtain;

    if not FP12SetWSqr(TX) then Exit;
    if not FP12SetV(TY) then Exit;
    if not FP12Mul(TX, FP12X, TX, Prime) then Exit;
    if not FP12Mul(TY, FP12Y, TY, Prime) then Exit;

    if not AffinePointSetCoordinatesFP2(P, TX[0][0], TY[0][0]) then Exit;
    Result := True;
  finally
    FLocalFP12Pool.Recycle(TY);
    FLocalFP12Pool.Recycle(TX);
  end;
end;

function AffinePointIsOnCurve(const P: TCnAffinePoint; Prime: TCnBigNumber): Boolean;
var
  X, Y, B, T: TCnFP2;
begin
  Result := False;

  X := nil;
  Y := nil;
  B := nil;
  T := nil;

  try
    X := FLocalFP2Pool.Obtain;
    Y := FLocalFP2Pool.Obtain;
    B := FLocalFP2Pool.Obtain;
    T := FLocalFP2Pool.Obtain;

    if not B[0].SetZero then Exit;
    if not B[1].SetWord(CN_SM9_ECC_B) then Exit;   // B 给 5

    if not AffinePointGetCoordinatesFP2(P, X, Y) then Exit;

    // X^3 + 5 u
    if not FP2Mul(T, X, X, Prime) then Exit;
    if not FP2Mul(X, X, T, Prime) then Exit;
    if not FP2Add(X, X, B, Prime) then Exit;

    // Y^2
    if not FP2Mul(Y, Y, Y, Prime) then Exit;

    Result := FP2Equal(X, Y);
  finally
    FLocalFP2Pool.Recycle(T);
    FLocalFP2Pool.Recycle(B);
    FLocalFP2Pool.Recycle(Y);
    FLocalFP2Pool.Recycle(X);
  end;
end;

function AffinePointNegate(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if FP2Copy(Res.X, P.X) = nil then Exit;
  if not FP2Negate(Res.Y, P.Y, Prime) then Exit;
  if FP2Copy(Res.Z, P.Z) = nil then Exit;
  Result := True; 
end;

function AffinePointDouble(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
var
  L, T, X1, Y1, X2, Y2: TCnFP2;
begin
  Result := False;
  if P.IsAtInfinity then
  begin
    Result := Res.SetToInfinity;
    Exit;
  end;

  L := nil;
  T := nil;
  X1 := nil;
  Y1 := nil;
  X2 := nil;
  Y2 := nil;

  try
    L := FLocalFP2Pool.Obtain;
    T := FLocalFP2Pool.Obtain;
    X1 := FLocalFP2Pool.Obtain;
    Y1 := FLocalFP2Pool.Obtain;
    X2 := FLocalFP2Pool.Obtain;
    Y2 := FLocalFP2Pool.Obtain;

    if not AffinePointGetCoordinatesFP2(P, X1, Y1) then Exit;

    // L := 3 * x1^2 / (2 * y1)
    if not FP2Mul(L, X1, X1, Prime) then Exit;
    if not FP2Mul3(L, L, Prime) then Exit;
    if not FP2Add(T, Y1, Y1, Prime) then Exit;
    if not FP2Inverse(T, T, Prime) then Exit;
    if not FP2Mul(L, L, T, Prime) then Exit;

    // X2 = L^2 - 2 * X1
    if not FP2Mul(X2, L, L, Prime) then Exit;
    if not FP2Add(T, X1, X1, Prime) then Exit;
    if not FP2Sub(X2, X2, T, Prime) then Exit;

    // Y2 = L * (X1 - X2) - Y1
    if not FP2Sub(Y2, X1, X2, Prime) then Exit;
    if not FP2Mul(Y2, L, Y2, Prime) then Exit;
    if not FP2Sub(Y2, Y2, Y1, Prime) then Exit;

    if not AffinePointSetCoordinatesFP2(Res, X2, Y2) then Exit;

    Result := True;
  finally
    FLocalFP2Pool.Recycle(Y2);
    FLocalFP2Pool.Recycle(X2);
    FLocalFP2Pool.Recycle(Y1);
    FLocalFP2Pool.Recycle(X1);
    FLocalFP2Pool.Recycle(T);
    FLocalFP2Pool.Recycle(L);
  end;
end;

function AffinePointAdd(const Res: TCnAffinePoint; const P, Q: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
var
  X1, Y1, X2, Y2, X3, Y3, L, T: TCnFP2;
begin
  Result := False;

  if AffinePointIsAtInfinity(P) then
    Result := AffinePointCopy(Res, Q) <> nil
  else if AffinePointIsAtInfinity(Q) then
    Result := AffinePointCopy(Res, P) <> nil
  else if AffinePointEqual(P, Q) then
    Result := AffinePointDouble(P, Q, Prime)
  else
  begin
    T := nil;
    L := nil;
    X1 := nil;
    Y1 := nil;
    X2 := nil;
    Y2 := nil;
    X3 := nil;
    Y3 := nil;

    try
      T := FLocalFP2Pool.Obtain;
      L := FLocalFP2Pool.Obtain;
      X1 := FLocalFP2Pool.Obtain;
      Y1 := FLocalFP2Pool.Obtain;
      X2 := FLocalFP2Pool.Obtain;
      Y2 := FLocalFP2Pool.Obtain;
      X3 := FLocalFP2Pool.Obtain;
      Y3 := FLocalFP2Pool.Obtain;

      if not AffinePointGetCoordinatesFP2(P, X1, Y1) then Exit;
      if not AffinePointGetCoordinatesFP2(Q, X2, Y2) then Exit;
      if not FP2Add(T, Y1, Y2, Prime) then Exit;

      if T.IsZero and FP2Equal(X1, X2) then // 正负点
      begin
        Result := Res.SetToInfinity; // 和为 0
        Exit;
      end;

      // L = (Y2 - Y1)/(X2 - X1)
      if not FP2Sub(L, Y2, Y1, Prime) then Exit;
      if not FP2Sub(T, X2, X1, Prime) then Exit;
      if not FP2Inverse(T, T, Prime) then Exit;
      if not FP2Mul(L, L, T, Prime) then Exit;

      // X3 = L^2 - X1 - X2
      if not FP2Mul(X3, L, L, Prime) then Exit;
      if not FP2Sub(X3, X3, X1, Prime) then Exit;
      if not FP2Sub(X3, X3, X2, Prime) then Exit;

      // Y3 = L * (X1 - X3) - Y1
      if not FP2Sub(Y3, X1, X3, Prime) then Exit;
      if not FP2Mul(Y3, L, Y3, Prime) then Exit;
      if not FP2Sub(Y3, Y3, Y1, Prime) then Exit;

      Result := AffinePointSetCoordinatesFP2(Res, X3, Y3);
    finally
      FLocalFP2Pool.Recycle(Y3);
      FLocalFP2Pool.Recycle(X3);
      FLocalFP2Pool.Recycle(Y2);
      FLocalFP2Pool.Recycle(X2);
      FLocalFP2Pool.Recycle(Y1);
      FLocalFP2Pool.Recycle(X1);
      FLocalFP2Pool.Recycle(L);
      FLocalFP2Pool.Recycle(T);
    end;
  end;
end;

function AffinePointSub(const Res: TCnAffinePoint; const P, Q: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
var
  T: TCnAffinePoint;
begin
  Result := False;
  T := FLocalAffinePointPool.Obtain;
  try
    if not AffinePointNegate(T, Q, Prime) then Exit;
    if not AffinePointAdd(Res, P, T, Prime) then Exit;
    Result := True;
  finally
    FLocalAffinePointPool.Recycle(T);
  end;
end;

function AffinePointMul(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Num: TCnBigNumber; Prime: TCnBigNumber): Boolean;
var
  I, N: Integer;
  T: TCnAffinePoint;
begin
  Result := False;

  if Num.IsZero then
    Result := AffinePointSetToInfinity(Res)
  else if Num.IsOne then
    Result := AffinePointCopy(Res, P) <> nil
  else  // 乘对于加，等同于幂对于乘，所以和 Power 算法类似
  begin
    N := Num.GetBitsCount;
    if Res = P then
      T := FLocalAffinePointPool.Obtain
    else
      T := Res;
        
    try
      AffinePointCopy(T, P);
      for I := N - 2 downto 0 do
      begin
        if not AffinePointDouble(T, T, Prime) then Exit;
        if Num.IsBitSet(I) then
          if not AffinePointAdd(T, T, P, Prime) then Exit;
      end;

      if Res = P then
        if AffinePointCopy(Res, T) = nil then Exit;
      Result := True;
    finally
      if Res = P then
        FLocalAffinePointPool.Recycle(T);
    end;
  end;
end;

function AffinePointFrobenius(const Res: TCnAffinePoint; const P: TCnAffinePoint;
  Prime: TCnBigNumber): Boolean;
var
  X, Y: TCnFP12;
begin
  Result := False;

  X := nil;
  Y := nil;

  try
    X := FLocalFP12Pool.Obtain;
    Y := FLocalFP12Pool.Obtain;

    if not AffinePointGetJacobianCoordinatesFP12(P, X, Y, Prime) then Exit;
    if not FP12Power(X, X, Prime, Prime) then Exit;
    if not FP12Power(Y, Y, Prime, Prime) then Exit;
    if not AffinePointSetJacobianCoordinatesFP12(Res, X, Y, Prime) then Exit;
    Result := True;
  finally
    FLocalFP12Pool.Recycle(Y);
    FLocalFP12Pool.Recycle(X);
  end;
end;

// ============================ 双线性对计算函数 ===============================

// 求一点切线
function Tangent(const Res: TCnFP12; const T: TCnAffinePoint;
  const XP, YP: TCnBigNumber; Prime: TCnBigNumber): Boolean;
var
  X, Y, XT, YT, L, Q: TCnFP12;
begin
  Result := False;

  X := nil;
  Y := nil;
  XT := nil;
  YT := nil;
  L := nil;
  Q := nil;

  try
    X := FLocalFP12Pool.Obtain;
    Y := FLocalFP12Pool.Obtain;
    XT := FLocalFP12Pool.Obtain;
    YT := FLocalFP12Pool.Obtain;
    L := FLocalFP12Pool.Obtain;
    Q := FLocalFP12Pool.Obtain;

    if not AffinePointGetJacobianCoordinatesFP12(T, XT, YT, Prime) then Exit;

    if not FP12SetBigNumber(X, XP) then Exit;
    if not FP12SetBigNumber(Y, YP) then Exit;

    // L = (3 * YT^2)/(2 * YT)
    if not FP12Mul(L, XT, XT, Prime) then Exit;
    if not FP12Mul3(L, L, Prime) then Exit;
    if not FP12Add(Q, YT, YT, Prime) then Exit;
    if not FP12Inverse(Q, Q, Prime) then Exit;
    if not FP12Mul(L, L, Q, Prime) then Exit;

    // r = lambda * (x - xT) - y + yT
    if not FP12Sub(Res, X, XT, Prime) then Exit;
    if not FP12Mul(Res, L, Res, Prime) then Exit;
    if not FP12Sub(Res, Res, Y, Prime) then Exit;
    if not FP12Add(Res, Res, YT, Prime) then Exit;

    Result := True;
  finally
    FLocalFP12Pool.Recycle(Q);
    FLocalFP12Pool.Recycle(L);
    FLocalFP12Pool.Recycle(YT);
    FLocalFP12Pool.Recycle(XT);
    FLocalFP12Pool.Recycle(Y);
    FLocalFP12Pool.Recycle(X);
  end;
end;

// 求两点割线
function Secant(const Res: TCnFP12; const T, Q: TCnAffinePoint;
  const XP, YP: TCnBigNumber; Prime: TCnBigNumber): Boolean;
var
  X, Y, L, M, XT, YT, XQ, YQ: TCnFP12;
begin
  Result := False;

  X := nil;
  Y := nil;
  L := nil;
  M := nil;
  XT := nil;
  YT := nil;
  XQ := nil;
  YQ := nil;

  try
    X := FLocalFP12Pool.Obtain;
    Y := FLocalFP12Pool.Obtain;
    L := FLocalFP12Pool.Obtain;
    M := FLocalFP12Pool.Obtain;
    XT := FLocalFP12Pool.Obtain;
    YT := FLocalFP12Pool.Obtain;
    XQ := FLocalFP12Pool.Obtain;
    YQ := FLocalFP12Pool.Obtain;

    if not AffinePointGetJacobianCoordinatesFP12(T, XT, YT, Prime) then Exit;
    if not AffinePointGetJacobianCoordinatesFP12(Q, XQ, YQ, Prime) then Exit;

    if not FP12SetBigNumber(X, XP) then Exit;
    if not FP12SetBigNumber(Y, YP) then Exit;

    // L = (yT - yQ)/(xT - xQ)
    if not FP12Sub(L, YT, YQ, Prime) then Exit;
    if not FP12Sub(M, XT, XQ, Prime) then Exit;
    if not FP12Inverse(M, M, Prime) then Exit;
    if not FP12Mul(L, L, M, Prime) then Exit;

    // r = L * (x - xQ) - y + yQ
    if not FP12Sub(Res, X, XQ, Prime) then Exit;
    if not FP12Mul(Res, L, Res, Prime) then Exit;
    if not FP12Sub(Res, Res, Y, Prime) then Exit;
    if not FP12Add(Res, Res, YQ, Prime) then Exit;

    Result := True;
  finally
    FLocalFP12Pool.Recycle(YQ);
    FLocalFP12Pool.Recycle(XQ);
    FLocalFP12Pool.Recycle(YT);
    FLocalFP12Pool.Recycle(XT);
    FLocalFP12Pool.Recycle(M);
    FLocalFP12Pool.Recycle(L);
    FLocalFP12Pool.Recycle(Y);
    FLocalFP12Pool.Recycle(X);
  end;
end;

function FP12FastExp1(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if FP2Copy(Res[0][0], F[0][0]) = nil then Exit;
  if not FP2Negate(Res[0][1], F[0][1], Prime) then Exit;
  if not FP2Negate(Res[1][0], F[1][0], Prime) then Exit;
  if FP2Copy(Res[1][1], F[1][1]) = nil then Exit;
  if FP2Copy(Res[2][0], F[2][0]) = nil then Exit;
  if not FP2Negate(Res[2][1], F[2][1], Prime) then Exit;
  Result := True;
end;

function FP12FastExp2(const Res: TCnFP12; const F: TCnFP12; Prime: TCnBigNumber): Boolean;
begin
  Result := False;
  if FP2Copy(Res[0][0], F[0][0]) = nil then Exit;
  if not FP2Negate(Res[0][1], F[0][1], Prime) then Exit;
  if not FP2Mul(Res[1][0], F[1][0], FFP12FastExpPW20, Prime) then Exit;
  if not FP2Mul(Res[1][1], F[1][1], FFP12FastExpPW21, Prime) then Exit;
  if not FP2Mul(Res[2][0], F[2][0], FFP12FastExpPW22, Prime) then Exit;
  if not FP2Mul(Res[2][1], F[2][1], FFP12FastExpPW23, Prime) then Exit;
  Result := True;
end;

function FinalFastExp(const Res: TCnFP12; const F: TCnFP12; const K: TCnBigNumber;
  Prime: TCnBigNumber): Boolean;
var
  I, N: Integer;
  T, T0: TCnFP12;
begin
  Result := False;

  T := nil;
  T0 := nil;

  try
    T := FLocalFP12Pool.Obtain;
    T0 := FLocalFP12Pool.Obtain;

    if FP12Copy(T, F) = nil then Exit;
    // FP12Copy(T0, F);
    if not FP12Inverse(T0, T, Prime) then Exit;
    if not FP12FastExp1(T, T, Prime) then Exit;
    if not FP12Mul(T, T0, T, Prime) then Exit;
    if FP12Copy(T0, T) = nil then Exit;

    if not FP12FastExp2(T, T, Prime) then Exit;
    if not FP12Mul(T, T0, T, Prime) then Exit;
    if FP12Copy(T0, T) = nil then Exit;

    N := K.GetBitsCount;
    for I := N - 2 downto 0 do
    begin
      if not FP12Mul(T, T, T, Prime) then Exit;
      if K.IsBitSet(I) then
        if not FP12Mul(T, T, T0, Prime) then Exit;
    end;

    if FP12Copy(Res, T) = nil then Exit;
    Result := True;
  finally
    FLocalFP12Pool.Recycle(T0);
    FLocalFP12Pool.Recycle(T);
  end;
end;

function Rate(const F: TCnFP12; const Q: TCnAffinePoint; const XP, YP: TCnBigNumber;
  const A: TCnBigNumber; const K: TCnBigNumber; Prime: TCnBigNumber): Boolean;
var
  I, N: Integer;
  T, Q1, Q2: TCnAffinePoint;
  G: TCnFP12;
begin
  Result := False;

  T := nil;
  Q1 := nil;
  Q2 := nil;
  G := nil;

  try
    T := FLocalAffinePointPool.Obtain;
    Q1 := FLocalAffinePointPool.Obtain;
    Q2 := FLocalAffinePointPool.Obtain;
    G := FLocalFP12Pool.Obtain;

    if not FP12SetOne(F) then Exit;
    if AffinePointCopy(T, Q) = nil then Exit;
    N := A.GetBitsCount;

    for I := N - 2 downto 0 do
    begin
      if not Tangent(G, T, XP, YP, Prime) then Exit;
      if not FP12Mul(F, F, F, Prime) then Exit;
      if not FP12Mul(F, F, G, Prime) then Exit;

      if not AffinePointDouble(T, T, Prime) then Exit;

      if A.IsBitSet(I) then
      begin
        if not Secant(G, T, Q, XP, YP, Prime) then Exit;
        if not FP12Mul(F, F, G, Prime) then Exit;
        if not AffinePointAdd(T, T, Q, Prime) then Exit;
      end;
    end;

    if not AffinePointFrobenius(Q1, Q, Prime) then Exit;

    if not AffinePointFrobenius(Q2, Q, Prime) then Exit;
    if not AffinePointFrobenius(Q2, Q2, Prime) then Exit;

    if not Secant(G, T, Q1, XP, YP, Prime) then Exit;
    if not FP12Mul(F, F, G, Prime) then Exit;

    if not AffinePointAdd(T, T, Q1, Prime) then Exit;

    if not AffinePointNegate(Q2, Q2, Prime) then Exit;
    if not Secant(G, T, Q2, XP, YP, Prime) then Exit;
    if not FP12Mul(F, F, G, Prime) then Exit;

    if not AffinePointAdd(T, T, Q2, Prime) then Exit;

    if not FinalFastExp(F, F, K, Prime) then Exit;
    Result := True;
  finally
    FLocalFP12Pool.Recycle(G);
    FLocalAffinePointPool.Recycle(Q2);
    FLocalAffinePointPool.Recycle(Q1);
    FLocalAffinePointPool.Recycle(T);
  end;
end;

function SM9RatePairing(const F: TCnFP12; const Q: TCnAffinePoint; const P: TCnEccPoint): Boolean;
var
  XP, YP: TCnBigNumber; // P 点坐标的引用
  AQ: TCnAffinePoint;   // Q 点坐标的引用
begin
  if P <> nil then
  begin
    XP := P.X;
    YP := P.Y;
  end
  else // 如果 P 是 nil，则使用 SM9 的曲线的 G1 点
  begin
    XP := FSM9G1P1X;
    YP := FSM9G1P1Y;
  end;

  if Q = nil then // 如果 Q 是 nil，则使用 SM9 曲线的 G2 点
  begin
    AQ := FLocalAffinePointPool.Obtain;
    AQ.SetCoordinatesBigNumbers(FSM9G2P2X0, FSM9G2P2X1, FSM9G2P2Y0, FSM9G2P2Y1);
  end
  else
    AQ := Q;

  // 计算 R-ate 对的值
  Result := Rate(F, AQ, XP, YP, FSM96TPlus2, FSM9FastExpP3, FSM9FiniteFieldSize);

  if Q = nil then
    FLocalAffinePointPool.Recycle(AQ);
end;

{ TCnFP2 }

constructor TCnFP2.Create;
begin
  inherited;
  F0 := TCnBigNumber.Create;
  F1 := TCnBigNumber.Create;
end;

destructor TCnFP2.Destroy;
begin
  F1.Free;
  F0.Free;
  inherited;
end;

function TCnFP2.GetItems(Index: Integer): TCnBigNumber;
begin
  if Index = 0 then
    Result := F0
  else if Index = 1 then
    Result := F1
  else
    raise Exception.CreateFmt(SListIndexError, [Index]);
end;

function TCnFP2.IsOne: Boolean;
begin
  Result := FP2IsOne(Self);
end;

function TCnFP2.IsZero: Boolean;
begin
  Result := FP2IsZero(Self);
end;

function TCnFP2.SetBigNumber(const Num: TCnBigNumber): Boolean;
begin
  Result := FP2SetBigNumber(Self, Num);
end;

function TCnFP2.SetHex(const S0, S1: string): Boolean;
begin
  Result := FP2SetHex(Self, S0, S1);
end;

function TCnFP2.SetOne: Boolean;
begin
  Result := FP2SetOne(Self);
end;

function TCnFP2.SetU: Boolean;
begin
  Result := FP2SetU(Self);
end;

function TCnFP2.SetWord(Value: Cardinal): Boolean;
begin
  Result := FP2SetWord(Self, Value);
end;

function TCnFP2.SetWords(Value0, Value1: Cardinal): Boolean;
begin
  Result := FP2SetWords(Self, Value0, Value1);
end;

function TCnFP2.SetZero: Boolean;
begin
  Result := FP2SetZero(Self);
end;

function TCnFP2.ToString: string;
begin
  Result := FP2ToString(Self);
end;

{ TCnFP4 }

constructor TCnFP4.Create;
begin
  inherited;
  F0 := TCnFP2.Create;
  F1 := TCnFP2.Create;
end;

destructor TCnFP4.Destroy;
begin
  F1.Free;
  F0.Free;
  inherited;
end;

function TCnFP4.GetItems(Index: Integer): TCnFP2;
begin
  if Index = 0 then
    Result := F0
  else if Index = 1 then
    Result := F1
  else
    raise Exception.CreateFmt(SListIndexError, [Index]);
end;

function TCnFP4.IsOne: Boolean;
begin
  Result := FP4IsOne(Self);
end;

function TCnFP4.IsZero: Boolean;
begin
  Result := FP4IsZero(Self);
end;

function TCnFP4.SetBigNumber(const Num: TCnBigNumber): Boolean;
begin
  Result := FP4SetBigNumber(Self, Num);
end;

function TCnFP4.SetBigNumbers(const Num0, Num1: TCnBigNumber): Boolean;
begin
  Result := FP4SetBigNumbers(Self, Num0, Num1);
end;

function TCnFP4.SetHex(const S0, S1, S2, S3: string): Boolean;
begin
  Result := FP4SetHex(Self, S0, S1, S2, S3);
end;

function TCnFP4.SetOne: Boolean;
begin
  Result := FP4SetOne(Self);
end;

function TCnFP4.SetU: Boolean;
begin
  Result := FP4SetU(Self);
end;

function TCnFP4.SetV: Boolean;
begin
  Result := FP4SetV(Self);
end;

function TCnFP4.SetWord(Value: Cardinal): Boolean;
begin
  Result := FP4SetWord(Self, Value);
end;

function TCnFP4.SetWords(Value0, Value1, Value2,
  Value3: Cardinal): Boolean;
begin
  Result := FP4SetWords(Self, Value0, Value1, Value2, Value3);
end;

function TCnFP4.SetZero: Boolean;
begin
  Result := FP4SetZero(Self);
end;

function TCnFP4.ToString: string;
begin
  Result := FP4ToString(Self);
end;

{ TCnFP12 }

constructor TCnFP12.Create;
begin
  inherited;
  F0 := TCnFP4.Create;
  F1 := TCnFP4.Create;
  F2 := TCnFP4.Create;
end;

destructor TCnFP12.Destroy;
begin
  F2.Free;
  F1.Free;
  F0.Free;
  inherited;
end;

function TCnFP12.GetItems(Index: Integer): TCnFP4;
begin
  if Index = 0 then
    Result := F0
  else if Index = 1 then
    Result := F1
  else if Index = 2 then
    Result := F2
  else
    raise Exception.CreateFmt(SListIndexError, [Index]);
end;

function TCnFP12.IsOne: Boolean;
begin
  Result := FP12IsOne(Self);
end;

function TCnFP12.IsZero: Boolean;
begin
  Result := FP12IsZero(Self);
end;

function TCnFP12.SetBigNumber(const Num: TCnBigNumber): Boolean;
begin
  Result := FP12SetBigNumber(Self, Num);
end;

function TCnFP12.SetBigNumbers(const Num0, Num1, Num2: TCnBigNumber): Boolean;
begin
  Result := FP12SetBigNumbers(Self, Num0, Num1, Num2);
end;

function TCnFP12.SetHex(const S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10,
  S11: string): Boolean;
begin
  Result := FP12SetHex(Self, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11);
end;

function TCnFP12.SetOne: Boolean;
begin
  Result := FP12SetOne(Self);
end;

function TCnFP12.SetU: Boolean;
begin
  Result := FP12SetU(Self);
end;

function TCnFP12.SetV: Boolean;
begin
  Result := FP12SetV(Self);
end;

function TCnFP12.SetW: Boolean;
begin
  Result := FP12SetW(Self);
end;

function TCnFP12.SetWord(Value: Cardinal): Boolean;
begin
  Result := FP12SetWord(Self, Value);
end;

function TCnFP12.SetWords(Value0, Value1, Value2, Value3, Value4, Value5,
  Value6, Value7, Value8, Value9, Value10, Value11: Cardinal): Boolean;
begin
  Result := FP12SetWords(Self, Value0, Value1, Value2, Value3, Value4, Value5,
    Value6, Value7, Value8, Value9, Value10, Value11);
end;

function TCnFP12.SetWSqr: Boolean;
begin
  Result := FP12SetWSqr(Self);
end;

function TCnFP12.SetZero: Boolean;
begin
  Result := FP12SetZero(Self);
end;

function TCnFP12.ToString: string;
begin
  Result := FP12ToString(Self);
end;

{ TCnFP2Pool }

function TCnFP2Pool.CreateObject: TObject;
begin
  Result := TCnFP2.Create;
end;

function TCnFP2Pool.Obtain: TCnFP2;
begin
  Result := TCnFP2(inherited Obtain);
  Result.SetZero;
end;

procedure TCnFP2Pool.Recycle(Num: TCnFP2);
begin
  inherited Recycle(Num);
end;

{ TCnFP4Pool }

function TCnFP4Pool.CreateObject: TObject;
begin
  Result := TCnFP4.Create;
end;

function TCnFP4Pool.Obtain: TCnFP4;
begin
  Result := TCnFP4(inherited Obtain);
  Result.SetZero;
end;

procedure TCnFP4Pool.Recycle(Num: TCnFP4);
begin
  inherited Recycle(Num);
end;

{ TCnFP12Pool }

function TCnFP12Pool.CreateObject: TObject;
begin
  Result := TCnFP12.Create;
end;

function TCnFP12Pool.Obtain: TCnFP12;
begin
  Result := TCnFP12(inherited Obtain);
  Result.SetZero;
end;

procedure TCnFP12Pool.Recycle(Num: TCnFP12);
begin
  inherited Recycle(Num);
end;

{ TCnAffinePoint }

constructor TCnAffinePoint.Create;
begin
  inherited;
  FX := TCnFP2.Create;
  FY := TCnFP2.Create;
  FZ := TCnFP2.Create;
end;

destructor TCnAffinePoint.Destroy;
begin
  FZ.Free;
  FY.Free;
  FX.Free;
  inherited;
end;

function TCnAffinePoint.GetCoordinatesFP2(const FP2X,
  FP2Y: TCnFP2): Boolean;
begin
  Result := AffinePointGetCoordinatesFP2(Self, FP2X, FP2Y);
end;

function TCnAffinePoint.GetExtCoordinatesFP12(const FP12X, FP12Y: TCnFP12;
  Prime: TCnBigNumber): Boolean;
begin
  Result := AffinePointGetJacobianCoordinatesFP12(Self, FP12X, FP12Y, Prime);
end;

function TCnAffinePoint.IsAtInfinity: Boolean;
begin
  Result := AffinePointIsAtInfinity(Self);
end;

function TCnAffinePoint.IsOnCurve(Prime: TCnBigNumber): Boolean;
begin
  Result := AffinePointIsOnCurve(Self, Prime);
end;

function TCnAffinePoint.SetCoordinatesBigNumbers(const X0, X1, Y0,
  Y1: TCnBigNumber): Boolean;
begin
  Result := AffinePointSetCoordinatesBigNumbers(Self, X0, X1, Y0, Y1);
end;

function TCnAffinePoint.SetCoordinatesFP2(const FP2X,
  FP2Y: TCnFP2): Boolean;
begin
  Result := AffinePointSetCoordinatesFP2(Self, FP2X, FP2Y);
end;

function TCnAffinePoint.SetCoordinatesHex(const SX0, SX1, SY0,
  SY1: string): Boolean;
begin
  Result := AffinePointSetCoordinatesHex(Self, SX0, SX1, SY0, SY1);
end;

function TCnAffinePoint.SetExtCoordinatesFP12(const FP12X, FP12Y: TCnFP12;
  Prime: TCnBigNumber): Boolean;
begin
  Result := AffinePointSetJacobianCoordinatesFP12(Self, FP12X, FP12Y, Prime);
end;

function TCnAffinePoint.SetToInfinity: Boolean;
begin
  Result := AffinePointSetToInfinity(Self);
end;

procedure TCnAffinePoint.SetZero;
begin
  AffinePointSetZero(Self);
end;

function TCnAffinePoint.ToString: string;
begin
  Result := AffinePointToString(Self);
end;

{ TCnAffinePointPool }

function TCnAffinePointPool.CreateObject: TObject;
begin
  Result := TCnAffinePoint.Create;
end;

function TCnAffinePointPool.Obtain: TCnAffinePoint;
begin
  Result := TCnAffinePoint(inherited Obtain);
//  Result.SetZero;
end;

procedure TCnAffinePointPool.Recycle(Num: TCnAffinePoint);
begin
  inherited Recycle(Num);
end;

procedure InitSM9Consts;
begin
  FSM9FiniteFieldSize := TCnBigNumber.FromHex(CN_SM9_FINITE_FIELD);
  FSM9Order := TCnBigNumber.FromHex(CN_SM9_ORDER);
  FSM9G1P1X := TCnBigNumber.FromHex(CN_SM9_G1_P1X);
  FSM9G1P1Y := TCnBigNumber.FromHex(CN_SM9_G1_P1Y);
  FSM9G2P2X0 := TCnBigNumber.FromHex(CN_SM9_G2_P2X0);
  FSM9G2P2X1 := TCnBigNumber.FromHex(CN_SM9_G2_P2X1);
  FSM9G2P2Y0 := TCnBigNumber.FromHex(CN_SM9_G2_P2Y0);
  FSM9G2P2Y1 := TCnBigNumber.FromHex(CN_SM9_G2_P2Y1);
  FSM96TPlus2 := TCnBigNumber.FromHex(CN_SM9_6T_PLUS_2);
  FSM9FastExpP3 := TCnBigNumber.FromHex(CN_SM9_FAST_EXP_P3);
  FFP12FastExpPW20 := TCnBigNumber.FromHex(CN_SM9_FAST_EXP_PW20);
  FFP12FastExpPW21 := TCnBigNumber.FromHex(CN_SM9_FAST_EXP_PW21);
  FFP12FastExpPW22 := TCnBigNumber.FromHex(CN_SM9_FAST_EXP_PW22);
  FFP12FastExpPW23 := TCnBigNumber.FromHex(CN_SM9_FAST_EXP_PW23);
end;

procedure FreeSM9Consts;
begin
  FSM9FiniteFieldSize.Free;
  FSM9Order.Free;
  FSM9G1P1X.Free;
  FSM9G1P1Y.Free;
  FSM9G2P2X0.Free;
  FSM9G2P2X1.Free;
  FSM9G2P2Y0.Free;
  FSM9G2P2Y1.Free;
  FSM96TPlus2.Free;
  FSM9FastExpP3.Free;
  FFP12FastExpPW20.Free;
  FFP12FastExpPW21.Free;
  FFP12FastExpPW22.Free;
  FFP12FastExpPW23.Free;
end;

initialization
  FLocalBigNumberPool := TCnBigNumberPool.Create;
  FLocalFP2Pool := TCnFP2Pool.Create;
  FLocalFP4Pool := TCnFP4Pool.Create;
  FLocalFP12Pool := TCnFP12Pool.Create;
  FLocalAffinePointPool := TCnAffinePointPool.Create;

  InitSM9Consts;

finalization
  FLocalAffinePointPool.Free;
  FLocalFP12Pool.Free;
  FLocalFP4Pool.Free;
  FLocalFP2Pool.Free;
  FLocalBigNumberPool.Free;

  FreeSM9Consts;

end.
