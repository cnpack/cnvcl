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

unit CnPolynomial;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：多项式运算实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了系数为 Int64 及大整数的一元与二元多项式运算，以及一元有理分式的运算。
*
*           支持普通的整系数多项式四则运算，除法只支持除数最高次数为 1 的情况。
*           支持有限扩域范围内的多项式四则运算，系数均 mod p 并且结果对本原多项式求余。
*           支持大整数系数多项式以及有理分式在普通四则运算以及在有限扩域范围内的运算。
*
*           注：素数幂模下多项式环的整系数多项式求逆判断算法来源于《一种新的重模剩余类环中元素逆的求法》，
*              河北省科学院学报，2009 年 3 月（注意其中的计算算法不靠谱）。
*              实际计算算法来源于 stackoverflow 上 William Whyte 以及 Sonel Sharam 的帖子。
*
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.01.25 V1.8
*               调整有理数相关函数的参数顺序，注意和旧版不兼容
*           2023.09.01 V1.7
*               实现素数幂模下多项式环的整系数多项式求逆
*           2021.12.01 V1.6
*               实现 BigNumber 范围内的二元整系数多项式及其运算，包括有限域内
*           2021.11.17 V1.5
*               实现 Int64 范围内的二元整系数多项式及其运算，包括有限域内
*           2020.08.29 V1.4
*               实现 Int64 范围内的快速数论变换/快速傅立叶变换多项式乘法，但都有所限制
*           2020.11.14 V1.3
*               实现有限扩域中 Int64 以及大整数范围内的有理分式的代换
*           2020.11.08 V1.3
*               实现有限扩域中大整数范围内的多项式以及有理分式及其运算
*           2020.10.20 V1.2
*               实现有限扩域中 Int64 范围内的有理分式及其运算
*           2020.08.28 V1.1
*               实现有限扩域中 Int64 范围内的多项式及其运算，包括对本原多项式求余的模逆元
*           2020.08.21 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, Math, Contnrs, CnPrime, CnNative,
  CnMatrix, CnContainers, CnBigNumber, CnBigRational, CnComplex, CnDFT;

type
  ECnPolynomialException = class(Exception);
  {* 多项式相关异常}

// =============================================================================
//
//                    一元整系数多项式与一元整系数有理分式
//
// =============================================================================

  TCnInt64Polynomial = class(TCnInt64List)
  {* 一元整系数多项式，系数范围为 Int64，Items[n] 就是 n 次项系数}
  private
    procedure EnsureDegree(Degree: Integer);  // 确保第 Degree 次存在，也就是 Count >= Degree + 1
    function GetMaxDegree: Integer;
    procedure SetMaxDegree(const Value: Integer);
  public
    constructor Create(LowToHighCoefficients: array of const); overload;
    {* 构造函数，参数为从低到高的系数，注意系数初始化时大于 MaxInt32/MaxInt64 的会被当成 Integer/Int64 而变负。

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的低次到高次的多项式系数

       返回值：                                           - 返回创建的对象实例
    }

    constructor Create; overload;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SetCoefficents(LowToHighCoefficients: array of const);
    {* 一次批量设置从低到高的系数。

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的低次到高次的多项式系数

       返回值：（无）
    }

    procedure SetCoefficent(Degree: Integer; Coefficient: Integer);
    {* 设置某次项的系数。

       参数：
         Degree: Integer                                  - 该项的多项式的次数
         Coefficient: Integer                             - 该项的多项式系数

       返回值：（无）
    }

    procedure CorrectTop;
    {* 剔除高次的 0 系数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    procedure SetString(const Poly: string);
    {* 将多项式字符串转换为本对象的内容。

       参数：
         const Poly: string               - 待转换的字符串

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 返回是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    procedure SetZero;
    {* 设为 0}

    function IsOne: Boolean;
    {* 返回是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    procedure SetOne;
    {* 设为 1}

    function IsNegOne: Boolean;
    {* 返回是否为 -1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 -1
    }

    procedure Negate;
    {* 所有系数求反}

    function IsMonic: Boolean;
    {* 是否首一多项式，也即最高次系数是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否首一多项式
    }

    property MaxDegree: Integer read GetMaxDegree write SetMaxDegree;
    {* 最高次数，0 开始，基于 Count 所以只能是 Integer，下标遍历时使用 0 到 MaxDegree}
  end;

  TCnInt64PolynomialList = class(TObjectList)
  {* 容纳一元整系数多项式的对象列表，同时拥有一元整系数多项式对象们}
  private

  protected
    function GetItem(Index: Integer): TCnInt64Polynomial;
    procedure SetItem(Index: Integer; APoly: TCnInt64Polynomial);
  public
    constructor Create; reintroduce;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function Add: TCnInt64Polynomial; overload;
    {* 新增一个一元整系数多项式对象，返回该对象。
       注意添加后返回的对象已由列表纳入管理，无需也不应手动释放。

       参数：
         （无）

       返回值：TCnInt64Polynomial         - 内部新增的大数对象
    }

    function Add(APoly: TCnInt64Polynomial): Integer; overload;
    {* 添加外部的一元整系数多项式对象。注意添加后该对象由列表纳入管理，无需也不应手动释放。

       参数：
         APoly: TCnInt64Polynomial        - 待添加的一元整系数多项式对象

       返回值：Integer                    - 新增的该一元整系数多项式对象的索引值
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将一元整系数多项式列表转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property Items[Index: Integer]: TCnInt64Polynomial read GetItem write SetItem; default;
    {* 一元整系数多项式列表项}
  end;

  TCnInt64RationalPolynomial = class(TPersistent)
  {* 一元整系数有理分式，分母分子分别为一元整系数多项式}
  private
    FNumerator: TCnInt64Polynomial;
    FDenominator: TCnInt64Polynomial;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function IsInt: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否整多项式，也就是判断分母是否是正负 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否整多项式
    }

    function IsZero: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    function IsOne: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    procedure Reciprocal;
    {* 变成倒数}

    procedure Neg;
    {* 变成负的}

    procedure SetZero;
    {* 设为 0}

    procedure SetOne;
    {* 设为 1}

    procedure Reduce;
    {* 约分}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 输出成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    procedure SetString(const Rational: string);
    {* 将多项式或分式字符串转换为本对象的内容。

       参数：
         const Rational: string           -

       返回值：（无）
    }

    property Numerator: TCnInt64Polynomial read FNumerator;
    {* 分子式}
    property Denominator: TCnInt64Polynomial read FDenominator;
    {* 分母式}
  end;

  TCnInt64PolynomialPool = class(TCnMathObjectPool)
  {* 一元整系数多项式池实现类，允许使用到一元整系数多项式的地方自行创建一元整系数多项式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnInt64Polynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnInt64Polynomial         - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnInt64Polynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnInt64Polynomial         - 待归还的多项式对象

       返回值：（无）
    }
  end;

  TCnInt64RationalPolynomialPool = class(TCnMathObjectPool)
  {* 一元整系数有理分式池实现类，允许使用到一元整系数有理分式的地方自行创建整一元系数有理分式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnInt64RationalPolynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnInt64RationalPolynomial - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnInt64RationalPolynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnInt64RationalPolynomial - 待归还的多项式对象

       返回值：（无）
    }
  end;

// =============================================================================
//
//                 一元大整系数多项式与一元大整系数有理分式
//
// =============================================================================

  TCnBigNumberPolynomial = class(TCnBigNumberList)
  {* 一元大整系数多项式，Items[n] 就是 n 次项系数}
  private
    procedure EnsureDegree(Degree: Integer);
    function GetMaxDegree: Integer;
    procedure SetMaxDegree(const Value: Integer);
  public
    constructor Create(LowToHighCoefficients: array of const); overload;
    {* 构造函数，参数为从低到高的系数，注意系数初始化时大于 MaxInt32/MaxInt64 的会被当成 Integer/Int64 而变负。

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的低次到高次的多项式系数

       返回值：                                           - 返回创建的对象实例
    }

    constructor Create; overload;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SetCoefficents(LowToHighCoefficients: array of const);
    {* 一次批量设置从低到高的系数。

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的低次到高次的多项式系数

       返回值：（无）
    }

    procedure SetCoefficent(Degree: Integer; Coefficient: TCnBigNumber);
    {* 设置某次项的系数，内部值会从 Coefficient 中复制过去。

       参数：
         Degree: Integer                                  - 该项的多项式的次数
         Coefficient: TCnBigNumber                        - 该项的多项式系数

       返回值：（无）
    }

    procedure CorrectTop;
    {* 剔除高次的 0 系数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    procedure SetString(const Poly: string);
    {* 将多项式字符串转换为本对象的内容。

       参数：
         const Poly: string               - 待转换的字符串

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 返回是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    procedure SetZero;
    {* 设为 0}

    function IsOne: Boolean;
    {* 返回是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    procedure SetOne;
    {* 设为 1}

    function IsNegOne: Boolean;
    {* 返回是否为 -1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 -1
    }

    procedure Negate;
    {* 所有系数求反}

    function IsMonic: Boolean;
    {* 是否首一多项式。

       参数：
         （无）

       返回值：Boolean                    - 返回是否首一多项式
    }

    property MaxDegree: Integer read GetMaxDegree write SetMaxDegree;
    {* 最高次数，0 开始}
  end;

  TCnBigNumberPolynomialPool = class(TCnMathObjectPool)
  {* 一元大整系数多项式池实现类，允许使用到一元大整数系数多项式的地方自行创建一元大整数系数多项式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumberPolynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigNumberPolynomial     - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnBigNumberPolynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnBigNumberPolynomial     - 待归还的多项式对象

       返回值：（无）
    }
  end;

  TCnBigNumberRationalPolynomial = class(TPersistent)
  {* 一元大整系数有理分式，分母分子分别为一元大整系数多项式}
  private
    FNumerator: TCnBigNumberPolynomial;
    FDenominator: TCnBigNumberPolynomial;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function IsInt: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否整多项式，也就是判断分母是否是正负 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否整多项式
    }

    function IsZero: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    function IsOne: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    procedure Reciprocal;
    {* 设为倒数}

    procedure Neg;
    {* 设为负的}

    procedure SetZero;
    {* 设为 0}

    procedure SetOne;
    {* 设为 1}

    procedure Reduce;
    {* 约分}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 转换成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    procedure SetString(const Rational: string);
    {* 将多项式或分式字符串转换为本对象的内容。

       参数：
         const Rational: string           - 待转换的字符串

       返回值：（无）
    }

    property Numerator: TCnBigNumberPolynomial read FNumerator;
    {* 分子多项式}
    property Denominator: TCnBigNumberPolynomial read FDenominator;
    {* 分母多项式}
  end;

  TCnBigNumberRationalPolynomialPool = class(TCnMathObjectPool)
  {* 一元大整系数有理分式池实现类，允许使用到一元大整系数有理分式的地方自行创建一元大整系数有理分式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumberRationalPolynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigNumberRationalPolynomial             - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnBigNumberRationalPolynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnBigNumberRationalPolynomial             - 待归还的多项式对象

       返回值：（无）
    }
  end;

  TCnBigComplexPolynomial = class(TCnBigComplexList)
  {* 一元大整数复数多项式}
  private
    procedure EnsureDegree(Degree: Integer);
    function GetMaxDegree: Integer;
    procedure SetMaxDegree(const Value: Integer);
  public
    constructor Create(LowToHighCoefficients: array of const); overload;
    {* 构造函数，参数为从低到高的系数，注意系数初始化时大于 MaxInt32/MaxInt64 的会被当成 Integer/Int64 而变负。

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的低次到高次的多项式系数

       返回值：                                           - 返回创建的对象实例
    }

    constructor Create; overload;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SetCoefficents(LowToHighCoefficients: array of const);
    {* 一次批量设置从低到高的系数。

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的低次到高次的多项式系数

       返回值：（无）
    }

    procedure SetCoefficent(Degree: Integer; Coefficient: TCnBigComplex);
    {* 设置某次项的系数，内部值会从 Coefficient 中复制过去。

       参数：
         Degree: Integer                                  - 该项的多项式的次数
         Coefficient: TCnBigComplex                        - 该项的多项式系数

       返回值：（无）
    }

    procedure CorrectTop;
    {* 剔除高次的 0 系数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    function IsZero: Boolean;
    {* 返回是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    procedure SetZero;
    {* 设为 0}

    function IsOne: Boolean;
    {* 返回是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 1
    }

    procedure SetOne;
    {* 设为 1}

    function IsNegOne: Boolean;
    {* 返回是否为 -1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 -1
    }

    procedure Negate;
    {* 所有系数求反}

    function IsMonic: Boolean;
    {* 是否首一多项式。

       参数：
         （无）

       返回值：Boolean                    - 返回是否首一多项式
    }

    property MaxDegree: Integer read GetMaxDegree write SetMaxDegree;
    {* 最高次数，0 开始}
  end;

  TCnBigComplexPolynomialPool = class(TCnMathObjectPool)
  {* 一元大整数复数多项式池实现类，允许使用到一元大整数复数多项式的地方自行创建一元大整数复数多项式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigComplexPolynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigComplexPolynomial     - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnBigComplexPolynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnBigComplexPolynomial     - 待归还的多项式对象

       返回值：（无）
    }
  end;

  TCnBigComplexDecimalPolynomial = class(TCnBigComplexDecimalList)
  {* 一元大浮点复数多项式}
  private
    procedure EnsureDegree(Degree: Integer);
    function GetMaxDegree: Integer;
    procedure SetMaxDegree(const Value: Integer);
  public
    constructor Create(LowToHighCoefficients: array of const); overload;
    {* 构造函数，参数为从低到高的系数。注意系数初始化时若大于 MaxInt32/MaxInt64 的值被当作 Integer/Int64 类型处理而变负

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的从低到高的多项式系数

       返回值：                                           - 返回创建的多项式对象
    }

    constructor Create; overload;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure SetCoefficents(LowToHighCoefficients: array of const);
    {* 一次性设置所有从低到高的系数

       参数：
         LowToHighCoefficients: array of const            - 从 0 开始的从低到高的多项式系数

       返回值：（无）
    }

    procedure SetCoefficent(Degree: Integer; Coefficient: TCnBigComplexDecimal);
    {* 设置某一次项的系数，内部会复制 Coefficient 的值过去

       参数：
         Degree: Integer                                  - 设置的多项式的次数
         Coefficient: TCnBigComplexDecimal                - 设置的多项式系数

       返回值：（无）
    }

    procedure CorrectTop;
    {* 删除最高次项的 0 系数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 多项式转换为字符串表示

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    function IsZero: Boolean;
    {* 判断多项式是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 多项式是否为 0
    }

    procedure SetZero;
    {* 设为 0}

    function IsOne: Boolean;
    {* 判断多项式是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 多项式是否为 1
    }

    procedure SetOne;
    {* 设为 1}

    function IsNegOne: Boolean;
    {* 判断多项式是否为 -1。

       参数：
         （无）

       返回值：Boolean                    - 多项式是否为 -1
    }

    procedure Negate;
    {* 取反系数}

    function IsMonic: Boolean;
    {* 是否为首一多项式。

       参数：
         （无）

       返回值：Boolean                    - 多项式是否为首一多项式
    }

    property MaxDegree: Integer read GetMaxDegree write SetMaxDegree;
    {* 最高次数，从0 开始}
  end;

  TCnBigComplexDecimalPolynomialPool = class(TCnMathObjectPool)
  {* 一元大浮点复数多项式池实现类，允许使用到一元大浮点复数多项式的地方自行创建一元大浮点复数多项式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigComplexDecimalPolynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还

       参数：
         （无）

       返回值：TCnBigComplexDecimalPolynomial - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnBigComplexDecimalPolynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnBigComplexDecimalPolynomial - 待归还的多项式对象

       返回值：（无）
    }
  end;

// ====================== 一元大整数复数多项式基础运算 =============================

function BigComplexPolynomialNew: TCnBigComplexPolynomial;
{* 创建一个动态分配的一元大整数复数多项式对象，等同于 TCnBigComplexPolynomial.Create。

   参数：
     （无）

   返回值：TCnBigComplexPolynomial        - 返回创建的一元大整数复数多项式对象
}

procedure BigComplexPolynomialFree(P: TCnBigComplexPolynomial);
{* 释放一个一元大整数复数多项式对象，等同于 TCnBigComplexPolynomial.Free

   参数：
     P: TCnBigComplexPolynomial           - 待释放的一元大整数复数多项式

   返回值：（无）
}

function BigComplexPolynomialDuplicate(P: TCnBigComplexPolynomial): TCnBigComplexPolynomial;
{* 从一个一元大整数复数多项式对象克隆一个新对象。

   参数：
     P: TCnBigComplexPolynomial           - 待复制的一元大整数复数多项式

   返回值：TCnBigComplexPolynomial        - 返回新建的一元大整数复数多项式
}

function BigComplexPolynomialCopy(Dest: TCnBigComplexPolynomial;
  Source: TCnBigComplexPolynomial): TCnBigComplexPolynomial;
{* 复制一个一元大整数复数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnBigComplexPolynomial        - 目标一元大整数复数多项式
     Source: TCnBigComplexPolynomial      - 源一元大整数复数多项式

   返回值：TCnBigComplexPolynomial        - 成功则返回目标对象，失败则返回 nil
}

procedure BigComplexPolynomialSwap(A: TCnBigComplexPolynomial;
  B: TCnBigComplexPolynomial);
{* 交换两个一元大整数复数多项式对象的系数值。

   参数：
     A: TCnBigComplexPolynomial           - 待交换的一元大整数复数多项式一
     B: TCnBigComplexPolynomial           - 待交换的一元大整数复数多项式二

   返回值：（无）
}

function BigComplexPolynomialToString(P: TCnBigComplexPolynomial;
  const VarName: string = 'X'): string;
{* 将一个一元大整数复数多项式对象转成字符串，未知数默认以 X 表示。

   参数：
     P: TCnBigComplexPolynomial    - 待转换的一元大整数复数多项式
     const VarName: string                - 代表未知数的字符串

   返回值：string                         - 返回字符串
}

function BigComplexPolynomialIsZero(P: TCnBigComplexPolynomial): Boolean;
{* 判断一个一元大整数复数多项式对象是否为 0。

   参数：
     P: TCnBigComplexPolynomial           - 待判断的一元大整数复数多项式

   返回值：Boolean                        - 返回是否为 0
}

procedure BigComplexPolynomialSetZero(P: TCnBigComplexPolynomial);
{* 将一个一元大整数复数多项式对象设为 0。

   参数：
     P: TCnBigComplexPolynomial           - 待设置的一元大整数复数多项式

   返回值：（无）
}

function BigComplexPolynomialIsOne(P: TCnBigComplexPolynomial): Boolean;
{* 判断一个一元大整数复数多项式对象是否为 1。

   参数：
     P: TCnBigComplexPolynomial    - 待判断的一元大整数复数多项式

   返回值：Boolean                        - 返回是否为 1
}

procedure BigComplexPolynomialSetOne(P: TCnBigComplexPolynomial);
{* 将一个一元大整数复数多项式对象设为 1。

   参数：
     P: TCnBigComplexPolynomial    - 待设置的一元大整数复数多项式

   返回值：（无）
}

function BigComplexPolynomialIsNegOne(P: TCnBigComplexPolynomial): Boolean;
{* 判断一个一元大整数复数多项式对象是否为 -1。

   参数：
     P: TCnBigComplexPolynomial           - 待判断的一元大整数复数多项式

   返回值：Boolean                        - 返回是否为 -1。
}

procedure BigComplexPolynomialNegate(P: TCnBigComplexPolynomial);
{* 将一个一元大整数复数多项式对象所有系数求反。

   参数：
     P: TCnBigComplexPolynomial           - 待计算的一元大整数复数多项式

   返回值：（无）
}

function BigComplexPolynomialIsMonic(P: TCnBigComplexPolynomial): Boolean;
{* 判断一个一元大整数复数多项式是否是首一多项式，也就是判断最高次系数是否为 1。

   参数：
     P: TCnBigComplexPolynomial           - 待判断的一元大整数复数多项式

   返回值：Boolean                        - 返回是否首一多项式
}

procedure BigComplexPolynomialShiftLeft(P: TCnBigComplexPolynomial; N: Integer);
{* 将一个一元大整数复数多项式对象左移 N 次，也就是各项指数都加 N。

   参数：
     P: TCnBigComplexPolynomial           - 待左移的一元大整数复数多项式
     N: Integer                           - 左移次数

   返回值：（无）
}

procedure BigComplexPolynomialShiftRight(P: TCnBigComplexPolynomial; N: Integer);
{* 将一个一元大整数复数多项式对象右移 N 次，也就是各项指数都减 N，小于 0 的忽略了。

   参数：
     P: TCnBigComplexPolynomial           - 待右移的一元大整数复数多项式
     N: Integer                           - 右移次数

   返回值：（无）
}

function BigComplexPolynomialEqual(A: TCnBigComplexPolynomial;
  B: TCnBigComplexPolynomial): Boolean;
{* 判断两个一元大整数复数多项式每项系数是否对应相等，是则返回 True。

   参数：
     A: TCnBigComplexPolynomial           - 待判断的一元大整数复数多项式一
     B: TCnBigComplexPolynomial           - 待判断的一元大整数复数多项式二

   返回值：Boolean                        - 返回是否相等
}

function BigComplexPolynomialAdd(Res: TCnBigComplexPolynomial;
  P1: TCnBigComplexPolynomial; P2: TCnBigComplexPolynomial): Boolean;
{* 两个一元大整数复数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigComplexPolynomial         - 用来容纳结果的一元大整数复数多项式
     P1: TCnBigComplexPolynomial          - 加数一
     P2: TCnBigComplexPolynomial          - 加数二

   返回值：Boolean                        - 返回是否相加成功
}

function BigComplexPolynomialSub(Res: TCnBigComplexPolynomial;
  P1: TCnBigComplexPolynomial; P2: TCnBigComplexPolynomial): Boolean;
{* 两个一元大整数复数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigComplexPolynomial         - 用来容纳结果的一元大整数复数多项式
     P1: TCnBigComplexPolynomial          - 被减数
     P2: TCnBigComplexPolynomial          - 减数

   返回值：Boolean                        - 返回是否相减成功
}

function BigComplexPolynomialMul(Res: TCnBigComplexPolynomial;
  P1: TCnBigComplexPolynomial; P2: TCnBigComplexPolynomial): Boolean;
{* 两个一元大整数复数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigComplexPolynomial         - 用来容纳结果的一元大整数复数多项式
     P1: TCnBigComplexPolynomial          - 乘数一
     P2: TCnBigComplexPolynomial          - 乘数二

   返回值：Boolean                        - 返回是否相乘成功
}

procedure BigComplexPolynomialMulBigComplex(P: TCnBigComplexPolynomial;
  N: TCnBigComplex);
{* 将一个一元大整数复数多项式对象的各个系数都乘以大浮点数复数 N。

   参数：
     P: TCnBigComplexPolynomial           - 待乘的一元大整数复数多项式
     N: TCnBigComplex                     - 乘数

   返回值：无。
}

function BigComplexPolynomialPower(Res: TCnBigComplexPolynomial;
  P: TCnBigComplexPolynomial; Exponent: Int64): Boolean;
{* 计算一元大整数复数多项式的 Exponent 次幂，返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnBigComplexPolynomial         - 用来容纳结果的一元大整数复数多项式
     P: TCnBigComplexPolynomial           - 底数
     Exponent: Int64                      - 指数

   返回值：Boolean                        - 返回是否计算成功
}

function BigComplexPolynomialCompose(Res: TCnBigComplexPolynomial;
  F: TCnBigComplexPolynomial; P: TCnBigComplexPolynomial): Boolean;
{* 一元大整数复数多项式代换，也就是计算 F(P(x))，返回是否计算成功，Res 可以是 F 或 P。

   参数：
     Res: TCnBigComplexPolynomial         - 用来容纳结果的一元大整数复数多项式
     F: TCnBigComplexPolynomial           - 代换原式
     P: TCnBigComplexPolynomial           - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigComplexPolynomialGetValue(Res: TCnBigComplex; F: TCnBigComplexPolynomial;
  X: TCnBigComplex);
{* 一元大整数复数多项式求值，也就是计算 F(x)。Res 不能是 X。

   参数：
     Res: TCnBigComplex                   - 用来容纳结果的无限精度复数
     F: TCnBigComplexPolynomial           - 待求值的一元大整数复数多项式
     X: TCnBigComplex                     - 未知数的值

   返回值：（无）
}

// ====================== 一元大浮点复数多项式基础运算 =============================

function BigComplexDecimalPolynomialNew: TCnBigComplexDecimalPolynomial;
{* 创建一个动态分配的一元大浮点复数多项式对象，等同于 TCnBigComplexDecimalPolynomial.Create。

   参数：
     （无）

   返回值：TCnBigComplexDecimalPolynomial - 返回创建的一元大浮点复数多项式对象
}

procedure BigComplexDecimalPolynomialFree(P: TCnBigComplexDecimalPolynomial);
{* 释放一个一元大浮点复数多项式对象，等同于 TCnBigComplexDecimalPolynomial.Free

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待释放的一元大浮点复数多项式

   返回值：（无）
}

function BigComplexDecimalPolynomialDuplicate(P: TCnBigComplexDecimalPolynomial): TCnBigComplexDecimalPolynomial;
{* 从一个一元大浮点复数多项式对象克隆一个新对象。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待复制的一元大浮点复数多项式

   返回值：TCnBigComplexDecimalPolynomial - 返回新建的一元大浮点复数多项式
}

function BigComplexDecimalPolynomialCopy(Dest: TCnBigComplexDecimalPolynomial;
  Source: TCnBigComplexDecimalPolynomial): TCnBigComplexDecimalPolynomial;
{* 复制一个一元大浮点复数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnBigComplexDecimalPolynomial                 - 目标一元大浮点复数多项式
     Source: TCnBigComplexDecimalPolynomial               - 源一元大浮点复数多项式

   返回值：TCnBigComplexDecimalPolynomial                 - 成功则返回目标对象，失败则返回 nil
}

procedure BigComplexDecimalPolynomialSwap(A: TCnBigComplexDecimalPolynomial;
  B: TCnBigComplexDecimalPolynomial);
{* 交换两个一元大浮点复数多项式对象的系数值。

   参数：
     A: TCnBigComplexDecimalPolynomial    - 待交换的一元大浮点复数多项式一
     B: TCnBigComplexDecimalPolynomial    - 待交换的一元大浮点复数多项式二

   返回值：（无）
}

function BigComplexDecimalPolynomialToString(P: TCnBigComplexDecimalPolynomial;
  const VarName: string = 'X'): string;
{* 将一个一元大浮点复数多项式对象转成字符串，未知数默认以 X 表示。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待转换的一元大浮点复数多项式
     const VarName: string                - 代表未知数的字符串

   返回值：string                         - 返回字符串
}

function BigComplexDecimalPolynomialIsZero(P: TCnBigComplexDecimalPolynomial): Boolean;
{* 判断一个一元大浮点复数多项式对象是否为 0。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待判断的一元大浮点复数多项式

   返回值：Boolean                        - 返回是否为 0
}

procedure BigComplexDecimalPolynomialSetZero(P: TCnBigComplexDecimalPolynomial);
{* 将一个一元大浮点复数多项式对象设为 0。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待设置的一元大浮点复数多项式

   返回值：（无）
}

function BigComplexDecimalPolynomialIsOne(P: TCnBigComplexDecimalPolynomial): Boolean;
{* 判断一个一元大浮点复数多项式对象是否为 1。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待判断的一元大浮点复数多项式

   返回值：Boolean                        - 返回是否为 1
}

procedure BigComplexDecimalPolynomialSetOne(P: TCnBigComplexDecimalPolynomial);
{* 将一个一元大浮点复数多项式对象设为 1。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待设置的一元大浮点复数多项式

   返回值：（无）
}

function BigComplexDecimalPolynomialIsNegOne(P: TCnBigComplexDecimalPolynomial): Boolean;
{* 判断一个一元大浮点复数多项式对象是否为 -1。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待判断的一元大浮点复数多项式

   返回值：Boolean                        - 返回是否为 -1。
}

procedure BigComplexDecimalPolynomialNegate(P: TCnBigComplexDecimalPolynomial);
{* 将一个一元大浮点复数多项式对象所有系数求反。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待计算的一元大浮点复数多项式

   返回值：（无）
}

function BigComplexDecimalPolynomialIsMonic(P: TCnBigComplexDecimalPolynomial): Boolean;
{* 判断一个一元大浮点复数多项式是否是首一多项式，也就是判断最高次系数是否为 1。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待判断的一元大浮点复数多项式

   返回值：Boolean                        - 返回是否首一多项式
}

procedure BigComplexDecimalPolynomialShiftLeft(P: TCnBigComplexDecimalPolynomial; N: Integer);
{* 将一个一元大浮点复数多项式对象左移 N 次，也就是各项指数都加 N。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待左移的一元大浮点复数多项式
     N: Integer                           - 左移次数

   返回值：（无）
}

procedure BigComplexDecimalPolynomialShiftRight(P: TCnBigComplexDecimalPolynomial; N: Integer);
{* 将一个一元大浮点复数多项式对象右移 N 次，也就是各项指数都减 N，小于 0 的忽略了。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待右移的一元大浮点复数多项式
     N: Integer                           - 右移次数

   返回值：（无）
}

function BigComplexDecimalPolynomialEqual(A: TCnBigComplexDecimalPolynomial;
  B: TCnBigComplexDecimalPolynomial): Boolean;
{* 判断两个一元大浮点复数多项式每项系数是否对应相等，是则返回 True。

   参数：
     A: TCnBigComplexDecimalPolynomial    - 待判断的一元大浮点复数多项式一
     B: TCnBigComplexDecimalPolynomial    - 待判断的一元大浮点复数多项式二

   返回值：Boolean                        - 返回是否相等
}

function BigComplexDecimalPolynomialAdd(Res: TCnBigComplexDecimalPolynomial;
  P1: TCnBigComplexDecimalPolynomial; P2: TCnBigComplexDecimalPolynomial): Boolean;
{* 两个一元大浮点复数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigComplexDecimalPolynomial  - 用来容纳结果的一元大浮点复数多项式
     P1: TCnBigComplexDecimalPolynomial   - 加数一
     P2: TCnBigComplexDecimalPolynomial   - 加数二

   返回值：Boolean                        - 返回是否相加成功
}

function BigComplexDecimalPolynomialSub(Res: TCnBigComplexDecimalPolynomial;
  P1: TCnBigComplexDecimalPolynomial; P2: TCnBigComplexDecimalPolynomial): Boolean;
{* 两个一元大浮点复数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigComplexDecimalPolynomial  - 用来容纳结果的一元大浮点复数多项式
     P1: TCnBigComplexDecimalPolynomial   - 被减数
     P2: TCnBigComplexDecimalPolynomial   - 减数

   返回值：Boolean                        - 返回是否相减成功
}

function BigComplexDecimalPolynomialMul(Res: TCnBigComplexDecimalPolynomial;
  P1: TCnBigComplexDecimalPolynomial; P2: TCnBigComplexDecimalPolynomial): Boolean;
{* 两个一元大浮点复数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigComplexDecimalPolynomial  - 用来容纳结果的一元大浮点复数多项式
     P1: TCnBigComplexDecimalPolynomial   - 乘数一
     P2: TCnBigComplexDecimalPolynomial   - 乘数二

   返回值：Boolean                        - 返回是否相乘成功
}

function BigComplexDecimalPolynomialDiv(Res: TCnBigComplexDecimalPolynomial;
  Remain: TCnBigComplexDecimalPolynomial; P: TCnBigComplexDecimalPolynomial;
  Divisor: TCnBigComplexDecimalPolynomial): Boolean;
{* 两个一元大浮点复数多项式对象相除，商放至 Res 中，余式放在 Remain 中，返回相除是否成功。
   Res 或 Remain 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。

   参数：
     Res: TCnBigComplexDecimalPolynomial                  - 用来容纳结果的一元大浮点复数多项式
     Remain: TCnBigComplexDecimalPolynomial               - 用来容纳余式的一元大浮点复数多项式
     P: TCnBigComplexDecimalPolynomial                    - 被除数
     Divisor: TCnBigComplexDecimalPolynomial              - 除数

   返回值：Boolean                                        - 返回是否相除成功
}

procedure BigComplexDecimalPolynomialMulBigComplexDecimal(P: TCnBigComplexDecimalPolynomial;
  N: TCnBigComplexDecimal);
{* 将一个一元大浮点复数多项式对象的各个系数都乘以大浮点数复数 N。

   参数：
     P: TCnBigComplexDecimalPolynomial    - 待乘的一元大浮点复数多项式
     N: TCnBigComplexDecimal              - 乘数

   返回值：无。
}

function BigComplexDecimalPolynomialPower(Res: TCnBigComplexDecimalPolynomial;
  P: TCnBigComplexDecimalPolynomial; Exponent: Int64): Boolean;
{* 计算一元大浮点复数多项式的 Exponent 次幂，返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnBigComplexDecimalPolynomial  - 用来容纳结果的一元大浮点复数多项式
     P: TCnBigComplexDecimalPolynomial    - 底数
     Exponent: Int64                      - 指数

   返回值：Boolean                        - 返回是否计算成功
}

function BigComplexDecimalPolynomialCompose(Res: TCnBigComplexDecimalPolynomial;
  F: TCnBigComplexDecimalPolynomial; P: TCnBigComplexDecimalPolynomial): Boolean;
{* 一元大浮点复数多项式代换，也就是计算 F(P(x))，返回是否计算成功，Res 可以是 F 或 P。

   参数：
     Res: TCnBigComplexDecimalPolynomial  - 用来容纳结果的一元大浮点复数多项式
     F: TCnBigComplexDecimalPolynomial    - 代换原式
     P: TCnBigComplexDecimalPolynomial    - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigComplexDecimalPolynomialGetValue(Res: TCnBigComplexDecimal; F: TCnBigComplexDecimalPolynomial;
  X: TCnBigComplexDecimal);
{* 一元大浮点复数多项式求值，也就是计算 F(x)。Res 不能是 X。

   参数：
     Res: TCnBigComplexDecimal            - 用来容纳结果的无限精度复数
     F: TCnBigComplexDecimalPolynomial    - 待求值的一元大浮点复数多项式
     X: TCnBigComplexDecimal              - 未知数的值

   返回值：（无）
}

// ====================== 一元整系数多项式基础运算 =============================

function Int64PolynomialNew: TCnInt64Polynomial;
{* 创建一个动态分配的一元整系数多项式对象，等同于 TCnInt64Polynomial.Create。

   参数：
     （无）

   返回值：TCnInt64Polynomial             - 返回创建的一元整系数多项式对象
}

procedure Int64PolynomialFree(P: TCnInt64Polynomial);
{* 释放一个一元整系数多项式对象，等同于 TCnInt64Polynomial.Free

   参数：
     P: TCnInt64Polynomial                - 待释放的一元整系数多项式

   返回值：（无）
}

function Int64PolynomialDuplicate(P: TCnInt64Polynomial): TCnInt64Polynomial;
{* 从一个一元整系数多项式对象克隆一个新对象。

   参数：
     P: TCnInt64Polynomial                - 待复制的一元整系数多项式

   返回值：TCnInt64Polynomial             - 返回新建的一元整系数多项式
}

function Int64PolynomialCopy(Dest: TCnInt64Polynomial; Source: TCnInt64Polynomial): TCnInt64Polynomial;
{* 复制一个一元整系数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnInt64Polynomial             - 目标一元整系数多项式
     Source: TCnInt64Polynomial           - 源一元整系数多项式

   返回值：TCnInt64Polynomial             - 成功则返回目标对象，失败则返回 nil
}

procedure Int64PolynomialSwap(A: TCnInt64Polynomial; B: TCnInt64Polynomial);
{* 交换两个一元整系数多项式对象的系数值。

   参数：
     A: TCnInt64Polynomial                - 待交换的一元整系数多项式一
     B: TCnInt64Polynomial                - 待交换的一元整系数多项式二

   返回值：（无）
}

function Int64PolynomialToString(P: TCnInt64Polynomial; const VarName: string = 'X'): string;
{* 将一个一元整系数多项式对象转成字符串，未知数默认以 X 表示。

   参数：
     P: TCnInt64Polynomial                - 待转换的一元整系数多项式
     const VarName: string                - 代表未知数的字符串

   返回值：string                         - 返回字符串
}

function Int64PolynomialSetString(P: TCnInt64Polynomial;
  const Str: string; const VarName: string = 'X'): Boolean;
{* 将字符串形式的一元整系数多项式赋值给一元整系数多项式对象，返回是否赋值成功。

   参数：
     P: TCnInt64Polynomial                - 用来容纳结果的一元整系数多项式
     const Str: string                    - 多项式字符串
     const VarName: string                - 代表未知数的字符串

   返回值：Boolean                        - 返回是否赋值成功
}

function Int64PolynomialIsZero(P: TCnInt64Polynomial): Boolean;
{* 判断一个一元整系数多项式对象是否为 0。

   参数：
     P: TCnInt64Polynomial                - 待判断的一元整系数多项式

   返回值：Boolean                        - 返回是否为 0
}

procedure Int64PolynomialSetZero(P: TCnInt64Polynomial);
{* 将一个一元整系数多项式对象设为 0。

   参数：
     P: TCnInt64Polynomial                - 待设置的一元整系数多项式

   返回值：（无）
}

function Int64PolynomialIsOne(P: TCnInt64Polynomial): Boolean;
{* 判断一个一元整系数多项式对象是否为 1。

   参数：
     P: TCnInt64Polynomial                - 待判断的一元整系数多项式

   返回值：Boolean                        - 返回是否为 1
}

procedure Int64PolynomialSetOne(P: TCnInt64Polynomial);
{* 将一个一元整系数多项式对象设为 1。

   参数：
     P: TCnInt64Polynomial                - 待设置的一元整系数多项式

   返回值：（无）
}

function Int64PolynomialIsNegOne(P: TCnInt64Polynomial): Boolean;
{* 判断一个一元整系数多项式对象是否为 -1。

   参数：
     P: TCnInt64Polynomial                - 待判断的一元整系数多项式

   返回值：Boolean                        - 返回是否为 -1
}

procedure Int64PolynomialNegate(P: TCnInt64Polynomial);
{* 将一个一元整系数多项式对象所有系数求反。

   参数：
     P: TCnInt64Polynomial                - 待计算的一元整系数多项式

   返回值：（无）
}

function Int64PolynomialIsMonic(P: TCnInt64Polynomial): Boolean;
{* 判断一个一元整系数多项式是否是首一多项式，也就是判断最高次系数是否为 1。

   参数：
     P: TCnInt64Polynomial                - 待判断的一元整系数多项式

   返回值：Boolean                        - 返回是否为首一多项式
}

procedure Int64PolynomialShiftLeft(P: TCnInt64Polynomial; N: Integer);
{* 将一个一元整系数多项式对象左移 N 次，也就是各项指数都加 N。

   参数：
     P: TCnInt64Polynomial                - 待左移的一元整系数多项式
     N: Integer                           - 左移次数

   返回值：（无）
}

procedure Int64PolynomialShiftRight(P: TCnInt64Polynomial; N: Integer);
{* 将一个一元整系数多项式对象右移 N 次，也就是各项指数都减 N，小于 0 的次数则忽略了。

   参数：
     P: TCnInt64Polynomial                - 待右移的一元整系数多项式
     N: Integer                           - 右移次数

   返回值：（无）
}

function Int64PolynomialEqual(A: TCnInt64Polynomial; B: TCnInt64Polynomial): Boolean;
{* 判断俩一元整系数多项式每项系数是否对应相等，是则返回 True。

   参数：
     A: TCnInt64Polynomial                - 待判断的一元整系数多项式一
     B: TCnInt64Polynomial                - 待判断的一元整系数多项式二

   返回值：Boolean                        - 返回是否相等
}

// ====================== 一元整系数多项式普通运算 =============================

procedure Int64PolynomialAddWord(P: TCnInt64Polynomial; N: Int64);
{* 将一个一元整系数多项式对象的常系数加上 N。

   参数：
     P: TCnInt64Polynomial                - 待处理的一元整系数多项式
     N: Int64                             - 常系数加数

   返回值：（无）
}

procedure Int64PolynomialSubWord(P: TCnInt64Polynomial; N: Int64);
{* 将一个一元整系数多项式对象的常系数减去 N。

   参数：
     P: TCnInt64Polynomial                - 待处理的一元整系数多项式
     N: Int64                             - 常系数减数

   返回值：（无）
}

procedure Int64PolynomialMulWord(P: TCnInt64Polynomial; N: Int64);
{* 将一个一元整系数多项式对象的各个系数都乘以 N。

   参数：
     P: TCnInt64Polynomial                - 待处理的一元整系数多项式
     N: Int64                             - 乘数

   返回值：（无）
}

procedure Int64PolynomialDivWord(P: TCnInt64Polynomial; N: Int64);
{* 将一个一元整系数多项式对象的各个系数都除以 N，如不能整除则取整。

   参数：
     P: TCnInt64Polynomial                - 待处理的一元整系数多项式
     N: Int64                             - 除数

   返回值：（无）
}

procedure Int64PolynomialNonNegativeModWord(P: TCnInt64Polynomial; N: Int64);
{* 将一个一元整系数多项式对象的各个系数都对 N 非负求余，可以用于有限域化。

   参数：
     P: TCnInt64Polynomial                - 待处理的一元整系数多项式
     N: Int64                             - 除数

   返回值：（无）
}

function Int64PolynomialAdd(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
{* 两个一元整系数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 加数一
     P2: TCnInt64Polynomial               - 加数二

   返回值：Boolean                        - 返回是否相加成功
}

function Int64PolynomialSub(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
{* 两个一元整系数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 被减数
     P2: TCnInt64Polynomial               - 减数

   返回值：Boolean                        - 返回是否相减成功
}

function Int64PolynomialMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
{* 两个一元整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 乘数一
     P2: TCnInt64Polynomial               - 乘数二

   返回值：Boolean                        - 返回是否相乘成功
}

function Int64PolynomialDftMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
{* 两个一元整系数多项式对象使用离散傅立叶变换与离散傅立叶逆变换相乘，结果放至 Res 中，
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。
   注：使用复数提速但因为浮点缘故可能出现部分系数有个位误差，不是很推荐使用。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 乘数一
     P2: TCnInt64Polynomial               - 乘数二

   返回值：Boolean                        - 返回是否相乘成功
}

function Int64PolynomialNttMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
{* 两个一元整系数多项式对象使用快速数论变换与快速数论逆变换相乘，结果放至 Res 中，
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。
   注：多项式系数只支持 [0, CN_P) 区间，多项式项数必须小于模数的 2^23，因此适用范围也不广。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 乘数一
     P2: TCnInt64Polynomial               - 乘数二

   返回值：Boolean                        - 返回是否相乘成功
}

function Int64PolynomialDiv(Res: TCnInt64Polynomial; Remain: TCnInt64Polynomial;
  P: TCnInt64Polynomial; Divisor: TCnInt64Polynomial; ErrMulFactor: PInt64 = nil): Boolean;
{* 两个一元整系数多项式对象相除，商放至 Res 中，余式放在 Remain 中，返回相除是否成功，
   注意当商式或余式出现无法整除的分数时会返回 False，表示无法支持，调用者务必判断返回值。
   返回 False 时如 ErrMulFactor 参数不为空，则会返回被除式各系数应当乘上多少才可以整除的值。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     Remain: TCnInt64Polynomial           - 用来容纳余式的一元整系数多项式
     P: TCnInt64Polynomial                - 被除数
     Divisor: TCnInt64Polynomial          - 除数
     ErrMulFactor: PInt64                 - 提供指针，返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否相除成功
}

function Int64PolynomialMod(Res: TCnInt64Polynomial; P: TCnInt64Polynomial;
  Divisor: TCnInt64Polynomial; ErrMulFactor: PInt64 = nil): Boolean;
{* 两个一元整系数多项式对象求余，余式放至 Res 中，返回求余是否成功，
   注意当商式或余式出现无法整除的分数时会返回 False，表示无法支持，调用者务必判断返回值。
   返回 False 时如 ErrMulFactor 参数不为空，则会返回被除式各系数应当乘上多少才可以整除的值。
   Res 可以是 P 或 Divisor，P 可以是 Divisor。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64Polynomial                - 被除数
     Divisor: TCnInt64Polynomial          - 除数
     ErrMulFactor: PInt64                 - 提供指针，返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否求余成功
}

function Int64PolynomialPower(Res: TCnInt64Polynomial; P: TCnInt64Polynomial; Exponent: Int64): Boolean;
{* 计算一元整系数多项式的 Exponent 次幂，不考虑系数溢出的问题，返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64Polynomial                - 底数
     Exponent: Int64                      - 指数

   返回值：Boolean                        - 返回是否计算成功
}

function Int64PolynomialReduce(P: TCnInt64Polynomial): Int64;
{* 化简一元整系数多项式系数，也就是找多项式系数的最大公约数，各个系数除以它，返回最大公约数。

   参数：
     P: TCnInt64Polynomial                - 待化简的一元整系数多项式

   返回值：Int64                          - 返回各系数的最大公约数
}

procedure Int64PolynomialCentralize(P: TCnInt64Polynomial; Modulus: Int64);
{* 对一元整系数多项式系数进行中心化处理，也即把 [0, M - 1] 改为 [1 - (M + 1) div 2, M div 2]。
   也即大于 M div 2 的系数要减 M，注意 Modulus 不一定是素数。

   参数：
     P: TCnInt64Polynomial                - 待中心化的一元整系数多项式
     Modulus: Int64                       - 模数

   返回值：（无）
}

function Int64PolynomialGreatestCommonDivisor(Res: TCnInt64Polynomial;
  P1: TCnInt64Polynomial; P2: TCnInt64Polynomial): Boolean;
{* 计算两个一元整系数多项式的最大公因式，返回是否计算成功，Res 可以是 P1 或 P2。
   注意计算可能会因为系数不能整除而失败，即使调用者自行保证 P1 P2 均为首一多项式也不能保证。
   如返回 False，调用者可干脆认为互素，最大公因式为 1，会塞入 Res 里。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 待计算最大公因式的一元整系数多项式一
     P2: TCnInt64Polynomial               - 待计算最大公因式的一元整系数多项式二

   返回值：Boolean                        - 返回是否计算成功
}

function Int64PolynomialLeastCommonMultiple(Res: TCnInt64Polynomial;
  P1: TCnInt64Polynomial; P2: TCnInt64Polynomial): Boolean;
{* 计算两个一元整系数多项式的最小公倍式，返回是否计算成功，Res 可以是 P1 或 P2。
   注意计算可能会因为系数不能整除而失败，即使调用者自行保证 P1 P2 均为首一多项式也不能保证。
   如返回 False，调用者可干脆认为互素，最小公倍式为两者相乘，请自行计算。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 待计算最小公倍式的一元整系数多项式一
     P2: TCnInt64Polynomial               - 待计算最小公倍式的一元整系数多项式二

   返回值：Boolean                        - 返回是否计算成功
}

function Int64PolynomialCompose(Res: TCnInt64Polynomial;
  F: TCnInt64Polynomial; P: TCnInt64Polynomial): Boolean;
{* 一元整系数多项式代换，也就是计算 F(P(x))，返回是否计算成功，Res 可以是 F 或 P。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     F: TCnInt64Polynomial                - 代换原式
     P: TCnInt64Polynomial                - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64PolynomialGetValue(F: TCnInt64Polynomial; X: Int64): Int64;
{* 一元整系数多项式求值，也就是计算 F(x)，返回计算结果。

   参数：
     F: TCnInt64Polynomial                - 待求值的一元整系数多项式
     X: Int64                             - 未知数的值

   返回值：Int64                          - 返回计算结果
}

procedure Int64PolynomialReduce2(P1: TCnInt64Polynomial; P2: TCnInt64Polynomial);
{* 针对两个一元整系数多项式进行约分，也就是缩至互素，用于有理分式约分运算。

   参数：
     P1: TCnInt64Polynomial               - 待约分的一元整系数多项式一
     P2: TCnInt64Polynomial               - 待约分的一元整系数多项式二

   返回值：（无）
}

// ===================== 有限扩域下的整系数多项式模运算 ========================

function Int64PolynomialGaloisEqual(A: TCnInt64Polynomial;
  B: TCnInt64Polynomial; Prime: Int64): Boolean;
{* 两个一元整系数多项式在模 Prime 的条件下是否相等。

   参数：
     A: TCnInt64Polynomial                - 待判断的一元整系数多项式一
     B: TCnInt64Polynomial                - 待判断的一元整系数多项式二
     Prime: Int64                         - 模数

   返回值：Boolean                        - 返回是否相等
}

procedure Int64PolynomialGaloisNegate(P: TCnInt64Polynomial; Prime: Int64);
{* 将一个一元整系数多项式对象所有系数在模 Prime 的条件下求反。

   参数：
     P: TCnInt64Polynomial                - 待计算的一元整系数多项式
     Prime: Int64                         - 模数

   返回值：（无）
}

function Int64PolynomialGaloisAdd(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个一元整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 加数一
     P2: TCnInt64Polynomial               - 加数二
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否相加成功
}

function Int64PolynomialGaloisSub(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个一元整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 被减数
     P2: TCnInt64Polynomial               - 减数
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否相减成功
}

function Int64PolynomialGaloisMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个一元整系数多项式对象在 Prime 次方阶有限域上相乘，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 乘数一
     P2: TCnInt64Polynomial               - 乘数二
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否相乘成功
}

function Int64PolynomialGaloisDiv(Res: TCnInt64Polynomial; Remain: TCnInt64Polynomial;
  P: TCnInt64Polynomial; Divisor: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil;
  ErrMulFactor: PInt64 = nil): Boolean;
{* 两个一元整系数多项式对象在 Prime 次方阶有限域上相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回 False 时如 ErrMulFactor 参数不为空，则会返回被除式各系数应当乘上多少才可以整除的值。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     Remain: TCnInt64Polynomial           - 用来容纳余式的一元整系数多项式
     P: TCnInt64Polynomial                - 被除数
     Divisor: TCnInt64Polynomial          - 除数
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式
     ErrMulFactor: PInt64                 - 提供指针，返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否相除成功
}

function Int64PolynomialGaloisMod(Res: TCnInt64Polynomial; P: TCnInt64Polynomial;
  Divisor: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil;
  ErrMulFactor: PInt64 = nil): Boolean;
{* 两个一元整系数多项式对象在 Prime 次方阶有限域上求余，余数放至 Res 中，返回求余是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   返回 False 时如 ErrMulFactor 参数不为空，则会返回被除式各系数应当乘上多少才可以整除的值
   Res 可以是 P 或 Divisor，P 可以是 Divisor

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64Polynomial                - 被除数
     Divisor: TCnInt64Polynomial          - 除数
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式
     ErrMulFactor: PInt64                 - 提供指针，返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否求余成功
}

function Int64PolynomialGaloisPower(Res: TCnInt64Polynomial; P: TCnInt64Polynomial;
  Exponent: Int64; Prime: Int64; Primitive: TCnInt64Polynomial = nil;
  ExponentHi: Int64 = 0): Boolean;
{* 计算一元整系数多项式在 Prime 次方阶有限域上的 Exponent 次幂，Exponent 可以是 128 位，
   Exponent 两个部分如果是负值，自动转成 UInt64。
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64Polynomial                - 底数
     Exponent: Int64                      - 指数低 64 位
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式
     ExponentHi: Int64                    - 指数高 64 位

   返回值：Boolean                        - 返回是否计算成功
}

procedure Int64PolynomialGaloisAddWord(P: TCnInt64Polynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的一元整系数多项式的常系数加上 N 再 mod Prime。

   参数：
     P: TCnInt64Polynomial                - 待计算的一元整系数多项式
     N: Int64                             - 常系数加数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

procedure Int64PolynomialGaloisSubWord(P: TCnInt64Polynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的一元整系数多项式的常系数减去 N 再 mod Prime。

   参数：
     P: TCnInt64Polynomial                - 待计算的一元整系数多项式
     N: Int64                             - 常系数减数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

procedure Int64PolynomialGaloisMulWord(P: TCnInt64Polynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的一元整系数多项式各项系数乘以 N 再 mod Prime。

   参数：
     P: TCnInt64Polynomial                - 待计算的一元整系数多项式
     N: Int64                             - 乘数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

procedure Int64PolynomialGaloisDivWord(P: TCnInt64Polynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的一元整系数多项式各项系数除以 N，也就是乘以 N 的逆元再 mod Prime。

   参数：
     P: TCnInt64Polynomial                - 待计算的一元整系数多项式
     N: Int64                             - 常系数除数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

function Int64PolynomialGaloisMonic(P: TCnInt64Polynomial; Prime: Int64): Integer;
{* 将 Prime 次方阶有限域上的一元整系数多项式各项系数同除最高项，使首项为一，返回除的值。

   参数：
     P: TCnInt64Polynomial                - 待计算的一元整系数多项式
     Prime: Int64                         - 有限域上界

   返回值：Integer                        - 返回除的值
}

function Int64PolynomialGaloisGreatestCommonDivisor(Res: TCnInt64Polynomial;
  P1: TCnInt64Polynomial; P2: TCnInt64Polynomial; Prime: Int64): Boolean;
{* 计算两个一元整系数多项式在 Prime 次方阶有限域上的最大公因式，返回是否计算成功，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 待计算最大公因式的一元整系数多项式一
     P2: TCnInt64Polynomial               - 待计算最大公因式的一元整系数多项式二
     Prime: Int64                         - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function Int64PolynomialGaloisLeastCommonMultiple(Res: TCnInt64Polynomial;
  P1: TCnInt64Polynomial; P2: TCnInt64Polynomial; Prime: Int64): Boolean;
{* 计算两个一元整系数多项式在 Prime 次方阶有限域上的最小公倍式，返回是否计算成功，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P1: TCnInt64Polynomial               - 待计算最小公倍式的一元整系数多项式一
     P2: TCnInt64Polynomial               - 待计算最小公倍式的一元整系数多项式二
     Prime: Int64                         - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

procedure Int64PolynomialGaloisExtendedEuclideanGcd(A: TCnInt64Polynomial;
  B: TCnInt64Polynomial; X: TCnInt64Polynomial; Y: TCnInt64Polynomial; Prime: Int64);
{* 扩展欧几里得辗转相除法在 Prime 次方阶有限域上求二元一次不定整系数多项式方程 A * X + B * Y = 1 的解。

   参数：
     A: TCnInt64Polynomial                - 二元一次不定整系数多项式方程系数 A
     B: TCnInt64Polynomial                - 二元一次不定整系数多项式方程系数 B
     X: TCnInt64Polynomial                - 用来容纳结果 X 的一元整系数多项式
     Y: TCnInt64Polynomial                - 用来容纳结果 Y 的一元整系数多项式
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

procedure Int64PolynomialGaloisModularInverse(Res: TCnInt64Polynomial;
  X: TCnInt64Polynomial; Modulus: TCnInt64Polynomial; Prime: Int64; CheckGcd: Boolean = False);
{* 求一元整系数多项式 X 在 Prime 次方阶有限域上针对 Modulus 的模反多项式或叫模逆元多项式 Y，
   满足 (X * Y) mod M = 1，调用者须尽量保证 X、Modulus 互素，且 Res 不能为 X 或 Modulus。
   CheckGcd 参数为 True 时，内部会检查 X、Modulus 是否互素，不互素则抛出异常。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     X: TCnInt64Polynomial                - 待计算的一元整系数多项式
     Modulus: TCnInt64Polynomial          - 模数
     Prime: Int64                         - 有限域上界
     CheckGcd: Boolean                    - 是否检查互素

   返回值：（无）
}

function Int64PolynomialGaloisPrimePowerModularInverse(Res: TCnInt64Polynomial;
  X: TCnInt64Polynomial; Modulus: TCnInt64Polynomial; PrimeRoot: Integer; Exponent: Integer): Boolean;
{* 求一元整系数多项式 X 在素域的多次幂模，也就是在 PrimeRoot 的 Exponent 次方阶有限域上，
   针对 Modulus 求 X 的模反多项式或叫模逆元多项式 Y，满足 (X * Y) mod M = 1。
   返回求逆是否成功，Res 不能为 X 或 Modulus。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     X: TCnInt64Polynomial                - 待计算的一元整系数多项式
     Modulus: TCnInt64Polynomial          - 模数
     PrimeRoot: Integer                   - 有限域素数
     Exponent: Integer                    - 有限域指数

   返回值：Boolean                        - 返回是否计算成功
}

function Int64PolynomialGaloisCompose(Res: TCnInt64Polynomial; F: TCnInt64Polynomial;
  P: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 在 Prime 次方阶有限域上进行一元整系数多项式代换，也就是计算 F(P(x))，返回是否计算成功，Res 可以是 F 或 P。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     F: TCnInt64Polynomial                - 代换原式
     P: TCnInt64Polynomial                - 待代换式
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64PolynomialGaloisGetValue(F: TCnInt64Polynomial; X: Int64; Prime: Int64): Int64;
{* 在 Prime 次方阶有限域上进行一元整系数多项式求值，也就是计算 F(x)，返回计算结果。

   参数：
     F: TCnInt64Polynomial                - 待求值的一元整系数多项式
     X: Int64                             - 未知数的值
     Prime: Int64                         - 有限域上界

   返回值：Int64                          - 返回计算结果
}

function Int64PolynomialGaloisCalcDivisionPolynomial(A: Int64; B: Int64; Degree: Int64;
  OutDivisionPolynomial: TCnInt64Polynomial; Prime: Int64): Boolean;
{* 递归计算指定椭圆曲线在 Prime 次方阶有限域上的 N 阶可除多项式，返回是否计算成功。
   注意 Degree 是奇数时，可除多项式是纯 x 的多项式，偶数时，是（x 的多项式）* y 的形式，
   本结果只给出 x 的多项式部分。
   规则参考自 F. MORAIN 的文章并加上除以 2 的推导修正
  《COMPUTING THE CARDINALITY OF CM ELLIPTIC CURVES USING TORSION POINTS》

   参数：
     A: Int64                                             - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: Int64                                             - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     Degree: Int64                                        - 需计算的可除多项式阶数
     OutDivisionPolynomial: TCnInt64Polynomial            - 用来容纳结果的一元整系数多项式
     Prime: Int64                                         - 有限域上界

   返回值：Boolean                                        - 返回是否计算成功
}

procedure Int64PolynomialGaloisReduce2(P1: TCnInt64Polynomial; P2: TCnInt64Polynomial; Prime: Int64);
{* 在 Prime 次方阶有限域上针对两个一元整系数多项式进行约分，也就是缩至互素，用于有理分式约分运算。

   参数：
     P1: TCnInt64Polynomial               - 待约分的一元整系数多项式一
     P2: TCnInt64Polynomial               - 待约分的一元整系数多项式二
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

// ===================== 一元整系数有理分式常规运算 ============================

function Int64RationalPolynomialEqual(R1: TCnInt64RationalPolynomial;
  R2: TCnInt64RationalPolynomial): Boolean;
{* 比较两个一元整系数有理分式是否相等。

   参数：
     R1: TCnInt64RationalPolynomial       - 待比较的一元整系数有理分式一
     R2: TCnInt64RationalPolynomial       - 待比较的一元整系数有理分式二

   返回值：Boolean                        - 返回是否相等
}

function Int64RationalPolynomialCopy(Dest: TCnInt64RationalPolynomial;
  Source: TCnInt64RationalPolynomial): TCnInt64RationalPolynomial;
{* 一元整系数有理分式复制。

   参数：
     Dest: TCnInt64RationalPolynomial     - 目标一元整系数有理分式
     Source: TCnInt64RationalPolynomial   - 源一元整系数有理分式

   返回值：TCnInt64RationalPolynomial     - 成功则返回目标对象，失败则返回 nil
}

procedure Int64RationalPolynomialAdd(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial); overload;
{* 一元整系数有理分式普通加法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 加数一
     R2: TCnInt64RationalPolynomial                       - 加数二

   返回值：（无）
}

procedure Int64RationalPolynomialSub(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial); overload;
{* 一元整系数有理分式普通减法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被减数
     R2: TCnInt64RationalPolynomial                       - 减数

   返回值：（无）
}

procedure Int64RationalPolynomialMul(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial); overload;
{* 一元整系数有理分式普通乘法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 乘数一
     R2: TCnInt64RationalPolynomial                       - 乘数二

   返回值：（无）
}

procedure Int64RationalPolynomialDiv(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial); overload;
{* 一元整系数有理分式普通除法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被除数
     R2: TCnInt64RationalPolynomial                       - 除数

   返回值：（无）
}

procedure Int64RationalPolynomialAddWord(R: TCnInt64RationalPolynomial; N: Int64);
{* 一元整系数有理分式普通加法加上 Int64。

   参数：
     R: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     N: Int64                             - 加数

   返回值：（无）
}

procedure Int64RationalPolynomialSubWord(R: TCnInt64RationalPolynomial; N: Int64);
{* 一元整系数有理分式普通减法减去 Int64。

   参数：
     R: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     N: Int64                             - 减数

   返回值：（无）
}

procedure Int64RationalPolynomialMulWord(R: TCnInt64RationalPolynomial; N: Int64);
{* 一元整系数有理分式普通乘法乘以 Int64。

   参数：
     R: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     N: Int64                             - 乘数

   返回值：（无）
}

procedure Int64RationalPolynomialDivWord(R: TCnInt64RationalPolynomial; N: Int64);
{* 一元整系数有理分式普通除法除以 Int64。

   参数：
     R: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     N: Int64                             - 除数

   返回值：（无）
}

procedure Int64RationalPolynomialAdd(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial); overload;
{* 一元整系数有理分式与整系数多项式的普通加法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 加数一
     P1: TCnInt64Polynomial                               - 加数二

   返回值：（无）
}

procedure Int64RationalPolynomialSub(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial); overload;
{* 一元整系数有理分式与整系数多项式的普通减法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被减数
     P1: TCnInt64Polynomial                               - 减数

   返回值：（无）
}

procedure Int64RationalPolynomialMul(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial); overload;
{* 一元整系数有理分式与整系数多项式的普通乘法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 乘数一
     P1: TCnInt64Polynomial                               - 乘数二

   返回值：（无）
}

procedure Int64RationalPolynomialDiv(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial); overload;
{* 一元整系数有理分式与整系数多项式的普通除法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被除数
     P1: TCnInt64Polynomial                               - 除数

   返回值：（无）
}

function Int64RationalPolynomialCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64RationalPolynomial; P: TCnInt64RationalPolynomial): Boolean; overload;
{* 一元整系数有理分式代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnInt64RationalPolynomial      - 用来容纳结果的一元整系数有理分式
     F: TCnInt64RationalPolynomial        - 代换原式
     P: TCnInt64RationalPolynomial        - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64RationalPolynomialCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64RationalPolynomial; P: TCnInt64Polynomial): Boolean; overload;
{* 一元整系数有理分式代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnInt64RationalPolynomial      - 用来容纳结果的一元整系数有理分式
     F: TCnInt64RationalPolynomial        - 代换原式
     P: TCnInt64Polynomial                - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64RationalPolynomialCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64Polynomial; P: TCnInt64RationalPolynomial): Boolean; overload;
{* 一元整系数有理分式代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnInt64RationalPolynomial      - 用来容纳结果的一元整系数有理分式
     F: TCnInt64Polynomial                - 代换原式
     P: TCnInt64RationalPolynomial        - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

procedure Int64RationalPolynomialGetValue(Res: TCnRationalNumber;
  F: TCnInt64RationalPolynomial; X: Int64);
{* 一元整系数有理分式求值，也就是计算 F(x)，将结果放在 Res 中。

   参数：
     Res: TCnRationalNumber               - 用来容纳结果的有理数对象
     F: TCnInt64RationalPolynomial        - 待求值的一元整系数有理分式
     X: Int64                             - 未知数的值

   返回值：（无）
}

// ================= 一元整系数有理分式在有限域上的模运算 ======================

function Int64RationalPolynomialGaloisEqual(R1: TCnInt64RationalPolynomial;
  R2: TCnInt64RationalPolynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 比较两个模系数一元整系数有理分式是否相等。

   参数：
     R1: TCnInt64RationalPolynomial       - 待比较的一元整系数有理分式一
     R2: TCnInt64RationalPolynomial       - 待比较的一元整系数有理分式二
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否相等
}

procedure Int64RationalPolynomialGaloisNegate(P: TCnInt64RationalPolynomial;
  Prime: Int64);
{* 将一个一元整系数有理分式对象分子的所有系数在模 Prime 的条件下求反。

   参数：
     P: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     Prime: Int64                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisAdd(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial;
  Prime: Int64); overload;
{* 一元整系数有理分式模系数加法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 加数一
     R2: TCnInt64RationalPolynomial                       - 加数二
     Prime: Int64                                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisSub(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial;
  Prime: Int64); overload;
{* 一元整系数有理分式模系数减法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被减数
     R2: TCnInt64RationalPolynomial                       - 减数
     Prime: Int64                                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisMul(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial;
  Prime: Int64); overload;
{* 一元整系数有理分式模系数乘法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 乘数一
     R2: TCnInt64RationalPolynomial                       - 乘数二
     Prime: Int64                                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisDiv(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; R2: TCnInt64RationalPolynomial;
  Prime: Int64); overload;
{* 一元整系数有理分式模系数除法，三个参数可以是同一对象。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被除数
     R2: TCnInt64RationalPolynomial                       - 除数
     Prime: Int64                                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisAddWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
{* 一元整系数有理分式模系数加法加上 Int64。

   参数：
     R: TCnInt64RationalPolynomial                   - 待计算的一元整系数有理分式
     N: Int64                             - 加数
     Prime: Int64                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisSubWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
{* 一元整系数有理分式模系数减法减去 Int64。

   参数：
     R: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     N: Int64                             - 减数
     Prime: Int64                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisMulWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
{* 一元整系数有理分式模系数乘法乘以 Int64。

   参数：
     R: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     N: Int64                             - 乘数
     Prime: Int64                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisDivWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
{* 一元整系数有理分式模系数除法除以 Int64。

   参数：
     R: TCnInt64RationalPolynomial        - 待计算的一元整系数有理分式
     N: Int64                             - 除数
     Prime: Int64                         - 模数

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisAdd(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64); overload;
{* 一元整系数有理分式与整系数多项式的模系数加法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 加数一
     P1: TCnInt64Polynomial                               - 加数二
     Prime: Int64                                         - 有限域上界

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisSub(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64); overload;
{* 一元整系数有理分式与整系数多项式的模系数减法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被减数
     P1: TCnInt64Polynomial                               - 减数
     Prime: Int64                                         - 有限域上界

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisMul(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64); overload;
{* 一元整系数有理分式与整系数多项式的模系数乘法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 乘数一
     P1: TCnInt64Polynomial                               - 乘数二
     Prime: Int64                                         - 有限域上界

   返回值：（无）
}

procedure Int64RationalPolynomialGaloisDiv(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64); overload;
{* 一元整系数有理分式与整系数多项式的模系数除法，RationalResult 可以是 R1。

   参数：
     Res: TCnInt64RationalPolynomial                      - 用来容纳结果的一元整系数有理分式
     R1: TCnInt64RationalPolynomial                       - 被除数
     P1: TCnInt64Polynomial                               - 除数
     Prime: Int64                                         - 模数

   返回值：（无）
}

function Int64RationalPolynomialGaloisCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64RationalPolynomial; P: TCnInt64RationalPolynomial; Prime: Int64;
  Primitive: TCnInt64Polynomial = nil): Boolean; overload;
{* 一元整系数有理分式模系数代换，也就是计算 F(P(x))，返回是否计算成功

   参数：
     Res: TCnInt64RationalPolynomial      - 用来容纳结果的一元整系数有理分式
     F: TCnInt64RationalPolynomial        - 代换原式
     P: TCnInt64RationalPolynomial        - 待代换式
     Prime: Int64                         - 模数
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64RationalPolynomialGaloisCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64RationalPolynomial; P: TCnInt64Polynomial; Prime: Int64;
  Primitive: TCnInt64Polynomial = nil): Boolean; overload;
{* 一元整系数有理分式模系数代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnInt64RationalPolynomial      - 用来容纳结果的一元整系数有理分式
     F: TCnInt64RationalPolynomial        - 代换原式
     P: TCnInt64Polynomial                - 待代换式
     Prime: Int64                         - 模数
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64RationalPolynomialGaloisCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64Polynomial; P: TCnInt64RationalPolynomial; Prime: Int64;
  Primitive: TCnInt64Polynomial = nil): Boolean; overload;
{* 一元整系数有理分式模系数代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnInt64RationalPolynomial      - 用来容纳结果的一元整系数有理分式
     F: TCnInt64Polynomial                - 代换原式
     P: TCnInt64RationalPolynomial        - 待代换式
     Prime: Int64                         - 模数
     Primitive: TCnInt64Polynomial        - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64RationalPolynomialGaloisGetValue(F: TCnInt64RationalPolynomial;
  X: Int64; Prime: Int64): Int64;
{* 一元整系数有理分式模系数求值，也就是模计算 F(x)，除法用乘法模逆元表示。

   参数：
     F: TCnInt64RationalPolynomial        - 待求值的一元整系数有理分式
     X: Int64                             - 未知数的值
     Prime: Int64                         - 模数

   返回值：Int64                          - 返回求值结果
}

// ===================== 一元大整系数多项式基础运算 ============================

function BigNumberPolynomialNew: TCnBigNumberPolynomial;
{* 创建一个动态分配的一元大整系数多项式对象，等同于 TCnBigNumberPolynomial.Create。

   参数：
     （无）

   返回值：TCnBigNumberPolynomial         - 返回创建的一元大整系数多项式对象
}

procedure BigNumberPolynomialFree(P: TCnBigNumberPolynomial);
{* 释放一个一元大整系数多项式对象，等同于 TCnBigNumberPolynomial.Free。

   参数：
     P: TCnBigNumberPolynomial            - 待释放的一元大整系数多项式对象

   返回值：（无）
}

function BigNumberPolynomialDuplicate(P: TCnBigNumberPolynomial): TCnBigNumberPolynomial;
{* 从一个一元大整系数多项式对象克隆一个新对象。

   参数：
     P: TCnBigNumberPolynomial            - 待复制的一元大整系数多项式

   返回值：TCnBigNumberPolynomial         - 返回新建的一元大整系数多项式
}

function BigNumberPolynomialCopy(Dest: TCnBigNumberPolynomial;
  Source: TCnBigNumberPolynomial): TCnBigNumberPolynomial;
{* 复制一个一元大整系数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnBigNumberPolynomial         - 目标一元大整系数多项式
     Source: TCnBigNumberPolynomial       - 源一元大整系数多项式

   返回值：TCnBigNumberPolynomial         - 成功则返回目标对象，失败则返回 nil
}

procedure BigNumberPolynomialSwap(A: TCnBigNumberPolynomial; B: TCnBigNumberPolynomial);
{* 交换两个一元大整系数多项式对象的系数值。

   参数：
     A: TCnBigNumberPolynomial            - 待交换的一元整系数多项式一
     B: TCnBigNumberPolynomial            - 待交换的一元整系数多项式二

   返回值：（无）
}

function BigNumberPolynomialToString(P: TCnBigNumberPolynomial;
  const VarName: string = 'X'): string;
{* 将一个一元大整系数多项式对象转成字符串，未知数默认以 X 表示。

   参数：
     P: TCnBigNumberPolynomial            - 待转换的一元大整系数多项式
     const VarName: string                - 代表未知数的字符串

   返回值：string                         - 返回字符串
}

function BigNumberPolynomialSetString(P: TCnBigNumberPolynomial;
  const Str: string; const VarName: string = 'X'): Boolean;
{* 将字符串形式的一元大整系数多项式赋值给整系数多项式对象，返回是否赋值成功。

   参数：
     P: TCnBigNumberPolynomial            - 待赋值的一元大整系数多项式
     const Str: string                    - 多项式字符串
     const VarName: string                - 代表未知数的字符串

   返回值：Boolean                        - 返回是否赋值成功
}

function BigNumberPolynomialIsZero(P: TCnBigNumberPolynomial): Boolean;
{* 判断一个一元大整系数多项式对象是否为 0。

   参数：
     P: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式

   返回值：Boolean                        - 返回是否为 0
}

procedure BigNumberPolynomialSetZero(P: TCnBigNumberPolynomial);
{* 将一个一元大整系数多项式对象设为 0。

   参数：
     P: TCnBigNumberPolynomial            - 待设置的一元大整系数多项式

   返回值：（无）
}

function BigNumberPolynomialIsOne(P: TCnBigNumberPolynomial): Boolean;
{* 判断一个一元大整系数多项式对象是否为 1。

   参数：
     P: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式

   返回值：Boolean                        - 返回是否为 1
}

procedure BigNumberPolynomialSetOne(P: TCnBigNumberPolynomial);
{* 将一个一元大整系数多项式对象设为 1。

   参数：
     P: TCnBigNumberPolynomial            - 待设置的一元大整系数多项式

   返回值：（无）
}

function BigNumberPolynomialIsNegOne(P: TCnBigNumberPolynomial): Boolean;
{* 判断一个一元大整系数多项式对象是否为 -1。

   参数：
     P: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式

   返回值：Boolean                        - 返回是否为 -1。
}

procedure BigNumberPolynomialNegate(P: TCnBigNumberPolynomial);
{* 将一个一元大整系数多项式对象所有系数求反。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式

   返回值：（无）
}

function BigNumberPolynomialIsMonic(P: TCnBigNumberPolynomial): Boolean;
{* 判断一个一元大整系数多项式是否是首一多项式，也就是判断最高次系数是否为 1。

   参数：
     P: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式

   返回值：Boolean                        - 返回是否首一多项式
}

procedure BigNumberPolynomialShiftLeft(P: TCnBigNumberPolynomial; N: Integer);
{* 将一个一元大整系数多项式对象左移 N 次，也就是各项指数都加 N。

   参数：
     P: TCnBigNumberPolynomial            - 待左移的一元大整系数多项式
     N: Integer                           - 左移次数

   返回值：（无）
}

procedure BigNumberPolynomialShiftRight(P: TCnBigNumberPolynomial; N: Integer);
{* 将一个一元大整系数多项式对象右移 N 次，也就是各项指数都减 N，小于 0 的忽略了。

   参数：
     P: TCnBigNumberPolynomial            - 待右移的一元大整系数多项式
     N: Integer                           - 右移次数

   返回值：（无）
}

function BigNumberPolynomialEqual(A: TCnBigNumberPolynomial; B: TCnBigNumberPolynomial): Boolean;
{* 判断两个一元大整系数多项式每项系数是否对应相等，是则返回 True。

   参数：
     A: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式一
     B: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式二

   返回值：Boolean                        - 返回是否相等
}

// ======================== 一元大整系数多项式普通运算 =============================

procedure BigNumberPolynomialAddWord(P: TCnBigNumberPolynomial; N: Cardinal);
{* 将一个一元大整系数多项式对象的常系数加上 N。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: Cardinal                          - 常系数加数

   返回值：（无）
}

procedure BigNumberPolynomialSubWord(P: TCnBigNumberPolynomial; N: Cardinal);
{* 将一个一元大整系数多项式对象的常系数减去 N。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: Cardinal                          - 常系数减数

   返回值：（无）
}

procedure BigNumberPolynomialMulWord(P: TCnBigNumberPolynomial; N: Cardinal);
{* 将一个一元大整系数多项式对象的各个系数都乘以 N。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: Cardinal                          - 乘数

   返回值：（无）
}

procedure BigNumberPolynomialDivWord(P: TCnBigNumberPolynomial; N: Cardinal);
{* 将一个一元大整系数多项式对象的各个系数都除以 N，如不能整除则取整。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: Cardinal                          - 除数

   返回值：（无）
}

procedure BigNumberPolynomialNonNegativeModWord(P: TCnBigNumberPolynomial; N: Cardinal);
{* 将一个一元大整系数多项式对象的各个系数都对 N 非负求余，可以用于有限域化。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: Cardinal                          - 除数

   返回值：（无）
}

procedure BigNumberPolynomialAddBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
{* 将一个一元大整系数多项式对象的常系数加上大数 N。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: TCnBigNumber                      - 加数

   返回值：（无）
}

procedure BigNumberPolynomialSubBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
{* 将一个一元大整系数多项式对象的常系数减去大数 N。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: TCnBigNumber                      - 减数

   返回值：（无）
}

procedure BigNumberPolynomialMulBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
{* 将一个一元大整系数多项式对象的各个系数都乘以大数 N。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: TCnBigNumber                      - 乘数

   返回值：（无）
}

procedure BigNumberPolynomialDivBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
{* 将一个一元大整系数多项式对象的各个系数都除以大数 N，如不能整除则取整。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: TCnBigNumber                      - 除数

   返回值：（无）
}

procedure BigNumberPolynomialNonNegativeModBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
{* 将一个一元大整系数多项式对象的各个系数都对大数 N 非负求余。

   参数：
     P: TCnBigNumberPolynomial            - 待处理的一元大整系数多项式
     N: TCnBigNumber                      - 除数

   返回值：（无）
}

function BigNumberPolynomialAdd(Res: TCnBigNumberPolynomial; P1: TCnBigNumberPolynomial;
  P2: TCnBigNumberPolynomial): Boolean;
{* 两个一元大整系数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 加数一
     P2: TCnBigNumberPolynomial           - 加数二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialSub(Res: TCnBigNumberPolynomial; P1: TCnBigNumberPolynomial;
  P2: TCnBigNumberPolynomial): Boolean;
{* 两个一元大整系数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 被减数
     P2: TCnBigNumberPolynomial           - 减数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialMul(Res: TCnBigNumberPolynomial; P1: TCnBigNumberPolynomial;
  P2: TCnBigNumberPolynomial): Boolean;
{* 两个一元大整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 乘数一
     P2: TCnBigNumberPolynomial           - 乘数二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialDiv(Res: TCnBigNumberPolynomial; Remain: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Divisor: TCnBigNumberPolynomial; ErrMulFactor: TCnBigNumber = nil): Boolean;
{* 两个一元大整系数多项式对象相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   注意当商式或余式出现无法整除的分数时会返回 False，表示无法支持，调用者务必判断返回值。
   返回 False 时如 ErrMulFactor 参数不为空，则会返回被除式各系数应当乘上多少才可以整除的值。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     Remain: TCnBigNumberPolynomial       - 用来容纳余数的一元大整系数多项式
     P: TCnBigNumberPolynomial            - 被除数
     Divisor: TCnBigNumberPolynomial      - 除数
     ErrMulFactor: TCnBigNumber           - 返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialMod(Res: TCnBigNumberPolynomial; P: TCnBigNumberPolynomial;
  Divisor: TCnBigNumberPolynomial; ErrMulFactor: TCnBigNumber = nil): Boolean;
{* 两个一元大整系数多项式对象求余，余数放至 Res 中，返回求余是否成功，
   注意当商式或余式出现无法整除的分数时会返回 False，表示无法支持，调用者务必判断返回值。
   返回 False 时如 ErrMulFactor 参数不为空，则会返回被除式各系数应当乘上多少才可以整除的值。
   Res 可以是 P 或 Divisor，P 可以是 Divisor。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberPolynomial            - 被除数
     Divisor: TCnBigNumberPolynomial      - 除数
     ErrMulFactor: TCnBigNumber           - 返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialPower(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Exponent: TCnBigNumber): Boolean;
{* 计算一元大整系数多项式的 Exponent 次幂，返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberPolynomial            - 底数
     Exponent: TCnBigNumber               - 指数

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberPolynomialReduce(P: TCnBigNumberPolynomial);
{* 化简一元大整系数多项式系数，也就是找多项式系数的最大公约数，各个系数除以它。

   参数：
     P: TCnBigNumberPolynomial            - 待化简的一元大整系数多项式

   返回值：（无）
}

procedure BigNumberPolynomialCentralize(P: TCnBigNumberPolynomial; Modulus: TCnBigNumber);
{* 对一元大整系数多项式系数进行中心化处理，也即把 [0, M - 1] 改为 [1 - (M + 1) div 2, M div 2]。
   也即大于 M div 2 的系数要减 M，注意 Modulus 不一定是素数。

   参数：
     P: TCnBigNumberPolynomial            - 待中心化的一元大整系数多项式
     Modulus: TCnBigNumber                - 模数

   返回值：（无）
}

function BigNumberPolynomialGreatestCommonDivisor(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial): Boolean;
{* 计算两个一元大整系数多项式的最大公因式，返回是否计算成功，Res 可以是 P1 或 P2。
   注意计算可能会因为系数不能整除而失败，即使调用者自行保证 P1 P2 均为首一多项式也不能保证行。
   如返回 False，调用者可干脆认为互素，最大公因式为 1。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 待计算最大公因式的一元大整系数多项式一
     P2: TCnBigNumberPolynomial           - 待计算最大公因式的一元大整系数多项式二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialLeastCommonMultiple(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial): Boolean;
{* 计算两个一元大整系数多项式的最小公倍式，返回是否计算成功，Res 可以是 P1 或 P2。
   注意计算可能会因为系数不能整除而失败，即使调用者自行保证 P1 P2 均为首一多项式也不能保证行。
   如返回 False，调用者可干脆认为互素，最小公倍式为两者相乘，请自行计算。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 待计算最小公倍式的一元大整系数多项式一
     P2: TCnBigNumberPolynomial           - 待计算最小公倍式的一元大整系数多项式二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialCompose(Res: TCnBigNumberPolynomial;
  F: TCnBigNumberPolynomial; P: TCnBigNumberPolynomial): Boolean;
{* 一元大整系数多项式代换，也就是计算 F(P(x))，返回是否计算成功，Res 可以是 F 或 P。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     F: TCnBigNumberPolynomial            - 代换原式
     P: TCnBigNumberPolynomial            - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberPolynomialGetValue(Res: TCnBigNumber; F: TCnBigNumberPolynomial;
  X: TCnBigNumber);
{* 一元大整系数多项式求值，也就是计算 F(x)。Res 不能是 X。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     F: TCnBigNumberPolynomial            - 待求值的一元大整系数多项式
     X: TCnBigNumber                      - 未知数的值

   返回值：（无）
}

procedure BigNumberPolynomialReduce2(P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial);
{* 针对两个一元大整系数多项式进行约分，也就是缩至互素，用于有理分式约分运算。

   参数：
     P1: TCnBigNumberPolynomial           - 待约分的一元大整系数多项式一
     P2: TCnBigNumberPolynomial           - 待约分的一元大整系数多项式二

   返回值：（无）
}

// ===================== 有限扩域下的整系数多项式模运算 ========================

function BigNumberPolynomialGaloisEqual(A: TCnBigNumberPolynomial;
  B: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean;
{* 两个一元大整系数多项式在模 Prime 的条件下是否相等

   参数：
     A: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式一
     B: TCnBigNumberPolynomial            - 待判断的一元大整系数多项式二
     Prime: TCnBigNumber                  - 模数

   返回值：Boolean                        - 返回是否相等
}

procedure BigNumberPolynomialGaloisNegate(P: TCnBigNumberPolynomial;
  Prime: TCnBigNumber);
{* 将一个一元大整系数多项式对象所有系数在模 Prime 的条件下求反

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     Prime: TCnBigNumber                  - 模数

   返回值：（无）
}

function BigNumberPolynomialGaloisAdd(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
{* 两个一元大整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 加数一
     P2: TCnBigNumberPolynomial           - 加数二
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisSub(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
{* 两个一元大整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 被减数
     P2: TCnBigNumberPolynomial           - 减数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisMul(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
{* 两个一元大整系数多项式对象在 Prime 次方阶有限域上相乘，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 乘数一
     P2: TCnBigNumberPolynomial           - 乘数二
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisDiv(Res: TCnBigNumberPolynomial;
  Remain: TCnBigNumberPolynomial; P: TCnBigNumberPolynomial;
  Divisor: TCnBigNumberPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial = nil; ErrMulFactor: TCnBigNumber = nil): Boolean;
{* 两个一元大整系数多项式对象在 Prime 次方阶有限域上相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     Remain: TCnBigNumberPolynomial       - 用来容纳余式的一元大整系数多项式
     P: TCnBigNumberPolynomial            - 被除数
     Divisor: TCnBigNumberPolynomial      - 除数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式
     ErrMulFactor: TCnBigNumber           - 返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisMod(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Divisor: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil; ErrMulFactor: TCnBigNumber = nil): Boolean;
{* 两个一元大整系数多项式对象在 Prime 次方阶有限域上求余，余数放至 Res 中，返回求余是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   Res 可以是 P 或 Divisor，P 可以是 Divisor。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberPolynomial            - 被除数
     Divisor: TCnBigNumberPolynomial      - 除数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式
     ErrMulFactor: TCnBigNumber           - 返回值 False 时此处可返回被除式各系数应当乘上多少才可以整除的值

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisPower(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Exponent: TCnBigNumber; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial = nil): Boolean; overload;
{* 计算一元大整系数多项式在 Prime 次方阶有限域上的 Exponent 次幂，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberPolynomial            - 底数
     Exponent: TCnBigNumber               - 指数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisPower(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Exponent: Cardinal; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial = nil): Boolean; overload;
{* 计算一元大整系数多项式在 Prime 次方阶有限域上的 Exponent 次幂，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回是否计算成功，Res 可以是 P、

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberPolynomial            - 底数
     Exponent: Cardinal                   - 指数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisAddWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
{* 将 Prime 次方阶有限域上的一元大整系数多项式的常系数加上 N 再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: Cardinal                          - 常系数加数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisSubWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
{* 将 Prime 次方阶有限域上的一元大整系数多项式的常系数减去 N 再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: Cardinal                          - 常系数减数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisMulWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
{* 将 Prime 次方阶有限域上的一元大整系数多项式各项系数乘以 N 再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: Cardinal                          - 乘数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisDivWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
{* 将 Prime 次方阶有限域上的整系数多项式各项系数除以 N，也就是乘以 N 的逆元再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: Cardinal                          - 除数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberPolynomialGaloisAddBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的一元大整系数多项式的常系数加上 N 再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: TCnBigNumber                      - 常系数加数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure BigNumberPolynomialGaloisSubBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的一元大整系数多项式的常系数减去 N 再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: TCnBigNumber                      - 常系数减数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure BigNumberPolynomialGaloisMulBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的一元大整系数多项式各项系数乘以 N 再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: TCnBigNumber                      - 乘数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure BigNumberPolynomialGaloisDivBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的一元大整系数多项式各项系数除以 N，也就是乘以 N 的逆元再 mod Prime。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     N: TCnBigNumber                      - 除数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure BigNumberPolynomialGaloisMonic(P: TCnBigNumberPolynomial; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的一元大整系数多项式各项系数同除最高项，使首项为一。

   参数：
     P: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

function BigNumberPolynomialGaloisGreatestCommonDivisor(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean;
{* 计算两个一元大整系数多项式在 Prime 次方阶有限域上的最大公因式，返回是否计算成功，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 待计算最大公因式的一元大整系数多项式一
     P2: TCnBigNumberPolynomial           - 待计算最大公因式的一元大整系数多项式二
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisLeastCommonMultiple(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean;
{* 计算两个一元大整系数多项式在 Prime 次方阶有限域上的最小公倍式，返回是否计算成功，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P1: TCnBigNumberPolynomial           - 待计算最小公倍式的一元大整系数多项式一
     P2: TCnBigNumberPolynomial           - 待计算最小公倍式的一元大整系数多项式二
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberPolynomialGaloisExtendedEuclideanGcd(A: TCnBigNumberPolynomial;
  B: TCnBigNumberPolynomial; X: TCnBigNumberPolynomial; Y: TCnBigNumberPolynomial;
  Prime: TCnBigNumber);
{* 扩展欧几里得辗转相除法在 Prime 次方阶有限域上求二元一次不定一元大整系数多项式方程 A * X + B * Y = 1 的解。

   参数：
     A: TCnBigNumberPolynomial            - 二元一次不定大整系数多项式方程系数 A
     B: TCnBigNumberPolynomial            - 二元一次不定大整系数多项式方程系数 B
     X: TCnBigNumberPolynomial            - 用来容纳结果 X 的一元大整系数多项式
     Y: TCnBigNumberPolynomial            - 用来容纳结果 Y 的一元大整系数多项式
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure BigNumberPolynomialGaloisModularInverse(Res: TCnBigNumberPolynomial;
  X: TCnBigNumberPolynomial; Modulus: TCnBigNumberPolynomial; Prime: TCnBigNumber;
  CheckGcd: Boolean = False);
{* 求一元大整系数多项式 X 在 Prime 次方阶有限域上针对 Modulus 的模反多项式或叫模逆元多项式 Y，
   满足 (X * Y) mod M = 1，调用者须尽量保证 X、Modulus 互素，且 Res 不能为 X 或 Modulus。
   CheckGcd 参数为 True 时，内部会检查 X、Modulus 是否互素。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     X: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     Modulus: TCnBigNumberPolynomial      - 模数
     Prime: TCnBigNumber                  - 有限域上界
     CheckGcd: Boolean                    - 是否检查互素

   返回值：（无）
}

function BigNumberPolynomialGaloisPrimePowerModularInverse(Res: TCnBigNumberPolynomial;
  X: TCnBigNumberPolynomial; Modulus: TCnBigNumberPolynomial; PrimeRoot: TCnBigNumber;
  Exponent: Integer): Boolean;
{* 求一元大整系数多项式 X 在素数的多次幂模，也就是 PrimeRoot 的 Exponent 次方阶有限域上，
   针对 Modulus 求 X 的模反多项式或叫模逆元多项式 Y，满足 (X * Y) mod M = 1。
   返回求逆是否成功，Res 不能为 X 或 Modulus。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     X: TCnBigNumberPolynomial            - 待计算的一元大整系数多项式
     Modulus: TCnBigNumberPolynomial      - 模数
     PrimeRoot: TCnBigNumber              - 有限域素数
     Exponent: Integer                    - 有限域指数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisCompose(Res: TCnBigNumberPolynomial;
  F: TCnBigNumberPolynomial; P: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
{* 在 Prime 次方阶有限域上进行一元大整系数多项式代换，也就是计算 F(P(x))，返回是否计算成功，Res 可以是 F 或 P。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     F: TCnBigNumberPolynomial            - 代换原式
     P: TCnBigNumberPolynomial            - 待代换式
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisGetValue(Res: TCnBigNumber;
  F: TCnBigNumberPolynomial; X: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 在 Prime 次方阶有限域上进行一元大整系数多项式求值，也就是计算 F(x)，返回是否计算成功。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     F: TCnBigNumberPolynomial            - 待求值的一元大整系数多项式
     X: TCnBigNumber                      - 未知数的值
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisCalcDivisionPolynomial(A: Integer; B: Integer;
  Degree: Integer; OutDivisionPolynomial: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean; overload;
{* 递归计算指定椭圆曲线在 Prime 次方阶有限域上的 N 阶可除多项式，返回是否计算成功。
   注意 Degree 是奇数时，可除多项式是纯 x 的多项式，偶数时，是（x 的多项式）* y 的形式，
   本结果只给出 x 的多项式部分，也就是 f 的形式（偶数时已经除了 y），不是 Ψ的形式。
   其中 A B 是 32 位有符号整数。

   参数：
     A: Integer                                           - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: Integer                                           - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     Degree: Integer                                      - 需计算的可除多项式阶数
     OutDivisionPolynomial: TCnBigNumberPolynomial        - 用来容纳结果的一元大整系数多项式
     Prime: TCnBigNumber                                  - 有限域上界

   返回值：Boolean                                        - 返回是否计算成功
}

function BigNumberPolynomialGaloisCalcDivisionPolynomial(A: TCnBigNumber; B: TCnBigNumber;
  Degree: Integer; OutDivisionPolynomial: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean; overload;
{* 递归计算指定椭圆曲线在 Prime 次方阶有限域上的 N 阶可除多项式，返回是否计算成功
   注意 Degree 是奇数时，可除多项式是纯 x 的多项式，偶数时，是（x 的多项式）* y 的形式，
   本结果只给出 x 的多项式部分。
   规则参考自 F. MORAIN 的文章并加上除以 2 的推导修正
  《COMPUTING THE CARDINALITY OF CM ELLIPTIC CURVES USING TORSION POINTS》

   参数：
     A: TCnBigNumber                                      - 魏尔斯特拉斯椭圆曲线方程的 a 参数
     B: TCnBigNumber                                      - 魏尔斯特拉斯椭圆曲线方程的 b 参数
     Degree: Integer                                      - 需计算的可除多项式阶数
     OutDivisionPolynomial: TCnBigNumberPolynomial        - 用来容纳结果的一元大整系数多项式
     Prime: TCnBigNumber                                  - 有限域上界

   返回值：Boolean                                        - 返回是否计算成功
}

procedure BigNumberPolynomialGaloisReduce2(P1: TCnBigNumberPolynomial;
  P2: TCnBigNumberPolynomial; Prime: TCnBigNumber);
{* 在 Prime 次方阶有限域上针对两个一元大整系数多项式进行约分，也就是缩至互素，用于有理分式约分运算

   参数：
     P1: TCnBigNumberPolynomial           - 待约分的一元大整系数多项式一
     P2: TCnBigNumberPolynomial           - 待约分的一元大整系数多项式二
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

// ===================== 一元大整系数有理分式常规运算 ==========================

function BigNumberRationalPolynomialEqual(R1: TCnBigNumberRationalPolynomial;
  R2: TCnBigNumberRationalPolynomial): Boolean;
{* 比较两个一元大整系数有理分式是否相等。

   参数：
     R1: TCnBigNumberRationalPolynomial   - 待比较的一元大整系数有理分式一
     R2: TCnBigNumberRationalPolynomial   - 待比较的一元大整系数有理分式二

   返回值：Boolean                        - 返回是否相等
}

function BigNumberRationalPolynomialCopy(Dest: TCnBigNumberRationalPolynomial;
  Source: TCnBigNumberRationalPolynomial): TCnBigNumberRationalPolynomial;
{* 一元大整系数有理分式复制。

   参数：
     Dest: TCnBigNumberRationalPolynomial                 - 目标一元大整系数有理分式
     Source: TCnBigNumberRationalPolynomial               - 源一元大整系数有理分式

   返回值：TCnBigNumberRationalPolynomial                 - 成功则返回目标对象，失败则返回 nil
}

procedure BigNumberRationalPolynomialAdd(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial); overload;
{* 一元大整系数有理分式普通加法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 加数一
     R2: TCnBigNumberRationalPolynomial                   - 加数二

   返回值：（无）
}

procedure BigNumberRationalPolynomialSub(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial); overload;
{* 一元大整系数有理分式普通减法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被减数
     R2: TCnBigNumberRationalPolynomial                   - 减数

   返回值：（无）
}

procedure BigNumberRationalPolynomialMul(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial); overload;
{* 一元大整系数有理分式普通乘法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 乘数一
     R2: TCnBigNumberRationalPolynomial                   - 乘数二

   返回值：（无）
}

procedure BigNumberRationalPolynomialDiv(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial); overload;
{* 一元大整系数有理分式普通除法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被除数
     R2: TCnBigNumberRationalPolynomial                   - 除数

   返回值：（无）
}

procedure BigNumberRationalPolynomialAddBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
{* 一元大整系数有理分式普通加法，加上一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 加数

   返回值：（无）
}

procedure BigNumberRationalPolynomialSubBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
{* 一元大整系数有理分式普通减法，减去一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 减数

   返回值：（无）
}

procedure BigNumberRationalPolynomialMulBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
{* 一元大整系数有理分式普通乘法，乘以一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 乘数

   返回值：（无）
}

procedure BigNumberRationalPolynomialDivBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
{* 一元大整系数有理分式普通除法，除以一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 除数

   返回值：（无）
}

procedure BigNumberRationalPolynomialAdd(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
{* 一元大整系数有理分式与一元大整系数多项式的普通加法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 加数一
     P1: TCnBigNumberPolynomial                           - 加数二

   返回值：（无）
}

procedure BigNumberRationalPolynomialSub(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
{* 一元大整系数有理分式与一元大整系数多项式的普通减法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被减数
     P1: TCnBigNumberPolynomial                           - 减数

   返回值：（无）
}

procedure BigNumberRationalPolynomialMul(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
{* 一元大整系数有理分式与一元大整系数多项式的普通乘法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 乘数一
     P1: TCnBigNumberPolynomial                           - 乘数二

   返回值：（无）
}

procedure BigNumberRationalPolynomialDiv(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
{* 一元大整系数有理分式与整系数多项式的普通除法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被除数
     P1: TCnBigNumberPolynomial                           - 除数

   返回值：（无）
}

function BigNumberRationalPolynomialCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberRationalPolynomial; P: TCnBigNumberRationalPolynomial): Boolean; overload;
{* 一元大整系数有理分式代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnBigNumberRationalPolynomial  - 用来容纳结果的一元大整系数有理分式
     F: TCnBigNumberRationalPolynomial    - 代换原式
     P: TCnBigNumberRationalPolynomial    - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberRationalPolynomialCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberRationalPolynomial; P: TCnBigNumberPolynomial): Boolean; overload;
{* 一元大整系数有理分式代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnBigNumberRationalPolynomial  - 用来容纳结果的一元大整系数有理分式
     F: TCnBigNumberRationalPolynomial    - 代换原式
     P: TCnBigNumberPolynomial            - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberRationalPolynomialCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberPolynomial; P: TCnBigNumberRationalPolynomial): Boolean; overload;
{* 整系数有理分式代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnBigNumberRationalPolynomial  - 用来容纳结果的一元大整系数有理分式
     F: TCnBigNumberPolynomial            - 代换原式
     P: TCnBigNumberRationalPolynomial    - 待代换式

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberRationalPolynomialGetValue(Res: TCnBigRational;
  F: TCnBigNumberRationalPolynomial; X: TCnBigNumber);
{* 一元大整系数有理分式求值，也就是计算 F(x)，将结果放在 Res 中。

   参数：
     F: TCnBigNumberRationalPolynomial    - 待求值的一元大整系数有理分式
     X: TCnBigNumber                      - 未知数的值
     Res: TCnBigRational                  - 用来容纳结果的一元大整系数有理分式

   返回值：（无）
}

// ================== 一元大整系数有理分式在有限域上的模运算 ===================

function BigNumberRationalPolynomialGaloisEqual(R1: TCnBigNumberRationalPolynomial;
  R2: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
{* 比较两个一元大整系数模系数有理分式是否相等。

   参数：
     R1: TCnBigNumberRationalPolynomial   - 待比较的一元大整系数有理分式一
     R2: TCnBigNumberRationalPolynomial   - 待比较的一元大整系数有理分式二
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否相等
}

procedure BigNumberRationalPolynomialGaloisNegate(P: TCnBigNumberRationalPolynomial;
  Prime: TCnBigNumber);
{* 将一个一元大整系数有理分式对象分子的所有系数在模 Prime 的条件下求反。

   参数：
     P: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisAdd(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式模系数加法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 加数一
     R2: TCnBigNumberRationalPolynomial                   - 加数二
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisSub(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式模系数减法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被减数
     R2: TCnBigNumberRationalPolynomial                   - 减数
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisMul(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式模系数乘法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 乘数一
     R2: TCnBigNumberRationalPolynomial                   - 乘数二
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisDiv(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; R2: TCnBigNumberRationalPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式模系数除法，三个参数可以是同一对象。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被除数
     R2: TCnBigNumberRationalPolynomial                   - 除数
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisAddBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
{* 一元大整系数有理分式模系数加法，加上一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 加数
     Prime: TCnBigNumber                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisSubBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
{* 一元大整系数有理分式模系数减法，减去一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 减数
     Prime: TCnBigNumber                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisMulBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
{* 一元大整系数有理分式模系数乘法，乘以一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 乘数
     Prime: TCnBigNumber                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisDivBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
{* 一元大整系数有理分式模系数除法，除以一个大数。

   参数：
     R: TCnBigNumberRationalPolynomial    - 待计算的一元大整系数有理分式
     Num: TCnBigNumber                    - 除数
     Prime: TCnBigNumber                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisAdd(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式与一元大整系数多项式的模系数加法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 加数一
     P1: TCnBigNumberPolynomial                           - 加数二
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisSub(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式与一元大整系数多项式的模系数减法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被减数
     P1: TCnBigNumberPolynomial                           - 减数
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisMul(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式与一元大整系数多项式的模系数乘法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 乘数一
     P1: TCnBigNumberPolynomial                           - 乘数二
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

procedure BigNumberRationalPolynomialGaloisDiv(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial;
  Prime: TCnBigNumber); overload;
{* 一元大整系数有理分式与一元大整系数多项式的模系数除法，RationalResult 可以是 R1。

   参数：
     Res: TCnBigNumberRationalPolynomial                  - 用来容纳结果的一元大整系数有理分式
     R1: TCnBigNumberRationalPolynomial                   - 被除数
     P1: TCnBigNumberPolynomial                           - 除数
     Prime: TCnBigNumber                                  - 模数

   返回值：（无）
}

function BigNumberRationalPolynomialGaloisCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberRationalPolynomial; P: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial = nil): Boolean; overload;
{* 一元大整系数有理分式模系数代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnBigNumberRationalPolynomial  - 用来容纳结果的一元大整系数有理分式
     F: TCnBigNumberRationalPolynomial    - 代换原式
     P: TCnBigNumberRationalPolynomial    - 待代换式
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberRationalPolynomialGaloisCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberRationalPolynomial; P: TCnBigNumberPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial = nil): Boolean; overload;
{* 一元大整系数有理分式模系数代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnBigNumberRationalPolynomial  - 用来容纳结果的一元大整系数有理分式
     F: TCnBigNumberRationalPolynomial    - 代换原式
     P: TCnBigNumberPolynomial            - 待代换式
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberRationalPolynomialGaloisCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberPolynomial; P: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial = nil): Boolean; overload;
{* 一元大整系数有理分式模系数代换，也就是计算 F(P(x))，返回是否计算成功。

   参数：
     Res: TCnBigNumberRationalPolynomial  - 用来容纳结果的一元大整系数有理分式
     F: TCnBigNumberPolynomial            - 代换原式
     P: TCnBigNumberRationalPolynomial    - 待代换式
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberPolynomial    - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberRationalPolynomialGaloisGetValue(Res: TCnBigNumber;
  F: TCnBigNumberRationalPolynomial; X: TCnBigNumber; Prime: TCnBigNumber);
{* 一元大整系数有理分式模系数求值，也就是模计算 F(x)，除法用乘法模逆元表示。

   参数：
     Res: TCnBigNumber                    - 用来容纳结果的大数对象
     F: TCnBigNumberRationalPolynomial    - 待求值的一元大整系数有理分式
     X: TCnBigNumber                      - 未知数的值
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

// =============================================================================
//
//                            二元整系数多项式
//
// =============================================================================

{
   FXs TObjectList
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | X^n   的 Y 系数 List  | -> | X^n*Y^0 的系数  |X^n*Y^1 的系数   | ......
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | X^n-1 的 Y 系数 List  | -> | X^n-1*Y^0 的系数|X^n-1*Y^1 的系数 | ......
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |......                 | -> |
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | X^0   的 Y 系数 List  | -> | X^0*Y^0 的系数  | X^0*Y^1 的系数  | ......
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}
type
  TCnInt64BiPolynomial = class
  {* 二元整系数多项式，内部实现非稀疏，因此一大就容易爆内存}
  private
    FXs: TObjectList; // 元素为 TCnInt64List，存储该 X 次幂的每一个不同的 Y 次幂的系数
    procedure EnsureDegrees(XDegree: Integer; YDegree: Integer);
    {* 确保 XDegree, YDegree 的元素存在}
    function GetMaxXDegree: Integer;
    function GetMaxYDegree: Integer;
    procedure SetMaxXDegree(const Value: Integer);
    procedure SetMaxYDegree(const Value: Integer);
    function GetYFactorsList(Index: Integer): TCnInt64List;
    function GetSafeValue(XDegree: Integer; YDegree: Integer): Int64;
    procedure SetSafeValue(XDegree: Integer; YDegree: Integer; const Value: Int64);
  protected
    function CompactYDegree(YList: TCnInt64List): Boolean;
    {* 去除一个 Y 系数高次零项，如全 0 则返回 True。

       参数：
         YList: TCnInt64List              - 待处理的系数列表

       返回值：Boolean                    - 返回是否全 0
    }

    property YFactorsList[Index: Integer]: TCnInt64List read GetYFactorsList;
    {* 封装的对 X 的 Index 次项的 Y 系数列表}
    procedure Clear;
    {* 内部清空所有数据，只给 FXs[0] 留一个 List，一般不对外使用}
  public
    constructor Create(XDegree: Integer = 0; YDegree: Integer = 0);
    {* 构造函数，传入 X 和 Y 的最高次数，可默认为 0，以后再补设。

       参数：
         XDegree: Integer                 - X 的最高次系数
         YDegree: Integer                 - Y 的最高次系数

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure SetYCoefficentsFromPolynomial(XDegree: Integer; PY: TCnInt64Polynomial);
    {* 针对特定次数的 X，从一元的 Y 多项式中一次批量设置 Y 的系数。

       参数：
         XDegree: Integer                 - X 次数
         PY: TCnInt64Polynomial           - 包含 Y 系数列表的多项式

       返回值：（无）
    }

    procedure SetYCoefficents(XDegree: Integer; LowToHighYCoefficients: array of const);
    {* 针对特定次数的 X，一次批量设置 Y 从低到高的系数。

       参数：
         XDegree: Integer                                 - X 次数
         LowToHighYCoefficients: array of const           - Y 系数列表

       返回值：（无）
    }

    procedure SetXCoefficents(YDegree: Integer; LowToHighXCoefficients: array of const);
    {* 针对特定次数的 Y，一次批量设置 X 从低到高的系数。

       参数：
         YDegree: Integer                                 - Y 次数
         LowToHighYCoefficients: array of const           - X 系数列表

       返回值：（无）
    }

    procedure SetXYCoefficent(XDegree: Integer; YDegree: Integer; ACoefficient: Int64);
    {* 针对特定次数的 X 和 Y，设置其系数。

       参数：
         XDegree: Integer                 - X 次数
         YDegree: Integer                 - Y 次数
         ACoefficient: Int64              - 系数

       返回值：（无）
    }

    procedure CorrectTop;
    {* 剔除高次的 0 系数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    procedure SetString(const Poly: string);
    {* 将多项式字符串转换为本对象的内容。

       参数：
         const Poly: string               - 待转换的字符串

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 返回是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    procedure SetZero;
    {* 设为 0}

    procedure SetOne;
    {* 设为 1}

    procedure Negate;
    {* 所有系数求反}

    function IsMonicX: Boolean;
    {* 是否是关于 X 的首一多项式。

       参数：
         （无）

       返回值：Boolean                    - 返回是否关于 X 的首一多项式
    }

    procedure Transpose;
    {* 转置，也就是互换 X Y 元}

    property MaxXDegree: Integer read GetMaxXDegree write SetMaxXDegree;
    {* X 元的最高次数，0 开始，基于 Count 所以只能是 Integer}
    property MaxYDegree: Integer read GetMaxYDegree write SetMaxYDegree;
    {* X 元的最高次数，0 开始，基于 Count 所以只能是 Integer}

    property SafeValue[XDegree, YDegree: Integer]: Int64 read GetSafeValue write SetSafeValue;
    {* 安全的读写系数方法，读不存在时返回 0。写不存在时自动扩展}
  end;

  TCnInt64BiPolynomialPool = class(TCnMathObjectPool)
  {* 二元整系数多项式池实现类，允许使用到二元整系数多项式的地方自行创建二元整系数多项式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnInt64BiPolynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnInt64BiPolynomial       - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnInt64BiPolynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnInt64BiPolynomial       - 待归还的多项式对象

       返回值：（无）
    }
  end;

function Int64BiPolynomialNew: TCnInt64BiPolynomial;
{* 创建一个二元整系数多项式对象，等同于 TCnInt64BiPolynomial.Create。

   参数：
     （无）

   返回值：TCnInt64BiPolynomial           - 返回创建的二元整系数多项式对象
}

procedure Int64BiPolynomialFree(P: TCnInt64BiPolynomial);
{* 释放一个二元整系数多项式对象，等同于 TCnInt64BiPolynomial.Free。

   参数：
     P: TCnInt64BiPolynomial              - 待释放的二元整系数多项式对象

   返回值：（无）
}

function Int64BiPolynomialDuplicate(P: TCnInt64BiPolynomial): TCnInt64BiPolynomial;
{* 从一个二元整系数多项式对象克隆一个新对象。

   参数：
     P: TCnInt64BiPolynomial              - 待复制的二元整系数多项式

   返回值：TCnInt64BiPolynomial           - 返回新建的二元整系数多项式
}

function Int64BiPolynomialCopy(Dest: TCnInt64BiPolynomial;
  Source: TCnInt64BiPolynomial): TCnInt64BiPolynomial;
{* 复制一个二元整系数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnInt64BiPolynomial           - 目标二元整系数多项式
     Source: TCnInt64BiPolynomial         - 源二元整系数多项式

   返回值：TCnInt64BiPolynomial           - 成功则返回目标对象，失败则返回 nil
}

function Int64BiPolynomialCopyFromX(Dest: TCnInt64BiPolynomial;
  SrcX: TCnInt64Polynomial): TCnInt64BiPolynomial;
{* 从一元 X 整系数多项式中复制一个二元整系数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnInt64BiPolynomial           - 目标二元整系数多项式
     SrcX: TCnInt64Polynomial             - 源一元 X 整系数多项式

   返回值：TCnInt64BiPolynomial           - 成功则返回目标对象，失败则返回 nil
}

function Int64BiPolynomialCopyFromY(Dest: TCnInt64BiPolynomial;
  SrcY: TCnInt64Polynomial): TCnInt64BiPolynomial;
{* 从一元 Y 整系数多项式中复制一个二元整系数多项式对象，成功返回 Dest

   参数：
     Dest: TCnInt64BiPolynomial           - 目标二元整系数多项式
     SrcY: TCnInt64Polynomial             - 源一元 Y 整系数多项式

   返回值：TCnInt64BiPolynomial           - 成功则返回目标对象，失败则返回 nil
}

function Int64BiPolynomialToString(P: TCnInt64BiPolynomial;
  const Var1Name: string = 'X'; const Var2Name: string = 'Y'): string;
{* 将一个二元整系数多项式对象转成字符串，未知数默认以 X 和 Y 表示。

   参数：
     P: TCnInt64BiPolynomial              - 待转换的二元整系数多项式
     const Var1Name: string               - 代表第一个未知数的字符串
     const Var2Name: string               - 代表第二个未知数的字符串

   返回值：string                         - 返回字符串
}

function Int64BiPolynomialSetString(P: TCnInt64BiPolynomial;
  const Str: string; const Var1Name: string = 'X'; const Var2Name: string = 'Y'): Boolean;
{* 将字符串形式的二元整系数多项式赋值给二元整系数多项式对象，返回是否赋值成功。

   参数：
     P: TCnInt64BiPolynomial              - 待赋值的二元整系数多项式
     const Str: string                    - 多项式字符串
     const Var1Name: string               - 代表第一个未知数的字符串
     const Var2Name: string               - 代表第二个未知数的字符串

   返回值：Boolean                        - 返回是否赋值成功
}

function Int64BiPolynomialIsZero(P: TCnInt64BiPolynomial): Boolean;
{* 判断一个二元整系数多项式对象是否为 0。

   参数：
     P: TCnInt64BiPolynomial              - 待判断的二元整系数多项式

   返回值：Boolean                        - 返回是否为 0
}

procedure Int64BiPolynomialSetZero(P: TCnInt64BiPolynomial);
{* 将一个二元整系数多项式对象设为 0。

   参数：
     P: TCnInt64BiPolynomial              - 待设置的二元整系数多项式

   返回值：（无）
}

procedure Int64BiPolynomialSetOne(P: TCnInt64BiPolynomial);
{* 将一个二元整系数多项式对象设为 1。

   参数：
     P: TCnInt64BiPolynomial              - 待设置的二元整系数多项式

   返回值：（无）
}

procedure Int64BiPolynomialNegate(P: TCnInt64BiPolynomial);
{* 将一个二元整系数多项式对象所有系数求反。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式

   返回值：（无）
}

function Int64BiPolynomialIsMonicX(P: TCnInt64BiPolynomial): Boolean;
{* 判断一个二元整系数多项式是否是关于 X 的首一多项式，也就是判断 X 最高次的系数是否为 1。

   参数：
     P: TCnInt64BiPolynomial              - 待判断的二元整系数多项式

   返回值：Boolean                        - 返回是否 X 的首一多项式
}

procedure Int64BiPolynomialShiftLeftX(P: TCnInt64BiPolynomial; N: Integer);
{* 将一个二元整系数多项式对象的 X 左移 N 次，也就是 X 各项指数都加 N。

   参数：
     P: TCnInt64BiPolynomial              - 待左移的二元整系数多项式
     N: Integer                           - 左移次数

   返回值：（无）
}

procedure Int64BiPolynomialShiftRightX(P: TCnInt64BiPolynomial; N: Integer);
{* 将一个二元整系数多项式对象的 X 右移 N 次，也就是 X 各项指数都减 N，小于 0 的忽略了。

   参数：
     P: TCnInt64BiPolynomial              - 待右移的二元整系数多项式
     N: Integer                           - 右移次数

   返回值：（无）
}

function Int64BiPolynomialEqual(A: TCnInt64BiPolynomial; B: TCnInt64BiPolynomial): Boolean;
{* 判断俩二元整系数多项式每项系数是否对应相等，是则返回 True。

   参数：
     A: TCnInt64BiPolynomial              - 待判断的二元整系数多项式一
     B: TCnInt64BiPolynomial              - 待判断的二元整系数多项式二

   返回值：Boolean                        - 返回是否相等
}

// ====================== 二元整系数多项式普通运算 =============================

procedure Int64BiPolynomialAddWord(P: TCnInt64BiPolynomial; N: Int64);
{* 将一个二元整系数多项式对象的各个系数加上 N。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 加数

   返回值：（无）
}

procedure Int64BiPolynomialSubWord(P: TCnInt64BiPolynomial; N: Int64);
{* 将一个二元整系数多项式对象的各个系数减去 N。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 减数

   返回值：（无）
}

procedure Int64BiPolynomialMulWord(P: TCnInt64BiPolynomial; N: Int64);
{* 将一个二元整系数多项式对象的各个系数都乘以 N。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 乘数

   返回值：（无）
}

procedure Int64BiPolynomialDivWord(P: TCnInt64BiPolynomial; N: Int64);
{* 将一个二元整系数多项式对象的各个系数都除以 N，如不能整除则取整。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 除数

   返回值：（无）
}

procedure Int64BiPolynomialNonNegativeModWord(P: TCnInt64BiPolynomial; N: Int64);
{* 将一个二元整系数多项式对象的各个系数都对 N 非负求余，可以用于有限域化。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 除数

   返回值：（无）
}

function Int64BiPolynomialAdd(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial): Boolean;
{* 两个二元整系数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 加数一
     P2: TCnInt64BiPolynomial             - 加数二

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialSub(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial): Boolean;
{* 两个二元整系数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 被减数
     P2: TCnInt64BiPolynomial             - 减数

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialMul(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial): Boolean;
{* 两个二元整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 乘数一
     P2: TCnInt64BiPolynomial             - 乘数二

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialMulX(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PX: TCnInt64Polynomial): Boolean;
{* 一个二元整系数多项式对象与一个 X 的一元整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 乘数一
     PX: TCnInt64Polynomial               - 乘数二

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialMulY(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PY: TCnInt64Polynomial): Boolean;
{* 一个二元整系数多项式对象与一个 Y 的一元整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 乘数一
     PY: TCnInt64Polynomial               - 乘数二

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialDivX(Res: TCnInt64BiPolynomial; Remain: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Divisor: TCnInt64BiPolynomial): Boolean;
{* 两个二元整系数多项式对象以 X 为主相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   注意 Divisor 必须是 X 的首一多项式，否则会返回 False，表示无法支持，调用者务必判断返回值。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     Remain: TCnInt64BiPolynomial         - 用来容纳余式的二元整系数多项式
     P: TCnInt64BiPolynomial              - 被除数
     Divisor: TCnInt64BiPolynomial        - 除数

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialModX(Res: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Divisor: TCnInt64BiPolynomial): Boolean;
{* 两个二元整系数多项式对象以 X 为主求余，余数放至 Res 中，返回求余是否成功，
   注意 Divisor 必须是 X 的首一多项式，否则会返回 False，表示无法支持，调用者务必判断返回值。
   Res 可以是 P 或 Divisor，P 可以是 Divisor

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P: TCnInt64BiPolynomial              - 被除数
     Divisor: TCnInt64BiPolynomial        - 除数

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialPower(Res: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Exponent: Int64): Boolean;
{* 计算二元整系数多项式的 Exponent 次幂，不考虑系数溢出的问题，返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P: TCnInt64BiPolynomial              - 底数
     Exponent: Int64                      - 指数

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialEvaluateByY(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; YValue: Int64): Boolean;
{* 将一具体 Y 值代入二元整系数多项式，得到只包含 X 的一元整系数多项式。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64BiPolynomial              - 待代入的二元整系数多项式
     YValue: Int64                        - 未知数 Y 的值

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialEvaluateByX(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; XValue: Int64): Boolean;
{* 将一具体 X 值代入二元整系数多项式，得到只包含 Y 的一元整系数多项式。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64BiPolynomial              - 待代入的二元整系数多项式
     XValue: Int64                        - 未知数 X 的值

   返回值：Boolean                        - 返回是否计算成功
}

procedure Int64BiPolynomialTranspose(Dest: TCnInt64BiPolynomial; Source: TCnInt64BiPolynomial);
{* 将二元整系数多项式的 X Y 元互换至另一个二元整系数多项式对象中，Src 和 Dest 可以相同。

   参数：
     Dest: TCnInt64BiPolynomial           - 目标二元整系数多项式
     Source: TCnInt64BiPolynomial         - 源二元整系数多项式

   返回值：（无）
}

procedure Int64BiPolynomialExtractYByX(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; XDegree: Int64);
{* 将二元整系数多项式的 X 次方系数提取出来放到一个 Y 的一元多项式里。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64BiPolynomial              - 待提取的二元整系数多项式
     XDegree: Int64                       - 指定 X 的次数

   返回值：（无）
}

procedure Int64BiPolynomialExtractXByY(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; YDegree: Int64);
{* 将二元整系数多项式的 Y 次方系数提取出来放到一个 X 的一元多项式里。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64BiPolynomial              - 待提取的二元整系数多项式
     YDegree: Int64                       - 指定 Y 的次数

   返回值：（无）
}

// =================== 二元整系数多项式式在有限域上的模运算 ====================

function Int64BiPolynomialGaloisEqual(A: TCnInt64BiPolynomial;
  B: TCnInt64BiPolynomial; Prime: Int64): Boolean;
{* 两个二元整系数多项式在模 Prime 的条件下是否相等。

   参数：
     A: TCnInt64BiPolynomial              - 待判断的二元整系数多项式一
     B: TCnInt64BiPolynomial              - 待判断的二元整系数多项式一
     Prime: Int64                         - 模数

   返回值：Boolean                        - 返回是否相等
}

procedure Int64BiPolynomialGaloisNegate(P: TCnInt64BiPolynomial; Prime: Int64);
{* 将一个二元整系数多项式对象所有系数在模 Prime 的条件下求反。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     Prime: Int64                         - 模数

   返回值：（无）
}

function Int64BiPolynomialGaloisAdd(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial = nil): Boolean;
{* 两个二元整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 加数一
     P2: TCnInt64BiPolynomial             - 加数二
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisSub(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial = nil): Boolean;
{* 两个二元整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 被减数
     P2: TCnInt64BiPolynomial             - 减数
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisMul(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial = nil): Boolean;
{* 两个二元整系数多项式对象在 Prime 次方阶有限域上相乘，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 乘数一
     P2: TCnInt64BiPolynomial             - 乘数二
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisMulX(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PX: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial = nil): Boolean;
{* 一个二元整系数多项式对象与一个 X 的一元整系数多项式对象在 Prime 次方阶有限域上相乘，
  结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 乘数一
     PX: TCnInt64Polynomial               - 乘数二
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisMulY(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PY: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial = nil): Boolean;
{* 一个二元整系数多项式对象与一个 Y 的一元整系数多项式对象在 Prime 次方阶有限域上相乘，
  结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P1: TCnInt64BiPolynomial             - 乘数一
     PY: TCnInt64Polynomial               - 乘数二
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisDivX(Res: TCnInt64BiPolynomial; Remain: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Divisor: TCnInt64BiPolynomial; Prime: Int64;
  Primitive: TCnInt64BiPolynomial = nil): Boolean;
{* 两个二元整系数多项式对象在 Prime 次方阶有限域上相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   调用者需自行保证 Divisor 是 X 的首一多项式且 Prime 是素数且本原多项式 Primitive 为 X 的不可约多项式。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。
   注意：和一元多项式不同，只是系数求模了。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     Remain: TCnInt64BiPolynomial         - 用来容纳余式的二元整系数多项式
     P: TCnInt64BiPolynomial              - 被除数
     Divisor: TCnInt64BiPolynomial        - 除数
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisModX(Res: TCnInt64BiPolynomial; P: TCnInt64BiPolynomial;
  Divisor: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial = nil): Boolean;
{* 两个二元整系数多项式对象在 Prime 次方阶有限域上求余，余数放至 Res 中，返回求余是否成功，
   调用者需自行保证 Divisor 是 X 的首一多项式且 Prime 是素数且本原多项式 Primitive 为 X 的不可约多项式。
   Res 可以是 P 或 Divisor，P 可以是 Divisor。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P: TCnInt64BiPolynomial              - 被除数
     Divisor: TCnInt64BiPolynomial        - 除数
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisPower(Res: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Exponent: Int64; Prime: Int64;
  Primitive: TCnInt64BiPolynomial = nil; ExponentHi: Int64 = 0): Boolean;
{* 计算二元整系数多项式在 Prime 次方阶有限域上的 Exponent 次幂，Exponent 可以是 128 位，
   Exponent 两个部分如果是负值，自动转成 UInt64。
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnInt64BiPolynomial            - 用来容纳结果的二元整系数多项式
     P: TCnInt64BiPolynomial              - 底数
     Exponent: Int64                      - 指数低 64 位
     Prime: Int64                         - 有限域上界
     Primitive: TCnInt64BiPolynomial      - 本原多项式
     ExponentHi: Int64                    - 指数高 64 位

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisEvaluateByY(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; YValue: Int64; Prime: Int64): Boolean;
{* 将一具体 Y 值代入二元整系数多项式，得到只包含 X 的一元整系数多项式，系数针对 Prime 取模。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64BiPolynomial              - 待代入的二元整系数多项式
     YValue: Int64                        - 未知数 Y 的值
     Prime: Int64                         - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function Int64BiPolynomialGaloisEvaluateByX(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; XValue: Int64; Prime: Int64): Boolean;
{* 将一具体 X 值代入二元整系数多项式，得到只包含 Y 的一元整系数多项式，系数针对 Prime 取模。

   参数：
     Res: TCnInt64Polynomial              - 用来容纳结果的一元整系数多项式
     P: TCnInt64BiPolynomial              - 待代入的二元整系数多项式
     XValue: Int64                        - 未知数 X 的值
     Prime: Int64                         - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

procedure Int64BiPolynomialGaloisAddWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的二元整系数多项式的各项系数加上 N 再 mod Prime，注意不是常系数。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 加数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

procedure Int64BiPolynomialGaloisSubWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的二元整系数多项式的各项系数减去 N 再 mod Prime，注意不是常系数。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 减数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

procedure Int64BiPolynomialGaloisMulWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的二元整系数多项式各项系数乘以 N 再 mod Prime。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 乘数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

procedure Int64BiPolynomialGaloisDivWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
{* 将 Prime 次方阶有限域上的二元整系数多项式各项系数除以 N，也就是乘以 N 的逆元再 mod Prime。

   参数：
     P: TCnInt64BiPolynomial              - 待计算的二元整系数多项式
     N: Int64                             - 除数
     Prime: Int64                         - 有限域上界

   返回值：（无）
}

// =============================================================================
//
//                           二元大整系数多项式
//
// =============================================================================

{
   FXs TObjectList
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | X^n   的 Y 系数 Sparse| -> | X^n*Y^0 的系数  |X^n*Y^3 的系数   | ......
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | X^n-1 的 Y 系数 Sparse| -> | X^n-1*Y^2 的系数|X^n-1*Y^5 的系数 | ......
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |......                 | -> |
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | X^0   的 Y 系数 Sparse| -> | X^0*Y^4 的系数  | X^0*Y^7 的系数  | ......
  +-+-+-+-+-+-+-+-+-+-+-+-+    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

type
  TCnBigNumberBiPolynomial = class
  {* 二元大整系数多项式，内部采取稀疏方式，稍微少占点内存}
  private
    FXs: TCnRefObjectList; // 元素为 TCnSparseBigNumberList，存储该 X 次幂的每一个不同的 Y 次幂的系数
    procedure EnsureDegrees(XDegree: Integer; YDegree: Integer);
    {* 确保 XDegree, YDegree 的元素存在}
    function GetMaxXDegree: Integer;
    function GetMaxYDegree: Integer;
    procedure SetMaxXDegree(const Value: Integer);
    procedure SetMaxYDegree(const Value: Integer);
    function GetYFactorsList(Index: Integer): TCnSparseBigNumberList;
    function GetSafeValue(XDegree: Integer; YDegree: Integer): TCnBigNumber;
    procedure SetSafeValue(XDegree: Integer; YDegree: Integer; const Value: TCnBigNumber);
    function GetReadonlyValue(XDegree: Integer; YDegree: Integer): TCnBigNumber;
  protected
    function CompactYDegree(YList: TCnSparseBigNumberList): Boolean;
    {* 去除一个 Y 系数高次零项，如是 nil 或有内容但全 0 则返回 True。

       参数：
         YList: TCnSparseBigNumberList    - 待处理的系数列表

       返回值：Boolean                    - 返回是否全 0
    }

    property YFactorsList[Index: Integer]: TCnSparseBigNumberList read GetYFactorsList;
    {* 封装的对 X 的 Index 次项的 Y 系数列表，FXs[Index] 为 nil 时会自动创建出来，FXs.Count 不够时会自动扩容}
    procedure Clear;
    {* 内部清空所有数据，只给 FXs[0] 留一个 List，一般不对外使用}
  public
    constructor Create(XDegree: Integer = 0; YDegree: Integer = 0);
    {* 构造函数，传入 X 和 Y 的最高次数，可默认为 0，以后再补设。

       参数：
         XDegree: Integer                 - X 的最高次系数
         YDegree: Integer                 - Y 的最高次系数

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure SetYCoefficentsFromPolynomial(XDegree: Integer; PY: TCnInt64Polynomial); overload;
    {* 针对特定次数的 X，从一元的 Y 多项式中一次批量设置 Y 的系数。

       参数：
         XDegree: Integer                 - X 次数
         PY: TCnInt64Polynomial           - 包含 Y 系数列表的多项式

       返回值：（无）
    }

    procedure SetYCoefficentsFromPolynomial(XDegree: Integer; PY: TCnBigNumberPolynomial); overload;
    {* 针对特定次数的 X，从一元的大整系数 Y 多项式中一次批量设置 Y 的系数。

       参数：
         XDegree: Integer                 - X 次数
         PY: TCnBigNumberPolynomial       - 包含 Y 系数列表的多项式

       返回值：（无）
    }

    procedure SetYCoefficents(XDegree: Integer; LowToHighYCoefficients: array of const);
    {* 针对特定次数的 X，一次批量设置 Y 从低到高的系数。

       参数：
         XDegree: Integer                                 - X 次数
         LowToHighYCoefficients: array of const           - Y 系数列表

       返回值：（无）
    }

    procedure SetXCoefficents(YDegree: Integer; LowToHighXCoefficients: array of const);
    {* 针对特定次数的 Y，一次批量设置 X 从低到高的系数。

       参数：
         YDegree: Integer                                 - Y 次数
         LowToHighXCoefficients: array of const           - X 系数列表

       返回值：（无）
    }

    procedure SetXYCoefficent(XDegree: Integer; YDegree: Integer; ACoefficient: Int64); overload;
    {* 针对特定次数的 X 和 Y，设置其系数。

       参数：
         XDegree: Integer                 - X 次数
         YDegree: Integer                 - Y 次数
         ACoefficient: Int64              - 整数系数

       返回值：（无）
    }

    procedure SetXYCoefficent(XDegree: Integer; YDegree: Integer; ACoefficient: TCnBigNumber); overload;
    {* 针对特定次数的 X 和 Y，设置其系数。

       参数：
         XDegree: Integer                 - X 次数
         YDegree: Integer                 - Y 次数
         ACoefficient: TCnBigNumber       - 大整数系数

       返回值：（无）
    }

    procedure CorrectTop;
    {* 剔除高次的 0 系数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    procedure SetString(const Poly: string);
    {* 将多项式字符串转换为本对象的内容。

       参数：
         const Poly: string               - 待转换的字符串

       返回值：（无）
    }

    function IsZero: Boolean;
    {* 返回是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    procedure SetZero;
    {* 设为 0}

    procedure SetOne;
    {* 设为 1}

    procedure Negate;
    {* 所有系数求反}

    function IsMonicX: Boolean;
    {* 是否是关于 X 的首一多项式。

       参数：
         （无）

       返回值：Boolean                    - 返回是否是关于 X 的首一多项式
    }

    procedure Transpose;
    {* 转置，也就是互换 X Y 元}

    property MaxXDegree: Integer read GetMaxXDegree write SetMaxXDegree;
    {* X 元的最高次数，0 开始，基于 Count 所以只能是 Integer，
      设置后能保证新增的每个 XDegree，其对应的 SparseBigNumberList 都存在}
    property MaxYDegree: Integer read GetMaxYDegree write SetMaxYDegree;
    {* X 元的最高次数，0 开始，基于 Count 所以只能是 Integer}

    property SafeValue[XDegree, YDegree: Integer]: TCnBigNumber read GetSafeValue write SetSafeValue;
    {* 安全的读写系数方法，读不存在时返回 0。写不存在时自动扩展并内部复制大数值}
    property ReadonlyValue[XDegree, YDegree: Integer]: TCnBigNumber read GetReadonlyValue;
    {* 只读的根据参数的俩 Exponent 获取大数的方法，读时如内部查不到，会返回一固定的零值 TCnBigNumber 对象，切勿修改其值}
  end;

  TCnBigNumberBiPolynomialPool = class(TCnMathObjectPool)
  {* 二元大整系数多项式池实现类，允许使用到二元大整系数多项式的地方自行创建二元大整系数多项式池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigNumberBiPolynomial; reintroduce;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TCnBigNumberBiPolynomial   - 返回的多项式对象
    }

    procedure Recycle(Poly: TCnBigNumberBiPolynomial); reintroduce;
    {* 将一个对象归还至对象池。

       参数：
         Poly: TCnBigNumberBiPolynomial   - 待归还的多项式对象

       返回值：（无）
    }
  end;

function BigNumberBiPolynomialNew: TCnBigNumberBiPolynomial;
{* 创建一个二元大整系数多项式对象，等同于 TCnBigNumberBiPolynomial.Create。

   参数：
     （无）

   返回值：TCnBigNumberBiPolynomial       - 返回创建的二元大整系数多项式对象
}

procedure BigNumberBiPolynomialFree(P: TCnBigNumberBiPolynomial);
{* 释放一个二元大整系数多项式对象，等同于 TCnBigNumberBiPolynomial.Free。

   参数：
     P: TCnBigNumberBiPolynomial          - 待释放的二元大整系数多项式对象

   返回值：（无）
}

function BigNumberBiPolynomialDuplicate(P: TCnBigNumberBiPolynomial): TCnBigNumberBiPolynomial;
{* 从一个二元大整系数多项式对象克隆一个新对象。

   参数：
     P: TCnBigNumberBiPolynomial          - 待复制的二元大整系数多项式

   返回值：TCnBigNumberBiPolynomial       - 返回新建的二元大整系数多项式
}

function BigNumberBiPolynomialCopy(Dest: TCnBigNumberBiPolynomial;
  Source: TCnBigNumberBiPolynomial): TCnBigNumberBiPolynomial;
{* 复制一个二元大整系数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnBigNumberBiPolynomial       - 目标二元大整系数多项式
     Source: TCnBigNumberBiPolynomial     - 源二元大整系数多项式

   返回值：TCnBigNumberBiPolynomial       - 成功则返回目标对象，失败则返回 nil
}

function BigNumberBiPolynomialCopyFromX(Dest: TCnBigNumberBiPolynomial;
  SrcX: TCnBigNumberPolynomial): TCnBigNumberBiPolynomial;
{* 从一元 X 大整系数多项式中复制一个二元大整系数多项式对象，成功返回 Dest。

   参数：
     Dest: TCnBigNumberBiPolynomial       - 目标二元大整系数多项式
     SrcX: TCnBigNumberPolynomial         - 源一元 X 大整系数多项式

   返回值：TCnBigNumberBiPolynomial       - 成功则返回目标对象，失败则返回 nil
}

function BigNumberBiPolynomialCopyFromY(Dest: TCnBigNumberBiPolynomial;
  SrcY: TCnBigNumberPolynomial): TCnBigNumberBiPolynomial;
{* 从一元 Y 大整系数多项式中复制一个二元大整系数多项式对象，成功返回 Dest

   参数：
     Dest: TCnBigNumberBiPolynomial       - 目标二元大整系数多项式
     SrcY: TCnBigNumberPolynomial         - 源一元 Y 大整系数多项式

   返回值：TCnBigNumberBiPolynomial       - 成功则返回目标对象，失败则返回 nil
}

function BigNumberBiPolynomialToString(P: TCnBigNumberBiPolynomial;
  const Var1Name: string = 'X'; const Var2Name: string = 'Y'): string;
{* 将一个二元大整系数多项式对象转成字符串，未知数默认以 X 和 Y 表示。

   参数：
     P: TCnBigNumberBiPolynomial          - 待转换的二元大整系数多项式
     const Var1Name: string               - 代表第一个未知数的字符串
     const Var2Name: string               - 代表第二个未知数的字符串

   返回值：string                         - 返回字符串
}

function BigNumberBiPolynomialSetString(P: TCnBigNumberBiPolynomial;
  const Str: string; const Var1Name: string = 'X'; const Var2Name: string = 'Y'): Boolean;
{* 将字符串形式的二元大整系数多项式赋值给二元大整系数多项式对象，返回是否赋值成功。

   参数：
     P: TCnBigNumberBiPolynomial          - 待赋值的二元大整系数多项式
     const Str: string                    - 多项式字符串
     const Var1Name: string               - 代表第一个未知数的字符串
     const Var2Name: string               - 代表第二个未知数的字符串

   返回值：Boolean                        - 返回是否赋值成功
}

function BigNumberBiPolynomialIsZero(P: TCnBigNumberBiPolynomial): Boolean;
{* 判断一个二元大整系数多项式对象是否为 0。

   参数：
     P: TCnBigNumberBiPolynomial          - 待判断的二元大整系数多项式

   返回值：Boolean                        - 返回是否为 0
}

procedure BigNumberBiPolynomialSetZero(P: TCnBigNumberBiPolynomial);
{* 将一个二元大整系数多项式对象设为 0。

   参数：
     P: TCnBigNumberBiPolynomial          - 待设置的二元大整系数多项式

   返回值：（无）
}

procedure BigNumberBiPolynomialSetOne(P: TCnBigNumberBiPolynomial);
{* 将一个二元大整系数多项式对象设为 1。

   参数：
     P: TCnBigNumberBiPolynomial          - 待设置的二元大整系数多项式

   返回值：（无）
}

procedure BigNumberBiPolynomialNegate(P: TCnBigNumberBiPolynomial);
{* 将一个二元大整系数多项式对象所有系数求反。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式

   返回值：（无）
}

function BigNumberBiPolynomialIsMonicX(P: TCnBigNumberBiPolynomial): Boolean;
{* 判断一个二元大整系数多项式是否是关于 X 的首一多项式，也就是判断 X 最高次的系数是否为 1。

   参数：
     P: TCnBigNumberBiPolynomial          - 待判断的二元大整系数多项式

   返回值：Boolean                        - 返回是否 X 的首一多项式
}

procedure BigNumberBiPolynomialShiftLeftX(P: TCnBigNumberBiPolynomial; N: Integer);
{* 将一个二元大整系数多项式对象的 X 左移 N 次，也就是 X 各项指数都加 N。

   参数：
     P: TCnBigNumberBiPolynomial          - 待左移的二元大整系数多项式
     N: Integer                           - 左移次数

   返回值：（无）
}

procedure BigNumberBiPolynomialShiftRightX(P: TCnBigNumberBiPolynomial; N: Integer);
{* 将一个二元大整系数多项式对象的 X 右移 N 次，也就是 X 各项指数都减 N，小于 0 的忽略了。

   参数：
     P: TCnBigNumberBiPolynomial          - 待右移的二元大整系数多项式
     N: Integer                           - 右移次数

   返回值：（无）
}

function BigNumberBiPolynomialEqual(A: TCnBigNumberBiPolynomial; B: TCnBigNumberBiPolynomial): Boolean;
{* 判断两个二元大整系数多项式每项系数是否对应相等，是则返回 True。

   参数：
     A: TCnBigNumberBiPolynomial          - 待判断的二元大整系数多项式一
     B: TCnBigNumberBiPolynomial          - 待判断的二元大整系数多项式二

   返回值：Boolean                        - 返回是否相等
}

// ===================== 二元大整系数多项式普通运算 ============================

// procedure BigNumberBiPolynomialAddWord(P: TCnBigNumberBiPolynomial; N: Int64);
{* 将一个二元大整系数多项式对象的各个系数加上 N，但对于稀疏列表来说没啥意义，不实现}

// procedure BigNumberBiPolynomialSubWord(P: TCnBigNumberBiPolynomial; N: Int64);
{* 将一个二元大整系数多项式对象的各个系数减去 N，但对于稀疏列表来说没啥意义，不实现}

procedure BigNumberBiPolynomialMulWord(P: TCnBigNumberBiPolynomial; N: Int64);
{* 将一个二元大整系数多项式对象的各个系数都乘以 N。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: Int64                             - 乘数

   返回值：（无）
}

procedure BigNumberBiPolynomialDivWord(P: TCnBigNumberBiPolynomial; N: Int64);
{* 将一个二元大整系数多项式对象的各个系数都除以 N，如不能整除则取整。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: Int64                             - 除数

   返回值：（无）
}

procedure BigNumberBiPolynomialNonNegativeModWord(P: TCnBigNumberBiPolynomial; N: Int64);
{* 将一个二元大整系数多项式对象的各个系数都对 N 非负求余，可以用于有限域化。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: Int64                             - 除数

   返回值：（无）
}

procedure BigNumberBiPolynomialMulBigNumber(P: TCnBigNumberBiPolynomial; N: TCnBigNumber);
{* 将一个二元大整系数多项式对象的各个系数都乘以大数 N。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: TCnBigNumber                      - 乘数

   返回值：（无）
}

procedure BigNumberBiPolynomialDivBigNumber(P: TCnBigNumberBiPolynomial; N: TCnBigNumber);
{* 将一个二元大整系数多项式对象的各个系数都除以大数 N，如不能整除则取整。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: TCnBigNumber                      - 除数

   返回值：（无）
}

procedure BigNumberBiPolynomialNonNegativeModBigNumber(P: TCnBigNumberBiPolynomial; N: TCnBigNumber);
{* 将一个二元大整系数多项式对象的各个系数都对 N 非负求余，可以用于有限域化。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: TCnBigNumber                      - 除数

   返回值：（无）
}

function BigNumberBiPolynomialAdd(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial): Boolean;
{* 两个二元大整系数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 加数一
     P2: TCnBigNumberBiPolynomial         - 加数二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialSub(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial): Boolean;
{* 两个二元大整系数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 被减数
     P2: TCnBigNumberBiPolynomial         - 减数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialMul(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial): Boolean;
{* 两个二元大整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 乘数一
     P2: TCnBigNumberBiPolynomial         - 乘数二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialMulX(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PX: TCnBigNumberPolynomial): Boolean;
{* 一个二元大整系数多项式对象与一个 X 的一元大整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 乘数一
     PX: TCnBigNumberPolynomial           - 乘数二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialMulY(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PY: TCnBigNumberPolynomial): Boolean;
{* 一个二元大整系数多项式对象与一个 Y 的一元大整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 乘数一
     PY: TCnBigNumberPolynomial           - 乘数二

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialDivX(Res: TCnBigNumberBiPolynomial; Remain: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Divisor: TCnBigNumberBiPolynomial): Boolean;
{* 两个二元大整系数多项式对象以 X 为主相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   注意 Divisor 必须是 X 的首一多项式，否则会返回 False，表示无法支持，调用者务必判断返回值。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     Remain: TCnBigNumberBiPolynomial     - 用来容纳余式的二元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 被除数
     Divisor: TCnBigNumberBiPolynomial    - 除数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialModX(Res: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Divisor: TCnBigNumberBiPolynomial): Boolean;
{* 两个二元大整系数多项式对象以 X 为主求余，余数放至 Res 中，返回求余是否成功，
   注意 Divisor 必须是 X 的首一多项式，否则会返回 False，表示无法支持，调用者务必判断返回值。
   Res 可以是 P 或 Divisor，P 可以是 Divisor。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 被除数
     Divisor: TCnBigNumberBiPolynomial    - 除数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialPower(Res: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Exponent: TCnBigNumber): Boolean;
{* 计算二元大整系数多项式的 Exponent 次幂，不考虑系数溢出的问题，返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 底数
     Exponent: TCnBigNumber               - 指数

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialEvaluateByY(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; YValue: TCnBigNumber): Boolean;
{* 将一具体 Y 值代入二元大整系数多项式，得到只包含 X 的一元大整系数多项式。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 待代入的二元整系数多项式
     YValue: TCnBigNumber                 - 未知数 Y 的值

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialEvaluateByX(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; XValue: TCnBigNumber): Boolean;
{* 将一具体 X 值代入二元大整系数多项式，得到只包含 Y 的一元大整系数多项式。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 待代入的二元整系数多项式
     XValue: TCnBigNumber                 - 未知数 X 的值

   返回值：Boolean                        - 返回是否计算成功
}

procedure BigNumberBiPolynomialTranspose(Dest: TCnBigNumberBiPolynomial;
  Source: TCnBigNumberBiPolynomial);
{* 将二元大整系数多项式的 X Y 元互换至另一个二元大整系数多项式对象中，Src 和 Dest 可以相同。

   参数：
     Dest: TCnBigNumberBiPolynomial       - 目标二元大整系数多项式
     Source: TCnBigNumberBiPolynomial     - 源二元大整系数多项式

   返回值：（无）
}

procedure BigNumberBiPolynomialExtractYByX(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; XDegree: Integer);
{* 将二元大整系数多项式的 X 次方系数提取出来放到一个 Y 的一元多项式里。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 待提取的二元大整系数多项式
     XDegree: Integer                     - 指定 X 的次数

   返回值：（无）
}

procedure BigNumberBiPolynomialExtractXByY(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; YDegree: Integer);
{* 将二元大整系数多项式的 Y 次方系数提取出来放到一个 X 的一元多项式里。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 待提取的二元大整系数多项式
     YDegree: Integer                     - 指定 Y 的次数

   返回值：（无）
}

// ================== 二元大整系数多项式在有限域上的模运算 =====================

function BigNumberBiPolynomialGaloisEqual(A: TCnBigNumberBiPolynomial;
  B: TCnBigNumberBiPolynomial; Prime: TCnBigNumber): Boolean;
{* 两个二元大整系数多项式在模 Prime 的条件下是否相等。

   参数：
     A: TCnBigNumberBiPolynomial          - 待判断的二元大整系数多项式一
     B: TCnBigNumberBiPolynomial          - 待判断的二元大整系数多项式二
     Prime: TCnBigNumber                  - 模数

   返回值：Boolean                        - 返回是否相等
}

procedure BigNumberBiPolynomialGaloisNegate(P: TCnBigNumberBiPolynomial; Prime: TCnBigNumber);
{* 将一个二元大整系数多项式对象所有系数在模 Prime 的条件下求反。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     Prime: TCnBigNumber                  - 模数

   返回值：（无）
}

function BigNumberBiPolynomialGaloisAdd(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 两个二元大整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 加数一
     P2: TCnBigNumberBiPolynomial         - 加数二
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisSub(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 两个二元大整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式。
   返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 被减数
     P2: TCnBigNumberBiPolynomial         - 减数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisMul(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 两个二元大整系数多项式对象在 Prime 次方阶有限域上相乘，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 乘数一
     P2: TCnBigNumberBiPolynomial         - 乘数二
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisMulX(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PX: TCnBigNumberPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 一个二元大整系数多项式对象与一个 X 的一元大整系数多项式对象在 Prime 次方阶有限域上相乘，
  结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 乘数一
     PX: TCnBigNumberPolynomial           - 乘数二
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisMulY(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PY: TCnBigNumberPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 一个二元大整系数多项式对象与一个 Y 的一元大整系数多项式对象在 Prime 次方阶有限域上相乘，
  结果放至 Res 中，返回相乘是否成功，Res 可以是 P1。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P1: TCnBigNumberBiPolynomial         - 乘数一
     PY: TCnBigNumberPolynomial           - 乘数二
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisDivX(Res: TCnBigNumberBiPolynomial;
  Remain: TCnBigNumberBiPolynomial; P: TCnBigNumberBiPolynomial;
  Divisor: TCnBigNumberBiPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 两个二元大整系数多项式对象在 Prime 次方阶有限域上相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   调用者需自行保证 Divisor 是 X 的首一多项式且 Prime 是素数且本原多项式 Primitive 为 X 的不可约多项式。
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor。
   注意：和一元多项式不同，只是系数求模了。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     Remain: TCnBigNumberBiPolynomial     - 用来容纳余式的二元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 被除数
     Divisor: TCnBigNumberBiPolynomial    - 除数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisModX(Res: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Divisor: TCnBigNumberBiPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 两个二元大整系数多项式对象在 Prime 次方阶有限域上求余，余数放至 Res 中，返回求余是否成功，
   调用者需自行保证 Divisor 是 X 的首一多项式且 Prime 是素数且本原多项式 Primitive 为 X 的不可约多项式。
   Res 可以是 P 或 Divisor，P 可以是 Divisor。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 被除数
     Divisor: TCnBigNumberBiPolynomial    - 除数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisPower(Res: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Exponent: TCnBigNumber; Prime: TCnBigNumber;
  Primitive: TCnBigNumberBiPolynomial = nil): Boolean;
{* 计算二元大整系数多项式在 Prime 次方阶有限域上的 Exponent 次幂。
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式。
   返回是否计算成功，Res 可以是 P。

   参数：
     Res: TCnBigNumberBiPolynomial        - 用来容纳结果的二元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 底数
     Exponent: TCnBigNumber               - 指数
     Prime: TCnBigNumber                  - 有限域上界
     Primitive: TCnBigNumberBiPolynomial  - 本原多项式

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisEvaluateByY(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; YValue: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 将一具体 Y 值代入二元大整系数多项式，得到只包含 X 的一元大整系数多项式，系数针对 Prime 取模。

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 待代入的二元大整系数多项式
     YValue: TCnBigNumber                 - 未知数 Y 的值
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

function BigNumberBiPolynomialGaloisEvaluateByX(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; XValue: TCnBigNumber; Prime: TCnBigNumber): Boolean;
{* 将一具体 X 值代入二元大整系数多项式，得到只包含 Y 的一元大整系数多项式，系数针对 Prime 取模

   参数：
     Res: TCnBigNumberPolynomial          - 用来容纳结果的一元大整系数多项式
     P: TCnBigNumberBiPolynomial          - 待代入的二元大整系数多项式
     XValue: TCnBigNumber                 - 未知数 X 的值
     Prime: TCnBigNumber                  - 有限域上界

   返回值：Boolean                        - 返回是否计算成功
}

// procedure BigNumberBiPolynomialGaloisAddWord(P: TCnBigNumberBiPolynomial; N: Int64; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的二元大整系数多项式的各项系数加上 N 再 mod Prime，注意不是常系数，但对于稀疏列表来说没啥意义，不实现}

// procedure BigNumberBiPolynomialGaloisSubWord(P: TCnBigNumberBiPolynomial; N: Int64; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的二元大整系数多项式的各项系数减去 N 再 mod Prime，注意不是常系数，但对于稀疏列表来说没啥意义，不实现}

procedure BigNumberBiPolynomialGaloisMulWord(P: TCnBigNumberBiPolynomial; N: Int64; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的二元大整系数多项式各项系数乘以 N 再 mod Prime。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: Int64                             - 乘数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure BigNumberBiPolynomialGaloisDivWord(P: TCnBigNumberBiPolynomial; N: Int64; Prime: TCnBigNumber);
{* 将 Prime 次方阶有限域上的二元大整系数多项式各项系数除以 N，也就是乘以 N 的逆元再 mod Prime。

   参数：
     P: TCnBigNumberBiPolynomial          - 待计算的二元大整系数多项式
     N: Int64                             - 除数
     Prime: TCnBigNumber                  - 有限域上界

   返回值：（无）
}

procedure Int64PolynomialToBigNumberPolynomial(Dest: TCnBigNumberPolynomial; Source: TCnInt64Polynomial);
{* 将一元整系数多项式赋值给一大整系数多项式。

   参数：
     Dest: TCnBigNumberPolynomial         - 目标一元大整系数多项式
     Source: TCnInt64Polynomial           - 源一元整系数多项式

   返回值：（无）
}

var
  CnInt64PolynomialOne: TCnInt64Polynomial = nil;
  {* 表示 1 的 Int64 多项式常量}
  CnInt64PolynomialZero: TCnInt64Polynomial = nil;
  {* 表示 0 的 Int64 多项式常量}

  CnBigNumberPolynomialOne: TCnBigNumberPolynomial = nil;
  {* 表示 1 的大数多项式常量}
  CnBigNumberPolynomialZero: TCnBigNumberPolynomial = nil;
  {* 表示 0 的大数多项式常量}

implementation

resourcestring
  SCnErrorPolynomialInvalidDegree = 'Invalid Degree %d';
  SCnErrorPolynomialInvalidExponent = 'Invalid Exponent %d';
  SCnErrorPolynomialDegreeTooLarge = 'Degree Too Large';
  SCnErrorPolynomialGCDMustOne = 'Modular Inverse Need GCD = 1';
  SCnErrorPolynomialGaloisInvalidDegree = 'Galois Division Polynomial Invalid Degree';

var
  FLocalInt64PolynomialPool: TCnInt64PolynomialPool = nil;
  FLocalInt64RationalPolynomialPool: TCnInt64RationalPolynomialPool = nil;
  FLocalBigNumberPolynomialPool: TCnBigNumberPolynomialPool = nil;
  FLocalBigNumberRationalPolynomialPool: TCnBigNumberRationalPolynomialPool = nil;
  FLocalBigNumberPool: TCnBigNumberPool = nil;
  FLocalInt64BiPolynomialPool: TCnInt64BiPolynomialPool = nil;
  FLocalBigNumberBiPolynomialPool: TCnBigNumberBiPolynomialPool = nil;
  FLocalBigComplexPool: TCnBigComplexPool = nil;
  FLocalBigComplexPolynomialPool: TCnBigComplexPolynomialPool = nil;
  FLocalBigComplexDecimalPool: TCnBigComplexDecimalPool = nil;
  FLocalBigComplexDecimalPolynomialPool: TCnBigComplexDecimalPolynomialPool = nil;

procedure CheckDegree(Degree: Integer);
begin
  if Degree < 0 then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidDegree, [Degree]);
end;

function VarPower(const VarName: string; E: Integer): string;
begin
  if E = 0 then
    Result := ''
  else if E = 1 then
    Result := VarName
  else
    Result := VarName + '^' + IntToStr(E);
end;

function VarPower2(const Var1Name, Var2Name: string; E1, E2: Integer): string;
begin
  Result := VarPower(Var1Name, E1) + VarPower(Var2Name, E2);
end;

// 多项式系数转字符串时封装的规则，DecStr 是该系数的字符串形式，可能有 - 号，
// 返回值当系数非 0 时为 True，表示后面需要加单项式
function VarItemFactor(var Res: string; ExpsIsZero: Boolean; const DecStr: string): Boolean;
var
  IsPositive, IsNegative, IsZero, IsOne, IsNegOne: Boolean;
begin
  Result := True;
  if Length(DecStr) = 0 then
    Exit;

  IsZero := (DecStr = '0') or (DecStr = '-0');
  IsOne := DecStr = '1';
  IsNegOne := DecStr = '-1';

  IsNegative := (not IsZero) and (DecStr[1] = '-');
  IsPositive := (not IsZero) and (DecStr[1] <> '-');

  if IsZero then // 零系数
  begin
    if ExpsIsZero and (Res = '') then
      Res := '0';
    // 否则对 Res 啥都不做
    Result := False;
  end
  else if IsPositive then // 大于 0
  begin
    if IsOne and not ExpsIsZero then  // 非常数项的 1 系数无需显示
    begin
      if Res <> '' then  // 最高项 Res 为空，无需加号
        Res := Res + '+';
    end
    else
    begin
      if Res = '' then  // 最高项无需加号
        Res := DecStr
      else
        Res := Res + '+' + DecStr;
    end;
  end
  else if IsNegative then // 小于 0，要用减号
  begin
    if IsNegOne and not ExpsIsZero then // 非常数项的 -1 无需显示 1，只需减号
      Res := Res + '-'
    else
      Res := Res + DecStr; // DecStr 里有减号
  end;
end;

// 封装的从 TVarRec 也就是 array of const 元素里返回 Int64 的函数
function ExtractInt64FromArrayConstElement(Element: TVarRec): Int64;
begin
  case Element.VType of
  vtInteger:
    begin
      Result := Element.VInteger;
    end;
  vtInt64:
    begin
      Result := Element.VInt64^;
    end;
  vtBoolean:
    begin
      if Element.VBoolean then
        Result := 1
      else
        Result := 0;
    end;
  vtString:
    begin
      Result := StrToInt(string(Element.VString^));
    end;
  else
    raise ECnPolynomialException.CreateFmt(SInvalidInteger, ['Coefficients ' + Element.VString^]);
  end;
end;

// 封装的从 TVarRec 也就是 array of const 元素里返回大数字符串的函数
function ExtractBigNumberFromArrayConstElement(Element: TVarRec): string;
begin
  Result := '';
  case Element.VType of
  vtInteger:
    begin
      Result := IntToStr(Element.VInteger);
    end;
  vtInt64:
    begin
      Result := IntToStr(Element.VInt64^);
    end;
  vtBoolean:
    begin
      if Element.VBoolean then
        Result := '1'
      else
        Result := '0';
    end;
  vtString:
    begin
      Result := string(Element.VString^);
    end;
  vtObject:
    begin
      // 接受 TCnBigNumber 并从中复制值
      if Element.VObject is TCnBigNumber then
        Result := (Element.VObject as TCnBigNumber).ToDec;
    end;
  else
    raise ECnPolynomialException.CreateFmt(SInvalidInteger, ['Coefficients ' + Element.VString^]);
  end;
end;

function Exponent128IsZero(Exponent, ExponentHi: Int64): Boolean;
begin
  Result := (Exponent = 0) and (ExponentHi = 0);
end;

function Exponent128IsOne(Exponent, ExponentHi: Int64): Boolean;
begin
  Result := (Exponent = 1) and (ExponentHi = 0);
end;

procedure ExponentShiftRightOne(var Exponent, ExponentHi: Int64);
begin
  Exponent := Exponent shr 1;
  if (ExponentHi and 1) <> 0 then
    Exponent := Exponent or $8000000000000000;
  ExponentHi := ExponentHi shr 1;
end;

{ TCnInt64Polynomial }

procedure TCnInt64Polynomial.CorrectTop;
begin
  while (MaxDegree > 0) and (Items[MaxDegree] = 0) do
    Delete(MaxDegree);
end;

constructor TCnInt64Polynomial.Create;
begin
  inherited;
  Add(0);   // 常系数项
end;

constructor TCnInt64Polynomial.Create(LowToHighCoefficients: array of const);
begin
  inherited Create;
  SetCoefficents(LowToHighCoefficients);
end;

destructor TCnInt64Polynomial.Destroy;
begin

  inherited;
end;

procedure TCnInt64Polynomial.EnsureDegree(Degree: Integer);
begin
  if Degree > MaxDegree then
    SetMaxDegree(Degree);
end;

function TCnInt64Polynomial.GetMaxDegree: Integer;
begin
  if Count = 0 then
    Add(0);
  Result := Count - 1;
end;

function TCnInt64Polynomial.IsMonic: Boolean;
begin
  Result := Int64PolynomialIsMonic(Self);
end;

function TCnInt64Polynomial.IsNegOne: Boolean;
begin
  Result := Int64PolynomialIsNegOne(Self);
end;

function TCnInt64Polynomial.IsOne: Boolean;
begin
  Result := Int64PolynomialIsOne(Self);
end;

function TCnInt64Polynomial.IsZero: Boolean;
begin
  Result := Int64PolynomialIsZero(Self);
end;

procedure TCnInt64Polynomial.Negate;
begin
  Int64PolynomialNegate(Self);
end;

procedure TCnInt64Polynomial.SetCoefficent(Degree, Coefficient: Integer);
begin
  CheckDegree(Degree);
  EnsureDegree(Degree);
  Items[Degree] := Coefficient;
end;

procedure TCnInt64Polynomial.SetCoefficents(LowToHighCoefficients: array of const);
var
  I: Integer;
begin
  Clear;
  for I := Low(LowToHighCoefficients) to High(LowToHighCoefficients) do
    Add(ExtractInt64FromArrayConstElement(LowToHighCoefficients[I]));

  if Count = 0 then
    Add(0)
  else
    CorrectTop;
end;

procedure TCnInt64Polynomial.SetMaxDegree(const Value: Integer);
begin
  CheckDegree(Value);
  Count := Value + 1;
end;

procedure TCnInt64Polynomial.SetOne;
begin
  Int64PolynomialSetOne(Self);
end;

procedure TCnInt64Polynomial.SetString(const Poly: string);
begin
  Int64PolynomialSetString(Self, Poly);
end;

procedure TCnInt64Polynomial.SetZero;
begin
  Int64PolynomialSetZero(Self);
end;

function TCnInt64Polynomial.ToString: string;
begin
  Result := Int64PolynomialToString(Self);
end;

// ============================ 多项式系列操作函数 =============================

function Int64PolynomialNew: TCnInt64Polynomial;
begin
  Result := TCnInt64Polynomial.Create;
end;

procedure Int64PolynomialFree(P: TCnInt64Polynomial);
begin
  P.Free;
end;

function Int64PolynomialDuplicate(P: TCnInt64Polynomial): TCnInt64Polynomial;
begin
  if P = nil then
  begin
    Result := nil;
    Exit;
  end;

  Result := Int64PolynomialNew;
  if Result <> nil then
    Int64PolynomialCopy(Result, P);
end;

function Int64PolynomialCopy(Dest: TCnInt64Polynomial;
  Source: TCnInt64Polynomial): TCnInt64Polynomial;
var
  I: Integer;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    Dest.Clear;
    for I := 0 to Source.Count - 1 do
      Dest.Add(Source[I]);
    Dest.CorrectTop;
  end;
end;

procedure Int64PolynomialSwap(A: TCnInt64Polynomial; B: TCnInt64Polynomial);
var
  T: TCnInt64Polynomial;
begin
  if (A = nil) or (B = nil) then
    Exit;

  T := FLocalInt64PolynomialPool.Obtain;
  try
    Int64PolynomialCopy(T, A);
    Int64PolynomialCopy(A, B);
    Int64PolynomialCopy(B, T);
  finally
    FLocalInt64PolynomialPool.Recycle(T);
  end;
end;

function Int64PolynomialToString(P: TCnInt64Polynomial; const VarName: string): string;
var
  I: Integer;
begin
  Result := '';
  if Int64PolynomialIsZero(P) then
  begin
    Result := '0';
    Exit;
  end;

  for I := P.MaxDegree downto 0 do
  begin
    if VarItemFactor(Result, (I = 0), IntToStr(P[I])) then
      Result := Result + VarPower(VarName, I);
  end;
end;

{$WARNINGS OFF}

function Int64PolynomialSetString(P: TCnInt64Polynomial;
  const Str: string; const VarName: string): Boolean;
var
  C, Ptr: PChar;
  Num: string;
  MDFlag, E: Integer;
  F: Int64;
  IsNeg: Boolean;
begin
  Result := False;
  if Str = '' then
    Exit;

  MDFlag := -1;
  C := @Str[1];

  while C^ <> #0 do
  begin
    if not (C^ in ['+', '-', '0'..'9']) and (C^ <> VarName) then
    begin
      Inc(C);
      Continue;
    end;

    IsNeg := False;
    if C^ = '+' then
      Inc(C)
    else if C^ = '-' then
    begin
      IsNeg := True;
      Inc(C);
    end;

    F := 1;
    if C^ in ['0'..'9'] then // 找系数
    begin
      Ptr := C;
      while C^ in ['0'..'9'] do
        Inc(C);

      // Ptr 到 C 之间是数字，代表一个系数
      SetString(Num, Ptr, C - Ptr);
      F := StrToInt64(Num);
      if IsNeg then
        F := -F;
    end
    else if IsNeg then
      F := -F;

    if C^ = VarName then
    begin
      E := 1;
      Inc(C);
      if C^ = '^' then // 找指数
      begin
        Inc(C);
        if C^ in ['0'..'9'] then
        begin
          Ptr := C;
          while C^ in ['0'..'9'] do
            Inc(C);

          // Ptr 到 C 之间是数字，代表一个指数
          SetString(Num, Ptr, C - Ptr);
          E := StrToInt64(Num);
        end;
      end;
    end
    else
      E := 0;

    // 指数找完了，凑
    if MDFlag = -1 then // 第一个指数是 MaxDegree
    begin
      P.MaxDegree := E;
      MDFlag := 0;
    end;

    P[E] := F;
  end;
end;

{$WARNINGS ON}

function Int64PolynomialIsZero(P: TCnInt64Polynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and (P[0] = 0);
end;

procedure Int64PolynomialSetZero(P: TCnInt64Polynomial);
begin
  P.Clear;
  P.Add(0);
end;

function Int64PolynomialIsOne(P: TCnInt64Polynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and (P[0] = 1);
end;

procedure Int64PolynomialSetOne(P: TCnInt64Polynomial);
begin
  P.Clear;
  P.Add(1);
end;

function Int64PolynomialIsNegOne(P: TCnInt64Polynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and (P[0] = -1);
end;

procedure Int64PolynomialNegate(P: TCnInt64Polynomial);
var
  I: Integer;
begin
  for I := 0 to P.MaxDegree do
    P[I] := -P[I];
end;

function Int64PolynomialIsMonic(P: TCnInt64Polynomial): Boolean;
begin
  Result := P[P.MaxDegree] = 1;
end;

procedure Int64PolynomialShiftLeft(P: TCnInt64Polynomial; N: Integer);
begin
  if N = 0 then
    Exit
  else if N < 0 then
    Int64PolynomialShiftRight(P, -N)
  else
    P.InsertBatch(0, N);
end;

procedure Int64PolynomialShiftRight(P: TCnInt64Polynomial; N: Integer);
begin
  if N = 0 then
    Exit
  else if N < 0 then
    Int64PolynomialShiftLeft(P, -N)
  else
  begin
    P.DeleteLow(N);

    if P.Count = 0 then
      P.Add(0);
  end;
end;

function Int64PolynomialEqual(A, B: TCnInt64Polynomial): Boolean;
var
  I: Integer;
begin
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  Result := A.MaxDegree = B.MaxDegree;
  if Result then
  begin
    for I := A.MaxDegree downto 0 do
    begin
      if A[I] <> B[I] then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure Int64PolynomialAddWord(P: TCnInt64Polynomial; N: Int64);
begin
  P[0] := P[0] + N;
end;

procedure Int64PolynomialSubWord(P: TCnInt64Polynomial; N: Int64);
begin
  P[0] := P[0] - N;
end;

procedure Int64PolynomialMulWord(P: TCnInt64Polynomial; N: Int64);
var
  I: Integer;
begin
  if N = 0 then
    Int64PolynomialSetZero(P)
  else if N <> 1 then
  begin
    for I := 0 to P.MaxDegree do
      P[I] := P[I] * N;
  end;
end;

procedure Int64PolynomialDivWord(P: TCnInt64Polynomial; N: Int64);
var
  I: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);;

  if N <> 1 then
  begin
    for I := 0 to P.MaxDegree do
      P[I] := P[I] div N;
  end;
end;

procedure Int64PolynomialNonNegativeModWord(P: TCnInt64Polynomial; N: Int64);
var
  I: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);

  for I := 0 to P.MaxDegree do
    P[I] := Int64NonNegativeMod(P[I], N);
end;

function Int64PolynomialAdd(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
var
  I, D1, D2: Integer;
  PBig: TCnInt64Polynomial;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then
      PBig := P1
    else
      PBig := P2;

    Res.MaxDegree := D1; // 考虑到 Res 可能是 P1 或 P2，所以给 Res 的 MaxDegree 赋值得放上面的比较之后
    for I := D1 downto D2 + 1 do
      Res[I] := PBig[I];
  end
  else // D1 = D2 说明俩加式同次
    Res.MaxDegree := D1;

  for I := D2 downto 0 do
    Res[I] := P1[I] + P2[I];
  Res.CorrectTop;
  Result := True;
end;

function Int64PolynomialSub(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
var
  I, D1, D2: Integer;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  Res.MaxDegree := D1;
  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then // 被减式大
    begin
      for I := D1 downto D2 + 1 do
        Res[I] := P1[I];
    end
    else  // 减式大
    begin
      for I := D1 downto D2 + 1 do
        Res[I] := -P2[I];
    end;
  end;

  for I := D2 downto 0 do
    Res[I] := P1[I] - P2[I];
  Res.CorrectTop;
  Result := True;
end;

function Int64PolynomialMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
var
  R: TCnInt64Polynomial;
  I, J: Integer;
begin
  if Int64PolynomialIsZero(P1) or Int64PolynomialIsZero(P2) then
  begin
    Int64PolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalInt64PolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxDegree := P1.MaxDegree + P2.MaxDegree;

  for I := 0 to P1.MaxDegree do
  begin
    // 把第 I 次方的数字乘以 P2 的每一个数字，加到结果的 I 开头的部分
    for J := 0 to P2.MaxDegree do
    begin
      R[I + J] := R[I + J] + P1[I] * P2[J];
    end;
  end;

  R.CorrectTop;
  if (Res = P1) or (Res = P2) then
  begin
    Int64PolynomialCopy(Res, R);
    FLocalInt64PolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function Int64PolynomialDftMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
var
  M1, M2: PCnComplexNumber;
  C1, C2: PCnComplexArray;
  M, I: Integer;
begin
  Result := False;
  M := P1.MaxDegree;
  if M < P2.MaxDegree then
    M := P2.MaxDegree;

  if M < 0 then
    Exit;

  if M = 0 then // 俩常数项，直接算
  begin
    Res.SetMaxDegree(0);
    Res[0] := P1[0] * P2[0];
    Result := True;
    Exit;
  end;

  // M 得到最高次数，加 1 表示多项式最多项数
  Inc(M);

  // 乘以 2 表示多项式积的最多项数
  M := M shl 1;

  // 再找比 M 大或者等于 M 的 2 的整数次幂
  if not IsUInt32PowerOf2(Cardinal(M)) then
  begin
    // 如果不是 2 的整数次幂
    M := GetUInt32HighBits(Cardinal(M)); // M 得到最高位的 1 的位置，不会是 -1
    if M > 30 then
      raise ECnPolynomialException.Create(SCnErrorPolynomialDegreeTooLarge);

    Inc(M);
    M := 1 shl M; // 得到比 M 大的最小的 2 的整数次幂
  end;

  M1 := GetMemory(M * SizeOf(TCnComplexNumber));
  M2 := GetMemory(M * SizeOf(TCnComplexNumber));

  C1 := PCnComplexArray(M1);
  C2 := PCnComplexArray(M2);

  try
    for I := 0 to M - 1 do
    begin
      ComplexNumberSetZero(C1^[I]);
      ComplexNumberSetZero(C2^[I]);
    end;

    for I := 0 to P1.MaxDegree do
    begin
      C1^[I].R := P1[I];
      C1^[I].I := 0.0;
    end;
    for I := 0 to P2.MaxDegree do
    begin
      C2^[I].R := P2[I];
      C2^[I].I := 0.0;
    end;

    CnFFT(C1, M);
    CnFFT(C2, M);        // 得到两组点值

    for I := 0 to M - 1 do   // 点值相乘
      ComplexNumberMul(C1^[I], C1^[I], C2^[I]);

    Result := CnIFFT(C1, M);       // 点值变回系数表达式

    Res.SetZero;
    Res.SetMaxDegree(M);
    for I := 0 to M - 1 do   // 点值四舍五入整数部分取整
      Res[I] := Round(C1^[I].R);

    Res.CorrectTop;
  finally
    FreeMemory(M1);
    FreeMemory(M2);
  end;
end;

function Int64PolynomialNttMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
var
  M1, M2: PInt64;
  C1, C2: PInt64Array;
  M, I: Integer;
begin
  Result := False;
  M := P1.MaxDegree;
  if M < P2.MaxDegree then
    M := P2.MaxDegree;

  if M < 0 then
    Exit;

  if M = 0 then // 俩常数项，直接算
  begin
    Res.SetMaxDegree(0);
    Res[0] := P1[0] * P2[0];
    Result := True;
    Exit;
  end;

  // M 得到最高次数，加 1 表示多项式最多项数
  Inc(M);

  // 乘以 2 表示多项式积的最多项数
  M := M shl 1;

  // 再找比 M 大或者等于 M 的 2 的整数次幂
  if not IsUInt32PowerOf2(Cardinal(M)) then
  begin
    // 如果不是 2 的整数次幂
    M := GetUInt32HighBits(Cardinal(M)); // M 得到最高位的 1 的位置，不会是 -1
    if M > 30 then
      raise ECnPolynomialException.Create(SCnErrorPolynomialDegreeTooLarge);

    Inc(M);
    M := 1 shl M; // 得到比 M 大的最小的 2 的整数次幂
  end;

  M1 := GetMemory(M * SizeOf(Int64));
  M2 := GetMemory(M * SizeOf(Int64));

  C1 := PInt64Array(M1);
  C2 := PInt64Array(M2);

  try
    for I := 0 to M - 1 do
    begin
      C1^[I] := 0;
      C2^[I] := 0;
    end;

    for I := 0 to P1.MaxDegree do
      C1^[I] := P1[I];

    for I := 0 to P2.MaxDegree do
      C2^[I] := P2[I];

    CnNTT(C1, M);
    CnNTT(C2, M);        // 得到两组点值

    for I := 0 to M - 1 do   // 点值相乘，但容易溢出
      C1^[I] := C1^[I] * C2^[I];

    Result := CnINTT(C1, M);       // 点值变回系数表达式

    Res.SetZero;
    Res.SetMaxDegree(M);
    for I := 0 to M - 1 do
      Res[I] := C1^[I];

    Res.CorrectTop;
  finally
    FreeMemory(M1);
    FreeMemory(M2);
  end;
end;

function Int64PolynomialDiv(Res: TCnInt64Polynomial; Remain: TCnInt64Polynomial;
  P: TCnInt64Polynomial; Divisor: TCnInt64Polynomial; ErrMulFactor: PInt64): Boolean;
var
  SubRes: TCnInt64Polynomial; // 容纳递减差
  MulRes: TCnInt64Polynomial; // 容纳除数乘积
  DivRes: TCnInt64Polynomial; // 容纳临时商
  I, D: Integer;
  T: Int64;
begin
  if Int64PolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      Int64PolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      Int64PolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;

  try
    SubRes := FLocalInt64PolynomialPool.Obtain;
    Int64PolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := FLocalInt64PolynomialPool.Obtain;
    DivRes.MaxDegree := D;
    MulRes := FLocalInt64PolynomialPool.Obtain;

    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then                 // 中间结果可能跳位
        Continue;

      // 判断 Divisor[Divisor.MaxDegree] 是否能整除 SubRes[P.MaxDegree - I] 不能则说明超出了整型多项式范围，无法支持，只能出错
      if (SubRes[P.MaxDegree - I] mod Divisor[Divisor.MaxDegree]) <> 0 then
      begin
        Result := False;
        if ErrMulFactor <> nil then
        begin
          // Divisor[Divisor.MaxDegree] 乘以两者的最大公约数
          ErrMulFactor^ := Divisor[Divisor.MaxDegree] *
            CnInt64GreatestCommonDivisor(SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]);
        end;
        Exit;
      end;

      Int64PolynomialCopy(MulRes, Divisor);
      Int64PolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次
      T := SubRes[P.MaxDegree - I] div MulRes[MulRes.MaxDegree];
      Int64PolynomialMulWord(MulRes, T); // 除式乘到最高次系数相同
      DivRes[D - I] := T;                // 商放到 DivRes 位置
      Int64PolynomialSub(SubRes, SubRes, MulRes);              // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      Int64PolynomialCopy(Remain, SubRes);
    if Res <> nil then
      Int64PolynomialCopy(Res, DivRes);

    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(SubRes);
    FLocalInt64PolynomialPool.Recycle(MulRes);
    FLocalInt64PolynomialPool.Recycle(DivRes);
  end;
end;

function Int64PolynomialMod(Res: TCnInt64Polynomial; P: TCnInt64Polynomial;
  Divisor: TCnInt64Polynomial; ErrMulFactor: PInt64): Boolean;
begin
  Result := Int64PolynomialDiv(nil, Res, P, Divisor, ErrMulFactor);
end;

function Int64PolynomialPower(Res: TCnInt64Polynomial;
  P: TCnInt64Polynomial; Exponent: Int64): Boolean;
var
  T: TCnInt64Polynomial;
begin
  if Exponent = 0 then
  begin
    Res.SetCoefficents([1]);
    Result := True;
    Exit;
  end
  else if Exponent = 1 then
  begin
    if Res <> P then
      Int64PolynomialCopy(Res, P);
    Result := True;
    Exit;
  end
  else if Exponent < 0 then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidExponent, [Exponent]);

  T := FLocalInt64PolynomialPool.Obtain;
  Int64PolynomialCopy(T, P);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetCoefficents([1]);
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        Int64PolynomialMul(Res, Res, T);

      Exponent := Exponent shr 1;
      if Exponent > 0 then
        Int64PolynomialMul(T, T, T);
    end;
    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(T);
  end;
end;

function Int64PolynomialReduce(P: TCnInt64Polynomial): Int64;
var
  I: Integer;
  D: Int64;

  function Gcd(A, B: Int64): Int64;
  var
    T: Int64;
  begin
    while B <> 0 do
    begin
      T := B;
      B := A mod B;
      A := T;
    end;
    Result := A;
  end;

begin
  if P.MaxDegree = 0 then
  begin
    Result := P[P.MaxDegree];
    if P[P.MaxDegree] <> 0 then
      P[P.MaxDegree] := 1;
  end
  else
  begin
    D := P[0];
    for I := 0 to P.MaxDegree - 1 do
    begin
      D := Gcd(D, P[I + 1]);
      if D = 1 then
        Break;
    end;

    Result := D;
    if Result > 1 then
      Int64PolynomialDivWord(P, Result);
  end;
end;

procedure Int64PolynomialCentralize(P: TCnInt64Polynomial; Modulus: Int64);
var
  I: Integer;
  K: Int64;
begin
  K := Modulus div 2;
  for I := 0 to P.MaxDegree do
    if P[I] > K then
      P[I] := P[I] - Modulus;
end;

function Int64PolynomialGreatestCommonDivisor(Res: TCnInt64Polynomial;
  P1, P2: TCnInt64Polynomial): Boolean;
var
  A, B, C: TCnInt64Polynomial;
  MF: Int64;
  GcdValue: Int64;
begin
  A := nil;
  B := nil;
  C := nil;

  try
    A := FLocalInt64PolynomialPool.Obtain;
    B := FLocalInt64PolynomialPool.Obtain;

    if P1.MaxDegree >= P2.MaxDegree then
    begin
      Int64PolynomialCopy(A, P1);
      Int64PolynomialCopy(B, P2);
    end
    else
    begin
      Int64PolynomialCopy(A, P2);
      Int64PolynomialCopy(B, P1);
    end;

    // 特殊处理：如果两个多项式都是常数项，直接计算整数 GCD
    if (A.MaxDegree = 0) and (B.MaxDegree = 0) then
    begin
      GcdValue := CnInt64GreatestCommonDivisor(A[0], B[0]);
      Res.SetCoefficents([GcdValue]);
      Result := True;
      Exit;
    end;

    C := FLocalInt64PolynomialPool.Obtain;
    while not B.IsZero do
    begin
      Int64PolynomialCopy(C, B);        // 备份 B
      while not Int64PolynomialMod(B, A, B, @MF) do   // A mod B 给 B
        Int64PolynomialMulWord(A, MF);

      // B 要系数约分化简
      Int64PolynomialReduce(B);
      Int64PolynomialCopy(A, C);        // 原始 B 给 A
    end;

    Int64PolynomialCopy(Res, A);
    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(A);
    FLocalInt64PolynomialPool.Recycle(B);
    FLocalInt64PolynomialPool.Recycle(C);
  end;
end;

function Int64PolynomialLeastCommonMultiple(Res: TCnInt64Polynomial;
  P1, P2: TCnInt64Polynomial): Boolean;
var
  G, M, R: TCnInt64Polynomial;
begin
  Result := False;
  if Int64PolynomialEqual(P1, P2) then
  begin
    Int64PolynomialCopy(Res, P1);
    Result := True;
    Exit;
  end;

  G := nil;
  M := nil;
  R := nil;

  try
    G := FLocalInt64PolynomialPool.Obtain;
    M := FLocalInt64PolynomialPool.Obtain;
    R := FLocalInt64PolynomialPool.Obtain;

    if not Int64PolynomialMul(M, P1, P2) then
      Exit;

    if not Int64PolynomialGreatestCommonDivisor(G, P1, P2) then
      Exit;

    if not Int64PolynomialDiv(Res, R, M, G) then
      Exit;

    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(R);
    FLocalInt64PolynomialPool.Recycle(M);
    FLocalInt64PolynomialPool.Recycle(G);
  end;
end;

function Int64PolynomialCompose(Res: TCnInt64Polynomial;
  F, P: TCnInt64Polynomial): Boolean;
var
  I: Integer;
  R, X, T: TCnInt64Polynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    Res[0] := F[0];
    Result := True;
    Exit;
  end;

  if (Res = F) or (Res = P) then
    R := FLocalInt64PolynomialPool.Obtain
  else
    R := Res;

  X := FLocalInt64PolynomialPool.Obtain;
  T := FLocalInt64PolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      Int64PolynomialCopy(T, X);
      Int64PolynomialMulWord(T, F[I]);
      Int64PolynomialAdd(R, R, T);

      if I <> F.MaxDegree then
        Int64PolynomialMul(X, X, P);
    end;

    if (Res = F) or (Res = P) then
    begin
      Int64PolynomialCopy(Res, R);
      FLocalInt64PolynomialPool.Recycle(R);
    end;
  finally
    FLocalInt64PolynomialPool.Recycle(X);
    FLocalInt64PolynomialPool.Recycle(T);
  end;
  Result := True;
end;

function Int64PolynomialGetValue(F: TCnInt64Polynomial; X: Int64): Int64;
var
  I: Integer;
  T: Int64;
begin
  Result := F[0];
  if (X = 0) or (F.MaxDegree = 0) then    // 只有常数项的情况下，得常数项
    Exit;

  T := X;

  // 把 F 中的每个系数都和 X 的对应次幂相乘，最后相加
  for I := 1 to F.MaxDegree do
  begin
    Result := Result + F[I] * T;
    if I <> F.MaxDegree then
      T := T * X;
  end;
end;

procedure Int64PolynomialReduce2(P1, P2: TCnInt64Polynomial);
var
  D: TCnInt64Polynomial;
begin
  if P1 = P2 then
  begin
    P1.SetOne;
    Exit;
  end;

  D := FLocalInt64PolynomialPool.Obtain;
  try
    if not Int64PolynomialGreatestCommonDivisor(D, P1, P2) then
      Exit;

    if not D.IsOne then
    begin
      Int64PolynomialDiv(P1, nil, P1, D);
      Int64PolynomialDiv(P1, nil, P1, D);
    end;
  finally
    FLocalInt64PolynomialPool.Recycle(D);
  end;
end;

function Int64PolynomialGaloisEqual(A, B: TCnInt64Polynomial; Prime: Int64): Boolean;
var
  I: Integer;
begin
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  Result := A.MaxDegree = B.MaxDegree;
  if Result then
  begin
    for I := A.MaxDegree downto 0 do
    begin
      if (A[I] <> B[I]) and (Int64NonNegativeMod(A[I], Prime) <> Int64NonNegativeMod(B[I], Prime)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

procedure Int64PolynomialGaloisNegate(P: TCnInt64Polynomial; Prime: Int64);
var
  I: Integer;
begin
  for I := 0 to P.MaxDegree do
    P[I] := Int64NonNegativeMod(-P[I], Prime);
end;

function Int64PolynomialGaloisAdd(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialAdd(Res, P1, P2);
  if Result then
  begin
    Int64PolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      Int64PolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function Int64PolynomialGaloisSub(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialSub(Res, P1, P2);
  if Result then
  begin
    Int64PolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      Int64PolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function Int64PolynomialGaloisMul(Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
var
  R: TCnInt64Polynomial;
  I, J: Integer;
  T: Int64;
begin
  if Int64PolynomialIsZero(P1) or Int64PolynomialIsZero(P2) then
  begin
    Int64PolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalInt64PolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxDegree := P1.MaxDegree + P2.MaxDegree;

  for I := 0 to P1.MaxDegree do
  begin
    // 把第 I 次方的数字乘以 P2 的每一个数字，加到结果的 I 开头的部分，再取模
    for J := 0 to P2.MaxDegree do
    begin
      // 容易溢出，不能直接相乘
      T := Int64NonNegativeMulMod(P1[I], P2[J], Prime);
      R[I + J] := Int64NonNegativeMod(R[I + J] + Int64NonNegativeMod(T, Prime), Prime);
      // TODO: 暂未处理加法溢出的情况
    end;
  end;

  R.CorrectTop;

  // 再对本原多项式取模，注意这里传入的本原多项式是 mod 操作的除数，不是本原多项式参数
  if Primitive <> nil then
    Int64PolynomialGaloisMod(R, R, Primitive, Prime);

  if (Res = P1) or (Res = P2) then
  begin
    Int64PolynomialCopy(Res, R);
    FLocalInt64PolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function Int64PolynomialGaloisDiv(Res: TCnInt64Polynomial; Remain: TCnInt64Polynomial;
  P: TCnInt64Polynomial; Divisor: TCnInt64Polynomial; Prime: Int64;
  Primitive: TCnInt64Polynomial; ErrMulFactor: PInt64): Boolean;
var
  SubRes: TCnInt64Polynomial; // 容纳递减差
  MulRes: TCnInt64Polynomial; // 容纳除数乘积
  DivRes: TCnInt64Polynomial; // 容纳临时商
  I, D: Integer;
  K, T: Int64;
begin
  if Int64PolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  // 无需担心不能整除的问题，因为有逆元和 mod 操作，但逆元不存在时不好整只能出错

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      Int64PolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      Int64PolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;

  try
    SubRes := FLocalInt64PolynomialPool.Obtain;
    Int64PolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := FLocalInt64PolynomialPool.Obtain;
    DivRes.MaxDegree := D;
    MulRes := FLocalInt64PolynomialPool.Obtain;

    if Divisor[Divisor.MaxDegree] = 1 then
      K := 1
    else
      K := CnInt64ModularInverse2(Divisor[Divisor.MaxDegree], Prime); // K 是除式最高位的逆元，注意可能为 0

    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then               // 中间结果可能跳位
        Continue;
      Int64PolynomialCopy(MulRes, Divisor);
      Int64PolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次

      if K <> 0 then // 有模逆元
      begin
        // 除式要乘一个数，这个数是 SubRes 最高位除以除式最高位得到的结果，也即 SubRes 最高位乘以除式最高位的逆元再 mod Prime
        T := Int64NonNegativeMulMod(SubRes[P.MaxDegree - I], K, Prime);
        Int64PolynomialGaloisMulWord(MulRes, T, Prime);          // 除式乘到最高次系数相同
      end
      else  // Prime 和除式最高位不互素时模逆元 K 不存在，要分整除和不整除两种情况
      begin
        T := SubRes[P.MaxDegree - I] mod Divisor[Divisor.MaxDegree];
        if T <> 0 then  // 不整除又没有模逆元，无论如何都没法除，只能出错退出
        begin
          Result := False;
          if ErrMulFactor <> nil then
          begin
            // Divisor[Divisor.MaxDegree] 乘以两者的最大公约数
            ErrMulFactor^ := Divisor[Divisor.MaxDegree] *
              CnInt64GreatestCommonDivisor(SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]);
          end;
          Exit;
        end
        else
        begin
          T := SubRes[P.MaxDegree - I] div Divisor[Divisor.MaxDegree];
          Int64PolynomialGaloisMulWord(MulRes, T, Prime);      // 除式乘到最高次系数相同
        end;
      end;

      DivRes[D - I] := T;                                      // 对应位的商放到 DivRes 位置
      Int64PolynomialGaloisSub(SubRes, SubRes, MulRes, Prime); // 减求模后结果重新放回 SubRes
    end;

    // 商与余式都需要再模本原多项式
    if Primitive <> nil then
    begin
      Int64PolynomialGaloisMod(SubRes, SubRes, Primitive, Prime);
      Int64PolynomialGaloisMod(DivRes, DivRes, Primitive, Prime);
    end;

    if Remain <> nil then
      Int64PolynomialCopy(Remain, SubRes);
    if Res <> nil then
      Int64PolynomialCopy(Res, DivRes);
    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(SubRes);
    FLocalInt64PolynomialPool.Recycle(MulRes);
    FLocalInt64PolynomialPool.Recycle(DivRes);
  end;
end;

function Int64PolynomialGaloisMod(Res: TCnInt64Polynomial;
  P: TCnInt64Polynomial; Divisor: TCnInt64Polynomial; Prime: Int64;
  Primitive: TCnInt64Polynomial; ErrMulFactor: PInt64): Boolean;
begin
  Result := Int64PolynomialGaloisDiv(nil, Res, P, Divisor, Prime, Primitive, ErrMulFactor);
end;

function Int64PolynomialGaloisPower(Res, P: TCnInt64Polynomial;
  Exponent: Int64; Prime: Int64; Primitive: TCnInt64Polynomial;
  ExponentHi: Int64): Boolean;
var
  T: TCnInt64Polynomial;
begin
  if Exponent128IsZero(Exponent, ExponentHi) then
  begin
    Res.SetCoefficents([1]);
    Result := True;
    Exit;
  end
  else if Exponent128IsOne(Exponent, ExponentHi) then
  begin
    if Res <> P then
      Int64PolynomialCopy(Res, P);
    Result := True;
    Exit;
  end;

  T := FLocalInt64PolynomialPool.Obtain;
  Int64PolynomialCopy(T, P);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetCoefficents([1]);
    while not Exponent128IsZero(Exponent, ExponentHi) do
    begin
      if (Exponent and 1) <> 0 then
        Int64PolynomialGaloisMul(Res, Res, T, Prime, Primitive);

      ExponentShiftRightOne(Exponent, ExponentHi);
      if not Exponent128IsZero(Exponent, ExponentHi) then
        Int64PolynomialGaloisMul(T, T, T, Prime, Primitive);
    end;
    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(T);
  end;
end;

procedure Int64PolynomialGaloisAddWord(P: TCnInt64Polynomial; N: Int64;
  Prime: Int64);
begin
  if N <> 0 then
    P[0] := Int64NonNegativeMod(P[0] + N, Prime);
end;

procedure Int64PolynomialGaloisSubWord(P: TCnInt64Polynomial; N: Int64;
  Prime: Int64);
begin
  if N <> 0 then
    P[0] := Int64NonNegativeMod(P[0] - N, Prime);
end;

procedure Int64PolynomialGaloisMulWord(P: TCnInt64Polynomial; N: Int64;
  Prime: Int64);
var
  I: Integer;
begin
  if N = 0 then
  begin
    Int64PolynomialSetZero(P);
  end
  else if N <> 1 then
  begin
    for I := 0 to P.MaxDegree do
      P[I] := Int64NonNegativeMulMod(P[I], N, Prime);
  end;
end;

procedure Int64PolynomialGaloisDivWord(P: TCnInt64Polynomial; N: Int64;
  Prime: Int64);
var
  I: Integer;
  K: Int64;
  B: Boolean;
begin
  if (N = 0) or (Prime = 0) then
    raise EDivByZero.Create(SDivByZero);

  B := N < 0;
  if B then
    N := -N;

  K := CnInt64ModularInverse2(N, Prime);
  for I := 0 to P.MaxDegree do
  begin
    P[I] := Int64NonNegativeMulMod(P[I], K, Prime);
    if B then
      P[I] := Prime - P[I];
  end;
end;

function Int64PolynomialGaloisMonic(P: TCnInt64Polynomial; Prime: Int64): Integer;
begin
  Result := P[P.MaxDegree];
  if (Result <> 1) and (Result <> 0) then
    Int64PolynomialGaloisDivWord(P, Result, Prime);
end;

function Int64PolynomialGaloisGreatestCommonDivisor(Res: TCnInt64Polynomial;
  P1, P2: TCnInt64Polynomial; Prime: Int64): Boolean;
var
  A, B, C: TCnInt64Polynomial;
begin
  Result := False;
  A := nil;
  B := nil;
  C := nil;

  try
    A := FLocalInt64PolynomialPool.Obtain;
    B := FLocalInt64PolynomialPool.Obtain;

    if P1.MaxDegree >= P2.MaxDegree then
    begin
      Int64PolynomialCopy(A, P1);
      Int64PolynomialCopy(B, P2);
    end
    else
    begin
      Int64PolynomialCopy(A, P2);
      Int64PolynomialCopy(B, P1);
    end;

    C := FLocalInt64PolynomialPool.Obtain;
    while not B.IsZero do
    begin
      Int64PolynomialCopy(C, B);          // 备份 B
      if not Int64PolynomialGaloisMod(B, A, B, Prime) then  // A mod B 给 B
        Exit;

      if B.MaxDegree = 0 then  // 如果是常数项则变为 1
      begin
        if B[0] <> 0 then
          B[0] := 1;
      end;

      Int64PolynomialCopy(A, C);          // 原始 B 给 A
    end;

    Int64PolynomialCopy(Res, A);
    Int64PolynomialGaloisMonic(Res, Prime);      // 首项化为一
    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(A);
    FLocalInt64PolynomialPool.Recycle(B);
    FLocalInt64PolynomialPool.Recycle(C);
  end;
end;

function Int64PolynomialGaloisLeastCommonMultiple(Res: TCnInt64Polynomial;
  P1, P2: TCnInt64Polynomial; Prime: Int64): Boolean;
var
  G, M, R: TCnInt64Polynomial;
begin
  Result := False;
  if Int64PolynomialEqual(P1, P2) then
  begin
    Int64PolynomialCopy(Res, P1);
    Result := True;
    Exit;
  end;

  G := nil;
  M := nil;
  R := nil;

  try
    G := FLocalInt64PolynomialPool.Obtain;
    M := FLocalInt64PolynomialPool.Obtain;
    R := FLocalInt64PolynomialPool.Obtain;

    if not Int64PolynomialGaloisMul(M, P1, P2, Prime) then
      Exit;

    if not Int64PolynomialGaloisGreatestCommonDivisor(G, P1, P2, Prime) then
      Exit;

    if not Int64PolynomialGaloisDiv(Res, R, M, G, Prime) then
      Exit;

    Result := True;
  finally
    FLocalInt64PolynomialPool.Recycle(R);
    FLocalInt64PolynomialPool.Recycle(M);
    FLocalInt64PolynomialPool.Recycle(G);
  end;
end;

procedure Int64PolynomialGaloisExtendedEuclideanGcd(A, B: TCnInt64Polynomial;
  X, Y: TCnInt64Polynomial; Prime: Int64);
var
  T, P, M: TCnInt64Polynomial;
begin
  if B.IsZero then
  begin
    X.SetZero;
    X[0] := CnInt64ModularInverse2(A[0], Prime);
    // X 得是 A 对于 P 的模逆元而不能像整数的辗转相除法那样是 1
    // 因为 A 可能是不等于 1 的整数
    Y.SetZero;
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := FLocalInt64PolynomialPool.Obtain;
      P := FLocalInt64PolynomialPool.Obtain;
      M := FLocalInt64PolynomialPool.Obtain;

      Int64PolynomialGaloisMod(P, A, B, Prime);

      Int64PolynomialGaloisExtendedEuclideanGcd(B, P, Y, X, Prime);

      // Y := Y - (A div B) * X;
      Int64PolynomialGaloisDiv(P, M, A, B, Prime);
      Int64PolynomialGaloisMul(P, P, X, Prime);
      Int64PolynomialGaloisSub(Y, Y, P, Prime);
    finally
      FLocalInt64PolynomialPool.Recycle(M);
      FLocalInt64PolynomialPool.Recycle(P);
      FLocalInt64PolynomialPool.Recycle(T);
    end;
  end;
end;

procedure Int64PolynomialGaloisModularInverse(Res: TCnInt64Polynomial;
  X, Modulus: TCnInt64Polynomial; Prime: Int64; CheckGcd: Boolean);
var
  X1, Y, G: TCnInt64Polynomial;
begin
  X1 := nil;
  Y := nil;
  G := nil;

  try
    if CheckGcd then
    begin
      G := FLocalInt64PolynomialPool.Obtain;
      Int64PolynomialGaloisGreatestCommonDivisor(G, X, Modulus, Prime);
      if not G.IsOne then
        raise ECnPolynomialException.Create(SCnErrorPolynomialGCDMustOne);
    end;

    X1 := FLocalInt64PolynomialPool.Obtain;
    Y := FLocalInt64PolynomialPool.Obtain;

    Int64PolynomialCopy(X1, X);

    // 扩展欧几里得辗转相除法求二元一次不定整系数多项式方程 A * X - B * Y = 1 的整数解
    Int64PolynomialGaloisExtendedEuclideanGcd(X1, Modulus, Res, Y, Prime);
  finally
    FLocalInt64PolynomialPool.Recycle(X1);
    FLocalInt64PolynomialPool.Recycle(Y);
    FLocalInt64PolynomialPool.Recycle(G);
  end;
end;

function Int64PolynomialGaloisPrimePowerModularInverse(Res: TCnInt64Polynomial;
  X, Modulus: TCnInt64Polynomial; PrimeRoot, Exponent: Integer): Boolean;
var
  F, G, T: TCnInt64Polynomial;
  N: Integer;
  P: Int64;
begin
  // 原始 X 和 Modulus 是模 PrimeRoot^Exponent 下的，各系数对 PrimeRoot 求模得到 F 和 G 俩多项式

  if Exponent < 2 then
    raise ECnPolynomialException.Create(SCnErrorPolynomialInvalidExponent);

  F := nil;
  G := nil;
  T := nil;

  try
    F := FLocalInt64PolynomialPool.Obtain;
    G := FLocalInt64PolynomialPool.Obtain;

    Int64PolynomialCopy(F, X);
    Int64PolynomialCopy(G, Modulus);

    Int64PolynomialNonNegativeModWord(F, PrimeRoot);
    Int64PolynomialNonNegativeModWord(G, PrimeRoot);

    T := FLocalInt64PolynomialPool.Obtain;
    Int64PolynomialGaloisGreatestCommonDivisor(T, F, G, PrimeRoot);

    Result := T.IsOne;  // F G 释放了可以复用
    if not Result then  // 须 PrimeRoot 下互素 PrimeRoot^Exponent 下才有逆元
      Exit;

    Int64PolynomialGaloisModularInverse(T, F, G, PrimeRoot); // 求 PrimeRoot 模下的逆多项式

    N := 2;
    while N <= Exponent do
    begin
      // T := (p * T - X * T^2) in Ring(p^n, M)

      P := Int64NonNegativPower(PrimeRoot, N);

      Int64PolynomialGaloisMul(F, T, T, P);
      Int64PolynomialGaloisMul(F, F, X, P);

      Int64PolynomialGaloisMulWord(T, PrimeRoot, P);
      Int64PolynomialGaloisSub(T, T, F, P, Modulus);

      N := N + 1;
    end;

    // Result := T in Ring(p^e, M)
    P := Int64NonNegativPower(PrimeRoot, Exponent);
    Result := Int64PolynomialGaloisMod(Res, T, Modulus, P);
  finally
    FLocalInt64PolynomialPool.Recycle(T);
    FLocalInt64PolynomialPool.Recycle(G);
    FLocalInt64PolynomialPool.Recycle(F);
  end;
end;

function Int64PolynomialGaloisCompose(Res: TCnInt64Polynomial;
  F, P: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
var
  I: Integer;
  R, X, T: TCnInt64Polynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    Res[0] := Int64NonNegativeMod(F[0], Prime);
    Result := True;
    Exit;
  end;

  if (Res = F) or (Res = P) then
    R := FLocalInt64PolynomialPool.Obtain
  else
    R := Res;

  X := FLocalInt64PolynomialPool.Obtain;
  T := FLocalInt64PolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      Int64PolynomialCopy(T, X);
      Int64PolynomialGaloisMulWord(T, F[I], Prime);
      Int64PolynomialGaloisAdd(R, R, T, Prime);

      if I <> F.MaxDegree then
        Int64PolynomialGaloisMul(X, X, P, Prime);
    end;

    if Primitive <> nil then
      Int64PolynomialGaloisMod(R, R, Primitive, Prime);

    if (Res = F) or (Res = P) then
    begin
      Int64PolynomialCopy(Res, R);
      FLocalInt64PolynomialPool.Recycle(R);
    end;
  finally
    FLocalInt64PolynomialPool.Recycle(X);
    FLocalInt64PolynomialPool.Recycle(T);
  end;
  Result := True;
end;

function Int64PolynomialGaloisGetValue(F: TCnInt64Polynomial; X, Prime: Int64): Int64;
var
  I: Integer;
  T: Int64;
begin
  Result := Int64NonNegativeMod(F[0], Prime);
  if (X = 0) or (F.MaxDegree = 0) then    // 只有常数项的情况下，得常数项
    Exit;

  T := X;

  // 把 F 中的每个系数都和 X 的对应次幂相乘，最后相加
  for I := 1 to F.MaxDegree do
  begin
    Result := Int64NonNegativeMod(Result + Int64NonNegativeMulMod(F[I], T, Prime), Prime);
    if I <> F.MaxDegree then
      T := Int64NonNegativeMulMod(T, X, Prime);
  end;
  Result := Int64NonNegativeMod(Result, Prime);
end;

{
  可除多项式分两种，一种是含 x y 的 F，一种是只含 x 的 f，后者对于 y 点坐标需要额外乘个 y
  由于 Fn 在 n 为偶数时必然含有 y * 的项，所以可以规定 Fn = fn * y （n 为偶），fn = Fn （n 为奇）

  F0 = 0
  F1 = 1
  F2 = 2y
  F3 = 3x^4 + 6Ax^2 + 12Bx - A^2
  F4 = 4y * (x^6 + 5Ax^4 + 20Bx^3 - 5A^2x^2 - 4ABx - 8B^2 - A^3)
  F5 = 5x^12 + 62Ax^10 + 380Bx^9 + 105A^2x^8 + 240BAx^7 + (-300A^3 - 240B^2)x^6
    - 696BA^2x^5 + (-125A^4 - 1920B^2A)x^4 + (-80BA^3 - 1600B^3)x^3 + (-50A^5 - 240B^2A^2)x^2
    + (100BA^4 - 640B^3A)x + (A^6 - 32B^2A^3 - 256B4)
  ......

  一般：
    F2n+1 = Fn+2 * Fn^3 - Fn-1 * Fn+1^3
    F2n   = (Fn/2y) * (Fn+2 * Fn-1^2 - Fn-2 * Fn+1^2)       // 别看除了 2y，实际上必然有 * y 项

  对应的：

  f0 = 0
  f1 = 1
  f2 = 2
  f3 = 3x^4 + 6Ax^2 + 12Bx - A^2
  f4 = 4 * (x^6 + 5Ax^4 + 20Bx^3 - 5A^2x^2 - 4ABx - 8B^2 - A^3)
  f5 = 5x^12 + 62Ax^10 + 380Bx^9 + 105A^2x^8 + 240BAx^7 + (-300A^3 - 240B^2)x^6
    - 696BA^2x^5 + (-125A^4 - 1920B^2A)x^4 + (-80BA^3 - 1600B^3)x^3 + (-50A^5 - 240B^2A^2)x^2
    + (100BA^4 - 640B^3A)x + (A^6 - 32B^2A^3 - 256B4)
  ......

  一般：
    f2n = fn * (fn+2 * fn-1 ^ 2 - fn-2 * fn+1 ^ 2) / 2
    f2n+1 = fn+2 * fn^3 - fn-1 * fn+1^3 * (x^3 + Ax + B)^2     //  n为奇
          = (x^3 + Ax + B)^2 * fn+2 * fn^3 - fn-1 * fn+1^3     //  n为偶

}
function Int64PolynomialGaloisCalcDivisionPolynomial(A, B: Int64; Degree: Int64;
  OutDivisionPolynomial: TCnInt64Polynomial; Prime: Int64): Boolean;
var
  N: Integer;
  MI, T1, T2: Int64;
  D1, D2, D3, Y4: TCnInt64Polynomial;
begin
  if Degree < 0 then
    raise ECnPolynomialException.Create(SCnErrorPolynomialGaloisInvalidDegree)
  else if Degree = 0 then
  begin
    OutDivisionPolynomial.SetCoefficents([0]);  // f0(X) = 0
    Result := True;
  end
  else if Degree = 1 then
  begin
    OutDivisionPolynomial.SetCoefficents([1]);  // f1(X) = 1
    Result := True;
  end
  else if Degree = 2 then
  begin
    OutDivisionPolynomial.SetCoefficents([2]);  // f2(X) = 2
    Result := True;
  end
  else if Degree = 3 then   // f3(X) = 3 X4 + 6 a X2 + 12 b X - a^2
  begin
    OutDivisionPolynomial.MaxDegree := 4;
    OutDivisionPolynomial[4] := 3;
    OutDivisionPolynomial[3] := 0;
    OutDivisionPolynomial[2] := Int64NonNegativeMulMod(6, A, Prime);
    OutDivisionPolynomial[1] := Int64NonNegativeMulMod(12, B, Prime);
    OutDivisionPolynomial[0] := Int64NonNegativeMulMod(-A, A, Prime);

    Result := True;
  end
  else if Degree = 4 then // f4(X) = 4 X6 + 20 a X4 + 80 b X3 - 20 a2X2 - 16 a b X - 4 a3 - 32 b^2
  begin
    OutDivisionPolynomial.MaxDegree := 6;
    OutDivisionPolynomial[6] := 4;
    OutDivisionPolynomial[5] := 0;
    OutDivisionPolynomial[4] := Int64NonNegativeMulMod(20, A, Prime);
    OutDivisionPolynomial[3] := Int64NonNegativeMulMod(80, B, Prime);
    OutDivisionPolynomial[2] := Int64NonNegativeMulMod(Int64NonNegativeMulMod(-20, A, Prime), A, Prime);
    OutDivisionPolynomial[1] := Int64NonNegativeMulMod(Int64NonNegativeMulMod(-16, A, Prime), B, Prime);
    T1 := Int64NonNegativeMulMod(Int64NonNegativeMulMod(Int64NonNegativeMulMod(-4, A, Prime), A, Prime), A, Prime);
    T2 := Int64NonNegativeMulMod(Int64NonNegativeMulMod(-32, B, Prime), B, Prime);
    OutDivisionPolynomial[0] := Int64NonNegativeMod(T1 + T2, Prime); // TODO: 暂未处理相加溢出的取模

    Result := True;
  end
  else
  begin
    D1 := nil;
    D2 := nil;
    D3 := nil;
    Y4 := nil;

    try
      // 开始递归计算
      N := Degree shr 1;
      if (Degree and 1) = 0 then // Degree 是偶数，计算 fn * (fn+2 * fn-1 ^ 2 - fn-2 * fn+1 ^ 2) / 2
      begin
        D1 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 2, D1, Prime);

        D2 := FLocalInt64PolynomialPool.Obtain;        // D1 得到 fn+2
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);
        Int64PolynomialGaloisMul(D2, D2, D2, Prime);   // D2 得到 fn-1 ^2

        Int64PolynomialGaloisMul(D1, D1, D2, Prime);   // D1 得到 fn+2 * fn-1 ^ 2

        D3 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 2, D3, Prime);  // D3 得到 fn-2

        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 1, D2, Prime);
        Int64PolynomialGaloisMul(D2, D2, D2, Prime);   // D2 得到 fn+1^2
        Int64PolynomialGaloisMul(D2, D2, D3, Prime);   // D2 得到 fn-2 * fn+1^2

        Int64PolynomialGaloisSub(D1, D1, D2, Prime);   // D1 得到 fn+2 * fn-1^2 - fn-2 * fn+1^2

        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N, D2, Prime);    // D2 得到 fn
        Int64PolynomialGaloisMul(OutDivisionPolynomial, D2, D1, Prime);     // 相乘得到 f2n
        MI := CnInt64ModularInverse(2, Prime);
        Int64PolynomialGaloisMulWord(OutDivisionPolynomial, MI, Prime);     // 再除以 2
      end
      else // Degree 是奇数
      begin
        Y4 := FLocalInt64PolynomialPool.Obtain;
        Y4.SetCoefficents([B, A, 0, 1]);
        Int64PolynomialGaloisMul(Y4, Y4, Y4, Prime);

        D1 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 2, D1, Prime); // D1 得到 fn+2

        D2 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N, D2, Prime);
        Int64PolynomialGaloisPower(D2, D2, 3, Prime);                        // D2 得到 fn^3

        D3 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 1, D3, Prime);
        Int64PolynomialGaloisPower(D3, D3, 3, Prime);                        // D3 得到 fn+1^3

        if (N and 1) <> 0 then // N 是奇数，计算 f2n+1 = fn+2 * fn^3 - fn-1 * fn+1^3 * (x^3 + Ax + B)^2
        begin
          Int64PolynomialGaloisMul(D1, D1, D2, Prime);  // D1 得到 fn+2 * fn^3

          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);
          Int64PolynomialGaloisMul(D2, D2, Y4, Prime);  // D2 得到 fn-1 * Y^4

          Int64PolynomialGaloisMul(D2, D2, D3, Prime);  // D2 得到 fn+1^3 * fn-1 * Y^4
          Int64PolynomialGaloisSub(OutDivisionPolynomial, D1, D2, Prime);
        end
        else // N 是偶数，计算 (x^3 + Ax + B)^2 * fn+2 * fn^3 - fn-1 * fn+1^3
        begin
          Int64PolynomialGaloisMul(D1, D1, D2, Prime);
          Int64PolynomialGaloisMul(D1, D1, Y4, Prime);  // D1 得到 Y^4 * fn+2 * fn^3

          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);  // D2 得到 fn-1

          Int64PolynomialGaloisMul(D2, D2, D3, Prime);  // D2 得到 fn-1 * fn+1^3

          Int64PolynomialGaloisSub(OutDivisionPolynomial, D1, D2, Prime);
        end;
      end;
    finally
      FLocalInt64PolynomialPool.Recycle(D1);
      FLocalInt64PolynomialPool.Recycle(D2);
      FLocalInt64PolynomialPool.Recycle(D3);
      FLocalInt64PolynomialPool.Recycle(Y4);
    end;
    Result := True;
  end;
end;

procedure Int64PolynomialGaloisReduce2(P1, P2: TCnInt64Polynomial; Prime: Int64);
var
  D: TCnInt64Polynomial;
begin
  if P1 = P2 then
  begin
    P1.SetOne;
    Exit;
  end;

  D := FLocalInt64PolynomialPool.Obtain;
  try
    if not Int64PolynomialGaloisGreatestCommonDivisor(D, P1, P2, Prime) then
      Exit;

    if not D.IsOne then
    begin
      Int64PolynomialGaloisDiv(P1, nil, P1, D, Prime);
      Int64PolynomialGaloisDiv(P1, nil, P1, D, Prime);
    end;
  finally
    FLocalInt64PolynomialPool.Recycle(D);
  end;
end;

{ TCnInt64PolynomialPool }

function TCnInt64PolynomialPool.CreateObject: TObject;
begin
  Result := TCnInt64Polynomial.Create;
end;

function TCnInt64PolynomialPool.Obtain: TCnInt64Polynomial;
begin
  Result := TCnInt64Polynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnInt64PolynomialPool.Recycle(Poly: TCnInt64Polynomial);
begin
  inherited Recycle(Poly);
end;

{ TCnInt64RationalPolynomial }

procedure TCnInt64RationalPolynomial.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnInt64RationalPolynomial then
  begin
    Int64PolynomialCopy(TCnInt64RationalPolynomial(Dest).Numerator, FNumerator);
    Int64PolynomialCopy(TCnInt64RationalPolynomial(Dest).Denominator, FDenominator);
  end
  else
    inherited;
end;

constructor TCnInt64RationalPolynomial.Create;
begin
  inherited;
  FNumerator := TCnInt64Polynomial.Create([0]);
  FDenominator := TCnInt64Polynomial.Create([1]);
end;

destructor TCnInt64RationalPolynomial.Destroy;
begin
  FDenominator.Free;
  FNumerator.Free;
  inherited;
end;

function TCnInt64RationalPolynomial.IsInt: Boolean;
begin
  Result := FDenominator.IsOne or FDenominator.IsNegOne;
end;

function TCnInt64RationalPolynomial.IsOne: Boolean;
begin
  Result := not FNumerator.IsZero and Int64PolynomialEqual(FNumerator, FDenominator);
end;

function TCnInt64RationalPolynomial.IsZero: Boolean;
begin
  Result := not FDenominator.IsZero and FNumerator.IsZero;
end;

procedure TCnInt64RationalPolynomial.Neg;
begin
  FNumerator.Negate;
end;

procedure TCnInt64RationalPolynomial.Reciprocal;
var
  T: TCnInt64Polynomial;
begin
  if FNumerator.IsZero then
    raise EDivByZero.Create(SDivByZero);

  T := FLocalInt64PolynomialPool.Obtain;
  try
    Int64PolynomialCopy(T, FDenominator);
    Int64PolynomialCopy(FDenominator, FNumerator);
    Int64PolynomialCopy(FNumerator, T);
  finally
    FLocalInt64PolynomialPool.Recycle(T);
  end;
end;

procedure TCnInt64RationalPolynomial.Reduce;
begin
  Int64PolynomialReduce2(FNumerator, FDenominator);
end;

procedure TCnInt64RationalPolynomial.SetOne;
begin
  FDenominator.SetOne;
  FNumerator.SetOne;
end;

procedure TCnInt64RationalPolynomial.SetString(const Rational: string);
var
  P: Integer;
  N, D: string;
begin
  P := Pos('/', Rational);
  if P > 1 then
  begin
    N := Copy(Rational, 1, P - 1);
    D := Copy(Rational, P + 1, MaxInt);

    FNumerator.SetString(Trim(N));
    FDenominator.SetString(Trim(D));
  end
  else
  begin
    FNumerator.SetString(Rational);
    FDenominator.SetOne;
  end;
end;

procedure TCnInt64RationalPolynomial.SetZero;
begin
  FDenominator.SetOne;
  FNumerator.SetZero;
end;

function TCnInt64RationalPolynomial.ToString: string;
begin
  if FDenominator.IsOne then
    Result := FNumerator.ToString
  else if FNumerator.IsZero then
    Result := '0'
  else
    Result := FNumerator.ToString + ' / ' + FDenominator.ToString;
end;

// ============================= 有理分式运算 ==================================

function Int64RationalPolynomialEqual(R1, R2: TCnInt64RationalPolynomial): Boolean;
var
  T1, T2: TCnInt64Polynomial;
begin
  if R1 = R2 then
  begin
    Result := True;
    Exit;
  end;

  if R1.IsInt and R2.IsInt then
  begin
    Result := Int64PolynomialEqual(R1.Numerator, R2.Numerator);
    Exit;
  end;

  T1 := FLocalInt64PolynomialPool.Obtain;
  T2 := FLocalInt64PolynomialPool.Obtain;

  try
    // 判断分子分母互相乘的结果是否相等
    Int64PolynomialMul(T1, R1.Numerator, R2.Denominator);
    Int64PolynomialMul(T2, R2.Numerator, R1.Denominator);
    Result := Int64PolynomialEqual(T1, T2);
  finally
    FLocalInt64PolynomialPool.Recycle(T2);
    FLocalInt64PolynomialPool.Recycle(T1);
  end;
end;

function Int64RationalPolynomialCopy(Dest: TCnInt64RationalPolynomial;
  Source: TCnInt64RationalPolynomial): TCnInt64RationalPolynomial;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    Int64PolynomialCopy(Dest.Numerator, Source.Numerator);
    Int64PolynomialCopy(Dest.Denominator, Source.Denominator);
  end;
end;

procedure Int64RationalPolynomialAdd(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial);
var
  M, R, F1, F2, D1, D2: TCnInt64Polynomial;
begin
  if R1.IsInt and R2.IsInt then
  begin
    Int64PolynomialAdd(Res.Numerator, R1.Numerator, R2.Numerator);
    Res.Denominator.SetOne;
    Exit;
  end
  else if R1.IsZero then
  begin
    if R2 <> Res then
      Res.Assign(R2);
  end
  else if R2.IsZero then
  begin
    if R1 <> Res then
      Res.Assign(R1);
  end
  else
  begin
    M := nil;
    R := nil;
    F1 := nil;
    F2 := nil;
    D1 := nil;
    D2 := nil;

    try
      // 求分母的最小公倍数
      M := FLocalInt64PolynomialPool.Obtain;
      R := FLocalInt64PolynomialPool.Obtain;
      F1 := FLocalInt64PolynomialPool.Obtain;
      F2 := FLocalInt64PolynomialPool.Obtain;
      D1 := FLocalInt64PolynomialPool.Obtain;
      D2 := FLocalInt64PolynomialPool.Obtain;

      Int64PolynomialCopy(D1, R1.Denominator);
      Int64PolynomialCopy(D2, R2.Denominator);

      if not Int64PolynomialLeastCommonMultiple(M, D1, D2) then
        Int64PolynomialMul(M, D1, D2);   // 无法求最小公倍式表示系数无法整除，直接相乘

      Int64PolynomialDiv(F1, R, M, D1);
      Int64PolynomialDiv(F2, R, M, D2);

      Int64PolynomialCopy(Res.Denominator, M);
      Int64PolynomialMul(R, R1.Numerator, F1);
      Int64PolynomialMul(M, R2.Numerator, F2);
      Int64PolynomialAdd(Res.Numerator, R, M);
    finally
      FLocalInt64PolynomialPool.Recycle(M);
      FLocalInt64PolynomialPool.Recycle(R);
      FLocalInt64PolynomialPool.Recycle(F1);
      FLocalInt64PolynomialPool.Recycle(F2);
      FLocalInt64PolynomialPool.Recycle(D1);
      FLocalInt64PolynomialPool.Recycle(D2);
    end;
  end;
end;

procedure Int64RationalPolynomialSub(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial);
begin
  R2.Numerator.Negate;
  Int64RationalPolynomialAdd(Res, R1, R2);
  if Res <> R2 then
    R2.Numerator.Negate;
end;

procedure Int64RationalPolynomialMul(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial);
begin
  Int64PolynomialMul(Res.Numerator, R1.Numerator, R2.Numerator);
  Int64PolynomialMul(Res.Denominator, R1.Denominator, R2.Denominator);
end;

procedure Int64RationalPolynomialDiv(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial);
var
  N: TCnInt64Polynomial;
begin
  if R2.IsZero then
    raise EDivByZero.Create(SDivByZero);

  N := FLocalInt64PolynomialPool.Obtain; // 交叉相乘，必须用中间变量，防止 Res 是 Number1 或 Number 2
  try
    Int64PolynomialMul(N, R1.Numerator, R2.Denominator);
    Int64PolynomialMul(Res.Denominator, R1.Denominator, R2.Numerator);
    Int64PolynomialCopy(Res.Numerator, N);
  finally
    FLocalInt64PolynomialPool.Recycle(N);
  end;
end;

procedure Int64RationalPolynomialAddWord(R: TCnInt64RationalPolynomial; N: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialAdd(R, R, P);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialSubWord(R: TCnInt64RationalPolynomial; N: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialSub(R, R, P);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialMulWord(R: TCnInt64RationalPolynomial; N: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialMul(R, R, P);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialDivWord(R: TCnInt64RationalPolynomial; N: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialDiv(R, R, P);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialAdd(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial);
var
  T: TCnInt64RationalPolynomial;
begin
  if P1.IsZero then
  begin
    if R1 <> Res then
    begin
      Int64RationalPolynomialCopy(Res, R1);
      Exit;
    end;
  end;

  T := FLocalInt64RationalPolynomialPool.Obtain;
  try
    T.Denominator.SetOne;
    Int64PolynomialCopy(T.Numerator, P1);
    Int64RationalPolynomialAdd(Res, R1, T);
  finally
    FLocalInt64RationalPolynomialPool.Recycle(T);
  end;
end;

procedure Int64RationalPolynomialSub(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial);
begin
  P1.Negate;
  try
    Int64RationalPolynomialAdd(Res, R1, P1);
  finally
    P1.Negate;
  end;
end;

procedure Int64RationalPolynomialMul(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial);
begin
  if P1.IsZero then
    Res.SetZero
  else if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    Int64PolynomialMul(Res.Numerator, R1.Numerator, P1);
    Int64PolynomialCopy(Res.Denominator, R1.Denominator);
  end;
end;

procedure Int64RationalPolynomialDiv(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial);
begin
  if P1.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    Int64PolynomialMul(Res.Denominator, R1.Denominator, P1);
    Int64PolynomialCopy(Res.Numerator, R1.Numerator);
  end;
end;

function Int64RationalPolynomialCompose(Res: TCnInt64RationalPolynomial;
  F, P: TCnInt64RationalPolynomial): Boolean;
var
  RN, RD: TCnInt64RationalPolynomial;
begin
  if P.IsInt then
    Result := Int64RationalPolynomialCompose(Res, F, P.Numerator)
  else
  begin
    RD := FLocalInt64RationalPolynomialPool.Obtain;
    RN := FLocalInt64RationalPolynomialPool.Obtain;

    try
      Int64RationalPolynomialCompose(RN, F.Numerator, P);
      Int64RationalPolynomialCompose(RD, F.Denominator, P);

      Int64PolynomialMul(Res.Numerator, RN.Numerator, RD.Denominator);
      Int64PolynomialMul(Res.Denominator, RN.Denominator, RD.Numerator);
      Result := True;
    finally
      FLocalInt64RationalPolynomialPool.Recycle(RN);
      FLocalInt64RationalPolynomialPool.Recycle(RD);
    end;
  end;
end;

function Int64RationalPolynomialCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64RationalPolynomial; P: TCnInt64Polynomial): Boolean;
begin
  Int64PolynomialCompose(Res.Numerator, F.Numerator, P);
  Int64PolynomialCompose(Res.Denominator, F.Denominator, P);
  Result := True;
end;

function Int64RationalPolynomialCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64Polynomial; P: TCnInt64RationalPolynomial): Boolean;
var
  I: Integer;
  R, X, T: TCnInt64RationalPolynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    Res.Numerator[0] := F[0];
    Result := True;
    Exit;
  end;

  if Res = P then
    R := FLocalInt64RationalPolynomialPool.Obtain
  else
    R := Res;

  X := FLocalInt64RationalPolynomialPool.Obtain;
  T := FLocalInt64RationalPolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      Int64RationalPolynomialCopy(T, X);
      Int64RationalPolynomialMulWord(T, F[I]);
      Int64RationalPolynomialAdd(R, T, R);

      if I <> F.MaxDegree then
        Int64RationalPolynomialMul(X, P, X);
    end;

    if Res = P then
    begin
      Int64RationalPolynomialCopy(Res, R);
      FLocalInt64RationalPolynomialPool.Recycle(R);
    end;
  finally
    FLocalInt64RationalPolynomialPool.Recycle(X);
    FLocalInt64RationalPolynomialPool.Recycle(T);
  end;
  Result := True;
end;

procedure Int64RationalPolynomialGetValue(Res: TCnRationalNumber;
  F: TCnInt64RationalPolynomial; X: Int64);
begin
  Res.Numerator := Int64PolynomialGetValue(F.Numerator, X);
  Res.Denominator := Int64PolynomialGetValue(F.Denominator, X);
  Res.Reduce;
end;

// ====================== 有理分式在有限域上的模运算 ===========================

function Int64RationalPolynomialGaloisEqual(R1, R2: TCnInt64RationalPolynomial;
  Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
var
  T1, T2: TCnInt64Polynomial;
begin
  if R1 = R2 then
  begin
    Result := True;
    Exit;
  end;

  T1 := FLocalInt64PolynomialPool.Obtain;
  T2 := FLocalInt64PolynomialPool.Obtain;

  try
    // 判断分子分母互相乘的结果是否相等
    Int64PolynomialGaloisMul(T1, R1.Numerator, R2.Denominator, Prime, Primitive);
    Int64PolynomialGaloisMul(T2, R2.Numerator, R1.Denominator, Prime, Primitive);
    Result := Int64PolynomialGaloisEqual(T1, T2, Prime);
  finally
    FLocalInt64PolynomialPool.Recycle(T2);
    FLocalInt64PolynomialPool.Recycle(T1);
  end;
end;

procedure Int64RationalPolynomialGaloisNegate(P: TCnInt64RationalPolynomial;
  Prime: Int64);
begin
  Int64PolynomialGaloisNegate(P.Numerator, Prime);
end;

procedure Int64RationalPolynomialGaloisAdd(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial; Prime: Int64);
var
  M, R, F1, F2, D1, D2: TCnInt64Polynomial;
begin
  if R1.IsInt and R2.IsInt then
  begin
    Int64PolynomialGaloisAdd(Res.Numerator, R1.Numerator,
      R2.Numerator, Prime);
    Res.Denominator.SetOne;
    Exit;
  end
  else if R1.IsZero then
  begin
    if R2 <> Res then
      Res.Assign(R2);
  end
  else if R2.IsZero then
  begin
    if R1 <> Res then
      Res.Assign(R1);
  end
  else
  begin
    M := nil;
    R := nil;
    F1 := nil;
    F2 := nil;
    D1 := nil;
    D2 := nil;

    try
      // 求分母的最小公倍数
      M := FLocalInt64PolynomialPool.Obtain;
      R := FLocalInt64PolynomialPool.Obtain;
      F1 := FLocalInt64PolynomialPool.Obtain;
      F2 := FLocalInt64PolynomialPool.Obtain;
      D1 := FLocalInt64PolynomialPool.Obtain;
      D2 := FLocalInt64PolynomialPool.Obtain;

      Int64PolynomialCopy(D1, R1.Denominator);
      Int64PolynomialCopy(D2, R2.Denominator);

      if not Int64PolynomialGaloisLeastCommonMultiple(M, D1, D2, Prime) then
        Int64PolynomialGaloisMul(M, D1, D2, Prime);   // 无法求最小公倍式表示系数无法整除，直接相乘

      Int64PolynomialGaloisDiv(F1, R, M, D1, Prime);  // 最小公倍数 M div D1 结果放 F1
      Int64PolynomialGaloisDiv(F2, R, M, D2, Prime);  // 最小公倍数 M div D2 结果放 F2

      Int64PolynomialCopy(Res.Denominator, M);  // 结果的分母是最小公倍数
      Int64PolynomialGaloisMul(R, R1.Numerator, F1, Prime);
      Int64PolynomialGaloisMul(M, R2.Numerator, F2, Prime);
      Int64PolynomialGaloisAdd(Res.Numerator, R, M, Prime);
    finally
      FLocalInt64PolynomialPool.Recycle(M);
      FLocalInt64PolynomialPool.Recycle(R);
      FLocalInt64PolynomialPool.Recycle(F1);
      FLocalInt64PolynomialPool.Recycle(F2);
      FLocalInt64PolynomialPool.Recycle(D1);
      FLocalInt64PolynomialPool.Recycle(D2);
    end;
  end;
end;

procedure Int64RationalPolynomialGaloisSub(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial; Prime: Int64);
begin
  R2.Numerator.Negate;
  Int64RationalPolynomialGaloisAdd(Res, R1, R2, Prime);
  if Res <> R2 then
    R2.Numerator.Negate;
end;

procedure Int64RationalPolynomialGaloisMul(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial; Prime: Int64);
begin
  Int64PolynomialGaloisMul(Res.Numerator, R1.Numerator, R2.Numerator, Prime);
  Int64PolynomialGaloisMul(Res.Denominator, R1.Denominator, R2.Denominator, Prime);
end;

procedure Int64RationalPolynomialGaloisDiv(Res: TCnInt64RationalPolynomial;
  R1, R2: TCnInt64RationalPolynomial; Prime: Int64);
var
  N: TCnInt64Polynomial;
begin
  if R2.IsZero then
    raise EDivByZero.Create(SDivByZero);

  N := FLocalInt64PolynomialPool.Obtain; // 交叉相乘，必须用中间变量，防止 Res 是 Number1 或 Number 2
  try
    Int64PolynomialGaloisMul(N, R1.Numerator, R2.Denominator, Prime);
    Int64PolynomialGaloisMul(Res.Denominator, R1.Denominator, R2.Numerator, Prime);
    Int64PolynomialCopy(Res.Numerator, N);
  finally
    FLocalInt64PolynomialPool.Recycle(N);
  end;
end;

procedure Int64RationalPolynomialGaloisAddWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialGaloisAdd(R, R, P, Prime);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialGaloisSubWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialGaloisSub(R, R, P, Prime);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialGaloisMulWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialGaloisMul(R, R, P, Prime);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialGaloisDivWord(R: TCnInt64RationalPolynomial;
  N: Int64; Prime: Int64);
var
  P: TCnInt64Polynomial;
begin
  P := FLocalInt64PolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    P[0] := N;
    Int64RationalPolynomialGaloisDiv(R, R, P, Prime);
  finally
    FLocalInt64PolynomialPool.Recycle(P);
  end;
end;

procedure Int64RationalPolynomialGaloisAdd(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64);
var
  T: TCnInt64RationalPolynomial;
begin
  if P1.IsZero then
  begin
    if R1 <> Res then
    begin
      Int64RationalPolynomialCopy(Res, R1);
      Exit;
    end;
  end;

  T := FLocalInt64RationalPolynomialPool.Obtain;
  try
    T.Denominator.SetOne;
    Int64PolynomialCopy(T.Numerator, P1);
    Int64RationalPolynomialGaloisAdd(Res, R1, T, Prime);
  finally
    FLocalInt64RationalPolynomialPool.Recycle(T);
  end;
end;

procedure Int64RationalPolynomialGaloisSub(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64);
begin
  P1.Negate;
  try
    Int64RationalPolynomialGaloisAdd(Res, R1, P1, Prime);
  finally
    P1.Negate;
  end;
end;

procedure Int64RationalPolynomialGaloisMul(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64);
begin
  if P1.IsZero then
    Res.SetZero
  else if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    Int64PolynomialGaloisMul(Res.Numerator, R1.Numerator, P1, Prime);
    Int64PolynomialCopy(Res.Denominator, R1.Denominator);
  end;
end;

procedure Int64RationalPolynomialGaloisDiv(Res: TCnInt64RationalPolynomial;
  R1: TCnInt64RationalPolynomial; P1: TCnInt64Polynomial; Prime: Int64);
begin
  if P1.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    Int64PolynomialGaloisMul(Res.Denominator, R1.Denominator, P1, Prime);
    Int64PolynomialCopy(Res.Numerator, R1.Numerator);
  end;
end;

function Int64RationalPolynomialGaloisCompose(Res: TCnInt64RationalPolynomial;
  F, P: TCnInt64RationalPolynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
var
  RN, RD: TCnInt64RationalPolynomial;
begin
  if P.IsInt then
    Result := Int64RationalPolynomialGaloisCompose(Res, F, P.Numerator, Prime, Primitive)
  else
  begin
    RD := FLocalInt64RationalPolynomialPool.Obtain;
    RN := FLocalInt64RationalPolynomialPool.Obtain;

    try
      Int64RationalPolynomialGaloisCompose(RN, F.Numerator, P, Prime, Primitive);
      Int64RationalPolynomialGaloisCompose(RD, F.Denominator, P, Prime, Primitive);

      Int64PolynomialGaloisMul(Res.Numerator, RN.Numerator, RD.Denominator, Prime);
      Int64PolynomialGaloisMul(Res.Denominator, RN.Denominator, RD.Numerator, Prime);

      if Primitive <> nil then
      begin
        Int64PolynomialGaloisMod(Res.Numerator, Res.Numerator, Primitive, Prime);
        Int64PolynomialGaloisMod(Res.Denominator, Res.Denominator, Primitive, Prime);
      end;
      Result := True;
    finally
      FLocalInt64RationalPolynomialPool.Recycle(RN);
      FLocalInt64RationalPolynomialPool.Recycle(RD);
    end;
  end;
end;

function Int64RationalPolynomialGaloisCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64RationalPolynomial; P: TCnInt64Polynomial; Prime: Int64;
  Primitive: TCnInt64Polynomial): Boolean;
begin
  Int64PolynomialGaloisCompose(Res.Numerator, F.Numerator, P, Prime, Primitive);
  Int64PolynomialGaloisCompose(Res.Denominator, F.Denominator, P, Prime, Primitive);
  Result := True;
end;

function Int64RationalPolynomialGaloisCompose(Res: TCnInt64RationalPolynomial;
  F: TCnInt64Polynomial; P: TCnInt64RationalPolynomial; Prime: Int64;
  Primitive: TCnInt64Polynomial): Boolean;
var
  I: Integer;
  R, X, T: TCnInt64RationalPolynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    Res.Numerator[0] := Int64NonNegativeMod(F[0], Prime);
    Result := True;
    Exit;
  end;

  if Res = P then
    R := FLocalInt64RationalPolynomialPool.Obtain
  else
    R := Res;

  X := FLocalInt64RationalPolynomialPool.Obtain;
  T := FLocalInt64RationalPolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      Int64RationalPolynomialCopy(T, X);
      Int64RationalPolynomialGaloisMulWord(T, F[I], Prime);
      Int64RationalPolynomialGaloisAdd(R, T, R, Prime);

      if I <> F.MaxDegree then
        Int64RationalPolynomialGaloisMul(X, P, X, Prime);
    end;

    if Primitive <> nil then
    begin
      Int64PolynomialGaloisMod(R.Numerator, R.Numerator, Primitive, Prime);
      Int64PolynomialGaloisMod(R.Denominator, R.Denominator, Primitive, Prime);
    end;

    if Res = P then
    begin
      Int64RationalPolynomialCopy(Res, R);
      FLocalInt64RationalPolynomialPool.Recycle(R);
    end;
  finally
    FLocalInt64RationalPolynomialPool.Recycle(X);
    FLocalInt64RationalPolynomialPool.Recycle(T);
  end;
  Result := True;
end;

function Int64RationalPolynomialGaloisGetValue(F: TCnInt64RationalPolynomial;
  X: Int64; Prime: Int64): Int64;
var
  N, D: Int64;
begin
  D := Int64PolynomialGaloisGetValue(F.Denominator, X, Prime);
  if D = 0 then
    raise EDivByZero.Create(SDivByZero);

  N := Int64PolynomialGaloisGetValue(F.Numerator, X, Prime);
  Result := Int64NonNegativeMulMod(N, CnInt64ModularInverse2(D, Prime), Prime);
end;

{ TCnBigNumberPolynomial }

procedure TCnBigNumberPolynomial.CorrectTop;
begin
  while (MaxDegree > 0) and Items[MaxDegree].IsZero do
    Delete(MaxDegree);
end;

constructor TCnBigNumberPolynomial.Create;
begin
  inherited Create;
  Add.SetZero;   // 常系数项
end;

constructor TCnBigNumberPolynomial.Create(LowToHighCoefficients: array of const);
begin
  inherited Create;
  SetCoefficents(LowToHighCoefficients);
end;

destructor TCnBigNumberPolynomial.Destroy;
begin

  inherited;
end;

procedure TCnBigNumberPolynomial.EnsureDegree(Degree: Integer);
begin
  if Degree > MaxDegree then
    SetMaxDegree(Degree);
end;

function TCnBigNumberPolynomial.GetMaxDegree: Integer;
begin
  if Count = 0 then
    Add.SetZero;
  Result := Count - 1;
end;

function TCnBigNumberPolynomial.IsMonic: Boolean;
begin
  Result := BigNumberPolynomialIsMonic(Self);
end;

function TCnBigNumberPolynomial.IsNegOne: Boolean;
begin
  Result := BigNumberPolynomialIsNegOne(Self);
end;

function TCnBigNumberPolynomial.IsOne: Boolean;
begin
  Result := BigNumberPolynomialIsOne(Self);
end;

function TCnBigNumberPolynomial.IsZero: Boolean;
begin
  Result := BigNumberPolynomialIsZero(Self);
end;

procedure TCnBigNumberPolynomial.Negate;
begin
  BigNumberPolynomialNegate(Self);
end;

procedure TCnBigNumberPolynomial.SetCoefficent(Degree: Integer;
  Coefficient: TCnBigNumber);
begin
  CheckDegree(Degree);
  EnsureDegree(Degree);
  BigNumberCopy(Items[Degree], Coefficient);
end;

procedure TCnBigNumberPolynomial.SetCoefficents(LowToHighCoefficients: array of const);
var
  I: Integer;
begin
  Clear;
  for I := Low(LowToHighCoefficients) to High(LowToHighCoefficients) do
  begin
    case LowToHighCoefficients[I].VType of
    vtInteger:
      begin
        Add.SetInteger(LowToHighCoefficients[I].VInteger);
      end;
    vtInt64:
      begin
        Add.SetInt64(LowToHighCoefficients[I].VInt64^);
      end;
    vtBoolean:
      begin
        if LowToHighCoefficients[I].VBoolean then
          Add.SetOne
        else
          Add.SetZero;
      end;
    vtString:
      begin
        Add.SetDec(LowToHighCoefficients[I].VString^);
      end;
    vtObject:
      begin
        // 接受 TCnBigNumber 并从中复制值
        if LowToHighCoefficients[I].VObject is TCnBigNumber then
          BigNumberCopy(Add, LowToHighCoefficients[I].VObject as TCnBigNumber);
      end;
    else
      raise ECnPolynomialException.CreateFmt(SInvalidInteger, ['Coefficients ' + IntToStr(I)]);
    end;
  end;

  if Count = 0 then
    Add.SetZero
  else
    CorrectTop;
end;

procedure TCnBigNumberPolynomial.SetMaxDegree(const Value: Integer);
var
  I, OC: Integer;
begin
  CheckDegree(Value);

  OC := Count;
  Count := Value + 1; // 直接设置 Count，如变小，会自动释放多余的对象

  if Count > OC then  // 增加的部分创建新对象
  begin
    for I := OC to Count - 1 do
      Items[I] := TCnBigNumber.Create;
  end;
end;

procedure TCnBigNumberPolynomial.SetOne;
begin
  BigNumberPolynomialSetOne(Self);
end;

procedure TCnBigNumberPolynomial.SetString(const Poly: string);
begin
  BigNumberPolynomialSetString(Self, Poly);
end;

procedure TCnBigNumberPolynomial.SetZero;
begin
  BigNumberPolynomialSetZero(Self);
end;

function TCnBigNumberPolynomial.ToString: string;
begin
  Result := BigNumberPolynomialToString(Self);
end;

{ TCnBigNumberRationalPolynomial }

procedure TCnBigNumberRationalPolynomial.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnBigNumberRationalPolynomial then
  begin
    BigNumberPolynomialCopy(TCnBigNumberRationalPolynomial(Dest).Numerator, FNumerator);
    BigNumberPolynomialCopy(TCnBigNumberRationalPolynomial(Dest).Denominator, FDenominator);
  end
  else
    inherited;
end;

constructor TCnBigNumberRationalPolynomial.Create;
begin
  inherited;
  FNumerator := TCnBigNumberPolynomial.Create([0]);
  FDenominator := TCnBigNumberPolynomial.Create([1]);
end;

destructor TCnBigNumberRationalPolynomial.Destroy;
begin
  FDenominator.Free;
  FNumerator.Free;
  inherited;
end;

function TCnBigNumberRationalPolynomial.IsInt: Boolean;
begin
  Result := FDenominator.IsOne or FDenominator.IsNegOne;
end;

function TCnBigNumberRationalPolynomial.IsOne: Boolean;
begin
  Result := not FNumerator.IsZero and BigNumberPolynomialEqual(FNumerator, FDenominator);
end;

function TCnBigNumberRationalPolynomial.IsZero: Boolean;
begin
  Result := not FDenominator.IsZero and FNumerator.IsZero;
end;

procedure TCnBigNumberRationalPolynomial.Neg;
begin
  FNumerator.Negate;
end;

procedure TCnBigNumberRationalPolynomial.Reciprocal;
var
  T: TCnBigNumberPolynomial;
begin
  if FNumerator.IsZero then
    raise EDivByZero.Create(SDivByZero);

  T := FLocalBigNumberPolynomialPool.Obtain;
  try
    BigNumberPolynomialCopy(T, FDenominator);
    BigNumberPolynomialCopy(FDenominator, FNumerator);
    BigNumberPolynomialCopy(FNumerator, T);
  finally
    FLocalBigNumberPolynomialPool.Recycle(T);
  end;
end;

procedure TCnBigNumberRationalPolynomial.Reduce;
begin
  BigNumberPolynomialReduce2(FNumerator, FDenominator);
end;

procedure TCnBigNumberRationalPolynomial.SetOne;
begin
  FDenominator.SetOne;
  FNumerator.SetOne;
end;

procedure TCnBigNumberRationalPolynomial.SetString(const Rational: string);
var
  P: Integer;
  N, D: string;
begin
  P := Pos('/', Rational);
  if P > 1 then
  begin
    N := Copy(Rational, 1, P - 1);
    D := Copy(Rational, P + 1, MaxInt);

    FNumerator.SetString(Trim(N));
    FDenominator.SetString(Trim(D));
  end
  else
  begin
    FNumerator.SetString(Rational);
    FDenominator.SetOne;
  end;
end;

procedure TCnBigNumberRationalPolynomial.SetZero;
begin
  FDenominator.SetOne;
  FNumerator.SetZero;
end;

function TCnBigNumberRationalPolynomial.ToString: string;
begin
  if FDenominator.IsOne then
    Result := FNumerator.ToString
  else if FNumerator.IsZero then
    Result := '0'
  else
    Result := FNumerator.ToString + ' / ' + FDenominator.ToString;
end;

{ TCnBigNumberPolynomialPool }

function TCnBigNumberPolynomialPool.CreateObject: TObject;
begin
  Result := TCnBigNumberPolynomial.Create;
end;

function TCnBigNumberPolynomialPool.Obtain: TCnBigNumberPolynomial;
begin
  Result := TCnBigNumberPolynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigNumberPolynomialPool.Recycle(Poly: TCnBigNumberPolynomial);
begin
  inherited Recycle(Poly);
end;

{ TCnInt64RationalPolynomialPool }

function TCnInt64RationalPolynomialPool.CreateObject: TObject;
begin
  Result := TCnInt64RationalPolynomial.Create;
end;

function TCnInt64RationalPolynomialPool.Obtain: TCnInt64RationalPolynomial;
begin
  Result := TCnInt64RationalPolynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnInt64RationalPolynomialPool.Recycle(Poly: TCnInt64RationalPolynomial);
begin
  inherited Recycle(Poly);
end;

function BigNumberPolynomialNew: TCnBigNumberPolynomial;
begin
  Result := TCnBigNumberPolynomial.Create;
end;

procedure BigNumberPolynomialFree(P: TCnBigNumberPolynomial);
begin
  P.Free;
end;

function BigNumberPolynomialDuplicate(P: TCnBigNumberPolynomial): TCnBigNumberPolynomial;
begin
  if P = nil then
  begin
    Result := nil;
    Exit;
  end;

  Result := BigNumberPolynomialNew;
  if Result <> nil then
    BigNumberPolynomialCopy(Result, P);
end;

function BigNumberPolynomialCopy(Dest: TCnBigNumberPolynomial;
  Source: TCnBigNumberPolynomial): TCnBigNumberPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    Dest.MaxDegree := Source.MaxDegree;
    for I := 0 to Source.Count - 1 do
      BigNumberCopy(Dest[I], Source[I]);
    Dest.CorrectTop;
  end;
end;

procedure BigNumberPolynomialSwap(A: TCnBigNumberPolynomial; B: TCnBigNumberPolynomial);
var
  T: TCnBigNumberPolynomial;
begin
  if (A = nil) or (B = nil) then
    Exit;

  T := FLocalBigNumberPolynomialPool.Obtain;
  try
    BigNumberPolynomialCopy(T, A);
    BigNumberPolynomialCopy(A, B);
    BigNumberPolynomialCopy(B, T);
  finally
    FLocalBigNumberPolynomialPool.Recycle(T);
  end;
end;

function BigNumberPolynomialToString(P: TCnBigNumberPolynomial;
  const VarName: string): string;
var
  I: Integer;
begin
  Result := '';
  if BigNumberPolynomialIsZero(P) then
  begin
    Result := '0';
    Exit;
  end;

  for I := P.MaxDegree downto 0 do
  begin
    if VarItemFactor(Result, (I = 0), P[I].ToDec) then
      Result := Result + VarPower(VarName, I);
  end;
end;

{$WARNINGS OFF}

function BigNumberPolynomialSetString(P: TCnBigNumberPolynomial;
  const Str: string; const VarName: string): Boolean;
var
  C, Ptr: PChar;
  Num, ES: string;
  MDFlag, E: Integer;
  IsNeg: Boolean;
begin
  Result := False;
  if Str = '' then
    Exit;

  MDFlag := -1;
  C := @Str[1];

  while C^ <> #0 do
  begin
    if not (C^ in ['+', '-', '0'..'9']) and (C^ <> VarName) then
    begin
      Inc(C);
      Continue;
    end;

    IsNeg := False;
    if C^ = '+' then
      Inc(C)
    else if C^ = '-' then
    begin
      IsNeg := True;
      Inc(C);
    end;

    Num := '1';
    if C^ in ['0'..'9'] then // 找系数
    begin
      Ptr := C;
      while C^ in ['0'..'9'] do
        Inc(C);

      // Ptr 到 C 之间是数字，代表一个系数
      SetString(Num, Ptr, C - Ptr);
      if IsNeg then
        Num := '-' + Num;
    end
    else if IsNeg then
      Num := '-' + Num;

    if C^ = VarName then
    begin
      E := 1;
      Inc(C);
      if C^ = '^' then // 找指数
      begin
        Inc(C);
        if C^ in ['0'..'9'] then
        begin
          Ptr := C;
          while C^ in ['0'..'9'] do
            Inc(C);

          // Ptr 到 C 之间是数字，代表一个指数
          SetString(ES, Ptr, C - Ptr);
          E := StrToInt64(ES);
        end;
      end;
    end
    else
      E := 0;

    // 指数找完了，凑
    if MDFlag = -1 then // 第一个指数是 MaxDegree
    begin
      P.MaxDegree := E;
      MDFlag := 0;
    end;

    P[E].SetDec(AnsiString(Num));
  end;
end;

{$WARNINGS ON}

function BigNumberPolynomialIsZero(P: TCnBigNumberPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsZero;
end;

procedure BigNumberPolynomialSetZero(P: TCnBigNumberPolynomial);
begin
  P.Clear;
  P.Add.SetZero;
end;

function BigNumberPolynomialIsOne(P: TCnBigNumberPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsOne;
end;

procedure BigNumberPolynomialSetOne(P: TCnBigNumberPolynomial);
begin
  P.Clear;
  P.Add.SetOne;
end;

function BigNumberPolynomialIsNegOne(P: TCnBigNumberPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsNegOne;
end;

procedure BigNumberPolynomialNegate(P: TCnBigNumberPolynomial);
var
  I: Integer;
begin
  for I := 0 to P.MaxDegree do
    P[I].Negate;
end;

function BigNumberPolynomialIsMonic(P: TCnBigNumberPolynomial): Boolean;
begin
  Result := P[P.MaxDegree].IsOne;
end;

procedure BigNumberPolynomialShiftLeft(P: TCnBigNumberPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigNumberPolynomialShiftRight(P, -N)
  else
    for I := 1 to N do
      P.Insert(0, TCnBigNumber.Create);
end;

procedure BigNumberPolynomialShiftRight(P: TCnBigNumberPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigNumberPolynomialShiftLeft(P, -N)
  else
  begin
    for I := 1 to N do
      P.Delete(0);

    if P.Count = 0 then
      P.Add.SetZero;
  end;
end;

function BigNumberPolynomialEqual(A, B: TCnBigNumberPolynomial): Boolean;
var
  I: Integer;
begin
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  Result := A.MaxDegree = B.MaxDegree;
  if Result then
  begin
    for I := A.MaxDegree downto 0 do
    begin
      if BigNumberCompare(A[I], B[I]) <> 0 then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

// ======================== 一元大整系数多项式普通运算 =============================

procedure BigNumberPolynomialAddWord(P: TCnBigNumberPolynomial; N: Cardinal);
begin
  if N <> 0 then
    BigNumberAddWord(P[0], N);
end;

procedure BigNumberPolynomialSubWord(P: TCnBigNumberPolynomial; N: Cardinal);
begin
  if N <> 0 then
    BigNumberSubWord(P[0], N);
end;

procedure BigNumberPolynomialMulWord(P: TCnBigNumberPolynomial; N: Cardinal);
var
  I: Integer;
begin
  if N = 0 then
    BigNumberPolynomialSetZero(P)
  else if N <> 1 then
  begin
    for I := 0 to P.MaxDegree do
      BigNumberMulWord(P[I], N);
  end;
end;

procedure BigNumberPolynomialDivWord(P: TCnBigNumberPolynomial; N: Cardinal);
var
  I: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);

  if N <> 1 then
  begin
    for I := 0 to P.MaxDegree do
      BigNumberDivWord(P[I], N);
  end;
end;

procedure BigNumberPolynomialNonNegativeModWord(P: TCnBigNumberPolynomial; N: Cardinal);
var
  I: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);;

  for I := 0 to P.MaxDegree do
  begin
    BigNumberModWord(P[I], N);
    if P[I].IsNegative then
      BigNumberAddWord(P[I], N);
  end;
end;

procedure BigNumberPolynomialAddBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
begin
  BigNumberAdd(P[0], P[0], N);
end;

procedure BigNumberPolynomialSubBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
begin
  BigNumberSub(P[0], P[0], N);
end;

procedure BigNumberPolynomialMulBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
var
  I: Integer;
begin
  if N.IsZero then
    BigNumberPolynomialSetZero(P)
  else if not N.IsOne then
  begin
    for I := 0 to P.MaxDegree do
      BigNumberMul(P[I], P[I], N);
  end;
end;

procedure BigNumberPolynomialDivBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
var
  I: Integer;
  T: TCnBigNumber;
begin
  if N.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if not N.IsOne then
  begin
    T := FLocalBigNumberPool.Obtain;
    try
      for I := 0 to P.MaxDegree do
        BigNumberDiv(P[I], T, P[I], N);
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
  end;
end;

procedure BigNumberPolynomialNonNegativeModBigNumber(P: TCnBigNumberPolynomial; N: TCnBigNumber);
var
  I: Integer;
begin
  if N.IsZero then
    raise EDivByZero.Create(SDivByZero);

  for I := 0 to P.MaxDegree do
    BigNumberNonNegativeMod(P[I], P[I], N);
end;

function BigNumberPolynomialAdd(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial): Boolean;
var
  I, D1, D2: Integer;
  PBig: TCnBigNumberPolynomial;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then
      PBig := P1
    else
      PBig := P2;

    Res.MaxDegree := D1; // 考虑到 Res 可能是 P1 或 P2，所以给 Res 的 MaxDegree 赋值得放上面的比较之后
    for I := D1 downto D2 + 1 do
      BigNumberCopy(Res[I], PBig[I]);
  end
  else // D1 = D2 说明俩加式同次
    Res.MaxDegree := D1;

  for I := D2 downto 0 do
    BigNumberAdd(Res[I], P1[I], P2[I]);

  Res.CorrectTop;
  Result := True;
end;

function BigNumberPolynomialSub(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial): Boolean;
var
  I, D1, D2: Integer;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  Res.MaxDegree := D1;
  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then // 被减式大
    begin
      for I := D1 downto D2 + 1 do
        BigNumberCopy(Res[I], P1[I]);
    end
    else  // 减式大
    begin
      for I := D1 downto D2 + 1 do
      begin
        BigNumberCopy(Res[I], P2[I]);
        Res[I].Negate;
      end;
    end;
  end;

  for I := D2 downto 0 do
    BigNumberSub(Res[I], P1[I], P2[I]);

  Res.CorrectTop;
  Result := True;
end;

function BigNumberPolynomialMul(Res: TCnBigNumberPolynomial; P1: TCnBigNumberPolynomial;
  P2: TCnBigNumberPolynomial): Boolean;
var
  R: TCnBigNumberPolynomial;
  T: TCnBigNumber;
  I, J: Integer;
begin
  if BigNumberPolynomialIsZero(P1) or BigNumberPolynomialIsZero(P2) then
  begin
    BigNumberPolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  T := FLocalBigNumberPool.Obtain;
  if (Res = P1) or (Res = P2) then
    R := FLocalBigNumberPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxDegree := P1.MaxDegree + P2.MaxDegree;

  for I := 0 to P1.MaxDegree do
  begin
    // 把第 I 次方的数字乘以 P2 的每一个数字，加到结果的 I 开头的部分
    for J := 0 to P2.MaxDegree do
    begin
      BigNumberMul(T, P1[I], P2[J]);
      BigNumberAdd(R[I + J], R[I + J], T);
    end;
  end;

  R.CorrectTop;
  if (Res = P1) or (Res = P2) then
  begin
    BigNumberPolynomialCopy(Res, R);
    FLocalBigNumberPolynomialPool.Recycle(R);
  end;
  FLocalBigNumberPool.Recycle(T);
  Result := True;
end;

function BigNumberPolynomialDiv(Res: TCnBigNumberPolynomial; Remain: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Divisor: TCnBigNumberPolynomial; ErrMulFactor: TCnBigNumber): Boolean;
var
  SubRes: TCnBigNumberPolynomial; // 容纳递减差
  MulRes: TCnBigNumberPolynomial; // 容纳除数乘积
  DivRes: TCnBigNumberPolynomial; // 容纳临时商
  I, D: Integer;
  T, R: TCnBigNumber;
begin
  if BigNumberPolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      BigNumberPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      BigNumberPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;
  T := nil;
  R := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    R := FLocalBigNumberPool.Obtain;

    SubRes := FLocalBigNumberPolynomialPool.Obtain;
    BigNumberPolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := FLocalBigNumberPolynomialPool.Obtain;
    DivRes.MaxDegree := D;
    MulRes := FLocalBigNumberPolynomialPool.Obtain;

    Result := False;
    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then                 // 中间结果可能跳位
        Continue;

      // 判断 Divisor[Divisor.MaxDegree] 是否能整除 SubRes[P.MaxDegree - I] 不能则说明超出了整型多项式范围，无法支持，只能出错
      if not BigNumberMod(T, SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]) then
        Exit;

      if not T.IsZero then
      begin
        if ErrMulFactor <> nil then
        begin
          // Divisor[Divisor.MaxDegree] 乘以两者的最大公约数
          if BigNumberGcd(T, SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]) then
            BigNumberMul(ErrMulFactor, Divisor[Divisor.MaxDegree], T);
        end;
        Exit;
      end;

      BigNumberPolynomialCopy(MulRes, Divisor);
      BigNumberPolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次
      BigNumberDiv(T, R, SubRes[P.MaxDegree - I], MulRes[MulRes.MaxDegree]);

      BigNumberPolynomialMulBigNumber(MulRes, T); // 除式乘到最高次系数相同
      BigNumberCopy(DivRes[D - I], T);            // 商放到 DivRes 位置

      BigNumberPolynomialSub(SubRes, SubRes, MulRes);              // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      BigNumberPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      BigNumberPolynomialCopy(Res, DivRes);
  finally
    FLocalBigNumberPolynomialPool.Recycle(SubRes);
    FLocalBigNumberPolynomialPool.Recycle(MulRes);
    FLocalBigNumberPolynomialPool.Recycle(DivRes);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(R);
  end;
  Result := True;
end;

function BigNumberPolynomialMod(Res: TCnBigNumberPolynomial; P: TCnBigNumberPolynomial;
  Divisor: TCnBigNumberPolynomial; ErrMulFactor: TCnBigNumber): Boolean;
begin
  Result := BigNumberPolynomialDiv(nil, Res, P, Divisor, ErrMulFactor);
end;

function BigNumberPolynomialPower(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Exponent: TCnBigNumber): Boolean;
var
  T: TCnBigNumberPolynomial;
  E: TCnBigNumber;
begin
  if Exponent.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent.IsOne then
  begin
    if Res <> P then
      BigNumberPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end
  else if Exponent.IsNegative then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidExponent, [Exponent.ToDec]);

  T := FLocalBigNumberPolynomialPool.Obtain;
  BigNumberPolynomialCopy(T, P);
  E := FLocalBigNumberPool.Obtain;
  BigNumberCopy(E, Exponent);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while not E.IsZero do // E 大于 0 无需判断
    begin
      if BigNumberIsBitSet(E, 0) then
        BigNumberPolynomialMul(Res, Res, T);

      BigNumberShiftRightOne(E, E);
      if not E.IsZero then // 最后一次无需乘了
        BigNumberPolynomialMul(T, T, T);
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(E);
    FLocalBigNumberPolynomialPool.Recycle(T);
  end;
end;

procedure BigNumberPolynomialReduce(P: TCnBigNumberPolynomial);
var
  I: Integer;
  D: TCnBigNumber;
begin
  if P.MaxDegree = 0 then
  begin
    if not P[P.MaxDegree].IsZero then
      P[P.MaxDegree].SetOne;
  end
  else
  begin
    D := FLocalBigNumberPool.Obtain;
    BigNumberCopy(D, P[0]);

    for I := 0 to P.MaxDegree - 1 do
    begin
      BigNumberGcd(D, D, P[I + 1]);
      if D.IsOne then
        Break;
    end;

    if not D.IsOne then
      BigNumberPolynomialDivBigNumber(P, D);
  end;
end;

procedure BigNumberPolynomialCentralize(P: TCnBigNumberPolynomial; Modulus: TCnBigNumber);
var
  I: Integer;
  K: TCnBigNumber;
begin
  K := FLocalBigNumberPool.Obtain;
  try
    BigNumberShiftRightOne(K, Modulus);
    for I := 0 to P.MaxDegree do
      if BigNumberCompare(P[I], K) > 0 then
        BigNumberSub(P[I], P[I], Modulus);
  finally
    FLocalBigNumberPool.Recycle(K);
  end;
end;

function BigNumberPolynomialGreatestCommonDivisor(Res: TCnBigNumberPolynomial;
  P1, P2: TCnBigNumberPolynomial): Boolean;
var
  A, B, C: TCnBigNumberPolynomial;
  MF: TCnBigNumber;
  GcdValue: TCnBigNumber;
begin
  A := nil;
  B := nil;
  C := nil;
  MF := nil;
  GcdValue := nil;

  try
    A := FLocalBigNumberPolynomialPool.Obtain;
    B := FLocalBigNumberPolynomialPool.Obtain;
    MF := FLocalBigNumberPool.Obtain;

    if P1.MaxDegree >= P2.MaxDegree then
    begin
      BigNumberPolynomialCopy(A, P1);
      BigNumberPolynomialCopy(B, P2);
    end
    else
    begin
      BigNumberPolynomialCopy(A, P2);
      BigNumberPolynomialCopy(B, P1);
    end;

    // 特殊处理：如果两个多项式都是常数项，直接计算 BigNumber GCD
    if (A.MaxDegree = 0) and (B.MaxDegree = 0) then
    begin
      GcdValue := FLocalBigNumberPool.Obtain;
      if BigNumberGcd(GcdValue, A[0], B[0]) then
      begin
        Res.SetCoefficent(0, GcdValue);
        Result := True;
      end
      else
        Result := False;
      Exit;
    end;

    C := FLocalBigNumberPolynomialPool.Obtain;
    while not B.IsZero do
    begin
      BigNumberPolynomialCopy(C, B);        // 备份 B
      while not BigNumberPolynomialMod(B, A, B, MF) do   // A mod B 给 B
        BigNumberPolynomialMulBigNumber(A, MF);

      // B 要系数约分化简
      BigNumberPolynomialReduce(B);
      BigNumberPolynomialCopy(A, C);        // 原始 B 给 A
    end;

    BigNumberPolynomialCopy(Res, A);
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(GcdValue);
    FLocalBigNumberPool.Recycle(MF);
    FLocalBigNumberPolynomialPool.Recycle(C);
    FLocalBigNumberPolynomialPool.Recycle(B);
    FLocalBigNumberPolynomialPool.Recycle(A);
  end;
end;

function BigNumberPolynomialLeastCommonMultiple(Res: TCnBigNumberPolynomial;
  P1, P2: TCnBigNumberPolynomial): Boolean;
var
  G, M, R: TCnBigNumberPolynomial;
begin
  Result := False;
  if BigNumberPolynomialEqual(P1, P2) then
  begin
    BigNumberPolynomialCopy(Res, P1);
    Result := True;
    Exit;
  end;

  G := nil;
  M := nil;
  R := nil;

  try
    G := FLocalBigNumberPolynomialPool.Obtain;
    M := FLocalBigNumberPolynomialPool.Obtain;
    R := FLocalBigNumberPolynomialPool.Obtain;

    if not BigNumberPolynomialMul(M, P1, P2) then
      Exit;

    if not BigNumberPolynomialGreatestCommonDivisor(G, P1, P2) then
      Exit;

    if not BigNumberPolynomialDiv(Res, R, M, G) then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPolynomialPool.Recycle(R);
    FLocalBigNumberPolynomialPool.Recycle(M);
    FLocalBigNumberPolynomialPool.Recycle(G);
  end;
end;

function BigNumberPolynomialCompose(Res: TCnBigNumberPolynomial;
  F, P: TCnBigNumberPolynomial): Boolean;
var
  I: Integer;
  R, X, T: TCnBigNumberPolynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    BigNumberCopy(Res[0], F[0]);
    Result := True;
    Exit;
  end;

  if (Res = F) or (Res = P) then
    R := FLocalBigNumberPolynomialPool.Obtain
  else
    R := Res;

  X := FLocalBigNumberPolynomialPool.Obtain;
  T := FLocalBigNumberPolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      BigNumberPolynomialCopy(T, X);
      BigNumberPolynomialMulBigNumber(T, F[I]);
      BigNumberPolynomialAdd(R, R, T);

      if I <> F.MaxDegree then
        BigNumberPolynomialMul(X, X, P);
    end;

    if (Res = F) or (Res = P) then
    begin
      BigNumberPolynomialCopy(Res, R);
      FLocalBigNumberPolynomialPool.Recycle(R);
    end;
  finally
    FLocalBigNumberPolynomialPool.Recycle(X);
    FLocalBigNumberPolynomialPool.Recycle(T);
  end;
  Result := True;
end;

procedure BigNumberPolynomialGetValue(Res: TCnBigNumber; F: TCnBigNumberPolynomial;
  X: TCnBigNumber);
var
  I: Integer;
  T, M: TCnBigNumber;
begin
  BigNumberCopy(Res, F[0]);
  if X.IsZero or (F.MaxDegree = 0) then    // 只有常数项的情况下，得常数项
    Exit;

  T := FLocalBigNumberPool.Obtain;
  M := FLocalBigNumberPool.Obtain;

  try
    BigNumberCopy(T, X);

    // 把 F 中的每个系数都和 X 的对应次幂相乘，最后相加
    for I := 1 to F.MaxDegree do
    begin
      BigNumberMul(M, F[I], T);
      BigNumberAdd(Res, Res, M);

      if I <> F.MaxDegree then
        BigNumberMul(T, T, X);
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(M);
  end;
end;

procedure BigNumberPolynomialReduce2(P1, P2: TCnBigNumberPolynomial);
var
  D: TCnBigNumberPolynomial;
begin
  if P1 = P2 then
  begin
    P1.SetOne;
    Exit;
  end;

  D := FLocalBigNumberPolynomialPool.Obtain;
  try
    if not BigNumberPolynomialGreatestCommonDivisor(D, P1, P2) then
      Exit;

    if not D.IsOne then
    begin
      BigNumberPolynomialDiv(P1, nil, P1, D);
      BigNumberPolynomialDiv(P1, nil, P1, D);
    end;
  finally
    FLocalBigNumberPolynomialPool.Recycle(D);
  end;
end;

// ===================== 有限扩域下的整系数多项式模运算 ========================

function BigNumberPolynomialGaloisEqual(A, B: TCnBigNumberPolynomial;
  Prime: TCnBigNumber): Boolean;
var
  I: Integer;
  T1, T2: TCnBigNumber;
begin
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  Result := A.MaxDegree = B.MaxDegree;
  if Result then
  begin
    T1 := FLocalBigNumberPool.Obtain;
    T2 := FLocalBigNumberPool.Obtain;

    try
      for I := A.MaxDegree downto 0 do
      begin
        if BigNumberEqual(A[I], B[I]) then
          Continue;

        // 不相等则判断余数
        BigNumberNonNegativeMod(T1, A[I], Prime);
        BigNumberNonNegativeMod(T2, B[I], Prime);

        if not BigNumberEqual(T1, T2) then
        begin
          Result := False;
          Exit;
        end;
      end;
    finally
      FLocalBigNumberPool.Recycle(T2);
      FLocalBigNumberPool.Recycle(T1);
    end;
  end;
end;

procedure BigNumberPolynomialGaloisNegate(P: TCnBigNumberPolynomial;
  Prime: TCnBigNumber);
var
  I: Integer;
begin
  for I := 0 to P.MaxDegree do
  begin
    P[I].Negate;
    BigNumberNonNegativeMod(P[I], P[I], Prime);
  end;
end;

function BigNumberPolynomialGaloisAdd(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
begin
  Result := BigNumberPolynomialAdd(Res, P1, P2);
  if Result then
  begin
    BigNumberPolynomialNonNegativeModBigNumber(Res, Prime);
    if Primitive <> nil then
      BigNumberPolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function BigNumberPolynomialGaloisSub(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
begin
  Result := BigNumberPolynomialSub(Res, P1, P2);
  if Result then
  begin
    BigNumberPolynomialNonNegativeModBigNumber(Res, Prime);
    if Primitive <> nil then
      BigNumberPolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function BigNumberPolynomialGaloisMul(Res: TCnBigNumberPolynomial;
  P1: TCnBigNumberPolynomial; P2: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
var
  R: TCnBigNumberPolynomial;
  T: TCnBigNumber;
  I, J: Integer;
begin
  if BigNumberPolynomialIsZero(P1) or BigNumberPolynomialIsZero(P2) then
  begin
    BigNumberPolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  T := FLocalBigNumberPool.Obtain;
  if (Res = P1) or (Res = P2) then
    R := FLocalBigNumberPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxDegree := P1.MaxDegree + P2.MaxDegree;

  for I := 0 to P1.MaxDegree do
  begin
    // 把第 I 次方的数字乘以 P2 的每一个数字，加到结果的 I 开头的部分
    for J := 0 to P2.MaxDegree do
    begin
      BigNumberMul(T, P1[I], P2[J]);
      BigNumberAdd(R[I + J], R[I + J], T);
      BigNumberNonNegativeMod(R[I + J], R[I + J], Prime);
    end;
  end;

  R.CorrectTop;

  // 再对本原多项式取模，注意这里传入的本原多项式是 mod 操作的除数，不是本原多项式参数
  if Primitive <> nil then
    BigNumberPolynomialGaloisMod(R, R, Primitive, Prime);

  if (Res = P1) or (Res = P2) then
  begin
    BigNumberPolynomialCopy(Res, R);
    FLocalBigNumberPolynomialPool.Recycle(R);
  end;
  FLocalBigNumberPool.Recycle(T);
  Result := True;
end;

function BigNumberPolynomialGaloisDiv(Res: TCnBigNumberPolynomial;
  Remain: TCnBigNumberPolynomial; P: TCnBigNumberPolynomial;
  Divisor: TCnBigNumberPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial; ErrMulFactor: TCnBigNumber): Boolean;
var
  SubRes: TCnBigNumberPolynomial; // 容纳递减差
  MulRes: TCnBigNumberPolynomial; // 容纳除数乘积
  DivRes: TCnBigNumberPolynomial; // 容纳临时商
  I, D: Integer;
  K, T: TCnBigNumber;
  Co: Boolean;
begin
  Result := False;
  if BigNumberPolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      BigNumberPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      BigNumberPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;
  T := nil;
  K := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    SubRes := FLocalBigNumberPolynomialPool.Obtain;
    BigNumberPolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := FLocalBigNumberPolynomialPool.Obtain;
    DivRes.MaxDegree := D;
    MulRes := FLocalBigNumberPolynomialPool.Obtain;

    Co := True;
    K := FLocalBigNumberPool.Obtain;
    if Divisor[Divisor.MaxDegree].IsOne then
      K.SetOne
    else
      Co := BigNumberModularInverse(K, Divisor[Divisor.MaxDegree], Prime, True);
      // 要传 CheckGcd 为 True 以在不互素时返回 Co 为 False

    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then               // 中间结果可能跳位
        Continue;
      BigNumberPolynomialCopy(MulRes, Divisor);
      BigNumberPolynomialShiftLeft(MulRes, D - I);             // 对齐到 SubRes 的最高次

      if Co then // 互素有模逆元
      begin
        // 除式要乘一个数，这个数是 SubRes 最高位除以除式最高位得到的结果，也即 SubRes 最高位乘以除式最高位的逆元再 mod Prime
        BigNumberDirectMulMod(T, SubRes[P.MaxDegree - I], K, Prime);
        BigNumberPolynomialGaloisMulBigNumber(MulRes, T, Prime);          // 除式乘到最高次系数相同
      end
      else // Prime 和除式最高位不互素时模逆元 K 不存在，要分整除和不整除两种情况
      begin
        BigNumberMod(T, SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]);
        if not T.IsZero then // 不整除又没有模逆元，无论如何都没法除，只能出错退出
        begin
          if ErrMulFactor <> nil then
          begin
            // Divisor[Divisor.MaxDegree] 乘以两者的最大公约数
            if BigNumberGcd(T, SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]) then
              BigNumberMul(ErrMulFactor, Divisor[Divisor.MaxDegree], T);
          end;
          Exit;
        end
        else
        begin
          BigNumberDiv(T, nil, SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]);
          BigNumberPolynomialGaloisMulBigNumber(MulRes, T, Prime); // 除式乘到最高次系数相同
        end;
      end;

      BigNumberCopy(DivRes[D - I], T);                             // 对应位的商放到 DivRes 位置
      BigNumberPolynomialGaloisSub(SubRes, SubRes, MulRes, Prime); // 减求模后结果重新放回 SubRes
    end;

    // 商与余式都需要再模本原多项式
    if Primitive <> nil then
    begin
      BigNumberPolynomialGaloisMod(SubRes, SubRes, Primitive, Prime);
      BigNumberPolynomialGaloisMod(DivRes, DivRes, Primitive, Prime);
    end;

    if Remain <> nil then
      BigNumberPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      BigNumberPolynomialCopy(Res, DivRes);
    Result := True;
  finally
    FLocalBigNumberPolynomialPool.Recycle(SubRes);
    FLocalBigNumberPolynomialPool.Recycle(MulRes);
    FLocalBigNumberPolynomialPool.Recycle(DivRes);
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(K);
  end;
end;

function BigNumberPolynomialGaloisMod(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Divisor: TCnBigNumberPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial; ErrMulFactor: TCnBigNumber): Boolean;
begin
  Result := BigNumberPolynomialGaloisDiv(nil, Res, P, Divisor, Prime, Primitive, ErrMulFactor);
end;

function BigNumberPolynomialGaloisPower(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Exponent: TCnBigNumber;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial): Boolean;
var
  T: TCnBigNumberPolynomial;
  E: TCnBigNumber;
begin
  if Exponent.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent.IsOne then
  begin
    if Res <> P then
      BigNumberPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end
  else if Exponent.IsNegative then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidExponent, [Exponent]);

  T := FLocalBigNumberPolynomialPool.Obtain;
  BigNumberPolynomialCopy(T, P);
  E := FLocalBigNumberPool.Obtain;
  BigNumberCopy(E, Exponent);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while not E.IsZero do
    begin
      if BigNumberIsBitSet(E, 0) then
        BigNumberPolynomialGaloisMul(Res, Res, T, Prime, Primitive);

      BigNumberShiftRightOne(E, E);
      if not E.IsZero then
        BigNumberPolynomialGaloisMul(T, T, T, Prime, Primitive);
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(E);
    FLocalBigNumberPolynomialPool.Recycle(T);
  end;
end;

function BigNumberPolynomialGaloisPower(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberPolynomial; Exponent: Cardinal; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial): Boolean; overload;
var
  T: TCnBigNumber;
begin
  T := FLocalBigNumberPool.Obtain;
  try
    T.SetWord(Exponent);
    Result := BigNumberPolynomialGaloisPower(Res, P, T, Prime, Primitive);
  finally
    FLocalBigNumberPool.Recycle(T);
  end;
end;

function BigNumberPolynomialGaloisAddWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
begin
  if N <> 0 then
  begin
    BigNumberAddWord(P[0], N);
    BigNumberNonNegativeMod(P[0], P[0], Prime);
  end;
  Result := True;
end;

function BigNumberPolynomialGaloisSubWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
begin
  if N <> 0 then
  begin
    BigNumberSubWord(P[0], N);
    BigNumberNonNegativeMod(P[0], P[0], Prime);
  end;
  Result := True;
end;

function BigNumberPolynomialGaloisMulWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
var
  I: Integer;
begin
  if N = 0 then
  begin
    BigNumberPolynomialSetZero(P);
  end
  else if N <> 1 then
  begin
    for I := 0 to P.MaxDegree do
    begin
      BigNumberMulWord(P[I], N);
      BigNumberNonNegativeMod(P[I], P[I], Prime);
    end;
  end;
  Result := True;
end;

function BigNumberPolynomialGaloisDivWord(P: TCnBigNumberPolynomial;
  N: Cardinal; Prime: TCnBigNumber): Boolean;
var
  I: Integer;
  K, T: TCnBigNumber;
begin
  if (N = 0) or Prime.IsZero then
    raise EDivByZero.Create(SDivByZero);

  K := nil;
  T := nil;

  try
    K := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;
    T.SetWord(N);

    BigNumberModularInverse(K, T, Prime);
    for I := 0 to P.MaxDegree do
    begin
      BigNumberMul(P[I], P[I], T);
      BigNumberNonNegativeMod(P[I], P[I], Prime);
    end;
  finally
    FLocalBigNumberPool.Recycle(K);
    FLocalBigNumberPool.Recycle(T);
  end;
  Result := True;
end;

procedure BigNumberPolynomialGaloisAddBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
begin
  BigNumberAdd(P[0], P[0], N);
  BigNumberNonNegativeMod(P[0], P[0], Prime);
end;

procedure BigNumberPolynomialGaloisSubBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
begin
  BigNumberSub(P[0], P[0], N);
  BigNumberNonNegativeMod(P[0], P[0], Prime);
end;

procedure BigNumberPolynomialGaloisMulBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
var
  I: Integer;
begin
  if N.IsZero then
    BigNumberPolynomialSetZero(P)
  else if not N.IsOne then
  begin
    for I := 0 to P.MaxDegree do
    begin
      BigNumberMul(P[I], P[I], N);
      BigNumberNonNegativeMod(P[I], P[I], Prime);
    end;
  end;
end;

procedure BigNumberPolynomialGaloisDivBigNumber(P: TCnBigNumberPolynomial;
  N: TCnBigNumber; Prime: TCnBigNumber);
var
  I: Integer;
  K: TCnBigNumber;
  B: Boolean;
begin
  if N.IsZero or Prime.IsZero then
    raise EDivByZero.Create(SDivByZero);

  B := N.IsNegative;
  if B then
    N.Negate;

  K := FLocalBigNumberPool.Obtain;
  try
    BigNumberModularInverse(K, N, Prime);

    for I := 0 to P.MaxDegree do
    begin
      BigNumberMul(P[I], P[I], K);
      BigNumberNonNegativeMod(P[I], P[I], Prime);

      if B then
        BigNumberSub(P[I], Prime, P[I]);
    end;
  finally
    FLocalBigNumberPool.Recycle(K);
    if B then
      N.Negate;
  end;
end;

procedure BigNumberPolynomialGaloisMonic(P: TCnBigNumberPolynomial; Prime: TCnBigNumber);
begin
  if not P[P.MaxDegree].IsZero and not P[P.MaxDegree].IsOne then
    BigNumberPolynomialGaloisDivBigNumber(P, P[P.MaxDegree], Prime);
end;

function BigNumberPolynomialGaloisGreatestCommonDivisor(Res: TCnBigNumberPolynomial;
  P1, P2: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean;
var
  A, B, C: TCnBigNumberPolynomial;
begin
  A := nil;
  B := nil;
  C := nil;

  try
    A := FLocalBigNumberPolynomialPool.Obtain;
    B := FLocalBigNumberPolynomialPool.Obtain;

    if P1.MaxDegree >= P2.MaxDegree then
    begin
      BigNumberPolynomialCopy(A, P1);
      BigNumberPolynomialCopy(B, P2);
    end
    else
    begin
      BigNumberPolynomialCopy(A, P2);
      BigNumberPolynomialCopy(B, P1);
    end;

    C := FLocalBigNumberPolynomialPool.Obtain;
    while not B.IsZero do
    begin
      BigNumberPolynomialCopy(C, B);          // 备份 B
      BigNumberPolynomialGaloisMod(B, A, B, Prime);  // A mod B 给 B

      if B.MaxDegree = 0 then  // 如果是常数项则变为 1
      begin
        if not B[0].IsZero then
          B[0].SetOne;
      end;

      BigNumberPolynomialCopy(A, C);          // 原始 B 给 A
    end;

    BigNumberPolynomialCopy(Res, A);
    BigNumberPolynomialGaloisMonic(Res, Prime);      // 首项化为一
    Result := True;
  finally
    FLocalBigNumberPolynomialPool.Recycle(A);
    FLocalBigNumberPolynomialPool.Recycle(B);
    FLocalBigNumberPolynomialPool.Recycle(C);
  end;
end;

function BigNumberPolynomialGaloisLeastCommonMultiple(Res: TCnBigNumberPolynomial;
  P1, P2: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean;
var
  G, M, R: TCnBigNumberPolynomial;
begin
  Result := False;
  if BigNumberPolynomialEqual(P1, P2) then
  begin
    BigNumberPolynomialCopy(Res, P1);
    Result := True;
    Exit;
  end;

  G := nil;
  M := nil;
  R := nil;

  try
    G := FLocalBigNumberPolynomialPool.Obtain;
    M := FLocalBigNumberPolynomialPool.Obtain;
    R := FLocalBigNumberPolynomialPool.Obtain;

    if not BigNumberPolynomialGaloisMul(M, P1, P2, Prime) then
      Exit;

    if not BigNumberPolynomialGaloisGreatestCommonDivisor(G, P1, P2, Prime) then
      Exit;

    if not BigNumberPolynomialGaloisDiv(Res, R, M, G, Prime) then
      Exit;

    Result := True;
  finally
    FLocalBigNumberPolynomialPool.Recycle(R);
    FLocalBigNumberPolynomialPool.Recycle(M);
    FLocalBigNumberPolynomialPool.Recycle(G);
  end;
end;

procedure BigNumberPolynomialGaloisExtendedEuclideanGcd(A, B: TCnBigNumberPolynomial;
  X, Y: TCnBigNumberPolynomial; Prime: TCnBigNumber);
var
  T, P, M: TCnBigNumberPolynomial;
begin
  if B.IsZero then
  begin
    X.SetZero;
    BigNumberModularInverse(X[0], A[0], Prime);
    // X 得是 A 对于 P 的模逆元而不能像整数的辗转相除法那样是 1
    // 因为 A 可能是不等于 1 的整数
    Y.SetZero;
  end
  else
  begin
    T := nil;
    P := nil;
    M := nil;

    try
      T := FLocalBigNumberPolynomialPool.Obtain;
      P := FLocalBigNumberPolynomialPool.Obtain;
      M := FLocalBigNumberPolynomialPool.Obtain;

      BigNumberPolynomialGaloisMod(P, A, B, Prime);

      BigNumberPolynomialGaloisExtendedEuclideanGcd(B, P, Y, X, Prime);

      // Y := Y - (A div B) * X;
      BigNumberPolynomialGaloisDiv(P, M, A, B, Prime);
      BigNumberPolynomialGaloisMul(P, P, X, Prime);
      BigNumberPolynomialGaloisSub(Y, Y, P, Prime);
    finally
      FLocalBigNumberPolynomialPool.Recycle(M);
      FLocalBigNumberPolynomialPool.Recycle(P);
      FLocalBigNumberPolynomialPool.Recycle(T);
    end;
  end;
end;

procedure BigNumberPolynomialGaloisModularInverse(Res: TCnBigNumberPolynomial;
  X, Modulus: TCnBigNumberPolynomial; Prime: TCnBigNumber; CheckGcd: Boolean = False);
var
  X1, Y, G: TCnBigNumberPolynomial;
begin
  X1 := nil;
  Y := nil;
  G := nil;

  try
    if CheckGcd then
    begin
      G := FLocalBigNumberPolynomialPool.Obtain;
      BigNumberPolynomialGaloisGreatestCommonDivisor(G, X, Modulus, Prime);
      if not G.IsOne then
        raise ECnPolynomialException.Create(SCnErrorPolynomialGCDMustOne);
    end;

    X1 := FLocalBigNumberPolynomialPool.Obtain;
    Y := FLocalBigNumberPolynomialPool.Obtain;

    BigNumberPolynomialCopy(X1, X);

    // 扩展欧几里得辗转相除法求二元一次不定整系数多项式方程 A * X - B * Y = 1 的整数解
    BigNumberPolynomialGaloisExtendedEuclideanGcd(X1, Modulus, Res, Y, Prime);
  finally
    FLocalBigNumberPolynomialPool.Recycle(X1);
    FLocalBigNumberPolynomialPool.Recycle(Y);
    FLocalBigNumberPolynomialPool.Recycle(G);
  end;
end;

function BigNumberPolynomialGaloisPrimePowerModularInverse(Res: TCnBigNumberPolynomial;
  X, Modulus: TCnBigNumberPolynomial; PrimeRoot: TCnBigNumber; Exponent: Integer): Boolean;
var
  F, G, T: TCnBigNumberPolynomial;
  N: Integer;
  P: TCnBigNumber;
begin
  // 原始 X 和 Modulus 是模 PrimeRoot^Exponent 下的，各系数对 PrimeRoot 求模得到 F 和 G 俩多项式

  if Exponent < 2 then
    raise ECnPolynomialException.Create(SCnErrorPolynomialInvalidExponent);

  F := nil;
  G := nil;
  T := nil;
  P := nil;

  try
    F := FLocalBigNumberPolynomialPool.Obtain;
    G := FLocalBigNumberPolynomialPool.Obtain;

    BigNumberPolynomialCopy(F, X);
    BigNumberPolynomialCopy(G, Modulus);

    BigNumberPolynomialNonNegativeModBigNumber(F, PrimeRoot);
    BigNumberPolynomialNonNegativeModBigNumber(G, PrimeRoot);

    T := FLocalBigNumberPolynomialPool.Obtain;
    BigNumberPolynomialGaloisGreatestCommonDivisor(T, F, G, PrimeRoot);

    Result := T.IsOne;  // F G 释放了可以复用
    if not Result then  // 须 PrimeRoot 下互素 PrimeRoot^Exponent 下才有逆元
      Exit;

    BigNumberPolynomialGaloisModularInverse(T, F, G, PrimeRoot); // 求 PrimeRoot 模下的逆多项式

    N := 2;
    P := FLocalBigNumberPool.Obtain;
    while N <= Exponent do
    begin
      // T := (p * T - X * T^2) in Ring(p^n, M)

      BigNumberPower(P, PrimeRoot, Cardinal(N));

      BigNumberPolynomialGaloisMul(F, T, T, P);
      BigNumberPolynomialGaloisMul(F, F, X, P);

      BigNumberPolynomialGaloisMulBigNumber(T, PrimeRoot, P);
      BigNumberPolynomialGaloisSub(T, T, F, P, Modulus);

      N := N + 1;
    end;

    // Result := T in Ring(p^e, M)
    BigNumberPower(P, PrimeRoot, Cardinal(Exponent));
    Result := BigNumberPolynomialGaloisMod(Res, T, Modulus, P);
  finally
    FLocalBigNumberPool.Recycle(P);
    FLocalBigNumberPolynomialPool.Recycle(T);
    FLocalBigNumberPolynomialPool.Recycle(G);
    FLocalBigNumberPolynomialPool.Recycle(F);
  end;
end;

function BigNumberPolynomialGaloisCompose(Res: TCnBigNumberPolynomial;
  F, P: TCnBigNumberPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
var
  I: Integer;
  R, X, T: TCnBigNumberPolynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    BigNumberNonNegativeMod(Res[0], F[0], Prime);
    Result := True;
    Exit;
  end;

  if (Res = F) or (Res = P) then
    R := FLocalBigNumberPolynomialPool.Obtain
  else
    R := Res;

  X := FLocalBigNumberPolynomialPool.Obtain;
  T := FLocalBigNumberPolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      BigNumberPolynomialCopy(T, X);
      BigNumberPolynomialGaloisMulBigNumber(T, F[I], Prime);
      BigNumberPolynomialGaloisAdd(R, R, T, Prime);

      if I <> F.MaxDegree then
        BigNumberPolynomialGaloisMul(X, X, P, Prime);
    end;

    if Primitive <> nil then
      BigNumberPolynomialGaloisMod(R, R, Primitive, Prime);

    if (Res = F) or (Res = P) then
    begin
      BigNumberPolynomialCopy(Res, R);
      FLocalBigNumberPolynomialPool.Recycle(R);
    end;
  finally
    FLocalBigNumberPolynomialPool.Recycle(X);
    FLocalBigNumberPolynomialPool.Recycle(T);
  end;
  Result := True;
end;

function BigNumberPolynomialGaloisGetValue(Res: TCnBigNumber;
  F: TCnBigNumberPolynomial; X, Prime: TCnBigNumber): Boolean;
var
  I: Integer;
  T, M: TCnBigNumber;
begin
  Result := True;
  BigNumberNonNegativeMod(Res, F[0], Prime);
  if X.IsZero or (F.MaxDegree = 0) then    // 只有常数项的情况下，得常数项
    Exit;

  T := nil;
  M := nil;

  try
    T := FLocalBigNumberPool.Obtain;
    BigNumberCopy(T, X);
    M := FLocalBigNumberPool.Obtain;

    // 把 F 中的每个系数都和 X 的对应次幂相乘，最后相加
    for I := 1 to F.MaxDegree do
    begin
      BigNumberDirectMulMod(M, F[I], T, Prime);
      BigNumberAdd(Res, Res, M);
      BigNumberNonNegativeMod(Res, Res, Prime);

      if I <> F.MaxDegree then
        BigNumberDirectMulMod(T, T, X, Prime);
    end;
    BigNumberNonNegativeMod(Res, Res, Prime);
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(M);
  end;
end;

function BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B: Integer; Degree: Integer;
  OutDivisionPolynomial: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean; overload;
var
  NA, NB: TCnBigNumber;
begin
  NA := FLocalBigNumberPool.Obtain;
  NB := FLocalBigNumberPool.Obtain;

  try
    NA.SetInteger(A);
    NB.SetInteger(B);
    Result := BigNumberPolynomialGaloisCalcDivisionPolynomial(NA, NB, Degree,
      OutDivisionPolynomial, Prime);
  finally
    FLocalBigNumberPool.Recycle(NB);
    FLocalBigNumberPool.Recycle(NA);
  end;
end;

function BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B: TCnBigNumber; Degree: Integer;
  OutDivisionPolynomial: TCnBigNumberPolynomial; Prime: TCnBigNumber): Boolean;
var
  N: Integer;
  T, MI: TCnBigNumber;
  D1, D2, D3, Y4: TCnBigNumberPolynomial;
begin
  if Degree < 0 then
    raise ECnPolynomialException.Create('Galois Division Polynomial Invalid Degree')
  else if Degree = 0 then
  begin
    OutDivisionPolynomial.SetCoefficents([0]);  // f0(X) = 0
    Result := True;
  end
  else if Degree = 1 then
  begin
    OutDivisionPolynomial.SetCoefficents([1]);  // f1(X) = 1
    Result := True;
  end
  else if Degree = 2 then
  begin
    OutDivisionPolynomial.SetCoefficents([2]);  // f2(X) = 2
    Result := True;
  end
  else if Degree = 3 then   // f3(X) = 3 X4 + 6 a X2 + 12 b X - a^2
  begin
    OutDivisionPolynomial.MaxDegree := 4;
    OutDivisionPolynomial[4].SetWord(3);
    OutDivisionPolynomial[3].SetWord(0);
    BigNumberMulWordNonNegativeMod(OutDivisionPolynomial[2], A, 6, Prime);
    BigNumberMulWordNonNegativeMod(OutDivisionPolynomial[1], B, 12, Prime);

    T := FLocalBigNumberPool.Obtain;
    try
      BigNumberCopy(T, A);
      T.Negate;
      BigNumberDirectMulMod(OutDivisionPolynomial[0], T, A, Prime);
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
    Result := True;
  end
  else if Degree = 4 then // f4(X) = 4 X6 + 20 a X4 + 80 b X3 - 20 a2X2 - 16 a b X - 4 a3 - 32 b^2
  begin
    OutDivisionPolynomial.MaxDegree := 6;
    OutDivisionPolynomial[6].SetWord(4);
    OutDivisionPolynomial[5].SetWord(0);
    BigNumberMulWordNonNegativeMod(OutDivisionPolynomial[4], A, 20, Prime);
    BigNumberMulWordNonNegativeMod(OutDivisionPolynomial[3], B, 80, Prime);

    T := FLocalBigNumberPool.Obtain;
    try
      BigNumberMulWordNonNegativeMod(T, A, -20, Prime);
      BigNumberDirectMulMod(OutDivisionPolynomial[2], T, A, Prime);
      BigNumberMulWordNonNegativeMod(T, A, -16, Prime);
      BigNumberDirectMulMod(OutDivisionPolynomial[1], T, B, Prime);

      BigNumberMulWordNonNegativeMod(T, A, -4, Prime);
      BigNumberDirectMulMod(T, T, A, Prime);
      BigNumberDirectMulMod(OutDivisionPolynomial[0], T, A, Prime);

      BigNumberMulWordNonNegativeMod(T, B, -32, Prime);
      BigNumberDirectMulMod(T, T, B, Prime);
      BigNumberAdd(OutDivisionPolynomial[0], OutDivisionPolynomial[0], T);
      BigNumberNonNegativeMod(OutDivisionPolynomial[0], OutDivisionPolynomial[0], Prime);
    finally
      FLocalBigNumberPool.Recycle(T);
    end;
    Result := True;
  end
  else
  begin
    D1 := nil;
    D2 := nil;
    D3 := nil;
    Y4 := nil;
    MI := nil;

    try
      // 开始递归计算
      N := Degree shr 1;
      if (Degree and 1) = 0 then // Degree 是偶数，计算 fn * (fn+2 * fn-1 ^ 2 - fn-2 * fn+1 ^ 2) / 2
      begin
        D1 := FLocalBigNumberPolynomialPool.Obtain;
        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N + 2, D1, Prime);

        D2 := FLocalBigNumberPolynomialPool.Obtain;        // D1 得到 fn+2
        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);
        BigNumberPolynomialGaloisMul(D2, D2, D2, Prime);   // D2 得到 fn-1 ^2

        BigNumberPolynomialGaloisMul(D1, D1, D2, Prime);   // D1 得到 fn+2 * fn-1 ^ 2

        D3 := FLocalBigNumberPolynomialPool.Obtain;
        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N - 2, D3, Prime);  // D3 得到 fn-2

        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N + 1, D2, Prime);
        BigNumberPolynomialGaloisMul(D2, D2, D2, Prime);   // D2 得到 fn+1^2
        BigNumberPolynomialGaloisMul(D2, D2, D3, Prime);   // D2 得到 fn-2 * fn+1^2

        BigNumberPolynomialGaloisSub(D1, D1, D2, Prime);   // D1 得到 fn+2 * fn-1^2 - fn-2 * fn+1^2

        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N, D2, Prime);    // D2 得到 fn
        BigNumberPolynomialGaloisMul(OutDivisionPolynomial, D2, D1, Prime);     // 相乘得到 f2n

        MI := FLocalBigNumberPool.Obtain;
        BigNumberModularInverseWord(MI, 2, Prime);
        BigNumberPolynomialGaloisMulBigNumber(OutDivisionPolynomial, MI, Prime);     // 再除以 2
      end
      else // Degree 是奇数
      begin
        Y4 := FLocalBigNumberPolynomialPool.Obtain;
        Y4.MaxDegree := 3;
        BigNumberCopy(Y4[0], B);
        BigNumberCopy(Y4[1], A);
        Y4[2].SetZero;
        Y4[3].SetOne;

        BigNumberPolynomialGaloisMul(Y4, Y4, Y4, Prime);

        D1 := FLocalBigNumberPolynomialPool.Obtain;
        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N + 2, D1, Prime); // D1 得到 fn+2

        D2 := FLocalBigNumberPolynomialPool.Obtain;
        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N, D2, Prime);
        BigNumberPolynomialGaloisPower(D2, D2, 3, Prime);                        // D2 得到 fn^3

        D3 := FLocalBigNumberPolynomialPool.Obtain;
        BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N + 1, D3, Prime);
        BigNumberPolynomialGaloisPower(D3, D3, 3, Prime);                        // D3 得到 fn+1^3

        if (N and 1) <> 0 then // N 是奇数，计算 f2n+1 = fn+2 * fn^3 - fn-1 * fn+1^3 * (x^3 + Ax + B)^2
        begin
          BigNumberPolynomialGaloisMul(D1, D1, D2, Prime);  // D1 得到 fn+2 * fn^3

          BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);
          BigNumberPolynomialGaloisMul(D2, D2, Y4, Prime);     // D2 得到 fn-1 * Y^4

          BigNumberPolynomialGaloisMul(D2, D2, D3, Prime);     // D2 得到 fn+1^3 * fn-1 * Y^4
          BigNumberPolynomialGaloisSub(OutDivisionPolynomial, D1, D2, Prime);
        end
        else // N 是偶数，计算 (x^3 + Ax + B)^2 * fn+2 * fn^3 - fn-1 * fn+1^3
        begin
          BigNumberPolynomialGaloisMul(D1, D1, D2, Prime);
          BigNumberPolynomialGaloisMul(D1, D1, Y4, Prime);   // D1 得到 Y^4 * fn+2 * fn^3

          BigNumberPolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);  // D2 得到 fn-1

          BigNumberPolynomialGaloisMul(D2, D2, D3, Prime);  // D2 得到 fn-1 * fn+1^3

          BigNumberPolynomialGaloisSub(OutDivisionPolynomial, D1, D2, Prime);
        end;
      end;
    finally
      FLocalBigNumberPolynomialPool.Recycle(D1);
      FLocalBigNumberPolynomialPool.Recycle(D2);
      FLocalBigNumberPolynomialPool.Recycle(D3);
      FLocalBigNumberPolynomialPool.Recycle(Y4);
      FLocalBigNumberPool.Recycle(MI);
    end;
    Result := True;
  end;
end;

procedure BigNumberPolynomialGaloisReduce2(P1, P2: TCnBigNumberPolynomial; Prime: TCnBigNumber);
var
  D: TCnBigNumberPolynomial;
begin
  if P1 = P2 then
  begin
    P1.SetOne;
    Exit;
  end;

  D := FLocalBigNumberPolynomialPool.Obtain;
  try
    if not BigNumberPolynomialGaloisGreatestCommonDivisor(D, P1, P2, Prime) then
      Exit;

    if not D.IsOne then
    begin
      BigNumberPolynomialGaloisDiv(P1, nil, P1, D, Prime);
      BigNumberPolynomialGaloisDiv(P1, nil, P1, D, Prime);
    end;
  finally
    FLocalBigNumberPolynomialPool.Recycle(D);
  end;
end;

{ TCnBigNumberRationalPolynomialPool }

function TCnBigNumberRationalPolynomialPool.CreateObject: TObject;
begin
  Result := TCnBigNumberRationalPolynomial.Create;
end;

function TCnBigNumberRationalPolynomialPool.Obtain: TCnBigNumberRationalPolynomial;
begin
  Result := TCnBigNumberRationalPolynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigNumberRationalPolynomialPool.Recycle(
  Poly: TCnBigNumberRationalPolynomial);
begin
  inherited Recycle(Poly);
end;

{ TCnBigComplexPolynomial }

procedure TCnBigComplexPolynomial.CorrectTop;
begin
  while (MaxDegree > 0) and Items[MaxDegree].IsZero do
    Delete(MaxDegree);
end;

constructor TCnBigComplexPolynomial.Create;
begin
  inherited Create;
  Add.SetZero;   // 常系数项
end;

constructor TCnBigComplexPolynomial.Create(LowToHighCoefficients: array of const);
begin
  inherited Create;
  SetCoefficents(LowToHighCoefficients);
end;

destructor TCnBigComplexPolynomial.Destroy;
begin

  inherited;
end;

procedure TCnBigComplexPolynomial.EnsureDegree(Degree: Integer);
begin
  if Degree > MaxDegree then
    SetMaxDegree(Degree);
end;

function TCnBigComplexPolynomial.GetMaxDegree: Integer;
begin
  if Count = 0 then
    Add.SetZero;
  Result := Count - 1;
end;

function TCnBigComplexPolynomial.IsMonic: Boolean;
begin
  Result := BigComplexPolynomialIsMonic(Self);
end;

function TCnBigComplexPolynomial.IsNegOne: Boolean;
begin
  Result := BigComplexPolynomialIsNegOne(Self);
end;

function TCnBigComplexPolynomial.IsOne: Boolean;
begin
  Result := BigComplexPolynomialIsOne(Self);
end;

function TCnBigComplexPolynomial.IsZero: Boolean;
begin
  Result := BigComplexPolynomialIsZero(Self);
end;

procedure TCnBigComplexPolynomial.Negate;
begin
  BigComplexPolynomialNegate(Self);
end;

procedure TCnBigComplexPolynomial.SetCoefficent(Degree: Integer;
  Coefficient: TCnBigComplex);
begin
  CheckDegree(Degree);
  EnsureDegree(Degree);
  BigComplexCopy(Items[Degree], Coefficient);
end;

procedure TCnBigComplexPolynomial.SetCoefficents(LowToHighCoefficients: array of const);
var
  I: Integer;
begin
  Clear;
  for I := Low(LowToHighCoefficients) to High(LowToHighCoefficients) do
  begin
    case LowToHighCoefficients[I].VType of
    vtInteger:
      begin
        Add.SetValue(LowToHighCoefficients[I].VInteger, 0);
      end;
    vtInt64:
      begin
        Add.SetValue(LowToHighCoefficients[I].VInt64^, 0);
      end;
    vtBoolean:
      begin
        if LowToHighCoefficients[I].VBoolean then
          Add.SetOne
        else
          Add.SetZero;
      end;
    vtString:
      begin
        Add.SetString(LowToHighCoefficients[I].VString^);
      end;
    vtObject:
      begin
        // 接受 TCnBigComplex 并从中复制值
        if LowToHighCoefficients[I].VObject is TCnBigComplex then
          BigComplexCopy(Add, LowToHighCoefficients[I].VObject as TCnBigComplex);
      end;
    else
      raise ECnPolynomialException.CreateFmt(SInvalidInteger, ['Coefficients ' + IntToStr(I)]);
    end;
  end;

  if Count = 0 then
    Add.SetZero
  else
    CorrectTop;
end;

procedure TCnBigComplexPolynomial.SetMaxDegree(const Value: Integer);
var
  I, OC: Integer;
begin
  CheckDegree(Value);

  OC := Count;
  Count := Value + 1; // 直接设置 Count，如变小，会自动释放多余的对象

  if Count > OC then  // 增加的部分创建新对象
  begin
    for I := OC to Count - 1 do
      Items[I] := TCnBigComplex.Create;
  end;
end;

procedure TCnBigComplexPolynomial.SetOne;
begin
  BigComplexPolynomialSetOne(Self);
end;

procedure TCnBigComplexPolynomial.SetZero;
begin
  BigComplexPolynomialSetZero(Self);
end;

function TCnBigComplexPolynomial.ToString: string;
begin
  Result := BigComplexPolynomialToString(Self);
end;

{ TCnBigComplexPolynomialPool }

function TCnBigComplexPolynomialPool.CreateObject: TObject;
begin
  Result := TCnBigComplexPolynomial.Create;
end;

function TCnBigComplexPolynomialPool.Obtain: TCnBigComplexPolynomial;
begin
  Result := TCnBigComplexPolynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigComplexPolynomialPool.Recycle(Poly: TCnBigComplexPolynomial);
begin
  inherited Recycle(Poly);
end;

{ TCnBigComplexDecimalPolynomial }

constructor TCnBigComplexDecimalPolynomial.Create;
begin
  inherited Create;
end;

constructor TCnBigComplexDecimalPolynomial.Create(LowToHighCoefficients: array of const);
begin
  inherited Create;
  SetCoefficents(LowToHighCoefficients);
end;

destructor TCnBigComplexDecimalPolynomial.Destroy;
begin
  inherited;
end;

procedure TCnBigComplexDecimalPolynomial.EnsureDegree(Degree: Integer);
begin
  if Degree > MaxDegree then
    SetMaxDegree(Degree);
end;

function TCnBigComplexDecimalPolynomial.GetMaxDegree: Integer;
begin
  if Count = 0 then
    Add.SetZero;
  Result := Count - 1;
end;

procedure TCnBigComplexDecimalPolynomial.SetMaxDegree(const Value: Integer);
var
  I, OC: Integer;
begin
  CheckDegree(Value);

  OC := Count;
  Count := Value + 1; // 直接设置 Count，如变小，会自动释放多余的对象

  if Count > OC then  // 增加的部分创建新对象
  begin
    for I := OC to Count - 1 do
      Items[I] := TCnBigComplexDecimal.Create;
  end;
end;

procedure TCnBigComplexDecimalPolynomial.SetCoefficents(LowToHighCoefficients: array of const);
var
  I: Integer;
begin
  Clear;
  for I := Low(LowToHighCoefficients) to High(LowToHighCoefficients) do
  begin
    case LowToHighCoefficients[I].VType of
    vtInteger:
      begin
        Add.SetValue(LowToHighCoefficients[I].VInteger, 0);
      end;
    vtInt64:
      begin
        Add.SetValue(LowToHighCoefficients[I].VInt64^, 0);
      end;
    vtBoolean:
      begin
        if LowToHighCoefficients[I].VBoolean then
          Add.SetOne
        else
          Add.SetZero;
      end;
    vtString:
      begin
        Add.SetString(LowToHighCoefficients[I].VString^);
      end;
    vtObject:
      begin
        // 接受 TCnBigComplexDecimal 并从中复制值
        if LowToHighCoefficients[I].VObject is TCnBigComplexDecimal then
          BigComplexDecimalCopy(Add, LowToHighCoefficients[I].VObject as TCnBigComplexDecimal);
      end;
    else
      raise ECnPolynomialException.CreateFmt(SInvalidInteger, ['Coefficients ' + IntToStr(I)]);
    end;
  end;

  if Count = 0 then
    Add.SetZero
  else
    CorrectTop;
end;

procedure TCnBigComplexDecimalPolynomial.SetCoefficent(Degree: Integer; Coefficient: TCnBigComplexDecimal);
begin
  EnsureDegree(Degree);
  BigComplexDecimalCopy(Items[Degree], Coefficient);
end;

procedure TCnBigComplexDecimalPolynomial.CorrectTop;
begin
  while (Count > 0) and TCnBigComplexDecimal(Items[Count - 1]).IsZero do
    Delete(Count - 1);
end;

function TCnBigComplexDecimalPolynomial.ToString: string;
begin
  Result := BigComplexDecimalPolynomialToString(Self);
end;

function TCnBigComplexDecimalPolynomial.IsZero: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if not Items[I].IsZero then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TCnBigComplexDecimalPolynomial.SetZero;
begin
  Clear;
end;

function TCnBigComplexDecimalPolynomial.IsOne: Boolean;
begin
  Result := (MaxDegree = 0) and Items[0].IsOne;
end;

procedure TCnBigComplexDecimalPolynomial.SetOne;
begin
  Clear;
  EnsureDegree(0);
  Items[0].SetOne;
end;

function TCnBigComplexDecimalPolynomial.IsNegOne: Boolean;
begin
  Result := (MaxDegree = 0) and Items[0].IsNegOne;
end;

procedure TCnBigComplexDecimalPolynomial.Negate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Negate;
end;

function TCnBigComplexDecimalPolynomial.IsMonic: Boolean;
begin
  CorrectTop;
  if Count = 0 then
    Result := False
  else
    Result := TCnBigComplexDecimal(Items[Count - 1]).IsOne;
end;

{ TCnBigComplexDecimalPolynomialPool }

function TCnBigComplexDecimalPolynomialPool.CreateObject: TObject;
begin
  Result := TCnBigComplexDecimalPolynomial.Create;
end;

function TCnBigComplexDecimalPolynomialPool.Obtain: TCnBigComplexDecimalPolynomial;
begin
  Result := TCnBigComplexDecimalPolynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigComplexDecimalPolynomialPool.Recycle(Poly: TCnBigComplexDecimalPolynomial);
begin
  inherited Recycle(Poly);
end;

// ================= 一元大整数复数多项式独立函数实现 ========================

function BigComplexPolynomialNew: TCnBigComplexPolynomial;
begin
  Result := TCnBigComplexPolynomial.Create;
end;

procedure BigComplexPolynomialFree(P: TCnBigComplexPolynomial);
begin
  P.Free;
end;

function BigComplexPolynomialDuplicate(P: TCnBigComplexPolynomial): TCnBigComplexPolynomial;
var
  I: Integer;
begin
  Result := TCnBigComplexPolynomial.Create;
  for I := 0 to P.Count - 1 do
  begin
    Result.EnsureDegree(I);
    BigComplexCopy(Result.Items[I], P.Items[I]);
  end;
end;

function BigComplexPolynomialCopy(Dest: TCnBigComplexPolynomial;
  Source: TCnBigComplexPolynomial): TCnBigComplexPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    Dest.Clear;
    for I := 0 to Source.Count - 1 do
    begin
      Dest.EnsureDegree(I);
      BigComplexCopy(Dest.Items[I], Source.Items[I]);
    end;
  end;
end;

procedure BigComplexPolynomialSwap(A: TCnBigComplexPolynomial;
  B: TCnBigComplexPolynomial);
var
  I, MaxCount: Integer;
  Temp: TCnBigComplex;
begin
  MaxCount := Max(A.Count, B.Count);

  // 确保两个多项式都有足够的项
  while A.Count < MaxCount do
  begin
    A.Add;
    TCnBigComplex(A.Items[A.Count - 1]).SetZero;
  end;

  while B.Count < MaxCount do
  begin
    B.Add;
    TCnBigComplex(B.Items[B.Count - 1]).SetZero;
  end;

  // 交换所有项
  for I := 0 to MaxCount - 1 do
  begin
    Temp := TCnBigComplex.Create;
    try
      BigComplexCopy(Temp, A.Items[I]);
      BigComplexCopy(A.Items[I], B.Items[I]);
      BigComplexCopy(B.Items[I], Temp);
    finally
      Temp.Free;
    end;
  end;
end;

function BigComplexPolynomialToString(P: TCnBigComplexPolynomial;
  const VarName: string): string;
var
  I: Integer;
begin
  P.CorrectTop;
  if P.Count = 0 then
  begin
    Result := '0';
    Exit;
  end;

  for I := P.Count - 1 downto 0 do
  begin
    if not P[I].IsZero then
    begin
      if Result <> '' then
        Result := Result + '+';
      if I = 0 then
        Result := Result + P[I].ToString
      else if I = 1 then
      begin
        if P[I].IsOne then
          Result := Result + 'X'
        else
        begin
          if P[I].IsPureReal then
            Result := Result + P[I].ToString + 'X'
          else
            Result := Result + '(' + P[I].ToString + ')X';
        end;
      end
      else
      begin
        if P[I].IsOne then
          Result := Result + 'X^' + IntToStr(I)
        else
        begin
          if P[I].IsPureReal then
            Result := Result + P[I].ToString + 'X^' + IntToStr(I)
          else
            Result := Result + '(' + P[I].ToString + ')X^' + IntToStr(I);
        end;
      end;
    end;
  end;
end;

function BigComplexPolynomialIsZero(P: TCnBigComplexPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsZero;
end;

procedure BigComplexPolynomialSetZero(P: TCnBigComplexPolynomial);
begin
  P.Clear;
  P.Add.SetZero;
end;

function BigComplexPolynomialIsOne(P: TCnBigComplexPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsOne;
end;

procedure BigComplexPolynomialSetOne(P: TCnBigComplexPolynomial);
begin
  P.Clear;
  P.Add.SetOne;
end;

function BigComplexPolynomialIsNegOne(P: TCnBigComplexPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsNegOne;
end;

procedure BigComplexPolynomialNegate(P: TCnBigComplexPolynomial);
var
  I: Integer;
begin
  for I := 0 to P.MaxDegree do
    P[I].Negate;
end;

function BigComplexPolynomialIsMonic(P: TCnBigComplexPolynomial): Boolean;
begin
  Result := P[P.MaxDegree].IsOne;
end;

procedure BigComplexPolynomialShiftLeft(P: TCnBigComplexPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigComplexPolynomialShiftRight(P, -N)
  else
    for I := 1 to N do
      P.Insert(0, TCnBigComplex.Create);
end;

procedure BigComplexPolynomialShiftRight(P: TCnBigComplexPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigComplexPolynomialShiftLeft(P, -N)
  else
  begin
    for I := 1 to N do
      P.Delete(0);

    if P.Count = 0 then
      P.Add.SetZero;
  end;
end;

function BigComplexPolynomialEqual(A: TCnBigComplexPolynomial;
  B: TCnBigComplexPolynomial): Boolean;
var
  I: Integer;
begin
  Result := False;
  if A.Count <> B.Count then
    Exit;

  for I := 0 to A.Count - 1 do
  begin
    if not BigComplexEqual(A.Items[I], B.Items[I]) then
      Exit;
  end;

  Result := True;
end;

function BigComplexPolynomialAdd(Res: TCnBigComplexPolynomial;
  P1: TCnBigComplexPolynomial; P2: TCnBigComplexPolynomial): Boolean;
var
  I, D1, D2: Integer;
  PBig: TCnBigComplexPolynomial;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then
      PBig := P1
    else
      PBig := P2;

    Res.MaxDegree := D1; // 考虑到 Res 可能是 P1 或 P2，所以给 Res 的 MaxDegree 赋值得放上面的比较之后
    for I := D1 downto D2 + 1 do
      BigComplexCopy(Res[I], PBig[I]);
  end
  else // D1 = D2 说明俩加式同次
    Res.MaxDegree := D1;

  for I := D2 downto 0 do
    BigComplexAdd(Res[I], P1[I], P2[I]);

  Res.CorrectTop;
  Result := True;
end;

function BigComplexPolynomialSub(Res: TCnBigComplexPolynomial;
  P1: TCnBigComplexPolynomial; P2: TCnBigComplexPolynomial): Boolean;
var
  I, D1, D2: Integer;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  Res.MaxDegree := D1;
  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then // 被减式大
    begin
      for I := D1 downto D2 + 1 do
        BigComplexCopy(Res[I], P1[I]);
    end
    else  // 减式大
    begin
      for I := D1 downto D2 + 1 do
      begin
        BigComplexCopy(Res[I], P2[I]);
        Res[I].Negate;
      end;
    end;
  end;

  for I := D2 downto 0 do
    BigComplexSub(Res[I], P1[I], P2[I]);

  Res.CorrectTop;
  Result := True;
end;

function BigComplexPolynomialMul(Res: TCnBigComplexPolynomial;
  P1: TCnBigComplexPolynomial; P2: TCnBigComplexPolynomial): Boolean;
var
  R: TCnBigComplexPolynomial;
  T: TCnBigComplex;
  I, J: Integer;
begin
  if BigComplexPolynomialIsZero(P1) or BigComplexPolynomialIsZero(P2) then
  begin
    BigComplexPolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  T := FLocalBigComplexPool.Obtain;
  if (Res = P1) or (Res = P2) then
    R := FLocalBigComplexPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxDegree := P1.MaxDegree + P2.MaxDegree;

  for I := 0 to P1.MaxDegree do
  begin
    // 把第 I 次方的数字乘以 P2 的每一个数字，加到结果的 I 开头的部分
    for J := 0 to P2.MaxDegree do
    begin
      BigComplexMul(T, P1[I], P2[J]);
      BigComplexAdd(R[I + J], R[I + J], T);
    end;
  end;

  R.CorrectTop;
  if (Res = P1) or (Res = P2) then
  begin
    BigComplexPolynomialCopy(Res, R);
    FLocalBigComplexPolynomialPool.Recycle(R);
  end;

  FLocalBigComplexPool.Recycle(T);
  Result := True;
end;

procedure BigComplexPolynomialMulBigComplex(P: TCnBigComplexPolynomial;
  N: TCnBigComplex);
var
  I: Integer;
  T: TCnBigComplex;
begin
  if BigComplexIsZero(N) then
    BigComplexPolynomialSetZero(P)
  else if not N.IsOne then
  begin
    T := FLocalBigComplexPool.Obtain;
    try
      for I := 0 to P.MaxDegree do
      begin
        BigComplexMul(T, P[I], N);
        BigComplexCopy(P[I], T);
      end;
    finally
      FLocalBigComplexPool.Recycle(T);
    end;
  end;
end;

function BigComplexPolynomialPower(Res: TCnBigComplexPolynomial;
  P: TCnBigComplexPolynomial; Exponent: Int64): Boolean;
var
  Temp, Temp2: TCnBigComplexPolynomial;
  I: Int64;
begin
  Result := True;

  if Exponent < 0 then
  begin
    Result := False;
    Exit;
  end;

  if Exponent = 0 then
  begin
    Res.SetOne;
    Exit;
  end;

  if Exponent = 1 then
  begin
    BigComplexPolynomialCopy(Res, P);
    Exit;
  end;

  Temp := TCnBigComplexPolynomial.Create;
  Temp2 := TCnBigComplexPolynomial.Create;
  try
    BigComplexPolynomialCopy(Temp, P);
    Res.SetOne;

    I := Exponent;
    while I > 0 do
    begin
      if I and 1 = 1 then
      begin
        BigComplexPolynomialMul(Temp2, Res, Temp);
        BigComplexPolynomialCopy(Res, Temp2);
      end;

      BigComplexPolynomialMul(Temp2, Temp, Temp);
      BigComplexPolynomialCopy(Temp, Temp2);
      I := I shr 1;
    end;
  finally
    Temp.Free;
    Temp2.Free;
  end;
end;

function BigComplexPolynomialCompose(Res: TCnBigComplexPolynomial;
  F: TCnBigComplexPolynomial; P: TCnBigComplexPolynomial): Boolean;
var
  I: Integer;
  Temp, Temp2, PowerP: TCnBigComplexPolynomial;
begin
  Result := True;

  if F.IsZero then
  begin
    Res.SetZero;
    Exit;
  end;

  Temp := TCnBigComplexPolynomial.Create;
  Temp2 := TCnBigComplexPolynomial.Create;
  PowerP := TCnBigComplexPolynomial.Create;
  try
    Res.SetZero;
    PowerP.SetOne;

    for I := 0 to F.MaxDegree do
    begin
      if I > 0 then
      begin
        BigComplexPolynomialMul(Temp2, PowerP, P);
        BigComplexPolynomialCopy(PowerP, Temp2);
      end;

      Temp.Clear;
      Temp.EnsureDegree(0);
      BigComplexCopy(Temp.Items[0], F.Items[I]);

      BigComplexPolynomialMul(Temp2, Temp, PowerP);
      BigComplexPolynomialAdd(Res, Res, Temp2);
    end;
  finally
    Temp.Free;
    Temp2.Free;
    PowerP.Free;
  end;
end;

procedure BigComplexPolynomialGetValue(Res: TCnBigComplex; F: TCnBigComplexPolynomial;
  X: TCnBigComplex);
var
  I: Integer;
  T, M: TCnBigComplex;
begin
  BigComplexCopy(Res, F[0]);
  if X.IsZero or (F.MaxDegree = 0) then    // 只有常数项的情况下，得常数项
    Exit;

  T := FLocalBigComplexPool.Obtain;
  M := FLocalBigComplexPool.Obtain;

  try
    BigComplexCopy(T, X);

    // 把 F 中的每个系数都和 X 的对应次幂相乘，最后相加
    for I := 1 to F.MaxDegree do
    begin
      BigComplexMul(M, F[I], T);
      BigComplexAdd(Res, Res, M);

      if I <> F.MaxDegree then
        BigComplexMul(T, T, X);
    end;
  finally
    FLocalBigComplexPool.Recycle(T);
    FLocalBigComplexPool.Recycle(M);
  end;
end;

// ======================= 一元大整系数有理分式常规运算 ============================

function BigNumberRationalPolynomialEqual(R1, R2: TCnBigNumberRationalPolynomial): Boolean;
var
  T1, T2: TCnBigNumberPolynomial;
begin
  if R1 = R2 then
  begin
    Result := True;
    Exit;
  end;

  if R1.IsInt and R2.IsInt then
  begin
    Result := BigNumberPolynomialEqual(R1.Numerator, R2.Numerator);
    Exit;
  end;

  T1 := FLocalBigNumberPolynomialPool.Obtain;
  T2 := FLocalBigNumberPolynomialPool.Obtain;

  try
    // 判断分子分母互相乘的结果是否相等
    BigNumberPolynomialMul(T1, R1.Numerator, R2.Denominator);
    BigNumberPolynomialMul(T2, R2.Numerator, R1.Denominator);
    Result := BigNumberPolynomialEqual(T1, T2);
  finally
    FLocalBigNumberPolynomialPool.Recycle(T2);
    FLocalBigNumberPolynomialPool.Recycle(T1);
  end;
end;

function BigNumberRationalPolynomialCopy(Dest: TCnBigNumberRationalPolynomial;
  Source: TCnBigNumberRationalPolynomial): TCnBigNumberRationalPolynomial;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    BigNumberPolynomialCopy(Dest.Numerator, Source.Numerator);
    BigNumberPolynomialCopy(Dest.Denominator, Source.Denominator);
  end;
end;

procedure BigNumberRationalPolynomialAdd(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial); overload;
var
  M, R, F1, F2, D1, D2: TCnBigNumberPolynomial;
begin
  if R1.IsInt and R2.IsInt then
  begin
    BigNumberPolynomialAdd(Res.Numerator, R1.Numerator, R2.Numerator);
    Res.Denominator.SetOne;
    Exit;
  end
  else if R1.IsZero then
  begin
    if R2 <> Res then
      Res.Assign(R2);
  end
  else if R2.IsZero then
  begin
    if R1 <> Res then
      Res.Assign(R1);
  end
  else
  begin
    M := nil;
    R := nil;
    F1 := nil;
    F2 := nil;
    D1 := nil;
    D2 := nil;

    try
      // 求分母的最小公倍数
      M := FLocalBigNumberPolynomialPool.Obtain;
      R := FLocalBigNumberPolynomialPool.Obtain;
      F1 := FLocalBigNumberPolynomialPool.Obtain;
      F2 := FLocalBigNumberPolynomialPool.Obtain;
      D1 := FLocalBigNumberPolynomialPool.Obtain;
      D2 := FLocalBigNumberPolynomialPool.Obtain;

      BigNumberPolynomialCopy(D1, R1.Denominator);
      BigNumberPolynomialCopy(D2, R2.Denominator);

      if not BigNumberPolynomialLeastCommonMultiple(M, D1, D2) then
        BigNumberPolynomialMul(M, D1, D2);   // 无法求最小公倍式表示系数无法整除，直接相乘

      BigNumberPolynomialDiv(F1, R, M, D1);
      BigNumberPolynomialDiv(F2, R, M, D2);

      BigNumberPolynomialCopy(Res.Denominator, M);
      BigNumberPolynomialMul(R, R1.Numerator, F1);
      BigNumberPolynomialMul(M, R2.Numerator, F2);
      BigNumberPolynomialAdd(Res.Numerator, R, M);
    finally
      FLocalBigNumberPolynomialPool.Recycle(M);
      FLocalBigNumberPolynomialPool.Recycle(R);
      FLocalBigNumberPolynomialPool.Recycle(F1);
      FLocalBigNumberPolynomialPool.Recycle(F2);
      FLocalBigNumberPolynomialPool.Recycle(D1);
      FLocalBigNumberPolynomialPool.Recycle(D2);
    end;
  end;
end;

procedure BigNumberRationalPolynomialSub(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial); overload;
begin
  R2.Numerator.Negate;
  BigNumberRationalPolynomialAdd(Res, R1, R2);
  if Res <> R2 then
    R2.Numerator.Negate;
end;

procedure BigNumberRationalPolynomialMul(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial); overload;
begin
  BigNumberPolynomialMul(Res.Numerator, R1.Numerator, R2.Numerator);
  BigNumberPolynomialMul(Res.Denominator, R1.Denominator, R2.Denominator);
end;

procedure BigNumberRationalPolynomialDiv(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial); overload;
var
  N: TCnBigNumberPolynomial;
begin
  if R2.IsZero then
    raise EDivByZero.Create(SDivByZero);

  N := FLocalBigNumberPolynomialPool.Obtain; // 交叉相乘，必须用中间变量，防止 Res 是 Number1 或 Number 2
  try
    BigNumberPolynomialMul(N, R1.Numerator, R2.Denominator);
    BigNumberPolynomialMul(Res.Denominator, R1.Denominator, R2.Numerator);
    BigNumberPolynomialCopy(Res.Numerator, N);
  finally
    FLocalBigNumberPolynomialPool.Recycle(N);
  end;
end;

procedure BigNumberRationalPolynomialAddBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialAdd(R, R, P);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialSubBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialSub(R, R, P);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialMulBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialMul(R, R, P);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialDivBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialDiv(R, R, P);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialAdd(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
var
  T: TCnBigNumberRationalPolynomial;
begin
  if P1.IsZero then
  begin
    if R1 <> Res then
    begin
      BigNumberRationalPolynomialCopy(Res, R1);
      Exit;
    end;
  end;

  T := FLocalBigNumberRationalPolynomialPool.Obtain;
  try
    T.Denominator.SetOne;
    BigNumberPolynomialCopy(T.Numerator, P1);
    BigNumberRationalPolynomialAdd(Res, R1, T);
  finally
    FLocalBigNumberRationalPolynomialPool.Recycle(T);
  end;
end;

procedure BigNumberRationalPolynomialSub(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
begin
  P1.Negate;
  try
    BigNumberRationalPolynomialAdd(Res, R1, P1);
  finally
    P1.Negate;
  end;
end;

procedure BigNumberRationalPolynomialMul(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
begin
  if P1.IsZero then
    Res.SetZero
  else if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    BigNumberPolynomialMul(Res.Numerator, R1.Numerator, P1);
    BigNumberPolynomialCopy(Res.Denominator, R1.Denominator);
  end;
end;

procedure BigNumberRationalPolynomialDiv(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial); overload;
begin
  if P1.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    BigNumberPolynomialMul(Res.Denominator, R1.Denominator, P1);
    BigNumberPolynomialCopy(Res.Numerator, R1.Numerator);
  end;
end;

function BigNumberRationalPolynomialCompose(Res: TCnBigNumberRationalPolynomial;
  F, P: TCnBigNumberRationalPolynomial): Boolean;
var
  RN, RD: TCnBigNumberRationalPolynomial;
begin
  if P.IsInt then
    Result := BigNumberRationalPolynomialCompose(Res, F, P.Numerator)
  else
  begin
    RD := FLocalBigNumberRationalPolynomialPool.Obtain;
    RN := FLocalBigNumberRationalPolynomialPool.Obtain;

    try
      BigNumberRationalPolynomialCompose(RN, F.Numerator, P);
      BigNumberRationalPolynomialCompose(RD, F.Denominator, P);

      BigNumberPolynomialMul(Res.Numerator, RN.Numerator, RD.Denominator);
      BigNumberPolynomialMul(Res.Denominator, RN.Denominator, RD.Numerator);
      Result := True;
    finally
      FLocalBigNumberRationalPolynomialPool.Recycle(RN);
      FLocalBigNumberRationalPolynomialPool.Recycle(RD);
    end;
  end;
end;

function BigNumberRationalPolynomialCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberRationalPolynomial; P: TCnBigNumberPolynomial): Boolean;
begin
  BigNumberPolynomialCompose(Res.Numerator, F.Numerator, P);
  BigNumberPolynomialCompose(Res.Denominator, F.Denominator, P);
  Result := True;
end;

function BigNumberRationalPolynomialCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberPolynomial; P: TCnBigNumberRationalPolynomial): Boolean;
var
  I: Integer;
  R, X, T: TCnBigNumberRationalPolynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    Res.Numerator[0] := F[0];
    Result := True;
    Exit;
  end;

  if Res = P then
    R := FLocalBigNumberRationalPolynomialPool.Obtain
  else
    R := Res;

  X := FLocalBigNumberRationalPolynomialPool.Obtain;
  T := FLocalBigNumberRationalPolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      BigNumberRationalPolynomialCopy(T, X);
      BigNumberRationalPolynomialMulBigNumber(T, F[I]);
      BigNumberRationalPolynomialAdd(R, T, R);

      if I <> F.MaxDegree then
        BigNumberRationalPolynomialMul(X, P, X);
    end;

    if Res = P then
    begin
      BigNumberRationalPolynomialCopy(Res, R);
      FLocalBigNumberRationalPolynomialPool.Recycle(R);
    end;
  finally
    FLocalBigNumberRationalPolynomialPool.Recycle(X);
    FLocalBigNumberRationalPolynomialPool.Recycle(T);
  end;
  Result := True;
end;

procedure BigNumberRationalPolynomialGetValue(Res: TCnBigRational;
  F: TCnBigNumberRationalPolynomial; X: TCnBigNumber);
begin
  BigNumberPolynomialGetValue(Res.Numerator, F.Numerator, X);
  BigNumberPolynomialGetValue(Res.Denominator, F.Denominator, X);
  Res.Reduce;
end;

// ================== 一元大整系数有理分式在有限域上的模运算 ===================

function BigNumberRationalPolynomialGaloisEqual(R1, R2: TCnBigNumberRationalPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial = nil): Boolean;
var
  T1, T2: TCnBigNumberPolynomial;
begin
  if R1 = R2 then
  begin
    Result := True;
    Exit;
  end;

  T1 := FLocalBigNumberPolynomialPool.Obtain;
  T2 := FLocalBigNumberPolynomialPool.Obtain;

  try
    // 判断分子分母互相乘的结果是否相等
    BigNumberPolynomialGaloisMul(T1, R1.Numerator, R2.Denominator, Prime, Primitive);
    BigNumberPolynomialGaloisMul(T2, R2.Numerator, R1.Denominator, Prime, Primitive);
    Result := BigNumberPolynomialGaloisEqual(T1, T2, Prime);
  finally
    FLocalBigNumberPolynomialPool.Recycle(T2);
    FLocalBigNumberPolynomialPool.Recycle(T1);
  end;
end;

procedure BigNumberRationalPolynomialGaloisNegate(P: TCnBigNumberRationalPolynomial;
  Prime: TCnBigNumber);
begin
  BigNumberPolynomialGaloisNegate(P.Numerator, Prime);
end;

procedure BigNumberRationalPolynomialGaloisAdd(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber); overload;
var
  M, R, F1, F2, D1, D2: TCnBigNumberPolynomial;
begin
  if R1.IsInt and R2.IsInt then
  begin
    BigNumberPolynomialGaloisAdd(Res.Numerator, R1.Numerator,
      R2.Numerator, Prime);
    Res.Denominator.SetOne;
    Exit;
  end
  else if R1.IsZero then
  begin
    if R2 <> Res then
      Res.Assign(R2);
  end
  else if R2.IsZero then
  begin
    if R1 <> Res then
      Res.Assign(R1);
  end
  else
  begin
    M := nil;
    R := nil;
    F1 := nil;
    F2 := nil;
    D1 := nil;
    D2 := nil;

    try
      // 求分母的最小公倍数
      M := FLocalBigNumberPolynomialPool.Obtain;
      R := FLocalBigNumberPolynomialPool.Obtain;
      F1 := FLocalBigNumberPolynomialPool.Obtain;
      F2 := FLocalBigNumberPolynomialPool.Obtain;
      D1 := FLocalBigNumberPolynomialPool.Obtain;
      D2 := FLocalBigNumberPolynomialPool.Obtain;

      BigNumberPolynomialCopy(D1, R1.Denominator);
      BigNumberPolynomialCopy(D2, R2.Denominator);

      if not BigNumberPolynomialGaloisLeastCommonMultiple(M, D1, D2, Prime) then
        BigNumberPolynomialGaloisMul(M, D1, D2, Prime);   // 无法求最小公倍式表示系数无法整除，直接相乘

      BigNumberPolynomialGaloisDiv(F1, R, M, D1, Prime);  // 最小公倍数 M div D1 结果放 F1
      BigNumberPolynomialGaloisDiv(F2, R, M, D2, Prime);  // 最小公倍数 M div D2 结果放 F2

      BigNumberPolynomialCopy(Res.Denominator, M);  // 结果的分母是最小公倍数
      BigNumberPolynomialGaloisMul(R, R1.Numerator, F1, Prime);
      BigNumberPolynomialGaloisMul(M, R2.Numerator, F2, Prime);
      BigNumberPolynomialGaloisAdd(Res.Numerator, R, M, Prime);
    finally
      FLocalBigNumberPolynomialPool.Recycle(M);
      FLocalBigNumberPolynomialPool.Recycle(R);
      FLocalBigNumberPolynomialPool.Recycle(F1);
      FLocalBigNumberPolynomialPool.Recycle(F2);
      FLocalBigNumberPolynomialPool.Recycle(D1);
      FLocalBigNumberPolynomialPool.Recycle(D2);
    end;
  end;
end;

procedure BigNumberRationalPolynomialGaloisSub(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber); overload;
begin
  R2.Numerator.Negate;
  BigNumberRationalPolynomialGaloisAdd(Res, R1, R2, Prime);
  if Res <> R2 then
    R2.Numerator.Negate;
end;

procedure BigNumberRationalPolynomialGaloisMul(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber); overload;
begin
  BigNumberPolynomialGaloisMul(Res.Numerator, R1.Numerator, R2.Numerator, Prime);
  BigNumberPolynomialGaloisMul(Res.Denominator, R1.Denominator, R2.Denominator, Prime);
end;

procedure BigNumberRationalPolynomialGaloisDiv(Res: TCnBigNumberRationalPolynomial;
  R1, R2: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber); overload;
var
  N: TCnBigNumberPolynomial;
begin
  if R2.IsZero then
    raise EDivByZero.Create(SDivByZero);

  N := FLocalBigNumberPolynomialPool.Obtain; // 交叉相乘，必须用中间变量，防止 Res 是 Number1 或 Number 2
  try
    BigNumberPolynomialGaloisMul(N, R1.Numerator, R2.Denominator, Prime);
    BigNumberPolynomialGaloisMul(Res.Denominator, R1.Denominator, R2.Numerator, Prime);
    BigNumberPolynomialCopy(Res.Numerator, N);
  finally
    FLocalBigNumberPolynomialPool.Recycle(N);
  end;
end;

procedure BigNumberRationalPolynomialGaloisAddBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialGaloisAdd(R, R, P, Prime);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialGaloisSubBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialGaloisSub(R, R, P, Prime);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialGaloisMulBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialGaloisMul(R, R, P, Prime);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialGaloisDivBigNumber(R: TCnBigNumberRationalPolynomial;
  Num: TCnBigNumber; Prime: TCnBigNumber);
var
  P: TCnBigNumberPolynomial;
begin
  P := FLocalBigNumberPolynomialPool.Obtain;
  try
    P.MaxDegree := 0;
    BigNumberCopy(P[0], Num);
    BigNumberRationalPolynomialGaloisDiv(R, R, P, Prime);
  finally
    FLocalBigNumberPolynomialPool.Recycle(P);
  end;
end;

procedure BigNumberRationalPolynomialGaloisAdd(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial; Prime: TCnBigNumber); overload;
var
  T: TCnBigNumberRationalPolynomial;
begin
  if P1.IsZero then
  begin
    if R1 <> Res then
    begin
      BigNumberRationalPolynomialCopy(Res, R1);
      Exit;
    end;
  end;

  T := FLocalBigNumberRationalPolynomialPool.Obtain;
  try
    T.Denominator.SetOne;
    BigNumberPolynomialCopy(T.Numerator, P1);
    BigNumberRationalPolynomialGaloisAdd(Res, R1, T, Prime);
  finally
    FLocalBigNumberRationalPolynomialPool.Recycle(T);
  end;
end;

procedure BigNumberRationalPolynomialGaloisSub(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial; Prime: TCnBigNumber); overload;
begin
  P1.Negate;
  try
    BigNumberRationalPolynomialGaloisAdd(Res, R1, P1, Prime);
  finally
    P1.Negate;
  end;
end;

procedure BigNumberRationalPolynomialGaloisMul(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial; Prime: TCnBigNumber); overload;
begin
  if P1.IsZero then
    Res.SetZero
  else if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    BigNumberPolynomialGaloisMul(Res.Numerator, R1.Numerator, P1, Prime);
    BigNumberPolynomialCopy(Res.Denominator, R1.Denominator);
  end;
end;

procedure BigNumberRationalPolynomialGaloisDiv(Res: TCnBigNumberRationalPolynomial;
  R1: TCnBigNumberRationalPolynomial; P1: TCnBigNumberPolynomial; Prime: TCnBigNumber);
begin
  if P1.IsZero or Prime.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if P1.IsOne then
    Res.Assign(R1)
  else
  begin
    BigNumberPolynomialGaloisMul(Res.Denominator, R1.Denominator, P1, Prime);
    BigNumberPolynomialCopy(Res.Numerator, R1.Numerator);
  end;
end;

function BigNumberRationalPolynomialGaloisCompose(Res: TCnBigNumberRationalPolynomial;
  F, P: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberPolynomial): Boolean;
var
  RN, RD: TCnBigNumberRationalPolynomial;
begin
  if P.IsInt then
    Result := BigNumberRationalPolynomialGaloisCompose(Res, F, P.Numerator, Prime, Primitive)
  else
  begin
    RD := FLocalBigNumberRationalPolynomialPool.Obtain;
    RN := FLocalBigNumberRationalPolynomialPool.Obtain;

    try
      BigNumberRationalPolynomialGaloisCompose(RN, F.Numerator, P, Prime, Primitive);
      BigNumberRationalPolynomialGaloisCompose(RD, F.Denominator, P, Prime, Primitive);

      BigNumberPolynomialGaloisMul(Res.Numerator, RN.Numerator, RD.Denominator, Prime);
      BigNumberPolynomialGaloisMul(Res.Denominator, RN.Denominator, RD.Numerator, Prime);

      if Primitive <> nil then
      begin
        BigNumberPolynomialGaloisMod(Res.Numerator, Res.Numerator, Primitive, Prime);
        BigNumberPolynomialGaloisMod(Res.Denominator, Res.Denominator, Primitive, Prime);
      end;
      Result := True;
    finally
      FLocalBigNumberRationalPolynomialPool.Recycle(RN);
      FLocalBigNumberRationalPolynomialPool.Recycle(RD);
    end;
  end;
end;

function BigNumberRationalPolynomialGaloisCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberRationalPolynomial; P: TCnBigNumberPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial): Boolean;
begin
  BigNumberPolynomialGaloisCompose(Res.Numerator, F.Numerator, P, Prime, Primitive);
  BigNumberPolynomialGaloisCompose(Res.Denominator, F.Denominator, P, Prime, Primitive);
  Result := True;
end;

function BigNumberRationalPolynomialGaloisCompose(Res: TCnBigNumberRationalPolynomial;
  F: TCnBigNumberPolynomial; P: TCnBigNumberRationalPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberPolynomial): Boolean;
var
  I: Integer;
  R, X, T: TCnBigNumberRationalPolynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    BigNumberNonNegativeMod(Res.Numerator[0], F[0], Prime);
    Result := True;
    Exit;
  end;

  if Res = P then
    R := FLocalBigNumberRationalPolynomialPool.Obtain
  else
    R := Res;

  X := FLocalBigNumberRationalPolynomialPool.Obtain;
  T := FLocalBigNumberRationalPolynomialPool.Obtain;

  try
    X.SetOne;
    R.SetZero;

    // 把 F 中的每个系数都和 P 的对应次幂相乘，最后相加
    for I := 0 to F.MaxDegree do
    begin
      BigNumberRationalPolynomialCopy(T, X);
      BigNumberRationalPolynomialGaloisMulBigNumber(T, F[I], Prime);
      BigNumberRationalPolynomialGaloisAdd(R, T, R, Prime);

      if I <> F.MaxDegree then
        BigNumberRationalPolynomialGaloisMul(X, P, X, Prime);
    end;

    if Primitive <> nil then
    begin
      BigNumberPolynomialGaloisMod(R.Numerator, R.Numerator, Primitive, Prime);
      BigNumberPolynomialGaloisMod(R.Denominator, R.Denominator, Primitive, Prime);
    end;

    if Res = P then
    begin
      BigNumberRationalPolynomialCopy(Res, R);
      FLocalBigNumberRationalPolynomialPool.Recycle(R);
    end;
  finally
    FLocalBigNumberRationalPolynomialPool.Recycle(X);
    FLocalBigNumberRationalPolynomialPool.Recycle(T);
  end;
  Result := True;
end;

procedure BigNumberRationalPolynomialGaloisGetValue(Res: TCnBigNumber;
  F: TCnBigNumberRationalPolynomial; X: TCnBigNumber; Prime: TCnBigNumber);
var
  N, D, T: TCnBigNumber;
begin
  D := nil;
  N := nil;
  T := nil;

  try
    D := FLocalBigNumberPool.Obtain;
    BigNumberPolynomialGaloisGetValue(D, F.Denominator, X, Prime);
    if D.IsZero then
      raise EDivByZero.Create(SDivByZero);

    N := FLocalBigNumberPool.Obtain;
    BigNumberPolynomialGaloisGetValue(N, F.Numerator, X, Prime);

    T := FLocalBigNumberPool.Obtain;
    BigNumberModularInverse(T, D, Prime);
    BigNumberMul(N, T, N);
    BigNumberNonNegativeMod(Res, N, Prime);
  finally
    FLocalBigNumberPool.Recycle(D);
    FLocalBigNumberPool.Recycle(N);
    FLocalBigNumberPool.Recycle(T);
  end;
end;

{ TCnInt64BiPolynomial }

procedure TCnInt64BiPolynomial.CorrectTop;
var
  I: Integer;
  Compact, MeetNonEmpty: Boolean;
  YL: TCnInt64List;
begin
  MeetNonEmpty := False;
  for I := FXs.Count - 1 downto 0 do
  begin
    YL := TCnInt64List(FXs[I]);
    Compact := CompactYDegree(YL);

    if not Compact then     // 本次压缩非 0
      MeetNonEmpty := True;

    if Compact and not MeetNonEmpty then // 最高的一路下来压缩出来全 0 的要删掉
    begin
      FXs.Delete(I);
      YL.Free;
    end;
  end;
end;

function TCnInt64BiPolynomial.CompactYDegree(YList: TCnInt64List): Boolean;
var
  I: Integer;
begin
  for I := YList.Count - 1 downto 0 do
  begin
    if YList[I] = 0 then
      YList.Delete(I)
    else
      Break;
  end;

  Result := YList.Count = 0;
end;

constructor TCnInt64BiPolynomial.Create(XDegree, YDegree: Integer);
begin
  FXs := TObjectList.Create(False);
  EnsureDegrees(XDegree, YDegree);
end;

destructor TCnInt64BiPolynomial.Destroy;
var
  I: Integer;
begin
  for I := FXs.Count - 1 downto 0 do
    FXs[I].Free;
  FXs.Free;
  inherited;
end;

procedure TCnInt64BiPolynomial.EnsureDegrees(XDegree, YDegree: Integer);
var
  I, OldCount: Integer;
begin
  CheckDegree(XDegree);
  CheckDegree(YDegree);

  OldCount := FXs.Count;
  if (XDegree + 1) > FXs.Count then
  begin
    for I := FXs.Count + 1 to XDegree + 1 do
    begin
      FXs.Add(TCnInt64List.Create);
      TCnInt64List(FXs[FXs.Count - 1]).Count := YDegree + 1;
    end;
  end;

  for I:= OldCount - 1 downto 0 do
    if TCnInt64List(FXs[I]).Count < YDegree + 1 then
      TCnInt64List(FXs[I]).Count := YDegree + 1;
end;

function TCnInt64BiPolynomial.GetMaxXDegree: Integer;
begin
  Result := FXs.Count - 1;
end;

function TCnInt64BiPolynomial.GetMaxYDegree: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := FXs.Count - 1 downto 0 do
    if YFactorsList[I].Count - 1 > Result then
      Result := YFactorsList[I].Count - 1;
end;

function TCnInt64BiPolynomial.GetYFactorsList(
  Index: Integer): TCnInt64List;
begin
  if (Index < 0) or (Index >= FXs.Count) then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidDegree, [Index]);

  Result := TCnInt64List(FXs[Index]);
end;

function TCnInt64BiPolynomial.IsZero: Boolean;
begin
  Result := Int64BiPolynomialIsZero(Self);
end;

procedure TCnInt64BiPolynomial.Negate;
begin
  Int64BiPolynomialNegate(Self);
end;

procedure TCnInt64BiPolynomial.SetMaxXDegree(const Value: Integer);
var
  I: Integer;
begin
  CheckDegree(Value);

  if Value + 1 > FXs.Count then
  begin
    for I := FXs.Count + 1 to Value + 1 do
      FXs.Add(TCnInt64List.Create);
  end
  else if Value + 1 < FXs.Count then
  begin
    for I := FXs.Count - 1 downto Value + 1 do
    begin
      FXs[I].Free;
      FXs.Delete(I);
    end;
  end;
end;

procedure TCnInt64BiPolynomial.SetMaxYDegree(const Value: Integer);
var
  I: Integer;
begin
  CheckDegree(Value);

  for I := FXs.Count - 1 downto 0 do
    TCnInt64List(FXs[I]).Count := Value + 1;
end;

procedure TCnInt64BiPolynomial.SetString(const Poly: string);
begin
  Int64BiPolynomialSetString(Self, Poly);
end;

procedure TCnInt64BiPolynomial.SetZero;
begin
  Int64BiPolynomialSetZero(Self);
end;

function TCnInt64BiPolynomial.ToString: string;
begin
  Result := Int64BiPolynomialToString(Self);
end;

function Int64BiPolynomialNew: TCnInt64BiPolynomial;
begin
  Result := TCnInt64BiPolynomial.Create;
end;

procedure Int64BiPolynomialFree(P: TCnInt64BiPolynomial);
begin
  P.Free;
end;

function Int64BiPolynomialDuplicate(P: TCnInt64BiPolynomial): TCnInt64BiPolynomial;
begin
  if P = nil then
  begin
    Result := nil;
    Exit;
  end;

  Result := Int64BiPolynomialNew;
  if Result <> nil then
    Int64BiPolynomialCopy(Result, P);
end;

function Int64BiPolynomialCopy(Dest: TCnInt64BiPolynomial;
  Source: TCnInt64BiPolynomial): TCnInt64BiPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    if Source.MaxXDegree >= 0 then
    begin
      Dest.MaxXDegree := Source.MaxXDegree;
      for I := 0 to Source.MaxXDegree do
        CnInt64ListCopy(Dest.YFactorsList[I], Source.YFactorsList[I]);
    end
    else
      Dest.SetZero; // 如果 Source 未初始化，则 Dest 也清零
  end;
end;

function Int64BiPolynomialCopyFromX(Dest: TCnInt64BiPolynomial;
  SrcX: TCnInt64Polynomial): TCnInt64BiPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  Dest.Clear;

  Dest.MaxXDegree := SrcX.MaxDegree;
  for I := 0 to SrcX.MaxDegree do
    Dest.SafeValue[I, 0] := SrcX[I]; // 给每一个 YList 的首元素设值
end;

function Int64BiPolynomialCopyFromY(Dest: TCnInt64BiPolynomial;
  SrcY: TCnInt64Polynomial): TCnInt64BiPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  Dest.Clear;

  for I := 0 to SrcY.MaxDegree do
    Dest.YFactorsList[0].Add(SrcY[I]); // 给最低一个 YList 的所有元素设值
end;

function Int64BiPolynomialToString(P: TCnInt64BiPolynomial;
  const Var1Name: string; const Var2Name: string): string;
var
  I, J: Integer;
  YL: TCnInt64List;
begin
  Result := '';
  for I := P.FXs.Count - 1 downto 0 do
  begin
    YL := TCnInt64List(P.FXs[I]);
    for J := YL.Count - 1 downto 0 do
    begin
      if VarItemFactor(Result, (J = 0) and (I = 0), IntToStr(YL[J])) then
        Result := Result + VarPower2(Var1Name, Var2Name, I, J);
    end;
  end;

  if Result = '' then
    Result := '0';
end;

{$WARNINGS OFF}

function Int64BiPolynomialSetString(P: TCnInt64BiPolynomial;
  const Str: string; const Var1Name: string; const Var2Name: string): Boolean;
var
  C, Ptr: PChar;
  Num: string;
  E1, E2: Integer;
  F: Int64;
  IsNeg: Boolean;
begin
  // 二元多项式字符串解析有点难
  Result := False;
  if (P = nil) or (Str = '') then
    Exit;

  P.SetZero;
  C := @Str[1];

  while C^ <> #0 do
  begin
    if not (C^ in ['+', '-', '0'..'9']) and (C^ <> Var1Name) and (C^ <> Var2Name) then
    begin
      Inc(C);
      Continue;
    end;

    IsNeg := False;
    if C^ = '+' then
      Inc(C)
    else if C^ = '-' then
    begin
      IsNeg := True;
      Inc(C);
    end;

    F := 1;
    if C^ in ['0'..'9'] then // 找系数
    begin
      Ptr := C;
      while C^ in ['0'..'9'] do
        Inc(C);

      // Ptr 到 C 之间是数字，代表一个系数
      SetString(Num, Ptr, C - Ptr);
      F := StrToInt64(Num);
      if IsNeg then
        F := -F;
    end
    else if IsNeg then
      F := -F;

    E1 := 0;
    if C^ = Var1Name then
    begin
      E1 := 1;
      Inc(C);
      if C^ = '^' then // 找指数
      begin
        Inc(C);
        if C^ in ['0'..'9'] then
        begin
          Ptr := C;
          while C^ in ['0'..'9'] do
            Inc(C);

          // Ptr 到 C 之间是数字，代表一个指数
          SetString(Num, Ptr, C - Ptr);
          E1 := StrToInt64(Num);
        end;
      end;
    end;

    E2 := 0;
    if C^ = Var2Name then
    begin
      E2 := 1;
      Inc(C);
      if C^ = '^' then // 找指数
      begin
        Inc(C);
        if C^ in ['0'..'9'] then
        begin
          Ptr := C;
          while C^ in ['0'..'9'] do
            Inc(C);

          // Ptr 到 C 之间是数字，代表一个指数
          SetString(Num, Ptr, C - Ptr);
          E2 := StrToInt64(Num);
        end;
      end;
    end;

    // 俩指数找完了，凑
    P.SafeValue[E1, E2] := F;
  end;

  Result := True;
end;

{$WARNINGS ON}

function Int64BiPolynomialIsZero(P: TCnInt64BiPolynomial): Boolean;
begin
  Result := (P.FXs.Count = 1) and (TCnInt64List(P.FXs[0]).Count = 1)
    and (TCnInt64List(P.FXs[0])[0] = 0);
end;

procedure Int64BiPolynomialSetZero(P: TCnInt64BiPolynomial);
var
  I: Integer;
begin
  if P.FXs.Count <= 0 then
    P.FXs.Add(TCnInt64List.Create)
  else
    for I := P.FXs.Count - 1 downto 1 do
    begin
      P.FXs[I].Free;
      P.FXs.Delete(I);
    end;

  if P.YFactorsList[0].Count <= 0 then
    P.YFactorsList[0].Add(0)
  else
  begin
    for I := P.YFactorsList[0].Count - 1 downto 1 do
      P.YFactorsList[0].Delete(I);

    P.YFactorsList[0][0] := 0;
  end;
end;

procedure Int64BiPolynomialSetOne(P: TCnInt64BiPolynomial);
var
  I: Integer;
begin
  if P.FXs.Count <= 0 then
    P.FXs.Add(TCnInt64List.Create)
  else
    for I := P.FXs.Count - 1 downto 1 do
    begin
      P.FXs[I].Free;
      P.FXs.Delete(I);
    end;

  if P.YFactorsList[0].Count <= 0 then
    P.YFactorsList[0].Add(1)
  else
  begin
    for I := P.YFactorsList[0].Count - 1 downto 1 do
      P.YFactorsList[0].Delete(I);

    P.YFactorsList[0][0] := 1;
  end;
end;

procedure Int64BiPolynomialNegate(P: TCnInt64BiPolynomial);
var
  I, J: Integer;
  YL: TCnInt64List;
begin
  for I := P.FXs.Count - 1 downto 0 do
  begin
    YL := TCnInt64List(P.FXs[I]);
    for J := YL.Count - 1 downto 0 do
      YL[J] := - YL[J];
  end;
end;

function Int64BiPolynomialIsMonicX(P: TCnInt64BiPolynomial): Boolean;
begin
  Result := False;
  if P.MaxXDegree >= 0 then
    Result := (P.YFactorsList[P.MaxXDegree].Count = 1) and (P.YFactorsList[P.MaxXDegree][0] = 1);
end;

procedure Int64BiPolynomialShiftLeftX(P: TCnInt64BiPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    Int64BiPolynomialShiftRightX(P, -N)
  else
    for I := 0 to N - 1 do
      P.FXs.Insert(0, TCnInt64List.Create);
end;

procedure Int64BiPolynomialShiftRightX(P: TCnInt64BiPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    Int64BiPolynomialShiftLeftX(P, -N)
  else
  begin
    if N > P.FXs.Count then
      N := P.FXs.Count;

    for I := 0 to N - 1 do
    begin
      P.FXs[0].Free;
      P.FXs.Delete(0);
    end;
  end;
end;

function Int64BiPolynomialEqual(A, B: TCnInt64BiPolynomial): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  if (A = nil) or (B = nil) then
    Exit;

  if A.MaxXDegree <> B.MaxXDegree then
    Exit;

  for I := A.FXs.Count - 1 downto 0 do
  begin
    if A.YFactorsList[I].Count <> B.YFactorsList[I].Count then
      Exit;

    for J := A.YFactorsList[I].Count - 1 downto 0 do
      if A.YFactorsList[I][J] <> B.YFactorsList[I][J] then
        Exit;
  end;
  Result := True;
end;

procedure Int64BiPolynomialAddWord(P: TCnInt64BiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  for I := P.FXs.Count - 1 downto 0 do
    for J := P.YFactorsList[I].Count - 1 downto 0 do
      P.YFactorsList[I][J] := P.YFactorsList[I][J] + N;
end;

procedure Int64BiPolynomialSubWord(P: TCnInt64BiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  for I := P.FXs.Count - 1 downto 0 do
    for J := P.YFactorsList[I].Count - 1 downto 0 do
      P.YFactorsList[I][J] := P.YFactorsList[I][J] - N;
end;

procedure Int64BiPolynomialMulWord(P: TCnInt64BiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  if N = 0 then
    P.SetZero
  else if N <> 1 then
    for I := P.FXs.Count - 1 downto 0 do
      for J := P.YFactorsList[I].Count - 1 downto 0 do
        P.YFactorsList[I][J] := P.YFactorsList[I][J] * N;
end;

procedure Int64BiPolynomialDivWord(P: TCnInt64BiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);

  if N <> 1 then
  begin
    for I := P.FXs.Count - 1 downto 0 do
    begin
      for J := P.YFactorsList[I].Count - 1 downto 0 do
        P.YFactorsList[I][J] := P.YFactorsList[I][J] div N;
    end;
  end;
end;

procedure Int64BiPolynomialNonNegativeModWord(P: TCnInt64BiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);

  for I := P.FXs.Count - 1 downto 0 do
  begin
    for J := P.YFactorsList[I].Count - 1 downto 0 do
      P.YFactorsList[I][J] := Int64NonNegativeMod(P.YFactorsList[I][J], N);
  end;
end;

function Int64BiPolynomialAdd(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial): Boolean;
var
  I, J, MaxX, MaxY: Integer;
begin
  MaxX := Max(P1.MaxXDegree, P2.MaxXDegree);
  MaxY := Max(P1.MaxYDegree, P2.MaxYDegree);
  Res.MaxXDegree := MaxX;
  Res.MaxYDegree := MaxY;

  for I := MaxX downto 0 do
  begin
    for J := MaxY downto 0 do
    begin
      Res.YFactorsList[I][J] := P1.SafeValue[I, J] + P2.SafeValue[I, J];
    end;
  end;

  Res.CorrectTop;
  Result := True;
end;

function Int64BiPolynomialSub(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial): Boolean;
var
  I, J, MaxX, MaxY: Integer;
begin
  MaxX := Max(P1.MaxXDegree, P2.MaxXDegree);
  MaxY := Max(P1.MaxYDegree, P2.MaxYDegree);
  Res.MaxXDegree := MaxX;
  Res.MaxYDegree := MaxY;

  for I := MaxX downto 0 do
  begin
    for J := MaxY downto 0 do
    begin
      Res.YFactorsList[I][J] := P1.SafeValue[I, J] - P2.SafeValue[I, J];
    end;
  end;

  Res.CorrectTop;
  Result := True;
end;

function Int64BiPolynomialMul(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial): Boolean;
var
  I, J, K, L: Integer;
  R: TCnInt64BiPolynomial;
begin
  if P1.IsZero or P2.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalInt64BiPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxXDegree := P1.MaxXDegree + P2.MaxXDegree;
  R.MaxYDegree := P1.MaxYDegree + P2.MaxYDegree;

  for I := P1.FXs.Count - 1 downto 0 do
  begin
    for J := P1.YFactorsList[I].Count - 1 downto 0 do
    begin
      // 拿到 P1.SafeValue[I, J]，要遍历相乘 P2 的每一个
      for K := P2.FXs.Count - 1 downto 0 do
      begin
        for L := P2.YFactorsList[K].Count - 1 downto 0 do
        begin
          R.SafeValue[I + K, J + L] := R.SafeValue[I + K, J + L] + P1.SafeValue[I, J] * P2.SafeValue[K, L];
        end;
      end;
    end;
  end;

  R.CorrectTop;
  if (Res = P1) or (Res = P2) then
  begin
    Int64BiPolynomialCopy(Res, R);
    FLocalInt64BiPolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function Int64BiPolynomialMulX(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PX: TCnInt64Polynomial): Boolean;
var
  P: TCnInt64BiPolynomial;
begin
  P := FLocalInt64BiPolynomialPool.Obtain;
  try
    Int64BiPolynomialCopyFromX(P, PX);
    Result := Int64BiPolynomialMul(Res, P1, P);
  finally
    FLocalInt64BiPolynomialPool.Recycle(P);
  end;
end;

function Int64BiPolynomialMulY(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PY: TCnInt64Polynomial): Boolean;
var
  P: TCnInt64BiPolynomial;
begin
  P := FLocalInt64BiPolynomialPool.Obtain;
  try
    Int64BiPolynomialCopyFromY(P, PY);
    Result := Int64BiPolynomialMul(Res, P1, P);
  finally
    FLocalInt64BiPolynomialPool.Recycle(P);
  end;
end;

function Int64BiPolynomialDivX(Res: TCnInt64BiPolynomial; Remain: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Divisor: TCnInt64BiPolynomial): Boolean;
var
  SubRes: TCnInt64BiPolynomial; // 容纳递减差
  MulRes: TCnInt64BiPolynomial; // 容纳除数乘积
  DivRes: TCnInt64BiPolynomial; // 容纳临时商
  I, D: Integer;
  TY: TCnInt64Polynomial;        // 容纳首一多项式需要乘的 Y 多项式
begin
  Result := False;
  if Int64BiPolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxXDegree > P.MaxXDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      Int64BiPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      Int64BiPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  if not Divisor.IsMonicX then // 只支持 X 的首一多项式
    Exit;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;
  TY := nil;

  try
    SubRes := FLocalInt64BiPolynomialPool.Obtain;
    Int64BiPolynomialCopy(SubRes, P);

    D := P.MaxXDegree - Divisor.MaxXDegree;
    DivRes := FLocalInt64BiPolynomialPool.Obtain;
    DivRes.MaxXDegree := D;
    MulRes := FLocalInt64BiPolynomialPool.Obtain;

    TY := FLocalInt64PolynomialPool.Obtain;

    for I := 0 to D do
    begin
      if P.MaxXDegree - I > SubRes.MaxXDegree then                 // 中间结果可能跳位
        Continue;

      Int64BiPolynomialCopy(MulRes, Divisor);
      Int64BiPolynomialShiftLeftX(MulRes, D - I);                 // 对齐到 SubRes 的最高次

      Int64BiPolynomialExtractYByX(TY, SubRes, P.MaxXDegree - I);
      Int64BiPolynomialMulY(MulRes, MulRes, TY);                  // 除式乘到最高次系数相同

      DivRes.SetYCoefficentsFromPolynomial(D - I, TY);            // 商放到 DivRes 位置
      Int64BiPolynomialSub(SubRes, SubRes, MulRes);               // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      Int64BiPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      Int64BiPolynomialCopy(Res, DivRes);
  finally
    FLocalInt64BiPolynomialPool.Recycle(SubRes);
    FLocalInt64BiPolynomialPool.Recycle(MulRes);
    FLocalInt64BiPolynomialPool.Recycle(DivRes);
    FLocalInt64PolynomialPool.Recycle(TY);
  end;
  Result := True;
end;

function Int64BiPolynomialModX(Res: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Divisor: TCnInt64BiPolynomial): Boolean;
begin
  Result := Int64BiPolynomialDivX(nil, Res, P, Divisor);
end;

function Int64BiPolynomialPower(Res: TCnInt64BiPolynomial;
  P: TCnInt64BiPolynomial; Exponent: Int64): Boolean;
var
  T: TCnInt64BiPolynomial;
begin
  if Exponent = 0 then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent = 1 then
  begin
    if Res <> P then
      Int64BiPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end
  else if Exponent < 0 then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidExponent, [Exponent]);

  T := FLocalInt64BiPolynomialPool.Obtain;
  Int64BiPolynomialCopy(T, P);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        Int64BiPolynomialMul(Res, Res, T);

      Exponent := Exponent shr 1;
      if Exponent > 0 then
        Int64BiPolynomialMul(T, T, T);
    end;
    Result := True;
  finally
    FLocalInt64BiPolynomialPool.Recycle(T);
  end;
end;

function Int64BiPolynomialEvaluateByY(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; YValue: Int64): Boolean;
var
  I, J: Integer;
  Sum, TY: Int64;
  YL: TCnInt64List;
begin
  // 针对每一个 FXs[I] 的 List，遍历计算其 Y 各次方值累加，作为 X 的系数
  Res.Clear;
  for I := 0 to P.FXs.Count - 1 do
  begin
    Sum := 0;
    TY := 1;
    YL := TCnInt64List(P.FXs[I]);

    for J := 0 to YL.Count - 1 do
    begin
      Sum := Sum + TY * YL[J];
      TY := TY * YValue;
    end;
    Res.Add(Sum);
  end;
  Result := True;
end;

function Int64BiPolynomialEvaluateByX(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; XValue: Int64): Boolean;
var
  I, J: Integer;
  Sum, TX: Int64;
begin
  // 针对每一个 Y 次数，遍历 FXs[I] 的 List 中的该次数元素，相乘累加，作为 Y 的系数
  Res.Clear;
  for I := 0 to P.MaxYDegree do
  begin
    Sum := 0;
    TX := 1;

    for J := 0 to P.FXs.Count - 1 do
    begin
      Sum := Sum + TX * P.SafeValue[J, I];
      TX := TX * XValue;
    end;
    Res.Add(Sum);
  end;
  Result := True;
end;

procedure Int64BiPolynomialTranspose(Dest, Source: TCnInt64BiPolynomial);
var
  I, J: Integer;
  T: TCnInt64BiPolynomial;
begin
  if Source = Dest then
    T := FLocalInt64BiPolynomialPool.Obtain
  else
    T := Dest;

  // 将 Source 转置塞入 T 中
  T.SetZero;
  T.MaxXDegree := Source.MaxYDegree;
  T.MaxYDegree := Source.MaxXDegree;

  for I := Source.FXs.Count - 1 downto 0 do
    for J := Source.YFactorsList[I].Count - 1 downto 0 do
      T.SafeValue[J, I] := Source.SafeValue[I, J];

  if Source = Dest then
  begin
    Int64BiPolynomialCopy(Dest, T);
    FLocalInt64BiPolynomialPool.Recycle(T);
  end;
end;

procedure Int64BiPolynomialExtractYByX(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; XDegree: Int64);
begin
  CheckDegree(XDegree);
  if XDegree < P.FXs.Count then
    CnInt64ListCopy(Res, TCnInt64List(P.FXs[XDegree]))
  else
    Res.SetZero;
end;

procedure Int64BiPolynomialExtractXByY(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; YDegree: Int64);
var
  I: Integer;
begin
  CheckDegree(YDegree);
  Res.Clear;
  for I := 0 to P.FXs.Count - 1 do
    Res.Add(P.SafeValue[I, YDegree]);

  Res.CorrectTop;
end;

function Int64BiPolynomialGaloisEqual(A, B: TCnInt64BiPolynomial; Prime: Int64): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  if (A = nil) or (B = nil) then
    Exit;

  if A.MaxXDegree <> B.MaxXDegree then
    Exit;

  for I := A.FXs.Count - 1 downto 0 do
  begin
    if A.YFactorsList[I].Count <> B.YFactorsList[I].Count then
      Exit;

    for J := A.YFactorsList[I].Count - 1 downto 0 do
      if (A.YFactorsList[I][J] <> B.YFactorsList[I][J]) and
        (Int64NonNegativeMod(A.YFactorsList[I][J], Prime) <> Int64NonNegativeMod(A.YFactorsList[I][J], Prime)) then
        Exit;
  end;
  Result := True;
end;

procedure Int64BiPolynomialGaloisNegate(P: TCnInt64BiPolynomial; Prime: Int64);
var
  I, J: Integer;
  YL: TCnInt64List;
begin
  for I := P.FXs.Count - 1 downto 0 do
  begin
    YL := TCnInt64List(P.FXs[I]);
    for J := YL.Count - 1 downto 0 do
      YL[J] := Int64NonNegativeMod(-YL[J], Prime);
  end;
end;

function Int64BiPolynomialGaloisAdd(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial): Boolean;
begin
  Result := Int64BiPolynomialAdd(Res, P1, P2);
  if Result then
  begin
    Int64BiPolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      Int64BiPolynomialGaloisModX(Res, Res, Primitive, Prime);
  end;
end;

function Int64BiPolynomialGaloisSub(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial): Boolean;
begin
  Result := Int64BiPolynomialSub(Res, P1, P2);
  if Result then
  begin
    Int64BiPolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      Int64BiPolynomialGaloisModX(Res, Res, Primitive, Prime);
  end;
end;

function Int64BiPolynomialGaloisMul(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  P2: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial): Boolean;
var
  I, J, K, L: Integer;
  R: TCnInt64BiPolynomial;
  T: Int64;
begin
  if P1.IsZero or P2.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalInt64BiPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxXDegree := P1.MaxXDegree + P2.MaxXDegree;
  R.MaxYDegree := P1.MaxYDegree + P2.MaxYDegree;

  for I := P1.FXs.Count - 1 downto 0 do
  begin
    for J := P1.YFactorsList[I].Count - 1 downto 0 do
    begin
      // 拿到 P1.SafeValue[I, J]，要遍历相乘 P2 的每一个
      for K := P2.FXs.Count - 1 downto 0 do
      begin
        for L := P2.YFactorsList[K].Count - 1 downto 0 do
        begin
          // 容易溢出，不能直接相乘
          T := Int64NonNegativeMulMod(P1.SafeValue[I, J], P2.SafeValue[K, L], Prime);
          R.SafeValue[I + K, J + L] := Int64NonNegativeMod(R.SafeValue[I + K, J + L] + Int64NonNegativeMod(T, Prime), Prime);
          // TODO: 暂未处理加法溢出的情况
        end;
      end;
    end;
  end;

  R.CorrectTop;

  // 再对本原多项式取模，注意这里传入的本原多项式是 mod 操作的除数，不是本原多项式参数
  if Primitive <> nil then
    Int64BiPolynomialGaloisModX(R, R, Primitive, Prime);

  if (Res = P1) or (Res = P2) then
  begin
    Int64BiPolynomialCopy(Res, R);
    FLocalInt64BiPolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function Int64BiPolynomialGaloisMulX(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PX: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial): Boolean;
var
  P: TCnInt64BiPolynomial;
begin
  P := FLocalInt64BiPolynomialPool.Obtain;
  try
    Int64BiPolynomialCopyFromX(P, PX);
    Result := Int64BiPolynomialGaloisMul(Res, P1, P, Prime, Primitive);
  finally
    FLocalInt64BiPolynomialPool.Recycle(P);
  end;
end;

function Int64BiPolynomialGaloisMulY(Res: TCnInt64BiPolynomial; P1: TCnInt64BiPolynomial;
  PY: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial): Boolean;
var
  P: TCnInt64BiPolynomial;
begin
  P := FLocalInt64BiPolynomialPool.Obtain;
  try
    Int64BiPolynomialCopyFromY(P, PY);
    Result := Int64BiPolynomialGaloisMul(Res, P1, P, Prime, Primitive);
  finally
    FLocalInt64BiPolynomialPool.Recycle(P);
  end;
end;

function Int64BiPolynomialGaloisDivX(Res: TCnInt64BiPolynomial;
  Remain: TCnInt64BiPolynomial; P: TCnInt64BiPolynomial;
  Divisor: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial): Boolean;
var
  SubRes: TCnInt64BiPolynomial; // 容纳递减差
  MulRes: TCnInt64BiPolynomial; // 容纳除数乘积
  DivRes: TCnInt64BiPolynomial; // 容纳临时商
  I, D: Integer;
  TY: TCnInt64Polynomial;        // 容纳首一多项式需要乘的 Y 多项式
begin
  Result := False;
  if Int64BiPolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxXDegree > P.MaxXDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      Int64BiPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      Int64BiPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  if not Divisor.IsMonicX then // 只支持 X 的首一多项式
    Exit;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;
  TY := nil;

  try
    SubRes := FLocalInt64BiPolynomialPool.Obtain;
    Int64BiPolynomialCopy(SubRes, P);

    D := P.MaxXDegree - Divisor.MaxXDegree;
    DivRes := FLocalInt64BiPolynomialPool.Obtain;
    DivRes.MaxXDegree := D;
    MulRes := FLocalInt64BiPolynomialPool.Obtain;

    TY := FLocalInt64PolynomialPool.Obtain;

    for I := 0 to D do
    begin
      if P.MaxXDegree - I > SubRes.MaxXDegree then                 // 中间结果可能跳位
        Continue;

      Int64BiPolynomialCopy(MulRes, Divisor);
      Int64BiPolynomialShiftLeftX(MulRes, D - I);                 // 对齐到 SubRes 的最高次

      Int64BiPolynomialExtractYByX(TY, SubRes, P.MaxXDegree - I);
      Int64BiPolynomialGaloisMulY(MulRes, MulRes, TY, Prime, Primitive);     // 除式乘到最高次系数相同

      DivRes.SetYCoefficentsFromPolynomial(D - I, TY);            // 商放到 DivRes 位置
      Int64BiPolynomialGaloisSub(SubRes, SubRes, MulRes, Prime, Primitive);  // 减后结果重新放回 SubRes
    end;

    // 商与余式都需要再模本原多项式
    if Primitive <> nil then
    begin
      Int64BiPolynomialGaloisModX(SubRes, SubRes, Primitive, Prime);
      Int64BiPolynomialGaloisModX(DivRes, DivRes, Primitive, Prime);
    end;

    if Remain <> nil then
      Int64BiPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      Int64BiPolynomialCopy(Res, DivRes);
  finally
    FLocalInt64BiPolynomialPool.Recycle(SubRes);
    FLocalInt64BiPolynomialPool.Recycle(MulRes);
    FLocalInt64BiPolynomialPool.Recycle(DivRes);
    FLocalInt64PolynomialPool.Recycle(TY);
  end;
  Result := True;
end;

function Int64BiPolynomialGaloisModX(Res: TCnInt64BiPolynomial; P: TCnInt64BiPolynomial;
  Divisor: TCnInt64BiPolynomial; Prime: Int64; Primitive: TCnInt64BiPolynomial): Boolean;
begin
  Result := Int64BiPolynomialGaloisDivX(nil, Res, P, Divisor, Prime, Primitive);
end;

function Int64BiPolynomialGaloisPower(Res, P: TCnInt64BiPolynomial;
  Exponent: Int64; Prime: Int64; Primitive: TCnInt64BiPolynomial;
  ExponentHi: Int64): Boolean;
var
  T: TCnInt64BiPolynomial;
begin
  if Exponent128IsZero(Exponent, ExponentHi) then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent128IsOne(Exponent, ExponentHi) then
  begin
    if Res <> P then
      Int64BiPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end;

  T := FLocalInt64BiPolynomialPool.Obtain;
  Int64BiPolynomialCopy(T, P);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while not Exponent128IsZero(Exponent, ExponentHi) do
    begin
      if (Exponent and 1) <> 0 then
        Int64BiPolynomialGaloisMul(Res, Res, T, Prime, Primitive);

      ExponentShiftRightOne(Exponent, ExponentHi);
      if not Exponent128IsZero(Exponent, ExponentHi) then
        Int64BiPolynomialGaloisMul(T, T, T, Prime, Primitive);
    end;
    Result := True;
  finally
    FLocalInt64BiPolynomialPool.Recycle(T);
  end;
end;

function Int64BiPolynomialGaloisEvaluateByY(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; YValue, Prime: Int64): Boolean;
var
  I, J: Integer;
  Sum, TY: Int64;
  YL: TCnInt64List;
begin
  // 针对每一个 FXs[I] 的 List，遍历计算其 Y 各次方值累加，作为 X 的系数
  Res.Clear;
  for I := 0 to P.FXs.Count - 1 do
  begin
    Sum := 0;
    TY := 1;
    YL := TCnInt64List(P.FXs[I]);

    for J := 0 to YL.Count - 1 do
    begin
      // TODO: 暂不考虑相加溢出的情况
      Sum := Int64NonNegativeMod(Sum + Int64NonNegativeMulMod(TY, YL[J], Prime), Prime);
      TY := Int64NonNegativeMulMod(TY, YValue, Prime);
    end;
    Res.Add(Sum);
  end;
  Result := True;
end;

function Int64BiPolynomialGaloisEvaluateByX(Res: TCnInt64Polynomial;
  P: TCnInt64BiPolynomial; XValue, Prime: Int64): Boolean;
var
  I, J: Integer;
  Sum, TX: Int64;
begin
  // 针对每一个 Y 次数，遍历 FXs[I] 的 List 中的该次数元素，相乘累加，作为 Y 的系数
  Res.Clear;
  for I := 0 to P.MaxYDegree do
  begin
    Sum := 0;
    TX := 1;

    for J := 0 to P.FXs.Count - 1 do
    begin
      // TODO: 暂不考虑相加溢出的情况
      Sum := Int64NonNegativeMod(Sum + Int64NonNegativeMulMod(TX, P.SafeValue[J, I], Prime), Prime);
      TX := Int64NonNegativeMulMod(TX, XValue, Prime);
    end;
    Res.Add(Sum);
  end;
  Result := True;
end;

procedure Int64BiPolynomialGaloisAddWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
var
  I, J: Integer;
begin
  for I := P.FXs.Count - 1 downto 0 do
    for J := P.YFactorsList[I].Count - 1 downto 0 do
      P.YFactorsList[I][J] := Int64NonNegativeMod(P.YFactorsList[I][J] + N, Prime);
end;

procedure Int64BiPolynomialGaloisSubWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
var
  I, J: Integer;
begin
  for I := P.FXs.Count - 1 downto 0 do
    for J := P.YFactorsList[I].Count - 1 downto 0 do
      P.YFactorsList[I][J] := Int64NonNegativeMod(P.YFactorsList[I][J] - N, Prime);
end;

procedure Int64BiPolynomialGaloisMulWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
var
  I, J: Integer;
begin
  if N = 0 then
    P.SetZero
  else // 有 Prime 需要 Mod，不判断是否是 1 了
    for I := P.FXs.Count - 1 downto 0 do
      for J := P.YFactorsList[I].Count - 1 downto 0 do
        P.YFactorsList[I][J] := Int64NonNegativeMulMod(P.YFactorsList[I][J], N, Prime);
end;

procedure Int64BiPolynomialGaloisDivWord(P: TCnInt64BiPolynomial; N: Int64; Prime: Int64);
var
  I, J: Integer;
  K: Int64;
  B: Boolean;
begin
  if (N = 0) or (Prime = 0) then
    raise EDivByZero.Create(SDivByZero);

  B := N < 0;
  if B then
    N := -N;

  K := CnInt64ModularInverse2(N, Prime);
  for I := P.FXs.Count - 1 downto 0 do
  begin
    for J := P.YFactorsList[I].Count - 1 downto 0 do
    begin
      P.YFactorsList[I][J] := Int64NonNegativeMulMod(P.YFactorsList[I][J], K, Prime);
      if B then
        P.YFactorsList[I][J] := Prime - P.YFactorsList[I][J];
    end;
  end;
end;

procedure TCnInt64BiPolynomial.SetXCoefficents(YDegree: Integer;
  LowToHighXCoefficients: array of const);
var
  I: Integer;
begin
  CheckDegree(YDegree);

  MaxXDegree := High(LowToHighXCoefficients);

  if YDegree > MaxYDegree then
    MaxYDegree := YDegree;

  for I := Low(LowToHighXCoefficients) to High(LowToHighXCoefficients) do
    SafeValue[I, YDegree] := ExtractInt64FromArrayConstElement(LowToHighXCoefficients[I]);
end;

procedure TCnInt64BiPolynomial.SetYCoefficents(XDegree: Integer;
  LowToHighYCoefficients: array of const);
var
  I: Integer;
begin
  CheckDegree(XDegree);

  if XDegree > MaxXDegree then
    MaxXDegree := XDegree;

  YFactorsList[XDegree].Clear;
  for I := Low(LowToHighYCoefficients) to High(LowToHighYCoefficients) do
    YFactorsList[XDegree].Add(ExtractInt64FromArrayConstElement(LowToHighYCoefficients[I]));
end;

procedure TCnInt64BiPolynomial.SetXYCoefficent(XDegree, YDegree: Integer;
  ACoefficient: Int64);
begin
  CheckDegree(XDegree);
  CheckDegree(YDegree);

  if MaxXDegree < XDegree then
    MaxXDegree := XDegree;

  if YFactorsList[XDegree].Count - 1 < YDegree then
    YFactorsList[XDegree].Count := YDegree + 1;

  YFactorsList[XDegree][YDegree] := ACoefficient;
end;

function TCnInt64BiPolynomial.GetSafeValue(XDegree, YDegree: Integer): Int64;
var
  YL: TCnInt64List;
begin
  Result := 0;
  if (XDegree >= 0) and (XDegree < FXs.Count) then
  begin
    YL := TCnInt64List(FXs[XDegree]);
    if (YDegree >= 0) and (YDegree < YL.Count) then
      Result := YL[YDegree];
  end;
end;

procedure TCnInt64BiPolynomial.SetSafeValue(XDegree, YDegree: Integer;
  const Value: Int64);
begin
  SetXYCoefficent(XDegree, YDegree, Value);
end;

procedure TCnInt64BiPolynomial.SetOne;
begin
  Int64BiPolynomialSetOne(Self);
end;

procedure TCnInt64BiPolynomial.Transpose;
begin
  Int64BiPolynomialTranspose(Self, Self);
end;

function TCnInt64BiPolynomial.IsMonicX: Boolean;
begin
  Result := Int64BiPolynomialIsMonicX(Self);
end;

procedure TCnInt64BiPolynomial.SetYCoefficentsFromPolynomial(
  XDegree: Integer; PY: TCnInt64Polynomial);
var
  I: Integer;
begin
  CheckDegree(XDegree);

  if XDegree > MaxXDegree then   // 确保 X 次项的 List 存在
    MaxXDegree := XDegree;

  YFactorsList[XDegree].Clear;
  for I := 0 to PY.MaxDegree do
    YFactorsList[XDegree].Add(PY[I]); // 给特定的 YList 的所有元素设值
end;

procedure TCnInt64BiPolynomial.Clear;
var
  I: Integer;
begin
  if FXs.Count <= 0 then
    FXs.Add(TCnInt64List.Create)
  else
    for I := FXs.Count - 1 downto 1 do
    begin
      FXs[I].Free;
      FXs.Delete(I);
    end;

  YFactorsList[0].Clear;
end;

{ TCnInt64BiPolynomialPool }

function TCnInt64BiPolynomialPool.CreateObject: TObject;
begin
  Result := TCnInt64BiPolynomial.Create;
end;

function TCnInt64BiPolynomialPool.Obtain: TCnInt64BiPolynomial;
begin
  Result := TCnInt64BiPolynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnInt64BiPolynomialPool.Recycle(Poly: TCnInt64BiPolynomial);
begin
  inherited Recycle(Poly);
end;

// ========================== 二元大整系数多项式 ===============================

function BigNumberBiPolynomialNew: TCnBigNumberBiPolynomial;
begin
  Result := TCnBigNumberBiPolynomial.Create;
end;

procedure BigNumberBiPolynomialFree(P: TCnBigNumberBiPolynomial);
begin
  P.Free;
end;

function BigNumberBiPolynomialDuplicate(P: TCnBigNumberBiPolynomial): TCnBigNumberBiPolynomial;
begin
  if P = nil then
  begin
    Result := nil;
    Exit;
  end;

  Result := BigNumberBiPolynomialNew;
  if Result <> nil then
    BigNumberBiPolynomialCopy(Result, P);
end;

function BigNumberBiPolynomialCopy(Dest: TCnBigNumberBiPolynomial;
  Source: TCnBigNumberBiPolynomial): TCnBigNumberBiPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    if Source.MaxXDegree >= 0 then
    begin
      Dest.MaxXDegree := Source.MaxXDegree;
      for I := 0 to Source.MaxXDegree do
      begin
        if Source.FXs[I] = nil then
        begin
          Dest.FXs[I].Free;
          Dest.FXs[I] := nil;
        end
        else
          Source.YFactorsList[I].AssignTo(Dest.YFactorsList[I]);
      end;
    end
    else
      Dest.SetZero; // 如果 Source 未初始化，则 Dest 也清零
  end;
end;

function BigNumberBiPolynomialCopyFromX(Dest: TCnBigNumberBiPolynomial;
  SrcX: TCnBigNumberPolynomial): TCnBigNumberBiPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  Dest.Clear;

  Dest.MaxXDegree := SrcX.MaxDegree;
  for I := 0 to SrcX.MaxDegree do
    if SrcX[I].IsZero then
    begin
      Dest.FXs[I].Free;
      Dest.FXs[I] := nil;
    end
    else
      Dest.SafeValue[I, 0] := SrcX[I]; // 给每一个 YList 的首元素设值，0 则清空 FXs 对应项
end;

function BigNumberBiPolynomialCopyFromY(Dest: TCnBigNumberBiPolynomial;
  SrcY: TCnBigNumberPolynomial): TCnBigNumberBiPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  Dest.Clear;

  if not SrcY.IsZero then
    for I := 0 to SrcY.MaxDegree do
      Dest.YFactorsList[0].AddPair(I, SrcY[I]); // 给最低一个 YList 的所有元素设值
end;

function BigNumberBiPolynomialToString(P: TCnBigNumberBiPolynomial;
  const Var1Name: string; const Var2Name: string): string;
var
  I, J: Integer;
  YL: TCnSparseBigNumberList;
begin
  Result := '';
  for I := P.FXs.Count - 1 downto 0 do
  begin
    YL := TCnSparseBigNumberList(P.FXs[I]);  // 只处理存在的项，无需新增 0 项
    if YL <> nil then
      for J := YL.Count - 1 downto 0 do
      begin
        if VarItemFactor(Result, (YL[J].Exponent = 0) and (I = 0), YL[J].Value.ToDec) then
          Result := Result + VarPower2(Var1Name, Var2Name, I, YL[J].Exponent);
      end;
  end;

  if Result = '' then
    Result := '0';
end;

{$WARNINGS OFF}

function BigNumberBiPolynomialSetString(P: TCnBigNumberBiPolynomial;
  const Str: string; const Var1Name: string; const Var2Name: string): Boolean;
var
  C, Ptr: PChar;
  Num, ES: string;
  E1, E2: Integer;
  IsNeg: Boolean;
begin
  // 二元多项式字符串解析有点难
  Result := False;
  if (P = nil) or (Str = '') then
    Exit;

  P.SetZero;
  C := @Str[1];

  while C^ <> #0 do
  begin
    if not (C^ in ['+', '-', '0'..'9']) and (C^ <> Var1Name) and (C^ <> Var2Name) then
    begin
      Inc(C);
      Continue;
    end;

    IsNeg := False;
    if C^ = '+' then
      Inc(C)
    else if C^ = '-' then
    begin
      IsNeg := True;
      Inc(C);
    end;

    Num := '1';
    if C^ in ['0'..'9'] then // 找系数
    begin
      Ptr := C;
      while C^ in ['0'..'9'] do
        Inc(C);

      // Ptr 到 C 之间是数字，代表一个系数
      SetString(Num, Ptr, C - Ptr);
      if IsNeg then
        Num := '-' + Num;
    end
    else if IsNeg then
      Num := '-' + Num;

    E1 := 0;
    if C^ = Var1Name then
    begin
      E1 := 1;
      Inc(C);
      if C^ = '^' then // 找指数
      begin
        Inc(C);
        if C^ in ['0'..'9'] then
        begin
          Ptr := C;
          while C^ in ['0'..'9'] do
            Inc(C);

          // Ptr 到 C 之间是数字，代表一个指数
          SetString(ES, Ptr, C - Ptr);
          E1 := StrToInt64(ES);
        end;
      end;
    end;

    E2 := 0;
    if C^ = Var2Name then
    begin
      E2 := 1;
      Inc(C);
      if C^ = '^' then // 找指数
      begin
        Inc(C);
        if C^ in ['0'..'9'] then
        begin
          Ptr := C;
          while C^ in ['0'..'9'] do
            Inc(C);

          // Ptr 到 C 之间是数字，代表一个指数
          SetString(ES, Ptr, C - Ptr);
          E2 := StrToInt64(ES);
        end;
      end;
    end;

    // 俩指数找完了，凑
    P.SafeValue[E1, E2].SetDec(AnsiString(Num));
  end;

  Result := True;
end;

{$WARNINGS ON}

function BigNumberBiPolynomialIsZero(P: TCnBigNumberBiPolynomial): Boolean;
begin
  Result := True;
  if P.FXs.Count = 0 then
    Exit;

  if (P.FXs.Count = 1) and ((P.FXs[0] = nil) or (TCnSparseBigNumberList(P.FXs[0]).Count = 0)) then
    Exit;

  if (P.FXs.Count = 1) and (P.FXs[0] <> nil) and (TCnSparseBigNumberList(P.FXs[0]).Count = 1)
    and (TCnSparseBigNumberList(P.FXs[0])[0].Exponent = 0) and TCnSparseBigNumberList(P.FXs[0])[0].Value.IsZero then
    Exit;

  Result := False;
end;

procedure BigNumberBiPolynomialSetZero(P: TCnBigNumberBiPolynomial);
begin
  P.FXs.Clear;
end;

procedure BigNumberBiPolynomialSetOne(P: TCnBigNumberBiPolynomial);
var
  I: Integer;
begin
  if P.FXs.Count <= 0 then
    P.FXs.Add(TCnSparseBigNumberList.Create)
  else
    for I := P.FXs.Count - 1 downto 1 do
    begin
      P.FXs[I].Free;
      P.FXs.Delete(I);
    end;

  if P.YFactorsList[0].Count <= 0 then
    P.YFactorsList[0].Add(TCnExponentBigNumberPair.Create)
  else
  begin
    for I := P.YFactorsList[0].Count - 1 downto 1 do
      P.YFactorsList[0].Delete(I);
  end;

  P.YFactorsList[0][0].Exponent := 0;
  P.YFactorsList[0][0].Value.SetOne;
end;

procedure BigNumberBiPolynomialNegate(P: TCnBigNumberBiPolynomial);
var
  I, J: Integer;
  YL: TCnSparseBigNumberList;
begin
  for I := P.FXs.Count - 1 downto 0 do
  begin
    YL := TCnSparseBigNumberList(P.FXs[I]); // 如不存在，无需创建
    if YL <> nil then
      for J := YL.Count - 1 downto 0 do
        YL[I].Value.Negate;
  end;
end;

function BigNumberBiPolynomialIsMonicX(P: TCnBigNumberBiPolynomial): Boolean;
begin
  Result := False;
  if P.MaxXDegree >= 0 then
    Result := (P.YFactorsList[P.MaxXDegree].Count = 1) and (P.YFactorsList[P.MaxXDegree][0].Exponent = 0)
      and (P.YFactorsList[P.MaxXDegree][0].Value.IsOne);
end;

procedure BigNumberBiPolynomialShiftLeftX(P: TCnBigNumberBiPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigNumberBiPolynomialShiftRightX(P, -N)
  else
    for I := 0 to N - 1 do
      P.FXs.InsertBatch(0, N);
end;

procedure BigNumberBiPolynomialShiftRightX(P: TCnBigNumberBiPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigNumberBiPolynomialShiftLeftX(P, -N)
  else
  begin
    if N > P.FXs.Count then
      N := P.FXs.Count;

    for I := N - 1 downto 0 do
      P.FXs[I].Free;

    P.FXs.DeleteLow(N);
  end;
end;

function BigNumberBiPolynomialEqual(A, B: TCnBigNumberBiPolynomial): Boolean;
var
  I: Integer;
begin
  Result := False;
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  if (A = nil) or (B = nil) then
    Exit;

  if A.MaxXDegree <> B.MaxXDegree then
    Exit;

  for I := A.FXs.Count - 1 downto 0 do
  begin
    if not SparseBigNumberListEqual(TCnSparseBigNumberList(A.FXs[I]), TCnSparseBigNumberList(B.FXs[I])) then
      Exit;

//    if (A.FXs[I] = nil) and (B.FXs[I] = nil) then
//      Continue;
//
//    if A.YFactorsList[I].Count <> B.YFactorsList[I].Count then
//      Exit;
//
//    for J := A.YFactorsList[I].Count - 1 downto 0 do
//      if (A.YFactorsList[I][J].Exponent <> B.YFactorsList[I][J].Exponent) or
//        not BigNumberEqual(A.YFactorsList[I][J].Value, B.YFactorsList[I][J].Value) then
//        Exit;
  end;
  Result := True;
end;

// ===================== 二元大整系数多项式普通运算 ============================

procedure BigNumberBiPolynomialMulWord(P: TCnBigNumberBiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  if N = 0 then
    P.SetZero
  else if N <> 1 then
    for I := P.FXs.Count - 1 downto 0 do
      if P.FXs[I] <> nil then
        for J := P.YFactorsList[I].Count - 1 downto 0 do
          P.YFactorsList[I][J].Value.MulWord(N);
end;

procedure BigNumberBiPolynomialDivWord(P: TCnBigNumberBiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);

  if N <> 1 then
  begin
    for I := P.FXs.Count - 1 downto 0 do
    begin
      if P.FXs[I] <> nil then
      begin
        for J := P.YFactorsList[I].Count - 1 downto 0 do
          P.YFactorsList[I][J].Value.DivWord(N);
      end;
    end;
  end;
end;

procedure BigNumberBiPolynomialNonNegativeModWord(P: TCnBigNumberBiPolynomial; N: Int64);
var
  I, J: Integer;
begin
  if N = 0 then
    raise EDivByZero.Create(SDivByZero);

  for I := P.FXs.Count - 1 downto 0 do
  begin
    if P.FXs[I] <> nil then
    begin
      for J := P.YFactorsList[I].Count - 1 downto 0 do
        P.YFactorsList[I][J].Value.ModWord(N); // 不是 NonNegativeMod 先这样
    end;
  end;
end;

procedure BigNumberBiPolynomialMulBigNumber(P: TCnBigNumberBiPolynomial; N: TCnBigNumber);
var
  I, J: Integer;
begin
  if N.IsZero then
    P.SetZero
  else if not N.IsOne then
  begin
    for I := P.FXs.Count - 1 downto 0 do
    begin
      if P.FXs[I] <> nil then
      begin
        for J := P.YFactorsList[I].Count - 1 downto 0 do
          BigNumberMul(P.YFactorsList[I][J].Value, P.YFactorsList[I][J].Value, N);
      end;
    end;
  end;
end;

procedure BigNumberBiPolynomialDivBigNumber(P: TCnBigNumberBiPolynomial; N: TCnBigNumber);
var
  I, J: Integer;
begin
  if N.IsZero then
    raise EDivByZero.Create(SDivByZero);

  if not N.IsOne then
  begin
    for I := P.FXs.Count - 1 downto 0 do
    begin
      if P.FXs[I] <> nil then
      begin
        for J := P.YFactorsList[I].Count - 1 downto 0 do
          BigNumberDiv(P.YFactorsList[I][J].Value, nil, P.YFactorsList[I][J].Value, N);
      end;
    end;
  end;
end;

procedure BigNumberBiPolynomialNonNegativeModBigNumber(P: TCnBigNumberBiPolynomial; N: TCnBigNumber);
var
  I, J: Integer;
begin
  if N.IsZero then
    raise EDivByZero.Create(SDivByZero);

  for I := P.FXs.Count - 1 downto 0 do
  begin
    if P.FXs[I] <> nil then
    begin
      for J := P.YFactorsList[I].Count - 1 downto 0 do
        BigNumberNonNegativeMod(P.YFactorsList[I][J].Value, P.YFactorsList[I][J].Value, N);
    end;
  end;
end;

function BigNumberBiPolynomialAdd(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial): Boolean;
var
  I, M: Integer;
  S1, S2: TCnSparseBigNumberList;
begin
  M := Max(P1.MaxXDegree, P2.MaxXDegree);
  Res.SetMaxXDegree(M);

  for I := M downto 0 do
  begin
    if I >= P1.FXs.Count then
      S1 := nil
    else
      S1 := TCnSparseBigNumberList(P1.FXs[I]);

    if I >= P2.FXs.Count then
      S2 := nil
    else
      S2 := TCnSparseBigNumberList(P2.FXs[I]);

    if (S1 = nil) and (S2 = nil) then
    begin
      Res.FXs[I].Free;
      Res.FXs[I] := nil;
    end
    else
      SparseBigNumberListMerge(Res.YFactorsList[I], S1, S2, True); // 本循环确保覆盖每一个 Res.YFactorsList[I]
  end;
  Res.CorrectTop;
  Result := True;
end;

function BigNumberBiPolynomialSub(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial): Boolean;
var
  I, M: Integer;
  S1, S2: TCnSparseBigNumberList;
begin
  M := Max(P1.MaxXDegree, P2.MaxXDegree);
  Res.SetMaxXDegree(M);

  for I := M downto 0 do
  begin
    if I >= P1.FXs.Count then
      S1 := nil
    else
      S1 := TCnSparseBigNumberList(P1.FXs[I]);

    if I >= P2.FXs.Count then
      S2 := nil
    else
      S2 := TCnSparseBigNumberList(P2.FXs[I]);

    if (S1 = nil) and (S2 = nil) then
    begin
      Res.FXs[I].Free;
      Res.FXs[I] := nil;
    end
    else
      SparseBigNumberListMerge(Res.YFactorsList[I], S1, S2, False);
  end;
  Res.CorrectTop;
  Result := True;
end;

function BigNumberBiPolynomialMul(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial): Boolean;
var
  I, J, K, L: Integer;
  R: TCnBigNumberBiPolynomial;
  T: TCnBigNumber;
  Pair1, Pair2: TCnExponentBigNumberPair;
begin
  if P1.IsZero or P2.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalBigNumberBiPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxXDegree := P1.MaxXDegree + P2.MaxXDegree;
  R.MaxYDegree := P1.MaxYDegree + P2.MaxYDegree;

  T := FLocalBigNumberPool.Obtain;
  try
    for I := P1.FXs.Count - 1 downto 0 do
    begin
      if P1.FXs[I] = nil then
        Continue;

      for J := P1.YFactorsList[I].Count - 1 downto 0 do
      begin
        Pair1 := P1.YFactorsList[I][J];
        // 拿到 P1.SafeValue[I, J]，要遍历相乘 P2 的每一个
        for K := P2.FXs.Count - 1 downto 0 do
        begin
          if P2.FXs[K] = nil then
            Continue;

          for L := P2.YFactorsList[K].Count - 1 downto 0 do
          begin
            Pair2 := P2.YFactorsList[K][L];
            BigNumberMul(T, Pair1.Value, Pair2.Value);
            BigNumberAdd(R.SafeValue[I + K, Pair1.Exponent + Pair2.Exponent],
              R.SafeValue[I + K, Pair1.Exponent + Pair2.Exponent], T);
          end;
        end;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
  end;

  R.CorrectTop;
  if (Res = P1) or (Res = P2) then
  begin
    BigNumberBiPolynomialCopy(Res, R);
    FLocalBigNumberBiPolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function BigNumberBiPolynomialMulX(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PX: TCnBigNumberPolynomial): Boolean;
var
  P: TCnBigNumberBiPolynomial;
begin
  P := FLocalBigNumberBiPolynomialPool.Obtain;
  try
    BigNumberBiPolynomialCopyFromX(P, PX);
    Result := BigNumberBiPolynomialMul(Res, P1, P);
  finally
    FLocalBigNumberBiPolynomialPool.Recycle(P);
  end;
end;

function BigNumberBiPolynomialMulY(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PY: TCnBigNumberPolynomial): Boolean;
var
  P: TCnBigNumberBiPolynomial;
begin
  P := FLocalBigNumberBiPolynomialPool.Obtain;
  try
    BigNumberBiPolynomialCopyFromY(P, PY);
    Result := BigNumberBiPolynomialMul(Res, P1, P);
  finally
    FLocalBigNumberBiPolynomialPool.Recycle(P);
  end;
end;

function BigNumberBiPolynomialDivX(Res: TCnBigNumberBiPolynomial;
  Remain: TCnBigNumberBiPolynomial; P: TCnBigNumberBiPolynomial;
  Divisor: TCnBigNumberBiPolynomial): Boolean;
var
  SubRes: TCnBigNumberBiPolynomial; // 容纳递减差
  MulRes: TCnBigNumberBiPolynomial; // 容纳除数乘积
  DivRes: TCnBigNumberBiPolynomial; // 容纳临时商
  I, D: Integer;
  TY: TCnBigNumberPolynomial;       // 容纳首一多项式需要乘的 Y 多项式
begin
  Result := False;
  if BigNumberBiPolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxXDegree > P.MaxXDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      BigNumberBiPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      BigNumberBiPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  if not Divisor.IsMonicX then // 只支持 X 的首一多项式
    Exit;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;
  TY := nil;

  try
    SubRes := FLocalBigNumberBiPolynomialPool.Obtain;
    BigNumberBiPolynomialCopy(SubRes, P);

    D := P.MaxXDegree - Divisor.MaxXDegree;
    DivRes := FLocalBigNumberBiPolynomialPool.Obtain;
    DivRes.MaxXDegree := D;
    MulRes := FLocalBigNumberBiPolynomialPool.Obtain;

    TY := FLocalBigNumberPolynomialPool.Obtain;

    for I := 0 to D do
    begin
      if P.MaxXDegree - I > SubRes.MaxXDegree then                 // 中间结果可能跳位
        Continue;

      BigNumberBiPolynomialCopy(MulRes, Divisor);
      BigNumberBiPolynomialShiftLeftX(MulRes, D - I);              // 对齐到 SubRes 的最高次

      BigNumberBiPolynomialExtractYByX(TY, SubRes, P.MaxXDegree - I);
      BigNumberBiPolynomialMulY(MulRes, MulRes, TY);               // 除式乘到最高次系数相同

      DivRes.SetYCoefficentsFromPolynomial(D - I, TY);             // 商放到 DivRes 位置
      BigNumberBiPolynomialSub(SubRes, SubRes, MulRes);            // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      BigNumberBiPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      BigNumberBiPolynomialCopy(Res, DivRes);
  finally
    FLocalBigNumberBiPolynomialPool.Recycle(SubRes);
    FLocalBigNumberBiPolynomialPool.Recycle(MulRes);
    FLocalBigNumberBiPolynomialPool.Recycle(DivRes);
    FLocalBigNumberPolynomialPool.Recycle(TY);
  end;
  Result := True;
end;

function BigNumberBiPolynomialModX(Res: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Divisor: TCnBigNumberBiPolynomial): Boolean;
begin
  Result := BigNumberBiPolynomialDivX(nil, Res, P, Divisor);
end;

function BigNumberBiPolynomialPower(Res: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Exponent: TCnBigNumber): Boolean;
var
  T: TCnBigNumberBiPolynomial;
  E: TCnBigNumber;
begin
  if Exponent.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent.IsOne then
  begin
    if Res <> P then
      BigNumberBiPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end
  else if Exponent.IsNegative then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidExponent, [Exponent.ToDec]);

  T := FLocalBigNumberBiPolynomialPool.Obtain;
  BigNumberBiPolynomialCopy(T, P);
  E := FLocalBigNumberPool.Obtain;
  BigNumberCopy(E, Exponent);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while not E.IsZero do
    begin
      if BigNumberIsBitSet(E, 0) then
        BigNumberBiPolynomialMul(Res, Res, T);

      BigNumberShiftRightOne(E, E);
      if not E.IsZero then // 最后一次不用乘了
        BigNumberBiPolynomialMul(T, T, T);
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(E);
    FLocalBigNumberBiPolynomialPool.Recycle(T);
  end;
end;

function BigNumberBiPolynomialEvaluateByY(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; YValue: TCnBigNumber): Boolean;
var
  I, J: Integer;
  Sum, TY, T: TCnBigNumber;
  YL: TCnSparseBigNumberList;
  Pair: TCnExponentBigNumberPair;
begin
  // 针对每一个 FXs[I] 的 List，遍历计算其 Y 各次方值累加，作为 X 的系数
  Res.Clear;
  Sum := nil;
  TY := nil;
  T := nil;

  try
    Sum := FLocalBigNumberPool.Obtain;
    TY := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    for I := 0 to P.FXs.Count - 1 do
    begin
      if P.FXs[I] = nil then
        Continue;

      Sum.SetZero;
      YL := P.YFactorsList[I];

      if YL.Count > 0 then
      begin
        if YL.Bottom.Exponent = 0 then
          TY.SetOne
        else
          BigNumberPower(TY, YValue, YL.Bottom.Exponent);

        for J := 0 to YL.Count - 1 do
        begin
          Pair := YL[J];

          // Sum := Sum + TY * YL[J];
          BigNumberMul(T, TY, Pair.Value);
          BigNumberAdd(Sum, Sum, T);

          // TY := TY * Power(YValue, YL[J+1].Exponent - YL[J].Exponent);
          if J < YL.Count - 1 then
          begin
            BigNumberPower(T, YValue, YL[J + 1].Exponent - YL[J].Exponent);
            BigNumberMul(TY, TY, T);
          end;
        end;
      end;
      BigNumberCopy(Res.Add, Sum);
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(TY);
    FLocalBigNumberPool.Recycle(Sum);
  end;
  Result := True;
end;

function BigNumberBiPolynomialEvaluateByX(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; XValue: TCnBigNumber): Boolean;
var
  I, J: Integer;
  Sum, TX, T: TCnBigNumber;
begin
  // 针对每一个 Y 次数，遍历 FXs[I] 的 List 中的该次数元素，相乘累加，作为 Y 的系数
  Res.Clear;
  Sum := nil;
  TX := nil;
  T := nil;

  try
    Sum := FLocalBigNumberPool.Obtain;
    TX := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    for I := 0 to P.MaxYDegree do
    begin
      Sum.SetZero;
      TX.SetOne;

      for J := 0 to P.FXs.Count - 1 do
      begin
        //Sum := Sum + TX * P.SafeValue[J, I];
        BigNumberMul(T, TX, P.ReadonlyValue[J, I]);
        BigNumberAdd(Sum, Sum, T);

        //TX := TX * XValue;
        BigNumberMul(TX, TX, XValue);
      end;
      BigNumberCopy(Res.Add, Sum);
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(TX);
    FLocalBigNumberPool.Recycle(Sum);
  end;
  Result := True;
end;

procedure BigNumberBiPolynomialTranspose(Dest, Source: TCnBigNumberBiPolynomial);
var
  I, J: Integer;
  T: TCnBigNumberBiPolynomial;
  Pair: TCnExponentBigNumberPair;
begin
  if Source = Dest then
    T := FLocalBigNumberBiPolynomialPool.Obtain
  else
    T := Dest;

  // 将 Source 转置塞入 T 中
  T.SetZero;
  T.MaxXDegree := Source.MaxYDegree;
  T.MaxYDegree := Source.MaxXDegree;

  for I := Source.FXs.Count - 1 downto 0 do
  begin
    if Source.FXs[I] <> nil then
      for J := Source.YFactorsList[I].Count - 1 downto 0 do
      begin
        Pair := Source.YFactorsList[I][J];
        T.SafeValue[Pair.Exponent, I] := Pair.Value; // 内部复制
      end;
  end;

  if Source = Dest then
  begin
    BigNumberBiPolynomialCopy(Dest, T);
    FLocalBigNumberBiPolynomialPool.Recycle(T);
  end;
end;

procedure BigNumberBiPolynomialExtractYByX(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; XDegree: Integer);
var
  I: Integer;
  Pair: TCnExponentBigNumberPair;
begin
  CheckDegree(XDegree);
  Res.SetZero;

  if XDegree < P.FXs.Count then
  begin
    if P.FXs[XDegree] <> nil then
    begin
      Pair := P.YFactorsList[XDegree].Top;
      Res.MaxDegree := Pair.Exponent;

      for I := 0 to P.YFactorsList[XDegree].Count - 1 do
      begin
        Pair := P.YFactorsList[XDegree][I];
        if Res[Pair.Exponent] = nil then
          Res[Pair.Exponent] := TCnBigNumber.Create;

        BigNumberCopy(Res[Pair.Exponent], Pair.Value);
      end;
    end;
  end;
end;

procedure BigNumberBiPolynomialExtractXByY(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; YDegree: Integer);
var
  I: Integer;
begin
  CheckDegree(YDegree);
  Res.Clear;
  for I := 0 to P.FXs.Count - 1 do
    BigNumberCopy(Res.Add, P.ReadonlyValue[I, YDegree]);

  Res.CorrectTop;
end;

// ================== 二元大整系数多项式式在有限域上的模运算 ===================

function BigNumberBiPolynomialGaloisEqual(A, B: TCnBigNumberBiPolynomial; Prime: TCnBigNumber): Boolean;
var
  I, J: Integer;
  T1, T2: TCnBigNumber;
begin
  Result := False;
  if A = B then
  begin
    Result := True;
    Exit;
  end;

  if (A = nil) or (B = nil) then
    Exit;

  if A.MaxXDegree <> B.MaxXDegree then
    Exit;

  T1 := nil;
  T2 := nil;

  try
    T1 := FLocalBigNumberPool.Obtain;
    T2 := FLocalBigNumberPool.Obtain;

    for I := A.FXs.Count - 1 downto 0 do
    begin
      // TODO: 未处理 A[I] 和 B[I] 一个是 nil，另一个经过 mod 运算后是 0 的情形
      if (A.FXs[I] = nil) and (B.FXs[I] = nil) then
        Continue
      else if A.FXs[I] = nil then // 判断 B 是否为 0
      begin
        if not SparseBigNumberListIsZero(TCnSparseBigNumberList(B.FXs[I])) then
          Exit;
      end
      else if B.FXs[I] = nil then // 判断 A 是否为 0
      begin
        if not SparseBigNumberListIsZero(TCnSparseBigNumberList(A.FXs[I])) then
          Exit;
      end;

      if A.YFactorsList[I].Count <> B.YFactorsList[I].Count then
        Exit;

      for J := A.YFactorsList[I].Count - 1 downto 0 do
      begin
        if (A.YFactorsList[I][J].Exponent <> B.YFactorsList[I][J].Exponent) or
          not BigNumberEqual(A.YFactorsList[I][J].Value, B.YFactorsList[I][J].Value) then
        begin
          BigNumberNonNegativeMod(T1, A.YFactorsList[I][J].Value, Prime);
          BigNumberNonNegativeMod(T2, B.YFactorsList[I][J].Value, Prime);
          if not BigNumberEqual(T1, T2) then
            Exit;
        end;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(T1);
    FLocalBigNumberPool.Recycle(T2);
  end;
  Result := True;
end;

procedure BigNumberBiPolynomialGaloisNegate(P: TCnBigNumberBiPolynomial; Prime: TCnBigNumber);
var
  I, J: Integer;
  YL: TCnSparseBigNumberList;
begin
  for I := P.FXs.Count - 1 downto 0 do
  begin
    YL := TCnSparseBigNumberList(P.FXs[I]);
    if YL <> nil then
      for J := YL.Count - 1 downto 0 do
      begin
        YL[J].Value.Negate;
        BigNumberNonNegativeMod(YL[J].Value, YL[J].Value, Prime);
      end;
  end;
end;

function BigNumberBiPolynomialGaloisAdd(Res: TCnBigNumberBiPolynomial;
  P1: TCnBigNumberBiPolynomial; P2: TCnBigNumberBiPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial): Boolean;
begin
  Result := BigNumberBiPolynomialAdd(Res, P1, P2);
  if Result then
  begin
    BigNumberBiPolynomialNonNegativeModBigNumber(Res, Prime);
    if Primitive <> nil then
      BigNumberBiPolynomialGaloisModX(Res, Res, Primitive, Prime);
  end;
end;

function BigNumberBiPolynomialGaloisSub(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial): Boolean;
begin
  Result := BigNumberBiPolynomialSub(Res, P1, P2);
  if Result then
  begin
    BigNumberBiPolynomialNonNegativeModBigNumber(Res, Prime);
    if Primitive <> nil then
      BigNumberBiPolynomialGaloisModX(Res, Res, Primitive, Prime);
  end;
end;

function BigNumberBiPolynomialGaloisMul(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  P2: TCnBigNumberBiPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial): Boolean;
var
  I, J, K, L: Integer;
  R: TCnBigNumberBiPolynomial;
  T: TCnBigNumber;
  Pair1, Pair2: TCnExponentBigNumberPair;
begin
  if P1.IsZero or P2.IsZero then
  begin
    Res.SetZero;
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalBigNumberBiPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxXDegree := P1.MaxXDegree + P2.MaxXDegree;
  R.MaxYDegree := P1.MaxYDegree + P2.MaxYDegree;

  T := FLocalBigNumberPool.Obtain;
  try
    for I := P1.FXs.Count - 1 downto 0 do
    begin
      if P1.FXs[I] = nil then
        Continue;

      for J := P1.YFactorsList[I].Count - 1 downto 0 do
      begin
        Pair1 := P1.YFactorsList[I][J];
        // 拿到 P1.SafeValue[I, J] 里的非 0 项，要遍历相乘 P2 的每一个非 0 项
        for K := P2.FXs.Count - 1 downto 0 do
        begin
          if P2.FXs[K] = nil then
            Continue;

          for L := P2.YFactorsList[K].Count - 1 downto 0 do
          begin
            Pair2 := P2.YFactorsList[K][L];
            BigNumberMul(T, Pair1.Value, Pair2.Value);
            BigNumberAdd(R.SafeValue[I + K, Pair1.Exponent + Pair2.Exponent],
              R.SafeValue[I + K, Pair1.Exponent + Pair2.Exponent], T);
            BigNumberNonNegativeMod(R.SafeValue[I + K, Pair1.Exponent + Pair2.Exponent],
              R.SafeValue[I + K, Pair1.Exponent + Pair2.Exponent], Prime);
          end;
        end;
      end;
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
  end;

  R.CorrectTop;
  if Primitive <> nil then
    BigNumberBiPolynomialGaloisModX(R, R, Primitive, Prime);

  if (Res = P1) or (Res = P2) then
  begin
    BigNumberBiPolynomialCopy(Res, R);
    FLocalBigNumberBiPolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function BigNumberBiPolynomialGaloisMulX(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PX: TCnBigNumberPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial): Boolean;
var
  P: TCnBigNumberBiPolynomial;
begin
  P := FLocalBigNumberBiPolynomialPool.Obtain;
  try
    BigNumberBiPolynomialCopyFromX(P, PX);
    Result := BigNumberBiPolynomialGaloisMul(Res, P1, P, Prime, Primitive);
  finally
    FLocalBigNumberBiPolynomialPool.Recycle(P);
  end;
end;

function BigNumberBiPolynomialGaloisMulY(Res: TCnBigNumberBiPolynomial; P1: TCnBigNumberBiPolynomial;
  PY: TCnBigNumberPolynomial; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial): Boolean;
var
  P: TCnBigNumberBiPolynomial;
begin
  P := FLocalBigNumberBiPolynomialPool.Obtain;
  try
    BigNumberBiPolynomialCopyFromY(P, PY);
    Result := BigNumberBiPolynomialGaloisMul(Res, P1, P, Prime, Primitive);
  finally
    FLocalBigNumberBiPolynomialPool.Recycle(P);
  end;
end;

function BigNumberBiPolynomialGaloisDivX(Res: TCnBigNumberBiPolynomial;
  Remain: TCnBigNumberBiPolynomial; P: TCnBigNumberBiPolynomial;
  Divisor: TCnBigNumberBiPolynomial; Prime: TCnBigNumber;
  Primitive: TCnBigNumberBiPolynomial): Boolean;
var
  SubRes: TCnBigNumberBiPolynomial; // 容纳递减差
  MulRes: TCnBigNumberBiPolynomial; // 容纳除数乘积
  DivRes: TCnBigNumberBiPolynomial; // 容纳临时商
  I, D: Integer;
  TY: TCnBigNumberPolynomial;       // 容纳首一多项式需要乘的 Y 多项式
begin
  Result := False;
  if BigNumberBiPolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxXDegree > P.MaxXDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      BigNumberBiPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      BigNumberBiPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  if not Divisor.IsMonicX then // 只支持 X 的首一多项式
    Exit;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;
  TY := nil;

  try
    SubRes := FLocalBigNumberBiPolynomialPool.Obtain;
    BigNumberBiPolynomialCopy(SubRes, P);

    D := P.MaxXDegree - Divisor.MaxXDegree;
    DivRes := FLocalBigNumberBiPolynomialPool.Obtain;
    DivRes.MaxXDegree := D;
    MulRes := FLocalBigNumberBiPolynomialPool.Obtain;

    TY := FLocalBigNumberPolynomialPool.Obtain;

    for I := 0 to D do
    begin
      if P.MaxXDegree - I > SubRes.MaxXDegree then                 // 中间结果可能跳位
        Continue;

      BigNumberBiPolynomialCopy(MulRes, Divisor);
      BigNumberBiPolynomialShiftLeftX(MulRes, D - I);              // 对齐到 SubRes 的最高次

      BigNumberBiPolynomialExtractYByX(TY, SubRes, P.MaxXDegree - I);
      BigNumberBiPolynomialGaloisMulY(MulRes, MulRes, TY, Prime, Primitive);               // 除式乘到最高次系数相同

      DivRes.SetYCoefficentsFromPolynomial(D - I, TY);             // 商放到 DivRes 位置
      BigNumberBiPolynomialGaloisSub(SubRes, SubRes, MulRes, Prime, Primitive);            // 减后结果重新放回 SubRes
    end;

    // 商与余式都需要再模本原多项式
    if Primitive <> nil then
    begin
      BigNumberBiPolynomialGaloisModX(SubRes, SubRes, Primitive, Prime);
      BigNumberBiPolynomialGaloisModX(DivRes, DivRes, Primitive, Prime);
    end;

    if Remain <> nil then
      BigNumberBiPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      BigNumberBiPolynomialCopy(Res, DivRes);
  finally
    FLocalBigNumberBiPolynomialPool.Recycle(SubRes);
    FLocalBigNumberBiPolynomialPool.Recycle(MulRes);
    FLocalBigNumberBiPolynomialPool.Recycle(DivRes);
    FLocalBigNumberPolynomialPool.Recycle(TY);
  end;
  Result := True;
end;

function BigNumberBiPolynomialGaloisModX(Res: TCnBigNumberBiPolynomial;
  P: TCnBigNumberBiPolynomial; Divisor: TCnBigNumberBiPolynomial;
  Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial): Boolean;
begin
  Result := BigNumberBiPolynomialGaloisDivX(nil, Res, P, Divisor, Prime, Primitive);
end;

function BigNumberBiPolynomialGaloisPower(Res, P: TCnBigNumberBiPolynomial;
  Exponent: TCnBigNumber; Prime: TCnBigNumber; Primitive: TCnBigNumberBiPolynomial): Boolean;
var
  T: TCnBigNumberBiPolynomial;
  E: TCnBigNumber;
begin
  if Exponent.IsZero then
  begin
    Res.SetOne;
    Result := True;
    Exit;
  end
  else if Exponent.IsOne then
  begin
    if Res <> P then
      BigNumberBiPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end
  else if Exponent.IsNegative then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidExponent, [Exponent.ToDec]);

  T := FLocalBigNumberBiPolynomialPool.Obtain;
  BigNumberBiPolynomialCopy(T, P);
  E := FLocalBigNumberPool.Obtain;
  BigNumberCopy(E, Exponent);

  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetOne;
    while not E.IsZero do
    begin
      if BigNumberIsBitSet(E, 0) then
        BigNumberBiPolynomialGaloisMul(Res, Res, T, Prime, Primitive);

      BigNumberShiftRightOne(E, E);
      if not E.IsZero then
        BigNumberBiPolynomialGaloisMul(T, T, T, Prime, Primitive);
    end;
    Result := True;
  finally
    FLocalBigNumberPool.Recycle(E);
    FLocalBigNumberBiPolynomialPool.Recycle(T);
  end;
end;

function BigNumberBiPolynomialGaloisEvaluateByY(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; YValue, Prime: TCnBigNumber): Boolean;
var
  I, J: Integer;
  Sum, TY, T, TE: TCnBigNumber;
  YL: TCnSparseBigNumberList;
  Pair: TCnExponentBigNumberPair;
begin
  // 针对每一个 FXs[I] 的 List，遍历计算其 Y 各次方值累加，作为 X 的系数
  Res.Clear;
  Sum := nil;
  TY := nil;
  TE := nil;
  T := nil;

  try
    Sum := FLocalBigNumberPool.Obtain;
    TY := FLocalBigNumberPool.Obtain;
    TE := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    for I := 0 to P.FXs.Count - 1 do
    begin
      if P.FXs[I] = nil then
        Continue;

      Sum.SetZero;
      YL := P.YFactorsList[I];

      if YL.Count > 0 then
      begin
        if YL.Bottom.Exponent = 0 then
          TY.SetOne
        else if YL.Bottom.Exponent = 1 then
          BigNumberCopy(TY, YValue)
        else if YL.Bottom.Exponent = 2 then
          BigNumberDirectMulMod(TY, YValue, YValue, Prime)
        else
        begin
          T.SetWord(YL.Bottom.Exponent);
          BigNumberPowerMod(TY, YValue, T, Prime);
        end;

        for J := 0 to YL.Count - 1 do
        begin
          Pair := YL[J];

          // Sum := Sum + TY * YL[J];
          BigNumberMul(T, TY, Pair.Value);
          BigNumberAdd(Sum, Sum, T);
          BigNumberNonNegativeMod(Sum, Sum, Prime);

          // TY := TY * Power(YValue, YL[J+1].Exponent - YL[J].Exponent);
          if J < YL.Count - 1 then
          begin
            TE.SetWord(YL[J + 1].Exponent - YL[J].Exponent);
            BigNumberPowerMod(T, YValue, TE, Prime);
            BigNumberDirectMulMod(TY, TY, T, Prime);
          end;
        end;
      end;
      BigNumberCopy(Res.Add, Sum);
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(TY);
    FLocalBigNumberPool.Recycle(TE);
    FLocalBigNumberPool.Recycle(Sum);
  end;
  Result := True;
end;

function BigNumberBiPolynomialGaloisEvaluateByX(Res: TCnBigNumberPolynomial;
  P: TCnBigNumberBiPolynomial; XValue, Prime: TCnBigNumber): Boolean;
var
  I, J: Integer;
  Sum, TX, T: TCnBigNumber;
begin
  // 针对每一个 Y 次数，遍历 FXs[I] 的 List 中的该次数元素，相乘累加，作为 Y 的系数
  Res.Clear;
  Sum := nil;
  TX := nil;
  T := nil;

  try
    Sum := FLocalBigNumberPool.Obtain;
    TX := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;

    for I := 0 to P.MaxYDegree do
    begin
      Sum.SetZero;
      TX.SetOne;

      for J := 0 to P.FXs.Count - 1 do
      begin
        if P.FXs[J] <> nil then
        begin
          //Sum := Sum + TX * P.SafeValue[J, I];
          BigNumberMul(T, TX, P.ReadonlyValue[J, I]);
          BigNumberAdd(Sum, Sum, T);
          BigNumberNonNegativeMod(Sum, Sum, Prime);
        end;

        //TX := TX * XValue;
        BigNumberMul(TX, TX, XValue);
        BigNumberNonNegativeMod(TX, TX, Prime);
      end;
      BigNumberCopy(Res.Add, Sum);
    end;
  finally
    FLocalBigNumberPool.Recycle(T);
    FLocalBigNumberPool.Recycle(TX);
    FLocalBigNumberPool.Recycle(Sum);
  end;
  Result := True;
end;

procedure BigNumberBiPolynomialGaloisMulWord(P: TCnBigNumberBiPolynomial;
  N: Int64; Prime: TCnBigNumber);
var
  I, J: Integer;
begin
  if N = 0 then
    P.SetZero
  else // 有 Prime 需要 Mod，不判断是否是 1 了
    for I := P.FXs.Count - 1 downto 0 do
    begin
      if P.FXs[I] <> nil then
        for J := P.YFactorsList[I].Count - 1 downto 0 do
        begin
          P.YFactorsList[I][J].Value.MulWord(N);
          BigNumberNonNegativeMod(P.YFactorsList[I][J].Value, P.YFactorsList[I][J].Value, Prime);
        end;
    end;
end;

procedure BigNumberBiPolynomialGaloisDivWord(P: TCnBigNumberBiPolynomial;
  N: Int64; Prime: TCnBigNumber);
var
  I, J: Integer;
  B: Boolean;
  K, T: TCnBigNumber;
begin
  if (N = 0) or Prime.IsZero then
    raise EDivByZero.Create(SDivByZero);

  B := N < 0;
  if B then
    N := -N;

  K := nil;
  T := nil;

  try
    K := FLocalBigNumberPool.Obtain;
    T := FLocalBigNumberPool.Obtain;
    T.SetWord(N);

    BigNumberModularInverse(K, T, Prime);

    for I := P.FXs.Count - 1 downto 0 do
    begin
      if P.FXs[I] <> nil then
        for J := P.YFactorsList[I].Count - 1 downto 0 do
        begin
          BigNumberDirectMulMod(P.YFactorsList[I][J].Value, P.YFactorsList[I][J].Value, K, Prime);
          if B then
            BigNumberSub(P.YFactorsList[I][J].Value, Prime, P.YFactorsList[I][J].Value);
        end;
    end;
  finally
    FLocalBigNumberPool.Recycle(K);
    FLocalBigNumberPool.Recycle(T);
  end;
end;

procedure Int64PolynomialToBigNumberPolynomial(Dest: TCnBigNumberPolynomial;
  Source: TCnInt64Polynomial);
var
  I: Integer;
begin
  Dest.MaxDegree := Source.MaxDegree;
  for I := 0 to Source.MaxDegree do
    Dest[I].SetInt64(Source[I]);
end;
{ TCnBigNumberBiPolynomial }

procedure TCnBigNumberBiPolynomial.Clear;
var
  I: Integer;
begin
//  if FXs.Count <= 0 then
//    FXs.Add(TCnSparseBigNumberList.Create)
//  else
    for I := FXs.Count - 1 downto 0 do
    begin
      FXs[I].Free;
      FXs.Delete(I);
    end;

//  YFactorsList[0].Clear;
end;

function TCnBigNumberBiPolynomial.CompactYDegree(
  YList: TCnSparseBigNumberList): Boolean;
begin
  if YList = nil then
    Result := True
  else
  begin
    YList.Compact;
    Result := YList.Count = 0;
  end;
end;

procedure TCnBigNumberBiPolynomial.CorrectTop;
var
  I: Integer;
  Compact, MeetNonEmpty: Boolean;
  YL: TCnSparseBigNumberList;
begin
  MeetNonEmpty := False;
  for I := FXs.Count - 1 downto 0 do
  begin
    YL := TCnSparseBigNumberList(FXs[I]);
    if YL = nil then
      Compact := True
    else
      Compact := CompactYDegree(YL);

    if not Compact then     // 本次压缩非 0
      MeetNonEmpty := True;

    if Compact and not MeetNonEmpty then // 最高的一路下来压缩出来全 0 的要删掉
    begin
      FXs.Delete(I);
      YL.Free;
    end
    else if Compact then // 普通的压缩的全 0 的，需要释放 SparseBigNumberList，但 FXs 里还得占位
    begin
      FXs[I] := nil;
      YL.Free;
    end;
  end;
end;

constructor TCnBigNumberBiPolynomial.Create(XDegree, YDegree: Integer);
begin
  FXs := TCnRefObjectList.Create;
  EnsureDegrees(XDegree, YDegree);
end;

destructor TCnBigNumberBiPolynomial.Destroy;
var
  I: Integer;
begin
  for I := FXs.Count - 1 downto 0 do
    FXs[I].Free;
  FXs.Free;
  inherited;
end;

procedure TCnBigNumberBiPolynomial.EnsureDegrees(XDegree,
  YDegree: Integer);
var
  I: Integer;
begin
  CheckDegree(XDegree);
  CheckDegree(YDegree);

  // OldCount := FXs.Count;
  if (XDegree + 1) > FXs.Count then
  begin
    for I := FXs.Count + 1 to XDegree + 1 do
    begin
      FXs.Add(nil);
      // TCnSparseBigNumberList(FXs[FXs.Count - 1]).Count := YDegree + 1;
    end;
  end;

//  for I:= OldCount - 1 downto 0 do
//    if TCnSparseBigNumberList(FXs[I]).Count < YDegree + 1 then
//      TCnSparseBigNumberList(FXs[I]).Count := YDegree + 1;
end;

function TCnBigNumberBiPolynomial.GetMaxXDegree: Integer;
begin
  Result := FXs.Count - 1;
end;

function TCnBigNumberBiPolynomial.GetMaxYDegree: Integer;
var
  I: Integer;
  Pair: TCnExponentBigNumberPair;
begin
  Result := 0;
  for I := FXs.Count - 1 downto 0 do
  begin
    if FXs[I] <> nil then
      if YFactorsList[I].Count > 0 then
      begin
        Pair := YFactorsList[I].Top;
        if Pair <> nil then
        begin
          if Pair.Exponent > Result then
          Result := Pair.Exponent;
        end;
      end;
  end;
end;

function TCnBigNumberBiPolynomial.GetReadonlyValue(XDegree,
  YDegree: Integer): TCnBigNumber;
var
  YL: TCnSparseBigNumberList;
begin
  Result := CnBigNumberZero;
  if (XDegree >= 0) and (XDegree < FXs.Count) then
  begin
    YL := TCnSparseBigNumberList(FXs[XDegree]);
    if YL <> nil then
      if (YDegree >= 0) and (YDegree < YL.Count) then
        Result := YL.ReadonlyValue[YDegree];
  end;
end;

function TCnBigNumberBiPolynomial.GetSafeValue(XDegree,
  YDegree: Integer): TCnBigNumber;
var
  YL: TCnSparseBigNumberList;
begin
  if XDegree > MaxXDegree then
    MaxXDegree := XDegree;

  YL := YFactorsList[XDegree];  // 确保 XDegree 存在
  Result := YL.SafeValue[YDegree];
end;

function TCnBigNumberBiPolynomial.GetYFactorsList(
  Index: Integer): TCnSparseBigNumberList;
begin
  if Index < 0 then
    raise ECnPolynomialException.CreateFmt(SCnErrorPolynomialInvalidDegree, [Index]);

  if Index >= FXs.Count then
    FXs.Count := Index + 1;

  Result := TCnSparseBigNumberList(FXs[Index]);
  if Result = nil then
  begin
    Result := TCnSparseBigNumberList.Create;
    FXs[Index] := Result;
  end;
end;

function TCnBigNumberBiPolynomial.IsMonicX: Boolean;
begin
  Result := BigNumberBiPolynomialIsMonicX(Self);
end;

function TCnBigNumberBiPolynomial.IsZero: Boolean;
begin
  Result := BigNumberBiPolynomialIsZero(Self);
end;

procedure TCnBigNumberBiPolynomial.Negate;
begin
  BignumberBiPolynomialNegate(Self);
end;

procedure TCnBigNumberBiPolynomial.SetMaxXDegree(const Value: Integer);
var
  I: Integer;
begin
  CheckDegree(Value);

  if Value + 1 > FXs.Count then
  begin
    FXs.Count := Value + 1; // 不预先创建
//    for I := FXs.Count + 1 to Value + 1 do
//      FXs.Add(TCnSparseBigNumberList.Create);
  end
  else if Value + 1 < FXs.Count then
  begin
    for I := FXs.Count - 1 downto Value + 1 do
    begin
      FXs[I].Free;
      FXs.Delete(I);
    end;
  end;
end;

procedure TCnBigNumberBiPolynomial.SetMaxYDegree(const Value: Integer);
begin
  // Not Needed
end;

procedure TCnBigNumberBiPolynomial.SetOne;
begin
  BigNumberBiPolynomialSetOne(Self);
end;

procedure TCnBigNumberBiPolynomial.SetSafeValue(XDegree, YDegree: Integer;
  const Value: TCnBigNumber);
var
  YL: TCnSparseBigNumberList;
begin
  if XDegree > MaxXDegree then
    MaxXDegree := XDegree;

  YL := YFactorsList[XDegree];    // 确保 XDegree 存在
  YL.SafeValue[YDegree] := Value; // 内部 Copy 大数
end;

procedure TCnBigNumberBiPolynomial.SetString(const Poly: string);
begin
  BigNumberBiPolynomialSetString(Self, Poly);
end;

procedure TCnBigNumberBiPolynomial.SetXCoefficents(YDegree: Integer;
  LowToHighXCoefficients: array of const);
var
  I: Integer;
  S: string;
begin
  CheckDegree(YDegree);

  MaxXDegree := High(LowToHighXCoefficients);

  if YDegree > MaxYDegree then
    MaxYDegree := YDegree;

  for I := Low(LowToHighXCoefficients) to High(LowToHighXCoefficients) do
  begin
    S := ExtractBigNumberFromArrayConstElement(LowToHighXCoefficients[I]);
    if S <> '' then
      SafeValue[I, YDegree].SetDec(AnsiString(ExtractBigNumberFromArrayConstElement(LowToHighXCoefficients[I])));
  end;
end;

procedure TCnBigNumberBiPolynomial.SetXYCoefficent(XDegree,
  YDegree: Integer; ACoefficient: Int64);
begin
  CheckDegree(XDegree);
  CheckDegree(YDegree);

  if MaxXDegree < XDegree then
    MaxXDegree := XDegree;

  YFactorsList[XDegree].SafeValue[YDegree].SetInt64(ACoefficient); // 内部是 BigNumberCopy 值
end;

procedure TCnBigNumberBiPolynomial.SetXYCoefficent(XDegree,
  YDegree: Integer; ACoefficient: TCnBigNumber);
begin
  CheckDegree(XDegree);
  CheckDegree(YDegree);

  if MaxXDegree < XDegree then
    MaxXDegree := XDegree;

  YFactorsList[XDegree].SafeValue[YDegree] := ACoefficient; // 内部是 BigNumberCopy 值
end;

procedure TCnBigNumberBiPolynomial.SetYCoefficents(XDegree: Integer;
  LowToHighYCoefficients: array of const);
var
  I: Integer;
begin
  CheckDegree(XDegree);

  if XDegree > MaxXDegree then
    MaxXDegree := XDegree;

  YFactorsList[XDegree].Clear;
  for I := Low(LowToHighYCoefficients) to High(LowToHighYCoefficients) do
    YFactorsList[XDegree].SafeValue[I].SetDec(AnsiString(ExtractBigNumberFromArrayConstElement(LowToHighYCoefficients[I])));
end;

procedure TCnBigNumberBiPolynomial.SetYCoefficentsFromPolynomial(
  XDegree: Integer; PY: TCnInt64Polynomial);
var
  I: Integer;
begin
  CheckDegree(XDegree);

  if XDegree > MaxXDegree then
    MaxXDegree := XDegree;

  if PY.IsZero then
  begin
    FXs[XDegree].Free;
    FXs[XDegree] := nil;
  end
  else
  begin
    YFactorsList[XDegree].Clear; // 确保 X 次项的 List 存在
    for I := 0 to PY.MaxDegree do
      YFactorsList[XDegree].SafeValue[I].SetInt64(PY[I]);
  end;
end;

procedure TCnBigNumberBiPolynomial.SetYCoefficentsFromPolynomial(
  XDegree: Integer; PY: TCnBigNumberPolynomial);
var
  I: Integer;
begin
  CheckDegree(XDegree);

  if XDegree > MaxXDegree then
    MaxXDegree := XDegree;

  if PY.IsZero then
  begin
    FXs[XDegree].Free;
    FXs[XDegree] := nil;
  end
  else
  begin
    YFactorsList[XDegree].Clear;   // 确保 X 次项的 List 存在
    for I := 0 to PY.MaxDegree do
      YFactorsList[XDegree].SafeValue[I] := PY[I];
  end;
end;

procedure TCnBigNumberBiPolynomial.SetZero;
begin
  BigNumberBiPolynomialSetZero(Self);
end;

function TCnBigNumberBiPolynomial.ToString: string;
begin
  Result := BigNumberBiPolynomialToString(Self);
end;

procedure TCnBigNumberBiPolynomial.Transpose;
begin
  BigNumberBiPolynomialTranspose(Self, Self);
end;

{ TCnBigNumberBiPolynomialPool }

function TCnBigNumberBiPolynomialPool.CreateObject: TObject;
begin
  Result := TCnBigNumberBiPolynomial.Create;
end;

function TCnBigNumberBiPolynomialPool.Obtain: TCnBigNumberBiPolynomial;
begin
  Result := TCnBigNumberBiPolynomial(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigNumberBiPolynomialPool.Recycle(
  Poly: TCnBigNumberBiPolynomial);
begin
  inherited Recycle(Poly);
end;

{ TCnInt64PolynomialList }

function TCnInt64PolynomialList.Add: TCnInt64Polynomial;
begin
  Result := TCnInt64Polynomial.Create;
  Add(Result);
end;

function TCnInt64PolynomialList.Add(APoly: TCnInt64Polynomial): Integer;
begin
  Result := inherited Add(APoly);
end;

constructor TCnInt64PolynomialList.Create;
begin
  inherited Create(True);
end;

destructor TCnInt64PolynomialList.Destroy;
begin

  inherited;
end;

function TCnInt64PolynomialList.GetItem(Index: Integer): TCnInt64Polynomial;
begin
  Result := TCnInt64Polynomial(inherited GetItem(Index));
end;

procedure TCnInt64PolynomialList.SetItem(Index: Integer;
  APoly: TCnInt64Polynomial);
begin
  inherited SetItem(Index, APoly);
end;

function TCnInt64PolynomialList.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := Items[I].ToString
    else
      Result := Result + ',' + Items[I].ToString;
  end;
end;

// ================= 一元大浮点复数多项式独立函数实现 ========================

function BigComplexDecimalPolynomialNew: TCnBigComplexDecimalPolynomial;
begin
  Result := TCnBigComplexDecimalPolynomial.Create;
end;

procedure BigComplexDecimalPolynomialFree(P: TCnBigComplexDecimalPolynomial);
begin
  P.Free;
end;

function BigComplexDecimalPolynomialDuplicate(P: TCnBigComplexDecimalPolynomial): TCnBigComplexDecimalPolynomial;
var
  I: Integer;
begin
  Result := TCnBigComplexDecimalPolynomial.Create;
  for I := 0 to P.Count - 1 do
  begin
    Result.EnsureDegree(I);
    BigComplexDecimalCopy(Result.Items[I], P.Items[I]);
  end;
end;

function BigComplexDecimalPolynomialCopy(Dest: TCnBigComplexDecimalPolynomial;
  Source: TCnBigComplexDecimalPolynomial): TCnBigComplexDecimalPolynomial;
var
  I: Integer;
begin
  Result := Dest;
  if Source <> Dest then
  begin
    Dest.Clear;
    for I := 0 to Source.Count - 1 do
    begin
      Dest.EnsureDegree(I);
      BigComplexDecimalCopy(Dest.Items[I], Source.Items[I]);
    end;
  end;
end;

procedure BigComplexDecimalPolynomialSwap(A: TCnBigComplexDecimalPolynomial;
  B: TCnBigComplexDecimalPolynomial);
var
  I, MaxCount: Integer;
  Temp: TCnBigComplexDecimal;
begin
  MaxCount := Max(A.Count, B.Count);

  // 确保两个多项式都有足够的项
  while A.Count < MaxCount do
  begin
    A.Add;
    TCnBigComplexDecimal(A.Items[A.Count - 1]).SetZero;
  end;

  while B.Count < MaxCount do
  begin
    B.Add;
    TCnBigComplexDecimal(B.Items[B.Count - 1]).SetZero;
  end;

  // 交换所有项
  for I := 0 to MaxCount - 1 do
  begin
    Temp := TCnBigComplexDecimal.Create;
    try
      BigComplexDecimalCopy(Temp, A.Items[I]);
      BigComplexDecimalCopy(A.Items[I], B.Items[I]);
      BigComplexDecimalCopy(B.Items[I], Temp);
    finally
      Temp.Free;
    end;
  end;
end;

function BigComplexDecimalPolynomialToString(P: TCnBigComplexDecimalPolynomial;
  const VarName: string): string;
var
  I: Integer;
begin
  P.CorrectTop;
  if P.Count = 0 then
  begin
    Result := '0';
    Exit;
  end;

  for I := P.Count - 1 downto 0 do
  begin
    if not P[I].IsZero then
    begin
      if Result <> '' then
        Result := Result + '+';
      if I = 0 then
        Result := Result + P[I].ToString
      else if I = 1 then
      begin
        if P[I].IsOne then
          Result := Result + 'X'
        else
          Result := Result + '(' + P[I].ToString + ')X'
      end
      else
      begin
        if P[I].IsOne then
          Result := Result + 'X^' + IntToStr(I)
        else
          Result := Result + '(' + P[I].ToString + ')X^' + IntToStr(I);
      end;
    end;
  end;
end;

function BigComplexDecimalPolynomialIsZero(P: TCnBigComplexDecimalPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsZero;
end;

procedure BigComplexDecimalPolynomialSetZero(P: TCnBigComplexDecimalPolynomial);
begin
  P.Clear;
  P.Add.SetZero;
end;

function BigComplexDecimalPolynomialIsOne(P: TCnBigComplexDecimalPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsOne;
end;

procedure BigComplexDecimalPolynomialSetOne(P: TCnBigComplexDecimalPolynomial);
begin
  P.Clear;
  P.Add.SetOne;
end;

function BigComplexDecimalPolynomialIsNegOne(P: TCnBigComplexDecimalPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and P[0].IsNegOne;
end;

procedure BigComplexDecimalPolynomialNegate(P: TCnBigComplexDecimalPolynomial);
var
  I: Integer;
begin
  for I := 0 to P.MaxDegree do
    P[I].Negate;
end;

function BigComplexDecimalPolynomialIsMonic(P: TCnBigComplexDecimalPolynomial): Boolean;
begin
  Result := P[P.MaxDegree].IsOne;
end;

procedure BigComplexDecimalPolynomialShiftLeft(P: TCnBigComplexDecimalPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigComplexDecimalPolynomialShiftRight(P, -N)
  else
    for I := 1 to N do
      P.Insert(0, TCnBigComplexDecimal.Create);
end;

procedure BigComplexDecimalPolynomialShiftRight(P: TCnBigComplexDecimalPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    BigComplexDecimalPolynomialShiftLeft(P, -N)
  else
  begin
    for I := 1 to N do
      P.Delete(0);

    if P.Count = 0 then
      P.Add.SetZero;
  end;
end;

function BigComplexDecimalPolynomialEqual(A: TCnBigComplexDecimalPolynomial;
  B: TCnBigComplexDecimalPolynomial): Boolean;
var
  I: Integer;
begin
  Result := False;
  if A.Count <> B.Count then
    Exit;

  for I := 0 to A.Count - 1 do
  begin
    if not BigComplexDecimalEqual(A.Items[I], B.Items[I]) then
      Exit;
  end;

  Result := True;
end;

function BigComplexDecimalPolynomialAdd(Res: TCnBigComplexDecimalPolynomial;
  P1: TCnBigComplexDecimalPolynomial; P2: TCnBigComplexDecimalPolynomial): Boolean;
var
  I, D1, D2: Integer;
  PBig: TCnBigComplexDecimalPolynomial;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then
      PBig := P1
    else
      PBig := P2;

    Res.MaxDegree := D1; // 考虑到 Res 可能是 P1 或 P2，所以给 Res 的 MaxDegree 赋值得放上面的比较之后
    for I := D1 downto D2 + 1 do
      BigComplexDecimalCopy(Res[I], PBig[I]);
  end
  else // D1 = D2 说明俩加式同次
    Res.MaxDegree := D1;

  for I := D2 downto 0 do
    BigComplexDecimalAdd(Res[I], P1[I], P2[I]);

  Res.CorrectTop;
  Result := True;
end;

function BigComplexDecimalPolynomialSub(Res: TCnBigComplexDecimalPolynomial;
  P1: TCnBigComplexDecimalPolynomial; P2: TCnBigComplexDecimalPolynomial): Boolean;
var
  I, D1, D2: Integer;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  Res.MaxDegree := D1;
  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then // 被减式大
    begin
      for I := D1 downto D2 + 1 do
        BigComplexDecimalCopy(Res[I], P1[I]);
    end
    else  // 减式大
    begin
      for I := D1 downto D2 + 1 do
      begin
        BigComplexDecimalCopy(Res[I], P2[I]);
        Res[I].Negate;
      end;
    end;
  end;

  for I := D2 downto 0 do
    BigComplexDecimalSub(Res[I], P1[I], P2[I]);

  Res.CorrectTop;
  Result := True;
end;

function BigComplexDecimalPolynomialMul(Res: TCnBigComplexDecimalPolynomial;
  P1: TCnBigComplexDecimalPolynomial; P2: TCnBigComplexDecimalPolynomial): Boolean;
var
  R: TCnBigComplexDecimalPolynomial;
  T: TCnBigComplexDecimal;
  I, J: Integer;
begin
  if BigComplexDecimalPolynomialIsZero(P1) or BigComplexDecimalPolynomialIsZero(P2) then
  begin
    BigComplexDecimalPolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  T := FLocalBigComplexDecimalPool.Obtain;
  if (Res = P1) or (Res = P2) then
    R := FLocalBigComplexDecimalPolynomialPool.Obtain
  else
    R := Res;

  R.Clear;
  R.MaxDegree := P1.MaxDegree + P2.MaxDegree;

  for I := 0 to P1.MaxDegree do
  begin
    // 把第 I 次方的数字乘以 P2 的每一个数字，加到结果的 I 开头的部分
    for J := 0 to P2.MaxDegree do
    begin
      BigComplexDecimalMul(T, P1[I], P2[J]);
      BigComplexDecimalAdd(R[I + J], R[I + J], T);
    end;
  end;

  R.CorrectTop;
  if (Res = P1) or (Res = P2) then
  begin
    BigComplexDecimalPolynomialCopy(Res, R);
    FLocalBigComplexDecimalPolynomialPool.Recycle(R);
  end;

  FLocalBigComplexDecimalPool.Recycle(T);
  Result := True;
end;

procedure BigComplexDecimalPolynomialMulBigComplexDecimal(P: TCnBigComplexDecimalPolynomial;
  N: TCnBigComplexDecimal);
var
  I: Integer;
  T: TCnBigComplexDecimal;
begin
  if BigComplexDecimalIsZero(N) then
    BigComplexDecimalPolynomialSetZero(P)
  else if not N.IsOne then
  begin
    T := FLocalBigComplexDecimalPool.Obtain;
    try
      for I := 0 to P.MaxDegree do
      begin
        BigComplexDecimalMul(T, P[I], N);
        BigComplexDecimalCopy(P[I], T);
      end;
    finally
      FLocalBigComplexDecimalPool.Recycle(T);
    end;
  end;
end;

function BigComplexDecimalPolynomialDiv(Res: TCnBigComplexDecimalPolynomial;
  Remain: TCnBigComplexDecimalPolynomial; P: TCnBigComplexDecimalPolynomial;
  Divisor: TCnBigComplexDecimalPolynomial): Boolean;
var
  SubRes: TCnBigComplexDecimalPolynomial; // 容纳递减差
  MulRes: TCnBigComplexDecimalPolynomial; // 容纳除数乘积
  DivRes: TCnBigComplexDecimalPolynomial; // 容纳临时商
  I, D: Integer;
  T: TCnBigComplexDecimal;
begin
  Result := False;

  if BigComplexDecimalPolynomialIsZero(Divisor) then
    raise EDivByZero.Create(SDivByZero);

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      BigComplexDecimalPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      BigComplexDecimalPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;
  T := nil;

  try
    T := FLocalBigComplexDecimalPool.Obtain;

    SubRes := FLocalBigComplexDecimalPolynomialPool.Obtain;
    BigComplexDecimalPolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := FLocalBigComplexDecimalPolynomialPool.Obtain;
    DivRes.MaxDegree := D;
    MulRes := FLocalBigComplexDecimalPolynomialPool.Obtain;

    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then                 // 中间结果可能跳位
        Continue;

      // 大浮点数复数除法：T = SubRes[P.MaxDegree - I] / Divisor[Divisor.MaxDegree]
      if BigComplexDecimalIsZero(Divisor[Divisor.MaxDegree]) then
        Exit;

      BigComplexDecimalDiv(T, SubRes[P.MaxDegree - I], Divisor[Divisor.MaxDegree]);

      BigComplexDecimalPolynomialCopy(MulRes, Divisor);
      BigComplexDecimalPolynomialShiftLeft(MulRes, D - I);        // 对齐到 SubRes 的最高次

      BigComplexDecimalPolynomialMulBigComplexDecimal(MulRes, T); // 除式乘到最高次系数相同
      BigComplexDecimalCopy(DivRes[D - I], T);                    // 商放到 DivRes 位置

      BigComplexDecimalPolynomialSub(SubRes, SubRes, MulRes);     // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      BigComplexDecimalPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      BigComplexDecimalPolynomialCopy(Res, DivRes);

    Result := True;
  finally
    FLocalBigComplexDecimalPolynomialPool.Recycle(DivRes);
    FLocalBigComplexDecimalPolynomialPool.Recycle(MulRes);
    FLocalBigComplexDecimalPolynomialPool.Recycle(SubRes);
    FLocalBigComplexDecimalPool.Recycle(T);
  end;
end;

function BigComplexDecimalPolynomialPower(Res: TCnBigComplexDecimalPolynomial;
  P: TCnBigComplexDecimalPolynomial; Exponent: Int64): Boolean;
var
  Temp, Temp2: TCnBigComplexDecimalPolynomial;
  I: Int64;
begin
  Result := True;

  if Exponent < 0 then
  begin
    Result := False;
    Exit;
  end;

  if Exponent = 0 then
  begin
    Res.SetOne;
    Exit;
  end;

  if Exponent = 1 then
  begin
    BigComplexDecimalPolynomialCopy(Res, P);
    Exit;
  end;

  Temp := TCnBigComplexDecimalPolynomial.Create;
  Temp2 := TCnBigComplexDecimalPolynomial.Create;
  try
    BigComplexDecimalPolynomialCopy(Temp, P);
    Res.SetOne;

    I := Exponent;
    while I > 0 do
    begin
      if I and 1 = 1 then
      begin
        BigComplexDecimalPolynomialMul(Temp2, Res, Temp);
        BigComplexDecimalPolynomialCopy(Res, Temp2);
      end;

      BigComplexDecimalPolynomialMul(Temp2, Temp, Temp);
      BigComplexDecimalPolynomialCopy(Temp, Temp2);
      I := I shr 1;
    end;
  finally
    Temp.Free;
    Temp2.Free;
  end;
end;

function BigComplexDecimalPolynomialCompose(Res: TCnBigComplexDecimalPolynomial;
  F: TCnBigComplexDecimalPolynomial; P: TCnBigComplexDecimalPolynomial): Boolean;
var
  I: Integer;
  Temp, Temp2, PowerP: TCnBigComplexDecimalPolynomial;
begin
  Result := True;

  if F.IsZero then
  begin
    Res.SetZero;
    Exit;
  end;

  Temp := TCnBigComplexDecimalPolynomial.Create;
  Temp2 := TCnBigComplexDecimalPolynomial.Create;
  PowerP := TCnBigComplexDecimalPolynomial.Create;
  try
    Res.SetZero;
    PowerP.SetOne;

    for I := 0 to F.MaxDegree do
    begin
      if I > 0 then
      begin
        BigComplexDecimalPolynomialMul(Temp2, PowerP, P);
        BigComplexDecimalPolynomialCopy(PowerP, Temp2);
      end;

      Temp.Clear;
      Temp.EnsureDegree(0);
      BigComplexDecimalCopy(Temp.Items[0], F.Items[I]);

      BigComplexDecimalPolynomialMul(Temp2, Temp, PowerP);
      BigComplexDecimalPolynomialAdd(Res, Res, Temp2);
    end;
  finally
    Temp.Free;
    Temp2.Free;
    PowerP.Free;
  end;
end;

procedure BigComplexDecimalPolynomialGetValue(Res: TCnBigComplexDecimal; F: TCnBigComplexDecimalPolynomial;
  X: TCnBigComplexDecimal);
var
  I: Integer;
  T, M: TCnBigComplexDecimal;
begin
  BigComplexDecimalCopy(Res, F[0]);
  if X.IsZero or (F.MaxDegree = 0) then    // 只有常数项的情况下，得常数项
    Exit;

  T := FLocalBigComplexDecimalPool.Obtain;
  M := FLocalBigComplexDecimalPool.Obtain;

  try
    BigComplexDecimalCopy(T, X);

    // 把 F 中的每个系数都和 X 的对应次幂相乘，最后相加
    for I := 1 to F.MaxDegree do
    begin
      BigComplexDecimalMul(M, F[I], T);
      BigComplexDecimalAdd(Res, Res, M);

      if I <> F.MaxDegree then
        BigComplexDecimalMul(T, T, X);
    end;
  finally
    FLocalBigComplexDecimalPool.Recycle(T);
    FLocalBigComplexDecimalPool.Recycle(M);
  end;
end;

initialization
  FLocalInt64PolynomialPool := TCnInt64PolynomialPool.Create;
  FLocalInt64RationalPolynomialPool := TCnInt64RationalPolynomialPool.Create;
  FLocalBigNumberPolynomialPool := TCnBigNumberPolynomialPool.Create;
  FLocalBigNumberRationalPolynomialPool := TCnBigNumberRationalPolynomialPool.Create;
  FLocalBigNumberPool := TCnBigNumberPool.Create;
  FLocalInt64BiPolynomialPool := TCnInt64BiPolynomialPool.Create;
  FLocalBigNumberBiPolynomialPool := TCnBigNumberBiPolynomialPool.Create;
  FLocalBigComplexPool := TCnBigComplexPool.Create;
  FLocalBigComplexPolynomialPool := TCnBigComplexPolynomialPool.Create;
  FLocalBigComplexDecimalPool := TCnBigComplexDecimalPool.Create;
  FLocalBigComplexDecimalPolynomialPool := TCnBigComplexDecimalPolynomialPool.Create;

  CnInt64PolynomialOne := TCnInt64Polynomial.Create([1]);
  CnInt64PolynomialZero := TCnInt64Polynomial.Create([0]);

  CnBigNumberPolynomialOne := TCnBigNumberPolynomial.Create([1]);
  CnBigNumberPolynomialZero := TCnBigNumberPolynomial.Create([0]);

finalization
  // CnInt64PolynomialOne.ToString; // 手工调用防止被编译器忽略

  CnBigNumberPolynomialOne.Free;
  CnBigNumberPolynomialZero.Free;

  CnInt64PolynomialOne.Free;
  CnInt64PolynomialZero.Free;

  FLocalBigComplexDecimalPolynomialPool.Free;
  FLocalBigComplexDecimalPool.Free;
  FLocalBigComplexPolynomialPool.Free;
  FLocalBigComplexPool.Free;
  FLocalBigNumberBiPolynomialPool.Free;
  FLocalInt64BiPolynomialPool.Free;
  FLocalInt64PolynomialPool.Free;
  FLocalInt64RationalPolynomialPool.Free;
  FLocalBigNumberPolynomialPool.Free;
  FLocalBigNumberRationalPolynomialPool.Free;
  FLocalBigNumberPool.Free;

end.
