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

unit CnMatrix;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：整数矩阵运算实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 Int64 范围内及大整数范围内的矩阵运算。

*           高阶行列式的代数余子式计算方法初步验证通过，矩阵求逆结果可能不是整数。
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行。另外 Hadamard 相关乘法，积矩阵可否是原矩阵待考察
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.07.01 V1.3
*               加入矩阵斜角索引的计算
*           2022.06.29 V1.2
*               加入浮点矩阵计算
*           2019.06.12 V1.1
*               加入有理数矩阵计算
*           2019.06.05 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, CnMath;

type
  ECnMatrixException = class(Exception);
  {* 矩阵相关异常}

  TCnIntMatrix = class(TPersistent)
  {* Int64 范围内的整数矩阵的实现类}
  private
    FMatrix: array of array of Int64;
    FColCount: Integer;
    FRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function GetValue(Row: Integer; Col: Integer): Int64;
    function GetZigZagValue(Index: Integer): Int64;
    procedure SetZigZagValue(Index: Integer; const Value: Int64);
  protected
    procedure SetValue(Row: Integer; Col: Integer; const AValue: Int64); virtual;

    function Add3(X: Int64; Y: Int64; Z: Int64): Int64; virtual;
    function Mul3(X: Int64; Y: Int64; Z: Int64): Int64; virtual;
    function NegativeOnePower(N: Integer): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    {* 内部赋值}
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    {* 构造函数。

       参数：
         ARowCount: Integer               - 指定矩阵行数
         AColCount: Integer               - 指定矩阵列数

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    // 供子类重载实现自定义的加减乘除操作，基类因为是整数，没有除操作
    function OperationAdd(X: Int64; Y: Int64): Int64; virtual;
    {* 元素加法操作。

       参数：
         X: Int64                         - 加数一
         Y: Int64                         - 加数二

       返回值：Int64                      - 返回和
    }

    function OperationSub(X: Int64; Y: Int64): Int64; virtual;
    {* 元素减法操作。

       参数：
         X: Int64                         - 被减数
         Y: Int64                         - 减数

       返回值：Int64                      - 返回差
    }

    function OperationMul(X: Int64; Y: Int64): Int64; virtual;
    {* 元素乘法操作。

       参数：
         X: Int64                         - 乘数一
         Y: Int64                         - 乘数二

       返回值：Int64                      - 返回积
    }

    function OperationDiv(X: Int64; Y: Int64): Int64; virtual;
    {* 元素除法操作，基类未实现，会抛异常。

       参数：
         X: Int64                         - 被除数
         Y: Int64                         - 除数

       返回值：Int64                      - 返回商
    }


    procedure Add(Factor: Int64);
    {* 矩阵各元素加上一个常数。

       参数：
         Factor: Int64                    - 加数

       返回值：（无）
    }

    procedure Mul(Factor: Int64);
    {* 矩阵各元素乘以一个常数。

       参数：
         Factor: Int64                    - 乘数

       返回值：（无）
    }

    procedure Divide(Factor: Int64); virtual;
    {* 矩阵各元素除以一个常数，基类因为是整数，未实现除法，会抛异常。

       参数：
         Factor: Int64                    - 除数

       返回值：（无）
    }

    procedure SetE(Size: Integer);
    {* 设置为 Size 阶单位矩阵。

       参数：
         Size: Integer                    - 阶数

       返回值：（无）
    }

    procedure SetZero;
    {* 设置为全 0 矩阵}

    procedure Transpose;
    {* 矩阵转置，也就是行列互换}

    function Determinant: Int64; virtual;
    {* 求方阵行列式值。

       参数：
         （无）

       返回值：Int64                      - 返回行列式值
    }

    function Trace: Int64;
    {* 求方阵的迹，也就是左上到右下的对角线元素的和。

       参数：
         （无）

       返回值：Int64                      - 返回迹
    }

    function IsSquare: Boolean;
    {* 是否方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否方阵
    }

    function IsZero: Boolean;
    {* 是否全 0 方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否全 0 方阵
    }

    function IsE: Boolean;
    {* 是否单位方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否单位方阵
    }

    function IsSymmetrical: Boolean;
    {* 是否对称方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否对称方阵
    }

    function IsSingular: Boolean;
    {* 是否奇异方阵，也就是行列式是否等于 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否奇异方阵
    }

    procedure DeleteRow(Row: Integer);
    {* 删除其中一行。

       参数：
         Row: Integer                     - 待删除的行号

       返回值：（无）
    }

    procedure DeleteCol(Col: Integer);
    {* 删除其中一列。

       参数：
         Col: Integer                     - 待删除的列号

       返回值：（无）
    }

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* 输出到字符串列表

       参数：
         List: TStrings                   - 用来容纳结果的字符串列表
         Sep: Char                        - 分隔符

       返回值：（无）
    }

    property Value[Row, Col: Integer]: Int64 read GetValue write SetValue; default;
    {* 根据行列下标访问矩阵元素，下标都从 0 开始}
    property ZigZagValue[Index: Integer]: Int64 read GetZigZagValue write SetZigZagValue;
    {* 方阵中左上角开始斜排的单值索引}
  published
    property ColCount: Integer read FColCount write SetColCount;
    {* 矩阵列数}
    property RowCount: Integer read FRowCount write SetRowCount;
    {* 矩阵行数}
  end;

  TCnFloatMatrix = class(TPersistent)
  {* 浮点数范围内的整数矩阵的实现类}
  private
    FMatrix: array of array of Extended;
    FColCount: Integer;
    FRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    function GetValue(Row: Integer; Col: Integer): Extended;
    function GetZigZagValue(Index: Integer): Extended;
    procedure SetZigZagValue(Index: Integer; const Value: Extended);
  protected
    procedure SetValue(Row: Integer; Col: Integer; const AValue: Extended); virtual;

    function Add3(X: Extended; Y: Extended; Z: Extended): Extended; virtual;
    function Mul3(X: Extended; Y: Extended; Z: Extended): Extended; virtual;
    function NegativeOnePower(N: Integer): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    {* 内部赋值}
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    {* 构造函数。

       参数：
         ARowCount: Integer               - 指定矩阵行数
         AColCount: Integer               - 指定矩阵列数

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    // 供子类重载实现自定义的加减乘除操作
    function OperationAdd(X: Extended; Y: Extended): Extended; virtual;
    {* 元素加法操作。

       参数：
         X: Extended                      - 加数一
         Y: Extended                      - 加数二

       返回值：Extended                   - 返回和
    }

    function OperationSub(X: Extended; Y: Extended): Extended; virtual;
    {* 元素减法操作。

       参数：
         X: Extended                      - 被减数
         Y: Extended                      - 减数

       返回值：Extended                   - 返回差
    }

    function OperationMul(X: Extended; Y: Extended): Extended; virtual;
    {* 元素乘法操作。

       参数：
         X: Extended                      - 乘数一
         Y: Extended                      - 乘数二

       返回值：Extended                   - 返回积
    }

    function OperationDiv(X: Extended; Y: Extended): Extended; virtual;
    {* 元素除法操作。

       参数：
         X: Extended                      - 被除数
         Y: Extended                      - 除数

       返回值：Extended                   - 返回商
    }

    procedure Add(Factor: Extended);
    {* 矩阵各元素加上一个常数

       参数：
         Factor: Extended                 - 加数

       返回值：（无）
    }

    procedure Mul(Factor: Extended);
    {* 矩阵各元素乘以一个常数。

       参数：
         Factor: Extended                 - 乘数

       返回值：（无）
    }

    procedure Divide(Factor: Extended); virtual;
    {* 矩阵各元素除以一个常数。

       参数：
         Factor: Extended                 - 除数

       返回值：（无）
    }

    procedure SetE(Size: Integer);
    {* 设置为 Size 阶单位矩阵。

       参数：
         Size: Integer                    - 阶数

       返回值：（无）
    }

    procedure SetZero;
    {* 设置为全 0 矩阵}

    procedure Transpose;
    {* 矩阵转置，也就是行列互换}

    function Determinant: Extended; virtual;
    {* 求方阵行列式值。

       参数：
         （无）

       返回值：Extended                   - 返回行列式值
    }

    function Trace: Extended;
    {* 求方阵的迹，也就是左上到右下的对角线元素的和。

       参数：
         （无）

       返回值：Extended                   - 返回迹
    }

    function IsSquare: Boolean;
    {* 是否方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否方阵
    }

    function IsZero: Boolean;
    {* 是否全 0 方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否全 0 方阵
    }

    function IsE: Boolean;
    {* 是否单位方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否单位方阵
    }

    function IsSymmetrical: Boolean;
    {* 是否对称方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否对称方阵
    }

    function IsSingular: Boolean;
    {* 是否奇异方阵，也就是行列式是否等于 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否奇异方阵
    }

    procedure DeleteRow(Row: Integer);
    {* 删除其中一行。

       参数：
         Row: Integer                     - 待删除的行号

       返回值：（无）
    }

    procedure DeleteCol(Col: Integer);
    {* 删除其中一列。

       参数：
         Col: Integer                     - 待删除的列号

       返回值：（无）
    }

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* 输出到字符串列表。

       参数：
         List: TStrings                   - 用来容纳结果的字符串列表
         Sep: Char                        - 分隔符

       返回值：（无）
    }

    property Value[Row, Col: Integer]: Extended read GetValue write SetValue; default;
    {* 根据行列下标访问矩阵元素，下标都从 0 开始}
    property ZigZagValue[Index: Integer]: Extended read GetZigZagValue write SetZigZagValue;
    {* 方阵中左上角开始斜排的单值索引}
  published
    property ColCount: Integer read FColCount write SetColCount;
    {* 矩阵列数}
    property RowCount: Integer read FRowCount write SetRowCount;
    {* 矩阵行数}
  end;

  TCnRationalNumber = class(TPersistent)
  {* 表示一个有理数，分子分母均在 Int64 范围内}
  private
    FNumerator: Int64;
    FDenominator: Int64;
    procedure SetDenominator(const Value: Int64);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    {* 内部赋值}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function IsInt: Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    {* 是否整数，也就是判断分母是否是正负 1。

       参数：
         （无）

       返回值：Boolean                    - 返回是否整数
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

    function IsNegative: Boolean;
    {* 是否为负值。

       参数：
         （无）

       返回值：Boolean                    - 返回是否负值
    }

    procedure Neg;
    {* 变成相反数}

    procedure Reciprocal;
    {* 变成倒数}

    procedure SetZero;
    {* 设为 0}

    procedure SetOne;
    {* 设为 1}

    function EqualInt(Value: Int64): Boolean;
    {* 是否与另一值相等。

       参数：
         Value: Int64                     - 待比较的整数

       返回值：Boolean                    - 返回是否相等
    }

    function Equal(Value: TCnRationalNumber): Boolean;
    {* 是否与另一值相等。

       参数：
         Value: TCnRationalNumber         - 待比较的有理数

       返回值：Boolean                    - 返回是否相等
    }

    procedure Add(Value: Int64); overload;
    {* 加上一个整数。

       参数：
         Value: Int64                     - 加数

       返回值：（无）
    }

    procedure Sub(Value: Int64); overload;
    {* 减去一个整数。

       参数：
         Value: Int64                     - 减数

       返回值：（无）
    }

    procedure Mul(Value: Int64); overload;
    {* 乘以一个整数。

       参数：
         Value: Int64                     - 乘数

       返回值：（无）
    }

    procedure Divide(Value: Int64); overload;
    {* 除以一个整数。

       参数：
         Value: Int64                     - 除数

       返回值：（无）
    }

    procedure Add(Value: TCnRationalNumber); overload;
    {* 加上一个有理数。

       参数：
         Value: TCnRationalNumber         - 加数

       返回值：（无）
    }

    procedure Sub(Value: TCnRationalNumber); overload;
    {* 减去一个有理数。

       参数：
         Value: TCnRationalNumber         - 减数

       返回值：（无）
    }

    procedure Mul(Value: TCnRationalNumber); overload;
    {* 乘以一个有理数。

       参数：
         Value: TCnRationalNumber         - 乘数

       返回值：（无）
    }

    procedure Divide(Value: TCnRationalNumber); overload;
    {* 除以一个有理数。

       参数：
         Value: TCnRationalNumber         - 除数

       返回值：（无）
    }

    procedure SetIntValue(Value: Int64);
    {* 值设为一个整数。

       参数：
         Value: Int64                     - 待设置的整数

       返回值：（无）
    }

    procedure SetValue(ANumerator: Int64; ADenominator: Int64);
    {* 值设为一个分数。

       参数：
         ANumerator: Int64                - 分子
         ADenominator: Int64              - 分母

       返回值：（无）
    }

    procedure SetString(const Value: string);
    {* 值设为一个字符串，可以是纯数字，或带 / 的分数。

       参数：
         const Value: string              - 待设置的字符串

       返回值：（无）
    }

    procedure Reduce;
    {* 尽量约分}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 输出成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property Numerator: Int64 read FNumerator write FNumerator;
    {* 分子}
    property Denominator: Int64 read FDenominator write SetDenominator;
    {* 分母}
  end;

  TCn2DObjectList = class
  {* 二维对象数组，拥有其中的对象}
  private
    FRowCount: Integer;
    FColCount: Integer;
    FRows: TObjectList;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
  protected
    function GetValueObject(Row: Integer; Col: Integer): TObject;
    procedure SetValueObject(Row: Integer; Col: Integer; const Value: TObject); // 一组 TObjectList
  public
    constructor Create(ARowCount: Integer; AColCount: Integer); virtual;
    {* 构造函数。

       参数：
         ARowCount: Integer               - 指定二维对象行数
         AColCount: Integer               - 指定二维对象列数

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure DeleteRow(Row: Integer);
    {* 删除其中一行。

       参数：
         Row: Integer                     - 待删除的行号

       返回值：（无）
    }

    procedure DeleteCol(Col: Integer);
    {* 删除其中一列。

       参数：
         Col: Integer                     - 待删除的列号

       返回值：（无）
    }

    property ValueObject[Row, Col: Integer]: TObject read GetValueObject write SetValueObject; default;
    {* 二维数组值}
    property RowCount: Integer read GetRowCount write SetRowCount;
    {* 行数}
    property ColCount: Integer read GetColCount write SetColCount;
    {* 列数}
  end;

  TCnRationalMatrix = class(TPersistent)
  {* 有理数范围内的整数矩阵的实现类}
  private
    FMatrix: TCn2DObjectList;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    procedure SetValue(Row: Integer; Col: Integer; const Value: TCnRationalNumber);
    function GetValue(Row: Integer; Col: Integer): TCnRationalNumber;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetZigZagValue(Index: Integer): TCnRationalNumber;
    procedure SetZigZagValue(Index: Integer;
      const Value: TCnRationalNumber);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    {* 内部赋值}
  public
    constructor Create(ARowCount: Integer = 1; AColCount: Integer = 1); virtual;
    {* 构造函数。

       参数：
         ARowCount: Integer               - 指定矩阵行数
         AColCount: Integer               - 指定矩阵列数

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}


    procedure Add(Factor: Int64); overload;
    {* 矩阵各元素加上一个常数。

       参数：
         Factor: Int64                    - 加数

       返回值：（无）
    }

    procedure Mul(Factor: Int64); overload;
    {* 矩阵各元素乘以一个常数。

       参数：
         Factor: Int64                    - 乘数

       返回值：（无）
    }

    procedure Divide(Factor: Int64); overload;
    {* 矩阵各元素除以一个常数。

       参数：
         Factor: Int64                    - 除数

       返回值：（无）
    }

    procedure Add(Factor: TCnRationalNumber); overload;
    {* 矩阵各元素加上一个常数。

       参数：
         Factor: TCnRationalNumber        - 加数

       返回值：（无）
    }

    procedure Mul(Factor: TCnRationalNumber); overload;
    {* 矩阵各元素乘以一个常数。

       参数：
         Factor: TCnRationalNumber        - 乘数

       返回值：（无）
    }

    procedure Divide(Factor: TCnRationalNumber); overload;
    {* 矩阵各元素除以一个常数。

       参数：
         Factor: TCnRationalNumber        - 除数

       返回值：（无）
    }

    procedure SetE(Size: Integer);
    {* 设置为 Size 阶单位矩阵。

       参数：
         Size: Integer                    - 阶数

       返回值：（无）
    }

    procedure SetZero;
    {* 设置为全 0 矩阵}

    procedure Transpose;
    {* 矩阵转置，也就是行列互换}

    procedure DeleteRow(Row: Integer);
    {* 删除其中一行。

       参数：
         Row: Integer                     - 待删除的行号

       返回值：（无）
    }

    procedure DeleteCol(Col: Integer);
    {* 删除其中一列。

       参数：
         Col: Integer                     - 待删除的列号

       返回值：（无）
    }

    procedure Determinant(D: TCnRationalNumber);
    {* 求方阵行列式值。

       参数：
         D: TCnRationalNumber             - 用来容纳结果的有理数

       返回值：（无）
    }

    procedure Trace(T: TCnRationalNumber);
    {* 求方阵的迹，也就是对角线元素的和。

       参数：
         T: TCnRationalNumber             - 用来容纳结果的有理数

       返回值：（无）
    }

    function IsSquare: Boolean;
    {* 是否方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否方阵
    }

    function IsZero: Boolean;
    {* 是否全 0 方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否全 0 方阵
    }

    function IsE: Boolean;
    {* 是否单位方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否单位方阵
    }

    function IsSymmetrical: Boolean;
    {* 是否对称方阵。

       参数：
         （无）

       返回值：Boolean                    - 返回是否对称方阵
    }

    function IsSingular: Boolean;
    {* 是否奇异方阵，也就是行列式是否等于 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否奇异方阵
    }

    procedure DumpToStrings(List: TStrings; Sep: Char = ' ');
    {* 输出到字符串列表。

       参数：
         List: TStrings                   - 用来容纳结果的字符串列表
         Sep: Char                        - 分隔符

       返回值：（无）
    }

    property Value[Row, Col: Integer]: TCnRationalNumber read GetValue write SetValue; default;
    {* 根据行列下标访问矩阵元素，下标都从 0 开始}
    property ZigZagValue[Index: Integer]: TCnRationalNumber read GetZigZagValue write SetZigZagValue;
    {* 方阵中左上角开始斜排的单值索引}
  published
    property ColCount: Integer read GetColCount write SetColCount;
    {* 矩阵列数}
    property RowCount: Integer read GetRowCount write SetRowCount;
    {* 矩阵行数}
  end;

// ============================ 整数矩阵运算方法 ===============================

procedure CnMatrixMul(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix); overload;
{* 两个矩阵相乘，结果放 MulResult 矩阵中，要求 Matrix1 列数与 Martrix2 行数相等。
   MulResult 不能是 Matrix1 或 Matrix2。

   参数：
     Matrix1: TCnIntMatrix                - 乘数矩阵一
     Matrix2: TCnIntMatrix                - 乘数矩阵二
     MulResult: TCnIntMatrix              - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix); overload;
{* 求方阵 K 次幂，结果放 PowerResult 矩阵中，PowerResult 不能是 Matrix。

   参数：
     Matrix: TCnIntMatrix                 - 底数矩阵
     K: Integer                           - 指数
     PowerResult: TCnIntMatrix            - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixAdd(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix; AddResult: TCnIntMatrix); overload;
{* 两个矩阵相加，结果放 AddResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 行数相等。
   AddResult 可以是 Matrix1 或 Matrix2。

   参数：
     Matrix1: TCnIntMatrix                - 加数矩阵一
     Matrix2: TCnIntMatrix                - 加数矩阵二
     AddResult: TCnIntMatrix              - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixHadamardProduct(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix); overload;
{* 两个矩阵哈达马相乘，结果放 ProductResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 相等。
   ProductResult 可以是 Matrix1 或 Matrix2 或其他。

   参数：
     Matrix1: TCnIntMatrix                - 乘数矩阵一
     Matrix2: TCnIntMatrix                - 乘数矩阵二
     ProductResult: TCnIntMatrix          - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixTranspose(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix); overload;
{* 转置矩阵，将第一个矩阵转置至第二个，Matrix1、Matrix2 可以相等。

   参数：
     Matrix1: TCnIntMatrix                - 待转置的原矩阵
     Matrix2: TCnIntMatrix                - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixMinor(Matrix: TCnIntMatrix; Row: Integer; Col: Integer;
  MinorResult: TCnIntMatrix); overload;
{* 求矩阵的余子式，也即去除指定行列后剩下的矩阵。

   参数：
     Matrix: TCnIntMatrix                 - 待求余子式的原矩阵
     Row: Integer                         - 去除的行号
     Col: Integer                         - 去除的列号
     MinorResult: TCnIntMatrix            - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixAdjoint(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix); overload;
{* 求方阵的伴随阵。

   参数：
     Matrix1: TCnIntMatrix                - 待求伴随阵的原矩阵
     Matrix2: TCnIntMatrix                - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixInverse(Matrix1: TCnIntMatrix; Matrix2: TCnIntMatrix); overload;
{* 求方阵的逆矩阵，也就是伴随阵除以行列式，注意 TCnIntMatrix 不直接支持逆矩阵，
   因为除可能导致非整数，需要改用有理数矩阵来表示，或子类伽罗华矩阵。

   参数：
     Matrix1: TCnIntMatrix                - 待求逆矩阵的原矩阵
     Matrix2: TCnIntMatrix                - 用来容纳结果的矩阵

   返回值：（无）
}

// =========================== 浮点数矩阵运算方法 ==============================

procedure CnMatrixMul(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix; MulResult: TCnFloatMatrix); overload;
{* 两个矩阵相乘，结果放 MulResult 矩阵中，要求 Matrix1 列数与 Martrix2 行数相等。
   MulResult 不能是 Matrix1 或 Matrix2。

   参数：
     Matrix1: TCnFloatMatrix              - 乘数矩阵一
     Matrix2: TCnFloatMatrix              - 乘数矩阵二
     MulResult: TCnFloatMatrix            - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixPower(Matrix: TCnFloatMatrix; K: Integer; PowerResult: TCnFloatMatrix); overload;
{* 求方阵 K 次幂，结果放 PowerResult 矩阵中，PowerResult 不能是 Matrix。

   参数：
     Matrix: TCnFloatMatrix               - 底数矩阵
     K: Integer                           - 指数
     PowerResult: TCnFloatMatrix          - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixAdd(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix;
  AddResult: TCnFloatMatrix); overload;
{* 两个矩阵相加，结果放 AddResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 行数相等。
   AddResult 可以是 Matrix1 或 Matrix2。

   参数：
     Matrix1: TCnFloatMatrix              - 加数矩阵一
     Matrix2: TCnFloatMatrix              - 加数矩阵二
     AddResult: TCnFloatMatrix            - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixHadamardProduct(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix;
  ProductResult: TCnFloatMatrix); overload;
{* 两个矩阵哈达马相乘，结果放 ProductResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 相等。
   ProductResult 可以是 Matrix1 或 Matrix2 或其他。

   参数：
     Matrix1: TCnFloatMatrix              - 乘数矩阵一
     Matrix2: TCnFloatMatrix              - 乘数矩阵二
     ProductResult: TCnFloatMatrix        - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixTranspose(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix); overload;
{* 转置矩阵，将第一个矩阵转置至第二个，Matrix1、Matrix2 可以相等。

   参数：
     Matrix1: TCnFloatMatrix              - 待转置的原矩阵
     Matrix2: TCnFloatMatrix              - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixMinor(Matrix: TCnFloatMatrix; Row: Integer; Col: Integer;
  MinorResult: TCnFloatMatrix); overload;
{* 求矩阵的余子式，也即去除指定行列后剩下的矩阵。

   参数：
     Matrix: TCnFloatMatrix               - 待求余子式的原矩阵
     Row: Integer                         - 去除的行号
     Col: Integer                         - 去除的列号
     MinorResult: TCnFloatMatrix          - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixAdjoint(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix); overload;
{* 求方阵的伴随阵。

   参数：
     Matrix1: TCnFloatMatrix              - 待求伴随阵的原矩阵
     Matrix2: TCnFloatMatrix              - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixInverse(Matrix1: TCnFloatMatrix; Matrix2: TCnFloatMatrix); overload;
{* 求方阵的逆矩阵，也就是伴随阵除以行列式。

   参数：
     Matrix1: TCnFloatMatrix              - 待求逆矩阵的原矩阵
     Matrix2: TCnFloatMatrix              - 用来容纳结果的矩阵

   返回值：（无）
}

// =========================== 有理数矩阵运算方法 ==============================

procedure CnIntToRationalMatrix(Int: TCnIntMatrix; Rational: TCnRationalMatrix);
{* 将一个整数矩阵转换为有理数矩阵。

   参数：
     Int: TCnIntMatrix                    - 待转换的整数矩阵
     Rational: TCnRationalMatrix          - 用来容纳结果的有理数矩阵

   返回值：（无）
}

procedure CnMatrixMul(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix;
  MulResult: TCnRationalMatrix); overload;
{* 两个矩阵相乘，结果放 MulResult 矩阵中，要求 Matrix1 列数与 Martrix2 行数相等。
   MulResult 不能是 Matrix1 或 Matrix2。

   参数：
     Matrix1: TCnRationalMatrix           - 乘数矩阵一
     Matrix2: TCnRationalMatrix           - 乘数矩阵二
     MulResult: TCnRationalMatrix         - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixPower(Matrix: TCnRationalMatrix; K: Integer; PowerResult: TCnRationalMatrix); overload;
{* 求方阵 K 次幂，结果放 PowerResult 矩阵中，PowerResult 不能是 Matrix。

   参数：
     Matrix: TCnRationalMatrix            - 底数矩阵
     K: Integer                           - 指数
     PowerResult: TCnRationalMatrix       - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixAdd(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix;
  AddResult: TCnRationalMatrix); overload;
{* 两个矩阵相加，结果放 AddResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 相等。
   AddResult 可以是 Matrix1 或 Matrix2。

   参数：
     Matrix1: TCnRationalMatrix           - 加数矩阵一
     Matrix2: TCnRationalMatrix           - 加数矩阵二
     AddResult: TCnRationalMatrix         - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixHadamardProduct(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix;
  ProductResult: TCnRationalMatrix); overload;
{* 两个矩阵哈达马相乘，结果放 ProductResult 矩阵中，要求 Matrix1 尺寸与 Martrix2 相等。
   ProductResult 可以是 Matrix1 或 Matrix2。

   参数：
     Matrix1: TCnRationalMatrix           - 乘数矩阵一
     Matrix2: TCnRationalMatrix           - 乘数矩阵二
     ProductResult: TCnRationalMatrix     - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixTranspose(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix); overload;
{* 转置矩阵，将第一个矩阵转置至第二个，Matrix1、Matrix2 可以相等。

   参数：
     Matrix1: TCnRationalMatrix           - 待转置的原矩阵
     Matrix2: TCnRationalMatrix           - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixMinor(Matrix: TCnRationalMatrix; Row: Integer; Col: Integer;
  MinorResult: TCnRationalMatrix); overload;
{* 求矩阵的余子式，也即去除指定行列后剩下的矩阵。

   参数：
     Matrix: TCnRationalMatrix            - 待求余子式的原矩阵
     Row: Integer                         - 去除的行号
     Col: Integer                         - 去除的列号
     MinorResult: TCnRationalMatrix       - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixAdjoint(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix); overload;
{* 求方阵的伴随阵。

   参数：
     Matrix1: TCnRationalMatrix           - 待求伴随阵的原矩阵
     Matrix2: TCnRationalMatrix           - 用来容纳结果的矩阵

   返回值：（无）
}

procedure CnMatrixInverse(Matrix1: TCnRationalMatrix; Matrix2: TCnRationalMatrix); overload;
{* 求方阵的逆矩阵，也就是伴随阵除以行列式，需要有理数矩阵来表示。

   参数：
     Matrix1: TCnRationalMatrix           - 待求逆矩阵的原矩阵
     Matrix2: TCnRationalMatrix           - 用来容纳结果的矩阵

   返回值：（无）
}

// ============================== 有理数运算方法 ===============================

procedure CnRationalNumberAdd(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* 有理数加法，三个参数可以是同一对象。

   参数：
     Number1: TCnRationalNumber           - 加数一
     Number2: TCnRationalNumber           - 加数二
     RationalResult: TCnRationalNumber    - 用来容纳结果的有理数

   返回值：（无）
}

procedure CnRationalNumberAdd3(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* 有理数三个数加法，RationalResult 不能是 Number1 或 Number2 或 Number3。

   参数：
     Number1: TCnRationalNumber           - 加数一
     Number2: TCnRationalNumber           - 加数二
     Number3: TCnRationalNumber           - 加数三
     RationalResult: TCnRationalNumber    - 用来容纳结果的有理数

   返回值：（无）
}

procedure CnRationalNumberSub(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* 有理数减法，三个参数可以是同一对象。

   参数：
     Number1: TCnRationalNumber           - 被减数
     Number2: TCnRationalNumber           - 减数
     RationalResult: TCnRationalNumber    - 用来容纳结果的有理数

   返回值：（无）
}

procedure CnRationalNumberMul(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* 有理数乘法，三个参数可以是同一对象。

   参数：
     Number1: TCnRationalNumber           - 乘数一
     Number2: TCnRationalNumber           - 乘数二
     RationalResult: TCnRationalNumber    - 用来容纳结果的有理数

   返回值：（无）
}

procedure CnRationalNumberMul3(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
{* 有理数三个数乘法，RationalResult 不能是 Number1 或 Number2。

   参数：
     Number1: TCnRationalNumber           - 乘数一
     Number2: TCnRationalNumber           - 乘数二
     Number3: TCnRationalNumber           - 乘数三
     RationalResult: TCnRationalNumber    - 用来容纳结果的有理数

   返回值：（无）
}

procedure CnRationalNumberDiv(Number1: TCnRationalNumber; Number2: TCnRationalNumber;
  RationalResult: TCnRationalNumber);
{* 有理数除法，三个参数可以是同一对象。

   参数：
     Number1: TCnRationalNumber           - 被除数
     Number2: TCnRationalNumber           - 除数
     RationalResult: TCnRationalNumber    - 用来容纳结果的有理数

   返回值：（无）
}

function CnRationalNumberCompare(Number1: TCnRationalNumber; Number2: TCnRationalNumber): Integer;
{* 比较两个有理数，前者大于、等于、小于后者时分别返回 1、0、-1。

   参数：
     Number1: TCnRationalNumber           - 待比较的有理数一
     Number2: TCnRationalNumber           - 待比较的有理数二

   返回值：Integer                        - 返回比较结果
}

procedure CnReduceInt64(var X: Int64; var Y: Int64);
{* 尽量按比例缩小，也就是约分。

   参数：
     var X: Int64                         - 待约分的分子
     var Y: Int64                         - 待约分的分母

   返回值：（无）
}

function RowColToZigZag(ARow: Integer; ACol: Integer; N: Integer): Integer;
{* 将 N 阶方阵中的行列值转换为左上角斜排的索引值，均 0 开始。

   参数：
     ARow: Integer                        - 行号
     ACol: Integer                        - 列号
     N: Integer                           - 方阵阶数

   返回值：Integer                        - 返回索引值
}

procedure ZigZagToRowCol(Index: Integer; out ARow: Integer; out ACol: Integer; N: Integer);
{* 将 N 阶方阵中的左上角斜排的索引值转换为行列值，均 0 开始。

   参数：
     Index: Integer                       - 索引值
     out ARow: Integer                    - 输出的行号
     out ACol: Integer                    - 输出的列号
     N: Integer                           - 方阵阶数

   返回值：（无）
}

implementation

resourcestring
  SCnErrorRowColCountFmt = 'Error Row or Col Count: %d';
  SCnErrorRowColIndexFmt = 'Error Row or Col: %d';
  SCnErrorRowColIndex2Fmt = 'Error Row or Col: %d, %d';
  SCnErrorZigZagIndexFmt = 'Error ZigZag Index: %d';
  SCnErrorZigZagRowColCount = 'ZigZag Row Col Count must Equal';
  SCnErrorResultFactors = 'Matrix Result can not Be Factors';
  SCnErrorMulRowCount = 'Matrix 1 Col Count must Equal to Matrix 2 Row Count';
  SCnErrorPowerSquare = 'Matrix Power Must Be Square';
  SCnErrorDivNotImplInt = 'Operation Div NOT Implemented in Int Matrix';
  SCnErrorTraceSquare = 'Only Square Matrix can Trace';
  SCnErrorInvalidPower = 'Invalid Matrix Power';
  SCnErrorRowColMustEqual = 'Matrix 1/2 Row/Col Count must Equal';
  SCnErrorRowColMinorFmt = 'Invalid Minor Row or Col %d, %d';
  SCnErrorAdjointSquare = 'Only Square can Adjoint';
  SCnErrorInverseZeroDeteminant = 'NO Inverse Matrix for Deteminant is 0';
  SCnErrorDeterminantSquare = 'Only Square can Determinant';

procedure CheckCount(Value: Integer);
begin
  if Value <= 0 then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColCountFmt, [Value]);
end;

procedure CheckIndex(Value: Integer);
begin
  if Value < 0 then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColIndexFmt, [Value]);
end;

{
  0  1  5  6
  2  4  7 12
  3  8 11 13
  9 10 14 15
}
// 将 N 阶方阵中的行列值转换为左上角斜排的索引值，均 0 开始
function RowColToZigZag(ARow, ACol: Integer; N: Integer): Integer;
var
  L, A, T: Integer;
begin
  CheckIndex(ARow);
  CheckIndex(ACol);
  if (ARow >= N) or (ACol >= N) then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColIndex2Fmt, [ARow, ACol]);

  // 在第 Row + Col + 1 斜层（左上角为第 1 斜层），先求之前完整斜层的数量和，再求本斜层内的偏移
  // 最长的斜层是第 N 层（共 N(N+1)/2 个），总共有 2N - 1 层
  L := ARow + ACol;
  if L <= N then
    A := L * (L + 1) div 2 // A 是之前完整斜层的数量和
  else
  begin
    A := N * (N + 1) div 2;  // 1 到 N 的斜层和
    T := 2 * N - 1 - (ARow + ACol);
    A := A + (A - N - (T * (T + 1) div 2)); // N + 1 后的斜层和
  end;
  // A 是之前完整斜层的数量和

  if L and 1 = 0 then
  begin
    // 本身奇数斜层，左下往右上排
    if L < N then
      Result := A + ACol
    else
    Result := A + ACol - (L + 1 - N);
  end
  else
  begin
    // 本身偶数斜层，右上往左下排
    if L < N then
      Result := A + ARow
    else
      Result := A + ARow - (L + 1 - N);
  end;
end;

// 将 N 阶方阵中的左上角斜排的索引值转换为行列值，均 0 开始
procedure ZigZagToRowCol(Index: Integer; out ARow, ACol: Integer; N: Integer);
var
  L, A: Integer;

  procedure FindLevelIndex(var Level, IndexLevel: Integer);
  var
    TA: Integer;
  begin
    TA := Trunc(Sqrt(IndexLevel * 2 + 2));
    if TA * TA + TA < IndexLevel * 2 + 2 then
    begin
      Level := TA;
      IndexLevel := IndexLevel - (TA * TA + TA) div 2;
    end
    else
    begin
      Level := TA - 1;
      if Level < 0 then
        Level := 0;
      IndexLevel := IndexLevel - (Level * Level + Level) div 2;
    end;
  end;

begin
  CheckIndex(Index);
  if Index > (N * N - 1) then
    raise ECnMatrixException.CreateFmt(SCnErrorZigZagIndexFmt, [Index]);

  A := N * (N + 1) div 2; // A 是从左上到包括对角线在内的所有数量

  L := 0;
  if Index < A then
  begin
    // 在左上区，包括对角线
    FindLevelIndex(L, Index);
    // L 是左上不包括自己的完整层数（可以为 0），且有 Row + Col = L，Index 是本层中的偏移，0 开始（方向待判断）

    if L and 1 = 0 then
    begin
      // 本身在左上区的奇数斜层，左下往右上排
      ACol := Index;
      ARow := L - Index;
    end
    else
    begin
      // 本身在左上区的偶数斜层，右上往左下排
      ARow := Index;
      ACol := L - Index;
    end;
  end
  else
  begin
    // 在右下区，不包括对角线
    Index := N * N - 1 - Index;
    FindLevelIndex(L, Index);
    Index := L - Index;
    // L 是右下不包括自己的完整层数（可以为 0），且有 (N - 1 - Row) + (N - 1 - Col) = L，
    // 也就是 (Row + Col = 2N - 2 - L)，且 Index 是本层中的偏移，0 开始（方向待判断）

    if L and 1 = 0 then
    begin
      // 本身在右下区的奇数斜层，左下往右上排
      ARow := N - 1 - Index;
      ACol := 2 * N - 2 - L - ARow;
    end
    else
    begin
      // 本身在右下区的偶数斜层，右上往左下排
      ACol := N - 1 - Index;
      ARow := 2 * N - 2 - L - ACol;
    end;
  end;
end;

// 计算 -1 的 N 次方，供求代数余子式用
function InternalNegativeOnePower(N: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (N and 1) * (-2) + 1;
end;

procedure CnIntToRationalMatrix(Int: TCnIntMatrix; Rational: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Int <> nil) and (Rational <> nil) then
  begin
    Rational.ColCount := Int.ColCount;
    Rational.RowCount := Int.RowCount;

    for I := 0 to Rational.RowCount - 1 do
    begin
      for J := 0 to Rational.ColCount - 1 do
        Rational.Value[I, J].SetIntValue(Int.Value[I, J]);
    end;
  end;
end;

procedure CnMatrixMul(Matrix1, Matrix2: TCnIntMatrix; MulResult: TCnIntMatrix);
var
  I, J, K: Integer;
  T, Sum: Int64;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create(SCnErrorResultFactors);

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create(SCnErrorMulRowCount);

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

procedure CnMatrixMul(Matrix1, Matrix2: TCnFloatMatrix; MulResult: TCnFloatMatrix);
var
  I, J, K: Integer;
  T, Sum: Extended;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create(SCnErrorResultFactors);

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create(SCnErrorMulRowCount);

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

procedure CnMatrixMul(Matrix1, Matrix2: TCnRationalMatrix; MulResult: TCnRationalMatrix);
var
  I, J, K: Integer;
  T, Sum: TCnRationalNumber;
begin
  if (MulResult = Matrix1) or (MulResult = Matrix2) then
    raise ECnMatrixException.Create(SCnErrorResultFactors);

  if Matrix1.ColCount <> Matrix2.RowCount then
    raise ECnMatrixException.Create(SCnErrorMulRowCount);

  MulResult.RowCount := Matrix1.RowCount;
  MulResult.ColCount := Matrix2.ColCount;

  Sum := TCnRationalNumber.Create;
  T := TCnRationalNumber.Create;

  // Value[I, J] := 矩阵 1 第 I 行与矩阵 2 第 J 列对应乘并相加
  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        Sum.SetIntValue(0);
        for K := 0 to Matrix1.ColCount - 1 do
        begin
          CnRationalNumberMul(Matrix1.Value[I, K], Matrix2.Value[K, J], T);
          CnRationalNumberAdd(Sum, T, Sum);
        end;
        MulResult.Value[I, J] := Sum;
      end;
    end;
  finally
    Sum.Free;
  end;
end;

procedure CnMatrixPower(Matrix: TCnIntMatrix; K: Integer; PowerResult: TCnIntMatrix);
var
  I: Integer;
  T: TCnIntMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create(SCnErrorPowerSquare);

  if K < 0 then
    raise ECnMatrixException.Create(SCnErrorInvalidPower);

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

procedure CnMatrixPower(Matrix: TCnFloatMatrix; K: Integer; PowerResult: TCnFloatMatrix);
var
  I: Integer;
  T: TCnFloatMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create(SCnErrorPowerSquare);

  if K < 0 then
    raise ECnMatrixException.Create(SCnErrorInvalidPower);

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

  T := TCnFloatMatrix.Create(Matrix.RowCount, Matrix.ColCount);
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

procedure CnMatrixPower(Matrix: TCnRationalMatrix; K: Integer; PowerResult: TCnRationalMatrix);
var
  I: Integer;
  T: TCnRationalMatrix;
begin
  if not Matrix.IsSquare then
    raise ECnMatrixException.Create(SCnErrorPowerSquare);

  if K < 0 then
    raise ECnMatrixException.Create(SCnErrorInvalidPower);

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

  T := TCnRationalMatrix.Create(Matrix.RowCount, Matrix.ColCount);
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
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      AddResult.Value[I, J] := Matrix1.OperationAdd(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnFloatMatrix; AddResult: TCnFloatMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      AddResult.Value[I, J] := Matrix1.OperationAdd(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixAdd(Matrix1, Matrix2: TCnRationalMatrix; AddResult: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  AddResult.RowCount := Matrix1.RowCount;
  AddResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      CnRationalNumberAdd(Matrix1.Value[I, J], Matrix2.Value[I, J], AddResult.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnIntMatrix; ProductResult: TCnIntMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      ProductResult.Value[I, J] := Matrix1.OperationMul(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnFloatMatrix; ProductResult: TCnFloatMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      ProductResult.Value[I, J] := Matrix1.OperationMul(Matrix1.Value[I, J], Matrix2.Value[I, J]);
end;

procedure CnMatrixHadamardProduct(Matrix1, Matrix2: TCnRationalMatrix; ProductResult: TCnRationalMatrix);
var
  I, J: Integer;
begin
  if (Matrix1.ColCount <> Matrix2.ColCount) or (Matrix1.RowCount <> Matrix2.RowCount) then
    raise ECnMatrixException.Create(SCnErrorRowColMustEqual);

  ProductResult.RowCount := Matrix1.RowCount;
  ProductResult.ColCount := Matrix1.ColCount;
  for I := 0 to Matrix1.RowCount - 1 do
    for J := 0 to Matrix1.ColCount - 1 do
      CnRationalNumberMul(Matrix1.Value[I, J], Matrix2.Value[I, J], ProductResult.Value[I, J]);
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

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnFloatMatrix);
var
  I, J: Integer;
  Tmp: TCnFloatMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnFloatMatrix.Create(1, 1);
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

procedure CnMatrixTranspose(Matrix1, Matrix2: TCnRationalMatrix);
var
  I, J: Integer;
  Tmp: TCnRationalMatrix;
begin
  if Matrix1 = Matrix2 then
  begin
    Tmp := TCnRationalMatrix.Create(1, 1);
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
    raise ECnMatrixException.CreateFmt(SCnErrorRowColMinorFmt, [Row, Col]);

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

procedure CnMatrixMinor(Matrix: TCnFloatMatrix; Row, Col: Integer; MinorResult: TCnFloatMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColMinorFmt, [Row, Col]);

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

procedure CnMatrixMinor(Matrix: TCnRationalMatrix; Row, Col: Integer; MinorResult: TCnRationalMatrix);
var
  SR, SC, DR, DC: Integer;
begin
  if ((Row < 0) or (Row >= Matrix.RowCount)) or
    ((Col < 0) or (Col >= Matrix.ColCount)) then
    raise ECnMatrixException.CreateFmt(SCnErrorRowColMinorFmt, [Row, Col]);

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
    raise ECnMatrixException.Create(SCnErrorAdjointSquare);

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnIntMatrix(Matrix1.ClassType.NewInstance);
  Minor.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1); // 用子类实现

  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Matrix2.Value[I, J] := Matrix1.NegativeOnePower(I + J) * Minor.Determinant;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    Minor.Free;
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnFloatMatrix);
var
  I, J: Integer;
  Minor: TCnFloatMatrix;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create(SCnErrorAdjointSquare);

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnFloatMatrix(Matrix1.ClassType.NewInstance);
  Minor.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1); // 用子类实现

  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Matrix2.Value[I, J] := Matrix1.NegativeOnePower(I + J) * Minor.Determinant;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    Minor.Free;
  end;
end;

procedure CnMatrixAdjoint(Matrix1, Matrix2: TCnRationalMatrix);
var
  I, J: Integer;
  Minor: TCnRationalMatrix;
  T: TCnRationalNumber;
begin
  if not Matrix1.IsSquare then
    raise ECnMatrixException.Create(SCnErrorAdjointSquare);

  Matrix2.RowCount := Matrix1.RowCount;
  Matrix2.ColCount := Matrix1.ColCount;

  Minor := TCnRationalMatrix.Create(Matrix1.RowCount - 1, Matrix1.ColCount - 1);
  T := TCnRationalNumber.Create;
  try
    for I := 0 to Matrix1.RowCount - 1 do
    begin
      for J := 0 to Matrix2.ColCount - 1 do
      begin
        CnMatrixMinor(Matrix1, I, J, Minor);
        Minor.Determinant(T);
        T.Mul(InternalNegativeOnePower(I + J));
        Matrix2.Value[I, J] := T;
      end;
    end;
    CnMatrixTranspose(Matrix2, Matrix2);
  finally
    T.Free;
    Minor.Free;
  end;
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnIntMatrix);
var
  D: Int64;
begin
  D := Matrix1.Determinant;
  if D = 0 then
    raise ECnMatrixException.Create(SCnErrorInverseZeroDeteminant);

  CnMatrixAdjoint(Matrix1, Matrix2);
  Matrix2.Divide(D);
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnFloatMatrix);
var
  D: Extended;
begin
  D := Matrix1.Determinant;
  if FloatAlmostZero(D) then
    raise ECnMatrixException.Create(SCnErrorInverseZeroDeteminant);

  CnMatrixAdjoint(Matrix1, Matrix2);
  Matrix2.Divide(D);
end;

procedure CnMatrixInverse(Matrix1, Matrix2: TCnRationalMatrix);
var
  D: TCnRationalNumber;
begin
  D := TCnRationalNumber.Create;
  try
    Matrix1.Determinant(D);
    if D.IsZero then
      raise ECnMatrixException.Create(SCnErrorInverseZeroDeteminant);

    CnMatrixAdjoint(Matrix1, Matrix2);
    Matrix2.Divide(D);
  finally
    D.Free;
  end;
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

function TCnIntMatrix.Add3(X, Y, Z: Int64): Int64;
begin
  Result := OperationAdd(OperationAdd(X, Y), Z);
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

constructor TCnIntMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRowCount := ARowCount;
  FColCount := AColCount;
  SetLength(FMatrix, FRowCount, FColCount);
end;

procedure TCnIntMatrix.DeleteCol(Col: Integer);
var
  T: array of array of Int64;
  I, J, SJ, DJ: Integer;
begin
  if (Col >= 0) or (Col < FColCount) then
  begin
    // 把每 Row 的元素取出来放到临时 T 里，剔除第 Col 个
    SetLength(T, FRowCount, FColCount - 1);

    for I := 0 to FRowCount - 1 do
    begin
      SJ := 0;
      DJ := 0;
      while SJ < FColCount do
      begin
        if SJ = Col then
        begin
          Inc(SJ);
          Continue;
        end;
        T[I, DJ] := FMatrix[I, SJ];
        Inc(SJ);
        Inc(DJ);
      end;
    end;

    Dec(FColCount);
    SetLength(FMatrix, FRowCount, FColCount);
    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        FMatrix[I, J] := T[I, J];

    SetLength(T, 0);
  end;
end;

procedure TCnIntMatrix.DeleteRow(Row: Integer);
var
  I, J: Integer;
begin
  if (Row >= 0) or (Row < FRowCount) then
  begin
    // 把第 Row + 1 行到 FRowCount - 1 行的一维数组朝前移动一格，末行时无需移
    if Row < FRowCount - 1 then
    begin
      for I := Row + 1 to FRowCount - 1 do
      begin
        for J := 0 to FColCount - 1 do
        begin
          FMatrix[I - 1, J] := FMatrix[I, J];
        end;
      end;
    end;
    Dec(FRowCount);
    SetLength(FMatrix, FRowCount, FColCount);
  end;
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
    raise ECnMatrixException.Create(SCnErrorDeterminantSquare);

  if FRowCount = 1 then
    Result := FMatrix[0, 0]
  else if FRowCount = 2 then
    Result := FMatrix[0, 0] * FMatrix[1, 1] - FMatrix[0, 1] * FMatrix[1, 0]
  else if RowCount = 3 then
  begin
    Result := OperationSub(Add3(Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2]),
      Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0]),
      Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])),
      Add3(Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1]),
        Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2]),
        Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0])));
  end
  else
  begin
    // 利用代数余子式 Minor/Cofactor 计算高阶行列式
    Result := 0;
    Minor := TCnIntMatrix(ClassType.NewInstance); // 需要用子类进行统一的运算
    Minor.Create(FRowCount - 1, FColCount - 1);

    // Minor := Self.clas TCnIntMatrix.Create(FRowCount - 1, FColCount - 1);
    try
      for I := 0 to FColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);
        Result := OperationAdd(Result, Mul3(FMatrix[0, I], NegativeOnePower(I), Minor.Determinant));
      end;
    finally
      Minor.Free;
    end;
  end;
end;

procedure TCnIntMatrix.Divide(Factor: Int64);
begin
  raise ECnMatrixException.Create(SCnErrorDivNotImplInt);
end;

procedure TCnIntMatrix.DumpToStrings(List: TStrings; Sep: Char = ' ');
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
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

function TCnIntMatrix.GetZigZagValue(Index: Integer): Int64;
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  Result := GetValue(R, C);
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
  Result := False;
  if not IsSquare then
    Exit;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if FMatrix[I, J] <> 0 then
        Exit;
    end;
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

function TCnIntMatrix.Mul3(X, Y, Z: Int64): Int64;
begin
  Result := OperationMul(OperationMul(X, Y), Z);
end;

function TCnIntMatrix.NegativeOnePower(N: Integer): Integer;
begin
  Result := InternalNegativeOnePower(N);
end;

function TCnIntMatrix.OperationAdd(X, Y: Int64): Int64;
begin
  Result := X + Y;
end;

function TCnIntMatrix.OperationDiv(X, Y: Int64): Int64;
begin
  raise ECnMatrixException.Create(SCnErrorDivNotImplInt);
end;

function TCnIntMatrix.OperationMul(X, Y: Int64): Int64;
begin
  Result := X * Y;
end;

function TCnIntMatrix.OperationSub(X, Y: Int64): Int64;
begin
  Result := X - Y;
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

procedure TCnIntMatrix.SetValue(Row, Col: Integer; const AValue: Int64);
begin
  FMatrix[Row, Col] := AValue;
end;

procedure TCnIntMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := 0;
end;

procedure TCnIntMatrix.SetZigZagValue(Index: Integer; const Value: Int64);
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  SetValue(R, C, Value);
end;

function TCnIntMatrix.Trace: Int64;
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorTraceSquare);

  Result := 0;
  for I := 0 to FRowCount - 1 do
    Result := OperationAdd(Result, FMatrix[I, I]);
end;

procedure TCnIntMatrix.Transpose;
begin
  CnMatrixTranspose(Self, Self);
end;

{ TCnFloatMatrix }

procedure TCnFloatMatrix.Add(Factor: Extended);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationAdd(FMatrix[I, J], Factor);
end;

function TCnFloatMatrix.Add3(X, Y, Z: Extended): Extended;
begin
  Result := OperationAdd(OperationAdd(X, Y), Z);
end;

procedure TCnFloatMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnFloatMatrix then
  begin
    TCnFloatMatrix(Dest).RowCount := FRowCount;
    TCnFloatMatrix(Dest).ColCount := FColCount;

    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        TCnFloatMatrix(Dest).Value[I, J] := FMatrix[I, J];
  end
  else
    inherited;
end;

constructor TCnFloatMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRowCount := ARowCount;
  FColCount := AColCount;
  SetLength(FMatrix, FRowCount, FColCount);
end;

procedure TCnFloatMatrix.DeleteCol(Col: Integer);
var
  T: array of array of Extended;
  I, J, SJ, DJ: Integer;
begin
  if (Col >= 0) or (Col < FColCount) then
  begin
    // 把每 Row 的元素取出来放到临时 T 里，剔除第 Col 个
    SetLength(T, FRowCount, FColCount - 1);

    for I := 0 to FRowCount - 1 do
    begin
      SJ := 0;
      DJ := 0;
      while SJ < FColCount do
      begin
        if SJ = Col then
        begin
          Inc(SJ);
          Continue;
        end;
        T[I, DJ] := FMatrix[I, SJ];
        Inc(SJ);
        Inc(DJ);
      end;
    end;

    Dec(FColCount);
    SetLength(FMatrix, FRowCount, FColCount);
    for I := 0 to FRowCount - 1 do
      for J := 0 to FColCount - 1 do
        FMatrix[I, J] := T[I, J];

    SetLength(T, 0);
  end;
end;

procedure TCnFloatMatrix.DeleteRow(Row: Integer);
var
  I, J: Integer;
begin
  if (Row >= 0) or (Row < FRowCount) then
  begin
    // 把第 Row + 1 行到 FRowCount - 1 行的一维数组朝前移动一格，末行时无需移
    if Row < FRowCount - 1 then
    begin
      for I := Row + 1 to FRowCount - 1 do
      begin
        for J := 0 to FColCount - 1 do
        begin
          FMatrix[I - 1, J] := FMatrix[I, J];
        end;
      end;
    end;
    Dec(FRowCount);
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

destructor TCnFloatMatrix.Destroy;
begin
  SetLength(FMatrix, 0);
  inherited;
end;

function TCnFloatMatrix.Determinant: Extended;
var
  I: Integer;
  Minor: TCnFloatMatrix;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorDeterminantSquare);

  if FRowCount = 1 then
    Result := FMatrix[0, 0]
  else if FRowCount = 2 then
    Result := FMatrix[0, 0] * FMatrix[1, 1] - FMatrix[0, 1] * FMatrix[1, 0]
  else if RowCount = 3 then
  begin
    Result := OperationSub(Add3(Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2]),
      Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0]),
      Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])),
      Add3(Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1]),
        Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2]),
        Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0])));
  end
  else
  begin
    // 利用代数余子式 Minor/Cofactor 计算高阶行列式
    Result := 0;
    Minor := TCnFloatMatrix(ClassType.NewInstance); // 需要用子类进行统一的运算
    Minor.Create(FRowCount - 1, FColCount - 1);

    // Minor := Self.clas TCnFloatMatrix.Create(FRowCount - 1, FColCount - 1);
    try
      for I := 0 to FColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);
        Result := OperationAdd(Result, Mul3(FMatrix[0, I], NegativeOnePower(I), Minor.Determinant));
      end;
    finally
      Minor.Free;
    end;
  end;
end;

procedure TCnFloatMatrix.Divide(Factor: Extended);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationDiv(FMatrix[I, J], Factor);
end;

procedure TCnFloatMatrix.DumpToStrings(List: TStrings; Sep: Char);
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  for I := 0 to FRowCount - 1 do
  begin
    S := '';
    for J := 0 to FColCount - 1 do
    begin
      if J = 0 then
        S := FloatToStr(FMatrix[I, J])
      else
        S := S + Sep + FloatToStr(FMatrix[I, J]);
    end;
    List.Add(S);
  end;
end;

function TCnFloatMatrix.GetValue(Row, Col: Integer): Extended;
begin
  Result := FMatrix[Row, Col];
end;

function TCnFloatMatrix.GetZigZagValue(Index: Integer): Extended;
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  Result := GetValue(R, C);
end;

function TCnFloatMatrix.IsE: Boolean;
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

function TCnFloatMatrix.IsSingular: Boolean;
begin
  if not IsSquare then
    Result := False
  else
    Result := FloatAlmostZero(Determinant);
end;

function TCnFloatMatrix.IsSquare: Boolean;
begin
  Result := (FColCount = FRowCount);
end;

function TCnFloatMatrix.IsSymmetrical: Boolean;
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

function TCnFloatMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if not IsSquare then
    Exit;

  for I := 0 to FRowCount - 1 do
  begin
    for J := 0 to FColCount - 1 do
    begin
      if FMatrix[I, J] <> 0 then
        Exit;
    end;
  end;

  Result := True;
end;

procedure TCnFloatMatrix.Mul(Factor: Extended);
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := OperationMul(FMatrix[I, J], Factor);
end;

function TCnFloatMatrix.Mul3(X, Y, Z: Extended): Extended;
begin
  Result := OperationMul(OperationMul(X, Y), Z);
end;

function TCnFloatMatrix.NegativeOnePower(N: Integer): Integer;
begin
  Result := InternalNegativeOnePower(N);
end;

function TCnFloatMatrix.OperationAdd(X, Y: Extended): Extended;
begin
  Result := X + Y;
end;

function TCnFloatMatrix.OperationDiv(X, Y: Extended): Extended;
begin
  Result := X / Y;
end;

function TCnFloatMatrix.OperationMul(X, Y: Extended): Extended;
begin
  Result := X * Y;
end;

function TCnFloatMatrix.OperationSub(X, Y: Extended): Extended;
begin
  Result := X - Y;
end;

procedure TCnFloatMatrix.SetColCount(const Value: Integer);
begin
  if FColCount <> Value then
  begin
    CheckCount(Value);
    FColCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnFloatMatrix.SetE(Size: Integer);
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

procedure TCnFloatMatrix.SetRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
  begin
    CheckCount(Value);
    FRowCount := Value;
    SetLength(FMatrix, FRowCount, FColCount);
  end;
end;

procedure TCnFloatMatrix.SetValue(Row, Col: Integer; const AValue: Extended);
begin
  FMatrix[Row, Col] := AValue;
end;

procedure TCnFloatMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to FRowCount - 1 do
    for J := 0 to FColCount - 1 do
      FMatrix[I, J] := 0;
end;

procedure TCnFloatMatrix.SetZigZagValue(Index: Integer;
  const Value: Extended);
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  SetValue(R, C, Value);
end;

function TCnFloatMatrix.Trace: Extended;
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorTraceSquare);

  Result := 0;
  for I := 0 to FRowCount - 1 do
    Result := OperationAdd(Result, FMatrix[I, I]);
end;

procedure TCnFloatMatrix.Transpose;
begin
  CnMatrixTranspose(Self, Self);
end;

{ TCnRationalNumber }

procedure TCnRationalNumber.Add(Value: TCnRationalNumber);
begin
  CnRationalNumberAdd(Self, Value, Self);
end;

procedure TCnRationalNumber.Add(Value: Int64);
begin
  FNumerator := FNumerator + Value * FDenominator;
end;

procedure TCnRationalNumber.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnRationalNumber then
  begin
    TCnRationalNumber(Dest).Numerator := FNumerator;
    TCnRationalNumber(Dest).Denominator := FDenominator;
  end
  else
    inherited;
end;

constructor TCnRationalNumber.Create;
begin
  FDenominator := 1;
end;

destructor TCnRationalNumber.Destroy;
begin
  inherited;

end;

procedure TCnRationalNumber.Divide(Value: TCnRationalNumber);
begin
  CnRationalNumberDiv(Self, Value, Self);
end;

procedure TCnRationalNumber.Divide(Value: Int64);
begin
  Denominator := FDenominator * Value;
  Reduce;
end;

function TCnRationalNumber.Equal(Value: TCnRationalNumber): Boolean;
begin
  Result := FNumerator * Value.Denominator = FDenominator * Value.Numerator;
end;

function TCnRationalNumber.EqualInt(Value: Int64): Boolean;
begin
  Result := FNumerator = FDenominator * Value;
end;

function TCnRationalNumber.IsInt: Boolean;
begin
  Result := (FDenominator = 1) or (FDenominator = -1);
end;

function TCnRationalNumber.IsNegative: Boolean;
begin
  Result := ((FNumerator < 0) and (FDenominator > 0))
    or ((FNumerator > 0) and (FDenominator < 0))
end;

function TCnRationalNumber.IsOne: Boolean;
begin
  Result := FNumerator = FDenominator;
end;

function TCnRationalNumber.IsZero: Boolean;
begin
  Result := FNumerator = 0;
end;

procedure TCnRationalNumber.Mul(Value: TCnRationalNumber);
begin
  CnRationalNumberMul(Self, Value, Self);
end;

procedure TCnRationalNumber.Mul(Value: Int64);
begin
  FNumerator := FNumerator * Value;
  Reduce;
end;

procedure TCnRationalNumber.Neg;
begin
  FNumerator := - FNumerator;
end;

procedure TCnRationalNumber.Reciprocal;
var
  T: Int64;
begin
  T := FDenominator;
  FDenominator := FNumerator;
  FNumerator := T;
end;

procedure TCnRationalNumber.Reduce;
begin
  if (FDenominator < 0) and (FNumerator < 0) then
  begin
    FDenominator := -FDenominator;
    FNumerator := -FNumerator;
  end;

  if FNumerator = 0 then
  begin
    FDenominator := 1;
    Exit;
  end;

  if not IsInt then
    CnReduceInt64(FNumerator, FDenominator);
end;

procedure TCnRationalNumber.SetDenominator(const Value: Int64);
begin
  if Value = 0 then
    raise EDivByZero.Create('Denominator can NOT be Zero.');

  FDenominator := Value;
end;

procedure TCnRationalNumber.SetIntValue(Value: Int64);
begin
  FDenominator := 1;
  FNumerator := Value;
end;

procedure TCnRationalNumber.SetOne;
begin
  FDenominator := 1;
  FNumerator := 1;
end;

procedure TCnRationalNumber.SetString(const Value: string);
var
  P: Integer;
  N, D: string;
begin
  P := Pos('/', Value);
  if P > 1 then
  begin
    N := Copy(Value, 1, P - 1);
    D := Copy(Value, P + 1, MaxInt);
    FNumerator := StrToInt64(N);
    FDenominator := StrToInt64(D);
  end
  else
  begin
    FNumerator := StrToInt64(Value);
    FDenominator := 1;
  end;
end;

procedure TCnRationalNumber.SetValue(ANumerator, ADenominator: Int64);
begin
  Denominator := ADenominator;
  Numerator := ANumerator;
end;

procedure TCnRationalNumber.SetZero;
begin
  FDenominator := 1;
  FNumerator := 0;
end;

procedure TCnRationalNumber.Sub(Value: TCnRationalNumber);
begin
  CnRationalNumberSub(Self, Value, Self);
end;

procedure TCnRationalNumber.Sub(Value: Int64);
begin
  FNumerator := FNumerator - Value * FDenominator;
end;

function TCnRationalNumber.ToString: string;
begin
  if IsInt or (FNumerator = 0) then
    Result := IntToStr(FNumerator)
  else
    Result := IntToStr(FNumerator) + '/' + IntToStr(FDenominator);
end;

// 求两个 Int64 的最大公约数，要求都大于 0
function Int64Gcd(A, B: Int64): Int64;
begin
  if B = 0 then
    Result := A
  else
    Result := Int64Gcd(B, A mod B);
end;

// 求两个 Int64 的最小公倍数，要求都大于 0，暂不考虑可能溢出的情况
function Int64Lcm(A, B: Int64): Int64;
var
  D: Int64;
begin
  if A = B then
  begin
    Result := A;
    Exit;
  end;

  D := Int64Gcd(A, B);
  if D = 1 then
    Result := A * B
  else
  begin
    // 大数先除，避免溢出
    if A > B then
      Result := A div D * B
    else
      Result := B div D * A;
  end;
end;

procedure CnRationalNumberAdd(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, F1, F2, D1, D2: Int64;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    RationalResult.Numerator := Number1.Numerator + Number2.Numerator;
  end
  else
  begin
    // 求分母的最小公倍数
    D1 := Number1.Denominator;
    D2 := Number2.Denominator;

    B1 := D1 < 0;
    B2 := D2 < 0;
    if B1 then
      D1 := -D1;

    if B2 then
      D2 := -D2;

    M := Int64Lcm(D1, D2);
    F1 := M div D1;
    F2 := M div D2;

    RationalResult.Denominator := M;
    RationalResult.Numerator := Number1.Numerator * F1 * SIGN_ARRAY[B1]
      + Number2.Numerator * F2 * SIGN_ARRAY[B2]; // 可能溢出，暂无办法
    RationalResult.Reduce;
  end;
end;

procedure CnRationalNumberAdd3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
begin
  CnRationalNumberAdd(Number1, Number2, RationalResult);
  CnRationalNumberAdd(RationalResult, Number3, RationalResult);
end;

procedure CnRationalNumberSub(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
const
  SIGN_ARRAY: array[False..True] of Integer = (1, -1);
var
  M, F1, F2, D1, D2: Int64;
  B1, B2: Boolean;
begin
  if Number1.IsInt and Number2.IsInt then
  begin
    RationalResult.Numerator := Number1.Numerator - Number2.Numerator;
  end
  else
  begin
    // 求分母的最小公倍数
    D1 := Number1.Denominator;
    D2 := Number2.Denominator;

    B1 := D1 < 0;
    B2 := D2 < 0;
    if B1 then
      D1 := -D1;

    if B2 then
      D2 := -D2;

    M := Int64Lcm(D1, D2);
    F1 := M div D1;
    F2 := M div D2;

    RationalResult.Denominator := M;
    RationalResult.Numerator := Number1.Numerator * F1 * SIGN_ARRAY[B1]
      - Number2.Numerator * F2 * SIGN_ARRAY[B2]; // 可能溢出，暂无办法
    RationalResult.Reduce;
  end;
end;

procedure CnRationalNumberMul(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
var
  X, Y: Int64;
begin
  // 假设 Number1、Number2 自身已经约分了，直接乘容易溢出，先互相约
  X := Number1.Numerator;
  Y := Number2.Denominator;
  CnReduceInt64(X, Y);
  if X < Number1.Numerator then
  begin
    // 有约的了
    RationalResult.Numerator := X * Number2.Numerator;
    RationalResult.Denominator := Number1.Denominator * Y;
  end
  else
  begin
    X := Number1.Denominator;
    Y := Number2.Numerator;
    CnReduceInt64(X, Y);
    if X < Number1.Denominator then
    begin
      // 有的约了
      RationalResult.Numerator := Number1.Numerator * Y;
      RationalResult.Denominator := X * Number2.Denominator;
    end
    else
    begin
      RationalResult.Numerator := Number1.Numerator * Number2.Numerator;
      RationalResult.Denominator := Number1.Denominator * Number2.Denominator;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnRationalNumberMul3(Number1, Number2, Number3: TCnRationalNumber; RationalResult: TCnRationalNumber);
begin
  CnRationalNumberMul(Number1, Number2, RationalResult);
  CnRationalNumberMul(RationalResult, Number3, RationalResult);
end;

procedure CnRationalNumberDiv(Number1, Number2: TCnRationalNumber; RationalResult: TCnRationalNumber);
var
  X, Y: Int64;
begin
  // 假设 Number1、Number2 自身已经约分了，直接乘容易溢出，先互相约
  X := Number1.Numerator;
  Y := Number2.Numerator;
  CnReduceInt64(X, Y);
  if X < Number1.Numerator then
  begin
    RationalResult.Numerator := X * Number2.Denominator;
    RationalResult.Denominator := Number1.Denominator * Y;
  end
  else
  begin
    X := Number1.Denominator;
    Y := Number2.Denominator;
    CnReduceInt64(X, Y);
    if X < Number1.Denominator then
    begin
      RationalResult.Numerator := Number1.Numerator * Y;
      RationalResult.Denominator := X * Number2.Numerator;
    end
    else
    begin
      RationalResult.Numerator := Number1.Numerator * Number2.Denominator;
      RationalResult.Denominator := Number1.Denominator * Number2.Numerator;
    end;
  end;
  RationalResult.Reduce;
end;

procedure CnReduceInt64(var X, Y: Int64);
var
  D: Int64;
begin
  D := Int64Gcd(X, Y);
  if D > 1 then
  begin
    X := X div D;
    Y := Y div D;
  end;
end;

function CnRationalNumberCompare(Number1, Number2: TCnRationalNumber): Integer;
var
  R: Int64;
begin
  if not Number1.IsNegative and Number2.IsNegative then
    Result := 1
  else if Number1.IsNegative and not Number2.IsNegative then
    Result := -1
  else  // 同符号才需要计算
  begin
    R := Number1.Numerator * Number2.Denominator - Number2.Numerator * Number1.Denominator;
    if R > 0 then
      Result := 1
    else if R < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;

{ TCn2DObjectList }

constructor TCn2DObjectList.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FRows := TObjectList.Create(True);
  RowCount := ARowCount;
  ColCount := AColCount;
end;

procedure TCn2DObjectList.DeleteCol(Col: Integer);
var
  I: Integer;
begin
  for I := 0 to FRowCount - 1 do
    TObjectList(FRows[I]).Delete(Col);
  Dec(FColCount);
end;

procedure TCn2DObjectList.DeleteRow(Row: Integer);
begin
  FRows.Delete(Row);
  Dec(FRowCount);
end;

destructor TCn2DObjectList.Destroy;
begin
  FRows.Free;
  inherited;
end;

function TCn2DObjectList.GetColCount: Integer;
begin
  Result := FColCount;
end;

function TCn2DObjectList.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TCn2DObjectList.GetValueObject(Row, Col: Integer): TObject;
begin
  Result := TObjectList(FRows[Row])[Col];
end;

procedure TCn2DObjectList.SetColCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FColCount then
  begin
    CheckCount(Value);
    FColCount := Value;

    for I := 0 to FRows.Count - 1 do
    begin
      if FRows[I] = nil then
        FRows[I] := TObjectList.Create(True);

      TObjectList(FRows[I]).Count := FColCount;
    end;
  end;
end;

procedure TCn2DObjectList.SetRowCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FRowCount then
  begin
    CheckCount(Value);
    FRowCount := Value;
    FRows.Count := Value;

    for I := 0 to FRows.Count - 1 do
    begin
      if FRows[I] = nil then
      begin
        FRows[I] := TObjectList.Create(True);
        TObjectList(FRows[I]).Count := FColCount;
      end;
    end;
  end;
end;

procedure TCn2DObjectList.SetValueObject(Row, Col: Integer; const Value: TObject);
begin
  TObjectList(FRows[Row])[Col] := Value;
end;

{ TCnRationalMatrix }

procedure TCnRationalMatrix.Add(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Add(Factor);
end;

procedure TCnRationalMatrix.Add(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Add(Factor);
end;

procedure TCnRationalMatrix.AssignTo(Dest: TPersistent);
var
  I, J: Integer;
begin
  if Dest is TCnRationalMatrix then
  begin
    TCnRationalMatrix(Dest).RowCount := RowCount;
    TCnRationalMatrix(Dest).ColCount := ColCount;

    for I := 0 to RowCount - 1 do
      for J := 0 to ColCount - 1 do
        TCnRationalMatrix(Dest).Value[I, J] := TCnRationalNumber(FMatrix[I, J]);
  end
  else
    inherited;
end;

constructor TCnRationalMatrix.Create(ARowCount, AColCount: Integer);
begin
  inherited Create;
  CheckCount(ARowCount);
  CheckCount(AColCount);

  FMatrix := TCn2DObjectList.Create(ARowCount, AColCount);
end;

procedure TCnRationalMatrix.DeleteCol(Col: Integer);
begin
  FMatrix.DeleteCol(Col);
end;

procedure TCnRationalMatrix.DeleteRow(Row: Integer);
begin
  FMatrix.DeleteRow(Row);
end;

destructor TCnRationalMatrix.Destroy;
begin
  FMatrix.Free;
  inherited;
end;

procedure TCnRationalMatrix.Determinant(D: TCnRationalNumber);
var
  I: Integer;
  Minor: TCnRationalMatrix;
  T: TCnRationalNumber;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorDeterminantSquare);

  if RowCount = 1 then
    D.Assign(Value[0, 0])
  else if RowCount = 2 then
  begin
    T := TCnRationalNumber.Create;
    try
      CnRationalNumberMul(Value[0, 0], Value[1, 1], D);
      CnRationalNumberMul(Value[0, 1], Value[1, 0], T);
      CnRationalNumberSub(D, T, D);
    finally
      T.Free;
    end;
    // [0, 0] * [1, 1] - [0, 1] * [1, 0]
  end
  else if RowCount = 3 then
  begin
    T := TCnRationalNumber.Create;
    D.SetZero;
    try
      CnRationalNumberMul3(Value[0, 0], Value[1, 1], Value[2, 2], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 1], Value[1, 2], Value[2, 0], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 2], Value[1, 0], Value[2, 1], T);
      CnRationalNumberAdd(D, T, D);
      CnRationalNumberMul3(Value[0, 0], Value[1, 2], Value[2, 1], T);
      CnRationalNumberSub(D, T, D);
      CnRationalNumberMul3(Value[0, 1], Value[1, 0], Value[2, 2], T);
      CnRationalNumberSub(D, T, D);
      CnRationalNumberMul3(Value[0, 2], Value[1, 1], Value[2, 0], T);
      CnRationalNumberSub(D, T, D);
    finally
      T.Free;
    end
//    Result := Mul3(FMatrix[0, 0], FMatrix[1, 1], FMatrix[2, 2])
//      + Mul3(FMatrix[0, 1], FMatrix[1, 2], FMatrix[2, 0])
//      + Mul3(FMatrix[0, 2], FMatrix[1, 0], FMatrix[2, 1])
//      - Mul3(FMatrix[0, 0], FMatrix[1, 2], FMatrix[2, 1])
//      - Mul3(FMatrix[0, 1], FMatrix[1, 0], FMatrix[2, 2])
//      - Mul3(FMatrix[0, 2], FMatrix[1, 1], FMatrix[2, 0]);
  end
  else
  begin
    // 利用代数余子式 Minor/Cofactor 计算高阶行列式
    D.SetZero;
    Minor := TCnRationalMatrix.Create(RowCount - 1, ColCount - 1);
    T := TCnRationalNumber.Create;
    try
      for I := 0 to ColCount - 1 do
      begin
        CnMatrixMinor(Self, 0, I, Minor);

        Minor.Determinant(T);
        T.Mul(InternalNegativeOnePower(I));
        T.Mul(Value[0, I]);
        D.Add(T);
        // Result := Result + (FMatrix[0, I] * NegativeOnePower(I)* Minor.Determinant));
      end;
    finally
      Minor.Free;
      T.Free;
    end;
  end;
end;

procedure TCnRationalMatrix.Divide(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Divide(Factor);
end;

procedure TCnRationalMatrix.Divide(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Divide(Factor);
end;

procedure TCnRationalMatrix.DumpToStrings(List: TStrings; Sep: Char);
var
  I, J: Integer;
  S: string;
begin
  if List = nil then
    Exit;

  List.Clear;
  for I := 0 to RowCount - 1 do
  begin
    S := '';
    for J := 0 to ColCount - 1 do
    begin
      if J = 0 then
        S := Value[I, J].ToString
      else
        S := S + Sep + Value[I, J].ToString;
    end;
    List.Add(S);
  end;
end;

function TCnRationalMatrix.GetColCount: Integer;
begin
  Result := FMatrix.ColCount;
end;

function TCnRationalMatrix.GetRowCount: Integer;
begin
  Result := FMatrix.RowCount;
end;

function TCnRationalMatrix.GetValue(Row, Col: Integer): TCnRationalNumber;
begin
  Result := TCnRationalNumber(FMatrix[Row, Col]);
  if Result = nil then
  begin
    Result := TCnRationalNumber.Create;
    FMatrix[Row, Col] := Result;
  end;
end;

function TCnRationalMatrix.GetZigZagValue(Index: Integer): TCnRationalNumber;
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  Result := GetValue(R, C);
end;

function TCnRationalMatrix.IsE: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to RowCount - 1 do
  begin
    for J := 0 to ColCount - 1 do
    begin
      if (I = J) and not Value[I, J].IsOne then
      begin
        Result := False;
        Exit;
      end
      else if (I <> J) and not Value[I, J].IsZero then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function TCnRationalMatrix.IsSingular: Boolean;
var
  D: TCnRationalNumber;
begin
  if not IsSquare then
    Result := False
  else
  begin
    D := TCnRationalNumber.Create;
    try
      Determinant(D);
      Result := D.IsZero;
    finally
      D.Free;
    end;
  end;
end;

function TCnRationalMatrix.IsSquare: Boolean;
begin
  Result := (ColCount = RowCount);
end;

function TCnRationalMatrix.IsSymmetrical: Boolean;
var
  I, J: Integer;
begin
  if not IsSquare then
  begin
    Result := False;
    Exit;
  end;

  for I := 0 to RowCount - 1 do
    for J := 0 to I do
      if not Value[I, J].Equal(Value[J, I]) then
      begin
        Result := False;
        Exit;
      end;

  Result := True;
end;

function TCnRationalMatrix.IsZero: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if not IsSquare then
    Exit;

  for I := 0 to RowCount - 1 do
  begin
    for J := 0 to ColCount - 1 do
    begin
      if not Value[I, J].IsZero then
        Exit;
    end;
  end;

  Result := True;
end;

procedure TCnRationalMatrix.Mul(Factor: TCnRationalNumber);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Mul(Factor);
end;

procedure TCnRationalMatrix.Mul(Factor: Int64);
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].Mul(Factor);
end;

procedure TCnRationalMatrix.SetColCount(const Value: Integer);
begin
  FMatrix.ColCount := Value;
end;

procedure TCnRationalMatrix.SetE(Size: Integer);
var
  I, J: Integer;
begin
  CheckCount(Size);

  RowCount := Size;
  ColCount := Size;
  for I := 0 to Size - 1 do
    for J := 0 to Size - 1 do
      if I = J then
        Value[I, J].SetOne
      else
        Value[I, J].SetZero;
end;

procedure TCnRationalMatrix.SetRowCount(const Value: Integer);
begin
  FMatrix.RowCount := Value;
end;

procedure TCnRationalMatrix.SetValue(Row, Col: Integer;
  const Value: TCnRationalNumber);
begin
  if FMatrix[Row, Col] = nil then
    FMatrix[Row, Col] := TCnRationalNumber.Create;
  TCnRationalNumber(FMatrix[Row, Col]).Assign(Value);
end;

procedure TCnRationalMatrix.SetZero;
var
  I, J: Integer;
begin
  for I := 0 to RowCount - 1 do
    for J := 0 to ColCount - 1 do
      Value[I, J].SetZero;
end;

procedure TCnRationalMatrix.SetZigZagValue(Index: Integer; const Value: TCnRationalNumber);
var
  R, C: Integer;
begin
  if RowCount <> ColCount then
    raise ECnMatrixException.Create(SCnErrorZigZagRowColCount);

  ZigZagToRowCol(Index, R, C, RowCount);
  SetValue(R, C, Value);
end;

procedure TCnRationalMatrix.Trace(T: TCnRationalNumber);
var
  I: Integer;
begin
  if not IsSquare then
    raise ECnMatrixException.Create(SCnErrorTraceSquare);

  T.SetZero;
  for I := 0 to RowCount - 1 do
    T.Add(Value[I, I]);
end;

procedure TCnRationalMatrix.Transpose;
begin
  CnMatrixTranspose(Self, Self);
end;

end.

