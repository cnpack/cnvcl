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

unit CnComplex;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：浮点复数运算实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了扩展精度浮点数的复数结构 TCnComplexNumber 及其各类运算。
*           为提高效率，使用 record 而不用 TObject。
*
*           也实现了基于大整数的复数类，注意不支持除、绝对值等需要浮点运算的场合。
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.01.20 V1.2
*               增加无限精度浮点数复数
*           2023.06.26 V1.1
*               增加辐角与绝对值等函数
*           2020.11.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, SysConst, Math, Contnrs,
  CnBigNumber, CnBigDecimal, CnContainers;

type
  ECnComplexNumberException = class(Exception);
  {* 复数相关的异常}

  TCnComplexNumber = packed record
  {* 浮点精度的复数表示结构}
    R: Extended;
    {* 实部}
    I: Extended;
    {* 虚部}
  end;
  PCnComplexNumber = ^TCnComplexNumber;
  {* 指向复数结构的指针}

  TCnComplexArray = array[0..8191] of TCnComplexNumber;
  {* 复数结构数组}

  PCnComplexArray = ^TCnComplexArray;
  {* 指向复数结构数组的指针}

  TCnBigComplex = class(TObject)
  {* 实部虚部均为大整数的复数类}
  private
    FR: TCnBigNumber;
    FI: TCnBigNumber;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大整数复数转换成字符串。

       参数：
         （无）

       返回值：string                     - 返回大数字符串
    }

    procedure SetString(const Str: string);
    {* 将复数字符串转换为本对象的内容。

       参数：
         const Str: string                - 待转换的复数字符串

       返回值：（无）
    }

    procedure SetZero;
    {* 将大整数复数设置为 0，返回是否设置成功。

       参数：
         （无）

       返回值：Boolean                    - 返回是否设置成功
    }

    procedure SetOne;
    {* 将大整数复数设置为 0，返回是否设置成功。

       参数：
         （无）

       返回值：Boolean                    - 返回是否设置成功
    }

    procedure SetI;
    {* 将大整数复数设置为 i，返回是否设置成功。

       参数：
         （无）

       返回值：Boolean                    - 返回是否设置成功
    }

    function IsZero: Boolean;
    {* 返回大整数复数是否为 0。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    function IsPureReal: Boolean;
    {* 返回大整数复数是否为纯实数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    function IsPureImaginary: Boolean;
    {* 返回大整数复数是否为纯虚数。

       参数：
         （无）

       返回值：Boolean                    - 返回是否为 0
    }

    procedure SetValue(AR: Int64; AI: Int64); overload;
    {* 大整数复数赋值。

       参数：
         AR: Int64                        - 大整数复数的实部
         AI: Int64                        - 大整数复数的虚部

       返回值：（无）
    }

    procedure SetValue(const AR: string; const AI: string); overload;
    {* 大整数复数赋值。

       参数：
         const AR: string                 - 实部的十进制整数字符串形式
         const AI: string                 - 虚部的十进制整数字符串形式

       返回值：（无）
    }

    function AbsoluteValue(Res: TCnBigNumber): Boolean; overload;
    {* 返回大整数复数的绝对值，也即距复平面原点的距离，以大整数表示。

       参数：
         Res: TCnBigComplex               - 用来容纳结果的大整数对象

       返回值：Boolean                    - 返回是否求值成功
    }

    function AbsoluteValue: Extended; overload;
    {* 返回大整数复数的绝对值，也即距复平面原点的距离，以浮点数表示。

       参数：
         （无）

       返回值：Boolean                    - 返回是否求值成功
    }

    function Argument: Extended;
    {* 返回大整数复数的辐角主值，也即与复平面正 X 轴的夹角，范围在 0 到 2π。

       参数：
         （无）

       返回值：Extended                   - 返回大整数复数的辐角主值，单位为弧度
    }

    procedure Negate;
    {* 大整数复数求相反数，也即实部与虚部正负号均反号}
    procedure Conjugate;
    {* 大整数复数求共轭复数，也即虚部正负号反号}

    property R: TCnBigNumber read FR;
    {* 实部}
    property I: TCnBigNumber read FI;
    {* 虚部}
  end;

  TCnBigComplexPool = class(TCnMathObjectPool)
  {* 大整数复数池实现类，允许使用到大整数复数的地方自行创建大整数复数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigComplex;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         Num: TCnBigComplexDecimal        - 待归还至池中的对象

       返回值：（无）
    }
    procedure Recycle(Num: TCnBigComplex);
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnBigComplexDecimal        - 待归还至池中的对象

       返回值：（无）
    }
  end;

  TCnBigComplexList = class(TObjectList)
  {* 容纳大整数复数的对象列表，同时拥有大整数复数对象们}
  private

  protected
    function GetItem(Index: Integer): TCnBigComplex;
    procedure SetItem(Index: Integer; ABigComplex: TCnBigComplex);
  public
    constructor Create; reintroduce;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function Add: TCnBigComplex; overload;
    {* 新增一个大整数复数对象，返回该对象。注意添加后返回的对象已由列表纳入管理，无需也不应手动释放。

       参数：
         （无）

       返回值：TCnBigComplex              - 内部新增的大整数复数对象
    }

    function Add(ABigComplex: TCnBigComplex): Integer; overload;
    {* 添加外部的大整数复数对象，注意添加后该对象由列表纳入管理，无需也不应手动释放。

       参数：
         ABigComplex: TCnBigComplex       - 待添加的大整数复数对象

       返回值：Integer                    - 新增的该大整数复数对象的索引值
    }

    function Add(AR, AI: Integer): TCnBigComplex; overload;
    {* 添加一复数的实部虚部整数系数，内部生成大整数复数对象，注意返回的结果已由列表纳入管理，无需也不应手动释放。

       参数：
         AR: Integer                      - 待添加的复数实部整数
         AI: Integer                      - 待添加的复数实部整数

       返回值：TCnBigComplex              - 新增的该大整数复数对象
    }

    procedure AddList(List: TCnBigComplexList);
    {* 添加一大整数复数列表，也即复制列表内的所有大整数复数对象并添加。

       参数：
         List: TCnBigComplexList          - 待添加的整数

       返回值：（无）
    }

    function Remove(ABigComplex: TCnBigComplex): Integer;
    {* 从列表中删除指定引用的大整数复数对象并释放。

       参数：
         ABigComplex: TCnBigComplex       - 待删除的大整数复数对象

       返回值：Integer                    - 删除的位置索引，无则返回 -1
    }

    function IndexOfValue(ABigComplex: TCnBigComplex): Integer;
    {* 根据大整数复数的值在列表中查找该值对应的位置索引。

       参数：
         ABigComplex: TCnBigComplex       - 待查找的大整数复数值

       返回值：Integer                    - 返回位置索引，无则返回 -1
    }

    procedure Insert(Index: Integer; ABigComplex: TCnBigComplex);
    {* 在第 Index 个位置前插入大整数复数对象，注意插入后无需也不应手动释放。

       参数：
         Index: Integer                   - 待插入的位置索引
         ABigComplex: TCnBigComplex       - 待插入的大整数复数对象

       返回值：（无）
    }

    procedure RemoveDuplicated;
    {* 去重，也就是删除并释放值重复的大整数复数对象，只留一个}

    procedure SumTo(Sum: TCnBigComplex);
    {* 列表内所有数求和。

       参数：
         Sum: TCnBigComplex               - 输出的和

       返回值：（无）
    }

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大整数复数列表转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property Items[Index: Integer]: TCnBigComplex read GetItem write SetItem; default;
    {* 大整数复数列表项}
  end;

  TCnBigComplexDecimal = class(TObject)
  {* 实部虚部均为无限精度浮点数的大浮点复数类}
  private
    FR: TCnBigDecimal;
    FI: TCnBigDecimal;
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大浮点复数转换为字符串。

       参数：
         无

       返回值：string                     - 返回复数字符串
    }

    procedure SetString(const Str: string);
    {* 将复数字符串转换为本对象的内容。

       参数：
         const Str: string                - 待转换的复数字符串

       返回值：（无）
    }

    procedure SetZero;
    {* 将大浮点复数设置为 0}

    procedure SetOne;
    {* 将大浮点复数设置为 1}

    procedure SetI;
    {* 将大浮点复数设置为 i}

    function IsZero: Boolean;
    {* 返回复数是否为 0

       参数：
         （无）

       返回值：Boolean                    - 大浮点复数是否为 0
    }

    function IsOne: Boolean;
    {* 返回复数是否为 1。

       参数：
         （无）

       返回值：Boolean                    - 大浮点复数是否为 1
    }

    function IsNegOne: Boolean;
    {* 返回复数是否为 -1。

       参数：
         （无）

       返回值：Boolean                    - 大浮点复数是否为 -1
    }

    function IsPureReal: Boolean;
    {* 返回大浮点复数是否为纯实数。

       参数：
         （无）

       返回值：Boolean                    - 大浮点复数是否为纯实数
    }

    function IsPureImaginary: Boolean;
    {* 返回大浮点复数是否为纯虚数。

       参数：
         （无）

       返回值：Boolean                    - 大浮点复数是否为纯虚数
    }

    procedure SetValue(AR: Int64; AI: Int64); overload;
    {* 设置大浮点复数值。

       参数：
         AR: Int64                        - 大浮点复数的实部
         AI: Int64                        - 大浮点复数的虚部

       返回值：（无）
    }

    procedure SetValue(const AR: string; const AI: string); overload;
    {* 设置大浮点复数值。

       参数：
         const AR: string                 - 大浮点复数实部的十进制数字符串形式
         const AI: string                 - 大浮点复数虚部的十进制数字符串形式

       返回值：（无）
    }

    procedure RoundTo(Precision: Integer; RoundMode: TCnBigRoundMode = dr465RoundEven);
    {* 实部与虚部均舍入至指定小数位数，如原来小数位数少于 Precision 则不动。

       参数：
         Precision: Integer               - 指定小数位数
         RoundMode: TCnBigRoundMode       - 舍入的规则

       返回值：（无）
    }

    function AbsoluteValue(Res: TCnBigDecimal; Precision: Integer = 0): Boolean; overload;
    {* 返回大浮点复数的绝对值，也即复平面上原点的距离，以大数表示。

       参数：
         Res: TCnBigDecimal               - 用于存放结果的大浮点数
         Precision: Integer               - 保留小数点后几位，0 表示按默认设置来

       返回值：Boolean                    - 返回运算是否成功
    }

    function AbsoluteValue: Extended; overload;
    {* 返回大浮点复数的绝对值，也即复平面上原点的距离，以浮点数表示。

       参数：
         （无）

       返回值：Extended                   - 返回运算是否成功
    }

    function Argument: Extended;
    {* 返回大浮点复数的辐角值，也即复平面上与 X 轴的夹角，范围从 0 到 2π。

       参数：
         （无）

       返回值：Extended                   - 返回大浮点复数的辐角值，单位为弧度
    }

    procedure Negate;
    {* 求大浮点复数的相反数，也即实部和虚部都取相反数}
    procedure Conjugate;
    {* 求大浮点复数的共轭，也即虚部取相反数}

    property R: TCnBigDecimal read FR;
    {* 实部}
    property I: TCnBigDecimal read FI;
    {* 虚部}
  end;

  TCnBigComplexDecimalPool = class(TCnMathObjectPool)
  {* 大浮点复数池实现类，允许使用到大浮点复数的地方自行创建大浮点复数池}
  protected
    function CreateObject: TObject; override;
  public
    function Obtain: TCnBigComplexDecimal;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         Num: TCnBigComplexDecimal        - 待归还至池中的对象

       返回值：（无）
    }
    procedure Recycle(Num: TCnBigComplexDecimal);
    {* 将一个对象归还至对象池。

       参数：
         Num: TCnBigComplexDecimal        - 待归还至池中的对象

       返回值：（无）
    }
  end;

  TCnBigComplexDecimalList = class(TObjectList)
  {* 容纳大浮点数复数的对象列表，同时拥有大浮点数复数对象们}
  private

  protected
    function GetItem(Index: Integer): TCnBigComplexDecimal;
    procedure SetItem(Index: Integer; ABigComplexDecimal: TCnBigComplexDecimal);
  public
    constructor Create; reintroduce;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    function Add: TCnBigComplexDecimal; overload;
    {* 新增一个大浮点数复数对象，返回该对象。注意添加后返回的对象已由列表纳入管理，无需也不应手动释放。

       参数：
         （无）

       返回值：TCnBigComplexDecimal                       - 内部新增的大浮点数复数对象
    }

    function Add(ABigComplexDecimal: TCnBigComplexDecimal): Integer; overload;
    {* 添加外部的大浮点数复数对象，注意添加后该对象由列表纳入管理，无需也不应手动释放。

       参数：
         ABigComplexDecimal: TCnBigComplexDecimal         - 待添加的大浮点数复数对象

       返回值：Integer                                    - 新增的该大浮点数复数对象的索引值
    }

    function Add(AR, AI: Integer): TCnBigComplexDecimal; overload;
    {* 添加实部与虚部的系数，内部生成大浮点数复数对象，注意返回的结果已由列表纳入管理，无需也不应手动释放。

       参数：
         AR: Integer                      - 待添加的大浮点数复数的实部整数
         AI: Integer                      - 待添加的大浮点数复数的虚部整数

       返回值：TCnBigComplexDecimal       - 新增的该大浮点数复数对象
    }

    procedure AddList(List: TCnBigComplexDecimalList);
    {* 添加一大浮点数复数列表，也即复制列表内的所有大浮点数复数对象并添加。

       参数：
         List: TCnBigComplexDecimalList   - 待添加的整数

       返回值：（无）
    }

    function Remove(ABigComplexDecimal: TCnBigComplexDecimal): Integer;
    {* 从列表中删除指定引用的大浮点数复数对象并释放。

       参数：
         ABigComplexDecimal: TCnBigComplexDecimal         - 待删除的大浮点数复数对象

       返回值：Integer                                    - 删除的位置索引，无则返回 -1
    }

    function IndexOfValue(ABigComplexDecimal: TCnBigComplexDecimal): Integer;
    {* 根据大浮点数复数的值在列表中查找该值对应的位置索引。

       参数：
         ABigComplexDecimal: TCnBigComplexDecimal         - 待查找的大浮点数复数值

       返回值：Integer                                    - 返回位置索引，无则返回 -1
    }

    procedure Insert(Index: Integer; ABigComplexDecimal: TCnBigComplexDecimal);
    {* 在第 Index 个位置前插入大浮点数复数对象，注意插入后无需也不应手动释放。

       参数：
         Index: Integer                                   - 待插入的位置索引
         ABigComplexDecimal: TCnBigComplexDecimal         - 待插入的大浮点数复数对象

       返回值：（无）
    }

    procedure RemoveDuplicated;
    {* 去重，也就是删除并释放值重复的大浮点数复数对象，只留一个}

    procedure SumTo(Sum: TCnBigComplexDecimal);
    {* 列表内所有数求和。

       参数：
         Sum: TCnBigComplexDecimal        - 输出的和

       返回值：（无）
    }


    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将大浮点数复数列表转成字符串。

       参数：
         （无）

       返回值：string                     - 返回字符串
    }

    property Items[Index: Integer]: TCnBigComplexDecimal read GetItem write SetItem; default;
    {* 大浮点数复数列表项}
  end;

// ======================== 浮点精度的复数运算 =================================

function ComplexNumberIsZero(var Complex: TCnComplexNumber): Boolean;
{* 返回复数是否为 0。

   参数：
     var Complex: TCnComplexNumber        - 待判断的复数

   返回值：Boolean                        - 返回是否等于 0
}

procedure ComplexNumberSetZero(var Complex: TCnComplexNumber);
{* 复数置 0。

   参数：
     var Complex: TCnComplexNumber        - 待设置的复数

   返回值：（无）
}

procedure ComplexNumberSetOne(var Complex: TCnComplexNumber);
{* 复数置 1。

   参数：
     var Complex: TCnComplexNumber        - 待设置的复数

   返回值：（无）
}

procedure ComplexNumberSetI(var Complex: TCnComplexNumber);
{* 复数置 i。

   参数：
     var Complex: TCnComplexNumber        - 待设置的复数

   返回值：（无）
}

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  AR: Extended; AI: Extended); overload;
{* 复数赋值。

   参数：
     var Complex: TCnComplexNumber        - 待赋值的复数
     AR: Extended                         - 复数的实部
     AI: Extended                         - 复数的虚部

   返回值：（无）
}

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  const AR: string; const AI: string); overload;
{* 复数赋值。

   参数：
     var Complex: TCnComplexNumber        - 待赋值的复数
     const AR: string                     - 实部的浮点字符串形式
     const AI: string                     - 虚部的浮点字符串形式

   返回值：（无）
}

procedure ComplexNumberSetString(var Complex: TCnComplexNumber; const Str: string);
{* 用形如 1.4+2i、3i、0、2-i 这种字符串给复数赋值。

   参数：
     var Complex: TCnComplexNumber        - 待赋值的复数
     const Str: string                    - 复数值字符串

   返回值：（无）
}

function ComplexNumberToString(var Complex: TCnComplexNumber): string;
{* 复数转换为形如 a + bi 的字符串，实部虚部若有 0 则对应省略。

   参数：
     var Complex: TCnComplexNumber        - 待转换的复数

   返回值：string                         - 返回复数的字符串形式
}

function ComplexNumberEqual(var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber): Boolean;
{* 判断两个复数值是否相等。

   参数：
     var Complex1: TCnComplexNumber       - 待比较的复数一
     var Complex2: TCnComplexNumber       - 待比较的复数二

   返回值：Boolean                        - 返回两个复数值是否相等
}

procedure ComplexNumberSwap(var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber);
{* 交换两个复数的值。

   参数：
     var Complex1: TCnComplexNumber       - 待交换的复数一
     var Complex2: TCnComplexNumber       - 待交换的复数二

   返回值：（无）
}

procedure ComplexNumberCopy(var Dest: TCnComplexNumber; var Source: TCnComplexNumber);
{* 复制复数的值。

   参数：
     var Dest: TCnComplexNumber           - 目标复数
     var Source: TCnComplexNumber         - 源复数

   返回值：（无）
}

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* 复数加法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2。

   参数：
     var Res: TCnComplexNumber            - 复数和
     var Complex1: TCnComplexNumber       - 复数加数一
     var Complex2: TCnComplexNumber       - 复数加数二

   返回值：（无）
}

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* 复数减法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2。

   参数：
     var Res: TCnComplexNumber            - 复数差
     var Complex1: TCnComplexNumber       - 复数被减数
     var Complex2: TCnComplexNumber       - 复数减数

   返回值：（无）
}

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* 复数乘法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2。

   参数：
     var Res: TCnComplexNumber            - 复数积
     var Complex1: TCnComplexNumber       - 复数乘数一
     var Complex2: TCnComplexNumber       - 复数乘数二

   返回值：（无）
}

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex1: TCnComplexNumber; var Complex2: TCnComplexNumber); overload;
{* 复数除法，Complex1 和 Complex2 可以是同一个结构，Res 可以是 Complex1 或 Complex2。

   参数：
     var Res: TCnComplexNumber            - 复数商
     var Complex1: TCnComplexNumber       - 复数被除数
     var Complex2: TCnComplexNumber       - 复数除数

   返回值：（无）
}

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的加法，Complex 和 Res 可以是同一个结构。

   参数：
     var Res: TCnComplexNumber            - 复数和
     var Complex: TCnComplexNumber        - 复数加数
     Value: Extended                      - 浮点数加数

   返回值：（无）
}

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的减法，Complex 和 Res 可以是同一个结构。

   参数：
     var Res: TCnComplexNumber            - 复数差
     var Complex: TCnComplexNumber        - 复数被减数
     Value: Extended                      - 浮点数减数

   返回值：（无）
}

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的乘法，Complex 和 Res 可以是同一个结构。

   参数：
     var Res: TCnComplexNumber            - 复数积
     var Complex: TCnComplexNumber        - 复数乘数
     Value: Extended                      - 浮点数乘数

   返回值：（无）
}

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
{* 复数与浮点数的除法，Complex 和 Res 可以是同一个结构。

   参数：
     var Res: TCnComplexNumber            - 复数商
     var Complex: TCnComplexNumber        - 复数被除数
     Value: Extended                      - 浮点除数

   返回值：（无）
}

procedure ComplexNumberSqrt(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
{* 求复数的平方根，只返回其中一个，如果需要另一个，实部虚部各取负就行。

   参数：
     var Res: TCnComplexNumber            - 复数平方根结果
     var Complex: TCnComplexNumber        - 待求平方根的复数

   返回值：（无）
}

procedure ComplexNegate(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
{* 获得负的复数，Res 可以是 Complex。

   参数：
     var Res: TCnComplexNumber            - 复数的求负结果
     var Complex: TCnComplexNumber        - 待求负的复数

   返回值：（无）
}

procedure ComplexConjugate(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
{* 获得共轭复数，Res 可以是 Complex。

   参数：
     var Res: TCnComplexNumber            - 复数的共轭结果
     var Complex: TCnComplexNumber        - 待求共轭的复数

   返回值：（无）
}

function ComplexIsPureReal(var Complex: TCnComplexNumber): Boolean;
{* 复数是否纯实数，也就是判断虚部是否为 0。

   参数：
     var Complex: TCnComplexNumber        - 待判断的复数

   返回值：Boolean                        - 返回是否纯实数
}

function ComplexIsPureImaginary(var Complex: TCnComplexNumber): Boolean;
{* 复数是否纯虚数，也就是判断实部是否为 0 且虚部不为 0。

   参数：
     var Complex: TCnComplexNumber        - 待判断的复数

   返回值：Boolean                        - 返回是否纯虚数
}

function ComplexNumberAbsoluteValue(var Complex: TCnComplexNumber): Extended;
{* 返回复数的绝对值，也即距复平面原点的距离。

   参数：
     var Complex: TCnComplexNumber        - 待计算的复数

   返回值：Extended                       - 返回复数的绝对值
}

function ComplexNumberArgument(var Complex: TCnComplexNumber): Extended;
{* 返回复数的辐角主值，也即与复平面正 X 轴的夹角，范围在 0 到 2π。

   参数：
     var Complex: TCnComplexNumber        - 待计算的复数

   返回值：Extended                       - 返回复数的辐角主值，单位为弧度
}

procedure ComplexNumberSetAbsoluteArgument(var Complex: TCnComplexNumber;
  AnAbsolute: Extended; AnArgument: Extended);
{* 设置一复数的绝对值与辐角值。

   参数：
     var Complex: TCnComplexNumber        - 待设置的复数
     AnAbsolute: Extended                 - 待设置的绝对值
     AnArgument: Extended                 - 待设置的辐角值

   返回值：（无）
}

// ========================== 大整数的复数运算 =================================

function BigComplexIsZero(Complex: TCnBigComplex): Boolean;
{* 返回大整数复数是否为 0。

   参数：
     Complex: TCnBigComplex         - 待判断的大整数复数

   返回值：Boolean                        - 返回是否等于 0
}

procedure BigComplexSetZero(Complex: TCnBigComplex);
{* 大整数复数置 0。

   参数：
     Complex: TCnBigComplex         - 待设置的大整数复数

   返回值：（无）
}

procedure BigComplexSetOne(Complex: TCnBigComplex);
{* 大整数复数置 1。

   参数：
     Complex: TCnBigComplex         - 待设置的大整数复数

   返回值：（无）
}

procedure BigComplexSetI(Complex: TCnBigComplex);
{* 大整数复数置 i。

   参数：
     Complex: TCnBigComplex         - 待设置的大整数复数

   返回值：（无）
}

procedure BigComplexSetValue(Complex: TCnBigComplex;
  AR: Int64; AI: Int64); overload;
{* 大整数复数赋值。

   参数：
     Complex: TCnBigComplex         - 待赋值的大整数复数
     AR: Int64                            - 大整数复数的实部
     AI: Int64                            - 大整数复数的虚部

   返回值：（无）
}

procedure BigComplexSetValue(Complex: TCnBigComplex;
  const AR: string; const AI: string); overload;
{* 大整数复数赋值。

   参数：
     Complex: TCnBigComplex         - 待赋值的大整数复数
     const AR: string                     - 实部的十进制整数字符串形式
     const AI: string                     - 虚部的十进制整数字符串形式

   返回值：（无）
}

procedure BigComplexSetString(Complex: TCnBigComplex; const Str: string);
{* 用形如 1.4+2i、3i、0、2-i 这种字符串给大整数复数赋值。

   参数：
     Complex: TCnBigComplex         - 待赋值的大整数复数
     const Str: string                    - 复数值字符串

   返回值：（无）
}

function BigComplexToString(Complex: TCnBigComplex): string;
{* 大整数复数转换为形如 a + bi 的字符串，实部虚部若有 0 则对应省略。

   参数：
     Complex: TCnBigComplex         - 待转换的大整数复数

   返回值：string                         - 返回大整数复数的字符串形式
}

function BigComplexEqual(Complex1: TCnBigComplex; Complex2: TCnBigComplex): Boolean;
{* 判断两个大整数复数值是否相等。

   参数：
     Complex1: TCnBigComplex        - 待比较的大整数复数一
     Complex2: TCnBigComplex        - 待比较的大整数复数二

   返回值：Boolean                        - 返回两个大整数复数值是否相等
}

procedure BigComplexSwap(Complex1: TCnBigComplex; Complex2: TCnBigComplex);
{* 交换两个大整数复数的值。

   参数：
     Complex1: TCnBigComplex        - 待交换的大整数复数一
     Complex2: TCnBigComplex        - 待交换的大整数复数二

   返回值：（无）
}

procedure BigComplexCopy(Dest: TCnBigComplex; Source: TCnBigComplex);
{* 复制大整数复数的值。

   参数：
     Dest: TCnBigComplex            - 目标大整数复数
     Source: TCnBigComplex          - 源大整数复数

   返回值：（无）
}

procedure BigComplexAdd(Res: TCnBigComplex;
  Complex1: TCnBigComplex; Complex2: TCnBigComplex); overload;
{* 大整数复数加法，Complex1 和 Complex2 可以是同一个对象，Res 可以是 Complex1 或 Complex2。

   参数：
     Res: TCnBigComplex             - 大整数复数和
     Complex1: TCnBigComplex        - 大整数复数加数一
     Complex2: TCnBigComplex        - 大整数复数加数二

   返回值：（无）
}

procedure BigComplexSub(Res: TCnBigComplex;
  Complex1: TCnBigComplex; Complex2: TCnBigComplex); overload;
{* 大整数复数减法，Complex1 和 Complex2 可以是同一个对象，Res 可以是 Complex1 或 Complex2。

   参数：
     Res: TCnBigComplex             - 大整数复数差
     Complex1: TCnBigComplex        - 大整数复数被减数
     Complex2: TCnBigComplex        - 大整数复数减数

   返回值：（无）
}

procedure BigComplexMul(Res: TCnBigComplex;
  Complex1: TCnBigComplex; Complex2: TCnBigComplex); overload;
{* 大整数复数乘法，Complex1 和 Complex2 可以是同一个对象，Res 可以是 Complex1 或 Complex2。

   参数：
     Res: TCnBigComplex             - 大整数复数积
     Complex1: TCnBigComplex        - 大整数复数乘数一
     Complex2: TCnBigComplex        - 大整数复数乘数二

   返回值：（无）
}

procedure BigComplexAdd(Res: TCnBigComplex;
  Complex: TCnBigComplex; Value: Int64); overload;
{* 大整数复数与整数的加法，Complex 和 Res 可以是同一个对象。

   参数：
     Res: TCnBigComplex             - 大整数复数和
     Complex: TCnBigComplex         - 大整数复数加数
     Value: Int64                         - 整数加数

   返回值：（无）
}

procedure BigComplexSub(Res: TCnBigComplex;
  Complex: TCnBigComplex; Value: Int64); overload;
{* 大整数复数与整数的减法，Complex 和 Res 可以是同一个对象。

   参数：
     Res: TCnBigComplex             - 大整数复数差
     Complex: TCnBigComplex         - 大整数复数被减数
     Value: Int64                         - 整数减数

   返回值：（无）
}

procedure BigComplexMul(Res: TCnBigComplex;
  Complex: TCnBigComplex; Value: Int64); overload;
{* 大整数复数与整数的乘法，Complex 和 Res 可以是同一个对象。

   参数：
     Res: TCnBigComplex             - 大整数复数积
     Complex: TCnBigComplex         - 大整数复数乘数
     Value: Int64                         - 整数乘数

   返回值：（无）
}

procedure BigComplexNegate(Res: TCnBigComplex; Complex: TCnBigComplex);
{* 获得大整数复数的负值，Res 可以是 Complex。

   参数：
     Res: TCnBigComplex            - 大整数复数的求负结果
     Complex: TCnBigComplex        - 待求负的大整数复数

   返回值：（无）
}

procedure BigComplexConjugate(Res: TCnBigComplex; Complex: TCnBigComplex);
{* 获得共轭大整数复数，Res 可以是 Complex。

   参数：
     Res: TCnBigComplex            - 大整数复数的共轭结果
     Complex: TCnBigComplex        - 待求共轭的大整数复数

   返回值：（无）
}

function BigComplexIsPureReal(Complex: TCnBigComplex): Boolean;
{* 大整数复数是否纯实数，也就是判断虚部是否为 0。

   参数：
     Complex: TCnBigComplex         - 待判断的大整数复数

   返回值：Boolean                        - 返回是否纯实数
}

function BigComplexIsPureImaginary(Complex: TCnBigComplex): Boolean;
{* 大整数复数是否纯虚数，也就是判断实部是否为 0 且虚部不为 0。

   参数：
     Complex: TCnBigComplex         - 待判断的大整数复数

   返回值：Boolean                        - 返回是否纯虚数
}

function BigComplexAbsoluteValue(Complex: TCnBigComplex): Extended; overload;
{* 返回大整数复数的绝对值，也即距复平面原点的距离，以浮点数表示。

   参数：
     Complex: TCnBigComplex         - 待计算的大整数复数

   返回值：Extended                       - 返回大整数复数的绝对值
}

function BigComplexAbsoluteValue(Res: TCnBigNumber; Complex: TCnBigComplex): Boolean; overload;
{* 返回大整数复数的绝对值，也即距复平面原点的距离，以大整数表示。

   参数：
     Res: TCnBigComplex             - 用来容纳结果的大整数对象
     Complex: TCnBigComplex         - 待计算的大整数复数

   返回值：Boolean                        - 返回是否求值成功
}

function BigComplexArgument(Complex: TCnBigComplex): Extended;
{* 返回大整数复数的辐角主值，也即与复平面正 X 轴的夹角，范围在 0 到 2π。

   参数：
     Complex: TCnBigComplex         - 待计算的大整数复数

   返回值：Extended                       - 返回大整数复数的辐角主值，单位为弧度
}

// ============================ 大浮点复数运算 =================================

function BigComplexDecimalIsZero(Complex: TCnBigComplexDecimal): Boolean;
{* 返回大浮点复数是否为 0。

   参数：
     Complex: TCnBigComplexDecimal        - 待判断的大浮点复数

   返回值：Boolean                        - 大浮点复数是否为零
}

procedure BigComplexDecimalSetZero(Complex: TCnBigComplexDecimal);
{* 将大浮点复数设置为 0。

   参数：
     Complex: TCnBigComplexDecimal        - 待设置的大浮点复数

   返回值：（无）
}

procedure BigComplexDecimalSetOne(Complex: TCnBigComplexDecimal);
{* 将大浮点复数设置为 1

   参数：
     Complex: TCnBigComplexDecimal        - 待设置的大浮点复数

   返回值：（无）
}

procedure BigComplexDecimalSetI(Complex: TCnBigComplexDecimal);
{* 将大浮点复数设置为 i。

   参数：
     Complex: TCnBigComplexDecimal        - 待设置的大浮点复数

   返回值：（无）
}

procedure BigComplexDecimalSetValue(Complex: TCnBigComplexDecimal;
  AR: Int64; AI: Int64); overload;
{* 设置大浮点复数值。

   参数：
     Complex: TCnBigComplexDecimal        - 待赋值的大浮点复数
     AR: Int64                            - 大浮点复数的实部
     AI: Int64                            - 大浮点复数的虚部

   返回值：（无）
}

procedure BigComplexDecimalSetValue(Complex: TCnBigComplexDecimal;
  const AR: string; const AI: string); overload;
{* 设置大浮点复数值。

   参数：
     Complex: TCnBigComplexDecimal        - 待赋值的大浮点复数
     const AR: string                     - 实部的十进制数字符串形式
     const AI: string                     - 虚部的十进制数字符串形式

   返回值：（无）
}

procedure BigComplexDecimalSetString(Complex: TCnBigComplexDecimal; const Str: string);
{* 用形如 1.4+2i、3i、0、2-i 这种字符串给大浮点复数赋值。

   参数：
     Complex: TCnBigComplexDecimal        - 待赋值的大浮点复数复数
     const Str: string                    - 复数值字符串

   返回值：（无）
}

function BigComplexDecimalToString(Complex: TCnBigComplexDecimal): string;
{* 将大浮点复数转换为形如 a + bi 的字符串，实部或虚部为 0 时相应省略。

   参数：
     Complex: TCnBigComplexDecimal        - 待转换的大浮点复数

   返回值：string                         - 返回大浮点复数的字符串形式
}

function BigComplexDecimalEqual(Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal): Boolean;
{* 判断两个大浮点复数是否相等。

   参数：
     Complex1: TCnBigComplexDecimal       - 待比较的大浮点复数一
     Complex2: TCnBigComplexDecimal       - 待比较的大浮点复数二

   返回值：Boolean                        - 两个复数是否相等
}

procedure BigComplexDecimalSwap(Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal);
{* 交换两个大浮点复数的值。

   参数：
     Complex1: TCnBigComplexDecimal       - 待交换的大浮点复数一
     Complex2: TCnBigComplexDecimal       - 待交换的大浮点复数二

   返回值：（无）
}

procedure BigComplexDecimalCopy(Dest: TCnBigComplexDecimal; Source: TCnBigComplexDecimal);
{* 复制大浮点复数的值。

   参数：
     Dest: TCnBigComplexDecimal           - 目标大浮点复数
     Source: TCnBigComplexDecimal         - 源大浮点复数

   返回值：（无）
}

procedure BigComplexDecimalAdd(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal); overload;
{* 大浮点复数加法。Complex1 和 Complex2 相加，结果存入大浮点复数 Res 中，Res 可以是 Complex1 或 Complex2。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Complex1: TCnBigComplexDecimal       - 大浮点复数加数一
     Complex2: TCnBigComplexDecimal       - 大浮点复数加数二

   返回值：（无）
}

procedure BigComplexDecimalSub(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal); overload;
{* 大浮点复数减法。Complex1 和 Complex2 相减，结果存入大浮点复数 Res 中，Res 可以是 Complex1 或 Complex2。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Complex1: TCnBigComplexDecimal       - 大浮点复数被减数
     Complex2: TCnBigComplexDecimal       - 大浮点复数减数

   返回值：（无）
}

procedure BigComplexDecimalMul(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal); overload;
{* 大浮点复数乘法。Complex1 和 Complex2 相乘，结果存入大浮点复数 Res 中，Res 可以是 Complex1 或 Complex2。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Complex1: TCnBigComplexDecimal       - 大浮点复数乘数一
     Complex2: TCnBigComplexDecimal       - 大浮点复数乘数二

   返回值：（无）
}

procedure BigComplexDecimalDiv(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal; DivPrecision: Integer = 0);
{* 大浮点复数除法。Complex1 和 Complex2 相除，结果存入大浮点复数 Res 中，Res 可以是 Complex1 或 Complex2。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Complex1: TCnBigComplexDecimal       - 大浮点复数被除数
     Complex2: TCnBigComplexDecimal       - 大浮点复数除数
     DivPrecision: Integer                - 保留小数点后几位，0 表示按默认设置来

   返回值：（无）
}

procedure BigComplexDecimalAdd(Res: TCnBigComplexDecimal;
  Complex: TCnBigComplexDecimal; Value: Int64); overload;
{* 大浮点复数与整数加法。Complex 和 Value 相加，结果存入 Res 中，Res 可以是 Complex。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Complex: TCnBigComplexDecimal        - 大浮点复数加数
     Value: Int64                         - 大浮点整数加数

   返回值：（无）
}

procedure BigComplexDecimalSub(Res: TCnBigComplexDecimal;
  Complex: TCnBigComplexDecimal; Value: Int64); overload;
{* 大浮点复数与整数减法。Complex 和 Value 相减，结果存入 Res 中，Res 可以是 Complex。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Complex: TCnBigComplexDecimal        - 大浮点复数被减数
     Value: Int64                         - 大浮点整数减数

   返回值：（无）
}

procedure BigComplexDecimalMul(Res: TCnBigComplexDecimal;
  Complex: TCnBigComplexDecimal; Value: Int64); overload;
{* 大浮点复数与整数乘法。Complex 和 Value 相乘，结果存入 Res 中，Res 可以是 Complex。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Complex: TCnBigComplexDecimal        - 大浮点复数乘数
     Value: Int64                         - 整数乘数

   返回值：（无）
}

procedure BigComplexDecimalRealMul(Res, Num: TCnBigComplexDecimal;
  RealValue: TCnBigDecimal);
{* 大浮点复数与大浮点实数的乘法，Res 和 Num 可以是同一个对象。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Num: TCnBigComplexDecimal            - 大浮点复数乘数
     RealValue: TCnBigDecimal             - 大浮点数乘数

   返回值：（无）
}

procedure BigComplexDecimalRealDiv(Res, Num: TCnBigComplexDecimal;
  RealValue: TCnBigDecimal; DivPrecision: Integer = 0);
{* 大浮点复数与大浮点实数的除法，Res 和 Num 可以是同一个对象。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Num: TCnBigComplexDecimal            - 大浮点复数被除数
     RealValue: TCnBigDecimal             - 大浮点数除数
     DivPrecision: Integer                - 保留小数点后几位，0 表示按默认设置来

   返回值：（无）
}

function BigComplexDecimalPower(Res, Num: TCnBigComplexDecimal;
  N: Integer): Boolean;
{* 大浮点复数的整数次幂，Res 和 Num 可以是同一个对象。

   参数：
     Res: TCnBigComplexDecimal            - 结果大浮点复数
     Num: TCnBigComplexDecimal            - 大浮点复数底
     N: Integer                           - 指数

   返回值：（无）
}

procedure BigComplexDecimalNegate(Res: TCnBigComplexDecimal; Complex: TCnBigComplexDecimal);
{* 求大浮点复数的相反数。结果存入 Res 中，Res 可以是 Complex。

   参数：
     Res: TCnBigComplexDecimal            - 结果复数取负值
     Complex: TCnBigComplexDecimal        - 待取负的复数

   返回值：（无）
}

procedure BigComplexDecimalConjugate(Res: TCnBigComplexDecimal; Complex: TCnBigComplexDecimal);
{* 求大浮点复数的共轭。结果存入 Res 中，Res 可以是 Complex。

   参数：
     Res: TCnBigComplexDecimal            - 结果复数共轭
     Complex: TCnBigComplexDecimal        - 待共轭的复数

   返回值：（无）
}

function BigComplexDecimalIsPureReal(Complex: TCnBigComplexDecimal): Boolean;
{* 判断大浮点复数是否为纯实数，也即判断虚部是否为 0。

   参数：
     Complex: TCnBigComplexDecimal        - 待判断的复数

   返回值：Boolean                        - 复数是否为纯实数
}

function BigComplexDecimalIsPureImaginary(Complex: TCnBigComplexDecimal): Boolean;
{* 判断大浮点复数是否为纯虚数，也即判断实部是否为 0 虚部不为 0。

   参数：
     Complex: TCnBigComplexDecimal        - 待判断的复数

   返回值：Boolean                        - 复数是否为纯虚数
}

function BigComplexDecimalAbsoluteValue(Complex: TCnBigComplexDecimal): Extended; overload;
{* 返回大浮点复数的绝对值，也即复平面上原点的距离，以浮点数表示。

   参数：
     Complex: TCnBigComplexDecimal        - 待求绝对值的大浮点复数

   返回值：Extended                       - 返回复大浮点数的绝对值
}

function BigComplexDecimalAbsoluteValue(Res: TCnBigDecimal; Complex: TCnBigComplexDecimal;
  Precision: Integer = 0): Boolean; overload;
{* 返回大浮点复数的绝对值，也即复平面上原点的距离，以大数表示。

   参数：
     Res: TCnBigComplexDecimal            - 用于存放结果的大浮点复数
     Complex: TCnBigComplexDecimal        - 待求绝对值的大浮点复数
     Precision: Integer                   - 保留小数点后几位，0 表示按默认设置来

   返回值：Boolean                        - 运算是否赋值成功
}

function BigComplexDecimalArgument(Complex: TCnBigComplexDecimal): Extended;
{* 返回大浮点复数的辐角值，也即复平面上与 X 轴的夹角，范围从 0 到 2π。

   参数：
     Complex: TCnBigComplexDecimal        - 待求辐角的大浮点复数

   返回值：Extended                       - 返回大浮点复数的辐角值，单位为弧度
}


var
  CnComplexZero: TCnComplexNumber;
  {* 复数 0}

  CnComplexOne: TCnComplexNumber;
  {* 复数 1}

  CnComplexOneI: TCnComplexNumber;
  {* 复数 i}

  CnComplexNegOneI: TCnComplexNumber;
  {* 复数 -i}

  CnBigComplexZero: TCnBigComplex;
  {* 复数 0}

  CnBigComplexOne: TCnBigComplex;
  {* 复数 1}

  CnBigComplexOneI: TCnBigComplex;
  {* 复数 i}

  CnBigComplexNegOneI: TCnBigComplex;
  {* 复数 -i}

  CnBigComplexDecimalZero: TCnBigComplexDecimal;
  {* 全局复数 0}

  CnBigComplexDecimalOne: TCnBigComplexDecimal;
  {* 全局复数 1}

  CnBigComplexDecimalOneI: TCnBigComplexDecimal;
  {* 全局复数 i}

  CnBigComplexDecimalNegOneI: TCnBigComplexDecimal;
  {* 全局复数 -i}

implementation

uses
  CnMath;

var
  FBigComplexDecimalPool: TCnBigComplexDecimalPool = nil;
  FBigDecimalPool: TCnBigDecimalPool = nil;
  FBigNumberPool: TCnBigNumberPool = nil;

function ComplexNumberIsZero(var Complex: TCnComplexNumber): Boolean;
begin
  Result := (Complex.R = 0) and (Complex.I = 0);
end;

procedure ComplexNumberSetZero(var Complex: TCnComplexNumber);
begin
  Complex.R := 0.0;
  Complex.I := 0.0;
end;

procedure ComplexNumberSetOne(var Complex: TCnComplexNumber);
begin
  Complex.R := 1.0;
  Complex.I := 0.0;
end;

procedure ComplexNumberSetI(var Complex: TCnComplexNumber);
begin
  Complex.R := 0.0;
  Complex.I := 1.0;
end;

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber; AR, AI: Extended);
begin
  Complex.R := AR;
  Complex.I := AI;
end;

procedure ComplexNumberSetValue(var Complex: TCnComplexNumber;
  const AR, AI: string);
begin
  ComplexNumberSetZero(Complex);
  if (AR = '') and (AI = '') then
    Exit
  else if AR = '' then
    Complex.I := StrToFloat(AI)
  else if AI = '' then
    Complex.R := StrToFloat(AR)
  else
    ComplexNumberSetValue(Complex, StrToFloat(AR), StrToFloat(AI));
end;

procedure ComplexNumberSetString(var Complex: TCnComplexNumber; const Str: string);
var
  S: string;
  PlusPos, MinusPos, IPos: Integer;
  RealPart, ImagPart: string;
  HasI: Boolean;
begin
  ComplexNumberSetZero(Complex);
  S := Trim(Str);
  if S = '' then
    Exit;

  IPos := Pos('i', LowerCase(S));
  HasI := IPos > 0;

  if not HasI then
  begin
    Complex.R := StrToFloat(S);
    Exit;
  end;

  if IPos > 0 then
    Delete(S, IPos, 1);
  S := Trim(S);

  PlusPos := 0;
  MinusPos := 0;
  if Length(S) > 1 then
  begin
    PlusPos := Pos('+', Copy(S, 2, Length(S)));
    if PlusPos > 0 then
      Inc(PlusPos);

    MinusPos := Pos('-', Copy(S, 2, Length(S)));
    if MinusPos > 0 then
      Inc(MinusPos);
  end;

  if (PlusPos > 0) or (MinusPos > 0) then
  begin
    if PlusPos > 0 then
    begin
      RealPart := Trim(Copy(S, 1, PlusPos - 1));
      ImagPart := Trim(Copy(S, PlusPos + 1, Length(S)));
    end
    else
    begin
      RealPart := Trim(Copy(S, 1, MinusPos - 1));
      ImagPart := Trim(Copy(S, MinusPos, Length(S)));
    end;

    if RealPart <> '' then
      Complex.R := StrToFloat(RealPart);
    if ImagPart = '-' then
      Complex.I := -1
    else if ImagPart <> '' then
      Complex.I := StrToFloat(ImagPart)
    else
      Complex.I := 1.0;
  end
  else
  begin
    if S = '' then
      Complex.I := 1.0
    else if S = '-' then
      Complex.I := -1.0
    else if S = '+' then
      Complex.I := 1.0
    else
      Complex.I := StrToFloat(S);
  end;
end;

function ComplexNumberToString(var Complex: TCnComplexNumber): string;
begin
  if ComplexIsPureReal(Complex) then
    Result := Format('%f', [Complex.R])
  else if ComplexIsPureImaginary(Complex) then
    Result := Format('%fi', [Complex.I])
  else if Complex.I < 0 then
    Result := Format('%f%fi', [Complex.R, Complex.I])
  else
    Result := Format('%f+%fi', [Complex.R, Complex.I]);
end;

function ComplexNumberEqual(var Complex1, Complex2: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex1.R, Complex2.R) and FloatEqual(Complex1.I, Complex2.I);
end;

procedure ComplexNumberSwap(var Complex1, Complex2: TCnComplexNumber);
var
  T: Extended;
begin
  T := Complex1.R;
  Complex1.R := Complex2.R;
  Complex2.R := T;

  T := Complex1.I;
  Complex1.I := Complex2.I;
  Complex2.I := T;
end;

procedure ComplexNumberCopy(var Dest, Source: TCnComplexNumber);
begin
  Dest.R := Source.R;
  Dest.I := Source.I;
end;

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
begin
  Res.R := Complex1.R + Complex2.R;
  Res.I := Complex1.I + Complex2.I;
end;

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
begin
  Res.R := Complex1.R - Complex2.R;
  Res.I := Complex1.I - Complex2.I;
end;

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
var
  T: Extended;
begin
  T := Complex1.R * Complex2.R - Complex1.I * Complex2.I;
  Res.I := Complex1.R * Complex2.I + Complex1.I * Complex2.R;
  Res.R := T;
end;

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex1, Complex2: TCnComplexNumber);
var
  T, D: Extended;
begin
  D := Complex2.R * Complex2.R + Complex2.I * Complex2.I;
  if FloatEqual(D, 0.0) then
    raise EZeroDivide.Create(SZeroDivide);

  T := (Complex1.R * Complex2.R + Complex1.I * Complex2.I) / D;
  Res.I := (Complex1.I * Complex2.R - Complex1.R * Complex2.I) / D;
  Res.R := T;
end;

procedure ComplexNumberAdd(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R + Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberSub(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R - Value;
  Res.I := Complex.I;
end;

procedure ComplexNumberMul(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R * Value;
  Res.I := Complex.I * Value;
end;

procedure ComplexNumberDiv(var Res: TCnComplexNumber;
  var Complex: TCnComplexNumber; Value: Extended); overload;
begin
  Res.R := Complex.R / Value;
  Res.I := Complex.I / Value;
end;

procedure ComplexNumberSqrt(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
var
  R, A: Extended;
begin
  R := FloatSqrt(ComplexNumberAbsoluteValue(Complex));
  A := ComplexNumberArgument(Complex) / 2;

  ComplexNumberSetAbsoluteArgument(Res, R, A);
end;

procedure ComplexNegate(var Res: TCnComplexNumber; var Complex: TCnComplexNumber);
begin
  Res.R := -Complex.R;
  Res.I := -Complex.I;
end;

procedure ComplexConjugate(var Res, Complex: TCnComplexNumber);
begin
  Res.R := Complex.R;
  Res.I := -Complex.I;
end;

function ComplexIsPureReal(var Complex: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex.I, 0.0);
end;

function ComplexIsPureImaginary(var Complex: TCnComplexNumber): Boolean;
begin
  Result := FloatEqual(Complex.R, 0.0) and not FloatEqual(Complex.I, 0.0);
end;

function ComplexNumberAbsoluteValue(var Complex: TCnComplexNumber): Extended;
begin
  Result := Sqrt(Complex.R * Complex.R + Complex.I * Complex.I);
end;

function ComplexNumberArgument(var Complex: TCnComplexNumber): Extended;
begin
  if Complex.I = 0 then
  begin
    if Complex.R >= 0 then     // 正实数辐角返回 0，包括 0 也凑合着返回 0
      Result := 0
    else
      Result := CN_PI;         // 复实数辐角返回 π
  end
  else if Complex.R = 0 then
  begin
    if Complex.I > 0 then      // 正纯虚数辐角返回半 π
      Result := CN_PI / 2
    else
      Result := CN_PI + CN_PI / 2;   // 复纯虚数辐角返回 3π/2
  end
  else // 实部虚部均不为 0
  begin
    Result := ArcTan2(Complex.I, Complex.R);
    if Result < 0 then
      Result := Result + CN_PI * 2;
  end;
end;

procedure ComplexNumberSetAbsoluteArgument(var Complex: TCnComplexNumber;
  AnAbsolute, AnArgument: Extended);
begin
  Complex.R := AnAbsolute * Cos(AnArgument);
  Complex.I := AnAbsolute * Sin(AnArgument);
end;

function BigComplexIsZero(Complex: TCnBigComplex): Boolean;
begin
  Result := Complex.FR.IsZero and Complex.FI.IsZero;
end;

procedure BigComplexSetZero(Complex: TCnBigComplex);
begin
  Complex.FR.SetZero;
  Complex.FI.SetZero;
end;

procedure BigComplexSetOne(Complex: TCnBigComplex);
begin
  Complex.FR.SetOne;
  Complex.FI.SetZero;
end;

procedure BigComplexSetI(Complex: TCnBigComplex);
begin
  Complex.FR.SetZero;
  Complex.FI.SetOne;
end;

procedure BigComplexSetValue(Complex: TCnBigComplex;
  AR: Int64; AI: Int64);
begin
  Complex.FR.SetInt64(AR);
  Complex.FI.SetInt64(AI);
end;

procedure BigComplexSetValue(Complex: TCnBigComplex;
  const AR: string; const AI: string);
begin
  Complex.FR.SetDec(AnsiString(AR));
  Complex.FI.SetDec(AnsiString(AI));
end;

procedure BigComplexSetString(Complex: TCnBigComplex; const Str: string);
var
  S: string;
  PlusPos, MinusPos, IPos: Integer;
  RealPart, ImagPart: string;
  HasI: Boolean;
begin
  BigComplexSetZero(Complex);
  S := Trim(Str);
  if S = '' then
    Exit;

  IPos := Pos('i', LowerCase(S));
  HasI := IPos > 0;

  if not HasI then
  begin
    Complex.FR.SetDec(AnsiString(S));
    Exit;
  end;

  if IPos > 0 then
    Delete(S, IPos, 1);
  S := Trim(S);

  PlusPos := 0;
  MinusPos := 0;
  if Length(S) > 1 then
  begin
    PlusPos := Pos('+', Copy(S, 2, Length(S)));
    if PlusPos > 0 then
      Inc(PlusPos);

    MinusPos := Pos('-', Copy(S, 2, Length(S)));
    if MinusPos > 0 then
      Inc(MinusPos);
  end;

  if (PlusPos > 0) or (MinusPos > 0) then
  begin
    if PlusPos > 0 then
    begin
      RealPart := Trim(Copy(S, 1, PlusPos - 1));
      ImagPart := Trim(Copy(S, PlusPos + 1, Length(S)));
    end
    else
    begin
      RealPart := Trim(Copy(S, 1, MinusPos - 1));
      ImagPart := Trim(Copy(S, MinusPos, Length(S)));
    end;

    if RealPart <> '' then
      Complex.FR.SetDec(AnsiString(RealPart));
    if ImagPart <> '' then
      Complex.FI.SetDec(AnsiString(ImagPart))
    else
      Complex.FI.SetOne;
  end
  else
  begin
    if S = '' then
      Complex.FI.SetOne
    else if S = '-' then
    begin
      Complex.FI.SetOne;
      Complex.FI.SetNegative(True);
    end
    else if S = '+' then
      Complex.FI.SetOne
    else
      Complex.FI.SetDec(AnsiString(S));
  end;
end;

function BigComplexToString(Complex: TCnBigComplex): string;
begin
  if BigComplexIsPureReal(Complex) then
    Result := Complex.FR.ToDec
  else if BigComplexIsPureImaginary(Complex) then
    Result := Complex.FI.ToDec + 'i'
  else if Complex.FI.IsNegative then
    Result := Complex.FR.ToDec + Complex.FI.ToDec
  else
    Result := Complex.FR.ToDec + '+' + Complex.FI.ToDec;
end;

function BigComplexEqual(Complex1: TCnBigComplex; Complex2: TCnBigComplex): Boolean;
begin
  Result := BigNumberEqual(Complex1.FR, Complex2.FR) and BigNumberEqual(Complex1.FI, Complex2.FI);
end;

procedure BigComplexSwap(Complex1: TCnBigComplex; Complex2: TCnBigComplex);
begin
  BigNumberSwap(Complex1.FR, Complex2.FR);
  BigNumberSwap(Complex1.FI, Complex2.FI);
end;

procedure BigComplexCopy(Dest: TCnBigComplex; Source: TCnBigComplex);
begin
  BigNumberCopy(Dest.FR, Source.FR);
  BigNumberCopy(Dest.FI, Source.FI);
end;

procedure BigComplexAdd(Res: TCnBigComplex;
  Complex1: TCnBigComplex; Complex2: TCnBigComplex);
begin
  BigNumberAdd(Res.FR, Complex1.FR, Complex2.FR);
  BigNumberAdd(Res.FI, Complex1.FI, Complex2.FI);
end;

procedure BigComplexSub(Res: TCnBigComplex;
  Complex1: TCnBigComplex; Complex2: TCnBigComplex);
begin
  BigNumberSub(Res.FR, Complex1.FR, Complex2.FR);
  BigNumberSub(Res.FI, Complex1.FI, Complex2.FI);
end;

procedure BigComplexMul(Res: TCnBigComplex;
  Complex1: TCnBigComplex; Complex2: TCnBigComplex);
var
  T1, T2, T3, T4: TCnBigNumber;
begin
  // (a+bi) * (c+di) = (ac-bd) + (ad+bc)i
  T1 := FBigNumberPool.Obtain;
  T2 := FBigNumberPool.Obtain;
  T3 := FBigNumberPool.Obtain;
  T4 := FBigNumberPool.Obtain;
  try
    BigNumberMul(T1, Complex1.FR, Complex2.FR);  // ac
    BigNumberMul(T2, Complex1.FI, Complex2.FI);  // bd
    BigNumberMul(T3, Complex1.FR, Complex2.FI);  // ad
    BigNumberMul(T4, Complex1.FI, Complex2.FR);  // bc

    BigNumberSub(Res.FR, T1, T2);  // ac - bd
    BigNumberAdd(Res.FI, T3, T4);  // ad + bc
  finally
    FBigNumberPool.Recycle(T4);
    FBigNumberPool.Recycle(T3);
    FBigNumberPool.Recycle(T2);
    FBigNumberPool.Recycle(T1);
  end;
end;

procedure BigComplexAdd(Res: TCnBigComplex;
  Complex: TCnBigComplex; Value: Int64);
var
  T: TCnBigNumber;
begin
  BigComplexCopy(Res, Complex);
  T := TCnBigNumber.Create;
  try
    T.SetInt64(Value);
    BigNumberAdd(Res.FR, Res.FR, T);
  finally
    T.Free;
  end;
end;

procedure BigComplexSub(Res: TCnBigComplex;
  Complex: TCnBigComplex; Value: Int64);
var
  T: TCnBigNumber;
begin
  BigComplexCopy(Res, Complex);
  T := TCnBigNumber.Create;
  try
    T.SetInt64(Value);
    BigNumberSub(Res.FR, Res.FR, T);
  finally
    T.Free;
  end;
end;

procedure BigComplexMul(Res: TCnBigComplex;
  Complex: TCnBigComplex; Value: Int64);
var
  T: TCnBigNumber;
begin
  BigComplexCopy(Res, Complex);
  T := TCnBigNumber.Create;
  try
    T.SetInt64(Value);
    BigNumberMul(Res.FR, Res.FR, T);
    BigNumberMul(Res.FI, Res.FI, T);
  finally
    T.Free;
  end;
end;

procedure BigComplexNegate(Res: TCnBigComplex; Complex: TCnBigComplex);
begin
  BigNumberCopy(Res.FR, Complex.FR);
  BigNumberCopy(Res.FI, Complex.FI);
  Res.FR.Negate;
  Res.FI.Negate;
end;

procedure BigComplexConjugate(Res: TCnBigComplex; Complex: TCnBigComplex);
begin
  BigNumberCopy(Res.FR, Complex.FR);
  BigNumberCopy(Res.FI, Complex.FI);
  Res.FI.Negate;
end;

function BigComplexIsPureReal(Complex: TCnBigComplex): Boolean;
begin
  Result := Complex.FI.IsZero;
end;

function BigComplexIsPureImaginary(Complex: TCnBigComplex): Boolean;
begin
  Result := Complex.FR.IsZero and not Complex.FI.IsZero;
end;

function BigComplexAbsoluteValue(Complex: TCnBigComplex): Extended;
var
  X, Y: Extended;
begin
  X := BigNumberGetFloat(Complex.FR);
  Y := BigNumberGetFloat(Complex.FI);
  Result := Sqrt(X * X + Y * Y);
end;

function BigComplexAbsoluteValue(Res: TCnBigNumber; Complex: TCnBigComplex): Boolean;
var
  X, Y: TCnBigNumber;
begin
  X := BigNumberDuplicate(Complex.FR);
  Y := BigNumberDuplicate(Complex.FI);
  BigNumberMul(X, X, X);
  BigNumberMul(Y, Y, Y);
  BigNumberAdd(Res, X, Y);
  Result := BigNumberSqrt(Res, Res);
end;

function BigComplexArgument(Complex: TCnBigComplex): Extended;
var
  X, Y: Extended;
begin
  X := BigNumberGetFloat(Complex.FR);
  Y := BigNumberGetFloat(Complex.FI);

  if Complex.FI.IsZero then
  begin
    if not Complex.FR.IsNegative then
      Result := 0                    // 正实数辐角返回 0，包括 0 也凑合着返回 0
    else
      Result := CN_PI;               // 复实数辐角返回 π
  end
  else if Complex.FR.IsZero then
  begin
    if not Complex.FI.IsZero and not Complex.FI.IsNegative then
      Result := CN_PI / 2            // 正纯虚数辐角返回半 π
    else
      Result := CN_PI + CN_PI / 2;   // 复纯虚数辐角返回 3π/2
  end
  else // 实部虚部均不为 0
  begin
    Result := ArcTan2(Y, X);
    if Result < 0 then
      Result := Result + CN_PI * 2;
  end;
end;

{ TCnBigComplex }

function TCnBigComplex.AbsoluteValue(Res: TCnBigNumber): Boolean;
begin
  Result := BigComplexAbsoluteValue(Res, Self);
end;

function TCnBigComplex.AbsoluteValue: Extended;
begin
  Result := BigComplexAbsoluteValue(Self);
end;

function TCnBigComplex.Argument: Extended;
begin
  Result := BigComplexArgument(Self);
end;

procedure TCnBigComplex.Conjugate;
begin
  BigComplexConjugate(Self, Self);
end;

constructor TCnBigComplex.Create;
begin
  inherited;
  FR := TCnBigNumber.Create;
  FI := TCnBigNumber.Create;
end;

destructor TCnBigComplex.Destroy;
begin
  FI.Free;
  FR.Free;
  inherited;
end;

function TCnBigComplex.IsPureImaginary: Boolean;
begin
  Result := BigComplexIsPureImaginary(Self);
end;

function TCnBigComplex.IsPureReal: Boolean;
begin
  Result := BigComplexIsPureReal(Self);
end;

function TCnBigComplex.IsZero: Boolean;
begin
  Result := BigComplexIsZero(Self);
end;

procedure TCnBigComplex.Negate;
begin
  BigComplexNegate(Self, Self);
end;

procedure TCnBigComplex.SetI;
begin
  BigComplexSetI(Self);
end;

procedure TCnBigComplex.SetOne;
begin
  BigComplexSetOne(Self);
end;

procedure TCnBigComplex.SetString(const Str: string);
begin
  BigComplexSetString(Self, Str);
end;

procedure TCnBigComplex.SetValue(const AR, AI: string);
begin
  BigComplexSetValue(Self, AR, AI);
end;

procedure TCnBigComplex.SetValue(AR, AI: Int64);
begin
  BigComplexSetValue(Self, AR, AI);
end;

procedure TCnBigComplex.SetZero;
begin
  BigComplexSetZero(Self);
end;

function TCnBigComplex.ToString: string;
begin
  Result := BigComplexToString(Self);
end;

{ TCnBigComplexPool }

function TCnBigComplexPool.CreateObject: TObject;
begin
  Result := TCnBigComplex.Create;
end;

function TCnBigComplexPool.Obtain: TCnBigComplex;
begin
  Result := TCnBigComplex(inherited Obtain);
end;

procedure TCnBigComplexPool.Recycle(Num: TCnBigComplex);
begin
  inherited Recycle(Num);
end;

{ TCnBigComplexList }

function TCnBigComplexList.Add(ABigComplex: TCnBigComplex): Integer;
begin
  Result := inherited Add(ABigComplex);
end;

function TCnBigComplexList.Add: TCnBigComplex;
begin
  Result := TCnBigComplex.Create;
  Add(Result);
end;

function TCnBigComplexList.Add(AR, AI: Integer): TCnBigComplex;
begin
  Result := TCnBigComplex.Create;
  Result.SetValue(AR, AI);
  Add(Result);
end;

procedure TCnBigComplexList.AddList(List: TCnBigComplexList);
var
  I: Integer;
  T: TCnBigComplex;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
    begin
      T := TCnBigComplex.Create;
      BigComplexCopy(T, List[I]);
      Add(T);
    end;
  end;
end;

constructor TCnBigComplexList.Create;
begin
  inherited Create(True);
end;

destructor TCnBigComplexList.Destroy;
begin

  inherited;
end;

function TCnBigComplexList.GetItem(Index: Integer): TCnBigComplex;
begin
  Result := TCnBigComplex(inherited GetItem(Index));
end;

function TCnBigComplexList.IndexOfValue(ABigComplex: TCnBigComplex): Integer;
begin
  Result := 0;
  while (Result < Count) and BigComplexEqual(Items[Result], ABigComplex) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TCnBigComplexList.Insert(Index: Integer;
  ABigComplex: TCnBigComplex);
begin
  inherited Insert(Index, ABigComplex);
end;

function TCnBigComplexList.Remove(ABigComplex: TCnBigComplex): Integer;
begin
  Result := inherited Remove(ABigComplex);
end;

procedure TCnBigComplexList.RemoveDuplicated;
var
  I, Idx: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    // 去除重复的项
    Idx := IndexOfValue(Items[I]);
    if (Idx >= 0) and (Idx <> I) then
      Delete(I);
  end;
end;

procedure TCnBigComplexList.SetItem(Index: Integer;
  ABigComplex: TCnBigComplex);
begin
  inherited SetItem(Index, ABigComplex);
end;

procedure TCnBigComplexList.SumTo(Sum: TCnBigComplex);
var
  I: Integer;
begin
  Sum.SetZero;
  for I := 0 to Count - 1 do
    BigComplexAdd(Sum, Sum, Items[I]);
end;

function TCnBigComplexList.ToString: string;
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

function BigComplexDecimalIsZero(Complex: TCnBigComplexDecimal): Boolean;
begin
  Result := Complex.FR.IsZero and Complex.FI.IsZero;
end;

function BigComplexDecimalIsOne(Complex: TCnBigComplexDecimal): Boolean;
begin
  Result := Complex.FR.IsOne and Complex.FI.IsZero;
end;

function BigComplexDecimalIsNegOne(Complex: TCnBigComplexDecimal): Boolean;
begin
  Result := Complex.FR.IsNegOne and Complex.FI.IsZero;
end;

procedure BigComplexDecimalSetZero(Complex: TCnBigComplexDecimal);
begin
  Complex.FR.SetZero;
  Complex.FI.SetZero;
end;

procedure BigComplexDecimalSetOne(Complex: TCnBigComplexDecimal);
begin
  Complex.FR.SetOne;
  Complex.FI.SetZero;
end;

procedure BigComplexDecimalSetI(Complex: TCnBigComplexDecimal);
begin
  Complex.FR.SetZero;
  Complex.FI.SetOne;
end;

procedure BigComplexDecimalSetValue(Complex: TCnBigComplexDecimal;
  AR: Int64; AI: Int64);
begin
  Complex.FR.SetInt64(AR);
  Complex.FI.SetInt64(AI);
end;

procedure BigComplexDecimalSetValue(Complex: TCnBigComplexDecimal;
  const AR: string; const AI: string);
begin
  Complex.FR.SetDec(AR);
  Complex.FI.SetDec(AI);
end;

procedure BigComplexDecimalSetString(Complex: TCnBigComplexDecimal; const Str: string);
var
  S: string;
  PlusPos, MinusPos, IPos: Integer;
  RealPart, ImagPart: string;
  HasI: Boolean;
begin
  BigComplexDecimalSetZero(Complex);
  S := Trim(Str);
  if S = '' then
    Exit;

  IPos := Pos('i', LowerCase(S));
  HasI := IPos > 0;

  if not HasI then
  begin
    Complex.FR.SetDec(S);
    Exit;
  end;

  if IPos > 0 then
    Delete(S, IPos, 1);
  S := Trim(S);

  PlusPos := 0;
  MinusPos := 0;
  if Length(S) > 1 then
  begin
    PlusPos := Pos('+', Copy(S, 2, Length(S)));
    if PlusPos > 0 then
      Inc(PlusPos);

    MinusPos := Pos('-', Copy(S, 2, Length(S)));
    if MinusPos > 0 then
      Inc(MinusPos);
  end;

  if (PlusPos > 0) or (MinusPos > 0) then
  begin
    if PlusPos > 0 then
    begin
      RealPart := Trim(Copy(S, 1, PlusPos - 1));
      ImagPart := Trim(Copy(S, PlusPos + 1, Length(S)));
    end
    else
    begin
      RealPart := Trim(Copy(S, 1, MinusPos - 1));
      ImagPart := Trim(Copy(S, MinusPos, Length(S)));
    end;

    if RealPart <> '' then
      Complex.FR.SetDec(RealPart);
    if ImagPart = '-' then
    begin
      Complex.FI.SetOne;
      Complex.FI.Negate;
    end
    else if ImagPart <> '' then
      Complex.FI.SetDec(ImagPart)
    else
      Complex.FI.SetOne;
  end
  else
  begin
    if S = '' then
      Complex.FI.SetOne
    else if S = '-' then
    begin
      Complex.FI.SetOne;
      Complex.FI.Negate;
    end
    else if S = '+' then
      Complex.FI.SetOne
    else
      Complex.FI.SetDec(S);
  end;
end;

function BigComplexDecimalToString(Complex: TCnBigComplexDecimal): string;
var
  SI: string;
begin
  SI := Complex.FI.ToString;
  if BigComplexDecimalIsPureReal(Complex) then
    Result := Complex.FR.ToString
  else if BigComplexDecimalIsPureImaginary(Complex) then
  begin
    if SI = '1' then
      Result := 'i'
    else
      Result := SI + 'i';
  end
  else if Complex.FI.IsNegative then
  begin
    if SI = '-1' then
      Result := Complex.FR.ToString + '-i'
    else
      Result := Complex.FR.ToString + SI + 'i';
  end
  else
  begin
    if SI = '1' then
      Result := Complex.FR.ToString + '+i'
    else
      Result := Complex.FR.ToString + '+' + SI + 'i';
  end;
end;

function BigComplexDecimalEqual(Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal): Boolean;
begin
  Result := BigDecimalEqual(Complex1.FR, Complex2.FR) and BigDecimalEqual(Complex1.FI, Complex2.FI);
end;

procedure BigComplexDecimalSwap(Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal);
var
  T: TCnBigDecimal;
begin
  T := FBigDecimalPool.Obtain;
  try
    BigDecimalCopy(T, Complex1.FR);
    BigDecimalCopy(Complex1.FR, Complex2.FR);
    BigDecimalCopy(Complex2.FR, T);

    BigDecimalCopy(T, Complex1.FI);
    BigDecimalCopy(Complex1.FI, Complex2.FI);
    BigDecimalCopy(Complex2.FI, T);
  finally
    FBigDecimalPool.Recycle(T);
  end;
end;

procedure BigComplexDecimalCopy(Dest: TCnBigComplexDecimal; Source: TCnBigComplexDecimal);
begin
  BigDecimalCopy(Dest.FR, Source.FR);
  BigDecimalCopy(Dest.FI, Source.FI);
end;

procedure BigComplexDecimalAdd(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal);
begin
  BigDecimalAdd(Res.FR, Complex1.FR, Complex2.FR);
  BigDecimalAdd(Res.FI, Complex1.FI, Complex2.FI);
end;

procedure BigComplexDecimalSub(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal);
begin
  BigDecimalSub(Res.FR, Complex1.FR, Complex2.FR);
  BigDecimalSub(Res.FI, Complex1.FI, Complex2.FI);
end;

procedure BigComplexDecimalMul(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal);
var
  T1, T2, T3, T4: TCnBigDecimal;
begin
  T1 := FBigDecimalPool.Obtain;
  T2 := FBigDecimalPool.Obtain;
  T3 := FBigDecimalPool.Obtain;
  T4 := FBigDecimalPool.Obtain;
  try
    BigDecimalMul(T1, Complex1.FR, Complex2.FR);
    BigDecimalMul(T2, Complex1.FI, Complex2.FI);
    BigDecimalMul(T3, Complex1.FR, Complex2.FI);
    BigDecimalMul(T4, Complex1.FI, Complex2.FR);

    BigDecimalSub(Res.FR, T1, T2);
    BigDecimalAdd(Res.FI, T3, T4);
  finally
    FBigDecimalPool.Recycle(T4);
    FBigDecimalPool.Recycle(T3);
    FBigDecimalPool.Recycle(T2);
    FBigDecimalPool.Recycle(T1);
  end;
end;

procedure BigComplexDecimalDiv(Res: TCnBigComplexDecimal;
  Complex1: TCnBigComplexDecimal; Complex2: TCnBigComplexDecimal; DivPrecision: Integer);
var
  T1, T2, D: TCnBigDecimal;
begin
  T1 := FBigDecimalPool.Obtain;;
  T2 := FBigDecimalPool.Obtain;;
  D := FBigDecimalPool.Obtain;;
  try
    BigDecimalMul(T1, Complex2.FR, Complex2.FR);
    BigDecimalMul(T2, Complex2.FI, Complex2.FI);
    BigDecimalAdd(D, T1, T2);

    if D.IsZero then
      raise EZeroDivide.Create(SZeroDivide);

    BigDecimalMul(T1, Complex1.FR, Complex2.FR);
    BigDecimalMul(T2, Complex1.FI, Complex2.FI);
    BigDecimalAdd(T1, T1, T2);
    BigDecimalDiv(Res.FR, T1, D, DivPrecision);

    BigDecimalMul(T1, Complex1.FI, Complex2.FR);
    BigDecimalMul(T2, Complex1.FR, Complex2.FI);
    BigDecimalSub(T1, T1, T2);
    BigDecimalDiv(Res.FI, T1, D, DivPrecision);
  finally
    FBigDecimalPool.Recycle(D);
    FBigDecimalPool.Recycle(T2);
    FBigDecimalPool.Recycle(T1);
  end;
end;

procedure BigComplexDecimalAdd(Res: TCnBigComplexDecimal;
  Complex: TCnBigComplexDecimal; Value: Int64);
var
  T: TCnBigDecimal;
begin
  BigComplexDecimalCopy(Res, Complex);

  T := FBigDecimalPool.Obtain;
  try
    T.SetInt64(Value);
    BigDecimalAdd(Res.FR, Res.FR, T);
  finally
    FBigDecimalPool.Recycle(T);
  end;
end;

procedure BigComplexDecimalSub(Res: TCnBigComplexDecimal;
  Complex: TCnBigComplexDecimal; Value: Int64);
var
  T: TCnBigDecimal;
begin
  BigComplexDecimalCopy(Res, Complex);

  T := FBigDecimalPool.Obtain;
  try
    T.SetInt64(Value);
    BigDecimalSub(Res.FR, Res.FR, T);
  finally
    FBigDecimalPool.Recycle(T);
  end;
end;

procedure BigComplexDecimalMul(Res: TCnBigComplexDecimal;
  Complex: TCnBigComplexDecimal; Value: Int64);
var
  T: TCnBigDecimal;
begin
  BigComplexDecimalCopy(Res, Complex);

  T := FBigDecimalPool.Obtain;
  try
    T.SetInt64(Value);
    BigDecimalMul(Res.FR, Res.FR, T);
    BigDecimalMul(Res.FI, Res.FI, T);
  finally
    FBigDecimalPool.Recycle(T);
  end;
end;

procedure BigComplexDecimalRealMul(Res, Num: TCnBigComplexDecimal;
  RealValue: TCnBigDecimal);
begin
  BigDecimalMul(Res.FR, Num.FR, RealValue);
  BigDecimalMul(Res.FI, Num.FI, RealValue);
end;

procedure BigComplexDecimalRealDiv(Res, Num: TCnBigComplexDecimal;
  RealValue: TCnBigDecimal; DivPrecision: Integer);
begin
  BigDecimalDiv(Res.FR, Num.FR, RealValue, DivPrecision);
  BigDecimalDiv(Res.FI, Num.FI, RealValue, DivPrecision);
end;

function BigComplexDecimalPower(Res, Num: TCnBigComplexDecimal;
  N: Integer): Boolean;
var
  T: TCnBigComplexDecimal;
begin
  Result := True;
  if N = 0 then
  begin
    Res.SetOne;
    Exit;
  end
  else if N = 1 then
  begin
    if Res <> Num then
      BigComplexDecimalCopy(Res, Num);
    Exit;
  end;

  if N < 0 then
  begin
    T := FBigComplexDecimalPool.Obtain;
    try
      T.SetOne;
      BigComplexDecimalDiv(T, T, Num);
      Result := BigComplexDecimalPower(Res, T, -N);
      Exit;
    finally
      FBigComplexDecimalPool.Recycle(T);
    end;
  end;

  T := FBigComplexDecimalPool.Obtain;
  try
    BigComplexDecimalCopy(T, Num);
    Res.SetOne;

    while N > 0 do
    begin
      if (N and 1) = 1 then
        BigComplexDecimalMul(Res, Res, T);

      N := N shr 1;
      if N > 0 then
        BigComplexDecimalMul(T, T, T);
    end;
  finally
    FBigComplexDecimalPool.Recycle(T);
  end;
end;

procedure BigComplexDecimalNegate(Res: TCnBigComplexDecimal; Complex: TCnBigComplexDecimal);
begin
  BigDecimalCopy(Res.FR, Complex.FR);
  BigDecimalCopy(Res.FI, Complex.FI);
  Res.FR.Negate;
  Res.FI.Negate;
end;

procedure BigComplexDecimalConjugate(Res: TCnBigComplexDecimal; Complex: TCnBigComplexDecimal);
begin
  BigDecimalCopy(Res.FR, Complex.FR);
  BigDecimalCopy(Res.FI, Complex.FI);
  Res.FI.Negate;
end;

function BigComplexDecimalIsPureReal(Complex: TCnBigComplexDecimal): Boolean;
begin
  Result := Complex.FI.IsZero;
end;

function BigComplexDecimalIsPureImaginary(Complex: TCnBigComplexDecimal): Boolean;
begin
  Result := Complex.FR.IsZero and not Complex.FI.IsZero;
end;

function BigComplexDecimalAbsoluteValue(Complex: TCnBigComplexDecimal): Extended;
var
  X, Y: TCnBigDecimal;
begin
  X := FBigDecimalPool.Obtain;
  Y := FBigDecimalPool.Obtain;
  try
    BigDecimalMul(X, Complex.FR, Complex.FR);
    BigDecimalMul(Y, Complex.FI, Complex.FI);

    BigDecimalAdd(X, X, Y);
    BigDecimalSqrt(Y, X);
    Result := BigDecimalToExtended(Y);
  finally
    FBigDecimalPool.Recycle(Y);
    FBigDecimalPool.Recycle(X);
  end;
end;

function BigComplexDecimalAbsoluteValue(Res: TCnBigDecimal;
  Complex: TCnBigComplexDecimal; Precision: Integer): Boolean;
var
  X, Y: TCnBigDecimal;
begin
  X := FBigDecimalPool.Obtain;
  Y := FBigDecimalPool.Obtain;
  try
    BigDecimalMul(X, Complex.FR, Complex.FR);
    BigDecimalMul(Y, Complex.FI, Complex.FI);
    BigDecimalAdd(Res, X, Y);
    Result := BigDecimalSqrt(Res, Res, Precision);
  finally
    FBigDecimalPool.Recycle(Y);
    FBigDecimalPool.Recycle(X);
  end;
end;

function BigComplexDecimalArgument(Complex: TCnBigComplexDecimal): Extended;
var
  X, Y: Extended;
begin
  X := BigDecimalToExtended(Complex.FR);
  Y := BigDecimalToExtended(Complex.FI);

  if Complex.FI.IsZero then
  begin
    if not Complex.FR.IsNegative then
      Result := 0
    else
      Result := CN_PI;
  end
  else if Complex.FR.IsZero then
  begin
    if not Complex.FI.IsNegative then
      Result := CN_PI / 2
    else
      Result := CN_PI + CN_PI / 2;
  end
  else
  begin
    Result := ArcTan2(Y, X);
    if Result < 0 then
      Result := Result + CN_PI * 2;
  end;
end;

{ TCnBigComplexDecimal }

constructor TCnBigComplexDecimal.Create;
begin
  inherited;
  FR := TCnBigDecimal.Create;
  FI := TCnBigDecimal.Create;
end;

destructor TCnBigComplexDecimal.Destroy;
begin
  FI.Free;
  FR.Free;
  inherited;
end;

function TCnBigComplexDecimal.AbsoluteValue(Res: TCnBigDecimal;
  Precision: Integer): Boolean;
begin
  Result := BigComplexDecimalAbsoluteValue(Res, Self, Precision);
end;

function TCnBigComplexDecimal.AbsoluteValue: Extended;
begin
  Result := BigComplexDecimalAbsoluteValue(Self);
end;

function TCnBigComplexDecimal.Argument: Extended;
begin
  Result := BigComplexDecimalArgument(Self);
end;

procedure TCnBigComplexDecimal.Conjugate;
begin
  BigComplexDecimalConjugate(Self, Self);
end;

function TCnBigComplexDecimal.IsPureImaginary: Boolean;
begin
  Result := BigComplexDecimalIsPureImaginary(Self);
end;

function TCnBigComplexDecimal.IsPureReal: Boolean;
begin
  Result := BigComplexDecimalIsPureReal(Self);
end;

function TCnBigComplexDecimal.IsZero: Boolean;
begin
  Result := BigComplexDecimalIsZero(Self);
end;

function TCnBigComplexDecimal.IsNegOne: Boolean;
begin
  Result := BigComplexDecimalIsNegOne(Self)
end;

function TCnBigComplexDecimal.IsOne: Boolean;
begin
  Result := BigComplexDecimalIsOne(Self);
end;

procedure TCnBigComplexDecimal.Negate;
begin
  BigComplexDecimalNegate(Self, Self);
end;

procedure TCnBigComplexDecimal.SetI;
begin
  BigComplexDecimalSetI(Self);
end;

procedure TCnBigComplexDecimal.SetOne;
begin
  BigComplexDecimalSetOne(Self);
end;

procedure TCnBigComplexDecimal.SetValue(const AR, AI: string);
begin
  BigComplexDecimalSetValue(Self, AR, AI);
end;

procedure TCnBigComplexDecimal.SetValue(AR, AI: Int64);
begin
  BigComplexDecimalSetValue(Self, AR, AI);
end;

procedure TCnBigComplexDecimal.SetZero;
begin
  BigComplexDecimalSetZero(Self);
end;

function TCnBigComplexDecimal.ToString: string;
begin
  Result := BigComplexDecimalToString(Self);
end;

procedure TCnBigComplexDecimal.RoundTo(Precision: Integer;
  RoundMode: TCnBigRoundMode);
begin
  FR.RoundTo(Precision, RoundMode);
  FI.RoundTo(Precision, RoundMode);
end;

procedure TCnBigComplexDecimal.SetString(const Str: string);
begin
  BigComplexDecimalSetString(Self, Str);
end;

{ TCnBigComplexDecimalPool }

function TCnBigComplexDecimalPool.CreateObject: TObject;
begin
  Result := TCnBigComplexDecimal.Create;
end;

function TCnBigComplexDecimalPool.Obtain: TCnBigComplexDecimal;
begin
  Result := TCnBigComplexDecimal(inherited Obtain);
  Result.SetZero;
end;

procedure TCnBigComplexDecimalPool.Recycle(Num: TCnBigComplexDecimal);
begin
  inherited Recycle(Num);
end;

{ TCnBigComplexDecimalList }

function TCnBigComplexDecimalList.Add(ABigComplexDecimal: TCnBigComplexDecimal): Integer;
begin
  Result := inherited Add(ABigComplexDecimal);
end;

function TCnBigComplexDecimalList.Add: TCnBigComplexDecimal;
begin
  Result := TCnBigComplexDecimal.Create;
  Add(Result);
end;

function TCnBigComplexDecimalList.Add(AR, AI: Integer): TCnBigComplexDecimal;
begin
  Result := TCnBigComplexDecimal.Create;
  Result.SetValue(AR, AI);
  Add(Result);
end;

procedure TCnBigComplexDecimalList.AddList(List: TCnBigComplexDecimalList);
var
  I: Integer;
  T: TCnBigComplexDecimal;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
    begin
      T := TCnBigComplexDecimal.Create;
      BigComplexDecimalCopy(T, List[I]);
      Add(T);
    end;
  end;
end;

constructor TCnBigComplexDecimalList.Create;
begin
  inherited Create(True);
end;

destructor TCnBigComplexDecimalList.Destroy;
begin

  inherited;
end;

function TCnBigComplexDecimalList.GetItem(Index: Integer): TCnBigComplexDecimal;
begin
  Result := TCnBigComplexDecimal(inherited GetItem(Index));
end;

function TCnBigComplexDecimalList.IndexOfValue(ABigComplexDecimal: TCnBigComplexDecimal): Integer;
begin
  Result := 0;
  while (Result < Count) and BigComplexDecimalEqual(Items[Result], ABigComplexDecimal) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TCnBigComplexDecimalList.Insert(Index: Integer;
  ABigComplexDecimal: TCnBigComplexDecimal);
begin
  inherited Insert(Index, ABigComplexDecimal);
end;

function TCnBigComplexDecimalList.Remove(ABigComplexDecimal: TCnBigComplexDecimal): Integer;
begin
  Result := inherited Remove(ABigComplexDecimal);
end;

procedure TCnBigComplexDecimalList.RemoveDuplicated;
var
  I, Idx: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    // 去除重复的项
    Idx := IndexOfValue(Items[I]);
    if (Idx >= 0) and (Idx <> I) then
      Delete(I);
  end;
end;

procedure TCnBigComplexDecimalList.SetItem(Index: Integer;
  ABigComplexDecimal: TCnBigComplexDecimal);
begin
  inherited SetItem(Index, ABigComplexDecimal);
end;

procedure TCnBigComplexDecimalList.SumTo(Sum: TCnBigComplexDecimal);
var
  I: Integer;
begin
  Sum.SetZero;
  for I := 0 to Count - 1 do
    BigComplexDecimalAdd(Sum, Sum, Items[I]);
end;

function TCnBigComplexDecimalList.ToString: string;
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

initialization
  ComplexNumberSetZero(CnComplexZero);

  CnComplexOne.R := 1;
  CnComplexOne.I := 0;

  CnComplexOneI.R := 0;
  CnComplexOneI.I := 1;

  CnComplexNegOneI.R := 0;
  CnComplexNegOneI.I := -1;

  CnBigComplexZero := TCnBigComplex.Create;
  CnBigComplexZero.FR.SetZero;
  CnBigComplexZero.FI.SetZero;

  CnBigComplexOne := TCnBigComplex.Create;
  CnBigComplexOne.FR.SetOne;
  CnBigComplexOne.FI.SetZero;

  CnBigComplexOneI := TCnBigComplex.Create;
  CnBigComplexOneI.FR.SetZero;
  CnBigComplexOneI.FI.SetOne;

  CnBigComplexNegOneI := TCnBigComplex.Create;
  CnBigComplexNegOneI.FR.SetZero;
  CnBigComplexNegOneI.FI.SetInteger(-1);

  CnBigComplexDecimalZero := TCnBigComplexDecimal.Create;
  CnBigComplexDecimalZero.FR.SetZero;
  CnBigComplexDecimalZero.FI.SetZero;

  CnBigComplexDecimalOne := TCnBigComplexDecimal.Create;
  CnBigComplexDecimalOne.FR.SetOne;
  CnBigComplexDecimalOne.FI.SetZero;

  CnBigComplexDecimalOneI := TCnBigComplexDecimal.Create;
  CnBigComplexDecimalOneI.FR.SetZero;
  CnBigComplexDecimalOneI.FI.SetOne;

  CnBigComplexDecimalNegOneI := TCnBigComplexDecimal.Create;
  CnBigComplexDecimalNegOneI.FR.SetZero;
  CnBigComplexDecimalNegOneI.FI.SetInt64(-1);

  FBigComplexDecimalPool := TCnBigComplexDecimalPool.Create;
  FBigDecimalPool := TCnBigDecimalPool.Create;
  FBigNumberPool := TCnBigNumberPool.Create;

finalization
  FBigNumberPool.Free;
  FBigDecimalPool.Free;
  FBigComplexDecimalPool.Free;

  CnBigComplexDecimalNegOneI.Free;
  CnBigComplexDecimalOneI.Free;
  CnBigComplexDecimalOne.Free;
  CnBigComplexDecimalZero.Free;

  CnBigComplexNegOneI.Free;
  CnBigComplexOneI.Free;
  CnBigComplexOne.Free;
  CnBigComplexZero.Free;

end.
