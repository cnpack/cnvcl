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

unit CnPolynomial;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：多项式运算实现单元
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：支持普通的整系数多项式四则运算，除法只支持除数最高次数为 1 的情况
*           支持有限扩域范围内的多项式四则运算，系数均 mod p 并且结果对本原多项式求余
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.08.28 V1.1
*               实现有限扩域中对本原多项式求余的模逆元
*           2020.08.21 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, Math, Contnrs, CnPrimeNumber, CnNativeDecl;

type
  ECnPolynomialException = class(Exception);

  TCnInt64Polynomial = class(TCnInt64List)
  {* 整系数多项式，系数范围为 Int64}
  private
    function GetMaxDegree: Integer;
    procedure SetMaxDegree(const Value: Integer);
  public
    constructor Create(LowToHighCoefficients: array of const); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure SetCoefficents(LowToHighCoefficients: array of const);
    {* 一次批量设置从低到高的系数}
    procedure CorrectTop;
    {* 剔除高次的 0 系数}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 将多项式转成字符串}
    function IsZero: Boolean;
    {* 返回是否为 0}
    procedure SetZero;
    {* 设为 0}
    function IsOne: Boolean;
    {* 返回是否为 1}
    procedure SetOne;
    {* 设为 1}
    property MaxDegree: Integer read GetMaxDegree write SetMaxDegree;
    {* 最高次数，0 开始}
  end;

  TCnInt64PolynomialPool = class(TObjectList)
  {* 整系数多项式池实现类，允许使用到大数的地方自行创建大数池}
  private
{$IFDEF MULTI_THREAD}
  {$IFDEF MSWINDOWS}
    FCriticalSection: TRTLCriticalSection;
  {$ELSE}
    FCriticalSection: TCriticalSection;
  {$ENDIF}
{$ENDIF}
    procedure Enter; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    procedure Leave; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Obtain: TCnInt64Polynomial;
    procedure Recycle(Poly: TCnInt64Polynomial);
  end;

function Int64PolynomialNew: TCnInt64Polynomial;
{* 创建一个动态分配的整系数多项式对象，等同于 TCnInt64Polynomial.Create}

procedure Int64PolynomialFree(const P: TCnInt64Polynomial);
{* 释放一个整系数多项式对象，等同于 TCnInt64Polynomial.Free}

function Int64PolynomialDuplicate(const P: TCnInt64Polynomial): TCnInt64Polynomial;
{* 从一个整系数多项式对象克隆一个新对象}

function Int64PolynomialCopy(const Dst: TCnInt64Polynomial;
  const Src: TCnInt64Polynomial): TCnInt64Polynomial;
{* 复制一个整系数多项式对象，成功返回 Dst}

function Int64PolynomialToString(const P: TCnInt64Polynomial;
  const VarName: string = 'X'): string;
{* 将一个整系数多项式对象转成字符串，未知数默认以 X 表示}

function Int64PolynomialIsZero(const P: TCnInt64Polynomial): Boolean;
{* 判断一个整系数多项式对象是否为 0}

procedure Int64PolynomialSetZero(const P: TCnInt64Polynomial);
{* 将一个整系数多项式对象设为 0}

function Int64PolynomialIsOne(const P: TCnInt64Polynomial): Boolean;
{* 判断一个整系数多项式对象是否为 1}

procedure Int64PolynomialSetOne(const P: TCnInt64Polynomial);
{* 将一个整系数多项式对象设为 1}

procedure Int64PolynomialNegate(const P: TCnInt64Polynomial);
{* 将一个整系数多项式对象所有系数求反}

procedure Int64PolynomialShiftLeft(const P: TCnInt64Polynomial; N: Integer);
{* 将一个整系数多项式对象左移 N 次，也就是各项指数都加 N}

procedure Int64PolynomialShiftRight(const P: TCnInt64Polynomial; N: Integer);
{* 将一个整系数多项式对象右移 N 次，也就是各项指数都减 N，小于 0 的忽略了}

function Int64PolynomialEqual(const A, B: TCnInt64Polynomial): Boolean;
{* 判断俩整系数多项式每项系数是否对应相等，是则返回 True}

// =========================== 多项式普通运算 ==================================

procedure Int64PolynomialAddWord(const P: TCnInt64Polynomial; N: Integer);
{* 将一个整系数多项式对象的常系数加上 N}

procedure Int64PolynomialSubWord(const P: TCnInt64Polynomial; N: Integer);
{* 将一个整系数多项式对象的常系数减去 N}

procedure Int64PolynomialMulWord(const P: TCnInt64Polynomial; N: Integer);
{* 将一个整系数多项式对象的各个系数都乘以 N}

procedure Int64PolynomialDivWord(const P: TCnInt64Polynomial; N: Integer);
{* 将一个整系数多项式对象的各个系数都除以 N，如不能整除则取整}

procedure Int64PolynomialNonNegativeModWord(const P: TCnInt64Polynomial; N: Int64);
{* 将一个整系数多项式对象的各个系数都对 N 非负求余}

function Int64PolynomialAdd(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial): Boolean;
{* 两个整系数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function Int64PolynomialSub(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial): Boolean;
{* 两个整系数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function Int64PolynomialMul(const Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
{* 两个整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function Int64PolynomialDiv(const Res: TCnInt64Polynomial; const Remain: TCnInt64Polynomial;
  const P: TCnInt64Polynomial; const Divisor: TCnInt64Polynomial): Boolean;
{* 两个整系数多项式对象相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   注意当商式或余式出现无法整除的分数时会抛出异常，无法支持，
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor}

function Int64PolynomialMod(const Res: TCnInt64Polynomial; const P: TCnInt64Polynomial;
  const Divisor: TCnInt64Polynomial): Boolean;
{* 两个整系数多项式对象求余，余数放至 Res 中，返回求余是否成功，
   注意当商式或余式出现无法整除的分数时会抛出异常，无法支持，
   Res 可以是 P 或 Divisor，P 可以是 Divisor}

function Int64PolynomialPower(const Res: TCnInt64Polynomial;
  const P: TCnInt64Polynomial;  Exponent: LongWord): Boolean;
{* 计算整系数多项式的 Exponent 次幂，不考虑系数溢出的问题，
   返回计算是否成功，Res 可以是 P}

function Int64PolynomialReduce(const P: TCnInt64Polynomial): Integer;
{* 化简多项式系数，也就是找多项式系数的最大公约数，各个系数除以它，返回最大公约数}

function Int64PolynomialGreatestCommonDivisor(const Res: TCnInt64Polynomial;
  const P1, P2: TCnInt64Polynomial): Boolean;
{* 计算两个整系数多项式的最大公因式，返回计算是否成功，Res 可以是 P1 或 P2
   注意计算可能会因为系数不能整除而失败，即使调用者自行保证 P1 P2 均为首一多项式也不能保证行}

function Int64PolynomialCompose(const Res: TCnInt64Polynomial;
  const F, P: TCnInt64Polynomial): Boolean;
{* 整系数多项式代换，也就是计算 F(P(x))，返回是否计算成功}

function Int64PolynomialCalcDivisionPolynomial(A, B: Integer; Degree: Integer;
  outDivisionPolynomial: TCnInt64Polynomial): Boolean;
{* 递归计算可除多项式，返回是否计算成功，注意 Integer 范围内次数一多就容易溢出
   规则参考自 F. MORAIN 的文章
  《COMPUTING THE CARDINALITY OF CM ELLIPTIC CURVES USING TORSION POINTS》}

// ===================== 有限扩域下的整系数多项式模运算 ========================

function Int64PolynomialGaloisAdd(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式
   返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function Int64PolynomialGaloisSub(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式
   返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function Int64PolynomialGaloisMul(const Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相乘，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function Int64PolynomialGaloisDiv(const Res: TCnInt64Polynomial;
  const Remain: TCnInt64Polynomial; const P: TCnInt64Polynomial;
  const Divisor: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor}

function Int64PolynomialGaloisMod(const Res: TCnInt64Polynomial; const P: TCnInt64Polynomial;
  const Divisor: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上求余，余数放至 Res 中，返回求余是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   Res 可以是 P 或 Divisor，P 可以是 Divisor}

function Int64PolynomialGaloisPower(const Res, P: TCnInt64Polynomial;
  Exponent: LongWord; Prime: Int64; Primitive: TCnInt64Polynomial = nil): Boolean;
{* 计算整系数多项式在 Prime 次方阶有限域上的 Exponent 次幂，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   返回计算是否成功，Res 可以是 P}

function Int64PolynomialGaloisAddWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
{* 将 Prime 次方阶有限域上的整系数多项式的常系数加上 N 再 mod Prime}

function Int64PolynomialGaloisSubWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
{* 将 Prime 次方阶有限域上的整系数多项式的常系数减去 N 再 mod Prime}

function Int64PolynomialGaloisMulWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
{* 将 Prime 次方阶有限域上的整系数多项式各项系数乘以 N 再 mod Prime}

function Int64PolynomialGaloisDivWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
{* 将 Prime 次方阶有限域上的整系数多项式各项系数除以 N，也就是乘以 N 的逆元再 mod Prime}

function Int64PolynomialGaloisMonic(const P: TCnInt64Polynomial; Prime: Int64): Integer;
{* 将 Prime 次方阶有限域上的整系数多项式各项系数同除最高项，使首项为一，返回除的值}

function Int64PolynomialGaloisGreatestCommonDivisor(const Res: TCnInt64Polynomial;
  const P1, P2: TCnInt64Polynomial; Prime: Int64): Boolean;
{* 计算两个整系数多项式在 Prime 次方阶有限域上的最大公因式，返回计算是否成功，Res 可以是 P1 或 P2}

procedure Int64PolynomialGaloisExtendedEuclideanGcd(A, B: TCnInt64Polynomial;
  X, Y: TCnInt64Polynomial; Prime: Int64);
{* 扩展欧几里得辗转相除法在 Prime 次方阶有限域上求二元一次不定整系数多项式方程 A * X - B * Y = 1 的解}

procedure Int64PolynomialGaloisModularInverse(const Res: TCnInt64Polynomial;
  X, Modulus: TCnInt64Polynomial; Prime: Int64);
{* 求整系数多项式 X 在 Prime 次方阶有限域上针对 Modulus 的模反多项式或叫模逆元多项式 Y，
   满足 (X * Y) mod M = 1，调用者须自行保证 X、Modulus 互素，且 Res 不能为 X 或 Modulus}

function Int64PolynomialGaloisCompose(const Res: TCnInt64Polynomial;
  const F, P: TCnInt64Polynomial; Prime: Int64): Boolean;
{* 在 Prime 次方阶有限域上进行整系数多项式代换，也就是计算 F(P(x))，返回是否计算成功}

function Int64PolynomialGaloisCalcDivisionPolynomial(A, B: Integer; Degree: Integer;
  outDivisionPolynomial: TCnInt64Polynomial; Prime: Int64): Boolean;
{* 递归计算在 Prime 次方阶有限域上的 N 阶可除多项式，返回是否计算成功
   规则参考自 F. MORAIN 的文章
  《COMPUTING THE CARDINALITY OF CM ELLIPTIC CURVES USING TORSION POINTS》}

implementation

resourcestring
  SCnInvalidDegree = 'Invalid Degree %d';
  SCnErrorDivExactly = 'Can NOT Divide Exactly for Integer Polynomial.';

var
  FLocalInt64PolynomialPool: TCnInt64PolynomialPool = nil;

// 封装的非负求余函数，也就是余数为负时，加个除数变正，调用者需保证 P 大于 0
function NonNegativeMod(N: Int64; P: Int64): Int64;
begin
  if P <= 0 then
    raise ECnPolynomialException.Create('Can NOT Mod a Negative Prime.');

  Result := N mod P;
  if N < 0 then
    Inc(Result, P);
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

function TCnInt64Polynomial.GetMaxDegree: Integer;
begin
  if Count = 0 then
    Add(0);
  Result := Count - 1;
end;

function TCnInt64Polynomial.IsOne: Boolean;
begin
  Result := Int64PolynomialIsOne(Self);
end;

function TCnInt64Polynomial.IsZero: Boolean;
begin
  Result := Int64PolynomialIsZero(Self);
end;

procedure TCnInt64Polynomial.SetCoefficents(LowToHighCoefficients: array of const);
var
  I: Integer;
begin
  Clear;
  for I := Low(LowToHighCoefficients) to High(LowToHighCoefficients) do
  begin
    case LowToHighCoefficients[I].VType of
    vtInteger:
      begin
        Add(LowToHighCoefficients[I].VInteger);
      end;
    vtInt64:
      begin
        Add(LowToHighCoefficients[I].VInt64^);
      end;
    vtBoolean:
      begin
        if LowToHighCoefficients[I].VBoolean then
          Add(1)
        else
          Add(0);
      end;
    vtString:
      begin
        Add(StrToInt(LowToHighCoefficients[I].VString^));
      end;
    else
      raise ECnPolynomialException.CreateFmt(SInvalidInteger, ['Coefficients ' + IntToStr(I)]);
    end;
  end;

  if Count = 0 then
    Add(0)
  else
    CorrectTop;
end;

procedure TCnInt64Polynomial.SetMaxDegree(const Value: Integer);
begin
  if Value < 0 then
    raise ECnPolynomialException.CreateFmt(SCnInvalidDegree, [Value]);
  Count := Value + 1;
end;

procedure TCnInt64Polynomial.SetOne;
begin
  Int64PolynomialSetOne(Self);
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

procedure Int64PolynomialFree(const P: TCnInt64Polynomial);
begin
  P.Free;
end;

function Int64PolynomialDuplicate(const P: TCnInt64Polynomial): TCnInt64Polynomial;
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

function Int64PolynomialCopy(const Dst: TCnInt64Polynomial;
  const Src: TCnInt64Polynomial): TCnInt64Polynomial;
var
  I: Integer;
begin
  Result := Dst;
  if Src <> Dst then
  begin
    Dst.Clear;
    for I := 0 to Src.Count - 1 do
      Dst.Add(Src[I]);
    Dst.CorrectTop;
  end;
end;

function Int64PolynomialToString(const P: TCnInt64Polynomial;
  const VarName: string = 'X'): string;
var
  I, C: Integer;

  function VarPower(E: Integer): string;
  begin
    if E = 0 then
      Result := ''
    else if E = 1 then
      Result := VarName
    else
      Result := VarName + '^' + IntToStr(E);
  end;

begin
  Result := '';
  if Int64PolynomialIsZero(P) then
  begin
    Result := '0';
    Exit;
  end;

  for I := P.MaxDegree downto 0 do
  begin
    C := P[I];
    if C = 0 then
    begin
      Continue;
    end
    else if C > 0 then
    begin
      if Result = '' then  // 最高项无需加号
        Result := IntToStr(C) + VarPower(I)
      else
        Result := Result + '+' + IntToStr(C) + VarPower(I);
    end
    else // 小于 0，要用减号
      Result := Result + IntToStr(C) + VarPower(I);
  end;
end;

function Int64PolynomialIsZero(const P: TCnInt64Polynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and (P[0] = 0);
end;

procedure Int64PolynomialSetZero(const P: TCnInt64Polynomial);
begin
  P.Clear;
  P.Add(0);
end;

function Int64PolynomialIsOne(const P: TCnInt64Polynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and (P[0] = 1);
end;

procedure Int64PolynomialSetOne(const P: TCnInt64Polynomial);
begin
  P.Clear;
  P.Add(1);
end;

procedure Int64PolynomialNegate(const P: TCnInt64Polynomial);
var
  I: Integer;
begin
  for I := 0 to P.MaxDegree do
    P[I] := -P[I];
end;

procedure Int64PolynomialShiftLeft(const P: TCnInt64Polynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    Int64PolynomialShiftRight(P, -N)
  else
  begin
    for I := 1 to N do
      P.Insert(0, 0);
  end;
end;

procedure Int64PolynomialShiftRight(const P: TCnInt64Polynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    Int64PolynomialShiftLeft(P, -N)
  else
  begin
    for I := 1 to N do
    begin
      if P.Count = 0 then
        Break;
      P.Delete(0);
    end;

    if P.Count = 0 then
      P.Add(0);
  end;
end;

function Int64PolynomialEqual(const A, B: TCnInt64Polynomial): Boolean;
var
  I: Integer;
begin
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

procedure Int64PolynomialAddWord(const P: TCnInt64Polynomial; N: Integer);
begin
  P[0] := P[0] + N;
end;

procedure Int64PolynomialSubWord(const P: TCnInt64Polynomial; N: Integer);
begin
  P[0] := P[0] - N;
end;

procedure Int64PolynomialMulWord(const P: TCnInt64Polynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
  begin
    Int64PolynomialSetZero(P);
    Exit;
  end
  else
  begin
    for I := 0 to P.MaxDegree do
      P[I] := P[I] * N;
  end;
end;

procedure Int64PolynomialDivWord(const P: TCnInt64Polynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    raise ECnPolynomialException.Create(SZeroDivide);

  for I := 0 to P.MaxDegree do
    P[I] := P[I] div N;
end;

procedure Int64PolynomialNonNegativeModWord(const P: TCnInt64Polynomial; N: Int64);
var
  I: Integer;
begin
  if N = 0 then
    raise ECnPolynomialException.Create(SZeroDivide);

  for I := 0 to P.MaxDegree do
    P[I] := NonNegativeMod(P[I], N);
end;

function Int64PolynomialAdd(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial): Boolean;
var
  I, D1, D2: Integer;
  PBig: TCnInt64Polynomial;
begin
  D1 := Max(P1.MaxDegree, P2.MaxDegree);
  D2 := Min(P1.MaxDegree, P2.MaxDegree);

  Res.MaxDegree := D1;
  if D1 > D2 then
  begin
    if P1.MaxDegree > P2.MaxDegree then
      PBig := P1
    else
      PBig := P2;

    for I := D1 downto D2 + 1 do
      Res[I] := PBig[I];
  end;

  for I := D2 downto 0 do
    Res[I] := P1[I] + P2[I];
  Res.CorrectTop;
  Result := True;
end;

function Int64PolynomialSub(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial): Boolean;
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

function Int64PolynomialMul(const Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial): Boolean;
var
  R: TCnInt64Polynomial;
  I, J, M: Integer;
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

  M := P1.MaxDegree + P2.MaxDegree;
  R.MaxDegree := M;

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

function Int64PolynomialDiv(const Res: TCnInt64Polynomial; const Remain: TCnInt64Polynomial;
  const P: TCnInt64Polynomial; const Divisor: TCnInt64Polynomial): Boolean;
var
  SubRes: TCnInt64Polynomial; // 容纳递减差
  MulRes: TCnInt64Polynomial; // 容纳除数乘积
  DivRes: TCnInt64Polynomial; // 容纳临时商
  I, D: Integer;
begin
  if Int64PolynomialIsZero(Divisor) then
    raise ECnPolynomialException.Create(SDivByZero);

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
        raise ECnPolynomialException.Create(SCnErrorDivExactly);

      Int64PolynomialCopy(MulRes, Divisor);
      Int64PolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次
      Int64PolynomialMulWord(MulRes, SubRes[P.MaxDegree - I] div MulRes[MulRes.MaxDegree]); // 除式乘到最高次系数相同
      DivRes[D - I] := SubRes[P.MaxDegree - I];                  // 商放到 DivRes 位置
      Int64PolynomialSub(SubRes, SubRes, MulRes);              // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      Int64PolynomialCopy(Remain, SubRes);
    if Res <> nil then
      Int64PolynomialCopy(Res, DivRes);
  finally
    FLocalInt64PolynomialPool.Recycle(SubRes);
    FLocalInt64PolynomialPool.Recycle(MulRes);
    FLocalInt64PolynomialPool.Recycle(DivRes);
  end;
  Result := True;
end;

function Int64PolynomialMod(const Res: TCnInt64Polynomial; const P: TCnInt64Polynomial;
  const Divisor: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialDiv(nil, Res, P, Divisor);
end;

function Int64PolynomialPower(const Res: TCnInt64Polynomial;
  const P: TCnInt64Polynomial; Exponent: LongWord): Boolean;
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
  end;

  T := Int64PolynomialDuplicate(P);
  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetCoefficents([1]);
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        Int64PolynomialMul(Res, Res, T);

      Exponent := Exponent shr 1;
      Int64PolynomialMul(T, T, T);
    end;
    Result := True;
  finally
    T.Free;
  end;
end;

function Int64PolynomialReduce(const P: TCnInt64Polynomial): Integer;
var
  I, D: Integer;

  function Gcd(A, B: Integer): Integer;
  var
    T: Integer;
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

function Int64PolynomialGreatestCommonDivisor(const Res: TCnInt64Polynomial;
  const P1, P2: TCnInt64Polynomial): Boolean;
var
  A, B, C: TCnInt64Polynomial;
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

    C := FLocalInt64PolynomialPool.Obtain;
    while not B.IsZero do
    begin
      Int64PolynomialCopy(C, B);        // 备份 B
      Int64PolynomialMod(B, A, B);      // A mod B 给 B
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

function Int64PolynomialCompose(const Res: TCnInt64Polynomial;
  const F, P: TCnInt64Polynomial): Boolean;
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
  Result := True;
end;

function Int64PolynomialCalcDivisionPolynomial(A, B: Integer; Degree: Integer;
  outDivisionPolynomial: TCnInt64Polynomial): Boolean;
var
  N: Integer;
  D1, D2, D3, Y: TCnInt64Polynomial;
begin
  Result := False;
  if Degree < 0 then
    Exit
  else if Degree = 0 then
  begin
    outDivisionPolynomial.SetCoefficents([0]);  // f0(X) = 0
    Result := True;
  end
  else if (Degree = 1) or (Degree = 2) then
  begin
    outDivisionPolynomial.SetCoefficents([1]);  // f1(X) = 1, f2(X) = 1,
    Result := True;
  end
  else if Degree = 3 then   // f3(X) = 3 X4 + 6 a X2 + 12 b X - a^2
  begin
    outDivisionPolynomial.SetCoefficents([- A * A,
      12 * B, 6 * A, 0, 3]);
    Result := True;
  end
  else if Degree = 4 then // f4(X) = 2 X6 + 10 a X4 + 40 b X3 - 10 a2X2 - 8 a b X - 2 a3 - 16 b^2
  begin
    outDivisionPolynomial.SetCoefficents([
      -2 * A * A * A - 16 * B * B,
      -8 * A * B, -10 * A * A,
      40 * B, 10 * A, 0, 2]);
    Result := True;
  end
  else
  begin
    D1 := nil;
    D2 := nil;
    D3 := nil;
    Y := nil;

    try
      // 开始递归计算
      N := Degree shr 1;
      if (Degree and 1) = 0 then // Degree 是偶数
      begin
        D1 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialCalcDivisionPolynomial(A, B, N + 2, D1);

        D2 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialCalcDivisionPolynomial(A, B, N - 1, D2);
        Int64PolynomialMul(D2, D2, D2);

        Int64PolynomialAdd(D1, D1, D2);   // D1 得到 Fn+2 * Fn-1 ^ 2

        D3 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialCalcDivisionPolynomial(A, B, N - 2, D3);

        Int64PolynomialCalcDivisionPolynomial(A, B, N + 1, D2);
        Int64PolynomialMul(D2, D2, D2);   // D2 得到 Fn-2 * Fn+1 ^ 2

        Int64PolynomialSub(D1, D1, D2);   // D1 得到 Fn+2 * Fn-1 ^ 2 - Fn-2 * Fn+1 ^ 2

        Int64PolynomialCalcDivisionPolynomial(A, B, N, D2);          // D2 得到 Fn
        Int64PolynomialCompose(outDivisionPolynomial, D2, D1); // 代入得到 F2n
      end
      else // Degree 是奇数
      begin
        Y := FLocalInt64PolynomialPool.Obtain;
        Y.SetCoefficents([4 * B, 4 * A, 0, 4]);
        Int64PolynomialMul(Y, Y, Y);

        if (N and 1) <> 0 then // N 是奇数
        begin
          D1 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialCalcDivisionPolynomial(A, B, N + 2, D1);

          D2 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialCalcDivisionPolynomial(A, B, N, D2);
          Int64PolynomialPower(D2, D2, 3);

          Int64PolynomialMul(D1, D1, D2);  // D1 得到 Fn+2 * Fn ^ 3

          D3 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialCalcDivisionPolynomial(A, B, N + 1, D3);
          Int64PolynomialPower(D3, D3, 3); // D3 得到 Fn+1 ^ 3

          Int64PolynomialCalcDivisionPolynomial(A, B, N - 1, D2);
          Int64PolynomialCompose(D2, D2, Y); // D2 得到 Fn-1(Y)

          Int64PolynomialMul(D2, D2, D3);    // D2 得到 Fn+1 ^ 3 * Fn-1(Y)
          Int64PolynomialSub(outDivisionPolynomial, D1, D2);
        end
        else // N 是偶数
        begin
          D1 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialCalcDivisionPolynomial(A, B, N + 2, D1);

          D2 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialCalcDivisionPolynomial(A, B, N, D2);
          Int64PolynomialPower(D2, D2, 3);

          Int64PolynomialMul(D1, D1, D2);
          Int64PolynomialMul(D1, D1, Y);   // D1 得到 Y * Fn+2 * Fn ^ 3

          D3 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialCalcDivisionPolynomial(A, B, N + 1, D3);
          Int64PolynomialPower(D3, D3, 3); // D3 得到 Fn+1 ^ 3

          Int64PolynomialCalcDivisionPolynomial(A, B, N - 1, D2);     // D2 得到 Fn-1

          Int64PolynomialMul(D2, D2, D3);  // D2 得到 Fn+1 ^ 3 * Fn-1

          Int64PolynomialSub(outDivisionPolynomial, D1, D2);
        end;
      end;
    finally
      FLocalInt64PolynomialPool.Recycle(D1);
      FLocalInt64PolynomialPool.Recycle(D2);
      FLocalInt64PolynomialPool.Recycle(D3);
      FLocalInt64PolynomialPool.Recycle(Y);
    end;
    Result := True;
  end;
end;

function Int64PolynomialGaloisAdd(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialAdd(Res, P1, P2);
  if Result then
  begin
    Int64PolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      Int64PolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function Int64PolynomialGaloisSub(const Res: TCnInt64Polynomial; const P1: TCnInt64Polynomial;
  const P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialSub(Res, P1, P2);
  if Result then
  begin
    Int64PolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      Int64PolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function Int64PolynomialGaloisMul(const Res: TCnInt64Polynomial; P1: TCnInt64Polynomial;
  P2: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
var
  R: TCnInt64Polynomial;
  I, J, M: Integer;
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

  M := P1.MaxDegree + P2.MaxDegree;
  R.MaxDegree := M;

  for I := 0 to P1.MaxDegree do
  begin
    // 把第 I 次方的数字乘以 P2 的每一个数字，加到结果的 I 开头的部分，再取模
    for J := 0 to P2.MaxDegree do
    begin
      R[I + J] := NonNegativeMod(R[I + J] + P1[I] * P2[J], Prime);
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

function Int64PolynomialGaloisDiv(const Res: TCnInt64Polynomial;
  const Remain: TCnInt64Polynomial; const P: TCnInt64Polynomial;
  const Divisor: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
var
  SubRes: TCnInt64Polynomial; // 容纳递减差
  MulRes: TCnInt64Polynomial; // 容纳除数乘积
  DivRes: TCnInt64Polynomial; // 容纳临时商
  I, D: Integer;
  K, T: Int64;
begin
  if Int64PolynomialIsZero(Divisor) then
    raise ECnPolynomialException.Create(SDivByZero);

  // 无需担心不能整除的问题，因为有逆元和 mod 操作

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
      K := CnInt64ModularInverse2(Divisor[Divisor.MaxDegree], Prime); // K 是除式最高位的逆元

    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then                 // 中间结果可能跳位
        Continue;
      Int64PolynomialCopy(MulRes, Divisor);
      Int64PolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次

      // 除式要乘一个数，这个数是 SubRes 最高位除以除式最高位得到的结果，也即 SubRes 最高位乘以除式最高位的逆元再 mod Prime
      T := NonNegativeMod(SubRes[P.MaxDegree - I] * K, Prime);
      Int64PolynomialGaloisMulWord(MulRes, T, Prime);          // 除式乘到最高次系数相同

      DivRes[D - I] := SubRes[P.MaxDegree - I];                  // 商放到 DivRes 位置
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

function Int64PolynomialGaloisMod(const Res: TCnInt64Polynomial; const P: TCnInt64Polynomial;
  const Divisor: TCnInt64Polynomial; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
begin
  Result := Int64PolynomialGaloisDiv(nil, Res, P, Divisor, Prime, Primitive);
end;

function Int64PolynomialGaloisPower(const Res, P: TCnInt64Polynomial;
  Exponent: LongWord; Prime: Int64; Primitive: TCnInt64Polynomial): Boolean;
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
  end;

  T := Int64PolynomialDuplicate(P);
  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetCoefficents([1]);
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        Int64PolynomialGaloisMul(Res, Res, T, Prime, Primitive);

      Exponent := Exponent shr 1;
      Int64PolynomialGaloisMul(T, T, T, Prime, Primitive);
    end;
    Result := True;
  finally
    T.Free;
  end;
end;

function Int64PolynomialGaloisAddWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
begin
  P[0] := NonNegativeMod(P[0] + N, Prime);
  Result := True;
end;

function Int64PolynomialGaloisSubWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
begin
  P[0] := NonNegativeMod(P[0] - N, Prime);
  Result := True;
end;

function Int64PolynomialGaloisMulWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
begin
  Int64PolynomialMulWord(P, N);
  Int64PolynomialNonNegativeModWord(P, Prime);
  Result := True;
end;

function Int64PolynomialGaloisDivWord(const P: TCnInt64Polynomial; N: Integer; Prime: Int64): Boolean;
var
  I: Integer;
  K: Int64;
  B: Boolean;
begin
  if N = 0 then
    raise ECnPolynomialException.Create(SDivByZero);

  B := N < 0;
  if B then
    N := -N;

  K := CnInt64ModularInverse2(N, Prime);
  for I := 0 to P.MaxDegree do
  begin
    P[I] := NonNegativeMod(P[I] * K, Prime);
    if B then
      P[I] := Prime - LongWord(P[I]);
  end;
  Result := True;
end;

function Int64PolynomialGaloisMonic(const P: TCnInt64Polynomial; Prime: Int64): Integer;
begin
  Result := P[P.MaxDegree];
  if (Result <> 1) and (Result <> 0) then
    Int64PolynomialGaloisDivWord(P, Result, Prime);
end;

function Int64PolynomialGaloisGreatestCommonDivisor(const Res: TCnInt64Polynomial;
  const P1, P2: TCnInt64Polynomial; Prime: Int64): Boolean;
var
  A, B, C: TCnInt64Polynomial;
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

    C := FLocalInt64PolynomialPool.Obtain;
    while not B.IsZero do
    begin
      Int64PolynomialCopy(C, B);          // 备份 B
      Int64PolynomialGaloisMod(B, A, B, Prime);  // A mod B 给 B
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

procedure Int64PolynomialGaloisModularInverse(const Res: TCnInt64Polynomial;
  X, Modulus: TCnInt64Polynomial; Prime: Int64);
var
  X1, Y: TCnInt64Polynomial;
begin
  X1 := nil;
  Y := nil;

  try
    X1 := FLocalInt64PolynomialPool.Obtain;
    Y := FLocalInt64PolynomialPool.Obtain;

    Int64PolynomialCopy(X1, X);

    // 扩展欧几里得辗转相除法求二元一次不定整系数多项式方程 A * X - B * Y = 1 的整数解
    Int64PolynomialGaloisExtendedEuclideanGcd(X1, Modulus, Res, Y, Prime);
  finally
    FLocalInt64PolynomialPool.Recycle(X1);
    FLocalInt64PolynomialPool.Recycle(Y);
  end;
end;

function Int64PolynomialGaloisCompose(const Res: TCnInt64Polynomial;
  const F, P: TCnInt64Polynomial; Prime: Int64): Boolean;
var
  I: Integer;
  R, X, T: TCnInt64Polynomial;
begin
  if P.IsZero or (F.MaxDegree = 0) then    // 0 代入，或只有常数项的情况下，得常数项
  begin
    Res.SetOne;
    Res[0] := NonNegativeMod(F[0], Prime);
    Result := True;
    Exit;
  end;

  if (Res = F) or (Res = P) then
    R := FLocalInt64PolynomialPool.Obtain
  else
    R := Res;

  X := FLocalInt64PolynomialPool.Obtain;
  T := FLocalInt64PolynomialPool.Obtain;
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

  if (Res = F) or (Res = P) then
  begin
    Int64PolynomialCopy(Res, R);
    FLocalInt64PolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function Int64PolynomialGaloisCalcDivisionPolynomial(A, B: Integer; Degree: Integer;
  outDivisionPolynomial: TCnInt64Polynomial; Prime: Int64): Boolean;
var
  N: Integer;
  D1, D2, D3, Y: TCnInt64Polynomial;
begin
  Result := False;
  if Degree < 0 then
    Exit
  else if Degree = 0 then
  begin
    outDivisionPolynomial.SetCoefficents([0]);  // f0(X) = 0
    Result := True;
  end
  else if (Degree = 1) or (Degree = 2) then
  begin
    outDivisionPolynomial.SetCoefficents([1]);  // f1(X) = 1, f2(X) = 1,
    Result := True;
  end
  else if Degree = 3 then   // f3(X) = 3 X4 + 6 a X2 + 12 b X - a^2
  begin
    outDivisionPolynomial.SetCoefficents([- A * A,
      12 * B, 6 * A, 0, 3]);
    Int64PolynomialNonNegativeModWord(outDivisionPolynomial, Prime);
    Result := True;
  end
  else if Degree = 4 then // f4(X) = 2 X6 + 10 a X4 + 40 b X3 - 10 a2X2 - 8 a b X - 2 a3 - 16 b^2
  begin
    outDivisionPolynomial.SetCoefficents([-2 * A * A * A - 16 * B * B,
      -8 * A * B, -10 * A * A, 40 * B, 10 * A, 0, 2]);
    Int64PolynomialNonNegativeModWord(outDivisionPolynomial, Prime);
    Result := True;
  end
  else
  begin
    D1 := nil;
    D2 := nil;
    D3 := nil;
    Y := nil;

    try
      // 开始递归计算
      N := Degree shr 1;
      if (Degree and 1) = 0 then // Degree 是偶数
      begin
        D1 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 2, D1, Prime);

        D2 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);
        Int64PolynomialGaloisMul(D2, D2, D2, Prime);

        Int64PolynomialGaloisAdd(D1, D1, D2, Prime);   // D1 得到 Fn+2 * Fn-1 ^ 2

        D3 := FLocalInt64PolynomialPool.Obtain;
        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 2, D3, Prime);

        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 1, D2, Prime);
        Int64PolynomialGaloisMul(D2, D2, D2, Prime);   // D2 得到 Fn-2 * Fn+1 ^ 2

        Int64PolynomialGaloisSub(D1, D1, D2, Prime);   // D1 得到 Fn+2 * Fn-1 ^ 2 - Fn-2 * Fn+1 ^ 2

        Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N, D2, Prime);          // D2 得到 Fn
        Int64PolynomialCompose(outDivisionPolynomial, D2, D1); // 代入得到 F2n
      end
      else // Degree 是奇数
      begin
        Y := FLocalInt64PolynomialPool.Obtain;
        Y.SetCoefficents([4 * B, 4 * A, 0, 4]);
        Int64PolynomialGaloisMul(Y, Y, Y, Prime);

        if (N and 1) <> 0 then // N 是奇数
        begin
          D1 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 2, D1, Prime);

          D2 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N, D2, Prime);
          Int64PolynomialGaloisPower(D2, D2, 3, Prime);

          Int64PolynomialGaloisMul(D1, D1, D2, Prime);  // D1 得到 Fn+2 * Fn ^ 3

          D3 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 1, D3, Prime);
          Int64PolynomialGaloisPower(D3, D3, 3, Prime); // D3 得到 Fn+1 ^ 3

          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);
          Int64PolynomialGaloisCompose(D2, D2, Y, Prime); // D2 得到 Fn-1(Y)

          Int64PolynomialGaloisMul(D2, D2, D3, Prime);    // D2 得到 Fn+1 ^ 3 * Fn-1(Y)
          Int64PolynomialGaloisSub(outDivisionPolynomial, D1, D2, Prime);
        end
        else // N 是偶数
        begin
          D1 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 2, D1, Prime);

          D2 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N, D2, Prime);
          Int64PolynomialGaloisPower(D2, D2, 3, Prime);

          Int64PolynomialGaloisMul(D1, D1, D2, Prime);
          Int64PolynomialGaloisMul(D1, D1, Y, Prime);   // D1 得到 Y * Fn+2 * Fn ^ 3

          D3 := FLocalInt64PolynomialPool.Obtain;
          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N + 1, D3, Prime);
          Int64PolynomialGaloisPower(D3, D3, 3, Prime); // D3 得到 Fn+1 ^ 3

          Int64PolynomialGaloisCalcDivisionPolynomial(A, B, N - 1, D2, Prime);     // D2 得到 Fn-1

          Int64PolynomialGaloisMul(D2, D2, D3, Prime);  // D2 得到 Fn+1 ^ 3 * Fn-1

          Int64PolynomialGaloisSub(outDivisionPolynomial, D1, D2, Prime);
        end;
      end;
    finally
      FLocalInt64PolynomialPool.Recycle(D1);
      FLocalInt64PolynomialPool.Recycle(D2);
      FLocalInt64PolynomialPool.Recycle(D3);
      FLocalInt64PolynomialPool.Recycle(Y);
    end;
    Result := True;
  end;
end;

{ TCnInt64PolynomialPool }

constructor TCnInt64PolynomialPool.Create;
begin
  inherited Create(False);
{$IFDEF MULTI_THREAD}
{$IFDEF MSWINDOWS}
  InitializeCriticalSection(FCriticalSection);
{$ELSE}
  FCriticalSection := TCriticalSection.Create;
{$ENDIF}
{$ENDIF}
end;

destructor TCnInt64PolynomialPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;

{$IFDEF MULTI_THREAD}
{$IFDEF MSWINDOWS}
  DeleteCriticalSection(FCriticalSection);
{$ELSE}
  FCriticalSection.Free;
{$ENDIF}
{$ENDIF}
end;

procedure TCnInt64PolynomialPool.Enter;
begin
{$IFDEF MULTI_THREAD}
{$IFDEF MSWINDOWS}
  EnterCriticalSection(FCriticalSection);
{$ELSE}
  FCriticalSection.Acquire;
{$ENDIF}
{$ENDIF}
end;

procedure TCnInt64PolynomialPool.Leave;
begin
{$IFDEF MULTI_THREAD}
{$IFDEF MSWINDOWS}
  LeaveCriticalSection(FCriticalSection);
{$ELSE}
  FCriticalSection.Release;
{$ENDIF}
{$ENDIF}
end;

function TCnInt64PolynomialPool.Obtain: TCnInt64Polynomial;
begin
  Enter;
  if Count = 0 then
  begin
    Result := TCnInt64Polynomial.Create;
  end
  else
  begin
    Result := TCnInt64Polynomial(Items[Count - 1]);
    Delete(Count - 1);
  end;
  Leave;

  Result.SetZero;
end;

procedure TCnInt64PolynomialPool.Recycle(Poly: TCnInt64Polynomial);
begin
  if Poly <> nil then
  begin
    Enter;
    Add(Poly);
    Leave;
  end;
end;

initialization
  FLocalInt64PolynomialPool := TCnInt64PolynomialPool.Create;

finalization
  FLocalInt64PolynomialPool.Free;

end.
