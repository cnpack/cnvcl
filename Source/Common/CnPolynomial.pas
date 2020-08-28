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

  TCnIntegerList = class(TList)
  {* 整数列表}
  private
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const Value: Integer);
  public
    function Add(Item: Integer): Integer; reintroduce;
    procedure Insert(Index: Integer; Item: Integer); reintroduce;
    property Items[Index: Integer]: Integer read Get write Put; default;
  end;

  TCnIntegerPolynomial = class(TCnIntegerList)
  {* 整系数多项式}
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
    procedure SetOne;
    {* 设为 1}
    property MaxDegree: Integer read GetMaxDegree write SetMaxDegree;
    {* 最高次数，0 开始}
  end;

  TCnIntegerPolynomialPool = class(TObjectList)
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

    function Obtain: TCnIntegerPolynomial;
    procedure Recycle(Poly: TCnIntegerPolynomial);
  end;

function IntegerPolynomialNew: TCnIntegerPolynomial;
{* 创建一个动态分配的整系数多项式对象，等同于 TCnIntegerPolynomial.Create}

procedure IntegerPolynomialFree(const P: TCnIntegerPolynomial);
{* 释放一个整系数多项式对象，等同于 TCnIntegerPolynomial.Free}

function IntegerPolynomialDuplicate(const P: TCnIntegerPolynomial): TCnIntegerPolynomial;
{* 从一个整系数多项式对象克隆一个新对象}

function IntegerPolynomialCopy(const Dst: TCnIntegerPolynomial;
  const Src: TCnIntegerPolynomial): TCnIntegerPolynomial;
{* 复制一个整系数多项式对象，成功返回 Dst}

function IntegerPolynomialToString(const P: TCnIntegerPolynomial;
  const VarName: string = 'X'): string;
{* 将一个整系数多项式对象转成字符串，未知数默认以 X 表示}

function IntegerPolynomialIsZero(const P: TCnIntegerPolynomial): Boolean;
{* 判断一个整系数多项式对象是否为 0}

procedure IntegerPolynomialSetZero(const P: TCnIntegerPolynomial);
{* 将一个整系数多项式对象设为 0}

procedure IntegerPolynomialSetOne(const P: TCnIntegerPolynomial);
{* 将一个整系数多项式对象设为 1}

procedure IntegerPolynomialShiftLeft(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象左移 N 次，也就是各项指数都加 N}

procedure IntegerPolynomialShiftRight(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象右移 N 次，也就是各项指数都减 N，小于 0 的忽略了}

// =========================== 多项式普通运算 ==================================

procedure IntegerPolynomialAddWord(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象的常系数加上 N}

procedure IntegerPolynomialSubWord(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象的常系数减去 N}

procedure IntegerPolynomialMulWord(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象的各个系数都乘以 N}

procedure IntegerPolynomialDivWord(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象的各个系数都除以 N，如不能整除则取整}

procedure IntegerPolynomialNonNegativeModWord(const P: TCnIntegerPolynomial; N: LongWord);
{* 将一个整系数多项式对象的各个系数都对 N 非负求余}

function IntegerPolynomialAdd(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialSub(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相减，结果放至 Res 中，返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialMul(const Res: TCnIntegerPolynomial; P1: TCnIntegerPolynomial;
  P2: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialDiv(const Res: TCnIntegerPolynomial; const Remain: TCnIntegerPolynomial;
  const P: TCnIntegerPolynomial; const Divisor: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   注意当商式或余式出现无法整除的分数时会抛出异常，无法支持，
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor}

function IntegerPolynomialMod(const Res: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象求余，余数放至 Res 中，返回求余是否成功，
   注意当商式或余式出现无法整除的分数时会抛出异常，无法支持，
   Res 可以是 P 或 Divisor，P 可以是 Divisor}

function IntegerPolynomialPower(const Res: TCnIntegerPolynomial;
  const P: TCnIntegerPolynomial;  Exponent: LongWord): Boolean;
{* 计算整系数多项式的 Exponent 次幂，不考虑系数溢出的问题，
   返回计算是否成功，Res 可以是 P}

function IntegerPolynomialReduce(const P: TCnIntegerPolynomial): Integer;
{* 化简多项式系数，也就是找多项式系数的最大公约数，各个系数除以它，返回最大公约数}

function IntegerPolynomialGreatestCommonDivisor(const Res: TCnIntegerPolynomial;
  const P1, P2: TCnIntegerPolynomial): Boolean;
{* 计算两个整系数多项式的最大公因式，返回计算是否成功，Res 可以是 P1 或 P2
   注意计算可能会因为系数不能整除而失败，即使调用者自行保证 P1 P2 均为首一多项式也不能保证行}

// ===================== 有限扩域下的整系数多项式模运算 ========================

function IntegerPolynomialGaloisAdd(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式
   返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialGaloisSub(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相加，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且 Res 次数低于本原多项式
   返回相减是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialGaloisMul(const Res: TCnIntegerPolynomial; P1: TCnIntegerPolynomial;
  P2: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相乘，结果放至 Res 中，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialGaloisDiv(const Res: TCnIntegerPolynomial;
  const Remain: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor}

function IntegerPolynomialGaloisMod(const Res: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial = nil): Boolean;
{* 两个整系数多项式对象在 Prime 次方阶有限域上求余，余数放至 Res 中，返回求余是否成功，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   Res 可以是 P 或 Divisor，P 可以是 Divisor}

function IntegerPolynomialGaloisPower(const Res, P: TCnIntegerPolynomial;
  Exponent, Prime: LongWord; Primitive: TCnIntegerPolynomial): Boolean;
{* 计算整系数多项式在 Prime 次方阶有限域上的 Exponent 次幂，
   调用者需自行保证 Prime 是素数且本原多项式 Primitive 为不可约多项式
   返回计算是否成功，Res 可以是 P}

function IntegerPolynomialGaloisMulWord(const P: TCnIntegerPolynomial; N: Integer; Prime: LongWord): Boolean;
{* 将 Prime 次方阶有限域上的整系数多项式各项系数乘以 N 再 mod Prime}

function IntegerPolynomialGaloisDivWord(const P: TCnIntegerPolynomial; N: Integer; Prime: LongWord): Boolean;
{* 将 Prime 次方阶有限域上的整系数多项式各项系数除以 N，也就是乘以 N 的逆元再 mod Prime}

function IntegerPolynomialGaloisMonic(const P: TCnIntegerPolynomial; Prime: LongWord): Integer;
{* 将 Prime 次方阶有限域上的整系数多项式各项系数同除最高项，使首项为一，返回除的值}

function IntegerPolynomialGaloisGreatestCommonDivisor(const Res: TCnIntegerPolynomial;
  const P1, P2: TCnIntegerPolynomial; Prime: LongWord): Boolean;
{* 计算两个整系数多项式在 Prime 次方阶有限域上的最大公因式，返回计算是否成功，Res 可以是 P1 或 P2}

procedure IntegerPolynomialGaloisExtendedEuclideanGcd(A, B: TCnIntegerPolynomial;
  X, Y: TCnIntegerPolynomial; Prime: LongWord);
{* 扩展欧几里得辗转相除法在 Prime 次方阶有限域上求二元一次不定整系数多项式方程 A * X - B * Y = 1 的解}

procedure IntegerPolynomialGaloisModularInverse(const Res: TCnIntegerPolynomial;
  X, Modulus: TCnIntegerPolynomial; Prime: LongWord);
{* 求整系数多项式 X 在 Prime 次方阶有限域上针对 Modulus 的模反多项式或叫模逆元多项式 Y，
   满足 (X * Y) mod M = 1，调用者须自行保证 X、Modulus 互素}

implementation

resourcestring
  SCnInvalidDegree = 'Invalid Degree %d';
  SCnErrorDivMaxDegree = 'Only MaxDegree 1 Support for Integer Polynomial.';
  SCnErrorDivExactly = 'Can NOT Divide Exactly for Integer Polynomial.';

var
  FLocalIntegerPolynomialPool: TCnIntegerPolynomialPool = nil;

// 封装的非负求余函数，也就是余数为负时，加个除数变正
function NonNegativeMod(N: Integer; P: LongWord): Integer;
begin
  Result := N mod P;
  if N < 0 then
    Inc(Result, P);
end;

{ TCnIntegerList }

function TCnIntegerList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(IntegerToPointer(Item));
end;

function TCnIntegerList.Get(Index: Integer): Integer;
begin
  Result := PointerToInteger(inherited Get(Index));
end;

procedure TCnIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, IntegerToPointer(Item));
end;

procedure TCnIntegerList.Put(Index: Integer; const Value: Integer);
begin
  inherited Put(Index, IntegerToPointer(Value));
end;

{ TCnIntegerPolynomial }

procedure TCnIntegerPolynomial.CorrectTop;
begin
  while (MaxDegree > 0) and (Items[MaxDegree] = 0) do
    Delete(MaxDegree);
end;

constructor TCnIntegerPolynomial.Create;
begin
  inherited;
  Add(0);   // 常系数项
end;

constructor TCnIntegerPolynomial.Create(LowToHighCoefficients: array of const);
begin
  inherited Create;
  SetCoefficents(LowToHighCoefficients);
end;

destructor TCnIntegerPolynomial.Destroy;
begin

  inherited;
end;

function TCnIntegerPolynomial.GetMaxDegree: Integer;
begin
  if Count = 0 then
    Add(0);
  Result := Count - 1;
end;

function TCnIntegerPolynomial.IsZero: Boolean;
begin
  Result := IntegerPolynomialIsZero(Self);
end;

procedure TCnIntegerPolynomial.SetCoefficents(LowToHighCoefficients: array of const);
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

procedure TCnIntegerPolynomial.SetMaxDegree(const Value: Integer);
begin
  if Value < 0 then
    raise ECnPolynomialException.CreateFmt(SCnInvalidDegree, [Value]);
  Count := Value + 1;
end;

procedure TCnIntegerPolynomial.SetOne;
begin
  IntegerPolynomialSetOne(Self);
end;

procedure TCnIntegerPolynomial.SetZero;
begin
  IntegerPolynomialSetZero(Self);
end;

function TCnIntegerPolynomial.ToString: string;
begin
  Result := IntegerPolynomialToString(Self);
end;

// ============================ 多项式系列操作函数 =============================

function IntegerPolynomialNew: TCnIntegerPolynomial;
begin
  Result := TCnIntegerPolynomial.Create;
end;

procedure IntegerPolynomialFree(const P: TCnIntegerPolynomial);
begin
  P.Free;
end;

function IntegerPolynomialDuplicate(const P: TCnIntegerPolynomial): TCnIntegerPolynomial;
begin
  if P = nil then
  begin
    Result := nil;
    Exit;
  end;

  Result := IntegerPolynomialNew;
  if Result <> nil then
    IntegerPolynomialCopy(Result, P);
end;

function IntegerPolynomialCopy(const Dst: TCnIntegerPolynomial;
  const Src: TCnIntegerPolynomial): TCnIntegerPolynomial;
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

function IntegerPolynomialToString(const P: TCnIntegerPolynomial;
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
  if IntegerPolynomialIsZero(P) then
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

function IntegerPolynomialIsZero(const P: TCnIntegerPolynomial): Boolean;
begin
  Result := (P.MaxDegree = 0) and (P[0] = 0);
end;

procedure IntegerPolynomialSetZero(const P: TCnIntegerPolynomial);
begin
  P.Clear;
  P.Add(0);
end;

procedure IntegerPolynomialSetOne(const P: TCnIntegerPolynomial);
begin
  P.Clear;
  P.Add(1);
end;

procedure IntegerPolynomialShiftLeft(const P: TCnIntegerPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    IntegerPolynomialShiftRight(P, -N)
  else
  begin
    for I := 1 to N do
      P.Insert(0, 0);
  end;
end;

procedure IntegerPolynomialShiftRight(const P: TCnIntegerPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    Exit
  else if N < 0 then
    IntegerPolynomialShiftLeft(P, -N)
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

procedure IntegerPolynomialAddWord(const P: TCnIntegerPolynomial; N: Integer);
begin
  P[0] := P[0] + N;
end;

procedure IntegerPolynomialSubWord(const P: TCnIntegerPolynomial; N: Integer);
begin
  P[0] := P[0] - N;
end;

procedure IntegerPolynomialMulWord(const P: TCnIntegerPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
  begin
    IntegerPolynomialSetZero(P);
    Exit;
  end
  else
  begin
    for I := 0 to P.MaxDegree do
      P[I] := P[I] * N;
  end;
end;

procedure IntegerPolynomialDivWord(const P: TCnIntegerPolynomial; N: Integer);
var
  I: Integer;
begin
  if N = 0 then
    raise ECnPolynomialException.Create(SZeroDivide);

  for I := 0 to P.MaxDegree do
    P[I] := P[I] div N;
end;

procedure IntegerPolynomialNonNegativeModWord(const P: TCnIntegerPolynomial; N: LongWord);
var
  I: Integer;
begin
  if N = 0 then
    raise ECnPolynomialException.Create(SZeroDivide);

  for I := 0 to P.MaxDegree do
    P[I] := NonNegativeMod(P[I], N);
end;

function IntegerPolynomialAdd(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial): Boolean;
var
  I, D1, D2: Integer;
  PBig: TCnIntegerPolynomial;
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

function IntegerPolynomialSub(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial): Boolean;
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

function IntegerPolynomialMul(const Res: TCnIntegerPolynomial; P1: TCnIntegerPolynomial;
  P2: TCnIntegerPolynomial): Boolean;
var
  R: TCnIntegerPolynomial;
  I, J, M: Integer;
begin
  if IntegerPolynomialIsZero(P1) or IntegerPolynomialIsZero(P2) then
  begin
    IntegerPolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalIntegerPolynomialPool.Obtain
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
    IntegerPolynomialCopy(Res, R);
    FLocalIntegerPolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function IntegerPolynomialDiv(const Res: TCnIntegerPolynomial; const Remain: TCnIntegerPolynomial;
  const P: TCnIntegerPolynomial; const Divisor: TCnIntegerPolynomial): Boolean;
var
  SubRes: TCnIntegerPolynomial; // 容纳递减差
  MulRes: TCnIntegerPolynomial; // 容纳除数乘积
  DivRes: TCnIntegerPolynomial; // 容纳临时商
  I, D: Integer;
begin
  if IntegerPolynomialIsZero(Divisor) then
    raise ECnPolynomialException.Create(SDivByZero);

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      IntegerPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      IntegerPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;

  try
    SubRes := FLocalIntegerPolynomialPool.Obtain;
    IntegerPolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := FLocalIntegerPolynomialPool.Obtain;
    DivRes.MaxDegree := D;
    MulRes := FLocalIntegerPolynomialPool.Obtain;

    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then                 // 中间结果可能跳位
        Continue;

      // 判断 Divisor[Divisor.MaxDegree] 是否能整除 SubRes[P.MaxDegree - I] 不能则说明超出了整型多项式范围，无法支持，只能出错
      if (SubRes[P.MaxDegree - I] mod Divisor[Divisor.MaxDegree]) <> 0 then
        raise ECnPolynomialException.Create(SCnErrorDivExactly);

      IntegerPolynomialCopy(MulRes, Divisor);
      IntegerPolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次
      IntegerPolynomialMulWord(MulRes, SubRes[P.MaxDegree - I] div MulRes[MulRes.MaxDegree]); // 除式乘到最高次系数相同
      DivRes[D - I] := SubRes[P.MaxDegree - I];                  // 商放到 DivRes 位置
      IntegerPolynomialSub(SubRes, SubRes, MulRes);              // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      IntegerPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      IntegerPolynomialCopy(Res, DivRes);
  finally
    FLocalIntegerPolynomialPool.Recycle(SubRes);
    FLocalIntegerPolynomialPool.Recycle(MulRes);
    FLocalIntegerPolynomialPool.Recycle(DivRes);
  end;
  Result := True;
end;

function IntegerPolynomialMod(const Res: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial): Boolean;
begin
  Result := IntegerPolynomialDiv(nil, Res, P, Divisor);
end;

function IntegerPolynomialPower(const Res: TCnIntegerPolynomial;
  const P: TCnIntegerPolynomial; Exponent: LongWord): Boolean;
var
  T: TCnIntegerPolynomial;
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
      IntegerPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end;

  T := IntegerPolynomialDuplicate(P);
  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetCoefficents([1]);
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        IntegerPolynomialMul(Res, Res, T);

      Exponent := Exponent shr 1;
      IntegerPolynomialMul(T, T, T);
    end;
    Result := True;
  finally
    T.Free;
  end;
end;

function IntegerPolynomialReduce(const P: TCnIntegerPolynomial): Integer;
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
      IntegerPolynomialDivWord(P, Result);
  end;
end;

function IntegerPolynomialGreatestCommonDivisor(const Res: TCnIntegerPolynomial;
  const P1, P2: TCnIntegerPolynomial): Boolean;
var
  A, B, C: TCnIntegerPolynomial;
begin
  A := nil;
  B := nil;
  C := nil;
  try
    A := FLocalIntegerPolynomialPool.Obtain;
    B := FLocalIntegerPolynomialPool.Obtain;

    if P1.MaxDegree >= P2.MaxDegree then
    begin
      IntegerPolynomialCopy(A, P1);
      IntegerPolynomialCopy(B, P2);
    end
    else
    begin
      IntegerPolynomialCopy(A, P2);
      IntegerPolynomialCopy(B, P1);
    end;

    C := FLocalIntegerPolynomialPool.Obtain;
    while not B.IsZero do
    begin
      IntegerPolynomialCopy(C, B);        // 备份 B
      IntegerPolynomialMod(B, A, B);      // A mod B 给 B
      // B 要系数约分化简
      IntegerPolynomialReduce(B);
      IntegerPolynomialCopy(A, C);        // 原始 B 给 A
    end;

    IntegerPolynomialCopy(Res, A);
    Result := True;
  finally
    FLocalIntegerPolynomialPool.Recycle(A);
    FLocalIntegerPolynomialPool.Recycle(B);
    FLocalIntegerPolynomialPool.Recycle(C);
  end;
end;

function IntegerPolynomialGaloisAdd(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial): Boolean;
begin
  Result := IntegerPolynomialAdd(Res, P1, P2);
  if Result then
  begin
    IntegerPolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      IntegerPolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function IntegerPolynomialGaloisSub(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial): Boolean;
begin
  Result := IntegerPolynomialSub(Res, P1, P2);
  if Result then
  begin
    IntegerPolynomialNonNegativeModWord(Res, Prime);
    if Primitive <> nil then
      IntegerPolynomialGaloisMod(Res, Res, Primitive, Prime);
  end;
end;

function IntegerPolynomialGaloisMul(const Res: TCnIntegerPolynomial; P1: TCnIntegerPolynomial;
  P2: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial): Boolean;
var
  R: TCnIntegerPolynomial;
  I, J, M: Integer;
begin
  if IntegerPolynomialIsZero(P1) or IntegerPolynomialIsZero(P2) then
  begin
    IntegerPolynomialSetZero(Res);
    Result := True;
    Exit;
  end;

  if (Res = P1) or (Res = P2) then
    R := FLocalIntegerPolynomialPool.Obtain
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
    IntegerPolynomialGaloisMod(R, R, Primitive, Prime);

  if (Res = P1) or (Res = P2) then
  begin
    IntegerPolynomialCopy(Res, R);
    FLocalIntegerPolynomialPool.Recycle(R);
  end;
  Result := True;
end;

function IntegerPolynomialGaloisDiv(const Res: TCnIntegerPolynomial;
  const Remain: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial): Boolean;
var
  SubRes: TCnIntegerPolynomial; // 容纳递减差
  MulRes: TCnIntegerPolynomial; // 容纳除数乘积
  DivRes: TCnIntegerPolynomial; // 容纳临时商
  I, D, K, T: Integer;
begin
  if IntegerPolynomialIsZero(Divisor) then
    raise ECnPolynomialException.Create(SDivByZero);

  // 无需担心不能整除的问题，因为有逆元和 mod 操作

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      IntegerPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      IntegerPolynomialCopy(Remain, P);
    Result := True;
    Exit;
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;

  try
    SubRes := FLocalIntegerPolynomialPool.Obtain;
    IntegerPolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := FLocalIntegerPolynomialPool.Obtain;
    DivRes.MaxDegree := D;
    MulRes := FLocalIntegerPolynomialPool.Obtain;

    if Divisor[Divisor.MaxDegree] = 1 then
      K := 1
    else
      K := CnInt64ModularInverse(Divisor[Divisor.MaxDegree], Prime); // K 是除式最高位的逆元

    for I := 0 to D do
    begin
      if P.MaxDegree - I > SubRes.MaxDegree then                 // 中间结果可能跳位
        Continue;
      IntegerPolynomialCopy(MulRes, Divisor);
      IntegerPolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次

      // 除式要乘一个数，这个数是 SubRes 最高位除以除式最高位得到的结果，也即 SubRes 最高位乘以除式最高位的逆元再 mod Prime
      T := NonNegativeMod(SubRes[P.MaxDegree - I] * K, Prime);
      IntegerPolynomialGaloisMulWord(MulRes, T, Prime);          // 除式乘到最高次系数相同

      DivRes[D - I] := SubRes[P.MaxDegree - I];                  // 商放到 DivRes 位置
      IntegerPolynomialGaloisSub(SubRes, SubRes, MulRes, Prime); // 减求模后结果重新放回 SubRes
    end;

    // 商与余式都需要再模本原多项式
    if Primitive <> nil then
    begin
      IntegerPolynomialGaloisMod(SubRes, SubRes, Primitive, Prime);
      IntegerPolynomialGaloisMod(DivRes, DivRes, Primitive, Prime);
    end;

    if Remain <> nil then
      IntegerPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      IntegerPolynomialCopy(Res, DivRes);
    Result := True;
  finally
    FLocalIntegerPolynomialPool.Recycle(SubRes);
    FLocalIntegerPolynomialPool.Recycle(MulRes);
    FLocalIntegerPolynomialPool.Recycle(DivRes);
  end;
end;

function IntegerPolynomialGaloisMod(const Res: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial; Prime: LongWord; Primitive: TCnIntegerPolynomial): Boolean;
begin
  Result := IntegerPolynomialGaloisDiv(nil, Res, P, Divisor, Prime, Primitive);
end;

function IntegerPolynomialGaloisPower(const Res, P: TCnIntegerPolynomial;
  Exponent, Prime: LongWord; Primitive: TCnIntegerPolynomial): Boolean;
var
  T: TCnIntegerPolynomial;
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
      IntegerPolynomialCopy(Res, P);
    Result := True;
    Exit;
  end;

  T := IntegerPolynomialDuplicate(P);
  try
    // 二进制形式快速计算 T 的次方，值给 Res
    Res.SetCoefficents([1]);
    while Exponent > 0 do
    begin
      if (Exponent and 1) <> 0 then
        IntegerPolynomialGaloisMul(Res, Res, T, Prime, Primitive);

      Exponent := Exponent shr 1;
      IntegerPolynomialGaloisMul(T, T, T, Prime, Primitive);
    end;
    Result := True;
  finally
    T.Free;
  end;
end;

function IntegerPolynomialGaloisMulWord(const P: TCnIntegerPolynomial; N: Integer; Prime: LongWord): Boolean;
begin
  IntegerPolynomialMulWord(P, N);
  IntegerPolynomialNonNegativeModWord(P, Prime);
  Result := True;
end;

function IntegerPolynomialGaloisDivWord(const P: TCnIntegerPolynomial; N: Integer; Prime: LongWord): Boolean;
var
  I, K: Integer;
  B: Boolean;
begin
  if N = 0 then
    raise ECnPolynomialException.Create(SDivByZero);

  B := N < 0;
  if B then
    N := -N;

  K := CnInt64ModularInverse(N, Prime); 
  for I := 0 to P.MaxDegree do
  begin
    P[I] := NonNegativeMod(P[I] * K, Prime);
    if B then
      P[I] := Prime - LongWord(P[I]);
  end;
  Result := True;
end;

function IntegerPolynomialGaloisMonic(const P: TCnIntegerPolynomial; Prime: LongWord): Integer;
begin
  Result := P[P.MaxDegree];
  if (Result <> 1) and (Result <> 0) then
    IntegerPolynomialGaloisDivWord(P, Result, Prime);
end;

function IntegerPolynomialGaloisGreatestCommonDivisor(const Res: TCnIntegerPolynomial;
  const P1, P2: TCnIntegerPolynomial; Prime: LongWord): Boolean;
var
  A, B, C: TCnIntegerPolynomial;
begin
  A := nil;
  B := nil;
  C := nil;
  try
    A := FLocalIntegerPolynomialPool.Obtain;
    B := FLocalIntegerPolynomialPool.Obtain;

    if P1.MaxDegree >= P2.MaxDegree then
    begin
      IntegerPolynomialCopy(A, P1);
      IntegerPolynomialCopy(B, P2);
    end
    else
    begin
      IntegerPolynomialCopy(A, P2);
      IntegerPolynomialCopy(B, P1);
    end;

    C := FLocalIntegerPolynomialPool.Obtain;
    while not B.IsZero do
    begin
      IntegerPolynomialCopy(C, B);          // 备份 B
      IntegerPolynomialGaloisMod(B, A, B, Prime);  // A mod B 给 B
      IntegerPolynomialCopy(A, C);          // 原始 B 给 A
    end;

    IntegerPolynomialCopy(Res, A);
    IntegerPolynomialGaloisMonic(Res, Prime);      // 首项化为一
    Result := True;
  finally
    FLocalIntegerPolynomialPool.Recycle(A);
    FLocalIntegerPolynomialPool.Recycle(B);
    FLocalIntegerPolynomialPool.Recycle(C);
  end;
end;

procedure IntegerPolynomialGaloisExtendedEuclideanGcd(A, B: TCnIntegerPolynomial;
  X, Y: TCnIntegerPolynomial; Prime: LongWord);
var
  T, P, M: TCnIntegerPolynomial;
begin
  if B.IsZero then
  begin
    X.SetZero;
    X[0] := CnInt64ModularInverse(A[0], Prime);
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
      T := FLocalIntegerPolynomialPool.Obtain;
      P := FLocalIntegerPolynomialPool.Obtain;
      M := FLocalIntegerPolynomialPool.Obtain;

      IntegerPolynomialGaloisMod(P, A, B, Prime);

      IntegerPolynomialGaloisExtendedEuclideanGcd(B, P, Y, X, Prime);

      // Y := Y - (A div B) * X;
      IntegerPolynomialGaloisDiv(P, M, A, B, Prime);
      IntegerPolynomialGaloisMul(P, P, X, Prime);
      IntegerPolynomialGaloisSub(Y, Y, P, Prime);
    finally
      FLocalIntegerPolynomialPool.Recycle(M);
      FLocalIntegerPolynomialPool.Recycle(P);
      FLocalIntegerPolynomialPool.Recycle(T);
    end;
  end;
end;

procedure IntegerPolynomialGaloisModularInverse(const Res: TCnIntegerPolynomial;
  X, Modulus: TCnIntegerPolynomial; Prime: LongWord);
var
  X1, Y: TCnIntegerPolynomial;
begin
  X1 := nil;
  Y := nil;

  try
    X1 := FLocalIntegerPolynomialPool.Obtain;
    Y := FLocalIntegerPolynomialPool.Obtain;

    IntegerPolynomialCopy(X1, X);

    // 扩展欧几里得辗转相除法求二元一次不定整系数多项式方程 A * X - B * Y = 1 的整数解
    IntegerPolynomialGaloisExtendedEuclideanGcd(X1, Modulus, Res, Y, Prime);
  finally
    FLocalIntegerPolynomialPool.Recycle(X1);
    FLocalIntegerPolynomialPool.Recycle(Y);
  end;
end;

{ TCnIntegerPolynomialPool }

constructor TCnIntegerPolynomialPool.Create;
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

destructor TCnIntegerPolynomialPool.Destroy;
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

procedure TCnIntegerPolynomialPool.Enter;
begin
{$IFDEF MULTI_THREAD}
{$IFDEF MSWINDOWS}
  EnterCriticalSection(FCriticalSection);
{$ELSE}
  FCriticalSection.Acquire;
{$ENDIF}
{$ENDIF}
end;

procedure TCnIntegerPolynomialPool.Leave;
begin
{$IFDEF MULTI_THREAD}
{$IFDEF MSWINDOWS}
  LeaveCriticalSection(FCriticalSection);
{$ELSE}
  FCriticalSection.Release;
{$ENDIF}
{$ENDIF}
end;

function TCnIntegerPolynomialPool.Obtain: TCnIntegerPolynomial;
begin
  Enter;
  if Count = 0 then
  begin
    Result := TCnIntegerPolynomial.Create;
  end
  else
  begin
    Result := TCnIntegerPolynomial(Items[Count - 1]);
    Delete(Count - 1);
  end;
  Leave;

  Result.SetZero;
end;

procedure TCnIntegerPolynomialPool.Recycle(Poly: TCnIntegerPolynomial);
begin
  if Poly <> nil then
  begin
    Enter;
    Add(Poly);
    Leave;
  end;
end;

initialization
  FLocalIntegerPolynomialPool := TCnIntegerPolynomialPool.Create;

finalization
  FLocalIntegerPolynomialPool.Free;

end.
