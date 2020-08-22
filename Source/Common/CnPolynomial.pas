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
* 备    注：整系数多项式除法只支持除数最高次数为 1 的情况
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.08.21 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SysConst, Math, CnNativeDecl;

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
    property MaxDegree: Integer read GetMaxDegree write SetMaxDegree;
    {* 最高次数，0 开始}
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

procedure IntegerPolynomialShiftLeft(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象左移 N 次，也就是各项指数都加 N}

procedure IntegerPolynomialShiftRight(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象右移 N 次，也就是各项指数都减 N，小于 0 的忽略了}

procedure IntegerPolynomialMulWord(const P: TCnIntegerPolynomial; N: Integer);
{* 将一个整系数多项式对象的各个系数都乘以 N}

function IntegerPolynomialAdd(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相加，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialSub(const Res: TCnIntegerPolynomial; const P1: TCnIntegerPolynomial;
  const P2: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相减，结果放至 Res 中，返回相加是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialMul(const Res: TCnIntegerPolynomial; P1: TCnIntegerPolynomial;
  P2: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相乘，结果放至 Res 中，返回相乘是否成功，P1 可以是 P2，Res 可以是 P1 或 P2}

function IntegerPolynomialDiv(const Res: TCnIntegerPolynomial; const Remain: TCnIntegerPolynomial;
  const P: TCnIntegerPolynomial; const Divisor: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象相除，商放至 Res 中，余数放在 Remain 中，返回相除是否成功，
   Res 或 Remail 可以是 nil，不给出对应结果。P 可以是 Divisor，Res 可以是 P 或 Divisor}

function IntegerPolynomialMod(const Res: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial): Boolean;
{* 两个整系数多项式对象求余，余数放至 Res 中，返回求余是否成功，
   Res 可以是 P 或 Divisor，P 可以是 Divisor}

implementation

resourcestring
  SCnInvalidDegree = 'Invalid Degree %d';
  SCnErrorDivMaxDegree = 'Only MaxDegree 1 Support for Integer Polynomial.';

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
    R := TCnIntegerPolynomial.Create
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
    R.Free;
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

  if Divisor[Divisor.MaxDegree] <> 1 then
    raise ECnPolynomialException.Create(SCnErrorDivMaxDegree);

  if Divisor.MaxDegree > P.MaxDegree then // 除式次数高不够除，直接变成余数
  begin
    if Res <> nil then
      IntegerPolynomialSetZero(Res);
    if (Remain <> nil) and (P <> Remain) then
      IntegerPolynomialCopy(Remain, P);
  end;

  // 够除，循环
  SubRes := nil;
  MulRes := nil;
  DivRes := nil;

  try
    SubRes := TCnIntegerPolynomial.Create;
    IntegerPolynomialCopy(SubRes, P);

    D := P.MaxDegree - Divisor.MaxDegree;
    DivRes := TCnIntegerPolynomial.Create;
    DivRes.MaxDegree := D;
    MulRes := TCnIntegerPolynomial.Create;

    for I := 0 to D do
    begin
      IntegerPolynomialCopy(MulRes, Divisor);
      IntegerPolynomialShiftLeft(MulRes, D - I);                 // 对齐到 SubRes 的最高次
      IntegerPolynomialMulWord(MulRes, SubRes[P.MaxDegree - I]); // 除式乘到最高次系数相同
      DivRes[D - I] := SubRes[P.MaxDegree - I];                  // 商放到 DivRes 位置
      IntegerPolynomialSub(SubRes, SubRes, MulRes);              // 减后结果重新放回 SubRes
    end;

    if Remain <> nil then
      IntegerPolynomialCopy(Remain, SubRes);
    if Res <> nil then
      IntegerPolynomialCopy(Res, DivRes);
    Result := True;
  finally
    SubRes.Free;
    MulRes.Free;
    DivRes.Free;
  end;
end;

function IntegerPolynomialMod(const Res: TCnIntegerPolynomial; const P: TCnIntegerPolynomial;
  const Divisor: TCnIntegerPolynomial): Boolean;
begin
  Result := IntegerPolynomialDiv(nil, Res, P, Divisor);
end;

end.
