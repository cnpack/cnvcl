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

unit CnSecretSharing;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：秘密共享的算法实现单元，目前包括 Shamir 门限方案
* 单元作者：刘啸
* 备    注：Shamir 门限方案是利用构造多项式生成多个点坐标并利用插值还原点的秘密共享方案
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.05.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils,
  CnConsts, CnNativeDecl, CnPrimeNumber, CnContainers, CnPolynomial, CnRandom,
  CnBigNumber;

const
  CN_SHAMIR_DEFAULT_PRIME_BITS         = 1024;

  // 错误码
  ECN_SECRET_OK                        = ECN_OK; // 没错
  ECN_SECRET_ERROR_BASE                = ECN_CUSTOM_ERROR_BASE + $400; // Secret Sharing 错误码基准

  ECN_SECRET_INVALID_INPUT             = ECN_SECRET_ERROR_BASE + 1;    // 输入为空或长度不对
  ECN_SECRET_RANDOM_ERROR              = ECN_SECRET_ERROR_BASE + 2;    // 随机数相关错误

function CnInt64ShamirSplit(Secret: Int64; ShareCount, Threshold: Integer;
  OutShares: TCnInt64List; var Prime: Int64): Boolean;
{* 用 Shamir 门限方案实现 Int64 的秘密共享。将一 Int64 值拆分为 ShareCount 个 Int64 值
  只需要其中 Threshold 个值及其顺序就能还原 Secret，返回是否拆分成功。
  拆分值放 OutShares 中，对应顺序值为其下标 + 1（如第 0 项对应 1）
  相关素数可以在 Prime 中指定，如为 0，则生成符合要求的素数值返回}

function CnInt64ShamirReconstruct(Prime: Int64; InOrders, InShares: TCnInt64List;
  out Secret: Int64): Boolean;
{* 用 Shamir 门限方案实现 Int64 的秘密共享。将 Threshold 个拆分后的 Int64 值与其对应序号并结合
  大素数重组成 Secret，返回是否重组成功。成功则秘密值放 Secret 中返回}

function CnShamirSplit(Secret: TCnBigNumber; ShareCount, Threshold: Integer;
  OutOrders, OutShares: TCnBigNumberList; Prime: TCnBigNumber): Boolean;
{* 用 Shamir 门限方案实现大数范围内的秘密共享。将一大数 Secret 拆分为 ShareCount 个大数值
  只需要其中 Threshold 个值及其顺序就能还原 Secret，返回是否拆分成功。
  拆分顺序放 InOrders 中（其内容是 1 2 3 4 ……），拆分值放 OutShares 中
  如 Prime 值不为 0 且大于 Secret 的话，调用者需自行保证 Prime 为素数}

function CnShamirReconstruct(Prime: TCnBigNumber; InOrders, InShares: TCnBigNumberList;
  OutSecret: TCnBigNumber): Boolean;
{* 用 Shamir 门限方案实现大数范围内的秘密共享。将 Threshold 个拆分后的大数值与其对应序号并结合
  大素数重组成 Secret，返回是否重组成功。成功则秘密值放 Secret 中返回}

implementation

function CnInt64ShamirSplit(Secret: Int64; ShareCount, Threshold: Integer;
  OutShares: TCnInt64List; var Prime: Int64): Boolean;
var
  Poly: TCnInt64Polynomial;
  I: Integer;
begin
  Result := False;

  if (Secret < 0) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if (Prime <= 0) or (Prime <= Secret) or not CnInt64IsPrime(Prime) then // 如果非素数或过小，则重新生成
  begin
    if Secret < CN_PRIME_NUMBERS_SQRT_UINT32[High(CN_PRIME_NUMBERS_SQRT_UINT32)] then
      Prime := CN_PRIME_NUMBERS_SQRT_UINT32[High(CN_PRIME_NUMBERS_SQRT_UINT32)]
    else
    begin
      // TODO: 寻找一个比 Secret 大的素数

    end;
  end;

  Poly := nil;
  try
    Poly := TCnInt64Polynomial.Create;

    Poly.MaxDegree := Threshold - 1;
    Poly[0] := Secret;
    for I := 1 to Poly.MaxDegree do
      Poly[I] := RandomInt64LessThan(Prime);

    // 生成了随机多项式
    OutShares.Clear;
    for I := 1 to ShareCount do
      OutShares.Add(Int64PolynomialGaloisGetValue(Poly, I, Prime));

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    Poly.Free;
  end;
end;

function CnInt64ShamirReconstruct(Prime: Int64; InOrders, InShares: TCnInt64List;
  out Secret: Int64): Boolean;
var
  I, J: Integer;
  X, Y, N, D: Int64;
begin
  Result := False;
  if (Prime < 2) or (InOrders.Count < 2) or (InOrders.Count <> InShares.Count) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  // 拉格朗日插值公式，InOrder 是一堆 X 坐标，InShares 是一堆 Y 坐标
  // 有 T 个 Shares，则要累加 T 项，每项针对一个 X Y 坐标，有以下计算方法：
  // 每项 = 本Y * (-其他所有 X 的积) / (本X - 其他所有X) 的积)

  Secret := 0;
  for I := 0 to InOrders.Count - 1 do
  begin
    X := InOrders[I];
    Y := InShares[I];

    //  连乘的放到分子 N 中，连除的放到分母 D 中再求模逆元
    N := Y;
    D := 1;
    for J := 0 to InOrders.Count - 1 do
    begin
      if J <> I then
      begin
        N := Int64NonNegativeMulMod(N, InOrders[J], Prime);
        D := Int64NonNegativeMulMod(D, Int64AddMod(X, -InOrders[J], Prime), Prime);
      end;
    end;
    D := CnInt64ModularInverse2(D, Prime);

    N := Int64NonNegativeMulMod(N, D, Prime);
    Secret := Int64AddMod(Secret, N, Prime);
  end;

  Result := True;
  _CnSetLastError(ECN_SECRET_OK);
end;

function CnShamirSplit(Secret: TCnBigNumber; ShareCount, Threshold: Integer;
  OutOrders, OutShares: TCnBigNumberList; Prime: TCnBigNumber): Boolean;
var
  Poly: TCnBigNumberPolynomial;
  T: TCnBigNumber;
  I, Bits: Integer;
begin
  Result := False;

  if (Secret.IsNegative) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Prime.IsZero or Prime.IsNegative or (BigNumberCompare(Prime, Secret) <= 0) then // 如果素数过小，则重新生成
  begin
    // 寻找一个比 Secret 大的素数，以 Bits 为媒介
    Bits := Secret.GetBitsCount + 1;
    if Bits < CN_SHAMIR_DEFAULT_PRIME_BITS then
      Bits := CN_SHAMIR_DEFAULT_PRIME_BITS;

    if not BigNumberGeneratePrimeByBitsCount(Prime, Bits) then
      Exit;
  end;

  Poly := nil;
  T := nil;

  try
    Poly := TCnBigNumberPolynomial.Create;

    Poly.MaxDegree := Threshold - 1;
    BigNumberCopy(Poly[0], Secret);
    for I := 1 to Poly.MaxDegree do
    begin
      if not BigNumberRandRange(Poly[I], Prime) then
      begin
        _CnSetLastError(ECN_SECRET_RANDOM_ERROR);
        Exit;
      end;
    end;

    // 生成了随机多项式，用 1 到 ShareCount 代入分别求值
    OutOrders.Clear;
    OutShares.Clear;

    T := TCnBigNumber.Create;
    for I := 1 to ShareCount do
    begin
      OutOrders.Add.SetWord(I);
      T.SetWord(I);
      if not BigNumberPolynomialGaloisGetValue(OutShares.Add, Poly, T, Prime) then
        Exit;
    end;

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    T.Free;
    Poly.Free;
  end;
end;

function CnShamirReconstruct(Prime: TCnBigNumber; InOrders, InShares: TCnBigNumberList;
  OutSecret: TCnBigNumber): Boolean;
var
  I, J: Integer;
  X, Y, T, N, D: TCnBigNumber;
begin
  Result := False;
  if Prime.IsNegative or Prime.IsZero or (InOrders.Count < 2) or (InOrders.Count <> InShares.Count) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  // 拉格朗日插值公式，InOrder 是一堆 X 坐标，InShares 是一堆 Y 坐标
  // 有 T 个 Shares，则要累加 T 项，每项针对一个 X Y 坐标，有以下计算方法：
  // 每项 = 本Y * (-其他所有 X 的积) / (本X - 其他所有X) 的积)

  N := nil;
  D := nil;
  T := nil;

  try
    OutSecret.SetZero;

    T := TCnBigNumber.Create;
    N := TCnBigNumber.Create;
    D := TCnBigNumber.Create;

    for I := 0 to InOrders.Count - 1 do
    begin
      X := InOrders[I];
      Y := InShares[I];

      //  连乘的放到分子 N 中，连除的放到分母 D 中再求模逆元
      if BigNumberCopy(N, Y) = nil then
        Exit;
      D.SetOne;

      for J := 0 to InOrders.Count - 1 do
      begin
        if J <> I then
        begin
          if not BigNumberDirectMulMod(N, N, InOrders[J], Prime) then
            Exit;

          if not BigNumberSubMod(T, X, InOrders[J], Prime) then
            Exit;

          if not BigNumberDirectMulMod(D, D, T, Prime) then
            Exit;
        end;
      end;

      if not BigNumberModularInverse(T, D, Prime) then
        Exit;

      if not BigNumberMulMod(N, T, D, Prime) then
        Exit;

      if not BigNumberAddMod(OutSecret, OutSecret, N, Prime) then
        Exit;
    end;

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    T.Free;
    D.Free;
    N.Free;
  end;
end;

end.
