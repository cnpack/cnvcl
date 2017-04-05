{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2017 CnPack 开发组                       }
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

unit CnRSA;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：RSA 算法单元
* 单元作者：刘啸
* 备    注：包括 Int64 范围内的 RSA 算法以及大数算法，公钥 Exponent 固定使用 65537。
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2017.04.05 V1.1
*               实现大数的 RSA 密钥生成与加解密
*           2017.04.03 V1.0
*               创建单元，Int64 范围内的 RSA 从 CnPrimeNumber 中独立出来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, CnPrimeNumber, CnBigNumber;

// Int64 范围内的 RSA 加解密实现

function Int64ExtendedEuclideanGcd(A, B: Int64; out X: Int64; out Y: Int64): Int64;
{* 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解}

function CnInt64RSAGenerateKeys(out PrimeKey1: Integer; out PrimeKey2: Integer;
  out PrivKeyProduct: Int64; out PrivKeyExponent: Int64;
  out PubKeyProduct: Int64; out PubKeyExponent: Int64): Boolean;
{* 生成 RSA 算法所需的公私钥，素数均不大于 Integer，Keys 均不大于 Int64}

function CnInt64RSAEncrypt(Data: Int64; PrivKeyProduct: Int64;
  PrivKeyExponent: Int64; out Res: Int64): Boolean;
{* 利用上面生成的私钥对数据进行加密，返回加密是否成功}

function CnInt64RSADecrypt(Res: Int64; PubKeyProduct: Int64;
  PubKeyExponent: Int64; out Data: Int64): Boolean;
{* 利用上面生成的公钥对数据进行解密，返回解密是否成功}

// 大数范围内的 RSA 加解密实现

function CnRSAGenerateKeys(Bits: Integer; PrimeKey1, PrimeKey2,
  PrivKeyProduct, PrivKeyExponent, PubKeyProduct, PubKeyExponent: TCnBigNumber): Boolean;
{* 生成 RSA 算法所需的公私钥，Bits 是素数范围，其余参数均为生成}

function CnRSAEncrypt(Data: TCnBigNumber; PrivKeyProduct: TCnBigNumber;
  PrivKeyExponent: TCnBigNumber; Res: TCnBigNumber): Boolean;
{* 利用上面生成的私钥对数据进行加密，返回加密是否成功}

function CnRSADecrypt(Res: TCnBigNumber; PubKeyProduct: TCnBigNumber;
  PubKeyExponent: TCnBigNumber; Data: TCnBigNumber): Boolean;
{* 利用上面生成的公钥对数据进行解密，返回解密是否成功}

implementation

// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分
function Int64RSACrypt(Data: Int64; Product: Int64; Exponent: Int64;
  out Res: Int64): Boolean;
begin
  Res := MontgomeryPowerMod(Data, Exponent, Product);
  Result := True;
end;

// 扩展欧几里得辗转相除法求二元一次不定方程 A * X + B * Y = 1 的整数解
function Int64ExtendedEuclideanGcd(A, B: Int64; out X: Int64; out Y: Int64): Int64;
var
  R, T: Int64;
begin
  if B = 0 then
  begin
    X := 1;
    Y := 0;
    Result := A;
  end
  else
  begin
    R := Int64ExtendedEuclideanGcd(B, A mod B, X, Y);
    T := X;
    X := Y;
    Y := T - (A div B) * Y;
    Result := R;
  end;
end;

// 生成 RSA 算法所需的公私钥，素数均不大于 Integer，Keys 均不大于 Int64
function CnInt64RSAGenerateKeys(out PrimeKey1: Integer; out PrimeKey2: Integer;
  out PrivKeyProduct: Int64; out PrivKeyExponent: Int64;
  out PubKeyProduct: Int64; out PubKeyExponent: Int64): Boolean;
var
  N: Integer;
  Product, Y: Int64;
begin
  PrimeKey1 := CnGenerateInt32Prime;

  N := Trunc(Random * 1000);
  Sleep(N);

  PrimeKey2 := CnGenerateInt32Prime;
  PrivKeyProduct := Int64(PrimeKey1) * Int64(PrimeKey2);
  PubKeyProduct := Int64(PrimeKey2) * Int64(PrimeKey1);   // 积在公私钥中是相同的
  PubKeyExponent := 65537;                                // 固定

  Product := Int64(PrimeKey1 - 1) * Int64(PrimeKey2 - 1);

  //                      e                d                p
  // 用辗转相除法求 PubKeyExponent * PrivKeyExponent mod Product = 1 中的 PrivKeyExponent
  // 也就是解方程 e * d + p * y = 1，其中 e、p 已知，求 d 与 y。
  Int64ExtendedEuclideanGcd(PubKeyExponent, Product, PrivKeyExponent, Y);
  while PrivKeyExponent < 0 do
  begin
     // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上 p，加到大于零为止
     PrivKeyExponent := PrivKeyExponent + Product;
  end;
  Result := True;
end;

// 利用上面生成的私钥对数据进行加密，返回加密是否成功
function CnInt64RSAEncrypt(Data: Int64; PrivKeyProduct: Int64;
  PrivKeyExponent: Int64; out Res: Int64): Boolean;
begin
  Result := Int64RSACrypt(Data, PrivKeyProduct, PrivKeyExponent, Res);
end;

// 利用上面生成的公钥对数据进行解密，返回解密是否成功
function CnInt64RSADecrypt(Res: Int64; PubKeyProduct: Int64;
  PubKeyExponent: Int64; out Data: Int64): Boolean;
begin
  Result := Int64RSACrypt(Res, PubKeyProduct, PubKeyExponent, Data);
end;

function CnRSAGenerateKeys(Bits: Integer; PrimeKey1, PrimeKey2,
  PrivKeyProduct, PrivKeyExponent, PubKeyProduct, PubKeyExponent: TCnBigNumber): Boolean;
var
  N: Integer;
  P, Y, R, S1, S2, One: TCnBigNumber;
begin
  Result := False;
  if Bits <= 16 then
    Exit;

  PrimeKey1.Clear;
  PrimeKey2.Clear;
  PrivKeyProduct.Clear;
  PrivKeyExponent.Clear;
  PubKeyProduct.Clear;
  PubKeyExponent.Clear;

  if not BigNumberGeneratePrime(PrimeKey1, Bits div 8) then
    Exit;

  N := Trunc(Random * 1000);
  Sleep(N);

  if not BigNumberGeneratePrime(PrimeKey2, Bits div 8) then
    Exit;

  if not BigNumberMul(PrivKeyProduct, PrimeKey1, PrimeKey2) then
    Exit;

  if not BigNumberMul(PubKeyProduct, PrimeKey1, PrimeKey2) then
    Exit;

  PubKeyExponent.SetDec('65537');

  R := nil;
  Y := nil;
  P := nil;
  S1 := nil;
  S2 := nil;
  One := nil;

  try
    R := TCnBigNumber.Create;
    Y := TCnBigNumber.Create;
    P := TCnBigNumber.Create;
    S1 := TCnBigNumber.Create;
    S2 := TCnBigNumber.Create;
    One := TCnBigNumber.Create;

    BigNumberSetOne(One);
    BigNumberSub(S1, PrimeKey1, One);
    BigNumberSub(S2, PrimeKey2, One);
    BigNumberMul(P, S1, S2);

    BigNumberExtendedEuclideanGcd(PubKeyExponent, P, PrivKeyExponent, Y, R);

    // 如果求出来的 d 小于 0，则不符合条件，需要将 d 加上 p
    if BigNumberIsNegative(PrivKeyExponent) then
       BigNumberAdd(PrivKeyExponent, PrivKeyExponent, P);
  finally
    One.Free;
    S2.Free;
    S1.Free;
    P.Free;
    Y.Free;
    R.Free;
  end;

  Result := True;
end;

// 利用公私钥对数据进行加解密，注意加解密使用的是同一套机制，无需区分
function RSACrypt(Data: TCnBigNumber; Product: TCnBigNumber; Exponent: TCnBigNumber;
  out Res: TCnBigNumber): Boolean;
begin
  Result := BigNumberMontgomeryPowerMod(Res, Data, Exponent, Product);
end;

// 利用上面生成的私钥对数据进行加密，返回加密是否成功
function CnRSAEncrypt(Data: TCnBigNumber; PrivKeyProduct: TCnBigNumber;
  PrivKeyExponent: TCnBigNumber; Res: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Data, PrivKeyProduct, PrivKeyExponent, Res);
end;

// 利用上面生成的公钥对数据进行解密，返回解密是否成功
function CnRSADecrypt(Res: TCnBigNumber; PubKeyProduct: TCnBigNumber;
  PubKeyExponent: TCnBigNumber; Data: TCnBigNumber): Boolean;
begin
  Result := RSACrypt(Res, PubKeyProduct, PubKeyExponent, Data);
end;

end.
