{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnPaillier;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：Paillier 算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 Int64 范围内以及大整数范围内的加法同态 Paillier 算法。
*
*           Paillier 加密的特性：俩明文各自加密后的结果相乘，乘积作为密文解开后
*           得到的结果是原始俩明文相加（加法同态），是协同密钥体制的基础。
*
*           概念：某整数针对 N 的阶，是该整数最小次方模 N 为 1 的那个次方数
*           阶必然能够整除和 N 互质的数量（欧拉函数），求阶可枚举欧拉函数的质因数。
*           如果阶等于 N 的欧拉函数，则说明该整数一路乘方模 N 下去能够覆盖 N 下
*           的所有互质内容，这个阶就是原根。

* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.05.22 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF},
  CnConsts, CnNative, CnBigNumber;

const
  CN_PAILLIER_DEFAULT_PRIMEBITS = 2048;
  {* Paillier 算法的默认素数位数}

  // 错误码
  ECN_PAILLIER_OK                      = ECN_OK;
  {* Paillier 系列错误码：无错误，值为 0}

  ECN_PAILLIER_ERROR_BASE              = ECN_CUSTOM_ERROR_BASE + $300;
  {* Paillier 系列错误码的基准起始值，为 ECN_CUSTOM_ERROR_BASE 加上 $300}

  ECN_PAILLIER_INVALID_INPUT           = ECN_PAILLIER_ERROR_BASE + 1;
  {* Paillier 错误码之输入为空或长度错误}
  ECN_PAILLIER_RANDOM_ERROR            = ECN_PAILLIER_ERROR_BASE + 2;
  {* Paillier 错误码之随机数相关错误}

type
  TCnInt64PaillierPrivateKey = packed record
  {* Int64 范围内的 Paillier 私钥}
    P: TUInt64;
    Q: TUInt64;              // 两个素数，为避免溢出，范围控制在 Integer 内
    Lambda: TUInt64;
    Mu: TUInt64;
  end;
  PCnInt64PaillierPrivateKey = ^TCnInt64PaillierPrivateKey;

  TCnInt64PaillierPublicKey = packed record
  {* Int64 范围内的 Paillier 公钥}
    N: TUInt64;              // 俩素数乘积
    G: TUInt64;
  end;
  PCnInt64PaillierPublicKey = ^TCnInt64PaillierPublicKey;

  TCnPaillierPrivateKey = class(TPersistent)
  {* 大数范围内的 Paillier 私钥}
  private
    FP: TCnBigNumber;
    FQ: TCnBigNumber;
    FMu: TCnBigNumber;
    FLambda: TCnBigNumber;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property P: TCnBigNumber read FP;
    {* 大素数一}
    property Q: TCnBigNumber read FQ;
    {* 大素数二}
    property Lambda: TCnBigNumber read FLambda;
    {* 大素数各自减一的乘积}
    property Mu: TCnBigNumber read FMu;
    {* 通过模逆元计算而来的 Mu}
  end;

  TCnPaillierPublicKey = class(TPersistent)
  {* 大数范围内的 Paillier 公钥}
  private
    FG: TCnBigNumber;
    FN: TCnBigNumber;
    FN2: TCnBigNumber;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property N: TCnBigNumber read FN;
    {* 俩素数乘积}
    property G: TCnBigNumber read FG;
    {* 俩素数乘积加一}
    property N2: TCnBigNumber read FN2;
    {* N 的平方，存着方便计算}
  end;

function CnGenerateInt64PaillierKeys(var PrivateKey: TCnInt64PaillierPrivateKey;
  var PublicKey: TCnInt64PaillierPublicKey): Boolean;
{* 随机生成一对 Int64 范围内的 Paillier 公私钥，返回生成是否成功}

function CnInt64PaillierEncrypt(var PublicKey: TCnInt64PaillierPublicKey;
  Data: Int64; out Res: Int64; RandFactor: Int64 = 0): Boolean;
{* Int64 范围内的 Paillier 公钥加密明文数据得到密文，返回加密是否成功。
  允许外部传入随机数，0 表示内部生成}

function CnInt64PaillierDecrypt(var PrivateKey: TCnInt64PaillierPrivateKey;
  var PublicKey: TCnInt64PaillierPublicKey; EnData: Int64; out Res: Int64): Boolean;
{* Int64 范围内的 Paillier 私钥解密密文数据得到明文，返回解密是否成功}

function CnInt64PaillierAddPlain(Data1: Int64; Data2: Int64;
  var PublicKey: TCnInt64PaillierPublicKey): Int64;
{* Int64 范围内 Paillier 加法同态的明文加法，内部是模 N 加}

function CnInt64PaillierAddCipher(EnData1: Int64; EnData2: Int64;
  var PublicKey: TCnInt64PaillierPublicKey): Int64;
{* Int64 范围内 Paillier 加法同态的密文加法，内部是模 N^2 乘}

function CnGeneratePaillierKeys(PrivateKey: TCnPaillierPrivateKey;
  PublicKey: TCnPaillierPublicKey; PrimeBits: Integer = CN_PAILLIER_DEFAULT_PRIMEBITS): Boolean;
{* 随机生成一对大数范围内的 Paillier 公私钥，返回生成是否成功}

function CnPaillierEncrypt(PublicKey: TCnPaillierPublicKey;
  Data: TCnBigNumber; Res: TCnBigNumber; RandFactor: TCnBigNumber = nil): Boolean;
{* 大数范围内的 Paillier 公钥加密明文数据得到密文，返回加密是否成功}

function CnPaillierDecrypt(PrivateKey: TCnPaillierPrivateKey;
  PublicKey: TCnPaillierPublicKey; EnData: TCnBigNumber; Res: TCnBigNumber): Boolean;
{* 大数范围内的 Paillier 私钥解密密文数据得到明文，返回解密是否成功}

function CnPaillierAddPlain(const Res: TCnBigNumber; Data1: TCnBigNumber; Data2: TCnBigNumber;
  PublicKey: TCnPaillierPublicKey): Boolean;
{* 大数范围内 Paillier 加法同态的明文加法，内部是模 N 加}

function CnPaillierAddCipher(const Res: TCnBigNumber; EnData1: TCnBigNumber; EnData2: TCnBigNumber;
  PublicKey: TCnPaillierPublicKey): Boolean;
{* 大数范围内 Paillier 加法同态的密文加法，内部是模 N^2 乘}

implementation

uses
  CnPrimeNumber, CnRandom;

function CnGenerateInt64PaillierKeys(var PrivateKey: TCnInt64PaillierPrivateKey;
  var PublicKey: TCnInt64PaillierPublicKey): Boolean;
var
  AN, Lam: Int64;
  AP, AQ: Integer;
begin
  // 先随机生成两个 Integer 范围内的素数
  repeat
    repeat
      AP := CnPickRandomSmallPrime;
      AN := Trunc(Random * 100); // 复用 AN，以调整 CnPickRandomSmallPrime 内部的随机数发生器
      Sleep(AN);
      AQ := CnPickRandomSmallPrime;
    until (AP > 0) and (AQ > 0) and (AP <> AQ);

    // 得到俩素数后检查互质
    AN := CnInt64GreatestCommonDivisor(Int64(AP) * Int64(AQ), Int64(AP - 1) * Int64(AQ - 1));
  until AN = 1;

  AN := Int64(AP) * Int64(AQ);
  Lam := CnInt64LeastCommonMultiple(AP - 1, AQ - 1);
  // 得到了 N 和 Lambda，下面选择 G，G 要和 N^2 互素，且其对 N^2 的阶要能被 N 整除，且运算一阵后得到的值对 N 有逆元
  // 直接用 N + 1 貌似可以？
  // 因为：1、N 是 N + 1 模 N^2 的阶（原因： (N + 1)^N mod N^2 用二项式展开消去得到 = N^2 + 1 mod N^2 = 1），因而满足被 N 整除条件。
  // 求 Mu 的话，公式可以简化
  // (N + 1)^((P-1)*(Q-1)) mod N^2 二项式展开消去最后得到 1 + (P-1)*(Q-1)*N，代入 L 函数（减一除 N 那个），得到 (P-1)*(Q-1)
  // 所以当 G 取 N + 1 时，Mu 是 (P-1)*(Q-1) 对 N 的逆元

  PrivateKey.P := AP;
  PrivateKey.Q := AQ;
  PrivateKey.Lambda := Lam;

  PublicKey.N := AN;
  PublicKey.G := AN + 1;

  PrivateKey.Mu := CnInt64ModularInverse2(Lam, AN);

  Result := True;
end;

function CnInt64PaillierEncrypt(var PublicKey: TCnInt64PaillierPublicKey;
  Data: Int64; out Res: Int64; RandFactor: Int64): Boolean;
var
  T1, T2, R, N2, G: TUInt64;
begin
  // 公钥加密：随机生成 R < N，然后密文 = (G^M * R^N) mod N^2
  Result := False;
  if Data >= PublicKey.N then
    Exit;

  N2 := UInt64Mul(PublicKey.N, PublicKey.N);
  R := RandFactor;
  if R = 0 then
    R := RandomInt64LessThan(PublicKey.N - 2) // 注意！R 必须和 N 互质！也就是不能是 P 或 Q 的倍数！
  else
    R := UInt64Mod(R, PublicKey.N - 2); // 如果外面传的太大

  //  照理最多加 2 就能规避
  G := CnInt64GreatestCommonDivisor(R, PublicKey.N);
  if G > 1 then
  begin
    R := R + 1;
    G := CnInt64GreatestCommonDivisor(R, PublicKey.N);
    if G > 1 then
      R := R + 1;
  end;

{$IFDEF SUPPORT_UINT64}
  T1 := MontgomeryPowerMod(PublicKey.G, UInt64(Data), N2);
{$ELSE}
  T1 := MontgomeryPowerMod(PublicKey.G, Data, N2);
{$ENDIF}
  T2 := MontgomeryPowerMod(R, PublicKey.N, N2);
  Res := UInt64NonNegativeMulMod(T1, T2, N2); // 不怕溢出，变负也行

  Result := True;
end;

function CnInt64PaillierDecrypt(var PrivateKey: TCnInt64PaillierPrivateKey;
  var PublicKey: TCnInt64PaillierPublicKey; EnData: Int64; out Res: Int64): Boolean;
var
  T, N2: TUInt64;
begin
  // 私钥解密：明文 = ((((密文^Lambda mod N^2) - 1) / N) * Mu) mod N
  N2 := UInt64Mul(PublicKey.N, PublicKey.N);

{$IFDEF SUPPORT_UINT64}
  T := MontgomeryPowerMod(UInt64(EnData), PrivateKey.Lambda, N2);
{$ELSE}
  T := MontgomeryPowerMod(EnData, PrivateKey.Lambda, N2);
{$ENDIF}

  T := UInt64Div(T - 1, PublicKey.N); // 这里按 G 的设定，能整除
  Res := Int64NonNegativeMulMod(T, PrivateKey.Mu, PublicKey.N);

  Result := True;
end;

function CnInt64PaillierAddPlain(Data1, Data2: Int64;
  var PublicKey: TCnInt64PaillierPublicKey): Int64;
begin
  Result := UInt64NonNegativeAddMod(Data1, Data2, PublicKey.N);
end;

function CnInt64PaillierAddCipher(EnData1, EnData2: Int64;
  var PublicKey: TCnInt64PaillierPublicKey): Int64;
begin
  Result := UInt64NonNegativeMulMod(EnData1, EnData2, UInt64Mul(PublicKey.N, PublicKey.N));
end;

{ TCnPaillierPrivateKey }

procedure TCnPaillierPrivateKey.Assign(Source: TPersistent);
begin
  if Source is TCnPaillierPrivateKey then
  begin
    BigNumberCopy(FP, (Source as TCnPaillierPrivateKey).P);
    BigNumberCopy(FQ, (Source as TCnPaillierPrivateKey).Q);
    BigNumberCopy(FMu, (Source as TCnPaillierPrivateKey).Mu);
    BigNumberCopy(FLambda, (Source as TCnPaillierPrivateKey).Lambda);
  end
  else
    inherited;
end;

constructor TCnPaillierPrivateKey.Create;
begin
  inherited;
  FP := TCnBigNumber.Create;
  FQ := TCnBigNumber.Create;
  FLambda := TCnBigNumber.Create;
  FMu := TCnBigNumber.Create;
end;

destructor TCnPaillierPrivateKey.Destroy;
begin
  FMu.Free;
  FLambda.Free;
  FQ.Free;
  FP.Free;
  inherited;
end;

{ TCnPaillierPublicKey }

procedure TCnPaillierPublicKey.Assign(Source: TPersistent);
begin
  if Source is TCnPaillierPublicKey then
  begin
    BigNumberCopy(FN, (Source as TCnPaillierPublicKey).N);
    BigNumberCopy(FG, (Source as TCnPaillierPublicKey).G);
  end
  else
    inherited;
end;

constructor TCnPaillierPublicKey.Create;
begin
  inherited;
  FN := TCnBigNumber.Create;
  FG := TCnBigNumber.Create;
  FN2 := TCnBigNumber.Create;
end;

destructor TCnPaillierPublicKey.Destroy;
begin
  FN2.Free;
  FG.Free;
  FN.Free;
  inherited;
end;

function CnGeneratePaillierKeys(PrivateKey: TCnPaillierPrivateKey;
  PublicKey: TCnPaillierPublicKey; PrimeBits: Integer): Boolean;
var
  Suc: Boolean;
  AN, T, Lam, AP, AQ: TCnBigNumber;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) or (PrimeBits < 128) then
  begin
    _CnSetLastError(ECN_PAILLIER_INVALID_INPUT);
    Exit;
  end;

  AP := nil;
  AQ := nil;
  AN := nil;
  T := nil;
  Lam := nil;

  try
    AP := TCnBigNumber.Create;
    AQ := TCnBigNumber.Create;
    AN := TCnBigNumber.Create;
    T := TCnBigNumber.Create;
    Lam := TCnBigNumber.Create;

    Suc := False;
    repeat
      if not BigNumberGeneratePrimeByBitsCount(PrivateKey.P, PrimeBits) then
        Exit;
      if not BigNumberGeneratePrimeByBitsCount(PrivateKey.Q, PrimeBits) then
        Exit;

      if BigNumberEqual(PrivateKey.P, PrivateKey.Q) then // 不能相等
        Continue;

      if not BigNumberMul(AN, PrivateKey.P, PrivateKey.Q) then // 计算 P * Q
        Exit;

      if BigNumberCopy(AP, PrivateKey.P) = nil then
        Exit;
      if BigNumberCopy(AQ, PrivateKey.Q) = nil then
        Exit;

      AP.SubWord(1);
      AQ.SubWord(1);

      if not BigNumberMul(Lam, AP, AQ) then // 计算 (P - 1) * (Q - 1)
        Exit;

      if not BigNumberGcd(T, AN, Lam) then
        Exit;

      if T.IsOne then // PQ 与 (P-1)*(Q-1) 互素
        Suc := True;
    until Suc;

    if BigNumberCopy(PublicKey.N, AN) = nil then
      Exit;

    if not BigNumberMul(PublicKey.N2, PublicKey.N, PublicKey.N) then // 计算 N2
      Exit;

    if BigNumberCopy(PublicKey.G, AN) = nil then
      Exit;

    PublicKey.G.AddWord(1);  // G := N + 1

    if BigNumberCopy(PrivateKey.Lambda, Lam) = nil then
      Exit;

    // 再求 Mu，计算逆元
    if not BigNumberModularInverse(PrivateKey.Mu, Lam, AN) then
      Exit;

    Result := True;
    _CnSetLastError(ECN_PAILLIER_OK);
  finally
    Lam.Free;
    T.Free;
    AN.Free;
    AQ.Free;
    AP.Free;
  end;
end;

function CnPaillierEncrypt(PublicKey: TCnPaillierPublicKey;
  Data: TCnBigNumber; Res: TCnBigNumber; RandFactor: TCnBigNumber): Boolean;
var
  R, T1, T2, G, M: TCnBigNumber;
begin
  // 公钥加密：随机生成 R < N，然后密文 = (G^M * R^N) mod N^2
  Result := False;
  if BigNumberCompare(Data, PublicKey.N) >= 0 then // 待加密的明文数字不能比 N 大
  begin
    _CnSetLastError(ECN_PAILLIER_INVALID_INPUT);
    Exit;
  end;

  T1 := nil;
  R := nil;
  M := nil;
  G := nil;
  T2 := nil;

  try
    T1 := TCnBigNumber.Create;

    // 可以简化。g = n + 1 的情况下，g^m mod n^2 = m*n + 1 mod n^2
    if BigNumberCopy(T1, PublicKey.G) = nil then
      Exit;

    T1.SubWord(1);
    if BigNumberEqual(T1, PublicKey.N) then // 判断 g = n + 1
    begin
      if not BigNumberMul(T1, Data, PublicKey.N) then // Data * N
        Exit;

      T1.AddWord(1);                                     // Data * N + 1
      if not BigNumberMod(T1, T1, PublicKey.N2) then     // 再 mod N^2
        Exit;
    end
    else
    begin
      if not BigNumberPowerMod(T1, PublicKey.G, Data, PublicKey.N2) then
        Exit;
    end;

    // 生成随机值。注意！R 必须和 N 互质！也就是不能是 P 或 Q 的倍数！
    R := TCnBigNumber.Create;
    M := TCnBigNumber.Create;
    if BigNumberCopy(M, PublicKey.N) = nil then
      Exit;

    M.SubWord(2); // 以备万一不互质时加二处理

    if (RandFactor <> nil) and not RandFactor.IsZero then // 有外界传入的随机数
    begin
      if BigNumberCopy(R, RandFactor) = nil then
        Exit;

      if R.IsNegative then // 预防为负
        R.Negate;

      if not BigNumberMod(R, R, M) then // 预防随机数过大
        Exit;
    end
    else
    begin
      if not BigNumberRandRange(R, M) then
      begin
        _CnSetLastError(ECN_PAILLIER_RANDOM_ERROR);
        Exit;
      end;
    end;

    G := TCnBigNumber.Create;
    if not BigNumberGcd(G, R, PublicKey.N) then
      Exit;

    // 判断互质，照理加两次 1 就足够了
    if not G.IsOne then
    begin
      R.AddWord(1);
      if not BigNumberGcd(G, R, PublicKey.N) then
        Exit;

      if not G.IsOne then
        R.AddWord(1);
    end;

    T2 := TCnBigNumber.Create;
    if not BigNumberPowerMod(T2, R, PublicKey.N, PublicKey.N2) then
      Exit;

    if not BigNumberDirectMulMod(Res, T1, T2, PublicKey.N2) then
      Exit;

    Result := True;
    _CnSetLastError(ECN_PAILLIER_OK);
  finally
    T2.Free;
    G.Free;
    M.Free;
    R.Free;
    T1.Free;
  end;
end;

function CnPaillierDecrypt(PrivateKey: TCnPaillierPrivateKey;
  PublicKey: TCnPaillierPublicKey; EnData: TCnBigNumber; Res: TCnBigNumber): Boolean;
var
  T: TCnBigNumber;
begin
  // 私钥解密：明文 = ((((密文^Lambda mod N^2) - 1) / N) * Mu) mod N
  Result := False;

  T := nil;

  try
    T := TCnBigNumber.Create;
    if not BigNumberPowerMod(T, EnData, PrivateKey.Lambda, PublicKey.N2) then
      Exit;

    T.SubWord(1);
    if not BigNumberDiv(T, nil, T, PublicKey.N) then
      Exit;

    if not BigNumberDirectMulMod(Res, T, PrivateKey.Mu, PublicKey.N) then
      Exit;

    Result := True;
    _CnSetLastError(ECN_PAILLIER_OK);
  finally
    T.Free;
  end;
end;

function CnPaillierAddPlain(const Res: TCnBigNumber; Data1, Data2: TCnBigNumber;
  PublicKey: TCnPaillierPublicKey): Boolean;
begin
  Result := BigNumberAddMod(Res, Data1, Data2, PublicKey.N);
end;

function CnPaillierAddCipher(const Res: TCnBigNumber; EnData1, EnData2: TCnBigNumber;
  PublicKey: TCnPaillierPublicKey): Boolean;
begin
  Result := BigNumberDirectMulMod(Res, EnData1, EnData2, PublicKey.N2);
end;

end.
