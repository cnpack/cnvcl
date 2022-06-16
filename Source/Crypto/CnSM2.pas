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

unit CnSM2;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SM2 椭圆曲线算法单元
* 单元作者：刘啸
* 备    注：实现了 GM/T0003.x-2012《SM2椭圆曲线公钥密码算法》
*           规范中的基于 SM2 的数据加解密、签名验签、密钥交换
*           注意其签名规范完全不同于 Openssl 中的 Ecc 签名，并且杂凑函数只能使用 SM3
*           另外，注意 SM2 椭圆曲线签名无法从签名与原始值中恢复公钥
*           虽然有 PublicKey = (s + r)^-1 * (k*G - s*G)
*           且尽管 k 对外未知但 k*G 的坐标 x1 是可用 r 反推出来，因为 r <= (e + x1) mod n
*           所以 x1 <= (r - e) mod n，因而 y1 也能算出来，但 e 使用了公钥的杂凑值
*           导致出现了先有蛋还是先有鸡的问题
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：Win7 + XE
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.06.16 V1.5
*               使用预计算 2 次幂点来加速 SM2 点乘计算
*           2022.06.01 V1.5
*               增加简易的协同解密与签名的实现
*           2022.05.27 V1.4
*               增加文件加解密的实现
*           2022.05.26 V1.3
*               增加非交互式 Schnorr 零知识证明验证过程的实现
*           2022.03.30 V1.2
*               兼容加解密的 C1C3C2 与 C1C2C3 排列模式以及前导字节 04
*           2021.11.25 V1.1
*               增加封装的 SignFile 与 VerifyFile 函数
*           2020.04.04 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, CnECC, CnBigNumber, CnConsts, CnSM3;

const
  CN_SM2_FINITEFIELD_BYTESIZE = 32; // 256 Bits

  // 错误码
  ECN_SM2_OK                           = ECN_OK; // 没错
  ECN_SM2_ERROR_BASE                   = ECN_CUSTOM_ERROR_BASE + $200; // SM2 错误码基准

  ECN_SM2_INVALID_INPUT                = ECN_SM2_ERROR_BASE + 1; // 输入为空或长度不对
  ECN_SM2_RANDOM_ERROR                 = ECN_SM2_ERROR_BASE + 2; // 随机数相关错误
  ECN_SM2_BIGNUMBER_ERROR              = ECN_SM2_ERROR_BASE + 3; // 大数运算错误
  ECN_SM2_DECRYPT_INFINITE_ERROR       = ECN_SM2_ERROR_BASE + 4; // 解密时碰上无穷远点
  ECN_SM2_KEYEXCHANGE_INFINITE_ERROR   = ECN_SM2_ERROR_BASE + 5; // 密钥交换碰上无穷远点

type
  TCnSM2PrivateKey = TCnEccPrivateKey;
  {* SM2 的私钥就是普通椭圆曲线的私钥}

  TCnSM2PublicKey = TCnEccPublicKey;
  {* SM2 的公钥就是普通椭圆曲线的公钥}

  TCnSM2 = class(TCnEcc)
  {* SM2 椭圆曲线运算类，具体实现在指定曲线类型的基类 TCnEcc 中}
  public
    constructor Create; override;

    procedure AffineMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point); override;
    {* 使用预计算的仿射坐标点进行加速}
  end;

  TCnSM2Signature = class(TCnEccSignature);
  {* SM2 椭圆曲线签名的内容就是普通椭圆曲线的签名的内容（注意算法与文件格式不同）}

  TCnSM2CryptSequenceType = (cstC1C3C2, cstC1C2C3);
  {* SM2 加密数据时的拼接方式，国标上是 C1C3C2，但经常有 C1C2C3 的版本，故此做兼容}

  TCnSM2CollaborativePrivateKey = TCnEccPrivateKey;
  {* SM2 协同私钥就是普通椭圆曲线的私钥，但有至少两个}

  TCnSM2CollaborativePublicKey = TCnEccPublicKey;
  {* SM2 协同私钥就是普通椭圆曲线的公钥，同样是一个}

// ========================== SM2 椭圆曲线密钥生成 =============================

function CnSM2GenerateKeys(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil): Boolean;
{* 生成一对 SM2 公私钥}

// ========================= SM2 椭圆曲线加解密算法 ============================

function CnSM2EncryptData(PlainData: Pointer; DataLen: Integer; OutStream:
  TStream; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2;
  IncludePrefixByte: Boolean = True): Boolean;
{* 用公钥对数据块进行加密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3
   IncludePrefixByte 用来声明是否包括 C1 前导的 $04 一字节，默认包括}

function CnSM2DecryptData(EnData: Pointer; DataLen: Integer; OutStream: TStream;
  PrivateKey: TCnSM2PrivateKey; SM2: TCnSM2 = nil;
  SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
{* 用私钥对数据块进行解密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则
   SequenceType 用来指明内部拼接采用默认国标的 C1C3C2 还是想当然的 C1C2C3
   无需 IncludePrefixByte 参数，内部自动处理}

function CnSM2EncryptFile(const InFile, OutFile: string; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
{* 用公钥加密 InFile 文件内容，加密结果存 OutFile 里，返回是否加密成功}

function CnSM2DecryptFile(const InFile, OutFile: string; PrivateKey: TCnSM2PrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
{* 用私钥解密 InFile 文件内容，解密结果存 OutFile 里，返回是否解密成功}

// ====================== SM2 椭圆曲线数字签名验证算法 =========================

function CnSM2SignData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  OutSignature: TCnSM2Signature; PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil): Boolean;
{* 私钥对数据块签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法
   第2部分:数字签名算法》中的运算规则，要附上签名者与曲线信息以及公钥的数字摘要}

function CnSM2VerifyData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  InSignature: TCnSM2Signature; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean;
{* 公钥验证数据块的签名，按 GM/T0003.2-2012《SM2椭圆曲线公钥密码算法
   第2部分:数字签名算法》中的运算规则来}

function CnSM2SignFile(const UserID: AnsiString; const FileName: string;
  PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): string;
{* 封装的私钥对文件签名操作，返回签名值的十六进制字符串，注意内部操作是将文件全部加载入内存
  如签名出错则返回空值}

function CnSM2VerifyFile(const UserID: AnsiString; const FileName: string;
  const InHexSignature: string; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean;
{* 封装的公钥验证数据块的签名，参数是签名值的十六进制字符串，注意内部操作是将文件全部加载入内存
  验证通过返回 True，不通过或出错返回 False}

// ======================== SM2 椭圆曲线密钥交换算法 ===========================

{
  SM2 密钥交换前提：A B 双方都有自身 ID 与公私钥，并都知道对方的 ID 与对方的公钥
}
function CnSM2KeyExchangeAStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey;
  OutARand: TCnBigNumber; OutRA: TCnEccPoint; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第一步 A 用户生成随机点 RA，供发给 B
  输入：A B 的用户名，所需密码长度、自己的私钥、双方的公钥
  输出：随机值 OutARand；生成的随机点 RA（发给 B）}

function CnSM2KeyExchangeBStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey; InRA: TCnEccPoint;
  out OutKeyB: AnsiString; OutRB: TCnEccPoint; out OutOptionalSB: TSM3Digest;
  out OutOptionalS2: TSM3Digest; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第二步 B 用户收到 A 的数据，计算 Kb，并把可选的验证结果返回 A
  输入：A B 的用户名，所需密码长度、自己的私钥、双方的公钥、A 传来的 RA
  输出：计算成功的共享密钥 Kb、生成的随机点 RB（发给 A）、可选的校验杂凑 SB（发给 A 验证），可选的校验杂凑 S2}

function CnSM2KeyExchangeAStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey; MyRA, InRB: TCnEccPoint;
  MyARand: TCnBigNumber; out OutKeyA: AnsiString; InOptionalSB: TSM3Digest;
  out OutOptionalSA: TSM3Digest; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第三步 A 用户收到 B 的数据计算 Ka，并把可选的验证结果返回 B，初步协商好 Ka = Kb
  输入：A B 的用户名，所需密码长度、自己的私钥、双方的公钥、B 传来的 RB 与可选的 SB，自己的点 RA、自己的随机值 MyARand
  输出：计算成功的共享密钥 Ka、可选的校验杂凑 SA（发给 B 验证）}

function CnSM2KeyExchangeBStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey;
  InOptionalSA: TSM3Digest; MyOptionalS2: TSM3Digest; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 的密钥交换协议，第四步 B 用户收到 A 的数据计算结果校验，协商完毕，此步可选
  实质上只对比 B 第二步生成的 S2 与 A 第三步发来的 SA，其余参数均不使用}

// =============== 基于 SM2/SM3 的非交互式 Schnorr 零知识证明 ==================

function CnSM2SchnorrProve(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  OutR: TCnEccPoint; OutZ: TCnBigNumber; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2/SM3 的非交互式 Schnorr 零知识证明步骤一，由私钥拥有者调用
  私钥拥有者生成 R 和 Z，返回生成是否成功
  该函数用于 SM2 私钥拥有者证明自己拥有对应公钥的私钥而无需公开该私钥}

function CnSM2SchnorrCheck(PublicKey: TCnSM2PublicKey; InR: TCnEccPoint;
  InZ: TCnBigNumber; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2/SM3 的非交互式 Schnorr 零知识证明步骤二，由拿到公钥者验证
  验证对方发来的 R 和 Z，如果成功，说明对方拥有该公钥对应的私钥
  该函数用于验证对方是否拥有某 SM2 公钥对应的私钥}

// ========== SM2 椭圆曲线双方互相信任的简易协同算法之协同密钥生成 =============

{
  本协同模式下，A B 双方互相信任，因而不对对方做认证，无条件相信对方发来的数据。
}
function CnSM2CollaborativeGenerateKeyAStep1(PrivateKeyA: TCnSM2CollaborativePrivateKey;
  PointToB: TCnEccPoint; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的协同算法，A 第一步生成自己的私钥 PrivateKeyA，并产出中间结果 PointToB，
  该点值需要传输至 B，返回是否生成成功}

function CnSM2CollaborativeGenerateKeyBStep1(PrivateKeyB: TCnSM2CollaborativePrivateKey;
  InPointFromA: TCnEccPoint; PublicKey: TCnSM2CollaborativePublicKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的协同算法，B 第二步生成自己的私钥 PrivateKeyB，并根据 A 传来的中间结果 InPointFromA，
  生成公用的公钥 PublicKey，返回是否生成成功
  公钥 PublicKey 后面需要传给 A 并公布出去}

// =============== SM2 椭圆曲线双方互相信任的简易协同签名算法 ==================

function CnSM2CollaborativeSignAStep1(const UserID: AnsiString; PlainData: Pointer;
  DataLen: Integer; OutHashEToB: TCnBigNumber; OutQToB: TCnEccPoint; OutRandK: TCnBigNumber;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的协同签名，A 第一步根据原始数据签出中间值 E 和 Q，发送给 B，返回该步签名是否成功
  注意 OutRandK 不要发给 B！}

function CnSM2CollaborativeSignBStep1(InHashEFromA: TCnBigNumber; InQFromA: TCnEccPoint;
  OutRToB, OutS1ToB, OutS2ToB: TCnBigNumber; PrivateKeyB: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的协同签名，B 第二步根据 A 签出的中间值 E 和 Q，生成 R S1 S2 发送回 A，返回该步签名是否成功}

function CnSM2CollaborativeSignAStep2(InRandK, InRFromB, InS1FromB, InS2FromB: TCnBigNumber;
  OutSignature: TCnSM2Signature; PrivateKeyA: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的协同签名，A 第三步根据 A 第一步的 OutRandK 随机值与 B 签出的中间值 R S1 S2，生成最终签名，返回该步签名是否成功}

// =============== SM2 椭圆曲线双方互相信任的简易协同解密算法 ==================

function CnSM2CollaborativeDecryptAStep1(EnData: Pointer; DataLen: Integer;
  OutTToB: TCnEccPoint; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的协同解密，A 第一步根据密文解出中间值 T，发送给 B，返回该步解密是否成功}

function CnSM2CollaborativeDecryptBStep1(InTFromA: TCnEccPoint; OutTToA: TCnEccPoint;
  PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2 = nil): Boolean;
{* 基于 SM2 椭圆曲线的协同解密，B 第二步根据 A 解出的中间值 T，生成另一个中间值 T 发送回 A，
  返回该步解密是否成功}

function CnSM2CollaborativeDecryptAStep2(EnData: Pointer; DataLen: Integer;
  InTFromB: TCnEccPoint; OutStream: TStream; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
{* 基于 SM2 椭圆曲线的协同解密，A 第三步根据 B 解出的中间值 T 算出最终解密结果写入 Stream，
  返回该步最终解密是否成功
  注意密文与 SequenceType 须保持和 AStep1 中的完全一致}

implementation

uses
  CnKDF;

var
  FSM2AffineGPower2KList: TObjectList = nil; // SM2 的 G 点的预计算坐标，第 n 个表示 2^n 次方倍点

{* X <= 2^W + (x and (2^W - 1) 表示把 x 的第 W 位置 1，第 W + 1 及以上全塞 0
   简而言之就是取 X 的低 W 位并保证再一位的第 W 位是 1，位从 0 开始数
  其中 W 是 N 的 BitsCount 的一半少点儿，该函数用于密钥交换
  注意：它和 CnECC 中的同名函数功能不同}
procedure BuildShortXValue(X: TCnBigNumber; Order: TCnBigNumber);
var
  I, W: Integer;
begin
  W := (Order.GetBitsCount + 1) div 2 - 1;
  BigNumberSetBit(X, W);
  for I := W + 1 to X.GetBitsCount - 1 do
    BigNumberClearBit(X, I);
end;

{ TCnSM2 }

procedure TCnSM2.AffineMultiplePoint(K: TCnBigNumber; Point: TCnEcc3Point);
var
  I: Integer;
  E, R: TCnEcc3Point;
  IsG: Boolean;
begin
  if BigNumberIsNegative(K) then
  begin
    BigNumberSetNegative(K, False);
    AffinePointInverse(Point);
  end;

  if BigNumberIsZero(K) then
  begin
    Point.X.SetZero;
    Point.Y.SetZero;
    Point.Z.SetZero;
    Exit;
  end
  else if BigNumberIsOne(K) then // 乘 1 无需动
    Exit;

  IsG := Point.Z.IsOne and BigNumberEqual(Point.X, Generator.X) and BigNumberEqual(Point.Y, Generator.Y);

  R := nil;
  E := nil;

  try
    R := TCnEcc3Point.Create;
    E := TCnEcc3Point.Create;

    E.X := Point.X;
    E.Y := Point.Y;
    E.Z := Point.Z;

    for I := 0 to BigNumberGetBitsCount(K) - 1 do
    begin
      if BigNumberIsBitSet(K, I) then
        AffinePointAddPoint(R, E, R);

      // 如果 P 是 G 点，则无需点加，直接取出
      if IsG and (I < FSM2AffineGPower2KList.Count - 1) then
        E.Assign(TCnEcc3Point(FSM2AffineGPower2KList[I + 1]))
      else // 如果此次没有预置点，则 E 自加
        AffinePointAddPoint(E, E, E);
    end;

    Point.X := R.X;
    Point.Y := R.Y;
    Point.Z := R.Z;
  finally
    R.Free;
    E.Free;
  end;
end;

constructor TCnSM2.Create;
begin
  inherited;
  Load(ctSM2);
end;

function CnSM2GenerateKeys(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    SM2.GenerateKeys(PrivateKey, PublicKey);
    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  传入明文 M，长 MLen 字节，随机生成 k，计算

  C1 = k * G => (x1, y1)         // 非压缩存储，长度为两个数字位长加 1，在 SM2 中也就是 32 * 2 + 1 = 65 字节

  k * PublicKey => (x2, y2)
  t <= KDF(x2‖y2, MLen)
  C2 <= M xor t                  // 长度 MLen

  C3 <= SM3(x2‖M‖y2)           // 长度 32 字节

  密文为：C1‖C3‖C2             // 总长 MLen + 97 字节
}
function CnSM2EncryptData(PlainData: Pointer; DataLen: Integer; OutStream:
  TStream; PublicKey: TCnSM2PublicKey; SM2: TCnSM2;
  SequenceType: TCnSM2CryptSequenceType; IncludePrefixByte: Boolean): Boolean;
var
  Py, P1, P2: TCnEccPoint;
  K: TCnBigNumber;
  B: Byte;
  M: PAnsiChar;
  I: Integer;
  Buf: array of Byte;
  KDFStr, T, C3H: AnsiString;
  Sm3Dig: TSM3Digest;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (OutStream = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  Py := nil;
  P1 := nil;
  P2 := nil;
  K := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    K := TCnBigNumber.Create;

    // 确保公钥 X Y 均存在
    if PublicKey.Y.IsZero then
    begin
      Py := TCnEccPoint.Create;
      if not SM2.PlainToPoint(PublicKey.X, Py) then
        Exit;
      BigNumberCopy(PublicKey.Y, Py.Y);
    end;

    // 生成一个随机 K
    if not BigNumberRandRange(K, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;

    P1 := TCnEccPoint.Create;
    P1.Assign(SM2.Generator);
    SM2.MultiplePoint(K, P1);  // 计算出 K * G 得到 X1 Y1

    OutStream.Position := 0;
    if IncludePrefixByte then
    begin
      B := 4;
      OutStream.Write(B, 1);
    end;

    SetLength(Buf, P1.X.GetBytesCount);
    P1.X.ToBinary(@Buf[0]);
    OutStream.Write(Buf[0], P1.X.GetBytesCount);
    SetLength(Buf, P1.Y.GetBytesCount);
    P1.Y.ToBinary(@Buf[0]);
    OutStream.Write(Buf[0], P1.Y.GetBytesCount); // 拼成 C1

    P2 := TCnEccPoint.Create;
    P2.Assign(PublicKey);
    SM2.MultiplePoint(K, P2); // 计算出 K * PublicKey 得到 X2 Y2

    SetLength(KDFStr, P2.X.GetBytesCount + P2.Y.GetBytesCount);
    P2.X.ToBinary(@KDFStr[1]);
    P2.Y.ToBinary(@KDFStr[P2.X.GetBytesCount + 1]);
    T := CnSM2KDF(KDFStr, DataLen);

    M := PAnsiChar(PlainData);
    for I := 1 to DataLen do
      T[I] := AnsiChar(Byte(T[I]) xor Byte(M[I - 1])); // T 里是 C2，但先不能写

    SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + DataLen);
    P2.X.ToBinary(@C3H[1]);
    Move(M[0], C3H[P2.X.GetBytesCount + 1], DataLen);
    P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + DataLen + 1]); // 拼成算 C3 的
    Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

    if SequenceType = cstC1C3C2 then
    begin
      OutStream.Write(Sm3Dig[0], SizeOf(TSM3Digest));        // 写入 C3
      OutStream.Write(T[1], DataLen);                        // 写入 C2
    end
    else
    begin
      OutStream.Write(T[1], DataLen);                        // 写入 C2
      OutStream.Write(Sm3Dig[0], SizeOf(TSM3Digest));        // 写入 C3
    end;

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    P2.Free;
    P1.Free;
    Py.Free;
    K.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  MLen <= DataLen - SM3DigLength - 2 * Sm2 Byte Length - 1，劈开拿到 C1 C2 C3

  PrivateKey * C1 => (x2, y2)

  t <= KDF(x2‖y2, Mlen)

  M' <= C2 xor t

  还可对比 SM3(x2‖M‖y2) Hash 是否与 C3 相等
}
function CnSM2DecryptData(EnData: Pointer; DataLen: Integer; OutStream: TStream;
  PrivateKey: TCnSM2PrivateKey; SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  MP: AnsiString;
  KDFStr, T, C3H: AnsiString;
  SM2IsNil: Boolean;
  P2: TCnEccPoint;
  I, PrefixLen: Integer;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  if (EnData = nil) or (DataLen <= 0) or (OutStream = nil) or (PrivateKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  P2 := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    MLen := DataLen - SizeOf(TSM3Digest) - (SM2.BitsCount div 4);
    if MLen <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    P2 := TCnEccPoint.Create;
    M := PAnsiChar(EnData);
    if M^ = #$04 then  // 跳过可能的前导字节 $04
    begin
      Dec(MLen);
      if MLen <= 0 then
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
        Exit;
      end;

      PrefixLen := 1;
      Inc(M);
    end
    else
      PrefixLen := 0;

    // 读出 C1
    P2.X.SetBinary(M, SM2.BitsCount div 8);
    Inc(M, SM2.BitsCount div 8);
    P2.Y.SetBinary(M, SM2.BitsCount div 8);
    if P2.IsZero then
    begin
      _CnSetLastError(ECN_SM2_DECRYPT_INFINITE_ERROR);
      Exit;
    end;

    SM2.MultiplePoint(PrivateKey, P2);

    SetLength(KDFStr, P2.X.GetBytesCount + P2.Y.GetBytesCount);
    P2.X.ToBinary(@KDFStr[1]);
    P2.Y.ToBinary(@KDFStr[P2.X.GetBytesCount + 1]);
    T := CnSM2KDF(KDFStr, MLen);

    if SequenceType = cstC1C3C2 then
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, SizeOf(TSM3Digest) + (SM2.BitsCount div 4) + PrefixLen); // 跳过 C3 指向 C2
      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I])); // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + MLen);
      P2.X.ToBinary(@C3H[1]);
      Move(MP[1], C3H[P2.X.GetBytesCount + 1], MLen);
      P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + MLen + 1]);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, (SM2.BitsCount div 4) + PrefixLen);             // M 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TSM3Digest)) then  // 比对 Hash 是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end
    else // C1C2C3 的排列
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, (SM2.BitsCount div 4) + PrefixLen);  // 指向 C2

      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I])); // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + MLen);
      P2.X.ToBinary(@C3H[1]);
      Move(MP[1], C3H[P2.X.GetBytesCount + 1], MLen);
      P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + MLen + 1]);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, (SM2.BitsCount div 4) + PrefixLen + MLen);      // 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TSM3Digest)) then  // 比对 Hash 是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end;
  finally
    P2.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2EncryptFile(const InFile, OutFile: string; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
var
  Stream: TMemoryStream;
  F: TFileStream;
begin
  Stream := nil;
  F := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFile);

    F := TFileStream.Create(OutFile, fmCreate);
    Result := CnSM2EncryptData(Stream.Memory, Stream.Size, F, PublicKey, SM2, SequenceType);
  finally
    F.Free;
    Stream.Free;
  end;
end;

function CnSM2DecryptFile(const InFile, OutFile: string; PrivateKey: TCnSM2PrivateKey;
  SM2: TCnSM2 = nil; SequenceType: TCnSM2CryptSequenceType = cstC1C3C2): Boolean;
var
  Stream: TMemoryStream;
  F: TFileStream;
begin
  Stream := nil;
  F := nil;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(InFile);

    F := TFileStream.Create(OutFile, fmCreate);
    Result := CnSM2DecryptData(Stream.Memory, Stream.Size, F, PrivateKey, SM2, SequenceType);
  finally
    F.Free;
    Stream.Free;
  end;
end;

// 计算 Za 值也就是 Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
function CalcSM2UserHash(const UserID: AnsiString; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2): TSM3Digest;
var
  Stream: TMemoryStream;
  Len: Integer;
  ULen: Word;
begin
  Stream := TMemoryStream.Create;
  try
    Len := Length(UserID) * 8;
    ULen := ((Len and $FF) shl 8) or ((Len and $FF00) shr 8);

    Stream.Write(ULen, SizeOf(ULen));
    if ULen > 0 then
      Stream.Write(UserID[1], Length(UserID));

    BigNumberWriteBinaryToStream(SM2.CoefficientA, Stream);
    BigNumberWriteBinaryToStream(SM2.CoefficientB, Stream);
    BigNumberWriteBinaryToStream(SM2.Generator.X, Stream);
    BigNumberWriteBinaryToStream(SM2.Generator.Y, Stream);
    BigNumberWriteBinaryToStream(PublicKey.X, Stream);
    BigNumberWriteBinaryToStream(PublicKey.Y, Stream);

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);  // 算出 ZA
  finally
    Stream.Free;
  end;
end;

// 根据 Za 与数据再次计算杂凑值 e
function CalcSM2SignatureHash(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  PublicKey: TCnSM2PublicKey; SM2: TCnSM2): TSM3Digest;
var
  Stream: TMemoryStream;
  Sm3Dig: TSM3Digest;
begin
  Stream := TMemoryStream.Create;
  try
    Sm3Dig := CalcSM2UserHash(UserID, PublicKey, SM2);
    Stream.Write(Sm3Dig[0], SizeOf(TSM3Digest));
    Stream.Write(PlainData^, DataLen);

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);  // 再次算出杂凑值 e
  finally
    Stream.Free;
  end;
end;

{
  ZA <= Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
  e <= Hash(ZA‖M)

  k * G => (x1, y1)

  输出签名 r <= (e + x1) mod n

  输出签名 s <= ((1 + PrivateKey)^-1 * (k - r * PrivateKey)) mod n

}
function CnSM2SignData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  OutSignature: TCnSM2Signature; PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  SM2: TCnSM2): Boolean;
var
  K, R, E: TCnBigNumber;
  P: TCnEccPoint;
  SM2IsNil: Boolean;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (OutSignature = nil) or
    (PrivateKey = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K := nil;
  P := nil;
  E := nil;
  R := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataLen, PublicKey, SM2); // 杂凑值 e

    P := TCnEccPoint.Create;
    E := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    K := TCnBigNumber.Create;

    while True do
    begin
      // 生成一个随机 K
      if not BigNumberRandRange(K, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;

      P.Assign(SM2.Generator);
      SM2.MultiplePoint(K, P);

      // 计算 R = (e + x) mod N
      E.SetBinary(@Sm3Dig[0], SizeOf(TSM3Digest));
      if not BigNumberAdd(E, E, P.X) then
        Exit;
      if not BigNumberMod(R, E, SM2.Order) then // 算出 R 后 E 不用了
        Exit;

      if R.IsZero then  // R 不能为 0
        Continue;

      if not BigNumberAdd(E, R, K) then
        Exit;
      if BigNumberCompare(E, SM2.Order) = 0 then // R + K = N 也不行
        Continue;

      BigNumberCopy(OutSignature.R, R);  // 得到一个签名值 R

      BigNumberCopy(E, PrivateKey);
      BigNumberAddWord(E, 1);
      BigNumberModularInverse(R, E, SM2.Order);      // 求逆元得到 (1 + PrivateKey)^-1，放在 R 里

      // 求 K - R * PrivateKey，又用起 E 来
      if not BigNumberMul(E, OutSignature.R, PrivateKey) then
        Exit;
      if not BigNumberSub(E, K, E) then
        Exit;

      if not BigNumberMul(R, E, R) then // (1 + PrivateKey)^-1 * (K - R * PrivateKey) 放在 R 里
        Exit;

      if not BigNumberNonNegativeMod(OutSignature.S, R, SM2.Order) then // 注意余数不能为负
        Exit;

      Result := True;
      _CnSetLastError(ECN_SM2_OK);

      Break;
    end;
  finally
    K.Free;
    P.Free;
    R.Free;
    E.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  s 和 r 是签名值
  ZA = Hash(EntLen‖UserID‖a‖b‖xG‖yG‖xA‖yA)
  e <= Hash(ZA‖M)

  t <= (r + s) mod n
  P <= s * G + t * PublicKey
  r' <= (e + P.x) mod n
  比对 r' 和 r
}
function CnSM2VerifyData(const UserID: AnsiString; PlainData: Pointer; DataLen: Integer;
  InSignature: TCnSM2Signature; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): Boolean;
var
  K, R, E: TCnBigNumber;
  P, Q: TCnEccPoint;
  SM2IsNil: Boolean;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (InSignature = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K := nil;
  P := nil;
  Q := nil;
  E := nil;
  R := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if BigNumberCompare(InSignature.R, SM2.Order) >= 0 then
      Exit;
    if BigNumberCompare(InSignature.S, SM2.Order) >= 0 then
      Exit;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataLen, PublicKey, SM2); // 杂凑值 e

    P := TCnEccPoint.Create;
    Q := TCnEccPoint.Create;
    E := TCnBigNumber.Create;
    R := TCnBigNumber.Create;
    K := TCnBigNumber.Create;

    if not BigNumberAdd(K, InSignature.R, InSignature.S) then
      Exit;
    if not BigNumberNonNegativeMod(R, K, SM2.Order) then
      Exit;
    if R.IsZero then  // (r + s) mod n = 0 则失败，这里 R 是文中的 T
      Exit;

    P.Assign(SM2.Generator);
    SM2.MultiplePoint(InSignature.S, P);
    Q.Assign(PublicKey);
    SM2.MultiplePoint(R, Q);
    SM2.PointAddPoint(P, Q, P);   // s * G + t * PublicKey => P

    E.SetBinary(@Sm3Dig[0], SizeOf(TSM3Digest));
    if not BigNumberAdd(E, E, P.X) then
      Exit;

    if not BigNumberNonNegativeMod(R, E, SM2.Order) then
      Exit;

    Result := BigNumberCompare(R, InSignature.R) = 0;
    _CnSetLastError(ECN_SM2_OK); // 正常进行校验，即使校验不通过也清空错误码
  finally
    K.Free;
    P.Free;
    Q.Free;
    R.Free;
    E.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2SignFile(const UserID: AnsiString; const FileName: string;
  PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): string;
var
  OutSign: TCnSM2Signature;
  Stream: TMemoryStream;
begin
  Result := '';
  if not FileExists(FileName) then
  begin
    _CnSetLastError(ECN_FILE_NOT_FOUND);
    Exit;
  end;

  OutSign := nil;
  Stream := nil;

  try
    OutSign := TCnSM2Signature.Create;
    Stream := TMemoryStream.Create;

    Stream.LoadFromFile(FileName);
    if CnSM2SignData(UserID, Stream.Memory, Stream.Size, OutSign, PrivateKey, PublicKey, SM2) then
      Result := OutSign.ToHex(SM2.BytesCount);
  finally
    Stream.Free;
    OutSign.Free;
  end;
end;

function CnSM2VerifyFile(const UserID: AnsiString; const FileName: string;
  const InHexSignature: string; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): Boolean;
var
  InSign: TCnSM2Signature;
  Stream: TMemoryStream;
begin
  Result := False;
  if not FileExists(FileName) then
  begin
    _CnSetLastError(ECN_FILE_NOT_FOUND);
    Exit;
  end;

  InSign := nil;
  Stream := nil;

  try
    InSign := TCnSM2Signature.Create;
    InSign.SetHex(InHexSignature);

    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(FileName);

    Result := CnSM2VerifyData(UserID, Stream.Memory, Stream.Size, InSign, PublicKey, SM2);
  finally
    Stream.Free;
    InSign.Free;
  end;
end;

{
  计算交换出的密钥：KDF(Xuv‖Yuv‖Za‖Zb, kLen)
}
function CalcSM2ExchangeKey(UV: TCnEccPoint; Za, Zb: TSM3Digest; KeyByteLength: Integer): AnsiString;
var
  Stream: TMemoryStream;
  S: AnsiString;
begin
  Stream := TMemoryStream.Create;
  try
    BigNumberWriteBinaryToStream(UV.X, Stream);
    BigNumberWriteBinaryToStream(UV.Y, Stream);
    Stream.Write(Za[0], SizeOf(TSM3Digest));
    Stream.Write(Zb[0], SizeOf(TSM3Digest));

    SetLength(S, Stream.Size);
    Stream.Position := 0;
    Stream.Read(S[1], Stream.Size);

    Result := CnSM2KDF(S, KeyByteLength);
  finally
    SetLength(S, 0);
    Stream.Free;
  end;
end;

{
  Hash(0x02‖Yuv‖Hash(Xuv‖Za‖Zb‖X1‖Y1‖X2‖Y2))
       0x03
}
function CalcSM2OptionalSig(UV, P1, P2: TCnEccPoint; Za, Zb: TSM3Digest; Step2or3: Boolean): TSM3Digest;
var
  Stream: TMemoryStream;
  Sm3Dig: TSM3Digest;
  B: Byte;
begin
  if Step2or3 then
    B := 2
  else
    B := 3;

  Stream := TMemoryStream.Create;
  try
    BigNumberWriteBinaryToStream(UV.X, Stream);
    Stream.Write(Za[0], SizeOf(TSM3Digest));
    Stream.Write(Zb[0], SizeOf(TSM3Digest));
    BigNumberWriteBinaryToStream(P1.X, Stream);
    BigNumberWriteBinaryToStream(P1.Y, Stream);
    BigNumberWriteBinaryToStream(P2.X, Stream);
    BigNumberWriteBinaryToStream(P2.Y, Stream);
    Sm3Dig := SM3(PAnsiChar(Stream.Memory), Stream.Size);

    Stream.Clear;
    Stream.Write(B, 1);
    BigNumberWriteBinaryToStream(UV.Y, Stream);
    Stream.Write(Sm3Dig[0], SizeOf(TSM3Digest));

    Result := SM3(PAnsiChar(Stream.Memory), Stream.Size);
  finally
    Stream.Free;
  end;
end;

{
  随机值 rA * G => RA 传给 B
}
function CnSM2KeyExchangeAStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey;
  OutARand: TCnBigNumber; OutRA: TCnEccPoint; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (KeyByteLength <= 0) or (APrivateKey = nil) or (APublicKey = nil) or (OutRA = nil)
    or (OutARand = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;
  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(OutARand, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;

    OutRA.Assign(SM2.Generator);
    SM2.MultiplePoint(OutARand, OutRA);
    Result := True;
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  随机值 * G => RB
  x2 <= RB.X
  X2 <= 2^W + (x2 and (2^W - 1) 表示把 x2 的第 W 位置 1，W + 1 以上全塞 0
  T <= (BPrivateKey + 随机值 * X2) mod N

  x1 <= RA.X
  X1 <= 2^W + (x1 and (2^W - 1)
  KB <= (h * T) * (APublicKey + X1 * RA)

  注意 BigNumber 的 BitCount 为 2 为底的对数向上取整
}
function CnSM2KeyExchangeBStep1(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey; InRA: TCnEccPoint;
  out OutKeyB: AnsiString; OutRB: TCnEccPoint; out OutOptionalSB: TSM3Digest;
  out OutOptionalS2: TSM3Digest; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
  R, X, T: TCnBigNumber;
  V: TCnEccPoint;
  Za, Zb: TSM3Digest;
begin
  Result := False;
  if (KeyByteLength <= 0) or (BPrivateKey = nil) or (APublicKey = nil) or
    (BPublicKey = nil) or (InRA = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;
  R := nil;
  X := nil;
  T := nil;
  V := nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not SM2.IsPointOnCurve(InRA) then // 验证传过来的 RA 是否满足方程
      Exit;

    R := TCnBigNumber.Create;
    if not BigNumberRandRange(R, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;

    OutRB.Assign(SM2.Generator);
    SM2.MultiplePoint(R, OutRB);

    X := TCnBigNumber.Create;
    BigNumberCopy(X, OutRB.X);

    // 2^W 次方表示第 W 位 1（位从 0 开始算） ，2^W - 1 则表示 0 位到 W - 1 位全置 1
    // X2 = 2^W + (x2 and (2^W - 1) 表示把 x2 的第 W 位置 1，W + 1 以上全塞 0，x2 是 RB.X
    BuildShortXValue(X, SM2.Order);

    if not BigNumberMul(X, R, X) then
      Exit;
    if not BigNumberAdd(X, X, BPrivateKey) then
      Exit;

    T := TCnBigNumber.Create;
    if not BigNumberNonNegativeMod(T, X, SM2.Order) then // T = (BPrivateKey + 随机值 * X2) mod N
      Exit;

    BigNumberCopy(X, InRA.X);
    BuildShortXValue(X, SM2.Order);

    // 计算 XV YV。 (h * t) * (APublicKey + X * RA)
    V := TCnEccPoint.Create;
    V.Assign(InRA);
    SM2.MultiplePoint(X, V);
    SM2.PointAddPoint(V, APublicKey, V);
    SM2.MultiplePoint(T, V);

    if V.X.IsZero or V.Y.IsZero then // 如果是无穷远点则协商失败
    begin
      _CnSetLastError(ECN_SM2_KEYEXCHANGE_INFINITE_ERROR);
      Exit;
    end;

    // 协商初步成功，计算 KB
    Za := CalcSM2UserHash(AUserID, APublicKey, SM2);
    Zb := CalcSM2UserHash(BUserID, BPublicKey, SM2);
    OutKeyB := CalcSM2ExchangeKey(V, Za, Zb, KeyByteLength); // 共享密钥协商成功！

    // 然后计算 SB 供 A 核对
    OutOptionalSB := CalcSM2OptionalSig(V, InRA, OutRB, Za, Zb, True);

    // 顺便计算 S2 等 A 发来 SA 时核对
    OutOptionalS2 := CalcSM2OptionalSig(V, InRA, OutRB, Za, Zb, False);
    Result := True;
  finally
    V.Free;
    T.Free;
    X.Free;
    R.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2KeyExchangeAStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  APrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey; MyRA, InRB: TCnEccPoint;
  MyARand: TCnBigNumber; out OutKeyA: AnsiString; InOptionalSB: TSM3Digest;
  out OutOptionalSA: TSM3Digest; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
  X, T: TCnBigNumber;
  U: TCnEccPoint;
  Za, Zb: TSM3Digest;
begin
  Result := False;
  if (KeyByteLength <= 0) or (APrivateKey = nil) or (APublicKey = nil) or
    (BPublicKey = nil) or (MyRA = nil) or (InRB = nil) or (MyARand = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;
  X := nil;
  T := nil;
  U := nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not SM2.IsPointOnCurve(InRB) then // 验证传过来的 RB 是否满足方程
      Exit;

    X := TCnBigNumber.Create;
    BigNumberCopy(X, MyRA.X);
    BuildShortXValue(X, SM2.Order);     // 从 RA 里整出 X1

    if not BigNumberMul(X, MyARand, X) then
      Exit;
    if not BigNumberAdd(X, X, APrivateKey) then
      Exit;

    T := TCnBigNumber.Create;
    if not BigNumberNonNegativeMod(T, X, SM2.Order) then // T = (APrivateKey + 随机值 * X1) mod N
      Exit;

    BigNumberCopy(X, InRB.X);
    BuildShortXValue(X, SM2.Order);

    // 计算 XU YU。 (h * t) * (BPublicKey + X * RB)
    U := TCnEccPoint.Create;
    U.Assign(InRB);
    SM2.MultiplePoint(X, U);
    SM2.PointAddPoint(U, BPublicKey, U);
    SM2.MultiplePoint(T, U);

    if U.X.IsZero or U.Y.IsZero then // 如果是无穷远点则协商失败
    begin
      _CnSetLastError(ECN_SM2_KEYEXCHANGE_INFINITE_ERROR);
      Exit;
    end;

    // 协商初步成功，计算 KA
    Za := CalcSM2UserHash(AUserID, APublicKey, SM2);
    Zb := CalcSM2UserHash(BUserID, BPublicKey, SM2);
    OutKeyA := CalcSM2ExchangeKey(U, Za, Zb, KeyByteLength); // 共享密钥协商成功！

    // 然后计算 SB 核对
    OutOptionalSA := CalcSM2OptionalSig(U, MyRA, InRB, Za, Zb, True);
    if not CompareMem(@OutOptionalSA[0], @InOptionalSB[0], SizeOf(TSM3Digest)) then
      Exit;

    // 然后计算 SA 供 B 核对
    OutOptionalSA := CalcSM2OptionalSig(U, MyRA, InRB, Za, Zb, False);
    Result := True;
  finally
    U.Free;
    T.Free;
    X.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2KeyExchangeBStep2(const AUserID, BUserID: AnsiString; KeyByteLength: Integer;
  BPrivateKey: TCnSM2PrivateKey; APublicKey, BPublicKey: TCnSM2PublicKey;
  InOptionalSA: TSM3Digest; MyOptionalS2: TSM3Digest; SM2: TCnSM2): Boolean;
begin
  Result := CompareMem(@InOptionalSA[0], @MyOptionalS2[0], SizeOf(TSM3Digest));
end;

{
  随机取 r
  点 R <= r * G
  数 c <= Hash(PublicKey, R)
  数 z <= r + c * PrivateKey
}
function CnSM2SchnorrProve(PrivateKey: TCnSM2PrivateKey; PublicKey: TCnSM2PublicKey;
  OutR: TCnEccPoint; OutZ: TCnBigNumber; SM2: TCnSM2): Boolean;
var
  R: TCnBigNumber;
  Dig: TSM3Digest;
  SM2IsNil: Boolean;
  Stream: TMemoryStream;
begin
  Result := False;
  if (PrivateKey = nil) or (PublicKey = nil) or (OutR = nil) or (OutZ = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  R := nil;
  Stream := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    R := TCnBigNumber.Create;
    if not BigNumberRandBytes(R, CN_SM2_FINITEFIELD_BYTESIZE) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;

    OutR.Assign(SM2.Generator);
    SM2.MultiplePoint(R, OutR);

    Stream := TMemoryStream.Create;
    if CnEccPointToStream(PublicKey, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
      Exit;

    if CnEccPointToStream(OutR, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
      Exit;

    Dig := SM3(Stream.Memory, Stream.Size);

    OutZ.SetBinary(@Dig[0], SizeOf(TSM3Digest));

    // 注意，此处无需也不能 mod P！
    if not BigNumberMul(OutZ, OutZ, PrivateKey) then
      Exit;

    if not BigNumberAdd(OutZ, OutZ, R) then
      Exit;

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    Stream.Free;
    R.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

{
  数 c <= Hash(PublicKey, R)
  点 z * G ?= R + c * PublicKey
}
function CnSM2SchnorrCheck(PublicKey: TCnSM2PublicKey; InR: TCnEccPoint;
  InZ: TCnBigNumber; SM2: TCnSM2): Boolean;
var
  C: TCnBigNumber;
  Dig: TSM3Digest;
  SM2IsNil: Boolean;
  Stream: TMemoryStream;
  P1, P2: TCnEccPoint;
begin
  Result := False;
  if (PublicKey = nil) or (InR = nil) or (InZ = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  Stream := nil;
  C := nil;
  P1 := nil;
  P2 := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Stream := TMemoryStream.Create;
    if CnEccPointToStream(PublicKey, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
      Exit;

    if CnEccPointToStream(InR, Stream, CN_SM2_FINITEFIELD_BYTESIZE) <= 0 then
      Exit;

    Dig := SM3(Stream.Memory, Stream.Size);

    C := TCnBigNumber.Create;
    C.SetBinary(@Dig[0], SizeOf(TSM3Digest));

    P1 := TCnEccPoint.Create;
    P1.Assign(SM2.Generator);
    SM2.MultiplePoint(InZ, P1);

    P2 := TCnEccPoint.Create;
    P2.Assign(PublicKey);
    SM2.MultiplePoint(C, P2);
    SM2.PointAddPoint(P2, InR, P2);

    Result := CnEccPointsEqual(P1, P2);
    _CnSetLastError(ECN_SM2_OK);
  finally
    P2.Free;
    P1.Free;
    C.Free;
    Stream.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

// ========== SM2 椭圆曲线双方互相信任的简易协同算法之协同密钥生成 =============

function CnSM2CollaborativeGenerateKeyAStep1(PrivateKeyA: TCnSM2CollaborativePrivateKey;
  PointToB: TCnEccPoint; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyA = nil) or (PointToB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(PrivateKeyA, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if PrivateKeyA.IsZero then // 万一真拿到 0，就加 1
      PrivateKeyA.SetOne;

    PointToB.Assign(SM2.Generator);
    SM2.MultiplePoint(PrivateKeyA, PointToB); // 基点乘 PrivateKeyA 次给 B

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeGenerateKeyBStep1(PrivateKeyB: TCnSM2CollaborativePrivateKey;
  InPointFromA: TCnEccPoint; PublicKey: TCnSM2CollaborativePublicKey; SM2: TCnSM2): Boolean;
var
  P: TCnEccPoint;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InPointFromA = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  P := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    if not BigNumberRandRange(PrivateKeyB, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if PrivateKeyB.IsZero then // 万一真拿到 0，就加 1
      PrivateKeyB.SetOne;

    PublicKey.Assign(InPointFromA);
    SM2.MultiplePoint(PrivateKeyB, PublicKey); // 得到的 PublicKey 还要减 G

    P := TCnEccPoint.Create;
    P.Assign(SM2.Generator);
    SM2.PointInverse(P);
    SM2.PointAddPoint(PublicKey, P, PublicKey);

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    P.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

// =============== SM2 椭圆曲线双方互相信任的简易协同签名算法 ==================

function CnSM2CollaborativeSignAStep1(const UserID: AnsiString; PlainData: Pointer;
  DataLen: Integer; OutHashEToB: TCnBigNumber; OutQToB: TCnEccPoint; OutRandK: TCnBigNumber;
  PrivateKeyA: TCnSM2CollaborativePrivateKey; PublicKey: TCnSM2PublicKey; SM2: TCnSM2): Boolean;
var
  Sm3Dig: TSM3Digest;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyA = nil) or (OutHashEToB = nil) or (OutQToB = nil) or
    (OutRandK = nil) or (PublicKey = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Sm3Dig := CalcSM2SignatureHash(UserID, PlainData, DataLen, PublicKey, SM2); // 杂凑值 e 要给 B
    OutHashEToB.SetBinary(@Sm3Dig[0], SizeOf(TSM3Digest));

    if not BigNumberRandRange(OutRandK, SM2.Order) then
    begin
      _CnSetLastError(ECN_SM2_RANDOM_ERROR);
      Exit;
    end;
    if OutRandK.IsZero then // 万一真拿到 0，就加 1
      OutRandK.SetOne;

    OutQToB.Assign(SM2.Generator);
    SM2.MultiplePoint(OutRandK, OutQToB); // K 要留着给 A 签名的下一步

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeSignBStep1(InHashEFromA: TCnBigNumber; InQFromA: TCnEccPoint;
  OutRToB, OutS1ToB, OutS2ToB: TCnBigNumber; PrivateKeyB: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2): Boolean;
var
  K1, K2, Inv: TCnBigNumber;
  P, Q: TCnEccPoint;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InHashEFromA = nil) or (InQFromA = nil)
    or (OutRToB = nil) or (OutS1ToB = nil) or (OutS2ToB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  K1 := nil;
  K2 := nil;
  Q := nil;
  P := nil;
  Inv := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    K1 := TCnBigNumber.Create;
    K2 := TCnBigNumber.Create;
    Q := TCnEccPoint.Create;
    P := TCnEccPoint.Create;
    Inv := TCnBigNumber.Create;

    while True do
    begin
      if not BigNumberRandRange(K1, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;
      if K1.IsZero then // 万一真拿到 0，就加 1
        K1.SetOne;

      Q.Assign(SM2.Generator);
      SM2.MultiplePoint(K1, Q); // 先计算出一个自己的 Q 点

      // 再生成一次随机 K
      if not BigNumberRandRange(K2, SM2.Order) then
      begin
        _CnSetLastError(ECN_SM2_RANDOM_ERROR);
        Exit;
      end;
      if K2.IsZero then // 万一真拿到 0，就加 1
        K2.SetOne;

      P.Assign(InQFromA);
      SM2.MultiplePoint(K2, P);   // 对方的 Q 点乘以自己的 K
      SM2.PointAddPoint(P, Q, Q); // 再加上自己的 Q

      // r = (Q.x + e) mod N
      if not BigNumberAddMod(OutRToB, Q.X, InHashEFromA, SM2.Order) then
        Exit;

      if OutRToB.IsZero then
        Continue;

      if not BigNumberModularInverse(Inv, PrivateKeyB, SM2.Order) then
        Exit;

      if not BigNumberDirectMulMod(OutS1ToB, Inv, K2, SM2.Order) then // 算出 s1 = k2 / PrivateKeyB
        Exit;

      if not BigNumberAddMod(K1, K1, OutRToB, SM2.Order) then // K1 + r
        Exit;

      if not BigNumberDirectMulMod(OutS2ToB, K1, Inv, SM2.Order) then // K1 + r / PrivateKeyB
        Exit;

      Result := True;
      _CnSetLastError(ECN_SM2_OK);

      Break;
    end;
  finally
    Inv.Free;
    P.Free;
    Q.Free;
    K2.Free;
    K1.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeSignAStep2(InRandK, InRFromB, InS1FromB, InS2FromB: TCnBigNumber;
  OutSignature: TCnSM2Signature; PrivateKeyA: TCnSM2CollaborativePrivateKey; SM2: TCnSM2): Boolean;
var
  Inv, T: TCnBigNumber;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyA = nil) or (OutSignature = nil) or
    (InRFromB = nil) or (InS1FromB = nil) or (InS2FromB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  Inv := nil;
  T := nil;
  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    Inv := TCnBigNumber.Create;
    if not BigNumberModularInverse(Inv, PrivateKeyA, SM2.Order) then
      Exit;

    T := TCnBigNumber.Create;
    if not BigNumberDirectMulMod(T, Inv, InS2FromB, SM2.Order) then // T := S2 / PrivateKeyA
      Exit;

    if not BigNumberDirectMulMod(OutSignature.S, InRandK, Inv, SM2.Order) then // K / PrivateKeyA
      Exit;

    if not BigNumberDirectMulMod(OutSignature.S, OutSignature.S, InS1FromB, SM2.Order) then // K * S1 / PrivateKeyA
      Exit;

    if not BigNumberAddMod(OutSignature.S, OutSignature.S, T, SM2.Order) then
      Exit;

    if not BigNumberSubMod(OutSignature.S, OutSignature.S, InRFromB, SM2.Order) then
      Exit;

    if not OutSignature.S.IsZero then
    begin
      if not BigNumberAdd(T, OutSignature.S, InRFromB) then
        Exit;

      if not BigNumberEqual(T, SM2.Order) then
      begin
        if BigNumberCopy(OutSignature.R, InRFromB) = nil then
          Exit;
      end;

      Result := True;
      _CnSetLastError(ECN_SM2_OK);
    end;
  finally
    T.Free;
    Inv.Free;
    if SM2IsNil then
      SM2.Free;
  end;
end;

// =============== SM2 椭圆曲线双方互相信任的简易协同解密算法 ==================

function CnSM2CollaborativeDecryptAStep1(EnData: Pointer; DataLen: Integer;
  OutTToB: TCnEccPoint; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (EnData = nil) or (DataLen <= 0) or (PrivateKeyA = nil)
    or (OutTToB = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    MLen := DataLen - SizeOf(TSM3Digest) - (SM2.BitsCount div 4);
    if MLen <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    M := PAnsiChar(EnData);
    if M^ = #$04 then  // 跳过可能的前导字节 $04
    begin
      Dec(MLen);
      if MLen <= 0 then
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
        Exit;
      end;

      Inc(M);
    end;

    // 读出 C1
    OutTToB.X.SetBinary(M, SM2.BitsCount div 8);
    Inc(M, SM2.BitsCount div 8);
    OutTToB.Y.SetBinary(M, SM2.BitsCount div 8);
    if OutTToB.IsZero then
    begin
      _CnSetLastError(ECN_SM2_DECRYPT_INFINITE_ERROR);
      Exit;
    end;

    SM2.MultiplePoint(PrivateKeyA, OutTToB); // C1 点乘私钥发给 B

    Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeDecryptBStep1(InTFromA: TCnEccPoint; OutTToA: TCnEccPoint;
  PrivateKeyB: TCnSM2CollaborativePrivateKey; SM2: TCnSM2): Boolean;
var
  SM2IsNil: Boolean;
begin
  Result := False;
  if (PrivateKeyB = nil) or (InTFromA = nil) or (OutTToA = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    OutTToA.Assign(InTFromA);
    SM2.MultiplePoint(PrivateKeyB, OutTToA);

     Result := True;
    _CnSetLastError(ECN_SM2_OK);
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

function CnSM2CollaborativeDecryptAStep2(EnData: Pointer; DataLen: Integer;
  InTFromB: TCnEccPoint; OutStream: TStream; PrivateKeyA: TCnSM2CollaborativePrivateKey;
  SM2: TCnSM2; SequenceType: TCnSM2CryptSequenceType): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  MP: AnsiString;
  KDFStr, T, C3H: AnsiString;
  P2: TCnEccPoint;
  I, PrefixLen: Integer;
  Sm3Dig: TSM3Digest;
  SM2IsNil: Boolean;
begin
  Result := False;
  if (EnData = nil) or (DataLen <= 0) or (PrivateKeyA = nil)
    or (InTFromB = nil) or (OutStream = nil) then
  begin
    _CnSetLastError(ECN_SM2_INVALID_INPUT);
    Exit;
  end;

  SM2IsNil := SM2 = nil;

  try
    if SM2IsNil then
      SM2 := TCnSM2.Create;

    MLen := DataLen - SizeOf(TSM3Digest) - (SM2.BitsCount div 4);
    if MLen <= 0 then
    begin
      _CnSetLastError(ECN_SM2_INVALID_INPUT);
      Exit;
    end;

    P2 := TCnEccPoint.Create;
    M := PAnsiChar(EnData);
    if M^ = #$04 then  // 跳过可能的前导字节 $04
    begin
      Dec(MLen);
      if MLen <= 0 then
      begin
        _CnSetLastError(ECN_SM2_INVALID_INPUT);
        Exit;
      end;

      PrefixLen := 1;
      Inc(M);
    end
    else
      PrefixLen := 0;

    // 读出 C1
    P2.X.SetBinary(M, SM2.BitsCount div 8);
    Inc(M, SM2.BitsCount div 8);
    P2.Y.SetBinary(M, SM2.BitsCount div 8);
    if P2.IsZero then
    begin
      _CnSetLastError(ECN_SM2_DECRYPT_INFINITE_ERROR);
      Exit;
    end;

    // P2 <= InTFromB - C1
    SM2.PointSubPoint(InTFromB, P2, P2);

    // 以下同常规解密

    SetLength(KDFStr, P2.X.GetBytesCount + P2.Y.GetBytesCount);
    P2.X.ToBinary(@KDFStr[1]);
    P2.Y.ToBinary(@KDFStr[P2.X.GetBytesCount + 1]);
    T := CnSM2KDF(KDFStr, MLen);

    if SequenceType = cstC1C3C2 then
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, SizeOf(TSM3Digest) + (SM2.BitsCount div 4) + PrefixLen); // 跳过 C3 指向 C2
      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I])); // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + MLen);
      P2.X.ToBinary(@C3H[1]);
      Move(MP[1], C3H[P2.X.GetBytesCount + 1], MLen);
      P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + MLen + 1]);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, (SM2.BitsCount div 4) + PrefixLen);             // M 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TSM3Digest)) then  // 比对 Hash 是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end
    else // C1C2C3 的排列
    begin
      SetLength(MP, MLen);
      M := PAnsiChar(EnData);
      Inc(M, (SM2.BitsCount div 4) + PrefixLen);  // 指向 C2

      for I := 1 to MLen do
        MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I])); // 和 KDF 做异或，在 MP 里得到明文

      SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + MLen);
      P2.X.ToBinary(@C3H[1]);
      Move(MP[1], C3H[P2.X.GetBytesCount + 1], MLen);
      P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + MLen + 1]);    // 拼成算 C3 的
      Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

      M := PAnsiChar(EnData);
      Inc(M, (SM2.BitsCount div 4) + PrefixLen + MLen);      // 指向 C3
      if CompareMem(@Sm3Dig[0], M, SizeOf(TSM3Digest)) then  // 比对 Hash 是否相等
      begin
        OutStream.Write(MP[1], Length(MP));

        Result := True;
        _CnSetLastError(ECN_SM2_OK);
      end;
    end;
  finally
    if SM2IsNil then
      SM2.Free;
  end;
end;

procedure CheckPrePower2Points;
var
  SM2: TCnSM2;
  P, Q: TCnEcc3Point;
  I: Integer;
begin
  if FSM2AffineGPower2KList.Count > 0 then
    Exit;

  SM2 := TCnSM2.Create;
  try
    P := TCnEcc3Point.Create;
    CnEccPointToEcc3Point(SM2.Generator, P);

    FSM2AffineGPower2KList.Add(P);      // 第 0 个是 2 的 0 次方也就是 1 倍就是自身
    for I := 1 to 255 do
    begin
      Q := TCnEcc3Point.Create;
      SM2.AffinePointAddPoint(P, P, Q); // Q 变成 2P
      FSM2AffineGPower2KList.Add(Q);    // 加入列表
      P.Assign(Q);                      // P 变成 2P 准备下次循环
    end;
  finally
    SM2.Free;
  end;
end;

procedure InitSM2;
begin
  FSM2AffineGPower2KList := TObjectList.Create(True);
  CheckPrePower2Points;
end;

procedure FintSM2;
begin
  FSM2AffineGPower2KList.Free;
end;

initialization
  InitSM2;

finalization
  FintSM2;

end.

