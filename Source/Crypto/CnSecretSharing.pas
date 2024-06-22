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

unit CnSecretSharing;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：秘密共享的算法实现单元，目前包括 Shamir 门限方案与 Feldman VSS 方案
* 单元作者：CnPack 开发组
* 备    注：Shamir 门限方案是利用构造多项式生成多个点坐标并利用插值还原点的秘密共享方案
*           Shamir 方案的问题是，拆分后的秘密没有验证是否正确的机制，有改进版的 Feldman VSS
*           后者需要用到素数 P 满足 P = 2*Q + 1 且 Q 也是素数，内嵌的 Shamir 模 Q
*           用于验证的计算则模 P
*           Shamir 门限方案包括 Split 和 Reconstruct 俩场合
*           Feldman VSS 加了验证，包括 Split、Verify 和 Reconstruct 仨场合
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2022.05.28 V1.1
*               在 Shamir 的基础上实现 Feldman's VSS 方案
*           2022.05.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils,
  CnConsts, CnNative, CnPrimeNumber, CnContainers, CnPolynomial, CnRandom,
  CnBigNumber;

const
  CN_SHAMIR_DEFAULT_PRIME_BITS         = 1024;
  {* Shamir 算法的素数默认位数}

  // 错误码
  ECN_SECRET_OK                        = ECN_OK; // 没错
  ECN_SECRET_ERROR_BASE                = ECN_CUSTOM_ERROR_BASE + $400; // Secret Sharing 错误码基准

  ECN_SECRET_INVALID_INPUT             = ECN_SECRET_ERROR_BASE + 1;    // 输入为空或长度不对
  ECN_SECRET_RANDOM_ERROR              = ECN_SECRET_ERROR_BASE + 2;    // 随机数相关错误
  ECN_SECRET_FELDMAN_CHECKERROR        = ECN_SECRET_ERROR_BASE + 3;    // Feldman VSS 检查错误
  ECN_SECRET_PRIME_ERROR               = ECN_SECRET_ERROR_BASE + 4;    // 素数错误

//==============================================================================
//
//  RFC 7919 中推荐的 Diffie-Hellman 使用的素数，减一除 2 也是素数，生成元均为 2
//  注意此处的生成元 2 并不符合原根的 G^q mod p <> 1 的要求，
//  相反，竟然等于 1，满足 Feldman VSS 的要求
//
//==============================================================================

  CN_PRIME_FFDHE_2048 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B423861285C97FFFFFFFFFFFFFFFF';

  CN_PRIME_FFDHE_3072 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B66C62E37FFFFFFFFFFFFFFFF';

  CN_PRIME_FFDHE_4096 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB' +
    '7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004' +
    '87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832' +
    'A907600A918130C46DC778F971AD0038092999A333CB8B7A' +
    '1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF' +
    '8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E655F6A' +
    'FFFFFFFFFFFFFFFF';

  CN_PRIME_FFDHE_6144 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB' +
    '7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004' +
    '87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832' +
    'A907600A918130C46DC778F971AD0038092999A333CB8B7A' +
    '1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF' +
    '8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E0DD902' +
    '0BFD64B645036C7A4E677D2C38532A3A23BA4442CAF53EA6' +
    '3BB454329B7624C8917BDD64B1C0FD4CB38E8C334C701C3A' +
    'CDAD0657FCCFEC719B1F5C3E4E46041F388147FB4CFDB477' +
    'A52471F7A9A96910B855322EDB6340D8A00EF092350511E3' +
    '0ABEC1FFF9E3A26E7FB29F8C183023C3587E38DA0077D9B4' +
    '763E4E4B94B2BBC194C6651E77CAF992EEAAC0232A281BF6' +
    'B3A739C1226116820AE8DB5847A67CBEF9C9091B462D538C' +
    'D72B03746AE77F5E62292C311562A846505DC82DB854338A' +
    'E49F5235C95B91178CCF2DD5CACEF403EC9D1810C6272B04' +
    '5B3B71F9DC6B80D63FDD4A8E9ADB1E6962A69526D43161C1' +
    'A41D570D7938DAD4A40E329CD0E40E65FFFFFFFFFFFFFFFF';

  CN_PRIME_FFDHE_8192 =
    'FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1' +
    'D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9' +
    '7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561' +
    '2433F51F5F066ED0856365553DED1AF3B557135E7F57C935' +
    '984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735' +
    '30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB' +
    'B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19' +
    '0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61' +
    '9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73' +
    '3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA' +
    '886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238' +
    '61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C' +
    'AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3' +
    '64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D' +
    'ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF' +
    '3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB' +
    '7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004' +
    '87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832' +
    'A907600A918130C46DC778F971AD0038092999A333CB8B7A' +
    '1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF' +
    '8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E0DD902' +
    '0BFD64B645036C7A4E677D2C38532A3A23BA4442CAF53EA6' +
    '3BB454329B7624C8917BDD64B1C0FD4CB38E8C334C701C3A' +
    'CDAD0657FCCFEC719B1F5C3E4E46041F388147FB4CFDB477' +
    'A52471F7A9A96910B855322EDB6340D8A00EF092350511E3' +
    '0ABEC1FFF9E3A26E7FB29F8C183023C3587E38DA0077D9B4' +
    '763E4E4B94B2BBC194C6651E77CAF992EEAAC0232A281BF6' +
    'B3A739C1226116820AE8DB5847A67CBEF9C9091B462D538C' +
    'D72B03746AE77F5E62292C311562A846505DC82DB854338A' +
    'E49F5235C95B91178CCF2DD5CACEF403EC9D1810C6272B04' +
    '5B3B71F9DC6B80D63FDD4A8E9ADB1E6962A69526D43161C1' +
    'A41D570D7938DAD4A40E329CCFF46AAA36AD004CF600C838' +
    '1E425A31D951AE64FDB23FCEC9509D43687FEB69EDD1CC5E' +
    '0B8CC3BDF64B10EF86B63142A3AB8829555B2F747C932665' +
    'CB2C0F1CC01BD70229388839D2AF05E454504AC78B758282' +
    '2846C0BA35C35F5C59160CC046FD8251541FC68C9C86B022' +
    'BB7099876A460E7451A8A93109703FEE1C217E6C3826E52C' +
    '51AA691E0E423CFC99E9E31650C1217B624816CDAD9A95F9' +
    'D5B8019488D9C0A0A1FE3075A577E23183F81D4A3F2FA457' +
    '1EFC8CE0BA8A4FE8B6855DFE72B0A66EDED2FBABFBE58A30' +
    'FAFABE1C5D71A87E2F741EF8C1FE86FEA6BBFDE530677F0D' +
    '97D11D49F7A8443D0822E506A9F4614E011E2A94838FF88C' +
    'D68C8BB7C5C6424CFFFFFFFFFFFFFFFF';

type
  TCnFeldmanVssPiece = class
  {* FeldmanVss 分片实现类}
  private
    FOrder: TCnBigNumber;
    FShare: TCnBigNumber;
    FCommitments: TCnBigNumberList;
    function GetCommitmenet(Index: Integer): TCnBigNumber;
    function GetCommitmentCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Order: TCnBigNumber read FOrder;
    {* 自变量 X}
    property Share: TCnBigNumber read FShare;
    {* 对应的秘密分片}
    property CommitmentCount: Integer read GetCommitmentCount;
    {* 公开的承诺列表数量}
    property Commitments[Index: Integer]: TCnBigNumber read GetCommitmenet;
    {* 公开的承诺列表}
  end;

// ====================== Shamir 门限方案实现秘密共享 ==========================

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

// =============== Feldman's VSS 扩展 Shamir 门限方案实现秘密共享 ==============

function CnInt64FeldmanVssGeneratePrime(out Prime, Generator: Int64): Boolean;
{* 生成 Feldman VSS 所需的素数和生成元，返回是否生成成功。（注意不同于 DH 的要求）
   其中素数（大）减一的一半也是素数（小），生成元的小素数幂模大素数值为 1}

function CnInt64FeldmanVssSplit(Secret: Int64; ShareCount, Threshold: Integer;
  OutShares, OutCommitments: TCnInt64List; var Prime, Generator: Int64): Boolean;
{* Feldman 扩展的 Shamir 门限方案实现 Int64 的秘密共享。
  将一 Int64 值拆分为 ShareCount 个 Int64 值，并生成验证值（Commitment）
  只需要其中 Threshold 个值及其顺序就能还原 Secret，返回是否拆分成功。
  拆分值放 OutShares 中，对应顺序值为其下标 + 1（如第 0 项对应 1）
  拆分的 Share 值可通过 CnInt64FeldmanVssVerify 用公开的验证值来验证其是否合法
  相关素数可以在 Prime 中指定，如为 0，则生成符合要求的素数值返回
  注意内部封装的 Shamir 使用的素数是 (Prime - 1) / 2，并非 Prime}

function CnInt64FeldmanVssVerify(Prime, Generator: Int64; InOrder, InShare: Int64;
  Commitments: TCnInt64List): Boolean;
{* Feldman 扩展的 Shamir 门限方案实现 Int64 的秘密共享，返回验证是否通过。
  用公开的验证值来验证某序号的秘密分片是否合法}

function CnInt64FeldmanVssReconstruct(Prime, Generator: Int64; InOrders, InShares,
  Commitments: TCnInt64List; out Secret: Int64; Verify: Boolean = True): Boolean;
{* Feldman 扩展的 Shamir 门限方案实现 Int64 的秘密共享。Verify 表示重组前是否验证
  将 Threshold 个拆分后的 Int64 值与其对应序号并结合大素数重组成 Secret，
  返回是否重组成功，成功则秘密值放 Secret 中返回。}

function CnFeldmanVssSplit(Secret: TCnBigNumber; ShareCount, Threshold: Integer;
  OutOrders, OutShares, OutCommitments: TCnBigNumberList; Prime, Generator: TCnBigNumber): Boolean;
{* Feldman 扩展的 Shamir 门限方案实现大数范围内的秘密共享，
  将 Threshold 个拆分后的大数值与其对应序号并结合，并生成验证值（Commitment），返回拆分是否成功
  拆分出的值可通过 CnFeldmanVssVerify 用公开的验证值来验证其是否合法
  相关素数因生成困难，须由外界传入，可使用 CN_PRIME_FFDHE_* 系列符合 2q + 1 的素数，
  生成元均为 2，注意素数须比 Secret 大，且内部封装的 Shamir 使用的素数是 (Prime - 1) / 2，并非 Prime
  拆分后，Orders 与 Shares 一一对应各自分配给秘密持有方，Commitments 和 Prime、Generator 则全部公开}

function CnFeldmanVssVerify(Prime, Generator: TCnBigNumber; InOrder, InShare: TCnBigNumber;
  Commitments: TCnBigNumberList): Boolean;
{* Feldman 扩展的 Shamir 门限方案实现大数范围内的秘密共享分片数据验证，返回验证是否通过。
  用公开的验证值以及 Prime、Generator 来验证某序号的秘密分片是否合法}

function CnFeldmanVssReconstruct(Prime, Generator: TCnBigNumber; InOrders, InShares,
  Commitments: TCnBigNumberList; OutSecret: TCnBigNumber; Verify: Boolean = True): Boolean;
{* Feldman 扩展的 Shamir 门限方案实现大数范围内的秘密共享。Verify 表示重组前是否验证
  将 Threshold 个拆分后的大数值与其对应序号并结合大素数重组成 Secret，返回是否重组成功。
  成功则秘密值放 Secret 中返回}

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
    begin
      Poly[I] := RandomInt64LessThan(Prime);
      if Poly[I] = 0 then  // 避免出现 0 系数造成负面影响
        Poly[I] := 1;
    end;

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
    begin
      _CnSetLastError(ECN_SECRET_PRIME_ERROR);
      Exit;
    end;
  end;

  Poly := nil;
  T := nil;

  try
    Poly := TCnBigNumberPolynomial.Create;

    Poly.MaxDegree := Threshold - 1;
    if BigNumberCopy(Poly[0], Secret) = nil then
      Exit;

    for I := 1 to Poly.MaxDegree do
    begin
      if not BigNumberRandRange(Poly[I], Prime) then
      begin
        _CnSetLastError(ECN_SECRET_RANDOM_ERROR);
        Exit;
      end;

      if Poly[I].IsZero then // 避免系数出现 0
        Poly[I].SetOne;
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

      if not BigNumberDirectMulMod(N, T, N, Prime) then
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

function CnInt64FeldmanVssGeneratePrime(out Prime, Generator: Int64): Boolean;
var
  I: Integer;
  Q: TUInt64;
begin
  repeat
    Prime := CnGenerateInt64Prime2;
    Q := (Prime - 1) shr 1;

    if not CnInt64IsPrime(Q) then
      Continue;

    for I := 2 to MaxInt do
    begin
      if MontgomeryPowerMod(I, Q, Prime) = 1 then
      begin
        Generator := I;
        Result := True;
        Exit;
      end;
    end;
  until False;
end;

function CnInt64FeldmanVssSplit(Secret: Int64; ShareCount, Threshold: Integer;
  OutShares, OutCommitments: TCnInt64List; var Prime, Generator: Int64): Boolean;
var
  Poly: TCnInt64Polynomial;
  Q: Int64;
  I: Integer;
begin
  Result := False;
  if (Secret < 0) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if (Prime = 0) or (Generator = 0) then // 0 表示要重新生成
  begin
    if not CnInt64FeldmanVssGeneratePrime(Prime, Generator) then
      Exit;

    if Secret > Prime then
    begin
      _CnSetLastError(ECN_SECRET_INVALID_INPUT);
      Exit;
    end;
  end;
  Q := (Prime - 1) shr 1;

  // 开始照搬 Shamir 门限方案拆分，但拆分用 Q，算 Commits 用 Prime
  Poly := nil;
  try
    Poly := TCnInt64Polynomial.Create;

    Poly.MaxDegree := Threshold - 1;
    Poly[0] := Secret;
    for I := 1 to Poly.MaxDegree do
    begin
      Poly[I] := RandomInt64LessThan(Q);
      if Poly[I] = 0 then  // 避免出现 0 系数造成负面影响
        Poly[I] := 1;
    end;

    // 生成了随机多项式
    OutShares.Clear;
    for I := 1 to ShareCount do
      OutShares.Add(Int64PolynomialGaloisGetValue(Poly, I, Q));

    // 生成系数的承诺
    OutCommitments.Clear;
    for I := 1 to Threshold do
      OutCommitments.Add(MontgomeryPowerMod(Generator, Poly[I - 1], Prime));

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    Poly.Free;
  end;
end;

function CnInt64FeldmanVssVerify(Prime, Generator: Int64; InOrder, InShare: Int64;
  Commitments: TCnInt64List): Boolean;
var
  I: Integer;
  L, R, T: Int64;
begin
  // 验证某分片是否正确，先计算 Generator^InShare mod Prime
  L := MontgomeryPowerMod(Generator, InShare, Prime);

  // 再计算多项的积，每项序数 I，是 Commitments[I]^(InOrder^I) mod Prime
  R := 1;
  for I := 0 to Commitments.Count - 1 do
  begin
    T := PowerPowerMod(Commitments[I], InOrder, I, Prime);
    R := MultipleMod(R, T, Prime);
  end;

  Result := L = R;
  _CnSetLastError(ECN_SECRET_OK);
end;

function CnInt64FeldmanVssReconstruct(Prime, Generator: Int64; InOrders, InShares,
  Commitments: TCnInt64List; out Secret: Int64; Verify: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Prime < 2) or (Generator < 2) or (InOrders.Count <> InShares.Count)
    or (InOrders.Count < 2) or (Commitments.Count <= 1) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Verify then
  begin
    for I := 0 to InOrders.Count - 1 do
    begin
      if not CnInt64FeldmanVssVerify(Prime, Generator, InOrders[I],
        InShares[I], Commitments) then
      begin
        _CnSetLastError(ECN_SECRET_FELDMAN_CHECKERROR);
        Exit;
      end;
    end;
  end;

  Result := CnInt64ShamirReconstruct((Prime - 1) shr 1, InOrders, InShares, Secret);
end;

function CnFeldmanVssSplit(Secret: TCnBigNumber; ShareCount, Threshold: Integer;
  OutOrders, OutShares, OutCommitments: TCnBigNumberList; Prime, Generator: TCnBigNumber): Boolean;
var
  Poly: TCnBigNumberPolynomial;
  T, Q: TCnBigNumber;
  I: Integer;
begin
  Result := False;

  if (Secret.IsNegative) or (ShareCount < 3) or (Threshold < 2) or (ShareCount < Threshold) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Prime.IsZero or Prime.IsNegative or Generator.IsZero or Generator.IsNegative or
    (BigNumberCompare(Prime, Secret) <= 0) then // 如果素数与生成元不对，则出错退出
  begin
    _CnSetLastError(ECN_SECRET_PRIME_ERROR);
    Exit;
  end;

  Q := nil;
  Poly := nil;
  T := nil;

  try
    Q := TCnBigNumber.Create;
    if BigNumberCopy(Q, Prime) = nil then
      Exit;
    Q.SubWord(1);
    Q.ShiftRightOne;

    Poly := TCnBigNumberPolynomial.Create;
    Poly.MaxDegree := Threshold - 1;
    if BigNumberCopy(Poly[0], Secret) = nil then
      Exit;

    for I := 1 to Poly.MaxDegree do
    begin
      if not BigNumberRandRange(Poly[I], Q) then
      begin
        _CnSetLastError(ECN_SECRET_RANDOM_ERROR);
        Exit;
      end;

      if Poly[I].IsZero then // 避免系数出现 0
        Poly[I].SetOne;
    end;

    // 生成了随机多项式，用 1 到 ShareCount 代入分别求值
    OutOrders.Clear;
    OutShares.Clear;

    T := TCnBigNumber.Create;
    for I := 1 to ShareCount do
    begin
      OutOrders.Add.SetWord(I);
      T.SetWord(I);
      if not BigNumberPolynomialGaloisGetValue(OutShares.Add, Poly, T, Q) then
        Exit;
    end;

    // 生成系数的承诺
    OutCommitments.Clear;
    for I := 1 to Threshold do
    begin
      if not BigNumberPowerMod(OutCommitments.Add, Generator, Poly[I - 1], Prime) then
        Exit;
    end;

    Result := True;
    _CnSetLastError(ECN_SECRET_OK);
  finally
    T.Free;
    Poly.Free;
    Q.Free;
  end;
end;

function CnFeldmanVssVerify(Prime, Generator: TCnBigNumber; InOrder, InShare: TCnBigNumber;
  Commitments: TCnBigNumberList): Boolean;
var
  I: Integer;
  L, R, T, D: TCnBigNumber;
begin
  Result := False;

  // 验证某分片是否正确，先计算 Generator^InShare mod Prime
  L := nil;
  R := nil;
  T := nil;
  D := nil;

  try
    L := TCnBigNumber.Create;
    if not BigNumberPowerMod(L, Generator, InShare, Prime) then
      Exit;

    // 再计算多项的积，每项序数 I，是 Commitments[I]^(InOrder^I) mod Prime
    R := TCnBigNumber.Create;
    R.SetOne;

    T := TCnBigNumber.Create;
    D := TCnBigNumber.Create;

    for I := 0 to Commitments.Count - 1 do
    begin
      D.SetWord(I);
      if not BigNumberPowerPowerMod(T, Commitments[I], InOrder, D, Prime) then
        Exit;

      if not BigNumberDirectMulMod(R, R, T, Prime) then
        Exit;
    end;

    Result := BigNumberEqual(L, R);
    _CnSetLastError(ECN_SECRET_OK);
  finally
    D.Free;
    T.Free;
    R.Free;
    L.Free;
  end;
end;

function CnFeldmanVssReconstruct(Prime, Generator: TCnBigNumber; InOrders, InShares,
  Commitments: TCnBigNumberList; OutSecret: TCnBigNumber; Verify: Boolean = True): Boolean;
var
  I: Integer;
  Q: TCnBigNumber;
begin
  Result := False;
  if Prime.IsZero or Prime.IsNegative or Generator.IsZero or Generator.IsNegative
    or (InOrders.Count <> InShares.Count) or (InOrders.Count < 2) or (Commitments.Count <= 1) then
  begin
    _CnSetLastError(ECN_SECRET_INVALID_INPUT);
    Exit;
  end;

  if Verify then
  begin
    for I := 0 to InOrders.Count - 1 do
    begin
      if not CnFeldmanVssVerify(Prime, Generator, InOrders[I],
        InShares[I], Commitments) then
      begin
        _CnSetLastError(ECN_SECRET_FELDMAN_CHECKERROR);
        Exit;
      end;
    end;
  end;

  Q := TCnBigNumber.Create;
  try
    if BigNumberCopy(Q, Prime) = nil then
      Exit;
    Q.SubWord(1);
    Q.ShiftRightOne;

    Result := CnShamirReconstruct(Q, InOrders, InShares, OutSecret);
  finally
    Q.Free;
  end;
end;

{ TCnFeldmanVssPiece }

constructor TCnFeldmanVssPiece.Create;
begin
  inherited;
  FOrder := TCnBigNumber.Create;
  FShare := TCnBigNumber.Create;
  FCommitments := TCnBigNumberList.Create;
end;

destructor TCnFeldmanVssPiece.Destroy;
begin
  FCommitments.Free;
  FShare.Free;
  FOrder.Free;
  inherited;
end;

function TCnFeldmanVssPiece.GetCommitmenet(Index: Integer): TCnBigNumber;
begin
  Result := FCommitments[Index];
end;

function TCnFeldmanVssPiece.GetCommitmentCount: Integer;
begin
  Result := FCommitments.Count;
end;

end.
