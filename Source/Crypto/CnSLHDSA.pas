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

unit CnSLHDSA;
{* |<PRE>
================================================================================
* 单元名称：SLH-DSA 无状态杂凑数字签名算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 NIST FIPS 205 规范中的 SLH-DSA
*           （Stateless Hash-Based Digital Signature Algorithm），
*           即抗量子的无状态杂凑数字签名算法（原 SPHINCS+）。
*
* 开发平台：Win10 + Delphi 12.0
* 内容测试：尚未测试
* 说    明：本单元使用 SHA2/SHA3 等底层杂凑，需 CnSHA2 和 CnSHA3 等单元。
* 修改记录：2026.05.18 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes,
  CnSHA2, CnSHA3, CnSM3, CnRandom, CnBits, CnNative;

const
  // 地址类型常量
  CN_SLH_ADRS_WOTS_HASH  = 0;
  {* ADRS 地址类型：WOTS+ 杂凑}
  CN_SLH_ADRS_WOTS_PK    = 1;
  {* ADRS 地址类型：WOTS+ 公钥}
  CN_SLH_ADRS_TREE       = 2;
  {* ADRS 地址类型：XMSS 树}
  CN_SLH_ADRS_FORS_TREE  = 3;
  {* ADRS 地址类型：FORS 树}
  CN_SLH_ADRS_FORS_ROOTS = 4;
  {* ADRS 地址类型：FORS 根压缩}
  CN_SLH_ADRS_WOTS_PRF   = 5;
  {* ADRS 地址类型：WOTS+ 密钥生成}
  CN_SLH_ADRS_FORS_PRF   = 6;
  {* ADRS 地址类型：FORS 密钥生成}

  // 地址偏移常量
  // Bytes 0-3: Layer (4 bytes)
  // Bytes 4-15: Tree address (12 bytes, value right-aligned at bytes 8-15)
  // Bytes 16-19: Type (4 bytes)
  // Bytes 20-23: Word 4 (key pair / tree index)
  // Bytes 24-27: Word 5 (chain address / tree height)
  // Bytes 28-31: Word 6 (hash address / tree index)
  CN_ADRS_OFFSET_LAYER    = 0;
  {* ADRS 层地址偏移（4 字节）}
  CN_ADRS_OFFSET_TREE     = 8;
  {* ADRS 树地址偏移（8 字节，在 12 字节字段中右对齐）}
  CN_ADRS_OFFSET_TYPE     = 16;
  {* ADRS 类型偏移（4 字节）}
  CN_ADRS_OFFSET_KEYPAIR  = 20;
  {* ADRS 密钥对索引偏移（4 字节）}
  CN_ADRS_OFFSET_CHAIN    = 24;
  {* ADRS 链地址偏移（4 字节）}
  CN_ADRS_OFFSET_HASH     = 28;
  {* ADRS 杂凑地址偏移（4 字节）}

  CN_ADRS_SIZE_SHAKE      = 32;
  {* ADRS 地址大小：SHAKE 变体（32 字节）}
  CN_ADRS_SIZE_SHA2       = 22;
  {* ADRS 地址大小：SHA2 变体（22 字节）}

type
  ECnSlhException = class(Exception);
  {* SLH-DSA 异常基类}

  PCnSlhAddr = ^TCnSlhAddr;
  {* ADRS 地址指针类型}
  TCnSlhAddr = array[0..31] of Byte;
  {* ADRS 地址类型（SHAKE 变体，32 字节）}

  TCnSlhAddrSHA2 = array[0..21] of Byte;
  {* ADRS 地址类型（SHA2 变体，22 字节压缩格式）}

  TCnSlhBytes16 = array[0..15] of Byte;
  {* 16 字节缓冲区，对应 n=16 参数集}
  TCnSlhBytes24 = array[0..23] of Byte;
  {* 24 字节缓冲区，对应 n=24 参数集}
  TCnSlhBytes32 = array[0..31] of Byte;
  {* 32 字节缓冲区，对应 n=32 参数集}
  TCnSlhDigest = TBytes;
  {* 通用摘要类型}

  TCnSlhParamSet = (
    slhSHA2_128s,
    slhSHA2_128f,
    slhSHA2_192s,
    slhSHA2_192f,
    slhSHA2_256s,
    slhSHA2_256f,
    slhSHAKE_128s,
    slhSHAKE_128f,
    slhSHAKE_192s,
    slhSHAKE_192f,
    slhSHAKE_256s,
    slhSHAKE_256f
  );
  {* SLH-DSA 参数集枚举，共 12 种，按安全级别和性能分为 s（签名小）和 f（签名快）两组}

  PCnSlhParams = ^TCnSlhParams;
  {* SLH-DSA 参数记录指针}
  TCnSlhParams = packed record
    N: Byte;
    {* 安全参数（字节）: 16/24/32}
    H: Byte;
    {* Hypertree 总高度: 63/64/66/68}
    D: Byte;
    {* Hypertree 层数: 7/8/17/22}
    Hp: Byte;
    {* 每层 XMSS 树高度 h' = h / d}
    A: Byte;
    {* FORS 树高度，每棵树有 2^a 个叶子}
    K: Byte;
    {* FORS 树数量}
    W: Byte;
    {* Winternitz 参数，固定为 w = 16}
    M: Byte;
    {* 消息摘要长度（字节）}
    Len: Byte;
    {* WOTS+ 链数 Len = Ceil(8*n / Lg(w))}
    Len1: Byte;
    {* WOTS+ 消息编码链数，Len1 = Len}
    Len2: Byte;
    {* WOTS+ 校验和链数}
    LenTotal: Byte;
    {* WOTS+ 总链数 Len + Len2}
    IsSHA2: Boolean;
    {* 杂凑家族标识：True = SHA2 系列，False = SHAKE 系列}
  end;
  {* SLH-DSA 参数记录，存储一个参数集的全部参数和派生常量}

  TCnSlhPublicKey = class
  {* SLH-DSA 公钥}
  public
    Seed: TBytes;
    {* 公钥种子（n 字节）}
    Root: TBytes;
    {* 公钥根值（n 字节）}
  end;
  {* SLH-DSA 公钥类型}

  TCnSlhSecretKey = class
  {* SLH-DSA 私钥}
  public
    Seed: TBytes;
    {* 私钥种子（n 字节）}
    Prf: TBytes;
    {* 私钥伪随机函数种子（n 字节）}
    PKSeed: TBytes;
    {* 公钥种子副本（n 字节）}
    PKRoot: TBytes;
    {* 公钥根值副本（n 字节）}
  end;
  {* SLH-DSA 私钥类型}

  TCnSlhSignature = TBytes;
  {* SLH-DSA 签名类型，为 TBytes 别名}

  TCnSlhProgressEvent = procedure(Percent: Integer; var Cancel: Boolean) of object;
  {* SLH-DSA 进度回调事件，Percent 为进度百分比，Cancel 用于取消操作}

  TCnSlhPrehashID = (
    shiSHA2_224,
    shiSHA2_256,
    shiSHA2_384,
    shiSHA2_512,
    shiSHA2_512_224,
    shiSHA2_512_256,
    shiSHA3_224,
    shiSHA3_256,
    shiSHA3_384,
    shiSHA3_512,
    shiSHAKE128,
    shiSHAKE256,
    shiSM3           // 注意 SM3 的标识值不在 FIPS 205 规范里，属于我们新增的
  );
  {* SLH-DSA Prehash 模式杂凑算法标识}

  TCnSLHDSA = class;


  TCnSlhFFunc = function(AParams: PCnSlhParams; const PKSeed: TBytes;
    var ADRS: TCnSlhAddr; const M1: TBytes): TBytes;
  {* 核心杂凑函数 F 的函数指针类型，用于 WOTS+ 链内杂凑}

  TCnSlhHFunc = function(AParams: PCnSlhParams; const PKSeed: TBytes;
    var ADRS: TCnSlhAddr; const M1, M2: TBytes): TBytes;
  {* 核心杂凑函数 H 的函数指针类型，用于 Merkle 树节点杂凑}

  TCnSlhTlFunc = function(AParams: PCnSlhParams; const PKSeed: TBytes;
    var ADRS: TCnSlhAddr; const M: TBytes): TBytes;
  {* 核心杂凑函数 T_l 的函数指针类型，用于 WOTS+ 公钥压缩}

  TCnSlhPRFFunc = function(AParams: PCnSlhParams; const PKSeed, SKSeed: TBytes;
    var ADRS: TCnSlhAddr): TBytes;
  {* 核心杂凑函数 PRF 的函数指针类型，用于生成 WOTS+/FORS 私钥值}

  TCnSlhPRFMsgFunc = function(AParams: PCnSlhParams;
    const SKPrf, OptRand, M: TBytes): TBytes;
  {* 核心杂凑函数 PRF_msg 的函数指针类型，用于生成签名随机数 R}

  TCnSlhHMsgFunc = function(AParams: PCnSlhParams;
    const R, PKSeed, PKRoot, M: TBytes): TBytes;
  {* 核心杂凑函数 H_msg 的函数指针类型，用于生成消息摘要}

  TCnSlhHashFuncs = packed record
  {* 核心杂凑函数表，根据参数集初始化对应的 SHA2 或 SHAKE 实现}
    F: TCnSlhFFunc;
    H: TCnSlhHFunc;
    T_l: TCnSlhTlFunc;
    PRF: TCnSlhPRFFunc;
    PRF_msg: TCnSlhPRFMsgFunc;
    H_msg: TCnSlhHMsgFunc;
  end;

  TCnSLHDSA = class
  {* SLH-DSA 无状态杂凑数字签名算法实现类}
  private
    FParams: PCnSlhParams;
    FParamSet: TCnSlhParamSet;
    FOnProgress: TCnSlhProgressEvent;
    FCancel: Boolean;
    FHashFuncs: TCnSlhHashFuncs;
    // 大小查询
    function GetPublicKeySize: Integer;
    function GetSecretKeySize: Integer;
    function GetSignatureSize: Integer;
  protected
    function SignInternal(const MPrime: TBytes;
      const SK: TCnSlhSecretKey; const AddRnd: TBytes): TCnSlhSignature;
    {* 内部签名函数，Pure 和 Prehash 模式共用}
  public
    constructor Create(AParamSet: TCnSlhParamSet);
    {* 构造函数，指定 SLH-DSA 参数集}
    destructor Destroy; override;
    {* 析构函数}

    procedure GenerateKeys(PK: TCnSlhPublicKey; SK: TCnSlhSecretKey);
    {* 生成 SLH-DSA 密钥对}

    function SignBytes(const SK: TCnSlhSecretKey; const Msg: TBytes;
      Randomize: Boolean = True): TCnSlhSignature;
    {* Pure 模式签名，Randomize 为 True 时使用随机化（Hedged）签名，为 False 时确定性签名}
    function VerifyBytes(const PK: TCnSlhPublicKey; const Msg: TBytes;
      const SIG: TCnSlhSignature): Boolean;
    {* Pure 模式验签，返回 True 表示签名有效}

    function SignPreHash(const M: TBytes; const SK: TCnSlhSecretKey;
      HashID: TCnSlhPrehashID; Randomize: Boolean = True): TCnSlhSignature;
    {* Prehash 模式签名，先对消息进行指定杂凑再签名}
    function VerifyPreHash(const M: TBytes; const SIG: TCnSlhSignature;
      const PK: TCnSlhPublicKey; HashID: TCnSlhPrehashID): Boolean;
    {* Prehash 模式验签，需要与签名时相同的 HashID}

    function PublicKeyToBytes(const PK: TCnSlhPublicKey): TBytes;
    {* 将公钥序列化为字节数组（2n 字节）}
    procedure BytesToPublicKey(PK: TCnSlhPublicKey; const Data: TBytes);
    {* 从字节数组反序列化为公钥}
    function SecretKeyToBytes(const SK: TCnSlhSecretKey): TBytes;
    {* 将私钥序列化为字节数组（4n 字节）}
    procedure BytesToSecretKey(SK: TCnSlhSecretKey; const Data: TBytes);
    {* 从字节数组反序列化为私钥}
    function SignatureToBytes(const SIG: TCnSlhSignature): TBytes;
    {* 将签名序列化为字节数组}
    function BytesToSignature(const Data: TBytes): TCnSlhSignature;
    {* 从字节数组反序列化为签名}

    property ParamSet: TCnSlhParamSet read FParamSet;
    {* 当前使用的参数集}
    property Params: PCnSlhParams read FParams;
    {* 当前参数集的参数记录指针}
    property SecretKeySize: Integer read GetSecretKeySize;
    {* 私钥长度（字节）}
    property PublicKeySize: Integer read GetPublicKeySize;
    {* 公钥长度（字节）}
    property SignatureSize: Integer read GetSignatureSize;
    {* 签名长度（字节）}
    property OnProgress: TCnSlhProgressEvent read FOnProgress write FOnProgress;
    {* 进度回调事件}
  end;

// -------------------------------------------------------------------
// 全局辅助函数
// -------------------------------------------------------------------

function SlhGetParams(AParamSet: TCnSlhParamSet): PCnSlhParams;
{* 根据参数集枚举获取对应的参数记录指针}
function SlhParamSetName(AParamSet: TCnSlhParamSet): string;
{* 获取参数集的名称字符串}
function SlhParamSetFromName(const AName: string): TCnSlhParamSet;
{* 根据名称字符串获取对应的参数集枚举}

function SlhReadU32BE(const Buf): Cardinal;
{* 以大端序从缓冲区读取 32 位无符号整数}
function SlhReadU64BE(const Buf): TUInt64;
{* 以大端序从缓冲区读取 64 位无符号整数}
procedure SlhWriteU32BE(var Buf; Value: Cardinal);
{* 以大端序将 32 位无符号整数写入缓冲区}
procedure SlhWriteU64BE(var Buf; Value: TUInt64);
{* 以大端序将 64 位无符号整数写入缓冲区}

procedure SlhAddrInit(var AD: TCnSlhAddr);
{* 初始化 ADRS 地址，清零所有字段}
procedure SlhAddrSetLayer(var AD: TCnSlhAddr; Layer: Cardinal);
{* 设置 ADRS 层地址域}
procedure SlhAddrSetTree(var AD: TCnSlhAddr; Tree: TUInt64);
{* 设置 ADRS 树地址域}
procedure SlhAddrSetType(var AD: TCnSlhAddr; Typ: Cardinal);
{* 设置 ADRS 类型域（0-6，对应 WOTS+/FORS/XMSS 等）}
procedure SlhAddrSetTypeAndClear(var AD: TCnSlhAddr; Typ: Cardinal);
{* 设置 ADRS 类型域并清空后续字段}
procedure SlhAddrSetKeyPair(var AD: TCnSlhAddr; Pair: Cardinal);
{* 设置 ADRS 密钥对索引域}
procedure SlhAddrSetChain(var AD: TCnSlhAddr; Chain: Cardinal);
{* 设置 ADRS 链地址域}
procedure SlhAddrSetHash(var AD: TCnSlhAddr; Hash: Cardinal);
{* 设置 ADRS 杂凑地址域}
procedure SlhAddrSetTreeHeight(var AD: TCnSlhAddr; Height: Cardinal);
{* 设置 ADRS 树高度域}
procedure SlhAddrSetTreeIndex(var AD: TCnSlhAddr; Index: Cardinal);
{* 设置 ADRS 树索引域}
procedure SlhAddrCopy(const Src: TCnSlhAddr; var Dst: TCnSlhAddr);
{* 复制 ADRS 地址}
procedure SlhAddrCompress(const Src: TCnSlhAddr; var Dst: TCnSlhAddrSHA2);
{* 将 SHAKE 32 字节 ADRS 压缩为 SHA2 22 字节格式}
procedure SlhAddrDecompress(const Src: TCnSlhAddrSHA2; var Dst: TCnSlhAddr);
{* 将 SHA2 22 字节 ADRS 解压为 SHAKE 32 字节格式}
function SlhAddrSize(IsSHA2: Boolean): Integer;
{* 获取 ADRS 地址大小，SHA2 返回 22，SHAKE 返回 32}
function SlhAddrToBytes(const AD: TCnSlhAddr; IsSHA2: Boolean): TBytes;
{* 将 ADRS 地址转换为字节数组}

function SlhMGF1_SHA256(const Seed: TBytes; OutLen: Integer): TBytes;
{* 基于 SHA-256 的 MGF1 掩码生成函数（SHA2 参数集 H_msg 使用）}
function SlhMGF1_SHA512(const Seed: TBytes; OutLen: Integer): TBytes;
{* 基于 SHA-512 的 MGF1 掩码生成函数（SHA2 参数集 H_msg 使用）}

function SlhF(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1: TBytes): TBytes;
{* 核心杂凑函数 F，用于 WOTS+ 链内单步杂凑}
function SlhH(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1, M2: TBytes): TBytes;
{* 核心杂凑函数 H，用于 Merkle 树内部节点杂凑}
function SlhTl(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M: TBytes): TBytes;
{* 核心杂凑函数 T_l，用于 WOTS+ 公钥压缩}
function SlhPRF(Params: PCnSlhParams; const PKSeed, SKSeed: TBytes;
  var ADRS: TCnSlhAddr): TBytes;
{* 核心杂凑函数 PRF，用于 WOTS+ 和 FORS 私钥值生成}
function SlhPRFMsg(Params: PCnSlhParams;
  const SKPrf, OptRand, M: TBytes): TBytes;
{* 核心杂凑函数 PRF_msg，用于生成签名随机数 R}
function SlhHMsg(Params: PCnSlhParams;
  const R, PKSeed, PKRoot, M: TBytes): TBytes;
{* 核心杂凑函数 H_msg，用于生成签名消息摘要}

procedure SlhInitHashFuncs(Params: PCnSlhParams; out Funcs: TCnSlhHashFuncs);
{* 根据参数集初始化 6 个核心杂凑函数的函数指针表}

procedure SlhBaseWEncode(const M: TBytes; N, W, Len: Byte; var Msg: TBytes; Offset: Integer);
{* WOTS+ 基 w 编码，将消息编码为 Len 个基 w 的整数序列}
function SlhBaseWChecksum(const Msg: TBytes; W, Len1, Len2: Byte): TBytes;
{* WOTS+ 基 w 校验和计算}

function SlhChain(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const X0: TBytes; Start, Steps: Byte): TBytes;
{* WOTS+ 链迭代（Algorithm 1），对 X0 迭代调用 F 函数 Steps 次}

function SlhWotsKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;
{* WOTS+ 密钥生成（Algorithm 3）}
function SlhWotsSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const M, SKSeed, PKSeed: TBytes): TBytes;
{* WOTS+ 签名（Algorithm 4）}
function SlhWotsPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SIG, M, PKSeed: TBytes): TBytes;
{* WOTS+ 从签名恢复公钥（Algorithm 5）}

function SlhXmssTreeHash(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes; LeafIdx: Cardinal): TBytes;
{* XMSS 树杂凑（Algorithm 9），计算指定叶节点的认证路径根}
function SlhXmssKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;
{* XMSS 密钥生成（Algorithm 8）}
function SlhXmssSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const M, SKSeed, PKSeed: TBytes; Idx: Cardinal): TBytes;
{* XMSS 签名（Algorithm 10）}
function SlhXmssPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  Idx: Cardinal; const SIG, M, PKSeed: TBytes): TBytes;
{* XMSS 从签名恢复公钥（Algorithm 13）}

function SlhHtSign(Params: PCnSlhParams; const M, SKSeed, PKSeed: TBytes;
  TreeIdx: TUInt64; LeafIdx: Cardinal): TBytes;
{* Hypertree 签名（Algorithm 12）}
function SlhHtVerify(Params: PCnSlhParams; const M, SIG: TBytes;
  const PKSeed: TBytes; TreeIdx: TUInt64; LeafIdx: Cardinal;
  const PKRoot: TBytes): Boolean;
{* Hypertree 验签（Algorithm 14）}

function SlhForsTreeHash(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes; TreeIdx: Byte): TBytes;
{* FORS 树杂凑，构建一棵 FORS Merkle 树并返回根}
function SlhForsKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;
{* FORS 密钥生成，生成 k 棵树根并拼接}
function SlhForsSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const Md, SKSeed, PKSeed: TBytes): TBytes;
{* FORS 签名（Algorithm 15）}
function SlhForsPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SIG, Md, PKSeed: TBytes): TBytes;
{* FORS 从签名恢复公钥（Algorithm 16）}

implementation

// ===================================================================
// 参数表
// ===================================================================
//
// 数据来源：FIPS 205 Section 11, Table 1-2
// 计算规则：
//   Len = Ceil(8*n / Lg(w)),  w=16, Lg(w)=4  => Len = 2*n
//   Len1 = Len
//   Len2 = Floor(Log2(Len1*(w-1)) / Lg(w)) + 1
//   LenTotal = Len + Len2
//   hp = h div d
//
//   n=16: Len=32, Len1=32, Len2=3, LenTotal=35
//   n=24: Len=48, Len1=48, Len2=3, LenTotal=51
//   n=32: Len=64, Len1=64, Len2=3, LenTotal=67

const
  SLH_PARAM_COUNT = 12;
  SLH_PARAM_TABLE: array[0..SLH_PARAM_COUNT - 1] of TCnSlhParams = (
    // 0: slhSHA2_128s
    (N:16; H:63; D:7;  Hp:9;  A:12; K:14; W:16; M:30;
     Len:32; Len1:32; Len2:3; LenTotal:35; IsSHA2:True),
    // 1: slhSHA2_128f
    (N:16; H:66; D:22; Hp:3;  A:6;  K:33; W:16; M:34;
     Len:32; Len1:32; Len2:3; LenTotal:35; IsSHA2:True),
    // 2: slhSHA2_192s
    (N:24; H:63; D:7;  Hp:9;  A:14; K:17; W:16; M:39;
     Len:48; Len1:48; Len2:3; LenTotal:51; IsSHA2:True),
    // 3: slhSHA2_192f
    (N:24; H:66; D:22; Hp:3;  A:8;  K:33; W:16; M:42;
     Len:48; Len1:48; Len2:3; LenTotal:51; IsSHA2:True),
    // 4: slhSHA2_256s
    (N:32; H:64; D:8;  Hp:8;  A:14; K:22; W:16; M:47;
     Len:64; Len1:64; Len2:3; LenTotal:67; IsSHA2:True),
    // 5: slhSHA2_256f
    (N:32; H:68; D:17; Hp:4;  A:9;  K:35; W:16; M:49;
     Len:64; Len1:64; Len2:3; LenTotal:67; IsSHA2:True),
    // 6: slhSHAKE_128s
    (N:16; H:63; D:7;  Hp:9;  A:12; K:14; W:16; M:30;
     Len:32; Len1:32; Len2:3; LenTotal:35; IsSHA2:False),
    // 7: slhSHAKE_128f
    (N:16; H:66; D:22; Hp:3;  A:6;  K:33; W:16; M:34;
     Len:32; Len1:32; Len2:3; LenTotal:35; IsSHA2:False),
    // 8: slhSHAKE_192s
    (N:24; H:63; D:7;  Hp:9;  A:14; K:17; W:16; M:39;
     Len:48; Len1:48; Len2:3; LenTotal:51; IsSHA2:False),
    // 9: slhSHAKE_192f
    (N:24; H:66; D:22; Hp:3;  A:8;  K:33; W:16; M:42;
     Len:48; Len1:48; Len2:3; LenTotal:51; IsSHA2:False),
    // 10: slhSHAKE_256s
    (N:32; H:64; D:8;  Hp:8;  A:14; K:22; W:16; M:47;
     Len:64; Len1:64; Len2:3; LenTotal:67; IsSHA2:False),
    // 11: slhSHAKE_256f
    (N:32; H:68; D:17; Hp:4;  A:9;  K:35; W:16; M:49;
     Len:64; Len1:64; Len2:3; LenTotal:67; IsSHA2:False)
  );

  SLH_PARAM_SET_NAMES: array[0..SLH_PARAM_COUNT - 1] of string = (
    'SLH-DSA-SHA2-128s',
    'SLH-DSA-SHA2-128f',
    'SLH-DSA-SHA2-192s',
    'SLH-DSA-SHA2-192f',
    'SLH-DSA-SHA2-256s',
    'SLH-DSA-SHA2-256f',
    'SLH-DSA-SHAKE-128s',
    'SLH-DSA-SHAKE-128f',
    'SLH-DSA-SHAKE-192s',
    'SLH-DSA-SHAKE-192f',
    'SLH-DSA-SHAKE-256s',
    'SLH-DSA-SHAKE-256f'
  );

// ===================================================================
// 辅助函数
// ===================================================================

function SlhGetParams(AParamSet: TCnSlhParamSet): PCnSlhParams;
begin
  Result := @SLH_PARAM_TABLE[Ord(AParamSet)];
end;

function SlhParamSetName(AParamSet: TCnSlhParamSet): string;
begin
  Result := SLH_PARAM_SET_NAMES[Ord(AParamSet)];
end;

function SlhParamSetFromName(const AName: string): TCnSlhParamSet;
var
  I: Integer;
begin
  for I := 0 to SLH_PARAM_COUNT - 1 do
  begin
    if SameText(AName, SLH_PARAM_SET_NAMES[I]) then
    begin
      Result := TCnSlhParamSet(I);
      Exit;
    end;
  end;
  raise ECnSlhException.Create('Unknown SLH-DSA parameter set: ' + AName);
end;

// -------------------------------------------------------------------
// 大端读写
// -------------------------------------------------------------------

function SlhReadU32BE(const Buf): Cardinal;
begin
  Result := UInt32NetworkToHost(PCardinal(@Buf)^);
end;

function SlhReadU64BE(const Buf): TUInt64;
begin
  Result := UInt64NetworkToHost(PUInt64(@Buf)^);
end;

procedure SlhWriteU32BE(var Buf; Value: Cardinal);
begin
  PCardinal(@Buf)^ := UInt32HostToNetwork(Value);
end;

procedure SlhWriteU64BE(var Buf; Value: TUInt64);
begin
  PUInt64(@Buf)^ := UInt64HostToNetwork(Value);
end;

// ===================================================================
// ADRS 地址操作
// ===================================================================

procedure SlhAddrInit(var AD: TCnSlhAddr);
begin
  FillChar(AD, SizeOf(TCnSlhAddr), 0);
end;

procedure SlhAddrSetLayer(var AD: TCnSlhAddr; Layer: Cardinal);
begin
  SlhWriteU32BE(AD[0], Layer);
end;

procedure SlhAddrSetTree(var AD: TCnSlhAddr; Tree: TUInt64);
begin
  SlhWriteU64BE(AD[CN_ADRS_OFFSET_TREE], Tree);
end;

procedure SlhAddrSetType(var AD: TCnSlhAddr; Typ: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_TYPE], Typ);
end;

procedure SlhAddrSetTypeAndClear(var AD: TCnSlhAddr; Typ: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_TYPE], Typ);
  FillChar(AD[CN_ADRS_OFFSET_KEYPAIR], 12, 0); // Clear Words 4-6 (bytes 20-31)
end;

procedure SlhAddrSetKeyPair(var AD: TCnSlhAddr; Pair: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_KEYPAIR], Pair);
end;

procedure SlhAddrSetChain(var AD: TCnSlhAddr; Chain: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_CHAIN], Chain);
end;

procedure SlhAddrSetHash(var AD: TCnSlhAddr; Hash: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_HASH], Hash);
end;

procedure SlhAddrSetTreeHeight(var AD: TCnSlhAddr; Height: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_CHAIN], Height); // Word 5
end;

procedure SlhAddrSetTreeIndex(var AD: TCnSlhAddr; Index: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_HASH], Index); // Word 6
end;

procedure SlhAddrCopy(const Src: TCnSlhAddr; var Dst: TCnSlhAddr);
begin
  Move(Src, Dst, SizeOf(TCnSlhAddr));
end;

procedure SlhAddrCompress(const Src: TCnSlhAddr; var Dst: TCnSlhAddrSHA2);
begin
  // FIPS 205: ADRSc = ADRS[3] || ADRS[8:16] || ADRS[19] || ADRS[20:32]
  Dst[0] := Src[3];                  // Layer (1 byte, LSB of 4-byte field)
  Move(Src[8], Dst[1], 8);           // Tree address (8 bytes)
  Dst[9] := Src[19];                 // Type (1 byte, LSB of 4-byte field)
  Move(Src[20], Dst[10], 12);        // KeyPair(4) + Chain(4) + Hash(4)
end;

procedure SlhAddrDecompress(const Src: TCnSlhAddrSHA2; var Dst: TCnSlhAddr);
begin
  FillChar(Dst, SizeOf(TCnSlhAddr), 0);
  // Reverse of compress: ADRSc = ADRS[3] || ADRS[8:16] || ADRS[19] || ADRS[20:32]
  Dst[3] := Src[0];                  // Layer
  Move(Src[1], Dst[8], 8);           // Tree
  Dst[19] := Src[9];                 // Type
  Move(Src[10], Dst[20], 12);        // Rest
end;

function SlhAddrSize(IsSHA2: Boolean): Integer;
begin
  if IsSHA2 then
    Result := CN_ADRS_SIZE_SHA2
  else
    Result := CN_ADRS_SIZE_SHAKE;
end;

function SlhAddrToBytes(const AD: TCnSlhAddr; IsSHA2: Boolean): TBytes;
begin
  if IsSHA2 then
  begin
    // ADRSc = ADRS[3] || ADRS[8:16] || ADRS[19] || ADRS[20:32] = 22 bytes
    SetLength(Result, CN_ADRS_SIZE_SHA2);
    Result[0] := AD[3];
    Move(AD[8], Result[1], 8);
    Result[9] := AD[19];
    Move(AD[20], Result[10], 12);
  end
  else
  begin
    SetLength(Result, CN_ADRS_SIZE_SHAKE);
    Move(AD, Result[0], CN_ADRS_SIZE_SHAKE);
  end;
end;

// ===================================================================
// 6 核心杂凑函数
// ===================================================================

function SlhMGF1_SHA256(const Seed: TBytes; OutLen: Integer): TBytes;
var
  Counter: Cardinal;
  D256: TCnSHA256Digest;
  Offset, I, Chunk: Integer;
  Data: TBytes;
begin
  SetLength(Result, OutLen);
  Offset := 0;
  Counter := 0;
  SetLength(Data, Length(Seed) + 4);
  if Length(Seed) > 0 then
    Move(Seed[0], Data[0], Length(Seed));
  while Offset < OutLen do
  begin
    Data[Length(Seed)] := Byte(Counter shr 24);
    Data[Length(Seed) + 1] := Byte(Counter shr 16);
    Data[Length(Seed) + 2] := Byte(Counter shr 8);
    Data[Length(Seed) + 3] := Byte(Counter);
    D256 := SHA256Bytes(Data);
    Chunk := 32;
    if Offset + Chunk > OutLen then
      Chunk := OutLen - Offset;
    for I := 0 to Chunk - 1 do
      Result[Offset + I] := D256[I];
    Inc(Offset, 32);
    Inc(Counter);
  end;
end;

function SlhMGF1_SHA512(const Seed: TBytes; OutLen: Integer): TBytes;
var
  Counter: Cardinal;
  D512: TCnSHA512Digest;
  Offset, I, Chunk: Integer;
  Data: TBytes;
begin
  SetLength(Result, OutLen);
  Offset := 0;
  Counter := 0;
  SetLength(Data, Length(Seed) + 4);
  if Length(Seed) > 0 then
    Move(Seed[0], Data[0], Length(Seed));
  while Offset < OutLen do
  begin
    Data[Length(Seed)] := Byte(Counter shr 24);
    Data[Length(Seed) + 1] := Byte(Counter shr 16);
    Data[Length(Seed) + 2] := Byte(Counter shr 8);
    Data[Length(Seed) + 3] := Byte(Counter);
    D512 := SHA512Bytes(Data);
    Chunk := 64;
    if Offset + Chunk > OutLen then
      Chunk := OutLen - Offset;
    for I := 0 to Chunk - 1 do
      Result[Offset + I] := D512[I];
    Inc(Offset, 64);
    Inc(Counter);
  end;
end;

// -------------------------------------------------------------------
// SlhF - FIPS 205 Section 10: 链内杂凑函数
// SHA2: SHA-256(PK.seed || toByte(0, 64-n) || ADRSc || M1)
// SHAKE: SHAKE256(PK.seed || ADRS || M1, 8n)
// -------------------------------------------------------------------
function SlhF(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1: TBytes): TBytes;
var
  AddrBytes: TBytes;
  Input: TBytes;
  PadLen, Offset: Integer;
  D256: TCnSHA256Digest;
begin
  if not Params.IsSHA2 then
  begin
    // SHAKE: PK.seed || ADRS(32) || M1
    SetLength(Input, Length(PKSeed) + CN_ADRS_SIZE_SHAKE + Length(M1));
    Offset := 0;
    Move(PKSeed[0], Input[0], Length(PKSeed));
    Inc(Offset, Length(PKSeed));
    Move(ADRS, Input[Offset], CN_ADRS_SIZE_SHAKE);
    Inc(Offset, CN_ADRS_SIZE_SHAKE);
    if Length(M1) > 0 then
      Move(M1[0], Input[Offset], Length(M1));
    Result := SHAKE256Bytes(Input, Params.N);
    Exit;
  end;

  // SHA2: F always uses SHA-256, padding = toByte(0, 64-n)
  AddrBytes := SlhAddrToBytes(ADRS, True);
  PadLen := 64 - Params.N;
  SetLength(Input, Params.N + PadLen + Length(AddrBytes) + Length(M1));
  Offset := 0;
  Move(PKSeed[0], Input[0], Params.N);
  Inc(Offset, Params.N);
  FillChar(Input[Offset], PadLen, 0);
  Inc(Offset, PadLen);
  Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
  Inc(Offset, Length(AddrBytes));
  if Length(M1) > 0 then
    Move(M1[0], Input[Offset], Length(M1));

  D256 := SHA256Bytes(Input);
  SetLength(Result, Params.N);
  Move(D256, Result[0], Params.N);
end;

// -------------------------------------------------------------------
// SlhH - FIPS 205 Section 10: 树杂凑函数
// SHA2 n=16: SHA-256(PK.seed || toByte(0, 64-n) || ADRSc || M1 || M2)
// SHA2 n>16: SHA-512(PK.seed || toByte(0, 128-n) || ADRSc || M1 || M2)
// SHAKE: SHAKE256(PK.seed || ADRS || M1 || M2, 8n)
// -------------------------------------------------------------------
function SlhH(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1, M2: TBytes): TBytes;
var
  AddrBytes: TBytes;
  Input: TBytes;
  PadLen, Offset: Integer;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  if not Params.IsSHA2 then
  begin
    // SHAKE: PK.seed || ADRS(32) || M1 || M2
    SetLength(Input, Length(PKSeed) + CN_ADRS_SIZE_SHAKE + Length(M1) + Length(M2));
    Offset := 0;
    Move(PKSeed[0], Input[0], Length(PKSeed));
    Inc(Offset, Length(PKSeed));
    Move(ADRS, Input[Offset], CN_ADRS_SIZE_SHAKE);
    Inc(Offset, CN_ADRS_SIZE_SHAKE);
    Move(M1[0], Input[Offset], Length(M1));
    Inc(Offset, Length(M1));
    Move(M2[0], Input[Offset], Length(M2));
    Result := SHAKE256Bytes(Input, Params.N);
    Exit;
  end;

  // SHA2: n=16 uses SHA-256 (pad to 64), n>16 uses SHA-512 (pad to 128)
  AddrBytes := SlhAddrToBytes(ADRS, True);
  if Params.N <= 16 then
  begin
    PadLen := 64 - Params.N;
    SetLength(Input, Params.N + PadLen + Length(AddrBytes) + Length(M1) + Length(M2));
    Offset := 0;
    Move(PKSeed[0], Input[0], Params.N);
    Inc(Offset, Params.N);
    FillChar(Input[Offset], PadLen, 0);
    Inc(Offset, PadLen);
    Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
    Inc(Offset, Length(AddrBytes));
    Move(M1[0], Input[Offset], Length(M1));
    Inc(Offset, Length(M1));
    Move(M2[0], Input[Offset], Length(M2));

    D256 := SHA256Bytes(Input);
    SetLength(Result, Params.N);
    Move(D256, Result[0], Params.N);
  end
  else
  begin
    PadLen := 128 - Params.N;
    SetLength(Input, Params.N + PadLen + Length(AddrBytes) + Length(M1) + Length(M2));
    Offset := 0;
    Move(PKSeed[0], Input[0], Params.N);
    Inc(Offset, Params.N);
    FillChar(Input[Offset], PadLen, 0);
    Inc(Offset, PadLen);
    Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
    Inc(Offset, Length(AddrBytes));
    Move(M1[0], Input[Offset], Length(M1));
    Inc(Offset, Length(M1));
    Move(M2[0], Input[Offset], Length(M2));

    D512 := SHA512Bytes(Input);
    SetLength(Result, Params.N);
    Move(D512, Result[0], Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhT_l - FIPS 205 Section 10: WOTS+ 链压缩（L-Tree）
// SHA2 n=16: Trunc_n(SHA-256(PK.seed || toByte(0, 64-n) || ADRSc || M))
// SHA2 n>16: Trunc_n(SHA-512(PK.seed || toByte(0, 128-n) || ADRSc || M))
// SHAKE: Trunc_n(SHAKE256(PK.seed || ADRS || M, 8n))
// -------------------------------------------------------------------
function SlhTl(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M: TBytes): TBytes;
var
  AddrBytes: TBytes;
  Input: TBytes;
  PadLen, Offset: Integer;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  if not Params.IsSHA2 then
  begin
    // SHAKE: PK.seed || ADRS(32) || M
    SetLength(Input, Length(PKSeed) + CN_ADRS_SIZE_SHAKE + Length(M));
    Offset := 0;
    Move(PKSeed[0], Input[0], Length(PKSeed));
    Inc(Offset, Length(PKSeed));
    Move(ADRS, Input[Offset], CN_ADRS_SIZE_SHAKE);
    Inc(Offset, CN_ADRS_SIZE_SHAKE);
    if Length(M) > 0 then
      Move(M[0], Input[Offset], Length(M));
    Result := SHAKE256Bytes(Input, Params.N);
    Exit;
  end;

  // SHA2: n=16 uses SHA-256, n>16 uses SHA-512
  AddrBytes := SlhAddrToBytes(ADRS, True);
  if Params.N <= 16 then
  begin
    PadLen := 64 - Params.N;
    SetLength(Input, Params.N + PadLen + Length(AddrBytes) + Length(M));
    Offset := 0;
    Move(PKSeed[0], Input[0], Params.N);
    Inc(Offset, Params.N);
    FillChar(Input[Offset], PadLen, 0);
    Inc(Offset, PadLen);
    Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
    Inc(Offset, Length(AddrBytes));
    if Length(M) > 0 then
      Move(M[0], Input[Offset], Length(M));

    D256 := SHA256Bytes(Input);
    SetLength(Result, Params.N);
    Move(D256, Result[0], Params.N);
  end
  else
  begin
    PadLen := 128 - Params.N;
    SetLength(Input, Params.N + PadLen + Length(AddrBytes) + Length(M));
    Offset := 0;
    Move(PKSeed[0], Input[0], Params.N);
    Inc(Offset, Params.N);
    FillChar(Input[Offset], PadLen, 0);
    Inc(Offset, PadLen);
    Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
    Inc(Offset, Length(AddrBytes));
    if Length(M) > 0 then
      Move(M[0], Input[Offset], Length(M));

    D512 := SHA512Bytes(Input);
    SetLength(Result, Params.N);
    Move(D512, Result[0], Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhPRF - FIPS 205 Section 10: 伪随机函数（用于密钥生成）
// SHA2: Trunc_n(SHA-256(PK.seed || toByte(0, 64-n) || ADRSc || SK.seed))
// SHAKE: Trunc_n(SHAKE256(PK.seed || ADRS || SK.seed, 8n))
// -------------------------------------------------------------------
function SlhPRF(Params: PCnSlhParams; const PKSeed, SKSeed: TBytes;
  var ADRS: TCnSlhAddr): TBytes;
var
  AddrBytes: TBytes;
  Input: TBytes;
  PadLen, Offset: Integer;
  D256: TCnSHA256Digest;
begin
  if not Params.IsSHA2 then
  begin
    // SHAKE: PK.seed || ADRS(32) || SK.seed
    SetLength(Input, Length(PKSeed) + CN_ADRS_SIZE_SHAKE + Length(SKSeed));
    Offset := 0;
    Move(PKSeed[0], Input[0], Length(PKSeed));
    Inc(Offset, Length(PKSeed));
    Move(ADRS, Input[Offset], CN_ADRS_SIZE_SHAKE);
    Inc(Offset, CN_ADRS_SIZE_SHAKE);
    Move(SKSeed[0], Input[Offset], Length(SKSeed));
    Result := SHAKE256Bytes(Input, Params.N);
    Exit;
  end;

  // SHA2: PRF always uses SHA-256, padding = toByte(0, 64-n)
  AddrBytes := SlhAddrToBytes(ADRS, True);
  PadLen := 64 - Params.N;
  SetLength(Input, Params.N + PadLen + Length(AddrBytes) + Length(SKSeed));
  Offset := 0;
  Move(PKSeed[0], Input[0], Params.N);
  Inc(Offset, Params.N);
  FillChar(Input[Offset], PadLen, 0);
  Inc(Offset, PadLen);
  Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
  Inc(Offset, Length(AddrBytes));
  Move(SKSeed[0], Input[Offset], Length(SKSeed));

  D256 := SHA256Bytes(Input);
  SetLength(Result, Params.N);
  Move(D256, Result[0], Params.N);
end;

// -------------------------------------------------------------------
// SlhPRF_msg - FIPS 205 Section 10: 消息伪随机函数
// SHA2 n=16: Trunc_n(HMAC-SHA-256(SK.prf, opt_rand || M))
// SHA2 n>16: Trunc_n(HMAC-SHA-512(SK.prf, opt_rand || M))
// SHAKE: Trunc_n(SHAKE256(SK.prf || opt_rand || M, 8n))
// -------------------------------------------------------------------
function SlhPRFMsg(Params: PCnSlhParams;
  const SKPrf, OptRand, M: TBytes): TBytes;
var
  Data: TBytes;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  if not Params.IsSHA2 then
  begin
    Data := ConcatBytes(SKPrf, OptRand, M);
    Result := SHAKE256Bytes(Data, Params.N);
    Exit;
  end;

  // SHA2: HMAC-SHA-x(SK.prf, opt_rand || M) -- key is SK.prf directly
  SetLength(Data, Length(OptRand) + Length(M));
  Move(OptRand[0], Data[0], Length(OptRand));
  if Length(M) > 0 then
    Move(M[0], Data[Length(OptRand)], Length(M));

  if Params.N <= 16 then
  begin
    D256 := SHA256HmacBytes(SKPrf, Data);
    SetLength(Result, Params.N);
    Move(D256, Result[0], Params.N);
  end
  else
  begin
    D512 := SHA512HmacBytes(SKPrf, Data);
    SetLength(Result, Params.N);
    Move(D512, Result[0], Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhH_msg - FIPS 205 Section 10: 消息杂凑函数
// SHA2 n=16: MGF1-SHA-256(R || PK.seed || SHA-256(R || PK.seed || PK.root || M), m)
// SHA2 n>16: MGF1-SHA-512(R || PK.seed || SHA-512(R || PK.seed || PK.root || M), m)
// SHAKE: SHAKE256(R || PK.Seed || PK.Root || M, m)
// -------------------------------------------------------------------
function SlhHMsg(Params: PCnSlhParams;
  const R, PKSeed, PKRoot, M: TBytes): TBytes;
var
  InnerInput, MGFSeed: TBytes;
  Offset: Integer;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  if not Params.IsSHA2 then
  begin
    InnerInput := ConcatBytes(R, PKSeed, PKRoot, M);
    Result := SHAKE256Bytes(InnerInput, Params.M);
    Exit;
  end;

  // SHA2: inner_hash = SHA-x(R || PK.seed || PK.root || M)
  SetLength(InnerInput, Length(R) + Length(PKSeed) + Length(PKRoot) + Length(M));
  Offset := 0;
  Move(R[0], InnerInput[0], Length(R));
  Inc(Offset, Length(R));
  Move(PKSeed[0], InnerInput[Offset], Length(PKSeed));
  Inc(Offset, Length(PKSeed));
  Move(PKRoot[0], InnerInput[Offset], Length(PKRoot));
  Inc(Offset, Length(PKRoot));
  if Length(M) > 0 then
    Move(M[0], InnerInput[Offset], Length(M));

  if Params.N <= 16 then
  begin
    // SHA-256 inner hash, then MGF1-SHA-256(R || PK.seed || h, m)
    D256 := SHA256Bytes(InnerInput);
    SetLength(MGFSeed, Length(R) + Length(PKSeed) + 32);
    Move(R[0], MGFSeed[0], Length(R));
    Move(PKSeed[0], MGFSeed[Length(R)], Length(PKSeed));
    Move(D256, MGFSeed[Length(R) + Length(PKSeed)], 32);
    Result := SlhMGF1_SHA256(MGFSeed, Params.M);
  end
  else
  begin
    // SHA-512 inner hash, then MGF1-SHA-512(R || PK.seed || h, m)
    D512 := SHA512Bytes(InnerInput);
    SetLength(MGFSeed, Length(R) + Length(PKSeed) + 64);
    Move(R[0], MGFSeed[0], Length(R));
    Move(PKSeed[0], MGFSeed[Length(R)], Length(PKSeed));
    Move(D512, MGFSeed[Length(R) + Length(PKSeed)], 64);
    Result := SlhMGF1_SHA512(MGFSeed, Params.M);
  end;
end;

// -------------------------------------------------------------------
// SlhInitHashFuncs - 函数选择器
// -------------------------------------------------------------------
procedure SlhInitHashFuncs(Params: PCnSlhParams; out Funcs: TCnSlhHashFuncs);
begin
  Funcs.F := SlhF;
  Funcs.H := SlhH;
  Funcs.T_l := SlhTl;
  Funcs.PRF := SlhPRF;
  Funcs.PRF_msg := SlhPRFMsg;
  Funcs.H_msg := SlhHMsg;
end;

// ===================================================================
// WOTS+ 实现
// ===================================================================

// -------------------------------------------------------------------
// SlhBaseWEncode - 基 w 编码
// 将 n 字节消息转换为 Len 个基 w 值（w=16, log2(w)=4）
// -------------------------------------------------------------------
procedure SlhBaseWEncode(const M: TBytes; N, W, Len: Byte;
  var Msg: TBytes; Offset: Integer);
var
  I, InIdx: Integer;
  Bits: Integer;
  Buf: Byte;
begin
  InIdx := 0;
  Bits := 0;
  Buf := 0;
  for I := 0 to Len - 1 do
  begin
    if Bits < 4 then
    begin
      if InIdx < Length(M) then
      begin
        Buf := M[InIdx];
        Inc(InIdx);
        Bits := 8;
      end
      else
        Bits := 0;
    end;
    Dec(Bits, 4);
    Msg[Offset + I] := (Buf shr Bits) and $0F;
  end;
end;

// -------------------------------------------------------------------
// SlhBaseWChecksum - 校验和编码
// -------------------------------------------------------------------
function SlhBaseWChecksum(const Msg: TBytes; W, Len1, Len2: Byte): TBytes;
var
  I: Integer;
  csum: Cardinal;
  CsumBytes, ShiftBits: Integer;
begin
  csum := 0;
  for I := 0 to Len1 - 1 do
    csum := csum + (W - 1 - Msg[I]);

  // FIPS 205: csum <<= (8 - ((len2 * lg(w)) mod 8)) mod 8
  ShiftBits := (8 - ((Len2 * 4) mod 8)) mod 8;
  csum := csum shl ShiftBits;

  CsumBytes := (Len2 * 4 + 7) div 8;
  SetLength(Result, CsumBytes);
  for I := 0 to CsumBytes - 1 do
  begin
    Result[CsumBytes - 1 - I] := Byte(csum);
    csum := csum shr 8;
  end;
end;

// -------------------------------------------------------------------
// SlhChain - Algorithm 1: WOTS+ 链迭代
// 对 X0 迭代调用 F 函数 Steps 次
// Input: X0 (n), PK.Seed (n), ADRS
// Output: X_{Steps} (n)
// -------------------------------------------------------------------
function SlhChain(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const X0: TBytes; Start, Steps: Byte): TBytes;
var
  I: Integer;
begin
  Result := X0;
  for I := Start to Start + Steps - 1 do
  begin
    SlhAddrSetHash(ADRS, I);
    Result := SlhF(Params, PKSeed, ADRS, Result);
  end;
end;

// -------------------------------------------------------------------
// SlhWotsKeyGen - FIPS 205 Algorithm 6: WOTS+ 密钥生成
// Input: SK.Seed (n), PK.Seed (n), ADRS (type=WOTS_HASH, keypair set)
// Output: PK (n)
// -------------------------------------------------------------------
function SlhWotsKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;
var
  I: Integer;
  SK, PK_i: TBytes;
  PkList: TBytes;
  SkAdrs, WotsPkAdrs: TCnSlhAddr;
  KP: Cardinal;
begin
  // Save key pair address from caller
  KP := SlhReadU32BE(ADRS[CN_ADRS_OFFSET_KEYPAIR]);

  // Create skADRS with type WOTS_PRF for PRF calls
  SlhAddrCopy(ADRS, SkAdrs);
  SlhAddrSetTypeAndClear(SkAdrs, CN_SLH_ADRS_WOTS_PRF);
  SlhAddrSetKeyPair(SkAdrs, KP);

  SetLength(PkList, Params.LenTotal * Params.N);
  for I := 0 to Params.LenTotal - 1 do
  begin
    // PRF with WOTS_PRF address
    SlhAddrSetChain(SkAdrs, I);
    SK := SlhPRF(Params, PKSeed, SKSeed, SkAdrs);

    // Chain with WOTS_HASH address (original ADRS)
    SlhAddrSetChain(ADRS, I);
    SlhAddrSetHash(ADRS, 0);
    PK_i := SlhChain(Params, PKSeed, ADRS, SK, 0, Params.W - 1);
    Move(PK_i[0], PkList[I * Params.N], Params.N);
  end;

  // Compress with WOTS_PK address
  SlhAddrCopy(ADRS, WotsPkAdrs);
  SlhAddrSetTypeAndClear(WotsPkAdrs, CN_SLH_ADRS_WOTS_PK);
  SlhAddrSetKeyPair(WotsPkAdrs, KP);
  Result := SlhTl(Params, PKSeed, WotsPkAdrs, PkList);
end;

// -------------------------------------------------------------------
// SlhWotsSign - FIPS 205 Algorithm 7: WOTS+ 签名
// Input: M (n), SK.Seed (n), PK.Seed (n), ADRS
// Output: SIG (LenTotal * n)
// -------------------------------------------------------------------
function SlhWotsSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const M, SKSeed, PKSeed: TBytes): TBytes;
var
  I: Integer;
  Msg: TBytes;
  CsumBytes: TBytes;
  TotalLen: Integer;
  SK, SigI: TBytes;
  SkAdrs: TCnSlhAddr;
  KP: Cardinal;
begin
  TotalLen := Params.LenTotal;
  SetLength(Msg, TotalLen);

  // Base-w encode message
  SlhBaseWEncode(M, Params.N, Params.W, Params.Len, Msg, 0);

  // Checksum
  CsumBytes := SlhBaseWChecksum(Msg, Params.W, Params.Len1, Params.Len2);

  // Base-w encode checksum into remaining positions
  SlhBaseWEncode(CsumBytes, Length(CsumBytes), Params.W, Params.Len2,
    Msg, Params.Len);

  // Create skADRS for PRF calls
  KP := SlhReadU32BE(ADRS[CN_ADRS_OFFSET_KEYPAIR]);
  SlhAddrCopy(ADRS, SkAdrs);
  SlhAddrSetTypeAndClear(SkAdrs, CN_SLH_ADRS_WOTS_PRF);
  SlhAddrSetKeyPair(SkAdrs, KP);

  // Generate signature
  SetLength(Result, TotalLen * Params.N);
  for I := 0 to TotalLen - 1 do
  begin
    SlhAddrSetChain(SkAdrs, I);
    SK := SlhPRF(Params, PKSeed, SKSeed, SkAdrs);

    SlhAddrSetChain(ADRS, I);
    SlhAddrSetHash(ADRS, 0);
    SigI := SlhChain(Params, PKSeed, ADRS, SK, 0, Msg[I]);
    Move(SigI[0], Result[I * Params.N], Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhWotsPKFromSig - FIPS 205 Algorithm 8: 从签名恢复 WOTS+ 公钥
// Input: SIG (LenTotal * n), M (n), PK.Seed (n), ADRS
// Output: PK (n)
// -------------------------------------------------------------------
function SlhWotsPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SIG, M, PKSeed: TBytes): TBytes;
var
  I: Integer;
  Msg: TBytes;
  CsumBytes: TBytes;
  TotalLen: Integer;
  SigI, PK_i: TBytes;
  PkList: TBytes;
  WotsPkAdrs: TCnSlhAddr;
  KP: Cardinal;
begin
  TotalLen := Params.LenTotal;
  SetLength(Msg, TotalLen);

  // Base-w encode message
  SlhBaseWEncode(M, Params.N, Params.W, Params.Len, Msg, 0);

  // Checksum
  CsumBytes := SlhBaseWChecksum(Msg, Params.W, Params.Len1, Params.Len2);

  // Base-w encode checksum
  SlhBaseWEncode(CsumBytes, Length(CsumBytes), Params.W, Params.Len2,
    Msg, Params.Len);

  // Recover public key from signature (all LenTotal chains)
  SetLength(PkList, TotalLen * Params.N);
  for I := 0 to TotalLen - 1 do
  begin
    SlhAddrSetChain(ADRS, I);
    SetLength(SigI, Params.N);
    Move(SIG[I * Params.N], SigI[0], Params.N);
    PK_i := SlhChain(Params, PKSeed, ADRS, SigI, Msg[I], Params.W - 1 - Msg[I]);
    Move(PK_i[0], PkList[I * Params.N], Params.N);
  end;

  // Compress with WOTS_PK address
  KP := SlhReadU32BE(ADRS[CN_ADRS_OFFSET_KEYPAIR]);
  SlhAddrCopy(ADRS, WotsPkAdrs);
  SlhAddrSetTypeAndClear(WotsPkAdrs, CN_SLH_ADRS_WOTS_PK);
  SlhAddrSetKeyPair(WotsPkAdrs, KP);
  Result := SlhTl(Params, PKSeed, WotsPkAdrs, PkList);
end;

// ===================================================================
// XMSS 实现
// ===================================================================

// -------------------------------------------------------------------
// SlhXmssTreeHash - Algorithm 9: 堆栈式 Merkle 树杂凑
// Leaf = wots_PKgen output (NO extra hashing)
// Internal node at height z: H(PK.seed, ADRS[TREE,z,i], left || right)
// -------------------------------------------------------------------
function SlhXmssTreeHash(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes; LeafIdx: Cardinal): TBytes;
var
  StackNodes: array of TBytes;
  StackHeights: array of Integer;
  StackSize: Integer;
  LeafCount: Cardinal;
  I, CurrentHeight: Integer;
  Node, Left, Right: TBytes;
  TmpAdrs: TCnSlhAddr;
begin
  LeafCount := Cardinal(1) shl Params.Hp;
  SetLength(StackNodes, Params.Hp + 1);
  SetLength(StackHeights, Params.Hp + 1);
  StackSize := 0;

  for I := 0 to LeafCount - 1 do
  begin
    // Generate leaf: setTypeAndClear(WOTS_HASH), setKeyPairAddress(i)
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_WOTS_HASH);
    SlhAddrSetKeyPair(TmpAdrs, I);
    // Leaf IS the WOTS+ public key directly (wots_PKgen already includes T_l)
    Node := SlhWotsKeyGen(Params, TmpAdrs, SKSeed, PKSeed);

    CurrentHeight := 0;

    // Merge with stack
    while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
    begin
      Dec(StackSize);
      Left := StackNodes[StackSize];
      Right := Node;

      Inc(CurrentHeight);
      // Internal node: setTypeAndClear(TREE), setTreeHeight(z), setTreeIndex(i)
      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_TREE);
      SlhAddrSetTreeHeight(TmpAdrs, CurrentHeight);
      SlhAddrSetTreeIndex(TmpAdrs, I shr CurrentHeight);
      Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);
    end;

    // Push to stack
    StackNodes[StackSize] := Node;
    StackHeights[StackSize] := CurrentHeight;
    Inc(StackSize);
  end;

  // Root is the only node remaining
  Result := StackNodes[StackSize - 1];
end;

// -------------------------------------------------------------------
// SlhXmssKeyGen - Algorithm 8: XMSS 密钥生成
// 构建 Merkle 树，返回树根
// Input: SK.Seed (n), PK.Seed (n), ADRS
// Output: PK (n)
// -------------------------------------------------------------------
function SlhXmssKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;
begin
  Result := SlhXmssTreeHash(Params, ADRS, SKSeed, PKSeed, 0);
end;

// -------------------------------------------------------------------
// SlhXmssSign - Algorithm 10: XMSS 签名
// 生成 WOTS+ 签名 + 认证路径
// Input: M (n), SK.Seed (n), PK.Seed (n), Idx
// Output: SIG (LenTotal*n + hp*n)
// -------------------------------------------------------------------
function SlhXmssSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const M, SKSeed, PKSeed: TBytes; Idx: Cardinal): TBytes;
var
  StackNodes: array of TBytes;
  StackHeights: array of Integer;
  StackSize: Integer;
  LeafCount: Cardinal;
  I, CurrentHeight: Integer;
  Node, Left, Right: TBytes;
  TmpAdrs: TCnSlhAddr;
  AuthPath: array of TBytes;
  SigWots: TBytes;
  Offset: Integer;
begin
  LeafCount := Cardinal(1) shl Params.Hp;
  SetLength(StackNodes, Params.Hp + 1);
  SetLength(StackHeights, Params.Hp + 1);
  StackSize := 0;

  SetLength(AuthPath, Params.Hp);

  for I := 0 to LeafCount - 1 do
  begin
    // Generate leaf: setTypeAndClear(WOTS_HASH), setKeyPairAddress(i)
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_WOTS_HASH);
    SlhAddrSetKeyPair(TmpAdrs, I);
    // Leaf IS the WOTS+ public key directly
    Node := SlhWotsKeyGen(Params, TmpAdrs, SKSeed, PKSeed);

    CurrentHeight := 0;

    // Merge with stack and track auth path
    while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
    begin
      // Before merging, check if one of the two nodes is an auth path sibling
      if CurrentHeight < Params.Hp then
      begin
        if (I shr (CurrentHeight + 1)) = (Idx shr (CurrentHeight + 1)) then
        begin
          if ((Idx shr CurrentHeight) and 1) = 0 then
            AuthPath[CurrentHeight] := Node
          else
            AuthPath[CurrentHeight] := StackNodes[StackSize - 1];
        end;
      end;

      Dec(StackSize);
      Left := StackNodes[StackSize];
      Right := Node;

      Inc(CurrentHeight);
      // Internal node: setTypeAndClear(TREE), setTreeHeight(z), setTreeIndex(i)
      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_TREE);
      SlhAddrSetTreeHeight(TmpAdrs, CurrentHeight);
      SlhAddrSetTreeIndex(TmpAdrs, I shr CurrentHeight);
      Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);
    end;

    // Push to stack
    StackNodes[StackSize] := Node;
    StackHeights[StackSize] := CurrentHeight;
    Inc(StackSize);
  end;

  // Generate WOTS+ signature for leaf at Idx
  SlhAddrCopy(ADRS, TmpAdrs);
  SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_WOTS_HASH);
  SlhAddrSetKeyPair(TmpAdrs, Idx);
  SigWots := SlhWotsSign(Params, TmpAdrs, M, SKSeed, PKSeed);

  // Build output: SIG_WOTS || auth_path
  SetLength(Result, Params.LenTotal * Params.N + Params.Hp * Params.N);
  Move(SigWots[0], Result[0], Params.LenTotal * Params.N);
  Offset := Params.LenTotal * Params.N;
  for I := 0 to Params.Hp - 1 do
  begin
    if Length(AuthPath[I]) = Params.N then
      Move(AuthPath[I][0], Result[Offset], Params.N)
    else
      FillChar(Result[Offset], Params.N, 0);
    Inc(Offset, Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhXmssPKFromSig - Algorithm 12: 从签名恢复 XMSS 公钥
// Input: Idx, SIG (LenTotal*n + hp*n), M (n), PK.Seed (n), ADRS
// Output: PK (n)
// -------------------------------------------------------------------
function SlhXmssPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  Idx: Cardinal; const SIG, M, PKSeed: TBytes): TBytes;
var
  SigWots, AuthPathNode: TBytes;
  I, Offset: Integer;
  Node: TBytes;
  TmpAdrs: TCnSlhAddr;
begin
  // Parse WOTS+ signature
  SetLength(SigWots, Params.LenTotal * Params.N);
  Move(SIG[0], SigWots[0], Params.LenTotal * Params.N);

  // Recover WOTS+ public key with WOTS_HASH address
  SlhAddrCopy(ADRS, TmpAdrs);
  SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_WOTS_HASH);
  SlhAddrSetKeyPair(TmpAdrs, Idx);
  Node := SlhWotsPKFromSig(Params, TmpAdrs, SigWots, M, PKSeed);

  // Node IS the leaf (no extra H(node,node))

  // Compute root using auth path
  for I := 0 to Params.Hp - 1 do
  begin
    Offset := Params.LenTotal * Params.N + I * Params.N;
    SetLength(AuthPathNode, Params.N);
    Move(SIG[Offset], AuthPathNode[0], Params.N);

    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_TREE);
    SlhAddrSetTreeHeight(TmpAdrs, I + 1);
    SlhAddrSetTreeIndex(TmpAdrs, Idx shr (I + 1));

    if ((Idx shr I) and 1) = 0 then
      Node := SlhH(Params, PKSeed, TmpAdrs, Node, AuthPathNode)
    else
      Node := SlhH(Params, PKSeed, TmpAdrs, AuthPathNode, Node);
  end;

  Result := Node;
end;

// ===================================================================
// Hypertree 实现
// ===================================================================

// -------------------------------------------------------------------
// SlhHtSign - Algorithm 12: Hypertree 签名
// Input: M (n), SK.Seed (n), PK.Seed (n), TreeIdx, LeafIdx
// Output: SIG_HT (d * (LenTotal*n + hp*n))
// -------------------------------------------------------------------
function SlhHtSign(Params: PCnSlhParams; const M, SKSeed, PKSeed: TBytes;
  TreeIdx: TUInt64; LeafIdx: Cardinal): TBytes;
var
  Layer: Integer;
  ADRS: TCnSlhAddr;
  Root, SigXmss: TBytes;
  Offset: Integer;
  LeafMask: TUInt64;
begin
  Root := M;
  SetLength(Result, Params.D * (Params.LenTotal * Params.N + Params.Hp * Params.N));
  Offset := 0;
  LeafMask := (TUInt64(1) shl Params.Hp) - 1;

  for Layer := 0 to Params.D - 1 do
  begin
    SlhAddrInit(ADRS);
    SlhAddrSetLayer(ADRS, Layer);
    SlhAddrSetTree(ADRS, TreeIdx);

    SigXmss := SlhXmssSign(Params, ADRS, Root, SKSeed, PKSeed, LeafIdx);

    Move(SigXmss[0], Result[Offset], Length(SigXmss));
    Inc(Offset, Length(SigXmss));

    if Layer < Params.D - 1 then
    begin
      Root := SlhXmssPKFromSig(Params, ADRS, LeafIdx, SigXmss, Root, PKSeed);
      LeafIdx := TreeIdx and LeafMask;
      TreeIdx := TreeIdx shr Params.Hp;
    end;
  end;
end;

// -------------------------------------------------------------------
// SlhHtVerify - Algorithm 14: Hypertree 验签
// Input: M (n), SIG_HT, PK.Seed (n), TreeIdx, LeafIdx, PK.Root (n)
// Output: Boolean
// -------------------------------------------------------------------
function SlhHtVerify(Params: PCnSlhParams; const M, SIG: TBytes;
  const PKSeed: TBytes; TreeIdx: TUInt64; LeafIdx: Cardinal;
  const PKRoot: TBytes): Boolean;
var
  Layer: Integer;
  ADRS: TCnSlhAddr;
  Node, SigXmss: TBytes;
  Offset: Integer;
  XmssSigLen: Integer;
  LeafMask: TUInt64;
  ComputedRoot: TBytes;
begin
  Node := M;
  XmssSigLen := Params.LenTotal * Params.N + Params.Hp * Params.N;
  LeafMask := (TUInt64(1) shl Params.Hp) - 1;
  Offset := 0;
  Result := True;

  for Layer := 0 to Params.D - 1 do
  begin
    SlhAddrInit(ADRS);
    SlhAddrSetLayer(ADRS, Layer);
    SlhAddrSetTree(ADRS, TreeIdx);

    // Extract XMSS signature for this layer
    SetLength(SigXmss, XmssSigLen);
    Move(SIG[Offset], SigXmss[0], XmssSigLen);
    Inc(Offset, XmssSigLen);

    ComputedRoot := SlhXmssPKFromSig(Params, ADRS, LeafIdx, SigXmss, Node, PKSeed);

    if Layer = Params.D - 1 then
    begin
      // Last layer: compare with PKRoot
      Result := (Length(ComputedRoot) = Length(PKRoot))
        and ConstTimeCompareMem(@ComputedRoot[0], @PKRoot[0], Length(PKRoot));
      if not Result then
        Exit;
    end
    else
    begin
      Node := ComputedRoot;
      LeafIdx := TreeIdx and LeafMask;
      TreeIdx := TreeIdx shr Params.Hp;
    end;
  end;
end;

// ===================================================================
// FORS 实现
// ===================================================================

// -------------------------------------------------------------------
// SlhForsTreeHash - FIPS 205 Algorithm 14: 构建一棵 FORS Merkle 树并返回根
// TreeIdx = FORS tree number (j), global index = j*2^a + i
// ADRS KeyPair already set by caller to idx_leaf
// -------------------------------------------------------------------
function SlhForsTreeHash(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes; TreeIdx: Byte): TBytes;
var
  StackNodes: array of TBytes;
  StackHeights: array of Integer;
  StackSize: Integer;
  LeafCount, GlobalBase: Cardinal;
  I, CurrentHeight: Integer;
  Node, Left, Right, SK: TBytes;
  TmpAdrs: TCnSlhAddr;
  KP: Cardinal;
begin
  LeafCount := Cardinal(1) shl Params.A;
  GlobalBase := Cardinal(TreeIdx) * LeafCount;
  SetLength(StackNodes, Params.A + 1);
  SetLength(StackHeights, Params.A + 1);
  StackSize := 0;

  // Preserve KeyPair from caller
  KP := SlhReadU32BE(ADRS[CN_ADRS_OFFSET_KEYPAIR]);

  for I := 0 to LeafCount - 1 do
  begin
    // PRF: generate leaf private key
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_PRF);
    SlhAddrSetKeyPair(TmpAdrs, KP);
    SlhAddrSetTreeIndex(TmpAdrs, GlobalBase + Cardinal(I));
    SK := SlhPRF(Params, PKSeed, SKSeed, TmpAdrs);

    // Leaf = F(PK.seed, ADRS[FORS_TREE], SK_i)
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
    SlhAddrSetKeyPair(TmpAdrs, KP);
    SlhAddrSetTreeHeight(TmpAdrs, 0);
    SlhAddrSetTreeIndex(TmpAdrs, GlobalBase + Cardinal(I));
    Node := SlhF(Params, PKSeed, TmpAdrs, SK);

    CurrentHeight := 0;

    // Merge with stack
    while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
    begin
      Dec(StackSize);
      Left := StackNodes[StackSize];
      Right := Node;

      Inc(CurrentHeight);
      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
      SlhAddrSetKeyPair(TmpAdrs, KP);
      SlhAddrSetTreeHeight(TmpAdrs, CurrentHeight);
      SlhAddrSetTreeIndex(TmpAdrs, (GlobalBase + Cardinal(I)) shr CurrentHeight);
      Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);
    end;

    StackNodes[StackSize] := Node;
    StackHeights[StackSize] := CurrentHeight;
    Inc(StackSize);
  end;

  Result := StackNodes[StackSize - 1];
end;

// -------------------------------------------------------------------
// SlhForsKeyGen - FORS 密钥生成
// 生成 k 棵 FORS 树，返回 k 个根拼接（k*n 字节）
// Input: ADRS (type=FORS_ROOTS), SK.Seed (n), PK.Seed (n)
// Output: k*n 字节（k 个根拼接）
// -------------------------------------------------------------------
function SlhForsKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;
var
  I: Integer;
  TmpAdrs: TCnSlhAddr;
  Root: TBytes;
begin
  SetLength(Result, Params.K * Params.N);
  for I := 0 to Params.K - 1 do
  begin
    SlhAddrCopy(ADRS, TmpAdrs);
    Root := SlhForsTreeHash(Params, TmpAdrs, SKSeed, PKSeed, I);
    Move(Root[0], Result[I * Params.N], Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhForsSign - Algorithm 16: FORS 签名
// Input: Md (message digest part), SK.Seed (n), PK.Seed (n), ADRS
// Output: SIG_FORS = k * (sk + auth_path) = k * (a+1) * n 字节
// -------------------------------------------------------------------
function SlhForsSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const Md, SKSeed, PKSeed: TBytes): TBytes;
var
  TreeI: Integer;
  LeafIdx, I, CurrentHeight: Integer;
  StackNodes: array of TBytes;
  StackHeights: array of Integer;
  StackSize: Integer;
  LeafCount, GlobalBase, GlobalI: Cardinal;
  Node, Left, Right, SK: TBytes;
  TmpAdrs: TCnSlhAddr;
  AuthPath: array of TBytes;
  Offset: Integer;
  BitIdx: Integer;
  KP: Cardinal;
begin
  SetLength(Result, Params.K * (Params.A + 1) * Params.N);
  Offset := 0;
  KP := SlhReadU32BE(ADRS[CN_ADRS_OFFSET_KEYPAIR]);

  for TreeI := 0 to Params.K - 1 do
  begin
    // Extract leaf index from Md (a bits per tree)
    LeafIdx := 0;
    for I := 0 to Params.A - 1 do
    begin
      BitIdx := TreeI * Params.A + I;
      if (Md[BitIdx div 8] and (1 shl (7 - (BitIdx mod 8)))) <> 0 then
        LeafIdx := LeafIdx or (1 shl (Params.A - 1 - I));
    end;

    // Treehash with auth path tracking
    LeafCount := Cardinal(1) shl Params.A;
    GlobalBase := Cardinal(TreeI) * LeafCount;
    SetLength(StackNodes, Params.A + 1);
    SetLength(StackHeights, Params.A + 1);
    StackSize := 0;
    SetLength(AuthPath, Params.A);

    for I := 0 to LeafCount - 1 do
    begin
      GlobalI := GlobalBase + Cardinal(I);

      // PRF: generate leaf private key
      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_PRF);
      SlhAddrSetKeyPair(TmpAdrs, KP);
      SlhAddrSetTreeIndex(TmpAdrs, GlobalI);
      SK := SlhPRF(Params, PKSeed, SKSeed, TmpAdrs);

      // Leaf = F(PK.seed, ADRS[FORS_TREE], SK_i)
      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
      SlhAddrSetKeyPair(TmpAdrs, KP);
      SlhAddrSetTreeHeight(TmpAdrs, 0);
      SlhAddrSetTreeIndex(TmpAdrs, GlobalI);
      Node := SlhF(Params, PKSeed, TmpAdrs, SK);

      CurrentHeight := 0;

      while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
      begin
        // Track auth path
        if CurrentHeight < Params.A then
        begin
          if (I shr (CurrentHeight + 1)) = (LeafIdx shr (CurrentHeight + 1)) then
          begin
            if ((LeafIdx shr CurrentHeight) and 1) = 0 then
              AuthPath[CurrentHeight] := Node
            else
              AuthPath[CurrentHeight] := StackNodes[StackSize - 1];
          end;
        end;

        Dec(StackSize);
        Left := StackNodes[StackSize];
        Right := Node;

        Inc(CurrentHeight);
        SlhAddrCopy(ADRS, TmpAdrs);
        SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
        SlhAddrSetKeyPair(TmpAdrs, KP);
        SlhAddrSetTreeHeight(TmpAdrs, CurrentHeight);
        SlhAddrSetTreeIndex(TmpAdrs, GlobalI shr CurrentHeight);
        Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);
      end;

      StackNodes[StackSize] := Node;
      StackHeights[StackSize] := CurrentHeight;
      Inc(StackSize);
    end;

    // Save SK for selected leaf
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_PRF);
    SlhAddrSetKeyPair(TmpAdrs, KP);
    SlhAddrSetTreeIndex(TmpAdrs, GlobalBase + Cardinal(LeafIdx));
    SK := SlhPRF(Params, PKSeed, SKSeed, TmpAdrs);
    Move(SK[0], Result[Offset], Params.N);
    Inc(Offset, Params.N);

    // Save auth path
    for I := 0 to Params.A - 1 do
    begin
      if Length(AuthPath[I]) = Params.N then
        Move(AuthPath[I][0], Result[Offset], Params.N)
      else
        FillChar(Result[Offset], Params.N, 0);
      Inc(Offset, Params.N);
    end;
  end;
end;

// -------------------------------------------------------------------
// SlhForsPKFromSig - Algorithm 16: 从签名恢复 FORS 公钥
// Input: SIG_FORS, Md, PK.Seed, ADRS
// Output: k*n 字节（k 个根拼接）
// -------------------------------------------------------------------
function SlhForsPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SIG, Md, PKSeed: TBytes): TBytes;
var
  TreeI, I, LeafIdx, BitIdx: Integer;
  Node, Sibling: TBytes;
  TmpAdrs: TCnSlhAddr;
  Offset: Integer;
  KP, GlobalBase, LeafCount: Cardinal;
begin
  SetLength(Result, Params.K * Params.N);
  Offset := 0;

  // Preserve KeyPair from caller
  KP := SlhReadU32BE(ADRS[CN_ADRS_OFFSET_KEYPAIR]);
  LeafCount := Cardinal(1) shl Params.A;

  for TreeI := 0 to Params.K - 1 do
  begin
    GlobalBase := Cardinal(TreeI) * LeafCount;

    // Extract leaf index from Md
    LeafIdx := 0;
    for I := 0 to Params.A - 1 do
    begin
      BitIdx := TreeI * Params.A + I;
      if (Md[BitIdx div 8] and (1 shl (7 - (BitIdx mod 8)))) <> 0 then
        LeafIdx := LeafIdx or (1 shl (Params.A - 1 - I));
    end;

    // Extract SK from signature
    SetLength(Node, Params.N);
    Move(SIG[Offset], Node[0], Params.N);
    Inc(Offset, Params.N);

    // Compute leaf = F(PK.seed, ADRS, SK)
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
    SlhAddrSetKeyPair(TmpAdrs, KP);
    SlhAddrSetTreeHeight(TmpAdrs, 0);
    SlhAddrSetTreeIndex(TmpAdrs, GlobalBase + Cardinal(LeafIdx));
    Node := SlhF(Params, PKSeed, TmpAdrs, Node);

    // Compute root using auth path
    for I := 0 to Params.A - 1 do
    begin
      SetLength(Sibling, Params.N);
      Move(SIG[Offset + I * Params.N], Sibling[0], Params.N);

      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetTypeAndClear(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
      SlhAddrSetKeyPair(TmpAdrs, KP);
      SlhAddrSetTreeHeight(TmpAdrs, I + 1);
      SlhAddrSetTreeIndex(TmpAdrs, (GlobalBase + Cardinal(LeafIdx)) shr (I + 1));

      if ((LeafIdx shr I) and 1) = 0 then
        Node := SlhH(Params, PKSeed, TmpAdrs, Node, Sibling)
      else
        Node := SlhH(Params, PKSeed, TmpAdrs, Sibling, Node);
    end;

    // Save root
    Move(Node[0], Result[TreeI * Params.N], Params.N);
    Inc(Offset, Params.A * Params.N);
  end;
end;

// ===================================================================
// TCnSlhContext
// ===================================================================

constructor TCnSLHDSA.Create(AParamSet: TCnSlhParamSet);
begin
  inherited Create;
  FParamSet := AParamSet;
  FParams := SlhGetParams(AParamSet);
  FCancel := False;
  SlhInitHashFuncs(FParams, FHashFuncs);
end;

destructor TCnSLHDSA.Destroy;
begin
  inherited;
end;

// -------------------------------------------------------------------
// 顶层 API（Algorithm 21-23）
// -------------------------------------------------------------------

function SlhBytesToUInt64BE(const Data: TBytes; Offset, Len: Integer): TUInt64;
var
  Tmp: array[0..7] of Byte;
  CopyLen: Integer;
begin
  if Len >= 8 then
    CopyLen := 8
  else
    CopyLen := Len;
  FillChar(Tmp, SizeOf(Tmp), 0);
  Move(Data[Offset + Len - CopyLen], Tmp[8 - CopyLen], CopyLen);
  Result := UInt64NetworkToHost(PUInt64(@Tmp)^);
end;

procedure TCnSLHDSA.GenerateKeys(PK: TCnSlhPublicKey;
  SK: TCnSlhSecretKey);
var
  ADRS: TCnSlhAddr;
begin
  PK.Seed := CnRandomBytes(FParams.N);
  SK.Seed := CnRandomBytes(FParams.N);
  SK.Prf := CnRandomBytes(FParams.N);

  // Compute PK.root = root of top-most XMSS tree (layer d-1, tree 0)
  SlhAddrInit(ADRS);
  SlhAddrSetLayer(ADRS, FParams.D - 1);
  SlhAddrSetTree(ADRS, 0);
  PK.Root := SlhXmssKeyGen(FParams, ADRS, SK.Seed, PK.Seed);

  SK.PKSeed := PK.Seed;
  SK.PKRoot := PK.Root;
end;

function TCnSLHDSA.SignInternal(const MPrime: TBytes;
  const SK: TCnSlhSecretKey; const AddRnd: TBytes): TCnSlhSignature;
var
  R, Digest, Md: TBytes;
  MdLen, TreeLen, LeafLen: Integer;
  TreeIdx: TUInt64;
  LeafIdx: Cardinal;
  ADRS: TCnSlhAddr;
  SigFors, SigHt, PKFors, PKForsCompressed: TBytes;
  ForsSigLen, HtSigLen, TotalLen: Integer;
  P: PCnSlhParams;
begin
  P := FParams;

  // R = PRF_msg(SK.Prf, OptRand, M)
  R := SlhPRFMsg(P, SK.Prf, AddRnd, MPrime);

  // Digest = H_msg(R, PK.Seed, PK.Root, M)
  Digest := SlhHMsg(P, R, SK.PKSeed, SK.PKRoot, MPrime);

  // Split Digest into three byte-aligned blocks per FIPS 205
  MdLen := (P.K * P.A + 7) div 8;
  TreeLen := (P.H - P.Hp + 7) div 8;
  LeafLen := (P.Hp + 7) div 8;
  SetLength(Md, MdLen);
  Move(Digest[0], Md[0], MdLen);

  TreeIdx := SlhBytesToUInt64BE(Digest, MdLen, TreeLen);
  if P.H - P.Hp < 64 then
    TreeIdx := TreeIdx and ((TUInt64(1) shl (P.H - P.Hp)) - 1);

  LeafIdx := Cardinal(SlhBytesToUInt64BE(Digest, MdLen + TreeLen, LeafLen));
  LeafIdx := LeafIdx and ((Cardinal(1) shl P.Hp) - 1);

  // FORS signature
  SlhAddrInit(ADRS);
  SlhAddrSetTree(ADRS, TreeIdx);
  SlhAddrSetTypeAndClear(ADRS, CN_SLH_ADRS_FORS_TREE);
  SlhAddrSetKeyPair(ADRS, LeafIdx);
  SigFors := SlhForsSign(P, ADRS, Md, SK.Seed, SK.PKSeed);

  // PK_FORS from signature
  PKFors := SlhForsPKFromSig(P, ADRS, SigFors, Md, SK.PKSeed);
  SlhAddrSetTypeAndClear(ADRS, CN_SLH_ADRS_FORS_ROOTS);
  SlhAddrSetKeyPair(ADRS, LeafIdx);
  PKForsCompressed := SlhTl(P, SK.PKSeed, ADRS, PKFors);

  // Hypertree signature
  SigHt := SlhHtSign(P, PKForsCompressed, SK.Seed, SK.PKSeed, TreeIdx, LeafIdx);

  // Combine: R || SIG_FORS || SIG_HT
  ForsSigLen := P.K * (P.A + 1) * P.N;
  HtSigLen := P.D * (P.LenTotal * P.N + P.Hp * P.N);
  TotalLen := P.N + ForsSigLen + HtSigLen;
  SetLength(Result, TotalLen);
  Move(R[0], Result[0], P.N);
  Move(SigFors[0], Result[P.N], ForsSigLen);
  Move(SigHt[0], Result[P.N + ForsSigLen], HtSigLen);
end;

function TCnSLHDSA.SignBytes(const SK: TCnSlhSecretKey; const Msg: TBytes;
  Randomize: Boolean): TCnSlhSignature;
var
  OptRand: TBytes;
begin
  if Randomize then
    OptRand := CnRandomBytes(FParams.N)
  else
    OptRand := Copy(SK.PKSeed, 0, FParams.N);

  Result := SignInternal(Msg, SK, OptRand);
end;

function TCnSLHDSA.VerifyBytes(const PK: TCnSlhPublicKey; const Msg: TBytes;
  const SIG: TCnSlhSignature): Boolean;
var
  P: PCnSlhParams;
  R, Digest, Md: TBytes;
  SigFors, SigHt, PKFors, PKForsCompressed: TBytes;
  MdLen, TreeLen, LeafLen: Integer;
  TreeIdx: TUInt64;
  LeafIdx: Cardinal;
  ForsSigLen, HtSigLen: Integer;
  ADRS: TCnSlhAddr;
begin
  P := FParams;
  Result := False;

  ForsSigLen := P.K * (P.A + 1) * P.N;
  HtSigLen := P.D * (P.LenTotal * P.N + P.Hp * P.N);

  // Check minimum signature size
  if Length(SIG) < P.N + ForsSigLen + HtSigLen then
    Exit;

  // Parse signature
  SetLength(R, P.N);
  Move(SIG[0], R[0], P.N);

  SetLength(SigFors, ForsSigLen);
  Move(SIG[P.N], SigFors[0], ForsSigLen);

  SetLength(SigHt, HtSigLen);
  Move(SIG[P.N + ForsSigLen], SigHt[0], HtSigLen);

  // Digest = H_msg(R, PK.Seed, PK.Root, M)
  Digest := SlhHMsg(P, R, PK.Seed, PK.Root, Msg);

  // Split Digest into three byte-aligned blocks per FIPS 205
  MdLen := (P.K * P.A + 7) div 8;
  TreeLen := (P.H - P.Hp + 7) div 8;
  LeafLen := (P.Hp + 7) div 8;
  SetLength(Md, MdLen);
  Move(Digest[0], Md[0], MdLen);

  TreeIdx := SlhBytesToUInt64BE(Digest, MdLen, TreeLen);
  if P.H - P.Hp < 64 then
    TreeIdx := TreeIdx and ((TUInt64(1) shl (P.H - P.Hp)) - 1);

  LeafIdx := Cardinal(SlhBytesToUInt64BE(Digest, MdLen + TreeLen, LeafLen));
  LeafIdx := LeafIdx and ((Cardinal(1) shl P.Hp) - 1);

  // Recover PK_FORS from signature
  SlhAddrInit(ADRS);
  SlhAddrSetTree(ADRS, TreeIdx);
  SlhAddrSetTypeAndClear(ADRS, CN_SLH_ADRS_FORS_TREE);
  SlhAddrSetKeyPair(ADRS, LeafIdx);
  try
    PKFors := SlhForsPKFromSig(P, ADRS, SigFors, Md, PK.Seed);
  except
    Exit;
  end;

  // Compress FORS roots
  SlhAddrSetTypeAndClear(ADRS, CN_SLH_ADRS_FORS_ROOTS);
  SlhAddrSetKeyPair(ADRS, LeafIdx);
  PKForsCompressed := SlhTl(P, PK.Seed, ADRS, PKFors);

  // Verify Hypertree
  Result := SlhHtVerify(P, PKForsCompressed, SigHt, PK.Seed,
    TreeIdx, LeafIdx, PK.Root);
end;

// -------------------------------------------------------------------
// Prehash 模式（Algorithm 24-27）
// -------------------------------------------------------------------

function SlhGetPrehashHashIDByte(HashID: TCnSlhPrehashID): Byte;
begin
  Result := Ord(HashID) + 1;
end;

function SlhGetPrehashOutputLen(HashID: TCnSlhPrehashID): Integer;
begin
  case HashID of
    shiSHA2_224, shiSHA3_224, shiSHA2_512_224: Result := 28;
    shiSHA2_256, shiSHA3_256, shiSHA2_512_256, shiSHAKE128, shiSM3: Result := 32;
    shiSHA2_384, shiSHA3_384: Result := 48;
    shiSHA2_512, shiSHA3_512: Result := 64;
    shiSHAKE256: Result := 64;
  else
    raise ECnSlhException.Create('Unknown Prehash ID');
  end;
end;

function SlhPrehashData(const M: TBytes; HashID: TCnSlhPrehashID): TBytes;
var
  D: TBytes;
  HashIDByte: Byte;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
  D224: TCnSHA224Digest;
  D384: TCnSHA384Digest;
  D512_224: TCnSHA512_224Digest;
  D512_256: TCnSHA512_256Digest;
  D3_224: TCnSHA3_224Digest;
  D3_256: TCnSHA3_256Digest;
  D3_384: TCnSHA3_384Digest;
  D3_512: TCnSHA3_512Digest;
  DSM3: TCnSM3Digest;
begin
  case HashID of
    shiSHA2_224:
      begin D224 := SHA224Bytes(M); SetLength(D, 28); Move(D224, D[0], 28); end;
    shiSHA2_256:
      begin D256 := SHA256Bytes(M); SetLength(D, 32); Move(D256, D[0], 32); end;
    shiSHA2_384:
      begin D384 := SHA384Bytes(M); SetLength(D, 48); Move(D384, D[0], 48); end;
    shiSHA2_512:
      begin D512 := SHA512Bytes(M); SetLength(D, 64); Move(D512, D[0], 64); end;
    shiSHA2_512_224:
      begin D512_224 := SHA512_224Bytes(M); SetLength(D, 28); Move(D512_224, D[0], 28); end;
    shiSHA2_512_256:
      begin D512_256 := SHA512_256Bytes(M); SetLength(D, 32); Move(D512_256, D[0], 32); end;
    shiSHA3_224:
      begin D3_224 := SHA3_224Bytes(M); SetLength(D, 28); Move(D3_224, D[0], 28); end;
    shiSHA3_256:
      begin D3_256 := SHA3_256Bytes(M); SetLength(D, 32); Move(D3_256, D[0], 32); end;
    shiSHA3_384:
      begin D3_384 := SHA3_384Bytes(M); SetLength(D, 48); Move(D3_384, D[0], 48); end;
    shiSHA3_512:
      begin D3_512 := SHA3_512Bytes(M); SetLength(D, 64); Move(D3_512, D[0], 64); end;
    shiSHAKE128:
      D := SHAKE128Bytes(M, 32);
    shiSHAKE256:
      D := SHAKE256Bytes(M, 64);
    shiSM3:
      begin DSM3 := SM3Bytes(M); SetLength(D, 32); Move(DSM3, D[0], 32); end;
  else
    raise ECnSlhException.Create('Unknown Prehash ID');
  end;

  HashIDByte := SlhGetPrehashHashIDByte(HashID);
  SetLength(Result, 3 + Length(D));
  Result[0] := $00;
  Result[1] := $01;
  Result[2] := HashIDByte;
  Move(D[0], Result[3], Length(D));
end;

function TCnSLHDSA.SignPreHash(const M: TBytes;
  const SK: TCnSlhSecretKey; HashID: TCnSlhPrehashID;
  Randomize: Boolean): TCnSlhSignature;
var
  MPrime, OptRand: TBytes;
begin
  MPrime := SlhPrehashData(M, HashID);

  if Randomize then
    OptRand := CnRandomBytes(FParams.N)
  else
    OptRand := Copy(SK.PKSeed, 0, FParams.N);

  Result := SignInternal(MPrime, SK, OptRand);
end;

function TCnSLHDSA.VerifyPreHash(const M: TBytes;
  const SIG: TCnSlhSignature; const PK: TCnSlhPublicKey;
  HashID: TCnSlhPrehashID): Boolean;
var
  MPrime: TBytes;
begin
  MPrime := SlhPrehashData(M, HashID);
  Result := VerifyBytes(PK, MPrime, SIG);
end;

// -------------------------------------------------------------------
// 序列化 API
// -------------------------------------------------------------------
function TCnSLHDSA.PublicKeyToBytes(
  const PK: TCnSlhPublicKey): TBytes;
begin
  SetLength(Result, FParams.N * 2);
  Move(PK.Seed[0], Result[0], FParams.N);
  Move(PK.Root[0], Result[FParams.N], FParams.N);
end;

procedure TCnSLHDSA.BytesToPublicKey(
  PK: TCnSlhPublicKey; const Data: TBytes);
begin
  if Length(Data) < FParams.N * 2 then
    raise ECnSlhException.Create('Invalid public key data length');
  SetLength(PK.Seed, FParams.N);
  SetLength(PK.Root, FParams.N);
  Move(Data[0], PK.Seed[0], FParams.N);
  Move(Data[FParams.N], PK.Root[0], FParams.N);
end;

function TCnSLHDSA.SecretKeyToBytes(
  const SK: TCnSlhSecretKey): TBytes;
begin
  SetLength(Result, FParams.N * 4);
  Move(SK.Seed[0], Result[0], FParams.N);
  Move(SK.Prf[0], Result[FParams.N], FParams.N);
  Move(SK.PKSeed[0], Result[FParams.N * 2], FParams.N);
  Move(SK.PKRoot[0], Result[FParams.N * 3], FParams.N);
end;

procedure TCnSLHDSA.BytesToSecretKey(
  SK: TCnSlhSecretKey; const Data: TBytes);
begin
  if Length(Data) < FParams.N * 4 then
    raise ECnSlhException.Create('Invalid secret key data length');
  SetLength(SK.Seed, FParams.N);
  SetLength(SK.Prf, FParams.N);
  SetLength(SK.PKSeed, FParams.N);
  SetLength(SK.PKRoot, FParams.N);
  Move(Data[0], SK.Seed[0], FParams.N);
  Move(Data[FParams.N], SK.Prf[0], FParams.N);
  Move(Data[FParams.N * 2], SK.PKSeed[0], FParams.N);
  Move(Data[FParams.N * 3], SK.PKRoot[0], FParams.N);
end;

function TCnSLHDSA.SignatureToBytes(
  const SIG: TCnSlhSignature): TBytes;
begin
  Result := SIG;
end;

function TCnSLHDSA.BytesToSignature(
  const Data: TBytes): TCnSlhSignature;
begin
  Result := Data;
end;

function TCnSLHDSA.GetPublicKeySize: Integer;
begin
  Result := FParams.N * 2;
end;

function TCnSLHDSA.GetSecretKeySize: Integer;
begin
  Result := FParams.N * 4;
end;

function TCnSLHDSA.GetSignatureSize: Integer;
var
  P: PCnSlhParams;
begin
  P := FParams;
  // SIG = R (n) + SIG_FORS (k*(a+1)*n) + SIG_HT (d*(LenTotal*n + hp*n))
  Result := P.N                                     // R
          + P.K * (P.A + 1) * P.N                   // SIG_FORS
          + P.D * (P.LenTotal * P.N + P.Hp * P.N);  // SIG_HT
end;

end.
