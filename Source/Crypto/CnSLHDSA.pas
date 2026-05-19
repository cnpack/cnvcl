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
* 单元名称：后量子数字签名单元
* 单元名称：SLH-DSA 后量子数字签名算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 修   注：本单元实现了 NIST FIPS 205 规范中的 SLH-DSA
*           （Stateless Hash-Based Digital Signature Algorithm），
*           即无状态杂凑数字签名算法（原 SPHINCS+）。
*
* 开发平台：Win10 + Delphi 12.0
* 内容测试：尚未测试
* 说    明：本单元使用 SHA2/SHA3 等底层杂凑，需 CnSHA2 和 CnSHA3 单元。
* 修改记录：2026.05.18 V1.0
*               创建单元，实现初始架构
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes,
  CnSHA2, CnSHA3, CnRandom, CnBits, CnNative;

const
  // 地址类型常量
  CN_SLH_ADRS_WOTS_HASH  = 0;
  CN_SLH_ADRS_WOTS_PK    = 1;
  CN_SLH_ADRS_TREE       = 2;
  CN_SLH_ADRS_FORS_TREE  = 3;
  CN_SLH_ADRS_FORS_ROOTS = 4;
  CN_SLH_ADRS_WOTS_PRF   = 5;
  CN_SLH_ADRS_FORS_PRF   = 6;

  // 地址偏移常量
  CN_ADRS_OFFSET_LAYER    = 0;
  CN_ADRS_OFFSET_TREE     = 4;
  CN_ADRS_OFFSET_TYPE     = 12;
  CN_ADRS_OFFSET_KEYPAIR  = 16;
  CN_ADRS_OFFSET_CHAIN    = 20;
  CN_ADRS_OFFSET_HASH     = 24;
  CN_ADRS_OFFSET_ZERO     = 28;

  CN_ADRS_SIZE_SHAKE      = 32;
  CN_ADRS_SIZE_SHA2       = 22;

type
  ECnSlhException = class(Exception);

  // -------------------------------------------------------------------
  // 地址类型：SHAKE 32 字节，SHA2 22 字节
  // -------------------------------------------------------------------
  PCnSlhAddr = ^TCnSlhAddr;
  TCnSlhAddr = array[0..31] of Byte;

  TCnSlhAddrSHA2 = array[0..21] of Byte;

  // -------------------------------------------------------------------
  // 安全参数缓冲区
  // -------------------------------------------------------------------
  TCnSlhBytes16 = array[0..15] of Byte;
  TCnSlhBytes24 = array[0..23] of Byte;
  TCnSlhBytes32 = array[0..31] of Byte;
  TCnSlhDigest = TBytes;

  // -------------------------------------------------------------------
  // 参数集枚举
  // -------------------------------------------------------------------
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

  // -------------------------------------------------------------------
  // 参数记录
  // -------------------------------------------------------------------
  PCnSlhParams = ^TCnSlhParams;
  TCnSlhParams = record
    N: Byte;            // 安全参数（字节）: 16/24/32
    H: Byte;            // Hypertree 总高度: 63/64/66/68
    D: Byte;            // Hypertree 层数: 7/8/17/22
    Hp: Byte;           // 每层高度 h' = h/d
    A: Byte;            // FORS 树高度 (2^a 叶节点)
    K: Byte;            // FORS 树数量
    W: Byte;            // Winternitz 参数: w = 16 (固定)
    M: Byte;            // 消息摘要长度（字节）
    // 派生常量
    Len: Byte;          // WOTS+ 链数
    Len1: Byte;
    Len2: Byte;
    LenTotal: Byte;
    // 杂凑家族标识
    IsSHA2: Boolean;
  end;

  // -------------------------------------------------------------------
  // 密钥与签名类型
  // -------------------------------------------------------------------
  TCnSlhPublicKey = record
    Seed: TBytes;
    Root: TBytes;
  end;

  TCnSlhSecretKey = record
    Seed: TBytes;
    Prf: TBytes;
    PKSeed: TBytes;
    PKRoot: TBytes;
  end;

  TCnSlhSignature = TBytes;

  // -------------------------------------------------------------------
  // 进度回调
  // -------------------------------------------------------------------
  TCnSlhProgressEvent = procedure(Percent: Integer; var Cancel: Boolean) of object;

  // -------------------------------------------------------------------
  // Prehash 标识
  // -------------------------------------------------------------------
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
    shiSHAKE256
  );

  // -------------------------------------------------------------------
  // 前向声明
  // -------------------------------------------------------------------
  TCnSLHDSA = class;

  // -------------------------------------------------------------------
  // 杂凑函数表
  // -------------------------------------------------------------------
  TCnSlhFFunc = function(AParams: PCnSlhParams; const PKSeed: TBytes;
    var ADRS: TCnSlhAddr; const M1: TBytes): TBytes;

  TCnSlhHFunc = function(AParams: PCnSlhParams; const PKSeed: TBytes;
    var ADRS: TCnSlhAddr; const M1, M2: TBytes): TBytes;

  TCnSlhTlFunc = function(AParams: PCnSlhParams; const PKSeed: TBytes;
    var ADRS: TCnSlhAddr; const M: TBytes): TBytes;

  TCnSlhPRFFunc = function(AParams: PCnSlhParams; const PKSeed, SKSeed: TBytes;
    var ADRS: TCnSlhAddr): TBytes;

  TCnSlhPRFMsgFunc = function(AParams: PCnSlhParams;
    const SKPrf, OptRand, M: TBytes): TBytes;

  TCnSlhHMsgFunc = function(AParams: PCnSlhParams;
    const R, PKSeed, PKRoot, M: TBytes): TBytes;

  TCnSlhHashFuncs = record
    F: TCnSlhFFunc;
    H: TCnSlhHFunc;
    T_l: TCnSlhTlFunc;
    PRF: TCnSlhPRFFunc;
    PRF_msg: TCnSlhPRFMsgFunc;
    H_msg: TCnSlhHMsgFunc;
  end;

  // -------------------------------------------------------------------
  // 主上下文类
  // -------------------------------------------------------------------
  TCnSLHDSA = class
  private
    FParams: PCnSlhParams;
    FParamSet: TCnSlhParamSet;
    FOnProgress: TCnSlhProgressEvent;
    FCancel: Boolean;
    FHashFuncs: TCnSlhHashFuncs;
  public
    constructor Create(AParamSet: TCnSlhParamSet);
    destructor Destroy; override;

    property ParamSet: TCnSlhParamSet read FParamSet;
    property Params: PCnSlhParams read FParams;
    property OnProgress: TCnSlhProgressEvent read FOnProgress write FOnProgress;

    // 顶层 API
    procedure GenerateKeys(out PK: TCnSlhPublicKey; out SK: TCnSlhSecretKey);
    function Sign(const M: TBytes; const SK: TCnSlhSecretKey;
      Randomize: Boolean = True): TCnSlhSignature;
    function Verify(const M: TBytes; const SIG: TCnSlhSignature;
      const PK: TCnSlhPublicKey): Boolean;
    function SignInternal(const MPrime: TBytes;
      const SK: TCnSlhSecretKey; const AddRnd: TBytes): TCnSlhSignature;

    // Prehash 模式
    function SignPreHash(const M: TBytes; const SK: TCnSlhSecretKey;
      HashID: TCnSlhPrehashID; Randomize: Boolean = True): TCnSlhSignature;
    function VerifyPreHash(const M: TBytes; const SIG: TCnSlhSignature;
      const PK: TCnSlhPublicKey; HashID: TCnSlhPrehashID): Boolean;

    // 序列化
    function PublicKeyToBytes(const PK: TCnSlhPublicKey): TBytes;
    function BytesToPublicKey(const Data: TBytes): TCnSlhPublicKey;
    function SecretKeyToBytes(const SK: TCnSlhSecretKey): TBytes;
    function BytesToSecretKey(const Data: TBytes): TCnSlhSecretKey;
    function SignatureToBytes(const SIG: TCnSlhSignature): TBytes;
    function BytesToSignature(const Data: TBytes): TCnSlhSignature;

    // 大小查询
    function GetPublicKeySize: Integer;
    function GetSecretKeySize: Integer;
    function GetSignatureSize: Integer;
  end;

// -------------------------------------------------------------------
// 全局辅助函数
// -------------------------------------------------------------------

// 参数查询
function SlhGetParams(AParamSet: TCnSlhParamSet): PCnSlhParams;
function SlhParamSetName(AParamSet: TCnSlhParamSet): string;
function SlhParamSetFromName(const AName: string): TCnSlhParamSet;

// 大端读写
function SlhReadU32BE(const Buf): Cardinal;
function SlhReadU64BE(const Buf): TUInt64;
procedure SlhWriteU32BE(var Buf; Value: Cardinal);
procedure SlhWriteU64BE(var Buf; Value: TUInt64);

// ADRS 地址操作
procedure SlhAddrInit(var AD: TCnSlhAddr);
procedure SlhAddrSetLayer(var AD: TCnSlhAddr; Layer: Cardinal);
procedure SlhAddrSetTree(var AD: TCnSlhAddr; Tree: TUInt64);
procedure SlhAddrSetType(var AD: TCnSlhAddr; Typ: Cardinal);
procedure SlhAddrSetKeyPair(var AD: TCnSlhAddr; Pair: Cardinal);
procedure SlhAddrSetChain(var AD: TCnSlhAddr; Chain: Cardinal);
procedure SlhAddrSetHash(var AD: TCnSlhAddr; Hash: Cardinal);
procedure SlhAddrSetTreeHeight(var AD: TCnSlhAddr; Height: Cardinal);
procedure SlhAddrSetTreeIndex(var AD: TCnSlhAddr; Index: Cardinal);
procedure SlhAddrCopy(const Src: TCnSlhAddr; var Dst: TCnSlhAddr);
procedure SlhAddrCompress(const Src: TCnSlhAddr; var Dst: TCnSlhAddrSHA2);
procedure SlhAddrDecompress(const Src: TCnSlhAddrSHA2; var Dst: TCnSlhAddr);
function SlhAddrSize(IsSHA2: Boolean): Integer;
function SlhAddrToBytes(const AD: TCnSlhAddr; IsSHA2: Boolean): TBytes;

// SHA2 辅助：MGF1
function SlhMGF1_SHA256(const Seed: TBytes; OutLen: Integer): TBytes;
function SlhMGF1_SHA512(const Seed: TBytes; OutLen: Integer): TBytes;

// 6 核心杂凑函数
function SlhF(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1: TBytes): TBytes;
function SlhH(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1, M2: TBytes): TBytes;
function SlhTl(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M: TBytes): TBytes;
function SlhPRF(Params: PCnSlhParams; const PKSeed, SKSeed: TBytes;
  var ADRS: TCnSlhAddr): TBytes;
function SlhPRFMsg(Params: PCnSlhParams;
  const SKPrf, OptRand, M: TBytes): TBytes;
function SlhHMsg(Params: PCnSlhParams;
  const R, PKSeed, PKRoot, M: TBytes): TBytes;

// 函数选择器
procedure SlhInitHashFuncs(Params: PCnSlhParams; out Funcs: TCnSlhHashFuncs);

// WOTS+ 基 w 编码
procedure SlhBaseWEncode(const M: TBytes; N, W, Len: Byte; var Msg: TBytes; Offset: Integer);
function SlhBaseWChecksum(const Msg: TBytes; W, Len1, Len2: Byte): TBytes;

// WOTS+ 链迭代（Algorithm 1）
function SlhChain(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const X0: TBytes; Start, Steps: Byte): TBytes;

// WOTS+ 密钥生成（Algorithm 3）
function SlhWotsKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;

// WOTS+ 签名（Algorithm 4）
function SlhWotsSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const M, SKSeed, PKSeed: TBytes): TBytes;

// WOTS+ 从签名恢复公钥（Algorithm 5）
function SlhWotsPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SIG, M, PKSeed: TBytes): TBytes;

// XMSS 树杂凑（Algorithm 9）
function SlhXmssTreeHash(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes; LeafIdx: Cardinal): TBytes;

// XMSS 密钥生成（Algorithm 8）
function SlhXmssKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;

// XMSS 签名（Algorithm 10）
function SlhXmssSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const M, SKSeed, PKSeed: TBytes; Idx: Cardinal): TBytes;

// XMSS 从签名恢复公钥（Algorithm 13）
function SlhXmssPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  Idx: Cardinal; const SIG, M, PKSeed: TBytes): TBytes;

// Hypertree 签名（Algorithm 12）
function SlhHtSign(Params: PCnSlhParams; const M, SKSeed, PKSeed: TBytes;
  TreeIdx: TUInt64; LeafIdx: Cardinal): TBytes;

// Hypertree 验签（Algorithm 14）
function SlhHtVerify(Params: PCnSlhParams; const M, SIG: TBytes;
  const PKSeed: TBytes; TreeIdx: TUInt64; LeafIdx: Cardinal;
  const PKRoot: TBytes): Boolean;

// FORS 树杂凑（构建一棵 FORS Merkle 树，返回根）
function SlhForsTreeHash(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes; TreeIdx: Byte): TBytes;

// FORS 密钥生成（k 棵树根拼接）
function SlhForsKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;

// FORS 签名（Algorithm 15）
function SlhForsSign(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const Md, SKSeed, PKSeed: TBytes): TBytes;

// FORS 从签名恢复公钥（Algorithm 16）
function SlhForsPKFromSig(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SIG, Md, PKSeed: TBytes): TBytes;

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
  SlhWriteU64BE(AD[4], Tree);
end;

procedure SlhAddrSetType(var AD: TCnSlhAddr; Typ: Cardinal);
begin
  SlhWriteU32BE(AD[CN_ADRS_OFFSET_TYPE], Typ);
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
  SlhAddrSetHash(AD, Height);
end;

procedure SlhAddrSetTreeIndex(var AD: TCnSlhAddr; Index: Cardinal);
begin
  SlhAddrSetKeyPair(AD, Index);
end;

procedure SlhAddrCopy(const Src: TCnSlhAddr; var Dst: TCnSlhAddr);
begin
  Move(Src, Dst, SizeOf(TCnSlhAddr));
end;

procedure SlhAddrCompress(const Src: TCnSlhAddr; var Dst: TCnSlhAddrSHA2);
begin
  // SHA2 22B = Tree[6] + Type[2] + KeyPair[4] + Chain[4] + Hash[4] + Zero[2]
  // Take from extended 32B: Tree[6..11], Type[14..15], KeyPair[16..19],
  //   Chain[20..23], Hash[24..27], Zero[30..31]
  Move(Src[6], Dst[0], 6);      // Tree (lower 48 bits)
  Move(Src[14], Dst[6], 2);     // Type (lower 16 bits)
  Move(Src[16], Dst[8], 4);     // KeyPair
  Move(Src[20], Dst[12], 4);    // Chain
  Move(Src[24], Dst[16], 4);    // Hash
  Move(Src[30], Dst[20], 2);    // Zero (lower 16 bits)
end;

procedure SlhAddrDecompress(const Src: TCnSlhAddrSHA2; var Dst: TCnSlhAddr);
begin
  FillChar(Dst, SizeOf(TCnSlhAddr), 0);
  // Reverse of compress
  Move(Src[0], Dst[6], 6);      // Tree
  Move(Src[6], Dst[14], 2);     // Type
  Move(Src[8], Dst[16], 4);     // KeyPair
  Move(Src[12], Dst[20], 4);    // Chain
  Move(Src[16], Dst[24], 4);    // Hash
  Move(Src[20], Dst[30], 2);    // Zero
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
    SetLength(Result, CN_ADRS_SIZE_SHA2);
    Move(AD[6], Result[0], 6);      // Tree (lower 48 bits)
    Move(AD[14], Result[6], 2);     // Type (lower 16 bits)
    Move(AD[16], Result[8], 4);     // KeyPair
    Move(AD[20], Result[12], 4);    // Chain
    Move(AD[24], Result[16], 4);    // Hash
    Move(AD[30], Result[20], 2);    // Zero (lower 16 bits)
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
// SlhF - Algorithm 2: 链内杂凑函数
// Input: PK.Seed (n) || ADRS (32/22) || 0xFF || M1 (n)
// Output: n
// -------------------------------------------------------------------
function SlhF(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1: TBytes): TBytes;
var
  AddrBytes: TBytes;
  Input: TBytes;
  Offset: Integer;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  AddrBytes := SlhAddrToBytes(ADRS, Params.IsSHA2);
  SetLength(Input, Length(PKSeed) + Length(AddrBytes) + 1 + Length(M1));
  Offset := 0;
  Move(PKSeed[0], Input[0], Length(PKSeed));
  Inc(Offset, Length(PKSeed));
  Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
  Inc(Offset, Length(AddrBytes));
  Input[Offset] := $FF;
  Inc(Offset);
  Move(M1[0], Input[Offset], Length(M1));

  if Params.IsSHA2 then
  begin
    if Params.N <= 16 then
    begin
      D256 := SHA256Bytes(Input);
      SetLength(Result, Params.N);
      Move(D256, Result[0], Params.N);
    end
    else
    begin
      D512 := SHA512Bytes(Input);
      SetLength(Result, Params.N);
      Move(D512, Result[0], Params.N);
    end;
  end
  else
    Result := SHAKE256Bytes(Input, Params.N);
end;

// -------------------------------------------------------------------
// SlhH - Algorithm 3: 树杂凑函数
// Input: PK.Seed (n) || ADRS (32/22) || M1 (n) || M2 (n)
// Output: n
// -------------------------------------------------------------------
function SlhH(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M1, M2: TBytes): TBytes;
var
  AddrBytes: TBytes;
  Input: TBytes;
  Offset: Integer;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  AddrBytes := SlhAddrToBytes(ADRS, Params.IsSHA2);
  SetLength(Input, Length(PKSeed) + Length(AddrBytes) +
    Length(M1) + Length(M2));
  Offset := 0;
  Move(PKSeed[0], Input[0], Length(PKSeed));
  Inc(Offset, Length(PKSeed));
  Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
  Inc(Offset, Length(AddrBytes));
  Move(M1[0], Input[Offset], Length(M1));
  Inc(Offset, Length(M1));
  Move(M2[0], Input[Offset], Length(M2));

  if Params.IsSHA2 then
  begin
    if Params.N <= 16 then
    begin
      D256 := SHA256Bytes(Input);
      SetLength(Result, Params.N);
      Move(D256, Result[0], Params.N);
    end
    else
    begin
      D512 := SHA512Bytes(Input);
      SetLength(Result, Params.N);
      Move(D512, Result[0], Params.N);
    end;
  end
  else
    Result := SHAKE256Bytes(Input, Params.N);
end;

// -------------------------------------------------------------------
// SlhT_l - Algorithm 4: WOTS+ 链压缩（L-Tree）
// Input: PK.Seed (n) || ADRS (32/22) || M (Len*n)
// Output: n
// -------------------------------------------------------------------
function SlhTl(Params: PCnSlhParams; const PKSeed: TBytes;
  var ADRS: TCnSlhAddr; const M: TBytes): TBytes;
var
  Height, I, Count, NextCount: Integer;
  Nodes, NextNodes: TBytes;
begin
  Count := Length(M) div Params.N;
  SetLength(Nodes, Count * Params.N);
  Move(M[0], Nodes[0], Count * Params.N);
  Height := 0;
  while Count > 1 do
  begin
    NextCount := (Count + 1) div 2;
    SetLength(NextNodes, NextCount * Params.N);
    for I := 0 to NextCount - 1 do
    begin
      if 2 * I + 1 < Count then
      begin
        SlhAddrSetChain(ADRS, 2 * I);
        SlhAddrSetHash(ADRS, Height);
        // Compute H(PKSeed, ADRS, left, right)
        Result := SlhH(Params, PKSeed, ADRS,
          Copy(Nodes, 2 * I * Params.N, Params.N),
          Copy(Nodes, (2 * I + 1) * Params.N, Params.N));
        Move(Result[0], NextNodes[I * Params.N], Params.N);
      end
      else
        Move(Nodes[2 * I * Params.N], NextNodes[I * Params.N], Params.N);
    end;
    Nodes := NextNodes;
    Count := NextCount;
    Inc(Height);
  end;
  SetLength(Result, Params.N);
  Move(Nodes[0], Result[0], Params.N);
end;

// -------------------------------------------------------------------
// SlhPRF - Algorithm 5: 伪随机函数（用于密钥生成）
// Input: PK.Seed (n) || ADRS (32/22) || SK.Seed (n)
// Output: n
// -------------------------------------------------------------------
function SlhPRF(Params: PCnSlhParams; const PKSeed, SKSeed: TBytes;
  var ADRS: TCnSlhAddr): TBytes;
var
  AddrBytes: TBytes;
  Input: TBytes;
  Offset: Integer;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  AddrBytes := SlhAddrToBytes(ADRS, Params.IsSHA2);
  SetLength(Input, Length(PKSeed) + Length(AddrBytes) + Length(SKSeed));
  Offset := 0;
  Move(PKSeed[0], Input[0], Length(PKSeed));
  Inc(Offset, Length(PKSeed));
  Move(AddrBytes[0], Input[Offset], Length(AddrBytes));
  Inc(Offset, Length(AddrBytes));
  Move(SKSeed[0], Input[Offset], Length(SKSeed));

  if Params.IsSHA2 then
  begin
    if Params.N <= 16 then
    begin
      D256 := SHA256Bytes(Input);
      SetLength(Result, Params.N);
      Move(D256, Result[0], Params.N);
    end
    else
    begin
      D512 := SHA512Bytes(Input);
      SetLength(Result, Params.N);
      Move(D512, Result[0], Params.N);
    end;
  end
  else
    Result := SHAKE256Bytes(Input, Params.N);
end;

// -------------------------------------------------------------------
// SlhPRF_msg - Algorithm 6: 消息伪随机函数
// Input: SK.Prf (n) || OptRand (n) || M
// Output: n
// SHAKE: SHAKE256(SK.Prf || OptRand || M, n)
// SHA2: HMAC-SHA256/512(SK.Prf || 0x01, OptRand || M)[0:n-1]
// -------------------------------------------------------------------
function SlhPRFMsg(Params: PCnSlhParams;
  const SKPrf, OptRand, M: TBytes): TBytes;
var
  Key, Data: TBytes;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  if not Params.IsSHA2 then
  begin
    Key := ConcatBytes(SKPrf, OptRand, M);
    Result := SHAKE256Bytes(Key, Params.N);
    Exit;
  end;

  // SHA2: HMAC-SHA256/512
  SetLength(Key, Length(SKPrf) + 1);
  Move(SKPrf[0], Key[0], Length(SKPrf));
  Key[Length(SKPrf)] := $01;

  SetLength(Data, Length(OptRand) + Length(M));
  Move(OptRand[0], Data[0], Length(OptRand));
  if Length(M) > 0 then
    Move(M[0], Data[Length(OptRand)], Length(M));

  if Params.N <= 16 then
  begin
    D256 := SHA256HmacBytes(Key, Data);
    SetLength(Result, Params.N);
    Move(D256, Result[0], Params.N);
  end
  else
  begin
    D512 := SHA512HmacBytes(Key, Data);
    SetLength(Result, Params.N);
    Move(D512, Result[0], Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhH_msg - Algorithm 7: 消息杂凑函数
// Input: R (n) || PK.Seed (n) || PK.Root (n) || M
// Output: m
// SHAKE: SHAKE256(R || PK.Seed || PK.Root || M, m)
// SHA2: MGF1(HMAC-SHA256/512(R || PK.Seed, PK.Root || M), m)
// -------------------------------------------------------------------
function SlhHMsg(Params: PCnSlhParams;
  const R, PKSeed, PKRoot, M: TBytes): TBytes;
var
  Key, Data: TBytes;
  D256: TCnSHA256Digest;
  D512: TCnSHA512Digest;
begin
  if not Params.IsSHA2 then
  begin
    Key := ConcatBytes(R, PKSeed, PKRoot, M);
    Result := SHAKE256Bytes(Key, Params.M);
    Exit;
  end;

  // SHA2: MGF1(HMAC-SHA256/512(R || PK.Seed, PK.Root || M), m)
  SetLength(Key, Length(R) + Length(PKSeed));
  Move(R[0], Key[0], Length(R));
  Move(PKSeed[0], Key[Length(R)], Length(PKSeed));

  SetLength(Data, Length(PKRoot) + Length(M));
  Move(PKRoot[0], Data[0], Length(PKRoot));
  if Length(M) > 0 then
    Move(M[0], Data[Length(PKRoot)], Length(M));

  if Params.N <= 16 then
  begin
    D256 := SHA256HmacBytes(Key, Data);
    SetLength(Key, 32);
    Move(D256, Key[0], 32);
    Result := SlhMGF1_SHA256(Key, Params.M);
  end
  else
  begin
    D512 := SHA512HmacBytes(Key, Data);
    SetLength(Key, 64);
    Move(D512, Key[0], 64);
    Result := SlhMGF1_SHA512(Key, Params.M);
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
  CsumBytes: Integer;
begin
  csum := 0;
  for I := 0 to Len1 - 1 do
    csum := csum + (W - 1 - Msg[I]);

  // Len2 * log2(w) bits, convert to bytes
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
// SlhWotsKeyGen - Algorithm 3: WOTS+ 密钥生成
// Input: SK.Seed (n), PK.Seed (n), ADRS
// Output: PK (n)
// -------------------------------------------------------------------
function SlhWotsKeyGen(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes): TBytes;
var
  I: Integer;
  SK, PK_i: TBytes;
  PkList: TBytes;
begin
  SetLength(PkList, Params.Len * Params.N);
  for I := 0 to Params.Len - 1 do
  begin
    SlhAddrSetChain(ADRS, I);
    SlhAddrSetHash(ADRS, 0);
    SK := SlhPRF(Params, PKSeed, SKSeed, ADRS);
    PK_i := SlhChain(Params, PKSeed, ADRS, SK, 0, Params.W - 1);
    Move(PK_i[0], PkList[I * Params.N], Params.N);
  end;
  Result := SlhTl(Params, PKSeed, ADRS, PkList);
end;

// -------------------------------------------------------------------
// SlhWotsSign - Algorithm 4: WOTS+ 签名
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

  // Generate signature
  SetLength(Result, TotalLen * Params.N);
  for I := 0 to TotalLen - 1 do
  begin
    SlhAddrSetChain(ADRS, I);
    SlhAddrSetHash(ADRS, 0);
    SK := SlhPRF(Params, PKSeed, SKSeed, ADRS);
    SigI := SlhChain(Params, PKSeed, ADRS, SK, 0, Msg[I]);
    Move(SigI[0], Result[I * Params.N], Params.N);
  end;
end;

// -------------------------------------------------------------------
// SlhWotsPKFromSig - Algorithm 5: 从签名恢复 WOTS+ 公钥
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

  // Recover public key from signature
  SetLength(PkList, Params.Len * Params.N);
  for I := 0 to TotalLen - 1 do
  begin
    SlhAddrSetChain(ADRS, I);
    SetLength(SigI, Params.N);
    Move(SIG[I * Params.N], SigI[0], Params.N);
    PK_i := SlhChain(Params, PKSeed, ADRS, SigI, Msg[I], Params.W - 1 - Msg[I]);
    if I < Params.Len then
      Move(PK_i[0], PkList[I * Params.N], Params.N);
  end;

  Result := SlhTl(Params, PKSeed, ADRS, PkList);
end;

// ===================================================================
// XMSS 实现
// ===================================================================

// -------------------------------------------------------------------
// SlhXmssTreeHash - Algorithm 9: 堆栈式 Merkle 树杂凑
// 返回根节点；LeafIdx 用于认证路径生成（未使用 LeafIdx 时为简化版本）
// Input: SK.Seed (n), PK.Seed (n), LeafIdx
// Output: root (n)
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
    // Generate leaf: WOTS+ keygen compressed to n bytes
    TmpAdrs := ADRS;
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_WOTS_HASH);
    SlhAddrSetChain(TmpAdrs, 0); // will be set by WotsKeyGen
    Node := SlhWotsKeyGen(Params, TmpAdrs, SKSeed, PKSeed);

    // Compress leaf: H(PK.seed, ADRS, node, node)
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_TREE);
    SlhAddrSetTreeHeight(TmpAdrs, 0);
    SlhAddrSetTreeIndex(TmpAdrs, I);
    Node := SlhH(Params, PKSeed, TmpAdrs, Node, Node);

    CurrentHeight := 0;

    // Merge with stack
    while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
    begin
      Dec(StackSize);
      Left := StackNodes[StackSize];
      Right := Node;

      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_TREE);
      SlhAddrSetTreeHeight(TmpAdrs, CurrentHeight);
      SlhAddrSetTreeIndex(TmpAdrs, I shr (CurrentHeight + 1));
      Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);

      Inc(CurrentHeight);
    end;

    // Push to stack
    StackNodes[StackSize] := Node;
    StackHeights[StackSize] := CurrentHeight;
    Inc(StackSize);
  end;

  // Root is the only node remaining (after merging down)
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
  I, CurrentHeight, HIdx: Integer;
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
    // Generate leaf: WOTS+ keygen compressed to n bytes
    TmpAdrs := ADRS;
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_WOTS_HASH);
    Node := SlhWotsKeyGen(Params, TmpAdrs, SKSeed, PKSeed);

    // Compress leaf
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_TREE);
    SlhAddrSetTreeHeight(TmpAdrs, 0);
    SlhAddrSetTreeIndex(TmpAdrs, I);
    Node := SlhH(Params, PKSeed, TmpAdrs, Node, Node);

    CurrentHeight := 0;

    // Merge with stack
    while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
    begin
      if (I shr (CurrentHeight + 1)) = (Idx shr (CurrentHeight + 1)) then
      begin
        HIdx := CurrentHeight;
        if HIdx < Params.Hp then
        begin
          if ((Idx shr CurrentHeight) and 1) = 0 then
            AuthPath[HIdx] := Node
          else
            AuthPath[HIdx] := StackNodes[StackSize - 1];
        end;
      end;

      Dec(StackSize);
      Left := StackNodes[StackSize];
      Right := Node;

      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_TREE);
      SlhAddrSetTreeHeight(TmpAdrs, CurrentHeight);
      SlhAddrSetTreeIndex(TmpAdrs, I shr (CurrentHeight + 1));
      Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);

      Inc(CurrentHeight);
    end;

    // Push to stack
    StackNodes[StackSize] := Node;
    StackHeights[StackSize] := CurrentHeight;
    Inc(StackSize);
  end;

  // Generate WOTS+ signature
  SlhAddrSetType(ADRS, CN_SLH_ADRS_WOTS_HASH);
  SigWots := SlhWotsSign(Params, ADRS, M, SKSeed, PKSeed);

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
// SlhXmssPKFromSig - Algorithm 13: 从签名恢复 XMSS 公钥
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
  // Parse signature
  SetLength(SigWots, Params.LenTotal * Params.N);
  Move(SIG[0], SigWots[0], Params.LenTotal * Params.N);

  // Recover WOTS+ public key
  SlhAddrSetType(ADRS, CN_SLH_ADRS_WOTS_HASH);
  Node := SlhWotsPKFromSig(Params, ADRS, SigWots, M, PKSeed);

  // Compress leaf: H(PK.Seed, ADRS[TREE, Layer, Tree, Height=0, Index=Idx], Node, Node)
  SlhAddrCopy(ADRS, TmpAdrs);
  SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_TREE);
  SlhAddrSetTreeHeight(TmpAdrs, 0);
  SlhAddrSetTreeIndex(TmpAdrs, Idx);
  SlhAddrSetChain(TmpAdrs, 0);
  Node := SlhH(Params, PKSeed, TmpAdrs, Node, Node);

  // Compute root using auth path
  for I := 0 to Params.Hp - 1 do
  begin
    Offset := Params.LenTotal * Params.N + I * Params.N;
    SetLength(AuthPathNode, Params.N);
    Move(SIG[Offset], AuthPathNode[0], Params.N);

    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_TREE);
    SlhAddrSetTreeHeight(TmpAdrs, I);
    SlhAddrSetTreeIndex(TmpAdrs, Idx shr (I + 1));
    SlhAddrSetChain(TmpAdrs, 0);

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
            and CompareMem(@ComputedRoot[0], @PKRoot[0], Length(PKRoot));
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
// SlhForsTreeHash - 构建一棵 FORS Merkle 树并返回根
// Input: ADRS (type=FORS_TREE), SK.Seed (n), PK.Seed (n), TreeIdx
// Output: root (n)
// -------------------------------------------------------------------
function SlhForsTreeHash(Params: PCnSlhParams; var ADRS: TCnSlhAddr;
  const SKSeed, PKSeed: TBytes; TreeIdx: Byte): TBytes;
var
  StackNodes: array of TBytes;
  StackHeights: array of Integer;
  StackSize: Integer;
  LeafCount: Cardinal;
  I, CurrentHeight: Integer;
  Node, Left, Right, SK: TBytes;
  TmpAdrs: TCnSlhAddr;
begin
  LeafCount := Cardinal(1) shl Params.A;
  SetLength(StackNodes, Params.A + 1);
  SetLength(StackHeights, Params.A + 1);
  StackSize := 0;

  for I := 0 to LeafCount - 1 do
  begin
    // Generate leaf private key: PRF(PK.seed, SK.seed, ADRS)
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_PRF);
    SlhAddrSetKeyPair(TmpAdrs, TreeIdx);
    SlhAddrSetChain(TmpAdrs, I);
    SlhAddrSetHash(TmpAdrs, 0);
    SK := SlhPRF(Params, PKSeed, SKSeed, TmpAdrs);

    // Leaf = F(PK.seed, ADRS, SK_i)
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
    SlhAddrSetKeyPair(TmpAdrs, TreeIdx);
    SlhAddrSetChain(TmpAdrs, I);
    SlhAddrSetHash(TmpAdrs, 0);
    Node := SlhF(Params, PKSeed, TmpAdrs, SK);

    CurrentHeight := 0;

    // Merge with stack
    while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
    begin
      Dec(StackSize);
      Left := StackNodes[StackSize];
      Right := Node;

      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
      SlhAddrSetKeyPair(TmpAdrs, TreeIdx);
      SlhAddrSetChain(TmpAdrs, I shr (CurrentHeight + 1));
      SlhAddrSetHash(TmpAdrs, CurrentHeight);
      Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);

      Inc(CurrentHeight);
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
// SlhForsSign - Algorithm 15: FORS 签名
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
  LeafCount: Cardinal;
  Node, Left, Right, SK: TBytes;
  TmpAdrs: TCnSlhAddr;
  AuthPath: array of TBytes;
  AuthIdx: Integer;
  Offset: Integer;
  BitIdx: Integer;
begin
  SetLength(Result, Params.K * (Params.A + 1) * Params.N);
  Offset := 0;

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
    SetLength(StackNodes, Params.A + 1);
    SetLength(StackHeights, Params.A + 1);
    StackSize := 0;
    SetLength(AuthPath, Params.A);

    for I := 0 to LeafCount - 1 do
    begin
      // Generate leaf private key
      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_PRF);
      SlhAddrSetKeyPair(TmpAdrs, TreeI);
      SlhAddrSetChain(TmpAdrs, I);
      SlhAddrSetHash(TmpAdrs, 0);
      SK := SlhPRF(Params, PKSeed, SKSeed, TmpAdrs);

      // Leaf = F(PK.seed, ADRS, SK_i)
      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
      SlhAddrSetKeyPair(TmpAdrs, TreeI);
      SlhAddrSetChain(TmpAdrs, I);
      SlhAddrSetHash(TmpAdrs, 0);
      Node := SlhF(Params, PKSeed, TmpAdrs, SK);

      CurrentHeight := 0;

      while (StackSize > 0) and (StackHeights[StackSize - 1] = CurrentHeight) do
      begin
        if (I shr (CurrentHeight + 1)) = (LeafIdx shr (CurrentHeight + 1)) then
        begin
          AuthIdx := CurrentHeight;
          if AuthIdx < Params.A then
          begin
            if ((LeafIdx shr CurrentHeight) and 1) = 0 then
              AuthPath[AuthIdx] := Node
            else
              AuthPath[AuthIdx] := StackNodes[StackSize - 1];
          end;
        end;

        Dec(StackSize);
        Left := StackNodes[StackSize];
        Right := Node;

        SlhAddrCopy(ADRS, TmpAdrs);
        SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
        SlhAddrSetKeyPair(TmpAdrs, TreeI);
        SlhAddrSetChain(TmpAdrs, I shr (CurrentHeight + 1));
        SlhAddrSetHash(TmpAdrs, CurrentHeight);
        Node := SlhH(Params, PKSeed, TmpAdrs, Left, Right);

        Inc(CurrentHeight);
      end;

      StackNodes[StackSize] := Node;
      StackHeights[StackSize] := CurrentHeight;
      Inc(StackSize);
    end;

    // Save SK for selected leaf at auth path start
    // Regenerate SK for LeafIdx
    SlhAddrCopy(ADRS, TmpAdrs);
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_PRF);
    SlhAddrSetKeyPair(TmpAdrs, TreeI);
    SlhAddrSetChain(TmpAdrs, LeafIdx);
    SlhAddrSetHash(TmpAdrs, 0);
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
begin
  SetLength(Result, Params.K * Params.N);
  Offset := 0;

  for TreeI := 0 to Params.K - 1 do
  begin
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
    SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
    SlhAddrSetKeyPair(TmpAdrs, TreeI);
    SlhAddrSetChain(TmpAdrs, LeafIdx);
    SlhAddrSetHash(TmpAdrs, 0);
    Node := SlhF(Params, PKSeed, TmpAdrs, Node);

    // Compute root using auth path
    for I := 0 to Params.A - 1 do
    begin
      SetLength(Sibling, Params.N);
      Move(SIG[Offset + I * Params.N], Sibling[0], Params.N);

      SlhAddrCopy(ADRS, TmpAdrs);
      SlhAddrSetType(TmpAdrs, CN_SLH_ADRS_FORS_TREE);
      SlhAddrSetKeyPair(TmpAdrs, TreeI);
      SlhAddrSetChain(TmpAdrs, LeafIdx shr (I + 1));
      SlhAddrSetHash(TmpAdrs, I);

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

procedure TCnSLHDSA.GenerateKeys(out PK: TCnSlhPublicKey;
  out SK: TCnSlhSecretKey);
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
  R, Digest, Md, IdxBytes: TBytes;
  MdLen, IdxLen: Integer;
  Idx, TreeIdx: TUInt64;
  LeafIdx: Cardinal;
  LeafMask: TUInt64;
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

  // Split Digest: Md (m - ceil(h/8) bytes) + IdxBytes (ceil(h/8) bytes)
  IdxLen := (P.H + 7) div 8;
  MdLen := P.M - IdxLen;
  SetLength(Md, MdLen);
  Move(Digest[0], Md[0], MdLen);
  SetLength(IdxBytes, IdxLen);
  Move(Digest[MdLen], IdxBytes[0], IdxLen);

  // Idx = low h bits
  Idx := SlhBytesToUInt64BE(IdxBytes, 0, IdxLen);
  if P.H < 64 then
    Idx := Idx and ((TUInt64(1) shl P.H) - 1);

  // FORS signature
  SlhAddrInit(ADRS);
  SlhAddrSetType(ADRS, CN_SLH_ADRS_FORS_TREE);
  SigFors := SlhForsSign(P, ADRS, Md, SK.Seed, SK.PKSeed);

  // PK_FORS = T_l(ForsKeyGen roots)
  SlhAddrInit(ADRS);
  SlhAddrSetType(ADRS, CN_SLH_ADRS_FORS_ROOTS);
  PKFors := SlhForsKeyGen(P, ADRS, SK.Seed, SK.PKSeed);
  PKForsCompressed := SlhTl(P, SK.PKSeed, ADRS, PKFors);

  // Hypertree indices
  LeafMask := (TUInt64(1) shl P.Hp) - 1;
  LeafIdx := Idx and LeafMask;
  TreeIdx := Idx shr P.Hp;

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

function TCnSLHDSA.Sign(const M: TBytes; const SK: TCnSlhSecretKey;
  Randomize: Boolean = True): TCnSlhSignature;
var
  OptRand: TBytes;
begin
  SetLength(OptRand, FParams.N);
  if Randomize then
    OptRand := CnRandomBytes(FParams.N)
  else
    FillChar(OptRand[0], FParams.N, 0);

  Result := SignInternal(M, SK, OptRand);
end;

function TCnSLHDSA.Verify(const M: TBytes; const SIG: TCnSlhSignature;
  const PK: TCnSlhPublicKey): Boolean;
var
  P: PCnSlhParams;
  R, Digest, Md, IdxBytes: TBytes;
  SigFors, SigHt, PKFors, PKForsCompressed: TBytes;
  MdLen, IdxLen: Integer;
  Idx, TreeIdx: TUInt64;
  LeafIdx: Cardinal;
  LeafMask: TUInt64;
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
  Digest := SlhHMsg(P, R, PK.Seed, PK.Root, M);

  // Split Digest
  IdxLen := (P.H + 7) div 8;
  MdLen := P.M - IdxLen;
  SetLength(Md, MdLen);
  Move(Digest[0], Md[0], MdLen);
  SetLength(IdxBytes, IdxLen);
  Move(Digest[MdLen], IdxBytes[0], IdxLen);

  // Idx
  Idx := SlhBytesToUInt64BE(IdxBytes, 0, IdxLen);
  if P.H < 64 then
    Idx := Idx and ((TUInt64(1) shl P.H) - 1);

  // Recover PK_FORS from signature
  SlhAddrInit(ADRS);
  SlhAddrSetType(ADRS, CN_SLH_ADRS_FORS_TREE);
  try
    PKFors := SlhForsPKFromSig(P, ADRS, SigFors, Md, PK.Seed);
  except
    Exit;
  end;

  // Compress FORS roots
  SlhAddrInit(ADRS);
  SlhAddrSetType(ADRS, CN_SLH_ADRS_FORS_ROOTS);
  PKForsCompressed := SlhTl(P, PK.Seed, ADRS, PKFors);

  // Verify Hypertree
  LeafMask := (TUInt64(1) shl P.Hp) - 1;
  LeafIdx := Idx and LeafMask;
  TreeIdx := Idx shr P.Hp;

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
    shiSHA2_256, shiSHA3_256, shiSHA2_512_256, shiSHAKE128: Result := 32;
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
  Randomize: Boolean = True): TCnSlhSignature;
var
  MPrime, OptRand: TBytes;
begin
  MPrime := SlhPrehashData(M, HashID);

  SetLength(OptRand, FParams.N);
  if Randomize then
    OptRand := CnRandomBytes(FParams.N)
  else
    FillChar(OptRand[0], FParams.N, 0);

  Result := SignInternal(MPrime, SK, OptRand);
end;

function TCnSLHDSA.VerifyPreHash(const M: TBytes;
  const SIG: TCnSlhSignature; const PK: TCnSlhPublicKey;
  HashID: TCnSlhPrehashID): Boolean;
var
  MPrime: TBytes;
begin
  MPrime := SlhPrehashData(M, HashID);
  Result := Verify(MPrime, SIG, PK);
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

function TCnSLHDSA.BytesToPublicKey(
  const Data: TBytes): TCnSlhPublicKey;
begin
  if Length(Data) < FParams.N * 2 then
    raise ECnSlhException.Create('Invalid public key data length');
  SetLength(Result.Seed, FParams.N);
  SetLength(Result.Root, FParams.N);
  Move(Data[0], Result.Seed[0], FParams.N);
  Move(Data[FParams.N], Result.Root[0], FParams.N);
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

function TCnSLHDSA.BytesToSecretKey(
  const Data: TBytes): TCnSlhSecretKey;
begin
  if Length(Data) < FParams.N * 4 then
    raise ECnSlhException.Create('Invalid secret key data length');
  SetLength(Result.Seed, FParams.N);
  SetLength(Result.Prf, FParams.N);
  SetLength(Result.PKSeed, FParams.N);
  SetLength(Result.PKRoot, FParams.N);
  Move(Data[0], Result.Seed[0], FParams.N);
  Move(Data[FParams.N], Result.Prf[0], FParams.N);
  Move(Data[FParams.N * 2], Result.PKSeed[0], FParams.N);
  Move(Data[FParams.N * 3], Result.PKRoot[0], FParams.N);
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
