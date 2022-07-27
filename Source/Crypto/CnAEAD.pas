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

unit CnAEAD;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：各类 AEAD 实现单元
* 单元作者：刘啸（Liu Xiao）
* 备    注：AEAD 是关联数据认证加密的简称。可以用密码、关联数据与初始化向量等对
*           数据进行加密与生成验证内容，解密时如果验证内容通不过则失败。
*           注：字节串转换为大整数时相当于大端表达法，且大下标指向高位地址
*           目前实现了 GHash128（似乎也叫 GMAC）
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.07.27 V1.0
*               创建单元。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative;

const
  GHASH_BLOCK = 16;       // GHASH 的分组 16 字节

  GCM_BLOCK   = 16;       // GCM 的分组 16 字节

  GCM_NONCE_LENGTH = 12;  // 12 字节的 Nonce 与计数器拼成完整 Iv

type
  TGHash128Buffer = array[0..GHASH_BLOCK - 1] of Byte;
  {* GHash128 的分块}

  TGHash128Key = array[0..GHASH_BLOCK - 1] of Byte;
  {* GHash128 的密钥}

  TGHash128Tag    = array[0..GHASH_BLOCK - 1] of Byte;
  {* GHash128 的计算结果}

  TGHash128Context = packed record
  {* 用于多次分块计算的 GHash128 上下文结构}
    HashKey:  TGHash128Buffer;
    State:    TGHash128Buffer;
    AADByteLen: Integer;
    DataByteLen: Integer;
  end;

  TGCM128Buffer = array[0..GCM_BLOCK - 1] of Byte;
  {* GCM 模式的分块，内部无论用 AES 还是 SM4 均为 16 字节}

  TGCM128Key = array[0..GCM_BLOCK - 1] of Byte;
  {* GCM 模式的密钥，内部无论用 AES 还是 SM4 均为 16 字节}

  TGCM128Tag    = array[0..GCM_BLOCK - 1] of Byte;
  {* GCM 的计算结果}

procedure GMulBlock128(var X, Y: TGHash128Buffer; var R: TGHash128Buffer);
{* 实现 GHash 中的伽罗华域 (2^128) 上的块乘法操作。基本测试通过，也符合交换律
  注意 2 次幂有限域乘法里的加法是模 2 加也就是异或（也等同于模 2 减）
  同时还要模一个模多项式 GHASH_POLY，其中的单次减同样也即异或}

procedure GHash128(var HashKey: TGHash128Key; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer; var OutTag: TGHash128Tag);
{* 以指定 HashKey 与附加数据 AAD，对一块数据进行 GHash 计算得到 Tag 摘要，
  对应文档中的 GHash(H, A C)}

function GHash128Bytes(var HashKey: TGHash128Key; Data, AAD: TBytes): TGHash128Tag;
{* 字节数组方式进行 GHash 计算，内部调用 GHash128}

// 以下三个函数用于外部持续对数据进行零散的 GHash128 计算，GHash128Update 可多次被调用
// 注意 GHash128Update 中 Data 需要尽量传整块，如不整块，末尾会补 0 计算，
// 而不是类似于其他杂凑函数那样留存着等下一轮凑足后再整

procedure GHash128Start(var Ctx: TGHash128Context; var HashKey: TGHash128Key;
  AAD: Pointer; AADByteLength: Integer);
{* 开始 GHash128 的第一步，初始化后全部计算好 AAD 内容}

procedure GHash128Update(var Ctx: TGHash128Context; Data: Pointer; DataByteLength: Integer);
{* 对一块数据进行 GHash128，结果与计算长度均记录在 Ctx 中，可以多次调用}

procedure GHash128Finish(var Ctx: TGHash128Context; var Output: TGHash128Tag);
{* GHash128 结束，补算长度，并返回结果}

// ========================== AES/SM4 - GCM 加解密函数 =========================

function AES128GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;
function AES192GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;
function AES256GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;
function SM4GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;

function AES128GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;
function AES192GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;
function AES256GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;
function SM4GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;

implementation

uses
  CnSM4, CnAES;

const
  GHASH_POLY: TGHash128Buffer = ($E1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

type
  TGCMEncryptType = (getAES128, getAES192, getAES256, getSM4);
  {* 支持的四种 GCM 类型}

  TGCMContext = packed record
  case TGCMEncryptType of
    getAES128: (ExpandedKey128: TAESExpandedKey128);
    getAES192: (ExpandedKey192: TAESExpandedKey192);
    getAES256: (ExpandedKey256: TAESExpandedKey256);
    getSM4:    (SM4Context: TSM4Context);
  end;

{
  GCM 使用的 Galois(2^128) 域内的乘法

  一般来说对于字节串，例如 $FEDC，其 BIT 顺序就应该是 FEDC，也就是 1111111011011100，
  GCM 中，它对应的整数也等同于这串二进制的常规表达。
  然后 LSB 指右边的低位的，MSB 指左边高位的，左右移位符合常规习惯。
  开发过程中碰到问题：下标 0 是指最左还是最右？
  GCM 用的模多项式是 128,7,2,1,0，而算法中的表达是 E1000000000……，说明下标是右边高位
  因此，127 下标是整数高位，右移是往整数的高位方向移。
  同时，127 也对应着地址高位（0 是地址低位），右移是往地址高位方向移
}
procedure GMulBlock128(var X, Y: TGHash128Buffer; var R: TGHash128Buffer);
var
  I: Integer;
  Z, V: TGHash128Buffer;
  B: Boolean;

  // 注意此处判断字节内 Bit 的顺序也是字节内的高位是 0，
  function GHashIsBitSet(AMem: Pointer; N: Integer): Boolean;
  var
    P: PCnByte;
    A1, B1: Integer;
    V: Byte;
  begin
    A1 := N div 8;
    B1 := 7 - (N mod 8);
    P := PCnByte(TCnNativeInt(AMem) + A1);

    V := Byte(1 shl B1);
    Result := (P^ and V) <> 0;
  end;

begin
  FillChar(Z[0], SizeOf(TGHash128Buffer), 0);
  Move(X[0], V[0], SizeOf(TGHash128Buffer));

  for I := 0 to 127 do
  begin
    if GHashIsBitSet(@Y[0], I) then
      MemoryXor(@Z[0], @V[0], SizeOf(TGHash128Buffer), @Z[0]);

    B := GHashIsBitSet(@V[0], 127); // 判断大整数的高位是否是 1
    MemoryShiftRight(@V[0], nil, SizeOf(TGHash128Buffer), 1);
    if B then
      MemoryXor(@V[0], @GHASH_POLY[0], SizeOf(TGHash128Buffer), @V[0]);
  end;
  Move(Z[0], R[0], SizeOf(TGHash128Buffer));
end;

procedure GHash128(var HashKey: TGHash128Key; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer; var OutTag: TGHash128Tag);
var
  AL, DL: Integer;
  AL64, DL64: Int64;
  X, Y, H: TGHash128Buffer;
begin
  // 对比 GHash(H, A, C)，Data 是 C，AAD 是 A，Key 是 H
  // 按 16 字节分，C 有 m 块，A 有 n 块（末块都可能不满），
  // 共有 m + n 轮针对数据的 GaloisMulBlock，再加一轮位长度

  FillChar(X[0], SizeOf(TGHash128Buffer), 0);  // 初始全 0
  Move(HashKey[0], H[0], SizeOf(TGHash128Buffer));

  AL := AADByteLength;
  DL := DataByteLength;
  if Data = nil then
    DL := 0;
  if AAD = nil then
    AL := 0;

  // 算整块 A
  while AL >= GHASH_BLOCK do
  begin
    Move(AAD^, Y[0], GHASH_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);  // 一轮计算结果再次放入 X

    AAD := Pointer(TCnNativeInt(AAD) + GHASH_BLOCK);
    Dec(AL, GHASH_BLOCK);
  end;

  // 算余块 A，如果有的话
  if AL > 0 then
  begin
    FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
    Move(AAD^, Y[0], AL);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // 算整块 C
  while DL >= GHASH_BLOCK do
  begin
    Move(Data^, Y[0], GHASH_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);  // 一轮计算结果再次放入 X

    Data := Pointer(TCnNativeInt(Data) + GHASH_BLOCK);
    Dec(DL, GHASH_BLOCK);
  end;

  // 算余块 C，如果有的话
  if DL > 0 then
  begin
    FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
    Move(Data^, Y[0], DL);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // 最后再算一轮长度，A 和 C 各四字节拼起来，拼接要求符合网络标准与阅读习惯也就是 BigEndian
  FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
  AL64 := Int64ToBigEndian(AADByteLength * 8);
  DL64 := Int64ToBigEndian(DataByteLength * 8);

  Move(AL64, Y[0], SizeOf(Int64));
  Move(DL64, Y[SizeOf(Int64)], SizeOf(Int64));

  MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
  GMulBlock128(Y, H, X); // 再乘一轮

  Move(X[0], OutTag[0], SizeOf(TGHash128Tag));
end;

function GHash128Bytes(var HashKey: TGHash128Key; Data, AAD: TBytes): TGHash128Tag;
var
  C, A: Pointer;
begin
  if Data = nil then
    C := nil
  else
    C := @Data[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  GHash128(HashKey, C, Length(Data), A, Length(AAD), Result);
end;

procedure GHash128Start(var Ctx: TGHash128Context; var HashKey: TGHash128Key;
  AAD: Pointer; AADByteLength: Integer);
var
  Y: TGHash128Buffer;
begin
  FillChar(Ctx.State[0], SizeOf(TGHash128Buffer), 0);  // 初始全 0
  Move(HashKey[0], Ctx.HashKey[0], SizeOf(TGHash128Buffer));

  Ctx.DataByteLen := 0;
  Ctx.AADByteLen := AADByteLength;
  if AAD = nil then
    Ctx.AADByteLen := 0;

  // 算整块 A
  while AADByteLength >= GHASH_BLOCK do
  begin
    Move(AAD^, Y[0], GHASH_BLOCK);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);  // 一轮计算结果再次放入 Ctx.State

    AAD := Pointer(TCnNativeInt(AAD) + GHASH_BLOCK);
    Dec(AADByteLength, GHASH_BLOCK);
  end;

  // 算余块 A，如果有的话
  if AADByteLength > 0 then
  begin
    FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
    Move(AAD^, Y[0], AADByteLength);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);
  end;
end;

procedure GHash128Update(var Ctx: TGHash128Context; Data: Pointer; DataByteLength: Integer);
var
  Y: TGHash128Buffer;
begin
  if (Data = nil) or (DataByteLength <= 0) then
    Exit;

  Ctx.DataByteLen := Ctx.DataByteLen + DataByteLength;

  // 算整块 C
  while DataByteLength >= GHASH_BLOCK do
  begin
    Move(Data^, Y[0], GHASH_BLOCK);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);  // 一轮计算结果再次放入 Ctx.State

    Data := Pointer(TCnNativeInt(Data) + GHASH_BLOCK);
    Dec(DataByteLength, GHASH_BLOCK);
  end;

  // 算余块 C，如果有的话
  if DataByteLength > 0 then
  begin
    FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
    Move(Data^, Y[0], DataByteLength);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);
  end;
end;

procedure GHash128Finish(var Ctx: TGHash128Context; var Output: TGHash128Tag);
var
  Y: TGHash128Buffer;
  AL64, DL64: Int64;
begin
  // 最后再算一轮长度，A 和 C 各四字节拼起来
  FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
  AL64 := Int64ToBigEndian(Ctx.AADByteLen * 8);
  DL64 := Int64ToBigEndian(Ctx.DataByteLen * 8);

  Move(AL64, Y[0], SizeOf(Int64));
  Move(DL64, Y[SizeOf(Int64)], SizeOf(Int64));

  MemoryXor(@Y[0], @Ctx.State[0], SizeOf(TGHash128Buffer), @Y[0]);
  GMulBlock128(Y, Ctx.HashKey, Ctx.State); // 再乘一轮，

  Move(Ctx.State[0], Output[0], SizeOf(TGHash128Tag)); // 结果放 Output
end;

// 根据对称加密算法类型初始化加密密钥结构，注意不需要解密密钥结构
procedure GCMEncryptInit(var Context: TGCMContext; Key: Pointer;
  KeyByteLength: Integer; EncryptType: TGCMEncryptType);
var
  Key128: TAESKey128;
  Key192: TAESKey192;
  Key256: TAESKey256;
  SM4Key: TSM4Key;
  L: Integer;
begin
  FillChar(Context, SizeOf(TGCMContext), 0);
  L := KeyByteLength;

  case EncryptType of
    getAES128:
      begin
        if L > SizeOf(TAESKey128) then
          L := SizeOf(TAESKey128);
        FillChar(Key128[0], SizeOf(TAESKey128), 0);
        Move(Key^, Key128[0], L);
        ExpandAESKeyForEncryption(Key128, Context.ExpandedKey128);
      end;
    getAES192:
      begin
        if L > SizeOf(TAESKey192) then
          L := SizeOf(TAESKey192);
        FillChar(Key192[0], SizeOf(TAESKey192), 0);
        Move(Key^, Key192[0], L);
        ExpandAESKeyForEncryption(Key192, Context.ExpandedKey192);
      end;
    getAES256:
      begin
        if L > SizeOf(TAESKey256) then
          L := SizeOf(TAESKey256);
        FillChar(Key256[0], SizeOf(TAESKey256), 0);
        Move(Key^, Key256[0], L);
        ExpandAESKeyForEncryption(Key256, Context.ExpandedKey256);
      end;
    getSM4:
      begin
        if L > SizeOf(TSM4Key) then
          L := SizeOf(TSM4Key);
        FillChar(SM4Key[0], SizeOf(SM4Key), 0);
        Move(Key^, SM4Key[0], L);
        SM4SetKeyEnc(Context.SM4Context, @SM4Key[0]);
      end;
  end;
end;

// 根据对称加密算法类型加密一个块，各块串起来就是加密结果，注意不需要块解密
procedure GCMEncryptBlock(var Context: TGCMContext; var InData, OutData: TGCM128Buffer;
  EncryptType: TGCMEncryptType);
begin
  case EncryptType of
    getAES128: EncryptAES(TAESBuffer(InData), Context.ExpandedKey128, TAESBuffer(OutData));
    getAES192: EncryptAES(TAESBuffer(InData), Context.ExpandedKey192, TAESBuffer(OutData));
    getAES256: EncryptAES(TAESBuffer(InData), Context.ExpandedKey256, TAESBuffer(OutData));
    getSM4:    SM4OneRound(@(Context.SM4Context.Sk[0]), @InData[0], @OutData[0]);
  end;
end;

// 根据 Key、Iv、明文和额外数据，计算 GCM 加密密文与认证结果
procedure GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AuthData: Pointer;
  AuthDataByteLength: Integer; EnData: Pointer; var OutTag: TGCM128Tag;
  EncryptType: TGCMEncryptType);
var
  H: TGHash128Key;
  Y, Y0: TGHash128Buffer;// Y 拼合了计数器的内容
  Cnt, M: Cardinal;      // 计数器
  C: TGCM128Buffer;      // 加密中间数据存储地
  GcmCtx: TGCMContext;
  GHashCtx: TGHash128Context;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Iv = nil then
    IvByteLength := 0;
  if AuthData = nil then
    AuthDataByteLength := 0;

  GCMEncryptInit(GcmCtx, Key, KeyByteLength, EncryptType);

  // 计算 Enc(Key, 128 个 0)，得到 H
  FillChar(H[0], SizeOf(H), 0);
  GCMEncryptBlock(GcmCtx, TGCM128Buffer(H), TGCM128Buffer(H), EncryptType);

  // 初始化计数器，以后 Cnt 自增并塞进 Y 的后 32 位中
  if IvByteLength = GCM_NONCE_LENGTH then
  begin
    Move(Iv^, Y[0], GCM_NONCE_LENGTH);
    Cnt := 1;
    M := Int32ToBigEndian(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));
  end
  else
  begin
    GHash128(H, Iv, IvByteLength, nil, 0, TGHash128Tag(Y));
    Move(Y[GCM_NONCE_LENGTH], Cnt, SizeOf(Cardinal));
    ReverseMemory(@Cnt, SizeOf(Cardinal));
  end;

  // 先把最开始的 Y 值的加密结果计算出来
  GCMEncryptBlock(GcmCtx, TGCM128Buffer(Y), TGCM128Buffer(Y0), EncryptType);

  // 初始化 GHash
  GHash128Start(GHashCtx, H, AuthData, AuthDataByteLength);

  // 开始循环加密整块
  while PlainByteLength >= GCM_BLOCK do
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32ToBigEndian(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 对 Y 加密 C 暂时得到本块的加密结果
    GCMEncryptBlock(GcmCtx, TGCM128Buffer(Y), C, EncryptType);

    // 和明文异或，C 得到异或后的结果，完整块
    MemoryXor(PlainData, @C[0], SizeOf(TGCM128Buffer), @C[0]);

    // 存起密文
    Move(C[0], EnData^, SizeOf(TGCM128Buffer));

    // C 进行 GHash
    GHash128Update(GHashCtx, @C[0], SizeOf(TGCM128Buffer));

    // 准备下一步
    PlainData := Pointer(TCnNativeInt(PlainData) + GCM_BLOCK);
    EnData := Pointer(TCnNativeInt(EnData) + GCM_BLOCK);
    Dec(PlainByteLength, GCM_BLOCK);
  end;

  if PlainByteLength > 0 then
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32ToBigEndian(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 对 Y 加密 C 暂时得到本块的加密结果
    GCMEncryptBlock(GcmCtx, TGCM128Buffer(Y), C, EncryptType);

    // 和明文异或，C 得到异或后的结果，但长度只有 PlainByteLength
    MemoryXor(PlainData, @C[0], PlainByteLength, @C[0]);

    // 存起密文，完毕
    Move(C[0], EnData^, PlainByteLength);

    // C 进行 GHash
    GHash128Update(GHashCtx, @C[0], PlainByteLength);
  end;

  // 算出最终 GHash 的 Tag
  GHash128Finish(GHashCtx, TGHash128Tag(OutTag));

  // 再和开始的内容异或得到最终 Tag
  MemoryXor(@OutTag[0], @Y0[0], SizeOf(TGHash128Tag), @OutTag[0]);
end;

// 根据 Key、Iv、明文和额外数据，计算 GCM 加密密文与认证结果
function GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AuthData: Pointer;
  AuthDataByteLength: Integer; PlainData: Pointer; var InTag: TGCM128Tag;
  EncryptType: TGCMEncryptType): Boolean;
var
  H: TGHash128Key;
  Y, Y0: TGHash128Buffer;// Y 拼合了计数器的内容
  Cnt, M: Cardinal;      // 计数器
  C: TGCM128Buffer;      // 加密中间数据存储地
  GcmCtx: TGCMContext;
  GHashCtx: TGHash128Context;
  Tag: TGCM128Tag;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Iv = nil then
    IvByteLength := 0;
  if AuthData = nil then
    AuthDataByteLength := 0;

  GCMEncryptInit(GcmCtx, Key, KeyByteLength, EncryptType);

  // 计算 Enc(Key, 128 个 0)，得到 H
  FillChar(H[0], SizeOf(H), 0);
  GCMEncryptBlock(GcmCtx, TGCM128Buffer(H), TGCM128Buffer(H), EncryptType);

  // 初始化计数器，以后 Cnt 自增并塞进 Y 的后 32 位中
  if IvByteLength = GCM_NONCE_LENGTH then
  begin
    Move(Iv^, Y[0], GCM_NONCE_LENGTH);
    Cnt := 1;
    M := Int32ToBigEndian(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));
  end
  else
  begin
    GHash128(H, Iv, IvByteLength, nil, 0, TGHash128Tag(Y));
    Move(Y[GCM_NONCE_LENGTH], Cnt, SizeOf(Cardinal));
    ReverseMemory(@Cnt, SizeOf(Cardinal));
  end;

  // 先把最开始的 Y 值的加密结果计算出来
  GCMEncryptBlock(GcmCtx, TGCM128Buffer(Y), TGCM128Buffer(Y0), EncryptType);

  // 初始化 GHash
  GHash128Start(GHashCtx, H, AuthData, AuthDataByteLength);

  // 开始循环加密整块
  while EnByteLength >= GCM_BLOCK do
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32ToBigEndian(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 密文先进行 GHash
    GHash128Update(GHashCtx, EnData, SizeOf(TGCM128Buffer));

    // 对 Y 加密 C 暂时得到本块的加密结果
    GCMEncryptBlock(GcmCtx, TGCM128Buffer(Y), C, EncryptType);

    // 和密文异或，C 得到异或后的结果，完整块
    MemoryXor(EnData, @C[0], SizeOf(TGCM128Buffer), @C[0]);

    // 存起明文
    Move(C[0], PlainData^, SizeOf(TGCM128Buffer));

    // 准备下一步
    EnData := Pointer(TCnNativeInt(EnData) + GCM_BLOCK);
    PlainData := Pointer(TCnNativeInt(PlainData) + GCM_BLOCK);
    Dec(EnByteLength, GCM_BLOCK);
  end;

  if EnByteLength > 0 then
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32ToBigEndian(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 密文先进行 GHash
    GHash128Update(GHashCtx, EnData, EnByteLength);

    // 对 Y 加密 C 暂时得到本块的加密结果
    GCMEncryptBlock(GcmCtx, TGCM128Buffer(Y), C, EncryptType);

    // 和明文异或，C 得到异或后的结果，但长度只有 EnByteLength
    MemoryXor(EnData, @C[0], EnByteLength, @C[0]);

    // 存起密文，完毕
    Move(C[0], PlainData^, EnByteLength);
  end;

  // 算出最终 GHash 的 Tag
  GHash128Finish(GHashCtx, TGHash128Tag(Tag));

  // 再和开始的内容异或得到最终 Tag
  MemoryXor(@Tag[0], @Y0[0], SizeOf(TGHash128Tag), @Tag[0]);

  Result := CompareMem(@Tag[0], @InTag[0], SizeOf(TGHash128Tag));
end;

function GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag;
  EncryptType: TGCMEncryptType): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if PlainData = nil then
    P := nil
  else
    P := @PlainData[0];

  if AuthData = nil then
    A := nil
  else
    A := @AuthData[0];

  if Length(PlainData) > 0 then
  begin
    SetLength(Result, Length(PlainData));
    GCMEncrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AuthData), @Result[0], OutTag, EncryptType);
  end
  else
  begin
    GCMEncrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AuthData), nil, OutTag, EncryptType);
  end;
end;

function GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag;
  EncryptType: TGCMEncryptType): TBytes;
var
  K, I, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Iv = nil then
    I := nil
  else
    I := @Iv[0];

  if EnData = nil then
    P := nil
  else
    P := @EnData[0];

  if AuthData = nil then
    A := nil
  else
    A := @AuthData[0];

  if Length(EnData) > 0 then
  begin
    SetLength(Result, Length(EnData));
    if not GCMDecrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AuthData), @Result[0], InTag, EncryptType) then // Tag 比对失败则返回
      SetLength(Result, 0);
  end
  else
  begin
    GCMDecrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AuthData), nil, InTag, EncryptType); // 没密文，其实 Tag 比对成功与否都没用
  end;
end;

function AES128GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AuthData, OutTag, getAES128);
end;

function AES192GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AuthData, OutTag, getAES192);
end;

function AES256GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AuthData, OutTag, getAES256);
end;

function SM4GCMEncryptBytes(Key, Iv, PlainData, AuthData: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AuthData, OutTag, getSM4);
end;

function AES128GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AuthData, InTag, getAES128);
end;

function AES192GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AuthData, InTag, getAES192);
end;

function AES256GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AuthData, InTag, getAES256);
end;

function SM4GCMDecryptBytes(Key, Iv, EnData, AuthData: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AuthData, InTag, getSM4);
end;

end.
