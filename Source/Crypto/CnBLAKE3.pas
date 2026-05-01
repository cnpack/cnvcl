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

unit CnBLAKE3;
{* |<PRE>
================================================================================
* 软件名称：密码算法库
* 单元名称：BLAKE3 哈希算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
*           参考 https://github.com/BLAKE3-team/BLAKE3 的 C 参考实现移植而来
* 备    注：本单元实现 BLAKE3 杂凑算法，支持普通杂凑、带密钥杂凑（类似于 HMAC）、
*           派生密钥三种模式，以及 XOF 可变长度扩展输出。
* 开发平台：PWin7 + Delphi 7.0
* 兼容测试：PWinXP/7/10/11 + Delphi 5/6/7 ~ D12
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.06.15 V1.0
*               创建本单元。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} CnNative, CnConsts;

const
  CN_BLAKE3_OUTBYTES   = 32;
  {* BLAKE3 默认输出长度：32 字节}

  CN_BLAKE3_KEYBYTES   = 32;
  {* BLAKE3 KeyedHash 模式密钥长度：32 字节}

  CN_BLAKE3_BLOCKBYTES = 64;
  {* BLAKE3 Block 长度：64 字节}

  CN_BLAKE3_CHUNKBYTES = 1024;
  {* BLAKE3 Chunk 长度：1024 字节}

type
  ECnBLAKE3Exception = class(Exception);
  {* BLAKE3 相关异常}

  PCnBLAKE3Digest = ^TCnBLAKE3Digest;
  {* BLAKE3 摘要值指针}

  TCnBLAKE3Digest = array[0..CN_BLAKE3_OUTBYTES - 1] of Byte;
  {* BLAKE3 默认摘要类型，32 字节数组}

  TCnBLAKE3DerivedKey = array[0..CN_BLAKE3_OUTBYTES - 1] of Byte;
  {* BLAKE3 派生密钥结构}

  PCnBLAKE3DerivedKey = ^TCnBLAKE3DerivedKey;
  {* BLAKE3 派生密钥结构指针}

  TCnBLAKE3ChunkState = packed record
  {* BLAKE3 Chunk 状态（内部使用）}
    CV: array[0..7] of Cardinal;
    {* 当前链式值（8 个 32 位字）}
    ChunkCounter: TUInt64;
    {* Chunk 计数器}
    Buf: array[0..CN_BLAKE3_BLOCKBYTES - 1] of Byte;
    {* 64 字节缓冲区}
    BufLen: Byte;
    {* 缓冲区已用字节数}
    BlocksCompressed: Byte;
    {* 已压缩的 Block 数}
    Flags: Cardinal;
    {* 标志位}
  end;

  TCnBLAKE3Context = packed record
  {* BLAKE3 流式计算上下文结构体（公开）}
    Key: array[0..7] of Cardinal;
    {* 密钥字（8 个 32 位字）}
    Chunk: TCnBLAKE3ChunkState;
    {* 当前 Chunk 状态}
    CVStackLen: Byte;
    {* CV 栈深度（最大 54）}
    CVStack: array[0..54 * CN_BLAKE3_OUTBYTES - 1] of Byte;
    {* CV 栈（每个 CV 32 字节）}
  end;

  TCnBLAKE3CalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* 计算 BLAKE3 哈希值进度回调事件类型}

function BLAKE3(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar = nil;
  KeyLength: Integer = 0): TCnBLAKE3Digest;
{* 对数据块进行 BLAKE3 计算。

   参数：
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块字节长度
     Key: PAnsiChar                       - BLAKE3 密钥，默认为空
     KeyLength: Integer                   - BLAKE3 密钥字节长度，默认为 0

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

function BLAKE3Buffer(const Buffer; Count: Cardinal; const Key; KeyCount: Cardinal): TCnBLAKE3Digest;
{* 对数据块进行 BLAKE3 计算。

   参数：
     const Buffer                         - 待计算的数据块
     Count: Cardinal                      - 待计算的数据块字节长度
     const Key                            - BLAKE3 密钥
     KeyCount: Cardinal                   - BLAKE3 密钥字节长度

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

function BLAKE3Bytes(const Data: TBytes; const Key: TBytes = nil): TCnBLAKE3Digest;
{* 对字节数组进行 BLAKE3 计算。

   参数：
     const Data: TBytes                   - 待计算的字节数组
     const Key: TBytes                    - BLAKE3 密钥字节数组，默认为空

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

function BLAKE3String(const Str: string; const Key: string = ''): TCnBLAKE3Digest;
{* 对 String 类型数据进行 BLAKE3 计算，注意 D2009 或以上版本的 string 为 UnicodeString，
   代码中会将其强行转换成 AnsiString 进行计算。

   参数：
     const Str: string                    - 待计算的字符串
     const Key: string                    - BLAKE3 密钥的字符串形式

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

function BLAKE3StringA(const Str: AnsiString; const Key: AnsiString = ''): TCnBLAKE3Digest;
{* 对 AnsiString 类型数据进行 BLAKE3 计算。

   参数：
     const Str: AnsiString                - 待计算的字符串
     const Key: AnsiString                - BLAKE3 密钥的字符串形式

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

function BLAKE3StringW(const Str: WideString; const Key: WideString = ''): TCnBLAKE3Digest;
{* 对 WideString 类型字符串进行转换并进行 BLAKE3 计算。
   计算前 Windows 下会调用 WideCharToMultyByte 转换为 AnsiString 类型，
   其他平台会直接转换为 AnsiString 类型，再进行计算。

   参数：
     const Str: WideString                - 待计算的宽字符串
     const Key: WideString                - BLAKE3 密钥的宽字符串形式

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

{$IFDEF UNICODE}

function BLAKE3UnicodeString(const Str: string; const Key: string = ''): TCnBLAKE3Digest;
{* 对 UnicodeString 类型数据进行直接的 BLAKE3 计算，直接计算内部 UTF16 内容，不进行转换。

   参数：
     const Str: string                    - 待计算的宽字符串
     const Key: string                    - BLAKE3 密钥的宽字符串形式

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

{$ELSE}

function BLAKE3UnicodeString(const Str: WideString; const Key: WideString = ''): TCnBLAKE3Digest;
{* 对 UnicodeString 类型数据进行直接的 BLAKE3 计算，直接计算内部 UTF16 内容，不进行转换。

   参数：
     const Str: string                    - 待计算的宽字符串
     const Key: WideString                - BLAKE3 密钥的宽字符串形式

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

{$ENDIF}

function BLAKE3File(const FileName: string; Key: TBytes = nil;
  CallBack: TCnBLAKE3CalcProgressFunc = nil): TCnBLAKE3Digest;
{* 对指定文件内容进行 BLAKE3 计算。

   参数：
     const FileName: string               - 待计算的文件名
     Key: TBytes                          - BLAKE3 密钥字节数组，默认为空
     CallBack: TCnBLAKE3CalcProgressFunc  - 进度回调函数，默认为空

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

function BLAKE3Stream(Stream: TStream; Key: TBytes = nil;
  CallBack: TCnBLAKE3CalcProgressFunc = nil): TCnBLAKE3Digest;
{* 对指定流数据进行 BLAKE3 计算。

   参数：
     Stream: TStream                      - 待计算的流内容
     Key: TBytes                          - BLAKE3 密钥字节数组，默认为空
     CallBack: TCnBLAKE3CalcProgressFunc  - 进度回调函数，默认为空

   返回值：TCnBLAKE3Digest                - 返回的 BLAKE3 杂凑值
}

// 以下三类函数用于外部持续对数据进行零散的 BLAKE3 计算，BLAKE3Update 可多次被调用

procedure BLAKE3Init(var Context: TCnBLAKE3Context; Key: PAnsiChar = nil; KeyLength: Integer = 0);
{* 初始化一轮 BLAKE3 计算上下文，准备计算 BLAKE3 结果。注意当 Key 存在时长度将截断或补 #0 为 32 字节。

   参数：
     var Context: TCnBLAKE2SContext       - 待初始化的 BLAKE3 上下文
     Key: PAnsiChar                       - BLAKE3 密钥，默认为空
     KeyLength: Integer                   - BLAKE3 密钥字节长度，默认为 0

   返回值：（无）
}

procedure BLAKE3InitDeriveKey(var Context: TCnBLAKE3Context; ContextStr: PAnsiChar; ContextLength: Integer);
{* 使用派生密钥模式初始化一轮 BLAKE3 计算上下文，准备计算 BLAKE3 结果。

   参数：
     var Context: TCnBLAKE2SContext       - 待初始化的 BLAKE3 上下文
     ContextStr: PAnsiChar                - BLAKE3 派生密钥，默认为空
     KeyLength: Integer                   - BLAKE3 密钥字节长度，默认为 0

   返回值：（无）
}

procedure BLAKE3Update(var Context: TCnBLAKE3Context; Input: PAnsiChar; ByteLength: Cardinal);
{* 以初始化后的上下文对一块数据进行 BLAKE3 计算。
   可多次调用以连续计算不同的数据块，无需将不同的数据块拼凑在连续的内存中。

   参数：
     var Context: TCnBLAKE2BContext       - BLAKE3 上下文
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块的字节长度

   返回值：（无）
}

procedure BLAKE3Final(var Context: TCnBLAKE3Context; var Digest: TCnBLAKE3Digest);
{* 结束本轮计算，将 BLAKE3 结果返回至 Digest 中。

   参数：
     var Context: TCnBLAKE2BContext       - BLAKE3 上下文
     var Digest: TCnBLAKE3Digest          - 返回的 BLAKE3 杂凑值

   返回值：（无）
}

procedure BLAKE3FinalXOF(var Context: TCnBLAKE3Context; OutBuf: PByte; OutLength: Integer);
{* 结束本轮计算，将 OutLength 字节的 XOF 输出写入 OutBuf。

   参数：
     var Context: TCnBLAKE2BContext       - BLAKE3 上下文
     OutBuf: PByte                        - 供写入变长杂凑结果的数据区
     OutLength: Integer                   - 需写入变长杂凑结果的字节长度

   返回值：（无）
}

function BLAKE3DeriveKey(Context: PAnsiChar; ContextLength: Integer;
  KeyMaterial: PAnsiChar; KeyMaterialLength: Integer): TCnBLAKE3DerivedKey;
{* 使用派生密钥模式，通过数据块形式的上下文和密钥材料派生出 BLAKE3 密钥。

   参数：
     Context: PAnsiChar                   - BLAKE3 上下文数据块地址
     ContextLength: Integer               - BLAKE3 上下文数据块字节长度
     KeyMaterial: PAnsiChar               - BLAKE3 密钥材料地址
     KeyMaterialLength: Integer           - BLAKE3 密钥材料地址的字节长度

   返回值：TCnBLAKE3DerivedKey            - 返回的 BLAKE3 派生密钥
}

function BLAKE3DeriveKeyBytes(const Context: TBytes;
  const KeyMaterial: TBytes): TCnBLAKE3DerivedKey;
{* 使用派生密钥模式，通过字节数组形式的上下文和密钥材料派生出 BLAKE3 密钥。

   参数：
     Context: TBytes                      - BLAKE3 上下文字节数组
     KeyMaterial: TBytes                  - BLAKE3 密钥材料字节数组

   返回值：TCnBLAKE3DerivedKey            - 返回的 BLAKE3 派生密钥
}

function BLAKE3DeriveKeyStr(const Context: AnsiString;
  const KeyMaterial: TBytes): TCnBLAKE3DerivedKey;
{* 使用派生密钥模式，通过 AnsiString 形式的上下文和字节数组形式的密钥材料派生出 BLAKE3 密钥。

   参数：
     Context: AnsiString                  - BLAKE3 上下文字符串
     KeyMaterial: AnsiString              - BLAKE3 密钥材料字符串

   返回值：TCnBLAKE3DerivedKey            - 返回的 BLAKE3 派生密钥
}

function BLAKE3XOF(Input: PAnsiChar; ByteLength: Cardinal; OutLength: Integer): TBytes;
{* 对数据块计算 BLAKE3 XOF 可变长度杂凑输出，返回 OutLength 字节长度的杂凑结果。

   参数：
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块字节长度
     OutLength: Integer                   - 输出的杂凑结果的字节长度

   返回值：TBytes                         - 返回 OutLength 字节长度的杂凑结果
}

function BLAKE3XOFBuffer(const Buffer; Count: Cardinal; OutLength: Integer): TBytes;
{* 对内存块计算 BLAKE3 XOF 可变长度杂凑输出，返回 OutLength 字节长度的杂凑结果。

   参数：
     const Buffer                         - 待计算的数据块
     Count: Cardinal                      - 待计算的数据块字节长度
     OutLength: Integer                   - 输出的杂凑结果的字节长度

   返回值：TBytes                         - 返回 OutLength 字节长度的杂凑结果
}

function BLAKE3XOFBytes(const Data: TBytes; OutLength: Integer): TBytes;
{* 对字节数组计算 BLAKE3 XOF 可变长度杂凑输出，返回 OutLength 字节长度的杂凑结果。

   参数：
     const Data: TBytes                   - 待计算的字节数组
     OutLength: Integer                   - 输出的杂凑结果的字节长度

   返回值：TBytes                         - 返回 OutLength 字节长度的杂凑结果
}

function BLAKE3Print(const Digest: TCnBLAKE3Digest): string;
{* 以十六进制格式输出 BLAKE3 杂凑值，返回 64 个小写十六进制字符的字符串。

   参数：
     const Digest: TCnBLAKE3Digest        - 指定的 BLAKE3 杂凑值

   返回值：string                         - 返回十六进制字符串
}

function BLAKE3Match(const D1, D2: TCnBLAKE3Digest): Boolean;
{* 比较两个 BLAKE3 杂凑值是否相等。

   参数：
     const D1: TCnBLAKE3Digest            - 待比较的 BLAKE3 杂凑值一
     const D2: TCnBLAKE3Digest            - 待比较的 BLAKE3 杂凑值二

   返回值：Boolean                        - 返回是否相等
}

function BLAKE3DigestToStr(const Digest: TCnBLAKE3Digest): string;
{* BLAKE3 杂凑值内容直接转 string，每字节对应一字符。

   参数：
     const Digest: TCnBLAKE3Digest        - 待转换的 BLAKE3 杂凑值

   返回值：string                         - 返回的字符串
}

implementation

resourcestring
  SCnErrorBLAKE3InvalidXOFSize = 'BLAKE3 XOF Output Length must be Positive';

const
  BLAKE3_IV: array[0..7] of Cardinal = (
    $6A09E667, $BB67AE85, $3C6EF372, $A54FF53A,
    $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
  );
  {* BLAKE3 初始化向量，与 SHA-256 相同}

  BLAKE3_MSG_PERMUTATION: array[0..15] of Byte = (
    2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8
  );
  {* BLAKE3 消息置换表}

  BLAKE3_FLAG_CHUNK_START         = $01;
  {* Chunk 第一个 Block 标志}
  BLAKE3_FLAG_CHUNK_END           = $02;
  {* Chunk 最后一个 Block 标志}
  BLAKE3_FLAG_PARENT              = $04;
  {* 父节点标志}
  BLAKE3_FLAG_ROOT                = $08;
  {* 根节点标志}
  BLAKE3_FLAG_KEYED_HASH          = $10;
  {* Keyed Hash 模式标志}
  BLAKE3_FLAG_DERIVE_KEY_CONTEXT  = $20;
  {* DeriveKey 上下文哈希标志}
  BLAKE3_FLAG_DERIVE_KEY_MATERIAL = $40;
  {* DeriveKey 材料哈希标志}

  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

function ROR32(A, B: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A shr B) or (A shl (32 - B));
end;

// BLAKE3 G 混合函数，旋转量：16, 12, 8, 7（与 BLAKE2s 相同）
procedure BLAKE3G(var State: array of Cardinal; A, B, C, D: Integer; MX, MY: Cardinal);
begin
  State[A] := State[A] + State[B] + MX;
  State[D] := ROR32(State[D] xor State[A], 16);
  State[C] := State[C] + State[D];
  State[B] := ROR32(State[B] xor State[C], 12);
  State[A] := State[A] + State[B] + MY;
  State[D] := ROR32(State[D] xor State[A], 8);
  State[C] := State[C] + State[D];
  State[B] := ROR32(State[B] xor State[C], 7);
end;

// BLAKE3 单轮压缩：列混合 + 对角线混合
procedure BLAKE3Round(var State: array of Cardinal; const M: array of Cardinal);
begin
  // 列混合
  BLAKE3G(State, 0, 4,  8, 12, M[ 0], M[ 1]);
  BLAKE3G(State, 1, 5,  9, 13, M[ 2], M[ 3]);
  BLAKE3G(State, 2, 6, 10, 14, M[ 4], M[ 5]);
  BLAKE3G(State, 3, 7, 11, 15, M[ 6], M[ 7]);
  // 对角线混合
  BLAKE3G(State, 0, 5, 10, 15, M[ 8], M[ 9]);
  BLAKE3G(State, 1, 6, 11, 12, M[10], M[11]);
  BLAKE3G(State, 2, 7,  8, 13, M[12], M[13]);
  BLAKE3G(State, 3, 4,  9, 14, M[14], M[15]);
end;

// 按 BLAKE3_MSG_PERMUTATION 对 16 个消息字重排
procedure BLAKE3Permute(var M: array of Cardinal);
var
  Orig: array[0..15] of Cardinal;
  I: Integer;
begin
  for I := 0 to 15 do
    Orig[I] := M[I];
  for I := 0 to 15 do
    M[I] := Orig[BLAKE3_MSG_PERMUTATION[I]];
end;

// BLAKE3 完整压缩函数，产生 16 个 32 位字的输出
procedure BLAKE3Compress(const CV: array of Cardinal; const BlockWords: array of Cardinal;
  Counter: TUInt64; BlockLen: Cardinal; Flags: Cardinal; var Output: array of Cardinal);
var
  State: array[0..15] of Cardinal;
  M: array[0..15] of Cardinal;
  I: Integer;
begin
  // 初始化 16 字状态
  for I := 0 to 7 do
    State[I] := CV[I];
  State[ 8] := BLAKE3_IV[0];
  State[ 9] := BLAKE3_IV[1];
  State[10] := BLAKE3_IV[2];
  State[11] := BLAKE3_IV[3];
  State[12] := Cardinal(Counter and $FFFFFFFF);
  State[13] := Cardinal(Counter shr 32);
  State[14] := BlockLen;
  State[15] := Flags;

  // 复制消息字
  for I := 0 to 15 do
    M[I] := BlockWords[I];

  // 执行 7 轮（最后一轮不做置换）
  BLAKE3Round(State, M); BLAKE3Permute(M);
  BLAKE3Round(State, M); BLAKE3Permute(M);
  BLAKE3Round(State, M); BLAKE3Permute(M);
  BLAKE3Round(State, M); BLAKE3Permute(M);
  BLAKE3Round(State, M); BLAKE3Permute(M);
  BLAKE3Round(State, M); BLAKE3Permute(M);
  BLAKE3Round(State, M);

  // 生成输出
  for I := 0 to 7 do
    Output[I] := State[I] xor State[I + 8];
  for I := 0 to 7 do
    Output[I + 8] := State[I + 8] xor CV[I];
end;

// 将 64 字节 Block 按小端序装入 16 个 Cardinal
procedure BlockBytesToWords(const Block: array of Byte; var Words: array of Cardinal);
var
  I: Integer;
  P: PCardinal;
begin
  for I := 0 to 15 do
  begin
    P := PCardinal(TCnIntAddress(@Block[0]) + I * SizeOf(Cardinal));
    Words[I] := UInt32ToLittleEndian(P^);
  end;
end;

// 将 Cardinal 数组按小端序写出为字节缓冲区
procedure WordsToBytes(const Words: array of Cardinal; OutBuf: PByte; Count: Integer);
var
  I: Integer;
  W: Cardinal;
begin
  for I := 0 to Count - 1 do
  begin
    W := UInt32ToLittleEndian(Words[I]);
    Move(W, PByte(TCnIntAddress(OutBuf) + I * SizeOf(Cardinal))^, SizeOf(Cardinal));
  end;
end;

// 初始化 Chunk 状态
procedure ChunkStateInit(var Chunk: TCnBLAKE3ChunkState; const KeyWords: array of Cardinal;
  ChunkCounter: TUInt64; Flags: Cardinal);
var
  I: Integer;
begin
  for I := 0 to 7 do
    Chunk.CV[I] := KeyWords[I];
  Chunk.ChunkCounter := ChunkCounter;
  FillChar(Chunk.Buf[0], SizeOf(Chunk.Buf), 0);
  Chunk.BufLen := 0;
  Chunk.BlocksCompressed := 0;
  Chunk.Flags := Flags;
end;

// 返回当前 Chunk 已处理的字节数
function ChunkStateLen(const Chunk: TCnBLAKE3ChunkState): Cardinal;
begin
  Result := Cardinal(Chunk.BlocksCompressed) * CN_BLAKE3_BLOCKBYTES + Chunk.BufLen;
end;

// 返回当前 Block 是否为 Chunk 第一个 Block 的标志
function ChunkStateStartFlag(const Chunk: TCnBLAKE3ChunkState): Cardinal;
begin
  if Chunk.BlocksCompressed = 0 then
    Result := BLAKE3_FLAG_CHUNK_START
  else
    Result := 0;
end;

// 将输入字节填入 Chunk 缓冲区，满 64 字节时调用 BLAKE3Compress 压缩
procedure ChunkStateUpdate(var Chunk: TCnBLAKE3ChunkState; Input: PByte; ByteLength: Cardinal);
var
  Want, Take: Cardinal;
  BlockWords: array[0..15] of Cardinal;
  CompressOut: array[0..15] of Cardinal;
begin
  while ByteLength > 0 do
  begin
    // 如果缓冲区已满，先压缩
    if Chunk.BufLen = CN_BLAKE3_BLOCKBYTES then
    begin
      BlockBytesToWords(Chunk.Buf, BlockWords);
      BLAKE3Compress(Chunk.CV, BlockWords, Chunk.ChunkCounter,
        CN_BLAKE3_BLOCKBYTES, Chunk.Flags or ChunkStateStartFlag(Chunk), CompressOut);
      // 更新 CV 为压缩输出的前 8 个字
      Move(CompressOut[0], Chunk.CV[0], 8 * SizeOf(Cardinal));
      Inc(Chunk.BlocksCompressed);
      Chunk.BufLen := 0;
      FillChar(Chunk.Buf[0], SizeOf(Chunk.Buf), 0);
    end;

    // 填充缓冲区
    Want := CN_BLAKE3_BLOCKBYTES - Chunk.BufLen;
    if ByteLength < Want then
      Take := ByteLength
    else
      Take := Want;
    Move(Input^, Chunk.Buf[Chunk.BufLen], Take);
    Inc(Chunk.BufLen, Take);
    Inc(Input, Take);
    Dec(ByteLength, Take);
  end;
end;

// 对最后一个 Block 调用 BLAKE3Compress，设置 CHUNK_END 标志，返回前 8 个字作为 CV
procedure ChunkStateOutput(const Chunk: TCnBLAKE3ChunkState; var OutCV: array of Cardinal);
var
  BlockWords: array[0..15] of Cardinal;
  CompressOut: array[0..15] of Cardinal;
  BlockFlags: Cardinal;
begin
  BlockFlags := Chunk.Flags or ChunkStateStartFlag(Chunk) or BLAKE3_FLAG_CHUNK_END;
  BlockBytesToWords(Chunk.Buf, BlockWords);
  BLAKE3Compress(Chunk.CV, BlockWords, Chunk.ChunkCounter,
    Chunk.BufLen, BlockFlags, CompressOut);
  Move(CompressOut[0], OutCV[0], 8 * SizeOf(Cardinal));
end;

// 计算父节点 CV：将两个子 CV 拼接为 64 字节 Block，调用 BLAKE3Compress
procedure ParentCV(const LeftChildCV, RightChildCV: array of Cardinal;
  const KeyWords: array of Cardinal; Flags: Cardinal; var OutCV: array of Cardinal);
var
  BlockWords: array[0..15] of Cardinal;
  CompressOut: array[0..15] of Cardinal;
  I: Integer;
begin
  for I := 0 to 7 do
    BlockWords[I] := LeftChildCV[I];
  for I := 0 to 7 do
    BlockWords[I + 8] := RightChildCV[I];
  BLAKE3Compress(KeyWords, BlockWords, 0, CN_BLAKE3_BLOCKBYTES,
    Flags or BLAKE3_FLAG_PARENT, CompressOut);
  Move(CompressOut[0], OutCV[0], 8 * SizeOf(Cardinal));
end;

// 将新 Chunk 的 CV 压入 CVStack，根据 TotalChunks 尾部零位数决定合并次数
procedure AddChunkCV(var Context: TCnBLAKE3Context; const NewCV: array of Cardinal;
  TotalChunks: TUInt64);
var
  PostMerge: TUInt64;
  LeftCV, RightCV, MergedCV: array[0..7] of Cardinal;
  StackOffset: Integer;
begin
  // 将新 CV 压入栈
  Move(NewCV[0], Context.CVStack[Context.CVStackLen * CN_BLAKE3_OUTBYTES],
    CN_BLAKE3_OUTBYTES);
  Inc(Context.CVStackLen);

  // 根据 TotalChunks 的尾部零位数决定合并次数
  PostMerge := TotalChunks;
  while (PostMerge and 1) = 0 do
  begin
    PostMerge := PostMerge shr 1;
    // 弹出栈顶两个 CV，合并后压回
    StackOffset := (Context.CVStackLen - 2) * CN_BLAKE3_OUTBYTES;
    Move(Context.CVStack[StackOffset], LeftCV[0], CN_BLAKE3_OUTBYTES);
    Move(Context.CVStack[StackOffset + CN_BLAKE3_OUTBYTES], RightCV[0], CN_BLAKE3_OUTBYTES);
    ParentCV(LeftCV, RightCV, Context.Key, Context.Chunk.Flags, MergedCV);
    Dec(Context.CVStackLen);
    Move(MergedCV[0], Context.CVStack[(Context.CVStackLen - 1) * CN_BLAKE3_OUTBYTES],
      CN_BLAKE3_OUTBYTES);
  end;
end;

// 将 CVStack 中剩余的所有 CV 从右到左依次合并，直到只剩一个根 CV
procedure MergeCV(var Context: TCnBLAKE3Context; var RootCV: array of Cardinal);
var
  LeftCV, RightCV, MergedCV: array[0..7] of Cardinal;
  StackOffset: Integer;
begin
  // 最后一个 CV 就是当前 Chunk 的 CV，已在调用前设置到 RootCV
  while Context.CVStackLen > 0 do
  begin
    StackOffset := (Context.CVStackLen - 1) * CN_BLAKE3_OUTBYTES;
    Move(Context.CVStack[StackOffset], LeftCV[0], CN_BLAKE3_OUTBYTES);
    Move(RootCV[0], RightCV[0], CN_BLAKE3_OUTBYTES);
    ParentCV(LeftCV, RightCV, Context.Key, Context.Chunk.Flags, MergedCV);
    Dec(Context.CVStackLen);
    Move(MergedCV[0], RootCV[0], CN_BLAKE3_OUTBYTES);
  end;
end;

procedure BLAKE3Init(var Context: TCnBLAKE3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  P: PCardinal;
  KeyBuf: array[0..CN_BLAKE3_KEYBYTES - 1] of Byte;
  Take: Integer;
begin
  FillChar(Context, SizeOf(TCnBLAKE3Context), 0);
  if (Key <> nil) and (KeyLength > 0) then
  begin
    FillChar(KeyBuf[0], CN_BLAKE3_KEYBYTES, 0);
    if KeyLength > CN_BLAKE3_KEYBYTES then
      Take := CN_BLAKE3_KEYBYTES
    else
      Take := KeyLength;
    Move(Key^, KeyBuf[0], Take);

    // 将 32 字节密钥按小端序解析为 8 个 Cardinal
    for I := 0 to 7 do
    begin
      P := PCardinal(TCnIntAddress(@KeyBuf[0]) + I * SizeOf(Cardinal));
      Context.Key[I] := UInt32ToLittleEndian(P^);
    end;
    ChunkStateInit(Context.Chunk, Context.Key, 0, BLAKE3_FLAG_KEYED_HASH);
  end
  else
  begin
    Move(BLAKE3_IV[0], Context.Key[0], 8 * SizeOf(Cardinal));
    ChunkStateInit(Context.Chunk, BLAKE3_IV, 0, 0);
  end;
end;

procedure BLAKE3InitDeriveKey(var Context: TCnBLAKE3Context; ContextStr: PAnsiChar; ContextLength: Integer);
var
  CtxHasher: TCnBLAKE3Context;
  CtxKey: TCnBLAKE3DerivedKey;
  TmpDigest: TCnBLAKE3Digest;
  I: Integer;
  P: PCardinal;
begin
  // 第一步：用 DERIVE_KEY_CONTEXT 标志对 ContextStr 哈希，得到 32 字节上下文密钥
  FillChar(CtxHasher, SizeOf(TCnBLAKE3Context), 0);
  Move(BLAKE3_IV[0], CtxHasher.Key[0], 8 * SizeOf(Cardinal));
  ChunkStateInit(CtxHasher.Chunk, BLAKE3_IV, 0, BLAKE3_FLAG_DERIVE_KEY_CONTEXT);
  BLAKE3Update(CtxHasher, ContextStr, ContextLength);
  BLAKE3Final(CtxHasher, TmpDigest);
  Move(TmpDigest[0], CtxKey[0], CN_BLAKE3_OUTBYTES);

  // 第二步：将上下文密钥解析为 8 个 Cardinal，作为新的密钥字
  FillChar(Context, SizeOf(TCnBLAKE3Context), 0);
  for I := 0 to 7 do
  begin
    P := PCardinal(TCnIntAddress(@CtxKey[0]) + I * SizeOf(Cardinal));
    Context.Key[I] := UInt32ToLittleEndian(P^);
  end;
  ChunkStateInit(Context.Chunk, Context.Key, 0, BLAKE3_FLAG_DERIVE_KEY_MATERIAL);
end;

procedure BLAKE3Update(var Context: TCnBLAKE3Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  ChunkCV: array[0..7] of Cardinal;
  ChunkSpace: Cardinal;
  TotalChunks: TUInt64;
begin
  if (Input = nil) or (ByteLength = 0) then
    Exit;

  // 处理输入数据
  while ByteLength > 0 do
  begin
    // 当前 Chunk 剩余空间
    ChunkSpace := CN_BLAKE3_CHUNKBYTES - ChunkStateLen(Context.Chunk);

    if ByteLength <= ChunkSpace then
    begin
      // 数据可以放入当前 Chunk
      ChunkStateUpdate(Context.Chunk, PByte(Input), ByteLength);
      Exit;
    end;

    // 填满当前 Chunk
    ChunkStateUpdate(Context.Chunk, PByte(Input), ChunkSpace);
    Inc(Input, ChunkSpace);
    Dec(ByteLength, ChunkSpace);

    // 完成当前 Chunk，获取其 CV
    ChunkStateOutput(Context.Chunk, ChunkCV);
    TotalChunks := Context.Chunk.ChunkCounter + 1;
    AddChunkCV(Context, ChunkCV, TotalChunks);

    // 初始化下一个 Chunk
    ChunkStateInit(Context.Chunk, Context.Key, TotalChunks, Context.Chunk.Flags);
  end;
end;
// Internal helper: get root output words (16 words) with ROOT flag
procedure BLAKE3GetRootOutput(var Context: TCnBLAKE3Context;
  Counter: TUInt64; var OutWords: array of Cardinal);
var
  CtxCopy: TCnBLAKE3Context;
  LastChunkCV: array[0..7] of Cardinal;
  LeftCV, RightCV, MergedCV: array[0..7] of Cardinal;
  BlockWords: array[0..15] of Cardinal;
  RootFlags: Cardinal;
  I: Integer;
begin
  CtxCopy := Context;

  // Get the last chunk CV
  ChunkStateOutput(CtxCopy.Chunk, LastChunkCV);

  if CtxCopy.CVStackLen = 0 then
  begin
    // Single chunk: root is the chunk itself
    // Re-compress the last block with ROOT flag and XOF counter
    BlockBytesToWords(CtxCopy.Chunk.Buf, BlockWords);
    RootFlags := CtxCopy.Chunk.Flags or ChunkStateStartFlag(CtxCopy.Chunk)
      or BLAKE3_FLAG_CHUNK_END or BLAKE3_FLAG_ROOT;
    BLAKE3Compress(CtxCopy.Chunk.CV, BlockWords, Counter,
      CtxCopy.Chunk.BufLen, RootFlags, OutWords);
  end
  else
  begin
    // Multiple chunks: merge all CVs, last merge uses ROOT flag
    // Start with the last chunk CV as the right child
    Move(LastChunkCV[0], RightCV[0], CN_BLAKE3_OUTBYTES);

    // Merge all but the bottom stack entry
    while CtxCopy.CVStackLen > 1 do
    begin
      Move(CtxCopy.CVStack[(CtxCopy.CVStackLen - 1) * CN_BLAKE3_OUTBYTES],
        LeftCV[0], CN_BLAKE3_OUTBYTES);
      ParentCV(LeftCV, RightCV, CtxCopy.Key, CtxCopy.Chunk.Flags, MergedCV);
      Dec(CtxCopy.CVStackLen);
      Move(MergedCV[0], RightCV[0], CN_BLAKE3_OUTBYTES);
    end;

    // Final merge with ROOT flag
    Move(CtxCopy.CVStack[0], LeftCV[0], CN_BLAKE3_OUTBYTES);
    for I := 0 to 7 do
      BlockWords[I] := LeftCV[I];
    for I := 0 to 7 do
      BlockWords[I + 8] := RightCV[I];
    BLAKE3Compress(CtxCopy.Key, BlockWords, Counter, CN_BLAKE3_BLOCKBYTES,
      CtxCopy.Chunk.Flags or BLAKE3_FLAG_PARENT or BLAKE3_FLAG_ROOT, OutWords);
  end;
end;

procedure BLAKE3Final(var Context: TCnBLAKE3Context; var Digest: TCnBLAKE3Digest);
var
  OutWords: array[0..15] of Cardinal;
begin
  BLAKE3GetRootOutput(Context, 0, OutWords);
  WordsToBytes(OutWords, @Digest[0], 8);
end;

procedure BLAKE3FinalXOF(var Context: TCnBLAKE3Context; OutBuf: PByte; OutLength: Integer);
var
  OutWords: array[0..15] of Cardinal;
  BlockBuf: array[0..63] of Byte;
  BlockCounter: TUInt64;
  Written, Take: Integer;
begin
  if OutLength <= 0 then
    raise ECnBLAKE3Exception.Create(SCnErrorBLAKE3InvalidXOFSize);

  BlockCounter := 0;
  Written := 0;
  while Written < OutLength do
  begin
    BLAKE3GetRootOutput(Context, BlockCounter, OutWords);
    WordsToBytes(OutWords, @BlockBuf[0], 16);
    Take := OutLength - Written;
    if Take > CN_BLAKE3_BLOCKBYTES then
      Take := CN_BLAKE3_BLOCKBYTES;
    Move(BlockBuf[0], PByte(TCnIntAddress(OutBuf) + Written)^, Take);
    Inc(Written, Take);
    Inc(BlockCounter);
  end;
end;
// ============ 普通哈希便捷函数实现 ============

function BLAKE3(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar;
  KeyLength: Integer): TCnBLAKE3Digest;
var
  Context: TCnBLAKE3Context;
begin
  if (Key <> nil) and (KeyLength > 0) then
    BLAKE3Init(Context, Key, KeyLength)
  else
    BLAKE3Init(Context);
  BLAKE3Update(Context, Input, ByteLength);
  BLAKE3Final(Context, Result);
end;

function BLAKE3Buffer(const Buffer; Count: Cardinal; const Key; KeyCount: Cardinal): TCnBLAKE3Digest;
var
  Context: TCnBLAKE3Context;
begin
  if KeyCount > 0 then
    BLAKE3Init(Context, PAnsiChar(@Key), KeyCount)
  else
    BLAKE3Init(Context);
  BLAKE3Update(Context, PAnsiChar(@Buffer), Count);
  BLAKE3Final(Context, Result);
end;

function BLAKE3Bytes(const Data: TBytes; const Key: TBytes): TCnBLAKE3Digest;
var
  D: PAnsiChar;
  DL: Cardinal;
begin
  if (Data = nil) or (Length(Data) = 0) then
  begin
    D := nil;
    DL := 0;
  end
  else
  begin
    D := PAnsiChar(@Data[0]);
    DL := Length(Data);
  end;
  if (Key <> nil) and (Length(Key) > 0) then
    Result := BLAKE3(D, DL, PAnsiChar(@Key[0]), Length(Key))
  else
    Result := BLAKE3(D, DL, nil, 0);
end;

function BLAKE3String(const Str: string; const Key: string): TCnBLAKE3Digest;
var
  AStr: AnsiString;
  AKey: AnsiString;
begin
  AStr := AnsiString(Str);
  AKey := AnsiString(Key);
  Result := BLAKE3StringA(AStr, AKey);
end;

function BLAKE3StringA(const Str: AnsiString; const Key: AnsiString): TCnBLAKE3Digest;
var
  Context: TCnBLAKE3Context;
begin
  if Length(Key) > 0 then
    BLAKE3Init(Context, PAnsiChar(Key), Length(Key))
  else
    BLAKE3Init(Context);
  BLAKE3Update(Context, PAnsiChar(Str), Length(Str));
  BLAKE3Final(Context, Result);
end;

function BLAKE3StringW(const Str: WideString; const Key: WideString): TCnBLAKE3Digest;
var
  Context: TCnBLAKE3Context;
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
  KeyA: PAnsiChar;
  KeyLen: Cardinal;
{$ELSE}
  S: string;
  A: AnsiString;
  AKey: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Length(Key) > 0 then
  begin
    GetMem(KeyA, Length(Key) * SizeOf(WideChar));
    try
      KeyLen := WideCharToMultiByte(0, 0, PWideChar(Key), Length(Key),
        PAnsiChar(KeyA), Length(Key) * SizeOf(WideChar), nil, nil);
      BLAKE3Init(Context, KeyA, KeyLen);
    finally
      FreeMem(KeyA);
    end;
  end
  else
    BLAKE3Init(Context);

  GetMem(Content, Length(Str) * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, PWideChar(Str), Length(Str),
      PAnsiChar(Content), Length(Str) * SizeOf(WideChar), nil, nil);
    BLAKE3Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}
  if Length(Key) > 0 then
  begin
    S := StrNew(PWideChar(Key));
    AKey := AnsiString(S);
    BLAKE3Init(Context, PAnsiChar(AKey), Length(AKey));
  end
  else
    BLAKE3Init(Context);

  S := StrNew(PWideChar(Str));
  A := AnsiString(S);
  BLAKE3Update(Context, PAnsiChar(A), Length(A));
{$ENDIF}
  BLAKE3Final(Context, Result);
end;

{$IFDEF UNICODE}
function BLAKE3UnicodeString(const Str: string; const Key: string): TCnBLAKE3Digest;
{$ELSE}
function BLAKE3UnicodeString(const Str: WideString; const Key: WideString): TCnBLAKE3Digest;
{$ENDIF}
var
  Context: TCnBLAKE3Context;
begin
  if Length(Key) > 0 then
    BLAKE3Init(Context, PAnsiChar(@Key[1]), Length(Key) * SizeOf(WideChar))
  else
    BLAKE3Init(Context);
  if Length(Str) > 0 then
    BLAKE3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE3Final(Context, Result);
end;

function InternalBLAKE3Stream(Stream: TStream; const BufSize: Cardinal;
  var Context: TCnBLAKE3Context; CallBack: TCnBLAKE3CalcProgressFunc): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
begin
  Result := False;
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then
  begin
    Result := True;
    Exit;
  end;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        BLAKE3Update(Context, Buf, ReadBytes);

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

function BLAKE3Stream(Stream: TStream; Key: TBytes; CallBack: TCnBLAKE3CalcProgressFunc): TCnBLAKE3Digest;
var
  Context: TCnBLAKE3Context;
begin
  if (Key <> nil) and (Length(Key) > 0) then
    BLAKE3Init(Context, PAnsiChar(@Key[0]), Length(Key))
  else
    BLAKE3Init(Context);
  InternalBLAKE3Stream(Stream, 4096 * 1024, Context, CallBack);
  BLAKE3Final(Context, Result);
end;

function FileSizeIsLargeThanMaxOrCanNotMap3(const AFileName: string; out IsEmpty: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  H: THandle;
  Info: BY_HANDLE_FILE_INFORMATION;
  Rec: Int64Rec;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := False;
  IsEmpty := False;
  H := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  if H = INVALID_HANDLE_VALUE then
    Exit;
  try
    if not GetFileInformationByHandle(H, Info) then
      Exit;
  finally
    CloseHandle(H);
  end;
  Rec.Lo := Info.nFileSizeLow;
  Rec.Hi := Info.nFileSizeHigh;
  Result := (Rec.Hi > 0) or (Rec.Lo > MAX_FILE_SIZE);
  IsEmpty := (Rec.Hi = 0) and (Rec.Lo = 0);
{$ELSE}
  Result := True;
  IsEmpty := False;
{$ENDIF}
end;

function BLAKE3File(const FileName: string; Key: TBytes; CallBack: TCnBLAKE3CalcProgressFunc): TCnBLAKE3Digest;
var
  Context: TCnBLAKE3Context;
  Stream: TStream;
  FileIsZeroSize: Boolean;
{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
begin
  if (Key <> nil) and (Length(Key) > 0) then
    BLAKE3Init(Context, PAnsiChar(@Key[0]), Length(Key))
  else
    BLAKE3Init(Context);
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap3(FileName, FileIsZeroSize) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalBLAKE3Stream(Stream, 4096 * 1024, Context, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
      FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
      FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
        if MapHandle <> 0 then
        begin
          try
            ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
            if ViewPointer <> nil then
            begin
              try
                BLAKE3Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
              finally
                UnmapViewOfFile(ViewPointer);
              end;
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
{$ENDIF}
  end;
  BLAKE3Final(Context, Result);
end;

function BLAKE3DeriveKey(Context: PAnsiChar; ContextLength: Integer;
  KeyMaterial: PAnsiChar; KeyMaterialLength: Integer): TCnBLAKE3DerivedKey;
var
  Ctx: TCnBLAKE3Context;
  TmpDigest: TCnBLAKE3Digest;
begin
  BLAKE3InitDeriveKey(Ctx, Context, ContextLength);
  BLAKE3Update(Ctx, KeyMaterial, KeyMaterialLength);
  BLAKE3Final(Ctx, TmpDigest);
  Move(TmpDigest[0], Result[0], CN_BLAKE3_OUTBYTES);
end;

function BLAKE3DeriveKeyBytes(const Context: TBytes; const KeyMaterial: TBytes): TCnBLAKE3DerivedKey;
var
  CP: PAnsiChar;
  CL: Integer;
  KP: PAnsiChar;
  KL: Integer;
begin
  if (Context = nil) or (Length(Context) = 0) then
  begin
    CP := nil;
    CL := 0;
  end
  else
  begin
    CP := PAnsiChar(@Context[0]);
    CL := Length(Context);
  end;
  if (KeyMaterial = nil) or (Length(KeyMaterial) = 0) then
  begin
    KP := nil;
    KL := 0;
  end
  else
  begin
    KP := PAnsiChar(@KeyMaterial[0]);
    KL := Length(KeyMaterial);
  end;
  Result := BLAKE3DeriveKey(CP, CL, KP, KL);
end;

function BLAKE3DeriveKeyStr(const Context: AnsiString; const KeyMaterial: TBytes): TCnBLAKE3DerivedKey;
var
  KP: PAnsiChar;
  KL: Integer;
begin
  if (KeyMaterial = nil) or (Length(KeyMaterial) = 0) then
  begin
    KP := nil;
    KL := 0;
  end
  else
  begin
    KP := PAnsiChar(@KeyMaterial[0]);
    KL := Length(KeyMaterial);
  end;
  Result := BLAKE3DeriveKey(PAnsiChar(Context), Length(Context), KP, KL);
end;

function BLAKE3XOF(Input: PAnsiChar; ByteLength: Cardinal; OutLength: Integer): TBytes;
var
  Context: TCnBLAKE3Context;
begin
  if OutLength <= 0 then
    raise ECnBLAKE3Exception.Create(SCnErrorBLAKE3InvalidXOFSize);
  BLAKE3Init(Context);
  BLAKE3Update(Context, Input, ByteLength);
  SetLength(Result, OutLength);
  BLAKE3FinalXOF(Context, @Result[0], OutLength);
end;

function BLAKE3XOFBuffer(const Buffer; Count: Cardinal; OutLength: Integer): TBytes;
var
  Context: TCnBLAKE3Context;
begin
  if OutLength <= 0 then
    raise ECnBLAKE3Exception.Create(SCnErrorBLAKE3InvalidXOFSize);
  BLAKE3Init(Context);
  BLAKE3Update(Context, PAnsiChar(@Buffer), Count);
  SetLength(Result, OutLength);
  BLAKE3FinalXOF(Context, @Result[0], OutLength);
end;

function BLAKE3XOFBytes(const Data: TBytes; OutLength: Integer): TBytes;
var
  D: PAnsiChar;
  DL: Cardinal;
begin
  if OutLength <= 0 then
    raise ECnBLAKE3Exception.Create(SCnErrorBLAKE3InvalidXOFSize);
  if (Data = nil) or (Length(Data) = 0) then
  begin
    D := nil;
    DL := 0;
  end
  else
  begin
    D := PAnsiChar(@Data[0]);
    DL := Length(Data);
  end;
  Result := BLAKE3XOF(D, DL, OutLength);
end;

function BLAKE3Print(const Digest: TCnBLAKE3Digest): string;
begin
  Result := DataToHex(@Digest[0], CN_BLAKE3_OUTBYTES, False);
end;

function BLAKE3Match(const D1, D2: TCnBLAKE3Digest): Boolean;
begin
  Result := ConstTimeCompareMem(@D1[0], @D2[0], SizeOf(TCnBLAKE3Digest));
end;

function BLAKE3DigestToStr(const Digest: TCnBLAKE3Digest): string;
begin
  Result := MemoryToString(@Digest[0], CN_BLAKE3_OUTBYTES);
end;

end.
