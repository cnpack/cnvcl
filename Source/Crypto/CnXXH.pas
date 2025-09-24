{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnXXH;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：XXHash 杂凑算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了 XXHash 系列杂凑算法，包括 XXH32/XXH64。
*           Seed 相当于 Key 或者初始化向量，直接影响最终计算结果。
*           注意算法内大量使用 UInt64 乘法，几乎都会溢出，已按 UInt64 截断处理
*           另外，32 位和 64 位结果的输出采用了网络字节顺序，和主机大小端无关
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.09.23 V1.0
*               创建单元。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative, CnConsts;

type
  PCnXXH32Digest = ^TCnXXH32Digest;
  TCnXXH32Digest = array[0..3] of Byte;
  {* XXH32 杂凑结果，32 位 4 字节}

  PCnXXH64Digest = ^TCnXXH64Digest;
  TCnXXH64Digest = array[0..7] of Byte;
  {* XXH64 杂凑结果，64 位 8 字节}

  TCnXXH32Context = packed record
  {* XXH32 的上下文结构}
    TotalLen: TUInt64;
    V1, V2, V3, V4: Cardinal;
    Mem: array[0..15] of Cardinal;
    MemSize: Cardinal;
    Seed: Cardinal;
  end;

  TCnXXH64Context = packed record
  {* XXH64 的上下文结构}
    TotalLen: TUInt64;
    V1, V2, V3, V4: TUInt64;
    Mem: array[0..31] of Byte;
    MemSize: Cardinal;
    Seed: TUInt64;
  end;

  TCnXXHCalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* 各类 XXH 系列杂凑进度回调事件类型声明}

function XXH32(Input: PAnsiChar; ByteLength: Cardinal; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对数据块进行 XXH32 计算。

   参数：
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块字节长度
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH64(Input: PAnsiChar; ByteLength: Cardinal; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对数据块进行 XXH64 计算。

   参数：
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块字节长度
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

function XXH32Buffer(const Buffer; Count: Cardinal; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对数据块进行 XXH32 计算。

   参数：
     const Buffer                         - 待计算的数据块地址
     Count: Cardinal                      - 待计算的数据块字节长度
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                - 返回的 XXH32 杂凑值
}

function XXH64Buffer(const Buffer; Count: Cardinal; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对数据块进行 XXH64 计算。

   参数：
     const Buffer                         - 待计算的数据块地址
     Count: Cardinal                      - 待计算的数据块字节长度
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

function XXH32Bytes(Data: TBytes; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对字节数组进行 XXH32 计算。

   参数：
     Data: TBytes                         - 待计算的字节数组
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH64Bytes(Data: TBytes; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对字节数组进行 XXH64 计算。

   参数：
     Data: TBytes                         - 待计算的字节数组
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

function XXH32String(const Str: string; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对 String 类型数据进行 XXH32 计算，注意 D2009 或以上版本的 string 为 UnicodeString，
   代码中会将其强行转换成 AnsiString 进行计算。

   参数：
     const Str: string                    - 待计算的字符串
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH64String(const Str: string; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对 String 类型数据进行 XXH64 计算，注意 D2009 或以上版本的 string 为 UnicodeString，
   代码中会将其强行转换成 AnsiString 进行计算。

   参数：
     const Str: string                    - 待计算的字符串
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

function XXH32StringA(const Str: AnsiString; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对 AnsiString 类型数据进行 XXH32 计算。

   参数：
     const Str: AnsiString                - 待计算的字符串
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH32StringW(const Str: WideString; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对 WideString 类型数据进行 XXH32 计算。
   计算前 Windows 下会调用 WideCharToMultyByte 转换为 AnsiString 类型，
   其他平台会直接转换为 AnsiString 类型，再进行计算。

   参数：
     const Str: WideString                - 待计算的宽字符串
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH64StringA(const Str: AnsiString; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对 AnsiString 类型数据进行 XXH64 计算。

   参数：
     const Str: AnsiString                - 待计算的字符串
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

function XXH64StringW(const Str: WideString; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对 WideString 类型数据进行 XXH64 计算。
   计算前 Windows 下会调用 WideCharToMultyByte 转换为 AnsiString 类型，
   其他平台会直接转换为 AnsiString 类型，再进行计算。

   参数：
     const Str: WideString                - 待计算的宽字符串
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

{$IFDEF UNICODE}

function XXH32UnicodeString(const Str: string; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对 UnicodeString 类型数据进行直接的 XXH32 计算，直接计算内部 UTF16 内容，不进行转换。

   参数：
     const Str: string                    - 待计算的宽字符串
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH64UnicodeString(const Str: string; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对 UnicodeString 类型数据进行直接的 XXH64 计算，直接计算内部 UTF16 内容，不进行转换。

   参数：
     const Str: string                    - 待计算的宽字符串
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                - 返回的 XXH64 杂凑值
}

{$ELSE}

function XXH32UnicodeString(const Str: WideString; Seed: Cardinal = 0): TCnXXH32Digest;
{* 对 UnicodeString 类型数据进行直接的 XXH32 计算，直接计算内部 UTF16 内容，不进行转换。

   参数：
     const Str: WideString                - 待计算的宽字符串
     Seed: Cardinal                       - 种子值，默认为 0

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH64UnicodeString(const Str: WideString; Seed: TUInt64 = 0): TCnXXH64Digest;
{* 对 UnicodeString 类型数据进行直接的 XXH64 计算，直接计算内部 UTF16 内容，不进行转换。

   参数：
     const Str: WideString                - 待计算的宽字符串
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

{$ENDIF}

function XXH32File(const FileName: string; Seed: Cardinal = 0; CallBack: TCnXXHCalcProgressFunc =
  nil): TCnXXH32Digest;
{* 对指定文件内容进行 XXH32 计算。

   参数：
     const FileName: string               - 待计算的文件名
     Seed: Cardinal                       - 种子值，默认为 0
     CallBack: TCnXXHCalcProgressFunc     - 进度回调函数，默认为空

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH32Stream(Stream: TStream; Seed: Cardinal = 0; CallBack: TCnXXHCalcProgressFunc = nil):
  TCnXXH32Digest;
{* 对指定流数据进行 XXH32 计算。

   参数：
     Stream: TStream                      - 待计算的流内容
     Seed: Cardinal                       - 种子值，默认为 0
     CallBack: TCnXXHCalcProgressFunc     - 进度回调函数，默认为空

   返回值：TCnXXH32Digest                 - 返回的 XXH32 杂凑值
}

function XXH64File(const FileName: string; Seed: TUInt64 = 0; CallBack: TCnXXHCalcProgressFunc =
  nil): TCnXXH64Digest;
{* 对指定文件内容进行 XXH64 计算。

   参数：
     const FileName: string               - 待计算的文件名
     Seed: TUInt64                        - 种子值，默认为 0
     CallBack: TCnXXHCalcProgressFunc     - 进度回调函数，默认为空

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

function XXH64Stream(Stream: TStream; Seed: TUInt64 = 0; CallBack: TCnXXHCalcProgressFunc = nil):
  TCnXXH64Digest;
{* 对指定流数据进行 XXH64 计算。

   参数：
     Stream: TStream                      - 待计算的流内容
     Seed: TUInt64                        - 种子值，默认为 0
     CallBack: TCnXXHCalcProgressFunc     - 进度回调函数，默认为空

   返回值：TCnXXH64Digest                 - 返回的 XXH64 杂凑值
}

// 以下三个函数用于外部持续对数据进行零散的 XXH32 计算，XXH32Update 可多次被调用

procedure XXH32Init(var Context: TCnXXH32Context; Seed: Cardinal = 0);
{* 初始化一轮 XXH32 计算上下文，准备计算 XXH32 结果。

   参数：
     var Context: TCnXXH32Context        - 待初始化的 XXH32 上下文
     Seed: Cardinal                      - 种子值，默认为 0

   返回值：（无）
}

procedure XXH32Update(var Context: TCnXXH32Context; Input: PAnsiChar; ByteLength: Cardinal);
{* 以初始化后的上下文对一块数据进行 XXH32 计算。
   可多次调用以连续计算不同的数据块，无需将不同的数据块拼凑在连续的内存中。

   参数：
     var Context: TCnXXH32Context         - XXH32 上下文
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块的字节长度

   返回值：（无）
}

procedure XXH32Final(var Context: TCnXXH32Context; var Digest: TCnXXH32Digest);
{* 结束本轮计算，将 XXH32 结果返回至 Digest 中。

   参数：
     var Context: TCnXXH32Context        - XXH32 上下文
     var Digest: TCnXXH32Digest          - 返回的 XXH32 杂凑值

   返回值：（无）
}

// 以下三个函数用于外部持续对数据进行零散的 XXH64 计算，XXH64Update 可多次被调用

procedure XXH64Init(var Context: TCnXXH64Context; Seed: TUInt64 = 0);
{* 初始化一轮 XXH64 计算上下文，准备计算 XXH64 结果。

   参数：
     var Context: TCnXXH64Context         - 待初始化的 XXH64 上下文
     Seed: TUInt64                        - 种子值，默认为 0

   返回值：（无）
}

procedure XXH64Update(var Context: TCnXXH64Context; Input: PAnsiChar; ByteLength: Cardinal);
{* 以初始化后的上下文对一块数据进行 XXH64 计算。
   可多次调用以连续计算不同的数据块，无需将不同的数据块拼凑在连续的内存中。

   参数：
     var Context: TCnXXH64Context         - XXH64 上下文
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块的字节长度

   返回值：（无）
}

procedure XXH64Final(var Context: TCnXXH64Context; var Digest: TCnXXH64Digest);
{* 结束本轮计算，将 XXH64 结果返回至 Digest 中。

   参数：
     var Context: TCnXXH64Context        - XXH64 上下文
     var Digest: TCnXXH64Digest          - 返回的 XXH64 杂凑值

   返回值：（无）
}

function XXH32Print(const Digest: TCnXXH32Digest): string;
{* 以十六进制格式输出 XXH32 杂凑值。

   参数：
     const Digest: TCnXXH32Digest         - 指定的 XXH32 杂凑值

   返回值：string                         - 返回十六进制字符串
}

function XXH64Print(const Digest: TCnXXH64Digest): string;
{* 以十六进制格式输出 XXH64 杂凑值。

   参数：
     const Digest: TCnXXH64Digest         - 指定的 XXH64 杂凑值

   返回值：string                         - 返回十六进制字符串
}

function XXH32Match(const D1: TCnXXH32Digest; const D2: TCnXXH32Digest): Boolean;
{* 比较两个 XXH32 杂凑值是否相等。

   参数：
     const D1: TCnXXH32Digest             - 待比较的 XXH32 杂凑值一
     const D2: TCnXXH32Digest             - 待比较的 XXH32 杂凑值二

   返回值：Boolean                        - 返回是否相等
}

function XXH64Match(const D1: TCnXXH64Digest; const D2: TCnXXH64Digest): Boolean;
{* 比较两个 XXH64 杂凑值是否相等。

   参数：
     const D1: TCnXXH64Digest             - 待比较的 XXH64 杂凑值一
     const D2: TCnXXH64Digest             - 待比较的 XXH64 杂凑值二

   返回值：Boolean                        - 返回是否相等
}

function XXH32DigestToStr(const Digest: TCnXXH32Digest): string;
{* XXH32 杂凑值内容直接转 string，每字节对应一字符。

   参数：
     const Digest: TCnXXH32Digest         - 待转换的 XXH32 杂凑值

   返回值：string                         - 返回的字符串
}

function XXH64DigestToStr(const Digest: TCnXXH64Digest): string;
{* XXH64 杂凑值内容直接转 string，每字节对应一字符。

   参数：
     const Digest: TCnXXH64Digest         - 待转换的 XXH64 杂凑值

   返回值：string                         - 返回的字符串
}

implementation

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  CN_XXH32_BLOCK_SIZE  = 16;
  CN_XXH64_BLOCK_SIZE  = 32;
  CN_XXH3_BLOCK_SIZE   = 256;

  CN_XXH32_PRIME32_1 = 2654435761;
  CN_XXH32_PRIME32_2 = 2246822519;
  CN_XXH32_PRIME32_3 = 3266489917;
  CN_XXH32_PRIME32_4 = 668265263;
  CN_XXH32_PRIME32_5 = 374761393;

  CN_XXH64_PRIME64_1: TUInt64 = $9E3779B185EBCA87; // 11400714785074694791;
  CN_XXH64_PRIME64_2: TUInt64 = $C2B2AE3D27D4EB4F; // 14029467366897019727;
  CN_XXH64_PRIME64_3: TUInt64 = $165667B19E3779F9; // 1609587929392839161;
  CN_XXH64_PRIME64_4: TUInt64 = $85EBCA77C2B2AE63; // 9650029242287828579;
  CN_XXH64_PRIME64_5: TUInt64 = $27D4EB2F165667C5; // 2870177450012600261;

  // XXH3 常量
  CN_XXH3_SECRET_DEFAULT_SIZE = 192;
  CN_XXH3_SECRET: array[0..CN_XXH3_SECRET_DEFAULT_SIZE-1] of Byte = (
    $B8, $FE, $6C, $39, $23, $A4, $4B, $BE, $7C, $01, $81, $2C, $F7, $21, $AD, $1C,
    $DE, $D4, $6D, $E9, $83, $9B, $97, $AF, $72, $BC, $8F, $1E, $F5, $D9, $E8, $C4,
    $34, $CF, $4F, $F9, $C0, $5E, $F2, $6A, $9E, $EA, $2B, $7D, $E0, $D8, $94, $FA,
    $BB, $40, $7E, $2E, $A5, $1B, $75, $F0, $3E, $51, $53, $64, $2A, $ED, $8B, $B0,
    $E6, $BD, $7A, $DF, $10, $77, $12, $C8, $4C, $BF, $6B, $58, $60, $D6, $3D, $61,
    $CE, $1D, $E7, $2D, $DC, $B7, $80, $35, $0F, $74, $A8, $31, $D1, $30, $3F, $98,
    $AA, $07, $5F, $F8, $7F, $CD, $7B, $0B, $F6, $EF, $67, $68, $07, $AC, $28, $B1,
    $5B, $93, $B9, $5A, $08, $97, $4E, $0E, $95, $F4, $9F, $1F, $12, $C2, $5D, $E2,
    $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0,
    $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0,
    $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0,
    $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
  );

type
  TCnXXHType = (xtXXH32, xtXXH64);

  TCnXXHGeneralDigest = TCnXXH64Digest; // 最长的

// 辅助函数
function XXH3_avalanche(H64: TUInt64): TUInt64;
begin
  H64 := H64 xor (H64 shr 37);
  H64 := H64 * CN_XXH64_PRIME64_3;
  H64 := H64 xor (H64 shr 32);
  Result := H64;
end;

function XXH3_mul128Fold64(Lo, Hi: TUInt64): TUInt64;
var
  Product: TUInt64;
begin
  Product := Lo * Hi;
  Result := Product xor (Product shr 64);
end;

function XXH3_scrambleAcc(Acc: TUInt64; SecretValue: TUInt64): TUInt64;
var
  XorShifted: TUInt64;
begin
  XorShifted := Acc xor (Acc shr 47);
  XorShifted := XorShifted xor SecretValue;
  Result := XorShifted * CN_XXH64_PRIME64_1;
end;

function RolDWord(Value: Cardinal; Shift: Byte): Cardinal;
begin
  Result := (Value shl Shift) or (Value shr (32 - Shift));
end;

function RolQWord(Value: TUInt64; Shift: Byte): TUInt64;
begin
  Result := (Value shl Shift) or (Value shr (64 - Shift));
end;

function XXH3_mix16B(const Input: Pointer; const Secret: Pointer): TUInt64;
var
  Input64: PUInt64;
  Secret64: PUInt64;
begin
  Input64 := PUInt64(Input);
  Secret64 := PUInt64(Secret);
  
  // 官方 XXH3 算法的 mix16B 实现
  Result := Input64^ xor Secret64^;
  Inc(Input64);
  Inc(Secret64);
  Result := Result * CN_XXH64_PRIME64_1;
  Result := Result + (Input64^ xor Secret64^);
  Result := RolQWord(Result, 31);
  Result := Result * CN_XXH64_PRIME64_2;
end;

procedure XXH32Init(var Context: TCnXXH32Context; Seed: Cardinal);
begin
  FillChar(Context, SizeOf(Context), 0);
  Context.Seed := Seed;
  Context.V1 := Seed + CN_XXH32_PRIME32_1 + CN_XXH32_PRIME32_2;
  Context.V2 := Seed + CN_XXH32_PRIME32_2;
  Context.V3 := Seed + 0;
  Context.V4 := Seed - CN_XXH32_PRIME32_1;
end;

procedure XXH32Update(var Context: TCnXXH32Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  P: PByte;
  Len: Cardinal;
  V1, V2, V3, V4: Cardinal;
  P32: PCardinal;
begin
  if (Input = nil) or (ByteLength = 0) then
    Exit;

  P := PByte(Input);
  Len := ByteLength;
  Context.TotalLen := Context.TotalLen + UInt64(Len);

  // 处理缓冲区中已有的数据
  if Context.MemSize > 0 then
  begin
    if Context.MemSize + Len < CN_XXH32_BLOCK_SIZE then
    begin
      Move(P^, Context.Mem[Context.MemSize], Len);
      Context.MemSize := Context.MemSize + Len;
      Exit;
    end;

    // 填充缓冲区到 16 字节
    Move(P^, Context.Mem[Context.MemSize], CN_XXH32_BLOCK_SIZE - Context.MemSize);
    P32 := @Context.Mem[0];

    // 处理 16 字节块
    Context.V1 := RolDWord(Context.V1 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    Context.V2 := RolDWord(Context.V2 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    Context.V3 := RolDWord(Context.V3 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    Context.V4 := RolDWord(Context.V4 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;

    Inc(P, CN_XXH32_BLOCK_SIZE - Context.MemSize);
    Dec(Len, CN_XXH32_BLOCK_SIZE - Context.MemSize);
    Context.MemSize := 0;
  end;

  // 处理完整的 16 字节块
  V1 := Context.V1;
  V2 := Context.V2;
  V3 := Context.V3;
  V4 := Context.V4;

  while Len >= CN_XXH32_BLOCK_SIZE do
  begin
    P32 := PCardinal(P);
    V1 := RolDWord(V1 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    V2 := RolDWord(V2 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    V3 := RolDWord(V3 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    V4 := RolDWord(V4 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;

    Inc(P, CN_XXH32_BLOCK_SIZE);
    Dec(Len, CN_XXH32_BLOCK_SIZE);
  end;

  Context.V1 := V1;
  Context.V2 := V2;
  Context.V3 := V3;
  Context.V4 := V4;

  // 保存剩余数据到缓冲区
  if Len > 0 then
  begin
    Move(P^, Context.Mem[0], Len);
    Context.MemSize := Len;
  end;
end;

procedure XXH32UpdateW(var Context: TCnXXH32Context; Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // 必须是 UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // 代码页默认用 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    XXH32Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Input);
  A := AnsiString(S);
  XXH32Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure XXH32Final(var Context: TCnXXH32Context; var Digest: TCnXXH32Digest);
var
  Hash: Cardinal;
  P: PByte;
  P32: PCardinal;
begin
  if Context.TotalLen >= CN_XXH32_BLOCK_SIZE then
  begin
    Hash := RolDWord(Context.V1, 1) + RolDWord(Context.V2, 7) + 
      RolDWord(Context.V3, 12) + RolDWord(Context.V4, 18);
  end
  else
  begin
    Hash := Context.Seed + CN_XXH32_PRIME32_5;
  end;

  Hash := Hash + Cardinal(Context.TotalLen);

  // 处理缓冲区中剩余的数据
  P := @Context.Mem[0];
  while Context.MemSize >= 4 do
  begin
    Hash := Hash + PCardinal(P)^ * CN_XXH32_PRIME32_3;
    Hash := RolDWord(Hash, 17) * CN_XXH32_PRIME32_4;
    Inc(P, 4);
    Dec(Context.MemSize, 4);
  end;

  while Context.MemSize > 0 do
  begin
    Hash := Hash + P^ * CN_XXH32_PRIME32_5;
    Hash := RolDWord(Hash, 11) * CN_XXH32_PRIME32_1;
    Inc(P);
    Dec(Context.MemSize);
  end;

  // 最终混合
  Hash := Hash xor (Hash shr 15);
  Hash := Hash * CN_XXH32_PRIME32_2;
  Hash := Hash xor (Hash shr 13);
  Hash := Hash * CN_XXH32_PRIME32_3;
  Hash := Hash xor (Hash shr 16);

  // 将结果存入Digest
  P32 := @Digest[0];
  P32^ := UInt32HostToNetwork(Hash);
end;

procedure XXH64Init(var Context: TCnXXH64Context; Seed: TUInt64);
begin
  FillChar(Context, SizeOf(Context), 0);
  Context.Seed := Seed;
  Context.V1 := Seed + CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_2;
  Context.V2 := Seed + CN_XXH64_PRIME64_2;
  Context.V3 := Seed + 0;
  Context.V4 := Seed - CN_XXH64_PRIME64_1;
end;

procedure XXH64Update(var Context: TCnXXH64Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  P: PByte;
  Len: Cardinal;
  V1, V2, V3, V4: TUInt64;
  P64: PUInt64;
begin
  if (Input = nil) or (ByteLength = 0) then
    Exit;

  P := PByte(Input);
  Len := ByteLength;
  Context.TotalLen := Context.TotalLen + Len;

  // 处理缓冲区中已有的数据
  if Context.MemSize > 0 then
  begin
    if Context.MemSize + Len < CN_XXH64_BLOCK_SIZE then
    begin
      Move(P^, Context.Mem[Context.MemSize], Len);
      Context.MemSize := Context.MemSize + Len;
      Exit;
    end;

    // 填充缓冲区到 32 字节
    Move(P^, Context.Mem[Context.MemSize], CN_XXH64_BLOCK_SIZE - Context.MemSize);
    P64 := @Context.Mem[0];

    // 处理 32 字节块
    Context.V1 := RolQWord(Context.V1 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    Context.V2 := RolQWord(Context.V2 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    Context.V3 := RolQWord(Context.V3 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    Context.V4 := RolQWord(Context.V4 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;

    Inc(P, CN_XXH64_BLOCK_SIZE - Context.MemSize);
    Dec(Len, CN_XXH64_BLOCK_SIZE - Context.MemSize);
    Context.MemSize := 0;
  end;

  // 处理完整的 32 字节块
  V1 := Context.V1;
  V2 := Context.V2;
  V3 := Context.V3;
  V4 := Context.V4;

  while Len >= CN_XXH64_BLOCK_SIZE do
  begin
    P64 := PUInt64(P);
    V1 := RolQWord(V1 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    V2 := RolQWord(V2 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    V3 := RolQWord(V3 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    V4 := RolQWord(V4 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;

    Inc(P, CN_XXH64_BLOCK_SIZE);
    Dec(Len, CN_XXH64_BLOCK_SIZE);
  end;

  Context.V1 := V1;
  Context.V2 := V2;
  Context.V3 := V3;
  Context.V4 := V4;

  // 保存剩余数据到缓冲区
  if Len > 0 then
  begin
    Move(P^, Context.Mem[0], Len);
    Context.MemSize := Len;
  end;
end;

procedure XXH64UpdateW(var Context: TCnXXH64Context; Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // 必须是 UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // 代码页默认用 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    XXH64Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Input);
  A := AnsiString(S);
  XXH64Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure XXH64Final(var Context: TCnXXH64Context; var Digest: TCnXXH64Digest);
var
  Hash, V1, V2, V3, V4: TUInt64;
  P: PByte;
  P64: PUInt64;
begin
  if Context.TotalLen >= CN_XXH64_BLOCK_SIZE then
  begin
    V1 := Context.V1;
    V2 := Context.V2;
    V3 := Context.V3;
    V4 := Context.V4;
    Hash := RolQWord(V1, 1) + RolQWord(V2, 7) + RolQWord(V3, 12) + RolQWord(V4, 18);

    V1 := RolQWord(V1 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V1) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;

    V2 := RolQWord(V2 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V2) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;

    V3 := RolQWord(V3 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V3) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;

    V4 := RolQWord(V4 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V4) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;
  end
  else
    Hash := Context.Seed + CN_XXH64_PRIME64_5;

  Hash := Hash + Context.TotalLen;

  // 处理缓冲区中剩余的数据
  P := @Context.Mem[0];
  while Context.MemSize >= 8 do
  begin
    Hash := Hash xor (RolQWord(PUInt64(P)^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1);
    Hash := RolQWord(Hash, 27) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;
    Inc(P, 8);
    Dec(Context.MemSize, 8);
  end;

  while Context.MemSize >= 4 do
  begin
    Hash := Hash xor (PCardinal(P)^ * CN_XXH64_PRIME64_1);
    Hash := RolQWord(Hash, 23) * CN_XXH64_PRIME64_2 + CN_XXH64_PRIME64_3;
    Inc(P, 4);
    Dec(Context.MemSize, 4);
  end;

  while Context.MemSize > 0 do
  begin
    Hash := Hash xor (P^ * CN_XXH64_PRIME64_5);
    Hash := RolQWord(Hash, 11) * CN_XXH64_PRIME64_1;
    Inc(P);
    Dec(Context.MemSize);
  end;

  // 最终混合
  Hash := Hash xor (Hash shr 33);
  Hash := Hash * CN_XXH64_PRIME64_2;
  Hash := Hash xor (Hash shr 29);
  Hash := Hash * CN_XXH64_PRIME64_3;
  Hash := Hash xor (Hash shr 32);

  // 将结果存入 Digest
  P64 := @Digest[0];
  P64^ := UInt64HostToNetwork(Hash);
end;

// 对数据块进行 XXH32 计算
function XXH32(Input: PAnsiChar; ByteLength: Cardinal; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, Input, ByteLength);
  XXH32Final(Context, Result);
end;

// 对数据块进行 XXH64 计算
function XXH64(Input: PAnsiChar; ByteLength: Cardinal; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, Input, ByteLength);
  XXH64Final(Context, Result);
end;

// 对数据块进行 XXH32 计算
function XXH32Buffer(const Buffer; Count: Cardinal; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(Buffer), Count);
  XXH32Final(Context, Result);
end;

// 对数据块进行 XXH64 计算
function XXH64Buffer(const Buffer; Count: Cardinal; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(Buffer), Count);
  XXH64Final(Context, Result);
end;

// 对字节数组进行 XXH32 计算
function XXH32Bytes(Data: TBytes; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(@Data[0]), Length(Data));
  XXH32Final(Context, Result);
end;

// 对字节数组进行 XXH64 计算
function XXH64Bytes(Data: TBytes; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(@Data[0]), Length(Data));
  XXH64Final(Context, Result);
end;

// 对 String 类型数据进行 XXH32 计算
function XXH32String(const Str: string; Seed: Cardinal): TCnXXH32Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := XXH32StringA(AStr, Seed);
end;

// 对 String 类型数据进行 XXH64 计算
function XXH64String(const Str: string; Seed: TUInt64): TCnXXH64Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := XXH64StringA(AStr, Seed);
end;

// 对 UnicodeString 类型数据进行直接的 XXH32 计算，不进行转换
{$IFDEF UNICODE}
function XXH32UnicodeString(const Str: string; Seed: Cardinal): TCnXXH32Digest;
{$ELSE}
function XXH32UnicodeString(const Str: WideString; Seed: Cardinal): TCnXXH32Digest;
{$ENDIF}
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  XXH32Final(Context, Result);
end;

// 对 UnicodeString 类型数据进行直接的 XXH64 计算，不进行转换
{$IFDEF UNICODE}
function XXH64UnicodeString(const Str: string; Seed: TUInt64): TCnXXH64Digest;
{$ELSE}
function XXH64UnicodeString(const Str: WideString; Seed: TUInt64): TCnXXH64Digest;
{$ENDIF}
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  XXH64Final(Context, Result);
end;

// 对 AnsiString 类型数据进行 XXH32 计算
function XXH32StringA(const Str: AnsiString; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(Str), Length(Str));
  XXH32Final(Context, Result);
end;

// 对 WideString 类型数据进行 XXH32 计算
function XXH32StringW(const Str: WideString; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32UpdateW(Context, PWideChar(Str), Length(Str));
  XXH32Final(Context, Result);
end;

// 对 AnsiString 类型数据进行 XXH64 计算
function XXH64StringA(const Str: AnsiString; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(Str), Length(Str));
  XXH64Final(Context, Result);
end;

// 对 WideString 类型数据进行 XXH64 计算
function XXH64StringW(const Str: WideString; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64UpdateW(Context, PWideChar(Str), Length(Str));
  XXH64Final(Context, Result);
end;

function InternalXXHStream(Stream: TStream; const BufSize: Cardinal; Seed: TUInt64; var D:
  TCnXXHGeneralDigest; XXHType: TCnXXHType; CallBack: TCnXXHCalcProgressFunc): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;

  Context32: TCnXXH32Context;
  Context64: TCnXXH64Context;
//  Context3_64: TCnXXH3_64Context;
//  Context3_128: TCnXXH3_128Context;
  Dig32: TCnXXH32Digest;
  Dig64: TCnXXH64Digest;
//  Dig3_64: TCnXXH3_64Digest;
//  Dig3_128: TCnXXH3_128Digest;

  procedure _XXHInit;
  begin
    case XXHType of
      xtXXH32:
        XXH32Init(Context32);
      xtXXH64:
        XXH64Init(Context64);
//      xtXXH3_64:
//        XXH3_64Init(Context3_64);
//      xtXXH3_128:
//        XXH3_128Init(Context3_128);
    end;
  end;

  procedure _XXHUpdate;
  begin
    case XXHType of
      xtXXH32:
        XXH32Update(Context32, Buf, ReadBytes);
      xtXXH64:
        XXH64Update(Context64, Buf, ReadBytes);
//      xtXXH3_64:
//        XXH3_64Update(Context3_64, Buf, ReadBytes);
//      xtXXH3_128:
//        XXH3_128Update(Context3_128, Buf, ReadBytes);
    end;
  end;

  procedure _XXHFinal;
  begin
    case XXHType of
      xtXXH32:
        XXH32Final(Context32, Dig32);
      xtXXH64:
        XXH64Final(Context64, Dig64);
//      xtXXH3_64:
//        XXH3_64Final(Context3_64, Dig3_64);
//      xtXXH3_128:
//        XXH3_128Final(Context3_128, Dig3_128);
    end;
  end;

  procedure _CopyResult;
  begin
    case XXHType of
      xtXXH32:
        Move(Dig32[0], D[0], SizeOf(TCnXXH32Digest));
      xtXXH64:
        Move(Dig64[0], D[0], SizeOf(TCnXXH64Digest));
//      xtXXH3_64:
//        Move(Dig3_64[0], D[0], SizeOf(TCnXXH3_64Digest));
//      xtXXH3_128:
//        Move(Dig3_128[0], D[0], SizeOf(TCnXXH3_128Digest));
    end;
  end;

begin
  Result := False;
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then
    Exit;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  _XXHInit;
 
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        _XXHUpdate;

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    _XXHFinal;
    _CopyResult;
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// 对指定流进行 XXH32 计算
function XXH32Stream(Stream: TStream; Seed: Cardinal; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH32Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  InternalXXHStream(Stream, 4096 * 1024, Seed, Dig, xtXXH32, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH32Digest));
end;

// 对指定流进行 XXH64 计算
function XXH64Stream(Stream: TStream; Seed: TUInt64; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH64Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  InternalXXHStream(Stream, 4096 * 1024, Seed, Dig, xtXXH64, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH64Digest));
end;

function FileSizeIsLargeThanMaxOrCanNotMap(const AFileName: string; out IsEmpty: Boolean): Boolean;
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
  Result := True; // 非 Windows 平台返回 True，表示不 Mapping
{$ENDIF}
end;

function InternalXXHFile(const FileName: string; Seed: TUInt64; XXHType: TCnXXHType;
  CallBack: TCnXXHCalcProgressFunc): TCnXXHGeneralDigest;
var
  Context32: TCnXXH32Context;
  Context64: TCnXXH64Context;
//  Context3_64: TCnXXH3_64Context;
//  Context3_128: TCnXXH3_128Context;
  Dig32: TCnXXH32Digest;
  Dig64: TCnXXH64Digest;
//  Dig3_64: TCnXXH3_64Digest;
//  Dig3_128: TCnXXH3_128Digest;

{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  procedure _XXHInit;
  begin
    case XXHType of
      xtXXH32:
        XXH32Init(Context32);
      xtXXH64:
        XXH64Init(Context64);
//      xtXXH3_64:
//        XXH3_64Init(Context3_64);
//      xtXXH3_128:
//        XXH3_128Init(Context3_128);
    end;
  end;

{$IFDEF MSWINDOWS}
  procedure _XXHUpdate;
  begin
    case XXHType of
      xtXXH32:
        XXH32Update(Context32, ViewPointer, GetFileSize(FileHandle, nil));
      xtXXH64:
        XXH64Update(Context64, ViewPointer, GetFileSize(FileHandle, nil));
//      xtXXH3_64:
//        XXH3_64Update(Context3_64, ViewPointer, GetFileSize(FileHandle, nil));
//      xtXXH3_128:
//        XXH3_128Update(Context3_128, ViewPointer, GetFileSize(FileHandle, nil));
    end;
  end;
{$ENDIF}

  procedure _XXHFinal;
  begin
    case XXHType of
      xtXXH32:
        XXH32Final(Context32, Dig32);
      xtXXH64:
        XXH64Final(Context64, Dig64);
//      xtXXH3_64:
//        XXH3_64Final(Context3_64, Dig3_64);
//      xtXXH3_128:
//        XXH3_128Final(Context3_128, Dig3_128);
    end;
  end;

  procedure _CopyResult(var D: TCnXXHGeneralDigest);
  begin
    case XXHType of
      xtXXH32:
        Move(Dig32[0], D[0], SizeOf(TCnXXH32Digest));
      xtXXH64:
        Move(Dig64[0], D[0], SizeOf(TCnXXH64Digest));
//      xtXXH3_64:
//        Move(Dig3_64[0], D[0], SizeOf(TCnXXH3_64Digest));
//      xtXXH3_128:
//        Move(Dig3_128[0], D[0], SizeOf(TCnXXH3_128Digest));
    end;
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // 大于 2G 的文件可能 Map 失败，或非 Windows 平台，采用流方式循环处理
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalXXHStream(Stream, 4096 * 1024, Seed, Result, XXHType, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    _XXHInit;
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
                _XXHUpdate;
              finally
                UnmapViewOfFile(ViewPointer);
              end;
            end
            else
            begin
              raise Exception.Create(SCnErrorMapViewOfFile + IntToStr(GetLastError));
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end
        else
        begin
          if not FileIsZeroSize then
            raise Exception.Create(SCnErrorCreateFileMapping + IntToStr(GetLastError));
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
    _XXHFinal;
    _CopyResult(Result);
{$ENDIF}
  end;
end;

// 对指定文件数据进行 XXH32 计算
function XXH32File(const FileName: string; Seed: Cardinal; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH32Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  Dig := InternalXXHFile(FileName, Seed, xtXXH32, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH32Digest));
end;

// 对指定文件数据进行 XXH64 计算
function XXH64File(const FileName: string; Seed: TUInt64; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH64Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  Dig := InternalXXHFile(FileName, Seed, xtXXH64, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH64Digest));
end;

// 以十六进制格式输出 XXH32 杂凑值
function XXH32Print(const Digest: TCnXXH32Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnXXH32Digest));
end;

// 以十六进制格式输出 XXH64 杂凑值
function XXH64Print(const Digest: TCnXXH64Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnXXH64Digest));
end;

// 比较两个 XXH32 杂凑值是否相等
function XXH32Match(const D1, D2: TCnXXH32Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnXXH32Digest));
end;

// 比较两个 XXH64 杂凑值是否相等
function XXH64Match(const D1, D2: TCnXXH64Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnXXH64Digest));
end;

// XXH32 杂凑值转 string
function XXH32DigestToStr(const Digest: TCnXXH32Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnXXH32Digest));
end;

// XXH64 杂凑值转 string
function XXH64DigestToStr(const Digest: TCnXXH64Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnXXH64Digest));
end;

end.
