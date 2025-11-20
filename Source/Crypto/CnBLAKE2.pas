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

unit CnBLAKE2;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：BLAKE 杂凑算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
*           参考 https://github.com/BLAKE2/BLAKE2 的 C 代码移植，补充部分功能
* 备    注：本单元参考 RFC 7693 实现了 BLAKE2 系列杂凑算法 2S/2B。
*           注意，因为 BLAKE2 内部设计了 Key 值，因而无需额外的 HMAC 实现。
* 开发平台：PWin7 + Delphi 7.0
* 兼容测试：PWinXP/7/10/11 + Delphi 5/6/7 ~ D12
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.06.15 V1.0
*               创建单元。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} CnNative, CnConsts;

const
  CN_BLAKE2S_BLOCKBYTES    = 64;
  {* BLAKE2S 块大小，64 字节}
  CN_BLAKE2S_OUTBYTES      = 32;
  {* BLAKE2S 杂凑结果长度，32 字节}
  CN_BLAKE2S_KEYBYTES      = 32;
  {* BLAKE2S 密码长度，32 字节}
  CN_BLAKE2S_SALTBYTES     = 8;
  {* BLAKE2S 盐长度，8 字节}
  CN_BLAKE2S_PERSONALBYTES = 8;
  {* BLAKE2S 内部长度，8 字节}

  CN_BLAKE2B_BLOCKBYTES    = 128;
  {* BLAKE2S 块大小，128 字节}
  CN_BLAKE2B_OUTBYTES      = 64;
  {* BLAKE2B 杂凑结果大小，64 字节}
  CN_BLAKE2B_KEYBYTES      = 64;
  {* BLAKE2B 密码长度，64 字节}
  CN_BLAKE2B_SALTBYTES     = 16;
  {* BLAKE2B 盐长度，16 字节}
  CN_BLAKE2B_PERSONALBYTES = 16;
  {* BLAKE2B 内部长度，16 字节}

type
  ECnBLAKE2Exception = class(Exception);
  {* BLAKE2 相关异常}

  PCnBLAKE2SDigest = ^TCnBLAKE2SDigest;
  {* BLAKE2S 杂凑结果指针}
  TCnBLAKE2SDigest = array[0..CN_BLAKE2S_OUTBYTES - 1] of Byte;
  {* BLAKE2S 杂凑结果，最长 32 字节}

  PCnBLAKE2BDigest = ^TCnBLAKE2BDigest;
  {* BLAKE2B 杂凑结果指针}
  TCnBLAKE2BDigest = array[0..CN_BLAKE2B_OUTBYTES - 1] of Byte;
  {* BLAKE2B 杂凑结果，最长 64 字节}

  TCnBLAKE2SContext = packed record
  {* BLAKE2S 的上下文结构}
    H: array[0..7] of Cardinal;
    T: array[0..1] of Cardinal;
    F: array[0..1] of Cardinal;
    Buf: array[0..CN_BLAKE2S_BLOCKBYTES - 1] of Byte;
    BufLen: Integer;
    OutLen: Integer;
    Last: Byte;
  end;

  TCnBLAKE2BContext = packed record
  {* BLAKE2B 的上下文结构}
    H: array[0..7] of TUInt64;
    T: array[0..1] of TUInt64;
    F: array[0..1] of TUInt64;
    Buf: array[0..CN_BLAKE2B_BLOCKBYTES - 1] of Byte;
    BufLen: Integer;
    OutLen: Integer;
    Last: Byte;
  end;

  TCnBLAKE2CalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* 各类 BLAKE2 系列杂凑进度回调事件类型声明}

function BLAKE2S(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar = nil;
  KeyLength: Integer = 0; DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对数据块进行 BLAKE2S 计算。注意当 Key 存在时长度将截断或补 #0 为 32 字节。

   参数：
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块字节长度
     Key: PAnsiChar                       - BLAKE2S 密钥，默认为空
     KeyLength: Integer                   - BLAKE2S 密钥字节长度，默认为 0
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest               - 返回的 BLAKE2S 杂凑值
}

function BLAKE2B(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar = nil;
  KeyLength: Integer = 0; DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对数据块进行 BLAKE2B 计算。注意当 Key 存在时长度将截断或补 #0 为 64 字节。

   参数：
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块字节长度
     Key: PAnsiChar                       - BLAKE2B 密钥，默认为空
     KeyLength: Integer                   - BLAKE2B 密钥字节长度，默认为 0
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

function BLAKE2SBuffer(const Buffer; Count: Cardinal; const Key; KeyCount: Cardinal;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对数据块进行 BLAKE2S 计算。注意当 Key 存在时长度将截断或补 #0 为 32 字节。

   参数：
     const Buffer                         - 待计算的数据块地址
     Count: Cardinal                      - 待计算的数据块字节长度
     const Key                            - BLAKE2S 密钥地址
     KeyCount: Cardinal                   - BLAKE2S 密钥字节长度
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest              - 返回的 BLAKE2S 杂凑值
}

function BLAKE2BBuffer(const Buffer; Count: Cardinal; const Key; KeyCount: Cardinal;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对数据块进行 BLAKE2B 计算。注意当 Key 存在时长度将截断或补 #0 为 64 字节。

   参数：
     const Buffer                         - 待计算的数据块地址
     Count: Cardinal                      - 待计算的数据块字节长度
     const Key                            - BLAKE2B 密钥地址
     KeyCount: Cardinal                   - BLAKE2B 密钥字节长度
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

function BLAKE2SBytes(const Data: TBytes; const Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对字节数组进行 BLAKE2S 计算。注意当 Key 存在时长度将截断或补 #0 为 32 字节。

   参数：
     const Data: TBytes                   - 待计算的字节数组
     const Key: TBytes                    - BLAKE2S 密钥字节数组，默认为空
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest               - 返回的 BLAKE2S 杂凑值
}

function BLAKE2BBytes(const Data: TBytes; const Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对字节数组进行 BLAKE2B 计算。注意当 Key 存在时长度将截断或补 #0 为 64 字节。

   参数：
     const Data: TBytes                   - 待计算的字节数组
     const Key: TBytes                    - BLAKE2B 密钥字节数组，默认为空
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

function BLAKE2SString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对 String 类型数据进行 BLAKE2S 计算，注意 D2009 或以上版本的 string 为 UnicodeString，
   代码中会将其强行转换成 AnsiString 进行计算。注意当 Key 非空时长度将截断或补 #0 为 32 字节。

   参数：
     const Str: string                    - 待计算的字符串
     const Key: string                    - BLAKE2S 密钥的字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest               - 返回的 BLAKE2S 杂凑值
}

function BLAKE2BString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对 String 类型数据进行 BLAKE2B 计算，注意 D2009 或以上版本的 string 为 UnicodeString，
   代码中会将其强行转换成 AnsiString 进行计算。注意当 Key 非空时长度将截断或补 #0 为 64 字节。

   参数：
     const Str: string                    - 待计算的字符串
     const Key: string                    - BLAKE2B 密钥的字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

function BLAKE2SStringA(const Str: AnsiString; const Key: AnsiString = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对 AnsiString 类型数据进行 BLAKE2S 计算。注意当 Key 非空时长度将截断或补 #0 为 32 字节。

   参数：
     const Str: AnsiString                - 待计算的字符串
     const Key: AnsiString                - BLAKE2S 密钥的字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest               - 返回的 BLAKE2S 杂凑值
}

function BLAKE2SStringW(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对 WideString 类型数据进行 BLAKE2S 计算。
   计算前 Windows 下会调用 WideCharToMultyByte 转换为 AnsiString 类型，
   其他平台会直接转换为 AnsiString 类型，再进行计算。
   注意当 Key 非空时转换后的长度将截断或补 #0 为 32 字节。

   参数：
     const Str: WideString                - 待计算的宽字符串
     const Key: WideString                - BLAKE2S 密钥的宽字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest               - 返回的 BLAKE2S 杂凑值
}

function BLAKE2BStringA(const Str: AnsiString; const Key: AnsiString = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对 AnsiString 类型数据进行 BLAKE2B 计算。注意当 Key 非空时长度将截断或补 #0 为 64 字节。

   参数：
     const Str: AnsiString                - 待计算的字符串
     const Key: AnsiString                - BLAKE2B 密钥的字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

function BLAKE2BStringW(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对 WideString 类型数据进行 BLAKE2B 计算。
   计算前 Windows 下会调用 WideCharToMultyByte 转换为 AnsiString 类型，
   其他平台会直接转换为 AnsiString 类型，再进行计算。
   注意当 Key 非空时转换后的长度将截断或补 #0 为 64 字节。

   参数：
     const Str: WideString                - 待计算的宽字符串
     const Key: WideString                - BLAKE2B 密钥的宽字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

{$IFDEF UNICODE}

function BLAKE2SUnicodeString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对 UnicodeString 类型数据进行直接的 BLAKE2S 计算，直接计算内部 UTF16 内容，不进行转换。
   注意当 Key 非空时长度将截断或补 #0 为 32 字节。

   参数：
     const Str: string                    - 待计算的宽字符串
     const Key: string                    - BLAKE2S 密钥的宽字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest               - 返回的 BLAKE2S 杂凑值
}

function BLAKE2BUnicodeString(const Str: string; const Key: string = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对 UnicodeString 类型数据进行直接的 BLAKE2S 计算，直接计算内部 UTF16 内容，不进行转换。
   注意当 Key 非空时长度将截断或补 #0 为 64 字节。

   参数：
     const Str: string                    - 待计算的宽字符串
     const Key: string                    - BLAKE2B 密钥的宽字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

{$ELSE}

function BLAKE2SUnicodeString(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): TCnBLAKE2SDigest;
{* 对 UnicodeString 类型数据进行直接的 BLAKE2S 计算，直接计算内部 UTF16 内容，不进行转换。
   注意当 Key 非空时长度将截断或补 #0 为 32 字节。

   参数：
     const Str: WideString                - 待计算的宽字符串
     const Key: WideString                - BLAKE2S 密钥的宽字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：TCnBLAKE2SDigest               - 返回的 BLAKE2S 杂凑值
}

function BLAKE2BUnicodeString(const Str: WideString; const Key: WideString = '';
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): TCnBLAKE2BDigest;
{* 对 UnicodeString 类型数据进行直接的 BLAKE2S 计算，直接计算内部 UTF16 内容，不进行转换。
   注意当 Key 非空时长度将截断或补 #0 为 64 字节。

   参数：
     const Str: WideString                - 待计算的宽字符串
     const Key: WideString                - BLAKE2B 密钥的宽字符串形式
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：TCnBLAKE2BDigest               - 返回的 BLAKE2B 杂凑值
}

{$ENDIF}

function BLAKE2SFile(const FileName: string; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc =
  nil): TCnBLAKE2SDigest;
{* 对指定文件内容进行 BLAKE2S 计算。

   参数：
     const FileName: string                - 待计算的文件名
     Key: TBytes                           - BLAKE2S 密钥字节数组，默认为空
     DigestLength: Integer                 - 指定输出的摘要字节长度，默认 32
     CallBack: TCnBLAKE2CalcProgressFunc   - 进度回调函数，默认为空

   返回值：TCnBLAKE2SDigest                - 返回的 BLAKE2S 杂凑值
}

function BLAKE2SStream(Stream: TStream; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc = nil):
  TCnBLAKE2SDigest;
{* 对指定流数据进行 BLAKE2S 计算。

   参数：
     Stream: TStream                       - 待计算的流内容
     Key: TBytes                           - BLAKE2S 密钥字节数组，默认为空
     DigestLength: Integer                 - 指定输出的摘要字节长度，默认 32
     CallBack: TCnBLAKE2CalcProgressFunc   - 进度回调函数，默认为空

   返回值：TCnBLAKE2SDigest                - 返回的 BLAKE2S 杂凑值
}

function BLAKE2BFile(const FileName: string; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc =
  nil): TCnBLAKE2BDigest;
{* 对指定文件内容进行 BLAKE2B 计算。

   参数：
     const FileName: string                - 待计算的文件名
     Key: TBytes                           - BLAKE2B 密钥字节数组，默认为空
     DigestLength: Integer                 - 指定输出的摘要字节长度，默认 32
     CallBack: TCnBLAKE2CalcProgressFunc   - 进度回调函数，默认为空

   返回值：TCnBLAKE2BDigest                - 返回的 BLAKE2B 杂凑值
}

function BLAKE2BStream(Stream: TStream; Key: TBytes = nil;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES; CallBack: TCnBLAKE2CalcProgressFunc = nil):
  TCnBLAKE2BDigest;
{* 对指定流数据进行 BLAKE2B 计算。

   参数：
     Stream: TStream                       - 待计算的流内容
     Key: TBytes                           - BLAKE2B 密钥字节数组，默认为空
     DigestLength: Integer                 - 指定输出的摘要字节长度，默认 32
     CallBack: TCnBLAKE2CalcProgressFunc   - 进度回调函数，默认为空

   返回值：TCnBLAKE2BDigest                - 返回的 BLAKE2B 杂凑值
}

// 以下三类函数用于外部持续对数据进行零散的 BLAKE2S 计算，BLAKE2SUpdate 可多次被调用

procedure BLAKE2SInit(var Context: TCnBLAKE2SContext; Key: PAnsiChar = nil; KeyLength: Integer = 0;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES);
{* 初始化一轮 BLAKE2S 计算上下文，准备计算 BLAKE2S 结果。注意当 Key 存在时长度将截断或补 #0 为 32 字节。

   参数：
     var Context: TCnBLAKE2SContext       - 待初始化的 BLAKE2S 上下文
     Key: PAnsiChar                       - BLAKE2S 密钥，默认为空
     KeyLength: Integer                   - BLAKE2S 密钥字节长度，默认为 0
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 32

   返回值：（无）
}

procedure BLAKE2SUpdate(var Context: TCnBLAKE2SContext; Input: PAnsiChar; ByteLength: Cardinal);
{* 以初始化后的上下文对一块数据进行 BLAKE2S 计算。
   可多次调用以连续计算不同的数据块，无需将不同的数据块拼凑在连续的内存中。

   参数：
     var Context: TCnBLAKE2SContext       - BLAKE2S 上下文
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块的字节长度

   返回值：（无）
}

procedure BLAKE2SFinal(var Context: TCnBLAKE2SContext; var Digest: TCnBLAKE2SDigest);
{* 结束本轮计算，将 BLAKE2S 结果返回至 Digest 中。

   参数：
     var Context: TCnBLAKE2SContext       - BLAKE2S 上下文
     var Digest: TCnBLAKE2SDigest         - 返回的 BLAKE2S 杂凑值，注意可能只有前 OutLength 个字节有效

   返回值：（无）
}

// 以下三个函数用于外部持续对数据进行零散的 BLAKE2B 计算，BLAKE2BUpdate 可多次被调用

procedure BLAKE2BInit(var Context: TCnBLAKE2BContext; Key: PAnsiChar = nil; KeyLength: Integer = 0;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES);
{* 初始化一轮 BLAKE2B 计算上下文，准备计算 BLAKE2B 结果。注意当 Key 存在时长度将截断或补 #0 为 64 字节。

   参数：
     var Context: TCnBLAKE2BContext       - 待初始化的 BLAKE2B 上下文
     Key: PAnsiChar                       - BLAKE2B 密钥，默认为空
     KeyLength: Integer                   - BLAKE2B 密钥字节长度，默认为 0
     DigestLength: Integer                - 指定输出的摘要字节长度，默认 64

   返回值：（无）
}

procedure BLAKE2BUpdate(var Context: TCnBLAKE2BContext; Input: PAnsiChar; ByteLength: Cardinal);
{* 以初始化后的上下文对一块数据进行 BLAKE2B 计算。
   可多次调用以连续计算不同的数据块，无需将不同的数据块拼凑在连续的内存中。

   参数：
     var Context: TCnBLAKE2BContext       - BLAKE2B 上下文
     Input: PAnsiChar                     - 待计算的数据块地址
     ByteLength: Cardinal                 - 待计算的数据块的字节长度

   返回值：（无）
}

procedure BLAKE2BFinal(var Context: TCnBLAKE2BContext; var Digest: TCnBLAKE2BDigest);
{* 结束本轮计算，将 BLAKE2B 结果返回至 Digest 中。

   参数：
     var Context: TCnBLAKE2BContext       - BLAKE2B 上下文
     var Digest: TCnBLAKE2BDigest         - 返回的 BLAKE2B 杂凑值，注意可能只有前 OutLength 个字节有效

   返回值：（无）
}

function BLAKE2SPrint(const Digest: TCnBLAKE2SDigest;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): string;
{* 以十六进制格式输出 BLAKE2S 杂凑值。

   参数：
     const Digest: TCnBLAKE2SDigest       - 指定的 BLAKE2S 杂凑值
     DigestLength: Integer                - 杂凑结果字节长度，默认 32

   返回值：string                         - 返回十六进制字符串
}

function BLAKE2BPrint(const Digest: TCnBLAKE2BDigest;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): string;
{* 以十六进制格式输出 BLAKE2B 杂凑值。

   参数：
     const Digest: TCnBLAKE2BDigest       - 指定的 BLAKE2B 杂凑值
     DigestLength: Integer                - 杂凑结果字节长度，默认 64

   返回值：string                         - 返回十六进制字符串
}

function BLAKE2SMatch(const D1, D2: TCnBLAKE2SDigest;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): Boolean;
{* 比较两个 BLAKE2S 杂凑值是否相等。

   参数：
     const D1: TCnBLAKE2SDigest           - 待比较的 BLAKE2S 杂凑值一
     const D2: TCnBLAKE2SDigest           - 待比较的 BLAKE2S 杂凑值二
     DigestLength: Integer                - 杂凑结果字节长度，默认 32

   返回值：Boolean                        - 返回是否相等
}

function BLAKE2BMatch(const D1, D2: TCnBLAKE2BDigest;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): Boolean;
{* 比较两个 BLAKE2B 杂凑值是否相等。

   参数：
     const D1: TCnBLAKE2BDigest           - 待比较的 BLAKE2B 杂凑值一
     const D2: TCnBLAKE2BDigest           - 待比较的 BLAKE2B 杂凑值二
     DigestLength: Integer                - 杂凑结果字节长度，默认 64

   返回值：Boolean                        - 返回是否相等
}

function BLAKE2SDigestToStr(const Digest: TCnBLAKE2SDigest;
  DigestLength: Integer = CN_BLAKE2S_OUTBYTES): string;
{* BLAKE2S 杂凑值内容直接转 string，每字节对应一字符。

   参数：
     const Digest: TCnBLAKE2SDigest       - 待转换的 BLAKE2S 杂凑值
     DigestLength: Integer                - 杂凑结果字节长度，默认 32

   返回值：string                         - 返回的字符串
}

function BLAKE2BDigestToStr(const Digest: TCnBLAKE2BDigest;
  DigestLength: Integer = CN_BLAKE2B_OUTBYTES): string;
{* BLAKE2B 杂凑值内容直接转 string，每字节对应一字符。

   参数：
     const Digest: TCnBLAKE2BDigest       - 待转换的 BLAKE2B 杂凑值
     DigestLength: Integer                - 杂凑结果字节长度，默认 64

   返回值：string                         - 返回的字符串
}

implementation

resourcestring
  SCnErrorBlake2InvalidKeySize = 'Invalid Key Length';
  SCnErrorBlake2InvalidDigestSize = 'Invalid Digest Length';

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  BLAKE2S_IV: array[0..7] of Cardinal = (
    $6A09E667, $BB67AE85, $3C6EF372, $A54FF53A,
    $510E527F, $9B05688C, $1F83D9AB, $5BE0CD19
  );

  BLAKE2B_IV: array[0..7] of TUInt64 = (
    $6A09E667F3BCC908, $BB67AE8584CAA73B,
    $3C6EF372FE94F82B, $A54FF53A5F1D36F1,
    $510E527FADE682D1, $9B05688C2B3E6C1F,
    $1F83D9ABFB41BD6B, $5BE0CD19137E2179
  );

  BLAKE2S_SIGMA: array[0..9, 0..15] of Byte = (
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
    ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
    (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
    (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
    (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
    ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
    ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
    (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
    ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 )
  );

  BLAKE2B_SIGMA: array[0..11, 0..15] of Byte = (
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 ),
    ( 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 ),
    (  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 ),
    (  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 ),
    (  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 ),
    ( 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 ),
    ( 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 ),
    (  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 ),
    ( 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 ),
    (  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 ),
    ( 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 )
  );

type
  TCnBLAKE2SParam = packed record
    DigestLength: Byte;
    KeyLength: Byte;
    FanOut: Byte;
    Depth: Byte;
    LeafLength: Cardinal;
    NodeOffset: Cardinal;
    XofLength: Word;
    NodeDepth: Byte;
    InnerLength: Byte;
    Salt: array[0..CN_BLAKE2S_SALTBYTES - 1] of Byte;
    Personal: array[0..CN_BLAKE2S_PERSONALBYTES - 1] of Byte;
  end;

  TCnBLAKE2BParam = packed record
    DigestLength: Byte;
    KeyLength: Byte;
    FanOut: Byte;
    Depth: Byte;
    LeafLength: Cardinal;
    NodeOffset: Cardinal;
    XofLength: Cardinal;
    NodeDepth: Byte;
    InnerLength: Byte;
    Reserved: array[0..13] of Byte; // 对齐 32 用
    Salt: array[0..CN_BLAKE2B_SALTBYTES - 1] of Byte;
    Personal: array[0..CN_BLAKE2B_PERSONALBYTES - 1] of Byte;
  end;

  PCnBLAKE2GeneralDigest = ^TCnBLAKE2GeneralDigest;
  TCnBLAKE2GeneralDigest = array[0..CN_BLAKE2B_OUTBYTES - 1] of Byte;

  TBLAKE2Type = (btBLAKE2B, btBLAKE2S);

function ROTRight256(A, B: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A shr B) or (A shl (32 - B));
end;

function ROTRight512(X: TUInt64; Y: Integer): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X shr Y) or (X shl (64 - Y));
end;

procedure GS(MPtr: Pointer; R, I: Integer; var A, B, C, D: Cardinal);
var
  M: PCnUInt32Array;
begin
  M := PCnUInt32Array(MPtr);

  A := A + B + M[BLAKE2S_SIGMA[R][2 * I]];
  D := ROTRight256(D xor A, 16);
  C := C + D;
  B := ROTRight256(B xor C, 12);
  A := A + B + M[BLAKE2S_SIGMA[R][2 * I + 1]];
  D := ROTRight256(D xor A, 8);
  C := C + D;
  B := ROTRight256(B xor C, 7);
end;

procedure RoundS(MPtr, VPtr: Pointer; R: Integer);
var
  V: PCnUInt32Array;
begin
  V := PCnUInt32Array(VPtr);

  GS(MPtr, R, 0, V^[0], V^[4], V^[8], V^[12]);
  GS(MPtr, R, 1, V^[1], V^[5], V^[9], V^[13]);
  GS(MPtr, R, 2, V^[2], V^[6], V^[10], V^[14]);
  GS(MPtr, R, 3, V^[3], V^[7], V^[11], V^[15]);
  GS(MPtr, R, 4, V^[0], V^[5], V^[10], V^[15]);
  GS(MPtr, R, 5, V^[1], V^[6], V^[11], V^[12]);
  GS(MPtr, R, 6, V^[2], V^[7], V^[8], V^[13]);
  GS(MPtr, R, 7, V^[3], V^[4], V^[9], V^[14]);
end;

procedure GB(MPtr: Pointer; R, I: Integer; var A, B, C, D: TUInt64);
var
  M: PCnUInt64Array;
begin
  M := PCnUInt64Array(MPtr);

  A := A + B + M[BLAKE2B_SIGMA[R][2 * I]];
  D := ROTRight512(D xor A, 32);
  C := C + D;
  B := ROTRight512(B xor C, 24);
  A := A + B + M[BLAKE2B_SIGMA[R][2 * I + 1]];
  D := ROTRight512(D xor A, 16);
  C := C + D;
  B := ROTRight512(B xor C, 63);
end;

procedure RoundB(MPtr, VPtr: Pointer; R: Integer);
var
  V: PCnUInt64Array;
begin
  V := PCnUInt64Array(VPtr);

  GB(MPtr, R, 0, V^[0], V^[4], V^[8], V^[12]);
  GB(MPtr, R, 1, V^[1], V^[5], V^[9], V^[13]);
  GB(MPtr, R, 2, V^[2], V^[6], V^[10], V^[14]);
  GB(MPtr, R, 3, V^[3], V^[7], V^[11], V^[15]);
  GB(MPtr, R, 4, V^[0], V^[5], V^[10], V^[15]);
  GB(MPtr, R, 5, V^[1], V^[6], V^[11], V^[12]);
  GB(MPtr, R, 6, V^[2], V^[7], V^[8], V^[13]);
  GB(MPtr, R, 7, V^[3], V^[4], V^[9], V^[14]);
end;

procedure BLAKE2SCompress(var Context: TCnBLAKE2SContext; InPtr: Pointer);
var
  I: Integer;
  C: PCardinal;
  M, V: array[0..15] of Cardinal;
begin
  for I := 0 to 15 do
  begin
    C := PCardinal(TCnIntAddress(InPtr) + I * SizeOf(Cardinal));
    M[I] := UInt32ToLittleEndian(C^);
  end;

  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := BLAKE2S_IV[0];
  V[ 9] := BLAKE2S_IV[1];
  V[10] := BLAKE2S_IV[2];
  V[11] := BLAKE2S_IV[3];
  V[12] := Context.T[0] xor BLAKE2S_IV[4];
  V[13] := Context.T[1] xor BLAKE2S_IV[5];
  V[14] := Context.F[0] xor BLAKE2S_IV[6];
  V[15] := Context.F[1] xor BLAKE2S_IV[7];

  RoundS(@M[0], @V[0], 0);
  RoundS(@M[0], @V[0], 1);
  RoundS(@M[0], @V[0], 2);
  RoundS(@M[0], @V[0], 3);
  RoundS(@M[0], @V[0], 4);
  RoundS(@M[0], @V[0], 5);
  RoundS(@M[0], @V[0], 6);
  RoundS(@M[0], @V[0], 7);
  RoundS(@M[0], @V[0], 8);
  RoundS(@M[0], @V[0], 9);

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor V[I] xor V[I + 8];
end;

procedure BLAKE2BCompress(var Context: TCnBLAKE2BContext; InPtr: Pointer);
var
  I: Integer;
  C: PUInt64;
  M, V: array[0..15] of TUInt64;
begin
  for I := 0 to 15 do
  begin
    C := PUInt64(TCnIntAddress(InPtr) + I * SizeOf(TUInt64));
    M[I] := UInt64ToLittleEndian(C^);
  end;

  for I := 0 to 7 do
    V[I] := Context.H[I];

  V[ 8] := BLAKE2B_IV[0];
  V[ 9] := BLAKE2B_IV[1];
  V[10] := BLAKE2B_IV[2];
  V[11] := BLAKE2B_IV[3];
  V[12] := Context.T[0] xor BLAKE2B_IV[4];
  V[13] := Context.T[1] xor BLAKE2B_IV[5];
  V[14] := Context.F[0] xor BLAKE2B_IV[6];
  V[15] := Context.F[1] xor BLAKE2B_IV[7];

  RoundB(@M[0], @V[0], 0);
  RoundB(@M[0], @V[0], 1);
  RoundB(@M[0], @V[0], 2);
  RoundB(@M[0], @V[0], 3);
  RoundB(@M[0], @V[0], 4);
  RoundB(@M[0], @V[0], 5);
  RoundB(@M[0], @V[0], 6);
  RoundB(@M[0], @V[0], 7);
  RoundB(@M[0], @V[0], 8);
  RoundB(@M[0], @V[0], 9);
  RoundB(@M[0], @V[0], 10);
  RoundB(@M[0], @V[0], 11);

  for I := 0 to 7 do
    Context.H[I] := Context.H[I] xor V[I] xor V[I + 8];
end;

procedure IncBLAKE2SCounter(var Context: TCnBLAKE2SContext; Step: Integer);
begin
  Context.T[0] := Context.T[0] + Cardinal(Step);
  if Context.T[0] < Cardinal(Step) then
    Inc(Context.T[1]);
end;

procedure IncBLAKE2BCounter(var Context: TCnBLAKE2BContext; Step: Integer);
begin
  Context.T[0] := Context.T[0] + Cardinal(Step);
  if Context.T[0] < TUInt64(Step) then
    Inc(Context.T[1]);
end;

// 以下三类函数用于外部持续对数据进行零散的 BLAKE2S 计算，BLAKE2SUpdate 可多次被调用

procedure BLAKE2SInit(var Context: TCnBLAKE2SContext; Key: PAnsiChar;
  KeyLength, DigestLength: Integer);
var
  I: Integer;
  B2SP: TCnBLAKE2SParam;
  P: PCnUInt32Array;
  B: array[0..CN_BLAKE2S_BLOCKBYTES - 1] of Byte;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  // 初始化结构
  FillChar(Context, SizeOf(TCnBLAKE2SContext), 0);
  Context.OutLen := DigestLength;

  // 初始化参数区
  FillChar(B2SP, SizeOf(TCnBLAKE2SParam), 0);

  if Key = nil then
    KeyLength := 0;
  if KeyLength > CN_BLAKE2S_KEYBYTES then
    KeyLength := CN_BLAKE2S_KEYBYTES;

  B2SP.DigestLength := DigestLength;
  B2SP.KeyLength := KeyLength;
  B2SP.FanOut := 1;
  B2SP.Depth := 1;

  // 用参数区初始化结构
  P := PCnUInt32Array(@B2SP);
  for I := 0 to 7 do
    Context.H[I] := BLAKE2S_IV[I] xor UInt32ToLittleEndian(P^[I]);

  // 有 Key 就先复制进一半块里算一把
  if (Key <> nil) and (KeyLength > 0) then
  begin
    FillChar(B[0], SizeOf(B), 0);
    Move(Key^, B[0], KeyLength);
    BLAKE2SUpdate(Context, @B[0], CN_BLAKE2S_BLOCKBYTES);
  end;
end;

procedure BLAKE2SInitW(var Context: TCnBLAKE2SContext; Key: PWideChar;
  CharLength, DigestLength: Integer);
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
    iLen := WideCharToMultiByte(0, 0, Key, CharLength, // 代码页默认用 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    BLAKE2SInit(Context, Content, iLen, DigestLength);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Key);
  A := AnsiString(S);
  BLAKE2SInit(Context, @A[1], Length(A), DigestLength);
{$ENDIF}
end;

procedure BLAKE2SUpdate(var Context: TCnBLAKE2SContext; Input: PAnsiChar; ByteLength: Cardinal);
var
  Left, Fill: Cardinal;
begin
  Left := Context.BufLen;
  Fill := CN_BLAKE2S_BLOCKBYTES - Left;

  if (Input <> nil) and (ByteLength > 0) then
  begin
    if ByteLength > Fill then
    begin
      // 补计算上回的
      Context.BufLen := 0;
      Move(Input^, Context.Buf[Left], Fill);

      IncBLAKE2SCounter(Context, CN_BLAKE2S_BLOCKBYTES);
      BLAKE2SCompress(Context, @Context.Buf[0]);

      Inc(Input, Fill);
      Dec(ByteLength, Fill);

      // 循环计算整块
      while ByteLength > CN_BLAKE2S_BLOCKBYTES do
      begin
        IncBLAKE2SCounter(Context, CN_BLAKE2S_BLOCKBYTES);
        BLAKE2SCompress(Context, Input);

        Inc(Input, CN_BLAKE2S_BLOCKBYTES);
        Dec(ByteLength, CN_BLAKE2S_BLOCKBYTES);
      end;
    end;

    // 不足块的留着等下回或 Final
    Move(Input^, Context.Buf[Context.BufLen], ByteLength);
    Context.BufLen := Context.BufLen + Integer(ByteLength);
  end;
end;

procedure BLAKE2SUpdateW(var Context: TCnBLAKE2SContext;
  Input: PWideChar; CharLength: Cardinal);
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
    BLAKE2SUpdate(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Input);
  A := AnsiString(S);
  BLAKE2SUpdate(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure BLAKE2SFinal(var Context: TCnBLAKE2SContext; var Digest: TCnBLAKE2SDigest);
var
  I: Integer;
  Dig: TCnBLAKE2SDigest;
  P: PCnUInt32Array;
begin
  IncBLAKE2SCounter(Context, Context.BufLen);

  // 最后一块没算完的补 0 算完
  FillChar(Context.Buf[Context.BufLen], CN_BLAKE2S_BLOCKBYTES - Context.BufLen, 0);
  Context.F[0] := Cardinal(-1);
  BLAKE2SCompress(Context, @Context.Buf[0]);

  // 生成结果
  P := PCnUInt32Array(@Dig[0]);
  for I := 0 to 7 do
    P^[I] := UInt32ToLittleEndian(Context.H[I]);

  // 按需输出
  FillChar(Digest[0], SizeOf(TCnBLAKE2SDigest), 0);
  Move(Dig[0], Digest[0], Context.OutLen);
end;

function BLAKE2S(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar;
  KeyLength, DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, Key, KeyLength, DigestLength);
  BLAKE2SUpdate(Context, Input, ByteLength);
  BLAKE2SFinal(Context, Result);
end;

function BLAKE2SBuffer(const Buffer; Count: Cardinal; const Key;
  KeyCount: Cardinal; DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, PAnsiChar(Key), KeyCount, DigestLength);
  BLAKE2SUpdate(Context, PAnsiChar(Buffer), Count);
  BLAKE2SFinal(Context, Result);
end;

// 以下三类函数用于外部持续对数据进行零散的 BLAKE2S 计算，BLAKE2SUpdate 可多次被调用

procedure BLAKE2BInit(var Context: TCnBLAKE2BContext; Key: PAnsiChar;
  KeyLength, DigestLength: Integer);
var
  I: Integer;
  B2BP: TCnBLAKE2BParam;
  P: PCnUInt64Array;
  B: array[0..CN_BLAKE2B_BLOCKBYTES - 1] of Byte;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  // 初始化结构
  FillChar(Context, SizeOf(TCnBLAKE2BContext), 0);
  Context.OutLen := DigestLength;

  // 初始化参数区
  FillChar(B2BP, SizeOf(TCnBLAKE2BParam), 0);

  if Key = nil then
    KeyLength := 0;
  if KeyLength > CN_BLAKE2B_KEYBYTES then
    KeyLength := CN_BLAKE2B_KEYBYTES;

  B2BP.DigestLength := DigestLength;
  B2BP.KeyLength := KeyLength;
  B2BP.FanOut := 1;
  B2BP.Depth := 1;

  // 用参数区初始化结构
  P := PCnUInt64Array(@B2BP);
  for I := 0 to 7 do
    Context.H[I] := BLAKE2B_IV[I] xor UInt64ToLittleEndian(P^[I]);

  // 有 Key 就先复制进一半块里算一把
  if (Key <> nil) and (KeyLength > 0) then
  begin
    FillChar(B[0], SizeOf(B), 0);
    Move(Key^, B[0], KeyLength);
    BLAKE2BUpdate(Context, @B[0], CN_BLAKE2B_BLOCKBYTES);
  end;
end;

procedure BLAKE2BInitW(var Context: TCnBLAKE2BContext; Key: PWideChar;
  CharLength, DigestLength: Integer);
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
    iLen := WideCharToMultiByte(0, 0, Key, CharLength, // 代码页默认用 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    BLAKE2BInit(Context, Content, iLen, DigestLength);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Key);
  A := AnsiString(S);
  BLAKE2BInit(Context, @A[1], Length(A), DigestLength);
{$ENDIF}
end;

procedure BLAKE2BUpdate(var Context: TCnBLAKE2BContext; Input: PAnsiChar; ByteLength: Cardinal);
var
  Left, Fill: Cardinal;
begin
  Left := Context.BufLen;
  Fill := CN_BLAKE2B_BLOCKBYTES - Left;

  if (Input <> nil) and (ByteLength > 0) then
  begin
    if ByteLength > Fill then
    begin
      // 补计算上回的
      Context.BufLen := 0;
      Move(Input^, Context.Buf[Left], Fill);

      IncBLAKE2BCounter(Context, CN_BLAKE2B_BLOCKBYTES);
      BLAKE2BCompress(Context, @Context.Buf[0]);

      Inc(Input, Fill);
      Dec(ByteLength, Fill);

      // 循环计算整块
      while ByteLength > CN_BLAKE2B_BLOCKBYTES do
      begin
        IncBLAKE2BCounter(Context, CN_BLAKE2B_BLOCKBYTES);
        BLAKE2BCompress(Context, Input);

        Inc(Input, CN_BLAKE2B_BLOCKBYTES);
        Dec(ByteLength, CN_BLAKE2B_BLOCKBYTES);
      end;
    end;

    // 不足块的留着等下回或 Final
    Move(Input^, Context.Buf[Context.BufLen], ByteLength);
    Context.BufLen := Context.BufLen + Integer(ByteLength);
  end;
end;

procedure BLAKE2BUpdateW(var Context: TCnBLAKE2BContext;
  Input: PWideChar; CharLength: Cardinal);
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
    BLAKE2BUpdate(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Input);
  A := AnsiString(S);
  BLAKE2BUpdate(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure BLAKE2BFinal(var Context: TCnBLAKE2BContext; var Digest: TCnBLAKE2BDigest);
var
  I: Integer;
  Dig: TCnBLAKE2BDigest;
  P: PCnUInt64Array;
begin
  IncBLAKE2BCounter(Context, Context.BufLen);

  // 最后一块没算完的补 0 算完
  FillChar(Context.Buf[Context.BufLen], CN_BLAKE2B_BLOCKBYTES - Context.BufLen, 0);
  Context.F[0] := TUInt64(-1);
  BLAKE2BCompress(Context, @Context.Buf[0]);

  // 生成结果
  P := PCnUInt64Array(@Dig[0]);
  for I := 0 to 7 do
    P^[I] := UInt64ToLittleEndian(Context.H[I]);

  // 按需输出
  FillChar(Digest[0], SizeOf(TCnBLAKE2BDigest), 0);
  Move(Dig[0], Digest[0], Context.OutLen);
end;

function BLAKE2B(Input: PAnsiChar; ByteLength: Cardinal; Key: PAnsiChar;
  KeyLength, DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, Key, KeyLength, DigestLength);
  BLAKE2BUpdate(Context, Input, ByteLength);
  BLAKE2BFinal(Context, Result);
end;

function BLAKE2BBuffer(const Buffer; Count: Cardinal; const Key;
  KeyCount: Cardinal; DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, PAnsiChar(Key), KeyCount, DigestLength);
  BLAKE2BUpdate(Context, PAnsiChar(Buffer), Count);
  BLAKE2BFinal(Context, Result);
end;

function BLAKE2SBytes(const Data: TBytes; const Key: TBytes;
  DigestLength: Integer): TCnBLAKE2SDigest;
var
  D, K: PAnsiChar;
  DL, KL: Cardinal;
begin
  if (Data = nil) or (Length(Data) = 0) then
  begin
    D := nil;
    DL := 0;
  end
  else
  begin
    D := @Data[0];
    DL := Length(Data);
  end;

  if (Key = nil) or (Length(Key) = 0) then
  begin
    K := nil;
    KL := 0;
  end
  else
  begin
    K := @Key[0];
    KL := Length(Key);
  end;

  Result := BLAKE2S(D, DL, K, KL, DigestLength);
end;

function BLAKE2BBytes(const Data: TBytes; const Key: TBytes;
  DigestLength: Integer): TCnBLAKE2BDigest;
var
  D, K: PAnsiChar;
  DL, KL: Cardinal;
begin
  if (Data = nil) or (Length(Data) = 0) then
  begin
    D := nil;
    DL := 0;
  end
  else
  begin
    D := @Data[0];
    DL := Length(Data);
  end;

  if (Key = nil) or (Length(Key) = 0) then
  begin
    K := nil;
    KL := 0;
  end
  else
  begin
    K := @Key[0];
    KL := Length(Key);
  end;

  Result := BLAKE2B(D, DL, K, KL, DigestLength);
end;

function BLAKE2SString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2SDigest;
var
  AStr, AKey: AnsiString;
begin
  AStr := AnsiString(Str);
  AKey := AnsiString(Key);
  Result := BLAKE2SStringA(AStr, AKey, DigestLength);
end;

function BLAKE2BString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2BDigest;
var
  AStr, AKey: AnsiString;
begin
  AStr := AnsiString(Str);
  AKey := AnsiString(Key);
  Result := BLAKE2BStringA(AStr, AKey, DigestLength);
end;

function BLAKE2SStringA(const Str: AnsiString; const Key: AnsiString;
  DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, PAnsiChar(Key), Length(Key), DigestLength);
  BLAKE2SUpdate(Context, PAnsiChar(Str), Length(Str));
  BLAKE2SFinal(Context, Result);
end;

function BLAKE2SStringW(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2SDigest;
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInitW(Context, PWideChar(Key), Length(Key), DigestLength);
  BLAKE2SUpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE2SFinal(Context, Result);
end;

function BLAKE2BStringA(const Str: AnsiString; const Key: AnsiString;
  DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, PAnsiChar(Key), Length(Key), DigestLength);
  BLAKE2BUpdate(Context, PAnsiChar(Str), Length(Str));
  BLAKE2BFinal(Context, Result);
end;

function BLAKE2BStringW(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2BDigest;
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInitW(Context, PWideChar(Key), Length(Key), DigestLength);
  BLAKE2BUpdateW(Context, PWideChar(Str), Length(Str));
  BLAKE2BFinal(Context, Result);
end;

{$IFDEF UNICODE}
function BLAKE2SUnicodeString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2SDigest;
{$ELSE}
function BLAKE2SUnicodeString(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2SDigest;
{$ENDIF}
var
  Context: TCnBLAKE2SContext;
begin
  BLAKE2SInit(Context, PAnsiChar(@Key[1]), Length(Key) * SizeOf(WideChar), DigestLength);
  BLAKE2SUpdate(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE2SFinal(Context, Result);
end;

{$IFDEF UNICODE}
function BLAKE2BUnicodeString(const Str: string; const Key: string;
  DigestLength: Integer): TCnBLAKE2BDigest;
{$ELSE}
function BLAKE2BUnicodeString(const Str: WideString; const Key: WideString;
  DigestLength: Integer): TCnBLAKE2BDigest;
{$ENDIF}
var
  Context: TCnBLAKE2BContext;
begin
  BLAKE2BInit(Context, PAnsiChar(@Key[1]), Length(Key) * SizeOf(WideChar), DigestLength);
  BLAKE2BUpdate(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  BLAKE2BFinal(Context, Result);
end;

function InternalBLAKE2Stream(Stream: TStream; Key: TBytes; DigestLength: Integer;
  const BufSize: Cardinal; var D: TCnBLAKE2GeneralDigest; BLAKE2Type: TBLAKE2Type;
  CallBack: TCnBLAKE2CalcProgressFunc): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
  KP: PAnsiChar;
  KL: Cardinal;

  Context2S: TCnBLAKE2SContext;
  Context2B: TCnBLAKE2BContext;
  Dig2S: TCnBLAKE2SDigest;
  Dig2B: TCnBLAKE2BDigest;

  procedure _BLAKE2Init;
  begin
    if (Key = nil) or (Length(Key) = 0) then
    begin
      KP := nil;
      KL := 0;
    end
    else
    begin
      KP := @Key[0];
      KL := Length(Key);
    end;

    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SInit(Context2S, KP, KL, DigestLength);
      btBLAKE2B:
        BLAKE2BInit(Context2B, KP, KL, DigestLength);
    end;
  end;

  procedure _BLAKE2Update;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SUpdate(Context2S, Buf, ReadBytes);
      btBLAKE2B:
        BLAKE2BUpdate(Context2B, Buf, ReadBytes);
    end;
  end;

  procedure _BLAKE2Final;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SFinal(Context2S, Dig2S);
      btBLAKE2B:
        BLAKE2BFinal(Context2B, Dig2B);
    end;
  end;

  procedure _CopyResult;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        Move(Dig2S[0], D[0], SizeOf(TCnBLAKE2SDigest));
      btBLAKE2B:
        Move(Dig2B[0], D[0], SizeOf(TCnBLAKE2BDigest));
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
  _BLAKE2Init;
 
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        _BLAKE2Update;

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    _BLAKE2Final;
    _CopyResult;
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

function BLAKE2SStream(Stream: TStream; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2SDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  InternalBLAKE2Stream(Stream, Key, DigestLength, 4096 * 1024, Dig, btBLAKE2S, CallBack);
  Move(Dig[0], Result[0], DigestLength);
end;

function BLAKE2BStream(Stream: TStream; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2BDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  InternalBLAKE2Stream(Stream, Key, DigestLength, 4096 * 1024, Dig, btBLAKE2B, CallBack);
  Move(Dig[0], Result[0], DigestLength);
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

function InternalBLAKE2File(const FileName: string; Key: TBytes; DigestLength: Integer;
  BLAKE2Type: TBLAKE2Type; CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2GeneralDigest;
var
  Context2S: TCnBLAKE2SContext;
  Context2B: TCnBLAKE2BContext;
  Dig2S: TCnBLAKE2SDigest;
  Dig2B: TCnBLAKE2BDigest;
  KP: PAnsiChar;
  KL: Cardinal;

{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  procedure _BLAKE2Init;
  begin
    if (Key = nil) or (Length(Key) = 0) then
    begin
      KP := nil;
      KL := 0;
    end
    else
    begin
      KP := @Key[0];
      KL := Length(Key);
    end;

    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SInit(Context2S, KP, KL, DigestLength);
      btBLAKE2B:
        BLAKE2BInit(Context2B, KP, KL, DigestLength);
    end;
  end;

{$IFDEF MSWINDOWS}
  procedure _BLAKE2Update;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SUpdate(Context2S, ViewPointer, GetFileSize(FileHandle, nil));
      btBLAKE2B:
        BLAKE2BUpdate(Context2B, ViewPointer, GetFileSize(FileHandle, nil));
    end;
  end;
{$ENDIF}

  procedure _BLAKE2Final;
  begin
    case BLAKE2Type of
      btBLAKE2S:
        BLAKE2SFinal(Context2S, Dig2S);
      btBLAKE2B:
        BLAKE2BFinal(Context2B, Dig2B);
    end;
  end;

  procedure _CopyResult(var D: TCnBLAKE2GeneralDigest);
  begin
    case BLAKE2Type of
      btBLAKE2S:
        Move(Dig2S[0], D[0], SizeOf(TCnBLAKE2SDigest));
      btBLAKE2B:
        Move(Dig2B[0], D[0], SizeOf(TCnBLAKE2BDigest));
    end;
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // 大于 2G 的文件可能 Map 失败，或非 Windows 平台，采用流方式循环处理
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalBLAKE2Stream(Stream, Key, DigestLength, 4096 * 1024, Result, BLAKE2Type, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    _BLAKE2Init;
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
                _BLAKE2Update;
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
    _BLAKE2Final;
    _CopyResult(Result);
{$ENDIF}
  end;
end;

function BLAKE2SFile(const FileName: string; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2SDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Dig := InternalBLAKE2File(FileName, Key, DigestLength, btBLAKE2S, CallBack);
  Move(Dig[0], Result[0], DigestLength);
end;

function BLAKE2BFile(const FileName: string; Key: TBytes; DigestLength: Integer;
  CallBack: TCnBLAKE2CalcProgressFunc): TCnBLAKE2BDigest;
var
  Dig: TCnBLAKE2GeneralDigest;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Dig := InternalBLAKE2File(FileName, Key, DigestLength, btBLAKE2B, CallBack);
  Move(Dig[0], Result[0], DigestLength);
end;

function BLAKE2SPrint(const Digest: TCnBLAKE2SDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := DataToHex(@Digest[0], DigestLength);
end;

function BLAKE2BPrint(const Digest: TCnBLAKE2BDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := DataToHex(@Digest[0], DigestLength);
end;

function BLAKE2SMatch(const D1, D2: TCnBLAKE2SDigest; DigestLength: Integer): Boolean;
var
  I: Integer;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  I := 0;
  Result := True;
  while Result and (I < DigestLength) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

function BLAKE2BMatch(const D1, D2: TCnBLAKE2BDigest; DigestLength: Integer): Boolean;
var
  I: Integer;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  I := 0;
  Result := True;
  while Result and (I < DigestLength) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

function BLAKE2SDigestToStr(const Digest: TCnBLAKE2SDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2S_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := MemoryToString(@Digest[0], DigestLength);
end;

function BLAKE2BDigestToStr(const Digest: TCnBLAKE2BDigest; DigestLength: Integer): string;
begin
  if (DigestLength <= 0) or (DigestLength > CN_BLAKE2B_OUTBYTES) then
    raise ECnBLAKE2Exception.Create(SCnErrorBlake2InvalidDigestSize);

  Result := MemoryToString(@Digest[0], DigestLength);
end;

end.
