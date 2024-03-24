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

unit CnSM4;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：国产分组密码算法 SM4 单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：参考国密算法公开文档 SM4 Encryption alogrithm
*           并参考移植 goldboar 的 C 代码*
*           本单元未处理对齐方式，默认只在末尾补 0，
*           如需要 PKCS 之类的支持，，请在外部调用CnPemUtils 中的 PKCS 处理函数
*           另外高版本 Delphi 中请尽量避免使用 AnsiString 参数版本的函数（十六进制除外），
*           避免不可视字符出现乱码影响加解密结果。
* 开发平台：Windows 7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP/7 + Delphi 5/6 + MaxOS 64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.07.21 V1.7
*               加入 CTR 模式的支持
*           2022.06.21 V1.6
*               加入几个字节数组到十六进制字符串之间的加解密函数
*           2022.04.26 V1.5
*               修改 LongWord 与 Integer 地址转换以支持 MacOS64
*           2022.04.19 V1.4
*               使用初始化向量时内部备份，不修改传入的内容
*           2021.12.12 V1.3
*               加入 CFB/OFB 模式的支持
*           2020.03.24 V1.2
*               增加部分封装函数包括流函数
*           2019.04.15 V1.1
*               支持 Win32/Win64/MacOS
*           2014.09.25 V1.0
*               移植并创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative;

const
  CN_SM4_KEYSIZE = 16;
  {* SM4 的密码长度 16 字节}

  CN_SM4_BLOCKSIZE = 16;
  {* SM4 的分块长度 16 字节}

  CN_SM4_NONCESIZE = 8;
  {* SM4 的 CTR 模式下的准初始化向量长度 8 字节}

type
  TCnSM4Key    = array[0..CN_SM4_KEYSIZE - 1] of Byte;
  {* SM4 的加密 Key}

  TCnSM4Buffer = array[0..CN_SM4_BLOCKSIZE - 1] of Byte;
  {* SM4 的加密块}

  TCnSM4Iv     = array[0..CN_SM4_BLOCKSIZE - 1] of Byte;
  {* SM4 的 CBC/CFB/OFB 等的初始化向量}

  TCnSM4Nonce  = array[0..CN_SM4_NONCESIZE - 1] of Byte;
  {* SM4 的 CTR 模式下的初始化向量，与一个八字节计数器拼在一起作为真正的 Iv}

  TCnSM4Context = packed record
    Mode: Integer;                                       {!<  encrypt/decrypt }
    Sk: array[0..CN_SM4_KEYSIZE * 2 - 1] of Cardinal;    {!<  SM4 subkeys     }
  end;

procedure SM4Encrypt(Key: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar; Len: Integer);
{* 原始的 SM4 加密数据块，ECB 模式，将 Input 内的明文内容加密搁到 Output 中
  调用者自行保证 Key 指向内容至少 16 字节，Input 和 Output 指向内容长相等并且都为 Len 字节
  且 Len 必须被 16 整除}

procedure SM4Decrypt(Key: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar; Len: Integer);
{* 原始的 SM4 解密数据块，ECB 模式，将 Input 内的密文内容解密搁到 Output 中
  调用者自行保证 Key 指向内容至少需 16 字节，Input 和 Output 指向内容长相等并且都为 Len 字节
  且 Len 必须被 16 整除}

// ============== 明文字符串与密文十六进制字符串之间的加解密 ===================

procedure SM4EncryptEcbStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
{* SM4-ECB 封装好的针对 AnsiString 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Input    原始待加密字符串，其长度如不是 16 倍数，计算时会被填充 #0 至长度达到 16 的倍数
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4DecryptEcbStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
{* SM4-ECB 封装好的针对 AnsiString 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Input    原始待解密字符串，其长度如不是 16 倍数，计算时会被填充 #0 至长度达到 16 的倍数
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4EncryptCbcStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-CBC 封装好的针对 AnsiString 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Iv       不短于 16 字节的初始化向量，太长则超出部分忽略
  Input    原始待加密字符串
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4DecryptCbcStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-CBC 封装好的针对 AnsiString 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Iv       不短于 16 字节的初始化向量，太长则超出部分忽略
  Input    原始待解密字符串
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4EncryptCfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-CFB 封装好的针对 AnsiString 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Iv       不短于 16 字节的初始化向量，太长则超出部分忽略
  Input    原始待加密字符串
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4DecryptCfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-CFB 封装好的针对 AnsiString 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Iv       不短于 16 字节的初始化向量，太长则超出部分忽略
  Input    原始待解密字符串
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4EncryptOfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-OFB 封装好的针对 AnsiString 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Iv       不短于 16 字节的初始化向量，太长则超出部分忽略
  Input    原始待加密字符串
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4DecryptOfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-OFB 封装好的针对 AnsiString 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Iv       不短于 16 字节的初始化向量，太长则超出部分忽略
  Input    原始待解密字符串
  Output   Output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 16) + 1) * 16
 |</PRE>}

procedure SM4EncryptCtrStr(Key: AnsiString; Nonce: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-OFB 封装好的针对 AnsiString 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Nonce    不短于 8 字节的初始化向量，太长则超出部分忽略
  Input    原始待加密字符串
  Output   Output 输出区，其长度必须大于或等于 Length(Input)
 |</PRE>}

procedure SM4DecryptCtrStr(Key: AnsiString; Nonce: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* SM4-OFB 封装好的针对 AnsiString 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 #0
  Nonce    不短于 8 字节的初始化向量，太长则超出部分忽略
  Input    原始待解密字符串
  Output   Output 输出区，其长度必须大于或等于 Length(Input)
 |</PRE>}

// ================= 明文字节数组与密文字节数组之间的加解密 ====================

function SM4EncryptEcbBytes(Key: TBytes; const Input: TBytes): TBytes;
{* SM4-ECB 封装好的针对 TBytes 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Input    原始待加密内容，其长度如不是 16 倍数，计算时会被填充 0 至长度达到 16 的倍数
  返回值   加密内容
 |</PRE>}

function SM4DecryptEcbBytes(Key: TBytes; const Input: TBytes): TBytes;
{* SM4-ECB 封装好的针对 TBytes 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Input    原始待加密内容，其长度如不是 16 倍数，计算时会被填充 0 至长度达到 16 的倍数
  返回值   解密内容
 |</PRE>}

function SM4EncryptCbcBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* SM4-CBC 封装好的针对 TBytes 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptCbcBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* SM4-CBC 封装好的针对 TBytes 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    input 密文
  返回值   解密内容
 |</PRE>}

function SM4EncryptCfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* SM4-CFB 封装好的针对 TBytes 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptCfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* SM4-CFB 封装好的针对 TBytes 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    input 密文
  返回值   解密内容
 |</PRE>}

function SM4EncryptOfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* SM4-OFB 封装好的针对 TBytes 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptOfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* SM4-OFB 封装好的针对 TBytes 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    input 密文
  返回值   解密内容
 |</PRE>}

function SM4EncryptCtrBytes(Key, Nonce: TBytes; const Input: TBytes): TBytes;
{* SM4-CTR 封装好的针对 TBytes 的加密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Nonce    8 字节初始化向量，太长则超出部分忽略，不足则在 Nonce 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptCtrBytes(Key, Nonce: TBytes; const Input: TBytes): TBytes;
{* SM4-CTR 封装好的针对 TBytes 的解密方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Nonce    8 字节初始化向量，太长则超出部分忽略，不足则在 Nonce 后补 0
  Input    input 密文
  返回值   解密内容
 |</PRE>}

// ============== 明文字节数组与密文十六进制字符串之间的加解密 =================

function SM4EncryptEcbBytesToHex(Key: TBytes; const Input: TBytes): AnsiString;
{* SM4-ECB 封装好的针对 TBytes 的加密并转换成十六进制字符串的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Input    原始待加密内容，其长度如不是 16 倍数，计算时会被填充 0 至长度达到 16 的倍数
  返回值   加密内容
 |</PRE>}

function SM4DecryptEcbBytesFromHex(Key: TBytes; const Input: AnsiString): TBytes;
{* SM4-ECB 封装好的针对十六进制字符串解密成 TBytes 的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Input    十六进制密文，其解码后的长度如不是 16 倍数，计算时会被填充 0 至长度达到 16 的倍数
  返回值   解密内容
 |</PRE>}

function SM4EncryptCbcBytesToHex(Key, Iv: TBytes; const Input: TBytes): AnsiString;
{* SM4-CBC 封装好的针对 TBytes 的加密并转换成十六进制字符串的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptCbcBytesFromHex(Key, Iv: TBytes; const Input: AnsiString): TBytes;
{* SM4-CBC 封装好的针对十六进制字符串解密成 TBytes 的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    十六进制密文
  返回值   解密内容
 |</PRE>}

function SM4EncryptCfbBytesToHex(Key, Iv: TBytes; const Input: TBytes): AnsiString;
{* SM4-CFB 封装好的针对 TBytes 的加密并转换成十六进制字符串的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptCfbBytesFromHex(Key, Iv: TBytes; const Input: AnsiString): TBytes;
{* SM4-CFB 封装好的针对十六进制字符串解密成 TBytes 的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    十六进制密文
  返回值   解密内容
 |</PRE>}

function SM4EncryptOfbBytesToHex(Key, Iv: TBytes; const Input: TBytes): AnsiString;
{* SM4-OFB 封装好的针对 TBytes 的加密并转换成十六进制字符串的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptOfbBytesFromHex(Key, Iv: TBytes; const Input: AnsiString): TBytes;
{* SM4-OFB 封装好的针对十六进制字符串解密成 TBytes 的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Iv       16 字节初始化向量，太长则超出部分忽略，不足则在 Iv 后补 0
  Input    十六进制密文
  返回值   解密内容
 |</PRE>}

function SM4EncryptCtrBytesToHex(Key, Nonce: TBytes; const Input: TBytes): AnsiString;
{* SM4-CTR 封装好的针对 TBytes 的加密并转换成十六进制字符串的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Nonce    8 字节初始化向量，太长则超出部分忽略，不足则在 Nonce 后补 0
  Input    原始待加密内容
  返回值   加密内容
 |</PRE>}

function SM4DecryptCtrBytesFromHex(Key, Nonce: TBytes; const Input: AnsiString): TBytes;
{* SM4-CTR 封装好的针对十六进制字符串解密成 TBytes 的方法
 |<PRE>
  Key      16 字节密码，太长则截断，不足则补 0
  Nonce    8 字节初始化向量，太长则超出部分忽略，不足则在 Nonce 后补 0
  Input    十六进制密文
  返回值   解密内容
 |</PRE>}

// ======================= 明文流与密文流之间的加解密 ==========================

procedure SM4EncryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; Dest: TStream); overload;
{* SM4-ECB 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure SM4DecryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; Dest: TStream); overload;
{* SM4-ECB 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

procedure SM4EncryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
{* SM4-CBC 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure SM4DecryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
{* SM4-CBC 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

procedure SM4EncryptStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
{* SM4-CFB 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure SM4DecryptStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
{* SM4-CFB 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

procedure SM4EncryptStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
{* SM4-OFB 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure SM4DecryptStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
{* SM4-OFB 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

procedure SM4EncryptStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Nonce; Dest: TStream);
{* SM4-CTR 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure SM4DecryptStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Nonce; Dest: TStream);
{* SM4-CTR 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

// 以下仨函数为底层加密函数，开放出来供外部挨块加密使用

procedure SM4SetKeyEnc(var Ctx: TCnSM4Context; Key: PAnsiChar);
{* 将 16 字节 Key 塞进 Context 并设置为加密模式}

procedure SM4SetKeyDec(var Ctx: TCnSM4Context; Key: PAnsiChar);
{* 将 16 字节 Key 塞进 Context 并设置为解密模式}

procedure SM4OneRound(SK: PCardinal; Input: PAnsiChar; Output: PAnsiChar);
{* 加解密一个块，内容从 Input 至 Output，长度 16 字节，两者可以是同一个区域
  SK是 TSM4Context 的 Sk，加还是解由其决定}

implementation

resourcestring
  SCnErrorSM4InvalidInBufSize = 'Invalid Buffer Size for Decryption';
  SCnErrorSM4ReadError = 'Stream Read Error';
  SCnErrorSM4WriteError = 'Stream Write Error';

const
  SM4_ENCRYPT = 1;
  SM4_DECRYPT = 0;

  SBoxTable: array[0..CN_SM4_KEYSIZE - 1] of array[0..CN_SM4_KEYSIZE - 1] of Byte = (
    ($D6, $90, $E9, $FE, $CC, $E1, $3D, $B7, $16, $B6, $14, $C2, $28, $FB, $2C, $05),
    ($2B, $67, $9A, $76, $2A, $BE, $04, $C3, $AA, $44, $13, $26, $49, $86, $06, $99),
    ($9C, $42, $50, $F4, $91, $EF, $98, $7A, $33, $54, $0B, $43, $ED, $CF, $AC, $62),
    ($E4, $B3, $1C, $A9, $C9, $08, $E8, $95, $80, $DF, $94, $FA, $75, $8F, $3F, $A6),
    ($47, $07, $A7, $FC, $F3, $73, $17, $BA, $83, $59, $3C, $19, $E6, $85, $4F, $A8),
    ($68, $6B, $81, $B2, $71, $64, $DA, $8B, $F8, $EB, $0F, $4B, $70, $56, $9D, $35),
    ($1E, $24, $0E, $5E, $63, $58, $D1, $A2, $25, $22, $7C, $3B, $01, $21, $78, $87),
    ($D4, $00, $46, $57, $9F, $D3, $27, $52, $4C, $36, $02, $E7, $A0, $C4, $C8, $9E),
    ($EA, $BF, $8A, $D2, $40, $C7, $38, $B5, $A3, $F7, $F2, $CE, $F9, $61, $15, $A1),
    ($E0, $AE, $5D, $A4, $9B, $34, $1A, $55, $AD, $93, $32, $30, $F5, $8C, $B1, $E3),
    ($1D, $F6, $E2, $2E, $82, $66, $CA, $60, $C0, $29, $23, $AB, $0D, $53, $4E, $6F),
    ($D5, $DB, $37, $45, $DE, $FD, $8E, $2F, $03, $FF, $6A, $72, $6D, $6C, $5B, $51),
    ($8D, $1B, $AF, $92, $BB, $DD, $BC, $7F, $11, $D9, $5C, $41, $1F, $10, $5A, $D8),
    ($0A, $C1, $31, $88, $A5, $CD, $7B, $BD, $2D, $74, $D0, $12, $B8, $E5, $B4, $B0),
    ($89, $69, $97, $4A, $0C, $96, $77, $7E, $65, $B9, $F1, $09, $C5, $6E, $C6, $84),
    ($18, $F0, $7D, $EC, $3A, $DC, $4D, $20, $79, $EE, $5F, $3E, $D7, $CB, $39, $48)
  );

  FK: array[0..3] of Cardinal = ($A3B1BAC6, $56AA3350, $677D9197, $B27022DC);

  CK: array[0..CN_SM4_KEYSIZE * 2 - 1] of Cardinal = (
    $00070E15, $1C232A31, $383F464D, $545B6269,
    $70777E85, $8C939AA1, $A8AFB6BD, $C4CBD2D9,
    $E0E7EEF5, $FC030A11, $181F262D, $343B4249,
    $50575E65, $6C737A81, $888F969D, $A4ABB2B9,
    $C0C7CED5, $DCE3EAF1, $F8FF060D, $141B2229,
    $30373E45, $4C535A61, $686F767D, $848B9299,
    $A0A7AEB5, $BCC3CAD1, $D8DFE6ED, $F4FB0209,
    $10171E25, $2C333A41, $484F565D, $646B7279 );

function Min(A, B: Integer): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

procedure GetULongBe(var N: Cardinal; B: PAnsiChar; I: Integer);
var
  D: Cardinal;
begin
  D := (Cardinal(B[I]) shl 24) or (Cardinal(B[I + 1]) shl 16) or
    (Cardinal(B[I + 2]) shl 8) or (Cardinal(B[I + 3]));
  N := D;
end;

procedure PutULongBe(N: Cardinal; B: PAnsiChar; I: Integer);
begin
  B[I] := AnsiChar(N shr 24);
  B[I + 1] := AnsiChar(N shr 16);
  B[I + 2] := AnsiChar(N shr 8);
  B[I + 3] := AnsiChar(N);
end;

function SM4Shl(X: Cardinal; N: Integer): Cardinal;
begin
  Result := (X and $FFFFFFFF) shl N;
end;

function ROTL(X: Cardinal; N: Integer): Cardinal;
begin
  Result := SM4Shl(X, N) or (X shr (32 - N));
end;

procedure Swap(var A: Cardinal; var B: Cardinal);
var
  T: Cardinal;
begin
  T := A;
  A := B;
  B := T;
end;

function SM4SBox(Inch: Byte): Byte;
var
  PTable: Pointer;
begin
  PTable := @(SboxTable[0][0]);
  Result := PByte(TCnNativeInt(PTable) + Inch)^;
end;

function SM4Lt(Ka: Cardinal): Cardinal;
var
  BB: Cardinal;
  A: array[0..3] of Byte;
  B: array[0..3] of Byte;
begin
  BB := 0;
  PutULongBe(Ka, @(A[0]), 0);
  B[0] := SM4SBox(A[0]);
  B[1] := SM4SBox(A[1]);
  B[2] := SM4SBox(A[2]);
  B[3] := SM4SBox(A[3]);
  GetULongBe(BB, @(B[0]), 0);

  Result := BB xor (ROTL(BB, 2)) xor (ROTL(BB, 10)) xor (ROTL(BB, 18))
    xor (ROTL(BB, 24));
end;

function SM4F(X0: Cardinal; X1: Cardinal; X2: Cardinal; X3: Cardinal; RK: Cardinal): Cardinal;
begin
  Result := X0 xor SM4Lt(X1 xor X2 xor X3 xor RK);
end;

function SM4CalciRK(Ka: Cardinal): Cardinal;
var
  BB: Cardinal;
  A: array[0..3] of Byte;
  B: array[0..3] of Byte;
begin
  PutULongBe(Ka, @(A[0]), 0);
  B[0] := SM4SBox(A[0]);
  B[1] := SM4SBox(A[1]);
  B[2] := SM4SBox(A[2]);
  B[3] := SM4SBox(A[3]);
  GetULongBe(BB, @(B[0]), 0);
  Result := BB xor ROTL(BB, 13) xor ROTL(BB, 23);
end;

// SK Points to 32 DWord Array; Key Points to 16 Byte Array
procedure SM4SetKey(SK: PCardinal; Key: PAnsiChar);
var
  MK: array[0..3] of Cardinal;
  K: array[0..35] of Cardinal;
  I: Integer;
begin
  GetULongBe(MK[0], Key, 0);
  GetULongBe(MK[1], Key, 4);
  GetULongBe(MK[2], Key, 8);
  GetULongBe(MK[3], Key, 12);

  K[0] := MK[0] xor FK[0];
  K[1] := MK[1] xor FK[1];
  K[2] := MK[2] xor FK[2];
  K[3] := MK[3] xor FK[3];

  for I := 0 to 31 do
  begin
    K[I + 4] := K[I] xor SM4CalciRK(K[I + 1] xor K[I + 2] xor K[I + 3] xor CK[I]);
    (PCardinal(TCnNativeInt(SK) + I * SizeOf(Cardinal)))^ := K[I + 4];
  end;
end;

// SK Points to 32 DWord Array; Input/Output Points to 16 Byte Array
// Input 和 Output 可以是同一处区域
procedure SM4OneRound(SK: PCardinal; Input: PAnsiChar; Output: PAnsiChar);
var
  I: Integer;
  UlBuf: array[0..35] of Cardinal;
begin
  FillChar(UlBuf[0], SizeOf(UlBuf), 0);

  GetULongBe(UlBuf[0], Input, 0);
  GetULongBe(UlBuf[1], Input, 4);
  GetULongBe(UlBuf[2], Input, 8);
  GetULongBe(UlBuf[3], Input, 12);

  for I := 0 to 31 do
  begin
    UlBuf[I + 4] := SM4F(UlBuf[I], UlBuf[I + 1], UlBuf[I + 2], UlBuf[I + 3],
      (PCardinal(TCnNativeInt(SK) + I * SizeOf(Cardinal)))^);
  end;

  PutULongBe(UlBuf[35], Output, 0);
  PutULongBe(UlBuf[34], Output, 4);
  PutULongBe(UlBuf[33], Output, 8);
  PutULongBe(UlBuf[32], Output, 12);
end;

procedure SM4SetKeyEnc(var Ctx: TCnSM4Context; Key: PAnsiChar);
begin
  Ctx.Mode := SM4_ENCRYPT;
  SM4SetKey(@(Ctx.Sk[0]), Key);
end;

procedure SM4SetKeyDec(var Ctx: TCnSM4Context; Key: PAnsiChar);
var
  I: Integer;
begin
  Ctx.Mode := SM4_DECRYPT;
  SM4SetKey(@(Ctx.Sk[0]), Key);

  for I := 0 to CN_SM4_KEYSIZE - 1 do
    Swap(Ctx.Sk[I], Ctx.Sk[31 - I]);
end;

procedure SM4CryptEcb(var Ctx: TCnSM4Context; Mode: Integer; Length: Integer;
  Input: PAnsiChar; Output: PAnsiChar);
var
  EndBuf: TCnSM4Buffer;
begin
  while Length > 0 do
  begin
    if Length >= CN_SM4_BLOCKSIZE then
    begin
      SM4OneRound(@(Ctx.Sk[0]), Input, Output);
    end
    else
    begin
      // 尾部不足 16，补 0
      FillChar(EndBuf[0], CN_SM4_BLOCKSIZE, 0);
      Move(Input^, EndBuf[0], Length);
      SM4OneRound(@(Ctx.Sk[0]), @(EndBuf[0]), Output);
    end;
    Inc(Input, CN_SM4_BLOCKSIZE);
    Inc(Output, CN_SM4_BLOCKSIZE);
    Dec(Length, CN_SM4_BLOCKSIZE);
  end;
end;

procedure SM4CryptEcbStr(Mode: Integer; Key: AnsiString;
  const Input: AnsiString; Output: PAnsiChar);
var
  Ctx: TCnSM4Context;
begin
  if Length(Key) < CN_SM4_KEYSIZE then
    while Length(Key) < CN_SM4_KEYSIZE do Key := Key + Chr(0) // 16 bytes at least padding 0.
  else if Length(Key) > CN_SM4_KEYSIZE then
    Key := Copy(Key, 1, CN_SM4_KEYSIZE);  // Only keep 16

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1]));
    SM4CryptEcb(Ctx, SM4_ENCRYPT, Length(Input), @(Input[1]), @(Output[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyDec(Ctx, @(Key[1]));
    SM4CryptEcb(Ctx, SM4_DECRYPT, Length(Input), @(Input[1]), @(Output[0]));
  end;
end;

procedure SM4CryptCbc(var Ctx: TCnSM4Context; Mode: Integer; Length: Integer;
  Iv: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar);
var
  I: Integer;
  EndBuf: TCnSM4Buffer;
  LocalIv: TCnSM4Iv;
begin
  Move(Iv^, LocalIv[0], CN_SM4_BLOCKSIZE);
  if Mode = SM4_ENCRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= CN_SM4_BLOCKSIZE then
      begin
        for I := 0 to CN_SM4_BLOCKSIZE - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Input) + I))^
            xor LocalIv[I];

        SM4OneRound(@(Ctx.Sk[0]), Output, Output);
        Move(Output[0], LocalIv[0], CN_SM4_BLOCKSIZE);
      end
      else
      begin
        // 尾部不足 16，补 0
        FillChar(EndBuf[0], SizeOf(EndBuf), 0);
        Move(Input^, EndBuf[0], Length);

        for I := 0 to CN_SM4_BLOCKSIZE - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := EndBuf[I]
            xor LocalIv[I];

        SM4OneRound(@(Ctx.Sk[0]), Output, Output);
        Move(Output[0], LocalIv[0], CN_SM4_BLOCKSIZE);
      end;

      Inc(Input, CN_SM4_BLOCKSIZE);
      Inc(Output, CN_SM4_BLOCKSIZE);
      Dec(Length, CN_SM4_BLOCKSIZE);
    end;
  end
  else if Mode = SM4_DECRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= CN_SM4_BLOCKSIZE then
      begin
        SM4OneRound(@(Ctx.Sk[0]), Input, Output);

        for I := 0 to CN_SM4_BLOCKSIZE - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Output) + I))^
            xor LocalIv[I];

        Move(Input^, LocalIv[0], CN_SM4_BLOCKSIZE);
      end
      else
      begin
        // 尾部不足 16，补 0
        FillChar(EndBuf[0], SizeOf(EndBuf), 0);
        Move(Input^, EndBuf[0], Length);
        SM4OneRound(@(Ctx.Sk[0]), @(EndBuf[0]), Output);

        for I := 0 to CN_SM4_BLOCKSIZE - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Output) + I))^
            xor LocalIv[I];

        Move(EndBuf[0], LocalIv[0], CN_SM4_BLOCKSIZE);
      end;

      Inc(Input, CN_SM4_BLOCKSIZE);
      Inc(Output, CN_SM4_BLOCKSIZE);
      Dec(Length, CN_SM4_BLOCKSIZE);
    end;
  end;
end;

procedure SM4CryptCfb(var Ctx: TCnSM4Context; Mode: Integer; Length: Integer;
  Iv: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar);
var
  I: Integer;
  LocalIv: TCnSM4Iv;
begin
  Move(Iv^, LocalIv[0], CN_SM4_BLOCKSIZE);
  if Mode = SM4_ENCRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= CN_SM4_BLOCKSIZE then
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);  // 先加密 Iv

        for I := 0 to CN_SM4_BLOCKSIZE - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Input) + I))^
            xor (PByte(TCnNativeInt(Output) + I))^;  // 加密结果与明文异或作为输出密文

        Move(Output[0], LocalIv[0], CN_SM4_BLOCKSIZE);  // 密文取代 Iv 以备下一轮
      end
      else
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);

        for I := 0 to Length - 1 do // 只需异或剩余长度，无需处理完整的 16 字节
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Input) + I))^
            xor (PByte(TCnNativeInt(Output) + I))^;
      end;

      Inc(Input, CN_SM4_BLOCKSIZE);
      Inc(Output, CN_SM4_BLOCKSIZE);
      Dec(Length, CN_SM4_BLOCKSIZE);
    end;
  end
  else if Mode = SM4_DECRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= CN_SM4_BLOCKSIZE then
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);   // 先加密 Iv

        for I := 0 to CN_SM4_BLOCKSIZE - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Output) + I))^
            xor (PByte(TCnNativeInt(Input) + I))^;    // 加密结果与密文异或得到明文

        Move(Input[0], LocalIv[0], CN_SM4_BLOCKSIZE);    // 密文取代 Iv 再拿去下一轮加密
      end
      else
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);

        for I := 0 to Length - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Output) + I))^
            xor (PByte(TCnNativeInt(Input) + I))^;
      end;

      Inc(Input, CN_SM4_BLOCKSIZE);
      Inc(Output, CN_SM4_BLOCKSIZE);
      Dec(Length, CN_SM4_BLOCKSIZE);
    end;
  end;
end;

procedure SM4CryptOfb(var Ctx: TCnSM4Context; Mode: Integer; Length: Integer;
  Iv: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar);
var
  I: Integer;
  LocalIv: TCnSM4Iv;
begin
  Move(Iv^, LocalIv[0], CN_SM4_BLOCKSIZE);
  if Mode = SM4_ENCRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= CN_SM4_BLOCKSIZE then
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);  // 先加密 Iv

        Move(Output[0], LocalIv[0], CN_SM4_BLOCKSIZE);  // 加密结果先留存给下一步

        for I := 0 to CN_SM4_BLOCKSIZE - 1 do      // 加密结果与明文异或出密文
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Input) + I))^
            xor (PByte(TCnNativeInt(Output) + I))^;
      end
      else
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);  // 先加密 Iv

        for I := 0 to Length - 1 do             // 无需完整 16 字节
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Input) + I))^
            xor (PByte(TCnNativeInt(Output) + I))^;
      end;

      Inc(Input, CN_SM4_BLOCKSIZE);
      Inc(Output, CN_SM4_BLOCKSIZE);
      Dec(Length, CN_SM4_BLOCKSIZE);
    end;
  end
  else if Mode = SM4_DECRYPT then
  begin
    while Length > 0 do
    begin
      if Length >= CN_SM4_BLOCKSIZE then
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);   // 先加密 Iv

        Move(Output[0], LocalIv[0], CN_SM4_BLOCKSIZE);   // 加密结果先留存给下一步

        for I := 0 to CN_SM4_BLOCKSIZE - 1 do       // 加密内容与密文异或得到明文
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Output) + I))^
            xor (PByte(TCnNativeInt(Input) + I))^;
      end
      else
      begin
        SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], Output);   // 先加密 Iv

        for I := 0 to Length - 1 do
          (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Output) + I))^
            xor (PByte(TCnNativeInt(Input) + I))^;
      end;

      Inc(Input, CN_SM4_BLOCKSIZE);
      Inc(Output, CN_SM4_BLOCKSIZE);
      Dec(Length, CN_SM4_BLOCKSIZE);
    end;
  end;
end;

// CTR 模式加密数据块。Output 长度可以和 Input 一样，不必向上取整
procedure SM4CryptCtr(var Ctx: TCnSM4Context; Mode: Integer; Length: Integer;
  Nonce: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar);
var
  I: Integer;
  LocalIv: TCnSM4Iv;
  Cnt, T: Int64;
begin
  Cnt := 1;

  // 不区分加解密
  while Length > 0 do
  begin
    if Length >= CN_SM4_BLOCKSIZE then
    begin
      Move(Nonce^, LocalIv[0], SizeOf(TCnSM4Nonce));
      T := Int64HostToNetwork(Cnt);
      Move(T, LocalIv[SizeOf(TCnSM4Nonce)], SizeOf(Int64));

      SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], @LocalIv[0]);  // 先加密 Iv

      for I := 0 to CN_SM4_BLOCKSIZE - 1 do      // 加密结果与明文异或出密文
        (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Input) + I))^
          xor LocalIv[I];
    end
    else
    begin
      Move(Nonce^, LocalIv[0], SizeOf(TCnSM4Nonce));
      T := Int64HostToNetwork(Cnt);
      Move(T, LocalIv[SizeOf(TCnSM4Nonce)], SizeOf(Int64));

      SM4OneRound(@(Ctx.Sk[0]), @LocalIv[0], @LocalIv[0]);  // 先加密 Iv

      for I := 0 to Length - 1 do             // 无需完整 16 字节
        (PByte(TCnNativeInt(Output) + I))^ := (PByte(TCnNativeInt(Input) + I))^
          xor LocalIv[I];
    end;

    Inc(Input, CN_SM4_BLOCKSIZE);
    Inc(Output, CN_SM4_BLOCKSIZE);
    Dec(Length, CN_SM4_BLOCKSIZE);
    Inc(Cnt);
  end;
end;

procedure SM4CryptCbcStr(Mode: Integer; Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  Ctx: TCnSM4Context;
begin
  if Length(Key) < CN_SM4_KEYSIZE then
    while Length(Key) < CN_SM4_KEYSIZE do Key := Key + Chr(0) // 16 bytes at least padding 0.
  else if Length(Key) > CN_SM4_KEYSIZE then
    Key := Copy(Key, 1, CN_SM4_KEYSIZE);  // Only keep 16

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1]));
    SM4CryptCbc(Ctx, SM4_ENCRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyDec(Ctx, @(Key[1]));
    SM4CryptCbc(Ctx, SM4_DECRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end;
end;

procedure SM4CryptCfbStr(Mode: Integer; Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  Ctx: TCnSM4Context;
begin
  if Length(Key) < CN_SM4_KEYSIZE then
    while Length(Key) < CN_SM4_KEYSIZE do Key := Key + Chr(0) // 16 bytes at least padding 0.
  else if Length(Key) > CN_SM4_KEYSIZE then
    Key := Copy(Key, 1, CN_SM4_KEYSIZE);  // Only keep 16

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1]));
    SM4CryptCfb(Ctx, SM4_ENCRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1])); // 注意 CFB 的解密也用的是加密！
    SM4CryptCfb(Ctx, SM4_DECRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end;
end;

procedure SM4CryptOfbStr(Mode: Integer; Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  Ctx: TCnSM4Context;
begin
  if Length(Key) < CN_SM4_KEYSIZE then
    while Length(Key) < CN_SM4_KEYSIZE do Key := Key + Chr(0) // 16 bytes at least padding 0.
  else if Length(Key) > CN_SM4_KEYSIZE then
    Key := Copy(Key, 1, CN_SM4_KEYSIZE);  // Only keep 16

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1]));
    SM4CryptOfb(Ctx, SM4_ENCRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1])); // 注意 OFB 的解密也用的是加密！
    SM4CryptOfb(Ctx, SM4_DECRYPT, Length(Input), @(Iv[0]), @(Input[1]), @(Output[0]));
  end;
end;

procedure SM4CryptCtrStr(Mode: Integer; Key: AnsiString; Nonce: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  Ctx: TCnSM4Context;
begin
  if Length(Key) < CN_SM4_KEYSIZE then
    while Length(Key) < CN_SM4_KEYSIZE do Key := Key + Chr(0) // 16 bytes at least padding 0.
  else if Length(Key) > CN_SM4_KEYSIZE then
    Key := Copy(Key, 1, CN_SM4_KEYSIZE);  // Only keep 16

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1]));
    SM4CryptCtr(Ctx, SM4_ENCRYPT, Length(Input), @(Nonce[0]), @(Input[1]), @(Output[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[1])); // 注意 CTR 的解密也用的是加密！
    SM4CryptCtr(Ctx, SM4_DECRYPT, Length(Input), @(Nonce[0]), @(Input[1]), @(Output[0]));
  end;
end;

procedure SM4EncryptEcbStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptEcbStr(SM4_ENCRYPT, Key, Input, Output);
end;

procedure SM4DecryptEcbStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptEcbStr(SM4_DECRYPT, Key, Input, Output);
end;

procedure SM4EncryptCbcStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptCbcStr(SM4_ENCRYPT, Key, Iv, Input, Output);
end;

procedure SM4DecryptCbcStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptCbcStr(SM4_DECRYPT, Key, Iv, Input, Output);
end;

procedure SM4EncryptCfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptCfbStr(SM4_ENCRYPT, Key, Iv, Input, Output);
end;

procedure SM4DecryptCfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptCfbStr(SM4_DECRYPT, Key, Iv, Input, Output);
end;

procedure SM4EncryptOfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptOfbStr(SM4_ENCRYPT, Key, Iv, Input, Output);
end;

procedure SM4DecryptOfbStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptOfbStr(SM4_DECRYPT, Key, Iv, Input, Output);
end;

procedure SM4EncryptCtrStr(Key: AnsiString; Nonce: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptCtrStr(SM4_ENCRYPT, Key, Nonce, Input, Output);
end;

procedure SM4DecryptCtrStr(Key: AnsiString; Nonce: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
begin
  SM4CryptCtrStr(SM4_DECRYPT, Key, Nonce, Input, Output);
end;

function SM4CryptEcbBytes(Mode: Integer; Key: TBytes;
  const Input: TBytes): TBytes;
var
  Ctx: TCnSM4Context;
  I, Len: Integer;
begin
  Len := Length(Input);
  if Len <= 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, (((Len - 1) div 16) + 1) * 16);

  Len := Length(Key);
  if Len < CN_SM4_KEYSIZE then // Key 长度小于 16 字节补 0
  begin
    SetLength(Key, CN_SM4_KEYSIZE);
    for I := Len to CN_SM4_KEYSIZE - 1 do
      Key[I] := 0;
  end;
  // 长度大于 16 字节时 SM4SetKeyEnc 会自动忽略后面的部分

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0]));
    SM4CryptEcb(Ctx, SM4_ENCRYPT, Length(Input), @(Input[0]), @(Result[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyDec(Ctx, @(Key[0]));
    SM4CryptEcb(Ctx, SM4_DECRYPT, Length(Input), @(Input[0]), @(Result[0]));
  end;
end;

function SM4CryptCbcBytes(Mode: Integer; Key, Iv: TBytes;
  const Input: TBytes): TBytes;
var
  Ctx: TCnSM4Context;
  I, Len: Integer;
begin
  Len := Length(Input);
  if Len <= 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, (((Len - 1) div 16) + 1) * 16);

  Len := Length(Key);
  if Len < CN_SM4_KEYSIZE then // Key 长度小于 16 字节补 0
  begin
    SetLength(Key, CN_SM4_KEYSIZE);
    for I := Len to CN_SM4_KEYSIZE - 1 do
      Key[I] := 0;
  end;
  // 长度大于 16 字节时 SM4SetKeyEnc 会自动忽略后面的部分

  Len := Length(Iv);
  if Len < CN_SM4_BLOCKSIZE then // Iv 长度小于 16 字节补 0
  begin
    SetLength(Iv, CN_SM4_BLOCKSIZE);
    for I := Len to CN_SM4_BLOCKSIZE - 1 do
      Iv[I] := 0;
  end;

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0]));
    SM4CryptCbc(Ctx, SM4_ENCRYPT, Length(Input), @(Iv[0]), @(Input[0]), @(Result[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyDec(Ctx, @(Key[0]));
    SM4CryptCbc(Ctx, SM4_DECRYPT, Length(Input), @(Iv[0]), @(Input[0]), @(Result[0]));
  end;
end;

function SM4CryptCfbBytes(Mode: Integer; Key, Iv: TBytes;
  const Input: TBytes): TBytes;
var
  Ctx: TCnSM4Context;
  I, Len: Integer;
begin
  Len := Length(Input);
  if Len <= 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, (((Len - 1) div 16) + 1) * 16);

  Len := Length(Key);
  if Len < CN_SM4_KEYSIZE then // Key 长度小于 16 字节补 0
  begin
    SetLength(Key, CN_SM4_KEYSIZE);
    for I := Len to CN_SM4_KEYSIZE - 1 do
      Key[I] := 0;
  end;
  // 长度大于 16 字节时 SM4SetKeyEnc 会自动忽略后面的部分

  Len := Length(Iv);
  if Len < CN_SM4_BLOCKSIZE then // Iv 长度小于 16 字节补 0
  begin
    SetLength(Iv, CN_SM4_BLOCKSIZE);
    for I := Len to CN_SM4_BLOCKSIZE - 1 do
      Iv[I] := 0;
  end;

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0]));
    SM4CryptCfb(Ctx, SM4_ENCRYPT, Length(Input), @(Iv[0]), @(Input[0]), @(Result[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0])); // 注意 CFB 的解密也用的是加密！
    SM4CryptCfb(Ctx, SM4_DECRYPT, Length(Input), @(Iv[0]), @(Input[0]), @(Result[0]));
  end;
end;

function SM4CryptOfbBytes(Mode: Integer; Key, Iv: TBytes;
  const Input: TBytes): TBytes;
var
  Ctx: TCnSM4Context;
  I, Len: Integer;
begin
  Len := Length(Input);
  if Len <= 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, (((Len - 1) div 16) + 1) * 16);

  Len := Length(Key);
  if Len < CN_SM4_KEYSIZE then // Key 长度小于 16 字节补 0
  begin
    SetLength(Key, CN_SM4_KEYSIZE);
    for I := Len to CN_SM4_KEYSIZE - 1 do
      Key[I] := 0;
  end;
  // 长度大于 16 字节时 SM4SetKeyEnc 会自动忽略后面的部分

  Len := Length(Iv);
  if Len < CN_SM4_BLOCKSIZE then // Iv 长度小于 16 字节补 0
  begin
    SetLength(Iv, CN_SM4_BLOCKSIZE);
    for I := Len to CN_SM4_BLOCKSIZE - 1 do
      Iv[I] := 0;
  end;

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0]));
    SM4CryptOfb(Ctx, SM4_ENCRYPT, Length(Input), @(Iv[0]), @(Input[0]), @(Result[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0])); // 注意 OFB 的解密也用的是加密！
    SM4CryptOfb(Ctx, SM4_DECRYPT, Length(Input), @(Iv[0]), @(Input[0]), @(Result[0]));
  end;
end;

function SM4CryptCtrBytes(Mode: Integer; Key, Nonce: TBytes;
  const Input: TBytes): TBytes;
var
  Ctx: TCnSM4Context;
  I, Len: Integer;
begin
  Len := Length(Input);
  if Len <= 0 then
  begin
    Result := nil;
    Exit;
  end;
  SetLength(Result, Len);

  Len := Length(Key);
  if Len < CN_SM4_KEYSIZE then // Key 长度小于 16 字节补 0
  begin
    SetLength(Key, CN_SM4_KEYSIZE);
    for I := Len to CN_SM4_KEYSIZE - 1 do
      Key[I] := 0;
  end;
  // 长度大于 16 字节时 SM4SetKeyEnc 会自动忽略后面的部分

  Len := Length(Nonce);
  if Len < CN_SM4_NONCESIZE then // Nonce 长度小于 16 字节补 0
  begin
    SetLength(Nonce, CN_SM4_NONCESIZE);
    for I := Len to CN_SM4_NONCESIZE - 1 do
      Nonce[I] := 0;
  end;

  if Mode = SM4_ENCRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0]));
    SM4CryptCtr(Ctx, SM4_ENCRYPT, Length(Input), @(Nonce[0]), @(Input[0]), @(Result[0]));
  end
  else if Mode = SM4_DECRYPT then
  begin
    SM4SetKeyEnc(Ctx, @(Key[0])); // 注意 CTR 的解密也用的是加密！
    SM4CryptCtr(Ctx, SM4_DECRYPT, Length(Input), @(Nonce[0]), @(Input[0]), @(Result[0]));
  end;
end;

function SM4EncryptEcbBytes(Key: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptEcbBytes(SM4_ENCRYPT, Key, Input);
end;

function SM4DecryptEcbBytes(Key: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptEcbBytes(SM4_DECRYPT, Key, Input);
end;

function SM4EncryptCbcBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptCbcBytes(SM4_ENCRYPT, Key, Iv, Input);
end;

function SM4DecryptCbcBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptCbcBytes(SM4_DECRYPT, Key, Iv, Input);
end;

function SM4EncryptCfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptCfbBytes(SM4_ENCRYPT, Key, Iv, Input);
end;

function SM4DecryptCfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptCfbBytes(SM4_DECRYPT, Key, Iv, Input);
end;

function SM4EncryptOfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptOfbBytes(SM4_ENCRYPT, Key, Iv, Input);
end;

function SM4DecryptOfbBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptOfbBytes(SM4_DECRYPT, Key, Iv, Input);
end;

function SM4EncryptCtrBytes(Key, Nonce: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptCtrBytes(SM4_ENCRYPT, Key, Nonce, Input);
end;

function SM4DecryptCtrBytes(Key, Nonce: TBytes; const Input: TBytes): TBytes;
begin
  Result := SM4CryptCtrBytes(SM4_DECRYPT, Key, Nonce, Input);
end;

function SM4EncryptEcbBytesToHex(Key: TBytes; const Input: TBytes): AnsiString;
begin
  Result := AnsiString(BytesToHex(SM4EncryptEcbBytes(Key, Input)));
end;

function SM4DecryptEcbBytesFromHex(Key: TBytes; const Input: AnsiString): TBytes;
begin
  Result := SM4DecryptEcbBytes(Key, HexToBytes(string(Input)));
end;

function SM4EncryptCbcBytesToHex(Key, Iv: TBytes; const Input: TBytes): AnsiString;
begin
  Result := AnsiString(BytesToHex(SM4EncryptCbcBytes(Key, Iv, Input)));
end;

function SM4DecryptCbcBytesFromHex(Key, Iv: TBytes; const Input: AnsiString): TBytes;
begin
  Result := SM4DecryptCbcBytes(Key, Iv, HexToBytes(string(Input)));
end;

function SM4EncryptCfbBytesToHex(Key, Iv: TBytes; const Input: TBytes): AnsiString;
begin
  Result := AnsiString(BytesToHex(SM4EncryptCfbBytes(Key, Iv, Input)));
end;

function SM4DecryptCfbBytesFromHex(Key, Iv: TBytes; const Input: AnsiString): TBytes;
begin
  Result := SM4DecryptCfbBytes(Key, Iv, HexToBytes(string(Input)));
end;

function SM4EncryptOfbBytesToHex(Key, Iv: TBytes; const Input: TBytes): AnsiString;
begin
  Result := AnsiString(BytesToHex(SM4EncryptOfbBytes(Key, Iv, Input)));
end;

function SM4DecryptOfbBytesFromHex(Key, Iv: TBytes; const Input: AnsiString): TBytes;
begin
  Result := SM4DecryptOfbBytes(Key, Iv, HexToBytes(string(Input)));
end;

function SM4EncryptCtrBytesToHex(Key, Nonce: TBytes; const Input: TBytes): AnsiString;
begin
  Result := AnsiString(BytesToHex(SM4EncryptCtrBytes(Key, Nonce, Input)));
end;

function SM4DecryptCtrBytesFromHex(Key, Nonce: TBytes; const Input: AnsiString): TBytes;
begin
  Result := SM4DecryptCtrBytes(Key, Nonce, HexToBytes(string(Input)));
end;

procedure SM4EncryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  SM4SetKeyEnc(Ctx, @(Key[0]));
  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    SM4OneRound(@(Ctx.Sk[0]), @(TempIn[0]), @(TempOut[0]));

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorSM4WriteError);

    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then // 尾部补 0
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);

    SM4OneRound(@(Ctx.Sk[0]), @(TempIn[0]), @(TempOut[0]));

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4DecryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;
  if (Count mod SizeOf(TCnSM4Buffer)) > 0 then
    raise Exception.Create(SCnErrorSM4InvalidInBufSize);

  SM4SetKeyDec(Ctx, @(Key[0]));
  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    SM4OneRound(@(Ctx.Sk[0]), @(TempIn[0]), @(TempOut[0]));

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorSM4WriteError);

    Dec(Count, SizeOf(TCnSM4Buffer));
  end;
end;

procedure SM4EncryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  Vector := InitVector;
  SM4SetKeyEnc(Ctx, @(Key[0]));

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^;

    SM4OneRound(@(Ctx.Sk[0]), @(TempIn[0]), @(TempOut[0]));

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorSM4WriteError);

    Move(TempOut[0], Vector[0], SizeOf(TCnSM4Iv));
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@Vector[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@Vector[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@Vector[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@Vector[12])^;

    SM4OneRound(@(Ctx.Sk[0]), @(TempIn[0]), @(TempOut[0]));

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4DecryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector1, Vector2: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;
  if (Count mod SizeOf(TCnSM4Buffer)) > 0 then
    raise Exception.Create(SCnErrorSM4InvalidInBufSize);

  Vector1 := InitVector;
  SM4SetKeyDec(Ctx, @(Key[0]));

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorSM4ReadError);

    Move(TempIn[0], Vector2[0], SizeOf(TCnSM4Iv));
    SM4OneRound(@(Ctx.Sk[0]), @(TempIn[0]), @(TempOut[0]));

    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@Vector1[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@Vector1[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@Vector1[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@Vector1[12])^;

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorSM4WriteError);

    Vector1 := Vector2;
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;
end;

procedure SM4EncryptStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  Vector := InitVector;
  SM4SetKeyEnc(Ctx, @(Key[0]));

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));     // Key 先加密 Iv

    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // 加密结果与明文异或
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempOut, SizeOf(TempOut));   // 异或的结果写进密文结果
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SCnErrorSM4WriteError);

    Move(TempOut[0], Vector[0], SizeOf(TCnSM4Iv));    // 密文结果取代 Iv 供下一轮加密
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);
    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));

    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempOut, Count);  // 最后写入的只包括密文长度的部分，无需整个块
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4DecryptStreamCFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  Vector := InitVector;
  SM4SetKeyEnc(Ctx, @(Key[0]));  // 注意是加密！不是解密！

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));             // 密文读入至 TempIn
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorSM4ReadError);
    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0])); // Iv 先加密至 TempOut

    // 加密后的内容 TempOut 和密文 TempIn 异或得到明文 TempOut
    PCardinal(@TempOut[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;

    Done := Dest.Write(TempOut, SizeOf(TempOut));      // 明文 TempOut 写出去
    if Done < SizeOf(TempOut) then
      raise EStreamError(SCnErrorSM4WriteError);
    Move(TempIn[0], Vector[0], SizeOf(TCnSM4Iv));       // 保留密文 TempIn 取代 Iv 作为下一次加密再异或的内容
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);
    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));

    PCardinal(@TempOut[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempOut[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempOut[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempOut[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempOut, Count);  // 最后写入的只包括密文长度的部分，无需整个块
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4EncryptStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  Vector := InitVector;
  SM4SetKeyEnc(Ctx, @(Key[0]));

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));     // Key 先加密 Iv

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // 加密结果与明文异或
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempIn, SizeOf(TempIn));     // 异或的结果写进密文结果
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4WriteError);

    Move(TempOut[0], Vector[0], SizeOf(TCnSM4Iv));    // 加密结果取代 Iv 供下一轮加密，注意不是异或结果
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);
    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempIn, Count);  // 最后写入的只包括密文长度的部分，无需整个块
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4DecryptStreamOFB(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Iv; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  Vector := InitVector;
  SM4SetKeyEnc(Ctx, @(Key[0]));  // 注意是加密！不是解密！

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));             // 密文读入至 TempIn
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorSM4ReadError);
    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));  // Iv 先加密至 TempOut

    // 加密后的内容 TempOut 和密文 TempIn 异或得到明文 TempIn
    PCardinal(@TempIn[0])^ := PCardinal(@TempOut[0])^ xor PCardinal(@TempIn[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempOut[4])^ xor PCardinal(@TempIn[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempOut[8])^ xor PCardinal(@TempIn[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempOut[12])^ xor PCardinal(@TempIn[12])^;

    Done := Dest.Write(TempIn, SizeOf(TempIn));        // 明文 TempIn 写出去
    if Done < SizeOf(TempIn) then
      raise EStreamError(SCnErrorSM4WriteError);
    Move(TempOut[0], Vector[0], SizeOf(TCnSM4Iv));       // 保留加密结果 TempOut 取代 Iv 作为下一次加密再异或的内容
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);
    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempOut, Count);  // 最后写入的只包括密文长度的部分，无需整个块
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4EncryptStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Nonce; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
  Cnt, T: Int64;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  Cnt := 1;
  SM4SetKeyEnc(Ctx, @(Key[0]));

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    // Nonce 和计数器拼成 Iv
    T := Int64HostToNetwork(Cnt);
    Move(InitVector[0], Vector[0], SizeOf(TCnSM4Nonce));
    Move(T, Vector[SizeOf(TCnSM4Nonce)], SizeOf(Int64));

    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));     // Key 先加密 Iv

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // 加密结果与明文异或
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempIn, SizeOf(TempIn));   // 异或的结果写进密文结果
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4WriteError);

    Inc(Cnt);
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    // Nonce 和计数器拼成 Iv
    T := Int64HostToNetwork(Cnt);
    Move(InitVector[0], Vector[0], SizeOf(TCnSM4Nonce));
    Move(T, Vector[SizeOf(TCnSM4Nonce)], SizeOf(Int64));

    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempIn, Count);  // 最后写入的只包括密文长度的部分，无需整个块
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4DecryptStreamCTR(Source: TStream; Count: Cardinal;
  const Key: TCnSM4Key; const InitVector: TCnSM4Nonce; Dest: TStream);
var
  TempIn, TempOut: TCnSM4Buffer;
  Vector: TCnSM4Iv;
  Done: Cardinal;
  Ctx: TCnSM4Context;
  Cnt, T: Int64;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else
    Count := Min(Count, Source.Size - Source.Position);

  if Count = 0 then
    Exit;

  Cnt := 1;
  SM4SetKeyEnc(Ctx, @(Key[0]));    // 注意是加密！不是解密！

  while Count >= SizeOf(TCnSM4Buffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    // Nonce 和计数器拼成 Iv
    T := Int64HostToNetwork(Cnt);
    Move(InitVector[0], Vector[0], SizeOf(TCnSM4Nonce));
    Move(T, Vector[SizeOf(TCnSM4Nonce)], SizeOf(Int64));

    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));     // Key 先加密 Iv

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;  // 加密结果与密文异或
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempIn, SizeOf(TempIn));   // 异或的结果写进明文结果
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SCnErrorSM4WriteError);

    Inc(Cnt);
    Dec(Count, SizeOf(TCnSM4Buffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4ReadError);

    // Nonce 和计数器拼成 Iv
    T := Int64HostToNetwork(Cnt);
    Move(InitVector[0], Vector[0], SizeOf(TCnSM4Nonce));
    Move(T, Vector[SizeOf(TCnSM4Nonce)], SizeOf(Int64));

    SM4OneRound(@(Ctx.Sk[0]), @(Vector[0]), @(TempOut[0]));

    PCardinal(@TempIn[0])^ := PCardinal(@TempIn[0])^ xor PCardinal(@TempOut[0])^;
    PCardinal(@TempIn[4])^ := PCardinal(@TempIn[4])^ xor PCardinal(@TempOut[4])^;
    PCardinal(@TempIn[8])^ := PCardinal(@TempIn[8])^ xor PCardinal(@TempOut[8])^;
    PCardinal(@TempIn[12])^ := PCardinal(@TempIn[12])^ xor PCardinal(@TempOut[12])^;

    Done := Dest.Write(TempIn, Count);  // 最后写入的只包括密文长度的部分，无需整个块
    if Done < Count then
      raise EStreamError.Create(SCnErrorSM4WriteError);
  end;
end;

procedure SM4Encrypt(Key: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar; Len: Integer);
var
  Ctx: TCnSM4Context;
begin
  SM4SetKeyEnc(Ctx, Key);
  SM4CryptEcb(Ctx, SM4_ENCRYPT, Len, Input, Output);
end;

procedure SM4Decrypt(Key: PAnsiChar; Input: PAnsiChar; Output: PAnsiChar; Len: Integer);
var
  Ctx: TCnSM4Context;
begin
  SM4SetKeyDec(Ctx, Key);
  SM4CryptEcb(Ctx, SM4_DECRYPT, Len, Input, Output);
end;

end.
