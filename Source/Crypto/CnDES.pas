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

unit CnDES;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：DES 算法单元
* 单元作者：匿名/佚名
* 备    注：由匿名作者搜集整理而来
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2021.02.07 V1.4
*               增加对 TBytes 的支持
*           2020.03.25 V1.3
*               增加 3DES 的支持
*           2020.03.24 V1.2
*               增加 ECB/CBC 字符串与流加解密函数，删除原有的字符串加密函数
*           2019.04.15 V1.1
*               支持 Win32/Win64/MacOS
*           2008.05.30 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

const
  DES_KEYSIZE = 8;
  DES_BLOCKSIZE = 8;

  TRIPLE_DES_KEYSIZE = DES_KEYSIZE * 3;
  TRIPLE_DES_BLOCKSIZE = DES_BLOCKSIZE;

type
  TDESKey = array[0..DES_KEYSIZE - 1] of Byte;
  {* DES 的加密 Key}

  TDESBuffer = array[0..DES_BLOCKSIZE - 1] of Byte;
  {* DES 的加密块}

  TDESIv  = array[0..DES_BLOCKSIZE - 1] of Byte;
  {* DES 的 CBC 的初始化向量}

  T3DESKey = array[0..TRIPLE_DES_KEYSIZE - 1] of Byte;
  {* 3DES 的密码}

  T3DESBuffer = TDESBuffer;
  {* 3DES 的加密块，等于 DES 的加密块}

  T3DESIv = TDESIv;
  {* 3DES 的 CBC 的初始化向量，等于 DES 的 CBC 的初始化向量}

// ================================= DES =======================================

procedure DESEncryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
{* DES-ECB 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Input    input 字符串，其长度如不是 8 倍数，计算时会被填充 #0 至长度达到 8 的倍数
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

procedure DESDecryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
{* DES-ECB 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Input    input 字符串，其长度如不是 8 倍数，计算时会被填充 #0 至长度达到 8 的倍数
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

procedure DESEncryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* DES-CBC 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Iv       8 字节初始化向量，运算过程中会改变，因此调用者需要保存原始数据
  Input    input string
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

procedure DESDecryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* DES-CBC 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Iv       8 字节初始化向量，运算过程中会改变，因此调用者需要保存原始数据
  Input    input string
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function DESEncryptStrToHex(const Str, Key: AnsiString): AnsiString;
{* 传入明文与加密 Key，DES 加密返回转换成十六进制的密文，ECB 模式，明文末尾可能补 0}

function DESDecryptStrFromHex(const StrHex, Key: AnsiString): AnsiString;
{* 传入十六进制的密文与加密 Key，DES ECB 解密返回明文}

{$IFDEF TBYTES_DEFINED}

function DESEncryptECBBytes(Key: TBytes; Input: TBytes): TBytes;
{* DES-ECB 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 0
  Input    输入内容，其长度如不是 8 倍数，计算时会被填充 0 至长度达到 8 的倍数
  返回值   密文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function DESDecryptECBBytes(Key: TBytes; const Input: TBytes): TBytes;
{* DES-ECB 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 0
  Input    待解密密文
  返回值   明文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function DESEncryptCBCBytes(Key, Iv: TBytes; Input: TBytes): TBytes;
{* DES-CBC 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Iv       8 字节初始化向量，太长则截断，不足则补 0，运算过程中会改变，因此调用者需要保存原始数据
  Input    输入内容，其长度如不是 8 倍数，计算时会被填充 0 至长度达到 8 的倍数
  返回值   密文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function DESDecryptCBCBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* DES-CBC 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 0
  Iv       8 字节初始化向量，太长则截断，不足则补 0，运算过程中会改变，因此调用者需要保存原始数据
  Input    待解密密文
  返回值   明文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

{$ENDIF}

procedure DESEncryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TDESKey; Dest: TStream); overload;
{* DES-ECB 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure DESDecryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TDESKey; Dest: TStream); overload;
{* DES-ECB 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

procedure DESEncryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TDESKey; const InitVector: TDESIv; Dest: TStream); overload;
{* DES-CBC 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure DESDecryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TDESKey; const InitVector: TDESIv; Dest: TStream); overload;
{* DES-CBC 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

// =========================== 3-DES (Triple DES) ==============================

procedure TripleDESEncryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
{* 3DES-ECB 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Input    input 字符串，其长度如不是 8 倍数，计算时会被填充 #0 至长度达到 8 的倍数
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

procedure TripleDESDecryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
{* 3DES-ECB 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Input    input 字符串，其长度如不是 8 倍数，计算时会被填充 #0 至长度达到 8 的倍数
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

procedure TripleDESEncryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* 3DES-CBC 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Iv       8 字节初始化向量，运算过程中会改变，因此调用者需要保存原始数据
  Input    input string
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

procedure TripleDESDecryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
{* 3DES-CBC 封装好的针对 AnsiString 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Iv       8 字节初始化向量，运算过程中会改变，因此调用者需要保存原始数据
  Input    input string
  Output   output 输出区，其长度必须大于或等于 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function TripleDESEncryptStrToHex(const Str, Key: AnsiString): AnsiString;
{* 传入明文与加密 Key，3DES 加密返回转换成十六进制的密文，ECB 模式，明文末尾可能补 0}

function TripleDESDecryptStrFromHex(const StrHex, Key: AnsiString): AnsiString;
{* 传入十六进制的密文与加密 Key，3DES ECB 解密返回明文}

{$IFDEF TBYTES_DEFINED}

function TripleDESEncryptECBBytes(Key: TBytes; Input: TBytes): TBytes;
{* 3DES-ECB 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 0
  Input    输入内容，其长度如不是 8 倍数，计算时会被填充 0 至长度达到 8 的倍数
  返回值   密文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function TripleDESDecryptECBBytes(Key: TBytes; const Input: TBytes): TBytes;
{* 3DES-ECB 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 0
  Input    待解密密文
  返回值   明文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function TripleDESEncryptCBCBytes(Key, Iv: TBytes; Input: TBytes): TBytes;
{* 3DES-CBC 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 #0
  Iv       8 字节初始化向量，太长则截断，不足则补 0，运算过程中会改变，因此调用者需要保存原始数据
  Input    输入内容，其长度如不是 8 倍数，计算时会被填充 0 至长度达到 8 的倍数
  返回值   密文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

function TripleDESDecryptCBCBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
{* 3DES-CBC 封装好的针对 TBytes 的加解密方法
 |<PRE>
  Key      8 字节密码，太长则截断，不足则补 0
  Iv       8 字节初始化向量，太长则截断，不足则补 0，运算过程中会改变，因此调用者需要保存原始数据
  Input    待解密密文
  返回值   明文，其长度被设置为 (((Length(Input) - 1) div 8) + 1) * 8
 |</PRE>}

{$ENDIF}

procedure TripleDESEncryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; Dest: TStream); overload;
{* 3DES-ECB 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure TripleDESDecryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; Dest: TStream); overload;
{* 3DES-ECB 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

procedure TripleDESEncryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; const InitVector: TDESIv; Dest: TStream); overload;
{* 3DES-CBC 流加密，Count 为 0 表示从头加密整个流，否则只加密 Stream 当前位置起 Count 的字节数}

procedure TripleDESDecryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; const InitVector: TDESIv; Dest: TStream); overload;
{* 3DES-CBC 流解密，Count 为 0 表示从头解密整个流，否则只解密 Stream 当前位置起 Count 的字节数}

implementation

resourcestring
  SInvalidInBufSize = 'Invalid Buffer Size for Decryption';
  SReadError = 'Stream Read Error';
  SWriteError = 'Stream Write Error';

type
  PLongWord = ^LongWord;
  TKeyByte = array[0..5] of Byte;
  TDesMode = (dmEncry, dmDecry);
  TSubKey = array[0..15] of TKeyByte;

const
  BitIP: array[0..63] of Byte =
  (57, 49, 41, 33, 25, 17, 9, 1,
    59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5,
    63, 55, 47, 39, 31, 23, 15, 7,
    56, 48, 40, 32, 24, 16, 8, 0,
    58, 50, 42, 34, 26, 18, 10, 2,
    60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6);

  BitCP: array[0..63] of Byte =
  (39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30,
    37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28,
    35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26,
    33, 1, 41, 9, 49, 17, 57, 25,
    32, 0, 40, 8, 48, 16, 56, 24);

  BitExp: array[0..47] of Integer =
  (31, 0, 1, 2, 3, 4, 3, 4, 5, 6, 7, 8, 7, 8, 9, 10,
    11, 12, 11, 12, 13, 14, 15, 16, 15, 16, 17, 18, 19, 20, 19, 20,
    21, 22, 23, 24, 23, 24, 25, 26, 27, 28, 27, 28, 29, 30, 31, 0);

  BitPM: array[0..31] of Byte =
  (15, 6, 19, 20, 28, 11, 27, 16, 0, 14, 22, 25, 4, 17, 30, 9,
    1, 7, 23, 13, 31, 26, 2, 8, 18, 12, 29, 5, 21, 10, 3, 24);

  sBox: array[0..7] of array[0..63] of Byte =
  ((14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7,
    0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8,
    4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0,
    15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13),

    (15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10,
    3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5,
    0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15,
    13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9),

    (10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8,
    13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1,
    13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7,
    1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12),

    (7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15,
    13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9,
    10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4,
    3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14),

    (2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9,
    14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6,
    4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14,
    11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3),

    (12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11,
    10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8,
    9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6,
    4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13),

    (4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1,
    13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6,
    1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2,
    6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12),

    (13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7,
    1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2,
    7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8,
    2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11));

  BitPMC1: array[0..55] of Byte =
  (56, 48, 40, 32, 24, 16, 8,
    0, 57, 49, 41, 33, 25, 17,
    9, 1, 58, 50, 42, 34, 26,
    18, 10, 2, 59, 51, 43, 35,
    62, 54, 46, 38, 30, 22, 14,
    6, 61, 53, 45, 37, 29, 21,
    13, 5, 60, 52, 44, 36, 28,
    20, 12, 4, 27, 19, 11, 3);

  BitPMC2: array[0..47] of Byte =
  (13, 16, 10, 23, 0, 4,
    2, 27, 14, 5, 20, 9,
    22, 18, 11, 3, 25, 7,
    15, 6, 26, 19, 12, 1,
    40, 51, 30, 36, 46, 54,
    29, 39, 50, 44, 32, 47,
    43, 48, 38, 55, 33, 52,
    45, 41, 49, 35, 28, 31);

function Min(A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

procedure InitPermutation(var InData: array of Byte);
var
  NewData: array[0..7] of Byte;
  I: Integer;
begin
  FillChar(NewData, 8, 0);
  for I := 0 to 63 do
    if (InData[BitIP[I] shr 3] and (1 shl (7 - (BitIP[I] and $07)))) <> 0 then
      NewData[I shr 3] := NewData[I shr 3] or (1 shl (7 - (I and $07)));
  for I := 0 to 7 do InData[I] := NewData[I];
end;

procedure ConversePermutation(var InData: array of Byte);
var
  NewData: array[0..7] of Byte;
  I: Integer;
begin
  FillChar(NewData, 8, 0);
  for I := 0 to 63 do
    if (InData[BitCP[I] shr 3] and (1 shl (7 - (BitCP[I] and $07)))) <> 0 then
      NewData[I shr 3] := NewData[I shr 3] or (1 shl (7 - (I and $07)));
  for I := 0 to 7 do InData[I] := NewData[I];
end;

procedure Expand(InData: array of Byte; var OutData: array of Byte);
var
  I: Integer;
begin
  FillChar(OutData, 6, 0);
  for I := 0 to 47 do
    if (InData[BitExp[I] shr 3] and (1 shl (7 - (BitExp[I] and $07)))) <> 0 then
      OutData[I shr 3] := OutData[I shr 3] or (1 shl (7 - (I and $07)));
end;

procedure Permutation(var InData: array of Byte);
var
  NewData: array[0..3] of Byte;
  I: Integer;
begin
  FillChar(NewData, 4, 0);
  for I := 0 to 31 do
    if (InData[BitPM[I] shr 3] and (1 shl (7 - (BitPM[I] and $07)))) <> 0 then
      NewData[I shr 3] := NewData[I shr 3] or (1 shl (7 - (I and $07)));
  for I := 0 to 3 do InData[I] := NewData[I];
end;

function Si(S, InByte: Byte): Byte;
var
  c: Byte;
begin
  c := (InByte and $20) or ((InByte and $1E) shr 1) or
    ((InByte and $01) shl 4);
  Result := (sBox[S][c] and $0F);
end;

procedure PermutationChoose1(InData: array of Byte; var OutData: array of Byte);
var
  I: Integer;
begin
  FillChar(OutData, 7, 0);
  for I := 0 to 55 do
    if (InData[BitPMC1[I] shr 3] and (1 shl (7 - (BitPMC1[I] and $07)))) <> 0 then
      OutData[I shr 3] := OutData[I shr 3] or (1 shl (7 - (I and $07)));
end;

procedure PermutationChoose2(InData: array of Byte; var OutData: array of Byte);
var
  I: Integer;
begin
  FillChar(OutData, 6, 0);
  for I := 0 to 47 do
    if (InData[BitPMC2[I] shr 3] and (1 shl (7 - (BitPMC2[I] and $07)))) <> 0 then
      OutData[I shr 3] := OutData[I shr 3] or (1 shl (7 - (I and $07)));
end;

procedure CycleMove(var InData: array of Byte; bitMove: Byte);
var
  I: Integer;
begin
  for I := 0 to bitMove - 1 do
  begin
    InData[0] := (InData[0] shl 1) or (InData[1] shr 7);
    InData[1] := (InData[1] shl 1) or (InData[2] shr 7);
    InData[2] := (InData[2] shl 1) or (InData[3] shr 7);
    InData[3] := (InData[3] shl 1) or ((InData[0] and $10) shr 4);
    InData[0] := (InData[0] and $0F);
  end;
end;

procedure MakeKey(InKey: array of Byte; var OutKey: array of TKeyByte);
const
  bitDisplace: array[0..15] of Byte =
    (1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1);
var
  OutData56: array[0..6] of Byte;
  Key28l: array[0..3] of Byte;
  Key28r: array[0..3] of Byte;
  Key56o: array[0..6] of Byte;
  I: Integer;
begin
  PermutationChoose1(InKey, OutData56);
  Key28l[0] := OutData56[0] shr 4;
  Key28l[1] := (OutData56[0] shl 4) or (OutData56[1] shr 4);
  Key28l[2] := (OutData56[1] shl 4) or (OutData56[2] shr 4);
  Key28l[3] := (OutData56[2] shl 4) or (OutData56[3] shr 4);
  Key28r[0] := OutData56[3] and $0F;
  Key28r[1] := OutData56[4];
  Key28r[2] := OutData56[5];
  Key28r[3] := OutData56[6];
  for I := 0 to 15 do
  begin
    CycleMove(Key28l, bitDisplace[I]);
    CycleMove(Key28r, bitDisplace[I]);
    Key56o[0] := (Key28l[0] shl 4) or (Key28l[1] shr 4);
    Key56o[1] := (Key28l[1] shl 4) or (Key28l[2] shr 4);
    Key56o[2] := (Key28l[2] shl 4) or (Key28l[3] shr 4);
    Key56o[3] := (Key28l[3] shl 4) or (Key28r[0]);
    Key56o[4] := Key28r[1];
    Key56o[5] := Key28r[2];
    Key56o[6] := Key28r[3];
    PermutationChoose2(Key56o, OutKey[I]);
  end;
end;

procedure Encry(InData, ASubKey: array of Byte; var OutData: array of Byte);
var
  OutBuf: array[0..5] of Byte;
  Buf: array[0..7] of Byte;
  I: Integer;
begin
  Expand(InData, OutBuf);
  for I := 0 to 5 do OutBuf[I] := OutBuf[I] xor ASubKey[I];
  Buf[0] := OutBuf[0] shr 2;
  Buf[1] := ((OutBuf[0] and $03) shl 4) or (OutBuf[1] shr 4);
  Buf[2] := ((OutBuf[1] and $0F) shl 2) or (OutBuf[2] shr 6);
  Buf[3] := OutBuf[2] and $3F;
  Buf[4] := OutBuf[3] shr 2;
  Buf[5] := ((OutBuf[3] and $03) shl 4) or (OutBuf[4] shr 4);
  Buf[6] := ((OutBuf[4] and $0F) shl 2) or (OutBuf[5] shr 6);
  Buf[7] := OutBuf[5] and $3F;
  for I := 0 to 7 do Buf[I] := si(I, Buf[I]);
  for I := 0 to 3 do OutBuf[I] := (Buf[I * 2] shl 4) or Buf[I * 2 + 1];
  Permutation(OutBuf);
  for I := 0 to 3 do OutData[I] := OutBuf[I];
end;

procedure DesData(DesMode: TDesMode; SubKey: TSubKey; InData: array of Byte;
  var OutData: array of Byte);
var
  I, J: Integer;
  Temp, Buf: array[0..3] of Byte;
begin
  for I := 0 to 7 do OutData[I] := InData[I];
  InitPermutation(OutData);
  if DesMode = dmEncry then
  begin
    for I := 0 to 15 do
    begin
      for J := 0 to 3 do Temp[J] := OutData[J];
      for J := 0 to 3 do OutData[J] := OutData[J + 4];
      Encry(OutData, SubKey[I], Buf);
      for J := 0 to 3 do OutData[J + 4] := Temp[J] xor Buf[J];
    end;
    for J := 0 to 3 do Temp[J] := OutData[J + 4];
    for J := 0 to 3 do OutData[J + 4] := OutData[J];
    for J := 0 to 3 do OutData[J] := Temp[J];
  end
  else if DesMode = dmDecry then
  begin
    for I := 15 downto 0 do
    begin
      for J := 0 to 3 do Temp[J] := OutData[J];
      for J := 0 to 3 do OutData[J] := OutData[J + 4];
      Encry(OutData, SubKey[I], Buf);
      for J := 0 to 3 do OutData[J + 4] := Temp[J] xor Buf[J];
    end;
    for J := 0 to 3 do Temp[J] := OutData[J + 4];
    for J := 0 to 3 do OutData[J + 4] := OutData[J];
    for J := 0 to 3 do OutData[J] := Temp[J];
  end;
  ConversePermutation(OutData);
end;

procedure MakeKeyAlign(var Key: AnsiString);
begin
  if Length(Key) < DES_KEYSIZE then
    while Length(Key) < DES_KEYSIZE do
      Key := Key + Chr(0);
end;

procedure MakeInputAlign(var Str: AnsiString);
begin
  while Length(Str) mod DES_KEYSIZE <> 0 do
    Str := Str + Chr(0);
end;

{$IFDEF TBYTES_DEFINED}

procedure MakeKeyBytesAlign(var Key: TBytes);
var
  I, Len: Integer;
begin
  Len := Length(Key);
  if Len < DES_KEYSIZE then
  begin
    SetLength(Key, DES_KEYSIZE);
    for I := Len to DES_KEYSIZE - 1 do
      Key[I] := 0;
  end;
end;

procedure MakeInputBytesAlign(var Input: TBytes);
var
  I, Len, NL: Integer;
begin
  Len := Length(Input);
  if Len mod DES_BLOCKSIZE <> 0 then
  begin
    NL := ((Len div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE;
    SetLength(Input, NL);
    for I := Len to NL - 1 do
      Input[I] := 0;
  end;
end;

{$ENDIF}

procedure DESEncryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  Str: AnsiString;
  I: Integer;
  SubKey: TSubKey;
begin
  MakeKeyAlign(Key);

  Str := Input;
  MakeInputAlign(Str);

  Move(Key[1], KeyByte[0], SizeOf(TDESKey));
  MakeKey(KeyByte, SubKey);

  for I := 0 to Length(Str) div DES_BLOCKSIZE - 1 do
  begin
    Move(Str[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));
    DesData(dmEncry, SubKey, StrByte, OutByte);
    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;
end;

procedure DESDecryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  I: Integer;
  SubKey: TSubKey;
begin
  MakeKeyAlign(Key);
  Move(Key[1], KeyByte[0], SizeOf(TDESKey));
  MakeKey(KeyByte, SubKey);

  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));
    DesData(dmDecry, SubKey, StrByte, OutByte);
    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;

  // 末尾补的 0 由外部判断删除
end;

procedure DESEncryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  Vector: TDESIv;
  Str: AnsiString;
  I: Integer;
  SubKey: TSubKey;
begin
  MakeKeyAlign(Key);

  Str := Input;
  MakeInputAlign(Str);

  Move(Key[1], KeyByte[0], SizeOf(TDESKey));
  MakeKey(KeyByte, SubKey);
  Move(Iv^, Vector[0], SizeOf(TDESIv));

  for I := 0 to Length(Str) div DES_BLOCKSIZE - 1 do
  begin
    Move(Str[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));

    // CBC 数据块的值先跟 Iv 异或
    PLongWord(@StrByte[0])^ := PLongWord(@StrByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@StrByte[4])^ := PLongWord(@StrByte[4])^ xor PLongWord(@Vector[4])^;

    // 再加密
    DesData(dmEncry, SubKey, StrByte, OutByte);
    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 加密结果更新到 Iv
    Move(OutByte[0], Vector[0], SizeOf(TDESIv));
  end;
end;

procedure DESDecryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  Vector, TV: TDESIv;
  I: Integer;
  SubKey: TSubKey;
begin
  MakeKeyAlign(Key);
  Move(Key[1], KeyByte[0], SizeOf(TDESKey));

  MakeKey(KeyByte, SubKey);
  Move(Iv^, Vector[0], SizeOf(TDESIv));

  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));
    Move(StrByte[0], TV[0], SizeOf(TDESIv)); // 密文先存一下

    // 先解密
    DesData(dmDecry, SubKey, StrByte, OutByte);

    // CBC 数据块解密后的值再跟 Iv 异或
    PLongWord(@OutByte[0])^ := PLongWord(@OutByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@OutByte[4])^ := PLongWord(@OutByte[4])^ xor PLongWord(@Vector[4])^;

    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 密文更新到 Iv
    Move(TV[0], Vector[0], SizeOf(TDESIv));
  end;

  // 末尾补的 0 由外部判断删除
end;

procedure SetResultLengthUsingInput(const Str: AnsiString; var Res: AnsiString);
var
  Len: Integer;
begin
  Len := Length(Str);
  if Len < DES_BLOCKSIZE then
    Len := DES_BLOCKSIZE
  else
    Len := (((Len - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE;
  SetLength(Res, Len);
end;

function DESEncryptStrToHex(const Str, Key: AnsiString): AnsiString;
var
  TempResult, Temp: AnsiString;
  I: Integer;
begin
  SetResultLengthUsingInput(Str, TempResult);
  DESEncryptECBStr(Key, Str, @TempResult[1]);

  Result := '';
  for I := 0 to Length(TempResult) - 1 do
  begin
    Temp := AnsiString(Format('%x', [Ord(TempResult[I + 1])]));
    if Length(Temp) = 1 then
      Temp := '0' + Temp;
    Result := Result + Temp;
  end;
end;

function HexToInt(const Hex: AnsiString): Integer;
var
  I, Res: Integer;
  C: AnsiChar;
begin
  Res := 0;
  for I := 0 to Length(Hex) - 1 do
  begin
    C := Hex[I + 1];
    if (C >= '0') and (C <= '9') then
      Res := Res * 16 + Ord(C) - Ord('0')
    else if (C >= 'A') and (C <= 'F') then
      Res := Res * 16 + Ord(C) - Ord('A') + 10
    else if (C >= 'a') and (C <= 'f') then
      Res := Res * 16 + Ord(C) - Ord('a') + 10
    else
      raise Exception.Create('Error: not a Hex String');
  end;
  Result := Res;
end;

function DESDecryptStrFromHex(const StrHex, Key: AnsiString): AnsiString;
var
  Str, Temp: AnsiString;
  I: Integer;
begin
  Str := '';
  for I := 0 to Length(StrHex) div 2 - 1 do
  begin
    Temp := Copy(StrHex, I * 2 + 1, 2);
    Str := Str + AnsiChar(HexToInt(Temp));
  end;

  SetResultLengthUsingInput(Str, Result);
  DESDecryptECBStr(Key, Str, @(Result[1]));
end;

{$IFDEF TBYTES_DEFINED}

function DESEncryptECBBytes(Key: TBytes; Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  I: Integer;
  SubKey: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  MakeKeyBytesAlign(Key);
  MakeInputBytesAlign(Input);

  Move(Key[0], KeyByte[0], SizeOf(TDESKey));
  MakeKey(KeyByte, SubKey);

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));
    DesData(dmEncry, SubKey, StrByte, OutByte);
    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;
end;

function DESDecryptECBBytes(Key: TBytes; const Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  I: Integer;
  SubKey: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  MakeKeyBytesAlign(Key);
  Move(Key[0], KeyByte[0], SizeOf(TDESKey));
  MakeKey(KeyByte, SubKey);

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));
    DesData(dmDecry, SubKey, StrByte, OutByte);
    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;
end;

function DESEncryptCBCBytes(Key, Iv: TBytes; Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  Vector: TDESIv;
  I: Integer;
  SubKey: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  MakeKeyBytesAlign(Key);
  MakeKeyBytesAlign(Iv);

  MakeInputBytesAlign(Input);

  Move(Key[0], KeyByte[0], SizeOf(TDESKey));
  MakeKey(KeyByte, SubKey);
  Move(Iv[0], Vector[0], SizeOf(TDESIv));

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));

    // CBC 数据块的值先跟 Iv 异或
    PLongWord(@StrByte[0])^ := PLongWord(@StrByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@StrByte[4])^ := PLongWord(@StrByte[4])^ xor PLongWord(@Vector[4])^;

    // 再加密
    DesData(dmEncry, SubKey, StrByte, OutByte);
    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 加密结果更新到 Iv
    Move(OutByte[0], Vector[0], SizeOf(TDESIv));
  end;
end;

function DESDecryptCBCBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  KeyByte: TDESKey;
  Vector, TV: TDESIv;
  I: Integer;
  SubKey: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  MakeKeyBytesAlign(Key);
  Move(Key[0], KeyByte[0], SizeOf(TDESKey));

  MakeKey(KeyByte, SubKey);
  MakeKeyBytesAlign(Iv);
  Move(Iv[0], Vector[0], SizeOf(TDESIv));

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));
    Move(StrByte[0], TV[0], SizeOf(TDESIv)); // 密文先存一下

    // 先解密
    DesData(dmDecry, SubKey, StrByte, OutByte);

    // CBC 数据块解密后的值再跟 Iv 异或
    PLongWord(@OutByte[0])^ := PLongWord(@OutByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@OutByte[4])^ := PLongWord(@OutByte[4])^ xor PLongWord(@Vector[4])^;

    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 密文更新到 Iv
    Move(TV[0], Vector[0], SizeOf(TDESIv));
  end;
end;

{$ENDIF}

procedure DESEncryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TDESKey; Dest: TStream); overload;
var
  TempIn, TempOut: TDESBuffer;
  Done: Cardinal;
  SubKey: TSubKey;
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

  MakeKey(Key, SubKey);
  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    DesData(dmEncry, SubKey, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    Dec(Count, SizeOf(TDESBuffer));
  end;

  if Count > 0 then // 尾部补 0
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);

    DesData(dmEncry, SubKey, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
  end;
end;

procedure DESDecryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: TDESKey; Dest: TStream); overload;
var
  TempIn, TempOut: TDESBuffer;
  Done: Cardinal;
  SubKey: TSubKey;
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
  if (Count mod SizeOf(TDESBuffer)) > 0 then
    raise Exception.Create(SInvalidInBufSize);

  MakeKey(Key, SubKey);
  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    DesData(dmDecry, SubKey, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    Dec(Count, SizeOf(TDESBuffer));
  end;
end;

procedure DESEncryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TDESKey; const InitVector: TDESIv; Dest: TStream); overload;
var
  TempIn, TempOut: TDESBuffer;
  Vector: TDESIv;
  Done: Cardinal;
  SubKey: TSubKey;
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
  MakeKey(Key, SubKey);

  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;

    DesData(dmEncry, SubKey, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    Move(TempOut[0], Vector[0], SizeOf(TDESIv));
    Dec(Count, SizeOf(TDESBuffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);

    PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;

    DesData(dmEncry, SubKey, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
  end;
end;

procedure DESDecryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: TDESKey; const InitVector: TDESIv; Dest: TStream); overload;
var
  TempIn, TempOut: TDESBuffer;
  Vector1, Vector2: TDESIv;
  Done: Cardinal;
  SubKey: TSubKey;
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
  if (Count mod SizeOf(TDESBuffer)) > 0 then
    raise Exception.Create(SInvalidInBufSize);

  Vector1 := InitVector;
  MakeKey(Key, SubKey);

  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SReadError);

    Move(TempIn[0], Vector2[0], SizeOf(TDESIv));
    DesData(dmDecry, SubKey, TempIn, TempOut);

    PLongWord(@TempOut[0])^ := PLongWord(@TempOut[0])^ xor PLongWord(@Vector1[0])^;
    PLongWord(@TempOut[4])^ := PLongWord(@TempOut[4])^ xor PLongWord(@Vector1[4])^;

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SWriteError);

    Vector1 := Vector2;
    Dec(Count, SizeOf(TDESBuffer));
  end;
end;

procedure Make3DESKeys(Keys: AnsiString; var K1, K2, K3: TDESKey); overload;
var
  I: Integer;
begin
  if Length(Keys) < TRIPLE_DES_KEYSIZE then
    while Length(Keys) < TRIPLE_DES_KEYSIZE do
      Keys := Keys + Chr(0);

  for I := 0 to DES_KEYSIZE - 1 do
  begin
    K1[I] := Ord(Keys[I + 1]);
    K2[I] := Ord(Keys[I + 1 + DES_KEYSIZE]);
    K3[I] := Ord(Keys[I + 1 + DES_KEYSIZE * 2]);
  end;
end;

procedure Make3DESKeys(Keys: T3DESKey; var K1, K2, K3: TDESKey); overload;
var
  I: Integer;
begin
  for I := 0 to DES_KEYSIZE - 1 do
  begin
    K1[I] := Keys[I];
    K2[I] := Keys[I + DES_KEYSIZE];
    K3[I] := Keys[I + DES_KEYSIZE * 2];
  end;
end;

{$IFDEF TBYTES_DEFINED}

procedure Make3DESKeys(Keys: TBytes; var K1, K2, K3: TDESKey); overload;
var
  I, Len: Integer;
begin
  Len := Length(Keys);
  if Len < TRIPLE_DES_KEYSIZE then
  begin
    SetLength(Keys, TRIPLE_DES_KEYSIZE);
    for I := Len to TRIPLE_DES_KEYSIZE - 1 do
      Keys[I] := 0;
  end;

  for I := 0 to DES_KEYSIZE - 1 do
  begin
    K1[I] := Ord(Keys[I]);
    K2[I] := Ord(Keys[I + DES_KEYSIZE]);
    K3[I] := Ord(Keys[I + DES_KEYSIZE * 2]);
  end;
end;

{$ENDIF}

procedure TripleDESEncryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  Str: AnsiString;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  Str := Input;
  MakeInputAlign(Str);

  for I := 0 to Length(Str) div DES_BLOCKSIZE - 1 do
  begin
    Move(Str[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));

    DesData(dmEncry, SubKey1, StrByte, OutByte);
    DesData(dmDecry, SubKey2, OutByte, StrByte);
    DesData(dmEncry, SubKey3, StrByte, OutByte);

    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;
end;

procedure TripleDESDecryptECBStr(Key: AnsiString; const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));

    DesData(dmDecry, SubKey3, StrByte, OutByte);
    DesData(dmEncry, SubKey2, OutByte, StrByte);
    DesData(dmDecry, SubKey1, StrByte, OutByte);

    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;

  // 末尾补的 0 由外部判断删除
end;

procedure TripleDESEncryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  Vector: TDESIv;
  Str: AnsiString;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  Str := Input;
  MakeInputAlign(Str);
  Move(Iv^, Vector[0], SizeOf(TDESIv));

  for I := 0 to Length(Str) div DES_BLOCKSIZE - 1 do
  begin
    Move(Str[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));

    // CBC 数据块的值先跟 Iv 异或
    PLongWord(@StrByte[0])^ := PLongWord(@StrByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@StrByte[4])^ := PLongWord(@StrByte[4])^ xor PLongWord(@Vector[4])^;

    // 再加密
    DesData(dmEncry, SubKey1, StrByte, OutByte);
    DesData(dmDecry, SubKey2, OutByte, StrByte);
    DesData(dmEncry, SubKey3, StrByte, OutByte);

    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 加密结果更新到 Iv
    Move(OutByte[0], Vector[0], SizeOf(TDESIv));
  end;
end;

procedure TripleDESDecryptCBCStr(Key: AnsiString; Iv: PAnsiChar;
  const Input: AnsiString; Output: PAnsiChar);
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  Vector, TV: TDESIv;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  Move(Iv^, Vector[0], SizeOf(TDESIv));

  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE + 1], StrByte[0], SizeOf(TDESBuffer));
    Move(StrByte[0], TV[0], SizeOf(TDESIv)); // 密文先存一下

    // 先解密
    DesData(dmDecry, SubKey3, StrByte, OutByte);
    DesData(dmEncry, SubKey2, OutByte, StrByte);
    DesData(dmDecry, SubKey1, StrByte, OutByte);

    // CBC 数据块解密后的值再跟 Iv 异或
    PLongWord(@OutByte[0])^ := PLongWord(@OutByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@OutByte[4])^ := PLongWord(@OutByte[4])^ xor PLongWord(@Vector[4])^;

    Move(OutByte[0], Output[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 密文更新到 Iv
    Move(TV[0], Vector[0], SizeOf(TDESIv));
  end;

  // 末尾补的 0 由外部判断删除
end;

function TripleDESEncryptStrToHex(const Str, Key: AnsiString): AnsiString;
var
  TempResult, Temp: AnsiString;
  I: Integer;
begin
  SetResultLengthUsingInput(Str, TempResult);
  TripleDESEncryptECBStr(Key, Str, @TempResult[1]);

  Result := '';
  for I := 0 to Length(TempResult) - 1 do
  begin
    Temp := AnsiString(Format('%x', [Ord(TempResult[I + 1])]));
    if Length(Temp) = 1 then
      Temp := '0' + Temp;
    Result := Result + Temp;
  end;
end;

function TripleDESDecryptStrFromHex(const StrHex, Key: AnsiString): AnsiString;
var
  Str, Temp: AnsiString;
  I: Integer;
begin
  Str := '';
  for I := 0 to Length(StrHex) div 2 - 1 do
  begin
    Temp := Copy(StrHex, I * 2 + 1, 2);
    Str := Str + AnsiChar(HexToInt(Temp));
  end;

  SetResultLengthUsingInput(Str, Result);
  TripleDESDecryptECBStr(Key, Str, @(Result[1]));
end;

{$IFDEF TBYTES_DEFINED}

function TripleDESEncryptECBBytes(Key: TBytes; Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  MakeInputBytesAlign(Input);

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));

    DesData(dmEncry, SubKey1, StrByte, OutByte);
    DesData(dmDecry, SubKey2, OutByte, StrByte);
    DesData(dmEncry, SubKey3, StrByte, OutByte);

    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;
end;

function TripleDESDecryptECBBytes(Key: TBytes; const Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));

    DesData(dmDecry, SubKey3, StrByte, OutByte);
    DesData(dmEncry, SubKey2, OutByte, StrByte);
    DesData(dmDecry, SubKey1, StrByte, OutByte);

    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));
  end;
end;

function TripleDESEncryptCBCBytes(Key, Iv: TBytes; Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  Vector: TDESIv;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  MakeInputBytesAlign(Input);
  Move(Iv[0], Vector[0], SizeOf(TDESIv));

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));

    // CBC 数据块的值先跟 Iv 异或
    PLongWord(@StrByte[0])^ := PLongWord(@StrByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@StrByte[4])^ := PLongWord(@StrByte[4])^ xor PLongWord(@Vector[4])^;

    // 再加密
    DesData(dmEncry, SubKey1, StrByte, OutByte);
    DesData(dmDecry, SubKey2, OutByte, StrByte);
    DesData(dmEncry, SubKey3, StrByte, OutByte);

    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 加密结果更新到 Iv
    Move(OutByte[0], Vector[0], SizeOf(TDESIv));
  end;
end;

function TripleDESDecryptCBCBytes(Key, Iv: TBytes; const Input: TBytes): TBytes;
var
  StrByte, OutByte: TDESBuffer;
  K1, K2, K3: TDESKey;
  Vector, TV: TDESIv;
  I: Integer;
  SubKey1, SubKey2, SubKey3: TSubKey;
begin
  if Length(Input) <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  Move(Iv[0], Vector[0], SizeOf(TDESIv));

  SetLength(Result, (((Length(Input) - 1) div DES_BLOCKSIZE) + 1) * DES_BLOCKSIZE);
  for I := 0 to Length(Input) div DES_BLOCKSIZE - 1 do
  begin
    Move(Input[I * DES_BLOCKSIZE], StrByte[0], SizeOf(TDESBuffer));
    Move(StrByte[0], TV[0], SizeOf(TDESIv)); // 密文先存一下

    // 先解密
    DesData(dmDecry, SubKey3, StrByte, OutByte);
    DesData(dmEncry, SubKey2, OutByte, StrByte);
    DesData(dmDecry, SubKey1, StrByte, OutByte);

    // CBC 数据块解密后的值再跟 Iv 异或
    PLongWord(@OutByte[0])^ := PLongWord(@OutByte[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@OutByte[4])^ := PLongWord(@OutByte[4])^ xor PLongWord(@Vector[4])^;

    Move(OutByte[0], Result[I * DES_BLOCKSIZE], SizeOf(TDESBuffer));

    // 密文更新到 Iv
    Move(TV[0], Vector[0], SizeOf(TDESIv));
  end;
end;

{$ENDIF}

procedure TripleDESEncryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; Dest: TStream); overload;
var
  K1, K2, K3: TDESKey;
  TempIn, TempOut: TDESBuffer;
  Done: Cardinal;
  SubKey1, SubKey2, SubKey3: TSubKey;
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

  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    DesData(dmEncry, SubKey1, TempIn, TempOut);
    DesData(dmDecry, SubKey2, TempOut, TempIn);
    DesData(dmEncry, SubKey3, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    Dec(Count, SizeOf(TDESBuffer));
  end;

  if Count > 0 then // 尾部补 0
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);

    DesData(dmEncry, SubKey1, TempIn, TempOut);
    DesData(dmDecry, SubKey2, TempOut, TempIn);
    DesData(dmEncry, SubKey3, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
  end;
end;

procedure TripleDESDecryptStreamECB(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; Dest: TStream); overload;
var
  K1, K2, K3: TDESKey;
  TempIn, TempOut: TDESBuffer;
  Done: Cardinal;
  SubKey1, SubKey2, SubKey3: TSubKey;
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
  if (Count mod SizeOf(TDESBuffer)) > 0 then
    raise Exception.Create(SInvalidInBufSize);

  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    DesData(dmDecry, SubKey3, TempIn, TempOut);
    DesData(dmEncry, SubKey2, TempOut, TempIn);
    DesData(dmDecry, SubKey1, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    Dec(Count, SizeOf(TDESBuffer));
  end;
end;

procedure TripleDESEncryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; const InitVector: TDESIv; Dest: TStream); overload;
var
  K1, K2, K3: TDESKey;
  TempIn, TempOut: TDESBuffer;
  Vector: TDESIv;
  Done: Cardinal;
  SubKey1, SubKey2, SubKey3: TSubKey;
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
  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;

    DesData(dmEncry, SubKey1, TempIn, TempOut);
    DesData(dmDecry, SubKey2, TempOut, TempIn);
    DesData(dmEncry, SubKey3, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    Move(TempOut[0], Vector[0], SizeOf(TDESIv));
    Dec(Count, SizeOf(TDESBuffer));
  end;

  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);

    PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;

    DesData(dmEncry, SubKey1, TempIn, TempOut);
    DesData(dmDecry, SubKey2, TempOut, TempIn);
    DesData(dmEncry, SubKey3, TempIn, TempOut);

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
  end;
end;

procedure TripleDESDecryptStreamCBC(Source: TStream; Count: Cardinal;
  const Key: T3DESKey; const InitVector: TDESIv; Dest: TStream); overload;
var
  K1, K2, K3: TDESKey;
  TempIn, TempOut: TDESBuffer;
  Vector1, Vector2: TDESIv;
  Done: Cardinal;
  SubKey1, SubKey2, SubKey3: TSubKey;
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
  if (Count mod SizeOf(TDESBuffer)) > 0 then
    raise Exception.Create(SInvalidInBufSize);

  Vector1 := InitVector;
  Make3DESKeys(Key, K1, K2, K3);
  MakeKey(K1, SubKey1);
  MakeKey(K2, SubKey2);
  MakeKey(K3, SubKey3);

  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SReadError);

    Move(TempIn[0], Vector2[0], SizeOf(TDESIv));

    DesData(dmDecry, SubKey3, TempIn, TempOut);
    DesData(dmEncry, SubKey2, TempOut, TempIn);
    DesData(dmDecry, SubKey1, TempIn, TempOut);

    PLongWord(@TempOut[0])^ := PLongWord(@TempOut[0])^ xor PLongWord(@Vector1[0])^;
    PLongWord(@TempOut[4])^ := PLongWord(@TempOut[4])^ xor PLongWord(@Vector1[4])^;

    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SWriteError);

    Vector1 := Vector2;
    Dec(Count, SizeOf(TDESBuffer));
  end;
end;

end.
