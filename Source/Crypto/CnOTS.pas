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

unit CnOTS;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：一次性杂凑签名算法实现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：本单元实现了基于 SM3 与 SHA256 的 OTS/M-OTS/W-OTS/W-OTS+ 一次性杂凑签名算法
*           （Hash Based One Time Signature），其他长度的杂凑算法暂未实现。
*           其中 W-OTS+ 采用 w = 16 也就是 4 位一块的分块方式，NIST 在 SLHDSA 中推荐的。
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2025.11.21 V1.1
*               加入 W-OTS+ 的实现
*           2023.11.25 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnBits, CnRandom, CnSM3, CnSHA2;

const
  CN_WOTS_PLUS_W = 16;

type
  ECnOTSException = class(Exception);
  {* OTS 相关异常}

// ================ Lamport 发明的常规 OTS，结合 SM3 杂凑算法 ==================

  TCnOTSSM3PrivateKey = array[0..(SizeOf(TCnSM3Digest) * 8 * 2) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名私钥，为 256 * 2 个随机值，为一致起见，单个随机值长度取 SM3 的结果长度}

  TCnOTSSM3PublicKey = array[0..(SizeOf(TCnSM3Digest) * 8 * 2) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名公钥，为 256 * 2 个随机值的 SM3 杂凑值}

  TCnOTSSM3Signature = array[0..(SizeOf(TCnSM3Digest) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名值，实际上是 256 个 SM3 杂凑值}

  TCnOTSSM3VerificationKey = array[0..(SizeOf(TCnSM3Digest) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名验证密钥，实际上是从私钥中抽取的 256 个随机值}

// =============== Lamport 发明的常规 OTS，结合 SHA256 杂凑算法 ================

  TCnOTSSHA256PrivateKey = array[0..(SizeOf(TCnSHA256Digest) * 8 * 2) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名私钥，为 256 * 2 个随机值，为一致起见，单个随机值长度取 SHA256 的结果长度}

  TCnOTSSHA256PublicKey = array[0..(SizeOf(TCnSHA256Digest) * 8 * 2) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名公钥，为 256 * 2 个随机值的 SHA256 杂凑值}

  TCnOTSSHA256Signature = array[0..(SizeOf(TCnSHA256Digest) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名值，实际上是 256 个 SHA256 杂凑值}

  TCnOTSSHA256VerificationKey = array[0..(SizeOf(TCnSHA256Digest) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名验证密钥，实际上是从私钥中抽取的 256 个随机值}

// ========== Merkle 发明的 M-OTS，校验 0 的个数，结合 SM3 杂凑算法 ============

  TCnMOTSSM3PrivateKey = array[0..((SizeOf(TCnSM3Digest) + 1) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 M-OTS 私钥，为 256 个随机值加上一个单字节校验和，为一致起见，单个随机值长度取 SM3 的结果长度}

  TCnMOTSSM3PublicKey = array[0..((SizeOf(TCnSM3Digest)+ 1) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 M-OTS 公钥，为 256 个随机值加上一个单字节校验和的 SM3 杂凑值}

  TCnMOTSSM3Signature = array[0..((SizeOf(TCnSM3Digest) + 1) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 M-OTS 签名，实际上是 256 个 SM3 杂凑值加上一个单字节校验和，其中 0 对应全 0}

// ========== Merkle 发明的 M-OTS，校验 0 的个数，结合 SHA256 杂凑算法 ============

  TCnMOTSSHA256PrivateKey = array[0..((SizeOf(TCnSHA256Digest) + 1) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 M-OTS 私钥，为 256 个随机值加上一个单字节校验和，为一致起见，单个随机值长度取 SHA256 的结果长度}

  TCnMOTSSHA256PublicKey = array[0..((SizeOf(TCnSHA256Digest)+ 1) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 M-OTS 公钥，为 256 个随机值加上一个单字节校验和的 SHA256 杂凑值}

  TCnMOTSSHA256Signature = array[0..((SizeOf(TCnSHA256Digest) + 1) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 M-OTS 签名，实际上是 256 个 SHA256 杂凑值加上一个单字节校验和，其中 0 对应全 0}

// ===== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SM3 杂凑算法 ======

  TCnWOTSSM3PrivateKey = array[0..SizeOf(TCnSM3Digest) + 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS 私钥，为 32 个随机值加上一个双字节校验和，为一致起见，单个随机值长度取 SM3 的结果长度}

  TCnWOTSSM3PublicKey = array[0..SizeOf(TCnSM3Digest) + 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS 公钥，为 32 个随机值加上一个双字节校验和各计算 256 次得到的 SM3 杂凑值}

  TCnWOTSSM3Signature = array[0..SizeOf(TCnSM3Digest) + 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS 签名，为 32 个 SM3 杂凑值加上一个双字节校验和，注意双字节按网络顺序存储}

// ==== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SHA256 杂凑算法 ====

  TCnWOTSSHA256PrivateKey = array[0..SizeOf(TCnSHA256Digest) + 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS 私钥，为 32 个随机值加上一个双字节校验和，为一致起见，单个随机值长度取 SHA256 的结果长度}

  TCnWOTSSHA256PublicKey = array[0..SizeOf(TCnSHA256Digest) + 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS 公钥，为 32 个随机值加上一个双字节校验和各计算 256 次得到的 SHA256 杂凑值}

  TCnWOTSSHA256Signature = array[0..SizeOf(TCnSHA256Digest) + 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS 签名，为 32 个 SHA256 杂凑值加上一个双字节校验和，注意双字节按网络顺序存储}

// ==== Winternitz 改进的 W-OTS+，取 w = 16 也即 4 位，结合 SM3 杂凑算法 ======

  TCnWOTSPlusSM3PrivateKey = array[0..66] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS+ 私钥，为 64 个随机值加上三个校验和，单个随机值长度取 SM3 的结果长度}

  TCnWOTSPlusSM3PublicKey = array[0..66] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS+ 公钥，为私钥结合随机掩码链式计算得到的同样规模的 SM3 杂凑值}

  TCnWOTSPlusSM3Signature = array[0..66] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS+ 签名}

  TCnWOTSPlusSM3Mask = array[0..CN_WOTS_PLUS_W - 1] of TCnSM3Digest;
  {* W-OTS+ 使用的随机掩码，15 个公开值，验证时也需参与}

// ==== Winternitz 改进的 W-OTS+，取 W = 16 也即 4 位，结合 SHA256 杂凑算法 ====

  TCnWOTSPlusSHA256PrivateKey = array[0..66] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS+ 私钥，为 64 个随机值加上三个校验和，单个随机值长度取 SHA256 的结果长度}

  TCnWOTSPlusSHA256PublicKey = array[0..66] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS+ 公钥，为私钥结合随机掩码链式计算得到的同样规模的 SHA256 杂凑值}

  TCnWOTSPlusSHA256Signature = array[0..66] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS+ 签名}

  TCnWOTSPlusSHA256Mask = array[0..CN_WOTS_PLUS_W - 1] of TCnSHA256Digest;
  {* W-OTS Plus 使用的随机掩码，15 个公开值，验证时也需参与}

// ================ Lamport 发明的常规 OTS，结合 SM3 杂凑算法 ==================

function CnOTSSM3GenerateKeys(var PrivateKey: TCnOTSSM3PrivateKey;
  var PublicKey: TCnOTSSM3PublicKey): Boolean;
{* 生成一对基于 SM3 杂凑算法的 OTS 一次性杂凑签名公私钥，返回生成是否成功。

   参数：
     var PrivateKey: TCnOTSSM3PrivateKey  - 生成的基于 SM3 的一次性杂凑签名私钥
     var PublicKey: TCnOTSSM3PublicKey    - 生成的基于 SM3 的一次性杂凑签名公钥

   返回值：Boolean                        - 返回生成是否成功
}

procedure CnOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSM3PrivateKey; PublicKey: TCnOTSSM3PublicKey;
  var OutSignature: TCnOTSSM3Signature; var OutVerifyKey: TCnOTSSM3VerificationKey);
{* 根据公私钥生成指定数据块的 OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
   验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
   不能再用这批私钥给别的消息签名了，这正是一次性签名的含义。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnOTSSM3PrivateKey                      - 用来签名的基于 SM3 的一次性杂凑签名私钥
     PublicKey: TCnOTSSM3PublicKey                        - 用来签名的基于 SM3 的一次性杂凑签名公钥
     var OutSignature: TCnOTSSM3Signature                 - 输出的签名值
     var OutVerifyKey: TCnOTSSM3VerificationKey           - 输出的验证密钥

   返回值：（无）
}

function CnOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSM3Signature; PublicKey: TCnOTSSM3PublicKey;
  VerifyKey: TCnOTSSM3VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证指定数据块的 OTS 签名是否正确，返回验证是否成功。
   注意规范中 Signature 未参与验证，仅使用 VerifyKey 就行。

   参数：
     Data: Pointer                        - 待验证的明文数据块地址
     DataByteLen: Integer                 - 待验证的明文数据块字节长度
     Signature: TCnOTSSM3Signature        - 待验证的签名值
     PublicKey: TCnOTSSM3PublicKey        - 用来验证的基于 SM3 的一次性杂凑签名公钥
     VerifyKey: TCnOTSSM3VerificationKey  - 用来验证的验证密钥

   返回值：Boolean                        - 返回验证签名是否成功
}

procedure CnOTSSM3SignBytes(const Data: TBytes; PrivateKey: TCnOTSSM3PrivateKey;
  PublicKey: TCnOTSSM3PublicKey; var OutSignature: TCnOTSSM3Signature;
  var OutVerifyKey: TCnOTSSM3VerificationKey);
{* 根据公私钥生成字节数组的 OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
   验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
   不能再用这批私钥给别的消息签名了，这正是一次性签名的含义。

   参数：
     const Data: TBytes                                   - 待签名的明文字节数组
     PrivateKey: TCnOTSSM3PrivateKey                      - 用来签名的基于 SM3 的一次性杂凑签名私钥
     PublicKey: TCnOTSSM3PublicKey                        - 用来签名的基于 SM3 的一次性杂凑签名公钥
     var OutSignature: TCnOTSSM3Signature                 - 输出的签名值
     var OutVerifyKey: TCnOTSSM3VerificationKey           - 输出的验证密钥

   返回值：（无）
}

function CnOTSSM3VerifyBytes(const Data: TBytes; Signature: TCnOTSSM3Signature;
  PublicKey: TCnOTSSM3PublicKey; VerifyKey: TCnOTSSM3VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证字节数组的 OTS 签名是否正确，返回验证是否成功。
   注意规范中 Signature 未参与验证，仅使用 VerifyKey 就行。

   参数：
     const Data: TBytes                   - 待验证的明文字节数组
     Signature: TCnOTSSM3Signature        - 待验证的签名值
     PublicKey: TCnOTSSM3PublicKey        - 用来验证的基于 SM3 的一次性杂凑签名公钥
     VerifyKey: TCnOTSSM3VerificationKey  - 用来验证的验证密钥

   返回值：Boolean                        - 返回验证签名是否成功
}

// =============== Lamport 发明的常规 OTS，结合 SHA256 杂凑算法 ================

function CnOTSSHA256GenerateKeys(var PrivateKey: TCnOTSSHA256PrivateKey;
  var PublicKey: TCnOTSSHA256PublicKey): Boolean;
{* 生成一对基于 SHA256 杂凑算法的 OTS 一次性杂凑签名公私钥，返回生成是否成功。

   参数：
     var PrivateKey: TCnOTSSHA256PrivateKey               - 生成的基于 SHA256 的一次性杂凑签名私钥
     var PublicKey: TCnOTSSHA256PublicKey                 - 生成的基于 SHA256 的一次性杂凑签名公钥

   返回值：Boolean                                        - 返回生成是否成功
}

procedure CnOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSHA256PrivateKey; PublicKey: TCnOTSSHA256PublicKey;
  var OutSignature: TCnOTSSHA256Signature; var OutVerifyKey: TCnOTSSHA256VerificationKey);
{* 根据公私钥生成指定数据块的 OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
   验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
   不能再用这批私钥给别的消息签名了，这正是一次性签名的含义。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnOTSSHA256PrivateKey                   - 用来签名的基于 SHA256 的一次性杂凑签名私钥
     PublicKey: TCnOTSSHA256PublicKey                     - 用来签名的基于 SHA256 的一次性杂凑签名公钥
     var OutSignature: TCnOTSSHA256Signature              - 输出的签名值
     var OutVerifyKey: TCnOTSSHA256VerificationKey        - 输出的验证密钥

   返回值：（无）
}

function CnOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSHA256Signature; PublicKey: TCnOTSSHA256PublicKey;
  VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证指定数据块的 OTS 签名是否正确，返回验证是否成功。
   注意规范中 Signature 未参与验证，仅使用 VerifyKey 就行。

   参数：
     Data: Pointer                                        - 待验证的明文数据块地址
     DataByteLen: Integer                                 - 待验证的明文数据块字节长度
     Signature: TCnOTSSHA256Signature                     - 待验证的签名值
     PublicKey: TCnOTSSHA256PublicKey                     - 用来验证的基于 SHA256 的一次性杂凑签名公钥
     VerifyKey: TCnOTSSHA256VerificationKey               - 用来验证的验证密钥

   返回值：Boolean                                        - 返回验证签名是否成功
}

procedure CnOTSSHA256SignBytes(const Data: TBytes; PrivateKey: TCnOTSSHA256PrivateKey;
  PublicKey: TCnOTSSHA256PublicKey; var OutSignature: TCnOTSSHA256Signature;
  var OutVerifyKey: TCnOTSSHA256VerificationKey);
{* 根据公私钥生成字节数组的 OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
   验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
   不能再用这批私钥给别的消息签名了，这正是一次性签名的含义。

   参数：
     const Data: TBytes                                   - 待签名的明文字节数组
     PrivateKey: TCnOTSSHA256PrivateKey                   - 用来签名的基于 SHA256 的一次性杂凑签名私钥
     PublicKey: TCnOTSSHA256PublicKey                     - 用来签名的基于 SHA256 的一次性杂凑签名公钥
     var OutSignature: TCnOTSSHA256Signature              - 输出的签名值
     var OutVerifyKey: TCnOTSSHA256VerificationKey        - 输出的验证密钥

   返回值：（无）
}

function CnOTSSHA256VerifyBytes(const Data: TBytes; Signature: TCnOTSSHA256Signature;
  PublicKey: TCnOTSSHA256PublicKey; VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证字节数组的 OTS 签名是否正确，返回验证是否成功
   注意规范中 Signature 未参与验证，仅使用 VerifyKey 就行。

   参数：
     const Data: TBytes                                   - 待验证的明文字节数组
     Signature: TCnOTSSHA256Signature                     - 待验证的签名值
     PublicKey: TCnOTSSHA256PublicKey                     - 用来验证的基于 SHA256 的一次性杂凑签名公钥
     VerifyKey: TCnOTSSHA256VerificationKey               - 用来验证的验证密钥

   返回值：Boolean                                        - 返回验证签名是否成功
}

// ========== Merkle 改进的 M-OTS，校验 0 的个数，结合 SM3 杂凑算法 ============

function CnMOTSSM3GenerateKeys(var PrivateKey: TCnMOTSSM3PrivateKey;
  var PublicKey: TCnMOTSSM3PublicKey): Boolean;
{* 生成一对基于 SM3 杂凑算法的 M-OTS 公私钥，返回生成是否成功

   参数：
     var PrivateKey: TCnMOTSSM3PrivateKey - 基于 SM3 杂凑算法的 M-OTS 私钥
     var PublicKey: TCnMOTSSM3PublicKey   - 基于 SM3 杂凑算法的 M-OTS 公钥

   返回值：Boolean                        - 返回生成是否成功
}

procedure CnMOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSM3PrivateKey; var OutSignature: TCnMOTSSM3Signature);
{* 根据私钥生成指定数据块的 M-OTS 一次性杂凑签名。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnMOTSSM3PrivateKey                     - 用来签名的基于 SM3 的 M-OTS 私钥
     var OutSignature: TCnMOTSSM3Signature                - 输出的签名值

   返回值：（无）
}

function CnMOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSM3Signature; PublicKey: TCnMOTSSM3PublicKey): Boolean;
{* 根据明文与公钥验证指定数据块的 M-OTS 签名是否正确，返回验证是否成功。

   参数：
     Data: Pointer                        - 待验证的明文数据块地址
     DataByteLen: Integer                 - 待验证的明文数据块字节长度
     Signature: TCnMOTSSM3Signature       - 待验证的签名值
     PublicKey: TCnMOTSSM3PublicKey       - 用来验证的基于 SM3 的 M-OTS 公钥

   返回值：Boolean                        - 返回验证签名是否成功
}

procedure CnMOTSSM3SignBytes(const Data: TBytes; PrivateKey: TCnMOTSSM3PrivateKey;
  var OutSignature: TCnMOTSSM3Signature);
{* 根据私钥生成字节数组的 M-OTS 一次性杂凑签名。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥。

   参数：
     const Data: TBytes                                   - 待签名的明文字节数组
     PrivateKey: TCnMOTSSM3PrivateKey                     - 用来签名的基于 SM3 的 M-OTS 私钥
     var OutSignature: TCnMOTSSM3Signature                - 输出的签名值

   返回值：（无）
}

function CnMOTSSM3VerifyBytes(const Data: TBytes; Signature: TCnMOTSSM3Signature;
  PublicKey: TCnMOTSSM3PublicKey): Boolean;
{* 根据明文与公钥验证字节数组的 M-OTS 签名是否正确，返回验证是否成功。

   参数：
     const Data: TBytes                   - 待验证的明文字节数组
     Signature: TCnMOTSSM3Signature       - 待验证的签名值
     PublicKey: TCnMOTSSM3PublicKey       - 用来验证的基于 SM3 的 M-OTS 公钥

   返回值：Boolean                        - 返回验证签名是否成功
}

// ========== Merkle 改进的 M-OTS，校验 0 的个数，结合 SHA256 杂凑算法 ============

function CnMOTSSHA256GenerateKeys(var PrivateKey: TCnMOTSSHA256PrivateKey;
  var PublicKey: TCnMOTSSHA256PublicKey): Boolean;
{* 生成一对基于 SHA256 杂凑算法的 M-OTS 签名公私钥，返回生成是否成功。

   参数：
     var PrivateKey: TCnMOTSSHA256PrivateKey              - 基于 SHA256 杂凑算法的 M-OTS 私钥
     var PublicKey: TCnMOTSSHA256PublicKey                - 基于 SHA256 杂凑算法的 M-OTS 公钥

   返回值：Boolean                                        - 返回生成是否成功
}

procedure CnMOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSHA256PrivateKey; var OutSignature: TCnMOTSSHA256Signature);
{* 根据私钥生成指定数据块的 M-OTS 一次性杂凑签名。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnMOTSSHA256PrivateKey                  - 用来签名的基于 SHA256 杂凑算法的 M-OTS 私钥
     var OutSignature: TCnMOTSSHA256Signature             - 输出的签名值

   返回值：（无）
}

function CnMOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSHA256Signature; PublicKey: TCnMOTSSHA256PublicKey): Boolean;
{* 根据明文与公钥验证指定数据块的 M-OTS 签名是否正确，返回验证是否成功

   参数：
     Data: Pointer                        - 待验证的明文数据块地址
     DataByteLen: Integer                 - 待验证的明文数据块字节长度
     Signature: TCnMOTSSHA256Signature    - 待验证的签名值
     PublicKey: TCnMOTSSHA256PublicKey    - 用来验证的基于 SHA256 杂凑算法的 M-OTS 公钥

   返回值：Boolean                        - 返回验证签名是否成功
}

procedure CnMOTSSHA256SignBytes(const Data: TBytes; PrivateKey: TCnMOTSSHA256PrivateKey;
  var OutSignature: TCnMOTSSHA256Signature);
{* 根据私钥生成字节数组的 M-OTS 一次性杂凑签名。
   平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥。

   参数：
     const Data: TBytes                                   - 待签名的明文字节数组
     PrivateKey: TCnMOTSSHA256PrivateKey                  - 用来签名的基于 SHA256 杂凑算法的 M-OTS 私钥
     var OutSignature: TCnMOTSSHA256Signature             - 输出的签名值

   返回值：（无）
}

function CnMOTSSHA256VerifyBytes(const Data: TBytes; Signature: TCnMOTSSHA256Signature;
  PublicKey: TCnMOTSSHA256PublicKey): Boolean;
{* 根据明文与公钥验证字节数组的 M-OTS 签名是否正确，返回验证是否成功。

   参数：
     const Data: TBytes                   - 待验证的明文字节数组
     Signature: TCnMOTSSHA256Signature    - 待验证的签名值
     PublicKey: TCnMOTSSHA256PublicKey    - 用来验证的基于 SHA256 杂凑算法的 M-OTS 公钥

   返回值：Boolean                        - 返回验证签名是否成功
}

// ===== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SM3 杂凑算法 ======

function CnWOTSSM3GenerateKeys(var PrivateKey: TCnWOTSSM3PrivateKey;
  var PublicKey: TCnWOTSSM3PublicKey): Boolean;
{* 生成一对基于 SM3 杂凑算法的 W-OTS 一次性杂凑签名公私钥，返回生成是否成功。

   参数：
     var PrivateKey: TCnWOTSSM3PrivateKey - 基于 SM3 杂凑算法的 W-OTS 私钥
     var PublicKey: TCnWOTSSM3PublicKey   - 基于 SM3 杂凑算法的 W-OTS 公钥

   返回值：Boolean                        - 返回生成是否成功
}

procedure CnWOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSM3PrivateKey; var OutSignature: TCnWOTSSM3Signature);
{* 根据私钥生成指定数据块的 W-OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值，有验证需求时，给验证方公布公钥。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnWOTSSM3PrivateKey                     - 用来签名的基于 SM3 杂凑算法的 W-OTS 私钥
     var OutSignature: TCnWOTSSM3Signature                - 输出的签名值

   返回值：（无）
}

function CnWOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSM3Signature; PublicKey: TCnWOTSSM3PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS 签名是否正确，返回验证是否成功。

   参数：
     Data: Pointer                        - 待验证的明文数据块地址
     DataByteLen: Integer                 - 待验证的明文数据块字节长度
     Signature: TCnWOTSSM3Signature       - 待验证的签名值
     PublicKey: TCnWOTSSM3PublicKey       - 用来验证的基于 SM3 杂凑算法的 W-OTS 公钥

   返回值：Boolean                        - 返回验证签名是否成功
}

procedure CnWOTSSM3SignBytes(const Data: TBytes; PrivateKey: TCnWOTSSM3PrivateKey;
  var OutSignature: TCnWOTSSM3Signature);
{* 根据私钥生成字节数组的 W-OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值，有验证需求时，给验证方公布公钥。

   参数：
     const Data: TBytes                                   - 待签名的明文字节数组
     PrivateKey: TCnWOTSSM3PrivateKey                     - 用来签名的基于 SM3 杂凑算法的 W-OTS 私钥
     var OutSignature: TCnWOTSSM3Signature                - 输出的签名值

   返回值：（无）
}

function CnWOTSSM3VerifyBytes(const Data: TBytes; Signature: TCnWOTSSM3Signature;
  PublicKey: TCnWOTSSM3PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS 签名是否正确，返回验证是否成功。

   参数：
     const Data: TBytes                   - 待验证的明文字节数组
     Signature: TCnWOTSSM3Signature       - 待验证的签名值
     PublicKey: TCnWOTSSM3PublicKey       - 用来验证的基于 SM3 杂凑算法的 W-OTS 公钥

   返回值：Boolean                        - 返回验证签名是否成功
}

// ==== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SHA256 杂凑算法 ====

function CnWOTSSHA256GenerateKeys(var PrivateKey: TCnWOTSSHA256PrivateKey;
  var PublicKey: TCnWOTSSHA256PublicKey): Boolean;
{* 生成一对基于 SHA256 杂凑算法的 W-OTS 一次性杂凑签名公私钥，返回生成是否成功。

   参数：
     var PrivateKey: TCnWOTSSHA256PrivateKey              - 基于 SHA256 杂凑算法的 W-OTS 私钥
     var PublicKey: TCnWOTSSHA256PublicKey                - 基于 SHA256 杂凑算法的 W-OTS 公钥

   返回值：Boolean                                        - 返回生成是否成功
}

procedure CnWOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSHA256PrivateKey; var OutSignature: TCnWOTSSHA256Signature);
{* 根据私钥生成指定数据块的 W-OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值，有验证需求时，给验证方公布公钥。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnWOTSSHA256PrivateKey                  - 用来签名的基于 SHA256 杂凑算法的 W-OTS 私钥
     var OutSignature: TCnWOTSSHA256Signature             - 输出的签名值

   返回值：（无）
}

function CnWOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSHA256Signature; PublicKey: TCnWOTSSHA256PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS 签名是否正确，返回验证是否成功。

   参数：
     Data: Pointer                        - 待验证的明文数据块地址
     DataByteLen: Integer                 - 待验证的明文数据块字节长度
     Signature: TCnWOTSSHA256Signature    - 用来验证的基于 SHA256 杂凑算法的 W-OTS 公钥
     PublicKey: TCnWOTSSHA256PublicKey    - 待验证的签名值

   返回值：Boolean                        - 返回验证签名是否成功
}

procedure CnWOTSSHA256SignBytes(const Data: TBytes; PrivateKey: TCnWOTSSHA256PrivateKey;
  var OutSignature: TCnWOTSSHA256Signature);
{* 根据私钥生成字节数组的 W-OTS 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值，有验证需求时，给验证方公布公钥。

   参数：
     const Data: TBytes                                   - 待签名的明文字节数组
     PrivateKey: TCnWOTSSHA256PrivateKey                  - 用来签名的基于 SHA256 杂凑算法的 W-OTS 私钥
     var OutSignature: TCnWOTSSHA256Signature             - 输出的签名值

   返回值：（无）
}

function CnWOTSSHA256VerifyBytes(const Data: TBytes; Signature: TCnWOTSSHA256Signature;
  PublicKey: TCnWOTSSHA256PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS 签名是否正确，返回验证是否成功。

   参数：
     const Data: TBytes                   - 待验证的明文字节数组
     Signature: TCnWOTSSHA256Signature    - 待验证的签名值
     PublicKey: TCnWOTSSHA256PublicKey    - 用来验证的基于 SHA256 杂凑算法的 W-OTS 公钥

   返回值：Boolean                        - 返回验证签名是否成功
}

// ===== Winternitz 改进的 W-OTS+，取 n = 8 也即 1 字节，结合 SM3 杂凑算法 ======

function CnWOTSPlusSM3GenerateKeys(out PrivateKey: TCnWOTSPlusSM3PrivateKey;
  out PublicKey: TCnWOTSPlusSM3PublicKey; out Mask: TCnWOTSPlusSM3Mask): Boolean;
{* 生成一对基于 SM3 杂凑算法的 W-OTS+ 一次性杂凑签名公私钥及掩码，返回生成是否成功。

   参数：
     out PrivateKey: TCnWOTSPlusSM3PrivateKey             - 基于 SM3 杂凑算法的 W-OTS+ 私钥
     out PublicKey: TCnWOTSPlusSM3PublicKey               - 基于 SM3 杂凑算法的 W-OTS+ 公钥
     out Mask: TCnWOTSPlusSM3Mask                         - 基于 SM3 杂凑算法的 W-OTS+ 掩码，内部随机生成

   返回值：Boolean                                        - 返回生成是否成功
}

function CnWOTSPlusSM3GenerateKeysFromMask(var PrivateKey: TCnWOTSPlusSM3PrivateKey;
  var PublicKey: TCnWOTSPlusSM3PublicKey; const Mask: TCnWOTSPlusSM3Mask): Boolean;
{* 使用指定的掩码值生成一对基于 SM3 杂凑算法的 W-OTS+ 一次性杂凑签名公私钥，返回生成是否成功。

   参数：
     var PrivateKey: TCnWOTSPlusSM3PrivateKey             - 基于 SM3 杂凑算法的 W-OTS+ 私钥
     var PublicKey: TCnWOTSPlusSM3PublicKey               - 基于 SM3 杂凑算法的 W-OTS+ 公钥
     const Mask: TCnWOTSPlusSM3Salt                       - 指定的随机掩码

   返回值：Boolean                                        - 返回生成是否成功
}

procedure CnWOTSPlusSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSPlusSM3PrivateKey; const Mask: TCnWOTSPlusSM3Mask;
  var OutSignature: TCnWOTSPlusSM3Signature);
{* 根据私钥与随机掩码生成指定数据块的 W-OTS+ 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值，有验证需求时，给验证方公布公钥。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnWOTSPlusSM3PrivateKey                 - 用来签名的基于 SM3 杂凑算法的 W-OTS+ 私钥
     const Mask: TCnWOTSPlusSHA256Mask                    - 用来签名的随机掩码
     var OutSignature: TCnWOTSPlusSM3Signature            - 输出的签名值

   返回值：（无）
}

function CnWOTSPlusSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSPlusSM3Signature; PublicKey: TCnWOTSPlusSM3PublicKey;
  const Mask: TCnWOTSPlusSM3Mask): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS+ 签名是否正确，返回验证是否成功。

   参数：
     Data: Pointer                        - 待验证的明文数据块地址
     DataByteLen: Integer                 - 待验证的明文数据块字节长度
     Signature: TCnWOTSPlusSM3Signature   - 待验证的签名值
     PublicKey: TCnWOTSPlusSM3PublicKey   - 用来验证的基于 SM3 杂凑算法的 W-OTS+ 公钥
     const Mask: TCnWOTSPlusSM3Mask       - 用来验证的随机掩码

   返回值：Boolean                        - 返回验证签名是否成功
}

procedure CnWOTSPlusSM3SignBytes(const Data: TBytes; PrivateKey: TCnWOTSPlusSM3PrivateKey;
  const Mask: TCnWOTSPlusSM3Mask; var OutSignature: TCnWOTSPlusSM3Signature);
{* 根据私钥生成字节数组的 W-OTS+ 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值，有验证需求时，给验证方公布公钥。

   参数：
     const Data: TBytes                                    - 待签名的明文字节数组
     PrivateKey: TCnWOTSPlusSM3PrivateKey                  - 用来签名的基于 SM3 杂凑算法的 W-OTS Plus 私钥
     const Mask: TCnWOTSPlusSM3Mask                        - 用来签名的随机掩码
     var OutSignature: TCnWOTSPlusSM3Signature             - 输出的签名值

   返回值：（无）
}

function CnWOTSPlusSM3VerifyBytes(const Data: TBytes; Signature: TCnWOTSPlusSM3Signature;
  PublicKey: TCnWOTSPlusSM3PublicKey; const Mask: TCnWOTSPlusSM3Mask): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS+ 签名是否正确，返回验证是否成功。

   参数：
     const Data: TBytes                   - 待验证的明文字节数组
     Signature: TCnWOTSPlusSM3Signature   - 待验证的签名值
     PublicKey: TCnWOTSPlusSM3PublicKey   - 用来验证的基于 SM3 杂凑算法的 W-OTS Plus 公钥
     const Mask: TCnWOTSPlusSM3Mask;      - 用来验证的随机掩码

   返回值：Boolean                        - 返回验证签名是否成功
}

// ==== Winternitz 改进的 W-OTS+，取 w = 16 也即 4 位，结合 SHA256 杂凑算法 ====

function CnWOTSPlusSHA256GenerateKeys(out PrivateKey: TCnWOTSPlusSHA256PrivateKey;
  out PublicKey: TCnWOTSPlusSHA256PublicKey; out Mask: TCnWOTSPlusSHA256Mask): Boolean;
{* 生成一对基于 SHA256 杂凑算法的 W-OTS+ 一次性杂凑签名公私钥及掩码，返回生成是否成功。

   参数：
     out PrivateKey: TCnWOTSPlusSHA256PrivateKey          - 基于 SHA256 杂凑算法的 W-OTS+ 私钥
     out PublicKey: TCnWOTSPlusSHA256PublicKey            - 基于 SHA256 杂凑算法的 W-OTS+ 公钥
     out Masks: TCnWOTSPlusSHA256Mask                     - 基于 SHA256 杂凑算法的 W-OTS+ 掩码，内部随机生成

   返回值：Boolean                                        - 返回生成是否成功
}

function CnWOTSPlusSHA256GenerateKeysFromMask(var PrivateKey: TCnWOTSPlusSHA256PrivateKey;
  var PublicKey: TCnWOTSPlusSHA256PublicKey; const Mask: TCnWOTSPlusSHA256Mask): Boolean;
{* 使用指定的盐值生成一对基于 SHA256 杂凑算法的 W-OTS+ 一次性杂凑签名公私钥，返回生成是否成功。

   参数：
     var PrivateKey: TCnWOTSPlusSHA256PrivateKey          - 基于 SHA256 杂凑算法的 W-OTS Plus 私钥
     var PublicKey: TCnWOTSPlusSHA256PublicKey            - 基于 SHA256 杂凑算法的 W-OTS Plus 公钥
     const Salt: TCnWOTSPlusSHA256Salt                    - 指定的盐值

   返回值：Boolean                                        - 返回生成是否成功
}

procedure CnWOTSPlusSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSPlusSHA256PrivateKey; const Mask: TCnWOTSPlusSHA256Mask;
  var OutSignature: TCnWOTSPlusSHA256Signature);
{* 根据私钥和掩码生成指定数据块的 W-OTS+ 一次性杂凑签名。
   平时公布明文、掩码、签名值，有验证需求时，给验证方公布公钥。

   参数：
     Data: Pointer                                        - 待签名的明文数据块地址
     DataByteLen: Integer                                 - 待签名的明文数据块字节长度
     PrivateKey: TCnWOTSPlusSHA256PrivateKey              - 用来签名的基于 SHA256 杂凑算法的 W-OTS+ 私钥
     const Mask: TCnWOTSPlusSHA256Mask                    - 用于签名的随机掩码
     var OutSignature: TCnWOTSPlusSHA256Signature         - 输出的签名值

   返回值：（无）
}

function CnWOTSPlusSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSPlusSHA256Signature; PublicKey: TCnWOTSPlusSHA256PublicKey;
  const Mask: TCnWOTSPlusSHA256Mask): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS+ 签名是否正确，返回验证是否成功。

   参数：
     Data: Pointer                                        - 待验证的明文数据块地址
     DataByteLen: Integer                                 - 待验证的明文数据块字节长度
     Signature: TCnWOTSPlusSHA256Signature                - 待验证的签名值
     PublicKey: TCnWOTSPlusSHA256PublicKey                - 用来验证的基于 SHA256 杂凑算法的 W-OTS Plus 公钥
     const Mask: TCnWOTSPlusSHA256Mask                    - 用于验证的随机掩码

   返回值：Boolean                                        - 返回验证签名是否成功
}

procedure CnWOTSPlusSHA256SignBytes(const Data: TBytes; PrivateKey: TCnWOTSPlusSHA256PrivateKey;
  const Mask: TCnWOTSPlusSHA256Mask; var OutSignature: TCnWOTSPlusSHA256Signature);
{* 根据私钥生成字节数组的 W-OTS+ 一次性杂凑签名及验证这个签名的密钥。
   平时公布明文、签名值，有验证需求时，给验证方公布公钥。

   参数：
     const Data: TBytes                                   - 待签名的明文字节数组
     PrivateKey: TCnWOTSPlusSHA256PrivateKey              - 用来签名的基于 SHA256 杂凑算法的 W-OTS Plus 私钥
     const Mask: TCnWOTSPlusSHA256Mask                    - 用于签名的随机掩码
     var OutSignature: TCnWOTSPlusSHA256Signature         - 输出的签名值

   返回值：（无）
}

function CnWOTSPlusSHA256VerifyBytes(const Data: TBytes; Signature: TCnWOTSPlusSHA256Signature;
  PublicKey: TCnWOTSPlusSHA256PublicKey; const Mask: TCnWOTSPlusSHA256Mask): Boolean;
{* 根据明文、公布的公钥验证指定数据块的 W-OTS+ 签名是否正确，返回验证是否成功。

   参数：
     const Data: TBytes                                   - 待验证的明文字节数组
     Signature: TCnWOTSPlusSHA256Signature                - 待验证的签名值
     PublicKey: TCnWOTSPlusSHA256PublicKey                - 用来验证的基于 SHA256 杂凑算法的 W-OTS Plus 公钥
     const Mask: TCnWOTSPlusSHA256Mask                    - 用于验证的随机掩码

   返回值：Boolean                                        - 返回验证签名是否成功
}

implementation

resourcestring
  SCnErrorOTSInvalidParam = 'OTS Invalid Param';

const
  CN_WOTS_ROUND = 256;

function CnOTSSM3GenerateKeys(var PrivateKey: TCnOTSSM3PrivateKey;
  var PublicKey: TCnOTSSM3PublicKey): Boolean;
var
  I: Integer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnOTSSM3PrivateKey));
  if Result then
  begin
    for I := Low(TCnOTSSM3PublicKey) to High(TCnOTSSM3PublicKey) do
      PublicKey[I] := SM3(@PrivateKey[I], SizeOf(TCnSM3Digest));
  end;
end;

procedure CnOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSM3PrivateKey; PublicKey: TCnOTSSM3PublicKey;
  var OutSignature: TCnOTSSM3Signature; var OutVerifyKey: TCnOTSSM3VerificationKey);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSM3Digest;
begin
  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 是 1
      begin
        OutSignature[I] := PublicKey[I * 2 + 1];
        OutVerifyKey[I] := PrivateKey[I * 2 + 1];
      end
      else
      begin
        OutSignature[I] := PublicKey[I * 2];
        OutVerifyKey[I] := PrivateKey[I * 2];
      end;
    end;
  finally
    Bits.Free;
  end;
end;

function CnOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSM3Signature; PublicKey: TCnOTSSM3PublicKey;
  VerifyKey: TCnOTSSM3VerificationKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Cmp: TCnSM3Digest;
begin
  Result := False;
  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      Cmp := SM3(@VerifyKey[I], SizeOf(TCnSM3Digest)); // 计算私钥的杂凑值
      if Bits.Bit[I] then
        Result := SM3Match(Cmp, PublicKey[I * 2 + 1])  // 该位是 1，比较 1 对应的公钥
      else
        Result := SM3Match(Cmp, PublicKey[I * 2]);     // 该位是 0，比较 0 对应的公钥

      if not Result then
        Exit;
    end;
  finally
    Bits.Free;
  end;
end;

procedure CnOTSSM3SignBytes(const Data: TBytes; PrivateKey: TCnOTSSM3PrivateKey;
  PublicKey: TCnOTSSM3PublicKey; var OutSignature: TCnOTSSM3Signature;
  var OutVerifyKey: TCnOTSSM3VerificationKey);
begin
  if Length(Data) = 0 then
    CnOTSSM3SignData(nil, 0, PrivateKey, PublicKey, OutSignature, OutVerifyKey)
  else
    CnOTSSM3SignData(@Data[0], Length(Data), PrivateKey, PublicKey, OutSignature, OutVerifyKey);
end;

function CnOTSSM3VerifyBytes(const Data: TBytes; Signature: TCnOTSSM3Signature;
  PublicKey: TCnOTSSM3PublicKey; VerifyKey: TCnOTSSM3VerificationKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnOTSSM3VerifyData(nil, 0, Signature, PublicKey, VerifyKey)
  else
    Result := CnOTSSM3VerifyData(@Data[0], Length(Data), Signature, PublicKey, VerifyKey);
end;

function CnOTSSHA256GenerateKeys(var PrivateKey: TCnOTSSHA256PrivateKey;
  var PublicKey: TCnOTSSHA256PublicKey): Boolean;
var
  I: Integer;
  P: Pointer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnOTSSHA256PrivateKey));
  if Result then
  begin
    for I := Low(TCnOTSSHA256PublicKey) to High(TCnOTSSHA256PublicKey) do
    begin
      P := @PrivateKey[I];
      PublicKey[I] := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
    end;
  end;
end;

procedure CnOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSHA256PrivateKey; PublicKey: TCnOTSSHA256PublicKey;
  var OutSignature: TCnOTSSHA256Signature; var OutVerifyKey: TCnOTSSHA256VerificationKey);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSHA256Digest;
begin
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 是 1
      begin
        OutSignature[I] := PublicKey[I * 2 + 1];
        OutVerifyKey[I] := PrivateKey[I * 2 + 1];
      end
      else
      begin
        OutSignature[I] := PublicKey[I * 2];
        OutVerifyKey[I] := PrivateKey[I * 2];
      end;
    end;
  finally
    Bits.Free;
  end;
end;

function CnOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSHA256Signature; PublicKey: TCnOTSSHA256PublicKey;
  VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Cmp: TCnSHA256Digest;
  P: Pointer;
begin
  Result := False;
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      P := @VerifyKey[I];
      Cmp := SHA256Buffer(P, SizeOf(TCnSHA256Digest));    // 计算私钥的杂凑值
      if Bits.Bit[I] then
        Result := SHA256Match(Cmp, PublicKey[I * 2 + 1])  // 该位是 1，比较 1 对应的公钥
      else
        Result := SHA256Match(Cmp, PublicKey[I * 2]);     // 该位是 0，比较 0 对应的公钥

      if not Result then
        Exit;
    end;
  finally
    Bits.Free;
  end;
end;

procedure CnOTSSHA256SignBytes(const Data: TBytes; PrivateKey: TCnOTSSHA256PrivateKey;
  PublicKey: TCnOTSSHA256PublicKey; var OutSignature: TCnOTSSHA256Signature;
  var OutVerifyKey: TCnOTSSHA256VerificationKey);
begin
  if Length(Data) = 0 then
    CnOTSSHA256SignData(nil, 0, PrivateKey, PublicKey, OutSignature, OutVerifyKey)
  else
    CnOTSSHA256SignData(@Data[0], Length(Data), PrivateKey, PublicKey, OutSignature, OutVerifyKey);
end;

function CnOTSSHA256VerifyBytes(const Data: TBytes; Signature: TCnOTSSHA256Signature;
  PublicKey: TCnOTSSHA256PublicKey; VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnOTSSHA256VerifyData(nil, 0, Signature, PublicKey, VerifyKey)
  else
    Result := CnOTSSHA256VerifyData(@Data[0], Length(Data), Signature, PublicKey, VerifyKey);
end;

function CnMOTSSM3GenerateKeys(var PrivateKey: TCnMOTSSM3PrivateKey;
  var PublicKey: TCnMOTSSM3PublicKey): Boolean;
var
  I: Integer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnMOTSSM3PrivateKey));
  if Result then
    for I := Low(TCnMOTSSM3PublicKey) to High(TCnMOTSSM3PublicKey) do
      PublicKey[I] := SM3(@PrivateKey[I], SizeOf(TCnSM3Digest));
end;

procedure CnMOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSM3PrivateKey; var OutSignature: TCnMOTSSM3Signature);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSM3Digest;
  Cnt: Byte;
begin
  FillChar(OutSignature[0], SizeOf(TCnMOTSSM3Signature), 0);
  Dig := SM3(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名搁私钥
        OutSignature[I] := PrivateKey[I]
      else
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续搁私钥
        OutSignature[(SizeOf(TCnSM3Digest) * 8) + I] := PrivateKey[(SizeOf(TCnSM3Digest) * 8) + I];
    end;
  finally
    Bits.Free;
  end;
end;

function CnMOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSM3Signature; PublicKey: TCnMOTSSM3PublicKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Zero, Cmp: TCnSM3Digest;
  Cnt: Byte;
begin
  Result := False;
  FillChar(Zero[0], SizeOf(TCnSM3Digest), 0);
  Dig := SM3(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名是私钥，杂凑验证
      begin
        Cmp := SM3(@Signature[I], SizeOf(TCnSM3Digest)); // 计算私钥的杂凑值
        if not SM3Match(Cmp, PublicKey[I]) then
          Exit;
      end
      else
      begin
        if not SM3Match(Signature[I], Zero) then         // 对应位 0 得保证全 0
          Exit;
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
      end;
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续是私钥，杂凑验证
      begin
        Cmp := SM3(@Signature[(SizeOf(TCnSM3Digest) * 8) + I], SizeOf(TCnSM3Digest));
        if not SM3Match(Cmp, PublicKey[(SizeOf(TCnSM3Digest) * 8) + I]) then
          Exit;
      end
      else
      begin
        if not SM3Match(Signature[(SizeOf(TCnSM3Digest) * 8) + I], Zero) then         // 对应位 0 得保证全 0
          Exit;
      end;
    end;
    Result := True;
  finally
    Bits.Free;
  end;
end;

procedure CnMOTSSM3SignBytes(const Data: TBytes; PrivateKey: TCnMOTSSM3PrivateKey;
  var OutSignature: TCnMOTSSM3Signature);
begin
  if Length(Data) = 0 then
    CnMOTSSM3SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnMOTSSM3SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnMOTSSM3VerifyBytes(const Data: TBytes; Signature: TCnMOTSSM3Signature;
  PublicKey: TCnMOTSSM3PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnMOTSSM3VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnMOTSSM3VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

function CnMOTSSHA256GenerateKeys(var PrivateKey: TCnMOTSSHA256PrivateKey;
  var PublicKey: TCnMOTSSHA256PublicKey): Boolean;
var
  I: Integer;
  P: Pointer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnMOTSSHA256PrivateKey));
  if Result then
  begin
    for I := Low(TCnMOTSSHA256PublicKey) to High(TCnMOTSSHA256PublicKey) do
    begin
      P := @PrivateKey[I];
      PublicKey[I] := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
    end;
  end;
end;

procedure CnMOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSHA256PrivateKey; var OutSignature: TCnMOTSSHA256Signature);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSHA256Digest;
  Cnt: Byte;
begin
  FillChar(OutSignature[0], SizeOf(TCnMOTSSHA256Signature), 0);
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名搁私钥
        OutSignature[I] := PrivateKey[I]
      else
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续搁私钥
        OutSignature[(SizeOf(TCnSHA256Digest) * 8) + I] := PrivateKey[(SizeOf(TCnSHA256Digest) * 8) + I];
    end;
  finally
    Bits.Free;
  end;
end;

function CnMOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSHA256Signature; PublicKey: TCnMOTSSHA256PublicKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Zero, Cmp: TCnSHA256Digest;
  Cnt: Byte;
  P: Pointer;
begin
  Result := False;
  FillChar(Zero[0], SizeOf(TCnSHA256Digest), 0);
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名是私钥，杂凑验证
      begin
        P := @Signature[I];
        Cmp := SHA256Buffer(P, SizeOf(TCnSHA256Digest)); // 计算私钥的杂凑值
        if not SHA256Match(Cmp, PublicKey[I]) then
          Exit;
      end
      else
      begin
        if not SHA256Match(Signature[I], Zero) then         // 对应位 0 得保证全 0
          Exit;
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
      end;
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续是私钥，杂凑验证
      begin
        P := @Signature[(SizeOf(TCnSHA256Digest) * 8) + I];
        Cmp := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
        if not SHA256Match(Cmp, PublicKey[(SizeOf(TCnSHA256Digest) * 8) + I]) then
          Exit;
      end
      else
      begin
        if not SHA256Match(Signature[(SizeOf(TCnSHA256Digest) * 8) + I], Zero) then         // 对应位 0 得保证全 0
          Exit;
      end;
    end;
    Result := True;
  finally
    Bits.Free;
  end;
end;

procedure CnMOTSSHA256SignBytes(const Data: TBytes; PrivateKey: TCnMOTSSHA256PrivateKey;
  var OutSignature: TCnMOTSSHA256Signature);
begin
  if Length(Data) = 0 then
    CnMOTSSHA256SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnMOTSSHA256SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnMOTSSHA256VerifyBytes(const Data: TBytes; Signature: TCnMOTSSHA256Signature;
  PublicKey: TCnMOTSSHA256PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnMOTSSHA256VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnMOTSSHA256VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

function CnWOTSSM3GenerateKeys(var PrivateKey: TCnWOTSSM3PrivateKey;
  var PublicKey: TCnWOTSSM3PublicKey): Boolean;
var
  I, J: Integer;
  Dig: TCnSM3Digest;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSSM3PrivateKey));
  if Result then
  begin
    for I := Low(TCnWOTSSM3PublicKey) to High(TCnWOTSSM3PublicKey) do
    begin
      Dig := PrivateKey[I];
      for J := 0 to CN_WOTS_ROUND - 1 do
        Dig := SM3(@Dig[0], SizeOf(TCnSM3Digest));

      PublicKey[I] := Dig;
    end;
  end;
end;

procedure CnWOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSM3PrivateKey; var OutSignature: TCnWOTSSM3Signature);
var
  I, J: Integer;
  Dig, D: TCnSM3Digest;
  P: PByte;
  Sum, B: Word;
begin
  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSM3Digest) - 1 do
  begin
    D := PrivateKey[I];
    B := CN_WOTS_ROUND - Dig[I];             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
      D := SM3(@D[0], SizeOf(TCnSM3Digest)); // 根据字节数，用私钥计算 256 - 每个字节的杂凑次数

    OutSignature[I] := D;
    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := PrivateKey[High(TCnSM3Digest) + 1];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));
  OutSignature[High(TCnSM3Digest) + 1] := D;

  Inc(P);
  D := PrivateKey[High(TCnSM3Digest) + 2];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));
  OutSignature[High(TCnSM3Digest) + 2] := D;
end;

function CnWOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSM3Signature; PublicKey: TCnWOTSSM3PublicKey): Boolean;
var
  I, J: Integer;
  Dig, D: TCnSM3Digest;
  P: PByte;
  Sum, B: Word;
begin
  Result := False;

  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSM3Digest) - 1 do
  begin
    D := Signature[I];
    B := Dig[I];                             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
      D := SM3(@D[0], SizeOf(TCnSM3Digest)); // 根据字节数，用私钥计算每个字节的杂凑次数

    if not SM3Match(D, PublicKey[I]) then
      Exit;

    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := Signature[High(TCnSM3Digest) + 1];
  B := P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));

  if not SM3Match(D, PublicKey[High(TCnSM3Digest) + 1]) then
    Exit;

  Inc(P);
  D := Signature[High(TCnSM3Digest) + 2];
  B := P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));

  if not SM3Match(D, PublicKey[High(TCnSM3Digest) + 2]) then
    Exit;

  Result := True;
end;

procedure CnWOTSSM3SignBytes(const Data: TBytes; PrivateKey: TCnWOTSSM3PrivateKey;
  var OutSignature: TCnWOTSSM3Signature);
begin
  if Length(Data) = 0 then
    CnWOTSSM3SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnWOTSSM3SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnWOTSSM3VerifyBytes(const Data: TBytes; Signature: TCnWOTSSM3Signature;
  PublicKey: TCnWOTSSM3PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnWOTSSM3VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnWOTSSM3VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

function CnWOTSSHA256GenerateKeys(var PrivateKey: TCnWOTSSHA256PrivateKey;
  var PublicKey: TCnWOTSSHA256PublicKey): Boolean;
var
  I, J: Integer;
  Dig: TCnSHA256Digest;
  P: Pointer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSSHA256PrivateKey));
  if Result then
  begin
    for I := Low(TCnWOTSSHA256PublicKey) to High(TCnWOTSSHA256PublicKey) do
    begin
      Dig := PrivateKey[I];
      for J := 0 to CN_WOTS_ROUND - 1 do
      begin
        P := @Dig[0];
        Dig := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
      end;

      PublicKey[I] := Dig;
    end;
  end;
end;

procedure CnWOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSHA256PrivateKey; var OutSignature: TCnWOTSSHA256Signature);
var
  I, J: Integer;
  Dig, D: TCnSHA256Digest;
  P: PByte;
  Sum, B: Word;
  PB: Pointer;
begin
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSHA256Digest) - 1 do
  begin
    D := PrivateKey[I];
    B := CN_WOTS_ROUND - Dig[I];             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
    begin
      PB := @D[0];
      D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest)); // 根据字节数，用私钥计算 256 - 每个字节的杂凑次数
    end;

    OutSignature[I] := D;
    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := PrivateKey[High(TCnSHA256Digest) + 1];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;
  OutSignature[High(TCnSHA256Digest) + 1] := D;

  Inc(P);
  D := PrivateKey[High(TCnSHA256Digest) + 2];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;
  OutSignature[High(TCnSHA256Digest) + 2] := D;
end;

function CnWOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSHA256Signature; PublicKey: TCnWOTSSHA256PublicKey): Boolean;
var
  I, J: Integer;
  Dig, D: TCnSHA256Digest;
  P: PByte;
  Sum, B: Word;
  PB: Pointer;
begin
  Result := False;

  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSHA256Digest) - 1 do
  begin
    D := Signature[I];
    B := Dig[I];                             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
    begin
      PB := @D[0];
      D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest)); // 根据字节数，用私钥计算每个字节的杂凑次数
    end;

    if not SHA256Match(D, PublicKey[I]) then
      Exit;

    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := Signature[High(TCnSHA256Digest) + 1];
  B := P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;

  if not SHA256Match(D, PublicKey[High(TCnSHA256Digest) + 1]) then
    Exit;

  Inc(P);
  D := Signature[High(TCnSHA256Digest) + 2];
  B := P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;

  if not SHA256Match(D, PublicKey[High(TCnSHA256Digest) + 2]) then
    Exit;

  Result := True;
end;

procedure CnWOTSSHA256SignBytes(const Data: TBytes; PrivateKey: TCnWOTSSHA256PrivateKey;
  var OutSignature: TCnWOTSSHA256Signature);
begin
  if Length(Data) = 0 then
    CnWOTSSHA256SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnWOTSSHA256SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnWOTSSHA256VerifyBytes(const Data: TBytes; Signature: TCnWOTSSHA256Signature;
  PublicKey: TCnWOTSSHA256PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnWOTSSHA256VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnWOTSSHA256VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

// ================== W-OTS Plus 实现 (基于 SM3 杂凑算法) =====================

// 正确的 WOTS+ 链式计算函数（使用 KeyHash）
function WOTSPlusSM3Chain(const Input: TCnSM3Digest;      // 输入值
  Steps: Integer; const Masks: TCnWOTSPlusSM3Mask;        // 要走的步数、15个掩码
  StartIndex: Integer = 0): TCnSM3Digest;                 // 从掩码数组的哪个位置开始
var
  I: Integer;
  Current: TCnSM3Digest;
begin
  // 如果步数为 0，直接返回输入
  if Steps <= 0 then
  begin
    Result := Input;
    Exit;
  end;

  Current := Input;

  // 执行指定步数的链式计算
  for I := StartIndex to StartIndex + Steps - 1 do
  begin
    // 检查掩码索引是否有效
    if I > High(Masks) then
      raise Exception.Create(SCnErrorOTSInvalidParam);

    // WOTS+ 核心：KeyHash(k, x) = Hash(k ⊕ x)
    // 注意：这里使用掩码作为密钥，与当前值异或
    MemoryXor(@Current[0], @Masks[I][0], SizeOf(TCnSM3Digest), @Current[0]);

    // 计算杂凑
    Current := SM3(PAnsiChar(@Current[0]), SizeOf(TCnSM3Digest));
  end;

  Result := Current;
end;

function CnWOTSPlusSM3GenerateKeys(out PrivateKey: TCnWOTSPlusSM3PrivateKey;
  out PublicKey: TCnWOTSPlusSM3PublicKey; out Mask: TCnWOTSPlusSM3Mask): Boolean;
var
  I: Integer;
begin
  Result := False;

  // 生成私钥 - 67 个完全随机的 SHA256 摘要
  if not CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSPlusSHA256PrivateKey)) then
    Exit;

  // 生成掩码 - 15 个完全随机的 SHA256 摘要
  if not CnRandomFillBytes(@Mask[0], SizeOf(TCnWOTSPlusSHA256Mask)) then
    Exit;

  // 对每个私钥元素应用完整的杂凑链（w-1 = 15 步）
  for I := 0 to High(PrivateKey) do
    PublicKey[I] := WOTSPlusSM3Chain(PrivateKey[I], CN_WOTS_PLUS_W - 1, Mask, 0);

  Result := True;
end;

function CnWOTSPlusSM3GenerateKeysFromMask(var PrivateKey: TCnWOTSPlusSM3PrivateKey;
  var PublicKey: TCnWOTSPlusSM3PublicKey; const Mask: TCnWOTSPlusSM3Mask): Boolean;
var
  I: Integer;
begin
  Result := False;

  // 生成私钥 - 67 个完全随机的 SHA256 摘要
  if not CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSPlusSHA256PrivateKey)) then
    Exit;

  // 对每个私钥元素应用完整的杂凑链（w-1 = 15 步）
  for I := 0 to High(PrivateKey) do
    PublicKey[I] := WOTSPlusSM3Chain(PrivateKey[I], CN_WOTS_PLUS_W - 1, Mask, 0);

  Result := True;
end;

procedure CnWOTSPlusSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSPlusSM3PrivateKey; const Mask: TCnWOTSPlusSM3Mask;
  var OutSignature: TCnWOTSPlusSM3Signature);
var
  I: Integer;
  Dig: TCnSM3Digest;
  MB: array[0..63] of Byte;  // 64 个 4-bit 块
  Checksum: Integer;  // 改为 Integer 以容纳更大的值
  C: array[0..2] of Byte; // 3 个 4-bit 校验和块
begin
  if (Data = nil) or (DataByteLen = 0) then
    Dig := SM3(nil, 0)
  else
    Dig := SM3(Data, DataByteLen);

  // 将 32 字节消息哈希分解为 64 个 4-bit 块
  for I := 0 to 31 do
  begin
    MB[I * 2] := (Dig[I] and $0F);      // 低 4 位
    MB[I * 2 + 1] := (Dig[I] shr 4);    // 高 4 位
  end;

  // 计算校验和
  Checksum := 0;
  for I := 0 to 63 do
    Checksum := Checksum + (CN_WOTS_PLUS_W - 1 - MB[I]);

  // 将校验和分解为 3 个 4-bit 块
  C[0] := Checksum and $0F;
  C[1] := (Checksum shr 4) and $0F;
  C[2] := (Checksum shr 8) and $0F;

  // 计算前 64 个签名元素（消息部分）
  for I := 0 to 63 do
    OutSignature[I] := WOTSPlusSM3Chain(PrivateKey[I], MB[I], Mask, 0);

  // 计算校验和部分的签名元素（3 个块）
  for I := 0 to 2 do
    OutSignature[64 + I] := WOTSPlusSM3Chain(PrivateKey[64 + I],
      CN_WOTS_PLUS_W - 1 - C[I], Mask, 0);
end;

function CnWOTSPlusSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSPlusSM3Signature; PublicKey: TCnWOTSPlusSM3PublicKey;
  const Mask: TCnWOTSPlusSM3Mask): Boolean;
var
  I: Integer;
  Dig: TCnSM3Digest;
  MB: array[0..63] of Byte;  // 64 个 4-bit 块
  Checksum: Integer;
  C: array[0..2] of Byte; // 3 个 4-bit 校验和块
  ComputedPublicKey: TCnWOTSPlusSM3PublicKey;
begin
  Result := False;
  try
    if (Data = nil) or (DataByteLen = 0) then
      Dig := SM3(nil, 0)
    else
      Dig := SM3(Data, DataByteLen);

    // 将 32 字节哈希值分解为 64 个 4-bit 块
    for I := 0 to 31 do
    begin
      MB[I * 2] := (Dig[I] and $0F);      // 低 4 位
      MB[I * 2 + 1] := (Dig[I] shr 4);    // 高 4 位
    end;

    // 计算校验和
    Checksum := 0;
    for I := 0 to 63 do
      Checksum := Checksum + (CN_WOTS_PLUS_W - 1 - MB[I]);

    // 将校验和分解为 3 个 4-bit 块
    C[0] := Checksum and $0F;
    C[1] := (Checksum shr 4) and $0F;
    C[2] := (Checksum shr 8) and $0F;

    // 重新计算公钥进行验证

    // 消息部分：从签名位置开始，计算剩余步数到达公钥位置
    for I := 0 to 63 do
      ComputedPublicKey[I] := WOTSPlusSM3Chain(Signature[I],
        CN_WOTS_PLUS_W - 1 - MB[I], Mask, MB[I]);

    // 校验和部分：从签名位置开始，计算剩余步数到达公钥位置
    for I := 0 to 2 do
      ComputedPublicKey[64 + I] := WOTSPlusSM3Chain(Signature[64 + I],
        C[I], Mask, CN_WOTS_PLUS_W - 1 - C[I]);

    // 比较计算出的公钥和提供的公钥
    Result := CompareMem(@ComputedPublicKey[0], @PublicKey[0],
      SizeOf(TCnWOTSPlusSM3PublicKey));
  except
    Result := False;
  end;
end;

procedure CnWOTSPlusSM3SignBytes(const Data: TBytes; PrivateKey: TCnWOTSPlusSM3PrivateKey;
  const Mask: TCnWOTSPlusSM3Mask; var OutSignature: TCnWOTSPlusSM3Signature);
begin
  if Length(Data) = 0 then
    CnWOTSPlusSM3SignData(nil, 0, PrivateKey, Mask, OutSignature)
  else
    CnWOTSPlusSM3SignData(@Data[0], Length(Data), PrivateKey, Mask, OutSignature);
end;

function CnWOTSPlusSM3VerifyBytes(const Data: TBytes; Signature: TCnWOTSPlusSM3Signature;
  PublicKey: TCnWOTSPlusSM3PublicKey; const Mask: TCnWOTSPlusSM3Mask): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnWOTSPlusSM3VerifyData(nil, 0, Signature, PublicKey, Mask)
  else
    Result := CnWOTSPlusSM3VerifyData(@Data[0], Length(Data), Signature, PublicKey, Mask);
end;

// ==================== W-OTS+ 实现 (基于 SHA256 杂凑算法) =====================

// 正确的 WOTS+ 链式计算函数（使用 KeyHash）
function WOTSPlusSHA256Chain(const Input: TCnSHA256Digest;      // 输入值
  Steps: Integer; const Masks: TCnWOTSPlusSHA256Mask;           // 要走的步数、15个掩码
  StartIndex: Integer = 0): TCnSHA256Digest;                    // 从掩码数组的哪个位置开始
var
  I: Integer;
  Current: TCnSHA256Digest;
begin
  // 如果步数为 0，直接返回输入
  if Steps <= 0 then
  begin
    Result := Input;
    Exit;
  end;

  Current := Input;

  // 执行指定步数的链式计算
  for I := StartIndex to StartIndex + Steps - 1 do
  begin
    // 检查掩码索引是否有效
    if I > High(Masks) then
      raise Exception.Create(SCnErrorOTSInvalidParam);

    // WOTS+ 核心：KeyHash(k, x) = Hash(k ⊕ x)
    // 注意：这里使用掩码作为密钥，与当前值异或
    MemoryXor(@Current[0], @Masks[I][0], SizeOf(TCnSHA256Digest), @Current[0]);

    // 计算杂凑
    Current := SHA256(PAnsiChar(@Current[0]), SizeOf(TCnSHA256Digest));
  end;

  Result := Current;
end;

function CnWOTSPlusSHA256GenerateKeys(out PrivateKey: TCnWOTSPlusSHA256PrivateKey;
  out PublicKey: TCnWOTSPlusSHA256PublicKey; out Mask: TCnWOTSPlusSHA256Mask): Boolean;
var
  I: Integer;
begin
  Result := False;

  // 生成私钥 - 67 个完全随机的 SHA256 摘要
  if not CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSPlusSHA256PrivateKey)) then
    Exit;

  // 生成掩码 - 15 个完全随机的 SHA256 摘要
  if not CnRandomFillBytes(@Mask[0], SizeOf(TCnWOTSPlusSHA256Mask)) then
    Exit;

  // 对每个私钥元素应用完整的杂凑链（w-1 = 15 步）
  for I := 0 to High(PrivateKey) do
    PublicKey[I] := WOTSPlusSHA256Chain(PrivateKey[I], CN_WOTS_PLUS_W - 1, Mask, 0);

  Result := True;
end;

function CnWOTSPlusSHA256GenerateKeysFromMask(var PrivateKey: TCnWOTSPlusSHA256PrivateKey;
  var PublicKey: TCnWOTSPlusSHA256PublicKey; const Mask: TCnWOTSPlusSHA256Mask): Boolean;
var
  I: Integer;
begin
  Result := False;

  // 生成私钥 - 67 个完全随机的 SHA256 摘要
  if not CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSPlusSHA256PrivateKey)) then
    Exit;

  // 对每个私钥元素应用完整的杂凑链（w-1 = 15 步）
  for I := 0 to High(PrivateKey) do
    PublicKey[I] := WOTSPlusSHA256Chain(PrivateKey[I], CN_WOTS_PLUS_W - 1, Mask, 0);

  Result := True;
end;

procedure CnWOTSPlusSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSPlusSHA256PrivateKey; const Mask: TCnWOTSPlusSHA256Mask;
  var OutSignature: TCnWOTSPlusSHA256Signature);
var
  I: Integer;
  Dig: TCnSHA256Digest;
  MB: array[0..63] of Byte;  // 64 个 4-bit 块
  Checksum: Integer;  // 改为 Integer
  C: array[0..2] of Byte; // 3 个 4-bit 校验和块
begin
  if (Data = nil) or (DataByteLen = 0) then
    Dig := SHA256(nil, 0)
  else
    Dig := SHA256(Data, DataByteLen);

  // 将 32 字节消息哈希分解为 64 个 4-bit 块
  for I := 0 to 31 do
  begin
    MB[I * 2] := (Dig[I] and $0F);      // 低 4 位
    MB[I * 2 + 1] := (Dig[I] shr 4);    // 高 4 位
  end;

  // 计算校验和
  Checksum := 0;
  for I := 0 to 63 do
    Checksum := Checksum + (CN_WOTS_PLUS_W - 1 - MB[I]);

  // 将校验和分解为 3 个 4-bit 块
  C[0] := Checksum and $0F;
  C[1] := (Checksum shr 4) and $0F;
  C[2] := (Checksum shr 8) and $0F;

  // 计算前 64 个签名元素（消息部分）
  for I := 0 to 63 do
    OutSignature[I] := WOTSPlusSHA256Chain(PrivateKey[I], MB[I], Mask, 0);

  // 计算校验和部分的签名元素（3 个块）
  for I := 0 to 2 do
    OutSignature[64 + I] := WOTSPlusSHA256Chain(PrivateKey[64 + I],
      CN_WOTS_PLUS_W - 1 - C[I], Mask, 0);
end;

function CnWOTSPlusSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSPlusSHA256Signature; PublicKey: TCnWOTSPlusSHA256PublicKey;
  const Mask: TCnWOTSPlusSHA256Mask): Boolean;
var
  I: Integer;
  Dig: TCnSHA256Digest;
  MB: array[0..63] of Byte;  // 64 个 4-bit 块
  Checksum: Integer;
  C: array[0..2] of Byte; // 3 个 4-bit 校验和块
  ComputedPublicKey: TCnWOTSPlusSHA256PublicKey;
begin
  Result := False;
  try
    if (Data = nil) or (DataByteLen = 0) then
      Dig := SHA256(nil, 0)
    else
      Dig := SHA256(Data, DataByteLen);

    // 将 32 字节哈希值分解为 64 个 4-bit 块
    for I := 0 to 31 do
    begin
      MB[I * 2] := (Dig[I] and $0F);      // 低 4 位
      MB[I * 2 + 1] := (Dig[I] shr 4);    // 高 4 位
    end;

    // 计算校验和
    Checksum := 0;
    for I := 0 to 63 do
      Checksum := Checksum + (CN_WOTS_PLUS_W - 1 - MB[I]);

    // 将校验和分解为 3 个 4-bit 块
    C[0] := Checksum and $0F;
    C[1] := (Checksum shr 4) and $0F;
    C[2] := (Checksum shr 8) and $0F;

    // 重新计算公钥进行验证

    // 消息部分
    for I := 0 to 63 do
      ComputedPublicKey[I] := WOTSPlusSHA256Chain(Signature[I],
        CN_WOTS_PLUS_W - 1 - MB[I], Mask, MB[I]);

    // 校验和部分
    for I := 0 to 2 do
      ComputedPublicKey[64 + I] := WOTSPlusSHA256Chain(Signature[64 + I],
        C[I], Mask, CN_WOTS_PLUS_W - 1 - C[I]);

    // 比较计算出的公钥和提供的公钥
    Result := CompareMem(@ComputedPublicKey[0], @PublicKey[0],
      SizeOf(TCnWOTSPlusSHA256PublicKey));
  except
    Result := False;
  end;
end;

procedure CnWOTSPlusSHA256SignBytes(const Data: TBytes; PrivateKey: TCnWOTSPlusSHA256PrivateKey;
  const Mask: TCnWOTSPlusSHA256Mask; var OutSignature: TCnWOTSPlusSHA256Signature);
begin
  if Length(Data) = 0 then
    CnWOTSPlusSHA256SignData(nil, 0, PrivateKey, Mask, OutSignature)
  else
    CnWOTSPlusSHA256SignData(@Data[0], Length(Data), PrivateKey, Mask, OutSignature);
end;

function CnWOTSPlusSHA256VerifyBytes(const Data: TBytes; Signature: TCnWOTSPlusSHA256Signature;
  PublicKey: TCnWOTSPlusSHA256PublicKey; const Mask: TCnWOTSPlusSHA256Mask): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnWOTSPlusSHA256VerifyData(nil, 0, Signature, PublicKey, Mask)
  else
    Result := CnWOTSPlusSHA256VerifyData(@Data[0], Length(Data), Signature, PublicKey, Mask);
end;

end.
