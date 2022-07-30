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
*           目前实现了 GHash128（似乎也叫 GMAC）以及 CMAC，并基于两者实现了
*           AES128/192/256/SM4 的 GCM/CCM（但 CCM 里的 CMAC 和单纯的 CMAC 还不同）
*           GCM 参考文档《The Galois/Counter Mode of Operation (GCM)》以及
*           《NIST Special Publication 800-38D》以及 RFC 8998 的例子数据
*           CMAC 参考文档 NIST Special Publication 800-38B:
*           《Recommendation for Block Cipher Modes of Operation:
*           The CMAC Mode for Authentication》 以及 RFC 4993 的例子数据(AES-128)
*           CCM 参考文档 NIST Special Publication 800-38C:
*          《Recommendation for Block Cipher Modes of Operation:
*           The CCM Mode for Authentication and Confidentiality》以及 RFC 3610 的例子数据(AES-128)
*           注意 CCM 有两个编译期的参数，摘要长度 CCM_M_LEN 和明文长度的字节长度 CCM_L_LEN
*           NIST 800-38C 例子中是 4、8，RFC 3610 例子中是 8、2，RFC 8998 是 16、？
*           俩参数不同是无法通过 CCM 正确加解密的。
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.07.27 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative;

const
  AEAD_BLOCK  = 16;       // GHASH/GCM/CMAC 以及 AES/SM4 等的分组都是 16 字节

  GCM_NONCE_LENGTH = 12;  // 12 字节的 Nonce 与计数器拼成完整 Iv

  CCM_M_LEN = 8;         // CCM 认证 Tag 默认长 12 字节，取值范围 4 到 16，实际存放的是 (M - 2) / 2

  CCM_L_LEN = 2;         // 明文长度所占的字节数，取值范围 2 到 8，8 代表 Int64，实际存放的是 L - 1

  CCM_NONCE = 15 - CCM_L_LEN;

type
  T128BitsBuffer = array[0..AEAD_BLOCK - 1] of Byte;
  {* AEAD 中所有 128 位计算的分块}

  TGHash128Key = array[0..AEAD_BLOCK - 1] of Byte;
  {* GHash128 的密钥}

  TGHash128Tag    = array[0..AEAD_BLOCK - 1] of Byte;
  {* GHash128 的计算结果}

  TGHash128Context = packed record
  {* 用于多次分块计算的 GHash128 上下文结构}
    HashKey:     T128BitsBuffer;
    State:       T128BitsBuffer;
    AADByteLen:  Integer;
    DataByteLen: Integer;
  end;

  TGCM128Key = array[0..AEAD_BLOCK - 1] of Byte;
  {* GCM 模式的密钥，内部无论用 AES 还是 SM4 均为 16 字节}

  TGCM128Tag    = array[0..AEAD_BLOCK - 1] of Byte;
  {* GCM 的计算结果}

  TCMAC128Key = array[0..AEAD_BLOCK - 1] of Byte;
  {* CMAC 模式的密钥，内部无论用 AES 还是 SM4 均为 16 字节}

  TCMAC128Tag    = array[0..AEAD_BLOCK - 1] of Byte;
  {* CMAC 的计算结果}

  TCCM128Tag = array[0..CCM_M_LEN - 1] of Byte;
  {* CCM 的计算结果}

procedure GMulBlock128(var X, Y: T128BitsBuffer; var R: T128BitsBuffer);
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

// ======================= AES/SM4-GCM 字节数组加密函数 ========================

function AES128GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对明文进行 AES-128-GCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

function AES192GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对明文进行 AES-192-GCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

function AES256GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对明文进行 AES-256-GCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

function SM4GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对明文进行 SM4-GCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

// ======================== AES/SM4-GCM 数据块加密函数 =========================

procedure AES128GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
{* 使用密码、初始化向量、额外数据对明文进行 AES-128-GCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

procedure AES192GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
{* 使用密码、初始化向量、额外数据对明文进行 AES-192-GCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

procedure AES256GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
{* 使用密码、初始化向量、额外数据对明文进行 AES-256-GCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

procedure SM4GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
{* 使用密码、初始化向量、额外数据对明文进行 SM4-GCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

// ======================= AES/SM4-GCM 字节数组解密函数 ========================

function AES128GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对密文进行 AES-128-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

function AES192GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对密文进行 AES-192-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

function AES256GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对密文进行 AES-256-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

function SM4GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
{* 使用密码、初始化向量、额外数据对密文进行 SM4-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

// ======================== AES/SM4-GCM 数据块解密函数 =========================

function AES128GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
{* 使用密码、初始化向量、额外数据对密文进行 AES-128-GCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

function AES192GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
{* 使用密码、初始化向量、额外数据对密文进行 AES-192-GCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

function AES256GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
{* 使用密码、初始化向量、额外数据对密文进行 AES-256-GCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

function SM4GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
{* 使用密码、初始化向量、额外数据对密文进行 SM4-GCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

// ======================= AES/SM4-CMAC 字节数组杂凑函数 =======================

function AES128CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
{* 以指定的 Key 对数据进行 AES-128-CMAC 计算，返回计算出的 Tag，参数均为字节数组}

function AES192CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
{* 以指定的 Key 对数据进行 AES-192-CMAC 计算，返回计算出的 Tag，参数均为字节数组}

function AES256CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
{* 以指定的 Key 对数据进行 AES-256-CMAC 计算，返回计算出的 Tag，参数均为字节数组}

function SM4CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
{* 以指定的 Key 对数据进行 SM4-CMAC 计算，返回计算出的 Tag，参数均为字节数组}

// ======================== AES/SM4-CMAC 数据块杂凑函数 ========================

function AES128CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
{* 以指定的 Key 对数据进行 AES-128-CMAC 计算，返回计算出的 Tag，参数均为内存块}

function AES192CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
{* 以指定的 Key 对数据进行 AES-192-CMAC 计算，返回计算出的 Tag，参数均为字节数组}

function AES256CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
{* 以指定的 Key 对数据进行 AES-256-CMAC 计算，返回计算出的 Tag，参数均为字节数组}

function SM4CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
{* 以指定的 Key 对数据进行 SM4-CMAC 计算，返回计算出的 Tag，参数均为字节数组}

// ======================= AES/SM4-CCM 字节数组加密函数 ========================

function AES128CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对明文进行 AES-128-CCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

function AES192CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对明文进行 AES-192-CCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

function AES256CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对明文进行 AES-256-CCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

function SM4CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对明文进行 SM4-CCM 加密，返回密文
  以上参数与返回值均为字节数组，并在 OutTag 中返回认证数据供解密验证}

// ======================= AES/SM4-CCM 字节数组解密函数 ========================

function AES128CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对密文进行 AES-128-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

function AES192CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对密文进行 AES-192-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

function AES256CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对密文进行 AES-256-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

function SM4CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
{* 使用密码、临时数据、额外数据对密文进行 SM4-GCM 解密并验证，成功则返回明文
  以上参数与返回值均为字节数组，并验证 InTag 是否合法，不合法返回 nil}

// ======================== AES/SM4-CCM 数据块加密函数 =========================

procedure AES128CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
{* 使用密码、临时数据、额外数据对明文进行 AES-128-CCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

procedure AES192CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
{* 使用密码、临时数据、额外数据对明文进行 AES-192-CCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

procedure AES256CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
{* 使用密码、临时数据、额外数据对明文进行 AES-256-CCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

procedure SM4CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
{* 使用密码、临时数据、额外数据对明文进行 SM4-CCM 加密，返回密文至 OutEnData 所指的区域中
  OutEnData 所指的区域长度须至少为 PlainByteLength，否则可能引发越界等严重后果
  以上参数均为内存块并指定字节长度的形式，并在 OutTag 中返回认证数据供解密验证}

// ======================== AES/SM4-GCM 数据块解密函数 =========================

function AES128CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
{* 使用密码、临时数据、额外数据对密文进行 AES-128-CCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

function AES192CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
{* 使用密码、临时数据、额外数据对密文进行 AES-192-CCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

function AES256CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
{* 使用密码、临时数据、额外数据对密文进行 AES-256-CCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

function SM4CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
{* 使用密码、临时数据、额外数据对密文进行 SM4-CCM 解密并验证，
  成功则返回 True 并将明文返回至 OutPlainData 所指的区域中，
  以上参数均为内存块并指定字节长度的形式，并验证 InTag 是否合法，不合法返回 False}

implementation

uses
  CnSM4, CnAES;

const
  GHASH_POLY: T128BitsBuffer = ($E1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  CMAC_POLY: T128BitsBuffer =  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, $87);

type
  TAEADEncryptType = (aetAES128, aetAES192, aetAES256, aetSM4);
  {* 支持的四种对称加密类型}

  TAEADContext = packed record
  case TAEADEncryptType of
    aetAES128: (ExpandedKey128: TAESExpandedKey128);
    aetAES192: (ExpandedKey192: TAESExpandedKey192);
    aetAES256: (ExpandedKey256: TAESExpandedKey256);
    aetSM4:    (SM4Context: TSM4Context);
  end;

// 注意此处判断字节内 Bit 的顺序也是字节内的高位是 0
function AeadIsBitSet(AMem: Pointer; N: Integer): Boolean;
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

procedure MoveMost128(const Source; var Dest; ByteLen: Integer);
begin
  MoveMost(Source, Dest, ByteLen, AEAD_BLOCK);
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
procedure GMulBlock128(var X, Y: T128BitsBuffer; var R: T128BitsBuffer);
var
  I: Integer;
  Z, V: T128BitsBuffer;
  B: Boolean;
begin
  FillChar(Z[0], SizeOf(T128BitsBuffer), 0);
  Move(X[0], V[0], SizeOf(T128BitsBuffer));

  for I := 0 to 127 do
  begin
    if AeadIsBitSet(@Y[0], I) then
      MemoryXor(@Z[0], @V[0], SizeOf(T128BitsBuffer), @Z[0]);

    B := AeadIsBitSet(@V[0], 127); // 判断大整数的高位是否是 1
    MemoryShiftRight(@V[0], nil, SizeOf(T128BitsBuffer), 1);
    if B then
      MemoryXor(@V[0], @GHASH_POLY[0], SizeOf(T128BitsBuffer), @V[0]);
  end;
  Move(Z[0], R[0], SizeOf(T128BitsBuffer));
end;

procedure GHash128(var HashKey: TGHash128Key; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer; var OutTag: TGHash128Tag);
var
  AL, DL: Integer;
  AL64, DL64: Int64;
  X, Y, H: T128BitsBuffer;
begin
  // 对比 GHash(H, A, C)，Data 是 C，AAD 是 A，Key 是 H
  // 按 16 字节分，C 有 m 块，A 有 n 块（末块都可能不满），
  // 共有 m + n 轮针对数据的 GaloisMulBlock，再加一轮位长度

  FillChar(X[0], SizeOf(T128BitsBuffer), 0);  // 初始全 0
  Move(HashKey[0], H[0], SizeOf(T128BitsBuffer));

  AL := AADByteLength;
  DL := DataByteLength;
  if Data = nil then
    DL := 0;
  if AAD = nil then
    AL := 0;

  // 算整块 A
  while AL >= AEAD_BLOCK do
  begin
    Move(AAD^, Y[0], AEAD_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);  // 一轮计算结果再次放入 X

    AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK);
    Dec(AL, AEAD_BLOCK);
  end;

  // 算余块 A，如果有的话
  if AL > 0 then
  begin
    FillChar(Y[0], SizeOf(T128BitsBuffer), 0);
    Move(AAD^, Y[0], AL);

    MemoryXor(@Y[0], @X[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // 算整块 C
  while DL >= AEAD_BLOCK do
  begin
    Move(Data^, Y[0], AEAD_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);  // 一轮计算结果再次放入 X

    Data := Pointer(TCnNativeInt(Data) + AEAD_BLOCK);
    Dec(DL, AEAD_BLOCK);
  end;

  // 算余块 C，如果有的话
  if DL > 0 then
  begin
    FillChar(Y[0], SizeOf(T128BitsBuffer), 0);
    Move(Data^, Y[0], DL);

    MemoryXor(@Y[0], @X[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // 最后再算一轮长度，A 和 C 各四字节拼起来，拼接要求符合网络标准与阅读习惯也就是 BigEndian
  FillChar(Y[0], SizeOf(T128BitsBuffer), 0);
  AL64 := Int64HostToNetwork(AADByteLength * 8);
  DL64 := Int64HostToNetwork(DataByteLength * 8);

  Move(AL64, Y[0], SizeOf(Int64));
  Move(DL64, Y[SizeOf(Int64)], SizeOf(Int64));

  MemoryXor(@Y[0], @X[0], SizeOf(T128BitsBuffer), @Y[0]);
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
  Y: T128BitsBuffer;
begin
  FillChar(Ctx.State[0], SizeOf(T128BitsBuffer), 0);  // 初始全 0
  Move(HashKey[0], Ctx.HashKey[0], SizeOf(T128BitsBuffer));

  Ctx.DataByteLen := 0;
  Ctx.AADByteLen := AADByteLength;
  if AAD = nil then
    Ctx.AADByteLen := 0;

  // 算整块 A
  while AADByteLength >= AEAD_BLOCK do
  begin
    Move(AAD^, Y[0], AEAD_BLOCK);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);  // 一轮计算结果再次放入 Ctx.State

    AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK);
    Dec(AADByteLength, AEAD_BLOCK);
  end;

  // 算余块 A，如果有的话
  if AADByteLength > 0 then
  begin
    FillChar(Y[0], SizeOf(T128BitsBuffer), 0);
    Move(AAD^, Y[0], AADByteLength);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);
  end;
end;

procedure GHash128Update(var Ctx: TGHash128Context; Data: Pointer; DataByteLength: Integer);
var
  Y: T128BitsBuffer;
begin
  if (Data = nil) or (DataByteLength <= 0) then
    Exit;

  Ctx.DataByteLen := Ctx.DataByteLen + DataByteLength;

  // 算整块 C
  while DataByteLength >= AEAD_BLOCK do
  begin
    Move(Data^, Y[0], AEAD_BLOCK);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);  // 一轮计算结果再次放入 Ctx.State

    Data := Pointer(TCnNativeInt(Data) + AEAD_BLOCK);
    Dec(DataByteLength, AEAD_BLOCK);
  end;

  // 算余块 C，如果有的话
  if DataByteLength > 0 then
  begin
    FillChar(Y[0], SizeOf(T128BitsBuffer), 0);
    Move(Data^, Y[0], DataByteLength);

    MemoryXor(@Y[0], @Ctx.State[0], SizeOf(T128BitsBuffer), @Y[0]);
    GMulBlock128(Y, Ctx.HashKey, Ctx.State);
  end;
end;

procedure GHash128Finish(var Ctx: TGHash128Context; var Output: TGHash128Tag);
var
  Y: T128BitsBuffer;
  AL64, DL64: Int64;
begin
  // 最后再算一轮长度，A 和 C 各四字节拼起来
  FillChar(Y[0], SizeOf(T128BitsBuffer), 0);
  AL64 := Int64HostToNetwork(Ctx.AADByteLen * 8);
  DL64 := Int64HostToNetwork(Ctx.DataByteLen * 8);

  Move(AL64, Y[0], SizeOf(Int64));
  Move(DL64, Y[SizeOf(Int64)], SizeOf(Int64));

  MemoryXor(@Y[0], @Ctx.State[0], SizeOf(T128BitsBuffer), @Y[0]);
  GMulBlock128(Y, Ctx.HashKey, Ctx.State); // 再乘一轮，

  Move(Ctx.State[0], Output[0], SizeOf(TGHash128Tag)); // 结果放 Output
end;

// 根据对称加密算法类型初始化加密密钥结构，注意不需要解密密钥结构
procedure AEADEncryptInit(var Context: TAEADContext; Key: Pointer;
  KeyByteLength: Integer; EncryptType: TAEADEncryptType);
var
  Key128: TAESKey128;
  Key192: TAESKey192;
  Key256: TAESKey256;
  SM4Key: TSM4Key;
begin
  FillChar(Context, SizeOf(TAEADContext), 0);

  case EncryptType of
    aetAES128:
      begin
        MoveMost128(Key^, Key128[0], KeyByteLength);
        ExpandAESKeyForEncryption(Key128, Context.ExpandedKey128);
      end;
    aetAES192:
      begin
        MoveMost128(Key^, Key192[0], KeyByteLength);
        ExpandAESKeyForEncryption(Key192, Context.ExpandedKey192);
      end;
    aetAES256:
      begin
        MoveMost128(Key^, Key256[0], KeyByteLength);
        ExpandAESKeyForEncryption(Key256, Context.ExpandedKey256);
      end;
    aetSM4:
      begin
        MoveMost128(Key^, SM4Key[0], KeyByteLength);
        SM4SetKeyEnc(Context.SM4Context, @SM4Key[0]);
      end;
  end;
end;

// 根据对称加密算法类型加密一个块，各块串起来就是加密结果，注意不需要块解密
procedure AEADEncryptBlock(var Context: TAEADContext; var InData, OutData: T128BitsBuffer;
  EncryptType: TAEADEncryptType);
begin
  case EncryptType of
    aetAES128: EncryptAES(TAESBuffer(InData), Context.ExpandedKey128, TAESBuffer(OutData));
    aetAES192: EncryptAES(TAESBuffer(InData), Context.ExpandedKey192, TAESBuffer(OutData));
    aetAES256: EncryptAES(TAESBuffer(InData), Context.ExpandedKey256, TAESBuffer(OutData));
    aetSM4:    SM4OneRound(@(Context.SM4Context.Sk[0]), @InData[0], @OutData[0]);
  end;
end;

// 根据 Key、Iv、明文和额外数据，计算 GCM 加密密文与认证结果
procedure GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; EnData: Pointer; var OutTag: TGCM128Tag;
  EncryptType: TAEADEncryptType);
var
  H: TGHash128Key;
  Y, Y0: T128BitsBuffer; // Y 拼合了计数器的内容
  Cnt, M: Cardinal;      // 计数器
  C: T128BitsBuffer;      // 加密中间数据存储地
  AeadCtx: TAEADContext;
  GHashCtx: TGHash128Context;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Iv = nil then
    IvByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  AEADEncryptInit(AeadCtx, Key, KeyByteLength, EncryptType);

  // 计算 Enc(Key, 128 个 0)，得到 H
  FillChar(H[0], SizeOf(H), 0);
  AEADEncryptBlock(AeadCtx, T128BitsBuffer(H), T128BitsBuffer(H), EncryptType);

  // 初始化计数器，以后 Cnt 自增并塞进 Y 的后 32 位中
  if IvByteLength = GCM_NONCE_LENGTH then
  begin
    Move(Iv^, Y[0], GCM_NONCE_LENGTH);
    Cnt := 1;
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));
  end
  else
  begin
    GHash128(H, Iv, IvByteLength, nil, 0, TGHash128Tag(Y));
    Move(Y[GCM_NONCE_LENGTH], Cnt, SizeOf(Cardinal));
    ReverseMemory(@Cnt, SizeOf(Cardinal));
  end;

  // 先把最开始的 Y 值的加密结果计算出来
  AEADEncryptBlock(AeadCtx, T128BitsBuffer(Y), T128BitsBuffer(Y0), EncryptType);

  // 初始化 GHash
  GHash128Start(GHashCtx, H, AAD, AADByteLength);

  // 开始循环加密整块
  while PlainByteLength >= AEAD_BLOCK do
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 对 Y 加密 C 暂时得到本块的加密结果
    AEADEncryptBlock(AeadCtx, T128BitsBuffer(Y), C, EncryptType);

    // 和明文异或，C 得到异或后的结果，完整块
    MemoryXor(PlainData, @C[0], SizeOf(T128BitsBuffer), @C[0]);

    // 存起密文
    Move(C[0], EnData^, SizeOf(T128BitsBuffer));

    // C 进行 GHash
    GHash128Update(GHashCtx, @C[0], SizeOf(T128BitsBuffer));

    // 准备下一步
    PlainData := Pointer(TCnNativeInt(PlainData) + AEAD_BLOCK);
    EnData := Pointer(TCnNativeInt(EnData) + AEAD_BLOCK);
    Dec(PlainByteLength, AEAD_BLOCK);
  end;

  if PlainByteLength > 0 then
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 对 Y 加密 C 暂时得到本块的加密结果
    AEADEncryptBlock(AeadCtx, T128BitsBuffer(Y), C, EncryptType);

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
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; PlainData: Pointer; var InTag: TGCM128Tag;
  EncryptType: TAEADEncryptType): Boolean;
var
  H: TGHash128Key;
  Y, Y0: T128BitsBuffer;// Y 拼合了计数器的内容
  Cnt, M: Cardinal;      // 计数器
  C: T128BitsBuffer;      // 加密中间数据存储地
  AeadCtx: TAEADContext;
  GHashCtx: TGHash128Context;
  Tag: TGCM128Tag;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Iv = nil then
    IvByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  AEADEncryptInit(AeadCtx, Key, KeyByteLength, EncryptType);

  // 计算 Enc(Key, 128 个 0)，得到 H
  FillChar(H[0], SizeOf(H), 0);
  AEADEncryptBlock(AeadCtx, T128BitsBuffer(H), T128BitsBuffer(H), EncryptType);

  // 初始化计数器，以后 Cnt 自增并塞进 Y 的后 32 位中
  if IvByteLength = GCM_NONCE_LENGTH then
  begin
    Move(Iv^, Y[0], GCM_NONCE_LENGTH);
    Cnt := 1;
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));
  end
  else
  begin
    GHash128(H, Iv, IvByteLength, nil, 0, TGHash128Tag(Y));
    Move(Y[GCM_NONCE_LENGTH], Cnt, SizeOf(Cardinal));
    ReverseMemory(@Cnt, SizeOf(Cardinal));
  end;

  // 先把最开始的 Y 值的加密结果计算出来
  AEADEncryptBlock(AeadCtx, T128BitsBuffer(Y), T128BitsBuffer(Y0), EncryptType);

  // 初始化 GHash
  GHash128Start(GHashCtx, H, AAD, AADByteLength);

  // 开始循环加密整块
  while EnByteLength >= AEAD_BLOCK do
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 密文先进行 GHash
    GHash128Update(GHashCtx, EnData, SizeOf(T128BitsBuffer));

    // 对 Y 加密 C 暂时得到本块的加密结果
    AEADEncryptBlock(AeadCtx, T128BitsBuffer(Y), C, EncryptType);

    // 和密文异或，C 得到异或后的结果，完整块
    MemoryXor(EnData, @C[0], SizeOf(T128BitsBuffer), @C[0]);

    // 存起明文
    Move(C[0], PlainData^, SizeOf(T128BitsBuffer));

    // 准备下一步
    EnData := Pointer(TCnNativeInt(EnData) + AEAD_BLOCK);
    PlainData := Pointer(TCnNativeInt(PlainData) + AEAD_BLOCK);
    Dec(EnByteLength, AEAD_BLOCK);
  end;

  if EnByteLength > 0 then
  begin
    // 递增计数器并更新 Y
    Inc(Cnt);
    M := Int32HostToNetwork(Cnt);
    Move(M, Y[GCM_NONCE_LENGTH], SizeOf(M));

    // 密文先进行 GHash
    GHash128Update(GHashCtx, EnData, EnByteLength);

    // 对 Y 加密 C 暂时得到本块的加密结果
    AEADEncryptBlock(AeadCtx, T128BitsBuffer(Y), C, EncryptType);

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

function GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
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

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(PlainData) > 0 then
  begin
    SetLength(Result, Length(PlainData));
    GCMEncrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), @Result[0], OutTag, EncryptType);
  end
  else
  begin
    GCMEncrypt(K, Length(Key), I, Length(Iv), P, Length(PlainData), A,
      Length(AAD), nil, OutTag, EncryptType);
  end;
end;

function GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
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

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(EnData) > 0 then
  begin
    SetLength(Result, Length(EnData));
    if not GCMDecrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), @Result[0], InTag, EncryptType) then // Tag 比对失败则返回
      SetLength(Result, 0);
  end
  else
  begin
    GCMDecrypt(K, Length(Key), I, Length(Iv), P, Length(EnData), A,
      Length(AAD), nil, InTag, EncryptType); // 没密文，其实 Tag 比对成功与否都没用
  end;
end;

function AES128GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetAES128);
end;

function AES192GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetAES192);
end;

function AES256GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetAES256);
end;

function SM4GCMEncryptBytes(Key, Iv, PlainData, AAD: TBytes; var OutTag: TGCM128Tag): TBytes;
begin
  Result := GCMEncryptBytes(Key, Iv, PlainData, AAD, OutTag, aetSM4);
end;

procedure AES128GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES128);
end;

procedure AES192GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES192);
end;

procedure AES256GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES256);
end;

procedure SM4GCMEncrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TGCM128Tag);
begin
  GCMEncrypt(Key, KeyByteLength, Iv, IvByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetSM4);
end;

function AES128GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetAES128);
end;

function AES192GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetAES192);
end;

function AES256GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetAES256);
end;

function SM4GCMDecryptBytes(Key, Iv, EnData, AAD: TBytes; var InTag: TGCM128Tag): TBytes;
begin
  Result := GCMDecryptBytes(Key, Iv, EnData, AAD, InTag, aetSM4);
end;

function AES128GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES128);
end;

function AES192GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES192);
end;

function AES256GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES256);
end;

function SM4GCMDecrypt(Key: Pointer; KeyByteLength: Integer; Iv: Pointer; IvByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TGCM128Tag): Boolean;
begin
  Result := GCMDecrypt(Key, KeyByteLength, Iv, IvByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetSM4);
end;

procedure CMAC128(var Key: TCMAC128Key; Data: Pointer; DataByteLength: Integer;
  EncryptType: TAEADEncryptType; var OutTag: TCMAC128Tag);
var
  K1, K2: TCMAC128Key;
  L, X, Y: T128BitsBuffer;
  AeadCtx: TAEADContext;
  LastFull: Boolean;
begin
  AEADEncryptInit(AeadCtx, @Key[0], Length(Key), EncryptType);

  // 计算 Enc(Key, 128 个 0)，得到 L
  FillChar(L[0], SizeOf(L), 0);
  AEADEncryptBlock(AeadCtx, L, L, EncryptType);

  // 根据 L 计算俩子密钥
  MemoryShiftLeft(@L[0], @K1[0], SizeOf(TCMAC128Key), 1);
  if AeadIsBitSet(@L[0], 0) then
    MemoryXor(@K1[0], @CMAC_POLY[0], SizeOf(TCMAC128Key), @K1[0]);

  MemoryShiftLeft(@K1[0], @K2[0], SizeOf(TCMAC128Key), 1);
  if AeadIsBitSet(@K1[0], 0) then
    MemoryXor(@K2[0], @CMAC_POLY[0], SizeOf(TCMAC128Key), @K2[0]);

  // 开始分块计算，末块要额外处理
  LastFull := (DataByteLength mod AEAD_BLOCK) = 0;

  // 算整块 A
  FillChar(X[0], SizeOf(T128BitsBuffer), 0);
  while DataByteLength >= AEAD_BLOCK do
  begin
    Move(Data^, L[0], AEAD_BLOCK); // 复用 L 作为每块原始数据
    if LastFull and (DataByteLength = AEAD_BLOCK) then // 最后一个整块
    begin
      MemoryXor(@K1[0], @L[0], AEAD_BLOCK, @L[0]);
      MemoryXor(@X[0], @L[0], AEAD_BLOCK, @Y[0]);
      AEADEncryptBlock(AeadCtx, Y, T128BitsBuffer(OutTag), EncryptType); // 算出最终 Tag
      Exit;
    end;

    MemoryXor(@L[0], @X[0], SizeOf(T128BitsBuffer), @Y[0]);
    AEADEncryptBlock(AeadCtx, Y, X, EncryptType); // 一轮计算结果再次放入 X

    Data := Pointer(TCnNativeInt(Data) + AEAD_BLOCK);
    Dec(DataByteLength, AEAD_BLOCK);
  end;

  FillChar(L[0], SizeOf(T128BitsBuffer), 0);
  if DataByteLength > 0 then
  Move(Data^, L[0], DataByteLength);
  L[DataByteLength] := $80;         // 最后一块非整块，加上 Padding

  MemoryXor(@K2[0], @L[0], AEAD_BLOCK, @L[0]);
  MemoryXor(@X[0], @L[0], AEAD_BLOCK, @Y[0]);
  AEADEncryptBlock(AeadCtx, Y, T128BitsBuffer(OutTag), EncryptType); // 算出最终 Tag
end;

function CMAC128Bytes(Key, Data: TBytes; EncryptType: TAEADEncryptType): TCMAC128Tag;
var
  D: Pointer;
  Key128: TCMAC128Key;
begin
  if Data = nil then
    D := nil
  else
    D := @Data[0];

  MoveMost128(Key[0], Key128[0], Length(Key));
  CMAC128(Key128, D, Length(Data), EncryptType, Result);
end;

function AES128CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetAES128);
end;

function AES192CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetAES192);
end;

function AES256CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetAES256);
end;

function SM4CMAC128Bytes(Key, Data: TBytes): TCMAC128Tag;
begin
  Result := CMAC128Bytes(Key, Data, aetSM4);
end;

function AES128CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
var
  Key128: TCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetAES128, Result);
end;

function AES192CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
var
  Key128: TCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetAES192, Result);
end;

function AES256CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
var
  Key128: TCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetAES256, Result);
end;

function SM4CMAC128(Key: Pointer; KeyByteLength: Integer; Data: Pointer;
  DataByteLength: Integer): TCMAC128Tag;
var
  Key128: TCMAC128Key;
begin
  MoveMost128(Key^, Key128[0], KeyByteLength);
  CMAC128(Key128, Data, DataByteLength, aetSM4, Result);
end;

procedure CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer;
  NonceByteLength: Integer; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer; EnData: Pointer; var OutTag: TCCM128Tag;
  EncryptType: TAEADEncryptType);
var
  CMacCtx: TAEADContext;
  CtrCtx: TAEADContext;
  B0: T128BitsBuffer;   // CMAC 认证时的 Iv
  CX: T128BitsBuffer;   // CMAC 的计算结果存放的中间块
  A0: T128BitsBuffer;   // 附加数据块的第一块，以及后面做 CMAC 过程中的原始数据的中间块
  S0: T128BitsBuffer;   // 第一个加密块用于验证，不参与明文异或
  SX: T128BitsBuffer;   // CTR 的计算结果存放的中间块
  Ctr: T128BitsBuffer;  // CTR 的计数块
  Cnt, T: Int64;
  P: PCnByte;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Nonce = nil then
    NonceByteLength := 0;
  if Data = nil then
    DataByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  FillChar(B0[0], SizeOf(T128BitsBuffer), 0);
  FillChar(Ctr[0], SizeOf(T128BitsBuffer), 0);

//   +----+-+-+-+-+-+-+-+-+    |L'(L) 决定
//   | 位 |7|6|5|4|3|2|1|0|    |Nonce 的长度
//   +----+-+-+-+-+-+-+-+-+
//   |    |0|A|  M' |  L' |
//   +----+-+-+-+-+-+-+-+-+

  B0[0] := 4 * (CCM_M_LEN - 2) + CCM_L_LEN - 1;
  if (AAD <> nil) and (AADByteLength > 0) then
    B0[0] := B0[0] + 64;   // B0 块的第一个字节准备好，A 位是 1 表示有 AAD

  Ctr[0] := CCM_L_LEN - 1;

  // 填充 15 - L 个 Nonce
  MoveMost(Nonce^, B0[1], NonceByteLength, CCM_NONCE);
  MoveMost(Nonce^, Ctr[1], NonceByteLength, CCM_NONCE);

  // 放上网络字节顺序的明文长度，且从高位截断至 CCM_L_LEN 字节，这样才造好了 B0
  P := PCnByte(@T);
  Inc(P, SizeOf(Int64) - CCM_L_LEN);  // 这两句建立 P 和 T 的高几位的地址关系，后面持续使用

  T := Int64HostToNetwork(DataByteLength);
  Move(P^, B0[CCM_NONCE + 1], CCM_L_LEN);

  // 初始化 CMAC/CTR 的 Key 等，准备做 CMAC/CTR
  AEADEncryptInit(CMacCtx, Key, KeyByteLength, EncryptType);
  AEADEncryptInit(CtrCtx, Key, KeyByteLength, EncryptType);

  // Ctr 的后八个字节是计数器，现初始化为 0，并且计算 S0 作为验证字段之一
  Cnt := 0;
  AEADEncryptBlock(CtrCtx, Ctr, S0, EncryptType);

  // CMAC 先算 B0，中间结果放 CX，也就是 RFC 中的 CBC Iv Out
  AEADEncryptBlock(CMacCtx, B0, CX, EncryptType);

  // 有 AAD 的话接着造 A0
  if (B0[0] and $40 <> 0) then
  begin
    FillChar(A0[0], AEAD_BLOCK, 0);

    if AADByteLength < $1000 - $100 then
    begin
      PCnWord(@A0[0])^ := Int16HostToNetwork(SmallInt(AADByteLength)); // 共俩字节

      // 第一块准备好，可能有塞不满、满以及超，三种情况
      MoveMost(AAD^, A0[2], AADByteLength, AEAD_BLOCK - 2);

      // 这一块和 CX 异或，再 CMAC 之，结果放回 CX
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // 递增准备处理后面的
      AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK - 2);
      Dec(AADByteLength, AEAD_BLOCK - 2);
    end
    else
    begin
      // A0[0] 前俩字节准备好
      A0[0] := $FF;
      A0[1] := $FE;
      PCnLongWord32(@A0[2])^ := Int32HostToNetwork(AADByteLength); // 共六字节

      // 第一块准备好，可能有塞不满、满以及超，三种情况
      MoveMost(AAD^, A0[6], AADByteLength, AEAD_BLOCK - 6);

      // 这一块和 CX 异或，再 CMAC 之，结果放回 CX
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // 递增准备处理后面的
      AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK - 6);
      Dec(AADByteLength, AEAD_BLOCK - 6);
    end;

    // 不满或刚满的话，AADByteLength 此时小于等于 0，不继续
    while AADByteLength >= AEAD_BLOCK do
    begin
      Move(AAD^, A0[0], AEAD_BLOCK);

      // 后续块（也可能是最后一块）和 CX 异或，再 CMAC 之，结果放回 CX
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // 递增准备处理后面的
      AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK);
      Dec(AADByteLength, AEAD_BLOCK);
    end;

    if AADByteLength > 0 then // 还有剩余时才再多一块
    begin
      FillChar(A0[0], AEAD_BLOCK, 0);
      Move(AAD^, A0[0], AADByteLength);

      // CMAC 最后一块
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);
    end;
  end;

  // 算完了 AAD 的 CMAC 值，开始算 Data 的，继续用 A0
  // 并且开始加密块
  while DataByteLength >= AEAD_BLOCK do
  begin
    Move(Data^, A0[0], AEAD_BLOCK); // 明文放 A0

    // 计数器加一并生成加密块
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CCM_NONCE + 1], CCM_L_LEN);

    // 得到本块的加密结果，放 SX 中
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // 并与明文异或得到密文
    MemoryXor(@SX[0], @A0[0], AEAD_BLOCK, EnData);

    // 后续块（也可能是最后一块）和 CX 异或，再 CMAC 之，结果放回 CX
    MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

    // 递增准备处理后面的
    Data := Pointer(TCnNativeInt(Data) + AEAD_BLOCK);
    EnData := Pointer(TCnNativeInt(EnData) + AEAD_BLOCK);
    Dec(DataByteLength, AEAD_BLOCK);
  end;

  if DataByteLength > 0 then // 还有剩余时才再多一块
  begin
    FillChar(A0[0], AEAD_BLOCK, 0);
    Move(Data^, A0[0], DataByteLength);

    // 计数器加一并生成加密块
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CCM_NONCE + 1], CCM_L_LEN);

    // 得到本块的加密结果，放 SX 中
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // 并与最后一块明文异或得到密文
    MemoryXor(@SX[0], @A0[0], DataByteLength, EnData);

    // CMAC 最后一块
    MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);
  end;

  // 取出最后 CMAC 的结果与 CTR 0 的加密结果异或
  MemoryXor(@CX[0], @S0[0], AEAD_BLOCK, @CX[0]);

  // 移动至 OutTag 中返回
  FillChar(OutTag[0], SizeOf(TCCM128Tag), 0);
  Move(CX[0], OutTag[0], CCM_M_LEN);
end;

function CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
var
  K, N, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Nonce = nil then
    N := nil
  else
    N := @Nonce[0];

  if PlainData = nil then
    P := nil
  else
    P := @PlainData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(PlainData) > 0 then
  begin
    SetLength(Result, Length(PlainData));
    CCMEncrypt(K, Length(Key), N, Length(Nonce), P, Length(PlainData), A,
      Length(AAD), @Result[0], OutTag, EncryptType);
    if Length(Result) > 0 then
      Exit;
  end
  else
  begin
    CCMEncrypt(K, Length(Key), N, Length(Nonce), P, Length(PlainData), A,
      Length(AAD), nil, OutTag, EncryptType);
  end;
end;

function AES128CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetAES128);
end;

function AES192CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetAES192);
end;

function AES256CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetAES256);
end;

function SM4CCMEncryptBytes(Key, Nonce, PlainData, AAD: TBytes; var OutTag: TCCM128Tag): TBytes;
begin
  Result := CCMEncryptBytes(Key, Nonce, PlainData, AAD, OutTag, aetSM4);
end;

procedure AES128CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES128);
end;

procedure AES192CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES192);
end;

procedure AES256CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetAES256);
end;

procedure SM4CCMEncrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  PlainData: Pointer; PlainByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutEnData: Pointer; var OutTag: TCCM128Tag);
begin
  CCMEncrypt(Key, KeyByteLength, Nonce, NonceByteLength, PlainData, PlainByteLength,
    AAD, AADByteLength, OutEnData, OutTag, aetSM4);
end;

function CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer;
  AADByteLength: Integer; PlainData: Pointer; var InTag: TCCM128Tag;
  EncryptType: TAEADEncryptType): Boolean;
var
  CMacCtx: TAEADContext;
  CtrCtx: TAEADContext;
  B0: T128BitsBuffer;   // CMAC 认证时的 Iv
  CX: T128BitsBuffer;   // CMAC 的计算结果存放的中间块
  A0: T128BitsBuffer;   // 附加数据块的第一块，以及后面做 CMAC 过程中的原始数据的中间块
  S0: T128BitsBuffer;   // 第一个加密块用于验证，不参与明文异或
  SX: T128BitsBuffer;   // CTR 的计算结果存放的中间块
  Ctr: T128BitsBuffer;  // CTR 的计数块
  Cnt, T: Int64;
  P: PCnByte;
  Tag: TCCM128Tag;
begin
  if Key = nil then
    KeyByteLength := 0;
  if Nonce = nil then
    NonceByteLength := 0;
  if EnData = nil then
    EnByteLength := 0;
  if AAD = nil then
    AADByteLength := 0;

  FillChar(B0[0], SizeOf(T128BitsBuffer), 0);
  FillChar(Ctr[0], SizeOf(T128BitsBuffer), 0);

//   +----+-+-+-+-+-+-+-+-+    |L'(L) 决定
//   | 位 |7|6|5|4|3|2|1|0|    |Nonce 的长度
//   +----+-+-+-+-+-+-+-+-+
//   |    |0|A|  M' |  L' |
//   +----+-+-+-+-+-+-+-+-+

  B0[0] := 4 * (CCM_M_LEN - 2) + CCM_L_LEN - 1;
  if (AAD <> nil) and (AADByteLength > 0) then
    B0[0] := B0[0] + 64;   // B0 块的第一个字节准备好，A 位是 1 表示有 AAD

  Ctr[0] := CCM_L_LEN - 1;

  // 填充 15 - L 个 Nonce
  MoveMost(Nonce^, B0[1], NonceByteLength, CCM_NONCE);
  MoveMost(Nonce^, Ctr[1], NonceByteLength, CCM_NONCE);

  // 放上网络字节顺序的明文长度，且从高位截断至 CCM_L_LEN 字节，这样才造好了 B0
  P := PCnByte(@T);
  Inc(P, SizeOf(Int64) - CCM_L_LEN);  // 这两句建立 P 和 T 的高几位的地址关系，后面持续使用

  T := Int64HostToNetwork(EnByteLength);
  Move(P^, B0[CCM_NONCE + 1], CCM_L_LEN);

  // 初始化 CMAC/CTR 的 Key 等，准备做 CMAC/CTR
  AEADEncryptInit(CMacCtx, Key, KeyByteLength, EncryptType);
  AEADEncryptInit(CtrCtx, Key, KeyByteLength, EncryptType);

  // Ctr 的后八个字节是计数器，现初始化为 0，并且计算 S0 作为验证字段之一
  Cnt := 0;
  AEADEncryptBlock(CtrCtx, Ctr, S0, EncryptType);

  // CMAC 先算 B0，中间结果放 CX，也就是 RFC 中的 CBC Iv Out
  AEADEncryptBlock(CMacCtx, B0, CX, EncryptType);

  // 有 AAD 的话接着造 A0
  if (B0[0] and $40 <> 0) then
  begin
    FillChar(A0[0], AEAD_BLOCK, 0);

    if AADByteLength < $1000 - $100 then
    begin
      PCnWord(@A0[0])^ := Int16HostToNetwork(SmallInt(AADByteLength)); // 共俩字节

      // 第一块准备好，可能有塞不满、满以及超，三种情况
      MoveMost(AAD^, A0[2], AADByteLength, AEAD_BLOCK - 2);

      // 这一块和 CX 异或，再 CMAC 之，结果放回 CX
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // 递增准备处理后面的
      AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK - 2);
      Dec(AADByteLength, AEAD_BLOCK - 2);
    end
    else
    begin
      // A0[0] 前俩字节准备好
      A0[0] := $FF;
      A0[1] := $FE;
      PCnLongWord32(@A0[2])^ := Int32HostToNetwork(AADByteLength); // 共六字节

      // 第一块准备好，可能有塞不满、满以及超，三种情况
      MoveMost(AAD^, A0[6], AADByteLength, AEAD_BLOCK - 6);

      // 这一块和 CX 异或，再 CMAC 之，结果放回 CX
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // 递增准备处理后面的
      AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK - 6);
      Dec(AADByteLength, AEAD_BLOCK - 6);
    end;

    // 不满或刚满的话，AADByteLength 此时小于等于 0，不继续
    while AADByteLength >= AEAD_BLOCK do
    begin
      Move(AAD^, A0[0], AEAD_BLOCK);

      // 后续块（也可能是最后一块）和 CX 异或，再 CMAC 之，结果放回 CX
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

      // 递增准备处理后面的
      AAD := Pointer(TCnNativeInt(AAD) + AEAD_BLOCK);
      Dec(AADByteLength, AEAD_BLOCK);
    end;

    if AADByteLength > 0 then // 还有剩余时才再多一块
    begin
      FillChar(A0[0], AEAD_BLOCK, 0);
      Move(AAD^, A0[0], AADByteLength);

      // CMAC 最后一块
      MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
      AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);
    end;
  end;

  // 算完了 AAD 的 CMAC 值，开始算 Data 的，继续用 A0
  // 并且开始加密块并异或解密
  while EnByteLength >= AEAD_BLOCK do
  begin
    Move(EnData^, A0[0], AEAD_BLOCK); // 密文放 A0

    // 计数器加一并生成加密块
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CCM_NONCE + 1], CCM_L_LEN);

    // 得到本块的加密结果，放 SX 中
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // 并与密文异或得到明文
    MemoryXor(@SX[0], @A0[0], AEAD_BLOCK, PlainData);

    // 后续块（也可能是最后一块）明文和 CX 异或，再 CMAC 之，结果放回 CX
    MemoryXor(PlainData, @CX[0], AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

    // 递增准备处理后面的
    EnData := Pointer(TCnNativeInt(EnData) + AEAD_BLOCK);
    PlainData := Pointer(TCnNativeInt(PlainData) + AEAD_BLOCK);
    Dec(EnByteLength, AEAD_BLOCK);
  end;

  if EnByteLength > 0 then // 还有剩余时才再多一块
  begin
    FillChar(A0[0], AEAD_BLOCK, 0);
    Move(EnData^, A0[0], EnByteLength);

    // 计数器加一并生成加密块
    Inc(Cnt);
    T := Int64HostToNetwork(Cnt);
    Move(P^, Ctr[CCM_NONCE + 1], CCM_L_LEN);

    // 得到本块的加密结果，放 SX 中
    AEADEncryptBlock(CtrCtx, Ctr, SX, EncryptType);
    // 并与最后一块密文异或得到明文先放 A0 里供整块计算
    MemoryXor(@SX[0], @A0[0], EnByteLength, @A0[0]);

    // CMAC 最后一块
    MemoryXor(@A0[0], @CX[0], AEAD_BLOCK, @CX[0]);
    AEADEncryptBlock(CMacCtx, CX, CX, EncryptType);

    // 存下最后一块明文
    Move(A0[0], PlainData^, EnByteLength);
  end;

  // 取出最后 CMAC 的结果与 CTR 0 的加密结果异或
  MemoryXor(@CX[0], @S0[0], AEAD_BLOCK, @CX[0]);

  // CMAC 结果移动至 Tag 中
  FillChar(Tag[0], SizeOf(TCCM128Tag), 0);
  Move(CX[0], Tag[0], CCM_M_LEN);

  // 比对 Tag 是否相同
  Result := CompareMem(@Tag[0], @InTag[0], CCM_M_LEN);
end;

function CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag;
  EncryptType: TAEADEncryptType): TBytes;
var
  K, N, P, A: Pointer;
begin
  if Key = nil then
    K := nil
  else
    K := @Key[0];

  if Nonce = nil then
    N := nil
  else
    N := @Nonce[0];

  if EnData = nil then
    P := nil
  else
    P := @EnData[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  if Length(EnData) > 0 then
  begin
    SetLength(Result, Length(EnData));
    if not CCMDecrypt(K, Length(Key), N, Length(Nonce), P, Length(EnData), A,
      Length(AAD), @Result[0], InTag, EncryptType) then // Tag 比对失败则返回
      SetLength(Result, 0);
  end
  else
  begin
    CCMDecrypt(K, Length(Key), N, Length(Nonce), P, Length(EnData), A,
      Length(AAD), nil, InTag, EncryptType); // 没密文，其实 Tag 比对成功与否都没用
  end;
end;

function AES128CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetAES128);
end;

function AES192CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetAES192);
end;

function AES256CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetAES256);
end;

function SM4CCMDecryptBytes(Key, Nonce, EnData, AAD: TBytes; var InTag: TCCM128Tag): TBytes;
begin
  Result := CCMDecryptBytes(Key, Nonce, EnData, AAD, InTag, aetSM4);
end;

function AES128CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES128);
end;

function AES192CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES192);
end;

function AES256CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetAES256);
end;

function SM4CCMDecrypt(Key: Pointer; KeyByteLength: Integer; Nonce: Pointer; NonceByteLength: Integer;
  EnData: Pointer; EnByteLength: Integer; AAD: Pointer; AADByteLength: Integer;
  OutPlainData: Pointer; var InTag: TCCM128Tag): Boolean;
begin
  Result := CCMDecrypt(Key, KeyByteLength, Nonce, NonceByteLength, EnData, EnByteLength,
    AAD, AADByteLength, OutPlainData, InTag, aetSM4);
end;

end.
