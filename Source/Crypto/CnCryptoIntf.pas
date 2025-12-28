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

unit CnCryptoIntf;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：密码算法库 DLL/SO/DYLIB 对外输出声明单元
* 单元作者：CnPack 开发组
* 备    注：虽然 CnPack 密码算法库可以在 Pascal 编译器中直接编译源码，但对于其他
*           语言来说使用起来就不方便了。我们将其封装成对外输出的 DLL/SO 函数，
*           可编译成 DLL/SO/DYLIB 后运行期被其他进程动态加载，供其他语言运行时调用。
*
*           本单元是生成的 CnCrypto.dll/so/dylib 的调用声明，外部 Pascal 工程
*           uses 此单元即可。
* 开发平台：PWin7 + Delphi 7
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.12.26 V1.0
*                创建单元，实现声明
================================================================================
|</PRE>}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

interface

{$IFDEF VER130}
uses
  Windows;
{$ENDIF}

type
  TUInt8          = Byte;

  TUInt32         = Cardinal;

  // Delphi 5 6 7 不支持或者说不健康支持 UInt64
{$IFDEF VER130}
  TUInt64         = Int64;
{$ELSE}
  {$IFDEF VER140}
  TUInt64         = Int64;
  {$ELSE}
    {$IFDEF VER150}
  TUInt64         = Int64;
    {$ELSE}
  TUInt64         = UInt64;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

  TInt32          = Integer;

  TInt64          = Int64;

  TInt16          = SmallInt;

  TInt8           = ShortInt;

  TBool32         = Integer;

  TCnSize         = Integer;

  TCnCryptoHandle = Pointer;

  TCnResult       = TInt32;

{$IFDEF VER130}
  PByte           = Windows.PByte;
{$ENDIF}

const
  CN_OK                 = 0;
  CN_E_INVALID_ARG      = -1;
  CN_E_BUFFER_TOO_SMALL = -2;
  CN_E_UNSUPPORTED      = -3;
  CN_E_NO_MEMORY        = -4;
  CN_E_STATE            = -5;
  CN_E_VERIFY_FAIL      = -6;
  CN_E_INTERNAL         = -100;

const
  CN_HASH_MD5                = 1;
  CN_HASH_SHA1               = 2;
  CN_HASH_SHA2_256           = 4;
  CN_HASH_SHA2_512           = 6;
  CN_HASH_SHA3_224           = 8;
  CN_HASH_SHA3_256           = 9;
  CN_HASH_SHA3_384           = 10;
  CN_HASH_SHA3_512           = 11;
  CN_HASH_SM3                = 12;
  CN_HASH_BLAKE224           = 20;
  CN_HASH_BLAKE256           = 21;
  CN_HASH_BLAKE384           = 22;
  CN_HASH_BLAKE512           = 23;
  CN_HASH_BLAKE2S            = 24;
  CN_HASH_BLAKE2B            = 25;
  CN_HASH_XXH32              = 30;
  CN_HASH_XXH64              = 31;
  CN_CIPHER_AES128_CBC       = 1001;
  CN_CIPHER_AES192_CBC       = 1002;
  CN_CIPHER_AES256_CBC       = 1003;
  CN_CIPHER_AES128_ECB       = 1004;
  CN_CIPHER_AES192_ECB       = 1005;
  CN_CIPHER_AES256_ECB       = 1006;
  CN_CIPHER_AES128_CTR       = 1101;
  CN_CIPHER_AES192_CTR       = 1102;
  CN_CIPHER_AES256_CTR       = 1103;
  CN_CIPHER_AES128_OFB       = 1201;
  CN_CIPHER_AES192_OFB       = 1202;
  CN_CIPHER_AES256_OFB       = 1203;
  CN_CIPHER_AES128_CFB       = 1301;
  CN_CIPHER_AES192_CFB       = 1302;
  CN_CIPHER_AES256_CFB       = 1303;
  CN_CIPHER_DES_ECB          = 1401;
  CN_CIPHER_DES_CBC          = 1402;
  CN_CIPHER_3DES_ECB         = 1403;
  CN_CIPHER_3DES_CBC         = 1404;
  CN_CIPHER_SM4_ECB          = 1501;
  CN_CIPHER_SM4_CBC          = 1502;
  CN_CIPHER_SM4_CFB          = 1503;
  CN_CIPHER_SM4_OFB          = 1504;
  CN_CIPHER_SM4_CTR          = 1505;
  CN_CIPHER_RC4              = 1601;
  CN_CIPHER_ZUC              = 1801;
  CN_CIPHER_CHACHA20         = 1901;
  CN_AEAD_AES128_GCM         = 2001;
  CN_AEAD_AES192_GCM         = 2002;
  CN_AEAD_AES256_GCM         = 2003;
  CN_AEAD_SM4_GCM            = 2004;
  CN_AEAD_TAG_BYTES          = 16;
  CN_AEAD_CHACHA20_POLY1305  = 2101;
  CN_AEAD_XCHACHA20_POLY1305 = 2102;
  CN_RSA_PAD_PKCS1           = 1;
  CN_RSA_PAD_OAEP            = 2;
  CN_RSA_KEY_PKCS1           = 1;
  CN_RSA_KEY_PKCS8           = 2;
  CN_ECC_CURVE_SM2           = 3001;
  CN_ECC_CURVE_SECP256K1     = 3002;
  CN_ECC_CURVE_SECP256R1     = 3003;
  CN_ECC_CURVE_PRIME256V1    = 3004;
  CN_ECC_CURVE_SECP384R1     = 3005;
  CN_ECC_CURVE_SECP521R1     = 3006;
  CN_ECC_KEY_PKCS1           = 1;
  CN_ECC_KEY_PKCS8           = 2;
  CN_SM2_SEQ_C1C3C2          = 1;
  CN_SM2_SEQ_C1C2C3          = 2;
  CN_HASH_SHAKE128           = 40;
  CN_HASH_SHAKE256           = 41;
  CN_MLKEM_TYPE_512          = 5101;
  CN_MLKEM_TYPE_768          = 5102;
  CN_MLKEM_TYPE_1024         = 5103;
  CN_MLDSA_TYPE_44           = 5201;
  CN_MLDSA_TYPE_65           = 5202;
  CN_MLDSA_TYPE_87           = 5203;


function cn_get_version(var out_major, out_minor, out_patch: TUInt32): TCnResult; cdecl;
{* 获取库版本号（主/次/修订）。

   参数：
     out_major: TUInt32                    - 输出主版本号
     out_minor: TUInt32                    - 输出次版本号
     out_patch: TUInt32                    - 输出修订版本号

   返回值：TCnResult                       - 错误码，CN_OK 表示成功
}

function cn_get_abi_version: TUInt32; cdecl;
{* 获取库的 ABI 版本，用于二进制兼容性判断。

   参数：
     （无）

   返回值：TUInt32                         - ABI 版本号
}

function cn_lib_init: TCnResult; cdecl;
{* 初始化加密库，全局准备工作。

   参数：
     （无）

   返回值：TCnResult                       - 错误码，CN_OK 表示成功
}

function cn_lib_finalize: TCnResult; cdecl;
{* 释放加密库，清理全局资源。

   参数：
     （无）

   返回值：TCnResult                       - 错误码，CN_OK 表示成功
}

function cn_alloc(size: TCnSize): TCnCryptoHandle; cdecl;
{* 分配指定大小的安全内存。

   参数：
     size: TCnSize                          - 需要分配的字节数

   返回值：TCnCryptoHandle                  - 返回内存对象标识，失败为 nil
}

function cn_free(ptr: TCnCryptoHandle): TCnResult; cdecl;
{* 释放由库分配的内存。

   参数：
     ptr: TCnCryptoHandle                   - 由 cn_alloc 返回的对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_memzero(ptr: TCnCryptoHandle; size: TCnSize): TCnResult; cdecl;
{* 将指定内存区域安全清零。

   参数：
     ptr: TCnCryptoHandle                   - 目标内存对象标识
     size: TCnSize                          - 清零的字节数

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_endian_is_le: TBool32; cdecl;
{* 判断当前平台是否小端序。

   参数：
     （无）

   返回值：TBool32                          - 1 表示是，0 表示否
}

function cn_endian_is_be: TBool32; cdecl;
{* 判断当前平台是否大端序。

   参数：
     （无）

   返回值：TBool32                          - 1 表示是，0 表示否
}

function cn_data_to_hex(in_ptr: Pointer; in_len: TCnSize; out_hex: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 将二进制数据编码为十六进制字符串（小写，无分隔）。

   参数：
     in_ptr: Pointer                        - 输入数据指针
     in_len: TCnSize                        - 输入数据字节长度
     out_hex: PByte                         - 输出缓冲区。如 DLL 是 Unicode 编译，则此处输出双字节字符串内容，否则为单字节字符串内容
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，缓冲区不足返回 CN_E_BUFFER_TOO_SMALL
}

function cn_const_time_equal(a: Pointer; b: Pointer; len: TCnSize): TBool32; cdecl;
{* 常数时间比较两个缓冲区是否相等，避免时序攻击。

   参数：
     a: Pointer                             - 缓冲区 A
     b: Pointer                             - 缓冲区 B
     len: TCnSize                           - 比较字节长度

   返回值：TBool32                          - 1 表示相等，0 表示不等
}

function cn_const_time_select(flag: TBool32; a: Pointer; b: Pointer; len:
  TCnSize; out_ptr: Pointer): TCnResult; cdecl;
{* 常数时间从两个缓冲区中选择其一复制到输出，避免时序泄露。

   参数：
     flag: TBool32                          - 选择标志，非零选 a，零选 b
     a: Pointer                             - 源缓冲区 A
     b: Pointer                             - 源缓冲区 B
     len: TCnSize                           - 复制字节长度
     out_ptr: Pointer                       - 输出缓冲区

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_str_to_uint64(ascii_ptr: PByte; len: TCnSize; var out_value: TUInt64):
  TCnResult; cdecl;
{* 将 ASCII 数字字符串转换为 64 位无符号整数。

   参数：
     ascii_ptr: PByte                       - 输入 ASCII 字符串指针
     len: TCnSize                           - 字符串字节长度
     out_value: TUInt64                     - 输出转换后的数值

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，非法输入返回 CN_E_INVALID_ARG
}

function cn_base64_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 将数据进行 Base64 编码。

   参数：
     in_ptr: PByte                          - 输入数据指针
     in_len: TCnSize                        - 输入数据字节长度
     out_ptr: PByte                         - 输出缓冲区。如 DLL 是 Unicode 编译，则此处输出双字节字符串内容，否则为单字节字符串内容
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_base64_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 解码 Base64 字符串为二进制数据。

   参数：
     in_ptr: PByte                          - 输入 Base64 数据指针。如 DLL 是 Unicode 编译，则此处需要双字节字符串内容，否则为单字节字符串内容
     in_len: TCnSize                        - 输入字节长度
     out_ptr: PByte                         - 输出二进制缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，不合法输入返回 CN_E_INVALID_ARG
}

function cn_base64url_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 将数据进行 Base64URL 编码（URL 安全字符集）。

   参数：
     in_ptr: PByte                          - 输入数据指针
     in_len: TCnSize                        - 输入数据字节长度
     out_ptr: PByte                         - 输出缓冲区。如 DLL 是 Unicode 编译，则此处输出双字节字符串内容，否则为单字节字符串内容
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_base64url_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 解码 Base64URL 字符串为二进制数据。

   参数：
     in_ptr: PByte                          - 输入 Base64URL 数据指针。如 DLL 是 Unicode 编译，则此处需要双字节字符串内容，否则为单字节字符串内容
     in_len: TCnSize                        - 输入字节长度
     out_ptr: PByte                         - 输出二进制缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，不合法输入返回 CN_E_INVALID_ARG
}

function cn_otp_hotp(seed: PByte; seed_len: TCnSize; counter: TUInt64; digits:
  TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 生成 HOTP 一次性密码。

   参数：
     seed: PByte                            - 种子密钥
     seed_len: TCnSize                      - 种子密钥字节长度
     counter: TUInt64                       - 计数器值
     digits: TInt32                         - 密码位数（通常 6 或 8）
     out_code_ascii: PByte                  - 输出缓冲区（ASCII）
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_otp_totp(hash_id: TInt32; seed: PByte; seed_len: TCnSize; period_sec:
  TInt32; digits: TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
{* 生成 TOTP 基于时间的一次性密码。

   参数：
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*，如 SHA1/SM3 等）
     seed: PByte                            - 种子密钥
     seed_len: TCnSize                      - 种子密钥字节长度
     period_sec: TInt32                     - 时间步长（秒）
     digits: TInt32                         - 密码位数（通常 6 或 8）
     out_code_ascii: PByte                  - 输出内容缓冲区（ASCII）
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_hash_digest(alg_id: TInt32; data: PByte; len: TCnSize; out_digest:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 计算指定算法的消息杂凑。

   参数：
     alg_id: TInt32                         - 杂凑算法 ID（CN_HASH_*）
     data: PByte                            - 输入数据指针
     len: TCnSize                           - 输入数据字节长度
     out_digest: PByte                      - 输出内容缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_hmac(alg_id: TInt32; key: PByte; key_len: TCnSize; data: PByte; len:
  TCnSize; out_mac: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 计算 HMAC（带密钥的消息认证码）。

   参数：
     alg_id: TInt32                         - 杂凑算法 ID（CN_HASH_*）
     key: PByte                             - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     data: PByte                            - 输入数据指针
     len: TCnSize                           - 输入数据字节长度
     out_mac: PByte                         - 输出 MAC 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_kdf_pbkdf2(hash_id: TInt32; password: PByte; pwd_len: TCnSize; salt:
  PByte; salt_len: TCnSize; count: TInt32; out_key: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl;
{* 使用 PBKDF2 从口令派生密钥。

   参数：
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*）
     password: PByte                        - 口令指针
     pwd_len: TCnSize                       - 口令字节长度
     salt: PByte                            - 盐值指针
     salt_len: TCnSize                      - 盐值字节长度
     count: TInt32                          - 迭代次数
     out_key: PByte                         - 输出密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_kdf_hkdf(hash_id: TInt32; ikm: PByte; ikm_len: TCnSize; salt: PByte;
  salt_len: TCnSize; info: PByte; info_len: TCnSize; dk_len: TCnSize; out_key:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 使用 HKDF 从输入密钥材料 IKM 派生密钥。

   参数：
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*）
     ikm: PByte                             - 输入密钥材料 IKM
     ikm_len: TCnSize                       - IKM 字节长度
     salt: PByte                            - 可选盐值
     salt_len: TCnSize                      - 盐值字节长度
     info: PByte                            - 可选上下文信息
     info_len: TCnSize                      - 上下文字节长度
     dk_len: TCnSize                        - 期望派生密钥字节数
     out_key: PByte                         - 输出密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_cipher_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 对称加密：根据算法 ID、密钥与 IV 对数据加密。

   参数：
     alg_id: TInt32                         - 加密算法 ID（CN_CIPHER_*，如 AES/SM4/RC4 等）
     key: PByte                             - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     iv: PByte                              - 初始向量 IV（部分模式需要）
     iv_len: TCnSize                        - IV 字节长度
     in_ptr: PByte                          - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_ptr: PByte                         - 密文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_cipher_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 对称解密：根据算法 ID、密钥与 IV 对数据解密。

   参数：
     alg_id: TInt32                         - 加密算法 ID（CN_CIPHER_*）
     key: PByte                             - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     iv: PByte                              - 初始向量 IV
     iv_len: TCnSize                        - IV 字节长度
     in_ptr: PByte                          - 密文输入指针
     in_len: TCnSize                        - 密文字节长度
     out_ptr: PByte                         - 明文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_aead_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_ptr: PByte; in_len:
  TCnSize; out_cipher: PByte; cap_cipher: TCnSize; var out_cipher_len: TCnSize;
  out_tag: PByte; tag_cap: TCnSize; var out_tag_len: TCnSize): TCnResult; cdecl;
{* AEAD 加密（带附加认证数据）。支持 GCM/ChaCha20-Poly1305 等。

   参数：
     alg_id: TInt32                         - AEAD 算法 ID（CN_AEAD_*）
     key: PByte                             - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     nonce: PByte                           - 随机数/Nonce
     nonce_len: TCnSize                     - Nonce 字节长度
     aad: PByte                             - 附加认证数据（不加密）
     aad_len: TCnSize                       - AAD 字节长度
     in_ptr: PByte                          - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_cipher: PByte                      - 密文输出缓冲区
     cap_cipher: TCnSize                    - 密文缓冲区容量，单位字节
     out_cipher_len: TCnSize                - 实际密文字节长度
     out_tag: PByte                         - 认证标签输出缓冲区
     tag_cap: TCnSize                       - 标签缓冲区容量，单位字节
     out_tag_len: TCnSize                   - 实际标签字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_aead_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_cipher: PByte;
  in_len: TCnSize; in_tag: PByte; tag_len: TCnSize; out_plain: PByte; cap_plain:
  TCnSize; var out_plain_len: TCnSize): TCnResult; cdecl;
{* AEAD 解密与认证验证。

   参数：
     alg_id: TInt32                         - AEAD 算法 ID（CN_AEAD_*）
     key: PByte                             - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     nonce: PByte                           - 随机数/Nonce
     nonce_len: TCnSize                     - Nonce 字节长度
     aad: PByte                             - 附加认证数据
     aad_len: TCnSize                       - AAD 字节长度
     in_cipher: PByte                       - 密文输入缓冲区
     in_len: TCnSize                        - 密文字节长度
     in_tag: PByte                          - 输入认证标签
     tag_len: TCnSize                       - 标签字节长度
     out_plain: PByte                       - 明文输出缓冲区
     cap_plain: TCnSize                     - 明文缓冲区容量，单位字节
     out_plain_len: TCnSize                 - 实际明文字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，认证失败返回 CN_E_VERIFY_FAIL
}

function cn_rsa_privkey_new(use_crt: TBool32): TCnCryptoHandle; cdecl;
{* 创建 RSA 私钥对象。

   参数：
     use_crt: TBool32                       - 是否使用 CRT 优化（1/0）

   返回值：TCnCryptoHandle                  - RSA 私钥对象标识
}

function cn_rsa_pubkey_new: TCnCryptoHandle; cdecl;
{* 创建 RSA 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - RSA 公钥对象标识
}

function cn_rsa_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
{* 释放 RSA 密钥对象。

   参数：
     key: TCnCryptoHandle                   - RSA 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_generate_keys(modulus_bits: TInt32; use_crt: TBool32; var
  out_priv: TCnCryptoHandle; var out_pub: TCnCryptoHandle; use3: TBool32):
  TCnResult; cdecl;
{* 生成一对 RSA 公私钥。

   参数：
     modulus_bits: TInt32                   - 模数位数（1024/2048/3072/4096 等）
     use_crt: TBool32                       - 私钥是否启用 CRT
     out_priv: TCnCryptoHandle              - 输出 RSA 私钥对象标识
     out_pub: TCnCryptoHandle               - 输出 RSA 公钥对象标识
     use3: TBool32                          - 是否使用 e=3（否则使用 65537）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_pubkey_get_modulus_bytes(pub: TCnCryptoHandle): TCnSize; cdecl;
{* 获取 RSA 公钥模数字节数。

   参数：
     pub: TCnCryptoHandle                   - RSA 公钥对象标识

   返回值：TCnSize                          - 模数字节长度
}

function cn_rsa_privkey_get_modulus_bytes(priv: TCnCryptoHandle): TCnSize; cdecl;
{* 获取 RSA 私钥模数字节数。

   参数：
     priv: TCnCryptoHandle                  - RSA 私钥对象标识

   返回值：TCnSize                          - 模数字节长度
}

function cn_rsa_encrypt_with_public(padding: TInt32; pub: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
{* 使用 RSA 公钥加密（支持 PKCS1/OAEP 填充）。

   参数：
     padding: TInt32                        - 填充模式（CN_RSA_PAD_PKCS1 或 CN_RSA_PAD_OAEP）
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     in_ptr: PByte                          - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_ptr: PByte                         - 密文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_encrypt_with_private(priv: TCnCryptoHandle; in_ptr: PByte;
  in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 使用 RSA 私钥加密（不常用，通常用于签名流程）。

   参数：
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     in_ptr: PByte                          - 输入指针
     in_len: TCnSize                        - 输入字节长度
     out_ptr: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_decrypt_with_public(pub: TCnCryptoHandle; in_ptr: PByte; in_len:
  TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 使用 RSA 公钥解密（不常用，通常用于验签流程）。

   参数：
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     in_ptr: PByte                          - 输入指针
     in_len: TCnSize                        - 输入字节长度
     out_ptr: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_decrypt_with_private(padding: TInt32; priv: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
{* 使用 RSA 私钥解密（与公钥加密配套）。

   参数：
     padding: TInt32                        - 填充模式（CN_RSA_PAD_PKCS1 或 CN_RSA_PAD_OAEP）
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     in_ptr: PByte                          - 密文输入指针
     in_len: TCnSize                        - 密文字节长度
     out_ptr: PByte                         - 明文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_sign(digest_alg_id: TInt32; priv: TCnCryptoHandle; data: PByte;
  len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 使用 RSA 私钥生成签名。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*，如 SHA2-256 等）
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig: PByte                         - 签名输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_verify(digest_alg_id: TInt32; pub: TCnCryptoHandle; data: PByte;
  len: TCnSize; sig_ptr: PByte; sig_len: TCnSize): TBool32; cdecl;
{* 使用 RSA 公钥验证签名。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*，需与签名一致）
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig_ptr: PByte                         - 签名数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
}

function cn_rsa_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle): TCnResult; cdecl;
{* 从 PEM 文本加载 RSA 公/私钥（可选口令）。

   参数：
     pem_ptr: PByte                         - PEM 文本指针
     pem_len: TCnSize                       - PEM 文本字节长度
     password_ptr: PByte                    - 口令指针（可为空）
     password_len: TCnSize                  - 口令字节长度
     out_priv: TCnCryptoHandle              - 输出 RSA 私钥对象标识
     out_pub: TCnCryptoHandle               - 输出 RSA 公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_save_keys_to_pem(key_type_id: TInt32; priv: TCnCryptoHandle; pub:
  TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 将 RSA 公私钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_RSA_KEY_PKCS1/PKCS8）
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     out_buf: PByte                         - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_rsa_save_public_key_to_pem(key_type_id: TInt32; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 将 RSA 公钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_RSA_KEY_PKCS1/PKCS8）
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     out_buf: PByte                         - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ecc_privkey_new: TCnCryptoHandle; cdecl;
{* 创建 ECC 私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                 - ECC 私钥对象标识
}

function cn_ecc_pubkey_new: TCnCryptoHandle; cdecl;
{* 创建 ECC 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - ECC 公钥对象标识
}

function cn_ecc_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
{* 释放 ECC 密钥对象。

   参数：
     key: TCnCryptoHandle                   - ECC 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ecc_curve_bytes(curve_id: TInt32): TCnSize; cdecl;
{* 获取指定椭圆曲线的字节长度（基点/坐标大小）。

   参数：
     curve_id: TInt32                       - 曲线 ID（CN_ECC_CURVE_*）

   返回值：TCnSize                          - 字节长度
}

function cn_ecc_generate_keys(curve_id: TInt32; var out_priv: TCnCryptoHandle;
  var out_pub: TCnCryptoHandle): TCnResult; cdecl;
{* 生成一对 ECC 公私钥。

   参数：
     curve_id: TInt32                       - 曲线 ID（CN_ECC_CURVE_*）
     out_priv: TCnCryptoHandle              - 输出 ECC 私钥对象标识
     out_pub: TCnCryptoHandle               - 输出 ECC 公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ecc_sign(digest_alg_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl;
{* 使用 ECC 私钥生成签名（DER 编码）。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*）
     curve_id: TInt32                       - 曲线 ID（CN_ECC_CURVE_*）
     priv: TCnCryptoHandle                  - ECC 私钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig_der: PByte                     - 签名 DER 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ecc_verify(digest_alg_id: TInt32; curve_id: TInt32; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl;
{* 使用 ECC 公钥验证签名（DER 编码）。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*，需与签名一致）
     curve_id: TInt32                       - 曲线 ID（CN_ECC_CURVE_*）
     pub: TCnCryptoHandle                   - ECC 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig_der: PByte                         - 签名 DER 数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
}

function cn_ecc_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle; var out_curve_id: TInt32): TCnResult; cdecl;
{* 从 PEM 文本加载 ECC 公/私钥（可选口令），并返回曲线 ID。

   参数：
     pem_ptr: PByte                         - PEM 文本指针
     pem_len: TCnSize                       - PEM 文本字节长度
     password_ptr: PByte                    - 口令指针（可为空）
     password_len: TCnSize                  - 口令字节长度
     out_priv: TCnCryptoHandle              - 输出 ECC 私钥对象标识
     out_pub: TCnCryptoHandle               - 输出 ECC 公钥对象标识
     out_curve_id: TInt32                   - 输出曲线 ID（CN_ECC_CURVE_*）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ecc_save_keys_to_pem(key_type_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl;
{* 将 ECC 公私钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_ECC_KEY_PKCS1/PKCS8）
     curve_id: TInt32                       - 曲线 ID（CN_ECC_CURVE_*）
     priv: TCnCryptoHandle                  - ECC 私钥对象标识
     pub: TCnCryptoHandle                   - ECC 公钥对象标识
     out_buf: PByte                         - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ecc_save_public_key_to_pem(key_type_id: TInt32; curve_id: TInt32;
  pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
{* 将 ECC 公钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_ECC_KEY_PKCS1/PKCS8）
     curve_id: TInt32                       - 曲线 ID（CN_ECC_CURVE_*）
     pub: TCnCryptoHandle                   - ECC 公钥对象标识
     out_buf: PByte                         - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_sm2_privkey_new: TCnCryptoHandle; cdecl;
{* 创建 SM2 私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - SM2 私钥对象标识
}

function cn_sm2_pubkey_new: TCnCryptoHandle; cdecl;
{* 创建 SM2 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - SM2 公钥对象标识
}

function cn_sm2_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
{* 释放 SM2 密钥对象。

   参数：
     key: TCnCryptoHandle                   - SM2 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_sm2_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
{* 生成一对 SM2 公私钥。

   参数：
     out_priv: TCnCryptoHandle              - 输出 SM2 私钥对象标识
     out_pub: TCnCryptoHandle               - 输出 SM2 公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_sm2_encrypt(seq_type_id: TInt32; include_prefix: TBool32; pub:
  TCnCryptoHandle; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl;
{* 使用 SM2 公钥加密（支持 C1C3C2/C1C2C3 序列）。

   参数：
     seq_type_id: TInt32                    - 输出序列类型（CN_SM2_SEQ_*）
     include_prefix: TBool32                - 是否包含未压缩前缀
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     in_ptr: PByte                          - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_ptr: PByte                         - 密文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_sm2_decrypt(seq_type_id: TInt32; priv: TCnCryptoHandle; in_ptr:
  PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
{* 使用 SM2 私钥解密。

   参数：
     seq_type_id: TInt32                    - 输入序列类型（CN_SM2_SEQ_*）
     priv: TCnCryptoHandle                  - SM2 私钥对象标识
     in_ptr: PByte                          - 密文输入指针
     in_len: TCnSize                        - 密文字节长度
     out_ptr: PByte                         - 明文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_sm2_sign(user_id: PByte; user_id_len: TCnSize; priv: TCnCryptoHandle;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* SM2 签名（DER 编码），包含用户标识（ZA）计算。

   参数：
     user_id: PByte                         - 用户标识 ID
     user_id_len: TCnSize                   - 用户标识字节长度
     priv: TCnCryptoHandle                  - SM2 私钥对象标识
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig_der: PByte                     - 签名 DER 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_sm2_verify(user_id: PByte; user_id_len: TCnSize; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl;
{* SM2 验证签名（DER 编码）。

   参数：
     user_id: PByte                         - 用户标识 ID
     user_id_len: TCnSize                   - 用户标识字节长度
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig_der: PByte                         - 签名 DER 数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
}

function cn_sm2_save_keys_to_pem(priv: TCnCryptoHandle; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 将 SM2 公私钥以 PEM 格式导出。

   参数：
     priv: TCnCryptoHandle                  - SM2 私钥对象标识
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     out_buf: PByte                         - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_sm2_save_public_key_to_pem(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 将 SM2 公钥以 PEM 格式导出。

   参数：
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     out_buf: PByte                         - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_privkey_new: TCnCryptoHandle; cdecl;
{* 创建 Ed25519 私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - Ed25519 私钥对象标识
}

function cn_ed25519_pubkey_new: TCnCryptoHandle; cdecl;
{* 创建 Ed25519 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - Ed25519 公钥对象标识
}

function cn_ed25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
{* 释放 Ed25519 密钥对象。

   参数：
     key: TCnCryptoHandle                   - 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
{* 生成一对 Ed25519 公私钥。

   参数：
     out_priv: TCnCryptoHandle              - 输出私钥对象标识
     out_pub: TCnCryptoHandle               - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_sign(priv: TCnCryptoHandle; pub: TCnCryptoHandle; data:
  PByte; len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
{* Ed25519 签名。

   参数：
     priv: TCnCryptoHandle                  - Ed25519 私钥对象标识
     pub: TCnCryptoHandle                   - Ed25519 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig: PByte                         - 签名输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_verify(pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig:
  PByte; sig_len: TCnSize): TBool32; cdecl;
{* Ed25519 验证签名。

   参数：
     pub: TCnCryptoHandle                   - Ed25519 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig: PByte                             - 签名数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
}

function cn_ed25519_sign_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* Ed25519 扩展签名（带上下文/预杂凑选择）。

   参数：
     ctx: PByte                             - 上下文字符串指针（可选）
     ctx_len: TCnSize                       - 上下文字节长度
     ph_flag: TBool32                       - 预杂凑标记（1 表示使用）
     priv: TCnCryptoHandle                  - 私钥对象标识
     pub: TCnCryptoHandle                   - 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig: PByte                         - 签名输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_verify_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig: PByte; sig_len: TCnSize):
  TBool32; cdecl;
{* Ed25519 扩展验签（带上下文/预杂凑）。

   参数：
     ctx: PByte                             - 上下文字符串指针（可选）
     ctx_len: TCnSize                       - 上下文字节长度
     ph_flag: TBool32                       - 预杂凑标记（1 表示使用）
     pub: TCnCryptoHandle                   - 公钥对象标识
     data: PByte                            - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig: PByte                             - 签名数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
}

function cn_curve25519_privkey_new: TCnCryptoHandle; cdecl;
{* 创建 Curve25519（X25519）私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - 私钥对象标识
}

function cn_curve25519_pubkey_new: TCnCryptoHandle; cdecl;
{* 创建 Curve25519（X25519）公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - 公钥对象标识
}

function cn_curve25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
{* 释放 Curve25519 密钥对象。

   参数：
     key: TCnCryptoHandle                   - 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
{* 生成一对 Curve25519 公私钥。

   参数：
     out_priv: TCnCryptoHandle              - 输出私钥对象标识
     out_pub: TCnCryptoHandle               - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_dh_step1(self_priv: TCnCryptoHandle; out_point_bytes:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* X25519 DH 第一步：由私钥导出公钥点（字节）。

   参数：
     self_priv: TCnCryptoHandle             - 自己的私钥对象标识
     out_point_bytes: PByte                 - 输出公钥点缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_dh_step2(self_priv: TCnCryptoHandle; peer_point_bytes:
  PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
{* X25519 DH 第二步：与对方公钥点计算共享密钥。

   参数：
     self_priv: TCnCryptoHandle             - 自己的私钥对象标识
     peer_point_bytes: PByte                - 对方公钥点字节
     peer_len: TCnSize                      - 公钥点字节长度
     out_shared_bytes: PByte                - 输出共享密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_dh(self_priv: TCnCryptoHandle; peer_point_bytes: PByte;
  peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
{* X25519 DH 一步完成：计算共享密钥。

   参数：
     self_priv: TCnCryptoHandle             - 自己的私钥对象标识
     peer_point_bytes: PByte                - 对方公钥点字节
     peer_len: TCnSize                      - 公钥点字节长度
     out_shared_bytes: PByte                - 输出共享密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_dh_bytes(self_priv_bytes: PByte; self_len: TCnSize;
  peer_point_bytes: PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* X25519 DH（字节版）：由私钥字节与对方公钥点字节计算共享密钥。

   参数：
     self_priv_bytes: PByte                 - 自己的私钥字节
     self_len: TCnSize                      - 私钥字节长度
     peer_point_bytes: PByte                - 对方公钥点字节
     peer_len: TCnSize                      - 公钥点字节长度
     out_shared_bytes: PByte                - 输出共享密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_privkey_from_bytes(data: PByte; len: TCnSize; var out_priv:
  TCnCryptoHandle): TCnResult; cdecl;
{* 由字节导入 Ed25519 私钥。

   参数：
     data: PByte                            - 私钥字节
     len: TCnSize                           - 私钥字节长度
     out_priv: TCnCryptoHandle              - 输出私钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 导出 Ed25519 私钥为字节。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
{* 由字节导入 Ed25519 公钥。

   参数：
     data: PByte                            - 公钥字节
     len: TCnSize                           - 公钥字节长度
     out_pub: TCnCryptoHandle               - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 导出 Ed25519 公钥为字节。

   参数：
     pub: TCnCryptoHandle                   - 公钥对象标识
     out_buf: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_privkey_from_bytes(data: PByte; len: TCnSize; var
  out_priv: TCnCryptoHandle): TCnResult; cdecl;
{* 由字节导入 Curve25519 私钥。

   参数：
     data: PByte                            - 私钥字节
     len: TCnSize                           - 私钥字节长度
     out_priv: TCnCryptoHandle              - 输出私钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 导出 Curve25519 私钥为字节。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
{* 由字节导入 Curve25519 公钥。

   参数：
     data: PByte                            - 公钥字节
     len: TCnSize                           - 公钥字节长度
     out_pub: TCnCryptoHandle               - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 导出 Curve25519 公钥为字节。

   参数：
     pub: TCnCryptoHandle                   - 公钥对象标识
     out_buf: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
{* 由 Ed25519 私钥派生对应公钥（对象）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_pub: TCnCryptoHandle               - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_ed25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 由 Ed25519 私钥派生对应公钥（字节）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
{* 由 Curve25519 私钥派生对应公钥（对象）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_pub: TCnCryptoHandle               - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_curve25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
{* 由 Curve25519 私钥派生对应公钥（字节）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: PByte                         - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize                       - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_mlkem_generate_keys(type_id: TInt32; rand_d_hex: PByte; rand_d_len:
  TCnSize; rand_z_hex: PByte; rand_z_len: TCnSize; out_encap_key: PByte;
  encap_cap: TCnSize; var out_encap_len: TCnSize; out_decap_key: PByte;
  decap_cap: TCnSize; var out_decap_len: TCnSize): TCnResult; cdecl;
{* ML-KEM 生成密钥对，供抗量子密钥封装机制使用。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     rand_d_hex: PByte                      - D 随机源（十六进制 ASCII）
     rand_d_len: TCnSize                    - D 随机源字节长度
     rand_z_hex: PByte                      - Z 随机源（十六进制 ASCII）
     rand_z_len: TCnSize                    - Z 随机源字节长度
     out_encap_key: PByte                   - 输出封装公钥
     encap_cap: TCnSize                     - 封装公钥缓冲区容量
     out_encap_len: TCnSize                 - 实际封装公钥长度
     out_decap_key: PByte                   - 输出解封私钥
     decap_cap: TCnSize                     - 解封私钥缓冲区容量
     out_decap_len: TCnSize                 - 实际解封私钥长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_mlkem_encaps(type_id: TInt32; encap_key: PByte; encap_len: TCnSize;
  msg: PByte; msg_len: TCnSize; out_share_key: PByte; share_cap: TCnSize; var
  out_share_len: TCnSize; out_cipher: PByte; cipher_cap: TCnSize; var
  out_cipher_len: TCnSize): TCnResult; cdecl;
{* ML-KEM 封装共享密钥。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     encap_key: PByte                       - 封装公钥
     encap_len: TCnSize                     - 公钥字节长度
     msg: PByte                             - 可选消息（用于确定性封装）
     msg_len: TCnSize                       - 消息字节长度
     out_share_key: PByte                   - 输出共享密钥
     share_cap: TCnSize                     - 共享密钥缓冲区容量
     out_share_len: TCnSize                 - 实际共享密钥长度
     out_cipher: PByte                      - 输出密文
     cipher_cap: TCnSize                    - 密文缓冲区容量
     out_cipher_len: TCnSize                - 实际密文长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_mlkem_decaps(type_id: TInt32; decap_key: PByte; decap_len: TCnSize;
  cipher: PByte; cipher_len: TCnSize; out_share_key: PByte; share_cap: TCnSize;
  var out_share_len: TCnSize): TCnResult; cdecl;
{* ML-KEM 解封共享密钥。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     decap_key: PByte                       - 解封私钥
     decap_len: TCnSize                     - 私钥字节长度
     cipher: PByte                          - 输入密文
     cipher_len: TCnSize                    - 密文字节长度
     out_share_key: PByte                   - 输出共享密钥
     share_cap: TCnSize                     - 共享密钥缓冲区容量
     out_share_len: TCnSize                 - 实际共享密钥长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_mldsa_generate_keys(type_id: TInt32; rand_hex: PByte; rand_len:
  TCnSize; out_priv: PByte; priv_cap: TCnSize; var out_priv_len: TCnSize;
  out_pub: PByte; pub_cap: TCnSize; var out_pub_len: TCnSize): TCnResult; cdecl;
{* ML-DSA 生成签名密钥对（后量子）。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）
     rand_hex: PByte                        - 随机源（十六进制 ASCII）
     rand_len: TCnSize                      - 随机源字节长度
     out_priv: PByte                        - 输出私钥字节
     priv_cap: TCnSize                      - 私钥缓冲区容量
     out_priv_len: TCnSize                  - 实际私钥长度
     out_pub: PByte                         - 输出公钥字节
     pub_cap: TCnSize                       - 公钥缓冲区容量
     out_pub_len: TCnSize                   - 实际公钥长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_mldsa_sign(type_id: TInt32; sk: PByte; sk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; ctx: PByte; ctx_len: TCnSize; hash_id: TInt32; rand_hex:
  PByte; rand_len: TCnSize; out_sig: PByte; sig_cap: TCnSize; var out_sig_len:
  TCnSize): TCnResult; cdecl;
{* ML-DSA 生成签名。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）
     sk: PByte                              - 私钥字节
     sk_len: TCnSize                        - 私钥字节长度
     msg: PByte                             - 消息指针
     msg_len: TCnSize                       - 消息字节长度
     ctx: PByte                             - 可选上下文
     ctx_len: TCnSize                       - 上下文字节长度
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*，可选）
     rand_hex: PByte                        - 随机源（十六进制 ASCII）
     rand_len: TCnSize                      - 随机源字节长度
     out_sig: PByte                         - 输出签名字节
     sig_cap: TCnSize                       - 签名缓冲区容量
     out_sig_len: TCnSize                   - 实际签名长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_mldsa_verify(type_id: TInt32; pk: PByte; pk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; sig: PByte; sig_len: TCnSize; ctx: PByte; ctx_len: TCnSize;
  hash_id: TInt32): TBool32; cdecl;
{* ML-DSA 验证签名。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）
     pk: PByte                              - 公钥字节
     pk_len: TCnSize                        - 公钥字节长度
     msg: PByte                             - 消息指针
     msg_len: TCnSize                       - 消息字节长度
     sig: PByte                             - 签名字节
     sig_len: TCnSize                       - 签名字节长度
     ctx: PByte                             - 可选上下文
     ctx_len: TCnSize                       - 上下文字节长度
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*，可选）

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
}

function cn_mlkem_check_encap_key(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize): TCnResult; cdecl;
{* 校验 ML-KEM 封装公钥格式与合法性。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     encap_key: PByte                       - 封装公钥
     encap_len: TCnSize                     - 公钥字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示合法
}

function cn_mlkem_check_decap_key(type_id: TInt32; decap_key: PByte; decap_len:
  TCnSize): TCnResult; cdecl;
{* 校验 ML-KEM 解封私钥格式与合法性。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     decap_key: PByte                       - 解封私钥
     decap_len: TCnSize                     - 私钥字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示合法
}

function cn_mlkem_encaps_auto(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize; out_share_key: PByte; share_cap: TCnSize; var out_share_len: TCnSize;
  out_cipher: PByte; cipher_cap: TCnSize; var out_cipher_len: TCnSize): TCnResult; cdecl;
{* ML-KEM 自动封装共享密钥（不带消息）。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     encap_key: PByte                       - 封装公钥
     encap_len: TCnSize                     - 公钥字节长度
     out_share_key: PByte                   - 输出共享密钥
     share_cap: TCnSize                     - 共享密钥缓冲区容量
     out_share_len: TCnSize                 - 实际共享密钥长度
     out_cipher: PByte                      - 输出密文
     cipher_cap: TCnSize                    - 密文缓冲区容量
     out_cipher_len: TCnSize                - 实际密文长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
}

function cn_mlkem_expected_encap_key_len(type_id: TInt32): TCnSize; cdecl;
{* 查询 ML-KEM 封装公钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
}

function cn_mlkem_expected_decap_key_len(type_id: TInt32): TCnSize; cdecl;
{* 查询 ML-KEM 解封私钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
}

function cn_mlkem_expected_cipher_len(type_id: TInt32): TCnSize; cdecl;
{* 查询 ML-KEM 密文的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
}

function cn_mlkem_share_key_bytes: TCnSize; cdecl;
{* 查询 ML-KEM 共享密钥的字节长度。

   参数：
     （无）

   返回值：TCnSize                          - 字节长度
}

function cn_mldsa_expected_privkey_len(type_id: TInt32): TCnSize; cdecl;
{* 查询 ML-DSA 私钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
}

function cn_mldsa_expected_pubkey_len(type_id: TInt32): TCnSize; cdecl;
{* 查询 ML-DSA 公钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
}

function cn_mldsa_expected_sig_len(type_id: TInt32): TCnSize; cdecl;
{* 查询 ML-DSA 签名的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
}

implementation

const
{$IFDEF MSWINDOWS}
  CnCryptoLib = 'CnCrypto.dll';
{$ELSE}
  CnCryptoLib = 'CnCrypto';
{$ENDIF}

function cn_get_version; external CnCryptoLib name 'cn_get_version';
function cn_get_abi_version; external CnCryptoLib name 'cn_get_abi_version';
function cn_lib_init; external CnCryptoLib name 'cn_lib_init';
function cn_lib_finalize; external CnCryptoLib name 'cn_lib_finalize';
function cn_alloc; external CnCryptoLib name 'cn_alloc';
function cn_free; external CnCryptoLib name 'cn_free';
function cn_memzero; external CnCryptoLib name 'cn_memzero';
function cn_endian_is_le; external CnCryptoLib name 'cn_endian_is_le';
function cn_endian_is_be; external CnCryptoLib name 'cn_endian_is_be';
function cn_data_to_hex; external CnCryptoLib name 'cn_data_to_hex';
function cn_const_time_equal; external CnCryptoLib name 'cn_const_time_equal';
function cn_const_time_select; external CnCryptoLib name 'cn_const_time_select';
function cn_str_to_uint64; external CnCryptoLib name 'cn_str_to_uint64';
function cn_base64_encode; external CnCryptoLib name 'cn_base64_encode';
function cn_base64_decode; external CnCryptoLib name 'cn_base64_decode';
function cn_base64url_encode; external CnCryptoLib name 'cn_base64url_encode';
function cn_base64url_decode; external CnCryptoLib name 'cn_base64url_decode';
function cn_otp_hotp; external CnCryptoLib name 'cn_otp_hotp';
function cn_otp_totp; external CnCryptoLib name 'cn_otp_totp';
function cn_hash_digest; external CnCryptoLib name 'cn_hash_digest';
function cn_hmac; external CnCryptoLib name 'cn_hmac';
function cn_kdf_pbkdf2; external CnCryptoLib name 'cn_kdf_pbkdf2';
function cn_kdf_hkdf; external CnCryptoLib name 'cn_kdf_hkdf';
function cn_cipher_encrypt; external CnCryptoLib name 'cn_cipher_encrypt';
function cn_cipher_decrypt; external CnCryptoLib name 'cn_cipher_decrypt';
function cn_aead_encrypt; external CnCryptoLib name 'cn_aead_encrypt';
function cn_aead_decrypt; external CnCryptoLib name 'cn_aead_decrypt';
function cn_rsa_privkey_new; external CnCryptoLib name 'cn_rsa_privkey_new';
function cn_rsa_pubkey_new; external CnCryptoLib name 'cn_rsa_pubkey_new';
function cn_rsa_key_free; external CnCryptoLib name 'cn_rsa_key_free';
function cn_rsa_generate_keys; external CnCryptoLib name 'cn_rsa_generate_keys';
function cn_rsa_pubkey_get_modulus_bytes; external CnCryptoLib name 'cn_rsa_pubkey_get_modulus_bytes';
function cn_rsa_privkey_get_modulus_bytes; external CnCryptoLib name 'cn_rsa_privkey_get_modulus_bytes';
function cn_rsa_encrypt_with_public; external CnCryptoLib name 'cn_rsa_encrypt_with_public';
function cn_rsa_encrypt_with_private; external CnCryptoLib name 'cn_rsa_encrypt_with_private';
function cn_rsa_decrypt_with_public; external CnCryptoLib name 'cn_rsa_decrypt_with_public';
function cn_rsa_decrypt_with_private; external CnCryptoLib name 'cn_rsa_decrypt_with_private';
function cn_rsa_sign; external CnCryptoLib name 'cn_rsa_sign';
function cn_rsa_verify; external CnCryptoLib name 'cn_rsa_verify';
function cn_rsa_load_keys_from_pem; external CnCryptoLib name 'cn_rsa_load_keys_from_pem';
function cn_rsa_save_keys_to_pem; external CnCryptoLib name 'cn_rsa_save_keys_to_pem';
function cn_rsa_save_public_key_to_pem; external CnCryptoLib name 'cn_rsa_save_public_key_to_pem';
function cn_ecc_privkey_new; external CnCryptoLib name 'cn_ecc_privkey_new';
function cn_ecc_pubkey_new; external CnCryptoLib name 'cn_ecc_pubkey_new';
function cn_ecc_key_free; external CnCryptoLib name 'cn_ecc_key_free';
function cn_ecc_curve_bytes; external CnCryptoLib name 'cn_ecc_curve_bytes';
function cn_ecc_generate_keys; external CnCryptoLib name 'cn_ecc_generate_keys';
function cn_ecc_sign; external CnCryptoLib name 'cn_ecc_sign';
function cn_ecc_verify; external CnCryptoLib name 'cn_ecc_verify';
function cn_ecc_load_keys_from_pem; external CnCryptoLib name 'cn_ecc_load_keys_from_pem';
function cn_ecc_save_keys_to_pem; external CnCryptoLib name 'cn_ecc_save_keys_to_pem';
function cn_ecc_save_public_key_to_pem; external CnCryptoLib name 'cn_ecc_save_public_key_to_pem';
function cn_sm2_privkey_new; external CnCryptoLib name 'cn_sm2_privkey_new';
function cn_sm2_pubkey_new; external CnCryptoLib name 'cn_sm2_pubkey_new';
function cn_sm2_key_free; external CnCryptoLib name 'cn_sm2_key_free';
function cn_sm2_generate_keys; external CnCryptoLib name 'cn_sm2_generate_keys';
function cn_sm2_encrypt; external CnCryptoLib name 'cn_sm2_encrypt';
function cn_sm2_decrypt; external CnCryptoLib name 'cn_sm2_decrypt';
function cn_sm2_sign; external CnCryptoLib name 'cn_sm2_sign';
function cn_sm2_verify; external CnCryptoLib name 'cn_sm2_verify';
function cn_sm2_save_keys_to_pem; external CnCryptoLib name 'cn_sm2_save_keys_to_pem';
function cn_sm2_save_public_key_to_pem; external CnCryptoLib name 'cn_sm2_save_public_key_to_pem';
function cn_ed25519_privkey_new; external CnCryptoLib name 'cn_ed25519_privkey_new';
function cn_ed25519_pubkey_new; external CnCryptoLib name 'cn_ed25519_pubkey_new';
function cn_ed25519_key_free; external CnCryptoLib name 'cn_ed25519_key_free';
function cn_ed25519_generate_keys; external CnCryptoLib name 'cn_ed25519_generate_keys';
function cn_ed25519_sign; external CnCryptoLib name 'cn_ed25519_sign';
function cn_ed25519_verify; external CnCryptoLib name 'cn_ed25519_verify';
function cn_ed25519_sign_ex; external CnCryptoLib name 'cn_ed25519_sign_ex';
function cn_ed25519_verify_ex; external CnCryptoLib name 'cn_ed25519_verify_ex';
function cn_curve25519_privkey_new; external CnCryptoLib name 'cn_curve25519_privkey_new';
function cn_curve25519_pubkey_new; external CnCryptoLib name 'cn_curve25519_pubkey_new';
function cn_curve25519_key_free; external CnCryptoLib name 'cn_curve25519_key_free';
function cn_curve25519_generate_keys; external CnCryptoLib name 'cn_curve25519_generate_keys';
function cn_curve25519_dh_step1; external CnCryptoLib name 'cn_curve25519_dh_step1';
function cn_curve25519_dh_step2; external CnCryptoLib name 'cn_curve25519_dh_step2';
function cn_curve25519_dh; external CnCryptoLib name 'cn_curve25519_dh';
function cn_curve25519_dh_bytes; external CnCryptoLib name 'cn_curve25519_dh_bytes';
function cn_ed25519_privkey_from_bytes; external CnCryptoLib name 'cn_ed25519_privkey_from_bytes';
function cn_ed25519_privkey_to_bytes; external CnCryptoLib name 'cn_ed25519_privkey_to_bytes';
function cn_ed25519_pubkey_from_bytes; external CnCryptoLib name 'cn_ed25519_pubkey_from_bytes';
function cn_ed25519_pubkey_to_bytes; external CnCryptoLib name 'cn_ed25519_pubkey_to_bytes';
function cn_curve25519_privkey_from_bytes; external CnCryptoLib name 'cn_curve25519_privkey_from_bytes';
function cn_curve25519_privkey_to_bytes; external CnCryptoLib name 'cn_curve25519_privkey_to_bytes';
function cn_curve25519_pubkey_from_bytes; external CnCryptoLib name 'cn_curve25519_pubkey_from_bytes';
function cn_curve25519_pubkey_to_bytes; external CnCryptoLib name 'cn_curve25519_pubkey_to_bytes';
function cn_ed25519_derive_public; external CnCryptoLib name 'cn_ed25519_derive_public';
function cn_ed25519_derive_public_to_bytes; external CnCryptoLib name 'cn_ed25519_derive_public_to_bytes';
function cn_curve25519_derive_public; external CnCryptoLib name 'cn_curve25519_derive_public';
function cn_curve25519_derive_public_to_bytes; external CnCryptoLib name 'cn_curve25519_derive_public_to_bytes';
function cn_mlkem_generate_keys; external CnCryptoLib name 'cn_mlkem_generate_keys';
function cn_mlkem_encaps; external CnCryptoLib name 'cn_mlkem_encaps';
function cn_mlkem_decaps; external CnCryptoLib name 'cn_mlkem_decaps';
function cn_mldsa_generate_keys; external CnCryptoLib name 'cn_mldsa_generate_keys';
function cn_mldsa_sign; external CnCryptoLib name 'cn_mldsa_sign';
function cn_mldsa_verify; external CnCryptoLib name 'cn_mldsa_verify';
function cn_mlkem_check_encap_key; external CnCryptoLib name 'cn_mlkem_check_encap_key';
function cn_mlkem_check_decap_key; external CnCryptoLib name 'cn_mlkem_check_decap_key';
function cn_mlkem_encaps_auto; external CnCryptoLib name 'cn_mlkem_encaps_auto';
function cn_mlkem_expected_encap_key_len; external CnCryptoLib name 'cn_mlkem_expected_encap_key_len';
function cn_mlkem_expected_decap_key_len; external CnCryptoLib name 'cn_mlkem_expected_decap_key_len';
function cn_mlkem_expected_cipher_len; external CnCryptoLib name 'cn_mlkem_expected_cipher_len';
function cn_mlkem_share_key_bytes; external CnCryptoLib name 'cn_mlkem_share_key_bytes';
function cn_mldsa_expected_privkey_len; external CnCryptoLib name 'cn_mldsa_expected_privkey_len';
function cn_mldsa_expected_pubkey_len; external CnCryptoLib name 'cn_mldsa_expected_pubkey_len';
function cn_mldsa_expected_sig_len; external CnCryptoLib name 'cn_mldsa_expected_sig_len';

end.
