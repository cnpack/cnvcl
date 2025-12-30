/******************************************************************************}
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
{******************************************************************************/

#ifndef CNCRYPTO_INTF_H
#define CNCRYPTO_INTF_H

#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

#if defined(_WIN32) || defined(__CYGWIN__)
  #if defined(CNCRYPTO_BUILD)
    #define CNCRYPTO_API __declspec(dllexport)
  #else
    #define CNCRYPTO_API __declspec(dllimport)
  #endif
  #define CNCRYPTO_CALL __cdecl
#else
  #if defined(__GNUC__) && (__GNUC__ >= 4)
    #define CNCRYPTO_API __attribute__((visibility("default")))
  #else
    #define CNCRYPTO_API
  #endif
  #define CNCRYPTO_CALL
#endif

typedef uint8_t  TUInt8;
typedef uint32_t TUInt32;
typedef uint64_t TUInt64;
typedef int32_t  TInt32;
typedef int64_t  TInt64;
typedef int16_t  TInt16;
typedef int8_t   TInt8;
typedef int32_t  TBool32;
typedef int32_t  TCnSize;
typedef void*    TCnCryptoHandle;
typedef TInt32   TCnResult;

#define CN_OK                  0
#define CN_E_INVALID_ARG      -1
#define CN_E_BUFFER_TOO_SMALL -2
#define CN_E_UNSUPPORTED      -3
#define CN_E_NO_MEMORY        -4
#define CN_E_STATE            -5
#define CN_E_VERIFY_FAIL      -6
#define CN_E_INTERNAL        -100

#define CN_HASH_MD5               1
#define CN_HASH_SHA1              2
#define CN_HASH_SHA2_256          4
#define CN_HASH_SHA2_512          6
#define CN_HASH_SHA3_224          8
#define CN_HASH_SHA3_256          9
#define CN_HASH_SHA3_384          10
#define CN_HASH_SHA3_512          11
#define CN_HASH_SM3               12
#define CN_HASH_BLAKE224          20
#define CN_HASH_BLAKE256          21
#define CN_HASH_BLAKE384          22
#define CN_HASH_BLAKE512          23
#define CN_HASH_BLAKE2S           24
#define CN_HASH_BLAKE2B           25
#define CN_HASH_XXH32             30
#define CN_HASH_XXH64             31

#define CN_CIPHER_AES128_CBC      1001
#define CN_CIPHER_AES192_CBC      1002
#define CN_CIPHER_AES256_CBC      1003
#define CN_CIPHER_AES128_ECB      1004
#define CN_CIPHER_AES192_ECB      1005
#define CN_CIPHER_AES256_ECB      1006
#define CN_CIPHER_AES128_CTR      1101
#define CN_CIPHER_AES192_CTR      1102
#define CN_CIPHER_AES256_CTR      1103
#define CN_CIPHER_AES128_OFB      1201
#define CN_CIPHER_AES192_OFB      1202
#define CN_CIPHER_AES256_OFB      1203
#define CN_CIPHER_AES128_CFB      1301
#define CN_CIPHER_AES192_CFB      1302
#define CN_CIPHER_AES256_CFB      1303
#define CN_CIPHER_DES_ECB         1401
#define CN_CIPHER_DES_CBC         1402
#define CN_CIPHER_3DES_ECB        1403
#define CN_CIPHER_3DES_CBC        1404
#define CN_CIPHER_SM4_ECB         1501
#define CN_CIPHER_SM4_CBC         1502
#define CN_CIPHER_SM4_CFB         1503
#define CN_CIPHER_SM4_OFB         1504
#define CN_CIPHER_SM4_CTR         1505
#define CN_CIPHER_RC4             1601
#define CN_CIPHER_ZUC             1801
#define CN_CIPHER_CHACHA20        1901

#define CN_AEAD_AES128_GCM        2001
#define CN_AEAD_AES192_GCM        2002
#define CN_AEAD_AES256_GCM        2003
#define CN_AEAD_SM4_GCM           2004
#define CN_AEAD_TAG_BYTES         16
#define CN_AEAD_CHACHA20_POLY1305 2101
#define CN_AEAD_XCHACHA20_POLY1305 2102

#define CN_RSA_PAD_PKCS1          1
#define CN_RSA_PAD_OAEP           2
#define CN_RSA_KEY_PKCS1          1
#define CN_RSA_KEY_PKCS8          2

#define CN_ECC_CURVE_SM2          3001
#define CN_ECC_CURVE_SECP256K1    3002
#define CN_ECC_CURVE_SECP256R1    3003
#define CN_ECC_CURVE_PRIME256V1   3004
#define CN_ECC_CURVE_SECP384R1    3005
#define CN_ECC_CURVE_SECP521R1    3006
#define CN_ECC_KEY_PKCS1          1
#define CN_ECC_KEY_PKCS8          2
#define CN_SM2_SEQ_C1C3C2         1
#define CN_SM2_SEQ_C1C2C3         2

#define CN_HASH_SHAKE128          40
#define CN_HASH_SHAKE256          41

#define CN_MLKEM_TYPE_512         5101
#define CN_MLKEM_TYPE_768         5102
#define CN_MLKEM_TYPE_1024        5103

#define CN_MLDSA_TYPE_44          5201
#define CN_MLDSA_TYPE_65          5202
#define CN_MLDSA_TYPE_87          5203

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_get_version(TUInt32* out_major, TUInt32* out_minor, TUInt32* out_patch);
/* 获取库版本号（主/次/修订）。

   参数：
     out_major: TUInt32                    - 输出主版本号
     out_minor: TUInt32                    - 输出次版本号
     out_patch: TUInt32                    - 输出修订版本号

   返回值：TCnResult                       - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TUInt32   CNCRYPTO_CALL cn_get_abi_version(void);
/* 获取库的 ABI 版本，用于二进制兼容性判断。

   参数：
     （无）


   返回值：TUInt32                         - ABI 版本号
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_lib_init(void);
/* 初始化加密库，全局准备工作。

   参数：
     （无）

   返回值：TCnResult                       - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_lib_finalize(void);
/* 释放加密库，清理全局资源。

   参数：
     （无）

   返回值：TCnResult                       - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_alloc(TCnSize size);
/* 分配指定大小的安全内存。

   参数：
     size: TCnSize                          - 需要分配的字节数

   返回值：TCnCryptoHandle                  - 返回内存对象标识，失败为 NULL
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_free(TCnCryptoHandle ptr);
/* 释放由库分配的内存。

   参数：
     ptr: TCnCryptoHandle                   - 由 cn_alloc 返回的对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_memzero(TCnCryptoHandle ptr, TCnSize size);
/* 将指定内存区域安全清零。

   参数：
     ptr: TCnCryptoHandle                   - 目标内存对象标识
     size: TCnSize                          - 清零的字节数

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_endian_is_le(void);
/* 判断当前平台是否小端序。

   参数：
     （无）

   返回值：TBool32                          - 1 表示是，0 表示否
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_endian_is_be(void);
/* 判断当前平台是否大端序。

   参数：
     （无）

   返回值：TBool32                          - 1 表示是，0 表示否
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_data_to_hex(void* in_ptr, TCnSize in_len, uint8_t* out_hex, TCnSize cap, TCnSize* out_len);
/* 将二进制数据编码为十六进制字符串（小写，无分隔）。

   参数：
     in_ptr: void*                          - 输入数据指针
     in_len: TCnSize                        - 输入数据字节长度
     out_hex: uint8_t*                      - 输出缓冲区（ASCII）
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，缓冲区不足返回 CN_E_BUFFER_TOO_SMALL
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_const_time_equal(void* a, void* b, TCnSize len);
/* 常数时间比较两个缓冲区是否相等，避免时序攻击。

   参数：
     a: void*                               - 缓冲区 A
     b: void*                               - 缓冲区 B
     len: TCnSize                           - 比较字节长度

   返回值：TBool32                          - 1 表示相等，0 表示不等
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_const_time_select(TBool32 flag, void* a, void* b, TCnSize len, void* out_ptr);
/* 常数时间从两个缓冲区中选择其一复制到输出，避免时序泄露。

   参数：
     flag: TBool32                          - 选择标志，非零选 a，零选 b
     a: void*                               - 源缓冲区 A
     b: void*                               - 源缓冲区 B
     len: TCnSize                           - 复制字节长度
     out_ptr: void*                         - 输出缓冲区

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_str_to_uint64(uint8_t* ascii_ptr, TCnSize len, TUInt64* out_value);
/* 将 ASCII 数字字符串转换为 64 位无符号整数。

   参数：
     ascii_ptr: uint8_t*                    - 输入 ASCII 字符串指针
     len: TCnSize                           - 字符串字节长度
     out_value: TUInt64*                    - 输出转换后的数值

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，非法输入返回 CN_E_INVALID_ARG
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_base64_encode(uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 将数据进行 Base64 编码。

   参数：
     in_ptr: uint8_t*                       - 输入数据指针
     in_len: TCnSize                        - 输入数据字节长度
     out_ptr: uint8_t*                      - 输出缓冲区（ASCII）
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_base64_decode(uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 解码 Base64 字符串为二进制数据。

   参数：
     in_ptr: uint8_t*                       - 输入 Base64 数据指针（ASCII）
     in_len: TCnSize                        - 输入字节长度
     out_ptr: uint8_t*                      - 输出二进制缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，不合法输入返回 CN_E_INVALID_ARG
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_base64url_encode(uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 将数据进行 Base64URL 编码（URL 安全字符集）。

   参数：
     in_ptr: uint8_t*                       - 输入数据指针
     in_len: TCnSize                        - 输入数据字节长度
     out_ptr: uint8_t*                      - 输出缓冲区（ASCII）
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_base64url_decode(uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 解码 Base64URL 字符串为二进制数据。

   参数：
     in_ptr: uint8_t*                       - 输入 Base64URL 数据指针（ASCII）
     in_len: TCnSize                        - 输入字节长度
     out_ptr: uint8_t*                      - 输出二进制缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，不合法输入返回 CN_E_INVALID_ARG
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_otp_hotp(uint8_t* seed, TCnSize seed_len, TUInt64 counter, TInt32 digits, uint8_t* out_code_ascii, TCnSize cap, TCnSize* out_len);
/* 生成 HOTP 一次性密码。

   参数：
     seed: uint8_t*                         - 种子密钥
     seed_len: TCnSize                      - 种子密钥字节长度
     counter: TUInt64                       - 计数器值
     digits: TInt32                         - 密码位数（通常 6 或 8）
     out_code_ascii: uint8_t*               - 输出缓冲区（ASCII）
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_otp_totp(TInt32 hash_id, uint8_t* seed, TCnSize seed_len, TInt32 period_sec, TInt32 digits, uint8_t* out_code_ascii, TCnSize cap, TCnSize* out_len);
/* 生成 TOTP 基于时间的一次性密码。

   参数：
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*，如 SHA1/SM3 等）
     seed: uint8_t*                         - 种子密钥
     seed_len: TCnSize                      - 种子密钥字节长度
     period_sec: TInt32                     - 时间步长（秒）
     digits: TInt32                         - 密码位数（通常 6 或 8）
     out_code_ascii: uint8_t*               - 输出缓冲区（ASCII）
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_hash_digest(TInt32 alg_id, uint8_t* data, TCnSize len, uint8_t* out_digest, TCnSize cap, TCnSize* out_len);
/* 计算指定算法的消息杂凑。

   参数：
     alg_id: TInt32                         - 杂凑算法 ID（CN_HASH_*）
     data: uint8_t*                         - 输入数据指针
     len: TCnSize                           - 输入数据字节长度
     out_digest: uint8_t*                   - 输出杂凑缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_hmac(TInt32 alg_id, uint8_t* key, TCnSize key_len, uint8_t* data, TCnSize len, uint8_t* out_mac, TCnSize cap, TCnSize* out_len);
/* 计算 HMAC（带密钥的消息认证码）。

   参数：
     alg_id: TInt32                         - 杂凑算法 ID（CN_HASH_*）
     key: uint8_t*                          - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     data: uint8_t*                         - 输入数据指针
     len: TCnSize                           - 输入数据字节长度
     out_mac: uint8_t*                      - 输出 MAC 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_kdf_pbkdf2(TInt32 hash_id, uint8_t* password, TCnSize pwd_len, uint8_t* salt, TCnSize salt_len, TInt32 count, uint8_t* out_key, TCnSize cap, TCnSize* out_len);
/* 使用 PBKDF2 从口令派生密钥。

   参数：
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*）
     password: uint8_t*                     - 口令指针
     pwd_len: TCnSize                       - 口令字节长度
     salt: uint8_t*                         - 盐值指针
     salt_len: TCnSize                      - 盐值字节长度
     count: TInt32                          - 迭代次数
     out_key: uint8_t*                      - 输出密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_kdf_hkdf(TInt32 hash_id, uint8_t* ikm, TCnSize ikm_len, uint8_t* salt, TCnSize salt_len, uint8_t* info, TCnSize info_len, TCnSize dk_len, uint8_t* out_key, TCnSize cap, TCnSize* out_len);
/* 使用 HKDF 从输入密钥材料 IKM 派生密钥。

   参数：
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*）
     ikm: uint8_t*                          - 输入密钥材料 IKM
     ikm_len: TCnSize                       - IKM 字节长度
     salt: uint8_t*                         - 可选盐值
     salt_len: TCnSize                      - 盐值字节长度
     info: uint8_t*                         - 可选上下文信息
     info_len: TCnSize                      - 上下文字节长度
     dk_len: TCnSize                        - 期望派生密钥字节数
     out_key: uint8_t*                      - 输出密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_cipher_encrypt(TInt32 alg_id, uint8_t* key, TCnSize key_len, uint8_t* iv, TCnSize iv_len, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 对称加密：根据算法 ID、密钥与 IV 对数据加密。

   参数：
     alg_id: TInt32                         - 加密算法 ID（AES/SM4/RC4 等）
     key: uint8_t*                          - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     iv: uint8_t*                           - 初始向量 IV（部分模式需要）
     iv_len: TCnSize                        - IV 字节长度
     in_ptr: uint8_t*                       - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_ptr: uint8_t*                      - 密文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_cipher_decrypt(TInt32 alg_id, uint8_t* key, TCnSize key_len, uint8_t* iv, TCnSize iv_len, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 对称解密：根据算法 ID、密钥与 IV 对数据解密。

   参数：
     alg_id: TInt32                         - 加密算法 ID（与加密同）
     key: uint8_t*                          - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     iv: uint8_t*                           - 初始向量 IV
     iv_len: TCnSize                        - IV 字节长度
     in_ptr: uint8_t*                       - 密文输入指针
     in_len: TCnSize                        - 密文字节长度
     out_ptr: uint8_t*                      - 明文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_aead_encrypt(TInt32 alg_id, uint8_t* key, TCnSize key_len, uint8_t* nonce, TCnSize nonce_len, uint8_t* aad, TCnSize aad_len, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_cipher, TCnSize cap_cipher, TCnSize* out_cipher_len, uint8_t* out_tag, TCnSize tag_cap, TCnSize* out_tag_len);
/* AEAD 加密（带附加认证数据）。支持 GCM/ChaCha20-Poly1305 等。

   参数：
     alg_id: TInt32                         - AEAD 算法 ID
     key: uint8_t*                          - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     nonce: uint8_t*                        - 随机数/Nonce
     nonce_len: TCnSize                     - Nonce 字节长度
     aad: uint8_t*                          - 附加认证数据（不加密）
     aad_len: TCnSize                       - AAD 字节长度
     in_ptr: uint8_t*                       - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_cipher: uint8_t*                   - 密文输出缓冲区
     cap_cipher: TCnSize                    - 密文缓冲区容量，单位字节
     out_cipher_len: TCnSize*               - 实际密文字节长度
     out_tag: uint8_t*                      - 认证标签输出缓冲区
     tag_cap: TCnSize                       - 标签缓冲区容量，单位字节
     out_tag_len: TCnSize*                  - 实际标签字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_aead_decrypt(TInt32 alg_id, uint8_t* key, TCnSize key_len, uint8_t* nonce, TCnSize nonce_len, uint8_t* aad, TCnSize aad_len, uint8_t* in_cipher, TCnSize in_len, uint8_t* in_tag, TCnSize tag_len, uint8_t* out_plain, TCnSize cap_plain, TCnSize* out_plain_len);
/* AEAD 解密与认证验证。

   参数：
     alg_id: TInt32                         - AEAD 算法 ID
     key: uint8_t*                          - 密钥指针
     key_len: TCnSize                       - 密钥字节长度
     nonce: uint8_t*                        - 随机数/Nonce
     nonce_len: TCnSize                     - Nonce 字节长度
     aad: uint8_t*                          - 附加认证数据
     aad_len: TCnSize                       - AAD 字节长度
     in_cipher: uint8_t*                    - 密文输入缓冲区
     in_len: TCnSize                        - 密文字节长度
     in_tag: uint8_t*                       - 输入认证标签
     tag_len: TCnSize                       - 标签字节长度
     out_plain: uint8_t*                    - 明文输出缓冲区
     cap_plain: TCnSize                     - 明文缓冲区容量，单位字节
     out_plain_len: TCnSize*                - 实际明文字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功，认证失败返回 CN_E_VERIFY_FAIL
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_rsa_privkey_new(TBool32 use_crt);
/* 创建 RSA 私钥对象。

   参数：
     use_crt: TBool32                       - 是否使用 CRT 优化（1/0）

   返回值：TCnCryptoHandle                  - RSA 私钥对象标识
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_rsa_pubkey_new(void);
/* 创建 RSA 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - RSA 公钥对象标识
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_key_free(TCnCryptoHandle key);
/* 释放 RSA 密钥对象。

   参数：
     key: TCnCryptoHandle                   - RSA 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_generate_keys(TInt32 modulus_bits, TBool32 use_crt, TCnCryptoHandle* out_priv, TCnCryptoHandle* out_pub, TBool32 use3);
/* 生成一对 RSA 公私钥。

   参数：
     modulus_bits: TInt32                   - 模数位数（1024/2048/3072/4096 等）
     use_crt: TBool32                       - 私钥是否启用 CRT
     out_priv: TCnCryptoHandle*             - 输出 RSA 私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出 RSA 公钥对象标识
     use3: TBool32                          - 是否使用 e=3（否则使用 65537）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_rsa_pubkey_get_modulus_bytes(TCnCryptoHandle pub);
/* 获取 RSA 公钥模数字节数。

   参数：
     pub: TCnCryptoHandle                   - RSA 公钥对象标识

   返回值：TCnSize                          - 模数字节长度
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_rsa_privkey_get_modulus_bytes(TCnCryptoHandle priv);
/* 获取 RSA 私钥模数字节数。

   参数：
     priv: TCnCryptoHandle                  - RSA 私钥对象标识

   返回值：TCnSize                          - 模数字节长度
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_encrypt_with_public(TInt32 padding, TCnCryptoHandle pub, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 使用 RSA 公钥加密（支持 PKCS1/OAEP 填充）。

   参数：
     padding: TInt32                        - 填充模式（CN_RSA_PAD_PKCS1 或 CN_RSA_PAD_OAEP）
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     in_ptr: uint8_t*                       - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_ptr: uint8_t*                      - 密文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_encrypt_with_private(TCnCryptoHandle priv, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 使用 RSA 私钥加密（不常用，通常用于签名流程）。

   参数：
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     in_ptr: uint8_t*                       - 输入指针
     in_len: TCnSize                        - 输入字节长度
     out_ptr: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_decrypt_with_public(TCnCryptoHandle pub, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 使用 RSA 公钥解密（不常用，通常用于验签流程）。

   参数：
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     in_ptr: uint8_t*                       - 输入指针
     in_len: TCnSize                        - 输入字节长度
     out_ptr: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_decrypt_with_private(TInt32 padding, TCnCryptoHandle priv, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 使用 RSA 私钥解密（与公钥加密配套）。

   参数：
     padding: TInt32                        - 填充模式（CN_RSA_PAD_PKCS1 或 CN_RSA_PAD_OAEP）
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     in_ptr: uint8_t*                       - 密文输入指针
     in_len: TCnSize                        - 密文字节长度
     out_ptr: uint8_t*                      - 明文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_sign(TInt32 digest_alg_id, TCnCryptoHandle priv, uint8_t* data, TCnSize len, uint8_t* out_sig, TCnSize cap, TCnSize* out_len);
/* 使用 RSA 私钥生成签名。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*，如 SHA2-256 等）
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig: uint8_t*                      - 签名输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_rsa_verify(TInt32 digest_alg_id, TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* sig_ptr, TCnSize sig_len);
/* 使用 RSA 公钥验证签名。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*，需与签名一致）
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig_ptr: uint8_t*                      - 签名数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_load_keys_from_pem(uint8_t* pem_ptr, TCnSize pem_len, uint8_t* password_ptr, TCnSize password_len, TCnCryptoHandle* out_priv, TCnCryptoHandle* out_pub);
/* 从 PEM 文本加载 RSA 公/私钥（可选口令）。

   参数：
     pem_ptr: uint8_t*                      - PEM 文本指针
     pem_len: TCnSize                       - PEM 文本字节长度
     password_ptr: uint8_t*                 - 口令指针（可为空）
     password_len: TCnSize                  - 口令字节长度
     out_priv: TCnCryptoHandle*             - 输出 RSA 私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出 RSA 公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_save_keys_to_pem(TInt32 key_type_id, TCnCryptoHandle priv, TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 将 RSA 公私钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_RSA_KEY_PKCS1/PKCS8）
     priv: TCnCryptoHandle                  - RSA 私钥对象标识
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     out_buf: uint8_t*                      - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_rsa_save_public_key_to_pem(TInt32 key_type_id, TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 将 RSA 公钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_RSA_KEY_PKCS1/PKCS8）
     pub: TCnCryptoHandle                   - RSA 公钥对象标识
     out_buf: uint8_t*                      - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_ecc_privkey_new(void);
/* 创建 ECC 私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - ECC 私钥对象标识
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_ecc_pubkey_new(void);
/* 创建 ECC 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - ECC 公钥对象标识
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ecc_key_free(TCnCryptoHandle key);
/* 释放 ECC 密钥对象。

   参数：
     key: TCnCryptoHandle                   - ECC 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_ecc_curve_bytes(TInt32 curve_id);
/* 获取指定椭圆曲线的字节长度（基点/坐标大小）。

   参数：
     curve_id: TInt32                       - 曲线 ID（如 SM2/SECP256R1 等）

   返回值：TCnSize                          - 字节长度
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ecc_generate_keys(TInt32 curve_id, TCnCryptoHandle* out_priv, TCnCryptoHandle* out_pub);
/* 生成一对 ECC 公私钥。

   参数：
     curve_id: TInt32                       - 曲线 ID
     out_priv: TCnCryptoHandle*             - 输出 ECC 私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出 ECC 公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ecc_sign(TInt32 digest_alg_id, TInt32 curve_id, TCnCryptoHandle priv, uint8_t* data, TCnSize len, uint8_t* out_sig_der, TCnSize cap, TCnSize* out_len);
/* 使用 ECC 私钥生成签名（DER 编码）。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*）
     curve_id: TInt32                       - 曲线 ID
     priv: TCnCryptoHandle                  - ECC 私钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig_der: uint8_t*                  - 签名 DER 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_ecc_verify(TInt32 digest_alg_id, TInt32 curve_id, TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* sig_der, TCnSize sig_len);
/* 使用 ECC 公钥验证签名（DER 编码）。

   参数：
     digest_alg_id: TInt32                  - 杂凑算法 ID（CN_HASH_*）
     curve_id: TInt32                       - 曲线 ID
     pub: TCnCryptoHandle                   - ECC 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig_der: uint8_t*                      - 签名 DER 数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ecc_load_keys_from_pem(uint8_t* pem_ptr, TCnSize pem_len, uint8_t* password_ptr, TCnSize password_len, TCnCryptoHandle* out_priv, TCnCryptoHandle* out_pub, TInt32* out_curve_id);
/* 从 PEM 文本加载 ECC 公/私钥（可选口令），并返回曲线 ID。

   参数：
     pem_ptr: uint8_t*                      - PEM 文本指针
     pem_len: TCnSize                       - PEM 文本字节长度
     password_ptr: uint8_t*                 - 口令指针（可为空）
     password_len: TCnSize                  - 口令字节长度
     out_priv: TCnCryptoHandle*             - 输出 ECC 私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出 ECC 公钥对象标识
     out_curve_id: TInt32*                  - 输出曲线 ID

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ecc_save_keys_to_pem(TInt32 key_type_id, TInt32 curve_id, TCnCryptoHandle priv, TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 将 ECC 公私钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_ECC_KEY_PKCS1/PKCS8）
     curve_id: TInt32                       - 曲线 ID
     priv: TCnCryptoHandle                  - ECC 私钥对象标识
     pub: TCnCryptoHandle                   - ECC 公钥对象标识
     out_buf: uint8_t*                      - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ecc_save_public_key_to_pem(TInt32 key_type_id, TInt32 curve_id, TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 将 ECC 公钥以 PEM 格式导出。

   参数：
     key_type_id: TInt32                    - 密钥类型（CN_ECC_KEY_PKCS1/PKCS8）
     curve_id: TInt32                       - 曲线 ID
     pub: TCnCryptoHandle                   - ECC 公钥对象标识
     out_buf: uint8_t*                      - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_sm2_privkey_new(void);
/* 创建 SM2 私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - SM2 私钥对象标识
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_sm2_pubkey_new(void);
/* 创建 SM2 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - SM2 公钥对象标识
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_sm2_key_free(TCnCryptoHandle key);
/* 释放 SM2 密钥对象。

   参数：
     key: TCnCryptoHandle                   - SM2 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_sm2_generate_keys(TCnCryptoHandle* out_priv, TCnCryptoHandle* out_pub);
/* 生成一对 SM2 公私钥。

   参数：
     out_priv: TCnCryptoHandle*             - 输出 SM2 私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出 SM2 公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_sm2_encrypt(TInt32 seq_type_id, TBool32 include_prefix, TCnCryptoHandle pub, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 使用 SM2 公钥加密（支持 C1C3C2/C1C2C3 序列）。

   参数：
     seq_type_id: TInt32                    - 输出序列类型（CN_SM2_SEQ_*）
     include_prefix: TBool32                - 是否包含未压缩前缀
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     in_ptr: uint8_t*                       - 明文输入指针
     in_len: TCnSize                        - 明文字节长度
     out_ptr: uint8_t*                      - 密文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_sm2_decrypt(TInt32 seq_type_id, TCnCryptoHandle priv, uint8_t* in_ptr, TCnSize in_len, uint8_t* out_ptr, TCnSize cap, TCnSize* out_len);
/* 使用 SM2 私钥解密。

   参数：
     seq_type_id: TInt32                    - 输入序列类型（CN_SM2_SEQ_*）
     priv: TCnCryptoHandle                  - SM2 私钥对象标识
     in_ptr: uint8_t*                       - 密文输入指针
     in_len: TCnSize                        - 密文字节长度
     out_ptr: uint8_t*                      - 明文输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_sm2_sign(uint8_t* user_id, TCnSize user_id_len, TCnCryptoHandle priv, TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* out_sig_der, TCnSize cap, TCnSize* out_len);
/* SM2 签名（DER 编码），包含用户标识（ZA）计算。

   参数：
     user_id: uint8_t*                      - 用户标识 ID
     user_id_len: TCnSize                   - 用户标识字节长度
     priv: TCnCryptoHandle                  - SM2 私钥对象标识
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig_der: uint8_t*                  - 签名 DER 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_sm2_verify(uint8_t* user_id, TCnSize user_id_len, TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* sig_der, TCnSize sig_len);
/* SM2 验证签名（DER 编码）。

   参数：
     user_id: uint8_t*                      - 用户标识 ID
     user_id_len: TCnSize                   - 用户标识字节长度
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig_der: uint8_t*                      - 签名 DER 数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_sm2_save_keys_to_pem(TCnCryptoHandle priv, TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 将 SM2 公私钥以 PEM 格式导出。

   参数：
     priv: TCnCryptoHandle                  - SM2 私钥对象标识
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     out_buf: uint8_t*                      - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_sm2_save_public_key_to_pem(TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 将 SM2 公钥以 PEM 格式导出。

   参数：
     pub: TCnCryptoHandle                   - SM2 公钥对象标识
     out_buf: uint8_t*                      - 输出 PEM 缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_ed25519_privkey_new(void);
/* 创建 Ed25519 私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - Ed25519 私钥对象标识
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_ed25519_pubkey_new(void);
/* 创建 Ed25519 公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - Ed25519 公钥对象标识
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_key_free(TCnCryptoHandle key);
/* 释放 Ed25519 密钥对象。

   参数：
     key: TCnCryptoHandle                   - 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_generate_keys(TCnCryptoHandle* out_priv, TCnCryptoHandle* out_pub);
/* 生成一对 Ed25519 公私钥。

   参数：
     out_priv: TCnCryptoHandle*             - 输出私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_sign(TCnCryptoHandle priv, TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* out_sig, TCnSize cap, TCnSize* out_len);
/* Ed25519 签名。

   参数：
     priv: TCnCryptoHandle                  - Ed25519 私钥对象标识
     pub: TCnCryptoHandle                   - Ed25519 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig: uint8_t*                      - 签名输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_ed25519_verify(TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* sig, TCnSize sig_len);
/* Ed25519 验证签名。

   参数：
     pub: TCnCryptoHandle                   - Ed25519 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig: uint8_t*                          - 签名数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_sign_ex(uint8_t* ctx, TCnSize ctx_len, TBool32 ph_flag, TCnCryptoHandle priv, TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* out_sig, TCnSize cap, TCnSize* out_len);
/* Ed25519 扩展签名（带上下文/预杂凑选择）。

   参数：
     ctx: uint8_t*                          - 上下文字符串指针（可选）
     ctx_len: TCnSize                       - 上下文字节长度
     ph_flag: TBool32                       - 预杂凑标记（1 表示使用）
     priv: TCnCryptoHandle                  - 私钥对象标识
     pub: TCnCryptoHandle                   - 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     out_sig: uint8_t*                      - 签名输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_ed25519_verify_ex(uint8_t* ctx, TCnSize ctx_len, TBool32 ph_flag, TCnCryptoHandle pub, uint8_t* data, TCnSize len, uint8_t* sig, TCnSize sig_len);
/* Ed25519 扩展验签（带上下文/预杂凑）。

   参数：
     ctx: uint8_t*                          - 上下文字符串指针（可选）
     ctx_len: TCnSize                       - 上下文字节长度
     ph_flag: TBool32                       - 预杂凑标记（1 表示使用）
     pub: TCnCryptoHandle                   - 公钥对象标识
     data: uint8_t*                         - 原文数据指针
     len: TCnSize                           - 原文字节长度
     sig: uint8_t*                          - 签名数据指针
     sig_len: TCnSize                       - 签名字节长度

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_curve25519_privkey_new(void);
/* 创建 Curve25519（X25519）私钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - 私钥对象标识
*/

CNCRYPTO_API TCnCryptoHandle CNCRYPTO_CALL cn_curve25519_pubkey_new(void);
/* 创建 Curve25519（X25519）公钥对象。

   参数：
     （无）

   返回值：TCnCryptoHandle                  - 公钥对象标识
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_key_free(TCnCryptoHandle key);
/* 释放 Curve25519 密钥对象。

   参数：
     key: TCnCryptoHandle                   - 密钥对象标识（公钥或私钥）

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_generate_keys(TCnCryptoHandle* out_priv, TCnCryptoHandle* out_pub);
/* 生成一对 Curve25519 公私钥。

   参数：
     out_priv: TCnCryptoHandle*             - 输出私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_dh_step1(TCnCryptoHandle self_priv, uint8_t* out_point_bytes, TCnSize cap, TCnSize* out_len);
/* X25519 DH 第一步：由私钥导出公钥点（字节）。

   参数：
     self_priv: TCnCryptoHandle             - 自己的私钥对象标识
     out_point_bytes: uint8_t*              - 输出公钥点缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_dh_step2(TCnCryptoHandle self_priv, uint8_t* peer_point_bytes, TCnSize peer_len, uint8_t* out_shared_bytes, TCnSize cap, TCnSize* out_len);
/* X25519 DH 第二步：与对方公钥点计算共享密钥。

   参数：
     self_priv: TCnCryptoHandle             - 自己的私钥对象标识
     peer_point_bytes: uint8_t*             - 对方公钥点字节
     peer_len: TCnSize                      - 公钥点字节长度
     out_shared_bytes: uint8_t*             - 输出共享密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_dh(TCnCryptoHandle self_priv, uint8_t* peer_point_bytes, TCnSize peer_len, uint8_t* out_shared_bytes, TCnSize cap, TCnSize* out_len);
/* X25519 DH 一步完成：计算共享密钥。

   参数：
     self_priv: TCnCryptoHandle             - 自己的私钥对象标识
     peer_point_bytes: uint8_t*             - 对方公钥点字节
     peer_len: TCnSize                      - 公钥点字节长度
     out_shared_bytes: uint8_t*             - 输出共享密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_dh_bytes(uint8_t* self_priv_bytes, TCnSize self_len, uint8_t* peer_point_bytes, TCnSize peer_len, uint8_t* out_shared_bytes, TCnSize cap, TCnSize* out_len);
/* X25519 DH（字节版）：由私钥字节与对方公钥点字节计算共享密钥。

   参数：
     self_priv_bytes: uint8_t*              - 自己私钥字节
     self_len: TCnSize                      - 私钥字节长度
     peer_point_bytes: uint8_t*             - 对方公钥点字节
     peer_len: TCnSize                      - 公钥点字节长度
     out_shared_bytes: uint8_t*             - 输出共享密钥缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_privkey_from_bytes(uint8_t* data, TCnSize len, TCnCryptoHandle* out_priv);
/* 由字节导入 Ed25519 私钥。

   参数：
     data: uint8_t*                         - 私钥字节
     len: TCnSize                           - 私钥字节长度
     out_priv: TCnCryptoHandle*             - 输出私钥对象标识

   返回值：TCnResult                       - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_privkey_to_bytes(TCnCryptoHandle priv, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 导出 Ed25519 私钥为字节。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_pubkey_from_bytes(uint8_t* data, TCnSize len, TCnCryptoHandle* out_pub);
/* 由字节导入 Ed25519 公钥。

   参数：
     data: uint8_t*                         - 公钥字节
     len: TCnSize                           - 公钥字节长度
     out_pub: TCnCryptoHandle*              - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_pubkey_to_bytes(TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 导出 Ed25519 公钥为字节。

   参数：
     pub: TCnCryptoHandle                   - 公钥对象标识
     out_buf: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_privkey_from_bytes(uint8_t* data, TCnSize len, TCnCryptoHandle* out_priv);
/* 由字节导入 Curve25519 私钥。

   参数：
     data: uint8_t*                         - 私钥字节
     len: TCnSize                           - 私钥字节长度
     out_priv: TCnCryptoHandle*             - 输出私钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_privkey_to_bytes(TCnCryptoHandle priv, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 导出 Curve25519 私钥为字节。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_pubkey_from_bytes(uint8_t* data, TCnSize len, TCnCryptoHandle* out_pub);
/* 由字节导入 Curve25519 公钥。

   参数：
     data: uint8_t*                         - 公钥字节
     len: TCnSize                           - 公钥字节长度
     out_pub: TCnCryptoHandle*              - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_pubkey_to_bytes(TCnCryptoHandle pub, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 导出 Curve25519 公钥为字节。

   参数：
     pub: TCnCryptoHandle                   - 公钥对象标识
     out_buf: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_derive_public(TCnCryptoHandle priv, TCnCryptoHandle* out_pub);
/* 由 Ed25519 私钥派生对应公钥（对象）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_ed25519_derive_public_to_bytes(TCnCryptoHandle priv, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 由 Ed25519 私钥派生对应公钥（字节）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_derive_public(TCnCryptoHandle priv, TCnCryptoHandle* out_pub);
/* 由 Curve25519 私钥派生对应公钥（对象）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_pub: TCnCryptoHandle*              - 输出公钥对象标识

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_curve25519_derive_public_to_bytes(TCnCryptoHandle priv, uint8_t* out_buf, TCnSize cap, TCnSize* out_len);
/* 由 Curve25519 私钥派生对应公钥（字节）。

   参数：
     priv: TCnCryptoHandle                  - 私钥对象标识
     out_buf: uint8_t*                      - 输出缓冲区
     cap: TCnSize                           - 输出缓冲区容量，单位字节
     out_len: TCnSize*                      - 实际输出字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mlkem_generate_keys(TInt32 type_id, uint8_t* rand_d_hex, TCnSize rand_d_len, uint8_t* rand_z_hex, TCnSize rand_z_len, uint8_t* out_encap_key, TCnSize encap_cap, TCnSize* out_encap_len, uint8_t* out_decap_key, TCnSize decap_cap, TCnSize* out_decap_len);
/* ML-KEM 生成密钥对（后量子密钥封装机制）。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     rand_d_hex: uint8_t*                   - D 随机源（十六进制 ASCII）
     rand_d_len: TCnSize                    - D 随机源字节长度
     rand_z_hex: uint8_t*                   - Z 随机源（十六进制 ASCII）
     rand_z_len: TCnSize                    - Z 随机源字节长度
     out_encap_key: uint8_t*                - 输出封装公钥
     encap_cap: TCnSize                     - 封装公钥缓冲区容量
     out_encap_len: TCnSize*                - 实际封装公钥长度
     out_decap_key: uint8_t*                - 输出解封私钥
     decap_cap: TCnSize                     - 解封私钥缓冲区容量
     out_decap_len: TCnSize*                - 实际解封私钥长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mlkem_encaps(TInt32 type_id, uint8_t* encap_key, TCnSize encap_len, uint8_t* msg, TCnSize msg_len, uint8_t* out_share_key, TCnSize share_cap, TCnSize* out_share_len, uint8_t* out_cipher, TCnSize cipher_cap, TCnSize* out_cipher_len);
/* ML-KEM 封装共享密钥。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     encap_key: uint8_t*                    - 封装公钥
     encap_len: TCnSize                     - 公钥字节长度
     msg: uint8_t*                          - 可选消息（用于确定性封装）
     msg_len: TCnSize                       - 消息字节长度
     out_share_key: uint8_t*                - 输出共享密钥
     share_cap: TCnSize                     - 共享密钥缓冲区容量
     out_share_len: TCnSize*                - 实际共享密钥长度
     out_cipher: uint8_t*                   - 输出密文
     cipher_cap: TCnSize                    - 密文缓冲区容量
     out_cipher_len: TCnSize*               - 实际密文长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mlkem_decaps(TInt32 type_id, uint8_t* decap_key, TCnSize decap_len, uint8_t* cipher, TCnSize cipher_len, uint8_t* out_share_key, TCnSize share_cap, TCnSize* out_share_len);
/* ML-KEM 解封共享密钥。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     decap_key: uint8_t*                    - 解封私钥
     decap_len: TCnSize                     - 私钥字节长度
     cipher: uint8_t*                       - 输入密文
     cipher_len: TCnSize                    - 密文字节长度
     out_share_key: uint8_t*                - 输出共享密钥
     share_cap: TCnSize                     - 共享密钥缓冲区容量
     out_share_len: TCnSize*                - 实际共享密钥长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mldsa_generate_keys(TInt32 type_id, uint8_t* rand_hex, TCnSize rand_len, uint8_t* out_priv, TCnSize priv_cap, TCnSize* out_priv_len, uint8_t* out_pub, TCnSize pub_cap, TCnSize* out_pub_len);
/* ML-DSA 生成签名密钥对（后量子）。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）
     rand_hex: uint8_t*                     - 随机源（十六进制 ASCII）
     rand_len: TCnSize                      - 随机源字节长度
     out_priv: uint8_t*                     - 输出私钥字节
     priv_cap: TCnSize                      - 私钥缓冲区容量
     out_priv_len: TCnSize*                 - 实际私钥长度
     out_pub: uint8_t*                      - 输出公钥字节
     pub_cap: TCnSize                       - 公钥缓冲区容量
     out_pub_len: TCnSize*                  - 实际公钥长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mldsa_sign(TInt32 type_id, uint8_t* sk, TCnSize sk_len, uint8_t* msg, TCnSize msg_len, uint8_t* ctx, TCnSize ctx_len, TInt32 hash_id, uint8_t* rand_hex, TCnSize rand_len, uint8_t* out_sig, TCnSize sig_cap, TCnSize* out_sig_len);
/* ML-DSA 生成签名。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）
     sk: uint8_t*                           - 私钥字节
     sk_len: TCnSize                        - 私钥字节长度
     msg: uint8_t*                          - 消息指针
     msg_len: TCnSize                       - 消息字节长度
     ctx: uint8_t*                          - 可选上下文
     ctx_len: TCnSize                       - 上下文字节长度
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*，可选）
     rand_hex: uint8_t*                     - 随机源（十六进制 ASCII）
     rand_len: TCnSize                      - 随机源字节长度
     out_sig: uint8_t*                      - 输出签名字节
     sig_cap: TCnSize                       - 签名缓冲区容量
     out_sig_len: TCnSize*                  - 实际签名长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TBool32   CNCRYPTO_CALL cn_mldsa_verify(TInt32 type_id, uint8_t* pk, TCnSize pk_len, uint8_t* msg, TCnSize msg_len, uint8_t* sig, TCnSize sig_len, uint8_t* ctx, TCnSize ctx_len, TInt32 hash_id);
/* ML-DSA 验证签名。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）
     pk: uint8_t*                           - 公钥字节
     pk_len: TCnSize                        - 公钥字节长度
     msg: uint8_t*                          - 消息指针
     msg_len: TCnSize                       - 消息字节长度
     sig: uint8_t*                          - 签名字节
     sig_len: TCnSize                       - 签名字节长度
     ctx: uint8_t*                          - 可选上下文
     ctx_len: TCnSize                       - 上下文字节长度
     hash_id: TInt32                        - 杂凑算法 ID（CN_HASH_*，可选）

   返回值：TBool32                          - 1 表示验证通过，0 表示失败
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mlkem_check_encap_key(TInt32 type_id, uint8_t* encap_key, TCnSize encap_len);
/* 校验 ML-KEM 封装公钥格式与合法性。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     encap_key: uint8_t*                    - 封装公钥
     encap_len: TCnSize                     - 公钥字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示合法
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mlkem_check_decap_key(TInt32 type_id, uint8_t* decap_key, TCnSize decap_len);
/* 校验 ML-KEM 解封私钥格式与合法性。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     decap_key: uint8_t*                    - 解封私钥
     decap_len: TCnSize                     - 私钥字节长度

   返回值：TCnResult                        - 错误码，CN_OK 表示合法
*/

CNCRYPTO_API TCnResult CNCRYPTO_CALL cn_mlkem_encaps_auto(TInt32 type_id, uint8_t* encap_key, TCnSize encap_len, uint8_t* out_share_key, TCnSize share_cap, TCnSize* out_share_len, uint8_t* out_cipher, TCnSize cipher_cap, TCnSize* out_cipher_len);
/* ML-KEM 自动封装共享密钥（不带消息）。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）
     encap_key: uint8_t*                    - 封装公钥
     encap_len: TCnSize                     - 公钥字节长度
     out_share_key: uint8_t*                - 输出共享密钥
     share_cap: TCnSize                     - 共享密钥缓冲区容量
     out_share_len: TCnSize*                - 实际共享密钥长度
     out_cipher: uint8_t*                   - 输出密文
     cipher_cap: TCnSize                    - 密文缓冲区容量
     out_cipher_len: TCnSize*               - 实际密文长度

   返回值：TCnResult                        - 错误码，CN_OK 表示成功
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_mlkem_expected_encap_key_len(TInt32 type_id);
/* 查询 ML-KEM 封装公钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_mlkem_expected_decap_key_len(TInt32 type_id);
/* 查询 ML-KEM 解封私钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_mlkem_expected_cipher_len(TInt32 type_id);
/* 查询 ML-KEM 密文的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLKEM_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_mlkem_share_key_bytes(void);
/* 查询 ML-KEM 共享密钥的字节长度。

   参数：
     （无）

   返回值：TCnSize                          - 字节长度
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_mldsa_expected_privkey_len(TInt32 type_id);
/* 查询 ML-DSA 私钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_mldsa_expected_pubkey_len(TInt32 type_id);
/* 查询 ML-DSA 公钥的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
*/

CNCRYPTO_API TCnSize   CNCRYPTO_CALL cn_mldsa_expected_sig_len(TInt32 type_id);
/* 查询 ML-DSA 签名的期望长度。

   参数：
     type_id: TInt32                        - 类型（CN_MLDSA_TYPE_*）

   返回值：TCnSize                          - 期望字节长度
*/


#if defined(__cplusplus)
}
#endif

#endif /* CNCRYPTO_INTF_H*/

