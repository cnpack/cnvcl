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
* 单元名称：密码算法库 DLL/SO 对外输出声明单元
* 单元作者：CnPack 开发组
* 备    注：虽然 CnPack 密码算法库可以在 Pascal 编译器中直接编译源码，但对于其他
*           语言，使用起来不方便。我们将其封装成对外输出的 DLL/SO 函数，可编译
*           成 DLL/SO 后，运行期动态加载，供其他语言运行期调用。
*           本单元是生成的 CnCrypto.dll/so/dynlib 的调用声明。
* 开发平台：PWin7 + Delphi 7
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.12.26 V1.0
*                创建单元，实现声明与封装
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

const
  CnCryptoDLL = 'CnCrypto.dll';

type
  TUInt8          = Byte;

  TUInt32         = Cardinal;

{$IFDEF VER130}
  TUInt64         = Int64;
{$ELSE}
  TUInt64         = UInt64;
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

function cn_get_version(var out_major, out_minor, out_patch: TUInt32): TCnResult; cdecl; external CnCryptoDLL name 'cn_get_version';

function cn_get_abi_version: TUInt32; cdecl; external CnCryptoDLL name 'cn_get_abi_version';

function cn_lib_init: TCnResult; cdecl; external CnCryptoDLL name 'cn_lib_init';

function cn_lib_finalize: TCnResult; cdecl; external CnCryptoDLL name 'cn_lib_finalize';

function cn_alloc(size: TCnSize): TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_alloc';

function cn_free(ptr: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_free';

function cn_memzero(ptr: TCnCryptoHandle; size: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_memzero';

function cn_endian_is_le: TBool32; cdecl; external CnCryptoDLL name 'cn_endian_is_le';

function cn_endian_is_be: TBool32; cdecl; external CnCryptoDLL name 'cn_endian_is_be';

function cn_data_to_hex(in_ptr: Pointer; in_len: TCnSize; out_hex: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_data_to_hex';

function cn_const_time_equal(a: Pointer; b: Pointer; len: TCnSize): TBool32; cdecl; external CnCryptoDLL name 'cn_const_time_equal';

function cn_const_time_select(flag: TBool32; a: Pointer; b: Pointer; len:
  TCnSize; out_ptr: Pointer): TCnResult; cdecl; external CnCryptoDLL name 'cn_const_time_select';

function cn_str_to_uint64(ascii_ptr: PByte; len: TCnSize; var out_value: TUInt64):
  TCnResult; cdecl; external CnCryptoDLL name 'cn_str_to_uint64';

function cn_base64_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_base64_encode';

function cn_base64_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_base64_decode';

function cn_base64url_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_base64url_encode';

function cn_base64url_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_base64url_decode';

function cn_otp_hotp(seed: PByte; seed_len: TCnSize; counter: TUInt64; digits:
  TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_otp_hotp';

function cn_otp_totp(hash_id: TInt32; seed: PByte; seed_len: TCnSize; period_sec:
  TInt32; digits: TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_otp_totp';

function cn_hash_digest(alg_id: TInt32; data: PByte; len: TCnSize; out_digest:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_hash_digest';

function cn_hmac(alg_id: TInt32; key: PByte; key_len: TCnSize; data: PByte; len:
  TCnSize; out_mac: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_hmac';

function cn_kdf_pbkdf2(hash_id: TInt32; password: PByte; pwd_len: TCnSize; salt:
  PByte; salt_len: TCnSize; count: TInt32; out_key: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_kdf_pbkdf2';

function cn_kdf_hkdf(hash_id: TInt32; ikm: PByte; ikm_len: TCnSize; salt: PByte;
  salt_len: TCnSize; info: PByte; info_len: TCnSize; dk_len: TCnSize; out_key:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_kdf_hkdf';

function cn_cipher_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_cipher_encrypt';

function cn_cipher_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_cipher_decrypt';

function cn_aead_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_ptr: PByte; in_len:
  TCnSize; out_cipher: PByte; cap_cipher: TCnSize; var out_cipher_len: TCnSize;
  out_tag: PByte; tag_cap: TCnSize; var out_tag_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_aead_encrypt';

function cn_aead_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_cipher: PByte;
  in_len: TCnSize; in_tag: PByte; tag_len: TCnSize; out_plain: PByte; cap_plain:
  TCnSize; var out_plain_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_aead_decrypt';

function cn_rsa_privkey_new(use_crt: TBool32): TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_rsa_privkey_new';

function cn_rsa_pubkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_rsa_pubkey_new';

function cn_rsa_key_free(key: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_key_free';

function cn_rsa_generate_keys(modulus_bits: TInt32; use_crt: TBool32; var
  out_priv: TCnCryptoHandle; var out_pub: TCnCryptoHandle; use3: TBool32):
  TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_generate_keys';

function cn_rsa_pubkey_get_modulus_bytes(pub: TCnCryptoHandle): TCnSize; cdecl; external CnCryptoDLL name 'cn_rsa_pubkey_get_modulus_bytes';

function cn_rsa_privkey_get_modulus_bytes(priv: TCnCryptoHandle): TCnSize; cdecl; external CnCryptoDLL name 'cn_rsa_privkey_get_modulus_bytes';

function cn_rsa_encrypt_with_public(padding: TInt32; pub: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_encrypt_with_public';

function cn_rsa_encrypt_with_private(priv: TCnCryptoHandle; in_ptr: PByte;
  in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_encrypt_with_private';

function cn_rsa_decrypt_with_public(pub: TCnCryptoHandle; in_ptr: PByte; in_len:
  TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_decrypt_with_public';

function cn_rsa_decrypt_with_private(padding: TInt32; priv: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_decrypt_with_private';

function cn_rsa_sign(digest_alg_id: TInt32; priv: TCnCryptoHandle; data: PByte;
  len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_sign';

function cn_rsa_verify(digest_alg_id: TInt32; pub: TCnCryptoHandle; data: PByte;
  len: TCnSize; sig_ptr: PByte; sig_len: TCnSize): TBool32; cdecl; external CnCryptoDLL name 'cn_rsa_verify';

function cn_rsa_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_load_keys_from_pem';

function cn_rsa_save_keys_to_pem(key_type_id: TInt32; priv: TCnCryptoHandle; pub:
  TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_save_keys_to_pem';

function cn_rsa_save_public_key_to_pem(key_type_id: TInt32; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_rsa_save_public_key_to_pem';

function cn_ecc_privkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_ecc_privkey_new';

function cn_ecc_pubkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_ecc_pubkey_new';

function cn_ecc_key_free(key: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_ecc_key_free';

function cn_ecc_curve_bytes(curve_id: TInt32): TCnSize; cdecl; external CnCryptoDLL name 'cn_ecc_curve_bytes';

function cn_ecc_generate_keys(curve_id: TInt32; var out_priv: TCnCryptoHandle;
  var out_pub: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_ecc_generate_keys';

function cn_ecc_sign(digest_alg_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_ecc_sign';

function cn_ecc_verify(digest_alg_id: TInt32; curve_id: TInt32; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl; external CnCryptoDLL name 'cn_ecc_verify';

function cn_ecc_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle; var out_curve_id: TInt32): TCnResult; cdecl; external CnCryptoDLL name 'cn_ecc_load_keys_from_pem';

function cn_ecc_save_keys_to_pem(key_type_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_ecc_save_keys_to_pem';

function cn_ecc_save_public_key_to_pem(key_type_id: TInt32; curve_id: TInt32;
  pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl; external CnCryptoDLL name 'cn_ecc_save_public_key_to_pem';

function cn_sm2_privkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_sm2_privkey_new';

function cn_sm2_pubkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_sm2_pubkey_new';

function cn_sm2_key_free(key: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_sm2_key_free';

function cn_sm2_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_sm2_generate_keys';

function cn_sm2_encrypt(seq_type_id: TInt32; include_prefix: TBool32; pub:
  TCnCryptoHandle; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_sm2_encrypt';

function cn_sm2_decrypt(seq_type_id: TInt32; priv: TCnCryptoHandle; in_ptr:
  PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl; external CnCryptoDLL name 'cn_sm2_decrypt';

function cn_sm2_sign(user_id: PByte; user_id_len: TCnSize; priv: TCnCryptoHandle;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_sm2_sign';

function cn_sm2_verify(user_id: PByte; user_id_len: TCnSize; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl; external CnCryptoDLL name 'cn_sm2_verify';

function cn_sm2_save_keys_to_pem(priv: TCnCryptoHandle; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_sm2_save_keys_to_pem';

function cn_sm2_save_public_key_to_pem(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_sm2_save_public_key_to_pem';

function cn_ed25519_privkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_ed25519_privkey_new';

function cn_ed25519_pubkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_ed25519_pubkey_new';

function cn_ed25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_key_free';

function cn_ed25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_generate_keys';

function cn_ed25519_sign(priv: TCnCryptoHandle; pub: TCnCryptoHandle; data:
  PByte; len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_sign';

function cn_ed25519_verify(pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig:
  PByte; sig_len: TCnSize): TBool32; cdecl; external CnCryptoDLL name 'cn_ed25519_verify';

function cn_ed25519_sign_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_sign_ex';

function cn_ed25519_verify_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig: PByte; sig_len: TCnSize):
  TBool32; cdecl; external CnCryptoDLL name 'cn_ed25519_verify_ex';

function cn_curve25519_privkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_curve25519_privkey_new';

function cn_curve25519_pubkey_new: TCnCryptoHandle; cdecl; external CnCryptoDLL name 'cn_curve25519_pubkey_new';

function cn_curve25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_key_free';

function cn_curve25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_generate_keys';

function cn_curve25519_dh_step1(self_priv: TCnCryptoHandle; out_point_bytes:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_dh_step1';

function cn_curve25519_dh_step2(self_priv: TCnCryptoHandle; peer_point_bytes:
  PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_dh_step2';

function cn_curve25519_dh(self_priv: TCnCryptoHandle; peer_point_bytes: PByte;
  peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_dh';

function cn_curve25519_dh_bytes(self_priv_bytes: PByte; self_len: TCnSize;
  peer_point_bytes: PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_dh_bytes';

function cn_ed25519_privkey_from_bytes(data: PByte; len: TCnSize; var out_priv:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_privkey_from_bytes';

function cn_ed25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_privkey_to_bytes';

function cn_ed25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_pubkey_from_bytes';

function cn_ed25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_pubkey_to_bytes';

function cn_curve25519_privkey_from_bytes(data: PByte; len: TCnSize; var
  out_priv: TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_privkey_from_bytes';

function cn_curve25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_privkey_to_bytes';

function cn_curve25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_pubkey_from_bytes';

function cn_curve25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_pubkey_to_bytes';

function cn_ed25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_derive_public';

function cn_ed25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_ed25519_derive_public_to_bytes';

function cn_curve25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_derive_public';

function cn_curve25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_curve25519_derive_public_to_bytes';

function cn_mlkem_generate_keys(type_id: TInt32; rand_d_hex: PByte; rand_d_len:
  TCnSize; rand_z_hex: PByte; rand_z_len: TCnSize; out_encap_key: PByte;
  encap_cap: TCnSize; var out_encap_len: TCnSize; out_decap_key: PByte;
  decap_cap: TCnSize; var out_decap_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mlkem_generate_keys';

function cn_mlkem_encaps(type_id: TInt32; encap_key: PByte; encap_len: TCnSize;
  msg: PByte; msg_len: TCnSize; out_share_key: PByte; share_cap: TCnSize; var
  out_share_len: TCnSize; out_cipher: PByte; cipher_cap: TCnSize; var
  out_cipher_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mlkem_encaps';

function cn_mlkem_decaps(type_id: TInt32; decap_key: PByte; decap_len: TCnSize;
  cipher: PByte; cipher_len: TCnSize; out_share_key: PByte; share_cap: TCnSize;
  var out_share_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mlkem_decaps';

function cn_mldsa_generate_keys(type_id: TInt32; rand_hex: PByte; rand_len:
  TCnSize; out_priv: PByte; priv_cap: TCnSize; var out_priv_len: TCnSize;
  out_pub: PByte; pub_cap: TCnSize; var out_pub_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mldsa_generate_keys';

function cn_mldsa_sign(type_id: TInt32; sk: PByte; sk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; ctx: PByte; ctx_len: TCnSize; hash_id: TInt32; rand_hex:
  PByte; rand_len: TCnSize; out_sig: PByte; sig_cap: TCnSize; var out_sig_len:
  TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mldsa_sign';

function cn_mldsa_verify(type_id: TInt32; pk: PByte; pk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; sig: PByte; sig_len: TCnSize; ctx: PByte; ctx_len: TCnSize;
  hash_id: TInt32): TBool32; cdecl; external CnCryptoDLL name 'cn_mldsa_verify';

function cn_mlkem_check_encap_key(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mlkem_check_encap_key';

function cn_mlkem_check_decap_key(type_id: TInt32; decap_key: PByte; decap_len:
  TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mlkem_check_decap_key';

function cn_mlkem_encaps_auto(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize; out_share_key: PByte; share_cap: TCnSize; var out_share_len: TCnSize;
  out_cipher: PByte; cipher_cap: TCnSize; var out_cipher_len: TCnSize): TCnResult; cdecl; external CnCryptoDLL name 'cn_mlkem_encaps_auto';

function cn_mlkem_expected_encap_key_len(type_id: TInt32): TCnSize; cdecl; external CnCryptoDLL name 'cn_mlkem_expected_encap_key_len';

function cn_mlkem_expected_decap_key_len(type_id: TInt32): TCnSize; cdecl; external CnCryptoDLL name 'cn_mlkem_expected_decap_key_len';

function cn_mlkem_expected_cipher_len(type_id: TInt32): TCnSize; cdecl; external CnCryptoDLL name 'cn_mlkem_expected_cipher_len';

function cn_mlkem_share_key_bytes: TCnSize; cdecl; external CnCryptoDLL name 'cn_mlkem_share_key_bytes';

function cn_mldsa_expected_privkey_len(type_id: TInt32): TCnSize; cdecl; external CnCryptoDLL name 'cn_mldsa_expected_privkey_len';

function cn_mldsa_expected_pubkey_len(type_id: TInt32): TCnSize; cdecl; external CnCryptoDLL name 'cn_mldsa_expected_pubkey_len';

function cn_mldsa_expected_sig_len(type_id: TInt32): TCnSize; cdecl; external CnCryptoDLL name 'cn_mldsa_expected_sig_len';


implementation

end.

