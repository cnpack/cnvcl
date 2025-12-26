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

unit CnCryptoExport;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：密码算法库 DLL/SO 对外输出封装单元
* 单元作者：CnPack 开发组
* 备    注：虽然 CnPack 密码算法库可以在 Pascal 编译器中直接编译源码，但对于其他
*           语言，使用起来不方便。本单元将其封装成对外输出的 DLL/SO 函数，可编译
*           成 DLL/SO 后，运行期动态加载，供其他语言运行期调用。
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

function cn_get_version(var out_major, out_minor, out_patch: TUInt32): TCnResult; cdecl;

function cn_get_abi_version: TUInt32; cdecl;

function cn_lib_init: TCnResult; cdecl;

function cn_lib_finalize: TCnResult; cdecl;

function cn_alloc(size: TCnSize): TCnCryptoHandle; cdecl;

function cn_free(ptr: TCnCryptoHandle): TCnResult; cdecl;

function cn_memzero(ptr: TCnCryptoHandle; size: TCnSize): TCnResult; cdecl;

function cn_endian_is_le: TBool32; cdecl;

function cn_endian_is_be: TBool32; cdecl;

function cn_data_to_hex(in_ptr: Pointer; in_len: TCnSize; out_hex: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_const_time_equal(a: Pointer; b: Pointer; len: TCnSize): TBool32; cdecl;

function cn_const_time_select(flag: TBool32; a: Pointer; b: Pointer; len:
  TCnSize; out_ptr: Pointer): TCnResult; cdecl;

function cn_str_to_uint64(ascii_ptr: PByte; len: TCnSize; var out_value: TUInt64):
  TCnResult; cdecl;

function cn_base64_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_base64_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_base64url_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_base64url_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_otp_hotp(seed: PByte; seed_len: TCnSize; counter: TUInt64; digits:
  TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_otp_totp(hash_id: TInt32; seed: PByte; seed_len: TCnSize; period_sec:
  TInt32; digits: TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;

function cn_hash_digest(alg_id: TInt32; data: PByte; len: TCnSize; out_digest:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_hmac(alg_id: TInt32; key: PByte; key_len: TCnSize; data: PByte; len:
  TCnSize; out_mac: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_kdf_pbkdf2(hash_id: TInt32; password: PByte; pwd_len: TCnSize; salt:
  PByte; salt_len: TCnSize; count: TInt32; out_key: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl;

function cn_kdf_hkdf(hash_id: TInt32; ikm: PByte; ikm_len: TCnSize; salt: PByte;
  salt_len: TCnSize; info: PByte; info_len: TCnSize; dk_len: TCnSize; out_key:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_cipher_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_cipher_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_aead_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_ptr: PByte; in_len:
  TCnSize; out_cipher: PByte; cap_cipher: TCnSize; var out_cipher_len: TCnSize;
  out_tag: PByte; tag_cap: TCnSize; var out_tag_len: TCnSize): TCnResult; cdecl;

function cn_aead_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_cipher: PByte;
  in_len: TCnSize; in_tag: PByte; tag_len: TCnSize; out_plain: PByte; cap_plain:
  TCnSize; var out_plain_len: TCnSize): TCnResult; cdecl;

function cn_rsa_privkey_new(use_crt: TBool32): TCnCryptoHandle; cdecl;

function cn_rsa_pubkey_new: TCnCryptoHandle; cdecl;

function cn_rsa_key_free(key: TCnCryptoHandle): TCnResult; cdecl;

function cn_rsa_generate_keys(modulus_bits: TInt32; use_crt: TBool32; var
  out_priv: TCnCryptoHandle; var out_pub: TCnCryptoHandle; use3: TBool32):
  TCnResult; cdecl;

function cn_rsa_pubkey_get_modulus_bytes(pub: TCnCryptoHandle): TCnSize; cdecl;

function cn_rsa_privkey_get_modulus_bytes(priv: TCnCryptoHandle): TCnSize; cdecl;

function cn_rsa_encrypt_with_public(padding: TInt32; pub: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;

function cn_rsa_encrypt_with_private(priv: TCnCryptoHandle; in_ptr: PByte;
  in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_rsa_decrypt_with_public(pub: TCnCryptoHandle; in_ptr: PByte; in_len:
  TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_rsa_decrypt_with_private(padding: TInt32; priv: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;

function cn_rsa_sign(digest_alg_id: TInt32; priv: TCnCryptoHandle; data: PByte;
  len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_rsa_verify(digest_alg_id: TInt32; pub: TCnCryptoHandle; data: PByte;
  len: TCnSize; sig_ptr: PByte; sig_len: TCnSize): TBool32; cdecl;

function cn_rsa_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle): TCnResult; cdecl;

function cn_rsa_save_keys_to_pem(key_type_id: TInt32; priv: TCnCryptoHandle; pub:
  TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_rsa_save_public_key_to_pem(key_type_id: TInt32; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_ecc_privkey_new: TCnCryptoHandle; cdecl;

function cn_ecc_pubkey_new: TCnCryptoHandle; cdecl;

function cn_ecc_key_free(key: TCnCryptoHandle): TCnResult; cdecl;

function cn_ecc_curve_bytes(curve_id: TInt32): TCnSize; cdecl;

function cn_ecc_generate_keys(curve_id: TInt32; var out_priv: TCnCryptoHandle;
  var out_pub: TCnCryptoHandle): TCnResult; cdecl;

function cn_ecc_sign(digest_alg_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl;

function cn_ecc_verify(digest_alg_id: TInt32; curve_id: TInt32; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl;

function cn_ecc_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle; var out_curve_id: TInt32): TCnResult; cdecl;

function cn_ecc_save_keys_to_pem(key_type_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl;

function cn_ecc_save_public_key_to_pem(key_type_id: TInt32; curve_id: TInt32;
  pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;

function cn_sm2_privkey_new: TCnCryptoHandle; cdecl;

function cn_sm2_pubkey_new: TCnCryptoHandle; cdecl;

function cn_sm2_key_free(key: TCnCryptoHandle): TCnResult; cdecl;

function cn_sm2_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_sm2_encrypt(seq_type_id: TInt32; include_prefix: TBool32; pub:
  TCnCryptoHandle; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl;

function cn_sm2_decrypt(seq_type_id: TInt32; priv: TCnCryptoHandle; in_ptr:
  PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;

function cn_sm2_sign(user_id: PByte; user_id_len: TCnSize; priv: TCnCryptoHandle;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_sm2_verify(user_id: PByte; user_id_len: TCnSize; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl;

function cn_sm2_save_keys_to_pem(priv: TCnCryptoHandle; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_sm2_save_public_key_to_pem(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_ed25519_privkey_new: TCnCryptoHandle; cdecl;

function cn_ed25519_pubkey_new: TCnCryptoHandle; cdecl;

function cn_ed25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl;

function cn_ed25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_ed25519_sign(priv: TCnCryptoHandle; pub: TCnCryptoHandle; data:
  PByte; len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;

function cn_ed25519_verify(pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig:
  PByte; sig_len: TCnSize): TBool32; cdecl;

function cn_ed25519_sign_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_ed25519_verify_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig: PByte; sig_len: TCnSize):
  TBool32; cdecl;

function cn_curve25519_privkey_new: TCnCryptoHandle; cdecl;

function cn_curve25519_pubkey_new: TCnCryptoHandle; cdecl;

function cn_curve25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl;

function cn_curve25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_curve25519_dh_step1(self_priv: TCnCryptoHandle; out_point_bytes:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_curve25519_dh_step2(self_priv: TCnCryptoHandle; peer_point_bytes:
  PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;

function cn_curve25519_dh(self_priv: TCnCryptoHandle; peer_point_bytes: PByte;
  peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;

function cn_curve25519_dh_bytes(self_priv_bytes: PByte; self_len: TCnSize;
  peer_point_bytes: PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_ed25519_privkey_from_bytes(data: PByte; len: TCnSize; var out_priv:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_ed25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_ed25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_ed25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_curve25519_privkey_from_bytes(data: PByte; len: TCnSize; var
  out_priv: TCnCryptoHandle): TCnResult; cdecl;

function cn_curve25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_curve25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_curve25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_ed25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_ed25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_curve25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;

function cn_curve25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;

function cn_mlkem_generate_keys(type_id: TInt32; rand_d_hex: PByte; rand_d_len:
  TCnSize; rand_z_hex: PByte; rand_z_len: TCnSize; out_encap_key: PByte;
  encap_cap: TCnSize; var out_encap_len: TCnSize; out_decap_key: PByte;
  decap_cap: TCnSize; var out_decap_len: TCnSize): TCnResult; cdecl;

function cn_mlkem_encaps(type_id: TInt32; encap_key: PByte; encap_len: TCnSize;
  msg: PByte; msg_len: TCnSize; out_share_key: PByte; share_cap: TCnSize; var
  out_share_len: TCnSize; out_cipher: PByte; cipher_cap: TCnSize; var
  out_cipher_len: TCnSize): TCnResult; cdecl;

function cn_mlkem_decaps(type_id: TInt32; decap_key: PByte; decap_len: TCnSize;
  cipher: PByte; cipher_len: TCnSize; out_share_key: PByte; share_cap: TCnSize;
  var out_share_len: TCnSize): TCnResult; cdecl;

function cn_mldsa_generate_keys(type_id: TInt32; rand_hex: PByte; rand_len:
  TCnSize; out_priv: PByte; priv_cap: TCnSize; var out_priv_len: TCnSize;
  out_pub: PByte; pub_cap: TCnSize; var out_pub_len: TCnSize): TCnResult; cdecl;

function cn_mldsa_sign(type_id: TInt32; sk: PByte; sk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; ctx: PByte; ctx_len: TCnSize; hash_id: TInt32; rand_hex:
  PByte; rand_len: TCnSize; out_sig: PByte; sig_cap: TCnSize; var out_sig_len:
  TCnSize): TCnResult; cdecl;

function cn_mldsa_verify(type_id: TInt32; pk: PByte; pk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; sig: PByte; sig_len: TCnSize; ctx: PByte; ctx_len: TCnSize;
  hash_id: TInt32): TBool32; cdecl;

function cn_mlkem_check_encap_key(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize): TCnResult; cdecl;

function cn_mlkem_check_decap_key(type_id: TInt32; decap_key: PByte; decap_len:
  TCnSize): TCnResult; cdecl;

function cn_mlkem_encaps_auto(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize; out_share_key: PByte; share_cap: TCnSize; var out_share_len: TCnSize;
  out_cipher: PByte; cipher_cap: TCnSize; var out_cipher_len: TCnSize): TCnResult; cdecl;

function cn_mlkem_expected_encap_key_len(type_id: TInt32): TCnSize; cdecl;

function cn_mlkem_expected_decap_key_len(type_id: TInt32): TCnSize; cdecl;

function cn_mlkem_expected_cipher_len(type_id: TInt32): TCnSize; cdecl;

function cn_mlkem_share_key_bytes: TCnSize; cdecl;

function cn_mldsa_expected_privkey_len(type_id: TInt32): TCnSize; cdecl;

function cn_mldsa_expected_pubkey_len(type_id: TInt32): TCnSize; cdecl;

function cn_mldsa_expected_sig_len(type_id: TInt32): TCnSize; cdecl;

implementation

uses
  SysUtils, Classes, CnNative, CnBase64, CnMD5, CnSHA1, CnSHA2, CnAES, CnAEAD,
  CnPoly1305, CnDES, CnSM4, CnRC4, CnZUC, CnChaCha20, CnSHA3, CnSM3, CnBLAKE2,
  CnBLAKE, CnXXH, CnRSA, CnPemUtils, CnKDF, CnOTP, CnECC, CnSM2, Cn25519,
  CnBigNumber, CnMLKEM, CnMLDSA, CnRandom;

function cn_get_version(var out_major, out_minor, out_patch: TUInt32): TCnResult; cdecl;
begin
  out_major := 0;
  out_minor := 1;
  out_patch := 7;
  Result := CN_OK;
end;

function cn_kdf_pbkdf2(hash_id: TInt32; password: PByte; pwd_len: TCnSize; salt:
  PByte; salt_len: TCnSize; count: TInt32; out_key: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl;
var
  P, S, DK: TBytes;
  KH: TCnPBKDF2KeyHash;
begin
  if ((password = nil) and (pwd_len <> 0)) or ((salt = nil) and (salt_len <> 0))
    or (count <= 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case hash_id of
    CN_HASH_SHA1:
      KH := cpdfSha1Hmac;
    CN_HASH_SHA2_256:
      KH := cpdfSha256Hmac;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(P, pwd_len);
  SetLength(S, salt_len);
  if pwd_len > 0 then
    Move(password^, P[0], pwd_len);
  if salt_len > 0 then
    Move(salt^, S[0], salt_len);
  try
    DK := CnPBKDF2Bytes(P, S, count, cap, KH);
  except
    Result := CN_E_INTERNAL;
    Exit;
  end;
  out_len := Length(DK);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if out_len > 0 then
    Move(DK[0], out_key^, out_len);
  Result := CN_OK;
end;

function cn_kdf_hkdf(hash_id: TInt32; ikm: PByte; ikm_len: TCnSize; salt: PByte;
  salt_len: TCnSize; info: PByte; info_len: TCnSize; dk_len: TCnSize; out_key:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  HK: TCnHKDFHash;
  DK: TBytes;
begin
  case hash_id of
    CN_HASH_MD5:
      HK := chkMd5;
    CN_HASH_SHA1:
      HK := chkSha1;
    CN_HASH_SHA2_256:
      HK := chkSha256;
    CN_HASH_SHA3_256:
      HK := chkSha3_256;
    CN_HASH_SM3:
      HK := chkSm3;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := dk_len;
  if cap < dk_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  try
    DK := CnHKDF(HK, ikm, ikm_len, salt, salt_len, info, info_len, dk_len);
  except
    Result := CN_E_INTERNAL;
    Exit;
  end;
  if Length(DK) <> dk_len then
  begin
    Result := CN_E_INTERNAL;
    Exit;
  end;
  if dk_len > 0 then
    Move(DK[0], out_key^, dk_len);
  Result := CN_OK;
end;

function cn_mlkem_generate_keys(type_id: TInt32; rand_d_hex: PByte; rand_d_len:
  TCnSize; rand_z_hex: PByte; rand_z_len: TCnSize; out_encap_key: PByte;
  encap_cap: TCnSize; var out_encap_len: TCnSize; out_decap_key: PByte;
  decap_cap: TCnSize; var out_decap_len: TCnSize): TCnResult; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
  EnKey, DeKey: TBytes;
  RD, RZ: AnsiString;
begin
  if (out_encap_key = nil) or (out_decap_key = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if (rand_d_hex <> nil) and (rand_d_len > 0) then
    SetString(RD, PAnsiChar(rand_d_hex), Integer(rand_d_len))
  else
    RD := '';
  if (rand_z_hex <> nil) and (rand_z_len > 0) then
    SetString(RZ, PAnsiChar(rand_z_hex), Integer(rand_z_len))
  else
    RZ := '';
  M := TCnMLKEM.Create(T);
  try
    M.GenerateKeys(EnKey, DeKey, RD, RZ);
    out_encap_len := Length(EnKey);
    out_decap_len := Length(DeKey);
    if (encap_cap < out_encap_len) or (decap_cap < out_decap_len) then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_encap_len > 0 then
      Move(EnKey[0], out_encap_key^, out_encap_len);
    if out_decap_len > 0 then
      Move(DeKey[0], out_decap_key^, out_decap_len);
    Result := CN_OK;
  finally
    M.Free;
  end;
end;

function cn_mlkem_expected_encap_key_len(type_id: TInt32): TCnSize; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
begin
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := 0;
    Exit;
  end;
  M := TCnMLKEM.Create(T);
  try
    Result := M.GetEncapKeyByteLength;
  finally
    M.Free;
  end;
end;

function cn_mlkem_expected_decap_key_len(type_id: TInt32): TCnSize; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
begin
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := 0;
    Exit;
  end;
  M := TCnMLKEM.Create(T);
  try
    Result := M.GetDecapKeyByteLength;
  finally
    M.Free;
  end;
end;

function cn_mlkem_expected_cipher_len(type_id: TInt32): TCnSize; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
begin
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := 0;
    Exit;
  end;
  M := TCnMLKEM.Create(T);
  try
    Result := M.GetCipherByteLength;
  finally
    M.Free;
  end;
end;

function cn_mlkem_share_key_bytes: TCnSize; cdecl;
begin
  Result := 32;
end;

function cn_mldsa_expected_privkey_len(type_id: TInt32): TCnSize; cdecl;
var
  D: TCnMLDSA;
  T: TCnMLDSAType;
  Rows, Cols, NoiseBits: Integer;
begin
  case type_id of
    44, CN_MLDSA_TYPE_44:
      T := cmdt44;
    65, CN_MLDSA_TYPE_65:
      T := cmdt65;
    87, CN_MLDSA_TYPE_87:
      T := cmdt87;
  else
    Result := 0;
    Exit;
  end;
  D := TCnMLDSA.Create(T);
  try
    Rows := D.MatrixRowCount;
    Cols := D.MatrixColCount;
    case D.Noise of
      2:
        NoiseBits := 2;
      4:
        NoiseBits := 3;
    else
      NoiseBits := 2;
    end;
    Result := 2 * 32 + 64 + 32 * (Rows + Cols) * (NoiseBits + 1) + 32 * Rows *
      CN_MLDSA_DROPBIT;
  finally
    D.Free;
  end;
end;

function cn_mldsa_expected_pubkey_len(type_id: TInt32): TCnSize; cdecl;
var
  D: TCnMLDSA;
  T: TCnMLDSAType;
begin
  case type_id of
    44, CN_MLDSA_TYPE_44:
      T := cmdt44;
    65, CN_MLDSA_TYPE_65:
      T := cmdt65;
    87, CN_MLDSA_TYPE_87:
      T := cmdt87;
  else
    Result := 0;
    Exit;
  end;
  D := TCnMLDSA.Create(T);
  try
    Result := 32 + 32 * D.MatrixRowCount * CN_MLDSA_PUBKEY_BIT;
  finally
    D.Free;
  end;
end;

function cn_mldsa_expected_sig_len(type_id: TInt32): TCnSize; cdecl;
var
  D: TCnMLDSA;
  T: TCnMLDSAType;
  Bits: Integer;
begin
  case type_id of
    44, CN_MLDSA_TYPE_44:
      T := cmdt44;
    65, CN_MLDSA_TYPE_65:
      T := cmdt65;
    87, CN_MLDSA_TYPE_87:
      T := cmdt87;
  else
    Result := 0;
    Exit;
  end;
  D := TCnMLDSA.Create(T);
  try
    Bits := 1 + GetUInt32BitLength(D.Gamma1 - 1);
    Result := D.Lambda div 4 + D.MatrixColCount * 32 * Bits + D.Omega + D.MatrixRowCount;
  finally
    D.Free;
  end;
end;

function cn_mlkem_encaps(type_id: TInt32; encap_key: PByte; encap_len: TCnSize;
  msg: PByte; msg_len: TCnSize; out_share_key: PByte; share_cap: TCnSize; var
  out_share_len: TCnSize; out_cipher: PByte; cipher_cap: TCnSize; var
  out_cipher_len: TCnSize): TCnResult; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
  EnKey, MsgB, ShareKey, CipherText: TBytes;
begin
  if (encap_key = nil) or (msg = nil) or (out_share_key = nil) or (out_cipher =
    nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if msg_len <> 32 then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(EnKey, encap_len);
  if encap_len > 0 then
    Move(encap_key^, EnKey[0], encap_len);
  SetLength(MsgB, msg_len);
  if msg_len > 0 then
    Move(msg^, MsgB[0], msg_len);
  M := TCnMLKEM.Create(T);
  try
    try
      M.CheckEncapKey(EnKey);
    except
      Result := CN_E_INVALID_ARG;
      Exit;
    end;
    M.MLKEMEncaps(EnKey, MsgB, ShareKey, CipherText);
    out_share_len := Length(ShareKey);
    out_cipher_len := Length(CipherText);
    if (share_cap < out_share_len) or (cipher_cap < out_cipher_len) then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_share_len > 0 then
      Move(ShareKey[0], out_share_key^, out_share_len);
    if out_cipher_len > 0 then
      Move(CipherText[0], out_cipher^, out_cipher_len);
    Result := CN_OK;
  finally
    M.Free;
  end;
end;

function cn_mlkem_decaps(type_id: TInt32; decap_key: PByte; decap_len: TCnSize;
  cipher: PByte; cipher_len: TCnSize; out_share_key: PByte; share_cap: TCnSize;
  var out_share_len: TCnSize): TCnResult; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
  DeKey, CipherText, ShareKey: TBytes;
begin
  if (decap_key = nil) or (cipher = nil) or (out_share_key = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(DeKey, decap_len);
  if decap_len > 0 then
    Move(decap_key^, DeKey[0], decap_len);
  SetLength(CipherText, cipher_len);
  if cipher_len > 0 then
    Move(cipher^, CipherText[0], cipher_len);
  M := TCnMLKEM.Create(T);
  try
    try
      M.CheckDecapKey(DeKey);
    except
      Result := CN_E_INVALID_ARG;
      Exit;
    end;
    ShareKey := M.MLKEMDecaps(DeKey, CipherText);
    out_share_len := Length(ShareKey);
    if share_cap < out_share_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_share_len > 0 then
      Move(ShareKey[0], out_share_key^, out_share_len);
    Result := CN_OK;
  finally
    M.Free;
  end;
end;

function cn_mldsa_generate_keys(type_id: TInt32; rand_hex: PByte; rand_len:
  TCnSize; out_priv: PByte; priv_cap: TCnSize; var out_priv_len: TCnSize;
  out_pub: PByte; pub_cap: TCnSize; var out_pub_len: TCnSize): TCnResult; cdecl;
var
  D: TCnMLDSA;
  T: TCnMLDSAType;
  SK: TCnMLDSAPrivateKey;
  PK: TCnMLDSAPublicKey;
  Rand: AnsiString;
  SKB, PKB: TBytes;
begin
  if (out_priv = nil) or (out_pub = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    44, CN_MLDSA_TYPE_44:
      T := cmdt44;
    65, CN_MLDSA_TYPE_65:
      T := cmdt65;
    87, CN_MLDSA_TYPE_87:
      T := cmdt87;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if (rand_hex <> nil) and (rand_len > 0) then
    SetString(Rand, PAnsiChar(rand_hex), Integer(rand_len))
  else
    Rand := '';
  D := TCnMLDSA.Create(T);
  SK := TCnMLDSAPrivateKey.Create;
  PK := TCnMLDSAPublicKey.Create;
  try
    D.GenerateKeys(SK, PK, Rand);
    SKB := D.SavePrivateKeyToBytes(SK);
    PKB := D.SavePublicKeyToBytes(PK);
    out_priv_len := Length(SKB);
    out_pub_len := Length(PKB);
    if (priv_cap < out_priv_len) or (pub_cap < out_pub_len) then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_priv_len > 0 then
      Move(SKB[0], out_priv^, out_priv_len);
    if out_pub_len > 0 then
      Move(PKB[0], out_pub^, out_pub_len);
    Result := CN_OK;
  finally
    PK.Free;
    SK.Free;
    D.Free;
  end;
end;

function cn_mldsa_sign(type_id: TInt32; sk: PByte; sk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; ctx: PByte; ctx_len: TCnSize; hash_id: TInt32; rand_hex:
  PByte; rand_len: TCnSize; out_sig: PByte; sig_cap: TCnSize; var out_sig_len:
  TCnSize): TCnResult; cdecl;
var
  D: TCnMLDSA;
  T: TCnMLDSAType;
  PrivK: TCnMLDSAPrivateKey;
  SKB, MsgB, SigB: TBytes;
  CtxS, RandS: AnsiString;
  HT: TCnMLDSAHashType;
begin
  if (sk = nil) or (msg = nil) or (out_sig = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    44, CN_MLDSA_TYPE_44:
      T := cmdt44;
    65, CN_MLDSA_TYPE_65:
      T := cmdt65;
    87, CN_MLDSA_TYPE_87:
      T := cmdt87;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case hash_id of
    0:
      HT := cmhtNone;
    CN_HASH_SHA2_256:
      HT := cmhtSHA256;
    CN_HASH_SHA2_512:
      HT := cmhtSHA512;
    CN_HASH_SHA3_256:
      HT := cmhtSHA3_256;
    CN_HASH_SHA3_512:
      HT := cmhtSHA3_512;
    CN_HASH_SM3:
      HT := cmhtSM3;
    CN_HASH_SHAKE128:
      HT := cmhtSHAKE128;
    CN_HASH_SHAKE256:
      HT := cmhtSHAKE256;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if (ctx <> nil) and (ctx_len > 0) then
    SetString(CtxS, PAnsiChar(ctx), Integer(ctx_len))
  else
    CtxS := '';
  if (rand_hex <> nil) and (rand_len > 0) then
    SetString(RandS, PAnsiChar(rand_hex), Integer(rand_len))
  else
    RandS := '';
  SetLength(SKB, sk_len);
  if sk_len > 0 then
    Move(sk^, SKB[0], sk_len);
  SetLength(MsgB, msg_len);
  if msg_len > 0 then
    Move(msg^, MsgB[0], msg_len);
  D := TCnMLDSA.Create(T);
  PrivK := TCnMLDSAPrivateKey.Create;
  try
    try
      D.LoadPrivateKeyFromBytes(PrivK, SKB);
    except
      Result := CN_E_INVALID_ARG;
      Exit;
    end;
    SigB := D.SignBytes(PrivK, MsgB, CtxS, HT, RandS);
    out_sig_len := Length(SigB);
    if sig_cap < out_sig_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_sig_len > 0 then
      Move(SigB[0], out_sig^, out_sig_len);
    Result := CN_OK;
  finally
    PrivK.Free;
    D.Free;
  end;
end;

function cn_mldsa_verify(type_id: TInt32; pk: PByte; pk_len: TCnSize; msg: PByte;
  msg_len: TCnSize; sig: PByte; sig_len: TCnSize; ctx: PByte; ctx_len: TCnSize;
  hash_id: TInt32): TBool32; cdecl;
var
  D: TCnMLDSA;
  T: TCnMLDSAType;
  PubK: TCnMLDSAPublicKey;
  PKB, MsgB, SigB: TBytes;
  CtxS: AnsiString;
  HT: TCnMLDSAHashType;
  Ok: Boolean;
begin
  if (pk = nil) or (msg = nil) or (sig = nil) then
  begin
    Result := 0;
    Exit;
  end;
  case type_id of
    44, CN_MLDSA_TYPE_44:
      T := cmdt44;
    65, CN_MLDSA_TYPE_65:
      T := cmdt65;
    87, CN_MLDSA_TYPE_87:
      T := cmdt87;
  else
    Result := 0;
    Exit;
  end;
  case hash_id of
    0:
      HT := cmhtNone;
    CN_HASH_SHA2_256:
      HT := cmhtSHA256;
    CN_HASH_SHA2_512:
      HT := cmhtSHA512;
    CN_HASH_SHA3_256:
      HT := cmhtSHA3_256;
    CN_HASH_SHA3_512:
      HT := cmhtSHA3_512;
    CN_HASH_SM3:
      HT := cmhtSM3;
    CN_HASH_SHAKE128:
      HT := cmhtSHAKE128;
    CN_HASH_SHAKE256:
      HT := cmhtSHAKE256;
  else
    Result := 0;
    Exit;
  end;
  if (ctx <> nil) and (ctx_len > 0) then
    SetString(CtxS, PAnsiChar(ctx), Integer(ctx_len))
  else
    CtxS := '';
  SetLength(PKB, pk_len);
  if pk_len > 0 then
    Move(pk^, PKB[0], pk_len);
  SetLength(MsgB, msg_len);
  if msg_len > 0 then
    Move(msg^, MsgB[0], msg_len);
  SetLength(SigB, sig_len);
  if sig_len > 0 then
    Move(sig^, SigB[0], sig_len);
  D := TCnMLDSA.Create(T);
  PubK := TCnMLDSAPublicKey.Create;
  try
    try
      D.LoadPublicKeyFromBytes(PubK, PKB);
    except
      Result := 0;
      Exit;
    end;
    Ok := D.VerifyBytes(PubK, MsgB, SigB, CtxS, HT);
    if Ok then
      Result := 1
    else
      Result := 0;
  finally
    PubK.Free;
    D.Free;
  end;
end;

function cn_mlkem_check_encap_key(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize): TCnResult; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
  EnKey: TBytes;
begin
  if (encap_key = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(EnKey, encap_len);
  if encap_len > 0 then
    Move(encap_key^, EnKey[0], encap_len);
  M := TCnMLKEM.Create(T);
  try
    try
      M.CheckEncapKey(EnKey);
      Result := CN_OK;
    except
      Result := CN_E_INVALID_ARG;
    end;
  finally
    M.Free;
  end;
end;

function cn_mlkem_check_decap_key(type_id: TInt32; decap_key: PByte; decap_len:
  TCnSize): TCnResult; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
  DeKey: TBytes;
begin
  if (decap_key = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(DeKey, decap_len);
  if decap_len > 0 then
    Move(decap_key^, DeKey[0], decap_len);
  M := TCnMLKEM.Create(T);
  try
    try
      M.CheckDecapKey(DeKey);
      Result := CN_OK;
    except
      Result := CN_E_INVALID_ARG;
    end;
  finally
    M.Free;
  end;
end;

function cn_mlkem_encaps_auto(type_id: TInt32; encap_key: PByte; encap_len:
  TCnSize; out_share_key: PByte; share_cap: TCnSize; var out_share_len: TCnSize;
  out_cipher: PByte; cipher_cap: TCnSize; var out_cipher_len: TCnSize): TCnResult; cdecl;
var
  M: TCnMLKEM;
  T: TCnMLKEMType;
  EnKey, MsgB, ShareKey, CipherText: TBytes;
begin
  if (encap_key = nil) or (out_share_key = nil) or (out_cipher = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case type_id of
    512, CN_MLKEM_TYPE_512:
      T := cmkt512;
    768, CN_MLKEM_TYPE_768:
      T := cmkt768;
    1024, CN_MLKEM_TYPE_1024:
      T := cmkt1024;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(EnKey, encap_len);
  if encap_len > 0 then
    Move(encap_key^, EnKey[0], encap_len);
  SetLength(MsgB, 32);
  CnRandomFillBytes(@MsgB[0], Length(MsgB));
  M := TCnMLKEM.Create(T);
  try
    try
      M.CheckEncapKey(EnKey);
    except
      Result := CN_E_INVALID_ARG;
      Exit;
    end;
    M.MLKEMEncaps(EnKey, MsgB, ShareKey, CipherText);
    out_share_len := Length(ShareKey);
    out_cipher_len := Length(CipherText);
    if (share_cap < out_share_len) or (cipher_cap < out_cipher_len) then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_share_len > 0 then
      Move(ShareKey[0], out_share_key^, out_share_len);
    if out_cipher_len > 0 then
      Move(CipherText[0], out_cipher^, out_cipher_len);
    Result := CN_OK;
  finally
    M.Free;
  end;
end;

function cn_curve25519_dh(self_priv: TCnCryptoHandle; peer_point_bytes: PByte;
  peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
begin
  Result := cn_curve25519_dh_step2(self_priv, peer_point_bytes, peer_len,
    out_shared_bytes, cap, out_len);
end;

function cn_curve25519_dh_bytes(self_priv_bytes: PByte; self_len: TCnSize;
  peer_point_bytes: PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  Peer: TCnEccPoint;
  Shared: TCnEccPoint;
  SelfData: TCnCurve25519Data;
  PeerData: TCnCurve25519Data;
  OutData: TCnCurve25519Data;
  K: TCnCurve25519PrivateKey;
  Ok: Boolean;
begin
  if (self_priv_bytes = nil) or (peer_point_bytes = nil) or (out_shared_bytes =
    nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if (self_len <> SizeOf(SelfData)) or (peer_len <> SizeOf(PeerData)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(OutData);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  Move(self_priv_bytes^, SelfData[0], SizeOf(SelfData));
  Move(peer_point_bytes^, PeerData[0], SizeOf(PeerData));
  K := TCnCurve25519PrivateKey.Create;
  Peer := TCnEccPoint.Create;
  Shared := TCnEccPoint.Create;
  try
    K.LoadFromData(SelfData);
    CnCurve25519DataToPoint(PeerData, Peer);
    Ok := CnCurve25519KeyExchangeStep2(K, Peer, Shared);
    if not Ok then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    CnCurve25519PointToData(Shared, OutData);
    Move(OutData[0], out_shared_bytes^, out_len);
    Result := CN_OK;
  finally
    Shared.Free;
    Peer.Free;
    K.Free;
  end;
end;

function cn_ed25519_sign_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  Sig: TCnEd25519Signature;
  E: TCnEd25519;
  R, S, K, HP: TCnBigNumber;
  Dom, Stream: TMemoryStream;
  SigData: TCnEd25519SignatureData;
  PubData, RData: TCnEd25519Data;
  Dig: TCnSHA512Digest;
  MsgPtr: Pointer;
  MsgLen: Integer;
  DomNeeded: Boolean;
  B: Byte;
begin
  if (priv = nil) or (pub = nil) or (out_sig = nil) or ((data = nil) and (len <>
    0)) or ((ctx = nil) and (ctx_len <> 0)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if ctx_len > 255 then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(SigData);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  Sig := TCnEd25519Signature.Create;
  E := TCnEd25519.Create;
  R := TCnBigNumber.Create;
  S := TCnBigNumber.Create;
  K := TCnBigNumber.Create;
  HP := TCnBigNumber.Create;
  Dom := TMemoryStream.Create;
  Stream := TMemoryStream.Create;
  try
    DomNeeded := (ph_flag <> 0) or (ctx_len <> 0);
    if DomNeeded then
    begin
      B := Ord('S');
      Dom.Write(B, 1);
      B := Ord('i');
      Dom.Write(B, 1);
      B := Ord('g');
      Dom.Write(B, 1);
      B := Ord('E');
      Dom.Write(B, 1);
      B := Ord('d');
      Dom.Write(B, 1);
      B := Ord('2');
      Dom.Write(B, 1);
      B := Ord('5');
      Dom.Write(B, 1);
      B := Ord('5');
      Dom.Write(B, 1);
      B := Ord('1');
      Dom.Write(B, 1);
      B := Ord('9');
      Dom.Write(B, 1);
      B := $01;
      Dom.Write(B, 1);
      if ph_flag <> 0 then
        B := $01
      else
        B := $00;
      Dom.Write(B, 1);
      B := Byte(ctx_len and $FF);
      Dom.Write(B, 1);
      if ctx_len > 0 then
        Dom.Write(ctx^, ctx_len);
    end;
    CnCalcKeysFromEd25519PrivateKey(TCnEd25519PrivateKey(priv), S, HP);
    if ph_flag <> 0 then
    begin
      Dig := SHA512Buffer(data^, len);
      MsgPtr := @Dig[0];
      MsgLen := SizeOf(TCnSHA512Digest);
    end
    else
    begin
      MsgPtr := data;
      MsgLen := len;
    end;
    Stream.Clear;
    if DomNeeded then
      Stream.Write(Dom.Memory^, Dom.Size);
    BigNumberWriteBinaryToStream(HP, Stream, CN_25519_BLOCK_BYTESIZE);
    if MsgLen > 0 then
      Stream.Write(MsgPtr^, MsgLen);
    Dig := SHA512Buffer(Stream.Memory^, Stream.Size);
    ReverseMemory(@Dig[0], SizeOf(TCnSHA512Digest));
    R.SetBinary(@Dig[0], SizeOf(TCnSHA512Digest));
    BigNumberNonNegativeMod(R, R, E.Order);
    Sig.R.Assign(E.Generator);
    E.MultiplePoint(R, Sig.R);
    E.PointToPlain(Sig.R, RData);
    Stream.Clear;
    if DomNeeded then
      Stream.Write(Dom.Memory^, Dom.Size);
    Stream.Write(RData[0], SizeOf(TCnEd25519Data));
    E.PointToPlain(TCnEd25519PublicKey(pub), PubData);
    Stream.Write(PubData[0], SizeOf(TCnEd25519Data));
    if MsgLen > 0 then
      Stream.Write(MsgPtr^, MsgLen);
    Dig := SHA512Buffer(Stream.Memory^, Stream.Size);
    ReverseMemory(@Dig[0], SizeOf(TCnSHA512Digest));
    K.SetBinary(@Dig[0], SizeOf(TCnSHA512Digest));
    BigNumberNonNegativeMod(K, K, E.Order);
    BigNumberDirectMulMod(Sig.S, K, S, E.Order);
    BigNumberAddMod(Sig.S, R, Sig.S, E.Order);
    Sig.SaveToData(SigData);
    Move(SigData[0], out_sig^, out_len);
    Result := CN_OK;
  finally
    Stream.Free;
    Dom.Free;
    HP.Free;
    K.Free;
    S.Free;
    R.Free;
    E.Free;
    Sig.Free;
  end;
end;

function cn_ed25519_verify_ex(ctx: PByte; ctx_len: TCnSize; ph_flag: TBool32;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig: PByte; sig_len: TCnSize):
  TBool32; cdecl;
var
  SigObj: TCnEd25519Signature;
  E: TCnEd25519;
  L, R, M: TCnEccPoint;
  T: TCnBigNumber;
  Dom, Stream: TMemoryStream;
  DataR, DataPub: TCnEd25519Data;
  SigBytes: TCnEd25519SignatureData;
  Dig: TCnSHA512Digest;
  MsgPtr: Pointer;
  MsgLen: Integer;
  DomNeeded: Boolean;
  B: Byte;
begin
  if (pub = nil) or ((data = nil) and (len <> 0)) or (sig = nil) or (sig_len <>
    SizeOf(TCnEd25519SignatureData)) or ((ctx = nil) and (ctx_len <> 0)) then
  begin
    Result := 0;
    Exit;
  end;
  if ctx_len > 255 then
  begin
    Result := 0;
    Exit;
  end;
  SigObj := TCnEd25519Signature.Create;
  E := TCnEd25519.Create;
  L := TCnEccPoint.Create;
  R := TCnEccPoint.Create;
  M := TCnEccPoint.Create;
  T := TCnBigNumber.Create;
  Dom := TMemoryStream.Create;
  Stream := TMemoryStream.Create;
  try
    Move(sig^, SigBytes[0], SizeOf(TCnEd25519SignatureData));
    SigObj.LoadFromData(SigBytes);
    DomNeeded := (ph_flag <> 0) or (ctx_len <> 0);
    if DomNeeded then
    begin
      B := Ord('S');
      Dom.Write(B, 1);
      B := Ord('i');
      Dom.Write(B, 1);
      B := Ord('g');
      Dom.Write(B, 1);
      B := Ord('E');
      Dom.Write(B, 1);
      B := Ord('d');
      Dom.Write(B, 1);
      B := Ord('2');
      Dom.Write(B, 1);
      B := Ord('5');
      Dom.Write(B, 1);
      B := Ord('5');
      Dom.Write(B, 1);
      B := Ord('1');
      Dom.Write(B, 1);
      B := Ord('9');
      Dom.Write(B, 1);
      B := $01;
      Dom.Write(B, 1);
      if ph_flag <> 0 then
        B := $01
      else
        B := $00;
      Dom.Write(B, 1);
      B := Byte(ctx_len and $FF);
      Dom.Write(B, 1);
      if ctx_len > 0 then
        Dom.Write(ctx^, ctx_len);
    end;
    if ph_flag <> 0 then
    begin
      Dig := SHA512Buffer(data^, len);
      MsgPtr := @Dig[0];
      MsgLen := SizeOf(TCnSHA512Digest);
    end
    else
    begin
      MsgPtr := data;
      MsgLen := len;
    end;
    L.Assign(E.Generator);
    E.MultiplePoint(SigObj.S, L);
    E.MultiplePoint(8, L);
    R.Assign(SigObj.R);
    E.MultiplePoint(8, R);
    Stream.Clear;
    if DomNeeded then
      Stream.Write(Dom.Memory^, Dom.Size);
    CnEd25519PointToData(SigObj.R, DataR);
    Stream.Write(DataR[0], SizeOf(TCnEd25519Data));
    CnEd25519PointToData(TCnEd25519PublicKey(pub), DataPub);
    Stream.Write(DataPub[0], SizeOf(TCnEd25519Data));
    if MsgLen > 0 then
      Stream.Write(MsgPtr^, MsgLen);
    Dig := SHA512Buffer(Stream.Memory^, Stream.Size);
    ReverseMemory(@Dig[0], SizeOf(TCnSHA512Digest));
    T.SetBinary(@Dig[0], SizeOf(TCnSHA512Digest));
    T.MulWord(8);
    BigNumberNonNegativeMod(T, T, E.Order);
    M.Assign(TCnEd25519PublicKey(pub));
    E.MultiplePoint(T, M);
    E.PointAddPoint(R, M, R);
    if CnEccPointsEqual(L, R) then
      Result := 1
    else
      Result := 0;
  finally
    Stream.Free;
    Dom.Free;
    T.Free;
    M.Free;
    R.Free;
    L.Free;
    E.Free;
    SigObj.Free;
  end;
end;

function cn_rsa_privkey_new(use_crt: TBool32): TCnCryptoHandle; cdecl;
begin
  Result := TCnRSAPrivateKey.Create(use_crt <> 0);
end;

function cn_rsa_pubkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnRSAPublicKey.Create;
end;

function cn_rsa_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
begin
  if key = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  TObject(key).Free;
  Result := CN_OK;
end;

function cn_rsa_generate_keys(modulus_bits: TInt32; use_crt: TBool32; var
  out_priv: TCnCryptoHandle; var out_pub: TCnCryptoHandle; use3: TBool32):
  TCnResult; cdecl;
var
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
  Ok: Boolean;
begin
  Priv := TCnRSAPrivateKey.Create(use_crt <> 0);
  Pub := TCnRSAPublicKey.Create;
  Ok := CnRSAGenerateKeys(modulus_bits, Priv, Pub, use3 <> 0);
  if not Ok then
  begin
    Priv.Free;
    Pub.Free;
    Result := CN_E_INTERNAL;
    Exit;
  end;
  out_priv := Priv;
  out_pub := Pub;
  Result := CN_OK;
end;

function cn_rsa_pubkey_get_modulus_bytes(pub: TCnCryptoHandle): TCnSize; cdecl;
begin
  if pub = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := TCnRSAPublicKey(pub).BytesCount;
end;

function cn_rsa_privkey_get_modulus_bytes(priv: TCnCryptoHandle): TCnSize; cdecl;
begin
  if priv = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := TCnRSAPrivateKey(priv).BytesCount;
end;

function cn_rsa_encrypt_with_public(padding: TInt32; pub: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
var
  Pad: TCnRSAPaddingMode;
begin
  if (pub = nil) or (in_ptr = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case padding of
    CN_RSA_PAD_PKCS1:
      Pad := cpmPKCS1;
    CN_RSA_PAD_OAEP:
      Pad := cpmOAEP;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := TCnRSAPublicKey(pub).BytesCount;
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if not CnRSAEncryptData(in_ptr, in_len, out_ptr, TCnRSAPublicKey(pub), Pad) then
  begin
    Result := CN_E_INTERNAL;
    Exit;
  end;
  Result := CN_OK;
end;

function cn_rsa_encrypt_with_private(priv: TCnCryptoHandle; in_ptr: PByte;
  in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
begin
  if (priv = nil) or (in_ptr = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := TCnRSAPrivateKey(priv).BytesCount;
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if not CnRSAEncryptData(in_ptr, in_len, out_ptr, TCnRSAPrivateKey(priv)) then
  begin
    Result := CN_E_INTERNAL;
    Exit;
  end;
  Result := CN_OK;
end;

function cn_rsa_decrypt_with_public(pub: TCnCryptoHandle; in_ptr: PByte; in_len:
  TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  OutBytes: Integer;
  Tmp: Pointer;
begin
  if (pub = nil) or (in_ptr = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  GetMem(Tmp, in_len);
  try
    if not CnRSADecryptData(in_ptr, in_len, Tmp, OutBytes, TCnRSAPublicKey(pub))
      then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := OutBytes;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(Tmp^, out_ptr^, out_len);
    Result := CN_OK;
  finally
    FreeMem(Tmp);
  end;
end;

function cn_rsa_decrypt_with_private(padding: TInt32; priv: TCnCryptoHandle;
  in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
var
  Pad: TCnRSAPaddingMode;
  OutBytes: Integer;
  Tmp: Pointer;
begin
  if (priv = nil) or (in_ptr = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case padding of
    CN_RSA_PAD_PKCS1:
      Pad := cpmPKCS1;
    CN_RSA_PAD_OAEP:
      Pad := cpmOAEP;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  GetMem(Tmp, in_len);
  try
    if not CnRSADecryptData(in_ptr, in_len, Tmp, OutBytes, TCnRSAPrivateKey(priv),
      Pad) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := OutBytes;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(Tmp^, out_ptr^, out_len);
    Result := CN_OK;
  finally
    FreeMem(Tmp);
  end;
end;

function cn_rsa_sign(digest_alg_id: TInt32; priv: TCnCryptoHandle; data: PByte;
  len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  InS, OutS: TMemoryStream;
  SignType: TCnRSASignDigestType;
begin
  if (priv = nil) or ((data = nil) and (len <> 0)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case digest_alg_id of
    0:
      SignType := rsdtNone;
    CN_HASH_MD5:
      SignType := rsdtMD5;
    CN_HASH_SHA1:
      SignType := rsdtSHA1;
    CN_HASH_SHA2_256:
      SignType := rsdtSHA256;
    CN_HASH_SM3:
      SignType := rsdtSM3;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  InS := TMemoryStream.Create;
  OutS := TMemoryStream.Create;
  try
    if len > 0 then
    begin
      InS.Size := len;
      Move(data^, InS.Memory^, len);
    end;
    if not CnRSASignStream(InS, OutS, TCnRSAPrivateKey(priv), SignType) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := OutS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(OutS.Memory^, out_sig^, out_len);
    Result := CN_OK;
  finally
    InS.Free;
    OutS.Free;
  end;
end;

function cn_rsa_verify(digest_alg_id: TInt32; pub: TCnCryptoHandle; data: PByte;
  len: TCnSize; sig_ptr: PByte; sig_len: TCnSize): TBool32; cdecl;
var
  InS, SigS: TMemoryStream;
  SignType: TCnRSASignDigestType;
begin
  if (pub = nil) or ((data = nil) and (len <> 0)) or (sig_ptr = nil) then
  begin
    Result := 0;
    Exit;
  end;
  case digest_alg_id of
    0:
      SignType := rsdtNone;
    CN_HASH_MD5:
      SignType := rsdtMD5;
    CN_HASH_SHA1:
      SignType := rsdtSHA1;
    CN_HASH_SHA2_256:
      SignType := rsdtSHA256;
    CN_HASH_SM3:
      SignType := rsdtSM3;
  else
    Result := 0;
    Exit;
  end;
  InS := TMemoryStream.Create;
  SigS := TMemoryStream.Create;
  try
    if len > 0 then
    begin
      InS.Size := len;
      Move(data^, InS.Memory^, len);
    end;
    if sig_len > 0 then
    begin
      SigS.Size := sig_len;
      Move(sig_ptr^, SigS.Memory^, sig_len);
    end;
    if CnRSAVerifyStream(InS, SigS, TCnRSAPublicKey(pub), SignType) then
      Result := 1
    else
      Result := 0;
  finally
    InS.Free;
    SigS.Free;
  end;
end;

function cn_rsa_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle): TCnResult; cdecl;
var
  PemStr, PwdStr: string;
  Priv: TCnRSAPrivateKey;
  Pub: TCnRSAPublicKey;
begin
  if (pem_ptr = nil) or (pem_len = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetString(PemStr, PAnsiChar(pem_ptr), pem_len);
  if (password_ptr <> nil) and (password_len > 0) then
    SetString(PwdStr, PAnsiChar(password_ptr), password_len)
  else
    PwdStr := '';
  Priv := TCnRSAPrivateKey.Create(False);
  Pub := TCnRSAPublicKey.Create;
  if not CnRSALoadKeysFromPemStr(PemStr, Priv, Pub, ckhMd5, PwdStr) then
  begin
    Priv.Free;
    Pub.Free;
    Result := CN_E_INTERNAL;
    Exit;
  end;
  out_priv := Priv;
  out_pub := Pub;
  Result := CN_OK;
end;

function cn_rsa_save_keys_to_pem(key_type_id: TInt32; priv: TCnCryptoHandle; pub:
  TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  MS: TMemoryStream;
  KT: TCnRSAKeyType;
begin
  if (priv = nil) or (pub = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case key_type_id of
    CN_RSA_KEY_PKCS1:
      KT := CnRSA.cktPKCS1;
    CN_RSA_KEY_PKCS8:
      KT := CnRSA.cktPKCS8;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  MS := TMemoryStream.Create;
  try
    if not CnRSASaveKeysToPem(MS, TCnRSAPrivateKey(priv), TCnRSAPublicKey(pub),
      KT) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := MS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(MS.Memory^, out_buf^, out_len);
    Result := CN_OK;
  finally
    MS.Free;
  end;
end;

function cn_rsa_save_public_key_to_pem(key_type_id: TInt32; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  MS: TMemoryStream;
  KT: TCnRSAKeyType;
begin
  if pub = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case key_type_id of
    CN_RSA_KEY_PKCS1:
      KT := CnRSA.cktPKCS1;
    CN_RSA_KEY_PKCS8:
      KT := CnRSA.cktPKCS8;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  MS := TMemoryStream.Create;
  try
    if not CnRSASavePublicKeyToPem(MS, TCnRSAPublicKey(pub), KT) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := MS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(MS.Memory^, out_buf^, out_len);
    Result := CN_OK;
  finally
    MS.Free;
  end;
end;

function cn_ecc_privkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnEccPrivateKey.Create;
end;

function cn_ecc_pubkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnEccPublicKey.Create;
end;

function cn_ecc_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
begin
  if key = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  TObject(key).Free;
  Result := CN_OK;
end;

function cn_ecc_curve_bytes(curve_id: TInt32): TCnSize; cdecl;
var
  E: TCnEcc;
begin
  case curve_id of
    CN_ECC_CURVE_SM2:
      E := TCnSM2.Create;
    CN_ECC_CURVE_SECP256K1:
      E := TCnEcc.Create(ctSecp256k1);
    CN_ECC_CURVE_SECP256R1:
      E := TCnEcc.Create(ctSecp256r1);
    CN_ECC_CURVE_PRIME256V1:
      E := TCnEcc.Create(ctPrime256v1);
    CN_ECC_CURVE_SECP384R1:
      E := TCnEcc.Create(ctSecp384r1);
    CN_ECC_CURVE_SECP521R1:
      E := TCnEcc.Create(ctSecp521r1);
  else
    Result := 0;
    Exit;
  end;
  try
    Result := E.BytesCount;
  finally
    E.Free;
  end;
end;

function cn_ecc_generate_keys(curve_id: TInt32; var out_priv: TCnCryptoHandle;
  var out_pub: TCnCryptoHandle): TCnResult; cdecl;
var
  E: TCnEcc;
  Priv: TCnEccPrivateKey;
  Pub: TCnEccPublicKey;
begin
  case curve_id of
    CN_ECC_CURVE_SM2:
      E := TCnSM2.Create;
    CN_ECC_CURVE_SECP256K1:
      E := TCnEcc.Create(ctSecp256k1);
    CN_ECC_CURVE_SECP256R1:
      E := TCnEcc.Create(ctSecp256r1);
    CN_ECC_CURVE_PRIME256V1:
      E := TCnEcc.Create(ctPrime256v1);
    CN_ECC_CURVE_SECP384R1:
      E := TCnEcc.Create(ctSecp384r1);
    CN_ECC_CURVE_SECP521R1:
      E := TCnEcc.Create(ctSecp521r1);
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Priv := TCnEccPrivateKey.Create;
  Pub := TCnEccPublicKey.Create;
  try
    E.GenerateKeys(Priv, Pub);
    out_priv := Priv;
    out_pub := Pub;
    Result := CN_OK;
  except
    Priv.Free;
    Pub.Free;
    E.Free;
    Result := CN_E_INTERNAL;
    Exit;
  end;
  E.Free;
end;

function cn_ecc_sign(digest_alg_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl;
var
  InS, OutS: TMemoryStream;
  SignType: TCnEccSignDigestType;
  Ok: Boolean;
begin
  if (priv = nil) or ((data = nil) and (len <> 0)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case digest_alg_id of
    CN_HASH_MD5:
      SignType := esdtMD5;
    CN_HASH_SHA1:
      SignType := esdtSHA1;
    CN_HASH_SHA2_256:
      SignType := esdtSHA256;
    CN_HASH_SHA2_512:
      SignType := esdtSHA512;
    CN_HASH_SM3:
      SignType := esdtSM3;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  InS := TMemoryStream.Create;
  OutS := TMemoryStream.Create;
  try
    if len > 0 then
    begin
      InS.Size := len;
      Move(data^, InS.Memory^, len);
    end;
    case curve_id of
      CN_ECC_CURVE_SM2:
        Ok := CnEccSignStream(InS, OutS, ctSM2, TCnEccPrivateKey(priv), SignType);
      CN_ECC_CURVE_SECP256K1:
        Ok := CnEccSignStream(InS, OutS, ctSecp256k1, TCnEccPrivateKey(priv), SignType);
      CN_ECC_CURVE_SECP256R1:
        Ok := CnEccSignStream(InS, OutS, ctSecp256r1, TCnEccPrivateKey(priv), SignType);
      CN_ECC_CURVE_PRIME256V1:
        Ok := CnEccSignStream(InS, OutS, ctPrime256v1, TCnEccPrivateKey(priv), SignType);
      CN_ECC_CURVE_SECP384R1:
        Ok := CnEccSignStream(InS, OutS, ctSecp384r1, TCnEccPrivateKey(priv), SignType);
      CN_ECC_CURVE_SECP521R1:
        Ok := CnEccSignStream(InS, OutS, ctSecp521r1, TCnEccPrivateKey(priv), SignType);
    else
      Result := CN_E_INVALID_ARG;
      Exit;
    end;
    if not Ok then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := OutS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(OutS.Memory^, out_sig_der^, out_len);
    Result := CN_OK;
  finally
    InS.Free;
    OutS.Free;
  end;
end;

function cn_ecc_verify(digest_alg_id: TInt32; curve_id: TInt32; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl;
var
  InS, SigS: TMemoryStream;
  SignType: TCnEccSignDigestType;
  Ok: Boolean;
begin
  if (pub = nil) or ((data = nil) and (len <> 0)) or (sig_der = nil) then
  begin
    Result := 0;
    Exit;
  end;
  case digest_alg_id of
    CN_HASH_MD5:
      SignType := esdtMD5;
    CN_HASH_SHA1:
      SignType := esdtSHA1;
    CN_HASH_SHA2_256:
      SignType := esdtSHA256;
    CN_HASH_SHA2_512:
      SignType := esdtSHA512;
    CN_HASH_SM3:
      SignType := esdtSM3;
  else
    Result := 0;
    Exit;
  end;
  InS := TMemoryStream.Create;
  SigS := TMemoryStream.Create;
  try
    if len > 0 then
    begin
      InS.Size := len;
      Move(data^, InS.Memory^, len);
    end;
    if sig_len > 0 then
    begin
      SigS.Size := sig_len;
      Move(sig_der^, SigS.Memory^, sig_len);
    end;
    case curve_id of
      CN_ECC_CURVE_SM2:
        Ok := CnEccVerifyStream(InS, SigS, ctSM2, TCnEccPublicKey(pub), SignType);
      CN_ECC_CURVE_SECP256K1:
        Ok := CnEccVerifyStream(InS, SigS, ctSecp256k1, TCnEccPublicKey(pub), SignType);
      CN_ECC_CURVE_SECP256R1:
        Ok := CnEccVerifyStream(InS, SigS, ctSecp256r1, TCnEccPublicKey(pub), SignType);
      CN_ECC_CURVE_PRIME256V1:
        Ok := CnEccVerifyStream(InS, SigS, ctPrime256v1, TCnEccPublicKey(pub), SignType);
      CN_ECC_CURVE_SECP384R1:
        Ok := CnEccVerifyStream(InS, SigS, ctSecp384r1, TCnEccPublicKey(pub), SignType);
      CN_ECC_CURVE_SECP521R1:
        Ok := CnEccVerifyStream(InS, SigS, ctSecp521r1, TCnEccPublicKey(pub), SignType);
    else
      Result := 0;
      Exit;
    end;
    if Ok then
      Result := 1
    else
      Result := 0;
  finally
    InS.Free;
    SigS.Free;
  end;
end;

function cn_ecc_load_keys_from_pem(pem_ptr: PByte; pem_len: TCnSize;
  password_ptr: PByte; password_len: TCnSize; var out_priv: TCnCryptoHandle; var
  out_pub: TCnCryptoHandle; var out_curve_id: TInt32): TCnResult; cdecl;
var
  MS: TMemoryStream;
  Priv: TCnEccPrivateKey;
  Pub: TCnEccPublicKey;
  Curve: TCnEccCurveType;
  Pwd: string;
begin
  if (pem_ptr = nil) or (pem_len = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if (password_ptr <> nil) and (password_len > 0) then
    SetString(Pwd, PAnsiChar(password_ptr), Integer(password_len))
  else
    Pwd := '';
  Result := CN_OK;
  Priv := TCnEccPrivateKey.Create;
  Pub := TCnEccPublicKey.Create;
  MS := TMemoryStream.Create;
  try
    MS.Size := pem_len;
    Move(pem_ptr^, MS.Memory^, pem_len);
    if not CnEccLoadKeysFromPem(MS, Priv, Pub, Curve, ckhMd5, Pwd) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_priv := Priv;
    out_pub := Pub;
    case Curve of
      ctSM2:
        out_curve_id := CN_ECC_CURVE_SM2;
      ctSecp256k1:
        out_curve_id := CN_ECC_CURVE_SECP256K1;
      ctSecp256r1:
        out_curve_id := CN_ECC_CURVE_SECP256R1;
      ctPrime256v1:
        out_curve_id := CN_ECC_CURVE_PRIME256V1;
      ctSecp384r1:
        out_curve_id := CN_ECC_CURVE_SECP384R1;
      ctSecp521r1:
        out_curve_id := CN_ECC_CURVE_SECP521R1;
    else
      out_curve_id := 0;
    end;
  finally
    MS.Free;
    if Result <> CN_OK then
    begin
      Priv.Free;
      Pub.Free;
    end;
  end;
end;

function cn_ecc_save_keys_to_pem(key_type_id: TInt32; curve_id: TInt32; priv:
  TCnCryptoHandle; pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var
  out_len: TCnSize): TCnResult; cdecl;
var
  MS: TMemoryStream;
  KT: TCnEccKeyType;
  Curve: TCnEccCurveType;
begin
  if (priv = nil) or (pub = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case key_type_id of
    CN_ECC_KEY_PKCS1:
      KT := cktPKCS1;
    CN_ECC_KEY_PKCS8:
      KT := cktPKCS1;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case curve_id of
    CN_ECC_CURVE_SM2:
      Curve := ctSM2;
    CN_ECC_CURVE_SECP256K1:
      Curve := ctSecp256k1;
    CN_ECC_CURVE_SECP256R1:
      Curve := ctSecp256r1;
    CN_ECC_CURVE_PRIME256V1:
      Curve := ctPrime256v1;
    CN_ECC_CURVE_SECP384R1:
      Curve := ctSecp384r1;
    CN_ECC_CURVE_SECP521R1:
      Curve := ctSecp521r1;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  MS := TMemoryStream.Create;
  try
    if not CnEccSaveKeysToPem(MS, TCnEccPrivateKey(priv), TCnEccPublicKey(pub),
      Curve, KT) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := MS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(MS.Memory^, out_buf^, out_len);
    Result := CN_OK;
  finally
    MS.Free;
  end;
end;

function cn_ecc_save_public_key_to_pem(key_type_id: TInt32; curve_id: TInt32;
  pub: TCnCryptoHandle; out_buf: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
var
  MS: TMemoryStream;
  KT: TCnEccKeyType;
  Curve: TCnEccCurveType;
begin
  if pub = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case key_type_id of
    CN_ECC_KEY_PKCS1:
      KT := cktPKCS1;
    CN_ECC_KEY_PKCS8:
      KT := cktPKCS1;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case curve_id of
    CN_ECC_CURVE_SM2:
      Curve := ctSM2;
    CN_ECC_CURVE_SECP256K1:
      Curve := ctSecp256k1;
    CN_ECC_CURVE_SECP256R1:
      Curve := ctSecp256r1;
    CN_ECC_CURVE_PRIME256V1:
      Curve := ctPrime256v1;
    CN_ECC_CURVE_SECP384R1:
      Curve := ctSecp384r1;
    CN_ECC_CURVE_SECP521R1:
      Curve := ctSecp521r1;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  MS := TMemoryStream.Create;
  try
    if not CnEccSavePublicKeyToPem(MS, TCnEccPublicKey(pub), Curve, KT) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := MS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(MS.Memory^, out_buf^, out_len);
    Result := CN_OK;
  finally
    MS.Free;
  end;
end;

function cn_sm2_privkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnSM2PrivateKey.Create;
end;

function cn_sm2_pubkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnSM2PublicKey.Create;
end;

function cn_sm2_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
begin
  if key = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  TObject(key).Free;
  Result := CN_OK;
end;

function cn_sm2_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
var
  Priv: TCnSM2PrivateKey;
  Pub: TCnSM2PublicKey;
begin
  Priv := TCnSM2PrivateKey.Create;
  Pub := TCnSM2PublicKey.Create;
  if not CnSM2GenerateKeys(Priv, Pub) then
  begin
    Priv.Free;
    Pub.Free;
    Result := CN_E_INTERNAL;
    Exit;
  end;
  out_priv := Priv;
  out_pub := Pub;
  Result := CN_OK;
end;

function cn_sm2_encrypt(seq_type_id: TInt32; include_prefix: TBool32; pub:
  TCnCryptoHandle; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize;
  var out_len: TCnSize): TCnResult; cdecl;
var
  ST: TCnSM2CryptSequenceType;
  IncludePrefix: Boolean;
  Plain: TBytes;
  En: TBytes;
begin
  if (pub = nil) or ((in_ptr = nil) and (in_len <> 0)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case seq_type_id of
    CN_SM2_SEQ_C1C3C2:
      ST := cstC1C3C2;
    CN_SM2_SEQ_C1C2C3:
      ST := cstC1C2C3;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  IncludePrefix := include_prefix <> 0;
  SetLength(Plain, in_len);
  if in_len > 0 then
    Move(in_ptr^, Plain[0], in_len);
  En := CnSM2EncryptData(Plain, TCnSM2PublicKey(pub), nil, ST, IncludePrefix, '');
  out_len := Length(En);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if out_len > 0 then
    Move(En[0], out_ptr^, out_len);
  Result := CN_OK;
end;

function cn_sm2_decrypt(seq_type_id: TInt32; priv: TCnCryptoHandle; in_ptr:
  PByte; in_len: TCnSize; out_ptr: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
var
  ST: TCnSM2CryptSequenceType;
  En, Plain: TBytes;
begin
  if (priv = nil) or ((in_ptr = nil) and (in_len <> 0)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case seq_type_id of
    CN_SM2_SEQ_C1C3C2:
      ST := cstC1C3C2;
    CN_SM2_SEQ_C1C2C3:
      ST := cstC1C2C3;
  else
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(En, in_len);
  if in_len > 0 then
    Move(in_ptr^, En[0], in_len);
  Plain := CnSM2DecryptData(En, TCnSM2PrivateKey(priv), nil, ST);
  out_len := Length(Plain);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if out_len > 0 then
    Move(Plain[0], out_ptr^, out_len);
  Result := CN_OK;
end;

function cn_sm2_sign(user_id: PByte; user_id_len: TCnSize; priv: TCnCryptoHandle;
  pub: TCnCryptoHandle; data: PByte; len: TCnSize; out_sig_der: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  UID: AnsiString;
  Sig: TCnSM2Signature;
  Plain: TBytes;
  B64: string;
  Der: TBytes;
begin
  if (priv = nil) or ((data = nil) and (len <> 0)) or ((user_id = nil) and (user_id_len
    <> 0)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetString(UID, PAnsiChar(user_id), Integer(user_id_len));
  SetLength(Plain, len);
  if len > 0 then
    Move(data^, Plain[0], len);
  Sig := TCnSM2Signature.Create;
  try
    if not CnSM2SignData(UID, Plain, Sig, TCnSM2PrivateKey(priv),
      TCnSM2PublicKey(pub), nil, '') then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    B64 := Sig.ToAsn1Base64;
    if Base64Decode(B64, Der) <> ECN_BASE64_OK then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := Length(Der);
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(Der[0], out_sig_der^, out_len);
    Result := CN_OK;
  finally
    Sig.Free;
  end;
end;

function cn_sm2_verify(user_id: PByte; user_id_len: TCnSize; pub:
  TCnCryptoHandle; data: PByte; len: TCnSize; sig_der: PByte; sig_len: TCnSize):
  TBool32; cdecl;
var
  UID: AnsiString;
  Sig: TCnSM2Signature;
  Plain: TBytes;
  B64: string;
begin
  if (pub = nil) or ((data = nil) and (len <> 0)) or (sig_der = nil) or ((user_id
    = nil) and (user_id_len <> 0)) then
  begin
    Result := 0;
    Exit;
  end;
  SetString(UID, PAnsiChar(user_id), user_id_len);
  SetLength(Plain, len);
  if len > 0 then
    Move(data^, Plain[0], len);
  SetLength(B64, 0);
  B64 := '';
  if Base64Encode(sig_der, Integer(sig_len), B64, False) <> ECN_BASE64_OK then
  begin
    Result := 0;
    Exit;
  end;
  Sig := TCnSM2Signature.Create;
  try
    if not Sig.SetAsn1Base64(B64) then
    begin
      Result := 0;
      Exit;
    end;
    if CnSM2VerifyData(UID, Plain, Sig, TCnSM2PublicKey(pub), nil) then
      Result := 1
    else
      Result := 0;
  finally
    Sig.Free;
  end;
end;

function cn_sm2_save_keys_to_pem(priv: TCnCryptoHandle; pub: TCnCryptoHandle;
  out_buf: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  MS: TMemoryStream;
begin
  if (priv = nil) or (pub = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  MS := TMemoryStream.Create;
  try
    if not CnEccSaveKeysToPem(MS, TCnSM2PrivateKey(priv), TCnSM2PublicKey(pub),
      ctSM2, cktPKCS1) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := MS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(MS.Memory^, out_buf^, out_len);
    Result := CN_OK;
  finally
    MS.Free;
  end;
end;

function cn_sm2_save_public_key_to_pem(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  MS: TMemoryStream;
begin
  if pub = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  MS := TMemoryStream.Create;
  try
    if not CnEccSavePublicKeyToPem(MS, TCnSM2PublicKey(pub), ctSM2, cktPKCS1) then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_len := MS.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(MS.Memory^, out_buf^, out_len);
    Result := CN_OK;
  finally
    MS.Free;
  end;
end;

function cn_get_abi_version: TUInt32; cdecl;
begin
  Result := 3;
end;

function cn_lib_init: TCnResult; cdecl;
begin
  Result := CN_OK;
end;

function cn_lib_finalize: TCnResult; cdecl;
begin
  Result := CN_OK;
end;

function cn_alloc(size: TCnSize): TCnCryptoHandle; cdecl;
begin
  if size = 0 then
  begin
    Result := nil;
    Exit;
  end;
  GetMem(Result, size);
end;

function cn_free(ptr: TCnCryptoHandle): TCnResult; cdecl;
begin
  if ptr = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  FreeMem(ptr);
  Result := CN_OK;
end;

function cn_memzero(ptr: TCnCryptoHandle; size: TCnSize): TCnResult; cdecl;
begin
  if (ptr = nil) or (size = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  FillChar(ptr^, size, 0);
  Result := CN_OK;
end;

function cn_endian_is_le: TBool32; cdecl;
begin
  if CurrentByteOrderIsLittleEndian then
    Result := 1
  else
    Result := 0;
end;

function cn_endian_is_be: TBool32; cdecl;
begin
  if CurrentByteOrderIsBigEndian then
    Result := 1
  else
    Result := 0;
end;

function cn_data_to_hex(in_ptr: Pointer; in_len: TCnSize; out_hex: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  S: string;
  L: TCnSize;
begin
  if (in_ptr = nil) and (in_len <> 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  S := DataToHex(in_ptr, in_len, True);
  L := Length(S);
  out_len := L;
  if cap < L then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if L > 0 then
    Move(S[1], out_hex^, L);
  Result := CN_OK;
end;

function cn_const_time_equal(a: Pointer; b: Pointer; len: TCnSize): TBool32; cdecl;
var
  BA, BB: TBytes;
begin
  if (a = nil) or (b = nil) then
  begin
    Result := 0;
    Exit;
  end;
  SetLength(BA, len);
  SetLength(BB, len);
  Move(a^, BA[0], len);
  Move(b^, BB[0], len);
  if ConstTimeBytesEqual(BA, BB) then
    Result := 1
  else
    Result := 0;
end;

function cn_const_time_select(flag: TBool32; a: Pointer; b: Pointer; len:
  TCnSize; out_ptr: Pointer): TCnResult; cdecl;
var
  I: Integer;
  VA, VB: Byte;
begin
  if (a = nil) or (b = nil) or (out_ptr = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  for I := 0 to len - 1 do
  begin
    VA := PByteArray(a)[I];
    VB := PByteArray(b)[I];
    PByteArray(out_ptr)[I] := Byte(ConstTimeConditionalSelect8(flag <> 0, VA, VB));
  end;
  Result := CN_OK;
end;

function cn_str_to_uint64(ascii_ptr: PByte; len: TCnSize; var out_value: TUInt64):
  TCnResult; cdecl;
var
  S: string;
begin
  if (ascii_ptr = nil) or (len = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetString(S, PAnsiChar(ascii_ptr), len);
  out_value := StrToUInt64(S);
  Result := CN_OK;
end;

function cn_base64_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  S: string;
  R: Integer;
begin
  if (in_ptr = nil) and (in_len <> 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  R := Base64Encode(in_ptr, in_len, S, False);
  if R <> 0 then
  begin
    Result := CN_E_INTERNAL;
    Exit;
  end;
  out_len := Length(S);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if out_len > 0 then
    Move(S[1], out_ptr^, out_len);
  Result := CN_OK;
end;

function cn_base64_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  S: string;
  R: Integer;
  Data: TBytes;
begin
  if (in_ptr = nil) or (in_len = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetString(S, PAnsiChar(in_ptr), in_len);

  R := Base64Decode(S, Data, True);
  if R <> 0 then
  begin
    Result := CN_E_INTERNAL;
    Exit;
  end;

  out_len := Length(Data);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;

  if out_len > 0 then
    Move(Data[0], out_ptr^, out_len);

  Result := CN_OK;
end;

function cn_base64url_encode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  S: string;
  R: Integer;
begin
  if (in_ptr = nil) and (in_len <> 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  R := Base64Encode(in_ptr, in_len, S, True);
  if R <> 0 then
  begin
    Result := CN_E_INTERNAL;
    Exit;
  end;
  out_len := Length(S);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if out_len > 0 then
    Move(S[1], out_ptr^, out_len);
  Result := CN_OK;
end;

function cn_base64url_decode(in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  S: string;
  R: Integer;
  Data: TBytes;
begin
  if (in_ptr = nil) or (in_len = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetString(S, PAnsiChar(in_ptr), in_len);

  R := Base64Decode(S, Data, True);
  if R <> 0 then
  begin
    Result := CN_E_INTERNAL;
    Exit;
  end;

  out_len := Length(Data);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;

  if out_len > 0 then
    Move(Data[0], out_ptr^, out_len);

  Result := CN_OK;
end;

function cn_otp_hotp(seed: PByte; seed_len: TCnSize; counter: TUInt64; digits:
  TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  Gen: TCnHOTPGenerator;
  S: string;
begin
  if (seed = nil) or (seed_len = 0) or (digits <= 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Gen := TCnHOTPGenerator.Create;
  try
    Gen.SetSeedKey(seed, seed_len);
    Gen.SetCounter(counter);
    Gen.Digits := digits;
    S := Gen.OneTimePassword;
    out_len := Length(S);
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(S[1], out_code_ascii^, out_len);
    Result := CN_OK;
  finally
    Gen.Free;
  end;
end;

function cn_otp_totp(hash_id: TInt32; seed: PByte; seed_len: TCnSize; period_sec:
  TInt32; digits: TInt32; out_code_ascii: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
var
  Gen: TCnTOTPGenerator;
  S: string;
begin
  if (seed = nil) or (seed_len = 0) or (digits <= 0) or (period_sec <= 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Gen := TCnTOTPGenerator.Create;
  try
    Gen.SetSeedKey(seed, seed_len);
    Gen.Period := period_sec;
    Gen.Digits := digits;
    case hash_id of
      CN_HASH_SHA1:
        Gen.PasswordType := tptSHA1;
      CN_HASH_SHA2_256:
        Gen.PasswordType := tptSHA256;
      CN_HASH_SHA2_512:
        Gen.PasswordType := tptSHA512;
    else
      Result := CN_E_INVALID_ARG;
      Exit;
    end;
    S := Gen.OneTimePassword;
    out_len := Length(S);
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(S[1], out_code_ascii^, out_len);
    Result := CN_OK;
  finally
    Gen.Free;
  end;
end;

function cn_hash_digest(alg_id: TInt32; data: PByte; len: TCnSize; out_digest:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  D16: TCnMD5Digest;
  D20: TCnSHA1Digest;
  D32: TCnSHA256Digest;
  D64: TCnSHA512Digest;
  S3_224: TCnSHA3_224Digest;
  S3_256: TCnSHA3_256Digest;
  S3_384: TCnSHA3_384Digest;
  S3_512: TCnSHA3_512Digest;
  SM3D: TCnSM3Digest;
  BK224: TCnBLAKE224Digest;
  BK256: TCnBLAKE256Digest;
  BK384: TCnBLAKE384Digest;
  BK512: TCnBLAKE512Digest;
  B2S: TCnBLAKE2SDigest;
  B2B: TCnBLAKE2BDigest;
  XX32: TCnXXH32Digest;
  XX64: TCnXXH64Digest;
begin
  if (data = nil) and (len <> 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  case alg_id of
    CN_HASH_MD5:
      begin
        out_len := SizeOf(D16);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D16 := MD5Buffer(data^, len);
        Move(D16[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA1:
      begin
        out_len := SizeOf(D20);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D20 := SHA1Buffer(data^, len);
        Move(D20[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA2_256:
      begin
        out_len := SizeOf(D32);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D32 := SHA256Buffer(data^, len);
        Move(D32[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA2_512:
      begin
        out_len := SizeOf(D64);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D64 := SHA512Buffer(data^, len);
        Move(D64[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_224:
      begin
        out_len := SizeOf(S3_224);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_224 := SHA3_224Buffer(data^, len);
        Move(S3_224[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_256:
      begin
        out_len := SizeOf(S3_256);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_256 := SHA3_256Buffer(data^, len);
        Move(S3_256[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_384:
      begin
        out_len := SizeOf(S3_384);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_384 := SHA3_384Buffer(data^, len);
        Move(S3_384[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_512:
      begin
        out_len := SizeOf(S3_512);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_512 := SHA3_512Buffer(data^, len);
        Move(S3_512[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SM3:
      begin
        out_len := SizeOf(SM3D);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        SM3D := SM3Buffer(data^, len);
        Move(SM3D[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE224:
      begin
        out_len := SizeOf(BK224);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK224 := BLAKE224Buffer(data^, len);
        Move(BK224[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE256:
      begin
        out_len := SizeOf(BK256);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK256 := BLAKE256Buffer(data^, len);
        Move(BK256[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE384:
      begin
        out_len := SizeOf(BK384);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK384 := BLAKE384Buffer(data^, len);
        Move(BK384[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE512:
      begin
        out_len := SizeOf(BK512);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK512 := BLAKE512Buffer(data^, len);
        Move(BK512[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE2S:
      begin
        out_len := SizeOf(B2S);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        B2S := BLAKE2S(PAnsiChar(data), len, nil, 0, CN_BLAKE2S_OUTBYTES);
        Move(B2S[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE2B:
      begin
        out_len := SizeOf(B2B);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        B2B := BLAKE2B(PAnsiChar(data), len, nil, 0, CN_BLAKE2B_OUTBYTES);
        Move(B2B[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_XXH32:
      begin
        out_len := SizeOf(XX32);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        XX32 := XXH32Buffer(data^, len, 0);
        Move(XX32[0], out_digest^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_XXH64:
      begin
        out_len := SizeOf(XX64);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        XX64 := XXH64Buffer(data^, len, 0);
        Move(XX64[0], out_digest^, out_len);
        Result := CN_OK;
      end;
  else
    Result := CN_E_UNSUPPORTED;
  end;
end;

function cn_hmac(alg_id: TInt32; key: PByte; key_len: TCnSize; data: PByte; len:
  TCnSize; out_mac: PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  K, V: TBytes;
  D16: TCnMD5Digest;
  D20: TCnSHA1Digest;
  D32: TCnSHA256Digest;
  D64: TCnSHA512Digest;
  S3_224: TCnSHA3_224Digest;
  S3_256: TCnSHA3_256Digest;
  S3_384: TCnSHA3_384Digest;
  S3_512: TCnSHA3_512Digest;
  SM3D: TCnSM3Digest;
  BK224: TCnBLAKE224Digest;
  BK256: TCnBLAKE256Digest;
  BK384: TCnBLAKE384Digest;
  BK512: TCnBLAKE512Digest;
  B2S: TCnBLAKE2SDigest;
  B2B: TCnBLAKE2BDigest;
begin
  if (key = nil) or (data = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  SetLength(K, key_len);
  SetLength(V, len);
  Move(key^, K[0], key_len);
  Move(data^, V[0], len);
  case alg_id of
    CN_HASH_MD5:
      begin
        out_len := SizeOf(D16);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D16 := MD5HmacBytes(K, V);
        Move(D16[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA1:
      begin
        out_len := SizeOf(D20);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D20 := SHA1HmacBytes(K, V);
        Move(D20[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA2_256:
      begin
        out_len := SizeOf(D32);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D32 := SHA256HmacBytes(K, V);
        Move(D32[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA2_512:
      begin
        out_len := SizeOf(D64);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        D64 := SHA512HmacBytes(K, V);
        Move(D64[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_224:
      begin
        out_len := SizeOf(S3_224);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_224 := SHA3_224HmacBytes(K, V);
        Move(S3_224[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_256:
      begin
        out_len := SizeOf(S3_256);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_256 := SHA3_256HmacBytes(K, V);
        Move(S3_256[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_384:
      begin
        out_len := SizeOf(S3_384);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_384 := SHA3_384HmacBytes(K, V);
        Move(S3_384[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SHA3_512:
      begin
        out_len := SizeOf(S3_512);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        S3_512 := SHA3_512HmacBytes(K, V);
        Move(S3_512[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_SM3:
      begin
        out_len := SizeOf(SM3D);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        SM3D := SM3HmacBytes(K, V);
        Move(SM3D[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE224:
      begin
        out_len := SizeOf(BK224);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK224 := BLAKE224HmacBytes(K, V);
        Move(BK224[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE256:
      begin
        out_len := SizeOf(BK256);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK256 := BLAKE256HmacBytes(K, V);
        Move(BK256[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE384:
      begin
        out_len := SizeOf(BK384);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK384 := BLAKE384HmacBytes(K, V);
        Move(BK384[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE512:
      begin
        out_len := SizeOf(BK512);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        BK512 := BLAKE512HmacBytes(K, V);
        Move(BK512[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE2S:
      begin
        out_len := SizeOf(B2S);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        B2S := BLAKE2SBytes(V, K, CN_BLAKE2S_OUTBYTES);
        Move(B2S[0], out_mac^, out_len);
        Result := CN_OK;
      end;
    CN_HASH_BLAKE2B:
      begin
        out_len := SizeOf(B2B);
        if cap < out_len then
        begin
          Result := CN_E_BUFFER_TOO_SMALL;
          Exit;
        end;
        B2B := BLAKE2BBytes(V, K, CN_BLAKE2B_OUTBYTES);
        Move(B2B[0], out_mac^, out_len);
        Result := CN_OK;
      end;
  else
    Result := CN_E_UNSUPPORTED;
  end;
end;

function cn_cipher_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  Src, Dst: TMemoryStream;
  IVBuf: TCnAESBuffer;
  K128: TCnAESKey128;
  K192: TCnAESKey192;
  K256: TCnAESKey256;
  Nonce: TCnAESCTRNonce;
  CTRIv: TCnAESCTRIv;
  IvPos: PByte;
  DKey: TCnDESKey;
  DIVec: TCnDESIv;
  T3Key: TCn3DESKey;
  SM4K: TCnSM4Key;
  SM4IV: TCnSM4Iv;
  SM4Nonce: TCnSM4Nonce;
  I: Integer;
  KeyStreamLen: Cardinal;
  KeyStream: PCardinal;
  CK: TCnChaChaKey;
  CN: TCnChaChaNonce;
begin
  if (key = nil) or (in_ptr = nil) or (in_len = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Src := TMemoryStream.Create;
  Dst := TMemoryStream.Create;
  try
    Src.Size := in_len;
    Src.Position := 0;
    Move(in_ptr^, Src.Memory^, in_len);
    case alg_id of
      CN_CIPHER_DES_ECB, CN_CIPHER_DES_CBC:
        begin
          if (alg_id = CN_CIPHER_DES_ECB) and (key_len = SizeOf(DKey)) then
          begin
            Move(key^, DKey[0], SizeOf(DKey));
            DESEncryptStreamECB(Src, in_len, DKey, Dst);
          end
          else if (alg_id = CN_CIPHER_DES_CBC) and (key_len = SizeOf(DKey)) then
          begin
            if iv = nil then
            begin
              Result := CN_E_INVALID_ARG;
              Exit;
            end;
            if iv_len < SizeOf(DIVec) then
            begin
              Result := CN_E_BUFFER_TOO_SMALL;
              Exit;
            end;
            Move(key^, DKey[0], SizeOf(DKey));
            Move(iv^, DIVec[0], SizeOf(DIVec));
            DESEncryptStreamCBC(Src, in_len, DKey, DIVec, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_3DES_ECB, CN_CIPHER_3DES_CBC:
        begin
          if (alg_id = CN_CIPHER_3DES_ECB) and (key_len = SizeOf(T3Key)) then
          begin
            Move(key^, T3Key[0], SizeOf(T3Key));
            TripleDESEncryptStreamECB(Src, in_len, T3Key, Dst);
          end
          else if (alg_id = CN_CIPHER_3DES_CBC) and (key_len = SizeOf(T3Key))
            then
          begin
            if iv = nil then
            begin
              Result := CN_E_INVALID_ARG;
              Exit;
            end;
            if iv_len < SizeOf(DIVec) then
            begin
              Result := CN_E_BUFFER_TOO_SMALL;
              Exit;
            end;
            Move(key^, T3Key[0], SizeOf(T3Key));
            Move(iv^, DIVec[0], SizeOf(DIVec));
            TripleDESEncryptStreamCBC(Src, in_len, T3Key, DIVec, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_SM4_ECB, CN_CIPHER_SM4_CBC, CN_CIPHER_SM4_CFB, CN_CIPHER_SM4_OFB,
        CN_CIPHER_SM4_CTR:
        begin
          if key_len <> SizeOf(SM4K) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Move(key^, SM4K[0], SizeOf(SM4K));
          case alg_id of
            CN_CIPHER_SM4_ECB:
              SM4EncryptStreamECB(Src, in_len, SM4K, Dst);
            CN_CIPHER_SM4_CBC:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4IV)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4IV[0], SizeOf(SM4IV));
                SM4EncryptStreamCBC(Src, in_len, SM4K, SM4IV, Dst);
              end;
            CN_CIPHER_SM4_CFB:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4IV)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4IV[0], SizeOf(SM4IV));
                SM4EncryptStreamCFB(Src, in_len, SM4K, SM4IV, Dst);
              end;
            CN_CIPHER_SM4_OFB:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4IV)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4IV[0], SizeOf(SM4IV));
                SM4EncryptStreamOFB(Src, in_len, SM4K, SM4IV, Dst);
              end;
            CN_CIPHER_SM4_CTR:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4Nonce)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4Nonce[0], SizeOf(SM4Nonce));
                SM4EncryptStreamCTR(Src, in_len, SM4K, SM4Nonce, Dst);
              end;
          end;
        end;
      CN_CIPHER_RC4:
        begin
          if (key_len = 0) or (key_len > CN_RC4_MAX_KEY_BYTE_LENGTH) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Dst.Size := in_len;
          RC4Encrypt(key, key_len, Src.Memory, Dst.Memory, in_len);
        end;
      CN_CIPHER_ZUC:
        begin
          if (key_len <> CN_ZUC_KEYSIZE) or (iv = nil) or (iv_len <>
            CN_ZUC_KEYSIZE) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Dst.Size := in_len;
          KeyStreamLen := (in_len + SizeOf(Cardinal) - 1) div SizeOf(Cardinal);
          GetMem(KeyStream, KeyStreamLen * SizeOf(Cardinal));
          try
            ZUC(key, iv, KeyStream, KeyStreamLen);
            for I := 0 to in_len - 1 do
              PByteArray(Dst.Memory)[I] := PByteArray(Src.Memory)[I] xor
                PByteArray(KeyStream)[I];
          finally
            FreeMem(KeyStream);
          end;
        end;
      CN_CIPHER_CHACHA20:
        begin
          if (key_len <> SizeOf(CK)) or (iv = nil) or (iv_len <> SizeOf(CN)) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Move(key^, CK[0], SizeOf(CK));
          Move(iv^, CN[0], SizeOf(CN));
          Dst.Size := in_len;
          if not ChaCha20EncryptData(CK, CN, Src.Memory, in_len, Dst.Memory) then
          begin
            Result := CN_E_INTERNAL;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_ECB, CN_CIPHER_AES192_ECB, CN_CIPHER_AES256_ECB:
        begin
          if (alg_id = CN_CIPHER_AES128_ECB) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            EncryptAESStreamECB(Src, in_len, K128, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_ECB) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            EncryptAESStreamECB(Src, in_len, K192, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_ECB) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            EncryptAESStreamECB(Src, in_len, K256, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_CBC, CN_CIPHER_AES192_CBC, CN_CIPHER_AES256_CBC:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < SizeOf(IVBuf) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, IVBuf[0], SizeOf(IVBuf));
          if (alg_id = CN_CIPHER_AES128_CBC) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            EncryptAESStreamCBC(Src, in_len, K128, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_CBC) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            EncryptAESStreamCBC(Src, in_len, K192, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_CBC) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            EncryptAESStreamCBC(Src, in_len, K256, IVBuf, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_OFB, CN_CIPHER_AES192_OFB, CN_CIPHER_AES256_OFB:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < SizeOf(IVBuf) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, IVBuf[0], SizeOf(IVBuf));
          if (alg_id = CN_CIPHER_AES128_OFB) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            EncryptAESStreamOFB(Src, in_len, K128, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_OFB) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            EncryptAESStreamOFB(Src, in_len, K192, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_OFB) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            EncryptAESStreamOFB(Src, in_len, K256, IVBuf, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_CFB, CN_CIPHER_AES192_CFB, CN_CIPHER_AES256_CFB:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < SizeOf(IVBuf) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, IVBuf[0], SizeOf(IVBuf));
          if (alg_id = CN_CIPHER_AES128_CFB) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            EncryptAESStreamCFB(Src, in_len, K128, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_CFB) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            EncryptAESStreamCFB(Src, in_len, K192, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_CFB) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            EncryptAESStreamCFB(Src, in_len, K256, IVBuf, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_CTR, CN_CIPHER_AES192_CTR, CN_CIPHER_AES256_CTR:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < (SizeOf(Nonce) + SizeOf(CTRIv)) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, Nonce[0], SizeOf(Nonce));
          IvPos := PByte(iv);
          Inc(IvPos, SizeOf(Nonce));
          Move(IvPos^, CTRIv[0], SizeOf(CTRIv));
          if (alg_id = CN_CIPHER_AES128_CTR) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            EncryptAESStreamCTR(Src, in_len, K128, Nonce, CTRIv, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_CTR) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            EncryptAESStreamCTR(Src, in_len, K192, Nonce, CTRIv, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_CTR) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            EncryptAESStreamCTR(Src, in_len, K256, Nonce, CTRIv, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
    else
      Result := CN_E_UNSUPPORTED;
      Exit;
    end;
    out_len := Dst.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(Dst.Memory^, out_ptr^, out_len);
    Result := CN_OK;
  finally
    Src.Free;
    Dst.Free;
  end;
end;

function cn_cipher_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; iv:
  PByte; iv_len: TCnSize; in_ptr: PByte; in_len: TCnSize; out_ptr: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  Src, Dst: TMemoryStream;
  IVBuf: TCnAESBuffer;
  K128: TCnAESKey128;
  K192: TCnAESKey192;
  K256: TCnAESKey256;
  Nonce: TCnAESCTRNonce;
  CTRIv: TCnAESCTRIv;
  IvPos: PByte;
  DKey: TCnDESKey;
  DIVec: TCnDESIv;
  T3Key: TCn3DESKey;
  SM4K: TCnSM4Key;
  SM4IV: TCnSM4Iv;
  SM4Nonce: TCnSM4Nonce;
  I: Integer;
  KeyStreamLen: Cardinal;
  KeyStream: PCardinal;
  CK: TCnChaChaKey;
  CN: TCnChaChaNonce;
begin
  if (key = nil) or (in_ptr = nil) or (in_len = 0) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Src := TMemoryStream.Create;
  Dst := TMemoryStream.Create;
  try
    Src.Size := in_len;
    Src.Position := 0;
    Move(in_ptr^, Src.Memory^, in_len);
    case alg_id of
      CN_CIPHER_DES_ECB, CN_CIPHER_DES_CBC:
        begin
          if (alg_id = CN_CIPHER_DES_ECB) and (key_len = SizeOf(DKey)) then
          begin
            Move(key^, DKey[0], SizeOf(DKey));
            DESDecryptStreamECB(Src, in_len, DKey, Dst);
          end
          else if (alg_id = CN_CIPHER_DES_CBC) and (key_len = SizeOf(DKey)) then
          begin
            if iv = nil then
            begin
              Result := CN_E_INVALID_ARG;
              Exit;
            end;
            if iv_len < SizeOf(DIVec) then
            begin
              Result := CN_E_BUFFER_TOO_SMALL;
              Exit;
            end;
            Move(key^, DKey[0], SizeOf(DKey));
            Move(iv^, DIVec[0], SizeOf(DIVec));
            DESDecryptStreamCBC(Src, in_len, DKey, DIVec, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_3DES_ECB, CN_CIPHER_3DES_CBC:
        begin
          if (alg_id = CN_CIPHER_3DES_ECB) and (key_len = SizeOf(T3Key)) then
          begin
            Move(key^, T3Key[0], SizeOf(T3Key));
            TripleDESDecryptStreamECB(Src, in_len, T3Key, Dst);
          end
          else if (alg_id = CN_CIPHER_3DES_CBC) and (key_len = SizeOf(T3Key))
            then
          begin
            if iv = nil then
            begin
              Result := CN_E_INVALID_ARG;
              Exit;
            end;
            if iv_len < SizeOf(DIVec) then
            begin
              Result := CN_E_BUFFER_TOO_SMALL;
              Exit;
            end;
            Move(key^, T3Key[0], SizeOf(T3Key));
            Move(iv^, DIVec[0], SizeOf(DIVec));
            TripleDESDecryptStreamCBC(Src, in_len, T3Key, DIVec, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_SM4_ECB, CN_CIPHER_SM4_CBC, CN_CIPHER_SM4_CFB, CN_CIPHER_SM4_OFB,
        CN_CIPHER_SM4_CTR:
        begin
          if key_len <> SizeOf(SM4K) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Move(key^, SM4K[0], SizeOf(SM4K));
          case alg_id of
            CN_CIPHER_SM4_ECB:
              SM4DecryptStreamECB(Src, in_len, SM4K, Dst);
            CN_CIPHER_SM4_CBC:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4IV)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4IV[0], SizeOf(SM4IV));
                SM4DecryptStreamCBC(Src, in_len, SM4K, SM4IV, Dst);
              end;
            CN_CIPHER_SM4_CFB:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4IV)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4IV[0], SizeOf(SM4IV));
                SM4DecryptStreamCFB(Src, in_len, SM4K, SM4IV, Dst);
              end;
            CN_CIPHER_SM4_OFB:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4IV)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4IV[0], SizeOf(SM4IV));
                SM4DecryptStreamOFB(Src, in_len, SM4K, SM4IV, Dst);
              end;
            CN_CIPHER_SM4_CTR:
              begin
                if (iv = nil) or (iv_len < SizeOf(SM4Nonce)) then
                begin
                  Result := CN_E_INVALID_ARG;
                  Exit;
                end;
                Move(iv^, SM4Nonce[0], SizeOf(SM4Nonce));
                SM4DecryptStreamCTR(Src, in_len, SM4K, SM4Nonce, Dst);
              end;
          end;
        end;
      CN_CIPHER_RC4:
        begin
          if (key_len = 0) or (key_len > CN_RC4_MAX_KEY_BYTE_LENGTH) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Dst.Size := in_len;
          RC4Decrypt(key, key_len, Src.Memory, Dst.Memory, in_len);
        end;
      CN_CIPHER_ZUC:
        begin
          if (key_len <> CN_ZUC_KEYSIZE) or (iv = nil) or (iv_len <>
            CN_ZUC_KEYSIZE) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Dst.Size := in_len;
          KeyStreamLen := (in_len + SizeOf(Cardinal) - 1) div SizeOf(Cardinal);
          GetMem(KeyStream, KeyStreamLen * SizeOf(Cardinal));
          try
            ZUC(key, iv, KeyStream, KeyStreamLen);
            for I := 0 to in_len - 1 do
              PByteArray(Dst.Memory)[I] := PByteArray(Src.Memory)[I] xor
                PByteArray(KeyStream)[I];
          finally
            FreeMem(KeyStream);
          end;
        end;
      CN_CIPHER_CHACHA20:
        begin
          if (key_len <> SizeOf(CK)) or (iv = nil) or (iv_len <> SizeOf(CN)) then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          Move(key^, CK[0], SizeOf(CK));
          Move(iv^, CN[0], SizeOf(CN));
          Dst.Size := in_len;
          if not ChaCha20DecryptData(CK, CN, Src.Memory, in_len, Dst.Memory) then
          begin
            Result := CN_E_INTERNAL;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_ECB, CN_CIPHER_AES192_ECB, CN_CIPHER_AES256_ECB:
        begin
          if (alg_id = CN_CIPHER_AES128_ECB) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            DecryptAESStreamECB(Src, in_len, K128, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_ECB) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            DecryptAESStreamECB(Src, in_len, K192, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_ECB) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            DecryptAESStreamECB(Src, in_len, K256, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_CBC, CN_CIPHER_AES192_CBC, CN_CIPHER_AES256_CBC:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < SizeOf(IVBuf) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, IVBuf[0], SizeOf(IVBuf));
          if (alg_id = CN_CIPHER_AES128_CBC) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            DecryptAESStreamCBC(Src, in_len, K128, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_CBC) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            DecryptAESStreamCBC(Src, in_len, K192, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_CBC) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            DecryptAESStreamCBC(Src, in_len, K256, IVBuf, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_OFB, CN_CIPHER_AES192_OFB, CN_CIPHER_AES256_OFB:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < SizeOf(IVBuf) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, IVBuf[0], SizeOf(IVBuf));
          if (alg_id = CN_CIPHER_AES128_OFB) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            DecryptAESStreamOFB(Src, in_len, K128, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_OFB) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            DecryptAESStreamOFB(Src, in_len, K192, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_OFB) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            DecryptAESStreamOFB(Src, in_len, K256, IVBuf, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_CFB, CN_CIPHER_AES192_CFB, CN_CIPHER_AES256_CFB:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < SizeOf(IVBuf) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, IVBuf[0], SizeOf(IVBuf));
          if (alg_id = CN_CIPHER_AES128_CFB) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            DecryptAESStreamCFB(Src, in_len, K128, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_CFB) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            DecryptAESStreamCFB(Src, in_len, K192, IVBuf, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_CFB) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            DecryptAESStreamCFB(Src, in_len, K256, IVBuf, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
      CN_CIPHER_AES128_CTR, CN_CIPHER_AES192_CTR, CN_CIPHER_AES256_CTR:
        begin
          if iv = nil then
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
          if iv_len < (SizeOf(Nonce) + SizeOf(CTRIv)) then
          begin
            Result := CN_E_BUFFER_TOO_SMALL;
            Exit;
          end;
          Move(iv^, Nonce[0], SizeOf(Nonce));
          IvPos := PByte(iv);
          Inc(IvPos, SizeOf(Nonce));
          Move(IvPos^, CTRIv[0], SizeOf(CTRIv));
          if (alg_id = CN_CIPHER_AES128_CTR) and (key_len = SizeOf(K128)) then
          begin
            Move(key^, K128[0], SizeOf(K128));
            DecryptAESStreamCTR(Src, in_len, K128, Nonce, CTRIv, Dst);
          end
          else if (alg_id = CN_CIPHER_AES192_CTR) and (key_len = SizeOf(K192))
            then
          begin
            Move(key^, K192[0], SizeOf(K192));
            DecryptAESStreamCTR(Src, in_len, K192, Nonce, CTRIv, Dst);
          end
          else if (alg_id = CN_CIPHER_AES256_CTR) and (key_len = SizeOf(K256))
            then
          begin
            Move(key^, K256[0], SizeOf(K256));
            DecryptAESStreamCTR(Src, in_len, K256, Nonce, CTRIv, Dst);
          end
          else
          begin
            Result := CN_E_INVALID_ARG;
            Exit;
          end;
        end;
    else
      Result := CN_E_UNSUPPORTED;
      Exit;
    end;
    out_len := Dst.Size;
    if cap < out_len then
    begin
      Result := CN_E_BUFFER_TOO_SMALL;
      Exit;
    end;
    if out_len > 0 then
      Move(Dst.Memory^, out_ptr^, out_len);
    Result := CN_OK;
  finally
    Src.Free;
    Dst.Free;
  end;
end;

function cn_aead_encrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_ptr: PByte; in_len:
  TCnSize; out_cipher: PByte; cap_cipher: TCnSize; var out_cipher_len: TCnSize;
  out_tag: PByte; tag_cap: TCnSize; var out_tag_len: TCnSize): TCnResult; cdecl;
var
  TagG: TCnGCM128Tag;
  TagP: TCnPoly1305Digest;
begin
  if (key = nil) or (nonce = nil) or ((aad = nil) and (aad_len <> 0)) or
    (in_ptr = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if cap_cipher < in_len then
  begin
    out_cipher_len := in_len;
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if tag_cap < CN_AEAD_TAG_BYTES then
  begin
    out_tag_len := CN_AEAD_TAG_BYTES;
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  case alg_id of
    CN_AEAD_AES128_GCM:
      AES128GCMEncrypt(key, key_len, nonce, nonce_len, in_ptr, in_len, aad,
        aad_len, out_cipher, TagG);
    CN_AEAD_AES192_GCM:
      AES192GCMEncrypt(key, key_len, nonce, nonce_len, in_ptr, in_len, aad,
        aad_len, out_cipher, TagG);
    CN_AEAD_AES256_GCM:
      AES256GCMEncrypt(key, key_len, nonce, nonce_len, in_ptr, in_len, aad,
        aad_len, out_cipher, TagG);
    CN_AEAD_SM4_GCM:
      SM4GCMEncrypt(key, key_len, nonce, nonce_len, in_ptr, in_len, aad, aad_len,
        out_cipher, TagG);
    CN_AEAD_CHACHA20_POLY1305:
      ChaCha20Poly1305Encrypt(key, key_len, nonce, nonce_len, in_ptr, in_len,
        aad, aad_len, out_cipher, TagP);
    CN_AEAD_XCHACHA20_POLY1305:
      XChaCha20Poly1305Encrypt(key, key_len, nonce, nonce_len, in_ptr, in_len,
        aad, aad_len, out_cipher, TagP);
  else
    Result := CN_E_UNSUPPORTED;
    Exit;
  end;
  out_cipher_len := in_len;
  out_tag_len := CN_AEAD_TAG_BYTES;
  if (alg_id = CN_AEAD_CHACHA20_POLY1305) or (alg_id = CN_AEAD_XCHACHA20_POLY1305) then
    Move(TagP[0], out_tag^, out_tag_len)
  else
    Move(TagG[0], out_tag^, out_tag_len);
  Result := CN_OK;
end;

function cn_aead_decrypt(alg_id: TInt32; key: PByte; key_len: TCnSize; nonce:
  PByte; nonce_len: TCnSize; aad: PByte; aad_len: TCnSize; in_cipher: PByte;
  in_len: TCnSize; in_tag: PByte; tag_len: TCnSize; out_plain: PByte; cap_plain:
  TCnSize; var out_plain_len: TCnSize): TCnResult; cdecl;
var
  TagG: TCnGCM128Tag;
  TagP: TCnPoly1305Digest;
  Ok: Boolean;
begin
  if (key = nil) or (nonce = nil) or ((aad = nil) and (aad_len <> 0)) or
    (in_cipher = nil) or (in_tag = nil) or (tag_len <> CN_AEAD_TAG_BYTES) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if cap_plain < in_len then
  begin
    out_plain_len := in_len;
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  if (alg_id = CN_AEAD_CHACHA20_POLY1305) or (alg_id = CN_AEAD_XCHACHA20_POLY1305) then
    Move(in_tag^, TagP[0], CN_AEAD_TAG_BYTES)
  else
    Move(in_tag^, TagG[0], CN_AEAD_TAG_BYTES);
  case alg_id of
    CN_AEAD_AES128_GCM:
      Ok := AES128GCMDecrypt(key, key_len, nonce, nonce_len, in_cipher, in_len,
        aad, aad_len, out_plain, TagG);
    CN_AEAD_AES192_GCM:
      Ok := AES192GCMDecrypt(key, key_len, nonce, nonce_len, in_cipher, in_len,
        aad, aad_len, out_plain, TagG);
    CN_AEAD_AES256_GCM:
      Ok := AES256GCMDecrypt(key, key_len, nonce, nonce_len, in_cipher, in_len,
        aad, aad_len, out_plain, TagG);
    CN_AEAD_SM4_GCM:
      Ok := SM4GCMDecrypt(key, key_len, nonce, nonce_len, in_cipher, in_len, aad,
        aad_len, out_plain, TagG);
    CN_AEAD_CHACHA20_POLY1305:
      Ok := ChaCha20Poly1305Decrypt(key, key_len, nonce, nonce_len, in_cipher,
        in_len, aad, aad_len, out_plain, TagP);
    CN_AEAD_XCHACHA20_POLY1305:
      Ok := XChaCha20Poly1305Decrypt(key, key_len, nonce, nonce_len, in_cipher,
        in_len, aad, aad_len, out_plain, TagP);
  else
    Result := CN_E_UNSUPPORTED;
    Exit;
  end;
  if not Ok then
  begin
    Result := CN_E_VERIFY_FAIL;
    Exit;
  end;
  out_plain_len := in_len;
  Result := CN_OK;
end;

function cn_ed25519_privkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnEd25519PrivateKey.Create;
end;

function cn_ed25519_pubkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnEd25519PublicKey.Create;
end;

function cn_ed25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
begin
  if key = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  TObject(key).Free;
  Result := CN_OK;
end;

function cn_ed25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
var
  E: TCnEd25519;
  Priv: TCnEd25519PrivateKey;
  Pub: TCnEd25519PublicKey;
begin
  E := TCnEd25519.Create;
  Priv := TCnEd25519PrivateKey.Create;
  Pub := TCnEd25519PublicKey.Create;
  try
    if not E.GenerateKeys(Priv, Pub) then
    begin
      Priv.Free;
      Pub.Free;
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_priv := Priv;
    out_pub := Pub;
    Result := CN_OK;
  finally
    E.Free;
  end;
end;

function cn_ed25519_sign(priv: TCnCryptoHandle; pub: TCnCryptoHandle; data:
  PByte; len: TCnSize; out_sig: PByte; cap: TCnSize; var out_len: TCnSize):
  TCnResult; cdecl;
var
  Sig: TCnEd25519Signature;
  Ok: Boolean;
  SigData: TCnEd25519SignatureData;
begin
  if (priv = nil) or (pub = nil) or ((data = nil) and (len <> 0)) or (out_sig =
    nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(SigData);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  Sig := TCnEd25519Signature.Create;
  try
    Ok := CnEd25519SignData(data, len, TCnEd25519PrivateKey(priv),
      TCnEd25519PublicKey(pub), Sig);
    if not Ok then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    Sig.SaveToData(SigData);
    Move(SigData[0], out_sig^, out_len);
    Result := CN_OK;
  finally
    Sig.Free;
  end;
end;

function cn_ed25519_verify(pub: TCnCryptoHandle; data: PByte; len: TCnSize; sig:
  PByte; sig_len: TCnSize): TBool32; cdecl;
var
  SigObj: TCnEd25519Signature;
  Ok: Boolean;
  SigData: TCnEd25519SignatureData;
begin
  if (pub = nil) or ((data = nil) and (len <> 0)) or (sig = nil) or (sig_len <>
    SizeOf(SigData)) then
  begin
    Result := 0;
    Exit;
  end;
  Move(sig^, SigData[0], SizeOf(SigData));
  SigObj := TCnEd25519Signature.Create;
  try
    SigObj.LoadFromData(SigData);
    Ok := CnEd25519VerifyData(data, len, SigObj, TCnEd25519PublicKey(pub));
    if Ok then
      Result := 1
    else
      Result := 0;
  finally
    SigObj.Free;
  end;
end;

function cn_ed25519_privkey_from_bytes(data: PByte; len: TCnSize; var out_priv:
  TCnCryptoHandle): TCnResult; cdecl;
var
  D: TCnEd25519Data;
  K: TCnEd25519PrivateKey;
begin
  if (data = nil) or (len <> SizeOf(D)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Move(data^, D[0], SizeOf(D));
  K := TCnEd25519PrivateKey.Create;
  try
    K.LoadFromData(D);
    out_priv := K;
    Result := CN_OK;
  except
    K.Free;
    Result := CN_E_INTERNAL;
  end;
end;

function cn_ed25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  D: TCnEd25519Data;
begin
  if (priv = nil) or (out_buf = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(D);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  TCnEd25519PrivateKey(priv).SaveToData(D);
  Move(D[0], out_buf^, out_len);
  Result := CN_OK;
end;

function cn_ed25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
var
  D: TCnEd25519Data;
  K: TCnEd25519PublicKey;
begin
  if (data = nil) or (len <> SizeOf(D)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Move(data^, D[0], SizeOf(D));
  K := TCnEd25519PublicKey.Create;
  try
    K.LoadFromData(D);
    out_pub := K;
    Result := CN_OK;
  except
    K.Free;
    Result := CN_E_INTERNAL;
  end;
end;

function cn_ed25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  D: TCnEd25519Data;
begin
  if (pub = nil) or (out_buf = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(D);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  TCnEd25519PublicKey(pub).SaveToData(D);
  Move(D[0], out_buf^, out_len);
  Result := CN_OK;
end;

function cn_ed25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
var
  E: TCnEd25519;
  Pub: TCnEd25519PublicKey;
  K: TCnBigNumber;
begin
  if priv = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  E := TCnEd25519.Create;
  Pub := TCnEd25519PublicKey.Create;
  K := TCnBigNumber.Create;
  try
    CnCalcKeysFromEd25519PrivateKey(TCnEd25519PrivateKey(priv), K, nil);
    Pub.Assign(E.Generator);
    E.MultiplePoint(K, Pub);
    out_pub := Pub;
    Result := CN_OK;
  finally
    E.Free;
    K.Free;
  end;
end;

function cn_ed25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  Pub: TCnCryptoHandle;
  D: TCnEd25519Data;
  R: TCnResult;
begin
  if (priv = nil) or (out_buf = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(D);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  R := cn_ed25519_derive_public(priv, Pub);
  if R <> CN_OK then
  begin
    Result := R;
    Exit;
  end;
  try
    TCnEd25519PublicKey(Pub).SaveToData(D);
    Move(D[0], out_buf^, out_len);
    Result := CN_OK;
  finally
    TObject(Pub).Free;
  end;
end;

function cn_curve25519_privkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnCurve25519PrivateKey.Create;
end;

function cn_curve25519_pubkey_new: TCnCryptoHandle; cdecl;
begin
  Result := TCnCurve25519PublicKey.Create;
end;

function cn_curve25519_key_free(key: TCnCryptoHandle): TCnResult; cdecl;
begin
  if key = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  TObject(key).Free;
  Result := CN_OK;
end;

function cn_curve25519_generate_keys(var out_priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
var
  E: TCnCurve25519;
  Priv: TCnCurve25519PrivateKey;
  Pub: TCnCurve25519PublicKey;
begin
  E := TCnCurve25519.Create;
  Priv := TCnCurve25519PrivateKey.Create;
  Pub := TCnCurve25519PublicKey.Create;
  try
    if not E.GenerateKeys(Priv, Pub) then
    begin
      Priv.Free;
      Pub.Free;
      Result := CN_E_INTERNAL;
      Exit;
    end;
    out_priv := Priv;
    out_pub := Pub;
    Result := CN_OK;
  finally
    E.Free;
  end;
end;

function cn_curve25519_dh_step1(self_priv: TCnCryptoHandle; out_point_bytes:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  P: TCnEccPoint;
  Data: TCnCurve25519Data;
  Ok: Boolean;
begin
  if (self_priv = nil) or (out_point_bytes = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(Data);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  P := TCnEccPoint.Create;
  try
    Ok := CnCurve25519KeyExchangeStep1(TCnEccPrivateKey(self_priv), P);
    if not Ok then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    CnCurve25519PointToData(P, Data);
    Move(Data[0], out_point_bytes^, out_len);
    Result := CN_OK;
  finally
    P.Free;
  end;
end;

function cn_curve25519_dh_step2(self_priv: TCnCryptoHandle; peer_point_bytes:
  PByte; peer_len: TCnSize; out_shared_bytes: PByte; cap: TCnSize; var out_len:
  TCnSize): TCnResult; cdecl;
var
  Peer: TCnEccPoint;
  Shared: TCnEccPoint;
  DataIn: TCnCurve25519Data;
  DataOut: TCnCurve25519Data;
  Ok: Boolean;
begin
  if (self_priv = nil) or (peer_point_bytes = nil) or (out_shared_bytes = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  if peer_len <> SizeOf(DataIn) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(DataOut);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  Move(peer_point_bytes^, DataIn[0], SizeOf(DataIn));
  Peer := TCnEccPoint.Create;
  Shared := TCnEccPoint.Create;
  try
    CnCurve25519DataToPoint(DataIn, Peer);
    Ok := CnCurve25519KeyExchangeStep2(TCnEccPrivateKey(self_priv), Peer, Shared);
    if not Ok then
    begin
      Result := CN_E_INTERNAL;
      Exit;
    end;
    CnCurve25519PointToData(Shared, DataOut);
    Move(DataOut[0], out_shared_bytes^, out_len);
    Result := CN_OK;
  finally
    Peer.Free;
    Shared.Free;
  end;
end;

function cn_curve25519_privkey_from_bytes(data: PByte; len: TCnSize; var
  out_priv: TCnCryptoHandle): TCnResult; cdecl;
var
  D: TCnCurve25519Data;
  K: TCnCurve25519PrivateKey;
begin
  if (data = nil) or (len <> SizeOf(D)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Move(data^, D[0], SizeOf(D));
  K := TCnCurve25519PrivateKey.Create;
  try
    K.LoadFromData(D);
    out_priv := K;
    Result := CN_OK;
  except
    K.Free;
    Result := CN_E_INTERNAL;
  end;
end;

function cn_curve25519_privkey_to_bytes(priv: TCnCryptoHandle; out_buf: PByte;
  cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  D: TCnCurve25519Data;
begin
  if (priv = nil) or (out_buf = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(D);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  TCnCurve25519PrivateKey(priv).SaveToData(D);
  Move(D[0], out_buf^, out_len);
  Result := CN_OK;
end;

function cn_curve25519_pubkey_from_bytes(data: PByte; len: TCnSize; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
var
  D: TCnCurve25519Data;
  K: TCnCurve25519PublicKey;
begin
  if (data = nil) or (len <> SizeOf(D)) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  Move(data^, D[0], SizeOf(D));
  K := TCnCurve25519PublicKey.Create;
  try
    K.LoadFromData(D);
    out_pub := K;
    Result := CN_OK;
  except
    K.Free;
    Result := CN_E_INTERNAL;
  end;
end;

function cn_curve25519_pubkey_to_bytes(pub: TCnCryptoHandle; out_buf: PByte; cap:
  TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  D: TCnCurve25519Data;
begin
  if (pub = nil) or (out_buf = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(D);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  TCnCurve25519PublicKey(pub).SaveToData(D);
  Move(D[0], out_buf^, out_len);
  Result := CN_OK;
end;

function cn_curve25519_derive_public(priv: TCnCryptoHandle; var out_pub:
  TCnCryptoHandle): TCnResult; cdecl;
var
  E: TCnCurve25519;
  Pub: TCnCurve25519PublicKey;
begin
  if priv = nil then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  E := TCnCurve25519.Create;
  Pub := TCnCurve25519PublicKey.Create;
  try
    Pub.Assign(E.Generator);
    E.MultiplePoint(TCnCurve25519PrivateKey(priv), Pub);
    out_pub := Pub;
    Result := CN_OK;
  finally
    E.Free;
  end;
end;

function cn_curve25519_derive_public_to_bytes(priv: TCnCryptoHandle; out_buf:
  PByte; cap: TCnSize; var out_len: TCnSize): TCnResult; cdecl;
var
  Pub: TCnCryptoHandle;
  D: TCnCurve25519Data;
  R: TCnResult;
begin
  if (priv = nil) or (out_buf = nil) then
  begin
    Result := CN_E_INVALID_ARG;
    Exit;
  end;
  out_len := SizeOf(D);
  if cap < out_len then
  begin
    Result := CN_E_BUFFER_TOO_SMALL;
    Exit;
  end;
  R := cn_curve25519_derive_public(priv, Pub);
  if R <> CN_OK then
  begin
    Result := R;
    Exit;
  end;
  try
    TCnCurve25519PublicKey(Pub).SaveToData(D);
    Move(D[0], out_buf^, out_len);
    Result := CN_OK;
  finally
    TObject(Pub).Free;
  end;
end;

end.

